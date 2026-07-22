package codec

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"strings"
)

// ValidateEnvelope checks the stable envelope independently of the active Go
// schema. ValidateForToolchain, in decode.go, adds generated-schema checks.
func ValidateEnvelope(unit *Unit) error {
	if unit == nil {
		return fmt.Errorf("nil go-ast unit")
	}
	if unit.Format != WireFormat {
		return fmt.Errorf("unsupported wire format %q", unit.Format)
	}
	if unit.FormatVersion != WireFormatVersion {
		return fmt.Errorf("unsupported wire format version %d", unit.FormatVersion)
	}
	if unit.GoSchema.GoVersion == "" || unit.GoSchema.SchemaHash == "" ||
		unit.GoSchema.ASTPackageHash == "" || unit.GoSchema.TokenPackageHash == "" {
		return fmt.Errorf("unit has incomplete Go schema identity")
	}
	if unit.Root == "" {
		return fmt.Errorf("unit has no root node ID")
	}

	nodes := make(map[NodeID]WireNode, len(unit.Nodes))
	for _, node := range unit.Nodes {
		if node.ID == "" {
			return fmt.Errorf("node has an empty ID")
		}
		if _, duplicate := nodes[node.ID]; duplicate {
			return fmt.Errorf("duplicate node ID %q", node.ID)
		}
		if !validNamespace(node.Namespace) || node.Kind == "" {
			return fmt.Errorf("node %q has an invalid namespace or empty kind", node.ID)
		}
		if node.Fields == nil {
			return fmt.Errorf("node %q has no fields object", node.ID)
		}
		nodes[node.ID] = node
	}
	if _, ok := nodes[unit.Root]; !ok {
		return fmt.Errorf("root node %q is absent from node table", unit.Root)
	}

	sources := make(map[string]Source, len(unit.Sources))
	for _, source := range unit.Sources {
		if source.ID == "" {
			return fmt.Errorf("source has an empty ID")
		}
		if source.Size < 0 {
			return fmt.Errorf("source %q has negative size", source.ID)
		}
		previous := -1
		for index, offset := range source.LineOffsets {
			if offset < 0 || offset >= source.Size || offset <= previous {
				return fmt.Errorf("source %q has invalid line offset %d at index %d", source.ID, offset, index)
			}
			previous = offset
		}
		if _, duplicate := sources[source.ID]; duplicate {
			return fmt.Errorf("duplicate source ID %q", source.ID)
		}
		sources[source.ID] = source
	}

	origins := make(map[OriginID]Origin, len(unit.Origins))
	for _, origin := range unit.Origins {
		if origin.ID == "" || origin.Kind == "" {
			return fmt.Errorf("origin has an empty ID or kind")
		}
		if _, duplicate := origins[origin.ID]; duplicate {
			return fmt.Errorf("duplicate origin ID %q", origin.ID)
		}
		origins[origin.ID] = origin
	}

	for _, node := range unit.Nodes {
		if node.Origin != "" {
			if _, ok := origins[node.Origin]; !ok {
				return fmt.Errorf("node %q refers to unknown origin %q", node.ID, node.Origin)
			}
		}
		for name, raw := range node.Fields {
			if name == "" {
				return fmt.Errorf("node %q contains an empty field name", node.ID)
			}
			if err := validateWireValue(raw, nodes, sources); err != nil {
				return fmt.Errorf("node %q field %s: %w", node.ID, name, err)
			}
		}
	}

	for _, origin := range unit.Origins {
		if origin.Primary != "" {
			if _, ok := origins[origin.Primary]; !ok {
				return fmt.Errorf("origin %q has unknown primary origin %q", origin.ID, origin.Primary)
			}
		}
		for _, input := range origin.Inputs {
			if _, ok := origins[input]; !ok {
				return fmt.Errorf("origin %q has unknown input origin %q", origin.ID, input)
			}
		}
		for _, span := range origin.SourceSpans {
			if err := validateSpan(span, sources); err != nil {
				return fmt.Errorf("origin %q: %w", origin.ID, err)
			}
		}
	}
	if err := validateOriginAcyclic(origins); err != nil {
		return err
	}

	annotations := make(map[string]map[NodeID]struct{})
	for _, annotation := range unit.Annotations {
		if !validNamespace(annotation.Namespace) {
			return fmt.Errorf("invalid annotation namespace %q", annotation.Namespace)
		}
		if _, ok := nodes[annotation.Node]; !ok {
			return fmt.Errorf("annotation refers to unknown node %q", annotation.Node)
		}
		if len(annotation.Value) == 0 || !json.Valid(annotation.Value) {
			return fmt.Errorf("annotation %q has invalid JSON value", annotation.Namespace)
		}
		byNode := annotations[annotation.Namespace]
		if byNode == nil {
			byNode = make(map[NodeID]struct{})
			annotations[annotation.Namespace] = byNode
		}
		if _, duplicate := byNode[annotation.Node]; duplicate {
			return fmt.Errorf("duplicate annotation namespace %q for node %q", annotation.Namespace, annotation.Node)
		}
		byNode[annotation.Node] = struct{}{}
	}

	for index, extension := range unit.Extensions {
		if !validNamespace(extension.Namespace) || extension.Namespace == GoASTNamespace {
			return fmt.Errorf("extension %d has invalid namespace %q", index, extension.Namespace)
		}
		if extension.Version < 1 {
			return fmt.Errorf("extension %q has invalid version %d", extension.Namespace, extension.Version)
		}
		if len(extension.Value) == 0 || !json.Valid(extension.Value) {
			return fmt.Errorf("extension %q has invalid JSON value", extension.Namespace)
		}
	}
	for _, origin := range unit.Origins {
		if origin.Metadata != nil {
			if _, err := json.Marshal(origin.Metadata); err != nil {
				return fmt.Errorf("origin %q has invalid metadata: %w", origin.ID, err)
			}
		}
	}
	return nil
}

func validateWireValue(raw json.RawMessage, nodes map[NodeID]WireNode, sources map[string]Source) error {
	if len(raw) == 0 || !json.Valid(raw) {
		return fmt.Errorf("invalid JSON value")
	}
	if isNull(raw) {
		return nil
	}
	trimmed := bytes.TrimSpace(raw)
	if len(trimmed) == 0 {
		return fmt.Errorf("empty JSON value")
	}
	if trimmed[0] == '[' {
		var values []json.RawMessage
		if err := json.Unmarshal(trimmed, &values); err != nil {
			return err
		}
		for index, value := range values {
			if err := validateWireValue(value, nodes, sources); err != nil {
				return fmt.Errorf("element %d: %w", index, err)
			}
		}
		return nil
	}
	if trimmed[0] != '{' {
		return nil
	}

	object, err := decodeTaggedObject(trimmed)
	if err != nil {
		return err
	}
	if rawRef, ok := object["$ref"]; ok && len(object) == 1 && isJSONString(rawRef) {
		var ref NodeID
		if err := json.Unmarshal(rawRef, &ref); err != nil {
			return fmt.Errorf("invalid node reference: %w", err)
		}
		if _, exists := nodes[ref]; !exists {
			return fmt.Errorf("reference to unknown node %q", ref)
		}
		return nil
	}
	if rawPosition, ok := object["$position"]; ok && len(object) == 1 && isJSONObject(rawPosition) {
		var position Position
		if err := decodeStrictJSON(rawPosition, &position); err != nil {
			return fmt.Errorf("invalid position: %w", err)
		}
		source, exists := sources[position.Source]
		if !exists {
			return fmt.Errorf("position refers to unknown source %q", position.Source)
		}
		if position.ByteOffset < 0 || position.ByteOffset > source.Size {
			return fmt.Errorf("position offset %d is outside source %q (size %d)", position.ByteOffset, source.ID, source.Size)
		}
		return nil
	}
	if rawToken, ok := object["$token"]; ok && len(object) == 1 && isJSONString(rawToken) {
		var name string
		if err := json.Unmarshal(rawToken, &name); err != nil || name == "" {
			return fmt.Errorf("invalid symbolic token")
		}
		return nil
	}
	if rawEnum, ok := object["$enum"]; ok && len(object) == 1 && isJSONObject(rawEnum) {
		var enum EnumPayload
		if err := decodeStrictJSON(rawEnum, &enum); err != nil {
			return fmt.Errorf("invalid enum: %w", err)
		}
		if enum.Type == "" {
			return fmt.Errorf("enum has empty type")
		}
		for _, name := range enum.Names {
			if name == "" {
				return fmt.Errorf("enum contains an empty symbolic name")
			}
		}
		return nil
	}

	for name, value := range object {
		if err := validateWireValue(value, nodes, sources); err != nil {
			return fmt.Errorf("object field %s: %w", name, err)
		}
	}
	return nil
}

func decodeStrictJSON(raw json.RawMessage, target any) error {
	decoder := json.NewDecoder(bytes.NewReader(raw))
	decoder.DisallowUnknownFields()
	if err := decoder.Decode(target); err != nil {
		return err
	}
	var trailing any
	if err := decoder.Decode(&trailing); err != io.EOF {
		if err == nil {
			return fmt.Errorf("trailing JSON value")
		}
		return err
	}
	return nil
}

func isJSONString(raw json.RawMessage) bool {
	trimmed := bytes.TrimSpace(raw)
	return len(trimmed) >= 2 && trimmed[0] == '"'
}

func isJSONObject(raw json.RawMessage) bool {
	trimmed := bytes.TrimSpace(raw)
	return len(trimmed) >= 2 && trimmed[0] == '{'
}

func validateSpan(span Span, sources map[string]Source) error {
	source, ok := sources[span.Source]
	if !ok {
		return fmt.Errorf("span refers to unknown source %q", span.Source)
	}
	if span.StartByte < 0 || span.EndByte < span.StartByte || span.EndByte > source.Size {
		return fmt.Errorf("invalid span [%d,%d) for source %q (size %d)", span.StartByte, span.EndByte, source.ID, source.Size)
	}
	return nil
}

func validateOriginAcyclic(origins map[OriginID]Origin) error {
	state := make(map[OriginID]uint8, len(origins))
	var visit func(OriginID) error
	visit = func(id OriginID) error {
		switch state[id] {
		case 1:
			return fmt.Errorf("origin graph contains a cycle through %q", id)
		case 2:
			return nil
		}
		state[id] = 1
		origin := origins[id]
		if origin.Primary != "" {
			if err := visit(origin.Primary); err != nil {
				return err
			}
		}
		for _, input := range origin.Inputs {
			if err := visit(input); err != nil {
				return err
			}
		}
		state[id] = 2
		return nil
	}
	for id := range origins {
		if err := visit(id); err != nil {
			return err
		}
	}
	return nil
}

func validNamespace(namespace string) bool {
	if namespace == "" {
		return false
	}
	parts := strings.FieldsFunc(namespace, func(r rune) bool { return r == '.' || r == '/' })
	if len(parts) == 0 || strings.HasPrefix(namespace, ".") || strings.HasPrefix(namespace, "/") ||
		strings.HasSuffix(namespace, ".") || strings.HasSuffix(namespace, "/") ||
		strings.Contains(namespace, "..") || strings.Contains(namespace, "//") ||
		strings.Contains(namespace, "./") || strings.Contains(namespace, "/.") {
		return false
	}
	for partIndex, part := range parts {
		if part == "" {
			return false
		}
		for index, r := range part {
			if partIndex == 0 && index == 0 && !((r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z')) {
				return false
			}
			if !(r == '-' || r == '_' || r >= '0' && r <= '9' || r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z') {
				return false
			}
		}
	}
	return true
}
