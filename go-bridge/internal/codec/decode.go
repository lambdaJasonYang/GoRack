package codec

import (
	"bytes"
	"encoding/json"
	"fmt"
	"go/ast"
	"go/token"
	"sort"
	"strconv"

	"github.com/gorack/gorack/go-bridge/internal/generated"
)

type DecodeOptions struct {
	AllowSchemaMismatch bool
}

type Decoded struct {
	Root    ast.Node
	FileSet *token.FileSet
	Nodes   map[NodeID]ast.Node
	Unit    *Unit
}

func ValidateForToolchain(unit *Unit, options DecodeOptions) error {
	if err := ValidateEnvelope(unit); err != nil {
		return err
	}
	if !options.AllowSchemaMismatch && unit.GoSchema.SchemaHash != generated.SchemaHash {
		return fmt.Errorf("Go AST schema mismatch: wire=%s active=%s (%s)",
			unit.GoSchema.SchemaHash, generated.SchemaHash, generated.GoVersion)
	}
	for _, node := range unit.Nodes {
		if node.Namespace != GoASTNamespace {
			return fmt.Errorf("Go decoder cannot emit extension node %s.%s (%s)", node.Namespace, node.Kind, node.ID)
		}
		spec, ok := generated.NodeSpecs[node.Kind]
		if !ok {
			return fmt.Errorf("active Go schema does not contain node kind %q", node.Kind)
		}
		included := make(map[string]generated.FieldSpec, len(spec.Fields))
		for _, field := range spec.Fields {
			included[field.Name] = field
			raw, present := node.Fields[field.Name]
			if !present && !field.Optional {
				return fmt.Errorf("node %s (%s) lacks required field %s", node.ID, node.Kind, field.Name)
			}
			nullable := field.Optional || field.Kind == "position" ||
				field.Kind == "node-list" || field.Kind == "node-map"
			if present && isNull(raw) && !nullable {
				return fmt.Errorf("node %s (%s) has null required field %s", node.ID, node.Kind, field.Name)
			}
		}
		for field := range node.Fields {
			if _, ok := included[field]; !ok {
				return fmt.Errorf("active Go schema does not contain field %s.%s", node.Kind, field)
			}
		}
	}
	return validateASTAcyclic(unit)
}

// Go syntax is a DAG in practice (comments and import specs may be shared),
// but it must not contain a child-edge cycle. Such a graph is representable by
// the wire table yet unsafe to hand to go/format and many ast visitors.
func validateASTAcyclic(unit *Unit) error {
	edges := make(map[NodeID][]NodeID, len(unit.Nodes))
	for _, node := range unit.Nodes {
		spec := generated.NodeSpecs[node.Kind]
		for _, field := range spec.Fields {
			raw, present := node.Fields[field.Name]
			if !present || isNull(raw) {
				continue
			}
			refs, err := childReferenceIDs(raw, field.Kind)
			if err != nil {
				return fmt.Errorf("node %s (%s) field %s: %w", node.ID, node.Kind, field.Name, err)
			}
			edges[node.ID] = append(edges[node.ID], refs...)
		}
	}

	state := make(map[NodeID]uint8, len(unit.Nodes))
	var visit func(NodeID) error
	visit = func(id NodeID) error {
		switch state[id] {
		case 1:
			return fmt.Errorf("Go AST child graph contains a cycle through %q", id)
		case 2:
			return nil
		}
		state[id] = 1
		for _, child := range edges[id] {
			if err := visit(child); err != nil {
				return err
			}
		}
		state[id] = 2
		return nil
	}
	for _, node := range unit.Nodes {
		if err := visit(node.ID); err != nil {
			return err
		}
	}
	return nil
}

func childReferenceIDs(raw json.RawMessage, kind string) ([]NodeID, error) {
	switch kind {
	case "node":
		id, err := decodeRefID(raw)
		if err != nil {
			return nil, err
		}
		return []NodeID{id}, nil
	case "node-list":
		var values []json.RawMessage
		if err := json.Unmarshal(raw, &values); err != nil {
			return nil, fmt.Errorf("expected node-reference array: %w", err)
		}
		result := make([]NodeID, len(values))
		for index, value := range values {
			id, err := decodeRefID(value)
			if err != nil {
				return nil, fmt.Errorf("element %d: %w", index, err)
			}
			result[index] = id
		}
		return result, nil
	case "node-map":
		var values map[string]json.RawMessage
		if err := json.Unmarshal(raw, &values); err != nil {
			return nil, fmt.Errorf("expected node-reference object: %w", err)
		}
		keys := make([]string, 0, len(values))
		for key := range values {
			keys = append(keys, key)
		}
		sort.Strings(keys)
		result := make([]NodeID, 0, len(values))
		for _, key := range keys {
			id, err := decodeRefID(values[key])
			if err != nil {
				return nil, fmt.Errorf("key %q: %w", key, err)
			}
			result = append(result, id)
		}
		return result, nil
	default:
		return nil, nil
	}
}

func Decode(unit *Unit, options DecodeOptions) (*Decoded, error) {
	if err := ValidateForToolchain(unit, options); err != nil {
		return nil, err
	}

	fset := token.NewFileSet()
	sourceFiles := make(map[string]*token.File, len(unit.Sources))
	for _, source := range unit.Sources {
		if _, exists := sourceFiles[source.ID]; exists {
			return nil, fmt.Errorf("duplicate source ID %q", source.ID)
		}
		// token.FileSet.AddFile panics if base+size+1 overflows int. Wire
		// validation is an error boundary, so reject hostile sizes explicitly.
		maxInt := int(^uint(0) >> 1)
		if source.Size > maxInt-fset.Base()-1 {
			return nil, fmt.Errorf("source %q is too large for token.FileSet", source.ID)
		}
		file := fset.AddFile(source.Name, -1, source.Size)
		if source.LineOffsets != nil {
			lines := append([]int(nil), source.LineOffsets...)
			if !file.SetLines(lines) {
				return nil, fmt.Errorf("source %q has an invalid line-offset table", source.ID)
			}
		}
		sourceFiles[source.ID] = file
	}

	nodes := make(map[NodeID]ast.Node, len(unit.Nodes))
	for _, wireNode := range unit.Nodes {
		node, err := generated.NewNode(wireNode.Kind)
		if err != nil {
			return nil, fmt.Errorf("allocate node %s: %w", wireNode.ID, err)
		}
		nodes[wireNode.ID] = node
	}

	for _, wireNode := range unit.Nodes {
		node := nodes[wireNode.ID]
		spec := generated.NodeSpecs[wireNode.Kind]
		for _, field := range spec.Fields {
			raw, present := wireNode.Fields[field.Name]
			if !present {
				continue
			}
			value, err := decodeField(raw, field, nodes, sourceFiles)
			if err != nil {
				return nil, fmt.Errorf("decode node %s go/ast.%s.%s: %w", wireNode.ID, wireNode.Kind, field.Name, err)
			}
			if err := generated.SetNodeField(node, field.Name, value); err != nil {
				return nil, fmt.Errorf("set node %s go/ast.%s.%s: %w", wireNode.ID, wireNode.Kind, field.Name, err)
			}
		}
	}

	root := nodes[unit.Root]
	if nilASTNode(root) {
		return nil, fmt.Errorf("decoded root %q is nil", unit.Root)
	}
	return &Decoded{Root: root, FileSet: fset, Nodes: nodes, Unit: unit}, nil
}

func decodeField(raw json.RawMessage, spec generated.FieldSpec, nodes map[NodeID]ast.Node, sources map[string]*token.File) (any, error) {
	switch spec.Kind {
	case "node":
		if isNull(raw) {
			return nil, nil
		}
		return decodeRef(raw, nodes)

	case "node-list":
		if isNull(raw) {
			return []ast.Node(nil), nil
		}
		var values []json.RawMessage
		if err := json.Unmarshal(raw, &values); err != nil {
			return nil, fmt.Errorf("expected node-reference array: %w", err)
		}
		result := make([]ast.Node, len(values))
		for index, value := range values {
			child, err := decodeRef(value, nodes)
			if err != nil {
				return nil, fmt.Errorf("element %d: %w", index, err)
			}
			result[index] = child
		}
		return result, nil

	case "node-map":
		if isNull(raw) {
			return map[string]ast.Node(nil), nil
		}
		var values map[string]json.RawMessage
		if err := json.Unmarshal(raw, &values); err != nil {
			return nil, fmt.Errorf("expected node-reference object: %w", err)
		}
		result := make(map[string]ast.Node, len(values))
		for key, value := range values {
			child, err := decodeRef(value, nodes)
			if err != nil {
				return nil, fmt.Errorf("key %q: %w", key, err)
			}
			result[key] = child
		}
		return result, nil

	case "position":
		if isNull(raw) {
			return token.NoPos, nil
		}
		object, err := decodeTaggedObject(raw)
		if err != nil {
			return nil, err
		}
		positionRaw, ok := object["$position"]
		if !ok || len(object) != 1 {
			return nil, fmt.Errorf("expected $position object")
		}
		var position Position
		if err := decodeStrictJSON(positionRaw, &position); err != nil {
			return nil, err
		}
		file := sources[position.Source]
		if file == nil {
			return nil, fmt.Errorf("unknown position source %q", position.Source)
		}
		if position.ByteOffset < 0 || position.ByteOffset > file.Size() {
			return nil, fmt.Errorf("offset %d outside source %q", position.ByteOffset, position.Source)
		}
		return file.Pos(position.ByteOffset), nil

	case "token":
		object, err := decodeTaggedObject(raw)
		if err != nil {
			return nil, err
		}
		nameRaw, ok := object["$token"]
		if !ok || len(object) != 1 {
			return nil, fmt.Errorf("expected $token object")
		}
		var name string
		if err := json.Unmarshal(nameRaw, &name); err != nil {
			return nil, err
		}
		return generated.ParseToken(name)

	case "enum":
		object, err := decodeTaggedObject(raw)
		if err != nil {
			return nil, err
		}
		enumRaw, ok := object["$enum"]
		if !ok || len(object) != 1 {
			return nil, fmt.Errorf("expected $enum object")
		}
		var enum EnumPayload
		if err := decodeStrictJSON(enumRaw, &enum); err != nil {
			return nil, err
		}
		if enum.Type != spec.EnumType {
			return nil, fmt.Errorf("enum type %q does not match field type %q", enum.Type, spec.EnumType)
		}
		return generated.ParseEnum(enum.Type, enum.Names)

	case "string":
		var value string
		if err := json.Unmarshal(raw, &value); err != nil {
			return nil, err
		}
		return value, nil

	case "bool":
		var value bool
		if err := json.Unmarshal(raw, &value); err != nil {
			return nil, err
		}
		return value, nil

	case "int":
		decoder := json.NewDecoder(bytes.NewReader(raw))
		decoder.UseNumber()
		var value json.Number
		if err := decoder.Decode(&value); err != nil {
			return nil, err
		}
		parsed, err := strconv.ParseInt(value.String(), 10, strconv.IntSize)
		if err != nil {
			return nil, err
		}
		return int(parsed), nil

	default:
		return nil, fmt.Errorf("unsupported generated field kind %q", spec.Kind)
	}
}

func decodeRef(raw json.RawMessage, nodes map[NodeID]ast.Node) (ast.Node, error) {
	id, err := decodeRefID(raw)
	if err != nil {
		return nil, err
	}
	node, ok := nodes[id]
	if !ok {
		return nil, fmt.Errorf("unknown node reference %q", id)
	}
	return node, nil
}

func decodeRefID(raw json.RawMessage) (NodeID, error) {
	object, err := decodeTaggedObject(raw)
	if err != nil {
		return "", err
	}
	refRaw, ok := object["$ref"]
	if !ok || len(object) != 1 {
		return "", fmt.Errorf("expected $ref object")
	}
	var id NodeID
	if err := json.Unmarshal(refRaw, &id); err != nil {
		return "", err
	}
	return id, nil
}
