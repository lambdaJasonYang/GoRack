package codec

import (
	"encoding/json"
	"fmt"
	"go/ast"
	"go/token"
	"reflect"
	"sort"

	"github.com/gorack/gorack/go-bridge/internal/generated"
)

type EncodeOptions struct {
	// Origins joins existing stable provenance IDs to Go nodes. The origin
	// records themselves live in the stable Unit envelope.
	Origins map[ast.Node]OriginID

	OriginRecords []Origin
	Annotations   []Annotation
}

type encoder struct {
	fset *token.FileSet

	ids       map[ast.Node]NodeID
	nodes     []WireNode
	sourceIDs map[*token.File]string
	sources   []Source
	options   EncodeOptions
}

func ActiveSchemaIdentity() SchemaIdentity {
	return SchemaIdentity{
		GoVersion:        generated.GoVersion,
		SchemaHash:       generated.SchemaHash,
		ASTPackageHash:   generated.AstPackageHash,
		TokenPackageHash: generated.TokenPackageHash,
	}
}

func Encode(root ast.Node, fset *token.FileSet, options EncodeOptions) (*Unit, error) {
	if nilASTNode(root) {
		return nil, fmt.Errorf("cannot encode a nil AST root")
	}
	state := &encoder{
		fset:      fset,
		ids:       make(map[ast.Node]NodeID),
		sourceIDs: make(map[*token.File]string),
		options:   options,
	}
	rootID, err := state.encodeNode(root)
	if err != nil {
		return nil, err
	}
	unit := NewUnit(ActiveSchemaIdentity())
	unit.Root = rootID
	unit.Nodes = state.nodes
	unit.Sources = state.sources
	unit.Origins = append([]Origin(nil), options.OriginRecords...)
	unit.Annotations = append([]Annotation(nil), options.Annotations...)
	if err := ValidateForToolchain(unit, DecodeOptions{}); err != nil {
		return nil, fmt.Errorf("validate encoded AST: %w", err)
	}
	return unit, nil
}

func (state *encoder) encodeNode(node ast.Node) (NodeID, error) {
	if nilASTNode(node) {
		return "", nil
	}
	if id, found := state.ids[node]; found {
		return id, nil
	}
	kind, ok := generated.NodeKind(node)
	if !ok {
		return "", fmt.Errorf("active generated schema does not contain AST node type %T", node)
	}
	spec, ok := generated.NodeSpecs[kind]
	if !ok {
		return "", fmt.Errorf("generated node registry has no field schema for %s", kind)
	}

	id := NodeID(fmt.Sprintf("n%d", len(state.nodes)+1))
	state.ids[node] = id
	index := len(state.nodes)
	state.nodes = append(state.nodes, WireNode{
		ID:        id,
		Namespace: GoASTNamespace,
		Kind:      kind,
		Fields:    make(map[string]json.RawMessage, len(spec.Fields)),
	})
	if state.options.Origins != nil {
		state.nodes[index].Origin = state.options.Origins[node]
	}

	for _, field := range spec.Fields {
		value, err := generated.GetNodeField(node, field.Name)
		if err != nil {
			return "", fmt.Errorf("read go/ast.%s.%s: %w", kind, field.Name, err)
		}
		raw, err := state.encodeField(field, value)
		if err != nil {
			return "", fmt.Errorf("encode go/ast.%s.%s: %w", kind, field.Name, err)
		}
		state.nodes[index].Fields[field.Name] = raw
	}
	return id, nil
}

func (state *encoder) encodeField(spec generated.FieldSpec, value any) (json.RawMessage, error) {
	switch spec.Kind {
	case "node":
		child, _ := value.(ast.Node)
		if nilASTNode(child) {
			return json.RawMessage("null"), nil
		}
		id, err := state.encodeNode(child)
		if err != nil {
			return nil, err
		}
		return rawJSON(RefValue{Ref: id})

	case "node-list":
		children, ok := value.([]ast.Node)
		if !ok {
			return nil, fmt.Errorf("generated getter returned %T, want []ast.Node", value)
		}
		if children == nil {
			return json.RawMessage("null"), nil
		}
		refs := make([]RefValue, len(children))
		for index, child := range children {
			if nilASTNode(child) {
				return nil, fmt.Errorf("node list contains nil child at index %d", index)
			}
			id, err := state.encodeNode(child)
			if err != nil {
				return nil, err
			}
			refs[index] = RefValue{Ref: id}
		}
		return rawJSON(refs)

	case "node-map":
		children, ok := value.(map[string]ast.Node)
		if !ok {
			return nil, fmt.Errorf("generated getter returned %T, want map[string]ast.Node", value)
		}
		if children == nil {
			return json.RawMessage("null"), nil
		}
		keys := make([]string, 0, len(children))
		for key := range children {
			keys = append(keys, key)
		}
		sort.Strings(keys)
		refs := make(map[string]RefValue, len(children))
		for _, key := range keys {
			child := children[key]
			if nilASTNode(child) {
				return nil, fmt.Errorf("node map contains nil child at key %q", key)
			}
			id, err := state.encodeNode(child)
			if err != nil {
				return nil, err
			}
			refs[key] = RefValue{Ref: id}
		}
		return rawJSON(refs)

	case "position":
		position, ok := value.(token.Pos)
		if !ok {
			return nil, fmt.Errorf("generated getter returned %T, want token.Pos", value)
		}
		if !position.IsValid() {
			return json.RawMessage("null"), nil
		}
		wirePosition, err := state.encodePosition(position)
		if err != nil {
			return nil, err
		}
		return rawJSON(PositionValue{Position: wirePosition})

	case "token":
		value, ok := value.(token.Token)
		if !ok {
			return nil, fmt.Errorf("generated getter returned %T, want token.Token", value)
		}
		name, err := generated.TokenName(value)
		if err != nil {
			return nil, err
		}
		return rawJSON(TokenValue{Token: name})

	case "enum":
		names, err := generated.EnumNames(spec.EnumType, value)
		if err != nil {
			return nil, err
		}
		return rawJSON(EnumValue{Enum: EnumPayload{Type: spec.EnumType, Names: names}})

	case "string", "bool", "int":
		return rawJSON(value)

	default:
		return nil, fmt.Errorf("unsupported generated field kind %q", spec.Kind)
	}
}

func (state *encoder) encodePosition(position token.Pos) (Position, error) {
	if state.fset == nil {
		return Position{}, fmt.Errorf("a token.FileSet is required to encode valid token.Pos values")
	}
	file := state.fset.File(position)
	if file == nil {
		return Position{}, fmt.Errorf("position %d does not belong to the supplied token.FileSet", position)
	}
	sourceID, found := state.sourceIDs[file]
	if !found {
		sourceID = fmt.Sprintf("source-%d", len(state.sources)+1)
		state.sourceIDs[file] = sourceID
		var lineOffsets []int
		if file.Size() != 0 {
			lineOffsets = append([]int(nil), file.Lines()...)
		}
		state.sources = append(state.sources, Source{
			ID:          sourceID,
			Name:        file.Name(),
			Size:        file.Size(),
			LineOffsets: lineOffsets,
		})
	}
	offset := file.Offset(position)
	if offset < 0 || offset > file.Size() {
		return Position{}, fmt.Errorf("position %d has invalid file offset %d", position, offset)
	}
	return Position{Source: sourceID, ByteOffset: offset}, nil
}

func nilASTNode(node ast.Node) bool {
	if node == nil {
		return true
	}
	value := reflect.ValueOf(node)
	return value.Kind() == reflect.Pointer && value.IsNil()
}
