// Package codec defines Gorack's stable graph envelope and the bidirectional
// conversion between that envelope and the active toolchain's go/ast types.
package codec

import (
	"encoding/json"
	"fmt"
	"sort"
)

const (
	WireFormat        = "gorack-go-ast"
	WireFormatVersion = 1
	GoASTNamespace    = "go/ast"
)

type NodeID string
type OriginID string

type SchemaIdentity struct {
	GoVersion        string `json:"goVersion"`
	SchemaHash       string `json:"schemaHash"`
	ASTPackageHash   string `json:"astPackageHash"`
	TokenPackageHash string `json:"tokenPackageHash"`
}

type Unit struct {
	Format        string           `json:"format"`
	FormatVersion int              `json:"formatVersion"`
	GoSchema      SchemaIdentity   `json:"goSchema"`
	Root          NodeID           `json:"root"`
	Nodes         []WireNode       `json:"nodes"`
	Sources       []Source         `json:"sources,omitempty"`
	Origins       []Origin         `json:"origins,omitempty"`
	Annotations   []Annotation     `json:"annotations,omitempty"`
	Extensions    []ExtensionValue `json:"extensions,omitempty"`
}

type WireNode struct {
	ID        NodeID                     `json:"id"`
	Namespace string                     `json:"namespace"`
	Kind      string                     `json:"kind"`
	Fields    map[string]json.RawMessage `json:"fields"`
	Origin    OriginID                   `json:"origin,omitempty"`
}

type Source struct {
	ID          string `json:"id"`
	Name        string `json:"name"`
	Size        int    `json:"size"`
	ContentHash string `json:"contentHash,omitempty"`
	// LineOffsets preserves token.File's file-relative line table. Positions
	// remain byte offsets on the wire, while this optional table lets a
	// reconstructed FileSet report the same line/column information to the Go
	// printer and diagnostics. Older units may omit it.
	LineOffsets []int `json:"lineOffsets,omitempty"`
}

type Position struct {
	Source     string `json:"source"`
	ByteOffset int    `json:"byteOffset"`
}

type Span struct {
	Source    string `json:"source"`
	StartByte int    `json:"startByte"`
	EndByte   int    `json:"endByte"`
}

type Origin struct {
	ID          OriginID       `json:"id"`
	Kind        string         `json:"kind"`
	Pass        string         `json:"pass,omitempty"`
	Primary     OriginID       `json:"primary,omitempty"`
	Inputs      []OriginID     `json:"inputs,omitempty"`
	SourceSpans []Span         `json:"sourceSpans,omitempty"`
	Metadata    map[string]any `json:"metadata,omitempty"`
}

type Annotation struct {
	Namespace string          `json:"namespace"`
	Node      NodeID          `json:"node"`
	Value     json.RawMessage `json:"value"`
}

type ExtensionValue struct {
	Namespace string          `json:"namespace"`
	Version   int             `json:"version"`
	Value     json.RawMessage `json:"value"`
}

type RefValue struct {
	Ref NodeID `json:"$ref"`
}

type PositionValue struct {
	Position Position `json:"$position"`
}

type TokenValue struct {
	Token string `json:"$token"`
}

type EnumPayload struct {
	Type  string   `json:"type"`
	Names []string `json:"names"`
}

type EnumValue struct {
	Enum EnumPayload `json:"$enum"`
}

type GeneratedRange struct {
	File      string `json:"file"`
	StartByte int    `json:"startByte"`
	EndByte   int    `json:"endByte"`
}

type SourceMapEntry struct {
	Generated GeneratedRange `json:"generated"`
	Node      NodeID         `json:"node"`
	Origin    OriginID       `json:"origin,omitempty"`
}

type SourceMapDocument struct {
	Format        string           `json:"format"`
	FormatVersion int              `json:"formatVersion"`
	Entries       []SourceMapEntry `json:"entries"`
}

func NewUnit(identity SchemaIdentity) *Unit {
	return &Unit{
		Format:        WireFormat,
		FormatVersion: WireFormatVersion,
		GoSchema:      identity,
	}
}

func rawJSON(value any) (json.RawMessage, error) {
	data, err := json.Marshal(value)
	if err != nil {
		return nil, err
	}
	return data, nil
}

func mustRawJSON(value any) json.RawMessage {
	data, err := rawJSON(value)
	if err != nil {
		panic(err)
	}
	return data
}

func isNull(raw json.RawMessage) bool {
	return len(raw) == 0 || string(raw) == "null"
}

func nodeIndex(unit *Unit) map[NodeID]*WireNode {
	index := make(map[NodeID]*WireNode, len(unit.Nodes))
	for i := range unit.Nodes {
		index[unit.Nodes[i].ID] = &unit.Nodes[i]
	}
	return index
}

func sortedFieldNames(fields map[string]json.RawMessage) []string {
	names := make([]string, 0, len(fields))
	for name := range fields {
		names = append(names, name)
	}
	sort.Strings(names)
	return names
}

func decodeTaggedObject(raw json.RawMessage) (map[string]json.RawMessage, error) {
	var object map[string]json.RawMessage
	if err := json.Unmarshal(raw, &object); err != nil {
		return nil, err
	}
	if object == nil {
		return nil, fmt.Errorf("expected tagged object, got %s", raw)
	}
	return object, nil
}
