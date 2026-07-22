package codec

import (
	"encoding/json"
	"go/ast"
	"go/token"
	"math"
	"strings"
	"testing"
)

func TestValidateEnvelopeGraphAndMetadata(t *testing.T) {
	unit := NewUnit(SchemaIdentity{
		GoVersion:        "go-test",
		SchemaHash:       "sha256:schema",
		ASTPackageHash:   "sha256:ast",
		TokenPackageHash: "sha256:token",
	})
	unit.Root = "n1"
	unit.Sources = []Source{{ID: "source-1", Name: "main.go", Size: 20}}
	unit.Origins = []Origin{{
		ID:          "o1",
		Kind:        "source",
		SourceSpans: []Span{{Source: "source-1", StartByte: 0, EndByte: 20}},
	}}
	unit.Nodes = []WireNode{
		{
			ID:        "n1",
			Namespace: GoASTNamespace,
			Kind:      "BinaryExpr",
			Origin:    "o1",
			Fields: map[string]json.RawMessage{
				"X":     mustRawJSON(RefValue{Ref: "n2"}),
				"OpPos": mustRawJSON(PositionValue{Position: Position{Source: "source-1", ByteOffset: 8}}),
				"Op":    mustRawJSON(TokenValue{Token: "ADD"}),
				"Y":     mustRawJSON(RefValue{Ref: "n2"}),
			},
		},
		{
			ID:        "n2",
			Namespace: GoASTNamespace,
			Kind:      "Ident",
			Fields: map[string]json.RawMessage{
				"Name": mustRawJSON("x"),
			},
		},
	}
	unit.Annotations = []Annotation{{
		Namespace: "gorack.types.v1",
		Node:      "n1",
		Value:     json.RawMessage(`{"type":"int"}`),
	}}

	if err := ValidateEnvelope(unit); err != nil {
		t.Fatalf("ValidateEnvelope: %v", err)
	}
}

func TestDecodeRejectsFileSetSizeOverflow(t *testing.T) {
	unit := NewUnit(ActiveSchemaIdentity())
	unit.Root = "n1"
	unit.Sources = []Source{{ID: "source-1", Name: "huge.go", Size: math.MaxInt}}
	unit.Nodes = []WireNode{{
		ID: "n1", Namespace: GoASTNamespace, Kind: "Ident",
		Fields: map[string]json.RawMessage{
			"NamePos": mustRawJSON(PositionValue{Position: Position{Source: "source-1", ByteOffset: 0}}),
			"Name":    mustRawJSON("x"),
		},
	}}
	if _, err := Decode(unit, DecodeOptions{}); err == nil || !strings.Contains(err.Error(), "too large") {
		t.Fatalf("got %v, want token.FileSet size error", err)
	}
}

func TestValidateEnvelopeRejectsMissingReference(t *testing.T) {
	unit := NewUnit(testSchemaIdentity())
	unit.Root = "n1"
	unit.Nodes = []WireNode{{
		ID:        "n1",
		Namespace: GoASTNamespace,
		Kind:      "Ident",
		Fields: map[string]json.RawMessage{
			"Child": mustRawJSON(RefValue{Ref: "missing"}),
		},
	}}

	err := ValidateEnvelope(unit)
	if err == nil || !strings.Contains(err.Error(), "unknown node") {
		t.Fatalf("got %v, want unknown-node error", err)
	}
}

func TestValidateEnvelopeRejectsOriginCycle(t *testing.T) {
	unit := NewUnit(testSchemaIdentity())
	unit.Root = "n1"
	unit.Nodes = []WireNode{{
		ID:        "n1",
		Namespace: GoASTNamespace,
		Kind:      "Ident",
		Fields:    map[string]json.RawMessage{"Name": mustRawJSON("x")},
	}}
	unit.Origins = []Origin{
		{ID: "o1", Kind: "derived", Primary: "o2"},
		{ID: "o2", Kind: "derived", Primary: "o1"},
	}

	err := ValidateEnvelope(unit)
	if err == nil || !strings.Contains(err.Error(), "cycle") {
		t.Fatalf("got %v, want cycle error", err)
	}
}

func testSchemaIdentity() SchemaIdentity {
	return SchemaIdentity{
		GoVersion:        "go-test",
		SchemaHash:       "sha256:schema",
		ASTPackageHash:   "sha256:ast",
		TokenPackageHash: "sha256:token",
	}
}

func TestValidateEnvelopeRejectsDuplicateAnnotation(t *testing.T) {
	unit := NewUnit(testSchemaIdentity())
	unit.Root = "n1"
	unit.Nodes = []WireNode{{
		ID: "n1", Namespace: GoASTNamespace, Kind: "Ident",
		Fields: map[string]json.RawMessage{"Name": mustRawJSON("x")},
	}}
	unit.Annotations = []Annotation{
		{Namespace: "gorack.types.v1", Node: "n1", Value: json.RawMessage(`null`)},
		{Namespace: "gorack.types.v1", Node: "n1", Value: json.RawMessage(`false`)},
	}
	if err := ValidateEnvelope(unit); err == nil || !strings.Contains(err.Error(), "duplicate annotation") {
		t.Fatalf("got %v, want duplicate-annotation error", err)
	}
}

func TestValidateEnvelopeAcceptsSlashExtensionNamespace(t *testing.T) {
	unit := NewUnit(testSchemaIdentity())
	unit.Root = "n1"
	unit.Nodes = []WireNode{{
		ID: "n1", Namespace: "gorack/core", Kind: "Atomic",
		Fields: map[string]json.RawMessage{},
	}}
	unit.Extensions = []ExtensionValue{{
		Namespace: "gorack/core", Version: 1, Value: json.RawMessage(`{"enabled":true}`),
	}}
	if err := ValidateEnvelope(unit); err != nil {
		t.Fatalf("ValidateEnvelope: %v", err)
	}
}

func TestDecodeRejectsNonCanonicalTaggedFields(t *testing.T) {
	tests := []struct {
		name   string
		root   ast.Node
		field  string
		mutate json.RawMessage
	}{
		{
			name:   "token outer member",
			root:   &ast.BasicLit{Kind: token.INT, Value: "1"},
			field:  "Kind",
			mutate: json.RawMessage(`{"$token":"INT","junk":123}`),
		},
		{
			name:   "enum payload member",
			root:   &ast.ChanType{Dir: ast.SEND, Value: ast.NewIdent("int")},
			field:  "Dir",
			mutate: json.RawMessage(`{"$enum":{"type":"go/ast.ChanDir","names":["SEND"],"junk":123}}`),
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			unit, err := Encode(test.root, token.NewFileSet(), EncodeOptions{})
			if err != nil {
				t.Fatal(err)
			}
			unit.Nodes[0].Fields[test.field] = test.mutate
			if _, err := Decode(unit, DecodeOptions{}); err == nil {
				t.Fatal("Decode accepted a tagged value with unpreserved members")
			}
		})
	}

	unit := NewUnit(ActiveSchemaIdentity())
	unit.Root = "n1"
	unit.Sources = []Source{{ID: "source-1", Name: "x.go", Size: 1}}
	unit.Nodes = []WireNode{{
		ID: "n1", Namespace: GoASTNamespace, Kind: "Ident",
		Fields: map[string]json.RawMessage{
			"NamePos": json.RawMessage(`{"$position":{"source":"source-1","byteOffset":0,"junk":123}}`),
			"Name":    mustRawJSON("x"),
		},
	}}
	if _, err := Decode(unit, DecodeOptions{}); err == nil {
		t.Fatal("Decode accepted a position payload with an unpreserved member")
	}
}

func TestValidateEnvelopeRejectsInvalidNodeNamespace(t *testing.T) {
	unit := NewUnit(testSchemaIdentity())
	unit.Root = "n1"
	unit.Nodes = []WireNode{{
		ID: "n1", Namespace: "bad namespace", Kind: "Ident",
		Fields: map[string]json.RawMessage{"Name": mustRawJSON("x")},
	}}
	if err := ValidateEnvelope(unit); err == nil || !strings.Contains(err.Error(), "invalid namespace") {
		t.Fatalf("got %v, want invalid namespace error", err)
	}
}
