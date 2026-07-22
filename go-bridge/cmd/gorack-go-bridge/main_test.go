package main

import (
	"encoding/json"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/gorack/gorack/go-bridge/internal/codec"
)

func TestDecodeWithoutSourceMapAllowsParserCanonicalization(t *testing.T) {
	set := token.NewFileSet()
	file, err := parser.ParseFile(set, "input.go", "package p\nvar _ = T[int, string]\n",
		parser.SkipObjectResolution)
	if err != nil {
		t.Fatal(err)
	}
	indexList := file.Decls[0].(*ast.GenDecl).Specs[0].(*ast.ValueSpec).Values[0].(*ast.IndexListExpr)
	indexList.Indices = indexList.Indices[:1]
	unit, err := codec.Encode(file, set, codec.EncodeOptions{})
	if err != nil {
		t.Fatal(err)
	}
	directory := t.TempDir()
	input := filepath.Join(directory, "input.wire.json")
	output := filepath.Join(directory, "output.go")
	data, err := json.Marshal(unit)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(input, data, 0o644); err != nil {
		t.Fatal(err)
	}
	if err := runDecode([]string{"-in", input, "-out", output}); err != nil {
		t.Fatalf("ordinary decode failed on formattable AST: %v", err)
	}
	formatted, err := os.ReadFile(output)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := parser.ParseFile(token.NewFileSet(), output, formatted, parser.AllErrors); err != nil {
		t.Fatalf("decode output does not parse: %v", err)
	}

	mapPath := filepath.Join(directory, "output.map.json")
	err = runDecode([]string{"-in", input, "-out", output, "-source-map", mapPath})
	if err == nil || !strings.Contains(err.Error(), "changed kind") {
		t.Fatalf("source-map decode error = %v, want explicit kind-change diagnosis", err)
	}

	var envelope map[string]any
	if err := json.Unmarshal(data, &envelope); err != nil {
		t.Fatal(err)
	}
	envelope["futureEnvelopeField"] = true
	data, err = json.Marshal(envelope)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(input, data, 0o644); err != nil {
		t.Fatal(err)
	}
	err = runDecode([]string{"-in", input, "-out", output})
	if err == nil || !strings.Contains(err.Error(), "unknown field") {
		t.Fatalf("decode error = %v, want unknown-envelope-field rejection", err)
	}
}
