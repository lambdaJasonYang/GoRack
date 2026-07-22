package codec

import (
	"encoding/json"
	"go/ast"
	"go/parser"
	"go/token"
	"strings"
	"testing"

	"github.com/gorack/gorack/go-bridge/internal/generated"
)

func TestASTGraphRoundTrip(t *testing.T) {
	const source = `// Package demo exercises the bridge.
package demo

import "fmt"

type Box[T any] struct { Values []T }

var Both chan int

func Add(xs []int) []int {
	for i, value := range xs {
		xs[i] = value + 1
	}
	for range xs {
		fmt.Println(xs[:])
	}
	return xs
}
`
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "demo.go", source,
		parser.ParseComments|parser.SkipObjectResolution)
	if err != nil {
		t.Fatal(err)
	}
	if file.Doc == nil || len(file.Comments) == 0 || file.Doc != file.Comments[0] {
		t.Fatal("test fixture does not contain a shared package comment")
	}

	unit, err := Encode(file, fset, EncodeOptions{})
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	data, err := json.Marshal(unit)
	if err != nil {
		t.Fatal(err)
	}
	jsonText := string(data)
	for _, symbolic := range []string{`"$token":"ADD"`, `"$token":"ILLEGAL"`, `"names":["SEND","RECV"]`} {
		if !strings.Contains(jsonText, symbolic) {
			t.Errorf("wire JSON lacks symbolic value %s", symbolic)
		}
	}

	decoded, err := Decode(unit, DecodeOptions{})
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	decodedFile := decoded.Root.(*ast.File)
	if decodedFile.Doc != decodedFile.Comments[0] {
		t.Error("shared CommentGroup identity was not preserved")
	}
	genDecl := decodedFile.Decls[0].(*ast.GenDecl)
	if decodedFile.Imports[0] != genDecl.Specs[0].(*ast.ImportSpec) {
		t.Error("shared ImportSpec identity was not preserved")
	}
	if got, want := decoded.FileSet.Position(decodedFile.Name.NamePos).Offset,
		fset.Position(file.Name.NamePos).Offset; got != want {
		t.Errorf("decoded NamePos offset = %d, want %d", got, want)
	}
	if got, want := decoded.FileSet.Position(decodedFile.Name.NamePos).Line,
		fset.Position(file.Name.NamePos).Line; got != want {
		t.Errorf("decoded NamePos line = %d, want %d", got, want)
	}
	if !generated.StructuralEqual(file, decodedFile, func(left, right token.Pos) bool {
		if !left.IsValid() || !right.IsValid() {
			return left.IsValid() == right.IsValid()
		}
		return fset.Position(left).Offset == decoded.FileSet.Position(right).Offset
	}) {
		t.Error("generated structural equality rejected round-tripped AST")
	}

	formatted, sourceMap, err := FormatFileWithSourceMap(decoded, "generated.go")
	if err != nil {
		t.Fatalf("FormatFileWithSourceMap: %v", err)
	}
	if _, err := parser.ParseFile(token.NewFileSet(), "generated.go", formatted, parser.AllErrors); err != nil {
		t.Fatalf("formatted output does not parse: %v\n%s", err, formatted)
	}
	if len(sourceMap) == 0 {
		t.Fatal("source map is empty")
	}
	for _, entry := range sourceMap {
		if entry.Generated.StartByte < 0 || entry.Generated.EndByte < entry.Generated.StartByte || entry.Generated.EndByte > len(formatted) {
			t.Fatalf("invalid generated range: %+v (output length %d)", entry, len(formatted))
		}
	}
}

func TestNilAndEmptyNodeListsRemainDistinct(t *testing.T) {
	for _, test := range []struct {
		name string
		root *ast.FieldList
		nil  bool
	}{
		{name: "nil", root: &ast.FieldList{List: nil}, nil: true},
		{name: "empty", root: &ast.FieldList{List: []*ast.Field{}}, nil: false},
	} {
		t.Run(test.name, func(t *testing.T) {
			unit, err := Encode(test.root, token.NewFileSet(), EncodeOptions{})
			if err != nil {
				t.Fatal(err)
			}
			decoded, err := Decode(unit, DecodeOptions{})
			if err != nil {
				t.Fatal(err)
			}
			gotNil := decoded.Root.(*ast.FieldList).List == nil
			if gotNil != test.nil {
				t.Fatalf("decoded nil = %t, want %t", gotNil, test.nil)
			}
		})
	}
}

func TestSchemaMismatchRejected(t *testing.T) {
	unit, err := Encode(ast.NewIdent("x"), token.NewFileSet(), EncodeOptions{})
	if err != nil {
		t.Fatal(err)
	}
	unit.GoSchema.SchemaHash = "sha256:not-the-active-schema"
	if _, err := Decode(unit, DecodeOptions{}); err == nil || !strings.Contains(err.Error(), "schema mismatch") {
		t.Fatalf("got %v, want schema mismatch", err)
	}
}

func TestEncodeRejectsInvalidRequiredChildren(t *testing.T) {
	if _, err := Encode(&ast.BinaryExpr{}, token.NewFileSet(), EncodeOptions{}); err == nil ||
		!strings.Contains(err.Error(), "null required field X") {
		t.Fatalf("got %v, want required-child validation error", err)
	}
}

func TestEncodeRejectsASTChildCycle(t *testing.T) {
	root := &ast.ParenExpr{}
	root.X = root
	if _, err := Encode(root, token.NewFileSet(), EncodeOptions{}); err == nil ||
		!strings.Contains(err.Error(), "child graph contains a cycle") {
		t.Fatalf("got %v, want AST-cycle validation error", err)
	}
}

func TestPackageMapAllowsTagShapedKeys(t *testing.T) {
	file := &ast.File{Name: ast.NewIdent("p")}
	pkg := &ast.Package{Name: "p", Files: map[string]*ast.File{
		"$ref": file, "$token": file, "$position": file, "$enum": file,
	}}
	unit, err := Encode(pkg, token.NewFileSet(), EncodeOptions{})
	if err != nil {
		t.Fatal(err)
	}
	decoded, err := Decode(unit, DecodeOptions{})
	if err != nil {
		t.Fatal(err)
	}
	files := decoded.Root.(*ast.Package).Files
	for _, key := range []string{"$ref", "$token", "$position", "$enum"} {
		if files[key] == nil {
			t.Errorf("decoded package lost tag-shaped map key %q", key)
		}
	}
}

func TestZeroLengthSourceHasNoInvalidLineTable(t *testing.T) {
	set := token.NewFileSet()
	file := set.AddFile("empty.go", -1, 0)
	root := &ast.BadExpr{From: file.Pos(0), To: file.Pos(0)}
	unit, err := Encode(root, set, EncodeOptions{})
	if err != nil {
		t.Fatal(err)
	}
	if len(unit.Sources) != 1 || len(unit.Sources[0].LineOffsets) != 0 {
		t.Fatalf("zero-length source line offsets = %v", unit.Sources)
	}
	if _, err := Decode(unit, DecodeOptions{}); err != nil {
		t.Fatal(err)
	}
}
