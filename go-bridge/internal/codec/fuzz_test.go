package codec

import (
	"go/parser"
	"go/token"
	"testing"

	"github.com/gorack/gorack/go-bridge/internal/generated"
)

// FuzzASTRoundTrip makes schema changes exercise the complete generated
// constructor/getter/setter registry. Invalid Go text is simply outside this
// fuzz target; every parseable seed must survive the graph codec.
func FuzzASTRoundTrip(f *testing.F) {
	for _, source := range []string{
		"package p\nvar X = 1 + 2\n",
		"package p\nfunc F[T ~int](v T) T { return v }\n",
		"package p\n// doc\ntype T struct { X chan<- int }\n",
	} {
		f.Add(source)
	}

	f.Fuzz(func(t *testing.T, source string) {
		if len(source) > 1<<20 {
			t.Skip()
		}
		originalSet := token.NewFileSet()
		original, err := parser.ParseFile(originalSet, "fuzz.go", source,
			parser.ParseComments|parser.SkipObjectResolution)
		if err != nil {
			return
		}
		unit, err := Encode(original, originalSet, EncodeOptions{})
		if err != nil {
			t.Fatalf("Encode: %v", err)
		}
		decoded, err := Decode(unit, DecodeOptions{})
		if err != nil {
			t.Fatalf("Decode: %v", err)
		}
		if !generated.StructuralEqual(original, decoded.Root, func(left, right token.Pos) bool {
			if !left.IsValid() || !right.IsValid() {
				return left.IsValid() == right.IsValid()
			}
			leftPosition := originalSet.Position(left)
			rightPosition := decoded.FileSet.Position(right)
			return leftPosition.Filename == rightPosition.Filename &&
				leftPosition.Offset == rightPosition.Offset
		}) {
			t.Fatal("round-tripped AST is not structurally equal")
		}
		formatted, _, err := FormatFileWithSourceMap(decoded, "generated.go")
		if err != nil {
			t.Fatalf("format and source-map: %v", err)
		}
		if _, err := parser.ParseFile(token.NewFileSet(), "generated.go", formatted,
			parser.ParseComments|parser.SkipObjectResolution); err != nil {
			t.Fatalf("reparse formatted output: %v", err)
		}
	})
}
