package codec

import (
	"bytes"
	"go/format"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/gorack/gorack/go-bridge/internal/generated"
)

// TestStandardLibraryRoundTrip is the release/toolchain compatibility gate.
// It is opt-in because it walks the entire selected GOROOT, but unlike a
// schema-only check it proves that real syntax survives AST -> IR -> AST and
// formatting under that toolchain.
func TestStandardLibraryRoundTrip(t *testing.T) {
	if os.Getenv("GORACK_VERIFY_STDLIB") != "1" {
		t.Skip("set GORACK_VERIFY_STDLIB=1 to verify the selected Go standard library")
	}
	root := filepath.Join(runtime.GOROOT(), "src")
	files := 0
	err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() {
			// `go list std` excludes the toolchain commands and vendored command
			// dependencies. This walk uses the same practical boundary while
			// retaining internal standard-library packages and their tests.
			if path != root && (entry.Name() == "cmd" || entry.Name() == "vendor" ||
				entry.Name() == "testdata" || strings.HasPrefix(entry.Name(), ".")) {
				return filepath.SkipDir
			}
			return nil
		}
		if filepath.Ext(path) != ".go" {
			return nil
		}
		files++
		set := token.NewFileSet()
		file, err := parser.ParseFile(set, path, nil,
			parser.ParseComments|parser.SkipObjectResolution)
		if err != nil {
			return err
		}
		unit, err := Encode(file, set, EncodeOptions{})
		if err != nil {
			return err
		}
		decoded, err := Decode(unit, DecodeOptions{})
		if err != nil {
			return err
		}
		if !generated.StructuralEqual(file, decoded.Root, func(left, right token.Pos) bool {
			if !left.IsValid() || !right.IsValid() {
				return left.IsValid() == right.IsValid()
			}
			return set.Position(left).Offset == decoded.FileSet.Position(right).Offset
		}) {
			return &stdlibVerificationError{path: path, operation: "structural comparison"}
		}
		var formatted bytes.Buffer
		if err := format.Node(&formatted, decoded.FileSet, decoded.Root); err != nil {
			return err
		}
		reparsedSet := token.NewFileSet()
		reparsed, err := parser.ParseFile(reparsedSet, path, formatted.Bytes(),
			parser.ParseComments|parser.SkipObjectResolution)
		if err != nil {
			return err
		}
		if !generated.StructuralEqual(decoded.Root, reparsed, func(left, right token.Pos) bool {
			return left.IsValid() == right.IsValid()
		}) {
			return &stdlibVerificationError{path: path, operation: "formatted/reparsed canonical comparison"}
		}
		return nil
	})
	if err != nil {
		t.Fatalf("standard-library verification after %d files: %v", files, err)
	}
	if files < 100 {
		t.Fatalf("standard-library verification found only %d Go files under %s", files, root)
	}
	t.Logf("verified %d Go source files under %s", files, root)
}

type stdlibVerificationError struct {
	path      string
	operation string
}

func (err *stdlibVerificationError) Error() string {
	return err.path + ": failed " + err.operation
}
