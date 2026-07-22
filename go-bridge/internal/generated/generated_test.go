package generated

import (
	"go/ast"
	"go/token"
	"reflect"
	"strings"
	"testing"
)

func TestNewNodeAndNodeSpecs(t *testing.T) {
	fieldSpecType := reflect.TypeOf(FieldSpec{})
	wantFieldSpecFields := []string{"Name", "Kind", "Optional", "Interface", "Element", "EnumType"}
	if fieldSpecType.NumField() != len(wantFieldSpecFields) {
		t.Fatalf("FieldSpec has %d fields, want %d", fieldSpecType.NumField(), len(wantFieldSpecFields))
	}
	for index, name := range wantFieldSpecFields {
		if fieldSpecType.Field(index).Name != name {
			t.Errorf("FieldSpec field %d = %s, want %s", index, fieldSpecType.Field(index).Name, name)
		}
	}

	node, err := NewNode("BinaryExpr")
	if err != nil {
		t.Fatalf("NewNode(BinaryExpr): %v", err)
	}
	if _, ok := node.(*ast.BinaryExpr); !ok {
		t.Fatalf("NewNode(BinaryExpr) returned %T, want *ast.BinaryExpr", node)
	}
	if kind, ok := NodeKind(node); !ok || kind != "BinaryExpr" {
		t.Fatalf("NodeKind(%T) = %q, %v; want BinaryExpr, true", node, kind, ok)
	}

	spec, ok := NodeSpecs["BinaryExpr"]
	if !ok {
		t.Fatal("NodeSpecs has no BinaryExpr entry")
	}
	if spec.Namespace != "go/ast" || !reflect.DeepEqual(spec.Interfaces, []string{"Node", "Expr"}) {
		t.Fatalf("unexpected BinaryExpr spec: %#v", spec)
	}
	wantFields := []FieldSpec{
		{Name: "X", Kind: "node", Optional: false, Interface: "Expr"},
		{Name: "OpPos", Kind: "position", Optional: true},
		{Name: "Op", Kind: "token", Optional: false},
		{Name: "Y", Kind: "node", Optional: false, Interface: "Expr"},
	}
	if len(spec.Fields) != len(wantFields) {
		t.Fatalf("BinaryExpr has %d fields, want %d", len(spec.Fields), len(wantFields))
	}
	for index, want := range wantFields {
		got := spec.Fields[index]
		if got.Name != want.Name || got.Kind != want.Kind || got.Optional != want.Optional || got.Interface != want.Interface {
			t.Errorf("BinaryExpr field %d = %#v, want name=%q kind=%q optional=%v interface=%q", index, got, want.Name, want.Kind, want.Optional, want.Interface)
		}
	}

	if _, err := NewNode("NotANode"); err == nil || !strings.Contains(err.Error(), "unknown go/ast node kind") {
		t.Fatalf("NewNode(NotANode) error = %v, want unknown-kind error", err)
	}
}

func TestGetAndSetNodeFields(t *testing.T) {
	t.Run("node", func(t *testing.T) {
		expression := new(ast.BinaryExpr)
		left := ast.NewIdent("left")
		right := ast.NewIdent("right")

		if err := SetNodeField(expression, "X", left); err != nil {
			t.Fatalf("SetNodeField(X): %v", err)
		}
		if err := SetNodeField(expression, "Y", right); err != nil {
			t.Fatalf("SetNodeField(Y): %v", err)
		}
		if err := SetNodeField(expression, "Op", token.ADD); err != nil {
			t.Fatalf("SetNodeField(Op): %v", err)
		}

		got, err := GetNodeField(expression, "X")
		if err != nil {
			t.Fatalf("GetNodeField(X): %v", err)
		}
		if got != ast.Node(left) {
			t.Fatalf("GetNodeField(X) = %v, want original Ident", got)
		}
		if expression.Y != right || expression.Op != token.ADD {
			t.Fatalf("fields were not assigned: %#v", expression)
		}

		if err := SetNodeField(expression, "X", nil); err != nil {
			t.Fatalf("SetNodeField(X, nil): %v", err)
		}
		got, err = GetNodeField(expression, "X")
		if err != nil {
			t.Fatalf("GetNodeField(nil X): %v", err)
		}
		if got != nil {
			t.Fatalf("GetNodeField(nil X) = %#v, want nil ast.Node", got)
		}

		if err := SetNodeField(expression, "X", new(ast.BlockStmt)); err == nil {
			t.Fatal("SetNodeField accepted a Stmt for an Expr field")
		}
		if _, err := GetNodeField(expression, "Missing"); err == nil {
			t.Fatal("GetNodeField accepted an unknown field")
		}
	})

	t.Run("node-list", func(t *testing.T) {
		call := new(ast.CallExpr)
		children := []ast.Node{ast.NewIdent("first"), &ast.BasicLit{Kind: token.INT, Value: "2"}}
		if err := SetNodeField(call, "Args", children); err != nil {
			t.Fatalf("SetNodeField(Args): %v", err)
		}
		got, err := GetNodeField(call, "Args")
		if err != nil {
			t.Fatalf("GetNodeField(Args): %v", err)
		}
		if !reflect.DeepEqual(got, children) {
			t.Fatalf("GetNodeField(Args) = %#v, want %#v", got, children)
		}

		if err := SetNodeField(call, "Args", []ast.Node{}); err != nil {
			t.Fatalf("SetNodeField(Args, empty): %v", err)
		}
		got = mustGetField(t, call, "Args")
		if nodes := got.([]ast.Node); nodes == nil || len(nodes) != 0 {
			t.Fatalf("empty Args round trip = %#v, want non-nil empty slice", nodes)
		}
		if err := SetNodeField(call, "Args", nil); err != nil {
			t.Fatalf("SetNodeField(Args, nil): %v", err)
		}
		if nodes := mustGetField(t, call, "Args").([]ast.Node); nodes != nil {
			t.Fatalf("nil Args round trip = %#v, want nil slice", nodes)
		}
	})

	t.Run("node-map", func(t *testing.T) {
		pkg := new(ast.Package)
		files := map[string]ast.Node{
			"a.go": &ast.File{Name: ast.NewIdent("p")},
			"b.go": &ast.File{Name: ast.NewIdent("p")},
		}
		if err := SetNodeField(pkg, "Files", files); err != nil {
			t.Fatalf("SetNodeField(Files): %v", err)
		}
		got := mustGetField(t, pkg, "Files")
		if !reflect.DeepEqual(got, files) {
			t.Fatalf("GetNodeField(Files) = %#v, want %#v", got, files)
		}

		if err := SetNodeField(pkg, "Files", map[string]ast.Node{}); err != nil {
			t.Fatalf("SetNodeField(Files, empty): %v", err)
		}
		gotMap := mustGetField(t, pkg, "Files").(map[string]ast.Node)
		if gotMap == nil || len(gotMap) != 0 {
			t.Fatalf("empty Files round trip = %#v, want non-nil empty map", gotMap)
		}
		if err := SetNodeField(pkg, "Files", nil); err != nil {
			t.Fatalf("SetNodeField(Files, nil): %v", err)
		}
		if gotMap := mustGetField(t, pkg, "Files").(map[string]ast.Node); gotMap != nil {
			t.Fatalf("nil Files round trip = %#v, want nil map", gotMap)
		}
	})
}

func TestTokenRegistryRoundTripAndErrors(t *testing.T) {
	for _, test := range []struct {
		name     string
		value    token.Token
		spelling string
	}{
		{name: "ADD", value: token.ADD, spelling: "+"},
		{name: "FUNC", value: token.FUNC, spelling: "func"},
		{name: "IDENT", value: token.IDENT, spelling: "IDENT"},
	} {
		value, err := ParseToken(test.name)
		if err != nil {
			t.Fatalf("ParseToken(%q): %v", test.name, err)
		}
		if value != test.value {
			t.Errorf("ParseToken(%q) = %v, want %v", test.name, value, test.value)
		}
		name, err := TokenName(value)
		if err != nil {
			t.Fatalf("TokenName(%v): %v", value, err)
		}
		if name != test.name {
			t.Errorf("TokenName(ParseToken(%q)) = %q", test.name, name)
		}
		spelling, err := TokenSpelling(test.name)
		if err != nil {
			t.Fatalf("TokenSpelling(%q): %v", test.name, err)
		}
		if spelling != test.spelling {
			t.Errorf("TokenSpelling(%q) = %q, want %q", test.name, spelling, test.spelling)
		}
	}

	if _, err := ParseToken("NOT_A_TOKEN"); err == nil {
		t.Fatal("ParseToken accepted an unknown token name")
	}
	if _, err := TokenName(token.Token(1 << 20)); err == nil {
		t.Fatal("TokenName accepted an unknown token value")
	}
	if _, err := TokenSpelling("NOT_A_TOKEN"); err == nil {
		t.Fatal("TokenSpelling accepted an unknown token name")
	}
}

func TestChanDirEnumRoundTripAndErrors(t *testing.T) {
	const enumType = "go/ast.ChanDir"
	value, err := ParseEnum(enumType, []string{"SEND", "RECV"})
	if err != nil {
		t.Fatalf("ParseEnum(SEND|RECV): %v", err)
	}
	if value != ast.ChanDir(ast.SEND|ast.RECV) {
		t.Fatalf("ParseEnum(SEND|RECV) = %#v, want %v", value, ast.SEND|ast.RECV)
	}
	names, err := EnumNames(enumType, value)
	if err != nil {
		t.Fatalf("EnumNames(SEND|RECV): %v", err)
	}
	if !reflect.DeepEqual(names, []string{"SEND", "RECV"}) {
		t.Fatalf("EnumNames(SEND|RECV) = %#v", names)
	}

	for name, operation := range map[string]func() error{
		"unknown enum type":   func() error { _, err := ParseEnum("example.Invalid", []string{"SEND"}); return err },
		"unknown enum name":   func() error { _, err := ParseEnum(enumType, []string{"BOTH"}); return err },
		"duplicate enum name": func() error { _, err := ParseEnum(enumType, []string{"SEND", "SEND"}); return err },
		"wrong enum Go type":  func() error { _, err := EnumNames(enumType, int(ast.SEND)); return err },
		"unknown enum bits":   func() error { _, err := EnumNames(enumType, ast.ChanDir(4)); return err },
	} {
		t.Run(name, func(t *testing.T) {
			if err := operation(); err == nil {
				t.Fatalf("%s did not return an error", name)
			}
		})
	}
}

func TestChildEdgesSortsNodeMapKeys(t *testing.T) {
	pkg := new(ast.Package)
	files := map[string]ast.Node{
		"z.go": &ast.File{Name: ast.NewIdent("p")},
		"a.go": &ast.File{Name: ast.NewIdent("p")},
		"m.go": nil,
	}
	if err := SetNodeField(pkg, "Files", files); err != nil {
		t.Fatalf("SetNodeField(Files): %v", err)
	}
	edges := ChildEdges(pkg)
	if len(edges) != 2 {
		t.Fatalf("ChildEdges(Package) = %#v, want two non-nil edges", edges)
	}
	if edges[0].Field != "Files" || edges[0].Key != "a.go" || edges[0].Index != -1 {
		t.Errorf("first edge = %#v, want Files[a.go]", edges[0])
	}
	if edges[1].Field != "Files" || edges[1].Key != "z.go" || edges[1].Index != -1 {
		t.Errorf("second edge = %#v, want Files[z.go]", edges[1])
	}
}

func TestStructuralEqual(t *testing.T) {
	t.Run("nil node fields", func(t *testing.T) {
		left := &ast.BranchStmt{Tok: token.BREAK}
		right := &ast.BranchStmt{Tok: token.BREAK}
		if !StructuralEqual(left, right, nil) {
			t.Fatal("identical BranchStmt nodes with nil Label are not equal")
		}
		right.Label = ast.NewIdent("done")
		if StructuralEqual(left, right, nil) {
			t.Fatal("nil Label was equal to a non-nil Label")
		}
		var nilIdent *ast.Ident
		if !StructuralEqual(nilIdent, nil, nil) {
			t.Fatal("typed nil ast.Node was not equal to nil")
		}
	})

	t.Run("nil versus empty node list", func(t *testing.T) {
		left := &ast.CallExpr{Fun: ast.NewIdent("f"), Args: nil}
		right := &ast.CallExpr{Fun: ast.NewIdent("f"), Args: []ast.Expr{}}
		if StructuralEqual(left, right, nil) {
			t.Fatal("nil Args was equal to an empty Args slice")
		}
	})

	t.Run("nil versus empty node map", func(t *testing.T) {
		left := &ast.Package{Name: "p", Files: nil}
		right := &ast.Package{Name: "p", Files: map[string]*ast.File{}}
		if StructuralEqual(left, right, nil) {
			t.Fatal("nil Files was equal to an empty Files map")
		}
	})

	t.Run("position comparator", func(t *testing.T) {
		left := &ast.BasicLit{ValuePos: 11, Kind: token.INT, Value: "1"}
		right := &ast.BasicLit{ValuePos: 21, Kind: token.INT, Value: "1"}
		wantCalls := 1
		if nodeSpecHasField(NodeSpecs["BasicLit"], "ValueEnd") {
			if err := SetNodeField(left, "ValueEnd", token.Pos(17)); err != nil {
				t.Fatalf("SetNodeField(left ValueEnd): %v", err)
			}
			if err := SetNodeField(right, "ValueEnd", token.Pos(27)); err != nil {
				t.Fatalf("SetNodeField(right ValueEnd): %v", err)
			}
			wantCalls++
		}
		if StructuralEqual(left, right, nil) {
			t.Fatal("different positions were equal with the default comparator")
		}
		calls := 0
		equalLastDigit := func(left, right token.Pos) bool {
			calls++
			return left%10 == right%10
		}
		if !StructuralEqual(left, right, equalLastDigit) {
			t.Fatal("custom position comparator was not honored")
		}
		if calls != wantCalls {
			t.Fatalf("position comparator called %d times, want %d", calls, wantCalls)
		}
	})
}

func mustGetField(t *testing.T, node ast.Node, name string) any {
	t.Helper()
	value, err := GetNodeField(node, name)
	if err != nil {
		t.Fatalf("GetNodeField(%T, %q): %v", node, name, err)
	}
	return value
}

func nodeSpecHasField(spec NodeSpec, name string) bool {
	for _, field := range spec.Fields {
		if field.Name == name {
			return true
		}
	}
	return false
}
