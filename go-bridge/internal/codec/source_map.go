package codec

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"sort"

	"github.com/gorack/gorack/go-bridge/internal/generated"
)

type PathStep struct {
	Field string `json:"field"`
	Index int    `json:"index,omitempty"`
	Key   string `json:"key,omitempty"`
}

type StructuralPath []PathStep

// StructuralPaths records the first generated child-edge path to each node.
// Choosing the first path is intentional for shared comment objects: the same
// policy is applied to the reparsed tree, so the join remains deterministic.
func StructuralPaths(root ast.Node) map[ast.Node]StructuralPath {
	result := make(map[ast.Node]StructuralPath)
	var walk func(ast.Node, StructuralPath)
	walk = func(node ast.Node, path StructuralPath) {
		if nilASTNode(node) {
			return
		}
		if _, seen := result[node]; seen {
			return
		}
		result[node] = append(StructuralPath(nil), path...)
		for _, edge := range generated.ChildEdges(node) {
			step := PathStep{Field: edge.Field, Index: edge.Index, Key: edge.Key}
			walk(edge.Node, append(path, step))
		}
	}
	walk(root, nil)
	return result
}

func FollowStructuralPath(root ast.Node, path StructuralPath) (ast.Node, error) {
	current := root
	for depth, step := range path {
		value, err := generated.GetNodeField(current, step.Field)
		if err != nil {
			return nil, fmt.Errorf("path step %d (%s): %w", depth, step.Field, err)
		}
		switch typed := value.(type) {
		case ast.Node:
			current = typed
		case []ast.Node:
			if step.Index < 0 || step.Index >= len(typed) {
				return nil, fmt.Errorf("path step %d (%s): index %d outside length %d", depth, step.Field, step.Index, len(typed))
			}
			current = typed[step.Index]
		case map[string]ast.Node:
			child, ok := typed[step.Key]
			if !ok {
				return nil, fmt.Errorf("path step %d (%s): key %q is absent", depth, step.Field, step.Key)
			}
			current = child
		default:
			return nil, fmt.Errorf("path step %d (%s) is not a child edge", depth, step.Field)
		}
		if nilASTNode(current) {
			return nil, fmt.Errorf("path step %d (%s) reached nil", depth, step.Field)
		}
	}
	return current, nil
}

func FormatFileWithSourceMap(decoded *Decoded, generatedFilename string) ([]byte, []SourceMapEntry, error) {
	output, err := FormatFile(decoded)
	if err != nil {
		return nil, nil, err
	}
	entries, err := SourceMapForFormatted(decoded, generatedFilename, output)
	if err != nil {
		return nil, nil, err
	}
	return output, entries, nil
}

func FormatFile(decoded *Decoded) ([]byte, error) {
	if decoded == nil || decoded.Root == nil {
		return nil, fmt.Errorf("nil decoded AST")
	}
	file, ok := decoded.Root.(*ast.File)
	if !ok {
		return nil, fmt.Errorf("Go source emission requires an *ast.File root, got %T", decoded.Root)
	}
	var output bytes.Buffer
	if err := format.Node(&output, decoded.FileSet, file); err != nil {
		return nil, fmt.Errorf("format decoded Go AST: %w", err)
	}
	return output.Bytes(), nil
}

func SourceMapForFormatted(decoded *Decoded, generatedFilename string, formatted []byte) ([]SourceMapEntry, error) {
	if decoded == nil || decoded.Unit == nil {
		return nil, fmt.Errorf("decoded AST has no wire unit")
	}
	paths := StructuralPaths(decoded.Root)
	reparsedSet := token.NewFileSet()
	reparsed, err := parser.ParseFile(reparsedSet, generatedFilename, formatted,
		parser.ParseComments|parser.SkipObjectResolution)
	if err != nil {
		return nil, fmt.Errorf("reparse formatted Go source: %w", err)
	}
	wireNodes := nodeIndex(decoded.Unit)
	entries := make([]SourceMapEntry, 0, len(decoded.Nodes))
	ids := make([]NodeID, 0, len(decoded.Nodes))
	for id := range decoded.Nodes {
		ids = append(ids, id)
	}
	sort.Slice(ids, func(i, j int) bool { return ids[i] < ids[j] })
	for _, id := range ids {
		original := decoded.Nodes[id]
		path, reachable := paths[original]
		if !reachable {
			continue
		}
		reparsedNode, err := FollowStructuralPath(reparsed, path)
		if err != nil {
			return nil, fmt.Errorf("follow structural path for node %s: %w", id, err)
		}
		originalKind, originalOK := generated.NodeKind(original)
		reparsedKind, reparsedOK := generated.NodeKind(reparsedNode)
		if !originalOK || !reparsedOK || originalKind != reparsedKind {
			return nil, fmt.Errorf("formatted structural path for node %s changed kind go/ast.%s -> go/ast.%s",
				id, originalKind, reparsedKind)
		}
		if !reparsedNode.Pos().IsValid() || !reparsedNode.End().IsValid() {
			continue
		}
		file := reparsedSet.File(reparsedNode.Pos())
		endFile := reparsedSet.File(reparsedNode.End())
		if file == nil || endFile != file {
			return nil, fmt.Errorf("reparsed node %s has positions outside one generated file", id)
		}
		entry := SourceMapEntry{
			Generated: GeneratedRange{
				File:      generatedFilename,
				StartByte: file.Offset(reparsedNode.Pos()),
				EndByte:   file.Offset(reparsedNode.End()),
			},
			Node: id,
		}
		if wire := wireNodes[id]; wire != nil {
			entry.Origin = wire.Origin
		}
		entries = append(entries, entry)
	}
	sort.Slice(entries, func(i, j int) bool {
		if entries[i].Generated.StartByte != entries[j].Generated.StartByte {
			return entries[i].Generated.StartByte < entries[j].Generated.StartByte
		}
		if entries[i].Generated.EndByte != entries[j].Generated.EndByte {
			return entries[i].Generated.EndByte < entries[j].Generated.EndByte
		}
		return entries[i].Node < entries[j].Node
	})
	return entries, nil
}
