package main

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"go/parser"
	"go/token"
	"io"
	"os"
	"path/filepath"

	"github.com/gorack/gorack/go-bridge/internal/codec"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, "gorack-go-bridge:", err)
		os.Exit(1)
	}
}

func run(args []string) error {
	if len(args) == 0 {
		return usageError()
	}
	switch args[0] {
	case "encode":
		return runEncode(args[1:])
	case "decode":
		return runDecode(args[1:])
	case "schema":
		return runSchema(args[1:])
	case "help", "-h", "--help":
		printUsage(os.Stdout)
		return nil
	default:
		return fmt.Errorf("unknown command %q\n\n%s", args[0], usageText())
	}
}

func runEncode(args []string) error {
	flags := flag.NewFlagSet("encode", flag.ContinueOnError)
	flags.SetOutput(io.Discard)
	in := flags.String("in", "", "input Go source file")
	out := flags.String("out", "", "output Gorack wire JSON file, or - for stdout")
	if err := flags.Parse(args); err != nil {
		return err
	}
	if *in == "" || *out == "" || flags.NArg() != 0 {
		return errors.New("encode requires -in FILE.go and -out FILE.wire.json")
	}

	source, err := os.ReadFile(*in)
	if err != nil {
		return fmt.Errorf("read %s: %w", *in, err)
	}
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, *in, source,
		parser.ParseComments|parser.SkipObjectResolution)
	if err != nil {
		return fmt.Errorf("parse %s: %w", *in, err)
	}
	unit, err := codec.Encode(file, fset, codec.EncodeOptions{})
	if err != nil {
		return err
	}
	hash := sha256.Sum256(source)
	for index := range unit.Sources {
		if unit.Sources[index].Name == *in {
			unit.Sources[index].ContentHash = "sha256:" + hex.EncodeToString(hash[:])
		}
	}
	return writeJSON(*out, unit)
}

func runDecode(args []string) error {
	flags := flag.NewFlagSet("decode", flag.ContinueOnError)
	flags.SetOutput(io.Discard)
	in := flags.String("in", "", "input Gorack wire JSON file")
	out := flags.String("out", "", "output formatted Go source file, or - for stdout")
	sourceMap := flags.String("source-map", "", "optional generated source-map JSON file")
	allowMismatch := flags.Bool("allow-schema-mismatch", false, "attempt decoding despite a differing schema hash")
	if err := flags.Parse(args); err != nil {
		return err
	}
	if *in == "" || *out == "" || flags.NArg() != 0 {
		return errors.New("decode requires -in FILE.wire.json and -out FILE.go")
	}

	var unit codec.Unit
	if err := readJSON(*in, &unit); err != nil {
		return err
	}
	decoded, err := codec.Decode(&unit, codec.DecodeOptions{AllowSchemaMismatch: *allowMismatch})
	if err != nil {
		return err
	}
	generatedName := *out
	if generatedName == "-" {
		generatedName = "generated.go"
	}
	formatted, err := codec.FormatFile(decoded)
	if err != nil {
		return err
	}
	var entries []codec.SourceMapEntry
	if *sourceMap != "" {
		entries, err = codec.SourceMapForFormatted(decoded, generatedName, formatted)
		if err != nil {
			return err
		}
		document := codec.SourceMapDocument{
			Format:        "gorack-source-map",
			FormatVersion: 1,
			Entries:       entries,
		}
		if err := writeJSON(*sourceMap, document); err != nil {
			return err
		}
	}
	return writeBytes(*out, formatted)
}

func runSchema(args []string) error {
	flags := flag.NewFlagSet("schema", flag.ContinueOnError)
	flags.SetOutput(io.Discard)
	if err := flags.Parse(args); err != nil {
		return err
	}
	if flags.NArg() != 0 {
		return errors.New("schema takes no arguments")
	}
	return writeJSON("-", codec.ActiveSchemaIdentity())
}

func readJSON(path string, target any) error {
	var input io.ReadCloser
	if path == "-" {
		input = io.NopCloser(os.Stdin)
	} else {
		file, err := os.Open(path)
		if err != nil {
			return fmt.Errorf("open %s: %w", path, err)
		}
		input = file
	}
	defer input.Close()
	decoder := json.NewDecoder(input)
	decoder.DisallowUnknownFields()
	if err := decoder.Decode(target); err != nil {
		return fmt.Errorf("decode JSON %s: %w", path, err)
	}
	var trailing any
	if err := decoder.Decode(&trailing); !errors.Is(err, io.EOF) {
		if err == nil {
			return fmt.Errorf("decode JSON %s: trailing JSON value", path)
		}
		return fmt.Errorf("decode JSON %s: trailing data: %w", path, err)
	}
	return nil
}

func writeJSON(path string, value any) error {
	var output io.WriteCloser
	if path == "-" {
		output = nopWriteCloser{os.Stdout}
	} else {
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil && filepath.Dir(path) != "." {
			return fmt.Errorf("create output directory: %w", err)
		}
		file, err := os.Create(path)
		if err != nil {
			return fmt.Errorf("create %s: %w", path, err)
		}
		output = file
	}
	defer output.Close()
	encoder := json.NewEncoder(output)
	encoder.SetIndent("", "  ")
	encoder.SetEscapeHTML(false)
	if err := encoder.Encode(value); err != nil {
		return fmt.Errorf("write JSON %s: %w", path, err)
	}
	return nil
}

func writeBytes(path string, data []byte) error {
	if path == "-" {
		_, err := os.Stdout.Write(data)
		return err
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil && filepath.Dir(path) != "." {
		return fmt.Errorf("create output directory: %w", err)
	}
	if err := os.WriteFile(path, data, 0o644); err != nil {
		return fmt.Errorf("write %s: %w", path, err)
	}
	return nil
}

type nopWriteCloser struct{ io.Writer }

func (nopWriteCloser) Close() error { return nil }

func usageError() error {
	return errors.New(usageText())
}

func printUsage(output io.Writer) {
	fmt.Fprint(output, usageText())
}

func usageText() string {
	return `usage:
  gorack-go-bridge encode -in FILE.go -out FILE.wire.json
  gorack-go-bridge decode -in FILE.wire.json -out FILE.generated.go [-source-map FILE.map.json]
  gorack-go-bridge schema
`
}
