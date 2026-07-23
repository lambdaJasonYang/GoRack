# Gorack

<p align="center">
  <img src="assets/gorack-mascot.webp" alt="Gorack mascot" width="280">
</p>

Gorack lets you write Go programs with Lisp-style syntax and work with Go source as structured JSON.

## Install

Choose the branch or release tag matching your Go version:

```bash
git clone --branch go1.26.5 https://github.com/lambdaJasonYang/GoRack.git
cd GoRack
```

Requirements:

- The Go version recorded in `MANIFEST.json`
- Racket 8.x
- GNU Make

Build the bridge:

```bash
make bridge
```

## Quick start

Create `hello.rkt`:

```racket
#lang gorack

(package main
  (import "fmt")

  (defn main
    (-> () ())
    (:= name "Gorack")
    (fmt.Println "Hello from" name)))
```

Generate and run Go:

```bash
PLTCOLLECTS="$PWD:" racket hello.rkt > hello.wire.json

go-bridge/bin/gorack-go-bridge decode \
  -in hello.wire.json \
  -out hello.go

go run hello.go
```

Output:

```text
Hello from Gorack
```

Install the Racket language as a linked package to omit `PLTCOLLECTS`:

```bash
raco pkg install --auto --link ./gorack
```

## Functions and calls

Functions use a headed signature form:

```racket
(defn add
  (-> ([x int] [y int]) (int))
  (return (+ x y)))
```

The first signature list contains parameters and the second contains results. Use square brackets for named parameter and result bindings. Empty lists mean no parameters or no results:

```racket
(defn main
  (-> () ())
  ...)
```

Named results are supported:

```racket
(defn divide
  (-> ([x float64] [y float64])
      ([result float64] [err error]))
  ...)
```

Call functions directly with the callee at the head:

```racket
(add 20 22)
(fmt.Println "total:" total)
(strings.ToUpper name)
```

Dotted selectors remain intact:

```racket
request.URL.Path
service.Handler
(service.Handler request)
```

Function literals use the same signature syntax:

```racket
(fn
  (-> ([x int]) (int))
  (return (* x x)))
```

## Variables and assignment

```racket
(:= count 0)
(= count 10)
(+= count 1)

(:= [x y] [10 20])

(var name string)
(var= [language string "Gorack"])
(const= Answer 42)
```

Supported compound assignment heads include `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `bit-or=`, `^=`, `<<=`, `>>=`, and `&^=`.

## Expressions

```racket
(+ x y)
(- x y)
(* x y)
(/ x y)
(% x y)

(== x y)
(!= x y)
(< x y)
(<= x y)
(> x y)
(>= x y)

(and ready valid)
(or cached available)
(! failed)
```

Unary and binary forms share their Go operator where possible:

```racket
(- value)        ; -value
(* pointer)      ; *pointer
(& value)        ; &value

(* left right)   ; left * right
(& left right)   ; left & right
```

Additional bitwise forms:

```racket
(bit-or a b)
(xor a b)
(and-not a b)
(<< value bits)
(>> value bits)
```

## Conditions

```racket
(if (> total 10)
  (fmt.Println "large")
  (fmt.Println "small"))
```

Use `begin` for multi-statement branches:

```racket
(if ready
  (begin
    (fmt.Println "starting")
    (start))
  (begin
    (fmt.Println "waiting")
    (wait)))
```

Go-style initialization is expressed with a headed `init` clause:

```racket
(if (init (:= value (readValue)))
    (> value 0)
  (fmt.Println value))
```

## Loops

Condition loop:

```racket
(for running
  (work))
```

Three-part loop:

```racket
(for (:= i 0) (< i 10) (+= i 1)
  (fmt.Println i))
```

For arbitrary init or post expressions, use explicit headed clauses:

```racket
(for (init (prepare)) ready (post (advance))
  (work))
```

Infinite loop:

```racket
(for (forever)
  (work))
```

Range loop:

```racket
(for-range (:= [index value] values)
  (fmt.Println index value))
```

Assign to existing variables with `=`:

```racket
(for-range (= [index value] values)
  ...)
```

Ignore both range bindings:

```racket
(for-range values
  ...)
```

## Switches

```racket
(switch value
  (case 1
    (fmt.Println "one"))
  (case [2 3]
    (fmt.Println "two or three"))
  (default
    (fmt.Println "other")))
```

Switch with initialization:

```racket
(switch (init (:= value (readValue))) value
  (case 0 ...)
  (default ...))
```

Type switch:

```racket
(type-switch (:= value input)
  (case int
    (fmt.Println "int" value))
  (case [string bool]
    (fmt.Println "string or bool"))
  (default
    (fmt.Println "other")))
```

Without a binding:

```racket
(type-switch input
  (case int ...)
  (default ...))
```

## Channels and select

Channel types:

```racket
(chan int)
(chan-send int)
(chan-recv int)
```

Send and receive:

```racket
(send output value)
(<- input)
(:= value (<- input))
(:= [value ok] (<- input))
```

Select clauses use the same headed operations:

```racket
(select
  (case (send output value)
    (fmt.Println "sent"))

  (case (<- input)
    (fmt.Println "received"))

  (case (:= value (<- input))
    (fmt.Println value))

  (case (:= [value ok] (<- input))
    (fmt.Println value ok))

  (default
    (fmt.Println "not ready")))
```

## Structs and composite literals

```racket
(type Person
  (struct
    (Name string)
    (Age int)
    (Note string (tag (json note #:omitempty)))))
```

```racket
(:= person
  (composite Person
    (kv Name "Ada")
    (kv Age 37)))
```

## Types and interfaces

```racket
(type UserID int)
(type-alias Reader io.Reader)

(ptr Person)
(array 10 int)
(slice string)
(map string int)
```

Interface methods use the same signature syntax:

```racket
(type Reader
  (interface
    (method Read
      (-> ([buffer (slice byte)]) (int error)))
    (embed io.Closer)))
```

## Methods

```racket
(defn (p (ptr Person)) Rename
  (-> ([name string]) ())
  (= p.Name name))
```

## Statements

```racket
(return value)
(block statement ...)
(go (serve listener))
(defer (file.Close))
(send channel value)
(inc index)
(dec index)
(label retry statement)
(break)
(break outer)
(continue)
(continue outer)
(goto retry)
(fallthrough)
```

## Convert Go to JSON

```bash
go-bridge/bin/gorack-go-bridge encode \
  -in input.go \
  -out input.wire.json
```

## Convert JSON to Go

```bash
go-bridge/bin/gorack-go-bridge decode \
  -in input.wire.json \
  -out output.go
```

Generate a source map while decoding:

```bash
go-bridge/bin/gorack-go-bridge decode \
  -in input.wire.json \
  -out output.go \
  -source-map output.map.json
```

Display the schema included with the release:

```bash
go-bridge/bin/gorack-go-bridge schema
```

## Work with the syntax tree from Racket

```racket
#lang racket

(require gorack/go-kernel/wire)

(define unit
  (read-go-ast-json-file "input.wire.json"))

(write-go-ast-json-file
  "output.wire.json"
  unit)
```

## Commands

```bash
make bridge
make test
make go-test
make racket-test
make clean
```

`MANIFEST.json` records the Gorack version, exact Go version, schema hash, and source commit for the distribution.
