# Name: Dysfunctionally Functional Language

This is a compiler frontend ie an interpreter written in OCAML language.

# Installation guidelines

You will need to install:
* opam: the package manage for OCaml. Follow instructions here: https://opam.ocaml.org/doc/Install.html
* ocaml: the language we use as host for implementing our interpreters. Follow the instructions here: https://ocaml.org/docs/install.html
* menhir: the lexer and parser generator.  Just type `opam install menhir`
* ounit: the unit testting library. Just type `opam install ounit2`
* dune: the standard build system for OCaml. Just type `opam install dune`
* Steps to execute
1. clone the repo
2. move into folder `cd Dysfunctionally_Functional` 
3. open command-line and execute `interp <your expressions in quote>`

# Steps for adding different features

for language extension:

Syntax

- Concrete Syntax: rules you have to follow to filter out incorrect code
- Abstract Syntax:

Semantics → is how we run code

- Specification
    - Results
    - Evaluation Judgements
    - Evaluation rules
- Implementation

using parse  in utop you can convert from conceret to abstract syntax


# ARITH

Grammar Notation

```
<Expression>::= <Number>
<Expression>::= <Expression> − <Expression>
<Expression>::= <Expression>/<Expression>
<Expression>::= (<Expression>)
```

## Error Monad

The infix operator (>>=) is called *bind* and is **left associative**.
Consider the expression `c >>= f`; its behavior may be described as follows:
1. evaluate the argument `c` to produce a result (i.e. a non-error value or an error value); if `c`
returns an error, propagate it and conclude.
2. otherwise, if `c` returns `Ok v`, for some expressed value `v`, then pass `v` on to `f` by evaluating
`f v`.

# Let

```
<Expression>::= <Number>
<Expression>::= <Identifier>
<Expression>::= <Expression><BOp><Expression>
<Expression>::= zero?(<Expression>)
<Expression>::= if<Expression> then <Expression> else <Expression>
<Expression>::= let <Identifer> = <Expression> in <Expression>
<Expression>::= (<Expression>)

<BOp>::= + | - | * | /
```