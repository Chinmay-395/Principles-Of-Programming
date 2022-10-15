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