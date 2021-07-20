# pyre-ast

`pyre-ast` is an OCaml library to parse Python files.

The library features its full-fidelity to the official Python spec. Apart from a few technical edge cases, as long as a given file can be parsed by the CPython interpreter, `pyre-ast` will be able to parse the file without any problem. Furthermore, abstract syntax trees obtained from `pyre-ast` is guaranteed to 100% match the results obtained by Python's own [ast.parse](https://docs.python.org/3/library/ast.html#ast.parse) API, down to every AST node and every line and column number. 

Another notable feature of this library is that it represents the Python syntax using the [tagless-final style](http://okmij.org/ftp/tagless-final/JFP.pdf). This style typically offers more flexibility and extensibility for the downstream consumers of the syntax, and allow them to build up their analysis without explicitly constructing a syntax tree. On the other hand, this library does offer a tranditional "concrete" syntax tree structure as well, for developers who are less familiar with the tagless-final approach and more familiar with standard algebraic data type representation. 

## Installation

It is recommended to use [opam](https://opam.ocaml.org) for package management. To install `pyre-ast` with `opam`, you can run:
```
opam install pyre-ast
```

## Usage

It is recommended to use [dune](https://dune.readthedocs.io/en/stable/) as your build system. To use `pyre-ast` in your `dune` project, you can add `pyre-ast` to the `libraries` stanza in your `dune` file. For example,

```
(library
  (name mylib)
  (libraries pyre-ast))
```

Documentation of this library can be found [here](https://grievejia.github.io/pyre-ast/doc/pyre-ast/).

## Development

It is recommended to use a [local switch](https://opam.ocaml.org/blog/opam-local-switches/) for development:

```
$ git clone https://github.com/grievejia/pyre-ast.git
$ cd pyre-ast
$ opam switch create ./ 4.12.0
$ opam install . --deps-only
$ dune build  # Build the library
$ dune test   # Run tests
```
