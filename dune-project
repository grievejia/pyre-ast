(lang dune 2.8)
(name pyre-ast)
(version 0.1.2)
(cram enable)

(generate_opam_files true)

(source (github grievejia/pyre-ast))
(documentation "https://grievejia.github.io/pyre-ast/doc")
(license MIT)
(authors "Jia Chen")
(maintainers "grievejia@gmail.com")

(package
 (name pyre-ast)
 (synopsis "Full-fidelity Python parser in OCaml")
 (description "pyre-ast is an OCaml library to parse Python source files into abstract syntax trees. Under the hood, it relies on the CPython parser to do the parsing work and therefore the result is always 100% compatible with the official CPython implementation.")
 (depends
  (base (>= v0.14.1))
  (ppx_sexp_conv (>= v0.14.0))
  (ppx_compare (>= v0.14.0))
  (ppx_hash (>= v0.14.0))
  (ppx_deriving (>= 5.2.1))
  (ppx_make (>= 0.2.1))
  (stdio (and :with-test (>= v0.14.0)))
  (sexplib (and :with-test (>= v0.14.0)))
  (cmdliner (and :with-test (>= 1.0.0)))
))
