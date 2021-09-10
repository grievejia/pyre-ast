The `vendor` directory contains a patched version of CPython's source code (release `3.10.0rc2`). Below are the list of applied patches:

- \N escape sequence handling logic is removed from `Objects/unicodeobject.c`. \N handling requires loading the `unicodedata` extension module, yet extensions modules can never work in a minimally-initialized Python runtime that `pyre-ast` uses.
