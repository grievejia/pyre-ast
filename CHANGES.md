## unreleased

- Bump the bundled CPython version to 3.10.0

## 0.1.3 (2021-7-22)

- Fix crashes when parsing non-UTF8-decodable string literals.
- \N escape sequence in string literals will no longer cause a syntax error.
- Module parsing APIs now accept an additional `enable_type_comment` argument, controlling whether to parse type comments or not.

## 0.1.2 (2021-07-20)

- Fix linking flags and tests on Linux.

## 0.1.1 (2021-07-20)

- Column number of default parsing error starts from 0 instead of 1.
- Correctly set up test dependency with `stdio`.
- Make C bindings compatible with pre-OCaml-4.11 by always defining `Val_none`.

## 0.1.0 (2021-07-20)

Initial release.

Import code from CPython 3.10.0b3.
