## unreleased

- Bump the bundled CPython version to 3.10.1
- Fixed a potential segfault issue when parsing sources with non-utf8 encoding
- [API Breaking] The default line/column numbers for parsing errors are now set to -1, to be consistent with CPython runtime behaviors

## 0.1.7 (2021-12-1)

- `IndentationError` now gets correctly recognized as syntax error. This provides us with more precise locations info for the error.

## 0.1.6 (2021-11-12)

- Expose 2 additional fields `end_lineno` and `end_offset` from CPython3.10 `SyntaxError`. Fix an error in documentation where column numbers should start from 1 instead of 0. 
- Remove the optional `filename` argument from the `parse_module` API. It turns out that this argument is actually dropped when computing error messages so it does not serve any purpose at the moment.

## 0.1.5 (2021-10-8)

- Disable LTO build of CPython in release mode. Turning on LTO turns out to be rather detrimental to link time when building downstream binaries. 

## 0.1.4 (2021-10-5)

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
