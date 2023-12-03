# 1.2.1 (2023-12-03)

- Update parseclj to [1.1.1](https://github.com/clojure-emacs/parseclj/blob/v1.1.1/CHANGELOG.md#111-2022-02-07).

# 1.2.0 (2023-09-29)

- [#12](https://github.com/clojure-emacs/parseedn/issues/12): Allow empty vectors to be printed.
- [#14](https://github.com/clojure-emacs/parseedn/pull/14): avoid C stack overflows.
- [#15](https://github.com/clojure-emacs/parseedn/pull/15): correctly process empty hashmaps.
- [#17](https://github.com/clojure-emacs/parseedn/pull/17): Add support for namespaced maps.

# 1.1.0 (2022-02-07)

- [#11](https://github.com/clojure-emacs/parseedn/pull/11) Support a default-data-reader for tagged literals
- Via parseclj: support bigint

# 1.0.6 (2021-10-13)

- Make sure parseedn and parseclj versions are in sync

# 1.0.5 (2021-10-13)

- [#10](https://github.com/clojure-emacs/parseedn/pull/10) Replace `cl-case` with `cond`

# 1.0.4 (2021-09-30)

- Drop use of (map-merge 'alist) for older Emacsen

# 1.0.3 (2021-09-29)

- Follow upstream parseclj versioning

# 1.0.2 (2021-09-29)

- Follow upstream parseclj versioning

# 1.0.0 (2021-09-27)

## Added

- Added print handler for `#uuid` and `#inst`

# 0.2.0 (2020-09-09 / d25ebc5)

## Added

- Add support for alist, plist and an explicit error for unsupported dotted pair notation.

## Fixed

- Fix printing of hash-maps with multiple entries.

# 0.1.0 (2019-03-31 / ddf824b)

Initial release of parseedn
