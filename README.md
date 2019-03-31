[![Build Status](https://travis-ci.org/clojure-emacs/parseedn.svg?branch=master)](https://travis-ci.org/clojure-emacs/parseedn)

# EDN parser for Emacs Lisp

`parseedn` is an Emacs Lisp library for parsing [EDN
data](https://github.com/edn-format/edn). It uses
[`parseclj`](https://github.com/clojure-emacs/parseclj)'s shift-reduce parser
internally.

EDN and Emacs Lisp have some important differences that make translation from
one to the other not transparent (think representing an EDN map into Elisp, or
being able to differentiate between `false` and `nil` in Elisp).  Because of
this, `parseedn` takes certain decisions when parsing and transforming EDN data
into Elisp data types.  For more information please refer to [`parseclj`
DESIGN.md](https://github.com/clojure-emacs/parseclj/blob/master/DESIGN.md)
document.

Lastly, `parseedn` is in **alpha** stage, so its API is subject to change.

## Installation

Currently `parseend` is not part of [MELPA](http://melpa.milkbox.net/), so the
best way to install it is by getting your own copy of `parseedn`, *and* its
`parseclj` dependency, and putting it somewhere in your Emacs
[`load-path`](https://www.emacswiki.org/emacs/LoadPath).

You can just copy-paste this code into your Emacs init file:

```emacs-lisp
(add-to-list 'load-path "/path/to/your/copy/of/parseclj/")
(add-to-list 'load-path "/path/to/your/copy/of/parseedn/")
```

## Usage

- `parseedn-read`

    Read content from the current buffer as EDN and transforms it into an Emacs
    Lisp value.

- `parseedn-read-str` str

    Read STR as EDN and transfroms it into an Emacs Lisp value.

- `parseedn-print` datum

    Inserts DATUM as EDN Into the current buffer.  DATUM can be any Emacs Lisp
    value.

- `parseedn-print-str` datum

    Returns a string containing DATUM as EDN.  DATUM can be any Emacs Lisp
    value.

## Prior art

[edn.el](https://github.com/expez/edn.el) is an EDN-to-elisp parser based on the
PEG parser generator library.

## License

&copy; 2017-2019 Arne Brasseur

Distributed under the terms of the GNU General Public License 3.0 or later. See
[LICENSE](LICENSE).
