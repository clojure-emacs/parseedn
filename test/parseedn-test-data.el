;;; parseedn-test-data.el --- Clojure/EDN parser - test data

;; Copyright (C) 2017-2021  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Test data for reader / parser / printer / unparser

;;; Code:

(require 'a)

(setq parseedn-test-data
      (a-list

       "simple-list"
       (a-list
        :tags '(:edn-roundtrip)
        :source "(1 2 3)"
        :edn '((1 2 3)))


       "empty-list"
       (a-list
        :source "()"
        :edn '(()))

       "size-1"
       (a-list
        :tags '(:edn-roundtrip)
        :source "(1)"
        :edn '((1)))

       "leafs"
       (a-list
        :source "(nil true false hello-world)"
        :edn '((nil t nil hello-world)))

       "qualified-symbol"
       (a-list
        :tags '(:edn-roundtrip)
        :source "clojure.string/join"
        :edn '(clojure.string/join))

       "nested-lists"
       (a-list
        :source "((.9 abc (true) (hello)))"
        :edn '(((0.9 abc (t) (hello)))))

       "strings-1"
       (a-list
        :tags '(:edn-roundtrip)
        :source "\"abc hello \\t\\\"x\""
        :edn '("abc hello \t\"x"))

       "strings-2"
       (a-list
        :source "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")"
        :edn '(("---\f---\"-''-\\-\r\n")))

       "chars-1"
       (a-list
        :source "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)"
        :edn '((?\n ?\r ?\ ?\t ?a ?b ?c ?x ?y)))

       "chars-2"
       (a-list
        :source "\"\\u0078 \\o171\""
        :edn '("x y"))

       "keywords"
       (a-list
        :tags '(:edn-roundtrip)
        :source ":foo-bar"
        :edn '(:foo-bar))

       "vector"
       (a-list
        :tags '(:edn-roundtrip)
        :source "[123]"
        :edn '([123]))

       "map"
       (a-list
        :tags '(:edn-roundtrip)
        :source "{:count 123}"
        :edn (list (a-hash-table :count 123)))

       "prefixed-map-1"
       (a-list
        :source "#:foo.bar{:baz 1 :other.ns.prefix/qux 2}"
        :edn (list (a-hash-table :foo.bar/baz 1 :other.ns.prefix/qux 2)))

       "prefixed-map-2"
       (a-list
        :source "#:foo.bar {:baz 1}"
        :edn (list (a-hash-table :foo.bar/baz 1)))

       "set"
       (a-list
        :tags '(:edn-roundtrip)
        :source "#{:x}"
        :edn '((edn-set (:x))))

       "discard"
       (a-list
        :source "(10 #_11 12 #_#_ 13 14)"
        :edn '((10 12)))


       "tag-1"
       (a-list
        :tags '(:edn-roundtrip)
        :tag-readers '((:default . parseedn-tagged-literal))
        :source "#foo/bar [1]"
        :edn '((edn-tagged-literal foo/bar [1])))

       "tag-2"
       (a-list
        :tags '(:edn-roundtrip)
        :tag-readers '((:default . parseedn-tagged-literal))
        :source "(fn #param :param-name 1)"
        :edn '((fn (edn-tagged-literal param :param-name) 1)))

       "nested-tags"
       (a-list
        :tags '(:edn-roundtrip)
        :tag-readers '((:default . parseedn-tagged-literal))
        :edn (list (vector `(edn-tagged-literal lazy-error (edn-tagged-literal error ,(a-hash-table :cause "Divide by zero")))))
        :source "[#lazy-error #error {:cause \"Divide by zero\"}]")

       "booleans"
       (a-list
        :source "[nil true false]"
        :edn '([nil t nil]))

       "uuid"
       (a-list
        :source "#uuid \"c9c4eac2-7b23-4d62-a444-41a72dc09039\""
        :edn '((edn-uuid "c9c4eac2-7b23-4d62-a444-41a72dc09039")))

       "inst"
       (a-list
        :source "#inst \"2020-09-09T06:56:04\""
        ;; FIXME this value may differ depending on the timezone of your machine
        :edn '((edn-inst 24408 31876)))))

;;; parseedn-test-data.el ends here
