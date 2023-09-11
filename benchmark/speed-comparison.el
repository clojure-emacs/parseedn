;;; speed-comparison.el --- benchmark              -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2022  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; URL: http://www.github.com/clojure-emacs/parseedn

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

;; takes a file containing edn file names, one per line
;;
;;    locate *.edn > edn.list
;;
;; results end up as edn in *edn-parse-time-results*

;;; Code:

(require 'parseedn)

(with-current-buffer (find-file-noselect "edn.list")
  (goto-char 1)
  (while (and (< (point) (point-max)))
    (end-of-line)
    (let* ((fn (buffer-substring-no-properties (line-beginning-position) (point)))
           (buff (find-file-noselect fn))
           (edn-time 0)
           (clj-time 0))
      ;;(message fn)
      (with-current-buffer buff
        (let ((start (time-to-seconds (current-time))))
          (parseedn-read)
          (setq clj-time (+ clj-time (- (time-to-seconds (current-time)) start))))
        (goto-char 1)
        (let ((start (time-to-seconds (current-time))))
          (parseedn-read)
          (setq edn-time (+ edn-time (- (time-to-seconds (current-time)) start)))))
      (kill-buffer buff)
      (when (< (point) (point-max)) (right-char))
      (with-current-buffer "*edn-parse-time-results*"
        (insert "{:file \"" fn "\", :edn-time " (number-to-string edn-time) ", :clj-time " (number-to-string clj-time) "}\n")))))

(provide 'speed-comparison)
;;; speed-comparison.el ends here
