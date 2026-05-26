;;; gc-benchmark.el --- manual GC benchmark for parseedn -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: AI coding agent
;; URL: http://www.github.com/clojure-emacs/parseedn

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Batch-runnable benchmark for measuring parseedn allocation pressure via
;; GC count deltas under a fixed low `gc-cons-threshold'.
;;
;; Canonical usage from the repository root:
;;
;;   Emacs -Q --batch -L . -l benchmark/gc-benchmark.el \
;;     -f parseedn-benchmark-run-batch
;;
;; Optional environment variables:
;;
;;   PARSEEDN_BENCHMARK_ITERATIONS    Number of measured iterations.
;;                                    Default: 200
;;   PARSEEDN_BENCHMARK_GC_THRESHOLD  `gc-cons-threshold' during measurement.
;;                                    Default: 80000
;;   PARSEEDN_BENCHMARK_OUTPUT        Output file path.  Default: stdout
;;
;; The output is EDN and includes environment metadata, aggregate totals, and
;; per-fixture timing/size rows.

;;; Code:

(require 'package)
(package-initialize)

(require 'parseedn)
(require 'subr-x)

(defconst parseedn-benchmark--root-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Repository root inferred from the benchmark file location.")

(defconst parseedn-benchmark--fixtures-directory
  (expand-file-name "fixtures" parseedn-benchmark--root-directory)
  "Directory containing canonical benchmark fixtures.")

(defun parseedn-benchmark--env-int (name default)
  "Read integer environment variable NAME, or DEFAULT when unset."
  (if-let ((value (getenv name)))
      (string-to-number value)
    default))

(defun parseedn-benchmark--fixture-paths ()
  "Return sorted fixture paths for the benchmark corpus."
  (sort (directory-files parseedn-benchmark--fixtures-directory t "\\.edn\\'")
        #'string<))

(defun parseedn-benchmark--read-fixture (path)
  "Return fixture data for PATH as a plist."
  (with-temp-buffer
    (insert-file-contents path)
    (list :path path
          :name (file-name-nondirectory path)
          :bytes (buffer-size)
          :content (buffer-substring-no-properties (point-min) (point-max)))))

(defun parseedn-benchmark--fixtures ()
  "Return the canonical benchmark fixtures."
  (mapcar #'parseedn-benchmark--read-fixture
          (parseedn-benchmark--fixture-paths)))

(defun parseedn-benchmark--parse-string (string)
  "Parse STRING as EDN and return the resulting value."
  (parseedn-read-str string))

(defun parseedn-benchmark--warmup (fixtures)
  "Run one warmup parse pass over FIXTURES."
  (dolist (fixture fixtures)
    (parseedn-benchmark--parse-string (plist-get fixture :content))))

(defun parseedn-benchmark--measure-fixture (fixture iterations)
  "Measure FIXTURE over ITERATIONS and return a result plist."
  (let* ((content (plist-get fixture :content))
         (name (plist-get fixture :name))
         (bytes (plist-get fixture :bytes))
         (start-time (current-time)))
    (dotimes (_ iterations)
      (parseedn-benchmark--parse-string content))
    (list :name name
          :bytes bytes
          :iterations iterations
          :parses iterations
          :elapsed-seconds (float-time (time-subtract (current-time) start-time)))))

(defun parseedn-benchmark--measure (fixtures iterations gc-threshold)
  "Measure FIXTURES over ITERATIONS with GC-THRESHOLD.
Returns a plist containing aggregate and per-fixture results."
  (let ((gc-cons-threshold gc-threshold)
        (gc-cons-percentage 0.1)
        (start-gcs 0)
        (start-gc-elapsed (if (boundp 'gc-elapsed) gc-elapsed 0.0))
        (start-time nil)
        (rows nil))
    (garbage-collect)
    (setq start-gcs gcs-done)
    (setq start-time (current-time))
    (dolist (fixture fixtures)
      (push (parseedn-benchmark--measure-fixture fixture iterations) rows))
    (let* ((elapsed-seconds (float-time (time-subtract (current-time) start-time)))
           (gc-count-delta (- gcs-done start-gcs))
           (gc-elapsed-delta (and (boundp 'gc-elapsed)
                                  (- gc-elapsed start-gc-elapsed)))
           (rows (nreverse rows))
           (fixture-count (length rows))
           (bytes-total (apply #'+ (mapcar (lambda (row) (plist-get row :bytes)) rows)))
           (parse-count-total (apply #'+ (mapcar (lambda (row) (plist-get row :parses)) rows)))
           (fixture-seconds-total (apply #'+ (mapcar (lambda (row) (plist-get row :elapsed-seconds)) rows))))
      (list :iterations iterations
            :gc-threshold gc-threshold
            :fixture-count fixture-count
            :input-bytes-total bytes-total
            :parse-count-total parse-count-total
            :elapsed-seconds elapsed-seconds
            :fixture-elapsed-seconds-total fixture-seconds-total
            :gc-count-delta gc-count-delta
            :gc-elapsed-seconds gc-elapsed-delta
            :fixtures rows))))

(defun parseedn-benchmark--result (fixtures measurement)
  "Build the EDN result map from FIXTURES and MEASUREMENT."
  (let ((fixture-names (mapcar (lambda (fixture) (plist-get fixture :name)) fixtures)))
    `((:benchmark . "parseedn-gc")
      (:version . 1)
      (:emacs-version . ,emacs-version)
      (:system-type . ,(symbol-name system-type))
      (:timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z" (current-time)))
      (:default-directory . ,default-directory)
      (:fixture-names . ,fixture-names)
      (:measurement . ((:iterations . ,(plist-get measurement :iterations))
                       (:gc-threshold . ,(plist-get measurement :gc-threshold))
                       (:fixture-count . ,(plist-get measurement :fixture-count))
                       (:input-bytes-total . ,(plist-get measurement :input-bytes-total))
                       (:parse-count-total . ,(plist-get measurement :parse-count-total))
                       (:elapsed-seconds . ,(plist-get measurement :elapsed-seconds))
                       (:fixture-elapsed-seconds-total . ,(plist-get measurement :fixture-elapsed-seconds-total))
                       (:gc-count-delta . ,(plist-get measurement :gc-count-delta))
                       (:gc-elapsed-seconds . ,(or (plist-get measurement :gc-elapsed-seconds) 0.0))))
      (:fixtures . ,(mapcar (lambda (row)
                              `((:name . ,(plist-get row :name))
                                (:bytes . ,(plist-get row :bytes))
                                (:iterations . ,(plist-get row :iterations))
                                (:parses . ,(plist-get row :parses))
                                (:elapsed-seconds . ,(plist-get row :elapsed-seconds))))
                            (plist-get measurement :fixtures))))))

(defun parseedn-benchmark--write-result (result)
  "Write RESULT as EDN to stdout or to the configured output path."
  (let ((output (getenv "PARSEEDN_BENCHMARK_OUTPUT"))
        (text (concat (parseedn-print-str result) "\n")))
    (if (and output (not (string-empty-p output)))
        (with-temp-file output
          (insert text))
      (princ text))))

(defun parseedn-benchmark-run-batch ()
  "Run the parseedn GC benchmark and emit machine-readable EDN output."
  (let* ((iterations (parseedn-benchmark--env-int "PARSEEDN_BENCHMARK_ITERATIONS" 200))
         (gc-threshold (parseedn-benchmark--env-int "PARSEEDN_BENCHMARK_GC_THRESHOLD" 80000))
         (fixtures (parseedn-benchmark--fixtures)))
    (parseedn-benchmark--warmup fixtures)
    (parseedn-benchmark--write-result
     (parseedn-benchmark--result
      fixtures
      (parseedn-benchmark--measure fixtures iterations gc-threshold)))))

(provide 'gc-benchmark)
;;; gc-benchmark.el ends here
