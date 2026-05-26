;;; fixture-subset-benchmark.el --- targeted fixture subset benchmark -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Helper for task-oriented investigation of parseedn benchmark fixtures.
;; This extends the existing GC benchmark with fixture selection so heavy
;; workload shapes can be isolated without changing the canonical benchmark.

;;; Code:

(require 'package)
(package-initialize)

(require 'gc-benchmark
         (expand-file-name "gc-benchmark"
                           (file-name-directory (or load-file-name buffer-file-name default-directory))))
(require 'subr-x)

(defun parseedn-benchmark--selected-fixtures (fixtures)
  "Return FIXTURES filtered by PARSEEDN_BENCHMARK_FIXTURES, or all FIXTURES.
The environment variable is a comma-separated list of fixture file names."
  (if-let ((value (getenv "PARSEEDN_BENCHMARK_FIXTURES")))
      (let ((wanted (split-string value "," t "[[:space:]]*")))
        (seq-filter (lambda (fixture)
                      (member (plist-get fixture :name) wanted))
                    fixtures))
    fixtures))

(defun parseedn-benchmark-run-selected-fixtures-batch ()
  "Run the parseedn GC benchmark for selected fixtures and emit EDN output."
  (let* ((iterations (parseedn-benchmark--env-int "PARSEEDN_BENCHMARK_ITERATIONS" 200))
         (gc-threshold (parseedn-benchmark--env-int "PARSEEDN_BENCHMARK_GC_THRESHOLD" 80000))
         (fixtures (parseedn-benchmark--selected-fixtures
                    (parseedn-benchmark--fixtures))))
    (parseedn-benchmark--warmup fixtures)
    (parseedn-benchmark--write-result
     (parseedn-benchmark--result
      fixtures
      (parseedn-benchmark--measure fixtures iterations gc-threshold)))))

(provide 'fixture-subset-benchmark)
;;; fixture-subset-benchmark.el ends here
