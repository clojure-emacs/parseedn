;;; investigate-helpers.el --- helper microbenchmarks for parseedn -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Investigation helpers for comparing likely allocation-heavy helper patterns.
;; Intended for manual use during task 002 and similar investigations.

;;; Code:

(require 'package)
(package-initialize)

(require 'parseedn)

(defun parseedn-investigate--sample-children ()
  "Return a representative child sequence for helper microbenchmarks."
  '(:k1 1 :k2 2 :k3 3 :k4 4 :k5 5 :k6 6 :k7 7 :k8 8))

(defun parseedn-investigate--sample-kvs ()
  "Return sample key/value pairs for map helper microbenchmarks."
  '((:k1 1) (:k2 2) (:k3 3) (:k4 4) (:k5 5) (:k6 6) (:k7 7) (:k8 8)))

(defun parseedn-investigate-benchmark-vector-build (iterations)
  "Compare vector building approaches over ITERATIONS.
Returns an alist of elapsed seconds."
  (let ((children (parseedn-investigate--sample-children))
        (sink nil)
        (apply-seconds 0.0)
        (vconcat-seconds 0.0))
    (let ((start (current-time)))
      (dotimes (_ iterations)
        (setq sink (apply #'vector children)))
      (setq apply-seconds (float-time (time-subtract (current-time) start))))
    (let ((start (current-time)))
      (dotimes (_ iterations)
        (setq sink (vconcat children)))
      (setq vconcat-seconds (float-time (time-subtract (current-time) start))))
    (ignore sink)
    `((apply-vector . ,apply-seconds)
      (vconcat . ,vconcat-seconds))))

(defun parseedn-investigate-benchmark-map-build (iterations)
  "Exercise current map helper shapes over ITERATIONS.
Returns an alist of elapsed seconds."
  (let ((kvs (parseedn-investigate--sample-kvs))
        (sink nil)
        (plain-seconds 0.0))
    (let ((start (current-time)))
      (dotimes (_ iterations)
        (setq sink (parseedn--build-non-prefixed-map kvs)))
      (setq plain-seconds (float-time (time-subtract (current-time) start))))
    (ignore sink)
    `((build-non-prefixed-map . ,plain-seconds))))

(provide 'investigate-helpers)
;;; investigate-helpers.el ends here
