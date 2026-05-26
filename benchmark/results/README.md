# Benchmark results

This directory may contain example baseline outputs from the manual GC-pressure
benchmark.

These results are **environment-specific**. Compare before/after runs on the
same machine and Emacs version whenever possible.

Committed reference baseline:
- `baseline-2026-05-26-emacs-30.2-darwin.edn`
- `post-map-optimization-2026-05-26-emacs-30.2-darwin.edn`
- `post-vector-optimization-2026-05-26-emacs-30.2-darwin.edn`

To generate a new result:

```bash
emacs -Q --batch -L . -l benchmark/gc-benchmark.el \
  -f parseedn-benchmark-run-batch
```

Or write to a file:

```bash
PARSEEDN_BENCHMARK_OUTPUT=benchmark/results/latest.edn \
emacs -Q --batch -L . -l benchmark/gc-benchmark.el \
  -f parseedn-benchmark-run-batch
```
