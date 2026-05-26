# Benchmarks

This directory contains manual benchmarks for `parseedn`.

## Garbage / GC-pressure benchmark

`gc-benchmark.el` is the canonical manual benchmark for baseline allocation
pressure. It does not attempt exact byte-allocation accounting. Instead, it
measures GC pressure by recording `gcs-done` deltas while parsing a fixed
checked-in workload under a fixed low `gc-cons-threshold`.

### Canonical run procedure

Run from the repository root:

```bash
emacs -Q --batch -L . -l benchmark/gc-benchmark.el \
  -f parseedn-benchmark-run-batch
```

The benchmark locates its fixture corpus relative to `benchmark/gc-benchmark.el`,
so it should remain runnable as long as the repository layout is preserved.

Optional environment variables:

```bash
PARSEEDN_BENCHMARK_ITERATIONS=200
PARSEEDN_BENCHMARK_GC_THRESHOLD=80000
PARSEEDN_BENCHMARK_OUTPUT=benchmark/results/latest.edn
```

### Noise control

The benchmark reduces noise by:
- using a fixed checked-in workload under `benchmark/fixtures/`
- performing one warmup pass excluded from measurement
- forcing GC before measurement
- using a fixed low `gc-cons-threshold` during measurement
- emitting environment metadata with the results

For before/after comparisons, use the same machine and Emacs version whenever
possible.

### Output

The output is EDN containing:
- environment metadata
- aggregate measurement totals
- per-fixture rows

Primary metric:
- `:gc-count-delta`

Secondary metrics:
- `:elapsed-seconds`
- `:gc-elapsed-seconds`
- fixture and input-size metadata

### Existing speed benchmark

`speed-comparison.el` remains available for exploratory wall-clock timing using
an external `edn.list` file, but it is not the canonical baseline benchmark for
allocation pressure.
