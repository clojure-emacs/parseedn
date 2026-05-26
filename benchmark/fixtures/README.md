# Benchmark fixtures

This directory contains the canonical checked-in workload for the manual
allocation / GC-pressure benchmark.

Fixture intent:
- `scalars.edn` exercises leaf parsing for booleans, numbers, strings,
  keywords, symbols, and characters.
- `collections.edn` exercises nested vectors, lists, maps, and sets.
- `prefixed-maps.edn` exercises namespaced map handling.
- `tags.edn` exercises the built-in `#inst` and `#uuid` tag readers.
- `repeated-structure.edn` provides a denser repeated structure so repeated
  benchmark iterations generate more stable GC pressure.

These fixtures are representative rather than exhaustive. They are intended to
provide a stable baseline workload for before/after comparisons.
