# Stage04 Test Buckets (V2)

This directory defines stable Stage04 test buckets in the canonical V2 layout.

Buckets:
- `baseline`: canonical stage4 regression cases (`test1.c`..`test9.c`)
- `subset`: Stage4 Subset-C conformance litmus corpus (`tests/subset/subset*.c`)
- `as-bisect`: assembler spike bisect/repro corpus (`as_bisect*.c`)
- `ar-bisect`: archiver spike bisect/repro corpus (`ar_bisect*.c`)

The full bisect corpus is tracked as fixtures in this directory.

Use `manifests/*.lst` to consume bucket membership from scripts.
