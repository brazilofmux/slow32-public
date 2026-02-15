# Stage04 Test Buckets (V2)

This directory defines stable test buckets for V2 migration without moving legacy files yet.

Current source-of-truth test files remain in `selfhost/stage4/tests/`.

Buckets:
- `baseline`: canonical stage4 regression cases (`test1.c`..`test9.c`)
- `as-bisect`: assembler spike bisect/repro corpus (`as_bisect*.c`)
- `ar-bisect`: archiver spike bisect/repro corpus (`ar_bisect*.c`)

Use `manifests/*.lst` to consume bucket membership from scripts.
