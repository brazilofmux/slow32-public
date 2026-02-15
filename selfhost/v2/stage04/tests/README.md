# Stage04 Test Buckets (V2)

This directory defines stable Stage04 test buckets in the canonical V2 layout.

Buckets:
- `baseline`: canonical stage4 regression cases (`test1.c`..`test9.c`)
- `subset`: Stage4 Subset-C conformance litmus corpus (`tests/subset/subset*.c`)
- `subset-idioms`: Stage5 utility idioms proven as standalone subset-C litmus tests
- `subset-known-gaps`: tracked non-gating subset idioms (currently empty)
- `subset-stage5-as-known-gaps`: tracked non-gating idioms that fail only on Stage5 assembler path
- `as-bisect`: assembler spike bisect/repro corpus (`as_bisect*.c`)
- `ar-bisect`: archiver spike bisect/repro corpus (`ar_bisect*.c`)
- `reloc-bisect`: stage07 relocation-linker source bisect corpus (`reloc_bisect*.c`)
- `reloc-bisect-lo12`: focused LO12 sub-bisect corpus (`reloc_bisect03[a-c].c`)
- `reloc-bisect-lo12-pack`: focused LO12 pack-shape corpus (`reloc_bisect03a[1-3].c`)
- `reloc-bisect-lo12-rdwr`: LO12 minimal read/write-shape corpus (`reloc_bisect03a{00,9,...}.c`)

The full bisect corpus is tracked as fixtures in this directory.

Use `manifests/*.lst` to consume bucket membership from scripts.
