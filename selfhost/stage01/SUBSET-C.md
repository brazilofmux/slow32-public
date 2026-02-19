# Stage01 Subset-C Contract (v1)

This file defines the **minimum reliable C subset** expected from `selfhost/stage01/cc.fth`.

Scope:
- `int` and `char` scalar operations
- local/global variables
- arrays and pointer indexing
- function calls (including multiple args)
- control flow (`if/else`, `while`, `for`, `break`, `continue`)
- boolean/relational expressions (`&&`, `||`, comparisons)
- string/char literals used by stage4 utilities

Non-goals in this contract:
- full ISO C compatibility
- optimizer behavior
- advanced/complex C library surface

Conformance gate:
- Test corpus: `selfhost/stage01/tests/subset/`
- Manifest: `selfhost/stage01/tests/manifests/subset.lst`
- Runner: `selfhost/stage01/run-subset-conformance.sh`
- Stage2 idiom corpus: `selfhost/stage01/tests/subset-idioms/`
- Stage2 idiom manifest: `selfhost/stage01/tests/manifests/subset-stage2-idioms.lst`

Policy:
- Any change to `cc.fth` should keep this suite green.
- Utility spikes (`s32-as.c`, `s32-ar.c`, etc.) are integration tests, not a substitute for subset conformance.

Current known gap:
- none currently tracked; existing gap scanner remains available for future additions.

Known-gap tracking:
- Manifest: `selfhost/stage01/tests/manifests/subset-known-gaps.lst`
- Scanner: `selfhost/stage01/run-subset-gap-scan.sh`
- Stage2-assembler-path manifest: `selfhost/stage01/tests/manifests/subset-stage2-as-known-gaps.lst`
- Stage2-assembler-path scan: `selfhost/stage01/run-subset-gap-scan.sh --mode progressive-as --manifest ...`

Long-branch debug helper:
- Script: `selfhost/stage01/run-cc-long-branch-debug.sh`
- Usage: `selfhost/stage01/run-cc-long-branch-debug.sh --src selfhost/stage01/tests/subset/subset13_long_branch.c`
- Purpose: compile a single source with a temporary `DBG-LONG-BRANCH=1` compiler copy and print `[CC] Long branches lowered: <n>`.
