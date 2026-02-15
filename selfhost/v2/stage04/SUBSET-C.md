# Stage4 Subset-C Contract (v1)

This file defines the **minimum reliable C subset** expected from `selfhost/v2/stage04/cc.fth`.

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
- Test corpus: `selfhost/v2/stage04/tests/subset/`
- Manifest: `selfhost/v2/stage04/tests/manifests/subset.lst`
- Runner: `selfhost/v2/stage04/run-subset-conformance.sh`

Policy:
- Any change to `cc.fth` should keep this suite green.
- Utility spikes (`s32-as.c`, `s32-ar.c`, etc.) are integration tests, not a substitute for subset conformance.
