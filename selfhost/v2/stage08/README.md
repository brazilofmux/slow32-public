# Stage08: Pragmatic Archiver Parity Gate

This stage is a practical widening step before `cc.c` work: it hardens the
subset-C archiver (`s32-ar.c`) across the expected command surface.

Gate commands covered:
- `c` create
- `rc` replace/create
- `t/x` list/extract
- `d` delete
- `m` move-to-end
- `v/p` verbose/list + print-member
- `cs` bounded symbol-scan path

Run:

```bash
selfhost/v2/stage08/run-regression.sh --emu ./tools/emulator/slow32-fast
```
