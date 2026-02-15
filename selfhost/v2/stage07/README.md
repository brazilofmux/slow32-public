# Stage07: Subset-C Linker Replacement (Spike)

This stage starts the C-subset linker track (`.s32o + .s32a -> .s32x`).

Current spike scope:
- Build `validation/s32-ld.c` through the existing selfhost pipeline (`stage04 -> stage01 -> stage03`).
- Run the resulting linker to link a tiny single-object program (`main: halt`).
- Execute the produced `.s32x` on the selected emulator.

Current constraints in `validation/s32-ld.c`:
- Single `.s32o` input only.
- No archive support yet.
- Bounded `REL_32` relocation support only.

Run:

```bash
selfhost/v2/stage07/run-spike.sh --emu ./tools/emulator/slow32-fast
```

Relocation-codegen bisect helper:

```bash
selfhost/v2/stage07/run-reloc-bisect.sh --emu ./tools/emulator/slow32-fast
```
