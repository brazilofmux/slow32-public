# Stage07: Subset-C Linker Replacement (Spike)

This stage starts the C-subset linker track (`.s32o + .s32a -> .s32x`).

Current spike scope:
- Build `validation/s32-ld.c` through the existing selfhost pipeline (`stage04 -> stage01 -> stage03`).
- Run the resulting linker to link a tiny single-object program (`main: halt`).
- Execute the produced `.s32x` on the selected emulator.

Current constraints in `validation/s32-ld.c`:
- Primary `.s32o` plus optional bounded aux input (`.s32o` or `.s32a`).
- Archive support is bounded and symbol-aware (single selected member).
- Bounded relocation support for `REL_32`, `REL_HI20`, `REL_LO12`,
  `REL_BRANCH`, and `REL_JAL`.

Relocation spike run (recommended gate while widening):

```bash
selfhost/v2/stage07/run-spike.sh --emu ./tools/emulator/slow32-fast --with-reloc-spike
```

Run:

```bash
selfhost/v2/stage07/run-spike.sh --emu ./tools/emulator/slow32-fast
```

Relocation-codegen bisect helper:

```bash
selfhost/v2/stage07/run-reloc-bisect.sh --emu ./tools/emulator/slow32-fast
```

Archive-resolution spike scaffold:

```bash
selfhost/v2/stage07/run-archive-spike.sh --emu ./tools/emulator/slow32-fast
```

Two-lane archive spike modes:
- `--mode direct`: pass `.s32a` directly to stage07 linker (expected pass).
- `--mode extract`: extract first member and pass it as optional aux object input
  to stage07 linker (expected pass).
