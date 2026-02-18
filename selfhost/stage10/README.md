# Stage 10: Subset C Assembler (Self-Hosted Parity)

Stage 10 kicks off the “Subset C on itself” cycle. By this point Stage 09 has
produced a fixed-point compiler (Gen2 == Gen3). Stage 10 uses that toolchain to
rebuild the assembler so it is **(a)** compiled entirely by the self-hosted C
stack and **(b)** functionally indistinguishable from `tools/slow32asm`.

## Goals

1. **Self-hosted build** – `cc.c` (Gen2) compiles `s32-as.c`, assembles it with
   the self-hosted assembler, and links it with the self-hosted linker/runtime.
2. **Directive + syntax parity** – `.macro`, `.include`, `.set`, pseudo-ops, and
   all other assembler surface match the `tools/` implementation.
3. **Relocation coverage** – output `.s32o` files emit every relocation type the
   production assembler does (REL32, HI20/LO12, branch/jal, TLS hooks, etc.).
4. **Diagnostics parity** – fatal errors and warnings use the same text,
   locations, and exit codes so regression logs remain comparable.

## Inputs

- Stage 09 Gen2 compiler, assembler, linker, runtime, and libc archives.
- `tools/assembler/slow32asm.c` (or its maintained mirror under `selfhost/`).
- Regression fixtures from Stage 04/05 (`test*.s`) plus assembler-specific
  spikes (macro recursion, `.incbin`, `.org`, `.fill`, `.align`, `.option`).

## Outputs

- `stage10/out/s32-as.s32x`: self-hosted assembler binary.
- `stage10/out/*.s32o`: regression artifacts assembled by both the baseline
  and Stage 10 binaries to support diffing.
- `stage10/logs/*.txt`: parity reports comparing section headers, relocations,
  symbol tables, and diagnostic streams.

## Suggested Workflow

1. **Bootstrap artifacts** – `stage09/README.md` explains how to capture Gen2
   compiler + runtime bundles. Export paths or symlinks here for convenience.
2. **Progressive parity** – keep the Stage 05 “progressive” modes until the
   Gen2 build is clean, then flip the default to the self-hosted binary and
   delete the crutches.
3. **Diff harness** – teach `run-spike.sh` (or an equivalent helper) to invoke
   both assemblers on the same corpus and compare `.s32o` blobs byte-for-byte.
4. **Gate** – add a `stage10/run-regression.sh` that fails if any diff or
   diagnostic mismatch occurs. This script becomes the stage gate.

## Exit Criteria

- Self-hosted assembler rebuild is reproducible (no host/Forth tools needed).
- Regression corpus passes with byte-identical `.s32o` output.
- Diagnostics align with `tools/slow32asm`.
- Gate script wired into `selfhost/run-stages.sh`.

## Notes

- This README describes the **target** state. Existing files in this directory
  are scaffolding from earlier experiments; keep them until the new gate lands,
  but treat them as historical.
- Emulator selection now lives in top-level docs (`selfhost/docs/BOOTSTRAP-V2.md`).
  Stage 10 is purely about the assembler parity milestone.
