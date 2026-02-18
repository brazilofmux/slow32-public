# Stage 12: Subset C Linker (Self-Hosted Parity)

Stage 12 completes the Subset C parity cycle. With the Stage 10 assembler and
Stage 11 archiver rebuilt under Gen2, we now rebuild the linker so its command
line, relocation engine, archive member resolution, and injected symbols match
`tools/s32-ld`.

## Goals

1. **Self-hosted build** – compile `s32-ld.c` (or the maintained minimal port)
   with the Stage 09 compiler and Stage 10/11 toolchain pieces.
2. **Relocation parity** – support every relocation that the production linker
   does (REL32, HI20/LO12, jal, branch, PC-relative data, weak references).
3. **Archive resolution** – consume Stage 11 archives, perform breadth-first
   pulls, and honor duplicate/weak symbol rules identically to `tools/s32-ld`.
4. **Injected symbols + layout** – generate the same memory map (`__bss_start`,
   `__heap_start`, section addresses) and `.s32x` headers, keeping binaries
   bit-identical to the reference builds whenever possible.

## Inputs

- Stage 10 assembler, Stage 11 archiver, libc/runtime archives, and Gen2 compiler.
- Linker sources from `tools/linker/s32-ld.c` (and shared headers in `common/`).
- Regression programs ranging from single-object tests to the Forth kernel,
  runtime, and cc.c self-host harnesses.

## Outputs

- `stage12/out/s32-ld.s32x`: self-hosted linker binary.
- Paired `.s32x` executables for each regression, produced by both Stage 12 and
  the production linker, ready for `cmp`/`objdump` comparison.
- Logs capturing relocation applications, archive pulls, and memory layouts.

## Suggested Workflow

1. **Relocation spikes** – adapt the Stage 07 relocation tests to run under the
   self-hosted assembler/object corpus so every relocation type is exercised.
2. **Archive member tracing** – log decisions about which archive members were
   pulled to ensure parity with the production linker (especially for mutually
   recursive symbols).
3. **Gate script** – `stage12/run-regression.sh` should rebuild the kernel,
   runtime, and a representative cc.c sample, diff the binaries, and fail on any
   mismatch.

## Exit Criteria

- Linker binary built entirely by the self-hosted toolchain.
- `.s32x` outputs match the reference linker for the full regression corpus.
- Archive member selection + relocation logs align.
- Gate added to `selfhost/run-stages.sh` so regressions block the ordered walk.

## Notes

- Files in this directory currently describe the earlier cc-min archiver spike.
  Keep them as historical material until the linker parity work lands, then
  migrate/retire as appropriate.
