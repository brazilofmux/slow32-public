# Stage 11: Subset C Archiver (Self-Hosted Parity)

Stage 11 replaces the Forth-built archiver with a Gen2-built binary that is
byte-for-byte compatible with `tools/s32-ar`. It consumes the Stage 10
self-hosted assembler and emits deterministic archives that the Stage 12 linker
relies on for member selection.

## Goals

1. **Self-hosted build** – compile `s32-ar.c` (or its maintained mirror) using
   the Stage 09 fixed-point compiler and Stage 10 assembler/linker.
2. **Archive format parity** – headers, timestamps, member padding, and symbol
   table encoding identical to the production archiver.
3. **Symbol index** – generate the exact symbol table (name offsets + member
   indices) expected by the linker’s archive scanner, including duplicate and
   weak symbol handling.
4. **Deterministic output** – identical archives across rebuilds regardless of
   filesystem ordering or host locale.

## Inputs

- Stage 10 parity assembler + runtime artifacts.
- Archiver sources (`tools/utilities/s32-ar.c`).
- Regression corpus: runtime `.s32o` files, libc objects, and synthetic
  fixtures covering empty members, duplicate symbols, long filenames, and
  archive-rescan stress tests.

## Outputs

- `stage11/out/s32-ar.s32x`: self-hosted archiver binary.
- `stage11/out/*.s32a`: paired archives from both the production and Stage 11
  archivers for diffing.
- `stage11/logs/*.txt`: reports comparing archive headers, member tables, and
  symbol indices line-by-line.

## Suggested Workflow

1. **Leverage Stage 10 harness** – reuse its parity diffing approach to compare
   archives and ensure symbol tables remain in sync.
2. **Archive scanner tests** – feed archives into the Stage 07 or Stage 12
   linker spikes to validate member selection behavior under heavy aliasing.
3. **Gate script** – add `stage11/run-regression.sh` that rebuilds runtime
   archives and fails on any diff or missing symbol. Wire it into
   `selfhost/run-stages.sh`.

## Exit Criteria

- Archiver binary produced entirely by the self-hosted toolchain.
- Runtime/libc archives rebuilt with Stage 11 pass byte-for-byte diffs.
- Archive scanner parity confirmed via linker spikes.
- Stage gate registered so regressions block the ordered walk.

## Notes

- `s32-as-port.c`, `run-spike.sh`, and other files in this directory document
  the prior cc-min experiment. Preserve them until the new parity gate is
  implemented, but treat them as historical context rather than the current
  deliverable.
