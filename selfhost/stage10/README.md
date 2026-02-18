# Stage 10: Emulator Selection Gate

## What This Is

Stage 10 is not new code — it's a selection gate. The emulators needed for
stages 11+ already exist in the main tree. This stage documents which one
to use and how to build it.

## Which Emulator

| Platform | Emulator | Path | Performance |
|----------|----------|------|-------------|
| ARM (AArch64) | slow32-dbt | `tools/dbt/slow32-dbt` | ~6 BIPS |
| Intel (x86-64) | slow32-dbt | `tools/dbt/slow32-dbt` | ~3 BIPS |
| Other | slow32-fast | `tools/emulator/slow32-fast` | ~50 MIPS |

All stages from 11 onward use **slow32-dbt** as the default emulator.

## Building

```bash
# From the repository root:
make

# Or build just the emulators:
cd tools/dbt && make        # slow32-dbt (ARM/Intel)
cd tools/emulator && make   # slow32-fast (portable)
```

## Relationship to Stage 00

Stage 00's `s32-emu` (800-line C emulator) remains essential — it is the
minimal bootstrap emulator that the Forth kernel runs on. Nothing smaller
will work for bootstrapping. Stage 10 doesn't replace it; it selects the
high-performance emulator for the heavier workloads from stage 11 onward.

## Why Not a SLOW-32 Emulator?

Writing an emulator in subset-C to run on SLOW-32 would only give emulated
floating-point and emulated MMIO. The host emulators provide real
floating-point, real file I/O, and real process arguments via MMIO.
The path to native execution is a cross-compiler (stages 14+), not
a self-hosted emulator.

## Status

Complete. Use slow32-dbt for all subsequent stages.
