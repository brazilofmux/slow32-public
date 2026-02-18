# V2 Stage Status

This file is the quick orientation map for bootstrap work.

| Stage | Goal | Entry | Current State | Next Blocker / Step |
|---|---|---|---|---|
| `stage00` | Minimal bootstrap emulator (~800-line `.s32x` runner) | `make -C selfhost/stage00 && make -C selfhost/stage00 test` | completed | none |
| `stage01` | Forth assembler (`.s -> .s32o`, first Forth tool) | `selfhost/stage01/run-regression.sh test1` | completed | none |
| `stage02` | Forth archiver (`.s32o -> .s32a`) | `selfhost/stage02/run-regression.sh test3` | completed | expand corpus beyond `test3` fixture path |
| `stage03` | Forth linker (`.s32o + .s32a -> .s32x`) | `selfhost/stage03/run-regression.sh test3` and `... archive`; `SELFHOST_EMU=./tools/emulator/slow32-fast selfhost/stage03/run-selfhost-kernel.sh` | completed (kernel regen gate enabled in stage walk) | add stricter output equivalence checks (optional) |
| `stage04` | Forth-hosted subset C compiler + Forth kernel fixed-point proof | `selfhost/stage04/run-regression.sh` | completed (stage gate) | maintain stability while Stage05/06 replace tools |
| `stage05` | Subset-C assembler implemented in Subset C (hosted by Forth) | `selfhost/stage05/run-pipeline.sh --mode progressive-as --test test1` | in progress (green on `test1..test3`) | extend green set to full stage04 regression corpus |
| `stage06` | Subset-C archiver implemented in Subset C (hosted by Forth) | `selfhost/stage05/run-pipeline.sh --mode stage6-ar-smoke` and `... stage6-ar-rc-smoke` and `... stage6-ar-tx-smoke` and `... stage6-ar-d-smoke` | in progress (smoke gate green on `c/rc/t/x/d`) | widen coverage to `m/v/p` parity and archive-scan parity (`cs`) |
| `stage07` | Subset-C linker implemented in Subset C (hosted by Forth) | `selfhost/stage07/run-spike.sh --with-reloc-spike` | in progress (single-object + bounded `REL_32/HI20/LO12` spike wired) | widen to branch/jal relocations + archive member resolution |
| `stage08` | Subset-C compiler implemented in Subset C (still hosted by Forth) | `selfhost/stage08/run-regression.sh` | in progress (archiver parity + first compiler-in-C spike green) | grow `cc.c` toward a full compiler while preserving the gate |
| `stage09` | Subset-C fixed-point proof (Subset C self-compiles) | `selfhost/stage09/` | not started | define reproducible fixed-point harness |
| `stage10` | Subset-C assembler re-implemented in Subset C (self-hosted; feature parity with tools/) | `selfhost/stage10/` | not started | carve baseline from Stage05 artifacts |
| `stage11` | Subset-C archiver re-implemented in Subset C (self-hosted; feature parity) | `selfhost/stage11/` | not started | mirror stage06 coverage with native runtime/tests |
| `stage12` | Subset-C linker re-implemented in Subset C (self-hosted; feature parity) | `selfhost/stage12/` | not started | reach main-tree parity + archive member resolution |
| `stage13` | Full C compiler (likely multi-layer; first non-subset pass) | `selfhost/stage13/` | not started | break compiler roadmap into per-layer subtargets |
| `stage14` | TBD – follow-on compiler layers (optimizer? codegen polish) | `selfhost/stage14/` | not started | derive scope from Stage13 layering |
| `stage15` | TBD – rebuilt toolchain with the full compiler stack | `selfhost/stage15/` | not started | requires prior stage definitions |
| `stage16` | TBD – final fixed-point checkpoint for full C stack | `selfhost/stage16/` | not started | inherits plan from Stage15 output |

## Stage Cycles

The current plan groups the bootstrap into repeating "cycles":

- Stage00 stands alone as the reference emulator (~800 lines).
- Stages01–04: assembler/archiver/linker/compiler written in Forth, ending with the Forth-kernel fixed-point proof.
- Stages05–08: the same four tools re-authored in Subset C but still hosted by the Forth stack.
- Stage09: fixed-point proof demonstrating Subset C can compile itself.
- Stages10–12: another assembler/archiver/linker pass, this time implemented and hosted by Subset C with feature parity to `tools/`.
- Stage13+: full C compiler bring-up. Exact layer count TBD, but Stage13 tracks the first compiler layer with follow-on stages recorded as we lock the plan.

## Ordered Walk

For a clean-checkout validation of active early stages:

```bash
selfhost/run-stages.sh --emu ./tools/emulator/slow32-fast
```
