# SLOW-32 Self-Hosting Bootstrap

This is the canonical bootstrap roadmap. It describes `selfhost/` **as it
actually exists**; see [History of This Plan](#history-of-this-plan) at the
bottom for what changed and why.

## Philosophy

1. **Trust is auditable.** The only opaque binary is the Stage 0 emulator
   (~780 lines of C). Everything above it is built from inspectable source.
2. **A stage is one *agreement*, not one *artifact*.** Each stage delivers a
   mutually compatible cluster of tools that can rebuild the next stage.
3. **Every cycle ends with a gate.** Fixed-point, purity, conformance, or
   checksum — regressions do not get to hide.

### Why Stages Are Clusters, Not Single Tools

The obvious plan is one artifact per stage: assembler, then archiver, then
linker, then compiler, then libc. It reads well and it does not work.

The four tools share a **format contract** — object layout, symbol tables,
relocation types, archive structure. Change the object format and you have
changed the assembler that emits it, the archiver that bundles it, and the
linker that consumes it, in the same breath. A stage that delivers "just the
linker" is incoherent: there is no such thing as a linker that agrees with
nothing.

So a stage boundary falls where a **self-consistent toolchain** exists, not
where a component is finished. That is why `stage01` is four tools rather than
four stages, and why the stage count is small.

### Why Layered Cycles

Each pass rebuilds the whole assembler/archiver/linker/compiler cluster under
stricter constraints, and each answers a different question:

- **Forth cycle** (`stage01`): do the Forth tools work, and can they rebuild the
  kernel that hosts them? (Kernel fixed-point proof lives here.)
- **Forth → C** (`stage02`): can Subset C tools, hosted by Forth, replace every
  Forth tool? Ends with Forth retired permanently.
- **Self-sufficiency** (`stage03`–`stage04`): can the C toolchain run purely on
  its own output, with a real lexer and parser?
- **Code quality** (`stage05`–`stage06`): can it acquire HIR/SSA, instruction
  selection and register allocation and still reproduce itself exactly?
- **Capability** (`stage07`–`stage08`): can non-trivial SLOW-32 programs — the
  DBT, the full emulator — build *inside* SLOW-32?

---

## Overview

```
Trust root
  stage00   Minimal emulator, host C, ~780 lines          [host cc]

Forth cycle
  stage01   as + ar + ld + cc, written in Forth           [stage00 + kernel.s32x]
            ✓ kernel fixed-point proof

Forth → C
  stage02   the same four, re-authored in Subset C,
            still hosted by the Forth toolchain           [stage01]
            ✓ CHECKPOINT: all Forth tools retired

Self-sufficiency
  stage03   first canonical cc.s32x + as/ar/ld
            + libc/runtime. Self-sufficient.              [stage02 cc-min]
  stage04   cc.s32x with Ragel lexer +
            recursive-descent parser                      [stage03]

Code quality
  stage05   HIR/SSA, BURG instruction selection,
            graph-colouring register allocation           [stage04]
  stage06   seeded from stage05                           [stage05]
            ✓ --fixed-point gate: gen2 == gen3

Capability
  stage07   stage06 shape + richer headers (assert.h, math.h,
            signal.h, stdio.h, time.h, ucontext.h, sys/*) so the
            DBT and full emulator build inside SLOW-32     [stage06]
  stage08   fork of stage07; active head for new language
            features (e.g. bitfields)                      [stage07]

Shared sources
  src/      canonical live frontend/HIR (lex, parser, sema, HIR/SSA).
            stage08 and the cross trees symlink it; stage03-07 keep full
            snapshots for reproducibility.

Sibling cross-compilers (independent of the numbered cycle)
  stage08-cross-x64   C → x86-64 ELF   cc-x64, ld-x64, ar-x64,
                                       libc_x64.a, s32fast-hir, dbt-x64
  stage08-cross-a64   C → AArch64 ELF  cc-a64, ld-a64, ar-a64,
                                       libc_a64.a, s32fast-hir, dbt-a64
```

### Stage Summary

| Stage | Delivers | Language | Built By |
|-------|----------|----------|----------|
| stage00 | emulator (trust root) | C (host) | host C compiler |
| stage01 | as, ar, ld, cc | Forth | stage00 + `kernel.s32x` |
| stage02 | as, ar, ld, cc (cc-min) | Subset C | stage01 (Forth) |
| stage03 | cc.s32x, as, ar, ld, libc | Subset C | stage02 cc-min |
| stage04 | + Ragel lexer, RD parser | C | stage03 |
| stage05 | + HIR/SSA, BURG, graph colouring | C | stage04 |
| stage06 | same, `--fixed-point` gate | C | stage05 |
| stage07 | + richer headers | C | stage06 |
| stage08 | active head | C | stage07 |
| cross-x64 | cc-x64 → x86-64 ELF | C | stage08 / host |
| cross-a64 | cc-a64 → AArch64 ELF | C | stage08 / host |

---

## Trust Model

The bootstrap crosses its trust boundary exactly **three** times.
`check-bootstrap-purity.sh` enforces this over `stage00`..`stage08`, and names
the permitted seeds:

- `stage00/s32-emu` — built from ~780 lines of host C
- `forth/kernel.s32x` — the Forth kernel binary
- `forth/prelude.fth` — the Forth prelude

Note `forth/` is **top-level** in the repository, a peer of the applications
(`dbase`, `lua`, `sbasic`, …), not a subdirectory of `selfhost/`. The kernel is
both a bootstrap seed and a shipped program.

**What this model does and does not claim.** It claims the opaque surface is
small enough to audit. It does *not* claim Thompson-freedom: `gen2 == gen3`
proves the compiler is a deterministic function of its input, which is exactly
what a compromised compiler also produces. Closing that would require diverse
double-compiling — building the toolchain with a genuinely independent
implementation and comparing bytes. That has not been done.

A single point-in-time cross-check against gforth *was* run, and it earned its
keep: gforth enforces IF/THEN control-flow balance and our Forth did not, which
surfaced an unmatched `IF` in `cc.fth`. The strictness was then reimplemented
natively (`aedbc353`), regression-tested (`27b4f822`), and the compiler bug
fixed (`49f62669`) — all on 2026-06-25. gforth is not wired into any gate; what
was kept is the rule, not the oracle.

### Stage 0: The Trust Root

- **You need:** a C compiler on the host (gcc, clang, tcc — anything), and
  `s32-emu.c` (~780 lines, POSIX-only, no external dependencies).
- **You produce:** `s32-emu`, a minimal emulator that loads and runs `.s32x`.
- **It handles:** all 34 integer instructions; legacy MMIO (PUTCHAR, GETCHAR,
  BRK, EXIT); MMIO ring-buffer I/O (files, args, environment); `.s32x` header
  parsing and section loading.
- **It does NOT handle:** debugging (trace, breakpoints, step), service
  negotiation, performance work.
- **Verification:** run known-good `.s32x` binaries and compare output
  character-for-character against `slow32` / `slow32-fast`.
- **Optional speed path:** `stage00/Makefile` symlinks `s32-emu` to
  `tools/dbt/slow32-dbt` when present. This is a runtime-speed convenience
  only — the DBT runs higher stages tens to hundreds of times faster. The
  Stage 0 source build remains the canonical seed; the DBT does not change
  which inputs cross the trust boundary, and its own source is self-hostable
  via the sibling cross-compilers.

---

## Gates and Verification

| Gate | Script / flag | Checks |
|------|---------------|--------|
| Stage chain | `run-stages.sh` | each stage rebuilds the next |
| Purity | `check-bootstrap-purity.sh` | no seeds beyond the permitted three, `stage00`..`stage08` |
| Fixed point | stage06 `--fixed-point` | `gen2 == gen3` |
| Reproducibility | `sha256sums.md`, `verify-emu-sums.sh` | bit-identical artifacts across platforms *and* emulators |
| ISA conformance | `isa-conformance/` | instruction semantics |
| ABI conformance | `run-abi-conformance.sh` | calling convention |

`sha256sums.md` records bit-identical output across a Raspberry Pi 4, a Ryzen 5
3600, a Celeron N4500 and a Lenovo Cortex-A720/X4, under `s32-emu`,
`slow32-fast` and `slow32-dbt` (stages 4/5/6). The naive interpreter on a Pi and
the JIT on a Ryzen emit the same bytes.

---

## Reference Docs

- [`TOOL-NAMING.md`](../TOOL-NAMING.md) — tool-name conventions across stages
- [`ISSUES.md`](../ISSUES.md) — code-review findings, open and closed
- [`README.md`](../README.md) — stage directory guide

---

## History of This Plan

Worth recording, because the shape of the tree is the argument.

The original plan (V1/V2) specified **sixteen fine-grained stages**, each
delivering exactly one artifact — assembler, then archiver, then linker, then
compiler, then standard library — across Layers A–F. It argued that squashing
stages was "terrible for actually doing the work" and that "Stage 7 of 16 is
more motivating than somewhere in Stage 2."

The work overtook it roughly four or five revisions ago. Planned Stages 1–4
became `stage01`; Stages 5–8 became `stage02`; the `gen2 == gen3` gate landed at
`stage06` rather than the planned Stage 9; Stages 10–16 never existed; and
`stage05`'s HIR/SSA/BURG/graph-colouring work was never in the plan at all.

The reason is in [Why Stages Are Clusters](#why-stages-are-clusters-not-single-tools):
one artifact per stage is incoherent when the artifacts share a format
contract. The plan was right about the destination and wrong about every
increment, which is the normal condition of plans.

This document sat at V2, still labelled "the single source of truth", while the
tree moved several revisions past it. Updated 2026-07-16 to describe what is
actually on disk.
