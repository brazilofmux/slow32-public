# The stage08 inliner

Added 2026-08-31.  **Ships disabled** (`S12CC_INLINE=<n>` enables it);
the reason is capacity, not correctness.

## Why it exists

The [ILP study](ILP-STUDY.md) profiled the self-hosted compiler
compiling its own source and found **14.2% of executed instructions are
jumps** — triple DOOM's rate, and the signature of a recursive-descent
parser making many small calls.  It also found that workload's
dependence-height limit is only 8.74 IPC, with memory dependences adding
*nothing* (models 1 and 2 agree to the cycle): the compiler is waiting on
itself, and calls are a large part of what it waits on.  Inlining cuts
the jump rate and the dependence height at once.

## How it works

Not an AST transform.  The callee's body AST is **lowered again** at the
call site (`hl_inline_call` in `hir_lower.h`), which buys three things a
clone-and-splice would not:

- **Evaluation order and conditionality come free.**  A call inside
  `a && f(b)` is lowered where the `&&` already placed it, so there is
  no need for a whitelist of "safe" call positions.
- **No AST cloning**, so no node renaming and no aliasing hazards.
- **The callee's locals become ordinary allocas in the caller's frame**,
  so mem2reg — and `ssa_split_pair_allocas` for its doubles — promotes
  them exactly like the caller's own.  This is why the generated code is
  clean: `sq(i)` inlines to a single `mul`, with every parameter slot
  promoted to a register and no memory traffic.

The frame is relocated by **one constant shift** rather than a
per-variable map: reserve `locals_size` bytes in the caller and map every
callee offset to `off - shift`.  Arrays and structs come along without
needing their sizes, and 8-alignment survives because the shift is a
multiple of 8.  `return` inside an inlined body stores to a result slot
and branches to a continuation block.

Refused: varargs, struct parameters or return, bodies containing labels
or `goto`, recursion (direct or mutual), argument-count mismatches, and
**callees containing loops** — a loop amortises its own call overhead, so
inlining it inflates the caller for nothing (measured: refusing daxpy
turned a 3% loss into a 2.4% win).

## Measured

| | inlining off | on (budget 20) |
|---|---:|---:|
| LINPACK-C | 1,000,445,397 | **976,654,747** (−2.4%) |
| mandel-C | 35,262,855 | 35,262,855 (no inlinable calls) |

Budget sweep on LINPACK-C shows the classic inlining curve — small
callees win, generous budgets lose: 25 → −2.4%, 50 → −1.9%, 100 → +2.5%,
400 → +3.1%.  The `no loops` rule is what separates the two regimes.

## Why it ships off: four fixed ceilings

Inlining `s12cc.c` makes the compiler exceed limits in the **frozen
bootstrap tools**, and self-hosting is not negotiable.  Hit in sequence:

| ceiling | value | disposition |
|---|---|---|
| `HL_MAX_ALLOCA` | 256 | raised to 2048 (each splice mints parameter/result slots) |
| `HIR_MAX_BLOCK` | 2048 | guarded — refuse to inline into already-huge callers |
| `cg_out` | 4 MB | **was silently truncating**; now a hard error |
| stage07 `s32-ld` `MAX_FILE_SYM` | 2048 | blocks self-host; each splice mints fresh block labels |

Raising them one at a time just finds the next one.  The real fix is
grow-on-demand capacity across the toolchain, which is its own project.

One of these was a pre-existing landmine worth calling out: `cg_out`
overflow **dropped characters silently** and emitted a corrupt `.s` that
failed much later and confusingly.  Inlining made it reachable; the bug
predates it.

## Enabling

```bash
S12CC_INLINE=20 cc program.c program.s   # node budget; 0 = off
```

Correctness is gated by `selfhost/stage08/tests/` plus a dedicated
semantic torture test covering early return, loops and `break` in the
callee, side-effect ordering, short-circuit conditionality, nesting,
recursion refusal, per-instance locals, address-taken locals, doubles,
and array locals — green at every budget tried.

---

## Capacity: the follow-on project

The ceiling table above is the argument for making toolchain capacity
grow rather than be rationed.  Measured headroom at the time (2026-08-31):

| ceiling | limit | actual | used |
|---|---:|---:|---:|
| `s32-ld` `MAX_FILE_SYM` | 2048 | **2053** | **100.2% — broke the build** |
| `s32-as` `MAX_TEXT` | 1,048,576 | 549,936 | 52% |
| `s32-as` `MAX_LBL` | 32,768 | 15,681 | 48% |
| `s32-as` `MAX_REL` | 65,536 | 15,375 | 23% |
| `s32-ar` `MAX_MEMBERS` | 128 | 13 | 10% |

Two of these were within one compiler-doubling of failing, which is why
the work goes bottom-up (assembler, librarian, linker) rather than
compiler-first: the compiler is what *produces* the pressure, and
enlarging it while the consumers stay fixed just relocates the failure.

`s32vec.h` is the primitive — grow-on-demand arrays, not an arena.
Arenas suit many small same-lifetime objects (the compiler's AST/HIR
nodes); the tools' symbol, relocation and section tables are
homogeneous indexed arrays that want `realloc`.

**Landed:** the assembler's three section images (`.text`/`.rodata`/
`.data`).  Output is byte-identical across nine inputs including the
2 MB compiler self-compile, the ceiling is gone, and 3 MB of BSS goes
with it — which also cuts startup, since crt0 memsets BSS before every
run (the ILP trace measured 83 M instructions clearing 16.7 MB).

**Not landed — open bug.** Converting the label and relocation tables
makes the assembler emit **one spurious empty symbol** (string table
grows by exactly one NUL; the object gains a symbol with an empty
name).  Bisected: sections-only is byte-identical, so it is isolated to
`get_lbl`/`add_reloc_ex`.  It is *not* the obvious dangling-pointer
hazard — snapshotting the name into a local buffer before growing the
pool did not fix it — and it is not the vector primitive, which passes
a 5,000-element on-target unit test for both element types.  Whatever
it is, it predates or is exposed by the growth and deserves a proper
root-cause rather than a guess.
