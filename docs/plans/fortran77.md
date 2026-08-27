# Fortran 77 on SLOW-32

Status: **started 2026-08-27.** Backend detached and proven; frontend
not yet begun.

## Why this one

`docs/plans/1987-desk.md` §8 is titled "Languages, only if they change
the job", and Fortran 77 is its first entry: *"We already have CORDIC
and native f64. 80s science was Fortran talking to a plotter, not
Python talking to a GPU."*

Sorted by domain rather than by name, the desk already covers systems
(C/C++), structured (FPC), interactive (sbasic), business-interactive
(dBase, clip), stack (Forth), scripting (Lua) and symbolic (lisp,
prolog — both minimal). **Numeric/scientific is empty.** No other
missing language opens a domain the machine cannot otherwise reach;
this one does, and its substrate quietly finished landing after the
desk bullet was written.

### The substrate is better than the bullet claimed

The desk credited CORDIC. That is superseded and should not be cited
as the reason:

- CORDIC was profiled and **Newton series beat it**. `math_soft.c`
  carries the Newton implementations.
- Those soft routines are a **fallback**, not the main path. Under the
  DBT, `math_intercepts[]` in `tools/dbt/dbt.c` overrides **37**
  symbols — `sin cos tan asin acos atan sinh cosh tanh exp log log10
  sqrt fabs ceil floor round trunc` and the `f` variants — with native
  host calls. The Newton code runs only when the DBT does not
  intercept.
- The DBTs offer native f32 and f64 arithmetic.

So a numeric workload gets native FP *and* native transcendentals.
That is a real numerics platform, not a toy.

**Gap worth closing early:** `pow`, `atan2` and `fmod` are NOT in the
intercept table (`fmodf` is). F77 leans on all three — `**` with a real
exponent, and `ATAN2` is everywhere in numeric code. They currently
fall to the Newton path under exactly the workload Fortran brings.

## Two rulings that shape the design

Both from the user, 2026-08-27, and both narrow the work:

1. **`fortran/` is self-contained: COPY the backend, do not share it.**
   `selfhost/` must stay free to evolve without worrying about breaking
   f77. This is deliberately *not* the cc-x64 / cc-a64 pattern, which
   symlinks `../src`. The copied files carry a provenance header naming
   their source commit; re-syncs are deliberate and re-stamp it.

2. **SLOW-32 is the only target.** The path to x86-64 and aarch64 is
   through `slow32-dbt`, exactly as it is for every other language in
   the set. f77 will not grow native backends: if x64/a64 performance
   is wanted, improve slow32-dbt generally rather than opening two new
   streams.

## Architecture

The stage08 compiler splits cleanly, and the split is sharper than
expected — the backend has just **four** references to the C AST
(`fn->name`, `fn->locals_size`, `fn->next`, `prog->body`, plus
`nparams`/`is_varargs` in the prologue), in ~8.8k lines:

```
  f77 frontend (new)          copied backend (8,851 lines)
  ------------------          ----------------------------
  fixed-form lexer   ─┐
  parser → AST        ├──►  hir.h ──► hir_ssa ──► hir_opt ──► hir_licm
  sema (implicit       │                                          │
   typing, COMMON)     │                                    hir_burg
  lower → HIR        ─┘                                          │
                                              hir_regalloc ──► hir_codegen
                                                                 │
                                                          SLOW-32 asm
```

Inherited free: SSA construction, mem2reg, CSE/DSE/LICM, BURG
instruction selection, IRC graph-colouring regalloc, compare-branch
fusion — all proven by DOOM and sbasic.

### The frontend contract

`src/f77_contract.h` supplies the 45 symbols the backend reads,
in four groups: `fd*` diagnostics; the type encoding and `ty_size` /
`ty_is_*` predicates (kept **bit-identical** to the C compiler's so FP
pair handling and ABI assignment behave identically); the alloca
registry the SSA promoter scans; and the `ps_g*` global-data tables
`gen_data()` emits.

Type mapping: `INTEGER`/`LOGICAL`→`TY_INT`, `INTEGER*2`→`TY_SHORT`,
`REAL`→`TY_FLOAT`, `DOUBLE PRECISION`→`TY_DOUBLE`, `CHARACTER`→
`TY_CHAR`, `COMPLEX`→a pair.

### COMMON blocks are a compiler problem, not a linker problem

`common/s32_formats.h` defines only `LOCAL`/`GLOBAL`/`WEAK` bindings —
there is no COMMON/tentative-definition binding, so `s32-ld` cannot
merge blocks the way an ELF linker would. Each COMMON block is
therefore emitted by *this compiler* as one named, sized `.bss` object
via the `ps_g*` tables, with EQUIVALENCE riding on the same mechanism.
No linker change is required, and none should be added for this.

## Milestones

1. **Backend detached and proven.** ✅ **DONE 2026-08-27.** Copied at
   `849dd791`; `tests/backend_slice.c` drives it with hand-built HIR
   and no frontend, producing Σi² correctly on the emulator (0, 1, 5,
   14, 30, 55, 385). Kept as a permanent gate: it catches a broken
   frontend contract after a re-sync immediately and specifically.
   Notable: mem2reg promoted both allocas to registers, so the emitted
   loop touches no stack at all.
2. **Fixed-form lexer.** ✅ **DONE 2026-08-27.** Split in two, matching
   house style: `f77_card.h` hand-writes the card-image layer (columns
   1-5 label, 6 continuation, 7-72 text, 73-80 ignored; comment lines;
   blank squeezing; Hollerith counts) because counted reads are not a
   regular-language job, and `f77_lexer.rl` is a Ragel `-G2` scanner
   over the assembled statement. Gated by `tests/torture.f`, which
   pins the rules that bite: `1.EQ.2` splitting correctly rather than
   lexing `1.` as a REAL, `X.GE.1.5.AND..NOT.Y`, `1.5D-3` vs `1.E5`,
   Hollerith with embedded blanks (`6HAB CD `), `IT''S`, continuation
   cards, tab-format source, case folding, and column 73+ ignored.

   **The lexer classifies no keywords, on purpose.** F77 has no reserved
   words, so `PROGRAM TORTUR` arrives as the single name
   `PROGRAMTORTUR` and `DO 20 I = 1, 10` is indistinguishable from
   `DO20I = 1.10` at this level — the comma is what separates them, and
   only the parser can see it. Consequence for milestone 3: **the
   parser must classify statements by prefix-matching keywords against
   the assembled statement text, then re-init the scanner past the
   keyword.** That is how real F77 front ends work; it is not a
   workaround.
3. **Vertical slice to `STOP`.** ✅ **DONE 2026-08-27.** `PROGRAM`/`END`,
   `INTEGER`/`REAL`/`LOGICAL` declarations, implicit typing (I-N rule),
   assignment, mixed-mode arithmetic, all six relationals,
   `.AND.`/`.OR.`/`.NOT.`/`.EQV.`/`.NEQV.`, logical `IF`, block
   `IF`/`ELSE IF`/`ELSE`/`ENDIF`, `DO` (including negative and
   zero-trip), `CONTINUE`, `GOTO`, `STOP n`.

   One pass, syntax-directed, lowering straight to HIR — no AST. F77
   requires declarations before executable statements, so nothing needs
   a second look, and the copied backend's SSA/optimizer does the work
   an AST would have been built to enable. Forward `GOTO` works because
   HIR blocks can be created before they are filled.

   The PROGRAM unit is emitted as `main`, so the existing crt0 turns its
   return value into the process exit status. Linking with `--mmio` +
   `libc_mmio` is what propagates that status out of the emulator, which
   is how `STOP n` is checked — no Fortran I/O runtime needed yet.

   `DO` uses the standard trip count, `MAX(0, (m2-m1+m3)/m3)`, computed
   once at entry, rather than re-testing the variable. That is what
   makes a negative or run-time-signed step work without knowing its
   sign at compile time, and it gives correct zero-trip behaviour.

   Statement classification lives in `f77_classify()` and is the part
   that could not be done in the lexer: `IF(` matches balanced parens
   and then looks at what follows (`THEN` → block IF, `=` → assignment
   to array element `IF(...)`, else logical IF); `DO` requires a
   top-level `=` *and* a top-level `,` after it, which is the entire
   difference between `DO 20 I = 1, 10` and `DO20I = 1.10`.

   Gated differentially against the oracle by `tests/f77/slice[1-3].f`,
   which are **self-checking**: each wrong answer exits with its own
   code and success falls through to `STOP 0`, so the exit status
   depends on the computation rather than on a literal. Verified
   non-vacuous by mutation — swapping integer `+`/`-` in the compiler
   fails all three.

   **`DOUBLE PRECISION` added the same day.** SLOW-32 native does not
   define `S12CC_NATIVE_F64`, so a double is a PAIR of 32-bit values:
   the lo word is the expression's value and the hi word travels beside
   it in `ex_hi`, which must be captured immediately after each
   subexpression because the next emission overwrites it. Operations are
   emitted as calls to the `__fp64_*` helpers — and every one of the 14
   is recognised by the backend (`hcg_fp64_kind`/`hcg_fp64_emit`) and
   replaced with inline hardware FP, so **no call survives to the
   assembly**: `slice4.f` emits `fadd.d`, `fsub.d`, `fmul.d`, `fdiv.d`,
   `fneg.d`, `feq.d`, `flt.d`, `fle.d` and `fcvt.d`, with zero `__fp64`
   references left. Covers mixed-mode promotion (DOUBLE beats REAL beats
   INTEGER), both conversion directions, and DOUBLE relationals — only
   eq/lt/le exist in hardware, so `>`, `>=` and `/=` are built by
   swapping operands or inverting.

   Not yet: `**`, arrays, subprograms, and `COMPLEX`.

4. **The FORMAT engine.** The sleeper: `WRITE(6,100)` /
   `FORMAT(1X,F10.4)` is an interpreted mini-language at runtime and is
   the single largest component — most of what libf2c is. Every route
   to F77 pays this; there is no shortcut.
5. **Subprograms.** `SUBROUTINE`/`FUNCTION`, by-reference arguments,
   `COMMON`, `SAVE`, `EXTERNAL`.
6. **Arrays.** Column-major, 1-based, arbitrary lower bounds, adjustable
   dimensions in dummy arguments.
7. **The app that justifies it.** Per the desk's own rule, the language
   earns its place through a program, not a test suite: a LINPACK-shaped
   kernel plus a plotter routine on the tube.

## Testing

The project's standing method is a differential oracle (LLVM for
stage08, gforth for stage01). There was no Fortran on this host, so one
was built.

**`slow32:fortran-oracle`** (`Dockerfile.fortran-oracle`, Alpine 3.21 +
GNU Fortran 14.2.0, ~218 MB) compiles and runs a fixed-form program and
prints what it printed. `fortran/tests/oracle.sh` wraps it: it mounts
only the source file's directory, auto-detects podman/docker, and
passes the program's exit status through.

It is deliberately a **separate image**. `slow32:toolchain` and
`slow32:emulator` are what `~/builder` builds and the ECR mirror
serves, and neither needs gfortran; nothing in this image is required
to build or run SLOW-32 software.

Oracle settings, which are part of the comparison and should not drift:

    -std=legacy -ffixed-form -fno-range-check

Two behaviours pinned while setting it up, because tests depend on them:

- `STOP n` writes its message to **stderr** and the value to the **exit
  status**. stdout therefore stays clean for diffing, and milestone 3's
  slice can be checked by exit code alone with no I/O runtime.
- Podman on macOS runs in a VM that shares home paths but **not**
  `/tmp`, so oracle sources must live under `$HOME` — `tests/f77/`
  does.

Gate 3 in `tests/run-tests.sh` runs every `tests/f77/*.f` under both the
oracle and (once it exists) our compiler, requiring stdout and exit
status to match. It reports SKIP until milestone 3 lands the driver.

## What not to do

- No native x64/a64 backends (ruling 2). slow32-dbt is the path.
- No symlinks back into `selfhost/` (ruling 1).
- No COMMON support in `s32-ld`; merge compiler-side.
- No new HIR opcodes without a demonstrated need — the existing set
  covers everything F77 requires, COMPLEX included (as pairs).
