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
- Those soft routines are a **fallback**, not the main path.
- The DBTs offer native f32 and f64 arithmetic.

### Which universe this lives in

`fortran/` is in the tree's **ordinary** universe, not `selfhost/`'s
closed one, and the two play different games:

- **`selfhost/`** bottoms out at the ~900-line `s32-emu.c`. Anything it
  needs, it must build itself. Nothing may come from outside.
- **Everything else**, including here, is two-sided: the *front* may use
  anything on the host to target SLOW-32, and the *emulators* may use
  anything in their environment — possibly a different machine — to run
  SLOW-32 code.

So `slow32-dbt` linking host libm (`LDFLAGS = -lm`) and intercepting
~37 math symbols found in the guest's symbol table is **sanctioned**:
that is the emulator using its environment, by design. `sbasic.s32x`
carries `sqrt`, `atan2` and `floor` and runs them on the host under the
DBT, legitimately. It is a benchmarking caveat — a DBT run of
math-heavy code is not timing guest instructions — not a cheat.

Two consequences for f77, both settled by this:

- Using host `strtod` and host doubles to convert literals is fine and
  permanent. f77 is a cross-compiler in the ordinary universe and is
  not required to self-host, so it needs none of the machinery stage08
  built to avoid exactly that (`lex_p10`/`lex_p5`, Dekker `twoProd`).
- Lowering an intrinsic to a guest libm call will be a legitimate
  option when `EXP`, `LOG`, `ATAN2`, the trig functions and
  real-exponent `**` arrive.

As it happens f77 needs **no math libcall at all today**: every FP
operation it emits is a SLOW-32 hardware instruction — `fadd.d`,
`fsub.d`, `fmul.d`, `fdiv.d`, `fneg.d`, `feq.d`, `flt.d`, `fle.d`,
`fcvt.*`, `fsqrt.d`, `fsqrt.s`. The LINPACK binary's only symbols are
`main`, `DAXPY`, `DGEFA` and its own routines, and it gives an
identical answer under `slow32`, `slow32-fast` and `slow32-dbt`. The
harness reports this as a code-quality fact (`math-libcalls`) rather
than enforcing it, so that adding the missing transcendentals is not
blocked by a rule that was never this directory's.

**Consequence for the intrinsics still to come:** `**` with a real
exponent, `ATAN2`, `EXP`, `LOG` and the trig functions have no SLOW-32
instruction behind them, so each will be either inline guest code or a
call into the SLOW-32 libc — the latter being legitimate here (see
"Which universe this lives in").

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
6. **Arrays.** ✅ **MOSTLY DONE 2026-08-27.** Column-major, 1-based,
   arbitrary lower bounds (`R(0:9)`, `A(-5:5)`), up to rank 7, elements
   of any type including DOUBLE PRECISION, the `DIMENSION` statement,
   and `REAL*8`-style length specifiers. Element offset is
   `(s1-lo1) + (s2-lo2)*n1 + ...` — first subscript fastest, the
   opposite of C — and constant subscripts fold away completely.

   **Caveat, recorded rather than glossed:** `tests/f77/slice5.f` does
   NOT prove the layout. It writes and reads through the same
   subscripts, so a row-major implementation would pass it identically.
   Column-major was verified by *inspection* instead — `M(2,3)` in an
   `INTEGER M(3,4)` compiles to a `+28` byte offset, i.e. element 7,
   which is `(2-1)+(3-1)*3`; row-major would have been element 6. A
   real black-box test needs the layout to be observable through a
   second view of the same storage. **That test is now delivered**:
   `slice6.f` passes an `INTEGER M(3,4)` to a subroutine that receives
   it as `A(12)`, making the storage order observable. Proven to have
   teeth by mutation — under a row-major mutant `slice6` FAILS while
   `slice5` still PASSES, confirming exactly the blindness described
   here.

   Not yet: adjustable dimensions in dummy arguments, which come with
   subprograms.
7. **The app that justifies it.** ✅ **LINPACK RUNS 2026-08-27.**
   `tests/f77/linpack.f` is the real thing — `DGEFA`, `DGESL`, `DAXPY`,
   `DSCAL`, `DDOT`, `IDAMAX` in the shapes they actually take: adjustable
   dimensions `A(LDA,1)`, by-reference arguments, `DABS`/`DMAX1`,
   column-major traversal, and the pivot search. It factors a
   diagonally-dominant matrix, solves `A*x = b` for a right-hand side
   whose exact solution is all ones, and checks the residual — matching
   gfortran exactly.

   Getting there needed three things, all now done:

   - **Adjustable dimensions.** `A(LDA,1)` inside a subprogram, where
     `LDA` is itself a dummy argument, so the column stride is a
     run-time value. The stride stays a compile-time constant until the
     first run-time extent appears and becomes a value from then on; the
     last dimension's extent is never needed, which is why both
     `A(LDA,1)` and `A(LDA,*)` work.
   - **Intrinsics.** `ABS`/`IABS`/`DABS` (sign-bit clear for FP, the
     branchless `(x^(x>>31))-(x>>31)` for integers), the `MAX`/`MIN`
     families n-ary, `MOD`, `SIGN`, the conversions, and `SQRT`/`DSQRT`.
     `MAX`/`MIN` and `SIGN` select branchlessly via
     `b ^ ((a^b) & -cond)` — SLOW-32 has no conditional move, and a
     branch mid-expression would mean splitting the block.
   - **A real bug in argument passing.** `f77_actual_addr` took any
     leading NAME as the whole actual, so `IDAMAX(N-K+1, A(K,K), 1)`
     passed `N`'s address and left `-K+1` unconsumed. It survived
     earlier tests only because their actuals happened to start with a
     digit. Now a text-level lookahead over the assembled statement
     decides whether a NAME (plus an optional balanced subscript list)
     really is the entire argument, which avoids parse-and-rewind
     un-emitting HIR that subscripts had already produced.

   Still owed for the *benchmark* proper rather than the kernel: the
   FORMAT engine, for printing the timing and residual table.


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
