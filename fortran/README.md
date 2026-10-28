# fortran — Fortran 77 for SLOW-32

Status: **passes NIST's FCVS 2.1, 192 of 192 (2026-10-10).**  The whole
of ANSI X3.9-1978 that the validation suite exercises: INTEGER, REAL,
DOUBLE PRECISION, LOGICAL, CHARACTER (substrings, concatenation,
assumed length, CHARACTER functions, A editing, internal files) and
COMPLEX; arrays with any bounds including adjustable ones in dummies;
SUBROUTINE/FUNCTION with by-reference arguments, alternate returns,
ENTRY, statement functions, procedures and specific intrinsics as
arguments, dummy procedures; COMMON/SAVE/EQUIVALENCE/BLOCK DATA,
DATA (with implied-DO and substrings)/PARAMETER, IMPLICIT; computed
and assigned GOTO, arithmetic IF, PAUSE; FORMAT with every F77 edit
descriptor (I/F/E/D/G/L/A, Iw.m, Ew.dEe, kP, S/SP/SS, BN/BZ, T/TL/TR,
X, H, apostrophe, slash, colon, groups), list-directed I/O laid out as
gfortran lays it out, formats held in CHARACTER entities or ASSIGNed
variables; sequential and direct-access files, formatted and
unformatted, OPEN/CLOSE/INQUIRE with their specifiers, REWIND,
BACKSPACE, ENDFILE, END=/ERR=/IOSTAT=, preconnected units as fort.N.
LINPACK and the FCVS inspection programs match gfortran's output.

Beyond F77, MIL-STD-1753 (taken from fort370): INCLUDE in its three
spellings (`'name'`, `(name)`, bare), nested eight deep, the text a
file beside the source or in the current directory, as written or in
lower case, with or without `.f`, `.for` or `.inc`; the bit functions
IAND, IOR, IEOR, NOT, ISHFT, ISHFTC, IBITS, BTEST, IBSET, IBCLR and
MVBITS, in line; and `Z'..'`/`O'..'` constants in DATA for INTEGER,
LOGICAL, REAL and DOUBLE PRECISION items.  fort370's IBM extensions
(DEFINE FILE, `u'r`, FIND, PUNCH, unquoted `Z1F`) are not taken.

The transcendentals are calls by their C libm names, on purpose:
slow32 and slow32-fast execute the Newton-series SLOW-32 code linked
from `libs32.s32a`, while slow32-dbt and qemu-tcg find those names in
the `.s32x` symbol table and substitute the host's native routines at
translation time -- the 8087 coprocessor arrangement, without the bus:
same binary everywhere, native transcendentals wherever the engine can
provide them, and the fallback ships inside the binary itself.

See [`../docs/plans/fortran77.md`](../docs/plans/fortran77.md) for the
plan, the rulings behind it, and the milestone list.

## Layout

    src/f77.c           the compiler, one translation unit including the rest
    src/f77_card.h      fixed-form card reader, statements assembled
    src/f77_lexer.rl    Ragel -G2 token scanner; f77_lexer_gen.c is the
                        generated output, checked in; gen_lexer.sh regenerates it
    src/f77_parse.h     recursive-descent parser, lowering straight to HIR
    src/f77_shim.h      the 6-field unit descriptor the backend reads
    src/f77_contract.h  the 45-symbol frontend contract (types, globals,
                        alloca registry, diagnostics)
    src/hir*.h          COPIED SLOW-32 backend (see below)
    runtime/libf77.c    the guest runtime (I/O, formats, intrinsics), built by clang
    tests/              harness; backend_slice is the load-bearing gate
    bench/              LINPACK and mandel, f77 against clang (RESULTS.md)

## The copied backend

`src/hir.h`, `hir_ssa.h`, `hir_opt.h`, `hir_licm.h`, `hir_burg.h`,
`hir_regalloc.h` and `hir_codegen.h` are **copies** of the stage08
backend. The first copy was taken at `849dd791` (2026-08-31); the
copies were re-synced on 2026-10-11 at `30e16a57`, and since then
each file is byte-identical to its stage08 original past the one-line
provenance banner (`hir.h` carries the ruling as well). They are copies
on purpose: `selfhost/` must be free to evolve without breaking f77, so
these are deliberately NOT symlinks into `../selfhost/src` the way
cc-x64 and cc-a64 do it.

A re-sync is a deliberate act: re-copy, re-stamp the banners, and run
the tests -- `backend_slice` exists precisely to catch a contract break
at the moment it happens. What the frontend owes the backend is in
`f77_contract.h` and `f77_shim.h` (the C AST fields it reads, the type
predicates, the alloca registry, the global-data tables, and inert
stand-ins for the C inliner's hooks).

**No divergences remain.** Everything prototyped here between the two
copies -- fp64 pairs allocated as a unit, per-block liveness, iterated
spilling, live-range splitting, strength reduction, the fused FP
compare-branch, the hardware `FSQRT.D`/`FSQRT.S` for `__fp64_sqrt` and
`__fp32_sqrt` -- was ported to stage08 and came back identical. The
`DIVERGENCE` comments still in the sources are history, not a to-do.
The one that opened after that (`hir_opt.h` letting CSE match two equal
integer constants within a block, `ho_cse_key`, from fort370) closed
when stage08 took the same rule; the 2026-10-27 re-sync (at `370629f9`)
left the copied headers equal to stage08's past their banners, and
`hir_alias.h` (the memory model's alias analysis) is copied with them.
stage08 has moved on since -- its opt-in profile-guided allocation, the
peeled last pass and leaf phi coalescing of `cc359989` -- and only
wrong-code fixes have come across since (`cc359989`'s two, and those
the C fuzzer `regression/run-stage08-fuzz.sh` found); the rest waits for
the next re-sync.

The 2026-10-11 re-sync is what a re-sync is for. The six weeks of
stage08 work it brought in (call-site live-range splitting, frameless
leaves, r30 as a GPR, shared spill slots, return values computed into
r1, a terminator's jump dropped when the next block is laid out next)
cost 0.9% of LINPACK's instructions and left mandel byte-identical --
and the FCVS went 192 to 189 on the first build. The two causes were a
stage08 bug the C front had never tripped (the dominator-tree children
were stale in a function with nothing to promote, and the new split
walked them: selfhost ISSUES-86) and a front-end liberty the old
backend had tolerated (statements after RETURN emitted into the block
RETURN closed). Both are fixed on both sides, and `tests/f77/entry2.f`
and `charfn1.f` hold the two shapes.

The 2026-10-27 re-sync brought in the memory model (alias analysis,
loads out of loops, cross-block dead stores) and the allocator's step 5
(phi webs, rematerialized address arithmetic, one reload per block, and
the fixes those turned up). Every Fortran-local change since the last
copy was already in stage08, so the copy is plain again. It found one
thing on the first measurement: LINPACK's DGEFA got 806,400
instructions slower because a rematerialized `LDA+1` kept being
recomputed after a later spill round put `LDA` in memory, a load and an
add at every use. Fixed in stage08 -- such an ADDI now goes to memory
with its operand -- and LINPACK ended 0.01% faster than before the
re-sync, mandel 0.08% faster, SQLite unchanged.

## fort370 is a cousin, not a twin

`~/fort370` (FORTRAN 77 for MVS 3.8j) began as this compiler and still
shares its optimizer; its wrong-code fixes come back here, and each one
gets a test in `tests/f77/` (the 2026-10-14 batch from a real program it
ran: `phiundef1`, `leafargs1`, `fmtend1`, `units1`, two of them stage08
bugs as well). What does not come back is IBM's meaning. There a
subprogram's local keeps its value from one call to the next without a
SAVE, because FORTRAN H's storage is static and old programs count on
it, so fort370 keeps such a variable in its frame slot. Here the
standard rules (ANSI X3.9-1978 17.3): a local not named in a SAVE
becomes undefined when the subprogram returns, and a program that reads
it on the next call is wrong. Both
compilers say FORTRAN 77; they serve different programs.

SLOW-32 is the only target. x86-64 and aarch64 are reached through
`slow32-dbt`, as with every other language here.

## The oracle

F77 is developed against a reference implementation, as every compiler
here is. `Dockerfile.fortran-oracle` (repo root) builds
`slow32:fortran-oracle` -- Alpine + GNU Fortran 14.2.0 -- and
`tests/oracle.sh` runs a program through it:

    podman build -t slow32:fortran-oracle -f Dockerfile.fortran-oracle .
    ./tests/oracle.sh tests/f77/sumsq.f

It is a separate image on purpose: `slow32:toolchain` and
`slow32:emulator` are what `~/builder` builds, and neither needs
gfortran. Sources must live under `$HOME` (podman's macOS VM does not
share `/tmp`).

## Building and compiling

    ./build.sh                       # out/f77 (host cc) + runtime/libf77.s32o (clang)
    ./compile.sh prog.f -o prog.s32x # f77 -> slow32asm -> s32-ld, libf77 + MMIO libc
    ../tools/dbt/slow32-dbt prog.s32x

`compile.sh`, `runtime/build.sh` and `tests/run-tests.sh` honour
`S32_F77`, `S32_LIBF77`, `S32_AS`, `S32_LD`, `S32_RT` and
`S32_RT_INCLUDE`, so an installed copy can be pointed at instead of the
tree.

## Container

`slow32:fortran` (`Dockerfile.fortran` at the tree root, FROM
`slow32:base`) carries `f77`, `libf77.s32o` and `s32f77`, which is
`compile.sh` pointed at the `/opt/slow32` install; published as
`hurriedreformist/slow32:fortran` (amd64 and arm64):

    podman run --rm -v $(pwd):/data slow32:fortran s32f77 prog.f -o prog.s32x
    podman run --rm -v $(pwd):/data slow32:fortran s32run prog.s32x

f77 and libf77 are built in a stage FROM `slow32:toolchain`; the image
itself has no C compiler and no gfortran, so the suite inside it runs in
self-check mode (every test program ends in STOP 0, so exit 0 is
required; output is not diffed against the oracle) and reports the two
host-compiler gates as SKIPPED.  That is the gate ~/builder runs before
pushing it.

## Tests

    ./tests/run-tests.sh

`backend_slice` drives the copied backend with hand-built HIR and no
frontend at all, then runs the result on the emulator. It is the test
that proves the copy is still wired correctly.  The `diff:` tests are
programs under `tests/f77/` (a sibling `.in` is stdin) whose output
must match gfortran's byte for byte; the ones named for a feature
(`character1`, `complex1`, `entry1`, `dummyproc*`, `unformatted1`,
`internal1`, `dblescape`) were written while bringing up that feature
against the FCVS and are the regression guard for it.

## The FCVS

    ./tests/run-fcvs.sh              # all 192 programs, ~10 s
    ./tests/run-fcvs.sh -v FM110     # one, verbosely

NIST's Fortran Compiler Validation System 2.1 -- 192 self-checking and
inspection programs for X3.9-1978, public domain -- is the acceptance
suite.  It is NOT in this tree: fetch `fcvs21.tar.Z` (the Wayback copy
of `itl.nist.gov/div897/ctg/suites/fcvs21.tar.Z`) into
`~/refs/fcvs/fcvs21` or point `FCVS` at it.  The harness compiles each
program through the compile.sh pipeline, runs it on slow32-dbt with
its `.DAT` as stdin, and reads the program's own run summary; the 139
programs that print values for inspection are instead diffed against
gfortran through `oracle.sh` (trailing blanks removed, gfortran's
warning blocks filtered, the PROGRAM card NIST prepended to 40 files
dropped so gfortran accepts them).  Outputs land in `/tmp/fcvs-slow32`
(`FMxxx.out`, `FMxxx.diff`, `results.tsv`).  First sort on 2026-10-10
was 41 of 192; everything the suite found is in the git log of that
day, and two of the finds were latent wrong-code bugs no test had
caught: a DOUBLE scalar written through its address (READ, CALL) kept
its old value, and a FUNCTION falling off END without RETURN returned
0 (every direct caller was inlined and never ran the body).
