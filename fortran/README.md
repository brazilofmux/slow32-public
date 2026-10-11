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

    src/f77_shim.h      the 6-field unit descriptor the backend reads
    src/f77_contract.h  the 45-symbol frontend contract (types, globals,
                        alloca registry, diagnostics)
    src/hir*.h          COPIED SLOW-32 backend (see below)
    tests/              harness; backend_slice is the load-bearing gate

## The copied backend

`src/hir.h`, `hir_ssa.h`, `hir_opt.h`, `hir_licm.h`, `hir_burg.h`,
`hir_regalloc.h` and `hir_codegen.h` are **copies** of the stage08
backend, taken at commit `849dd791` (`hir_licm.h` re-synced at
`1fd968dd` for the pruned hoisting; each file's header is authoritative). They are copies on purpose:
`selfhost/` must be free to evolve without breaking f77, so these are
deliberately NOT symlinks into `../selfhost/src` the way cc-x64 and
cc-a64 do it.

Each file carries a provenance header naming its source commit. A
re-sync is a deliberate act: re-copy, re-stamp the vintage, and run the
tests — `backend_slice` exists precisely to catch a contract break at
the moment it happens.

**Deliberate divergences from the selfhost original** (re-apply these
after any re-sync; each is marked in-place with a `DIVERGENCE` comment):

- `hir_codegen.h` — `hcg_fp64_kind`/`hcg_fp64_emit` additionally
  recognise `__fp64_sqrt` and `__fp32_sqrt`, emitting the hardware
  `FSQRT.D` / `FSQRT.S`. The C compiler reaches sqrt through
  `HI_FSQRT`, which this backend does not implement and **silently
  emits nothing for**, so without this Fortran would call a libm
  function whose entire body is one instruction. Not ported to
  stage08, where nothing emits those names.

The fp64 **aligned-pair allocation** (`ra_pair_claim`/`hcg_pair_reg`)
was prototyped here and has since been **ported upstream to
`selfhost/stage08`**, so it is no longer a divergence — the two copies
agree on it.

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
`compile.sh` pointed at the `/opt/slow32` install:

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
