# fortran — Fortran 77 for SLOW-32

Status: working F77 subset (2026-08-30). Vertical slice, arrays,
subprograms, FORMAT, `READ` (formatted and list-directed, with
`END=`), `OPEN`/`CLOSE`/`REWIND` (sequential formatted files),
`COMMON`/`SAVE`, `DATA`/`PARAMETER`, `IMPLICIT` (including `NONE`),
computed `GOTO`, arithmetic `IF`, `**` (INTEGER and real exponents),
the transcendental intrinsics (SIN/COS/TAN, ASIN/ACOS/ATAN/ATAN2,
EXP/LOG/LOG10, SINH/COSH/TANH, generic and D-specific names), and
LINPACK run on SLOW-32 and match gfortran. Still refused (honestly):
`EQUIVALENCE`, `CHARACTER`, `COMPLEX`, assigned `GOTO`, `BACKSPACE`,
`ENDFILE`, unformatted and direct-access I/O.

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
backend, taken at commit `849dd791`. They are copies on purpose:
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

## Tests

    ./tests/run-tests.sh

`backend_slice` drives the copied backend with hand-built HIR and no
frontend at all, then runs the result on the emulator. It is the test
that proves the copy is still wired correctly.
