# runtime/libm: the math library, musl's

Both C libraries -- the clang runtime (`runtime/`, `runtime/Makefile`) and
the self-hosted one (`selfhost/stage08/build-s12cc.sh`) -- build their
`<math.h>` from this directory: musl's libm, taken verbatim.  `<complex.h>`'s
functions are musl's too (`musl/src/complex`); the clang runtime builds
them, and the self-hosted library will once stage08 compiles `_Complex`.

| | |
|---|---|
| release | musl 1.2.6 (2026-03-20) |
| tarball | `musl-1.2.6.tar.gz`, sha256 `d585fd3b613c66151fc3249e8ed44f77020cb5e6c1e635a616d3f9f82460512a` |
| signature | good, key `8364 8929 0BB6 B70F 99FF DA05 56BC DB59 3020 450F` ("musl libc &lt;musl@libc.org&gt;", from musl.libc.org/musl.pub) |
| taken | `src/math/*.c`, `src/math/*.h` (the generic sources, not the per-architecture ones), `src/complex/*.c`, `src/internal/libm.h`, `src/internal/complex_impl.h`, `arch/generic/fp_arch.h`, `COPYRIGHT` |
| licence | MIT for musl; the files from Sun, FreeBSD's msun authors and Arm carry their own permissive notices, kept in each file -- see `musl/COPYRIGHT` |

Nothing under `musl/` is edited.  Everything SLOW-32 needs is beside it:

| file | what it is |
|---|---|
| `compat.h` | put ahead of every musl source: defines musl's private `hidden` and `weak_alias` (empty: a static library hides nothing, and the aliases only made non-C11 extras), declares the non-C11 functions musl's sources call one from another, and includes `rename.h` |
| `include/` | the private headers musl's sources include: `endian.h` (little-endian), `features.h`, `atomic.h` (only `a_clz_64`, for `fma.c`) |
| `gen.py` | writes the three below from `musl/src/math` and `runtime/include/math.h`; rerun after any update |
| `complex_sources.txt` | the `<complex.h>` files, all of `musl/src/complex`, built as objects `mc_<file>` |
| `sources.txt` | the musl files both libraries build: all but `sqrt.c`, `sqrtf.c`, `sqrt_data.c` (SLOW-32 has a square-root instruction: `runtime/math_hw.c`, `selfhost/stage08/libc/math_hw.c`).  The clang runtime also leaves out `fabs.c`/`fabsf.c` for its `FABS` instruction |
| `rename.h` | each C11 function musl defines, as `__musl_<name>`: musl's functions, and its calls among them, carry no errno handling |
| `errno.c` | the public functions.  musl reports errors through floating-point exception flags, which SLOW-32 does not have, so `math_errhandling` is `MATH_ERRNO`: each wrapper sets `EDOM` when a NaN comes back from arguments that are not NaN (a domain error) and `ERANGE` when an infinity comes back from finite ones (a pole error or overflow).  Underflow sets nothing (7.12.1p6 lets it) |
| `legacy.c` | `isnan`, `isinf`, `isfinite` as functions, for objects built against the old header (C makes them macros) |

Each musl file is compiled through a stub of two lines --
`#include "compat.h"` and `#include "musl/src/math/<file>"` -- so the source
stays as released.  Objects are named `m_<file>`: an archive member is
known by its base name, and libm's `errno.c` would otherwise displace the
runtime's.

`<math.h>`, `<float.h>`, `<fenv.h>` and `<tgmath.h>` are one file each,
the same in `runtime/include` and `selfhost/stage08/include`, and so will
`<complex.h>` be.  The prototypes in `<math.h>` and `<complex.h>` are
musl's.  The complex multiplication and division the compilers call
(`__muldc3` and the rest) are `runtime/complex_rt.c`, not musl's (musl
leaves them to the compiler's support library).

The DBT's `math_intercepts` (tools/dbt/dbt.c) still swap the functions it
knows for the host's, by name; the reference interpreter and the other
engines run musl's.

## Updating

    runtime/libm/update-musl.sh 1.2.7

downloads the release and its signature from musl.libc.org, verifies the
signature against the key above, replaces `musl/` with the new release's
files, reports which sources came and went, and reruns `gen.py`.  Then
update the table above, rebuild both libraries, and run the gates:
`regression/libc-conformance/tools/probe.py --check` first, then the
usual set.
