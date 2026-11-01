# Clause 7 inventory, 2026-11-01

> Rerun the same evening after musl's libm went in (and `<float.h>`,
> `<fenv.h>`, `<tgmath.h>`, the scanf family): rt 638, sh 553 of 898.
> The table and lists below are the morning's survey as taken;
> `inventory.tsv` is the current answer.

Which of the identifiers Annex B lists does each library provide?  Method
and answers: [README](README.md); per identifier: `inventory.tsv`.  The
tree at d47d9f47 (both libraries' `time_t` signed and 64 bits).  Optional
headers and Annex K are not counted.

| header | identifiers | clang runtime provides | stage08 library provides |
|---|---|---|---|
| `<assert.h>` | 3 | 2 | 2 |
| `<ctype.h>` | 14 | 14 | 14 |
| `<errno.h>` | 4 | 4 | 4 |
| `<fenv.h>` | 24 | 16 | 0 |
| `<float.h>` | 40 | 29 | 0 |
| `<inttypes.h>` | 161 | 17 | 27 |
| `<iso646.h>` | 11 | 11 | 0 |
| `<limits.h>` | 19 | 19 | 18 |
| `<locale.h>` | 10 | 10 | 0 |
| `<math.h>` | 203 | 77 | 63 |
| `<setjmp.h>` | 3 | 3 | 3 |
| `<signal.h>` | 11 | 11 | 11 |
| `<stdalign.h>` | 2 | 2 | 2 |
| `<stdarg.h>` | 5 | 5 | 1 |
| `<stdbool.h>` | 4 | 4 | 4 |
| `<stddef.h>` | 5 | 5 | 5 |
| `<stdint.h>` | 65 | 65 | 14 |
| `<stdio.h>` | 64 | 59 | 56 |
| `<stdlib.h>` | 49 | 42 | 36 |
| `<stdnoreturn.h>` | 1 | 1 | 1 |
| `<string.h>` | 24 | 24 | 24 |
| `<tgmath.h>` | 60 | 0 | 0 |
| `<time.h>` | 18 | 16 | 16 |
| `<uchar.h>` | 8 | 0 | 0 |
| `<wchar.h>` | 68 | 9 | 0 |
| `<wctype.h>` | 22 | 0 | 0 |
| **all** | **898** | **445** | **301** |

`<stdint.h>` and `<inttypes.h>` count each `N` pattern four times (8,
16, 32, 64), and `<inttypes.h>` is mostly its `PRI`/`SCN` macros, so the
raw numbers overweight them.  `<tgmath.h>`, `<wchar.h>`, `<wctype.h>` and
`<uchar.h>` are required of a hosted implementation, not optional.

## Both libraries

1. **No `<tgmath.h>`, `<wctype.h>` or `<uchar.h>`, and next to nothing of
   `<wchar.h>`** (the runtime has 9 of its 68 names, the selfhost library
   none).  Wide characters are the largest single gap.  Multibyte
   conversion in `<stdlib.h>` (`mblen`, `mbtowc`, `wctomb`, `mbstowcs`,
   `wcstombs`) is missing with them, as are `wcstoimax`/`wcstoumax`.
2. **`<math.h>` stops at C89 plus a little.**  Missing in both: the inverse
   hyperbolics, `exp2`, `expm1`, `log1p`, `logb`, `ilogb`, `scalbn`,
   `scalbln`, `hypot`, `erf`, `erfc`, `lgamma`, `tgamma`, `nearbyint`,
   `rint`, `lrint`, `llrint`, `lround`, `llround`, `remainder`, `remquo`,
   `nan`, `nextafter`, `nexttoward` -- each with its `f` and `l` forms --
   and `float_t`, `double_t`, `math_errhandling`, `MATH_ERRNO`,
   `MATH_ERREXCEPT`, `FP_ILOGB0`, `FP_ILOGBNAN`, the `FP_FAST_FMA*` and
   the six comparison macros (`isgreater` ... `isunordered`, 7.12.14).
3. **`<inttypes.h>`:** `imaxabs`, `imaxdiv`, `imaxdiv_t`, `strtoimax`,
   `strtoumax` missing.
4. **`<assert.h>` has no `static_assert`** (7.2p3: the macro for
   `_Static_assert`).
5. **`<stdio.h>`:** no `vscanf`/`vfscanf`, no `FOPEN_MAX`.  `scanf` and
   `fscanf` are declared by the runtime but not defined (only `sscanf`
   is); the selfhost library has neither.
6. **`<stdlib.h>`:** no `quick_exit`/`at_quick_exit`.
7. **`<time.h>`:** no `timespec_get`/`TIME_UTC`.

## The clang runtime only

8. **It does not say the optional parts are absent.**  Neither
   `__STDC_NO_COMPLEX__`, `__STDC_NO_ATOMICS__` nor `__STDC_NO_THREADS__`
   is defined for a clang build, so by 6.10.8.3 the implementation claims
   complex arithmetic, atomics and threads -- and `<complex.h>`'s
   functions and `<threads.h>` are not there.  (stage08 defines all three.)
9. **`<float.h>` is the runtime's own** (it shadows clang's) and lacks
   C11's additions: `FLT_EVAL_METHOD`, `DECIMAL_DIG`, `*_DECIMAL_DIG`,
   `*_HAS_SUBNORM`, `*_TRUE_MIN`.
10. **`<inttypes.h>` (also the runtime's own) has only the plain
    `PRI` macros**: no `SCN` macros at all, and no `LEAST`/`FAST` forms.
11. `<fenv.h>`: no `FE_*` exception or rounding-direction macros besides
    `FE_TONEAREST`.  That may be conforming -- 7.6p6-p8 define each only
    if the implementation supports it -- step 2 decides.

## The selfhost library only

12. **No `<float.h>`, `<iso646.h>`, `<locale.h>` or `<fenv.h>` at all**,
    and no `MB_LEN_MAX` in `<limits.h>`.
13. **`<stdint.h>` lacks the `least` and `fast` types** and their limits,
    and `INTPTR_*`, `PTRDIFF_*`, `SIZE_MAX`, `SIG_ATOMIC_*`, `WCHAR_*`,
    `WINT_*`.
14. **`va_start`, `va_arg`, `va_end`, `va_copy` are parser intrinsics,
    not macros** (7.16.1 says macros): `#ifdef va_arg` is false, and
    `va_copy` does not exist.
15. **`<math.h>`** additionally lacks `HUGE_VAL*`, `INFINITY`, `NAN`, the
    `FP_*` classification macros, `fpclassify`, `isnormal`, `signbit`
    and `fma`.
16. **`<stdlib.h>`** additionally lacks `strtof`, `strtold`, `lldiv`,
    `lldiv_t`, `aligned_alloc`, `MB_CUR_MAX`; **`<stdio.h>`** `tmpnam`,
    `TMP_MAX`, `L_tmpnam`.

`FILE` is an incomplete type in the selfhost library; C11 6.2.5p1 counts
incomplete types as object types, so that is allowed (musl does the
same).
