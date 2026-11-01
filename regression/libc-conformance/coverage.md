# C11 clause 7 coverage: both libraries, 2026-11-01

The first behaviour survey: 339 probes over clause 7, every probe built
against both libraries and judged against what macOS's and glibc's
libraries both print (or a recorded ruling).  Totals as recorded in `verdicts.txt`
(after the `%z` ruling):

| library | PASS | DIFFER | NOBUILD |
|---|---|---|---|
| rt: clang + `runtime/` | 248 | 48 | 43 |
| sh: stage08 cc + the selfhost library | 210 | 47 | 82 |

After the fixes below (still 2026-11-01), `verdicts.txt` holds 346
probes -- four UTF-8 ones and three for `<complex.h>` added -- and every
one is PASS in both libraries.

| page | probes | rt PASS / DIFFER / NOBUILD | sh PASS / DIFFER / NOBUILD |
|---|---|---|---|
| [7.1-7.20](c11-7.1-7.20.md) small headers | 87 | 70 / 6 / 11 | 53 / 9 / 25 |
| [7.6, 7.12, 7.25](c11-7.6-7.12-7.25.md) mathematics | 54 | 26 / 12 / 16 | 15 / 12 / 27 |
| [7.11, 7.14, 7.24](c11-7.11-7.14-7.24.md) strings, locale, signals | 41 | 41 / 0 / 0 | 33 / 1 / 7 |
| [7.21](c11-7.21.md) input/output | 62 | 36 / 23 / 3 | 40 / 18 / 4 |
| [7.22](c11-7.22.md) general utilities | 55 | 49 / 3 / 3 | 43 / 3 / 9 |
| [7.27-7.30](c11-7.27-7.30.md) time, wide characters | 40 | 26 / 4 / 10 | 26 / 4 / 10 |

No probe is ORACLE-SPLIT or TIMEOUT.  Much of the code is shared
(`printf_enhanced.c`, `sscanf.c`, `dtoa.c`, `math_soft.c`, `time_std.c`
are built into both), so one defect usually shows in both columns.

## The work, by root cause

Ranked by harm: wrong answers in programs that are correct first.

**Compilers (found here, belong to clause 6)**
1. *Fixed 2026-11-01.* A structure passed through `...` could not be
   fetched with `va_arg` -- both compilers, the LLVM backend included
   (7.16.1.1).  Now it travels as the address of the caller's copy in
   the next slot, in both compilers (interop-gated both ways).
2. *Fixed 2026-11-01.* stage08's `va_start` landed on the wrong argument
   after a `long long` or `double` last parameter, or nine or more
   named words (7.16.1.4).
3. *Fixed 2026-11-01.* stage08 `offsetof` refused `s.b` and `a[3]`
   designators (7.19p3).
4. *Deferred (quality).* stage08 converts the constant `1e23` to the
   other neighbouring double than `strtod` does.  6.4.4.2p3 allows
   either, but 6.4.4.2p5 recommends they agree.  The lexer's
   double-double conversion is documented exact except within ~2^-50
   ulp of a halfway point, and 1e23 is such a point; exactness wants a
   big-integer tiebreak in the lexer (calling strtod is not open: the
   first bootstrap generation has none, and stage07 cannot return a
   double).

**Formatted I/O (shared `sscanf.c`, `printf_enhanced.c`)**
5. *Fixed 2026-11-01.* `sscanf` had no `%[`, never returned EOF,
   stored a whole int for `%hhd`/`%hd`, knew no `j z t L`, `%p`, `%a`,
   ignored float field widths and converted prefixes that are not
   matching sequences; there was no `scanf`, `fscanf`, `vscanf` or
   `vfscanf`.  `runtime/sscanf.c` is now one scanner over a string or
   a stream, with every conversion and length modifier.
6. *Fixed 2026-11-01.* `printf` ignored `hh`/`h`, had no `%n` and no
   `%a` (which misaligned every later argument), decided `#x`'s prefix
   after padding, zero-filled `%Np`, printed one character of `%ls`.

**Mathematics** -- *fixed 2026-11-01: the library is musl's libm*
(runtime/libm, MIT, verbatim; both libraries build it)
7. `math_soft.c` is gone: `round` was `floor(x + 0.5)`, `modf` lost
   negative zeros, `pow` failed for negative bases with large integral
   exponents and lost exact tiny results, `exp` overflowed early,
   `log(+inf)` never returned, and large-argument trigonometry was
   inaccurate.
8. `math_errhandling` is `MATH_ERRNO`, and errno is set
   (runtime/libm/errno.c); every C99 function is there, `float` and
   `long double` forms included; the classification and comparison
   macros are macros; `<tgmath.h>`, `<float.h>` and `<fenv.h>` exist in
   both libraries.  Building `<tgmath.h>` found a stage08 bug: a call
   whose callee is an expression designating a function -- `(f)(x)`,
   `_Generic(...)(x)` -- took the result as int and left its arguments
   unconverted (fixed in the same commit).

**Streams** -- *fixed 2026-11-01*
9. Both: the end-of-file indicator is sticky now (7.21.7.1p3); `fopen`
   takes `x` (the host interface gained an EXCL open flag, docs/SPEC.md
   §8); `FOPEN_MAX` is 64 in both, and the selfhost library's stream
   pool and descriptor table grew to hold that many.
10. rt: `fputc`/`putchar` return the byte as unsigned char; a second
    `ungetc` fails instead of replacing the first; `getchar` is
    `fgetc(stdin)`, so `freopen` of stdin, `ungetc` on it and
    `feof(stdin)` all hold.  sh gained `tmpnam`, `L_tmpnam`, `TMP_MAX`.

**Numeric conversion** -- *fixed 2026-11-01*
11. `strtof` (runtime/strtof.c, both libraries) rounds once: on the one
    case "(float)strtod" can get wrong -- the double lands on a midpoint
    between two floats -- it compares the input's digits with the
    midpoint exactly; ERANGE on overflow and underflow.  A 3000-input
    differential against the host (regression/libc-tests/strtof_hard)
    finds no difference; the old runtime differed 540 times.  dtoa.c's
    gethex compared a carry past DBL_MAX with Emax, not emax (one
    token).  The selfhost library's dtoa is built with errno and
    NAN(...) at last, and it has strtof and strtold.

**Headers and types** -- *fixed 2026-11-01*
12. `<assert.h>`, `<stdint.h>`, `<inttypes.h>` are one file each for both
    libraries: assert redefined at every inclusion by NDEBUG, naming the
    function, on stderr (runtime/assert.c), and `static_assert`; every
    least/fast/pointer/size limit and `INTn_C`, `int8_t` signed char;
    every PRI/SCN macro, PTR ones by pointer size; `imaxabs`, `imaxdiv`,
    `strtoimax`, `strtoumax` (runtime/inttypes.c).  sh: `LONG_*` typed
    long, `MB_LEN_MAX`, the `va_*` macros, `<iso646.h>`.  rt: `wchar_t`
    and `wint_t` unsigned int, as clang's `L'x'` is.  clang predefines
    `__STDC_NO_COMPLEX__` and `__STDC_NO_THREADS__` (not atomics, which
    work).
13. Missing headers and families are the inventory's list
    ([inventory.md](inventory.md)).

**Smaller** -- *fixed 2026-11-01*
14. sh `memcmp`, `memchr`, `strncat`, `strncpy` and `calloc` take their
    counts unsigned (size_t); calloc's overflow test is unsigned.
15. `difftime` subtracts unsigned (no overflow); `mktime` reads a
    `tm_isdst` the zone disagrees with by that kind's offset, and is -1
    when the normalized year does not fit; `%C` truncates and `%y`/`%g`
    are the last two digits for years at or below 0 (ruled 2026-11-01:
    the standard's "truncated", not glibc's floor).  Also new: sh
    `aligned_alloc`, `lldiv`, `MB_CUR_MAX`, a "C"-only `<locale.h>`;
    both `quick_exit`/`at_quick_exit` and `timespec_get`/`TIME_UTC`.

**Wide characters** -- *fixed 2026-11-01*
16. `<wchar.h>`, `<wctype.h>` and `<uchar.h>` are one file each for both
    libraries, and every function in them exists in both, from shared
    sources: `runtime/wchar.c` (the conversions, `<stdlib.h>`'s five,
    `<uchar.h>`'s four), `wctype.c`, `wcs.c` (the wide strings,
    `wcstod`-family through the narrow ones, `wcsftime`, `wcscoll`,
    `wcsxfrm`), `wstdio.c` (fwide, wide character I/O, the wprintf and
    wscanf families over the narrow engines' wide modes).  LC_CTYPE says
    the encoding: single bytes in "C"/"POSIX", UTF-8 in "C.UTF-8" (new,
    in both libraries) and every named locale; `MB_CUR_MAX` follows it.
    The Unicode tables are generated from libutf's Unicode 16 data and
    equal glibc's C.UTF-8 at every code point.  Four probes run in a
    UTF-8 locale.  Narrow `printf`'s `%ls`/`%lc` and `scanf`'s `%l` forms
    convert properly now (they were one byte per character).

**Complex** -- *done 2026-11-01*
17. `<complex.h>` in both libraries: musl's 22 functions in three
    precisions, `runtime/complex_rt.c`'s `__mulsc3`/`__muldc3`/`__divsc3`/
    `__divdc3` (clang called `__divdc3` with no definition anywhere), and
    `_Complex` in stage08: a structure of its two parts to the machine, so
    clang's ABI for free, and its arithmetic rewritten by sema into real
    arithmetic on the parts, as clang does it -- the two compilers agree
    to the bit on every probe and a 48-line torture test.  Neither
    compiler predefines `__STDC_NO_COMPLEX__` any more (the cross
    compilers do).  Found on the way: stage08 folded `-(0.0)` and `-0.0f`
    in static initializers to +0 (selfhost ISSUES-107).

## Rulings

The survey recorded eight `.expect` files.  Two depart from both host
libraries; both were confirmed on 2026-11-01:

- `7.27.3.5-strftime-zone-isdst`: `%z` follows `tm_gmtoff`, as POSIX.1-2024
  specifies (C's text, read strictly, follows `tm_isdst`).  glibc's
  output is the expect file; both libraries already do this.
- `7.29.6-utf8-conversions` and `7.30-utf8-classes` (2026-11-01):
  macOS has no `<uchar.h>` and its wide classes break 7.30.2.1's own
  relations, so glibc is the oracle -- except that a five-byte UTF-8
  form, which glibc still decodes, is EILSEQ (RFC 3629; `MB_CUR_MAX`
  4).  [c11-7.27-7.30.md](c11-7.27-7.30.md) has the detail.
- `7.21.6.2-sscanf-prefix-not-match`: `"25exp"` with `%lf` is a matching
  failure, by 7.21.6.2p9-10 and the standard's own worked case; both
  hosts convert the prefix, and so do both our libraries (item 5).
