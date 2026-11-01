# C11 clause 7 coverage: both libraries, 2026-11-01

The first behaviour survey: 339 probes over clause 7, every probe built
against both libraries and judged against what macOS's and glibc's
libraries both print (or a recorded ruling).  Totals from one run of
`tools/probe.py`, recorded in `verdicts.txt`:

| library | PASS | DIFFER | NOBUILD |
|---|---|---|---|
| rt: clang + `runtime/` | 247 | 49 | 43 |
| sh: stage08 cc + the selfhost library | 209 | 48 | 82 |

| page | probes | rt PASS / DIFFER / NOBUILD | sh PASS / DIFFER / NOBUILD |
|---|---|---|---|
| [7.1-7.20](c11-7.1-7.20.md) small headers | 87 | 70 / 6 / 11 | 53 / 9 / 25 |
| [7.6, 7.12, 7.25](c11-7.6-7.12-7.25.md) mathematics | 54 | 26 / 12 / 16 | 15 / 12 / 27 |
| [7.11, 7.14, 7.24](c11-7.11-7.14-7.24.md) strings, locale, signals | 41 | 41 / 0 / 0 | 33 / 1 / 7 |
| [7.21](c11-7.21.md) input/output | 62 | 36 / 23 / 3 | 40 / 18 / 4 |
| [7.22](c11-7.22.md) general utilities | 55 | 49 / 3 / 3 | 43 / 3 / 9 |
| [7.27-7.30](c11-7.27-7.30.md) time, wide characters | 40 | 25 / 5 / 10 | 25 / 5 / 10 |

No probe is ORACLE-SPLIT or TIMEOUT.  Much of the code is shared
(`printf_enhanced.c`, `sscanf.c`, `dtoa.c`, `math_soft.c`, `time_std.c`
are built into both), so one defect usually shows in both columns.

## The work, by root cause

Ranked by harm: wrong answers in programs that are correct first.

**Compilers (found here, belong to clause 6)**
1. A structure passed through `...` cannot be fetched with `va_arg` --
   both compilers, the LLVM backend included (7.16.1.1).
2. stage08's `va_start` lands on the wrong argument after a `long long`
   or `double` last parameter, or nine or more named ones (7.16.1.4).
3. stage08 `offsetof` refuses `s.b` and `a[3]` designators (7.19p3).
4. stage08 converts the constant `1e23` to the other neighbouring double
   than `strtod` does.  6.4.4.2p3 allows either, but 6.4.4.2p5
   recommends they agree.

**Formatted I/O (shared `sscanf.c`, `printf_enhanced.c`)**
5. `sscanf`: no `%[`, never returns EOF, `%hhd`/`%hd` store a whole int
   (memory corruption), no `j z t L`, `%p`, `%a`, no float field width,
   a prefix that is not a matching sequence is converted.  No `scanf`,
   `fscanf`, `vscanf`, `vfscanf` at all.
6. `printf`: `hh`/`h` ignored, no `%n`, no `%a` (which then misaligns
   every later argument), `#x` decides on the prefix after padding,
   `%Np` zero-fills, `%ls` prints one character.

**Mathematics (shared `math_soft.c`)**
7. `round` is `floor(x + 0.5)`; `modf` loses negative zeros; `pow` of a
   negative base fails for integral exponents beyond int, and loses
   exact tiny results; `exp`/`sinh`/`cosh` overflow below DBL_MAX's
   logarithm; `log(+inf)` never returns.  Large-argument trigonometry is
   inaccurate (quality, 5.2.4.2.2p6).
8. No `math_errhandling` and no `errno` from math functions (7.12p9),
   and the C99 additions are missing (inventory).  The classification
   macros are functions.

**Streams**
9. Both: end-of-file is not sticky (7.21.7.1p3); `fopen` ignores `x`.
10. rt: `fputc` returns `c` unconverted; a second `ungetc` overwrites
    the first; `freopen` of stdin and `feof(stdin)` bypass the stream.

**Numeric conversion**
11. rt `strtof` rounds twice and never sets ERANGE; rt hex `strtod`
    rounding past DBL_MAX misses ERANGE; sh `strtod` never sets ERANGE
    and refuses `NAN(...)` (its `dtoa.c` is built `-DNO_ERRNO
    -DNo_Hex_NaN`).

**Headers and types**
12. sh `PRI*PTR` are the 64-bit forms on a 32-bit target (shifts every
    later printf argument); sh `LONG_*` are int-typed; sh `int8_t` is
    plain char; rt `wchar_t` is int where clang makes wide literals
    unsigned; `assert` re-inclusion ignores a changed NDEBUG; rt's
    assertion message goes to stdout; neither names the function.
13. Missing headers and families are the inventory's list
    ([inventory.md](inventory.md)).

**Smaller**
14. sh `strncat` and `calloc` take `int` sizes: a huge `n` appends
    nothing, `calloc(SIZE_MAX, SIZE_MAX)` returns one byte.
15. `difftime` overflows in long long; `mktime` ignores a `tm_isdst`
    that disagrees with the zone and does not fail when the year
    overflows; `strftime %y %g %C` for years at or before 0.

## Rulings to confirm

The survey recorded eight `.expect` files.  Two depart from both host
libraries and want a decision:

- `7.27.3.5-strftime-zone-isdst`: by 7.27.3.5p3 `%z` follows
  `tm_isdst`; glibc and both our libraries follow `tm_gmtoff` (POSIX).
  Delete the `.expect` to rule for POSIX.
- `7.21.6.2-sscanf-prefix-not-match`: `"25exp"` with `%lf` is a
  matching failure by 7.21.6.2p9-10; both hosts convert the prefix.
