# SLOW-32 self-host toolchain runtime kit

Runtime support for the self-hosted compiler `../cc.s32x` and tools
(`../s32-as.s32x`, `../s32-ld.s32x`, `../s32-ar.s32x`), built from
`slow-32/selfhost/stage08`.

> **Source of truth.** This file is the master copy. It is deployed as
> `~/s32x/selfhost/README.md` alongside the kit — edit it here, not there.

## Interop status: stage08 and clang share one ABI

As of **2026-08-24** there is no known calling-convention divergence between
stage08 `cc` and the LLVM slow32 backend. The last two gaps — double arguments
(clang's aligned-pair convention) and struct-by-value arguments (clang's byval
convention) — are closed. Struct *return* uses the shared hidden-pointer-in-r3
sret convention.

Verified **bidirectionally** by `selfhost/stage08/run-interop-llvm.sh`, whose
gates are hard failures: int/pointer args, >8 args spilling to the stack,
`long long` register pairs with `r1:r2` return, stage08→clang callbacks,
integer varargs, doubles, struct-by-value in both directions, struct return,
byval slot layout after 8 register args, and caller-copy semantics.

Objects from the two compilers link and run together in any combination.
Confirmed locally on this kit (identical output from every row):

| Objects | crt0 | libc | |
|---|---|---|---|
| stage08 | selfhost | selfhost | ✅ |
| stage08 | LLVM | LLVM | ✅ |
| stage08 | LLVM | selfhost | ✅ |
| stage08 | selfhost | LLVM | ✅ |
| stage08 | LLVM | both archives in one link | ✅ |

Probe covered struct-by-value args, struct return, a mixed
`{char,short,int,double}` struct, interleaved `double`/`int` args crossing the
register boundary, and varargs doubles.

### The one real caveat: `__muldi3`

Clang inlines 64-bit multiply via `UMUL_LOHI`, so **`__muldi3` is not in
`runtime/libs32.s32a` or `runtime/libc_mmio.s32a`**. stage08 emits a libcall.
A stage08 object that multiplies `long long` therefore fails to link against
the LLVM archives alone:

```
Error: Undefined symbol '__muldi3'
```

`libc.s32a` in this kit provides it. When linking stage08 objects against the
**LLVM** runtime instead, add `selfhost/stage08/builtins64.s`:

```sh
slow32asm slow-32/selfhost/stage08/builtins64.s builtins64.s32o
s32-ld -o prog.s32x --mmio 64K runtime/crt0.s32o prog.s32o builtins64.s32o \
       runtime/libc_mmio.s32a runtime/libs32.s32a
```

`__divdi3`, `__udivdi3` and `__moddi3` are in both runtimes; only `__muldi3`
is one-sided.

### Floating point is hardware now

stage08 emits **HW FP instructions** (`fadd.d`, `fdiv.d`, `fcvt.d.w`,
`fcvt.w.d`), not softfloat libcalls, so `builtins_fp64.s` is no longer needed
for newly compiled code. `libc.s32a` still carries the `__fp64_*` softfloat
routines for older objects that call them.

`printf` carries full FP formatting (`%f`/`%e`/`%g`, width flags) via David
Gay's dtoa + printf_enhanced — the same sources as the clang runtime, so
formatted output is byte-identical across the two libcs.

## Contents

- `include/` — C headers; pass with `-Iinclude` (cc.s32x has no default search path)
- `crt0.s32o` — startup object; link **first**, do not put in an archive
- `libc.s32a` — archived runtime (MMIO I/O variant) built from `stage08/lib/*.s32o`

## Build + link a program

All commands run under the emulator (here `slow32` / `slow32-fast`):

```sh
SH=~/s32x/selfhost
KIT=~/s32x
slow32 $KIT/cc.s32x     -I$SH/include prog.c prog.s
slow32 $KIT/s32-as.s32x prog.s prog.s32o
slow32 $KIT/s32-ld.s32x -o prog.s32x --mmio 64K $SH/crt0.s32o prog.s32o $SH/libc.s32a
slow32 prog.s32x
```

`--mmio 64K` is required — this libc is the MMIO I/O variant. Without it
`__mmio_base` resolves to 0 and the program faults on the first write
(`Memory fault ... at 0xF0E8F900`).

## Kit vintage

No known issues in the current kit. (`getenv` was a stub returning NULL
through 2026-09-08; a kit older than that sees no environment at all.)
Several bugs have been fixed since the first kit; if your kit predates
the commit named, the bug is live.
Note which ARTIFACT carries each fix -- most are in `cc.s32x`, but the
argv fix below lives in `libc.s32a`, so a stale `libc.s32a` keeps the
bug even beside a fresh compiler.

Fixed 2026-09-09, fifth batch (`7cce2b2f`..`0b65ae29`) -- call-boundary
register allocation (GitHub issue 67) and two fp64 return bugs it exposed.
All in `cc.s32x`:

- **A double passed to a call and still live after it filled the HIR
  instruction table.**  Live-range splitting recursed onto the fp64 pair
  partner and bounced back, so six lines of C became 1.3M lines of
  assembly and the assembler died on "Instruction buffer size overflow".
  In SQLite it took out `dekkerMul2`, `kahanBabuskaNeumaierStep`,
  `absFunc` and `strftimeFunc`.  A kit built from `4ce2473c` or
  `09078277` cannot compile such a function at all -- the failure is
  loud, not silent.
- **An indirect call returning `double` was typed `int`**, so no CALLHI
  was emitted and the lo word of the `r1:r2` pair was run back through
  `fcvt.d.w` as an integer.  Silent wrong answers.  Fixed for every
  callee shape: a parameter, a typedef, a local variable, a struct member
  through `.` and `->`, a pointer returned from another call, a local or
  global array slot, and a pointer-to-function-pointer subscripted or
  dereferenced.  `long long` returns took the same path and are fixed
  with it.  This one is live in every kit before `0b65ae29`.
- **Arguments now live in `r3`-`r10`.**  Behaviour-neutral, but it moves
  code: the sqlite3 shell's `.text` and its executed instruction count
  both changed, so a kit mixing old and new objects is fine while
  size/perf numbers taken across the boundary are not comparable.

Fixed 2026-09-09, fourth batch (`e10709eb`..`88811360`):

- **A silent ABI miscompile, and the last of it.** An i64 argument on a
  call through a function pointer went as ONE word unless the pointer was
  a plain struct member -- the callee then read whatever the high
  register happened to hold.  A local, a typedef'd pointer, a global, an
  array slot (`methods[i](0)`), and a function returning a
  function-pointer typedef (`getf(0)(0)`) all took that path.  All of
  them now carry the declared signature and convert.  This is the one
  change here that can silently change what an existing program computes,
  and it is a fix in every case: the old behaviour was never right.
- **`#if` arithmetic is 64-bit.**  `#if BIG > 0` with
  `#define BIG 2147483648` was false because the value wrapped to a
  negative int; `0x80000000` likewise, and a `1000000000 * 4`
  intermediate.  Signed comparison and `#define N -5` are unchanged.

Nothing else in this batch reaches the kit: the rest is test coverage
and a Fortran front-end fix.

Fixed 2026-09-09, third batch (`2db38549`..`4a2116ae`) -- a second review
pass, and the bootstrap compiler behind it:

- **`getenv` works.** It was a stub returning NULL; a program on an older
  kit sees no environment at all.  It now goes through the GETENV request
  the host has always implemented, so `$HOME`, `$PATH` and the rest reach
  the guest.  This is the one change here that a program can notice
  without being a compiler.
- **Silent drops became diagnostics.** A `#define` body over 4096 bytes,
  a macro nesting past 64, more than 32 argument regions, a switch
  nested past the depth limit, and a BSS `.space` whose running total
  would wrap `int` in `s32-as` -- each of these used to continue with a
  wrong answer and now stops.  A short `write` of the streamed assembly
  is retried rather than silently truncating the `.s`.
- **Address constants.** `&(sym)` and `&(((T*)K)[i])` are the same
  address constants as their unparenthesised forms and now fold; the
  second is how SQLite spells `SQLITE_INT_TO_PTR` once a macro adds a
  layer of grouping.
- **`__VA_ARGS__` and the 9th macro argument** are recorded as argument
  regions, so a macro named inside them still expands.
- **Jump tables** no longer treat a Duff's device as fall-through-free;
  a case label nested inside another statement now forces the
  conservative lowering.
- **`ST_MAX_STRUCTS`** stops below `TY_PTR` -- struct 4088 and up used
  to encode as a pointer type.
- **The instruction selector** labels operand chains with an explicit
  stack instead of host recursion, bounded.
- **The bootstrap.** stage07, which compiles this compiler, left the high
  register untouched when a `long long` function returned a narrower
  expression.  Repaired in place; measured byte-identical output for the
  one file it compiles here, so nothing in this kit changed because of
  it -- it only unblocks future work.

Fixed 2026-09-08, second batch (`be9b3786`..`a1ec1606`) -- a review pass
over the SQLite work, then the defects that pass exposed:

- **Silent miscompiles.** A `char`/`short` local kept its wide value
  through `+=` and `++`/`--` (only plain assignment, return and parameter
  copy-in were narrowed).  `int x = 7; int x;` zeroed the initializer --
  C keeps it, and two initialized definitions are now a redefinition.
  Constant shifts were 32-bit, so `1LL << 32` was 1.  A dominance
  frontier wider than 32 entries dropped the rest, losing phis with no
  diagnostic; it now stops.  A nested function-pointer member,
  `T (*(*name)(args))(...)` (`sqlite3_vfs.xDlSym`), had no recorded
  signature, so an i64 argument through it went as one word.
- **Preprocessor.** `#if` treated a comment as a token, so
  `#elif !defined(__GNUC__)  /* comment */` evaluated wrong -- SQLite has
  three of those in a row selecting `SQLITE_INT_TO_PTR`.  Backslash-CRLF
  and line counting on continued directives.  A text-bodied macro in
  `#if` is now expanded as a full expression, not a primary.
- **Refused or mis-parsed inputs.** `char (*a[])` is an array of
  pointers, not of function pointers.  Unsuffixed decimal `2147483648`
  is a `long long` (but a numeric `#define` past INT_MAX still is not --
  GitHub issue 60).  `&((T*)K)[i]` folds as an address constant, which
  is how SQLite spells `SQLITE_INT_TO_PTR`; `&(sym)` with grouping
  parens still does not (GitHub issue 59).
- **Bounds.** The switch pre-scan wrote case labels past `HL_MAX_CASE`.
  `c_lexer.rl` carried pre-SQLite buffer sizes.

`sqlite/check-stage08.sh` is now the acceptance gate for this compiler:
it builds pristine SQLite 3.51.0 with clang and with stage08 and requires
the two programs to print the same bytes.  The 60-program suite stayed
green through a batch that left `sqlite3.c` uncompilable, which is what
that script exists to catch.

Fixed 2026-09-08 (`796d09b0`, `fdbe49b7`) -- what it took for this
compiler to build pristine SQLite 3.51.0 (selfhost ISSUES-67 has the
whole list; `sqlite/build-stage08.sh` is the acceptance test, its output
byte-identical to the clang build's).  All in `cc.s32x` unless noted:

- **Silent miscompiles.** A `char`/`short` local promoted to a register
  kept its wide value (an 8-bit hash accumulator returned 5443).  A
  `long long` literal 0 passed through a function-pointer struct member
  went as one word.  `0x80000000` was typed signed and sign-extended
  into a 64-bit target.  `(*p->m)(args)` through a function-pointer
  member jumped through the code word the pointer named.  `x <u -1`
  folded to a compare against 4095.  Dead-code elimination kept every
  phi alive through a phi-to-phi cycle (code bloat, not wrong code).
  Case labels nested inside another case's braces were dropped by the
  switch pre-scan.  Functions past 8192 instructions read the BURG cost
  table off its end and emitted garbage symbol names.
- **Refused inputs.** `#if` expressions continued across lines; a
  macro's own name inside its expansion; nested function-pointer
  declarators, functions returning function pointers, `signed` alone,
  `offsetof` in constants; `~`/`!` in constant expressions; a
  parenthesised declarator `char *(name[])`; a stray file-scope `;`;
  more than 64 adjacent string literals; block-scope statics with
  string or floating initializers.
- **Capacity and speed.** Macro expansion no longer copies the file's
  tail per expansion (the 9MB amalgamation compiles in a minute);
  ~40 ceilings raised.  `-mlong-calls` for programs whose callees sit
  more than 1MB away.
- **`libc.s32a` / `include/`.** stat, lstat, mkdir, chdir, opendir,
  readdir, closedir, getrusage, gettimeofday, signal, strtod, atof,
  strtoll, strtoull, sscanf, time, localtime, gmtime, access,
  ftruncate; headers sys/types.h, sys/stat.h, dirent.h, limits.h,
  memory.h, errno values, `_IONBF`, `BUFSIZ`.
- **`s32-as.s32x`.** BSS is counted, not emitted a byte at a time.

Fixed 2026-09-01 (`a3cb6cf5`) -- capacity, and one that bites at the
command line:

- **Guest argv was silently capped at 32 arguments and 4KB
  (`libc.s32a`).** The startup code fetched argv into a fixed blob, and
  *any* overflow -- too many arguments or too many bytes -- fell
  through with `argc = 0`, so the program saw no arguments at all and
  said nothing about why. `s32-ar rc lib.s32a <50 files>` printed its
  usage banner instead of building an archive; so did any other tool
  handed a wide glob. Now sized from what the host staged, with a
  chunked fetch (one request moves at most 48KB; the host cap is 64KB).
- **`s32-ar` dropped symbol-index entries past 8192 silently
  (`s32-ar.s32x`).** An archive with more globals than that linked with
  phantom "undefined symbol" errors that pointed nowhere near the
  archiver. Its other ceilings (128 members, 64KB string tables, a flat
  4MB data buffer) now grow on demand too, matching the host `s32-ar`.

Fixed 2026-08-30 (the return-side twin of #6):

- **Silent miscompile: a narrower value returned from a `long long` or
  `double` function ([#13](https://github.com/brazilofmux/slow-32/issues/13)).**
  `unsigned q = x / d; return q;` in a function declared `unsigned long
  long` handed back the low word with the pair's high register untouched
  (`100.00 / 4` came out as `12 * 2^32 + 2500` hundredths on the
  self-hosted COBOL leg). Sema now wraps the returned value in the cast
  to the function's type, as #6 did for arguments; `tests/test_ret_widen.c`
  pins unsigned, signed, char, a call and an int-to-double return.

Fixed in `a34a578a` and `4b14e491` (2026-08-29) — a `cc.s32x` built
before 20:30 that evening lacks them:

- **Block-scope declarator list ([#8](https://github.com/brazilofmux/slow-32/issues/8)).**
  `int a = 1, b = 2;` was refused at block scope with
  `expected token 56 got 57` (the parser wanted `;` and found `,`), as
  were `int a[2] = { 1, 2 }, b = 3;`, `int a = 1, b[2];` and
  `int k = { 1 };`. File scope took all of them. Found by the self-hosted
  build of `cobol/libcob/libcob.c` on Kagura.
- **Silent miscompile: `long long` initializer lost its high word ([#11](https://github.com/brazilofmux/slow-32/issues/11)).**
  A file-scope `long long` array initializer wrote each element's 32-bit
  encoding twice (`{ 1LL, 10LL }` → `01 00 00 00 01 00 00 00 …`), and a
  `long long` global initialized past 32 bits kept only the low word. A
  shift by 32 wraps on SLOW-32, and the constant evaluator was 32-bit
  throughout. libcob's `pow10tab` is the first shape: every COBOL
  division returned 0 when libcob was built with the old compiler.

Changed 2026-08-30 (runtime, not compiler): `__udivsi3`, `__divsi3` and
`__umodsi3` in `libc.s32a` use the hardware divider -- one `div` when both
operands are under 2^31, a fixup for the other cases -- instead of a
32-round shift-subtract loop. Same results at every edge (the regression
suite's `feature-udiv-edge`); a kit older than this runs unsigned division
some twenty times slower, nothing else differs.

Fixed in `9b6d29ac` (2026-08-25):

- **Silent miscompile ([#6](https://github.com/brazilofmux/slow-32/issues/6)).**
  An argument whose marshalling class differed from the declared parameter's
  was passed unconverted — an `int` bound to a `long long` parameter left the
  pair's high register holding whatever was there. `mul(k, 7)` yielded
  `0x800000007` instead of `0x700000007`. Context-sensitive: it frequently read
  a stale zero and appeared to work. The root cause was the parser's function
  registry recording return types only, so argument classification never saw
  the declared parameter type; the fix stores parameter types and lets sema
  insert the conversions, so all three backends inherit it.
- `sizeof x` without parentheses was rejected, surfacing as an empty 44-byte
  object file rather than a diagnostic.

Verified fixed on this kit: implicit and explicit 64-bit args, `int` variables,
sign extension of negatives, the reverse `long long`-to-`int` parameter hazard,
`int`-to-`double` promotion, both `sizeof` forms, a block-scope declarator
list mixing scalars, arrays and brace initializers, and a file-scope
`long long` table read back as a 64-bit quotient.

## Regenerating

From `slow-32/selfhost/stage08` after `make`, into the kit at `~/s32x/selfhost/`:

- `include/`   ← `stage08/include/`
- `crt0.s32o`  ← `stage08/lib/crt0.s32o`
- `libc.s32a`  ← `s32-ar rc libc.s32a` over `stage08/lib/*.s32o` (minus `crt0.s32o`)
- `README.md`  ← `stage08/RUNTIME_KIT.md` (this file)

The compiler and tools themselves (`cc.s32x`, `s32-as.s32x`, `s32-ld.s32x`,
`s32-ar.s32x`) go one level up, in `~/s32x/`.
