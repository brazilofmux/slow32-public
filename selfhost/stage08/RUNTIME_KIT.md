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

Rebuilt 2026-10-25 (`765ca6e1`).  `cc.s32x`, `libc.s32a` and
`s32-ld.s32x`; the other tools and `include/` rebuild identical.
`s32-ld.s32x` takes `--heap-size SIZE` (`c45872a1`): the heap below the
MMIO window was always 64 MB, too small for Argon2 at a 64 MiB memory
cost; the default is unchanged.  The compiler (`41d24594`, `765ca6e1`)
keeps char and short parameters in registers, drops masks that change
no bit (`andi 255` after a byte load), inlines bodies of 14 AST nodes
or fewer at every site, fully unrolls counted loops of up to 16 trips,
and folds constant offsets and shared frame addresses into the loads
and stores that use them.  A byte-oriented AES-256 went from 7x clang's
executed instructions to 2.4x; SQLite under stage08, -5% shell
dynamic.

Rebuilt 2026-10-25 (`c451b7f9`).  `cc.s32x`, `libc.s32a` and
`include/unistd.h`; the tools rebuild identical.  The compiler
bottom-tests `while` and `for` loops (`7be0f6c9`) and no longer computes
two returns' values into r1 when both are phis of one block (`6748e15e`,
wrong code from ordinary C).  `libc.s32a` gains `getcwd`.

Rebuilt 2026-10-25 (`2b610f81`).  `cc.s32x`, `libc.s32a` and the five
tools.  The compiler treats equal literals as one operand in CSE and
folds the strides strength reduction builds (`cdf06a4c`); `memcpy` and
`memset` in `libc.s32a` and in every tool loop with one branch per 16
bytes.  SQLite under stage08: -5.5% static, -7% shell dynamic.  The
tools' output is byte-identical to before (checked on s12cc.c).

Rebuilt 2026-10-24 (`1c13e70c`).  `cc.s32x` and `libc.s32a`: a 64-bit
shift by a run-time count is inline instead of a helper call, 8-byte
parameters stay in registers instead of being stored to the frame, and
statics of up to 12 AST nodes are inlined by default (`S12CC_INLINE`
unset; giving it selects the full policy as before).  SHA-512 under
the DBT went from 6x clang to 1.5x.  `libc.s32a` grows 1.8%.

Fixed 2026-10-24 (`302a417e`).  In `s32-ld.s32x` only: an object's
local symbols stay its own instead of filling the linker's 4,096-entry
global table, so a unit with thousands of string literals links.

Rebuilt 2026-10-21 (tree at `59a6c7e2`).  The clang-built apps only;
`cc.s32x`, the tools, `crt0.s32o`, every `libc.s32a` member and
`include/` rebuild byte-identical.  The apps carry the runtime's
`fma`/`fmaf` (one rounding), getenv keeping more than 64 names, and
`nanosleep` rejecting a bad interval; `dbase.s32x` also gains the
LOCAL fix (a LOCAL no longer releases the variable it shadows).

Fixed later 2026-10-20 (`f9e2301f`).  In `libc.s32a` (`malloc.s32o`, the
only member that changed) and so in every tool and app that links it --
`cc.s32x`, `s32-as/ld/ar`, `slow32dis/dump` and the clang-built apps,
whose clang runtime had the same fault:

- **A request too large to exist could come back as a small block**:
  sizes just under 2 GB (here) or 4 GB (the clang runtime) wrapped while
  being rounded up.  And `realloc` given a size past 2 GB freed the block
  it was handed and said NULL, so the caller's next use of it was a
  double free.  Anything over 1 GB is now refused before any arithmetic.

Fixed 2026-10-20 (`389da90c`, `ed653145`).  In `s32-as.s32x`, and in
`libc.s32a` and `include/` (getopt); `cc.s32x`, `s32-ld`, `s32-ar` and
the other 32 library members are unchanged.  The clang-built apps were
rebuilt by the re-pinned LLVM (`8d15281a`):

- **Every section an object file had was marked 4-aligned**, whatever
  its `.align`/`.balign`/`.comm` asked for, so the linker could place a
  16-aligned variable off its boundary.
- **No `getopt`**: `<unistd.h>` declares `getopt`, `<getopt.h>`
  `getopt_long` and `getopt_long_only` (the new `getopt.s32o`).

Fixed 2026-10-16 (`f8815894`).  In `islisp.s32x` only; the compiler,
tools and library are unchanged:

- **islisp ran out of its 32MB heap in a long allocation loop**
  (islisp ISSUES-7): about two million conses with a few thousand live.
  `regression/run-kit-islisp.sh` (`islisp/tests/lists.lsp`) catches it.

Fixed 2026-10-14, later (`959a43c3`).  In `cc.s32x` only; every
`libc.s32a` member compiles the same before and after:

- **A float passed through `...` was not promoted to double**
  (selfhost ISSUES-90).  `printf("%f", f)` with `f` a float printed
  0.000000 or a 40-digit number; any variadic callee reading
  `va_arg(ap, double)` got the wrong value.
- **A call through a pointer to a variadic function placed its double
  arguments as for a fixed signature** (ISSUES-90): the callee's
  va_arg walk read the wrong registers.

Fixed 2026-10-14 (`d67aab3b`, `90c9ef3e`).  In `cc.s32x`, and in
`libc.s32a`'s `posix_proc.s32o` (getenv) and `dtoa.s32o` (recompiled
by the fixed compiler); the tools were rebuilt by the fixed stage07;
`<signal.h>` gained `SIGPIPE`:

- **phi(undefined, x) became x where x did not dominate** (selfhost
  ISSUES-88).  A variable set only inside a loop -- the index of the
  maximum, say -- could read as a value computed later in that loop:
  wrong results, silently.  Found by fort370 on a real program; stage05
  through stage08 all had it.
- **A frameless leaf read its 9th and later arguments from the
  caller's frame** (ISSUES-88).  A function needing no frame addressed
  its stack arguments through r30, which it never sets.
- **A second getenv overwrote the first one's answer** (ISSUES-89).
  Each name's answer now has storage of its own.

Fixed 2026-10-07, ninth batch.  In `cc.s32x` only; any libc member
compiled from a source with such an expression is wrong until rebuilt:

- **The integer promotions were skipped** (selfhost ISSUES-82).  An
  `unsigned char` or `unsigned short` operand made its arithmetic,
  comparison, shift or unary-minus result unsigned, so `(c >= '0' && c
  <= '9' ? c - '0' : -1) < 0` was an unsigned compare, never true, and
  `c > -1` false.  Silent, in any sign-sensitive use of such a result;
  found by cobol's libcob (the 2014 date and time functions' scanner).

Fixed 2026-10-09, ninth batch (`70579651`, `1bd54189`).  In `cc.s32x` only;
the tools, `crt0.s32o`, the headers and all 32 `libc.s32a` members rebuilt
content-identical:

- **A hex escape stopped after two digits, and an out-of-range one was
  accepted** (selfhost ISSUES-83).  `"\x0041"` was three bytes; `"\xC3\xA4b"`,
  one escape to the standard and out of range, compiled as the bytes its
  author meant while clang and gcc refuse it.  Every digit is taken now,
  and a value over 255 is an error (octal too).  With it, a character
  constant above 127 is sign-extended: `'\xff'` is -1, as clang's slow32
  and stage08's own `ldb` have it, not 255.
- **sizeof of a string literal was 4** (selfhost ISSUES-84), the pointer
  it decays to, not the array's size; and `sizeof ( expression )` ended
  at the parenthesis, so `sizeof("abc")[0]` indexed the size.

Fixed 2026-10-07, eighth batch (`e98e1926`).  In `cc.s32x` only:

- **A struct member read after a call took its address reloaded from
  the wrong frame offset** (selfhost ISSUES-81).  `touch(&w.scale); k =
  w.scale;` read past the struct: the register allocator's call-split
  copy carried no frame-address chain, and the BURG's LOAD(faddr) fold
  took its offset from the copy, 0.  Silent, in any value of a local
  struct's member read after such a call; found by cobol's libcob
  (`sf_store`), which carried a workaround for one day.

Fixed 2026-09-29, seventh batch (`12d523f8`).  In `cc.s32x` only; the
tools, `crt0.s32o` and every `libc.s32a` member rebuilt content-identical:

- **INT64_MIN in a static initializer was 0** (selfhost ISSUES-70).  The
  constant folder carried a 64-bit value's high word through a literal,
  unary minus and shifts only, so `-9223372036854775807LL - 1` -- how
  headers spell INT64_MIN and LLONG_MIN -- folded to 0, and `&`, `|`,
  `^` lost the high word too.  Silent, in any static table built on such
  a constant.  A `*`, `/` or `%` of a constant that does not fit 32 bits
  is now an error, not a dropped word.  Wide constants compared with
  `<` `==` and the rest still compare low words only.

Fixed 2026-09-11, sixth batch (`877afcf1`..`f4e09a25`) -- self-hosting
restored, and the whole-program size work.  All in `cc.s32x` unless noted:

- **stage08 could not rebuild itself, and no default gate said so.**  Two
  defects, one hiding the other.  `SSA_DF_W` (32) was too small for the
  compiler's own source, so gen1 died with "too many dominance-frontier
  entries" -- raised to 64.  Behind that, a `goto` into a `case` label
  (Ragel's `st15` falling into `case 15` in `lex_next`) gave a jump-table
  target a second predecessor that JMPTAB edges cannot carry a phi copy
  for; the lexer entered with a stale register and every input compiled
  to 18 empty bytes.  Table targets with more than one predecessor are
  now trampolined through a `BR`.  A kit built from `4ce2473c` through
  `877afcf1` produces a working compiler for user programs but NOT a
  working gen2; only the fixed-point gate could see it, and that gate is
  now on by default (`run-tests.sh`, GitHub issue 75).
- **Short calls by default, with linker veneers.**  `s32-ld` punches
  32KB islands at each end of `.text` (and between input units about
  every 960KB) and emits `lui`/`addi`/`jalr` stubs for any JAL past
  +/-1MB, failing loudly if none is reachable.  SQLite no longer needs
  `-mlong-calls`: 24,222 instructions, 15% of the excess over clang
  (GitHub issue 74).  The same mechanism is in the kit's `s32-ld.s32x`
  (GitHub issue 85); a single unit longer than the stride still needs
  the compiler's `-mlong-calls`.  The kit linker has no `-r` output.
- **Frame elision.**  A leaf with no live stack omits its prologue; any
  non-varargs function with a frame of at most 2047 bytes addresses its
  slots SP-relative and skips the `r30` save -- 1,929 of SQLite's 2,365
  functions.  Behaviour-neutral by every gate, but it moves code: size
  and perf numbers across this boundary are not comparable.
- **Dead statics dropped after inlining** (`S12CC_INLINE=<n>`, still
  opt-in) and **return values computed straight into `r1`**.  Small on
  their own; recorded so the numbers add up.

Net on SQLite: 337,898 -> 304,522 instructions, 1.91x -> 1.72x clang.

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

## Container

`slow32:kit` (`Dockerfile.kit` at the tree root, FROM `slow32:base`) is
this kit at `/opt/slow32/kit` with `s32kcc`, the three-step compile above
as one command on the image's emulator. `scripts/build-kit-image.sh`
stages `~/s32x` (minus `regal.s32x`), builds, runs both kit differentials
inside the image, and with `--push` builds both architectures and pushes
them for `~/builder/manifest.sh` to stitch. It runs where the kit is; the
kit is not in git.

## stdio is buffered (2026-10)

`libc.s32a`'s stdio is a real one as of selfhost ISSUES-73: stdout is line
buffered, stderr is not buffered, files are fully buffered, and `exit`
(or returning from `main`) writes what the streams still hold.  The
fd-named functions (`fdputc`, `fdgetc`, ...) are the same streams by
descriptor; the tools themselves are on `<stdio.h>` since ISSUES-74.  Three things a program written against the old
library might trip on:

- a program that ends by a `halt` instruction of its own, or by a fault,
  never reaches `exit`: what stdout holds of an unfinished line is lost.
  Finish the line, or `fflush(stdout)`;
- `write(1, ...)` and `read(0, ...)` are still the bare requests.  Mixed
  with `printf` or `fgets` on the same descriptor they are out of step
  with the buffer, as in any C library;
- `exit(n)` now exits with `n`.  It used to exit with whatever the last
  call had returned.

A kit built before this change has the unbuffered library.

## The rest of the library (2026-10)

As of selfhost ISSUES-75 `libc.s32a` has the functions a C program
expects of a hosted library, and the ones it already had behave as the
standard says.  New: `setjmp`/`longjmp`; `signal`/`raise` (a handler
runs when the program raises the signal -- nothing else sends one), and
`abort` through `SIGABRT`; `atexit`, `_Exit`, `_exit`; `abs`, `labs`,
`atol`, `div`, `ldiv`, `rand`, `srand`, `system`; `strnlen`, `strpbrk`,
`strtok`, `strtok_r`, `memrchr`, `strcoll`, `strxfrm`, `strerror`;
`mktime`, `asctime`, `ctime`, `difftime`, `strftime`, `gmtime_r`,
`localtime_r`, `clock`, `clock_gettime`, `nanosleep`, `sleep`; `fdopen`,
`freopen`, `tmpfile`, `getline`, `getdelim`, `fgetpos`, `fsetpos`,
`setbuf`; `isblank`, `iscntrl`, `isgraph`, `isascii`, `toascii`;
`fabsf`.  `<setjmp.h>` and `<strings.h>` are new headers, and `<math.h>`
declares the transcendentals the library always defined.

What a program written against the old library might trip on:

- `strtol` clamps at the ends of `long` and sets `ERANGE`; "0x" with no
  hexadecimal digit after it is the number 0 followed by an x;
- `abort` ends the run with status 134 through `SIGABRT`'s handler, and
  runs no `atexit` function (it was `exit(134)`);
- `usleep` sleeps.  (It sent the wrong request and returned at once.)
- `perror` says what `errno` means; it said "error";
- `RAND_MAX` is 65535, which is what `rand` returns up to.

Not there: `scanf`/`fscanf` (`sscanf` is), locales other than "C", wide
and multibyte characters, `tmpnam`, memory streams.  `time_t` is 32 bits.

The tools, at the same time:

- `s32-as` takes any number of values on a `.word`, `.half` or `.quad`
  line.  It assembled the first seven and dropped the rest without a
  word (stage08 cc writes one value a line; hand-written assembly need
  not).
- `cc.s32x` keeps the locals of a function that calls `setjmp` in memory,
  so they hold what was last stored when `longjmp` comes back; lays out
  an array of a typedef'd array type (`jmp_buf handlers[4]`) as the
  two-dimensional array it is, where it made four pointers; and gives
  `sizeof` of such a type, of an array's row, and of `int[5]` the
  array's size.

A kit built before this change has none of the above.

## Regenerating

From `slow-32/selfhost/stage08` after `make`, into the kit at `~/s32x/selfhost/`:

- `include/`   ← `stage08/include/`
- `crt0.s32o`  ← `stage08/lib/crt0.s32o`
- `libc.s32a`  ← `s32-ar rc libc.s32a` over `stage08/lib/*.s32o` (minus `crt0.s32o`)
- `README.md`  ← `stage08/RUNTIME_KIT.md` (this file)

The compiler and tools themselves (`cc.s32x`, `s32-as.s32x`, `s32-ld.s32x`,
`s32-ar.s32x`) go one level up, in `~/s32x/`.
