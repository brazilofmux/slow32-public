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

No known issues in the current kit. Four bugs have been fixed since the
first kit; if your `cc.s32x` predates the commit named, the bug is live.

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
