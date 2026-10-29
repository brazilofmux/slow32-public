# Selfhost C Dialect Map (live path)

**Audience:** anyone planning dialect work or third-party ports.  
**Canonical code:** `selfhost/src/` (parser, sema, HIR, `pp.h`), consumed by:

| Consumer | Output | Role |
|----------|--------|------|
| `stage08` (stage08 cc, `s12cc.c` → `cc.s32x`) | SLOW-32 asm / toolchain | Bootstrap / guest selfhost |
| `stage08-cross-x64` (`cc-x64 --hir`) | x86-64 ELF | Host cross; GCC-parity pressure |
| `stage08-cross-a64` (`cc-a64 --hir`) | AArch64 ELF | Host cross; same frontend |

Historical stages (`stage03`–`stage07`) keep **snapshots**; do not use them as the dialect product surface. Stage06's compiler lags the live frontend by design.

Companion docs: [BOOTSTRAP.md](BOOTSTRAP.md), [C-SUBSET.md](C-SUBSET.md) (native *tools* inventory — older), [ISSUES.md](../ISSUES.md) Section C.

---

## Goals (what “enough C” means)

1. **Bootstrap** — fixed-point / purity gates; rebuild as/ar/ld/cc/libc.  
2. **Capability** — compile emulator + DBT under guest tools.  
3. **Cross pressure** — compile substantial C on `cc-x64`/`cc-a64` with correct ABI vs gcc (diff-test corpus).  

stage08 cc targets **C11** (`__STDC_VERSION__` 201112L): every valid probe in
[`c-conformance/`](../c-conformance/README.md) passes, and its diagnostics follow
clang's default grading (error where clang errors, warn where it warns).
Complex types are absent by choice (`__STDC_NO_COMPLEX__`).  VLAs work on
SLOW-32; the cross targets define `__STDC_NO_VLA__` and refuse them.  GNU
extensions beyond those listed here, and “TCC compiles clean”, remain
optional.

---

## Language surface (live frontend)

### Solid / tested

| Area | Notes | Evidence |
|------|--------|----------|
| Integers incl. `long long` | Full promotion ladder | selfhost ISSUES-33, tests `test_llong`, d07 |
| `float` / `double` | Hardware FP on SLOW-32; host FP units on the cross targets | selfhost ISSUES-34, FLOATING-POINT.md |
| Struct / union / enum / typedef | Layout, offsetof, multi-decl | layout / phase tests |
| Struct **by value** (params + returns) | clang's byval ABI on SLOW-32 since 2026-08-24 (caller copies, address in a stack slot; sret in r3) | selfhost ISSUES-51, d31_struct_byval |
| Anonymous struct/union members | C11 | phase27, d21/d22 |
| Designated initializers | Sparse, nested, strings | phase28/29, d23/d24/d30 |
| Compound literals (block scope) | phase30, d25/d26 | |
| Flexible array members | phase / flex_array tests | |
| Bitfields | Parse + HIR extract/store | `test_bitfields.c`, `hl_bf_*` |
| GNU stmt expressions `({…})` | d34_stmt_expr | |
| `_Atomic` (as unqualified type), `typeof` / `__typeof__` | Decl noise | test_decl_dialect, d28/d29 |
| `__attribute__((…))` skip | Prefix/infix/suffix | d29 |
| Varargs (int / ptr / long long) | x64 + a64 | d32_varargs, selfhost ISSUES-48 GP path |
| Statement / expression control flow | goto, switch (dense tables), short-circuit | corpus d05–d20 |
| C11 keywords | `_Generic`, `_Alignas`/`_Alignof` (over 4 on locals realigns the frame), `_Static_assert`, `_Noreturn`, `_Thread_local`, `__func__`; `_Bool` a real type | c-conformance 6.7 probes, docs/TYPES.md S6 |
| Variable length arrays | SLOW-32 only; sp restored at block end, break/continue and goto; VM types, `sizeof` at run time | 6.7.6.2 / 6.8.6 probes, docs/TYPES.md |
| C types and constraint checks | Typed table (docs/TYPES.md); constraints of 6.5-6.9 diagnosed as clang's defaults grade them; const, redeclarations, initializers checked | c-conformance DIAG rows |
| Lexical | wide and `u8`/`u`/`U` literals, UCNs, hex floats, digraphs; K&R definitions | 6.4 probes |

### Preprocessor (`src/pp.h`) — complete for C11 chapter 6.10

| Feature | Status |
|---------|--------|
| `#define` object-like | Yes |
| `#define` function-like + `__VA_ARGS__` | Yes (phase19/23) |
| `#undef` | Yes |
| `#include "…"` / path search (`-I`, source dir) | Yes |
| `#ifdef` / `#ifndef` / `#else` / `#endif` | Yes |
| `#if` expression eval (`defined`, ints, `&&` `\|\|` `!` …) | Yes |
| `#elif` | Yes (phase6 chains) |
| Macro expand on `TK_IDENT` | Yes |
| `#line` | Yes (presumed file and line, docs/TYPES.md S4) |
| `#pragma` | `once` honoured, `STDC` accepted, `_Pragma` operator; others ignored |
| `#error` | Hard error |
| `#` stringize / `##` paste | Yes: arguments pre-expanded, placemarkers, `#__VA_ARGS__`, painted names |
| `__FILE__` / `__LINE__` | Per file: a header's `__FILE__` names the header |
| Predefined | `__STDC__`, `__STDC_VERSION__` 201112L, `__STDC_HOSTED__` 1, `__DATE__`/`__TIME__`, the `__STDC_NO_*` set, arch predefs |

### Still thin / deferred

| Gap | Why it matters | Priority |
|-----|----------------|----------|
| Full dtoa / `%e`/`%g` fidelity in the **cross** libcs | Crude `%.Nf` only (SLOW-32's libc prints through dtoa) | Low |
| Full C99 external-`inline` | Link-time ODR oddities | Low |
| Rich Unix headers (`dlfcn`, full `sys/*`) | Ports, not codegen | As needed |
| Guest stack / frame limits (selfhost ISSUES-46) | Large TUs feel “broken” | Ops, not dialect |

---

## ABI notes (cross)

- **Struct by value (cross targets only; SLOW-32 uses clang's byval convention, above):** one pointer arg (address of source); callee copies into local slot. Matches neither SysV full multi-reg ABI nor pure stack copy — intentional simple ABI; must stay consistent across x64/a64 guest-lowering.  
- **Struct return:** hidden `__retptr` (sret-style).  
- **Varargs:** `va_list` is `char *` pointing at a frame control block (GP + FP cursors + shared stack overflow). Both X/GP and V/XMM register save areas are filled in the prologue.

---

## How to extend dialect safely

1. Implement in **`selfhost/src/`** (not a frozen stage snapshot).  
2. Add a **stage08** `test_phase*.c` and/or **cross** `diff-test/corpus/dNN_*.c` that matches gcc.  
3. Rebuild `cc-x64` / `cc-a64` and run `make test` / diff-test where host tools exist.  
4. For guest impact, rebuild stage08 and run fixed-point if the change is in the self-compiled path.  
5. Update this file and ISSUES Section C in the same change.

---

## Revision

| Date | Note |
|------|------|
| 2026-08-08 | Initial map after full-tree audit; corrected stale selfhost ISSUES-35/38 claims; predefs + `#error` landed with tests. |
| 2026-10-29 | C11: docs/TYPES.md S1-S6 and VLAs; `c-conformance/probe.sh --check` gates the frontend. |


---

## ABI interop with the LLVM backend (native SLOW-32)

**Verified 2026-08-08** (`stage08/run-interop-llvm.sh` — one `.s32x` built from a
clang TU and a stage08 cc TU, calling each other in both directions, linked with the
native `s32-ld` against clang's `crt0.s32o` + `libc_debug.s32a`):

| Crossing the boundary | Status |
|---|---|
| `int` / pointer args + returns | **interoperable** |
| >8 args (stack passing, fp+0…) | **interoperable** |
| `long long` (register pairs, r1:r2 return) | **interoperable** |
| stage08 cc → clang callbacks (incl. via extern) | **interoperable** |
| stage08 cc code against clang's libc/crt0 | **interoperable** |
| Integer varargs (stage08-cc-defined, clang-called) | **interoperable** — the
call-site convention is shared; each compiler's `va_*` machinery is
callee-private |
| `double` args/returns | **interoperable** since 2026-08-24 (clang's aligned-pair convention) |
| struct by value | **interoperable** since 2026-08-24, both directions (clang's byval convention; sret in r3) |

**Link recipe:** add `stage08/builtins64.s` (`__muldi3` — clang inlines MUL via
`UMUL_LOHI` so `libs32.s32a` never carried it) and `stage08/builtins_fp64.s`
(`__fp64_mul` … — clang uses HW FP). Both coexist with `libs32.s32a`; archive
members are only pulled for still-unresolved symbols.

The two divergences this table first recorded (double passing and
struct-by-value) were closed on 2026-08-24 by adopting clang's conventions;
`run-interop-llvm.sh` gates both.
