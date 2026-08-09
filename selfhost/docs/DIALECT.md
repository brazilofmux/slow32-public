# Selfhost C Dialect Map (live path)

**Audience:** anyone planning dialect work or third-party ports.  
**Canonical code:** `selfhost/src/` (parser, sema, HIR, `pp.h`), consumed by:

| Consumer | Output | Role |
|----------|--------|------|
| `stage08` (`s12cc` → `cc.s32x`) | SLOW-32 asm / toolchain | Bootstrap / guest selfhost |
| `stage08-cross-x64` (`cc-x64 --hir`) | x86-64 ELF | Host cross; GCC-parity pressure |
| `stage08-cross-a64` (`cc-a64 --hir`) | AArch64 ELF | Host cross; same frontend |

Historical stages (`stage03`–`stage07`) keep **snapshots**; do not use them as the dialect product surface. Stage06 `s12cc` lags the live frontend by design.

Companion docs: [BOOTSTRAP.md](BOOTSTRAP.md), [C-SUBSET.md](C-SUBSET.md) (native *tools* inventory — older), [ISSUES.md](../ISSUES.md) Section C.

---

## Goals (what “enough C” means)

1. **Bootstrap** — fixed-point / purity gates; rebuild as/ar/ld/cc/libc.  
2. **Capability** — compile emulator + DBT under guest tools.  
3. **Cross pressure** — compile substantial C on `cc-x64`/`cc-a64` with correct ABI vs gcc (diff-test corpus).  

Full C11/GNU and “TCC compiles clean” are **optional**, not trust-root requirements.

---

## Language surface (live frontend)

### Solid / tested

| Area | Notes | Evidence |
|------|--------|----------|
| Integers incl. `long long` | Full promotion ladder | `#33`, tests `test_llong`, d07 |
| `float` / `double` | HIR + host FP units / soft builtins | `#34`, FLOATING-POINT.md |
| Struct / union / enum / typedef | Layout, offsetof, multi-decl | layout / phase tests |
| Struct **by value** (params + returns) | Hidden-pointer ABI; callee copies | `#51`, d31_struct_byval |
| Anonymous struct/union members | C11 | phase27, d21/d22 |
| Designated initializers | Sparse, nested, strings | phase28/29, d23/d24/d30 |
| Compound literals (block scope) | phase30, d25/d26 | |
| Flexible array members | phase / flex_array tests | |
| Bitfields | Parse + HIR extract/store | `test_bitfields.c`, `hl_bf_*` |
| GNU stmt expressions `({…})` | d34_stmt_expr | |
| `_Atomic` (as unqualified type), `typeof` / `__typeof__` | Decl noise | test_decl_dialect, d28/d29 |
| `__attribute__((…))` skip | Prefix/infix/suffix | d29 |
| Varargs (int / ptr / long long) | x64 + a64 | d32_varargs, `#48` GP path |
| Statement / expression control flow | goto, switch (dense tables), short-circuit | corpus d05–d20 |

### Preprocessor (`src/pp.h`) — **much more than ISSUES #35 claimed**

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
| `#line` / `#pragma` / unknown | **Skipped** (no-op) |
| `#error` | **Was no-op** → hard error (see Pack dialect work) |
| `#` stringize / `##` paste | **Not implemented** in expand (lexer knows `##`) |
| `__FILE__` / `__LINE__` | **Dynamic** (see predefs) |
| `__STDC__` / arch predefs | **Injected** at parse start |

### Still thin / deferred

| Gap | Why it matters | Priority |
|-----|----------------|----------|
| `##` / `#` in macros | Header metaprogramming | Medium (third-party) |
| Nested include file name stack for `__FILE__` | Correctness in headers | Low–medium |
| FP varargs (`va_arg(ap, double)`) on a64 | V0–V7 save area | Medium (cross polish) |
| libc `%f`/`%g`/`%e` | Stats / demos print `?` | Medium after FP varargs |
| Full C99 external-`inline` | Link-time ODR oddities | Low |
| Rich Unix headers (`dlfcn`, full `sys/*`) | Ports, not codegen | As needed |
| Guest stack / frame limits (`ISSUES` #46) | Large TUs feel “broken” | Ops, not dialect |

---

## ABI notes (cross)

- **Struct by value:** one pointer arg (address of source); callee copies into local slot. Matches neither SysV full multi-reg ABI nor pure stack copy — intentional simple ABI; must stay consistent across x64/a64 guest-lowering.  
- **Struct return:** hidden `__retptr` (sret-style).  
- **Varargs:** tagged-pointer `va_list` (`typedef char *va_list`); GP only until FP path lands.

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
| 2026-08-08 | Initial map after full-tree audit; corrected stale #35/#38 claims; predefs + `#error` landed with tests. |
