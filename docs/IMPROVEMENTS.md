# SLOW-32 Improvement Suggestions

This document consolidates feedback and improvement suggestions for the SLOW-32 toolchain components.

## Resolved Issues

### Assembler (all resolved)
- **Register names**: Case-insensitive via `tolower()` in `parse_register()` ✅
- **`.word symbol`**: REL_32 relocations for jump tables/data pointers ✅
- **Auto-align in `.text`**: `bump_size()` for uniform section accounting ✅
- **S-format parsing**: Explicit `rs2` handling with multiple addressing modes ✅
- **`%lo(symbol)`**: Correctly marked and applied in loads/stores ✅

### LLVM Backend (all resolved)
- **Switch/jump tables**: Custom `LowerJumpTable()`, assembler REL_32, linker support ✅
- **Address formation / GEP scaling**: `SelectAddr()` handles global+offset, 12-bit constraints ✅
- **Long branches**: R2 reserved for materialization, pseudo instructions expanded ✅

### Toolchain (all resolved)
1. **64-bit Division** ✅ — `UMUL_LOHI` custom lowering verified correct.
2. **Linker Error Handling** ✅ — Hard errors on unresolved symbols (exit code 1).
3. **REL_CALL Implementation** ✅ — Implemented in `s32-ld.c`.
4. **Linker Range Checks** ✅ — LO12 signed ±2048, BRANCH +4094, JAL +1,048,574.
5. **Printf Formatting** ✅ — `%llu` and other formats verified.
6. **Runtime Safety & Optimization** ✅ — `memset` beq-only loops, segregated free list malloc.
7. **Emulator Safety** ✅ — W^X unified across all emulators, respects header flag.
8. **Shared Loader** ✅ — Callback-based `s32x_loader.h` used by all emulators.
9. **IEEE 754 Floating-Point** ✅ — Native f32/f64 instructions across entire toolchain.

## Open Items

1. **String-float conversions** (dtoa/strtod) — Not yet implemented. Significant effort (~6-7K lines). Low priority until needed.
2. **Math library transcendentals** (sin, cos, exp, log, etc.) — Stubs satisfy the linker; dbt/QEMU intercept at runtime. No soft-float implementations planned.

## Testing Recommendations

1. Test multi-file linking with global symbols
2. Test negative offsets in data access
3. Test loops with complex control flow
4. Test all intrinsics with edge cases (0 size, odd sizes, etc.)
5. Add string function regression tests (strlen, strcpy, memcpy, etc.)
6. Add stdlib regression tests (qsort, strtol, realloc)
