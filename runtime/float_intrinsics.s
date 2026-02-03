# SLOW-32 floating-point library call stubs
#
# These exist solely to satisfy the linker and place symbols in the .s32x
# symtab so that dbt/QEMU TCG can intercept them at runtime and execute
# them natively on the host.
#
# The LLVM backend emits native FP instructions (fadd.s, fsub.s, etc.)
# directly -- it does NOT emit libcalls for arithmetic, comparisons, or
# conversions. The only FP operations that expand to libcalls are FREM
# (fmodf/fmod), because there is no hardware remainder instruction.
#
# If you get a linker error for a missing FP symbol, that's a real bug --
# either the backend is incorrectly emitting a libcall, or user code is
# calling a math function that needs a stub added here.

.text

# =============================================
# FREM stubs (LLVM expands FREM to these)
# =============================================

# fmodf: f32 remainder - intercepted by dbt/QEMU at runtime
.global fmodf
fmodf:
    # Trap: if we actually execute this, dbt/QEMU didn't intercept.
    # Halt loudly instead of returning a bogus value.
    halt

# fmod: f64 remainder - intercepted by dbt/QEMU at runtime
# Args: r3:r4 = dividend (f64), r5:r6 = divisor (f64)
# Return: r1:r2 (f64)
.global fmod
fmod:
    # Trap: if we actually execute this, dbt/QEMU didn't intercept.
    # Halt loudly instead of returning a bogus value.
    halt
