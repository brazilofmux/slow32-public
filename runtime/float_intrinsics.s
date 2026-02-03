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

# =============================================
# f32 unary stubs: float fn(float)
# =============================================

.global sqrtf
sqrtf:
    halt

.global fabsf
fabsf:
    halt

.global sinf
sinf:
    halt

.global cosf
cosf:
    halt

.global tanf
tanf:
    halt

.global asinf
asinf:
    halt

.global acosf
acosf:
    halt

.global atanf
atanf:
    halt

.global sinhf
sinhf:
    halt

.global coshf
coshf:
    halt

.global tanhf
tanhf:
    halt

.global expf
expf:
    halt

.global logf
logf:
    halt

.global log10f
log10f:
    halt

.global ceilf
ceilf:
    halt

.global floorf
floorf:
    halt

.global roundf
roundf:
    halt

.global truncf
truncf:
    halt

# =============================================
# f64 unary stubs: double fn(double)
# =============================================

.global sqrt
sqrt:
    halt

.global fabs
fabs:
    halt

.global sin
sin:
    halt

.global cos
cos:
    halt

.global tan
tan:
    halt

.global asin
asin:
    halt

.global acos
acos:
    halt

.global atan
atan:
    halt

.global sinh
sinh:
    halt

.global cosh
cosh:
    halt

.global tanh
tanh:
    halt

.global exp
exp:
    halt

.global log
log:
    halt

.global log10
log10:
    halt

.global ceil
ceil:
    halt

.global floor
floor:
    halt

.global round
round:
    halt

.global trunc
trunc:
    halt

# =============================================
# f32 binary stubs: float fn(float, float)
# =============================================

.global powf
powf:
    halt

.global atan2f
atan2f:
    halt

.global copysignf
copysignf:
    halt

# =============================================
# f64 binary stubs: double fn(double, double)
# =============================================

.global pow
pow:
    halt

.global atan2
atan2:
    halt

.global copysign
copysign:
    halt

# =============================================
# float/int mixed stubs
# =============================================

.global ldexpf
ldexpf:
    halt

.global ldexp
ldexp:
    halt

# =============================================
# Pointer-out stubs
# =============================================

.global frexpf
frexpf:
    halt

.global frexp
frexp:
    halt

.global modff
modff:
    halt

.global modf
modf:
    halt

# =============================================
# Int-returning stubs
# =============================================

.global isnan
isnan:
    halt

.global isinf
isinf:
    halt

.global isfinite
isfinite:
    halt
