# builtins_fp64.s -- 64-bit floating-point helper functions for SLOW-32
#
# All functions follow the SLOW-32 ABI:
#   Args in r3-r6 (lo/hi pairs), return in r1:r2 (lo:hi)
#   Callee-saved: r11-r28
#
# Double values are stored as register pairs (rN, rN+1) where N is even.
# Args come in r3-r6 but r3 is odd, so we shuffle to even-based pairs:
#   r4:r5 = operand 1 (or result), r6:r7 = operand 2
#
# Functions:
#   __fp64_add      - f64 add
#   __fp64_sub      - f64 subtract
#   __fp64_mul      - f64 multiply
#   __fp64_div      - f64 divide
#   __fp64_neg      - f64 negate
#   __fp64_eq       - f64 equal (returns 0/1)
#   __fp64_lt       - f64 less than (returns 0/1)
#   __fp64_le       - f64 less-equal (returns 0/1)
#   __fp64_cvt_itoD - int to double
#   __fp64_cvt_DtoI - double to int
#   __fp64_cvt_ftoD - float to double
#   __fp64_cvt_DtoF - double to float
#   __fp64_cvt_ltoD - long long to double
#   __fp64_cvt_DtoL - double to long long

.text

# ================================================================
# Binary arithmetic: args r3=lo1, r4=hi1, r5=lo2, r6=hi2
# Shuffle: r7=r6, r6=r5, r5=r4, r4=r3 → pairs r4:r5 and r6:r7
# Result in r4:r5 → r1:r2
# ================================================================

.global __fp64_add
__fp64_add:
    addi r7, r6, 0
    addi r6, r5, 0
    addi r5, r4, 0
    addi r4, r3, 0
    fadd.d r4, r4, r6
    addi r1, r4, 0
    addi r2, r5, 0
    jalr r0, r31, 0

.global __fp64_sub
__fp64_sub:
    addi r7, r6, 0
    addi r6, r5, 0
    addi r5, r4, 0
    addi r4, r3, 0
    fsub.d r4, r4, r6
    addi r1, r4, 0
    addi r2, r5, 0
    jalr r0, r31, 0

.global __fp64_mul
__fp64_mul:
    addi r7, r6, 0
    addi r6, r5, 0
    addi r5, r4, 0
    addi r4, r3, 0
    fmul.d r4, r4, r6
    addi r1, r4, 0
    addi r2, r5, 0
    jalr r0, r31, 0

.global __fp64_div
__fp64_div:
    addi r7, r6, 0
    addi r6, r5, 0
    addi r5, r4, 0
    addi r4, r3, 0
    fdiv.d r4, r4, r6
    addi r1, r4, 0
    addi r2, r5, 0
    jalr r0, r31, 0

# ================================================================
# Unary: args r3=lo, r4=hi
# Shuffle: r5=r4, r4=r3 → pair r4:r5
# ================================================================

.global __fp64_neg
__fp64_neg:
    addi r5, r4, 0
    addi r4, r3, 0
    fneg.d r4, r4, r0
    addi r1, r4, 0
    addi r2, r5, 0
    jalr r0, r31, 0

# ================================================================
# Comparisons: args r3=lo1, r4=hi1, r5=lo2, r6=hi2
# Same shuffle as binary ops, result is single int in r1
# ================================================================

.global __fp64_eq
__fp64_eq:
    addi r7, r6, 0
    addi r6, r5, 0
    addi r5, r4, 0
    addi r4, r3, 0
    feq.d r1, r4, r6
    jalr r0, r31, 0

.global __fp64_lt
__fp64_lt:
    addi r7, r6, 0
    addi r6, r5, 0
    addi r5, r4, 0
    addi r4, r3, 0
    flt.d r1, r4, r6
    jalr r0, r31, 0

.global __fp64_le
__fp64_le:
    addi r7, r6, 0
    addi r6, r5, 0
    addi r5, r4, 0
    addi r4, r3, 0
    fle.d r1, r4, r6
    jalr r0, r31, 0

# ================================================================
# Conversions
# ================================================================

# int → double: r3=int, returns r1:r2=double
.global __fp64_cvt_itoD
__fp64_cvt_itoD:
    addi r4, r3, 0
    fcvt.d.w r4, r4, r0
    addi r1, r4, 0
    addi r2, r5, 0
    jalr r0, r31, 0

# double → int: r3=lo, r4=hi, returns r1=int
.global __fp64_cvt_DtoI
__fp64_cvt_DtoI:
    addi r5, r4, 0
    addi r4, r3, 0
    fcvt.w.d r1, r4, r0
    jalr r0, r31, 0

# float → double: r3=float_bits, returns r1:r2=double
.global __fp64_cvt_ftoD
__fp64_cvt_ftoD:
    addi r4, r3, 0
    fcvt.d.s r4, r4, r0
    addi r1, r4, 0
    addi r2, r5, 0
    jalr r0, r31, 0

# double → float: r3=lo, r4=hi, returns r1=float_bits
.global __fp64_cvt_DtoF
__fp64_cvt_DtoF:
    addi r5, r4, 0
    addi r4, r3, 0
    fcvt.s.d r1, r4, r0
    jalr r0, r31, 0

# long long → double: r3=lo, r4=hi, returns r1:r2=double
.global __fp64_cvt_ltoD
__fp64_cvt_ltoD:
    addi r5, r4, 0
    addi r4, r3, 0
    fcvt.d.l r4, r4, r0
    addi r1, r4, 0
    addi r2, r5, 0
    jalr r0, r31, 0

# double → long long: r3=lo, r4=hi, returns r1:r2=long_long
.global __fp64_cvt_DtoL
__fp64_cvt_DtoL:
    addi r5, r4, 0
    addi r4, r3, 0
    fcvt.l.d r4, r4, r0
    addi r1, r4, 0
    addi r2, r5, 0
    jalr r0, r31, 0
