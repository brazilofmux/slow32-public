# SLOW-32 soft-float intrinsics
# Now that we have FP instructions, these use real FP operations.
# Calling convention: args in r3-r10, return in r1 (r1:r2 for 64-bit).
# For f64/int64 register pair instructions, rN:rN+1 means rN=low, rN+1=high.

.text

# =============================================
# f32 arithmetic
# =============================================

.global __addsf3
__addsf3:
    fadd.s r1, r3, r4
    jalr r0, lr, 0

.global __subsf3
__subsf3:
    fsub.s r1, r3, r4
    jalr r0, lr, 0

.global __mulsf3
__mulsf3:
    fmul.s r1, r3, r4
    jalr r0, lr, 0

.global __divsf3
__divsf3:
    fdiv.s r1, r3, r4
    jalr r0, lr, 0

# =============================================
# f32 comparison (compiler-rt convention)
# =============================================

.global __eqsf2
__eqsf2:
    feq.s r1, r3, r4
    xori r1, r1, 1
    jalr r0, lr, 0

.global __nesf2
__nesf2:
    feq.s r1, r3, r4
    xori r1, r1, 1
    jalr r0, lr, 0

.global __ltsf2
__ltsf2:
    flt.s r1, r3, r4
    bne r1, r0, .ltsf2_neg
    feq.s r1, r3, r4
    bne r1, r0, .ltsf2_zero
    addi r1, r0, 1
    jalr r0, lr, 0
.ltsf2_neg:
    addi r1, r0, -1
    jalr r0, lr, 0
.ltsf2_zero:
    add r1, r0, r0
    jalr r0, lr, 0

.global __lesf2
__lesf2:
    flt.s r1, r3, r4
    bne r1, r0, .lesf2_neg
    feq.s r1, r3, r4
    bne r1, r0, .lesf2_zero
    addi r1, r0, 1
    jalr r0, lr, 0
.lesf2_neg:
    addi r1, r0, -1
    jalr r0, lr, 0
.lesf2_zero:
    add r1, r0, r0
    jalr r0, lr, 0

.global __gesf2
__gesf2:
    flt.s r1, r3, r4
    bne r1, r0, .gesf2_neg
    feq.s r1, r3, r4
    bne r1, r0, .gesf2_zero
    addi r1, r0, 1
    jalr r0, lr, 0
.gesf2_neg:
    addi r1, r0, -1
    jalr r0, lr, 0
.gesf2_zero:
    add r1, r0, r0
    jalr r0, lr, 0

.global __gtsf2
__gtsf2:
    flt.s r1, r3, r4
    bne r1, r0, .gtsf2_neg
    feq.s r1, r3, r4
    bne r1, r0, .gtsf2_zero
    addi r1, r0, 1
    jalr r0, lr, 0
.gtsf2_neg:
    addi r1, r0, -1
    jalr r0, lr, 0
.gtsf2_zero:
    add r1, r0, r0
    jalr r0, lr, 0

.global __unordsf2
__unordsf2:
    feq.s r1, r3, r3
    beq r1, r0, .unord_true
    feq.s r1, r4, r4
    beq r1, r0, .unord_true
    add r1, r0, r0
    jalr r0, lr, 0
.unord_true:
    addi r1, r0, 1
    jalr r0, lr, 0

# =============================================
# f32 <-> int32 conversions
# =============================================

.global __fixsfsi
__fixsfsi:
    fcvt.w.s r1, r3
    jalr r0, lr, 0

.global __fixunssfsi
__fixunssfsi:
    fcvt.wu.s r1, r3
    jalr r0, lr, 0

.global __floatsisf
__floatsisf:
    fcvt.s.w r1, r3
    jalr r0, lr, 0

.global __floatunsisf
__floatunsisf:
    fcvt.s.wu r1, r3
    jalr r0, lr, 0

# =============================================
# f32 <-> int64 conversions
# 64-bit return in r1(lo):r2(hi), 64-bit arg in r3(lo):r4(hi)
# Register pair instructions use rN(lo):rN+1(hi)
# =============================================

.global __fixsfdi
__fixsfdi:
    # float(r3) -> int64(r1:r2)
    # fcvt.l.s writes to rd:rd+1, so use r1 -> r1(lo):r2(hi). Perfect.
    fcvt.l.s r1, r3
    jalr r0, lr, 0

.global __fixunssfdi
__fixunssfdi:
    fcvt.lu.s r1, r3
    jalr r0, lr, 0

.global __floatdisf
__floatdisf:
    # int64(r3:r4) -> float(r1)
    # fcvt.s.l reads rs1:rs1+1, so r3:r4. Perfect.
    fcvt.s.l r1, r3
    jalr r0, lr, 0

.global __floatundisf
__floatundisf:
    fcvt.s.lu r1, r3
    jalr r0, lr, 0

# =============================================
# sqrtf and fmodf
# =============================================

.global sqrtf
sqrtf:
    fsqrt.s r1, r3
    jalr r0, lr, 0

# fmodf: complex operation, stub returns 0 for now
.global fmodf
fmodf:
    add r1, r0, r0
    jalr r0, lr, 0
