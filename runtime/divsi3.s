    .global __udivsi3
    .global __divsi3

# Unsigned 32-bit division on the hardware signed divider.
# r3 = dividend a, r4 = divisor b, quotient in r1.
#
# DIV is signed, so it serves directly when neither operand has bit 31
# set -- the common case, one instruction.  Otherwise:
#   b >= 2^31: the quotient is 0 or 1 (a >= b).
#   a >= 2^31, b < 2^31: q0 = (a >> 1) / b fits the signed divider and
#       the true quotient is 2*q0 or 2*q0 + 1; the remainder a - 2*q0*b
#       is in [0, 2b), so one compare against b settles it.
# A zero divisor returns all ones, as the old restoring loop did.
__udivsi3:
    beq r4, r0, .udiv_divzero
    or r5, r3, r4
    blt r5, r0, .udiv_big
    div r1, r3, r4
    jalr r0, r31, 0

.udiv_big:
    blt r4, r0, .udiv_bigdiv
    srli r5, r3, 1
    div r5, r5, r4
    slli r1, r5, 1
    mul r6, r1, r4
    sub r6, r3, r6
    bltu r6, r4, .udiv_done
    addi r1, r1, 1
.udiv_done:
    jalr r0, r31, 0

.udiv_bigdiv:
    sltu r1, r3, r4
    xori r1, r1, 1
    jalr r0, r31, 0

.udiv_divzero:
    addi r1, r0, -1
    jalr r0, r31, 0

# Signed 32-bit division: the hardware divider, with the zero-divisor
# result (-1) the old helper returned.  INT_MIN / -1 gives INT_MIN in
# hardware, as it did through the unsigned helper.
__divsi3:
    beq r4, r0, .div_divzero
    div r1, r3, r4
    jalr r0, r31, 0

.div_divzero:
    addi r1, r0, -1
    jalr r0, r31, 0
