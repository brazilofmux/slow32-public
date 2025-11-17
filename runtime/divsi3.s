    .global __udivsi3
    .global __divsi3

# Unsigned 32-bit division using restoring division algorithm
# r3 = dividend, r4 = divisor, result in r1
__udivsi3:
    beq r4, r0, .udiv_divzero
    addi r1, r0, 0      # quotient
    addi r5, r0, 0      # remainder
    addi r6, r0, 32     # bit counter

.udiv_loop:
    beq r6, r0, .udiv_done
    addi r6, r6, -1
    slli r5, r5, 1
    srli r7, r3, 31
    or r5, r5, r7
    slli r3, r3, 1
    slli r1, r1, 1
    bltu r5, r4, .udiv_loop
    sub r5, r5, r4
    ori r1, r1, 1
    beq r0, r0, .udiv_loop

.udiv_done:
    jalr r0, r31, 0

.udiv_divzero:
    addi r1, r0, -1
    jalr r0, r31, 0

# Signed 32-bit division implemented via unsigned helper
# r3 = dividend, r4 = divisor, result in r1
__divsi3:
    beq r4, r0, .div_divzero
    addi r5, r0, 0          # track if result negative
    blt r3, r0, .div_neg_dividend
    beq r0, r0, .div_check_divisor

.div_neg_dividend:
    sub r3, r0, r3
    xori r5, r5, 1

.div_check_divisor:
    bge r4, r0, .div_do
    sub r4, r0, r4
    xori r5, r5, 1

.div_do:
    jal __udivsi3
    beq r5, r0, .div_done
    sub r1, r0, r1
    jalr r0, r31, 0

.div_done:
    jalr r0, r31, 0

.div_divzero:
    addi r1, r0, -1
    jalr r0, r31, 0
