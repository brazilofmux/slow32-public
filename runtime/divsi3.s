    .global __udivsi3
    .global __divsi3
    
__udivsi3:
    # Unsigned 32-bit division
    # r3 = dividend, r4 = divisor
    # Returns quotient in r1
    
    addi r1, r0, 0      # quotient = 0
    addi r5, r0, 32     # bit counter
    
.udiv_loop:
    beq r5, r0, .udiv_done
    
    # Shift quotient left
    slli r1, r1, 1
    
    # Check if divisor <= dividend
    sleu r6, r4, r3
    beq r6, r0, .udiv_skip
    
    # Subtract divisor from dividend
    sub r3, r3, r4
    ori r1, r1, 1       # Set low bit of quotient
    
.udiv_skip:
    slli r3, r3, 1      # Shift dividend left
    addi r5, r5, -1
    j .udiv_loop
    
.udiv_done:
    jalr r0, r31, 0

__divsi3:
    # For now, just call unsigned version
    # TODO: Add signed division support
    j __udivsi3