# Minimal standard library functions for SLOW-32

# putchar and puts are in putchar.s and stdio_minimal.s
.global putint

# putint(int n) - output an integer in decimal
# r1 = integer to print
putint:
    addi sp, sp, -16
    stw  sp+0, lr
    stw  sp+4, r16
    stw  sp+8, r17
    stw  sp+12, r18
    
    add  r16, r1, r0    # r16 = number
    addi r17, r0, 0     # r17 = digit count
    addi r18, sp, -32   # r18 = buffer pointer (below stack)
    
    # Handle negative
    slt  r4, r16, r0
    beq  r4, r0, putint_positive
    addi r1, r0, 45     # '-'
    debug r1
    sub  r16, r0, r16   # make positive
    
putint_positive:
    # Handle zero special case
    bne  r16, r0, putint_convert
    addi r1, r0, 48     # '0'
    debug r1
    beq  r0, r0, putint_done  # unconditional branch
    
putint_convert:
    # Convert to digits (backwards)
    beq  r16, r0, putint_print
    
    # r16 % 10
    addi r4, r0, 10
    rem  r3, r16, r4
    addi r3, r3, 48     # convert to ASCII
    stb  r18+0, r3      # store digit
    addi r18, r18, 1
    addi r17, r17, 1
    
    # r16 / 10
    div  r16, r16, r4
    beq  r0, r0, putint_convert  # unconditional branch back
    
putint_print:
    # Print digits (reverse order)
    beq  r17, r0, putint_done
    addi r18, r18, -1
    ldbu r1, r18+0
    debug r1
    addi r17, r17, -1
    beq  r0, r0, putint_print  # unconditional branch back
    
putint_done:
    ldw  r18, sp+12
    ldw  r17, sp+8
    ldw  r16, sp+4
    ldw  lr, sp+0
    addi sp, sp, 16
    jalr r0, lr, 0
