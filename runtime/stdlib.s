# Minimal standard library functions for SLOW-32

.global putchar
.global puts
.global putint


# putchar(int c) - output a character
# r1 = character to print
putchar:
    debug r1
    jalr r0, lr, 0      # return

# puts(char *s) - output a string (pointer in r3)
# r3 = pointer to null-terminated string (matches compiler calling convention)
puts:
    addi sp, sp, -8
    stw  sp+0, lr
    stw  sp+4, r16      # save callee-saved register
    
    add  r16, r3, r0    # r16 = string pointer (from r3)
    
puts_loop:
    ldbu r1, r16+0      # load byte
    beq  r1, r0, puts_done  # if null, done
    debug r1            # print character
    addi r16, r16, 1    # advance pointer
    beq  r0, r0, puts_loop  # unconditional branch back
    
puts_done:
    # Print newline
    addi r1, r0, 10
    debug r1
    
    # Restore and return
    ldw  r16, sp+4
    ldw  lr, sp+0
    addi sp, sp, 8
    jalr r0, lr, 0

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
    slt  r2, r16, r0
    beq  r2, r0, putint_positive
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
    addi r2, r0, 10
    rem  r3, r16, r2
    addi r3, r3, 48     # convert to ASCII
    stb  r18+0, r3      # store digit
    addi r18, r18, 1
    addi r17, r17, 1
    
    # r16 / 10
    div  r16, r16, r2
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