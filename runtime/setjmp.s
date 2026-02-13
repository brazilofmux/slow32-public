# SLOW-32 setjmp/longjmp implementation
# jmp_buf is an array of 21 ints storing R11-R28, SP(R29), FP(R30), LR(R31)

.global setjmp
.global longjmp

# int setjmp(jmp_buf env)
# env pointer in R3
# Saves callee-saved registers and returns 0
setjmp:
    stw  r3+0,  r11
    stw  r3+4,  r12
    stw  r3+8,  r13
    stw  r3+12, r14
    stw  r3+16, r15
    stw  r3+20, r16
    stw  r3+24, r17
    stw  r3+28, r18
    stw  r3+32, r19
    stw  r3+36, r20
    stw  r3+40, r21
    stw  r3+44, r22
    stw  r3+48, r23
    stw  r3+52, r24
    stw  r3+56, r25
    stw  r3+60, r26
    stw  r3+64, r27
    stw  r3+68, r28
    stw  r3+72, r29     # SP
    stw  r3+76, r30     # FP
    stw  r3+80, r31     # LR
    add  r1, r0, r0     # return 0
    jalr r0, r31, 0

# void longjmp(jmp_buf env, int val)
# env in R3, val in R4
# Restores all saved registers, returns val (or 1 if val==0)
longjmp:
    # Ensure val != 0 (C standard: longjmp must return non-zero)
    bne  r4, r0, .Lval_ok
    addi r4, r0, 1
.Lval_ok:
    add  r1, r4, r0     # set return value
    ldw  r11, r3+0
    ldw  r12, r3+4
    ldw  r13, r3+8
    ldw  r14, r3+12
    ldw  r15, r3+16
    ldw  r16, r3+20
    ldw  r17, r3+24
    ldw  r18, r3+28
    ldw  r19, r3+32
    ldw  r20, r3+36
    ldw  r21, r3+40
    ldw  r22, r3+44
    ldw  r23, r3+48
    ldw  r24, r3+52
    ldw  r25, r3+56
    ldw  r26, r3+60
    ldw  r27, r3+64
    ldw  r28, r3+68
    ldw  r29, r3+72     # SP
    ldw  r30, r3+76     # FP
    ldw  r31, r3+80     # LR (return address of setjmp caller)
    jalr r0, r31, 0     # jump back to setjmp caller
