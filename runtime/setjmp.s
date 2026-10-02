# SLOW-32 setjmp/longjmp implementation
#
# One source for both C libraries: the self-hosted library assembles this
# file too (selfhost/stage08/build-s12cc.sh), with its own assembler --
# which reads "stw base, src, offset" and not the "base+offset" spelling,
# so that is how the loads and stores are written.  The registers a
# callee keeps are the same r11-r28 under both compilers.
# jmp_buf is an array of 21 ints storing R11-R28, SP(R29), FP(R30), LR(R31)

.global setjmp
.global longjmp

# int setjmp(jmp_buf env)
# env pointer in R3
# Saves callee-saved registers and returns 0
setjmp:
    stw  r3, r11, 0
    stw  r3, r12, 4
    stw  r3, r13, 8
    stw  r3, r14, 12
    stw  r3, r15, 16
    stw  r3, r16, 20
    stw  r3, r17, 24
    stw  r3, r18, 28
    stw  r3, r19, 32
    stw  r3, r20, 36
    stw  r3, r21, 40
    stw  r3, r22, 44
    stw  r3, r23, 48
    stw  r3, r24, 52
    stw  r3, r25, 56
    stw  r3, r26, 60
    stw  r3, r27, 64
    stw  r3, r28, 68
    stw  r3, r29, 72     # SP
    stw  r3, r30, 76     # FP
    stw  r3, r31, 80     # LR
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
    ldw  r11, r3, 0
    ldw  r12, r3, 4
    ldw  r13, r3, 8
    ldw  r14, r3, 12
    ldw  r15, r3, 16
    ldw  r16, r3, 20
    ldw  r17, r3, 24
    ldw  r18, r3, 28
    ldw  r19, r3, 32
    ldw  r20, r3, 36
    ldw  r21, r3, 40
    ldw  r22, r3, 44
    ldw  r23, r3, 48
    ldw  r24, r3, 52
    ldw  r25, r3, 56
    ldw  r26, r3, 60
    ldw  r27, r3, 64
    ldw  r28, r3, 68
    ldw  r29, r3, 72     # SP
    ldw  r30, r3, 76     # FP
    ldw  r31, r3, 80     # LR (return address of setjmp caller)
    jalr r0, r31, 0     # jump back to setjmp caller
