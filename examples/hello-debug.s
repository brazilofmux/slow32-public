        .section .rodata
msg:    .ascii  "Hello, SLOW-32!\n\0"

        .section .text
        .globl  _start
_start:
        li      r3, msg
.loop:
        ldbu    r2, r3+0
        beq     r2, r0, .done
        debug   r2
        addi    r3, r3, 1
        jal     r0, .loop
.done:
        halt