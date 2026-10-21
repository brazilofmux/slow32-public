# Tiny guest used as an EXEC child. Prints 'x' then HALT.
.global _start
.text
_start:
    addi    r1, r0, 120     # 'x'
    debug   r1
    addi    r1, r0, 0
    halt
