	.text
	.globl main
	.p2align 2
main:
	addi sp, sp, -16
	addi r1, r0, 0x53    # 'S'
	stw sp+4, r1
	ldw r2, 4(sp)
	beq r1, r2, ok
	addi r3, r0, 0x46    # 'F'
	debug r3
	halt
ok:
	addi r3, r0, 0x53    # 'S'
	debug r3
	addi r3, r0, 10      # '\n'
	debug r3
	halt
