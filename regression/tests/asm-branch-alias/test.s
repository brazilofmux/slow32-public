	.text
	.globl main
	.p2align 2
main:
	addi r1, r0, 1
	addi r2, r0, 2

	bgt r2, r1, bgt_ok
	addi r3, r0, 0x58    # 'X' fail
	debug r3
	halt

bgt_ok:
	ble r1, r2, ble_ok
	addi r3, r0, 0x59    # 'Y' fail
	debug r3
	halt

ble_ok:
	addi r3, r0, 0x4F    # 'O'
	debug r3
	addi r3, r0, 0x4B    # 'K'
	debug r3
	addi r3, r0, 10      # '\n'
	debug r3
	halt
