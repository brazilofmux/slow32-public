	.file	"test.s"
	.text
	.globl	main
	.p2align	2
	.type	main,@function
main:
	# Trigger pseudo-op expansion (currently emits LUI opcode 0x25)
	li r1, 0x12345678
	la r2, target
	# If we ever get here, stop cleanly
	halt

	.section	.rodata
	.p2align	2
target:
	.word	0xDEADBEEF
