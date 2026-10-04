	# A fault stops the machine with a fault status, not whatever r1
	# holds (docs/SPEC.md 7.2): an illegal opcode is 132 (128 + SIGILL).
	# r1 is set to 7 first so the old behaviour -- exit 7 -- shows.
	.text
	.globl	main
	.p2align	2
main:
	li r1, 7
	.word	0x0000007E
	halt
