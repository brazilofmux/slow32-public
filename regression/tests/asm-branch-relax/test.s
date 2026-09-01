# Branch relaxation: a conditional branch reaches only +/-4096 bytes, so the
# assembler rewrites an out-of-range one as an inverted branch over a jal.
# This pins the three cases that must come out right: a far FORWARD branch,
# a far BACKWARD branch, and a near branch that must be left alone.
#
# Prints "AB\n" and exits 0.  Every failure path prints its own letter and
# exits with a distinct code, so a break says which case broke.
#
# Mutation-tested: dropping the condition inversion in relax_branches()
# makes this print 'n' and exit 110.  The sibling cases cover what this
# one does not -- asm-branch-relax-cascade the fixed-point iteration,
# asm-branch-relax-numeric the numeric-displacement shifts.

	.text
	.globl main
	.p2align 2
main:
	addi r5, r0, 7
	addi r6, r0, 9

	# (1) Near branch, comfortably in range: must be encoded verbatim.
	#     If relaxation touched it, control reaches near_bad.
	beq r5, r6, near_bad		# not taken (7 != 9)
	addi r3, r0, 0x41		# 'A'
	debug r3

	# (2) Far FORWARD branch, taken: inverted, with a jal to 'fwd'.
	beq r5, r5, fwd

near_bad:
	addi r3, r0, 0x6E		# 'n'
	debug r3
	addi r1, r0, 110
	halt

	.space 6000

	# (3) Landing site for the far BACKWARD branch below.
back:
	addi r3, r0, 0x42		# 'B'
	debug r3
	addi r3, r0, 10
	debug r3
	addi r1, r0, 0
	halt

	.space 6000

fwd:
	# Reached by (2).  Now branch far BACKWARD, also out of range.
	bne r5, r6, back		# taken (7 != 9)
	addi r3, r0, 0x62		# 'b' - backward relaxation fell through
	debug r3
	addi r1, r0, 98
	halt
