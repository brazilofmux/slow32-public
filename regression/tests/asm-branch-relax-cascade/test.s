# Branch relaxation is iterated to a fixed point: the four bytes one
# insertion adds can push a *different* branch, previously in range, out of
# it.  This pins that cascade.
#
#   P: beq r5, r5, p_target      offset +4092 -- in range by one word
#      .space 4088
#   Q: beq r5, r6, q_target      far: relaxed on the first pass, and the
#                                jal it inserts sits exactly at p_target
#   p_target:                    ... so P's offset becomes +4096, and a
#                                second pass has to relax P as well.
#
# Prints "AB\n" and exits 0.
#
# Mutation-tested: capping relax_branches() at one pass makes this fail to
# assemble -- "Branch offset out of range at address 0x00000008", which is
# P.  It is the only one of the three relax tests that mutation reaches.

	.text
	.globl main
	.p2align 2
main:
	addi r5, r0, 7
	addi r6, r0, 9

	# P -- in range as written, out of range once Q relaxes.
	beq r5, r5, p_target

	.space 4088

	# Q -- far, relaxed on the first pass.  Never executed: P jumps over
	# it.  Not taken as written, so a mis-shifted P that falls through
	# still lands somewhere visible rather than looping.
	beq r5, r6, q_target

p_target:
	addi r3, r0, 0x41		# 'A'
	debug r3
	jal r0, done

	.space 6000

q_target:
	addi r3, r0, 0x71		# 'q' - P landed on Q's inserted jal
	debug r3
	addi r1, r0, 113
	halt

done:
	addi r3, r0, 0x42		# 'B'
	debug r3
	addi r3, r0, 10
	debug r3
	addi r1, r0, 0
	halt
