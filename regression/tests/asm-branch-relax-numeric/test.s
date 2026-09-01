# Relaxation shifts label operands, relocations, label addresses and CFI
# ranges -- but a NUMERIC PC-relative displacement is baked into the
# encoding at parse time, so nothing walks it.  When the inserted jal lands
# between such an instruction and its target, the displacement has to move
# by four, or the branch silently goes four bytes wrong.
#
# Five cases, each with its own out-of-range branch to force one insertion:
#
#   C   control: branch and target both precede every insertion -- must NOT move
#   F   forward straddle: displacement grows by four
#   J   forward straddle through a numeric JAL
#   Jc  control through a numeric JAL whose target is one word SHORT of the
#       insertion point.  This is the case that separates the two bases -- a
#       conditional branch targets PC+4+imm, a JAL targets PC+imm.  Share one
#       base and this jal's target is computed four bytes high, crosses the
#       insertion point, and gets a shift it should never have had.  Case J
#       alone cannot see that: there the base slip and the target slip cancel.
#   B   backward straddle: displacement grows more negative by four
#
# Every landing site is bracketed: the word before it and the word after it
# are both poison jumps, so a displacement that drifts in either direction
# lands somewhere that prints and halts rather than somewhere that happens
# to work.  (An earlier draft put case B's success path directly after its
# branch, and a corrupted displacement fell straight into it.)
#
# Every "far" branch is written NOT taken (r5=7, r6=9), so control falls
# through it to the case below; the far labels are pure poison, reached only
# when a displacement landed on the inserted jal.
#
# Prints "AB\n" and exits 0.  Each failure path prints its own letter.
#
# Mutation-tested against relax_branches() in tools/assembler/slow32asm.c --
# each row is a separate build, and the letter is what this test printed:
#
#   numeric fixup loop disabled ......... 'f'   (F)   caught here only
#   jal displacements not fixed ......... 'j'   (J)   caught here only
#   one base for branch and jal ......... 'k'   (Jc)  caught here only
#   own address never shifts ............ 'j'   (J)   caught here only
#   target always shifts ................ 'c'   (C)   caught here only
#   forward straddles only .............. 'b'   (B)   caught here only
#   condition not inverted .............. 'k'   (Jc)  also caught by the
#                                                     other two relax tests
#
# Every case earns its place: no two mutations are caught by the same one,
# and asm-branch-relax / -cascade catch none of the first six.

	.text
	.globl main
	.p2align 2
main:
	addi r5, r0, 7
	addi r6, r0, 9

# --- C: control pair, both sides ahead of every insertion point below.
#     Bracketed on the far side by a RUN of poisons: an over-eager fixup
#     applies once per insertion, so the drift here is a multiple of four,
#     not just one word.  A single poison lets a big drift sail past.
	beq r5, r5, 4			# -> c_ok
	jal r0, fail_c			# drifted back, or not taken
c_ok:
	jal r0, c_cont			# exact
	jal r0, fail_c			# +4
	jal r0, fail_c			# +8
	jal r0, fail_c			# +12
	jal r0, fail_c			# +16
	jal r0, fail_c			# +20
c_cont:

# --- F: forward straddle.  +8 must become +12.
	beq r5, r5, 8			# -> f_ok, past the far branch and poison
	beq r5, r6, far_f		# relaxed: jal inserted right after
	jal r0, fail_f			# four bytes early
f_ok:
	jal r0, f_cont			# exact
	jal r0, fail_f			# four bytes late
f_cont:
	addi r3, r0, 0x41		# 'A'
	debug r3

# --- J: numeric JAL straddle.  +8 must become +12.
	jal r0, 8			# -> j_ok, past the far branch
	beq r5, r6, far_j		# relaxed; four bytes early lands on the
					# inserted jal, which goes to far_j
j_ok:
	jal r0, j_cont			# exact
	jal r0, fail_j			# four bytes late
j_cont:

# --- Jc: numeric JAL onto the far branch itself, one word short of the
#     insertion point.  Must NOT move.
	jal r0, 4			# -> the far branch on the next line
	beq r5, r6, far_jc		# relaxed; the inverted branch is taken,
					# so control falls past the inserted jal
jc_cont:
	# No poison word is needed here, and none belongs here: post-relax the
	# inverted branch itself lands on jc_cont, so a word placed between
	# them is on the main path, not off it.  A jal that wrongly moved
	# lands on the inserted jal instead -- which goes to far_jc.

# --- B: backward straddle.  -16 must become -20.
	jal r0, b_br
b_land:
	jal r0, b_ok			# exact
	jal r0, fail_b			# four bytes late
b_br:
	beq r5, r6, far_b		# relaxed: jal inserted right after
	beq r5, r5, -16			# -> b_land
	jal r0, fail_b			# not taken, or drifted forward

b_ok:
	addi r3, r0, 0x42		# 'B'
	debug r3

	addi r3, r0, 10
	debug r3
	addi r1, r0, 0
	halt

fail_c:
	addi r3, r0, 0x63		# 'c'
	debug r3
	addi r1, r0, 99
	halt
fail_f:
	addi r3, r0, 0x66		# 'f'
	debug r3
	addi r1, r0, 102
	halt
fail_j:
	addi r3, r0, 0x6A		# 'j'
	debug r3
	addi r1, r0, 106
	halt
fail_jc:
	addi r3, r0, 0x6B		# 'k'
	debug r3
	addi r1, r0, 107
	halt
fail_b:
	addi r3, r0, 0x62		# 'b'
	debug r3
	addi r1, r0, 98
	halt

	.space 6000

far_f:	jal r0, fail_f
far_j:	jal r0, fail_j
far_jc:	jal r0, fail_jc
far_b:	jal r0, fail_b
