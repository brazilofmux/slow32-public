# Directed test for DBT select-idiom fusion (the idiom contract's first
# entry — see docs/plans/engine-room.md).
#
# The LLVM backend's canonical branchless select is
#     C  = setcc(a, b)
#     M  = sub r0, C          ; r0-anchored negation
#     t2 = xor T, F
#     u  = and t2, M
#     rd = xor F, u           ; = C ? T : F
# The DBT recognizes this shape in its prescan and replaces the final XOR
# with CMP+CSEL (a64) / CMP+CMOVcc (x64). This test proves the fused code
# still computes the select correctly across operand orders, interleaved
# instructions, condition kinds, and both condition outcomes — and that
# the allocated-zero negative control (sub from a register that merely
# holds 0) stays correct when the DBT refuses to fuse it.
#
# Values are round-tripped through the stack so translate-time constant
# propagation cannot fold the comparisons away.
#
# Success output: "OK\n". On failure: "F<digit>\n".

	.text
	.globl main
	.p2align 2
main:
	# Opaque values: r5=7, r6=9, r10=-5, r16=0xFFFFFFF0
	addi	r1, r0, 7
	stw	sp+-4, r1
	addi	r1, r0, 9
	stw	sp+-8, r1
	addi	r1, r0, -5
	stw	sp+-12, r1
	addi	r1, r0, -16
	stw	sp+-16, r1
	ldw	r5, -4(sp)
	ldw	r6, -8(sp)
	ldw	r10, -12(sp)
	ldw	r16, -16(sp)

	# T = 0x54 ('T'), F = 0x46 ('F') — also opaque
	addi	r1, r0, 0x54
	stw	sp+-20, r1
	addi	r1, r0, 0x46
	stw	sp+-24, r1
	ldw	r11, -20(sp)
	ldw	r12, -24(sp)

	# --- 1: canonical order, SEQ true (7 == 7) -> T ---
	seq	r7, r5, r5
	sub	r8, r0, r7
	xor	r13, r11, r12
	and	r14, r13, r8
	xor	r15, r12, r14
	bne	r15, r11, fail1

	# --- 2: final XOR operands swapped, SEQ false (7 == 9) -> F ---
	seq	r7, r5, r6
	sub	r8, r0, r7
	xor	r13, r11, r12
	and	r14, r13, r8
	xor	r15, r14, r12
	bne	r15, r12, fail2

	# --- 3: AND operands swapped, SLT true (-5 < 7 signed) -> T ---
	slt	r7, r10, r5
	sub	r8, r0, r7
	xor	r13, r11, r12
	and	r14, r8, r13
	xor	r15, r12, r14
	bne	r15, r11, fail3

	# --- 4: unrelated inst interleaved, SGTU true (0xFFFFFFF0 >u 9) -> T ---
	sgtu	r7, r16, r6
	sub	r8, r0, r7
	addi	r17, r0, 123		# unrelated, between mask and blend
	xor	r13, r11, r12
	and	r14, r13, r8
	xor	r15, r12, r14
	bne	r15, r11, fail4

	# --- 5: T^F computed before the mask, SLE false (9 <= 7) -> F ---
	sle	r7, r6, r5
	xor	r13, r11, r12
	sub	r8, r0, r7
	and	r14, r13, r8
	xor	r15, r12, r14
	bne	r15, r12, fail5

	# --- 6: negative control — allocated zero, must refuse to fuse
	#        but still compute correctly. SNE true (7 != 9) -> T ---
	add	r20, r0, r0		# r20 holds 0 but is not r0
	sne	r7, r5, r6
	sub	r8, r20, r7
	xor	r13, r11, r12
	and	r14, r13, r8
	xor	r15, r12, r14
	bne	r15, r11, fail6

	# --- 7: benchmark shape — regalloc reuses the setcc operand's
	#        register for cond AND mask; operand rematerialized from
	#        its ANDI def. (7&2)==2 -> false... (7&2)=2, seq vs r21=2
	#        -> true -> T ---
	addi	r1, r0, 2
	stw	sp+-28, r1
	ldw	r21, -28(sp)		# r21 = 2, opaque
	andi	r7, r5, 2		# a = 7 & 2 = 2 (r7)
	seq	r7, r7, r21		# C into a's own register
	xor	r13, r11, r12
	sub	r7, r0, r7		# M also into r7 — a and C both dead
	and	r14, r13, r7
	xor	r15, r12, r14
	bne	r15, r11, fail7

	# --- 8: in-place inner XOR — t2 overwrites T's register; the DBT
	#        must recover T = t2 ^ F. SLT false (7 < -5 signed) -> F ---
	slt	r7, r5, r10
	sub	r8, r0, r7
	add	r22, r11, r0		# T in r22
	xor	r22, r22, r12		# t2 = T ^ F, in place over T
	and	r14, r22, r8
	xor	r15, r12, r14
	bne	r15, r12, fail8

	# --- 9: in-place inner XOR, condition true side. SGE true
	#        (7 >= -5 signed) -> T ---
	sge	r7, r5, r10
	sub	r8, r0, r7
	add	r22, r11, r0		# T in r22
	xor	r22, r22, r12		# t2 destroys T
	and	r14, r22, r8
	xor	r15, r12, r14
	bne	r15, r11, fail9

	# --- 10: negative control — operand clobbered by an FMT_R op
	#         (not rematerializable), must refuse but stay correct.
	#         SLTU true (7 <u 9) -> T ---
	add	r23, r5, r0		# a = 7 in r23
	sltu	r7, r23, r6
	sub	r8, r0, r7
	add	r23, r6, r6		# clobber a with an FMT_R def
	xor	r13, r11, r12
	and	r14, r13, r8
	xor	r15, r12, r14
	bne	r15, r11, fail10

	addi	r28, r0, 0x4F		# 'O'
	debug	r28
	addi	r28, r0, 0x4B		# 'K'
	debug	r28
	addi	r28, r0, 10
	debug	r28
	addi	r1, r0, 0
	halt

fail1:
	addi	r27, r0, 0x31
	beq	r0, r0, failout
fail2:
	addi	r27, r0, 0x32
	beq	r0, r0, failout
fail3:
	addi	r27, r0, 0x33
	beq	r0, r0, failout
fail4:
	addi	r27, r0, 0x34
	beq	r0, r0, failout
fail5:
	addi	r27, r0, 0x35
	beq	r0, r0, failout
fail6:
	addi	r27, r0, 0x36
	beq	r0, r0, failout
fail7:
	addi	r27, r0, 0x37
	beq	r0, r0, failout
fail8:
	addi	r27, r0, 0x38
	beq	r0, r0, failout
fail9:
	addi	r27, r0, 0x39
	beq	r0, r0, failout
fail10:
	addi	r27, r0, 0x41
failout:
	addi	r28, r0, 0x46		# 'F'
	debug	r28
	debug	r27
	addi	r28, r0, 10
	debug	r28
	addi	r1, r0, 1
	halt
