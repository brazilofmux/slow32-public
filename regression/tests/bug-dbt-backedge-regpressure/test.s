# Directed stress test for DBT in-block back-edge optimization under
# register pressure (ISSUES.md #11 trigger from the RV32IM sister project).
#
# The hazard: a self-loop whose body uses MORE distinct guest registers than
# the DBT has host register slots (x86: 8, a64: 7). Mid-block evictions can
# desynchronize the register mapping the back-edge jump expects at the loop
# head. The sister project silently truncated unrolled struct copies.
#
# Test 1: 10-reg unrolled copy loop, block starts AT the loop head.
# Test 2: 11-reg unrolled copy loop, loop head is MID-block (fall-through
#         entry), one loop-invariant read-only register in the body.
# Test 3: low-pressure control loop (should take the "stable" fast path).
# Test 4: copy loop with a rarely-taken forward branch (deferred side exit)
#         that fires after ~30 back-edge iterations - stresses the deferred
#         exit dirty-snapshot promotion.
# Test 5: prescan-invisible back-edge. The loop's backward branch sits BEYOND
#         a `j` (jal r0) that the block prescan stops at, so the loop head is
#         never in backedge_targets - yet superblock jump-over inlining still
#         reaches the backward branch, and an ungated in-block back-edge jump
#         to the loop head's pc_map offset lands on a lazily-emitted pending
#         write flush (str W0 -> guest reg) that re-executes every iteration
#         with clobbered scratch, silently corrupting the register. This is
#         the shape that corrupted r13 in cpp-exception-basic's DWARF CFI
#         interpreter loop.
#
# Success output: "12345\nOK\n".  On failure: "F<digit>\n" and halt.

	.text
	.globl main
	.p2align 2
main:
	# Buffers on the stack (writable region below sp):
	#   r1 = src  = sp-2048 (240 bytes)
	#   r2 = dst  = sp-1536 (240 bytes)
	addi	r1, sp, -2048
	addi	r2, sp, -1536

	# Fill src[0..59] with pattern v(i) = 777 + i*1013
	addi	r3, r0, 60
	addi	r5, r0, 777
	addi	r4, r0, 1013
	addi	r6, r1, 0
fill:
	stw	r6+0, r5
	add	r5, r5, r4
	addi	r6, r6, 4
	addi	r3, r3, -1
	bne	r3, r0, fill

# ---------------- Test 1: 10 distinct regs, block starts at loop head ------
	addi	r17, r1, 0		# src cursor
	addi	r18, r2, 0		# dst cursor
	addi	r19, r1, 240		# src end
	addi	r20, r0, 0		# running checksum
	beq	r0, r0, t1loop		# jump so the block STARTS at t1loop
t1loop:
	ldw	r11, 0(r17)
	ldw	r12, 4(r17)
	ldw	r13, 8(r17)
	ldw	r14, 12(r17)
	ldw	r15, 16(r17)
	ldw	r16, 20(r17)
	stw	r18+0, r11
	stw	r18+4, r12
	stw	r18+8, r13
	stw	r18+12, r14
	stw	r18+16, r15
	stw	r18+20, r16
	add	r20, r20, r11
	add	r20, r20, r12
	add	r20, r20, r13
	add	r20, r20, r14
	add	r20, r20, r15
	add	r20, r20, r16
	addi	r17, r17, 24
	addi	r18, r18, 24
	bltu	r17, r19, t1loop

	# Verify: dst == src word-for-word, and recomputed sum == r20
	addi	r6, r1, 0
	addi	r7, r2, 0
	addi	r8, r0, 0
	addi	r9, r0, 60
v1loop:
	ldw	r21, 0(r6)
	ldw	r22, 0(r7)
	bne	r21, r22, fail1
	add	r8, r8, r22
	addi	r6, r6, 4
	addi	r7, r7, 4
	addi	r9, r9, -1
	bne	r9, r0, v1loop
	bne	r8, r20, fail1
	addi	r28, r0, 0x31		# '1'
	debug	r28

# ---------------- Test 2: 11 distinct regs, loop head mid-block ------------
	# Copy dst -> buf3 (sp-1024), XORing with an invariant mask, then
	# XOR back during verify. The DEBUG above ended the previous block,
	# so this init falls through INTO the loop head: back-edge targets a
	# mid-block pc.
	addi	r9, r2, 0		# src cursor (= dst of test 1)
	addi	r10, sp, -1024		# buf3 cursor
	addi	r21, r2, 240		# src end
	addi	r22, r0, 0		# running checksum
	addi	r23, r0, 1365		# invariant XOR mask (0x555)
t2loop:
	ldw	r3, 0(r9)
	ldw	r4, 4(r9)
	ldw	r5, 8(r9)
	ldw	r6, 12(r9)
	ldw	r7, 16(r9)
	ldw	r8, 20(r9)
	xor	r3, r3, r23
	xor	r4, r4, r23
	xor	r5, r5, r23
	xor	r6, r6, r23
	xor	r7, r7, r23
	xor	r8, r8, r23
	stw	r10+0, r3
	stw	r10+4, r4
	stw	r10+8, r5
	stw	r10+12, r6
	stw	r10+16, r7
	stw	r10+20, r8
	add	r22, r22, r3
	add	r22, r22, r4
	add	r22, r22, r5
	add	r22, r22, r6
	add	r22, r22, r7
	add	r22, r22, r8
	addi	r9, r9, 24
	addi	r10, r10, 24
	bltu	r9, r21, t2loop

	# Verify: buf3[i] ^ mask == src[i], recomputed sum == r22
	addi	r6, r1, 0
	addi	r7, sp, -1024
	addi	r8, r0, 0
	addi	r9, r0, 60
v2loop:
	ldw	r11, 0(r6)
	ldw	r12, 0(r7)
	add	r8, r8, r12
	xor	r12, r12, r23
	bne	r11, r12, fail2
	addi	r6, r6, 4
	addi	r7, r7, 4
	addi	r9, r9, -1
	bne	r9, r0, v2loop
	bne	r8, r22, fail2
	addi	r28, r0, 0x32		# '2'
	debug	r28

# ---------------- Test 3: low pressure (5 regs) - stable fast path ---------
	addi	r3, r0, 0		# sum
	addi	r4, r0, 1		# i
	addi	r5, r0, 100		# limit
t3loop:
	add	r3, r3, r4
	addi	r4, r4, 1
	sle	r6, r4, r5
	bne	r6, r0, t3loop
	# expect sum == 5050
	addi	r7, r0, 1010
	slli	r8, r7, 2
	add	r8, r8, r7
	bne	r3, r8, fail3
	addi	r28, r0, 0x33		# '3'
	debug	r28

# ---------------- Test 4: side exit fires after ~30 iterations -------------
	# Copy src -> buf4 (sp-512) word-at-a-time; inside the loop a forward
	# branch fires exactly once, when the loaded word == 777+30*1013 =
	# 31167. The handler bumps r25. Checks: copy correct AND r25 == 1.
	addi	r17, r1, 0		# src cursor
	addi	r18, sp, -512		# buf4 cursor
	addi	r19, r1, 240		# src end
	addi	r25, r0, 0		# sentinel hit counter
	addi	r26, r0, 1947		# build 31167 = (1947<<4)|15
	slli	r26, r26, 4
	addi	r26, r26, 15
	addi	r24, r0, 0		# running checksum
t4loop:
	ldw	r11, 0(r17)
	beq	r11, r26, t4hit		# forward branch, taken exactly once
t4res:
	stw	r18+0, r11
	add	r24, r24, r11
	addi	r17, r17, 4
	addi	r18, r18, 4
	bltu	r17, r19, t4loop
	beq	r0, r0, t4chk
t4hit:
	addi	r25, r25, 1
	beq	r0, r0, t4res
t4chk:
	addi	r6, r0, 1
	bne	r25, r6, fail4
	addi	r6, r1, 0
	addi	r7, sp, -512
	addi	r8, r0, 0
	addi	r9, r0, 60
v4loop:
	ldw	r11, 0(r6)
	ldw	r12, 0(r7)
	bne	r11, r12, fail4
	add	r8, r8, r12
	addi	r6, r6, 4
	addi	r7, r7, 4
	addi	r9, r9, -1
	bne	r9, r0, v4loop
	bne	r8, r24, fail4
	addi	r28, r0, 0x34		# '4'
	debug	r28

# ---------------- Test 5: prescan-invisible back-edge + pending write ------
	# The DEBUG above ends the previous block, so this init starts a fresh
	# block. Register pressure: r5,r6,r9,r10,r11,r12,r13 all have >=2 uses
	# in the prescan window so they win the 7 cache slots; r24 (1 use) and
	# r1 stay uncached. r24's write is the pending-write victim.
	addi	r6, r0, 0		# i
	addi	r9, r0, 10		# limit
	addi	r10, r0, 77		# filler
	add	r12, r9, r10		# filler (2 uses)
	add	r13, r9, r10		# filler (2 uses)
	addi	r24, sp, -256		# VICTIM: pending write, r24 uncached
t5loop:
	add	r5, r6, r10		# loop head (cached ops: no scratch use)
	add	r5, r5, r12
	add	r5, r5, r13
	ldw	r11, 0(r1)		# uncached r1: scratch load flushes the
	add	r11, r11, r5		#   pending r24 write mid-loop
	bne	r6, r9, t5over		# forward branch...
	j	t5done			# ...over a plain jump: prescan stops HERE
t5over:
	addi	r6, r6, 1
	beq	r0, r0, t5loop		# backward branch the prescan never saw
t5done:
	addi	r7, sp, -256
	bne	r24, r7, fail5		# r24 must still be sp-256
	addi	r7, r0, 10
	bne	r6, r7, fail5
	addi	r28, r0, 0x35		# '5'
	debug	r28

	# All passed
	addi	r28, r0, 10		# '\n'
	debug	r28
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
failout:
	addi	r28, r0, 0x46		# 'F'
	debug	r28
	debug	r27
	addi	r28, r0, 10
	debug	r28
	addi	r1, r0, 1
	halt
