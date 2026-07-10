# Directed test for the AArch64 DBT shifted-EOR fold miscompile.
#
# The a64 translator folds a preceding SLLI/SRLI/SRAI into the next XOR's
# shifted-register operand (EOR Wd, Wn, Wm, <shift> #imm), recomputing the
# shift from the SOURCE register. That is only valid when the shift was not
# in-place: `slli r5, r5, 1` overwrites its own source, so the fold applied
# the shift a second time (r6 = r7 ^ (r5 << 2) instead of << 1).
#
# Found by --paranoid-lite on lua's branchless min/max in luaD_growstack,
# which silently grew the stack by 4x instead of 2x — no test failed.
#
# Values are round-tripped through the stack so constant propagation can't
# fold the shift/xor at translation time (the fold only fires on register
# operands).
#
# Success output: "OK\n". On failure: "F<digit>\n".

	.text
	.globl main
	.p2align 2
main:
	# Opaque values: r7=3, r10=5, r11=80, r12=-32 via stack round-trip
	addi	r1, r0, 3
	stw	sp+-4, r1
	addi	r1, r0, 5
	stw	sp+-8, r1
	addi	r1, r0, 80
	stw	sp+-12, r1
	addi	r1, r0, -32
	stw	sp+-16, r1
	ldw	r7, -4(sp)
	ldw	r10, -8(sp)
	ldw	r11, -12(sp)
	ldw	r12, -16(sp)

	# --- 1: in-place SLLI + XOR ---
	add	r5, r10, r0		# r5 = 5 (not a tracked constant)
	slli	r5, r5, 1		# r5 = 10
	xor	r6, r5, r7		# r6 = 10 ^ 3 = 9 (buggy fold: 20 ^ 3 = 23)
	addi	r8, r0, 9
	bne	r6, r8, fail1
	addi	r8, r0, 10
	bne	r5, r8, fail1		# r5 itself must be 10

	# --- 2: in-place SRLI + XOR (operands swapped) ---
	add	r5, r11, r0		# r5 = 80
	srli	r5, r5, 2		# r5 = 20
	xor	r6, r7, r5		# r6 = 3 ^ 20 = 23
	addi	r8, r0, 23
	bne	r6, r8, fail2

	# --- 3: in-place SRAI + XOR ---
	add	r5, r12, r0		# r5 = -32
	srai	r5, r5, 3		# r5 = -4
	xor	r6, r5, r7		# r6 = -4 ^ 3 = -1
	addi	r8, r0, -1
	bne	r6, r8, fail3

	# --- 4: control, non-in-place shift (fold is legal here) ---
	add	r5, r10, r0		# r5 = 5
	slli	r9, r5, 1		# r9 = 10, r5 preserved
	xor	r6, r9, r7		# r6 = 10 ^ 3 = 9
	addi	r8, r0, 9
	bne	r6, r8, fail4
	addi	r8, r0, 5
	bne	r5, r8, fail4

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
failout:
	addi	r28, r0, 0x46		# 'F'
	debug	r28
	debug	r27
	addi	r28, r0, 10
	debug	r28
	addi	r1, r0, 1
	halt
