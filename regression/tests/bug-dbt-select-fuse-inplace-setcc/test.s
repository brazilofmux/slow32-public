# Directed test for the select-fusion in-place-setcc miscompile (both DBT
# back ends: translate_a64.c and translate.c).
#
# Both translators recognize LLVM's branchless select idiom
#
#     C  = setcc a, b
#     M  = sub r0, C
#     t2 = xor T, F
#     u  = and t2, M
#     rd = xor F, u          ; = C ? T : F
#
# and replace the final XOR with CMP a,b + CSEL/CMOV. The re-materialized
# compare reads a and b at the FUSION POINT, so both must still hold their
# original values there. The recognizer checked that with defined_between(),
# which scans defs strictly AFTER the setcc — and therefore cannot see the
# setcc overwriting its own compare operand:
#
#     addi r3, r0, 255
#     sgtu r3, r1, r3        # in place: r3 is now the boolean, not 255
#     ...
#     xor  r17, r1, r4       # fused CMP r1, r3 compares against 0/1
#
# Found by --paranoid-lite on lua's lparser.c (test-control repeat/until
# printed 2 instead of 128).
#
# Values are round-tripped through the stack so the idiom is built from
# register operands rather than tracked constants.
#
# Success output: "OK\n". On failure: "F<digit>\n".

	.text
	.globl main
	.p2align 2
main:
	# Opaque values: r7=100, r10=227, r11=300 via stack round-trip
	addi	r1, r0, 100
	stw	sp+-4, r1
	addi	r1, r0, 227
	stw	sp+-8, r1
	addi	r1, r0, 300
	stw	sp+-12, r1
	ldw	r7, -4(sp)
	ldw	r10, -8(sp)
	ldw	r11, -12(sp)

	# --- 1: in-place SGTU, condition FALSE (100 >u 255 is false) ---
	# Correct: r17 = F = r7 = 100.
	# Buggy:   CMP r7, r3 where r3 is the boolean 0 -> 100 >u 0 -> true
	#          -> picks T = r10 = 227.
	add	r5, r7, r0		# F = 100
	add	r6, r10, r0		# T = 227
	addi	r3, r0, 255
	sgtu	r3, r5, r3		# C = 0   (in place: destroys the 255)
	xor	r4, r6, r5		# t2 = T ^ F
	sub	r2, r0, r3		# M = 0
	and	r4, r4, r2		# u = 0
	xor	r17, r5, r4		# rd = F = 100
	addi	r8, r0, 100
	bne	r17, r8, fail1

	# --- 2: in-place SGTU, condition TRUE (300 >u 255) ---
	# Correct: r17 = T = 227. Same answer either way; guards against a
	# fix that simply inverts the condition.
	add	r5, r11, r0		# F = 300
	add	r6, r10, r0		# T = 227
	addi	r3, r0, 255
	sgtu	r3, r5, r3		# C = 1
	xor	r4, r6, r5
	sub	r2, r0, r3		# M = 0xFFFFFFFF
	and	r4, r4, r2
	xor	r17, r5, r4		# rd = T = 227
	addi	r8, r0, 227
	bne	r17, r8, fail2

	# --- 3: in-place SLT on the FIRST operand ---
	# `slt r5, r5, r6` clobbers a, not b. Correct: 100 <s 227 -> T = 227.
	# Buggy: CMP boolean(1) <s 227 -> also true, so make the T/F choice
	# the discriminator by re-deriving F from the clobbered register.
	add	r5, r7, r0		# a = F = 100
	add	r6, r10, r0		# b = T = 227
	add	r9, r5, r0		# keep an unclobbered copy of F
	slt	r5, r5, r6		# C = 1  (in place on a)
	xor	r4, r6, r9		# t2 = T ^ F
	sub	r2, r0, r5		# M = 0xFFFFFFFF
	and	r4, r4, r2
	xor	r17, r9, r4		# rd = T = 227
	addi	r8, r0, 227
	bne	r17, r8, fail3

	# --- 4: control, setcc writes a third register (fusion is legal) ---
	add	r5, r7, r0		# F = 100
	add	r6, r10, r0		# T = 227
	addi	r3, r0, 255
	sgtu	r12, r5, r3		# C = 0, 255 preserved in r3
	xor	r4, r6, r5
	sub	r2, r0, r12
	and	r4, r4, r2
	xor	r17, r5, r4		# rd = F = 100
	addi	r8, r0, 100
	bne	r17, r8, fail4

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
