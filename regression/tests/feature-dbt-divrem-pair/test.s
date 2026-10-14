# Directed test for DBT DIV/REM pairing (tools/dbt/divrem.h).
#
# A DIV and a REM of the same two registers share one host divide: the
# first computes both halves and holds its partner's in a host register
# (R10 on x86-64, W12 on AArch64) until the partner, now a move.  This
# test proves the shared divide gives SLOW-32's answers in both orders
# and at both special cases (divisor 0: DIV -1, REM the dividend;
# INT32_MIN / -1: DIV INT32_MIN, REM 0), and that the pairing is
# refused or dropped where the held value would be stale: an operand
# written in between, the first's destination being an operand, and a
# partner at a loop head entered again by a back edge.  The lone DIV
# and REM by zero are here too: the AArch64 DBT once gave x / 0 = 0.
#
# Values are round-tripped through the stack so translate-time constant
# propagation cannot fold the divides away.
#
# Success output: "OK\n". On failure: "F<letter>\n".

	.text
	.globl main
	.p2align 2
main:
	# Opaque values: r5=47, r6=5, r7=-47, r9=0, r10=INT32_MIN, r11=-1
	addi	r1, r0, 47
	stw	sp+-4, r1
	addi	r1, r0, 5
	stw	sp+-8, r1
	addi	r1, r0, -47
	stw	sp+-12, r1
	stw	sp+-16, r0
	lui	r1, 0x80000
	stw	sp+-20, r1
	addi	r1, r0, -1
	stw	sp+-24, r1
	ldw	r5, -4(sp)
	ldw	r6, -8(sp)
	ldw	r7, -12(sp)
	ldw	r9, -16(sp)
	ldw	r10, -20(sp)
	ldw	r11, -24(sp)

	# --- A: REM then DIV, a store between: 47 % 5 = 2, 47 / 5 = 9 ---
	rem	r12, r5, r6
	stw	sp+-28, r12
	div	r13, r5, r6
	addi	r1, r0, 2
	bne	r12, r1, failA
	addi	r1, r0, 9
	bne	r13, r1, failA

	# --- B: DIV then REM, negative dividend: -47 / 5 = -9, % = -2 ---
	div	r12, r7, r6
	add	r20, r5, r6
	rem	r13, r7, r6
	addi	r1, r0, -9
	bne	r12, r1, failB
	addi	r1, r0, -2
	bne	r13, r1, failB

	# --- C: REM then DIV by zero: 47 % 0 = 47, 47 / 0 = -1 ---
	rem	r12, r5, r9
	div	r13, r5, r9
	bne	r12, r5, failC
	bne	r13, r11, failC

	# --- D: DIV then REM by zero: -47 / 0 = -1, -47 % 0 = -47 ---
	div	r12, r7, r9
	rem	r13, r7, r9
	bne	r12, r11, failD
	bne	r13, r7, failD

	# --- E: INT32_MIN / -1 = INT32_MIN, % = 0, both orders ---
	rem	r12, r10, r11
	div	r13, r10, r11
	bne	r12, r0, failE
	bne	r13, r10, failE
	div	r12, r10, r11
	rem	r13, r10, r11
	bne	r12, r10, failE
	bne	r13, r0, failE

	# --- F: an operand written between: no pairing.  47 % 5 = 2,
	#     then r6 = 6 and 47 / 6 = 7 (the held quotient would say 9) ---
	rem	r12, r5, r6
	addi	r6, r6, 1
	div	r13, r5, r6
	addi	r6, r6, -1
	addi	r1, r0, 2
	bne	r12, r1, failF
	addi	r1, r0, 7
	bne	r13, r1, failF

	# --- G: the first's destination is an operand: r20 = 47;
	#     r20 = 47 % 5 = 2; 2 / 5 = 0 (the held quotient would say 9) ---
	add	r20, r5, r0
	rem	r20, r20, r6
	div	r21, r20, r6
	addi	r1, r0, 2
	bne	r20, r1, failG
	bne	r21, r0, failG

	# --- H: lone DIV and REM by zero ---
	div	r12, r5, r9
	bne	r12, r11, failH
	rem	r13, r7, r9
	bne	r13, r7, failH

	# --- I: the partner at a loop head.  Before the loop, REM r16 % r6
	#     could hold a quotient for the DIV at the head; the back edge
	#     enters the head again with r16 changed.  r16 = 47, 57, 67:
	#     quotients 9, 11, 13, summed 33 ---
	add	r16, r5, r0
	addi	r17, r0, 3
	add	r18, r0, r0
	rem	r12, r16, r6
loopI:
	div	r13, r16, r6
	add	r18, r18, r13
	addi	r16, r16, 10
	addi	r17, r17, -1
	bne	r17, r0, loopI
	addi	r1, r0, 33
	bne	r18, r1, failI

	# --- J: pairs in a loop body (the pi spigot's shape): for i = 1..20,
	#     x = 10*i + 7, r = x % (2i+1), q = x / (2i+1); sum r + 3q ---
	addi	r17, r0, 1
	add	r18, r0, r0
loopJ:
	addi	r1, r0, 10
	mul	r19, r17, r1
	addi	r19, r19, 7
	slli	r20, r17, 1
	addi	r20, r20, 1
	rem	r21, r19, r20
	stw	sp+-28, r21
	div	r22, r19, r20
	add	r18, r18, r21
	add	r18, r18, r22
	add	r18, r18, r22
	add	r18, r18, r22
	addi	r17, r17, 1
	addi	r1, r0, 21
	bne	r17, r1, loopJ
	addi	r1, r0, 340
	bne	r18, r1, failJ

	addi	r28, r0, 0x4F		# 'O'
	debug	r28
	addi	r28, r0, 0x4B		# 'K'
	debug	r28
	addi	r28, r0, 10
	debug	r28
	addi	r1, r0, 0
	halt

failA:
	addi	r27, r0, 0x41
	beq	r0, r0, failout
failB:
	addi	r27, r0, 0x42
	beq	r0, r0, failout
failC:
	addi	r27, r0, 0x43
	beq	r0, r0, failout
failD:
	addi	r27, r0, 0x44
	beq	r0, r0, failout
failE:
	addi	r27, r0, 0x45
	beq	r0, r0, failout
failF:
	addi	r27, r0, 0x46
	beq	r0, r0, failout
failG:
	addi	r27, r0, 0x47
	beq	r0, r0, failout
failH:
	addi	r27, r0, 0x48
	beq	r0, r0, failout
failI:
	addi	r27, r0, 0x49
	beq	r0, r0, failout
failJ:
	addi	r27, r0, 0x4A
failout:
	addi	r28, r0, 0x46		# 'F'
	debug	r28
	debug	r27
	addi	r28, r0, 10
	debug	r28
	addi	r1, r0, 1
	halt
