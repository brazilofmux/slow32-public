# A double or 64-bit operand is a register PAIR (rN, rN+1) and the
# instruction names only rN.  The DBT's block analyses read rd/rs1/rs2
# only, so they missed the hidden half (regression/fuzz/isafuzz.py,
# 2026-10-27).  Each case prints one word; all must print
#   00000001 00000800 00000000 80000001
	.text
	.globl main
	.p2align 2
main:
	lui r28, %hi(out)
	addi r28, r28, %lo(out)

	# A. r25 is written, read once, read again as FLE.D's rs2+1, and
	#    overwritten by that FLE.D.  The a64 dead-temporary scan saw one
	#    read before the overwrite and skipped the store, so FLE.D
	#    compared against a stale r25 (seed 2813).
	#    (A JAL first: a new block, so nothing is a known constant.)
	addi r2, r0, 0
	addi r3, r0, 0          # (r2, r3) = 0.0
	addi r24, r0, 0
	addi r25, r0, 0         # memory r25 = 0: (r24, r25) would be 0.0
	addi r5, r0, 1
	addi r6, r0, 0
	jal r0, .La
.La:
	sge r25, r5, r6         # 1: (r24, r25) = a tiny positive double
	add r11, r25, r6        # the one ordinary read
	flt.d r25, r2, r24      # 0.0 < tiny: 1, into r25 itself
	stw r28+0, r25

	# B. FABS.D writes r19 as rd+1 inside a hot loop; the loop-written
	#    set missed it, so the register cache kept r19's first value
	#    across the back-edge (seed 2717).
	addi r19, r0, 3
	addi r16, r0, 0
	addi r17, r0, 0
	addi r27, r0, 500
.Lb:
	ori r15, r19, 2048      # 0x803 once, then 0x800
	fabs.d r18, r16         # r19 = r17 & 0x7FFFFFFF = 0
	addi r27, r27, -1
	bne r27, r0, .Lb
	stw r28+4, r15

	# C. FCVT.LU.S writes r31 as rd+1 inside a hot loop (seed 14057).
	addi r31, r0, 5
	addi r23, r0, 3
	lui r26, 0x3f800        # 1.0f
	addi r27, r0, 500
.Lc:
	sgeu r20, r31, r23      # 5 >= 3 once, then 0 >= 3
	fcvt.lu.s r30, r26      # (r30, r31) = 1: r31 = 0
	addi r27, r27, -1
	bne r27, r0, .Lc
	stw r28+8, r20

	# D. FNEG.D reads r15 as rs1+1 between an ordinary read and an
	#    overwrite: the same skipped store (seed 22304).
	addi r14, r0, 7
	addi r15, r0, 0
	jal r0, .Ld
.Ld:
	seq r15, r14, r14       # 1
	sltiu r29, r15, 1       # the one ordinary read
	fneg.d r10, r14         # r11 = r15 ^ 0x80000000
	sltu r15, r0, r0        # overwrite
	stw r28+12, r11

	# print the four words in hex, one per line
	addi r2, r28, 0
	addi r3, r28, 16
	addi r8, r0, 39
.Lw:
	ldw r4, r2+0
	addi r5, r0, 8
.Ln:
	srli r6, r4, 28
	slli r4, r4, 4
	sltiu r7, r6, 10
	xori r7, r7, 1
	mul r7, r7, r8
	add r6, r6, r7
	addi r6, r6, 48
	debug r6
	addi r5, r5, -1
	bne r5, r0, .Ln
	addi r6, r0, 10
	debug r6
	addi r2, r2, 4
	bne r2, r3, .Lw
	addi r1, r0, 0
	halt
	.data
out:
	.space 16
