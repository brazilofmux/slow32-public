	.file	"convert_extra.c"
	.text
	.globl	strtoul                         # -- Begin function strtoul
	.p2align	2
	.type	strtoul,@function
strtoul:                                # @strtoul
# %bb.0:
	addi sp, sp, -72
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 72
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, r19
	stw fp+-40, r20
	stw fp+-44, r21
	stw fp+-48, r22
	stw fp+-52, r23
	stw fp+-56, r24
	stw fp+-60, r25
	stw fp+-64, lr
	add r13, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	addi r2, r3, -1
	addi r17, r0, 0
.LBB0_1:
	addi r15, r2, 1
	ldb r14, r2+1
	add r3, r14, r0
	jal r31, isspace
	add r2, r15, r0
	bne r1, r17, .LBB0_1
.LBB0_2:
	ori r1, r0, 45
	seq r19, r14, r1
	ori r1, r0, 43
	seq r1, r14, r1
	or  r2, r19, r1
	add r18, r15, r2
	ori  r3, r13, 16
	addi r1, r0, 16
	beq r3, r1, .LBB0_4
.LBB0_3:
	jal r0, .LBB0_9
	jal r0, .LBB0_9
.LBB0_4:
	ldbu r3, r18+0
	addi r4, r0, 48
	beq r3, r4, .LBB0_6
.LBB0_5:
	addi r1, r0, 10
	jal r0, .LBB0_8
.LBB0_6:
	add r2, r15, r2
	ldbu r3, r2+1
	ori  r3, r3, 32
	addi r4, r0, 120
	beq r3, r4, .LBB0_32
.LBB0_7:
	addi r1, r0, 8
.LBB0_8:
	ori r2, r0, 0
	seq r2, r13, r2
	xor r1, r1, r13
	addi r3, r0, 0
	sub r2, r3, r2
	and r1, r1, r2
	xor r13, r13, r1
.LBB0_9:
	addi r1, r13, -37
	addi r2, r0, -36
	sgtu r1, r1, r2
	bne r1, r17, .LBB0_11
.LBB0_10:
	addi r1, r0, 0
	bne r11, r1, .LBB0_30
	jal r0, .LBB0_31
.LBB0_11:
	addi r14, r0, -1
	add r3, r14, r0
	add r4, r13, r0
	jal r31, __udivsi3
	add r15, r1, r0
	ldbu r1, r18+0
	bne r1, r17, .LBB0_13
.LBB0_12:
	add r20, r17, r0
	add r21, r17, r0
	jal r0, .LBB0_27
.LBB0_13:
	mul r2, r15, r13
	xor r23, r2, r14
	addi r24, r0, 0
	addi r25, r0, 10
	addi r22, r0, 1
	add r21, r24, r0
	add r20, r24, r0
.LBB0_14:
	slli r1, r1, 24
	srai r16, r1, 24
	addi r1, r16, -48
	sltu r2, r1, r25
	bne r2, r24, .LBB0_18
.LBB0_15:
	add r3, r16, r0
	jal r31, isalpha
	bne r1, r24, .LBB0_17
.LBB0_16:
	jal r0, .LBB0_27
	jal r0, .LBB0_27
.LBB0_17:
	add r3, r16, r0
	jal r31, toupper
	addi r1, r1, -55
.LBB0_18:
	slt r2, r1, r13
	bne r2, r24, .LBB0_20
.LBB0_19:
	jal r0, .LBB0_27
	jal r0, .LBB0_27
.LBB0_20:
	sgtu r2, r20, r15
	beq r2, r24, .LBB0_22
.LBB0_21:
	add r20, r14, r0
	add r21, r14, r0
	jal r0, .LBB0_27
.LBB0_22:
	bne r20, r15, .LBB0_25
	bne r20, r15, .LBB0_25
.LBB0_23:
	slt r2, r23, r1
	beq r2, r24, .LBB0_25
.LBB0_24:
	jal r0, .LBB0_21
	jal r0, .LBB0_21
.LBB0_25:
	mul r2, r20, r13
	add r20, r1, r2
	addi r2, r18, 1
	ldbu r1, r18+1
	add r21, r22, r0
	add r18, r2, r0
	bne r1, r24, .LBB0_14
.LBB0_26:
	add r18, r2, r0
	add r21, r22, r0
.LBB0_27:
	sub r1, r17, r20
	sub r2, r17, r19
	xor r1, r1, r20
	and r1, r1, r2
	xor r1, r20, r1
	ori r2, r0, -1
	sgt r2, r21, r2
	xor r1, r1, r14
	sub r2, r17, r2
	and r1, r1, r2
	xor r1, r1, r14
	bne r11, r17, .LBB0_29
.LBB0_28:
	jal r0, .LBB0_31
	jal r0, .LBB0_31
.LBB0_29:
	ori r2, r0, 0
	seq r2, r21, r2
	xor r3, r12, r18
	addi r4, r0, 0
	sub r2, r4, r2
	and r2, r3, r2
	xor r12, r18, r2
.LBB0_30:
	stw r11+0, r12
.LBB0_31:
	ldw lr, fp+-64
	ldw r25, fp+-60
	ldw r24, fp+-56
	ldw r23, fp+-52
	ldw r22, fp+-48
	ldw r21, fp+-44
	ldw r20, fp+-40
	ldw r19, fp+-36
	ldw r18, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB0_32:
	addi r18, r2, 2
	add r13, r1, r0
	jal r0, .LBB0_9
.Lfunc_end0:
	.size	strtoul, .Lfunc_end0-strtoul
                                        # -- End function
	.globl	itoa                            # -- Begin function itoa
	.p2align	2
	.type	itoa,@function
itoa:                                   # @itoa
# %bb.0:
	add r1, r4, r0
	addi r2, r5, -37
	addi r4, r0, -36
	sgtu r4, r2, r4
	addi r2, r0, 0
	bne r4, r2, .LBB1_2
.LBB1_1:
	stb r1+0, r2
	jalr r0, r31, 0
.LBB1_2:
	ori r4, r0, -1
	sgt r4, r3, r4
	beq r4, r2, .LBB1_4
.LBB1_3:
	add r4, r1, r0
	jal r0, .LBB1_7
.LBB1_4:
	addi r4, r0, 10
	beq r5, r4, .LBB1_6
.LBB1_5:
	add r4, r1, r0
	jal r0, .LBB1_7
.LBB1_6:
	sub r3, r2, r3
	addi r4, r1, 1
	addi r6, r0, 45
	stb r1+0, r6
.LBB1_7:
	addi r8, r4, -1
	ori r7, r0, 10
.LBB1_8:
	addi r6, r8, 1
	div r9, r3, r5
	mul r10, r9, r5
	sub r3, r3, r10
	slt r10, r3, r7
	sub r10, r2, r10
	andi r10, r10, 7
	xori r10, r10, 55
	add r3, r10, r3
	stb r8+1, r3
	add r8, r6, r0
	add r3, r9, r0
	bne r9, r2, .LBB1_8
.LBB1_9:
	stb r6+1, r2
	sgeu r3, r4, r6
	bne r3, r2, .LBB1_12
.LBB1_10:
	addi r3, r4, 1
.LBB1_11:
	ldbu r4, r6+0
	ldbu r5, r3+-1
	addi r7, r6, -1
	stb r6+0, r5
	stb r3+-1, r4
	addi r4, r3, 1
	sltu r5, r3, r7
	add r3, r4, r0
	add r6, r7, r0
	bne r5, r2, .LBB1_11
.LBB1_12:
	jalr r0, r31, 0
.Lfunc_end1:
	.size	itoa, .Lfunc_end1-itoa
                                        # -- End function
	.globl	utoa                            # -- Begin function utoa
	.p2align	2
	.type	utoa,@function
utoa:                                   # @utoa
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, lr
	add r11, r4, r0
	addi r1, r5, -37
	addi r2, r0, -35
	sltu r1, r1, r2
	addi r14, r0, 0
	bne r1, r14, .LBB2_7
.LBB2_1:
	add r12, r5, r0
	add r13, r3, r0
	addi r17, r11, -1
	addi r15, r0, 10
.LBB2_2:
	addi r16, r17, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, __udivsi3
	mul r2, r1, r12
	sub r2, r13, r2
	sltu r3, r2, r15
	addi r4, r2, 55
	ori  r2, r2, 48
	xor r2, r2, r4
	sub r3, r14, r3
	and r2, r2, r3
	xor r2, r4, r2
	stb r17+1, r2
	sleu r2, r12, r13
	add r17, r16, r0
	add r13, r1, r0
	bne r2, r14, .LBB2_2
.LBB2_3:
	stb r16+1, r14
	sgeu r1, r11, r16
	bne r1, r14, .LBB2_6
.LBB2_4:
	addi r1, r16, -1
	add r2, r11, r0
.LBB2_5:
	ldbu r3, r1+1
	ldbu r4, r2+0
	stb r1+1, r4
	addi r4, r2, 1
	stb r2+0, r3
	addi r2, r1, -1
	sltu r3, r4, r1
	add r1, r2, r0
	add r2, r4, r0
	bne r3, r14, .LBB2_5
.LBB2_6:
	add r1, r11, r0
	ldw lr, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB2_7:
	stb r11+0, r14
	jal r0, .LBB2_6
.Lfunc_end2:
	.size	utoa, .Lfunc_end2-utoa
                                        # -- End function
	.globl	ltoa                            # -- Begin function ltoa
	.p2align	2
	.type	ltoa,@function
ltoa:                                   # @ltoa
# %bb.0:
	add r1, r4, r0
	addi r2, r5, -37
	addi r4, r0, -36
	sgtu r4, r2, r4
	addi r2, r0, 0
	bne r4, r2, .LBB3_2
.LBB3_1:
	stb r1+0, r2
	jalr r0, r31, 0
.LBB3_2:
	ori r4, r0, -1
	sgt r4, r3, r4
	beq r4, r2, .LBB3_4
.LBB3_3:
	add r4, r1, r0
	jal r0, .LBB3_7
.LBB3_4:
	addi r4, r0, 10
	beq r5, r4, .LBB3_6
.LBB3_5:
	add r4, r1, r0
	jal r0, .LBB3_7
.LBB3_6:
	sub r3, r2, r3
	addi r4, r1, 1
	addi r6, r0, 45
	stb r1+0, r6
.LBB3_7:
	addi r8, r4, -1
	ori r7, r0, 10
.LBB3_8:
	addi r6, r8, 1
	div r9, r3, r5
	mul r10, r9, r5
	sub r3, r3, r10
	slt r10, r3, r7
	sub r10, r2, r10
	andi r10, r10, 7
	xori r10, r10, 55
	add r3, r10, r3
	stb r8+1, r3
	add r8, r6, r0
	add r3, r9, r0
	bne r9, r2, .LBB3_8
.LBB3_9:
	stb r6+1, r2
	sgeu r3, r4, r6
	bne r3, r2, .LBB3_12
.LBB3_10:
	addi r3, r6, -1
.LBB3_11:
	ldbu r5, r3+1
	ldbu r6, r4+0
	stb r3+1, r6
	addi r6, r4, 1
	stb r4+0, r5
	addi r4, r3, -1
	sltu r5, r6, r3
	add r3, r4, r0
	add r4, r6, r0
	bne r5, r2, .LBB3_11
.LBB3_12:
	jalr r0, r31, 0
.Lfunc_end3:
	.size	ltoa, .Lfunc_end3-ltoa
                                        # -- End function
	.globl	ultoa                           # -- Begin function ultoa
	.p2align	2
	.type	ultoa,@function
ultoa:                                  # @ultoa
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, lr
	add r11, r4, r0
	addi r1, r5, -37
	addi r2, r0, -35
	sltu r1, r1, r2
	addi r14, r0, 0
	bne r1, r14, .LBB4_7
.LBB4_1:
	add r12, r5, r0
	add r13, r3, r0
	addi r17, r11, -1
	addi r15, r0, 10
.LBB4_2:
	addi r16, r17, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, __udivsi3
	mul r2, r1, r12
	sub r2, r13, r2
	sltu r3, r2, r15
	addi r4, r2, 55
	ori  r2, r2, 48
	xor r2, r2, r4
	sub r3, r14, r3
	and r2, r2, r3
	xor r2, r4, r2
	stb r17+1, r2
	sleu r2, r12, r13
	add r17, r16, r0
	add r13, r1, r0
	bne r2, r14, .LBB4_2
.LBB4_3:
	stb r16+1, r14
	sgeu r1, r11, r16
	bne r1, r14, .LBB4_6
.LBB4_4:
	addi r1, r16, -1
	add r2, r11, r0
.LBB4_5:
	ldbu r3, r1+1
	ldbu r4, r2+0
	stb r1+1, r4
	addi r4, r2, 1
	stb r2+0, r3
	addi r2, r1, -1
	sltu r3, r4, r1
	add r1, r2, r0
	add r2, r4, r0
	bne r3, r14, .LBB4_5
.LBB4_6:
	add r1, r11, r0
	ldw lr, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB4_7:
	stb r11+0, r14
	jal r0, .LBB4_6
.Lfunc_end4:
	.size	ultoa, .Lfunc_end4-ultoa
                                        # -- End function
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
