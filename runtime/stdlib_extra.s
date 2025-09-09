	.file	"stdlib_extra.c"
	.text
	.globl	qsort                           # -- Begin function qsort
	.p2align	2
	.type	qsort,@function
qsort:                                  # @qsort
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	addi r1, r0, 0
	beq r3, r1, .LBB0_5
.LBB0_1:
	beq r4, r1, .LBB0_5
	beq r4, r1, .LBB0_5
.LBB0_2:
	beq r5, r1, .LBB0_5
	beq r5, r1, .LBB0_5
.LBB0_3:
	beq r6, r1, .LBB0_5
	beq r6, r1, .LBB0_5
.LBB0_4:
	jal r31, qsort_recursive
.LBB0_5:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	qsort, .Lfunc_end0-qsort
                                        # -- End function
	.p2align	2                               # -- Begin function qsort_recursive
	.type	qsort_recursive,@function
qsort_recursive:                        # @qsort_recursive
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
	stw fp+-52, lr
	addi r1, r0, 2
	sltu r1, r4, r1
	addi r15, r0, 0
	bne r1, r15, .LBB1_8
.LBB1_1:
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	add r14, r3, r0
	ori r16, r0, 0
	addi r17, r0, -1
	addi r18, r0, 1
.LBB1_2:
	addi r20, r13, -1
	mul r1, r20, r12
	add r19, r14, r1
	add r22, r15, r0
	add r21, r15, r0
.LBB1_3:
	jalr r31, r11, 0
	sgt r1, r1, r16
	beq r1, r15, .LBB1_9
.LBB1_4:
	jal r0, .LBB1_12
	jal r0, .LBB1_12
.LBB1_5:
	mul r1, r21, r12
	add r14, r14, r1
	add r1, r15, r0
.LBB1_6:
	add r2, r19, r1
	add r3, r14, r1
	ldbu r4, r3+0
	ldbu r5, r2+0
	stb r3+0, r5
	stb r2+0, r4
	addi r1, r1, 1
	bne r12, r1, .LBB1_6
.LBB1_7:
	jal r31, __udivsi3
	xor r1, r1, r17
	add r13, r13, r1
	jal r31, qsort_recursive
	add r14, r14, r12
	sgtu r1, r13, r18
	bne r1, r15, .LBB1_2
.LBB1_8:
	ldw lr, fp+-52
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
.LBB1_9:
	mul r1, r22, r12
	add r1, r14, r1
	mul r2, r21, r12
	add r3, r14, r2
	add r2, r12, r0
.LBB1_10:
	addi r2, r2, -1
	ldbu r4, r3+0
	ldbu r5, r1+0
	addi r6, r3, 1
	stb r3+0, r5
	addi r3, r1, 1
	stb r1+0, r4
	add r1, r3, r0
	add r3, r6, r0
	bne r2, r15, .LBB1_10
.LBB1_11:
	addi r21, r21, 1
.LBB1_12:
	addi r22, r22, 1
	beq r22, r20, .LBB1_5
	jal r0, .LBB1_3
.Lfunc_end1:
	.size	qsort_recursive, .Lfunc_end1-qsort_recursive
                                        # -- End function
	.globl	bsearch                         # -- Begin function bsearch
	.p2align	2
	.type	bsearch,@function
bsearch:                                # @bsearch
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 56
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, lr
	addi r1, r0, 0
	bne r4, r1, .LBB2_2
.LBB2_1:
	jal r0, .LBB2_15
	jal r0, .LBB2_15
.LBB2_2:
	add r14, r5, r0
	addi r15, r0, 0
	bne r5, r15, .LBB2_4
.LBB2_3:
	jal r0, .LBB2_15
	jal r0, .LBB2_15
.LBB2_4:
	add r11, r6, r0
	beq r6, r15, .LBB2_5
	bne r6, r15, .LBB2_6
.LBB2_5:
	jal r0, .LBB2_15
	jal r0, .LBB2_15
.LBB2_6:
	add r12, r7, r0
	beq r7, r15, .LBB2_15
.LBB2_7:
	add r13, r4, r0
	ori r16, r0, 0
	add r17, r15, r0
.LBB2_8:
	sub r1, r14, r17
	srli r1, r1, 1
	add r18, r1, r17
	jalr r31, r12, 0
	slt r2, r1, r16
	beq r2, r15, .LBB2_10
.LBB2_9:
	add r14, r18, r0
	jal r0, .LBB2_13
.LBB2_10:
	bne r1, r15, .LBB2_12
.LBB2_11:
	mul r1, r18, r11
	add r1, r13, r1
	jal r0, .LBB2_15
.LBB2_12:
	addi r17, r18, 1
.LBB2_13:
	sltu r1, r17, r14
	bne r1, r15, .LBB2_8
.LBB2_14:
	add r1, r15, r0
.LBB2_15:
	ldw lr, fp+-36
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
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end2:
	.size	bsearch, .Lfunc_end2-bsearch
                                        # -- End function
	.globl	strtol                          # -- Begin function strtol
	.p2align	2
	.type	strtol,@function
strtol:                                 # @strtol
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, r11
	addi r2, r0, 5
	addi r1, r0, 0
	addi r6, r0, 32
.LBB3_1:
	ldbu r7, r3+0
	addi r8, r7, -9
	sltu r8, r8, r2
	bne r8, r1, .LBB3_37
.LBB3_2:
	beq r7, r6, .LBB3_37
	beq r7, r6, .LBB3_37
.LBB3_3:
	addi r2, r0, 43
	beq r7, r2, .LBB3_6
.LBB3_4:
	addi r2, r0, 45
	bne r7, r2, .LBB3_7
.LBB3_5:
	addi r3, r3, 1
	addi r2, r0, -1
	jal r0, .LBB3_8
.LBB3_6:
	addi r3, r3, 1
	addi r2, r0, 1
	jal r0, .LBB3_8
.LBB3_7:
	addi r2, r0, 1
.LBB3_8:
	addi r6, r0, 16
	beq r5, r6, .LBB3_16
.LBB3_9:
	bne r5, r1, .LBB3_10
	beq r5, r1, .LBB3_11
.LBB3_10:
	add r6, r5, r0
	jal r0, .LBB3_21
.LBB3_11:
	ldbu r5, r3+0
	addi r7, r0, 48
	beq r5, r7, .LBB3_13
.LBB3_12:
	addi r6, r0, 10
	jal r0, .LBB3_22
.LBB3_13:
	ldbu r5, r3+1
	ori  r7, r5, 32
	addi r8, r0, 120
	beq r7, r8, .LBB3_15
.LBB3_14:
	addi r3, r3, 1
	addi r6, r0, 8
	jal r0, .LBB3_22
.LBB3_15:
	addi r3, r3, 2
	jal r0, .LBB3_21
.LBB3_16:
	ldbu r5, r3+0
	addi r7, r0, 48
	beq r5, r7, .LBB3_18
.LBB3_17:
	addi r6, r0, 16
	jal r0, .LBB3_22
.LBB3_18:
	ldbu r5, r3+1
	ori  r5, r5, 32
	addi r6, r0, 120
	beq r5, r6, .LBB3_20
.LBB3_19:
	addi r6, r0, 16
	add r5, r7, r0
	jal r0, .LBB3_23
.LBB3_20:
	addi r3, r3, 2
	addi r6, r0, 16
.LBB3_21:
	ldbu r5, r3+0
.LBB3_22:
	andi r8, r5, 255
	addi r7, r0, 0
	beq r8, r7, .LBB3_34
.LBB3_23:
	addi r8, r0, 9
	addi r9, r0, 25
	add r7, r1, r0
	add r10, r3, r0
.LBB3_24:
	addi r3, r5, -48
	andi r3, r3, 255
	sgtu r11, r3, r8
	bne r11, r1, .LBB3_26
.LBB3_25:
	jal r0, .LBB3_31
	jal r0, .LBB3_31
.LBB3_26:
	addi r3, r5, -97
	andi r3, r3, 255
	sgtu r3, r3, r9
	bne r3, r1, .LBB3_28
.LBB3_27:
	andi r3, r5, 255
	addi r3, r3, -87
	jal r0, .LBB3_31
.LBB3_28:
	addi r3, r5, -65
	andi r3, r3, 255
	sgtu r3, r3, r9
	beq r3, r1, .LBB3_30
.LBB3_29:
	add r3, r10, r0
	jal r0, .LBB3_34
.LBB3_30:
	andi r3, r5, 255
	addi r3, r3, -55
.LBB3_31:
	slt r5, r3, r6
	bne r5, r1, .LBB3_33
.LBB3_32:
	add r3, r10, r0
	jal r0, .LBB3_34
.LBB3_33:
	mul r5, r7, r6
	add r7, r3, r5
	addi r3, r10, 1
	ldbu r5, r10+1
	add r10, r3, r0
	bne r5, r1, .LBB3_24
.LBB3_34:
	addi r1, r0, 0
	beq r4, r1, .LBB3_36
.LBB3_35:
	stw r4+0, r3
.LBB3_36:
	mul r1, r7, r2
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB3_37:
	addi r3, r3, 1
	jal r0, .LBB3_1
.Lfunc_end3:
	.size	strtol, .Lfunc_end3-strtol
                                        # -- End function
	.globl	strtoul                         # -- Begin function strtoul
	.p2align	2
	.type	strtoul,@function
strtoul:                                # @strtoul
# %bb.0:
	addi r1, r0, 5
	addi r2, r0, 0
	addi r6, r0, 32
.LBB4_1:
	ldbu r7, r3+0
	addi r8, r7, -9
	sltu r8, r8, r1
	bne r8, r2, .LBB4_3
.LBB4_2:
	bne r7, r6, .LBB4_4
	bne r7, r6, .LBB4_4
.LBB4_3:
	addi r3, r3, 1
	jal r0, .LBB4_1
.LBB4_4:
	ori r1, r0, 43
	seq r1, r7, r1
	add r6, r3, r1
	beq r5, r2, .LBB4_7
.LBB4_5:
	addi r8, r0, 16
	beq r5, r8, .LBB4_12
.LBB4_6:
	jal r0, .LBB4_17
	jal r0, .LBB4_17
.LBB4_7:
	ldbu r7, r6+0
	addi r5, r0, 48
	beq r7, r5, .LBB4_9
.LBB4_8:
	addi r5, r0, 10
	jal r0, .LBB4_18
.LBB4_9:
	add r1, r3, r1
	ldbu r7, r1+1
	ori  r3, r7, 32
	addi r5, r0, 120
	beq r3, r5, .LBB4_11
.LBB4_10:
	addi r6, r1, 1
	addi r5, r0, 8
	jal r0, .LBB4_18
.LBB4_11:
	addi r6, r1, 2
	addi r5, r0, 16
	jal r0, .LBB4_17
.LBB4_12:
	ldbu r5, r6+0
	addi r7, r0, 48
	beq r5, r7, .LBB4_14
.LBB4_13:
	add r7, r5, r0
	add r5, r8, r0
	jal r0, .LBB4_18
.LBB4_14:
	add r1, r3, r1
	ldbu r3, r1+1
	ori  r3, r3, 32
	addi r5, r0, 120
	beq r3, r5, .LBB4_16
.LBB4_15:
	addi r5, r0, 16
	jal r0, .LBB4_19
.LBB4_16:
	addi r6, r1, 2
	addi r5, r0, 16
.LBB4_17:
	ldbu r7, r6+0
.LBB4_18:
	andi r3, r7, 255
	addi r1, r0, 0
	beq r3, r1, .LBB4_30
.LBB4_19:
	addi r3, r0, 9
	addi r8, r0, 25
	add r1, r2, r0
	add r9, r6, r0
.LBB4_20:
	addi r6, r7, -48
	andi r6, r6, 255
	sgtu r10, r6, r3
	bne r10, r2, .LBB4_22
.LBB4_21:
	jal r0, .LBB4_27
	jal r0, .LBB4_27
.LBB4_22:
	addi r6, r7, -97
	andi r6, r6, 255
	sgtu r6, r6, r8
	bne r6, r2, .LBB4_24
.LBB4_23:
	andi r6, r7, 255
	addi r6, r6, -87
	jal r0, .LBB4_27
.LBB4_24:
	addi r6, r7, -65
	andi r6, r6, 255
	sgtu r6, r6, r8
	beq r6, r2, .LBB4_26
.LBB4_25:
	add r6, r9, r0
	jal r0, .LBB4_30
.LBB4_26:
	andi r6, r7, 255
	addi r6, r6, -55
.LBB4_27:
	slt r7, r6, r5
	bne r7, r2, .LBB4_29
.LBB4_28:
	add r6, r9, r0
	jal r0, .LBB4_30
.LBB4_29:
	mul r1, r1, r5
	add r1, r6, r1
	addi r6, r9, 1
	ldbu r7, r9+1
	add r9, r6, r0
	bne r7, r2, .LBB4_20
.LBB4_30:
	addi r2, r0, 0
	beq r4, r2, .LBB4_32
.LBB4_31:
	stw r4+0, r6
.LBB4_32:
	jalr r0, r31, 0
.Lfunc_end4:
	.size	strtoul, .Lfunc_end4-strtoul
                                        # -- End function
	.globl	div                             # -- Begin function div
	.p2align	2
	.type	div,@function
div:                                    # @div
# %bb.0:
	div r1, r4, r5
	stw r3+0, r1
	mul r1, r1, r5
	sub r1, r4, r1
	stw r3+4, r1
	jalr r0, r31, 0
.Lfunc_end5:
	.size	div, .Lfunc_end5-div
                                        # -- End function
	.globl	ldiv                            # -- Begin function ldiv
	.p2align	2
	.type	ldiv,@function
ldiv:                                   # @ldiv
# %bb.0:
	div r1, r4, r5
	stw r3+0, r1
	mul r1, r1, r5
	sub r1, r4, r1
	stw r3+4, r1
	jalr r0, r31, 0
.Lfunc_end6:
	.size	ldiv, .Lfunc_end6-ldiv
                                        # -- End function
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
