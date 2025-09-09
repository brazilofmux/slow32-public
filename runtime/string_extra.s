	.file	"string_extra.c"
	.text
	.globl	strdup                          # -- Begin function strdup
	.p2align	2
	.type	strdup,@function
strdup:                                 # @strdup
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	addi r2, r0, 0
	beq r3, r2, .LBB0_7
.LBB0_1:
	add r11, r3, r0
	jal r31, strlen
	add r12, r1, r0
	jal r31, malloc
	addi r2, r0, 0
	beq r1, r2, .LBB0_7
.LBB0_2:
	addi r2, r12, 1
	addi r3, r0, 0
	bne r2, r3, .LBB0_4
.LBB0_3:
	add r2, r1, r0
	jal r0, .LBB0_7
.LBB0_4:
	add r4, r3, r0
.LBB0_5:
	add r5, r11, r4
	ldbu r5, r5+0
	add r6, r1, r4
	stb r6+0, r5
	addi r4, r4, 1
	sltu r5, r4, r2
	bne r5, r3, .LBB0_5
.LBB0_6:
	add r2, r1, r0
.LBB0_7:
	add r1, r2, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	strdup, .Lfunc_end0-strdup
                                        # -- End function
	.globl	strnlen                         # -- Begin function strnlen
	.p2align	2
	.type	strnlen,@function
strnlen:                                # @strnlen
# %bb.0:
	addi r1, r0, 0
	beq r4, r1, .LBB1_6
.LBB1_1:
	addi r2, r0, 0
	add r1, r2, r0
.LBB1_2:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r2, .LBB1_4
.LBB1_3:
	jal r0, .LBB1_6
	jalr r0, r31, 0
.LBB1_4:
	addi r1, r1, 1
	bne r4, r1, .LBB1_2
.LBB1_5:
	add r1, r4, r0
.LBB1_6:
	jalr r0, r31, 0
.Lfunc_end1:
	.size	strnlen, .Lfunc_end1-strnlen
                                        # -- End function
	.globl	strpbrk                         # -- Begin function strpbrk
	.p2align	2
	.type	strpbrk,@function
strpbrk:                                # @strpbrk
# %bb.0:
	ldbu r6, r3+0
	addi r1, r0, 0
	beq r6, r1, .LBB2_8
.LBB2_1:
	ldbu r2, r4+0
	addi r4, r4, 1
	andi r5, r2, 255
	addi r1, r0, 0
.LBB2_2:
	beq r5, r1, .LBB2_7
	beq r5, r1, .LBB2_7
.LBB2_3:
	andi r6, r6, 255
	add r7, r4, r0
	add r8, r2, r0
	jal r0, .LBB2_5
.LBB2_4:
	ldbu r8, r7+0
	addi r7, r7, 1
	beq r8, r1, .LBB2_7
.LBB2_5:
	andi r8, r8, 255
	bne r6, r8, .LBB2_4
.LBB2_6:
	add r1, r3, r0
	jalr r0, r31, 0
.LBB2_7:
	addi r7, r3, 1
	ldbu r6, r3+1
	add r3, r7, r0
	bne r6, r1, .LBB2_2
.LBB2_8:
	jalr r0, r31, 0
.Lfunc_end2:
	.size	strpbrk, .Lfunc_end2-strpbrk
                                        # -- End function
	.globl	strspn                          # -- Begin function strspn
	.p2align	2
	.type	strspn,@function
strspn:                                 # @strspn
# %bb.0:
	ldbu r7, r3+0
	addi r1, r0, 0
	beq r7, r1, .LBB3_9
.LBB3_1:
	ldbu r2, r4+0
	addi r5, r4, 1
	addi r4, r0, 0
	andi r6, r2, 255
	add r1, r4, r0
.LBB3_2:
	bne r6, r4, .LBB3_6
.LBB3_3:
	add r1, r4, r0
	jalr r0, r31, 0
.LBB3_4:
	ldbu r9, r8+0
	addi r8, r8, 1
	bne r9, r4, .LBB3_7
.LBB3_5:
	jal r0, .LBB3_9
	jalr r0, r31, 0
.LBB3_6:
	andi r7, r7, 255
	add r8, r5, r0
	add r9, r2, r0
.LBB3_7:
	andi r9, r9, 255
	bne r7, r9, .LBB3_4
.LBB3_8:
	addi r1, r1, 1
	addi r8, r3, 1
	ldbu r7, r3+1
	add r3, r8, r0
	bne r7, r4, .LBB3_2
.LBB3_9:
	jalr r0, r31, 0
.Lfunc_end3:
	.size	strspn, .Lfunc_end3-strspn
                                        # -- End function
	.globl	strcspn                         # -- Begin function strcspn
	.p2align	2
	.type	strcspn,@function
strcspn:                                # @strcspn
# %bb.0:
	ldbu r7, r3+0
	addi r1, r0, 0
	beq r7, r1, .LBB4_8
.LBB4_1:
	ldbu r2, r4+0
	addi r4, r4, 1
	addi r5, r0, 0
	andi r6, r2, 255
	add r1, r5, r0
.LBB4_2:
	beq r6, r5, .LBB4_7
	beq r6, r5, .LBB4_7
.LBB4_3:
	andi r7, r7, 255
	add r8, r4, r0
	add r9, r2, r0
	jal r0, .LBB4_5
.LBB4_4:
	ldbu r9, r8+0
	addi r8, r8, 1
	beq r9, r5, .LBB4_7
.LBB4_5:
	andi r9, r9, 255
	bne r7, r9, .LBB4_4
.LBB4_6:
	jal r0, .LBB4_8
	jalr r0, r31, 0
.LBB4_7:
	addi r1, r1, 1
	addi r8, r3, 1
	ldbu r7, r3+1
	add r3, r8, r0
	bne r7, r5, .LBB4_2
.LBB4_8:
	jalr r0, r31, 0
.Lfunc_end4:
	.size	strcspn, .Lfunc_end4-strcspn
                                        # -- End function
	.globl	strtok                          # -- Begin function strtok
	.p2align	2
	.type	strtok,@function
strtok:                                 # @strtok
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, r11
	addi r2, r0, 0
	bne r3, r2, .LBB5_2
.LBB5_1:
	ori  r1, r0, %lo(strtok_save)
	lui r3, %hi(strtok_save)
	add r1, r3, r1
	ldw r3, r1+0
	addi r1, r0, 0
	beq r3, r1, .LBB5_20
.LBB5_2:
	ldbu r8, r3+0
	add r1, r2, r0
	beq r8, r2, .LBB5_10
.LBB5_3:
	ldbu r5, r4+0
	addi r1, r0, 0
	beq r5, r1, .LBB5_10
.LBB5_4:
	addi r6, r4, 1
	addi r1, r0, 0
	addi r7, r0, 0
	add r9, r3, r0
.LBB5_5:
	andi r8, r8, 255
	add r10, r6, r0
	add r11, r5, r0
.LBB5_6:
	andi r11, r11, 255
	beq r8, r11, .LBB5_9
.LBB5_7:
	ldbu r11, r10+0
	addi r10, r10, 1
	bne r11, r7, .LBB5_6
.LBB5_8:
	jal r0, .LBB5_10
	jal r0, .LBB5_10
.LBB5_9:
	addi r1, r1, 1
	addi r10, r9, 1
	ldbu r8, r9+1
	add r9, r10, r0
	bne r8, r7, .LBB5_5
.LBB5_10:
	add r1, r3, r1
	ldbu r8, r1+0
	bne r8, r2, .LBB5_12
.LBB5_11:
	ori  r1, r0, %lo(strtok_save)
	lui r2, %hi(strtok_save)
	add r2, r2, r1
	addi r1, r0, 0
	stw r2+0, r1
	jal r0, .LBB5_20
.LBB5_12:
	ldbu r3, r4+0
	addi r4, r4, 1
	andi r6, r3, 255
	addi r2, r0, 0
	add r5, r1, r0
.LBB5_13:
	addi r7, r5, 1
	beq r6, r2, .LBB5_18
.LBB5_14:
	andi r8, r8, 255
	add r9, r4, r0
	add r10, r3, r0
	jal r0, .LBB5_16
.LBB5_15:
	ldbu r10, r9+0
	addi r9, r9, 1
	beq r10, r2, .LBB5_18
.LBB5_16:
	andi r10, r10, 255
	bne r8, r10, .LBB5_15
.LBB5_17:
	ori  r3, r0, %lo(strtok_save)
	lui r4, %hi(strtok_save)
	add r3, r4, r3
	stw r3+0, r7
	stb r5+0, r2
	jal r0, .LBB5_20
.LBB5_18:
	ldbu r8, r5+1
	add r5, r7, r0
	bne r8, r2, .LBB5_13
.LBB5_19:
	ori  r3, r0, %lo(strtok_save)
	lui r4, %hi(strtok_save)
	add r3, r4, r3
	stw r3+0, r2
.LBB5_20:
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	strtok, .Lfunc_end5-strtok
                                        # -- End function
	.globl	strtok_r                        # -- Begin function strtok_r
	.p2align	2
	.type	strtok_r,@function
strtok_r:                               # @strtok_r
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	addi r2, r0, 0
	beq r3, r2, .LBB6_2
.LBB6_1:
	stw r5+0, r3
	jal r0, .LBB6_3
.LBB6_2:
	ldw r3, r5+0
	addi r1, r0, 0
	beq r3, r1, .LBB6_21
.LBB6_3:
	ldbu r9, r3+0
	add r1, r2, r0
	beq r9, r2, .LBB6_11
.LBB6_4:
	ldbu r6, r4+0
	addi r1, r0, 0
	beq r6, r1, .LBB6_11
.LBB6_5:
	addi r7, r4, 1
	addi r1, r0, 0
	addi r8, r0, 0
	add r10, r3, r0
.LBB6_6:
	andi r9, r9, 255
	add r11, r7, r0
	add r12, r6, r0
.LBB6_7:
	andi r12, r12, 255
	beq r9, r12, .LBB6_10
.LBB6_8:
	ldbu r12, r11+0
	addi r11, r11, 1
	bne r12, r8, .LBB6_7
.LBB6_9:
	jal r0, .LBB6_11
	jal r0, .LBB6_11
.LBB6_10:
	addi r1, r1, 1
	addi r11, r10, 1
	ldbu r9, r10+1
	add r10, r11, r0
	bne r9, r8, .LBB6_6
.LBB6_11:
	add r1, r3, r1
	stw r5+0, r1
	ldbu r9, r1+0
	bne r9, r2, .LBB6_13
.LBB6_12:
	addi r1, r0, 0
	stw r5+0, r1
	jal r0, .LBB6_21
.LBB6_13:
	ldbu r3, r4+0
	addi r4, r4, 1
	andi r6, r3, 255
	addi r2, r0, 0
	add r7, r1, r0
.LBB6_14:
	addi r8, r7, 1
	beq r6, r2, .LBB6_19
.LBB6_15:
	andi r9, r9, 255
	add r10, r4, r0
	add r11, r3, r0
	jal r0, .LBB6_17
.LBB6_16:
	ldbu r11, r10+0
	addi r10, r10, 1
	beq r11, r2, .LBB6_19
.LBB6_17:
	andi r11, r11, 255
	bne r9, r11, .LBB6_16
.LBB6_18:
	stw r5+0, r8
	stb r7+0, r2
	jal r0, .LBB6_21
.LBB6_19:
	ldbu r9, r7+1
	add r7, r8, r0
	bne r9, r2, .LBB6_14
.LBB6_20:
	stw r5+0, r2
.LBB6_21:
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	strtok_r, .Lfunc_end6-strtok_r
                                        # -- End function
	.globl	strcasecmp                      # -- Begin function strcasecmp
	.p2align	2
	.type	strcasecmp,@function
strcasecmp:                             # @strcasecmp
# %bb.0:
	addi r1, r0, 0
	addi r2, r0, 26
.LBB7_1:
	ldbu r7, r3+0
	slli r5, r7, 24
	srai r6, r5, 24
	ldb r5, r4+0
	beq r7, r1, .LBB7_6
.LBB7_2:
	beq r5, r1, .LBB7_6
	beq r5, r1, .LBB7_6
.LBB7_3:
	addi r7, r7, -65
	andi r7, r7, 255
	sltu r7, r7, r2
	ori  r8, r6, 32
	xor r8, r8, r6
	sub r7, r1, r7
	and r7, r8, r7
	xor r6, r6, r7
	addi r7, r5, -65
	andi r7, r7, 255
	sltu r7, r7, r2
	ori  r8, r5, 32
	xor r8, r8, r5
	sub r7, r1, r7
	and r7, r8, r7
	xor r5, r5, r7
	addi r3, r3, 1
	addi r4, r4, 1
	beq r6, r5, .LBB7_1
.LBB7_4:
	sub r1, r6, r5
.LBB7_5:
	jalr r0, r31, 0
.LBB7_6:
	sub r1, r6, r5
	jalr r0, r31, 0
.Lfunc_end7:
	.size	strcasecmp, .Lfunc_end7-strcasecmp
                                        # -- End function
	.globl	strncasecmp                     # -- Begin function strncasecmp
	.p2align	2
	.type	strncasecmp,@function
strncasecmp:                            # @strncasecmp
# %bb.0:
	addi r1, r0, 0
	addi r2, r0, 26
.LBB8_1:
	bne r5, r1, .LBB8_3
.LBB8_2:
	jal r0, .LBB8_7
	jalr r0, r31, 0
.LBB8_3:
	ldbu r8, r3+0
	slli r6, r8, 24
	srai r7, r6, 24
	ldb r6, r4+0
	beq r8, r1, .LBB8_8
.LBB8_4:
	beq r6, r1, .LBB8_8
	beq r6, r1, .LBB8_8
.LBB8_5:
	addi r8, r8, -65
	andi r8, r8, 255
	sltu r8, r8, r2
	ori  r9, r7, 32
	xor r9, r9, r7
	sub r8, r1, r8
	and r8, r9, r8
	xor r7, r7, r8
	addi r8, r6, -65
	andi r8, r8, 255
	sltu r8, r8, r2
	ori  r9, r6, 32
	xor r9, r9, r6
	sub r8, r1, r8
	and r8, r9, r8
	xor r6, r6, r8
	addi r3, r3, 1
	addi r4, r4, 1
	addi r5, r5, -1
	beq r7, r6, .LBB8_1
.LBB8_6:
	sub r1, r7, r6
.LBB8_7:
	jalr r0, r31, 0
.LBB8_8:
	sub r1, r7, r6
	jalr r0, r31, 0
.Lfunc_end8:
	.size	strncasecmp, .Lfunc_end8-strncasecmp
                                        # -- End function
	.type	strtok_save,@object             # @strtok_save
	.local	strtok_save
	.comm	strtok_save,4,4
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
