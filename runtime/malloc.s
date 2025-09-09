	.file	"malloc.c"
	.text
	.globl	malloc                          # -- Begin function malloc
	.p2align	2
	.type	malloc,@function
malloc:                                 # @malloc
# %bb.0:
	addi r1, r0, 0
	beq r3, r1, .LBB0_10
.LBB0_1:
	ori  r1, r0, %lo(heap_head)
	lui r2, %hi(heap_head)
	add r4, r2, r1
	ldw r2, r4+0
	addi r1, r0, 0
	bne r2, r1, .LBB0_3
.LBB0_2:
	ori  r2, r0, %lo(__heap_start)
	lui r5, %hi(__heap_start)
	add r2, r5, r2
	stw r4+0, r2
	ori  r4, r0, %lo(__heap_end)
	lui r5, %hi(__heap_end)
	add r4, r5, r4
	sub r4, r4, r2
	stw r2+0, r4
	ori  r4, r0, %lo(__heap_start)
	lui r5, %hi(__heap_start)
	add r4, r5, r4
	addi r5, r0, 0
	stw r4+0, r5
	ori  r4, r0, %lo(__heap_start)
	lui r5, %hi(__heap_start)
	add r4, r5, r4
	addi r5, r0, 1
	stw r4+0, r5
.LBB0_3:
	addi r3, r3, 7
	addi r4, r0, -8
	and r3, r3, r4
.LBB0_4:
	ldw r4, r2+8
	beq r4, r1, .LBB0_9
.LBB0_5:
	ldw r4, r2+0
	sltu r5, r4, r3
	bne r5, r1, .LBB0_9
.LBB0_6:
	addi r1, r3, 20
	sleu r5, r4, r1
	addi r1, r0, 0
	bne r5, r1, .LBB0_8
.LBB0_7:
	add r5, r2, r3
	addi r6, r5, 12
	sub r4, r4, r3
	addi r4, r4, -12
	stw r5+12, r4
	ldw r4, r2+4
	stw r5+16, r4
	addi r4, r0, 1
	stw r5+20, r4
	stw r2+0, r3
	stw r2+4, r6
.LBB0_8:
	stw r2+8, r1
	addi r1, r2, 12
	jalr r0, r31, 0
.LBB0_9:
	ldw r2, r2+4
	bne r2, r1, .LBB0_4
.LBB0_10:
	jalr r0, r31, 0
.Lfunc_end0:
	.size	malloc, .Lfunc_end0-malloc
                                        # -- End function
	.globl	free                            # -- Begin function free
	.p2align	2
	.type	free,@function
free:                                   # @free
# %bb.0:
	addi r1, r0, 0
	beq r3, r1, .LBB1_9
.LBB1_1:
	addi r2, r0, 1
	stw r3+-4, r2
	ori  r2, r0, %lo(heap_head)
	lui r3, %hi(heap_head)
	add r2, r3, r2
	ldw r2, r2+0
	beq r2, r1, .LBB1_9
.LBB1_2:
	ldw r4, r2+8
	ldw r3, r2+4
	bne r4, r1, .LBB1_4
.LBB1_3:
	add r2, r3, r0
	jal r0, .LBB1_8
.LBB1_4:
	beq r3, r1, .LBB1_9
	beq r3, r1, .LBB1_9
.LBB1_5:
	ldw r4, r3+8
	bne r4, r1, .LBB1_7
.LBB1_6:
	add r2, r3, r0
	jal r0, .LBB1_8
.LBB1_7:
	ldw r4, r3+0
	ldw r5, r2+0
	add r4, r4, r5
	addi r4, r4, 12
	stw r2+0, r4
	ldw r3, r3+4
	stw r2+4, r3
.LBB1_8:
	bne r2, r1, .LBB1_2
	bne r2, r1, .LBB1_2
.LBB1_9:
	jalr r0, r31, 0
.Lfunc_end1:
	.size	free, .Lfunc_end1-free
                                        # -- End function
	.globl	calloc                          # -- Begin function calloc
	.p2align	2
	.type	calloc,@function
calloc:                                 # @calloc
# %bb.0:
	mul r2, r3, r4
	addi r1, r0, 0
	beq r3, r1, .LBB2_4
.LBB2_1:
	andi r5, r4, 65535
	srli r6, r3, 16
	mul r7, r6, r5
	srli r4, r4, 16
	andi r3, r3, 65535
	mul r8, r3, r4
	add r7, r8, r7
	mul r3, r3, r5
	srli r3, r3, 16
	add r3, r7, r3
	srli r3, r3, 16
	mul r4, r6, r4
	add r3, r4, r3
	ori r4, r0, 0
	sne r3, r3, r4
	beq r3, r1, .LBB2_3
.LBB2_2:
	jal r0, .LBB2_18
	jalr r0, r31, 0
.LBB2_3:
	addi r3, r0, 0
	bne r2, r3, .LBB2_5
	jal r0, .LBB2_18
.LBB2_4:
	addi r1, r0, 0
	beq r2, r1, .LBB2_18
.LBB2_5:
	ori  r1, r0, %lo(heap_head)
	lui r3, %hi(heap_head)
	add r4, r3, r1
	ldw r3, r4+0
	addi r1, r0, 0
	bne r3, r1, .LBB2_7
.LBB2_6:
	ori  r3, r0, %lo(__heap_start)
	lui r5, %hi(__heap_start)
	add r3, r5, r3
	stw r4+0, r3
	ori  r4, r0, %lo(__heap_end)
	lui r5, %hi(__heap_end)
	add r4, r5, r4
	sub r4, r4, r3
	stw r3+0, r4
	ori  r4, r0, %lo(__heap_start)
	lui r5, %hi(__heap_start)
	add r4, r5, r4
	addi r5, r0, 0
	stw r4+0, r5
	ori  r4, r0, %lo(__heap_start)
	lui r5, %hi(__heap_start)
	add r4, r5, r4
	addi r5, r0, 1
	stw r4+0, r5
.LBB2_7:
	addi r4, r2, 7
	addi r5, r0, -8
	and r4, r4, r5
.LBB2_8:
	ldw r5, r3+8
	beq r5, r1, .LBB2_14
.LBB2_9:
	ldw r6, r3+0
	sltu r5, r6, r4
	bne r5, r1, .LBB2_14
.LBB2_10:
	addi r1, r4, 20
	sleu r1, r6, r1
	addi r5, r0, 0
	bne r1, r5, .LBB2_12
.LBB2_11:
	add r1, r3, r4
	addi r7, r1, 12
	sub r6, r6, r4
	addi r6, r6, -12
	stw r1+12, r6
	ldw r6, r3+4
	stw r1+16, r6
	addi r6, r0, 1
	stw r1+20, r6
	stw r3+0, r4
	stw r3+4, r7
.LBB2_12:
	stw r3+8, r5
	addi r1, r3, 12
	bne r2, r5, .LBB2_16
.LBB2_13:
	jal r0, .LBB2_18
	jalr r0, r31, 0
.LBB2_14:
	ldw r3, r3+4
	bne r3, r1, .LBB2_8
.LBB2_15:
	jal r0, .LBB2_18
	jalr r0, r31, 0
.LBB2_16:
	add r3, r5, r0
.LBB2_17:
	add r4, r1, r3
	stb r4+0, r5
	addi r3, r3, 1
	sltu r4, r3, r2
	bne r4, r5, .LBB2_17
.LBB2_18:
	jalr r0, r31, 0
.Lfunc_end2:
	.size	calloc, .Lfunc_end2-calloc
                                        # -- End function
	.globl	realloc                         # -- Begin function realloc
	.p2align	2
	.type	realloc,@function
realloc:                                # @realloc
# %bb.0:
	addi r1, r0, 0
	bne r3, r1, .LBB3_12
.LBB3_1:
	addi r1, r0, 0
	beq r4, r1, .LBB3_51
.LBB3_2:
	ori  r1, r0, %lo(heap_head)
	lui r2, %hi(heap_head)
	add r3, r2, r1
	ldw r2, r3+0
	addi r1, r0, 0
	bne r2, r1, .LBB3_4
.LBB3_3:
	ori  r2, r0, %lo(__heap_start)
	lui r5, %hi(__heap_start)
	add r2, r5, r2
	stw r3+0, r2
	ori  r3, r0, %lo(__heap_end)
	lui r5, %hi(__heap_end)
	add r3, r5, r3
	sub r3, r3, r2
	stw r2+0, r3
	ori  r3, r0, %lo(__heap_start)
	lui r5, %hi(__heap_start)
	add r3, r5, r3
	addi r5, r0, 0
	stw r3+0, r5
	ori  r3, r0, %lo(__heap_start)
	lui r5, %hi(__heap_start)
	add r3, r5, r3
	addi r5, r0, 1
	stw r3+0, r5
.LBB3_4:
	addi r3, r4, 7
	addi r4, r0, -8
	and r3, r3, r4
.LBB3_5:
	ldw r4, r2+8
	beq r4, r1, .LBB3_10
.LBB3_6:
	ldw r4, r2+0
	sltu r5, r4, r3
	bne r5, r1, .LBB3_10
.LBB3_7:
	addi r1, r3, 20
	sleu r5, r4, r1
	addi r1, r0, 0
	bne r5, r1, .LBB3_9
.LBB3_8:
	add r5, r2, r3
	addi r6, r5, 12
	sub r4, r4, r3
	addi r4, r4, -12
	stw r5+12, r4
	ldw r4, r2+4
	stw r5+16, r4
	addi r4, r0, 1
	stw r5+20, r4
	stw r2+0, r3
	stw r2+4, r6
.LBB3_9:
	stw r2+8, r1
	addi r1, r2, 12
	jalr r0, r31, 0
.LBB3_10:
	ldw r2, r2+4
	bne r2, r1, .LBB3_5
.LBB3_11:
	jal r0, .LBB3_51
	jalr r0, r31, 0
.LBB3_12:
	bne r4, r1, .LBB3_24
	bne r4, r1, .LBB3_24
.LBB3_13:
	addi r1, r0, 1
	stw r3+-4, r1
	ori  r1, r0, %lo(heap_head)
	lui r2, %hi(heap_head)
	add r1, r2, r1
	ldw r2, r1+0
	addi r1, r0, 0
	beq r2, r1, .LBB3_51
.LBB3_14:
	addi r1, r0, 0
.LBB3_15:
	ldw r4, r2+8
	ldw r3, r2+4
	bne r4, r1, .LBB3_17
.LBB3_16:
	add r2, r3, r0
	jal r0, .LBB3_22
.LBB3_17:
	beq r3, r1, .LBB3_18
	bne r3, r1, .LBB3_19
.LBB3_18:
	jal r0, .LBB3_51
	jalr r0, r31, 0
.LBB3_19:
	ldw r4, r3+8
	bne r4, r1, .LBB3_21
.LBB3_20:
	add r2, r3, r0
	jal r0, .LBB3_22
.LBB3_21:
	ldw r4, r3+0
	ldw r5, r2+0
	add r4, r4, r5
	addi r4, r4, 12
	stw r2+0, r4
	ldw r3, r3+4
	stw r2+4, r3
.LBB3_22:
	bne r2, r1, .LBB3_15
	bne r2, r1, .LBB3_15
.LBB3_23:
	jal r0, .LBB3_51
	jalr r0, r31, 0
.LBB3_24:
	ldw r2, r3+-12
	sgeu r2, r2, r4
	beq r2, r1, .LBB3_26
.LBB3_25:
	add r1, r3, r0
	jalr r0, r31, 0
.LBB3_26:
	ori  r2, r0, %lo(heap_head)
	lui r5, %hi(heap_head)
	add r5, r5, r2
	ldw r2, r5+0
	bne r2, r1, .LBB3_28
.LBB3_27:
	ori  r2, r0, %lo(__heap_start)
	lui r6, %hi(__heap_start)
	add r2, r6, r2
	stw r5+0, r2
	ori  r6, r0, %lo(__heap_end)
	lui r7, %hi(__heap_end)
	add r6, r7, r6
	sub r6, r6, r2
	stw r2+0, r6
	ori  r6, r0, %lo(__heap_start)
	lui r7, %hi(__heap_start)
	add r6, r7, r6
	addi r7, r0, 0
	stw r6+0, r7
	ori  r6, r0, %lo(__heap_start)
	lui r7, %hi(__heap_start)
	add r6, r7, r6
	addi r7, r0, 1
	stw r6+0, r7
.LBB3_28:
	addi r4, r4, 7
	addi r6, r0, -8
	and r6, r4, r6
	add r4, r2, r0
.LBB3_29:
	ldw r7, r4+8
	beq r7, r1, .LBB3_36
.LBB3_30:
	ldw r8, r4+0
	sltu r7, r8, r6
	bne r7, r1, .LBB3_36
.LBB3_31:
	addi r1, r6, 20
	sgtu r1, r8, r1
	addi r7, r0, 0
	bne r1, r7, .LBB3_38
.LBB3_32:
	stw r4+8, r7
	addi r1, r4, 12
	ldw r4, r3+-12
	beq r4, r7, .LBB3_35
.LBB3_33:
	add r5, r7, r0
.LBB3_34:
	add r6, r3, r5
	ldbu r6, r6+0
	add r8, r1, r5
	stb r8+0, r6
	addi r5, r5, 1
	sltu r6, r5, r4
	bne r6, r7, .LBB3_34
.LBB3_35:
	addi r4, r0, 1
	stw r3+-4, r4
	jal r0, .LBB3_42
.LBB3_36:
	ldw r4, r4+4
	bne r4, r1, .LBB3_29
.LBB3_37:
	jal r0, .LBB3_51
	jalr r0, r31, 0
.LBB3_38:
	addi r1, r4, 12
	add r2, r1, r6
	sub r7, r8, r6
	addi r7, r7, -12
	stw r2+0, r7
	ldw r7, r4+4
	stw r2+4, r7
	addi r7, r0, 1
	stw r2+8, r7
	stw r4+0, r6
	stw r4+4, r2
	ldw r2, r5+0
	addi r5, r0, 0
	stw r4+8, r5
	ldw r4, r3+-12
	beq r4, r5, .LBB3_41
.LBB3_39:
	add r6, r5, r0
.LBB3_40:
	add r8, r3, r6
	ldbu r8, r8+0
	add r9, r1, r6
	stb r9+0, r8
	addi r6, r6, 1
	sltu r8, r6, r4
	bne r8, r5, .LBB3_40
.LBB3_41:
	stw r3+-4, r7
	beq r2, r5, .LBB3_51
.LBB3_42:
	addi r3, r0, 0
.LBB3_43:
	ldw r5, r2+8
	ldw r4, r2+4
	bne r5, r3, .LBB3_45
.LBB3_44:
	add r2, r4, r0
	jal r0, .LBB3_50
.LBB3_45:
	beq r4, r3, .LBB3_46
	bne r4, r3, .LBB3_47
.LBB3_46:
	jal r0, .LBB3_51
	jalr r0, r31, 0
.LBB3_47:
	ldw r5, r4+8
	bne r5, r3, .LBB3_49
.LBB3_48:
	add r2, r4, r0
	jal r0, .LBB3_50
.LBB3_49:
	ldw r5, r4+0
	ldw r6, r2+0
	add r5, r5, r6
	addi r5, r5, 12
	stw r2+0, r5
	ldw r4, r4+4
	stw r2+4, r4
.LBB3_50:
	bne r2, r3, .LBB3_43
	bne r2, r3, .LBB3_43
.LBB3_51:
	jalr r0, r31, 0
.Lfunc_end3:
	.size	realloc, .Lfunc_end3-realloc
                                        # -- End function
	.type	heap_head,@object               # @heap_head
	.local	heap_head
	.comm	heap_head,4,4
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
