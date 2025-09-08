	.file	"malloc.c"
	.text
	.globl	malloc                          # -- Begin function malloc
	.p2align	2
	.type	malloc,@function
malloc:                                 # @malloc
# %bb.0:
	addi r1, r0, 0
	beq r3, r1, .LBB0_11
.LBB0_1:
	ori  r1, r0, %lo(heap_start)
	lui r2, %hi(heap_start)
	add r2, r2, r1
	ldw r4, r2+0
	addi r1, r0, 0
	beq r4, r1, .LBB0_2
	jal r0, .LBB0_3
.LBB0_2:
	lui r4, 256
	stw r2+0, r4
	addi r5, r4, 4084
	stw r4+0, r5
	addi r5, r4, 4
	addi r6, r0, 0
	stw r5+0, r6
	addi r4, r4, 8
	addi r5, r0, 1
	stw r4+0, r5
.LBB0_3:
	ldw r2, r2+0
	beq r2, r1, .LBB0_11
.LBB0_4:
	addi r1, r3, 7
	addi r3, r0, -8
	and r3, r1, r3
	addi r1, r0, 0
.LBB0_5:
	ldw r4, r2+8
	beq r4, r1, .LBB0_7
.LBB0_6:
	ldw r4, r2+0
	sltu r5, r4, r3
	beq r5, r1, .LBB0_8
.LBB0_7:
	ldw r2, r2+4
	beq r2, r1, .LBB0_11
	jal r0, .LBB0_11
.LBB0_8:
	addi r1, r3, 20
	sleu r5, r4, r1
	addi r1, r0, 0
	beq r5, r1, .LBB0_9
	jal r0, .LBB0_10
.LBB0_9:
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
.LBB0_10:
	stw r2+8, r1
	addi r1, r2, 12
.LBB0_11:
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
	beq r3, r1, .LBB1_8
.LBB1_1:
	addi r2, r0, 1
	stw r3+-4, r2
	ori  r2, r0, %lo(heap_start)
	lui r3, %hi(heap_start)
	add r2, r3, r2
	ldw r2, r2+0
	beq r2, r1, .LBB1_8
	jal r0, .LBB1_2
.LBB1_6:
	ldw r2, r2+4
.LBB1_7:
	bne r2, r1, .LBB1_2
	beq r2, r1, .LBB1_8
.LBB1_8:
	jalr r0, r31, 0
.LBB1_2:
	ldw r3, r2+8
	beq r3, r1, .LBB1_6
.LBB1_3:
	ldw r3, r2+4
	beq r3, r1, .LBB1_6
.LBB1_4:
	ldw r4, r3+8
	beq r4, r1, .LBB1_6
.LBB1_5:
	ldw r4, r3+0
	ldw r5, r2+0
	add r4, r4, r5
	addi r4, r4, 12
	stw r2+0, r4
	ldw r3, r3+4
	stw r2+4, r3
	jal r0, .LBB1_7
.Lfunc_end1:
	.size	free, .Lfunc_end1-free
                                        # -- End function
	.globl	calloc                          # -- Begin function calloc
	.p2align	2
	.type	calloc,@function
calloc:                                 # @calloc
# %bb.0:
	addi r1, r0, 0
	beq r3, r1, .LBB2_2
.LBB2_1:
	andi r2, r4, 65535
	srli r5, r3, 16
	mul r6, r5, r2
	srli r7, r4, 16
	andi r8, r3, 65535
	mul r9, r8, r7
	add r6, r9, r6
	mul r2, r8, r2
	srli r2, r2, 16
	add r2, r6, r2
	srli r2, r2, 16
	mul r5, r5, r7
	add r2, r5, r2
	ori r5, r0, 0
	sne r2, r2, r5
	beq r2, r1, .LBB2_2
.LBB2_2:
	mul r2, r3, r4
	addi r1, r0, 0
	add r3, r1, r0
	beq r2, r1, .LBB2_13
.LBB2_3:
	ori  r3, r0, %lo(heap_start)
	lui r4, %hi(heap_start)
	add r4, r4, r3
	ldw r5, r4+0
	addi r3, r0, 0
	beq r5, r3, .LBB2_4
	jal r0, .LBB2_5
.LBB2_4:
	lui r5, 256
	stw r4+0, r5
	addi r6, r5, 4084
	stw r5+0, r6
	addi r6, r5, 4
	addi r7, r0, 0
	stw r6+0, r7
	addi r5, r5, 8
	addi r6, r0, 1
	stw r5+0, r6
.LBB2_5:
	ldw r4, r4+0
	beq r4, r3, .LBB2_13
.LBB2_6:
	addi r3, r2, 7
	addi r5, r0, -8
	and r5, r3, r5
	addi r3, r0, 0
.LBB2_7:
	ldw r6, r4+8
	beq r6, r3, .LBB2_9
.LBB2_8:
	ldw r6, r4+0
	sltu r7, r6, r5
	beq r7, r3, .LBB2_10
.LBB2_9:
	ldw r4, r4+4
	beq r4, r3, .LBB2_13
	jal r0, .LBB2_13
.LBB2_10:
	addi r3, r5, 20
	sleu r7, r6, r3
	addi r3, r0, 0
	beq r7, r3, .LBB2_11
	jal r0, .LBB2_12
.LBB2_11:
	add r7, r4, r5
	addi r8, r7, 12
	sub r6, r6, r5
	addi r6, r6, -12
	stw r7+12, r6
	ldw r6, r4+4
	stw r7+16, r6
	addi r6, r0, 1
	stw r7+20, r6
	stw r4+0, r5
	stw r4+4, r8
.LBB2_12:
	stw r4+8, r3
	addi r3, r4, 12
.LBB2_13:
	beq r3, r1, .LBB2_19
	beq r3, r1, .LBB2_19
.LBB2_14:
	addi r1, r0, 0
	beq r2, r1, .LBB2_15
	jal r0, .LBB2_16
.LBB2_15:
	add r1, r3, r0
	jalr r0, r31, 0
.LBB2_16:
	add r4, r1, r0
.LBB2_17:
	add r5, r3, r4
	stb r5+0, r1
	addi r4, r4, 1
	sltu r5, r4, r2
	bne r5, r1, .LBB2_17
.LBB2_18:
	add r1, r3, r0
.LBB2_19:
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
	beq r3, r1, .LBB3_1
.LBB3_13:
	bne r4, r1, .LBB3_23
	beq r4, r1, .LBB3_14
.LBB3_23:
	ldw r2, r3+-12
	sgeu r2, r2, r4
	beq r2, r1, .LBB3_25
.LBB3_24:
	add r1, r3, r0
	jalr r0, r31, 0
.LBB3_1:
	addi r1, r0, 0
	beq r4, r1, .LBB3_48
.LBB3_2:
	ori  r1, r0, %lo(heap_start)
	lui r2, %hi(heap_start)
	add r2, r2, r1
	ldw r3, r2+0
	addi r1, r0, 0
	beq r3, r1, .LBB3_3
	jal r0, .LBB3_4
.LBB3_3:
	lui r3, 256
	stw r2+0, r3
	addi r5, r3, 4084
	stw r3+0, r5
	addi r5, r3, 4
	addi r6, r0, 0
	stw r5+0, r6
	addi r3, r3, 8
	addi r5, r0, 1
	stw r3+0, r5
.LBB3_4:
	ldw r2, r2+0
	beq r2, r1, .LBB3_48
.LBB3_5:
	addi r1, r4, 7
	addi r3, r0, -8
	and r3, r1, r3
	addi r1, r0, 0
.LBB3_6:
	ldw r4, r2+8
	beq r4, r1, .LBB3_11
.LBB3_7:
	ldw r4, r2+0
	sltu r5, r4, r3
	beq r5, r1, .LBB3_8
.LBB3_11:
	ldw r2, r2+4
	beq r2, r1, .LBB3_12
	jal r0, .LBB3_6
.LBB3_12:
	jal r0, .LBB3_48
	jalr r0, r31, 0
.LBB3_14:
	addi r1, r0, 1
	stw r3+-4, r1
	ori  r1, r0, %lo(heap_start)
	lui r2, %hi(heap_start)
	add r1, r2, r1
	ldw r2, r1+0
	addi r1, r0, 0
	beq r2, r1, .LBB3_48
.LBB3_15:
	addi r1, r0, 0
.LBB3_16:
	ldw r3, r2+8
	beq r3, r1, .LBB3_20
.LBB3_17:
	ldw r3, r2+4
	beq r3, r1, .LBB3_20
.LBB3_18:
	ldw r4, r3+8
	beq r4, r1, .LBB3_20
.LBB3_19:
	ldw r4, r3+0
	ldw r5, r2+0
	add r4, r4, r5
	addi r4, r4, 12
	stw r2+0, r4
	ldw r3, r3+4
	stw r2+4, r3
	jal r0, .LBB3_21
.LBB3_20:
	ldw r2, r2+4
.LBB3_21:
	beq r2, r1, .LBB3_22
	beq r2, r1, .LBB3_22
	jal r0, .LBB3_16
.LBB3_22:
	jal r0, .LBB3_48
	jalr r0, r31, 0
.LBB3_25:
	ori  r2, r0, %lo(heap_start)
	lui r5, %hi(heap_start)
	add r2, r5, r2
	ldw r5, r2+0
	beq r5, r1, .LBB3_26
	jal r0, .LBB3_27
.LBB3_26:
	lui r5, 256
	stw r2+0, r5
	addi r6, r5, 4084
	stw r5+0, r6
	addi r6, r5, 4
	addi r7, r0, 0
	stw r6+0, r7
	addi r5, r5, 8
	addi r6, r0, 1
	stw r5+0, r6
.LBB3_27:
	ldw r6, r2+0
	add r5, r1, r0
	beq r6, r1, .LBB3_35
.LBB3_28:
	addi r4, r4, 7
	addi r5, r0, -8
	and r4, r4, r5
	addi r5, r0, 0
.LBB3_29:
	ldw r7, r6+8
	beq r7, r5, .LBB3_31
.LBB3_30:
	ldw r7, r6+0
	sltu r8, r7, r4
	beq r8, r5, .LBB3_32
.LBB3_31:
	ldw r6, r6+4
	beq r6, r5, .LBB3_35
	jal r0, .LBB3_35
.LBB3_8:
	addi r1, r3, 20
	sleu r5, r4, r1
	addi r1, r0, 0
	beq r5, r1, .LBB3_9
	jal r0, .LBB3_10
.LBB3_9:
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
.LBB3_10:
	stw r2+8, r1
	addi r1, r2, 12
	jalr r0, r31, 0
.LBB3_32:
	addi r5, r4, 20
	sleu r8, r7, r5
	addi r5, r0, 0
	beq r8, r5, .LBB3_33
	jal r0, .LBB3_34
.LBB3_33:
	add r8, r6, r4
	addi r9, r8, 12
	sub r7, r7, r4
	addi r7, r7, -12
	stw r8+12, r7
	ldw r7, r6+4
	stw r8+16, r7
	addi r7, r0, 1
	stw r8+20, r7
	stw r6+0, r4
	stw r6+4, r9
.LBB3_34:
	stw r6+8, r5
	addi r5, r6, 12
.LBB3_35:
	beq r5, r1, .LBB3_48
	beq r5, r1, .LBB3_48
.LBB3_36:
	ldw r4, r3+-12
	addi r1, r0, 0
	beq r4, r1, .LBB3_39
.LBB3_37:
	add r6, r1, r0
.LBB3_38:
	add r7, r3, r6
	ldbu r7, r7+0
	add r8, r5, r6
	stb r8+0, r7
	addi r6, r6, 1
	sltu r7, r6, r4
	bne r7, r1, .LBB3_38
.LBB3_39:
	addi r4, r0, 1
	stw r3+-4, r4
	ldw r2, r2+0
	beq r2, r1, .LBB3_40
	jal r0, .LBB3_41
.LBB3_40:
	add r1, r5, r0
	jalr r0, r31, 0
.LBB3_45:
	ldw r2, r2+4
.LBB3_46:
	bne r2, r1, .LBB3_41
	beq r2, r1, .LBB3_47
.LBB3_41:
	ldw r3, r2+8
	beq r3, r1, .LBB3_45
.LBB3_42:
	ldw r3, r2+4
	beq r3, r1, .LBB3_45
.LBB3_43:
	ldw r4, r3+8
	beq r4, r1, .LBB3_45
.LBB3_44:
	ldw r4, r3+0
	ldw r6, r2+0
	add r4, r4, r6
	addi r4, r4, 12
	stw r2+0, r4
	ldw r3, r3+4
	stw r2+4, r3
	jal r0, .LBB3_46
.LBB3_47:
	add r1, r5, r0
.LBB3_48:
	jalr r0, r31, 0
.Lfunc_end3:
	.size	realloc, .Lfunc_end3-realloc
                                        # -- End function
	.type	heap_start,@object              # @heap_start
	.local	heap_start
	.comm	heap_start,4,4
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
