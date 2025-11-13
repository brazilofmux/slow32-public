	.file	"builtins.c"
	.text
	.globl	__umodsi3                       # -- Begin function __umodsi3
	.p2align	2
	.type	__umodsi3,@function
__umodsi3:                              # @__umodsi3
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	addi r1, r0, 0
	beq r4, r1, .LBB0_2
.LBB0_1:
	add r3, r11, r0
	add r12, r4, r0
	jal r31, __udivsi3
	mul r1, r1, r12
	sub r11, r11, r1
.LBB0_2:
	add r1, r11, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	__umodsi3, .Lfunc_end0-__umodsi3
                                        # -- End function
	.globl	__udivdi3                       # -- Begin function __udivdi3
	.p2align	2
	.type	__udivdi3,@function
__udivdi3:                              # @__udivdi3
# %bb.0:
	addi sp, sp, -72
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 72
	addi r1, fp, -16
	stw r1+4, r4
	stw r1+0, r3
	addi r1, fp, -24
	stw r1+4, r6
	stw r1+0, r5
	ldw r3, r1+4
	ldw r1, r1+0
	or  r1, r1, r3
	addi r3, r0, 0
	bne r1, r3, .LBB1_2
	jal r0, .LBB1_1
.LBB1_1:
	addi r1, r0, -1
	addi r3, fp, -8
	stw r3+4, r1
	stw r3+0, r1
	jal r0, .LBB1_17
.LBB1_2:
	addi r1, fp, -16
	ldw r3, r1+0
	ldw r1, r1+4
	addi r4, fp, -24
	ldw r5, r4+0
	ldw r4, r4+4
	sgeu r6, r1, r4
	sgeu r3, r3, r5
	xor r3, r3, r6
	seq r1, r1, r4
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	xor r1, r6, r1
	bne r1, r4, .LBB1_4
	jal r0, .LBB1_3
.LBB1_3:
	addi r1, r0, 0
	addi r3, fp, -8
	stw r3+4, r1
	stw r3+0, r1
	jal r0, .LBB1_17
.LBB1_4:
	addi r1, fp, -24
	ldw r3, r1+4
	ldw r1, r1+0
	addi r4, r1, -1
	sltu r5, r4, r1
	add r5, r3, r5
	addi r5, r5, -1
	and r1, r1, r4
	and r3, r3, r5
	or  r1, r1, r3
	addi r3, r0, 0
	bne r1, r3, .LBB1_9
	jal r0, .LBB1_5
.LBB1_5:
	addi r1, r0, 0
	addi r3, fp, -28
	stw r3+0, r1
	addi r1, fp, -24
	ldw r3, r1+0
	ldw r1, r1+4
	addi r4, fp, -36
	stw r4+4, r1
	stw r4+0, r3
	jal r0, .LBB1_6
.LBB1_6:
	addi r1, fp, -36
	ldw r3, r1+4
	ldw r1, r1+0
	addi r4, r0, 2
	sltu r1, r1, r4
	ori r4, r0, 0
	seq r3, r3, r4
	addi r4, r0, 0
	sub r3, r4, r3
	and r1, r1, r3
	bne r1, r4, .LBB1_8
	jal r0, .LBB1_7
.LBB1_7:
	addi r1, fp, -36
	ldw r3, r1+0
	ldw r4, r1+4
	slli r5, r4, 31
	srli r3, r3, 1
	or  r3, r3, r5
	srli r4, r4, 1
	stw r1+4, r4
	stw r1+0, r3
	addi r1, fp, -28
	ldw r3, r1+0
	addi r3, r3, 1
	stw r1+0, r3
	jal r0, .LBB1_6
.LBB1_8:
	addi r1, fp, -16
	ldw r3, r1+4
	ldw r1, r1+0
	addi r4, fp, -28
	ldw r4, r4+0
	srl r1, r1, r4
	addi r5, r0, 32
	sub r5, r5, r4
	sll r5, r3, r5
	or  r1, r1, r5
	addi r5, r4, -32
	srl r5, r3, r5
	xor r5, r5, r1
	addi r6, r0, 31
	sgtu r6, r4, r6
	addi r7, r0, 0
	sub r6, r7, r6
	and r5, r5, r6
	xor r1, r1, r5
	srl r3, r3, r4
	and r4, r3, r6
	xor r3, r3, r4
	addi r4, fp, -8
	stw r4+4, r3
	stw r4+0, r1
	jal r0, .LBB1_17
.LBB1_9:
	addi r1, r0, 0
	addi r3, fp, -44
	stw r3+4, r1
	stw r3+0, r1
	addi r3, fp, -52
	stw r3+4, r1
	stw r3+0, r1
	addi r1, r0, 63
	addi r3, fp, -56
	stw r3+0, r1
	jal r0, .LBB1_10
.LBB1_10:
	addi r1, fp, -56
	ldw r1, r1+0
	addi r3, r0, -1
	bgt r1, r3, .LBB1_12
	jal r0, .LBB1_11
.LBB1_11:
	jal r0, .LBB1_16
.LBB1_12:
	addi r1, fp, -52
	ldw r3, r1+4
	ldw r4, r1+0
	srli r5, r4, 31
	slli r3, r3, 1
	or  r3, r3, r5
	slli r4, r4, 1
	addi r5, fp, -16
	ldw r6, r5+4
	ldw r5, r5+0
	addi r7, fp, -56
	ldw r7, r7+0
	srl r5, r5, r7
	addi r8, r0, 32
	sub r8, r8, r7
	sll r8, r6, r8
	or  r5, r5, r8
	addi r8, r7, -32
	srl r6, r6, r8
	xor r6, r6, r5
	addi r8, r0, 31
	sgtu r7, r7, r8
	and r6, r6, r7
	xor r5, r5, r6
	andi r5, r5, 1
	or  r4, r4, r5
	stw r1+4, r3
	stw r1+0, r4
	ldw r3, r1+0
	ldw r1, r1+4
	addi r4, fp, -24
	ldw r5, r4+0
	ldw r4, r4+4
	sltu r6, r1, r4
	sltu r3, r3, r5
	xor r3, r3, r6
	seq r1, r1, r4
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	xor r1, r6, r1
	bne r1, r4, .LBB1_14
	jal r0, .LBB1_13
.LBB1_13:
	addi r1, fp, -24
	ldw r3, r1+4
	ldw r1, r1+0
	addi r4, fp, -52
	ldw r5, r4+4
	ldw r6, r4+0
	sltu r7, r6, r1
	sub r3, r5, r3
	sub r3, r3, r7
	sub r1, r6, r1
	stw r4+0, r1
	stw r4+4, r3
	addi r1, fp, -56
	ldw r1, r1+0
	addi r3, r0, 1
	sll r4, r3, r1
	addi r5, r0, 31
	sgtu r5, r1, r5
	addi r6, r0, 0
	sub r5, r6, r5
	and r6, r4, r5
	xor r4, r4, r6
	addi r6, r0, 32
	sub r6, r6, r1
	srl r6, r3, r6
	addi r1, r1, -32
	sll r1, r3, r1
	xor r1, r1, r6
	and r1, r1, r5
	xor r1, r6, r1
	addi r3, fp, -44
	ldw r5, r3+0
	ldw r6, r3+4
	or  r1, r6, r1
	or  r4, r5, r4
	stw r3+0, r4
	stw r3+4, r1
	jal r0, .LBB1_14
.LBB1_14:
	jal r0, .LBB1_15
.LBB1_15:
	addi r1, fp, -56
	ldw r3, r1+0
	addi r3, r3, -1
	stw r1+0, r3
	jal r0, .LBB1_10
.LBB1_16:
	addi r1, fp, -44
	ldw r3, r1+0
	ldw r1, r1+4
	addi r4, fp, -8
	stw r4+4, r1
	stw r4+0, r3
	jal r0, .LBB1_17
.LBB1_17:
	addi r3, fp, -8
	ldw r1, r3+0
	ldw r2, r3+4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 72
	jalr r0, r31, 0
.Lfunc_end1:
	.size	__udivdi3, .Lfunc_end1-__udivdi3
                                        # -- End function
	.globl	__divdi3                        # -- Begin function __divdi3
	.p2align	2
	.type	__divdi3,@function
__divdi3:                               # @__divdi3
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	srai r7, r4, 31
	xor r3, r3, r7
	sltu r1, r3, r7
	xor r8, r4, r7
	sub r8, r8, r7
	sub r1, r8, r1
	sub r3, r3, r7
	srai r8, r6, 31
	xor r5, r5, r8
	sltu r7, r5, r8
	xor r9, r6, r8
	sub r9, r9, r8
	sub r7, r9, r7
	sub r5, r5, r8
	xor r11, r6, r4
	add r4, r1, r0
	add r6, r7, r0
	jal r31, __udivdi3
	ori r3, r0, 0
	sne r4, r1, r3
	add r4, r2, r4
	addi r5, r0, 0
	sub r4, r5, r4
	sub r6, r5, r1
	slt r3, r11, r3
	xor r6, r6, r1
	sub r3, r5, r3
	and r5, r6, r3
	xor r1, r1, r5
	xor r4, r4, r2
	and r3, r4, r3
	xor r2, r2, r3
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	__divdi3, .Lfunc_end2-__divdi3
                                        # -- End function
	.globl	__umoddi3                       # -- Begin function __umoddi3
	.p2align	2
	.type	__umoddi3,@function
__umoddi3:                              # @__umoddi3
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 56
	addi r1, fp, -16
	stw r1+4, r4
	stw r1+0, r3
	addi r1, fp, -24
	stw r1+4, r6
	stw r1+0, r5
	ldw r3, r1+4
	ldw r1, r1+0
	or  r1, r1, r3
	addi r3, r0, 0
	bne r1, r3, .LBB3_2
	jal r0, .LBB3_1
.LBB3_1:
	addi r1, fp, -16
	ldw r3, r1+0
	ldw r1, r1+4
	addi r4, fp, -8
	stw r4+4, r1
	stw r4+0, r3
	jal r0, .LBB3_14
.LBB3_2:
	addi r1, fp, -16
	ldw r3, r1+0
	ldw r1, r1+4
	addi r4, fp, -24
	ldw r5, r4+0
	ldw r4, r4+4
	sgeu r6, r1, r4
	sgeu r3, r3, r5
	xor r3, r3, r6
	seq r1, r1, r4
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	xor r1, r6, r1
	bne r1, r4, .LBB3_4
	jal r0, .LBB3_3
.LBB3_3:
	addi r1, fp, -16
	ldw r3, r1+0
	ldw r1, r1+4
	addi r4, fp, -8
	stw r4+4, r1
	stw r4+0, r3
	jal r0, .LBB3_14
.LBB3_4:
	addi r1, fp, -24
	ldw r3, r1+4
	ldw r1, r1+0
	addi r4, r1, -1
	sltu r5, r4, r1
	add r5, r3, r5
	addi r5, r5, -1
	and r1, r1, r4
	and r3, r3, r5
	or  r1, r1, r3
	addi r3, r0, 0
	bne r1, r3, .LBB3_6
	jal r0, .LBB3_5
.LBB3_5:
	addi r1, fp, -16
	ldw r3, r1+0
	ldw r1, r1+4
	addi r4, fp, -24
	ldw r5, r4+4
	ldw r4, r4+0
	addi r6, r4, -1
	sltu r4, r6, r4
	add r4, r5, r4
	addi r4, r4, -1
	and r1, r1, r4
	and r3, r3, r6
	addi r4, fp, -8
	stw r4+0, r3
	stw r4+4, r1
	jal r0, .LBB3_14
.LBB3_6:
	addi r1, r0, 0
	addi r3, fp, -32
	stw r3+4, r1
	stw r3+0, r1
	addi r1, r0, 63
	addi r3, fp, -36
	stw r3+0, r1
	jal r0, .LBB3_7
.LBB3_7:
	addi r1, fp, -36
	ldw r1, r1+0
	addi r3, r0, -1
	bgt r1, r3, .LBB3_9
	jal r0, .LBB3_8
.LBB3_8:
	jal r0, .LBB3_13
.LBB3_9:
	addi r1, fp, -32
	ldw r3, r1+4
	ldw r4, r1+0
	srli r5, r4, 31
	slli r3, r3, 1
	or  r3, r3, r5
	slli r4, r4, 1
	addi r5, fp, -16
	ldw r6, r5+4
	ldw r5, r5+0
	addi r7, fp, -36
	ldw r7, r7+0
	srl r5, r5, r7
	addi r8, r0, 32
	sub r8, r8, r7
	sll r8, r6, r8
	or  r5, r5, r8
	addi r8, r7, -32
	srl r6, r6, r8
	xor r6, r6, r5
	addi r8, r0, 31
	sgtu r7, r7, r8
	and r6, r6, r7
	xor r5, r5, r6
	andi r5, r5, 1
	or  r4, r4, r5
	stw r1+4, r3
	stw r1+0, r4
	ldw r3, r1+0
	ldw r1, r1+4
	addi r4, fp, -24
	ldw r5, r4+0
	ldw r4, r4+4
	sltu r6, r1, r4
	sltu r3, r3, r5
	xor r3, r3, r6
	seq r1, r1, r4
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	xor r1, r6, r1
	bne r1, r4, .LBB3_11
	jal r0, .LBB3_10
.LBB3_10:
	addi r1, fp, -24
	ldw r3, r1+4
	ldw r1, r1+0
	addi r4, fp, -32
	ldw r5, r4+4
	ldw r6, r4+0
	sltu r7, r6, r1
	sub r3, r5, r3
	sub r3, r3, r7
	sub r1, r6, r1
	stw r4+0, r1
	stw r4+4, r3
	jal r0, .LBB3_11
.LBB3_11:
	jal r0, .LBB3_12
.LBB3_12:
	addi r1, fp, -36
	ldw r3, r1+0
	addi r3, r3, -1
	stw r1+0, r3
	jal r0, .LBB3_7
.LBB3_13:
	addi r1, fp, -32
	ldw r3, r1+0
	ldw r1, r1+4
	addi r4, fp, -8
	stw r4+4, r1
	stw r4+0, r3
	jal r0, .LBB3_14
.LBB3_14:
	addi r3, fp, -8
	ldw r1, r3+0
	ldw r2, r3+4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end3:
	.size	__umoddi3, .Lfunc_end3-__umoddi3
                                        # -- End function
	.globl	__moddi3                        # -- Begin function __moddi3
	.p2align	2
	.type	__moddi3,@function
__moddi3:                               # @__moddi3
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	ori r11, r0, 0
	slt r12, r4, r11
	srai r1, r4, 31
	xor r3, r3, r1
	sltu r7, r3, r1
	xor r4, r4, r1
	sub r4, r4, r1
	sub r4, r4, r7
	sub r3, r3, r1
	srai r1, r6, 31
	xor r5, r5, r1
	sltu r7, r5, r1
	xor r6, r6, r1
	sub r6, r6, r1
	sub r6, r6, r7
	sub r5, r5, r1
	jal r31, __umoddi3
	sne r3, r1, r11
	add r3, r2, r3
	addi r4, r0, 0
	sub r3, r4, r3
	sub r5, r4, r1
	xor r5, r5, r1
	sub r4, r4, r12
	and r5, r5, r4
	xor r1, r1, r5
	xor r3, r3, r2
	and r3, r3, r4
	xor r2, r2, r3
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	__moddi3, .Lfunc_end4-__moddi3
                                        # -- End function
	.globl	__clzdi2                        # -- Begin function __clzdi2
	.p2align	2
	.type	__clzdi2,@function
__clzdi2:                               # @__clzdi2
# %bb.0:
	or  r5, r3, r4
	addi r1, r0, 0
	beq r5, r1, .LBB5_2
.LBB5_1:
	ori r5, r0, 0
	seq r5, r4, r5
	xor r6, r3, r4
	sub r5, r1, r5
	and r6, r6, r5
	xor r4, r4, r6
	and r6, r3, r5
	xor r3, r3, r6
	andi r5, r5, 32
	lui r6, 16
	sltu r6, r4, r6
	ori  r7, r5, 16
	slli r8, r4, 16
	srli r9, r3, 16
	or  r8, r8, r9
	slli r9, r3, 16
	sub r6, r1, r6
	xor r9, r9, r3
	and r9, r9, r6
	xor r3, r3, r9
	xor r8, r8, r4
	and r8, r8, r6
	xor r4, r4, r8
	xor r7, r7, r5
	and r6, r7, r6
	xor r5, r5, r6
	lui r6, 4096
	sltu r6, r4, r6
	ori  r7, r5, 8
	srli r8, r3, 24
	slli r9, r4, 8
	or  r8, r9, r8
	slli r9, r3, 8
	xor r9, r9, r3
	sub r6, r1, r6
	and r9, r9, r6
	xor r3, r3, r9
	xor r8, r8, r4
	and r8, r8, r6
	xor r4, r4, r8
	xor r7, r7, r5
	and r6, r7, r6
	xor r5, r5, r6
	lui r6, 65536
	sltu r6, r4, r6
	ori  r7, r5, 4
	srli r8, r3, 28
	slli r9, r4, 4
	or  r8, r9, r8
	slli r9, r3, 4
	xor r9, r9, r3
	sub r6, r1, r6
	and r9, r9, r6
	xor r3, r3, r9
	xor r8, r8, r4
	and r8, r8, r6
	xor r4, r4, r8
	xor r7, r7, r5
	and r6, r7, r6
	xor r5, r5, r6
	lui r6, 262144
	sltu r6, r4, r6
	addi r7, r5, 2
	srli r3, r3, 30
	slli r8, r4, 2
	or  r3, r8, r3
	xor r3, r3, r4
	sub r1, r1, r6
	and r3, r3, r1
	xor r3, r4, r3
	xor r4, r7, r5
	and r1, r4, r1
	xor r1, r5, r1
	ori r4, r0, -1
	sgt r3, r3, r4
	add r1, r1, r3
	jal r0, .LBB5_3
.LBB5_2:
	addi r1, r0, 64
.LBB5_3:
	jalr r0, r31, 0
.Lfunc_end5:
	.size	__clzdi2, .Lfunc_end5-__clzdi2
                                        # -- End function
	.globl	__ashrdi3                       # -- Begin function __ashrdi3
	.p2align	2
	.type	__ashrdi3,@function
__ashrdi3:                              # @__ashrdi3
# %bb.0:
	add r1, r3, r0
	addi r3, r0, 0
	beq r5, r3, .LBB6_4
.LBB6_1:
	addi r3, r0, 31
	bgt r5, r3, .LBB6_3
.LBB6_2:
	srl r1, r1, r5
	addi r3, r0, 32
	sub r3, r3, r5
	sll r3, r4, r3
	or  r1, r3, r1
	sra r4, r4, r5
	jal r0, .LBB6_4
.LBB6_3:
	addi r1, r5, -32
	sra r1, r4, r1
	srai r4, r4, 31
.LBB6_4:
	add r2, r4, r0
	jalr r0, r31, 0
.Lfunc_end6:
	.size	__ashrdi3, .Lfunc_end6-__ashrdi3
                                        # -- End function
	.globl	__lshrdi3                       # -- Begin function __lshrdi3
	.p2align	2
	.type	__lshrdi3,@function
__lshrdi3:                              # @__lshrdi3
# %bb.0:
	add r1, r3, r0
	addi r3, r0, 0
	beq r5, r3, .LBB7_4
.LBB7_1:
	addi r6, r0, 31
	bgt r5, r6, .LBB7_3
.LBB7_2:
	srl r1, r1, r5
	addi r3, r0, 32
	sub r3, r3, r5
	sll r3, r4, r3
	or  r1, r3, r1
	srl r4, r4, r5
	jal r0, .LBB7_4
.LBB7_3:
	addi r1, r5, -32
	srl r1, r4, r1
	add r4, r3, r0
.LBB7_4:
	add r2, r4, r0
	jalr r0, r31, 0
.Lfunc_end7:
	.size	__lshrdi3, .Lfunc_end7-__lshrdi3
                                        # -- End function
	.globl	__ashldi3                       # -- Begin function __ashldi3
	.p2align	2
	.type	__ashldi3,@function
__ashldi3:                              # @__ashldi3
# %bb.0:
	add r1, r3, r0
	addi r3, r0, 0
	beq r5, r3, .LBB8_4
.LBB8_1:
	addi r6, r0, 31
	bgt r5, r6, .LBB8_3
.LBB8_2:
	sll r3, r4, r5
	addi r4, r0, 32
	sub r4, r4, r5
	srl r4, r1, r4
	or  r4, r3, r4
	sll r1, r1, r5
	jal r0, .LBB8_4
.LBB8_3:
	addi r4, r5, -32
	sll r4, r1, r4
	add r1, r3, r0
.LBB8_4:
	add r2, r4, r0
	jalr r0, r31, 0
.Lfunc_end8:
	.size	__ashldi3, .Lfunc_end8-__ashldi3
                                        # -- End function
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git d89d732e6b8f9c83536a5038ba4e43c85c0ecc82)"
	.section	".note.GNU-stack","",@progbits
