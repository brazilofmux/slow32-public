	.file	"builtins.c"
	.text
	.globl	__udivdi3                       # -- Begin function __udivdi3
	.p2align	2
	.type	__udivdi3,@function
__udivdi3:                              # @__udivdi3
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
	stw fp+-36, r19
	stw fp+-40, r20
	or  r2, r5, r6
	addi r1, r0, 0
	beq r2, r1, .LBB0_1
.LBB0_2:
	sltu r2, r3, r5
	sltu r7, r4, r6
	xor r2, r2, r7
	seq r8, r4, r6
	sub r8, r1, r8
	and r2, r2, r8
	xor r7, r7, r2
	add r2, r1, r0
	beq r7, r1, .LBB0_3
	jal r0, .LBB0_10
.LBB0_3:
	addi r1, r5, -1
	sltu r2, r1, r5
	add r2, r6, r2
	addi r2, r2, -1
	and r1, r5, r1
	and r2, r6, r2
	or  r1, r1, r2
	addi r7, r0, 0
	beq r1, r7, .LBB0_6
.LBB0_4:
	addi r8, r0, 63
	addi r9, r0, 32
	addi r10, r0, 31
	addi r11, r0, 1
	addi r12, r0, -1
	add r13, r7, r0
	add r14, r7, r0
	add r1, r7, r0
	add r2, r7, r0
.LBB0_5:
	srli r15, r13, 31
	slli r14, r14, 1
	or  r14, r14, r15
	slli r13, r13, 1
	srl r15, r3, r8
	sub r16, r9, r8
	sll r17, r4, r16
	or  r15, r15, r17
	addi r17, r8, -32
	srl r18, r4, r17
	xor r18, r18, r15
	sgtu r19, r8, r10
	sub r19, r7, r19
	and r18, r18, r19
	xor r15, r15, r18
	andi r15, r15, 1
	or  r15, r15, r13
	sltu r13, r15, r5
	sltu r18, r14, r6
	xor r13, r13, r18
	seq r20, r14, r6
	sub r20, r7, r20
	and r13, r13, r20
	xor r13, r18, r13
	srl r16, r11, r16
	sll r17, r11, r17
	xor r17, r17, r16
	and r17, r17, r19
	xor r16, r16, r17
	sll r17, r11, r8
	and r18, r17, r19
	xor r17, r17, r18
	sub r13, r7, r13
	and r18, r17, r13
	xor r17, r17, r18
	and r18, r16, r13
	xor r16, r16, r18
	or  r2, r16, r2
	or  r1, r17, r1
	and r16, r6, r13
	xor r16, r6, r16
	and r13, r5, r13
	xor r17, r5, r13
	sub r13, r15, r17
	sltu r15, r15, r17
	sub r14, r14, r16
	sub r14, r14, r15
	addi r8, r8, -1
	bne r8, r12, .LBB0_5
	jal r0, .LBB0_10
.LBB0_1:
	addi r1, r0, -1
	add r2, r1, r0
	jal r0, .LBB0_10
.LBB0_6:
	xori r1, r5, 1
	or  r7, r1, r6
	addi r1, r0, 0
	add r2, r1, r0
	beq r7, r1, .LBB0_9
.LBB0_7:
	addi r7, r0, 0
	ori r8, r0, 0
	addi r9, r0, 3
	add r2, r7, r0
.LBB0_8:
	srli r10, r5, 1
	slli r11, r6, 31
	or  r10, r10, r11
	srli r11, r6, 1
	addi r2, r2, 1
	sne r12, r6, r8
	sgtu r5, r5, r9
	xor r5, r5, r12
	seq r6, r6, r8
	sub r6, r7, r6
	and r5, r5, r6
	xor r12, r12, r5
	add r5, r10, r0
	add r6, r11, r0
	bne r12, r7, .LBB0_8
.LBB0_9:
	srl r3, r3, r2
	addi r5, r0, 32
	sub r5, r5, r2
	sll r5, r4, r5
	or  r3, r3, r5
	addi r5, r2, -32
	srl r5, r4, r5
	xor r5, r5, r3
	addi r6, r0, 31
	sgtu r6, r2, r6
	sub r6, r1, r6
	and r1, r5, r6
	xor r1, r3, r1
	srl r2, r4, r2
	and r3, r2, r6
	xor r2, r2, r3
.LBB0_10:
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
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end0:
	.size	__udivdi3, .Lfunc_end0-__udivdi3
                                        # -- End function
	.globl	__divdi3                        # -- Begin function __divdi3
	.p2align	2
	.type	__divdi3,@function
__divdi3:                               # @__divdi3
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
	or  r7, r5, r6
	addi r1, r0, 0
	ori r2, r0, 0
	beq r7, r1, .LBB1_1
.LBB1_2:
	srai r7, r4, 31
	xor r8, r3, r7
	sltu r3, r8, r7
	xor r9, r4, r7
	sub r9, r9, r7
	sub r3, r9, r3
	sub r7, r8, r7
	srai r8, r6, 31
	xor r9, r5, r8
	sltu r5, r9, r8
	xor r10, r6, r8
	sub r10, r10, r8
	sub r5, r10, r5
	sub r8, r9, r8
	sltu r9, r7, r8
	sltu r10, r3, r5
	xor r9, r9, r10
	seq r11, r3, r5
	sub r11, r1, r11
	and r9, r9, r11
	xor r11, r10, r9
	add r9, r1, r0
	add r10, r1, r0
	beq r11, r1, .LBB1_3
	jal r0, .LBB1_10
.LBB1_3:
	addi r9, r8, -1
	sltu r10, r9, r8
	add r10, r5, r10
	addi r10, r10, -1
	and r9, r8, r9
	and r10, r5, r10
	or  r9, r9, r10
	addi r11, r0, 0
	beq r9, r11, .LBB1_6
.LBB1_4:
	addi r12, r0, 63
	addi r13, r0, 32
	addi r14, r0, 31
	addi r15, r0, 1
	addi r16, r0, -1
	add r17, r11, r0
	add r18, r11, r0
	add r9, r11, r0
	add r10, r11, r0
.LBB1_5:
	srli r19, r17, 31
	slli r18, r18, 1
	or  r18, r18, r19
	slli r17, r17, 1
	srl r19, r7, r12
	sub r20, r13, r12
	sll r21, r3, r20
	or  r19, r19, r21
	addi r21, r12, -32
	srl r22, r3, r21
	xor r22, r22, r19
	sgtu r23, r12, r14
	sub r23, r11, r23
	and r22, r22, r23
	xor r19, r19, r22
	andi r19, r19, 1
	or  r19, r19, r17
	sltu r17, r19, r8
	sltu r22, r18, r5
	xor r17, r17, r22
	seq r24, r18, r5
	sub r24, r11, r24
	and r17, r17, r24
	xor r17, r22, r17
	srl r20, r15, r20
	sll r21, r15, r21
	xor r21, r21, r20
	and r21, r21, r23
	xor r20, r20, r21
	sll r21, r15, r12
	and r22, r21, r23
	xor r21, r21, r22
	sub r17, r11, r17
	and r22, r21, r17
	xor r21, r21, r22
	and r22, r20, r17
	xor r20, r20, r22
	or  r10, r20, r10
	or  r9, r21, r9
	and r20, r5, r17
	xor r20, r5, r20
	and r17, r8, r17
	xor r21, r8, r17
	sub r17, r19, r21
	sltu r19, r19, r21
	sub r18, r18, r20
	sub r18, r18, r19
	addi r12, r12, -1
	bne r12, r16, .LBB1_5
	jal r0, .LBB1_10
.LBB1_1:
	addi r9, r0, -1
	add r10, r9, r0
	jal r0, .LBB1_10
.LBB1_6:
	xori r9, r8, 1
	or  r11, r9, r5
	addi r9, r0, 0
	add r10, r9, r0
	beq r11, r9, .LBB1_9
.LBB1_7:
	addi r11, r0, 0
	addi r12, r0, 3
	add r10, r11, r0
.LBB1_8:
	srli r13, r8, 1
	slli r14, r5, 31
	or  r13, r13, r14
	srli r14, r5, 1
	addi r10, r10, 1
	sne r15, r5, r2
	sgtu r8, r8, r12
	xor r8, r8, r15
	seq r5, r5, r2
	sub r5, r11, r5
	and r5, r8, r5
	xor r15, r15, r5
	add r8, r13, r0
	add r5, r14, r0
	bne r15, r11, .LBB1_8
.LBB1_9:
	srl r5, r7, r10
	addi r7, r0, 32
	sub r7, r7, r10
	sll r7, r3, r7
	or  r5, r5, r7
	addi r7, r10, -32
	srl r7, r3, r7
	xor r7, r7, r5
	addi r8, r0, 31
	sgtu r8, r10, r8
	sub r8, r9, r8
	and r7, r7, r8
	xor r9, r5, r7
	srl r3, r3, r10
	and r5, r3, r8
	xor r10, r3, r5
.LBB1_10:
	xor r3, r6, r4
	sne r4, r9, r2
	add r4, r10, r4
	sub r4, r1, r4
	sub r5, r1, r9
	slt r2, r3, r2
	xor r3, r5, r9
	sub r2, r1, r2
	and r1, r3, r2
	xor r1, r9, r1
	xor r3, r4, r10
	and r2, r3, r2
	xor r2, r10, r2
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
.Lfunc_end1:
	.size	__divdi3, .Lfunc_end1-__divdi3
                                        # -- End function
	.globl	__umoddi3                       # -- Begin function __umoddi3
	.p2align	2
	.type	__umoddi3,@function
__umoddi3:                              # @__umoddi3
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	addi r1, r5, -1
	sltu r2, r1, r5
	add r2, r6, r2
	addi r2, r2, -1
	sgeu r7, r1, r3
	sgeu r8, r2, r4
	xor r9, r7, r8
	seq r10, r2, r4
	addi r7, r0, 0
	sub r10, r7, r10
	and r9, r9, r10
	xor r8, r8, r9
	beq r8, r7, .LBB2_2
.LBB2_1:
	add r1, r3, r0
	add r2, r4, r0
	jal r0, .LBB2_5
.LBB2_2:
	and r8, r5, r1
	and r9, r6, r2
	or  r8, r8, r9
	beq r8, r7, .LBB2_6
.LBB2_3:
	addi r8, r0, 63
	addi r9, r0, 32
	addi r10, r0, 31
	addi r11, r0, -1
	add r1, r7, r0
	add r2, r7, r0
.LBB2_4:
	srli r12, r1, 31
	slli r2, r2, 1
	or  r2, r2, r12
	slli r1, r1, 1
	srl r12, r3, r8
	sub r13, r9, r8
	sll r13, r4, r13
	or  r12, r12, r13
	addi r13, r8, -32
	srl r13, r4, r13
	xor r13, r13, r12
	sgtu r14, r8, r10
	and r13, r13, r14
	xor r12, r12, r13
	andi r12, r12, 1
	or  r12, r12, r1
	sltu r1, r12, r5
	sltu r13, r2, r6
	xor r1, r1, r13
	seq r14, r2, r6
	sub r14, r7, r14
	and r1, r1, r14
	xor r1, r13, r1
	sub r1, r7, r1
	and r13, r6, r1
	xor r13, r6, r13
	and r1, r5, r1
	xor r14, r5, r1
	sub r1, r12, r14
	sltu r12, r12, r14
	sub r2, r2, r13
	sub r2, r2, r12
	addi r8, r8, -1
	bne r8, r11, .LBB2_4
	jal r0, .LBB2_5
.LBB2_6:
	and r1, r1, r3
	and r2, r2, r4
.LBB2_5:
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	__umoddi3, .Lfunc_end2-__umoddi3
                                        # -- End function
	.globl	__moddi3                        # -- Begin function __moddi3
	.p2align	2
	.type	__moddi3,@function
__moddi3:                               # @__moddi3
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
	srai r1, r4, 31
	xor r3, r3, r1
	sltu r2, r3, r1
	xor r7, r4, r1
	sub r7, r7, r1
	sub r2, r7, r2
	sub r3, r3, r1
	srai r1, r6, 31
	xor r7, r5, r1
	sltu r5, r7, r1
	xor r6, r6, r1
	sub r6, r6, r1
	sub r5, r6, r5
	sub r6, r7, r1
	addi r7, r6, -1
	sltu r1, r7, r6
	add r1, r5, r1
	addi r8, r1, -1
	seq r9, r8, r2
	addi r1, r0, 0
	sub r9, r1, r9
	sgeu r10, r8, r2
	sgeu r11, r7, r3
	xor r11, r11, r10
	and r9, r11, r9
	xor r9, r10, r9
	beq r9, r1, .LBB3_2
.LBB3_1:
	add r11, r3, r0
	add r12, r2, r0
	jal r0, .LBB3_5
.LBB3_2:
	and r9, r6, r7
	and r10, r5, r8
	or  r9, r9, r10
	beq r9, r1, .LBB3_6
.LBB3_3:
	addi r7, r0, 63
	addi r8, r0, 32
	addi r9, r0, 31
	addi r10, r0, -1
	add r11, r1, r0
	add r12, r1, r0
.LBB3_4:
	srli r13, r11, 31
	slli r12, r12, 1
	or  r12, r12, r13
	slli r11, r11, 1
	srl r13, r3, r7
	sub r14, r8, r7
	sll r14, r2, r14
	or  r13, r13, r14
	addi r14, r7, -32
	srl r14, r2, r14
	xor r14, r14, r13
	sgtu r15, r7, r9
	and r14, r14, r15
	xor r13, r13, r14
	andi r13, r13, 1
	or  r13, r13, r11
	sltu r11, r13, r6
	sltu r14, r12, r5
	xor r11, r11, r14
	seq r15, r12, r5
	sub r15, r1, r15
	and r11, r11, r15
	xor r11, r14, r11
	sub r11, r1, r11
	and r14, r5, r11
	xor r14, r5, r14
	and r11, r6, r11
	xor r15, r6, r11
	sub r11, r13, r15
	sltu r13, r13, r15
	sub r12, r12, r14
	sub r12, r12, r13
	addi r7, r7, -1
	bne r7, r10, .LBB3_4
	jal r0, .LBB3_5
.LBB3_6:
	and r11, r7, r3
	and r12, r8, r2
.LBB3_5:
	ori r2, r0, 0
	slt r3, r4, r2
	sne r2, r11, r2
	add r2, r12, r2
	sub r2, r1, r2
	sub r4, r1, r11
	xor r4, r4, r11
	sub r3, r1, r3
	and r1, r4, r3
	xor r1, r11, r1
	xor r2, r2, r12
	and r2, r2, r3
	xor r2, r12, r2
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end3:
	.size	__moddi3, .Lfunc_end3-__moddi3
                                        # -- End function
	.globl	__clzdi2                        # -- Begin function __clzdi2
	.p2align	2
	.type	__clzdi2,@function
__clzdi2:                               # @__clzdi2
# %bb.0:
	or  r2, r3, r4
	addi r1, r0, 0
	beq r2, r1, .LBB4_1
.LBB4_2:
	ori r2, r0, 0
	seq r2, r4, r2
	xor r5, r3, r4
	sub r2, r1, r2
	and r5, r5, r2
	xor r4, r4, r5
	and r5, r3, r2
	xor r3, r3, r5
	andi r2, r2, 32
	lui r5, 16
	sltu r5, r4, r5
	ori  r6, r2, 16
	slli r7, r4, 16
	srli r8, r3, 16
	or  r7, r7, r8
	slli r8, r3, 16
	sub r5, r1, r5
	xor r8, r8, r3
	and r8, r8, r5
	xor r3, r3, r8
	xor r7, r7, r4
	and r7, r7, r5
	xor r4, r4, r7
	xor r6, r6, r2
	and r5, r6, r5
	xor r2, r2, r5
	lui r5, 4096
	sltu r5, r4, r5
	ori  r6, r2, 8
	srli r7, r3, 24
	slli r8, r4, 8
	or  r7, r8, r7
	slli r8, r3, 8
	xor r8, r8, r3
	sub r5, r1, r5
	and r8, r8, r5
	xor r3, r3, r8
	xor r7, r7, r4
	and r7, r7, r5
	xor r4, r4, r7
	xor r6, r6, r2
	and r5, r6, r5
	xor r2, r2, r5
	lui r5, 65536
	sltu r5, r4, r5
	ori  r6, r2, 4
	srli r7, r3, 28
	slli r8, r4, 4
	or  r7, r8, r7
	slli r8, r3, 4
	xor r8, r8, r3
	sub r5, r1, r5
	and r8, r8, r5
	xor r3, r3, r8
	xor r7, r7, r4
	and r7, r7, r5
	xor r4, r4, r7
	xor r6, r6, r2
	and r5, r6, r5
	xor r2, r2, r5
	lui r5, 262144
	sltu r5, r4, r5
	addi r6, r2, 2
	srli r3, r3, 30
	slli r7, r4, 2
	or  r3, r7, r3
	xor r3, r3, r4
	sub r1, r1, r5
	and r3, r3, r1
	xor r3, r4, r3
	xor r4, r6, r2
	and r1, r4, r1
	xor r1, r2, r1
	ori r2, r0, -1
	sgt r2, r3, r2
	add r1, r1, r2
.LBB4_3:
	jalr r0, r31, 0
.LBB4_1:
	addi r1, r0, 64
	jalr r0, r31, 0
.Lfunc_end4:
	.size	__clzdi2, .Lfunc_end4-__clzdi2
                                        # -- End function
	.globl	__ashrdi3                       # -- Begin function __ashrdi3
	.p2align	2
	.type	__ashrdi3,@function
__ashrdi3:                              # @__ashrdi3
# %bb.0:
	add r2, r4, r0
	add r1, r3, r0
	addi r3, r0, 0
	beq r5, r3, .LBB5_1
.LBB5_2:
	ori r4, r0, 31
	sgt r4, r5, r4
	bne r4, r3, .LBB5_4
.LBB5_3:
	srl r1, r1, r5
	addi r3, r0, 32
	sub r3, r3, r5
	sll r3, r2, r3
	or  r1, r3, r1
	sra r2, r2, r5
	jalr r0, r31, 0
.LBB5_1:
	jal r0, .LBB5_5
	jalr r0, r31, 0
.LBB5_4:
	addi r1, r5, -32
	sra r1, r2, r1
	srai r2, r2, 31
.LBB5_5:
	jalr r0, r31, 0
.Lfunc_end5:
	.size	__ashrdi3, .Lfunc_end5-__ashrdi3
                                        # -- End function
	.globl	__lshrdi3                       # -- Begin function __lshrdi3
	.p2align	2
	.type	__lshrdi3,@function
__lshrdi3:                              # @__lshrdi3
# %bb.0:
	add r2, r4, r0
	add r1, r3, r0
	addi r3, r0, 0
	beq r5, r3, .LBB6_1
.LBB6_2:
	ori r4, r0, 31
	sgt r4, r5, r4
	bne r4, r3, .LBB6_4
.LBB6_3:
	srl r1, r1, r5
	addi r3, r0, 32
	sub r3, r3, r5
	sll r3, r2, r3
	or  r1, r3, r1
	srl r2, r2, r5
	jalr r0, r31, 0
.LBB6_1:
	jal r0, .LBB6_5
	jalr r0, r31, 0
.LBB6_4:
	addi r1, r5, -32
	srl r1, r2, r1
	add r2, r3, r0
.LBB6_5:
	jalr r0, r31, 0
.Lfunc_end6:
	.size	__lshrdi3, .Lfunc_end6-__lshrdi3
                                        # -- End function
	.globl	__ashldi3                       # -- Begin function __ashldi3
	.p2align	2
	.type	__ashldi3,@function
__ashldi3:                              # @__ashldi3
# %bb.0:
	add r2, r4, r0
	add r1, r3, r0
	addi r3, r0, 0
	beq r5, r3, .LBB7_1
.LBB7_2:
	ori r4, r0, 31
	sgt r4, r5, r4
	bne r4, r3, .LBB7_4
.LBB7_3:
	sll r2, r2, r5
	addi r3, r0, 32
	sub r3, r3, r5
	srl r3, r1, r3
	or  r2, r2, r3
	sll r1, r1, r5
	jalr r0, r31, 0
.LBB7_1:
	jal r0, .LBB7_5
	jalr r0, r31, 0
.LBB7_4:
	addi r2, r5, -32
	sll r2, r1, r2
	add r1, r3, r0
.LBB7_5:
	jalr r0, r31, 0
.Lfunc_end7:
	.size	__ashldi3, .Lfunc_end7-__ashldi3
                                        # -- End function
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
