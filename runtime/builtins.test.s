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
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, lr
	addi r1, fp, -12
	stw r1+4, r4
	stw r1+0, r3
	addi r7, fp, -20
	stw r7+4, r6
	stw r7+0, r5
	ldw r4, r1+4
	ldw r3, r1+0
	ldw r6, r7+4
	ldw r5, r7+0
	addi r7, r0, 0
	jal r31, udivmoddi3_core
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end1:
	.size	__udivdi3, .Lfunc_end1-__udivdi3
                                        # -- End function
	.p2align	2                               # -- Begin function udivmoddi3_core
	.type	udivmoddi3_core,@function
udivmoddi3_core:                        # @udivmoddi3_core
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 56
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
	or  r1, r5, r6
	addi r8, r0, 0
	beq r1, r8, .LBB2_4
.LBB2_1:
	sgeu r1, r3, r5
	sgeu r9, r4, r6
	xor r1, r1, r9
	seq r10, r4, r6
	sub r10, r8, r10
	and r1, r1, r10
	xor r1, r9, r1
	bne r1, r8, .LBB2_7
.LBB2_2:
	addi r1, r0, 0
	beq r7, r1, .LBB2_16
.LBB2_3:
	stw r7+0, r3
	stw r7+4, r4
	addi r1, r0, 0
	add r9, r1, r0
	jal r0, .LBB2_23
.LBB2_4:
	addi r1, r0, 0
	beq r7, r1, .LBB2_6
.LBB2_5:
	stw r7+0, r3
	stw r7+4, r4
.LBB2_6:
	addi r1, r0, -1
	add r9, r1, r0
	jal r0, .LBB2_23
.LBB2_7:
	addi r1, r5, -1
	sltu r9, r1, r5
	add r9, r6, r9
	addi r9, r9, -1
	and r10, r5, r1
	and r11, r6, r9
	or  r10, r10, r11
	beq r10, r8, .LBB2_17
.LBB2_8:
	addi r10, r0, 63
	addi r11, r0, 32
	addi r12, r0, 31
	addi r14, r0, -1
	addi r15, r0, 1
	add r16, r8, r0
	add r13, r8, r0
	add r1, r8, r0
	add r9, r8, r0
	jal r0, .LBB2_11
.LBB2_9:
	sll r17, r15, r10
	or  r1, r17, r1
.LBB2_10:
	addi r10, r10, -1
	beq r10, r14, .LBB2_14
.LBB2_11:
	srli r17, r16, 31
	slli r13, r13, 1
	or  r13, r13, r17
	slli r16, r16, 1
	srl r17, r3, r10
	sub r18, r11, r10
	sll r18, r4, r18
	or  r18, r17, r18
	addi r17, r10, -32
	srl r19, r4, r17
	xor r19, r19, r18
	sgtu r20, r10, r12
	and r19, r19, r20
	xor r18, r18, r19
	andi r18, r18, 1
	or  r16, r18, r16
	sltu r18, r16, r5
	sltu r19, r13, r6
	xor r20, r18, r19
	seq r21, r13, r6
	sub r21, r8, r21
	and r20, r20, r21
	xor r19, r19, r20
	bne r19, r8, .LBB2_10
.LBB2_12:
	sub r13, r13, r6
	sub r13, r13, r18
	sub r16, r16, r5
	bltu r10, r11, .LBB2_9
.LBB2_13:
	sll r17, r15, r17
	or  r9, r17, r9
	jal r0, .LBB2_10
.LBB2_14:
	addi r3, r0, 0
	beq r7, r3, .LBB2_23
.LBB2_15:
	stw r7+0, r16
	stw r7+4, r13
	jal r0, .LBB2_23
.LBB2_16:
	add r9, r1, r0
	jal r0, .LBB2_23
.LBB2_17:
	xori r8, r5, 1
	or  r11, r8, r6
	addi r10, r0, 0
	add r8, r10, r0
	beq r11, r10, .LBB2_20
.LBB2_18:
	addi r11, r0, 0
	ori r12, r0, 0
	addi r13, r0, 3
	add r8, r11, r0
.LBB2_19:
	srli r14, r5, 1
	slli r15, r6, 31
	or  r14, r14, r15
	srli r15, r6, 1
	addi r8, r8, 1
	sne r16, r6, r12
	sgtu r5, r5, r13
	xor r5, r5, r16
	seq r6, r6, r12
	sub r6, r11, r6
	and r5, r5, r6
	xor r16, r16, r5
	add r5, r14, r0
	add r6, r15, r0
	bne r16, r11, .LBB2_19
.LBB2_20:
	beq r7, r10, .LBB2_22
.LBB2_21:
	and r5, r9, r4
	and r1, r1, r3
	stw r7+0, r1
	stw r7+4, r5
.LBB2_22:
	srl r1, r3, r8
	addi r3, r0, 32
	sub r3, r3, r8
	sll r3, r4, r3
	or  r1, r1, r3
	addi r3, r8, -32
	srl r3, r4, r3
	xor r3, r3, r1
	addi r5, r0, 31
	sgtu r5, r8, r5
	addi r6, r0, 0
	sub r5, r6, r5
	and r3, r3, r5
	xor r1, r1, r3
	srl r3, r4, r8
	and r4, r3, r5
	xor r9, r3, r4
.LBB2_23:
	add r2, r9, r0
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
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end2:
	.size	udivmoddi3_core, .Lfunc_end2-udivmoddi3_core
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
.Lfunc_end3:
	.size	__divdi3, .Lfunc_end3-__divdi3
                                        # -- End function
	.globl	__umoddi3                       # -- Begin function __umoddi3
	.p2align	2
	.type	__umoddi3,@function
__umoddi3:                              # @__umoddi3
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, lr
	addi r1, fp, -16
	stw r1+4, r4
	stw r1+0, r3
	addi r7, fp, -24
	stw r7+4, r6
	stw r7+0, r5
	ldw r3, r1+0
	ldw r4, r1+4
	addi r11, fp, -32
	stw r11+4, r4
	stw r11+0, r3
	ldw r4, r1+4
	ldw r3, r1+0
	ldw r6, r7+4
	ldw r5, r7+0
	add r7, r11, r0
	jal r31, udivmoddi3_core
	ldw r1, r11+0
	ldw r2, r11+4
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end4:
	.size	__umoddi3, .Lfunc_end4-__umoddi3
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
.Lfunc_end5:
	.size	__moddi3, .Lfunc_end5-__moddi3
                                        # -- End function
	.globl	__clzdi2                        # -- Begin function __clzdi2
	.p2align	2
	.type	__clzdi2,@function
__clzdi2:                               # @__clzdi2
# %bb.0:
	or  r5, r3, r4
	addi r1, r0, 0
	beq r5, r1, .LBB6_2
.LBB6_1:
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
	jal r0, .LBB6_3
.LBB6_2:
	addi r1, r0, 64
.LBB6_3:
	jalr r0, r31, 0
.Lfunc_end6:
	.size	__clzdi2, .Lfunc_end6-__clzdi2
                                        # -- End function
	.globl	__ashrdi3                       # -- Begin function __ashrdi3
	.p2align	2
	.type	__ashrdi3,@function
__ashrdi3:                              # @__ashrdi3
# %bb.0:
	add r1, r3, r0
	addi r3, r0, 0
	beq r5, r3, .LBB7_4
.LBB7_1:
	addi r3, r0, 31
	bgt r5, r3, .LBB7_3
.LBB7_2:
	srl r1, r1, r5
	addi r3, r0, 32
	sub r3, r3, r5
	sll r3, r4, r3
	or  r1, r3, r1
	sra r4, r4, r5
	jal r0, .LBB7_4
.LBB7_3:
	addi r1, r5, -32
	sra r1, r4, r1
	srai r4, r4, 31
.LBB7_4:
	add r2, r4, r0
	jalr r0, r31, 0
.Lfunc_end7:
	.size	__ashrdi3, .Lfunc_end7-__ashrdi3
                                        # -- End function
	.globl	__lshrdi3                       # -- Begin function __lshrdi3
	.p2align	2
	.type	__lshrdi3,@function
__lshrdi3:                              # @__lshrdi3
# %bb.0:
	add r1, r3, r0
	addi r3, r0, 0
	beq r5, r3, .LBB8_4
.LBB8_1:
	addi r6, r0, 31
	bgt r5, r6, .LBB8_3
.LBB8_2:
	srl r1, r1, r5
	addi r3, r0, 32
	sub r3, r3, r5
	sll r3, r4, r3
	or  r1, r3, r1
	srl r4, r4, r5
	jal r0, .LBB8_4
.LBB8_3:
	addi r1, r5, -32
	srl r1, r4, r1
	add r4, r3, r0
.LBB8_4:
	add r2, r4, r0
	jalr r0, r31, 0
.Lfunc_end8:
	.size	__lshrdi3, .Lfunc_end8-__lshrdi3
                                        # -- End function
	.globl	__ashldi3                       # -- Begin function __ashldi3
	.p2align	2
	.type	__ashldi3,@function
__ashldi3:                              # @__ashldi3
# %bb.0:
	add r1, r3, r0
	addi r3, r0, 0
	beq r5, r3, .LBB9_4
.LBB9_1:
	addi r6, r0, 31
	bgt r5, r6, .LBB9_3
.LBB9_2:
	sll r3, r4, r5
	addi r4, r0, 32
	sub r4, r4, r5
	srl r4, r1, r4
	or  r4, r3, r4
	sll r1, r1, r5
	jal r0, .LBB9_4
.LBB9_3:
	addi r4, r5, -32
	sll r4, r1, r4
	add r1, r3, r0
.LBB9_4:
	add r2, r4, r0
	jalr r0, r31, 0
.Lfunc_end9:
	.size	__ashldi3, .Lfunc_end9-__ashldi3
                                        # -- End function
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 91d20c24bea33cebad7dd8cf3f8201a47a06a180)"
	.section	".note.GNU-stack","",@progbits
