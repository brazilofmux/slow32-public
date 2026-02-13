	.file	"unify.c"
	.text
	.globl	unify                           # -- Begin function unify
	.p2align	2
	.type	unify,@function
unify:                                  # @unify
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
	stw fp+-44, lr
	lui r16, %hi(usp)
	addi r16, r16, %lo(usp)
	lui r17, %hi(ustack)
	addi r17, r17, %lo(ustack)
	stw r17+0, r3
	addi r1, r0, 2
	stw r16+0, r1
	lui r3, %hi(ustack+4)
	addi r3, r3, %lo(ustack+4)
	stw r3+0, r4
	addi r18, r0, 1
	addi r11, r0, 0
	addi r19, r0, 3
	lui r20, 1
	jal r0, .LBB0_4
.LBB0_1:
	add r1, r13, r0
.LBB0_2:
	srli r3, r1, 2
	add r4, r12, r0
	jal r31, bind
.LBB0_3:
	ldw r1, r16+0
	ble r1, r11, .LBB0_18
.LBB0_4:
	addi r1, r1, -1
	stw r16+0, r1
	slli r1, r1, 2
	add r1, r1, r17
	ldw r3, r1+0
	jal r31, deref
	add r12, r1, r0
	ldw r1, r16+0
	addi r1, r1, -1
	stw r16+0, r1
	slli r1, r1, 2
	add r1, r1, r17
	ldw r3, r1+0
	jal r31, deref
	beq r1, r12, .LBB0_3
.LBB0_5:
	add r13, r1, r0
	andi r3, r1, 3
	beq r3, r19, .LBB0_1
.LBB0_6:
	andi r1, r12, 3
	bne r1, r19, .LBB0_8
.LBB0_7:
	add r1, r12, r0
	add r12, r13, r0
	jal r0, .LBB0_2
.LBB0_8:
	beq r13, r11, .LBB0_19
.LBB0_9:
	bne r3, r11, .LBB0_19
.LBB0_10:
	beq r12, r11, .LBB0_19
.LBB0_11:
	bne r1, r11, .LBB0_19
.LBB0_12:
	add r3, r13, r0
	jal r31, compound_functor
	add r14, r1, r0
	add r3, r12, r0
	jal r31, compound_functor
	bne r14, r1, .LBB0_19
.LBB0_13:
	add r3, r13, r0
	jal r31, compound_arity
	add r14, r1, r0
	add r3, r12, r0
	jal r31, compound_arity
	bne r14, r1, .LBB0_19
.LBB0_14:
	ldw r1, r16+0
	slli r3, r14, 1
	add r1, r1, r3
	bgt r1, r20, .LBB0_19
.LBB0_15:
	blt r14, r18, .LBB0_3
.LBB0_16:
	add r15, r11, r0
.LBB0_17:
	add r3, r13, r0
	add r4, r15, r0
	jal r31, compound_arg
	ldw r3, r16+0
	addi r4, r3, 1
	stw r16+0, r4
	slli r3, r3, 2
	add r3, r3, r17
	stw r3+0, r1
	add r3, r12, r0
	add r4, r15, r0
	jal r31, compound_arg
	ldw r3, r16+0
	addi r4, r3, 1
	stw r16+0, r4
	slli r3, r3, 2
	add r3, r3, r17
	stw r3+0, r1
	addi r15, r15, 1
	bne r14, r15, .LBB0_17
	jal r0, .LBB0_3
.LBB0_18:
	add r11, r18, r0
.LBB0_19:
	add r1, r11, r0
	ldw lr, fp+-44
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
	.size	unify, .Lfunc_end0-unify
                                        # -- End function
	.type	usp,@object                     # @usp
	.local	usp
	.comm	usp,4,4
	.type	ustack,@object                  # @ustack
	.local	ustack
	.comm	ustack,16384,4
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
