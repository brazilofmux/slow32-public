	.file	"test.ll"
	.text
	.globl	test_udiv                       # -- Begin function test_udiv
	.p2align	2
	.type	test_udiv,@function
test_udiv:                              # @test_udiv
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	jal r31, __udivdi3
	addi r2, r0, 0
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	test_udiv, .Lfunc_end0-test_udiv
                                        # -- End function
	.globl	test_sdiv                       # -- Begin function test_sdiv
	.p2align	2
	.type	test_sdiv,@function
test_sdiv:                              # @test_sdiv
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	jal r31, __divdi3
	addi r2, r0, 0
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	test_sdiv, .Lfunc_end1-test_sdiv
                                        # -- End function
	.globl	test_urem                       # -- Begin function test_urem
	.p2align	2
	.type	test_urem,@function
test_urem:                              # @test_urem
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	jal r31, __umoddi3
	addi r2, r0, 0
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	test_urem, .Lfunc_end2-test_urem
                                        # -- End function
	.globl	test_srem                       # -- Begin function test_srem
	.p2align	2
	.type	test_srem,@function
test_srem:                              # @test_srem
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	jal r31, __moddi3
	addi r2, r0, 0
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	test_srem, .Lfunc_end3-test_srem
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
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
	stw fp+-24, lr
	lui r1, 244
	addi r13, r1, 576
	addi r5, r0, 1000
	addi r11, r0, 0
	add r3, r13, r0
	add r4, r11, r0
	add r6, r11, r0
	jal r31, test_udiv
	addi r12, r0, 1
	beq r12, r11, .LBB4_15
	jal r0, .LBB4_16
.LBB4_15:
	lui r14, 870993
	addi r4, r0, 232
	add r3, r14, r0
	add r5, r13, r0
	add r6, r11, r0
	jal r31, test_udiv
	beq r12, r11, .LBB4_1
.LBB4_16:
	addi r1, r0, 1
	jal r0, .LBB4_14
.LBB4_1:
	lui r1, 1048332
	addi r15, r1, 3520
	addi r4, r0, -1
	addi r5, r0, 1000
	add r3, r15, r0
	add r6, r11, r0
	jal r31, test_sdiv
	beq r12, r11, .LBB4_2
	jal r0, .LBB4_16
.LBB4_2:
	addi r5, r0, -1000
	addi r6, r0, -1
	add r3, r13, r0
	add r4, r11, r0
	jal r31, test_sdiv
	beq r12, r11, .LBB4_3
	jal r0, .LBB4_16
.LBB4_3:
	addi r5, r0, -1000
	addi r4, r0, -1
	add r3, r15, r0
	add r6, r4, r0
	jal r31, test_sdiv
	beq r12, r11, .LBB4_4
	jal r0, .LBB4_16
.LBB4_4:
	lui r1, 301
	addi r15, r1, 1671
	addi r5, r0, 1000
	add r3, r15, r0
	add r4, r11, r0
	add r6, r11, r0
	jal r31, test_urem
	beq r12, r11, .LBB4_5
	jal r0, .LBB4_16
.LBB4_5:
	addi r3, r14, 123
	addi r4, r0, 232
	add r5, r13, r0
	add r6, r11, r0
	jal r31, test_urem
	beq r12, r11, .LBB4_6
	jal r0, .LBB4_16
.LBB4_6:
	lui r1, 1048275
	addi r3, r1, 2425
	addi r4, r0, -1
	addi r5, r0, 1000
	add r6, r11, r0
	jal r31, test_srem
	beq r12, r11, .LBB4_7
	jal r0, .LBB4_16
.LBB4_7:
	addi r5, r0, -1000
	addi r6, r0, -1
	add r3, r15, r0
	add r4, r11, r0
	jal r31, test_srem
	beq r12, r11, .LBB4_8
	jal r0, .LBB4_16
.LBB4_8:
	lui r13, 16
	addi r5, r0, 256
	add r3, r13, r0
	add r4, r11, r0
	add r6, r11, r0
	jal r31, test_udiv
	beq r12, r11, .LBB4_9
	jal r0, .LBB4_16
.LBB4_9:
	addi r3, r13, 1
	addi r5, r0, 256
	add r4, r11, r0
	add r6, r11, r0
	jal r31, test_urem
	beq r12, r11, .LBB4_10
	jal r0, .LBB4_16
.LBB4_10:
	lui r1, 30141
	addi r3, r1, 3349
	add r4, r11, r0
	add r5, r12, r0
	add r6, r11, r0
	jal r31, test_udiv
	beq r12, r11, .LBB4_11
	jal r0, .LBB4_16
.LBB4_11:
	lui r1, 3
	addi r5, r1, 57
	add r3, r11, r0
	add r4, r11, r0
	add r6, r11, r0
	jal r31, test_udiv
	beq r12, r11, .LBB4_12
	jal r0, .LBB4_16
.LBB4_12:
	lui r4, 524288
	add r3, r11, r0
	add r5, r11, r0
	add r6, r12, r0
	jal r31, test_udiv
	beq r12, r11, .LBB4_13
	jal r0, .LBB4_16
.LBB4_13:
	addi r1, r0, 0
.LBB4_14:
	ldw lr, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end4:
	.size	main, .Lfunc_end4-main
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
