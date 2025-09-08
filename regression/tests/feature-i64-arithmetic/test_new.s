	.file	"test.ll"
	.text
	.globl	test_add_simple                 # -- Begin function test_add_simple
	.p2align	2
	.type	test_add_simple,@function
test_add_simple:                        # @test_add_simple
# %bb.0:
	add r2, r4, r6
	add r1, r3, r5
	sltu r3, r1, r3
	add r2, r2, r3
	jalr r0, r31, 0
.Lfunc_end0:
	.size	test_add_simple, .Lfunc_end0-test_add_simple
                                        # -- End function
	.globl	test_sub_simple                 # -- Begin function test_sub_simple
	.p2align	2
	.type	test_sub_simple,@function
test_sub_simple:                        # @test_sub_simple
# %bb.0:
	sltu r1, r3, r5
	sub r2, r4, r6
	sub r2, r2, r1
	sub r1, r3, r5
	jalr r0, r31, 0
.Lfunc_end1:
	.size	test_sub_simple, .Lfunc_end1-test_sub_simple
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	addi r3, r0, 100
	addi r5, r0, 200
	addi r11, r0, 0
	add r4, r11, r0
	add r6, r11, r0
	jal r31, test_add_simple
	addi r12, r0, 1
	beq r12, r11, .LBB2_8
	jal r0, .LBB2_9
.LBB2_8:
	addi r3, r0, -1
	add r4, r11, r0
	add r5, r12, r0
	add r6, r11, r0
	jal r31, test_add_simple
	beq r12, r11, .LBB2_1
.LBB2_9:
	addi r1, r0, 1
	jal r0, .LBB2_7
.LBB2_1:
	addi r3, r0, -1
	add r4, r3, r0
	add r5, r12, r0
	add r6, r11, r0
	jal r31, test_add_simple
	beq r12, r11, .LBB2_2
	jal r0, .LBB2_9
.LBB2_2:
	addi r3, r0, 500
	addi r5, r0, 200
	add r4, r11, r0
	add r6, r11, r0
	jal r31, test_sub_simple
	beq r12, r11, .LBB2_3
	jal r0, .LBB2_9
.LBB2_3:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r12, r0
	add r6, r11, r0
	jal r31, test_sub_simple
	beq r12, r11, .LBB2_4
	jal r0, .LBB2_9
.LBB2_4:
	add r3, r11, r0
	add r4, r11, r0
	add r5, r12, r0
	add r6, r11, r0
	jal r31, test_sub_simple
	beq r12, r11, .LBB2_5
	jal r0, .LBB2_9
.LBB2_5:
	lui r1, 633806
	addi r3, r1, 3824
	lui r1, 74565
	addi r4, r1, 1656
	lui r1, 554580
	addi r5, r1, 801
	lui r1, 65245
	addi r6, r1, 2985
	jal r31, test_add_simple
	beq r12, r11, .LBB2_6
	jal r0, .LBB2_9
.LBB2_6:
	addi r1, r0, 0
.LBB2_7:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	main, .Lfunc_end2-main
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
