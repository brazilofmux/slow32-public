	.file	"test.ll"
	.text
	.globl	test_mul                        # -- Begin function test_mul
	.p2align	2
	.type	test_mul,@function
test_mul:                               # @test_mul
# %bb.0:
	andi r1, r5, 65535
	srli r2, r3, 16
	mul r7, r2, r1
	srli r8, r5, 16
	andi r9, r3, 65535
	mul r10, r9, r8
	add r7, r10, r7
	mul r1, r9, r1
	srli r9, r1, 16
	add r7, r7, r9
	slli r9, r7, 16
	andi r1, r1, 65535
	or  r1, r1, r9
	srli r7, r7, 16
	mul r2, r2, r8
	add r2, r2, r7
	mul r3, r3, r6
	add r2, r2, r3
	mul r3, r4, r5
	add r2, r2, r3
	jalr r0, r31, 0
.Lfunc_end0:
	.size	test_mul, .Lfunc_end0-test_mul
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
	stw fp+-20, lr
	addi r3, r0, 100
	addi r5, r0, 200
	addi r11, r0, 0
	add r4, r11, r0
	add r6, r11, r0
	jal r31, test_mul
	addi r12, r0, 1
	beq r12, r11, .LBB1_13
	jal r0, .LBB1_14
.LBB1_13:
	lui r1, 244
	addi r13, r1, 576
	addi r5, r0, 1000
	add r3, r13, r0
	add r4, r11, r0
	add r6, r11, r0
	jal r31, test_mul
	beq r12, r11, .LBB1_1
.LBB1_14:
	addi r1, r0, 1
	jal r0, .LBB1_12
.LBB1_1:
	lui r1, 30141
	addi r14, r1, 3349
	add r3, r14, r0
	add r4, r11, r0
	add r5, r11, r0
	add r6, r11, r0
	jal r31, test_mul
	beq r12, r11, .LBB1_2
	jal r0, .LBB1_14
.LBB1_2:
	lui r1, 241127
	addi r3, r1, 2225
	add r4, r11, r0
	add r5, r12, r0
	add r6, r11, r0
	jal r31, test_mul
	beq r12, r11, .LBB1_3
	jal r0, .LBB1_14
.LBB1_3:
	addi r5, r0, -1
	add r3, r14, r0
	add r4, r11, r0
	add r6, r5, r0
	jal r31, test_mul
	beq r12, r11, .LBB1_4
	jal r0, .LBB1_14
.LBB1_4:
	addi r3, r0, -1000
	addi r5, r0, -2000
	addi r4, r0, -1
	add r6, r4, r0
	jal r31, test_mul
	beq r12, r11, .LBB1_5
	jal r0, .LBB1_14
.LBB1_5:
	lui r1, 3
	addi r3, r1, 57
	addi r5, r0, 256
	add r4, r11, r0
	add r6, r11, r0
	jal r31, test_mul
	beq r12, r11, .LBB1_6
	jal r0, .LBB1_14
.LBB1_6:
	lui r3, 256
	add r4, r11, r0
	add r5, r3, r0
	add r6, r11, r0
	jal r31, test_mul
	beq r12, r11, .LBB1_7
	jal r0, .LBB1_14
.LBB1_7:
	addi r3, r0, -1
	addi r5, r0, 2
	add r4, r11, r0
	add r6, r11, r0
	jal r31, test_mul
	beq r12, r11, .LBB1_8
	jal r0, .LBB1_14
.LBB1_8:
	lui r3, 16
	add r4, r11, r0
	add r5, r3, r0
	add r6, r11, r0
	jal r31, test_mul
	beq r12, r11, .LBB1_9
	jal r0, .LBB1_14
.LBB1_9:
	lui r1, 1048332
	addi r3, r1, 3520
	addi r4, r0, -1
	add r5, r13, r0
	add r6, r11, r0
	jal r31, test_mul
	beq r12, r11, .LBB1_10
	jal r0, .LBB1_14
.LBB1_10:
	lui r1, 524288
	addi r4, r1, 4095
	addi r3, r0, -1
	addi r5, r0, 2
	add r6, r11, r0
	jal r31, test_mul
	beq r12, r11, .LBB1_11
	jal r0, .LBB1_14
.LBB1_11:
	addi r1, r0, 0
.LBB1_12:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
