	.file	"test_arithmetic.ll"
	.text
	.globl	test_arithmetic                 # -- Begin function test_arithmetic
	.type	test_arithmetic,@function
test_arithmetic:                        # @test_arithmetic
# %bb.0:                                # %entry
	ori r1, r0, 87
	jalr r0, r31, 0
.Lfunc_end0:
	.size	test_arithmetic, .Lfunc_end0-test_arithmetic
                                        # -- End function
	.globl	test_remainder                  # -- Begin function test_remainder
	.type	test_remainder,@function
test_remainder:                         # @test_remainder
# %bb.0:                                # %entry
	ori r1, r0, 4
	jalr r0, r31, 0
.Lfunc_end1:
	.size	test_remainder, .Lfunc_end1-test_remainder
                                        # -- End function
	.globl	main                            # -- Begin function main
	.type	main,@function
main:                                   # @main
# %bb.0:                                # %entry
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	jal r31, test_arithmetic
	ori r2, r0, 87
	beq r1, r2, .LBB2_3
	jal r0, .LBB2_4
# %bb.3:                                # %test2
.LBB2_3:
	jal r31, test_remainder
	ori r2, r0, 4
	beq r1, r2, .LBB2_1
.LBB2_4:                                # %fail
	ori r3, r0, 70
	jal r31, debug_char
	ori r3, r0, 65
	jal r31, debug_char
	ori r3, r0, 73
	jal r31, debug_char
	ori r3, r0, 76
	jal r31, debug_char
	ori r3, r0, 10
	jal r31, debug_char
	ori r1, r0, 1
	jal r0, .LBB2_2
.LBB2_1:                                # %pass
	ori r3, r0, 80
	jal r31, debug_char
	ori r3, r0, 65
	jal r31, debug_char
	ori r3, r0, 83
	jal r31, debug_char
	jal r31, debug_char
	ori r3, r0, 10
	jal r31, debug_char
	ori r1, r0, 0
.LBB2_2:                                # %pass
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	main, .Lfunc_end2-main
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
