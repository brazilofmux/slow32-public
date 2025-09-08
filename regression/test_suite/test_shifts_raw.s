	.file	"test_shifts.ll"
	.text
	.globl	test_shift_immediate            # -- Begin function test_shift_immediate
	.type	test_shift_immediate,@function
test_shift_immediate:                   # @test_shift_immediate
# %bb.0:                                # %entry
	ori r1, r0, 80
	jalr r0, r31, 0
.Lfunc_end0:
	.size	test_shift_immediate, .Lfunc_end0-test_shift_immediate
                                        # -- End function
	.globl	test_shift_register             # -- Begin function test_shift_register
	.type	test_shift_register,@function
test_shift_register:                    # @test_shift_register
# %bb.0:                                # %entry
	ori r1, r0, 2
	sll r1, r1, r3
	ori r2, r0, 32
	srl r2, r2, r3
	ori r4, r0, -32
	sra r3, r4, r3
	add r1, r1, r2
	add r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end1:
	.size	test_shift_register, .Lfunc_end1-test_shift_register
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
	jal r31, test_shift_immediate
	ori r2, r0, 80
	beq r1, r2, .LBB2_3
	jal r0, .LBB2_4
# %bb.3:                                # %test2
	ori r3, r0, 2
	jal r31, test_shift_register
	ori r2, r0, 8
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
