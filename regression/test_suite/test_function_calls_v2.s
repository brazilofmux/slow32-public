	.file	"test_function_calls.ll"
	.text
	.globl	simple_add                      # -- Begin function simple_add
	.type	simple_add,@function
simple_add:                             # @simple_add
# %bb.0:
	add r1, r3, r4
	jalr r0, r31, 0
.Lfunc_end0:
	.size	simple_add, .Lfunc_end0-simple_add
                                        # -- End function
	.globl	recursive_factorial             # -- Begin function recursive_factorial
	.type	recursive_factorial,@function
recursive_factorial:                    # @recursive_factorial
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	ori r1, r0, 0
	beq r3, r1, .LBB1_1
.LBB1_3:
	addi r1, r3, -1
	add r2, r3, r0
	add r3, r1, r0
	jal r31, recursive_factorial
	mul r1, r2, r1
	jal r0, .LBB1_2
.LBB1_1:
	ori r1, r0, 1
.LBB1_2:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	recursive_factorial, .Lfunc_end1-recursive_factorial
                                        # -- End function
	.globl	many_args                       # -- Begin function many_args
	.type	many_args,@function
many_args:                              # @many_args
# %bb.0:
	addi r1, fp, 12
	ldw r1, r1+0
	addi r2, fp, 8
	ldw r2, r2+0
	add r3, r3, r4
	add r3, r3, r5
	add r3, r3, r6
	add r3, r3, r7
	add r3, r3, r8
	add r3, r3, r9
	add r3, r3, r10
	add r2, r3, r2
	add r1, r2, r1
	jalr r0, r31, 0
.Lfunc_end2:
	.size	many_args, .Lfunc_end2-many_args
                                        # -- End function
	.globl	level3                          # -- Begin function level3
	.type	level3,@function
level3:                                 # @level3
# %bb.0:
	slli r1, r3, 1
	jalr r0, r31, 0
.Lfunc_end3:
	.size	level3, .Lfunc_end3-level3
                                        # -- End function
	.globl	level2                          # -- Begin function level2
	.type	level2,@function
level2:                                 # @level2
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	addi r3, r3, 5
	jal r31, level3
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	level2, .Lfunc_end4-level2
                                        # -- End function
	.globl	level1                          # -- Begin function level1
	.type	level1,@function
level1:                                 # @level1
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	addi r3, r3, 10
	jal r31, level2
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	level1, .Lfunc_end5-level1
                                        # -- End function
	.globl	main                            # -- Begin function main
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	ori r3, r0, 15
	ori r4, r0, 25
	jal r31, simple_add
	ori r2, r0, 40
	beq r1, r2, .LBB6_5
	jal r0, .LBB6_6
.LBB6_5:
	ori r3, r0, 5
	jal r31, recursive_factorial
	ori r3, r0, 120
	beq r1, r3, .LBB6_1
.LBB6_6:
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
	jal r0, .LBB6_4
.LBB6_1:
	ori r1, r0, 10
	addi r3, sp, -8
	add sp, r3, r0
	addi r4, sp, -4
	stw r4+0, r1
	ori r1, r0, 9
	stw r3+0, r1
	ori r3, r0, 1
	ori r4, r0, 2
	ori r5, r0, 3
	ori r6, r0, 4
	ori r7, r0, 5
	ori r8, r0, 6
	ori r9, r0, 7
	ori r10, r0, 8
	jal r31, many_args
	ori r3, r0, 55
	beq r1, r3, .LBB6_2
	jal r0, .LBB6_6
.LBB6_2:
	ori r3, r0, 5
	jal r31, level1
	beq r1, r2, .LBB6_3
	jal r0, .LBB6_6
.LBB6_3:
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
.LBB6_4:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	main, .Lfunc_end6-main
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
