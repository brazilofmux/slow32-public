	.file	"stdio_minimal.c"
	.text
	.globl	puts                            # -- Begin function puts
	.p2align	2
	.type	puts,@function
puts:                                   # @puts
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	ldbu r1, r3+0
	addi r11, r0, 0
	beq r1, r11, .LBB0_3
.LBB0_1:
	addi r12, r3, 1
.LBB0_2:
	jal r31, putchar
	ldbu r1, r12+0
	addi r12, r12, 1
	bne r1, r11, .LBB0_2
.LBB0_3:
	addi r1, r0, 10    # Load newline character
	jal r31, putchar
	addi r1, r0, 0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	puts, .Lfunc_end0-puts
                                        # -- End function
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
