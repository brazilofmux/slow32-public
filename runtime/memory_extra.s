	.file	"memory_extra.c"
	.text
	.globl	memrchr                         # -- Begin function memrchr
	.p2align	2
	.type	memrchr,@function
memrchr:                                # @memrchr
# %bb.0:
	addi r1, r0, 0
	andi r2, r4, 255
.LBB0_1:
	bne r5, r1, .LBB0_3
.LBB0_2:
	jal r0, .LBB0_5
	jalr r0, r31, 0
.LBB0_3:
	addi r4, r5, -1
	add r5, r3, r5
	ldbu r6, r5+-1
	add r5, r4, r0
	bne r6, r2, .LBB0_1
.LBB0_4:
	add r1, r3, r4
.LBB0_5:
	jalr r0, r31, 0
.Lfunc_end0:
	.size	memrchr, .Lfunc_end0-memrchr
                                        # -- End function
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
