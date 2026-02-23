	.file	"test_arrays.c"
	.text
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	lui r1, 7
	addi r1, r1, -1872
	jalr r0, r31, 0
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
                                        # -- End function
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git c0d812afef07081ba1a1e21ccfeb301488b0b716)"
	.section	".note.GNU-stack","",@progbits
