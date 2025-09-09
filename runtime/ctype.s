	.file	"ctype.c"
	.text
	.globl	isalnum                         # -- Begin function isalnum
	.p2align	2
	.type	isalnum,@function
isalnum:                                # @isalnum
# %bb.0:
	addi r1, r3, -48
	addi r2, r0, 10
	sltu r1, r1, r2
	addi r2, r0, -33
	and r2, r3, r2
	addi r2, r2, -65
	addi r3, r0, 26
	sltu r2, r2, r3
	or  r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end0:
	.size	isalnum, .Lfunc_end0-isalnum
                                        # -- End function
	.globl	isalpha                         # -- Begin function isalpha
	.p2align	2
	.type	isalpha,@function
isalpha:                                # @isalpha
# %bb.0:
	addi r1, r0, -33
	and r1, r3, r1
	addi r1, r1, -65
	addi r2, r0, 26
	sltu r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end1:
	.size	isalpha, .Lfunc_end1-isalpha
                                        # -- End function
	.globl	isblank                         # -- Begin function isblank
	.p2align	2
	.type	isblank,@function
isblank:                                # @isblank
# %bb.0:
	ori r1, r0, 32
	seq r1, r3, r1
	ori r2, r0, 9
	seq r2, r3, r2
	or  r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end2:
	.size	isblank, .Lfunc_end2-isblank
                                        # -- End function
	.globl	iscntrl                         # -- Begin function iscntrl
	.p2align	2
	.type	iscntrl,@function
iscntrl:                                # @iscntrl
# %bb.0:
	addi r1, r0, 32
	sltu r1, r3, r1
	ori r2, r0, 127
	seq r2, r3, r2
	or  r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end3:
	.size	iscntrl, .Lfunc_end3-iscntrl
                                        # -- End function
	.globl	isdigit                         # -- Begin function isdigit
	.p2align	2
	.type	isdigit,@function
isdigit:                                # @isdigit
# %bb.0:
	addi r1, r3, -48
	addi r2, r0, 10
	sltu r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end4:
	.size	isdigit, .Lfunc_end4-isdigit
                                        # -- End function
	.globl	isgraph                         # -- Begin function isgraph
	.p2align	2
	.type	isgraph,@function
isgraph:                                # @isgraph
# %bb.0:
	addi r1, r3, -33
	addi r2, r0, 94
	sltu r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end5:
	.size	isgraph, .Lfunc_end5-isgraph
                                        # -- End function
	.globl	islower                         # -- Begin function islower
	.p2align	2
	.type	islower,@function
islower:                                # @islower
# %bb.0:
	addi r1, r3, -97
	addi r2, r0, 26
	sltu r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end6:
	.size	islower, .Lfunc_end6-islower
                                        # -- End function
	.globl	isprint                         # -- Begin function isprint
	.p2align	2
	.type	isprint,@function
isprint:                                # @isprint
# %bb.0:
	addi r1, r3, -32
	addi r2, r0, 95
	sltu r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end7:
	.size	isprint, .Lfunc_end7-isprint
                                        # -- End function
	.globl	ispunct                         # -- Begin function ispunct
	.p2align	2
	.type	ispunct,@function
ispunct:                                # @ispunct
# %bb.0:
	addi r2, r3, -127
	addi r1, r0, 0
	addi r4, r0, -94
	sltu r2, r2, r4
	bne r2, r1, .LBB8_2
.LBB8_1:
	addi r1, r3, -58
	addi r2, r0, -10
	sltu r1, r1, r2
	andi r2, r3, 95
	addi r2, r2, -91
	addi r3, r0, -26
	sltu r2, r2, r3
	and r1, r1, r2
.LBB8_2:
	jalr r0, r31, 0
.Lfunc_end8:
	.size	ispunct, .Lfunc_end8-ispunct
                                        # -- End function
	.globl	isspace                         # -- Begin function isspace
	.p2align	2
	.type	isspace,@function
isspace:                                # @isspace
# %bb.0:
	addi r1, r0, 1
	addi r2, r3, -9
	addi r4, r0, 4
	sltu r2, r2, r4
	addi r4, r0, 0
	beq r2, r4, .LBB9_2
.LBB9_1:
	jal r0, .LBB9_4
	jalr r0, r31, 0
.LBB9_2:
	addi r2, r0, 32
	beq r3, r2, .LBB9_4
.LBB9_3:
	ori r1, r0, 13
	seq r1, r3, r1
.LBB9_4:
	jalr r0, r31, 0
.Lfunc_end9:
	.size	isspace, .Lfunc_end9-isspace
                                        # -- End function
	.globl	isupper                         # -- Begin function isupper
	.p2align	2
	.type	isupper,@function
isupper:                                # @isupper
# %bb.0:
	addi r1, r3, -65
	addi r2, r0, 26
	sltu r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end10:
	.size	isupper, .Lfunc_end10-isupper
                                        # -- End function
	.globl	isxdigit                        # -- Begin function isxdigit
	.p2align	2
	.type	isxdigit,@function
isxdigit:                               # @isxdigit
# %bb.0:
	addi r1, r3, -48
	addi r2, r0, 10
	sltu r1, r1, r2
	addi r2, r0, -33
	and r2, r3, r2
	addi r2, r2, -65
	addi r3, r0, 6
	sltu r2, r2, r3
	or  r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end11:
	.size	isxdigit, .Lfunc_end11-isxdigit
                                        # -- End function
	.globl	tolower                         # -- Begin function tolower
	.p2align	2
	.type	tolower,@function
tolower:                                # @tolower
# %bb.0:
	addi r1, r3, -65
	addi r2, r0, 26
	sltu r1, r1, r2
	ori  r2, r3, 32
	xor r2, r2, r3
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r2, r1
	xor r1, r3, r1
	jalr r0, r31, 0
.Lfunc_end12:
	.size	tolower, .Lfunc_end12-tolower
                                        # -- End function
	.globl	toupper                         # -- Begin function toupper
	.p2align	2
	.type	toupper,@function
toupper:                                # @toupper
# %bb.0:
	addi r1, r3, -97
	addi r2, r0, 26
	sltu r1, r1, r2
	addi r2, r3, -32
	xor r2, r2, r3
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r2, r1
	xor r1, r3, r1
	jalr r0, r31, 0
.Lfunc_end13:
	.size	toupper, .Lfunc_end13-toupper
                                        # -- End function
	.globl	isascii                         # -- Begin function isascii
	.p2align	2
	.type	isascii,@function
isascii:                                # @isascii
# %bb.0:
	addi r1, r0, 128
	sltu r1, r3, r1
	jalr r0, r31, 0
.Lfunc_end14:
	.size	isascii, .Lfunc_end14-isascii
                                        # -- End function
	.globl	toascii                         # -- Begin function toascii
	.p2align	2
	.type	toascii,@function
toascii:                                # @toascii
# %bb.0:
	andi r1, r3, 127
	jalr r0, r31, 0
.Lfunc_end15:
	.size	toascii, .Lfunc_end15-toascii
                                        # -- End function
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
