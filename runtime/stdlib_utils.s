	.file	"stdlib_utils.c"
	.text
	.globl	exit                            # -- Begin function exit
	.p2align	2
	.type	exit,@function
exit:                                   # @exit
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
	add r11, r3, r0
	lui r12, 65536
	ldw r14, r12+0
	addi r1, r14, 1
	andi r13, r1, 255
	addi r15, r12, 4
	ldw r1, r15+0
	beq r13, r1, .LBB0_1
.LBB0_1:
	jal r31, yield
	ldw r1, r15+0
	beq r13, r1, .LBB0_1
.LBB0_2:
	slli r1, r14, 4
	lui r2, 65537
	add r1, r1, r2
	addi r2, r0, 9
	stw r1+0, r2
	ori  r2, r1, 4
	addi r3, r0, 0
	stw r2+0, r3
	ori  r2, r1, 8
	stw r2+0, r3
	ori  r1, r1, 12
	stw r1+0, r11
	stw r12+0, r13
.LBB0_3:
	jal r31, yield
	jal r0, .LBB0_3
.Lfunc_end0:
	.size	exit, .Lfunc_end0-exit
                                        # -- End function
	.section	.text.unlikely.,"ax",@progbits
	.globl	abort                           # -- Begin function abort
	.p2align	2
	.type	abort,@function
abort:                                  # @abort
# %bb.0:
	addi sp, sp, -8
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 8
	addi r3, r0, 1
	jal r31, exit
.Lfunc_end1:
	.size	abort, .Lfunc_end1-abort
                                        # -- End function
	.text
	.globl	abs                             # -- Begin function abs
	.p2align	2
	.type	abs,@function
abs:                                    # @abs
# %bb.0:
	srai r1, r3, 31
	xor r2, r3, r1
	sub r1, r2, r1
	jalr r0, r31, 0
.Lfunc_end2:
	.size	abs, .Lfunc_end2-abs
                                        # -- End function
	.globl	labs                            # -- Begin function labs
	.p2align	2
	.type	labs,@function
labs:                                   # @labs
# %bb.0:
	srai r1, r3, 31
	xor r2, r3, r1
	sub r1, r2, r1
	jalr r0, r31, 0
.Lfunc_end3:
	.size	labs, .Lfunc_end3-labs
                                        # -- End function
	.globl	atoi                            # -- Begin function atoi
	.p2align	2
	.type	atoi,@function
atoi:                                   # @atoi
# %bb.0:
	addi r2, r0, 5
	addi r1, r0, 0
	addi r4, r0, 32
.LBB4_1:
	ldbu r5, r3+0
	addi r6, r5, -9
	sltu r6, r6, r2
	beq r6, r1, .LBB4_3
.LBB4_2:
	addi r3, r3, 1
	jal r0, .LBB4_1
.LBB4_3:
	beq r5, r4, .LBB4_2
	beq r5, r4, .LBB4_2
.LBB4_4:
	addi r2, r0, 43
	beq r5, r2, .LBB4_7
.LBB4_5:
	addi r2, r0, 45
	beq r5, r2, .LBB4_6
	jal r0, .LBB4_8
.LBB4_6:
	addi r3, r3, 1
	addi r2, r0, -1
	jal r0, .LBB4_9
.LBB4_7:
	addi r3, r3, 1
	addi r2, r0, 1
	jal r0, .LBB4_9
.LBB4_8:
	addi r2, r0, 1
.LBB4_9:
	ldbu r4, r3+0
	addi r5, r4, -48
	andi r5, r5, 255
	addi r6, r0, 9
	sgtu r5, r5, r6
	beq r5, r1, .LBB4_10
	jal r0, .LBB4_12
.LBB4_10:
	addi r3, r3, 1
	addi r5, r0, 0
	addi r6, r0, 10
	add r1, r5, r0
.LBB4_11:
	mul r1, r1, r6
	addi r4, r4, -48
	andi r4, r4, 255
	add r1, r1, r4
	ldbu r4, r3+0
	addi r7, r4, -48
	andi r7, r7, 255
	addi r3, r3, 1
	sltu r7, r7, r6
	bne r7, r5, .LBB4_11
.LBB4_12:
	mul r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end4:
	.size	atoi, .Lfunc_end4-atoi
                                        # -- End function
	.globl	atol                            # -- Begin function atol
	.p2align	2
	.type	atol,@function
atol:                                   # @atol
# %bb.0:
	addi r2, r0, 5
	addi r1, r0, 0
	addi r4, r0, 32
.LBB5_1:
	ldbu r5, r3+0
	addi r6, r5, -9
	sltu r6, r6, r2
	beq r6, r1, .LBB5_3
.LBB5_2:
	addi r3, r3, 1
	jal r0, .LBB5_1
.LBB5_3:
	beq r5, r4, .LBB5_2
	beq r5, r4, .LBB5_2
.LBB5_4:
	addi r2, r0, 43
	beq r5, r2, .LBB5_7
.LBB5_5:
	addi r2, r0, 45
	beq r5, r2, .LBB5_6
	jal r0, .LBB5_8
.LBB5_6:
	addi r3, r3, 1
	addi r2, r0, -1
	jal r0, .LBB5_9
.LBB5_7:
	addi r3, r3, 1
	addi r2, r0, 1
	jal r0, .LBB5_9
.LBB5_8:
	addi r2, r0, 1
.LBB5_9:
	ldbu r4, r3+0
	addi r5, r4, -48
	andi r5, r5, 255
	addi r6, r0, 9
	sgtu r5, r5, r6
	beq r5, r1, .LBB5_10
	jal r0, .LBB5_12
.LBB5_10:
	addi r3, r3, 1
	addi r5, r0, 0
	addi r6, r0, 10
	add r1, r5, r0
.LBB5_11:
	mul r1, r1, r6
	addi r4, r4, -48
	andi r4, r4, 255
	add r1, r1, r4
	ldbu r4, r3+0
	addi r7, r4, -48
	andi r7, r7, 255
	addi r3, r3, 1
	sltu r7, r7, r6
	bne r7, r5, .LBB5_11
.LBB5_12:
	mul r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end5:
	.size	atol, .Lfunc_end5-atol
                                        # -- End function
	.globl	rand                            # -- Begin function rand
	.p2align	2
	.type	rand,@function
rand:                                   # @rand
# %bb.0:
	ori  r1, r0, %lo(rand_seed)
	lui r2, %hi(rand_seed)
	add r1, r2, r1
	ldw r2, r1+0
	lui r3, 269413
	addi r3, r3, 3693
	mul r2, r2, r3
	addi r2, r2, 12345
	stw r1+0, r2
	srli r1, r2, 16
	jalr r0, r31, 0
.Lfunc_end6:
	.size	rand, .Lfunc_end6-rand
                                        # -- End function
	.globl	srand                           # -- Begin function srand
	.p2align	2
	.type	srand,@function
srand:                                  # @srand
# %bb.0:
	ori  r1, r0, %lo(rand_seed)
	lui r2, %hi(rand_seed)
	add r1, r2, r1
	stw r1+0, r3
	jalr r0, r31, 0
.Lfunc_end7:
	.size	srand, .Lfunc_end7-srand
                                        # -- End function
	.globl	getenv                          # -- Begin function getenv
	.p2align	2
	.type	getenv,@function
getenv:                                 # @getenv
# %bb.0:
	addi r1, r0, 0
	jalr r0, r31, 0
.Lfunc_end8:
	.size	getenv, .Lfunc_end8-getenv
                                        # -- End function
	.type	rand_seed,@object               # @rand_seed
	.data
	.p2align	2, 0x0
rand_seed:
	.word	1                               # 0x1
	.size	rand_seed, 4

	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
