	.file	"env.c"
	.text
	.globl	env_create                      # -- Begin function env_create
	.p2align	2
	.type	env_create,@function
env_create:                             # @env_create
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	add r4, r3, r0
	addi r3, r0, 0
	jal r31, cons_alloc
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	env_create, .Lfunc_end0-env_create
                                        # -- End function
	.globl	env_lookup                      # -- Begin function env_lookup
	.p2align	2
	.type	env_lookup,@function
env_lookup:                             # @env_lookup
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	addi r11, r0, 0
                                        # implicit-def: $r1
	jal r0, .LBB1_3
.LBB1_1:
	ldw r3, r3+16
.LBB1_2:
	bne r5, r11, .LBB1_10
.LBB1_3:
	beq r3, r11, .LBB1_9
.LBB1_4:
	ldw r7, r3+12
	jal r0, .LBB1_6
.LBB1_5:
	ldw r1, r7+16
	add r7, r5, r0
	beq r6, r4, .LBB1_2
.LBB1_6:
	add r5, r7, r0
	beq r7, r11, .LBB1_1
.LBB1_7:
	ldw r7, r5+12
	ldw r6, r7+12
	beq r6, r4, .LBB1_5
.LBB1_8:
	ldw r7, r5+16
	bne r6, r4, .LBB1_6
	jal r0, .LBB1_2
.LBB1_9:
	ldw r4, r4+12
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	jal r31, lisp_error2
	add r1, r11, r0
.LBB1_10:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	env_lookup, .Lfunc_end1-env_lookup
                                        # -- End function
	.globl	env_define                      # -- Begin function env_define
	.p2align	2
	.type	env_define,@function
env_define:                             # @env_define
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, lr
	addi r11, fp, -20
	stw r11+0, r3
	addi r1, fp, -24
	stw r1+0, r4
	addi r3, fp, -28
	stw r3+0, r5
	lui r12, %hi(root_sp)
	addi r12, r12, %lo(root_sp)
	ldw r6, r12+0
	addi r7, r6, 1
	stw r12+0, r7
	slli r8, r6, 2
	lui r13, %hi(root_stack)
	addi r13, r13, %lo(root_stack)
	add r8, r8, r13
	stw r8+0, r11
	addi r8, r6, 2
	stw r12+0, r8
	slli r7, r7, 2
	add r7, r7, r13
	stw r7+0, r1
	addi r1, r6, 3
	stw r12+0, r1
	slli r1, r8, 2
	add r1, r1, r13
	stw r1+0, r3
	add r3, r4, r0
	add r4, r5, r0
	jal r31, cons_alloc
	addi r3, fp, -32
	stw r3+0, r1
	ldw r4, r12+0
	addi r5, r4, 1
	stw r12+0, r5
	slli r4, r4, 2
	add r4, r4, r13
	stw r4+0, r3
	ldw r3, r11+0
	ldw r4, r3+12
	add r3, r1, r0
	jal r31, cons_alloc
	ldw r3, r12+0
	addi r3, r3, -4
	stw r12+0, r3
	ldw r3, r11+0
	stw r3+12, r1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end2:
	.size	env_define, .Lfunc_end2-env_define
                                        # -- End function
	.globl	env_set                         # -- Begin function env_set
	.p2align	2
	.type	env_set,@function
env_set:                                # @env_set
# %bb.0:
	addi r1, r0, 0
	addi r6, r0, 1
	jal r0, .LBB3_3
.LBB3_1:
	ldw r3, r3+16
.LBB3_2:
	bne r7, r1, .LBB3_9
.LBB3_3:
	beq r3, r1, .LBB3_10
.LBB3_4:
	ldw r9, r3+12
	jal r0, .LBB3_6
.LBB3_5:
	stw r9+16, r5
	add r9, r7, r0
	beq r8, r4, .LBB3_2
.LBB3_6:
	add r7, r9, r0
	beq r9, r1, .LBB3_1
.LBB3_7:
	ldw r9, r7+12
	ldw r8, r9+12
	beq r8, r4, .LBB3_5
.LBB3_8:
	ldw r9, r7+16
	bne r8, r4, .LBB3_6
	jal r0, .LBB3_2
.LBB3_9:
	add r1, r6, r0
.LBB3_10:
	jalr r0, r31, 0
.Lfunc_end3:
	.size	env_set, .Lfunc_end3-env_set
                                        # -- End function
	.globl	env_extend                      # -- Begin function env_extend
	.p2align	2
	.type	env_extend,@function
env_extend:                             # @env_extend
# %bb.0:
	addi sp, sp, -88
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 88
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, r19
	stw fp+-40, r20
	stw fp+-44, r21
	stw fp+-48, lr
	add r1, r3, r0
	addi r3, fp, -68
	stw r3+0, r1
	addi r12, fp, -72
	stw r12+0, r4
	addi r13, fp, -76
	stw r13+0, r5
	lui r14, %hi(root_sp)
	addi r14, r14, %lo(root_sp)
	ldw r4, r14+0
	addi r5, r4, 1
	stw r14+0, r5
	slli r6, r4, 2
	lui r16, %hi(root_stack)
	addi r16, r16, %lo(root_stack)
	add r6, r6, r16
	stw r6+0, r3
	addi r3, r4, 2
	stw r14+0, r3
	slli r5, r5, 2
	add r5, r5, r16
	stw r5+0, r12
	addi r4, r4, 3
	stw r14+0, r4
	slli r3, r3, 2
	add r3, r3, r16
	stw r3+0, r13
	addi r11, r0, 0
	add r3, r11, r0
	add r4, r1, r0
	jal r31, cons_alloc
	addi r15, fp, -80
	stw r15+0, r1
	ldw r1, r14+0
	addi r3, r1, 1
	stw r14+0, r3
	slli r1, r1, 2
	add r1, r1, r16
	stw r1+0, r15
	ldw r12, r12+0
	beq r12, r11, .LBB4_7
.LBB4_1:
	andi r1, r12, 1
	bne r1, r11, .LBB4_7
.LBB4_2:
	addi r21, r0, 1
	addi r17, fp, -52
	addi r20, fp, -56
	addi r19, fp, -60
	addi r18, fp, -64
.LBB4_3:
	ldw r13, r13+0
	ldw r1, r12+0
	beq r1, r21, .LBB4_8
.LBB4_4:
	beq r13, r11, .LBB4_11
.LBB4_5:
	ldw r1, r15+0
	ldw r3, r12+12
	ldw r4, r13+12
	stw r17+0, r1
	stw r20+0, r3
	stw r19+0, r4
	ldw r1, r14+0
	addi r5, r1, 1
	stw r14+0, r5
	slli r6, r1, 2
	add r6, r6, r16
	stw r6+0, r17
	addi r6, r1, 2
	stw r14+0, r6
	slli r5, r5, 2
	add r5, r5, r16
	stw r5+0, r20
	addi r1, r1, 3
	stw r14+0, r1
	slli r1, r6, 2
	add r1, r1, r16
	stw r1+0, r19
	jal r31, cons_alloc
	stw r18+0, r1
	ldw r3, r14+0
	addi r4, r3, 1
	stw r14+0, r4
	slli r3, r3, 2
	add r3, r3, r16
	stw r3+0, r18
	ldw r3, r17+0
	ldw r4, r3+12
	add r3, r1, r0
	jal r31, cons_alloc
	ldw r3, r14+0
	addi r3, r3, -4
	stw r14+0, r3
	ldw r3, r17+0
	stw r3+12, r1
	ldw r12, r12+16
	beq r12, r11, .LBB4_7
.LBB4_6:
	addi r13, r13, 16
	andi r1, r12, 1
	beq r1, r11, .LBB4_3
.LBB4_7:
	ldw r1, r14+0
	addi r1, r1, -4
	jal r0, .LBB4_9
.LBB4_8:
	ldw r1, r15+0
	stw r17+0, r1
	stw r20+0, r12
	stw r19+0, r13
	ldw r1, r14+0
	addi r3, r1, 1
	stw r14+0, r3
	slli r4, r1, 2
	add r4, r4, r16
	stw r4+0, r17
	addi r4, r1, 2
	stw r14+0, r4
	slli r3, r3, 2
	add r3, r3, r16
	stw r3+0, r20
	addi r1, r1, 3
	stw r14+0, r1
	slli r1, r4, 2
	add r1, r1, r16
	stw r1+0, r19
	add r3, r12, r0
	add r4, r13, r0
	jal r31, cons_alloc
	stw r18+0, r1
	ldw r3, r14+0
	addi r4, r3, 1
	stw r14+0, r4
	slli r3, r3, 2
	add r3, r3, r16
	stw r3+0, r18
	ldw r3, r17+0
	ldw r4, r3+12
	add r3, r1, r0
	jal r31, cons_alloc
	ldw r3, r14+0
	addi r4, r3, -4
	stw r14+0, r4
	ldw r4, r17+0
	stw r4+12, r1
	addi r1, r3, -8
.LBB4_9:
	stw r14+0, r1
	ldw r11, r15+0
.LBB4_10:
	add r1, r11, r0
	ldw lr, fp+-48
	ldw r21, fp+-44
	ldw r20, fp+-40
	ldw r19, fp+-36
	ldw r18, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 88
	jalr r0, r31, 0
.LBB4_11:
	lui r3, %hi(.L.str.1)
	addi r3, r3, %lo(.L.str.1)
	jal r31, lisp_error
	ldw r1, r14+0
	addi r1, r1, -4
	stw r14+0, r1
	jal r0, .LBB4_10
.Lfunc_end4:
	.size	env_extend, .Lfunc_end4-env_extend
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"unbound variable"
	.size	.L.str, 17

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"too few arguments"
	.size	.L.str.1, 18

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
