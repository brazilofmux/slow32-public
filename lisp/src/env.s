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
	addi r12, fp, -24
	stw r12+0, r4
	addi r13, fp, -28
	stw r13+0, r5
	add r3, r11, r0
	jal r31, push_root_checked
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r13, r0
	jal r31, push_root_checked
	ldw r3, r12+0
	ldw r4, r13+0
	jal r31, cons_alloc
	addi r12, fp, -32
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	ldw r3, r12+0
	ldw r1, r11+0
	ldw r4, r1+12
	jal r31, cons_alloc
	lui r3, %hi(root_sp)
	addi r3, r3, %lo(root_sp)
	ldw r4, r3+0
	addi r4, r4, -4
	stw r3+0, r4
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
	stw fp+-44, lr
	addi r11, fp, -64
	stw r11+0, r3
	addi r12, fp, -68
	stw r12+0, r4
	addi r17, fp, -72
	stw r17+0, r5
	add r3, r11, r0
	jal r31, push_root_checked
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r17, r0
	jal r31, push_root_checked
	ldw r4, r11+0
	addi r13, r0, 0
	add r3, r13, r0
	jal r31, cons_alloc
	addi r11, fp, -76
	stw r11+0, r1
	add r3, r11, r0
	jal r31, push_root_checked
	ldw r19, r12+0
	lui r18, %hi(root_sp)
	addi r18, r18, %lo(root_sp)
	beq r19, r13, .LBB4_7
.LBB4_1:
	andi r1, r19, 1
	bne r1, r13, .LBB4_7
.LBB4_2:
	addi r20, r0, 1
	addi r12, fp, -48
	addi r15, fp, -52
	addi r16, fp, -56
	addi r14, fp, -60
.LBB4_3:
	ldw r17, r17+0
	ldw r1, r19+0
	beq r1, r20, .LBB4_8
.LBB4_4:
	beq r17, r13, .LBB4_11
.LBB4_5:
	ldw r1, r11+0
	ldw r3, r19+12
	ldw r4, r17+12
	stw r12+0, r1
	stw r15+0, r3
	stw r16+0, r4
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r15, r0
	jal r31, push_root_checked
	add r3, r16, r0
	jal r31, push_root_checked
	ldw r3, r15+0
	ldw r4, r16+0
	jal r31, cons_alloc
	stw r14+0, r1
	add r3, r14, r0
	jal r31, push_root_checked
	ldw r3, r14+0
	ldw r1, r12+0
	ldw r4, r1+12
	jal r31, cons_alloc
	ldw r3, r18+0
	addi r3, r3, -4
	stw r18+0, r3
	ldw r3, r12+0
	stw r3+12, r1
	ldw r19, r19+16
	beq r19, r13, .LBB4_7
.LBB4_6:
	addi r17, r17, 16
	andi r1, r19, 1
	beq r1, r13, .LBB4_3
.LBB4_7:
	ldw r1, r18+0
	addi r1, r1, -4
	jal r0, .LBB4_9
.LBB4_8:
	ldw r1, r11+0
	stw r12+0, r1
	stw r15+0, r19
	stw r16+0, r17
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r15, r0
	jal r31, push_root_checked
	add r3, r16, r0
	jal r31, push_root_checked
	ldw r3, r15+0
	ldw r4, r16+0
	jal r31, cons_alloc
	stw r14+0, r1
	add r3, r14, r0
	jal r31, push_root_checked
	ldw r3, r14+0
	ldw r1, r12+0
	ldw r4, r1+12
	jal r31, cons_alloc
	ldw r3, r18+0
	addi r4, r3, -4
	stw r18+0, r4
	ldw r4, r12+0
	stw r4+12, r1
	addi r1, r3, -8
.LBB4_9:
	stw r18+0, r1
	ldw r13, r11+0
.LBB4_10:
	add r1, r13, r0
	ldw lr, fp+-44
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
	ldw r1, r18+0
	addi r1, r1, -4
	stw r18+0, r1
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
