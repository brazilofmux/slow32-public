	.file	"main.c"
	.text
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi sp, sp, -72
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 72
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
	jal r31, heap_init
	jal r31, reader_init
	jal r31, eval_init
	addi r11, r0, 0
	add r3, r11, r0
	jal r31, env_create
	lui r12, %hi(global_env)
	addi r12, r12, %lo(global_env)
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	ldw r3, r12+0
	jal r31, builtins_register
	lui r18, %hi(g_error)
	addi r18, r18, %lo(g_error)
	stw r18+0, r11
	addi r13, fp, -48
	stw r13+0, r11
	add r3, r13, r0
	jal r31, lisp_read
	addi r14, fp, -52
	stw r14+0, r1
	ldw r1, r13+0
	beq r1, r11, .LBB0_2
.LBB0_1:
	addi r1, r0, 0
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
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB0_2:
	lui r19, %hi(root_sp)
	addi r19, r19, %lo(root_sp)
	lui r1, 524288
	addi r20, r1, -1
	addi r15, r0, 10
	lui r16, %hi(.L.str)
	addi r16, r16, %lo(.L.str)
	lui r17, %hi(g_errmsg)
	addi r17, r17, %lo(g_errmsg)
	jal r0, .LBB0_5
.LBB0_3:
	add r3, r16, r0
	add r4, r17, r0
	jal r31, printf
.LBB0_4:
	stw r18+0, r11
	stw r13+0, r11
	add r3, r13, r0
	jal r31, lisp_read
	stw r14+0, r1
	ldw r1, r13+0
	bne r1, r11, .LBB0_1
.LBB0_5:
	ldw r1, r18+0
	bne r1, r11, .LBB0_3
.LBB0_6:
	add r3, r14, r0
	jal r31, push_root_checked
	ldw r3, r14+0
	ldw r4, r12+0
	jal r31, eval
	ldw r3, r19+0
	addi r3, r3, -1
	stw r19+0, r3
	ldw r3, r18+0
	bne r3, r11, .LBB0_3
.LBB0_7:
	beq r1, r20, .LBB0_4
.LBB0_8:
	add r3, r1, r0
	jal r31, lisp_print
	add r3, r15, r0
	jal r31, putchar
	jal r0, .LBB0_4
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Error: %s\n"
	.size	.L.str, 11

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
