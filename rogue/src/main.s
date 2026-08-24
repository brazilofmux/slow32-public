	.file	"main.c"
	.text
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi sp, sp, -160
	stw sp+0, lr
	stw sp+156, r11
	stw sp+152, r12
	stw sp+148, r13
	stw sp+144, r14
	stw sp+140, r15
	stw sp+136, r16
	stw sp+132, r17
	stw sp+128, r18
	stw sp+124, r19
	stw sp+120, r20
	stw sp+116, r21
	stw sp+112, r22
	stw sp+108, r23
	stw sp+104, r24
	add r13, r3, r0
	addi r20, r0, 0
	addi r3, r0, 2
	addi r17, r0, 1
	add r18, r20, r0
	add r19, r20, r0
	add r1, r20, r0
	add r11, r20, r0
	blt r13, r3, .LBB0_11
.LBB0_1:
	add r12, r4, r0
	addi r21, r0, 0
	lui r14, %hi(.L.str)
	addi r14, r14, %lo(.L.str)
	lui r15, %hi(.L.str.1)
	addi r15, r15, %lo(.L.str.1)
	lui r16, %hi(.L.str.2)
	addi r16, r16, %lo(.L.str.2)
	add r11, r21, r0
	add r23, r17, r0
	add r19, r21, r0
	add r18, r21, r0
	add r22, r17, r0
	jal r0, .LBB0_4
.LBB0_2:
	add r18, r17, r0
.LBB0_3:
	addi r22, r22, 1
	bge r22, r13, .LBB0_10
.LBB0_4:
	slli r1, r22, 2
	add r24, r12, r1
	ldw r3, r24+0
	add r4, r14, r0
	jal r31, strcmp
	beq r1, r21, .LBB0_2
.LBB0_5:
	ldw r3, r24+0
	add r4, r15, r0
	jal r31, strcmp
	beq r1, r21, .LBB0_9
.LBB0_6:
	ldw r3, r24+0
	add r4, r16, r0
	jal r31, strcmp
	bne r1, r21, .LBB0_18
.LBB0_7:
	addi r22, r22, 1
	bge r22, r13, .LBB0_18
.LBB0_8:
	slli r1, r22, 2
	add r1, r12, r1
	ldw r3, r1+0
	jal r31, atoi
	add r11, r1, r0
	add r23, r21, r0
	jal r0, .LBB0_3
.LBB0_9:
	add r19, r17, r0
	jal r0, .LBB0_3
.LBB0_10:
	addi r1, r0, 0
	seq r1, r23, r1
.LBB0_11:
	lui r12, %hi(g+5132)
	addi r12, r12, %lo(g+5132)
	stw r12+0, r18
	lui r15, %hi(g+5128)
	addi r15, r15, %lo(g+5128)
	stw r15+0, r19
	lui r14, %hi(g+5092)
	addi r14, r14, %lo(g+5092)
	lui r13, %hi(stdout)
	addi r13, r13, %lo(stdout)
	bne r1, r20, .LBB0_13
.LBB0_12:
	lui r3, %hi(g)
	addi r3, r3, %lo(g)
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	jal r31, load_game
	addi r3, r0, 0
	beq r1, r3, .LBB0_19
.LBB0_13:
	addi r1, r0, 0
	bne r11, r1, .LBB0_15
.LBB0_14:
	addi r3, r0, 0
	jal r31, time
	lui r3, 648056
	addi r3, r3, -1607
	xor r11, r1, r3
.LBB0_15:
	lui r3, %hi(g)
	addi r3, r3, %lo(g)
	add r4, r11, r0
	jal r31, init_game
	stw r12+0, r18
	stw r15+0, r19
	addi r1, r0, 0
	bne r18, r1, .LBB0_21
.LBB0_16:
	jal r31, term_init
	addi r3, r0, 0
	bne r1, r3, .LBB0_21
.LBB0_17:
	lui r3, %hi(g)
	addi r3, r3, %lo(g)
	jal r31, ui_play
	jal r31, term_cleanup
	jal r0, .LBB0_29
.LBB0_18:
	lui r3, %hi(.L.str.3)
	addi r3, r3, %lo(.L.str.3)
	jal r31, printf
	addi r11, r0, 1
	jal r0, .LBB0_31
.LBB0_19:
	lui r1, %hi(g+5136)
	addi r1, r1, %lo(g+5136)
	stb r1+0, r3
	beq r18, r3, .LBB0_16
.LBB0_20:
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	jal r31, printf
	addi r1, r0, 0
	beq r18, r1, .LBB0_16
.LBB0_21:
	stw r12+0, r17
	lui r11, %hi(g)
	addi r11, r11, %lo(g)
	lui r3, %hi(.L.str.7)
	addi r3, r3, %lo(.L.str.7)
	jal r31, printf
	ldw r4, r13+0
	add r3, r11, r0
	jal r31, line_render
	lui r15, %hi(stdin)
	addi r15, r15, %lo(stdin)
	ldw r5, r15+0
	addi r3, sp, 24
	addi r4, r0, 80
	jal r31, fgets
	addi r16, r0, 0
	beq r1, r16, .LBB0_27
.LBB0_22:
	lui r11, %hi(g)
	addi r11, r11, %lo(g)
	addi r12, r0, 80
	addi r18, r0, 10
	jal r0, .LBB0_24
.LBB0_23:
	ldw r4, r13+0
	add r3, r11, r0
	jal r31, line_render
	ldw r5, r15+0
	addi r3, sp, 24
	add r4, r12, r0
	jal r31, fgets
	beq r1, r16, .LBB0_27
.LBB0_24:
	ldbu r1, sp+24
	beq r1, r16, .LBB0_23
.LBB0_25:
	beq r1, r18, .LBB0_23
.LBB0_26:
	slli r1, r1, 24
	srai r4, r1, 24
	add r3, r11, r0
	jal r31, do_command
	bne r1, r16, .LBB0_23
.LBB0_27:
	ldw r1, r14+0
	bne r1, r16, .LBB0_29
.LBB0_28:
	addi r1, r0, 3
	stw r14+0, r1
.LBB0_29:
	ldw r1, r14+0
	addi r1, r1, -1
	addi r11, r0, 0
	bgtu r1, r17, .LBB0_31
.LBB0_30:
	lui r3, %hi(g)
	addi r3, r3, %lo(g)
	lui r12, %hi(.L.str.6)
	addi r12, r12, %lo(.L.str.6)
	add r4, r12, r0
	jal r31, record_score
	ldw r4, r13+0
	add r3, r12, r0
	jal r31, show_scores
.LBB0_31:
	add r1, r11, r0
	ldw r24, sp+104
	ldw r23, sp+108
	ldw r22, sp+112
	ldw r21, sp+116
	ldw r20, sp+120
	ldw r19, sp+124
	ldw r18, sp+128
	ldw r17, sp+132
	ldw r16, sp+136
	ldw r15, sp+140
	ldw r14, sp+144
	ldw r13, sp+148
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jalr r0, r31, 0
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"--line"
	.size	.L.str, 7

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"--wizard"
	.size	.L.str.1, 9

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"--seed"
	.size	.L.str.2, 7

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"usage: rogue [--line] [--wizard] [--seed N]\n"
	.size	.L.str.3, 45

	.type	g,@object                       # @g
	.local	g
	.comm	g,5336,4
	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"rogue.sav"
	.size	.L.str.4, 10

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"Welcome back to the Dungeons of Doom.\n"
	.size	.L.str.5, 39

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"rogue.scr"
	.size	.L.str.6, 10

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"Hello. Welcome to the Dungeons of Doom.\n"
	.size	.L.str.7, 41

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e507704cf3c4d36284ffcb21f50e8531ceb63f7f)"
	.section	".note.GNU-stack","",@progbits
