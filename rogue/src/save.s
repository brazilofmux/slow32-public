	.file	"save.c"
	.text
	.globl	save_game                       # -- Begin function save_game
	.p2align	2
	.type	save_game,@function
save_game:                              # @save_game
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 48
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	add r12, r4, r0
	add r14, r3, r0
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	add r3, r12, r0
	jal r31, fopen
	addi r11, r0, -1
	addi r16, r0, 0
	beq r1, r16, .LBB0_5
.LBB0_1:
	add r13, r1, r0
	lui r3, %hi(.L.str.1)
	addi r3, r3, %lo(.L.str.1)
	addi r4, r0, 1
	addi r15, r0, 6
	add r5, r15, r0
	add r6, r1, r0
	jal r31, fwrite
	bne r1, r15, .LBB0_4
.LBB0_2:
	lui r1, 1
	addi r15, r1, 1240
	addi r4, r0, 1
	add r3, r14, r0
	add r5, r15, r0
	add r6, r13, r0
	jal r31, fwrite
	bne r1, r15, .LBB0_4
.LBB0_3:
	add r3, r13, r0
	jal r31, fclose
	add r11, r16, r0
	jal r0, .LBB0_5
.LBB0_4:
	add r3, r13, r0
	jal r31, fclose
	add r3, r12, r0
	jal r31, remove
.LBB0_5:
	add r1, r11, r0
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end0:
	.size	save_game, .Lfunc_end0-save_game
                                        # -- End function
	.globl	load_game                       # -- Begin function load_game
	.p2align	2
	.type	load_game,@function
load_game:                              # @load_game
# %bb.0:
	addi sp, sp, -80
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 80
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
	add r12, r4, r0
	add r11, r3, r0
	lui r16, 1
	addi r1, r16, 1036
	add r17, r3, r1
	ldw r18, r17+0
	addi r1, r16, 1032
	add r19, r3, r1
	ldw r20, r19+0
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	add r3, r12, r0
	jal r31, fopen
	addi r13, r0, -1
	addi r21, r0, 0
	beq r1, r21, .LBB1_6
.LBB1_1:
	add r14, r1, r0
	addi r3, fp, -50
	addi r4, r0, 1
	addi r15, r0, 6
	add r5, r15, r0
	add r6, r1, r0
	jal r31, fread
	bne r1, r15, .LBB1_5
.LBB1_2:
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	addi r3, fp, -50
	addi r5, r0, 6
	jal r31, memcmp
	bne r1, r21, .LBB1_5
.LBB1_3:
	addi r15, r16, 1240
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r15, r0
	add r6, r14, r0
	jal r31, fread
	bne r1, r15, .LBB1_5
.LBB1_4:
	add r3, r14, r0
	jal r31, fclose
	add r3, r12, r0
	jal r31, remove
	stw r17+0, r18
	stw r19+0, r20
	addi r1, r16, 996
	add r1, r11, r1
	stw r1+0, r21
	addi r1, r16, 1040
	add r1, r11, r1
	stb r1+0, r21
	add r13, r21, r0
	jal r0, .LBB1_6
.LBB1_5:
	add r3, r14, r0
	jal r31, fclose
.LBB1_6:
	add r1, r13, r0
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
	addi sp, sp, 80
	jalr r0, r31, 0
.Lfunc_end1:
	.size	load_game, .Lfunc_end1-load_game
                                        # -- End function
	.globl	record_score                    # -- Begin function record_score
	.p2align	2
	.type	record_score,@function
record_score:                           # @record_score
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 32
	stw fp+-4, r11
	stw fp+-8, r12
	add r1, r4, r0
	add r12, r3, r0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	add r3, r1, r0
	jal r31, fopen
	add r11, r1, r0
	addi r1, r0, 0
	beq r11, r1, .LBB2_7
.LBB2_1:
	lui r1, 1
	addi r3, r1, 996
	add r3, r12, r3
	ldw r3, r3+0
	addi r4, r1, 544
	add r4, r12, r4
	ldw r5, r4+0
	addi r4, r0, 1
	beq r3, r4, .LBB2_4
.LBB2_2:
	addi r4, r0, 2
	bne r3, r4, .LBB2_5
.LBB2_3:
	addi r1, r1, 536
	add r1, r12, r1
	ldw r6, r1+0
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	jal r0, .LBB2_6
.LBB2_4:
	addi r3, r1, 1000
	add r6, r12, r3
	addi r1, r1, 548
	add r1, r12, r1
	ldw r7, r1+0
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	jal r0, .LBB2_6
.LBB2_5:
	addi r1, r1, 548
	add r1, r12, r1
	ldw r6, r1+0
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
.LBB2_6:
	add r3, r11, r0
	jal r31, fprintf
	add r3, r11, r0
	jal r31, fclose
.LBB2_7:
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end2:
	.size	record_score, .Lfunc_end2-record_score
                                        # -- End function
	.globl	show_scores                     # -- Begin function show_scores
	.p2align	2
	.type	show_scores,@function
show_scores:                            # @show_scores
# %bb.0:
	addi sp, sp, -176
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 176
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	add r11, r4, r0
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	jal r31, fopen
	addi r16, r0, 0
	beq r1, r16, .LBB3_5
.LBB3_1:
	add r12, r1, r0
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	add r3, r11, r0
	jal r31, fprintf
	addi r17, r0, 10
	addi r13, fp, -156
	addi r14, r0, 128
	lui r15, %hi(.L.str.9)
	addi r15, r15, %lo(.L.str.9)
.LBB3_2:
	add r3, r13, r0
	add r4, r14, r0
	add r5, r12, r0
	jal r31, fgets
	beq r1, r16, .LBB3_4
.LBB3_3:
	add r3, r11, r0
	add r4, r15, r0
	add r5, r13, r0
	jal r31, fprintf
	addi r17, r17, -1
	bne r17, r16, .LBB3_2
.LBB3_4:
	add r3, r12, r0
	jal r31, fclose
.LBB3_5:
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 176
	jalr r0, r31, 0
.Lfunc_end3:
	.size	show_scores, .Lfunc_end3-show_scores
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"wb"
	.size	.L.str, 3

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"RGUE1\n"
	.size	.L.str.1, 7

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"rb"
	.size	.L.str.2, 3

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"a"
	.size	.L.str.3, 2

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"%d gold, escaped with the Amulet (level %d)\n"
	.size	.L.str.4, 45

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"%d gold, killed by %s on dungeon level %d\n"
	.size	.L.str.5, 43

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"%d gold, quit on dungeon level %d\n"
	.size	.L.str.6, 35

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"r"
	.size	.L.str.7, 2

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"\nTop Rogueists:\n"
	.size	.L.str.8, 17

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"  %s"
	.size	.L.str.9, 5

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e34f541beea69553ff1fd655361b4faa1e656dc2)"
	.section	".note.GNU-stack","",@progbits
