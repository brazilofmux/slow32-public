	.file	"game.c"
	.text
	.globl	rnd                             # -- Begin function rnd
	.p2align	2
	.type	rnd,@function
rnd:                                    # @rnd
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 16
	ldw r1, r3+0
	slli r5, r1, 13
	xor r1, r5, r1
	srli r5, r1, 17
	xor r1, r5, r1
	slli r5, r1, 5
	xor r1, r5, r1
	stw r3+0, r1
	addi r3, r0, 1
	blt r4, r3, .LBB0_2
.LBB0_1:
	srli r3, r1, 8
	jal r31, __umodsi3
	jal r0, .LBB0_3
.LBB0_2:
	addi r1, r0, 0
.LBB0_3:
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end0:
	.size	rnd, .Lfunc_end0-rnd
                                        # -- End function
	.globl	roll                            # -- Begin function roll
	.p2align	2
	.type	roll,@function
roll:                                   # @roll
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
	stw fp+-28, r17
	addi r15, r0, 1
	blt r4, r15, .LBB1_6
.LBB1_1:
	add r11, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	ldw r17, r3+0
	addi r16, r0, 0
	add r14, r16, r0
	jal r0, .LBB1_3
.LBB1_2:
	add r14, r1, r14
	addi r13, r13, -1
	beq r13, r16, .LBB1_5
.LBB1_3:
	slli r1, r17, 13
	xor r1, r1, r17
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r17, r3, r1
	add r1, r15, r0
	blt r11, r15, .LBB1_2
.LBB1_4:
	srli r3, r17, 8
	add r4, r11, r0
	jal r31, __umodsi3
	addi r1, r1, 1
	jal r0, .LBB1_2
.LBB1_5:
	stw r12+0, r17
	jal r0, .LBB1_7
.LBB1_6:
	addi r14, r0, 0
.LBB1_7:
	add r1, r14, r0
	ldw r17, fp+-28
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
.Lfunc_end1:
	.size	roll, .Lfunc_end1-roll
                                        # -- End function
	.globl	msgf                            # -- Begin function msgf
	.p2align	2
	.type	msgf,@function
msgf:                                   # @msgf
# %bb.0:
	addi sp, sp, -160
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 160
	stw fp+-28, r11
	stw fp+-32, r12
	stw fp+-36, r13
	stw fp+-40, r14
	add r1, r4, r0
	add r11, r3, r0
	addi lr, fp, -24
	addi r3, lr, 4
	stw r3+0, r6
	stw fp+-4, r10
	stw fp+-8, r9
	stw fp+-12, r8
	stw fp+-16, r7
	stw fp+-24, r5
	stw fp+-44, lr
	addi r3, fp, -144
	addi r4, r0, 100
	add r5, r1, r0
	add r6, lr, r0
	jal r31, vsnprintf
	lui r13, 1
	addi r1, r13, 1040
	add r12, r11, r1
	ldbu r1, r12+0
	addi r14, r0, 0
	beq r1, r14, .LBB2_2
.LBB2_1:
	addi r1, r13, 1140
	add r3, r11, r1
	addi r5, r0, 99
	add r4, r12, r0
	jal r31, strncpy
	addi r1, r13, 1239
	add r1, r11, r1
	stb r1+0, r14
.LBB2_2:
	addi r4, fp, -144
	addi r5, r0, 99
	add r3, r12, r0
	jal r31, strncpy
	addi r1, r13, 1139
	add r1, r11, r1
	stb r1+0, r14
	addi r1, r13, 1036
	add r1, r11, r1
	ldw r1, r1+0
	beq r1, r14, .LBB2_4
.LBB2_3:
	lui r3, %hi(.L.str.15)
	addi r3, r3, %lo(.L.str.15)
	add r4, r12, r0
	jal r31, printf
.LBB2_4:
	ldw r14, fp+-40
	ldw r13, fp+-36
	ldw r12, fp+-32
	ldw r11, fp+-28
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 160
	jalr r0, r31, 0
.Lfunc_end2:
	.size	msgf, .Lfunc_end2-msgf
                                        # -- End function
	.globl	rank_name                       # -- Begin function rank_name
	.p2align	2
	.type	rank_name,@function
rank_name:                              # @rank_name
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 16
	addi r1, r0, 1
	sgt r1, r3, r1
	addi r4, r0, 0
	sub r1, r4, r1
	xori r3, r3, 1
	and r1, r3, r1
	xori r3, r1, 1
	addi r5, r0, 16
	slt r3, r3, r5
	sub r3, r4, r3
	xori r1, r1, 17
	and r1, r1, r3
	xori r1, r1, 16
	slli r1, r1, 2
	lui r3, %hi(rank_name.ranks-4)
	addi r3, r3, %lo(rank_name.ranks-4)
	add r1, r1, r3
	ldw r1, r1+0
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end3:
	.size	rank_name, .Lfunc_end3-rank_name
                                        # -- End function
	.globl	inv_name                        # -- Begin function inv_name
	.p2align	2
	.type	inv_name,@function
inv_name:                               # @inv_name
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 16
	add r1, r6, r0
	add r3, r5, r0
	ldw r5, r4+0
	addi r5, r5, -2
	addi r6, r0, 5
	bgtu r5, r6, .LBB4_9
.LBB4_1:
	slli r5, r5, 2
	lui r6, %hi(.LJTI4_0)
	addi r6, r6, %lo(.LJTI4_0)
	add r5, r6, r5
	ldw r5, r5+0
	jalr r0, r5, 0
.LBB4_2:
	ldw r6, r4+8
	addi r4, r0, 2
	blt r6, r4, .LBB4_10
.LBB4_3:
	lui r5, %hi(.L.str.32)
	addi r5, r5, %lo(.L.str.32)
	jal r0, .LBB4_11
.LBB4_4:
	ldw r6, r4+12
	ldw r4, r4+4
	slli r4, r4, 3
	lui r5, %hi(armor_table)
	addi r5, r5, %lo(armor_table)
	add r4, r4, r5
	ldw r7, r4+0
	lui r5, %hi(.L.str.37)
	addi r5, r5, %lo(.L.str.37)
	jal r0, .LBB4_11
.LBB4_5:
	ldw r4, r4+4
	slli r4, r4, 2
	lui r5, %hi(scroll_names)
	addi r5, r5, %lo(scroll_names)
	add r4, r4, r5
	ldw r6, r4+0
	lui r5, %hi(.L.str.35)
	addi r5, r5, %lo(.L.str.35)
	jal r0, .LBB4_11
.LBB4_6:
	ldw r6, r4+12
	ldw r4, r4+4
	addi r5, r0, 12
	mul r4, r4, r5
	lui r5, %hi(weapon_table)
	addi r5, r5, %lo(weapon_table)
	add r4, r4, r5
	ldw r7, r4+0
	lui r5, %hi(.L.str.36)
	addi r5, r5, %lo(.L.str.36)
	jal r0, .LBB4_11
.LBB4_7:
	ldw r4, r4+4
	slli r4, r4, 2
	lui r5, %hi(potion_names)
	addi r5, r5, %lo(potion_names)
	add r4, r4, r5
	ldw r6, r4+0
	lui r5, %hi(.L.str.34)
	addi r5, r5, %lo(.L.str.34)
	jal r0, .LBB4_11
.LBB4_8:
	lui r5, %hi(.L.str.38)
	addi r5, r5, %lo(.L.str.38)
	jal r0, .LBB4_11
.LBB4_9:
	lui r5, %hi(.L.str.39)
	addi r5, r5, %lo(.L.str.39)
	jal r0, .LBB4_11
.LBB4_10:
	lui r5, %hi(.L.str.33)
	addi r5, r5, %lo(.L.str.33)
.LBB4_11:
	add r4, r1, r0
	jal r31, snprintf
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end4:
	.size	inv_name, .Lfunc_end4-inv_name
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI4_0,@object
.LJTI4_0:
	.word	.LBB4_2
	.word	.LBB4_7
	.word	.LBB4_5
	.word	.LBB4_6
	.word	.LBB4_4
	.word	.LBB4_8
	.size	.LJTI4_0, 24
                                        # -- End function
	.text
	.globl	init_game                       # -- Begin function init_game
	.p2align	2
	.type	init_game,@function
init_game:                              # @init_game
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 32
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	add r12, r4, r0
	add r13, r3, r0
	lui r14, 1
	addi r5, r14, 1240
	addi r11, r0, 0
	add r4, r11, r0
	jal r31, memset
	seq r1, r12, r11
	sub r1, r11, r1
	lui r4, 21365
	addi r4, r4, 1825
	xor r4, r12, r4
	and r1, r4, r1
	xor r1, r12, r1
	stw r13+0, r1
	addi r1, r14, 524
	add r1, r13, r1
	addi r4, r0, 12
	stw r1+0, r4
	addi r1, r14, 520
	add r1, r13, r1
	stw r1+0, r4
	addi r1, r14, 532
	add r1, r13, r1
	addi r4, r0, 16
	stw r1+0, r4
	addi r1, r14, 528
	add r1, r13, r1
	stw r1+0, r4
	addi r1, r14, 536
	add r4, r13, r1
	addi r1, r0, 1
	stw r4+0, r1
	addi r4, r14, 548
	add r4, r13, r4
	stw r4+0, r1
	addi r4, r14, 552
	add r4, r13, r4
	stw r4+0, r1
	addi r4, r14, 556
	add r4, r13, r4
	addi r5, r0, 1300
	stw r4+0, r5
	addi r4, r14, 560
	add r4, r13, r4
	addi r6, r0, -1
	stw r4+0, r6
	addi r5, r14, 564
	add r5, r13, r5
	stw r5+0, r6
	addi r6, r14, 572
	addi r7, r14, 988
.LBB5_1:
	add r8, r13, r6
	stw r8+0, r11
	addi r6, r6, 16
	bne r6, r7, .LBB5_1
.LBB5_2:
	addi r6, r14, 572
	add r6, r13, r6
	addi r7, r0, 2
	stw r6+0, r7
	addi r6, r14, 580
	add r6, r13, r6
	stw r6+0, r1
	addi r6, r14, 588
	add r6, r13, r6
	addi r8, r0, 5
	stw r6+0, r8
	addi r6, r14, 592
	add r6, r13, r6
	stw r6+0, r1
	addi r6, r14, 596
	add r6, r13, r6
	stw r6+0, r1
	addi r6, r14, 600
	add r6, r13, r6
	stw r6+0, r1
	addi r6, r14, 604
	add r6, r13, r6
	addi r8, r0, 6
	stw r6+0, r8
	addi r6, r14, 608
	add r6, r13, r6
	stw r6+0, r1
	addi r6, r14, 612
	add r6, r13, r6
	stw r6+0, r1
	addi r6, r14, 616
	add r6, r13, r6
	stw r6+0, r11
	stw r4+0, r1
	stw r5+0, r7
	add r3, r13, r0
	jal r31, new_level
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end5:
	.size	init_game, .Lfunc_end5-init_game
                                        # -- End function
	.globl	player_armor                    # -- Begin function player_armor
	.p2align	2
	.type	player_armor,@function
player_armor:                           # @player_armor
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 16
	lui r1, 1
	addi r4, r1, 564
	add r4, r3, r4
	ldw r4, r4+0
	addi r5, r0, 0
	blt r4, r5, .LBB6_2
.LBB6_1:
	slli r4, r4, 4
	add r3, r3, r4
	addi r1, r1, 572
	add r1, r3, r1
	ldw r3, r1+4
	slli r3, r3, 3
	lui r4, %hi(armor_table+4)
	addi r4, r4, %lo(armor_table+4)
	add r3, r3, r4
	ldw r3, r3+0
	ldw r1, r1+12
	add r1, r1, r3
	jal r0, .LBB6_3
.LBB6_2:
	addi r1, r0, 1
.LBB6_3:
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end6:
	.size	player_armor, .Lfunc_end6-player_armor
                                        # -- End function
	.globl	mon_visible                     # -- Begin function mon_visible
	.p2align	2
	.type	mon_visible,@function
mon_visible:                            # @mon_visible
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
	add r11, r4, r0
	add r12, r3, r0
	lui r1, 1
	addi r3, r1, 512
	add r14, r12, r3
	ldw r4, r14+0
	addi r1, r1, 516
	add r15, r12, r1
	ldw r5, r15+0
	add r3, r12, r0
	jal r31, room_at
	add r13, r1, r0
	ldw r4, r11+4
	ldw r5, r11+8
	add r3, r12, r0
	jal r31, room_at
	ldw r3, r11+4
	ldw r4, r14+0
	ldw r5, r11+8
	ldw r6, r15+0
	sub r3, r3, r4
	addi r3, r3, 1
	addi r4, r0, 3
	sltu r3, r3, r4
	sub r5, r5, r6
	addi r5, r5, 1
	sltu r4, r5, r4
	and r3, r3, r4
	addi r4, r0, -1
	sgt r4, r13, r4
	seq r1, r13, r1
	and r1, r4, r1
	or  r1, r3, r1
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end7:
	.size	mon_visible, .Lfunc_end7-mon_visible
                                        # -- End function
	.globl	do_command                      # -- Begin function do_command
	.p2align	2
	.type	do_command,@function
do_command:                             # @do_command
# %bb.0:
	addi sp, sp, -160
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 160
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
	stw fp+-48, r22
	stw fp+-52, r23
	stw fp+-56, r24
	stw fp+-60, r25
	stw fp+-64, r26
	stw fp+-68, r27
	stw fp+-72, r28
	add r11, r3, r0
	lui r17, 1
	addi r1, r17, 1040
	add r1, r3, r1
	addi r12, r0, 0
	stb r1+0, r12
	addi r1, r4, -38
	addi r3, r0, 83
	bgtu r1, r3, .LBB8_93
.LBB8_1:
	slli r1, r1, 2
	lui r3, %hi(.LJTI8_0)
	addi r3, r3, %lo(.LJTI8_0)
	add r1, r3, r1
	ldw r1, r1+0
	add r13, r12, r0
	jalr r0, r1, 0
.LBB8_2:
	addi r1, r17, 1032
	add r1, r11, r1
	ldw r1, r1+0
	addi r13, r0, 1
	addi r3, r0, 0
	beq r1, r3, .LBB8_159
.LBB8_3:
	addi r1, r17, 504
	add r1, r11, r1
	ldw r1, r1+0
	addi r3, r17, 512
	add r3, r11, r3
	stw r3+0, r1
	addi r1, r17, 508
	add r1, r11, r1
	ldw r1, r1+0
	addi r3, r17, 516
	add r3, r11, r3
	stw r3+0, r1
	add r3, r11, r0
	jal r31, mark_seen
	lui r4, %hi(.L.str.54)
	addi r4, r4, %lo(.L.str.54)
	jal r0, .LBB8_122
.LBB8_4:
	addi r13, r0, 0
	addi r5, r0, 1
	jal r0, .LBB8_61
.LBB8_5:
	addi r4, r17, 572
	add r15, r11, r4
	addi r1, r0, 0
	addi r3, r0, 4
	addi r5, r17, 988
.LBB8_6:
	add r6, r11, r4
	ldw r6, r6+0
	seq r6, r6, r3
	add r1, r1, r6
	addi r4, r4, 16
	bne r4, r5, .LBB8_6
.LBB8_7:
	addi r4, r0, 1
	bleu r1, r4, .LBB8_9
.LBB8_8:
	addi r1, r17, 1036
	add r1, r11, r1
	ldw r1, r1+0
	addi r4, r0, 0
	beq r1, r4, .LBB8_95
.LBB8_9:
	addi r1, r0, -1
	addi r5, r0, 0
	addi r6, r0, 26
	add r7, r15, r0
	add r4, r5, r0
	add r8, r5, r0
.LBB8_10:
	ldw r9, r7+0
	seq r9, r9, r3
	xor r10, r8, r1
	sub lr, r5, r9
	and r10, r10, lr
	xor r1, r1, r10
	add r4, r4, r9
	addi r8, r8, 1
	addi r7, r7, 16
	bne r8, r6, .LBB8_10
.LBB8_11:
	addi r3, r0, 0
	bne r4, r3, .LBB8_96
.LBB8_12:
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	lui r5, %hi(.L.str.43)
	addi r5, r5, %lo(.L.str.43)
	add r3, r11, r0
	jal r31, msgf
	addi r1, r0, -1
	jal r0, .LBB8_96
.LBB8_13:
	addi r4, r17, 572
	add r14, r11, r4
	addi r1, r0, 0
	addi r3, r0, 2
	addi r5, r17, 988
.LBB8_14:
	add r6, r11, r4
	ldw r6, r6+0
	seq r6, r6, r3
	add r1, r1, r6
	addi r4, r4, 16
	bne r4, r5, .LBB8_14
.LBB8_15:
	addi r15, r0, 1
	bleu r1, r15, .LBB8_17
.LBB8_16:
	addi r1, r17, 1036
	add r1, r11, r1
	ldw r1, r1+0
	addi r4, r0, 0
	beq r1, r4, .LBB8_100
.LBB8_17:
	addi r1, r0, -1
	addi r5, r0, 0
	addi r6, r0, 26
	add r7, r14, r0
	add r4, r5, r0
	add r8, r5, r0
.LBB8_18:
	ldw r9, r7+0
	seq r9, r9, r3
	xor r10, r8, r1
	sub lr, r5, r9
	and r10, r10, lr
	xor r1, r1, r10
	add r4, r4, r9
	addi r8, r8, 1
	addi r7, r7, 16
	bne r8, r6, .LBB8_18
.LBB8_19:
	addi r3, r0, 0
	bne r4, r3, .LBB8_101
.LBB8_20:
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	lui r5, %hi(.L.str.40)
	addi r5, r5, %lo(.L.str.40)
	add r3, r11, r0
	jal r31, msgf
	addi r1, r0, -1
	jal r0, .LBB8_101
.LBB8_21:
	addi r4, r17, 572
	add r14, r11, r4
	addi r1, r0, 0
	addi r3, r0, 5
	addi r5, r17, 988
.LBB8_22:
	add r6, r11, r4
	ldw r6, r6+0
	seq r6, r6, r3
	add r1, r1, r6
	addi r4, r4, 16
	bne r4, r5, .LBB8_22
.LBB8_23:
	addi r4, r0, 1
	bleu r1, r4, .LBB8_25
.LBB8_24:
	addi r1, r17, 1036
	add r1, r11, r1
	ldw r1, r1+0
	addi r4, r0, 0
	beq r1, r4, .LBB8_105
.LBB8_25:
	addi r1, r0, -1
	addi r5, r0, 0
	addi r6, r0, 26
	add r7, r14, r0
	add r4, r5, r0
	add r8, r5, r0
.LBB8_26:
	ldw r9, r7+0
	seq r9, r9, r3
	xor r10, r8, r1
	sub lr, r5, r9
	and r10, r10, lr
	xor r1, r1, r10
	add r4, r4, r9
	addi r8, r8, 1
	addi r7, r7, 16
	bne r8, r6, .LBB8_26
.LBB8_27:
	addi r3, r0, 0
	bne r4, r3, .LBB8_106
.LBB8_28:
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	lui r5, %hi(.L.str.44)
	addi r5, r5, %lo(.L.str.44)
	add r3, r11, r0
	jal r31, msgf
	addi r1, r0, -1
	jal r0, .LBB8_106
.LBB8_29:
	addi r1, r17, 564
	add r1, r11, r1
	ldw r3, r1+0
	addi r13, r0, 0
	blt r3, r13, .LBB8_91
.LBB8_30:
	addi r3, r0, -1
	stw r1+0, r3
	lui r4, %hi(.L.str.48)
	addi r4, r4, %lo(.L.str.48)
	jal r0, .LBB8_122
.LBB8_31:
	addi r1, r17, 1032
	add r1, r11, r1
	ldw r1, r1+0
	addi r13, r0, 1
	addi r3, r0, 0
	beq r1, r3, .LBB8_159
.LBB8_32:
	addi r1, r17, 524
	add r1, r11, r1
	addi r3, r0, 999
	stw r1+0, r3
	addi r1, r17, 520
	add r1, r11, r1
	stw r1+0, r3
	lui r4, %hi(.L.str.57)
	addi r4, r4, %lo(.L.str.57)
	jal r0, .LBB8_122
.LBB8_33:
	addi r4, r0, -1
	addi r5, r0, 1
	jal r0, .LBB8_35
.LBB8_34:
	addi r4, r0, 1
	addi r5, r0, -1
.LBB8_35:
	add r3, r11, r0
	jal r0, .LBB8_53
.LBB8_36:
	addi r4, r0, 1
	jal r0, .LBB8_52
.LBB8_37:
	addi r1, r17, 512
	add r1, r11, r1
	ldw r1, r1+0
	addi r3, r0, 80
	mul r1, r1, r3
	add r1, r11, r1
	addi r3, r17, 516
	add r3, r11, r3
	ldw r3, r3+0
	add r1, r1, r3
	ldbu r1, r1+4
	addi r3, r0, 37
	bne r1, r3, .LBB8_88
.LBB8_38:
	addi r1, r17, 568
	add r1, r11, r1
	ldw r1, r1+0
	addi r13, r0, 0
	beq r1, r13, .LBB8_108
.LBB8_39:
	addi r1, r17, 548
	add r14, r11, r1
	ldw r1, r14+0
	addi r3, r0, 1
	bne r1, r3, .LBB8_121
.LBB8_40:
	addi r1, r17, 996
	add r1, r11, r1
	addi r3, r0, 2
	stw r1+0, r3
	lui r4, %hi(.L.str.86)
	addi r4, r4, %lo(.L.str.86)
	jal r0, .LBB8_122
.LBB8_41:
	addi r4, r17, 572
	add r14, r11, r4
	addi r1, r0, 0
	addi r3, r0, 6
	addi r5, r17, 988
.LBB8_42:
	add r6, r11, r4
	ldw r6, r6+0
	seq r6, r6, r3
	add r1, r1, r6
	addi r4, r4, 16
	bne r4, r5, .LBB8_42
.LBB8_43:
	addi r4, r0, 1
	bleu r1, r4, .LBB8_45
.LBB8_44:
	addi r1, r17, 1036
	add r1, r11, r1
	ldw r1, r1+0
	addi r4, r0, 0
	beq r1, r4, .LBB8_109
.LBB8_45:
	addi r1, r0, -1
	addi r5, r0, 0
	addi r6, r0, 26
	add r7, r14, r0
	add r4, r5, r0
	add r8, r5, r0
.LBB8_46:
	ldw r9, r7+0
	seq r9, r9, r3
	xor r10, r8, r1
	sub lr, r5, r9
	and r10, r10, lr
	xor r1, r1, r10
	add r4, r4, r9
	addi r8, r8, 1
	addi r7, r7, 16
	bne r8, r6, .LBB8_46
.LBB8_47:
	addi r3, r0, 0
	bne r4, r3, .LBB8_110
.LBB8_48:
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	lui r5, %hi(.L.str.46)
	addi r5, r5, %lo(.L.str.46)
	add r3, r11, r0
	jal r31, msgf
	addi r1, r0, -1
	jal r0, .LBB8_110
.LBB8_49:
	addi r1, r17, 1032
	add r1, r11, r1
	ldw r1, r1+0
	addi r13, r0, 1
	addi r3, r0, 0
	beq r1, r3, .LBB8_159
.LBB8_50:
	add r3, r11, r0
	jal r31, reveal_map
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	jal r0, .LBB8_122
.LBB8_51:
	addi r4, r0, -1
.LBB8_52:
	add r3, r11, r0
	add r5, r4, r0
.LBB8_53:
	jal r31, try_move
	add r13, r12, r0
	jal r0, .LBB8_159
.LBB8_54:
	addi r1, r17, 512
	add r1, r11, r1
	ldw r1, r1+0
	addi r3, r0, 80
	mul r1, r1, r3
	add r1, r11, r1
	addi r3, r17, 516
	add r3, r11, r3
	ldw r3, r3+0
	add r1, r1, r3
	ldbu r1, r1+4
	addi r3, r0, 37
	bne r1, r3, .LBB8_89
.LBB8_55:
	addi r1, r17, 548
	add r13, r11, r1
	ldw r4, r13+0
	addi r1, r4, 1
	stw r13+0, r1
	addi r3, r17, 552
	add r3, r11, r3
	ldw r5, r3+0
	blt r4, r5, .LBB8_57
.LBB8_56:
	stw r3+0, r1
.LBB8_57:
	add r3, r11, r0
	jal r31, new_level
	ldw r5, r13+0
	lui r4, %hi(.L.str.83)
	addi r4, r4, %lo(.L.str.83)
	jal r0, .LBB8_90
.LBB8_58:
	addi r1, r17, 1032
	add r1, r11, r1
	ldw r1, r1+0
	addi r3, r0, 0
	beq r1, r3, .LBB8_93
.LBB8_59:
	addi r1, r17, 568
	add r1, r11, r1
	addi r13, r0, 1
	stw r1+0, r13
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	jal r0, .LBB8_122
.LBB8_60:
	addi r13, r0, 0
	addi r5, r0, -1
.LBB8_61:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, try_move
	jal r0, .LBB8_159
.LBB8_62:
	addi r1, r17, 1036
	add r18, r11, r1
	ldw r1, r18+0
	addi r19, r0, 0
	beq r1, r19, .LBB8_92
.LBB8_63:
	addi r1, r17, 572
	add r13, r11, r1
	addi r1, r17, 560
	add r21, r11, r1
	addi r1, r17, 564
	add r22, r11, r1
	addi r23, r0, 26
	addi r14, fp, -136
	addi r15, r0, 64
	lui r24, %hi(.L.str.90)
	addi r24, r24, %lo(.L.str.90)
	lui r1, %hi(.L.str.89)
	addi r1, r1, %lo(.L.str.89)
	xor r25, r1, r24
	lui r1, %hi(.L.str.91)
	addi r1, r1, %lo(.L.str.91)
	xor r26, r1, r24
	lui r16, %hi(.L.str.88)
	addi r16, r16, %lo(.L.str.88)
	add r20, r19, r0
	add r27, r19, r0
	jal r0, .LBB8_66
.LBB8_64:
	addi r20, r20, 1
.LBB8_65:
	addi r27, r27, 1
	addi r13, r13, 16
	beq r27, r23, .LBB8_83
.LBB8_66:
	ldw r1, r13+0
	beq r1, r19, .LBB8_65
.LBB8_67:
	add r4, r13, r0
	add r5, r14, r0
	add r6, r15, r0
	jal r31, inv_name
	ldw r1, r18+0
	beq r1, r19, .LBB8_64
.LBB8_68:
	addi r4, r27, 97
	ldw r1, r21+0
	seq r1, r27, r1
	sub r1, r19, r1
	and r1, r25, r1
	xor r6, r1, r24
	ldw r1, r22+0
	seq r1, r27, r1
	sub r1, r19, r1
	and r1, r26, r1
	xor r7, r1, r24
	add r3, r16, r0
	add r5, r14, r0
	jal r31, printf
	jal r0, .LBB8_64
.LBB8_69:
	lui r4, %hi(.L.str.50)
	addi r4, r4, %lo(.L.str.50)
	add r3, r11, r0
	jal r31, save_game
	addi r12, r0, 0
	beq r1, r12, .LBB8_94
.LBB8_70:
	lui r4, %hi(.L.str.52)
	addi r4, r4, %lo(.L.str.52)
	add r3, r11, r0
	jal r31, msgf
	jal r0, .LBB8_226
.LBB8_71:
	addi r1, r17, 996
	add r1, r11, r1
	addi r3, r0, 3
	stw r1+0, r3
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	add r3, r11, r0
	jal r31, msgf
	addi r12, r0, 0
	jal r0, .LBB8_226
.LBB8_72:
	addi r4, r0, -1
	jal r0, .LBB8_74
.LBB8_73:
	addi r4, r0, 1
.LBB8_74:
	addi r13, r0, 0
	add r3, r11, r0
	add r5, r13, r0
	jal r31, try_move
	jal r0, .LBB8_159
.LBB8_75:
	addi r3, r17, 572
	add r14, r11, r3
	addi r1, r0, 0
	addi r16, r0, 3
	addi r4, r17, 988
.LBB8_76:
	add r5, r11, r3
	ldw r5, r5+0
	seq r5, r5, r16
	add r1, r1, r5
	addi r3, r3, 16
	bne r3, r4, .LBB8_76
.LBB8_77:
	addi r15, r0, 1
	bleu r1, r15, .LBB8_79
.LBB8_78:
	addi r1, r17, 1036
	add r1, r11, r1
	ldw r1, r1+0
	addi r3, r0, 0
	beq r1, r3, .LBB8_113
.LBB8_79:
	addi r1, r0, -1
	addi r4, r0, 0
	addi r5, r0, 26
	add r6, r14, r0
	add r3, r4, r0
	add r7, r4, r0
.LBB8_80:
	ldw r8, r6+0
	seq r8, r8, r16
	xor r9, r7, r1
	sub r10, r4, r8
	and r9, r9, r10
	xor r1, r1, r9
	add r3, r3, r8
	addi r7, r7, 1
	addi r6, r6, 16
	bne r7, r5, .LBB8_80
.LBB8_81:
	addi r4, r0, 0
	bne r3, r4, .LBB8_114
.LBB8_82:
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	lui r5, %hi(.L.str.42)
	addi r5, r5, %lo(.L.str.42)
	add r3, r11, r0
	jal r31, msgf
	addi r1, r0, -1
	jal r0, .LBB8_114
.LBB8_83:
	ldw r1, r18+0
	addi r13, r0, 0
	beq r1, r13, .LBB8_86
.LBB8_84:
	addi r1, r17, 568
	add r1, r11, r1
	ldw r1, r1+0
	beq r1, r13, .LBB8_86
.LBB8_85:
	lui r3, %hi(.L.str.92)
	addi r3, r3, %lo(.L.str.92)
	jal r31, printf
.LBB8_86:
	bne r20, r13, .LBB8_93
.LBB8_87:
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	add r3, r11, r0
	jal r31, msgf
	jal r0, .LBB8_93
.LBB8_88:
	lui r4, %hi(.L.str.84)
	addi r4, r4, %lo(.L.str.84)
	jal r0, .LBB8_90
.LBB8_89:
	lui r4, %hi(.L.str.82)
	addi r4, r4, %lo(.L.str.82)
.LBB8_90:
	add r3, r11, r0
	jal r31, msgf
	add r13, r12, r0
	jal r0, .LBB8_159
.LBB8_91:
	lui r4, %hi(.L.str.49)
	addi r4, r4, %lo(.L.str.49)
	jal r0, .LBB8_122
.LBB8_92:
	add r3, r11, r0
	jal r31, ui_show_inventory
.LBB8_93:
	addi r13, r0, 1
	jal r0, .LBB8_159
.LBB8_94:
	lui r4, %hi(.L.str.51)
	addi r4, r4, %lo(.L.str.51)
	add r3, r11, r0
	jal r31, msgf
	addi r1, r17, 996
	add r1, r11, r1
	addi r3, r0, 3
	stw r1+0, r3
	jal r0, .LBB8_226
.LBB8_95:
	lui r5, %hi(.L.str.43)
	addi r5, r5, %lo(.L.str.43)
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, ui_pick_slot
.LBB8_96:
	addi r13, r0, 0
	blt r1, r13, .LBB8_159
.LBB8_97:
	slli r1, r1, 4
	add r14, r15, r1
	ldw r1, r14+4
	addi r3, r0, 3
	bgtu r1, r3, .LBB8_158
.LBB8_98:
	slli r1, r1, 2
	lui r3, %hi(.LJTI8_1)
	addi r3, r3, %lo(.LJTI8_1)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB8_99:
	add r3, r11, r0
	jal r31, reveal_map
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
	jal r0, .LBB8_156
.LBB8_100:
	lui r5, %hi(.L.str.40)
	addi r5, r5, %lo(.L.str.40)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, ui_pick_slot
.LBB8_101:
	addi r13, r0, 0
	blt r1, r13, .LBB8_159
.LBB8_102:
	addi r3, r17, 556
	add r3, r11, r3
	addi r4, r0, 1300
	stw r3+0, r4
	slli r1, r1, 4
	add r1, r14, r1
	ldw r3, r1+8
	addi r4, r3, -1
	stw r1+8, r4
	bgt r3, r15, .LBB8_104
.LBB8_103:
	addi r3, r0, 0
	stw r1+0, r3
.LBB8_104:
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	jal r0, .LBB8_122
.LBB8_105:
	lui r5, %hi(.L.str.44)
	addi r5, r5, %lo(.L.str.44)
	addi r4, r0, 5
	add r3, r11, r0
	jal r31, ui_pick_slot
.LBB8_106:
	addi r13, r0, 0
	blt r1, r13, .LBB8_159
.LBB8_107:
	addi r3, r17, 560
	add r3, r11, r3
	stw r3+0, r1
	slli r1, r1, 4
	add r4, r14, r1
	addi r14, fp, -136
	addi r6, r0, 64
	add r5, r14, r0
	jal r31, inv_name
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	jal r0, .LBB8_112
.LBB8_108:
	lui r4, %hi(.L.str.85)
	addi r4, r4, %lo(.L.str.85)
	jal r0, .LBB8_122
.LBB8_109:
	lui r5, %hi(.L.str.46)
	addi r5, r5, %lo(.L.str.46)
	addi r4, r0, 6
	add r3, r11, r0
	jal r31, ui_pick_slot
.LBB8_110:
	addi r13, r0, 0
	blt r1, r13, .LBB8_159
.LBB8_111:
	addi r3, r17, 564
	add r3, r11, r3
	stw r3+0, r1
	slli r1, r1, 4
	add r4, r14, r1
	addi r14, fp, -136
	addi r6, r0, 64
	add r5, r14, r0
	jal r31, inv_name
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
.LBB8_112:
	add r3, r11, r0
	add r5, r14, r0
	jal r31, msgf
	jal r0, .LBB8_159
.LBB8_113:
	lui r5, %hi(.L.str.42)
	addi r5, r5, %lo(.L.str.42)
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, ui_pick_slot
.LBB8_114:
	addi r13, r0, 0
	blt r1, r13, .LBB8_159
.LBB8_115:
	slli r1, r1, 4
	add r14, r14, r1
	ldw r1, r14+4
	bgtu r1, r16, .LBB8_158
.LBB8_116:
	slli r1, r1, 2
	lui r3, %hi(.LJTI8_2)
	addi r3, r3, %lo(.LJTI8_2)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB8_117:
	addi r1, r17, 536
	add r1, r11, r1
	ldw r3, r1+0
	blt r3, r15, .LBB8_148
.LBB8_118:
	ldw r5, r11+0
	addi r4, r0, 0
	add r1, r4, r0
.LBB8_119:
	slli r6, r5, 13
	xor r5, r6, r5
	srli r6, r5, 17
	xor r5, r6, r5
	slli r6, r5, 5
	xor r5, r6, r5
	srli r6, r5, 8
	andi r6, r6, 3
	add r1, r1, r6
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r4, .LBB8_119
.LBB8_120:
	stw r11+0, r5
	jal r0, .LBB8_149
.LBB8_121:
	addi r1, r1, -1
	stw r14+0, r1
	add r3, r11, r0
	jal r31, new_level
	ldw r5, r14+0
	lui r4, %hi(.L.str.87)
	addi r4, r4, %lo(.L.str.87)
.LBB8_122:
	add r3, r11, r0
	jal r31, msgf
	jal r0, .LBB8_159
.LBB8_123:
	addi r1, r17, 560
	add r1, r11, r1
	ldw r1, r1+0
	addi r3, r0, 0
	blt r1, r3, .LBB8_146
.LBB8_124:
	slli r1, r1, 4
	add r1, r15, r1
	ldw r3, r1+12
	addi r3, r3, 1
	stw r1+12, r3
	lui r4, %hi(.L.str.102)
	addi r4, r4, %lo(.L.str.102)
	jal r0, .LBB8_156
.LBB8_125:
	addi r1, r17, 564
	add r1, r11, r1
	ldw r1, r1+0
	addi r3, r0, 0
	blt r1, r3, .LBB8_147
.LBB8_126:
	slli r1, r1, 4
	add r1, r15, r1
	ldw r3, r1+12
	addi r3, r3, 1
	stw r1+12, r3
	lui r4, %hi(.L.str.104)
	addi r4, r4, %lo(.L.str.104)
	jal r0, .LBB8_156
.LBB8_127:
	addi r1, r11, 4
	ldw r13, r11+0
	addi r3, r0, 0
	lui r4, 47663
	addi r4, r4, -1861
	addi r5, r0, 22
	lui r6, 13107
	addi r6, r6, 820
	addi r7, r0, 80
	addi r8, r0, 46
	addi r9, r17, -392
	addi r10, r17, -388
	addi lr, r17, -384
	addi r15, r0, 320
	addi r16, r0, 1000
	add r18, r3, r0
	jal r0, .LBB8_129
.LBB8_128:
	addi r18, r18, 1
	beq r18, r16, .LBB8_241
.LBB8_129:
	slli r19, r13, 13
	xor r13, r19, r13
	srli r19, r13, 17
	xor r13, r19, r13
	slli r19, r13, 5
	xor r13, r19, r13
	srli r19, r13, 8
	mulhu r20, r19, r4
	mul r20, r20, r5
	sub r19, r19, r20
	slli r20, r13, 13
	xor r13, r20, r13
	srli r20, r13, 17
	xor r13, r20, r13
	slli r20, r13, 5
	xor r13, r20, r13
	srli r20, r13, 8
	mulhu r21, r20, r6
	mul r21, r21, r7
	sub r20, r20, r21
	mul r21, r19, r7
	add r21, r1, r21
	add r21, r21, r20
	ldbu r21, r21+0
	bne r21, r8, .LBB8_128
.LBB8_130:
	add r21, r3, r0
	jal r0, .LBB8_132
.LBB8_131:
	addi r21, r21, 20
	beq r21, r15, .LBB8_144
.LBB8_132:
	add r22, r11, r21
	add r23, r22, r9
	ldw r23, r23+0
	blt r23, r3, .LBB8_131
.LBB8_133:
	add r23, r22, r10
	ldw r23, r23+0
	bne r23, r19, .LBB8_131
.LBB8_134:
	add r22, r22, lr
	ldw r22, r22+0
	bne r22, r20, .LBB8_131
	jal r0, .LBB8_128
.LBB8_135:
	addi r1, r17, 528
	add r3, r11, r1
	ldw r4, r3+0
	addi r1, r4, 1
	stw r3+0, r1
	addi r3, r17, 532
	add r3, r11, r3
	ldw r5, r3+0
	blt r4, r5, .LBB8_137
.LBB8_136:
	stw r3+0, r1
.LBB8_137:
	lui r4, %hi(.L.str.97)
	addi r4, r4, %lo(.L.str.97)
	jal r0, .LBB8_156
.LBB8_138:
	ldw r1, r11+0
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	addi r3, r0, -1
	xor r3, r1, r3
	srli r3, r3, 8
	addi r4, r0, -8
	or  r3, r3, r4
	stw r11+0, r1
	addi r1, r17, 520
	add r13, r11, r1
	ldw r1, r13+0
	add r1, r1, r3
	stw r13+0, r1
	lui r4, %hi(.L.str.98)
	addi r4, r4, %lo(.L.str.98)
	add r3, r11, r0
	jal r31, msgf
	ldw r1, r13+0
	addi r16, r0, 0
	bgt r1, r16, .LBB8_158
.LBB8_139:
	addi r1, r17, 996
	add r1, r11, r1
	stw r1+0, r15
	addi r1, r17, 1000
	add r3, r11, r1
	lui r13, %hi(.L.str.61)
	addi r13, r13, %lo(.L.str.61)
	addi r5, r0, 31
	add r4, r13, r0
	jal r31, strncpy
	addi r1, r17, 1031
	add r1, r11, r1
	stb r1+0, r16
	lui r4, %hi(.L.str.99)
	addi r4, r4, %lo(.L.str.99)
	add r3, r11, r0
	add r5, r13, r0
	jal r0, .LBB8_157
.LBB8_140:
	addi r1, r17, 536
	add r1, r11, r1
	ldw r3, r1+0
	blt r3, r15, .LBB8_152
.LBB8_141:
	ldw r5, r11+0
	addi r4, r0, 0
	add r1, r4, r0
.LBB8_142:
	slli r6, r5, 13
	xor r5, r6, r5
	srli r6, r5, 17
	xor r5, r6, r5
	slli r6, r5, 5
	xor r5, r6, r5
	srli r6, r5, 8
	andi r6, r6, 7
	add r1, r1, r6
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r4, .LBB8_142
.LBB8_143:
	stw r11+0, r5
	jal r0, .LBB8_153
.LBB8_144:
	stw r11+0, r13
	addi r1, r17, 512
	add r1, r11, r1
	stw r1+0, r19
	addi r1, r17, 516
	add r1, r11, r1
	stw r1+0, r20
	add r3, r11, r0
	jal r31, mark_seen
.LBB8_145:
	lui r4, %hi(.L.str.101)
	addi r4, r4, %lo(.L.str.101)
	jal r0, .LBB8_156
.LBB8_146:
	lui r4, %hi(.L.str.103)
	addi r4, r4, %lo(.L.str.103)
	jal r0, .LBB8_156
.LBB8_147:
	lui r4, %hi(.L.str.105)
	addi r4, r4, %lo(.L.str.105)
	jal r0, .LBB8_156
.LBB8_148:
	addi r1, r0, 0
.LBB8_149:
	addi r3, r17, 520
	add r3, r11, r3
	ldw r4, r3+0
	add r5, r4, r1
	stw r3+0, r5
	addi r1, r17, 524
	add r1, r11, r1
	ldw r4, r1+0
	ble r5, r4, .LBB8_151
.LBB8_150:
	addi r4, r4, 1
	stw r1+0, r4
	stw r3+0, r4
.LBB8_151:
	lui r4, %hi(.L.str.95)
	addi r4, r4, %lo(.L.str.95)
	jal r0, .LBB8_156
.LBB8_152:
	addi r1, r0, 0
.LBB8_153:
	addi r3, r17, 520
	add r3, r11, r3
	ldw r4, r3+0
	add r5, r4, r1
	stw r3+0, r5
	addi r1, r17, 524
	add r1, r11, r1
	ldw r4, r1+0
	ble r5, r4, .LBB8_155
.LBB8_154:
	addi r4, r4, 2
	stw r1+0, r4
	stw r3+0, r4
.LBB8_155:
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
.LBB8_156:
	add r3, r11, r0
.LBB8_157:
	jal r31, msgf
.LBB8_158:
	addi r13, r0, 0
	stw r14+0, r13
.LBB8_159:
	addi r1, r17, 996
	add r14, r11, r1
	ldw r1, r14+0
	bne r1, r12, .LBB8_226
.LBB8_160:
	addi r25, r0, 0
	bne r13, r25, .LBB8_225
.LBB8_161:
	addi r18, r17, -392
	add r19, r11, r18
	addi r1, r17, 512
	add r20, r11, r1
	addi r1, r17, 516
	add r21, r11, r1
	addi r22, r11, 4
	addi r23, r0, 20
	addi r24, r0, 2
	addi r15, r0, 1
	lui r1, 7
	addi r1, r1, -1791
	stw fp+-140, r1
	lui r26, 349525
	addi r1, r26, 1366
	stw fp+-144, r1
	addi r28, r0, -1
	add r16, r25, r0
	jal r0, .LBB8_164
.LBB8_162:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, monster_attacks
.LBB8_163:
	addi r16, r16, 1
	addi r1, r0, 16
	beq r16, r1, .LBB8_224
.LBB8_164:
	ldw r1, r14+0
	bne r1, r25, .LBB8_224
.LBB8_165:
	mul r1, r16, r23
	add r12, r19, r1
	ldw r27, r12+0
	blt r27, r25, .LBB8_163
.LBB8_166:
	ldw r1, r12+16
	beq r1, r25, .LBB8_171
.LBB8_167:
	ldw r4, r20+0
	ldw r5, r21+0
	add r3, r11, r0
	jal r31, room_at
	add r13, r1, r0
	ldw r4, r12+4
	ldw r5, r12+8
	add r3, r11, r0
	jal r31, room_at
	ldw r3, r12+4
	ldw r4, r20+0
	ldw r5, r12+8
	ldw r6, r21+0
	sub r3, r3, r4
	addi r3, r3, 1
	sgtu r3, r3, r24
	sub r4, r5, r6
	addi r4, r4, 1
	sgtu r4, r4, r24
	or  r4, r3, r4
	addi r3, r0, 0
	bne r4, r15, .LBB8_169
.LBB8_168:
	slt r4, r13, r3
	sne r1, r13, r1
	or  r1, r4, r1
	bne r1, r3, .LBB8_163
.LBB8_169:
	ldw r1, r11+0
	slli r4, r1, 13
	xor r1, r4, r1
	srli r4, r1, 17
	xor r1, r4, r1
	slli r4, r1, 5
	xor r1, r4, r1
	stw r11+0, r1
	srli r1, r1, 8
	lui r4, 699051
	addi r4, r4, -1365
	mul r1, r1, r4
	addi r4, r26, 1365
	bgtu r1, r4, .LBB8_163
.LBB8_170:
	stw r12+16, r3
	jal r0, .LBB8_163
.LBB8_171:
	ldw r1, r12+4
	ldw r4, r20+0
	bne r1, r4, .LBB8_173
.LBB8_172:
	ldw r3, r12+8
	ldw r5, r21+0
	beq r3, r5, .LBB8_163
.LBB8_173:
	sub r1, r1, r4
	addi r1, r1, 1
	bgtu r1, r24, .LBB8_175
.LBB8_174:
	ldw r1, r12+8
	ldw r3, r21+0
	sub r1, r1, r3
	addi r1, r1, 1
	bleu r1, r24, .LBB8_162
.LBB8_175:
	ldw r5, r21+0
	add r3, r11, r0
	jal r31, room_at
	add r13, r1, r0
	ldw r4, r12+4
	ldw r5, r12+8
	add r3, r11, r0
	jal r31, room_at
	ldw r3, r12+4
	ldw r4, r20+0
	ldw r6, r12+8
	ldw r5, r21+0
	sub r3, r3, r4
	addi r3, r3, 1
	sgtu r3, r3, r24
	sub r6, r6, r5
	addi r6, r6, 1
	sgtu r6, r6, r24
	or  r3, r3, r6
	bne r3, r15, .LBB8_178
.LBB8_176:
	slt r3, r13, r25
	sne r1, r13, r1
	or  r1, r3, r1
	beq r1, r25, .LBB8_178
.LBB8_177:
	sll r1, r15, r27
	ldw r3, fp+-140
	and r1, r1, r3
	bne r1, r25, .LBB8_163
.LBB8_178:
	bne r27, r25, .LBB8_180
.LBB8_179:
	ldw r1, r11+0
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	stw r11+0, r1
	andi r3, r1, 256
	beq r3, r25, .LBB8_183
.LBB8_180:
	add r3, r11, r0
	jal r31, room_at
	add r13, r1, r0
	ldw r4, r12+4
	ldw r5, r12+8
	add r3, r11, r0
	jal r31, room_at
	ldw r4, r12+4
	ldw r5, r20+0
	ldw r3, r12+8
	ldw r6, r21+0
	sub r7, r4, r5
	addi r7, r7, 1
	sgtu r7, r7, r24
	sub r8, r3, r6
	addi r8, r8, 1
	sgtu r8, r8, r24
	or  r7, r7, r8
	bne r7, r15, .LBB8_184
.LBB8_181:
	addi r7, r0, 0
	slt r8, r13, r7
	sne r1, r13, r1
	or  r1, r8, r1
	beq r1, r7, .LBB8_184
.LBB8_182:
	ldw r1, r11+0
.LBB8_183:
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	srli r3, r1, 8
	ldw r6, fp+-144
	mulhu r4, r3, r6
	slli r5, r4, 1
	add r4, r5, r4
	xor r4, r4, r28
	add r5, r4, r3
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	stw r11+0, r1
	srli r1, r1, 8
	mulhu r3, r1, r6
	slli r4, r3, 1
	add r3, r4, r3
	xor r3, r3, r28
	add r1, r3, r1
	jal r0, .LBB8_185
.LBB8_184:
	slt r1, r5, r4
	sgt r4, r5, r4
	sub r5, r4, r1
	slt r1, r6, r3
	sgt r3, r6, r3
	sub r1, r3, r1
.LBB8_185:
	ldw r4, r12+4
	add r6, r4, r5
	ldw r7, r12+8
	add r3, r7, r1
	addi r8, r0, 21
	bgtu r6, r8, .LBB8_194
.LBB8_186:
	addi r9, r0, 79
	bgtu r3, r9, .LBB8_194
.LBB8_187:
	addi r9, r0, 80
	mul r9, r6, r9
	add r9, r22, r9
	add r9, r9, r3
	ldbu r9, r9+0
	addi r9, r9, -35
	andi r9, r9, 255
	addi r10, r0, 11
	bgtu r9, r10, .LBB8_194
.LBB8_188:
	addi r10, r17, -1787
	srl r9, r10, r9
	andi r10, r9, 1
	addi r9, r0, 0
	beq r10, r9, .LBB8_194
.LBB8_189:
	add r10, r9, r0
	jal r0, .LBB8_191
.LBB8_190:
	addi r10, r10, 20
	addi lr, r0, 320
	beq r10, lr, .LBB8_204
.LBB8_191:
	add lr, r11, r10
	add r13, lr, r18
	ldw r13, r13+0
	blt r13, r9, .LBB8_190
.LBB8_192:
	addi r13, r17, -388
	add r13, lr, r13
	ldw r13, r13+0
	bne r13, r6, .LBB8_190
.LBB8_193:
	addi r13, r17, -384
	add lr, lr, r13
	ldw lr, lr+0
	bne lr, r3, .LBB8_190
.LBB8_194:
	bgtu r6, r8, .LBB8_208
.LBB8_195:
	addi r9, r0, 79
	bgtu r7, r9, .LBB8_208
.LBB8_196:
	addi r9, r0, 80
	mul r9, r6, r9
	add r9, r22, r9
	add r9, r9, r7
	ldbu r9, r9+0
	addi r9, r9, -35
	andi r9, r9, 255
	addi r10, r0, 11
	bgtu r9, r10, .LBB8_208
.LBB8_197:
	addi r10, r17, -1787
	srl r9, r10, r9
	andi r10, r9, 1
	addi r9, r0, 0
	beq r10, r9, .LBB8_208
.LBB8_198:
	add r10, r9, r0
	jal r0, .LBB8_200
.LBB8_199:
	addi r10, r10, 20
	addi lr, r0, 320
	beq r10, lr, .LBB8_205
.LBB8_200:
	add lr, r11, r10
	add r13, lr, r18
	ldw r13, r13+0
	blt r13, r9, .LBB8_199
.LBB8_201:
	addi r13, r17, -388
	add r13, lr, r13
	ldw r13, r13+0
	bne r13, r6, .LBB8_199
.LBB8_202:
	addi r13, r17, -384
	add lr, lr, r13
	ldw lr, lr+0
	bne lr, r7, .LBB8_199
.LBB8_203:
	addi r9, r0, 0
	jal r0, .LBB8_206
.LBB8_204:
	add r7, r3, r0
	jal r0, .LBB8_221
.LBB8_205:
	add r9, r15, r0
.LBB8_206:
	addi r10, r0, 0
	beq r5, r10, .LBB8_208
.LBB8_207:
	bne r9, r10, .LBB8_221
.LBB8_208:
	bgtu r4, r8, .LBB8_163
.LBB8_209:
	addi r5, r0, 79
	bgtu r3, r5, .LBB8_163
.LBB8_210:
	addi r5, r0, 80
	mul r5, r4, r5
	add r5, r22, r5
	add r5, r5, r3
	ldbu r5, r5+0
	addi r5, r5, -35
	andi r5, r5, 255
	addi r6, r0, 11
	bgtu r5, r6, .LBB8_163
.LBB8_211:
	addi r6, r17, -1787
	srl r5, r6, r5
	andi r6, r5, 1
	addi r5, r0, 0
	beq r6, r5, .LBB8_163
.LBB8_212:
	add r6, r5, r0
	jal r0, .LBB8_214
.LBB8_213:
	addi r6, r6, 20
	addi r7, r0, 320
	beq r6, r7, .LBB8_218
.LBB8_214:
	add r7, r11, r6
	add r8, r7, r18
	ldw r8, r8+0
	blt r8, r5, .LBB8_213
.LBB8_215:
	addi r8, r17, -388
	add r8, r7, r8
	ldw r8, r8+0
	bne r8, r4, .LBB8_213
.LBB8_216:
	addi r8, r17, -384
	add r7, r7, r8
	ldw r7, r7+0
	bne r7, r3, .LBB8_213
.LBB8_217:
	addi r5, r0, 0
	jal r0, .LBB8_219
.LBB8_218:
	addi r5, r0, 1
.LBB8_219:
	addi r8, r0, 0
	beq r1, r8, .LBB8_163
.LBB8_220:
	add r6, r4, r0
	add r7, r3, r0
	beq r5, r8, .LBB8_163
.LBB8_221:
	ldw r1, r20+0
	bne r6, r1, .LBB8_223
.LBB8_222:
	ldw r1, r21+0
	beq r7, r1, .LBB8_162
.LBB8_223:
	stw r12+4, r6
	stw r12+8, r7
	jal r0, .LBB8_163
.LBB8_224:
	ldw r1, r14+0
	addi r13, r0, 0
	beq r1, r13, .LBB8_227
.LBB8_225:
	ldw r1, r14+0
	addi r3, r0, 0
	seq r12, r1, r3
.LBB8_226:
	add r1, r12, r0
	ldw r28, fp+-72
	ldw r27, fp+-68
	ldw r26, fp+-64
	ldw r25, fp+-60
	ldw r24, fp+-56
	ldw r23, fp+-52
	ldw r22, fp+-48
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
	addi sp, sp, 160
	jalr r0, r31, 0
.LBB8_227:
	addi r1, r17, 988
	add r1, r11, r1
	ldw r3, r1+0
	addi r3, r3, 1
	stw r1+0, r3
	addi r1, r17, 556
	add r12, r11, r1
	ldw r1, r12+0
	blt r1, r15, .LBB8_232
.LBB8_228:
	addi r1, r1, -1
	stw r12+0, r1
	beq r1, r13, .LBB8_235
.LBB8_229:
	addi r3, r0, 300
	beq r1, r3, .LBB8_234
.LBB8_230:
	addi r3, r0, 150
	bne r1, r3, .LBB8_237
.LBB8_231:
	lui r4, %hi(.L.str.110)
	addi r4, r4, %lo(.L.str.110)
	jal r0, .LBB8_236
.LBB8_232:
	addi r1, r17, 520
	add r1, r11, r1
	ldw r3, r1+0
	addi r4, r3, -1
	stw r1+0, r4
	bgt r3, r15, .LBB8_237
.LBB8_233:
	stw r14+0, r15
	addi r1, r17, 1000
	add r3, r11, r1
	lui r12, %hi(.L.str.112)
	addi r12, r12, %lo(.L.str.112)
	addi r5, r0, 31
	add r4, r12, r0
	jal r31, strncpy
	addi r1, r17, 1031
	add r1, r11, r1
	stb r1+0, r13
	lui r4, %hi(.L.str.99)
	addi r4, r4, %lo(.L.str.99)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, msgf
	jal r0, .LBB8_225
.LBB8_234:
	lui r4, %hi(.L.str.109)
	addi r4, r4, %lo(.L.str.109)
	jal r0, .LBB8_236
.LBB8_235:
	lui r4, %hi(.L.str.111)
	addi r4, r4, %lo(.L.str.111)
.LBB8_236:
	add r3, r11, r0
	jal r31, msgf
.LBB8_237:
	addi r1, r17, 520
	add r1, r11, r1
	ldw r3, r1+0
	addi r4, r17, 524
	add r4, r11, r4
	ldw r4, r4+0
	bge r3, r4, .LBB8_225
.LBB8_238:
	ldw r4, r12+0
	blt r4, r15, .LBB8_225
.LBB8_239:
	addi r4, r17, 536
	add r4, r11, r4
	ldw r4, r4+0
	slli r4, r4, 1
	addi r5, r0, 21
	sub r4, r5, r4
	addi r5, r0, 3
	sgt r5, r4, r5
	sub r5, r13, r5
	xori r4, r4, 3
	and r4, r4, r5
	xori r5, r4, 3
	addi r4, r17, 992
	add r4, r11, r4
	ldw r6, r4+0
	addi r6, r6, 1
	stw r4+0, r6
	blt r6, r5, .LBB8_225
.LBB8_240:
	stw r4+0, r13
	addi r3, r3, 1
	stw r1+0, r3
	jal r0, .LBB8_225
.LBB8_241:
	stw r11+0, r13
	jal r0, .LBB8_145
.Lfunc_end8:
	.size	do_command, .Lfunc_end8-do_command
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI8_0,@object
.LJTI8_0:
	.word	.LBB8_2
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_49
	.word	.LBB8_31
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_159
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_37
	.word	.LBB8_93
	.word	.LBB8_54
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_58
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_71
	.word	.LBB8_93
	.word	.LBB8_69
	.word	.LBB8_29
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_41
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_34
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_13
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_60
	.word	.LBB8_62
	.word	.LBB8_73
	.word	.LBB8_72
	.word	.LBB8_4
	.word	.LBB8_93
	.word	.LBB8_36
	.word	.LBB8_93
	.word	.LBB8_93
	.word	.LBB8_75
	.word	.LBB8_5
	.word	.LBB8_159
	.word	.LBB8_93
	.word	.LBB8_33
	.word	.LBB8_93
	.word	.LBB8_21
	.word	.LBB8_93
	.word	.LBB8_51
	.size	.LJTI8_0, 336
	.type	.LJTI8_1,@object
.LJTI8_1:
	.word	.LBB8_99
	.word	.LBB8_127
	.word	.LBB8_123
	.word	.LBB8_125
	.size	.LJTI8_1, 16
	.type	.LJTI8_2,@object
.LJTI8_2:
	.word	.LBB8_117
	.word	.LBB8_140
	.word	.LBB8_135
	.word	.LBB8_138
	.size	.LJTI8_2, 16
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function try_move
	.type	try_move,@function
try_move:                               # @try_move
# %bb.0:
	addi sp, sp, -144
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 144
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
	stw fp+-48, r22
	stw fp+-52, r23
	stw fp+-56, r24
	add r11, r3, r0
	lui r13, 1
	addi r1, r13, 512
	add r12, r3, r1
	ldw r6, r12+0
	add r1, r6, r4
	addi r3, r13, 516
	add r16, r11, r3
	ldw r7, r16+0
	add r3, r7, r5
	addi r8, r13, -392
	add r8, r11, r8
	addi r9, r13, -384
	add r10, r11, r9
	addi r15, r0, 0
	addi lr, r0, -1
	addi r14, r0, 16
	add r9, r15, r0
	jal r0, .LBB9_2
.LBB9_1:
	addi r9, r9, 1
	addi r10, r10, 20
	beq r9, r14, .LBB9_8
.LBB9_2:
	ldw r17, r10+-8
	blt r17, r15, .LBB9_1
.LBB9_3:
	ldw r17, r10+-4
	bne r17, r1, .LBB9_1
.LBB9_4:
	ldw r17, r10+0
	bne r17, r3, .LBB9_1
.LBB9_5:
	addi r17, r0, 0
	blt r9, r17, .LBB9_9
.LBB9_6:
	addi r18, r0, 20
	mul r1, r9, r18
	add r16, r8, r1
	ldw r1, r16+0
	addi r3, r0, 40
	mul r1, r1, r3
	lui r3, %hi(mon_table)
	addi r3, r3, %lo(mon_table)
	lui r4, %hi(mon_table+20)
	addi r4, r4, %lo(mon_table+20)
	add r4, r1, r4
	ldw r4, r4+0
	addi r5, r13, 536
	add r15, r11, r5
	ldw r5, r15+0
	addi r6, r13, 528
	add r6, r11, r6
	ldw r19, r6+0
	ble r19, r18, .LBB9_26
.LBB9_7:
	addi r6, r0, -3
	jal r0, .LBB9_31
.LBB9_8:
	add r9, lr, r0
	addi r17, r0, 0
	bge r9, r17, .LBB9_6
.LBB9_9:
	addi r8, r0, 21
	bgtu r1, r8, .LBB9_47
.LBB9_10:
	addi r8, r0, 79
	bgtu r3, r8, .LBB9_47
.LBB9_11:
	addi r9, r0, 80
	mul r8, r1, r9
	add r10, r11, r8
	add r14, r10, r3
	ldbu r10, r14+4
	addi r10, r10, -35
	addi lr, r0, 11
	bgtu r10, lr, .LBB9_47
.LBB9_12:
	addi r18, r0, 1
	sll r10, r18, r10
	andi r10, r10, 2309
	beq r10, r17, .LBB9_47
.LBB9_13:
	beq r4, r17, .LBB9_17
.LBB9_14:
	addi r4, r0, 0
	beq r5, r4, .LBB9_17
.LBB9_15:
	addi r4, r11, 4
	mul r5, r6, r9
	add r5, r4, r5
	add r5, r5, r7
	ldbu r6, r5+0
	addi r5, r0, 43
	beq r6, r5, .LBB9_47
.LBB9_16:
	add r4, r4, r8
	add r4, r4, r3
	ldbu r4, r4+0
	beq r4, r5, .LBB9_47
.LBB9_17:
	stw r12+0, r1
	stw r16+0, r3
	add r3, r11, r0
	jal r31, mark_seen
	addi r4, r13, -72
	addi r5, r0, 576
	addi r6, r13, -56
	addi r7, r13, -52
	add r8, r17, r0
	jal r0, .LBB9_19
.LBB9_18:
	addi r8, r8, 24
	beq r8, r5, .LBB9_67
.LBB9_19:
	add r1, r11, r8
	add r15, r1, r4
	ldw r3, r15+0
	beq r3, r17, .LBB9_18
.LBB9_20:
	add r9, r1, r6
	ldw r9, r9+0
	ldw r10, r12+0
	bne r9, r10, .LBB9_18
.LBB9_21:
	add r9, r1, r7
	ldw r9, r9+0
	ldw r10, r16+0
	bne r9, r10, .LBB9_18
.LBB9_22:
	addi r4, r0, 7
	beq r3, r4, .LBB9_59
.LBB9_23:
	addi r5, r0, 2
	beq r3, r5, .LBB9_52
.LBB9_24:
	bne r3, r18, .LBB9_55
.LBB9_25:
	addi r3, r13, -64
	add r1, r1, r3
	ldw r5, r1+0
	addi r1, r13, 544
	add r1, r11, r1
	ldw r3, r1+0
	add r3, r3, r5
	stw r1+0, r3
	lui r4, %hi(.L.str.79)
	addi r4, r4, %lo(.L.str.79)
	jal r0, .LBB9_60
.LBB9_26:
	addi r6, r0, 18
	ble r19, r6, .LBB9_28
.LBB9_27:
	addi r6, r0, -2
	jal r0, .LBB9_31
.LBB9_28:
	ble r19, r14, .LBB9_30
.LBB9_29:
	addi r6, r0, -1
	jal r0, .LBB9_31
.LBB9_30:
	addi r6, r0, 7
	slt r6, r19, r6
.LBB9_31:
	add r17, r1, r3
	sub r1, r4, r5
	add r1, r1, r6
	addi r1, r1, 10
	addi r20, r0, 0
	stw r16+16, r20
	ldw r3, r11+0
	slli r4, r3, 13
	xor r3, r4, r3
	srli r4, r3, 17
	xor r3, r4, r3
	slli r4, r3, 5
	xor r21, r4, r3
	stw r11+0, r21
	srli r3, r21, 8
	lui r4, 52429
	addi r4, r4, -819
	mulhu r4, r3, r4
	mul r4, r4, r18
	sub r3, r3, r4
	addi r3, r3, 1
	bge r3, r1, .LBB9_33
.LBB9_32:
	ldw r5, r17+0
	lui r4, %hi(.L.str.77)
	addi r4, r4, %lo(.L.str.77)
	jal r0, .LBB9_46
.LBB9_33:
	addi r1, r13, 560
	add r1, r11, r1
	ldw r1, r1+0
	blt r1, r20, .LBB9_38
.LBB9_34:
	slli r1, r1, 4
	add r1, r11, r1
	addi r3, r13, 572
	add r22, r1, r3
	ldw r1, r22+4
	addi r3, r0, 12
	mul r1, r1, r3
	lui r3, %hi(weapon_table+4)
	addi r3, r3, %lo(weapon_table+4)
	add r3, r1, r3
	ldw r23, r3+0
	lui r3, %hi(weapon_table+8)
	addi r3, r3, %lo(weapon_table+8)
	add r1, r1, r3
	ldw r12, r1+0
	add r24, r20, r0
.LBB9_35:
	slli r1, r21, 13
	xor r1, r1, r21
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r21, r3, r1
	srli r3, r21, 8
	add r4, r12, r0
	jal r31, __umodsi3
	add r1, r24, r1
	addi r24, r1, 1
	addi r23, r23, -1
	bne r23, r20, .LBB9_35
.LBB9_36:
	stw r11+0, r21
	ldw r1, r22+12
	add r1, r1, r24
	ble r19, r18, .LBB9_39
.LBB9_37:
	addi r3, r0, 3
	jal r0, .LBB9_44
.LBB9_38:
	slli r1, r21, 13
	xor r1, r1, r21
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r3, r3, r1
	srli r1, r3, 8
	andi r1, r1, 3
	addi r1, r1, 1
	stw r11+0, r3
	bgt r19, r18, .LBB9_37
.LBB9_39:
	addi r3, r0, 18
	ble r19, r3, .LBB9_41
.LBB9_40:
	addi r3, r0, 2
	jal r0, .LBB9_44
.LBB9_41:
	ble r19, r14, .LBB9_43
.LBB9_42:
	addi r3, r0, 1
	jal r0, .LBB9_44
.LBB9_43:
	addi r3, r0, 7
	slt r3, r19, r3
	sub r3, r20, r3
.LBB9_44:
	add r1, r3, r1
	addi r3, r0, 1
	sgt r3, r1, r3
	sub r3, r20, r3
	xori r1, r1, 1
	and r1, r1, r3
	xori r1, r1, 1
	ldw r3, r16+12
	sub r1, r3, r1
	stw r16+12, r1
	ldw r5, r17+0
	ble r1, r20, .LBB9_48
.LBB9_45:
	lui r4, %hi(.L.str.76)
	addi r4, r4, %lo(.L.str.76)
.LBB9_46:
	add r3, r11, r0
	jal r31, msgf
.LBB9_47:
	ldw r24, fp+-56
	ldw r23, fp+-52
	ldw r22, fp+-48
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
	addi sp, sp, 144
	jalr r0, r31, 0
.LBB9_48:
	lui r4, %hi(.L.str.75)
	addi r4, r4, %lo(.L.str.75)
	add r3, r11, r0
	jal r31, msgf
	ldw r1, r17+32
	addi r3, r13, 540
	add r17, r11, r3
	ldw r3, r17+0
	add r1, r3, r1
	stw r17+0, r1
	addi r1, r0, -1
	stw r16+0, r1
	ldw r1, r15+0
	addi r3, r0, 15
	bgt r1, r3, .LBB9_47
.LBB9_49:
	addi r3, r13, 524
	add r16, r11, r3
	addi r3, r13, 520
	add r13, r11, r3
	lui r18, %hi(level_exp)
	addi r18, r18, %lo(level_exp)
	lui r3, 104858
	addi r19, r3, -1638
	addi r20, r0, 10
	lui r12, %hi(.L.str.78)
	addi r12, r12, %lo(.L.str.78)
.LBB9_50:
	ldw r3, r17+0
	slli r4, r1, 2
	add r4, r4, r18
	ldw r4, r4+0
	blt r3, r4, .LBB9_47
.LBB9_51:
	addi r5, r1, 1
	stw r15+0, r5
	ldw r1, r11+0
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	srli r3, r1, 8
	mulhu r4, r3, r19
	mul r4, r4, r20
	sub r3, r3, r4
	stw r11+0, r1
	ldw r1, r16+0
	add r1, r1, r3
	addi r1, r1, 1
	stw r16+0, r1
	stw r13+0, r1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, msgf
	ldw r1, r15+0
	blt r1, r14, .LBB9_50
	jal r0, .LBB9_47
.LBB9_52:
	addi r4, r13, 580
	add r4, r11, r4
	addi r16, r0, 0
	addi r6, r0, 26
.LBB9_53:
	ldw r7, r4+-8
	beq r7, r5, .LBB9_62
.LBB9_54:
	addi r16, r16, 1
	addi r4, r4, 16
	bne r16, r6, .LBB9_53
.LBB9_55:
	addi r4, r13, 584
	add r4, r11, r4
	addi r6, r0, 0
	addi r5, r0, -1
	addi r7, r0, 26
	add r16, r6, r0
.LBB9_56:
	ldw r8, r4+-12
	beq r8, r6, .LBB9_61
.LBB9_57:
	addi r16, r16, 1
	addi r4, r4, 16
	bne r16, r7, .LBB9_56
.LBB9_58:
	add r16, r5, r0
	jal r0, .LBB9_64
.LBB9_59:
	addi r1, r13, 568
	add r1, r11, r1
	stw r1+0, r18
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
.LBB9_60:
	add r3, r11, r0
	jal r0, .LBB9_66
.LBB9_61:
	stw r4+-12, r3
	addi r3, r13, -68
	add r3, r1, r3
	ldw r3, r3+0
	stw r4+-8, r3
	addi r3, r13, -64
	add r3, r1, r3
	ldw r3, r3+0
	stw r4+-4, r3
	addi r3, r13, -60
	add r1, r1, r3
	ldw r1, r1+0
	jal r0, .LBB9_63
.LBB9_62:
	addi r3, r13, -64
	add r1, r1, r3
	ldw r1, r1+0
	ldw r3, r4+0
	add r1, r3, r1
.LBB9_63:
	stw r4+0, r1
.LBB9_64:
	addi r1, r0, -1
	ble r16, r1, .LBB9_70
.LBB9_65:
	slli r1, r16, 4
	add r1, r11, r1
	addi r3, r13, 572
	add r4, r1, r3
	addi r12, fp, -120
	addi r6, r0, 64
	add r5, r12, r0
	jal r31, inv_name
	addi r6, r16, 97
	lui r4, %hi(.L.str.81)
	addi r4, r4, %lo(.L.str.81)
	add r3, r11, r0
	add r5, r12, r0
.LBB9_66:
	jal r31, msgf
	addi r1, r0, 0
	stw r15+0, r1
.LBB9_67:
	ldbu r1, r14+4
	addi r3, r0, 37
	bne r1, r3, .LBB9_47
.LBB9_68:
	addi r1, r13, 1036
	add r1, r11, r1
	ldw r1, r1+0
	addi r3, r0, 0
	bne r1, r3, .LBB9_47
.LBB9_69:
	lui r4, %hi(.L.str.74)
	addi r4, r4, %lo(.L.str.74)
	jal r0, .LBB9_46
.LBB9_70:
	lui r4, %hi(.L.str.80)
	addi r4, r4, %lo(.L.str.80)
	add r3, r11, r0
	jal r31, msgf
	jal r0, .LBB9_67
.Lfunc_end9:
	.size	try_move, .Lfunc_end9-try_move
                                        # -- End function
	.p2align	2                               # -- Begin function monster_attacks
	.type	monster_attacks,@function
monster_attacks:                        # @monster_attacks
# %bb.0:
	addi sp, sp, -96
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 96
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	add r11, r3, r0
	ldw r1, r4+0
	addi r3, r0, 40
	mul r1, r1, r3
	lui r3, %hi(mon_table)
	addi r3, r3, %lo(mon_table)
	add r13, r1, r3
	lui r14, 1
	addi r1, r14, 564
	add r1, r11, r1
	ldw r1, r1+0
	addi r15, r0, 0
	blt r1, r15, .LBB10_2
.LBB10_1:
	slli r1, r1, 4
	add r1, r11, r1
	addi r3, r14, 572
	add r1, r1, r3
	ldw r3, r1+4
	slli r3, r3, 3
	lui r4, %hi(armor_table+4)
	addi r4, r4, %lo(armor_table+4)
	add r3, r3, r4
	ldw r3, r3+0
	ldw r1, r1+12
	add r1, r3, r1
	addi r1, r1, 8
	jal r0, .LBB10_3
.LBB10_2:
	addi r1, r0, 9
.LBB10_3:
	ldw r3, r13+16
	sub r1, r1, r3
	ldw r3, r11+0
	slli r4, r3, 13
	xor r3, r4, r3
	srli r4, r3, 17
	xor r3, r4, r3
	slli r4, r3, 5
	xor r16, r4, r3
	stw r11+0, r16
	srli r3, r16, 8
	lui r4, 52429
	addi r4, r4, -819
	mulhu r4, r3, r4
	addi r5, r0, 20
	mul r4, r4, r5
	sub r3, r3, r4
	addi r3, r3, 1
	bge r3, r1, .LBB10_5
.LBB10_4:
	ldw r5, r13+0
	lui r4, %hi(.L.str.108)
	addi r4, r4, %lo(.L.str.108)
	add r3, r11, r0
	jal r0, .LBB10_9
.LBB10_5:
	ldw r18, r13+24
	ldw r12, r13+28
	add r17, r15, r0
.LBB10_6:
	slli r1, r16, 13
	xor r1, r1, r16
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r16, r3, r1
	srli r3, r16, 8
	add r4, r12, r0
	jal r31, __umodsi3
	add r1, r17, r1
	addi r17, r1, 1
	addi r18, r18, -1
	bne r18, r15, .LBB10_6
.LBB10_7:
	stw r11+0, r16
	addi r1, r14, 520
	add r15, r11, r1
	ldw r1, r15+0
	sub r1, r1, r17
	stw r15+0, r1
	ldw r12, r13+0
	lui r4, %hi(.L.str.106)
	addi r4, r4, %lo(.L.str.106)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, msgf
	ldw r1, r15+0
	addi r15, r0, 0
	bgt r1, r15, .LBB10_10
.LBB10_8:
	lui r5, %hi(.L.str.107)
	addi r5, r5, %lo(.L.str.107)
	addi r13, fp, -72
	addi r4, r0, 40
	add r3, r13, r0
	add r6, r12, r0
	jal r31, snprintf
	addi r1, r14, 996
	add r1, r11, r1
	addi r3, r0, 1
	stw r1+0, r3
	addi r1, r14, 1000
	add r3, r11, r1
	addi r5, r0, 31
	add r4, r13, r0
	jal r31, strncpy
	addi r1, r14, 1031
	add r1, r11, r1
	stb r1+0, r15
	lui r4, %hi(.L.str.99)
	addi r4, r4, %lo(.L.str.99)
	add r3, r11, r0
	add r5, r13, r0
.LBB10_9:
	jal r31, msgf
.LBB10_10:
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
	addi sp, sp, 96
	jalr r0, r31, 0
.Lfunc_end10:
	.size	monster_attacks, .Lfunc_end10-monster_attacks
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"bat"
	.size	.L.str, 4

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"emu"
	.size	.L.str.1, 4

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"kestrel"
	.size	.L.str.2, 8

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"snake"
	.size	.L.str.3, 6

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"hobgoblin"
	.size	.L.str.4, 10

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"rattlesnake"
	.size	.L.str.5, 12

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"orc"
	.size	.L.str.6, 4

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"zombie"
	.size	.L.str.7, 7

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"centaur"
	.size	.L.str.8, 8

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"quagga"
	.size	.L.str.9, 7

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"troll"
	.size	.L.str.10, 6

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"wraith"
	.size	.L.str.11, 7

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"griffin"
	.size	.L.str.12, 8

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"dragon"
	.size	.L.str.13, 7

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"jabberwock"
	.size	.L.str.14, 11

	.type	mon_table,@object               # @mon_table
	.section	.rodata,"a",@progbits
	.globl	mon_table
	.p2align	2, 0x0
mon_table:
	.word	.L.str
	.byte	66                              # 0x42
	.zero	3
	.word	1                               # 0x1
	.word	8                               # 0x8
	.word	1                               # 0x1
	.word	6                               # 0x6
	.word	1                               # 0x1
	.word	2                               # 0x2
	.word	1                               # 0x1
	.word	0                               # 0x0
	.word	.L.str.1
	.byte	69                              # 0x45
	.zero	3
	.word	1                               # 0x1
	.word	7                               # 0x7
	.word	1                               # 0x1
	.word	3                               # 0x3
	.word	1                               # 0x1
	.word	2                               # 0x2
	.word	2                               # 0x2
	.word	1                               # 0x1
	.word	.L.str.2
	.byte	75                              # 0x4b
	.zero	3
	.word	1                               # 0x1
	.word	6                               # 0x6
	.word	1                               # 0x1
	.word	3                               # 0x3
	.word	1                               # 0x1
	.word	4                               # 0x4
	.word	1                               # 0x1
	.word	1                               # 0x1
	.word	.L.str.3
	.byte	83                              # 0x53
	.zero	3
	.word	1                               # 0x1
	.word	9                               # 0x9
	.word	1                               # 0x1
	.word	5                               # 0x5
	.word	1                               # 0x1
	.word	3                               # 0x3
	.word	2                               # 0x2
	.word	1                               # 0x1
	.word	.L.str.4
	.byte	72                              # 0x48
	.zero	3
	.word	1                               # 0x1
	.word	9                               # 0x9
	.word	1                               # 0x1
	.word	5                               # 0x5
	.word	1                               # 0x1
	.word	8                               # 0x8
	.word	3                               # 0x3
	.word	1                               # 0x1
	.word	.L.str.5
	.byte	82                              # 0x52
	.zero	3
	.word	3                               # 0x3
	.word	12                              # 0xc
	.word	2                               # 0x2
	.word	7                               # 0x7
	.word	1                               # 0x1
	.word	6                               # 0x6
	.word	9                               # 0x9
	.word	1                               # 0x1
	.word	.L.str.6
	.byte	79                              # 0x4f
	.zero	3
	.word	4                               # 0x4
	.word	13                              # 0xd
	.word	1                               # 0x1
	.word	4                               # 0x4
	.word	1                               # 0x1
	.word	8                               # 0x8
	.word	5                               # 0x5
	.word	1                               # 0x1
	.word	.L.str.7
	.byte	90                              # 0x5a
	.zero	3
	.word	7                               # 0x7
	.word	14                              # 0xe
	.word	2                               # 0x2
	.word	2                               # 0x2
	.word	1                               # 0x1
	.word	8                               # 0x8
	.word	7                               # 0x7
	.word	1                               # 0x1
	.word	.L.str.8
	.byte	67                              # 0x43
	.zero	3
	.word	7                               # 0x7
	.word	16                              # 0x10
	.word	4                               # 0x4
	.word	6                               # 0x6
	.word	1                               # 0x1
	.word	6                               # 0x6
	.word	17                              # 0x11
	.word	0                               # 0x0
	.word	.L.str.9
	.byte	81                              # 0x51
	.zero	3
	.word	8                               # 0x8
	.word	17                              # 0x11
	.word	3                               # 0x3
	.word	7                               # 0x7
	.word	1                               # 0x1
	.word	5                               # 0x5
	.word	15                              # 0xf
	.word	1                               # 0x1
	.word	.L.str.10
	.byte	84                              # 0x54
	.zero	3
	.word	13                              # 0xd
	.word	22                              # 0x16
	.word	6                               # 0x6
	.word	6                               # 0x6
	.word	4                               # 0x4
	.word	4                               # 0x4
	.word	120                             # 0x78
	.word	1                               # 0x1
	.word	.L.str.11
	.byte	87                              # 0x57
	.zero	3
	.word	14                              # 0xe
	.word	23                              # 0x17
	.word	5                               # 0x5
	.word	6                               # 0x6
	.word	1                               # 0x1
	.word	6                               # 0x6
	.word	55                              # 0x37
	.word	0                               # 0x0
	.word	.L.str.12
	.byte	71                              # 0x47
	.zero	3
	.word	20                              # 0x14
	.word	26                              # 0x1a
	.word	13                              # 0xd
	.word	8                               # 0x8
	.word	4                               # 0x4
	.word	3                               # 0x3
	.word	2000                            # 0x7d0
	.word	1                               # 0x1
	.word	.L.str.13
	.byte	68                              # 0x44
	.zero	3
	.word	22                              # 0x16
	.word	26                              # 0x1a
	.word	10                              # 0xa
	.word	9                               # 0x9
	.word	3                               # 0x3
	.word	10                              # 0xa
	.word	5000                            # 0x1388
	.word	0                               # 0x0
	.word	.L.str.14
	.byte	74                              # 0x4a
	.zero	3
	.word	24                              # 0x18
	.word	26                              # 0x1a
	.word	15                              # 0xf
	.word	4                               # 0x4
	.word	2                               # 0x2
	.word	12                              # 0xc
	.word	3000                            # 0xbb8
	.word	0                               # 0x0
	.size	mon_table, 600

	.type	mon_table_len,@object           # @mon_table_len
	.globl	mon_table_len
	.p2align	2, 0x0
mon_table_len:
	.word	15                              # 0xf
	.size	mon_table_len, 4

	.type	.L.str.15,@object               # @.str.15
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.15:
	.asciz	"%s\n"
	.size	.L.str.15, 4

	.type	rank_name.ranks,@object         # @rank_name.ranks
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
rank_name.ranks:
	.word	.L.str.16
	.word	.L.str.17
	.word	.L.str.18
	.word	.L.str.19
	.word	.L.str.20
	.word	.L.str.21
	.word	.L.str.22
	.word	.L.str.23
	.word	.L.str.24
	.word	.L.str.25
	.word	.L.str.26
	.word	.L.str.27
	.word	.L.str.28
	.word	.L.str.29
	.word	.L.str.30
	.word	.L.str.31
	.size	rank_name.ranks, 64

	.type	.L.str.16,@object               # @.str.16
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.16:
	.asciz	"Rookie"
	.size	.L.str.16, 7

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"Apprentice"
	.size	.L.str.17, 11

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"Journeyman"
	.size	.L.str.18, 11

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"Adventurer"
	.size	.L.str.19, 11

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"Fighter"
	.size	.L.str.20, 8

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"Warrior"
	.size	.L.str.21, 8

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"Rogue"
	.size	.L.str.22, 6

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"Champion"
	.size	.L.str.23, 9

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"Master Rogue"
	.size	.L.str.24, 13

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"Warlord"
	.size	.L.str.25, 8

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"Hero"
	.size	.L.str.26, 5

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"Guild Master"
	.size	.L.str.27, 13

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"Dragonlord"
	.size	.L.str.28, 11

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"Wizard"
	.size	.L.str.29, 7

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"Rogue Geek"
	.size	.L.str.30, 11

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"Rogue Addict"
	.size	.L.str.31, 13

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"%d rations of food"
	.size	.L.str.32, 19

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"some food"
	.size	.L.str.33, 10

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"a potion of %s"
	.size	.L.str.34, 15

	.type	potion_names,@object            # @potion_names
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
potion_names:
	.word	.L.str.58
	.word	.L.str.59
	.word	.L.str.60
	.word	.L.str.61
	.size	potion_names, 16

	.type	.L.str.35,@object               # @.str.35
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.35:
	.asciz	"a scroll of %s"
	.size	.L.str.35, 15

	.type	scroll_names,@object            # @scroll_names
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
scroll_names:
	.word	.L.str.62
	.word	.L.str.63
	.word	.L.str.64
	.word	.L.str.65
	.size	scroll_names, 16

	.type	.L.str.36,@object               # @.str.36
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.36:
	.asciz	"a %+d %s"
	.size	.L.str.36, 9

	.type	weapon_table,@object            # @weapon_table
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
weapon_table:
	.word	.L.str.66
	.word	1                               # 0x1
	.word	6                               # 0x6
	.word	.L.str.67
	.word	2                               # 0x2
	.word	4                               # 0x4
	.word	.L.str.68
	.word	3                               # 0x3
	.word	4                               # 0x4
	.word	.L.str.69
	.word	4                               # 0x4
	.word	4                               # 0x4
	.size	weapon_table, 48

	.type	.L.str.37,@object               # @.str.37
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.37:
	.asciz	"%+d %s"
	.size	.L.str.37, 7

	.type	armor_table,@object             # @armor_table
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
armor_table:
	.word	.L.str.70
	.word	2                               # 0x2
	.word	.L.str.71
	.word	3                               # 0x3
	.word	.L.str.72
	.word	5                               # 0x5
	.word	.L.str.73
	.word	7                               # 0x7
	.size	armor_table, 32

	.type	.L.str.38,@object               # @.str.38
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.38:
	.asciz	"the Amulet of Yendor"
	.size	.L.str.38, 21

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"something odd"
	.size	.L.str.39, 14

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"eat"
	.size	.L.str.40, 4

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"Yum, that tasted good."
	.size	.L.str.41, 23

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"quaff"
	.size	.L.str.42, 6

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"read"
	.size	.L.str.43, 5

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"wield"
	.size	.L.str.44, 6

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"You are now wielding %s."
	.size	.L.str.45, 25

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"wear"
	.size	.L.str.46, 5

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"You are now wearing %s."
	.size	.L.str.47, 24

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"You take off your armor."
	.size	.L.str.48, 25

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"You are not wearing armor."
	.size	.L.str.49, 27

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"rogue.sav"
	.size	.L.str.50, 10

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"Game saved."
	.size	.L.str.51, 12

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"Cannot write rogue.sav."
	.size	.L.str.52, 24

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"You quit."
	.size	.L.str.53, 10

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"Zap! You stand on the staircase."
	.size	.L.str.54, 33

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"The level lies bare before you."
	.size	.L.str.55, 32

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"You now have the Amulet of Yendor!"
	.size	.L.str.56, 35

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"You feel invincible."
	.size	.L.str.57, 21

	.type	.L.str.58,@object               # @.str.58
.L.str.58:
	.asciz	"healing"
	.size	.L.str.58, 8

	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"extra healing"
	.size	.L.str.59, 14

	.type	.L.str.60,@object               # @.str.60
.L.str.60:
	.asciz	"gain strength"
	.size	.L.str.60, 14

	.type	.L.str.61,@object               # @.str.61
.L.str.61:
	.asciz	"poison"
	.size	.L.str.61, 7

	.type	.L.str.62,@object               # @.str.62
.L.str.62:
	.asciz	"magic mapping"
	.size	.L.str.62, 14

	.type	.L.str.63,@object               # @.str.63
.L.str.63:
	.asciz	"teleportation"
	.size	.L.str.63, 14

	.type	.L.str.64,@object               # @.str.64
.L.str.64:
	.asciz	"enchant weapon"
	.size	.L.str.64, 15

	.type	.L.str.65,@object               # @.str.65
.L.str.65:
	.asciz	"enchant armor"
	.size	.L.str.65, 14

	.type	.L.str.66,@object               # @.str.66
.L.str.66:
	.asciz	"dagger"
	.size	.L.str.66, 7

	.type	.L.str.67,@object               # @.str.67
.L.str.67:
	.asciz	"mace"
	.size	.L.str.67, 5

	.type	.L.str.68,@object               # @.str.68
.L.str.68:
	.asciz	"long sword"
	.size	.L.str.68, 11

	.type	.L.str.69,@object               # @.str.69
.L.str.69:
	.asciz	"two-handed sword"
	.size	.L.str.69, 17

	.type	.L.str.70,@object               # @.str.70
.L.str.70:
	.asciz	"leather armor"
	.size	.L.str.70, 14

	.type	.L.str.71,@object               # @.str.71
.L.str.71:
	.asciz	"ring mail"
	.size	.L.str.71, 10

	.type	.L.str.72,@object               # @.str.72
.L.str.72:
	.asciz	"chain mail"
	.size	.L.str.72, 11

	.type	.L.str.73,@object               # @.str.73
.L.str.73:
	.asciz	"plate mail"
	.size	.L.str.73, 11

	.type	.L.str.74,@object               # @.str.74
.L.str.74:
	.asciz	"There is a staircase here."
	.size	.L.str.74, 27

	.type	.L.str.75,@object               # @.str.75
.L.str.75:
	.asciz	"You have defeated the %s."
	.size	.L.str.75, 26

	.type	.L.str.76,@object               # @.str.76
.L.str.76:
	.asciz	"You hit the %s."
	.size	.L.str.76, 16

	.type	.L.str.77,@object               # @.str.77
.L.str.77:
	.asciz	"You miss the %s."
	.size	.L.str.77, 17

	.type	level_exp,@object               # @level_exp
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
level_exp:
	.word	0                               # 0x0
	.word	10                              # 0xa
	.word	20                              # 0x14
	.word	40                              # 0x28
	.word	80                              # 0x50
	.word	160                             # 0xa0
	.word	320                             # 0x140
	.word	640                             # 0x280
	.word	1300                            # 0x514
	.word	2600                            # 0xa28
	.word	5200                            # 0x1450
	.word	13000                           # 0x32c8
	.word	26000                           # 0x6590
	.word	50000                           # 0xc350
	.word	100000                          # 0x186a0
	.word	200000                          # 0x30d40
	.size	level_exp, 64

	.type	.L.str.78,@object               # @.str.78
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.78:
	.asciz	"Welcome to level %d."
	.size	.L.str.78, 21

	.type	.L.str.79,@object               # @.str.79
.L.str.79:
	.asciz	"You find %d gold pieces."
	.size	.L.str.79, 25

	.type	.L.str.80,@object               # @.str.80
.L.str.80:
	.asciz	"Your pack is full."
	.size	.L.str.80, 19

	.type	.L.str.81,@object               # @.str.81
.L.str.81:
	.asciz	"You now have %s (%c)."
	.size	.L.str.81, 22

	.type	.L.str.82,@object               # @.str.82
.L.str.82:
	.asciz	"I see no way down."
	.size	.L.str.82, 19

	.type	.L.str.83,@object               # @.str.83
.L.str.83:
	.asciz	"You descend to level %d."
	.size	.L.str.83, 25

	.type	.L.str.84,@object               # @.str.84
.L.str.84:
	.asciz	"I see no way up."
	.size	.L.str.84, 17

	.type	.L.str.85,@object               # @.str.85
.L.str.85:
	.asciz	"Your way is magically blocked."
	.size	.L.str.85, 31

	.type	.L.str.86,@object               # @.str.86
.L.str.86:
	.asciz	"You escape with the Amulet of Yendor. Total winner!"
	.size	.L.str.86, 52

	.type	.L.str.87,@object               # @.str.87
.L.str.87:
	.asciz	"You climb up to level %d."
	.size	.L.str.87, 26

	.type	.L.str.88,@object               # @.str.88
.L.str.88:
	.asciz	"%c) %s%s%s\n"
	.size	.L.str.88, 12

	.type	.L.str.89,@object               # @.str.89
.L.str.89:
	.asciz	" (weapon in hand)"
	.size	.L.str.89, 18

	.type	.L.str.90,@object               # @.str.90
.L.str.90:
	.zero	1
	.size	.L.str.90, 1

	.type	.L.str.91,@object               # @.str.91
.L.str.91:
	.asciz	" (being worn)"
	.size	.L.str.91, 14

	.type	.L.str.92,@object               # @.str.92
.L.str.92:
	.asciz	"   the Amulet of Yendor\n"
	.size	.L.str.92, 25

	.type	.L.str.93,@object               # @.str.93
.L.str.93:
	.asciz	"You are empty handed."
	.size	.L.str.93, 22

	.type	.L.str.94,@object               # @.str.94
.L.str.94:
	.asciz	"You have nothing to %s."
	.size	.L.str.94, 24

	.type	.L.str.95,@object               # @.str.95
.L.str.95:
	.asciz	"You begin to feel better."
	.size	.L.str.95, 26

	.type	.L.str.96,@object               # @.str.96
.L.str.96:
	.asciz	"You begin to feel much better."
	.size	.L.str.96, 31

	.type	.L.str.97,@object               # @.str.97
.L.str.97:
	.asciz	"You feel stronger. What bulging muscles!"
	.size	.L.str.97, 41

	.type	.L.str.98,@object               # @.str.98
.L.str.98:
	.asciz	"You feel very sick."
	.size	.L.str.98, 20

	.type	.L.str.99,@object               # @.str.99
.L.str.99:
	.asciz	"You die... killed by %s."
	.size	.L.str.99, 25

	.type	.L.str.100,@object              # @.str.100
.L.str.100:
	.asciz	"Oh, now this scroll has a map on it!"
	.size	.L.str.100, 37

	.type	.L.str.101,@object              # @.str.101
.L.str.101:
	.asciz	"You feel a wrenching sensation."
	.size	.L.str.101, 32

	.type	.L.str.102,@object              # @.str.102
.L.str.102:
	.asciz	"Your weapon glows blue for a moment."
	.size	.L.str.102, 37

	.type	.L.str.103,@object              # @.str.103
.L.str.103:
	.asciz	"Your hands tingle."
	.size	.L.str.103, 19

	.type	.L.str.104,@object              # @.str.104
.L.str.104:
	.asciz	"Your armor glows silver for a moment."
	.size	.L.str.104, 38

	.type	.L.str.105,@object              # @.str.105
.L.str.105:
	.asciz	"Your skin itches."
	.size	.L.str.105, 18

	.type	.L.str.106,@object              # @.str.106
.L.str.106:
	.asciz	"The %s hits you."
	.size	.L.str.106, 17

	.type	.L.str.107,@object              # @.str.107
.L.str.107:
	.asciz	"a %s"
	.size	.L.str.107, 5

	.type	.L.str.108,@object              # @.str.108
.L.str.108:
	.asciz	"The %s misses you."
	.size	.L.str.108, 19

	.type	.L.str.109,@object              # @.str.109
.L.str.109:
	.asciz	"You are starting to get hungry."
	.size	.L.str.109, 32

	.type	.L.str.110,@object              # @.str.110
.L.str.110:
	.asciz	"You are weak from hunger."
	.size	.L.str.110, 26

	.type	.L.str.111,@object              # @.str.111
.L.str.111:
	.asciz	"You are starving!"
	.size	.L.str.111, 18

	.type	.L.str.112,@object              # @.str.112
.L.str.112:
	.asciz	"starvation"
	.size	.L.str.112, 11

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e34f541beea69553ff1fd655361b4faa1e656dc2)"
	.section	".note.GNU-stack","",@progbits
