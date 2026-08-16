	.file	"ui.c"
	.text
	.globl	line_render                     # -- Begin function line_render
	.p2align	2
	.type	line_render,@function
line_render:                            # @line_render
# %bb.0:
	addi sp, sp, -192
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 192
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
	add r11, r4, r0
	add r12, r3, r0
	addi r17, r0, 0
	addi r13, fp, -168
	addi r18, r0, 80
	addi r19, r0, 79
	addi r20, r0, 32
	addi r21, r0, 1
	lui r14, %hi(.L.str)
	addi r14, r14, %lo(.L.str)
	addi r22, r0, 22
	add r15, r17, r0
	jal r0, .LBB0_2
.LBB0_1:
	add r3, r11, r0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, fprintf
	addi r15, r15, 1
	beq r15, r22, .LBB0_7
.LBB0_2:
	add r16, r17, r0
.LBB0_3:
	add r3, r12, r0
	add r4, r15, r0
	add r5, r16, r0
	jal r31, glyph_at
	add r3, r13, r16
	stb r3+0, r1
	addi r16, r16, 1
	bne r16, r18, .LBB0_3
.LBB0_4:
	stb fp+-88, r17
	add r1, r19, r0
.LBB0_5:
	add r3, r13, r1
	ldbu r4, r3+0
	bne r4, r20, .LBB0_1
.LBB0_6:
	stb r3+0, r17
	addi r3, r1, -1
	addi r4, r1, 1
	add r1, r3, r0
	bgtu r4, r21, .LBB0_5
	jal r0, .LBB0_1
.LBB0_7:
	lui r20, 1
	addi r1, r20, 548
	add r1, r12, r1
	ldw r13, r1+0
	addi r1, r20, 544
	add r1, r12, r1
	ldw r14, r1+0
	addi r1, r20, 520
	add r1, r12, r1
	ldw r15, r1+0
	addi r1, r20, 524
	add r1, r12, r1
	ldw r16, r1+0
	addi r1, r20, 528
	add r1, r12, r1
	ldw r17, r1+0
	addi r1, r20, 532
	add r1, r12, r1
	ldw r21, r1+0
	add r3, r12, r0
	jal r31, player_armor
	add r18, r1, r0
	addi r1, r20, 536
	add r1, r12, r1
	ldw r19, r1+0
	addi r1, r20, 540
	add r1, r12, r1
	ldw r12, r1+0
	add r3, r19, r0
	jal r31, rank_name
	addi sp, sp, -32
	stw sp+0, r21
	stw sp+4, r18
	stw sp+8, r19
	stw sp+12, r12
	stw sp+16, r1
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r12, fp, -168
	addi r4, r0, 120
	add r3, r12, r0
	add r6, r13, r0
	add r7, r14, r0
	add r8, r15, r0
	add r9, r16, r0
	add r10, r17, r0
	jal r31, snprintf
	addi sp, sp, 32
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, fprintf
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
	addi sp, sp, 192
	jalr r0, r31, 0
.Lfunc_end0:
	.size	line_render, .Lfunc_end0-line_render
                                        # -- End function
	.p2align	2                               # -- Begin function glyph_at
	.type	glyph_at,@function
glyph_at:                               # @glyph_at
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
	stw fp+-48, r22
	stw fp+-52, r23
	stw fp+-56, r24
	stw fp+-60, r25
	stw fp+-64, r26
	addi r1, r0, 80
	mul r1, r4, r1
	add r1, r3, r1
	add r15, r1, r5
	ldbu r1, r15+1764
	andi r1, r1, 1
	addi r18, r0, 0
	bne r1, r18, .LBB1_3
.LBB1_1:
	addi r16, r0, 32
.LBB1_2:
	slli r1, r16, 24
	srai r1, r1, 24
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
	addi sp, sp, 80
	jalr r0, r31, 0
.LBB1_3:
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	lui r17, 1
	addi r1, r17, 512
	add r1, r3, r1
	ldw r1, r1+0
	bne r4, r1, .LBB1_6
.LBB1_4:
	addi r1, r17, 516
	add r1, r13, r1
	ldw r1, r1+0
	bne r11, r1, .LBB1_6
.LBB1_5:
	addi r16, r0, 64
	jal r0, .LBB1_2
.LBB1_6:
	addi r19, r17, -392
	addi r20, r0, 1
	addi r21, r17, -388
	addi r22, r17, -384
	addi r23, r0, 40
	lui r24, %hi(mon_table+4)
	addi r24, r24, %lo(mon_table+4)
	addi r25, r0, 320
	add r26, r18, r0
                                        # implicit-def: $r16
	jal r0, .LBB1_9
.LBB1_7:
	beq r20, r18, .LBB1_2
.LBB1_8:
	addi r26, r26, 20
	beq r26, r25, .LBB1_15
.LBB1_9:
	add r1, r13, r26
	add r14, r1, r19
	ldw r3, r14+0
	blt r3, r18, .LBB1_7
.LBB1_10:
	add r3, r1, r21
	ldw r3, r3+0
	bne r3, r12, .LBB1_7
.LBB1_11:
	add r1, r1, r22
	ldw r1, r1+0
	bne r1, r11, .LBB1_7
.LBB1_12:
	add r3, r13, r0
	add r4, r14, r0
	jal r31, mon_visible
	add r3, r20, r0
	beq r1, r18, .LBB1_14
.LBB1_13:
	ldw r1, r14+0
	mul r1, r1, r23
	add r1, r1, r24
	ldbu r16, r1+0
	add r3, r18, r0
.LBB1_14:
	bne r3, r18, .LBB1_8
	jal r0, .LBB1_2
.LBB1_15:
	addi r1, r0, 0
	addi r3, r17, -72
	addi r4, r0, 1
	addi r5, r0, 576
	addi r6, r17, -56
	addi r7, r17, -52
	addi r8, r0, 42
	addi r9, r0, 6
	addi r10, r0, 63
	lui lr, %hi(.LJTI1_0)
	addi lr, lr, %lo(.LJTI1_0)
	addi r14, r0, 58
	addi r17, r0, 33
	addi r18, r0, 41
	addi r19, r0, 93
	addi r20, r0, 44
	add r21, r1, r0
	jal r0, .LBB1_18
.LBB1_16:
	beq r4, r1, .LBB1_2
.LBB1_17:
	addi r21, r21, 24
	beq r21, r5, .LBB1_30
.LBB1_18:
	add r22, r13, r21
	add r23, r22, r3
	ldw r23, r23+0
	beq r23, r1, .LBB1_16
.LBB1_19:
	add r24, r22, r6
	ldw r24, r24+0
	bne r24, r12, .LBB1_16
.LBB1_20:
	add r22, r22, r7
	ldw r24, r22+0
	add r22, r4, r0
	bne r24, r11, .LBB1_27
.LBB1_21:
	addi r16, r23, -1
	bgtu r16, r9, .LBB1_26
.LBB1_22:
	slli r16, r16, 2
	add r16, lr, r16
	ldw r23, r16+0
	add r22, r1, r0
	add r16, r8, r0
	jalr r0, r23, 0
.LBB1_23:
	add r16, r14, r0
	bne r1, r1, .LBB1_17
	jal r0, .LBB1_2
.LBB1_24:
	add r16, r17, r0
	bne r1, r1, .LBB1_17
	jal r0, .LBB1_2
.LBB1_25:
	add r16, r20, r0
	bne r1, r1, .LBB1_17
	jal r0, .LBB1_2
.LBB1_26:
	add r22, r1, r0
	add r16, r10, r0
.LBB1_27:
	bne r22, r1, .LBB1_17
	jal r0, .LBB1_2
.LBB1_28:
	add r16, r18, r0
	bne r1, r1, .LBB1_17
	jal r0, .LBB1_2
.LBB1_29:
	add r16, r19, r0
	bne r1, r1, .LBB1_17
	jal r0, .LBB1_2
.LBB1_30:
	ldbu r16, r15+4
	jal r0, .LBB1_2
.Lfunc_end1:
	.size	glyph_at, .Lfunc_end1-glyph_at
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI1_0,@object
.LJTI1_0:
	.word	.LBB1_27
	.word	.LBB1_23
	.word	.LBB1_24
	.word	.LBB1_26
	.word	.LBB1_28
	.word	.LBB1_29
	.word	.LBB1_25
	.size	.LJTI1_0, 28
                                        # -- End function
	.text
	.globl	ui_pick_slot                    # -- Begin function ui_pick_slot
	.p2align	2
	.type	ui_pick_slot,@function
ui_pick_slot:                           # @ui_pick_slot
# %bb.0:
	addi sp, sp, -112
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 112
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	add r11, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r14, fp, -84
	addi r4, r0, 64
	add r3, r14, r0
	add r6, r11, r0
	jal r31, snprintf
	addi r15, r0, 1
	add r3, r15, r0
	add r4, r15, r0
	jal r31, term_gotoxy
	add r3, r15, r0
	jal r31, term_clear
	add r3, r14, r0
	jal r31, term_puts
	jal r31, term_getkey
	addi r3, r1, -123
	addi r14, r0, -1
	addi r4, r0, -26
	bltu r3, r4, .LBB2_3
.LBB2_1:
	addi r1, r1, -97
	slli r3, r1, 4
	add r3, r12, r3
	lui r4, 1
	addi r4, r4, 572
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r13, .LBB2_4
.LBB2_2:
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	add r3, r12, r0
	add r5, r11, r0
	jal r31, msgf
.LBB2_3:
	add r1, r14, r0
.LBB2_4:
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 112
	jalr r0, r31, 0
.Lfunc_end2:
	.size	ui_pick_slot, .Lfunc_end2-ui_pick_slot
                                        # -- End function
	.globl	ui_show_inventory               # -- Begin function ui_show_inventory
	.p2align	2
	.type	ui_show_inventory,@function
ui_show_inventory:                      # @ui_show_inventory
# %bb.0:
	addi sp, sp, -256
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 256
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
	jal r31, term_save_screen
	addi r21, r0, 0
	add r3, r21, r0
	jal r31, term_clear
	addi r3, r0, 1
	stw fp+-236, r3
	add r4, r3, r0
	jal r31, term_gotoxy
	lui r3, %hi(.L.str.3)
	addi r3, r3, %lo(.L.str.3)
	jal r31, term_puts
	lui r3, 1
	addi r1, r3, 572
	add r14, r11, r1
	addi r1, r3, 560
	add r23, r11, r1
	stw fp+-240, r3
	addi r1, r3, 564
	add r13, r11, r0
	add r24, r11, r1
	addi r11, r0, 2
	addi r25, r0, 26
	addi r16, fp, -136
	addi r17, r0, 64
	lui r26, %hi(.L.str.6)
	addi r26, r26, %lo(.L.str.6)
	lui r1, %hi(.L.str.5)
	addi r1, r1, %lo(.L.str.5)
	xor r27, r1, r26
	lui r1, %hi(.L.str.7)
	addi r1, r1, %lo(.L.str.7)
	xor r28, r1, r26
	lui r18, %hi(.L.str.4)
	addi r18, r18, %lo(.L.str.4)
	addi r19, fp, -232
	addi r20, r0, 96
	add r22, r21, r0
	add r12, r21, r0
	jal r0, .LBB3_2
.LBB3_1:
	addi r12, r12, 1
	addi r14, r14, 16
	beq r12, r25, .LBB3_4
.LBB3_2:
	ldw r1, r14+0
	beq r1, r21, .LBB3_1
.LBB3_3:
	add r3, r13, r0
	add r4, r14, r0
	add r5, r16, r0
	add r6, r17, r0
	jal r31, inv_name
	addi r6, r12, 97
	ldw r1, r23+0
	seq r1, r12, r1
	sub r1, r21, r1
	and r1, r27, r1
	xor r8, r1, r26
	ldw r1, r24+0
	seq r1, r12, r1
	sub r1, r21, r1
	and r1, r28, r1
	xor r9, r1, r26
	add r3, r19, r0
	add r4, r20, r0
	add r5, r18, r0
	add r7, r16, r0
	jal r31, snprintf
	addi r15, r11, 1
	add r3, r11, r0
	ldw r4, fp+-236
	jal r31, term_gotoxy
	add r3, r19, r0
	jal r31, term_puts
	addi r22, r22, 1
	add r11, r15, r0
	jal r0, .LBB3_1
.LBB3_4:
	ldw r1, fp+-240
	addi r1, r1, 568
	add r1, r13, r1
	ldw r1, r1+0
	addi r12, r0, 0
	beq r1, r12, .LBB3_7
.LBB3_5:
	addi r13, r11, 1
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, term_gotoxy
	lui r3, %hi(.L.str.8)
	addi r3, r3, %lo(.L.str.8)
	jal r31, term_puts
	addi r22, r22, 1
	beq r22, r12, .LBB3_8
.LBB3_6:
	add r12, r13, r0
	jal r0, .LBB3_9
.LBB3_7:
	add r13, r11, r0
	bne r22, r12, .LBB3_6
.LBB3_8:
	addi r12, r13, 1
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, term_gotoxy
	lui r3, %hi(.L.str.9)
	addi r3, r3, %lo(.L.str.9)
	jal r31, term_puts
.LBB3_9:
	addi r3, r12, 1
	addi r4, r0, 1
	jal r31, term_gotoxy
	lui r3, %hi(.L.str.10)
	addi r3, r3, %lo(.L.str.10)
	jal r31, term_puts
	jal r31, term_getkey
	jal r31, term_restore_screen
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
	addi sp, sp, 256
	jalr r0, r31, 0
.Lfunc_end3:
	.size	ui_show_inventory, .Lfunc_end3-ui_show_inventory
                                        # -- End function
	.globl	ui_play                         # -- Begin function ui_play
	.p2align	2
	.type	ui_play,@function
ui_play:                                # @ui_play
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
	stw fp+-48, r22
	stw fp+-52, r23
	stw fp+-56, r24
	stw fp+-60, r25
	add r11, r3, r0
	addi r12, r0, 0
	add r3, r12, r0
	jal r31, term_clear
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	add r3, r11, r0
	jal r31, msgf
	addi r18, r0, 27
	addi r19, r0, 91
	addi r17, r0, 3
	lui r20, %hi(.Lswitch.table.ui_play)
	addi r20, r20, %lo(.Lswitch.table.ui_play)
	addi r21, r0, -1
	addi r22, r0, 63
	addi r13, r0, 1
	lui r23, %hi(help_screen.lines)
	addi r23, r23, %lo(help_screen.lines)
	addi r24, r0, 12
	addi r14, r0, 13
	lui r15, %hi(.L.str.10)
	addi r15, r15, %lo(.L.str.10)
	jal r0, .LBB4_2
.LBB4_1:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, do_command
	beq r1, r12, .LBB4_13
.LBB4_2:
	add r3, r11, r0
	jal r31, draw
	jal r31, term_getkey
	bne r1, r18, .LBB4_7
.LBB4_3:
	jal r31, term_kbhit
	add r3, r1, r0
	add r1, r18, r0
	beq r3, r12, .LBB4_7
.LBB4_4:
	jal r31, term_getkey
	add r3, r1, r0
	add r1, r18, r0
	bne r3, r19, .LBB4_7
.LBB4_5:
	jal r31, term_getkey
	addi r3, r1, -65
	add r1, r18, r0
	bgtu r3, r17, .LBB4_7
.LBB4_6:
	add r1, r3, r20
	ldbu r1, r1+0
.LBB4_7:
	ble r1, r21, .LBB4_12
.LBB4_8:
	bne r1, r22, .LBB4_1
.LBB4_9:
	jal r31, term_save_screen
	add r3, r12, r0
	jal r31, term_clear
	add r25, r23, r0
	add r16, r13, r0
.LBB4_10:
	add r3, r16, r0
	add r4, r13, r0
	jal r31, term_gotoxy
	ldw r3, r25+0
	jal r31, term_puts
	addi r16, r16, 1
	addi r25, r25, 4
	bne r16, r24, .LBB4_10
.LBB4_11:
	add r3, r14, r0
	add r4, r13, r0
	jal r31, term_gotoxy
	add r3, r15, r0
	jal r31, term_puts
	jal r31, term_getkey
	jal r31, term_restore_screen
	jal r0, .LBB4_2
.LBB4_12:
	lui r1, 1
	addi r1, r1, 996
	add r1, r11, r1
	stw r1+0, r17
.LBB4_13:
	add r3, r11, r0
	jal r31, draw
	lui r13, 1
	addi r1, r13, 996
	add r1, r11, r1
	ldw r1, r1+0
	bne r1, r17, .LBB4_15
.LBB4_14:
	addi r1, r13, 1040
	add r1, r11, r1
	ldbu r1, r1+0
	beq r1, r12, .LBB4_16
.LBB4_15:
	addi r12, r0, 1
	add r3, r12, r0
	add r4, r12, r0
	jal r31, term_gotoxy
	add r3, r12, r0
	jal r31, term_clear
	addi r1, r13, 1040
	add r3, r11, r1
	jal r31, term_puts
	addi r3, r0, 24
	add r4, r12, r0
	jal r31, term_gotoxy
	lui r3, %hi(.L.str.10)
	addi r3, r3, %lo(.L.str.10)
	jal r31, term_puts
	jal r31, term_getkey
.LBB4_16:
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
	addi sp, sp, 80
	jalr r0, r31, 0
.Lfunc_end4:
	.size	ui_play, .Lfunc_end4-ui_play
                                        # -- End function
	.p2align	2                               # -- Begin function draw
	.type	draw,@function
draw:                                   # @draw
# %bb.0:
	addi sp, sp, -192
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 192
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
	add r11, r3, r0
	jal r31, term_begin_update
	addi r12, r0, 1
	add r3, r12, r0
	add r4, r12, r0
	jal r31, term_gotoxy
	add r3, r12, r0
	jal r31, term_clear
	lui r19, 1
	addi r1, r19, 1040
	add r3, r11, r1
	jal r31, term_puts
	addi r15, r0, 0
	addi r16, r0, 80
	addi r17, r0, 22
	add r13, r15, r0
.LBB5_1:
	addi r3, r13, 2
	add r4, r12, r0
	jal r31, term_gotoxy
	add r14, r15, r0
.LBB5_2:
	add r3, r11, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, glyph_at
	add r3, r1, r0
	jal r31, term_putc
	addi r14, r14, 1
	bne r14, r16, .LBB5_2
.LBB5_3:
	addi r13, r13, 1
	bne r13, r17, .LBB5_1
.LBB5_4:
	addi r1, r19, 548
	add r1, r11, r1
	ldw r12, r1+0
	addi r1, r19, 544
	add r1, r11, r1
	ldw r13, r1+0
	addi r1, r19, 520
	add r1, r11, r1
	ldw r14, r1+0
	addi r1, r19, 524
	add r1, r11, r1
	ldw r15, r1+0
	addi r1, r19, 528
	add r1, r11, r1
	ldw r16, r1+0
	addi r1, r19, 532
	add r1, r11, r1
	ldw r20, r1+0
	add r3, r11, r0
	jal r31, player_armor
	add r17, r1, r0
	addi r1, r19, 536
	add r1, r11, r1
	ldw r18, r1+0
	addi r1, r19, 540
	add r1, r11, r1
	ldw r21, r1+0
	add r3, r18, r0
	jal r31, rank_name
	addi sp, sp, -32
	stw sp+0, r20
	stw sp+4, r17
	stw sp+8, r18
	stw sp+12, r21
	stw sp+16, r1
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r17, fp, -164
	addi r4, r0, 120
	add r3, r17, r0
	add r6, r12, r0
	add r7, r13, r0
	add r8, r14, r0
	add r9, r15, r0
	add r10, r16, r0
	jal r31, snprintf
	addi sp, sp, 32
	addi r3, r0, 24
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, term_gotoxy
	add r3, r12, r0
	jal r31, term_clear
	add r3, r17, r0
	jal r31, term_puts
	addi r1, r19, 512
	add r1, r11, r1
	ldw r1, r1+0
	addi r3, r1, 2
	addi r1, r19, 516
	add r1, r11, r1
	ldw r1, r1+0
	addi r4, r1, 1
	jal r31, term_gotoxy
	jal r31, term_end_update
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
	addi sp, sp, 192
	jalr r0, r31, 0
.Lfunc_end5:
	.size	draw, .Lfunc_end5-draw
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%s\n"
	.size	.L.str, 4

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"%s what? [a-z, ESC to cancel]"
	.size	.L.str.1, 30

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"You cannot %s that."
	.size	.L.str.2, 20

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"You are carrying:"
	.size	.L.str.3, 18

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"  %c) %s%s%s"
	.size	.L.str.4, 13

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	" (weapon in hand)"
	.size	.L.str.5, 18

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.zero	1
	.size	.L.str.6, 1

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	" (being worn)"
	.size	.L.str.7, 14

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"     the Amulet of Yendor"
	.size	.L.str.8, 26

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"  nothing at all"
	.size	.L.str.9, 17

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"--press any key--"
	.size	.L.str.10, 18

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"Hello. Welcome to the Dungeons of Doom. (? for help)"
	.size	.L.str.11, 53

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"Level: %d  Gold: %d  Hp: %d(%d)  Str: %d(%d)  Arm: %d  Exp: %d/%d  %s"
	.size	.L.str.12, 70

	.type	help_screen.lines,@object       # @help_screen.lines
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
help_screen.lines:
	.word	.L.str.13
	.word	.L.str.14
	.word	.L.str.15
	.word	.L.str.16
	.word	.L.str.17
	.word	.L.str.18
	.word	.L.str.19
	.word	.L.str.20
	.word	.L.str.21
	.word	.L.str.6
	.word	.L.str.22
	.word	0
	.size	help_screen.lines, 48

	.type	.L.str.13,@object               # @.str.13
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.13:
	.asciz	"Commands:"
	.size	.L.str.13, 10

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"  h j k l    move left/down/up/right (arrows work too)"
	.size	.L.str.14, 55

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"  y u b n    move diagonally"
	.size	.L.str.15, 29

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"  >  <       descend / ascend a staircase (%)"
	.size	.L.str.16, 46

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"  i          inventory        e   eat food"
	.size	.L.str.17, 43

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"  q          quaff a potion   r   read a scroll"
	.size	.L.str.18, 48

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"  w          wield a weapon   W   wear armor    T  take off"
	.size	.L.str.19, 60

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"  . or s     rest a turn"
	.size	.L.str.20, 25

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"  S          save and exit    Q   quit"
	.size	.L.str.21, 39

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"Fetch the Amulet of Yendor from level 26 and bring it back up."
	.size	.L.str.22, 63

	.type	.Lswitch.table.ui_play,@object  # @switch.table.ui_play
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
.Lswitch.table.ui_play:
	.ascii	"kjlh"
	.size	.Lswitch.table.ui_play, 4

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e34f541beea69553ff1fd655361b4faa1e656dc2)"
	.section	".note.GNU-stack","",@progbits
