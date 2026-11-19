	.file	"ui.c"
	.text
	.globl	line_render                     # -- Begin function line_render
	.p2align	2
	.type	line_render,@function
line_render:                            # @line_render
# %bb.0:
	addi sp, sp, -192
	stw sp+0, lr
	stw sp+188, r11
	stw sp+184, r12
	stw sp+180, r13
	stw sp+176, r14
	stw sp+172, r15
	stw sp+168, r16
	stw sp+164, r17
	stw sp+160, r18
	stw sp+156, r19
	stw sp+152, r20
	stw sp+148, r21
	add r11, r4, r0
	add r12, r3, r0
	addi r16, r0, 80
	addi r17, r0, 79
	addi r18, r0, 32
	addi r19, r0, 1
	lui r13, %hi(.L.str)
	addi r13, r13, %lo(.L.str)
	addi r20, r0, 22
	add r14, r0, r0
	jal r0, .LBB0_2
.LBB0_1:
	addi r5, sp, 28
	add r3, r11, r0
	add r4, r13, r0
	jal r31, fprintf
	addi r14, r14, 1
	beq r14, r20, .LBB0_7
.LBB0_2:
	add r15, r0, r0
.LBB0_3:
	add r3, r12, r0
	add r4, r14, r0
	add r5, r15, r0
	jal r31, glyph_at
	addi r3, sp, 28
	add r3, r3, r15
	stb r3+0, r1
	addi r15, r15, 1
	bne r15, r16, .LBB0_3
.LBB0_4:
	stb sp+108, r0
	add r1, r17, r0
.LBB0_5:
	addi r3, sp, 28
	add r3, r3, r1
	ldbu r4, r3+0
	bne r4, r18, .LBB0_1
.LBB0_6:
	stb r3+0, r0
	addi r3, r1, -1
	addi r4, r1, 1
	add r1, r3, r0
	bgtu r4, r19, .LBB0_5
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
	addi r12, sp, 60
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
	ldw r21, sp+148
	ldw r20, sp+152
	ldw r19, sp+156
	ldw r18, sp+160
	ldw r17, sp+164
	ldw r16, sp+168
	ldw r15, sp+172
	ldw r14, sp+176
	ldw r13, sp+180
	ldw r12, sp+184
	ldw r11, sp+188
	ldw lr, sp+0
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
	stw sp+76, r11
	stw sp+72, r12
	stw sp+68, r13
	stw sp+64, r14
	stw sp+60, r15
	stw sp+56, r16
	stw sp+52, r17
	stw sp+48, r18
	stw sp+44, r19
	stw sp+40, r20
	stw sp+36, r21
	stw sp+32, r22
	stw sp+28, r23
	stw sp+24, r24
	stw sp+20, r25
	addi r1, r0, 80
	mul r1, r4, r1
	add r1, r3, r1
	add r15, r1, r5
	ldbu r1, r15+1764
	andi r1, r1, 1
	bne r1, r0, .LBB1_3
.LBB1_1:
	addi r16, r0, 32
.LBB1_2:
	slli r1, r16, 24
	srai r1, r1, 24
	ldw r25, sp+20
	ldw r24, sp+24
	ldw r23, sp+28
	ldw r22, sp+32
	ldw r21, sp+36
	ldw r20, sp+40
	ldw r19, sp+44
	ldw r18, sp+48
	ldw r17, sp+52
	ldw r16, sp+56
	ldw r15, sp+60
	ldw r14, sp+64
	ldw r13, sp+68
	ldw r12, sp+72
	ldw r11, sp+76
	ldw lr, sp+0
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
	addi r18, r17, -392
	addi r19, r0, 1
	addi r20, r17, -388
	addi r21, r17, -384
	addi r22, r0, 40
	lui r23, %hi(mon_table+4)
	addi r23, r23, %lo(mon_table+4)
	addi r24, r0, 320
	add r25, r0, r0
                                        # implicit-def: $r16
	jal r0, .LBB1_9
.LBB1_7:
	beq r19, r0, .LBB1_2
.LBB1_8:
	addi r25, r25, 20
	beq r25, r24, .LBB1_15
.LBB1_9:
	add r1, r13, r25
	add r14, r1, r18
	ldw r3, r14+0
	blt r3, r0, .LBB1_7
.LBB1_10:
	add r3, r1, r20
	ldw r3, r3+0
	bne r3, r12, .LBB1_7
.LBB1_11:
	add r1, r1, r21
	ldw r1, r1+0
	bne r1, r11, .LBB1_7
.LBB1_12:
	add r3, r13, r0
	add r4, r14, r0
	jal r31, mon_visible
	add r3, r19, r0
	beq r1, r0, .LBB1_14
.LBB1_13:
	ldw r1, r14+0
	mul r1, r1, r22
	add r1, r1, r23
	ldbu r16, r1+0
	add r3, r0, r0
.LBB1_14:
	bne r3, r0, .LBB1_8
	jal r0, .LBB1_2
.LBB1_15:
	addi r1, r17, -72
	addi r3, r0, 1
	addi r4, r0, 576
	addi r5, r17, -56
	addi r6, r17, -52
	addi r7, r0, 42
	addi r8, r0, 6
	addi r9, r0, 63
	lui r10, %hi(.LJTI1_0)
	addi r10, r10, %lo(.LJTI1_0)
	addi lr, r0, 58
	addi r14, r0, 33
	addi r17, r0, 41
	addi r18, r0, 93
	addi r19, r0, 44
	add r20, r0, r0
	jal r0, .LBB1_18
.LBB1_16:
	beq r3, r0, .LBB1_2
.LBB1_17:
	addi r20, r20, 24
	beq r20, r4, .LBB1_31
.LBB1_18:
	add r21, r13, r20
	add r22, r21, r1
	ldw r22, r22+0
	beq r22, r0, .LBB1_16
.LBB1_19:
	add r23, r21, r5
	ldw r23, r23+0
	bne r23, r12, .LBB1_16
.LBB1_20:
	add r21, r21, r6
	ldw r23, r21+0
	add r21, r3, r0
	bne r23, r11, .LBB1_30
.LBB1_21:
	addi r16, r22, -1
	bgtu r16, r8, .LBB1_26
.LBB1_22:
	slli r16, r16, 2
	add r16, r10, r16
	ldw r22, r16+0
	add r16, r7, r0
	add r21, r0, r0
	jalr r0, r22, 0
.LBB1_23:
	add r16, lr, r0
	jal r0, .LBB1_29
.LBB1_24:
	add r16, r14, r0
	jal r0, .LBB1_29
.LBB1_25:
	add r16, r19, r0
	jal r0, .LBB1_29
.LBB1_26:
	add r16, r9, r0
	jal r0, .LBB1_29
.LBB1_27:
	add r16, r17, r0
	jal r0, .LBB1_29
.LBB1_28:
	add r16, r18, r0
.LBB1_29:
	add r21, r0, r0
.LBB1_30:
	bne r21, r0, .LBB1_17
	jal r0, .LBB1_2
.LBB1_31:
	ldbu r16, r15+4
	jal r0, .LBB1_2
.Lfunc_end1:
	.size	glyph_at, .Lfunc_end1-glyph_at
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI1_0,@object
.LJTI1_0:
	.word	.LBB1_30
	.word	.LBB1_23
	.word	.LBB1_24
	.word	.LBB1_26
	.word	.LBB1_27
	.word	.LBB1_28
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
	stw sp+108, r11
	stw sp+104, r12
	stw sp+100, r13
	stw sp+96, r14
	stw sp+92, r15
	add r11, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r14, sp, 28
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
	ldw r15, sp+92
	ldw r14, sp+96
	ldw r13, sp+100
	ldw r12, sp+104
	ldw r11, sp+108
	ldw lr, sp+0
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
	stw sp+252, r11
	stw sp+248, r12
	stw sp+244, r13
	stw sp+240, r14
	stw sp+236, r15
	stw sp+232, r16
	stw sp+228, r17
	stw sp+224, r18
	stw sp+220, r19
	stw sp+216, r20
	stw sp+212, r21
	stw sp+208, r22
	stw sp+204, r23
	stw sp+200, r24
	stw sp+196, r25
	stw sp+192, r26
	stw sp+188, r27
	stw sp+184, r28
	add r11, r3, r0
	jal r31, term_save_screen
	add r3, r0, r0
	jal r31, term_clear
	addi r12, r0, 1
	add r3, r12, r0
	add r4, r12, r0
	jal r31, term_gotoxy
	lui r3, %hi(.L.str.3)
	addi r3, r3, %lo(.L.str.3)
	jal r31, term_puts
	lui r21, 1
	addi r1, r21, 572
	add r13, r11, r1
	addi r1, r21, 560
	add r22, r11, r1
	addi r1, r21, 564
	add r23, r11, r1
	addi r14, r0, 2
	addi r24, r0, 26
	addi r15, r0, 64
	lui r25, %hi(.L.str.6)
	addi r25, r25, %lo(.L.str.6)
	lui r1, %hi(.L.str.5)
	addi r1, r1, %lo(.L.str.5)
	xor r26, r1, r25
	lui r1, %hi(.L.str.7)
	addi r1, r1, %lo(.L.str.7)
	xor r27, r1, r25
	lui r16, %hi(.L.str.4)
	addi r16, r16, %lo(.L.str.4)
	addi r17, r0, 96
	add r20, r0, r0
	add r28, r0, r0
	jal r0, .LBB3_2
.LBB3_1:
	addi r28, r28, 1
	addi r13, r13, 16
	beq r28, r24, .LBB3_4
.LBB3_2:
	ldw r1, r13+0
	beq r1, r0, .LBB3_1
.LBB3_3:
	addi r18, sp, 120
	add r3, r11, r0
	add r4, r13, r0
	add r5, r18, r0
	add r6, r15, r0
	jal r31, inv_name
	addi r6, r28, 97
	ldw r1, r22+0
	seq r1, r28, r1
	sub r1, r0, r1
	and r1, r26, r1
	xor r8, r25, r1
	ldw r1, r23+0
	seq r1, r28, r1
	sub r1, r0, r1
	and r1, r27, r1
	xor r9, r25, r1
	addi r19, sp, 24
	add r3, r19, r0
	add r4, r17, r0
	add r5, r16, r0
	add r7, r18, r0
	jal r31, snprintf
	addi r18, r14, 1
	add r3, r14, r0
	add r4, r12, r0
	jal r31, term_gotoxy
	add r3, r19, r0
	jal r31, term_puts
	addi r20, r20, 1
	add r14, r18, r0
	jal r0, .LBB3_1
.LBB3_4:
	addi r1, r21, 568
	add r1, r11, r1
	ldw r1, r1+0
	beq r1, r0, .LBB3_7
.LBB3_5:
	addi r11, r14, 1
	addi r4, r0, 1
	add r3, r14, r0
	jal r31, term_gotoxy
	lui r3, %hi(.L.str.8)
	addi r3, r3, %lo(.L.str.8)
	jal r31, term_puts
	addi r20, r20, 1
	beq r20, r0, .LBB3_8
.LBB3_6:
	add r12, r11, r0
	jal r0, .LBB3_9
.LBB3_7:
	add r11, r14, r0
	bne r20, r0, .LBB3_6
.LBB3_8:
	addi r12, r11, 1
	addi r4, r0, 1
	add r3, r11, r0
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
	ldw r28, sp+184
	ldw r27, sp+188
	ldw r26, sp+192
	ldw r25, sp+196
	ldw r24, sp+200
	ldw r23, sp+204
	ldw r22, sp+208
	ldw r21, sp+212
	ldw r20, sp+216
	ldw r19, sp+220
	ldw r18, sp+224
	ldw r17, sp+228
	ldw r16, sp+232
	ldw r15, sp+236
	ldw r14, sp+240
	ldw r13, sp+244
	ldw r12, sp+248
	ldw r11, sp+252
	ldw lr, sp+0
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
	stw sp+76, r11
	stw sp+72, r12
	stw sp+68, r13
	stw sp+64, r14
	stw sp+60, r15
	stw sp+56, r16
	stw sp+52, r17
	stw sp+48, r18
	stw sp+44, r19
	stw sp+40, r20
	stw sp+36, r21
	stw sp+32, r22
	stw sp+28, r23
	stw sp+24, r24
	add r11, r3, r0
	add r3, r0, r0
	jal r31, term_clear
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	add r3, r11, r0
	jal r31, msgf
	addi r17, r0, 27
	addi r18, r0, 91
	addi r16, r0, 3
	lui r19, %hi(.Lswitch.table.ui_play)
	addi r19, r19, %lo(.Lswitch.table.ui_play)
	addi r20, r0, -1
	addi r21, r0, 63
	addi r12, r0, 1
	lui r22, %hi(help_screen.lines)
	addi r22, r22, %lo(help_screen.lines)
	addi r23, r0, 12
	addi r13, r0, 13
	lui r14, %hi(.L.str.10)
	addi r14, r14, %lo(.L.str.10)
	jal r0, .LBB4_2
.LBB4_1:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, do_command
	beq r1, r0, .LBB4_13
.LBB4_2:
	add r3, r11, r0
	jal r31, draw
	jal r31, term_getkey
	bne r1, r17, .LBB4_7
.LBB4_3:
	jal r31, term_kbhit
	add r3, r1, r0
	add r1, r17, r0
	beq r3, r0, .LBB4_7
.LBB4_4:
	jal r31, term_getkey
	add r3, r1, r0
	add r1, r17, r0
	bne r3, r18, .LBB4_7
.LBB4_5:
	jal r31, term_getkey
	addi r3, r1, -65
	add r1, r17, r0
	bgtu r3, r16, .LBB4_7
.LBB4_6:
	add r1, r3, r19
	ldbu r1, r1+0
.LBB4_7:
	ble r1, r20, .LBB4_12
.LBB4_8:
	bne r1, r21, .LBB4_1
.LBB4_9:
	jal r31, term_save_screen
	add r3, r0, r0
	jal r31, term_clear
	add r24, r22, r0
	add r15, r12, r0
.LBB4_10:
	add r3, r15, r0
	add r4, r12, r0
	jal r31, term_gotoxy
	ldw r3, r24+0
	jal r31, term_puts
	addi r15, r15, 1
	addi r24, r24, 4
	bne r15, r23, .LBB4_10
.LBB4_11:
	add r3, r13, r0
	add r4, r12, r0
	jal r31, term_gotoxy
	add r3, r14, r0
	jal r31, term_puts
	jal r31, term_getkey
	jal r31, term_restore_screen
	jal r0, .LBB4_2
.LBB4_12:
	lui r1, 1
	addi r1, r1, 996
	add r1, r11, r1
	stw r1+0, r16
.LBB4_13:
	add r3, r11, r0
	jal r31, draw
	lui r13, 1
	addi r1, r13, 996
	add r1, r11, r1
	ldw r1, r1+0
	bne r1, r16, .LBB4_15
.LBB4_14:
	addi r1, r13, 1040
	add r1, r11, r1
	ldbu r1, r1+0
	beq r1, r0, .LBB4_16
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
	ldw r24, sp+24
	ldw r23, sp+28
	ldw r22, sp+32
	ldw r21, sp+36
	ldw r20, sp+40
	ldw r19, sp+44
	ldw r18, sp+48
	ldw r17, sp+52
	ldw r16, sp+56
	ldw r15, sp+60
	ldw r14, sp+64
	ldw r13, sp+68
	ldw r12, sp+72
	ldw r11, sp+76
	ldw lr, sp+0
	addi sp, sp, 80
	jal r0, term_getkey
.LBB4_16:
	ldw r24, sp+24
	ldw r23, sp+28
	ldw r22, sp+32
	ldw r21, sp+36
	ldw r20, sp+40
	ldw r19, sp+44
	ldw r18, sp+48
	ldw r17, sp+52
	ldw r16, sp+56
	ldw r15, sp+60
	ldw r14, sp+64
	ldw r13, sp+68
	ldw r12, sp+72
	ldw r11, sp+76
	ldw lr, sp+0
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
	stw sp+188, r11
	stw sp+184, r12
	stw sp+180, r13
	stw sp+176, r14
	stw sp+172, r15
	stw sp+168, r16
	stw sp+164, r17
	stw sp+160, r18
	stw sp+156, r19
	stw sp+152, r20
	stw sp+148, r21
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
	addi r15, r0, 80
	addi r16, r0, 22
	add r13, r0, r0
.LBB5_1:
	addi r3, r13, 2
	add r4, r12, r0
	jal r31, term_gotoxy
	add r14, r0, r0
.LBB5_2:
	add r3, r11, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, glyph_at
	add r3, r1, r0
	jal r31, term_putc
	addi r14, r14, 1
	bne r14, r15, .LBB5_2
.LBB5_3:
	addi r13, r13, 1
	bne r13, r16, .LBB5_1
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
	addi r17, sp, 60
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
	ldw r21, sp+148
	ldw r20, sp+152
	ldw r19, sp+156
	ldw r18, sp+160
	ldw r17, sp+164
	ldw r16, sp+168
	ldw r15, sp+172
	ldw r14, sp+176
	ldw r13, sp+180
	ldw r12, sp+184
	ldw r11, sp+188
	ldw lr, sp+0
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

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git 77f07727152e39f179dfe39786aa3bcae0860790)"
	.section	".note.GNU-stack","",@progbits
