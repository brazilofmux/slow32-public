	.file	"asteroids.c"
	.text
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi sp, sp, -352
	stw sp+0, lr
	stw sp+4, fp
	stw sp+348, r11
	stw sp+344, r12
	stw sp+340, r13
	stw sp+336, r14
	stw sp+332, r15
	stw sp+328, r16
	stw sp+324, r17
	stw sp+320, r18
	stw sp+316, r19
	stw sp+312, r20
	stw sp+308, r21
	stw sp+304, r22
	stw sp+300, r23
	stw sp+296, r24
	stw sp+292, r25
	stw sp+288, r26
	stw sp+284, r27
	stw sp+280, r28
	addi r11, r0, 2
	blt r3, r11, .LBB0_9
.LBB0_1:
	add r13, r3, r0
	addi r12, r4, 4
	addi r17, r0, 1
	lui r15, %hi(.L.str)
	addi r15, r15, %lo(.L.str)
	lui r18, %hi(test_mode)
	lui r16, %hi(.L.str.1)
	addi r16, r16, %lo(.L.str.1)
	add r19, r17, r0
	add r14, r0, r0
	add r21, r0, r0
	jal r0, .LBB0_4
.LBB0_2:
	ldw r3, r12+4
	jal r31, atoi
	add r21, r1, r0
	stb r18+%lo(test_mode), r17
.LBB0_3:
	addi r19, r19, 2
	addi r12, r12, 8
	bge r19, r13, .LBB0_10
.LBB0_4:
	ldw r3, r12+0
	add r4, r15, r0
	jal r31, strcmp
	addi r20, r19, 1
	bne r1, r0, .LBB0_6
.LBB0_5:
	blt r20, r13, .LBB0_2
.LBB0_6:
	ldw r3, r12+0
	add r4, r16, r0
	jal r31, strcmp
	bne r1, r0, .LBB0_12
.LBB0_7:
	bge r20, r13, .LBB0_12
.LBB0_8:
	ldw r3, r12+4
	jal r31, atoi
	add r14, r1, r0
	jal r0, .LBB0_3
.LBB0_9:
	add r21, r0, r0
	add r14, r0, r0
.LBB0_10:
	jal r31, tube_init
	beq r1, r0, .LBB0_15
.LBB0_11:
	lui r3, %hi(.L.str.3)
	addi r3, r3, %lo(.L.str.3)
	jal r0, .LBB0_13
.LBB0_12:
	lui r3, %hi(.L.str.2)
	addi r3, r3, %lo(.L.str.2)
.LBB0_13:
	jal r31, printf
	addi r1, r0, 1
.LBB0_14:
	ldw r28, sp+280
	ldw r27, sp+284
	ldw r26, sp+288
	ldw r25, sp+292
	ldw r24, sp+296
	ldw r23, sp+300
	ldw r22, sp+304
	ldw r21, sp+308
	ldw r20, sp+312
	ldw r19, sp+316
	ldw r18, sp+320
	ldw r17, sp+324
	ldw r16, sp+328
	ldw r15, sp+332
	ldw r14, sp+336
	ldw r13, sp+340
	ldw r12, sp+344
	ldw r11, sp+348
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 352
	jalr r0, r31, 0
.LBB0_15:
	addi r3, r0, 1
	add r12, r3, r0
	jal r31, tube_open
	beq r1, r0, .LBB0_17
.LBB0_16:
	lui r3, %hi(.L.str.4)
	addi r3, r3, %lo(.L.str.4)
	jal r31, printf
	jal r31, tube_cleanup
	add r1, r12, r0
	jal r0, .LBB0_14
.LBB0_17:
	stw sp+40, r21
	addi r16, r0, 0
	seq r1, r14, r16
	lui r3, 677856
	addi r3, r3, 469
	xor r3, r14, r3
	sub r1, r0, r1
	and r1, r3, r1
	xor r1, r14, r1
	lui r3, %hi(rng_state)
	stw sp+24, r3
	stw r3+%lo(rng_state), r1
	lui r1, %hi(lives)
	addi r28, r0, 3
	stw sp+116, r1
	stw r1+%lo(lives), r28
	lui r1, %hi(wave)
	addi r17, r0, 1
	stw sp+36, r1
	stw r1+%lo(wave), r17
	lui r1, %hi(next_life)
	lui r3, 2
	addi r3, r3, 1808
	stw r1+%lo(next_life), r3
	lui r1, %hi(py)
	lui r21, 128
	stw sp+124, r1
	stw r1+%lo(py), r21
	lui r1, %hi(px)
	stw sp+120, r1
	stw r1+%lo(px), r21
	lui r1, %hi(pvy)
	stw sp+44, r1
	stw r1+%lo(pvy), r0
	lui r1, %hi(pvx)
	stw sp+48, r1
	stw r1+%lo(pvx), r0
	lui r1, %hi(pangle)
	addi r3, r0, 16
	stw sp+56, r1
	stw sp+20, r3
	stw r1+%lo(pangle), r3
	lui r1, %hi(alive)
	stw sp+176, r1
	stb r1+%lo(alive), r17
	lui r1, %hi(invuln)
	addi r3, r0, 60
	stw sp+132, r1
	stw sp+16, r3
	stw r1+%lo(invuln), r3
	jal r31, spawn_wave
	lui r1, %hi(test_mode)
	stw sp+160, r1
	ldbu r1, r1+%lo(test_mode)
	bne r1, r0, .LBB0_22
.LBB0_18:
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	jal r31, printf
	jal r31, tube_info
	andi r1, r1, 256
	bne r1, r0, .LBB0_22
.LBB0_19:
	lui r1, 5
	addi r13, r1, -480
	addi r12, r0, 149
	add r1, r0, r0
.LBB0_20:
	add r14, r1, r0
	add r3, r13, r0
	jal r31, usleep
	jal r31, tube_info
	andi r1, r1, 256
	bne r1, r0, .LBB0_22
.LBB0_21:
	addi r1, r14, 1
	bltu r14, r12, .LBB0_20
.LBB0_22:
	addi r1, sp, 200
	addi r3, r1, 2
	stw sp+52, r3
	addi r3, r1, 2
	stw sp+196, r3
	addi r1, r1, 2
	stw sp+32, r1
	addi r1, sp, 268
	addi r1, r1, 2
	stw sp+28, r1
	lui r12, %hi(held_fire)
	lui r1, 16
	stw sp+192, r1
	addi r13, r1, -33
	addi r14, r0, 258
	addi r15, r0, 65
	addi r18, r0, 112
	addi r19, r0, 80
	addi r20, r0, 27
	addi r22, r0, 32
	lui r1, %hi(fire_latch)
	stw sp+136, r1
	addi r23, r0, 81
	addi r24, r0, 104
	lui r1, %hi(hyper_latch)
	stw sp+140, r1
	addi r25, r0, 257
	addi r26, r0, 113
	addi r27, r0, 256
	lui r1, %hi(held_up)
	stw sp+152, r1
	lui r1, %hi(tap_up)
	stw sp+156, r1
	addi r1, r0, 259
	stw sp+172, r1
	lui r1, %hi(held_right)
	stw sp+148, r1
	stw sp+112, r28
	stw sp+108, r12
	stw sp+104, r13
	stw sp+100, r14
	stw sp+96, r15
	stw sp+92, r18
	stw sp+88, r19
	stw sp+84, r20
	stw sp+80, r22
	stw sp+76, r23
	stw sp+72, r24
	stw sp+68, r25
	stw sp+64, r26
	stw sp+128, r27
	jal r0, .LBB0_24
.LBB0_23:
	lui r1, 8
	addi r3, r1, 232
	jal r31, usleep
	ldw r12, sp+108
	ldw r13, sp+104
	ldw r14, sp+100
	ldw r15, sp+96
	ldw r18, sp+92
	ldw r19, sp+88
	ldw r20, sp+84
	ldw r22, sp+80
	ldw r23, sp+76
	ldw r24, sp+72
	ldw r25, sp+68
	ldw r26, sp+64
.LBB0_24:
	ldw r1, sp+160
	ldbu r1, r1+%lo(test_mode)
	sub r1, r0, r1
	andi r1, r1, 68
	xori r4, r1, 64
	addi r3, sp, 200
	jal r31, tube_keys
	lui r3, %hi(held_left)
	lui fp, %hi(tap_left)
	lui r4, %hi(tap_right)
	stw sp+164, r4
	blt r1, r17, .LBB0_56
.LBB0_25:
	ldw r5, r12+%lo(held_fire)
	ldw r4, sp+52
	jal r0, .LBB0_28
.LBB0_26:
	ldbu r6, r4+0
	beq r6, r0, .LBB0_27
	jal r0, .LBB0_161
.LBB0_27:
	addi r1, r1, -1
	addi r4, r4, 4
	beq r1, r0, .LBB0_56
.LBB0_28:
	ldhu r7, r4+-2
	and r8, r7, r13
	add r6, r14, r0
	beq r8, r15, .LBB0_32
.LBB0_29:
	addi r6, r0, 68
	beq r8, r6, .LBB0_40
.LBB0_30:
	addi r9, r0, 87
	add r6, r7, r0
	bne r8, r9, .LBB0_32
.LBB0_31:
	add r6, r27, r0
.LBB0_32:
	bgt r6, r18, .LBB0_41
.LBB0_33:
	bgt r6, r19, .LBB0_46
.LBB0_34:
	beq r6, r20, .LBB0_26
.LBB0_35:
	bne r6, r22, .LBB0_27
.LBB0_36:
	ldbu r6, r4+0
	beq r6, r0, .LBB0_39
.LBB0_37:
	bne r5, r0, .LBB0_39
.LBB0_38:
	ldw r5, sp+136
	stb r5+%lo(fire_latch), r17
.LBB0_39:
	stw r12+%lo(held_fire), r6
	add r5, r6, r0
	jal r0, .LBB0_27
.LBB0_40:
	ldw r6, sp+172
	ble r6, r18, .LBB0_33
.LBB0_41:
	bgt r6, r25, .LBB0_50
.LBB0_42:
	beq r6, r26, .LBB0_26
.LBB0_43:
	bne r6, r27, .LBB0_27
.LBB0_44:
	ldbu r6, r4+0
	ldw r7, sp+152
	stw r7+%lo(held_up), r6
	beq r6, r0, .LBB0_27
.LBB0_45:
	ldw r6, sp+156
	stb r6+%lo(tap_up), r17
	jal r0, .LBB0_27
.LBB0_46:
	beq r6, r23, .LBB0_26
.LBB0_47:
	bne r6, r24, .LBB0_27
.LBB0_48:
	ldbu r6, r4+0
	beq r6, r0, .LBB0_27
.LBB0_49:
	ldw r6, sp+140
	stb r6+%lo(hyper_latch), r17
	jal r0, .LBB0_27
.LBB0_50:
	ldw r7, sp+172
	beq r6, r7, .LBB0_54
.LBB0_51:
	addi r7, r0, 258
	bne r6, r7, .LBB0_27
.LBB0_52:
	ldbu r6, r4+0
	stw r3+%lo(held_left), r6
	beq r6, r0, .LBB0_27
.LBB0_53:
	stb fp+%lo(tap_left), r17
	jal r0, .LBB0_27
.LBB0_54:
	ldbu r6, r4+0
	ldw r7, sp+148
	stw r7+%lo(held_right), r6
	beq r6, r0, .LBB0_27
.LBB0_55:
	ldw r6, sp+164
	stb r6+%lo(tap_right), r17
	jal r0, .LBB0_27
.LBB0_56:
	lui r1, %hi(game_over)
	stw sp+144, r1
	ldbu r1, r1+%lo(game_over)
	lui r25, 1048575
	lui r26, 1
	lui r22, %hi(shots+80)
	addi r22, r22, %lo(shots+80)
	lui r4, %hi(debris+480)
	addi r4, r4, %lo(debris+480)
	stw sp+188, r4
	lui r28, %hi(rocks)
	addi r28, r28, %lo(rocks)
	lui r27, %hi(shots+84)
	addi r27, r27, %lo(shots+84)
	lui r24, %hi(shots+88)
	addi r24, r24, %lo(shots+88)
	lui r4, %hi(debris+484)
	addi r4, r4, %lo(debris+484)
	stw sp+184, r4
	lui r4, %hi(debris+488)
	addi r4, r4, %lo(debris+488)
	stw sp+180, r4
	bne r1, r17, .LBB0_58
.LBB0_57:
	ldw r4, sp+160
	ldbu r4, r4+%lo(test_mode)
	andi r4, r4, 1
	beq r4, r0, .LBB0_119
.LBB0_58:
	stw sp+60, fp
	lui r4, %hi(frame)
	ldw r5, r4+%lo(frame)
	addi r5, r5, 1
	stw r4+%lo(frame), r5
	ldw r4, sp+176
	ldbu r4, r4+%lo(alive)
	lui fp, 256
	bne r4, r17, .LBB0_75
.LBB0_59:
	ldw r1, r3+%lo(held_left)
	ldw lr, sp+44
	ldw r6, sp+48
	bne r1, r0, .LBB0_61
.LBB0_60:
	ldw r1, sp+60
	ldbu r1, r1+%lo(tap_left)
	andi r1, r1, 1
	beq r1, r0, .LBB0_62
.LBB0_61:
	ldw r3, sp+56
	ldw r1, r3+%lo(pangle)
	addi r1, r1, 1
	andi r1, r1, 63
	stw r3+%lo(pangle), r1
.LBB0_62:
	ldw r1, sp+148
	ldw r1, r1+%lo(held_right)
	bne r1, r0, .LBB0_64
.LBB0_63:
	ldw r1, sp+164
	ldbu r1, r1+%lo(tap_right)
	andi r1, r1, 1
	beq r1, r0, .LBB0_65
.LBB0_64:
	ldw r3, sp+56
	ldw r1, r3+%lo(pangle)
	addi r1, r1, -1
	andi r1, r1, 63
	stw r3+%lo(pangle), r1
.LBB0_65:
	ldw r1, sp+152
	ldw r1, r1+%lo(held_up)
	sne r1, r1, r16
	ldw r3, sp+156
	ldbu r3, r3+%lo(tap_up)
	or  r1, r1, r3
	andi r1, r1, 1
	lui r3, %hi(thrusting)
	stw r3+%lo(thrusting), r1
	beq r1, r0, .LBB0_67
.LBB0_66:
	ldw r1, sp+56
	ldw r1, r1+%lo(pangle)
	slli r3, r1, 1
	lui r4, %hi(COS64)
	addi r4, r4, %lo(COS64)
	add r3, r3, r4
	ldh r3, r3+0
	slli r5, r3, 1
	add r3, r5, r3
	ldw r5, r6+%lo(pvx)
	add r3, r3, r5
	stw r6+%lo(pvx), r3
	addi r1, r1, 48
	andi r1, r1, 63
	slli r1, r1, 1
	add r1, r1, r4
	ldh r1, r1+0
	slli r3, r1, 1
	add r1, r3, r1
	ldw r3, lr+%lo(pvy)
	add r1, r1, r3
	stw lr+%lo(pvy), r1
.LBB0_67:
	ldw r1, r6+%lo(pvx)
	srai r3, r1, 31
	srli r3, r3, 26
	add r3, r1, r3
	srai r3, r3, 6
	sub r4, r1, r3
	stw r6+%lo(pvx), r4
	ldw r1, lr+%lo(pvy)
	srai r3, r1, 31
	srli r3, r3, 26
	add r3, r1, r3
	srai r3, r3, 6
	sub r1, r1, r3
	stw lr+%lo(pvy), r1
	addi r3, r26, 905
	blt r4, r3, .LBB0_69
.LBB0_68:
	addi r4, r26, 904
	stw r6+%lo(pvx), r4
.LBB0_69:
	ldw r5, r6+%lo(pvx)
	addi r4, r25, -905
	ble r5, r4, .LBB0_78
.LBB0_70:
	bge r1, r3, .LBB0_79
.LBB0_71:
	ldw r1, lr+%lo(pvy)
	bgt r1, r4, .LBB0_73
.LBB0_72:
	addi r1, r25, -904
	stw lr+%lo(pvy), r1
.LBB0_73:
	ldw r4, sp+120
	ldw r1, r4+%lo(px)
	ldw r3, r6+%lo(pvx)
	add r1, r3, r1
	addi r3, fp, -1
	and r1, r1, r3
	stw r4+%lo(px), r1
	ldw r5, sp+124
	ldw r1, r5+%lo(py)
	ldw r4, lr+%lo(pvy)
	add r1, r4, r1
	and r1, r1, r3
	stw r5+%lo(py), r1
	ldw r1, sp+132
	ldw r1, r1+%lo(invuln)
	blt r1, r17, .LBB0_85
.LBB0_74:
	addi r1, r1, -1
	ldw r3, sp+132
	stw r3+%lo(invuln), r1
	jal r0, .LBB0_85
.LBB0_75:
	ldw lr, sp+44
	ldw r12, sp+48
	bne r1, r0, .LBB0_85
.LBB0_76:
	lui r1, %hi(respawn_timer)
	ldw r3, r1+%lo(respawn_timer)
	ble r3, r0, .LBB0_80
.LBB0_77:
	addi r3, r3, -1
	stw r1+%lo(respawn_timer), r3
	jal r0, .LBB0_85
.LBB0_78:
	addi r5, r25, -904
	stw r6+%lo(pvx), r5
	blt r1, r3, .LBB0_71
.LBB0_79:
	addi r1, r26, 904
	stw lr+%lo(pvy), r1
	ldw r1, lr+%lo(pvy)
	ble r1, r4, .LBB0_72
	jal r0, .LBB0_73
.LBB0_80:
	addi r1, r0, -784
	jal r0, .LBB0_82
.LBB0_81:
	addi r1, r1, 28
	beq r1, r0, .LBB0_84
.LBB0_82:
	lui r3, %hi(rocks+784)
	addi r3, r3, %lo(rocks+784)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB0_81
.LBB0_83:
	lui r3, %hi(rocks+788)
	addi r3, r3, %lo(rocks+788)
	add r3, r1, r3
	ldw r3, r3+0
	lui r4, %hi(rocks+792)
	addi r4, r4, %lo(rocks+792)
	add r4, r1, r4
	ldw r4, r4+0
	lui r5, %hi(rocks+804)
	addi r5, r5, %lo(rocks+804)
	add r5, r1, r5
	ldw r5, r5+0
	seq r6, r5, r11
	seq r5, r5, r17
	sub r5, r0, r5
	andi r5, r5, 67
	xori r7, r5, 565
	sub r6, r0, r6
	xori r5, r5, 205
	and r5, r5, r6
	xor r5, r7, r5
	lui r6, 1048448
	add r7, r3, r6
	srai r8, r7, 31
	srli r8, r8, 24
	add r7, r7, r8
	srai r7, r7, 8
	add r6, r4, r6
	srai r8, r6, 31
	srli r8, r8, 24
	add r6, r6, r8
	srai r6, r6, 8
	addi r8, fp, 255
	sgt r3, r3, r8
	add r9, r7, r25
	xor r9, r9, r7
	sub r3, r0, r3
	and r3, r9, r3
	xor r3, r7, r3
	addi r7, r0, -2048
	slt r9, r3, r7
	add r10, r3, r26
	xor r10, r10, r3
	sub r9, r0, r9
	and r9, r10, r9
	xor r3, r3, r9
	sgt r4, r4, r8
	add r8, r6, r25
	xor r8, r8, r6
	sub r4, r0, r4
	and r4, r8, r4
	xor r4, r6, r4
	slt r6, r4, r7
	add r7, r4, r26
	xor r7, r7, r4
	sub r6, r0, r6
	and r6, r7, r6
	xor r4, r4, r6
	mul r3, r3, r3
	mul r4, r4, r4
	add r3, r4, r3
	mul r4, r5, r5
	bgeu r3, r4, .LBB0_81
	jal r0, .LBB0_85
.LBB0_84:
	ldw r1, sp+124
	stw r1+%lo(py), r21
	ldw r1, sp+120
	stw r1+%lo(px), r21
	stw lr+%lo(pvy), r0
	stw r12+%lo(pvx), r0
	ldw r1, sp+56
	ldw r3, sp+20
	stw r1+%lo(pangle), r3
	ldw r1, sp+176
	stb r1+%lo(alive), r17
	ldw r1, sp+132
	ldw r3, sp+16
	stw r1+%lo(invuln), r3
.LBB0_85:
	ldw r1, sp+136
	ldbu r1, r1+%lo(fire_latch)
	lui r13, %hi(shots+92)
	addi r13, r13, %lo(shots+92)
	lui r14, %hi(shots+96)
	addi r14, r14, %lo(shots+96)
	bne r1, r17, .LBB0_91
.LBB0_86:
	ldw r1, sp+136
	stb r1+%lo(fire_latch), r0
	ldw r1, sp+176
	ldbu r1, r1+%lo(alive)
	bne r1, r17, .LBB0_91
.LBB0_87:
	addi r1, r0, -80
.LBB0_88:
	add r3, r1, r22
	ldw r4, r3+0
	ble r4, r0, .LBB0_90
.LBB0_89:
	addi r1, r1, 20
	bne r1, r0, .LBB0_88
	jal r0, .LBB0_91
.LBB0_90:
	addi r4, r0, 34
	stw r3+0, r4
	ldw r3, sp+120
	ldw r3, r3+%lo(px)
	ldw r4, sp+56
	ldw r4, r4+%lo(pangle)
	slli r5, r4, 1
	lui r6, %hi(COS64)
	addi r6, r6, %lo(COS64)
	add r5, r5, r6
	ldh r5, r5+0
	addi r7, r0, 120
	mul r5, r5, r7
	add r3, r5, r3
	addi r8, fp, -1
	and r3, r3, r8
	add r9, r1, r27
	stw r9+0, r3
	ldw r3, sp+124
	ldw r3, r3+%lo(py)
	addi r4, r4, 48
	andi r4, r4, 63
	slli r4, r4, 1
	add r4, r4, r6
	ldh r4, r4+0
	mul r4, r4, r7
	add r3, r4, r3
	and r3, r3, r8
	add r6, r1, r24
	stw r6+0, r3
	ldw r3, sp+48
	ldw r3, r3+%lo(pvx)
	add r3, r3, r5
	add r5, r1, r13
	stw r5+0, r3
	ldw r3, lr+%lo(pvy)
	add r3, r3, r4
	add r1, r1, r14
	stw r1+0, r3
.LBB0_91:
	ldw r1, sp+140
	ldbu r1, r1+%lo(hyper_latch)
	bne r1, r17, .LBB0_95
.LBB0_92:
	ldw r1, sp+140
	stb r1+%lo(hyper_latch), r0
	ldw r1, sp+176
	ldbu r1, r1+%lo(alive)
	bne r1, r17, .LBB0_95
.LBB0_93:
	ldw r5, sp+24
	ldw r1, r5+%lo(rng_state)
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	srli r3, r1, 8
	addi r4, fp, -1
	and r3, r3, r4
	ldw r6, sp+120
	stw r6+%lo(px), r3
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	srli r3, r1, 8
	and r3, r3, r4
	ldw r4, sp+124
	stw r4+%lo(py), r3
	stw lr+%lo(pvy), r0
	ldw r3, sp+48
	stw r3+%lo(pvx), r0
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	stw r5+%lo(rng_state), r1
	andi r1, r1, 1792
	bne r1, r0, .LBB0_95
.LBB0_94:
	jal r31, kill_ship
.LBB0_95:
	addi r1, r0, -80
	jal r0, .LBB0_97
.LBB0_96:
	addi r1, r1, 20
	beq r1, r0, .LBB0_99
.LBB0_97:
	add r3, r1, r22
	ldw r4, r3+0
	blt r4, r17, .LBB0_96
.LBB0_98:
	addi r4, r4, -1
	stw r3+0, r4
	add r3, r1, r27
	ldw r4, r3+0
	add r5, r1, r13
	ldw r5, r5+0
	add r4, r5, r4
	addi r5, fp, -1
	and r4, r4, r5
	stw r3+0, r4
	add r3, r1, r24
	ldw r4, r3+0
	add r6, r1, r14
	ldw r6, r6+0
	add r4, r6, r4
	and r4, r4, r5
	stw r3+0, r4
	jal r0, .LBB0_96
.LBB0_99:
	addi r1, r0, -480
	jal r0, .LBB0_101
.LBB0_100:
	addi r1, r1, 20
	beq r1, r0, .LBB0_103
.LBB0_101:
	ldw r3, sp+188
	add r3, r1, r3
	ldw r4, r3+0
	blt r4, r17, .LBB0_100
.LBB0_102:
	addi r4, r4, -1
	stw r3+0, r4
	ldw r3, sp+184
	add r3, r1, r3
	ldw r4, r3+0
	lui r5, %hi(debris+492)
	addi r5, r5, %lo(debris+492)
	add r5, r1, r5
	ldw r5, r5+0
	add r4, r5, r4
	addi r5, fp, -1
	and r4, r4, r5
	stw r3+0, r4
	ldw r3, sp+180
	add r3, r1, r3
	ldw r4, r3+0
	lui r6, %hi(debris+496)
	addi r6, r6, %lo(debris+496)
	add r6, r1, r6
	ldw r6, r6+0
	add r4, r6, r4
	and r4, r4, r5
	stw r3+0, r4
	jal r0, .LBB0_100
.LBB0_103:
	ldw r1, sp+132
	ldw r20, r1+%lo(invuln)
	ldw r1, sp+120
	ldw r12, r1+%lo(px)
	ldw r1, sp+124
	ldw r1, r1+%lo(py)
	stw sp+168, r1
	add r14, r0, r0
	add r13, r0, r0
	jal r0, .LBB0_105
.LBB0_104:
	addi r13, r13, 1
	beq r13, r15, .LBB0_116
.LBB0_105:
	addi r15, r0, 28
	mul r1, r13, r15
	add r18, r1, r28
	ldw r1, r18+0
	beq r1, r0, .LBB0_104
.LBB0_106:
	addi r14, r14, 1
	ldw r1, r18+4
	ldw r3, r18+12
	add r1, r3, r1
	addi r3, fp, -1
	and r1, r1, r3
	stw r18+4, r1
	ldw r4, r18+8
	ldw r5, r18+16
	add r4, r5, r4
	and r3, r4, r3
	stw r18+8, r3
	addi r4, r0, -80
	jal r0, .LBB0_108
.LBB0_107:
	addi r4, r4, 20
	beq r4, r0, .LBB0_111
.LBB0_108:
	add r5, r4, r22
	ldw r6, r5+0
	blt r6, r17, .LBB0_107
.LBB0_109:
	add r6, r4, r27
	ldw r6, r6+0
	add r7, r4, r24
	ldw r7, r7+0
	ldw r8, r18+20
	seq r9, r8, r11
	seq r8, r8, r17
	sub r8, r0, r8
	andi r8, r8, 195
	xori r10, r8, 73
	sub r9, r0, r9
	xori r8, r8, 325
	and r8, r8, r9
	xor r8, r10, r8
	sub r6, r6, r1
	srai r9, r6, 31
	srli r9, r9, 24
	add r9, r6, r9
	srai r9, r9, 8
	sub r7, r7, r3
	srai r10, r7, 31
	srli r10, r10, 24
	add r10, r7, r10
	srai r10, r10, 8
	addi lr, r21, 255
	sgt r6, r6, lr
	add r19, r9, r25
	xor r19, r19, r9
	sub r6, r0, r6
	and r6, r19, r6
	xor r6, r9, r6
	addi r9, r0, -2048
	slt r19, r6, r9
	add r23, r6, r26
	xor r23, r23, r6
	sub r19, r0, r19
	and r19, r23, r19
	xor r6, r6, r19
	sgt r7, r7, lr
	add lr, r10, r25
	xor lr, lr, r10
	sub r7, r0, r7
	and r7, lr, r7
	xor r7, r10, r7
	slt r9, r7, r9
	add r10, r7, r26
	xor r10, r10, r7
	sub r9, r0, r9
	and r9, r10, r9
	xor r7, r7, r9
	mul r6, r6, r6
	mul r7, r7, r7
	add r6, r7, r6
	mul r7, r8, r8
	bgeu r6, r7, .LBB0_107
.LBB0_110:
	stw r5+0, r0
	add r3, r13, r0
	jal r31, kill_rock
.LBB0_111:
	ldw r1, r18+0
	beq r1, r0, .LBB0_104
.LBB0_112:
	ldw r1, sp+176
	ldbu r1, r1+%lo(alive)
	andi r1, r1, 1
	beq r1, r0, .LBB0_104
.LBB0_113:
	bne r20, r0, .LBB0_104
.LBB0_114:
	ldw r1, r18+4
	ldw r3, r18+8
	ldw r4, r18+20
	seq r5, r4, r11
	seq r4, r4, r17
	sub r4, r0, r4
	andi r4, r4, 195
	xori r6, r4, 125
	sub r5, r0, r5
	xori r4, r4, 317
	and r4, r4, r5
	xor r4, r6, r4
	sub r1, r12, r1
	srai r5, r1, 31
	srli r5, r5, 24
	add r5, r1, r5
	srai r5, r5, 8
	ldw r6, sp+168
	sub r3, r6, r3
	srai r6, r3, 31
	srli r6, r6, 24
	add r6, r3, r6
	srai r6, r6, 8
	addi r7, r21, 255
	sgt r1, r1, r7
	add r8, r5, r25
	xor r8, r8, r5
	sub r1, r0, r1
	and r1, r8, r1
	xor r1, r5, r1
	addi r5, r0, -2048
	slt r8, r1, r5
	add r9, r1, r26
	xor r9, r9, r1
	sub r8, r0, r8
	and r8, r9, r8
	xor r1, r1, r8
	sgt r3, r3, r7
	add r7, r6, r25
	xor r7, r7, r6
	sub r3, r0, r3
	and r3, r7, r3
	xor r3, r6, r3
	slt r5, r3, r5
	add r6, r3, r26
	xor r6, r6, r3
	sub r5, r0, r5
	and r5, r6, r5
	xor r3, r3, r5
	mul r1, r1, r1
	mul r3, r3, r3
	add r1, r3, r1
	mul r3, r4, r4
	bgeu r1, r3, .LBB0_104
.LBB0_115:
	add r3, r13, r0
	jal r31, kill_rock
	jal r31, kill_ship
	jal r0, .LBB0_104
.LBB0_116:
	ldw fp, sp+60
	bne r14, r0, .LBB0_119
.LBB0_117:
	ldw r1, sp+144
	ldbu r1, r1+%lo(game_over)
	andi r1, r1, 1
	bne r1, r0, .LBB0_119
.LBB0_118:
	ldw r3, sp+36
	ldw r1, r3+%lo(wave)
	addi r1, r1, 1
	stw r3+%lo(wave), r1
	jal r31, spawn_wave
.LBB0_119:
	ldw r1, sp+156
	stb r1+%lo(tap_up), r0
	ldw r1, sp+164
	stb r1+%lo(tap_right), r0
	stb fp+%lo(tap_left), r0
	lui r13, %hi(list)
	addi r13, r13, %lo(list)
	lui r1, 331776
	addi r1, r1, -1
	stw r13+0, r1
	stw sp+264, r11
	lui r1, 262144
	stw sp+168, r1
	addi r1, r1, 200
	lui r3, %hi(list+4)
	addi r3, r3, %lo(list+4)
	stw sp+164, r3
	stw r3+0, r1
	lui r1, %hi(score)
	ldw r8, r1+%lo(score)
	addi r6, r26, -196
	addi r4, sp, 264
	addi r5, r0, 120
	addi r7, r0, 14
	add r3, r13, r0
	add r9, r11, r0
	jal r31, vfont_uint
	ldw r1, sp+116
	ldw r7, r1+%lo(lives)
	lui r5, %hi(draw_ship.base+48)
	addi r5, r5, %lo(draw_ship.base+48)
	lui r6, %hi(draw_ship.base+52)
	addi r6, r6, %lo(draw_ship.base+52)
	lui r4, 838861
	lui r1, 196608
	lui r3, 131072
	blt r7, r17, .LBB0_127
.LBB0_120:
	addi r8, r0, 8
	slt r8, r7, r8
	sub r8, r0, r8
	xori r7, r7, 8
	and r7, r7, r8
	xori r7, r7, 8
	ldw lr, sp+264
	add r8, r0, r0
.LBB0_121:
	add r9, lr, r0
	addi r10, r0, -48
	ldw lr, sp+196
.LBB0_122:
	add r14, r10, r5
	ldw r14, r14+0
	add r15, r10, r6
	ldhu r15, r15+0
	sub r15, r0, r15
	sth lr+-2, r15
	sth lr+0, r14
	addi r10, r10, 8
	addi lr, lr, 4
	bne r10, r0, .LBB0_122
.LBB0_123:
	lui r10, 9
	addi r10, r10, -1024
	mul r10, r8, r10
	srli r10, r10, 8
	addi lr, r10, 160
	slli r10, r9, 2
	add r14, r10, r13
	ldw r15, sp+196
	add r10, r0, r0
	add r18, r0, r0
.LBB0_124:
	addi r19, r4, -819
	mulhu r19, r18, r19
	srli r19, r19, 2
	addi r20, r0, -20
	mul r19, r19, r20
	add r19, r15, r19
	ldh r20, r19+-2
	add r20, lr, r20
	ldh r19, r19+0
	sgt r23, r20, r16
	sub r23, r0, r23
	and r20, r20, r23
	xori r23, r20, 4095
	addi fp, r26, -1
	slt r20, r20, fp
	sub r20, r0, r20
	and r20, r23, r20
	xori r20, r20, 4095
	addi r23, r25, 396
	xor r12, r19, r23
	sgt r19, r19, r23
	sub r19, r0, r19
	and r12, r12, r19
	xor r12, r12, r23
	addi r19, r26, -396
	add r12, r12, r19
	xori r19, r12, 4095
	sltu r12, r12, fp
	sub r12, r0, r12
	and r12, r19, r12
	xori r12, r12, 4095
	seq r19, r10, r16
	sub r19, r0, r19
	and r19, r19, r1
	xor r19, r19, r3
	slli r20, r20, 16
	or  r19, r20, r19
	slli r12, r12, 4
	or  r12, r19, r12
	stw r14+0, r12
	addi r18, r18, 1
	addi r14, r14, 4
	addi r10, r10, -1
	addi r15, r15, 4
	addi r12, r0, -6
	bne r10, r12, .LBB0_124
.LBB0_125:
	addi r8, r8, 1
	sub lr, r9, r10
	bne r8, r7, .LBB0_121
.LBB0_126:
	sub r7, r9, r10
	stw sp+264, r7
.LBB0_127:
	ldw r8, sp+264
	addi r7, r8, 1
	stw sp+264, r7
	slli r8, r8, 2
	add r8, r8, r13
	ldw r9, sp+168
	addi r9, r9, 180
	stw r8+0, r9
	add r8, r0, r0
	jal r0, .LBB0_129
.LBB0_128:
	addi r8, r8, 1
	beq r8, r9, .LBB0_133
.LBB0_129:
	addi r9, r0, 28
	mul r10, r8, r9
	add r10, r10, r28
	ldw lr, r10+0
	beq lr, r0, .LBB0_128
.LBB0_130:
	ldw lr, r10+4
	ldw r12, r10+8
	ldw r18, r10+24
	ldw r10, r10+20
	seq r14, r10, r11
	seq r10, r10, r17
	sub r10, r0, r10
	andi r10, r10, 195
	xori r10, r10, 65
	sub r14, r0, r14
	ori  r15, r10, 260
	and r14, r15, r14
	xor r10, r10, r14
	srai r14, lr, 31
	srli r14, r14, 24
	add lr, lr, r14
	srai lr, lr, 8
	srai r14, r12, 31
	srli r14, r14, 24
	add r12, r12, r14
	srai r14, r12, 8
	slli r12, r7, 2
	add r15, r12, r13
	addi r12, r0, 48
	mul r12, r18, r12
	lui r18, %hi(rock_shape+2)
	addi r18, r18, %lo(rock_shape+2)
	add r18, r12, r18
	add r20, r0, r0
	add r25, r0, r0
.LBB0_131:
	lui r12, 699051
	addi r12, r12, -1365
	mulhu r12, r25, r12
	srli r12, r12, 3
	addi r19, r0, -48
	mul r12, r12, r19
	add r12, r18, r12
	ldh r19, r12+-2
	mul r19, r10, r19
	srai r23, r19, 31
	srli r23, r23, 24
	add r19, r19, r23
	srai r19, r19, 8
	add r19, r19, lr
	ldh r12, r12+0
	mul r12, r10, r12
	srai r23, r12, 31
	srli r23, r23, 24
	add r12, r12, r23
	srai r12, r12, 8
	add r12, r12, r14
	sgt r23, r19, r16
	sub r23, r0, r23
	and r19, r19, r23
	xori r23, r19, 4095
	addi fp, r26, -1
	slt r19, r19, fp
	sub r19, r0, r19
	and r19, r23, r19
	xori r19, r19, 4095
	sgt r23, r12, r16
	sub r23, r0, r23
	and r12, r12, r23
	xori r23, r12, 4095
	slt r12, r12, fp
	sub r12, r0, r12
	and r12, r23, r12
	xori r12, r12, 4095
	seq r23, r20, r16
	sub r23, r0, r23
	and r23, r23, r1
	xor r23, r23, r3
	slli r19, r19, 16
	or  r19, r19, r23
	slli r12, r12, 4
	or  r12, r19, r12
	stw r15+0, r12
	addi r25, r25, 1
	addi r15, r15, 4
	addi r20, r20, -1
	addi r18, r18, 4
	addi r12, r0, -13
	bne r20, r12, .LBB0_131
.LBB0_132:
	sub r7, r7, r20
	stw sp+264, r7
	jal r0, .LBB0_128
.LBB0_133:
	ldw r7, sp+264
	addi r9, r7, 1
	stw sp+264, r9
	slli r7, r7, 2
	add r8, r7, r13
	ldw r7, sp+168
	addi r7, r7, 255
	stw r8+0, r7
	addi r10, r0, -80
	ldw r28, sp+112
	jal r0, .LBB0_135
.LBB0_134:
	addi r10, r10, 20
	beq r10, r0, .LBB0_137
.LBB0_135:
	add r8, r10, r22
	ldw lr, r8+0
	lui r8, 65520
	blt lr, r17, .LBB0_134
.LBB0_136:
	add lr, r10, r27
	ldw lr, lr+0
	srai r12, lr, 31
	srli r12, r12, 24
	add lr, lr, r12
	slli lr, lr, 8
	and lr, lr, r8
	add r12, r10, r24
	ldw r12, r12+0
	srai r14, r12, 31
	srli r14, r14, 24
	add r12, r12, r14
	srli r12, r12, 4
	ldw r14, sp+192
	addi r14, r14, -16
	and r12, r12, r14
	or  lr, lr, r12
	or  lr, lr, r1
	addi r12, r9, 1
	stw sp+264, r12
	slli r9, r9, 2
	add r9, r9, r13
	stw r9+0, lr
	add r9, r12, r0
	jal r0, .LBB0_134
.LBB0_137:
	ldw r10, sp+264
	addi r9, r10, 1
	stw sp+264, r9
	slli r10, r10, 2
	add r10, r10, r13
	ldw lr, sp+168
	addi lr, lr, 120
	stw r10+0, lr
	addi r10, r0, -480
	ldw r27, sp+128
	ldw r15, sp+188
	ldw r18, sp+184
	ldw r19, sp+180
	jal r0, .LBB0_139
.LBB0_138:
	addi r10, r10, 20
	beq r10, r0, .LBB0_141
.LBB0_139:
	add lr, r10, r15
	ldw lr, lr+0
	blt lr, r17, .LBB0_138
.LBB0_140:
	add lr, r10, r18
	ldw lr, lr+0
	srai r12, lr, 31
	srli r12, r12, 24
	add lr, lr, r12
	slli lr, lr, 8
	and lr, lr, r8
	add r12, r10, r19
	ldw r12, r12+0
	srai r14, r12, 31
	srli r14, r14, 24
	add r12, r12, r14
	srli r12, r12, 4
	ldw r14, sp+192
	addi r14, r14, -16
	and r12, r12, r14
	or  lr, lr, r12
	or  lr, lr, r1
	addi r12, r9, 1
	stw sp+264, r12
	slli r9, r9, 2
	add r9, r9, r13
	stw r9+0, lr
	add r9, r12, r0
	jal r0, .LBB0_138
.LBB0_141:
	ldw r8, sp+264
	addi r9, r8, 1
	stw sp+264, r9
	slli r9, r8, 2
	add r10, r9, r13
	stw r10+0, r7
	ldw r10, sp+176
	ldbu r10, r10+%lo(alive)
	bne r10, r17, .LBB0_155
.LBB0_142:
	ldw r10, sp+132
	ldw r10, r10+%lo(invuln)
	blt r10, r17, .LBB0_144
.LBB0_143:
	lui r10, %hi(frame)
	ldbu r10, r10+%lo(frame)
	andi r10, r10, 2
	bne r10, r0, .LBB0_155
.LBB0_144:
	ldw r10, sp+120
	ldw r14, r10+%lo(px)
	ldw r10, sp+124
	ldw lr, r10+%lo(py)
	ldw r10, sp+56
	ldw r12, r10+%lo(pangle)
	lui r10, %hi(thrusting)
	ldw r20, r10+%lo(thrusting)
	lui r10, %hi(frame)
	ldw r19, r10+%lo(frame)
	slli r10, r12, 1
	lui r15, %hi(COS64)
	addi r15, r15, %lo(COS64)
	add r10, r10, r15
	ldh r10, r10+0
	addi r12, r12, 48
	andi r12, r12, 63
	slli r12, r12, 1
	add r12, r12, r15
	ldh r12, r12+0
	addi r15, r0, -48
	ldw r18, sp+32
.LBB0_145:
	add r22, r15, r5
	ldw r22, r22+0
	mul r23, r22, r10
	add r24, r15, r6
	ldw r24, r24+0
	mul r25, r24, r12
	sub r23, r23, r25
	srai r25, r23, 31
	srli r25, r25, 24
	add r23, r23, r25
	srli r23, r23, 8
	sth r18+-2, r23
	mul r22, r22, r12
	mul r23, r24, r10
	add r22, r23, r22
	srai r23, r22, 31
	srli r23, r23, 24
	add r22, r22, r23
	srli r22, r22, 8
	sth r18+0, r22
	addi r15, r15, 8
	addi r18, r18, 4
	bne r15, r0, .LBB0_145
.LBB0_146:
	srai r5, r14, 31
	srli r5, r5, 24
	add r5, r14, r5
	srai r5, r5, 8
	srli r6, lr, 8
	addi r18, r8, 2
	ldw lr, sp+164
	add r23, r9, lr
	lui lr, %hi(list+12)
	addi lr, lr, %lo(list+12)
	add r24, r9, lr
	add r22, r0, r0
	add lr, r0, r0
.LBB0_147:
	add r14, r24, r0
	addi r15, r4, -819
	mulhu r15, lr, r15
	srli r15, r15, 2
	addi r24, r0, -20
	mul r15, r15, r24
	addi r24, sp, 200
	add r15, r24, r15
	add r15, r15, r22
	ldh r24, r15+0
	add r24, r5, r24
	ldh r15, r15+2
	add r25, r6, r15
	sgt r15, r24, r16
	sub r15, r0, r15
	and r24, r24, r15
	xori r27, r24, 4095
	addi r15, r26, -1
	slt r24, r24, r15
	sub r24, r0, r24
	and r24, r27, r24
	xori r24, r24, 4095
	sgt r27, r25, r16
	sub r27, r0, r27
	and r25, r25, r27
	xori r27, r25, 4095
	slt r25, r25, r15
	sub r25, r0, r25
	and r25, r27, r25
	xori r25, r25, 4095
	seq r27, r22, r16
	sub r27, r0, r27
	and r27, r27, r1
	xor r27, r27, r3
	slli r24, r24, 16
	or  r24, r24, r27
	slli r25, r25, 4
	or  r24, r24, r25
	add r25, r23, r22
	stw r25+0, r24
	addi lr, lr, 1
	addi r18, r18, 1
	addi r22, r22, 4
	addi r24, r14, 4
	addi r25, r0, 24
	bne r22, r25, .LBB0_147
.LBB0_148:
	addi r4, r18, -1
	stw sp+264, r4
	ldw r27, sp+128
	beq r20, r0, .LBB0_155
.LBB0_149:
	andi r4, r19, 1
	beq r4, r0, .LBB0_155
.LBB0_150:
	addi r4, r0, -24
	ldw r19, sp+28
.LBB0_151:
	lui r20, %hi(draw_ship.fb+24)
	addi r20, r20, %lo(draw_ship.fb+24)
	add r20, r4, r20
	ldw r20, r20+0
	mul r23, r20, r10
	lui r24, %hi(draw_ship.fb+28)
	addi r24, r24, %lo(draw_ship.fb+28)
	add r24, r4, r24
	ldw r24, r24+0
	mul r25, r24, r12
	sub r23, r23, r25
	srai r25, r23, 31
	srli r25, r25, 24
	add r23, r23, r25
	srli r23, r23, 8
	sth r19+-2, r23
	mul r20, r20, r12
	mul r23, r24, r10
	add r20, r23, r20
	srai r23, r20, 31
	srli r23, r23, 24
	add r20, r20, r23
	srli r20, r20, 8
	sth r19+0, r20
	addi r4, r4, 8
	addi r19, r19, 4
	bne r4, r0, .LBB0_151
.LBB0_152:
	stw sp+264, r18
	add r4, r9, r22
	ldw r9, sp+164
	add r4, r4, r9
	ldw r9, sp+168
	addi r9, r9, 160
	stw r4+0, r9
	add r4, r0, r0
.LBB0_153:
	andi r9, r4, 1
	slli r9, r9, 2
	addi r10, sp, 268
	add r9, r10, r9
	ldh r10, r9+0
	add r10, r5, r10
	ldh r9, r9+2
	add r9, r6, r9
	sgt r12, r10, r16
	sub r12, r0, r12
	and r10, r10, r12
	xori r12, r10, 4095
	slt r10, r10, r15
	sub r10, r0, r10
	and r10, r12, r10
	xori r10, r10, 4095
	sgt r12, r9, r16
	sub r12, r0, r12
	and r9, r9, r12
	xori r12, r9, 4095
	slt r9, r9, r15
	sub r9, r0, r9
	and r9, r12, r9
	xori r9, r9, 4095
	seq r12, r4, r16
	sub r12, r0, r12
	and r12, r12, r1
	xor r12, r12, r3
	slli r10, r10, 16
	or  r10, r10, r12
	slli r9, r9, 4
	or  r9, r10, r9
	stw r14+0, r9
	addi r4, r4, 1
	addi r14, r14, 4
	bne r4, r28, .LBB0_153
.LBB0_154:
	add r1, r8, lr
	add r1, r1, r4
	addi r1, r1, 3
	stw sp+264, r1
	stw r14+0, r7
.LBB0_155:
	ldw r1, sp+144
	ldbu r1, r1+%lo(game_over)
	bne r1, r17, .LBB0_157
.LBB0_156:
	lui r3, %hi(list)
	addi r3, r3, %lo(list)
	lui r8, %hi(.L.str.8)
	addi r8, r8, %lo(.L.str.8)
	addi r6, r26, -1896
	addi r4, sp, 264
	addi r5, r0, 1500
	addi r7, r0, 20
	jal r31, vfont_text
.LBB0_157:
	ldw r1, sp+264
	addi r4, r1, 1
	stw sp+264, r4
	slli r1, r1, 2
	add r1, r1, r13
	stw r1+0, r0
	lui r12, %hi(frame)
	ldw r5, r12+%lo(frame)
	add r3, r13, r0
	jal r31, tube_present
	bne r1, r0, .LBB0_160
.LBB0_158:
	ldw r1, sp+160
	ldbu r1, r1+%lo(test_mode)
	beq r1, r17, .LBB0_159
	jal r0, .LBB0_23
.LBB0_159:
	ldw r1, r12+%lo(frame)
	ldw r3, sp+40
	ldw r12, sp+108
	ldw r13, sp+104
	ldw r14, sp+100
	ldw r15, sp+96
	ldw r18, sp+92
	ldw r19, sp+88
	ldw r20, sp+84
	ldw r22, sp+80
	ldw r23, sp+76
	ldw r24, sp+72
	ldw r25, sp+68
	ldw r26, sp+64
	bgeu r1, r3, .LBB0_161
	jal r0, .LBB0_24
.LBB0_160:
	lui r3, %hi(.L.str.6)
	addi r3, r3, %lo(.L.str.6)
	jal r31, printf
.LBB0_161:
	ldw r1, sp+160
	ldbu r1, r1+%lo(test_mode)
	bne r1, r17, .LBB0_165
.LBB0_162:
	addi r1, r0, -784
	lui r3, %hi(rocks+784)
	addi r3, r3, %lo(rocks+784)
	add r8, r0, r0
.LBB0_163:
	add r4, r1, r3
	ldw r4, r4+0
	add r8, r4, r8
	addi r1, r1, 28
	bne r1, r0, .LBB0_163
.LBB0_164:
	lui r1, %hi(frame)
	ldw r4, r1+%lo(frame)
	lui r1, %hi(score)
	ldw r5, r1+%lo(score)
	ldw r1, sp+116
	ldw r6, r1+%lo(lives)
	ldw r1, sp+36
	ldw r7, r1+%lo(wave)
	lui r3, %hi(.L.str.7)
	addi r3, r3, %lo(.L.str.7)
	jal r31, printf
.LBB0_165:
	jal r31, tube_cleanup
	add r1, r0, r0
	jal r0, .LBB0_14
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
                                        # -- End function
	.p2align	2                               # -- Begin function spawn_wave
	.type	spawn_wave,@function
spawn_wave:                             # @spawn_wave
# %bb.0:
	addi sp, sp, -64
	stw sp+0, lr
	stw sp+60, r11
	stw sp+56, r12
	stw sp+52, r13
	stw sp+48, r14
	stw sp+44, r15
	stw sp+40, r16
	stw sp+36, r17
	stw sp+32, r18
	stw sp+28, r19
	stw sp+24, r20
	stw sp+20, r21
	lui r1, %hi(wave)
	ldw r3, r1+%lo(wave)
	addi r1, r0, -2
	blt r3, r1, .LBB1_11
.LBB1_1:
	lui r1, %hi(rng_state)
	ldw r15, r1+%lo(rng_state)
	addi r4, r0, 3
	slt r4, r3, r4
	sub r4, r0, r4
	xori r3, r3, 3
	and r3, r3, r4
	xori r3, r3, 3
	addi r3, r3, 2
	lui r4, 128
	addi r5, r4, -256
	lui r6, 64
	lui r7, 256
	addi r7, r7, -1
	lui r8, %hi(rocks)
	addi r8, r8, %lo(rocks)
	addi r9, r0, 1
	lui r10, %hi(rocks+20)
	addi r10, r10, %lo(rocks+20)
	addi lr, r0, 2
	lui r11, 349525
	addi r11, r11, 1366
	lui r12, %hi(rocks+24)
	addi r12, r12, %lo(rocks+24)
	lui r13, %hi(rocks+4)
	addi r13, r13, %lo(rocks+4)
	add r16, r0, r0
	jal r0, .LBB1_4
.LBB1_2:
	stw r19+0, r9
	add r19, r16, r10
	stw r19+0, lr
	slli r19, r15, 13
	xor r15, r19, r15
	srli r19, r15, 17
	xor r15, r19, r15
	slli r19, r15, 5
	xor r15, r19, r15
	srli r19, r15, 8
	mulhu r20, r19, r11
	slli r21, r20, 1
	add r20, r21, r20
	sub r19, r19, r20
	add r20, r16, r12
	stw r20+0, r19
	add r19, r16, r13
	stw r19+0, r18
	lui r18, %hi(rocks+8)
	addi r18, r18, %lo(rocks+8)
	add r18, r16, r18
	stw r18+0, r17
	slli r17, r15, 13
	xor r15, r17, r15
	srli r17, r15, 17
	xor r15, r17, r15
	slli r17, r15, 5
	xor r15, r17, r15
	srli r17, r15, 8
	lui r18, 2731
	addi r18, r18, -1365
	mulhu r19, r17, r18
	srli r19, r19, 3
	lui r20, 1
	addi r20, r20, -1024
	mul r19, r19, r20
	sub r17, r17, r19
	addi r17, r17, -1536
	lui r19, %hi(rocks+12)
	addi r19, r19, %lo(rocks+12)
	add r19, r16, r19
	stw r19+0, r17
	slli r17, r15, 13
	xor r15, r17, r15
	srli r17, r15, 17
	xor r15, r17, r15
	slli r17, r15, 5
	xor r15, r17, r15
	srli r17, r15, 8
	mulhu r18, r17, r18
	srli r18, r18, 3
	mul r18, r18, r20
	sub r17, r17, r18
	addi r17, r17, -1536
	lui r18, %hi(rocks+16)
	addi r18, r18, %lo(rocks+16)
	add r16, r16, r18
	stw r16+0, r17
.LBB1_3:
	addi r16, r14, 1
	beq r14, r3, .LBB1_10
.LBB1_4:
	add r14, r16, r0
	slli r16, r15, 13
	xor r15, r16, r15
	srli r16, r15, 17
	xor r15, r16, r15
	slli r16, r15, 5
	xor r16, r16, r15
	slli r15, r16, 13
	xor r15, r15, r16
	srli r17, r15, 17
	xor r15, r17, r15
	slli r17, r15, 5
	xor r17, r17, r15
	slli r15, r17, 13
	xor r15, r15, r17
	srli r18, r15, 17
	xor r15, r18, r15
	slli r18, r15, 5
	xor r15, r18, r15
	andi r18, r15, 256
	bne r18, r0, .LBB1_6
.LBB1_5:
	and r16, r16, r5
	add r16, r16, r6
	srli r17, r17, 8
	and r17, r17, r7
	jal r0, .LBB1_7
.LBB1_6:
	slli r16, r15, 13
	xor r15, r16, r15
	srli r16, r15, 17
	xor r15, r16, r15
	slli r16, r15, 5
	xor r15, r16, r15
	srli r16, r15, 8
	and r16, r16, r7
	slli r17, r15, 13
	xor r15, r17, r15
	srli r17, r15, 17
	xor r15, r17, r15
	slli r17, r15, 5
	xor r15, r17, r15
	and r17, r15, r5
	add r17, r17, r6
.LBB1_7:
	xor r18, r16, r4
	add r16, r0, r0
.LBB1_8:
	add r19, r16, r8
	ldw r20, r19+0
	beq r20, r0, .LBB1_2
.LBB1_9:
	addi r16, r16, 28
	addi r19, r0, 784
	bne r16, r19, .LBB1_8
	jal r0, .LBB1_3
.LBB1_10:
	stw r1+%lo(rng_state), r15
.LBB1_11:
	ldw r21, sp+20
	ldw r20, sp+24
	ldw r19, sp+28
	ldw r18, sp+32
	ldw r17, sp+36
	ldw r16, sp+40
	ldw r15, sp+44
	ldw r14, sp+48
	ldw r13, sp+52
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.Lfunc_end1:
	.size	spawn_wave, .Lfunc_end1-spawn_wave
                                        # -- End function
	.p2align	2                               # -- Begin function spawn_rock
	.type	spawn_rock,@function
spawn_rock:                             # @spawn_rock
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	lui r1, %hi(rocks)
	addi r1, r1, %lo(rocks)
	addi r6, r0, 784
	add r12, r0, r0
.LBB2_1:
	add r7, r12, r1
	ldw r8, r7+0
	beq r8, r0, .LBB2_3
.LBB2_2:
	addi r12, r12, 28
	bne r12, r6, .LBB2_1
	jal r0, .LBB2_10
.LBB2_3:
	addi r1, r0, 3
	sub r1, r1, r3
	slli r6, r1, 1
	add r1, r6, r1
	addi r14, r1, 3
	addi r1, r0, 1
	stw r7+0, r1
	lui r1, %hi(rocks+20)
	addi r1, r1, %lo(rocks+20)
	add r1, r12, r1
	stw r1+0, r3
	lui r13, %hi(rng_state)
	ldw r1, r13+%lo(rng_state)
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	stw r13+%lo(rng_state), r1
	srli r3, r1, 8
	lui r6, 349525
	addi r6, r6, 1366
	mulhu r6, r3, r6
	slli r7, r6, 1
	add r6, r7, r6
	sub r3, r3, r6
	lui r6, %hi(rocks+24)
	addi r6, r6, %lo(rocks+24)
	add r6, r12, r6
	stw r6+0, r3
	lui r3, 256
	addi r3, r3, -1
	and r4, r4, r3
	lui r6, %hi(rocks+4)
	addi r6, r6, %lo(rocks+4)
	add r6, r12, r6
	stw r6+0, r4
	and r3, r5, r3
	lui r4, %hi(rocks+8)
	addi r4, r4, %lo(rocks+8)
	add r4, r12, r4
	stw r4+0, r3
	slli r11, r14, 9
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r15, r3, r1
	stw r13+%lo(rng_state), r15
	beq r11, r0, .LBB2_5
.LBB2_4:
	srli r3, r15, 8
	add r4, r11, r0
	jal r31, __umodsi3
	jal r0, .LBB2_6
.LBB2_5:
	add r1, r0, r0
.LBB2_6:
	slli r14, r14, 8
	sub r1, r1, r14
	lui r3, %hi(rocks+12)
	addi r3, r3, %lo(rocks+12)
	add r3, r12, r3
	stw r3+0, r1
	slli r1, r15, 13
	xor r1, r1, r15
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	stw r13+%lo(rng_state), r1
	beq r11, r0, .LBB2_8
.LBB2_7:
	srli r3, r1, 8
	add r4, r11, r0
	jal r31, __umodsi3
	jal r0, .LBB2_9
.LBB2_8:
	add r1, r0, r0
.LBB2_9:
	sub r1, r1, r14
	lui r3, %hi(rocks+16)
	addi r3, r3, %lo(rocks+16)
	add r3, r12, r3
	stw r3+0, r1
.LBB2_10:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end2:
	.size	spawn_rock, .Lfunc_end2-spawn_rock
                                        # -- End function
	.p2align	2                               # -- Begin function kill_rock
	.type	kill_rock,@function
kill_rock:                              # @kill_rock
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	addi r1, r0, 28
	mul r1, r3, r1
	lui r3, %hi(rocks)
	addi r3, r3, %lo(rocks)
	add r11, r1, r3
	lui r3, %hi(rocks+20)
	addi r3, r3, %lo(rocks+20)
	add r1, r1, r3
	ldw r1, r1+0
	addi r3, r0, 2
	seq r4, r1, r3
	addi r3, r0, 1
	seq r5, r1, r3
	sub r5, r0, r5
	andi r5, r5, 86
	xori r6, r5, 100
	sub r4, r0, r4
	xori r5, r5, 112
	and r4, r5, r4
	xor r4, r6, r4
	lui r5, %hi(score)
	ldw r6, r5+%lo(score)
	add r6, r4, r6
	stw r5+%lo(score), r6
	lui r4, %hi(next_life)
	ldw r5, r4+%lo(next_life)
	bltu r6, r5, .LBB3_2
.LBB3_1:
	lui r6, %hi(lives)
	ldw r7, r6+%lo(lives)
	addi r7, r7, 1
	stw r6+%lo(lives), r7
	lui r6, 2
	addi r6, r6, 1808
	add r5, r5, r6
	stw r4+%lo(next_life), r5
.LBB3_2:
	ldw r4, r11+4
	ldw r5, r11+8
	lui r6, %hi(rng_state)
	ldw r13, r6+%lo(rng_state)
	addi r7, r0, 6
	lui r8, %hi(debris+8)
	addi r8, r8, %lo(debris+8)
	lui r9, 104858
	addi r9, r9, -1638
	addi r10, r0, 10
	addi lr, r0, 22
	add r12, r0, r0
.LBB3_3:
	ldw r14, r8+-8
	bgt r14, r0, .LBB3_5
.LBB3_4:
	slli r14, r13, 13
	xor r13, r14, r13
	srli r14, r13, 17
	xor r13, r14, r13
	slli r14, r13, 5
	xor r13, r14, r13
	stw r6+%lo(rng_state), r13
	srli r14, r13, 8
	mulhu r15, r14, r9
	mul r15, r15, r10
	sub r14, r14, r15
	addi r14, r14, 12
	stw r8+-8, r14
	stw r8+-4, r4
	stw r8+0, r5
	slli r14, r13, 13
	xor r13, r14, r13
	srli r14, r13, 17
	xor r13, r14, r13
	slli r14, r13, 5
	xor r13, r14, r13
	stw r6+%lo(rng_state), r13
	srli r14, r13, 8
	andi r14, r14, 4095
	addi r14, r14, -2048
	stw r8+4, r14
	slli r14, r13, 13
	xor r13, r14, r13
	srli r14, r13, 17
	xor r13, r14, r13
	slli r14, r13, 5
	xor r13, r14, r13
	stw r6+%lo(rng_state), r13
	srli r14, r13, 8
	andi r14, r14, 4095
	addi r14, r14, -2048
	stw r8+8, r14
	addi r7, r7, -1
.LBB3_5:
	bgtu r12, lr, .LBB3_7
.LBB3_6:
	addi r12, r12, 1
	addi r8, r8, 20
	bgt r7, r0, .LBB3_3
.LBB3_7:
	blt r1, r3, .LBB3_9
.LBB3_8:
	addi r3, r1, -1
	jal r31, spawn_rock
	ldw r1, r11+20
	addi r3, r1, -1
	ldw r4, r11+4
	ldw r5, r11+8
	jal r31, spawn_rock
.LBB3_9:
	stw r11+0, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end3:
	.size	kill_rock, .Lfunc_end3-kill_rock
                                        # -- End function
	.p2align	2                               # -- Begin function kill_ship
	.type	kill_ship,@function
kill_ship:                              # @kill_ship
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	lui r1, %hi(px)
	ldw r1, r1+%lo(px)
	lui r3, %hi(py)
	ldw r3, r3+%lo(py)
	lui r4, %hi(rng_state)
	ldw lr, r4+%lo(rng_state)
	addi r5, r0, 10
	lui r6, %hi(debris+8)
	addi r6, r6, %lo(debris+8)
	lui r7, 104858
	addi r7, r7, -1638
	addi r8, r0, 22
	add r9, r0, r0
	add r10, r5, r0
.LBB4_1:
	ldw r11, r6+-8
	bgt r11, r0, .LBB4_3
.LBB4_2:
	slli r11, lr, 13
	xor lr, r11, lr
	srli r11, lr, 17
	xor lr, r11, lr
	slli r11, lr, 5
	xor lr, r11, lr
	stw r4+%lo(rng_state), lr
	srli r11, lr, 8
	mulhu r12, r11, r7
	mul r12, r12, r5
	sub r11, r11, r12
	addi r11, r11, 12
	stw r6+-8, r11
	stw r6+-4, r1
	stw r6+0, r3
	slli r11, lr, 13
	xor lr, r11, lr
	srli r11, lr, 17
	xor lr, r11, lr
	slli r11, lr, 5
	xor lr, r11, lr
	stw r4+%lo(rng_state), lr
	srli r11, lr, 8
	andi r11, r11, 4095
	addi r11, r11, -2048
	stw r6+4, r11
	slli r11, lr, 13
	xor lr, r11, lr
	srli r11, lr, 17
	xor lr, r11, lr
	slli r11, lr, 5
	xor lr, r11, lr
	stw r4+%lo(rng_state), lr
	srli r11, lr, 8
	andi r11, r11, 4095
	addi r11, r11, -2048
	stw r6+8, r11
	addi r10, r10, -1
.LBB4_3:
	bgtu r9, r8, .LBB4_5
.LBB4_4:
	addi r9, r9, 1
	addi r6, r6, 20
	bgt r10, r0, .LBB4_1
.LBB4_5:
	lui r1, %hi(alive)
	stb r1+%lo(alive), r0
	lui r1, %hi(lives)
	ldw r3, r1+%lo(lives)
	addi r4, r3, -1
	stw r1+%lo(lives), r4
	lui r1, %hi(respawn_timer)
	addi r4, r0, 60
	stw r1+%lo(respawn_timer), r4
	addi r1, r0, 1
	bgt r3, r1, .LBB4_7
.LBB4_6:
	lui r3, %hi(game_over)
	stb r3+%lo(game_over), r1
.LBB4_7:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end4:
	.size	kill_ship, .Lfunc_end4-kill_ship
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"--frames"
	.size	.L.str, 9

	.type	test_mode,@object               # @test_mode
	.local	test_mode
	.comm	test_mode,1,4
	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"--seed"
	.size	.L.str.1, 7

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"usage: asteroids [--frames N --seed S]\n"
	.size	.L.str.2, 40

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"no tube: attach a screen\n"
	.size	.L.str.3, 26

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"open-fail\n"
	.size	.L.str.4, 11

	.type	rng_state,@object               # @rng_state
	.data
	.p2align	2, 0x0
rng_state:
	.word	1                               # 0x1
	.size	rng_state, 4

	.type	lives,@object                   # @lives
	.local	lives
	.comm	lives,4,4
	.type	wave,@object                    # @wave
	.local	wave
	.comm	wave,4,4
	.type	next_life,@object               # @next_life
	.local	next_life
	.comm	next_life,4,4
	.type	.L.str.5,@object                # @.str.5
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.5:
	.asciz	"asteroids: arrows/WASD steer, space fires, h hyperspace, q quits\n"
	.size	.L.str.5, 66

	.type	game_over,@object               # @game_over
	.local	game_over
	.comm	game_over,1,4
	.type	tap_up,@object                  # @tap_up
	.local	tap_up
	.comm	tap_up,1,4
	.type	tap_right,@object               # @tap_right
	.local	tap_right
	.comm	tap_right,1,4
	.type	tap_left,@object                # @tap_left
	.local	tap_left
	.comm	tap_left,1,4
	.type	list,@object                    # @list
	.local	list
	.comm	list,8192,4
	.type	frame,@object                   # @frame
	.local	frame
	.comm	frame,4,4
	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"present-fail\n"
	.size	.L.str.6, 14

	.type	rocks,@object                   # @rocks
	.local	rocks
	.comm	rocks,784,4
	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"report frames=%u score=%u lives=%d wave=%d rocks=%d\n"
	.size	.L.str.7, 53

	.type	score,@object                   # @score
	.local	score
	.comm	score,4,4
	.type	py,@object                      # @py
	.local	py
	.comm	py,4,4
	.type	px,@object                      # @px
	.local	px
	.comm	px,4,4
	.type	pvy,@object                     # @pvy
	.local	pvy
	.comm	pvy,4,4
	.type	pvx,@object                     # @pvx
	.local	pvx
	.comm	pvx,4,4
	.type	pangle,@object                  # @pangle
	.local	pangle
	.comm	pangle,4,4
	.type	alive,@object                   # @alive
	.local	alive
	.comm	alive,1,4
	.type	invuln,@object                  # @invuln
	.local	invuln
	.comm	invuln,4,4
	.type	held_left,@object               # @held_left
	.local	held_left
	.comm	held_left,4,4
	.type	held_right,@object              # @held_right
	.local	held_right
	.comm	held_right,4,4
	.type	held_up,@object                 # @held_up
	.local	held_up
	.comm	held_up,4,4
	.type	held_fire,@object               # @held_fire
	.local	held_fire
	.comm	held_fire,4,4
	.type	fire_latch,@object              # @fire_latch
	.local	fire_latch
	.comm	fire_latch,1,4
	.type	hyper_latch,@object             # @hyper_latch
	.local	hyper_latch
	.comm	hyper_latch,1,4
	.type	thrusting,@object               # @thrusting
	.local	thrusting
	.comm	thrusting,4,4
	.type	respawn_timer,@object           # @respawn_timer
	.local	respawn_timer
	.comm	respawn_timer,4,4
	.type	shots,@object                   # @shots
	.local	shots
	.comm	shots,80,4
	.type	debris,@object                  # @debris
	.local	debris
	.comm	debris,480,4
	.type	COS64,@object                   # @COS64
	.section	.rodata,"a",@progbits
	.p2align	1, 0x0
COS64:
	.short	256                             # 0x100
	.short	255                             # 0xff
	.short	251                             # 0xfb
	.short	245                             # 0xf5
	.short	237                             # 0xed
	.short	226                             # 0xe2
	.short	213                             # 0xd5
	.short	198                             # 0xc6
	.short	181                             # 0xb5
	.short	162                             # 0xa2
	.short	142                             # 0x8e
	.short	121                             # 0x79
	.short	98                              # 0x62
	.short	74                              # 0x4a
	.short	50                              # 0x32
	.short	25                              # 0x19
	.short	0                               # 0x0
	.short	65511                           # 0xffe7
	.short	65486                           # 0xffce
	.short	65462                           # 0xffb6
	.short	65438                           # 0xff9e
	.short	65415                           # 0xff87
	.short	65394                           # 0xff72
	.short	65374                           # 0xff5e
	.short	65355                           # 0xff4b
	.short	65338                           # 0xff3a
	.short	65323                           # 0xff2b
	.short	65310                           # 0xff1e
	.short	65299                           # 0xff13
	.short	65291                           # 0xff0b
	.short	65285                           # 0xff05
	.short	65281                           # 0xff01
	.short	65280                           # 0xff00
	.short	65281                           # 0xff01
	.short	65285                           # 0xff05
	.short	65291                           # 0xff0b
	.short	65299                           # 0xff13
	.short	65310                           # 0xff1e
	.short	65323                           # 0xff2b
	.short	65338                           # 0xff3a
	.short	65355                           # 0xff4b
	.short	65374                           # 0xff5e
	.short	65394                           # 0xff72
	.short	65415                           # 0xff87
	.short	65438                           # 0xff9e
	.short	65462                           # 0xffb6
	.short	65486                           # 0xffce
	.short	65511                           # 0xffe7
	.short	0                               # 0x0
	.short	25                              # 0x19
	.short	50                              # 0x32
	.short	74                              # 0x4a
	.short	98                              # 0x62
	.short	121                             # 0x79
	.short	142                             # 0x8e
	.short	162                             # 0xa2
	.short	181                             # 0xb5
	.short	198                             # 0xc6
	.short	213                             # 0xd5
	.short	226                             # 0xe2
	.short	237                             # 0xed
	.short	245                             # 0xf5
	.short	251                             # 0xfb
	.short	255                             # 0xff
	.size	COS64, 128

	.type	rock_shape,@object              # @rock_shape
	.p2align	1, 0x0
rock_shape:
	.short	256                             # 0x100
	.short	64                              # 0x40
	.short	160                             # 0xa0
	.short	224                             # 0xe0
	.short	16                              # 0x10
	.short	256                             # 0x100
	.short	65392                           # 0xff70
	.short	224                             # 0xe0
	.short	65280                           # 0xff00
	.short	96                              # 0x60
	.short	65312                           # 0xff20
	.short	65504                           # 0xffe0
	.short	65280                           # 0xff00
	.short	65376                           # 0xff60
	.short	65424                           # 0xff90
	.short	65280                           # 0xff00
	.short	16                              # 0x10
	.short	65328                           # 0xff30
	.short	144                             # 0x90
	.short	65280                           # 0xff00
	.short	256                             # 0x100
	.short	65392                           # 0xff70
	.short	208                             # 0xd0
	.short	65520                           # 0xfff0
	.short	224                             # 0xe0
	.short	96                              # 0x60
	.short	96                              # 0x60
	.short	256                             # 0x100
	.short	65472                           # 0xffc0
	.short	208                             # 0xd0
	.short	65328                           # 0xff30
	.short	240                             # 0xf0
	.short	65280                           # 0xff00
	.short	32                              # 0x20
	.short	65376                           # 0xff60
	.short	65472                           # 0xffc0
	.short	65296                           # 0xff10
	.short	65344                           # 0xff40
	.short	65472                           # 0xffc0
	.short	65280                           # 0xff00
	.short	64                              # 0x40
	.short	65312                           # 0xff20
	.short	208                             # 0xd0
	.short	65328                           # 0xff30
	.short	256                             # 0x100
	.short	65472                           # 0xffc0
	.short	176                             # 0xb0
	.short	16                              # 0x10
	.short	256                             # 0x100
	.short	32                              # 0x20
	.short	192                             # 0xc0
	.short	192                             # 0xc0
	.short	48                              # 0x30
	.short	224                             # 0xe0
	.short	65440                           # 0xffa0
	.short	256                             # 0x100
	.short	65296                           # 0xff10
	.short	144                             # 0x90
	.short	65280                           # 0xff00
	.short	65488                           # 0xffd0
	.short	65344                           # 0xff40
	.short	65312                           # 0xff20
	.short	65520                           # 0xfff0
	.short	65280                           # 0xff00
	.short	96                              # 0x60
	.short	65344                           # 0xff40
	.short	224                             # 0xe0
	.short	65296                           # 0xff10
	.short	240                             # 0xf0
	.short	65440                           # 0xffa0
	.short	160                             # 0xa0
	.short	65504                           # 0xffe0
	.size	rock_shape, 144

	.type	.L.str.8,@object                # @.str.8
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.8:
	.asciz	"GAME OVER"
	.size	.L.str.8, 10

	.type	draw_ship.base,@object          # @draw_ship.base
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
draw_ship.base:
	.word	90                              # 0x5a
	.word	0                               # 0x0
	.word	4294967226                      # 0xffffffba
	.word	55                              # 0x37
	.word	4294967251                      # 0xffffffd3
	.word	27                              # 0x1b
	.word	4294967251                      # 0xffffffd3
	.word	4294967269                      # 0xffffffe5
	.word	4294967226                      # 0xffffffba
	.word	4294967241                      # 0xffffffc9
	.word	90                              # 0x5a
	.word	0                               # 0x0
	.size	draw_ship.base, 48

	.type	draw_ship.fb,@object            # @draw_ship.fb
	.p2align	2, 0x0
draw_ship.fb:
	.word	4294967251                      # 0xffffffd3
	.word	18                              # 0x12
	.word	4294967201                      # 0xffffffa1
	.word	0                               # 0x0
	.word	4294967251                      # 0xffffffd3
	.word	4294967278                      # 0xffffffee
	.size	draw_ship.fb, 24

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git 77f07727152e39f179dfe39786aa3bcae0860790)"
	.section	".note.GNU-stack","",@progbits
