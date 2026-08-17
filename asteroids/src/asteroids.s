	.file	"asteroids.c"
	.text
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi sp, sp, -384
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 384
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
	addi r12, r0, 0
	addi r11, r0, 2
	add r27, r12, r0
	add r14, r12, r0
	blt r3, r11, .LBB0_9
.LBB0_1:
	add r13, r3, r0
	addi r17, r4, 4
	addi r18, r0, 0
	addi r19, r0, 1
	lui r15, %hi(.L.str)
	addi r15, r15, %lo(.L.str)
	lui r20, %hi(test_mode)
	addi r20, r20, %lo(test_mode)
	lui r16, %hi(.L.str.1)
	addi r16, r16, %lo(.L.str.1)
	add r21, r19, r0
	add r14, r18, r0
	add r27, r18, r0
	jal r0, .LBB0_4
.LBB0_2:
	ldw r3, r17+4
	jal r31, atoi
	add r27, r1, r0
	stb r20+0, r19
.LBB0_3:
	addi r21, r21, 2
	addi r17, r17, 8
	bge r21, r13, .LBB0_9
.LBB0_4:
	ldw r3, r17+0
	add r4, r15, r0
	jal r31, strcmp
	addi r22, r21, 1
	bne r1, r18, .LBB0_6
.LBB0_5:
	blt r22, r13, .LBB0_2
.LBB0_6:
	ldw r3, r17+0
	add r4, r16, r0
	jal r31, strcmp
	bne r1, r18, .LBB0_11
.LBB0_7:
	bge r22, r13, .LBB0_11
.LBB0_8:
	ldw r3, r17+4
	jal r31, atoi
	add r14, r1, r0
	jal r0, .LBB0_3
.LBB0_9:
	jal r31, tube_init
	beq r1, r12, .LBB0_13
.LBB0_10:
	lui r3, %hi(.L.str.3)
	addi r3, r3, %lo(.L.str.3)
	jal r0, .LBB0_12
.LBB0_11:
	lui r3, %hi(.L.str.2)
	addi r3, r3, %lo(.L.str.2)
.LBB0_12:
	jal r31, printf
	addi r28, r0, 1
	jal r0, .LBB0_166
.LBB0_13:
	addi r28, r0, 1
	add r3, r28, r0
	jal r31, tube_open
	addi r12, r0, 0
	beq r1, r12, .LBB0_15
.LBB0_14:
	lui r3, %hi(.L.str.4)
	addi r3, r3, %lo(.L.str.4)
	jal r0, .LBB0_164
.LBB0_15:
	seq r1, r14, r12
	sub r1, r12, r1
	lui r3, 677856
	addi r3, r3, 469
	xor r3, r14, r3
	and r1, r3, r1
	xor r1, r14, r1
	lui r3, %hi(rng_state)
	addi r3, r3, %lo(rng_state)
	stw fp+-352, r3
	stw r3+0, r1
	lui r1, %hi(lives)
	addi r1, r1, %lo(lives)
	addi r16, r0, 3
	stw fp+-296, r1
	stw r1+0, r16
	lui r1, %hi(wave)
	addi r1, r1, %lo(wave)
	addi r18, r0, 1
	stw fp+-336, r1
	stw r1+0, r18
	lui r1, %hi(next_life)
	addi r1, r1, %lo(next_life)
	lui r3, 2
	addi r3, r3, 1808
	stw r1+0, r3
	lui r1, %hi(py)
	addi r1, r1, %lo(py)
	lui r22, 128
	stw fp+-284, r1
	stw r1+0, r22
	lui r1, %hi(px)
	addi r1, r1, %lo(px)
	stw fp+-288, r1
	stw r1+0, r22
	lui r1, %hi(pvy)
	addi r1, r1, %lo(pvy)
	stw fp+-328, r1
	stw r1+0, r12
	lui r1, %hi(pvx)
	addi r1, r1, %lo(pvx)
	stw fp+-332, r1
	stw r1+0, r12
	lui r1, %hi(pangle)
	addi r1, r1, %lo(pangle)
	addi r3, r0, 16
	stw fp+-320, r1
	stw fp+-356, r3
	stw r1+0, r3
	lui r1, %hi(alive)
	addi r1, r1, %lo(alive)
	stw fp+-180, r1
	stb r1+0, r18
	lui r1, %hi(invuln)
	addi r1, r1, %lo(invuln)
	addi r3, r0, 60
	stw fp+-248, r1
	stw fp+-360, r3
	stw r1+0, r3
	jal r31, spawn_wave
	lui r1, %hi(test_mode)
	addi r1, r1, %lo(test_mode)
	stw fp+-208, r1
	ldbu r1, r1+0
	bne r1, r12, .LBB0_20
.LBB0_16:
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	jal r31, printf
	jal r31, tube_info
	andi r1, r1, 256
	bne r1, r12, .LBB0_20
.LBB0_17:
	lui r1, 5
	addi r13, r1, -480
	addi r14, r0, 149
	add r1, r12, r0
.LBB0_18:
	add r15, r1, r0
	add r3, r13, r0
	jal r31, usleep
	jal r31, tube_info
	andi r1, r1, 256
	bne r1, r12, .LBB0_20
.LBB0_19:
	addi r1, r15, 1
	bltu r15, r14, .LBB0_18
.LBB0_20:
	addi r12, fp, -152
	addi r1, r12, 2
	stw fp+-324, r1
	addi r1, r12, 2
	stw fp+-160, r1
	addi r1, r12, 2
	stw fp+-340, r1
	addi r1, fp, -84
	stw fp+-344, r1
	addi r1, r1, 2
	stw fp+-348, r1
	addi r28, r0, 0
	lui r17, %hi(held_fire)
	addi r17, r17, %lo(held_fire)
	lui r19, 16
	addi r20, r19, -33
	addi r23, r0, 258
	addi r24, r0, 65
	addi r25, r0, 112
	addi r26, r0, 80
	addi r13, r0, 27
	addi r1, r0, 32
	stw fp+-216, r1
	lui r1, %hi(fire_latch)
	addi r1, r1, %lo(fire_latch)
	stw fp+-244, r1
	addi r14, r0, 81
	addi r1, r0, 104
	stw fp+-220, r1
	lui r1, %hi(hyper_latch)
	addi r1, r1, %lo(hyper_latch)
	stw fp+-232, r1
	addi r15, r0, 257
	addi r1, r0, 113
	stw fp+-204, r1
	addi r1, r0, 256
	stw fp+-192, r1
	lui r1, %hi(held_up)
	addi r1, r1, %lo(held_up)
	stw fp+-224, r1
	lui r1, %hi(tap_up)
	addi r1, r1, %lo(tap_up)
	stw fp+-212, r1
	addi r1, r0, 259
	stw fp+-184, r1
	stw fp+-172, r22
	stw fp+-300, r27
	stw fp+-304, r28
	stw fp+-292, r16
	stw fp+-280, r12
	stw fp+-252, r17
	stw fp+-256, r19
	stw fp+-260, r20
	stw fp+-264, r23
	stw fp+-268, r24
	stw fp+-272, r25
	stw fp+-276, r26
	stw fp+-308, r13
	stw fp+-312, r14
	stw fp+-316, r15
	jal r0, .LBB0_22
.LBB0_21:
	lui r1, 8
	addi r3, r1, 232
	jal r31, usleep
	ldw r16, fp+-292
	ldw r12, fp+-280
	ldw r13, fp+-308
	ldw r14, fp+-312
	ldw r15, fp+-316
.LBB0_22:
	ldw r1, fp+-208
	ldbu r1, r1+0
	sub r1, r28, r1
	andi r1, r1, 68
	xori r4, r1, 64
	add r3, r12, r0
	jal r31, tube_keys
	lui r4, %hi(held_left)
	addi r4, r4, %lo(held_left)
	lui lr, %hi(tap_left)
	addi lr, lr, %lo(tap_left)
	lui r3, %hi(held_right)
	addi r3, r3, %lo(held_right)
	lui r21, %hi(tap_right)
	addi r21, r21, %lo(tap_right)
	blt r1, r18, .LBB0_54
.LBB0_23:
	ldw r6, r17+0
	ldw r5, fp+-324
	jal r0, .LBB0_26
.LBB0_24:
	ldbu r7, r5+0
	beq r7, r28, .LBB0_25
	jal r0, .LBB0_160
.LBB0_25:
	addi r1, r1, -1
	addi r5, r5, 4
	beq r1, r28, .LBB0_54
.LBB0_26:
	ldhu r8, r5+-2
	and r9, r8, r20
	add r7, r23, r0
	beq r9, r24, .LBB0_30
.LBB0_27:
	addi r7, r0, 68
	beq r9, r7, .LBB0_38
.LBB0_28:
	addi r10, r0, 87
	add r7, r8, r0
	bne r9, r10, .LBB0_30
.LBB0_29:
	ldw r7, fp+-192
.LBB0_30:
	bgt r7, r25, .LBB0_39
.LBB0_31:
	bgt r7, r26, .LBB0_44
.LBB0_32:
	beq r7, r13, .LBB0_24
.LBB0_33:
	ldw r8, fp+-216
	bne r7, r8, .LBB0_25
.LBB0_34:
	ldbu r7, r5+0
	beq r7, r28, .LBB0_37
.LBB0_35:
	bne r6, r28, .LBB0_37
.LBB0_36:
	ldw r6, fp+-244
	stb r6+0, r18
.LBB0_37:
	stw r17+0, r7
	add r6, r7, r0
	jal r0, .LBB0_25
.LBB0_38:
	ldw r7, fp+-184
	ble r7, r25, .LBB0_31
.LBB0_39:
	bgt r7, r15, .LBB0_48
.LBB0_40:
	ldw r8, fp+-204
	beq r7, r8, .LBB0_24
.LBB0_41:
	ldw r8, fp+-192
	bne r7, r8, .LBB0_25
.LBB0_42:
	ldbu r7, r5+0
	ldw r8, fp+-224
	stw r8+0, r7
	beq r7, r28, .LBB0_25
.LBB0_43:
	ldw r7, fp+-212
	stb r7+0, r18
	jal r0, .LBB0_25
.LBB0_44:
	beq r7, r14, .LBB0_24
.LBB0_45:
	ldw r8, fp+-220
	bne r7, r8, .LBB0_25
.LBB0_46:
	ldbu r7, r5+0
	beq r7, r28, .LBB0_25
.LBB0_47:
	ldw r7, fp+-232
	stb r7+0, r18
	jal r0, .LBB0_25
.LBB0_48:
	ldw r8, fp+-184
	beq r7, r8, .LBB0_52
.LBB0_49:
	addi r8, r0, 258
	bne r7, r8, .LBB0_25
.LBB0_50:
	ldbu r7, r5+0
	stw r4+0, r7
	addi r8, r0, 0
	beq r7, r8, .LBB0_25
.LBB0_51:
	stb lr+0, r18
	jal r0, .LBB0_25
.LBB0_52:
	ldbu r7, r5+0
	stw r3+0, r7
	addi r8, r0, 0
	beq r7, r8, .LBB0_25
.LBB0_53:
	stb r21+0, r18
	jal r0, .LBB0_25
.LBB0_54:
	stw fp+-240, r21
	stw fp+-236, lr
	lui r1, %hi(game_over)
	addi r1, r1, %lo(game_over)
	stw fp+-228, r1
	ldbu r5, r1+0
	lui r24, 1048575
	lui r12, 1
	lui r26, %hi(shots+80)
	addi r26, r26, %lo(shots+80)
	lui r1, %hi(debris+480)
	addi r1, r1, %lo(debris+480)
	stw fp+-156, r1
	lui r21, %hi(rocks)
	addi r21, r21, %lo(rocks)
	lui r28, %hi(shots+84)
	addi r28, r28, %lo(shots+84)
	lui r14, %hi(shots+88)
	addi r14, r14, %lo(shots+88)
	lui r1, %hi(debris+484)
	addi r1, r1, %lo(debris+484)
	stw fp+-164, r1
	lui r1, %hi(debris+488)
	addi r1, r1, %lo(debris+488)
	stw fp+-168, r1
	bne r5, r18, .LBB0_56
.LBB0_55:
	ldw r1, fp+-208
	ldbu r1, r1+0
	andi r1, r1, 1
	addi r6, r0, 0
	beq r1, r6, .LBB0_117
.LBB0_56:
	lui r1, %hi(frame)
	addi r1, r1, %lo(frame)
	ldw r6, r1+0
	addi r6, r6, 1
	stw r1+0, r6
	ldw r1, fp+-180
	ldbu r1, r1+0
	lui r25, 256
	bne r1, r18, .LBB0_73
.LBB0_57:
	ldw r4, r4+0
	addi r1, r0, 0
	ldw r6, fp+-328
	bne r4, r1, .LBB0_59
.LBB0_58:
	ldw r4, fp+-236
	ldbu r4, r4+0
	andi r4, r4, 1
	beq r4, r1, .LBB0_60
.LBB0_59:
	ldw r5, fp+-320
	ldw r4, r5+0
	addi r4, r4, 1
	andi r4, r4, 63
	stw r5+0, r4
.LBB0_60:
	ldw r3, r3+0
	ldw r7, fp+-332
	bne r3, r1, .LBB0_62
.LBB0_61:
	ldw r3, fp+-240
	ldbu r3, r3+0
	andi r3, r3, 1
	beq r3, r1, .LBB0_63
.LBB0_62:
	ldw r4, fp+-320
	ldw r3, r4+0
	addi r3, r3, -1
	andi r3, r3, 63
	stw r4+0, r3
.LBB0_63:
	ldw r3, fp+-224
	ldw r3, r3+0
	sne r3, r3, r1
	ldw r4, fp+-212
	ldbu r4, r4+0
	or  r3, r3, r4
	andi r3, r3, 1
	lui r4, %hi(thrusting)
	addi r4, r4, %lo(thrusting)
	stw r4+0, r3
	beq r3, r1, .LBB0_65
.LBB0_64:
	ldw r1, fp+-320
	ldw r1, r1+0
	slli r3, r1, 1
	lui r4, %hi(COS64)
	addi r4, r4, %lo(COS64)
	add r3, r3, r4
	ldh r3, r3+0
	slli r5, r3, 1
	add r3, r5, r3
	ldw r5, r7+0
	add r3, r3, r5
	stw r7+0, r3
	addi r1, r1, 48
	andi r1, r1, 63
	slli r1, r1, 1
	add r1, r1, r4
	ldh r1, r1+0
	slli r3, r1, 1
	add r1, r3, r1
	ldw r3, r6+0
	add r1, r1, r3
	stw r6+0, r1
.LBB0_65:
	ldw r1, r7+0
	srai r3, r1, 31
	srli r3, r3, 26
	add r3, r1, r3
	srai r3, r3, 6
	sub r4, r1, r3
	stw r7+0, r4
	ldw r1, r6+0
	srai r3, r1, 31
	srli r3, r3, 26
	add r3, r1, r3
	srai r3, r3, 6
	sub r1, r1, r3
	stw r6+0, r1
	addi r3, r12, 905
	blt r4, r3, .LBB0_67
.LBB0_66:
	addi r4, r12, 904
	stw r7+0, r4
.LBB0_67:
	ldw r5, r7+0
	addi r4, r24, -905
	bgt r5, r4, .LBB0_76
.LBB0_68:
	addi r5, r24, -904
	stw r7+0, r5
	bge r1, r3, .LBB0_77
.LBB0_69:
	ldw r1, r6+0
	bgt r1, r4, .LBB0_71
.LBB0_70:
	addi r1, r24, -904
	stw r6+0, r1
.LBB0_71:
	ldw r4, fp+-288
	ldw r1, r4+0
	ldw r3, r7+0
	add r1, r3, r1
	addi r3, r25, -1
	and r1, r1, r3
	stw r4+0, r1
	ldw r5, fp+-284
	ldw r1, r5+0
	ldw r4, r6+0
	add r1, r4, r1
	and r1, r1, r3
	stw r5+0, r1
	ldw r1, fp+-248
	ldw r1, r1+0
	blt r1, r18, .LBB0_83
.LBB0_72:
	addi r1, r1, -1
	ldw r3, fp+-248
	stw r3+0, r1
	jal r0, .LBB0_83
.LBB0_73:
	addi r1, r0, 0
	ldw r13, fp+-328
	bne r5, r1, .LBB0_83
.LBB0_74:
	lui r3, %hi(respawn_timer)
	addi r3, r3, %lo(respawn_timer)
	ldw r4, r3+0
	ble r4, r1, .LBB0_78
.LBB0_75:
	addi r1, r4, -1
	stw r3+0, r1
	jal r0, .LBB0_83
.LBB0_76:
	blt r1, r3, .LBB0_69
.LBB0_77:
	addi r1, r12, 904
	stw r6+0, r1
	ldw r1, r6+0
	ble r1, r4, .LBB0_70
	jal r0, .LBB0_71
.LBB0_78:
	addi r3, r0, -784
	jal r0, .LBB0_80
.LBB0_79:
	addi r3, r3, 28
	beq r3, r1, .LBB0_82
.LBB0_80:
	lui r4, %hi(rocks+784)
	addi r4, r4, %lo(rocks+784)
	add r4, r3, r4
	ldw r4, r4+0
	beq r4, r1, .LBB0_79
.LBB0_81:
	lui r4, %hi(rocks+788)
	addi r4, r4, %lo(rocks+788)
	add r4, r3, r4
	ldw r4, r4+0
	lui r5, %hi(rocks+792)
	addi r5, r5, %lo(rocks+792)
	add r5, r3, r5
	ldw r5, r5+0
	lui r6, %hi(rocks+804)
	addi r6, r6, %lo(rocks+804)
	add r6, r3, r6
	ldw r6, r6+0
	seq r7, r6, r11
	seq r6, r6, r18
	sub r6, r1, r6
	andi r6, r6, 67
	xori r8, r6, 565
	sub r7, r1, r7
	xori r6, r6, 205
	and r6, r6, r7
	xor r6, r8, r6
	lui r7, 1048448
	add r8, r4, r7
	srai r9, r8, 31
	srli r9, r9, 24
	add r8, r8, r9
	srai r8, r8, 8
	add r7, r5, r7
	srai r9, r7, 31
	srli r9, r9, 24
	add r7, r7, r9
	srai r7, r7, 8
	addi r9, r25, 255
	sgt r4, r4, r9
	add r10, r8, r24
	xor r10, r10, r8
	sub r4, r1, r4
	and r4, r10, r4
	xor r4, r8, r4
	addi r8, r0, -2048
	slt r10, r4, r8
	add lr, r4, r12
	xor lr, lr, r4
	sub r10, r1, r10
	and r10, lr, r10
	xor r4, r4, r10
	sgt r5, r5, r9
	add r9, r7, r24
	xor r9, r9, r7
	sub r5, r1, r5
	and r5, r9, r5
	xor r5, r7, r5
	slt r7, r5, r8
	add r8, r5, r12
	xor r8, r8, r5
	sub r7, r1, r7
	and r7, r8, r7
	xor r5, r5, r7
	mul r4, r4, r4
	mul r5, r5, r5
	add r4, r5, r4
	mul r5, r6, r6
	bgeu r4, r5, .LBB0_79
	jal r0, .LBB0_83
.LBB0_82:
	ldw r3, fp+-284
	stw r3+0, r22
	ldw r3, fp+-288
	stw r3+0, r22
	stw r13+0, r1
	ldw r3, fp+-332
	stw r3+0, r1
	ldw r1, fp+-320
	ldw r3, fp+-356
	stw r1+0, r3
	ldw r1, fp+-180
	stb r1+0, r18
	ldw r1, fp+-248
	ldw r3, fp+-360
	stw r1+0, r3
.LBB0_83:
	ldw r1, fp+-244
	ldbu r1, r1+0
	lui r13, %hi(shots+92)
	addi r13, r13, %lo(shots+92)
	lui r15, %hi(shots+96)
	addi r15, r15, %lo(shots+96)
	bne r1, r18, .LBB0_89
.LBB0_84:
	addi r3, r0, 0
	ldw r1, fp+-244
	stb r1+0, r3
	ldw r1, fp+-180
	ldbu r1, r1+0
	bne r1, r18, .LBB0_89
.LBB0_85:
	addi r1, r0, -80
.LBB0_86:
	add r4, r1, r26
	ldw r5, r4+0
	ble r5, r3, .LBB0_88
.LBB0_87:
	addi r1, r1, 20
	bne r1, r3, .LBB0_86
	jal r0, .LBB0_89
.LBB0_88:
	addi r3, r0, 34
	stw r4+0, r3
	ldw r3, fp+-288
	ldw r3, r3+0
	ldw r4, fp+-320
	ldw r4, r4+0
	slli r5, r4, 1
	lui r6, %hi(COS64)
	addi r6, r6, %lo(COS64)
	add r5, r5, r6
	ldh r5, r5+0
	addi r7, r0, 120
	mul r5, r5, r7
	add r3, r5, r3
	addi r8, r25, -1
	and r3, r3, r8
	add r9, r1, r28
	stw r9+0, r3
	ldw r3, fp+-284
	ldw r3, r3+0
	addi r4, r4, 48
	andi r4, r4, 63
	slli r4, r4, 1
	add r4, r4, r6
	ldh r4, r4+0
	mul r4, r4, r7
	add r3, r4, r3
	and r3, r3, r8
	add r6, r1, r14
	stw r6+0, r3
	ldw r3, fp+-332
	ldw r3, r3+0
	add r3, r3, r5
	add r5, r1, r13
	stw r5+0, r3
	ldw r3, fp+-328
	ldw r3, r3+0
	add r3, r3, r4
	add r1, r1, r15
	stw r1+0, r3
.LBB0_89:
	ldw r1, fp+-232
	ldbu r1, r1+0
	bne r1, r18, .LBB0_93
.LBB0_90:
	addi r1, r0, 0
	ldw r3, fp+-232
	stb r3+0, r1
	ldw r3, fp+-180
	ldbu r3, r3+0
	bne r3, r18, .LBB0_93
.LBB0_91:
	ldw r6, fp+-352
	ldw r3, r6+0
	slli r4, r3, 13
	xor r3, r4, r3
	srli r4, r3, 17
	xor r3, r4, r3
	slli r4, r3, 5
	xor r3, r4, r3
	srli r4, r3, 8
	addi r5, r25, -1
	and r4, r4, r5
	ldw r7, fp+-288
	stw r7+0, r4
	slli r4, r3, 13
	xor r3, r4, r3
	srli r4, r3, 17
	xor r3, r4, r3
	slli r4, r3, 5
	xor r3, r4, r3
	srli r4, r3, 8
	and r4, r4, r5
	ldw r5, fp+-284
	stw r5+0, r4
	ldw r4, fp+-328
	stw r4+0, r1
	ldw r4, fp+-332
	stw r4+0, r1
	slli r4, r3, 13
	xor r3, r4, r3
	srli r4, r3, 17
	xor r3, r4, r3
	slli r4, r3, 5
	xor r3, r4, r3
	stw r6+0, r3
	andi r3, r3, 1792
	bne r3, r1, .LBB0_93
.LBB0_92:
	jal r31, kill_ship
.LBB0_93:
	addi r1, r0, -80
	jal r0, .LBB0_95
.LBB0_94:
	addi r1, r1, 20
	addi r23, r0, 0
	beq r1, r23, .LBB0_97
.LBB0_95:
	add r3, r1, r26
	ldw r4, r3+0
	blt r4, r18, .LBB0_94
.LBB0_96:
	addi r4, r4, -1
	stw r3+0, r4
	add r3, r1, r28
	ldw r4, r3+0
	add r5, r1, r13
	ldw r5, r5+0
	add r4, r5, r4
	addi r5, r25, -1
	and r4, r4, r5
	stw r3+0, r4
	add r3, r1, r14
	ldw r4, r3+0
	add r6, r1, r15
	ldw r6, r6+0
	add r4, r6, r4
	and r4, r4, r5
	stw r3+0, r4
	jal r0, .LBB0_94
.LBB0_97:
	addi r1, r0, -480
	jal r0, .LBB0_99
.LBB0_98:
	addi r1, r1, 20
	beq r1, r23, .LBB0_101
.LBB0_99:
	ldw r3, fp+-156
	add r3, r1, r3
	ldw r4, r3+0
	blt r4, r18, .LBB0_98
.LBB0_100:
	addi r4, r4, -1
	stw r3+0, r4
	ldw r3, fp+-164
	add r3, r1, r3
	ldw r4, r3+0
	lui r5, %hi(debris+492)
	addi r5, r5, %lo(debris+492)
	add r5, r1, r5
	ldw r5, r5+0
	add r4, r5, r4
	addi r5, r25, -1
	and r4, r4, r5
	stw r3+0, r4
	ldw r3, fp+-168
	add r3, r1, r3
	ldw r4, r3+0
	lui r6, %hi(debris+496)
	addi r6, r6, %lo(debris+496)
	add r6, r1, r6
	ldw r6, r6+0
	add r4, r6, r4
	and r4, r4, r5
	stw r3+0, r4
	jal r0, .LBB0_98
.LBB0_101:
	ldw r1, fp+-248
	ldw r1, r1+0
	stw fp+-188, r1
	ldw r1, fp+-288
	ldw r1, r1+0
	stw fp+-196, r1
	ldw r1, fp+-284
	ldw r1, r1+0
	stw fp+-200, r1
	add r15, r23, r0
	add r19, r23, r0
	jal r0, .LBB0_103
.LBB0_102:
	addi r15, r15, 1
	beq r15, r17, .LBB0_114
.LBB0_103:
	addi r17, r0, 28
	mul r1, r15, r17
	add r16, r1, r21
	ldw r1, r16+0
	beq r1, r23, .LBB0_102
.LBB0_104:
	addi r19, r19, 1
	ldw r1, r16+4
	ldw r3, r16+12
	add r1, r3, r1
	addi r3, r25, -1
	and r1, r1, r3
	stw r16+4, r1
	ldw r4, r16+8
	ldw r5, r16+16
	add r4, r5, r4
	and r3, r4, r3
	stw r16+8, r3
	addi r4, r0, -80
	jal r0, .LBB0_106
.LBB0_105:
	addi r4, r4, 20
	addi r5, r0, 0
	beq r4, r5, .LBB0_109
.LBB0_106:
	add r5, r4, r26
	ldw r6, r5+0
	blt r6, r18, .LBB0_105
.LBB0_107:
	add r6, r4, r28
	ldw r7, r6+0
	add r6, r4, r14
	ldw r8, r6+0
	ldw r6, r16+20
	seq r9, r6, r11
	seq r10, r6, r18
	addi r6, r0, 0
	sub r10, r6, r10
	andi r10, r10, 195
	xori lr, r10, 73
	sub r9, r6, r9
	xori r10, r10, 325
	and r9, r10, r9
	xor r9, lr, r9
	sub r7, r7, r1
	srai r10, r7, 31
	srli r10, r10, 24
	add r10, r7, r10
	srai r10, r10, 8
	sub r8, r8, r3
	srai lr, r8, 31
	srli lr, lr, 24
	add lr, r8, lr
	srai lr, lr, 8
	addi r27, r22, 255
	sgt r7, r7, r27
	add r13, r10, r24
	xor r13, r13, r10
	sub r7, r6, r7
	and r7, r13, r7
	xor r7, r10, r7
	addi r10, r0, -2048
	slt r13, r7, r10
	add r20, r7, r12
	xor r20, r20, r7
	sub r13, r6, r13
	and r13, r20, r13
	xor r7, r7, r13
	sgt r8, r8, r27
	add r13, lr, r24
	xor r13, r13, lr
	sub r8, r6, r8
	and r8, r13, r8
	xor r8, lr, r8
	slt r10, r8, r10
	add lr, r8, r12
	xor lr, lr, r8
	sub r10, r6, r10
	and r10, lr, r10
	xor r8, r8, r10
	mul r7, r7, r7
	mul r8, r8, r8
	add r7, r8, r7
	mul r8, r9, r9
	bgeu r7, r8, .LBB0_105
.LBB0_108:
	stw r5+0, r6
	add r3, r15, r0
	jal r31, kill_rock
.LBB0_109:
	ldw r3, r16+0
	addi r1, r0, 0
	beq r3, r1, .LBB0_102
.LBB0_110:
	ldw r3, fp+-180
	ldbu r3, r3+0
	andi r3, r3, 1
	beq r3, r1, .LBB0_102
.LBB0_111:
	ldw r3, fp+-188
	bne r3, r1, .LBB0_102
.LBB0_112:
	ldw r3, r16+4
	ldw r4, r16+8
	ldw r5, r16+20
	seq r6, r5, r11
	seq r5, r5, r18
	sub r5, r1, r5
	andi r5, r5, 195
	xori r7, r5, 125
	sub r6, r1, r6
	xori r5, r5, 317
	and r5, r5, r6
	xor r5, r7, r5
	ldw r6, fp+-196
	sub r3, r6, r3
	srai r6, r3, 31
	srli r6, r6, 24
	add r6, r3, r6
	srai r6, r6, 8
	ldw r7, fp+-200
	sub r4, r7, r4
	srai r7, r4, 31
	srli r7, r7, 24
	add r7, r4, r7
	srai r7, r7, 8
	addi r8, r22, 255
	sgt r3, r3, r8
	add r9, r6, r24
	xor r9, r9, r6
	sub r3, r1, r3
	and r3, r9, r3
	xor r3, r6, r3
	addi r6, r0, -2048
	slt r9, r3, r6
	add r10, r3, r12
	xor r10, r10, r3
	sub r9, r1, r9
	and r9, r10, r9
	xor r3, r3, r9
	sgt r4, r4, r8
	add r8, r7, r24
	xor r8, r8, r7
	sub r4, r1, r4
	and r4, r8, r4
	xor r4, r7, r4
	slt r6, r4, r6
	add r7, r4, r12
	xor r7, r7, r4
	sub r1, r1, r6
	and r1, r7, r1
	xor r1, r4, r1
	mul r3, r3, r3
	mul r1, r1, r1
	add r1, r1, r3
	mul r3, r5, r5
	bgeu r1, r3, .LBB0_102
.LBB0_113:
	add r3, r15, r0
	jal r31, kill_rock
	jal r31, kill_ship
	jal r0, .LBB0_102
.LBB0_114:
	addi r1, r0, 0
	bne r19, r1, .LBB0_117
.LBB0_115:
	ldw r3, fp+-228
	ldbu r3, r3+0
	andi r3, r3, 1
	bne r3, r1, .LBB0_117
.LBB0_116:
	ldw r3, fp+-336
	ldw r1, r3+0
	addi r1, r1, 1
	stw r3+0, r1
	jal r31, spawn_wave
.LBB0_117:
	addi r25, r0, 0
	ldw r1, fp+-212
	stb r1+0, r25
	ldw r1, fp+-240
	stb r1+0, r25
	ldw r1, fp+-236
	stb r1+0, r25
	lui r15, %hi(list)
	addi r15, r15, %lo(list)
	lui r1, 331776
	addi r1, r1, -1
	stw r15+0, r1
	stw fp+-88, r11
	lui r1, 262144
	stw fp+-188, r1
	addi r1, r1, 200
	lui r3, %hi(list+4)
	addi r3, r3, %lo(list+4)
	stw fp+-196, r3
	stw r3+0, r1
	lui r1, %hi(score)
	addi r1, r1, %lo(score)
	ldw r8, r1+0
	addi r6, r12, -196
	addi r4, fp, -88
	addi r5, r0, 120
	addi r7, r0, 14
	add r3, r15, r0
	add r9, r11, r0
	jal r31, vfont_uint
	ldw r1, fp+-296
	ldw r7, r1+0
	lui r5, %hi(draw_ship.base+48)
	addi r5, r5, %lo(draw_ship.base+48)
	lui r6, %hi(draw_ship.base+52)
	addi r6, r6, %lo(draw_ship.base+52)
	lui r4, 838861
	lui r1, 196608
	lui r3, 131072
	blt r7, r18, .LBB0_125
.LBB0_118:
	stw fp+-200, r21
	addi r8, r0, 8
	slt r8, r7, r8
	sub r8, r25, r8
	xori r7, r7, 8
	and r7, r7, r8
	xori r7, r7, 8
	addi r8, r0, 0
	ldw r10, fp+-88
.LBB0_119:
	add r9, r10, r0
	addi lr, r0, -48
	ldw r13, fp+-160
.LBB0_120:
	add r10, lr, r5
	ldw r16, r10+0
	add r10, lr, r6
	ldhu r17, r10+0
	addi r10, r0, 0
	sub r17, r10, r17
	sth r13+-2, r17
	sth r13+0, r16
	addi lr, lr, 8
	addi r13, r13, 4
	bne lr, r10, .LBB0_120
.LBB0_121:
	lui lr, 9
	addi lr, lr, -1024
	mul lr, r8, lr
	srli lr, lr, 8
	addi r13, lr, 160
	slli lr, r9, 2
	add r16, lr, r15
	ldw r17, fp+-160
	add lr, r10, r0
	add r19, r10, r0
.LBB0_122:
	addi r20, r4, -819
	mulhu r20, r19, r20
	srli r20, r20, 2
	addi r23, r0, -20
	mul r20, r20, r23
	add r20, r17, r20
	ldh r23, r20+-2
	add r23, r13, r23
	ldh r20, r20+0
	sgt r27, r23, r10
	sub r27, r10, r27
	and r23, r23, r27
	addi r27, r12, -1
	slt r21, r23, r27
	sub r21, r10, r21
	xori r23, r23, 4095
	and r21, r23, r21
	xori r21, r21, 4095
	addi r23, r24, 396
	xor r22, r20, r23
	sgt r20, r20, r23
	sub r20, r10, r20
	and r20, r22, r20
	xor r20, r20, r23
	addi r22, r12, -396
	add r20, r20, r22
	sltu r22, r20, r27
	sub r22, r10, r22
	xori r20, r20, 4095
	and r20, r20, r22
	xori r20, r20, 4095
	seq r22, lr, r10
	sub r22, r10, r22
	and r22, r22, r1
	xor r22, r22, r3
	slli r21, r21, 16
	or  r21, r21, r22
	slli r20, r20, 4
	or  r20, r21, r20
	stw r16+0, r20
	addi r19, r19, 1
	addi r16, r16, 4
	addi lr, lr, -1
	addi r17, r17, 4
	addi r20, r0, -6
	bne lr, r20, .LBB0_122
.LBB0_123:
	addi r8, r8, 1
	sub r10, r9, lr
	bne r8, r7, .LBB0_119
.LBB0_124:
	sub r7, r9, lr
	stw fp+-88, r7
	ldw r22, fp+-172
	ldw r21, fp+-200
.LBB0_125:
	ldw r8, fp+-88
	addi r7, r8, 1
	stw fp+-88, r7
	slli r8, r8, 2
	add r8, r8, r15
	ldw r9, fp+-188
	addi r9, r9, 180
	stw r8+0, r9
	add r8, r25, r0
	stw fp+-176, r26
	jal r0, .LBB0_127
.LBB0_126:
	addi r8, r8, 1
	beq r8, r9, .LBB0_131
.LBB0_127:
	addi r9, r0, 28
	mul r10, r8, r9
	add r10, r10, r21
	ldw lr, r10+0
	beq lr, r25, .LBB0_126
.LBB0_128:
	add r26, r14, r0
	add r14, r28, r0
	add r28, r25, r0
	add r25, r21, r0
	ldw r13, r10+4
	ldw r16, r10+8
	ldw r19, r10+24
	ldw r10, r10+20
	seq lr, r10, r11
	seq r17, r10, r18
	addi r10, r0, 0
	sub r17, r10, r17
	andi r17, r17, 195
	xori r17, r17, 65
	sub lr, r10, lr
	ori  r20, r17, 260
	and lr, r20, lr
	xor lr, r17, lr
	srai r17, r13, 31
	srli r17, r17, 24
	add r13, r13, r17
	srai r13, r13, 8
	srai r17, r16, 31
	srli r17, r17, 24
	add r16, r16, r17
	srai r16, r16, 8
	slli r17, r7, 2
	add r17, r17, r15
	addi r20, r0, 48
	mul r19, r19, r20
	lui r20, %hi(rock_shape+2)
	addi r20, r20, %lo(rock_shape+2)
	add r19, r19, r20
	add r23, r10, r0
	add r24, r10, r0
.LBB0_129:
	lui r20, 699051
	addi r20, r20, -1365
	mulhu r20, r24, r20
	srli r20, r20, 3
	addi r21, r0, -48
	mul r20, r20, r21
	add r20, r19, r20
	ldh r21, r20+-2
	mul r21, lr, r21
	srai r22, r21, 31
	srli r22, r22, 24
	add r21, r21, r22
	srai r21, r21, 8
	add r21, r21, r13
	ldh r20, r20+0
	mul r20, lr, r20
	srai r22, r20, 31
	srli r22, r22, 24
	add r20, r20, r22
	srai r20, r20, 8
	add r20, r20, r16
	sgt r22, r21, r10
	sub r22, r10, r22
	and r21, r21, r22
	addi r22, r12, -1
	slt r27, r21, r22
	sub r27, r10, r27
	xori r21, r21, 4095
	and r21, r21, r27
	xori r21, r21, 4095
	sgt r27, r20, r10
	sub r27, r10, r27
	and r20, r20, r27
	slt r22, r20, r22
	sub r22, r10, r22
	xori r20, r20, 4095
	and r20, r20, r22
	xori r20, r20, 4095
	seq r22, r23, r10
	sub r22, r10, r22
	and r22, r22, r1
	xor r22, r22, r3
	slli r21, r21, 16
	or  r21, r21, r22
	slli r20, r20, 4
	or  r20, r21, r20
	stw r17+0, r20
	addi r24, r24, 1
	addi r17, r17, 4
	addi r23, r23, -1
	addi r19, r19, 4
	addi r20, r0, -13
	bne r23, r20, .LBB0_129
.LBB0_130:
	sub r7, r7, r23
	stw fp+-88, r7
	ldw r22, fp+-172
	add r21, r25, r0
	add r25, r28, r0
	add r28, r14, r0
	add r14, r26, r0
	ldw r26, fp+-176
	jal r0, .LBB0_126
.LBB0_131:
	ldw r7, fp+-88
	addi r9, r7, 1
	stw fp+-88, r9
	slli r7, r7, 2
	add r8, r7, r15
	ldw r7, fp+-188
	addi r7, r7, 255
	stw r8+0, r7
	addi r10, r0, -80
	ldw r17, fp+-252
	ldw r19, fp+-256
	ldw r20, fp+-260
	ldw r23, fp+-264
	ldw r24, fp+-268
	ldw r25, fp+-272
	jal r0, .LBB0_133
.LBB0_132:
	addi r10, r10, 20
	addi r21, r0, 0
	beq r10, r21, .LBB0_135
.LBB0_133:
	add r8, r10, r26
	ldw lr, r8+0
	lui r8, 65520
	blt lr, r18, .LBB0_132
.LBB0_134:
	add lr, r10, r28
	ldw lr, lr+0
	srai r13, lr, 31
	srli r13, r13, 24
	add lr, lr, r13
	slli lr, lr, 8
	and lr, lr, r8
	add r13, r10, r14
	ldw r13, r13+0
	srai r16, r13, 31
	srli r16, r16, 24
	add r13, r13, r16
	srli r13, r13, 4
	addi r16, r19, -16
	and r13, r13, r16
	or  lr, lr, r13
	or  lr, lr, r1
	addi r13, r9, 1
	stw fp+-88, r13
	slli r9, r9, 2
	add r9, r9, r15
	stw r9+0, lr
	add r9, r13, r0
	jal r0, .LBB0_132
.LBB0_135:
	ldw r10, fp+-88
	addi r9, r10, 1
	stw fp+-88, r9
	slli r10, r10, 2
	add r10, r10, r15
	ldw lr, fp+-188
	addi lr, lr, 120
	stw r10+0, lr
	addi r10, r0, -480
	ldw r27, fp+-300
	ldw r28, fp+-304
	ldw r26, fp+-276
	jal r0, .LBB0_137
.LBB0_136:
	addi r10, r10, 20
	beq r10, r21, .LBB0_139
.LBB0_137:
	ldw lr, fp+-156
	add lr, r10, lr
	ldw lr, lr+0
	blt lr, r18, .LBB0_136
.LBB0_138:
	ldw lr, fp+-164
	add lr, r10, lr
	ldw lr, lr+0
	srai r13, lr, 31
	srli r13, r13, 24
	add lr, lr, r13
	slli lr, lr, 8
	and lr, lr, r8
	ldw r13, fp+-168
	add r13, r10, r13
	ldw r13, r13+0
	srai r14, r13, 31
	srli r14, r14, 24
	add r13, r13, r14
	srli r13, r13, 4
	addi r14, r19, -16
	and r13, r13, r14
	or  lr, lr, r13
	or  lr, lr, r1
	addi r13, r9, 1
	stw fp+-88, r13
	slli r9, r9, 2
	add r9, r9, r15
	stw r9+0, lr
	add r9, r13, r0
	jal r0, .LBB0_136
.LBB0_139:
	ldw r8, fp+-88
	addi r9, r8, 1
	stw fp+-88, r9
	slli r9, r8, 2
	add r10, r9, r15
	stw r10+0, r7
	ldw r10, fp+-180
	ldbu r10, r10+0
	bne r10, r18, .LBB0_154
.LBB0_140:
	ldw r10, fp+-248
	ldw r10, r10+0
	blt r10, r18, .LBB0_142
.LBB0_141:
	lui r10, %hi(frame)
	addi r10, r10, %lo(frame)
	ldbu r10, r10+0
	andi r10, r10, 2
	addi lr, r0, 0
	bne r10, lr, .LBB0_154
.LBB0_142:
	ldw r10, fp+-288
	ldw r13, r10+0
	ldw r10, fp+-284
	ldw lr, r10+0
	ldw r10, fp+-320
	ldw r14, r10+0
	lui r10, %hi(thrusting)
	addi r10, r10, %lo(thrusting)
	ldw r19, r10+0
	lui r10, %hi(frame)
	addi r10, r10, %lo(frame)
	ldw r10, r10+0
	stw fp+-156, r10
	slli r10, r14, 1
	lui r16, %hi(COS64)
	addi r16, r16, %lo(COS64)
	add r10, r10, r16
	ldh r10, r10+0
	addi r14, r14, 48
	andi r14, r14, 63
	slli r14, r14, 1
	add r14, r14, r16
	ldh r14, r14+0
	addi r16, r0, -48
	ldw r20, fp+-340
.LBB0_143:
	add r22, r16, r5
	ldw r22, r22+0
	mul r23, r22, r10
	add r24, r16, r6
	ldw r24, r24+0
	mul r25, r24, r14
	sub r23, r23, r25
	srai r25, r23, 31
	srli r25, r25, 24
	add r23, r23, r25
	srli r23, r23, 8
	sth r20+-2, r23
	mul r22, r22, r14
	mul r23, r24, r10
	add r22, r23, r22
	srai r23, r22, 31
	srli r23, r23, 24
	add r22, r22, r23
	srli r22, r22, 8
	sth r20+0, r22
	addi r16, r16, 8
	addi r20, r20, 4
	bne r16, r21, .LBB0_143
.LBB0_144:
	srai r5, r13, 31
	srli r5, r5, 24
	add r5, r13, r5
	srai r5, r5, 8
	srli r6, lr, 8
	addi r20, r8, 2
	ldw lr, fp+-196
	add r24, r9, lr
	lui lr, %hi(list+12)
	addi lr, lr, %lo(list+12)
	add r25, r9, lr
	add r23, r21, r0
	add lr, r21, r0
	ldw r17, fp+-280
.LBB0_145:
	add r13, r25, r0
	addi r16, r4, -819
	mulhu r16, lr, r16
	srli r16, r16, 2
	addi r22, r0, -20
	mul r16, r16, r22
	add r16, r17, r16
	add r16, r16, r23
	ldh r22, r16+0
	add r22, r5, r22
	ldh r16, r16+2
	add r25, r6, r16
	sgt r16, r22, r21
	sub r16, r21, r16
	and r22, r22, r16
	addi r16, r12, -1
	slt r26, r22, r16
	sub r26, r21, r26
	xori r22, r22, 4095
	and r22, r22, r26
	xori r22, r22, 4095
	sgt r26, r25, r21
	sub r26, r21, r26
	and r25, r25, r26
	slt r26, r25, r16
	sub r26, r21, r26
	xori r25, r25, 4095
	and r25, r25, r26
	xori r25, r25, 4095
	seq r26, r23, r21
	sub r26, r21, r26
	and r26, r26, r1
	xor r26, r26, r3
	slli r22, r22, 16
	or  r22, r22, r26
	slli r25, r25, 4
	or  r22, r22, r25
	add r25, r24, r23
	stw r25+0, r22
	addi lr, lr, 1
	addi r20, r20, 1
	addi r23, r23, 4
	addi r25, r13, 4
	addi r22, r0, 24
	bne r23, r22, .LBB0_145
.LBB0_146:
	addi r4, r20, -1
	stw fp+-88, r4
	addi r4, r0, 0
	ldw r22, fp+-172
	beq r19, r4, .LBB0_153
.LBB0_147:
	ldw r17, fp+-156
	andi r17, r17, 1
	beq r17, r4, .LBB0_153
.LBB0_148:
	addi r17, r0, -24
	ldw r19, fp+-348
.LBB0_149:
	lui r22, %hi(draw_ship.fb+24)
	addi r22, r22, %lo(draw_ship.fb+24)
	add r22, r17, r22
	ldw r22, r22+0
	mul r24, r22, r10
	lui r25, %hi(draw_ship.fb+28)
	addi r25, r25, %lo(draw_ship.fb+28)
	add r25, r17, r25
	ldw r25, r25+0
	mul r26, r25, r14
	sub r24, r24, r26
	srai r26, r24, 31
	srli r26, r26, 24
	add r24, r24, r26
	srli r24, r24, 8
	sth r19+-2, r24
	mul r22, r22, r14
	mul r24, r25, r10
	add r22, r24, r22
	srai r24, r22, 31
	srli r24, r24, 24
	add r22, r22, r24
	srli r22, r22, 8
	sth r19+0, r22
	addi r17, r17, 8
	addi r19, r19, 4
	bne r17, r4, .LBB0_149
.LBB0_150:
	stw fp+-88, r20
	add r9, r9, r23
	ldw r10, fp+-196
	add r9, r9, r10
	ldw r10, fp+-188
	addi r10, r10, 160
	stw r9+0, r10
	add r9, r4, r0
	ldw r19, fp+-292
	ldw r20, fp+-344
.LBB0_151:
	andi r10, r9, 1
	slli r10, r10, 2
	add r10, r20, r10
	ldh r14, r10+0
	add r14, r5, r14
	ldh r10, r10+2
	add r10, r6, r10
	sgt r17, r14, r4
	sub r17, r4, r17
	and r14, r14, r17
	slt r17, r14, r16
	sub r17, r4, r17
	xori r14, r14, 4095
	and r14, r14, r17
	xori r14, r14, 4095
	sgt r17, r10, r4
	sub r17, r4, r17
	and r10, r10, r17
	slt r17, r10, r16
	sub r17, r4, r17
	xori r10, r10, 4095
	and r10, r10, r17
	xori r10, r10, 4095
	seq r17, r9, r4
	sub r17, r4, r17
	and r17, r17, r1
	xor r17, r17, r3
	slli r14, r14, 16
	or  r14, r14, r17
	slli r10, r10, 4
	or  r10, r14, r10
	stw r13+0, r10
	addi r9, r9, 1
	addi r13, r13, 4
	bne r9, r19, .LBB0_151
.LBB0_152:
	add r1, r8, lr
	add r1, r1, r9
	addi r1, r1, 3
	stw fp+-88, r1
	stw r13+0, r7
	ldw r22, fp+-172
.LBB0_153:
	ldw r17, fp+-252
	ldw r19, fp+-256
	ldw r20, fp+-260
	ldw r23, fp+-264
	ldw r24, fp+-268
	ldw r25, fp+-272
	ldw r26, fp+-276
.LBB0_154:
	ldw r1, fp+-228
	ldbu r1, r1+0
	bne r1, r18, .LBB0_156
.LBB0_155:
	lui r3, %hi(list)
	addi r3, r3, %lo(list)
	lui r8, %hi(.L.str.8)
	addi r8, r8, %lo(.L.str.8)
	addi r6, r12, -1896
	addi r4, fp, -88
	addi r5, r0, 1500
	addi r7, r0, 20
	jal r31, vfont_text
.LBB0_156:
	ldw r1, fp+-88
	addi r4, r1, 1
	stw fp+-88, r4
	slli r1, r1, 2
	add r1, r1, r15
	stw r1+0, r21
	lui r12, %hi(frame)
	addi r12, r12, %lo(frame)
	ldw r5, r12+0
	add r3, r15, r0
	jal r31, tube_present
	bne r1, r21, .LBB0_159
.LBB0_157:
	ldw r1, fp+-208
	ldbu r1, r1+0
	beq r1, r18, .LBB0_158
	jal r0, .LBB0_21
.LBB0_158:
	ldw r1, r12+0
	ldw r16, fp+-292
	ldw r12, fp+-280
	ldw r13, fp+-308
	ldw r14, fp+-312
	ldw r15, fp+-316
	bgeu r1, r27, .LBB0_160
	jal r0, .LBB0_22
.LBB0_159:
	lui r3, %hi(.L.str.6)
	addi r3, r3, %lo(.L.str.6)
	jal r31, printf
.LBB0_160:
	ldw r1, fp+-208
	ldbu r1, r1+0
	bne r1, r18, .LBB0_165
.LBB0_161:
	addi r1, r0, 0
	addi r3, r0, -784
	lui r4, %hi(rocks+784)
	addi r4, r4, %lo(rocks+784)
	add r8, r1, r0
.LBB0_162:
	add r5, r3, r4
	ldw r5, r5+0
	add r8, r5, r8
	addi r3, r3, 28
	bne r3, r1, .LBB0_162
.LBB0_163:
	lui r1, %hi(frame)
	addi r1, r1, %lo(frame)
	ldw r4, r1+0
	lui r1, %hi(score)
	addi r1, r1, %lo(score)
	ldw r5, r1+0
	ldw r1, fp+-296
	ldw r6, r1+0
	ldw r1, fp+-336
	ldw r7, r1+0
	lui r3, %hi(.L.str.7)
	addi r3, r3, %lo(.L.str.7)
.LBB0_164:
	jal r31, printf
.LBB0_165:
	jal r31, tube_cleanup
.LBB0_166:
	add r1, r28, r0
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
	addi sp, sp, 384
	jalr r0, r31, 0
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
                                        # -- End function
	.p2align	2                               # -- Begin function spawn_wave
	.type	spawn_wave,@function
spawn_wave:                             # @spawn_wave
# %bb.0:
	addi sp, sp, -64
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 64
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
	lui r1, %hi(wave)
	addi r1, r1, %lo(wave)
	ldw r4, r1+0
	addi r1, r0, -2
	blt r4, r1, .LBB1_11
.LBB1_1:
	lui r1, %hi(rng_state)
	addi r1, r1, %lo(rng_state)
	ldw r16, r1+0
	addi r3, r0, 3
	slt r5, r4, r3
	addi r3, r0, 0
	sub r5, r3, r5
	xori r4, r4, 3
	and r4, r4, r5
	xori r4, r4, 3
	addi r4, r4, 2
	lui r5, 128
	addi r6, r5, -256
	lui r7, 64
	lui r8, 256
	addi r8, r8, -1
	lui r9, %hi(rocks)
	addi r9, r9, %lo(rocks)
	addi r10, r0, 1
	lui lr, %hi(rocks+20)
	addi lr, lr, %lo(rocks+20)
	addi r11, r0, 2
	lui r12, 349525
	addi r12, r12, 1366
	lui r13, %hi(rocks+24)
	addi r13, r13, %lo(rocks+24)
	lui r14, %hi(rocks+4)
	addi r14, r14, %lo(rocks+4)
	add r17, r3, r0
	jal r0, .LBB1_4
.LBB1_2:
	stw r20+0, r10
	add r20, r17, lr
	stw r20+0, r11
	slli r20, r16, 13
	xor r16, r20, r16
	srli r20, r16, 17
	xor r16, r20, r16
	slli r20, r16, 5
	xor r16, r20, r16
	srli r20, r16, 8
	mulhu r21, r20, r12
	slli r22, r21, 1
	add r21, r22, r21
	sub r20, r20, r21
	add r21, r17, r13
	stw r21+0, r20
	add r20, r17, r14
	stw r20+0, r19
	lui r19, %hi(rocks+8)
	addi r19, r19, %lo(rocks+8)
	add r19, r17, r19
	stw r19+0, r18
	slli r18, r16, 13
	xor r16, r18, r16
	srli r18, r16, 17
	xor r16, r18, r16
	slli r18, r16, 5
	xor r16, r18, r16
	srli r18, r16, 8
	lui r19, 2731
	addi r19, r19, -1365
	mulhu r20, r18, r19
	srli r20, r20, 3
	lui r21, 1
	addi r21, r21, -1024
	mul r20, r20, r21
	sub r18, r18, r20
	addi r18, r18, -1536
	lui r20, %hi(rocks+12)
	addi r20, r20, %lo(rocks+12)
	add r20, r17, r20
	stw r20+0, r18
	slli r18, r16, 13
	xor r16, r18, r16
	srli r18, r16, 17
	xor r16, r18, r16
	slli r18, r16, 5
	xor r16, r18, r16
	srli r18, r16, 8
	mulhu r19, r18, r19
	srli r19, r19, 3
	mul r19, r19, r21
	sub r18, r18, r19
	addi r18, r18, -1536
	lui r19, %hi(rocks+16)
	addi r19, r19, %lo(rocks+16)
	add r17, r17, r19
	stw r17+0, r18
.LBB1_3:
	addi r17, r15, 1
	beq r15, r4, .LBB1_10
.LBB1_4:
	add r15, r17, r0
	slli r17, r16, 13
	xor r16, r17, r16
	srli r17, r16, 17
	xor r16, r17, r16
	slli r17, r16, 5
	xor r17, r17, r16
	slli r16, r17, 13
	xor r16, r16, r17
	srli r18, r16, 17
	xor r16, r18, r16
	slli r18, r16, 5
	xor r18, r18, r16
	slli r16, r18, 13
	xor r16, r16, r18
	srli r19, r16, 17
	xor r16, r19, r16
	slli r19, r16, 5
	xor r16, r19, r16
	andi r19, r16, 256
	bne r19, r3, .LBB1_6
.LBB1_5:
	and r17, r17, r6
	add r17, r17, r7
	srli r18, r18, 8
	and r18, r18, r8
	jal r0, .LBB1_7
.LBB1_6:
	slli r17, r16, 13
	xor r16, r17, r16
	srli r17, r16, 17
	xor r16, r17, r16
	slli r17, r16, 5
	xor r16, r17, r16
	srli r17, r16, 8
	and r17, r17, r8
	slli r18, r16, 13
	xor r16, r18, r16
	srli r18, r16, 17
	xor r16, r18, r16
	slli r18, r16, 5
	xor r16, r18, r16
	and r18, r16, r6
	add r18, r18, r7
.LBB1_7:
	xor r19, r17, r5
	add r17, r3, r0
.LBB1_8:
	add r20, r17, r9
	ldw r21, r20+0
	beq r21, r3, .LBB1_2
.LBB1_9:
	addi r17, r17, 28
	addi r20, r0, 784
	bne r17, r20, .LBB1_8
	jal r0, .LBB1_3
.LBB1_10:
	stw r1+0, r16
.LBB1_11:
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
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 48
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	addi r1, r0, 0
	lui r6, %hi(rocks)
	addi r6, r6, %lo(rocks)
	addi r7, r0, 784
	add r13, r1, r0
.LBB2_1:
	add r8, r13, r6
	ldw r9, r8+0
	beq r9, r1, .LBB2_3
.LBB2_2:
	addi r13, r13, 28
	bne r13, r7, .LBB2_1
	jal r0, .LBB2_8
.LBB2_3:
	addi r1, r0, 3
	sub r1, r1, r3
	slli r6, r1, 1
	add r1, r6, r1
	addi r15, r1, 3
	addi r1, r0, 1
	stw r8+0, r1
	lui r1, %hi(rocks+20)
	addi r1, r1, %lo(rocks+20)
	add r1, r13, r1
	stw r1+0, r3
	lui r14, %hi(rng_state)
	addi r14, r14, %lo(rng_state)
	ldw r1, r14+0
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	stw r14+0, r1
	srli r3, r1, 8
	lui r6, 349525
	addi r6, r6, 1366
	mulhu r6, r3, r6
	slli r7, r6, 1
	add r6, r7, r6
	sub r3, r3, r6
	lui r6, %hi(rocks+24)
	addi r6, r6, %lo(rocks+24)
	add r6, r13, r6
	stw r6+0, r3
	lui r3, 256
	addi r3, r3, -1
	and r4, r4, r3
	lui r6, %hi(rocks+4)
	addi r6, r6, %lo(rocks+4)
	add r6, r13, r6
	stw r6+0, r4
	and r3, r5, r3
	lui r4, %hi(rocks+8)
	addi r4, r4, %lo(rocks+8)
	add r4, r13, r4
	stw r4+0, r3
	slli r11, r15, 9
	slli r3, r1, 13
	xor r1, r3, r1
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r16, r3, r1
	stw r14+0, r16
	addi r12, r0, 0
	add r1, r12, r0
	beq r11, r12, .LBB2_5
.LBB2_4:
	srli r3, r16, 8
	add r4, r11, r0
	jal r31, __umodsi3
.LBB2_5:
	slli r15, r15, 8
	sub r1, r1, r15
	lui r3, %hi(rocks+12)
	addi r3, r3, %lo(rocks+12)
	add r3, r13, r3
	stw r3+0, r1
	slli r1, r16, 13
	xor r1, r1, r16
	srli r3, r1, 17
	xor r1, r3, r1
	slli r3, r1, 5
	xor r1, r3, r1
	stw r14+0, r1
	beq r11, r12, .LBB2_7
.LBB2_6:
	srli r3, r1, 8
	add r4, r11, r0
	jal r31, __umodsi3
	add r12, r1, r0
.LBB2_7:
	sub r1, r12, r15
	lui r3, %hi(rocks+16)
	addi r3, r3, %lo(rocks+16)
	add r3, r13, r3
	stw r3+0, r1
.LBB2_8:
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
.Lfunc_end2:
	.size	spawn_rock, .Lfunc_end2-spawn_rock
                                        # -- End function
	.p2align	2                               # -- Begin function kill_rock
	.type	kill_rock,@function
kill_rock:                              # @kill_rock
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
	addi r6, r0, 0
	sub r5, r6, r5
	andi r5, r5, 86
	xori r7, r5, 100
	sub r4, r6, r4
	xori r5, r5, 112
	and r4, r5, r4
	xor r4, r7, r4
	lui r5, %hi(score)
	addi r5, r5, %lo(score)
	ldw r7, r5+0
	add r7, r4, r7
	stw r5+0, r7
	lui r4, %hi(next_life)
	addi r4, r4, %lo(next_life)
	ldw r5, r4+0
	bltu r7, r5, .LBB3_2
.LBB3_1:
	lui r7, %hi(lives)
	addi r7, r7, %lo(lives)
	ldw r8, r7+0
	addi r8, r8, 1
	stw r7+0, r8
	lui r7, 2
	addi r7, r7, 1808
	add r5, r5, r7
	stw r4+0, r5
.LBB3_2:
	ldw r4, r11+4
	ldw r5, r11+8
	lui r7, %hi(rng_state)
	addi r7, r7, %lo(rng_state)
	ldw r14, r7+0
	addi r8, r0, 6
	lui r9, %hi(debris+8)
	addi r9, r9, %lo(debris+8)
	lui r10, 104858
	addi r10, r10, -1638
	addi lr, r0, 10
	addi r12, r0, 22
	add r13, r6, r0
.LBB3_3:
	ldw r15, r9+-8
	bgt r15, r6, .LBB3_5
.LBB3_4:
	slli r15, r14, 13
	xor r14, r15, r14
	srli r15, r14, 17
	xor r14, r15, r14
	slli r15, r14, 5
	xor r14, r15, r14
	stw r7+0, r14
	srli r15, r14, 8
	mulhu r16, r15, r10
	mul r16, r16, lr
	sub r15, r15, r16
	addi r15, r15, 12
	stw r9+-8, r15
	stw r9+-4, r4
	stw r9+0, r5
	slli r15, r14, 13
	xor r14, r15, r14
	srli r15, r14, 17
	xor r14, r15, r14
	slli r15, r14, 5
	xor r14, r15, r14
	stw r7+0, r14
	srli r15, r14, 8
	andi r15, r15, 4095
	addi r15, r15, -2048
	stw r9+4, r15
	slli r15, r14, 13
	xor r14, r15, r14
	srli r15, r14, 17
	xor r14, r15, r14
	slli r15, r14, 5
	xor r14, r15, r14
	stw r7+0, r14
	srli r15, r14, 8
	andi r15, r15, 4095
	addi r15, r15, -2048
	stw r9+8, r15
	addi r8, r8, -1
.LBB3_5:
	bgtu r13, r12, .LBB3_7
.LBB3_6:
	addi r13, r13, 1
	addi r9, r9, 20
	bgt r8, r6, .LBB3_3
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
	addi r1, r0, 0
	stw r11+0, r1
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
.Lfunc_end3:
	.size	kill_rock, .Lfunc_end3-kill_rock
                                        # -- End function
	.p2align	2                               # -- Begin function kill_ship
	.type	kill_ship,@function
kill_ship:                              # @kill_ship
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 32
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	lui r1, %hi(px)
	addi r1, r1, %lo(px)
	ldw r1, r1+0
	lui r3, %hi(py)
	addi r3, r3, %lo(py)
	ldw r3, r3+0
	lui r4, %hi(rng_state)
	addi r4, r4, %lo(rng_state)
	ldw r11, r4+0
	addi r5, r0, 10
	addi r6, r0, 0
	lui r7, %hi(debris+8)
	addi r7, r7, %lo(debris+8)
	lui r8, 104858
	addi r8, r8, -1638
	addi r9, r0, 22
	add r10, r6, r0
	add lr, r5, r0
.LBB4_1:
	ldw r12, r7+-8
	bgt r12, r6, .LBB4_3
.LBB4_2:
	slli r12, r11, 13
	xor r11, r12, r11
	srli r12, r11, 17
	xor r11, r12, r11
	slli r12, r11, 5
	xor r11, r12, r11
	stw r4+0, r11
	srli r12, r11, 8
	mulhu r13, r12, r8
	mul r13, r13, r5
	sub r12, r12, r13
	addi r12, r12, 12
	stw r7+-8, r12
	stw r7+-4, r1
	stw r7+0, r3
	slli r12, r11, 13
	xor r11, r12, r11
	srli r12, r11, 17
	xor r11, r12, r11
	slli r12, r11, 5
	xor r11, r12, r11
	stw r4+0, r11
	srli r12, r11, 8
	andi r12, r12, 4095
	addi r12, r12, -2048
	stw r7+4, r12
	slli r12, r11, 13
	xor r11, r12, r11
	srli r12, r11, 17
	xor r11, r12, r11
	slli r12, r11, 5
	xor r11, r12, r11
	stw r4+0, r11
	srli r12, r11, 8
	andi r12, r12, 4095
	addi r12, r12, -2048
	stw r7+8, r12
	addi lr, lr, -1
.LBB4_3:
	bgtu r10, r9, .LBB4_5
.LBB4_4:
	addi r10, r10, 1
	addi r7, r7, 20
	bgt lr, r6, .LBB4_1
.LBB4_5:
	lui r1, %hi(alive)
	addi r1, r1, %lo(alive)
	addi r3, r0, 0
	stb r1+0, r3
	lui r1, %hi(lives)
	addi r1, r1, %lo(lives)
	ldw r3, r1+0
	addi r4, r3, -1
	stw r1+0, r4
	lui r1, %hi(respawn_timer)
	addi r1, r1, %lo(respawn_timer)
	addi r4, r0, 60
	stw r1+0, r4
	addi r1, r0, 1
	bgt r3, r1, .LBB4_7
.LBB4_6:
	lui r3, %hi(game_over)
	addi r3, r3, %lo(game_over)
	stb r3+0, r1
.LBB4_7:
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
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

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e34f541beea69553ff1fd655361b4faa1e656dc2)"
	.section	".note.GNU-stack","",@progbits
