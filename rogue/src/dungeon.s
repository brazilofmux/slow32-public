	.file	"dungeon.c"
	.text
	.globl	room_at                         # -- Begin function room_at
	.p2align	2
	.type	room_at,@function
room_at:                                # @room_at
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
	addi r6, r0, 0
	lui r1, 1
	addi r7, r1, -572
	addi r8, r1, -556
	addi r9, r0, 4
	addi r10, r1, -564
	addi lr, r1, -568
	addi r11, r1, -560
	addi r13, r0, 1
	addi r12, r0, -1
	addi r14, r0, 180
	add r15, r6, r0
                                        # implicit-def: $r17
	add r16, r6, r0
.LBB0_1:
	add r1, r3, r15
	add r18, r1, r8
	ldw r18, r18+0
	beq r18, r6, .LBB0_3
.LBB0_2:
	add r1, r17, r0
	add r18, r9, r0
	jal r0, .LBB0_8
.LBB0_3:
	add r18, r1, r7
	ldw r18, r18+0
	blt r4, r18, .LBB0_7
.LBB0_4:
	add r19, r1, r10
	ldw r19, r19+0
	add r18, r18, r19
	addi r18, r18, 1
	bgt r4, r18, .LBB0_7
.LBB0_5:
	add r18, r1, lr
	ldw r18, r18+0
	blt r5, r18, .LBB0_7
.LBB0_6:
	add r1, r1, r11
	ldw r1, r1+0
	add r1, r18, r1
	addi r19, r1, 1
	add r1, r16, r0
	add r18, r13, r0
	ble r5, r19, .LBB0_8
.LBB0_7:
	add r1, r17, r0
	add r18, r6, r0
.LBB0_8:
	ori  r17, r18, 4
	bne r17, r9, .LBB0_11
.LBB0_9:
	addi r16, r16, 1
	addi r15, r15, 20
	add r17, r1, r0
	bne r15, r14, .LBB0_1
.LBB0_10:
	add r1, r12, r0
.LBB0_11:
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
.Lfunc_end0:
	.size	room_at, .Lfunc_end0-room_at
                                        # -- End function
	.globl	new_level                       # -- Begin function new_level
	.p2align	2
	.type	new_level,@function
new_level:                              # @new_level
# %bb.0:
	addi sp, sp, -192
	stw sp+0, lr
	stw sp+4, fp
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
	stw sp+144, r22
	stw sp+140, r23
	stw sp+136, r24
	stw sp+132, r25
	stw sp+128, r26
	stw sp+124, r27
	stw sp+120, r28
	add r11, r3, r0
	addi r14, r3, 4
	addi r4, r0, 32
	addi r12, r0, 1760
	add r3, r14, r0
	stw sp+108, r4
	add r5, r12, r0
	jal r31, memset
	addi r3, r11, 1764
	addi r16, r0, 0
	add r4, r16, r0
	add r5, r12, r0
	jal r31, memset
	addi r4, r0, 3
	add r3, r11, r0
	stw sp+100, r4
	jal r31, rnd
	add r17, r1, r0
	lui r1, 1
	stw sp+68, r1
	addi r20, r1, -572
	add r7, r11, r20
	addi r12, r11, 85
	addi r1, r0, 20
	stw sp+116, r1
	lui r1, 699051
	addi r21, r1, -1365
	addi r1, r0, 7
	stw sp+84, r1
	addi r24, r0, -2
	addi r1, r0, 26
	stw sp+88, r1
	addi r22, r0, 1
	addi r1, r0, 4
	stw sp+112, r1
	addi r1, r0, 23
	stw sp+48, r1
	addi r25, r0, 9
	addi r19, r0, 18
	addi r1, r0, 5
	stw sp+80, r1
	addi r1, r0, 24
	stw sp+76, r1
	addi fp, r0, 80
	addi r26, r0, 124
	addi r13, r0, 46
	addi r27, r0, 45
	add r28, r16, r0
	stw sp+104, r7
	jal r0, .LBB1_3
.LBB1_1:
	stw r23+16, r22
	add r3, r11, r0
	ldw r4, sp+112
	jal r31, rnd
	add r1, r15, r1
	addi r1, r1, 1
	stw r23+0, r1
	add r3, r11, r0
	ldw r4, sp+48
	jal r31, rnd
	add r1, r1, r18
	addi r1, r1, 1
	stw r23+4, r1
	stw r23+12, r16
	stw r23+8, r16
	addi r17, r17, -1
	ldw r7, sp+104
.LBB1_2:
	addi r28, r28, 1
	beq r28, r25, .LBB1_13
.LBB1_3:
	ldw r1, sp+116
	mul r1, r28, r1
	add r23, r7, r1
	mulhu r1, r28, r21
	srli r3, r1, 1
	ldw r4, sp+84
	mul r15, r3, r4
	and r1, r1, r24
	add r1, r1, r3
	sub r1, r28, r1
	ldw r3, sp+88
	mul r18, r1, r3
	stw r23+16, r16
	blt r17, r22, .LBB1_5
.LBB1_4:
	add r3, r11, r0
	ldw r4, sp+100
	jal r31, rnd
	beq r1, r16, .LBB1_1
.LBB1_5:
	add r3, r11, r0
	ldw r4, sp+100
	jal r31, rnd
	addi r1, r1, 2
	stw r23+8, r1
	add r3, r11, r0
	add r4, r19, r0
	jal r31, rnd
	addi r1, r1, 4
	stw r23+12, r1
	ldw r1, r23+8
	ldw r3, sp+80
	sub r4, r3, r1
	add r3, r11, r0
	jal r31, rnd
	add r1, r1, r15
	stw r23+0, r1
	ldw r1, r23+12
	ldw r3, sp+76
	sub r4, r3, r1
	add r3, r11, r0
	jal r31, rnd
	add r1, r1, r18
	stw r23+4, r1
	ldw r3, r23+12
	add r3, r1, r3
	addi r3, r3, 1
	bgt r1, r3, .LBB1_7
.LBB1_6:
	add r3, r1, r0
	ldw r1, r23+0
	mul r1, r1, fp
	add r1, r3, r1
	add r1, r11, r1
	stb r1+4, r27
	ldw r1, r23+0
	ldw r4, r23+8
	mul r1, r1, fp
	mul r4, r4, fp
	add r1, r1, r4
	add r1, r3, r1
	add r1, r11, r1
	stb r1+84, r27
	addi r1, r3, 1
	ldw r4, r23+4
	ldw r5, r23+12
	add r4, r4, r5
	addi r4, r4, 1
	blt r3, r4, .LBB1_6
.LBB1_7:
	ldw r1, r23+8
	ldw r7, sp+104
	blt r1, r22, .LBB1_2
.LBB1_8:
	ldw r1, r23+0
	mul r3, r1, fp
	add r3, r12, r3
	jal r0, .LBB1_10
.LBB1_9:
	ldw r4, r23+0
	ldw r5, r23+8
	add r4, r5, r4
	addi r3, r3, 80
	bge r1, r4, .LBB1_2
.LBB1_10:
	addi r1, r1, 1
	mul r4, r1, fp
	add r4, r14, r4
	ldw r5, r23+4
	add r5, r4, r5
	stb r5+0, r26
	ldw r5, r23+4
	ldw r6, r23+12
	add r4, r4, r5
	add r4, r4, r6
	stb r4+1, r26
	ldw r4, r23+12
	blt r4, r22, .LBB1_9
.LBB1_11:
	ldw r4, r23+4
.LBB1_12:
	addi r5, r4, 1
	add r4, r3, r4
	stb r4+0, r13
	ldw r4, r23+4
	ldw r6, r23+12
	add r6, r6, r4
	add r4, r5, r0
	blt r5, r6, .LBB1_12
	jal r0, .LBB1_9
.LBB1_13:
	addi r16, r0, 0
	ldw r15, sp+68
	jal r0, .LBB1_15
.LBB1_14:
	add r16, r17, r0
	beq r17, r25, .LBB1_19
.LBB1_15:
	mulhu r1, r16, r21
	srli r3, r1, 1
	and r1, r1, r24
	add r1, r1, r3
	addi r1, r1, 2
	addi r17, r16, 1
	beq r1, r16, .LBB1_17
.LBB1_16:
	add r3, r11, r0
	add r4, r16, r0
	add r5, r17, r0
	jal r31, connect_rooms
.LBB1_17:
	addi r1, r16, -6
	ldw r3, sp+100
	bltu r1, r3, .LBB1_14
.LBB1_18:
	addi r5, r16, 3
	add r3, r11, r0
	add r4, r16, r0
	jal r31, connect_rooms
	jal r0, .LBB1_14
.LBB1_19:
	addi r1, r0, 0
	addi r3, r15, -556
	addi r4, r0, 180
	addi r12, r15, -568
	addi r5, r0, 35
	add r6, r1, r0
	ldw r9, sp+108
	jal r0, .LBB1_21
.LBB1_20:
	addi r6, r6, 20
	beq r6, r4, .LBB1_24
.LBB1_21:
	add r7, r11, r6
	add r8, r7, r3
	ldw r8, r8+0
	beq r8, r1, .LBB1_20
.LBB1_22:
	add r8, r7, r20
	ldw r8, r8+0
	mul r8, r8, fp
	add r8, r14, r8
	add r7, r7, r12
	ldw r7, r7+0
	add r7, r8, r7
	ldbu r8, r7+0
	bne r8, r9, .LBB1_20
.LBB1_23:
	stb r7+0, r5
	jal r0, .LBB1_20
.LBB1_24:
	addi r1, r15, 504
	stw sp+32, r1
	add r17, r11, r1
	addi r1, r15, 508
	add r18, r11, r1
	add r3, r11, r0
	add r4, r17, r0
	add r5, r18, r0
	jal r31, rand_floor
	ldw r1, r17+0
	mul r1, r1, fp
	add r1, r14, r1
	ldw r3, r18+0
	add r1, r1, r3
	addi r3, r0, 37
	stb r1+0, r3
	addi r1, r15, 512
	add r16, r11, r1
	addi r1, r15, 516
	add r19, r11, r1
.LBB1_25:
	add r3, r11, r0
	add r4, r16, r0
	add r5, r19, r0
	jal r31, rand_floor
	ldw r1, r16+0
	ldw r3, r17+0
	bne r1, r3, .LBB1_27
.LBB1_26:
	ldw r1, r19+0
	ldw r3, r18+0
	beq r1, r3, .LBB1_25
.LBB1_27:
	stw sp+36, r19
	stw sp+64, r16
	addi r17, r0, 4
	add r3, r11, r0
	add r4, r17, r0
	jal r31, rnd
	addi r3, r15, 548
	add r3, r11, r3
	stw sp+112, r3
	ldw r3, r3+0
	addi r4, r15, -392
	add r5, r11, r4
	stw sp+40, r5
	addi r6, r0, -1
	addi r25, r15, -72
	ldw r18, sp+104
.LBB1_28:
	add r5, r11, r4
	stw r5+0, r6
	addi r4, r4, 20
	bne r4, r25, .LBB1_28
.LBB1_29:
	stw sp+28, r6
	srai r4, r3, 31
	srli r4, r4, 30
	add r3, r3, r4
	srai r3, r3, 2
	add r1, r1, r3
	addi r1, r1, 3
	blt r1, r22, .LBB1_58
.LBB1_30:
	xori r3, r1, 16
	addi r4, r0, 16
	slt r1, r1, r4
	sub r1, r0, r1
	and r1, r3, r1
	xori r1, r1, 16
	stw sp+56, r1
	lui r1, %hi(mon_table_len)
	addi r1, r1, %lo(mon_table_len)
	ldw r1, r1+0
	stw sp+108, r1
	add r1, r11, r12
	stw sp+44, r1
	addi r26, r0, 0
	addi r12, r0, 40
	lui r21, %hi(mon_table)
	addi r21, r21, %lo(mon_table)
	lui r16, %hi(mon_table+8)
	addi r16, r16, %lo(mon_table+8)
	addi r15, r0, 100
	addi r1, r0, 99
	stw sp+60, r1
	addi r1, r0, 1000
	stw sp+52, r1
	addi r19, r0, 9
	add r24, r26, r0
                                        # implicit-def: $r1
                                        # kill: killed $r1
                                        # implicit-def: $r28
	jal r0, .LBB1_35
.LBB1_31:
	ldw r1, sp+116
	mul r1, r24, r1
	ldw r3, sp+40
	add r27, r3, r1
	stw r27+0, r20
	stw r27+4, r28
	stw r27+8, r7
	ldw r4, r23+16
	addi r5, r0, 8
	add r3, r11, r0
	jal r31, roll
	stw r27+12, r1
	ldw r1, r23+36
	addi r20, r0, 0
	beq r1, r20, .LBB1_57
.LBB1_32:
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, rnd
	seq r1, r1, r20
.LBB1_33:
	stw r27+16, r1
	addi r24, r24, 1
.LBB1_34:
	ldw r1, sp+56
	bge r24, r1, .LBB1_58
.LBB1_35:
	add r27, r26, r0
	jal r0, .LBB1_37
.LBB1_36:
	addi r27, r27, 1
	beq r27, r15, .LBB1_40
.LBB1_37:
	add r3, r11, r0
	ldw r4, sp+108
	jal r31, rnd
	add r20, r1, r0
	ldw r1, sp+112
	ldw r1, r1+0
	mul r3, r20, r12
	add r23, r3, r21
	add r3, r3, r16
	ldw r3, r3+0
	blt r1, r3, .LBB1_36
.LBB1_38:
	ldw r3, r23+12
	bgt r1, r3, .LBB1_36
.LBB1_39:
	ldw r1, sp+60
	bleu r27, r1, .LBB1_41
	jal r0, .LBB1_58
.LBB1_40:
	add r27, r15, r0
	ldw r1, sp+60
	bgtu r27, r1, .LBB1_58
.LBB1_41:
	stw sp+96, r28
	stw sp+72, r24
	ldw r27, sp+52
.LBB1_42:
	add r3, r11, r0
	add r4, r19, r0
	jal r31, rnd
	ldw r3, sp+116
	mul r1, r1, r3
	add r24, r18, r1
	ldw r1, r24+16
	beq r1, r26, .LBB1_44
.LBB1_43:
	add r3, r17, r0
	jal r0, .LBB1_46
.LBB1_44:
	ldw r28, r24+0
	ldw r4, r24+8
	add r3, r11, r0
	jal r31, rnd
	add r1, r28, r1
	addi r28, r1, 1
	add r18, r22, r0
	ldw r22, r24+4
	ldw r4, r24+12
	add r3, r11, r0
	jal r31, rnd
	add r1, r22, r1
	add r22, r18, r0
	ldw r18, sp+104
	addi r1, r1, 1
	mul r3, r28, fp
	add r3, r14, r3
	add r3, r3, r1
	ldbu r4, r3+0
	add r3, r26, r0
	bne r4, r13, .LBB1_46
.LBB1_45:
	stw sp+96, r28
	stw sp+92, r1
	add r3, r22, r0
.LBB1_46:
	ori  r1, r3, 4
	bne r1, r17, .LBB1_49
.LBB1_47:
	addi r27, r27, -1
	addi r1, r0, 0
	bne r27, r1, .LBB1_42
.LBB1_48:
	ldw r1, r18+0
	addi r28, r1, 1
	ldw r1, sp+44
	ldw r1, r1+0
	addi r7, r1, 1
	jal r0, .LBB1_50
.LBB1_49:
	ldw r28, sp+96
	ldw r7, sp+92
.LBB1_50:
	ldw r1, sp+64
	ldw r1, r1+0
	ldw r6, sp+68
	ldw r24, sp+72
	stw sp+92, r7
	bne r28, r1, .LBB1_52
.LBB1_51:
	ldw r1, sp+36
	ldw r1, r1+0
	beq r7, r1, .LBB1_34
.LBB1_52:
	addi r1, r0, 0
	add r3, r1, r0
	jal r0, .LBB1_54
.LBB1_53:
	addi r3, r3, 20
	addi r4, r0, 320
	beq r3, r4, .LBB1_31
.LBB1_54:
	add r4, r11, r3
	addi r5, r6, -392
	add r5, r4, r5
	ldw r5, r5+0
	blt r5, r1, .LBB1_53
.LBB1_55:
	addi r5, r6, -388
	add r5, r4, r5
	ldw r5, r5+0
	bne r5, r28, .LBB1_53
.LBB1_56:
	addi r5, r6, -384
	add r4, r4, r5
	ldw r4, r4+0
	bne r4, r7, .LBB1_53
	jal r0, .LBB1_34
.LBB1_57:
	addi r1, r0, 1
	jal r0, .LBB1_33
.LBB1_58:
	addi r14, r0, 4
	add r3, r11, r0
	add r4, r14, r0
	jal r31, rnd
	add r3, r11, r25
	stw sp+64, r3
	addi r13, r0, 0
	ldw r4, sp+32
.LBB1_59:
	add r3, r11, r25
	stw r3+0, r13
	addi r25, r25, 24
	bne r25, r4, .LBB1_59
.LBB1_60:
	ldw r3, sp+28
	blt r1, r3, .LBB1_77
.LBB1_61:
	addi r17, r1, 2
	addi r12, r0, 0
	ldw r1, sp+68
	addi r19, r1, -72
	addi r15, r0, 100
	addi r20, r1, -56
	addi r21, r1, -52
	addi r23, r1, -64
	addi r25, r1, -60
	addi r26, r0, 34
	addi r3, r0, 10
	stw sp+116, r3
	addi r28, r0, 54
	addi r1, r1, -68
	stw sp+108, r1
	addi r1, r0, 69
	stw sp+104, r1
	addi r1, r0, 79
	stw sp+96, r1
	addi r1, r0, 2
	stw sp+92, r1
	addi r1, r0, 89
	stw sp+72, r1
	addi r1, r0, 6
	stw sp+60, r1
	add r18, r12, r0
	add r13, r12, r0
	jal r0, .LBB1_65
.LBB1_62:
	ldw r1, sp+100
	stw r27+0, r1
.LBB1_63:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, rnd
	ldw r3, sp+108
	add r3, fp, r3
	stw r3+0, r1
.LBB1_64:
	addi r13, r13, 1
	addi r18, r18, 24
	beq r17, r13, .LBB1_77
.LBB1_65:
	add fp, r11, r18
	add r27, fp, r19
	add r3, r11, r0
	add r4, r15, r0
	jal r31, rnd
	add r16, r1, r0
	add r4, fp, r20
	add r5, fp, r21
	add r3, r11, r0
	jal r31, rand_floor
	add r24, fp, r23
	stw r24+0, r22
	add r1, fp, r25
	stw r1+0, r12
	bgt r16, r26, .LBB1_68
.LBB1_66:
	stw r27+0, r22
	ldw r1, sp+112
	ldw r1, r1+0
	ldw r3, sp+116
	mul r1, r1, r3
	addi r4, r1, 25
	add r3, r11, r0
	jal r31, rnd
	addi r1, r1, 5
.LBB1_67:
	stw r24+0, r1
	jal r0, .LBB1_64
.LBB1_68:
	ble r16, r28, .LBB1_62
.LBB1_69:
	ldw r1, sp+104
	bgt r16, r1, .LBB1_71
.LBB1_70:
	stw r27+0, r14
	jal r0, .LBB1_63
.LBB1_71:
	ldw r1, sp+96
	bgt r16, r1, .LBB1_73
.LBB1_72:
	ldw r1, sp+92
	stw r27+0, r1
	jal r0, .LBB1_64
.LBB1_73:
	ldw r1, sp+108
	add r24, fp, r1
	ldw r1, sp+72
	bgt r16, r1, .LBB1_75
.LBB1_74:
	ldw r1, sp+80
	jal r0, .LBB1_76
.LBB1_75:
	ldw r1, sp+60
.LBB1_76:
	stw r27+0, r1
	add r3, r11, r0
	add r4, r14, r0
	jal r31, rnd
	jal r0, .LBB1_67
.LBB1_77:
	ldw r1, sp+112
	ldw r1, r1+0
	ldw r3, sp+88
	blt r1, r3, .LBB1_81
.LBB1_78:
	ldw r1, sp+68
	addi r1, r1, 568
	add r1, r11, r1
	ldw r3, r1+0
	addi r1, r0, 0
	bne r3, r1, .LBB1_81
.LBB1_79:
	ldw r3, sp+48
	bgtu r13, r3, .LBB1_81
.LBB1_80:
	ldw r3, sp+76
	mul r3, r13, r3
	ldw r4, sp+64
	add r3, r4, r3
	ldw r4, sp+84
	stw r3+0, r4
	stw r3+4, r1
	stw r3+8, r22
	stw r3+12, r1
	addi r4, r3, 16
	addi r5, r3, 20
	add r3, r11, r0
	jal r31, rand_floor
.LBB1_81:
	add r3, r11, r0
	jal r31, mark_seen
	ldw r28, sp+120
	ldw r27, sp+124
	ldw r26, sp+128
	ldw r25, sp+132
	ldw r24, sp+136
	ldw r23, sp+140
	ldw r22, sp+144
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
	ldw fp, sp+4
	addi sp, sp, 192
	jalr r0, r31, 0
.Lfunc_end1:
	.size	new_level, .Lfunc_end1-new_level
                                        # -- End function
	.p2align	2                               # -- Begin function connect_rooms
	.type	connect_rooms,@function
connect_rooms:                          # @connect_rooms
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
	add r12, r5, r0
	add r11, r3, r0
	sub r19, r5, r4
	lui r18, 1
	addi r13, r18, -572
	add r16, r3, r13
	addi r21, r0, 20
	mul r1, r4, r21
	add r17, r16, r1
	ldw r1, r17+16
	ldw r15, r17+0
	addi r20, r0, 3
	addi r14, r0, 0
	bne r19, r20, .LBB2_3
.LBB2_1:
	addi r22, r0, 0
	beq r1, r22, .LBB2_5
.LBB2_2:
	ldw r17, r17+4
	jal r0, .LBB2_6
.LBB2_3:
	beq r1, r14, .LBB2_8
.LBB2_4:
	ldw r17, r17+4
	jal r0, .LBB2_9
.LBB2_5:
	ldw r1, r17+8
	add r1, r15, r1
	addi r15, r1, 1
	ldw r23, r17+4
	ldw r4, r17+12
	add r3, r11, r0
	jal r31, rnd
	add r1, r23, r1
	addi r17, r1, 1
.LBB2_6:
	mul r1, r12, r21
	add r1, r16, r1
	ldw r3, r1+16
	ldw r12, r1+0
	ldw r16, r1+4
	bne r3, r22, .LBB2_12
.LBB2_7:
	ldw r4, r1+12
	add r3, r11, r0
	jal r31, rnd
	add r1, r16, r1
	addi r16, r1, 1
	jal r0, .LBB2_12
.LBB2_8:
	ldw r4, r17+8
	add r3, r11, r0
	jal r31, rnd
	add r1, r15, r1
	addi r15, r1, 1
	ldw r1, r17+4
	ldw r3, r17+12
	add r1, r1, r3
	addi r17, r1, 1
.LBB2_9:
	mul r1, r12, r21
	add r16, r16, r1
	ldw r1, r16+16
	ldw r12, r16+0
	bne r1, r14, .LBB2_11
.LBB2_10:
	ldw r4, r16+8
	add r3, r11, r0
	jal r31, rnd
	add r1, r12, r1
	addi r12, r1, 1
.LBB2_11:
	ldw r16, r16+4
.LBB2_12:
	addi r1, r11, 4
	addi r4, r0, 80
	mul r6, r15, r4
	add r3, r1, r6
	add r3, r3, r17
	addi r7, r0, 43
	stb r3+0, r7
	mul r5, r12, r4
	add r1, r1, r5
	add r1, r1, r16
	stb r1+0, r7
	bne r19, r20, .LBB2_17
.LBB2_13:
	add r5, r12, r15
	srli r6, r5, 31
	add r5, r5, r6
	srai r8, r5, 1
	addi r5, r15, 1
	xor r6, r5, r8
	slt r7, r5, r8
	sub r7, r0, r7
	and r6, r6, r7
	xor r6, r8, r6
	sub r7, r8, r5
	sub r9, r5, r8
	xor r9, r9, r7
	sgt r5, r5, r8
	sub r5, r0, r5
	and r5, r9, r5
	xor r5, r7, r5
	addi r9, r5, 1
	mul r5, r6, r4
	add r5, r17, r5
	add r5, r5, r11
	addi r10, r5, 4
	addi r5, r0, 32
	addi r6, r0, 35
	addi r7, r0, 0
	jal r0, .LBB2_15
.LBB2_14:
	addi r9, r9, -1
	addi r10, r10, 80
	beq r9, r7, .LBB2_21
.LBB2_15:
	ldbu lr, r10+0
	bne lr, r5, .LBB2_14
.LBB2_16:
	stb r10+0, r6
	jal r0, .LBB2_14
.LBB2_17:
	add r7, r16, r17
	srli r8, r7, 31
	add r7, r7, r8
	srai r8, r7, 1
	addi r7, r17, 1
	xor r9, r7, r8
	slt r10, r7, r8
	sub r10, r0, r10
	and r9, r9, r10
	xor r10, r8, r9
	sub r9, r8, r7
	sub lr, r7, r8
	xor lr, lr, r9
	sgt r7, r7, r8
	sub r7, r0, r7
	and r7, lr, r7
	xor r7, r9, r7
	addi r9, r7, 1
	add r6, r10, r6
	add r6, r6, r11
	addi r10, r6, 4
	addi r6, r0, 32
	addi r7, r0, 35
	jal r0, .LBB2_19
.LBB2_18:
	addi r9, r9, -1
	addi r10, r10, 1
	beq r9, r14, .LBB2_25
.LBB2_19:
	ldbu lr, r10+0
	bne lr, r6, .LBB2_18
.LBB2_20:
	stb r10+0, r7
	jal r0, .LBB2_18
.LBB2_21:
	xor r9, r17, r16
	slt r10, r17, r16
	sub r10, r0, r10
	and r9, r9, r10
	xor r10, r16, r9
	sub r9, r16, r17
	sub lr, r17, r16
	xor lr, lr, r9
	sgt r19, r17, r16
	sub r19, r0, r19
	and lr, lr, r19
	xor r9, r9, lr
	addi r9, r9, 1
	mul lr, r8, r4
	add r10, r10, lr
	add r10, r10, r11
	addi r10, r10, 4
	jal r0, .LBB2_23
.LBB2_22:
	addi r9, r9, -1
	addi r10, r10, 1
	beq r9, r7, .LBB2_29
.LBB2_23:
	ldbu lr, r10+0
	bne lr, r5, .LBB2_22
.LBB2_24:
	stb r10+0, r6
	jal r0, .LBB2_22
.LBB2_25:
	xor r9, r15, r12
	slt r10, r15, r12
	sub r10, r0, r10
	and r9, r9, r10
	xor r10, r12, r9
	sub r9, r12, r15
	sub lr, r15, r12
	xor lr, lr, r9
	sgt r19, r15, r12
	sub r19, r0, r19
	and lr, lr, r19
	xor r9, r9, lr
	addi r9, r9, 1
	mul r4, r10, r4
	add r4, r8, r4
	add r4, r4, r11
	addi r4, r4, 4
	jal r0, .LBB2_27
.LBB2_26:
	addi r9, r9, -1
	addi r4, r4, 80
	beq r9, r14, .LBB2_33
.LBB2_27:
	ldbu r10, r4+0
	bne r10, r6, .LBB2_26
.LBB2_28:
	stb r4+0, r7
	jal r0, .LBB2_26
.LBB2_29:
	addi r9, r12, -1
	xor r10, r8, r9
	slt lr, r8, r9
	sub lr, r0, lr
	and r10, r10, lr
	xor r10, r9, r10
	sub lr, r9, r8
	sub r19, r8, r9
	xor r19, r19, lr
	sgt r8, r8, r9
	sub r8, r0, r8
	and r8, r19, r8
	xor r8, lr, r8
	addi r8, r8, 1
	mul r4, r10, r4
	add r4, r16, r4
	add r4, r4, r11
	addi r4, r4, 4
	jal r0, .LBB2_31
.LBB2_30:
	addi r8, r8, -1
	addi r4, r4, 80
	beq r8, r7, .LBB2_37
.LBB2_31:
	ldbu r9, r4+0
	bne r9, r5, .LBB2_30
.LBB2_32:
	stb r4+0, r6
	jal r0, .LBB2_30
.LBB2_33:
	addi r4, r16, -1
	xor r9, r8, r4
	slt r10, r8, r4
	sub r10, r0, r10
	and r9, r9, r10
	xor r9, r4, r9
	sub r10, r4, r8
	sub lr, r8, r4
	xor lr, lr, r10
	sgt r4, r8, r4
	sub r4, r0, r4
	and r4, lr, r4
	xor r4, r10, r4
	addi r4, r4, 1
	add r5, r9, r5
	add r5, r5, r11
	addi r5, r5, 4
	jal r0, .LBB2_35
.LBB2_34:
	addi r4, r4, -1
	addi r5, r5, 1
	beq r4, r14, .LBB2_37
.LBB2_35:
	ldbu r8, r5+0
	bne r8, r6, .LBB2_34
.LBB2_36:
	stb r5+0, r7
	jal r0, .LBB2_34
.LBB2_37:
	addi r4, r18, -556
	addi r10, r0, 4
	addi r5, r18, -564
	addi r6, r18, -568
	addi r7, r18, -560
	addi lr, r0, 1
	addi r9, r0, -1
	addi r8, r0, 180
	add r18, r14, r0
                                        # implicit-def: $r20
	add r19, r14, r0
.LBB2_38:
	add r21, r11, r18
	add r22, r21, r4
	ldw r22, r22+0
	beq r22, r14, .LBB2_40
.LBB2_39:
	add r21, r20, r0
	add r22, r10, r0
	jal r0, .LBB2_45
.LBB2_40:
	add r22, r21, r13
	ldw r22, r22+0
	blt r15, r22, .LBB2_44
.LBB2_41:
	add r23, r21, r5
	ldw r23, r23+0
	add r22, r22, r23
	addi r22, r22, 1
	bgt r15, r22, .LBB2_44
.LBB2_42:
	add r22, r21, r6
	ldw r22, r22+0
	blt r17, r22, .LBB2_44
.LBB2_43:
	add r21, r21, r7
	ldw r21, r21+0
	add r21, r22, r21
	addi r23, r21, 1
	add r21, r19, r0
	add r22, lr, r0
	ble r17, r23, .LBB2_45
.LBB2_44:
	add r21, r20, r0
	add r22, r14, r0
.LBB2_45:
	ori  r20, r22, 4
	bne r20, r10, .LBB2_48
.LBB2_46:
	addi r19, r19, 1
	addi r18, r18, 20
	add r20, r21, r0
	bne r18, r8, .LBB2_38
.LBB2_47:
	add r21, r9, r0
.LBB2_48:
	bgt r21, r9, .LBB2_50
.LBB2_49:
	addi r9, r0, 35
	stb r3+0, r9
.LBB2_50:
	addi r9, r0, 0
	addi r10, r0, 4
	addi lr, r0, 1
	addi r3, r0, -1
	add r14, r9, r0
                                        # implicit-def: $r17
	add r15, r9, r0
.LBB2_51:
	add r18, r11, r14
	add r19, r18, r4
	ldw r19, r19+0
	beq r19, r9, .LBB2_53
.LBB2_52:
	add r18, r17, r0
	add r19, r10, r0
	jal r0, .LBB2_58
.LBB2_53:
	add r19, r18, r13
	ldw r19, r19+0
	blt r12, r19, .LBB2_57
.LBB2_54:
	add r20, r18, r5
	ldw r20, r20+0
	add r19, r19, r20
	addi r19, r19, 1
	bgt r12, r19, .LBB2_57
.LBB2_55:
	add r19, r18, r6
	ldw r19, r19+0
	blt r16, r19, .LBB2_57
.LBB2_56:
	add r18, r18, r7
	ldw r18, r18+0
	add r18, r19, r18
	addi r20, r18, 1
	add r18, r15, r0
	add r19, lr, r0
	ble r16, r20, .LBB2_58
.LBB2_57:
	add r18, r17, r0
	add r19, r9, r0
.LBB2_58:
	ori  r17, r19, 4
	bne r17, r10, .LBB2_61
.LBB2_59:
	addi r15, r15, 1
	addi r14, r14, 20
	add r17, r18, r0
	bne r14, r8, .LBB2_51
.LBB2_60:
	add r18, r3, r0
.LBB2_61:
	bgt r18, r3, .LBB2_63
.LBB2_62:
	addi r3, r0, 35
	stb r1+0, r3
.LBB2_63:
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
.Lfunc_end2:
	.size	connect_rooms, .Lfunc_end2-connect_rooms
                                        # -- End function
	.p2align	2                               # -- Begin function rand_floor
	.type	rand_floor,@function
rand_floor:                             # @rand_floor
# %bb.0:
	addi sp, sp, -96
	stw sp+0, lr
	stw sp+92, r11
	stw sp+88, r12
	stw sp+84, r13
	stw sp+80, r14
	stw sp+76, r15
	stw sp+72, r16
	stw sp+68, r17
	stw sp+64, r18
	stw sp+60, r19
	stw sp+56, r20
	stw sp+52, r21
	stw sp+48, r22
	stw sp+44, r23
	stw sp+40, r24
	stw sp+36, r25
	stw sp+32, r26
	stw sp+28, r27
	add r11, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	lui r15, 1
	addi r1, r15, -572
	add r16, r3, r1
	addi r17, r3, 4
	addi r18, r0, 1000
	addi r14, r0, 9
	addi r19, r0, 20
	addi r20, r0, 4
	addi r21, r0, 0
	addi r22, r0, 80
	addi r23, r0, 46
	addi r24, r0, 1
.LBB3_1:
	add r3, r12, r0
	add r4, r14, r0
	jal r31, rnd
	mul r1, r1, r19
	add r26, r16, r1
	ldw r1, r26+16
	add r3, r20, r0
	bne r1, r21, .LBB3_4
.LBB3_2:
	ldw r25, r26+0
	ldw r4, r26+8
	add r3, r12, r0
	jal r31, rnd
	add r1, r25, r1
	addi r25, r1, 1
	ldw r27, r26+4
	ldw r4, r26+12
	add r3, r12, r0
	jal r31, rnd
	add r1, r27, r1
	addi r1, r1, 1
	mul r3, r25, r22
	add r3, r17, r3
	add r3, r3, r1
	ldbu r4, r3+0
	add r3, r21, r0
	bne r4, r23, .LBB3_4
.LBB3_3:
	stw r13+0, r25
	stw r11+0, r1
	add r3, r24, r0
.LBB3_4:
	ori  r1, r3, 4
	bne r1, r20, .LBB3_7
.LBB3_5:
	addi r18, r18, -1
	bne r18, r21, .LBB3_1
.LBB3_6:
	ldw r1, r16+0
	addi r1, r1, 1
	stw r13+0, r1
	addi r1, r15, -568
	add r1, r12, r1
	ldw r1, r1+0
	addi r1, r1, 1
	stw r11+0, r1
.LBB3_7:
	ldw r27, sp+28
	ldw r26, sp+32
	ldw r25, sp+36
	ldw r24, sp+40
	ldw r23, sp+44
	ldw r22, sp+48
	ldw r21, sp+52
	ldw r20, sp+56
	ldw r19, sp+60
	ldw r18, sp+64
	ldw r17, sp+68
	ldw r16, sp+72
	ldw r15, sp+76
	ldw r14, sp+80
	ldw r13, sp+84
	ldw r12, sp+88
	ldw r11, sp+92
	ldw lr, sp+0
	addi sp, sp, 96
	jalr r0, r31, 0
.Lfunc_end3:
	.size	rand_floor, .Lfunc_end3-rand_floor
                                        # -- End function
	.globl	mark_seen                       # -- Begin function mark_seen
	.p2align	2
	.type	mark_seen,@function
mark_seen:                              # @mark_seen
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
	stw sp+16, r22
	lui r13, 1
	addi r1, r13, 512
	add r1, r3, r1
	ldw r6, r1+0
	addi r4, r13, 516
	add r4, r3, r4
	ldw r7, r4+0
	addi r8, r13, -572
	add r5, r3, r8
	addi r9, r0, 0
	addi r10, r13, -556
	addi lr, r0, 4
	addi r11, r13, -564
	addi r12, r13, -568
	addi r13, r13, -560
	addi r15, r0, 1
	addi r14, r0, -1
	addi r16, r0, 180
	add r17, r9, r0
                                        # implicit-def: $r19
	add r18, r9, r0
.LBB4_1:
	add r20, r3, r17
	add r21, r20, r10
	ldw r21, r21+0
	beq r21, r9, .LBB4_3
.LBB4_2:
	add r20, r19, r0
	add r21, lr, r0
	jal r0, .LBB4_8
.LBB4_3:
	add r21, r20, r8
	ldw r21, r21+0
	blt r6, r21, .LBB4_7
.LBB4_4:
	add r22, r20, r11
	ldw r22, r22+0
	add r21, r21, r22
	addi r21, r21, 1
	bgt r6, r21, .LBB4_7
.LBB4_5:
	add r21, r20, r12
	ldw r21, r21+0
	blt r7, r21, .LBB4_7
.LBB4_6:
	add r20, r20, r13
	ldw r20, r20+0
	add r20, r21, r20
	addi r22, r20, 1
	add r20, r18, r0
	add r21, r15, r0
	ble r7, r22, .LBB4_8
.LBB4_7:
	add r20, r19, r0
	add r21, r9, r0
.LBB4_8:
	ori  r19, r21, 4
	bne r19, lr, .LBB4_11
.LBB4_9:
	addi r18, r18, 1
	addi r17, r17, 20
	add r19, r20, r0
	bne r17, r16, .LBB4_1
.LBB4_10:
	add r20, r14, r0
.LBB4_11:
	addi r9, r0, 0
	addi r8, r0, 80
	blt r20, r9, .LBB4_14
.LBB4_12:
	mul r6, r6, r8
	add r6, r3, r6
	add r6, r6, r7
	ldbu r6, r6+4
	addi r7, r0, 43
	beq r6, r7, .LBB4_14
.LBB4_13:
	addi r6, r0, 20
	mul r6, r20, r6
	add r5, r5, r6
	ldw r9, r5+0
	ldw r6, r5+8
	add r6, r9, r6
	addi r6, r6, 1
	ble r9, r6, .LBB4_22
.LBB4_14:
	ldw r5, r1+0
	addi r9, r5, -1
	mul r5, r5, r8
	add r3, r5, r3
	addi r3, r3, 1683
	addi r5, r0, 21
	addi r6, r0, 79
	jal r0, .LBB4_16
.LBB4_15:
	addi r9, r7, 1
	ldw r8, r1+0
	addi r3, r3, 80
	bgt r7, r8, .LBB4_21
.LBB4_16:
	add r7, r9, r0
	ldw r8, r4+0
	jal r0, .LBB4_18
.LBB4_17:
	ldw r10, r4+0
	addi r8, r8, 1
	bgt r9, r10, .LBB4_15
.LBB4_18:
	addi r9, r8, -1
	bgtu r7, r5, .LBB4_17
.LBB4_19:
	bgtu r9, r6, .LBB4_17
.LBB4_20:
	add r10, r3, r8
	ldbu lr, r10+0
	ori  lr, lr, 1
	stb r10+0, lr
	jal r0, .LBB4_17
.LBB4_21:
	ldw r22, sp+16
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
.LBB4_22:
	mul r6, r9, r8
	add r6, r6, r3
	addi r6, r6, 1764
	jal r0, .LBB4_24
.LBB4_23:
	addi r9, r7, 1
	ldw r10, r5+0
	ldw lr, r5+8
	add r10, r10, lr
	addi r10, r10, 1
	addi r6, r6, 80
	bge r7, r10, .LBB4_14
.LBB4_24:
	add r7, r9, r0
	ldw r9, r5+4
	ldw r10, r5+12
	add r10, r9, r10
	addi r10, r10, 1
	bgt r9, r10, .LBB4_23
.LBB4_25:
	add r10, r9, r0
	add r9, r6, r9
	ldbu lr, r9+0
	ori  lr, lr, 1
	stb r9+0, lr
	addi r9, r10, 1
	ldw lr, r5+4
	ldw r11, r5+12
	add lr, lr, r11
	addi lr, lr, 1
	blt r10, lr, .LBB4_25
	jal r0, .LBB4_23
.Lfunc_end4:
	.size	mark_seen, .Lfunc_end4-mark_seen
                                        # -- End function
	.globl	reveal_map                      # -- Begin function reveal_map
	.p2align	2
	.type	reveal_map,@function
reveal_map:                             # @reveal_map
# %bb.0:
	addi r1, r0, 0
	addi r4, r0, 32
	addi r5, r0, 80
	addi r6, r0, 22
	add r7, r1, r0
	jal r0, .LBB5_2
.LBB5_1:
	addi r7, r7, 1
	addi r3, r3, 80
	beq r7, r6, .LBB5_6
.LBB5_2:
	add r8, r1, r0
	jal r0, .LBB5_4
.LBB5_3:
	addi r8, r8, 1
	beq r8, r5, .LBB5_1
.LBB5_4:
	add r9, r3, r8
	ldbu r10, r9+4
	beq r10, r4, .LBB5_3
.LBB5_5:
	ldbu r10, r9+1764
	ori  r10, r10, 1
	stb r9+1764, r10
	jal r0, .LBB5_3
.LBB5_6:
	jalr r0, r31, 0
.Lfunc_end5:
	.size	reveal_map, .Lfunc_end5-reveal_map
                                        # -- End function
	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e507704cf3c4d36284ffcb21f50e8531ceb63f7f)"
	.section	".note.GNU-stack","",@progbits
