	.file	"dungeon.c"
	.text
	.globl	room_at                         # -- Begin function room_at
	.p2align	2
	.type	room_at,@function
room_at:                                # @room_at
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	stw sp+20, r17
	stw sp+16, r18
	lui r1, 1
	addi r6, r1, -572
	addi r7, r1, -556
	addi r8, r0, 4
	addi r9, r1, -564
	addi r10, r1, -568
	addi lr, r1, -560
	addi r12, r0, 1
	addi r11, r0, -1
	addi r13, r0, 180
	add r14, r0, r0
	add r15, r0, r0
                                        # implicit-def: $r16
.LBB0_1:
	add r1, r3, r14
	add r17, r1, r7
	ldw r17, r17+0
	beq r17, r0, .LBB0_3
.LBB0_2:
	add r1, r16, r0
	add r17, r8, r0
	jal r0, .LBB0_8
.LBB0_3:
	add r17, r1, r6
	ldw r17, r17+0
	blt r4, r17, .LBB0_7
.LBB0_4:
	add r18, r1, r9
	ldw r18, r18+0
	add r17, r17, r18
	addi r17, r17, 1
	bgt r4, r17, .LBB0_7
.LBB0_5:
	add r17, r1, r10
	ldw r17, r17+0
	blt r5, r17, .LBB0_7
.LBB0_6:
	add r1, r1, lr
	ldw r1, r1+0
	add r1, r17, r1
	addi r18, r1, 1
	add r1, r15, r0
	add r17, r12, r0
	ble r5, r18, .LBB0_8
.LBB0_7:
	add r1, r16, r0
	add r17, r0, r0
.LBB0_8:
	ori  r16, r17, 4
	bne r16, r8, .LBB0_11
.LBB0_9:
	addi r15, r15, 1
	addi r14, r14, 20
	add r16, r1, r0
	bne r14, r13, .LBB0_1
.LBB0_10:
	add r1, r11, r0
.LBB0_11:
	ldw r18, sp+16
	ldw r17, sp+20
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end0:
	.size	room_at, .Lfunc_end0-room_at
                                        # -- End function
	.globl	new_level                       # -- Begin function new_level
	.p2align	2
	.type	new_level,@function
new_level:                              # @new_level
# %bb.0:
	addi sp, sp, -176
	stw sp+0, lr
	stw sp+4, fp
	stw sp+172, r11
	stw sp+168, r12
	stw sp+164, r13
	stw sp+160, r14
	stw sp+156, r15
	stw sp+152, r16
	stw sp+148, r17
	stw sp+144, r18
	stw sp+140, r19
	stw sp+136, r20
	stw sp+132, r21
	stw sp+128, r22
	stw sp+124, r23
	stw sp+120, r24
	stw sp+116, r25
	stw sp+112, r26
	stw sp+108, r27
	stw sp+104, r28
	add r11, r3, r0
	addi r14, r3, 4
	addi r4, r0, 32
	addi r12, r0, 1760
	add r3, r14, r0
	stw sp+84, r4
	add r5, r12, r0
	jal r31, memset
	addi r3, r11, 1764
	add r4, r0, r0
	add r5, r12, r0
	jal r31, memset
	addi r4, r0, 3
	add r3, r11, r0
	stw sp+92, r4
	jal r31, rnd
	add r16, r1, r0
	lui r1, 1
	stw sp+60, r1
	addi r19, r1, -572
	add r27, r11, r19
	addi r24, r11, 85
	addi r1, r0, 20
	stw sp+100, r1
	lui r1, 699051
	addi r20, r1, -1365
	addi r22, r0, 7
	addi r12, r0, -2
	addi r1, r0, 26
	stw sp+80, r1
	addi r28, r0, 1
	addi r1, r0, 4
	stw sp+88, r1
	addi r1, r0, 23
	stw sp+40, r1
	addi r21, r0, 9
	addi r18, r0, 18
	addi r1, r0, 5
	stw sp+76, r1
	addi r1, r0, 24
	stw sp+68, r1
	addi fp, r0, 80
	addi r25, r0, 124
	addi r13, r0, 46
	addi r26, r0, 45
	add r17, r0, r0
	stw sp+96, r27
	jal r0, .LBB1_3
.LBB1_1:
	stw r15+16, r28
	add r3, r11, r0
	ldw r4, sp+88
	jal r31, rnd
	add r1, r27, r1
	addi r1, r1, 1
	stw r15+0, r1
	add r3, r11, r0
	ldw r4, sp+40
	jal r31, rnd
	add r1, r1, r23
	addi r1, r1, 1
	stw r15+4, r1
	stw r15+12, r0
	stw r15+8, r0
	addi r16, r16, -1
	ldw r27, sp+96
.LBB1_2:
	addi r17, r17, 1
	beq r17, r21, .LBB1_13
.LBB1_3:
	ldw r1, sp+100
	mul r1, r17, r1
	add r15, r27, r1
	mulhu r1, r17, r20
	srli r3, r1, 1
	mul r27, r3, r22
	and r1, r1, r12
	add r1, r1, r3
	sub r1, r17, r1
	ldw r3, sp+80
	mul r23, r1, r3
	stw r15+16, r0
	blt r16, r28, .LBB1_5
.LBB1_4:
	add r3, r11, r0
	ldw r4, sp+92
	jal r31, rnd
	beq r1, r0, .LBB1_1
.LBB1_5:
	add r3, r11, r0
	ldw r4, sp+92
	jal r31, rnd
	addi r1, r1, 2
	stw r15+8, r1
	add r3, r11, r0
	add r4, r18, r0
	jal r31, rnd
	addi r1, r1, 4
	stw r15+12, r1
	ldw r1, r15+8
	ldw r3, sp+76
	sub r4, r3, r1
	add r3, r11, r0
	jal r31, rnd
	add r1, r1, r27
	stw r15+0, r1
	ldw r1, r15+12
	ldw r3, sp+68
	sub r4, r3, r1
	add r3, r11, r0
	jal r31, rnd
	add r1, r1, r23
	stw r15+4, r1
	ldw r3, r15+12
	add r3, r1, r3
	addi r3, r3, 1
	bgt r1, r3, .LBB1_7
.LBB1_6:
	add r3, r1, r0
	ldw r1, r15+0
	mul r1, r1, fp
	add r1, r3, r1
	add r1, r11, r1
	stb r1+4, r26
	ldw r1, r15+0
	ldw r4, r15+8
	mul r4, r4, fp
	mul r1, r1, fp
	add r1, r4, r1
	add r1, r3, r1
	add r1, r11, r1
	stb r1+84, r26
	addi r1, r3, 1
	ldw r4, r15+4
	ldw r5, r15+12
	add r4, r4, r5
	addi r4, r4, 1
	blt r3, r4, .LBB1_6
.LBB1_7:
	ldw r1, r15+8
	ldw r27, sp+96
	blt r1, r28, .LBB1_2
.LBB1_8:
	ldw r1, r15+0
	mul r3, r1, fp
	add r3, r24, r3
	jal r0, .LBB1_10
.LBB1_9:
	ldw r4, r15+0
	ldw r5, r15+8
	add r4, r5, r4
	addi r3, r3, 80
	bge r1, r4, .LBB1_2
.LBB1_10:
	addi r1, r1, 1
	mul r4, r1, fp
	add r4, r14, r4
	ldw r5, r15+4
	add r5, r4, r5
	stb r5+0, r25
	ldw r5, r15+4
	ldw r6, r15+12
	add r4, r4, r5
	add r4, r4, r6
	stb r4+1, r25
	ldw r4, r15+12
	blt r4, r28, .LBB1_9
.LBB1_11:
	ldw r4, r15+4
.LBB1_12:
	addi r5, r4, 1
	add r4, r3, r4
	stb r4+0, r13
	ldw r4, r15+4
	ldw r6, r15+12
	add r6, r6, r4
	add r4, r5, r0
	blt r5, r6, .LBB1_12
	jal r0, .LBB1_9
.LBB1_13:
	add r16, r0, r0
	ldw r24, sp+60
	ldw r15, sp+84
	jal r0, .LBB1_15
.LBB1_14:
	add r16, r17, r0
	beq r17, r21, .LBB1_19
.LBB1_15:
	mulhu r1, r16, r20
	srli r3, r1, 1
	and r1, r1, r12
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
	ldw r3, sp+92
	bltu r1, r3, .LBB1_14
.LBB1_18:
	addi r5, r16, 3
	add r3, r11, r0
	add r4, r16, r0
	jal r31, connect_rooms
	jal r0, .LBB1_14
.LBB1_19:
	addi r1, r24, -556
	addi r3, r0, 180
	addi r12, r24, -568
	addi r4, r0, 35
	add r5, r0, r0
	jal r0, .LBB1_21
.LBB1_20:
	addi r5, r5, 20
	beq r5, r3, .LBB1_24
.LBB1_21:
	add r6, r11, r5
	add r7, r6, r1
	ldw r7, r7+0
	beq r7, r0, .LBB1_20
.LBB1_22:
	add r7, r6, r19
	ldw r7, r7+0
	mul r7, r7, fp
	add r7, r14, r7
	add r6, r6, r12
	ldw r6, r6+0
	add r6, r7, r6
	ldbu r7, r6+0
	bne r7, r15, .LBB1_20
.LBB1_23:
	stb r6+0, r4
	jal r0, .LBB1_20
.LBB1_24:
	addi r1, r24, 504
	stw sp+24, r1
	add r17, r11, r1
	addi r1, r24, 508
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
	addi r1, r24, 512
	add r15, r11, r1
	addi r1, r24, 516
	add r16, r11, r1
.LBB1_25:
	add r3, r11, r0
	add r4, r15, r0
	add r5, r16, r0
	jal r31, rand_floor
	ldw r1, r15+0
	ldw r3, r17+0
	bne r1, r3, .LBB1_27
.LBB1_26:
	ldw r1, r16+0
	ldw r3, r18+0
	beq r1, r3, .LBB1_25
.LBB1_27:
	stw sp+28, r16
	stw sp+56, r15
	addi r17, r0, 4
	add r3, r11, r0
	add r4, r17, r0
	jal r31, rnd
	addi r3, r24, 548
	add r3, r11, r3
	stw sp+64, r3
	ldw r3, r3+0
	addi r4, r24, -392
	add r5, r11, r4
	stw sp+32, r5
	addi r6, r0, -1
	addi r25, r24, -72
.LBB1_28:
	add r5, r11, r4
	stw r5+0, r6
	addi r4, r4, 20
	bne r4, r25, .LBB1_28
.LBB1_29:
	stw sp+20, r6
	srai r4, r3, 31
	srli r4, r4, 30
	add r3, r3, r4
	srai r3, r3, 2
	add r1, r1, r3
	addi r1, r1, 3
	stw sp+16, r22
	stw sp+88, r28
	ldw r22, sp+100
	blt r1, r28, .LBB1_60
.LBB1_30:
	xori r3, r1, 16
	addi r4, r0, 16
	slt r1, r1, r4
	sub r1, r0, r1
	and r1, r3, r1
	xori r1, r1, 16
	stw sp+48, r1
	lui r1, %hi(mon_table_len)
	ldw r18, r1+%lo(mon_table_len)
	add r1, r11, r12
	stw sp+36, r1
	addi r26, r0, 40
	lui r12, %hi(mon_table)
	addi r12, r12, %lo(mon_table)
	lui r21, %hi(mon_table+8)
	addi r21, r21, %lo(mon_table+8)
	addi r16, r0, 100
	addi r1, r0, 99
	stw sp+52, r1
	addi r1, r0, 1000
	stw sp+44, r1
	addi r19, r0, 9
	stw sp+72, r0
	jal r0, .LBB1_33
.LBB1_31:
	ldw r3, sp+72
.LBB1_32:
	ldw r1, sp+48
	stw sp+72, r3
	bge r3, r1, .LBB1_60
.LBB1_33:
	add r23, r0, r0
	ldw r28, sp+64
	jal r0, .LBB1_35
.LBB1_34:
	addi r23, r23, 1
	beq r23, r16, .LBB1_38
.LBB1_35:
	add r3, r11, r0
	add r4, r18, r0
	jal r31, rnd
	add r20, r1, r0
	ldw r1, r28+0
	mul r3, r20, r26
	add r15, r3, r12
	add r3, r3, r21
	ldw r3, r3+0
	blt r1, r3, .LBB1_34
.LBB1_36:
	ldw r3, r15+12
	bgt r1, r3, .LBB1_34
.LBB1_37:
	ldw r1, sp+52
	bleu r23, r1, .LBB1_39
	jal r0, .LBB1_60
.LBB1_38:
	add r23, r16, r0
	ldw r1, sp+52
	bgtu r23, r1, .LBB1_60
.LBB1_39:
	ldw r23, sp+44
                                        # implicit-def: $r28
                                        # implicit-def: $r1
                                        # kill: killed $r1
	jal r0, .LBB1_42
.LBB1_40:
	add r1, r17, r0
	ori  r1, r1, 4
	bne r1, r17, .LBB1_47
.LBB1_41:
	addi r23, r23, -1
	beq r23, r0, .LBB1_48
.LBB1_42:
	add r3, r11, r0
	add r4, r19, r0
	jal r31, rnd
	mul r1, r1, r22
	add r24, r27, r1
	ldw r1, r24+16
	bne r1, r0, .LBB1_40
.LBB1_43:
	ldw r27, r24+0
	ldw r4, r24+8
	add r3, r11, r0
	jal r31, rnd
	add r1, r27, r1
	addi r27, r1, 1
	ldw r22, r24+4
	ldw r4, r24+12
	add r3, r11, r0
	jal r31, rnd
	add r1, r22, r1
	addi r1, r1, 1
	mul r3, r27, fp
	add r3, r14, r3
	add r3, r3, r1
	ldbu r3, r3+0
	bne r3, r13, .LBB1_45
.LBB1_44:
	add r28, r27, r0
	stw sp+84, r1
	ldw r1, sp+88
	jal r0, .LBB1_46
.LBB1_45:
	add r1, r0, r0
.LBB1_46:
	ldw r27, sp+96
	ldw r22, sp+100
	ori  r1, r1, 4
	beq r1, r17, .LBB1_41
.LBB1_47:
	ldw r24, sp+60
	ldw r5, sp+84
	jal r0, .LBB1_49
.LBB1_48:
	ldw r1, r27+0
	addi r28, r1, 1
	ldw r1, sp+36
	ldw r1, r1+0
	addi r5, r1, 1
	ldw r24, sp+60
.LBB1_49:
	ldw r1, sp+56
	ldw r1, r1+0
	bne r28, r1, .LBB1_51
.LBB1_50:
	ldw r1, sp+28
	ldw r1, r1+0
	beq r5, r1, .LBB1_31
.LBB1_51:
	add r1, r0, r0
	jal r0, .LBB1_53
.LBB1_52:
	addi r1, r1, 20
	addi r3, r0, 320
	beq r1, r3, .LBB1_56
.LBB1_53:
	add r3, r11, r1
	addi r4, r24, -392
	add r4, r3, r4
	ldw r4, r4+0
	blt r4, r0, .LBB1_52
.LBB1_54:
	addi r4, r24, -388
	add r4, r3, r4
	ldw r4, r4+0
	bne r4, r28, .LBB1_52
.LBB1_55:
	addi r4, r24, -384
	add r3, r3, r4
	ldw r3, r3+0
	bne r3, r5, .LBB1_52
	jal r0, .LBB1_31
.LBB1_56:
	ldw r1, sp+72
	mul r1, r1, r22
	ldw r3, sp+32
	add r23, r3, r1
	stw r23+0, r20
	stw r23+4, r28
	stw r23+8, r5
	ldw r4, r15+16
	addi r5, r0, 8
	add r3, r11, r0
	jal r31, roll
	stw r23+12, r1
	ldw r1, r15+36
	beq r1, r0, .LBB1_58
.LBB1_57:
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, rnd
	addi r3, r0, 0
	seq r1, r1, r3
	jal r0, .LBB1_59
.LBB1_58:
	addi r1, r0, 1
.LBB1_59:
	stw r23+16, r1
	ldw r3, sp+72
	addi r3, r3, 1
	jal r0, .LBB1_32
.LBB1_60:
	addi r14, r0, 4
	add r3, r11, r0
	add r4, r14, r0
	jal r31, rnd
	add r22, r11, r25
	ldw r4, sp+24
.LBB1_61:
	add r3, r11, r25
	stw r3+0, r0
	addi r25, r25, 24
	bne r25, r4, .LBB1_61
.LBB1_62:
	ldw r3, sp+20
	blt r1, r3, .LBB1_78
.LBB1_63:
	addi r13, r1, 2
	addi r18, r24, -72
	addi r15, r0, 100
	addi r19, r24, -56
	addi r20, r24, -52
	addi r21, r24, -64
	addi r25, r24, -60
	addi r26, r0, 34
	addi r1, r0, 10
	stw sp+100, r1
	addi r28, r0, 54
	addi r1, r24, -68
	stw sp+96, r1
	addi r1, r0, 69
	stw sp+84, r1
	addi r1, r0, 79
	stw sp+72, r1
	addi r1, r0, 2
	stw sp+60, r1
	addi r1, r0, 89
	stw sp+56, r1
	addi r1, r0, 6
	stw sp+52, r1
	add r12, r0, r0
	add r17, r0, r0
	jal r0, .LBB1_66
.LBB1_64:
	stw fp+0, r3
	ldw r1, sp+64
	ldw r1, r1+0
	ldw r3, sp+100
	mul r1, r1, r3
	addi r4, r1, 25
	add r3, r11, r0
	jal r31, rnd
	addi r1, r1, 5
	stw r27+0, r1
.LBB1_65:
	addi r17, r17, 1
	addi r12, r12, 24
	beq r13, r17, .LBB1_79
.LBB1_66:
	add r23, r11, r12
	add fp, r23, r18
	add r3, r11, r0
	add r4, r15, r0
	jal r31, rnd
	add r16, r1, r0
	add r4, r23, r19
	add r5, r23, r20
	add r3, r11, r0
	jal r31, rand_floor
	add r27, r23, r21
	ldw r3, sp+88
	stw r27+0, r3
	add r1, r23, r25
	stw r1+0, r0
	ble r16, r26, .LBB1_64
.LBB1_67:
	bgt r16, r28, .LBB1_69
.LBB1_68:
	ldw r1, sp+92
	stw fp+0, r1
	jal r0, .LBB1_71
.LBB1_69:
	ldw r1, sp+84
	bgt r16, r1, .LBB1_72
.LBB1_70:
	stw fp+0, r14
.LBB1_71:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, rnd
	ldw r3, sp+96
	add r3, r23, r3
	stw r3+0, r1
	jal r0, .LBB1_65
.LBB1_72:
	ldw r1, sp+72
	bgt r16, r1, .LBB1_74
.LBB1_73:
	ldw r1, sp+60
	stw fp+0, r1
	jal r0, .LBB1_65
.LBB1_74:
	ldw r1, sp+96
	add r23, r23, r1
	ldw r1, sp+56
	bgt r16, r1, .LBB1_76
.LBB1_75:
	ldw r1, sp+76
	jal r0, .LBB1_77
.LBB1_76:
	ldw r1, sp+52
.LBB1_77:
	stw fp+0, r1
	add r3, r11, r0
	add r4, r14, r0
	jal r31, rnd
	stw r23+0, r1
	jal r0, .LBB1_65
.LBB1_78:
	add r17, r0, r0
.LBB1_79:
	ldw r1, sp+64
	ldw r1, r1+0
	ldw r3, sp+80
	blt r1, r3, .LBB1_83
.LBB1_80:
	addi r1, r24, 568
	add r1, r11, r1
	ldw r1, r1+0
	bne r1, r0, .LBB1_83
.LBB1_81:
	ldw r1, sp+40
	bgtu r17, r1, .LBB1_83
.LBB1_82:
	ldw r1, sp+68
	mul r1, r17, r1
	add r1, r22, r1
	ldw r3, sp+16
	stw r1+0, r3
	stw r1+4, r0
	ldw r3, sp+88
	stw r1+8, r3
	stw r1+12, r0
	addi r4, r1, 16
	addi r5, r1, 20
	add r3, r11, r0
	jal r31, rand_floor
.LBB1_83:
	add r3, r11, r0
	ldw r28, sp+104
	ldw r27, sp+108
	ldw r26, sp+112
	ldw r25, sp+116
	ldw r24, sp+120
	ldw r23, sp+124
	ldw r22, sp+128
	ldw r21, sp+132
	ldw r20, sp+136
	ldw r19, sp+140
	ldw r18, sp+144
	ldw r17, sp+148
	ldw r16, sp+152
	ldw r15, sp+156
	ldw r14, sp+160
	ldw r13, sp+164
	ldw r12, sp+168
	ldw r11, sp+172
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 176
	jal r0, mark_seen
.Lfunc_end1:
	.size	new_level, .Lfunc_end1-new_level
                                        # -- End function
	.p2align	2                               # -- Begin function connect_rooms
	.type	connect_rooms,@function
connect_rooms:                          # @connect_rooms
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
	add r12, r5, r0
	add r11, r3, r0
	sub r18, r5, r4
	lui r17, 1
	addi r13, r17, -572
	add r14, r3, r13
	addi r20, r0, 20
	mul r1, r4, r20
	add r16, r14, r1
	ldw r1, r16+16
	ldw r15, r16+0
	addi r19, r0, 3
	bne r18, r19, .LBB2_3
.LBB2_1:
	beq r1, r0, .LBB2_5
.LBB2_2:
	ldw r16, r16+4
	jal r0, .LBB2_6
.LBB2_3:
	beq r1, r0, .LBB2_8
.LBB2_4:
	ldw r16, r16+4
	jal r0, .LBB2_9
.LBB2_5:
	ldw r1, r16+8
	add r1, r15, r1
	addi r15, r1, 1
	ldw r21, r16+4
	ldw r4, r16+12
	add r3, r11, r0
	jal r31, rnd
	add r1, r21, r1
	addi r16, r1, 1
.LBB2_6:
	mul r1, r12, r20
	add r1, r14, r1
	ldw r3, r1+16
	ldw r12, r1+0
	ldw r14, r1+4
	bne r3, r0, .LBB2_12
.LBB2_7:
	ldw r4, r1+12
	add r3, r11, r0
	jal r31, rnd
	add r1, r14, r1
	addi r14, r1, 1
	jal r0, .LBB2_12
.LBB2_8:
	ldw r4, r16+8
	add r3, r11, r0
	jal r31, rnd
	add r1, r15, r1
	addi r15, r1, 1
	ldw r1, r16+4
	ldw r3, r16+12
	add r1, r1, r3
	addi r16, r1, 1
.LBB2_9:
	mul r1, r12, r20
	add r14, r14, r1
	ldw r1, r14+16
	ldw r12, r14+0
	bne r1, r0, .LBB2_11
.LBB2_10:
	ldw r4, r14+8
	add r3, r11, r0
	jal r31, rnd
	add r1, r12, r1
	addi r12, r1, 1
.LBB2_11:
	ldw r14, r14+4
.LBB2_12:
	addi r1, r11, 4
	addi r4, r0, 80
	mul r6, r15, r4
	add r3, r1, r6
	add r3, r3, r16
	addi r7, r0, 43
	stb r3+0, r7
	mul r5, r12, r4
	add r1, r1, r5
	add r1, r1, r14
	stb r1+0, r7
	bne r18, r19, .LBB2_17
.LBB2_13:
	add r5, r12, r15
	srli r6, r5, 31
	add r5, r5, r6
	srai r7, r5, 1
	addi r5, r15, 1
	xor r6, r5, r7
	slt r8, r5, r7
	sub r8, r0, r8
	and r6, r6, r8
	xor r6, r7, r6
	sub r8, r7, r5
	sub r9, r5, r7
	xor r9, r9, r8
	sgt r5, r5, r7
	sub r5, r0, r5
	and r5, r9, r5
	xor r5, r8, r5
	addi r8, r5, 1
	mul r5, r6, r4
	add r5, r16, r5
	add r5, r5, r11
	addi r9, r5, 4
	addi r5, r0, 32
	addi r6, r0, 35
	jal r0, .LBB2_15
.LBB2_14:
	addi r8, r8, -1
	addi r9, r9, 80
	beq r8, r0, .LBB2_21
.LBB2_15:
	ldbu r10, r9+0
	bne r10, r5, .LBB2_14
.LBB2_16:
	stb r9+0, r6
	jal r0, .LBB2_14
.LBB2_17:
	add r7, r14, r16
	srli r8, r7, 31
	add r7, r7, r8
	srai r8, r7, 1
	addi r7, r16, 1
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
	beq r9, r0, .LBB2_25
.LBB2_19:
	ldbu lr, r10+0
	bne lr, r6, .LBB2_18
.LBB2_20:
	stb r10+0, r7
	jal r0, .LBB2_18
.LBB2_21:
	xor r8, r16, r14
	slt r9, r16, r14
	sub r9, r0, r9
	and r8, r8, r9
	xor r9, r14, r8
	sub r8, r14, r16
	sub r10, r16, r14
	xor r10, r10, r8
	sgt lr, r16, r14
	sub lr, r0, lr
	and r10, r10, lr
	xor r8, r8, r10
	addi r8, r8, 1
	mul r10, r7, r4
	add r9, r9, r10
	add r9, r9, r11
	addi r9, r9, 4
	jal r0, .LBB2_23
.LBB2_22:
	addi r8, r8, -1
	addi r9, r9, 1
	beq r8, r0, .LBB2_29
.LBB2_23:
	ldbu r10, r9+0
	bne r10, r5, .LBB2_22
.LBB2_24:
	stb r9+0, r6
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
	sgt r18, r15, r12
	sub r18, r0, r18
	and lr, lr, r18
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
	beq r9, r0, .LBB2_33
.LBB2_27:
	ldbu r10, r4+0
	bne r10, r6, .LBB2_26
.LBB2_28:
	stb r4+0, r7
	jal r0, .LBB2_26
.LBB2_29:
	addi r8, r12, -1
	xor r9, r7, r8
	slt r10, r7, r8
	sub r10, r0, r10
	and r9, r9, r10
	xor r9, r8, r9
	sub r10, r8, r7
	sub lr, r7, r8
	xor lr, lr, r10
	sgt r7, r7, r8
	sub r7, r0, r7
	and r7, lr, r7
	xor r7, r10, r7
	addi r7, r7, 1
	mul r4, r9, r4
	add r4, r14, r4
	add r4, r4, r11
	addi r4, r4, 4
	jal r0, .LBB2_31
.LBB2_30:
	addi r7, r7, -1
	addi r4, r4, 80
	beq r7, r0, .LBB2_37
.LBB2_31:
	ldbu r8, r4+0
	bne r8, r5, .LBB2_30
.LBB2_32:
	stb r4+0, r6
	jal r0, .LBB2_30
.LBB2_33:
	addi r4, r14, -1
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
	beq r4, r0, .LBB2_37
.LBB2_35:
	ldbu r8, r5+0
	bne r8, r6, .LBB2_34
.LBB2_36:
	stb r5+0, r7
	jal r0, .LBB2_34
.LBB2_37:
	addi r4, r17, -556
	addi r10, r0, 4
	addi r5, r17, -564
	addi r6, r17, -568
	addi r7, r17, -560
	addi lr, r0, 1
	addi r9, r0, -1
	addi r8, r0, 180
	add r17, r0, r0
	add r18, r0, r0
                                        # implicit-def: $r19
.LBB2_38:
	add r20, r11, r17
	add r21, r20, r4
	ldw r21, r21+0
	beq r21, r0, .LBB2_40
.LBB2_39:
	add r20, r19, r0
	add r21, r10, r0
	jal r0, .LBB2_45
.LBB2_40:
	add r21, r20, r13
	ldw r21, r21+0
	blt r15, r21, .LBB2_44
.LBB2_41:
	add r22, r20, r5
	ldw r22, r22+0
	add r21, r21, r22
	addi r21, r21, 1
	bgt r15, r21, .LBB2_44
.LBB2_42:
	add r21, r20, r6
	ldw r21, r21+0
	blt r16, r21, .LBB2_44
.LBB2_43:
	add r20, r20, r7
	ldw r20, r20+0
	add r20, r21, r20
	addi r22, r20, 1
	add r20, r18, r0
	add r21, lr, r0
	ble r16, r22, .LBB2_45
.LBB2_44:
	add r20, r19, r0
	add r21, r0, r0
.LBB2_45:
	ori  r19, r21, 4
	bne r19, r10, .LBB2_48
.LBB2_46:
	addi r18, r18, 1
	addi r17, r17, 20
	add r19, r20, r0
	bne r17, r8, .LBB2_38
.LBB2_47:
	add r20, r9, r0
.LBB2_48:
	bgt r20, r9, .LBB2_50
.LBB2_49:
	addi r9, r0, 35
	stb r3+0, r9
.LBB2_50:
	addi r9, r0, 4
	addi r10, r0, 1
	addi r3, r0, -1
	add lr, r0, r0
	add r15, r0, r0
                                        # implicit-def: $r16
.LBB2_51:
	add r17, r11, lr
	add r18, r17, r4
	ldw r18, r18+0
	beq r18, r0, .LBB2_53
.LBB2_52:
	add r17, r16, r0
	add r18, r9, r0
	jal r0, .LBB2_58
.LBB2_53:
	add r18, r17, r13
	ldw r18, r18+0
	blt r12, r18, .LBB2_57
.LBB2_54:
	add r19, r17, r5
	ldw r19, r19+0
	add r18, r18, r19
	addi r18, r18, 1
	bgt r12, r18, .LBB2_57
.LBB2_55:
	add r18, r17, r6
	ldw r18, r18+0
	blt r14, r18, .LBB2_57
.LBB2_56:
	add r17, r17, r7
	ldw r17, r17+0
	add r17, r18, r17
	addi r19, r17, 1
	add r17, r15, r0
	add r18, r10, r0
	ble r14, r19, .LBB2_58
.LBB2_57:
	add r17, r16, r0
	add r18, r0, r0
.LBB2_58:
	ori  r16, r18, 4
	bne r16, r9, .LBB2_61
.LBB2_59:
	addi r15, r15, 1
	addi lr, lr, 20
	add r16, r17, r0
	bne lr, r8, .LBB2_51
.LBB2_60:
	add r17, r3, r0
.LBB2_61:
	bgt r17, r3, .LBB2_63
.LBB2_62:
	addi r3, r0, 35
	stb r1+0, r3
.LBB2_63:
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
.Lfunc_end2:
	.size	connect_rooms, .Lfunc_end2-connect_rooms
                                        # -- End function
	.p2align	2                               # -- Begin function rand_floor
	.type	rand_floor,@function
rand_floor:                             # @rand_floor
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
	stw sp+16, r26
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
	addi r21, r0, 80
	addi r22, r0, 46
	addi r23, r0, 1
.LBB3_1:
	add r3, r12, r0
	add r4, r14, r0
	jal r31, rnd
	mul r1, r1, r19
	add r24, r16, r1
	ldw r3, r24+16
	add r1, r20, r0
	bne r3, r0, .LBB3_4
.LBB3_2:
	ldw r25, r24+0
	ldw r4, r24+8
	add r3, r12, r0
	jal r31, rnd
	add r1, r25, r1
	addi r25, r1, 1
	ldw r26, r24+4
	ldw r4, r24+12
	add r3, r12, r0
	jal r31, rnd
	add r1, r26, r1
	addi r1, r1, 1
	mul r3, r25, r21
	add r3, r17, r3
	add r3, r3, r1
	ldbu r3, r3+0
	bne r3, r22, .LBB3_6
.LBB3_3:
	stw r13+0, r25
	stw r11+0, r1
	add r1, r23, r0
.LBB3_4:
	ori  r1, r1, 4
	bne r1, r20, .LBB3_8
.LBB3_5:
	addi r18, r18, -1
	bne r18, r0, .LBB3_1
	jal r0, .LBB3_7
.LBB3_6:
	add r1, r0, r0
	ori  r1, r1, 4
	beq r1, r20, .LBB3_5
	jal r0, .LBB3_8
.LBB3_7:
	ldw r1, r16+0
	addi r1, r1, 1
	stw r13+0, r1
	addi r1, r15, -568
	add r1, r12, r1
	ldw r1, r1+0
	addi r1, r1, 1
	stw r11+0, r1
.LBB3_8:
	ldw r26, sp+16
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
	lui r12, 1
	addi r1, r12, 512
	add r1, r3, r1
	ldw r6, r1+0
	addi r4, r12, 516
	add r4, r3, r4
	ldw r7, r4+0
	addi r8, r12, -572
	add r5, r3, r8
	addi r9, r12, -556
	addi r10, r0, 4
	addi lr, r12, -564
	addi r11, r12, -568
	addi r12, r12, -560
	addi r14, r0, 1
	addi r13, r0, -1
	addi r15, r0, 180
	add r16, r0, r0
	add r17, r0, r0
                                        # implicit-def: $r18
.LBB4_1:
	add r19, r3, r16
	add r20, r19, r9
	ldw r20, r20+0
	beq r20, r0, .LBB4_3
.LBB4_2:
	add r19, r18, r0
	add r20, r10, r0
	jal r0, .LBB4_8
.LBB4_3:
	add r20, r19, r8
	ldw r20, r20+0
	blt r6, r20, .LBB4_7
.LBB4_4:
	add r21, r19, lr
	ldw r21, r21+0
	add r20, r20, r21
	addi r20, r20, 1
	bgt r6, r20, .LBB4_7
.LBB4_5:
	add r20, r19, r11
	ldw r20, r20+0
	blt r7, r20, .LBB4_7
.LBB4_6:
	add r19, r19, r12
	ldw r19, r19+0
	add r19, r20, r19
	addi r21, r19, 1
	add r19, r17, r0
	add r20, r14, r0
	ble r7, r21, .LBB4_8
.LBB4_7:
	add r19, r18, r0
	add r20, r0, r0
.LBB4_8:
	ori  r18, r20, 4
	bne r18, r10, .LBB4_11
.LBB4_9:
	addi r17, r17, 1
	addi r16, r16, 20
	add r18, r19, r0
	bne r16, r15, .LBB4_1
.LBB4_10:
	add r19, r13, r0
.LBB4_11:
	addi r8, r0, 80
	blt r19, r0, .LBB4_14
.LBB4_12:
	mul r6, r6, r8
	add r6, r3, r6
	add r6, r6, r7
	ldbu r6, r6+4
	addi r7, r0, 43
	beq r6, r7, .LBB4_14
.LBB4_13:
	addi r6, r0, 20
	mul r6, r19, r6
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
	addi r1, r0, 32
	addi r4, r0, 80
	addi r5, r0, 22
	add r6, r0, r0
	jal r0, .LBB5_2
.LBB5_1:
	addi r6, r6, 1
	addi r3, r3, 80
	beq r6, r5, .LBB5_6
.LBB5_2:
	add r7, r0, r0
	jal r0, .LBB5_4
.LBB5_3:
	addi r7, r7, 1
	beq r7, r4, .LBB5_1
.LBB5_4:
	add r8, r3, r7
	ldbu r9, r8+4
	beq r9, r1, .LBB5_3
.LBB5_5:
	ldbu r9, r8+1764
	ori  r9, r9, 1
	stb r8+1764, r9
	jal r0, .LBB5_3
.LBB5_6:
	jalr r0, r31, 0
.Lfunc_end5:
	.size	reveal_map, .Lfunc_end5-reveal_map
                                        # -- End function
	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git 77f07727152e39f179dfe39786aa3bcae0860790)"
	.section	".note.GNU-stack","",@progbits
