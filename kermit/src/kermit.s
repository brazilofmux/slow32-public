	.file	"kermit.c"
	.text
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi sp, sp, -1184
	stw sp+0, lr
	stw sp+4, fp
	stw sp+1180, r11
	stw sp+1176, r12
	stw sp+1172, r13
	stw sp+1168, r14
	stw sp+1164, r15
	stw sp+1160, r16
	stw sp+1156, r17
	stw sp+1152, r18
	stw sp+1148, r19
	stw sp+1144, r20
	stw sp+1140, r21
	stw sp+1136, r22
	stw sp+1132, r23
	stw sp+1128, r24
	stw sp+1124, r25
	stw sp+1120, r26
	stw sp+1116, r27
	stw sp+1112, r28
	addi r16, r0, 2
	blt r3, r16, .LBB0_103
.LBB0_1:
	add r12, r4, r0
	add r11, r3, r0
	ldw r3, r4+4
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	jal r31, strcmp
	addi r15, r0, 0
	beq r1, r15, .LBB0_10
.LBB0_2:
	ldw r3, r12+4
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, strcmp
	bne r1, r15, .LBB0_103
.LBB0_3:
	addi r1, r0, 4
	bltu r11, r1, .LBB0_95
.LBB0_4:
	addi r17, r12, 8
	addi r18, r0, 2
	lui r13, %hi(.L.str.8)
	addi r13, r13, %lo(.L.str.8)
	addi r19, r0, 0
	lui r20, %hi(opt_drop)
	addi r20, r20, %lo(opt_drop)
	lui r14, %hi(.L.str.9)
	addi r14, r14, %lo(.L.str.9)
	addi r21, r0, 94
	lui r22, %hi(opt_time)
	addi r22, r22, %lo(opt_time)
	jal r0, .LBB0_7
.LBB0_5:
	ldw r3, r17+4
	jal r31, atoi
	stw r20+0, r1
.LBB0_6:
	addi r16, r18, 2
	addi r17, r17, 8
	addi r1, r18, 3
	add r18, r16, r0
	bgeu r1, r11, .LBB0_95
.LBB0_7:
	ldw r3, r17+0
	add r4, r13, r0
	jal r31, strcmp
	beq r1, r19, .LBB0_5
.LBB0_8:
	ldw r3, r17+0
	add r4, r14, r0
	jal r31, strcmp
	bne r1, r19, .LBB0_94
.LBB0_9:
	ldw r3, r17+4
	jal r31, atoi
	sgtu r3, r1, r21
	sub r3, r0, r3
	xori r4, r1, 5
	and r3, r4, r3
	xor r1, r1, r3
	stw r22+0, r1
	jal r0, .LBB0_6
.LBB0_10:
	addi r27, r0, 4
	bltu r11, r27, .LBB0_17
.LBB0_11:
	addi r14, r12, 8
	addi r15, r0, 3
	lui r12, %hi(.L.str.8)
	addi r12, r12, %lo(.L.str.8)
	addi r16, r0, 0
	lui r17, %hi(opt_drop)
	addi r17, r17, %lo(opt_drop)
	lui r13, %hi(.L.str.9)
	addi r13, r13, %lo(.L.str.9)
	addi r18, r0, 94
	lui r19, %hi(opt_time)
	addi r19, r19, %lo(opt_time)
	jal r0, .LBB0_14
.LBB0_12:
	ldw r3, r14+4
	jal r31, atoi
	stw r17+0, r1
.LBB0_13:
	addi r14, r14, 8
	addi r15, r15, 2
	bgeu r15, r11, .LBB0_17
.LBB0_14:
	ldw r3, r14+0
	add r4, r12, r0
	jal r31, strcmp
	beq r1, r16, .LBB0_12
.LBB0_15:
	ldw r3, r14+0
	add r4, r13, r0
	jal r31, strcmp
	bne r1, r16, .LBB0_17
.LBB0_16:
	ldw r3, r14+4
	jal r31, atoi
	sgtu r3, r1, r18
	sub r3, r0, r3
	xori r4, r1, 5
	and r3, r4, r3
	xor r1, r1, r3
	stw r19+0, r1
	jal r0, .LBB0_13
.LBB0_17:
	addi r12, r0, 2
	addi r4, r0, 1
	addi r13, r0, 0
	add r3, r12, r0
	add r16, r4, r0
	add r5, r13, r0
	jal r31, socket
	addi fp, r0, -1
	lui r28, %hi(stderr)
	addi r28, r28, %lo(stderr)
	ble r1, fp, .LBB0_92
.LBB0_18:
	add r11, r1, r0
	addi r14, sp, 548
	addi r15, r0, 16
	add r3, r14, r0
	add r4, r13, r0
	add r5, r15, r0
	jal r31, memset
	sth sp+548, r12
	lui r1, 520192
	addi r3, r1, 1
	jal r31, htonl
	stw sp+552, r1
	add r3, r13, r0
	jal r31, htons
	addi r3, r14, 2
	sth r3+0, r1
	add r3, r11, r0
	add r4, r14, r0
	add r5, r15, r0
	jal r31, bind
	blt r1, r13, .LBB0_91
.LBB0_19:
	addi r17, r0, 1
	add r3, r11, r0
	add r4, r17, r0
	jal r31, listen
	ble r1, fp, .LBB0_91
.LBB0_20:
	stw sp+544, r15
	addi r16, sp, 548
	add r3, r16, r0
	add r4, r13, r0
	add r5, r15, r0
	jal r31, memset
	addi r5, sp, 544
	add r3, r11, r0
	add r4, r16, r0
	jal r31, getsockname
	lui r3, %hi(.L.str.12)
	addi r3, r3, %lo(.L.str.12)
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	jal r31, fopen
	beq r1, r13, .LBB0_106
.LBB0_21:
	add r15, r1, r0
	addi r1, sp, 548
	addi r14, r1, 2
	ldhu r3, r14+0
	jal r31, ntohs
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	add r3, r15, r0
	add r5, r1, r0
	jal r31, fprintf
	add r3, r15, r0
	jal r31, fclose
	ldhu r3, r14+0
	jal r31, ntohs
	lui r3, %hi(.L.str.16)
	addi r3, r3, %lo(.L.str.16)
	add r4, r1, r0
	jal r31, printf
	lui r21, %hi(stdout)
	addi r21, r21, %lo(stdout)
	ldw r3, r21+0
	jal r31, fflush
	add r3, r11, r0
	add r4, r13, r0
	add r5, r13, r0
	jal r31, accept
	add r15, r1, r0
	addi r3, sp, 564
	addi r5, r0, 548
	add r4, r13, r0
	jal r31, memset
	stw sp+564, r15
	addi r18, r0, 94
	stw sp+1088, r18
	addi r1, r0, 35
	stw sp+1092, r1
	stw sp+1096, r1
	lui r19, %hi(opt_time)
	addi r19, r19, %lo(opt_time)
	ldw r1, r19+0
	stw sp+1100, r1
	stw sp+1104, fp
	blt r15, r13, .LBB0_111
.LBB0_22:
	addi r1, sp, 404
	addi r26, r1, 8
	lui r16, %hi(.L.str.18)
	addi r16, r16, %lo(.L.str.18)
	addi r15, r0, 69
	lui r1, %hi(.L.str.20)
	addi r1, r1, %lo(.L.str.20)
	stw sp+48, r1
	addi r1, r0, 58
	stw sp+44, r1
	addi r1, r0, 32
	stw sp+40, r1
	addi r20, r0, 10
	stw sp+36, r13
	add r24, r13, r0
	add r23, r13, r0
	add r22, r13, r0
.LBB0_23:
	addi r3, sp, 564
	addi r4, sp, 404
	jal r31, read_pkt
	ble r1, fp, .LBB0_29
.LBB0_24:
	beq r1, r13, .LBB0_31
.LBB0_25:
	bne r1, r12, .LBB0_46
.LBB0_26:
	addi r25, r24, 1
	addi r1, r0, 9
	blt r24, r1, .LBB0_45
.LBB0_27:
	ldw r3, r28+0
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	jal r31, fprintf
	addi r3, sp, 564
	addi r5, r0, 69
	addi r24, r0, 0
	add r4, r23, r0
	add r6, r24, r0
	add r7, r24, r0
	jal r31, send_pkt
	beq r22, r24, .LBB0_54
.LBB0_28:
	add r3, r22, r0
	jal r31, fclose
	addi r14, r0, 1
	jal r0, .LBB0_55
.LBB0_29:
	ldw r3, r28+0
	add r4, r16, r0
	jal r31, fprintf
	beq r22, r13, .LBB0_43
.LBB0_30:
	add r3, r22, r0
	jal r31, fclose
	jal r0, .LBB0_44
.LBB0_31:
	ldw r1, sp+408
	beq r1, r15, .LBB0_48
.LBB0_32:
	addi r3, r0, 83
	bne r1, r3, .LBB0_52
.LBB0_33:
	stw sp+1088, r18
	ldw r1, sp+540
	addi r14, r0, 1
	blt r1, r14, .LBB0_36
.LBB0_34:
	ldbu r3, sp+412
	addi r4, r3, -52
	andi r4, r4, 255
	addi r5, r0, 74
	bgtu r4, r5, .LBB0_36
.LBB0_35:
	addi r3, r3, -32
	stw sp+1088, r3
.LBB0_36:
	addi r25, r0, 2
	blt r1, r25, .LBB0_39
.LBB0_37:
	ldbu r3, sp+413
	addi r4, r3, -33
	andi r4, r4, 255
	addi r5, r0, 93
	bgtu r4, r5, .LBB0_39
.LBB0_38:
	addi r3, r3, -32
	stw sp+1100, r3
.LBB0_39:
	addi r3, r0, 6
	blt r1, r3, .LBB0_42
.LBB0_40:
	ldbu r1, sp+417
	addi r3, r1, -33
	andi r3, r3, 255
	addi r4, r0, 93
	bgtu r3, r4, .LBB0_42
.LBB0_41:
	stw sp+1096, r1
.LBB0_42:
	addi r1, r0, 126
	stb sp+276, r1
	ldbu r1, r19+0
	addi r1, r1, 32
	addi r6, sp, 276
	addi r3, r6, 1
	stb r3+0, r1
	addi r1, r6, 2
	lui r3, 4
	addi r3, r3, 32
	sth r1+0, r3
	lui r1, 2
	addi r1, r1, 813
	sth sp+280, r1
	ldw r4, sp+404
	addi r3, sp, 564
	addi r5, r0, 89
	addi r7, r0, 6
	jal r31, send_pkt
	ldw r1, sp+404
	addi r1, r1, 1
	andi r23, r1, 63
	addi r24, r0, 0
	ori  r1, r25, 2
	beq r1, r12, .LBB0_57
	jal r0, .LBB0_104
.LBB0_43:
	add r22, r13, r0
.LBB0_44:
	add r14, r17, r0
	add r25, r17, r0
	ori  r1, r25, 2
	beq r1, r12, .LBB0_57
	jal r0, .LBB0_104
.LBB0_45:
	add r24, r25, r0
.LBB0_46:
	addi r3, sp, 564
	addi r5, r0, 78
	addi r6, r0, 0
	add r4, r23, r0
	add r7, r6, r0
.LBB0_47:
	jal r31, send_pkt
	addi r14, r0, 1
	add r25, r12, r0
	ori  r1, r25, 2
	beq r1, r12, .LBB0_57
	jal r0, .LBB0_104
.LBB0_48:
	ldw r3, r28+0
	ldw r4, sp+48
	jal r31, fprintf
	ldw r1, sp+540
	blt r1, r17, .LBB0_50
.LBB0_49:
	ldw r4, r28+0
	ldw r3, sp+44
	jal r31, fputc
	ldw r4, r28+0
	ldw r3, sp+40
	jal r31, fputc
	ldw r5, sp+540
	ldw r6, r28+0
	add r3, r26, r0
	add r4, r17, r0
	jal r31, fwrite
.LBB0_50:
	ldw r4, r28+0
	add r3, r20, r0
	jal r31, fputc
	addi r24, r0, 0
	beq r22, r24, .LBB0_67
.LBB0_51:
	add r3, r22, r0
	jal r31, fclose
	addi r14, r0, 1
	jal r0, .LBB0_56
.LBB0_52:
	ldw r4, sp+404
	addi r3, r23, -1
	andi r3, r3, 63
	bne r4, r3, .LBB0_58
.LBB0_53:
	addi r3, sp, 564
	addi r5, r0, 89
	addi r24, r0, 0
	jal r0, .LBB0_69
.LBB0_54:
	addi r14, r0, 1
	add r22, r24, r0
.LBB0_55:
	add r24, r25, r0
.LBB0_56:
	add r25, r14, r0
	ori  r1, r25, 2
	bne r1, r12, .LBB0_104
.LBB0_57:
	bne r14, r13, .LBB0_23
	jal r0, .LBB0_108
.LBB0_58:
	bne r4, r23, .LBB0_68
.LBB0_59:
	stw sp+32, r26
	addi r1, r1, -66
	addi r3, r0, 24
	bgtu r1, r3, .LBB0_77
.LBB0_60:
	addi r24, r0, 0
	slli r1, r1, 2
	lui r3, %hi(.LJTI0_0)
	addi r3, r3, %lo(.LJTI0_0)
	add r1, r3, r1
	ldw r1, r1+0
	add r14, r24, r0
	jalr r0, r1, 0
.LBB0_61:
	addi r3, sp, 564
	addi r4, sp, 404
	addi r5, sp, 52
	addi r6, r0, 128
	jal r31, decode_data
	addi r25, r0, 4
	addi r14, r0, 1
	addi r4, r0, 0
	beq r22, r4, .LBB0_78
.LBB0_62:
	addi r3, r0, 1
	blt r1, r3, .LBB0_78
.LBB0_63:
	stw sp+24, r25
	stw sp+28, r4
	addi r3, sp, 52
	addi r25, r0, 1
	add r4, r25, r0
	add r5, r1, r0
	add r6, r22, r0
	add r26, r1, r0
	jal r31, fwrite
	ldw r3, sp+36
	add r3, r1, r3
	stw sp+36, r3
	bne r1, r26, .LBB0_65
.LBB0_64:
	add r3, r22, r0
	jal r31, ferror
	addi r3, r0, 0
	beq r1, r3, .LBB0_90
.LBB0_65:
	ldw r3, r28+0
	lui r4, %hi(.L.str.24)
	addi r4, r4, %lo(.L.str.24)
	jal r31, fprintf
	ldw r4, sp+404
	addi r3, sp, 564
	addi r5, r0, 69
	addi r26, r0, 0
	add r6, r26, r0
	add r7, r26, r0
	jal r31, send_pkt
	add r3, r22, r0
	jal r31, fclose
.LBB0_66:
	ldw r4, sp+28
	jal r0, .LBB0_79
.LBB0_67:
	addi r14, r0, 1
	add r22, r24, r0
	jal r0, .LBB0_56
.LBB0_68:
	addi r3, sp, 564
	addi r5, r0, 78
	addi r24, r0, 0
	add r4, r23, r0
.LBB0_69:
	add r6, r24, r0
	add r7, r24, r0
	jal r0, .LBB0_47
.LBB0_70:
	addi r3, sp, 564
	addi r4, sp, 404
	addi r25, sp, 180
	addi r6, r0, 95
	add r5, r25, r0
	jal r31, decode_data
	add r1, r25, r1
	addi r26, r0, 0
	stb r1+0, r26
	add r3, r25, r0
	jal r31, sanitize
	beq r22, r26, .LBB0_72
.LBB0_71:
	add r3, r22, r0
	jal r31, fclose
.LBB0_72:
	lui r4, %hi(.L.str.21)
	addi r4, r4, %lo(.L.str.21)
	addi r3, sp, 180
	jal r31, fopen
	add r22, r1, r0
	beq r1, r26, .LBB0_81
.LBB0_73:
	lui r3, %hi(.L.str.23)
	addi r3, r3, %lo(.L.str.23)
	addi r4, sp, 180
	jal r31, printf
	ldw r3, r21+0
	jal r31, fflush
	stw sp+36, r26
	add r25, r27, r0
	jal r0, .LBB0_82
.LBB0_74:
	addi r1, r0, 0
	beq r22, r1, .LBB0_84
.LBB0_75:
	add r3, r22, r0
	jal r31, fclose
	add r25, r1, r0
	addi r22, r0, 0
	beq r1, r22, .LBB0_85
.LBB0_76:
	ldw r3, r28+0
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	jal r31, fprintf
	ldw r4, sp+404
	addi r3, sp, 564
	addi r5, r0, 69
	addi r6, r0, 0
	add r7, r6, r0
	jal r31, send_pkt
	jal r0, .LBB0_86
.LBB0_77:
	addi r3, sp, 564
	addi r5, r0, 78
	addi r24, r0, 0
	add r4, r23, r0
	add r6, r24, r0
	add r7, r24, r0
	jal r31, send_pkt
	addi r14, r0, 1
	add r25, r12, r0
	jal r0, .LBB0_89
.LBB0_78:
	add r26, r14, r0
.LBB0_79:
	bne r26, r4, .LBB0_88
.LBB0_80:
	add r24, r4, r0
	jal r0, .LBB0_89
.LBB0_81:
	ldw r3, r28+0
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	addi r5, sp, 180
	jal r31, fprintf
	ldw r4, sp+404
	addi r3, sp, 564
	addi r5, r0, 69
	addi r6, r0, 0
	add r7, r6, r0
	jal r31, send_pkt
	addi r25, r0, 1
.LBB0_82:
	addi r14, r0, 1
	bne r22, r26, .LBB0_88
.LBB0_83:
	add r22, r26, r0
	add r24, r26, r0
	jal r0, .LBB0_89
.LBB0_84:
	addi r14, r0, 1
	add r22, r1, r0
	jal r0, .LBB0_88
.LBB0_85:
	lui r3, %hi(.L.str.26)
	addi r3, r3, %lo(.L.str.26)
	ldw r4, sp+36
	jal r31, printf
.LBB0_86:
	addi r14, r0, 1
	beq r25, r22, .LBB0_88
.LBB0_87:
	add r24, r22, r0
	add r25, r14, r0
	jal r0, .LBB0_89
.LBB0_88:
	ldw r4, sp+404
	addi r3, sp, 564
	addi r5, r0, 89
	add r6, r24, r0
	add r7, r24, r0
	jal r31, send_pkt
	addi r1, r23, 1
	andi r23, r1, 63
	add r25, r24, r0
.LBB0_89:
	ldw r26, sp+32
	ori  r1, r25, 2
	beq r1, r12, .LBB0_57
	jal r0, .LBB0_104
.LBB0_90:
	addi r26, r0, 1
	ldw r25, sp+24
	jal r0, .LBB0_66
.LBB0_91:
	ldw r3, r28+0
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	jal r0, .LBB0_93
.LBB0_92:
	ldw r3, r28+0
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
.LBB0_93:
	jal r31, fprintf
	add r1, r16, r0
	jal r0, .LBB0_105
.LBB0_94:
	add r16, r18, r0
.LBB0_95:
	addi r14, r16, 1
	lui r13, %hi(.L.str.2)
	addi r13, r13, %lo(.L.str.2)
	bge r14, r11, .LBB0_98
.LBB0_96:
	slli r1, r16, 2
	add r1, r12, r1
	ldw r3, r1+0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r31, strcmp
	addi r3, r0, 0
	bne r1, r3, .LBB0_98
.LBB0_97:
	slli r1, r14, 2
	add r1, r12, r1
	ldw r13, r1+0
	addi r16, r16, 2
.LBB0_98:
	add r1, r15, r0
	add r3, r15, r0
	bge r16, r11, .LBB0_102
.LBB0_99:
	slli r1, r16, 2
	add r1, r12, r1
	ldw r3, r1+0
	jal r31, atoi
	add r4, r1, r0
	addi r3, r1, -1
	addi r1, r0, 0
	lui r5, 16
	addi r5, r5, -2
	bgtu r3, r5, .LBB0_107
.LBB0_100:
	addi r6, r16, 1
	add r3, r1, r0
	ble r11, r6, .LBB0_102
.LBB0_101:
	slli r1, r6, 2
	add r5, r12, r1
	sub r6, r11, r6
	add r3, r13, r0
	jal r31, do_send
	addi r3, r0, 1
.LBB0_102:
	bne r3, r15, .LBB0_105
.LBB0_103:
	lui r3, %hi(.L.str.4)
	addi r3, r3, %lo(.L.str.4)
	jal r31, printf
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	jal r31, printf
	lui r3, %hi(.L.str.6)
	addi r3, r3, %lo(.L.str.6)
	addi r4, r0, 5
	jal r31, printf
	lui r3, %hi(.L.str.7)
	addi r3, r3, %lo(.L.str.7)
	jal r31, printf
.LBB0_104:
	addi r1, r0, 1
.LBB0_105:
	ldw r28, sp+1112
	ldw r27, sp+1116
	ldw r26, sp+1120
	ldw r25, sp+1124
	ldw r24, sp+1128
	ldw r23, sp+1132
	ldw r22, sp+1136
	ldw r21, sp+1140
	ldw r20, sp+1144
	ldw r19, sp+1148
	ldw r18, sp+1152
	ldw r17, sp+1156
	ldw r16, sp+1160
	ldw r15, sp+1164
	ldw r14, sp+1168
	ldw r13, sp+1172
	ldw r12, sp+1176
	ldw r11, sp+1180
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 1184
	jalr r0, r31, 0
.LBB0_106:
	ldw r3, r28+0
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	jal r31, fprintf
	jal r0, .LBB0_104
.LBB0_107:
	add r3, r1, r0
	beq r3, r15, .LBB0_103
	jal r0, .LBB0_105
.LBB0_108:
	addi r1, r0, 0
	add r12, r1, r0
	beq r22, r1, .LBB0_110
.LBB0_109:
	add r3, r22, r0
	jal r31, fclose
.LBB0_110:
	ldw r3, sp+564
	jal r31, close
	add r3, r11, r0
	jal r31, close
	lui r3, %hi(.L.str.27)
	addi r3, r3, %lo(.L.str.27)
	jal r31, printf
	add r1, r12, r0
	jal r0, .LBB0_105
.LBB0_111:
	ldw r3, r28+0
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	jal r31, fprintf
	jal r0, .LBB0_104
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI0_0,@object
.LJTI0_0:
	.word	.LBB0_88
	.word	.LBB0_77
	.word	.LBB0_61
	.word	.LBB0_77
	.word	.LBB0_70
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_77
	.word	.LBB0_74
	.size	.LJTI0_0, 100
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function do_send
	.type	do_send,@function
do_send:                                # @do_send
# %bb.0:
	addi sp, sp, -928
	stw sp+0, lr
	stw sp+4, fp
	stw sp+924, r11
	stw sp+920, r12
	stw sp+916, r13
	stw sp+912, r14
	stw sp+908, r15
	stw sp+904, r16
	stw sp+900, r17
	stw sp+896, r18
	stw sp+892, r19
	stw sp+888, r20
	stw sp+884, r21
	stw sp+880, r22
	stw sp+876, r23
	stw sp+872, r24
	stw sp+868, r25
	stw sp+864, r26
	stw sp+860, r27
	stw sp+856, r28
	add r11, r6, r0
	add r12, r5, r0
	add r16, r4, r0
	add r17, r3, r0
	addi r14, r0, 2
	addi r13, r0, 1
	addi r15, r0, 0
	add r3, r14, r0
	add r4, r13, r0
	add r5, r15, r0
	jal r31, socket
	add r18, r1, r0
	addi r3, sp, 308
	addi r5, r0, 548
	add r4, r15, r0
	jal r31, memset
	stw sp+308, r18
	addi r20, r0, 94
	stw sp+832, r20
	addi r1, r0, 35
	stw sp+836, r1
	stw sp+840, r1
	lui r21, %hi(opt_time)
	addi r21, r21, %lo(opt_time)
	ldw r1, r21+0
	stw sp+844, r1
	addi r24, r0, -1
	stw sp+848, r24
	ble r18, r24, .LBB1_12
.LBB1_1:
	addi r18, sp, 292
	addi r15, r0, 0
	addi r19, r0, 16
	add r3, r18, r0
	add r4, r15, r0
	add r5, r19, r0
	jal r31, memset
	sth sp+292, r14
	add r3, r17, r0
	jal r31, inet_addr
	stw sp+296, r1
	add r3, r16, r0
	jal r31, htons
	addi r3, r18, 2
	sth r3+0, r1
	ldw r3, sp+308
	add r4, r18, r0
	add r5, r19, r0
	jal r31, connect
	ble r1, r24, .LBB1_13
.LBB1_2:
	addi r1, r0, 126
	stb sp+164, r1
	ldbu r1, r21+0
	addi r1, r1, 32
	addi r3, sp, 164
	addi r4, r3, 1
	stb r4+0, r1
	addi r1, r3, 2
	lui r3, 4
	addi r3, r3, 32
	sth r1+0, r3
	lui r1, 2
	addi r1, r1, 813
	sth sp+168, r1
	addi r18, r0, 83
	addi r16, r0, 6
	lui r17, %hi(.L.str.32)
	addi r17, r17, %lo(.L.str.32)
	addi r19, r0, 89
	addi r21, r0, 8
	add r22, r15, r0
.LBB1_3:
	addi r3, sp, 308
	addi r6, sp, 164
	add r4, r15, r0
	add r5, r18, r0
	add r7, r16, r0
	jal r31, send_pkt
	blt r1, r15, .LBB1_15
.LBB1_4:
	addi r3, sp, 308
	addi r4, sp, 24
	jal r31, read_pkt
	blt r1, r15, .LBB1_11
.LBB1_5:
	add r3, r13, r0
	bne r1, r15, .LBB1_7
.LBB1_6:
	ldw r1, sp+28
	sne r1, r1, r19
	ldw r3, sp+24
	sne r3, r3, r15
	or  r3, r1, r3
.LBB1_7:
	bgtu r22, r21, .LBB1_9
.LBB1_8:
	addi r22, r22, 1
	bne r3, r15, .LBB1_3
.LBB1_9:
	addi r13, r0, 0
	beq r3, r13, .LBB1_17
.LBB1_10:
	lui r17, %hi(.L.str.33)
	addi r17, r17, %lo(.L.str.33)
.LBB1_11:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	add r4, r17, r0
	jal r0, .LBB1_14
.LBB1_12:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	jal r0, .LBB1_14
.LBB1_13:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	add r5, r17, r0
	add r6, r16, r0
.LBB1_14:
	jal r31, fprintf
.LBB1_15:
	addi r1, r0, 1
.LBB1_16:
	ldw r28, sp+856
	ldw r27, sp+860
	ldw r26, sp+864
	ldw r25, sp+868
	ldw r24, sp+872
	ldw r23, sp+876
	ldw r22, sp+880
	ldw r21, sp+884
	ldw r20, sp+888
	ldw r19, sp+892
	ldw r18, sp+896
	ldw r17, sp+900
	ldw r16, sp+904
	ldw r15, sp+908
	ldw r14, sp+912
	ldw r13, sp+916
	ldw r12, sp+920
	ldw r11, sp+924
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 928
	jalr r0, r31, 0
.LBB1_17:
	stw sp+832, r20
	ldw r1, sp+160
	addi r21, r0, 1
	blt r1, r21, .LBB1_20
.LBB1_18:
	ldbu r3, sp+32
	addi r4, r3, -52
	andi r4, r4, 255
	addi r5, r0, 74
	bgtu r4, r5, .LBB1_20
.LBB1_19:
	addi r3, r3, -32
	stw sp+832, r3
.LBB1_20:
	blt r1, r14, .LBB1_23
.LBB1_21:
	ldbu r3, sp+33
	addi r4, r3, -33
	andi r4, r4, 255
	addi r5, r0, 93
	bgtu r4, r5, .LBB1_23
.LBB1_22:
	addi r3, r3, -32
	stw sp+844, r3
.LBB1_23:
	blt r1, r16, .LBB1_26
.LBB1_24:
	ldbu r1, sp+37
	addi r3, r1, -33
	andi r3, r3, 255
	addi r4, r0, 93
	bgtu r3, r4, .LBB1_26
.LBB1_25:
	stw sp+840, r1
.LBB1_26:
	lui r25, %hi(stderr)
	addi r25, r25, %lo(stderr)
	blt r11, r21, .LBB1_71
.LBB1_27:
	addi r26, r0, 1
	lui r14, %hi(.L.str.36)
	addi r14, r14, %lo(.L.str.36)
	addi r27, r0, 47
	lui r15, %hi(.L.str.38)
	addi r15, r15, %lo(.L.str.38)
	lui r28, %hi(stdout)
	addi r28, r28, %lo(stdout)
	addi r16, r0, 70
	addi fp, r0, 5
	addi r17, r0, 68
	addi r1, r0, 90
	stw sp+20, r1
	lui r1, %hi(.L.str.26)
	addi r1, r1, %lo(.L.str.26)
	stw sp+16, r1
	add r19, r13, r0
	add r21, r26, r0
.LBB1_28:
	slli r1, r19, 2
	add r1, r12, r1
	ldw r23, r1+0
	add r3, r23, r0
	add r4, r14, r0
	jal r31, fopen
	add r20, r1, r0
	addi r1, r23, 1
	add r22, r23, r0
	jal r0, .LBB1_31
.LBB1_29:
	add r22, r1, r0
.LBB1_30:
	addi r1, r1, 1
.LBB1_31:
	ldbu r3, r1+-1
	beq r3, r27, .LBB1_29
.LBB1_32:
	bne r3, r13, .LBB1_30
.LBB1_33:
	beq r20, r13, .LBB1_74
.LBB1_34:
	add r3, r15, r0
	add r4, r22, r0
	jal r31, printf
	ldw r3, r28+0
	jal r31, fflush
	ldbu r5, r22+0
	add r7, r13, r0
	beq r5, r13, .LBB1_45
.LBB1_35:
	ldw r1, sp+832
	addi r1, r1, -3
	ldw r3, sp+836
	addi r4, r22, 1
	addi r7, r0, 0
	jal r0, .LBB1_39
.LBB1_36:
	addi r8, sp, 24
	add r7, r8, r7
	stb r7+0, r3
	xori r5, r5, 64
.LBB1_37:
	stb r7+1, r5
.LBB1_38:
	add r7, r6, r0
	ldbu r5, r4+0
	addi r4, r4, 1
	addi r6, r0, 0
	beq r5, r6, .LBB1_45
.LBB1_39:
	addi r6, r7, 2
	bgt r6, r1, .LBB1_45
.LBB1_40:
	andi r8, r5, 127
	addi r9, r0, 32
	bltu r8, r9, .LBB1_36
.LBB1_41:
	addi r9, r0, 127
	beq r8, r9, .LBB1_36
.LBB1_42:
	andi r8, r3, 255
	andi r9, r5, 255
	bne r9, r8, .LBB1_44
.LBB1_43:
	addi r8, sp, 24
	add r7, r8, r7
	stb r7+0, r3
	jal r0, .LBB1_37
.LBB1_44:
	addi r6, r7, 1
	addi r8, sp, 24
	add r7, r8, r7
	stb r7+0, r5
	jal r0, .LBB1_38
.LBB1_45:
	addi r3, sp, 308
	addi r6, sp, 24
	add r4, r21, r0
	add r5, r16, r0
	jal r31, send_reliably
	ble r1, r24, .LBB1_75
.LBB1_46:
	addi r1, r21, 1
	andi r21, r1, 63
	add r22, r13, r0
	jal r0, .LBB1_49
.LBB1_47:
	add r1, fp, r0
.LBB1_48:
	bne r1, r13, .LBB1_67
.LBB1_49:
	ldw r1, sp+832
	bge r1, fp, .LBB1_54
.LBB1_50:
	add r23, r13, r0
.LBB1_51:
	beq r23, r13, .LBB1_47
.LBB1_52:
	addi r3, sp, 308
	addi r6, sp, 24
	add r4, r21, r0
	add r5, r17, r0
	add r7, r23, r0
	jal r31, send_reliably
	ble r1, r24, .LBB1_66
.LBB1_53:
	addi r1, r21, 1
	andi r21, r1, 63
	add r1, r13, r0
	jal r0, .LBB1_48
.LBB1_54:
	addi r18, r1, -3
	addi r23, r0, 0
	jal r0, .LBB1_57
.LBB1_55:
	addi r3, r23, 1
	addi r4, sp, 24
	add r4, r4, r23
	stb r4+0, r1
	add r23, r3, r0
.LBB1_56:
	addi r22, r22, 1
	addi r1, r23, 2
	bgt r1, r18, .LBB1_51
.LBB1_57:
	add r3, r20, r0
	jal r31, fgetc
	beq r1, r24, .LBB1_64
.LBB1_58:
	ldw r3, sp+836
	andi r4, r1, 127
	addi r5, r0, 32
	bltu r4, r5, .LBB1_62
.LBB1_59:
	addi r5, r0, 127
	beq r4, r5, .LBB1_62
.LBB1_60:
	andi r4, r3, 255
	andi r5, r1, 255
	bne r5, r4, .LBB1_55
.LBB1_61:
	addi r4, sp, 24
	add r4, r4, r23
	stb r4+0, r3
	jal r0, .LBB1_63
.LBB1_62:
	addi r4, sp, 24
	add r4, r4, r23
	stb r4+0, r3
	xori r1, r1, 64
.LBB1_63:
	addi r23, r23, 2
	stb r4+1, r1
	jal r0, .LBB1_56
.LBB1_64:
	add r3, r20, r0
	jal r31, ferror
	addi r3, r0, 0
	beq r1, r3, .LBB1_51
.LBB1_65:
	ldw r3, r25+0
	lui r4, %hi(.L.str.39)
	addi r4, r4, %lo(.L.str.39)
	jal r31, fprintf
	add r3, r20, r0
	jal r31, fclose
	addi r3, sp, 308
	addi r5, r0, 69
	addi r6, r0, 0
	add r4, r21, r0
	add r7, r6, r0
	jal r31, send_pkt
	add r1, r26, r0
	jal r0, .LBB1_48
.LBB1_66:
	add r3, r20, r0
	jal r31, fclose
	add r1, r26, r0
	jal r0, .LBB1_48
.LBB1_67:
	bne r1, fp, .LBB1_77
.LBB1_68:
	add r3, r20, r0
	jal r31, ferror
	bne r1, r13, .LBB1_76
.LBB1_69:
	add r3, r20, r0
	jal r31, fclose
	addi r3, sp, 308
	add r4, r21, r0
	ldw r5, sp+20
	add r6, r13, r0
	add r7, r13, r0
	jal r31, send_reliably
	blt r1, r13, .LBB1_77
.LBB1_70:
	addi r1, r21, 1
	andi r21, r1, 63
	ldw r3, sp+16
	add r4, r22, r0
	jal r31, printf
	addi r19, r19, 1
	bne r19, r11, .LBB1_28
.LBB1_71:
	addi r3, sp, 308
	addi r5, r0, 66
	addi r6, r0, 0
	add r4, r21, r0
	add r11, r6, r0
	add r7, r6, r0
	jal r31, send_reliably
	ble r1, r24, .LBB1_73
.LBB1_72:
	ldw r3, sp+308
	jal r31, close
	add r1, r11, r0
	jal r0, .LBB1_16
.LBB1_73:
	ldw r3, r25+0
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	jal r0, .LBB1_14
.LBB1_74:
	ldw r3, r25+0
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	add r5, r23, r0
	jal r31, fprintf
	jal r0, .LBB1_77
.LBB1_75:
	add r3, r20, r0
	jal r31, fclose
	jal r0, .LBB1_77
.LBB1_76:
	ldw r3, r25+0
	lui r4, %hi(.L.str.39)
	addi r4, r4, %lo(.L.str.39)
	jal r31, fprintf
	add r3, r20, r0
	jal r31, fclose
	addi r3, sp, 308
	addi r5, r0, 69
	addi r6, r0, 0
	add r4, r21, r0
	add r7, r6, r0
	jal r31, send_pkt
.LBB1_77:
	ldw r3, r25+0
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
	jal r0, .LBB1_14
.Lfunc_end1:
	.size	do_send, .Lfunc_end1-do_send
                                        # -- End function
	.p2align	2                               # -- Begin function read_pkt
	.type	read_pkt,@function
read_pkt:                               # @read_pkt
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
	add r12, r4, r0
	add r11, r3, r0
	addi r14, r0, -1
	stw r3+540, r14
	ldw r3, r3+536
	addi r13, r0, 1
	blt r3, r13, .LBB2_2
.LBB2_1:
	ldw r1, r11+544
	addi r5, r1, 1
	stw r11+544, r5
	addi r4, r0, 0
	jal r31, s32_timer_start
	stw r11+540, r1
.LBB2_2:
	add r3, r11, r0
	jal r31, link_getc
	ble r1, r14, .LBB2_12
.LBB2_3:
	bne r1, r13, .LBB2_2
.LBB2_4:
	add r3, r11, r0
	jal r31, link_getc
	ble r1, r14, .LBB2_12
.LBB2_5:
	stb sp+28, r1
	addi r3, r1, -127
	addi r4, r0, -92
	bltu r3, r4, .LBB2_13
.LBB2_6:
	addi r4, r0, 33
	addi r3, sp, 28
	blt r1, r4, .LBB2_18
.LBB2_7:
	add r15, r1, r0
	addi r16, r1, -32
	addi r18, r3, 1
	addi r17, r0, 0
	addi r13, r0, 1
.LBB2_8:
	add r3, r11, r0
	jal r31, link_getc
	ble r1, r14, .LBB2_12
.LBB2_9:
	beq r1, r13, .LBB2_13
.LBB2_10:
	add r3, r18, r17
	stb r3+0, r1
	addi r17, r17, 1
	bne r16, r17, .LBB2_8
.LBB2_11:
	addi r5, r17, 1
	add r1, r15, r0
	jal r0, .LBB2_19
.LBB2_12:
	addi r3, r0, -2
	seq r1, r1, r3
	addi r1, r1, -1
	ori  r13, r1, 2
.LBB2_13:
	ldw r3, r11+540
	addi r1, r0, 0
	blt r3, r1, .LBB2_15
.LBB2_14:
	jal r31, s32_timer_cancel
	stw r11+540, r14
.LBB2_15:
	addi r1, r0, 2
	bne r13, r1, .LBB2_17
.LBB2_16:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	ldw r5, r11+536
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	jal r31, fprintf
.LBB2_17:
	add r1, r13, r0
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
.LBB2_18:
	addi r5, r0, 1
.LBB2_19:
	addi r4, r5, -1
	addi r3, sp, 28
	add r3, r3, r4
	ldbu r3, r3+0
	addi r6, r0, 2
	bltu r5, r6, .LBB2_22
.LBB2_20:
	addi r6, r0, 0
	addi r7, sp, 28
	add r5, r6, r0
.LBB2_21:
	ldbu r8, r7+0
	add r5, r5, r8
	addi r4, r4, -1
	addi r7, r7, 1
	bne r4, r6, .LBB2_21
	jal r0, .LBB2_23
.LBB2_22:
	addi r5, r0, 0
.LBB2_23:
	srli r4, r5, 6
	andi r4, r4, 3
	add r4, r4, r5
	andi r4, r4, 63
	addi r4, r4, 32
	bne r4, r3, .LBB2_25
.LBB2_24:
	addi r4, sp, 28
	addi r3, r4, 1
	ldbu r3, r3+0
	addi r3, r3, -32
	stw r12+0, r3
	addi r3, r4, 2
	ldbu r3, r3+0
	stw r12+4, r3
	addi r5, r1, -35
	stw r12+136, r5
	addi r3, r12, 8
	addi r4, r4, 3
	jal r31, memcpy
	addi r13, r0, 0
	jal r0, .LBB2_13
.LBB2_25:
	addi r13, r0, 1
	jal r0, .LBB2_13
.Lfunc_end2:
	.size	read_pkt, .Lfunc_end2-read_pkt
                                        # -- End function
	.p2align	2                               # -- Begin function send_pkt
	.type	send_pkt,@function
send_pkt:                               # @send_pkt
# %bb.0:
	addi sp, sp, -176
	stw sp+0, lr
	stw sp+172, r11
	stw sp+168, r12
	stw sp+164, r13
	stw sp+160, r14
	stw sp+156, r15
	add r1, r5, r0
	add r11, r3, r0
	lui r3, %hi(send_pkt.sent)
	addi r3, r3, %lo(send_pkt.sent)
	ldw r5, r3+0
	addi r5, r5, 1
	stw r3+0, r5
	lui r3, %hi(opt_drop)
	addi r3, r3, %lo(opt_drop)
	ldw r3, r3+0
	bne r5, r3, .LBB3_2
.LBB3_1:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	add r6, r1, r0
	jal r31, fprintf
	addi r12, r0, 0
	jal r0, .LBB3_9
.LBB3_2:
	add r13, r7, r0
	addi r3, r0, 1
	stb sp+20, r3
	addi r3, r7, 35
	addi r5, sp, 20
	addi r14, r5, 1
	stb r14+0, r3
	andi r3, r4, 63
	addi r3, r3, 32
	addi r4, r5, 2
	stb r4+0, r3
	addi r3, r5, 3
	stb r3+0, r1
	addi r3, r5, 4
	add r4, r6, r0
	add r5, r7, r0
	jal r31, memcpy
	addi r12, r0, 0
	addi r3, r0, -2
	add r1, r12, r0
	blt r13, r3, .LBB3_5
.LBB3_3:
	addi r3, r13, 3
	addi r4, r0, 0
	add r1, r4, r0
.LBB3_4:
	ldbu r5, r14+0
	add r1, r1, r5
	addi r3, r3, -1
	addi r14, r14, 1
	bne r3, r4, .LBB3_4
.LBB3_5:
	srli r3, r1, 6
	andi r3, r3, 3
	add r1, r3, r1
	andi r1, r1, 63
	addi r1, r1, 32
	addi r3, sp, 20
	add r3, r3, r13
	stb r3+4, r1
	addi r13, r13, 6
	addi r1, r0, 13
	stb r3+5, r1
	addi r14, r0, -1
	add r15, r12, r0
.LBB3_6:
	ble r13, r15, .LBB3_9
.LBB3_7:
	ldw r3, r11+0
	addi r1, sp, 20
	add r4, r1, r15
	sub r5, r13, r15
	add r6, r12, r0
	jal r31, send
	sgt r3, r1, r12
	sub r3, r0, r3
	and r3, r1, r3
	add r15, r3, r15
	bgt r1, r12, .LBB3_6
.LBB3_8:
	add r12, r14, r0
.LBB3_9:
	add r1, r12, r0
	ldw r15, sp+156
	ldw r14, sp+160
	ldw r13, sp+164
	ldw r12, sp+168
	ldw r11, sp+172
	ldw lr, sp+0
	addi sp, sp, 176
	jalr r0, r31, 0
.Lfunc_end3:
	.size	send_pkt, .Lfunc_end3-send_pkt
                                        # -- End function
	.p2align	2                               # -- Begin function decode_data
	.type	decode_data,@function
decode_data:                            # @decode_data
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	ldw r11, r4+136
	addi r1, r0, 1
	blt r11, r1, .LBB4_8
.LBB4_1:
	addi r7, r4, 8
	addi lr, r0, 0
	addi r8, r0, 32
	addi r9, r0, 127
	add r10, lr, r0
.LBB4_2:
	addi r1, r10, 1
	add r12, r7, r10
	ldbu r12, r12+0
	ldw r13, r3+532
	bne r13, r12, .LBB4_5
.LBB4_3:
	bge r1, r11, .LBB4_5
.LBB4_4:
	addi r10, r10, 2
	add r1, r7, r1
	ldbu r1, r1+0
	xori r11, r1, 64
	andi r12, r11, 127
	sltu r13, r12, r8
	seq r12, r12, r9
	sub r12, r0, r12
	andi r12, r12, 64
	xor r1, r1, r12
	xor r11, r11, r1
	sub r12, r0, r13
	and r11, r11, r12
	xor r12, r1, r11
	jal r0, .LBB4_6
.LBB4_5:
	add r10, r1, r0
.LBB4_6:
	addi r1, lr, 1
	add lr, r5, lr
	stb lr+0, r12
	ldw r11, r4+136
	bge r10, r11, .LBB4_9
.LBB4_7:
	add lr, r1, r0
	bltu r1, r6, .LBB4_2
	jal r0, .LBB4_9
.LBB4_8:
	addi r1, r0, 0
.LBB4_9:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end4:
	.size	decode_data, .Lfunc_end4-decode_data
                                        # -- End function
	.p2align	2                               # -- Begin function sanitize
	.type	sanitize,@function
sanitize:                               # @sanitize
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	add r11, r3, r0
	jal r31, strlen
	add r12, r11, r1
	addi r13, r0, 0
	beq r1, r13, .LBB5_4
.LBB5_1:
	addi r1, r0, 47
.LBB5_2:
	ldbu r3, r12+-1
	beq r3, r1, .LBB5_4
.LBB5_3:
	addi r12, r12, -1
	bgtu r12, r11, .LBB5_2
.LBB5_4:
	beq r12, r11, .LBB5_6
.LBB5_5:
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, memmove
.LBB5_6:
	ldbu r1, r11+0
	addi r3, r0, 46
	beq r1, r3, .LBB5_8
.LBB5_7:
	bne r1, r13, .LBB5_9
.LBB5_8:
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	add r3, r11, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, strcpy
.LBB5_9:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end5:
	.size	sanitize, .Lfunc_end5-sanitize
                                        # -- End function
	.p2align	2                               # -- Begin function link_getc
	.type	link_getc,@function
link_getc:                              # @link_getc
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
	add r11, r3, r0
	ldw r1, r3+520
	ldw r3, r3+516
	blt r1, r3, .LBB6_13
.LBB6_1:
	addi r4, r0, 1
	addi r5, sp, 20
	add r3, r11, r0
	jal r31, s32_dpc_wait_on
	addi r15, r0, 0
	addi r12, r0, -1
	blt r1, r15, .LBB6_11
.LBB6_2:
	addi r16, r0, 52
	addi r14, r0, 1
	addi r13, r0, 0
	addi r17, r0, 50
	jal r0, .LBB6_6
.LBB6_3:
	ldw r1, sp+28
	ldw r3, r11+0
	beq r1, r3, .LBB6_15
.LBB6_4:
	addi r3, sp, 20
	jal r31, s32_dpc_unread
.LBB6_5:
	addi r5, sp, 20
	add r3, r11, r0
	add r4, r14, r0
	jal r31, s32_dpc_wait_on
	blt r1, r13, .LBB6_12
.LBB6_6:
	ldw r1, sp+20
	beq r1, r16, .LBB6_3
.LBB6_7:
	bne r1, r17, .LBB6_4
.LBB6_8:
	ldw r1, sp+32
	ldw r3, r11+544
	bne r1, r3, .LBB6_5
.LBB6_9:
	ldw r1, r11+540
	blt r1, r13, .LBB6_5
.LBB6_10:
	addi r1, r0, -1
	stw r11+540, r1
	addi r12, r0, -2
	jal r0, .LBB6_12
.LBB6_11:
	add r13, r15, r0
.LBB6_12:
	beq r13, r15, .LBB6_14
.LBB6_13:
	ldw r1, r11+520
	add r3, r11, r1
	addi r1, r1, 1
	stw r11+520, r1
	ldbu r12, r3+4
.LBB6_14:
	add r1, r12, r0
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
.LBB6_15:
	addi r13, r0, 0
	ldbu r1, sp+32
	andi r1, r1, 8
	bne r1, r13, .LBB6_12
.LBB6_16:
	addi r4, r11, 4
	addi r5, r0, 512
	addi r13, r0, 0
	add r6, r13, r0
	jal r31, recv
	blt r1, r14, .LBB6_12
.LBB6_17:
	stw r11+516, r1
	addi r1, r0, 0
	stw r11+520, r1
                                        # implicit-def: $r12
	add r13, r14, r0
	jal r0, .LBB6_12
.Lfunc_end6:
	.size	link_getc, .Lfunc_end6-link_getc
                                        # -- End function
	.p2align	2                               # -- Begin function send_reliably
	.type	send_reliably,@function
send_reliably:                          # @send_reliably
# %bb.0:
	addi sp, sp, -240
	stw sp+0, lr
	stw sp+236, r11
	stw sp+232, r12
	stw sp+228, r13
	stw sp+224, r14
	stw sp+220, r15
	stw sp+216, r16
	stw sp+212, r17
	stw sp+208, r18
	stw sp+204, r19
	stw sp+200, r20
	stw sp+196, r21
	stw sp+192, r22
	stw sp+188, r23
	stw sp+184, r24
	stw sp+180, r25
	stw sp+176, r26
	stw sp+172, r27
	stw sp+168, r28
	add r11, r7, r0
	add r12, r6, r0
	add r13, r5, r0
	add r14, r4, r0
	add r15, r3, r0
	andi r17, r4, 63
	addi r19, r0, 0
	addi r18, r0, -1
	addi r20, r0, 10
	addi r21, r0, 1
	addi r22, r0, 89
	addi r23, r0, 78
	addi r24, r0, 69
	lui r25, %hi(stderr)
	addi r25, r25, %lo(stderr)
	lui r16, %hi(.L.str.40)
	addi r16, r16, %lo(.L.str.40)
	add r26, r19, r0
                                        # implicit-def: $r27
.LBB7_1:
	add r3, r15, r0
	add r4, r14, r0
	add r5, r13, r0
	add r6, r12, r0
	add r7, r11, r0
	jal r31, send_pkt
	add r3, r1, r0
	add r1, r18, r0
	add r4, r19, r0
	blt r3, r19, .LBB7_14
.LBB7_2:
                                        # implicit-def: $r3
	jal r0, .LBB7_5
.LBB7_3:
	add r3, r28, r0
	add r4, r21, r0
.LBB7_4:
	beq r4, r19, .LBB7_12
.LBB7_5:
	add r28, r3, r0
	addi r4, sp, 28
	add r3, r15, r0
	jal r31, read_pkt
	add r3, r18, r0
	add r4, r19, r0
	blt r1, r19, .LBB7_4
.LBB7_6:
	add r3, r21, r0
	add r4, r19, r0
	bne r1, r19, .LBB7_4
.LBB7_7:
	ldw r1, sp+32
	beq r1, r22, .LBB7_11
.LBB7_8:
	add r3, r21, r0
	add r4, r19, r0
	beq r1, r23, .LBB7_4
.LBB7_9:
	bne r1, r24, .LBB7_3
.LBB7_10:
	ldw r3, r25+0
	add r4, r16, r0
	jal r31, fprintf
	add r3, r18, r0
	add r4, r19, r0
	jal r0, .LBB7_4
.LBB7_11:
	ldw r1, sp+28
	add r3, r19, r0
	add r4, r19, r0
	bne r1, r17, .LBB7_3
	jal r0, .LBB7_4
.LBB7_12:
	beq r3, r19, .LBB7_16
.LBB7_13:
	sgt r4, r3, r18
	xor r1, r27, r18
	sub r3, r0, r4
	and r1, r1, r3
	xor r1, r1, r18
.LBB7_14:
	beq r4, r19, .LBB7_18
.LBB7_15:
	addi r26, r26, 1
	add r27, r1, r0
	bne r26, r20, .LBB7_1
	jal r0, .LBB7_17
.LBB7_16:
	add r1, r19, r0
	add r4, r19, r0
	bne r4, r19, .LBB7_15
	jal r0, .LBB7_18
.LBB7_17:
	add r1, r18, r0
.LBB7_18:
	ldw r28, sp+168
	ldw r27, sp+172
	ldw r26, sp+176
	ldw r25, sp+180
	ldw r24, sp+184
	ldw r23, sp+188
	ldw r22, sp+192
	ldw r21, sp+196
	ldw r20, sp+200
	ldw r19, sp+204
	ldw r18, sp+208
	ldw r17, sp+212
	ldw r16, sp+216
	ldw r15, sp+220
	ldw r14, sp+224
	ldw r13, sp+228
	ldw r12, sp+232
	ldw r11, sp+236
	ldw lr, sp+0
	addi sp, sp, 240
	jalr r0, r31, 0
.Lfunc_end7:
	.size	send_reliably, .Lfunc_end7-send_reliably
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"-r"
	.size	.L.str, 3

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"-s"
	.size	.L.str.1, 3

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"127.0.0.1"
	.size	.L.str.2, 10

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"-h"
	.size	.L.str.3, 3

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"usage: kermit -r [-t SECS]\n"
	.size	.L.str.4, 28

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"       kermit -s [-t SECS] [-h A.B.C.D] PORT FILE...\n"
	.size	.L.str.5, 54

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"  -t SECS  how long the peer should wait for us (TIME, default %d; 0 = forever)\n"
	.size	.L.str.6, 81

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"  -x N     testing: lose our Nth outgoing packet\n"
	.size	.L.str.7, 50

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"-x"
	.size	.L.str.8, 3

	.type	opt_drop,@object                # @opt_drop
	.local	opt_drop
	.comm	opt_drop,4,4
	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"-t"
	.size	.L.str.9, 3

	.type	opt_time,@object                # @opt_time
	.data
	.p2align	2, 0x0
opt_time:
	.word	5                               # 0x5
	.size	opt_time, 4

	.type	.L.str.10,@object               # @.str.10
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.10:
	.asciz	"kermit: socket failed\n"
	.size	.L.str.10, 23

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"kermit: bind/listen failed\n"
	.size	.L.str.11, 28

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"kermit.port"
	.size	.L.str.12, 12

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"w"
	.size	.L.str.13, 2

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"kermit: cannot write kermit.port\n"
	.size	.L.str.14, 34

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"%u\n"
	.size	.L.str.15, 4

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"Kermit ready on 127.0.0.1:%u\n"
	.size	.L.str.16, 30

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"kermit: accept failed\n"
	.size	.L.str.17, 23

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"kermit: link lost\n"
	.size	.L.str.18, 19

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"kermit: peer silent, giving up\n"
	.size	.L.str.19, 32

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"kermit: peer error"
	.size	.L.str.20, 19

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"wb"
	.size	.L.str.21, 3

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"kermit: cannot create %s\n"
	.size	.L.str.22, 26

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"Receiving %s... "
	.size	.L.str.23, 17

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"kermit: write failed\n"
	.size	.L.str.24, 22

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"kermit: close failed\n"
	.size	.L.str.25, 22

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"OK (%ld bytes)\n"
	.size	.L.str.26, 16

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"Goodbye.\n"
	.size	.L.str.27, 10

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"kermit: timeout after %d s\n"
	.size	.L.str.28, 28

	.type	send_pkt.sent,@object           # @send_pkt.sent
	.local	send_pkt.sent
	.comm	send_pkt.sent,4,4
	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"kermit: (test) dropping packet %d, type %c\n"
	.size	.L.str.29, 44

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"kermit.out"
	.size	.L.str.30, 11

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"kermit: cannot connect to %s:%d\n"
	.size	.L.str.31, 33

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"kermit: link lost in init\n"
	.size	.L.str.32, 27

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"kermit: no answer to Send-Init\n"
	.size	.L.str.33, 32

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"kermit: transfer failed\n"
	.size	.L.str.34, 25

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"kermit: no goodbye\n"
	.size	.L.str.35, 20

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"rb"
	.size	.L.str.36, 3

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"kermit: cannot open %s\n"
	.size	.L.str.37, 24

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"Sending %s... "
	.size	.L.str.38, 15

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"kermit: read error\n"
	.size	.L.str.39, 20

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"kermit: peer error\n"
	.size	.L.str.40, 20

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git 732ffcb6281afb050e70a68b47aa40e7ab93d1d6)"
	.section	".note.GNU-stack","",@progbits
