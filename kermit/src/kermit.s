	.file	"kermit.c"
	.text
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi sp, sp, -1168
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 1168
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
	addi r15, r0, 2
	blt r3, r15, .LBB0_11
.LBB0_1:
	add r12, r4, r0
	add r11, r3, r0
	ldw r3, r4+4
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	jal r31, strcmp
	addi r14, r0, 0
	beq r1, r14, .LBB0_14
.LBB0_2:
	ldw r3, r12+4
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, strcmp
	bne r1, r14, .LBB0_11
.LBB0_3:
	lui r13, %hi(.L.str.2)
	addi r13, r13, %lo(.L.str.2)
	addi r16, r0, 4
	blt r11, r16, .LBB0_6
.LBB0_4:
	ldw r3, r12+8
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r31, strcmp
	addi r3, r0, 0
	bne r1, r3, .LBB0_6
.LBB0_5:
	ldw r13, r12+12
	add r15, r16, r0
.LBB0_6:
	add r1, r14, r0
	add r3, r14, r0
	bge r15, r11, .LBB0_10
.LBB0_7:
	slli r1, r15, 2
	add r1, r12, r1
	ldw r3, r1+0
	jal r31, atoi
	add r4, r1, r0
	addi r3, r1, -1
	addi r1, r0, 0
	lui r5, 16
	addi r5, r5, -2
	bgtu r3, r5, .LBB0_86
.LBB0_8:
	addi r6, r15, 1
	add r3, r1, r0
	ble r11, r6, .LBB0_10
.LBB0_9:
	slli r1, r6, 2
	add r5, r12, r1
	sub r6, r11, r6
	add r3, r13, r0
	jal r31, do_send
	addi r3, r0, 1
.LBB0_10:
	bne r3, r14, .LBB0_13
.LBB0_11:
	lui r3, %hi(.L.str.4)
	addi r3, r3, %lo(.L.str.4)
	jal r31, printf
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	jal r31, printf
.LBB0_12:
	addi r1, r0, 1
.LBB0_13:
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
	addi sp, sp, 1168
	jalr r0, r31, 0
.LBB0_14:
	addi r12, r0, 2
	addi r4, r0, 1
	addi r13, r0, 0
	add r3, r12, r0
	add r16, r4, r0
	add r5, r13, r0
	jal r31, socket
	addi r22, r0, -1
	ble r1, r22, .LBB0_83
.LBB0_15:
	add r11, r1, r0
	addi r14, fp, -760
	addi r15, r0, 16
	add r3, r14, r0
	add r4, r13, r0
	add r5, r15, r0
	jal r31, memset
	sth fp+-760, r12
	lui r1, 520192
	addi r3, r1, 1
	jal r31, htonl
	stw fp+-756, r1
	add r3, r13, r0
	jal r31, htons
	addi r3, r14, 2
	sth r3+0, r1
	add r3, r11, r0
	add r4, r14, r0
	add r5, r15, r0
	jal r31, bind
	blt r1, r13, .LBB0_82
.LBB0_16:
	addi r25, r0, 1
	add r3, r11, r0
	add r4, r25, r0
	jal r31, listen
	ble r1, r22, .LBB0_82
.LBB0_17:
	stw fp+-764, r15
	addi r16, fp, -760
	add r3, r16, r0
	add r4, r13, r0
	add r5, r15, r0
	jal r31, memset
	addi r5, fp, -764
	add r3, r11, r0
	add r4, r16, r0
	jal r31, getsockname
	lui r3, %hi(.L.str.8)
	addi r3, r3, %lo(.L.str.8)
	lui r4, %hi(.L.str.9)
	addi r4, r4, %lo(.L.str.9)
	jal r31, fopen
	lui r23, %hi(stderr)
	addi r23, r23, %lo(stderr)
	beq r1, r13, .LBB0_85
.LBB0_18:
	add r15, r1, r0
	addi r14, r16, 2
	ldhu r3, r14+0
	jal r31, ntohs
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	add r3, r15, r0
	add r5, r1, r0
	jal r31, fprintf
	add r3, r15, r0
	jal r31, fclose
	ldhu r3, r14+0
	jal r31, ntohs
	lui r3, %hi(.L.str.12)
	addi r3, r3, %lo(.L.str.12)
	add r4, r1, r0
	jal r31, printf
	lui r14, %hi(stdout)
	addi r14, r14, %lo(stdout)
	ldw r3, r14+0
	jal r31, fflush
	addi r3, fp, -744
	addi r5, r0, 532
	add r4, r13, r0
	jal r31, memset
	addi r24, r0, 94
	stw fp+-220, r24
	addi r26, r0, 35
	stw fp+-216, r26
	add r3, r11, r0
	add r4, r13, r0
	add r5, r13, r0
	jal r31, accept
	stw fp+-744, r1
	blt r1, r13, .LBB0_90
.LBB0_19:
	stw fp+-1140, r14
	addi r27, fp, -212
	addi r15, r27, 4
	addi r16, fp, -744
	addi r17, fp, -904
	lui r18, %hi(.L.str.14)
	addi r18, r18, %lo(.L.str.14)
	addi r28, r0, 83
	add r20, r13, r0
	add r19, r13, r0
	stw fp+-1136, r13
.LBB0_20:
	add r3, r16, r0
	add r4, r17, r0
	jal r31, read_pkt
	ble r1, r22, .LBB0_27
.LBB0_21:
	beq r1, r13, .LBB0_29
.LBB0_22:
	lui r1, 2
	addi r1, r1, 769
	sth fp+-212, r1
	addi r1, r19, 32
	addi r3, r27, 2
	stb r3+0, r1
	addi r1, r27, 3
	addi r3, r0, 78
	stb r1+0, r3
	addi r21, r0, 0
	add r3, r15, r0
	add r4, r21, r0
	add r5, r21, r0
	jal r31, memcpy
	addi r1, r0, 1
.LBB0_23:
	add r3, r27, r1
	ldbu r3, r3+0
	add r21, r21, r3
	addi r1, r1, 1
	addi r3, r0, 4
	bne r1, r3, .LBB0_23
.LBB0_24:
	srli r1, r21, 6
	andi r1, r1, 3
	add r1, r1, r21
	andi r1, r1, 63
	addi r1, r1, 32
	stb fp+-208, r1
	addi r1, r0, 13
	stb fp+-207, r1
	addi r14, r0, 0
.LBB0_25:
	addi r1, r0, 5
	bgtu r14, r1, .LBB0_42
.LBB0_26:
	ldw r3, fp+-744
	add r4, r27, r14
	addi r1, r0, 6
	sub r5, r1, r14
	addi r21, r0, 0
	add r6, r21, r0
	jal r31, send
	sgt r3, r1, r21
	sub r3, r21, r3
	and r3, r1, r3
	add r14, r3, r14
	bgt r1, r21, .LBB0_25
	jal r0, .LBB0_42
.LBB0_27:
	ldw r3, r23+0
	add r4, r18, r0
	jal r31, fprintf
	beq r20, r13, .LBB0_45
.LBB0_28:
	add r3, r20, r0
	jal r31, fclose
	add r14, r25, r0
	add r1, r25, r0
	jal r0, .LBB0_43
.LBB0_29:
	ldw r1, fp+-900
	bne r1, r28, .LBB0_46
.LBB0_30:
	stw fp+-220, r24
	stw fp+-216, r26
	ldw r1, fp+-768
	addi r14, r0, 1
	blt r1, r14, .LBB0_33
.LBB0_31:
	ldbu r3, fp+-896
	addi r4, r3, -52
	andi r4, r4, 255
	addi r5, r0, 74
	bgtu r4, r5, .LBB0_33
.LBB0_32:
	addi r3, r3, -32
	stw fp+-220, r3
.LBB0_33:
	addi r3, r0, 6
	blt r1, r3, .LBB0_36
.LBB0_34:
	ldbu r1, fp+-891
	addi r3, r1, -33
	andi r3, r3, 255
	addi r4, r0, 93
	bgtu r3, r4, .LBB0_36
.LBB0_35:
	stw fp+-216, r1
.LBB0_36:
	lui r1, 262658
	addi r1, r1, 1406
	stw fp+-1032, r1
	lui r1, 2
	addi r1, r1, 813
	sth fp+-1028, r1
	ldbu r1, fp+-904
	lui r3, 3
	addi r3, r3, -1791
	sth fp+-212, r3
	andi r1, r1, 63
	addi r1, r1, 32
	addi r3, r27, 2
	stb r3+0, r1
	addi r1, r27, 3
	addi r3, r0, 89
	stb r1+0, r3
	addi r4, fp, -1032
	addi r5, r0, 6
	add r3, r15, r0
	jal r31, memcpy
	addi r1, r0, 0
.LBB0_37:
	add r3, r27, r14
	ldbu r3, r3+0
	add r1, r1, r3
	addi r14, r14, 1
	addi r3, r0, 10
	bne r14, r3, .LBB0_37
.LBB0_38:
	srli r3, r1, 6
	andi r3, r3, 3
	add r1, r3, r1
	andi r1, r1, 63
	addi r1, r1, 32
	stb fp+-202, r1
	addi r1, r0, 13
	stb fp+-201, r1
	addi r14, r0, 0
.LBB0_39:
	addi r1, r0, 11
	bgtu r14, r1, .LBB0_41
.LBB0_40:
	ldw r3, fp+-744
	add r4, r27, r14
	addi r1, r0, 12
	sub r5, r1, r14
	addi r21, r0, 0
	add r6, r21, r0
	jal r31, send
	sgt r3, r1, r21
	sub r3, r21, r3
	and r3, r1, r3
	add r14, r3, r14
	bgt r1, r21, .LBB0_39
.LBB0_41:
	ldw r1, fp+-904
	addi r1, r1, 1
	andi r19, r1, 63
.LBB0_42:
	addi r14, r0, 1
	add r1, r12, r0
.LBB0_43:
	ori  r1, r1, 2
	bne r1, r12, .LBB0_12
.LBB0_44:
	bne r14, r13, .LBB0_20
	jal r0, .LBB0_87
.LBB0_45:
	add r14, r25, r0
	add r1, r25, r0
	add r20, r13, r0
	jal r0, .LBB0_43
.LBB0_46:
	ldw r3, fp+-904
	addi r4, r19, -1
	andi r4, r4, 63
	bne r3, r4, .LBB0_52
.LBB0_47:
	lui r1, 2
	addi r1, r1, 769
	sth fp+-212, r1
	addi r1, r3, 32
	addi r3, r27, 2
	stb r3+0, r1
	addi r1, r27, 3
	addi r3, r0, 89
	stb r1+0, r3
	addi r21, r0, 0
	add r3, r15, r0
	add r4, r21, r0
	add r5, r21, r0
	jal r31, memcpy
	addi r1, r0, 1
.LBB0_48:
	add r3, r27, r1
	ldbu r3, r3+0
	add r21, r21, r3
	addi r1, r1, 1
	addi r3, r0, 4
	bne r1, r3, .LBB0_48
.LBB0_49:
	srli r1, r21, 6
	andi r1, r1, 3
	add r1, r1, r21
	andi r1, r1, 63
	addi r1, r1, 32
	stb fp+-208, r1
	addi r1, r0, 13
	stb fp+-207, r1
	addi r14, r0, 0
.LBB0_50:
	addi r1, r0, 5
	bgtu r14, r1, .LBB0_42
.LBB0_51:
	ldw r3, fp+-744
	add r4, r27, r14
	addi r1, r0, 6
	sub r5, r1, r14
	addi r21, r0, 0
	add r6, r21, r0
	jal r31, send
	sgt r3, r1, r21
	sub r3, r21, r3
	and r3, r1, r3
	add r14, r3, r14
	bgt r1, r21, .LBB0_50
	jal r0, .LBB0_42
.LBB0_52:
	bne r3, r19, .LBB0_56
.LBB0_53:
	stw fp+-1132, r24
	addi r24, r0, 1
	addi r1, r1, -66
	addi r3, r0, 24
	bgtu r1, r3, .LBB0_68
.LBB0_54:
	slli r1, r1, 2
	lui r3, %hi(.LJTI0_0)
	addi r3, r3, %lo(.LJTI0_0)
	add r1, r3, r1
	ldw r1, r1+0
	add r14, r24, r0
	jalr r0, r1, 0
.LBB0_55:
	addi r14, r0, 0
	jal r0, .LBB0_74
.LBB0_56:
	lui r1, 2
	addi r1, r1, 769
	sth fp+-212, r1
	addi r1, r19, 32
	addi r3, r27, 2
	stb r3+0, r1
	addi r1, r27, 3
	addi r3, r0, 78
	stb r1+0, r3
	addi r21, r0, 0
	add r3, r15, r0
	add r4, r21, r0
	add r5, r21, r0
	jal r31, memcpy
	addi r1, r0, 1
.LBB0_57:
	add r3, r27, r1
	ldbu r3, r3+0
	add r21, r21, r3
	addi r1, r1, 1
	addi r3, r0, 4
	bne r1, r3, .LBB0_57
.LBB0_58:
	srli r1, r21, 6
	andi r1, r1, 3
	add r1, r1, r21
	andi r1, r1, 63
	addi r1, r1, 32
	stb fp+-208, r1
	addi r1, r0, 13
	stb fp+-207, r1
	addi r14, r0, 0
.LBB0_59:
	addi r1, r0, 5
	bgtu r14, r1, .LBB0_42
.LBB0_60:
	ldw r3, fp+-744
	add r4, r27, r14
	addi r1, r0, 6
	sub r5, r1, r14
	addi r21, r0, 0
	add r6, r21, r0
	jal r31, send
	sgt r3, r1, r21
	sub r3, r21, r3
	and r3, r1, r3
	add r14, r3, r14
	bgt r1, r21, .LBB0_59
	jal r0, .LBB0_42
.LBB0_61:
	addi r3, fp, -744
	addi r4, fp, -904
	addi r21, fp, -1128
	addi r6, r0, 95
	add r5, r21, r0
	jal r31, decode_data
	add r1, r21, r1
	addi r4, r0, 0
	stb r1+0, r4
	add r3, r21, r0
	add r21, r4, r0
	jal r31, sanitize
	beq r20, r21, .LBB0_63
.LBB0_62:
	add r3, r20, r0
	jal r31, fclose
.LBB0_63:
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	addi r3, fp, -1128
	jal r31, fopen
	add r20, r1, r0
	beq r1, r21, .LBB0_72
.LBB0_64:
	lui r3, %hi(.L.str.17)
	addi r3, r3, %lo(.L.str.17)
	addi r4, fp, -1128
	jal r31, printf
	ldw r1, fp+-1140
	ldw r3, r1+0
	jal r31, fflush
	addi r1, r0, 4
	stw fp+-1136, r21
	jal r0, .LBB0_73
.LBB0_65:
	addi r3, fp, -744
	addi r4, fp, -904
	addi r5, fp, -212
	addi r6, r0, 128
	jal r31, decode_data
	addi r3, r0, 0
	beq r20, r3, .LBB0_68
.LBB0_66:
	addi r3, r0, 1
	blt r1, r3, .LBB0_68
.LBB0_67:
	addi r3, fp, -212
	addi r4, r0, 1
	add r5, r1, r0
	add r6, r20, r0
	add r14, r1, r0
	jal r31, fwrite
	ldw r1, fp+-1136
	add r1, r14, r1
	stw fp+-1136, r1
.LBB0_68:
	add r14, r24, r0
	jal r0, .LBB0_74
.LBB0_69:
	addi r21, r0, 0
	beq r20, r21, .LBB0_71
.LBB0_70:
	add r3, r20, r0
	jal r31, fclose
	lui r3, %hi(.L.str.18)
	addi r3, r3, %lo(.L.str.18)
	ldw r4, fp+-1136
	jal r31, printf
.LBB0_71:
	add r14, r24, r0
	add r20, r21, r0
	jal r0, .LBB0_74
.LBB0_72:
	ldw r3, r23+0
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	addi r5, fp, -1128
	jal r31, fprintf
	ldw r4, fp+-904
	addi r3, fp, -744
	addi r5, r0, 69
	addi r6, r0, 0
	add r7, r6, r0
	jal r31, send_pkt
	addi r1, r0, 1
.LBB0_73:
	addi r14, r0, 1
	beq r20, r21, .LBB0_81
.LBB0_74:
	ldbu r1, fp+-904
	lui r3, 2
	addi r3, r3, 769
	sth fp+-212, r3
	andi r1, r1, 63
	addi r1, r1, 32
	addi r3, r27, 2
	stb r3+0, r1
	addi r1, r27, 3
	addi r3, r0, 89
	stb r1+0, r3
	addi r21, r0, 0
	add r3, r15, r0
	add r4, r21, r0
	add r5, r21, r0
	jal r31, memcpy
.LBB0_75:
	add r1, r27, r24
	ldbu r1, r1+0
	add r21, r21, r1
	addi r24, r24, 1
	addi r1, r0, 4
	bne r24, r1, .LBB0_75
.LBB0_76:
	srli r1, r21, 6
	andi r1, r1, 3
	add r1, r1, r21
	andi r1, r1, 63
	addi r1, r1, 32
	stb fp+-208, r1
	addi r1, r0, 13
	stb fp+-207, r1
	addi r24, r0, 0
.LBB0_77:
	addi r1, r0, 5
	bgtu r24, r1, .LBB0_79
.LBB0_78:
	ldw r3, fp+-744
	add r4, r27, r24
	addi r1, r0, 6
	sub r5, r1, r24
	addi r21, r0, 0
	add r6, r21, r0
	jal r31, send
	sgt r3, r1, r21
	sub r3, r21, r3
	and r3, r1, r3
	add r24, r3, r24
	bgt r1, r21, .LBB0_77
.LBB0_79:
	addi r1, r19, 1
	andi r19, r1, 63
	addi r1, r0, 0
.LBB0_80:
	ldw r24, fp+-1132
	jal r0, .LBB0_43
.LBB0_81:
	add r20, r21, r0
	jal r0, .LBB0_80
.LBB0_82:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	jal r0, .LBB0_84
.LBB0_83:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
.LBB0_84:
	jal r31, fprintf
	add r1, r16, r0
	jal r0, .LBB0_13
.LBB0_85:
	ldw r3, r23+0
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	jal r31, fprintf
	jal r0, .LBB0_12
.LBB0_86:
	add r3, r1, r0
	beq r3, r14, .LBB0_11
	jal r0, .LBB0_13
.LBB0_87:
	addi r1, r0, 0
	add r12, r1, r0
	beq r20, r1, .LBB0_89
.LBB0_88:
	add r3, r20, r0
	jal r31, fclose
.LBB0_89:
	ldw r3, fp+-744
	jal r31, close
	add r3, r11, r0
	jal r31, close
	lui r3, %hi(.L.str.19)
	addi r3, r3, %lo(.L.str.19)
	jal r31, printf
	add r1, r12, r0
	jal r0, .LBB0_13
.LBB0_90:
	ldw r3, r23+0
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	jal r31, fprintf
	jal r0, .LBB0_12
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI0_0,@object
.LJTI0_0:
	.word	.LBB0_55
	.word	.LBB0_74
	.word	.LBB0_65
	.word	.LBB0_74
	.word	.LBB0_61
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_74
	.word	.LBB0_69
	.size	.LJTI0_0, 100
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function do_send
	.type	do_send,@function
do_send:                                # @do_send
# %bb.0:
	addi sp, sp, -1072
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 1072
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
	add r11, r6, r0
	add r22, r5, r0
	add r15, r4, r0
	add r16, r3, r0
	addi r3, fp, -744
	addi r14, r0, 0
	addi r5, r0, 532
	add r4, r14, r0
	jal r31, memset
	addi r20, r0, 94
	stw fp+-220, r20
	addi r21, r0, 35
	stw fp+-216, r21
	addi r17, r0, 2
	addi r13, r0, 1
	add r3, r17, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, socket
	stw fp+-744, r1
	addi r26, r0, -1
	ble r1, r26, .LBB1_15
.LBB1_1:
	addi r18, fp, -760
	addi r12, r0, 0
	addi r19, r0, 16
	add r3, r18, r0
	add r4, r12, r0
	add r5, r19, r0
	jal r31, memset
	sth fp+-760, r17
	add r3, r16, r0
	jal r31, inet_addr
	stw fp+-756, r1
	add r3, r15, r0
	jal r31, htons
	addi r3, r18, 2
	sth r3+0, r1
	ldw r3, fp+-744
	add r4, r18, r0
	add r5, r19, r0
	jal r31, connect
	ble r1, r26, .LBB1_16
.LBB1_2:
	stw fp+-1044, r22
	stw fp+-1040, r11
	lui r1, 262658
	addi r1, r1, 1406
	stw fp+-888, r1
	lui r1, 2
	addi r1, r1, 813
	sth fp+-884, r1
	addi r22, fp, -212
	addi r16, r22, 4
	lui r1, 340483
	addi r23, r1, -1791
	addi r17, fp, -888
	addi r15, r0, 6
	addi r24, r0, 10
	addi r25, r0, 13
	addi r27, r0, 11
	addi r28, r0, 12
	addi r1, fp, -744
	stw fp+-1032, r1
	addi r19, fp, -1028
	addi r1, r0, 89
	stw fp+-1036, r1
	addi r11, r0, 8
	add r18, r12, r0
.LBB1_3:
	stw fp+-212, r23
	add r3, r16, r0
	add r4, r17, r0
	add r5, r15, r0
	jal r31, memcpy
	add r3, r13, r0
	add r1, r12, r0
.LBB1_4:
	add r4, r22, r3
	ldbu r4, r4+0
	add r1, r1, r4
	addi r3, r3, 1
	bne r3, r24, .LBB1_4
.LBB1_5:
	srli r3, r1, 6
	andi r3, r3, 3
	add r1, r3, r1
	andi r1, r1, 63
	addi r1, r1, 32
	stb fp+-202, r1
	stb fp+-201, r25
	add r14, r12, r0
.LBB1_6:
	bgtu r14, r27, .LBB1_8
.LBB1_7:
	ldw r3, fp+-744
	add r4, r22, r14
	sub r5, r28, r14
	add r6, r12, r0
	jal r31, send
	sgt r3, r1, r12
	sub r3, r12, r3
	and r3, r1, r3
	add r14, r3, r14
	bgt r1, r12, .LBB1_6
	jal r0, .LBB1_18
.LBB1_8:
	ldw r3, fp+-1032
	add r4, r19, r0
	jal r31, read_pkt
	ble r1, r26, .LBB1_20
.LBB1_9:
	add r3, r13, r0
	bne r1, r12, .LBB1_11
.LBB1_10:
	ldw r1, fp+-1024
	ldw r3, fp+-1036
	sne r1, r1, r3
	ldw r3, fp+-1028
	sne r3, r3, r12
	or  r3, r1, r3
.LBB1_11:
	bgtu r18, r11, .LBB1_13
.LBB1_12:
	addi r18, r18, 1
	bne r3, r12, .LBB1_3
.LBB1_13:
	addi r13, r0, 0
	beq r3, r13, .LBB1_21
.LBB1_14:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	jal r0, .LBB1_17
.LBB1_15:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	jal r0, .LBB1_17
.LBB1_16:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.21)
	addi r4, r4, %lo(.L.str.21)
	add r5, r16, r0
	add r6, r15, r0
.LBB1_17:
	jal r31, fprintf
.LBB1_18:
	addi r1, r0, 1
.LBB1_19:
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
	addi sp, sp, 1072
	jalr r0, r31, 0
.LBB1_20:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	jal r0, .LBB1_17
.LBB1_21:
	stw fp+-220, r20
	stw fp+-216, r21
	ldw r1, fp+-892
	addi r23, r0, 1
	blt r1, r23, .LBB1_24
.LBB1_22:
	ldbu r3, fp+-1020
	addi r4, r3, -52
	andi r4, r4, 255
	addi r5, r0, 74
	bgtu r4, r5, .LBB1_24
.LBB1_23:
	addi r3, r3, -32
	stw fp+-220, r3
.LBB1_24:
	ldw r5, fp+-1040
	blt r1, r15, .LBB1_27
.LBB1_25:
	ldbu r1, fp+-1015
	addi r3, r1, -33
	andi r3, r3, 255
	addi r4, r0, 93
	bgtu r3, r4, .LBB1_27
.LBB1_26:
	stw fp+-216, r1
.LBB1_27:
	blt r5, r23, .LBB1_69
.LBB1_28:
	addi r23, r0, 1
	lui r14, %hi(.L.str.26)
	addi r14, r14, %lo(.L.str.26)
	addi r27, r0, 47
	lui r15, %hi(.L.str.28)
	addi r15, r15, %lo(.L.str.28)
	lui r28, %hi(stdout)
	addi r28, r28, %lo(stdout)
	addi r16, fp, -744
	addi r17, r0, 70
	addi r18, fp, -1028
	addi r20, r0, 5
	addi r12, r0, 90
	lui r1, %hi(.L.str.18)
	addi r1, r1, %lo(.L.str.18)
	stw fp+-1032, r1
	addi r21, r0, 68
	add r19, r13, r0
.LBB1_29:
	slli r1, r19, 2
	ldw r3, fp+-1044
	add r1, r3, r1
	ldw r25, r1+0
	add r3, r25, r0
	add r4, r14, r0
	jal r31, fopen
	add r22, r1, r0
	addi r1, r25, 1
	add r24, r25, r0
	jal r0, .LBB1_32
.LBB1_30:
	add r24, r1, r0
.LBB1_31:
	addi r1, r1, 1
.LBB1_32:
	ldbu r3, r1+-1
	beq r3, r27, .LBB1_30
.LBB1_33:
	bne r3, r13, .LBB1_31
.LBB1_34:
	beq r22, r13, .LBB1_72
.LBB1_35:
	add r3, r15, r0
	add r4, r24, r0
	jal r31, printf
	ldw r3, r28+0
	jal r31, fflush
	ldbu r5, r24+0
	add r7, r13, r0
	beq r5, r13, .LBB1_46
.LBB1_36:
	ldw r1, fp+-220
	addi r1, r1, -3
	ldw r3, fp+-216
	addi r4, r24, 1
	addi r7, r0, 0
	jal r0, .LBB1_40
.LBB1_37:
	add r7, r18, r7
	stb r7+0, r3
	xori r5, r5, 64
.LBB1_38:
	stb r7+1, r5
.LBB1_39:
	add r7, r6, r0
	ldbu r5, r4+0
	addi r4, r4, 1
	addi r6, r0, 0
	beq r5, r6, .LBB1_46
.LBB1_40:
	addi r6, r7, 2
	bgt r6, r1, .LBB1_46
.LBB1_41:
	andi r8, r5, 127
	addi r9, r0, 32
	bltu r8, r9, .LBB1_37
.LBB1_42:
	addi r9, r0, 127
	beq r8, r9, .LBB1_37
.LBB1_43:
	andi r8, r3, 255
	andi r9, r5, 255
	bne r9, r8, .LBB1_45
.LBB1_44:
	add r7, r18, r7
	stb r7+0, r3
	jal r0, .LBB1_38
.LBB1_45:
	addi r6, r7, 1
	add r7, r18, r7
	stb r7+0, r5
	jal r0, .LBB1_39
.LBB1_46:
	add r3, r16, r0
	add r4, r23, r0
	add r5, r17, r0
	add r6, r18, r0
	jal r31, send_reliably
	ble r1, r26, .LBB1_73
.LBB1_47:
	addi r1, r23, 1
	andi r23, r1, 63
	add r24, r13, r0
	jal r0, .LBB1_50
.LBB1_48:
	add r1, r20, r0
.LBB1_49:
	bne r1, r13, .LBB1_66
.LBB1_50:
	ldw r1, fp+-220
	bge r1, r20, .LBB1_55
.LBB1_51:
	add r25, r13, r0
.LBB1_52:
	beq r25, r13, .LBB1_48
.LBB1_53:
	addi r6, fp, -1028
	add r3, r16, r0
	add r4, r23, r0
	add r5, r21, r0
	add r7, r25, r0
	jal r31, send_reliably
	ble r1, r26, .LBB1_65
.LBB1_54:
	addi r1, r23, 1
	andi r23, r1, 63
	addi r1, r0, 0
	jal r0, .LBB1_49
.LBB1_55:
	addi r11, r1, -3
	addi r25, r0, 0
	jal r0, .LBB1_58
.LBB1_56:
	addi r3, r25, 1
	add r4, r18, r25
	stb r4+0, r1
	add r25, r3, r0
.LBB1_57:
	addi r24, r24, 1
	addi r1, r25, 2
	bgt r1, r11, .LBB1_52
.LBB1_58:
	add r3, r22, r0
	jal r31, fgetc
	beq r1, r26, .LBB1_52
.LBB1_59:
	ldw r3, fp+-216
	andi r4, r1, 127
	addi r5, r0, 32
	bltu r4, r5, .LBB1_63
.LBB1_60:
	addi r5, r0, 127
	beq r4, r5, .LBB1_63
.LBB1_61:
	andi r4, r3, 255
	andi r5, r1, 255
	bne r5, r4, .LBB1_56
.LBB1_62:
	add r4, r18, r25
	stb r4+0, r3
	jal r0, .LBB1_64
.LBB1_63:
	add r4, r18, r25
	stb r4+0, r3
	xori r1, r1, 64
.LBB1_64:
	addi r25, r25, 2
	stb r4+1, r1
	jal r0, .LBB1_57
.LBB1_65:
	add r3, r22, r0
	jal r31, fclose
	addi r1, r0, 1
	jal r0, .LBB1_49
.LBB1_66:
	bne r1, r20, .LBB1_74
.LBB1_67:
	add r3, r22, r0
	jal r31, fclose
	add r3, r16, r0
	add r4, r23, r0
	add r5, r12, r0
	add r6, r13, r0
	add r7, r13, r0
	jal r31, send_reliably
	blt r1, r13, .LBB1_74
.LBB1_68:
	addi r1, r23, 1
	andi r23, r1, 63
	ldw r3, fp+-1032
	add r4, r24, r0
	jal r31, printf
	addi r19, r19, 1
	ldw r1, fp+-1040
	bne r19, r1, .LBB1_29
.LBB1_69:
	addi r3, fp, -744
	addi r5, r0, 66
	addi r6, r0, 0
	add r4, r23, r0
	add r11, r6, r0
	add r7, r6, r0
	jal r31, send_reliably
	ble r1, r26, .LBB1_71
.LBB1_70:
	ldw r3, fp+-744
	jal r31, close
	add r1, r11, r0
	jal r0, .LBB1_19
.LBB1_71:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	jal r0, .LBB1_17
.LBB1_72:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.27)
	addi r4, r4, %lo(.L.str.27)
	add r5, r25, r0
	jal r31, fprintf
	jal r0, .LBB1_74
.LBB1_73:
	add r3, r22, r0
	jal r31, fclose
.LBB1_74:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.24)
	addi r4, r4, %lo(.L.str.24)
	jal r0, .LBB1_17
.Lfunc_end1:
	.size	do_send, .Lfunc_end1-do_send
                                        # -- End function
	.p2align	2                               # -- Begin function read_pkt
	.type	read_pkt,@function
read_pkt:                               # @read_pkt
# %bb.0:
	addi sp, sp, -208
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 208
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
	add r13, r3, r0
	addi r14, r3, 4
	addi r15, r0, -1
	addi r12, r0, 0
	addi r17, r0, 1
	addi r16, r0, 512
.LBB2_1:
	ldw r1, r13+520
	ldw r3, r13+516
	blt r1, r3, .LBB2_4
.LBB2_2:
	ldw r3, r13+0
	add r4, r14, r0
	add r5, r16, r0
	add r6, r12, r0
	jal r31, recv
	add r3, r15, r0
	blt r1, r17, .LBB2_5
.LBB2_3:
	stw r13+516, r1
	stw r13+520, r12
.LBB2_4:
	ldw r1, r13+520
	addi r3, r1, 1
	stw r13+520, r3
	add r1, r14, r1
	ldbu r3, r1+0
.LBB2_5:
	blt r3, r12, .LBB2_31
.LBB2_6:
	bne r3, r17, .LBB2_1
.LBB2_7:
	ldw r1, r13+520
	ldw r3, r13+516
	blt r1, r3, .LBB2_10
.LBB2_8:
	ldw r3, r13+0
	addi r5, r0, 512
	addi r16, r0, 0
	add r4, r14, r0
	add r6, r16, r0
	jal r31, recv
	blt r1, r17, .LBB2_22
.LBB2_9:
	stw r13+516, r1
	stw r13+520, r16
.LBB2_10:
	ldw r1, r13+520
	add r3, r13, r1
	addi r1, r1, 1
	stw r13+520, r1
	ldbu r18, r3+4
	blt r18, r12, .LBB2_31
.LBB2_11:
	stb fp+-180, r18
	addi r1, r18, -127
	addi r3, r0, -92
	add r15, r17, r0
	bltu r1, r3, .LBB2_31
.LBB2_12:
	addi r1, r0, 33
	blt r18, r1, .LBB2_23
.LBB2_13:
	addi r19, r18, -32
	addi r1, fp, -180
	addi r20, r1, 1
	addi r16, r0, 0
	addi r15, r0, -1
	addi r21, r0, 1
	addi r17, r0, 512
	add r22, r16, r0
.LBB2_14:
	ldw r1, r13+520
	ldw r3, r13+516
	blt r1, r3, .LBB2_17
.LBB2_15:
	ldw r3, r13+0
	add r4, r14, r0
	add r5, r17, r0
	add r6, r16, r0
	jal r31, recv
	add r3, r15, r0
	blt r1, r21, .LBB2_18
.LBB2_16:
	stw r13+516, r1
	stw r13+520, r16
.LBB2_17:
	ldw r1, r13+520
	addi r3, r1, 1
	stw r13+520, r3
	add r1, r14, r1
	ldbu r3, r1+0
.LBB2_18:
	blt r3, r16, .LBB2_31
.LBB2_19:
	beq r3, r21, .LBB2_30
.LBB2_20:
	add r1, r20, r22
	stb r1+0, r3
	addi r22, r22, 1
	bne r19, r22, .LBB2_14
.LBB2_21:
	addi r5, r22, 1
	jal r0, .LBB2_24
.LBB2_22:
	addi r18, r0, -1
	bge r18, r12, .LBB2_11
	jal r0, .LBB2_31
.LBB2_23:
	addi r5, r0, 1
.LBB2_24:
	addi r1, fp, -180
	addi r4, r5, -1
	add r3, r1, r4
	ldbu r3, r3+0
	addi r6, r0, 2
	bltu r5, r6, .LBB2_27
.LBB2_25:
	addi r5, r0, 0
	add r6, r1, r0
	add r12, r5, r0
.LBB2_26:
	ldbu r7, r6+0
	add r12, r12, r7
	addi r4, r4, -1
	addi r6, r6, 1
	bne r4, r5, .LBB2_26
.LBB2_27:
	srli r4, r12, 6
	andi r4, r4, 3
	add r4, r4, r12
	andi r4, r4, 63
	addi r4, r4, 32
	bne r4, r3, .LBB2_29
.LBB2_28:
	addi r3, r1, 1
	ldbu r3, r3+0
	addi r3, r3, -32
	stw r11+0, r3
	addi r3, r1, 2
	ldbu r3, r3+0
	stw r11+4, r3
	addi r5, r18, -35
	stw r11+136, r5
	addi r3, r11, 8
	addi r4, r1, 3
	jal r31, memcpy
	addi r15, r0, 0
	jal r0, .LBB2_31
.LBB2_29:
	addi r15, r0, 1
	jal r0, .LBB2_31
.LBB2_30:
	add r15, r21, r0
.LBB2_31:
	add r1, r15, r0
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
	addi sp, sp, 208
	jalr r0, r31, 0
.Lfunc_end2:
	.size	read_pkt, .Lfunc_end2-read_pkt
                                        # -- End function
	.p2align	2                               # -- Begin function send_pkt
	.type	send_pkt,@function
send_pkt:                               # @send_pkt
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
	add r13, r7, r0
	add r11, r3, r0
	addi r1, r0, 1
	stb fp+-156, r1
	addi r1, r7, 35
	addi r14, fp, -156
	addi r15, r14, 1
	stb r15+0, r1
	andi r1, r4, 63
	addi r1, r1, 32
	addi r3, r14, 2
	stb r3+0, r1
	addi r1, r14, 3
	stb r1+0, r5
	addi r3, r14, 4
	add r4, r6, r0
	add r5, r7, r0
	jal r31, memcpy
	addi r12, r0, 0
	addi r3, r0, -2
	add r1, r12, r0
	blt r13, r3, .LBB3_3
.LBB3_1:
	addi r3, r13, 3
	addi r4, r0, 0
	add r1, r4, r0
.LBB3_2:
	ldbu r5, r15+0
	add r1, r1, r5
	addi r3, r3, -1
	addi r15, r15, 1
	bne r3, r4, .LBB3_2
.LBB3_3:
	srli r3, r1, 6
	andi r3, r3, 3
	add r1, r3, r1
	andi r1, r1, 63
	addi r1, r1, 32
	add r3, r14, r13
	stb r3+4, r1
	addi r13, r13, 6
	addi r1, r0, 13
	stb r3+5, r1
	add r15, r12, r0
.LBB3_4:
	ble r13, r15, .LBB3_6
.LBB3_5:
	ldw r3, r11+0
	add r4, r14, r15
	sub r5, r13, r15
	add r6, r12, r0
	jal r31, send
	sgt r3, r1, r12
	sub r3, r12, r3
	and r3, r1, r3
	add r15, r3, r15
	bgt r1, r12, .LBB3_4
.LBB3_6:
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
	.size	send_pkt, .Lfunc_end3-send_pkt
                                        # -- End function
	.p2align	2                               # -- Begin function decode_data
	.type	decode_data,@function
decode_data:                            # @decode_data
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
	ldw r12, r4+136
	addi r1, r0, 1
	blt r12, r1, .LBB4_8
.LBB4_1:
	addi r7, r4, 8
	addi r8, r0, 0
	addi r9, r0, 32
	addi r10, r0, 127
	add lr, r8, r0
	add r11, r8, r0
.LBB4_2:
	addi r1, lr, 1
	add r13, r7, lr
	ldbu r13, r13+0
	ldw r14, r3+528
	bne r14, r13, .LBB4_5
.LBB4_3:
	bge r1, r12, .LBB4_5
.LBB4_4:
	addi lr, lr, 2
	add r1, r7, r1
	ldbu r1, r1+0
	xori r12, r1, 64
	andi r13, r12, 127
	sltu r14, r13, r9
	seq r13, r13, r10
	sub r13, r8, r13
	andi r13, r13, 64
	xor r1, r1, r13
	xor r12, r12, r1
	sub r13, r8, r14
	and r12, r12, r13
	xor r13, r1, r12
	jal r0, .LBB4_6
.LBB4_5:
	add lr, r1, r0
.LBB4_6:
	addi r1, r11, 1
	add r11, r5, r11
	stb r11+0, r13
	ldw r12, r4+136
	bge lr, r12, .LBB4_9
.LBB4_7:
	add r11, r1, r0
	bltu r1, r6, .LBB4_2
	jal r0, .LBB4_9
.LBB4_8:
	addi r1, r0, 0
.LBB4_9:
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
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
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 32
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
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
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	add r3, r11, r0
	jal r31, strcpy
.LBB5_9:
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end5:
	.size	sanitize, .Lfunc_end5-sanitize
                                        # -- End function
	.p2align	2                               # -- Begin function send_reliably
	.type	send_reliably,@function
send_reliably:                          # @send_reliably
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
	add r11, r7, r0
	add r17, r6, r0
	add r19, r5, r0
	add r14, r3, r0
	addi r27, r7, 35
	addi r15, fp, -212
	addi r20, r15, 1
	andi r1, r4, 63
	stw fp+-232, r1
	addi r28, r1, 32
	addi r25, r15, 4
	add r1, r15, r7
	stw fp+-224, r1
	addi r24, r7, 6
	addi r1, r7, 3
	stw fp+-240, r1
	addi r12, r0, 0
	addi r26, r0, 1
	addi r13, r15, 2
	addi r21, r15, 3
	addi r1, r0, -2
	stw fp+-216, r1
	addi r1, r0, 13
	stw fp+-220, r1
	addi r18, r0, -1
	addi r1, r0, 10
	stw fp+-228, r1
                                        # implicit-def: $r23
	add r22, r12, r0
	stw fp+-236, r20
	jal r0, .LBB6_4
.LBB6_1:
	add r23, r12, r0
	add r5, r12, r0
.LBB6_2:
	add r17, r19, r0
	add r19, r27, r0
	add r27, r28, r0
	add r28, r13, r0
	add r13, r21, r0
	add r21, r20, r0
	ldw r20, fp+-236
	beq r5, r12, .LBB6_25
.LBB6_3:
	addi r22, r22, 1
	ldw r1, fp+-228
	beq r22, r1, .LBB6_24
.LBB6_4:
	stb fp+-212, r26
	stb r20+0, r27
	stb r13+0, r28
	stb r21+0, r19
	add r3, r25, r0
	add r4, r17, r0
	add r5, r11, r0
	jal r31, memcpy
	add r1, r12, r0
	ldw r3, fp+-216
	blt r11, r3, .LBB6_7
.LBB6_5:
	add r3, r20, r0
	ldw r4, fp+-240
	add r1, r12, r0
.LBB6_6:
	ldbu r5, r3+0
	add r1, r1, r5
	addi r4, r4, -1
	addi r3, r3, 1
	bne r4, r12, .LBB6_6
.LBB6_7:
	srli r3, r1, 6
	andi r3, r3, 3
	add r1, r3, r1
	andi r1, r1, 63
	addi r1, r1, 32
	ldw r3, fp+-224
	stb r3+4, r1
	ldw r1, fp+-220
	stb r3+5, r1
	add r16, r12, r0
.LBB6_8:
	ble r24, r16, .LBB6_11
.LBB6_9:
	ldw r3, r14+0
	add r4, r15, r16
	sub r5, r24, r16
	add r6, r12, r0
	jal r31, send
	sgt r3, r1, r12
	sub r3, r12, r3
	and r3, r1, r3
	add r16, r3, r16
	bgt r1, r12, .LBB6_8
.LBB6_10:
	add r23, r18, r0
	add r5, r12, r0
	bne r5, r12, .LBB6_3
	jal r0, .LBB6_25
.LBB6_11:
	add r20, r21, r0
	add r21, r13, r0
	add r13, r28, r0
	add r28, r27, r0
	add r27, r19, r0
	add r19, r17, r0
                                        # implicit-def: $r3
	jal r0, .LBB6_14
.LBB6_12:
	add r3, r26, r0
.LBB6_13:
	beq r17, r12, .LBB6_22
.LBB6_14:
	add r16, r3, r0
	add r3, r14, r0
	add r4, r15, r0
	jal r31, read_pkt
	add r3, r18, r0
	add r17, r12, r0
	blt r1, r12, .LBB6_13
.LBB6_15:
	addi r17, r0, 0
	bne r1, r17, .LBB6_12
.LBB6_16:
	ldw r1, fp+-208
	addi r3, r0, 89
	beq r1, r3, .LBB6_20
.LBB6_17:
	addi r3, r0, 78
	beq r1, r3, .LBB6_12
.LBB6_18:
	addi r3, r0, 69
	bne r1, r3, .LBB6_21
.LBB6_19:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	jal r31, fprintf
	add r3, r18, r0
	jal r0, .LBB6_13
.LBB6_20:
	ldw r1, fp+-212
	addi r3, r0, 0
	add r17, r3, r0
	ldw r4, fp+-232
	beq r1, r4, .LBB6_13
.LBB6_21:
	addi r17, r0, 1
	add r3, r16, r0
	jal r0, .LBB6_13
.LBB6_22:
	beq r3, r12, .LBB6_1
.LBB6_23:
	sgt r5, r3, r18
	xor r3, r23, r18
	sub r4, r12, r5
	and r3, r3, r4
	xor r23, r3, r18
	jal r0, .LBB6_2
.LBB6_24:
	add r23, r18, r0
.LBB6_25:
	add r1, r23, r0
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
.Lfunc_end6:
	.size	send_reliably, .Lfunc_end6-send_reliably
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
	.asciz	"usage: kermit -r\n"
	.size	.L.str.4, 18

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"       kermit -s [-h A.B.C.D] PORT FILE...\n"
	.size	.L.str.5, 44

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"kermit: socket failed\n"
	.size	.L.str.6, 23

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"kermit: bind/listen failed\n"
	.size	.L.str.7, 28

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"kermit.port"
	.size	.L.str.8, 12

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"w"
	.size	.L.str.9, 2

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"kermit: cannot write kermit.port\n"
	.size	.L.str.10, 34

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"%u\n"
	.size	.L.str.11, 4

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"Kermit ready on 127.0.0.1:%u\n"
	.size	.L.str.12, 30

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"kermit: accept failed\n"
	.size	.L.str.13, 23

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"kermit: link lost\n"
	.size	.L.str.14, 19

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"wb"
	.size	.L.str.15, 3

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"kermit: cannot create %s\n"
	.size	.L.str.16, 26

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"Receiving %s... "
	.size	.L.str.17, 17

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"OK (%ld bytes)\n"
	.size	.L.str.18, 16

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"Goodbye.\n"
	.size	.L.str.19, 10

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"kermit.out"
	.size	.L.str.20, 11

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"kermit: cannot connect to %s:%d\n"
	.size	.L.str.21, 33

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"kermit: link lost in init\n"
	.size	.L.str.22, 27

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"kermit: no answer to Send-Init\n"
	.size	.L.str.23, 32

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"kermit: transfer failed\n"
	.size	.L.str.24, 25

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"kermit: no goodbye\n"
	.size	.L.str.25, 20

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"rb"
	.size	.L.str.26, 3

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"kermit: cannot open %s\n"
	.size	.L.str.27, 24

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"Sending %s... "
	.size	.L.str.28, 15

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"kermit: peer error\n"
	.size	.L.str.29, 20

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e34f541beea69553ff1fd655361b4faa1e656dc2)"
	.section	".note.GNU-stack","",@progbits
