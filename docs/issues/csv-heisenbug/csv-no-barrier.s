	.file	"csv.c"
	.text
	.globl	csv_init                        # -- Begin function csv_init
	.p2align	2
	.type	csv_init,@function
csv_init:                               # @csv_init
# %bb.0:
	addi r5, r0, 0
	lui r1, 4
	addi r4, r1, 536
	add r6, r5, r0
.LBB0_1:
	add r7, r3, r6
	stb r7+0, r5
	addi r6, r6, 1
	bltu r6, r4, .LBB0_1
.LBB0_2:
	addi r5, r0, 1
	stw r3+12, r5
	add r4, r3, r4
	addi r5, r0, 0
	stw r4+0, r5
	addi r4, r1, 540
	add r4, r3, r4
	stw r4+0, r5
	addi r4, r1, 544
	add r4, r3, r4
	stw r4+0, r5
	addi r1, r1, 548
	add r1, r3, r1
	stw r1+0, r5
	jalr r0, r31, 0
.Lfunc_end0:
	.size	csv_init, .Lfunc_end0-csv_init
                                        # -- End function
	.globl	csv_open                        # -- Begin function csv_open
	.p2align	2
	.type	csv_open,@function
csv_open:                               # @csv_open
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, lr
	add r1, r4, r0
	add r12, r3, r0
	addi r5, r0, 0
	lui r3, 4
	addi r4, r3, 536
	add r6, r5, r0
.LBB1_1:
	add r7, r12, r6
	stb r7+0, r5
	addi r6, r6, 1
	bltu r6, r4, .LBB1_1
.LBB1_2:
	addi r5, r0, 1
	stw r12+12, r5
	add r4, r12, r4
	addi r13, r0, 0
	stw r4+0, r13
	addi r4, r3, 540
	add r4, r12, r4
	stw r4+0, r13
	addi r4, r3, 544
	add r4, r12, r4
	stw r4+0, r13
	addi r3, r3, 548
	add r3, r12, r3
	stw r3+0, r13
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	add r3, r1, r0
	jal r31, fopen
	addi r11, r0, -1
	beq r1, r13, .LBB1_5
.LBB1_3:
	add r14, r1, r0
	addi r5, r0, 2
	add r3, r1, r0
	add r4, r13, r0
	jal r31, fseek
	beq r1, r13, .LBB1_6
.LBB1_4:
	add r3, r14, r0
	jal r31, fclose
.LBB1_5:
	add r1, r11, r0
	ldw lr, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB1_6:
	add r3, r14, r0
	jal r31, ftell
	addi r11, r0, -1
	ble r1, r11, .LBB1_4
.LBB1_7:
	add r15, r1, r0
	add r3, r14, r0
	add r4, r13, r0
	add r5, r13, r0
	jal r31, fseek
	bne r1, r13, .LBB1_4
.LBB1_8:
	beq r15, r13, .LBB1_12
.LBB1_9:
	add r3, r15, r0
	jal r31, malloc
	stw r12+0, r1
	beq r1, r13, .LBB1_4
.LBB1_10:
	addi r4, r0, 1
	add r3, r1, r0
	add r5, r15, r0
	add r6, r14, r0
	jal r31, fread
	add r16, r1, r0
	add r3, r14, r0
	jal r31, fclose
	bne r16, r15, .LBB1_13
.LBB1_11:
	stw r12+4, r15
	add r11, r13, r0
	jal r0, .LBB1_5
.LBB1_12:
	addi r11, r0, 0
	stw r12+0, r11
	stw r12+4, r11
	jal r0, .LBB1_4
.LBB1_13:
	ldw r3, r12+0
	jal r31, free
	stw r12+0, r13
	stw r12+4, r13
	jal r0, .LBB1_5
.Lfunc_end1:
	.size	csv_open, .Lfunc_end1-csv_open
                                        # -- End function
	.globl	csv_close                       # -- Begin function csv_close
	.p2align	2
	.type	csv_close,@function
csv_close:                              # @csv_close
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	ldw r3, r3+0
	addi r12, r0, 0
	beq r3, r12, .LBB2_2
.LBB2_1:
	jal r31, free
	stw r11+0, r12
.LBB2_2:
	stw r11+4, r12
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	csv_close, .Lfunc_end2-csv_close
                                        # -- End function
	.globl	csv_parse                       # -- Begin function csv_parse
	.p2align	2
	.type	csv_parse,@function
csv_parse:                              # @csv_parse
# %bb.0:
	addi sp, sp, -136
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 136
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
	stw fp+-76, lr
	add r11, r3, r0
	ldw r3, r3+4
	addi r1, r0, 0
	beq r3, r1, .LBB3_105
.LBB3_1:
	ldw r3, r11+0
	addi r1, r0, 0
	beq r3, r1, .LBB3_105
.LBB3_2:
	add r13, r5, r0
	add r12, r4, r0
	lui r1, 4
	addi r3, r1, 536
	add r21, r11, r3
	addi r17, r0, 0
	stw r21+0, r17
	addi r3, r1, 540
	add r23, r11, r3
	stw r23+0, r17
	addi r3, r1, 532
	add r16, r11, r3
	stw r16+0, r17
	stw r11+528, r17
	addi r14, r11, 532
	addi r15, r11, 16
	addi r3, r0, -1
	stw fp+-84, r3
	addi r25, r0, 7
	lui r26, %hi(.LJTI3_0)
	addi r26, r26, %lo(.LJTI3_0)
	addi r28, r0, 44
	addi r3, r0, 510
	stw fp+-100, r3
	addi r18, r0, 1
	addi r3, r0, 3
	stw fp+-92, r3
	lui r3, %hi(.LJTI3_4)
	addi r3, r3, %lo(.LJTI3_4)
	stw fp+-116, r3
	addi r3, r0, 5
	stw fp+-80, r3
	addi r3, r1, 548
	add r19, r11, r3
	addi r1, r1, 544
	add r20, r11, r1
	addi r1, r0, 8
	stw fp+-120, r1
	addi r24, r0, 31
	addi r1, r0, 511
	stw fp+-96, r1
	addi r1, r0, 4
	stw fp+-112, r1
	addi r1, r0, 2
	stw fp+-104, r1
	addi r1, r0, 34
	stw fp+-88, r1
	lui r1, %hi(.LJTI3_3)
	addi r1, r1, %lo(.LJTI3_3)
	stw fp+-108, r1
	add r22, r17, r0
	jal r0, .LBB3_6
.LBB3_3:
	ldw r3, r11+12
.LBB3_4:
	stw r16+0, r27
	stw r11+528, r27
	addi r1, r3, 1
	stw r11+12, r1
	stw r21+0, r27
.LBB3_5:
	addi r22, r22, 1
	ldw r1, r11+4
	bgeu r22, r1, .LBB3_94
.LBB3_6:
	ldw r1, r21+0
	bgtu r1, r25, .LBB3_104
.LBB3_7:
	ldw r3, r11+0
	add r3, r3, r22
	ldbu r3, r3+0
	slli r1, r1, 2
	add r1, r26, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB3_8:
	addi r1, r0, 10
	bne r3, r1, .LBB3_104
.LBB3_9:
	ldw r1, fp+-80
	stw r23+0, r1
	ldw r1, r19+0
	ldw r4, r16+0
	addi r27, r0, 0
	beq r1, r27, .LBB3_37
.LBB3_10:
	ldw r1, r20+0
	bne r4, r1, .LBB3_104
.LBB3_11:
	ldw r3, r11+12
	bne r12, r27, .LBB3_38
	jal r0, .LBB3_4
.LBB3_12:
	bgtu r3, r28, .LBB3_48
.LBB3_13:
	slli r1, r3, 2
	lui r4, %hi(.LJTI3_1)
	addi r4, r4, %lo(.LJTI3_1)
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB3_14:
	ldw r4, r16+0
	bgt r4, r24, .LBB3_36
.LBB3_15:
	ldw r1, r11+528
	add r1, r15, r1
	addi r27, r0, 0
	stb r1+0, r27
	add r3, r15, r0
	jal r31, strlen
	ldw r3, fp+-96
	sltu r3, r1, r3
	sub r3, r27, r3
	xori r1, r1, 511
	and r1, r1, r3
	xori r1, r1, 511
	beq r1, r27, .LBB3_35
.LBB3_16:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r4, r27, r0
.LBB3_17:
	add r5, r15, r4
	ldbu r5, r5+0
	add r6, r3, r4
	stb r6+0, r5
	addi r4, r4, 1
	bltu r4, r1, .LBB3_17
	jal r0, .LBB3_35
.LBB3_18:
	addi r1, r0, 10
	bne r3, r1, .LBB3_104
.LBB3_19:
	ldw r4, r16+0
	bgt r4, r24, .LBB3_36
.LBB3_20:
	ldw r1, r11+528
	add r1, r15, r1
	addi r27, r0, 0
	stb r1+0, r27
	add r3, r15, r0
	jal r31, strlen
	ldw r3, fp+-96
	sltu r3, r1, r3
	sub r3, r27, r3
	xori r1, r1, 511
	and r1, r1, r3
	xori r1, r1, 511
	beq r1, r27, .LBB3_35
.LBB3_21:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r4, r27, r0
.LBB3_22:
	add r5, r15, r4
	ldbu r5, r5+0
	add r6, r3, r4
	stb r6+0, r5
	addi r4, r4, 1
	bltu r4, r1, .LBB3_22
	jal r0, .LBB3_35
.LBB3_23:
	addi r1, r3, -10
	ldw r3, fp+-88
	bgtu r1, r3, .LBB3_104
.LBB3_24:
	slli r1, r1, 2
	ldw r3, fp+-108
	add r1, r3, r1
	ldw r3, r1+0
	ldw r1, fp+-84
	jalr r0, r3, 0
.LBB3_25:
	ldw r4, r16+0
	bgt r4, r24, .LBB3_36
.LBB3_26:
	ldw r1, r11+528
	add r1, r15, r1
	addi r27, r0, 0
	stb r1+0, r27
	add r3, r15, r0
	jal r31, strlen
	ldw r3, fp+-96
	sltu r3, r1, r3
	sub r3, r27, r3
	xori r1, r1, 511
	and r1, r1, r3
	xori r1, r1, 511
	beq r1, r27, .LBB3_35
.LBB3_27:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r4, r27, r0
.LBB3_28:
	add r5, r15, r4
	ldbu r5, r5+0
	add r6, r3, r4
	stb r6+0, r5
	addi r4, r4, 1
	bltu r4, r1, .LBB3_28
	jal r0, .LBB3_35
.LBB3_29:
	bgtu r3, r28, .LBB3_66
.LBB3_30:
	slli r1, r3, 2
	lui r4, %hi(.LJTI3_2)
	addi r4, r4, %lo(.LJTI3_2)
	add r1, r4, r1
	ldw r4, r1+0
	ldw r1, fp+-84
	jalr r0, r4, 0
.LBB3_31:
	ldw r4, r16+0
	bgt r4, r24, .LBB3_36
.LBB3_32:
	ldw r1, r11+528
	add r1, r15, r1
	addi r27, r0, 0
	stb r1+0, r27
	add r3, r15, r0
	jal r31, strlen
	ldw r3, fp+-96
	sltu r3, r1, r3
	sub r3, r27, r3
	xori r1, r1, 511
	and r1, r1, r3
	xori r1, r1, 511
	beq r1, r27, .LBB3_35
.LBB3_33:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r4, r27, r0
.LBB3_34:
	add r5, r15, r4
	ldbu r5, r5+0
	add r6, r3, r4
	stb r6+0, r5
	addi r4, r4, 1
	bltu r4, r1, .LBB3_34
.LBB3_35:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r1, r3, r1
	stb r1+0, r27
	ldw r1, r16+0
	addi r4, r1, 1
	stw r16+0, r4
.LBB3_36:
	addi r27, r0, 0
	stw r11+528, r27
	addi r1, r0, 6
	stw r23+0, r1
	ldw r1, r19+0
	bne r1, r27, .LBB3_10
.LBB3_37:
	stw r20+0, r4
	stw r19+0, r18
	ldw r3, r11+12
	beq r12, r27, .LBB3_4
.LBB3_38:
	add r5, r14, r0
	add r6, r13, r0
	jalr lr, r12, 0
.LBB3_39:
	bgtu r3, r28, .LBB3_48
.LBB3_40:
	slli r1, r3, 2
	ldw r4, fp+-116
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB3_41:
	ldw r1, fp+-80
	stw r23+0, r1
	ldw r1, r19+0
	ldw r4, r16+0
	beq r1, r17, .LBB3_90
.LBB3_42:
	ldw r1, r20+0
	bne r4, r1, .LBB3_104
.LBB3_43:
	ldw r3, r11+12
	bne r12, r17, .LBB3_91
	jal r0, .LBB3_93
.LBB3_44:
	beq r3, r17, .LBB3_54
.LBB3_45:
	ldw r1, fp+-88
	bne r3, r1, .LBB3_52
.LBB3_46:
	ldw r1, fp+-104
	stw r21+0, r1
	jal r0, .LBB3_5
.LBB3_47:
	addi r1, r0, 6
	jal r0, .LBB3_76
.LBB3_48:
	ldw r1, r11+528
	ldw r4, fp+-100
	bgt r1, r4, .LBB3_50
.LBB3_49:
	addi r4, r1, 1
	stw r11+528, r4
	add r1, r15, r1
	stb r1+0, r3
.LBB3_50:
	stw r23+0, r18
	ldw r1, fp+-92
	stw r21+0, r1
	jal r0, .LBB3_5
.LBB3_51:
	stw r21+0, r18
	jal r0, .LBB3_5
.LBB3_52:
	ldw r1, r11+528
	ldw r4, fp+-100
	bgt r1, r4, .LBB3_54
.LBB3_53:
	addi r4, r1, 1
	stw r11+528, r4
	add r1, r15, r1
	stb r1+0, r3
.LBB3_54:
	ldw r1, fp+-104
	stw r23+0, r1
	jal r0, .LBB3_5
.LBB3_55:
	ldw r1, r16+0
	bgt r1, r24, .LBB3_82
.LBB3_56:
	ldw r1, r11+528
	add r1, r15, r1
	addi r27, r0, 0
	stb r1+0, r27
	add r3, r15, r0
	jal r31, strlen
	ldw r3, fp+-96
	sltu r3, r1, r3
	sub r3, r27, r3
	xori r1, r1, 511
	and r1, r1, r3
	xori r1, r1, 511
	beq r1, r27, .LBB3_81
.LBB3_57:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r4, r27, r0
.LBB3_58:
	add r5, r15, r4
	ldbu r5, r5+0
	add r6, r3, r4
	stb r6+0, r5
	addi r4, r4, 1
	bltu r4, r1, .LBB3_58
	jal r0, .LBB3_81
.LBB3_59:
	ldw r1, r11+528
	ldw r3, fp+-100
	bgt r1, r3, .LBB3_61
.LBB3_60:
	addi r3, r1, 1
	stw r11+528, r3
	add r1, r15, r1
	ldw r3, fp+-88
	stb r1+0, r3
.LBB3_61:
	stw r23+0, r25
	stw r21+0, r18
	jal r0, .LBB3_5
.LBB3_62:
	ldw r1, r16+0
	bgt r1, r24, .LBB3_82
.LBB3_63:
	ldw r1, r11+528
	add r1, r15, r1
	addi r27, r0, 0
	stb r1+0, r27
	add r3, r15, r0
	jal r31, strlen
	ldw r3, fp+-96
	sltu r3, r1, r3
	sub r3, r27, r3
	xori r1, r1, 511
	and r1, r1, r3
	xori r1, r1, 511
	beq r1, r27, .LBB3_81
.LBB3_64:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r4, r27, r0
.LBB3_65:
	add r5, r15, r4
	ldbu r5, r5+0
	add r6, r3, r4
	stb r6+0, r5
	addi r4, r4, 1
	bltu r4, r1, .LBB3_65
	jal r0, .LBB3_81
.LBB3_66:
	ldw r1, r11+528
	ldw r4, fp+-100
	bgt r1, r4, .LBB3_68
.LBB3_67:
	addi r4, r1, 1
	stw r11+528, r4
	add r1, r15, r1
	stb r1+0, r3
.LBB3_68:
	stw r23+0, r18
	jal r0, .LBB3_5
.LBB3_69:
	ldw r1, r16+0
	bgt r1, r24, .LBB3_74
.LBB3_70:
	ldw r1, r11+528
	add r1, r15, r1
	addi r27, r0, 0
	stb r1+0, r27
	add r3, r15, r0
	jal r31, strlen
	ldw r3, fp+-96
	sltu r3, r1, r3
	sub r3, r27, r3
	xori r1, r1, 511
	and r1, r1, r3
	xori r1, r1, 511
	beq r1, r27, .LBB3_73
.LBB3_71:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r4, r27, r0
.LBB3_72:
	add r5, r15, r4
	ldbu r5, r5+0
	add r6, r3, r4
	stb r6+0, r5
	addi r4, r4, 1
	bltu r4, r1, .LBB3_72
.LBB3_73:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r1, r3, r1
	stb r1+0, r27
	ldw r1, r16+0
	addi r1, r1, 1
	stw r16+0, r1
.LBB3_74:
	addi r1, r0, 0
	stw r11+528, r1
	ldw r1, fp+-112
	stw r23+0, r1
	stw r21+0, r25
	jal r0, .LBB3_5
.LBB3_75:
	ldw r1, fp+-80
.LBB3_76:
	stw r21+0, r1
	ldw r1, fp+-120
	stw r23+0, r1
	jal r0, .LBB3_5
.LBB3_77:
	ldw r1, r16+0
	bgt r1, r24, .LBB3_82
.LBB3_78:
	ldw r1, r11+528
	add r1, r15, r1
	addi r27, r0, 0
	stb r1+0, r27
	add r3, r15, r0
	jal r31, strlen
	ldw r3, fp+-96
	sltu r3, r1, r3
	sub r3, r27, r3
	xori r1, r1, 511
	and r1, r1, r3
	xori r1, r1, 511
	beq r1, r27, .LBB3_81
.LBB3_79:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r4, r27, r0
.LBB3_80:
	add r5, r15, r4
	ldbu r5, r5+0
	add r6, r3, r4
	stb r6+0, r5
	addi r4, r4, 1
	bltu r4, r1, .LBB3_80
.LBB3_81:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r1, r3, r1
	stb r1+0, r27
	ldw r1, r16+0
	addi r1, r1, 1
	stw r16+0, r1
.LBB3_82:
	addi r1, r0, 0
	stw r11+528, r1
	jal r0, .LBB3_89
.LBB3_83:
	ldw r1, r16+0
	bgt r1, r24, .LBB3_88
.LBB3_84:
	ldw r1, r11+528
	add r1, r15, r1
	stb r1+0, r17
	add r3, r15, r0
	jal r31, strlen
	ldw r3, fp+-96
	sltu r3, r1, r3
	sub r3, r17, r3
	xori r1, r1, 511
	and r1, r1, r3
	xori r1, r1, 511
	beq r1, r17, .LBB3_87
.LBB3_85:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r4, r17, r0
.LBB3_86:
	add r5, r15, r4
	ldbu r5, r5+0
	add r6, r3, r4
	stb r6+0, r5
	addi r4, r4, 1
	bltu r4, r1, .LBB3_86
.LBB3_87:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r1, r3, r1
	stb r1+0, r17
	ldw r1, r16+0
	addi r1, r1, 1
	stw r16+0, r1
.LBB3_88:
	stw r11+528, r17
.LBB3_89:
	ldw r1, fp+-92
	stw r23+0, r1
	ldw r1, fp+-112
	stw r21+0, r1
	jal r0, .LBB3_5
.LBB3_90:
	stw r20+0, r4
	stw r19+0, r18
	ldw r3, r11+12
	beq r12, r17, .LBB3_93
.LBB3_91:
	add r5, r14, r0
	add r6, r13, r0
	jalr lr, r12, 0
.LBB3_92:
	ldw r3, r11+12
.LBB3_93:
	stw r16+0, r17
	stw r11+528, r17
	addi r1, r3, 1
	stw r11+12, r1
	jal r0, .LBB3_5
.LBB3_94:
	ldw r1, r21+0
	addi r3, r1, -2
	ldw r4, fp+-92
	bgeu r3, r4, .LBB3_103
.LBB3_95:
	ldw r4, r16+0
	bgt r4, r24, .LBB3_100
.LBB3_96:
	ldw r1, r11+528
	add r1, r15, r1
	addi r17, r0, 0
	stb r1+0, r17
	add r3, r15, r0
	jal r31, strlen
	ldw r3, fp+-96
	sltu r3, r1, r3
	sub r3, r17, r3
	xori r1, r1, 511
	and r1, r1, r3
	xori r1, r1, 511
	beq r1, r17, .LBB3_99
.LBB3_97:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r4, r17, r0
.LBB3_98:
	add r5, r15, r4
	ldbu r5, r5+0
	add r6, r3, r4
	stb r6+0, r5
	addi r4, r4, 1
	bltu r4, r1, .LBB3_98
.LBB3_99:
	ldw r3, r16+0
	slli r3, r3, 9
	add r3, r14, r3
	add r1, r3, r1
	stb r1+0, r17
	ldw r1, r16+0
	addi r4, r1, 1
	stw r16+0, r4
.LBB3_100:
	addi r15, r0, 0
	stw r11+528, r15
	ldw r1, r19+0
	beq r1, r15, .LBB3_106
.LBB3_101:
	ldw r3, r20+0
	ldw r1, fp+-84
	bne r4, r3, .LBB3_105
.LBB3_102:
	bne r12, r15, .LBB3_107
	jal r0, .LBB3_108
.LBB3_103:
	addi r3, r0, 0
	beq r1, r3, .LBB3_105
.LBB3_104:
	ldw r1, fp+-84
.LBB3_105:
	ldw lr, fp+-76
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
	addi sp, sp, 136
	jalr r0, r31, 0
.LBB3_106:
	stw r20+0, r4
	stw r19+0, r18
	beq r12, r15, .LBB3_108
.LBB3_107:
	ldw r3, r11+12
	add r5, r14, r0
	add r6, r13, r0
	jalr lr, r12, 0
.LBB3_108:
	stw r16+0, r15
	stw r11+528, r15
	ldw r1, r11+12
	addi r1, r1, 1
	stw r11+12, r1
	add r1, r15, r0
	jal r0, .LBB3_105
.Lfunc_end3:
	.size	csv_parse, .Lfunc_end3-csv_parse
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI3_0:
	.word	.LBB3_39
	.word	.LBB3_44
	.word	.LBB3_23
	.word	.LBB3_29
	.word	.LBB3_12
	.word	.LBB3_8
	.word	.LBB3_18
	.word	.LBB3_8
.LJTI3_1:
	.word	.LBB3_50
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_14
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_69
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_51
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_77
.LJTI3_2:
	.word	.LBB3_68
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_31
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_47
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_105
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_66
	.word	.LBB3_62
.LJTI3_3:
	.word	.LBB3_25
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_47
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_59
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_105
	.word	.LBB3_55
.LJTI3_4:
	.word	.LBB3_50
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_41
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_75
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_51
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_48
	.word	.LBB3_83
                                        # -- End function
	.text
	.globl	csv_parse_file                  # -- Begin function csv_parse_file
	.p2align	2
	.type	csv_parse_file,@function
csv_parse_file:                         # @csv_parse_file
# %bb.0:
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -584
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 592
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, lr
	add r12, r5, r0
	add r13, r4, r0
	add r4, r3, r0
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r11, r1, -572
	add r3, r11, r0
	jal r31, csv_open
	addi r14, r0, 0
	beq r1, r14, .LBB4_2
.LBB4_1:
	addi r1, r0, -1
	jal r0, .LBB4_4
.LBB4_2:
	add r3, r11, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, csv_parse
	ldw r3, r11+0
	beq r3, r14, .LBB4_4
.LBB4_3:
	add r11, r1, r0
	jal r31, free
	add r1, r11, r0
.LBB4_4:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 592
	jalr r0, r31, 0
.Lfunc_end4:
	.size	csv_parse_file, .Lfunc_end4-csv_parse_file
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"rb"
	.size	.L.str, 3

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0e4c326a930b0c6d54608d411469f48934c05b80)"
	.section	".note.GNU-stack","",@progbits
