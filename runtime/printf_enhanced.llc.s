	.file	"printf_enhanced.c"
	.text
	.globl	vsnprintf_enhanced              # -- Begin function vsnprintf_enhanced
	.p2align	2
	.type	vsnprintf_enhanced,@function
vsnprintf_enhanced:                     # @vsnprintf_enhanced
# %bb.0:
	addi sp, sp, -200
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 200
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
	addi r1, fp, -80
	stw fp+-124, r1
	stw r1+0, r6
	addi r13, r0, 0
	bne r3, r13, .LBB0_1
	jal r0, .LBB0_226
.LBB0_1:
	add r12, r5, r0
	addi r5, r0, 0
	bne r4, r5, .LBB0_2
	jal r0, .LBB0_226
.LBB0_2:
	add r23, r3, r0
	addi r18, fp, -116
	ori  r1, r18, 1
	stw fp+-144, r1
	addi r1, r4, -1
	stw fp+-120, r1
	addi r1, fp, -112
	ori  r3, r1, 1
	stw fp+-172, r3
	stw fp+-180, r1
	addi r1, r1, -1
	stw fp+-156, r1
	addi r1, r0, -2
	stw fp+-176, r1
	addi r7, r0, 37
	addi r27, r0, 1
	addi r25, r0, 45
	add r13, r5, r0
	stw fp+-132, r23
	ldw r1, fp+-124
	stw fp+-136, r5
	stw fp+-128, r18
	stw fp+-140, r7
	ldbu r1, r12+0
	beq r1, r7, .LBB0_9
.LBB0_3:
	addi r3, r0, 0
	bne r1, r3, .LBB0_4
	jal r0, .LBB0_225
.LBB0_4:
	ldw r3, fp+-120
	bgeu r13, r3, .LBB0_8
.LBB0_5:
	addi r3, r13, 1
	add r4, r23, r13
	stb r4+0, r1
	add r13, r3, r0
	jal r0, .LBB0_8
.LBB0_6:
	add r13, r14, r0
.LBB0_7:
	ldw r5, fp+-136
	ldw r18, fp+-128
	ldw r7, fp+-140
.LBB0_8:
	addi r12, r12, 1
	ldbu r1, r12+0
	bne r1, r7, .LBB0_3
.LBB0_9:
	add r21, r5, r0
	add r9, r5, r0
	add r8, r5, r0
	add r10, r5, r0
	add r3, r5, r0
	addi r1, r12, 1
	jal r0, .LBB0_12
.LBB0_10:
	addi r12, r1, -1
	addi r8, r0, 1
.LBB0_11:
	add r3, r24, r0
	addi r1, r12, 1
.LBB0_12:
	add r24, r3, r0
	ldbu r4, r1+0
	addi r1, r1, 1
	add r3, r27, r0
	beq r4, r25, .LBB0_12
.LBB0_13:
	addi r3, r4, -32
	addi r4, r0, 16
	bgtu r3, r4, .LBB0_19
.LBB0_14:
	slli r3, r3, 2
	lui r4, %hi(.LJTI0_0)
	addi r4, r4, %lo(.LJTI0_0)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB0_15:
	addi r12, r1, -1
	addi r9, r0, 1
	jal r0, .LBB0_11
.LBB0_16:
	addi r12, r1, -1
	addi r21, r0, 1
	jal r0, .LBB0_11
.LBB0_17:
	addi r1, r1, -1
	addi r10, r0, 1
	addi r1, r1, 1
	add r3, r24, r0
	jal r0, .LBB0_12
.LBB0_18:
	ldw r5, fp+-124
	ldw r3, r5+0
	ldw r4, r3+0
	addi r3, r3, 4
	stw r5+0, r3
	ori r3, r0, 0
	slt r3, r4, r3
	srai r5, r4, 31
	xor r4, r4, r5
	sub r26, r4, r5
	addi r4, r0, 0
	sub r3, r4, r3
	xori r4, r24, 1
	and r3, r4, r3
	xor r24, r24, r3
	jal r0, .LBB0_22
.LBB0_19:
	addi r3, r1, -1
	ldbu r4, r1+-1
	addi r1, r4, -48
	andi r1, r1, 255
	addi lr, r0, 0
	addi r5, r0, 9
	bgtu r1, r5, .LBB0_23
.LBB0_20:
	addi r26, r0, 0
.LBB0_21:
	addi r5, r0, 10
	mul r1, r26, r5
	addi r4, r4, -48
	andi r4, r4, 255
	add r26, r1, r4
	addi r1, r3, 1
	ldbu r4, r3+1
	addi r3, r4, -48
	andi r6, r3, 255
	add r3, r1, r0
	bltu r6, r5, .LBB0_21
.LBB0_22:
	addi lr, r0, 1
	jal r0, .LBB0_24
.LBB0_23:
	add r26, lr, r0
	add r1, r3, r0
.LBB0_24:
	ldbu r4, r1+0
	addi r3, r0, 0
	addi r5, r0, 46
	bne r4, r5, .LBB0_27
.LBB0_25:
	ldbu r4, r1+1
	addi r5, r0, 42
	bne r4, r5, .LBB0_34
.LBB0_26:
	ldw r6, fp+-124
	ldw r4, r6+0
	ldw r5, r4+0
	addi r4, r4, 4
	stw r6+0, r4
	ori r4, r0, -1
	sgt r16, r5, r4
	addi r4, r0, 0
	sub r4, r4, r16
	and r17, r5, r4
	addi r12, r1, 2
	jal r0, .LBB0_28
.LBB0_27:
	add r16, r3, r0
	add r17, r3, r0
	add r12, r1, r0
.LBB0_28:
	ldbu r1, r12+0
	addi r4, r0, 104
	beq r1, r4, .LBB0_32
.LBB0_29:
	addi r4, r0, 108
	bne r1, r4, .LBB0_33
.LBB0_30:
	ldbu r1, r12+1
	bne r1, r4, .LBB0_38
.LBB0_31:
	addi r12, r12, 2
	addi r4, r0, 1
	add r6, r3, r0
	jal r0, .LBB0_39
.LBB0_32:
	addi r1, r12, 1
	ldbu r3, r12+1
	ori r4, r0, 104
	seq r3, r3, r4
	addi r4, r12, 2
	xor r5, r4, r1
	addi r4, r0, 0
	sub r3, r4, r3
	and r3, r5, r3
	xor r12, r1, r3
	add r6, r4, r0
	jal r0, .LBB0_39
.LBB0_33:
	add r4, r3, r0
	add r6, r3, r0
	jal r0, .LBB0_39
.LBB0_34:
	addi r12, r1, 1
	ldbu r4, r12+0
	addi r5, r4, -48
	andi r5, r5, 255
	addi r16, r0, 1
	addi r6, r0, 9
	bgtu r5, r6, .LBB0_51
.LBB0_35:
	addi r1, r1, 2
	addi r17, r0, 0
.LBB0_36:
	addi r5, r0, 10
	mul r6, r17, r5
	addi r4, r4, -48
	andi r4, r4, 255
	add r17, r6, r4
	ldbu r4, r1+0
	addi r6, r4, -48
	andi r6, r6, 255
	addi r1, r1, 1
	bltu r6, r5, .LBB0_36
.LBB0_37:
	addi r12, r1, -1
	jal r0, .LBB0_28
.LBB0_38:
	addi r12, r12, 1
	addi r6, r0, 1
	add r4, r3, r0
.LBB0_39:
	ldbu r28, r12+0
	addi r1, r28, -37
	addi r3, r0, 83
	bgtu r1, r3, .LBB0_60
.LBB0_40:
	slli r1, r1, 2
	lui r3, %hi(.LJTI0_1)
	addi r3, r3, %lo(.LJTI0_1)
	add r1, r3, r1
	ldw r3, r1+0
	lui r22, %hi(Digits100)
	addi r22, r22, %lo(Digits100)
	lui r11, %hi(Digits100+1)
	addi r11, r11, %lo(Digits100+1)
	lui r19, 2
	lui r1, %hi(Digits16L)
	addi r1, r1, %lo(Digits16L)
	jalr r0, r3, 0
.LBB0_41:
	ori r3, r0, 88
	seq r3, r28, r3
	lui r5, %hi(Digits16U)
	addi r5, r5, %lo(Digits16U)
	xor r5, r5, r1
	addi r16, r0, 0
	sub r3, r16, r3
	and r3, r5, r3
	xor r1, r3, r1
	beq r4, r16, .LBB0_53
.LBB0_42:
	ldw r7, fp+-124
	ldw r3, r7+0
	ldw r5, r3+0
	addi r4, r3, 4
	stw r7+0, r4
	ldw r6, r3+4
	addi r3, r3, 8
	stw r7+0, r3
	or  r3, r5, r6
	addi r4, r0, 0
	beq r3, r4, .LBB0_127
.LBB0_43:
	add r22, r8, r0
	add r19, r9, r0
	add r17, r10, r0
	ldw r7, fp+-156
	add r16, r4, r0
.LBB0_44:
	add r3, r7, r0
	addi r15, fp, -112
	add r7, r15, r16
	andi r8, r5, 15
	add r8, r1, r8
	ldbu r8, r8+0
	stb r7+0, r8
	srli r7, r5, 4
	slli r8, r6, 28
	or  r8, r7, r8
	srli r9, r6, 4
	ori r7, r0, 0
	sne r10, r6, r7
	addi r11, r0, 15
	sgtu r5, r5, r11
	xor r5, r5, r10
	seq r6, r6, r7
	sub r6, r4, r6
	and r5, r5, r6
	xor r10, r10, r5
	addi r16, r16, 1
	addi r7, r3, 1
	add r5, r8, r0
	add r6, r9, r0
	bne r10, r4, .LBB0_44
.LBB0_45:
	add r1, r15, r16
	addi r14, r0, 0
	stb r1+0, r14
	addi r1, r1, -1
	bgeu r15, r1, .LBB0_114
.LBB0_46:
	add r1, r15, r0
	add r10, r17, r0
	add r9, r19, r0
	add r8, r22, r0
.LBB0_47:
	add r4, r3, r0
	ldbu r3, r1+0
	ldbu r5, r4+1
	stb r1+0, r5
	stb r4+1, r3
	addi r1, r1, 1
	addi r3, r4, -1
	bltu r1, r4, .LBB0_47
	jal r0, .LBB0_176
.LBB0_48:
	addi r14, r0, 0
	beq r4, r14, .LBB0_73
.LBB0_49:
	ldw r4, fp+-124
	ldw r1, r4+0
	ldw r15, r1+0
	addi r3, r1, 4
	stw r4+0, r3
	ldw r23, r1+4
	addi r1, r1, 8
	stw r4+0, r1
	addi r1, r0, -1
	stw fp+-160, r21
	ble r23, r1, .LBB0_107
.LBB0_50:
	addi r20, fp, -112
	add r16, r23, r0
	jal r0, .LBB0_108
.LBB0_51:
	add r17, r3, r0
	jal r0, .LBB0_28
.LBB0_52:
	addi r15, fp, -112
	sth r15+0, r7
	jal r0, .LBB0_128
.LBB0_53:
	ldw r5, fp+-124
	ldw r4, r5+0
	ldw r3, r4+0
	addi r4, r4, 4
	stw r5+0, r4
	beq r6, r16, .LBB0_83
.LBB0_54:
	addi r16, r0, 0
	beq r3, r16, .LBB0_127
.LBB0_55:
	ldw r5, fp+-156
.LBB0_56:
	add r6, r3, r0
	add r4, r5, r0
	addi r15, fp, -112
	add r3, r15, r16
	andi r5, r6, 15
	add r5, r1, r5
	ldbu r5, r5+0
	stb r3+0, r5
	srli r3, r6, 4
	addi r16, r16, 1
	addi r5, r4, 1
	addi r7, r0, 15
	bgtu r6, r7, .LBB0_56
.LBB0_57:
	add r1, r15, r16
	addi r14, r0, 0
	stb r1+0, r14
	addi r1, r1, -1
	bgeu r15, r1, .LBB0_176
.LBB0_58:
	add r1, r15, r0
.LBB0_59:
	add r3, r4, r0
	ldbu r4, r1+0
	ldbu r5, r3+1
	stb r1+0, r5
	stb r3+1, r4
	addi r1, r1, 1
	addi r4, r3, -1
	bltu r1, r3, .LBB0_59
	jal r0, .LBB0_176
.LBB0_60:
	addi r15, fp, -112
	stb r15+0, r7
	ori  r1, r15, 1
	stb r1+0, r28
	ori  r1, r15, 2
	addi r14, r0, 0
	stb r1+0, r14
	addi r16, r0, 2
	jal r0, .LBB0_176
.LBB0_61:
	addi r16, r0, 0
	beq r4, r16, .LBB0_89
.LBB0_62:
	ldw r6, fp+-124
	ldw r1, r6+0
	ldw r4, r1+0
	addi r3, r1, 4
	stw r6+0, r3
	ldw r5, r1+4
	addi r1, r1, 8
	stw r6+0, r1
	or  r1, r4, r5
	addi r3, r0, 0
	beq r1, r3, .LBB0_127
.LBB0_63:
	add r19, r8, r0
	add r17, r9, r0
	add r11, r10, r0
	ldw r6, fp+-156
	add r16, r3, r0
.LBB0_64:
	add r1, r6, r0
	addi r15, fp, -112
	add r6, r15, r16
	andi r7, r4, 7
	ori  r7, r7, 48
	stb r6+0, r7
	srli r6, r4, 3
	slli r7, r5, 29
	or  r7, r6, r7
	srli r8, r5, 3
	ori r6, r0, 0
	sne r9, r5, r6
	addi r10, r0, 7
	sgtu r4, r4, r10
	xor r4, r4, r9
	seq r5, r5, r6
	sub r5, r3, r5
	and r4, r4, r5
	xor r9, r9, r4
	addi r16, r16, 1
	addi r6, r1, 1
	add r4, r7, r0
	add r5, r8, r0
	bne r9, r3, .LBB0_64
.LBB0_65:
	add r3, r15, r16
	addi r14, r0, 0
	stb r3+0, r14
	addi r3, r3, -1
	bgeu r15, r3, .LBB0_147
.LBB0_66:
	add r3, r15, r0
	add r10, r11, r0
	add r9, r17, r0
	add r8, r19, r0
.LBB0_67:
	add r4, r1, r0
	ldbu r1, r3+0
	ldbu r5, r4+1
	stb r3+0, r5
	stb r4+1, r1
	addi r3, r3, 1
	addi r1, r4, -1
	bltu r3, r4, .LBB0_67
	jal r0, .LBB0_176
.LBB0_68:
	ldw r4, fp+-124
	ldw r3, r4+0
	ldw r5, r3+0
	addi r3, r3, 4
	stw r4+0, r3
	addi r16, r0, 0
	beq r5, r16, .LBB0_105
.LBB0_69:
	add r10, r8, r0
	ldw r6, fp+-156
.LBB0_70:
	add r7, r5, r0
	add r4, r6, r0
	addi r3, fp, -112
	add r5, r3, r16
	andi r6, r7, 15
	add r6, r6, r1
	ldbu r6, r6+0
	stb r5+0, r6
	srli r5, r7, 4
	addi r16, r16, 1
	addi r6, r4, 1
	addi r8, r0, 15
	bgtu r7, r8, .LBB0_70
.LBB0_71:
	add r1, r3, r16
	addi r5, r0, 0
	stb r1+0, r5
	add r8, r10, r0
	addi r1, r1, -1
	bgeu r3, r1, .LBB0_106
.LBB0_72:
	add r1, r4, r0
	ldbu r4, r3+0
	ldbu r5, r1+1
	stb r3+0, r5
	stb r1+1, r4
	addi r3, r3, 1
	addi r4, r1, -1
	bltu r3, r1, .LBB0_72
	jal r0, .LBB0_106
.LBB0_73:
	ldw r3, fp+-124
	ldw r1, r3+0
	ldw r4, r1+0
	addi r1, r1, 4
	stw r3+0, r1
	srli r1, r4, 31
	addi r3, r0, -1
	beq r6, r14, .LBB0_96
.LBB0_74:
	ble r4, r3, .LBB0_136
.LBB0_75:
	addi r3, fp, -112
	addi r14, r0, 0
	bne r4, r14, .LBB0_137
	jal r0, .LBB0_146
.LBB0_76:
	ldw r4, fp+-124
	ldw r1, r4+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r4+0, r1
	addi r15, fp, -112
	stb r15+0, r3
	ori  r1, r15, 1
	addi r14, r0, 0
	stb r1+0, r14
	addi r16, r0, 1
	jal r0, .LBB0_176
.LBB0_77:
	addi r14, r0, 0
	beq r4, r14, .LBB0_98
.LBB0_78:
	ldw r5, fp+-124
	ldw r3, r5+0
	ldw r1, r3+0
	addi r4, r3, 4
	stw r5+0, r4
	ldw r4, r3+4
	addi r3, r3, 8
	stw r5+0, r3
	or  r3, r1, r4
	addi r14, r0, 0
	beq r3, r14, .LBB0_135
.LBB0_79:
	stw fp+-148, r8
	stw fp+-152, r9
	stw fp+-164, r10
	stw fp+-160, r21
	stw fp+-168, lr
	addi r15, r0, 100
	sltu r3, r1, r15
	ori r21, r0, 0
	seq r5, r4, r21
	sub r5, r14, r5
	and r3, r3, r5
	bne r3, r14, .LBB0_158
.LBB0_80:
	addi r20, fp, -112
	add r16, r1, r0
	add r17, r4, r0
.LBB0_81:
	addi r18, r0, 0
	add r3, r16, r0
	add r4, r17, r0
	add r5, r15, r0
	add r6, r18, r0
	jal r31, __udivdi3
	mul r3, r1, r15
	sub r3, r16, r3
	slli r3, r3, 1
	add r4, r3, r22
	add r3, r3, r11
	ldbu r3, r3+0
	stb r20+0, r3
	ldbu r4, r4+0
	addi r3, r20, 2
	stb r20+1, r4
	sne r4, r17, r21
	addi r5, r19, 1807
	sgtu r5, r16, r5
	xor r5, r5, r4
	seq r6, r17, r21
	sub r6, r18, r6
	and r5, r5, r6
	xor r5, r4, r5
	add r16, r1, r0
	add r17, r2, r0
	add r20, r3, r0
	add r4, r2, r0
	bne r5, r18, .LBB0_81
	jal r0, .LBB0_159
.LBB0_82:
	ldw r4, fp+-124
	ldw r1, r4+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r4+0, r1
	ori r1, r0, 0
	seq r1, r3, r1
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	xor r4, r3, r4
	addi r14, r0, 0
	sub r1, r14, r1
	and r1, r4, r1
	xor r15, r3, r1
	add r3, r15, r0
	add r11, lr, r0
	add r19, r10, r0
	add r22, r9, r0
	add r20, r21, r0
	add r21, r8, r0
	jal r31, strlen
	add r8, r21, r0
	add r21, r20, r0
	add r9, r22, r0
	add r10, r19, r0
	add lr, r11, r0
	xor r3, r17, r1
	sltu r4, r17, r1
	sub r4, r14, r4
	and r3, r3, r4
	sub r4, r14, r16
	and r3, r3, r4
	xor r16, r1, r3
	jal r0, .LBB0_176
.LBB0_83:
	beq r3, r16, .LBB0_127
.LBB0_84:
	ldw r5, fp+-156
.LBB0_85:
	add r6, r3, r0
	add r4, r5, r0
	addi r15, fp, -112
	add r3, r15, r16
	andi r5, r6, 15
	add r5, r1, r5
	ldbu r5, r5+0
	stb r3+0, r5
	srli r3, r6, 4
	addi r16, r16, 1
	addi r5, r4, 1
	addi r7, r0, 15
	bgtu r6, r7, .LBB0_85
.LBB0_86:
	add r1, r15, r16
	addi r14, r0, 0
	stb r1+0, r14
	addi r1, r1, -1
	bgeu r15, r1, .LBB0_176
.LBB0_87:
	add r1, r15, r0
.LBB0_88:
	add r3, r4, r0
	ldbu r4, r1+0
	ldbu r5, r3+1
	stb r1+0, r5
	stb r3+1, r4
	addi r1, r1, 1
	addi r4, r3, -1
	bltu r1, r3, .LBB0_88
	jal r0, .LBB0_176
.LBB0_89:
	ldw r4, fp+-124
	ldw r3, r4+0
	ldw r1, r3+0
	addi r3, r3, 4
	stw r4+0, r3
	beq r6, r16, .LBB0_121
.LBB0_90:
	addi r16, r0, 0
	beq r1, r16, .LBB0_127
.LBB0_91:
	ldw r4, fp+-156
.LBB0_92:
	add r5, r1, r0
	add r3, r4, r0
	addi r15, fp, -112
	add r1, r15, r16
	andi r4, r5, 7
	ori  r4, r4, 48
	stb r1+0, r4
	srli r1, r5, 3
	addi r16, r16, 1
	addi r4, r3, 1
	addi r6, r0, 7
	bgtu r5, r6, .LBB0_92
.LBB0_93:
	add r1, r15, r16
	addi r14, r0, 0
	stb r1+0, r14
	addi r1, r1, -1
	bgeu r15, r1, .LBB0_176
.LBB0_94:
	add r1, r15, r0
.LBB0_95:
	add r4, r3, r0
	ldbu r3, r1+0
	ldbu r5, r4+1
	stb r1+0, r5
	stb r4+1, r3
	addi r1, r1, 1
	addi r3, r4, -1
	bltu r1, r4, .LBB0_95
	jal r0, .LBB0_176
.LBB0_96:
	ble r4, r3, .LBB0_141
.LBB0_97:
	addi r3, fp, -112
	bne r4, r14, .LBB0_142
	jal r0, .LBB0_146
.LBB0_98:
	ldw r3, fp+-124
	ldw r1, r3+0
	ldw r4, r1+0
	addi r1, r1, 4
	stw r3+0, r1
	lui r1, 8
	lui r3, 16
	lui r5, 5
	beq r6, r14, .LBB0_129
.LBB0_99:
	addi r14, r0, 0
	beq r4, r14, .LBB0_135
.LBB0_100:
	stw fp+-148, r8
	stw fp+-152, r9
	stw fp+-164, r10
	addi r6, r0, 100
	bltu r4, r6, .LBB0_165
.LBB0_101:
	addi r7, fp, -112
.LBB0_102:
	add r9, r4, r0
	srli r4, r4, 16
	addi r8, r1, 1311
	mul r10, r4, r8
	addi r15, r3, -1
	and r15, r9, r15
	addi r16, r5, 491
	mul r17, r15, r16
	add r10, r17, r10
	mul r8, r15, r8
	srli r8, r8, 16
	add r8, r10, r8
	srli r8, r8, 16
	mul r4, r4, r16
	add r4, r4, r8
	srli r4, r4, 5
	mul r8, r4, r6
	sub r8, r9, r8
	slli r8, r8, 1
	add r10, r8, r22
	add r8, r8, r11
	ldbu r8, r8+0
	stb r7+0, r8
	ldbu r10, r10+0
	addi r8, r7, 2
	stb r7+1, r10
	addi r10, r19, 1807
	add r7, r8, r0
	bgtu r9, r10, .LBB0_102
.LBB0_103:
	addi r1, r0, 10
	bltu r4, r1, .LBB0_166
.LBB0_104:
	slli r1, r4, 1
	add r3, r1, r22
	add r1, r1, r11
	ldbu r1, r1+0
	stb r8+0, r1
	ldbu r3, r3+0
	addi r1, r8, 2
	stb r8+1, r3
	jal r0, .LBB0_167
.LBB0_105:
	addi r1, r0, 48
	addi r3, fp, -112
	sth r3+0, r1
	addi r16, r0, 1
.LBB0_106:
	addi r1, r0, 9
	sgtu r1, r26, r1
	and r3, lr, r1
	addi r1, r0, 0
	sub r3, r1, r3
	xori r4, lr, 1
	and r4, r4, r3
	xori lr, r4, 1
	xori r4, r26, 10
	and r3, r4, r3
	xori r26, r3, 10
	addi r14, r0, 1
	addi r15, fp, -112
	add r10, r14, r0
	jal r0, .LBB0_177
.LBB0_107:
	addi r1, fp, -112
	stb r1+0, r25
	addi r1, r0, 0
	sub r3, r1, r15
	ori r4, r0, 0
	sne r4, r15, r4
	add r4, r23, r4
	sub r16, r1, r4
	ldw r20, fp+-172
	add r15, r3, r0
.LBB0_108:
	or  r1, r15, r16
	addi r14, r0, 0
	beq r1, r14, .LBB0_113
.LBB0_109:
	stw fp+-148, r8
	stw fp+-152, r9
	stw fp+-164, r10
	stw fp+-168, lr
	addi r17, r0, 100
	sltu r1, r15, r17
	ori r18, r0, 0
	seq r3, r16, r18
	sub r3, r14, r3
	and r1, r1, r3
	add r21, r20, r0
	bne r1, r14, .LBB0_111
.LBB0_110:
	add r3, r15, r0
	add r4, r16, r0
	add r5, r17, r0
	add r6, r14, r0
	jal r31, __udivdi3
	mul r3, r1, r17
	sub r3, r15, r3
	slli r3, r3, 1
	add r4, r3, r22
	add r3, r3, r11
	ldbu r3, r3+0
	stb r21+0, r3
	ldbu r3, r4+0
	addi r4, r21, 2
	stb r21+1, r3
	sne r3, r16, r18
	addi r5, r19, 1807
	sgtu r5, r15, r5
	xor r5, r5, r3
	seq r6, r16, r18
	sub r6, r14, r6
	and r5, r5, r6
	xor r3, r3, r5
	add r15, r1, r0
	add r16, r2, r0
	add r21, r4, r0
	bne r3, r14, .LBB0_110
.LBB0_111:
	addi r1, r0, 10
	sltu r1, r15, r1
	seq r3, r16, r18
	sub r3, r14, r3
	and r1, r1, r3
	bne r1, r14, .LBB0_115
.LBB0_112:
	slli r1, r15, 1
	add r3, r1, r22
	add r1, r1, r11
	ldbu r1, r1+0
	stb r21+0, r1
	ldbu r3, r3+0
	addi r1, r21, 2
	stb r21+1, r3
	jal r0, .LBB0_116
.LBB0_113:
	addi r1, r0, 48
	stb r20+0, r1
	addi r1, r0, 0
	stb r20+1, r1
	addi r3, r0, 1
	ldw r18, fp+-128
	ldw r21, fp+-160
	jal r0, .LBB0_120
.LBB0_114:
	add r1, r14, r0
	add r10, r17, r0
	add r9, r19, r0
	add r8, r22, r0
	jal r0, .LBB0_177
.LBB0_115:
	ori  r3, r15, 48
	addi r1, r21, 1
	stb r21+0, r3
.LBB0_116:
	ldw r18, fp+-128
	ldw lr, fp+-168
	ldw r10, fp+-164
	ldw r9, fp+-152
	ldw r8, fp+-148
	sub r3, r1, r20
	addi r4, r0, 2
	ldw r21, fp+-160
	blt r3, r4, .LBB0_119
.LBB0_117:
	add r4, r20, r3
	addi r5, r4, -2
	add r4, r20, r0
.LBB0_118:
	add r6, r5, r0
	ldbu r5, r4+0
	ldbu r7, r6+1
	stb r4+0, r7
	stb r6+1, r5
	addi r4, r4, 1
	addi r5, r6, -1
	bltu r4, r6, .LBB0_118
.LBB0_119:
	stb r1+0, r14
.LBB0_120:
	srli r1, r23, 31
	add r3, r20, r3
	addi r15, fp, -112
	sub r16, r3, r15
	ldw r3, fp+-124
	jal r0, .LBB0_177
.LBB0_121:
	beq r1, r16, .LBB0_127
.LBB0_122:
	ldw r4, fp+-156
.LBB0_123:
	add r5, r1, r0
	add r3, r4, r0
	addi r15, fp, -112
	add r1, r15, r16
	andi r4, r5, 7
	ori  r4, r4, 48
	stb r1+0, r4
	srli r1, r5, 3
	addi r16, r16, 1
	addi r4, r3, 1
	addi r6, r0, 7
	bgtu r5, r6, .LBB0_123
.LBB0_124:
	add r1, r15, r16
	addi r14, r0, 0
	stb r1+0, r14
	addi r1, r1, -1
	bgeu r15, r1, .LBB0_176
.LBB0_125:
	add r1, r15, r0
.LBB0_126:
	add r4, r3, r0
	ldbu r3, r1+0
	ldbu r5, r4+1
	stb r1+0, r5
	stb r4+1, r3
	addi r1, r1, 1
	addi r3, r4, -1
	bltu r1, r4, .LBB0_126
	jal r0, .LBB0_176
.LBB0_127:
	addi r1, r0, 48
	addi r15, fp, -112
	sth r15+0, r1
.LBB0_128:
	addi r16, r0, 1
	addi r14, r0, 0
	jal r0, .LBB0_176
.LBB0_129:
	beq r4, r14, .LBB0_135
.LBB0_130:
	stw fp+-148, r8
	stw fp+-152, r9
	stw fp+-164, r10
	addi r6, r0, 100
	bltu r4, r6, .LBB0_170
.LBB0_131:
	addi r7, fp, -112
.LBB0_132:
	add r9, r4, r0
	srli r4, r4, 16
	addi r8, r1, 1311
	mul r10, r4, r8
	addi r15, r3, -1
	and r15, r9, r15
	addi r16, r5, 491
	mul r17, r15, r16
	add r10, r17, r10
	mul r8, r15, r8
	srli r8, r8, 16
	add r8, r10, r8
	srli r8, r8, 16
	mul r4, r4, r16
	add r4, r4, r8
	srli r4, r4, 5
	mul r8, r4, r6
	sub r8, r9, r8
	slli r8, r8, 1
	add r10, r8, r22
	add r8, r8, r11
	ldbu r8, r8+0
	stb r7+0, r8
	ldbu r10, r10+0
	addi r8, r7, 2
	stb r7+1, r10
	addi r10, r19, 1807
	add r7, r8, r0
	bgtu r9, r10, .LBB0_132
.LBB0_133:
	addi r1, r0, 10
	bltu r4, r1, .LBB0_171
.LBB0_134:
	slli r1, r4, 1
	add r3, r1, r22
	add r1, r1, r11
	ldbu r1, r1+0
	stb r8+0, r1
	ldbu r3, r3+0
	addi r1, r8, 2
	stb r8+1, r3
	jal r0, .LBB0_172
.LBB0_135:
	addi r1, r0, 48
	addi r15, fp, -112
	sth r15+0, r1
	addi r16, r0, 1
	jal r0, .LBB0_176
.LBB0_136:
	addi r3, fp, -112
	stb r3+0, r25
	addi r3, r0, 0
	sub r4, r3, r4
	ldw r3, fp+-172
	addi r14, r0, 0
	beq r4, r14, .LBB0_146
.LBB0_137:
	stw fp+-148, r8
	stw fp+-152, r9
	add r17, r10, r0
	addi r6, r0, 100
	add r5, r3, r0
	bltu r4, r6, .LBB0_139
.LBB0_138:
	add r7, r4, r0
	srli r4, r4, 16
	lui r8, 8
	addi r8, r8, 1311
	mul r9, r4, r8
	lui r10, 16
	addi r10, r10, -1
	and r10, r7, r10
	lui r15, 5
	addi r15, r15, 491
	mul r16, r10, r15
	add r9, r16, r9
	mul r8, r10, r8
	srli r8, r8, 16
	add r8, r9, r8
	srli r8, r8, 16
	mul r4, r4, r15
	add r4, r4, r8
	srli r4, r4, 5
	mul r8, r4, r6
	sub r8, r7, r8
	slli r8, r8, 1
	add r9, r8, r22
	add r8, r8, r11
	ldbu r8, r8+0
	stb r5+0, r8
	ldbu r8, r9+0
	addi r9, r5, 2
	stb r5+1, r8
	addi r8, r19, 1807
	add r5, r9, r0
	bgtu r7, r8, .LBB0_138
.LBB0_139:
	addi r6, r0, 10
	bltu r4, r6, .LBB0_148
.LBB0_140:
	slli r4, r4, 1
	add r6, r4, r22
	add r4, r4, r11
	ldbu r4, r4+0
	stb r5+0, r4
	ldbu r6, r6+0
	addi r4, r5, 2
	stb r5+1, r6
	jal r0, .LBB0_149
.LBB0_141:
	addi r3, fp, -112
	stb r3+0, r25
	addi r3, r0, 0
	sub r4, r3, r4
	ldw r3, fp+-172
	beq r4, r14, .LBB0_146
.LBB0_142:
	stw fp+-148, r8
	stw fp+-152, r9
	add r17, r10, r0
	addi r6, r0, 100
	add r5, r3, r0
	bltu r4, r6, .LBB0_144
.LBB0_143:
	add r7, r4, r0
	srli r4, r4, 16
	lui r8, 8
	addi r8, r8, 1311
	mul r9, r4, r8
	lui r10, 16
	addi r10, r10, -1
	and r10, r7, r10
	lui r15, 5
	addi r15, r15, 491
	mul r16, r10, r15
	add r9, r16, r9
	mul r8, r10, r8
	srli r8, r8, 16
	add r8, r9, r8
	srli r8, r8, 16
	mul r4, r4, r15
	add r4, r4, r8
	srli r4, r4, 5
	mul r8, r4, r6
	sub r8, r7, r8
	slli r8, r8, 1
	add r9, r8, r22
	add r8, r8, r11
	ldbu r8, r8+0
	stb r5+0, r8
	ldbu r8, r9+0
	addi r9, r5, 2
	stb r5+1, r8
	addi r8, r19, 1807
	add r5, r9, r0
	bgtu r7, r8, .LBB0_143
.LBB0_144:
	addi r6, r0, 10
	bltu r4, r6, .LBB0_152
.LBB0_145:
	slli r4, r4, 1
	add r6, r4, r22
	add r4, r4, r11
	ldbu r4, r4+0
	stb r5+0, r4
	ldbu r6, r6+0
	addi r4, r5, 2
	stb r5+1, r6
	jal r0, .LBB0_153
.LBB0_146:
	addi r4, r0, 48
	stb r3+0, r4
	addi r4, r0, 0
	stb r3+1, r4
	addi r5, r0, 1
	jal r0, .LBB0_157
.LBB0_147:
	add r1, r14, r0
	add r10, r11, r0
	add r9, r17, r0
	add r8, r19, r0
	jal r0, .LBB0_177
.LBB0_148:
	ori  r6, r4, 48
	addi r4, r5, 1
	stb r5+0, r6
.LBB0_149:
	add r10, r17, r0
	sub r5, r4, r3
	addi r6, r0, 2
	blt r5, r6, .LBB0_156
.LBB0_150:
	add r6, r3, r5
	addi r7, r6, -2
	add r6, r3, r0
.LBB0_151:
	add r8, r7, r0
	ldbu r7, r6+0
	ldbu r9, r8+1
	stb r6+0, r9
	stb r8+1, r7
	addi r6, r6, 1
	addi r7, r8, -1
	bltu r6, r8, .LBB0_151
	jal r0, .LBB0_156
.LBB0_152:
	ori  r6, r4, 48
	addi r4, r5, 1
	stb r5+0, r6
.LBB0_153:
	add r10, r17, r0
	sub r5, r4, r3
	addi r6, r0, 2
	blt r5, r6, .LBB0_156
.LBB0_154:
	add r6, r3, r5
	addi r7, r6, -2
	add r6, r3, r0
.LBB0_155:
	add r8, r7, r0
	ldbu r7, r6+0
	ldbu r9, r8+1
	stb r6+0, r9
	stb r8+1, r7
	addi r6, r6, 1
	addi r7, r8, -1
	bltu r6, r8, .LBB0_155
.LBB0_156:
	stb r4+0, r14
	ldw r9, fp+-152
	ldw r8, fp+-148
.LBB0_157:
	add r3, r3, r5
	addi r15, fp, -112
	sub r16, r3, r15
	jal r0, .LBB0_177
.LBB0_158:
	addi r3, fp, -112
.LBB0_159:
	addi r5, r0, 10
	sltu r5, r1, r5
	seq r4, r4, r21
	sub r4, r14, r4
	and r4, r5, r4
	bne r4, r14, .LBB0_161
.LBB0_160:
	slli r1, r1, 1
	add r4, r1, r22
	add r1, r1, r11
	ldbu r1, r1+0
	stb r3+0, r1
	ldbu r4, r4+0
	addi r1, r3, 2
	stb r3+1, r4
	jal r0, .LBB0_162
.LBB0_161:
	ori  r4, r1, 48
	addi r1, r3, 1
	stb r3+0, r4
.LBB0_162:
	ldw r3, fp+-124
	ldw r18, fp+-128
	ldw lr, fp+-168
	ldw r21, fp+-160
	ldw r10, fp+-164
	ldw r9, fp+-152
	ldw r8, fp+-148
	addi r15, fp, -112
	sub r16, r1, r15
	addi r3, r0, 2
	blt r16, r3, .LBB0_175
.LBB0_163:
	ldw r3, fp+-176
	add r4, r3, r1
	addi r3, fp, -112
.LBB0_164:
	add r5, r4, r0
	ldbu r4, r3+0
	ldbu r6, r5+1
	stb r3+0, r6
	stb r5+1, r4
	addi r3, r3, 1
	addi r4, r5, -1
	bltu r3, r5, .LBB0_164
	jal r0, .LBB0_175
.LBB0_165:
	addi r8, fp, -112
	addi r1, r0, 10
	bgeu r4, r1, .LBB0_104
.LBB0_166:
	ori  r3, r4, 48
	addi r1, r8, 1
	stb r8+0, r3
.LBB0_167:
	ldw r10, fp+-164
	ldw r9, fp+-152
	addi r15, fp, -112
	sub r16, r1, r15
	addi r3, r0, 2
	ldw r8, fp+-148
	blt r16, r3, .LBB0_175
.LBB0_168:
	ldw r3, fp+-176
	add r4, r3, r1
	addi r3, fp, -112
.LBB0_169:
	add r5, r4, r0
	ldbu r4, r3+0
	ldbu r6, r5+1
	stb r3+0, r6
	stb r5+1, r4
	addi r3, r3, 1
	addi r4, r5, -1
	bltu r3, r5, .LBB0_169
	jal r0, .LBB0_175
.LBB0_170:
	ldw r8, fp+-180
	addi r1, r0, 10
	bgeu r4, r1, .LBB0_134
.LBB0_171:
	ori  r3, r4, 48
	addi r1, r8, 1
	stb r8+0, r3
.LBB0_172:
	ldw r10, fp+-164
	ldw r9, fp+-152
	addi r15, fp, -112
	sub r16, r1, r15
	addi r3, r0, 2
	ldw r8, fp+-148
	blt r16, r3, .LBB0_175
.LBB0_173:
	ldw r3, fp+-176
	add r4, r3, r1
	addi r3, fp, -112
.LBB0_174:
	add r5, r4, r0
	ldbu r4, r3+0
	ldbu r6, r5+1
	stb r3+0, r6
	stb r5+1, r4
	addi r3, r3, 1
	addi r4, r5, -1
	bltu r3, r5, .LBB0_174
.LBB0_175:
	stb r1+0, r14
.LBB0_176:
	add r1, r14, r0
.LBB0_177:
	addi r3, r0, 0
	sth r18+0, r3
	ori  r4, r18, 2
	stb r4+0, r3
	beq r1, r3, .LBB0_179
.LBB0_178:
	ldbu r6, r15+0
	add r5, r18, r0
	add r4, r3, r0
	beq r6, r25, .LBB0_186
.LBB0_179:
	addi r4, r0, -1
	xor r5, r8, r4
	or  r5, r1, r5
	andi r5, r5, 1
	bne r5, r3, .LBB0_182
.LBB0_180:
	addi r6, r0, 43
	addi r5, r0, 100
	beq r28, r5, .LBB0_185
.LBB0_181:
	addi r5, r0, 105
	beq r28, r5, .LBB0_185
.LBB0_182:
	xor r4, r9, r4
	or  r4, r1, r4
	andi r6, r4, 1
	add r5, r18, r0
	add r4, r3, r0
	bne r6, r3, .LBB0_186
.LBB0_183:
	addi r6, r0, 32
	addi r4, r0, 105
	beq r28, r4, .LBB0_185
.LBB0_184:
	addi r7, r0, 100
	add r5, r18, r0
	add r4, r3, r0
	bne r28, r7, .LBB0_186
.LBB0_185:
	stb r18+0, r6
	addi r4, r0, 1
	ldw r5, fp+-144
.LBB0_186:
	beq r14, r3, .LBB0_189
.LBB0_187:
	addi r3, fp, -116
	or  r3, r3, r4
	addi r6, r0, 48
	stb r3+0, r6
	ori  r21, r4, 2
	addi r3, r0, 120
.LBB0_188:
	stb r5+1, r3
	ldw r23, fp+-132
	jal r0, .LBB0_197
.LBB0_189:
	andi r6, r21, 1
	addi r3, r0, 0
	beq r6, r3, .LBB0_192
.LBB0_190:
	ori  r6, r28, 32
	addi r7, r0, 120
	bne r6, r7, .LBB0_192
.LBB0_191:
	ldbu r7, r15+0
	addi r6, r0, 48
	bne r7, r6, .LBB0_207
.LBB0_192:
	ori r5, r0, 111
	sne r5, r28, r5
	addi r6, r0, -1
	xor r6, r21, r6
	or  r5, r5, r6
	andi r5, r5, 1
	bne r5, r3, .LBB0_195
.LBB0_193:
	ldbu r5, r15+0
	addi r3, r0, 48
	ldw r23, fp+-132
	bne r5, r3, .LBB0_196
.LBB0_194:
	add r21, r4, r0
	jal r0, .LBB0_197
.LBB0_195:
	add r21, r4, r0
	ldw r23, fp+-132
	jal r0, .LBB0_197
.LBB0_196:
	addi r21, r4, 1
	addi r5, fp, -116
	or  r4, r5, r4
	stb r4+0, r3
.LBB0_197:
	addi r18, r0, 0
	andi r19, r10, 1
	andi r11, r24, 1
	beq r19, r18, .LBB0_203
.LBB0_198:
	bne r11, r18, .LBB0_203
.LBB0_199:
	beq r1, r18, .LBB0_204
.LBB0_200:
	ldbu r1, r15+0
	bne r1, r25, .LBB0_204
.LBB0_201:
	ldw r1, fp+-120
	bgeu r13, r1, .LBB0_208
.LBB0_202:
	addi r14, r13, 1
	add r1, r23, r13
	stb r1+0, r25
	jal r0, .LBB0_209
.LBB0_203:
	add r17, r16, r0
	add r14, r13, r0
	add r22, r21, r0
	jal r0, .LBB0_211
.LBB0_204:
	add r17, r16, r0
	add r14, r13, r0
	beq r21, r18, .LBB0_210
.LBB0_205:
	add r1, r14, r21
	ldw r3, fp+-120
	sgtu r1, r1, r3
	sub r3, r3, r14
	xor r3, r3, r21
	addi r22, r0, 0
	sub r1, r22, r1
	and r1, r3, r1
	xor r13, r21, r1
	beq r13, r22, .LBB0_211
.LBB0_206:
	add r3, r23, r14
	addi r4, fp, -116
	add r5, r13, r0
	add r20, lr, r0
	jal r31, memcpy
	add lr, r20, r0
	add r14, r13, r14
	jal r0, .LBB0_211
.LBB0_207:
	ori r7, r0, 88
	seq r7, r28, r7
	addi r8, fp, -116
	or  r8, r8, r4
	stb r8+0, r6
	sub r3, r3, r7
	andi r3, r3, 32
	xori r3, r3, 120
	ori  r21, r4, 2
	jal r0, .LBB0_188
.LBB0_208:
	add r14, r13, r0
.LBB0_209:
	addi r15, r15, 1
	addi r17, r16, -1
	bne r21, r18, .LBB0_205
.LBB0_210:
	add r22, r18, r0
.LBB0_211:
	add r1, r21, r16
	sgtu r3, r26, r1
	sub r1, r26, r1
	sub r3, r18, r3
	and r1, r1, r3
	sub r3, r18, lr
	and r16, r1, r3
	xori r1, r24, 1
	ori r3, r0, 0
	seq r3, r16, r3
	addi r4, r0, -1
	xor r1, r1, r4
	or  r1, r3, r1
	andi r1, r1, 1
	bne r1, r18, .LBB0_215
.LBB0_212:
	add r13, r14, r16
	ldw r3, fp+-120
	sgtu r1, r13, r3
	sub r3, r3, r14
	xor r3, r3, r16
	addi r20, r0, 0
	sub r1, r20, r1
	and r1, r3, r1
	xor r5, r16, r1
	beq r5, r20, .LBB0_214
.LBB0_213:
	sub r1, r18, r19
	addi r3, r11, -1
	and r1, r3, r1
	andi r1, r1, 16
	ori  r4, r1, 32
	add r3, r23, r14
	jal r31, memset
	ldw r4, fp+-120
	xor r1, r13, r4
	sltu r3, r13, r4
	sub r3, r20, r3
	and r1, r1, r3
	xor r14, r4, r1
.LBB0_214:
	ldw r1, fp+-124
.LBB0_215:
	addi r18, r0, 0
	beq r22, r18, .LBB0_218
.LBB0_216:
	add r1, r14, r22
	ldw r3, fp+-120
	sgtu r1, r1, r3
	sub r3, r3, r14
	xor r3, r3, r22
	sub r1, r18, r1
	and r1, r3, r1
	xor r13, r22, r1
	beq r13, r18, .LBB0_218
.LBB0_217:
	add r3, r23, r14
	addi r4, fp, -116
	add r5, r13, r0
	jal r31, memcpy
	add r14, r13, r14
.LBB0_218:
	beq r17, r18, .LBB0_221
.LBB0_219:
	ldw r1, fp+-120
	bleu r1, r14, .LBB0_221
.LBB0_220:
	add r1, r14, r17
	ldw r3, fp+-120
	sgtu r1, r1, r3
	sub r3, r3, r14
	xor r3, r3, r17
	sub r1, r18, r1
	and r1, r3, r1
	xor r13, r17, r1
	add r3, r23, r14
	add r4, r15, r0
	add r5, r13, r0
	jal r31, memcpy
	add r14, r13, r14
.LBB0_221:
	bne r16, r18, .LBB0_222
	jal r0, .LBB0_6
.LBB0_222:
	bne r11, r18, .LBB0_223
	jal r0, .LBB0_6
.LBB0_223:
	add r11, r14, r16
	ldw r3, fp+-120
	sgtu r1, r11, r3
	sub r3, r3, r14
	xor r3, r3, r16
	sub r1, r18, r1
	and r1, r3, r1
	xor r5, r16, r1
	bne r5, r18, .LBB0_224
	jal r0, .LBB0_6
.LBB0_224:
	add r3, r23, r14
	addi r4, r0, 32
	jal r31, memset
	ldw r4, fp+-120
	xor r1, r11, r4
	sltu r3, r11, r4
	sub r3, r18, r3
	and r1, r1, r3
	xor r13, r4, r1
	jal r0, .LBB0_7
.LBB0_225:
	add r1, r23, r13
	stb r1+0, r3
.LBB0_226:
	add r1, r13, r0
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
	addi sp, sp, 200
	jalr r0, r31, 0
.Lfunc_end0:
	.size	vsnprintf_enhanced, .Lfunc_end0-vsnprintf_enhanced
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI0_0:
	.word	.LBB0_15
	.word	.LBB0_19
	.word	.LBB0_19
	.word	.LBB0_16
	.word	.LBB0_19
	.word	.LBB0_19
	.word	.LBB0_19
	.word	.LBB0_19
	.word	.LBB0_19
	.word	.LBB0_19
	.word	.LBB0_18
	.word	.LBB0_10
	.word	.LBB0_19
	.word	.LBB0_19
	.word	.LBB0_19
	.word	.LBB0_19
	.word	.LBB0_17
.LJTI0_1:
	.word	.LBB0_52
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_41
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_76
	.word	.LBB0_48
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_48
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_61
	.word	.LBB0_68
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_82
	.word	.LBB0_60
	.word	.LBB0_77
	.word	.LBB0_60
	.word	.LBB0_60
	.word	.LBB0_41
                                        # -- End function
	.text
	.globl	sprintf                         # -- Begin function sprintf
	.p2align	2
	.type	sprintf,@function
sprintf:                                # @sprintf
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 56
	stw fp+-28, r11
	stw fp+-32, lr
	add r1, r4, r0
	addi r11, fp, -24
	stw r11+20, r10
	stw r11+16, r9
	stw r11+12, r8
	stw r11+8, r7
	ori  r4, r11, 4
	stw r4+0, r6
	stw r11+0, r5
	addi r4, fp, -36
	stw r4+0, r11
	lui r4, 16
	add r5, r1, r0
	add r6, r11, r0
	jal r31, vsnprintf_enhanced
	ldw lr, fp+-32
	ldw r11, fp+-28
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end1:
	.size	sprintf, .Lfunc_end1-sprintf
                                        # -- End function
	.globl	snprintf                        # -- Begin function snprintf
	.p2align	2
	.type	snprintf,@function
snprintf:                               # @snprintf
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-24, lr
	addi r1, fp, -20
	stw r1+16, r10
	stw r1+12, r9
	stw r1+8, r8
	stw r1+4, r7
	stw r1+0, r6
	addi r6, fp, -28
	stw r6+0, r1
	add r6, r1, r0
	jal r31, vsnprintf_enhanced
	ldw lr, fp+-24
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end2:
	.size	snprintf, .Lfunc_end2-snprintf
                                        # -- End function
	.globl	vsnprintf                       # -- Begin function vsnprintf
	.p2align	2
	.type	vsnprintf,@function
vsnprintf:                              # @vsnprintf
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	jal r31, vsnprintf_enhanced
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	vsnprintf, .Lfunc_end3-vsnprintf
                                        # -- End function
	.globl	vsprintf                        # -- Begin function vsprintf
	.p2align	2
	.type	vsprintf,@function
vsprintf:                               # @vsprintf
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	add r6, r5, r0
	add r5, r4, r0
	lui r4, 16
	jal r31, vsnprintf_enhanced
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	vsprintf, .Lfunc_end4-vsprintf
                                        # -- End function
	.globl	printf                          # -- Begin function printf
	.p2align	2
	.type	printf,@function
printf:                                 # @printf
# %bb.0:
	addi sp, sp, -1096
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 1096
	stw fp+-32, r11
	stw fp+-36, r12
	stw fp+-40, r13
	stw fp+-44, r14
	stw fp+-48, lr
	add r1, r3, r0
	addi r12, fp, -28
	stw r12+24, r10
	stw r12+20, r9
	stw r12+16, r8
	stw r12+12, r7
	stw r12+8, r6
	stw r12+4, r5
	stw r12+0, r4
	addi r3, fp, -1076
	stw r3+0, r12
	addi r11, fp, -1072
	addi r4, r0, 1024
	add r3, r11, r0
	add r5, r1, r0
	add r6, r12, r0
	jal r31, vsnprintf_enhanced
	add r12, r1, r0
	addi r1, r0, 1
	blt r12, r1, .LBB5_3
.LBB5_1:
	addi r13, r0, 0
	add r14, r12, r0
.LBB5_2:
	ldb r3, r11+0
	jal r31, putchar
	addi r14, r14, -1
	addi r11, r11, 1
	bne r14, r13, .LBB5_2
.LBB5_3:
	add r1, r12, r0
	ldw lr, fp+-48
	ldw r14, fp+-44
	ldw r13, fp+-40
	ldw r12, fp+-36
	ldw r11, fp+-32
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 1096
	jalr r0, r31, 0
.Lfunc_end5:
	.size	printf, .Lfunc_end5-printf
                                        # -- End function
	.globl	vprintf                         # -- Begin function vprintf
	.p2align	2
	.type	vprintf,@function
vprintf:                                # @vprintf
# %bb.0:
	addi sp, sp, -1064
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 1064
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, lr
	add r6, r4, r0
	add r5, r3, r0
	addi r11, fp, -1044
	addi r4, r0, 1024
	add r3, r11, r0
	jal r31, vsnprintf_enhanced
	add r12, r1, r0
	addi r1, r0, 1
	blt r12, r1, .LBB6_3
.LBB6_1:
	addi r13, r0, 0
	add r14, r12, r0
.LBB6_2:
	ldb r3, r11+0
	jal r31, putchar
	addi r14, r14, -1
	addi r11, r11, 1
	bne r14, r13, .LBB6_2
.LBB6_3:
	add r1, r12, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 1064
	jalr r0, r31, 0
.Lfunc_end6:
	.size	vprintf, .Lfunc_end6-vprintf
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"(null)"
	.size	.L.str, 7

	.type	Digits100,@object               # @Digits100
Digits100:
	.asciz	"00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899"
	.size	Digits100, 201

	.type	Digits16U,@object               # @Digits16U
Digits16U:
	.asciz	"0123456789ABCDEF"
	.size	Digits16U, 17

	.type	Digits16L,@object               # @Digits16L
Digits16L:
	.asciz	"0123456789abcdef"
	.size	Digits16L, 17

	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 99630547eac2d049c7b64572e311926969e72fab)"
	.section	".note.GNU-stack","",@progbits
