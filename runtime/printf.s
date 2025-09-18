	.file	"printf.c"
	.text
	.globl	vprintf                         # -- Begin function vprintf
	.p2align	2
	.type	vprintf,@function
vprintf:                                # @vprintf
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
	addi r15, fp, -80
	stw r15+0, r4
	addi r16, fp, -92
	ori  r1, r16, 1
	stw fp+-100, r1
	addi r1, r16, -1
	stw fp+-108, r1
	addi r1, r16, -1
	stw fp+-112, r1
	addi r20, r16, -1
	addi r1, r16, -1
	stw fp+-116, r1
	addi r22, r0, 0
	addi r12, r0, 37
	addi r23, r0, 83
	addi r24, r0, 2
	lui r25, %hi(.LJTI0_0)
	addi r25, r25, %lo(.LJTI0_0)
	ori r1, r0, 88
	stw fp+-96, r1
	lui r27, %hi(Digits16L)
	addi r27, r27, %lo(Digits16L)
	lui r1, %hi(Digits16U)
	addi r1, r1, %lo(Digits16U)
	xor r28, r1, r27
	addi r18, r0, 48
	addi r1, r0, 1
	stw fp+-104, r1
	addi r21, r0, 15
	add r13, r22, r0
	jal r0, .LBB0_1
.LBB0_27:
	add r13, r13, r14
	add r11, r11, r24
.LBB0_1:
	ldbu r1, r11+0
	beq r1, r12, .LBB0_5
.LBB0_2:
	addi r2, r0, 0
	beq r1, r2, .LBB0_62
.LBB0_3:
	slli r1, r1, 24
	srai r3, r1, 24
	jal r31, putchar
	addi r13, r13, 1
	addi r1, r0, 1
	add r11, r11, r1
	jal r0, .LBB0_1
.LBB0_5:
	ldbu r3, r11+1
	addi r1, r3, -37
	sgtu r2, r1, r23
	bne r2, r22, .LBB0_61
.LBB0_6:
	slli r1, r1, 2
	add r1, r25, r1
	ldw r4, r1+0
	lui r1, 13
	lui r2, 16
	jalr r31, r4, 0
.LBB0_28:
	ldw r1, r15+0
	ldw r2, r1+0
	addi r1, r1, 4
	stw r15+0, r1
	beq r2, r22, .LBB0_65
.LBB0_29:
	ldw r1, fp+-96
	seq r1, r3, r1
	sub r1, r22, r1
	and r1, r28, r1
	xor r3, r1, r27
	add r4, r20, r0
	add r14, r22, r0
.LBB0_30:
	add r1, r4, r0
	add r4, r16, r14
	andi r5, r2, 15
	add r5, r3, r5
	ldbu r5, r5+0
	stb r4+0, r5
	srli r5, r2, 4
	addi r14, r14, 1
	addi r4, r1, 1
	sgtu r6, r2, r21
	add r2, r5, r0
	bne r6, r22, .LBB0_30
.LBB0_31:
	addi r2, fp, -92
	add r4, r2, r14
	addi r3, r0, 0
	stb r4+0, r3
	addi r4, r4, -1
	sgeu r4, r2, r4
	bne r4, r3, .LBB0_33
.LBB0_32:
	ldbu r4, r1+1
	ldbu r5, r2+0
	stb r1+1, r5
	stb r2+0, r4
	addi r2, r2, 1
	addi r4, r1, -1
	sltu r5, r2, r1
	add r1, r4, r0
	bne r5, r3, .LBB0_32
	jal r0, .LBB0_33
.LBB0_7:
	ldw r3, r15+0
	ldw r5, r3+0
	addi r3, r3, 4
	stw r15+0, r3
	ori r3, r0, -1
	sgt r3, r5, r3
	addi r14, r0, 0
	beq r3, r14, .LBB0_8
.LBB0_9:
	addi r3, fp, -92
	bne r5, r14, .LBB0_10
.LBB0_63:
	sth r3+0, r18
	addi r4, r0, 1
	jal r0, .LBB0_15
.LBB0_59:
	add r3, r12, r0
	jal r0, .LBB0_60
.LBB0_61:
	add r3, r12, r0
	jal r31, putchar
	ldb r3, r11+1
	jal r31, putchar
	addi r13, r13, 2
	add r11, r11, r24
	jal r0, .LBB0_1
.LBB0_35:
	ldw r1, r15+0
	ldw r4, r1+0
	addi r1, r1, 4
	stw r15+0, r1
	addi r3, r0, 0
	beq r4, r3, .LBB0_66
.LBB0_36:
	ldw r5, fp+-112
	add r14, r3, r0
.LBB0_37:
	add r1, r5, r0
	addi r2, fp, -92
	add r5, r2, r14
	andi r6, r4, 7
	ori  r6, r6, 48
	stb r5+0, r6
	srli r6, r4, 3
	addi r14, r14, 1
	addi r5, r1, 1
	addi r7, r0, 7
	sgtu r7, r4, r7
	add r4, r6, r0
	bne r7, r3, .LBB0_37
.LBB0_38:
	add r4, r2, r14
	addi r3, r0, 0
	stb r4+0, r3
	addi r4, r4, -1
	sgeu r4, r2, r4
	bne r4, r3, .LBB0_40
.LBB0_39:
	ldbu r4, r1+1
	ldbu r5, r2+0
	stb r1+1, r5
	stb r2+0, r4
	addi r2, r2, 1
	addi r4, r1, -1
	sltu r5, r2, r1
	add r1, r4, r0
	bne r5, r3, .LBB0_39
	jal r0, .LBB0_40
.LBB0_42:
	ldw r1, r15+0
	ldw r17, r1+0
	addi r1, r1, 4
	stw r15+0, r1
	addi r14, r0, 48
	add r3, r14, r0
	jal r31, putchar
	addi r3, r0, 120
	jal r31, putchar
	addi r3, r0, 0
	beq r17, r3, .LBB0_67
.LBB0_43:
	ldw r4, fp+-108
	add r14, r3, r0
.LBB0_44:
	add r1, r4, r0
	addi r2, fp, -92
	add r4, r2, r14
	andi r5, r17, 15
	add r5, r5, r27
	ldbu r5, r5+0
	stb r4+0, r5
	srli r5, r17, 4
	addi r14, r14, 1
	addi r4, r1, 1
	sgtu r6, r17, r21
	add r17, r5, r0
	bne r6, r3, .LBB0_44
.LBB0_45:
	add r4, r2, r14
	addi r3, r0, 0
	stb r4+0, r3
	addi r4, r4, -1
	sgeu r4, r2, r4
	bne r4, r3, .LBB0_47
.LBB0_46:
	ldbu r4, r1+1
	ldbu r5, r2+0
	stb r1+1, r5
	stb r2+0, r4
	addi r2, r2, 1
	addi r4, r1, -1
	sltu r5, r2, r1
	add r1, r4, r0
	bne r5, r3, .LBB0_46
.LBB0_47:
	addi r1, r0, 7
	sgtu r1, r14, r1
	beq r1, r3, .LBB0_48
	jal r0, .LBB0_50
.LBB0_53:
	ldw r1, r15+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r15+0, r1
.LBB0_60:
	jal r31, putchar
	addi r13, r13, 1
	add r11, r11, r24
	jal r0, .LBB0_1
.LBB0_20:
	ldw r3, r15+0
	ldw r6, r3+0
	addi r3, r3, 4
	stw r15+0, r3
	addi r5, r0, 0
	beq r6, r5, .LBB0_64
.LBB0_21:
	ldw r7, fp+-116
	add r14, r5, r0
.LBB0_22:
	add r3, r7, r0
	addi r4, fp, -92
	add r7, r4, r14
	srli r8, r6, 16
	addi r9, r1, -819
	mul r10, r8, r9
	addi r17, r2, -1
	and r17, r6, r17
	addi r19, r1, -820
	mul r26, r17, r19
	add r10, r26, r10
	mul r9, r17, r9
	srli r9, r9, 16
	add r9, r10, r9
	srli r9, r9, 16
	mul r8, r8, r19
	add r8, r8, r9
	srli r8, r8, 3
	addi r9, r0, 10
	mul r9, r8, r9
	sub r9, r6, r9
	ori  r9, r9, 48
	stb r7+0, r9
	addi r14, r14, 1
	addi r7, r3, 1
	addi r9, r0, 9
	sgtu r9, r6, r9
	add r6, r8, r0
	bne r9, r5, .LBB0_22
.LBB0_23:
	add r2, r4, r14
	addi r1, r0, 0
	stb r2+0, r1
	addi r2, r2, -1
	sgeu r2, r4, r2
	bne r2, r1, .LBB0_25
.LBB0_24:
	ldbu r2, r3+1
	ldbu r5, r4+0
	stb r3+1, r5
	stb r4+0, r2
	addi r4, r4, 1
	addi r2, r3, -1
	sltu r5, r4, r3
	add r3, r2, r0
	bne r5, r1, .LBB0_24
	jal r0, .LBB0_25
.LBB0_54:
	ldw r1, r15+0
	ldw r2, r1+0
	addi r1, r1, 4
	stw r15+0, r1
	ori r1, r0, 0
	seq r1, r2, r1
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	xor r3, r2, r3
	addi r14, r0, 0
	sub r1, r14, r1
	and r1, r3, r1
	xor r2, r2, r1
	ldbu r1, r2+0
	beq r1, r14, .LBB0_55
.LBB0_56:
	addi r17, r2, 1
.LBB0_57:
	slli r1, r1, 24
	srai r3, r1, 24
	jal r31, putchar
	addi r13, r13, 1
	ldbu r1, r17+0
	addi r17, r17, 1
	bne r1, r14, .LBB0_57
.LBB0_58:
	add r11, r11, r24
	jal r0, .LBB0_1
.LBB0_65:
	sth r16+0, r18
	ldw r14, fp+-104
.LBB0_33:
	add r17, r16, r0
	add r19, r14, r0
.LBB0_34:
	ldb r3, r17+0
	jal r31, putchar
	addi r19, r19, -1
	addi r17, r17, 1
	bne r19, r22, .LBB0_34
	jal r0, .LBB0_27
.LBB0_8:
	addi r3, r0, 45
	addi r4, fp, -92
	stb r4+0, r3
	addi r3, r0, 0
	sub r5, r3, r5
	ldw r3, fp+-100
.LBB0_10:
	addi r7, r3, -1
	add r4, r14, r0
.LBB0_11:
	add r6, r7, r0
	add r7, r3, r4
	srli r8, r5, 16
	addi r9, r1, -819
	mul r10, r8, r9
	addi r17, r2, -1
	and r17, r5, r17
	addi r19, r1, -820
	mul r26, r17, r19
	add r10, r26, r10
	mul r9, r17, r9
	srli r9, r9, 16
	add r9, r10, r9
	srli r9, r9, 16
	mul r8, r8, r19
	add r8, r8, r9
	srli r8, r8, 3
	addi r9, r0, 10
	mul r9, r8, r9
	sub r9, r5, r9
	ori  r9, r9, 48
	stb r7+0, r9
	addi r4, r4, 1
	addi r7, r6, 1
	addi r9, r0, 9
	sgtu r9, r5, r9
	add r5, r8, r0
	bne r9, r14, .LBB0_11
.LBB0_12:
	add r2, r3, r4
	addi r1, r0, 0
	stb r2+0, r1
	addi r2, r2, -1
	sgeu r2, r3, r2
	bne r2, r1, .LBB0_15
.LBB0_13:
	add r2, r3, r0
.LBB0_14:
	ldbu r5, r6+1
	ldbu r7, r2+0
	stb r6+1, r7
	stb r2+0, r5
	addi r2, r2, 1
	addi r5, r6, -1
	sltu r7, r2, r6
	add r6, r5, r0
	bne r7, r1, .LBB0_14
.LBB0_15:
	add r1, r3, r4
	addi r17, fp, -92
	beq r1, r17, .LBB0_19
.LBB0_16:
	sub r26, r1, r17
	add r19, r26, r0
.LBB0_17:
	ldb r3, r17+0
	jal r31, putchar
	addi r19, r19, -1
	addi r17, r17, 1
	bne r19, r14, .LBB0_17
.LBB0_18:
	add r13, r13, r26
.LBB0_19:
	add r11, r11, r24
	jal r0, .LBB0_1
.LBB0_66:
	addi r1, fp, -92
	sth r1+0, r18
	addi r14, r0, 1
.LBB0_40:
	addi r17, fp, -92
	add r19, r14, r0
.LBB0_41:
	ldb r3, r17+0
	jal r31, putchar
	addi r19, r19, -1
	addi r17, r17, 1
	addi r1, r0, 0
	bne r19, r1, .LBB0_41
	jal r0, .LBB0_27
.LBB0_67:
	addi r1, fp, -92
	sth r1+0, r14
	addi r14, r0, 1
.LBB0_48:
	addi r17, r0, 8
.LBB0_49:
	addi r3, r0, 48
	jal r31, putchar
	addi r17, r17, -1
	bne r14, r17, .LBB0_49
.LBB0_50:
	addi r17, fp, -92
	add r19, r14, r0
.LBB0_51:
	ldb r3, r17+0
	jal r31, putchar
	addi r19, r19, -1
	addi r17, r17, 1
	addi r1, r0, 0
	bne r19, r1, .LBB0_51
.LBB0_52:
	addi r2, r0, 8
	sgtu r2, r14, r2
	sub r1, r1, r2
	xori r2, r14, 8
	and r1, r2, r1
	xori r1, r1, 8
	add r1, r13, r1
	addi r13, r1, 2
	add r11, r11, r24
	jal r0, .LBB0_1
.LBB0_64:
	addi r1, fp, -92
	sth r1+0, r18
	addi r14, r0, 1
.LBB0_25:
	addi r17, fp, -92
	add r19, r14, r0
.LBB0_26:
	ldb r3, r17+0
	jal r31, putchar
	addi r19, r19, -1
	addi r17, r17, 1
	addi r1, r0, 0
	bne r19, r1, .LBB0_26
	jal r0, .LBB0_27
.LBB0_55:
	add r11, r11, r24
	jal r0, .LBB0_1
.LBB0_62:
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
	addi sp, sp, 136
	jalr r0, r31, 0
.Lfunc_end0:
	.size	vprintf, .Lfunc_end0-vprintf
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI0_0:
	.word	.LBB0_59
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_28
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_53
	.word	.LBB0_7
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_7
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_35
	.word	.LBB0_42
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_54
	.word	.LBB0_61
	.word	.LBB0_20
	.word	.LBB0_61
	.word	.LBB0_61
	.word	.LBB0_28
                                        # -- End function
	.text
	.globl	printf                          # -- Begin function printf
	.p2align	2
	.type	printf,@function
printf:                                 # @printf
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 56
	stw fp+-32, lr
	addi r1, fp, -28
	stw r1+24, r10
	stw r1+20, r9
	stw r1+16, r8
	stw r1+12, r7
	stw r1+8, r6
	stw r1+4, r5
	stw r1+0, r4
	addi r2, fp, -36
	stw r2+0, r1
	add r4, r1, r0
	jal r31, vprintf
	ldw lr, fp+-32
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end1:
	.size	printf, .Lfunc_end1-printf
                                        # -- End function
	.globl	vsprintf                        # -- Begin function vsprintf
	.p2align	2
	.type	vsprintf,@function
vsprintf:                               # @vsprintf
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
	add r12, r4, r0
	add r11, r3, r0
	addi r14, fp, -80
	stw r14+0, r5
	addi r15, fp, -92
	ori  r1, r15, 1
	stw fp+-96, r1
	addi r1, r15, -1
	stw fp+-104, r1
	addi r1, r15, -1
	stw fp+-108, r1
	addi r19, r15, -1
	addi r1, r15, -1
	stw fp+-112, r1
	addi r21, r0, 37
	addi r22, r0, 83
	addi r23, r0, 0
	lui r24, %hi(.LJTI2_0)
	addi r24, r24, %lo(.LJTI2_0)
	ori r25, r0, 88
	lui r26, %hi(Digits16L)
	addi r26, r26, %lo(Digits16L)
	lui r1, %hi(Digits16U)
	addi r1, r1, %lo(Digits16U)
	xor r27, r1, r26
	addi r28, r0, 48
	addi r1, r0, 1
	stw fp+-100, r1
	addi r18, r0, 15
	stw fp+-116, r3
	jal r0, .LBB2_1
.LBB2_27:
	add r11, r11, r1
	addi r12, r12, 2
.LBB2_1:
	ldbu r1, r12+0
	beq r1, r21, .LBB2_4
.LBB2_2:
	addi r2, r0, 0
	beq r1, r2, .LBB2_66
.LBB2_3:
	addi r12, r12, 1
	addi r2, r11, 1
	stb r11+0, r1
	add r11, r2, r0
	jal r0, .LBB2_1
.LBB2_4:
	ldbu r1, r12+1
	addi r2, r1, -37
	sgtu r3, r2, r22
	bne r3, r23, .LBB2_65
.LBB2_5:
	slli r2, r2, 2
	add r2, r24, r2
	ldw r4, r2+0
	lui r2, 13
	lui r3, 16
	jalr r31, r4, 0
.LBB2_28:
	ldw r2, r14+0
	ldw r3, r2+0
	addi r2, r2, 4
	stw r14+0, r2
	beq r3, r23, .LBB2_69
.LBB2_29:
	seq r1, r1, r25
	sub r1, r23, r1
	and r1, r27, r1
	xor r4, r1, r26
	add r5, r19, r0
	add r1, r23, r0
.LBB2_30:
	add r2, r5, r0
	add r5, r15, r1
	andi r6, r3, 15
	add r6, r4, r6
	ldbu r6, r6+0
	stb r5+0, r6
	srli r6, r3, 4
	addi r1, r1, 1
	addi r5, r2, 1
	sgtu r7, r3, r18
	add r3, r6, r0
	bne r7, r23, .LBB2_30
.LBB2_31:
	addi r3, fp, -92
	add r5, r3, r1
	addi r4, r0, 0
	stb r5+0, r4
	addi r5, r5, -1
	sgeu r5, r3, r5
	bne r5, r4, .LBB2_33
.LBB2_32:
	ldbu r5, r2+1
	ldbu r6, r3+0
	stb r2+1, r6
	stb r3+0, r5
	addi r3, r3, 1
	addi r5, r2, -1
	sltu r6, r3, r2
	add r2, r5, r0
	bne r6, r4, .LBB2_32
.LBB2_33:
	bne r1, r23, .LBB2_34
	jal r0, .LBB2_27
.LBB2_6:
	ldw r1, r14+0
	ldw r6, r1+0
	addi r1, r1, 4
	stw r14+0, r1
	ori r1, r0, -1
	sgt r4, r6, r1
	addi r1, r0, 0
	beq r4, r1, .LBB2_7
.LBB2_8:
	addi r4, fp, -92
	bne r6, r1, .LBB2_9
.LBB2_67:
	sth r4+0, r28
	addi r5, r0, 1
	jal r0, .LBB2_14
.LBB2_63:
	addi r1, r11, 1
	stb r11+0, r21
	jal r0, .LBB2_64
.LBB2_65:
	stb r11+0, r21
	ldbu r1, r12+1
	addi r2, r11, 2
	stb r11+1, r1
	addi r12, r12, 2
	add r11, r2, r0
	jal r0, .LBB2_1
.LBB2_36:
	ldw r1, r14+0
	ldw r5, r1+0
	addi r1, r1, 4
	stw r14+0, r1
	addi r4, r0, 0
	beq r5, r4, .LBB2_70
.LBB2_37:
	ldw r6, fp+-108
	add r1, r4, r0
.LBB2_38:
	add r2, r6, r0
	addi r3, fp, -92
	add r6, r3, r1
	andi r7, r5, 7
	ori  r7, r7, 48
	stb r6+0, r7
	srli r7, r5, 3
	addi r1, r1, 1
	addi r6, r2, 1
	addi r8, r0, 7
	sgtu r8, r5, r8
	add r5, r7, r0
	bne r8, r4, .LBB2_38
.LBB2_39:
	add r5, r3, r1
	addi r4, r0, 0
	stb r5+0, r4
	addi r5, r5, -1
	sgeu r5, r3, r5
	bne r5, r4, .LBB2_41
.LBB2_40:
	ldbu r5, r2+1
	ldbu r6, r3+0
	stb r2+1, r6
	stb r3+0, r5
	addi r3, r3, 1
	addi r5, r2, -1
	sltu r6, r3, r2
	add r2, r5, r0
	bne r6, r4, .LBB2_40
.LBB2_41:
	addi r2, r0, 0
	bne r1, r2, .LBB2_42
	jal r0, .LBB2_27
.LBB2_44:
	ldw r1, r14+0
	ldw r5, r1+0
	addi r1, r1, 4
	stw r14+0, r1
	stb r11+0, r28
	addi r1, r11, 2
	addi r2, r0, 120
	stb r11+1, r2
	addi r6, r0, 0
	beq r5, r6, .LBB2_50
.LBB2_45:
	ldw r7, fp+-104
	add r2, r6, r0
.LBB2_46:
	add r3, r7, r0
	addi r4, fp, -92
	add r7, r4, r2
	andi r8, r5, 15
	add r8, r8, r26
	ldbu r8, r8+0
	stb r7+0, r8
	srli r8, r5, 4
	addi r2, r2, 1
	addi r7, r3, 1
	sgtu r9, r5, r18
	add r5, r8, r0
	bne r9, r6, .LBB2_46
.LBB2_47:
	add r6, r4, r2
	addi r5, r0, 0
	stb r6+0, r5
	addi r6, r6, -1
	sgeu r6, r4, r6
	bne r6, r5, .LBB2_49
.LBB2_48:
	ldbu r6, r3+1
	ldbu r7, r4+0
	stb r3+1, r7
	stb r4+0, r6
	addi r4, r4, 1
	addi r6, r3, -1
	sltu r7, r4, r3
	add r3, r6, r0
	bne r7, r5, .LBB2_48
.LBB2_49:
	addi r3, r0, 7
	sgtu r3, r2, r3
	beq r3, r5, .LBB2_51
.LBB2_55:
	addi r3, r0, 0
	bne r2, r3, .LBB2_56
	jal r0, .LBB2_58
.LBB2_59:
	ldw r1, r14+0
	ldw r2, r1+0
	addi r1, r1, 4
	stw r14+0, r1
	addi r1, r11, 1
	stb r11+0, r2
.LBB2_64:
	addi r12, r12, 2
	add r11, r1, r0
	jal r0, .LBB2_1
.LBB2_18:
	ldw r1, r14+0
	ldw r6, r1+0
	addi r1, r1, 4
	stw r14+0, r1
	addi r5, r0, 0
	beq r6, r5, .LBB2_68
.LBB2_19:
	ldw r7, fp+-112
	add r1, r5, r0
.LBB2_20:
	add r4, r7, r0
	add r7, r15, r1
	srli r8, r6, 16
	addi r9, r2, -819
	mul r10, r8, r9
	addi r13, r3, -1
	and r13, r6, r13
	addi r16, r2, -820
	mul r17, r13, r16
	add r10, r17, r10
	mul r9, r13, r9
	srli r9, r9, 16
	add r9, r10, r9
	srli r9, r9, 16
	mul r8, r8, r16
	add r8, r8, r9
	srli r8, r8, 3
	addi r9, r0, 10
	mul r9, r8, r9
	sub r9, r6, r9
	ori  r9, r9, 48
	stb r7+0, r9
	addi r1, r1, 1
	addi r7, r4, 1
	addi r9, r0, 9
	sgtu r9, r6, r9
	add r6, r8, r0
	bne r9, r5, .LBB2_20
.LBB2_21:
	add r3, r15, r1
	addi r2, r0, 0
	stb r3+0, r2
	addi r3, r3, -1
	sgeu r3, r15, r3
	bne r3, r2, .LBB2_24
.LBB2_22:
	add r3, r15, r0
.LBB2_23:
	ldbu r5, r4+1
	ldbu r6, r3+0
	stb r4+1, r6
	stb r3+0, r5
	addi r3, r3, 1
	addi r5, r4, -1
	sltu r6, r3, r4
	add r4, r5, r0
	bne r6, r2, .LBB2_23
.LBB2_24:
	addi r2, r0, 0
	bne r1, r2, .LBB2_25
	jal r0, .LBB2_27
.LBB2_60:
	ldw r1, r14+0
	ldw r2, r1+0
	addi r1, r1, 4
	stw r14+0, r1
	ori r1, r0, 0
	seq r1, r2, r1
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	xor r3, r2, r3
	addi r20, r0, 0
	sub r1, r20, r1
	and r1, r3, r1
	xor r13, r2, r1
	add r3, r13, r0
	jal r31, strlen
	beq r1, r20, .LBB2_27
.LBB2_61:
	add r2, r20, r0
.LBB2_62:
	add r3, r13, r2
	ldbu r3, r3+0
	add r4, r11, r2
	stb r4+0, r3
	addi r2, r2, 1
	sltu r3, r2, r1
	bne r3, r20, .LBB2_62
	jal r0, .LBB2_27
.LBB2_69:
	sth r15+0, r28
	ldw r1, fp+-100
	beq r1, r23, .LBB2_27
.LBB2_34:
	add r2, r23, r0
.LBB2_35:
	add r3, r15, r2
	ldbu r3, r3+0
	add r4, r11, r2
	stb r4+0, r3
	addi r2, r2, 1
	sltu r3, r2, r1
	bne r3, r23, .LBB2_35
	jal r0, .LBB2_27
.LBB2_7:
	addi r4, r0, 45
	addi r5, fp, -92
	stb r5+0, r4
	addi r4, r0, 0
	sub r6, r4, r6
	ldw r4, fp+-96
.LBB2_9:
	addi r9, r4, -1
	addi r8, r0, 0
	add r5, r8, r0
.LBB2_10:
	add r7, r9, r0
	add r9, r4, r5
	srli r10, r6, 16
	addi r13, r2, -819
	mul r20, r10, r13
	addi lr, r3, -1
	and lr, r6, lr
	addi r16, r2, -820
	mul r17, lr, r16
	add r17, r17, r20
	mul r13, lr, r13
	srli r13, r13, 16
	add r13, r17, r13
	srli r13, r13, 16
	mul r10, r10, r16
	add r10, r10, r13
	srli r10, r10, 3
	addi r13, r0, 10
	mul r13, r10, r13
	sub r13, r6, r13
	ori  r13, r13, 48
	stb r9+0, r13
	addi r5, r5, 1
	addi r9, r7, 1
	addi r13, r0, 9
	sgtu r13, r6, r13
	add r6, r10, r0
	bne r13, r8, .LBB2_10
.LBB2_11:
	add r3, r4, r5
	addi r2, r0, 0
	stb r3+0, r2
	addi r3, r3, -1
	sgeu r3, r4, r3
	bne r3, r2, .LBB2_14
.LBB2_12:
	add r3, r4, r0
.LBB2_13:
	ldbu r6, r7+1
	ldbu r8, r3+0
	stb r7+1, r8
	stb r3+0, r6
	addi r3, r3, 1
	addi r6, r7, -1
	sltu r8, r3, r7
	add r7, r6, r0
	bne r8, r2, .LBB2_13
.LBB2_14:
	add r2, r4, r5
	addi r3, fp, -92
	sub r2, r2, r3
	beq r2, r1, .LBB2_17
.LBB2_15:
	add r4, r1, r0
.LBB2_16:
	add r5, r3, r4
	ldbu r5, r5+0
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	sltu r5, r4, r2
	bne r5, r1, .LBB2_16
.LBB2_17:
	add r11, r11, r2
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_70:
	addi r1, fp, -92
	sth r1+0, r28
	addi r1, r0, 1
	addi r2, r0, 0
	beq r1, r2, .LBB2_27
.LBB2_42:
	add r3, r2, r0
.LBB2_43:
	addi r4, fp, -92
	add r4, r4, r3
	ldbu r4, r4+0
	add r5, r11, r3
	stb r5+0, r4
	addi r3, r3, 1
	sltu r4, r3, r1
	bne r4, r2, .LBB2_43
	jal r0, .LBB2_27
.LBB2_50:
	addi r2, fp, -92
	sth r2+0, r28
	addi r2, r0, 1
.LBB2_51:
	addi r3, r0, 8
	sub r3, r3, r2
	addi r4, r0, 0
	beq r3, r4, .LBB2_54
.LBB2_52:
	add r5, r4, r0
.LBB2_53:
	add r6, r1, r5
	stb r6+0, r28
	addi r5, r5, 1
	sltu r6, r5, r3
	bne r6, r4, .LBB2_53
.LBB2_54:
	sub r1, r11, r2
	addi r1, r1, 10
	addi r3, r0, 0
	beq r2, r3, .LBB2_58
.LBB2_56:
	add r4, r3, r0
.LBB2_57:
	addi r5, fp, -92
	add r5, r5, r4
	ldbu r5, r5+0
	add r6, r1, r4
	stb r6+0, r5
	addi r4, r4, 1
	sltu r5, r4, r2
	bne r5, r3, .LBB2_57
.LBB2_58:
	add r11, r1, r2
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_68:
	addi r1, fp, -92
	sth r1+0, r28
	addi r1, r0, 1
	addi r2, r0, 0
	beq r1, r2, .LBB2_27
.LBB2_25:
	add r3, r2, r0
.LBB2_26:
	addi r4, fp, -92
	add r4, r4, r3
	ldbu r4, r4+0
	add r5, r11, r3
	stb r5+0, r4
	addi r3, r3, 1
	sltu r4, r3, r1
	bne r4, r2, .LBB2_26
	jal r0, .LBB2_27
.LBB2_66:
	stb r11+0, r2
	ldw r1, fp+-116
	sub r1, r11, r1
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
.Lfunc_end2:
	.size	vsprintf, .Lfunc_end2-vsprintf
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI2_0:
	.word	.LBB2_63
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_28
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_59
	.word	.LBB2_6
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_6
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_36
	.word	.LBB2_44
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_60
	.word	.LBB2_65
	.word	.LBB2_18
	.word	.LBB2_65
	.word	.LBB2_65
	.word	.LBB2_28
                                        # -- End function
	.text
	.globl	sprintf                         # -- Begin function sprintf
	.p2align	2
	.type	sprintf,@function
sprintf:                                # @sprintf
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-28, lr
	addi r1, fp, -24
	stw r1+20, r10
	stw r1+16, r9
	stw r1+12, r8
	stw r1+8, r7
	stw r1+0, r5
	ori  r2, r1, 4
	stw r2+0, r6
	addi r2, fp, -32
	stw r2+0, r1
	add r5, r1, r0
	jal r31, vsprintf
	ldw lr, fp+-28
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end3:
	.size	sprintf, .Lfunc_end3-sprintf
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"(null)"
	.size	.L.str, 7

	.type	Digits16U,@object               # @Digits16U
Digits16U:
	.asciz	"0123456789ABCDEF"
	.size	Digits16U, 17

	.type	Digits16L,@object               # @Digits16L
Digits16L:
	.asciz	"0123456789abcdef"
	.size	Digits16L, 17

	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git f70c231862e530d8ddece5423fa27678d1eecb34)"
	.section	".note.GNU-stack","",@progbits
