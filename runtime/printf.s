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
	addi fp, sp, 136
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
	stw fp+-104, r1
	addi r1, r16, -1
	stw fp+-112, r1
	addi r1, r16, -1
	stw fp+-96, r1
	addi r1, r16, -1
	stw fp+-116, r1
	addi r21, r0, 0
	addi r12, r0, 37
	addi r22, r0, 83
	addi r23, r0, 2
	ori  r1, r0, %lo(.LJTI0_0)
	lui r2, %hi(.LJTI0_0)
	add r24, r2, r1
	addi r25, r0, -4
	ori r1, r0, 88
	stw fp+-100, r1
	ori  r1, r0, %lo(Digits16L)
	lui r2, %hi(Digits16L)
	add r27, r2, r1
	ori  r1, r0, %lo(Digits16U)
	lui r2, %hi(Digits16U)
	add r1, r2, r1
	xor r28, r1, r27
	addi r18, r0, 48
	addi r1, r0, 1
	stw fp+-108, r1
	addi r17, r0, 15
	add r13, r21, r0
.LBB0_1:
	ldbu r1, r11+0
	beq r1, r12, .LBB0_61
.LBB0_2:
	addi r2, r0, 0
	beq r1, r2, .LBB0_63
.LBB0_3:
	slli r1, r1, 24
	srai r3, r1, 24
	jal r31, putchar
	addi r13, r13, 1
	addi r1, r0, 1
.LBB0_4:
	add r11, r11, r1
	jal r0, .LBB0_1
.LBB0_5:
	slli r1, r1, 2
	add r1, r24, r1
	ldw r2, r1+0
	lui r1, 13
	jalr r31, r2, 0
.LBB0_30:
	ldw r1, r15+0
	addi r1, r1, 3
	and r1, r1, r25
	addi r2, r1, 4
	stw r15+0, r2
	ldw r2, r1+0
	beq r2, r21, .LBB0_66
.LBB0_31:
	ldw r1, fp+-100
	seq r1, r3, r1
	sub r1, r21, r1
	and r1, r28, r1
	xor r3, r1, r27
	ldw r4, fp+-96
	add r14, r21, r0
.LBB0_32:
	add r1, r4, r0
	add r4, r16, r14
	andi r5, r2, 15
	add r5, r3, r5
	ldbu r5, r5+0
	stb r4+0, r5
	srli r5, r2, 4
	addi r14, r14, 1
	addi r4, r1, 1
	sgtu r6, r2, r17
	add r2, r5, r0
	bne r6, r21, .LBB0_32
.LBB0_33:
	add r2, r16, r14
	stb r2+0, r21
	addi r2, r2, -1
	sgeu r2, r16, r2
	beq r2, r21, .LBB0_35
.LBB0_34:
	jal r0, .LBB0_37
	jal r0, .LBB0_37
.LBB0_61:
	ldbu r3, r11+1
	addi r1, r3, -37
	sgtu r2, r1, r22
	beq r2, r21, .LBB0_5
.LBB0_62:
	add r3, r12, r0
	jal r31, putchar
	ldb r3, r11+1
	jal r31, putchar
	addi r13, r13, 2
	add r11, r11, r23
	jal r0, .LBB0_1
.LBB0_6:
	ldw r2, r15+0
	addi r2, r2, 3
	and r2, r2, r25
	addi r3, r2, 4
	stw r15+0, r3
	ldw r4, r2+0
	ori r2, r0, -1
	sgt r2, r4, r2
	addi r14, r0, 0
	beq r2, r14, .LBB0_7
.LBB0_8:
	addi r2, fp, -92
	beq r4, r14, .LBB0_64
	jal r0, .LBB0_9
.LBB0_64:
	sth r2+0, r18
	addi r3, r0, 1
	jal r0, .LBB0_15
.LBB0_40:
	ldw r1, r15+0
	addi r1, r1, 3
	and r1, r1, r25
	addi r2, r1, 4
	stw r15+0, r2
	ldw r20, r1+0
	addi r14, r0, 48
	add r3, r14, r0
	jal r31, putchar
	addi r3, r0, 120
	jal r31, putchar
	addi r3, r0, 0
	beq r20, r3, .LBB0_47
.LBB0_41:
	ldw r4, fp+-112
	add r14, r3, r0
.LBB0_42:
	add r1, r4, r0
	addi r2, fp, -92
	add r4, r2, r14
	andi r5, r20, 15
	add r5, r5, r27
	ldbu r5, r5+0
	stb r4+0, r5
	srli r5, r20, 4
	addi r14, r14, 1
	addi r4, r1, 1
	sgtu r6, r20, r17
	add r20, r5, r0
	bne r6, r3, .LBB0_42
.LBB0_43:
	add r4, r2, r14
	addi r3, r0, 0
	stb r4+0, r3
	addi r4, r4, -1
	sgeu r4, r2, r4
	beq r4, r3, .LBB0_44
.LBB0_44:
	ldbu r4, r1+1
	ldbu r5, r2+0
	stb r1+1, r5
	stb r2+0, r4
	addi r2, r2, 1
	addi r4, r1, -1
	sltu r5, r2, r1
	add r1, r4, r0
	bne r5, r3, .LBB0_44
.LBB0_45:
	addi r1, r0, 7
	sgtu r1, r14, r1
	beq r1, r3, .LBB0_48
.LBB0_46:
	addi r19, fp, -92
	add r20, r14, r0
.LBB0_51:
	ldb r3, r19+0
	jal r31, putchar
	addi r20, r20, -1
	addi r19, r19, 1
	addi r1, r0, 0
	beq r20, r1, .LBB0_52
	jal r0, .LBB0_51
.LBB0_52:
	addi r2, r0, 8
	sgtu r2, r14, r2
	sub r1, r1, r2
	xori r2, r14, 8
	and r1, r2, r1
	xori r1, r1, 8
	add r1, r13, r1
	addi r13, r1, 2
	add r11, r11, r23
	jal r0, .LBB0_1
.LBB0_54:
	ldw r1, r15+0
	addi r1, r1, 3
	and r1, r1, r25
	addi r2, r1, 4
	stw r15+0, r2
	ldw r1, r1+0
	ori r2, r0, 0
	seq r2, r1, r2
	ori  r3, r0, %lo(.L.str)
	lui r4, %hi(.L.str)
	add r3, r4, r3
	xor r3, r1, r3
	addi r14, r0, 0
	sub r2, r14, r2
	and r2, r3, r2
	xor r2, r1, r2
	ldbu r1, r2+0
	beq r1, r14, .LBB0_55
.LBB0_56:
	addi r20, r2, 1
.LBB0_57:
	slli r1, r1, 24
	srai r3, r1, 24
	jal r31, putchar
	addi r13, r13, 1
	ldbu r1, r20+0
	addi r20, r20, 1
	beq r1, r14, .LBB0_58
	jal r0, .LBB0_57
.LBB0_58:
	add r11, r11, r23
	jal r0, .LBB0_1
.LBB0_53:
	ldw r1, r15+0
	addi r1, r1, 3
	and r1, r1, r25
	addi r2, r1, 4
	stw r15+0, r2
	ldw r3, r1+0
	jal r0, .LBB0_60
.LBB0_59:
	add r3, r12, r0
.LBB0_60:
	jal r31, putchar
	addi r13, r13, 1
	add r11, r11, r23
	jal r0, .LBB0_1
.LBB0_21:
	ldw r2, r15+0
	addi r2, r2, 3
	and r2, r2, r25
	addi r3, r2, 4
	stw r15+0, r3
	ldw r5, r2+0
	addi r4, r0, 0
	beq r5, r4, .LBB0_65
.LBB0_22:
	ldw r6, fp+-116
	add r14, r4, r0
.LBB0_23:
	add r2, r6, r0
	addi r3, fp, -92
	add r6, r3, r14
	srli r7, r5, 16
	addi r8, r1, 3277
	mul r9, r7, r8
	andi r10, r5, 65535
	addi r19, r1, 3276
	mul r20, r10, r19
	add r9, r20, r9
	mul r8, r10, r8
	srli r8, r8, 16
	add r8, r9, r8
	srli r8, r8, 16
	mul r7, r7, r19
	add r7, r7, r8
	srli r7, r7, 3
	addi r8, r0, 10
	mul r8, r7, r8
	sub r8, r5, r8
	ori  r8, r8, 48
	stb r6+0, r8
	addi r14, r14, 1
	addi r6, r2, 1
	addi r8, r0, 9
	sgtu r8, r5, r8
	add r5, r7, r0
	bne r8, r4, .LBB0_23
.LBB0_24:
	add r4, r3, r14
	addi r1, r0, 0
	stb r4+0, r1
	addi r4, r4, -1
	sgeu r4, r3, r4
	beq r4, r1, .LBB0_26
.LBB0_25:
	jal r0, .LBB0_27
	jal r0, .LBB0_27
.LBB0_26:
	ldbu r4, r2+1
	ldbu r5, r3+0
	stb r2+1, r5
	stb r3+0, r4
	addi r3, r3, 1
	addi r4, r2, -1
	sltu r5, r3, r2
	add r2, r4, r0
	bne r5, r1, .LBB0_26
	jal r0, .LBB0_27
.LBB0_7:
	addi r2, r0, 45
	addi r3, fp, -92
	stb r3+0, r2
	addi r2, r0, 0
	sub r4, r2, r4
	ldw r2, fp+-104
.LBB0_9:
	addi r6, r2, -1
	add r3, r14, r0
.LBB0_10:
	add r5, r6, r0
	add r6, r2, r3
	srli r7, r4, 16
	addi r8, r1, 3277
	mul r9, r7, r8
	andi r10, r4, 65535
	addi r19, r1, 3276
	mul r20, r10, r19
	add r9, r20, r9
	mul r8, r10, r8
	srli r8, r8, 16
	add r8, r9, r8
	srli r8, r8, 16
	mul r7, r7, r19
	add r7, r7, r8
	srli r7, r7, 3
	addi r8, r0, 10
	mul r8, r7, r8
	sub r8, r4, r8
	ori  r8, r8, 48
	stb r6+0, r8
	addi r3, r3, 1
	addi r6, r5, 1
	addi r8, r0, 9
	sgtu r8, r4, r8
	add r4, r7, r0
	bne r8, r14, .LBB0_10
.LBB0_11:
	add r4, r2, r3
	addi r1, r0, 0
	stb r4+0, r1
	addi r4, r4, -1
	sgeu r4, r2, r4
	beq r4, r1, .LBB0_13
.LBB0_12:
	jal r0, .LBB0_15
	jal r0, .LBB0_15
.LBB0_13:
	add r4, r2, r0
.LBB0_14:
	ldbu r6, r5+1
	ldbu r7, r4+0
	stb r5+1, r7
	stb r4+0, r6
	addi r4, r4, 1
	addi r6, r5, -1
	sltu r7, r4, r5
	add r5, r6, r0
	bne r7, r1, .LBB0_14
.LBB0_15:
	add r1, r2, r3
	addi r20, fp, -92
	beq r1, r20, .LBB0_16
.LBB0_19:
	sub r26, r1, r20
	add r19, r26, r0
.LBB0_20:
	ldb r3, r20+0
	jal r31, putchar
	addi r19, r19, -1
	addi r20, r20, 1
	beq r19, r14, .LBB0_17
	jal r0, .LBB0_20
.LBB0_17:
	add r13, r13, r26
.LBB0_18:
	add r11, r11, r23
	jal r0, .LBB0_1
.LBB0_16:
	jal r0, .LBB0_18
	jal r0, .LBB0_18
.LBB0_66:
	sth r16+0, r18
	ldw r14, fp+-108
	jal r0, .LBB0_37
.LBB0_35:
	addi r2, fp, -92
.LBB0_36:
	ldbu r3, r1+1
	ldbu r4, r2+0
	stb r1+1, r4
	stb r2+0, r3
	addi r2, r2, 1
	addi r3, r1, -1
	sltu r4, r2, r1
	addi r5, r0, 0
	add r1, r3, r0
	bne r4, r5, .LBB0_36
.LBB0_37:
	add r19, r16, r0
	add r20, r14, r0
.LBB0_38:
	ldb r3, r19+0
	jal r31, putchar
	addi r20, r20, -1
	addi r19, r19, 1
	beq r20, r21, .LBB0_39
	jal r0, .LBB0_38
.LBB0_39:
	jal r0, .LBB0_29
	jal r0, .LBB0_29
.LBB0_47:
	addi r1, fp, -92
	sth r1+0, r14
	addi r14, r0, 1
.LBB0_48:
	addi r19, r0, 8
.LBB0_49:
	addi r3, r0, 48
	jal r31, putchar
	addi r19, r19, -1
	beq r14, r19, .LBB0_50
	jal r0, .LBB0_49
.LBB0_50:
	jal r0, .LBB0_46
	jal r0, .LBB0_46
.LBB0_55:
	add r11, r11, r23
	jal r0, .LBB0_1
.LBB0_65:
	addi r1, fp, -92
	sth r1+0, r18
	addi r14, r0, 1
.LBB0_27:
	addi r19, fp, -92
	add r20, r14, r0
.LBB0_28:
	ldb r3, r19+0
	jal r31, putchar
	addi r20, r20, -1
	addi r19, r19, 1
	addi r1, r0, 0
	beq r20, r1, .LBB0_29
	jal r0, .LBB0_28
.LBB0_29:
	add r13, r13, r14
	add r11, r11, r23
	jal r0, .LBB0_1
.LBB0_63:
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
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_30
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_53
	.word	.LBB0_6
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_6
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_40
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_54
	.word	.LBB0_62
	.word	.LBB0_21
	.word	.LBB0_62
	.word	.LBB0_62
	.word	.LBB0_30
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
	addi fp, sp, 56
	stw fp+-32, lr
	addi r1, fp, -28
	stw r1+0, r4
	stw r1+4, r5
	stw r1+8, r6
	stw r1+12, r7
	stw r1+16, r8
	stw r1+20, r9
	stw r1+24, r10
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
	addi sp, sp, -120
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 120
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
	addi r18, r15, -1
	addi r1, r15, -1
	stw fp+-108, r1
	addi r20, r0, 37
	addi r21, r0, 83
	addi r22, r0, 0
	ori  r1, r0, %lo(.LJTI2_0)
	lui r2, %hi(.LJTI2_0)
	add r23, r2, r1
	addi r24, r0, -4
	ori r25, r0, 88
	ori  r1, r0, %lo(Digits16L)
	lui r2, %hi(Digits16L)
	add r26, r2, r1
	ori  r1, r0, %lo(Digits16U)
	lui r2, %hi(Digits16U)
	add r1, r2, r1
	xor r27, r1, r26
	addi r28, r0, 48
	addi r1, r0, 1
	stw fp+-100, r1
	addi r19, r0, 15
	stw fp+-112, r3
.LBB2_1:
	ldbu r1, r12+0
	beq r1, r20, .LBB2_62
.LBB2_2:
	addi r2, r0, 0
	beq r1, r2, .LBB2_65
.LBB2_3:
	addi r12, r12, 1
	addi r2, r11, 1
	stb r11+0, r1
	add r11, r2, r0
	jal r0, .LBB2_1
.LBB2_4:
	slli r1, r1, 2
	add r1, r23, r1
	ldw r3, r1+0
	lui r1, 13
	jalr r31, r3, 0
.LBB2_29:
	ldw r1, r14+0
	addi r1, r1, 3
	and r1, r1, r24
	addi r3, r1, 4
	stw r14+0, r3
	ldw r3, r1+0
	beq r3, r22, .LBB2_68
.LBB2_30:
	seq r1, r2, r25
	sub r1, r22, r1
	and r1, r27, r1
	xor r4, r1, r26
	add r5, r18, r0
	add r1, r22, r0
.LBB2_31:
	add r2, r5, r0
	add r5, r15, r1
	andi r6, r3, 15
	add r6, r4, r6
	ldbu r6, r6+0
	stb r5+0, r6
	srli r6, r3, 4
	addi r1, r1, 1
	addi r5, r2, 1
	sgtu r7, r3, r19
	add r3, r6, r0
	bne r7, r22, .LBB2_31
.LBB2_32:
	add r3, r15, r1
	stb r3+0, r22
	addi r3, r3, -1
	sgeu r3, r15, r3
	beq r3, r22, .LBB2_34
.LBB2_33:
	jal r0, .LBB2_36
	jal r0, .LBB2_36
.LBB2_62:
	ldbu r2, r12+1
	addi r1, r2, -37
	sgtu r3, r1, r21
	beq r3, r22, .LBB2_4
.LBB2_63:
	stb r11+0, r20
	ldbu r1, r12+1
	addi r2, r11, 2
	stb r11+1, r1
	add r11, r2, r0
.LBB2_64:
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_5:
	ldw r2, r14+0
	addi r2, r2, 3
	and r2, r2, r24
	addi r3, r2, 4
	stw r14+0, r3
	ldw r5, r2+0
	ori r2, r0, -1
	sgt r3, r5, r2
	addi r2, r0, 0
	beq r3, r2, .LBB2_6
.LBB2_7:
	addi r3, fp, -92
	beq r5, r2, .LBB2_66
	jal r0, .LBB2_8
.LBB2_66:
	sth r3+0, r28
	addi r4, r0, 1
	jal r0, .LBB2_14
.LBB2_40:
	ldw r1, r14+0
	addi r1, r1, 3
	and r1, r1, r24
	addi r2, r1, 4
	stw r14+0, r2
	ldw r5, r1+0
	stb r11+0, r28
	addi r1, r11, 2
	addi r2, r0, 120
	stb r11+1, r2
	addi r6, r0, 0
	beq r5, r6, .LBB2_47
.LBB2_41:
	ldw r7, fp+-104
	add r2, r6, r0
.LBB2_42:
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
	sgtu r9, r5, r19
	add r5, r8, r0
	bne r9, r6, .LBB2_42
.LBB2_43:
	add r6, r4, r2
	addi r5, r0, 0
	stb r6+0, r5
	addi r6, r6, -1
	sgeu r6, r4, r6
	beq r6, r5, .LBB2_44
.LBB2_44:
	ldbu r6, r3+1
	ldbu r7, r4+0
	stb r3+1, r7
	stb r4+0, r6
	addi r4, r4, 1
	addi r6, r3, -1
	sltu r7, r4, r3
	add r3, r6, r0
	bne r7, r5, .LBB2_44
.LBB2_45:
	addi r3, r0, 7
	sgtu r3, r2, r3
	beq r3, r5, .LBB2_48
.LBB2_46:
	jal r0, .LBB2_52
	jal r0, .LBB2_52
.LBB2_57:
	ldw r1, r14+0
	addi r1, r1, 3
	and r1, r1, r24
	addi r2, r1, 4
	stw r14+0, r2
	ldw r1, r1+0
	ori r2, r0, 0
	seq r2, r1, r2
	ori  r3, r0, %lo(.L.str)
	lui r4, %hi(.L.str)
	add r3, r4, r3
	xor r3, r1, r3
	addi r16, r0, 0
	sub r2, r16, r2
	and r2, r3, r2
	xor r13, r1, r2
	add r3, r13, r0
	jal r31, strlen
	beq r1, r16, .LBB2_60
.LBB2_58:
	add r2, r16, r0
.LBB2_59:
	add r3, r13, r2
	ldbu r3, r3+0
	add r4, r11, r2
	stb r4+0, r3
	addi r2, r2, 1
	sltu r3, r2, r1
	bne r3, r16, .LBB2_59
.LBB2_60:
	add r11, r11, r1
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_61:
	addi r1, r11, 1
	stb r11+0, r20
	add r11, r1, r0
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_56:
	ldw r1, r14+0
	addi r1, r1, 3
	and r1, r1, r24
	addi r2, r1, 4
	stw r14+0, r2
	ldw r1, r1+0
	addi r2, r11, 1
	stb r11+0, r1
	add r11, r2, r0
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_18:
	ldw r2, r14+0
	addi r2, r2, 3
	and r2, r2, r24
	addi r3, r2, 4
	stw r14+0, r3
	ldw r5, r2+0
	addi r4, r0, 0
	beq r5, r4, .LBB2_67
.LBB2_19:
	ldw r6, fp+-108
	add r2, r4, r0
.LBB2_20:
	add r3, r6, r0
	add r6, r15, r2
	srli r7, r5, 16
	addi r8, r1, 3277
	mul r9, r7, r8
	andi r10, r5, 65535
	addi r13, r1, 3276
	mul r16, r10, r13
	add r9, r16, r9
	mul r8, r10, r8
	srli r8, r8, 16
	add r8, r9, r8
	srli r8, r8, 16
	mul r7, r7, r13
	add r7, r7, r8
	srli r7, r7, 3
	addi r8, r0, 10
	mul r8, r7, r8
	sub r8, r5, r8
	ori  r8, r8, 48
	stb r6+0, r8
	addi r2, r2, 1
	addi r6, r3, 1
	addi r8, r0, 9
	sgtu r8, r5, r8
	add r5, r7, r0
	bne r8, r4, .LBB2_20
.LBB2_21:
	add r4, r15, r2
	addi r1, r0, 0
	stb r4+0, r1
	addi r4, r4, -1
	sgeu r4, r15, r4
	beq r4, r1, .LBB2_23
.LBB2_22:
	jal r0, .LBB2_25
	jal r0, .LBB2_25
.LBB2_6:
	addi r3, r0, 45
	addi r4, fp, -92
	stb r4+0, r3
	addi r3, r0, 0
	sub r5, r3, r5
	ldw r3, fp+-96
.LBB2_8:
	addi r8, r3, -1
	addi r7, r0, 0
	add r4, r7, r0
.LBB2_9:
	add r6, r8, r0
	add r8, r3, r4
	srli r9, r5, 16
	addi r10, r1, 3277
	mul r13, r9, r10
	andi r16, r5, 65535
	addi lr, r1, 3276
	mul r17, r16, lr
	add r13, r17, r13
	mul r10, r16, r10
	srli r10, r10, 16
	add r10, r13, r10
	srli r10, r10, 16
	mul r9, r9, lr
	add r9, r9, r10
	srli r9, r9, 3
	addi r10, r0, 10
	mul r10, r9, r10
	sub r10, r5, r10
	ori  r10, r10, 48
	stb r8+0, r10
	addi r4, r4, 1
	addi r8, r6, 1
	addi r10, r0, 9
	sgtu r10, r5, r10
	add r5, r9, r0
	bne r10, r7, .LBB2_9
.LBB2_10:
	add r5, r3, r4
	addi r1, r0, 0
	stb r5+0, r1
	addi r5, r5, -1
	sgeu r5, r3, r5
	beq r5, r1, .LBB2_12
.LBB2_11:
	jal r0, .LBB2_14
	jal r0, .LBB2_14
.LBB2_12:
	add r5, r3, r0
.LBB2_13:
	ldbu r7, r6+1
	ldbu r8, r5+0
	stb r6+1, r8
	stb r5+0, r7
	addi r5, r5, 1
	addi r7, r6, -1
	sltu r8, r5, r6
	add r6, r7, r0
	bne r8, r1, .LBB2_13
.LBB2_14:
	add r1, r3, r4
	addi r3, fp, -92
	sub r1, r1, r3
	beq r1, r2, .LBB2_17
.LBB2_15:
	add r4, r2, r0
.LBB2_16:
	add r5, r3, r4
	ldbu r5, r5+0
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	sltu r5, r4, r1
	bne r5, r2, .LBB2_16
.LBB2_17:
	add r11, r11, r1
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_68:
	sth r15+0, r28
	ldw r1, fp+-100
	jal r0, .LBB2_36
.LBB2_34:
	addi r3, fp, -92
.LBB2_35:
	ldbu r4, r2+1
	ldbu r5, r3+0
	stb r2+1, r5
	stb r3+0, r4
	addi r3, r3, 1
	addi r4, r2, -1
	sltu r5, r3, r2
	addi r6, r0, 0
	add r2, r4, r0
	bne r5, r6, .LBB2_35
.LBB2_36:
	beq r1, r22, .LBB2_39
	beq r1, r22, .LBB2_39
.LBB2_37:
	add r2, r22, r0
.LBB2_38:
	add r3, r15, r2
	ldbu r3, r3+0
	add r4, r11, r2
	stb r4+0, r3
	addi r2, r2, 1
	sltu r3, r2, r1
	bne r3, r22, .LBB2_38
.LBB2_39:
	add r11, r11, r1
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_47:
	addi r2, fp, -92
	sth r2+0, r28
	addi r2, r0, 1
.LBB2_48:
	addi r3, r0, 8
	sub r3, r3, r2
	addi r4, r0, 0
	beq r3, r4, .LBB2_51
.LBB2_49:
	add r5, r4, r0
.LBB2_50:
	add r6, r1, r5
	stb r6+0, r28
	addi r5, r5, 1
	sltu r6, r5, r3
	bne r6, r4, .LBB2_50
.LBB2_51:
	sub r1, r11, r2
	addi r1, r1, 10
.LBB2_52:
	addi r3, r0, 0
	beq r2, r3, .LBB2_55
.LBB2_53:
	add r4, r3, r0
.LBB2_54:
	addi r5, fp, -92
	add r5, r5, r4
	ldbu r5, r5+0
	add r6, r1, r4
	stb r6+0, r5
	addi r4, r4, 1
	sltu r5, r4, r2
	bne r5, r3, .LBB2_54
.LBB2_55:
	add r11, r1, r2
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_67:
	addi r1, fp, -92
	sth r1+0, r28
	addi r2, r0, 1
	jal r0, .LBB2_25
.LBB2_23:
	add r4, r15, r0
.LBB2_24:
	ldbu r5, r3+1
	ldbu r6, r4+0
	stb r3+1, r6
	stb r4+0, r5
	addi r4, r4, 1
	addi r5, r3, -1
	sltu r6, r4, r3
	add r3, r5, r0
	bne r6, r1, .LBB2_24
.LBB2_25:
	addi r1, r0, 0
	beq r2, r1, .LBB2_28
.LBB2_26:
	add r3, r1, r0
.LBB2_27:
	addi r4, fp, -92
	add r4, r4, r3
	ldbu r4, r4+0
	add r5, r11, r3
	stb r5+0, r4
	addi r3, r3, 1
	sltu r4, r3, r2
	bne r4, r1, .LBB2_27
.LBB2_28:
	add r11, r11, r2
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_65:
	stb r11+0, r2
	ldw r1, fp+-112
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
	addi sp, sp, 120
	jalr r0, r31, 0
.Lfunc_end2:
	.size	vsprintf, .Lfunc_end2-vsprintf
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI2_0:
	.word	.LBB2_61
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_29
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_56
	.word	.LBB2_5
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_5
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_40
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_57
	.word	.LBB2_63
	.word	.LBB2_18
	.word	.LBB2_63
	.word	.LBB2_63
	.word	.LBB2_29
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
	addi fp, sp, 40
	stw fp+-28, lr
	addi r1, fp, -24
	stw r1+0, r5
	ori  r2, r1, 4
	stw r2+0, r6
	stw r1+8, r7
	stw r1+12, r8
	stw r1+16, r9
	stw r1+20, r10
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

	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
