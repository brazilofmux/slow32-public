	.file	"printf.c"
	.text
	.globl	vprintf                         # -- Begin function vprintf
	.p2align	2
	.type	vprintf,@function
vprintf:                                # @vprintf
# %bb.0:
	addi sp, sp, -88
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 88
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
	stw fp+-64, lr
	add r11, r3, r0
	addi r18, fp, -68
	stw r18+0, r4
	addi r19, r0, 0
	addi r12, r0, 37
	addi r20, r0, 83
	addi r21, r0, 2
	lui r22, %hi(.LJTI0_0)
	addi r22, r22, %lo(.LJTI0_0)
	ori r23, r0, 88
	addi r13, fp, -80
	add r14, r19, r0
	jal r0, .LBB0_1
.LBB0_44:
	add r3, r12, r0
	jal r31, putchar
	ldb r3, r11+1
	jal r31, putchar
	addi r14, r14, 2
	add r11, r11, r21
.LBB0_1:
	ldbu r1, r11+0
	beq r1, r12, .LBB0_5
.LBB0_2:
	addi r2, r0, 0
	beq r1, r2, .LBB0_45
.LBB0_3:
	slli r1, r1, 24
	srai r3, r1, 24
	jal r31, putchar
	addi r14, r14, 1
	addi r1, r0, 1
	add r11, r11, r1
	jal r0, .LBB0_1
.LBB0_5:
	ldbu r1, r11+1
	addi r2, r1, -37
	bgtu r2, r20, .LBB0_44
.LBB0_6:
	slli r2, r2, 2
	add r2, r22, r2
	ldw r2, r2+0
	jalr r31, r2, 0
.LBB0_17:
	ldw r2, r18+0
	ldw r3, r2+0
	addi r2, r2, 4
	stw r18+0, r2
	seq r5, r1, r23
	add r4, r13, r0
	jal r31, slow32_utox
	beq r1, r19, .LBB0_21
.LBB0_18:
	add r15, r1, r0
	add r16, r13, r0
	add r17, r1, r0
.LBB0_19:
	ldb r3, r16+0
	jal r31, putchar
	addi r17, r17, -1
	addi r16, r16, 1
	bne r17, r19, .LBB0_19
.LBB0_20:
	add r14, r15, r14
.LBB0_21:
	add r11, r11, r21
	jal r0, .LBB0_1
.LBB0_7:
	ldw r1, r18+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r18+0, r1
	add r4, r13, r0
	jal r31, slow32_ltoa
	beq r1, r19, .LBB0_11
.LBB0_8:
	add r15, r1, r0
	add r16, r13, r0
	add r17, r1, r0
.LBB0_9:
	ldb r3, r16+0
	jal r31, putchar
	addi r17, r17, -1
	addi r16, r16, 1
	bne r17, r19, .LBB0_9
.LBB0_10:
	add r14, r15, r14
.LBB0_11:
	add r11, r11, r21
	jal r0, .LBB0_1
.LBB0_42:
	add r3, r12, r0
	jal r0, .LBB0_43
.LBB0_22:
	ldw r1, r18+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r18+0, r1
	add r4, r13, r0
	jal r31, slow32_utoo
	beq r1, r19, .LBB0_26
.LBB0_23:
	add r15, r1, r0
	addi r16, fp, -80
	add r17, r1, r0
.LBB0_24:
	ldb r3, r16+0
	jal r31, putchar
	addi r17, r17, -1
	addi r16, r16, 1
	addi r1, r0, 0
	bne r17, r1, .LBB0_24
.LBB0_25:
	add r14, r15, r14
.LBB0_26:
	add r11, r11, r21
	jal r0, .LBB0_1
.LBB0_27:
	ldw r1, r18+0
	ldw r17, r1+0
	addi r1, r1, 4
	stw r18+0, r1
	addi r3, r0, 48
	jal r31, putchar
	addi r3, r0, 120
	jal r31, putchar
	addi r16, fp, -80
	addi r15, r0, 0
	add r3, r17, r0
	add r4, r16, r0
	add r5, r15, r0
	jal r31, slow32_utox
	add r17, r1, r0
	addi r1, r0, 7
	addi r24, r0, 8
	bgtu r17, r1, .LBB0_34
.LBB0_28:
	add r25, r24, r0
.LBB0_29:
	addi r3, r0, 48
	jal r31, putchar
	addi r25, r25, -1
	bne r17, r25, .LBB0_29
.LBB0_30:
	beq r17, r15, .LBB0_31
.LBB0_34:
	addi r14, r14, 2
	add r25, r17, r0
.LBB0_35:
	ldb r3, r16+0
	jal r31, putchar
	addi r25, r25, -1
	addi r16, r16, 1
	bne r25, r15, .LBB0_35
.LBB0_32:
	sgtu r1, r17, r24
	sub r1, r15, r1
	xori r2, r17, 8
	and r1, r2, r1
	xori r1, r1, 8
	add r14, r14, r1
.LBB0_33:
	add r11, r11, r21
	jal r0, .LBB0_1
.LBB0_36:
	ldw r1, r18+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r18+0, r1
.LBB0_43:
	jal r31, putchar
	addi r14, r14, 1
	add r11, r11, r21
	jal r0, .LBB0_1
.LBB0_12:
	ldw r1, r18+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r18+0, r1
	addi r15, fp, -80
	add r4, r15, r0
	jal r31, slow32_utoa
	addi r17, r0, 0
	beq r1, r17, .LBB0_16
.LBB0_13:
	add r16, r1, r0
	add r24, r1, r0
.LBB0_14:
	ldb r3, r15+0
	jal r31, putchar
	addi r24, r24, -1
	addi r15, r15, 1
	bne r24, r17, .LBB0_14
.LBB0_15:
	add r14, r16, r14
.LBB0_16:
	add r11, r11, r21
	jal r0, .LBB0_1
.LBB0_37:
	ldw r1, r18+0
	ldw r2, r1+0
	addi r1, r1, 4
	stw r18+0, r1
	ori r1, r0, 0
	seq r1, r2, r1
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	xor r3, r2, r3
	addi r15, r0, 0
	sub r1, r15, r1
	and r1, r3, r1
	xor r2, r2, r1
	ldbu r1, r2+0
	beq r1, r15, .LBB0_38
.LBB0_39:
	addi r16, r2, 1
.LBB0_40:
	slli r1, r1, 24
	srai r3, r1, 24
	jal r31, putchar
	addi r14, r14, 1
	ldbu r1, r16+0
	addi r16, r16, 1
	bne r1, r15, .LBB0_40
.LBB0_41:
	add r11, r11, r21
	jal r0, .LBB0_1
.LBB0_38:
	add r11, r11, r21
	jal r0, .LBB0_1
.LBB0_31:
	sub r1, r14, r17
	addi r14, r1, 10
	jal r0, .LBB0_33
.LBB0_45:
	add r1, r14, r0
	ldw lr, fp+-64
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
	addi sp, sp, 88
	jalr r0, r31, 0
.Lfunc_end0:
	.size	vprintf, .Lfunc_end0-vprintf
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI0_0:
	.word	.LBB0_42
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_17
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_36
	.word	.LBB0_7
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_7
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_22
	.word	.LBB0_27
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_37
	.word	.LBB0_44
	.word	.LBB0_12
	.word	.LBB0_44
	.word	.LBB0_44
	.word	.LBB0_17
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
	addi sp, sp, -88
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 88
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
	stw fp+-60, lr
	add r12, r4, r0
	add r11, r3, r0
	addi r18, fp, -64
	stw r18+0, r5
	addi r19, r0, 37
	addi r20, r0, 83
	lui r21, %hi(.LJTI2_0)
	addi r21, r21, %lo(.LJTI2_0)
	ori r22, r0, 88
	addi r13, fp, -76
	addi r23, r0, 48
	addi r24, r0, 120
	addi r14, r0, 0
	add r15, r3, r0
	jal r0, .LBB2_1
.LBB2_10:
	ldw r2, r18+0
	ldw r3, r2+0
	addi r2, r2, 4
	stw r18+0, r2
	seq r5, r1, r22
	add r4, r13, r0
	jal r31, slow32_utox
.LBB2_7:
	add r16, r1, r0
	add r3, r15, r0
	add r4, r13, r0
	add r5, r1, r0
	jal r31, memcpy
	add r15, r15, r16
	addi r12, r12, 2
.LBB2_1:
	ldbu r1, r12+0
	beq r1, r19, .LBB2_4
.LBB2_2:
	beq r1, r14, .LBB2_20
.LBB2_3:
	addi r12, r12, 1
	addi r2, r15, 1
	stb r15+0, r1
	add r15, r2, r0
	jal r0, .LBB2_1
.LBB2_4:
	ldbu r1, r12+1
	addi r2, r1, -37
	bgtu r2, r20, .LBB2_19
.LBB2_5:
	slli r2, r2, 2
	add r2, r21, r2
	ldw r2, r2+0
	jalr r31, r2, 0
.LBB2_6:
	ldw r1, r18+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r18+0, r1
	add r4, r13, r0
	jal r31, slow32_ltoa
	jal r0, .LBB2_7
.LBB2_17:
	addi r1, r15, 1
	stb r15+0, r19
	jal r0, .LBB2_18
.LBB2_19:
	stb r15+0, r19
	ldbu r1, r12+1
	addi r2, r15, 2
	stb r15+1, r1
	addi r12, r12, 2
	add r15, r2, r0
	jal r0, .LBB2_1
.LBB2_11:
	ldw r1, r18+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r18+0, r1
	add r4, r13, r0
	jal r31, slow32_utoo
	jal r0, .LBB2_7
.LBB2_12:
	ldw r1, r18+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r18+0, r1
	stb r15+0, r23
	addi r16, r15, 2
	stb r15+1, r24
	add r4, r13, r0
	add r5, r14, r0
	jal r31, slow32_utox
	add r17, r1, r0
	addi r1, r0, 7
	bgtu r17, r1, .LBB2_14
.LBB2_13:
	addi r1, r0, 8
	sub r5, r1, r17
	addi r4, r0, 48
	add r3, r16, r0
	jal r31, memset
	sub r1, r15, r17
	addi r16, r1, 10
.LBB2_14:
	addi r4, fp, -76
	add r3, r16, r0
	add r5, r17, r0
	jal r31, memcpy
	add r15, r16, r17
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_15:
	ldw r1, r18+0
	ldw r2, r1+0
	addi r1, r1, 4
	stw r18+0, r1
	addi r1, r15, 1
	stb r15+0, r2
.LBB2_18:
	addi r12, r12, 2
	add r15, r1, r0
	jal r0, .LBB2_1
.LBB2_8:
	ldw r1, r18+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r18+0, r1
	addi r16, fp, -76
	add r4, r16, r0
	jal r31, slow32_utoa
	jal r0, .LBB2_9
.LBB2_16:
	ldw r1, r18+0
	ldw r2, r1+0
	addi r1, r1, 4
	stw r18+0, r1
	ori r1, r0, 0
	seq r1, r2, r1
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	xor r3, r2, r3
	sub r1, r14, r1
	and r1, r3, r1
	xor r16, r2, r1
	add r3, r16, r0
	jal r31, strlen
.LBB2_9:
	add r17, r1, r0
	add r3, r15, r0
	add r4, r16, r0
	add r5, r1, r0
	jal r31, memcpy
	add r15, r15, r17
	addi r12, r12, 2
	jal r0, .LBB2_1
.LBB2_20:
	stb r15+0, r14
	sub r1, r15, r11
	ldw lr, fp+-60
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
	addi sp, sp, 88
	jalr r0, r31, 0
.Lfunc_end2:
	.size	vsprintf, .Lfunc_end2-vsprintf
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI2_0:
	.word	.LBB2_17
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_10
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_15
	.word	.LBB2_6
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_6
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_11
	.word	.LBB2_12
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_16
	.word	.LBB2_19
	.word	.LBB2_8
	.word	.LBB2_19
	.word	.LBB2_19
	.word	.LBB2_10
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
	ori  r2, r1, 4
	stw r2+0, r6
	stw r1+0, r5
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

	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git f4f5c1e34ae9343d25641431f4d691b10fec3591)"
	.section	".note.GNU-stack","",@progbits
