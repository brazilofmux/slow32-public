	.file	"lundump.c"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaU_undump
.LCPI0_0:
	.quad	0x4077280000000000              # double 370.5
	.text
	.hidden	luaU_undump
	.globl	luaU_undump
	.p2align	2
	.type	luaU_undump,@function
luaU_undump:                            # @luaU_undump
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 56
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, lr
	add r12, r4, r0
	add r11, r3, r0
	ldbu r1, r5+0
	addi r3, r0, 27
	beq r1, r3, .LBB0_4
.LBB0_1:
	addi r3, r0, 64
	beq r1, r3, .LBB0_3
.LBB0_2:
	addi r3, r0, 61
	bne r1, r3, .LBB0_5
.LBB0_3:
	addi r5, r5, 1
	jal r0, .LBB0_5
.LBB0_4:
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
.LBB0_5:
	addi r1, fp, -48
	stw r1+8, r5
	stw r1+0, r11
	stw r1+4, r12
	lui r3, %hi(.L.str+1)
	addi r3, r3, %lo(.L.str+1)
	jal r31, strlen
	add r13, r1, r0
	addi r4, fp, -36
	add r3, r12, r0
	add r5, r1, r0
	jal r31, luaZ_read
	addi r14, r0, 0
	bne r1, r14, .LBB0_40
.LBB0_6:
	lui r3, %hi(.L.str+1)
	addi r3, r3, %lo(.L.str+1)
	addi r4, fp, -36
	add r5, r13, r0
	jal r31, memcmp
	bne r1, r14, .LBB0_41
.LBB0_7:
	ldw r1, r12+0
	addi r3, r1, -1
	stw r12+0, r3
	beq r1, r14, .LBB0_9
.LBB0_8:
	ldw r1, r12+4
	addi r3, r1, 1
	stw r12+4, r3
	ldbu r1, r1+0
	addi r15, r0, -1
	bne r1, r15, .LBB0_10
	jal r0, .LBB0_40
.LBB0_9:
	add r3, r12, r0
	jal r31, luaZ_fill
	addi r15, r0, -1
	beq r1, r15, .LBB0_40
.LBB0_10:
	andi r1, r1, 255
	addi r3, r0, 84
	bne r1, r3, .LBB0_42
.LBB0_11:
	ldw r1, r12+0
	addi r3, r1, -1
	stw r12+0, r3
	beq r1, r14, .LBB0_13
.LBB0_12:
	ldw r1, r12+4
	addi r3, r1, 1
	stw r12+4, r3
	ldbu r1, r1+0
	bne r1, r15, .LBB0_14
	jal r0, .LBB0_40
.LBB0_13:
	add r3, r12, r0
	jal r31, luaZ_fill
	beq r1, r15, .LBB0_40
.LBB0_14:
	andi r1, r1, 255
	bne r1, r14, .LBB0_43
.LBB0_15:
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	jal r31, strlen
	add r13, r1, r0
	addi r4, fp, -36
	add r3, r12, r0
	add r5, r1, r0
	jal r31, luaZ_read
	bne r1, r14, .LBB0_40
.LBB0_16:
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	addi r4, fp, -36
	add r5, r13, r0
	jal r31, memcmp
	bne r1, r14, .LBB0_44
.LBB0_17:
	ldw r1, r12+0
	addi r3, r1, -1
	stw r12+0, r3
	beq r1, r14, .LBB0_19
.LBB0_18:
	ldw r1, r12+4
	addi r3, r1, 1
	stw r12+4, r3
	ldbu r1, r1+0
	bne r1, r15, .LBB0_20
	jal r0, .LBB0_40
.LBB0_19:
	add r3, r12, r0
	jal r31, luaZ_fill
	beq r1, r15, .LBB0_40
.LBB0_20:
	andi r1, r1, 255
	addi r13, r0, 4
	bne r1, r13, .LBB0_45
.LBB0_21:
	ldw r1, r12+0
	addi r3, r1, -1
	stw r12+0, r3
	beq r1, r14, .LBB0_23
.LBB0_22:
	ldw r1, r12+4
	addi r3, r1, 1
	stw r12+4, r3
	ldbu r1, r1+0
	bne r1, r15, .LBB0_24
	jal r0, .LBB0_40
.LBB0_23:
	add r3, r12, r0
	jal r31, luaZ_fill
	beq r1, r15, .LBB0_40
.LBB0_24:
	andi r1, r1, 255
	bne r1, r13, .LBB0_46
.LBB0_25:
	ldw r1, r12+0
	addi r3, r1, -1
	stw r12+0, r3
	beq r1, r14, .LBB0_27
.LBB0_26:
	ldw r1, r12+4
	addi r3, r1, 1
	stw r12+4, r3
	ldbu r1, r1+0
	bne r1, r15, .LBB0_28
	jal r0, .LBB0_40
.LBB0_27:
	add r3, r12, r0
	jal r31, luaZ_fill
	beq r1, r15, .LBB0_40
.LBB0_28:
	andi r1, r1, 255
	addi r3, r0, 8
	bne r1, r3, .LBB0_47
.LBB0_29:
	addi r13, fp, -36
	addi r5, r0, 4
	add r3, r12, r0
	add r4, r13, r0
	jal r31, luaZ_read
	bne r1, r14, .LBB0_40
.LBB0_30:
	ldw r1, r13+0
	lui r3, 5
	addi r3, r3, 1656
	bne r1, r3, .LBB0_49
.LBB0_31:
	addi r5, r0, 8
	add r3, r12, r0
	add r4, r13, r0
	jal r31, luaZ_read
	bne r1, r14, .LBB0_40
.LBB0_32:
	ldw r5, r13+4
	ldw r4, r13+0
	lui r1, %hi(.LCPI0_0)
	addi r1, r1, %lo(.LCPI0_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r4, r6
	beq r1, r14, .LBB0_50
.LBB0_33:
	ldw r1, r12+0
	addi r3, r1, -1
	stw r12+0, r3
	beq r1, r14, .LBB0_39
.LBB0_34:
	ldw r1, r12+4
	addi r3, r1, 1
	stw r12+4, r3
	ldbu r1, r1+0
	beq r1, r15, .LBB0_40
.LBB0_35:
	andi r4, r1, 255
	add r3, r11, r0
	jal r31, luaF_newLclosure
	add r12, r1, r0
	ldw r1, r11+12
	stw r1+0, r12
	addi r3, r0, 70
	stb r1+8, r3
	add r3, r11, r0
	jal r31, luaD_inctop
	add r3, r11, r0
	jal r31, luaF_newproto
	stw r12+12, r1
	ldbu r3, r12+5
	andi r3, r3, 32
	beq r3, r14, .LBB0_38
.LBB0_36:
	ldbu r3, r1+5
	andi r3, r3, 24
	beq r3, r14, .LBB0_38
.LBB0_37:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r1, r0
	jal r31, luaC_barrier_
.LBB0_38:
	ldw r4, r12+12
	addi r3, fp, -48
	addi r5, r0, 0
	jal r31, loadFunction
	add r1, r12, r0
	ldw lr, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.LBB0_39:
	add r3, r12, r0
	jal r31, luaZ_fill
	bne r1, r15, .LBB0_35
.LBB0_40:
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
	addi r3, fp, -48
	jal r31, error
.LBB0_41:
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	addi r3, fp, -48
	jal r31, error
.LBB0_42:
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	addi r3, fp, -48
	jal r31, error
.LBB0_43:
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	addi r3, fp, -48
	jal r31, error
.LBB0_44:
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	addi r3, fp, -48
	jal r31, error
.LBB0_45:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	lui r5, %hi(.L.str.7)
	addi r5, r5, %lo(.L.str.7)
	jal r0, .LBB0_48
.LBB0_46:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	jal r0, .LBB0_48
.LBB0_47:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
.LBB0_48:
	add r3, r11, r0
	jal r31, luaO_pushfstring
	addi r3, fp, -48
	add r4, r1, r0
	jal r31, error
.LBB0_49:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	addi r3, fp, -48
	jal r31, error
.LBB0_50:
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	addi r3, fp, -48
	jal r31, error
.Lfunc_end0:
	.size	luaU_undump, .Lfunc_end0-luaU_undump
                                        # -- End function
	.p2align	2                               # -- Begin function loadFunction
	.type	loadFunction,@function
loadFunction:                           # @loadFunction
# %bb.0:
	addi sp, sp, -104
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 104
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
	add r13, r5, r0
	add r12, r4, r0
	add r11, r3, r0
	jal r31, loadStringN
	addi r14, r0, 0
	seq r3, r1, r14
	xor r4, r13, r1
	sub r3, r14, r3
	and r3, r4, r3
	xor r1, r1, r3
	stw r12+76, r1
	addi r18, r0, -1
	lui r1, 4096
	addi r19, r1, -1
	add r13, r14, r0
.LBB1_1:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r14, .LBB1_3
.LBB1_2:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_4
	jal r0, .LBB1_154
.LBB1_3:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_4:
	bgeu r13, r19, .LBB1_155
.LBB1_5:
	slli r3, r13, 7
	andi r4, r1, 127
	or  r13, r4, r3
	andi r1, r1, 128
	beq r1, r14, .LBB1_1
.LBB1_6:
	stw r12+40, r13
	addi r13, r0, 0
	add r14, r13, r0
.LBB1_7:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_9
.LBB1_8:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_10
	jal r0, .LBB1_154
.LBB1_9:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_10:
	bgeu r14, r19, .LBB1_155
.LBB1_11:
	slli r3, r14, 7
	andi r4, r1, 127
	or  r14, r4, r3
	andi r1, r1, 128
	beq r1, r13, .LBB1_7
.LBB1_12:
	stw r12+44, r14
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r13, r0, 0
	beq r1, r13, .LBB1_14
.LBB1_13:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_15
	jal r0, .LBB1_154
.LBB1_14:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_15:
	stb r12+6, r1
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_17
.LBB1_16:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_18
	jal r0, .LBB1_154
.LBB1_17:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_18:
	stb r12+7, r1
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_20
.LBB1_19:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_21
	jal r0, .LBB1_154
.LBB1_20:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_21:
	stb r12+8, r1
	add r15, r13, r0
.LBB1_22:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_24
.LBB1_23:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_25
	jal r0, .LBB1_154
.LBB1_24:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_25:
	bgeu r15, r19, .LBB1_155
.LBB1_26:
	slli r3, r15, 7
	andi r4, r1, 127
	or  r15, r4, r3
	andi r1, r1, 128
	beq r1, r13, .LBB1_22
.LBB1_27:
	ldw r3, r11+0
	lui r1, 262144
	addi r22, r1, -1
	bgeu r15, r22, .LBB1_156
.LBB1_28:
	slli r14, r15, 2
	addi r13, r0, 0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, luaM_malloc_
	stw r12+52, r1
	stw r12+20, r15
	ldw r3, r11+4
	add r4, r1, r0
	add r5, r14, r0
	jal r31, luaZ_read
	bne r1, r13, .LBB1_154
.LBB1_29:
	add r14, r13, r0
.LBB1_30:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_32
.LBB1_31:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_33
	jal r0, .LBB1_154
.LBB1_32:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_33:
	bgeu r14, r19, .LBB1_155
.LBB1_34:
	slli r15, r14, 7
	andi r16, r1, 127
	or  r14, r16, r15
	andi r1, r1, 128
	beq r1, r13, .LBB1_30
.LBB1_35:
	ldw r3, r11+0
	lui r1, 87381
	addi r1, r1, 1365
	stw fp+-92, r1
	bgeu r14, r1, .LBB1_156
.LBB1_36:
	addi r20, r0, 12
	mul r4, r14, r20
	addi r13, r0, 0
	add r5, r13, r0
	jal r31, luaM_malloc_
	stw r12+48, r1
	stw r12+16, r14
	beq r14, r13, .LBB1_53
.LBB1_37:
	or  r3, r15, r16
	addi r1, r1, 8
.LBB1_38:
	stb r1+0, r13
	addi r3, r3, -1
	addi r1, r1, 12
	bne r3, r13, .LBB1_38
.LBB1_39:
	or  r23, r15, r16
	addi r24, r0, 20
	lui r25, %hi(.LJTI1_0)
	addi r25, r25, %lo(.LJTI1_0)
	addi r14, fp, -80
	addi r15, r0, 4
	addi r26, r0, 3
	addi r16, fp, -88
	addi r17, r0, 8
	addi r27, r0, 19
	add r28, r13, r0
	jal r0, .LBB1_44
.LBB1_40:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, loadStringN
	beq r1, r13, .LBB1_157
.LBB1_41:
	stw r21+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
.LBB1_42:
	stb r21+8, r1
.LBB1_43:
	addi r23, r23, -1
	addi r28, r28, 12
	beq r23, r13, .LBB1_53
.LBB1_44:
	ldw r21, r12+48
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_46
.LBB1_45:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_47
	jal r0, .LBB1_154
.LBB1_46:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_47:
	andi r3, r1, 255
	bgtu r3, r24, .LBB1_43
.LBB1_48:
	add r21, r21, r28
	slli r3, r3, 2
	add r3, r25, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB1_49:
	ldw r3, r11+4
	add r4, r14, r0
	add r5, r15, r0
	jal r31, luaZ_read
	bne r1, r13, .LBB1_154
.LBB1_50:
	ldw r1, r14+0
	stw r21+0, r1
	add r1, r26, r0
	jal r0, .LBB1_42
.LBB1_51:
	ldw r3, r11+4
	add r4, r16, r0
	add r5, r17, r0
	jal r31, luaZ_read
	bne r1, r13, .LBB1_154
.LBB1_52:
	ldw r1, r16+4
	ldw r3, r16+0
	stw r21+4, r1
	stw r21+0, r3
	add r1, r27, r0
	jal r0, .LBB1_42
.LBB1_53:
	addi r13, r0, 0
	add r14, r13, r0
.LBB1_54:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_56
.LBB1_55:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_57
	jal r0, .LBB1_154
.LBB1_56:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_57:
	bgeu r14, r19, .LBB1_155
.LBB1_58:
	slli r16, r14, 7
	andi r17, r1, 127
	or  r14, r17, r16
	andi r1, r1, 128
	beq r1, r13, .LBB1_54
.LBB1_59:
	ldw r3, r11+0
	lui r1, 131072
	addi r15, r1, -1
	bgeu r14, r15, .LBB1_156
.LBB1_60:
	slli r4, r14, 3
	addi r13, r0, 0
	add r5, r13, r0
	jal r31, luaM_malloc_
	stw r12+60, r1
	stw r12+12, r14
	beq r14, r13, .LBB1_74
.LBB1_61:
	or  r3, r16, r17
.LBB1_62:
	stw r1+0, r13
	addi r3, r3, -1
	addi r1, r1, 8
	bne r3, r13, .LBB1_62
.LBB1_63:
	or  r14, r16, r17
	add r16, r13, r0
.LBB1_64:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_66
.LBB1_65:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_67
	jal r0, .LBB1_154
.LBB1_66:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_67:
	ldw r3, r12+60
	add r3, r3, r16
	stb r3+4, r1
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_69
.LBB1_68:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_70
	jal r0, .LBB1_154
.LBB1_69:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_70:
	ldw r3, r12+60
	add r3, r3, r16
	stb r3+5, r1
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_72
.LBB1_71:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_73
	jal r0, .LBB1_154
.LBB1_72:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_73:
	ldw r3, r12+60
	add r3, r3, r16
	stb r3+6, r1
	addi r14, r14, -1
	addi r16, r16, 8
	bne r14, r13, .LBB1_64
.LBB1_74:
	addi r13, r0, 0
	add r14, r13, r0
.LBB1_75:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_77
.LBB1_76:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_78
	jal r0, .LBB1_154
.LBB1_77:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_78:
	bgeu r14, r19, .LBB1_155
.LBB1_79:
	slli r16, r14, 7
	andi r17, r1, 127
	or  r14, r17, r16
	andi r1, r1, 128
	beq r1, r13, .LBB1_75
.LBB1_80:
	ldw r3, r11+0
	bgeu r14, r22, .LBB1_156
.LBB1_81:
	slli r4, r14, 2
	addi r13, r0, 0
	add r5, r13, r0
	jal r31, luaM_malloc_
	stw r12+56, r1
	stw r12+28, r14
	beq r14, r13, .LBB1_89
.LBB1_82:
	or  r3, r16, r17
.LBB1_83:
	stw r1+0, r13
	addi r3, r3, -1
	addi r1, r1, 4
	bne r3, r13, .LBB1_83
.LBB1_84:
	or  r14, r16, r17
	add r16, r13, r0
	jal r0, .LBB1_86
.LBB1_85:
	ldw r1, r12+56
	add r1, r1, r16
	ldw r4, r1+0
	ldw r5, r12+76
	add r3, r11, r0
	jal r31, loadFunction
	addi r14, r14, -1
	addi r16, r16, 4
	beq r14, r13, .LBB1_89
.LBB1_86:
	ldw r3, r11+0
	jal r31, luaF_newproto
	ldw r3, r12+56
	add r3, r3, r16
	stw r3+0, r1
	ldbu r3, r12+5
	andi r3, r3, 32
	beq r3, r13, .LBB1_85
.LBB1_87:
	ldbu r3, r1+5
	andi r3, r3, 24
	beq r3, r13, .LBB1_85
.LBB1_88:
	ldw r3, r11+0
	add r4, r12, r0
	add r5, r1, r0
	jal r31, luaC_barrier_
	jal r0, .LBB1_85
.LBB1_89:
	addi r13, r0, 0
	add r14, r13, r0
.LBB1_90:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_92
.LBB1_91:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_93
	jal r0, .LBB1_154
.LBB1_92:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_93:
	bgeu r14, r19, .LBB1_155
.LBB1_94:
	slli r3, r14, 7
	andi r4, r1, 127
	or  r14, r4, r3
	andi r1, r1, 128
	beq r1, r13, .LBB1_90
.LBB1_95:
	ldw r3, r11+0
	addi r13, r0, 0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, luaM_malloc_
	stw r12+64, r1
	stw r12+24, r14
	ldw r3, r11+4
	add r4, r1, r0
	add r5, r14, r0
	jal r31, luaZ_read
	bne r1, r13, .LBB1_154
.LBB1_96:
	add r14, r13, r0
.LBB1_97:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_99
.LBB1_98:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_100
	jal r0, .LBB1_154
.LBB1_99:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_100:
	bgeu r14, r19, .LBB1_155
.LBB1_101:
	slli r3, r14, 7
	andi r4, r1, 127
	or  r14, r4, r3
	andi r1, r1, 128
	beq r1, r13, .LBB1_97
.LBB1_102:
	ldw r3, r11+0
	bgeu r14, r15, .LBB1_156
.LBB1_103:
	slli r4, r14, 3
	addi r13, r0, 0
	add r5, r13, r0
	jal r31, luaM_malloc_
	stw r12+68, r1
	stw r12+36, r14
	beq r14, r13, .LBB1_118
.LBB1_104:
	add r15, r13, r0
.LBB1_105:
	add r17, r13, r0
.LBB1_106:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_108
.LBB1_107:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_109
	jal r0, .LBB1_154
.LBB1_108:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_109:
	bgeu r17, r19, .LBB1_155
.LBB1_110:
	slli r3, r17, 7
	andi r4, r1, 127
	or  r17, r4, r3
	andi r1, r1, 128
	beq r1, r13, .LBB1_106
.LBB1_111:
	ldw r1, r12+68
	slli r16, r15, 3
	add r1, r1, r16
	stw r1+0, r17
	add r17, r13, r0
.LBB1_112:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_114
.LBB1_113:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_115
	jal r0, .LBB1_154
.LBB1_114:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_115:
	bgeu r17, r19, .LBB1_155
.LBB1_116:
	slli r3, r17, 7
	andi r4, r1, 127
	or  r17, r4, r3
	andi r1, r1, 128
	beq r1, r13, .LBB1_112
.LBB1_117:
	ldw r1, r12+68
	add r1, r1, r16
	stw r1+4, r17
	addi r15, r15, 1
	bne r15, r14, .LBB1_105
.LBB1_118:
	addi r13, r0, 0
	add r14, r13, r0
.LBB1_119:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_121
.LBB1_120:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_122
	jal r0, .LBB1_154
.LBB1_121:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_122:
	bgeu r14, r19, .LBB1_155
.LBB1_123:
	slli r15, r14, 7
	andi r16, r1, 127
	or  r14, r16, r15
	andi r1, r1, 128
	beq r1, r13, .LBB1_119
.LBB1_124:
	ldw r3, r11+0
	ldw r1, fp+-92
	bgeu r14, r1, .LBB1_156
.LBB1_125:
	mul r4, r14, r20
	addi r13, r0, 0
	add r5, r13, r0
	jal r31, luaM_malloc_
	stw r12+72, r1
	stw r12+32, r14
	beq r14, r13, .LBB1_142
.LBB1_126:
	or  r3, r15, r16
.LBB1_127:
	stw r1+0, r13
	addi r3, r3, -1
	addi r1, r1, 12
	bne r3, r13, .LBB1_127
.LBB1_128:
	add r15, r13, r0
.LBB1_129:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, loadStringN
	ldw r3, r12+72
	mul r16, r15, r20
	add r3, r3, r16
	stw r3+0, r1
	add r17, r13, r0
.LBB1_130:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_132
.LBB1_131:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_133
	jal r0, .LBB1_154
.LBB1_132:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_133:
	bgeu r17, r19, .LBB1_155
.LBB1_134:
	slli r3, r17, 7
	andi r4, r1, 127
	or  r17, r4, r3
	andi r1, r1, 128
	beq r1, r13, .LBB1_130
.LBB1_135:
	ldw r1, r12+72
	add r1, r1, r16
	stw r1+4, r17
	add r17, r13, r0
.LBB1_136:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r13, .LBB1_138
.LBB1_137:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_139
	jal r0, .LBB1_154
.LBB1_138:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_139:
	bgeu r17, r19, .LBB1_155
.LBB1_140:
	slli r3, r17, 7
	andi r4, r1, 127
	or  r17, r4, r3
	andi r1, r1, 128
	beq r1, r13, .LBB1_136
.LBB1_141:
	ldw r1, r12+72
	add r1, r1, r16
	stw r1+8, r17
	addi r15, r15, 1
	bne r15, r14, .LBB1_129
.LBB1_142:
	addi r14, r0, 0
	add r13, r14, r0
.LBB1_143:
	ldw r3, r11+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r14, .LBB1_145
.LBB1_144:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r18, .LBB1_146
	jal r0, .LBB1_154
.LBB1_145:
	jal r31, luaZ_fill
	beq r1, r18, .LBB1_154
.LBB1_146:
	bgeu r13, r19, .LBB1_155
.LBB1_147:
	slli r3, r13, 7
	andi r4, r1, 127
	or  r13, r4, r3
	andi r1, r1, 128
	beq r1, r14, .LBB1_143
.LBB1_148:
	addi r14, r0, 0
	beq r13, r14, .LBB1_150
.LBB1_149:
	ldw r14, r12+12
.LBB1_150:
	addi r1, r0, 1
	blt r14, r1, .LBB1_153
.LBB1_151:
	addi r13, r0, 0
	add r15, r13, r0
.LBB1_152:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, loadStringN
	ldw r3, r12+60
	add r3, r3, r15
	stw r3+0, r1
	addi r14, r14, -1
	addi r15, r15, 8
	bne r14, r13, .LBB1_152
.LBB1_153:
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
	addi sp, sp, 104
	jalr r0, r31, 0
.LBB1_154:
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
	add r3, r11, r0
	jal r31, error
.LBB1_155:
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	add r3, r11, r0
	jal r31, error
.LBB1_156:
	jal r31, luaM_toobig
.LBB1_157:
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	add r3, r11, r0
	jal r31, error
.Lfunc_end1:
	.size	loadFunction, .Lfunc_end1-loadFunction
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI1_0:
	.word	.LBB1_42
	.word	.LBB1_42
	.word	.LBB1_43
	.word	.LBB1_49
	.word	.LBB1_40
	.word	.LBB1_43
	.word	.LBB1_43
	.word	.LBB1_43
	.word	.LBB1_43
	.word	.LBB1_43
	.word	.LBB1_43
	.word	.LBB1_43
	.word	.LBB1_43
	.word	.LBB1_43
	.word	.LBB1_43
	.word	.LBB1_43
	.word	.LBB1_43
	.word	.LBB1_42
	.word	.LBB1_43
	.word	.LBB1_51
	.word	.LBB1_40
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function error
	.type	error,@function
error:                                  # @error
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r6, r4, r0
	add r11, r3, r0
	ldw r3, r3+0
	ldw r5, r11+8
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	jal r31, luaO_pushfstring
	ldw r3, r11+0
	addi r4, r0, 3
	jal r31, luaD_throw
.Lfunc_end2:
	.size	error, .Lfunc_end2-error
                                        # -- End function
	.p2align	2                               # -- Begin function loadStringN
	.type	loadStringN,@function
loadStringN:                            # @loadStringN
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
	stw fp+-32, lr
	add r11, r4, r0
	add r13, r3, r0
	ldw r12, r3+0
	addi r14, r0, 0
	addi r16, r0, -1
	lui r1, 8192
	addi r17, r1, -1
	add r15, r14, r0
.LBB3_1:
	ldw r3, r13+4
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r14, .LBB3_3
.LBB3_2:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	bne r1, r16, .LBB3_4
	jal r0, .LBB3_16
.LBB3_3:
	jal r31, luaZ_fill
	beq r1, r16, .LBB3_16
.LBB3_4:
	bgeu r15, r17, .LBB3_17
.LBB3_5:
	slli r3, r15, 7
	andi r4, r1, 127
	or  r15, r4, r3
	andi r1, r1, 128
	beq r1, r14, .LBB3_1
.LBB3_6:
	addi r1, r0, 0
	beq r15, r1, .LBB3_15
.LBB3_7:
	addi r14, r15, -1
	addi r1, r0, 41
	bgtu r15, r1, .LBB3_10
.LBB3_8:
	ldw r3, r13+4
	addi r4, fp, -72
	add r5, r14, r0
	jal r31, luaZ_read
	addi r3, r0, 0
	bne r1, r3, .LBB3_16
.LBB3_9:
	addi r4, fp, -72
	add r3, r12, r0
	add r5, r14, r0
	jal r31, luaS_newlstr
	jal r0, .LBB3_12
.LBB3_10:
	add r3, r12, r0
	add r4, r14, r0
	jal r31, luaS_createlngstrobj
	add r15, r1, r0
	ldw r1, r12+12
	stw r1+0, r15
	ldbu r3, r15+4
	ori  r3, r3, 64
	stb r1+8, r3
	add r3, r12, r0
	jal r31, luaD_inctop
	add r16, r15, r0
	addi r4, r15, 16
	ldw r3, r13+4
	add r5, r14, r0
	jal r31, luaZ_read
	addi r3, r0, 0
	bne r1, r3, .LBB3_16
.LBB3_11:
	ldw r1, r12+12
	addi r1, r1, -12
	stw r12+12, r1
	add r1, r16, r0
.LBB3_12:
	ldbu r3, r11+5
	andi r4, r3, 32
	addi r3, r0, 0
	beq r4, r3, .LBB3_15
.LBB3_13:
	ldbu r4, r1+5
	andi r4, r4, 24
	beq r4, r3, .LBB3_15
.LBB3_14:
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	add r11, r1, r0
	jal r31, luaC_barrier_
	add r1, r11, r0
.LBB3_15:
	ldw lr, fp+-32
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
.LBB3_16:
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
	add r3, r13, r0
	jal r31, error
.LBB3_17:
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	add r3, r13, r0
	jal r31, error
.Lfunc_end3:
	.size	loadStringN, .Lfunc_end3-loadStringN
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\033Lua"
	.size	.L.str, 5

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"binary string"
	.size	.L.str.1, 14

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"not a binary chunk"
	.size	.L.str.2, 19

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"version mismatch"
	.size	.L.str.3, 17

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"format mismatch"
	.size	.L.str.4, 16

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"\031\223\r\n\032\n"
	.size	.L.str.5, 7

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"corrupted chunk"
	.size	.L.str.6, 16

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"Instruction"
	.size	.L.str.7, 12

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"lua_Integer"
	.size	.L.str.8, 12

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"lua_Number"
	.size	.L.str.9, 11

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"integer format mismatch"
	.size	.L.str.10, 24

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"float format mismatch"
	.size	.L.str.11, 22

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"truncated chunk"
	.size	.L.str.12, 16

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"%s: bad binary format (%s)"
	.size	.L.str.13, 27

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"%s size mismatch"
	.size	.L.str.14, 17

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"integer overflow"
	.size	.L.str.15, 17

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"bad format for constant string"
	.size	.L.str.16, 31

	.hidden	luaF_newLclosure
	.hidden	luaD_inctop
	.hidden	luaF_newproto
	.hidden	luaC_barrier_
	.hidden	luaZ_read
	.hidden	luaO_pushfstring
	.hidden	luaD_throw
	.hidden	luaZ_fill
	.hidden	luaS_newlstr
	.hidden	luaS_createlngstrobj
	.hidden	luaM_toobig
	.hidden	luaM_malloc_
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
