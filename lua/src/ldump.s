	.file	"ldump.c"
	.text
	.hidden	luaU_dump                       # -- Begin function luaU_dump
	.globl	luaU_dump
	.p2align	2
	.type	luaU_dump,@function
luaU_dump:                              # @luaU_dump
# %bb.0:
	addi sp, sp, -72
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 72
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, lr
	add r1, r5, r0
	add r12, r4, r0
	addi r11, fp, -52
	stw r11+0, r3
	stw r11+4, r5
	stw r11+8, r6
	stw r11+12, r7
	addi r14, r0, 0
	stw r11+16, r14
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	addi r13, r0, 4
	add r5, r13, r0
	jalr lr, r1, 0
	stw r11+16, r1
	addi r3, r0, 84
	addi r15, fp, -32
	stb r15+0, r3
	bne r1, r14, .LBB0_2
.LBB0_1:
	ldw r1, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	addi r4, fp, -32
	addi r5, r0, 1
	jalr lr, r1, 0
	stw r11+16, r1
.LBB0_2:
	stb r15+0, r14
	ldw r1, r11+16
	beq r1, r14, .LBB0_17
.LBB0_3:
	ldw r1, r11+16
	beq r1, r14, .LBB0_18
.LBB0_4:
	stb r15+0, r13
	ldw r1, r11+16
	bne r1, r14, .LBB0_6
.LBB0_5:
	ldw r1, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	addi r4, fp, -32
	addi r5, r0, 1
	jalr lr, r1, 0
	stw r11+16, r1
.LBB0_6:
	stb r15+0, r13
	ldw r1, r11+16
	bne r1, r14, .LBB0_8
.LBB0_7:
	ldw r1, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	addi r4, fp, -32
	addi r5, r0, 1
	jalr lr, r1, 0
	stw r11+16, r1
.LBB0_8:
	addi r1, r0, 8
	stb r15+0, r1
	ldw r1, r11+16
	bne r1, r14, .LBB0_10
.LBB0_9:
	ldw r1, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	addi r4, fp, -32
	addi r5, r0, 1
	jalr lr, r1, 0
	stw r11+16, r1
.LBB0_10:
	lui r1, 5
	addi r1, r1, 1656
	stw r15+0, r1
	ldw r1, r11+16
	bne r1, r14, .LBB0_12
.LBB0_11:
	ldw r1, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	addi r4, fp, -32
	addi r5, r0, 4
	jalr lr, r1, 0
	stw r11+16, r1
.LBB0_12:
	lui r1, 264051
	addi r1, r1, -2048
	stw r15+4, r1
	stw r15+0, r14
	ldw r1, r11+16
	bne r1, r14, .LBB0_14
.LBB0_13:
	ldw r1, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	addi r4, fp, -32
	addi r5, r0, 8
	jalr lr, r1, 0
	stw r11+16, r1
.LBB0_14:
	ldw r1, r12+12
	stb r15+0, r1
	ldw r1, r11+16
	bne r1, r14, .LBB0_16
.LBB0_15:
	ldw r1, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	addi r4, fp, -32
	addi r5, r0, 1
	jalr lr, r1, 0
	stw r11+16, r1
.LBB0_16:
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, dumpFunction
	ldw r1, r11+16
	ldw lr, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB0_17:
	ldw r1, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	addi r4, fp, -32
	addi r5, r0, 1
	jalr lr, r1, 0
	stw r11+16, r1
	ldw r1, r11+16
	bne r1, r14, .LBB0_4
.LBB0_18:
	ldw r1, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	addi r5, r0, 6
	jalr lr, r1, 0
	stw r11+16, r1
	jal r0, .LBB0_4
.Lfunc_end0:
	.size	luaU_dump, .Lfunc_end0-luaU_dump
                                        # -- End function
	.p2align	2                               # -- Begin function dumpFunction
	.type	dumpFunction,@function
dumpFunction:                           # @dumpFunction
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
	add r11, r4, r0
	add r12, r3, r0
	ldw r1, r3+12
	addi r14, r0, 0
	beq r1, r14, .LBB1_3
.LBB1_1:
	addi r1, fp, -72
	addi r3, r0, 128
	stb r1+4, r3
	ldw r3, r12+16
	addi r4, r0, 0
	bne r3, r4, .LBB1_5
.LBB1_2:
	addi r4, r1, 4
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	addi r5, r0, 1
	jalr lr, r1, 0
	stw r12+16, r1
	jal r0, .LBB1_5
.LBB1_3:
	ldw r4, r11+76
	beq r4, r5, .LBB1_1
.LBB1_4:
	add r3, r12, r0
	jal r31, dumpString
.LBB1_5:
	ldw r3, r11+40
	addi r13, fp, -72
	add r1, r14, r0
.LBB1_6:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r14, .LBB1_6
.LBB1_7:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	addi r14, r0, 0
	bne r3, r14, .LBB1_9
.LBB1_8:
	sub r5, r14, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_9:
	ldw r3, r11+44
	add r1, r14, r0
.LBB1_10:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r14, .LBB1_10
.LBB1_11:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	addi r14, r0, 0
	bne r3, r14, .LBB1_13
.LBB1_12:
	sub r5, r14, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_13:
	ldbu r1, r11+6
	stb r13+0, r1
	ldw r1, r12+16
	bne r1, r14, .LBB1_15
.LBB1_14:
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	addi r4, fp, -72
	addi r5, r0, 1
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_15:
	ldbu r1, r11+7
	stb r13+0, r1
	ldw r1, r12+16
	bne r1, r14, .LBB1_17
.LBB1_16:
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	addi r4, fp, -72
	addi r5, r0, 1
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_17:
	ldbu r1, r11+8
	stb r13+0, r1
	ldw r1, r12+16
	bne r1, r14, .LBB1_19
.LBB1_18:
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	addi r4, fp, -72
	addi r5, r0, 1
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_19:
	ldw r3, r11+20
	add r1, r14, r0
.LBB1_20:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r14, .LBB1_20
.LBB1_21:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	addi r14, r0, 0
	bne r3, r14, .LBB1_23
.LBB1_22:
	sub r5, r14, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_23:
	ldw r1, r11+20
	slli r5, r1, 2
	beq r5, r14, .LBB1_26
.LBB1_24:
	ldw r1, r12+16
	addi r3, r0, 0
	bne r1, r3, .LBB1_26
.LBB1_25:
	ldw r4, r11+52
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_26:
	ldw r18, r11+16
	add r1, r14, r0
	add r3, r18, r0
.LBB1_27:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r14, .LBB1_27
.LBB1_28:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	addi r19, r0, 0
	bne r3, r19, .LBB1_30
.LBB1_29:
	sub r5, r19, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_30:
	addi r17, r0, 1
	blt r18, r17, .LBB1_43
.LBB1_31:
	addi r20, r0, 0
	addi r14, r0, 1
	addi r21, r0, 17
	lui r22, %hi(.LJTI1_0)
	addi r22, r22, %lo(.LJTI1_0)
	addi r15, r0, 4
	addi r16, r0, 8
	add r23, r20, r0
	jal r0, .LBB1_34
.LBB1_32:
	ldw r4, r24+0
	add r3, r12, r0
	jal r31, dumpString
.LBB1_33:
	addi r18, r18, -1
	addi r23, r23, 12
	beq r18, r20, .LBB1_43
.LBB1_34:
	ldw r1, r11+48
	add r24, r1, r23
	ldbu r1, r24+8
	andi r25, r1, 63
	stb r13+0, r25
	ldw r1, r12+16
	bne r1, r20, .LBB1_36
.LBB1_35:
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	add r4, r13, r0
	add r5, r14, r0
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_36:
	addi r1, r25, -3
	bgtu r1, r21, .LBB1_33
.LBB1_37:
	slli r1, r1, 2
	add r1, r22, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB1_38:
	ldw r1, r24+0
	stw r13+0, r1
	ldw r1, r12+16
	bne r1, r20, .LBB1_33
.LBB1_39:
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	add r4, r13, r0
	add r5, r15, r0
	jal r0, .LBB1_42
.LBB1_40:
	ldw r1, r24+4
	ldw r3, r24+0
	stw r13+4, r1
	stw r13+0, r3
	ldw r1, r12+16
	bne r1, r20, .LBB1_33
.LBB1_41:
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	add r4, r13, r0
	add r5, r16, r0
.LBB1_42:
	jalr lr, r1, 0
	stw r12+16, r1
	jal r0, .LBB1_33
.LBB1_43:
	ldw r15, r11+12
	add r1, r19, r0
	add r3, r15, r0
.LBB1_44:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r19, .LBB1_44
.LBB1_45:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	addi r16, r0, 0
	bne r3, r16, .LBB1_47
.LBB1_46:
	sub r5, r16, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_47:
	blt r15, r17, .LBB1_56
.LBB1_48:
	addi r18, r0, 0
	addi r14, r0, 1
	add r19, r18, r0
	jal r0, .LBB1_50
.LBB1_49:
	addi r15, r15, -1
	addi r19, r19, 8
	beq r15, r18, .LBB1_56
.LBB1_50:
	ldw r1, r11+60
	add r1, r1, r19
	ldbu r1, r1+4
	stb r13+0, r1
	ldw r1, r12+16
	bne r1, r18, .LBB1_52
.LBB1_51:
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	add r4, r13, r0
	add r5, r14, r0
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_52:
	ldw r1, r11+60
	add r1, r1, r19
	ldbu r1, r1+5
	stb r13+0, r1
	ldw r1, r12+16
	bne r1, r18, .LBB1_54
.LBB1_53:
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	add r4, r13, r0
	add r5, r14, r0
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_54:
	ldw r1, r11+60
	add r1, r1, r19
	ldbu r1, r1+6
	stb r13+0, r1
	ldw r1, r12+16
	bne r1, r18, .LBB1_49
.LBB1_55:
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	add r4, r13, r0
	add r5, r14, r0
	jalr lr, r1, 0
	stw r12+16, r1
	jal r0, .LBB1_49
.LBB1_56:
	ldw r14, r11+28
	add r1, r16, r0
	add r3, r14, r0
.LBB1_57:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r16, .LBB1_57
.LBB1_58:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	addi r15, r0, 0
	bne r3, r15, .LBB1_60
.LBB1_59:
	sub r5, r15, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_60:
	blt r14, r17, .LBB1_63
.LBB1_61:
	addi r16, r0, 0
	add r18, r16, r0
.LBB1_62:
	ldw r1, r11+56
	add r1, r1, r18
	ldw r4, r1+0
	ldw r5, r11+76
	add r3, r12, r0
	jal r31, dumpFunction
	addi r14, r14, -1
	addi r18, r18, 4
	bne r14, r16, .LBB1_62
.LBB1_63:
	ldw r1, r12+12
	add r14, r15, r0
	bne r1, r15, .LBB1_65
.LBB1_64:
	ldw r14, r11+24
.LBB1_65:
	add r1, r15, r0
	add r3, r14, r0
.LBB1_66:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r15, .LBB1_66
.LBB1_67:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	addi r15, r0, 0
	bne r3, r15, .LBB1_69
.LBB1_68:
	sub r5, r15, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_69:
	beq r14, r15, .LBB1_72
.LBB1_70:
	ldw r1, r12+16
	addi r3, r0, 0
	bne r1, r3, .LBB1_72
.LBB1_71:
	ldw r4, r11+64
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	add r5, r14, r0
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_72:
	ldw r1, r12+12
	add r14, r15, r0
	bne r1, r15, .LBB1_74
.LBB1_73:
	ldw r14, r11+36
.LBB1_74:
	add r1, r15, r0
	add r3, r14, r0
.LBB1_75:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r15, .LBB1_75
.LBB1_76:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	addi r15, r0, 0
	bne r3, r15, .LBB1_78
.LBB1_77:
	sub r5, r15, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_78:
	blt r14, r17, .LBB1_89
.LBB1_79:
	addi r16, r0, 0
	add r18, r16, r0
	jal r0, .LBB1_81
.LBB1_80:
	addi r18, r18, 1
	beq r18, r14, .LBB1_89
.LBB1_81:
	ldw r1, r11+68
	slli r19, r18, 3
	add r1, r1, r19
	ldw r3, r1+0
	add r1, r16, r0
.LBB1_82:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r16, .LBB1_82
.LBB1_83:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	bne r3, r16, .LBB1_85
.LBB1_84:
	sub r5, r16, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_85:
	ldw r1, r11+68
	add r1, r1, r19
	ldw r3, r1+4
	add r1, r16, r0
.LBB1_86:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r16, .LBB1_86
.LBB1_87:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	bne r3, r16, .LBB1_80
.LBB1_88:
	sub r5, r16, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
	jal r0, .LBB1_80
.LBB1_89:
	ldw r1, r12+12
	add r14, r15, r0
	bne r1, r15, .LBB1_91
.LBB1_90:
	ldw r14, r11+32
.LBB1_91:
	add r1, r15, r0
	add r3, r14, r0
.LBB1_92:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r15, .LBB1_92
.LBB1_93:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	addi r15, r0, 0
	bne r3, r15, .LBB1_95
.LBB1_94:
	sub r5, r15, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_95:
	blt r14, r17, .LBB1_106
.LBB1_96:
	addi r16, r0, 0
	addi r18, r0, 12
	add r19, r16, r0
	jal r0, .LBB1_98
.LBB1_97:
	addi r19, r19, 1
	beq r19, r14, .LBB1_106
.LBB1_98:
	ldw r1, r11+72
	mul r20, r19, r18
	add r1, r1, r20
	ldw r4, r1+0
	add r3, r12, r0
	jal r31, dumpString
	ldw r1, r11+72
	add r1, r1, r20
	ldw r3, r1+4
	add r1, r16, r0
.LBB1_99:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r16, .LBB1_99
.LBB1_100:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	bne r3, r16, .LBB1_102
.LBB1_101:
	sub r5, r16, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_102:
	ldw r1, r11+72
	add r1, r1, r20
	ldw r3, r1+8
	add r1, r16, r0
.LBB1_103:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r16, .LBB1_103
.LBB1_104:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	bne r3, r16, .LBB1_97
.LBB1_105:
	sub r5, r16, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
	jal r0, .LBB1_97
.LBB1_106:
	ldw r1, r12+12
	add r14, r15, r0
	bne r1, r15, .LBB1_108
.LBB1_107:
	ldw r14, r11+12
.LBB1_108:
	add r1, r15, r0
	add r3, r14, r0
.LBB1_109:
	andi r4, r3, 127
	add r5, r13, r1
	stb r5+4, r4
	srli r3, r3, 7
	addi r1, r1, -1
	bne r3, r15, .LBB1_109
.LBB1_110:
	ldbu r3, r13+4
	ori  r3, r3, 128
	stb r13+4, r3
	ldw r3, r12+16
	addi r15, r0, 0
	bne r3, r15, .LBB1_112
.LBB1_111:
	sub r5, r15, r1
	add r1, r13, r1
	addi r4, r1, 5
	ldw r1, r12+4
	ldw r3, r12+0
	ldw r6, r12+8
	jalr lr, r1, 0
	stw r12+16, r1
.LBB1_112:
	blt r14, r17, .LBB1_115
.LBB1_113:
	add r13, r15, r0
.LBB1_114:
	ldw r1, r11+60
	add r1, r1, r13
	ldw r4, r1+0
	add r3, r12, r0
	jal r31, dumpString
	addi r14, r14, -1
	addi r13, r13, 8
	bne r14, r15, .LBB1_114
.LBB1_115:
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
.Lfunc_end1:
	.size	dumpFunction, .Lfunc_end1-dumpFunction
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI1_0:
	.word	.LBB1_38
	.word	.LBB1_32
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_33
	.word	.LBB1_40
	.word	.LBB1_32
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function dumpString
	.type	dumpString,@function
dumpString:                             # @dumpString
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
	stw fp+-20, lr
	add r11, r3, r0
	addi r1, r0, 0
	beq r4, r1, .LBB2_10
.LBB2_1:
	ldbu r12, r4+7
	addi r3, r0, 255
	bne r12, r3, .LBB2_3
.LBB2_2:
	ldw r12, r4+12
.LBB2_3:
	addi r5, r12, 1
	addi r3, fp, -25
	add r6, r1, r0
.LBB2_4:
	andi r7, r5, 127
	add r8, r3, r6
	stb r8+4, r7
	srli r5, r5, 7
	addi r6, r6, -1
	bne r5, r1, .LBB2_4
.LBB2_5:
	ldbu r1, r3+4
	ori  r1, r1, 128
	stb r3+4, r1
	ldw r1, r11+16
	addi r13, r0, 0
	bne r1, r13, .LBB2_7
.LBB2_6:
	sub r5, r13, r6
	add r1, r3, r6
	addi r1, r1, 5
	ldw r7, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	add r14, r4, r0
	add r4, r1, r0
	jalr lr, r7, 0
	add r4, r14, r0
	stw r11+16, r1
.LBB2_7:
	beq r12, r13, .LBB2_13
.LBB2_8:
	ldw r1, r11+16
	bne r1, r13, .LBB2_13
.LBB2_9:
	addi r4, r4, 16
	ldw r1, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	add r5, r12, r0
	jal r0, .LBB2_12
.LBB2_10:
	addi r3, r0, 128
	addi r1, fp, -30
	stb r1+4, r3
	ldw r3, r11+16
	addi r4, r0, 0
	bne r3, r4, .LBB2_13
.LBB2_11:
	addi r4, r1, 4
	ldw r1, r11+4
	ldw r3, r11+0
	ldw r6, r11+8
	addi r5, r0, 1
.LBB2_12:
	jalr lr, r1, 0
	stw r11+16, r1
.LBB2_13:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end2:
	.size	dumpString, .Lfunc_end2-dumpString
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\033Lua"
	.size	.L.str, 5

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"\031\223\r\n\032\n"
	.size	.L.str.1, 7

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
