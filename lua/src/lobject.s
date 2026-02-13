	.file	"lobject.c"
	.text
	.hidden	luaO_ceillog2                   # -- Begin function luaO_ceillog2
	.globl	luaO_ceillog2
	.p2align	2
	.type	luaO_ceillog2,@function
luaO_ceillog2:                          # @luaO_ceillog2
# %bb.0:
	addi r1, r3, -1
	addi r3, r0, 256
	bltu r1, r3, .LBB0_3
.LBB0_1:
	addi r3, r0, 0
	lui r4, 16
	addi r4, r4, -1
.LBB0_2:
	add r5, r1, r0
	addi r3, r3, 8
	srli r1, r1, 8
	bgtu r5, r4, .LBB0_2
	jal r0, .LBB0_4
.LBB0_3:
	addi r3, r0, 0
.LBB0_4:
	lui r4, %hi(luaO_ceillog2.log_2)
	addi r4, r4, %lo(luaO_ceillog2.log_2)
	add r1, r1, r4
	ldbu r1, r1+0
	add r1, r3, r1
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaO_ceillog2, .Lfunc_end0-luaO_ceillog2
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaO_rawarith
.LCPI1_0:
	.quad	0x0000000000000000              # double 0
.LCPI1_1:
	.quad	0x4000000000000000              # double 2
	.text
	.hidden	luaO_rawarith
	.globl	luaO_rawarith
	.p2align	2
	.type	luaO_rawarith,@function
luaO_rawarith:                          # @luaO_rawarith
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
	stw fp+-24, r16
	stw fp+-28, lr
	add r11, r7, r0
	addi r1, r0, 13
	bgtu r4, r1, .LBB1_16
.LBB1_1:
	addi r12, r0, 1
	sll r7, r12, r4
	lui r1, 3
	addi r1, r1, -128
	and r8, r7, r1
	addi r1, r0, 0
	beq r8, r1, .LBB1_10
.LBB1_2:
	ldbu r1, r5+8
	addi r13, r0, 3
	bne r1, r13, .LBB1_56
.LBB1_3:
	ldw r1, r5+0
	addi r5, fp, -32
	stw r5+0, r1
.LBB1_4:
	ldbu r1, r6+8
	bne r1, r13, .LBB1_57
.LBB1_5:
	ldw r1, r6+0
	addi r5, fp, -36
	stw r5+0, r1
.LBB1_6:
	addi r1, fp, -32
	ldw r5, r1+0
	addi r1, fp, -36
	ldw r6, r1+0
.LBB1_7:
	jal r31, intarith
	stw r11+0, r1
	stb r11+8, r13
.LBB1_8:
	addi r1, r0, 1
.LBB1_9:
	ldw lr, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.LBB1_10:
	andi r7, r7, 48
	beq r7, r1, .LBB1_16
.LBB1_11:
	ldbu r8, r5+8
	addi r7, r0, 3
	beq r8, r7, .LBB1_20
.LBB1_12:
	addi r9, r0, 19
	bne r8, r9, .LBB1_9
.LBB1_13:
	ldw r9, r5+4
	ldw r8, r5+0
	ldbu r5, r6+8
	beq r5, r7, .LBB1_21
.LBB1_14:
	addi r7, r0, 19
	bne r5, r7, .LBB1_9
.LBB1_15:
	ldw r15, r6+4
	ldw r14, r6+0
	jal r0, .LBB1_22
.LBB1_16:
	ldbu r7, r5+8
	addi r1, r0, 0
	addi r12, r0, 19
	beq r7, r12, .LBB1_25
.LBB1_17:
	addi r13, r0, 3
	bne r7, r13, .LBB1_9
.LBB1_18:
	ldbu r7, r6+8
	ldw r5, r5+0
	bne r7, r13, .LBB1_26
.LBB1_19:
	ldw r6, r6+0
	jal r0, .LBB1_7
.LBB1_20:
	ldw r5, r5+0
	fcvt.d.w r8, r5
	ldbu r5, r6+8
	bne r5, r7, .LBB1_14
.LBB1_21:
	ldw r1, r6+0
	fcvt.d.w r14, r1
.LBB1_22:
	lui r1, %hi(.LCPI1_0)
	addi r1, r1, %lo(.LCPI1_0)
	ldw r7, r1+4
	ldw r6, r1+0
	addi r1, r0, 12
	bgtu r4, r1, .LBB1_50
.LBB1_23:
	slli r1, r4, 2
	lui r4, %hi(.LJTI1_0)
	addi r4, r4, %lo(.LJTI1_0)
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB1_24:
	fadd.d r6, r8, r14
	jal r0, .LBB1_50
.LBB1_25:
	ldw r9, r5+4
	ldw r8, r5+0
	jal r0, .LBB1_27
.LBB1_26:
	fcvt.d.w r8, r5
.LBB1_27:
	ldbu r5, r6+8
	addi r7, r0, 3
	beq r5, r7, .LBB1_30
.LBB1_28:
	bne r5, r12, .LBB1_9
.LBB1_29:
	ldw r15, r6+4
	ldw r14, r6+0
	jal r0, .LBB1_31
.LBB1_30:
	ldw r1, r6+0
	fcvt.d.w r14, r1
.LBB1_31:
	lui r1, %hi(.LCPI1_0)
	addi r1, r1, %lo(.LCPI1_0)
	ldw r7, r1+4
	ldw r6, r1+0
	addi r1, r0, 12
	bgtu r4, r1, .LBB1_55
.LBB1_32:
	slli r1, r4, 2
	lui r4, %hi(.LJTI1_1)
	addi r4, r4, %lo(.LJTI1_1)
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB1_33:
	fadd.d r6, r8, r14
	jal r0, .LBB1_55
.LBB1_34:
	fdiv.d r6, r8, r14
	jal r0, .LBB1_50
.LBB1_35:
	fsub.d r6, r8, r14
	jal r0, .LBB1_50
.LBB1_36:
	fdiv.d r4, r8, r14
	add r3, r4, r0
	add r4, r5, r0
	jal r31, floor
	jal r0, .LBB1_49
.LBB1_37:
	fneg.d r6, r8
	jal r0, .LBB1_50
.LBB1_38:
	fmul.d r6, r8, r14
	jal r0, .LBB1_50
.LBB1_39:
	add r5, r8, r0
	add r6, r9, r0
	add r7, r14, r0
	add r8, r15, r0
	jal r31, luaV_modf
	jal r0, .LBB1_49
.LBB1_40:
	lui r1, %hi(.LCPI1_1)
	addi r1, r1, %lo(.LCPI1_1)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r14, r4
	xori r1, r1, 1
	addi r3, r0, 0
	bne r1, r3, .LBB1_48
.LBB1_41:
	fmul.d r6, r8, r8
	jal r0, .LBB1_50
.LBB1_42:
	fdiv.d r6, r8, r14
	jal r0, .LBB1_55
.LBB1_43:
	fsub.d r6, r8, r14
	jal r0, .LBB1_55
.LBB1_44:
	fdiv.d r4, r8, r14
	add r3, r4, r0
	add r4, r5, r0
	jal r31, floor
	jal r0, .LBB1_54
.LBB1_45:
	fneg.d r6, r8
	jal r0, .LBB1_55
.LBB1_46:
	fmul.d r6, r8, r14
	jal r0, .LBB1_55
.LBB1_47:
	add r5, r8, r0
	add r6, r9, r0
	add r7, r14, r0
	add r8, r15, r0
	jal r31, luaV_modf
	jal r0, .LBB1_54
.LBB1_48:
	add r3, r8, r0
	add r4, r9, r0
	add r5, r14, r0
	add r6, r15, r0
	jal r31, pow
.LBB1_49:
	add r6, r1, r0
	add r7, r2, r0
.LBB1_50:
	stw r11+4, r7
	stw r11+0, r6
	addi r1, r0, 19
	stb r11+8, r1
	add r1, r12, r0
	jal r0, .LBB1_9
.LBB1_51:
	lui r1, %hi(.LCPI1_1)
	addi r1, r1, %lo(.LCPI1_1)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r14, r4
	xori r1, r1, 1
	addi r3, r0, 0
	bne r1, r3, .LBB1_53
.LBB1_52:
	fmul.d r6, r8, r8
	jal r0, .LBB1_55
.LBB1_53:
	add r3, r8, r0
	add r4, r9, r0
	add r5, r14, r0
	add r6, r15, r0
	jal r31, pow
.LBB1_54:
	add r6, r1, r0
	add r7, r2, r0
.LBB1_55:
	stw r11+4, r7
	stw r11+0, r6
	stb r11+8, r12
	jal r0, .LBB1_8
.LBB1_56:
	addi r1, fp, -32
	addi r12, r0, 0
	add r14, r3, r0
	add r3, r5, r0
	add r15, r4, r0
	add r4, r1, r0
	add r5, r12, r0
	add r16, r6, r0
	jal r31, luaV_tointegerns
	add r6, r16, r0
	add r4, r15, r0
	add r3, r14, r0
	add r5, r1, r0
	add r1, r12, r0
	bne r5, r12, .LBB1_4
	jal r0, .LBB1_9
.LBB1_57:
	addi r1, fp, -36
	addi r12, r0, 0
	add r14, r3, r0
	add r3, r6, r0
	add r15, r4, r0
	add r4, r1, r0
	add r5, r12, r0
	jal r31, luaV_tointegerns
	add r4, r15, r0
	add r3, r14, r0
	add r5, r1, r0
	add r1, r12, r0
	bne r5, r12, .LBB1_6
	jal r0, .LBB1_9
.Lfunc_end1:
	.size	luaO_rawarith, .Lfunc_end1-luaO_rawarith
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI1_0:
	.word	.LBB1_24
	.word	.LBB1_35
	.word	.LBB1_38
	.word	.LBB1_39
	.word	.LBB1_40
	.word	.LBB1_34
	.word	.LBB1_36
	.word	.LBB1_50
	.word	.LBB1_50
	.word	.LBB1_50
	.word	.LBB1_50
	.word	.LBB1_50
	.word	.LBB1_37
.LJTI1_1:
	.word	.LBB1_33
	.word	.LBB1_43
	.word	.LBB1_46
	.word	.LBB1_47
	.word	.LBB1_51
	.word	.LBB1_42
	.word	.LBB1_44
	.word	.LBB1_55
	.word	.LBB1_55
	.word	.LBB1_55
	.word	.LBB1_55
	.word	.LBB1_55
	.word	.LBB1_45
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function intarith
	.type	intarith,@function
intarith:                               # @intarith
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r1, r0, 0
	addi r7, r0, 13
	bgtu r4, r7, .LBB2_15
.LBB2_1:
	slli r4, r4, 2
	lui r7, %hi(.LJTI2_0)
	addi r7, r7, %lo(.LJTI2_0)
	add r4, r7, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB2_2:
	add r1, r6, r5
	jal r0, .LBB2_15
.LBB2_3:
	addi r1, r0, 0
	sub r1, r1, r5
	jal r0, .LBB2_15
.LBB2_4:
	mul r1, r6, r5
	jal r0, .LBB2_15
.LBB2_5:
	and r1, r6, r5
	jal r0, .LBB2_15
.LBB2_6:
	addi r1, r0, -1
	xor r1, r5, r1
	jal r0, .LBB2_15
.LBB2_7:
	sub r1, r5, r6
	jal r0, .LBB2_15
.LBB2_8:
	or  r1, r6, r5
	jal r0, .LBB2_15
.LBB2_9:
	xor r1, r6, r5
	jal r0, .LBB2_15
.LBB2_10:
	add r4, r5, r0
	add r5, r6, r0
	jal r31, luaV_mod
	jal r0, .LBB2_15
.LBB2_11:
	add r4, r5, r0
	add r5, r6, r0
	jal r31, luaV_idiv
	jal r0, .LBB2_15
.LBB2_12:
	addi r1, r0, 0
	sub r4, r1, r6
	add r3, r5, r0
	jal r0, .LBB2_14
.LBB2_13:
	add r3, r5, r0
	add r4, r6, r0
.LBB2_14:
	jal r31, luaV_shiftl
.LBB2_15:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	intarith, .Lfunc_end2-intarith
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI2_0:
	.word	.LBB2_2
	.word	.LBB2_7
	.word	.LBB2_4
	.word	.LBB2_10
	.word	.LBB2_15
	.word	.LBB2_15
	.word	.LBB2_11
	.word	.LBB2_5
	.word	.LBB2_8
	.word	.LBB2_9
	.word	.LBB2_13
	.word	.LBB2_12
	.word	.LBB2_3
	.word	.LBB2_6
                                        # -- End function
	.text
	.hidden	luaO_arith                      # -- Begin function luaO_arith
	.globl	luaO_arith
	.p2align	2
	.type	luaO_arith,@function
luaO_arith:                             # @luaO_arith
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
	stw fp+-24, lr
	add r11, r7, r0
	add r12, r6, r0
	add r13, r5, r0
	add r15, r4, r0
	add r14, r3, r0
	jal r31, luaO_rawarith
	addi r3, r0, 0
	bne r1, r3, .LBB3_2
.LBB3_1:
	addi r7, r15, 6
	add r3, r14, r0
	add r4, r13, r0
	add r5, r12, r0
	add r6, r11, r0
	jal r31, luaT_trybinTM
.LBB3_2:
	ldw lr, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end3:
	.size	luaO_arith, .Lfunc_end3-luaO_arith
                                        # -- End function
	.hidden	luaO_hexavalue                  # -- Begin function luaO_hexavalue
	.globl	luaO_hexavalue
	.p2align	2
	.type	luaO_hexavalue,@function
luaO_hexavalue:                         # @luaO_hexavalue
# %bb.0:
	lui r1, %hi(luai_ctype_+1)
	addi r1, r1, %lo(luai_ctype_+1)
	add r1, r3, r1
	ldbu r1, r1+0
	andi r1, r1, 2
	addi r4, r0, 0
	seq r1, r1, r4
	addi r5, r3, -48
	ori  r3, r3, 32
	addi r3, r3, -87
	xor r3, r3, r5
	sub r1, r4, r1
	and r1, r3, r1
	xor r1, r5, r1
	jalr r0, r31, 0
.Lfunc_end4:
	.size	luaO_hexavalue, .Lfunc_end4-luaO_hexavalue
                                        # -- End function
	.hidden	luaO_str2num                    # -- Begin function luaO_str2num
	.globl	luaO_str2num
	.p2align	2
	.type	luaO_str2num,@function
luaO_str2num:                           # @luaO_str2num
# %bb.0:
	addi sp, sp, -264
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 264
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, lr
	add r11, r3, r0
	lui r5, %hi(luai_ctype_+1)
	addi r5, r5, %lo(luai_ctype_+1)
	addi r3, r0, 0
	add r6, r11, r0
.LBB5_1:
	ldbu r1, r6+0
	add r7, r1, r5
	ldbu r7, r7+0
	andi r7, r7, 8
	addi r6, r6, 1
	bne r7, r3, .LBB5_1
.LBB5_2:
	addi r7, r0, 43
	beq r1, r7, .LBB5_6
.LBB5_3:
	addi r7, r0, 45
	bne r1, r7, .LBB5_5
.LBB5_4:
	addi r9, r0, 8
	jal r0, .LBB5_7
.LBB5_5:
	addi r6, r6, -1
.LBB5_6:
	addi r9, r0, 7
	addi r3, r0, 1
.LBB5_7:
	ldbu r10, r6+0
	addi r1, r0, 48
	bne r10, r1, .LBB5_11
.LBB5_8:
	ldbu r1, r6+1
	ori  r1, r1, 32
	addi r7, r0, 120
	bne r1, r7, .LBB5_11
.LBB5_9:
	ldbu r9, r6+2
	add r1, r9, r5
	ldbu r1, r1+0
	andi r1, r1, 16
	addi r8, r0, 0
	bne r1, r8, .LBB5_26
.LBB5_10:
	addi r6, r6, 2
	add r7, r8, r0
	jal r0, .LBB5_29
.LBB5_11:
	add r1, r10, r5
	ldbu r1, r1+0
	andi r1, r1, 2
	addi r8, r0, 0
	bne r1, r8, .LBB5_13
.LBB5_12:
	add r7, r8, r0
	jal r0, .LBB5_29
.LBB5_13:
	addi r1, r0, 0
	lui r7, 52429
	addi r12, r7, -820
	addi r13, r0, 10
	addi r8, r0, 1
	add r7, r1, r0
	jal r0, .LBB5_15
.LBB5_14:
	mul r7, r7, r13
	add r7, r10, r7
	addi r14, r6, 1
	ldbu r10, r6+1
	add r6, r10, r5
	ldbu r6, r6+0
	andi r15, r6, 2
	add r6, r14, r0
	beq r15, r1, .LBB5_25
.LBB5_15:
	slli r10, r10, 24
	srai r10, r10, 24
	addi r10, r10, -48
	bltu r7, r12, .LBB5_14
.LBB5_16:
	bne r7, r12, .LBB5_18
.LBB5_17:
	ble r10, r9, .LBB5_14
.LBB5_18:
                                        # implicit-def: $r5
	addi r12, r0, 0
	bne r1, r12, .LBB5_34
.LBB5_19:
	add r17, r4, r0
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	add r3, r11, r0
	jal r31, strpbrk
	add r13, r12, r0
	beq r1, r12, .LBB5_21
.LBB5_20:
	ldbu r1, r1+0
	ori  r13, r1, 32
.LBB5_21:
	addi r3, r0, 110
	add r1, r12, r0
	beq r13, r3, .LBB5_23
.LBB5_22:
	addi r4, fp, -244
	add r3, r11, r0
	add r5, r13, r0
	jal r31, l_str2dloc
	addi r16, r0, 0
	beq r1, r16, .LBB5_37
.LBB5_23:
	beq r1, r12, .LBB5_36
.LBB5_24:
	addi r3, fp, -244
	ldw r5, r3+4
	ldw r3, r3+0
	add r4, r17, r0
	stw r17+4, r5
	stw r17+0, r3
	addi r3, r0, 19
	jal r0, .LBB5_35
.LBB5_25:
	add r6, r14, r0
	jal r0, .LBB5_29
.LBB5_26:
	addi r1, r6, 3
	addi r6, r0, 0
	add r7, r6, r0
.LBB5_27:
	slli r7, r7, 4
	slli r8, r9, 24
	srai r8, r8, 24
	add r9, r8, r5
	ldbu r9, r9+0
	andi r9, r9, 2
	seq r9, r9, r6
	addi r10, r8, -48
	ori  r8, r8, 32
	addi r8, r8, -87
	xor r8, r8, r10
	sub r9, r6, r9
	and r8, r8, r9
	xor r8, r10, r8
	add r7, r8, r7
	ldbu r9, r1+0
	add r8, r9, r5
	ldbu r8, r8+0
	andi r8, r8, 16
	addi r1, r1, 1
	bne r8, r6, .LBB5_27
.LBB5_28:
	addi r6, r1, -1
	addi r8, r0, 1
.LBB5_29:
	addi r6, r6, -1
	addi r1, r0, 0
.LBB5_30:
	add r9, r6, r0
	addi r6, r6, 1
	ldbu r9, r9+1
	add r10, r9, r5
	ldbu r10, r10+0
	andi r10, r10, 8
	bne r10, r1, .LBB5_30
.LBB5_31:
	sne r5, r9, r1
	xori r8, r8, 1
	or  r8, r8, r5
                                        # implicit-def: $r5
	bne r8, r1, .LBB5_33
.LBB5_32:
	addi r1, r0, 0
	sub r5, r1, r7
	sub r1, r1, r3
	xor r3, r7, r5
	and r1, r3, r1
	xor r5, r5, r1
	add r1, r6, r0
.LBB5_33:
	addi r12, r0, 0
	beq r1, r12, .LBB5_19
.LBB5_34:
	stw r4+0, r5
	addi r3, r0, 3
.LBB5_35:
	stb r4+8, r3
	sub r1, r1, r11
	addi r12, r1, 1
.LBB5_36:
	add r1, r12, r0
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
	addi sp, sp, 264
	jalr r0, r31, 0
.LBB5_37:
	addi r14, r0, 46
	add r3, r11, r0
	add r4, r14, r0
	jal r31, strchr
	beq r1, r16, .LBB5_40
.LBB5_38:
	add r15, r1, r0
	add r3, r11, r0
	jal r31, strlen
	add r3, r1, r0
	addi r4, r0, 200
	add r1, r16, r0
	bgtu r3, r4, .LBB5_23
.LBB5_39:
	addi r16, fp, -233
	add r3, r16, r0
	add r4, r11, r0
	jal r31, strcpy
	sub r1, r15, r11
	add r1, r16, r1
	stb r1+0, r14
	addi r4, fp, -244
	add r3, r16, r0
	add r5, r13, r0
	jal r31, l_str2dloc
	addi r3, r0, 0
	seq r4, r1, r3
	sub r1, r1, r16
	add r1, r11, r1
	sub r3, r3, r4
	and r3, r1, r3
	xor r1, r1, r3
	jal r0, .LBB5_23
.LBB5_40:
	add r1, r16, r0
	jal r0, .LBB5_23
.Lfunc_end5:
	.size	luaO_str2num, .Lfunc_end5-luaO_str2num
                                        # -- End function
	.hidden	luaO_utf8esc                    # -- Begin function luaO_utf8esc
	.globl	luaO_utf8esc
	.p2align	2
	.type	luaO_utf8esc,@function
luaO_utf8esc:                           # @luaO_utf8esc
# %bb.0:
	addi r1, r0, 128
	bgeu r4, r1, .LBB6_2
.LBB6_1:
	addi r1, r0, 1
	addi r5, r0, 7
	jal r0, .LBB6_5
.LBB6_2:
	addi r6, r3, 7
	addi r5, r0, 63
	addi r1, r0, 0
.LBB6_3:
	andi r7, r4, 63
	ori  r7, r7, 128
	add r8, r6, r1
	stb r8+0, r7
	srli r4, r4, 6
	srli r5, r5, 1
	addi r1, r1, -1
	bgtu r4, r5, .LBB6_3
.LBB6_4:
	addi r6, r0, -1
	xor r5, r5, r6
	slli r5, r5, 1
	or  r4, r5, r4
	addi r5, r1, 7
	addi r6, r0, 1
	sub r1, r6, r1
.LBB6_5:
	add r3, r3, r5
	stb r3+0, r4
	jalr r0, r31, 0
.Lfunc_end6:
	.size	luaO_utf8esc, .Lfunc_end6-luaO_utf8esc
                                        # -- End function
	.hidden	luaO_tostring                   # -- Begin function luaO_tostring
	.globl	luaO_tostring
	.p2align	2
	.type	luaO_tostring,@function
luaO_tostring:                          # @luaO_tostring
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
	stw fp+-20, lr
	add r11, r4, r0
	add r12, r3, r0
	ldbu r1, r4+8
	addi r3, r0, 3
	bne r1, r3, .LBB7_2
.LBB7_1:
	ldw r5, r11+0
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	addi r3, fp, -64
	jal r31, sprintf
	add r13, r1, r0
	jal r0, .LBB7_4
.LBB7_2:
	ldw r6, r11+4
	ldw r5, r11+0
	lui r4, %hi(.L.str.9)
	addi r4, r4, %lo(.L.str.9)
	addi r14, fp, -64
	add r3, r14, r0
	jal r31, sprintf
	add r13, r1, r0
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	add r3, r14, r0
	jal r31, strspn
	add r1, r14, r1
	ldbu r1, r1+0
	addi r3, r0, 0
	bne r1, r3, .LBB7_4
.LBB7_3:
	add r1, r14, r13
	addi r3, r0, 46
	stb r1+0, r3
	addi r13, r13, 2
	addi r3, r0, 48
	stb r1+1, r3
.LBB7_4:
	addi r4, fp, -64
	add r3, r12, r0
	add r5, r13, r0
	jal r31, luaS_newlstr
	stw r11+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r11+8, r1
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 72
	jalr r0, r31, 0
.Lfunc_end7:
	.size	luaO_tostring, .Lfunc_end7-luaO_tostring
                                        # -- End function
	.hidden	luaO_pushvfstring               # -- Begin function luaO_pushvfstring
	.globl	luaO_pushvfstring
	.p2align	2
	.type	luaO_pushvfstring,@function
luaO_pushvfstring:                      # @luaO_pushvfstring
# %bb.0:
	addi sp, sp, -328
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 328
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
	add r20, r4, r0
	addi r21, fp, -80
	stw r21+0, r5
	addi r22, r0, 0
	addi r12, fp, -292
	stw r12+8, r22
	stw r12+4, r22
	stw fp+-308, r3
	stw r12+0, r3
	addi r4, r0, 37
	add r3, r20, r0
	jal r31, strchr
	beq r1, r22, .LBB8_37
.LBB8_1:
	add r15, r1, r0
	addi r13, r12, 12
	addi r14, fp, -304
	addi r23, r14, 8
	addi r24, r14, 7
	addi r25, r0, 78
	lui r26, %hi(.LJTI8_0)
	addi r26, r26, %lo(.LJTI8_0)
	addi r27, r0, 199
	addi r16, r0, 1
	addi r1, r0, 2
	stw fp+-316, r1
	lui r18, %hi(.L.str.2)
	addi r18, r18, %lo(.L.str.2)
	addi r19, r0, 37
	addi r28, r0, 3
	addi r17, r0, 128
	addi r1, r0, 63
	stw fp+-312, r1
	jal r0, .LBB8_5
.LBB8_2:
	ldw r1, r21+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r21+0, r1
	seq r1, r3, r22
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	xor r4, r3, r4
	sub r1, r22, r1
	and r1, r4, r1
	xor r20, r3, r1
	add r3, r20, r0
	jal r31, strlen
	addi r3, fp, -292
	add r4, r20, r0
	add r5, r1, r0
.LBB8_3:
	jal r31, addstr2buff
.LBB8_4:
	addi r20, r15, 2
	add r3, r20, r0
	add r4, r19, r0
	jal r31, strchr
	add r15, r1, r0
	beq r1, r22, .LBB8_37
.LBB8_5:
	sub r5, r15, r20
	add r3, r12, r0
	add r4, r20, r0
	jal r31, addstr2buff
	ldbu r1, r15+1
	addi r3, r1, -37
	bgtu r3, r25, .LBB8_41
.LBB8_6:
	slli r3, r3, 2
	add r3, r26, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB8_7:
	ldw r5, r12+8
	blt r5, r27, .LBB8_32
.LBB8_8:
	ldw r20, r12+0
	ldw r11, r20+12
	add r3, r20, r0
	add r4, r13, r0
	jal r31, luaS_newlstr
	stw r11+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r11+8, r1
	ldw r1, r20+12
	addi r1, r1, 12
	stw r20+12, r1
	ldw r1, r12+4
	beq r1, r22, .LBB8_30
.LBB8_9:
	add r3, r20, r0
	ldw r4, fp+-316
	jal r31, luaV_concat
	jal r0, .LBB8_31
.LBB8_10:
	ldw r1, r21+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r21+0, r1
	stw r14+0, r3
	stb r14+8, r28
	jal r0, .LBB8_12
.LBB8_11:
	ldw r1, r21+0
	ldw r3, r1+0
	ldw r4, r1+4
	addi r1, r1, 8
	stw r21+0, r1
	stw r14+4, r4
	stw r14+0, r3
	addi r1, r0, 19
	stb r14+8, r1
.LBB8_12:
	addi r3, fp, -292
	jal r0, .LBB8_19
.LBB8_13:
	ldw r3, r21+0
	ldw r1, r3+0
	addi r3, r3, 4
	stw r21+0, r3
	bgeu r1, r17, .LBB8_23
.LBB8_14:
	stb r14+7, r1
	add r5, r16, r0
	jal r0, .LBB8_26
.LBB8_15:
	ldw r1, r21+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r21+0, r1
	stb r14+0, r3
	ldw r5, r12+8
	blt r5, r27, .LBB8_29
.LBB8_16:
	ldw r20, r12+0
	ldw r11, r20+12
	add r3, r20, r0
	add r4, r13, r0
	jal r31, luaS_newlstr
	stw r11+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r11+8, r1
	ldw r1, r20+12
	addi r1, r1, 12
	stw r20+12, r1
	ldw r1, r12+4
	beq r1, r22, .LBB8_27
.LBB8_17:
	addi r4, r0, 2
	add r3, r20, r0
	jal r31, luaV_concat
	jal r0, .LBB8_28
.LBB8_18:
	ldw r1, r21+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r21+0, r1
	stw r14+0, r3
	stb r14+8, r28
	add r3, r12, r0
.LBB8_19:
	add r4, r14, r0
	jal r31, addnum2buff
	jal r0, .LBB8_4
.LBB8_20:
	ldw r5, r12+8
	addi r1, r0, 180
	blt r5, r1, .LBB8_36
.LBB8_21:
	ldw r20, r12+0
	ldw r11, r20+12
	add r3, r20, r0
	add r4, r13, r0
	jal r31, luaS_newlstr
	stw r11+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r11+8, r1
	ldw r1, r20+12
	addi r1, r1, 12
	stw r20+12, r1
	ldw r1, r12+4
	beq r1, r22, .LBB8_34
.LBB8_22:
	addi r4, r0, 2
	add r3, r20, r0
	jal r31, luaV_concat
	jal r0, .LBB8_35
.LBB8_23:
	add r3, r22, r0
	ldw r4, fp+-312
.LBB8_24:
	andi r5, r1, 63
	ori  r5, r5, 128
	add r6, r24, r3
	stb r6+0, r5
	srli r1, r1, 6
	srli r4, r4, 1
	addi r3, r3, -1
	bgtu r1, r4, .LBB8_24
.LBB8_25:
	addi r5, r0, -1
	xor r4, r4, r5
	slli r4, r4, 1
	or  r1, r4, r1
	add r4, r24, r3
	stb r4+0, r1
	addi r1, r0, 1
	sub r5, r1, r3
.LBB8_26:
	sub r4, r23, r5
	add r3, r12, r0
	jal r0, .LBB8_3
.LBB8_27:
	addi r1, r0, 1
	stw r12+4, r1
.LBB8_28:
	stw r12+8, r22
.LBB8_29:
	ldw r1, r12+8
	add r3, r13, r1
	addi r4, fp, -304
	addi r5, r0, 1
	jal r0, .LBB8_33
.LBB8_30:
	stw r12+4, r16
.LBB8_31:
	stw r12+8, r22
.LBB8_32:
	ldw r1, r12+8
	add r3, r13, r1
	add r4, r18, r0
	add r5, r16, r0
.LBB8_33:
	jal r31, memcpy
	ldw r1, r12+8
	addi r1, r1, 1
	stw r12+8, r1
	jal r0, .LBB8_4
.LBB8_34:
	addi r1, r0, 1
	stw r12+4, r1
.LBB8_35:
	stw r12+8, r22
.LBB8_36:
	ldw r1, r12+8
	add r3, r13, r1
	ldw r1, r21+0
	ldw r5, r1+0
	addi r1, r1, 4
	stw r21+0, r1
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, sprintf
	ldw r3, r12+8
	add r1, r3, r1
	stw r12+8, r1
	jal r0, .LBB8_4
.LBB8_37:
	add r3, r20, r0
	jal r31, strlen
	add r3, r12, r0
	add r4, r20, r0
	add r5, r1, r0
	jal r31, addstr2buff
	addi r4, r12, 12
	ldw r5, r12+8
	ldw r13, r12+0
	ldw r11, r13+12
	add r3, r13, r0
	jal r31, luaS_newlstr
	stw r11+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r11+8, r1
	ldw r1, r13+12
	addi r1, r1, 12
	stw r13+12, r1
	ldw r1, r12+4
	addi r14, r0, 0
	beq r1, r14, .LBB8_39
.LBB8_38:
	addi r4, r0, 2
	add r3, r13, r0
	jal r31, luaV_concat
	jal r0, .LBB8_40
.LBB8_39:
	addi r1, r0, 1
	stw r12+4, r1
.LBB8_40:
	ldw r1, fp+-308
	stw r12+8, r14
	ldw r1, r1+12
	ldw r1, r1+-12
	addi r1, r1, 16
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
	addi sp, sp, 328
	jalr r0, r31, 0
.LBB8_41:
	slli r1, r1, 24
	srai r5, r1, 24
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	ldw r3, fp+-308
	jal r31, luaG_runerror
.Lfunc_end8:
	.size	luaO_pushvfstring, .Lfunc_end8-luaO_pushvfstring
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI8_0:
	.word	.LBB8_7
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_18
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_13
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_15
	.word	.LBB8_10
	.word	.LBB8_41
	.word	.LBB8_11
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_20
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_2
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function addstr2buff
	.type	addstr2buff,@function
addstr2buff:                            # @addstr2buff
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
	add r12, r5, r0
	add r13, r4, r0
	add r11, r3, r0
	addi r1, r0, 199
	bgtu r5, r1, .LBB9_4
.LBB9_1:
	ldw r5, r11+8
	sub r1, r1, r5
	ble r12, r1, .LBB9_12
.LBB9_2:
	addi r4, r11, 12
	ldw r14, r11+0
	ldw r15, r14+12
	add r3, r14, r0
	jal r31, luaS_newlstr
	stw r15+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r15+8, r1
	ldw r1, r14+12
	addi r1, r1, 12
	stw r14+12, r1
	ldw r1, r11+4
	addi r15, r0, 0
	beq r1, r15, .LBB9_10
.LBB9_3:
	addi r4, r0, 2
	add r3, r14, r0
	jal r31, luaV_concat
	jal r0, .LBB9_11
.LBB9_4:
	addi r4, r11, 12
	ldw r5, r11+8
	ldw r14, r11+0
	ldw r15, r14+12
	add r3, r14, r0
	jal r31, luaS_newlstr
	stw r15+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r15+8, r1
	ldw r1, r14+12
	addi r1, r1, 12
	stw r14+12, r1
	ldw r1, r11+4
	addi r15, r0, 0
	beq r1, r15, .LBB9_6
.LBB9_5:
	addi r4, r0, 2
	add r3, r14, r0
	jal r31, luaV_concat
	jal r0, .LBB9_7
.LBB9_6:
	addi r1, r0, 1
	stw r11+4, r1
.LBB9_7:
	stw r11+8, r15
	ldw r14, r11+0
	ldw r16, r14+12
	add r3, r14, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, luaS_newlstr
	stw r16+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r16+8, r1
	ldw r1, r14+12
	addi r1, r1, 12
	stw r14+12, r1
	ldw r1, r11+4
	beq r1, r15, .LBB9_9
.LBB9_8:
	addi r4, r0, 2
	add r3, r14, r0
	jal r31, luaV_concat
	jal r0, .LBB9_13
.LBB9_9:
	addi r1, r0, 1
	stw r11+4, r1
	jal r0, .LBB9_13
.LBB9_10:
	addi r1, r0, 1
	stw r11+4, r1
.LBB9_11:
	stw r11+8, r15
.LBB9_12:
	ldw r1, r11+8
	add r1, r11, r1
	addi r3, r1, 12
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcpy
	ldw r1, r11+8
	add r1, r1, r12
	stw r11+8, r1
.LBB9_13:
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
.Lfunc_end9:
	.size	addstr2buff, .Lfunc_end9-addstr2buff
                                        # -- End function
	.p2align	2                               # -- Begin function addnum2buff
	.type	addnum2buff,@function
addnum2buff:                            # @addnum2buff
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
	add r12, r4, r0
	add r11, r3, r0
	ldw r5, r3+8
	addi r1, r0, 156
	blt r5, r1, .LBB10_5
.LBB10_1:
	addi r4, r11, 12
	ldw r13, r11+0
	ldw r14, r13+12
	add r3, r13, r0
	jal r31, luaS_newlstr
	stw r14+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r14+8, r1
	ldw r1, r13+12
	addi r1, r1, 12
	stw r13+12, r1
	ldw r1, r11+4
	addi r14, r0, 0
	beq r1, r14, .LBB10_3
.LBB10_2:
	addi r4, r0, 2
	add r3, r13, r0
	jal r31, luaV_concat
	jal r0, .LBB10_4
.LBB10_3:
	addi r1, r0, 1
	stw r11+4, r1
.LBB10_4:
	stw r11+8, r14
.LBB10_5:
	ldw r1, r11+8
	add r1, r11, r1
	addi r13, r1, 12
	ldbu r1, r12+8
	addi r3, r0, 3
	bne r1, r3, .LBB10_7
.LBB10_6:
	ldw r5, r12+0
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	add r3, r13, r0
	jal r31, sprintf
	add r12, r1, r0
	jal r0, .LBB10_9
.LBB10_7:
	ldw r6, r12+4
	ldw r5, r12+0
	lui r4, %hi(.L.str.9)
	addi r4, r4, %lo(.L.str.9)
	add r3, r13, r0
	jal r31, sprintf
	add r12, r1, r0
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	add r3, r13, r0
	jal r31, strspn
	add r1, r13, r1
	ldbu r1, r1+0
	addi r3, r0, 0
	bne r1, r3, .LBB10_9
.LBB10_8:
	add r1, r13, r12
	addi r3, r0, 46
	stb r1+0, r3
	addi r12, r12, 2
	addi r3, r0, 48
	stb r1+1, r3
.LBB10_9:
	ldw r1, r11+8
	add r1, r1, r12
	stw r11+8, r1
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end10:
	.size	addnum2buff, .Lfunc_end10-addnum2buff
                                        # -- End function
	.hidden	luaO_pushfstring                # -- Begin function luaO_pushfstring
	.globl	luaO_pushfstring
	.p2align	2
	.type	luaO_pushfstring,@function
luaO_pushfstring:                       # @luaO_pushfstring
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
	addi r7, r1, 4
	stw r7+0, r6
	stw r1+0, r5
	addi r5, fp, -32
	stw r5+0, r1
	add r5, r1, r0
	jal r31, luaO_pushvfstring
	ldw lr, fp+-28
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end11:
	.size	luaO_pushfstring, .Lfunc_end11-luaO_pushfstring
                                        # -- End function
	.hidden	luaO_chunkid                    # -- Begin function luaO_chunkid
	.globl	luaO_chunkid
	.p2align	2
	.type	luaO_chunkid,@function
luaO_chunkid:                           # @luaO_chunkid
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
	add r11, r5, r0
	ldbu r1, r4+0
	addi r5, r0, 64
	beq r1, r5, .LBB12_4
.LBB12_1:
	addi r5, r0, 61
	bne r1, r5, .LBB12_6
.LBB12_2:
	addi r4, r4, 1
	addi r1, r0, 60
	bgtu r11, r1, .LBB12_9
.LBB12_3:
	add r5, r11, r0
	jal r0, .LBB12_13
.LBB12_4:
	addi r1, r0, 60
	bgtu r11, r1, .LBB12_10
.LBB12_5:
	addi r4, r4, 1
	add r5, r11, r0
	jal r0, .LBB12_13
.LBB12_6:
	addi r1, r0, 10
	add r13, r4, r0
	add r12, r3, r0
	add r3, r4, r0
	add r4, r1, r0
	jal r31, strchr
	add r14, r1, r0
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	addi r5, r0, 9
	add r3, r12, r0
	jal r31, memcpy
	addi r12, r12, 9
	addi r3, r0, 44
	addi r1, r0, 0
	bgtu r11, r3, .LBB12_11
.LBB12_7:
	bne r14, r1, .LBB12_11
.LBB12_8:
	add r3, r12, r0
	add r4, r13, r0
	add r5, r11, r0
	jal r31, memcpy
	add r3, r12, r11
	jal r0, .LBB12_12
.LBB12_9:
	addi r5, r0, 59
	add r11, r3, r0
	jal r31, memcpy
	addi r1, r0, 0
	stb r11+59, r1
	jal r0, .LBB12_14
.LBB12_10:
	lui r1, %hi(.L.str.4)
	addi r1, r1, %lo(.L.str.4)
	addi r5, r0, 3
	add r12, r3, r0
	add r13, r4, r0
	add r4, r1, r0
	jal r31, memcpy
	addi r3, r12, 3
	add r1, r13, r11
	addi r4, r1, -56
	addi r5, r0, 57
	jal r0, .LBB12_13
.LBB12_11:
	seq r3, r14, r1
	sub r5, r14, r13
	sub r3, r1, r3
	xor r6, r11, r5
	and r3, r6, r3
	xor r3, r5, r3
	addi r5, r0, 45
	sltu r5, r3, r5
	sub r1, r1, r5
	xori r3, r3, 45
	and r1, r3, r1
	xori r11, r1, 45
	add r3, r12, r0
	add r4, r13, r0
	add r5, r11, r0
	jal r31, memcpy
	add r11, r12, r11
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	addi r5, r0, 3
	add r3, r11, r0
	jal r31, memcpy
	addi r3, r11, 3
.LBB12_12:
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	addi r5, r0, 3
.LBB12_13:
	jal r31, memcpy
.LBB12_14:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end12:
	.size	luaO_chunkid, .Lfunc_end12-luaO_chunkid
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function l_str2dloc
.LCPI13_0:
	.quad	0x0000000000000000              # double 0
.LCPI13_1:
	.quad	0x4030000000000000              # double 16
	.text
	.p2align	2
	.type	l_str2dloc,@function
l_str2dloc:                             # @l_str2dloc
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
	add r12, r4, r0
	add r11, r3, r0
	addi r4, r0, 120
	addi r13, fp, -68
	lui r14, %hi(luai_ctype_+1)
	addi r14, r14, %lo(luai_ctype_+1)
	bne r5, r4, .LBB13_22
.LBB13_1:
	stw r13+0, r11
	addi r1, r0, 0
	add r6, r11, r0
.LBB13_2:
	ldbu r5, r6+0
	add r3, r5, r14
	ldbu r3, r3+0
	andi r3, r3, 8
	addi r6, r6, 1
	bne r3, r1, .LBB13_2
.LBB13_3:
	addi r3, r0, 43
	beq r5, r3, .LBB13_6
.LBB13_4:
	addi r7, r0, 45
	beq r5, r7, .LBB13_7
.LBB13_5:
	addi r6, r6, -1
.LBB13_6:
	addi r1, r0, 1
.LBB13_7:
	ldbu r7, r6+0
	lui r5, %hi(.LCPI13_0)
	addi r5, r5, %lo(.LCPI13_0)
	ldw r9, r5+4
	ldw r8, r5+0
	addi r5, r0, 48
	bne r7, r5, .LBB13_24
.LBB13_8:
	ldbu r7, r6+1
	ori  r7, r7, 32
	bne r7, r4, .LBB13_24
.LBB13_9:
	addi r4, r6, 2
	addi r15, r0, 0
	addi r17, r0, 46
	addi r18, r0, 1
	add r20, r15, r0
	add r16, r15, r0
	add r10, r15, r0
	add r6, r8, r0
	add r7, r9, r0
	add r19, r15, r0
	ldbu r22, r4+0
	beq r22, r17, .LBB13_19
.LBB13_10:
	add r21, r22, r14
	ldbu r21, r21+0
	andi r21, r21, 16
	beq r21, r15, .LBB13_32
.LBB13_11:
	bne r20, r15, .LBB13_14
.LBB13_12:
	bne r22, r5, .LBB13_14
.LBB13_13:
	addi r16, r16, 1
	addi r20, r0, 0
	jal r0, .LBB13_18
.LBB13_14:
	addi r21, r20, 1
	addi r23, r0, 29
	bgt r20, r23, .LBB13_16
.LBB13_15:
	slli r20, r22, 24
	srai r20, r20, 24
	add r22, r20, r14
	ldbu r22, r22+0
	andi r22, r22, 2
	addi r23, r0, 0
	seq r22, r22, r23
	addi r24, r20, -48
	ori  r20, r20, 32
	addi r20, r20, -87
	xor r20, r20, r24
	sub r22, r23, r22
	and r20, r20, r22
	xor r20, r24, r20
	fcvt.d.w r22, r20
	lui r20, %hi(.LCPI13_1)
	addi r20, r20, %lo(.LCPI13_1)
	ldw r25, r20+4
	ldw r24, r20+0
	fmul.d r6, r6, r24
	fadd.d r6, r6, r22
	jal r0, .LBB13_17
.LBB13_16:
	addi r10, r10, 1
.LBB13_17:
	add r20, r21, r0
.LBB13_18:
	addi r21, r0, 0
	sne r19, r19, r21
	sub r10, r10, r19
	jal r0, .LBB13_21
.LBB13_19:
	bne r19, r15, .LBB13_32
.LBB13_20:
	add r19, r18, r0
.LBB13_21:
	addi r4, r4, 1
	ldbu r22, r4+0
	bne r22, r17, .LBB13_10
	jal r0, .LBB13_19
.LBB13_22:
	addi r4, fp, -68
	add r3, r11, r0
	jal r31, strtod
.LBB13_23:
	add r8, r1, r0
	add r9, r2, r0
.LBB13_24:
	stw r12+4, r9
	stw r12+0, r8
	ldw r1, r13+0
	beq r1, r11, .LBB13_30
.LBB13_25:
	ldw r4, r13+0
	ldbu r3, r4+0
	add r1, r3, r14
	ldbu r1, r1+0
	andi r5, r1, 8
	addi r1, r0, 0
	beq r5, r1, .LBB13_29
.LBB13_26:
	addi r4, r4, 1
.LBB13_27:
	stw r13+0, r4
	ldbu r3, r4+0
	add r5, r3, r14
	ldbu r5, r5+0
	andi r5, r5, 8
	addi r4, r4, 1
	bne r5, r1, .LBB13_27
.LBB13_28:
	addi r4, r4, -1
.LBB13_29:
	seq r3, r3, r1
	sub r1, r1, r3
	and r1, r4, r1
	jal r0, .LBB13_31
.LBB13_30:
	addi r1, r0, 0
.LBB13_31:
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
.LBB13_32:
	addi r15, r0, 0
	sub r5, r15, r20
	beq r16, r5, .LBB13_24
.LBB13_33:
	stw r13+0, r4
	slli r5, r10, 2
	ldbu r10, r4+0
	ori  r10, r10, 32
	addi r16, r0, 112
	bne r10, r16, .LBB13_44
.LBB13_34:
	ldbu r10, r4+1
	beq r10, r3, .LBB13_37
.LBB13_35:
	addi r3, r0, 45
	bne r10, r3, .LBB13_38
.LBB13_36:
	addi r4, r4, 2
	add r3, r15, r0
	jal r0, .LBB13_40
.LBB13_37:
	addi r4, r4, 2
	jal r0, .LBB13_39
.LBB13_38:
	addi r4, r4, 1
.LBB13_39:
	addi r3, r0, 1
.LBB13_40:
	ldbu r10, r4+0
	add r16, r10, r14
	ldbu r16, r16+0
	andi r16, r16, 2
	beq r16, r15, .LBB13_24
.LBB13_41:
	addi r8, r0, 0
	addi r9, r0, 10
	add r17, r8, r0
.LBB13_42:
	mul r17, r17, r9
	addi r16, r4, 1
	slli r10, r10, 24
	srai r10, r10, 24
	add r10, r10, r17
	addi r17, r10, -48
	ldbu r10, r4+1
	add r4, r10, r14
	ldbu r4, r4+0
	andi r18, r4, 2
	add r4, r16, r0
	bne r18, r8, .LBB13_42
.LBB13_43:
	addi r4, r0, 0
	sub r8, r4, r17
	sub r3, r4, r3
	xor r4, r17, r8
	and r3, r4, r3
	xor r3, r8, r3
	add r5, r3, r5
	stw r13+0, r16
.LBB13_44:
	fneg.d r8, r6
	sub r1, r15, r1
	xor r3, r7, r9
	and r3, r3, r1
	xor r4, r9, r3
	xor r3, r6, r8
	and r1, r3, r1
	xor r3, r8, r1
	jal r31, ldexp
	jal r0, .LBB13_23
.Lfunc_end13:
	.size	l_str2dloc, .Lfunc_end13-l_str2dloc
                                        # -- End function
	.type	luaO_ceillog2.log_2,@object     # @luaO_ceillog2.log_2
	.section	.rodata,"a",@progbits
luaO_ceillog2.log_2:
	.ascii	"\000\001\002\002\003\003\003\003\004\004\004\004\004\004\004\004\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
	.size	luaO_ceillog2.log_2, 256

	.hidden	luai_ctype_
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"(null)"
	.size	.L.str, 7

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"%p"
	.size	.L.str.1, 3

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"%"
	.size	.L.str.2, 2

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"invalid option '%%%c' to 'lua_pushfstring'"
	.size	.L.str.3, 43

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"..."
	.size	.L.str.4, 4

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"[string \""
	.size	.L.str.5, 10

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"\"]"
	.size	.L.str.6, 3

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	".xXnN"
	.size	.L.str.7, 6

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"%ld"
	.size	.L.str.8, 4

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"%.14g"
	.size	.L.str.9, 6

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"-0123456789"
	.size	.L.str.10, 12

	.hidden	luaV_tointegerns
	.hidden	luaT_trybinTM
	.hidden	luaS_newlstr
	.hidden	luaG_runerror
	.hidden	luaV_mod
	.hidden	luaV_idiv
	.hidden	luaV_shiftl
	.hidden	luaV_modf
	.hidden	luaV_concat
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
