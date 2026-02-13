	.file	"lvm.c"
	.text
	.hidden	luaV_tonumber_                  # -- Begin function luaV_tonumber_
	.globl	luaV_tonumber_
	.p2align	2
	.type	luaV_tonumber_,@function
luaV_tonumber_:                         # @luaV_tonumber_
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
	ldbu r1, r3+8
	addi r12, r0, 3
	bne r1, r12, .LBB0_3
.LBB0_1:
	ldw r1, r3+0
	fcvt.d.w r6, r1
.LBB0_2:
	stw r4+4, r7
	stw r4+0, r6
	addi r5, r0, 1
	jal r0, .LBB0_8
.LBB0_3:
	andi r1, r1, 15
	addi r5, r0, 0
	addi r6, r0, 4
	bne r1, r6, .LBB0_8
.LBB0_4:
	add r13, r5, r0
	add r14, r4, r0
	ldw r15, r3+0
	addi r3, r15, 16
	addi r11, fp, -36
	add r4, r11, r0
	jal r31, luaO_str2num
	ldbu r3, r15+7
	addi r4, r0, 255
	bne r3, r4, .LBB0_6
.LBB0_5:
	ldw r3, r15+12
.LBB0_6:
	addi r3, r3, 1
	add r5, r13, r0
	bne r1, r3, .LBB0_8
.LBB0_7:
	add r4, r14, r0
	ldbu r1, r11+8
	seq r1, r1, r12
	ldw r3, r11+0
	fcvt.d.w r8, r3
	ldw r5, r11+4
	addi r6, r0, 0
	sub r1, r6, r1
	xor r6, r9, r5
	and r6, r6, r1
	xor r7, r5, r6
	xor r5, r8, r3
	and r1, r5, r1
	xor r6, r3, r1
	jal r0, .LBB0_2
.LBB0_8:
	add r1, r5, r0
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
.Lfunc_end0:
	.size	luaV_tonumber_, .Lfunc_end0-luaV_tonumber_
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaV_flttointeger
.LCPI1_0:
	.quad	0x3ff0000000000000              # double 1
.LCPI1_1:
	.quad	0xc1e0000000000000              # double -2147483648
.LCPI1_2:
	.quad	0x41e0000000000000              # double 2147483648
	.text
	.hidden	luaV_flttointeger
	.globl	luaV_flttointeger
	.p2align	2
	.type	luaV_flttointeger,@function
luaV_flttointeger:                      # @luaV_flttointeger
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r14
	stw fp+-16, r15
	stw fp+-20, lr
	add r12, r6, r0
	add r11, r5, r0
	add r15, r4, r0
	add r14, r3, r0
	add r4, r15, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r3, r14, r4
	addi r1, r0, 0
	bne r3, r1, .LBB1_4
.LBB1_1:
	beq r12, r1, .LBB1_6
.LBB1_2:
	addi r1, r0, 2
	bne r12, r1, .LBB1_4
.LBB1_3:
	lui r1, %hi(.LCPI1_0)
	addi r1, r1, %lo(.LCPI1_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fadd.d r4, r4, r6
.LBB1_4:
	lui r1, %hi(.LCPI1_1)
	addi r1, r1, %lo(.LCPI1_1)
	ldw r7, r1+4
	ldw r6, r1+0
	fle.d r1, r6, r4
	lui r3, %hi(.LCPI1_2)
	addi r3, r3, %lo(.LCPI1_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	and r12, r1, r3
	addi r1, r0, 1
	bne r12, r1, .LBB1_6
.LBB1_5:
	fcvt.w.d r1, r4
	stw r11+0, r1
.LBB1_6:
	add r1, r12, r0
	ldw lr, fp+-20
	ldw r15, fp+-16
	ldw r14, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end1:
	.size	luaV_flttointeger, .Lfunc_end1-luaV_flttointeger
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaV_tointegerns
.LCPI2_0:
	.quad	0x3ff0000000000000              # double 1
.LCPI2_1:
	.quad	0xc1e0000000000000              # double -2147483648
.LCPI2_2:
	.quad	0x41e0000000000000              # double 2147483648
	.text
	.hidden	luaV_tointegerns
	.globl	luaV_tointegerns
	.p2align	2
	.type	luaV_tointegerns,@function
luaV_tointegerns:                       # @luaV_tointegerns
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r14
	stw fp+-16, r15
	stw fp+-20, lr
	ldbu r1, r3+8
	addi r6, r0, 3
	beq r1, r6, .LBB2_8
.LBB2_1:
	addi r6, r0, 19
	bne r1, r6, .LBB2_9
.LBB2_2:
	add r12, r5, r0
	add r11, r4, r0
	ldw r15, r3+4
	ldw r14, r3+0
	add r3, r14, r0
	add r4, r15, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r14, r4
	addi r3, r0, 0
	bne r1, r3, .LBB2_6
.LBB2_3:
	add r1, r12, r0
	beq r12, r3, .LBB2_10
.LBB2_4:
	addi r3, r0, 2
	bne r1, r3, .LBB2_6
.LBB2_5:
	lui r1, %hi(.LCPI2_0)
	addi r1, r1, %lo(.LCPI2_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fadd.d r4, r4, r6
.LBB2_6:
	lui r1, %hi(.LCPI2_1)
	addi r1, r1, %lo(.LCPI2_1)
	ldw r7, r1+4
	ldw r6, r1+0
	fle.d r1, r6, r4
	lui r3, %hi(.LCPI2_2)
	addi r3, r3, %lo(.LCPI2_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	and r1, r1, r3
	addi r3, r0, 1
	bne r1, r3, .LBB2_10
.LBB2_7:
	fcvt.w.d r3, r4
	stw r11+0, r3
	jal r0, .LBB2_10
.LBB2_8:
	ldw r1, r3+0
	stw r4+0, r1
	addi r1, r0, 1
	jal r0, .LBB2_10
.LBB2_9:
	addi r1, r0, 0
.LBB2_10:
	ldw lr, fp+-20
	ldw r15, fp+-16
	ldw r14, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end2:
	.size	luaV_tointegerns, .Lfunc_end2-luaV_tointegerns
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaV_tointeger
.LCPI3_0:
	.quad	0x3ff0000000000000              # double 1
.LCPI3_1:
	.quad	0xc1e0000000000000              # double -2147483648
.LCPI3_2:
	.quad	0x41e0000000000000              # double 2147483648
	.text
	.hidden	luaV_tointeger
	.globl	luaV_tointeger
	.p2align	2
	.type	luaV_tointeger,@function
luaV_tointeger:                         # @luaV_tointeger
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
	add r12, r5, r0
	add r11, r4, r0
	add r13, r3, r0
	ldbu r1, r3+8
	andi r1, r1, 15
	addi r3, r0, 4
	bne r1, r3, .LBB3_4
.LBB3_1:
	ldw r14, r13+0
	addi r3, r14, 16
	addi r4, fp, -36
	jal r31, luaO_str2num
	ldbu r3, r14+7
	addi r4, r0, 255
	bne r3, r4, .LBB3_3
.LBB3_2:
	ldw r3, r14+12
.LBB3_3:
	addi r3, r3, 1
	sne r3, r1, r3
	jal r0, .LBB3_5
.LBB3_4:
	addi r3, r0, 1
.LBB3_5:
	addi r1, r0, 0
	sub r3, r1, r3
	addi r4, fp, -36
	xor r5, r13, r4
	and r3, r5, r3
	xor r3, r4, r3
	ldbu r4, r3+8
	addi r5, r0, 3
	beq r4, r5, .LBB3_13
.LBB3_6:
	addi r5, r0, 19
	bne r4, r5, .LBB3_14
.LBB3_7:
	ldw r15, r3+4
	ldw r14, r3+0
	add r3, r14, r0
	add r4, r15, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r3, r14, r4
	addi r1, r0, 0
	bne r3, r1, .LBB3_11
.LBB3_8:
	beq r12, r1, .LBB3_15
.LBB3_9:
	addi r1, r0, 2
	bne r12, r1, .LBB3_11
.LBB3_10:
	lui r1, %hi(.LCPI3_0)
	addi r1, r1, %lo(.LCPI3_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fadd.d r4, r4, r6
.LBB3_11:
	lui r1, %hi(.LCPI3_1)
	addi r1, r1, %lo(.LCPI3_1)
	ldw r7, r1+4
	ldw r6, r1+0
	fle.d r1, r6, r4
	lui r3, %hi(.LCPI3_2)
	addi r3, r3, %lo(.LCPI3_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	and r1, r1, r3
	addi r3, r0, 1
	bne r1, r3, .LBB3_14
.LBB3_12:
	fcvt.w.d r3, r4
	stw r11+0, r3
	jal r0, .LBB3_14
.LBB3_13:
	ldw r1, r3+0
	stw r11+0, r1
	addi r1, r0, 1
.LBB3_14:
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
.LBB3_15:
	add r1, r12, r0
	jal r0, .LBB3_14
.Lfunc_end3:
	.size	luaV_tointeger, .Lfunc_end3-luaV_tointeger
                                        # -- End function
	.hidden	luaV_finishget                  # -- Begin function luaV_finishget
	.globl	luaV_finishget
	.p2align	2
	.type	luaV_finishget,@function
luaV_finishget:                         # @luaV_finishget
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
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, r19
	stw fp+-40, lr
	add r1, r7, r0
	add r11, r6, r0
	add r13, r5, r0
	add r15, r4, r0
	add r12, r3, r0
	addi r17, r0, 2000
	addi r14, r0, 0
	addi r18, r0, 6
	addi r19, r0, 69
	jal r0, .LBB4_2
.LBB4_1:
	addi r17, r17, -1
	add r15, r16, r0
	beq r17, r14, .LBB4_17
.LBB4_2:
	beq r1, r14, .LBB4_7
.LBB4_3:
	ldw r1, r15+0
	ldw r3, r1+24
	add r16, r14, r0
	beq r3, r14, .LBB4_6
.LBB4_4:
	ldbu r1, r3+6
	andi r1, r1, 1
	add r16, r14, r0
	bne r1, r14, .LBB4_6
.LBB4_5:
	ldw r1, r12+16
	ldw r5, r1+160
	add r4, r14, r0
	jal r31, luaT_gettm
	add r16, r1, r0
.LBB4_6:
	bne r16, r14, .LBB4_8
	jal r0, .LBB4_13
.LBB4_7:
	add r3, r12, r0
	add r4, r15, r0
	add r5, r14, r0
	jal r31, luaT_gettmbyobj
	add r16, r1, r0
	ldbu r1, r1+8
	andi r1, r1, 15
	beq r1, r14, .LBB4_16
.LBB4_8:
	ldbu r3, r16+8
	andi r1, r3, 15
	beq r1, r18, .LBB4_12
.LBB4_9:
	add r1, r14, r0
	bne r3, r19, .LBB4_1
.LBB4_10:
	ldw r3, r16+0
	add r4, r13, r0
	jal r31, luaH_get
	ldbu r3, r1+8
	andi r3, r3, 15
	beq r3, r14, .LBB4_1
.LBB4_11:
	ldw r3, r1+0
	ldw r4, r1+4
	stw r11+4, r4
	stw r11+0, r3
	ldbu r1, r1+8
	jal r0, .LBB4_14
.LBB4_12:
	add r3, r12, r0
	add r4, r16, r0
	add r5, r15, r0
	add r6, r13, r0
	add r7, r11, r0
	jal r31, luaT_callTMres
	jal r0, .LBB4_15
.LBB4_13:
	addi r1, r0, 0
.LBB4_14:
	stb r11+8, r1
.LBB4_15:
	ldw lr, fp+-40
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
	addi sp, sp, 56
	jalr r0, r31, 0
.LBB4_16:
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	add r3, r12, r0
	add r4, r15, r0
	jal r31, luaG_typeerror
.LBB4_17:
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	add r3, r12, r0
	jal r31, luaG_runerror
.Lfunc_end4:
	.size	luaV_finishget, .Lfunc_end4-luaV_finishget
                                        # -- End function
	.hidden	luaV_finishset                  # -- Begin function luaV_finishset
	.globl	luaV_finishset
	.p2align	2
	.type	luaV_finishset,@function
luaV_finishset:                         # @luaV_finishset
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
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, r19
	stw fp+-40, r20
	stw fp+-44, r21
	stw fp+-48, r22
	stw fp+-52, lr
	add r15, r7, r0
	add r12, r6, r0
	add r13, r5, r0
	add r16, r4, r0
	add r11, r3, r0
	addi r19, r0, 2000
	addi r20, r0, 0
	addi r14, r0, 1
	addi r21, r0, 6
	addi r22, r0, 69
	jal r0, .LBB5_2
.LBB5_1:
	addi r19, r19, -1
	add r16, r17, r0
	beq r19, r20, .LBB5_23
.LBB5_2:
	beq r15, r20, .LBB5_7
.LBB5_3:
	ldw r18, r16+0
	ldw r3, r18+24
	add r17, r20, r0
	beq r3, r20, .LBB5_6
.LBB5_4:
	ldbu r1, r3+6
	andi r1, r1, 2
	add r17, r20, r0
	bne r1, r20, .LBB5_6
.LBB5_5:
	ldw r1, r11+16
	ldw r5, r1+164
	add r4, r14, r0
	jal r31, luaT_gettm
	add r17, r1, r0
.LBB5_6:
	bne r17, r20, .LBB5_8
	jal r0, .LBB5_16
.LBB5_7:
	add r3, r11, r0
	add r4, r16, r0
	add r5, r14, r0
	jal r31, luaT_gettmbyobj
	add r17, r1, r0
	ldbu r1, r1+8
	andi r1, r1, 15
	beq r1, r20, .LBB5_22
.LBB5_8:
	ldbu r1, r17+8
	andi r3, r1, 15
	beq r3, r21, .LBB5_15
.LBB5_9:
	add r15, r20, r0
	bne r1, r22, .LBB5_1
.LBB5_10:
	ldw r3, r17+0
	add r4, r13, r0
	jal r31, luaH_get
	add r15, r1, r0
	ldbu r1, r1+8
	andi r1, r1, 15
	beq r1, r20, .LBB5_1
.LBB5_11:
	ldw r1, r12+0
	ldw r3, r12+4
	stw r15+4, r3
	stw r15+0, r1
	ldbu r1, r12+8
	stb r15+8, r1
	ldbu r1, r12+8
	andi r3, r1, 64
	addi r1, r0, 0
	beq r3, r1, .LBB5_21
.LBB5_12:
	ldw r4, r17+0
	ldbu r3, r4+5
	andi r3, r3, 32
	beq r3, r1, .LBB5_21
.LBB5_13:
	ldw r3, r12+0
	ldbu r3, r3+5
	andi r3, r3, 24
	beq r3, r1, .LBB5_21
.LBB5_14:
	add r3, r11, r0
	jal r0, .LBB5_20
.LBB5_15:
	add r3, r11, r0
	add r4, r17, r0
	add r5, r16, r0
	add r6, r13, r0
	add r7, r12, r0
	jal r31, luaT_callTM
	jal r0, .LBB5_21
.LBB5_16:
	add r3, r11, r0
	add r4, r18, r0
	add r5, r13, r0
	add r6, r15, r0
	add r7, r12, r0
	jal r31, luaH_finishset
	ldbu r1, r18+6
	andi r1, r1, 192
	stb r18+6, r1
	ldbu r1, r12+8
	andi r3, r1, 64
	addi r1, r0, 0
	beq r3, r1, .LBB5_21
.LBB5_17:
	ldbu r3, r18+5
	andi r3, r3, 32
	beq r3, r1, .LBB5_21
.LBB5_18:
	ldw r3, r12+0
	ldbu r3, r3+5
	andi r3, r3, 24
	beq r3, r1, .LBB5_21
.LBB5_19:
	add r3, r11, r0
	add r4, r18, r0
.LBB5_20:
	jal r31, luaC_barrierback_
.LBB5_21:
	ldw lr, fp+-52
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
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB5_22:
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	add r3, r11, r0
	add r4, r16, r0
	jal r31, luaG_typeerror
.LBB5_23:
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	add r3, r11, r0
	jal r31, luaG_runerror
.Lfunc_end5:
	.size	luaV_finishset, .Lfunc_end5-luaV_finishset
                                        # -- End function
	.hidden	luaV_lessthan                   # -- Begin function luaV_lessthan
	.globl	luaV_lessthan
	.p2align	2
	.type	luaV_lessthan,@function
luaV_lessthan:                          # @luaV_lessthan
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldbu r7, r4+8
	andi r1, r7, 15
	addi r6, r0, 3
	bne r1, r6, .LBB6_5
.LBB6_1:
	ldbu r1, r5+8
	andi r8, r1, 15
	bne r8, r6, .LBB6_5
.LBB6_2:
	bne r7, r6, .LBB6_7
.LBB6_3:
	ldw r3, r4+0
	bne r1, r6, .LBB6_9
.LBB6_4:
	ldw r1, r5+0
	slt r1, r3, r1
	jal r0, .LBB6_6
.LBB6_5:
	jal r31, lessthanothers
.LBB6_6:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB6_7:
	ldw r7, r4+4
	ldw r6, r4+0
	addi r3, r0, 19
	bne r1, r3, .LBB6_10
.LBB6_8:
	ldw r9, r5+4
	ldw r8, r5+0
	flt.d r1, r6, r8
	jal r0, .LBB6_6
.LBB6_9:
	ldw r7, r5+4
	ldw r6, r5+0
	fcvt.d.w r4, r3
	flt.d r1, r4, r6
	jal r0, .LBB6_6
.LBB6_10:
	ldw r1, r5+0
	fcvt.d.w r4, r1
	flt.d r1, r6, r4
	jal r0, .LBB6_6
.Lfunc_end6:
	.size	luaV_lessthan, .Lfunc_end6-luaV_lessthan
                                        # -- End function
	.p2align	2                               # -- Begin function lessthanothers
	.type	lessthanothers,@function
lessthanothers:                         # @lessthanothers
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
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, r19
	stw fp+-40, lr
	ldbu r1, r4+8
	andi r6, r1, 15
	addi r1, r0, 4
	bne r6, r1, .LBB7_14
.LBB7_1:
	ldbu r6, r5+8
	andi r6, r6, 15
	bne r6, r1, .LBB7_14
.LBB7_2:
	ldw r1, r4+0
	ldw r3, r5+0
	ldbu r14, r1+7
	addi r4, r0, 255
	beq r14, r4, .LBB7_16
.LBB7_3:
	ldbu r15, r3+7
	beq r15, r4, .LBB7_17
.LBB7_4:
	addi r11, r1, 16
	addi r12, r3, 16
	addi r16, r0, 0
	addi r17, r0, -1
	addi r18, r0, 1
                                        # implicit-def: $r19
.LBB7_5:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, strcoll
	beq r1, r16, .LBB7_7
.LBB7_6:
	add r19, r1, r0
	jal r0, .LBB7_11
.LBB7_7:
	add r3, r11, r0
	jal r31, strlen
	add r13, r1, r0
	add r3, r12, r0
	jal r31, strlen
	bne r1, r15, .LBB7_9
.LBB7_8:
	add r19, r16, r0
	jal r0, .LBB7_11
.LBB7_9:
	bne r13, r14, .LBB7_12
.LBB7_10:
	add r19, r17, r0
.LBB7_11:
	add r1, r16, r0
	bne r1, r16, .LBB7_5
	jal r0, .LBB7_13
.LBB7_12:
	addi r3, r13, 1
	addi r1, r1, 1
	add r11, r11, r3
	sub r14, r14, r3
	add r12, r12, r1
	sub r15, r15, r1
	add r1, r18, r0
	bne r1, r16, .LBB7_5
.LBB7_13:
	srli r1, r19, 31
	jal r0, .LBB7_15
.LBB7_14:
	addi r6, r0, 20
	jal r31, luaT_callorderTM
.LBB7_15:
	ldw lr, fp+-40
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
	addi sp, sp, 56
	jalr r0, r31, 0
.LBB7_16:
	ldw r14, r1+12
	ldbu r15, r3+7
	bne r15, r4, .LBB7_4
.LBB7_17:
	ldw r15, r3+12
	jal r0, .LBB7_4
.Lfunc_end7:
	.size	lessthanothers, .Lfunc_end7-lessthanothers
                                        # -- End function
	.hidden	luaV_lessequal                  # -- Begin function luaV_lessequal
	.globl	luaV_lessequal
	.p2align	2
	.type	luaV_lessequal,@function
luaV_lessequal:                         # @luaV_lessequal
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldbu r7, r4+8
	andi r1, r7, 15
	addi r6, r0, 3
	bne r1, r6, .LBB8_5
.LBB8_1:
	ldbu r1, r5+8
	andi r8, r1, 15
	bne r8, r6, .LBB8_5
.LBB8_2:
	bne r7, r6, .LBB8_7
.LBB8_3:
	ldw r3, r4+0
	bne r1, r6, .LBB8_9
.LBB8_4:
	ldw r1, r5+0
	sle r1, r3, r1
	jal r0, .LBB8_6
.LBB8_5:
	jal r31, lessequalothers
.LBB8_6:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB8_7:
	ldw r7, r4+4
	ldw r6, r4+0
	addi r3, r0, 19
	bne r1, r3, .LBB8_10
.LBB8_8:
	ldw r9, r5+4
	ldw r8, r5+0
	fle.d r1, r6, r8
	jal r0, .LBB8_6
.LBB8_9:
	ldw r7, r5+4
	ldw r6, r5+0
	fcvt.d.w r4, r3
	fle.d r1, r4, r6
	jal r0, .LBB8_6
.LBB8_10:
	ldw r1, r5+0
	fcvt.d.w r4, r1
	fle.d r1, r6, r4
	jal r0, .LBB8_6
.Lfunc_end8:
	.size	luaV_lessequal, .Lfunc_end8-luaV_lessequal
                                        # -- End function
	.p2align	2                               # -- Begin function lessequalothers
	.type	lessequalothers,@function
lessequalothers:                        # @lessequalothers
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
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, r19
	stw fp+-40, lr
	ldbu r1, r4+8
	andi r6, r1, 15
	addi r1, r0, 4
	bne r6, r1, .LBB9_14
.LBB9_1:
	ldbu r6, r5+8
	andi r6, r6, 15
	bne r6, r1, .LBB9_14
.LBB9_2:
	ldw r1, r4+0
	ldw r3, r5+0
	ldbu r14, r1+7
	addi r4, r0, 255
	beq r14, r4, .LBB9_16
.LBB9_3:
	ldbu r15, r3+7
	beq r15, r4, .LBB9_17
.LBB9_4:
	addi r11, r1, 16
	addi r12, r3, 16
	addi r16, r0, 0
	addi r17, r0, -1
	addi r18, r0, 1
                                        # implicit-def: $r19
.LBB9_5:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, strcoll
	beq r1, r16, .LBB9_7
.LBB9_6:
	add r19, r1, r0
	jal r0, .LBB9_11
.LBB9_7:
	add r3, r11, r0
	jal r31, strlen
	add r13, r1, r0
	add r3, r12, r0
	jal r31, strlen
	bne r1, r15, .LBB9_9
.LBB9_8:
	sne r19, r13, r14
	jal r0, .LBB9_11
.LBB9_9:
	bne r13, r14, .LBB9_12
.LBB9_10:
	add r19, r17, r0
.LBB9_11:
	add r1, r16, r0
	bne r1, r16, .LBB9_5
	jal r0, .LBB9_13
.LBB9_12:
	addi r3, r13, 1
	addi r1, r1, 1
	add r11, r11, r3
	sub r14, r14, r3
	add r12, r12, r1
	sub r15, r15, r1
	add r1, r18, r0
	bne r1, r16, .LBB9_5
.LBB9_13:
	addi r1, r0, 1
	slt r1, r19, r1
	jal r0, .LBB9_15
.LBB9_14:
	addi r6, r0, 21
	jal r31, luaT_callorderTM
.LBB9_15:
	ldw lr, fp+-40
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
	addi sp, sp, 56
	jalr r0, r31, 0
.LBB9_16:
	ldw r14, r1+12
	ldbu r15, r3+7
	bne r15, r4, .LBB9_4
.LBB9_17:
	ldw r15, r3+12
	jal r0, .LBB9_4
.Lfunc_end9:
	.size	lessequalothers, .Lfunc_end9-lessequalothers
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaV_equalobj
.LCPI10_0:
	.quad	0xc1e0000000000000              # double -2147483648
.LCPI10_1:
	.quad	0x41e0000000000000              # double 2147483648
	.text
	.hidden	luaV_equalobj
	.globl	luaV_equalobj
	.p2align	2
	.type	luaV_equalobj,@function
luaV_equalobj:                          # @luaV_equalobj
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
	ldbu r6, r4+8
	andi r7, r6, 63
	ldbu r8, r5+8
	andi r1, r8, 63
	bne r7, r1, .LBB10_4
.LBB10_1:
	addi r1, r0, 22
	bgtu r7, r1, .LBB10_3
.LBB10_2:
	addi r1, r0, 1
	slli r6, r7, 2
	lui r7, %hi(.LJTI10_0)
	addi r7, r7, %lo(.LJTI10_0)
	add r6, r7, r6
	ldw r6, r6+0
	jalr r0, r6, 0
.LBB10_3:
	ldw r1, r4+0
	ldw r3, r5+0
	seq r1, r1, r3
	jal r0, .LBB10_40
.LBB10_4:
	andi r3, r6, 15
	addi r1, r0, 0
	addi r12, r0, 3
	bne r3, r12, .LBB10_40
.LBB10_5:
	andi r3, r8, 15
	bne r3, r12, .LBB10_40
.LBB10_6:
	beq r6, r12, .LBB10_10
.LBB10_7:
	addi r3, r0, 19
	bne r6, r3, .LBB10_40
.LBB10_8:
	ldw r15, r4+4
	ldw r14, r4+0
	add r3, r14, r0
	add r4, r15, r0
	add r11, r5, r0
	jal r31, floor
	add r6, r1, r0
	add r7, r2, r0
	feq.d r1, r14, r6
	xori r3, r1, 1
	addi r1, r0, 0
	bne r3, r1, .LBB10_40
.LBB10_9:
	add r5, r11, r0
	lui r1, %hi(.LCPI10_0)
	addi r1, r1, %lo(.LCPI10_0)
	ldw r9, r1+4
	ldw r8, r1+0
	flt.d r1, r6, r8
	lui r3, %hi(.LCPI10_1)
	addi r3, r3, %lo(.LCPI10_1)
	ldw r9, r3+4
	ldw r8, r3+0
	fle.d r3, r8, r6
	or  r3, r1, r3
	fcvt.w.d r11, r6
	jal r0, .LBB10_11
.LBB10_10:
	ldw r11, r4+0
	addi r3, r0, 0
.LBB10_11:
	addi r1, r0, 0
	bne r3, r1, .LBB10_40
.LBB10_12:
	ldbu r3, r5+8
	beq r3, r12, .LBB10_37
.LBB10_13:
	addi r4, r0, 19
	bne r3, r4, .LBB10_40
.LBB10_14:
	ldw r13, r5+4
	ldw r12, r5+0
	add r3, r12, r0
	add r4, r13, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r12, r4
	xori r3, r1, 1
	addi r1, r0, 0
	bne r3, r1, .LBB10_40
.LBB10_15:
	lui r1, %hi(.LCPI10_0)
	addi r1, r1, %lo(.LCPI10_0)
	ldw r7, r1+4
	ldw r6, r1+0
	flt.d r1, r4, r6
	lui r3, %hi(.LCPI10_1)
	addi r3, r3, %lo(.LCPI10_1)
	ldw r7, r3+4
	ldw r6, r3+0
	fle.d r3, r6, r4
	or  r6, r1, r3
	fcvt.w.d r3, r4
	jal r0, .LBB10_38
.LBB10_16:
	ldw r6, r4+0
	ldw r7, r5+0
	beq r6, r7, .LBB10_40
.LBB10_17:
	addi r1, r0, 0
	beq r3, r1, .LBB10_40
.LBB10_18:
	ldw r1, r6+12
	addi r11, r0, 0
	add r6, r11, r0
	beq r1, r11, .LBB10_21
.LBB10_19:
	ldbu r6, r1+6
	andi r7, r6, 32
	addi r6, r0, 0
	bne r7, r6, .LBB10_21
.LBB10_20:
	ldw r6, r3+16
	ldw r6, r6+180
	addi r7, r0, 5
	add r12, r3, r0
	add r3, r1, r0
	add r13, r4, r0
	add r4, r7, r0
	add r14, r5, r0
	add r5, r6, r0
	jal r31, luaT_gettm
	add r3, r12, r0
	add r4, r13, r0
	add r5, r14, r0
	add r6, r1, r0
.LBB10_21:
	bne r6, r11, .LBB10_34
.LBB10_22:
	ldw r1, r5+0
	ldw r1, r1+12
	jal r0, .LBB10_31
.LBB10_23:
	ldw r3, r4+0
	ldw r4, r5+0
	jal r31, luaS_eqlngstr
	jal r0, .LBB10_40
.LBB10_24:
	ldw r6, r4+0
	ldw r7, r5+0
	beq r6, r7, .LBB10_40
.LBB10_25:
	addi r1, r0, 0
	beq r3, r1, .LBB10_40
.LBB10_26:
	ldw r1, r6+24
	addi r11, r0, 0
	add r6, r11, r0
	beq r1, r11, .LBB10_29
.LBB10_27:
	ldbu r6, r1+6
	andi r7, r6, 32
	addi r6, r0, 0
	bne r7, r6, .LBB10_29
.LBB10_28:
	ldw r6, r3+16
	ldw r6, r6+180
	addi r7, r0, 5
	add r12, r3, r0
	add r3, r1, r0
	add r13, r4, r0
	add r4, r7, r0
	add r14, r5, r0
	add r5, r6, r0
	jal r31, luaT_gettm
	add r3, r12, r0
	add r4, r13, r0
	add r5, r14, r0
	add r6, r1, r0
.LBB10_29:
	bne r6, r11, .LBB10_34
.LBB10_30:
	ldw r1, r5+0
	ldw r1, r1+24
.LBB10_31:
	addi r6, r0, 0
	beq r1, r6, .LBB10_34
.LBB10_32:
	ldbu r6, r1+6
	andi r7, r6, 32
	addi r6, r0, 0
	bne r7, r6, .LBB10_34
.LBB10_33:
	ldw r6, r3+16
	ldw r6, r6+180
	addi r7, r0, 5
	add r11, r3, r0
	add r3, r1, r0
	add r12, r4, r0
	add r4, r7, r0
	add r13, r5, r0
	add r5, r6, r0
	jal r31, luaT_gettm
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	add r6, r1, r0
.LBB10_34:
	addi r1, r0, 0
	beq r6, r1, .LBB10_40
.LBB10_35:
	ldw r7, r3+12
	add r11, r3, r0
	add r1, r4, r0
	add r4, r6, r0
	add r6, r5, r0
	add r5, r1, r0
	jal r31, luaT_callTMres
	ldw r1, r11+12
	ldbu r1, r1+8
	addi r3, r0, 1
	sne r3, r1, r3
	andi r1, r1, 15
	addi r4, r0, 0
	sne r1, r1, r4
	and r1, r3, r1
	jal r0, .LBB10_40
.LBB10_36:
	ldw r7, r4+4
	ldw r6, r4+0
	ldw r9, r5+4
	ldw r8, r5+0
	feq.d r1, r6, r8
	jal r0, .LBB10_40
.LBB10_37:
	ldw r3, r5+0
	addi r6, r0, 0
.LBB10_38:
	addi r1, r0, 0
	bne r6, r1, .LBB10_40
.LBB10_39:
	seq r1, r11, r3
.LBB10_40:
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
.Lfunc_end10:
	.size	luaV_equalobj, .Lfunc_end10-luaV_equalobj
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI10_0:
	.word	.LBB10_40
	.word	.LBB10_40
	.word	.LBB10_3
	.word	.LBB10_3
	.word	.LBB10_3
	.word	.LBB10_24
	.word	.LBB10_3
	.word	.LBB10_16
	.word	.LBB10_3
	.word	.LBB10_3
	.word	.LBB10_3
	.word	.LBB10_3
	.word	.LBB10_3
	.word	.LBB10_3
	.word	.LBB10_3
	.word	.LBB10_3
	.word	.LBB10_3
	.word	.LBB10_40
	.word	.LBB10_3
	.word	.LBB10_36
	.word	.LBB10_23
	.word	.LBB10_3
	.word	.LBB10_3
                                        # -- End function
	.text
	.hidden	luaV_concat                     # -- Begin function luaV_concat
	.globl	luaV_concat
	.p2align	2
	.type	luaV_concat,@function
luaV_concat:                            # @luaV_concat
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
	addi r15, r0, 1
	bne r4, r15, .LBB11_2
.LBB11_1:
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
.LBB11_2:
	add r12, r4, r0
	add r11, r3, r0
	addi r16, r0, 4
	addi r17, r0, 68
	addi r18, r0, 0
	addi r19, r0, 3
	addi r20, r0, 255
	addi r23, r0, 2
	lui r1, 524288
	addi r22, r1, -21
	addi r24, r0, 40
	addi r1, r0, -12
	stw fp+-120, r1
	stw fp+-124, r17
	jal r0, .LBB11_6
.LBB11_3:
	add r23, r24, r0
	add r24, r18, r0
	add r18, r17, r0
	ldw r17, fp+-124
.LBB11_4:
	addi r1, r0, 0
	sub r1, r1, r26
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r25, r1
	stw r1+0, r13
	ldbu r3, r13+4
	ori  r3, r3, 64
	stb r1+8, r3
	addi r1, r26, -1
.LBB11_5:
	sub r12, r12, r1
	ldw r3, r11+12
	addi r4, r0, 0
	sub r1, r4, r1
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	stw r11+12, r1
	ble r12, r15, .LBB11_1
.LBB11_6:
	ldw r25, r11+12
	ldbu r1, r25+-16
	andi r1, r1, 15
	addi r1, r1, -3
	bgtu r1, r15, .LBB11_16
.LBB11_7:
	addi r13, r25, -12
	ldbu r1, r25+-4
	andi r1, r1, 15
	beq r1, r16, .LBB11_10
.LBB11_8:
	bne r1, r19, .LBB11_16
.LBB11_9:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaO_tostring
.LBB11_10:
	addi r14, r25, -24
	ldbu r1, r25+-4
	bne r1, r17, .LBB11_12
.LBB11_11:
	ldw r3, r13+0
	ldbu r3, r3+7
	beq r3, r18, .LBB11_37
.LBB11_12:
	ldbu r3, r25+-16
	bne r3, r17, .LBB11_14
.LBB11_13:
	ldw r3, r14+0
	ldbu r3, r3+7
	beq r3, r18, .LBB11_39
.LBB11_14:
	ldw r1, r13+0
	ldbu r13, r1+7
	beq r13, r20, .LBB11_18
.LBB11_15:
	add r26, r15, r0
	bge r12, r23, .LBB11_19
	jal r0, .LBB11_27
.LBB11_16:
	add r3, r11, r0
	jal r31, luaT_tryconcatTM
.LBB11_17:
	add r1, r15, r0
	jal r0, .LBB11_5
.LBB11_18:
	ldw r13, r1+12
	add r26, r15, r0
	blt r12, r23, .LBB11_27
.LBB11_19:
	ldbu r1, r14+8
	andi r1, r1, 15
	beq r1, r16, .LBB11_22
.LBB11_20:
	bne r1, r19, .LBB11_27
.LBB11_21:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaO_tostring
.LBB11_22:
	ldw r3, r14+0
	ldbu r1, r3+7
	bne r1, r20, .LBB11_24
.LBB11_23:
	ldw r1, r3+12
.LBB11_24:
	sub r3, r22, r13
	bgeu r1, r3, .LBB11_40
.LBB11_25:
	add r13, r1, r13
	addi r26, r26, 1
	addi r14, r14, -12
	bne r12, r26, .LBB11_19
.LBB11_26:
	add r26, r12, r0
.LBB11_27:
	bgtu r13, r24, .LBB11_32
.LBB11_28:
	addi r27, r26, 1
	ldw r1, fp+-120
	mul r1, r26, r1
	add r28, r25, r1
	add r21, r18, r0
	jal r0, .LBB11_30
.LBB11_29:
	addi r3, fp, -116
	add r3, r3, r21
	addi r4, r1, 16
	add r5, r14, r0
	jal r31, memcpy
	add r21, r14, r21
	addi r27, r27, -1
	addi r28, r28, 12
	addi r1, r0, 1
	ble r27, r1, .LBB11_36
.LBB11_30:
	ldw r1, r28+0
	ldbu r14, r1+7
	bne r14, r20, .LBB11_29
.LBB11_31:
	ldw r14, r1+12
	jal r0, .LBB11_29
.LBB11_32:
	add r17, r18, r0
	add r18, r24, r0
	add r24, r23, r0
	addi r27, r26, 1
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaS_createlngstrobj
	add r13, r1, r0
	addi r28, r1, 16
	ldw r1, fp+-120
	mul r1, r26, r1
	add r21, r25, r1
	addi r23, r0, 0
	jal r0, .LBB11_34
.LBB11_33:
	add r3, r28, r23
	addi r4, r1, 16
	add r5, r14, r0
	jal r31, memcpy
	add r23, r14, r23
	addi r27, r27, -1
	addi r21, r21, 12
	addi r1, r0, 1
	ble r27, r1, .LBB11_3
.LBB11_34:
	ldw r1, r21+0
	ldbu r14, r1+7
	bne r14, r20, .LBB11_33
.LBB11_35:
	ldw r14, r1+12
	jal r0, .LBB11_33
.LBB11_36:
	addi r4, fp, -116
	add r3, r11, r0
	add r5, r13, r0
	jal r31, luaS_newlstr
	add r13, r1, r0
	jal r0, .LBB11_4
.LBB11_37:
	ldbu r1, r25+-16
	andi r3, r1, 15
	add r1, r15, r0
	bne r3, r19, .LBB11_5
.LBB11_38:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaO_tostring
	jal r0, .LBB11_17
.LBB11_39:
	ldw r3, r25+-12
	ldw r4, r25+-8
	stw r25+-20, r4
	stw r25+-24, r3
	stb r25+-16, r1
	jal r0, .LBB11_17
.LBB11_40:
	addi r1, r0, 0
	sub r1, r1, r12
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r25, r1
	stw r11+12, r1
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	add r3, r11, r0
	jal r31, luaG_runerror
.Lfunc_end11:
	.size	luaV_concat, .Lfunc_end11-luaV_concat
                                        # -- End function
	.hidden	luaV_objlen                     # -- Begin function luaV_objlen
	.globl	luaV_objlen
	.p2align	2
	.type	luaV_objlen,@function
luaV_objlen:                            # @luaV_objlen
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
	add r12, r5, r0
	add r11, r4, r0
	ldbu r1, r5+8
	andi r1, r1, 63
	addi r4, r0, 4
	beq r1, r4, .LBB12_9
.LBB12_1:
	addi r4, r0, 20
	beq r1, r4, .LBB12_8
.LBB12_2:
	addi r4, r0, 5
	bne r1, r4, .LBB12_11
.LBB12_3:
	ldw r13, r12+0
	ldw r1, r13+24
	addi r14, r0, 0
	add r4, r14, r0
	beq r1, r14, .LBB12_6
.LBB12_4:
	ldbu r4, r1+6
	andi r5, r4, 16
	addi r4, r0, 0
	bne r5, r4, .LBB12_6
.LBB12_5:
	ldw r4, r3+16
	ldw r5, r4+176
	addi r4, r0, 4
	add r15, r3, r0
	add r3, r1, r0
	jal r31, luaT_gettm
	add r3, r15, r0
	add r4, r1, r0
.LBB12_6:
	bne r4, r14, .LBB12_12
.LBB12_7:
	add r3, r13, r0
	jal r31, luaH_getn
	jal r0, .LBB12_10
.LBB12_8:
	ldw r1, r12+0
	ldw r1, r1+12
	jal r0, .LBB12_10
.LBB12_9:
	ldw r1, r12+0
	ldbu r1, r1+7
.LBB12_10:
	stw r11+0, r1
	addi r1, r0, 3
	stb r11+8, r1
	jal r0, .LBB12_13
.LBB12_11:
	addi r5, r0, 4
	add r13, r3, r0
	add r4, r12, r0
	jal r31, luaT_gettmbyobj
	add r3, r13, r0
	add r4, r1, r0
	ldbu r1, r1+8
	andi r1, r1, 15
	addi r5, r0, 0
	beq r1, r5, .LBB12_14
.LBB12_12:
	add r5, r12, r0
	add r6, r12, r0
	add r7, r11, r0
	jal r31, luaT_callTMres
.LBB12_13:
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
.LBB12_14:
	lui r5, %hi(.L.str.4)
	addi r5, r5, %lo(.L.str.4)
	add r4, r12, r0
	jal r31, luaG_typeerror
.Lfunc_end12:
	.size	luaV_objlen, .Lfunc_end12-luaV_objlen
                                        # -- End function
	.hidden	luaV_idiv                       # -- Begin function luaV_idiv
	.globl	luaV_idiv
	.p2align	2
	.type	luaV_idiv,@function
luaV_idiv:                              # @luaV_idiv
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r1, r5, 1
	addi r6, r0, 1
	bleu r1, r6, .LBB13_4
.LBB13_1:
	div r1, r4, r5
	xor r3, r5, r4
	addi r6, r0, -1
	bgt r3, r6, .LBB13_3
.LBB13_2:
	mul r3, r1, r5
	sub r3, r4, r3
	addi r4, r0, 0
	sne r3, r3, r4
	sub r1, r1, r3
.LBB13_3:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB13_4:
	addi r1, r0, 0
	beq r5, r1, .LBB13_6
.LBB13_5:
	sub r1, r1, r4
	jal r0, .LBB13_3
.LBB13_6:
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	jal r31, luaG_runerror
.Lfunc_end13:
	.size	luaV_idiv, .Lfunc_end13-luaV_idiv
                                        # -- End function
	.hidden	luaV_mod                        # -- Begin function luaV_mod
	.globl	luaV_mod
	.p2align	2
	.type	luaV_mod,@function
luaV_mod:                               # @luaV_mod
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r1, r5, 1
	addi r6, r0, 1
	bleu r1, r6, .LBB14_4
.LBB14_1:
	rem r3, r4, r5
	addi r1, r0, 0
	beq r3, r1, .LBB14_3
.LBB14_2:
	xor r1, r3, r5
	srai r1, r1, 31
	and r1, r1, r5
	add r1, r1, r3
.LBB14_3:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB14_4:
	addi r1, r0, 0
	bne r5, r1, .LBB14_3
.LBB14_5:
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	jal r31, luaG_runerror
.Lfunc_end14:
	.size	luaV_mod, .Lfunc_end14-luaV_mod
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaV_modf
.LCPI15_0:
	.quad	0x0000000000000000              # double 0
	.text
	.hidden	luaV_modf
	.globl	luaV_modf
	.p2align	2
	.type	luaV_modf,@function
luaV_modf:                              # @luaV_modf
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r12
	stw fp+-8, r13
	stw fp+-12, lr
	add r13, r8, r0
	add r12, r7, r0
	add r4, r6, r0
	add r3, r5, r0
	add r5, r7, r0
	add r6, r13, r0
	jal r31, fmod
	add r4, r1, r0
	add r5, r2, r0
	lui r1, %hi(.LCPI15_0)
	addi r1, r1, %lo(.LCPI15_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fle.d r3, r4, r6
	addi r1, r0, 0
	bne r3, r1, .LBB15_2
.LBB15_1:
	flt.d r3, r12, r6
	bne r3, r1, .LBB15_4
	jal r0, .LBB15_5
.LBB15_2:
	fle.d r3, r12, r6
	bne r3, r1, .LBB15_5
.LBB15_3:
	fle.d r3, r6, r4
	bne r3, r1, .LBB15_5
.LBB15_4:
	fadd.d r4, r12, r4
.LBB15_5:
	add r1, r4, r0
	add r2, r5, r0
	ldw lr, fp+-12
	ldw r13, fp+-8
	ldw r12, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end15:
	.size	luaV_modf, .Lfunc_end15-luaV_modf
                                        # -- End function
	.hidden	luaV_shiftl                     # -- Begin function luaV_shiftl
	.globl	luaV_shiftl
	.p2align	2
	.type	luaV_shiftl,@function
luaV_shiftl:                            # @luaV_shiftl
# %bb.0:
	addi r1, r0, -1
	ble r4, r1, .LBB16_2
.LBB16_1:
	addi r1, r0, 31
	sgt r1, r4, r1
	sll r3, r3, r4
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	xor r1, r3, r1
	jal r0, .LBB16_5
.LBB16_2:
	addi r1, r0, -31
	bge r4, r1, .LBB16_4
.LBB16_3:
	addi r1, r0, 0
	jal r0, .LBB16_5
.LBB16_4:
	addi r1, r0, 0
	sub r1, r1, r4
	srl r1, r3, r1
.LBB16_5:
	jalr r0, r31, 0
.Lfunc_end16:
	.size	luaV_shiftl, .Lfunc_end16-luaV_shiftl
                                        # -- End function
	.hidden	luaV_finishOp                   # -- Begin function luaV_finishOp
	.globl	luaV_finishOp
	.p2align	2
	.type	luaV_finishOp,@function
luaV_finishOp:                          # @luaV_finishOp
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r3+20
	ldw r6, r1+16
	ldw r5, r6+-4
	andi r4, r5, 127
	addi r7, r4, -11
	addi r4, r0, 59
	bgtu r7, r4, .LBB17_11
.LBB17_1:
	ldw r4, r1+0
	addi r4, r4, 12
	slli r7, r7, 2
	lui r8, %hi(.LJTI17_0)
	addi r8, r8, %lo(.LJTI17_0)
	add r7, r8, r7
	ldw r7, r7+0
	jalr r0, r7, 0
.LBB17_2:
	srli r1, r5, 7
	jal r0, .LBB17_6
.LBB17_3:
	ldw r4, r3+12
	ldbu r6, r4+-4
	addi r7, r0, 1
	sne r7, r6, r7
	andi r6, r6, 15
	addi r8, r0, 0
	sne r6, r6, r8
	and r6, r7, r6
	addi r4, r4, -12
	stw r3+12, r4
	lui r3, 8
	and r3, r5, r3
	seq r3, r3, r8
	xor r3, r3, r6
	bne r3, r8, .LBB17_11
.LBB17_4:
	ldw r3, r1+16
	addi r3, r3, 4
	jal r0, .LBB17_10
.LBB17_5:
	ldw r1, r6+-8
	srli r1, r1, 7
.LBB17_6:
	andi r1, r1, 255
	addi r5, r0, 12
	mul r1, r1, r5
	add r1, r4, r1
	ldw r4, r3+12
	addi r5, r4, -12
	stw r3+12, r5
	ldw r3, r4+-12
	ldw r5, r4+-8
	stw r1+4, r5
	stw r1+0, r3
	ldbu r3, r4+-4
	stb r1+8, r3
	jal r0, .LBB17_11
.LBB17_7:
	srli r5, r5, 7
	andi r5, r5, 255
	addi r6, r0, 12
	mul r5, r5, r6
	add r4, r4, r5
	ldw r5, r1+28
	mul r5, r5, r6
	add r4, r4, r5
	stw r3+12, r4
	ldw r3, r1+16
	addi r3, r3, -4
	jal r0, .LBB17_10
.LBB17_8:
	ldw r1, r3+12
	srli r5, r5, 7
	andi r5, r5, 255
	addi r6, r1, -24
	addi r7, r0, 12
	mul r5, r5, r7
	add r4, r4, r5
	sub r4, r6, r4
	srai r4, r4, 2
	lui r5, 699051
	addi r5, r5, -1365
	mul r4, r4, r5
	ldw r5, r1+-12
	ldw r7, r1+-8
	stw r1+-32, r7
	stw r1+-36, r5
	ldbu r5, r1+-4
	stb r1+-28, r5
	stw r3+12, r6
	jal r31, luaV_concat
	jal r0, .LBB17_11
.LBB17_9:
	addi r3, r6, -4
.LBB17_10:
	stw r1+16, r3
.LBB17_11:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end17:
	.size	luaV_finishOp, .Lfunc_end17-luaV_finishOp
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI17_0:
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_2
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_5
	.word	.LBB17_5
	.word	.LBB17_5
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_11
	.word	.LBB17_2
	.word	.LBB17_8
	.word	.LBB17_9
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_3
	.word	.LBB17_3
	.word	.LBB17_3
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_3
	.word	.LBB17_3
	.word	.LBB17_3
	.word	.LBB17_3
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_7
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaV_execute
.LCPI18_0:
	.quad	0x0000000000000000              # double 0
.LCPI18_1:
	.quad	0x3ff0000000000000              # double 1
.LCPI18_2:
	.quad	0xc1e0000000000000              # double -2147483648
.LCPI18_3:
	.quad	0x41e0000000000000              # double 2147483648
.LCPI18_4:
	.quad	0x4000000000000000              # double 2
	.text
	.hidden	luaV_execute
	.globl	luaV_execute
	.p2align	2
	.type	luaV_execute,@function
luaV_execute:                           # @luaV_execute
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
	add r13, r4, r0
	add r11, r3, r0
	addi r1, fp, -88
	stw fp+-104, r1
	addi r25, r1, 8
	ldw r1, r3+112
	addi r24, r0, 0
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r28
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r26
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r3
                                        # kill: killed $r3
                                        # implicit-def: $r3
                                        # kill: killed $r3
.LBB18_1:
	ldw r3, r13+0
	ldw r12, r3+0
	ldw r3, r12+12
	ldw r21, r3+48
	ldw r16, r13+16
	add r15, r24, r0
	beq r1, r24, .LBB18_2
	jal r0, .LBB18_626
.LBB18_2:
	ldw r1, r13+0
	addi r14, r1, 12
	addi r1, r12, 16
	stw fp+-92, r1
	jal r0, .LBB18_8
.LBB18_3:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r5, r14, r1
	srli r1, r27, 24
	addi r6, r1, -1
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaT_getvarargs
.LBB18_4:
	ldw r15, r13+20
.LBB18_5:
	addi r17, r0, 0
.LBB18_6:
	add r16, r22, r0
.LBB18_7:
	addi r1, r0, 0
	beq r17, r1, .LBB18_8
	jal r0, .LBB18_622
.LBB18_8:
	addi r1, r0, 0
	beq r15, r1, .LBB18_9
	jal r0, .LBB18_483
.LBB18_9:
	add r15, r1, r0
	addi r22, r16, 4
	ldw r27, r16+0
	andi r1, r27, 127
	addi r3, r0, 81
	bgtu r1, r3, .LBB18_5
.LBB18_10:
	slli r1, r1, 2
	lui r3, %hi(.LJTI18_0)
	addi r3, r3, %lo(.LJTI18_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB18_11:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	srli r4, r27, 16
	andi r4, r4, 255
	mul r3, r4, r3
	add r3, r14, r3
	jal r0, .LBB18_119
.LBB18_12:
	stw fp+-112, r28
	stw fp+-108, r24
	add r1, r26, r0
	add r26, r25, r0
	add r3, r11, r0
	add r11, r21, r0
	add r4, r12, r0
	add r21, r1, r0
	ldw r12, fp+-96
	srli r1, r27, 7
	andi r1, r1, 255
	addi r19, r0, 12
	mul r1, r1, r19
	add r18, r14, r1
	add r28, r4, r0
	ldw r1, r4+12
	ldw r1, r1+56
	srli r5, r27, 13
	lui r4, 128
	addi r4, r4, -4
	and r4, r5, r4
	add r1, r1, r4
	ldw r20, r1+0
	stw r13+16, r22
	ldw r1, r13+4
	stw r3+12, r1
	ldw r16, r20+12
	ldw r24, r20+60
	add r27, r3, r0
	add r4, r16, r0
	jal r31, luaF_newLclosure
	add r17, r1, r0
	stw r1+12, r20
	stw r18+0, r1
	addi r1, r0, 70
	stb r18+8, r1
	addi r20, r0, 1
	blt r16, r20, .LBB18_21
.LBB18_13:
	addi r23, r17, 16
	addi r24, r24, 4
	jal r0, .LBB18_15
.LBB18_14:
	addi r16, r16, -1
	addi r23, r23, 4
	addi r24, r24, 8
	beq r16, r25, .LBB18_21
.LBB18_15:
	ldbu r3, r24+0
	ldbu r1, r24+1
	addi r25, r0, 0
	beq r3, r25, .LBB18_17
.LBB18_16:
	mul r1, r1, r19
	add r4, r14, r1
	add r3, r27, r0
	jal r31, luaF_findupval
	jal r0, .LBB18_18
.LBB18_17:
	slli r1, r1, 2
	ldw r3, fp+-92
	add r1, r3, r1
	ldw r1, r1+0
.LBB18_18:
	stw r23+0, r1
	ldbu r1, r17+5
	andi r1, r1, 32
	beq r1, r25, .LBB18_14
.LBB18_19:
	ldw r5, r23+0
	ldbu r1, r5+5
	andi r1, r1, 24
	beq r1, r25, .LBB18_14
.LBB18_20:
	add r3, r27, r0
	add r4, r17, r0
	jal r31, luaC_barrier_
	jal r0, .LBB18_14
.LBB18_21:
	ldw r1, r27+16
	ldw r1, r1+12
	addi r17, r0, 0
	stw fp+-96, r12
	blt r1, r20, .LBB18_23
.LBB18_22:
	stw r13+16, r22
	addi r1, r18, 12
	stw r27+12, r1
	add r3, r27, r0
	jal r31, luaC_step
	ldw r15, r13+20
.LBB18_23:
	add r16, r22, r0
	add r1, r21, r0
	add r12, r28, r0
	add r21, r11, r0
	add r25, r26, r0
	add r26, r1, r0
	add r11, r27, r0
	ldw r24, fp+-108
	ldw r28, fp+-112
	jal r0, .LBB18_7
.LBB18_24:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r16, r0, 12
	mul r19, r1, r16
	add r17, r14, r19
	srli r1, r27, 16
	andi r1, r1, 255
	addi r18, r0, 0
	bne r1, r18, .LBB18_25
	jal r0, .LBB18_297
.LBB18_25:
	addi r15, r1, -1
	jal r0, .LBB18_298
.LBB18_26:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	stw r13+16, r22
	ldw r3, r13+4
	stw r11+12, r3
	addi r4, r1, 36
	add r3, r11, r0
	jal r31, luaF_newtbcupval
	srli r1, r27, 13
	lui r3, 128
	addi r3, r3, -4
	and r1, r1, r3
	add r1, r22, r1
	addi r22, r1, 4
	ldw r27, r1+0
.LBB18_27:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r15, r14, r1
	addi r16, r15, 48
	addi r5, r0, 36
	add r3, r16, r0
	add r4, r15, r0
	jal r31, memcpy
	addi r1, r15, 84
	stw r11+12, r1
	stw r13+16, r22
	srli r5, r27, 24
	add r3, r11, r0
	add r4, r16, r0
	jal r31, luaD_call
	ldw r15, r13+20
	addi r1, r0, 0
	beq r15, r1, .LBB18_28
	jal r0, .LBB18_502
.LBB18_28:
	addi r1, r22, 4
	ldw r27, r22+0
	add r22, r1, r0
.LBB18_29:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	ldbu r3, r1+56
	andi r4, r3, 15
	addi r17, r0, 0
	beq r4, r17, .LBB18_6
.LBB18_30:
	ldw r4, r1+48
	ldw r5, r1+52
	stw r1+28, r5
	stw r1+24, r4
	stb r1+32, r3
	srli r1, r27, 13
	lui r3, 128
	addi r3, r3, -4
	and r1, r1, r3
	sub r16, r22, r1
	jal r0, .LBB18_7
.LBB18_31:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r16, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r3
	add r18, r14, r1
	ldbu r1, r18+8
	addi r19, r0, 3
	beq r1, r19, .LBB18_32
	jal r0, .LBB18_503
.LBB18_32:
	ldw r3, r18+0
.LBB18_33:
	addi r1, r0, -1
	stw fp+-172, r3
	xor r1, r3, r1
	stw r16+0, r1
	stb r16+8, r19
	jal r0, .LBB18_5
.LBB18_34:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	addi r19, r0, 3
	beq r3, r19, .LBB18_35
	jal r0, .LBB18_510
.LBB18_35:
	ldw r1, r1+0
	stw fp+-132, r1
.LBB18_36:
	srli r1, r27, 24
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	beq r3, r19, .LBB18_37
	jal r0, .LBB18_551
.LBB18_37:
	ldw r26, r1+0
	ldw r3, fp+-132
.LBB18_38:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	addi r16, r16, 8
	xor r3, r26, r3
	jal r0, .LBB18_202
.LBB18_39:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	ldbu r1, r1+8
	addi r3, r0, 1
	sne r3, r1, r3
	andi r1, r1, 15
	addi r17, r0, 0
	sne r1, r1, r17
	and r1, r3, r1
	lui r3, 8
	and r3, r27, r3
	seq r3, r3, r17
	xor r1, r3, r1
	bne r1, r17, .LBB18_91
	jal r0, .LBB18_92
.LBB18_40:
	ldw r3, r11+112
	addi r1, r0, 0
	beq r3, r1, .LBB18_41
	jal r0, .LBB18_515
.LBB18_41:
	ldw r3, r13+8
	stw r11+20, r3
	addi r3, r14, -12
	stw r11+12, r3
	ldh r4, r13+32
	addi r3, r0, 1
	bge r4, r3, .LBB18_42
	jal r0, .LBB18_338
.LBB18_42:
	addi r4, r4, 1
.LBB18_43:
	ldw r5, r11+12
	addi r6, r5, 12
	stw r11+12, r6
	stb r5+8, r1
	addi r4, r4, -1
	bgt r4, r3, .LBB18_43
	jal r0, .LBB18_338
.LBB18_44:
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	srli r1, r27, 16
	andi r1, r1, 255
	addi r4, r0, 12
	mul r1, r1, r4
	add r6, r14, r1
	srli r1, r27, 24
	mul r1, r1, r4
	add r3, r14, r1
	jal r0, .LBB18_102
.LBB18_45:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	srli r4, r27, 16
	andi r4, r4, 255
	mul r3, r4, r3
	add r1, r1, r3
	stw r11+12, r1
	stw r13+16, r22
	add r3, r11, r0
	jal r31, luaV_concat
	ldw r15, r13+20
	ldw r1, r11+16
	ldw r1, r1+12
	addi r17, r0, 0
	addi r3, r0, 1
	blt r1, r3, .LBB18_6
.LBB18_46:
	stw r13+16, r22
	add r3, r11, r0
	jal r31, luaC_step
	ldw r15, r13+20
	jal r0, .LBB18_6
.LBB18_47:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	ldw r1, r16+-4
	srli r5, r27, 16
	andi r5, r5, 255
	addi r5, r5, -127
	srli r8, r27, 24
	srli r6, r27, 15
	andi r6, r6, 1
	srli r1, r1, 7
	andi r1, r1, 255
	mul r1, r1, r3
	add r7, r14, r1
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
	jal r31, luaT_trybiniTM
	jal r0, .LBB18_4
.LBB18_48:
	srli r1, r27, 5
	lui r3, 32768
	addi r3, r3, -4
	and r1, r1, r3
	add r1, r22, r1
	lui r3, 1032192
	addi r3, r3, 4
	jal r0, .LBB18_387
.LBB18_49:
	srli r1, r27, 16
	andi r1, r1, 255
	addi r4, r0, 12
	mul r1, r1, r4
	add r7, r14, r1
	srli r1, r27, 24
	mul r1, r1, r4
	add r3, r14, r1
	jal r0, .LBB18_207
.LBB18_50:
	srli r1, r27, 16
	andi r1, r1, 255
	addi r4, r0, 12
	mul r1, r1, r4
	add r7, r14, r1
	srli r1, r27, 24
	mul r1, r1, r4
	add r3, r14, r1
	jal r0, .LBB18_108
.LBB18_51:
	srli r3, r27, 24
	addi r1, r0, 0
	add r16, r1, r0
	beq r3, r1, .LBB18_53
.LBB18_52:
	ldw r4, r13+24
	add r16, r4, r3
.LBB18_53:
	srli r3, r27, 7
	andi r3, r3, 255
	addi r19, r0, 12
	mul r3, r3, r19
	add r17, r14, r3
	srli r3, r27, 16
	andi r18, r3, 255
	bne r18, r1, .LBB18_54
	jal r0, .LBB18_305
.LBB18_54:
	mul r1, r18, r19
	add r1, r17, r1
	stw r11+12, r1
	jal r0, .LBB18_306
.LBB18_55:
	addi r1, r0, 12
	srli r3, r27, 16
	andi r3, r3, 255
	mul r3, r3, r1
	add r6, r14, r3
	ldbu r4, r6+8
	addi r3, r0, 3
	bne r4, r3, .LBB18_56
	jal r0, .LBB18_341
.LBB18_56:
	addi r5, r0, 19
	bne r4, r5, .LBB18_5
.LBB18_57:
	ldw r5, r6+4
	ldw r4, r6+0
	jal r0, .LBB18_342
.LBB18_58:
	srli r1, r27, 16
	andi r1, r1, 255
	addi r4, r0, 12
	mul r1, r1, r4
	add r7, r14, r1
	srli r1, r27, 24
	mul r1, r1, r4
	add r3, r14, r1
	jal r0, .LBB18_181
.LBB18_59:
	srli r1, r27, 7
	andi r3, r1, 255
	addi r1, r0, 12
	mul r3, r3, r1
	add r16, r14, r3
	srli r3, r27, 16
	andi r3, r3, 255
	mul r3, r3, r1
	add r19, r14, r3
	lui r3, 8
	and r3, r27, r3
	addi r17, r0, 0
	seq r3, r3, r17
	srli r4, r27, 24
	mul r4, r4, r1
	xor r5, r14, r21
	sub r3, r17, r3
	and r3, r5, r3
	xor r3, r21, r3
	add r18, r3, r4
	ldbu r3, r19+8
	addi r4, r0, 3
	beq r3, r4, .LBB18_60
	jal r0, .LBB18_287
.LBB18_60:
	ldbu r3, r16+8
	addi r20, r0, 0
	addi r4, r0, 69
	beq r3, r4, .LBB18_61
	jal r0, .LBB18_406
.LBB18_61:
	ldw r4, r19+0
	addi r5, r4, -1
	ldw r3, r16+0
	ldw r6, r3+8
	bltu r5, r6, .LBB18_62
	jal r0, .LBB18_457
.LBB18_62:
	ldw r3, r3+12
	mul r1, r5, r1
	add r7, r3, r1
	jal r0, .LBB18_458
.LBB18_63:
	srli r1, r27, 16
	andi r1, r1, 255
	addi r4, r0, 12
	mul r1, r1, r4
	add r3, r14, r1
	ldbu r1, r3+8
	addi r5, r0, 1
	seq r5, r1, r5
	andi r6, r1, 15
	addi r17, r0, 0
	seq r6, r6, r17
	or  r5, r5, r6
	lui r6, 8
	and r6, r27, r6
	seq r6, r6, r17
	bne r6, r5, .LBB18_92
.LBB18_64:
	srli r5, r27, 7
	andi r5, r5, 255
	mul r4, r5, r4
	add r4, r14, r4
	ldw r5, r3+0
	ldw r3, r3+4
	stw r4+4, r3
	stw r4+0, r5
	stb r4+8, r1
	jal r0, .LBB18_91
.LBB18_65:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	addi r19, r0, 3
	beq r3, r19, .LBB18_66
	jal r0, .LBB18_516
.LBB18_66:
	ldw r4, r1+0
.LBB18_67:
	srli r1, r27, 24
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	stw fp+-140, r4
	beq r3, r19, .LBB18_68
	jal r0, .LBB18_554
.LBB18_68:
	ldw r3, r1+0
.LBB18_69:
	addi r1, r0, -1
	stw fp+-168, r3
	bgt r3, r1, .LBB18_70
	jal r0, .LBB18_333
.LBB18_70:
	addi r1, r0, 31
	sgt r1, r3, r1
	sll r3, r4, r3
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	xor r1, r3, r1
	jal r0, .LBB18_332
.LBB18_71:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r16, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r3
	add r18, r14, r1
	jal r0, .LBB18_81
.LBB18_72:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	addi r19, r0, 3
	beq r3, r19, .LBB18_73
	jal r0, .LBB18_519
.LBB18_73:
	ldw r4, r1+0
.LBB18_74:
	srli r1, r27, 24
	addi r1, r1, -127
	addi r3, r0, -1
	stw fp+-160, r4
	bgt r4, r3, .LBB18_75
	jal r0, .LBB18_328
.LBB18_75:
	addi r3, r0, 31
	sgt r3, r4, r3
	sll r1, r1, r4
	jal r0, .LBB18_331
.LBB18_76:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	ldw r1, r16+-4
	srli r5, r27, 16
	andi r5, r5, 255
	mul r5, r5, r3
	add r5, r21, r5
	srli r8, r27, 24
	srli r6, r27, 15
	andi r6, r6, 1
	srli r1, r1, 7
	andi r1, r1, 255
	mul r1, r1, r3
	add r7, r14, r1
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
	jal r31, luaT_trybinassocTM
	jal r0, .LBB18_4
.LBB18_77:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r3
	add r5, r14, r1
	ldbu r3, r4+8
	addi r1, r0, 3
	beq r3, r1, .LBB18_78
	jal r0, .LBB18_256
.LBB18_78:
	ldbu r6, r5+8
	beq r6, r1, .LBB18_79
	jal r0, .LBB18_256
.LBB18_79:
	ldw r1, r4+0
	ldw r3, r5+0
	slt r1, r1, r3
	jal r0, .LBB18_385
.LBB18_80:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r16, r14, r1
	srli r1, r27, 14
	andi r1, r1, 1020
	ldw r4, fp+-92
	add r1, r4, r1
	ldw r1, r1+0
	ldw r18, r1+8
.LBB18_81:
	srli r1, r27, 24
	mul r1, r1, r3
	add r19, r21, r1
	ldbu r1, r18+8
	addi r17, r0, 0
	addi r3, r0, 69
	add r7, r17, r0
	bne r1, r3, .LBB18_138
.LBB18_82:
	ldw r4, r19+0
	ldw r3, r18+0
	jal r31, luaH_getshortstr
	jal r0, .LBB18_136
.LBB18_83:
	stw r13+16, r22
	srli r1, r27, 7
	andi r4, r1, 255
	ldw r6, r12+12
	add r3, r11, r0
	add r5, r13, r0
	jal r31, luaT_adjustvarargs
	ldw r15, r13+20
	addi r17, r0, 0
	beq r15, r17, .LBB18_84
	jal r0, .LBB18_522
.LBB18_84:
	ldw r1, r13+0
	addi r14, r1, 12
	jal r0, .LBB18_6
.LBB18_85:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r6, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r3
	add r4, r14, r1
	ldbu r3, r4+8
	addi r1, r0, 19
	bne r3, r1, .LBB18_86
	jal r0, .LBB18_359
.LBB18_86:
	addi r1, r0, 3
	beq r3, r1, .LBB18_87
	jal r0, .LBB18_365
.LBB18_87:
	ldw r3, r4+0
	addi r17, r0, 0
	sub r3, r17, r3
	stw r6+0, r3
	stb r6+8, r1
	jal r0, .LBB18_6
.LBB18_88:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	ldw r4, r16+4
	srli r4, r4, 7
	mul r3, r4, r3
	add r3, r21, r3
	addi r16, r16, 8
	ldw r4, r3+0
	ldw r5, r3+4
	stw r1+4, r5
	stw r1+0, r4
	ldbu r3, r3+8
	jal r0, .LBB18_394
.LBB18_89:
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	srli r1, r27, 16
	andi r1, r1, 255
	addi r4, r0, 12
	mul r1, r1, r4
	add r6, r14, r1
	srli r1, r27, 24
	mul r1, r1, r4
	add r3, r14, r1
	jal r0, .LBB18_213
.LBB18_90:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r3
	add r5, r21, r1
	addi r17, r0, 0
	add r3, r17, r0
	jal r31, luaV_equalobj
	srli r3, r27, 15
	andi r3, r3, 1
	bne r1, r3, .LBB18_92
.LBB18_91:
	ldw r1, r22+0
	srli r1, r1, 5
	lui r3, 32768
	addi r3, r3, -4
	and r1, r1, r3
	add r1, r22, r1
	lui r3, 1032192
	addi r3, r3, 8
	add r16, r1, r3
	ldw r15, r13+20
	jal r0, .LBB18_7
.LBB18_92:
	addi r16, r16, 8
	jal r0, .LBB18_7
.LBB18_93:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	addi r5, r1, -127
	ldbu r1, r4+8
	addi r3, r0, 19
	bne r1, r3, .LBB18_94
	jal r0, .LBB18_363
.LBB18_94:
	addi r3, r0, 3
	beq r1, r3, .LBB18_95
	jal r0, .LBB18_367
.LBB18_95:
	ldw r1, r4+0
	sgt r1, r1, r5
	jal r0, .LBB18_385
.LBB18_96:
	srli r1, r27, 7
	andi r3, r1, 255
	addi r1, r0, 12
	mul r3, r3, r1
	add r16, r14, r3
	srli r3, r27, 16
	andi r3, r3, 255
	mul r3, r3, r1
	add r17, r14, r3
	srli r3, r27, 24
	mul r3, r3, r1
	add r18, r14, r3
	ldbu r3, r18+8
	addi r4, r0, 3
	beq r3, r4, .LBB18_97
	jal r0, .LBB18_292
.LBB18_97:
	ldbu r3, r17+8
	addi r19, r0, 0
	addi r4, r0, 69
	beq r3, r4, .LBB18_98
	jal r0, .LBB18_408
.LBB18_98:
	ldw r4, r18+0
	addi r5, r4, -1
	ldw r3, r17+0
	ldw r6, r3+8
	bltu r5, r6, .LBB18_99
	jal r0, .LBB18_459
.LBB18_99:
	ldw r3, r3+12
	mul r1, r5, r1
	add r7, r3, r1
	jal r0, .LBB18_460
.LBB18_100:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	srli r3, r27, 14
	andi r3, r3, 1020
	ldw r4, fp+-92
	add r3, r4, r3
	ldw r3, r3+0
	ldw r3, r3+8
	jal r0, .LBB18_119
.LBB18_101:
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	srli r1, r27, 16
	andi r1, r1, 255
	addi r4, r0, 12
	mul r1, r1, r4
	add r6, r14, r1
	srli r1, r27, 24
	mul r1, r1, r4
	add r3, r21, r1
.LBB18_102:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r4
	add r18, r14, r1
	ldbu r4, r6+8
	addi r1, r0, 3
	bne r4, r1, .LBB18_103
	jal r0, .LBB18_268
.LBB18_103:
	addi r5, r0, 19
	bne r4, r5, .LBB18_5
.LBB18_104:
	ldw r5, r6+4
	ldw r4, r6+0
	ldbu r6, r3+8
	bne r6, r1, .LBB18_105
	jal r0, .LBB18_346
.LBB18_105:
	addi r1, r0, 19
	bne r6, r1, .LBB18_5
.LBB18_106:
	ldw r23, r3+4
	ldw r22, r3+0
	jal r0, .LBB18_347
.LBB18_107:
	srli r1, r27, 16
	andi r1, r1, 255
	addi r4, r0, 12
	mul r1, r1, r4
	add r7, r14, r1
	srli r1, r27, 24
	mul r1, r1, r4
	add r3, r21, r1
.LBB18_108:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r4
	add r1, r14, r1
	ldbu r4, r7+8
	addi r6, r0, 3
	bne r4, r6, .LBB18_109
	jal r0, .LBB18_273
.LBB18_109:
	addi r5, r0, 19
	bne r4, r5, .LBB18_5
.LBB18_110:
	ldw r5, r7+4
	ldw r4, r7+0
	ldbu r7, r3+8
	bne r7, r6, .LBB18_111
	jal r0, .LBB18_354
.LBB18_111:
	addi r6, r0, 19
	bne r7, r6, .LBB18_5
.LBB18_112:
	ldw r7, r3+4
	ldw r6, r3+0
	jal r0, .LBB18_355
.LBB18_113:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	srli r3, r27, 15
	lui r4, 1048560
	addi r4, r4, 1
	add r3, r3, r4
	fcvt.d.w r4, r3
	stw r1+4, r5
	stw r1+0, r4
	addi r3, r0, 19
	jal r0, .LBB18_143
.LBB18_114:
	srli r1, r27, 5
	andi r1, r1, 1020
	ldw r3, fp+-92
	add r1, r3, r1
	ldw r1, r1+0
	ldw r16, r1+8
	srli r1, r27, 16
	andi r1, r1, 255
	addi r3, r0, 12
	jal r0, .LBB18_158
.LBB18_115:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	addi r5, r1, -127
	ldbu r1, r4+8
	addi r3, r0, 19
	bne r1, r3, .LBB18_116
	jal r0, .LBB18_364
.LBB18_116:
	addi r3, r0, 3
	beq r1, r3, .LBB18_117
	jal r0, .LBB18_379
.LBB18_117:
	ldw r1, r4+0
	sle r1, r1, r5
	jal r0, .LBB18_385
.LBB18_118:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	srli r4, r27, 15
	mul r3, r4, r3
	add r3, r21, r3
.LBB18_119:
	ldw r4, r3+0
	ldw r5, r3+4
	stw r1+4, r5
	stw r1+0, r4
	ldbu r3, r3+8
	jal r0, .LBB18_143
.LBB18_120:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
.LBB18_121:
	addi r3, r0, 17
	jal r0, .LBB18_143
.LBB18_122:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	srli r3, r27, 15
	lui r4, 1048560
	addi r4, r4, 1
	add r3, r3, r4
	stw r1+0, r3
	addi r3, r0, 3
	jal r0, .LBB18_143
.LBB18_123:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	srli r3, r27, 24
	mul r3, r3, r18
	add r3, r21, r3
	ldw r19, r3+0
	ldbu r3, r1+8
	addi r20, r0, 3
	beq r3, r20, .LBB18_124
	jal r0, .LBB18_523
.LBB18_124:
	ldw r3, r1+0
.LBB18_125:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	addi r16, r16, 8
	stw fp+-152, r3
	xor r3, r3, r19
	jal r0, .LBB18_221
.LBB18_126:
	srli r1, r27, 7
	andi r3, r1, 255
	addi r1, r0, 12
	mul r3, r3, r1
	add r4, r14, r3
	srli r3, r27, 16
	andi r16, r3, 255
	lui r3, 8
	and r3, r27, r3
	addi r17, r0, 0
	seq r3, r3, r17
	srli r5, r27, 24
	mul r5, r5, r1
	xor r6, r14, r21
	sub r3, r17, r3
	and r3, r6, r3
	xor r3, r21, r3
	add r6, r3, r5
	ldbu r3, r4+8
	addi r5, r0, 69
	add r7, r17, r0
	beq r3, r5, .LBB18_127
	jal r0, .LBB18_414
.LBB18_127:
	addi r5, r16, -1
	ldw r3, r4+0
	ldw r7, r3+8
	bltu r5, r7, .LBB18_128
	jal r0, .LBB18_410
.LBB18_128:
	ldw r3, r3+12
	mul r1, r5, r1
	add r7, r3, r1
	jal r0, .LBB18_411
.LBB18_129:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	addi r19, r0, 3
	beq r3, r19, .LBB18_130
	jal r0, .LBB18_526
.LBB18_130:
	ldw r1, r1+0
	stw fp+-124, r1
.LBB18_131:
	srli r1, r27, 24
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	beq r3, r19, .LBB18_132
	jal r0, .LBB18_557
.LBB18_132:
	ldw r28, r1+0
	ldw r3, fp+-124
.LBB18_133:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	addi r16, r16, 8
	and r3, r28, r3
	jal r0, .LBB18_202
.LBB18_134:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r16, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r3
	add r18, r14, r1
	lui r1, 8
	and r1, r27, r1
	addi r17, r0, 0
	seq r1, r1, r17
	srli r4, r27, 24
	mul r3, r4, r3
	xor r4, r14, r21
	sub r1, r17, r1
	and r1, r4, r1
	xor r1, r21, r1
	add r19, r1, r3
	ldw r4, r19+0
	ldw r1, r18+0
	ldw r3, r18+4
	stw r16+16, r3
	stw r16+12, r1
	ldbu r1, r18+8
	stb r16+20, r1
	ldbu r1, r18+8
	addi r3, r0, 69
	add r7, r17, r0
	bne r1, r3, .LBB18_138
.LBB18_135:
	ldw r3, r18+0
	jal r31, luaH_getstr
.LBB18_136:
	add r7, r1, r0
	ldbu r1, r1+8
	andi r3, r1, 15
	addi r1, r0, 0
	beq r3, r1, .LBB18_138
.LBB18_137:
	ldw r3, r7+0
	ldw r4, r7+4
	stw r16+4, r4
	stw r16+0, r3
	ldbu r3, r7+8
	stb r16+8, r3
	add r17, r1, r0
	jal r0, .LBB18_6
.LBB18_138:
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
	add r4, r18, r0
	add r5, r19, r0
	add r6, r16, r0
	jal r0, .LBB18_420
.LBB18_139:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	addi r3, r1, -127
	ldbu r6, r4+8
	addi r1, r0, 0
	addi r5, r0, 19
	bne r6, r5, .LBB18_140
	jal r0, .LBB18_368
.LBB18_140:
	addi r7, r0, 3
	add r5, r1, r0
	beq r6, r7, .LBB18_141
	jal r0, .LBB18_369
.LBB18_141:
	ldw r4, r4+0
	seq r5, r4, r3
	jal r0, .LBB18_369
.LBB18_142:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	addi r3, r0, 1
.LBB18_143:
	stb r1+8, r3
	jal r0, .LBB18_5
.LBB18_144:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	addi r5, r1, -127
	ldbu r1, r4+8
	addi r3, r0, 19
	bne r1, r3, .LBB18_145
	jal r0, .LBB18_371
.LBB18_145:
	addi r3, r0, 3
	beq r1, r3, .LBB18_146
	jal r0, .LBB18_380
.LBB18_146:
	ldw r1, r4+0
	sge r1, r1, r5
	jal r0, .LBB18_385
.LBB18_147:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	addi r5, r1, -127
	ldbu r1, r4+8
	addi r3, r0, 19
	bne r1, r3, .LBB18_148
	jal r0, .LBB18_372
.LBB18_148:
	addi r3, r0, 3
	beq r1, r3, .LBB18_149
	jal r0, .LBB18_381
.LBB18_149:
	ldw r1, r4+0
	slt r1, r1, r5
	jal r0, .LBB18_385
.LBB18_150:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	srli r4, r27, 16
	andi r4, r4, 255
	mul r3, r4, r3
	add r5, r14, r3
	srli r3, r27, 24
	addi r3, r3, -127
	ldbu r6, r5+8
	addi r4, r0, 3
	bne r6, r4, .LBB18_151
	jal r0, .LBB18_373
.LBB18_151:
	addi r4, r0, 19
	beq r6, r4, .LBB18_152
	jal r0, .LBB18_5
.LBB18_152:
	ldw r7, r5+4
	ldw r6, r5+0
	fcvt.d.w r8, r3
	addi r16, r16, 8
	fadd.d r6, r6, r8
	stw r1+4, r7
	stw r1+0, r6
	jal r0, .LBB18_374
.LBB18_153:
	srli r1, r27, 7
	andi r3, r1, 255
	addi r1, r0, 12
	mul r3, r3, r1
	add r6, r14, r3
	srli r3, r27, 16
	andi r3, r3, 255
	mul r3, r3, r1
	add r4, r14, r3
	srli r16, r27, 24
	ldbu r3, r4+8
	addi r17, r0, 0
	addi r5, r0, 69
	add r7, r17, r0
	beq r3, r5, .LBB18_154
	jal r0, .LBB18_419
.LBB18_154:
	addi r5, r16, -1
	ldw r3, r4+0
	ldw r7, r3+8
	bltu r5, r7, .LBB18_155
	jal r0, .LBB18_416
.LBB18_155:
	ldw r3, r3+12
	mul r1, r5, r1
	add r7, r3, r1
	jal r0, .LBB18_417
.LBB18_156:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	addi r3, r0, 1
	stb r1+8, r3
	jal r0, .LBB18_370
.LBB18_157:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r16, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
.LBB18_158:
	mul r1, r1, r3
	add r19, r21, r1
	lui r1, 8
	and r1, r27, r1
	addi r17, r0, 0
	seq r1, r1, r17
	srli r4, r27, 24
	mul r3, r4, r3
	xor r4, r14, r21
	sub r1, r17, r1
	and r1, r4, r1
	xor r1, r21, r1
	add r18, r1, r3
	ldbu r1, r16+8
	addi r3, r0, 69
	add r7, r17, r0
	beq r1, r3, .LBB18_159
	jal r0, .LBB18_407
.LBB18_159:
	ldw r4, r19+0
	ldw r3, r16+0
	jal r31, luaH_getshortstr
	add r7, r1, r0
	ldbu r1, r1+8
	andi r3, r1, 15
	addi r1, r0, 0
	bne r3, r1, .LBB18_160
	jal r0, .LBB18_407
.LBB18_160:
	ldw r3, r18+0
	ldw r4, r18+4
	stw r7+4, r4
	stw r7+0, r3
	ldbu r3, r18+8
	stb r7+8, r3
	ldbu r3, r18+8
	andi r3, r3, 64
	bne r3, r1, .LBB18_290
	jal r0, .LBB18_413
.LBB18_161:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	srli r3, r27, 14
	andi r3, r3, 1020
	ldw r4, fp+-92
	add r3, r4, r3
	ldw r4, r3+0
	ldw r3, r4+8
	ldw r5, r1+0
	ldw r6, r1+4
	stw r3+4, r6
	stw r3+0, r5
	ldbu r5, r1+8
	stb r3+8, r5
	ldbu r3, r1+8
	andi r3, r3, 64
	addi r17, r0, 0
	bne r3, r17, .LBB18_162
	jal r0, .LBB18_6
.LBB18_162:
	ldbu r3, r4+5
	andi r3, r3, 32
	addi r17, r0, 0
	bne r3, r17, .LBB18_163
	jal r0, .LBB18_6
.LBB18_163:
	ldw r5, r1+0
	ldbu r1, r5+5
	andi r1, r1, 24
	addi r17, r0, 0
	bne r1, r17, .LBB18_164
	jal r0, .LBB18_6
.LBB18_164:
	add r3, r11, r0
	jal r31, luaC_barrier_
	jal r0, .LBB18_6
.LBB18_165:
	srli r1, r27, 7
	andi r3, r1, 255
	srli r1, r27, 16
	andi r1, r1, 255
	addi r1, r1, 1
	addi r4, r0, 12
	mul r3, r3, r4
	add r3, r14, r3
	addi r3, r3, 8
.LBB18_166:
	addi r17, r0, 0
	stb r3+0, r17
	addi r1, r1, -1
	addi r3, r3, 12
	bne r1, r17, .LBB18_166
	jal r0, .LBB18_6
.LBB18_167:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	srli r3, r27, 24
	mul r3, r3, r18
	add r3, r21, r3
	ldw r19, r3+0
	ldbu r3, r1+8
	addi r20, r0, 3
	beq r3, r20, .LBB18_168
	jal r0, .LBB18_531
.LBB18_168:
	ldw r3, r1+0
.LBB18_169:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	addi r16, r16, 8
	stw fp+-148, r3
	or  r3, r3, r19
	jal r0, .LBB18_221
.LBB18_170:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	addi r19, r0, 3
	beq r3, r19, .LBB18_171
	jal r0, .LBB18_534
.LBB18_171:
	ldw r4, r1+0
.LBB18_172:
	srli r1, r27, 24
	addi r3, r0, -1
	stw fp+-156, r4
	ble r27, r3, .LBB18_330
.LBB18_173:
	addi r3, r0, 127
	sub r1, r3, r1
	lui r3, 393216
	sltu r3, r27, r3
	sll r1, r4, r1
	jal r0, .LBB18_331
.LBB18_174:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	addi r19, r0, 3
	beq r3, r19, .LBB18_175
	jal r0, .LBB18_537
.LBB18_175:
	ldw r1, r1+0
	stw fp+-136, r1
.LBB18_176:
	srli r1, r27, 24
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	beq r3, r19, .LBB18_177
	jal r0, .LBB18_561
.LBB18_177:
	ldw r4, r1+0
.LBB18_178:
	addi r17, r0, 0
	sub r1, r17, r4
	addi r3, r0, -1
	stw fp+-164, r4
	ble r1, r3, .LBB18_335
.LBB18_179:
	addi r3, r0, 31
	sgt r3, r1, r3
	ldw r4, fp+-136
	sll r1, r4, r1
	sub r3, r17, r3
	and r3, r1, r3
	xor r1, r1, r3
	jal r0, .LBB18_336
.LBB18_180:
	srli r1, r27, 16
	andi r1, r1, 255
	addi r4, r0, 12
	mul r1, r1, r4
	add r7, r14, r1
	srli r1, r27, 24
	mul r1, r1, r4
	add r3, r21, r1
.LBB18_181:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r4
	add r1, r14, r1
	ldbu r4, r7+8
	addi r6, r0, 3
	beq r4, r6, .LBB18_277
.LBB18_182:
	addi r5, r0, 19
	beq r4, r5, .LBB18_183
	jal r0, .LBB18_5
.LBB18_183:
	ldw r5, r7+4
	ldw r4, r7+0
	ldbu r7, r3+8
	beq r7, r6, .LBB18_361
.LBB18_184:
	addi r6, r0, 19
	beq r7, r6, .LBB18_185
	jal r0, .LBB18_5
.LBB18_185:
	ldw r7, r3+4
	ldw r6, r3+0
	jal r0, .LBB18_362
.LBB18_186:
	addi r1, r0, 12
	srli r3, r27, 16
	andi r3, r3, 255
	mul r3, r3, r1
	add r6, r14, r3
	ldbu r4, r6+8
	addi r3, r0, 3
	bne r4, r3, .LBB18_187
	jal r0, .LBB18_389
.LBB18_187:
	addi r5, r0, 19
	beq r4, r5, .LBB18_188
	jal r0, .LBB18_5
.LBB18_188:
	ldw r5, r6+4
	ldw r4, r6+0
	jal r0, .LBB18_390
.LBB18_189:
	add r4, r11, r0
	add r11, r21, r0
	add r21, r12, r0
	add r12, r28, r0
	add r28, r26, r0
	add r26, r25, r0
	srli r18, r27, 24
	addi r17, r0, 0
	lui r1, 8
	add r25, r24, r0
	and r1, r27, r1
	beq r1, r17, .LBB18_191
.LBB18_190:
	ldw r1, r22+0
	slli r1, r1, 1
	lui r3, 524288
	addi r3, r3, -256
	and r1, r1, r3
	or  r18, r1, r18
.LBB18_191:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r23, r14, r1
	srli r1, r27, 16
	andi r22, r1, 255
	addi r20, r0, 1
	or  r24, r22, r18
	addi r19, r23, 12
	add r27, r4, r0
	stw r4+12, r19
	add r3, r4, r0
	jal r31, luaH_new
	stw r23+0, r1
	addi r3, r0, 69
	stb r23+8, r3
	beq r24, r17, .LBB18_193
.LBB18_192:
	addi r3, r22, -1
	sltu r4, r3, r22
	sll r3, r20, r3
	sub r4, r17, r4
	and r6, r3, r4
	add r3, r27, r0
	add r4, r1, r0
	add r5, r18, r0
	jal r31, luaH_resize
.LBB18_193:
	addi r16, r16, 8
	ldw r1, r27+16
	ldw r1, r1+12
	blt r1, r20, .LBB18_195
.LBB18_194:
	stw r13+16, r16
	stw r27+12, r19
	add r3, r27, r0
	jal r31, luaC_step
	ldw r15, r13+20
.LBB18_195:
	add r24, r25, r0
	add r25, r26, r0
	add r26, r28, r0
	add r28, r12, r0
	add r12, r21, r0
	add r21, r11, r0
	add r11, r27, r0
	jal r0, .LBB18_7
.LBB18_196:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r3
	add r5, r14, r1
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
	jal r31, luaV_equalobj
	jal r0, .LBB18_384
.LBB18_197:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	addi r19, r0, 3
	beq r3, r19, .LBB18_198
	jal r0, .LBB18_542
.LBB18_198:
	ldw r1, r1+0
	stw fp+-128, r1
.LBB18_199:
	srli r1, r27, 24
	mul r1, r1, r18
	add r1, r14, r1
	ldbu r3, r1+8
	beq r3, r19, .LBB18_200
	jal r0, .LBB18_564
.LBB18_200:
	ldw r4, r1+0
	ldw r3, fp+-128
.LBB18_201:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	addi r16, r16, 8
	stw fp+-96, r4
	or  r3, r4, r3
.LBB18_202:
	stw r1+0, r3
	stb r1+8, r19
	addi r17, r0, 0
	jal r0, .LBB18_7
.LBB18_203:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r3
	add r5, r14, r1
	ldbu r3, r4+8
	addi r1, r0, 3
	bne r3, r1, .LBB18_261
.LBB18_204:
	ldbu r6, r5+8
	bne r6, r1, .LBB18_261
.LBB18_205:
	ldw r1, r4+0
	ldw r3, r5+0
	sle r1, r1, r3
	jal r0, .LBB18_385
.LBB18_206:
	srli r1, r27, 16
	andi r1, r1, 255
	addi r4, r0, 12
	mul r1, r1, r4
	add r7, r14, r1
	srli r1, r27, 24
	mul r1, r1, r4
	add r3, r21, r1
.LBB18_207:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r4
	add r1, r14, r1
	ldbu r4, r7+8
	addi r6, r0, 3
	beq r4, r6, .LBB18_275
.LBB18_208:
	addi r5, r0, 19
	beq r4, r5, .LBB18_209
	jal r0, .LBB18_5
.LBB18_209:
	ldw r5, r7+4
	ldw r4, r7+0
	ldbu r7, r3+8
	beq r7, r6, .LBB18_357
.LBB18_210:
	addi r6, r0, 19
	beq r7, r6, .LBB18_211
	jal r0, .LBB18_5
.LBB18_211:
	ldw r7, r3+4
	ldw r6, r3+0
	jal r0, .LBB18_358
.LBB18_212:
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	srli r1, r27, 16
	andi r1, r1, 255
	addi r4, r0, 12
	mul r1, r1, r4
	add r6, r14, r1
	srli r1, r27, 24
	mul r1, r1, r4
	add r3, r21, r1
.LBB18_213:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r4
	add r17, r14, r1
	ldbu r4, r6+8
	addi r1, r0, 3
	beq r4, r1, .LBB18_280
.LBB18_214:
	addi r5, r0, 19
	beq r4, r5, .LBB18_215
	jal r0, .LBB18_5
.LBB18_215:
	ldw r5, r6+4
	ldw r4, r6+0
	ldbu r6, r3+8
	beq r6, r1, .LBB18_376
.LBB18_216:
	addi r1, r0, 19
	beq r6, r1, .LBB18_217
	jal r0, .LBB18_5
.LBB18_217:
	ldw r7, r3+4
	ldw r6, r3+0
	jal r0, .LBB18_377
.LBB18_218:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	srli r3, r27, 24
	mul r3, r3, r18
	add r3, r21, r3
	ldw r19, r3+0
	ldbu r3, r1+8
	addi r20, r0, 3
	beq r3, r20, .LBB18_219
	jal r0, .LBB18_547
.LBB18_219:
	ldw r3, r1+0
.LBB18_220:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	addi r16, r16, 8
	stw fp+-144, r3
	and r3, r3, r19
.LBB18_221:
	stw r1+0, r3
	stb r1+8, r20
	addi r17, r0, 0
	jal r0, .LBB18_7
.LBB18_222:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r3, r14, r1
	ldbu r4, r3+8
	addi r1, r0, 3
	beq r4, r1, .LBB18_395
.LBB18_223:
	addi r5, r0, 19
	beq r4, r5, .LBB18_224
	jal r0, .LBB18_5
.LBB18_224:
	ldw r5, r3+4
	ldw r4, r3+0
	jal r0, .LBB18_396
.LBB18_225:
	addi r18, r0, 12
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r18
	add r3, r14, r1
	ldbu r4, r3+8
	addi r1, r0, 3
	beq r4, r1, .LBB18_399
.LBB18_226:
	addi r5, r0, 19
	beq r4, r5, .LBB18_227
	jal r0, .LBB18_5
.LBB18_227:
	ldw r5, r3+4
	ldw r4, r3+0
	jal r0, .LBB18_400
.LBB18_228:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	addi r17, r0, 0
	addi r6, r0, 1
	add r3, r11, r0
	add r5, r17, r0
	jal r31, luaF_close
	ldw r15, r13+20
	jal r0, .LBB18_6
.LBB18_229:
	ldw r3, r11+112
	addi r1, r0, 0
	beq r3, r1, .LBB18_230
	jal r0, .LBB18_550
.LBB18_230:
	ldh r3, r13+32
	ldw r4, r13+8
	stw r11+20, r4
	beq r3, r1, .LBB18_337
.LBB18_231:
	srli r4, r27, 7
	andi r4, r4, 255
	addi r5, r0, 12
	mul r4, r4, r5
	add r4, r14, r4
	ldw r5, r4+0
	ldw r6, r4+4
	stw r14+-8, r6
	stw r14+-12, r5
	ldbu r4, r4+8
	stb r14+-4, r4
	stw r11+12, r14
	addi r4, r0, 2
	blt r3, r4, .LBB18_338
.LBB18_232:
	addi r3, r3, 1
.LBB18_233:
	ldw r5, r11+12
	addi r6, r5, 12
	stw r11+12, r6
	stb r5+8, r1
	addi r3, r3, -1
	ble r3, r4, .LBB18_338
	jal r0, .LBB18_233
.LBB18_234:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
	jal r31, luaF_newtbcupval
	jal r0, .LBB18_5
.LBB18_235:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	srli r1, r27, 16
	andi r1, r1, 255
	mul r1, r1, r3
	add r5, r14, r1
	add r3, r11, r0
	jal r31, luaV_objlen
	jal r0, .LBB18_4
.LBB18_236:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r4, r14, r1
	ldw r1, r16+-4
	srli r5, r27, 16
	andi r5, r5, 255
	mul r5, r5, r3
	add r5, r14, r5
	srli r7, r27, 24
	srli r1, r1, 7
	andi r1, r1, 255
	mul r1, r1, r3
	add r6, r14, r1
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
	jal r0, .LBB18_366
.LBB18_237:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	ldbu r4, r1+32
	addi r3, r0, 3
	bne r4, r3, .LBB18_294
.LBB18_238:
	ldw r4, r1+12
	addi r5, r0, 0
	beq r4, r5, .LBB18_296
.LBB18_239:
	ldw r5, r1+24
	ldw r6, r1+0
	addi r4, r4, -1
	stw r1+12, r4
	add r4, r6, r5
	stw r1+0, r4
	stw r1+36, r4
	jal r0, .LBB18_423
.LBB18_240:
	stw fp+-120, r12
	stw fp+-112, r28
	stw fp+-108, r24
	stw fp+-100, r25
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r16, r14, r1
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	addi r17, r16, 12
	ldbu r1, r16+8
	addi r28, r0, 3
	stw fp+-116, r26
	bne r1, r28, .LBB18_266
.LBB18_241:
	ldbu r1, r16+32
	bne r1, r28, .LBB18_266
.LBB18_242:
	ldw r12, fp+-96
	ldw r18, r16+24
	addi r26, r0, 0
	bne r18, r26, .LBB18_243
	jal r0, .LBB18_629
.LBB18_243:
	ldw r25, r16+0
	stw r16+36, r25
	stb r16+44, r28
	ldbu r1, r16+20
	andi r1, r1, 15
	addi r19, r0, 4
	bne r1, r19, .LBB18_463
.LBB18_244:
	ldw r23, r17+0
	addi r3, r23, 16
	addi r4, fp, -88
	jal r31, luaO_str2num
	ldbu r3, r23+7
	addi r4, r0, 255
	bne r3, r4, .LBB18_246
.LBB18_245:
	ldw r3, r23+12
.LBB18_246:
	addi r3, r3, 1
	sne r1, r1, r3
	jal r0, .LBB18_464
.LBB18_247:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	srli r4, r27, 16
	andi r4, r4, 255
	mul r3, r4, r3
	add r3, r14, r3
	ldbu r4, r3+8
	addi r3, r0, 1
	beq r4, r3, .LBB18_121
.LBB18_248:
	andi r4, r4, 15
	addi r17, r0, 0
	beq r4, r17, .LBB18_121
.LBB18_249:
	stb r1+8, r3
	jal r0, .LBB18_6
.LBB18_250:
	add r4, r26, r0
	add r26, r12, r0
	add r12, r28, r0
	add r28, r24, r0
	srli r1, r27, 7
	andi r1, r1, 255
	addi r20, r0, 12
	mul r23, r1, r20
	add r1, r14, r23
	srli r3, r27, 16
	andi r24, r3, 255
	ldw r18, r1+0
	addi r17, r0, 0
	beq r24, r17, .LBB18_314
.LBB18_251:
	ldw r1, r13+4
	stw r11+12, r1
	jal r0, .LBB18_315
.LBB18_252:
	srli r1, r27, 7
	andi r3, r1, 255
	addi r1, r0, 12
	mul r3, r3, r1
	add r4, r14, r3
	srli r3, r27, 16
	andi r3, r3, 255
	addi r17, r0, 0
	beq r3, r17, .LBB18_254
.LBB18_253:
	mul r1, r3, r1
	add r1, r4, r1
	stw r11+12, r1
.LBB18_254:
	srli r1, r27, 24
	addi r5, r1, -1
	stw r13+16, r22
	add r3, r11, r0
	jal r31, luaD_precall
	bne r1, r17, .LBB18_255
	jal r0, .LBB18_509
.LBB18_255:
	addi r17, r0, 2
	add r16, r22, r0
	add r13, r1, r0
	jal r0, .LBB18_7
.LBB18_256:
	andi r6, r3, 15
	bne r6, r1, .LBB18_285
.LBB18_257:
	ldbu r6, r5+8
	andi r7, r6, 15
	bne r7, r1, .LBB18_285
.LBB18_258:
	bne r3, r1, .LBB18_471
.LBB18_259:
	ldw r3, r4+0
	beq r6, r1, .LBB18_260
	jal r0, .LBB18_479
.LBB18_260:
	ldw r1, r5+0
	slt r1, r3, r1
	jal r0, .LBB18_385
.LBB18_261:
	andi r6, r3, 15
	bne r6, r1, .LBB18_286
.LBB18_262:
	ldbu r6, r5+8
	andi r7, r6, 15
	bne r7, r1, .LBB18_286
.LBB18_263:
	bne r3, r1, .LBB18_473
.LBB18_264:
	ldw r3, r4+0
	beq r6, r1, .LBB18_265
	jal r0, .LBB18_481
.LBB18_265:
	ldw r1, r5+0
	sle r1, r3, r1
	jal r0, .LBB18_385
.LBB18_266:
	ldbu r1, r16+20
	addi r20, r0, 19
	ldw r12, fp+-96
	bne r1, r20, .LBB18_312
.LBB18_267:
	ldw r25, r17+4
	ldw r24, r17+0
	jal r0, .LBB18_429
.LBB18_268:
	ldbu r4, r3+8
	bne r4, r1, .LBB18_345
.LBB18_269:
	ldw r3, r3+0
	addi r4, r3, 1
	addi r5, r0, 1
	bgtu r4, r5, .LBB18_270
	jal r0, .LBB18_581
.LBB18_270:
	ldw r4, r6+0
	rem r5, r4, r3
	addi r4, r0, 0
	beq r5, r4, .LBB18_272
.LBB18_271:
	xor r4, r5, r3
	srai r4, r4, 31
	and r3, r4, r3
	add r4, r3, r5
.LBB18_272:
	addi r16, r16, 8
	stw r18+0, r4
	stb r18+8, r1
	addi r17, r0, 0
	jal r0, .LBB18_7
.LBB18_273:
	ldbu r5, r3+8
	ldw r4, r7+0
	bne r5, r6, .LBB18_353
.LBB18_274:
	ldw r3, r3+0
	addi r16, r16, 8
	mul r3, r3, r4
	jal r0, .LBB18_279
.LBB18_275:
	ldbu r5, r3+8
	ldw r4, r7+0
	bne r5, r6, .LBB18_356
.LBB18_276:
	ldw r3, r3+0
	addi r16, r16, 8
	sub r3, r4, r3
	jal r0, .LBB18_279
.LBB18_277:
	ldbu r5, r3+8
	ldw r4, r7+0
	bne r5, r6, .LBB18_360
.LBB18_278:
	ldw r3, r3+0
	addi r16, r16, 8
	add r3, r3, r4
.LBB18_279:
	stw r1+0, r3
	stb r1+8, r6
	addi r17, r0, 0
	jal r0, .LBB18_7
.LBB18_280:
	ldbu r5, r3+8
	ldw r4, r6+0
	bne r5, r1, .LBB18_375
.LBB18_281:
	ldw r5, r3+0
	addi r3, r5, 1
	addi r6, r0, 1
	bgtu r3, r6, .LBB18_282
	jal r0, .LBB18_582
.LBB18_282:
	div r3, r4, r5
	xor r6, r5, r4
	addi r7, r0, -1
	bgt r6, r7, .LBB18_284
.LBB18_283:
	mul r5, r3, r5
	sub r4, r4, r5
	addi r5, r0, 0
	sne r4, r4, r5
	sub r3, r3, r4
.LBB18_284:
	addi r16, r16, 8
	stw r17+0, r3
	jal r0, .LBB18_378
.LBB18_285:
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
	jal r31, lessthanothers
	jal r0, .LBB18_384
.LBB18_286:
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
	jal r31, lessequalothers
	jal r0, .LBB18_384
.LBB18_287:
	ldbu r1, r16+8
	addi r3, r0, 69
	add r7, r17, r0
	bne r1, r3, .LBB18_407
.LBB18_288:
	ldw r3, r16+0
	add r4, r19, r0
	jal r31, luaH_get
	add r7, r1, r0
	ldbu r1, r1+8
	andi r1, r1, 15
	addi r3, r0, 0
	beq r1, r3, .LBB18_407
.LBB18_289:
	ldw r1, r18+0
	ldw r3, r18+4
	stw r7+4, r3
	stw r7+0, r1
	ldbu r1, r18+8
	stb r7+8, r1
	ldbu r1, r18+8
	andi r1, r1, 64
	addi r17, r0, 0
	bne r1, r17, .LBB18_290
	jal r0, .LBB18_6
.LBB18_290:
	ldw r4, r16+0
	ldbu r1, r4+5
	andi r1, r1, 32
	addi r17, r0, 0
	bne r1, r17, .LBB18_291
	jal r0, .LBB18_6
.LBB18_291:
	ldw r1, r18+0
	jal r0, .LBB18_455
.LBB18_292:
	ldbu r1, r17+8
	addi r3, r0, 69
	bne r1, r3, .LBB18_409
.LBB18_293:
	ldw r3, r17+0
	add r4, r18, r0
	jal r31, luaH_get
	add r7, r1, r0
	ldbu r1, r1+8
	andi r1, r1, 15
	addi r3, r0, 0
	bne r1, r3, .LBB18_462
	jal r0, .LBB18_461
.LBB18_294:
	ldw r9, r1+28
	ldw r8, r1+24
	ldw r7, r1+16
	ldw r6, r1+12
	ldw r5, r1+4
	ldw r4, r1+0
	fadd.d r4, r8, r4
	lui r3, %hi(.LCPI18_0)
	addi r3, r3, %lo(.LCPI18_0)
	ldw r17, r3+4
	ldw r16, r3+0
	fle.d r8, r8, r16
	addi r3, r0, 0
	bne r8, r3, .LBB18_421
.LBB18_295:
	flt.d r6, r6, r4
	beq r6, r3, .LBB18_422
.LBB18_296:
	add r16, r22, r0
	jal r0, .LBB18_388
.LBB18_297:
	ldw r1, r11+12
	sub r1, r1, r17
	srai r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r15, r1, r3
.LBB18_298:
	stw r13+16, r22
	lui r1, 8
	and r1, r27, r1
	beq r1, r18, .LBB18_302
.LBB18_299:
	stw r13+28, r15
	ldw r3, r11+12
	ldw r1, r13+4
	bgeu r3, r1, .LBB18_301
.LBB18_300:
	stw r11+12, r1
.LBB18_301:
	addi r5, r0, -1
	addi r6, r0, 1
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaF_close
	ldw r1, r13+20
	beq r1, r18, .LBB18_302
	jal r0, .LBB18_567
.LBB18_302:
	srli r1, r27, 24
	beq r1, r18, .LBB18_304
.LBB18_303:
	ldw r3, r13+24
	add r1, r1, r3
	ldw r3, r13+0
	sub r1, r18, r1
	mul r1, r1, r16
	add r1, r3, r1
	stw r13+0, r1
.LBB18_304:
	mul r1, r15, r16
	add r1, r17, r1
	stw r11+12, r1
	add r3, r11, r0
	add r4, r13, r0
	add r5, r15, r0
	jal r31, luaD_poscall
	ldw r15, r13+20
	jal r0, .LBB18_338
.LBB18_305:
	ldw r1, r11+12
	sub r1, r1, r17
	srai r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r18, r1, r3
.LBB18_306:
	stw r13+16, r22
	lui r1, 8
	and r1, r27, r1
	addi r20, r0, 0
	beq r1, r20, .LBB18_308
.LBB18_307:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaF_closeupval
.LBB18_308:
	add r3, r11, r0
	add r4, r13, r0
	add r5, r17, r0
	add r6, r18, r0
	add r7, r16, r0
	jal r31, luaD_pretailcall
	add r18, r1, r0
	blt r1, r20, .LBB18_310
.LBB18_309:
	ldw r1, r13+0
	sub r3, r20, r16
	mul r3, r3, r19
	add r1, r1, r3
	stw r13+0, r1
	add r3, r11, r0
	add r4, r13, r0
	add r5, r18, r0
	jal r31, luaD_poscall
	ldw r15, r13+20
	addi r17, r0, 9
	jal r0, .LBB18_311
.LBB18_310:
	addi r17, r0, 2
.LBB18_311:
	addi r1, r0, -1
	bgt r18, r1, .LBB18_338
	jal r0, .LBB18_6
.LBB18_312:
	ldbu r1, r16+20
	bne r1, r28, .LBB18_424
.LBB18_313:
	ldw r1, r17+0
	fcvt.d.w r24, r1
	jal r0, .LBB18_429
.LBB18_314:
	ldw r3, r11+12
	sub r1, r3, r1
	srai r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r1, r1, r3
	addi r24, r1, -1
.LBB18_315:
	stw fp+-100, r25
	srli r1, r27, 24
	add r19, r24, r1
	lui r1, 8
	and r1, r27, r1
	stw fp+-116, r4
	beq r1, r17, .LBB18_317
.LBB18_316:
	ldw r1, r16+4
	slli r1, r1, 1
	lui r3, 524288
	addi r3, r3, -256
	and r1, r1, r3
	add r19, r1, r19
	addi r22, r16, 8
.LBB18_317:
	ldw r27, fp+-96
	add r3, r18, r0
	jal r31, luaH_realasize
	bleu r19, r1, .LBB18_319
.LBB18_318:
	add r3, r11, r0
	add r4, r18, r0
	add r5, r19, r0
	jal r31, luaH_resizearray
.LBB18_319:
	addi r16, r0, 1
	stw fp+-96, r27
	blt r24, r16, .LBB18_327
.LBB18_320:
	mul r1, r19, r20
	addi r19, r1, -4
	addi r25, r24, 1
	mul r1, r24, r20
	add r1, r1, r23
	add r1, r14, r1
	addi r20, r1, 8
	add r24, r28, r0
	add r28, r12, r0
	add r12, r26, r0
	jal r0, .LBB18_322
.LBB18_321:
	addi r19, r19, -12
	addi r25, r25, -1
	addi r20, r20, -12
	ble r25, r16, .LBB18_326
.LBB18_322:
	ldw r1, r18+12
	add r1, r1, r19
	ldw r3, r20+-8
	ldw r4, r20+-4
	stw r1+-4, r4
	stw r1+-8, r3
	ldbu r3, r20+0
	stb r1+0, r3
	ldbu r1, r20+0
	andi r1, r1, 64
	addi r17, r0, 0
	beq r1, r17, .LBB18_321
.LBB18_323:
	ldbu r1, r18+5
	andi r1, r1, 32
	beq r1, r17, .LBB18_321
.LBB18_324:
	ldw r1, r20+-8
	ldbu r1, r1+5
	andi r1, r1, 24
	beq r1, r17, .LBB18_321
.LBB18_325:
	add r3, r11, r0
	add r4, r18, r0
	jal r31, luaC_barrierback_
	jal r0, .LBB18_321
.LBB18_326:
	add r16, r22, r0
	ldw r26, fp+-116
	ldw r25, fp+-100
	jal r0, .LBB18_7
.LBB18_327:
	add r16, r22, r0
	ldw r1, fp+-116
	add r24, r28, r0
	add r28, r12, r0
	ldw r25, fp+-100
	add r12, r26, r0
	add r26, r1, r0
	jal r0, .LBB18_7
.LBB18_328:
	addi r3, r0, -31
	blt r4, r3, .LBB18_334
.LBB18_329:
	addi r3, r0, 0
	sub r3, r3, r4
	srl r1, r1, r3
	jal r0, .LBB18_332
.LBB18_330:
	addi r1, r1, -127
	lui r3, 651264
	addi r3, r3, -1
	sgtu r3, r27, r3
	srl r1, r4, r1
.LBB18_331:
	addi r4, r0, 0
	sub r3, r4, r3
	and r3, r1, r3
	xor r1, r1, r3
.LBB18_332:
	srli r3, r27, 7
	andi r3, r3, 255
	mul r3, r3, r18
	add r3, r14, r3
	addi r16, r16, 8
	stw r3+0, r1
	stb r3+8, r19
	addi r17, r0, 0
	jal r0, .LBB18_7
.LBB18_333:
	addi r1, r0, -31
	bge r3, r1, .LBB18_452
.LBB18_334:
	addi r1, r0, 0
	jal r0, .LBB18_332
.LBB18_335:
	addi r3, r0, -31
	slt r1, r1, r3
	ldw r3, fp+-136
	srl r3, r3, r4
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	xor r1, r3, r1
.LBB18_336:
	srli r3, r27, 7
	andi r3, r3, 255
	mul r3, r3, r18
	add r3, r14, r3
	addi r16, r16, 8
	stw r3+0, r1
	stb r3+8, r19
	jal r0, .LBB18_7
.LBB18_337:
	addi r1, r14, -12
	stw r11+12, r1
.LBB18_338:
	ldbu r1, r13+34
	andi r1, r1, 4
	addi r3, r0, 0
	bne r1, r3, .LBB18_340
.LBB18_339:
	ldw r13, r13+8
	addi r17, r0, 3
	jal r0, .LBB18_6
.LBB18_340:
	addi r17, r0, 1
	jal r0, .LBB18_6
.LBB18_341:
	ldw r4, r6+0
	fcvt.d.w r4, r4
.LBB18_342:
	srli r6, r27, 24
	mul r6, r6, r1
	add r8, r21, r6
	ldbu r6, r8+8
	beq r6, r3, .LBB18_391
.LBB18_343:
	addi r3, r0, 19
	beq r6, r3, .LBB18_344
	jal r0, .LBB18_5
.LBB18_344:
	ldw r7, r8+4
	ldw r6, r8+0
	jal r0, .LBB18_392
.LBB18_345:
	ldw r4, r6+0
	fcvt.d.w r4, r4
	ldbu r6, r3+8
	beq r6, r1, .LBB18_346
	jal r0, .LBB18_105
.LBB18_346:
	ldw r1, r3+0
	fcvt.d.w r22, r1
.LBB18_347:
	add r3, r4, r0
	add r4, r5, r0
	add r5, r22, r0
	add r6, r23, r0
	jal r31, fmod
	add r4, r1, r0
	add r5, r2, r0
	lui r1, %hi(.LCPI18_0)
	addi r1, r1, %lo(.LCPI18_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fle.d r1, r4, r6
	addi r17, r0, 0
	bne r1, r17, .LBB18_349
.LBB18_348:
	flt.d r1, r22, r6
	addi r3, r0, 0
	bne r1, r3, .LBB18_351
	jal r0, .LBB18_352
.LBB18_349:
	fle.d r1, r22, r6
	bne r1, r17, .LBB18_352
.LBB18_350:
	fle.d r1, r6, r4
	bne r1, r17, .LBB18_352
.LBB18_351:
	fadd.d r4, r22, r4
.LBB18_352:
	addi r16, r16, 8
	stw r18+4, r5
	stw r18+0, r4
	addi r1, r0, 19
	stb r18+8, r1
	jal r0, .LBB18_7
.LBB18_353:
	fcvt.d.w r4, r4
	ldbu r7, r3+8
	beq r7, r6, .LBB18_354
	jal r0, .LBB18_111
.LBB18_354:
	ldw r3, r3+0
	fcvt.d.w r6, r3
.LBB18_355:
	addi r16, r16, 8
	fmul.d r4, r4, r6
	jal r0, .LBB18_393
.LBB18_356:
	fcvt.d.w r4, r4
	ldbu r7, r3+8
	bne r7, r6, .LBB18_210
.LBB18_357:
	ldw r3, r3+0
	fcvt.d.w r6, r3
.LBB18_358:
	addi r16, r16, 8
	fsub.d r4, r4, r6
	jal r0, .LBB18_393
.LBB18_359:
	ldw r9, r4+4
	ldw r8, r4+0
	fneg.d r4, r8
	stw r6+4, r5
	stw r6+0, r4
	stb r6+8, r1
	jal r0, .LBB18_5
.LBB18_360:
	fcvt.d.w r4, r4
	ldbu r7, r3+8
	bne r7, r6, .LBB18_184
.LBB18_361:
	ldw r3, r3+0
	fcvt.d.w r6, r3
.LBB18_362:
	addi r16, r16, 8
	fadd.d r4, r4, r6
	jal r0, .LBB18_393
.LBB18_363:
	ldw r7, r4+4
	ldw r6, r4+0
	fcvt.d.w r4, r5
	flt.d r1, r4, r6
	jal r0, .LBB18_385
.LBB18_364:
	ldw r7, r4+4
	ldw r6, r4+0
	fcvt.d.w r4, r5
	fle.d r1, r6, r4
	jal r0, .LBB18_385
.LBB18_365:
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	addi r7, r0, 18
	add r3, r11, r0
	add r5, r4, r0
.LBB18_366:
	jal r31, luaT_trybinTM
	jal r0, .LBB18_4
.LBB18_367:
	srli r7, r27, 24
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	addi r6, r0, 1
	jal r0, .LBB18_382
.LBB18_368:
	ldw r7, r4+4
	ldw r6, r4+0
	fcvt.d.w r4, r3
	feq.d r5, r6, r4
.LBB18_369:
	lui r3, 8
	and r3, r27, r3
	seq r3, r3, r1
	xor r3, r3, r5
	bne r3, r1, .LBB18_386
.LBB18_370:
	addi r16, r16, 8
	addi r17, r0, 0
	jal r0, .LBB18_7
.LBB18_371:
	ldw r7, r4+4
	ldw r6, r4+0
	fcvt.d.w r4, r5
	fle.d r1, r4, r6
	jal r0, .LBB18_385
.LBB18_372:
	ldw r7, r4+4
	ldw r6, r4+0
	fcvt.d.w r4, r5
	flt.d r1, r6, r4
	jal r0, .LBB18_385
.LBB18_373:
	ldw r5, r5+0
	addi r16, r16, 8
	add r3, r5, r3
	stw r1+0, r3
.LBB18_374:
	stb r1+8, r4
	addi r17, r0, 0
	jal r0, .LBB18_7
.LBB18_375:
	fcvt.d.w r4, r4
	ldbu r6, r3+8
	bne r6, r1, .LBB18_216
.LBB18_376:
	ldw r1, r3+0
	fcvt.d.w r6, r1
.LBB18_377:
	addi r16, r16, 8
	fdiv.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, floor
	stw r17+4, r2
	stw r17+0, r1
	addi r1, r0, 19
.LBB18_378:
	stb r17+8, r1
	addi r17, r0, 0
	jal r0, .LBB18_7
.LBB18_379:
	srli r7, r27, 24
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	addi r6, r0, 0
	addi r8, r0, 21
	jal r0, .LBB18_383
.LBB18_380:
	srli r7, r27, 24
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	addi r6, r0, 1
	addi r8, r0, 21
	jal r0, .LBB18_383
.LBB18_381:
	srli r7, r27, 24
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	addi r6, r0, 0
.LBB18_382:
	addi r8, r0, 20
.LBB18_383:
	add r3, r11, r0
	jal r31, luaT_callorderiTM
.LBB18_384:
	ldw r15, r13+20
.LBB18_385:
	srli r3, r27, 15
	andi r3, r3, 1
	bne r1, r3, .LBB18_370
.LBB18_386:
	ldw r1, r22+0
	srli r1, r1, 5
	lui r3, 32768
	addi r3, r3, -4
	and r1, r1, r3
	add r1, r22, r1
	lui r3, 1032192
	addi r3, r3, 8
.LBB18_387:
	add r16, r1, r3
.LBB18_388:
	ldw r15, r13+20
	addi r17, r0, 0
	jal r0, .LBB18_7
.LBB18_389:
	ldw r4, r6+0
	fcvt.d.w r4, r4
.LBB18_390:
	srli r6, r27, 24
	mul r6, r6, r1
	add r8, r14, r6
	ldbu r6, r8+8
	bne r6, r3, .LBB18_343
.LBB18_391:
	ldw r3, r8+0
	fcvt.d.w r6, r3
.LBB18_392:
	srli r3, r27, 7
	andi r3, r3, 255
	mul r1, r3, r1
	add r1, r14, r1
	addi r16, r16, 8
	fdiv.d r4, r4, r6
.LBB18_393:
	stw r1+4, r5
	stw r1+0, r4
	addi r3, r0, 19
.LBB18_394:
	stb r1+8, r3
	addi r17, r0, 0
	jal r0, .LBB18_7
.LBB18_395:
	ldw r3, r3+0
	fcvt.d.w r4, r3
.LBB18_396:
	srli r3, r27, 24
	mul r3, r3, r18
	add r3, r14, r3
	ldbu r6, r3+8
	beq r6, r1, .LBB18_401
.LBB18_397:
	addi r1, r0, 19
	beq r6, r1, .LBB18_398
	jal r0, .LBB18_5
.LBB18_398:
	ldw r7, r3+4
	ldw r6, r3+0
	jal r0, .LBB18_402
.LBB18_399:
	ldw r3, r3+0
	fcvt.d.w r4, r3
.LBB18_400:
	srli r3, r27, 24
	mul r3, r3, r18
	add r3, r21, r3
	ldbu r6, r3+8
	bne r6, r1, .LBB18_397
.LBB18_401:
	ldw r1, r3+0
	fcvt.d.w r6, r1
.LBB18_402:
	lui r1, %hi(.LCPI18_4)
	addi r1, r1, %lo(.LCPI18_4)
	ldw r9, r1+4
	ldw r8, r1+0
	feq.d r1, r6, r8
	xori r1, r1, 1
	addi r17, r0, 0
	bne r1, r17, .LBB18_404
.LBB18_403:
	fmul.d r4, r4, r4
	jal r0, .LBB18_405
.LBB18_404:
	add r3, r4, r0
	add r4, r5, r0
	add r5, r6, r0
	add r6, r7, r0
	jal r31, pow
	add r4, r1, r0
	add r5, r2, r0
.LBB18_405:
	srli r1, r27, 7
	andi r1, r1, 255
	mul r1, r1, r18
	add r1, r14, r1
	addi r16, r16, 8
	stw r1+4, r5
	stw r1+0, r4
	addi r3, r0, 19
	stb r1+8, r3
	jal r0, .LBB18_7
.LBB18_406:
	addi r1, r0, 1
	add r7, r20, r0
	beq r1, r20, .LBB18_289
.LBB18_407:
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
	add r4, r16, r0
	add r5, r19, r0
	add r6, r18, r0
	jal r0, .LBB18_415
.LBB18_408:
	addi r1, r0, 1
	add r7, r19, r0
	bne r1, r19, .LBB18_461
	jal r0, .LBB18_462
.LBB18_409:
	addi r7, r0, 0
	jal r0, .LBB18_461
.LBB18_410:
	add r18, r4, r0
	add r4, r16, r0
	add r19, r6, r0
	jal r31, luaH_getint
	add r6, r19, r0
	add r4, r18, r0
	add r7, r1, r0
.LBB18_411:
	ldbu r1, r7+8
	andi r3, r1, 15
	addi r1, r0, 0
	beq r3, r1, .LBB18_414
.LBB18_412:
	ldw r3, r6+0
	ldw r5, r6+4
	stw r7+4, r5
	stw r7+0, r3
	ldbu r3, r6+8
	stb r7+8, r3
	ldbu r3, r6+8
	andi r3, r3, 64
	bne r3, r1, .LBB18_453
.LBB18_413:
	add r17, r1, r0
	jal r0, .LBB18_6
.LBB18_414:
	ldw r5, fp+-104
	stw r5+0, r16
	addi r1, r0, 3
	stb r5+8, r1
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
.LBB18_415:
	jal r31, luaV_finishset
	ldw r15, r13+20
	jal r0, .LBB18_6
.LBB18_416:
	add r18, r4, r0
	add r4, r16, r0
	add r19, r6, r0
	jal r31, luaH_getint
	add r4, r18, r0
	add r6, r19, r0
	add r7, r1, r0
.LBB18_417:
	ldbu r1, r7+8
	andi r3, r1, 15
	addi r1, r0, 0
	beq r3, r1, .LBB18_419
.LBB18_418:
	ldw r3, r7+0
	ldw r4, r7+4
	stw r6+4, r4
	stw r6+0, r3
	ldbu r3, r7+8
	stb r6+8, r3
	add r17, r1, r0
	jal r0, .LBB18_6
.LBB18_419:
	ldw r5, fp+-104
	stw r5+0, r16
	addi r1, r0, 3
	stb r5+8, r1
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
.LBB18_420:
	jal r31, luaV_finishget
	ldw r15, r13+20
	jal r0, .LBB18_6
.LBB18_421:
	flt.d r6, r4, r6
	bne r6, r3, .LBB18_296
.LBB18_422:
	stw r1+4, r5
	stw r1+0, r4
	stw r1+40, r5
	stw r1+36, r4
	addi r3, r0, 19
.LBB18_423:
	stb r1+44, r3
	srli r1, r27, 13
	lui r3, 128
	addi r3, r3, -4
	and r1, r1, r3
	sub r16, r22, r1
	jal r0, .LBB18_388
.LBB18_424:
	andi r1, r1, 15
	addi r3, r0, 4
	beq r1, r3, .LBB18_425
	jal r0, .LBB18_630
.LBB18_425:
	ldw r19, r17+0
	addi r3, r19, 16
	addi r18, fp, -88
	add r4, r18, r0
	jal r31, luaO_str2num
	ldbu r3, r19+7
	addi r4, r0, 255
	bne r3, r4, .LBB18_427
.LBB18_426:
	ldw r3, r19+12
.LBB18_427:
	addi r3, r3, 1
	beq r1, r3, .LBB18_428
	jal r0, .LBB18_630
.LBB18_428:
	ldbu r1, r18+8
	seq r1, r1, r28
	ldw r3, r18+0
	fcvt.d.w r4, r3
	ldw r6, r18+4
	addi r7, r0, 0
	sub r1, r7, r1
	xor r7, r5, r6
	and r7, r7, r1
	xor r25, r6, r7
	xor r4, r4, r3
	and r1, r4, r1
	xor r24, r3, r1
.LBB18_429:
	addi r17, r16, 24
	ldbu r1, r16+32
	bne r1, r20, .LBB18_432
.LBB18_430:
	ldw r19, r17+4
	ldw r18, r17+0
	ldbu r1, r16+8
	bne r1, r20, .LBB18_434
.LBB18_431:
	ldw r5, r16+4
	ldw r4, r16+0
	jal r0, .LBB18_447
.LBB18_432:
	ldbu r1, r16+32
	bne r1, r28, .LBB18_436
.LBB18_433:
	ldw r1, r17+0
	fcvt.d.w r18, r1
	ldbu r1, r16+8
	beq r1, r20, .LBB18_431
.LBB18_434:
	bne r1, r28, .LBB18_441
.LBB18_435:
	ldw r1, r16+0
	fcvt.d.w r4, r1
	addi r23, r0, 0
	addi r1, r0, 0
	beq r23, r1, .LBB18_447
	jal r0, .LBB18_628
.LBB18_436:
	andi r1, r1, 15
	addi r3, r0, 4
	beq r1, r3, .LBB18_437
	jal r0, .LBB18_631
.LBB18_437:
	ldw r19, r17+0
	addi r3, r19, 16
	addi r18, fp, -88
	add r4, r18, r0
	jal r31, luaO_str2num
	ldbu r3, r19+7
	addi r4, r0, 255
	bne r3, r4, .LBB18_439
.LBB18_438:
	ldw r3, r19+12
.LBB18_439:
	addi r3, r3, 1
	beq r1, r3, .LBB18_440
	jal r0, .LBB18_631
.LBB18_440:
	ldbu r1, r18+8
	seq r1, r1, r28
	ldw r3, r18+0
	fcvt.d.w r4, r3
	ldw r6, r18+4
	addi r7, r0, 0
	sub r1, r7, r1
	xor r7, r5, r6
	and r7, r7, r1
	xor r19, r6, r7
	xor r4, r4, r3
	and r1, r4, r1
	xor r18, r3, r1
	ldbu r1, r16+8
	beq r1, r20, .LBB18_431
	jal r0, .LBB18_434
.LBB18_441:
	andi r1, r1, 15
	addi r23, r0, 1
	addi r3, r0, 4
	bne r1, r3, .LBB18_475
.LBB18_442:
	ldw r26, r16+0
	addi r3, r26, 16
	addi r17, fp, -88
	add r4, r17, r0
	jal r31, luaO_str2num
	ldbu r3, r26+7
	addi r4, r0, 255
	bne r3, r4, .LBB18_444
.LBB18_443:
	ldw r3, r26+12
.LBB18_444:
	addi r3, r3, 1
                                        # implicit-def: $r4_r5
	bne r1, r3, .LBB18_446
.LBB18_445:
	ldbu r1, r17+8
	seq r1, r1, r28
	ldw r3, r17+0
	fcvt.d.w r6, r3
	ldw r4, r17+4
	addi r23, r0, 0
	sub r1, r23, r1
	xor r5, r7, r4
	and r5, r5, r1
	xor r5, r4, r5
	xor r6, r6, r3
	and r1, r6, r1
	xor r4, r3, r1
	addi r1, r0, 0
	beq r23, r1, .LBB18_447
	jal r0, .LBB18_628
.LBB18_446:
	addi r1, r0, 0
	beq r23, r1, .LBB18_447
	jal r0, .LBB18_628
.LBB18_447:
	lui r1, %hi(.LCPI18_0)
	addi r1, r1, %lo(.LCPI18_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r18, r6
	xori r1, r1, 1
	addi r26, r0, 0
	bne r1, r26, .LBB18_448
	jal r0, .LBB18_629
.LBB18_448:
	fle.d r1, r18, r6
	bne r1, r26, .LBB18_450
.LBB18_449:
	flt.d r1, r24, r4
	addi r26, r0, 0
	beq r1, r26, .LBB18_451
	jal r0, .LBB18_501
.LBB18_450:
	flt.d r1, r4, r24
	bne r1, r26, .LBB18_501
.LBB18_451:
	stw r16+16, r25
	stw r16+12, r24
	stb r16+20, r20
	stw r16+28, r19
	stw r16+24, r18
	stb r16+32, r20
	stw r16+4, r5
	stw r16+0, r4
	stb r16+8, r20
	stw r16+40, r5
	stw r16+36, r4
	stb r16+44, r20
	addi r26, r0, 1
	jal r0, .LBB18_501
.LBB18_452:
	addi r1, r0, 0
	sub r1, r1, r3
	srl r1, r4, r1
	jal r0, .LBB18_332
.LBB18_453:
	ldw r4, r4+0
	ldbu r1, r4+5
	andi r1, r1, 32
	addi r17, r0, 0
	bne r1, r17, .LBB18_454
	jal r0, .LBB18_6
.LBB18_454:
	ldw r1, r6+0
.LBB18_455:
	ldbu r1, r1+5
	andi r1, r1, 24
	addi r17, r0, 0
	bne r1, r17, .LBB18_456
	jal r0, .LBB18_6
.LBB18_456:
	add r3, r11, r0
	jal r31, luaC_barrierback_
	jal r0, .LBB18_6
.LBB18_457:
	jal r31, luaH_getint
	add r7, r1, r0
.LBB18_458:
	ldbu r1, r7+8
	andi r1, r1, 15
	addi r3, r0, 0
	seq r1, r1, r3
	beq r1, r20, .LBB18_289
	jal r0, .LBB18_407
.LBB18_459:
	jal r31, luaH_getint
	add r7, r1, r0
.LBB18_460:
	ldbu r1, r7+8
	andi r1, r1, 15
	addi r3, r0, 0
	seq r1, r1, r3
	beq r1, r19, .LBB18_462
.LBB18_461:
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	add r3, r11, r0
	add r4, r17, r0
	add r5, r18, r0
	add r6, r16, r0
	jal r31, luaV_finishget
	jal r0, .LBB18_4
.LBB18_462:
	ldw r1, r7+0
	ldw r3, r7+4
	stw r16+4, r3
	stw r16+0, r1
	ldbu r1, r7+8
	stb r16+8, r1
	jal r0, .LBB18_5
.LBB18_463:
	addi r1, r0, 1
.LBB18_464:
	addi r20, r16, 20
	sub r3, r26, r1
	ldw r4, fp+-104
	xor r1, r17, r4
	and r1, r1, r3
	xor r1, r4, r1
	ldw r5, fp+-100
	xor r4, r20, r5
	and r3, r4, r3
	xor r3, r5, r3
	ldbu r3, r3+0
	beq r3, r28, .LBB18_476
.LBB18_465:
	addi r16, r0, 19
	bne r3, r16, .LBB18_469
.LBB18_466:
	addi r24, r0, 0
	slt r3, r18, r24
	stw fp+-180, r3
	add r23, r21, r0
	stw fp+-176, r19
	add r19, r25, r0
	add r25, r20, r0
	ldw r21, r1+4
	ldw r20, r1+0
	add r3, r20, r0
	add r4, r21, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r20, r4
	add r20, r25, r0
	add r25, r19, r0
	ldw r19, fp+-176
	add r21, r23, r0
	xori r1, r1, 1
	lui r3, %hi(.LCPI18_1)
	addi r3, r3, %lo(.LCPI18_1)
	ldw r7, r3+4
	ldw r6, r3+0
	fadd.d r6, r4, r6
	sub r1, r24, r1
	xor r3, r7, r2
	and r3, r3, r1
	xor r3, r2, r3
	xor r6, r6, r4
	and r1, r6, r1
	xor r1, r4, r1
	xor r3, r3, r2
	ldw r6, fp+-180
	sub r8, r24, r6
	and r3, r3, r8
	xor r7, r2, r3
	xor r1, r1, r4
	and r1, r1, r8
	xor r6, r4, r1
	lui r1, %hi(.LCPI18_2)
	addi r1, r1, %lo(.LCPI18_2)
	ldw r5, r1+4
	ldw r4, r1+0
	flt.d r1, r6, r4
	bne r1, r24, .LBB18_469
.LBB18_467:
	lui r1, %hi(.LCPI18_3)
	addi r1, r1, %lo(.LCPI18_3)
	ldw r5, r1+4
	ldw r4, r1+0
	fle.d r1, r4, r6
	bne r1, r24, .LBB18_469
.LBB18_468:
	fcvt.w.d r1, r6
	jal r0, .LBB18_494
.LBB18_469:
	ldbu r1, r20+0
	bne r1, r16, .LBB18_477
.LBB18_470:
	ldw r5, r17+4
	ldw r4, r17+0
	jal r0, .LBB18_489
.LBB18_471:
	ldw r9, r4+4
	ldw r8, r4+0
	addi r1, r0, 19
	bne r6, r1, .LBB18_480
.LBB18_472:
	ldw r7, r5+4
	ldw r6, r5+0
	flt.d r1, r8, r6
	jal r0, .LBB18_385
.LBB18_473:
	ldw r9, r4+4
	ldw r8, r4+0
	addi r1, r0, 19
	bne r6, r1, .LBB18_482
.LBB18_474:
	ldw r7, r5+4
	ldw r6, r5+0
	fle.d r1, r8, r6
	jal r0, .LBB18_385
.LBB18_475:
                                        # implicit-def: $r4_r5
	addi r1, r0, 0
	beq r23, r1, .LBB18_447
	jal r0, .LBB18_628
.LBB18_476:
	ldw r1, r1+0
	jal r0, .LBB18_494
.LBB18_477:
	bne r1, r28, .LBB18_484
.LBB18_478:
	ldw r1, r17+0
	fcvt.d.w r4, r1
	jal r0, .LBB18_489
.LBB18_479:
	ldw r7, r5+4
	ldw r6, r5+0
	fcvt.d.w r4, r3
	flt.d r1, r4, r6
	jal r0, .LBB18_385
.LBB18_480:
	ldw r1, r5+0
	fcvt.d.w r4, r1
	flt.d r1, r8, r4
	jal r0, .LBB18_385
.LBB18_481:
	ldw r7, r5+4
	ldw r6, r5+0
	fcvt.d.w r4, r3
	fle.d r1, r4, r6
	jal r0, .LBB18_385
.LBB18_482:
	ldw r1, r5+0
	fcvt.d.w r4, r1
	fle.d r1, r8, r4
	jal r0, .LBB18_385
.LBB18_483:
	add r3, r11, r0
	add r4, r16, r0
	jal r31, luaG_traceexec
	ldw r3, r13+0
	addi r14, r3, 12
	jal r0, .LBB18_9
.LBB18_484:
	andi r1, r1, 15
	beq r1, r19, .LBB18_485
	jal r0, .LBB18_630
.LBB18_485:
	ldw r19, r17+0
	addi r3, r19, 16
	addi r16, fp, -88
	add r4, r16, r0
	jal r31, luaO_str2num
	ldbu r3, r19+7
	addi r4, r0, 255
	bne r3, r4, .LBB18_487
.LBB18_486:
	ldw r3, r19+12
.LBB18_487:
	addi r3, r3, 1
	bne r1, r3, .LBB18_630
.LBB18_488:
	ldbu r1, r16+8
	seq r1, r1, r28
	ldw r3, r16+0
	fcvt.d.w r6, r3
	ldw r4, r16+4
	sub r1, r26, r1
	xor r5, r7, r4
	and r5, r5, r1
	xor r5, r4, r5
	xor r6, r6, r3
	and r1, r6, r1
	xor r4, r3, r1
.LBB18_489:
	lui r1, %hi(.LCPI18_0)
	addi r1, r1, %lo(.LCPI18_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fle.d r1, r4, r6
	bne r1, r26, .LBB18_492
.LBB18_490:
	addi r26, r0, 0
	blt r18, r26, .LBB18_501
.LBB18_491:
	lui r1, 524288
	addi r1, r1, -1
	jal r0, .LBB18_494
.LBB18_492:
	bgt r18, r26, .LBB18_501
.LBB18_493:
	lui r1, 524288
.LBB18_494:
	addi r26, r0, 0
	sgt r3, r18, r26
	sle r4, r25, r1
	sge r5, r25, r1
	xor r4, r4, r5
	sub r3, r26, r3
	and r3, r4, r3
	xor r3, r5, r3
	addi r16, r0, 1
	bne r3, r16, .LBB18_501
.LBB18_495:
	blt r18, r16, .LBB18_498
.LBB18_496:
	sub r1, r1, r25
	addi r3, r0, 1
	beq r18, r3, .LBB18_500
.LBB18_497:
	add r3, r1, r0
	add r4, r18, r0
	jal r0, .LBB18_499
.LBB18_498:
	sub r3, r25, r1
	addi r1, r0, 0
	sub r4, r1, r18
.LBB18_499:
	jal r31, __udivsi3
.LBB18_500:
	stw r17+0, r1
	stb r20+0, r28
	add r26, r16, r0
.LBB18_501:
	stw fp+-96, r12
	srli r1, r27, 13
	lui r3, 128
	addi r3, r3, -4
	and r1, r1, r3
	add r1, r22, r1
	addi r1, r1, 4
	addi r17, r0, 0
	sub r3, r17, r26
	xor r4, r22, r1
	and r3, r4, r3
	xor r16, r1, r3
	ldw r25, fp+-100
	ldw r24, fp+-108
	ldw r28, fp+-112
	ldw r26, fp+-116
	ldw r12, fp+-120
	jal r0, .LBB18_7
.LBB18_502:
	ldw r1, r13+0
	addi r14, r1, 12
	jal r0, .LBB18_28
.LBB18_503:
	add r4, r26, r0
	addi r20, r0, 1
	addi r3, r0, 19
	bne r1, r3, .LBB18_507
.LBB18_504:
	add r27, r25, r0
	add r26, r12, r0
	add r23, r24, r0
	add r17, r4, r0
	ldw r12, fp+-96
	ldw r25, r18+4
	ldw r24, r18+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	bne r3, r1, .LBB18_506
.LBB18_505:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	ldw r4, fp+-172
	xor r4, r4, r3
	and r1, r4, r1
	xor r1, r3, r1
	stw fp+-172, r1
.LBB18_506:
	add r4, r17, r0
	add r24, r23, r0
	add r12, r26, r0
	add r25, r27, r0
.LBB18_507:
	addi r17, r0, 0
	add r26, r4, r0
	beq r20, r17, .LBB18_568
.LBB18_508:
	stw r13+16, r22
	ldw r1, r13+4
	stw r11+12, r1
	addi r7, r0, 19
	add r3, r11, r0
	add r4, r18, r0
	add r5, r18, r0
	add r6, r16, r0
	jal r31, luaT_trybinTM
.LBB18_509:
	ldw r15, r13+20
	jal r0, .LBB18_6
.LBB18_510:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_514
.LBB18_511:
	add r26, r25, r0
	stw fp+-120, r12
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	bne r3, r1, .LBB18_513
.LBB18_512:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	ldw r4, fp+-132
	xor r4, r4, r3
	and r1, r4, r1
	xor r1, r3, r1
	stw fp+-132, r1
.LBB18_513:
	add r5, r17, r0
	add r24, r23, r0
	ldw r12, fp+-120
	add r25, r26, r0
.LBB18_514:
	addi r17, r0, 0
	add r26, r5, r0
	beq r20, r17, .LBB18_635
	jal r0, .LBB18_6
.LBB18_635:
	jal r0, .LBB18_36
.LBB18_515:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	stw r11+12, r1
	stw r13+16, r22
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaD_poscall
	addi r15, r0, 1
	jal r0, .LBB18_338
.LBB18_516:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_569
.LBB18_517:
	stw fp+-100, r25
	add r26, r12, r0
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	beq r3, r1, .LBB18_584
.LBB18_518:
	add r5, r17, r0
	ldw r4, fp+-140
	jal r0, .LBB18_585
.LBB18_519:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_570
.LBB18_520:
	stw fp+-100, r25
	add r26, r12, r0
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	beq r3, r1, .LBB18_588
.LBB18_521:
	ldw r4, fp+-160
	jal r0, .LBB18_589
.LBB18_522:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaD_hookcall
	addi r1, r0, 1
	stw r11+100, r1
	jal r0, .LBB18_84
.LBB18_523:
	add r5, r26, r0
	addi r23, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_571
.LBB18_524:
	stw fp+-120, r12
	stw fp+-100, r25
	add r26, r13, r0
	add r13, r11, r0
	add r11, r21, r0
	add r21, r28, r0
	add r28, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	beq r3, r1, .LBB18_592
.LBB18_525:
	ldw r3, fp+-152
	jal r0, .LBB18_593
.LBB18_526:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_530
.LBB18_527:
	add r26, r25, r0
	stw fp+-120, r12
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	bne r3, r1, .LBB18_529
.LBB18_528:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	ldw r4, fp+-124
	xor r4, r4, r3
	and r1, r4, r1
	xor r1, r3, r1
	stw fp+-124, r1
.LBB18_529:
	add r5, r17, r0
	add r24, r23, r0
	ldw r12, fp+-120
	add r25, r26, r0
.LBB18_530:
	addi r17, r0, 0
	add r26, r5, r0
	beq r20, r17, .LBB18_636
	jal r0, .LBB18_6
.LBB18_636:
	jal r0, .LBB18_131
.LBB18_531:
	add r5, r26, r0
	addi r23, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_572
.LBB18_532:
	stw fp+-120, r12
	stw fp+-100, r25
	add r26, r13, r0
	add r13, r11, r0
	add r11, r21, r0
	add r21, r28, r0
	add r28, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	beq r3, r1, .LBB18_596
.LBB18_533:
	ldw r3, fp+-148
	jal r0, .LBB18_597
.LBB18_534:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_573
.LBB18_535:
	stw fp+-100, r25
	add r26, r12, r0
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	beq r3, r1, .LBB18_600
.LBB18_536:
	ldw r4, fp+-156
	jal r0, .LBB18_601
.LBB18_537:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_541
.LBB18_538:
	add r26, r25, r0
	stw fp+-120, r12
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	bne r3, r1, .LBB18_540
.LBB18_539:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	ldw r4, fp+-136
	xor r4, r4, r3
	and r1, r4, r1
	xor r1, r3, r1
	stw fp+-136, r1
.LBB18_540:
	add r5, r17, r0
	add r24, r23, r0
	ldw r12, fp+-120
	add r25, r26, r0
.LBB18_541:
	addi r17, r0, 0
	add r26, r5, r0
	beq r20, r17, .LBB18_637
	jal r0, .LBB18_6
.LBB18_637:
	jal r0, .LBB18_176
.LBB18_542:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_546
.LBB18_543:
	add r26, r25, r0
	stw fp+-120, r12
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	bne r3, r1, .LBB18_545
.LBB18_544:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	ldw r4, fp+-128
	xor r4, r4, r3
	and r1, r4, r1
	xor r1, r3, r1
	stw fp+-128, r1
.LBB18_545:
	add r5, r17, r0
	add r24, r23, r0
	ldw r12, fp+-120
	add r25, r26, r0
.LBB18_546:
	addi r17, r0, 0
	add r26, r5, r0
	beq r20, r17, .LBB18_638
	jal r0, .LBB18_6
.LBB18_638:
	jal r0, .LBB18_199
.LBB18_547:
	add r5, r26, r0
	addi r23, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_574
.LBB18_548:
	stw fp+-120, r12
	stw fp+-100, r25
	add r26, r13, r0
	add r13, r11, r0
	add r11, r21, r0
	add r21, r28, r0
	add r28, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	beq r3, r1, .LBB18_604
.LBB18_549:
	ldw r3, fp+-144
	jal r0, .LBB18_605
.LBB18_550:
	srli r1, r27, 7
	andi r1, r1, 255
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r14, r1
	addi r1, r1, 12
	stw r11+12, r1
	stw r13+16, r22
	addi r15, r0, 1
	add r3, r11, r0
	add r4, r13, r0
	add r5, r15, r0
	jal r31, luaD_poscall
	jal r0, .LBB18_338
.LBB18_551:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_575
.LBB18_552:
	stw fp+-100, r25
	add r26, r12, r0
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	beq r3, r1, .LBB18_608
.LBB18_553:
	ldw r3, fp+-132
	add r5, r17, r0
	jal r0, .LBB18_609
.LBB18_554:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_576
.LBB18_555:
	stw fp+-100, r25
	add r26, r12, r0
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	beq r3, r1, .LBB18_611
.LBB18_556:
	add r5, r17, r0
	ldw r3, fp+-168
	jal r0, .LBB18_612
.LBB18_557:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_577
.LBB18_558:
	stw fp+-100, r25
	add r26, r12, r0
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	bne r3, r1, .LBB18_560
.LBB18_559:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	xor r4, r28, r3
	and r1, r4, r1
	xor r28, r3, r1
.LBB18_560:
	ldw r3, fp+-124
	add r5, r17, r0
	add r24, r23, r0
	add r12, r26, r0
	ldw r25, fp+-100
	jal r0, .LBB18_578
.LBB18_561:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_579
.LBB18_562:
	stw fp+-100, r25
	add r26, r12, r0
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	stw fp+-96, r12
	beq r3, r1, .LBB18_615
.LBB18_563:
	add r5, r17, r0
	ldw r4, fp+-164
	jal r0, .LBB18_616
.LBB18_564:
	add r5, r26, r0
	addi r20, r0, 1
	addi r4, r0, 19
	bne r3, r4, .LBB18_580
.LBB18_565:
	stw fp+-100, r25
	add r26, r12, r0
	add r23, r24, r0
	add r17, r5, r0
	ldw r12, fp+-96
	ldw r25, r1+4
	ldw r24, r1+0
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	feq.d r1, r24, r4
	xori r3, r1, 1
	addi r1, r0, 0
	beq r3, r1, .LBB18_619
.LBB18_566:
	ldw r3, fp+-128
	stw fp+-96, r12
	jal r0, .LBB18_620
.LBB18_567:
	ldw r1, r13+0
	addi r14, r1, 12
	add r17, r14, r19
	srli r1, r27, 24
	beq r1, r18, .LBB18_639
	jal r0, .LBB18_303
.LBB18_639:
	jal r0, .LBB18_304
.LBB18_568:
	ldw r3, fp+-172
	jal r0, .LBB18_33
.LBB18_569:
	ldw r4, fp+-140
	jal r0, .LBB18_586
.LBB18_570:
	ldw r4, fp+-160
	jal r0, .LBB18_590
.LBB18_571:
	ldw r3, fp+-152
	jal r0, .LBB18_594
.LBB18_572:
	ldw r3, fp+-148
	jal r0, .LBB18_598
.LBB18_573:
	ldw r4, fp+-156
	jal r0, .LBB18_602
.LBB18_574:
	ldw r3, fp+-144
	jal r0, .LBB18_606
.LBB18_575:
	ldw r3, fp+-132
	jal r0, .LBB18_610
.LBB18_576:
	ldw r3, fp+-168
	jal r0, .LBB18_613
.LBB18_577:
	ldw r3, fp+-124
.LBB18_578:
	addi r17, r0, 0
	add r26, r5, r0
	beq r20, r17, .LBB18_640
	jal r0, .LBB18_6
.LBB18_640:
	jal r0, .LBB18_133
.LBB18_579:
	ldw r4, fp+-164
	jal r0, .LBB18_617
.LBB18_580:
	ldw r3, fp+-128
	jal r0, .LBB18_621
.LBB18_581:
	addi r4, r0, 0
	beq r3, r4, .LBB18_633
	jal r0, .LBB18_272
.LBB18_582:
	add r8, r21, r0
	add r7, r26, r0
	add r26, r25, r0
	ldw r25, fp+-96
	ldw r21, fp+-104
	addi r3, r0, 0
	beq r5, r3, .LBB18_634
.LBB18_583:
	add r6, r28, r0
	add r28, r12, r0
	sub r3, r3, r4
	stw fp+-96, r25
	add r25, r26, r0
	add r26, r7, r0
	add r12, r28, r0
	add r28, r6, r0
	add r21, r8, r0
	jal r0, .LBB18_284
.LBB18_584:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	ldw r4, fp+-140
	xor r4, r4, r3
	and r1, r4, r1
	xor r4, r3, r1
	add r5, r17, r0
.LBB18_585:
	add r24, r23, r0
	add r12, r26, r0
	ldw r25, fp+-100
.LBB18_586:
	addi r17, r0, 0
	add r26, r5, r0
	bne r20, r17, .LBB18_587
	jal r0, .LBB18_67
.LBB18_587:
	stw fp+-140, r4
	jal r0, .LBB18_6
.LBB18_588:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	ldw r4, fp+-160
	xor r4, r4, r3
	and r1, r4, r1
	xor r4, r3, r1
.LBB18_589:
	add r5, r17, r0
	add r24, r23, r0
	add r12, r26, r0
	ldw r25, fp+-100
.LBB18_590:
	addi r17, r0, 0
	add r26, r5, r0
	bne r20, r17, .LBB18_591
	jal r0, .LBB18_74
.LBB18_591:
	stw fp+-160, r4
	jal r0, .LBB18_6
.LBB18_592:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r23, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r23
	ldw r4, fp+-152
	xor r4, r4, r3
	and r1, r4, r1
	xor r3, r3, r1
.LBB18_593:
	add r5, r17, r0
	add r24, r28, r0
	add r28, r21, r0
	add r21, r11, r0
	add r11, r13, r0
	add r13, r26, r0
	ldw r25, fp+-100
	ldw r12, fp+-120
.LBB18_594:
	addi r17, r0, 0
	add r26, r5, r0
	bne r23, r17, .LBB18_595
	jal r0, .LBB18_125
.LBB18_595:
	stw fp+-152, r3
	jal r0, .LBB18_6
.LBB18_596:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r23, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r23
	ldw r4, fp+-148
	xor r4, r4, r3
	and r1, r4, r1
	xor r3, r3, r1
.LBB18_597:
	add r5, r17, r0
	add r24, r28, r0
	add r28, r21, r0
	add r21, r11, r0
	add r11, r13, r0
	add r13, r26, r0
	ldw r25, fp+-100
	ldw r12, fp+-120
.LBB18_598:
	addi r17, r0, 0
	add r26, r5, r0
	bne r23, r17, .LBB18_599
	jal r0, .LBB18_169
.LBB18_599:
	stw fp+-148, r3
	jal r0, .LBB18_6
.LBB18_600:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	ldw r4, fp+-156
	xor r4, r4, r3
	and r1, r4, r1
	xor r4, r3, r1
.LBB18_601:
	add r5, r17, r0
	add r24, r23, r0
	add r12, r26, r0
	ldw r25, fp+-100
.LBB18_602:
	addi r17, r0, 0
	add r26, r5, r0
	bne r20, r17, .LBB18_603
	jal r0, .LBB18_172
.LBB18_603:
	stw fp+-156, r4
	jal r0, .LBB18_6
.LBB18_604:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r23, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r23
	ldw r4, fp+-144
	xor r4, r4, r3
	and r1, r4, r1
	xor r3, r3, r1
.LBB18_605:
	add r5, r17, r0
	add r24, r28, r0
	add r28, r21, r0
	add r21, r11, r0
	add r11, r13, r0
	add r13, r26, r0
	ldw r25, fp+-100
	ldw r12, fp+-120
.LBB18_606:
	addi r17, r0, 0
	add r26, r5, r0
	bne r23, r17, .LBB18_607
	jal r0, .LBB18_220
.LBB18_607:
	stw fp+-144, r3
	jal r0, .LBB18_6
.LBB18_608:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	xor r4, r17, r3
	and r1, r4, r1
	xor r5, r3, r1
	ldw r3, fp+-132
.LBB18_609:
	add r24, r23, r0
	add r12, r26, r0
	ldw r25, fp+-100
.LBB18_610:
	addi r17, r0, 0
	add r26, r5, r0
	beq r20, r17, .LBB18_641
	jal r0, .LBB18_6
.LBB18_641:
	jal r0, .LBB18_38
.LBB18_611:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	ldw r4, fp+-168
	xor r4, r4, r3
	and r1, r4, r1
	xor r3, r3, r1
	add r5, r17, r0
.LBB18_612:
	add r24, r23, r0
	add r12, r26, r0
	ldw r25, fp+-100
.LBB18_613:
	addi r17, r0, 0
	ldw r4, fp+-140
	add r26, r5, r0
	bne r20, r17, .LBB18_614
	jal r0, .LBB18_69
.LBB18_614:
	stw fp+-168, r3
	jal r0, .LBB18_6
.LBB18_615:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	ldw r4, fp+-164
	xor r4, r4, r3
	and r1, r4, r1
	xor r4, r3, r1
	add r5, r17, r0
.LBB18_616:
	add r24, r23, r0
	add r12, r26, r0
	ldw r25, fp+-100
.LBB18_617:
	addi r17, r0, 0
	add r26, r5, r0
	bne r20, r17, .LBB18_618
	jal r0, .LBB18_178
.LBB18_618:
	stw fp+-164, r4
	jal r0, .LBB18_6
.LBB18_619:
	lui r3, %hi(.LCPI18_2)
	addi r3, r3, %lo(.LCPI18_2)
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	lui r6, %hi(.LCPI18_3)
	addi r6, r6, %lo(.LCPI18_3)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	or  r20, r3, r6
	fcvt.w.d r3, r4
	sub r1, r1, r20
	xor r4, r12, r3
	and r1, r4, r1
	xor r1, r3, r1
	stw fp+-96, r1
	ldw r3, fp+-128
.LBB18_620:
	add r5, r17, r0
	add r24, r23, r0
	add r12, r26, r0
	ldw r25, fp+-100
.LBB18_621:
	addi r17, r0, 0
	ldw r4, fp+-96
	add r26, r5, r0
	beq r20, r17, .LBB18_642
	jal r0, .LBB18_6
.LBB18_642:
	jal r0, .LBB18_201
.LBB18_622:
	addi r1, r0, 2
	beq r17, r1, .LBB18_625
.LBB18_623:
	addi r1, r0, 3
	bne r17, r1, .LBB18_627
.LBB18_624:
	add r1, r15, r0
	jal r0, .LBB18_1
.LBB18_625:
	ldw r1, r11+112
	jal r0, .LBB18_1
.LBB18_626:
	add r3, r11, r0
	jal r31, luaG_tracecall
	add r15, r1, r0
	jal r0, .LBB18_2
.LBB18_627:
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
.LBB18_628:
	lui r5, %hi(.L.str.10)
	addi r5, r5, %lo(.L.str.10)
	add r3, r11, r0
	add r4, r16, r0
	jal r31, luaG_forerror
.LBB18_629:
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	add r3, r11, r0
	jal r31, luaG_runerror
.LBB18_630:
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	jal r0, .LBB18_632
.LBB18_631:
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
.LBB18_632:
	add r3, r11, r0
	add r4, r17, r0
	jal r31, luaG_forerror
.LBB18_633:
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	add r3, r11, r0
	jal r31, luaG_runerror
.LBB18_634:
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	add r3, r11, r0
	jal r31, luaG_runerror
.Lfunc_end18:
	.size	luaV_execute, .Lfunc_end18-luaV_execute
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI18_0:
	.word	.LBB18_11
	.word	.LBB18_122
	.word	.LBB18_113
	.word	.LBB18_118
	.word	.LBB18_88
	.word	.LBB18_142
	.word	.LBB18_156
	.word	.LBB18_120
	.word	.LBB18_165
	.word	.LBB18_100
	.word	.LBB18_161
	.word	.LBB18_80
	.word	.LBB18_96
	.word	.LBB18_153
	.word	.LBB18_71
	.word	.LBB18_114
	.word	.LBB18_59
	.word	.LBB18_126
	.word	.LBB18_157
	.word	.LBB18_189
	.word	.LBB18_134
	.word	.LBB18_150
	.word	.LBB18_180
	.word	.LBB18_206
	.word	.LBB18_107
	.word	.LBB18_101
	.word	.LBB18_225
	.word	.LBB18_55
	.word	.LBB18_212
	.word	.LBB18_218
	.word	.LBB18_167
	.word	.LBB18_123
	.word	.LBB18_170
	.word	.LBB18_72
	.word	.LBB18_58
	.word	.LBB18_49
	.word	.LBB18_50
	.word	.LBB18_44
	.word	.LBB18_222
	.word	.LBB18_186
	.word	.LBB18_89
	.word	.LBB18_129
	.word	.LBB18_197
	.word	.LBB18_34
	.word	.LBB18_65
	.word	.LBB18_174
	.word	.LBB18_236
	.word	.LBB18_47
	.word	.LBB18_76
	.word	.LBB18_85
	.word	.LBB18_31
	.word	.LBB18_247
	.word	.LBB18_235
	.word	.LBB18_45
	.word	.LBB18_228
	.word	.LBB18_234
	.word	.LBB18_48
	.word	.LBB18_196
	.word	.LBB18_77
	.word	.LBB18_203
	.word	.LBB18_90
	.word	.LBB18_139
	.word	.LBB18_147
	.word	.LBB18_115
	.word	.LBB18_93
	.word	.LBB18_144
	.word	.LBB18_39
	.word	.LBB18_63
	.word	.LBB18_252
	.word	.LBB18_51
	.word	.LBB18_24
	.word	.LBB18_40
	.word	.LBB18_229
	.word	.LBB18_237
	.word	.LBB18_240
	.word	.LBB18_26
	.word	.LBB18_27
	.word	.LBB18_29
	.word	.LBB18_250
	.word	.LBB18_12
	.word	.LBB18_3
	.word	.LBB18_83
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"index"
	.size	.L.str, 6

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"'__index' chain too long; possible loop"
	.size	.L.str.1, 40

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"'__newindex' chain too long; possible loop"
	.size	.L.str.2, 43

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"string length overflow"
	.size	.L.str.3, 23

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"get length of"
	.size	.L.str.4, 14

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"attempt to divide by zero"
	.size	.L.str.5, 26

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"attempt to perform 'n%%0'"
	.size	.L.str.6, 26

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"'for' step is zero"
	.size	.L.str.7, 19

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"limit"
	.size	.L.str.8, 6

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"step"
	.size	.L.str.9, 5

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"initial value"
	.size	.L.str.10, 14

	.hidden	luaT_gettmbyobj
	.hidden	luaG_typeerror
	.hidden	luaT_gettm
	.hidden	luaT_callTMres
	.hidden	luaH_get
	.hidden	luaG_runerror
	.hidden	luaH_finishset
	.hidden	luaC_barrierback_
	.hidden	luaT_callTM
	.hidden	luaS_eqlngstr
	.hidden	luaO_tostring
	.hidden	luaT_tryconcatTM
	.hidden	luaS_newlstr
	.hidden	luaS_createlngstrobj
	.hidden	luaH_getn
	.hidden	luaG_tracecall
	.hidden	luaG_traceexec
	.hidden	luaC_barrier_
	.hidden	luaH_getshortstr
	.hidden	luaH_getint
	.hidden	luaH_new
	.hidden	luaH_resize
	.hidden	luaC_step
	.hidden	luaH_getstr
	.hidden	luaT_trybinTM
	.hidden	luaT_trybiniTM
	.hidden	luaT_trybinassocTM
	.hidden	luaF_close
	.hidden	luaF_newtbcupval
	.hidden	luaT_callorderiTM
	.hidden	luaD_precall
	.hidden	luaF_closeupval
	.hidden	luaD_pretailcall
	.hidden	luaD_poscall
	.hidden	luaD_call
	.hidden	luaH_realasize
	.hidden	luaH_resizearray
	.hidden	luaT_getvarargs
	.hidden	luaT_adjustvarargs
	.hidden	luaD_hookcall
	.hidden	luaO_str2num
	.hidden	luaT_callorderTM
	.hidden	luaG_forerror
	.hidden	luaF_newLclosure
	.hidden	luaF_findupval
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
