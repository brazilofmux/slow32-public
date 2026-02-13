	.file	"lmathlib.c"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaopen_math
.LCPI0_0:
	.quad	0x407f800000000000              # double 504
.LCPI0_1:
	.quad	0x400921fb54442d18              # double 3.1415926535897931
.LCPI0_2:
	.quad	0x7ff0000000000000              # double +Inf
	.text
	.globl	luaopen_math
	.p2align	2
	.type	luaopen_math,@function
luaopen_math:                           # @luaopen_math
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
	add r11, r3, r0
	lui r1, %hi(.LCPI0_0)
	addi r1, r1, %lo(.LCPI0_0)
	ldw r6, r1+4
	ldw r5, r1+0
	addi r4, r0, 72
	jal r31, luaL_checkversion_
	addi r12, r0, 0
	addi r5, r0, 27
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_createtable
	lui r4, %hi(mathlib)
	addi r4, r4, %lo(mathlib)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_setfuncs
	lui r1, %hi(.LCPI0_1)
	addi r1, r1, %lo(.LCPI0_1)
	ldw r6, r1+4
	ldw r5, r1+0
	add r3, r11, r0
	jal r31, lua_pushnumber
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	addi r13, r0, -2
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	lui r1, %hi(.LCPI0_2)
	addi r1, r1, %lo(.LCPI0_2)
	ldw r6, r1+4
	ldw r5, r1+0
	add r3, r11, r0
	jal r31, lua_pushnumber
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	lui r14, 524288
	addi r4, r14, -1
	add r3, r11, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	addi r4, r0, 32
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_newuserdatauv
	add r13, r1, r0
	add r3, r12, r0
	jal r31, time
	add r14, r1, r0
	stw r13+4, r1
	stw r13+0, r12
	addi r1, r0, 255
	stw r13+12, r1
	stw r13+8, r12
	stw r13+20, r11
	stw r13+16, r12
	stw r13+28, r12
	stw r13+24, r12
	addi r16, r0, 16
	addi r15, fp, -36
.LBB0_1:
	add r3, r15, r0
	add r4, r13, r0
	jal r31, nextrand
	addi r16, r16, -1
	bne r16, r12, .LBB0_1
.LBB0_2:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushinteger
	add r3, r11, r0
	add r4, r11, r0
	jal r31, lua_pushinteger
	addi r4, r0, -3
	add r3, r11, r0
	jal r31, lua_settop
	lui r4, %hi(randfuncs)
	addi r4, r4, %lo(randfuncs)
	addi r12, r0, 1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_setfuncs
	add r1, r12, r0
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
.Lfunc_end0:
	.size	luaopen_math, .Lfunc_end0-luaopen_math
                                        # -- End function
	.p2align	2                               # -- Begin function math_abs
	.type	math_abs,@function
math_abs:                               # @math_abs
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	addi r4, r0, 1
	jal r31, lua_isinteger
	addi r4, r0, 1
	addi r3, r0, 0
	beq r1, r3, .LBB1_2
.LBB1_1:
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lua_tointegerx
	srai r3, r1, 31
	xor r1, r1, r3
	sub r4, r1, r3
	add r3, r11, r0
	jal r31, lua_pushinteger
	jal r0, .LBB1_3
.LBB1_2:
	add r3, r11, r0
	jal r31, luaL_checknumber
	add r3, r1, r0
	add r4, r2, r0
	jal r31, fabs
	add r3, r11, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
.LBB1_3:
	addi r1, r0, 1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	math_abs, .Lfunc_end1-math_abs
                                        # -- End function
	.p2align	2                               # -- Begin function math_acos
	.type	math_acos,@function
math_acos:                              # @math_acos
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checknumber
	add r3, r1, r0
	add r4, r2, r0
	jal r31, acos
	add r3, r11, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	math_acos, .Lfunc_end2-math_acos
                                        # -- End function
	.p2align	2                               # -- Begin function math_asin
	.type	math_asin,@function
math_asin:                              # @math_asin
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checknumber
	add r3, r1, r0
	add r4, r2, r0
	jal r31, asin
	add r3, r11, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	math_asin, .Lfunc_end3-math_asin
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function math_atan
.LCPI4_0:
	.quad	0x3ff0000000000000              # double 1
	.text
	.p2align	2
	.type	math_atan,@function
math_atan:                              # @math_atan
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checknumber
	add r13, r1, r0
	add r14, r2, r0
	lui r1, %hi(.LCPI4_0)
	addi r1, r1, %lo(.LCPI4_0)
	ldw r6, r1+4
	ldw r5, r1+0
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_optnumber
	add r3, r13, r0
	add r4, r14, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, atan2
	add r3, r11, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
	add r1, r12, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end4:
	.size	math_atan, .Lfunc_end4-math_atan
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function math_ceil
.LCPI5_0:
	.quad	0xc1e0000000000000              # double -2147483648
.LCPI5_1:
	.quad	0x41e0000000000000              # double 2147483648
	.text
	.p2align	2
	.type	math_ceil,@function
math_ceil:                              # @math_ceil
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
	addi r4, r0, 1
	jal r31, lua_isinteger
	addi r4, r0, 1
	addi r12, r0, 0
	beq r1, r12, .LBB5_2
.LBB5_1:
	add r3, r11, r0
	jal r31, lua_settop
	jal r0, .LBB5_6
.LBB5_2:
	add r3, r11, r0
	jal r31, luaL_checknumber
	add r3, r1, r0
	add r4, r2, r0
	jal r31, ceil
	add r4, r1, r0
	add r5, r2, r0
	lui r1, %hi(.LCPI5_0)
	addi r1, r1, %lo(.LCPI5_0)
	ldw r7, r1+4
	ldw r6, r1+0
	flt.d r1, r4, r6
	bne r1, r12, .LBB5_5
.LBB5_3:
	lui r1, %hi(.LCPI5_1)
	addi r1, r1, %lo(.LCPI5_1)
	ldw r7, r1+4
	ldw r6, r1+0
	fle.d r1, r6, r4
	bne r1, r12, .LBB5_5
.LBB5_4:
	fcvt.w.d r4, r4
	add r3, r11, r0
	jal r31, lua_pushinteger
	jal r0, .LBB5_6
.LBB5_5:
	add r3, r11, r0
	add r5, r4, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
.LBB5_6:
	addi r1, r0, 1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	math_ceil, .Lfunc_end5-math_ceil
                                        # -- End function
	.p2align	2                               # -- Begin function math_cos
	.type	math_cos,@function
math_cos:                               # @math_cos
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checknumber
	add r3, r1, r0
	add r4, r2, r0
	jal r31, cos
	add r3, r11, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	math_cos, .Lfunc_end6-math_cos
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function math_deg
.LCPI7_0:
	.quad	0x404ca5dc1a63c1f8              # double 57.295779513082323
	.text
	.p2align	2
	.type	math_deg,@function
math_deg:                               # @math_deg
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checknumber
	add r4, r1, r0
	add r5, r2, r0
	lui r1, %hi(.LCPI7_0)
	addi r1, r1, %lo(.LCPI7_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r6, r4, r6
	add r3, r11, r0
	add r5, r6, r0
	add r6, r7, r0
	jal r31, lua_pushnumber
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end7:
	.size	math_deg, .Lfunc_end7-math_deg
                                        # -- End function
	.p2align	2                               # -- Begin function math_exp
	.type	math_exp,@function
math_exp:                               # @math_exp
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checknumber
	add r3, r1, r0
	add r4, r2, r0
	jal r31, exp
	add r3, r11, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	math_exp, .Lfunc_end8-math_exp
                                        # -- End function
	.p2align	2                               # -- Begin function math_toint
	.type	math_toint,@function
math_toint:                             # @math_toint
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
	addi r4, r0, 1
	addi r12, fp, -16
	add r5, r12, r0
	jal r31, lua_tointegerx
	ldw r3, r12+0
	addi r4, r0, 0
	beq r3, r4, .LBB9_3
.LBB9_1:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushinteger
.LBB9_2:
	addi r1, r0, 1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB9_3:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_checkany
	add r3, r11, r0
	jal r31, lua_pushnil
	jal r0, .LBB9_2
.Lfunc_end9:
	.size	math_toint, .Lfunc_end9-math_toint
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function math_floor
.LCPI10_0:
	.quad	0xc1e0000000000000              # double -2147483648
.LCPI10_1:
	.quad	0x41e0000000000000              # double 2147483648
	.text
	.p2align	2
	.type	math_floor,@function
math_floor:                             # @math_floor
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
	addi r4, r0, 1
	jal r31, lua_isinteger
	addi r4, r0, 1
	addi r12, r0, 0
	beq r1, r12, .LBB10_2
.LBB10_1:
	add r3, r11, r0
	jal r31, lua_settop
	jal r0, .LBB10_6
.LBB10_2:
	add r3, r11, r0
	jal r31, luaL_checknumber
	add r3, r1, r0
	add r4, r2, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	lui r1, %hi(.LCPI10_0)
	addi r1, r1, %lo(.LCPI10_0)
	ldw r7, r1+4
	ldw r6, r1+0
	flt.d r1, r4, r6
	bne r1, r12, .LBB10_5
.LBB10_3:
	lui r1, %hi(.LCPI10_1)
	addi r1, r1, %lo(.LCPI10_1)
	ldw r7, r1+4
	ldw r6, r1+0
	fle.d r1, r6, r4
	bne r1, r12, .LBB10_5
.LBB10_4:
	fcvt.w.d r4, r4
	add r3, r11, r0
	jal r31, lua_pushinteger
	jal r0, .LBB10_6
.LBB10_5:
	add r3, r11, r0
	add r5, r4, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
.LBB10_6:
	addi r1, r0, 1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end10:
	.size	math_floor, .Lfunc_end10-math_floor
                                        # -- End function
	.p2align	2                               # -- Begin function math_fmod
	.type	math_fmod,@function
math_fmod:                              # @math_fmod
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, lua_isinteger
	addi r13, r0, 0
	beq r1, r13, .LBB11_5
.LBB11_1:
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_isinteger
	beq r1, r13, .LBB11_5
.LBB11_2:
	addi r4, r0, 2
	addi r14, r0, 0
	add r3, r11, r0
	add r5, r14, r0
	jal r31, lua_tointegerx
	add r13, r1, r0
	addi r1, r1, 1
	bgtu r1, r12, .LBB11_6
.LBB11_3:
	beq r13, r14, .LBB11_9
.LBB11_4:
	addi r4, r0, 0
	jal r0, .LBB11_7
.LBB11_5:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_checknumber
	add r12, r1, r0
	add r13, r2, r0
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_checknumber
	add r3, r12, r0
	add r4, r13, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, fmod
	add r3, r11, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
	jal r0, .LBB11_8
.LBB11_6:
	addi r4, r0, 1
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lua_tointegerx
	rem r4, r1, r13
.LBB11_7:
	add r3, r11, r0
	jal r31, lua_pushinteger
.LBB11_8:
	addi r1, r0, 1
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB11_9:
	lui r5, %hi(.L.str.27)
	addi r5, r5, %lo(.L.str.27)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB11_4
.Lfunc_end11:
	.size	math_fmod, .Lfunc_end11-math_fmod
                                        # -- End function
	.p2align	2                               # -- Begin function math_ult
	.type	math_ult,@function
math_ult:                               # @math_ult
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, lr
	add r11, r3, r0
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checkinteger
	add r13, r1, r0
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_checkinteger
	sltu r4, r13, r1
	add r3, r11, r0
	jal r31, lua_pushboolean
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end12:
	.size	math_ult, .Lfunc_end12-math_ult
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function math_log
.LCPI13_0:
	.quad	0x4024000000000000              # double 10
	.text
	.p2align	2
	.type	math_log,@function
math_log:                               # @math_log
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
	add r11, r3, r0
	addi r4, r0, 1
	jal r31, luaL_checknumber
	add r13, r1, r0
	add r12, r2, r0
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_type
	addi r16, r0, 0
	ble r1, r16, .LBB13_3
.LBB13_1:
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_checknumber
	add r14, r1, r0
	add r15, r2, r0
	lui r1, %hi(.LCPI13_0)
	addi r1, r1, %lo(.LCPI13_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r14, r4
	add r3, r13, r0
	add r4, r12, r0
	xori r1, r1, 1
	bne r1, r16, .LBB13_5
.LBB13_2:
	jal r31, log10
	jal r0, .LBB13_4
.LBB13_3:
	add r3, r13, r0
	add r4, r12, r0
	jal r31, log
.LBB13_4:
	add r6, r1, r0
	add r7, r2, r0
	jal r0, .LBB13_6
.LBB13_5:
	jal r31, log
	add r12, r1, r0
	add r13, r2, r0
	add r3, r14, r0
	add r4, r15, r0
	jal r31, log
	add r4, r1, r0
	add r5, r2, r0
	fdiv.d r6, r12, r4
.LBB13_6:
	add r3, r11, r0
	add r5, r6, r0
	add r6, r7, r0
	jal r31, lua_pushnumber
	addi r1, r0, 1
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
.Lfunc_end13:
	.size	math_log, .Lfunc_end13-math_log
                                        # -- End function
	.p2align	2                               # -- Begin function math_max
	.type	math_max,@function
math_max:                               # @math_max
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
	add r11, r3, r0
	jal r31, lua_gettop
	add r12, r1, r0
	addi r16, r0, 0
	ble r1, r16, .LBB14_3
.LBB14_1:
	addi r1, r0, 2
	bge r12, r1, .LBB14_4
.LBB14_2:
	addi r13, r0, 1
	jal r0, .LBB14_6
.LBB14_3:
	lui r5, %hi(.L.str.28)
	addi r5, r5, %lo(.L.str.28)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_argerror
	addi r1, r0, 2
	blt r12, r1, .LBB14_2
.LBB14_4:
	addi r14, r0, 1
	add r15, r14, r0
	add r13, r14, r0
.LBB14_5:
	addi r15, r15, 1
	add r3, r11, r0
	add r4, r13, r0
	add r5, r15, r0
	add r6, r14, r0
	jal r31, lua_compare
	seq r1, r1, r16
	sub r1, r16, r1
	xor r3, r13, r15
	and r1, r3, r1
	xor r13, r15, r1
	bne r12, r15, .LBB14_5
.LBB14_6:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_pushvalue
	addi r1, r0, 1
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
.Lfunc_end14:
	.size	math_max, .Lfunc_end14-math_max
                                        # -- End function
	.p2align	2                               # -- Begin function math_min
	.type	math_min,@function
math_min:                               # @math_min
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
	add r11, r3, r0
	jal r31, lua_gettop
	add r12, r1, r0
	addi r16, r0, 0
	ble r1, r16, .LBB15_3
.LBB15_1:
	addi r1, r0, 2
	bge r12, r1, .LBB15_4
.LBB15_2:
	addi r13, r0, 1
	jal r0, .LBB15_6
.LBB15_3:
	lui r5, %hi(.L.str.28)
	addi r5, r5, %lo(.L.str.28)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_argerror
	addi r1, r0, 2
	blt r12, r1, .LBB15_2
.LBB15_4:
	addi r14, r0, 1
	add r15, r14, r0
	add r13, r14, r0
.LBB15_5:
	addi r15, r15, 1
	add r3, r11, r0
	add r4, r15, r0
	add r5, r13, r0
	add r6, r14, r0
	jal r31, lua_compare
	seq r1, r1, r16
	sub r1, r16, r1
	xor r3, r13, r15
	and r1, r3, r1
	xor r13, r15, r1
	bne r12, r15, .LBB15_5
.LBB15_6:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_pushvalue
	addi r1, r0, 1
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
.Lfunc_end15:
	.size	math_min, .Lfunc_end15-math_min
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function math_modf
.LCPI16_0:
	.quad	0x0000000000000000              # double 0
.LCPI16_1:
	.quad	0xc1e0000000000000              # double -2147483648
.LCPI16_2:
	.quad	0x41e0000000000000              # double 2147483648
	.text
	.p2align	2
	.type	math_modf,@function
math_modf:                              # @math_modf
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 56
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r14
	stw fp+-16, r15
	stw fp+-20, r16
	stw fp+-24, r17
	stw fp+-28, r18
	stw fp+-32, r19
	stw fp+-36, lr
	add r11, r3, r0
	addi r4, r0, 1
	jal r31, lua_isinteger
	addi r4, r0, 1
	add r3, r11, r0
	addi r12, r0, 0
	beq r1, r12, .LBB16_2
.LBB16_1:
	jal r31, lua_settop
	lui r1, %hi(.LCPI16_0)
	addi r1, r1, %lo(.LCPI16_0)
	ldw r7, r1+4
	ldw r6, r1+0
	jal r0, .LBB16_10
.LBB16_2:
	jal r31, luaL_checknumber
	add r16, r1, r0
	add r17, r2, r0
	lui r1, %hi(.LCPI16_0)
	addi r1, r1, %lo(.LCPI16_0)
	ldw r15, r1+4
	ldw r14, r1+0
	add r3, r16, r0
	add r4, r2, r0
	fle.d r1, r14, r16
	bne r1, r12, .LBB16_4
.LBB16_3:
	jal r31, ceil
	jal r0, .LBB16_5
.LBB16_4:
	jal r31, floor
.LBB16_5:
	add r18, r1, r0
	add r19, r2, r0
	lui r1, %hi(.LCPI16_1)
	addi r1, r1, %lo(.LCPI16_1)
	ldw r5, r1+4
	ldw r4, r1+0
	flt.d r1, r18, r4
	bne r1, r12, .LBB16_8
.LBB16_6:
	lui r1, %hi(.LCPI16_2)
	addi r1, r1, %lo(.LCPI16_2)
	ldw r5, r1+4
	ldw r4, r1+0
	fle.d r1, r4, r18
	bne r1, r12, .LBB16_8
.LBB16_7:
	fcvt.w.d r4, r18
	add r3, r11, r0
	jal r31, lua_pushinteger
	jal r0, .LBB16_9
.LBB16_8:
	add r3, r11, r0
	add r5, r18, r0
	add r6, r19, r0
	jal r31, lua_pushnumber
.LBB16_9:
	feq.d r1, r16, r18
	fsub.d r4, r16, r18
	xor r3, r15, r5
	sub r1, r12, r1
	and r3, r3, r1
	xor r7, r5, r3
	xor r3, r14, r4
	and r1, r3, r1
	xor r6, r4, r1
.LBB16_10:
	add r3, r11, r0
	add r5, r6, r0
	add r6, r7, r0
	jal r31, lua_pushnumber
	addi r1, r0, 2
	ldw lr, fp+-36
	ldw r19, fp+-32
	ldw r18, fp+-28
	ldw r17, fp+-24
	ldw r16, fp+-20
	ldw r15, fp+-16
	ldw r14, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end16:
	.size	math_modf, .Lfunc_end16-math_modf
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function math_rad
.LCPI17_0:
	.quad	0x3f91df46a2529d39              # double 0.017453292519943295
	.text
	.p2align	2
	.type	math_rad,@function
math_rad:                               # @math_rad
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checknumber
	add r4, r1, r0
	add r5, r2, r0
	lui r1, %hi(.LCPI17_0)
	addi r1, r1, %lo(.LCPI17_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r6, r4, r6
	add r3, r11, r0
	add r5, r6, r0
	add r6, r7, r0
	jal r31, lua_pushnumber
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end17:
	.size	math_rad, .Lfunc_end17-math_rad
                                        # -- End function
	.p2align	2                               # -- Begin function math_sin
	.type	math_sin,@function
math_sin:                               # @math_sin
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checknumber
	add r3, r1, r0
	add r4, r2, r0
	jal r31, sin
	add r3, r11, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end18:
	.size	math_sin, .Lfunc_end18-math_sin
                                        # -- End function
	.p2align	2                               # -- Begin function math_sqrt
	.type	math_sqrt,@function
math_sqrt:                              # @math_sqrt
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checknumber
	add r3, r1, r0
	add r4, r2, r0
	jal r31, sqrt
	add r3, r11, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end19:
	.size	math_sqrt, .Lfunc_end19-math_sqrt
                                        # -- End function
	.p2align	2                               # -- Begin function math_tan
	.type	math_tan,@function
math_tan:                               # @math_tan
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checknumber
	add r3, r1, r0
	add r4, r2, r0
	jal r31, tan
	add r3, r11, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, lua_pushnumber
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end20:
	.size	math_tan, .Lfunc_end20-math_tan
                                        # -- End function
	.p2align	2                               # -- Begin function math_type
	.type	math_type,@function
math_type:                              # @math_type
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	addi r4, r0, 1
	jal r31, lua_type
	addi r4, r0, 1
	addi r3, r0, 3
	bne r1, r3, .LBB21_2
.LBB21_1:
	add r3, r11, r0
	jal r31, lua_isinteger
	addi r3, r0, 0
	seq r1, r1, r3
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	lui r5, %hi(.L.str.30)
	addi r5, r5, %lo(.L.str.30)
	xor r5, r5, r4
	sub r1, r3, r1
	and r1, r5, r1
	xor r4, r1, r4
	add r3, r11, r0
	jal r31, lua_pushstring
	jal r0, .LBB21_3
.LBB21_2:
	add r3, r11, r0
	jal r31, luaL_checkany
	add r3, r11, r0
	jal r31, lua_pushnil
.LBB21_3:
	addi r1, r0, 1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end21:
	.size	math_type, .Lfunc_end21-math_type
                                        # -- End function
	.p2align	2                               # -- Begin function nextrand
	.type	nextrand,@function
nextrand:                               # @nextrand
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	ldw r5, r4+8
	ldw r1, r4+12
	srli r6, r1, 30
	slli r7, r5, 2
	or  r6, r7, r6
	slli r7, r1, 2
	add r6, r6, r5
	add r8, r7, r1
	sltu r7, r8, r7
	add r6, r6, r7
	slli r7, r6, 7
	srli r9, r8, 25
	or  r7, r7, r9
	srli r6, r6, 25
	slli r8, r8, 7
	or  r9, r8, r6
	slli r6, r7, 3
	srli r8, r8, 29
	or  r6, r6, r8
	slli r8, r9, 3
	add r6, r6, r7
	add r7, r8, r9
	stw r3+0, r6
	stw r3+4, r7
	bgeu r7, r8, .LBB22_2
.LBB22_1:
	addi r6, r6, 1
	stw r3+0, r6
.LBB22_2:
	srli r3, r1, 15
	slli r6, r5, 17
	or  r3, r6, r3
	slli r6, r1, 17
	ldw r7, r4+0
	ldw r8, r4+4
	ldw r9, r4+16
	xor r9, r9, r7
	ldw r10, r4+20
	xor r10, r10, r8
	ldw r11, r4+24
	xor r11, r11, r5
	ldw r12, r4+28
	xor r12, r12, r1
	xor r5, r9, r5
	stw r4+8, r5
	xor r1, r10, r1
	stw r4+12, r1
	xor r1, r11, r7
	stw r4+0, r1
	xor r1, r12, r8
	stw r4+4, r1
	xor r1, r9, r3
	stw r4+16, r1
	xor r1, r10, r6
	stw r4+20, r1
	srli r1, r11, 19
	slli r3, r12, 13
	or  r1, r3, r1
	srli r3, r12, 19
	slli r5, r11, 13
	or  r3, r5, r3
	stw r4+24, r1
	stw r4+28, r3
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end22:
	.size	nextrand, .Lfunc_end22-nextrand
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function math_random
.LCPI23_0:
	.quad	0x4140000000000000              # double 2097152
.LCPI23_1:
	.quad	0x3ca0000000000000              # double 1.1102230246251565E-16
	.text
	.p2align	2
	.type	math_random,@function
math_random:                            # @math_random
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
	add r11, r3, r0
	lui r1, 1048572
	addi r4, r1, 383
	jal r31, lua_touserdata
	add r12, r1, r0
	addi r14, fp, -44
	add r3, r14, r0
	add r4, r1, r0
	jal r31, nextrand
	add r3, r11, r0
	jal r31, lua_gettop
	addi r4, r0, 1
	addi r3, r0, 2
	beq r1, r3, .LBB23_6
.LBB23_1:
	beq r1, r4, .LBB23_4
.LBB23_2:
	addi r3, r0, 0
	bne r1, r3, .LBB23_12
.LBB23_3:
	ldw r1, r14+0
	fcvt.d.wu r6, r1
	lui r1, %hi(.LCPI23_0)
	addi r1, r1, %lo(.LCPI23_0)
	ldw r9, r1+4
	ldw r8, r1+0
	fmul.d r6, r6, r8
	ldw r1, r14+4
	srli r1, r1, 11
	fcvt.d.wu r8, r1
	fadd.d r6, r6, r8
	lui r1, %hi(.LCPI23_1)
	addi r1, r1, %lo(.LCPI23_1)
	ldw r9, r1+4
	ldw r8, r1+0
	fmul.d r6, r6, r8
	add r3, r11, r0
	add r5, r6, r0
	add r6, r7, r0
	add r11, r4, r0
	jal r31, lua_pushnumber
	add r1, r11, r0
	jal r0, .LBB23_16
.LBB23_4:
	addi r13, r0, 1
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_checkinteger
	addi r3, r0, 0
	bne r1, r3, .LBB23_7
.LBB23_5:
	ldw r4, r14+4
	jal r0, .LBB23_15
.LBB23_6:
	add r3, r11, r0
	jal r31, luaL_checkinteger
	add r13, r1, r0
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_checkinteger
.LBB23_7:
	blt r1, r13, .LBB23_17
.LBB23_8:
	ldw r3, r14+4
	sub r15, r1, r13
	addi r1, r15, 1
	and r1, r1, r15
	addi r4, r0, 0
	beq r1, r4, .LBB23_13
.LBB23_9:
	srli r1, r15, 1
	or  r1, r1, r15
	srli r4, r1, 2
	or  r1, r4, r1
	srli r4, r1, 4
	or  r1, r4, r1
	srli r4, r1, 8
	or  r1, r4, r1
	srli r4, r1, 16
	or  r16, r4, r1
	and r1, r3, r16
	bleu r1, r15, .LBB23_14
.LBB23_10:
	addi r14, fp, -36
.LBB23_11:
	add r3, r14, r0
	add r4, r12, r0
	jal r31, nextrand
	ldw r1, r14+4
	and r1, r1, r16
	bgtu r1, r15, .LBB23_11
	jal r0, .LBB23_14
.LBB23_12:
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB23_16
.LBB23_13:
	and r1, r3, r15
.LBB23_14:
	add r4, r1, r13
.LBB23_15:
	add r3, r11, r0
	jal r31, lua_pushinteger
	addi r1, r0, 1
.LBB23_16:
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
.LBB23_17:
	lui r5, %hi(.L.str.32)
	addi r5, r5, %lo(.L.str.32)
	addi r4, r0, 1
	add r3, r11, r0
	add r15, r1, r0
	jal r31, luaL_argerror
	add r1, r15, r0
	jal r0, .LBB23_8
.Lfunc_end23:
	.size	math_random, .Lfunc_end23-math_random
                                        # -- End function
	.p2align	2                               # -- Begin function math_randomseed
	.type	math_randomseed,@function
math_randomseed:                        # @math_randomseed
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
	stw fp+-32, lr
	add r11, r3, r0
	lui r1, 1048572
	addi r4, r1, 383
	jal r31, lua_touserdata
	add r12, r1, r0
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_type
	addi r3, r0, -1
	beq r1, r3, .LBB24_3
.LBB24_1:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_checkinteger
	add r13, r1, r0
	addi r4, r0, 2
	addi r15, r0, 0
	add r3, r11, r0
	add r5, r15, r0
	jal r31, luaL_optinteger
	add r14, r1, r0
	stw r12+4, r13
	stw r12+0, r15
	addi r1, r0, 255
	stw r12+12, r1
	stw r12+8, r15
	stw r12+20, r14
	stw r12+16, r15
	stw r12+28, r15
	stw r12+24, r15
	addi r17, r0, 16
	addi r16, fp, -40
.LBB24_2:
	add r3, r16, r0
	add r4, r12, r0
	jal r31, nextrand
	addi r17, r17, -1
	bne r17, r15, .LBB24_2
	jal r0, .LBB24_6
.LBB24_3:
	addi r14, r0, 0
	add r3, r14, r0
	jal r31, time
	add r13, r1, r0
	stw r12+4, r1
	stw r12+0, r14
	addi r1, r0, 255
	stw r12+12, r1
	stw r12+8, r14
	stw r12+20, r11
	stw r12+16, r14
	stw r12+28, r14
	stw r12+24, r14
	addi r16, r0, 16
	addi r15, fp, -40
.LBB24_4:
	add r3, r15, r0
	add r4, r12, r0
	jal r31, nextrand
	addi r16, r16, -1
	bne r16, r14, .LBB24_4
.LBB24_5:
	add r14, r11, r0
.LBB24_6:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_pushinteger
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushinteger
	addi r1, r0, 2
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
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end24:
	.size	math_randomseed, .Lfunc_end24-math_randomseed
                                        # -- End function
	.type	mathlib,@object                 # @mathlib
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
mathlib:
	.word	.L.str.4
	.word	math_abs
	.word	.L.str.5
	.word	math_acos
	.word	.L.str.6
	.word	math_asin
	.word	.L.str.7
	.word	math_atan
	.word	.L.str.8
	.word	math_ceil
	.word	.L.str.9
	.word	math_cos
	.word	.L.str.10
	.word	math_deg
	.word	.L.str.11
	.word	math_exp
	.word	.L.str.12
	.word	math_toint
	.word	.L.str.13
	.word	math_floor
	.word	.L.str.14
	.word	math_fmod
	.word	.L.str.15
	.word	math_ult
	.word	.L.str.16
	.word	math_log
	.word	.L.str.17
	.word	math_max
	.word	.L.str.18
	.word	math_min
	.word	.L.str.19
	.word	math_modf
	.word	.L.str.20
	.word	math_rad
	.word	.L.str.21
	.word	math_sin
	.word	.L.str.22
	.word	math_sqrt
	.word	.L.str.23
	.word	math_tan
	.word	.L.str.24
	.word	math_type
	.word	.L.str.25
	.word	0
	.word	.L.str.26
	.word	0
	.word	.L.str
	.word	0
	.word	.L.str.1
	.word	0
	.word	.L.str.2
	.word	0
	.word	.L.str.3
	.word	0
	.zero	8
	.size	mathlib, 224

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"pi"
	.size	.L.str, 3

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"huge"
	.size	.L.str.1, 5

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"maxinteger"
	.size	.L.str.2, 11

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"mininteger"
	.size	.L.str.3, 11

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"abs"
	.size	.L.str.4, 4

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"acos"
	.size	.L.str.5, 5

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"asin"
	.size	.L.str.6, 5

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"atan"
	.size	.L.str.7, 5

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"ceil"
	.size	.L.str.8, 5

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"cos"
	.size	.L.str.9, 4

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"deg"
	.size	.L.str.10, 4

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"exp"
	.size	.L.str.11, 4

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"tointeger"
	.size	.L.str.12, 10

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"floor"
	.size	.L.str.13, 6

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"fmod"
	.size	.L.str.14, 5

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"ult"
	.size	.L.str.15, 4

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"log"
	.size	.L.str.16, 4

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"max"
	.size	.L.str.17, 4

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"min"
	.size	.L.str.18, 4

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"modf"
	.size	.L.str.19, 5

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"rad"
	.size	.L.str.20, 4

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"sin"
	.size	.L.str.21, 4

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"sqrt"
	.size	.L.str.22, 5

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"tan"
	.size	.L.str.23, 4

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"type"
	.size	.L.str.24, 5

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"random"
	.size	.L.str.25, 7

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"randomseed"
	.size	.L.str.26, 11

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"zero"
	.size	.L.str.27, 5

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"value expected"
	.size	.L.str.28, 15

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"integer"
	.size	.L.str.29, 8

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"float"
	.size	.L.str.30, 6

	.type	randfuncs,@object               # @randfuncs
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
randfuncs:
	.word	.L.str.25
	.word	math_random
	.word	.L.str.26
	.word	math_randomseed
	.zero	8
	.size	randfuncs, 24

	.type	.L.str.31,@object               # @.str.31
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.31:
	.asciz	"wrong number of arguments"
	.size	.L.str.31, 26

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"interval is empty"
	.size	.L.str.32, 18

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
