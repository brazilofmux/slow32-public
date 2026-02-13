	.file	"lbaselib.c"
	.text
	.globl	luaopen_base                    # -- Begin function luaopen_base
	.p2align	2
	.type	luaopen_base,@function
luaopen_base:                           # @luaopen_base
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
	lui r1, 1048572
	addi r4, r1, 384
	addi r5, r0, 2
	jal r31, lua_rawgeti
	lui r4, %hi(base_funcs)
	addi r4, r4, %lo(base_funcs)
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, luaL_setfuncs
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_pushvalue
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	addi r12, r0, -2
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_setfield
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	add r3, r11, r0
	jal r31, lua_pushstring
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_setfield
	addi r1, r0, 1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaopen_base, .Lfunc_end0-luaopen_base
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_assert
	.type	luaB_assert,@function
luaB_assert:                            # @luaB_assert
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
	jal r31, lua_toboolean
	addi r3, r0, 0
	beq r1, r3, .LBB1_3
.LBB1_1:
	add r3, r11, r0
	jal r31, lua_gettop
.LBB1_2:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB1_3:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_checkany
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rotate
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	lui r4, %hi(.L.str.26)
	addi r4, r4, %lo(.L.str.26)
	add r3, r11, r0
	jal r31, lua_pushstring
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_settop
	add r3, r11, r0
	jal r31, luaB_error
	jal r0, .LBB1_2
.Lfunc_end1:
	.size	luaB_assert, .Lfunc_end1-luaB_assert
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaB_collectgarbage
.LCPI2_0:
	.quad	0x3f50000000000000              # double 9.765625E-4
	.text
	.p2align	2
	.type	luaB_collectgarbage,@function
luaB_collectgarbage:                    # @luaB_collectgarbage
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
	add r11, r3, r0
	lui r5, %hi(.L.str.29)
	addi r5, r5, %lo(.L.str.29)
	lui r6, %hi(luaB_collectgarbage.opts)
	addi r6, r6, %lo(luaB_collectgarbage.opts)
	addi r4, r0, 1
	jal r31, luaL_checkoption
	slli r1, r1, 2
	lui r3, %hi(luaB_collectgarbage.optsnum)
	addi r3, r3, %lo(luaB_collectgarbage.optsnum)
	add r1, r1, r3
	ldw r12, r1+0
	addi r1, r12, -3
	addi r3, r0, 8
	bgtu r1, r3, .LBB2_3
.LBB2_1:
	slli r1, r1, 2
	lui r3, %hi(.LJTI2_0)
	addi r3, r3, %lo(.LJTI2_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB2_2:
	addi r4, r0, 2
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, luaL_optinteger
	add r3, r11, r0
	add r4, r12, r0
	add r5, r1, r0
	jal r0, .LBB2_4
.LBB2_3:
	add r3, r11, r0
	add r4, r12, r0
.LBB2_4:
	jal r31, lua_gc
	addi r3, r0, -1
	beq r1, r3, .LBB2_17
.LBB2_5:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushinteger
	jal r0, .LBB2_18
.LBB2_6:
	addi r4, r0, 2
	addi r12, r0, 0
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_optinteger
	add r13, r1, r0
	addi r4, r0, 3
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_optinteger
	addi r4, r0, 10
	add r3, r11, r0
	add r5, r13, r0
	add r6, r1, r0
	jal r31, lua_gc
	addi r3, r0, -1
	beq r1, r3, .LBB2_17
.LBB2_7:
	addi r3, r0, 11
	seq r1, r1, r3
	jal r0, .LBB2_10
.LBB2_8:
	addi r4, r0, 2
	addi r12, r0, 0
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_optinteger
	add r14, r1, r0
	addi r4, r0, 3
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_optinteger
	add r15, r1, r0
	addi r4, r0, 4
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_optinteger
	addi r13, r0, 11
	add r3, r11, r0
	add r4, r13, r0
	add r5, r14, r0
	add r6, r15, r0
	add r7, r1, r0
	jal r31, lua_gc
	addi r3, r0, -1
	beq r1, r3, .LBB2_17
.LBB2_9:
	seq r1, r1, r13
.LBB2_10:
	lui r3, %hi(.L.str.35)
	addi r3, r3, %lo(.L.str.35)
	lui r4, %hi(.L.str.36)
	addi r4, r4, %lo(.L.str.36)
	xor r4, r4, r3
	sub r1, r12, r1
	and r1, r4, r1
	xor r4, r1, r3
	add r3, r11, r0
	jal r31, lua_pushstring
	jal r0, .LBB2_18
.LBB2_11:
	addi r4, r0, 2
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, luaL_optinteger
	addi r4, r0, 5
	add r3, r11, r0
	add r5, r1, r0
	jal r0, .LBB2_15
.LBB2_12:
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, lua_gc
	add r12, r1, r0
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, lua_gc
	addi r3, r0, -1
	beq r12, r3, .LBB2_17
.LBB2_13:
	fcvt.d.w r4, r12
	fcvt.d.w r6, r1
	lui r1, %hi(.LCPI2_0)
	addi r1, r1, %lo(.LCPI2_0)
	ldw r9, r1+4
	ldw r8, r1+0
	fmul.d r6, r6, r8
	fadd.d r6, r6, r4
	add r3, r11, r0
	add r5, r6, r0
	add r6, r7, r0
	jal r31, lua_pushnumber
	jal r0, .LBB2_18
.LBB2_14:
	addi r4, r0, 9
	add r3, r11, r0
.LBB2_15:
	jal r31, lua_gc
	addi r3, r0, -1
	beq r1, r3, .LBB2_17
.LBB2_16:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushboolean
	jal r0, .LBB2_18
.LBB2_17:
	add r3, r11, r0
	jal r31, lua_pushnil
.LBB2_18:
	addi r1, r0, 1
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
.Lfunc_end2:
	.size	luaB_collectgarbage, .Lfunc_end2-luaB_collectgarbage
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI2_0:
	.word	.LBB2_12
	.word	.LBB2_3
	.word	.LBB2_11
	.word	.LBB2_2
	.word	.LBB2_2
	.word	.LBB2_3
	.word	.LBB2_14
	.word	.LBB2_6
	.word	.LBB2_8
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function luaB_dofile
	.type	luaB_dofile,@function
luaB_dofile:                            # @luaB_dofile
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
	addi r13, r0, 0
	add r4, r12, r0
	add r5, r13, r0
	add r6, r13, r0
	jal r31, luaL_optlstring
	add r14, r1, r0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, luaL_loadfilex
	bne r1, r13, .LBB3_3
.LBB3_1:
	lui r7, %hi(dofilecont)
	addi r7, r7, %lo(dofilecont)
	addi r5, r0, -1
	addi r4, r0, 0
	add r3, r11, r0
	add r6, r4, r0
	jal r31, lua_callk
	add r3, r11, r0
	jal r31, lua_gettop
	addi r1, r1, -1
.LBB3_2:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB3_3:
	add r3, r11, r0
	jal r31, lua_error
	jal r0, .LBB3_2
.Lfunc_end3:
	.size	luaB_dofile, .Lfunc_end3-luaB_dofile
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_error
	.type	luaB_error,@function
luaB_error:                             # @luaB_error
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
	addi r4, r0, 2
	addi r12, r0, 1
	add r5, r12, r0
	jal r31, luaL_optinteger
	add r13, r1, r0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_type
	addi r3, r0, 4
	bne r1, r3, .LBB4_3
.LBB4_1:
	blt r13, r12, .LBB4_3
.LBB4_2:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_where
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_pushvalue
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_concat
.LBB4_3:
	add r3, r11, r0
	jal r31, lua_error
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	luaB_error, .Lfunc_end4-luaB_error
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_getmetatable
	.type	luaB_getmetatable,@function
luaB_getmetatable:                      # @luaB_getmetatable
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
	jal r31, luaL_checkany
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_getmetatable
	addi r3, r0, 0
	beq r1, r3, .LBB5_2
.LBB5_1:
	lui r5, %hi(.L.str.37)
	addi r5, r5, %lo(.L.str.37)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_getmetafield
	jal r0, .LBB5_3
.LBB5_2:
	add r3, r11, r0
	jal r31, lua_pushnil
.LBB5_3:
	addi r1, r0, 1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	luaB_getmetatable, .Lfunc_end5-luaB_getmetatable
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_ipairs
	.type	luaB_ipairs,@function
luaB_ipairs:                            # @luaB_ipairs
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
	jal r31, luaL_checkany
	lui r4, %hi(ipairsaux)
	addi r4, r4, %lo(ipairsaux)
	addi r13, r0, 0
	add r3, r11, r0
	add r5, r13, r0
	jal r31, lua_pushcclosure
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushvalue
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_pushinteger
	addi r1, r0, 3
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	luaB_ipairs, .Lfunc_end6-luaB_ipairs
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_loadfile
	.type	luaB_loadfile,@function
luaB_loadfile:                          # @luaB_loadfile
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
	stw fp+-28, r17
	stw fp+-32, lr
	add r11, r3, r0
	addi r12, r0, 1
	addi r13, r0, 0
	add r4, r12, r0
	add r5, r13, r0
	add r6, r13, r0
	jal r31, luaL_optlstring
	add r16, r1, r0
	addi r14, r0, 2
	add r3, r11, r0
	add r4, r14, r0
	add r5, r13, r0
	add r6, r13, r0
	jal r31, luaL_optlstring
	add r17, r1, r0
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, lua_type
	add r15, r1, r0
	add r3, r11, r0
	add r4, r16, r0
	add r5, r17, r0
	jal r31, luaL_loadfilex
	bne r1, r13, .LBB7_5
.LBB7_1:
	addi r1, r0, -1
	beq r15, r1, .LBB7_4
.LBB7_2:
	seq r1, r15, r1
	addi r1, r1, -1
	andi r4, r1, 3
	add r3, r11, r0
	jal r31, lua_pushvalue
	addi r4, r0, -2
	addi r12, r0, 1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_setupvalue
	bne r1, r13, .LBB7_4
.LBB7_3:
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
.LBB7_4:
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
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB7_5:
	add r3, r11, r0
	jal r31, lua_pushnil
	addi r4, r0, -2
	addi r5, r0, 1
	add r3, r11, r0
	jal r31, lua_rotate
	add r12, r14, r0
	jal r0, .LBB7_4
.Lfunc_end7:
	.size	luaB_loadfile, .Lfunc_end7-luaB_loadfile
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_load
	.type	luaB_load,@function
luaB_load:                              # @luaB_load
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
	addi r12, r0, 1
	addi r16, fp, -36
	add r4, r12, r0
	add r5, r16, r0
	jal r31, lua_tolstring
	add r17, r1, r0
	lui r5, %hi(.L.str.38)
	addi r5, r5, %lo(.L.str.38)
	addi r4, r0, 3
	addi r13, r0, 0
	add r3, r11, r0
	add r6, r13, r0
	jal r31, luaL_optlstring
	add r15, r1, r0
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, lua_type
	add r14, r1, r0
	beq r17, r13, .LBB8_6
.LBB8_1:
	addi r4, r0, 2
	addi r6, r0, 0
	add r3, r11, r0
	add r5, r17, r0
	jal r31, luaL_optlstring
	ldw r5, r16+0
	add r3, r11, r0
	add r4, r17, r0
	add r6, r1, r0
	add r7, r15, r0
	jal r31, luaL_loadbufferx
	bne r1, r13, .LBB8_7
.LBB8_2:
	addi r1, r0, -1
	beq r14, r1, .LBB8_5
.LBB8_3:
	seq r1, r14, r1
	addi r1, r1, -1
	andi r4, r1, 4
	add r3, r11, r0
	jal r31, lua_pushvalue
	addi r4, r0, -2
	addi r12, r0, 1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_setupvalue
	bne r1, r13, .LBB8_5
.LBB8_4:
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
.LBB8_5:
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
	addi sp, sp, 56
	jalr r0, r31, 0
.LBB8_6:
	lui r5, %hi(.L.str.39)
	addi r5, r5, %lo(.L.str.39)
	addi r4, r0, 2
	addi r16, r0, 0
	add r3, r11, r0
	add r6, r16, r0
	jal r31, luaL_optlstring
	add r17, r1, r0
	addi r4, r0, 1
	addi r5, r0, 6
	add r3, r11, r0
	jal r31, luaL_checktype
	addi r4, r0, 5
	add r3, r11, r0
	jal r31, lua_settop
	lui r4, %hi(generic_reader)
	addi r4, r4, %lo(generic_reader)
	add r3, r11, r0
	add r5, r16, r0
	add r6, r17, r0
	add r7, r15, r0
	jal r31, lua_load
	beq r1, r13, .LBB8_2
.LBB8_7:
	add r3, r11, r0
	jal r31, lua_pushnil
	addi r4, r0, -2
	addi r5, r0, 1
	add r3, r11, r0
	jal r31, lua_rotate
	addi r12, r0, 2
	jal r0, .LBB8_5
.Lfunc_end8:
	.size	luaB_load, .Lfunc_end8-luaB_load
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_next
	.type	luaB_next,@function
luaB_next:                              # @luaB_next
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
	add r12, r3, r0
	addi r11, r0, 1
	addi r5, r0, 5
	add r4, r11, r0
	jal r31, luaL_checktype
	addi r13, r0, 2
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_settop
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_next
	addi r3, r0, 0
	bne r1, r3, .LBB9_2
.LBB9_1:
	add r3, r12, r0
	jal r31, lua_pushnil
	add r13, r11, r0
.LBB9_2:
	add r1, r13, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end9:
	.size	luaB_next, .Lfunc_end9-luaB_next
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_pairs
	.type	luaB_pairs,@function
luaB_pairs:                             # @luaB_pairs
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
	jal r31, luaL_checkany
	lui r5, %hi(.L.str.42)
	addi r5, r5, %lo(.L.str.42)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_getmetafield
	addi r3, r0, 0
	beq r1, r3, .LBB10_2
.LBB10_1:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushvalue
	lui r7, %hi(pairscont)
	addi r7, r7, %lo(pairscont)
	addi r5, r0, 3
	addi r6, r0, 0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_callk
	jal r0, .LBB10_3
.LBB10_2:
	lui r4, %hi(luaB_next)
	addi r4, r4, %lo(luaB_next)
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lua_pushcclosure
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_pushvalue
	add r3, r11, r0
	jal r31, lua_pushnil
.LBB10_3:
	addi r1, r0, 3
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end10:
	.size	luaB_pairs, .Lfunc_end10-luaB_pairs
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_pcall
	.type	luaB_pcall,@function
luaB_pcall:                             # @luaB_pcall
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
	jal r31, luaL_checkany
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushboolean
	add r3, r11, r0
	add r4, r12, r0
	add r5, r12, r0
	jal r31, lua_rotate
	add r3, r11, r0
	jal r31, lua_gettop
	addi r4, r1, -2
	lui r8, %hi(finishpcall)
	addi r8, r8, %lo(finishpcall)
	addi r5, r0, -1
	addi r6, r0, 0
	add r3, r11, r0
	add r7, r6, r0
	jal r31, lua_pcallk
	addi r5, r0, 2
	bgeu r1, r5, .LBB11_3
.LBB11_1:
	add r3, r11, r0
	jal r31, lua_gettop
.LBB11_2:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB11_3:
	addi r4, r0, 0
	add r3, r11, r0
	add r12, r5, r0
	jal r31, lua_pushboolean
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_pushvalue
	add r1, r12, r0
	jal r0, .LBB11_2
.Lfunc_end11:
	.size	luaB_pcall, .Lfunc_end11-luaB_pcall
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_print
	.type	luaB_print,@function
luaB_print:                             # @luaB_print
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
	stw fp+-40, r20
	stw fp+-44, lr
	add r11, r3, r0
	jal r31, lua_gettop
	add r12, r1, r0
	addi r1, r0, 1
	lui r19, %hi(stdout)
	addi r19, r19, %lo(stdout)
	blt r12, r1, .LBB12_5
.LBB12_1:
	addi r13, r0, 0
	addi r14, fp, -48
	addi r20, r0, 2
	lui r15, %hi(.L.str.43)
	addi r15, r15, %lo(.L.str.43)
	addi r16, r0, 1
	addi r17, r0, -2
	jal r0, .LBB12_3
.LBB12_2:
	ldw r5, r14+0
	ldw r6, r19+0
	add r3, r18, r0
	add r4, r16, r0
	jal r31, fwrite
	add r3, r11, r0
	add r4, r17, r0
	jal r31, lua_settop
	beq r12, r13, .LBB12_5
.LBB12_3:
	addi r13, r13, 1
	add r3, r11, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, luaL_tolstring
	add r18, r1, r0
	bltu r13, r20, .LBB12_2
.LBB12_4:
	ldw r6, r19+0
	add r3, r15, r0
	add r4, r16, r0
	add r5, r16, r0
	jal r31, fwrite
	jal r0, .LBB12_2
.LBB12_5:
	ldw r6, r19+0
	lui r3, %hi(.L.str.44)
	addi r3, r3, %lo(.L.str.44)
	addi r4, r0, 1
	add r5, r4, r0
	jal r31, fwrite
	ldw r3, r19+0
	jal r31, fflush
	addi r1, r0, 0
	ldw lr, fp+-44
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
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end12:
	.size	luaB_print, .Lfunc_end12-luaB_print
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_warn
	.type	luaB_warn,@function
luaB_warn:                              # @luaB_warn
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
	add r11, r3, r0
	jal r31, lua_gettop
	add r12, r1, r0
	addi r13, r0, 1
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_checklstring
	addi r15, r0, 2
	blt r12, r15, .LBB13_3
.LBB13_1:
	addi r14, r0, 0
.LBB13_2:
	addi r13, r13, 1
	add r3, r11, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, luaL_checklstring
	bne r12, r13, .LBB13_2
.LBB13_3:
	blt r12, r15, .LBB13_6
.LBB13_4:
	addi r13, r0, 1
	addi r14, r0, 0
	add r15, r13, r0
.LBB13_5:
	add r3, r11, r0
	add r4, r15, r0
	add r5, r14, r0
	jal r31, lua_tolstring
	add r3, r11, r0
	add r4, r1, r0
	add r5, r13, r0
	jal r31, lua_warning
	addi r15, r15, 1
	bne r12, r15, .LBB13_5
.LBB13_6:
	addi r13, r0, 0
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, lua_tolstring
	add r3, r11, r0
	add r4, r1, r0
	add r5, r13, r0
	jal r31, lua_warning
	add r1, r13, r0
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
.Lfunc_end13:
	.size	luaB_warn, .Lfunc_end13-luaB_warn
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_rawequal
	.type	luaB_rawequal,@function
luaB_rawequal:                          # @luaB_rawequal
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
	jal r31, luaL_checkany
	addi r13, r0, 2
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_checkany
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, lua_rawequal
	add r3, r11, r0
	add r4, r1, r0
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
.Lfunc_end14:
	.size	luaB_rawequal, .Lfunc_end14-luaB_rawequal
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_rawlen
	.type	luaB_rawlen,@function
luaB_rawlen:                            # @luaB_rawlen
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
	jal r31, lua_type
	addi r3, r0, -2
	and r1, r1, r3
	addi r3, r0, 4
	bne r1, r3, .LBB15_2
.LBB15_1:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rawlen
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushinteger
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB15_2:
	lui r5, %hi(.L.str.45)
	addi r5, r5, %lo(.L.str.45)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_typeerror
	jal r0, .LBB15_1
.Lfunc_end15:
	.size	luaB_rawlen, .Lfunc_end15-luaB_rawlen
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_rawget
	.type	luaB_rawget,@function
luaB_rawget:                            # @luaB_rawget
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
	addi r5, r0, 5
	add r4, r12, r0
	jal r31, luaL_checktype
	addi r13, r0, 2
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_checkany
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rawget
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end16:
	.size	luaB_rawget, .Lfunc_end16-luaB_rawget
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_rawset
	.type	luaB_rawset,@function
luaB_rawset:                            # @luaB_rawset
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
	addi r5, r0, 5
	add r4, r12, r0
	jal r31, luaL_checktype
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_checkany
	addi r13, r0, 3
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_checkany
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rawset
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end17:
	.size	luaB_rawset, .Lfunc_end17-luaB_rawset
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_select
	.type	luaB_select,@function
luaB_select:                            # @luaB_select
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
	jal r31, lua_gettop
	add r12, r1, r0
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_type
	addi r3, r0, 4
	bne r1, r3, .LBB18_3
.LBB18_1:
	addi r13, r0, 1
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_tolstring
	ldbu r1, r1+0
	addi r3, r0, 35
	bne r1, r3, .LBB18_3
.LBB18_2:
	addi r4, r12, -1
	add r3, r11, r0
	jal r31, lua_pushinteger
	jal r0, .LBB18_5
.LBB18_3:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_checkinteger
	addi r3, r0, 0
	slt r4, r1, r3
	add r5, r1, r12
	xor r6, r1, r12
	slt r1, r1, r12
	sub r1, r3, r1
	and r1, r6, r1
	xor r1, r12, r1
	xor r5, r5, r1
	sub r4, r3, r4
	and r4, r5, r4
	xor r13, r1, r4
	ble r13, r3, .LBB18_6
.LBB18_4:
	sub r13, r12, r13
.LBB18_5:
	add r1, r13, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB18_6:
	lui r5, %hi(.L.str.46)
	addi r5, r5, %lo(.L.str.46)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB18_4
.Lfunc_end18:
	.size	luaB_select, .Lfunc_end18-luaB_select
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_setmetatable
	.type	luaB_setmetatable,@function
luaB_setmetatable:                      # @luaB_setmetatable
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
	addi r4, r0, 2
	jal r31, lua_type
	add r12, r1, r0
	addi r4, r0, 1
	addi r13, r0, 5
	add r3, r11, r0
	add r5, r13, r0
	jal r31, luaL_checktype
	addi r14, r0, 0
	beq r12, r14, .LBB19_2
.LBB19_1:
	bne r12, r13, .LBB19_5
.LBB19_2:
	lui r5, %hi(.L.str.37)
	addi r5, r5, %lo(.L.str.37)
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_getmetafield
	bne r1, r14, .LBB19_6
.LBB19_3:
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_setmetatable
.LBB19_4:
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
.LBB19_5:
	lui r5, %hi(.L.str.47)
	addi r5, r5, %lo(.L.str.47)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_typeerror
	jal r0, .LBB19_2
.LBB19_6:
	lui r4, %hi(.L.str.48)
	addi r4, r4, %lo(.L.str.48)
	add r3, r11, r0
	jal r31, luaL_error
	add r12, r1, r0
	jal r0, .LBB19_4
.Lfunc_end19:
	.size	luaB_setmetatable, .Lfunc_end19-luaB_setmetatable
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_tonumber
	.type	luaB_tonumber,@function
luaB_tonumber:                          # @luaB_tonumber
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
	add r11, r3, r0
	addi r4, r0, 2
	jal r31, lua_type
	addi r12, r0, 0
	ble r1, r12, .LBB20_9
.LBB20_1:
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_checkinteger
	add r15, r1, r0
	addi r14, r0, 1
	addi r5, r0, 4
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaL_checktype
	addi r13, fp, -48
	add r3, r11, r0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, lua_tolstring
	add r14, r1, r0
	addi r1, r15, -2
	addi r3, r0, 35
	bgeu r1, r3, .LBB20_19
.LBB20_2:
	lui r4, %hi(.L.str.50)
	addi r4, r4, %lo(.L.str.50)
	add r3, r14, r0
	jal r31, strspn
	add r1, r14, r1
	ldbu r3, r1+0
	addi r4, r0, 45
	seq r17, r3, r4
	addi r4, r0, 43
	seq r3, r3, r4
	or  r3, r17, r3
	add r19, r1, r3
	ldbu r3, r19+0
	jal r31, isalnum
	beq r1, r12, .LBB20_8
.LBB20_3:
	addi r12, r0, 0
	add r18, r12, r0
	jal r0, .LBB20_6
.LBB20_4:
	jal r31, toupper
	addi r1, r1, -55
	bge r1, r15, .LBB20_8
.LBB20_5:
	mul r3, r18, r15
	add r18, r1, r3
	addi r16, r19, 1
	ldbu r3, r19+1
	jal r31, isalnum
	add r19, r16, r0
	beq r1, r12, .LBB20_11
.LBB20_6:
	ldbu r3, r19+0
	jal r31, isdigit
	ldbu r3, r19+0
	beq r1, r12, .LBB20_4
.LBB20_7:
	slli r1, r3, 24
	srai r1, r1, 24
	addi r1, r1, -48
	blt r1, r15, .LBB20_5
.LBB20_8:
	add r1, r12, r0
	jal r0, .LBB20_12
.LBB20_9:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_type
	addi r4, r0, 1
	addi r3, r0, 3
	bne r1, r3, .LBB20_14
.LBB20_10:
	add r3, r11, r0
	jal r31, lua_settop
	jal r0, .LBB20_18
.LBB20_11:
	lui r4, %hi(.L.str.50)
	addi r4, r4, %lo(.L.str.50)
	add r3, r16, r0
	jal r31, strspn
	add r1, r16, r1
	addi r3, r0, 0
	sub r4, r3, r18
	sub r3, r3, r17
	xor r4, r4, r18
	and r3, r4, r3
	xor r12, r18, r3
.LBB20_12:
	ldw r3, r13+0
	add r3, r14, r3
	bne r1, r3, .LBB20_17
.LBB20_13:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushinteger
	jal r0, .LBB20_18
.LBB20_14:
	addi r12, fp, -44
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	addi r3, r0, 0
	beq r1, r3, .LBB20_16
.LBB20_15:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_stringtonumber
	ldw r3, r12+0
	addi r3, r3, 1
	beq r1, r3, .LBB20_18
.LBB20_16:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_checkany
.LBB20_17:
	add r3, r11, r0
	jal r31, lua_pushnil
.LBB20_18:
	addi r1, r0, 1
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
.LBB20_19:
	lui r5, %hi(.L.str.49)
	addi r5, r5, %lo(.L.str.49)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB20_2
.Lfunc_end20:
	.size	luaB_tonumber, .Lfunc_end20-luaB_tonumber
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_tostring
	.type	luaB_tostring,@function
luaB_tostring:                          # @luaB_tostring
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
	jal r31, luaL_checkany
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_tolstring
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end21:
	.size	luaB_tostring, .Lfunc_end21-luaB_tostring
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_type
	.type	luaB_type,@function
luaB_type:                              # @luaB_type
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
	jal r31, lua_type
	addi r3, r0, -1
	beq r1, r3, .LBB22_2
.LBB22_1:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_typename
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushstring
	addi r1, r0, 1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB22_2:
	lui r5, %hi(.L.str.51)
	addi r5, r5, %lo(.L.str.51)
	addi r4, r0, 1
	add r3, r11, r0
	add r12, r1, r0
	jal r31, luaL_argerror
	add r1, r12, r0
	jal r0, .LBB22_1
.Lfunc_end22:
	.size	luaB_type, .Lfunc_end22-luaB_type
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_xpcall
	.type	luaB_xpcall,@function
luaB_xpcall:                            # @luaB_xpcall
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
	add r12, r3, r0
	jal r31, lua_gettop
	add r13, r1, r0
	addi r11, r0, 2
	addi r5, r0, 6
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaL_checktype
	addi r14, r0, 1
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_pushboolean
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_pushvalue
	addi r4, r0, 3
	add r3, r12, r0
	add r5, r11, r0
	jal r31, lua_rotate
	addi r4, r13, -2
	lui r8, %hi(finishpcall)
	addi r8, r8, %lo(finishpcall)
	addi r5, r0, -1
	add r3, r12, r0
	add r6, r11, r0
	add r7, r11, r0
	jal r31, lua_pcallk
	bgeu r1, r11, .LBB23_3
.LBB23_1:
	add r3, r12, r0
	jal r31, lua_gettop
	addi r11, r1, -2
.LBB23_2:
	add r1, r11, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB23_3:
	addi r4, r0, 0
	add r3, r12, r0
	jal r31, lua_pushboolean
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_pushvalue
	jal r0, .LBB23_2
.Lfunc_end23:
	.size	luaB_xpcall, .Lfunc_end23-luaB_xpcall
                                        # -- End function
	.p2align	2                               # -- Begin function dofilecont
	.type	dofilecont,@function
dofilecont:                             # @dofilecont
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	jal r31, lua_gettop
	addi r1, r1, -1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end24:
	.size	dofilecont, .Lfunc_end24-dofilecont
                                        # -- End function
	.p2align	2                               # -- Begin function ipairsaux
	.type	ipairsaux,@function
ipairsaux:                              # @ipairsaux
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
	addi r4, r0, 2
	jal r31, luaL_checkinteger
	addi r12, r1, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushinteger
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_geti
	addi r3, r0, 0
	seq r1, r1, r3
	sub r1, r3, r1
	andi r1, r1, 3
	xori r1, r1, 2
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end25:
	.size	ipairsaux, .Lfunc_end25-ipairsaux
                                        # -- End function
	.p2align	2                               # -- Begin function generic_reader
	.type	generic_reader,@function
generic_reader:                         # @generic_reader
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
	add r12, r3, r0
	lui r5, %hi(.L.str.40)
	addi r5, r5, %lo(.L.str.40)
	addi r4, r0, 2
	jal r31, luaL_checkstack
	addi r14, r0, 1
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_pushvalue
	addi r13, r0, 0
	add r3, r12, r0
	add r4, r13, r0
	add r5, r14, r0
	add r6, r13, r0
	add r7, r13, r0
	jal r31, lua_callk
	addi r4, r0, -1
	add r3, r12, r0
	jal r31, lua_type
	beq r1, r13, .LBB26_3
.LBB26_1:
	addi r4, r0, -1
	add r3, r12, r0
	jal r31, lua_isstring
	beq r1, r13, .LBB26_5
.LBB26_2:
	addi r4, r0, -1
	addi r13, r0, 5
	add r3, r12, r0
	add r5, r13, r0
	jal r31, lua_copy
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
	add r3, r12, r0
	add r4, r13, r0
	add r5, r11, r0
	jal r31, lua_tolstring
	add r13, r1, r0
	jal r0, .LBB26_4
.LBB26_3:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
	stw r11+0, r13
.LBB26_4:
	add r1, r13, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB26_5:
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	add r3, r12, r0
	jal r31, luaL_error
	jal r0, .LBB26_2
.Lfunc_end26:
	.size	generic_reader, .Lfunc_end26-generic_reader
                                        # -- End function
	.p2align	2                               # -- Begin function pairscont
	.type	pairscont,@function
pairscont:                              # @pairscont
# %bb.0:
	addi r1, r0, 3
	jalr r0, r31, 0
.Lfunc_end27:
	.size	pairscont, .Lfunc_end27-pairscont
                                        # -- End function
	.p2align	2                               # -- Begin function finishpcall
	.type	finishpcall,@function
finishpcall:                            # @finishpcall
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	addi r1, r0, 2
	bgeu r4, r1, .LBB28_3
.LBB28_1:
	add r11, r5, r0
	jal r31, lua_gettop
	sub r1, r1, r11
.LBB28_2:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB28_3:
	addi r4, r0, 0
	add r11, r3, r0
	add r12, r1, r0
	jal r31, lua_pushboolean
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_pushvalue
	add r1, r12, r0
	jal r0, .LBB28_2
.Lfunc_end28:
	.size	finishpcall, .Lfunc_end28-finishpcall
                                        # -- End function
	.type	base_funcs,@object              # @base_funcs
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
base_funcs:
	.word	.L.str.3
	.word	luaB_assert
	.word	.L.str.4
	.word	luaB_collectgarbage
	.word	.L.str.5
	.word	luaB_dofile
	.word	.L.str.6
	.word	luaB_error
	.word	.L.str.7
	.word	luaB_getmetatable
	.word	.L.str.8
	.word	luaB_ipairs
	.word	.L.str.9
	.word	luaB_loadfile
	.word	.L.str.10
	.word	luaB_load
	.word	.L.str.11
	.word	luaB_next
	.word	.L.str.12
	.word	luaB_pairs
	.word	.L.str.13
	.word	luaB_pcall
	.word	.L.str.14
	.word	luaB_print
	.word	.L.str.15
	.word	luaB_warn
	.word	.L.str.16
	.word	luaB_rawequal
	.word	.L.str.17
	.word	luaB_rawlen
	.word	.L.str.18
	.word	luaB_rawget
	.word	.L.str.19
	.word	luaB_rawset
	.word	.L.str.20
	.word	luaB_select
	.word	.L.str.21
	.word	luaB_setmetatable
	.word	.L.str.22
	.word	luaB_tonumber
	.word	.L.str.23
	.word	luaB_tostring
	.word	.L.str.24
	.word	luaB_type
	.word	.L.str.25
	.word	luaB_xpcall
	.word	.L.str
	.word	0
	.word	.L.str.2
	.word	0
	.zero	8
	.size	base_funcs, 208

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"_G"
	.size	.L.str, 3

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"Lua 5.4"
	.size	.L.str.1, 8

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"_VERSION"
	.size	.L.str.2, 9

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"assert"
	.size	.L.str.3, 7

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"collectgarbage"
	.size	.L.str.4, 15

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"dofile"
	.size	.L.str.5, 7

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"error"
	.size	.L.str.6, 6

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"getmetatable"
	.size	.L.str.7, 13

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"ipairs"
	.size	.L.str.8, 7

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"loadfile"
	.size	.L.str.9, 9

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"load"
	.size	.L.str.10, 5

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"next"
	.size	.L.str.11, 5

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"pairs"
	.size	.L.str.12, 6

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"pcall"
	.size	.L.str.13, 6

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"print"
	.size	.L.str.14, 6

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"warn"
	.size	.L.str.15, 5

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"rawequal"
	.size	.L.str.16, 9

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"rawlen"
	.size	.L.str.17, 7

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"rawget"
	.size	.L.str.18, 7

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"rawset"
	.size	.L.str.19, 7

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"select"
	.size	.L.str.20, 7

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"setmetatable"
	.size	.L.str.21, 13

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"tonumber"
	.size	.L.str.22, 9

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"tostring"
	.size	.L.str.23, 9

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"type"
	.size	.L.str.24, 5

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"xpcall"
	.size	.L.str.25, 7

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"assertion failed!"
	.size	.L.str.26, 18

	.type	luaB_collectgarbage.opts,@object # @luaB_collectgarbage.opts
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
luaB_collectgarbage.opts:
	.word	.L.str.27
	.word	.L.str.28
	.word	.L.str.29
	.word	.L.str.30
	.word	.L.str.31
	.word	.L.str.32
	.word	.L.str.33
	.word	.L.str.34
	.word	.L.str.35
	.word	.L.str.36
	.word	0
	.size	luaB_collectgarbage.opts, 44

	.type	.L.str.27,@object               # @.str.27
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.27:
	.asciz	"stop"
	.size	.L.str.27, 5

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"restart"
	.size	.L.str.28, 8

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"collect"
	.size	.L.str.29, 8

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"count"
	.size	.L.str.30, 6

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"step"
	.size	.L.str.31, 5

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"setpause"
	.size	.L.str.32, 9

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"setstepmul"
	.size	.L.str.33, 11

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"isrunning"
	.size	.L.str.34, 10

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"generational"
	.size	.L.str.35, 13

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"incremental"
	.size	.L.str.36, 12

	.type	luaB_collectgarbage.optsnum,@object # @luaB_collectgarbage.optsnum
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
luaB_collectgarbage.optsnum:
	.word	0                               # 0x0
	.word	1                               # 0x1
	.word	2                               # 0x2
	.word	3                               # 0x3
	.word	5                               # 0x5
	.word	6                               # 0x6
	.word	7                               # 0x7
	.word	9                               # 0x9
	.word	10                              # 0xa
	.word	11                              # 0xb
	.size	luaB_collectgarbage.optsnum, 40

	.type	.L.str.37,@object               # @.str.37
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.37:
	.asciz	"__metatable"
	.size	.L.str.37, 12

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"bt"
	.size	.L.str.38, 3

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"=(load)"
	.size	.L.str.39, 8

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"too many nested functions"
	.size	.L.str.40, 26

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"reader function must return a string"
	.size	.L.str.41, 37

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"__pairs"
	.size	.L.str.42, 8

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"\t"
	.size	.L.str.43, 2

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"\n"
	.size	.L.str.44, 2

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"table or string"
	.size	.L.str.45, 16

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"index out of range"
	.size	.L.str.46, 19

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"nil or table"
	.size	.L.str.47, 13

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"cannot change a protected metatable"
	.size	.L.str.48, 36

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"base out of range"
	.size	.L.str.49, 18

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	" \f\n\r\t\013"
	.size	.L.str.50, 7

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"value expected"
	.size	.L.str.51, 15

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
