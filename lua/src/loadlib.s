	.file	"loadlib.c"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaopen_package
.LCPI0_0:
	.quad	0x407f800000000000              # double 504
	.text
	.globl	luaopen_package
	.p2align	2
	.type	luaopen_package,@function
luaopen_package:                        # @luaopen_package
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
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	lui r1, 1048572
	addi r12, r1, 384
	add r4, r12, r0
	jal r31, luaL_getsubtable
	addi r15, r0, 0
	addi r13, r0, 1
	add r3, r11, r0
	add r4, r15, r0
	add r5, r13, r0
	jal r31, lua_createtable
	lui r4, %hi(gctm)
	addi r4, r4, %lo(gctm)
	add r3, r11, r0
	add r5, r15, r0
	jal r31, lua_pushcclosure
	lui r5, %hi(.L.str.13)
	addi r5, r5, %lo(.L.str.13)
	addi r14, r0, -2
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_setfield
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_setmetatable
	lui r1, %hi(.LCPI0_0)
	addi r1, r1, %lo(.LCPI0_0)
	ldw r6, r1+4
	ldw r5, r1+0
	addi r4, r0, 72
	add r3, r11, r0
	jal r31, luaL_checkversion_
	addi r5, r0, 7
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_createtable
	lui r4, %hi(pk_funcs)
	addi r4, r4, %lo(pk_funcs)
	add r3, r11, r0
	add r5, r15, r0
	jal r31, luaL_setfuncs
	addi r4, r0, 4
	add r3, r11, r0
	add r5, r15, r0
	jal r31, lua_createtable
	lui r16, %hi(createsearcherstable.searchers)
	addi r16, r16, %lo(createsearcherstable.searchers)
	addi r17, r0, 5
	add r15, r13, r0
.LBB0_1:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushvalue
	ldw r4, r16+0
	add r3, r11, r0
	add r5, r13, r0
	jal r31, lua_pushcclosure
	add r3, r11, r0
	add r4, r14, r0
	add r5, r15, r0
	jal r31, lua_rawseti
	addi r15, r15, 1
	addi r16, r16, 4
	bne r15, r17, .LBB0_1
.LBB0_2:
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r13, r0, -2
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	lui r6, %hi(.L.str.2)
	addi r6, r6, %lo(.L.str.2)
	add r3, r11, r0
	jal r31, setpath
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	lui r5, %hi(.L.str.4)
	addi r5, r5, %lo(.L.str.4)
	lui r6, %hi(.L.str.5)
	addi r6, r6, %lo(.L.str.5)
	add r3, r11, r0
	jal r31, setpath
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	add r3, r11, r0
	jal r31, lua_pushstring
	lui r5, %hi(.L.str.7)
	addi r5, r5, %lo(.L.str.7)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_getsubtable
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	lui r5, %hi(.L.str.10)
	addi r5, r5, %lo(.L.str.10)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_getsubtable
	lui r5, %hi(.L.str.11)
	addi r5, r5, %lo(.L.str.11)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	addi r5, r0, 2
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rawgeti
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_pushvalue
	lui r4, %hi(ll_funcs)
	addi r4, r4, %lo(ll_funcs)
	addi r12, r0, 1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_setfuncs
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_settop
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
.Lfunc_end0:
	.size	luaopen_package, .Lfunc_end0-luaopen_package
                                        # -- End function
	.p2align	2                               # -- Begin function setpath
	.type	setpath,@function
setpath:                                # @setpath
# %bb.0:
	addi sp, sp, -568
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 568
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, lr
	add r13, r6, r0
	add r15, r5, r0
	add r12, r4, r0
	add r11, r3, r0
	lui r4, %hi(.L.str.36)
	addi r4, r4, %lo(.L.str.36)
	lui r6, %hi(.L.str.37)
	addi r6, r6, %lo(.L.str.37)
	jal r31, lua_pushfstring
	add r3, r1, r0
	jal r31, getenv
	add r14, r1, r0
	addi r16, r0, 0
	beq r1, r16, .LBB1_2
.LBB1_1:
	bne r14, r16, .LBB1_3
	jal r0, .LBB1_4
.LBB1_2:
	add r3, r15, r0
	jal r31, getenv
	add r14, r1, r0
	beq r14, r16, .LBB1_4
.LBB1_3:
	lui r5, %hi(.L.str.39)
	addi r5, r5, %lo(.L.str.39)
	lui r1, 1048572
	addi r4, r1, 384
	add r3, r11, r0
	jal r31, lua_getfield
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_toboolean
	add r15, r1, r0
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	beq r15, r16, .LBB1_7
.LBB1_4:
	add r3, r11, r0
	add r4, r13, r0
.LBB1_5:
	jal r31, lua_pushstring
.LBB1_6:
	addi r4, r0, -3
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_setfield
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
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
	addi sp, sp, 568
	jalr r0, r31, 0
.LBB1_7:
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	add r3, r14, r0
	jal r31, strstr
	beq r1, r16, .LBB1_17
.LBB1_8:
	add r15, r1, r0
	add r3, r14, r0
	jal r31, strlen
	add r16, r1, r0
	addi r17, fp, -560
	add r3, r11, r0
	add r4, r17, r0
	jal r31, luaL_buffinit
	bgeu r14, r15, .LBB1_12
.LBB1_9:
	sub r5, r15, r14
	add r3, r17, r0
	add r4, r14, r0
	jal r31, luaL_addlstring
	ldw r1, r17+8
	ldw r3, r17+4
	bltu r1, r3, .LBB1_11
.LBB1_10:
	addi r3, fp, -560
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
.LBB1_11:
	ldw r1, r17+0
	ldw r3, r17+8
	addi r4, r3, 1
	stw r17+8, r4
	add r1, r1, r3
	addi r3, r0, 59
	stb r1+0, r3
.LBB1_12:
	add r3, r17, r0
	add r4, r13, r0
	jal r31, luaL_addstring
	add r1, r14, r16
	addi r13, r1, -2
	bgeu r15, r13, .LBB1_16
.LBB1_13:
	ldw r1, r17+8
	ldw r3, r17+4
	bltu r1, r3, .LBB1_15
.LBB1_14:
	addi r3, fp, -560
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
.LBB1_15:
	ldw r1, r17+0
	ldw r3, r17+8
	addi r4, r3, 1
	stw r17+8, r4
	add r1, r1, r3
	addi r3, r0, 59
	stb r1+0, r3
	addi r4, r15, 2
	sub r5, r13, r15
	add r3, r17, r0
	jal r31, luaL_addlstring
.LBB1_16:
	addi r3, fp, -560
	jal r31, luaL_pushresult
	jal r0, .LBB1_6
.LBB1_17:
	add r3, r11, r0
	add r4, r14, r0
	jal r0, .LBB1_5
.Lfunc_end1:
	.size	setpath, .Lfunc_end1-setpath
                                        # -- End function
	.p2align	2                               # -- Begin function gctm
	.type	gctm,@function
gctm:                                   # @gctm
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
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_len
	blt r1, r12, .LBB2_3
.LBB2_1:
	addi r13, r0, -1
	addi r14, r0, -2
.LBB2_2:
	add r15, r1, r0
	add r3, r11, r0
	add r4, r12, r0
	add r5, r1, r0
	jal r31, lua_rawgeti
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_touserdata
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_settop
	addi r1, r15, -1
	bgt r15, r12, .LBB2_2
.LBB2_3:
	addi r1, r0, 0
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
	.size	gctm, .Lfunc_end2-gctm
                                        # -- End function
	.p2align	2                               # -- Begin function ll_loadlib
	.type	ll_loadlib,@function
ll_loadlib:                             # @ll_loadlib
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
	addi r13, r0, 1
	addi r12, r0, 0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, luaL_checklstring
	add r14, r1, r0
	addi r4, r0, 2
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_checklstring
	add r3, r11, r0
	add r4, r14, r0
	add r5, r1, r0
	jal r31, lookforfunc
	bne r1, r12, .LBB3_2
.LBB3_1:
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
.LBB3_2:
	add r3, r11, r0
	add r14, r1, r0
	jal r31, lua_pushnil
	addi r4, r0, -2
	addi r13, r0, 1
	add r3, r11, r0
	add r5, r13, r0
	jal r31, lua_rotate
	seq r1, r14, r13
	lui r3, %hi(.L.str.18)
	addi r3, r3, %lo(.L.str.18)
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	xor r4, r4, r3
	sub r1, r12, r1
	and r1, r4, r1
	xor r4, r1, r3
	add r3, r11, r0
	jal r31, lua_pushstring
	addi r13, r0, 3
	jal r0, .LBB3_1
.Lfunc_end3:
	.size	ll_loadlib, .Lfunc_end3-ll_loadlib
                                        # -- End function
	.p2align	2                               # -- Begin function ll_searchpath
	.type	ll_searchpath,@function
ll_searchpath:                          # @ll_searchpath
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
	addi r13, r0, 1
	addi r14, r0, 0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, luaL_checklstring
	add r15, r1, r0
	addi r12, r0, 2
	add r3, r11, r0
	add r4, r12, r0
	add r5, r14, r0
	jal r31, luaL_checklstring
	add r16, r1, r0
	lui r5, %hi(.L.str.20)
	addi r5, r5, %lo(.L.str.20)
	addi r4, r0, 3
	add r3, r11, r0
	add r6, r14, r0
	jal r31, luaL_optlstring
	add r17, r1, r0
	lui r5, %hi(.L.str.21)
	addi r5, r5, %lo(.L.str.21)
	addi r4, r0, 4
	add r3, r11, r0
	add r6, r14, r0
	jal r31, luaL_optlstring
	add r3, r11, r0
	add r4, r15, r0
	add r5, r16, r0
	add r6, r17, r0
	add r7, r1, r0
	jal r31, searchpath
	bne r1, r14, .LBB4_2
.LBB4_1:
	add r3, r11, r0
	jal r31, lua_pushnil
	addi r4, r0, -2
	addi r5, r0, 1
	add r3, r11, r0
	jal r31, lua_rotate
	add r13, r12, r0
.LBB4_2:
	add r1, r13, r0
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
.Lfunc_end4:
	.size	ll_searchpath, .Lfunc_end4-ll_searchpath
                                        # -- End function
	.p2align	2                               # -- Begin function lookforfunc
	.type	lookforfunc,@function
lookforfunc:                            # @lookforfunc
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
	add r12, r5, r0
	add r13, r4, r0
	add r11, r3, r0
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	lui r1, 1048572
	addi r4, r1, 384
	jal r31, lua_getfield
	addi r14, r0, -1
	add r3, r11, r0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, lua_getfield
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_touserdata
	add r13, r1, r0
	addi r4, r0, -3
	add r3, r11, r0
	jal r31, lua_settop
	addi r1, r0, 0
	beq r13, r1, .LBB5_3
.LBB5_1:
	ldbu r3, r12+0
	addi r4, r0, 42
	bne r3, r4, .LBB5_4
.LBB5_2:
	addi r4, r0, 1
	add r3, r11, r0
	add r11, r1, r0
	jal r31, lua_pushboolean
	add r1, r11, r0
	jal r0, .LBB5_5
.LBB5_3:
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r3, r11, r0
	jal r31, lua_pushstring
	addi r1, r0, 1
	jal r0, .LBB5_5
.LBB5_4:
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r3, r11, r0
	jal r31, lua_pushstring
	addi r1, r0, 2
.LBB5_5:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end5:
	.size	lookforfunc, .Lfunc_end5-lookforfunc
                                        # -- End function
	.p2align	2                               # -- Begin function searchpath
	.type	searchpath,@function
searchpath:                             # @searchpath
# %bb.0:
	addi sp, sp, -1096
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 1096
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, lr
	add r12, r5, r0
	add r13, r4, r0
	add r11, r3, r0
	ldb r4, r6+0
	addi r16, r0, 0
	beq r4, r16, .LBB6_3
.LBB6_1:
	add r14, r7, r0
	add r15, r6, r0
	add r3, r13, r0
	jal r31, strchr
	beq r1, r16, .LBB6_3
.LBB6_2:
	add r3, r11, r0
	add r4, r13, r0
	add r5, r15, r0
	add r6, r14, r0
	jal r31, luaL_gsub
	add r13, r1, r0
.LBB6_3:
	addi r14, fp, -1088
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaL_buffinit
	lui r5, %hi(.L.str.22)
	addi r5, r5, %lo(.L.str.22)
	add r3, r14, r0
	add r4, r12, r0
	add r6, r13, r0
	jal r31, luaL_addgsub
	ldw r1, r14+8
	ldw r3, r14+4
	bltu r1, r3, .LBB6_5
.LBB6_4:
	addi r3, fp, -1088
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
.LBB6_5:
	ldw r1, r14+0
	ldw r3, r14+8
	addi r4, r3, 1
	stw r14+8, r4
	add r1, r1, r3
	stb r1+0, r16
	ldw r17, r14+0
	ldw r1, r14+8
	add r1, r17, r1
	addi r15, r1, -1
	lui r12, %hi(.L.str.24)
	addi r12, r12, %lo(.L.str.24)
	addi r13, r0, 59
.LBB6_6:
	beq r17, r15, .LBB6_9
.LBB6_7:
	ldbu r1, r17+0
	beq r1, r16, .LBB6_10
.LBB6_8:
	add r14, r17, r0
	jal r0, .LBB6_11
.LBB6_9:
	add r14, r16, r0
	bne r14, r16, .LBB6_12
	jal r0, .LBB6_14
.LBB6_10:
	stb r17+0, r13
	addi r14, r17, 1
.LBB6_11:
	add r3, r14, r0
	add r4, r13, r0
	jal r31, strchr
	seq r3, r1, r16
	xor r4, r15, r1
	sub r3, r16, r3
	and r3, r4, r3
	xor r17, r1, r3
	stb r17+0, r16
	beq r14, r16, .LBB6_14
.LBB6_12:
	add r3, r14, r0
	add r4, r12, r0
	jal r31, fopen
	beq r1, r16, .LBB6_6
.LBB6_13:
	add r3, r1, r0
	jal r31, fclose
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushstring
	jal r0, .LBB6_15
.LBB6_14:
	addi r3, fp, -1088
	jal r31, luaL_pushresult
	addi r4, r0, -1
	addi r12, r0, 0
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	add r13, r1, r0
	addi r14, fp, -560
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaL_buffinit
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	add r3, r14, r0
	jal r31, luaL_addstring
	lui r5, %hi(.L.str.23)
	addi r5, r5, %lo(.L.str.23)
	lui r6, %hi(.L.str.26)
	addi r6, r6, %lo(.L.str.26)
	add r3, r14, r0
	add r4, r13, r0
	jal r31, luaL_addgsub
	lui r4, %hi(.L.str.27)
	addi r4, r4, %lo(.L.str.27)
	add r3, r14, r0
	jal r31, luaL_addstring
	add r3, r14, r0
	jal r31, luaL_pushresult
	add r1, r12, r0
.LBB6_15:
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
	addi sp, sp, 1096
	jalr r0, r31, 0
.Lfunc_end6:
	.size	searchpath, .Lfunc_end6-searchpath
                                        # -- End function
	.p2align	2                               # -- Begin function searcher_preload
	.type	searcher_preload,@function
searcher_preload:                       # @searcher_preload
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
	addi r11, r0, 1
	addi r14, r0, 0
	add r4, r11, r0
	add r5, r14, r0
	jal r31, luaL_checklstring
	add r13, r1, r0
	lui r5, %hi(.L.str.10)
	addi r5, r5, %lo(.L.str.10)
	lui r1, 1048572
	addi r4, r1, 384
	add r3, r12, r0
	jal r31, lua_getfield
	addi r4, r0, -1
	add r3, r12, r0
	add r5, r13, r0
	jal r31, lua_getfield
	beq r1, r14, .LBB7_2
.LBB7_1:
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	add r3, r12, r0
	jal r31, lua_pushstring
	addi r11, r0, 2
	jal r0, .LBB7_3
.LBB7_2:
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	add r3, r12, r0
	add r5, r13, r0
	jal r31, lua_pushfstring
.LBB7_3:
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
.Lfunc_end7:
	.size	searcher_preload, .Lfunc_end7-searcher_preload
                                        # -- End function
	.p2align	2                               # -- Begin function searcher_Lua
	.type	searcher_Lua,@function
searcher_Lua:                           # @searcher_Lua
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
	addi r13, r0, 1
	addi r12, r0, 0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, luaL_checklstring
	add r14, r1, r0
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	lui r1, 1048572
	addi r4, r1, 383
	add r3, r11, r0
	jal r31, lua_getfield
	addi r4, r0, -1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	beq r1, r12, .LBB8_5
.LBB8_1:
	lui r6, %hi(.L.str.20)
	addi r6, r6, %lo(.L.str.20)
	lui r7, %hi(.L.str.21)
	addi r7, r7, %lo(.L.str.21)
	add r3, r11, r0
	add r4, r14, r0
	add r5, r1, r0
	jal r31, searchpath
	beq r1, r12, .LBB8_4
.LBB8_2:
	add r14, r1, r0
	add r3, r11, r0
	add r4, r1, r0
	add r5, r12, r0
	jal r31, luaL_loadfilex
	bne r1, r12, .LBB8_6
.LBB8_3:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushstring
	addi r13, r0, 2
.LBB8_4:
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
.LBB8_5:
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	add r3, r11, r0
	add r15, r1, r0
	jal r31, luaL_error
	add r1, r15, r0
	jal r0, .LBB8_1
.LBB8_6:
	addi r4, r0, 1
	addi r12, r0, 0
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	add r13, r1, r0
	addi r4, r0, -1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	add r3, r11, r0
	add r5, r13, r0
	add r6, r14, r0
	add r7, r1, r0
	jal r31, luaL_error
	add r13, r1, r0
	jal r0, .LBB8_4
.Lfunc_end8:
	.size	searcher_Lua, .Lfunc_end8-searcher_Lua
                                        # -- End function
	.p2align	2                               # -- Begin function searcher_C
	.type	searcher_C,@function
searcher_C:                             # @searcher_C
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
	addi r13, r0, 1
	addi r12, r0, 0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, luaL_checklstring
	add r14, r1, r0
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	lui r1, 1048572
	addi r4, r1, 383
	add r3, r11, r0
	jal r31, lua_getfield
	addi r4, r0, -1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	beq r1, r12, .LBB9_5
.LBB9_1:
	lui r6, %hi(.L.str.20)
	addi r6, r6, %lo(.L.str.20)
	lui r7, %hi(.L.str.21)
	addi r7, r7, %lo(.L.str.21)
	add r3, r11, r0
	add r4, r14, r0
	add r5, r1, r0
	jal r31, searchpath
	beq r1, r12, .LBB9_4
.LBB9_2:
	add r15, r1, r0
	add r3, r11, r0
	add r4, r1, r0
	add r5, r14, r0
	jal r31, loadfunc
	bne r1, r12, .LBB9_6
.LBB9_3:
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_pushstring
	addi r13, r0, 2
.LBB9_4:
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
.LBB9_5:
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	add r3, r11, r0
	add r15, r1, r0
	jal r31, luaL_error
	add r1, r15, r0
	jal r0, .LBB9_1
.LBB9_6:
	addi r4, r0, 1
	addi r12, r0, 0
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	add r13, r1, r0
	addi r4, r0, -1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	add r3, r11, r0
	add r5, r13, r0
	add r6, r15, r0
	add r7, r1, r0
	jal r31, luaL_error
	add r13, r1, r0
	jal r0, .LBB9_4
.Lfunc_end9:
	.size	searcher_C, .Lfunc_end9-searcher_C
                                        # -- End function
	.p2align	2                               # -- Begin function searcher_Croot
	.type	searcher_Croot,@function
searcher_Croot:                         # @searcher_Croot
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
	addi r12, r0, 1
	addi r14, r0, 0
	add r4, r12, r0
	add r5, r14, r0
	jal r31, luaL_checklstring
	add r13, r1, r0
	addi r4, r0, 46
	add r3, r1, r0
	jal r31, strchr
	beq r1, r14, .LBB10_9
.LBB10_1:
	sub r5, r1, r13
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_pushlstring
	addi r16, r0, -1
	addi r14, r0, 0
	add r3, r11, r0
	add r4, r16, r0
	add r5, r14, r0
	jal r31, lua_tolstring
	add r15, r1, r0
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	lui r1, 1048572
	addi r4, r1, 383
	add r3, r11, r0
	jal r31, lua_getfield
	add r3, r11, r0
	add r4, r16, r0
	add r5, r14, r0
	jal r31, lua_tolstring
	beq r1, r14, .LBB10_10
.LBB10_2:
	lui r6, %hi(.L.str.20)
	addi r6, r6, %lo(.L.str.20)
	lui r7, %hi(.L.str.21)
	addi r7, r7, %lo(.L.str.21)
	add r3, r11, r0
	add r4, r15, r0
	add r5, r1, r0
	jal r31, searchpath
	beq r1, r14, .LBB10_6
.LBB10_3:
	add r15, r1, r0
	add r3, r11, r0
	add r4, r1, r0
	add r5, r13, r0
	jal r31, loadfunc
	beq r1, r14, .LBB10_7
.LBB10_4:
	addi r3, r0, 2
	bne r1, r3, .LBB10_8
.LBB10_5:
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	add r3, r11, r0
	add r5, r13, r0
	add r6, r15, r0
	jal r31, lua_pushfstring
.LBB10_6:
	add r14, r12, r0
	jal r0, .LBB10_9
.LBB10_7:
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_pushstring
	addi r14, r0, 2
	jal r0, .LBB10_9
.LBB10_8:
	addi r4, r0, 1
	addi r12, r0, 0
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	add r13, r1, r0
	addi r4, r0, -1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	add r3, r11, r0
	add r5, r13, r0
	add r6, r15, r0
	add r7, r1, r0
	jal r31, luaL_error
	add r14, r1, r0
.LBB10_9:
	add r1, r14, r0
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
.LBB10_10:
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	add r3, r11, r0
	add r16, r1, r0
	jal r31, luaL_error
	add r1, r16, r0
	jal r0, .LBB10_2
.Lfunc_end10:
	.size	searcher_Croot, .Lfunc_end10-searcher_Croot
                                        # -- End function
	.p2align	2                               # -- Begin function loadfunc
	.type	loadfunc,@function
loadfunc:                               # @loadfunc
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
	add r1, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	lui r5, %hi(.L.str.20)
	addi r5, r5, %lo(.L.str.20)
	lui r6, %hi(.L.str.32)
	addi r6, r6, %lo(.L.str.32)
	add r4, r1, r0
	jal r31, luaL_gsub
	add r13, r1, r0
	addi r4, r0, 45
	add r3, r1, r0
	jal r31, strchr
	addi r15, r0, 0
	beq r1, r15, .LBB11_3
.LBB11_1:
	add r14, r1, r0
	sub r5, r1, r13
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_pushlstring
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
	add r3, r12, r0
	add r5, r1, r0
	jal r31, lua_pushfstring
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, lookforfunc
	addi r3, r0, 2
	bne r1, r3, .LBB11_4
.LBB11_2:
	seq r1, r1, r3
	addi r3, r14, 1
	xor r3, r3, r13
	sub r1, r15, r1
	and r1, r3, r1
	xor r13, r13, r1
.LBB11_3:
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
	add r3, r12, r0
	add r5, r13, r0
	jal r31, lua_pushfstring
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, lookforfunc
.LBB11_4:
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
.Lfunc_end11:
	.size	loadfunc, .Lfunc_end11-loadfunc
                                        # -- End function
	.p2align	2                               # -- Begin function ll_require
	.type	ll_require,@function
ll_require:                             # @ll_require
# %bb.0:
	addi sp, sp, -600
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 600
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
	add r11, r3, r0
	addi r14, r0, 1
	addi r13, r0, 0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, luaL_checklstring
	add r12, r1, r0
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_settop
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	lui r15, 1048572
	addi r4, r15, 384
	add r3, r11, r0
	jal r31, lua_getfield
	addi r4, r0, 2
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_getfield
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_toboolean
	bne r1, r13, .LBB12_15
.LBB12_1:
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r15, 383
	add r3, r11, r0
	jal r31, lua_getfield
	addi r3, r0, 5
	bne r1, r3, .LBB12_16
.LBB12_2:
	addi r14, fp, -588
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaL_buffinit
	addi r15, r0, 1
	lui r16, %hi(.L.str.42)
	addi r16, r16, %lo(.L.str.42)
	addi r17, r0, 3
	addi r18, r0, -2
	addi r19, r0, -1
	lui r20, %hi(.L.str.43)
	addi r20, r20, %lo(.L.str.43)
	addi r21, r0, 2
	addi r24, r0, 6
	addi r22, r0, -3
	add r23, r15, r0
	jal r0, .LBB12_4
.LBB12_3:
	add r4, r18, r0
	jal r31, lua_settop
	add r3, r14, r0
	jal r31, luaL_addvalue
.LBB12_4:
	add r3, r14, r0
	add r4, r16, r0
	jal r31, luaL_addstring
	add r3, r11, r0
	add r4, r17, r0
	add r5, r23, r0
	jal r31, lua_rawgeti
	beq r1, r13, .LBB12_8
.LBB12_5:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushstring
	add r3, r11, r0
	add r4, r15, r0
	add r5, r21, r0
	add r6, r13, r0
	add r7, r13, r0
	jal r31, lua_callk
	add r3, r11, r0
	add r4, r18, r0
	jal r31, lua_type
	beq r1, r24, .LBB12_9
.LBB12_6:
	add r3, r11, r0
	add r4, r18, r0
	jal r31, lua_isstring
	addi r23, r23, 1
	add r3, r11, r0
	bne r1, r13, .LBB12_3
.LBB12_7:
	add r4, r22, r0
	jal r31, lua_settop
	ldw r1, r14+8
	addi r1, r1, -2
	stw r14+8, r1
	jal r0, .LBB12_4
.LBB12_8:
	add r3, r11, r0
	add r4, r18, r0
	jal r31, lua_settop
	ldw r1, r14+8
	addi r1, r1, -2
	stw r14+8, r1
	add r3, r14, r0
	jal r31, luaL_pushresult
	add r3, r11, r0
	add r4, r19, r0
	add r5, r13, r0
	jal r31, lua_tolstring
	add r3, r11, r0
	add r4, r20, r0
	add r5, r12, r0
	add r6, r1, r0
	jal r31, luaL_error
	jal r0, .LBB12_5
.LBB12_9:
	addi r4, r0, -2
	addi r14, r0, 1
	add r3, r11, r0
	add r5, r14, r0
	jal r31, lua_rotate
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushvalue
	addi r4, r0, -3
	add r3, r11, r0
	jal r31, lua_pushvalue
	addi r4, r0, 2
	add r3, r11, r0
	add r5, r14, r0
	add r6, r13, r0
	add r7, r13, r0
	jal r31, lua_callk
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_type
	beq r1, r13, .LBB12_11
.LBB12_10:
	addi r4, r0, 2
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_setfield
	jal r0, .LBB12_12
.LBB12_11:
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
.LBB12_12:
	addi r14, r0, 2
	add r3, r11, r0
	add r4, r14, r0
	add r5, r12, r0
	jal r31, lua_getfield
	bne r1, r13, .LBB12_14
.LBB12_13:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_pushboolean
	addi r4, r0, -1
	addi r5, r0, -2
	add r3, r11, r0
	jal r31, lua_copy
	addi r4, r0, 2
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_setfield
.LBB12_14:
	addi r4, r0, -2
	addi r5, r0, 1
	add r3, r11, r0
	jal r31, lua_rotate
.LBB12_15:
	add r1, r14, r0
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
	addi sp, sp, 600
	jalr r0, r31, 0
.LBB12_16:
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB12_2
.Lfunc_end12:
	.size	ll_require, .Lfunc_end12-ll_require
                                        # -- End function
	.type	pk_funcs,@object                # @pk_funcs
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
pk_funcs:
	.word	.L.str.14
	.word	ll_loadlib
	.word	.L.str.15
	.word	ll_searchpath
	.word	.L.str.11
	.word	0
	.word	.L.str.3
	.word	0
	.word	.L.str
	.word	0
	.word	.L.str.16
	.word	0
	.word	.L.str.9
	.word	0
	.zero	8
	.size	pk_funcs, 64

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"path"
	.size	.L.str, 5

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"LUA_PATH"
	.size	.L.str.1, 9

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"/usr/local/share/lua/5.4/?.lua;/usr/local/share/lua/5.4/?/init.lua;/usr/local/lib/lua/5.4/?.lua;/usr/local/lib/lua/5.4/?/init.lua;./?.lua;./?/init.lua"
	.size	.L.str.2, 151

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"cpath"
	.size	.L.str.3, 6

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"LUA_CPATH"
	.size	.L.str.4, 10

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"/usr/local/lib/lua/5.4/?.so;/usr/local/lib/lua/5.4/loadall.so;./?.so"
	.size	.L.str.5, 69

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"/\n;\n?\n!\n-\n"
	.size	.L.str.6, 11

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"config"
	.size	.L.str.7, 7

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"_LOADED"
	.size	.L.str.8, 8

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"loaded"
	.size	.L.str.9, 7

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"_PRELOAD"
	.size	.L.str.10, 9

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"preload"
	.size	.L.str.11, 8

	.type	ll_funcs,@object                # @ll_funcs
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
ll_funcs:
	.word	.L.str.40
	.word	ll_require
	.zero	8
	.size	ll_funcs, 16

	.type	.L.str.12,@object               # @.str.12
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.12:
	.asciz	"_CLIBS"
	.size	.L.str.12, 7

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"__gc"
	.size	.L.str.13, 5

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"loadlib"
	.size	.L.str.14, 8

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"searchpath"
	.size	.L.str.15, 11

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"searchers"
	.size	.L.str.16, 10

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"absent"
	.size	.L.str.17, 7

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"init"
	.size	.L.str.18, 5

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"dynamic libraries not enabled; check your Lua installation"
	.size	.L.str.19, 59

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"."
	.size	.L.str.20, 2

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"/"
	.size	.L.str.21, 2

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"?"
	.size	.L.str.22, 2

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	";"
	.size	.L.str.23, 2

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"r"
	.size	.L.str.24, 2

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"no file '"
	.size	.L.str.25, 10

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"'\n\tno file '"
	.size	.L.str.26, 13

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"'"
	.size	.L.str.27, 2

	.type	createsearcherstable.searchers,@object # @createsearcherstable.searchers
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
createsearcherstable.searchers:
	.word	searcher_preload
	.word	searcher_Lua
	.word	searcher_C
	.word	searcher_Croot
	.word	0
	.size	createsearcherstable.searchers, 20

	.type	.L.str.28,@object               # @.str.28
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.28:
	.asciz	"no field package.preload['%s']"
	.size	.L.str.28, 31

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	":preload:"
	.size	.L.str.29, 10

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"'package.%s' must be a string"
	.size	.L.str.30, 30

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"error loading module '%s' from file '%s':\n\t%s"
	.size	.L.str.31, 46

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"_"
	.size	.L.str.32, 2

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"luaopen_%s"
	.size	.L.str.34, 11

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"no module '%s' in file '%s'"
	.size	.L.str.35, 28

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"%s%s"
	.size	.L.str.36, 5

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"_5_4"
	.size	.L.str.37, 5

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	";;"
	.size	.L.str.38, 3

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"LUA_NOENV"
	.size	.L.str.39, 10

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"require"
	.size	.L.str.40, 8

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"'package.searchers' must be a table"
	.size	.L.str.41, 36

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"\n\t"
	.size	.L.str.42, 3

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"module '%s' not found:%s"
	.size	.L.str.43, 25

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
