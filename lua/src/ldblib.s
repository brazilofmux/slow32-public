	.file	"ldblib.c"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaopen_debug
.LCPI0_0:
	.quad	0x407f800000000000              # double 504
	.text
	.globl	luaopen_debug
	.p2align	2
	.type	luaopen_debug,@function
luaopen_debug:                          # @luaopen_debug
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
	lui r1, %hi(.LCPI0_0)
	addi r1, r1, %lo(.LCPI0_0)
	ldw r6, r1+4
	ldw r5, r1+0
	addi r4, r0, 72
	jal r31, luaL_checkversion_
	addi r12, r0, 0
	addi r5, r0, 17
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_createtable
	lui r4, %hi(dblib)
	addi r4, r4, %lo(dblib)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_setfuncs
	addi r1, r0, 1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaopen_debug, .Lfunc_end0-luaopen_debug
                                        # -- End function
	.p2align	2                               # -- Begin function db_debug
	.type	db_debug,@function
db_debug:                               # @db_debug
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
	stw fp+-56, lr
	add r11, r3, r0
	lui r22, %hi(stderr)
	addi r22, r22, %lo(stderr)
	ldw r3, r22+0
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	lui r5, %hi(.L.str.18)
	addi r5, r5, %lo(.L.str.18)
	jal r31, fprintf
	ldw r3, r22+0
	jal r31, fflush
	lui r23, %hi(stdin)
	addi r23, r23, %lo(stdin)
	ldw r5, r23+0
	addi r3, fp, -306
	addi r4, r0, 250
	jal r31, fgets
	addi r12, r0, 0
	beq r1, r12, .LBB1_7
.LBB1_1:
	lui r13, %hi(.L.str.19)
	addi r13, r13, %lo(.L.str.19)
	addi r14, fp, -306
	lui r15, %hi(.L.str.20)
	addi r15, r15, %lo(.L.str.20)
	lui r16, %hi(.L.str.17)
	addi r16, r16, %lo(.L.str.17)
	lui r17, %hi(.L.str.18)
	addi r17, r17, %lo(.L.str.18)
	addi r18, r0, 250
	addi r19, r0, -1
	lui r20, %hi(.L.str.21)
	addi r20, r20, %lo(.L.str.21)
	jal r0, .LBB1_4
.LBB1_2:
	ldw r21, r22+0
	add r3, r11, r0
	add r4, r19, r0
	add r5, r12, r0
	jal r31, luaL_tolstring
	add r3, r21, r0
	add r4, r20, r0
	add r5, r1, r0
	jal r31, fprintf
	ldw r3, r22+0
	jal r31, fflush
.LBB1_3:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_settop
	ldw r3, r22+0
	add r4, r16, r0
	add r5, r17, r0
	jal r31, fprintf
	ldw r3, r22+0
	jal r31, fflush
	ldw r5, r23+0
	add r3, r14, r0
	add r4, r18, r0
	jal r31, fgets
	beq r1, r12, .LBB1_7
.LBB1_4:
	add r3, r14, r0
	add r4, r13, r0
	jal r31, strcmp
	beq r1, r12, .LBB1_7
.LBB1_5:
	add r3, r14, r0
	jal r31, strlen
	add r3, r11, r0
	add r4, r14, r0
	add r5, r1, r0
	add r6, r15, r0
	add r7, r12, r0
	jal r31, luaL_loadbufferx
	bne r1, r12, .LBB1_2
.LBB1_6:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r12, r0
	add r6, r12, r0
	add r7, r12, r0
	add r8, r12, r0
	jal r31, lua_pcallk
	bne r1, r12, .LBB1_2
	jal r0, .LBB1_3
.LBB1_7:
	addi r1, r0, 0
	ldw lr, fp+-56
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
.Lfunc_end1:
	.size	db_debug, .Lfunc_end1-db_debug
                                        # -- End function
	.p2align	2                               # -- Begin function db_getuservalue
	.type	db_getuservalue,@function
db_getuservalue:                        # @db_getuservalue
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
	addi r12, r0, 2
	addi r13, r0, 1
	add r4, r12, r0
	add r5, r13, r0
	jal r31, luaL_optinteger
	add r14, r1, r0
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_type
	addi r3, r0, 7
	bne r1, r3, .LBB2_3
.LBB2_1:
	addi r13, r0, 1
	add r3, r11, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, lua_getiuservalue
	addi r3, r0, -1
	beq r1, r3, .LBB2_4
.LBB2_2:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_pushboolean
	add r13, r12, r0
	jal r0, .LBB2_4
.LBB2_3:
	add r3, r11, r0
	jal r31, lua_pushnil
.LBB2_4:
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
.Lfunc_end2:
	.size	db_getuservalue, .Lfunc_end2-db_getuservalue
                                        # -- End function
	.p2align	2                               # -- Begin function db_gethook
	.type	db_gethook,@function
db_gethook:                             # @db_gethook
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
	addi r13, r0, 1
	add r4, r13, r0
	jal r31, lua_type
	addi r3, r0, 8
	add r12, r11, r0
	bne r1, r3, .LBB3_2
.LBB3_1:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_tothread
	add r12, r1, r0
.LBB3_2:
	add r3, r12, r0
	jal r31, lua_gethookmask
	add r14, r1, r0
	add r3, r12, r0
	jal r31, lua_gethook
	addi r16, r0, 0
	beq r1, r16, .LBB3_5
.LBB3_3:
	lui r3, %hi(hookf)
	addi r3, r3, %lo(hookf)
	beq r1, r3, .LBB3_6
.LBB3_4:
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	add r3, r11, r0
	jal r31, lua_pushstring
	jal r0, .LBB3_9
.LBB3_5:
	add r3, r11, r0
	jal r31, lua_pushnil
	addi r1, r0, 1
	jal r0, .LBB3_18
.LBB3_6:
	lui r5, %hi(.L.str.23)
	addi r5, r5, %lo(.L.str.23)
	lui r1, 1048572
	addi r4, r1, 384
	add r3, r11, r0
	jal r31, lua_getfield
	beq r11, r12, .LBB3_8
.LBB3_7:
	addi r4, r0, 1
	add r3, r12, r0
	jal r31, lua_checkstack
	addi r3, r0, 0
	beq r1, r3, .LBB3_19
.LBB3_8:
	add r3, r12, r0
	jal r31, lua_pushthread
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_xmove
	addi r15, r0, -2
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_rawget
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_rotate
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_settop
.LBB3_9:
	andi r1, r14, 1
	add r3, r16, r0
	beq r1, r16, .LBB3_11
.LBB3_10:
	addi r1, r0, 99
	addi r3, fp, -36
	stb r3+0, r1
	add r3, r13, r0
.LBB3_11:
	andi r1, r14, 2
	bne r1, r16, .LBB3_13
.LBB3_12:
	add r1, r3, r0
	jal r0, .LBB3_14
.LBB3_13:
	addi r1, r3, 1
	addi r4, fp, -36
	or  r3, r4, r3
	addi r4, r0, 114
	stb r3+0, r4
.LBB3_14:
	andi r4, r14, 4
	addi r3, r0, 0
	bne r4, r3, .LBB3_16
.LBB3_15:
	add r5, r1, r0
	jal r0, .LBB3_17
.LBB3_16:
	addi r5, r1, 1
	addi r4, fp, -36
	or  r1, r4, r1
	addi r4, r0, 108
	stb r1+0, r4
.LBB3_17:
	addi r4, fp, -36
	add r1, r4, r5
	stb r1+0, r3
	add r3, r11, r0
	jal r31, lua_pushstring
	add r3, r12, r0
	jal r31, lua_gethookcount
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushinteger
	addi r1, r0, 3
.LBB3_18:
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
.LBB3_19:
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB3_8
.Lfunc_end3:
	.size	db_gethook, .Lfunc_end3-db_gethook
                                        # -- End function
	.p2align	2                               # -- Begin function db_getinfo
	.type	db_getinfo,@function
db_getinfo:                             # @db_getinfo
# %bb.0:
	addi sp, sp, -152
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 152
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, lr
	add r12, r3, r0
	addi r11, r0, 1
	add r4, r11, r0
	jal r31, lua_type
	addi r3, r0, 8
	bne r1, r3, .LBB4_2
.LBB4_1:
	addi r16, r0, 1
	add r3, r12, r0
	add r4, r16, r0
	jal r31, lua_tothread
	add r13, r1, r0
	jal r0, .LBB4_3
.LBB4_2:
	addi r16, r0, 0
	add r13, r12, r0
.LBB4_3:
	addi r17, r16, 2
	lui r5, %hi(.L.str.30)
	addi r5, r5, %lo(.L.str.30)
	addi r14, r0, 0
	add r3, r12, r0
	add r4, r17, r0
	add r6, r14, r0
	jal r31, luaL_optlstring
	add r15, r1, r0
	beq r12, r13, .LBB4_5
.LBB4_4:
	addi r4, r0, 3
	add r3, r13, r0
	jal r31, lua_checkstack
	beq r1, r14, .LBB4_36
.LBB4_5:
	ldbu r1, r15+0
	addi r3, r0, 62
	beq r1, r3, .LBB4_35
.LBB4_6:
	addi r16, r16, 1
	add r3, r12, r0
	add r4, r16, r0
	jal r31, lua_type
	addi r3, r0, 6
	bne r1, r3, .LBB4_8
.LBB4_7:
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
	add r3, r12, r0
	add r5, r15, r0
	jal r31, lua_pushfstring
	add r15, r1, r0
	add r3, r12, r0
	add r4, r16, r0
	jal r31, lua_pushvalue
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_xmove
	jal r0, .LBB4_9
.LBB4_8:
	add r3, r12, r0
	add r4, r16, r0
	jal r31, luaL_checkinteger
	addi r5, fp, -140
	add r3, r13, r0
	add r4, r1, r0
	jal r31, lua_getstack
	beq r1, r14, .LBB4_26
.LBB4_9:
	addi r16, fp, -140
	add r3, r13, r0
	add r4, r15, r0
	add r5, r16, r0
	jal r31, lua_getinfo
	beq r1, r14, .LBB4_25
.LBB4_10:
	add r3, r12, r0
	add r4, r14, r0
	add r5, r14, r0
	jal r31, lua_createtable
	addi r4, r0, 83
	add r3, r15, r0
	jal r31, strchr
	beq r1, r14, .LBB4_12
.LBB4_11:
	ldw r4, r16+16
	ldw r5, r16+20
	add r3, r12, r0
	jal r31, lua_pushlstring
	lui r5, %hi(.L.str.34)
	addi r5, r5, %lo(.L.str.34)
	addi r17, r0, -2
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
	addi r4, r16, 44
	add r3, r12, r0
	jal r31, lua_pushstring
	lui r5, %hi(.L.str.35)
	addi r5, r5, %lo(.L.str.35)
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
	ldw r4, r16+28
	add r3, r12, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.36)
	addi r5, r5, %lo(.L.str.36)
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
	ldw r4, r16+32
	add r3, r12, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.37)
	addi r5, r5, %lo(.L.str.37)
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
	ldw r4, r16+12
	add r3, r12, r0
	jal r31, lua_pushstring
	lui r5, %hi(.L.str.38)
	addi r5, r5, %lo(.L.str.38)
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
.LBB4_12:
	addi r4, r0, 108
	add r3, r15, r0
	jal r31, strchr
	beq r1, r14, .LBB4_14
.LBB4_13:
	ldw r4, r16+24
	add r3, r12, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.39)
	addi r5, r5, %lo(.L.str.39)
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_setfield
.LBB4_14:
	addi r4, r0, 117
	add r3, r15, r0
	jal r31, strchr
	beq r1, r14, .LBB4_16
.LBB4_15:
	ldbu r4, r16+36
	add r3, r12, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.40)
	addi r5, r5, %lo(.L.str.40)
	addi r17, r0, -2
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
	ldbu r4, r16+37
	add r3, r12, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.41)
	addi r5, r5, %lo(.L.str.41)
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
	ldb r4, r16+38
	add r3, r12, r0
	jal r31, lua_pushboolean
	lui r5, %hi(.L.str.42)
	addi r5, r5, %lo(.L.str.42)
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
.LBB4_16:
	addi r4, r0, 110
	add r3, r15, r0
	jal r31, strchr
	beq r1, r14, .LBB4_18
.LBB4_17:
	ldw r4, r16+4
	add r3, r12, r0
	jal r31, lua_pushstring
	lui r5, %hi(.L.str.43)
	addi r5, r5, %lo(.L.str.43)
	addi r17, r0, -2
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
	ldw r4, r16+8
	add r3, r12, r0
	jal r31, lua_pushstring
	lui r5, %hi(.L.str.44)
	addi r5, r5, %lo(.L.str.44)
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
.LBB4_18:
	addi r4, r0, 114
	add r3, r15, r0
	jal r31, strchr
	beq r1, r14, .LBB4_20
.LBB4_19:
	ldhu r4, r16+40
	add r3, r12, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.45)
	addi r5, r5, %lo(.L.str.45)
	addi r17, r0, -2
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
	ldhu r4, r16+42
	add r3, r12, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.46)
	addi r5, r5, %lo(.L.str.46)
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_setfield
.LBB4_20:
	addi r4, r0, 116
	add r3, r15, r0
	jal r31, strchr
	beq r1, r14, .LBB4_22
.LBB4_21:
	ldb r4, r16+39
	add r3, r12, r0
	jal r31, lua_pushboolean
	lui r5, %hi(.L.str.47)
	addi r5, r5, %lo(.L.str.47)
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_setfield
.LBB4_22:
	addi r4, r0, 76
	add r3, r15, r0
	jal r31, strchr
	beq r1, r14, .LBB4_29
.LBB4_23:
	beq r12, r13, .LBB4_27
.LBB4_24:
	addi r5, r0, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, lua_xmove
	jal r0, .LBB4_28
.LBB4_25:
	lui r5, %hi(.L.str.33)
	addi r5, r5, %lo(.L.str.33)
	add r3, r12, r0
	add r4, r17, r0
	jal r31, luaL_argerror
	add r11, r1, r0
	jal r0, .LBB4_34
.LBB4_26:
	add r3, r12, r0
	jal r31, lua_pushnil
	jal r0, .LBB4_34
.LBB4_27:
	addi r4, r0, -2
	addi r5, r0, 1
	add r3, r12, r0
	jal r31, lua_rotate
.LBB4_28:
	lui r5, %hi(.L.str.48)
	addi r5, r5, %lo(.L.str.48)
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_setfield
.LBB4_29:
	addi r4, r0, 102
	add r3, r15, r0
	jal r31, strchr
	beq r1, r14, .LBB4_34
.LBB4_30:
	beq r12, r13, .LBB4_32
.LBB4_31:
	addi r5, r0, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, lua_xmove
	jal r0, .LBB4_33
.LBB4_32:
	addi r4, r0, -2
	addi r5, r0, 1
	add r3, r12, r0
	jal r31, lua_rotate
.LBB4_33:
	lui r5, %hi(.L.str.49)
	addi r5, r5, %lo(.L.str.49)
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_setfield
.LBB4_34:
	add r1, r11, r0
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
	addi sp, sp, 152
	jalr r0, r31, 0
.LBB4_35:
	lui r5, %hi(.L.str.31)
	addi r5, r5, %lo(.L.str.31)
	add r3, r12, r0
	add r4, r17, r0
	jal r31, luaL_argerror
	jal r0, .LBB4_6
.LBB4_36:
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	add r3, r12, r0
	jal r31, luaL_error
	jal r0, .LBB4_5
.Lfunc_end4:
	.size	db_getinfo, .Lfunc_end4-db_getinfo
                                        # -- End function
	.p2align	2                               # -- Begin function db_getlocal
	.type	db_getlocal,@function
db_getlocal:                            # @db_getlocal
# %bb.0:
	addi sp, sp, -152
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 152
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, lr
	add r11, r3, r0
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, lua_type
	addi r3, r0, 8
	bne r1, r3, .LBB5_2
.LBB5_1:
	addi r15, r0, 1
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_tothread
	add r13, r1, r0
	jal r0, .LBB5_3
.LBB5_2:
	addi r15, r0, 0
	add r13, r11, r0
.LBB5_3:
	addi r4, r15, 2
	add r3, r11, r0
	jal r31, luaL_checkinteger
	add r14, r1, r0
	addi r15, r15, 1
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_type
	addi r3, r0, 6
	bne r1, r3, .LBB5_5
.LBB5_4:
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_pushvalue
	addi r4, r0, 0
	add r3, r11, r0
	add r5, r14, r0
	jal r31, lua_getlocal
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushstring
	addi r12, r0, 1
	jal r0, .LBB5_11
.LBB5_5:
	add r3, r11, r0
	add r4, r15, r0
	jal r31, luaL_checkinteger
	addi r5, fp, -136
	add r3, r13, r0
	add r4, r1, r0
	jal r31, lua_getstack
	addi r16, r0, 0
	beq r1, r16, .LBB5_12
.LBB5_6:
	beq r11, r13, .LBB5_8
.LBB5_7:
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, lua_checkstack
	beq r1, r16, .LBB5_13
.LBB5_8:
	addi r4, fp, -136
	add r3, r13, r0
	add r5, r14, r0
	jal r31, lua_getlocal
	beq r1, r16, .LBB5_10
.LBB5_9:
	add r14, r1, r0
	addi r12, r0, 1
	add r3, r13, r0
	add r4, r11, r0
	add r5, r12, r0
	jal r31, lua_xmove
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushstring
	addi r4, r0, -2
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_rotate
	addi r12, r0, 2
	jal r0, .LBB5_11
.LBB5_10:
	add r3, r11, r0
	jal r31, lua_pushnil
.LBB5_11:
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
	addi sp, sp, 152
	jalr r0, r31, 0
.LBB5_12:
	lui r5, %hi(.L.str.50)
	addi r5, r5, %lo(.L.str.50)
	add r3, r11, r0
	add r4, r15, r0
	jal r31, luaL_argerror
	add r12, r1, r0
	jal r0, .LBB5_11
.LBB5_13:
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB5_8
.Lfunc_end5:
	.size	db_getlocal, .Lfunc_end5-db_getlocal
                                        # -- End function
	.p2align	2                               # -- Begin function db_getregistry
	.type	db_getregistry,@function
db_getregistry:                         # @db_getregistry
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r1, 1048572
	addi r4, r1, 384
	jal r31, lua_pushvalue
	addi r1, r0, 1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	db_getregistry, .Lfunc_end6-db_getregistry
                                        # -- End function
	.p2align	2                               # -- Begin function db_getmetatable
	.type	db_getmetatable,@function
db_getmetatable:                        # @db_getmetatable
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
	bne r1, r3, .LBB7_2
.LBB7_1:
	add r3, r11, r0
	jal r31, lua_pushnil
.LBB7_2:
	addi r1, r0, 1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end7:
	.size	db_getmetatable, .Lfunc_end7-db_getmetatable
                                        # -- End function
	.p2align	2                               # -- Begin function db_getupvalue
	.type	db_getupvalue,@function
db_getupvalue:                          # @db_getupvalue
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
	addi r11, r0, 2
	add r4, r11, r0
	jal r31, luaL_checkinteger
	add r13, r1, r0
	addi r14, r0, 1
	addi r5, r0, 6
	add r3, r12, r0
	add r4, r14, r0
	jal r31, luaL_checktype
	add r3, r12, r0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, lua_getupvalue
	addi r3, r0, 0
	beq r1, r3, .LBB8_2
.LBB8_1:
	add r3, r12, r0
	add r4, r1, r0
	jal r31, lua_pushstring
	addi r4, r0, -2
	addi r5, r0, 1
	add r3, r12, r0
	jal r31, lua_rotate
	add r3, r11, r0
.LBB8_2:
	add r1, r3, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end8:
	.size	db_getupvalue, .Lfunc_end8-db_getupvalue
                                        # -- End function
	.p2align	2                               # -- Begin function db_upvaluejoin
	.type	db_upvaluejoin,@function
db_upvaluejoin:                         # @db_upvaluejoin
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
	addi r4, r0, 2
	jal r31, luaL_checkinteger
	add r12, r1, r0
	addi r13, r0, 1
	addi r5, r0, 6
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_checktype
	add r3, r11, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, lua_upvalueid
	addi r15, r0, 0
	beq r1, r15, .LBB9_5
.LBB9_1:
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, luaL_checkinteger
	add r13, r1, r0
	addi r14, r0, 3
	addi r5, r0, 6
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaL_checktype
	add r3, r11, r0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, lua_upvalueid
	beq r1, r15, .LBB9_6
.LBB9_2:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_iscfunction
	bne r1, r15, .LBB9_7
.LBB9_3:
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, lua_iscfunction
	bne r1, r15, .LBB9_8
.LBB9_4:
	addi r4, r0, 1
	addi r6, r0, 3
	add r3, r11, r0
	add r5, r12, r0
	add r7, r13, r0
	jal r31, lua_upvaluejoin
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
.LBB9_5:
	lui r5, %hi(.L.str.52)
	addi r5, r5, %lo(.L.str.52)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB9_1
.LBB9_6:
	lui r5, %hi(.L.str.52)
	addi r5, r5, %lo(.L.str.52)
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB9_2
.LBB9_7:
	lui r5, %hi(.L.str.51)
	addi r5, r5, %lo(.L.str.51)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB9_3
.LBB9_8:
	lui r5, %hi(.L.str.51)
	addi r5, r5, %lo(.L.str.51)
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB9_4
.Lfunc_end9:
	.size	db_upvaluejoin, .Lfunc_end9-db_upvaluejoin
                                        # -- End function
	.p2align	2                               # -- Begin function db_upvalueid
	.type	db_upvalueid,@function
db_upvalueid:                           # @db_upvalueid
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
	jal r31, luaL_checkinteger
	add r12, r1, r0
	addi r13, r0, 1
	addi r5, r0, 6
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_checktype
	add r3, r11, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, lua_upvalueid
	addi r3, r0, 0
	beq r1, r3, .LBB10_2
.LBB10_1:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushlightuserdata
	jal r0, .LBB10_3
.LBB10_2:
	add r3, r11, r0
	jal r31, lua_pushnil
.LBB10_3:
	addi r1, r0, 1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end10:
	.size	db_upvalueid, .Lfunc_end10-db_upvalueid
                                        # -- End function
	.p2align	2                               # -- Begin function db_setuservalue
	.type	db_setuservalue,@function
db_setuservalue:                        # @db_setuservalue
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
	addi r4, r0, 3
	addi r12, r0, 1
	add r5, r12, r0
	jal r31, luaL_optinteger
	add r13, r1, r0
	addi r5, r0, 7
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_checktype
	addi r14, r0, 2
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaL_checkany
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, lua_setiuservalue
	addi r3, r0, 0
	bne r1, r3, .LBB11_2
.LBB11_1:
	add r3, r11, r0
	jal r31, lua_pushnil
.LBB11_2:
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
.Lfunc_end11:
	.size	db_setuservalue, .Lfunc_end11-db_setuservalue
                                        # -- End function
	.p2align	2                               # -- Begin function db_sethook
	.type	db_sethook,@function
db_sethook:                             # @db_sethook
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
	stw fp+-36, lr
	add r11, r3, r0
	addi r13, r0, 1
	add r4, r13, r0
	jal r31, lua_type
	addi r15, r0, 0
	addi r3, r0, 8
	add r14, r15, r0
	add r12, r11, r0
	bne r1, r3, .LBB12_2
.LBB12_1:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_tothread
	add r12, r1, r0
	add r14, r13, r0
.LBB12_2:
	addi r13, r14, 1
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_type
	ble r1, r15, .LBB12_4
.LBB12_3:
	addi r4, r14, 2
	addi r15, r0, 0
	add r3, r11, r0
	add r5, r15, r0
	jal r31, luaL_checklstring
	add r16, r1, r0
	addi r5, r0, 6
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_checktype
	addi r4, r14, 3
	add r3, r11, r0
	add r5, r15, r0
	jal r31, luaL_optinteger
	add r14, r1, r0
	addi r4, r0, 99
	add r3, r16, r0
	jal r31, strchr
	sne r17, r1, r15
	addi r4, r0, 114
	add r3, r16, r0
	jal r31, strchr
	seq r1, r1, r15
	addi r3, r17, 2
	xor r4, r17, r3
	sub r1, r15, r1
	and r1, r4, r1
	xor r17, r3, r1
	addi r4, r0, 108
	add r3, r16, r0
	jal r31, strchr
	seq r1, r1, r15
	addi r3, r17, 4
	xor r4, r17, r3
	sub r1, r15, r1
	and r1, r4, r1
	xor r1, r3, r1
	sgt r3, r14, r15
	addi r4, r1, 8
	xor r4, r4, r1
	sub r3, r15, r3
	and r3, r4, r3
	xor r16, r1, r3
	lui r15, %hi(hookf)
	addi r15, r15, %lo(hookf)
	jal r0, .LBB12_5
.LBB12_4:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_settop
	addi r15, r0, 0
	add r14, r15, r0
	add r16, r15, r0
.LBB12_5:
	lui r5, %hi(.L.str.23)
	addi r5, r5, %lo(.L.str.23)
	lui r1, 1048572
	addi r4, r1, 384
	add r3, r11, r0
	jal r31, luaL_getsubtable
	addi r18, r0, 0
	bne r1, r18, .LBB12_7
.LBB12_6:
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	add r3, r11, r0
	jal r31, lua_pushstring
	lui r5, %hi(.L.str.54)
	addi r5, r5, %lo(.L.str.54)
	addi r17, r0, -2
	add r3, r11, r0
	add r4, r17, r0
	jal r31, lua_setfield
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_pushvalue
	add r3, r11, r0
	add r4, r17, r0
	jal r31, lua_setmetatable
.LBB12_7:
	beq r11, r12, .LBB12_9
.LBB12_8:
	addi r4, r0, 1
	add r3, r12, r0
	jal r31, lua_checkstack
	beq r1, r18, .LBB12_10
.LBB12_9:
	add r3, r12, r0
	jal r31, lua_pushthread
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_xmove
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_pushvalue
	addi r4, r0, -3
	add r3, r11, r0
	jal r31, lua_rawset
	add r3, r12, r0
	add r4, r15, r0
	add r5, r16, r0
	add r6, r14, r0
	jal r31, lua_sethook
	addi r1, r0, 0
	ldw lr, fp+-36
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
.LBB12_10:
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB12_9
.Lfunc_end12:
	.size	db_sethook, .Lfunc_end12-db_sethook
                                        # -- End function
	.p2align	2                               # -- Begin function db_setlocal
	.type	db_setlocal,@function
db_setlocal:                            # @db_setlocal
# %bb.0:
	addi sp, sp, -152
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 152
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
	add r4, r13, r0
	jal r31, lua_type
	addi r16, r0, 0
	addi r3, r0, 8
	add r17, r16, r0
	add r12, r11, r0
	bne r1, r3, .LBB13_2
.LBB13_1:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_tothread
	add r12, r1, r0
	add r17, r13, r0
.LBB13_2:
	addi r14, r17, 1
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaL_checkinteger
	add r15, r1, r0
	addi r4, r17, 2
	add r3, r11, r0
	jal r31, luaL_checkinteger
	add r13, r1, r0
	addi r5, fp, -140
	add r3, r12, r0
	add r4, r15, r0
	jal r31, lua_getstack
	beq r1, r16, .LBB13_9
.LBB13_3:
	addi r14, r17, 3
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaL_checkany
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_settop
	beq r11, r12, .LBB13_5
.LBB13_4:
	addi r4, r0, 1
	add r3, r12, r0
	jal r31, lua_checkstack
	addi r3, r0, 0
	beq r1, r3, .LBB13_10
.LBB13_5:
	addi r14, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r14, r0
	jal r31, lua_xmove
	addi r4, fp, -140
	add r3, r12, r0
	add r5, r13, r0
	jal r31, lua_setlocal
	add r13, r1, r0
	addi r1, r0, 0
	bne r13, r1, .LBB13_7
.LBB13_6:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
.LBB13_7:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_pushstring
.LBB13_8:
	add r1, r14, r0
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
	addi sp, sp, 152
	jalr r0, r31, 0
.LBB13_9:
	lui r5, %hi(.L.str.50)
	addi r5, r5, %lo(.L.str.50)
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaL_argerror
	add r14, r1, r0
	jal r0, .LBB13_8
.LBB13_10:
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB13_5
.Lfunc_end13:
	.size	db_setlocal, .Lfunc_end13-db_setlocal
                                        # -- End function
	.p2align	2                               # -- Begin function db_setmetatable
	.type	db_setmetatable,@function
db_setmetatable:                        # @db_setmetatable
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
	jal r31, lua_type
	addi r3, r0, 0
	beq r1, r3, .LBB14_2
.LBB14_1:
	addi r3, r0, 5
	bne r1, r3, .LBB14_3
.LBB14_2:
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_settop
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_setmetatable
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB14_3:
	lui r5, %hi(.L.str.55)
	addi r5, r5, %lo(.L.str.55)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_typeerror
	jal r0, .LBB14_2
.Lfunc_end14:
	.size	db_setmetatable, .Lfunc_end14-db_setmetatable
                                        # -- End function
	.p2align	2                               # -- Begin function db_setupvalue
	.type	db_setupvalue,@function
db_setupvalue:                          # @db_setupvalue
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
	addi r4, r0, 3
	jal r31, luaL_checkany
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_checkinteger
	add r13, r1, r0
	addi r12, r0, 1
	addi r5, r0, 6
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_checktype
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, lua_setupvalue
	addi r3, r0, 0
	beq r1, r3, .LBB15_2
.LBB15_1:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushstring
	addi r4, r0, -1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_rotate
	add r3, r12, r0
.LBB15_2:
	add r1, r3, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end15:
	.size	db_setupvalue, .Lfunc_end15-db_setupvalue
                                        # -- End function
	.p2align	2                               # -- Begin function db_traceback
	.type	db_traceback,@function
db_traceback:                           # @db_traceback
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
	add r4, r12, r0
	jal r31, lua_type
	addi r14, r0, 0
	addi r3, r0, 8
	add r17, r14, r0
	add r13, r11, r0
	bne r1, r3, .LBB16_2
.LBB16_1:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_tothread
	add r13, r1, r0
	add r17, r12, r0
.LBB16_2:
	addi r16, r17, 1
	add r3, r11, r0
	add r4, r16, r0
	add r5, r14, r0
	jal r31, lua_tolstring
	add r15, r1, r0
	beq r1, r14, .LBB16_4
.LBB16_3:
	addi r4, r17, 2
	seq r5, r11, r13
	add r3, r11, r0
	jal r31, luaL_optinteger
	add r3, r11, r0
	add r4, r13, r0
	add r5, r15, r0
	add r6, r1, r0
	jal r31, luaL_traceback
	jal r0, .LBB16_6
.LBB16_4:
	add r3, r11, r0
	add r4, r16, r0
	jal r31, lua_type
	blt r1, r12, .LBB16_3
.LBB16_5:
	add r3, r11, r0
	add r4, r16, r0
	jal r31, lua_pushvalue
.LBB16_6:
	addi r1, r0, 1
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
.Lfunc_end16:
	.size	db_traceback, .Lfunc_end16-db_traceback
                                        # -- End function
	.p2align	2                               # -- Begin function db_setcstacklimit
	.type	db_setcstacklimit,@function
db_setcstacklimit:                      # @db_setcstacklimit
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
	jal r31, luaL_checkinteger
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_setcstacklimit
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
.Lfunc_end17:
	.size	db_setcstacklimit, .Lfunc_end17-db_setcstacklimit
                                        # -- End function
	.p2align	2                               # -- Begin function hookf
	.type	hookf,@function
hookf:                                  # @hookf
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r12, r4, r0
	add r11, r3, r0
	lui r5, %hi(.L.str.23)
	addi r5, r5, %lo(.L.str.23)
	lui r1, 1048572
	addi r4, r1, 384
	jal r31, lua_getfield
	add r3, r11, r0
	jal r31, lua_pushthread
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_rawget
	addi r3, r0, 6
	bne r1, r3, .LBB18_5
.LBB18_1:
	ldw r1, r12+0
	slli r1, r1, 2
	lui r3, %hi(hookf.hooknames)
	addi r3, r3, %lo(hookf.hooknames)
	add r1, r1, r3
	ldw r4, r1+0
	add r3, r11, r0
	jal r31, lua_pushstring
	ldw r4, r12+24
	add r3, r11, r0
	addi r1, r0, 0
	blt r4, r1, .LBB18_3
.LBB18_2:
	jal r31, lua_pushinteger
	jal r0, .LBB18_4
.LBB18_3:
	jal r31, lua_pushnil
.LBB18_4:
	addi r4, r0, 2
	addi r5, r0, 0
	add r3, r11, r0
	add r6, r5, r0
	add r7, r5, r0
	jal r31, lua_callk
.LBB18_5:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end18:
	.size	hookf, .Lfunc_end18-hookf
                                        # -- End function
	.type	dblib,@object                   # @dblib
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
dblib:
	.word	.L.str
	.word	db_debug
	.word	.L.str.1
	.word	db_getuservalue
	.word	.L.str.2
	.word	db_gethook
	.word	.L.str.3
	.word	db_getinfo
	.word	.L.str.4
	.word	db_getlocal
	.word	.L.str.5
	.word	db_getregistry
	.word	.L.str.6
	.word	db_getmetatable
	.word	.L.str.7
	.word	db_getupvalue
	.word	.L.str.8
	.word	db_upvaluejoin
	.word	.L.str.9
	.word	db_upvalueid
	.word	.L.str.10
	.word	db_setuservalue
	.word	.L.str.11
	.word	db_sethook
	.word	.L.str.12
	.word	db_setlocal
	.word	.L.str.13
	.word	db_setmetatable
	.word	.L.str.14
	.word	db_setupvalue
	.word	.L.str.15
	.word	db_traceback
	.word	.L.str.16
	.word	db_setcstacklimit
	.zero	8
	.size	dblib, 144

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"debug"
	.size	.L.str, 6

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"getuservalue"
	.size	.L.str.1, 13

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"gethook"
	.size	.L.str.2, 8

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"getinfo"
	.size	.L.str.3, 8

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"getlocal"
	.size	.L.str.4, 9

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"getregistry"
	.size	.L.str.5, 12

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"getmetatable"
	.size	.L.str.6, 13

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"getupvalue"
	.size	.L.str.7, 11

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"upvaluejoin"
	.size	.L.str.8, 12

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"upvalueid"
	.size	.L.str.9, 10

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"setuservalue"
	.size	.L.str.10, 13

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"sethook"
	.size	.L.str.11, 8

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"setlocal"
	.size	.L.str.12, 9

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"setmetatable"
	.size	.L.str.13, 13

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"setupvalue"
	.size	.L.str.14, 11

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"traceback"
	.size	.L.str.15, 10

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"setcstacklimit"
	.size	.L.str.16, 15

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"%s"
	.size	.L.str.17, 3

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"lua_debug> "
	.size	.L.str.18, 12

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"cont\n"
	.size	.L.str.19, 6

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"=(debug command)"
	.size	.L.str.20, 17

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"%s\n"
	.size	.L.str.21, 4

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"external hook"
	.size	.L.str.22, 14

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"_HOOKKEY"
	.size	.L.str.23, 9

	.type	hookf.hooknames,@object         # @hookf.hooknames
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
hookf.hooknames:
	.word	.L.str.24
	.word	.L.str.25
	.word	.L.str.26
	.word	.L.str.27
	.word	.L.str.28
	.size	hookf.hooknames, 20

	.type	.L.str.24,@object               # @.str.24
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.24:
	.asciz	"call"
	.size	.L.str.24, 5

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"return"
	.size	.L.str.25, 7

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"line"
	.size	.L.str.26, 5

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"count"
	.size	.L.str.27, 6

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"tail call"
	.size	.L.str.28, 10

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"stack overflow"
	.size	.L.str.29, 15

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"flnSrtu"
	.size	.L.str.30, 8

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"invalid option '>'"
	.size	.L.str.31, 19

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	">%s"
	.size	.L.str.32, 4

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"invalid option"
	.size	.L.str.33, 15

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"source"
	.size	.L.str.34, 7

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"short_src"
	.size	.L.str.35, 10

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"linedefined"
	.size	.L.str.36, 12

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"lastlinedefined"
	.size	.L.str.37, 16

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"what"
	.size	.L.str.38, 5

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"currentline"
	.size	.L.str.39, 12

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"nups"
	.size	.L.str.40, 5

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"nparams"
	.size	.L.str.41, 8

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"isvararg"
	.size	.L.str.42, 9

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"name"
	.size	.L.str.43, 5

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"namewhat"
	.size	.L.str.44, 9

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"ftransfer"
	.size	.L.str.45, 10

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"ntransfer"
	.size	.L.str.46, 10

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"istailcall"
	.size	.L.str.47, 11

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"activelines"
	.size	.L.str.48, 12

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"func"
	.size	.L.str.49, 5

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"level out of range"
	.size	.L.str.50, 19

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"Lua function expected"
	.size	.L.str.51, 22

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"invalid upvalue index"
	.size	.L.str.52, 22

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"k"
	.size	.L.str.53, 2

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"__mode"
	.size	.L.str.54, 7

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"nil or table"
	.size	.L.str.55, 13

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
