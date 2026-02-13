	.file	"ltablib.c"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaopen_table
.LCPI0_0:
	.quad	0x407f800000000000              # double 504
	.text
	.globl	luaopen_table
	.p2align	2
	.type	luaopen_table,@function
luaopen_table:                          # @luaopen_table
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
	addi r5, r0, 7
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_createtable
	lui r4, %hi(tab_funcs)
	addi r4, r4, %lo(tab_funcs)
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
	.size	luaopen_table, .Lfunc_end0-luaopen_table
                                        # -- End function
	.p2align	2                               # -- Begin function tconcat
	.type	tconcat,@function
tconcat:                                # @tconcat
# %bb.0:
	addi sp, sp, -584
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 584
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
	addi r12, r0, 1
	addi r5, r0, 5
	add r4, r12, r0
	jal r31, checktab
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_len
	add r15, r1, r0
	lui r5, %hi(.L.str.7)
	addi r5, r5, %lo(.L.str.7)
	addi r4, r0, 2
	addi r13, fp, -576
	add r3, r11, r0
	add r6, r13, r0
	jal r31, luaL_optlstring
	add r14, r1, r0
	addi r4, r0, 3
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_optinteger
	add r12, r1, r0
	addi r4, r0, 4
	add r3, r11, r0
	add r5, r15, r0
	jal r31, luaL_optinteger
	add r15, r1, r0
	addi r4, fp, -572
	add r3, r11, r0
	jal r31, luaL_buffinit
	addi r20, r0, 0
	bge r12, r15, .LBB1_6
.LBB1_1:
	addi r16, r0, 1
	addi r17, r0, -1
	lui r18, %hi(.L.str.11)
	addi r18, r18, %lo(.L.str.11)
	addi r19, fp, -572
.LBB1_2:
	add r3, r11, r0
	add r4, r16, r0
	add r5, r12, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r17, r0
	jal r31, lua_isstring
	beq r1, r20, .LBB1_4
.LBB1_3:
	add r3, r19, r0
	jal r31, luaL_addvalue
	ldw r5, r13+0
	add r3, r19, r0
	add r4, r14, r0
	jal r31, luaL_addlstring
	addi r12, r12, 1
	bne r15, r12, .LBB1_2
	jal r0, .LBB1_5
.LBB1_4:
	add r3, r11, r0
	add r4, r17, r0
	jal r31, lua_type
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_typename
	add r3, r11, r0
	add r4, r18, r0
	add r5, r1, r0
	add r6, r12, r0
	jal r31, luaL_error
	jal r0, .LBB1_3
.LBB1_5:
	add r12, r15, r0
.LBB1_6:
	bne r12, r15, .LBB1_9
.LBB1_7:
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_geti
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_isstring
	beq r1, r20, .LBB1_10
.LBB1_8:
	addi r3, fp, -572
	jal r31, luaL_addvalue
.LBB1_9:
	addi r3, fp, -572
	jal r31, luaL_pushresult
	addi r1, r0, 1
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
	addi sp, sp, 584
	jalr r0, r31, 0
.LBB1_10:
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_type
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_typename
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	add r3, r11, r0
	add r5, r1, r0
	add r6, r12, r0
	jal r31, luaL_error
	jal r0, .LBB1_8
.Lfunc_end1:
	.size	tconcat, .Lfunc_end1-tconcat
                                        # -- End function
	.p2align	2                               # -- Begin function tinsert
	.type	tinsert,@function
tinsert:                                # @tinsert
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
	addi r5, r0, 7
	add r4, r12, r0
	jal r31, checktab
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_len
	add r13, r1, r0
	addi r12, r1, 1
	add r3, r11, r0
	jal r31, lua_gettop
	addi r3, r0, 2
	beq r1, r3, .LBB2_7
.LBB2_1:
	addi r3, r0, 3
	bne r1, r3, .LBB2_8
.LBB2_2:
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_checkinteger
	add r14, r1, r0
	addi r1, r1, -1
	bgeu r1, r12, .LBB2_10
.LBB2_3:
	ble r12, r14, .LBB2_6
.LBB2_4:
	addi r12, r0, 1
.LBB2_5:
	add r15, r13, r0
	addi r13, r13, 1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r15, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, lua_seti
	addi r13, r15, -1
	bgt r15, r14, .LBB2_5
.LBB2_6:
	add r12, r14, r0
.LBB2_7:
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_seti
	addi r1, r0, 0
	jal r0, .LBB2_9
.LBB2_8:
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	add r3, r11, r0
	jal r31, luaL_error
.LBB2_9:
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
.LBB2_10:
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_argerror
	bgt r12, r14, .LBB2_4
	jal r0, .LBB2_6
.Lfunc_end2:
	.size	tinsert, .Lfunc_end2-tinsert
                                        # -- End function
	.p2align	2                               # -- Begin function tpack
	.type	tpack,@function
tpack:                                  # @tpack
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
	jal r31, lua_gettop
	add r12, r1, r0
	addi r13, r0, 1
	add r3, r11, r0
	add r4, r1, r0
	add r5, r13, r0
	jal r31, lua_createtable
	add r3, r11, r0
	add r4, r13, r0
	add r5, r13, r0
	jal r31, lua_rotate
	blt r12, r13, .LBB3_3
.LBB3_1:
	add r1, r12, r0
.LBB3_2:
	add r14, r1, r0
	add r3, r11, r0
	add r4, r13, r0
	add r5, r1, r0
	jal r31, lua_seti
	addi r1, r14, -1
	bgt r14, r13, .LBB3_2
.LBB3_3:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.14)
	addi r5, r5, %lo(.L.str.14)
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_setfield
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
.Lfunc_end3:
	.size	tpack, .Lfunc_end3-tpack
                                        # -- End function
	.p2align	2                               # -- Begin function tunpack
	.type	tunpack,@function
tunpack:                                # @tunpack
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
	addi r15, r0, 1
	add r5, r15, r0
	jal r31, luaL_optinteger
	add r13, r1, r0
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, lua_type
	addi r12, r0, 0
	ble r1, r12, .LBB4_2
.LBB4_1:
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, luaL_checkinteger
	add r14, r1, r0
	bge r1, r13, .LBB4_3
	jal r0, .LBB4_10
.LBB4_2:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_len
	add r14, r1, r0
	blt r1, r13, .LBB4_10
.LBB4_3:
	sub r12, r14, r13
	lui r1, 524288
	addi r1, r1, -2
	bgtu r12, r1, .LBB4_5
.LBB4_4:
	addi r12, r12, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_checkstack
	addi r3, r0, 0
	seq r15, r1, r3
.LBB4_5:
	addi r1, r0, 0
	bne r15, r1, .LBB4_11
.LBB4_6:
	ble r14, r13, .LBB4_9
.LBB4_7:
	addi r15, r0, 1
.LBB4_8:
	add r3, r11, r0
	add r4, r15, r0
	add r5, r13, r0
	jal r31, lua_geti
	addi r13, r13, 1
	bne r14, r13, .LBB4_8
.LBB4_9:
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r14, r0
	jal r31, lua_geti
.LBB4_10:
	add r1, r12, r0
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
.LBB4_11:
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	add r3, r11, r0
	jal r31, luaL_error
	add r12, r1, r0
	jal r0, .LBB4_10
.Lfunc_end4:
	.size	tunpack, .Lfunc_end4-tunpack
                                        # -- End function
	.p2align	2                               # -- Begin function tremove
	.type	tremove,@function
tremove:                                # @tremove
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
	addi r5, r0, 7
	add r4, r12, r0
	jal r31, checktab
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_len
	add r13, r1, r0
	addi r4, r0, 2
	add r3, r11, r0
	add r5, r1, r0
	jal r31, luaL_optinteger
	add r12, r1, r0
	beq r1, r13, .LBB5_2
.LBB5_1:
	addi r1, r12, -1
	bgtu r1, r13, .LBB5_7
.LBB5_2:
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_geti
	bge r12, r13, .LBB5_6
.LBB5_3:
	addi r14, r0, 1
.LBB5_4:
	addi r15, r12, 1
	add r3, r11, r0
	add r4, r14, r0
	add r5, r15, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r14, r0
	add r5, r12, r0
	jal r31, lua_seti
	add r12, r15, r0
	bne r13, r15, .LBB5_4
.LBB5_5:
	add r12, r13, r0
.LBB5_6:
	add r3, r11, r0
	jal r31, lua_pushnil
	addi r13, r0, 1
	add r3, r11, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, lua_seti
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
.LBB5_7:
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB5_2
.Lfunc_end5:
	.size	tremove, .Lfunc_end5-tremove
                                        # -- End function
	.p2align	2                               # -- Begin function tmove
	.type	tmove,@function
tmove:                                  # @tmove
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
	addi r19, r0, 2
	add r4, r19, r0
	jal r31, luaL_checkinteger
	add r12, r1, r0
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, luaL_checkinteger
	add r14, r1, r0
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, luaL_checkinteger
	add r15, r1, r0
	addi r4, r0, 5
	add r3, r11, r0
	jal r31, lua_type
	add r18, r1, r0
	addi r16, r0, 0
	sgt r1, r1, r16
	sub r1, r16, r1
	andi r1, r1, 4
	addi r13, r1, 1
	addi r17, r0, 1
	add r3, r11, r0
	add r4, r17, r0
	add r5, r17, r0
	jal r31, checktab
	add r3, r11, r0
	add r4, r13, r0
	add r5, r19, r0
	jal r31, checktab
	blt r14, r12, .LBB6_16
.LBB6_1:
	lui r1, 524288
	addi r20, r1, -1
	bgt r12, r16, .LBB6_3
.LBB6_2:
	add r1, r12, r20
	bge r14, r1, .LBB6_11
.LBB6_3:
	sub r19, r14, r12
	xor r1, r19, r20
	bgt r15, r1, .LBB6_12
.LBB6_4:
	bgt r15, r14, .LBB6_13
.LBB6_5:
	ble r15, r12, .LBB6_13
.LBB6_6:
	blt r18, r17, .LBB6_8
.LBB6_7:
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r13, r0
	add r6, r16, r0
	jal r31, lua_compare
	beq r1, r16, .LBB6_13
.LBB6_8:
	blt r19, r16, .LBB6_16
.LBB6_9:
	sub r18, r17, r12
	sub r12, r15, r12
.LBB6_10:
	add r3, r11, r0
	add r4, r17, r0
	add r5, r14, r0
	jal r31, lua_geti
	add r5, r12, r14
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_seti
	addi r14, r14, -1
	add r1, r18, r14
	bgt r1, r16, .LBB6_10
	jal r0, .LBB6_16
.LBB6_11:
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB6_3
.LBB6_12:
	lui r5, %hi(.L.str.17)
	addi r5, r5, %lo(.L.str.17)
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, luaL_argerror
	ble r15, r14, .LBB6_5
.LBB6_13:
	blt r19, r16, .LBB6_16
.LBB6_14:
	addi r16, r14, 1
	addi r14, r0, 1
.LBB6_15:
	add r3, r11, r0
	add r4, r14, r0
	add r5, r12, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r13, r0
	add r5, r15, r0
	jal r31, lua_seti
	addi r12, r12, 1
	addi r15, r15, 1
	bne r16, r12, .LBB6_15
.LBB6_16:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_pushvalue
	addi r1, r0, 1
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
.Lfunc_end6:
	.size	tmove, .Lfunc_end6-tmove
                                        # -- End function
	.p2align	2                               # -- Begin function sort
	.type	sort,@function
sort:                                   # @sort
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
	addi r13, r0, 1
	addi r5, r0, 7
	add r4, r13, r0
	jal r31, checktab
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_len
	add r12, r1, r0
	addi r1, r0, 2
	blt r12, r1, .LBB7_5
.LBB7_1:
	lui r1, 524288
	addi r1, r1, -1
	beq r12, r1, .LBB7_6
.LBB7_2:
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_type
	blt r1, r13, .LBB7_4
.LBB7_3:
	addi r4, r0, 2
	addi r5, r0, 6
	add r3, r11, r0
	jal r31, luaL_checktype
.LBB7_4:
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_settop
	addi r4, r0, 1
	addi r6, r0, 0
	add r3, r11, r0
	add r5, r12, r0
	jal r31, auxsort
.LBB7_5:
	addi r1, r0, 0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB7_6:
	lui r5, %hi(.L.str.18)
	addi r5, r5, %lo(.L.str.18)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB7_2
.Lfunc_end7:
	.size	sort, .Lfunc_end7-sort
                                        # -- End function
	.p2align	2                               # -- Begin function checktab
	.type	checktab,@function
checktab:                               # @checktab
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
	add r13, r5, r0
	add r12, r4, r0
	add r11, r3, r0
	jal r31, lua_type
	addi r3, r0, 5
	beq r1, r3, .LBB8_13
.LBB8_1:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_getmetatable
	addi r14, r0, 0
	beq r1, r14, .LBB8_12
.LBB8_2:
	andi r1, r13, 1
	bne r1, r14, .LBB8_5
.LBB8_3:
	addi r15, r0, 1
	andi r1, r13, 2
	bne r1, r14, .LBB8_7
.LBB8_4:
	addi r1, r0, 4
	bgeu r13, r1, .LBB8_9
	jal r0, .LBB8_11
.LBB8_5:
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	add r3, r11, r0
	jal r31, lua_pushstring
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_rawget
	beq r1, r14, .LBB8_12
.LBB8_6:
	addi r15, r0, 2
	andi r1, r13, 2
	beq r1, r14, .LBB8_4
.LBB8_7:
	lui r4, %hi(.L.str.9)
	addi r4, r4, %lo(.L.str.9)
	add r3, r11, r0
	jal r31, lua_pushstring
	addi r1, r0, -1
	xor r4, r15, r1
	add r3, r11, r0
	jal r31, lua_rawget
	beq r1, r14, .LBB8_12
.LBB8_8:
	addi r15, r15, 1
	addi r1, r0, 4
	bltu r13, r1, .LBB8_11
.LBB8_9:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	add r3, r11, r0
	jal r31, lua_pushstring
	addi r1, r0, -1
	xor r4, r15, r1
	add r3, r11, r0
	jal r31, lua_rawget
	beq r1, r14, .LBB8_12
.LBB8_10:
	addi r15, r15, 1
.LBB8_11:
	addi r1, r0, -1
	xor r4, r15, r1
	add r3, r11, r0
	jal r31, lua_settop
	jal r0, .LBB8_13
.LBB8_12:
	addi r5, r0, 5
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_checktype
.LBB8_13:
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
.Lfunc_end8:
	.size	checktab, .Lfunc_end8-checktab
                                        # -- End function
	.p2align	2                               # -- Begin function auxsort
	.type	auxsort,@function
auxsort:                                # @auxsort
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
	bgeu r4, r5, .LBB9_33
.LBB9_1:
	add r25, r6, r0
	add r12, r5, r0
	add r14, r4, r0
	add r11, r3, r0
	addi r13, fp, -100
	addi r1, r13, 4
	stw fp+-104, r1
	addi r16, r0, 1
	addi r17, r0, -1
	addi r18, r0, -2
	addi r19, r0, 0
	addi r20, r0, -3
	addi r23, r0, 100
	addi r24, r0, 2
	addi r1, fp, -80
	stw fp+-108, r1
	addi r22, fp, -88
	addi r1, r0, 4
	stw fp+-112, r1
	addi r1, r0, 8
	stw fp+-116, r1
	addi r21, r0, 12
	jal r0, .LBB9_3
.LBB9_2:
	bleu r12, r14, .LBB9_33
.LBB9_3:
	add r3, r11, r0
	add r4, r16, r0
	add r5, r14, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r16, r0
	add r5, r12, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r17, r0
	add r5, r18, r0
	jal r31, sort_comp
	beq r1, r19, .LBB9_5
.LBB9_4:
	addi r26, r0, 1
	add r3, r11, r0
	add r4, r26, r0
	add r5, r14, r0
	jal r31, lua_seti
	add r3, r11, r0
	add r4, r26, r0
	add r5, r12, r0
	jal r31, lua_seti
	sub r15, r12, r14
	bne r15, r16, .LBB9_6
	jal r0, .LBB9_33
.LBB9_5:
	add r3, r11, r0
	add r4, r20, r0
	jal r31, lua_settop
	sub r15, r12, r14
	beq r15, r16, .LBB9_33
.LBB9_6:
	bltu r15, r23, .LBB9_9
.LBB9_7:
	addi r1, r0, 0
	beq r25, r1, .LBB9_9
.LBB9_8:
	srli r26, r15, 2
	slli r4, r26, 1
	add r3, r25, r0
	jal r31, __umodsi3
	add r3, r26, r14
	add r26, r3, r1
	jal r0, .LBB9_10
.LBB9_9:
	add r1, r12, r14
	srli r26, r1, 1
.LBB9_10:
	add r3, r11, r0
	add r4, r16, r0
	add r5, r26, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r16, r0
	add r5, r14, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r18, r0
	add r5, r17, r0
	jal r31, sort_comp
	beq r1, r19, .LBB9_12
.LBB9_11:
	addi r27, r0, 1
	add r3, r11, r0
	add r4, r27, r0
	add r5, r26, r0
	jal r31, lua_seti
	add r3, r11, r0
	add r4, r27, r0
	add r5, r14, r0
	jal r31, lua_seti
	bne r15, r24, .LBB9_15
	jal r0, .LBB9_33
.LBB9_12:
	add r3, r11, r0
	add r4, r18, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r16, r0
	add r5, r12, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r17, r0
	add r5, r18, r0
	jal r31, sort_comp
	add r3, r11, r0
	beq r1, r19, .LBB9_14
.LBB9_13:
	add r4, r16, r0
	add r5, r26, r0
	jal r31, lua_seti
	add r3, r11, r0
	add r4, r16, r0
	add r5, r12, r0
	jal r31, lua_seti
	bne r15, r24, .LBB9_15
	jal r0, .LBB9_33
.LBB9_14:
	add r4, r20, r0
	jal r31, lua_settop
	beq r15, r24, .LBB9_33
.LBB9_15:
	add r3, r11, r0
	add r4, r16, r0
	add r5, r26, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r17, r0
	jal r31, lua_pushvalue
	addi r27, r12, -1
	add r3, r11, r0
	add r4, r16, r0
	add r5, r27, r0
	jal r31, lua_geti
	add r28, r27, r0
	add r15, r14, r0
	jal r0, .LBB9_17
.LBB9_16:
	add r15, r26, r0
	bltu r28, r26, .LBB9_27
.LBB9_17:
	add r3, r11, r0
	add r4, r16, r0
	add r5, r26, r0
	jal r31, lua_seti
	add r3, r11, r0
	add r4, r16, r0
	add r5, r28, r0
	jal r31, lua_seti
	addi r26, r15, 1
	add r3, r11, r0
	add r4, r16, r0
	add r5, r26, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r17, r0
	add r5, r18, r0
	jal r31, sort_comp
	beq r1, r19, .LBB9_23
.LBB9_18:
	addi r26, r15, 2
.LBB9_19:
	beq r12, r26, .LBB9_21
.LBB9_20:
	addi r15, r0, -2
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_settop
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r26, r0
	jal r31, lua_geti
	addi r4, r0, -1
	add r3, r11, r0
	add r5, r15, r0
	jal r31, sort_comp
	addi r26, r26, 1
	addi r3, r0, 0
	bne r1, r3, .LBB9_19
	jal r0, .LBB9_22
.LBB9_21:
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB9_20
.LBB9_22:
	addi r26, r26, -1
.LBB9_23:
	addi r28, r28, -1
	add r3, r11, r0
	add r4, r16, r0
	add r5, r28, r0
	jal r31, lua_geti
	add r3, r11, r0
	add r4, r20, r0
	add r5, r17, r0
	jal r31, sort_comp
	bne r1, r19, .LBB9_25
	jal r0, .LBB9_16
.LBB9_24:
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB9_26
.LBB9_25:
	bltu r28, r26, .LBB9_24
.LBB9_26:
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	addi r28, r28, -1
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r28, r0
	jal r31, lua_geti
	addi r4, r0, -3
	addi r5, r0, -1
	add r3, r11, r0
	jal r31, sort_comp
	addi r3, r0, 0
	bne r1, r3, .LBB9_25
	jal r0, .LBB9_16
.LBB9_27:
	add r3, r11, r0
	add r4, r18, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r16, r0
	add r5, r27, r0
	jal r31, lua_seti
	add r3, r11, r0
	add r4, r16, r0
	add r5, r26, r0
	jal r31, lua_seti
	sub r15, r26, r14
	sub r27, r12, r26
	bgeu r15, r27, .LBB9_29
.LBB9_28:
	addi r5, r26, -1
	add r3, r11, r0
	add r4, r14, r0
	add r6, r25, r0
	jal r31, auxsort
	addi r14, r26, 1
	jal r0, .LBB9_30
.LBB9_29:
	addi r4, r26, 1
	add r3, r11, r0
	add r5, r12, r0
	add r6, r25, r0
	jal r31, auxsort
	addi r12, r26, -1
	add r15, r27, r0
.LBB9_30:
	sub r1, r12, r14
	srli r1, r1, 7
	bleu r1, r15, .LBB9_2
.LBB9_31:
	jal r31, clock
	ldw r15, fp+-108
	stw r15+0, r1
	add r3, r19, r0
	jal r31, time
	stw r22+4, r2
	stw r22+0, r1
	add r3, r13, r0
	add r4, r15, r0
	ldw r5, fp+-112
	jal r31, memcpy
	ldw r3, fp+-104
	add r4, r22, r0
	ldw r5, fp+-116
	jal r31, memcpy
	add r1, r19, r0
	add r25, r19, r0
.LBB9_32:
	add r3, r13, r1
	ldw r3, r3+0
	add r25, r3, r25
	addi r1, r1, 4
	bne r1, r21, .LBB9_32
	jal r0, .LBB9_2
.LBB9_33:
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
.Lfunc_end9:
	.size	auxsort, .Lfunc_end9-auxsort
                                        # -- End function
	.p2align	2                               # -- Begin function sort_comp
	.type	sort_comp,@function
sort_comp:                              # @sort_comp
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
	addi r4, r0, 2
	jal r31, lua_type
	addi r3, r0, 0
	beq r1, r3, .LBB10_2
.LBB10_1:
	addi r14, r0, 2
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushvalue
	addi r4, r13, -1
	add r3, r11, r0
	jal r31, lua_pushvalue
	addi r4, r12, -2
	add r3, r11, r0
	jal r31, lua_pushvalue
	addi r5, r0, 1
	addi r6, r0, 0
	add r3, r11, r0
	add r4, r14, r0
	add r7, r6, r0
	jal r31, lua_callk
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_toboolean
	add r12, r1, r0
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	add r1, r12, r0
	jal r0, .LBB10_3
.LBB10_2:
	addi r6, r0, 1
	add r3, r11, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, lua_compare
.LBB10_3:
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
	.size	sort_comp, .Lfunc_end10-sort_comp
                                        # -- End function
	.type	tab_funcs,@object               # @tab_funcs
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
tab_funcs:
	.word	.L.str
	.word	tconcat
	.word	.L.str.1
	.word	tinsert
	.word	.L.str.2
	.word	tpack
	.word	.L.str.3
	.word	tunpack
	.word	.L.str.4
	.word	tremove
	.word	.L.str.5
	.word	tmove
	.word	.L.str.6
	.word	sort
	.zero	8
	.size	tab_funcs, 64

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"concat"
	.size	.L.str, 7

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"insert"
	.size	.L.str.1, 7

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"pack"
	.size	.L.str.2, 5

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"unpack"
	.size	.L.str.3, 7

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"remove"
	.size	.L.str.4, 7

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"move"
	.size	.L.str.5, 5

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"sort"
	.size	.L.str.6, 5

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.zero	1
	.size	.L.str.7, 1

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"__index"
	.size	.L.str.8, 8

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"__newindex"
	.size	.L.str.9, 11

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"__len"
	.size	.L.str.10, 6

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"invalid value (%s) at index %I in table for 'concat'"
	.size	.L.str.11, 53

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"position out of bounds"
	.size	.L.str.12, 23

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"wrong number of arguments to 'insert'"
	.size	.L.str.13, 38

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"n"
	.size	.L.str.14, 2

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"too many results to unpack"
	.size	.L.str.15, 27

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"too many elements to move"
	.size	.L.str.16, 26

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"destination wrap around"
	.size	.L.str.17, 24

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"array too big"
	.size	.L.str.18, 14

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"invalid order function for sorting"
	.size	.L.str.19, 35

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
