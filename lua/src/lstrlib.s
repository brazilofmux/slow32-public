	.file	"lstrlib.c"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaopen_string
.LCPI0_0:
	.quad	0x407f800000000000              # double 504
	.text
	.globl	luaopen_string
	.p2align	2
	.type	luaopen_string,@function
luaopen_string:                         # @luaopen_string
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
	lui r4, %hi(strlib)
	addi r4, r4, %lo(strlib)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_setfuncs
	addi r5, r0, 9
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_createtable
	lui r4, %hi(stringmetamethods)
	addi r4, r4, %lo(stringmetamethods)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_setfuncs
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	add r3, r11, r0
	jal r31, lua_pushstring
	addi r12, r0, -2
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushvalue
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_setmetatable
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushvalue
	lui r5, %hi(.L.str.79)
	addi r5, r5, %lo(.L.str.79)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_setfield
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_settop
	addi r1, r0, 1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaopen_string, .Lfunc_end0-luaopen_string
                                        # -- End function
	.p2align	2                               # -- Begin function str_byte
	.type	str_byte,@function
str_byte:                               # @str_byte
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
	addi r14, r0, 1
	addi r12, fp, -32
	add r4, r14, r0
	add r5, r12, r0
	jal r31, luaL_checklstring
	add r13, r1, r0
	addi r4, r0, 2
	add r3, r11, r0
	add r5, r14, r0
	jal r31, luaL_optinteger
	ldw r16, r12+0
	addi r12, r0, 0
	add r15, r1, r0
	bgt r1, r12, .LBB1_5
.LBB1_1:
	beq r1, r12, .LBB1_4
.LBB1_2:
	sub r3, r12, r16
	add r15, r14, r0
	blt r1, r3, .LBB1_5
.LBB1_3:
	add r3, r16, r1
	addi r15, r3, 1
	jal r0, .LBB1_5
.LBB1_4:
	add r15, r14, r0
.LBB1_5:
	addi r4, r0, 3
	add r3, r11, r0
	add r5, r1, r0
	jal r31, luaL_optinteger
	ble r1, r16, .LBB1_7
.LBB1_6:
	add r3, r16, r0
	jal r0, .LBB1_11
.LBB1_7:
	addi r3, r0, -1
	ble r1, r3, .LBB1_9
.LBB1_8:
	add r3, r1, r0
	jal r0, .LBB1_11
.LBB1_9:
	sub r4, r12, r16
	add r3, r12, r0
	blt r1, r4, .LBB1_11
.LBB1_10:
	add r1, r16, r1
	addi r3, r1, 1
.LBB1_11:
	bltu r3, r15, .LBB1_15
.LBB1_12:
	sub r1, r3, r15
	lui r3, 524288
	addi r3, r3, -1
	bgeu r1, r3, .LBB1_16
.LBB1_13:
	addi r12, r1, 1
	lui r5, %hi(.L.str.17)
	addi r5, r5, %lo(.L.str.17)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_checkstack
	add r1, r15, r13
	addi r13, r1, -1
	addi r14, r0, 0
	add r15, r12, r0
.LBB1_14:
	ldbu r4, r13+0
	add r3, r11, r0
	jal r31, lua_pushinteger
	addi r15, r15, -1
	addi r13, r13, 1
	bne r15, r14, .LBB1_14
.LBB1_15:
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
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB1_16:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r11, r0
	jal r31, luaL_error
	add r12, r1, r0
	jal r0, .LBB1_15
.Lfunc_end1:
	.size	str_byte, .Lfunc_end1-str_byte
                                        # -- End function
	.p2align	2                               # -- Begin function str_char
	.type	str_char,@function
str_char:                               # @str_char
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
	stw fp+-36, lr
	add r11, r3, r0
	jal r31, lua_gettop
	add r12, r1, r0
	addi r4, fp, -564
	add r3, r11, r0
	add r5, r1, r0
	jal r31, luaL_buffinitsize
	add r13, r1, r0
	addi r1, r0, 1
	blt r12, r1, .LBB2_5
.LBB2_1:
	addi r17, r0, 0
	addi r16, r0, 256
	lui r14, %hi(.L.str.18)
	addi r14, r14, %lo(.L.str.18)
.LBB2_2:
	addi r15, r17, 1
	add r3, r11, r0
	add r4, r15, r0
	jal r31, luaL_checkinteger
	bgeu r1, r16, .LBB2_4
.LBB2_3:
	add r3, r13, r17
	stb r3+0, r1
	add r17, r15, r0
	bne r12, r15, .LBB2_2
	jal r0, .LBB2_5
.LBB2_4:
	add r3, r11, r0
	add r4, r15, r0
	add r5, r14, r0
	add r18, r1, r0
	jal r31, luaL_argerror
	add r1, r18, r0
	jal r0, .LBB2_3
.LBB2_5:
	addi r3, fp, -564
	add r4, r12, r0
	jal r31, luaL_pushresultsize
	addi r1, r0, 1
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
	addi sp, sp, 584
	jalr r0, r31, 0
.Lfunc_end2:
	.size	str_char, .Lfunc_end2-str_char
                                        # -- End function
	.p2align	2                               # -- Begin function str_dump
	.type	str_dump,@function
str_dump:                               # @str_dump
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
	stw fp+-24, lr
	add r12, r3, r0
	addi r4, r0, 2
	jal r31, lua_toboolean
	add r14, r1, r0
	addi r11, r0, 1
	addi r5, r0, 6
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaL_checktype
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_settop
	addi r15, r0, 0
	addi r13, fp, -556
	stw r13+0, r15
	lui r4, %hi(writer)
	addi r4, r4, %lo(writer)
	add r3, r12, r0
	add r5, r13, r0
	add r6, r14, r0
	jal r31, lua_dump
	bne r1, r15, .LBB3_3
.LBB3_1:
	addi r3, r13, 4
	jal r31, luaL_pushresult
.LBB3_2:
	add r1, r11, r0
	ldw lr, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 568
	jalr r0, r31, 0
.LBB3_3:
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r3, r12, r0
	jal r31, luaL_error
	add r11, r1, r0
	jal r0, .LBB3_2
.Lfunc_end3:
	.size	str_dump, .Lfunc_end3-str_dump
                                        # -- End function
	.p2align	2                               # -- Begin function str_find
	.type	str_find,@function
str_find:                               # @str_find
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r4, r0, 1
	jal r31, str_find_aux
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	str_find, .Lfunc_end4-str_find
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function str_format
.LCPI5_0:
	.quad	0x7ff0000000000000              # double +Inf
.LCPI5_1:
	.quad	0xfff0000000000000              # double -Inf
	.text
	.p2align	2
	.type	str_format,@function
str_format:                             # @str_format
# %bb.0:
	addi sp, sp, -712
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 712
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
	add r11, r3, r0
	jal r31, lua_gettop
	stw fp+-660, r1
	addi r4, r0, 1
	addi r14, fp, -96
	add r3, r11, r0
	stw fp+-664, r4
	add r5, r14, r0
	jal r31, luaL_checklstring
	add r16, r1, r0
	ldw r15, r14+0
	addi r14, fp, -624
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaL_buffinit
	addi r20, r0, 0
	beq r15, r20, .LBB5_91
.LBB5_1:
	add r19, r16, r15
	addi r15, fp, -656
	addi r17, r15, 1
	addi r24, r0, 37
	addi r18, r0, 120
	lui r26, %hi(.L.str.42)
	addi r26, r26, %lo(.L.str.42)
	addi r1, r0, 22
	stw fp+-668, r1
	lui r1, %hi(.L.str.43)
	addi r1, r1, %lo(.L.str.43)
	stw fp+-684, r1
	lui r1, %hi(.L.str.32)
	addi r1, r1, %lo(.L.str.32)
	stw fp+-676, r1
	addi r1, r0, 55
	stw fp+-672, r1
	lui r1, %hi(.L.str.41)
	addi r1, r1, %lo(.L.str.41)
	stw fp+-704, r1
	lui r1, %hi(.LJTI5_0)
	addi r1, r1, %lo(.LJTI5_0)
	stw fp+-680, r1
	lui r1, %hi(.L.str.36)
	addi r1, r1, %lo(.L.str.36)
	stw fp+-696, r1
	lui r1, %hi(.L.str.37)
	addi r1, r1, %lo(.L.str.37)
	stw fp+-700, r1
                                        # implicit-def: $r23
	ldw r25, fp+-664
	jal r0, .LBB5_5
.LBB5_2:
	addi r1, r16, 1
	ldbu r3, r16+0
.LBB5_3:
	ldw r4, r14+0
	ldw r5, r14+8
	addi r6, r5, 1
	stw r14+8, r6
	add r4, r4, r5
	stb r4+0, r3
	add r16, r1, r0
	add r25, r21, r0
.LBB5_4:
	bgeu r16, r19, .LBB5_91
.LBB5_5:
	add r21, r25, r0
	ldbu r1, r16+0
	bne r1, r24, .LBB5_10
.LBB5_6:
	ldbu r1, r16+1
	bne r1, r24, .LBB5_12
.LBB5_7:
	ldw r1, r14+8
	ldw r3, r14+4
	bltu r1, r3, .LBB5_9
.LBB5_8:
	add r3, r14, r0
	ldw r4, fp+-664
	jal r31, luaL_prepbuffsize
.LBB5_9:
	addi r1, r16, 2
	ldbu r3, r16+1
	jal r0, .LBB5_3
.LBB5_10:
	ldw r1, r14+8
	ldw r3, r14+4
	bltu r1, r3, .LBB5_2
.LBB5_11:
	addi r3, fp, -624
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
	jal r0, .LBB5_2
.LBB5_12:
	addi r16, r16, 1
	add r3, r14, r0
	add r4, r18, r0
	jal r31, luaL_prepbuffsize
	add r12, r1, r0
	addi r25, r21, 1
	ldw r1, fp+-660
	bge r21, r1, .LBB5_18
.LBB5_13:
	add r3, r16, r0
	add r4, r26, r0
	jal r31, strspn
	add r27, r1, r0
	addi r28, r1, 1
	ldw r1, fp+-668
	bltu r28, r1, .LBB5_15
.LBB5_14:
	add r3, r11, r0
	ldw r4, fp+-684
	jal r31, luaL_error
.LBB5_15:
	stb r15+0, r24
	add r3, r17, r0
	add r4, r16, r0
	add r5, r28, r0
	jal r31, memcpy
	add r1, r17, r28
	stb r1+0, r20
	add r1, r16, r27
	addi r16, r1, 1
	ldbu r1, r1+0
	addi r1, r1, -65
	ldw r3, fp+-672
	bgtu r1, r3, .LBB5_29
.LBB5_16:
	slli r1, r1, 2
	ldw r3, fp+-680
	add r1, r3, r1
	ldw r1, r1+0
	ldw r27, fp+-676
	jalr r0, r1, 0
.LBB5_17:
	add r3, r11, r0
	add r4, r25, r0
	jal r31, luaL_checknumber
	stw fp+-688, r1
	stw fp+-692, r2
	lui r5, %hi(.L.str.36)
	addi r5, r5, %lo(.L.str.36)
	addi r6, r0, 1
	add r3, r11, r0
	add r4, r15, r0
	jal r31, checkformat
	add r3, r15, r0
	jal r31, strlen
	add r27, r1, r0
	lui r28, %hi(.L.str.37)
	addi r28, r28, %lo(.L.str.37)
	add r3, r28, r0
	jal r31, strlen
	add r22, r23, r0
	add r23, r1, r0
	add r27, r15, r27
	addi r3, r27, -1
	add r21, r19, r0
	add r19, r26, r0
	add r13, r18, r0
	ldbu r18, r27+-1
	add r4, r28, r0
	jal r31, strcpy
	add r1, r27, r23
	add r23, r22, r0
	stb r1+-1, r18
	add r18, r13, r0
	add r26, r19, r0
	add r19, r21, r0
	addi r3, r0, 0
	stb r1+0, r3
	add r3, r12, r0
	add r4, r15, r0
	ldw r5, fp+-688
	ldw r6, fp+-692
	jal r0, .LBB5_32
.LBB5_18:
	lui r5, %hi(.L.str.30)
	addi r5, r5, %lo(.L.str.30)
	add r3, r11, r0
	add r4, r25, r0
	jal r31, luaL_argerror
.LBB5_19:
	add r23, r1, r0
	add r21, r20, r0
	bne r21, r20, .LBB5_4
	jal r0, .LBB5_92
.LBB5_20:
	lui r27, %hi(.L.str.34)
	addi r27, r27, %lo(.L.str.34)
	jal r0, .LBB5_31
.LBB5_21:
	add r3, r11, r0
	add r4, r15, r0
	ldw r5, fp+-696
	ldw r6, fp+-664
	jal r31, checkformat
	add r3, r15, r0
	jal r31, strlen
	add r21, r1, r0
	ldw r27, fp+-700
	add r3, r27, r0
	jal r31, strlen
	add r22, r1, r0
	add r21, r15, r21
	addi r3, r21, -1
	add r13, r26, r0
	add r26, r23, r0
	ldbu r23, r21+-1
	add r4, r27, r0
	jal r31, strcpy
	add r1, r21, r22
	stb r1+-1, r23
	add r23, r26, r0
	add r26, r13, r0
	stb r1+0, r20
	add r3, r11, r0
	add r4, r25, r0
	jal r31, luaL_checknumber
	add r3, r11, r0
	add r4, r12, r0
	add r5, r15, r0
	add r7, r1, r0
	add r8, r2, r0
	jal r31, lua_number2strx
	jal r0, .LBB5_33
.LBB5_22:
	addi r1, r15, 2
	ldbu r1, r1+0
	addi r21, r0, 0
	beq r1, r21, .LBB5_49
.LBB5_23:
	lui r4, %hi(.L.str.39)
	addi r4, r4, %lo(.L.str.39)
	add r3, r11, r0
	jal r31, luaL_error
	add r23, r1, r0
	bne r21, r20, .LBB5_4
	jal r0, .LBB5_92
.LBB5_24:
	addi r28, fp, -88
	add r3, r11, r0
	add r4, r25, r0
	add r5, r28, r0
	jal r31, luaL_tolstring
	add r27, r1, r0
	addi r1, r15, 2
	ldbu r1, r1+0
	addi r3, r0, 0
	add r22, r3, r0
	beq r1, r3, .LBB5_69
.LBB5_25:
	stw fp+-688, r23
	add r23, r18, r0
	ldw r18, r28+0
	add r3, r27, r0
	jal r31, strlen
	bne r18, r1, .LBB5_89
.LBB5_26:
	lui r5, %hi(.L.str.31)
	addi r5, r5, %lo(.L.str.31)
	addi r21, fp, -656
	addi r6, r0, 1
	add r3, r11, r0
	add r4, r21, r0
	jal r31, checkformat
	addi r4, r0, 46
	add r3, r21, r0
	jal r31, strchr
	add r18, r23, r0
	bne r1, r22, .LBB5_70
.LBB5_27:
	ldw r1, r28+0
	addi r3, r0, 100
	bltu r1, r3, .LBB5_70
.LBB5_28:
	addi r3, fp, -624
	jal r31, luaL_addvalue
	add r1, r22, r0
	ldw r23, fp+-688
	jal r0, .LBB5_33
.LBB5_29:
	add r3, r11, r0
	ldw r4, fp+-704
	add r5, r15, r0
	jal r31, luaL_error
	jal r0, .LBB5_19
.LBB5_30:
	lui r27, %hi(.L.str.33)
	addi r27, r27, %lo(.L.str.33)
.LBB5_31:
	add r3, r11, r0
	add r4, r25, r0
	jal r31, luaL_checkinteger
	add r28, r1, r0
	addi r6, r0, 1
	add r3, r11, r0
	add r4, r15, r0
	add r5, r27, r0
	jal r31, checkformat
	add r3, r15, r0
	jal r31, strlen
	add r27, r1, r0
	lui r21, %hi(.L.str.35)
	addi r21, r21, %lo(.L.str.35)
	add r3, r21, r0
	jal r31, strlen
	add r22, r1, r0
	add r13, r26, r0
	add r26, r23, r0
	add r23, r15, r27
	addi r3, r23, -1
	ldbu r27, r23+-1
	add r4, r21, r0
	jal r31, strcpy
	add r1, r23, r22
	add r23, r26, r0
	add r26, r13, r0
	stb r1+-1, r27
	addi r3, r0, 0
	stb r1+0, r3
	add r3, r12, r0
	add r4, r15, r0
	add r5, r28, r0
.LBB5_32:
	jal r31, sprintf
.LBB5_33:
	ldw r3, r14+8
	add r1, r3, r1
	stw r14+8, r1
	addi r21, r0, 1
	bne r21, r20, .LBB5_4
	jal r0, .LBB5_92
.LBB5_34:
	add r3, r11, r0
	add r4, r25, r0
	jal r31, lua_topointer
	add r27, r1, r0
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	add r3, r17, r0
	jal r31, strspn
	add r21, r17, r1
	ldbu r3, r21+0
	addi r1, r0, 48
	beq r3, r1, .LBB5_37
.LBB5_35:
	jal r31, isdigit
	addi r22, r0, 0
	beq r1, r22, .LBB5_37
.LBB5_36:
	add r28, r23, r0
	add r23, r18, r0
	addi r18, r21, 1
	ldbu r3, r21+1
	jal r31, isdigit
	seq r1, r1, r22
	addi r3, r21, 2
	xor r4, r18, r3
	add r18, r23, r0
	add r23, r28, r0
	sub r1, r22, r1
	and r1, r4, r1
	xor r21, r3, r1
.LBB5_37:
	ldbu r3, r21+0
	jal r31, isalpha
	addi r21, r0, 0
	bne r1, r21, .LBB5_39
.LBB5_38:
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	addi r5, fp, -656
	add r3, r11, r0
	jal r31, luaL_error
.LBB5_39:
	bne r27, r21, .LBB5_41
.LBB5_40:
	add r3, r15, r0
	jal r31, strlen
	add r1, r15, r1
	addi r3, r0, 115
	stb r1+-1, r3
	lui r27, %hi(.L.str.38)
	addi r27, r27, %lo(.L.str.38)
.LBB5_41:
	addi r4, fp, -656
	add r3, r12, r0
	add r5, r27, r0
	jal r0, .LBB5_32
.LBB5_42:
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	add r3, r17, r0
	jal r31, strspn
	add r21, r17, r1
	ldbu r3, r21+0
	addi r1, r0, 48
	beq r3, r1, .LBB5_45
.LBB5_43:
	jal r31, isdigit
	addi r22, r0, 0
	beq r1, r22, .LBB5_45
.LBB5_44:
	add r27, r23, r0
	addi r23, r21, 1
	ldbu r3, r21+1
	jal r31, isdigit
	seq r1, r1, r22
	addi r3, r21, 2
	xor r4, r23, r3
	add r23, r27, r0
	sub r1, r22, r1
	and r1, r4, r1
	xor r21, r3, r1
.LBB5_45:
	ldbu r3, r21+0
	jal r31, isalpha
	addi r3, r0, 0
	bne r1, r3, .LBB5_47
.LBB5_46:
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	addi r5, fp, -656
	add r3, r11, r0
	jal r31, luaL_error
.LBB5_47:
	add r3, r11, r0
	add r4, r25, r0
	jal r31, luaL_checkinteger
	addi r4, fp, -656
	add r3, r12, r0
	add r5, r1, r0
	jal r0, .LBB5_32
.LBB5_48:
	addi r3, fp, -624
	addi r4, r0, 418
	jal r31, luaL_prepbuffsize
	add r12, r1, r0
	jal r0, .LBB5_17
.LBB5_49:
	add r3, r11, r0
	add r4, r25, r0
	jal r31, lua_type
	addi r3, r0, 2
	bltu r1, r3, .LBB5_71
.LBB5_50:
	addi r3, r0, 3
	beq r1, r3, .LBB5_72
.LBB5_51:
	addi r3, r0, 4
	bne r1, r3, .LBB5_77
.LBB5_52:
	stw fp+-688, r23
	add r28, r26, r0
	addi r21, fp, -92
	add r3, r11, r0
	add r4, r25, r0
	add r5, r21, r0
	jal r31, lua_tolstring
	add r26, r1, r0
	ldw r23, r21+0
	ldw r1, r14+8
	ldw r3, r14+4
	bltu r1, r3, .LBB5_54
.LBB5_53:
	addi r3, fp, -624
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
.LBB5_54:
	ldw r1, r14+0
	ldw r3, r14+8
	addi r4, r3, 1
	stw r14+8, r4
	add r1, r1, r3
	addi r22, r0, 34
	stb r1+0, r22
	addi r1, r0, 0
	add r27, r1, r0
	beq r23, r1, .LBB5_74
.LBB5_55:
	addi r26, r26, 1
	jal r0, .LBB5_58
.LBB5_56:
	ldbu r3, r26+-1
	ldw r4, r14+0
	ldw r5, r14+8
	addi r6, r5, 1
	stw r14+8, r6
	add r4, r4, r5
	stb r4+0, r3
.LBB5_57:
	addi r23, r23, -1
	addi r26, r26, 1
	beq r23, r1, .LBB5_74
.LBB5_58:
	ldbu r3, r26+-1
	addi r4, r0, 10
	beq r3, r4, .LBB5_61
.LBB5_59:
	addi r4, r0, 92
	beq r3, r4, .LBB5_61
.LBB5_60:
	bne r3, r22, .LBB5_66
.LBB5_61:
	ldw r3, r14+8
	ldw r4, r14+4
	bltu r3, r4, .LBB5_63
.LBB5_62:
	addi r3, fp, -624
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
	add r1, r27, r0
.LBB5_63:
	ldw r3, r14+0
	ldw r4, r14+8
	addi r5, r4, 1
	stw r14+8, r5
	add r3, r3, r4
	addi r4, r0, 92
	stb r3+0, r4
.LBB5_64:
	ldw r3, r14+8
	ldw r4, r14+4
	bltu r3, r4, .LBB5_56
.LBB5_65:
	addi r3, fp, -624
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
	add r1, r27, r0
	jal r0, .LBB5_56
.LBB5_66:
	jal r31, iscntrl
	beq r1, r27, .LBB5_68
.LBB5_67:
	ldbu r3, r26+0
	jal r31, isdigit
	seq r1, r1, r27
	ldbu r5, r26+-1
	lui r3, %hi(.L.str.53)
	addi r3, r3, %lo(.L.str.53)
	lui r4, %hi(.L.str.52)
	addi r4, r4, %lo(.L.str.52)
	xor r4, r4, r3
	sub r1, r27, r1
	and r1, r4, r1
	xor r4, r1, r3
	addi r21, fp, -88
	add r3, r21, r0
	jal r31, sprintf
	addi r3, fp, -624
	add r4, r21, r0
	jal r31, luaL_addstring
	add r1, r27, r0
	jal r0, .LBB5_57
.LBB5_68:
	add r1, r27, r0
	jal r0, .LBB5_64
.LBB5_69:
	addi r3, fp, -624
	jal r31, luaL_addvalue
	add r1, r22, r0
	jal r0, .LBB5_33
.LBB5_70:
	addi r4, fp, -656
	add r3, r12, r0
	add r5, r27, r0
	jal r31, sprintf
	add r21, r1, r0
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	add r1, r21, r0
	ldw r23, fp+-688
	jal r0, .LBB5_33
.LBB5_71:
	addi r21, r0, 0
	add r3, r11, r0
	add r4, r25, r0
	add r5, r21, r0
	jal r31, luaL_tolstring
	addi r3, fp, -624
	jal r31, luaL_addvalue
	add r1, r21, r0
	jal r0, .LBB5_33
.LBB5_72:
	add r22, r26, r0
	addi r26, fp, -624
	addi r4, r0, 120
	add r3, r26, r0
	jal r31, luaL_prepbuffsize
	add r27, r1, r0
	add r3, r11, r0
	add r4, r25, r0
	jal r31, lua_isinteger
	addi r5, r0, 0
	beq r1, r5, .LBB5_78
.LBB5_73:
	add r3, r11, r0
	add r4, r25, r0
	add r21, r5, r0
	jal r31, lua_tointegerx
	lui r3, 524288
	seq r3, r1, r3
	sub r3, r21, r3
	lui r4, %hi(.L.str.50)
	addi r4, r4, %lo(.L.str.50)
	lui r5, %hi(.L.str.49)
	addi r5, r5, %lo(.L.str.49)
	xor r5, r5, r4
	and r3, r5, r3
	xor r4, r3, r4
	add r3, r27, r0
	add r5, r1, r0
	jal r0, .LBB5_83
.LBB5_74:
	ldw r3, r14+8
	ldw r4, r14+4
	bltu r3, r4, .LBB5_76
.LBB5_75:
	addi r3, fp, -624
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
	add r1, r27, r0
.LBB5_76:
	ldw r3, r14+0
	ldw r4, r14+8
	addi r5, r4, 1
	stw r14+8, r5
	add r3, r3, r4
	stb r3+0, r22
	add r26, r28, r0
	ldw r23, fp+-688
	jal r0, .LBB5_33
.LBB5_77:
	lui r5, %hi(.L.str.51)
	addi r5, r5, %lo(.L.str.51)
	add r3, r11, r0
	add r4, r25, r0
	jal r31, luaL_argerror
	addi r1, r0, 0
	jal r0, .LBB5_33
.LBB5_78:
	add r21, r5, r0
	addi r12, r0, 0
	add r3, r11, r0
	add r4, r25, r0
	add r5, r12, r0
	jal r31, lua_tonumberx
	add r6, r1, r0
	add r7, r2, r0
	lui r1, %hi(.LCPI5_0)
	addi r1, r1, %lo(.LCPI5_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r6, r4
	beq r1, r12, .LBB5_80
.LBB5_79:
	lui r5, %hi(.L.str.54)
	addi r5, r5, %lo(.L.str.54)
	jal r0, .LBB5_82
.LBB5_80:
	lui r1, %hi(.LCPI5_1)
	addi r1, r1, %lo(.LCPI5_1)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r6, r4
	beq r1, r12, .LBB5_85
.LBB5_81:
	lui r5, %hi(.L.str.55)
	addi r5, r5, %lo(.L.str.55)
.LBB5_82:
	lui r4, %hi(.L.str.58)
	addi r4, r4, %lo(.L.str.58)
	add r3, r27, r0
.LBB5_83:
	jal r31, sprintf
	add r28, r1, r0
.LBB5_84:
	add r1, r21, r0
	ldw r3, r26+8
	add r3, r3, r28
	stw r26+8, r3
	add r26, r22, r0
	jal r0, .LBB5_33
.LBB5_85:
	feq.d r1, r6, r6
	and r1, r1, r1
	xori r1, r1, 1
	bne r1, r12, .LBB5_90
.LBB5_86:
	lui r5, %hi(.L.str.57)
	addi r5, r5, %lo(.L.str.57)
	add r3, r11, r0
	add r4, r27, r0
	add r7, r6, r0
	add r8, r2, r0
	jal r31, lua_number2strx
	add r28, r1, r0
	addi r4, r0, 46
	add r3, r27, r0
	add r5, r1, r0
	jal r31, memchr
	bne r1, r12, .LBB5_84
.LBB5_87:
	addi r13, r0, 46
	add r3, r27, r0
	add r4, r13, r0
	add r5, r28, r0
	jal r31, memchr
	beq r1, r12, .LBB5_84
.LBB5_88:
	stb r1+0, r13
	jal r0, .LBB5_84
.LBB5_89:
	lui r5, %hi(.L.str.40)
	addi r5, r5, %lo(.L.str.40)
	add r3, r11, r0
	add r4, r25, r0
	jal r31, luaL_argerror
	jal r0, .LBB5_26
.LBB5_90:
	lui r5, %hi(.L.str.56)
	addi r5, r5, %lo(.L.str.56)
	jal r0, .LBB5_82
.LBB5_91:
	addi r3, fp, -624
	jal r31, luaL_pushresult
	addi r23, r0, 1
.LBB5_92:
	add r1, r23, r0
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
	addi sp, sp, 712
	jalr r0, r31, 0
.Lfunc_end5:
	.size	str_format, .Lfunc_end5-str_format
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI5_0:
	.word	.LBB5_21
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_17
	.word	.LBB5_29
	.word	.LBB5_17
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_20
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_21
	.word	.LBB5_29
	.word	.LBB5_42
	.word	.LBB5_31
	.word	.LBB5_17
	.word	.LBB5_48
	.word	.LBB5_17
	.word	.LBB5_29
	.word	.LBB5_31
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_20
	.word	.LBB5_34
	.word	.LBB5_22
	.word	.LBB5_29
	.word	.LBB5_24
	.word	.LBB5_29
	.word	.LBB5_30
	.word	.LBB5_29
	.word	.LBB5_29
	.word	.LBB5_20
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function gmatch
	.type	gmatch,@function
gmatch:                                 # @gmatch
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
	addi r17, r0, 1
	addi r14, fp, -36
	add r4, r17, r0
	add r5, r14, r0
	jal r31, luaL_checklstring
	add r12, r1, r0
	addi r4, r0, 2
	addi r15, fp, -40
	add r3, r11, r0
	add r5, r15, r0
	jal r31, luaL_checklstring
	add r13, r1, r0
	addi r4, r0, 3
	add r3, r11, r0
	add r5, r17, r0
	jal r31, luaL_optinteger
	addi r16, r0, 0
	ble r1, r16, .LBB6_2
.LBB6_1:
	add r17, r1, r0
	jal r0, .LBB6_5
.LBB6_2:
	beq r1, r16, .LBB6_5
.LBB6_3:
	ldw r3, r14+0
	sub r4, r16, r3
	blt r1, r4, .LBB6_5
.LBB6_4:
	add r1, r1, r3
	addi r17, r1, 1
.LBB6_5:
	addi r17, r17, -1
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_settop
	addi r4, r0, 292
	add r3, r11, r0
	add r5, r16, r0
	jal r31, lua_newuserdatauv
	ldw r3, r14+0
	sgtu r4, r17, r3
	addi r5, r3, 1
	sub r4, r16, r4
	xor r5, r5, r17
	and r4, r5, r4
	xor r4, r17, r4
	ldw r5, r15+0
	stw r1+24, r11
	addi r6, r0, 200
	stw r1+28, r6
	stw r1+12, r12
	add r3, r12, r3
	stw r1+16, r3
	add r3, r13, r5
	stw r1+20, r3
	add r3, r12, r4
	stw r1+0, r3
	stw r1+4, r13
	stw r1+8, r16
	lui r4, %hi(gmatch_aux)
	addi r4, r4, %lo(gmatch_aux)
	addi r5, r0, 3
	add r3, r11, r0
	jal r31, lua_pushcclosure
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
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end6:
	.size	gmatch, .Lfunc_end6-gmatch
                                        # -- End function
	.p2align	2                               # -- Begin function str_gsub
	.type	str_gsub,@function
str_gsub:                               # @str_gsub
# %bb.0:
	addi sp, sp, -968
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 968
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
	add r19, r3, r0
	addi r21, r0, 1
	addi r17, fp, -84
	add r4, r21, r0
	add r5, r17, r0
	jal r31, luaL_checklstring
	add r11, r1, r0
	addi r4, r0, 2
	addi r18, fp, -88
	add r3, r19, r0
	add r5, r18, r0
	jal r31, luaL_checklstring
	add r13, r1, r0
	addi r4, r0, 3
	add r3, r19, r0
	jal r31, lua_type
	add r23, r1, r0
	ldw r1, r17+0
	addi r5, r1, 1
	addi r16, r0, 4
	add r3, r19, r0
	add r4, r16, r0
	jal r31, luaL_optinteger
	add r15, r1, r0
	ldbu r12, r13+0
	addi r14, r0, 94
	addi r1, r23, -3
	bltu r1, r16, .LBB7_2
.LBB7_1:
	lui r5, %hi(.L.str.59)
	addi r5, r5, %lo(.L.str.59)
	addi r4, r0, 3
	add r3, r19, r0
	jal r31, luaL_typeerror
.LBB7_2:
	addi r16, fp, -896
	add r3, r19, r0
	add r4, r16, r0
	jal r31, luaL_buffinit
	bne r12, r14, .LBB7_4
.LBB7_3:
	addi r13, r13, 1
	ldw r1, r18+0
	addi r1, r1, -1
	stw r18+0, r1
.LBB7_4:
	sne r24, r12, r14
	ldw r1, r17+0
	ldw r3, r18+0
	addi r18, fp, -368
	stw fp+-940, r19
	stw r18+12, r19
	addi r4, r0, 200
	stw r18+16, r4
	stw r18+0, r11
	add r1, r11, r1
	stw r18+4, r1
	add r1, r13, r3
	stw r18+8, r1
	addi r1, r18, 24
	stw fp+-900, r1
	addi r17, r0, 0
	addi r25, r0, 5
	addi r28, r0, -2
	addi r1, r0, 3
	stw fp+-904, r1
	addi r20, r0, -1
	lui r1, %hi(.L.str.29)
	addi r1, r1, %lo(.L.str.29)
	stw fp+-948, r1
	lui r1, %hi(.L.str.60)
	addi r1, r1, %lo(.L.str.60)
	stw fp+-944, r1
	addi r1, r0, 6
	stw fp+-912, r1
	lui r1, %hi(.L.str.23)
	addi r1, r1, %lo(.L.str.23)
	stw fp+-936, r1
	add r14, r17, r0
	add r22, r17, r0
	add r12, r17, r0
	stw fp+-908, r20
	stw fp+-916, r21
	stw fp+-920, r23
	stw fp+-924, r24
	stw fp+-928, r25
	stw fp+-932, r28
	jal r0, .LBB7_8
.LBB7_5:
	addi r1, r11, 1
	ldbu r3, r11+0
	ldw r4, r16+0
	ldw r5, r16+8
	addi r6, r5, 1
	stw r16+8, r6
	add r4, r4, r5
	stb r4+0, r3
	add r11, r1, r0
.LBB7_6:
	add r1, r24, r0
.LBB7_7:
	beq r1, r17, .LBB7_59
.LBB7_8:
	bge r22, r15, .LBB7_59
.LBB7_9:
	stb r18+20, r17
	add r3, r18, r0
	add r4, r11, r0
	add r5, r13, r0
	jal r31, match
	beq r1, r17, .LBB7_17
.LBB7_10:
	add r26, r1, r0
	beq r1, r14, .LBB7_17
.LBB7_11:
	addi r22, r22, 1
	ldw r27, r18+12
	beq r23, r25, .LBB7_21
.LBB7_12:
	ldw r1, fp+-912
	bne r23, r1, .LBB7_25
.LBB7_13:
	add r3, r27, r0
	ldw r4, fp+-904
	jal r31, lua_pushvalue
	ldbu r1, r18+20
	seq r3, r1, r17
	sne r4, r11, r17
	sub r4, r17, r4
	sub r3, r17, r3
	xori r5, r1, 1
	and r3, r5, r3
	and r3, r3, r4
	xor r19, r1, r3
	ldw r3, r18+12
	add r4, r19, r0
	ldw r5, fp+-936
	jal r31, luaL_checkstack
	beq r19, r17, .LBB7_16
.LBB7_14:
	addi r20, r0, 0
.LBB7_15:
	addi r3, fp, -368
	add r4, r20, r0
	add r5, r11, r0
	add r6, r26, r0
	jal r31, push_onecapture
	addi r20, r20, 1
	bne r19, r20, .LBB7_15
.LBB7_16:
	addi r5, r0, 1
	addi r6, r0, 0
	add r3, r27, r0
	add r4, r19, r0
	add r7, r6, r0
	jal r31, lua_callk
	ldw r20, fp+-908
	jal r0, .LBB7_52
.LBB7_17:
	ldw r1, r18+4
	bgeu r11, r1, .LBB7_20
.LBB7_18:
	ldw r1, r16+8
	ldw r3, r16+4
	bltu r1, r3, .LBB7_5
.LBB7_19:
	add r3, r16, r0
	add r4, r21, r0
	jal r31, luaL_prepbuffsize
	jal r0, .LBB7_5
.LBB7_20:
	add r1, r17, r0
	jal r0, .LBB7_7
.LBB7_21:
	ldbu r1, r18+20
	beq r1, r17, .LBB7_48
.LBB7_22:
	ldw r5, r18+28
	ldw r19, r18+24
	beq r5, r28, .LBB7_57
.LBB7_23:
	bne r5, r20, .LBB7_49
.LBB7_24:
	add r3, r27, r0
	ldw r4, fp+-948
	jal r31, luaL_error
	add r5, r20, r0
	bne r5, r28, .LBB7_50
	jal r0, .LBB7_51
.LBB7_25:
	addi r4, r0, 3
	addi r28, fp, -80
	add r3, r27, r0
	add r5, r28, r0
	jal r31, lua_tolstring
	add r19, r1, r0
	ldw r5, r28+0
	addi r23, r0, 37
	add r3, r1, r0
	add r4, r23, r0
	jal r31, memchr
	addi r14, r0, 0
	beq r1, r14, .LBB7_47
.LBB7_26:
	add r25, r1, r0
	sub r20, r26, r11
	jal r0, .LBB7_29
.LBB7_27:
	ldbu r1, r25+1
	ldw r3, r19+0
	ldw r4, r19+8
	addi r5, r4, 1
	stw r19+8, r5
	add r3, r3, r4
	stb r3+0, r1
.LBB7_28:
	addi r19, r25, 2
	sub r1, r21, r19
	ldw r3, r28+0
	add r5, r3, r1
	stw r28+0, r5
	addi r4, r0, 37
	add r3, r19, r0
	jal r31, memchr
	add r25, r1, r0
	beq r1, r14, .LBB7_47
.LBB7_29:
	add r21, r19, r0
	sub r5, r25, r19
	addi r19, fp, -896
	add r3, r19, r0
	add r4, r21, r0
	jal r31, luaL_addlstring
	ldbu r3, r25+1
	addi r1, r0, 48
	beq r3, r1, .LBB7_33
.LBB7_30:
	bne r3, r23, .LBB7_34
.LBB7_31:
	ldw r1, r19+8
	ldw r3, r19+4
	bltu r1, r3, .LBB7_27
.LBB7_32:
	addi r3, fp, -896
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
	jal r0, .LBB7_27
.LBB7_33:
	addi r3, fp, -896
	add r4, r11, r0
	add r5, r20, r0
	jal r31, luaL_addlstring
	jal r0, .LBB7_28
.LBB7_34:
	jal r31, isdigit
	beq r1, r14, .LBB7_39
.LBB7_35:
	ldb r1, r25+1
	addi r3, r1, -49
	ldbu r4, r18+20
	bge r3, r4, .LBB7_40
.LBB7_36:
	slli r1, r3, 3
	ldw r3, fp+-900
	add r3, r3, r1
	ldw r1, r3+4
	ldw r19, r3+0
	addi r24, r0, -2
	beq r1, r24, .LBB7_42
.LBB7_37:
	addi r24, r0, -1
	beq r1, r24, .LBB7_46
.LBB7_38:
	add r24, r1, r0
	jal r0, .LBB7_43
.LBB7_39:
	lui r4, %hi(.L.str.61)
	addi r4, r4, %lo(.L.str.61)
	addi r5, r0, 37
	add r3, r27, r0
	jal r31, luaL_error
	jal r0, .LBB7_28
.LBB7_40:
	add r19, r11, r0
	add r24, r20, r0
	beq r3, r14, .LBB7_43
.LBB7_41:
	ldw r3, r18+12
	addi r5, r1, -48
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	jal r31, luaL_error
	add r19, r11, r0
	add r24, r20, r0
	jal r0, .LBB7_43
.LBB7_42:
	ldw r3, r18+12
	ldw r1, r18+0
	sub r1, r19, r1
	addi r4, r1, 1
	jal r31, lua_pushinteger
.LBB7_43:
	addi r3, fp, -896
	addi r1, r0, -2
	bne r24, r1, .LBB7_45
.LBB7_44:
	jal r31, luaL_addvalue
	jal r0, .LBB7_28
.LBB7_45:
	add r4, r19, r0
	add r5, r24, r0
	jal r31, luaL_addlstring
	jal r0, .LBB7_28
.LBB7_46:
	ldw r3, r18+12
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	jal r31, luaL_error
	jal r0, .LBB7_43
.LBB7_47:
	ldw r5, r28+0
	addi r3, fp, -896
	add r4, r19, r0
	jal r31, luaL_addlstring
	ldw r21, fp+-916
	or  r12, r21, r12
	add r14, r26, r0
	add r11, r26, r0
	ldw r24, fp+-924
	add r1, r24, r0
	ldw r23, fp+-920
	ldw r25, fp+-928
	ldw r28, fp+-932
	ldw r20, fp+-908
	jal r0, .LBB7_7
.LBB7_48:
	sub r5, r26, r11
	add r19, r11, r0
.LBB7_49:
	beq r5, r28, .LBB7_51
.LBB7_50:
	ldw r3, r18+12
	add r4, r19, r0
	jal r31, lua_pushlstring
.LBB7_51:
	add r3, r27, r0
	ldw r4, fp+-904
	jal r31, lua_gettable
.LBB7_52:
	add r3, r27, r0
	add r4, r20, r0
	jal r31, lua_toboolean
	beq r1, r17, .LBB7_55
.LBB7_53:
	add r3, r27, r0
	add r4, r20, r0
	jal r31, lua_isstring
	beq r1, r17, .LBB7_58
.LBB7_54:
	add r3, r16, r0
	jal r31, luaL_addvalue
	or  r12, r21, r12
	jal r0, .LBB7_56
.LBB7_55:
	add r3, r27, r0
	add r4, r28, r0
	jal r31, lua_settop
	sub r5, r26, r11
	add r3, r16, r0
	add r4, r11, r0
	jal r31, luaL_addlstring
	or  r12, r17, r12
.LBB7_56:
	add r14, r26, r0
	add r11, r26, r0
	jal r0, .LBB7_6
.LBB7_57:
	ldw r1, r18+0
	sub r1, r19, r1
	addi r4, r1, 1
	add r3, r27, r0
	jal r31, lua_pushinteger
	add r5, r28, r0
	bne r5, r28, .LBB7_50
	jal r0, .LBB7_51
.LBB7_58:
	add r3, r27, r0
	add r4, r20, r0
	jal r31, lua_type
	add r3, r27, r0
	add r4, r1, r0
	jal r31, lua_typename
	add r3, r27, r0
	ldw r4, fp+-944
	add r5, r1, r0
	jal r31, luaL_error
	or  r12, r1, r12
	jal r0, .LBB7_56
.LBB7_59:
	addi r1, r0, 0
	beq r12, r1, .LBB7_61
.LBB7_60:
	ldw r1, r18+4
	sub r5, r1, r11
	addi r12, fp, -896
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaL_addlstring
	add r3, r12, r0
	jal r31, luaL_pushresult
	ldw r11, fp+-940
	jal r0, .LBB7_62
.LBB7_61:
	addi r4, r0, 1
	ldw r11, fp+-940
	add r3, r11, r0
	jal r31, lua_pushvalue
.LBB7_62:
	add r3, r11, r0
	add r4, r22, r0
	jal r31, lua_pushinteger
	addi r1, r0, 2
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
	addi sp, sp, 968
	jalr r0, r31, 0
.Lfunc_end7:
	.size	str_gsub, .Lfunc_end7-str_gsub
                                        # -- End function
	.p2align	2                               # -- Begin function str_len
	.type	str_len,@function
str_len:                                # @str_len
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, lr
	add r11, r3, r0
	addi r12, r0, 1
	addi r13, fp, -20
	add r4, r12, r0
	add r5, r13, r0
	jal r31, luaL_checklstring
	ldw r4, r13+0
	add r3, r11, r0
	jal r31, lua_pushinteger
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end8:
	.size	str_len, .Lfunc_end8-str_len
                                        # -- End function
	.p2align	2                               # -- Begin function str_lower
	.type	str_lower,@function
str_lower:                              # @str_lower
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
	stw fp+-20, lr
	add r13, r3, r0
	addi r4, r0, 1
	addi r11, fp, -24
	add r5, r11, r0
	jal r31, luaL_checklstring
	add r12, r1, r0
	ldw r5, r11+0
	addi r4, fp, -552
	add r3, r13, r0
	jal r31, luaL_buffinitsize
	add r13, r1, r0
	ldw r1, r11+0
	addi r4, r0, 0
	beq r1, r4, .LBB9_3
.LBB9_1:
	addi r14, r0, 0
.LBB9_2:
	add r1, r12, r14
	ldbu r3, r1+0
	jal r31, tolower
	add r3, r13, r14
	stb r3+0, r1
	addi r14, r14, 1
	ldw r4, r11+0
	bltu r14, r4, .LBB9_2
.LBB9_3:
	addi r3, fp, -552
	jal r31, luaL_pushresultsize
	addi r1, r0, 1
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 568
	jalr r0, r31, 0
.Lfunc_end9:
	.size	str_lower, .Lfunc_end9-str_lower
                                        # -- End function
	.p2align	2                               # -- Begin function str_match
	.type	str_match,@function
str_match:                              # @str_match
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r4, r0, 0
	jal r31, str_find_aux
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end10:
	.size	str_match, .Lfunc_end10-str_match
                                        # -- End function
	.p2align	2                               # -- Begin function str_rep
	.type	str_rep,@function
str_rep:                                # @str_rep
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
	stw fp+-52, lr
	add r18, r3, r0
	addi r11, r0, 1
	addi r12, fp, -56
	add r4, r11, r0
	add r5, r12, r0
	jal r31, luaL_checklstring
	add r13, r1, r0
	addi r14, r0, 2
	add r3, r18, r0
	add r4, r14, r0
	jal r31, luaL_checkinteger
	add r19, r1, r0
	lui r5, %hi(.L.str.37)
	addi r5, r5, %lo(.L.str.37)
	addi r4, r0, 3
	addi r15, fp, -60
	add r3, r18, r0
	add r6, r15, r0
	jal r31, luaL_optlstring
	addi r20, r0, 0
	ble r19, r20, .LBB11_3
.LBB11_1:
	add r16, r1, r0
	ldw r21, r12+0
	ldw r17, r15+0
	add r22, r17, r21
	sltu r1, r22, r17
	beq r1, r20, .LBB11_4
.LBB11_2:
	lui r4, %hi(.L.str.62)
	addi r4, r4, %lo(.L.str.62)
	add r3, r18, r0
	jal r31, luaL_error
	add r11, r1, r0
	jal r0, .LBB11_7
.LBB11_3:
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	add r3, r18, r0
	jal r31, lua_pushstring
	jal r0, .LBB11_7
.LBB11_4:
	lui r1, 524288
	addi r3, r1, -1
	add r4, r19, r0
	jal r31, __udivsi3
	bgtu r22, r1, .LBB11_2
.LBB11_5:
	mul r1, r21, r19
	addi r3, r19, -1
	mul r3, r17, r3
	add r17, r3, r1
	addi r4, fp, -588
	add r3, r18, r0
	add r5, r17, r0
	jal r31, luaL_buffinitsize
	add r18, r1, r0
	bne r19, r11, .LBB11_8
.LBB11_6:
	ldw r5, r12+0
	add r3, r18, r0
	add r4, r13, r0
	jal r31, memcpy
	addi r3, fp, -588
	add r4, r17, r0
	jal r31, luaL_pushresultsize
.LBB11_7:
	add r1, r11, r0
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
	addi sp, sp, 600
	jalr r0, r31, 0
.LBB11_8:
	addi r19, r19, 1
	jal r0, .LBB11_10
.LBB11_9:
	addi r19, r19, -1
	ble r19, r14, .LBB11_6
.LBB11_10:
	ldw r5, r12+0
	add r3, r18, r0
	add r4, r13, r0
	jal r31, memcpy
	ldw r1, r12+0
	add r18, r18, r1
	ldw r5, r15+0
	beq r5, r20, .LBB11_9
.LBB11_11:
	add r3, r18, r0
	add r4, r16, r0
	jal r31, memcpy
	ldw r1, r15+0
	add r18, r18, r1
	jal r0, .LBB11_9
.Lfunc_end11:
	.size	str_rep, .Lfunc_end11-str_rep
                                        # -- End function
	.p2align	2                               # -- Begin function str_reverse
	.type	str_reverse,@function
str_reverse:                            # @str_reverse
# %bb.0:
	addi sp, sp, -568
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 568
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, lr
	add r13, r3, r0
	addi r4, r0, 1
	addi r11, fp, -20
	add r5, r11, r0
	jal r31, luaL_checklstring
	add r12, r1, r0
	ldw r5, r11+0
	addi r4, fp, -548
	add r3, r13, r0
	jal r31, luaL_buffinitsize
	ldw r6, r11+0
	addi r4, r0, 0
	beq r6, r4, .LBB12_3
.LBB12_1:
	addi r3, r12, -1
	addi r5, r0, 0
	add r4, r6, r0
.LBB12_2:
	add r4, r3, r4
	ldbu r4, r4+0
	add r6, r1, r5
	stb r6+0, r4
	addi r5, r5, 1
	ldw r4, r11+0
	addi r3, r3, -1
	bltu r5, r4, .LBB12_2
.LBB12_3:
	addi r3, fp, -548
	jal r31, luaL_pushresultsize
	addi r1, r0, 1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 568
	jalr r0, r31, 0
.Lfunc_end12:
	.size	str_reverse, .Lfunc_end12-str_reverse
                                        # -- End function
	.p2align	2                               # -- Begin function str_sub
	.type	str_sub,@function
str_sub:                                # @str_sub
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
	addi r14, fp, -32
	add r4, r12, r0
	add r5, r14, r0
	jal r31, luaL_checklstring
	add r13, r1, r0
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_checkinteger
	ldw r16, r14+0
	addi r15, r0, 0
	ble r1, r15, .LBB13_2
.LBB13_1:
	add r12, r1, r0
	jal r0, .LBB13_5
.LBB13_2:
	beq r1, r15, .LBB13_5
.LBB13_3:
	sub r3, r15, r16
	blt r1, r3, .LBB13_5
.LBB13_4:
	add r1, r16, r1
	addi r12, r1, 1
.LBB13_5:
	addi r4, r0, 3
	addi r14, r0, -1
	add r3, r11, r0
	add r5, r14, r0
	jal r31, luaL_optinteger
	ble r1, r16, .LBB13_7
.LBB13_6:
	add r15, r16, r0
	jal r0, .LBB13_11
.LBB13_7:
	ble r1, r14, .LBB13_9
.LBB13_8:
	add r15, r1, r0
	jal r0, .LBB13_11
.LBB13_9:
	sub r3, r15, r16
	blt r1, r3, .LBB13_11
.LBB13_10:
	add r1, r16, r1
	addi r15, r1, 1
.LBB13_11:
	bgeu r15, r12, .LBB13_13
.LBB13_12:
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	add r3, r11, r0
	jal r31, lua_pushstring
	jal r0, .LBB13_14
.LBB13_13:
	add r1, r13, r12
	addi r4, r1, -1
	sub r1, r15, r12
	addi r5, r1, 1
	add r3, r11, r0
	jal r31, lua_pushlstring
.LBB13_14:
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
	.size	str_sub, .Lfunc_end13-str_sub
                                        # -- End function
	.p2align	2                               # -- Begin function str_upper
	.type	str_upper,@function
str_upper:                              # @str_upper
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
	stw fp+-20, lr
	add r13, r3, r0
	addi r4, r0, 1
	addi r11, fp, -24
	add r5, r11, r0
	jal r31, luaL_checklstring
	add r12, r1, r0
	ldw r5, r11+0
	addi r4, fp, -552
	add r3, r13, r0
	jal r31, luaL_buffinitsize
	add r13, r1, r0
	ldw r1, r11+0
	addi r4, r0, 0
	beq r1, r4, .LBB14_3
.LBB14_1:
	addi r14, r0, 0
.LBB14_2:
	add r1, r12, r14
	ldbu r3, r1+0
	jal r31, toupper
	add r3, r13, r14
	stb r3+0, r1
	addi r14, r14, 1
	ldw r4, r11+0
	bltu r14, r4, .LBB14_2
.LBB14_3:
	addi r3, fp, -552
	jal r31, luaL_pushresultsize
	addi r1, r0, 1
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 568
	jalr r0, r31, 0
.Lfunc_end14:
	.size	str_upper, .Lfunc_end14-str_upper
                                        # -- End function
	.p2align	2                               # -- Begin function str_pack
	.type	str_pack,@function
str_pack:                               # @str_pack
# %bb.0:
	addi sp, sp, -664
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 664
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
	add r27, r3, r0
	addi r12, r0, 1
	addi r13, r0, 0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, luaL_checklstring
	addi r16, fp, -620
	stw r16+0, r1
	addi r14, fp, -616
	stw r14+0, r27
	stw r14+4, r12
	stw r14+8, r12
	add r3, r27, r0
	jal r31, lua_pushnil
	addi r15, fp, -604
	add r3, r27, r0
	add r4, r15, r0
	jal r31, luaL_buffinit
	ldw r1, r16+0
	ldbu r1, r1+0
	beq r1, r13, .LBB15_66
.LBB15_1:
	addi r17, fp, -624
	addi r18, fp, -628
	addi r26, r0, -1
	lui r11, %hi(.LJTI15_0)
	addi r11, r11, %lo(.LJTI15_0)
	addi r19, r0, 3
	lui r1, %hi(.L.str.63)
	addi r1, r1, %lo(.L.str.63)
	stw fp+-648, r1
	addi r1, r0, 2
	stw fp+-640, r1
	add r24, r12, r0
	add r20, r13, r0
	stw fp+-644, r11
	jal r0, .LBB15_7
.LBB15_2:
	addi r4, fp, -636
	addi r5, r0, 8
.LBB15_3:
	add r3, r1, r0
	jal r31, memcpy
.LBB15_4:
	ldw r1, r23+8
	add r1, r1, r22
	stw r23+8, r1
.LBB15_5:
	add r24, r21, r0
.LBB15_6:
	ldw r1, r16+0
	ldbu r1, r1+0
	addi r3, r0, 0
	beq r1, r3, .LBB15_66
.LBB15_7:
	add r3, r14, r0
	add r4, r20, r0
	add r5, r16, r0
	add r6, r17, r0
	add r7, r18, r0
	jal r31, getdetails
	add r23, r1, r0
	ldw r21, r18+0
	ldw r22, r17+0
	blt r21, r12, .LBB15_13
.LBB15_8:
	add r25, r21, r0
	jal r0, .LBB15_10
.LBB15_9:
	ldw r1, r15+0
	ldw r3, r15+8
	addi r4, r3, 1
	stw r15+8, r4
	add r1, r1, r3
	stb r1+0, r13
	addi r25, r25, -1
	ble r25, r13, .LBB15_12
.LBB15_10:
	ldw r1, r15+8
	ldw r3, r15+4
	bltu r1, r3, .LBB15_9
.LBB15_11:
	add r3, r15, r0
	add r4, r12, r0
	jal r31, luaL_prepbuffsize
	jal r0, .LBB15_9
.LBB15_12:
	add r1, r26, r0
	jal r0, .LBB15_14
.LBB15_13:
	addi r1, r21, -1
.LBB15_14:
	add r3, r21, r20
	add r20, r3, r22
	stw r18+0, r1
	addi r21, r24, 1
	slli r1, r23, 2
	add r1, r11, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB15_15:
	add r3, r27, r0
	add r4, r21, r0
	jal r31, luaL_checkinteger
	add r23, r1, r0
	bgt r22, r19, .LBB15_18
.LBB15_16:
	slli r1, r22, 3
	addi r1, r1, -1
	sll r1, r12, r1
	sub r3, r13, r1
	blt r23, r3, .LBB15_65
.LBB15_17:
	bge r23, r1, .LBB15_65
.LBB15_18:
	ldw r24, r14+4
	add r3, r15, r0
	add r4, r22, r0
	jal r31, luaL_prepbuffsize
	seq r3, r24, r13
	addi r4, r22, -1
	sub r3, r13, r3
	and r4, r4, r3
	add r4, r1, r4
	stb r4+0, r23
	ldw r4, fp+-640
	blt r22, r4, .LBB15_21
.LBB15_19:
	addi r4, r22, -2
	add r5, r12, r0
	add r6, r23, r0
.LBB15_20:
	srli r6, r6, 8
	xor r7, r4, r5
	and r7, r7, r3
	xor r7, r5, r7
	add r7, r1, r7
	stb r7+0, r6
	addi r5, r5, 1
	addi r4, r4, -1
	bne r4, r26, .LBB15_20
.LBB15_21:
	addi r3, r0, 5
	blt r22, r3, .LBB15_25
.LBB15_22:
	addi r3, r0, -1
	bgt r23, r3, .LBB15_25
.LBB15_23:
	addi r4, r22, -5
	addi r5, r0, 4
.LBB15_24:
	addi r6, r0, 0
	seq r7, r24, r6
	xor r8, r4, r5
	sub r6, r6, r7
	and r6, r8, r6
	xor r6, r5, r6
	add r6, r1, r6
	addi r7, r0, 255
	stb r6+0, r7
	addi r5, r5, 1
	addi r4, r4, -1
	bne r4, r3, .LBB15_24
.LBB15_25:
	ldw r1, r15+8
	add r1, r1, r22
	stw r15+8, r1
	jal r0, .LBB15_5
.LBB15_26:
	add r3, r27, r0
	add r4, r21, r0
	jal r31, luaL_checknumber
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	addi r24, fp, -636
	stw r24+0, r1
	addi r23, fp, -604
	addi r4, r0, 4
	add r3, r23, r0
	jal r31, luaL_prepbuffsize
	ldw r3, r14+4
	addi r4, r0, 1
	bne r3, r4, .LBB15_59
.LBB15_27:
	addi r4, fp, -636
	addi r5, r0, 4
	jal r0, .LBB15_3
.LBB15_28:
	addi r22, fp, -636
	add r3, r27, r0
	add r4, r21, r0
	add r5, r22, r0
	jal r31, luaL_checklstring
	add r23, r1, r0
	add r3, r1, r0
	jal r31, strlen
	ldw r3, r22+0
	bne r1, r3, .LBB15_62
.LBB15_29:
	ldw r5, r22+0
	add r3, r15, r0
	add r4, r23, r0
	jal r31, luaL_addlstring
	ldw r1, r15+8
	ldw r3, r15+4
	bltu r1, r3, .LBB15_31
.LBB15_30:
	addi r3, fp, -604
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
.LBB15_31:
	ldw r1, r15+0
	ldw r3, r15+8
	addi r4, r3, 1
	stw r15+8, r4
	add r1, r1, r3
	addi r3, r0, 0
	stb r1+0, r3
	ldw r1, r22+0
	add r1, r20, r1
	addi r20, r1, 1
	jal r0, .LBB15_5
.LBB15_32:
	add r3, r27, r0
	add r4, r21, r0
	jal r31, luaL_checkinteger
	add r23, r1, r0
	addi r1, r0, 3
	bgt r22, r1, .LBB15_34
.LBB15_33:
	slli r1, r22, 3
	srl r1, r23, r1
	addi r3, r0, 0
	bne r1, r3, .LBB15_64
.LBB15_34:
	ldw r25, r14+4
	addi r24, fp, -604
	add r3, r24, r0
	add r4, r22, r0
	jal r31, luaL_prepbuffsize
	addi r3, r0, 0
	seq r4, r25, r3
	addi r5, r22, -1
	sub r3, r3, r4
	and r4, r5, r3
	add r4, r1, r4
	stb r4+0, r23
	ldw r4, fp+-640
	blt r22, r4, .LBB15_37
.LBB15_35:
	addi r4, r22, -2
	addi r5, r0, 1
.LBB15_36:
	srli r23, r23, 8
	xor r6, r4, r5
	and r6, r6, r3
	xor r6, r5, r6
	add r6, r1, r6
	stb r6+0, r23
	addi r5, r5, 1
	addi r4, r4, -1
	addi r6, r0, -1
	bne r4, r6, .LBB15_36
.LBB15_37:
	ldw r1, r24+8
	add r1, r1, r22
	stw r24+8, r1
	jal r0, .LBB15_5
.LBB15_38:
	addi r23, fp, -636
	add r3, r27, r0
	add r4, r21, r0
	add r5, r23, r0
	jal r31, luaL_checklstring
	ldw r3, r23+0
	bgtu r3, r22, .LBB15_63
.LBB15_39:
	ldw r5, r23+0
	addi r24, fp, -604
	add r3, r24, r0
	add r4, r1, r0
	jal r31, luaL_addlstring
	jal r0, .LBB15_41
.LBB15_40:
	ldw r1, r24+0
	ldw r3, r24+8
	addi r4, r3, 1
	stw r24+8, r4
	add r1, r1, r3
	addi r3, r0, 0
	stb r1+0, r3
.LBB15_41:
	ldw r1, r23+0
	addi r3, r1, 1
	stw r23+0, r3
	bgeu r1, r22, .LBB15_5
.LBB15_42:
	ldw r1, r24+8
	ldw r3, r24+4
	bltu r1, r3, .LBB15_40
.LBB15_43:
	addi r3, fp, -604
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
	jal r0, .LBB15_40
.LBB15_44:
	add r3, r27, r0
	add r4, r21, r0
	jal r31, luaL_checknumber
	addi r24, fp, -636
	stw r24+4, r2
	stw r24+0, r1
	addi r23, fp, -604
	addi r4, r0, 8
	add r3, r23, r0
	jal r31, luaL_prepbuffsize
	ldw r3, r14+4
	addi r4, r0, 1
	beq r3, r4, .LBB15_2
.LBB15_45:
	addi r3, r0, 7
.LBB15_46:
	add r4, r1, r3
	addi r5, r24, 1
	ldbu r6, r24+0
	stb r4+0, r6
	addi r3, r3, -1
	addi r4, r0, -1
	add r24, r5, r0
	bne r3, r4, .LBB15_46
	jal r0, .LBB15_4
.LBB15_47:
	add r3, r27, r0
	add r4, r21, r0
	jal r31, luaL_checknumber
	addi r24, fp, -636
	stw r24+4, r2
	stw r24+0, r1
	addi r23, fp, -604
	addi r4, r0, 8
	add r3, r23, r0
	jal r31, luaL_prepbuffsize
	ldw r3, r14+4
	addi r4, r0, 1
	beq r3, r4, .LBB15_2
.LBB15_48:
	addi r3, r0, 7
.LBB15_49:
	add r4, r1, r3
	addi r5, r24, 1
	ldbu r6, r24+0
	stb r4+0, r6
	addi r3, r3, -1
	addi r4, r0, -1
	add r24, r5, r0
	bne r3, r4, .LBB15_49
	jal r0, .LBB15_4
.LBB15_50:
	ldw r1, r15+8
	ldw r3, r15+4
	bltu r1, r3, .LBB15_52
.LBB15_51:
	addi r3, fp, -604
	addi r4, r0, 1
	jal r31, luaL_prepbuffsize
.LBB15_52:
	ldw r1, r15+0
	ldw r3, r15+8
	addi r4, r3, 1
	stw r15+8, r4
	add r1, r1, r3
	addi r3, r0, 0
	stb r1+0, r3
	jal r0, .LBB15_6
.LBB15_53:
	addi r23, fp, -636
	add r3, r27, r0
	add r4, r21, r0
	add r5, r23, r0
	jal r31, luaL_checklstring
	add r24, r1, r0
	add r11, r19, r0
	bgt r22, r19, .LBB15_55
.LBB15_54:
	ldw r1, r23+0
	slli r3, r22, 3
	srl r1, r1, r3
	addi r3, r0, 0
	bne r1, r3, .LBB15_61
.LBB15_55:
	ldw r28, r23+0
	ldw r19, r14+4
	addi r25, fp, -604
	add r3, r25, r0
	add r4, r22, r0
	jal r31, luaL_prepbuffsize
	addi r3, r0, 0
	seq r4, r19, r3
	addi r5, r22, -1
	sub r3, r3, r4
	and r4, r5, r3
	add r4, r1, r4
	stb r4+0, r28
	ldw r4, fp+-640
	blt r22, r4, .LBB15_58
.LBB15_56:
	addi r4, r22, -2
	addi r5, r0, 1
.LBB15_57:
	srli r28, r28, 8
	xor r6, r4, r5
	and r6, r6, r3
	xor r6, r5, r6
	add r6, r1, r6
	stb r6+0, r28
	addi r5, r5, 1
	addi r4, r4, -1
	addi r6, r0, -1
	bne r4, r6, .LBB15_57
.LBB15_58:
	ldw r1, r25+8
	add r1, r1, r22
	stw r25+8, r1
	ldw r5, r23+0
	add r3, r25, r0
	add r4, r24, r0
	jal r31, luaL_addlstring
	ldw r1, r23+0
	add r20, r1, r20
	add r24, r21, r0
	add r19, r11, r0
	ldw r11, fp+-644
	jal r0, .LBB15_6
.LBB15_59:
	add r3, r19, r0
.LBB15_60:
	add r4, r1, r3
	addi r5, r24, 1
	ldbu r6, r24+0
	stb r4+0, r6
	addi r3, r3, -1
	addi r4, r0, -1
	add r24, r5, r0
	bne r3, r4, .LBB15_60
	jal r0, .LBB15_4
.LBB15_61:
	lui r5, %hi(.L.str.66)
	addi r5, r5, %lo(.L.str.66)
	add r3, r27, r0
	add r4, r21, r0
	jal r31, luaL_argerror
	jal r0, .LBB15_55
.LBB15_62:
	lui r5, %hi(.L.str.40)
	addi r5, r5, %lo(.L.str.40)
	add r3, r27, r0
	add r4, r21, r0
	jal r31, luaL_argerror
	jal r0, .LBB15_29
.LBB15_63:
	lui r5, %hi(.L.str.65)
	addi r5, r5, %lo(.L.str.65)
	add r3, r27, r0
	add r4, r21, r0
	add r24, r1, r0
	jal r31, luaL_argerror
	add r1, r24, r0
	jal r0, .LBB15_39
.LBB15_64:
	lui r5, %hi(.L.str.64)
	addi r5, r5, %lo(.L.str.64)
	add r3, r27, r0
	add r4, r21, r0
	jal r31, luaL_argerror
	jal r0, .LBB15_34
.LBB15_65:
	add r3, r27, r0
	add r4, r21, r0
	ldw r5, fp+-648
	jal r31, luaL_argerror
	jal r0, .LBB15_18
.LBB15_66:
	addi r3, fp, -604
	jal r31, luaL_pushresult
	addi r1, r0, 1
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
	addi sp, sp, 664
	jalr r0, r31, 0
.Lfunc_end15:
	.size	str_pack, .Lfunc_end15-str_pack
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI15_0:
	.word	.LBB15_15
	.word	.LBB15_32
	.word	.LBB15_26
	.word	.LBB15_44
	.word	.LBB15_47
	.word	.LBB15_38
	.word	.LBB15_53
	.word	.LBB15_28
	.word	.LBB15_50
	.word	.LBB15_6
	.word	.LBB15_6
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function str_packsize
	.type	str_packsize,@function
str_packsize:                           # @str_packsize
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
	stw fp+-56, lr
	add r11, r3, r0
	addi r14, r0, 1
	addi r13, r0, 0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, luaL_checklstring
	addi r12, fp, -72
	stw r12+0, r1
	addi r3, fp, -68
	stw r3+0, r11
	stw r3+4, r14
	stw r3+8, r14
	ldbu r1, r1+0
	beq r1, r13, .LBB16_7
.LBB16_1:
	addi r20, r0, 0
	addi r14, fp, -68
	addi r15, fp, -76
	addi r16, fp, -80
	addi r21, r0, -2
	lui r1, 524288
	addi r22, r1, -1
	lui r17, %hi(.L.str.73)
	addi r17, r17, %lo(.L.str.73)
	addi r18, r0, 1
	lui r19, %hi(.L.str.72)
	addi r19, r19, %lo(.L.str.72)
	add r13, r20, r0
.LBB16_2:
	add r3, r14, r0
	add r4, r13, r0
	add r5, r12, r0
	add r6, r15, r0
	add r7, r16, r0
	jal r31, getdetails
	addi r1, r1, -8
	bgeu r1, r21, .LBB16_5
.LBB16_3:
	ldw r1, r16+0
	ldw r3, r15+0
	add r23, r3, r1
	stw r15+0, r23
	xor r1, r23, r22
	bgtu r13, r1, .LBB16_6
.LBB16_4:
	add r13, r23, r13
	ldw r1, r12+0
	ldbu r1, r1+0
	bne r1, r20, .LBB16_2
	jal r0, .LBB16_7
.LBB16_5:
	add r3, r11, r0
	add r4, r18, r0
	add r5, r19, r0
	jal r31, luaL_argerror
	jal r0, .LBB16_3
.LBB16_6:
	add r3, r11, r0
	add r4, r18, r0
	add r5, r17, r0
	jal r31, luaL_argerror
	jal r0, .LBB16_4
.LBB16_7:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_pushinteger
	addi r1, r0, 1
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
	addi sp, sp, 88
	jalr r0, r31, 0
.Lfunc_end16:
	.size	str_packsize, .Lfunc_end16-str_packsize
                                        # -- End function
	.p2align	2                               # -- Begin function str_unpack
	.type	str_unpack,@function
str_unpack:                             # @str_unpack
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
	add r11, r3, r0
	addi r17, r0, 1
	addi r12, r0, 0
	add r4, r17, r0
	add r5, r12, r0
	jal r31, luaL_checklstring
	addi r14, fp, -92
	stw r14+0, r1
	addi r4, r0, 2
	addi r13, fp, -96
	add r3, r11, r0
	add r5, r13, r0
	jal r31, luaL_checklstring
	add r21, r1, r0
	addi r26, r0, 3
	add r3, r11, r0
	add r4, r26, r0
	add r5, r17, r0
	jal r31, luaL_optinteger
	ldw r3, r13+0
	ble r1, r12, .LBB17_2
.LBB17_1:
	add r17, r1, r0
	jal r0, .LBB17_5
.LBB17_2:
	beq r1, r12, .LBB17_5
.LBB17_3:
	sub r4, r12, r3
	blt r1, r4, .LBB17_5
.LBB17_4:
	add r1, r1, r3
	addi r17, r1, 1
.LBB17_5:
	addi r25, r17, -1
	bgtu r25, r3, .LBB17_56
.LBB17_6:
	addi r16, fp, -88
	stw r16+0, r11
	addi r18, r0, 1
	stw r16+4, r18
	stw r16+8, r18
	ldw r1, r14+0
	ldbu r1, r1+0
	beq r1, r12, .LBB17_55
.LBB17_7:
	addi r17, fp, -92
	addi r18, fp, -100
	addi r19, fp, -104
	lui r1, %hi(.L.str.75)
	addi r1, r1, %lo(.L.str.75)
	stw fp+-136, r1
	addi r1, r0, 2
	stw fp+-116, r1
	lui r22, %hi(.L.str.76)
	addi r22, r22, %lo(.L.str.76)
	lui r1, %hi(.LJTI17_0)
	addi r1, r1, %lo(.LJTI17_0)
	stw fp+-120, r1
	addi r1, r0, 4
	stw fp+-124, r1
	addi r28, r0, 1
	addi r20, r0, -1
	add r14, r12, r0
	stw fp+-128, r21
	stw fp+-132, r26
	jal r0, .LBB17_12
.LBB17_8:
	ldw r21, fp+-128
	ldw r26, fp+-132
.LBB17_9:
	add r3, r11, r0
	jal r31, lua_pushinteger
.LBB17_10:
	add r14, r27, r0
.LBB17_11:
	add r25, r15, r23
	ldw r1, r17+0
	ldbu r1, r1+0
	addi r3, r0, 0
	beq r1, r3, .LBB17_54
.LBB17_12:
	add r3, r16, r0
	add r4, r25, r0
	add r5, r17, r0
	add r6, r18, r0
	add r7, r19, r0
	jal r31, getdetails
	add r24, r1, r0
	ldw r15, r19+0
	ldw r23, r18+0
	add r1, r23, r15
	ldw r3, r13+0
	sub r3, r3, r25
	bgtu r1, r3, .LBB17_51
.LBB17_13:
	add r15, r15, r25
	add r3, r11, r0
	ldw r4, fp+-116
	add r5, r22, r0
	jal r31, luaL_checkstack
	addi r27, r14, 1
	slli r1, r24, 2
	ldw r3, fp+-120
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB17_14:
	add r25, r21, r15
	ldw r1, r16+4
	ldw r3, fp+-124
	slt r3, r23, r3
	sub r3, r12, r3
	xori r4, r23, 4
	and r3, r4, r3
	xori r14, r3, 4
	add r4, r12, r0
	blt r23, r28, .LBB17_17
.LBB17_15:
	addi r3, r14, 1
	sub r5, r23, r14
	seq r4, r1, r12
	sub r6, r12, r4
	add r4, r12, r0
.LBB17_16:
	addi r7, r3, -2
	slli r4, r4, 8
	xor r8, r5, r7
	and r8, r8, r6
	xor r7, r7, r8
	add r7, r25, r7
	ldbu r7, r7+0
	or  r4, r4, r7
	addi r3, r3, -1
	addi r5, r5, 1
	bgt r3, r28, .LBB17_16
.LBB17_17:
	bgt r23, r26, .LBB17_36
.LBB17_18:
	bne r24, r12, .LBB17_9
.LBB17_19:
	slli r1, r23, 3
	addi r1, r1, -1
	sll r1, r28, r1
	xor r3, r4, r1
	sub r4, r3, r1
	jal r0, .LBB17_9
.LBB17_20:
	add r25, r21, r15
	ldw r14, r16+4
	ldw r1, fp+-124
	slt r1, r23, r1
	addi r24, r0, 0
	sub r1, r24, r1
	xori r3, r23, 4
	and r1, r3, r1
	xori r26, r1, 4
	blt r23, r28, .LBB17_23
.LBB17_21:
	addi r1, r26, 1
	sub r3, r23, r26
	addi r4, r0, 0
	add r24, r4, r0
.LBB17_22:
	seq r5, r14, r4
	addi r6, r1, -2
	slli r7, r24, 8
	sub r5, r4, r5
	xor r8, r3, r6
	and r5, r8, r5
	xor r5, r6, r5
	add r5, r25, r5
	ldbu r5, r5+0
	or  r24, r7, r5
	addi r1, r1, -1
	addi r3, r3, 1
	bgt r1, r28, .LBB17_22
.LBB17_23:
	addi r1, r0, 5
	bge r23, r1, .LBB17_41
.LBB17_24:
	ldw r1, r13+0
	add r3, r15, r23
	sub r1, r1, r3
	bgtu r24, r1, .LBB17_52
.LBB17_25:
	add r4, r25, r23
	add r3, r11, r0
	add r5, r24, r0
	jal r31, lua_pushlstring
	add r15, r24, r15
	add r14, r27, r0
	ldw r21, fp+-128
	ldw r26, fp+-132
	jal r0, .LBB17_11
.LBB17_26:
	add r4, r21, r15
	ldw r1, r16+4
	beq r1, r28, .LBB17_32
.LBB17_27:
	addi r1, r0, 7
.LBB17_28:
	addi r3, fp, -112
	add r3, r3, r1
	addi r5, r4, 1
	ldbu r4, r4+0
	stb r3+0, r4
	addi r1, r1, -1
	add r4, r5, r0
	bne r1, r20, .LBB17_28
	jal r0, .LBB17_50
.LBB17_29:
	add r4, r21, r15
	ldw r1, r16+4
	bne r1, r28, .LBB17_45
.LBB17_30:
	addi r3, fp, -112
	addi r5, r0, 4
	jal r31, memcpy
	jal r0, .LBB17_47
.LBB17_31:
	add r4, r21, r15
	ldw r1, r16+4
	bne r1, r28, .LBB17_48
.LBB17_32:
	addi r3, fp, -112
	addi r5, r0, 8
	jal r31, memcpy
	jal r0, .LBB17_50
.LBB17_33:
	add r24, r21, r15
	add r3, r24, r0
	jal r31, strlen
	add r25, r1, r0
	add r1, r1, r15
	ldw r3, r13+0
	bgeu r1, r3, .LBB17_53
.LBB17_34:
	add r3, r11, r0
	add r4, r24, r0
	add r5, r25, r0
	jal r31, lua_pushlstring
	add r1, r15, r25
	addi r15, r1, 1
	jal r0, .LBB17_10
.LBB17_35:
	add r4, r21, r15
	add r3, r11, r0
	add r5, r23, r0
	jal r31, lua_pushlstring
	jal r0, .LBB17_10
.LBB17_36:
	ldw r3, fp+-124
	beq r23, r3, .LBB17_9
.LBB17_37:
	seq r3, r24, r12
	sub r3, r12, r3
	srai r5, r4, 31
	and r3, r5, r3
	andi r24, r3, 255
	xor r3, r14, r20
	add r21, r3, r23
	seq r1, r1, r12
	sub r26, r12, r1
.LBB17_38:
	xor r1, r21, r14
	and r1, r1, r26
	xor r1, r14, r1
	add r1, r25, r1
	ldbu r1, r1+0
	bne r24, r1, .LBB17_40
.LBB17_39:
	addi r14, r14, 1
	addi r21, r21, -1
	blt r14, r23, .LBB17_38
	jal r0, .LBB17_8
.LBB17_40:
	lui r1, %hi(.L.str.78)
	addi r1, r1, %lo(.L.str.78)
	add r3, r11, r0
	stw fp+-140, r4
	add r4, r1, r0
	add r5, r23, r0
	jal r31, luaL_error
	ldw r4, fp+-140
	jal r0, .LBB17_39
.LBB17_41:
	xor r1, r26, r20
	add r21, r1, r23
.LBB17_42:
	addi r1, r0, 0
	seq r3, r14, r1
	xor r4, r21, r26
	sub r3, r1, r3
	and r3, r4, r3
	xor r3, r26, r3
	add r3, r25, r3
	ldbu r3, r3+0
	bne r3, r1, .LBB17_44
.LBB17_43:
	addi r26, r26, 1
	addi r21, r21, -1
	bne r21, r20, .LBB17_42
	jal r0, .LBB17_24
.LBB17_44:
	lui r4, %hi(.L.str.78)
	addi r4, r4, %lo(.L.str.78)
	add r3, r11, r0
	add r5, r23, r0
	jal r31, luaL_error
	jal r0, .LBB17_43
.LBB17_45:
	add r1, r26, r0
.LBB17_46:
	addi r3, fp, -112
	add r3, r3, r1
	addi r5, r4, 1
	ldbu r4, r4+0
	stb r3+0, r4
	addi r1, r1, -1
	add r4, r5, r0
	bne r1, r20, .LBB17_46
.LBB17_47:
	addi r1, fp, -112
	ldw r1, r1+0
	fcvt.d.s r6, r1
	add r3, r11, r0
	add r5, r6, r0
	add r6, r7, r0
	jal r31, lua_pushnumber
	jal r0, .LBB17_10
.LBB17_48:
	addi r1, r0, 7
.LBB17_49:
	addi r3, fp, -112
	add r3, r3, r1
	addi r5, r4, 1
	ldbu r4, r4+0
	stb r3+0, r4
	addi r1, r1, -1
	add r4, r5, r0
	bne r1, r20, .LBB17_49
.LBB17_50:
	addi r1, fp, -112
	ldw r6, r1+4
	ldw r5, r1+0
	add r3, r11, r0
	jal r31, lua_pushnumber
	jal r0, .LBB17_10
.LBB17_51:
	add r3, r11, r0
	ldw r4, fp+-116
	ldw r5, fp+-136
	jal r31, luaL_argerror
	jal r0, .LBB17_13
.LBB17_52:
	lui r5, %hi(.L.str.75)
	addi r5, r5, %lo(.L.str.75)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB17_25
.LBB17_53:
	lui r5, %hi(.L.str.77)
	addi r5, r5, %lo(.L.str.77)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB17_34
.LBB17_54:
	addi r17, r25, 1
	addi r18, r14, 1
.LBB17_55:
	add r3, r11, r0
	add r4, r17, r0
	jal r31, lua_pushinteger
	add r1, r18, r0
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
	addi sp, sp, 152
	jalr r0, r31, 0
.LBB17_56:
	lui r5, %hi(.L.str.74)
	addi r5, r5, %lo(.L.str.74)
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB17_6
.Lfunc_end17:
	.size	str_unpack, .Lfunc_end17-str_unpack
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI17_0:
	.word	.LBB17_14
	.word	.LBB17_14
	.word	.LBB17_29
	.word	.LBB17_31
	.word	.LBB17_26
	.word	.LBB17_35
	.word	.LBB17_20
	.word	.LBB17_33
	.word	.LBB17_11
	.word	.LBB17_11
	.word	.LBB17_11
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function writer
	.type	writer,@function
writer:                                 # @writer
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
	ldw r1, r6+0
	addi r7, r0, 0
	addi r11, r6, 4
	bne r1, r7, .LBB18_2
.LBB18_1:
	addi r1, r0, 1
	stw r6+0, r1
	add r12, r4, r0
	add r4, r11, r0
	add r13, r5, r0
	jal r31, luaL_buffinit
	add r4, r12, r0
	add r5, r13, r0
.LBB18_2:
	add r3, r11, r0
	jal r31, luaL_addlstring
	addi r1, r0, 0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end18:
	.size	writer, .Lfunc_end18-writer
                                        # -- End function
	.p2align	2                               # -- Begin function str_find_aux
	.type	str_find_aux,@function
str_find_aux:                           # @str_find_aux
# %bb.0:
	addi sp, sp, -360
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 360
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
	add r14, r4, r0
	add r11, r3, r0
	addi r18, r0, 1
	addi r17, fp, -64
	add r4, r18, r0
	add r5, r17, r0
	jal r31, luaL_checklstring
	add r12, r1, r0
	addi r15, r0, 2
	addi r16, fp, -68
	add r3, r11, r0
	add r4, r15, r0
	add r5, r16, r0
	jal r31, luaL_checklstring
	add r13, r1, r0
	addi r4, r0, 3
	add r3, r11, r0
	add r5, r18, r0
	jal r31, luaL_optinteger
	ldw r3, r17+0
	addi r22, r0, 0
	ble r1, r22, .LBB19_2
.LBB19_1:
	add r18, r1, r0
	jal r0, .LBB19_5
.LBB19_2:
	beq r1, r22, .LBB19_5
.LBB19_3:
	sub r4, r22, r3
	blt r1, r4, .LBB19_5
.LBB19_4:
	add r1, r1, r3
	addi r18, r1, 1
.LBB19_5:
	addi r20, r18, -1
	bleu r20, r3, .LBB19_8
.LBB19_6:
	add r3, r11, r0
	jal r31, lua_pushnil
	addi r15, r0, 1
.LBB19_7:
	add r1, r15, r0
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
	addi sp, sp, 360
	jalr r0, r31, 0
.LBB19_8:
	beq r14, r22, .LBB19_21
.LBB19_9:
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, lua_toboolean
	bne r1, r22, .LBB19_13
.LBB19_10:
	ldw r21, r16+0
	addi r23, r0, 0
	lui r18, %hi(.L.str.20)
	addi r18, r18, %lo(.L.str.20)
	add r24, r23, r0
.LBB19_11:
	add r19, r13, r24
	add r3, r19, r0
	add r4, r18, r0
	jal r31, strpbrk
	bne r1, r23, .LBB19_21
.LBB19_12:
	add r3, r19, r0
	jal r31, strlen
	add r1, r24, r1
	addi r24, r1, 1
	bleu r24, r21, .LBB19_11
.LBB19_13:
	add r14, r12, r20
	ldw r1, r16+0
	beq r1, r22, .LBB19_37
.LBB19_14:
	ldw r3, r17+0
	sub r3, r3, r20
	add r19, r22, r0
	bgtu r1, r3, .LBB19_38
.LBB19_15:
	addi r17, r1, -1
	sub r18, r3, r17
	addi r19, r0, 0
	beq r18, r19, .LBB19_38
.LBB19_16:
	addi r20, r13, 1
	addi r23, r0, 0
.LBB19_17:
	ldb r4, r13+0
	add r3, r14, r0
	add r5, r18, r0
	jal r31, memchr
	beq r1, r23, .LBB19_20
.LBB19_18:
	add r19, r1, r0
	addi r21, r1, 1
	add r3, r21, r0
	add r4, r20, r0
	add r5, r17, r0
	jal r31, memcmp
	beq r1, r23, .LBB19_38
.LBB19_19:
	add r1, r18, r14
	sub r18, r1, r21
	add r14, r21, r0
	bne r18, r23, .LBB19_17
.LBB19_20:
	add r19, r23, r0
	jal r0, .LBB19_38
.LBB19_21:
	ldbu r22, r13+0
	addi r23, r0, 94
	bne r22, r23, .LBB19_23
.LBB19_22:
	addi r13, r13, 1
	ldw r1, r16+0
	addi r1, r1, -1
	stw r16+0, r1
.LBB19_23:
	add r1, r12, r20
	ldw r3, r17+0
	ldw r4, r16+0
	addi r16, fp, -348
	stw r16+12, r11
	addi r5, r0, 200
	stw r16+16, r5
	stw r16+0, r12
	add r3, r12, r3
	stw r16+4, r3
	add r3, r13, r4
	stw r16+8, r3
	addi r17, r0, 0
	lui r18, %hi(.L.str.23)
	addi r18, r18, %lo(.L.str.23)
                                        # implicit-def: $r15
.LBB19_24:
	add r19, r1, r0
	stb r16+20, r17
	add r3, r16, r0
	add r4, r1, r0
	add r5, r13, r0
	jal r31, match
	add r20, r1, r0
	beq r1, r17, .LBB19_34
.LBB19_25:
	beq r14, r17, .LBB19_30
.LBB19_26:
	sub r1, r19, r12
	addi r4, r1, 1
	add r3, r11, r0
	jal r31, lua_pushinteger
	sub r4, r20, r12
	add r3, r11, r0
	jal r31, lua_pushinteger
	ldbu r15, r16+20
	ldw r3, r16+12
	add r4, r15, r0
	add r5, r18, r0
	jal r31, luaL_checkstack
	beq r15, r17, .LBB19_29
.LBB19_27:
	add r21, r17, r0
.LBB19_28:
	add r3, r16, r0
	add r4, r21, r0
	add r5, r17, r0
	add r6, r17, r0
	jal r31, push_onecapture
	addi r21, r21, 1
	bne r15, r21, .LBB19_28
.LBB19_29:
	addi r15, r15, 2
	jal r0, .LBB19_34
.LBB19_30:
	ldbu r1, r16+20
	seq r3, r1, r17
	sne r4, r19, r17
	sub r4, r17, r4
	sub r3, r17, r3
	xori r5, r1, 1
	and r3, r5, r3
	and r3, r3, r4
	xor r21, r1, r3
	ldw r3, r16+12
	add r4, r21, r0
	add r5, r18, r0
	jal r31, luaL_checkstack
	add r15, r17, r0
	beq r21, r17, .LBB19_34
.LBB19_31:
	add r15, r17, r0
.LBB19_32:
	add r3, r16, r0
	add r4, r15, r0
	add r5, r19, r0
	add r6, r20, r0
	jal r31, push_onecapture
	addi r15, r15, 1
	bne r21, r15, .LBB19_32
.LBB19_33:
	add r15, r21, r0
.LBB19_34:
	bne r20, r17, .LBB19_7
.LBB19_35:
	beq r22, r23, .LBB19_6
.LBB19_36:
	addi r1, r19, 1
	ldw r3, r16+4
	bltu r19, r3, .LBB19_24
	jal r0, .LBB19_6
.LBB19_37:
	add r19, r14, r0
.LBB19_38:
	beq r19, r22, .LBB19_6
.LBB19_39:
	sub r12, r19, r12
	addi r4, r12, 1
	add r3, r11, r0
	jal r31, lua_pushinteger
	ldw r1, r16+0
	add r4, r1, r12
	add r3, r11, r0
	jal r31, lua_pushinteger
	jal r0, .LBB19_7
.Lfunc_end19:
	.size	str_find_aux, .Lfunc_end19-str_find_aux
                                        # -- End function
	.p2align	2                               # -- Begin function match
	.type	match,@function
match:                                  # @match
# %bb.0:
	addi sp, sp, -120
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 120
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
	add r12, r5, r0
	add r15, r4, r0
	add r11, r3, r0
	ldw r1, r3+16
	addi r3, r1, -1
	stw r11+16, r3
	addi r18, r0, 0
	beq r1, r18, .LBB20_130
.LBB20_1:
	ldw r1, r11+8
	beq r12, r1, .LBB20_129
.LBB20_2:
	addi r19, r11, 24
	addi r20, r0, 5
	addi r21, r0, 91
	addi r22, r0, 94
	lui r27, %hi(.L.str.27)
	addi r27, r27, %lo(.L.str.27)
	addi r23, r0, 37
	addi r24, r0, 93
	addi r25, r0, 21
	addi r13, r0, 1
	lui r3, 512
	addi r3, r3, 9
	stw fp+-80, r3
	lui r28, %hi(.LJTI20_1)
	addi r28, r28, %lo(.LJTI20_1)
	addi r3, r0, -1
	stw fp+-84, r3
	jal r0, .LBB20_6
.LBB20_3:
	add r15, r26, r0
.LBB20_4:
	beq r26, r18, .LBB20_129
.LBB20_5:
	ldw r1, r11+8
	beq r12, r1, .LBB20_129
.LBB20_6:
	add r14, r15, r0
	ldbu r3, r12+0
	addi r4, r3, -36
	bgtu r4, r20, .LBB20_9
.LBB20_7:
	slli r4, r4, 2
	lui r5, %hi(.LJTI20_0)
	addi r5, r5, %lo(.LJTI20_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB20_8:
	addi r4, r12, 1
	beq r4, r1, .LBB20_119
.LBB20_9:
	addi r16, r12, 1
	beq r3, r21, .LBB20_22
.LBB20_10:
	bne r3, r23, .LBB20_29
.LBB20_11:
	beq r16, r1, .LBB20_108
.LBB20_12:
	addi r16, r12, 2
	jal r0, .LBB20_29
.LBB20_13:
	ldbu r4, r12+1
	addi r5, r4, -48
	addi r6, r0, 10
	bgeu r5, r6, .LBB20_49
.LBB20_14:
	addi r1, r0, 49
	bltu r4, r1, .LBB20_17
.LBB20_15:
	addi r1, r4, -49
	ldbu r3, r11+20
	bge r1, r3, .LBB20_17
.LBB20_16:
	slli r3, r1, 3
	add r3, r11, r3
	ldw r3, r3+28
	addi r4, r0, -1
	bne r3, r4, .LBB20_18
.LBB20_17:
	ldw r3, r11+12
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	jal r31, luaL_error
.LBB20_18:
	slli r1, r1, 3
	add r3, r19, r1
	ldw r16, r3+4
	ldw r1, r11+4
	sub r4, r1, r14
	addi r15, r0, 0
	add r1, r15, r0
	bltu r4, r16, .LBB20_20
.LBB20_19:
	ldw r3, r3+0
	add r4, r14, r0
	add r5, r16, r0
	jal r31, memcmp
	addi r3, r0, 0
	seq r1, r1, r3
	add r4, r14, r16
	sub r1, r3, r1
	and r1, r4, r1
.LBB20_20:
	beq r1, r15, .LBB20_129
.LBB20_21:
	addi r12, r12, 2
	add r15, r1, r0
	jal r0, .LBB20_5
.LBB20_22:
	ldbu r1, r12+1
	seq r1, r1, r22
	addi r3, r12, 2
	xor r3, r3, r16
	sub r1, r18, r1
	and r1, r3, r1
	xor r15, r16, r1
	jal r0, .LBB20_24
.LBB20_23:
	ldw r3, r11+8
	sltu r3, r1, r3
	addi r4, r15, 2
	xor r4, r4, r1
	sub r3, r18, r3
	and r3, r4, r3
	xor r15, r1, r3
	ldbu r1, r15+0
	beq r1, r24, .LBB20_28
.LBB20_24:
	ldw r1, r11+8
	beq r15, r1, .LBB20_27
.LBB20_25:
	addi r1, r15, 1
	ldbu r3, r15+0
	beq r3, r23, .LBB20_23
.LBB20_26:
	add r15, r1, r0
	ldbu r1, r15+0
	bne r1, r24, .LBB20_24
	jal r0, .LBB20_28
.LBB20_27:
	ldw r3, r11+12
	add r4, r27, r0
	jal r31, luaL_error
	jal r0, .LBB20_25
.LBB20_28:
	addi r16, r15, 1
.LBB20_29:
	add r3, r11, r0
	add r4, r14, r0
	add r5, r12, r0
	add r6, r16, r0
	jal r31, singlematch
	ldbu r3, r16+0
	beq r1, r18, .LBB20_38
.LBB20_30:
	addi r1, r3, -42
	bgtu r1, r25, .LBB20_48
.LBB20_31:
	slli r1, r1, 2
	add r1, r28, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB20_32:
	addi r14, r14, 1
.LBB20_33:
	add r3, r18, r0
	ldw r15, fp+-84
.LBB20_34:
	add r17, r3, r0
	addi r15, r15, 1
	add r4, r15, r14
	add r3, r11, r0
	add r5, r12, r0
	add r6, r16, r0
	jal r31, singlematch
	addi r3, r17, 1
	bne r1, r18, .LBB20_34
.LBB20_35:
	addi r16, r16, 1
                                        # implicit-def: $r15
.LBB20_36:
	addi r26, r0, 0
	blt r17, r26, .LBB20_3
.LBB20_37:
	add r4, r14, r17
	add r3, r11, r0
	add r5, r16, r0
	jal r31, match
	addi r26, r0, 0
	seq r3, r1, r26
	xor r4, r15, r1
	sub r3, r26, r3
	and r3, r4, r3
	xor r15, r1, r3
	addi r17, r17, -1
	beq r1, r26, .LBB20_36
	jal r0, .LBB20_4
.LBB20_38:
	addi r1, r3, -42
	bgtu r1, r25, .LBB20_60
.LBB20_39:
	sll r1, r13, r1
	ldw r3, fp+-80
	and r1, r1, r3
	add r26, r18, r0
	add r15, r18, r0
	beq r1, r18, .LBB20_4
.LBB20_40:
	addi r12, r16, 1
	add r26, r13, r0
	add r15, r14, r0
	jal r0, .LBB20_4
.LBB20_41:
	addi r17, r16, 1
                                        # implicit-def: $r15
	jal r0, .LBB20_43
.LBB20_42:
	add r15, r1, r0
	beq r26, r26, .LBB20_4
.LBB20_43:
	add r3, r11, r0
	add r4, r14, r0
	add r5, r17, r0
	jal r31, match
	addi r26, r0, 0
	bne r1, r26, .LBB20_42
.LBB20_44:
	add r3, r11, r0
	add r4, r14, r0
	add r5, r12, r0
	add r6, r16, r0
	jal r31, singlematch
	addi r3, r0, 0
	beq r1, r3, .LBB20_46
.LBB20_45:
	addi r14, r14, 1
	addi r3, r0, 1
	bne r3, r26, .LBB20_43
	jal r0, .LBB20_4
.LBB20_46:
	add r15, r3, r0
	bne r3, r26, .LBB20_43
	jal r0, .LBB20_4
.LBB20_47:
	addi r4, r14, 1
	addi r15, r16, 1
	add r3, r11, r0
	add r5, r15, r0
	jal r31, match
	addi r3, r0, 0
	seq r26, r1, r3
	sub r3, r3, r26
	xor r4, r15, r12
	and r4, r4, r3
	xor r12, r12, r4
	xor r4, r14, r1
	and r3, r4, r3
	xor r15, r1, r3
	jal r0, .LBB20_4
.LBB20_48:
	addi r15, r14, 1
	add r26, r13, r0
	add r12, r16, r0
	jal r0, .LBB20_4
.LBB20_49:
	addi r5, r0, 102
	beq r4, r5, .LBB20_61
.LBB20_50:
	addi r5, r0, 98
	bne r4, r5, .LBB20_9
.LBB20_51:
	addi r15, r12, 2
	addi r1, r1, -1
	bgeu r15, r1, .LBB20_109
.LBB20_52:
	ldbu r3, r14+0
	ldbu r1, r15+0
	addi r15, r0, 0
	add r5, r15, r0
	bne r3, r1, .LBB20_88
.LBB20_53:
	ldw r4, r11+4
	addi r3, r14, 1
	add r5, r15, r0
	bgeu r3, r4, .LBB20_88
.LBB20_54:
	ldbu r6, r12+3
	addi r5, r0, 1
	andi r6, r6, 255
	jal r0, .LBB20_57
.LBB20_55:
	seq r7, r7, r1
	add r5, r5, r7
.LBB20_56:
	addi r3, r3, 1
	beq r3, r4, .LBB20_87
.LBB20_57:
	ldbu r7, r3+0
	bne r7, r6, .LBB20_55
.LBB20_58:
	addi r5, r5, -1
	addi r7, r0, 0
	bne r5, r7, .LBB20_56
.LBB20_59:
	addi r5, r3, 1
	jal r0, .LBB20_88
.LBB20_60:
	add r26, r18, r0
	add r15, r18, r0
	jal r0, .LBB20_4
.LBB20_61:
	ldbu r1, r12+2
	bne r1, r21, .LBB20_110
.LBB20_62:
	addi r17, r12, 3
	ldbu r1, r12+2
	beq r1, r21, .LBB20_66
.LBB20_63:
	add r4, r17, r0
	bne r1, r23, .LBB20_73
.LBB20_64:
	ldw r1, r11+8
	beq r17, r1, .LBB20_111
.LBB20_65:
	addi r4, r12, 4
	jal r0, .LBB20_73
.LBB20_66:
	ldbu r1, r12+3
	seq r1, r1, r22
	addi r3, r12, 4
	xor r3, r3, r17
	addi r15, r0, 0
	sub r1, r15, r1
	and r1, r3, r1
	xor r16, r17, r1
	jal r0, .LBB20_68
.LBB20_67:
	ldw r3, r11+8
	sltu r3, r1, r3
	addi r4, r16, 2
	xor r4, r4, r1
	sub r3, r15, r3
	and r3, r4, r3
	xor r16, r1, r3
	ldbu r1, r16+0
	beq r1, r24, .LBB20_72
.LBB20_68:
	ldw r1, r11+8
	beq r16, r1, .LBB20_71
.LBB20_69:
	addi r1, r16, 1
	ldbu r3, r16+0
	beq r3, r23, .LBB20_67
.LBB20_70:
	add r16, r1, r0
	ldbu r1, r16+0
	bne r1, r24, .LBB20_68
	jal r0, .LBB20_72
.LBB20_71:
	ldw r3, r11+12
	lui r4, %hi(.L.str.27)
	addi r4, r4, %lo(.L.str.27)
	jal r31, luaL_error
	jal r0, .LBB20_69
.LBB20_72:
	addi r4, r16, 1
.LBB20_73:
	stw fp+-92, r27
	stw fp+-88, r19
	ldw r3, r11+0
	addi r1, r0, 0
	add r15, r1, r0
	beq r14, r3, .LBB20_75
.LBB20_74:
	ldbu r15, r14+-1
.LBB20_75:
	addi r12, r12, 2
	stw fp+-96, r4
	addi r16, r4, -1
	ldbu r3, r17+0
	seq r19, r3, r22
	xor r27, r17, r12
	sub r1, r1, r19
	and r1, r27, r1
	xor r26, r12, r1
	addi r1, r26, 1
	bgeu r1, r16, .LBB20_91
.LBB20_76:
	sne r3, r3, r22
	stw fp+-100, r3
	jal r0, .LBB20_79
.LBB20_77:
	add r26, r1, r0
	beq r15, r3, .LBB20_86
.LBB20_78:
	addi r1, r26, 1
	bgeu r1, r16, .LBB20_91
.LBB20_79:
	ldbu r3, r26+1
	ldbu r4, r26+2
	bne r3, r23, .LBB20_82
.LBB20_80:
	add r3, r15, r0
	jal r31, match_class
	addi r3, r0, 0
	bne r1, r3, .LBB20_90
.LBB20_81:
	addi r26, r26, 2
	jal r0, .LBB20_78
.LBB20_82:
	addi r5, r0, 45
	bne r4, r5, .LBB20_77
.LBB20_83:
	addi r26, r26, 3
	bgeu r26, r16, .LBB20_77
.LBB20_84:
	bltu r15, r3, .LBB20_78
.LBB20_85:
	ldbu r1, r26+0
	bltu r1, r15, .LBB20_78
.LBB20_86:
	ldw r19, fp+-100
	addi r26, r0, 0
	beq r19, r26, .LBB20_92
	jal r0, .LBB20_106
.LBB20_87:
	add r5, r15, r0
.LBB20_88:
	beq r5, r15, .LBB20_129
.LBB20_89:
	addi r12, r12, 4
	add r15, r5, r0
	jal r0, .LBB20_5
.LBB20_90:
	ldw r19, fp+-100
.LBB20_91:
	addi r26, r0, 0
	bne r19, r26, .LBB20_106
.LBB20_92:
	ldbu r3, r17+0
	seq r17, r3, r22
	sub r1, r26, r17
	and r1, r27, r1
	xor r19, r12, r1
	addi r1, r19, 1
	bgeu r1, r16, .LBB20_104
.LBB20_93:
	ldbu r15, r14+0
	sne r27, r3, r22
	jal r0, .LBB20_96
.LBB20_94:
	add r19, r1, r0
	beq r15, r3, .LBB20_103
.LBB20_95:
	addi r1, r19, 1
	bgeu r1, r16, .LBB20_104
.LBB20_96:
	ldbu r3, r19+1
	ldbu r4, r19+2
	bne r3, r23, .LBB20_99
.LBB20_97:
	add r3, r15, r0
	jal r31, match_class
	bne r1, r26, .LBB20_103
.LBB20_98:
	addi r19, r19, 2
	jal r0, .LBB20_95
.LBB20_99:
	addi r5, r0, 45
	bne r4, r5, .LBB20_94
.LBB20_100:
	addi r19, r19, 3
	bgeu r19, r16, .LBB20_94
.LBB20_101:
	bltu r15, r3, .LBB20_95
.LBB20_102:
	ldbu r1, r19+0
	bltu r1, r15, .LBB20_95
.LBB20_103:
	add r17, r27, r0
.LBB20_104:
	beq r17, r26, .LBB20_106
.LBB20_105:
	addi r1, r0, 1
	ldw r12, fp+-96
	add r15, r14, r0
	jal r0, .LBB20_107
.LBB20_106:
	add r1, r26, r0
	add r15, r26, r0
.LBB20_107:
	ldw r19, fp+-88
	ldw r27, fp+-92
	bne r1, r26, .LBB20_5
	jal r0, .LBB20_129
.LBB20_108:
	ldw r3, r11+12
	lui r4, %hi(.L.str.26)
	addi r4, r4, %lo(.L.str.26)
	jal r31, luaL_error
	jal r0, .LBB20_12
.LBB20_109:
	ldw r3, r11+12
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	jal r31, luaL_error
	jal r0, .LBB20_52
.LBB20_110:
	ldw r3, r11+12
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	jal r31, luaL_error
	jal r0, .LBB20_62
.LBB20_111:
	ldw r3, r11+12
	lui r4, %hi(.L.str.26)
	addi r4, r4, %lo(.L.str.26)
	jal r31, luaL_error
	jal r0, .LBB20_65
.LBB20_112:
	ldbu r1, r12+1
	addi r3, r0, 41
	bne r1, r3, .LBB20_123
.LBB20_113:
	addi r12, r12, 2
	ldbu r13, r11+20
	addi r1, r0, 32
	bltu r13, r1, .LBB20_115
.LBB20_114:
	ldw r3, r11+12
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	jal r31, luaL_error
.LBB20_115:
	slli r1, r13, 3
	add r1, r11, r1
	stw r1+24, r14
	addi r3, r0, -2
	jal r0, .LBB20_126
.LBB20_116:
	addi r12, r12, 1
	addi r3, r11, 20
	ldbu r1, r11+20
	slli r4, r1, 3
	add r3, r3, r4
	addi r4, r0, 1
	addi r13, r0, -1
.LBB20_117:
	blt r1, r4, .LBB20_120
.LBB20_118:
	addi r1, r1, -1
	ldw r5, r3+0
	addi r3, r3, -8
	bne r5, r13, .LBB20_117
	jal r0, .LBB20_121
.LBB20_119:
	ldw r1, r11+4
	seq r1, r14, r1
	addi r3, r0, 0
	sub r1, r3, r1
	and r15, r14, r1
	jal r0, .LBB20_129
.LBB20_120:
	ldw r3, r11+12
	lui r4, %hi(.L.str.24)
	addi r4, r4, %lo(.L.str.24)
	jal r31, luaL_error
.LBB20_121:
	slli r1, r1, 3
	add r16, r11, r1
	ldw r1, r16+24
	sub r1, r14, r1
	stw r16+28, r1
	add r3, r11, r0
	add r4, r14, r0
	add r5, r12, r0
	jal r31, match
	add r15, r1, r0
	addi r1, r0, 0
	bne r15, r1, .LBB20_129
.LBB20_122:
	addi r3, r16, 24
	stw r3+4, r13
	jal r0, .LBB20_128
.LBB20_123:
	addi r12, r12, 1
	ldbu r13, r11+20
	addi r1, r0, 32
	bltu r13, r1, .LBB20_125
.LBB20_124:
	ldw r3, r11+12
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	jal r31, luaL_error
.LBB20_125:
	slli r1, r13, 3
	add r1, r11, r1
	stw r1+24, r14
	addi r3, r0, -1
.LBB20_126:
	stw r1+28, r3
	addi r1, r13, 1
	stb r11+20, r1
	add r3, r11, r0
	add r4, r14, r0
	add r5, r12, r0
	jal r31, match
	add r15, r1, r0
	addi r1, r0, 0
	bne r15, r1, .LBB20_129
.LBB20_127:
	ldbu r3, r11+20
	addi r3, r3, -1
	stb r11+20, r3
.LBB20_128:
	add r15, r1, r0
.LBB20_129:
	ldw r1, r11+16
	addi r1, r1, 1
	stw r11+16, r1
	add r1, r15, r0
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
	addi sp, sp, 120
	jalr r0, r31, 0
.LBB20_130:
	ldw r3, r11+12
	lui r4, %hi(.L.str.21)
	addi r4, r4, %lo(.L.str.21)
	jal r31, luaL_error
	ldw r1, r11+8
	bne r12, r1, .LBB20_2
	jal r0, .LBB20_129
.Lfunc_end20:
	.size	match, .Lfunc_end20-match
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI20_0:
	.word	.LBB20_8
	.word	.LBB20_13
	.word	.LBB20_9
	.word	.LBB20_9
	.word	.LBB20_112
	.word	.LBB20_116
.LJTI20_1:
	.word	.LBB20_33
	.word	.LBB20_32
	.word	.LBB20_48
	.word	.LBB20_41
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_48
	.word	.LBB20_47
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function singlematch
	.type	singlematch,@function
singlematch:                            # @singlematch
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
	ldw r1, r3+4
	bgeu r4, r1, .LBB21_5
.LBB21_1:
	ldbu r12, r4+0
	ldbu r1, r5+0
	addi r3, r0, 91
	beq r1, r3, .LBB21_8
.LBB21_2:
	addi r3, r0, 46
	beq r1, r3, .LBB21_7
.LBB21_3:
	addi r3, r0, 37
	bne r1, r3, .LBB21_20
.LBB21_4:
	ldbu r4, r5+1
	add r3, r12, r0
	jal r31, match_class
	add r11, r1, r0
	jal r0, .LBB21_6
.LBB21_5:
	addi r11, r0, 0
.LBB21_6:
	add r1, r11, r0
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
.LBB21_7:
	addi r11, r0, 1
	jal r0, .LBB21_6
.LBB21_8:
	addi r13, r6, -1
	addi r1, r5, 1
	ldbu r3, r5+1
	addi r4, r0, 94
	seq r11, r3, r4
	xor r1, r1, r5
	addi r14, r0, 0
	sub r6, r14, r11
	and r1, r1, r6
	xor r16, r5, r1
	addi r1, r16, 1
	bgeu r1, r13, .LBB21_6
.LBB21_9:
	sne r15, r3, r4
	addi r17, r0, 37
	addi r18, r0, 45
	jal r0, .LBB21_12
.LBB21_10:
	add r16, r1, r0
	beq r12, r3, .LBB21_19
.LBB21_11:
	addi r1, r16, 1
	bgeu r1, r13, .LBB21_6
.LBB21_12:
	ldbu r3, r16+1
	ldbu r4, r16+2
	bne r3, r17, .LBB21_15
.LBB21_13:
	add r3, r12, r0
	jal r31, match_class
	bne r1, r14, .LBB21_19
.LBB21_14:
	addi r16, r16, 2
	jal r0, .LBB21_11
.LBB21_15:
	bne r4, r18, .LBB21_10
.LBB21_16:
	addi r16, r16, 3
	bgeu r16, r13, .LBB21_10
.LBB21_17:
	bltu r12, r3, .LBB21_11
.LBB21_18:
	ldbu r1, r16+0
	bltu r1, r12, .LBB21_11
.LBB21_19:
	add r11, r15, r0
	jal r0, .LBB21_6
.LBB21_20:
	seq r11, r1, r12
	jal r0, .LBB21_6
.Lfunc_end21:
	.size	singlematch, .Lfunc_end21-singlematch
                                        # -- End function
	.p2align	2                               # -- Begin function match_class
	.type	match_class,@function
match_class:                            # @match_class
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r4, r0
	add r12, r3, r0
	add r3, r4, r0
	jal r31, tolower
	addi r1, r1, -97
	addi r3, r0, 25
	bgtu r1, r3, .LBB22_16
.LBB22_1:
	slli r1, r1, 2
	lui r3, %hi(.LJTI22_0)
	addi r3, r3, %lo(.LJTI22_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB22_2:
	add r3, r12, r0
	jal r31, isalpha
	jal r0, .LBB22_13
.LBB22_3:
	addi r1, r0, 0
	seq r12, r12, r1
	jal r0, .LBB22_14
.LBB22_4:
	add r3, r12, r0
	jal r31, islower
	jal r0, .LBB22_13
.LBB22_5:
	add r3, r12, r0
	jal r31, ispunct
	jal r0, .LBB22_13
.LBB22_6:
	add r3, r12, r0
	jal r31, isgraph
	jal r0, .LBB22_13
.LBB22_7:
	add r3, r12, r0
	jal r31, isdigit
	jal r0, .LBB22_13
.LBB22_8:
	add r3, r12, r0
	jal r31, iscntrl
	jal r0, .LBB22_13
.LBB22_9:
	add r3, r12, r0
	jal r31, isspace
	jal r0, .LBB22_13
.LBB22_10:
	add r3, r12, r0
	jal r31, isupper
	jal r0, .LBB22_13
.LBB22_11:
	add r3, r12, r0
	jal r31, isxdigit
	jal r0, .LBB22_13
.LBB22_12:
	add r3, r12, r0
	jal r31, isalnum
.LBB22_13:
	add r12, r1, r0
.LBB22_14:
	add r3, r11, r0
	jal r31, islower
	addi r3, r0, 0
	seq r1, r1, r3
	seq r4, r12, r3
	xor r4, r4, r12
	sub r1, r3, r1
	and r1, r4, r1
	xor r1, r12, r1
.LBB22_15:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB22_16:
	seq r1, r11, r12
	jal r0, .LBB22_15
.Lfunc_end22:
	.size	match_class, .Lfunc_end22-match_class
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI22_0:
	.word	.LBB22_2
	.word	.LBB22_16
	.word	.LBB22_8
	.word	.LBB22_7
	.word	.LBB22_16
	.word	.LBB22_16
	.word	.LBB22_6
	.word	.LBB22_16
	.word	.LBB22_16
	.word	.LBB22_16
	.word	.LBB22_16
	.word	.LBB22_4
	.word	.LBB22_16
	.word	.LBB22_16
	.word	.LBB22_16
	.word	.LBB22_5
	.word	.LBB22_16
	.word	.LBB22_16
	.word	.LBB22_9
	.word	.LBB22_16
	.word	.LBB22_10
	.word	.LBB22_16
	.word	.LBB22_12
	.word	.LBB22_11
	.word	.LBB22_16
	.word	.LBB22_3
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function push_onecapture
	.type	push_onecapture,@function
push_onecapture:                        # @push_onecapture
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
	ldbu r1, r3+20
	bge r4, r1, .LBB23_4
.LBB23_1:
	slli r1, r4, 3
	add r3, r11, r1
	ldw r1, r3+28
	ldw r5, r3+24
	addi r12, r0, -2
	beq r1, r12, .LBB23_8
.LBB23_2:
	addi r12, r0, -1
	beq r1, r12, .LBB23_10
.LBB23_3:
	add r12, r1, r0
	addi r1, r0, -2
	bne r12, r1, .LBB23_6
	jal r0, .LBB23_7
.LBB23_4:
	addi r1, r0, 0
	bne r4, r1, .LBB23_11
.LBB23_5:
	sub r12, r6, r5
	addi r1, r0, -2
	beq r12, r1, .LBB23_7
.LBB23_6:
	ldw r3, r11+12
	add r4, r5, r0
	add r5, r12, r0
	jal r31, lua_pushlstring
.LBB23_7:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB23_8:
	ldw r3, r11+12
	ldw r1, r11+0
	sub r1, r5, r1
	addi r4, r1, 1
	add r13, r5, r0
	jal r31, lua_pushinteger
.LBB23_9:
	add r5, r13, r0
	addi r1, r0, -2
	bne r12, r1, .LBB23_6
	jal r0, .LBB23_7
.LBB23_10:
	ldw r3, r11+12
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	add r13, r5, r0
	jal r31, luaL_error
	jal r0, .LBB23_9
.LBB23_11:
	ldw r3, r11+12
	addi r1, r4, 1
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	add r12, r5, r0
	add r5, r1, r0
	add r13, r6, r0
	jal r31, luaL_error
	add r6, r13, r0
	add r5, r12, r0
	jal r0, .LBB23_5
.Lfunc_end23:
	.size	push_onecapture, .Lfunc_end23-push_onecapture
                                        # -- End function
	.p2align	2                               # -- Begin function checkformat
	.type	checkformat,@function
checkformat:                            # @checkformat
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
	add r13, r6, r0
	add r11, r4, r0
	add r12, r3, r0
	addi r14, r4, 1
	add r3, r14, r0
	add r4, r5, r0
	jal r31, strspn
	add r14, r14, r1
	ldbu r3, r14+0
	addi r1, r0, 48
	beq r3, r1, .LBB24_8
.LBB24_1:
	jal r31, isdigit
	addi r15, r0, 0
	beq r1, r15, .LBB24_3
.LBB24_2:
	addi r16, r14, 1
	ldbu r3, r14+1
	jal r31, isdigit
	seq r1, r1, r15
	addi r3, r14, 2
	xor r4, r16, r3
	sub r1, r15, r1
	and r1, r4, r1
	xor r14, r3, r1
.LBB24_3:
	beq r13, r15, .LBB24_8
.LBB24_4:
	ldbu r1, r14+0
	andi r1, r1, 255
	addi r3, r0, 46
	bne r1, r3, .LBB24_8
.LBB24_5:
	ldbu r3, r14+1
	jal r31, isdigit
	beq r1, r15, .LBB24_7
.LBB24_6:
	addi r13, r14, 2
	ldbu r3, r14+2
	jal r31, isdigit
	seq r1, r1, r15
	addi r3, r14, 3
	xor r4, r13, r3
	sub r1, r15, r1
	and r1, r4, r1
	xor r14, r3, r1
	jal r0, .LBB24_8
.LBB24_7:
	addi r14, r14, 1
.LBB24_8:
	ldbu r3, r14+0
	jal r31, isalpha
	addi r3, r0, 0
	bne r1, r3, .LBB24_10
.LBB24_9:
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	add r3, r12, r0
	add r5, r11, r0
	jal r31, luaL_error
.LBB24_10:
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
.Lfunc_end24:
	.size	checkformat, .Lfunc_end24-checkformat
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function lua_number2strx
.LCPI25_0:
	.quad	0x0000000000000000              # double 0
.LCPI25_1:
	.quad	0x4030000000000000              # double 16
	.text
	.p2align	2
	.type	lua_number2strx,@function
lua_number2strx:                        # @lua_number2strx
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
	add r9, r8, r0
	add r8, r7, r0
	add r13, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	lui r1, 524288
	addi r1, r1, -1
	and r1, r9, r1
	lui r3, 524032
	blt r1, r3, .LBB25_2
.LBB25_1:
	lui r4, %hi(.L.str.46)
	addi r4, r4, %lo(.L.str.46)
	jal r0, .LBB25_4
.LBB25_2:
	lui r1, %hi(.LCPI25_0)
	addi r1, r1, %lo(.LCPI25_0)
	ldw r17, r1+4
	ldw r16, r1+0
	feq.d r1, r8, r16
	xori r1, r1, 1
	addi r15, r0, 0
	bne r1, r15, .LBB25_11
.LBB25_3:
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
.LBB25_4:
	add r3, r11, r0
	add r5, r8, r0
	add r6, r9, r0
	jal r31, sprintf
	add r14, r1, r0
.LBB25_5:
	ldbu r1, r13+1
	addi r3, r0, 65
	bne r1, r3, .LBB25_9
.LBB25_6:
	addi r1, r0, 1
	blt r14, r1, .LBB25_10
.LBB25_7:
	addi r12, r0, 0
.LBB25_8:
	add r13, r11, r12
	ldbu r3, r13+0
	jal r31, toupper
	stb r13+0, r1
	addi r12, r12, 1
	bne r14, r12, .LBB25_8
	jal r0, .LBB25_10
.LBB25_9:
	addi r3, r0, 97
	bne r1, r3, .LBB25_19
.LBB25_10:
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
.LBB25_11:
	addi r14, fp, -68
	add r3, r8, r0
	add r4, r9, r0
	add r5, r14, r0
	jal r31, frexp
	add r4, r1, r0
	add r5, r2, r0
	fle.d r1, r16, r4
	bne r1, r15, .LBB25_13
.LBB25_12:
	addi r1, r0, 45
	stb r11+0, r1
	fneg.d r4, r4
	addi r19, r0, 1
	jal r0, .LBB25_14
.LBB25_13:
	add r19, r15, r0
.LBB25_14:
	add r21, r11, r19
	addi r1, r0, 48
	stb r21+0, r1
	addi r1, r0, 120
	stb r21+1, r1
	addi r20, r19, 3
	fadd.d r22, r4, r4
	add r3, r22, r0
	add r4, r23, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	fcvt.w.d r1, r4
	addi r18, r0, 10
	slt r3, r1, r18
	sub r3, r15, r3
	andi r3, r3, 103
	xori r3, r3, 87
	add r1, r3, r1
	stb r21+2, r1
	fsub.d r4, r22, r4
	ldw r1, r14+0
	addi r1, r1, -1
	stw r14+0, r1
	fle.d r1, r4, r16
	bne r1, r15, .LBB25_17
.LBB25_15:
	addi r15, r19, 4
	add r1, r11, r20
	addi r3, r0, 46
	stb r1+0, r3
	lui r1, %hi(.LCPI25_1)
	addi r1, r1, %lo(.LCPI25_1)
	ldw r21, r1+4
	ldw r20, r1+0
	addi r19, r0, 0
.LBB25_16:
	add r22, r15, r0
	addi r15, r15, 1
	fmul.d r24, r4, r20
	add r3, r24, r0
	add r4, r25, r0
	jal r31, floor
	add r4, r1, r0
	add r5, r2, r0
	fcvt.w.d r1, r4
	slt r3, r1, r18
	sub r3, r19, r3
	andi r3, r3, 103
	xori r3, r3, 87
	add r1, r3, r1
	add r3, r11, r22
	stb r3+0, r1
	fsub.d r4, r24, r4
	flt.d r1, r16, r4
	bne r1, r19, .LBB25_16
	jal r0, .LBB25_18
.LBB25_17:
	add r15, r20, r0
.LBB25_18:
	add r3, r11, r15
	ldw r5, r14+0
	lui r4, %hi(.L.str.48)
	addi r4, r4, %lo(.L.str.48)
	jal r31, sprintf
	add r14, r1, r15
	jal r0, .LBB25_5
.LBB25_19:
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	add r3, r12, r0
	jal r31, luaL_error
	add r14, r1, r0
	jal r0, .LBB25_10
.Lfunc_end25:
	.size	lua_number2strx, .Lfunc_end25-lua_number2strx
                                        # -- End function
	.p2align	2                               # -- Begin function gmatch_aux
	.type	gmatch_aux,@function
gmatch_aux:                             # @gmatch_aux
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
	add r12, r3, r0
	lui r1, 1048572
	addi r4, r1, 381
	jal r31, lua_touserdata
	add r11, r1, r0
	stw r1+24, r12
	ldw r12, r1+0
	ldw r1, r1+16
	bleu r12, r1, .LBB26_2
.LBB26_1:
	addi r15, r0, 0
	jal r0, .LBB26_14
.LBB26_2:
	addi r13, r11, 12
	addi r18, r0, 0
	addi r19, r0, 1
	lui r14, %hi(.L.str.23)
	addi r14, r14, %lo(.L.str.23)
                                        # implicit-def: $r15
.LBB26_3:
	stb r11+32, r18
	ldw r5, r11+4
	add r3, r13, r0
	add r4, r12, r0
	jal r31, match
	beq r1, r18, .LBB26_8
.LBB26_4:
	add r16, r1, r0
	ldw r3, r11+8
	add r1, r19, r0
	beq r16, r3, .LBB26_11
.LBB26_5:
	stw r11+8, r16
	stw r11+0, r16
	ldbu r1, r11+32
	seq r3, r1, r18
	sne r4, r12, r18
	sub r4, r18, r4
	sub r3, r18, r3
	xori r5, r1, 1
	and r3, r5, r3
	and r3, r3, r4
	xor r15, r1, r3
	ldw r3, r11+24
	add r4, r15, r0
	add r5, r14, r0
	jal r31, luaL_checkstack
	beq r15, r18, .LBB26_9
.LBB26_6:
	add r17, r18, r0
.LBB26_7:
	add r3, r13, r0
	add r4, r17, r0
	add r5, r12, r0
	add r6, r16, r0
	jal r31, push_onecapture
	addi r17, r17, 1
	bne r15, r17, .LBB26_7
	jal r0, .LBB26_10
.LBB26_8:
	add r1, r19, r0
	jal r0, .LBB26_11
.LBB26_9:
	add r15, r18, r0
.LBB26_10:
	add r1, r18, r0
.LBB26_11:
	beq r1, r18, .LBB26_14
.LBB26_12:
	addi r12, r12, 1
	ldw r1, r11+16
	bleu r12, r1, .LBB26_3
.LBB26_13:
	add r15, r18, r0
.LBB26_14:
	add r1, r15, r0
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
.Lfunc_end26:
	.size	gmatch_aux, .Lfunc_end26-gmatch_aux
                                        # -- End function
	.p2align	2                               # -- Begin function getdetails
	.type	getdetails,@function
getdetails:                             # @getdetails
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
	add r11, r7, r0
	add r14, r6, r0
	add r15, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	add r4, r5, r0
	add r5, r6, r0
	jal r31, getoption
	ldw r3, r14+0
	addi r14, fp, -32
	stw r14+0, r3
	addi r3, r0, 9
	bne r1, r3, .LBB27_5
.LBB27_1:
	ldw r3, r15+0
	ldbu r3, r3+0
	addi r16, r0, 0
	beq r3, r16, .LBB27_4
.LBB27_2:
	add r3, r13, r0
	add r4, r15, r0
	add r5, r14, r0
	add r15, r1, r0
	jal r31, getoption
	add r3, r1, r0
	add r1, r15, r0
	addi r4, r0, 5
	beq r3, r4, .LBB27_4
.LBB27_3:
	ldw r3, r14+0
	bne r3, r16, .LBB27_5
.LBB27_4:
	ldw r3, r13+0
	lui r5, %hi(.L.str.67)
	addi r5, r5, %lo(.L.str.67)
	addi r4, r0, 1
	add r15, r1, r0
	jal r31, luaL_argerror
	add r1, r15, r0
.LBB27_5:
	addi r3, r0, 0
	addi r4, r0, 5
	beq r1, r4, .LBB27_11
.LBB27_6:
	ldw r4, r14+0
	addi r5, r0, 2
	blt r4, r5, .LBB27_11
.LBB27_7:
	ldw r3, r13+8
	ble r4, r3, .LBB27_9
.LBB27_8:
	stw r14+0, r3
.LBB27_9:
	ldw r14, r14+0
	addi r15, r14, -1
	and r3, r14, r15
	addi r4, r0, 0
	bne r3, r4, .LBB27_12
.LBB27_10:
	and r3, r15, r12
	sub r3, r14, r3
	and r3, r3, r15
.LBB27_11:
	stw r11+0, r3
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
.LBB27_12:
	ldw r3, r13+0
	lui r5, %hi(.L.str.68)
	addi r5, r5, %lo(.L.str.68)
	addi r4, r0, 1
	add r13, r1, r0
	jal r31, luaL_argerror
	add r1, r13, r0
	jal r0, .LBB27_10
.Lfunc_end27:
	.size	getdetails, .Lfunc_end27-getdetails
                                        # -- End function
	.p2align	2                               # -- Begin function getoption
	.type	getoption,@function
getoption:                              # @getoption
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r6, r4+0
	addi r9, r6, 1
	stw r4+0, r9
	ldb r7, r6+0
	addi r8, r0, 0
	stw r5+0, r8
	andi r1, r7, 255
	addi r10, r1, -32
	addi r1, r0, 90
	bgtu r10, r1, .LBB28_29
.LBB28_1:
	addi r1, r0, 7
	slli r10, r10, 2
	lui r11, %hi(.LJTI28_0)
	addi r11, r11, %lo(.LJTI28_0)
	add r10, r11, r10
	ldw r10, r10+0
	jalr r0, r10, 0
.LBB28_2:
	addi r1, r0, 4
	jal r0, .LBB28_50
.LBB28_3:
	addi r1, r0, 4
	jal r0, .LBB28_34
.LBB28_4:
	addi r1, r0, 1
	stw r3+4, r1
	jal r0, .LBB28_45
.LBB28_5:
	ldb r1, r9+0
	addi r1, r1, -58
	addi r7, r0, -10
	bgeu r1, r7, .LBB28_30
.LBB28_6:
	addi r1, r0, 4
	jal r0, .LBB28_33
.LBB28_7:
	addi r1, r0, 8
	stw r5+0, r1
	addi r1, r0, 3
	jal r0, .LBB28_51
.LBB28_8:
	addi r1, r0, 1
	jal r0, .LBB28_34
.LBB28_9:
	addi r1, r0, 1
	stw r5+0, r1
	addi r1, r0, 8
	jal r0, .LBB28_51
.LBB28_10:
	ldb r1, r9+0
	addi r1, r1, -58
	addi r7, r0, -10
	bgeu r1, r7, .LBB28_35
.LBB28_11:
	addi r8, r0, 4
	jal r0, .LBB28_38
.LBB28_12:
	addi r1, r0, 9
	jal r0, .LBB28_51
.LBB28_13:
	addi r1, r0, 0
	stw r3+4, r1
	jal r0, .LBB28_45
.LBB28_14:
	ldb r1, r9+0
	addi r1, r1, -58
	addi r5, r0, -10
	bgeu r1, r5, .LBB28_40
.LBB28_15:
	addi r1, r0, 4
	jal r0, .LBB28_43
.LBB28_16:
	addi r1, r0, 1
	stw r5+0, r1
	jal r0, .LBB28_51
.LBB28_17:
	addi r1, r0, 2
	jal r0, .LBB28_50
.LBB28_18:
	ldb r1, r9+0
	addi r1, r1, -58
	addi r7, r0, -1
	addi r9, r0, -10
	add r8, r7, r0
	bltu r1, r9, .LBB28_22
.LBB28_19:
	addi r1, r6, 2
	addi r8, r0, 0
	addi r6, r0, 10
	addi r9, r0, 9
	lui r10, 52429
	addi r10, r10, -820
.LBB28_20:
	mul r8, r8, r6
	stw r4+0, r1
	ldb r11, r1+-1
	add r8, r8, r11
	addi r8, r8, -48
	ldb r11, r1+0
	addi r11, r11, -48
	bgtu r11, r9, .LBB28_22
.LBB28_21:
	addi r1, r1, 1
	blt r8, r10, .LBB28_20
.LBB28_22:
	stw r5+0, r8
	addi r1, r0, 5
	bne r8, r7, .LBB28_51
.LBB28_23:
	ldw r3, r3+0
	lui r4, %hi(.L.str.69)
	addi r4, r4, %lo(.L.str.69)
	add r11, r1, r0
	jal r31, luaL_error
	add r1, r11, r0
	jal r0, .LBB28_51
.LBB28_24:
	addi r1, r0, 8
	stw r5+0, r1
	addi r1, r0, 4
	jal r0, .LBB28_51
.LBB28_25:
	addi r1, r0, 2
	jal r0, .LBB28_34
.LBB28_26:
	ldb r1, r9+0
	addi r1, r1, -58
	addi r7, r0, -10
	bgeu r1, r7, .LBB28_46
.LBB28_27:
	addi r1, r0, 4
	jal r0, .LBB28_49
.LBB28_28:
	addi r1, r0, 4
	stw r5+0, r1
	addi r1, r0, 2
	jal r0, .LBB28_51
.LBB28_29:
	ldw r3, r3+0
	lui r4, %hi(.L.str.70)
	addi r4, r4, %lo(.L.str.70)
	add r5, r7, r0
	jal r31, luaL_error
	jal r0, .LBB28_45
.LBB28_30:
	addi r6, r6, 2
	addi r1, r0, 0
	addi r7, r0, 10
	addi r8, r0, 9
	lui r9, 52429
	addi r9, r9, -820
.LBB28_31:
	mul r1, r1, r7
	stw r4+0, r6
	ldb r10, r6+-1
	add r1, r1, r10
	addi r1, r1, -48
	ldb r10, r6+0
	addi r10, r10, -48
	bgtu r10, r8, .LBB28_33
.LBB28_32:
	addi r6, r6, 1
	blt r1, r9, .LBB28_31
.LBB28_33:
	addi r4, r1, -17
	addi r6, r0, -17
	bleu r4, r6, .LBB28_52
.LBB28_34:
	stw r5+0, r1
	addi r1, r0, 0
	jal r0, .LBB28_51
.LBB28_35:
	addi r1, r6, 2
	addi r6, r0, 10
	addi r7, r0, 9
	lui r9, 52429
	addi r9, r9, -820
.LBB28_36:
	mul r8, r8, r6
	stw r4+0, r1
	ldb r10, r1+-1
	add r8, r8, r10
	addi r8, r8, -48
	ldb r10, r1+0
	addi r10, r10, -48
	bgtu r10, r7, .LBB28_38
.LBB28_37:
	addi r1, r1, 1
	blt r8, r9, .LBB28_36
.LBB28_38:
	addi r1, r8, -17
	addi r4, r0, -17
	bleu r1, r4, .LBB28_53
.LBB28_39:
	stw r5+0, r8
	addi r1, r0, 6
	jal r0, .LBB28_51
.LBB28_40:
	addi r5, r6, 2
	addi r1, r0, 0
	addi r6, r0, 10
	addi r7, r0, 9
	lui r8, 52429
	addi r8, r8, -820
.LBB28_41:
	mul r1, r1, r6
	stw r4+0, r5
	ldb r9, r5+-1
	add r1, r1, r9
	addi r1, r1, -48
	ldb r9, r5+0
	addi r9, r9, -48
	bgtu r9, r7, .LBB28_43
.LBB28_42:
	addi r5, r5, 1
	blt r1, r8, .LBB28_41
.LBB28_43:
	addi r4, r1, -17
	addi r5, r0, -17
	bleu r4, r5, .LBB28_54
.LBB28_44:
	stw r3+8, r1
.LBB28_45:
	addi r1, r0, 10
	jal r0, .LBB28_51
.LBB28_46:
	addi r6, r6, 2
	addi r1, r0, 0
	addi r7, r0, 10
	addi r8, r0, 9
	lui r9, 52429
	addi r9, r9, -820
.LBB28_47:
	mul r1, r1, r7
	stw r4+0, r6
	ldb r10, r6+-1
	add r1, r1, r10
	addi r1, r1, -48
	ldb r10, r6+0
	addi r10, r10, -48
	bgtu r10, r8, .LBB28_49
.LBB28_48:
	addi r6, r6, 1
	blt r1, r9, .LBB28_47
.LBB28_49:
	addi r4, r1, -17
	addi r6, r0, -17
	bleu r4, r6, .LBB28_55
.LBB28_50:
	stw r5+0, r1
	addi r1, r0, 1
.LBB28_51:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB28_52:
	ldw r3, r3+0
	lui r4, %hi(.L.str.71)
	addi r4, r4, %lo(.L.str.71)
	addi r6, r0, 16
	add r11, r5, r0
	add r5, r1, r0
	jal r31, luaL_error
	add r5, r11, r0
	jal r0, .LBB28_34
.LBB28_53:
	ldw r3, r3+0
	lui r4, %hi(.L.str.71)
	addi r4, r4, %lo(.L.str.71)
	addi r6, r0, 16
	add r11, r5, r0
	add r5, r8, r0
	jal r31, luaL_error
	add r5, r11, r0
	add r8, r1, r0
	jal r0, .LBB28_39
.LBB28_54:
	ldw r5, r3+0
	lui r4, %hi(.L.str.71)
	addi r4, r4, %lo(.L.str.71)
	addi r6, r0, 16
	add r11, r3, r0
	add r3, r5, r0
	add r5, r1, r0
	jal r31, luaL_error
	add r3, r11, r0
	jal r0, .LBB28_44
.LBB28_55:
	ldw r3, r3+0
	lui r4, %hi(.L.str.71)
	addi r4, r4, %lo(.L.str.71)
	addi r6, r0, 16
	add r11, r5, r0
	add r5, r1, r0
	jal r31, luaL_error
	add r5, r11, r0
	jal r0, .LBB28_50
.Lfunc_end28:
	.size	getoption, .Lfunc_end28-getoption
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI28_0:
	.word	.LBB28_45
	.word	.LBB28_14
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_4
	.word	.LBB28_4
	.word	.LBB28_13
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_16
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_17
	.word	.LBB28_26
	.word	.LBB28_2
	.word	.LBB28_29
	.word	.LBB28_2
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_2
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_12
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_8
	.word	.LBB28_18
	.word	.LBB28_24
	.word	.LBB28_29
	.word	.LBB28_28
	.word	.LBB28_29
	.word	.LBB28_25
	.word	.LBB28_5
	.word	.LBB28_3
	.word	.LBB28_29
	.word	.LBB28_3
	.word	.LBB28_29
	.word	.LBB28_7
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_10
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_29
	.word	.LBB28_9
	.word	.LBB28_29
	.word	.LBB28_51
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function arith_add
	.type	arith_add,@function
arith_add:                              # @arith_add
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r5, %hi(.L.str.80)
	addi r5, r5, %lo(.L.str.80)
	addi r4, r0, 0
	jal r31, arith
	addi r1, r0, 1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end29:
	.size	arith_add, .Lfunc_end29-arith_add
                                        # -- End function
	.p2align	2                               # -- Begin function arith_sub
	.type	arith_sub,@function
arith_sub:                              # @arith_sub
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	lui r5, %hi(.L.str.81)
	addi r5, r5, %lo(.L.str.81)
	addi r11, r0, 1
	add r4, r11, r0
	jal r31, arith
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end30:
	.size	arith_sub, .Lfunc_end30-arith_sub
                                        # -- End function
	.p2align	2                               # -- Begin function arith_mul
	.type	arith_mul,@function
arith_mul:                              # @arith_mul
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r5, %hi(.L.str.82)
	addi r5, r5, %lo(.L.str.82)
	addi r4, r0, 2
	jal r31, arith
	addi r1, r0, 1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end31:
	.size	arith_mul, .Lfunc_end31-arith_mul
                                        # -- End function
	.p2align	2                               # -- Begin function arith_mod
	.type	arith_mod,@function
arith_mod:                              # @arith_mod
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r5, %hi(.L.str.83)
	addi r5, r5, %lo(.L.str.83)
	addi r4, r0, 3
	jal r31, arith
	addi r1, r0, 1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end32:
	.size	arith_mod, .Lfunc_end32-arith_mod
                                        # -- End function
	.p2align	2                               # -- Begin function arith_pow
	.type	arith_pow,@function
arith_pow:                              # @arith_pow
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r5, %hi(.L.str.84)
	addi r5, r5, %lo(.L.str.84)
	addi r4, r0, 4
	jal r31, arith
	addi r1, r0, 1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end33:
	.size	arith_pow, .Lfunc_end33-arith_pow
                                        # -- End function
	.p2align	2                               # -- Begin function arith_div
	.type	arith_div,@function
arith_div:                              # @arith_div
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r5, %hi(.L.str.85)
	addi r5, r5, %lo(.L.str.85)
	addi r4, r0, 5
	jal r31, arith
	addi r1, r0, 1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end34:
	.size	arith_div, .Lfunc_end34-arith_div
                                        # -- End function
	.p2align	2                               # -- Begin function arith_idiv
	.type	arith_idiv,@function
arith_idiv:                             # @arith_idiv
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r5, %hi(.L.str.86)
	addi r5, r5, %lo(.L.str.86)
	addi r4, r0, 6
	jal r31, arith
	addi r1, r0, 1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end35:
	.size	arith_idiv, .Lfunc_end35-arith_idiv
                                        # -- End function
	.p2align	2                               # -- Begin function arith_unm
	.type	arith_unm,@function
arith_unm:                              # @arith_unm
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r5, %hi(.L.str.87)
	addi r5, r5, %lo(.L.str.87)
	addi r4, r0, 12
	jal r31, arith
	addi r1, r0, 1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end36:
	.size	arith_unm, .Lfunc_end36-arith_unm
                                        # -- End function
	.p2align	2                               # -- Begin function arith
	.type	arith,@function
arith:                                  # @arith
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
	add r13, r4, r0
	add r11, r3, r0
	addi r4, r0, 1
	jal r31, lua_type
	addi r4, r0, 1
	addi r15, r0, 3
	bne r1, r15, .LBB37_2
.LBB37_1:
	add r3, r11, r0
	jal r31, lua_pushvalue
	jal r0, .LBB37_4
.LBB37_2:
	addi r14, fp, -32
	add r3, r11, r0
	add r5, r14, r0
	jal r31, lua_tolstring
	addi r3, r0, 0
	beq r1, r3, .LBB37_9
.LBB37_3:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_stringtonumber
	ldw r3, r14+0
	addi r3, r3, 1
	bne r1, r3, .LBB37_9
.LBB37_4:
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_type
	addi r4, r0, 2
	bne r1, r15, .LBB37_6
.LBB37_5:
	add r3, r11, r0
	jal r31, lua_pushvalue
	jal r0, .LBB37_8
.LBB37_6:
	addi r14, fp, -28
	add r3, r11, r0
	add r5, r14, r0
	jal r31, lua_tolstring
	addi r3, r0, 0
	beq r1, r3, .LBB37_9
.LBB37_7:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_stringtonumber
	ldw r3, r14+0
	addi r3, r3, 1
	bne r1, r3, .LBB37_9
.LBB37_8:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_arith
	jal r0, .LBB37_13
.LBB37_9:
	addi r13, r0, 2
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_type
	addi r3, r0, 4
	bne r1, r3, .LBB37_11
.LBB37_10:
	addi r12, r12, 2
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_type
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_typename
	add r13, r1, r0
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_type
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_typename
	lui r4, %hi(.L.str.88)
	addi r4, r4, %lo(.L.str.88)
	add r3, r11, r0
	add r5, r12, r0
	add r6, r13, r0
	add r7, r1, r0
	jal r31, luaL_error
	jal r0, .LBB37_12
.LBB37_11:
	addi r4, r0, 2
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_getmetafield
	addi r3, r0, 0
	beq r1, r3, .LBB37_10
.LBB37_12:
	addi r4, r0, -3
	addi r12, r0, 1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_rotate
	addi r4, r0, 2
	addi r6, r0, 0
	add r3, r11, r0
	add r5, r12, r0
	add r7, r6, r0
	jal r31, lua_callk
.LBB37_13:
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
.Lfunc_end37:
	.size	arith, .Lfunc_end37-arith
                                        # -- End function
	.type	strlib,@object                  # @strlib
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
strlib:
	.word	.L.str
	.word	str_byte
	.word	.L.str.1
	.word	str_char
	.word	.L.str.2
	.word	str_dump
	.word	.L.str.3
	.word	str_find
	.word	.L.str.4
	.word	str_format
	.word	.L.str.5
	.word	gmatch
	.word	.L.str.6
	.word	str_gsub
	.word	.L.str.7
	.word	str_len
	.word	.L.str.8
	.word	str_lower
	.word	.L.str.9
	.word	str_match
	.word	.L.str.10
	.word	str_rep
	.word	.L.str.11
	.word	str_reverse
	.word	.L.str.12
	.word	str_sub
	.word	.L.str.13
	.word	str_upper
	.word	.L.str.14
	.word	str_pack
	.word	.L.str.15
	.word	str_packsize
	.word	.L.str.16
	.word	str_unpack
	.zero	8
	.size	strlib, 144

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"byte"
	.size	.L.str, 5

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"char"
	.size	.L.str.1, 5

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"dump"
	.size	.L.str.2, 5

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"find"
	.size	.L.str.3, 5

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"format"
	.size	.L.str.4, 7

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"gmatch"
	.size	.L.str.5, 7

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"gsub"
	.size	.L.str.6, 5

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"len"
	.size	.L.str.7, 4

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"lower"
	.size	.L.str.8, 6

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"match"
	.size	.L.str.9, 6

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"rep"
	.size	.L.str.10, 4

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"reverse"
	.size	.L.str.11, 8

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"sub"
	.size	.L.str.12, 4

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"upper"
	.size	.L.str.13, 6

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"pack"
	.size	.L.str.14, 5

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"packsize"
	.size	.L.str.15, 9

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"unpack"
	.size	.L.str.16, 7

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"string slice too long"
	.size	.L.str.17, 22

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"value out of range"
	.size	.L.str.18, 19

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"unable to dump given function"
	.size	.L.str.19, 30

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"^$*+?.([%-"
	.size	.L.str.20, 11

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"pattern too complex"
	.size	.L.str.21, 20

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"missing '[' after '%%f' in pattern"
	.size	.L.str.22, 35

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"too many captures"
	.size	.L.str.23, 18

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"invalid pattern capture"
	.size	.L.str.24, 24

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"malformed pattern (missing arguments to '%%b')"
	.size	.L.str.25, 47

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"malformed pattern (ends with '%%')"
	.size	.L.str.26, 35

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"malformed pattern (missing ']')"
	.size	.L.str.27, 32

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"invalid capture index %%%d"
	.size	.L.str.28, 27

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"unfinished capture"
	.size	.L.str.29, 19

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"no value"
	.size	.L.str.30, 9

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"-"
	.size	.L.str.31, 2

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"-+0 "
	.size	.L.str.32, 5

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"-0"
	.size	.L.str.33, 3

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"-#0"
	.size	.L.str.34, 4

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"l"
	.size	.L.str.35, 2

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"-+#0 "
	.size	.L.str.36, 6

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.zero	1
	.size	.L.str.37, 1

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"(null)"
	.size	.L.str.38, 7

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"specifier '%%q' cannot have modifiers"
	.size	.L.str.39, 38

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"string contains zeros"
	.size	.L.str.40, 22

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"invalid conversion '%s' to 'format'"
	.size	.L.str.41, 36

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"-+#0 123456789."
	.size	.L.str.42, 16

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"invalid format (too long)"
	.size	.L.str.43, 26

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"invalid conversion specification: '%s'"
	.size	.L.str.44, 39

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"modifiers for format '%%a'/'%%A' not implemented"
	.size	.L.str.45, 49

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"%.14g"
	.size	.L.str.46, 6

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"%.14gx0p+0"
	.size	.L.str.47, 11

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"p%+d"
	.size	.L.str.48, 5

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"0x%lx"
	.size	.L.str.49, 6

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"%ld"
	.size	.L.str.50, 4

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"value has no literal form"
	.size	.L.str.51, 26

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"\\%d"
	.size	.L.str.52, 4

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"\\%03d"
	.size	.L.str.53, 6

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"1e9999"
	.size	.L.str.54, 7

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"-1e9999"
	.size	.L.str.55, 8

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"(0/0)"
	.size	.L.str.56, 6

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"%a"
	.size	.L.str.57, 3

	.type	.L.str.58,@object               # @.str.58
.L.str.58:
	.asciz	"%s"
	.size	.L.str.58, 3

	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"string/function/table"
	.size	.L.str.59, 22

	.type	.L.str.60,@object               # @.str.60
.L.str.60:
	.asciz	"invalid replacement value (a %s)"
	.size	.L.str.60, 33

	.type	.L.str.61,@object               # @.str.61
.L.str.61:
	.asciz	"invalid use of '%c' in replacement string"
	.size	.L.str.61, 42

	.type	.L.str.62,@object               # @.str.62
.L.str.62:
	.asciz	"resulting string too large"
	.size	.L.str.62, 27

	.type	.L.str.63,@object               # @.str.63
.L.str.63:
	.asciz	"integer overflow"
	.size	.L.str.63, 17

	.type	.L.str.64,@object               # @.str.64
.L.str.64:
	.asciz	"unsigned overflow"
	.size	.L.str.64, 18

	.type	.L.str.65,@object               # @.str.65
.L.str.65:
	.asciz	"string longer than given size"
	.size	.L.str.65, 30

	.type	.L.str.66,@object               # @.str.66
.L.str.66:
	.asciz	"string length does not fit in given size"
	.size	.L.str.66, 41

	.type	.L.str.67,@object               # @.str.67
.L.str.67:
	.asciz	"invalid next option for option 'X'"
	.size	.L.str.67, 35

	.type	.L.str.68,@object               # @.str.68
.L.str.68:
	.asciz	"format asks for alignment not power of 2"
	.size	.L.str.68, 41

	.type	.L.str.69,@object               # @.str.69
.L.str.69:
	.asciz	"missing size for format option 'c'"
	.size	.L.str.69, 35

	.type	.L.str.70,@object               # @.str.70
.L.str.70:
	.asciz	"invalid format option '%c'"
	.size	.L.str.70, 27

	.type	.L.str.71,@object               # @.str.71
.L.str.71:
	.asciz	"integral size (%d) out of limits [1,%d]"
	.size	.L.str.71, 40

	.type	.L.str.72,@object               # @.str.72
.L.str.72:
	.asciz	"variable-length format"
	.size	.L.str.72, 23

	.type	.L.str.73,@object               # @.str.73
.L.str.73:
	.asciz	"format result too large"
	.size	.L.str.73, 24

	.type	.L.str.74,@object               # @.str.74
.L.str.74:
	.asciz	"initial position out of string"
	.size	.L.str.74, 31

	.type	.L.str.75,@object               # @.str.75
.L.str.75:
	.asciz	"data string too short"
	.size	.L.str.75, 22

	.type	.L.str.76,@object               # @.str.76
.L.str.76:
	.asciz	"too many results"
	.size	.L.str.76, 17

	.type	.L.str.77,@object               # @.str.77
.L.str.77:
	.asciz	"unfinished string for format 'z'"
	.size	.L.str.77, 33

	.type	.L.str.78,@object               # @.str.78
.L.str.78:
	.asciz	"%d-byte integer does not fit into Lua Integer"
	.size	.L.str.78, 46

	.type	stringmetamethods,@object       # @stringmetamethods
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
stringmetamethods:
	.word	.L.str.80
	.word	arith_add
	.word	.L.str.81
	.word	arith_sub
	.word	.L.str.82
	.word	arith_mul
	.word	.L.str.83
	.word	arith_mod
	.word	.L.str.84
	.word	arith_pow
	.word	.L.str.85
	.word	arith_div
	.word	.L.str.86
	.word	arith_idiv
	.word	.L.str.87
	.word	arith_unm
	.word	.L.str.79
	.word	0
	.zero	8
	.size	stringmetamethods, 80

	.type	.L.str.79,@object               # @.str.79
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.79:
	.asciz	"__index"
	.size	.L.str.79, 8

	.type	.L.str.80,@object               # @.str.80
.L.str.80:
	.asciz	"__add"
	.size	.L.str.80, 6

	.type	.L.str.81,@object               # @.str.81
.L.str.81:
	.asciz	"__sub"
	.size	.L.str.81, 6

	.type	.L.str.82,@object               # @.str.82
.L.str.82:
	.asciz	"__mul"
	.size	.L.str.82, 6

	.type	.L.str.83,@object               # @.str.83
.L.str.83:
	.asciz	"__mod"
	.size	.L.str.83, 6

	.type	.L.str.84,@object               # @.str.84
.L.str.84:
	.asciz	"__pow"
	.size	.L.str.84, 6

	.type	.L.str.85,@object               # @.str.85
.L.str.85:
	.asciz	"__div"
	.size	.L.str.85, 6

	.type	.L.str.86,@object               # @.str.86
.L.str.86:
	.asciz	"__idiv"
	.size	.L.str.86, 7

	.type	.L.str.87,@object               # @.str.87
.L.str.87:
	.asciz	"__unm"
	.size	.L.str.87, 6

	.type	.L.str.88,@object               # @.str.88
.L.str.88:
	.asciz	"attempt to %s a '%s' with a '%s'"
	.size	.L.str.88, 33

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
