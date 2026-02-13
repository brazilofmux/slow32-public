	.file	"loslib.c"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaopen_os
.LCPI0_0:
	.quad	0x407f800000000000              # double 504
	.text
	.globl	luaopen_os
	.p2align	2
	.type	luaopen_os,@function
luaopen_os:                             # @luaopen_os
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
	addi r5, r0, 11
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_createtable
	lui r4, %hi(syslib)
	addi r4, r4, %lo(syslib)
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
	.size	luaopen_os, .Lfunc_end0-luaopen_os
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function os_clock
.LCPI1_0:
	.quad	0x412e848000000000              # double 1.0E+6
	.text
	.p2align	2
	.type	os_clock,@function
os_clock:                               # @os_clock
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	jal r31, clock
	fcvt.d.wu r4, r1
	lui r1, %hi(.LCPI1_0)
	addi r1, r1, %lo(.LCPI1_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fdiv.d r6, r4, r6
	add r3, r11, r0
	add r5, r6, r0
	add r6, r7, r0
	jal r31, lua_pushnumber
	addi r1, r0, 1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	os_clock, .Lfunc_end1-os_clock
                                        # -- End function
	.p2align	2                               # -- Begin function os_date
	.type	os_date,@function
os_date:                                # @os_date
# %bb.0:
	addi sp, sp, -648
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 648
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
	add r12, r3, r0
	lui r5, %hi(.L.str.11)
	addi r5, r5, %lo(.L.str.11)
	addi r11, r0, 1
	addi r13, fp, -80
	add r4, r11, r0
	add r6, r13, r0
	jal r31, luaL_optlstring
	add r15, r1, r0
	addi r4, r0, 2
	add r3, r12, r0
	jal r31, lua_type
	addi r24, r0, 0
	ble r1, r24, .LBB2_2
.LBB2_1:
	addi r4, r0, 2
	add r3, r12, r0
	jal r31, luaL_checkinteger
	srai r3, r1, 31
	jal r0, .LBB2_3
.LBB2_2:
	addi r3, r0, 0
	jal r31, time
	add r3, r2, r0
.LBB2_3:
	addi r4, fp, -88
	stw r4+0, r1
	stw r4+4, r3
	ldw r16, r13+0
	ldbu r1, r15+0
	addi r3, r0, 33
	bne r1, r3, .LBB2_23
.LBB2_4:
	addi r3, fp, -88
	jal r31, gmtime
	add r13, r1, r0
	addi r14, r15, 1
	beq r13, r24, .LBB2_24
.LBB2_5:
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	add r3, r14, r0
	jal r31, strcmp
	beq r1, r24, .LBB2_25
.LBB2_6:
	stw fp+-628, r11
	add r25, r15, r16
	addi r26, r0, 37
	addi r15, fp, -92
	stb r15+0, r26
	addi r16, fp, -620
	add r3, r12, r0
	add r4, r16, r0
	jal r31, luaL_buffinit
	bgeu r14, r25, .LBB2_22
.LBB2_7:
	addi r1, r15, 1
	stw fp+-624, r1
	addi r18, r0, 250
	addi r19, r0, 1
	lui r27, %hi(.L.str.24)
	addi r27, r27, %lo(.L.str.24)
	addi r28, r0, 97
	addi r17, r0, 124
	lui r20, %hi(.L.str.25)
	addi r20, r20, %lo(.L.str.25)
	jal r0, .LBB2_9
.LBB2_8:
	addi r1, r14, 1
	ldbu r3, r14+0
	ldw r4, r16+0
	ldw r5, r16+8
	addi r6, r5, 1
	stw r16+8, r6
	add r4, r4, r5
	stb r4+0, r3
	add r14, r1, r0
	bgeu r14, r25, .LBB2_22
.LBB2_9:
	ldbu r1, r14+0
	bne r1, r26, .LBB2_18
.LBB2_10:
	add r3, r16, r0
	add r4, r18, r0
	jal r31, luaL_prepbuffsize
	add r21, r1, r0
	addi r14, r14, 1
	sub r11, r25, r14
	blt r11, r19, .LBB2_17
.LBB2_11:
	add r1, r28, r0
	add r22, r19, r0
	add r23, r27, r0
.LBB2_12:
	andi r1, r1, 255
	bne r1, r17, .LBB2_14
.LBB2_13:
	addi r22, r22, 1
	jal r0, .LBB2_15
.LBB2_14:
	add r3, r14, r0
	add r4, r23, r0
	add r5, r22, r0
	jal r31, memcmp
	beq r1, r24, .LBB2_20
.LBB2_15:
	bgt r22, r11, .LBB2_17
.LBB2_16:
	add r23, r23, r22
	ldbu r1, r23+0
	andi r3, r1, 255
	bne r3, r24, .LBB2_12
.LBB2_17:
	add r3, r12, r0
	add r4, r20, r0
	add r5, r14, r0
	jal r31, lua_pushfstring
	add r3, r12, r0
	add r4, r19, r0
	add r5, r1, r0
	jal r31, luaL_argerror
	jal r0, .LBB2_21
.LBB2_18:
	ldw r1, r16+8
	ldw r3, r16+4
	bltu r1, r3, .LBB2_8
.LBB2_19:
	add r3, r16, r0
	add r4, r19, r0
	jal r31, luaL_prepbuffsize
	jal r0, .LBB2_8
.LBB2_20:
	ldw r11, fp+-624
	add r3, r11, r0
	add r4, r14, r0
	add r5, r22, r0
	jal r31, memcpy
	add r1, r11, r22
	stb r1+0, r24
	add r14, r14, r22
.LBB2_21:
	add r3, r21, r0
	add r4, r18, r0
	add r5, r15, r0
	add r6, r13, r0
	jal r31, strftime
	ldw r3, r16+8
	add r1, r3, r1
	stw r16+8, r1
	bltu r14, r25, .LBB2_9
.LBB2_22:
	addi r3, fp, -620
	jal r31, luaL_pushresult
	ldw r11, fp+-628
	jal r0, .LBB2_26
.LBB2_23:
	addi r3, fp, -88
	jal r31, localtime
	add r13, r1, r0
	add r14, r15, r0
	bne r13, r24, .LBB2_5
.LBB2_24:
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
	add r3, r12, r0
	jal r31, luaL_error
	add r11, r1, r0
	jal r0, .LBB2_26
.LBB2_25:
	addi r4, r0, 0
	addi r5, r0, 9
	add r3, r12, r0
	jal r31, lua_createtable
	add r3, r12, r0
	add r4, r13, r0
	jal r31, setallfields
.LBB2_26:
	add r1, r11, r0
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
	addi sp, sp, 648
	jalr r0, r31, 0
.Lfunc_end2:
	.size	os_date, .Lfunc_end2-os_date
                                        # -- End function
	.p2align	2                               # -- Begin function os_difftime
	.type	os_difftime,@function
os_difftime:                            # @os_difftime
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
	jal r31, luaL_checkinteger
	add r13, r1, r0
	srai r14, r1, 31
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_checkinteger
	srai r6, r1, 31
	add r3, r13, r0
	add r4, r14, r0
	add r5, r1, r0
	jal r31, difftime
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
.Lfunc_end3:
	.size	os_difftime, .Lfunc_end3-os_difftime
                                        # -- End function
	.p2align	2                               # -- Begin function os_execute
	.type	os_execute,@function
os_execute:                             # @os_execute
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
	addi r13, r0, 0
	add r4, r11, r0
	add r5, r13, r0
	add r6, r13, r0
	jal r31, luaL_optlstring
	lui r3, %hi(errno)
	addi r3, r3, %lo(errno)
	stw r3+0, r13
	sne r3, r1, r13
	sub r4, r13, r3
	add r3, r12, r0
	beq r1, r13, .LBB4_2
.LBB4_1:
	jal r31, luaL_execresult
	add r11, r1, r0
	jal r0, .LBB4_3
.LBB4_2:
	jal r31, lua_pushboolean
.LBB4_3:
	add r1, r11, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	os_execute, .Lfunc_end4-os_execute
                                        # -- End function
	.p2align	2                               # -- Begin function os_exit
	.type	os_exit,@function
os_exit:                                # @os_exit
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
	jal r31, lua_type
	addi r4, r0, 1
	bne r1, r12, .LBB5_2
.LBB5_1:
	add r3, r11, r0
	jal r31, lua_toboolean
	addi r3, r0, 0
	seq r12, r1, r3
	jal r0, .LBB5_3
.LBB5_2:
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, luaL_optinteger
	add r12, r1, r0
.LBB5_3:
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_toboolean
	addi r13, r0, 0
	beq r1, r13, .LBB5_5
.LBB5_4:
	add r3, r11, r0
	jal r31, lua_close
.LBB5_5:
	beq r11, r13, .LBB5_7
.LBB5_6:
	add r3, r12, r0
	jal r31, exit
.LBB5_7:
	addi r1, r0, 0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	os_exit, .Lfunc_end5-os_exit
                                        # -- End function
	.p2align	2                               # -- Begin function os_getenv
	.type	os_getenv,@function
os_getenv:                              # @os_getenv
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
	addi r5, r0, 0
	add r4, r12, r0
	jal r31, luaL_checklstring
	add r3, r1, r0
	jal r31, getenv
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushstring
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	os_getenv, .Lfunc_end6-os_getenv
                                        # -- End function
	.p2align	2                               # -- Begin function os_remove
	.type	os_remove,@function
os_remove:                              # @os_remove
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
	addi r12, r0, 0
	add r5, r12, r0
	jal r31, luaL_checklstring
	lui r3, %hi(errno)
	addi r3, r3, %lo(errno)
	stw r3+0, r12
	add r3, r11, r0
	add r4, r12, r0
	add r5, r1, r0
	jal r31, luaL_fileresult
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end7:
	.size	os_remove, .Lfunc_end7-os_remove
                                        # -- End function
	.p2align	2                               # -- Begin function os_rename
	.type	os_rename,@function
os_rename:                              # @os_rename
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
	addi r12, r0, 0
	add r5, r12, r0
	jal r31, luaL_checklstring
	addi r4, r0, 2
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_checklstring
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	stw r1+0, r12
	add r3, r11, r0
	add r4, r12, r0
	add r5, r12, r0
	jal r31, luaL_fileresult
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	os_rename, .Lfunc_end8-os_rename
                                        # -- End function
	.p2align	2                               # -- Begin function os_setlocale
	.type	os_setlocale,@function
os_setlocale:                           # @os_setlocale
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
	addi r5, r0, 0
	add r4, r12, r0
	add r6, r5, r0
	jal r31, luaL_optlstring
	lui r5, %hi(.L.str.26)
	addi r5, r5, %lo(.L.str.26)
	lui r6, %hi(os_setlocale.catnames)
	addi r6, r6, %lo(os_setlocale.catnames)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_checkoption
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	add r3, r11, r0
	jal r31, lua_pushstring
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end9:
	.size	os_setlocale, .Lfunc_end9-os_setlocale
                                        # -- End function
	.p2align	2                               # -- Begin function os_time
	.type	os_time,@function
os_time:                                # @os_time
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
	stw fp+-36, lr
	add r12, r3, r0
	addi r11, r0, 1
	add r4, r11, r0
	jal r31, lua_type
	addi r14, r0, 0
	ble r1, r14, .LBB10_14
.LBB10_1:
	addi r15, r0, 1
	addi r5, r0, 5
	add r3, r12, r0
	add r4, r15, r0
	jal r31, luaL_checktype
	add r3, r12, r0
	add r4, r15, r0
	jal r31, lua_settop
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	addi r16, r0, -1
	addi r6, r0, 1900
	add r3, r12, r0
	add r5, r16, r0
	jal r31, getfield
	addi r13, fp, -76
	stw r13+20, r1
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	add r3, r12, r0
	add r5, r16, r0
	add r6, r15, r0
	jal r31, getfield
	stw r13+16, r1
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r12, r0
	add r5, r16, r0
	add r6, r14, r0
	jal r31, getfield
	stw r13+12, r1
	lui r5, %hi(.L.str.18)
	addi r5, r5, %lo(.L.str.18)
	add r3, r12, r0
	add r4, r16, r0
	jal r31, lua_getfield
	add r17, r1, r0
	addi r15, fp, -40
	add r3, r12, r0
	add r4, r16, r0
	add r5, r15, r0
	jal r31, lua_tointegerx
	add r16, r1, r0
	ldw r1, r15+0
	bne r1, r14, .LBB10_4
.LBB10_2:
	addi r1, r0, 0
	bne r17, r1, .LBB10_20
.LBB10_3:
	addi r16, r0, 12
.LBB10_4:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
.LBB10_5:
	stw r13+8, r16
	lui r5, %hi(.L.str.19)
	addi r5, r5, %lo(.L.str.19)
	addi r16, r0, -1
	add r3, r12, r0
	add r4, r16, r0
	jal r31, lua_getfield
	add r17, r1, r0
	add r3, r12, r0
	add r4, r16, r0
	add r5, r15, r0
	jal r31, lua_tointegerx
	add r16, r1, r0
	ldw r1, r15+0
	bne r1, r14, .LBB10_7
.LBB10_6:
	addi r16, r0, 0
	bne r17, r16, .LBB10_21
.LBB10_7:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
.LBB10_8:
	stw r13+4, r16
	lui r5, %hi(.L.str.20)
	addi r5, r5, %lo(.L.str.20)
	addi r16, r0, -1
	add r3, r12, r0
	add r4, r16, r0
	jal r31, lua_getfield
	add r18, r1, r0
	add r3, r12, r0
	add r4, r16, r0
	add r5, r15, r0
	jal r31, lua_tointegerx
	add r17, r1, r0
	ldw r1, r15+0
	bne r1, r14, .LBB10_10
.LBB10_9:
	add r17, r14, r0
	bne r18, r14, .LBB10_22
.LBB10_10:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
.LBB10_11:
	stw r13+0, r17
	lui r5, %hi(.L.str.23)
	addi r5, r5, %lo(.L.str.23)
	add r3, r12, r0
	add r4, r16, r0
	jal r31, lua_getfield
	beq r1, r14, .LBB10_13
.LBB10_12:
	addi r4, r0, -1
	add r3, r12, r0
	jal r31, lua_toboolean
	add r16, r1, r0
.LBB10_13:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
	stw r13+32, r16
	add r3, r13, r0
	jal r31, mktime
	add r14, r1, r0
	add r15, r2, r0
	add r3, r12, r0
	add r4, r13, r0
	jal r31, setallfields
	jal r0, .LBB10_15
.LBB10_14:
	addi r3, r0, 0
	jal r31, time
	add r14, r1, r0
	add r15, r2, r0
.LBB10_15:
	and r1, r14, r15
	addi r3, r0, -1
	beq r1, r3, .LBB10_18
.LBB10_16:
	srai r1, r14, 31
	xor r3, r14, r14
	xor r1, r15, r1
	or  r1, r3, r1
	addi r3, r0, 0
	bne r1, r3, .LBB10_18
.LBB10_17:
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_pushinteger
	jal r0, .LBB10_19
.LBB10_18:
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
	add r3, r12, r0
	jal r31, luaL_error
	add r11, r1, r0
.LBB10_19:
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
	addi sp, sp, 88
	jalr r0, r31, 0
.LBB10_20:
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	lui r5, %hi(.L.str.18)
	addi r5, r5, %lo(.L.str.18)
	add r3, r12, r0
	jal r31, luaL_error
	add r16, r1, r0
	jal r0, .LBB10_5
.LBB10_21:
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	lui r5, %hi(.L.str.19)
	addi r5, r5, %lo(.L.str.19)
	add r3, r12, r0
	jal r31, luaL_error
	add r16, r1, r0
	jal r0, .LBB10_8
.LBB10_22:
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	lui r5, %hi(.L.str.20)
	addi r5, r5, %lo(.L.str.20)
	add r3, r12, r0
	jal r31, luaL_error
	add r17, r1, r0
	jal r0, .LBB10_11
.Lfunc_end10:
	.size	os_time, .Lfunc_end10-os_time
                                        # -- End function
	.p2align	2                               # -- Begin function os_tmpname
	.type	os_tmpname,@function
os_tmpname:                             # @os_tmpname
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r4, %hi(.L.str.36)
	addi r4, r4, %lo(.L.str.36)
	jal r31, luaL_error
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end11:
	.size	os_tmpname, .Lfunc_end11-os_tmpname
                                        # -- End function
	.p2align	2                               # -- Begin function setallfields
	.type	setallfields,@function
setallfields:                           # @setallfields
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
	add r12, r4, r0
	add r11, r3, r0
	ldw r1, r4+20
	addi r4, r1, 1900
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.15)
	addi r5, r5, %lo(.L.str.15)
	addi r13, r0, -2
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	ldw r1, r12+16
	addi r4, r1, 1
	add r3, r11, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	ldw r4, r12+12
	add r3, r11, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.17)
	addi r5, r5, %lo(.L.str.17)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	ldw r4, r12+8
	add r3, r11, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.18)
	addi r5, r5, %lo(.L.str.18)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	ldw r4, r12+4
	add r3, r11, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.19)
	addi r5, r5, %lo(.L.str.19)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	ldw r4, r12+0
	add r3, r11, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.20)
	addi r5, r5, %lo(.L.str.20)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	ldw r1, r12+28
	addi r4, r1, 1
	add r3, r11, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.21)
	addi r5, r5, %lo(.L.str.21)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	ldw r1, r12+24
	addi r4, r1, 1
	add r3, r11, r0
	jal r31, lua_pushinteger
	lui r5, %hi(.L.str.22)
	addi r5, r5, %lo(.L.str.22)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_setfield
	ldw r4, r12+32
	addi r1, r0, 0
	blt r4, r1, .LBB12_2
.LBB12_1:
	add r3, r11, r0
	jal r31, lua_pushboolean
	lui r5, %hi(.L.str.23)
	addi r5, r5, %lo(.L.str.23)
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_setfield
.LBB12_2:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end12:
	.size	setallfields, .Lfunc_end12-setallfields
                                        # -- End function
	.p2align	2                               # -- Begin function getfield
	.type	getfield,@function
getfield:                               # @getfield
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
	add r14, r6, r0
	add r11, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	addi r15, r0, -1
	add r4, r15, r0
	add r5, r13, r0
	jal r31, lua_getfield
	add r16, r1, r0
	addi r17, fp, -36
	add r3, r12, r0
	add r4, r15, r0
	add r5, r17, r0
	jal r31, lua_tointegerx
	ldw r4, r17+0
	addi r3, r0, 0
	beq r4, r3, .LBB13_5
.LBB13_1:
	bgt r1, r15, .LBB13_8
.LBB13_2:
	lui r3, 524288
	or  r3, r14, r3
	ble r3, r1, .LBB13_8
.LBB13_3:
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
.LBB13_4:
	add r3, r12, r0
	add r5, r13, r0
	jal r31, luaL_error
	add r11, r1, r0
	jal r0, .LBB13_10
.LBB13_5:
	bne r16, r3, .LBB13_11
.LBB13_6:
	bgt r11, r15, .LBB13_9
.LBB13_7:
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
	jal r0, .LBB13_4
.LBB13_8:
	sub r11, r1, r14
.LBB13_9:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
.LBB13_10:
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
	addi sp, sp, 56
	jalr r0, r31, 0
.LBB13_11:
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	jal r0, .LBB13_4
.Lfunc_end13:
	.size	getfield, .Lfunc_end13-getfield
                                        # -- End function
	.type	syslib,@object                  # @syslib
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
syslib:
	.word	.L.str
	.word	os_clock
	.word	.L.str.1
	.word	os_date
	.word	.L.str.2
	.word	os_difftime
	.word	.L.str.3
	.word	os_execute
	.word	.L.str.4
	.word	os_exit
	.word	.L.str.5
	.word	os_getenv
	.word	.L.str.6
	.word	os_remove
	.word	.L.str.7
	.word	os_rename
	.word	.L.str.8
	.word	os_setlocale
	.word	.L.str.9
	.word	os_time
	.word	.L.str.10
	.word	os_tmpname
	.zero	8
	.size	syslib, 96

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"clock"
	.size	.L.str, 6

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"date"
	.size	.L.str.1, 5

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"difftime"
	.size	.L.str.2, 9

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"execute"
	.size	.L.str.3, 8

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"exit"
	.size	.L.str.4, 5

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"getenv"
	.size	.L.str.5, 7

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"remove"
	.size	.L.str.6, 7

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"rename"
	.size	.L.str.7, 7

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"setlocale"
	.size	.L.str.8, 10

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"time"
	.size	.L.str.9, 5

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"tmpname"
	.size	.L.str.10, 8

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"%c"
	.size	.L.str.11, 3

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"date result cannot be represented in this installation"
	.size	.L.str.12, 55

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"*t"
	.size	.L.str.13, 3

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"year"
	.size	.L.str.15, 5

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"month"
	.size	.L.str.16, 6

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"day"
	.size	.L.str.17, 4

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"hour"
	.size	.L.str.18, 5

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"min"
	.size	.L.str.19, 4

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"sec"
	.size	.L.str.20, 4

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"yday"
	.size	.L.str.21, 5

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"wday"
	.size	.L.str.22, 5

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"isdst"
	.size	.L.str.23, 6

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"aAbBcdHIjmMpSUwWxXyYZ%"
	.size	.L.str.24, 23

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"invalid conversion specifier '%%%s'"
	.size	.L.str.25, 36

	.type	os_setlocale.catnames,@object   # @os_setlocale.catnames
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
os_setlocale.catnames:
	.word	.L.str.26
	.word	.L.str.27
	.word	.L.str.28
	.word	.L.str.29
	.word	.L.str.30
	.word	.L.str.9
	.word	0
	.size	os_setlocale.catnames, 28

	.type	.L.str.26,@object               # @.str.26
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.26:
	.asciz	"all"
	.size	.L.str.26, 4

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"collate"
	.size	.L.str.27, 8

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"ctype"
	.size	.L.str.28, 6

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"monetary"
	.size	.L.str.29, 9

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"numeric"
	.size	.L.str.30, 8

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"C"
	.size	.L.str.31, 2

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"time result cannot be represented in this installation"
	.size	.L.str.32, 55

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"field '%s' is not an integer"
	.size	.L.str.33, 29

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"field '%s' missing in date table"
	.size	.L.str.34, 33

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"field '%s' is out-of-bound"
	.size	.L.str.35, 27

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"unable to generate a unique filename"
	.size	.L.str.36, 37

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
