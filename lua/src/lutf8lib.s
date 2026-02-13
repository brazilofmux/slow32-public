	.file	"lutf8lib.c"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaopen_utf8
.LCPI0_0:
	.quad	0x407f800000000000              # double 504
	.text
	.globl	luaopen_utf8
	.p2align	2
	.type	luaopen_utf8,@function
luaopen_utf8:                           # @luaopen_utf8
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
	addi r5, r0, 6
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_createtable
	lui r4, %hi(funcs)
	addi r4, r4, %lo(funcs)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_setfuncs
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	addi r5, r0, 14
	add r3, r11, r0
	jal r31, lua_pushlstring
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r4, r0, -2
	add r3, r11, r0
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
	.size	luaopen_utf8, .Lfunc_end0-luaopen_utf8
                                        # -- End function
	.p2align	2                               # -- Begin function byteoffset
	.type	byteoffset,@function
byteoffset:                             # @byteoffset
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
	addi r13, r0, 1
	addi r16, fp, -44
	add r4, r13, r0
	add r5, r16, r0
	jal r31, luaL_checklstring
	add r15, r1, r0
	addi r12, r0, 2
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_checkinteger
	add r14, r1, r0
	addi r19, r0, -1
	sgt r1, r1, r19
	ldw r3, r16+0
	addi r3, r3, 1
	addi r17, r0, 0
	sub r1, r17, r1
	xori r4, r3, 1
	and r1, r4, r1
	xor r5, r3, r1
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, luaL_optinteger
	ldw r3, r16+0
	add r18, r1, r0
	bgt r1, r19, .LBB1_3
.LBB1_1:
	sub r4, r17, r1
	add r18, r17, r0
	bltu r3, r4, .LBB1_3
.LBB1_2:
	add r1, r1, r3
	addi r18, r1, 1
.LBB1_3:
	sgt r4, r18, r17
	addi r1, r18, -1
	sub r4, r17, r4
	xor r5, r1, r18
	and r17, r5, r4
	blt r18, r13, .LBB1_37
.LBB1_4:
	bgt r1, r3, .LBB1_37
.LBB1_5:
	xor r3, r18, r17
	addi r1, r0, 0
	beq r14, r1, .LBB1_8
.LBB1_6:
	add r4, r15, r3
	ldb r4, r4+0
	addi r5, r0, -65
	bgt r4, r5, .LBB1_13
.LBB1_7:
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB1_36
.LBB1_8:
	addi r4, r0, 1
	blt r3, r4, .LBB1_20
.LBB1_9:
	addi r13, r0, 0
	addi r5, r0, -65
.LBB1_10:
	add r6, r3, r0
	add r3, r15, r3
	ldb r3, r3+0
	bgt r3, r5, .LBB1_21
.LBB1_11:
	addi r3, r6, -1
	bgt r6, r4, .LBB1_10
.LBB1_12:
	add r3, r13, r0
	bne r13, r1, .LBB1_32
	jal r0, .LBB1_34
.LBB1_13:
	blt r14, r1, .LBB1_22
.LBB1_14:
	addi r4, r14, -1
	blt r14, r12, .LBB1_33
.LBB1_15:
	ldw r5, r16+0
	bge r3, r5, .LBB1_30
.LBB1_16:
	addi r6, r15, 1
	addi r7, r0, -64
.LBB1_17:
	add r8, r3, r0
	addi r3, r3, 1
	add r8, r6, r8
	ldb r8, r8+0
	blt r8, r7, .LBB1_17
.LBB1_18:
	addi r13, r4, -1
	blt r4, r12, .LBB1_31
.LBB1_19:
	add r4, r13, r0
	blt r3, r5, .LBB1_17
	jal r0, .LBB1_31
.LBB1_20:
	add r13, r1, r0
	bne r13, r1, .LBB1_32
	jal r0, .LBB1_34
.LBB1_21:
	add r3, r6, r0
	bne r13, r1, .LBB1_32
	jal r0, .LBB1_34
.LBB1_22:
	blt r3, r13, .LBB1_31
.LBB1_23:
	addi r4, r15, -1
	addi r5, r0, 1
	addi r6, r0, -64
	addi r7, r0, -2
.LBB1_24:
	slt r8, r3, r5
	sub r8, r1, r8
	xori r9, r3, 1
	and r8, r9, r8
	xori r8, r8, 1
	addi r9, r8, -1
.LBB1_25:
	add r8, r3, r0
	blt r3, r12, .LBB1_27
.LBB1_26:
	addi r3, r8, -1
	add r10, r4, r8
	ldb r10, r10+0
	blt r10, r6, .LBB1_25
	jal r0, .LBB1_28
.LBB1_27:
	add r3, r9, r0
.LBB1_28:
	addi r13, r14, 1
	bgt r14, r7, .LBB1_31
.LBB1_29:
	add r14, r13, r0
	bgt r8, r5, .LBB1_24
	jal r0, .LBB1_31
.LBB1_30:
	add r13, r4, r0
.LBB1_31:
	beq r13, r1, .LBB1_34
.LBB1_32:
	add r3, r11, r0
	jal r31, lua_pushnil
	jal r0, .LBB1_35
.LBB1_33:
	add r13, r4, r0
	bne r13, r1, .LBB1_32
.LBB1_34:
	addi r4, r3, 1
	add r3, r11, r0
	jal r31, lua_pushinteger
.LBB1_35:
	addi r1, r0, 1
.LBB1_36:
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
.LBB1_37:
	lui r5, %hi(.L.str.7)
	addi r5, r5, %lo(.L.str.7)
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB1_5
.Lfunc_end1:
	.size	byteoffset, .Lfunc_end1-byteoffset
                                        # -- End function
	.p2align	2                               # -- Begin function codepoint
	.type	codepoint,@function
codepoint:                              # @codepoint
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
	stw fp+-64, r26
	stw fp+-68, lr
	add r11, r3, r0
	addi r12, r0, 1
	addi r16, fp, -72
	add r4, r12, r0
	add r5, r16, r0
	jal r31, luaL_checklstring
	add r14, r1, r0
	addi r4, r0, 2
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_optinteger
	addi r17, r0, -1
	add r15, r1, r0
	bgt r1, r17, .LBB2_3
.LBB2_1:
	ldw r3, r16+0
	addi r15, r0, 0
	sub r4, r15, r1
	bltu r3, r4, .LBB2_3
.LBB2_2:
	add r1, r1, r3
	addi r15, r1, 1
.LBB2_3:
	addi r4, r0, 3
	add r3, r11, r0
	add r5, r15, r0
	jal r31, luaL_optinteger
	add r18, r1, r0
	bgt r1, r17, .LBB2_6
.LBB2_4:
	ldw r3, r16+0
	addi r18, r0, 0
	sub r4, r18, r1
	bltu r3, r4, .LBB2_6
.LBB2_5:
	add r1, r1, r3
	addi r18, r1, 1
.LBB2_6:
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, lua_toboolean
	add r12, r1, r0
	addi r13, r0, 0
	ble r15, r13, .LBB2_33
.LBB2_7:
	ldw r1, r16+0
	bgt r18, r1, .LBB2_34
.LBB2_8:
	blt r18, r15, .LBB2_32
.LBB2_9:
	sub r1, r18, r15
	lui r3, 524288
	addi r3, r3, -1
	bne r1, r3, .LBB2_11
.LBB2_10:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	add r3, r11, r0
	jal r31, luaL_error
	add r13, r1, r0
	jal r0, .LBB2_32
.LBB2_11:
	addi r4, r1, 1
	lui r5, %hi(.L.str.10)
	addi r5, r5, %lo(.L.str.10)
	add r3, r11, r0
	jal r31, luaL_checkstack
	add r16, r14, r18
	add r1, r14, r15
	addi r26, r1, -1
	addi r18, r0, 0
	lui r1, 512
	addi r19, r1, -2048
	lui r1, 272
	addi r20, r1, -1
	lui r1, 14
	addi r21, r1, -2048
	lui r14, %hi(.L.str.11)
	addi r14, r14, %lo(.L.str.11)
	addi r22, r0, 5
	lui r23, %hi(utf8_decode.limits)
	addi r23, r23, %lo(utf8_decode.limits)
	addi r24, r0, 128
                                        # implicit-def: $r15
	add r25, r18, r0
                                        # implicit-def: $r13
	jal r0, .LBB2_13
.LBB2_12:
	add r4, r14, r0
	jal r31, luaL_error
	add r13, r1, r0
	beq r26, r18, .LBB2_32
.LBB2_13:
	bgeu r26, r16, .LBB2_31
.LBB2_14:
	ldb r3, r26+0
	andi r1, r3, 255
	ble r3, r17, .LBB2_18
.LBB2_15:
	add r3, r26, r0
	bne r12, r18, .LBB2_28
.LBB2_16:
	bgtu r1, r20, .LBB2_26
.LBB2_17:
	and r4, r1, r19
	add r26, r18, r0
	bne r4, r21, .LBB2_28
	jal r0, .LBB2_29
.LBB2_18:
	andi r3, r1, 64
	bne r3, r18, .LBB2_20
.LBB2_19:
	add r6, r1, r0
	add r4, r18, r0
	add r3, r18, r0
	jal r0, .LBB2_23
.LBB2_20:
	addi r5, r26, 1
	add r3, r18, r0
	add r4, r18, r0
.LBB2_21:
	add r6, r5, r3
	ldbu r6, r6+0
	andi r7, r6, 192
	bne r7, r24, .LBB2_26
.LBB2_22:
	slli r4, r4, 6
	andi r6, r6, 63
	or  r4, r6, r4
	slli r6, r1, 1
	andi r7, r1, 32
	addi r3, r3, 1
	add r1, r6, r0
	bne r7, r18, .LBB2_21
.LBB2_23:
	bgtu r3, r22, .LBB2_26
.LBB2_24:
	andi r1, r6, 63
	slli r5, r3, 2
	add r6, r5, r3
	sll r1, r1, r6
	or  r1, r1, r4
	blt r1, r18, .LBB2_26
.LBB2_25:
	add r4, r5, r23
	ldw r4, r4+0
	bgeu r1, r4, .LBB2_27
.LBB2_26:
	add r26, r18, r0
	add r3, r11, r0
	bne r26, r18, .LBB2_30
	jal r0, .LBB2_12
.LBB2_27:
	add r3, r26, r3
	beq r12, r18, .LBB2_16
.LBB2_28:
	addi r26, r3, 1
	add r15, r1, r0
.LBB2_29:
	add r3, r11, r0
	beq r26, r18, .LBB2_12
.LBB2_30:
	add r4, r15, r0
	jal r31, lua_pushinteger
	addi r25, r25, 1
	bne r26, r18, .LBB2_13
	jal r0, .LBB2_32
.LBB2_31:
	add r13, r25, r0
.LBB2_32:
	add r1, r13, r0
	ldw lr, fp+-68
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
	addi sp, sp, 88
	jalr r0, r31, 0
.LBB2_33:
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_argerror
	ldw r1, r16+0
	ble r18, r1, .LBB2_8
.LBB2_34:
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, luaL_argerror
	bge r18, r15, .LBB2_9
	jal r0, .LBB2_32
.Lfunc_end2:
	.size	codepoint, .Lfunc_end2-codepoint
                                        # -- End function
	.p2align	2                               # -- Begin function utfchar
	.type	utfchar,@function
utfchar:                                # @utfchar
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
	addi r13, r0, 1
	bne r1, r13, .LBB3_3
.LBB3_1:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_checkinteger
	addi r3, r0, -1
	ble r1, r3, .LBB3_10
.LBB3_2:
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	add r3, r11, r0
	add r5, r1, r0
	jal r31, lua_pushfstring
	jal r0, .LBB3_9
.LBB3_3:
	add r12, r1, r0
	addi r4, fp, -564
	add r3, r11, r0
	jal r31, luaL_buffinit
	blt r12, r13, .LBB3_8
.LBB3_4:
	addi r13, r0, 0
	addi r17, r0, -1
	lui r14, %hi(.L.str.13)
	addi r14, r14, %lo(.L.str.13)
	addi r15, fp, -564
	lui r16, %hi(.L.str.12)
	addi r16, r16, %lo(.L.str.12)
.LBB3_5:
	addi r13, r13, 1
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_checkinteger
	ble r1, r17, .LBB3_7
.LBB3_6:
	add r3, r11, r0
	add r4, r14, r0
	add r5, r1, r0
	jal r31, lua_pushfstring
	add r3, r15, r0
	jal r31, luaL_addvalue
	bne r12, r13, .LBB3_5
	jal r0, .LBB3_8
.LBB3_7:
	add r3, r11, r0
	add r4, r13, r0
	add r5, r16, r0
	add r18, r1, r0
	jal r31, luaL_argerror
	add r1, r18, r0
	jal r0, .LBB3_6
.LBB3_8:
	addi r3, fp, -564
	jal r31, luaL_pushresult
.LBB3_9:
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
.LBB3_10:
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r4, r0, 1
	add r3, r11, r0
	add r12, r1, r0
	jal r31, luaL_argerror
	add r1, r12, r0
	jal r0, .LBB3_2
.Lfunc_end3:
	.size	utfchar, .Lfunc_end3-utfchar
                                        # -- End function
	.p2align	2                               # -- Begin function utflen
	.type	utflen,@function
utflen:                                 # @utflen
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
	stw fp+-64, r26
	stw fp+-68, r27
	stw fp+-72, r28
	stw fp+-76, lr
	add r12, r3, r0
	addi r11, r0, 1
	addi r17, fp, -80
	add r4, r11, r0
	add r5, r17, r0
	jal r31, luaL_checklstring
	add r13, r1, r0
	addi r14, r0, 2
	add r3, r12, r0
	add r4, r14, r0
	add r5, r11, r0
	jal r31, luaL_optinteger
	addi r15, r0, -1
	add r20, r1, r0
	bgt r1, r15, .LBB4_3
.LBB4_1:
	ldw r3, r17+0
	addi r20, r0, 0
	sub r4, r20, r1
	bltu r3, r4, .LBB4_3
.LBB4_2:
	add r1, r1, r3
	addi r20, r1, 1
.LBB4_3:
	addi r4, r0, 3
	add r3, r12, r0
	add r5, r15, r0
	jal r31, luaL_optinteger
	add r18, r1, r0
	bgt r1, r15, .LBB4_6
.LBB4_4:
	ldw r3, r17+0
	addi r18, r0, 0
	sub r4, r18, r1
	bltu r3, r4, .LBB4_6
.LBB4_5:
	add r1, r1, r3
	addi r18, r1, 1
.LBB4_6:
	addi r4, r0, 4
	add r3, r12, r0
	jal r31, lua_toboolean
	add r16, r1, r0
	addi r19, r0, 0
	sgt r3, r20, r19
	addi r1, r20, -1
	sub r21, r19, r3
	xor r22, r1, r20
	blt r20, r11, .LBB4_33
.LBB4_7:
	ldw r3, r17+0
	bgt r1, r3, .LBB4_33
.LBB4_8:
	and r21, r22, r21
	ldw r1, r17+0
	bgt r18, r1, .LBB4_32
.LBB4_9:
	xor r21, r20, r21
	addi r20, r13, 1
	lui r1, 512
	addi r22, r1, -2048
	lui r1, 272
	addi r23, r1, -1
	lui r1, 14
	addi r24, r1, -2048
	addi r25, r0, 5
	lui r26, %hi(utf8_decode.limits)
	addi r26, r26, %lo(utf8_decode.limits)
	addi r27, r0, 128
	add r17, r19, r0
	jal r0, .LBB4_11
.LBB4_10:
	sub r21, r28, r13
	addi r17, r17, 1
	beq r28, r19, .LBB4_30
.LBB4_11:
	bge r21, r18, .LBB4_29
.LBB4_12:
	add r1, r13, r21
	ldb r4, r1+0
	andi r3, r4, 255
	ble r4, r15, .LBB4_16
.LBB4_13:
	bne r16, r19, .LBB4_25
.LBB4_14:
	add r28, r19, r0
	bgtu r3, r23, .LBB4_26
.LBB4_15:
	and r3, r3, r22
	add r28, r19, r0
	bne r3, r24, .LBB4_25
	jal r0, .LBB4_26
.LBB4_16:
	andi r4, r3, 64
	bne r4, r19, .LBB4_18
.LBB4_17:
	add r7, r3, r0
	add r5, r19, r0
	add r4, r19, r0
	jal r0, .LBB4_21
.LBB4_18:
	add r6, r20, r21
	add r4, r19, r0
	add r5, r19, r0
.LBB4_19:
	add r7, r6, r4
	ldbu r7, r7+0
	andi r8, r7, 192
	bne r8, r27, .LBB4_28
.LBB4_20:
	slli r5, r5, 6
	andi r7, r7, 63
	or  r5, r7, r5
	slli r7, r3, 1
	andi r8, r3, 32
	addi r4, r4, 1
	add r3, r7, r0
	bne r8, r19, .LBB4_19
.LBB4_21:
	add r28, r19, r0
	bgtu r4, r25, .LBB4_26
.LBB4_22:
	andi r3, r7, 63
	slli r6, r4, 2
	add r7, r6, r4
	sll r3, r3, r7
	or  r3, r3, r5
	add r28, r19, r0
	blt r3, r19, .LBB4_26
.LBB4_23:
	add r5, r6, r26
	ldw r5, r5+0
	add r28, r19, r0
	bltu r3, r5, .LBB4_26
.LBB4_24:
	add r1, r1, r4
	beq r16, r19, .LBB4_14
.LBB4_25:
	addi r28, r1, 1
.LBB4_26:
	bne r28, r19, .LBB4_10
.LBB4_27:
	add r3, r12, r0
	jal r31, lua_pushnil
	addi r4, r21, 1
	add r3, r12, r0
	jal r31, lua_pushinteger
	bne r28, r19, .LBB4_11
	jal r0, .LBB4_30
.LBB4_28:
	add r28, r19, r0
	bne r28, r19, .LBB4_10
	jal r0, .LBB4_27
.LBB4_29:
	add r3, r12, r0
	add r4, r17, r0
	jal r31, lua_pushinteger
	jal r0, .LBB4_31
.LBB4_30:
	add r11, r14, r0
.LBB4_31:
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
	addi sp, sp, 88
	jalr r0, r31, 0
.LBB4_32:
	lui r5, %hi(.L.str.15)
	addi r5, r5, %lo(.L.str.15)
	addi r4, r0, 3
	add r3, r12, r0
	jal r31, luaL_argerror
	jal r0, .LBB4_9
.LBB4_33:
	lui r5, %hi(.L.str.14)
	addi r5, r5, %lo(.L.str.14)
	addi r4, r0, 2
	add r3, r12, r0
	jal r31, luaL_argerror
	jal r0, .LBB4_8
.Lfunc_end4:
	.size	utflen, .Lfunc_end4-utflen
                                        # -- End function
	.p2align	2                               # -- Begin function iter_codes
	.type	iter_codes,@function
iter_codes:                             # @iter_codes
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
	jal r31, lua_toboolean
	add r13, r1, r0
	addi r4, r0, 1
	addi r12, r0, 0
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_checklstring
	ldb r1, r1+0
	addi r3, r0, -65
	ble r1, r3, .LBB5_2
.LBB5_1:
	seq r1, r13, r12
	lui r3, %hi(iter_auxlax)
	addi r3, r3, %lo(iter_auxlax)
	lui r4, %hi(iter_auxstrict)
	addi r4, r4, %lo(iter_auxstrict)
	xor r4, r4, r3
	sub r1, r12, r1
	and r1, r4, r1
	xor r4, r1, r3
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_pushcclosure
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_pushvalue
	add r3, r11, r0
	add r4, r12, r0
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
.LBB5_2:
	lui r5, %hi(.L.str.11)
	addi r5, r5, %lo(.L.str.11)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB5_1
.Lfunc_end5:
	.size	iter_codes, .Lfunc_end5-iter_codes
                                        # -- End function
	.p2align	2                               # -- Begin function iter_auxlax
	.type	iter_auxlax,@function
iter_auxlax:                            # @iter_auxlax
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r4, r0, 0
	jal r31, iter_aux
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	iter_auxlax, .Lfunc_end6-iter_auxlax
                                        # -- End function
	.p2align	2                               # -- Begin function iter_auxstrict
	.type	iter_auxstrict,@function
iter_auxstrict:                         # @iter_auxstrict
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r4, r0, 1
	jal r31, iter_aux
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end7:
	.size	iter_auxstrict, .Lfunc_end7-iter_auxstrict
                                        # -- End function
	.p2align	2                               # -- Begin function iter_aux
	.type	iter_aux,@function
iter_aux:                               # @iter_aux
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
	add r13, r4, r0
	add r11, r3, r0
	addi r4, r0, 1
	addi r16, fp, -32
	add r5, r16, r0
	jal r31, luaL_checklstring
	add r15, r1, r0
	addi r12, r0, 2
	addi r14, r0, 0
	add r3, r11, r0
	add r4, r12, r0
	add r5, r14, r0
	jal r31, lua_tointegerx
	ldw r3, r16+0
	bgeu r1, r3, .LBB8_4
.LBB8_1:
	addi r4, r0, -64
.LBB8_2:
	add r5, r15, r1
	ldb r5, r5+0
	addi r1, r1, 1
	blt r5, r4, .LBB8_2
.LBB8_3:
	addi r1, r1, -1
.LBB8_4:
	bgeu r1, r3, .LBB8_24
.LBB8_5:
	add r3, r15, r1
	ldb r4, r3+0
	andi r14, r4, 255
	addi r5, r0, -1
	ble r4, r5, .LBB8_10
.LBB8_6:
	addi r4, r0, 0
	beq r13, r4, .LBB8_9
.LBB8_7:
	lui r5, 272
	addi r5, r5, -1
	bgtu r14, r5, .LBB8_19
.LBB8_8:
	lui r5, 512
	addi r5, r5, -2048
	and r5, r14, r5
	lui r6, 14
	addi r6, r6, -2048
	beq r5, r6, .LBB8_20
.LBB8_9:
	addi r4, r3, 1
	addi r3, r0, 0
	bne r4, r3, .LBB8_21
	jal r0, .LBB8_22
.LBB8_10:
	andi r5, r14, 64
	addi r4, r0, 0
	bne r5, r4, .LBB8_12
.LBB8_11:
	add r10, r14, r0
	add r6, r4, r0
	add r5, r4, r0
	jal r0, .LBB8_15
.LBB8_12:
	add r5, r1, r15
	addi r8, r5, 1
	addi r7, r0, 0
	addi r9, r0, 128
	add r5, r7, r0
	add r6, r7, r0
.LBB8_13:
	add r10, r8, r5
	ldbu r10, r10+0
	andi r15, r10, 192
	bne r15, r9, .LBB8_25
.LBB8_14:
	slli r6, r6, 6
	andi r10, r10, 63
	or  r6, r10, r6
	slli r10, r14, 1
	andi r15, r14, 32
	addi r5, r5, 1
	add r14, r10, r0
	bne r15, r7, .LBB8_13
.LBB8_15:
	addi r7, r0, 5
	bgtu r5, r7, .LBB8_19
.LBB8_16:
	andi r8, r10, 63
	slli r7, r5, 2
	add r9, r7, r5
	sll r8, r8, r9
	or  r14, r8, r6
	addi r6, r0, 0
	blt r14, r6, .LBB8_20
.LBB8_17:
	lui r4, %hi(utf8_decode.limits)
	addi r4, r4, %lo(utf8_decode.limits)
	add r4, r7, r4
	ldw r7, r4+0
	add r4, r6, r0
	bltu r14, r7, .LBB8_20
.LBB8_18:
	add r3, r3, r5
	addi r4, r0, 0
	bne r13, r4, .LBB8_7
	jal r0, .LBB8_9
.LBB8_19:
                                        # implicit-def: $r14
.LBB8_20:
	addi r3, r0, 0
	beq r4, r3, .LBB8_22
.LBB8_21:
	ldb r3, r4+0
	addi r4, r0, -65
	bgt r3, r4, .LBB8_23
.LBB8_22:
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	add r3, r11, r0
	jal r31, luaL_error
	add r14, r1, r0
	jal r0, .LBB8_24
.LBB8_23:
	addi r4, r1, 1
	add r3, r11, r0
	jal r31, lua_pushinteger
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushinteger
	add r14, r12, r0
.LBB8_24:
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
.LBB8_25:
                                        # implicit-def: $r14
	add r4, r7, r0
	addi r3, r0, 0
	bne r4, r3, .LBB8_21
	jal r0, .LBB8_22
.Lfunc_end8:
	.size	iter_aux, .Lfunc_end8-iter_aux
                                        # -- End function
	.type	funcs,@object                   # @funcs
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
funcs:
	.word	.L.str.2
	.word	byteoffset
	.word	.L.str.3
	.word	codepoint
	.word	.L.str.4
	.word	utfchar
	.word	.L.str.5
	.word	utflen
	.word	.L.str.6
	.word	iter_codes
	.word	.L.str.1
	.word	0
	.zero	8
	.size	funcs, 56

	.type	.L.str,@object                  # @.str
.L.str:
	.asciz	"[\000-\177\302-\375][\200-\277]*"
	.size	.L.str, 15

	.type	.L.str.1,@object                # @.str.1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.1:
	.asciz	"charpattern"
	.size	.L.str.1, 12

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"offset"
	.size	.L.str.2, 7

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"codepoint"
	.size	.L.str.3, 10

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"char"
	.size	.L.str.4, 5

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"len"
	.size	.L.str.5, 4

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"codes"
	.size	.L.str.6, 6

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"position out of bounds"
	.size	.L.str.7, 23

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"initial position is a continuation byte"
	.size	.L.str.8, 40

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"out of bounds"
	.size	.L.str.9, 14

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"string slice too long"
	.size	.L.str.10, 22

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"invalid UTF-8 code"
	.size	.L.str.11, 19

	.type	utf8_decode.limits,@object      # @utf8_decode.limits
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
utf8_decode.limits:
	.word	4294967295                      # 0xffffffff
	.word	128                             # 0x80
	.word	2048                            # 0x800
	.word	65536                           # 0x10000
	.word	2097152                         # 0x200000
	.word	67108864                        # 0x4000000
	.size	utf8_decode.limits, 24

	.type	.L.str.12,@object               # @.str.12
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.12:
	.asciz	"value out of range"
	.size	.L.str.12, 19

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"%U"
	.size	.L.str.13, 3

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"initial position out of bounds"
	.size	.L.str.14, 31

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"final position out of bounds"
	.size	.L.str.15, 29

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
