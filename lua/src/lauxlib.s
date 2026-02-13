	.file	"lauxlib.c"
	.text
	.globl	luaL_traceback                  # -- Begin function luaL_traceback
	.p2align	2
	.type	luaL_traceback,@function
luaL_traceback:                         # @luaL_traceback
# %bb.0:
	addi sp, sp, -760
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 760
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
	add r15, r6, r0
	add r16, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	addi r3, r0, 1
	addi r11, fp, -608
	addi r14, r0, 0
	add r18, r3, r0
.LBB0_1:
	add r20, r18, r0
	add r18, r3, r0
	add r3, r12, r0
	add r4, r18, r0
	add r5, r11, r0
	jal r31, lua_getstack
	slli r3, r18, 1
	bne r1, r14, .LBB0_1
.LBB0_2:
	bge r20, r18, .LBB0_5
.LBB0_3:
	addi r17, fp, -608
.LBB0_4:
	add r1, r18, r20
	srli r3, r1, 31
	add r1, r1, r3
	srai r19, r1, 1
	add r3, r12, r0
	add r4, r19, r0
	add r5, r17, r0
	jal r31, lua_getstack
	seq r1, r1, r14
	addi r3, r19, 1
	xor r4, r19, r18
	sub r1, r14, r1
	and r4, r4, r1
	xor r18, r18, r4
	xor r4, r20, r3
	and r1, r4, r1
	xor r20, r3, r1
	blt r20, r18, .LBB0_4
.LBB0_5:
	stw r11+12, r13
	addi r1, r11, 16
	stw fp+-736, r1
	stw r11+0, r1
	stw r11+8, r14
	addi r1, r0, 512
	stw r11+4, r1
	add r3, r13, r0
	add r4, r11, r0
	jal r31, lua_pushlightuserdata
	beq r16, r14, .LBB0_11
.LBB0_6:
	add r3, r16, r0
	jal r31, strlen
	beq r1, r14, .LBB0_8
.LBB0_7:
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r1, r0
	add r17, r1, r0
	jal r31, prepbuffsize
	add r3, r1, r0
	add r4, r16, r0
	add r5, r17, r0
	jal r31, memcpy
	ldw r1, r11+8
	add r1, r1, r17
	stw r11+8, r1
.LBB0_8:
	ldw r1, r11+8
	ldw r3, r11+4
	bltu r1, r3, .LBB0_10
.LBB0_9:
	addi r3, fp, -608
	addi r4, r0, 1
	addi r5, r0, -1
	jal r31, prepbuffsize
.LBB0_10:
	ldw r1, r11+0
	ldw r3, r11+8
	addi r4, r3, 1
	stw r11+8, r4
	add r1, r1, r3
	addi r3, r0, 10
	stb r1+0, r3
.LBB0_11:
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	jal r31, strlen
	beq r1, r14, .LBB0_13
.LBB0_12:
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r1, r0
	add r16, r1, r0
	jal r31, prepbuffsize
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	add r3, r1, r0
	add r5, r16, r0
	jal r31, memcpy
	ldw r1, r11+8
	add r1, r1, r16
	stw r11+8, r1
.LBB0_13:
	addi r16, fp, -716
	add r3, r12, r0
	add r4, r15, r0
	add r5, r16, r0
	jal r31, lua_getstack
	beq r1, r14, .LBB0_35
.LBB0_14:
	sub r1, r15, r18
	addi r3, r0, -22
	slt r1, r1, r3
	addi r1, r1, -1
	ori  r23, r1, 10
	addi r17, r16, 44
	addi r19, r18, -11
	addi r18, r0, -1
	lui r1, %hi(.L.str.1)
	addi r1, r1, %lo(.L.str.1)
	stw fp+-720, r1
	addi r20, fp, -80
	addi r21, r0, -2
	lui r22, %hi(.L.str.2)
	addi r22, r22, %lo(.L.str.2)
	lui r1, %hi(.L.str.3)
	addi r1, r1, %lo(.L.str.3)
	stw fp+-724, r1
	addi r1, r0, 67
	stw fp+-732, r1
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
	stw fp+-744, r1
	addi r1, r0, 109
	stw fp+-740, r1
	lui r1, %hi(.L.str.48)
	addi r1, r1, %lo(.L.str.48)
	stw fp+-748, r1
	lui r1, %hi(.L.str.49)
	addi r1, r1, %lo(.L.str.49)
	stw fp+-752, r1
	lui r1, %hi(.L.str.47)
	addi r1, r1, %lo(.L.str.47)
	stw fp+-728, r1
	lui r28, %hi(.L.str.5)
	addi r28, r28, %lo(.L.str.5)
	lui r24, %hi(.L.str.46)
	addi r24, r24, %lo(.L.str.46)
	lui r25, %hi(.L.str.4)
	addi r25, r25, %lo(.L.str.4)
	jal r0, .LBB0_17
.LBB0_15:
	xor r1, r15, r18
	add r5, r19, r1
	add r3, r13, r0
	ldw r4, fp+-720
	jal r31, lua_pushfstring
	ldw r15, r11+12
	add r3, r15, r0
	add r4, r18, r0
	add r5, r20, r0
	jal r31, lua_tolstring
	add r26, r1, r0
	ldw r4, r20+0
	add r3, r11, r0
	add r5, r21, r0
	jal r31, prepbuffsize
	ldw r5, r20+0
	add r3, r1, r0
	add r4, r26, r0
	jal r31, memcpy
	ldw r1, r20+0
	ldw r3, r11+8
	add r1, r3, r1
	stw r11+8, r1
	add r3, r15, r0
	add r4, r21, r0
	jal r31, lua_settop
	add r15, r19, r0
.LBB0_16:
	add r3, r12, r0
	add r4, r15, r0
	add r5, r16, r0
	jal r31, lua_getstack
	addi r23, r23, -1
	beq r1, r14, .LBB0_35
.LBB0_17:
	beq r23, r14, .LBB0_15
.LBB0_18:
	add r3, r12, r0
	add r4, r22, r0
	add r5, r16, r0
	jal r31, lua_getinfo
	ldw r6, r16+24
	ble r6, r14, .LBB0_20
.LBB0_19:
	add r3, r13, r0
	add r4, r25, r0
	jal r0, .LBB0_21
.LBB0_20:
	add r3, r13, r0
	ldw r4, fp+-724
.LBB0_21:
	add r5, r17, r0
	jal r31, lua_pushfstring
	ldw r26, r11+12
	add r3, r26, r0
	add r4, r18, r0
	add r5, r20, r0
	jal r31, lua_tolstring
	add r27, r1, r0
	ldw r4, r20+0
	add r3, r11, r0
	add r5, r21, r0
	jal r31, prepbuffsize
	ldw r5, r20+0
	add r3, r1, r0
	add r4, r27, r0
	jal r31, memcpy
	ldw r1, r20+0
	ldw r3, r11+8
	add r1, r3, r1
	stw r11+8, r1
	add r3, r26, r0
	add r4, r21, r0
	jal r31, lua_settop
	add r3, r13, r0
	add r4, r16, r0
	jal r31, pushglobalfuncname
	beq r1, r14, .LBB0_23
.LBB0_22:
	add r3, r13, r0
	add r4, r18, r0
	add r5, r14, r0
	jal r31, lua_tolstring
	add r3, r13, r0
	add r4, r24, r0
	add r5, r1, r0
	jal r31, lua_pushfstring
	add r3, r13, r0
	add r4, r21, r0
	add r5, r18, r0
	jal r31, lua_rotate
	add r3, r13, r0
	add r4, r21, r0
	jal r31, lua_settop
	jal r0, .LBB0_31
.LBB0_23:
	ldw r5, r16+8
	ldbu r1, r5+0
	beq r1, r14, .LBB0_26
.LBB0_24:
	ldw r6, r16+4
	add r3, r13, r0
	ldw r4, fp+-728
.LBB0_25:
	jal r31, lua_pushfstring
	jal r0, .LBB0_31
.LBB0_26:
	ldw r1, r16+12
	ldbu r1, r1+0
	ldw r3, fp+-732
	beq r1, r3, .LBB0_29
.LBB0_27:
	ldw r3, fp+-740
	bne r1, r3, .LBB0_34
.LBB0_28:
	add r3, r13, r0
	ldw r4, fp+-748
	jal r0, .LBB0_30
.LBB0_29:
	add r3, r13, r0
	ldw r4, fp+-744
.LBB0_30:
	jal r31, lua_pushstring
.LBB0_31:
	addi r15, r15, 1
	ldw r26, r11+12
	add r3, r26, r0
	add r4, r18, r0
	add r5, r20, r0
	jal r31, lua_tolstring
	add r27, r1, r0
	ldw r4, r20+0
	add r3, r11, r0
	add r5, r21, r0
	jal r31, prepbuffsize
	ldw r5, r20+0
	add r3, r1, r0
	add r4, r27, r0
	jal r31, memcpy
	ldw r1, r20+0
	ldw r3, r11+8
	add r1, r3, r1
	stw r11+8, r1
	add r3, r26, r0
	add r4, r21, r0
	jal r31, lua_settop
	ldbu r1, r16+39
	beq r1, r14, .LBB0_16
.LBB0_32:
	add r3, r28, r0
	jal r31, strlen
	beq r1, r14, .LBB0_16
.LBB0_33:
	add r26, r1, r0
	add r3, r11, r0
	add r4, r1, r0
	add r5, r18, r0
	jal r31, prepbuffsize
	add r3, r1, r0
	add r4, r28, r0
	add r5, r26, r0
	jal r31, memcpy
	ldw r1, r11+8
	add r1, r1, r26
	stw r11+8, r1
	jal r0, .LBB0_16
.LBB0_34:
	ldw r6, r16+28
	add r3, r13, r0
	ldw r4, fp+-752
	add r5, r17, r0
	jal r0, .LBB0_25
.LBB0_35:
	ldw r12, r11+12
	ldw r4, r11+0
	ldw r5, r11+8
	add r3, r12, r0
	jal r31, lua_pushlstring
	ldw r1, r11+0
	ldw r3, fp+-736
	beq r1, r3, .LBB0_37
.LBB0_36:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_closeslot
.LBB0_37:
	addi r11, r0, -2
	addi r5, r0, -1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_rotate
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_settop
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
	addi sp, sp, 760
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaL_traceback, .Lfunc_end0-luaL_traceback
                                        # -- End function
	.globl	luaL_buffinit                   # -- Begin function luaL_buffinit
	.p2align	2
	.type	luaL_buffinit,@function
luaL_buffinit:                          # @luaL_buffinit
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	stw r4+12, r3
	addi r1, r4, 16
	stw r4+0, r1
	addi r1, r0, 0
	stw r4+8, r1
	addi r1, r0, 512
	stw r4+4, r1
	jal r31, lua_pushlightuserdata
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	luaL_buffinit, .Lfunc_end1-luaL_buffinit
                                        # -- End function
	.globl	luaL_addstring                  # -- Begin function luaL_addstring
	.p2align	2
	.type	luaL_addstring,@function
luaL_addstring:                         # @luaL_addstring
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
	add r3, r4, r0
	jal r31, strlen
	addi r3, r0, 0
	beq r1, r3, .LBB2_2
.LBB2_1:
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r1, r0
	add r13, r1, r0
	jal r31, prepbuffsize
	add r3, r1, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, memcpy
	ldw r1, r11+8
	add r1, r1, r13
	stw r11+8, r1
.LBB2_2:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	luaL_addstring, .Lfunc_end2-luaL_addstring
                                        # -- End function
	.globl	luaL_prepbuffsize               # -- Begin function luaL_prepbuffsize
	.p2align	2
	.type	luaL_prepbuffsize,@function
luaL_prepbuffsize:                      # @luaL_prepbuffsize
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r5, r0, -1
	jal r31, prepbuffsize
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	luaL_prepbuffsize, .Lfunc_end3-luaL_prepbuffsize
                                        # -- End function
	.globl	luaL_addvalue                   # -- Begin function luaL_addvalue
	.p2align	2
	.type	luaL_addvalue,@function
luaL_addvalue:                          # @luaL_addvalue
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
	ldw r12, r3+12
	addi r4, r0, -1
	addi r13, fp, -28
	add r3, r12, r0
	add r5, r13, r0
	jal r31, lua_tolstring
	add r14, r1, r0
	ldw r4, r13+0
	addi r15, r0, -2
	add r3, r11, r0
	add r5, r15, r0
	jal r31, prepbuffsize
	ldw r5, r13+0
	add r3, r1, r0
	add r4, r14, r0
	jal r31, memcpy
	ldw r1, r13+0
	ldw r3, r11+8
	add r1, r3, r1
	stw r11+8, r1
	add r3, r12, r0
	add r4, r15, r0
	jal r31, lua_settop
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
.Lfunc_end4:
	.size	luaL_addvalue, .Lfunc_end4-luaL_addvalue
                                        # -- End function
	.globl	luaL_pushresult                 # -- Begin function luaL_pushresult
	.p2align	2
	.type	luaL_pushresult,@function
luaL_pushresult:                        # @luaL_pushresult
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r12, r3, r0
	ldw r11, r3+12
	ldw r4, r3+0
	ldw r5, r3+8
	add r3, r11, r0
	jal r31, lua_pushlstring
	ldw r1, r12+0
	addi r3, r12, 16
	beq r1, r3, .LBB5_2
.LBB5_1:
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_closeslot
.LBB5_2:
	addi r12, r0, -2
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rotate
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_settop
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	luaL_pushresult, .Lfunc_end5-luaL_pushresult
                                        # -- End function
	.globl	luaL_argerror                   # -- Begin function luaL_argerror
	.p2align	2
	.type	luaL_argerror,@function
luaL_argerror:                          # @luaL_argerror
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
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	addi r15, r0, 0
	addi r14, fp, -136
	add r4, r15, r0
	add r5, r14, r0
	jal r31, lua_getstack
	beq r1, r15, .LBB6_6
.LBB6_1:
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	add r3, r13, r0
	add r5, r14, r0
	jal r31, lua_getinfo
	ldw r3, r14+8
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	jal r31, strcmp
	bne r1, r15, .LBB6_3
.LBB6_2:
	addi r12, r12, -1
	beq r12, r15, .LBB6_8
.LBB6_3:
	ldw r1, r14+4
	bne r1, r15, .LBB6_11
.LBB6_4:
	addi r16, fp, -136
	add r3, r13, r0
	add r4, r16, r0
	jal r31, pushglobalfuncname
	beq r1, r15, .LBB6_9
.LBB6_5:
	addi r4, r0, -1
	addi r5, r0, 0
	add r3, r13, r0
	jal r31, lua_tolstring
	jal r0, .LBB6_10
.LBB6_6:
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	add r3, r13, r0
	add r5, r12, r0
.LBB6_7:
	add r6, r11, r0
	jal r0, .LBB6_12
.LBB6_8:
	ldw r5, r14+4
	lui r4, %hi(.L.str.9)
	addi r4, r4, %lo(.L.str.9)
	add r3, r13, r0
	jal r0, .LBB6_7
.LBB6_9:
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
.LBB6_10:
	stw r16+4, r1
.LBB6_11:
	ldw r6, r14+4
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	add r3, r13, r0
	add r5, r12, r0
	add r7, r11, r0
.LBB6_12:
	jal r31, luaL_error
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
.Lfunc_end6:
	.size	luaL_argerror, .Lfunc_end6-luaL_argerror
                                        # -- End function
	.globl	luaL_error                      # -- Begin function luaL_error
	.p2align	2
	.type	luaL_error,@function
luaL_error:                             # @luaL_error
# %bb.0:
	addi sp, sp, -168
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 168
	stw fp+-28, r11
	stw fp+-32, r12
	stw fp+-36, r13
	stw fp+-40, r14
	stw fp+-44, r15
	stw fp+-48, lr
	add r12, r4, r0
	add r11, r3, r0
	addi r1, fp, -24
	stw r1+20, r10
	stw r1+16, r9
	stw r1+12, r8
	stw r1+8, r7
	addi r3, r1, 4
	stw r3+0, r6
	stw r1+0, r5
	addi r15, fp, -160
	stw r15+0, r1
	addi r14, r0, 1
	addi r13, fp, -156
	add r3, r11, r0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, lua_getstack
	addi r3, r0, 0
	beq r1, r3, .LBB7_3
.LBB7_1:
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	add r3, r11, r0
	add r5, r13, r0
	jal r31, lua_getinfo
	ldw r6, r13+24
	blt r6, r14, .LBB7_3
.LBB7_2:
	addi r5, r13, 44
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	jal r0, .LBB7_4
.LBB7_3:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
.LBB7_4:
	add r3, r11, r0
	jal r31, lua_pushfstring
	ldw r5, r15+0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushvfstring
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_concat
	add r3, r11, r0
	jal r31, lua_error
	ldw lr, fp+-48
	ldw r15, fp+-44
	ldw r14, fp+-40
	ldw r13, fp+-36
	ldw r12, fp+-32
	ldw r11, fp+-28
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 168
	jalr r0, r31, 0
.Lfunc_end7:
	.size	luaL_error, .Lfunc_end7-luaL_error
                                        # -- End function
	.p2align	2                               # -- Begin function pushglobalfuncname
	.type	pushglobalfuncname,@function
pushglobalfuncname:                     # @pushglobalfuncname
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
	jal r31, lua_gettop
	add r13, r1, r0
	lui r4, %hi(.L.str.50)
	addi r4, r4, %lo(.L.str.50)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_getinfo
	lui r5, %hi(.L.str.43)
	addi r5, r5, %lo(.L.str.43)
	lui r1, 1048572
	addi r4, r1, 384
	add r3, r11, r0
	jal r31, lua_getfield
	addi r4, r0, 6
	add r3, r11, r0
	jal r31, lua_checkstack
	addi r14, r0, 0
	beq r1, r14, .LBB8_7
.LBB8_1:
	addi r12, r13, 1
	addi r5, r0, 2
	add r3, r11, r0
	add r4, r12, r0
	jal r31, findfield
	beq r1, r14, .LBB8_5
.LBB8_2:
	addi r4, r0, -1
	addi r14, r0, 0
	add r3, r11, r0
	add r5, r14, r0
	jal r31, lua_tolstring
	add r13, r1, r0
	lui r4, %hi(.L.str.52)
	addi r4, r4, %lo(.L.str.52)
	addi r5, r0, 3
	add r3, r1, r0
	jal r31, strncmp
	bne r1, r14, .LBB8_4
.LBB8_3:
	addi r4, r13, 3
	add r3, r11, r0
	jal r31, lua_pushstring
	addi r13, r0, -2
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_rotate
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_settop
.LBB8_4:
	addi r4, r0, -1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_copy
	addi r14, r0, 1
	jal r0, .LBB8_6
.LBB8_5:
	add r12, r13, r0
.LBB8_6:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_settop
	add r1, r14, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB8_7:
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	lui r5, %hi(.L.str.51)
	addi r5, r5, %lo(.L.str.51)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB8_1
.Lfunc_end8:
	.size	pushglobalfuncname, .Lfunc_end8-pushglobalfuncname
                                        # -- End function
	.globl	luaL_typeerror                  # -- Begin function luaL_typeerror
	.p2align	2
	.type	luaL_typeerror,@function
luaL_typeerror:                         # @luaL_typeerror
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
	add r11, r4, r0
	add r12, r3, r0
	jal r31, lua_getmetatable
	addi r15, r0, 0
	beq r1, r15, .LBB9_5
.LBB9_1:
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
	add r3, r12, r0
	jal r31, lua_pushstring
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_rawget
	beq r1, r15, .LBB9_4
.LBB9_2:
	add r14, r1, r0
	addi r15, r0, -2
	addi r5, r0, -1
	add r3, r12, r0
	add r4, r15, r0
	jal r31, lua_rotate
	add r3, r12, r0
	add r4, r15, r0
	jal r31, lua_settop
	addi r1, r0, 4
	bne r14, r1, .LBB9_5
.LBB9_3:
	addi r4, r0, -1
	addi r5, r0, 0
	add r3, r12, r0
	jal r31, lua_tolstring
	jal r0, .LBB9_8
.LBB9_4:
	addi r4, r0, -3
	add r3, r12, r0
	jal r31, lua_settop
.LBB9_5:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_type
	addi r3, r0, 2
	bne r1, r3, .LBB9_7
.LBB9_6:
	lui r6, %hi(.L.str.13)
	addi r6, r6, %lo(.L.str.13)
	jal r0, .LBB9_9
.LBB9_7:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_type
	add r3, r12, r0
	add r4, r1, r0
	jal r31, lua_typename
.LBB9_8:
	add r6, r1, r0
.LBB9_9:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	add r3, r12, r0
	add r5, r13, r0
	jal r31, lua_pushfstring
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, luaL_argerror
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
.Lfunc_end9:
	.size	luaL_typeerror, .Lfunc_end9-luaL_typeerror
                                        # -- End function
	.globl	luaL_getmetafield               # -- Begin function luaL_getmetafield
	.p2align	2
	.type	luaL_getmetafield,@function
luaL_getmetafield:                      # @luaL_getmetafield
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
	add r11, r3, r0
	jal r31, lua_getmetatable
	addi r13, r0, 0
	beq r1, r13, .LBB10_5
.LBB10_1:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushstring
	addi r12, r0, -2
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rawget
	addi r13, r0, 0
	beq r1, r13, .LBB10_3
.LBB10_2:
	add r14, r1, r0
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rotate
	add r13, r14, r0
	jal r0, .LBB10_4
.LBB10_3:
	addi r12, r0, -3
.LBB10_4:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_settop
.LBB10_5:
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
.Lfunc_end10:
	.size	luaL_getmetafield, .Lfunc_end10-luaL_getmetafield
                                        # -- End function
	.globl	luaL_where                      # -- Begin function luaL_where
	.p2align	2
	.type	luaL_where,@function
luaL_where:                             # @luaL_where
# %bb.0:
	addi sp, sp, -136
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 136
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	addi r12, fp, -120
	add r5, r12, r0
	jal r31, lua_getstack
	addi r3, r0, 0
	beq r1, r3, .LBB11_3
.LBB11_1:
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_getinfo
	ldw r6, r12+24
	addi r1, r0, 1
	blt r6, r1, .LBB11_3
.LBB11_2:
	addi r5, r12, 44
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	jal r0, .LBB11_4
.LBB11_3:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
.LBB11_4:
	add r3, r11, r0
	jal r31, lua_pushfstring
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 136
	jalr r0, r31, 0
.Lfunc_end11:
	.size	luaL_where, .Lfunc_end11-luaL_where
                                        # -- End function
	.globl	luaL_fileresult                 # -- Begin function luaL_fileresult
	.p2align	2
	.type	luaL_fileresult,@function
luaL_fileresult:                        # @luaL_fileresult
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
	addi r13, r0, 0
	beq r4, r13, .LBB12_2
.LBB12_1:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushboolean
	jal r0, .LBB12_8
.LBB12_2:
	add r14, r5, r0
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	ldw r12, r1+0
	add r3, r11, r0
	jal r31, lua_pushnil
	beq r12, r13, .LBB12_5
.LBB12_3:
	add r3, r12, r0
	jal r31, strerror
	beq r14, r13, .LBB12_6
.LBB12_4:
	add r5, r14, r0
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r3, r11, r0
	add r6, r1, r0
	jal r31, lua_pushfstring
	jal r0, .LBB12_7
.LBB12_5:
	lui r1, %hi(.L.str.18)
	addi r1, r1, %lo(.L.str.18)
	bne r14, r13, .LBB12_4
.LBB12_6:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushstring
.LBB12_7:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushinteger
	addi r12, r0, 3
.LBB12_8:
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
.Lfunc_end12:
	.size	luaL_fileresult, .Lfunc_end12-luaL_fileresult
                                        # -- End function
	.globl	luaL_execresult                 # -- Begin function luaL_execresult
	.p2align	2
	.type	luaL_execresult,@function
luaL_execresult:                        # @luaL_execresult
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
	add r13, r4, r0
	add r11, r3, r0
	addi r1, r0, 0
	beq r4, r1, .LBB13_3
.LBB13_1:
	lui r3, %hi(errno)
	addi r3, r3, %lo(errno)
	ldw r12, r3+0
	beq r12, r1, .LBB13_3
.LBB13_2:
	add r3, r11, r0
	jal r31, lua_pushnil
	add r3, r12, r0
	jal r31, strerror
	add r4, r1, r0
	jal r0, .LBB13_7
.LBB13_3:
	beq r13, r1, .LBB13_5
.LBB13_4:
	add r3, r11, r0
	jal r31, lua_pushnil
	jal r0, .LBB13_6
.LBB13_5:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_pushboolean
.LBB13_6:
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	add r12, r13, r0
.LBB13_7:
	add r3, r11, r0
	jal r31, lua_pushstring
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
.Lfunc_end13:
	.size	luaL_execresult, .Lfunc_end13-luaL_execresult
                                        # -- End function
	.globl	luaL_newmetatable               # -- Begin function luaL_newmetatable
	.p2align	2
	.type	luaL_newmetatable,@function
luaL_newmetatable:                      # @luaL_newmetatable
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
	add r11, r4, r0
	add r12, r3, r0
	lui r1, 1048572
	addi r13, r1, 384
	add r4, r13, r0
	add r5, r11, r0
	jal r31, lua_getfield
	add r3, r1, r0
	addi r1, r0, 0
	bne r3, r1, .LBB14_2
.LBB14_1:
	addi r14, r0, -2
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_settop
	addi r4, r0, 0
	addi r5, r0, 2
	add r3, r12, r0
	jal r31, lua_createtable
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_pushstring
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_setfield
	addi r4, r0, -1
	add r3, r12, r0
	jal r31, lua_pushvalue
	add r3, r12, r0
	add r4, r13, r0
	add r5, r11, r0
	jal r31, lua_setfield
	addi r1, r0, 1
.LBB14_2:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end14:
	.size	luaL_newmetatable, .Lfunc_end14-luaL_newmetatable
                                        # -- End function
	.globl	luaL_setmetatable               # -- Begin function luaL_setmetatable
	.p2align	2
	.type	luaL_setmetatable,@function
luaL_setmetatable:                      # @luaL_setmetatable
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r5, r4, r0
	add r11, r3, r0
	lui r1, 1048572
	addi r4, r1, 384
	jal r31, lua_getfield
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_setmetatable
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end15:
	.size	luaL_setmetatable, .Lfunc_end15-luaL_setmetatable
                                        # -- End function
	.globl	luaL_testudata                  # -- Begin function luaL_testudata
	.p2align	2
	.type	luaL_testudata,@function
luaL_testudata:                         # @luaL_testudata
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
	add r15, r4, r0
	add r11, r3, r0
	jal r31, lua_touserdata
	addi r14, r0, 0
	beq r1, r14, .LBB16_3
.LBB16_1:
	add r12, r1, r0
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_getmetatable
	addi r14, r0, 0
	beq r1, r14, .LBB16_3
.LBB16_2:
	lui r1, 1048572
	addi r4, r1, 384
	add r3, r11, r0
	add r5, r13, r0
	jal r31, lua_getfield
	addi r4, r0, -1
	addi r5, r0, -2
	add r3, r11, r0
	jal r31, lua_rawequal
	addi r3, r0, 0
	seq r1, r1, r3
	sub r1, r3, r1
	and r1, r12, r1
	xor r14, r12, r1
	addi r4, r0, -3
	add r3, r11, r0
	jal r31, lua_settop
.LBB16_3:
	add r1, r14, r0
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
.Lfunc_end16:
	.size	luaL_testudata, .Lfunc_end16-luaL_testudata
                                        # -- End function
	.globl	luaL_checkudata                 # -- Begin function luaL_checkudata
	.p2align	2
	.type	luaL_checkudata,@function
luaL_checkudata:                        # @luaL_checkudata
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
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	jal r31, lua_touserdata
	addi r16, r0, 0
	add r15, r16, r0
	beq r1, r16, .LBB17_3
.LBB17_1:
	add r14, r1, r0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, lua_getmetatable
	addi r15, r0, 0
	beq r1, r15, .LBB17_3
.LBB17_2:
	lui r1, 1048572
	addi r4, r1, 384
	add r3, r13, r0
	add r5, r11, r0
	jal r31, lua_getfield
	addi r4, r0, -1
	addi r5, r0, -2
	add r3, r13, r0
	jal r31, lua_rawequal
	addi r3, r0, 0
	seq r1, r1, r3
	sub r1, r3, r1
	and r1, r14, r1
	xor r15, r14, r1
	addi r4, r0, -3
	add r3, r13, r0
	jal r31, lua_settop
.LBB17_3:
	beq r15, r16, .LBB17_5
.LBB17_4:
	add r1, r15, r0
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
.LBB17_5:
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, luaL_typeerror
	jal r0, .LBB17_4
.Lfunc_end17:
	.size	luaL_checkudata, .Lfunc_end17-luaL_checkudata
                                        # -- End function
	.globl	luaL_checkoption                # -- Begin function luaL_checkoption
	.p2align	2
	.type	luaL_checkoption,@function
luaL_checkoption:                       # @luaL_checkoption
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
	add r14, r6, r0
	add r11, r4, r0
	add r12, r3, r0
	addi r13, r0, 0
	beq r5, r13, .LBB18_4
.LBB18_1:
	add r15, r5, r0
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_type
	addi r3, r0, 1
	blt r1, r3, .LBB18_6
.LBB18_2:
	add r3, r12, r0
	add r4, r11, r0
	add r5, r13, r0
	jal r31, lua_tolstring
	beq r1, r13, .LBB18_12
.LBB18_3:
	add r15, r1, r0
	ldw r3, r14+0
	bne r3, r13, .LBB18_7
	jal r0, .LBB18_10
.LBB18_4:
	addi r16, r0, 0
	add r3, r12, r0
	add r4, r11, r0
	add r5, r16, r0
	jal r31, lua_tolstring
	beq r1, r16, .LBB18_12
.LBB18_5:
	add r15, r1, r0
.LBB18_6:
	ldw r3, r14+0
	beq r3, r13, .LBB18_10
.LBB18_7:
	addi r16, r14, 4
	add r14, r13, r0
.LBB18_8:
	add r4, r15, r0
	jal r31, strcmp
	beq r1, r13, .LBB18_11
.LBB18_9:
	addi r14, r14, 1
	ldw r3, r16+0
	addi r16, r16, 4
	bne r3, r13, .LBB18_8
.LBB18_10:
	lui r4, %hi(.L.str.21)
	addi r4, r4, %lo(.L.str.21)
	add r3, r12, r0
	add r5, r15, r0
	jal r31, lua_pushfstring
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, luaL_argerror
	add r14, r1, r0
.LBB18_11:
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
.LBB18_12:
	addi r4, r0, 4
	add r3, r12, r0
	jal r31, lua_typename
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, luaL_typeerror
	addi r15, r0, 0
	ldw r3, r14+0
	bne r3, r13, .LBB18_7
	jal r0, .LBB18_10
.Lfunc_end18:
	.size	luaL_checkoption, .Lfunc_end18-luaL_checkoption
                                        # -- End function
	.globl	luaL_optlstring                 # -- Begin function luaL_optlstring
	.p2align	2
	.type	luaL_optlstring,@function
luaL_optlstring:                        # @luaL_optlstring
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
	add r12, r6, r0
	add r11, r5, r0
	add r13, r4, r0
	add r14, r3, r0
	jal r31, lua_type
	addi r15, r0, 0
	ble r1, r15, .LBB19_3
.LBB19_1:
	add r3, r14, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	add r11, r1, r0
	bne r1, r15, .LBB19_7
.LBB19_2:
	addi r4, r0, 4
	add r3, r14, r0
	jal r31, lua_typename
	add r3, r14, r0
	add r4, r13, r0
	add r5, r1, r0
	jal r31, luaL_typeerror
	add r11, r15, r0
	jal r0, .LBB19_7
.LBB19_3:
	addi r1, r0, 0
	beq r12, r1, .LBB19_7
.LBB19_4:
	beq r11, r1, .LBB19_6
.LBB19_5:
	add r3, r11, r0
	jal r31, strlen
.LBB19_6:
	stw r12+0, r1
.LBB19_7:
	add r1, r11, r0
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
.Lfunc_end19:
	.size	luaL_optlstring, .Lfunc_end19-luaL_optlstring
                                        # -- End function
	.globl	luaL_checklstring               # -- Begin function luaL_checklstring
	.p2align	2
	.type	luaL_checklstring,@function
luaL_checklstring:                      # @luaL_checklstring
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
	add r11, r4, r0
	add r12, r3, r0
	jal r31, lua_tolstring
	addi r3, r0, 0
	beq r1, r3, .LBB20_2
.LBB20_1:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB20_2:
	addi r4, r0, 4
	add r3, r12, r0
	add r13, r1, r0
	jal r31, lua_typename
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, luaL_typeerror
	add r1, r13, r0
	jal r0, .LBB20_1
.Lfunc_end20:
	.size	luaL_checklstring, .Lfunc_end20-luaL_checklstring
                                        # -- End function
	.globl	luaL_checkstack                 # -- Begin function luaL_checkstack
	.p2align	2
	.type	luaL_checkstack,@function
luaL_checkstack:                        # @luaL_checkstack
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r5, r0
	add r12, r3, r0
	jal r31, lua_checkstack
	addi r3, r0, 0
	beq r1, r3, .LBB21_2
.LBB21_1:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB21_2:
	beq r11, r3, .LBB21_4
.LBB21_3:
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	add r3, r12, r0
	add r5, r11, r0
	jal r31, luaL_error
	jal r0, .LBB21_1
.LBB21_4:
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	add r3, r12, r0
	jal r31, luaL_error
	jal r0, .LBB21_1
.Lfunc_end21:
	.size	luaL_checkstack, .Lfunc_end21-luaL_checkstack
                                        # -- End function
	.globl	luaL_checktype                  # -- Begin function luaL_checktype
	.p2align	2
	.type	luaL_checktype,@function
luaL_checktype:                         # @luaL_checktype
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
	add r13, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	jal r31, lua_type
	bne r1, r13, .LBB22_2
.LBB22_1:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB22_2:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_typename
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, luaL_typeerror
	jal r0, .LBB22_1
.Lfunc_end22:
	.size	luaL_checktype, .Lfunc_end22-luaL_checktype
                                        # -- End function
	.globl	luaL_checkany                   # -- Begin function luaL_checkany
	.p2align	2
	.type	luaL_checkany,@function
luaL_checkany:                          # @luaL_checkany
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
	jal r31, lua_type
	addi r3, r0, -1
	beq r1, r3, .LBB23_2
.LBB23_1:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB23_2:
	lui r5, %hi(.L.str.24)
	addi r5, r5, %lo(.L.str.24)
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB23_1
.Lfunc_end23:
	.size	luaL_checkany, .Lfunc_end23-luaL_checkany
                                        # -- End function
	.globl	luaL_checknumber                # -- Begin function luaL_checknumber
	.p2align	2
	.type	luaL_checknumber,@function
luaL_checknumber:                       # @luaL_checknumber
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
	add r11, r4, r0
	add r12, r3, r0
	addi r13, fp, -24
	add r5, r13, r0
	jal r31, lua_tonumberx
	add r14, r2, r0
	ldw r3, r13+0
	addi r4, r0, 0
	beq r3, r4, .LBB24_2
.LBB24_1:
	add r2, r14, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB24_2:
	addi r4, r0, 3
	add r3, r12, r0
	add r13, r1, r0
	jal r31, lua_typename
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, luaL_typeerror
	add r1, r13, r0
	jal r0, .LBB24_1
.Lfunc_end24:
	.size	luaL_checknumber, .Lfunc_end24-luaL_checknumber
                                        # -- End function
	.globl	luaL_optnumber                  # -- Begin function luaL_optnumber
	.p2align	2
	.type	luaL_optnumber,@function
luaL_optnumber:                         # @luaL_optnumber
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
	add r15, r6, r0
	add r14, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	jal r31, lua_type
	addi r3, r0, 1
	blt r1, r3, .LBB25_2
.LBB25_1:
	addi r13, fp, -28
	add r3, r12, r0
	add r4, r11, r0
	add r5, r13, r0
	jal r31, lua_tonumberx
	add r14, r1, r0
	add r15, r2, r0
	ldw r1, r13+0
	addi r3, r0, 0
	beq r1, r3, .LBB25_3
.LBB25_2:
	add r1, r14, r0
	add r2, r15, r0
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
.LBB25_3:
	addi r4, r0, 3
	add r3, r12, r0
	jal r31, lua_typename
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, luaL_typeerror
	jal r0, .LBB25_2
.Lfunc_end25:
	.size	luaL_optnumber, .Lfunc_end25-luaL_optnumber
                                        # -- End function
	.globl	luaL_checkinteger               # -- Begin function luaL_checkinteger
	.p2align	2
	.type	luaL_checkinteger,@function
luaL_checkinteger:                      # @luaL_checkinteger
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
	add r11, r4, r0
	add r12, r3, r0
	addi r13, fp, -20
	add r5, r13, r0
	jal r31, lua_tointegerx
	ldw r3, r13+0
	addi r4, r0, 0
	beq r3, r4, .LBB26_2
.LBB26_1:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB26_2:
	add r3, r12, r0
	add r4, r11, r0
	add r11, r1, r0
	jal r31, interror
	add r1, r11, r0
	jal r0, .LBB26_1
.Lfunc_end26:
	.size	luaL_checkinteger, .Lfunc_end26-luaL_checkinteger
                                        # -- End function
	.p2align	2                               # -- Begin function interror
	.type	interror,@function
interror:                               # @interror
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
	jal r31, lua_isnumber
	addi r3, r0, 0
	beq r1, r3, .LBB27_2
.LBB27_1:
	lui r5, %hi(.L.str.54)
	addi r5, r5, %lo(.L.str.54)
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB27_3
.LBB27_2:
	addi r4, r0, 3
	add r3, r12, r0
	jal r31, lua_typename
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, luaL_typeerror
.LBB27_3:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end27:
	.size	interror, .Lfunc_end27-interror
                                        # -- End function
	.globl	luaL_optinteger                 # -- Begin function luaL_optinteger
	.p2align	2
	.type	luaL_optinteger,@function
luaL_optinteger:                        # @luaL_optinteger
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
	add r12, r4, r0
	add r13, r3, r0
	jal r31, lua_type
	addi r3, r0, 1
	blt r1, r3, .LBB28_2
.LBB28_1:
	addi r14, fp, -24
	add r3, r13, r0
	add r4, r12, r0
	add r5, r14, r0
	jal r31, lua_tointegerx
	add r11, r1, r0
	ldw r1, r14+0
	addi r3, r0, 0
	beq r1, r3, .LBB28_3
.LBB28_2:
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
.LBB28_3:
	add r3, r13, r0
	add r4, r12, r0
	jal r31, interror
	jal r0, .LBB28_2
.Lfunc_end28:
	.size	luaL_optinteger, .Lfunc_end28-luaL_optinteger
                                        # -- End function
	.p2align	2                               # -- Begin function prepbuffsize
	.type	prepbuffsize,@function
prepbuffsize:                           # @prepbuffsize
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
	add r14, r5, r0
	add r11, r3, r0
	ldw r3, r3+4
	ldw r1, r11+8
	sub r5, r3, r1
	bgeu r5, r4, .LBB29_7
.LBB29_1:
	ldw r13, r11+12
	addi r5, r0, -1
	xor r5, r4, r5
	bgtu r1, r5, .LBB29_15
.LBB29_2:
	srli r5, r3, 1
	addi r6, r0, -2
	and r3, r3, r6
	add r3, r3, r5
	add r1, r1, r4
	xor r4, r3, r1
	sgtu r3, r3, r1
	addi r5, r0, 0
	sub r3, r5, r3
	and r3, r4, r3
	xor r12, r1, r3
.LBB29_3:
	ldw r1, r11+0
	addi r3, r11, 16
	beq r1, r3, .LBB29_8
.LBB29_4:
	addi r16, fp, -44
	add r3, r13, r0
	add r4, r16, r0
	jal r31, lua_getallocf
	add r17, r1, r0
	add r3, r13, r0
	add r4, r14, r0
	jal r31, lua_touserdata
	add r15, r1, r0
	ldw r3, r16+0
	ldw r4, r1+0
	ldw r5, r1+4
	add r6, r12, r0
	jalr lr, r17, 0
	add r14, r1, r0
	addi r1, r0, 0
	beq r12, r1, .LBB29_6
.LBB29_5:
	beq r14, r1, .LBB29_16
.LBB29_6:
	stw r15+0, r14
	stw r15+4, r12
	jal r0, .LBB29_13
.LBB29_7:
	ldw r3, r11+0
	add r1, r3, r1
	jal r0, .LBB29_14
.LBB29_8:
	addi r5, r0, -1
	add r3, r13, r0
	add r4, r14, r0
	jal r31, lua_rotate
	addi r4, r0, -2
	add r3, r13, r0
	jal r31, lua_settop
	addi r4, r0, 8
	addi r15, r0, 0
	add r3, r13, r0
	add r5, r15, r0
	jal r31, lua_newuserdatauv
	stw r1+0, r15
	stw r1+4, r15
	lui r4, %hi(.L.str.57)
	addi r4, r4, %lo(.L.str.57)
	add r3, r13, r0
	jal r31, luaL_newmetatable
	beq r1, r15, .LBB29_10
.LBB29_9:
	lui r4, %hi(boxmt)
	addi r4, r4, %lo(boxmt)
	addi r5, r0, 0
	add r3, r13, r0
	jal r31, luaL_setfuncs
.LBB29_10:
	addi r4, r0, -2
	add r3, r13, r0
	jal r31, lua_setmetatable
	addi r5, r0, 1
	add r3, r13, r0
	add r4, r14, r0
	jal r31, lua_rotate
	add r3, r13, r0
	add r4, r14, r0
	jal r31, lua_toclose
	addi r17, fp, -40
	add r3, r13, r0
	add r4, r17, r0
	jal r31, lua_getallocf
	add r18, r1, r0
	add r3, r13, r0
	add r4, r14, r0
	jal r31, lua_touserdata
	add r16, r1, r0
	ldw r3, r17+0
	ldw r4, r1+0
	ldw r5, r1+4
	add r6, r12, r0
	jalr lr, r18, 0
	add r14, r1, r0
	beq r12, r15, .LBB29_12
.LBB29_11:
	beq r14, r15, .LBB29_17
.LBB29_12:
	stw r16+0, r14
	stw r16+4, r12
	ldw r4, r11+0
	ldw r5, r11+8
	add r3, r14, r0
	jal r31, memcpy
.LBB29_13:
	stw r11+0, r14
	stw r11+4, r12
	ldw r1, r11+8
	add r1, r14, r1
.LBB29_14:
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
.LBB29_15:
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	add r3, r13, r0
	jal r31, luaL_error
	add r12, r1, r0
	jal r0, .LBB29_3
.LBB29_16:
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	add r3, r13, r0
	jal r31, lua_pushstring
	add r3, r13, r0
	jal r31, lua_error
	jal r0, .LBB29_6
.LBB29_17:
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	add r3, r13, r0
	jal r31, lua_pushstring
	add r3, r13, r0
	jal r31, lua_error
	jal r0, .LBB29_12
.Lfunc_end29:
	.size	prepbuffsize, .Lfunc_end29-prepbuffsize
                                        # -- End function
	.globl	luaL_addlstring                 # -- Begin function luaL_addlstring
	.p2align	2
	.type	luaL_addlstring,@function
luaL_addlstring:                        # @luaL_addlstring
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
	addi r1, r0, 0
	beq r5, r1, .LBB30_2
.LBB30_1:
	add r11, r4, r0
	addi r1, r0, -1
	add r12, r3, r0
	add r4, r5, r0
	add r13, r5, r0
	add r5, r1, r0
	jal r31, prepbuffsize
	add r3, r1, r0
	add r4, r11, r0
	add r5, r13, r0
	jal r31, memcpy
	ldw r1, r12+8
	add r1, r1, r13
	stw r12+8, r1
.LBB30_2:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end30:
	.size	luaL_addlstring, .Lfunc_end30-luaL_addlstring
                                        # -- End function
	.globl	luaL_pushresultsize             # -- Begin function luaL_pushresultsize
	.p2align	2
	.type	luaL_pushresultsize,@function
luaL_pushresultsize:                    # @luaL_pushresultsize
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r12, r3, r0
	ldw r1, r3+8
	add r5, r1, r4
	stw r3+8, r5
	ldw r11, r3+12
	ldw r4, r3+0
	add r3, r11, r0
	jal r31, lua_pushlstring
	ldw r1, r12+0
	addi r3, r12, 16
	beq r1, r3, .LBB31_2
.LBB31_1:
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_closeslot
.LBB31_2:
	addi r12, r0, -2
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rotate
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_settop
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end31:
	.size	luaL_pushresultsize, .Lfunc_end31-luaL_pushresultsize
                                        # -- End function
	.globl	luaL_buffinitsize               # -- Begin function luaL_buffinitsize
	.p2align	2
	.type	luaL_buffinitsize,@function
luaL_buffinitsize:                      # @luaL_buffinitsize
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r5, r0
	add r12, r4, r0
	stw r4+12, r3
	addi r1, r4, 16
	stw r4+0, r1
	addi r1, r0, 0
	stw r4+8, r1
	addi r1, r0, 512
	stw r4+4, r1
	jal r31, lua_pushlightuserdata
	addi r5, r0, -1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, prepbuffsize
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end32:
	.size	luaL_buffinitsize, .Lfunc_end32-luaL_buffinitsize
                                        # -- End function
	.globl	luaL_ref                        # -- Begin function luaL_ref
	.p2align	2
	.type	luaL_ref,@function
luaL_ref:                               # @luaL_ref
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
	addi r13, r0, -1
	add r4, r13, r0
	jal r31, lua_type
	addi r14, r0, 0
	beq r1, r14, .LBB33_3
.LBB33_1:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_absindex
	add r12, r1, r0
	addi r5, r0, 3
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_rawgeti
	beq r1, r14, .LBB33_4
.LBB33_2:
	addi r4, r0, -1
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lua_tointegerx
	add r13, r1, r0
	jal r0, .LBB33_5
.LBB33_3:
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	jal r0, .LBB33_9
.LBB33_4:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushinteger
	addi r5, r0, 3
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rawseti
	add r13, r14, r0
.LBB33_5:
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r12, r0
	beq r13, r14, .LBB33_7
.LBB33_6:
	add r5, r13, r0
	jal r31, lua_rawgeti
	addi r5, r0, 3
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rawseti
	jal r0, .LBB33_8
.LBB33_7:
	jal r31, lua_rawlen
	addi r13, r1, 1
.LBB33_8:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, lua_rawseti
.LBB33_9:
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
.Lfunc_end33:
	.size	luaL_ref, .Lfunc_end33-luaL_ref
                                        # -- End function
	.globl	luaL_unref                      # -- Begin function luaL_unref
	.p2align	2
	.type	luaL_unref,@function
luaL_unref:                             # @luaL_unref
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
	addi r1, r0, 0
	blt r5, r1, .LBB34_2
.LBB34_1:
	add r11, r5, r0
	add r12, r3, r0
	jal r31, lua_absindex
	add r13, r1, r0
	addi r14, r0, 3
	add r3, r12, r0
	add r4, r1, r0
	add r5, r14, r0
	jal r31, lua_rawgeti
	add r3, r12, r0
	add r4, r13, r0
	add r5, r11, r0
	jal r31, lua_rawseti
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_pushinteger
	add r3, r12, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, lua_rawseti
.LBB34_2:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end34:
	.size	luaL_unref, .Lfunc_end34-luaL_unref
                                        # -- End function
	.globl	luaL_loadfilex                  # -- Begin function luaL_loadfilex
	.p2align	2
	.type	luaL_loadfilex,@function
luaL_loadfilex:                         # @luaL_loadfilex
# %bb.0:
	addi sp, sp, -1080
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 1080
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, lr
	add r15, r5, r0
	add r14, r4, r0
	add r11, r3, r0
	jal r31, lua_gettop
	addi r12, r1, 1
	addi r13, r0, 0
	lui r18, %hi(errno)
	addi r18, r18, %lo(errno)
	beq r14, r13, .LBB35_3
.LBB35_1:
	lui r4, %hi(.L.str.26)
	addi r4, r4, %lo(.L.str.26)
	add r3, r11, r0
	add r5, r14, r0
	jal r31, lua_pushfstring
	stw r18+0, r13
	lui r4, %hi(.L.str.27)
	addi r4, r4, %lo(.L.str.27)
	add r3, r14, r0
	jal r31, fopen
	addi r3, fp, -1068
	stw r3+4, r1
	bne r1, r13, .LBB35_4
.LBB35_2:
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	jal r0, .LBB35_16
.LBB35_3:
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	add r3, r11, r0
	jal r31, lua_pushstring
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	ldw r1, r1+0
	addi r3, fp, -1068
	stw r3+4, r1
.LBB35_4:
	addi r16, fp, -1068
	stw r16+0, r13
	ldw r3, r16+4
	addi r17, fp, -1072
	add r4, r17, r0
	jal r31, skipcomment
	beq r1, r13, .LBB35_6
.LBB35_5:
	ldw r1, r16+0
	add r3, r16, r1
	addi r1, r1, 1
	stw r16+0, r1
	addi r1, r0, 10
	stb r3+8, r1
.LBB35_6:
	ldw r1, r17+0
	addi r3, r0, 27
	bne r1, r3, .LBB35_10
.LBB35_7:
	stw r16+0, r13
	beq r14, r13, .LBB35_10
.LBB35_8:
	stw r18+0, r13
	ldw r5, r16+4
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	add r3, r14, r0
	jal r31, freopen
	stw r16+4, r1
	beq r1, r13, .LBB35_19
.LBB35_9:
	addi r4, fp, -1072
	add r3, r1, r0
	jal r31, skipcomment
.LBB35_10:
	ldw r1, r17+0
	addi r3, r0, -1
	beq r1, r3, .LBB35_12
.LBB35_11:
	ldw r3, r16+0
	add r4, r16, r3
	addi r3, r3, 1
	stw r16+0, r3
	stb r4+8, r1
.LBB35_12:
	stw r18+0, r13
	addi r4, r0, -1
	add r3, r11, r0
	add r5, r13, r0
	jal r31, lua_tolstring
	lui r4, %hi(getF)
	addi r4, r4, %lo(getF)
	add r3, r11, r0
	add r5, r16, r0
	add r6, r1, r0
	add r7, r15, r0
	jal r31, lua_load
	add r15, r1, r0
	ldw r3, r16+4
	jal r31, ferror
	add r17, r1, r0
	beq r14, r13, .LBB35_14
.LBB35_13:
	ldw r3, r16+4
	jal r31, fclose
.LBB35_14:
	beq r17, r13, .LBB35_17
.LBB35_15:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_settop
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
.LBB35_16:
	add r3, r11, r0
	add r5, r12, r0
	jal r31, errfile
	addi r1, r0, 6
	jal r0, .LBB35_18
.LBB35_17:
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rotate
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	add r1, r15, r0
.LBB35_18:
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
	addi sp, sp, 1080
	jalr r0, r31, 0
.LBB35_19:
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	jal r0, .LBB35_16
.Lfunc_end35:
	.size	luaL_loadfilex, .Lfunc_end35-luaL_loadfilex
                                        # -- End function
	.p2align	2                               # -- Begin function errfile
	.type	errfile,@function
errfile:                                # @errfile
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
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	ldw r15, r1+0
	addi r16, r0, 0
	add r4, r5, r0
	add r5, r16, r0
	jal r31, lua_tolstring
	addi r14, r1, 1
	beq r15, r16, .LBB36_2
.LBB36_1:
	add r3, r15, r0
	jal r31, strerror
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	add r3, r11, r0
	add r5, r13, r0
	add r6, r14, r0
	add r7, r1, r0
	jal r0, .LBB36_3
.LBB36_2:
	lui r4, %hi(.L.str.61)
	addi r4, r4, %lo(.L.str.61)
	add r3, r11, r0
	add r5, r13, r0
	add r6, r14, r0
.LBB36_3:
	jal r31, lua_pushfstring
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_rotate
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
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
.Lfunc_end36:
	.size	errfile, .Lfunc_end36-errfile
                                        # -- End function
	.p2align	2                               # -- Begin function skipcomment
	.type	skipcomment,@function
skipcomment:                            # @skipcomment
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
	add r11, r4, r0
	add r12, r3, r0
	jal r31, getc
	addi r13, r0, 239
	bne r1, r13, .LBB37_5
.LBB37_1:
	add r3, r12, r0
	jal r31, getc
	addi r3, r0, 187
	bne r1, r3, .LBB37_4
.LBB37_2:
	add r3, r12, r0
	jal r31, getc
	add r3, r1, r0
	addi r4, r0, 191
	add r1, r13, r0
	bne r3, r4, .LBB37_5
.LBB37_3:
	add r3, r12, r0
	jal r31, getc
	jal r0, .LBB37_5
.LBB37_4:
	add r1, r13, r0
.LBB37_5:
	stw r11+0, r1
	addi r3, r0, 35
	bne r1, r3, .LBB37_10
.LBB37_6:
	addi r13, r0, 10
	addi r14, r0, -1
.LBB37_7:
	add r3, r12, r0
	jal r31, getc
	beq r1, r13, .LBB37_9
.LBB37_8:
	bne r1, r14, .LBB37_7
.LBB37_9:
	add r3, r12, r0
	jal r31, getc
	stw r11+0, r1
	addi r1, r0, 1
	jal r0, .LBB37_11
.LBB37_10:
	addi r1, r0, 0
.LBB37_11:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end37:
	.size	skipcomment, .Lfunc_end37-skipcomment
                                        # -- End function
	.p2align	2                               # -- Begin function getF
	.type	getF,@function
getF:                                   # @getF
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
	ldw r1, r4+0
	addi r3, r0, 1
	blt r1, r3, .LBB38_3
.LBB38_1:
	stw r5+0, r1
	addi r1, r0, 0
	stw r11+0, r1
.LBB38_2:
	addi r1, r11, 8
	jal r0, .LBB38_4
.LBB38_3:
	add r12, r5, r0
	ldw r3, r11+4
	jal r31, feof
	add r3, r1, r0
	addi r1, r0, 0
	beq r3, r1, .LBB38_5
.LBB38_4:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB38_5:
	addi r3, r11, 8
	ldw r6, r11+4
	addi r4, r0, 1
	addi r5, r0, 1024
	jal r31, fread
	stw r12+0, r1
	jal r0, .LBB38_2
.Lfunc_end38:
	.size	getF, .Lfunc_end38-getF
                                        # -- End function
	.globl	luaL_loadbufferx                # -- Begin function luaL_loadbufferx
	.p2align	2
	.type	luaL_loadbufferx,@function
luaL_loadbufferx:                       # @luaL_loadbufferx
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r1, fp, -12
	stw r1+0, r4
	stw r1+4, r5
	lui r4, %hi(getS)
	addi r4, r4, %lo(getS)
	add r5, r1, r0
	jal r31, lua_load
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end39:
	.size	luaL_loadbufferx, .Lfunc_end39-luaL_loadbufferx
                                        # -- End function
	.p2align	2                               # -- Begin function getS
	.type	getS,@function
getS:                                   # @getS
# %bb.0:
	ldw r3, r4+4
	addi r1, r0, 0
	beq r3, r1, .LBB40_2
.LBB40_1:
	stw r5+0, r3
	addi r1, r0, 0
	stw r4+4, r1
	ldw r1, r4+0
.LBB40_2:
	jalr r0, r31, 0
.Lfunc_end40:
	.size	getS, .Lfunc_end40-getS
                                        # -- End function
	.globl	luaL_loadstring                 # -- Begin function luaL_loadstring
	.p2align	2
	.type	luaL_loadstring,@function
luaL_loadstring:                        # @luaL_loadstring
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r4, r0
	add r12, r3, r0
	add r3, r4, r0
	jal r31, strlen
	addi r5, fp, -20
	stw r5+0, r11
	stw r5+4, r1
	lui r4, %hi(getS)
	addi r4, r4, %lo(getS)
	addi r7, r0, 0
	add r3, r12, r0
	add r6, r11, r0
	jal r31, lua_load
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end41:
	.size	luaL_loadstring, .Lfunc_end41-luaL_loadstring
                                        # -- End function
	.globl	luaL_callmeta                   # -- Begin function luaL_callmeta
	.p2align	2
	.type	luaL_callmeta,@function
luaL_callmeta:                          # @luaL_callmeta
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
	add r14, r5, r0
	add r11, r3, r0
	jal r31, lua_absindex
	add r12, r1, r0
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_getmetatable
	addi r13, r0, 0
	beq r1, r13, .LBB42_4
.LBB42_1:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushstring
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_rawget
	addi r13, r0, 0
	beq r1, r13, .LBB42_3
.LBB42_2:
	addi r13, r0, -2
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_rotate
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushvalue
	addi r13, r0, 1
	addi r6, r0, 0
	add r3, r11, r0
	add r4, r13, r0
	add r5, r13, r0
	add r7, r6, r0
	jal r31, lua_callk
	jal r0, .LBB42_4
.LBB42_3:
	addi r4, r0, -3
	add r3, r11, r0
	jal r31, lua_settop
.LBB42_4:
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
.Lfunc_end42:
	.size	luaL_callmeta, .Lfunc_end42-luaL_callmeta
                                        # -- End function
	.globl	luaL_len                        # -- Begin function luaL_len
	.p2align	2
	.type	luaL_len,@function
luaL_len:                               # @luaL_len
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
	jal r31, lua_len
	addi r4, r0, -1
	addi r13, fp, -20
	add r3, r11, r0
	add r5, r13, r0
	jal r31, lua_tointegerx
	add r12, r1, r0
	ldw r1, r13+0
	addi r3, r0, 0
	beq r1, r3, .LBB43_2
.LBB43_1:
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB43_2:
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB43_1
.Lfunc_end43:
	.size	luaL_len, .Lfunc_end43-luaL_len
                                        # -- End function
	.globl	luaL_tolstring                  # -- Begin function luaL_tolstring
	.p2align	2
	.type	luaL_tolstring,@function
luaL_tolstring:                         # @luaL_tolstring
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
	add r11, r5, r0
	add r12, r3, r0
	jal r31, lua_absindex
	add r13, r1, r0
	lui r5, %hi(.L.str.34)
	addi r5, r5, %lo(.L.str.34)
	add r3, r12, r0
	add r4, r1, r0
	jal r31, luaL_callmeta
	addi r16, r0, 0
	beq r1, r16, .LBB44_3
.LBB44_1:
	addi r4, r0, -1
	add r3, r12, r0
	jal r31, lua_isstring
	bne r1, r16, .LBB44_22
.LBB44_2:
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	add r3, r12, r0
	jal r31, luaL_error
	jal r0, .LBB44_22
.LBB44_3:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_type
	addi r17, r0, 4
	bgtu r1, r17, .LBB44_11
.LBB44_4:
	slli r1, r1, 2
	lui r3, %hi(.LJTI44_0)
	addi r3, r3, %lo(.LJTI44_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB44_5:
	lui r4, %hi(.L.str.40)
	addi r4, r4, %lo(.L.str.40)
	jal r0, .LBB44_9
.LBB44_6:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_isinteger
	addi r5, r0, 0
	add r3, r12, r0
	add r4, r13, r0
	beq r1, r16, .LBB44_14
.LBB44_7:
	jal r31, lua_tointegerx
	lui r4, %hi(.L.str.36)
	addi r4, r4, %lo(.L.str.36)
	add r3, r12, r0
	add r5, r1, r0
	jal r31, lua_pushfstring
	jal r0, .LBB44_22
.LBB44_8:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_toboolean
	seq r1, r1, r16
	lui r3, %hi(.L.str.38)
	addi r3, r3, %lo(.L.str.38)
	lui r4, %hi(.L.str.39)
	addi r4, r4, %lo(.L.str.39)
	xor r4, r4, r3
	sub r1, r16, r1
	and r1, r4, r1
	xor r4, r1, r3
.LBB44_9:
	add r3, r12, r0
	jal r31, lua_pushstring
	jal r0, .LBB44_22
.LBB44_10:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_pushvalue
	jal r0, .LBB44_22
.LBB44_11:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_getmetatable
	beq r1, r16, .LBB44_17
.LBB44_12:
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
	add r3, r12, r0
	jal r31, lua_pushstring
	addi r14, r0, -2
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_rawget
	addi r16, r0, 0
	beq r1, r16, .LBB44_15
.LBB44_13:
	add r15, r1, r0
	addi r5, r0, -1
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_rotate
	add r16, r15, r0
	jal r0, .LBB44_16
.LBB44_14:
	jal r31, lua_tonumberx
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	add r3, r12, r0
	add r5, r1, r0
	add r6, r2, r0
	jal r31, lua_pushfstring
	jal r0, .LBB44_22
.LBB44_15:
	addi r14, r0, -3
.LBB44_16:
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_settop
.LBB44_17:
	bne r16, r17, .LBB44_19
.LBB44_18:
	addi r4, r0, -1
	addi r5, r0, 0
	add r3, r12, r0
	jal r31, lua_tolstring
	jal r0, .LBB44_20
.LBB44_19:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_type
	add r3, r12, r0
	add r4, r1, r0
	jal r31, lua_typename
.LBB44_20:
	add r14, r1, r0
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_topointer
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	add r3, r12, r0
	add r5, r14, r0
	add r6, r1, r0
	jal r31, lua_pushfstring
	addi r1, r0, 0
	beq r16, r1, .LBB44_22
.LBB44_21:
	addi r13, r0, -2
	addi r5, r0, -1
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_rotate
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_settop
.LBB44_22:
	addi r4, r0, -1
	add r3, r12, r0
	add r5, r11, r0
	jal r31, lua_tolstring
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
.Lfunc_end44:
	.size	luaL_tolstring, .Lfunc_end44-luaL_tolstring
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI44_0:
	.word	.LBB44_5
	.word	.LBB44_8
	.word	.LBB44_11
	.word	.LBB44_6
	.word	.LBB44_10
                                        # -- End function
	.text
	.globl	luaL_setfuncs                   # -- Begin function luaL_setfuncs
	.p2align	2
	.type	luaL_setfuncs,@function
luaL_setfuncs:                          # @luaL_setfuncs
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
	add r12, r5, r0
	add r13, r4, r0
	add r11, r3, r0
	add r4, r5, r0
	jal r31, lua_checkstack
	addi r14, r0, 0
	beq r1, r14, .LBB45_9
.LBB45_1:
	ldw r1, r13+0
	beq r1, r14, .LBB45_10
.LBB45_2:
	sub r15, r14, r12
	addi r1, r0, -2
	sub r16, r1, r12
	addi r17, r0, 1
	jal r0, .LBB45_5
.LBB45_3:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushboolean
.LBB45_4:
	ldw r5, r13+0
	add r3, r11, r0
	add r4, r16, r0
	jal r31, lua_setfield
	addi r1, r13, 8
	ldw r3, r13+8
	add r13, r1, r0
	beq r3, r14, .LBB45_10
.LBB45_5:
	ldw r1, r13+4
	beq r1, r14, .LBB45_3
.LBB45_6:
	add r18, r12, r0
	blt r12, r17, .LBB45_8
.LBB45_7:
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_pushvalue
	addi r18, r18, -1
	bne r18, r14, .LBB45_7
.LBB45_8:
	ldw r4, r13+4
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_pushcclosure
	jal r0, .LBB45_4
.LBB45_9:
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	lui r5, %hi(.L.str.42)
	addi r5, r5, %lo(.L.str.42)
	add r3, r11, r0
	jal r31, luaL_error
	ldw r1, r13+0
	bne r1, r14, .LBB45_2
.LBB45_10:
	addi r1, r0, -1
	xor r4, r12, r1
	add r3, r11, r0
	jal r31, lua_settop
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
.Lfunc_end45:
	.size	luaL_setfuncs, .Lfunc_end45-luaL_setfuncs
                                        # -- End function
	.globl	luaL_getsubtable                # -- Begin function luaL_getsubtable
	.p2align	2
	.type	luaL_getsubtable,@function
luaL_getsubtable:                       # @luaL_getsubtable
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
	add r13, r4, r0
	add r12, r3, r0
	jal r31, lua_getfield
	addi r3, r0, 5
	bne r1, r3, .LBB46_2
.LBB46_1:
	addi r13, r0, 1
	jal r0, .LBB46_3
.LBB46_2:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_absindex
	add r14, r1, r0
	addi r13, r0, 0
	add r3, r12, r0
	add r4, r13, r0
	add r5, r13, r0
	jal r31, lua_createtable
	addi r4, r0, -1
	add r3, r12, r0
	jal r31, lua_pushvalue
	add r3, r12, r0
	add r4, r14, r0
	add r5, r11, r0
	jal r31, lua_setfield
.LBB46_3:
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
.Lfunc_end46:
	.size	luaL_getsubtable, .Lfunc_end46-luaL_getsubtable
                                        # -- End function
	.globl	luaL_requiref                   # -- Begin function luaL_requiref
	.p2align	2
	.type	luaL_requiref,@function
luaL_requiref:                          # @luaL_requiref
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
	add r14, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	lui r5, %hi(.L.str.43)
	addi r5, r5, %lo(.L.str.43)
	lui r1, 1048572
	addi r15, r1, 384
	add r4, r15, r0
	jal r31, lua_getfield
	addi r3, r0, 5
	beq r1, r3, .LBB47_2
.LBB47_1:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
	add r3, r12, r0
	add r4, r15, r0
	jal r31, lua_absindex
	add r15, r1, r0
	addi r4, r0, 0
	add r3, r12, r0
	add r5, r4, r0
	jal r31, lua_createtable
	addi r4, r0, -1
	add r3, r12, r0
	jal r31, lua_pushvalue
	lui r5, %hi(.L.str.43)
	addi r5, r5, %lo(.L.str.43)
	add r3, r12, r0
	add r4, r15, r0
	jal r31, lua_setfield
.LBB47_2:
	addi r15, r0, -1
	add r3, r12, r0
	add r4, r15, r0
	add r5, r11, r0
	jal r31, lua_getfield
	add r3, r12, r0
	add r4, r15, r0
	jal r31, lua_toboolean
	addi r16, r0, 0
	bne r1, r16, .LBB47_4
.LBB47_3:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
	addi r15, r0, 0
	add r3, r12, r0
	add r4, r14, r0
	add r5, r15, r0
	jal r31, lua_pushcclosure
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_pushstring
	addi r4, r0, 1
	add r3, r12, r0
	add r5, r4, r0
	add r6, r15, r0
	add r7, r15, r0
	jal r31, lua_callk
	addi r4, r0, -1
	add r3, r12, r0
	jal r31, lua_pushvalue
	addi r4, r0, -3
	add r3, r12, r0
	add r5, r11, r0
	jal r31, lua_setfield
.LBB47_4:
	addi r14, r0, -2
	addi r5, r0, -1
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_rotate
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_settop
	beq r13, r16, .LBB47_6
.LBB47_5:
	addi r4, r0, -1
	add r3, r12, r0
	jal r31, lua_pushvalue
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_setglobal
.LBB47_6:
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
.Lfunc_end47:
	.size	luaL_requiref, .Lfunc_end47-luaL_requiref
                                        # -- End function
	.globl	luaL_addgsub                    # -- Begin function luaL_addgsub
	.p2align	2
	.type	luaL_addgsub,@function
luaL_addgsub:                           # @luaL_addgsub
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
	add r12, r6, r0
	add r13, r5, r0
	add r15, r4, r0
	add r11, r3, r0
	add r3, r5, r0
	jal r31, strlen
	add r14, r1, r0
	add r3, r15, r0
	add r4, r13, r0
	jal r31, strstr
	addi r19, r0, 0
	beq r1, r19, .LBB48_7
.LBB48_1:
	add r17, r1, r0
	addi r16, r0, -1
	jal r0, .LBB48_3
.LBB48_2:
	add r15, r17, r14
	add r3, r15, r0
	add r4, r13, r0
	jal r31, strstr
	add r17, r1, r0
	beq r1, r19, .LBB48_7
.LBB48_3:
	beq r17, r15, .LBB48_5
.LBB48_4:
	sub r18, r17, r15
	add r3, r11, r0
	add r4, r18, r0
	add r5, r16, r0
	jal r31, prepbuffsize
	add r3, r1, r0
	add r4, r15, r0
	add r5, r18, r0
	jal r31, memcpy
	ldw r1, r11+8
	add r1, r1, r18
	stw r11+8, r1
.LBB48_5:
	add r3, r12, r0
	jal r31, strlen
	beq r1, r19, .LBB48_2
.LBB48_6:
	add r3, r11, r0
	add r4, r1, r0
	add r5, r16, r0
	add r15, r1, r0
	jal r31, prepbuffsize
	add r3, r1, r0
	add r4, r12, r0
	add r5, r15, r0
	jal r31, memcpy
	ldw r1, r11+8
	add r1, r1, r15
	stw r11+8, r1
	jal r0, .LBB48_2
.LBB48_7:
	add r3, r15, r0
	jal r31, strlen
	beq r1, r19, .LBB48_9
.LBB48_8:
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r1, r0
	add r12, r1, r0
	jal r31, prepbuffsize
	add r3, r1, r0
	add r4, r15, r0
	add r5, r12, r0
	jal r31, memcpy
	ldw r1, r11+8
	add r1, r1, r12
	stw r11+8, r1
.LBB48_9:
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
.Lfunc_end48:
	.size	luaL_addgsub, .Lfunc_end48-luaL_addgsub
                                        # -- End function
	.globl	luaL_gsub                       # -- Begin function luaL_gsub
	.p2align	2
	.type	luaL_gsub,@function
luaL_gsub:                              # @luaL_gsub
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
	stw fp+-28, lr
	add r12, r6, r0
	add r13, r5, r0
	add r14, r4, r0
	add r11, r3, r0
	addi r15, fp, -556
	stw r15+12, r3
	addi r16, r15, 16
	stw r15+0, r16
	addi r1, r0, 0
	stw r15+8, r1
	addi r1, r0, 512
	stw r15+4, r1
	add r4, r15, r0
	jal r31, lua_pushlightuserdata
	add r3, r15, r0
	add r4, r14, r0
	add r5, r13, r0
	add r6, r12, r0
	jal r31, luaL_addgsub
	ldw r12, r15+12
	ldw r4, r15+0
	ldw r5, r15+8
	add r3, r12, r0
	jal r31, lua_pushlstring
	ldw r1, r15+0
	beq r1, r16, .LBB49_2
.LBB49_1:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_closeslot
.LBB49_2:
	addi r13, r0, -2
	addi r14, r0, -1
	add r3, r12, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, lua_rotate
	add r3, r12, r0
	add r4, r13, r0
	jal r31, lua_settop
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_tolstring
	ldw lr, fp+-28
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
.Lfunc_end49:
	.size	luaL_gsub, .Lfunc_end49-luaL_gsub
                                        # -- End function
	.globl	luaL_newstate                   # -- Begin function luaL_newstate
	.p2align	2
	.type	luaL_newstate,@function
luaL_newstate:                          # @luaL_newstate
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	lui r3, %hi(l_alloc)
	addi r3, r3, %lo(l_alloc)
	addi r12, r0, 0
	add r4, r12, r0
	jal r31, lua_newstate
	add r11, r1, r0
	beq r1, r12, .LBB50_2
.LBB50_1:
	lui r4, %hi(panic)
	addi r4, r4, %lo(panic)
	add r3, r11, r0
	jal r31, lua_atpanic
	lui r4, %hi(warnfoff)
	addi r4, r4, %lo(warnfoff)
	add r3, r11, r0
	add r5, r11, r0
	jal r31, lua_setwarnf
.LBB50_2:
	add r1, r11, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end50:
	.size	luaL_newstate, .Lfunc_end50-luaL_newstate
                                        # -- End function
	.p2align	2                               # -- Begin function l_alloc
	.type	l_alloc,@function
l_alloc:                                # @l_alloc
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r3, r4, r0
	addi r1, r0, 0
	beq r6, r1, .LBB51_2
.LBB51_1:
	add r4, r6, r0
	jal r31, realloc
	jal r0, .LBB51_3
.LBB51_2:
	add r11, r1, r0
	jal r31, free
	add r1, r11, r0
.LBB51_3:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end51:
	.size	l_alloc, .Lfunc_end51-l_alloc
                                        # -- End function
	.p2align	2                               # -- Begin function panic
	.type	panic,@function
panic:                                  # @panic
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	addi r4, r0, -1
	jal r31, lua_type
	addi r3, r0, 4
	bne r1, r3, .LBB52_2
.LBB52_1:
	addi r4, r0, -1
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lua_tolstring
	add r5, r1, r0
	jal r0, .LBB52_3
.LBB52_2:
	lui r5, %hi(.L.str.62)
	addi r5, r5, %lo(.L.str.62)
.LBB52_3:
	lui r11, %hi(stderr)
	addi r11, r11, %lo(stderr)
	ldw r3, r11+0
	lui r4, %hi(.L.str.63)
	addi r4, r4, %lo(.L.str.63)
	jal r31, fprintf
	ldw r3, r11+0
	jal r31, fflush
	addi r1, r0, 0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end52:
	.size	panic, .Lfunc_end52-panic
                                        # -- End function
	.p2align	2                               # -- Begin function warnfoff
	.type	warnfoff,@function
warnfoff:                               # @warnfoff
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
	addi r12, r0, 0
	bne r5, r12, .LBB53_7
.LBB53_1:
	ldbu r1, r4+0
	addi r5, r0, 64
	bne r1, r5, .LBB53_7
.LBB53_2:
	add r13, r3, r0
	addi r11, r4, 1
	lui r4, %hi(.L.str.64)
	addi r4, r4, %lo(.L.str.64)
	add r3, r11, r0
	jal r31, strcmp
	beq r1, r12, .LBB53_5
.LBB53_3:
	lui r4, %hi(.L.str.65)
	addi r4, r4, %lo(.L.str.65)
	add r3, r11, r0
	jal r31, strcmp
	bne r1, r12, .LBB53_7
.LBB53_4:
	lui r4, %hi(warnfon)
	addi r4, r4, %lo(warnfon)
	jal r0, .LBB53_6
.LBB53_5:
	lui r4, %hi(warnfoff)
	addi r4, r4, %lo(warnfoff)
.LBB53_6:
	add r3, r13, r0
	add r5, r13, r0
	jal r31, lua_setwarnf
.LBB53_7:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end53:
	.size	warnfoff, .Lfunc_end53-warnfoff
                                        # -- End function
	.globl	luaL_checkversion_              # -- Begin function luaL_checkversion_
	.p2align	2
	.type	luaL_checkversion_,@function
luaL_checkversion_:                     # @luaL_checkversion_
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
	add r12, r4, r0
	add r15, r6, r0
	add r14, r5, r0
	add r11, r3, r0
	jal r31, lua_version
	add r8, r1, r0
	addi r1, r0, 72
	bne r12, r1, .LBB54_3
.LBB54_1:
	add r9, r2, r0
	feq.d r1, r8, r14
	addi r3, r0, 0
	bne r1, r3, .LBB54_5
.LBB54_2:
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	add r3, r11, r0
	add r5, r14, r0
	add r6, r15, r0
	add r7, r8, r0
	add r8, r9, r0
	jal r0, .LBB54_4
.LBB54_3:
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	add r3, r11, r0
.LBB54_4:
	jal r31, luaL_error
.LBB54_5:
	ldw lr, fp+-20
	ldw r15, fp+-16
	ldw r14, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end54:
	.size	luaL_checkversion_, .Lfunc_end54-luaL_checkversion_
                                        # -- End function
	.p2align	2                               # -- Begin function findfield
	.type	findfield,@function
findfield:                              # @findfield
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
	addi r13, r0, 0
	beq r5, r13, .LBB55_11
.LBB55_1:
	add r14, r5, r0
	add r12, r4, r0
	add r11, r3, r0
	addi r4, r0, -1
	jal r31, lua_type
	addi r3, r0, 5
	bne r1, r3, .LBB55_11
.LBB55_2:
	add r3, r11, r0
	jal r31, lua_pushnil
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_next
	addi r13, r0, 0
	beq r1, r13, .LBB55_11
.LBB55_3:
	addi r14, r14, -1
	addi r15, r0, -2
	addi r17, r0, 4
	addi r16, r0, -1
	addi r13, r0, 0
	jal r0, .LBB55_5
.LBB55_4:
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_next
	beq r1, r13, .LBB55_11
.LBB55_5:
	add r3, r11, r0
	add r4, r15, r0
	jal r31, lua_type
	bne r1, r17, .LBB55_4
.LBB55_6:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r16, r0
	jal r31, lua_rawequal
	bne r1, r13, .LBB55_9
.LBB55_7:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r14, r0
	jal r31, findfield
	beq r1, r13, .LBB55_4
.LBB55_8:
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	add r3, r11, r0
	jal r31, lua_pushstring
	addi r4, r0, -1
	addi r5, r0, -3
	add r3, r11, r0
	jal r31, lua_copy
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, lua_concat
	jal r0, .LBB55_10
.LBB55_9:
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
.LBB55_10:
	addi r13, r0, 1
.LBB55_11:
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
.Lfunc_end55:
	.size	findfield, .Lfunc_end55-findfield
                                        # -- End function
	.p2align	2                               # -- Begin function boxgc
	.type	boxgc,@function
boxgc:                                  # @boxgc
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
	addi r12, fp, -20
	add r4, r12, r0
	jal r31, lua_getallocf
	add r13, r1, r0
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_touserdata
	add r11, r1, r0
	ldw r3, r12+0
	ldw r4, r1+0
	ldw r5, r1+4
	addi r12, r0, 0
	add r6, r12, r0
	jalr lr, r13, 0
	stw r11+0, r1
	stw r11+4, r12
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end56:
	.size	boxgc, .Lfunc_end56-boxgc
                                        # -- End function
	.p2align	2                               # -- Begin function warnfon
	.type	warnfon,@function
warnfon:                                # @warnfon
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
	addi r14, r0, 0
	bne r5, r14, .LBB57_5
.LBB57_1:
	ldbu r1, r13+0
	addi r3, r0, 64
	bne r1, r3, .LBB57_5
.LBB57_2:
	addi r12, r13, 1
	lui r4, %hi(.L.str.64)
	addi r4, r4, %lo(.L.str.64)
	add r3, r12, r0
	jal r31, strcmp
	beq r1, r14, .LBB57_7
.LBB57_3:
	lui r4, %hi(.L.str.65)
	addi r4, r4, %lo(.L.str.65)
	add r3, r12, r0
	jal r31, strcmp
	bne r1, r14, .LBB57_6
.LBB57_4:
	lui r4, %hi(warnfon)
	addi r4, r4, %lo(warnfon)
	jal r0, .LBB57_8
.LBB57_5:
	lui r14, %hi(stderr)
	addi r14, r14, %lo(stderr)
	ldw r3, r14+0
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	lui r5, %hi(.L.str.67)
	addi r5, r5, %lo(.L.str.67)
	jal r31, fprintf
	ldw r3, r14+0
	jal r31, fflush
	add r3, r11, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, warnfcont
.LBB57_6:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB57_7:
	lui r4, %hi(warnfoff)
	addi r4, r4, %lo(warnfoff)
.LBB57_8:
	add r3, r11, r0
	add r5, r11, r0
	jal r31, lua_setwarnf
	jal r0, .LBB57_6
.Lfunc_end57:
	.size	warnfon, .Lfunc_end57-warnfon
                                        # -- End function
	.p2align	2                               # -- Begin function warnfcont
	.type	warnfcont,@function
warnfcont:                              # @warnfcont
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
	add r12, r5, r0
	add r5, r4, r0
	add r11, r3, r0
	lui r13, %hi(stderr)
	addi r13, r13, %lo(stderr)
	ldw r3, r13+0
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	jal r31, fprintf
	ldw r3, r13+0
	jal r31, fflush
	addi r1, r0, 0
	beq r12, r1, .LBB58_2
.LBB58_1:
	lui r4, %hi(warnfcont)
	addi r4, r4, %lo(warnfcont)
	jal r0, .LBB58_3
.LBB58_2:
	ldw r3, r13+0
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	lui r5, %hi(.L.str.68)
	addi r5, r5, %lo(.L.str.68)
	jal r31, fprintf
	ldw r3, r13+0
	jal r31, fflush
	lui r4, %hi(warnfon)
	addi r4, r4, %lo(warnfon)
.LBB58_3:
	add r3, r11, r0
	add r5, r11, r0
	jal r31, lua_setwarnf
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end58:
	.size	warnfcont, .Lfunc_end58-warnfcont
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"stack traceback:"
	.size	.L.str, 17

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"\n\t...\t(skipping %d levels)"
	.size	.L.str.1, 27

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"Slnt"
	.size	.L.str.2, 5

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"\n\t%s: in "
	.size	.L.str.3, 10

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"\n\t%s:%d: in "
	.size	.L.str.4, 13

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"\n\t(...tail calls...)"
	.size	.L.str.5, 21

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"bad argument #%d (%s)"
	.size	.L.str.6, 22

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"n"
	.size	.L.str.7, 2

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"method"
	.size	.L.str.8, 7

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"calling '%s' on bad self (%s)"
	.size	.L.str.9, 30

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"?"
	.size	.L.str.10, 2

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"bad argument #%d to '%s' (%s)"
	.size	.L.str.11, 30

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"__name"
	.size	.L.str.12, 7

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"light userdata"
	.size	.L.str.13, 15

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"%s expected, got %s"
	.size	.L.str.14, 20

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"Sl"
	.size	.L.str.15, 3

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"%s:%d: "
	.size	.L.str.16, 8

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.zero	1
	.size	.L.str.17, 1

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"(no extra info)"
	.size	.L.str.18, 16

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"%s: %s"
	.size	.L.str.19, 7

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"exit"
	.size	.L.str.20, 5

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"invalid option '%s'"
	.size	.L.str.21, 20

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"stack overflow (%s)"
	.size	.L.str.22, 20

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"stack overflow"
	.size	.L.str.23, 15

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"value expected"
	.size	.L.str.24, 15

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"=stdin"
	.size	.L.str.25, 7

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"@%s"
	.size	.L.str.26, 4

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"r"
	.size	.L.str.27, 2

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"open"
	.size	.L.str.28, 5

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"rb"
	.size	.L.str.30, 3

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"reopen"
	.size	.L.str.31, 7

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"read"
	.size	.L.str.32, 5

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"object length is not an integer"
	.size	.L.str.33, 32

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"__tostring"
	.size	.L.str.34, 11

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"'__tostring' must return a string"
	.size	.L.str.35, 34

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"%I"
	.size	.L.str.36, 3

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"%f"
	.size	.L.str.37, 3

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"true"
	.size	.L.str.38, 5

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"false"
	.size	.L.str.39, 6

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"nil"
	.size	.L.str.40, 4

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"%s: %p"
	.size	.L.str.41, 7

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"too many upvalues"
	.size	.L.str.42, 18

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"_LOADED"
	.size	.L.str.43, 8

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"core and library have incompatible numeric types"
	.size	.L.str.44, 49

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"version mismatch: app. needs %f, Lua core provides %f"
	.size	.L.str.45, 54

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"function '%s'"
	.size	.L.str.46, 14

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"%s '%s'"
	.size	.L.str.47, 8

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"main chunk"
	.size	.L.str.48, 11

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"function <%s:%d>"
	.size	.L.str.49, 17

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"f"
	.size	.L.str.50, 2

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"not enough stack"
	.size	.L.str.51, 17

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"_G."
	.size	.L.str.52, 4

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"."
	.size	.L.str.53, 2

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"number has no integer representation"
	.size	.L.str.54, 37

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"buffer too large"
	.size	.L.str.55, 17

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"not enough memory"
	.size	.L.str.56, 18

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"_UBOX*"
	.size	.L.str.57, 7

	.type	boxmt,@object                   # @boxmt
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
boxmt:
	.word	.L.str.58
	.word	boxgc
	.word	.L.str.59
	.word	boxgc
	.zero	8
	.size	boxmt, 24

	.type	.L.str.58,@object               # @.str.58
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.58:
	.asciz	"__gc"
	.size	.L.str.58, 5

	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"__close"
	.size	.L.str.59, 8

	.type	.L.str.60,@object               # @.str.60
.L.str.60:
	.asciz	"cannot %s %s: %s"
	.size	.L.str.60, 17

	.type	.L.str.61,@object               # @.str.61
.L.str.61:
	.asciz	"cannot %s %s"
	.size	.L.str.61, 13

	.type	.L.str.62,@object               # @.str.62
.L.str.62:
	.asciz	"error object is not a string"
	.size	.L.str.62, 29

	.type	.L.str.63,@object               # @.str.63
.L.str.63:
	.asciz	"PANIC: unprotected error in call to Lua API (%s)\n"
	.size	.L.str.63, 50

	.type	.L.str.64,@object               # @.str.64
.L.str.64:
	.asciz	"off"
	.size	.L.str.64, 4

	.type	.L.str.65,@object               # @.str.65
.L.str.65:
	.asciz	"on"
	.size	.L.str.65, 3

	.type	.L.str.66,@object               # @.str.66
.L.str.66:
	.asciz	"%s"
	.size	.L.str.66, 3

	.type	.L.str.67,@object               # @.str.67
.L.str.67:
	.asciz	"Lua warning: "
	.size	.L.str.67, 14

	.type	.L.str.68,@object               # @.str.68
.L.str.68:
	.asciz	"\n"
	.size	.L.str.68, 2

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
