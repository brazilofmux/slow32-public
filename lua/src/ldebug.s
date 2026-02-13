	.file	"ldebug.c"
	.text
	.hidden	luaG_getfuncline                # -- Begin function luaG_getfuncline
	.globl	luaG_getfuncline
	.p2align	2
	.type	luaG_getfuncline,@function
luaG_getfuncline:                       # @luaG_getfuncline
# %bb.0:
	ldw r6, r3+64
	addi r5, r0, 0
	beq r6, r5, .LBB0_4
.LBB0_1:
	ldw r7, r3+36
	beq r7, r5, .LBB0_3
.LBB0_2:
	ldw r1, r3+68
	ldw r8, r1+0
	bge r4, r8, .LBB0_5
.LBB0_3:
	addi r1, r3, 40
	addi r7, r0, -1
	ldw r1, r1+0
	bgt r4, r7, .LBB0_10
	jal r0, .LBB0_12
.LBB0_4:
	addi r1, r0, -1
	jal r0, .LBB0_12
.LBB0_5:
	srli r3, r4, 7
	xor r8, r7, r3
	sgt r7, r7, r3
	sub r7, r5, r7
	and r7, r8, r7
	xor r8, r3, r7
	addi r7, r8, -1
	slli r9, r3, 3
	add r9, r1, r9
.LBB0_6:
	beq r8, r3, .LBB0_9
.LBB0_7:
	ldw r10, r9+0
	addi r3, r3, 1
	addi r9, r9, 8
	bge r4, r10, .LBB0_6
.LBB0_8:
	addi r7, r3, -2
.LBB0_9:
	slli r3, r7, 3
	add r1, r1, r3
	ldw r7, r1+0
	addi r1, r1, 4
	ldw r1, r1+0
	ble r4, r7, .LBB0_12
.LBB0_10:
	sub r3, r4, r7
	add r4, r7, r6
	addi r4, r4, 1
.LBB0_11:
	ldb r6, r4+0
	add r1, r1, r6
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r5, .LBB0_11
.LBB0_12:
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaG_getfuncline, .Lfunc_end0-luaG_getfuncline
                                        # -- End function
	.globl	lua_sethook                     # -- Begin function lua_sethook
	.p2align	2
	.type	lua_sethook,@function
lua_sethook:                            # @lua_sethook
# %bb.0:
	addi r1, r0, 0
	seq r7, r4, r1
	seq r8, r5, r1
	sub r7, r1, r7
	and r7, r5, r7
	xor r5, r5, r7
	sub r7, r1, r8
	and r7, r4, r7
	xor r4, r4, r7
	stw r3+88, r4
	stw r3+104, r6
	stw r3+108, r6
	andi r4, r5, 255
	stw r3+112, r4
	beq r5, r1, .LBB1_6
.LBB1_1:
	ldw r3, r3+20
	beq r3, r1, .LBB1_6
.LBB1_2:
	addi r4, r0, 1
	jal r0, .LBB1_4
.LBB1_3:
	ldw r3, r3+8
	beq r3, r1, .LBB1_6
.LBB1_4:
	ldbu r5, r3+34
	andi r5, r5, 2
	bne r5, r1, .LBB1_3
.LBB1_5:
	stw r3+20, r4
	jal r0, .LBB1_3
.LBB1_6:
	jalr r0, r31, 0
.Lfunc_end1:
	.size	lua_sethook, .Lfunc_end1-lua_sethook
                                        # -- End function
	.globl	lua_gethook                     # -- Begin function lua_gethook
	.p2align	2
	.type	lua_gethook,@function
lua_gethook:                            # @lua_gethook
# %bb.0:
	ldw r1, r3+88
	jalr r0, r31, 0
.Lfunc_end2:
	.size	lua_gethook, .Lfunc_end2-lua_gethook
                                        # -- End function
	.globl	lua_gethookmask                 # -- Begin function lua_gethookmask
	.p2align	2
	.type	lua_gethookmask,@function
lua_gethookmask:                        # @lua_gethookmask
# %bb.0:
	ldw r1, r3+112
	jalr r0, r31, 0
.Lfunc_end3:
	.size	lua_gethookmask, .Lfunc_end3-lua_gethookmask
                                        # -- End function
	.globl	lua_gethookcount                # -- Begin function lua_gethookcount
	.p2align	2
	.type	lua_gethookcount,@function
lua_gethookcount:                       # @lua_gethookcount
# %bb.0:
	ldw r1, r3+104
	jalr r0, r31, 0
.Lfunc_end4:
	.size	lua_gethookcount, .Lfunc_end4-lua_gethookcount
                                        # -- End function
	.globl	lua_getstack                    # -- Begin function lua_getstack
	.p2align	2
	.type	lua_getstack,@function
lua_getstack:                           # @lua_getstack
# %bb.0:
	addi r1, r0, 0
	blt r4, r1, .LBB5_10
.LBB5_1:
	addi r8, r3, 52
	ldw r3, r3+20
	sne r6, r3, r8
	addi r1, r0, 0
	beq r4, r1, .LBB5_11
.LBB5_2:
	beq r3, r8, .LBB5_6
.LBB5_3:
	addi r9, r0, 2
.LBB5_4:
	addi r7, r4, -1
	ldw r3, r3+8
	sne r6, r3, r8
	blt r4, r9, .LBB5_7
.LBB5_5:
	add r4, r7, r0
	bne r3, r8, .LBB5_4
	jal r0, .LBB5_7
.LBB5_6:
	add r7, r4, r0
.LBB5_7:
	bne r7, r1, .LBB5_10
.LBB5_8:
	addi r4, r0, 0
	beq r6, r4, .LBB5_10
.LBB5_9:
	stw r5+104, r3
	addi r1, r0, 1
.LBB5_10:
	jalr r0, r31, 0
.LBB5_11:
	add r7, r4, r0
	beq r7, r1, .LBB5_8
	jal r0, .LBB5_10
.Lfunc_end5:
	.size	lua_getstack, .Lfunc_end5-lua_getstack
                                        # -- End function
	.hidden	luaG_findlocal                  # -- Begin function luaG_findlocal
	.globl	luaG_findlocal
	.p2align	2
	.type	luaG_findlocal,@function
luaG_findlocal:                         # @luaG_findlocal
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
	ldw r11, r4+0
	ldbu r1, r4+34
	andi r7, r1, 2
	addi r12, r0, 0
	add r1, r12, r0
	bne r7, r12, .LBB6_3
.LBB6_1:
	ldw r1, r11+0
	ldw r1, r1+12
	addi r7, r0, -1
	ble r5, r7, .LBB6_10
.LBB6_2:
	ldw r7, r4+16
	ldw r8, r1+52
	sub r7, r7, r8
	srai r7, r7, 2
	addi r7, r7, -1
	add r13, r3, r0
	add r3, r1, r0
	add r14, r4, r0
	add r4, r5, r0
	add r15, r5, r0
	add r5, r7, r0
	add r16, r6, r0
	jal r31, luaF_getlocalname
	add r3, r13, r0
	add r4, r14, r0
	add r5, r15, r0
	add r6, r16, r0
.LBB6_3:
	beq r1, r12, .LBB6_8
.LBB6_4:
	addi r3, r0, 0
	beq r6, r3, .LBB6_7
.LBB6_5:
	addi r3, r0, 12
	mul r3, r5, r3
	add r3, r11, r3
.LBB6_6:
	stw r6+0, r3
.LBB6_7:
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
.LBB6_8:
	ldw r1, r3+20
	beq r4, r1, .LBB6_13
.LBB6_9:
	ldw r3, r4+12
	jal r0, .LBB6_14
.LBB6_10:
	ldbu r3, r1+7
	addi r1, r0, 0
	beq r3, r1, .LBB6_7
.LBB6_11:
	ldw r3, r4+24
	addi r1, r0, 0
	sub r3, r1, r3
	blt r5, r3, .LBB6_7
.LBB6_12:
	addi r1, r0, 12
	mul r3, r3, r1
	add r3, r11, r3
	xor r4, r5, r7
	mul r1, r4, r1
	add r3, r3, r1
	lui r1, %hi(.L.str.9)
	addi r1, r1, %lo(.L.str.9)
	jal r0, .LBB6_6
.LBB6_13:
	addi r3, r3, 12
.LBB6_14:
	addi r1, r0, 0
	addi r7, r0, 1
	blt r5, r7, .LBB6_7
.LBB6_15:
	addi r7, r11, 12
	ldw r3, r3+0
	sub r3, r3, r7
	srai r3, r3, 2
	lui r7, 699051
	addi r7, r7, -1365
	mul r3, r3, r7
	blt r3, r5, .LBB6_7
.LBB6_16:
	ldbu r1, r4+34
	andi r1, r1, 2
	addi r3, r0, 0
	seq r1, r1, r3
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	lui r7, %hi(.L.str)
	addi r7, r7, %lo(.L.str)
	xor r7, r7, r4
	sub r1, r3, r1
	and r1, r7, r1
	xor r1, r1, r4
	addi r3, r0, 0
	bne r6, r3, .LBB6_5
	jal r0, .LBB6_7
.Lfunc_end6:
	.size	luaG_findlocal, .Lfunc_end6-luaG_findlocal
                                        # -- End function
	.globl	lua_getlocal                    # -- Begin function lua_getlocal
	.p2align	2
	.type	lua_getlocal,@function
lua_getlocal:                           # @lua_getlocal
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
	addi r13, r0, 0
	beq r4, r13, .LBB7_7
.LBB7_1:
	ldw r12, r4+104
	ldw r11, r12+0
	ldbu r1, r12+34
	andi r4, r1, 2
	add r1, r13, r0
	bne r4, r13, .LBB7_4
.LBB7_2:
	ldw r1, r11+0
	ldw r1, r1+12
	addi r4, r0, -1
	ble r5, r4, .LBB7_10
.LBB7_3:
	ldw r4, r12+16
	ldw r6, r1+52
	sub r4, r4, r6
	srai r4, r4, 2
	addi r6, r4, -1
	add r14, r3, r0
	add r3, r1, r0
	add r4, r5, r0
	add r15, r5, r0
	add r5, r6, r0
	jal r31, luaF_getlocalname
	add r5, r15, r0
	add r3, r14, r0
.LBB7_4:
	bne r1, r13, .LBB7_17
.LBB7_5:
	ldw r1, r3+20
	beq r12, r1, .LBB7_13
.LBB7_6:
	ldw r1, r12+12
	jal r0, .LBB7_14
.LBB7_7:
	ldw r1, r3+12
	ldbu r3, r1+-4
	addi r4, r0, 70
	bne r3, r4, .LBB7_9
.LBB7_8:
	ldw r1, r1+-12
	ldw r3, r1+12
	addi r1, r0, 0
	add r4, r5, r0
	add r5, r1, r0
	jal r31, luaF_getlocalname
	jal r0, .LBB7_20
.LBB7_9:
	addi r4, r0, 0
	jal r0, .LBB7_21
.LBB7_10:
	ldbu r7, r1+7
	addi r6, r0, 0
	add r1, r6, r0
	beq r7, r6, .LBB7_18
.LBB7_11:
	ldw r1, r12+24
	addi r6, r0, 0
	sub r7, r6, r1
	add r1, r6, r0
	blt r5, r7, .LBB7_18
.LBB7_12:
	addi r1, r0, 12
	mul r6, r7, r1
	add r6, r11, r6
	xor r4, r5, r4
	mul r1, r4, r1
	add r6, r6, r1
	lui r1, %hi(.L.str.9)
	addi r1, r1, %lo(.L.str.9)
	addi r4, r0, 0
	bne r1, r4, .LBB7_19
	jal r0, .LBB7_21
.LBB7_13:
	addi r1, r3, 12
.LBB7_14:
	addi r6, r0, 0
	addi r4, r0, 1
	blt r5, r4, .LBB7_22
.LBB7_15:
	addi r4, r11, 12
	ldw r1, r1+0
	sub r1, r1, r4
	srai r1, r1, 2
	lui r4, 699051
	addi r4, r4, -1365
	mul r4, r1, r4
	add r1, r6, r0
	blt r4, r5, .LBB7_18
.LBB7_16:
	ldbu r1, r12+34
	andi r1, r1, 2
	addi r4, r0, 0
	seq r1, r1, r4
	lui r6, %hi(.L.str.1)
	addi r6, r6, %lo(.L.str.1)
	lui r7, %hi(.L.str)
	addi r7, r7, %lo(.L.str)
	xor r7, r7, r6
	sub r1, r4, r1
	and r1, r7, r1
	xor r1, r1, r6
.LBB7_17:
	addi r4, r0, 12
	mul r4, r5, r4
	add r6, r11, r4
.LBB7_18:
	addi r4, r0, 0
	beq r1, r4, .LBB7_21
.LBB7_19:
	ldw r4, r3+12
	ldw r5, r6+0
	ldw r7, r6+4
	stw r4+4, r7
	stw r4+0, r5
	ldbu r5, r6+8
	stb r4+8, r5
	ldw r4, r3+12
	addi r4, r4, 12
	stw r3+12, r4
.LBB7_20:
	add r4, r1, r0
.LBB7_21:
	add r1, r4, r0
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
.LBB7_22:
	add r1, r6, r0
	addi r4, r0, 0
	bne r1, r4, .LBB7_19
	jal r0, .LBB7_21
.Lfunc_end7:
	.size	lua_getlocal, .Lfunc_end7-lua_getlocal
                                        # -- End function
	.globl	lua_setlocal                    # -- Begin function lua_setlocal
	.p2align	2
	.type	lua_setlocal,@function
lua_setlocal:                           # @lua_setlocal
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
	ldw r12, r4+104
	ldw r11, r12+0
	ldbu r1, r12+34
	andi r4, r1, 2
	addi r13, r0, 0
	add r1, r13, r0
	bne r4, r13, .LBB8_3
.LBB8_1:
	ldw r1, r11+0
	ldw r1, r1+12
	addi r6, r0, -1
	ble r5, r6, .LBB8_6
.LBB8_2:
	ldw r4, r12+16
	ldw r6, r1+52
	sub r4, r4, r6
	srai r4, r4, 2
	addi r6, r4, -1
	add r14, r3, r0
	add r3, r1, r0
	add r4, r5, r0
	add r15, r5, r0
	add r5, r6, r0
	jal r31, luaF_getlocalname
	add r5, r15, r0
	add r3, r14, r0
.LBB8_3:
	bne r1, r13, .LBB8_13
.LBB8_4:
	ldw r1, r3+20
	beq r12, r1, .LBB8_9
.LBB8_5:
	ldw r1, r12+12
	jal r0, .LBB8_10
.LBB8_6:
	ldbu r7, r1+7
	addi r4, r0, 0
	add r1, r4, r0
	beq r7, r4, .LBB8_14
.LBB8_7:
	ldw r1, r12+24
	addi r4, r0, 0
	sub r7, r4, r1
	add r1, r4, r0
	blt r5, r7, .LBB8_14
.LBB8_8:
	addi r1, r0, 12
	mul r4, r7, r1
	add r4, r11, r4
	xor r5, r5, r6
	mul r1, r5, r1
	add r4, r4, r1
	lui r1, %hi(.L.str.9)
	addi r1, r1, %lo(.L.str.9)
	addi r5, r0, 0
	bne r1, r5, .LBB8_15
	jal r0, .LBB8_16
.LBB8_9:
	addi r1, r3, 12
.LBB8_10:
	addi r4, r0, 0
	addi r6, r0, 1
	blt r5, r6, .LBB8_17
.LBB8_11:
	addi r6, r11, 12
	ldw r1, r1+0
	sub r1, r1, r6
	srai r1, r1, 2
	lui r6, 699051
	addi r6, r6, -1365
	mul r6, r1, r6
	add r1, r4, r0
	blt r6, r5, .LBB8_14
.LBB8_12:
	ldbu r1, r12+34
	andi r1, r1, 2
	addi r4, r0, 0
	seq r1, r1, r4
	lui r6, %hi(.L.str.1)
	addi r6, r6, %lo(.L.str.1)
	lui r7, %hi(.L.str)
	addi r7, r7, %lo(.L.str)
	xor r7, r7, r6
	sub r1, r4, r1
	and r1, r7, r1
	xor r1, r1, r6
.LBB8_13:
	addi r4, r0, 12
	mul r4, r5, r4
	add r4, r11, r4
.LBB8_14:
	addi r5, r0, 0
	beq r1, r5, .LBB8_16
.LBB8_15:
	ldw r5, r3+12
	ldw r6, r5+-12
	ldw r7, r5+-8
	stw r4+4, r7
	stw r4+0, r6
	ldbu r5, r5+-4
	stb r4+8, r5
	ldw r4, r3+12
	addi r4, r4, -12
	stw r3+12, r4
.LBB8_16:
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
.LBB8_17:
	add r1, r4, r0
	addi r5, r0, 0
	bne r1, r5, .LBB8_15
	jal r0, .LBB8_16
.Lfunc_end8:
	.size	lua_setlocal, .Lfunc_end8-lua_setlocal
                                        # -- End function
	.globl	lua_getinfo                     # -- Begin function lua_getinfo
	.p2align	2
	.type	lua_getinfo,@function
lua_getinfo:                            # @lua_getinfo
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
	add r14, r5, r0
	ldbu r1, r4+0
	addi r5, r0, 62
	bne r1, r5, .LBB9_2
.LBB9_1:
	ldw r1, r3+12
	addi r5, r1, -12
	addi r4, r4, 1
	stw fp+-108, r4
	stw r3+12, r5
	addi r20, r0, 0
	jal r0, .LBB9_3
.LBB9_2:
	stw fp+-108, r4
	ldw r20, r14+104
	ldw r5, r20+0
.LBB9_3:
	stw fp+-92, r3
	ldbu r1, r5+8
	addi r21, r0, 0
	ori  r1, r1, 32
	addi r3, r0, 102
	add r17, r21, r0
	bne r1, r3, .LBB9_5
.LBB9_4:
	ldw r17, r5+0
.LBB9_5:
	stw fp+-112, r5
	addi r15, r14, 4
	addi r16, r14, 44
	addi r12, r0, 1
	addi r22, r0, 41
	lui r23, %hi(.LJTI9_0)
	addi r23, r23, %lo(.LJTI9_0)
	lui r24, %hi(.L.str.11)
	addi r24, r24, %lo(.L.str.11)
	addi r25, r0, 4
	addi r26, r0, -1
	lui r27, %hi(.L.str.12)
	addi r27, r27, %lo(.L.str.12)
	addi r19, r0, 6
	lui r1, %hi(.L.str.13)
	addi r1, r1, %lo(.L.str.13)
	stw fp+-100, r1
	addi r1, r0, 2
	stw fp+-104, r1
	lui r13, %hi(.L.str.15)
	addi r13, r13, %lo(.L.str.15)
	lui r1, %hi(.L.str.14)
	addi r1, r1, %lo(.L.str.14)
	xor r28, r1, r13
	addi r1, r0, 255
	stw fp+-96, r1
	ldw r18, fp+-108
	jal r0, .LBB9_8
.LBB9_6:
	ldhu r1, r20+28
	sth r14+40, r1
	ldhu r1, r20+30
	sth r14+42, r1
.LBB9_7:
	addi r18, r18, 1
.LBB9_8:
	ldbu r1, r18+0
	addi r3, r1, -76
	bgtu r3, r22, .LBB9_40
.LBB9_9:
	slli r1, r3, 2
	add r1, r23, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB9_10:
	beq r17, r21, .LBB9_39
.LBB9_11:
	ldbu r1, r17+4
	bne r1, r19, .LBB9_39
.LBB9_12:
	ldw r1, r17+12
	ldw r3, r1+76
	beq r3, r21, .LBB9_48
.LBB9_13:
	addi r4, r3, 16
	stw r14+16, r4
	ldbu r4, r3+7
	ldw r5, fp+-96
	bne r4, r5, .LBB9_49
.LBB9_14:
	ldw r4, r3+12
	jal r0, .LBB9_49
.LBB9_15:
	addi r1, r0, 0
	beq r20, r1, .LBB9_17
.LBB9_16:
	ldbu r3, r20+35
	andi r3, r3, 1
	bne r3, r1, .LBB9_6
.LBB9_17:
	stw r14+40, r1
	addi r18, r18, 1
	jal r0, .LBB9_8
.LBB9_18:
	addi r1, r0, 0
	beq r17, r1, .LBB9_42
.LBB9_19:
	ldbu r3, r17+6
	stb r14+36, r3
	ldbu r3, r17+4
	bne r3, r19, .LBB9_43
.LBB9_20:
	ldw r1, r17+12
	ldbu r1, r1+7
	stb r14+38, r1
	ldw r1, r17+12
	ldbu r1, r1+6
	stb r14+37, r1
	addi r18, r18, 1
	jal r0, .LBB9_8
.LBB9_21:
	add r5, r26, r0
	beq r20, r21, .LBB9_47
.LBB9_22:
	ldbu r1, r20+34
	andi r1, r1, 2
	add r5, r26, r0
	bne r1, r21, .LBB9_47
.LBB9_23:
	ldw r1, r20+0
	ldw r1, r1+0
	ldw r6, r1+12
	ldw r1, r6+64
	add r5, r26, r0
	beq r1, r21, .LBB9_47
.LBB9_24:
	ldw r3, r20+16
	ldw r4, r6+52
	sub r3, r3, r4
	srai r3, r3, 2
	addi r4, r3, -1
	ldw r7, r6+36
	beq r7, r21, .LBB9_44
.LBB9_25:
	ldw r5, r6+68
	ldw r8, r5+0
	ble r3, r8, .LBB9_44
.LBB9_26:
	srli r6, r4, 7
	xor r8, r7, r6
	sgt r7, r7, r6
	addi r9, r0, 0
	sub r7, r9, r7
	and r7, r8, r7
	xor r8, r6, r7
	addi r7, r8, -1
	slli r9, r6, 3
	add r9, r5, r9
.LBB9_27:
	beq r8, r6, .LBB9_30
.LBB9_28:
	ldw r10, r9+0
	addi r6, r6, 1
	addi r9, r9, 8
	bgt r3, r10, .LBB9_27
.LBB9_29:
	addi r7, r6, -2
.LBB9_30:
	slli r6, r7, 3
	add r5, r5, r6
	ldw r6, r5+0
	addi r5, r5, 4
	ldw r5, r5+0
	blt r6, r4, .LBB9_45
	jal r0, .LBB9_47
.LBB9_31:
	addi r11, r0, 0
	add r1, r11, r0
	beq r20, r11, .LBB9_34
.LBB9_32:
	ldbu r1, r20+34
	andi r3, r1, 32
	addi r1, r0, 0
	bne r3, r1, .LBB9_34
.LBB9_33:
	ldw r4, r20+8
	ldw r3, fp+-92
	add r5, r15, r0
	jal r31, funcnamefromcall
.LBB9_34:
	stw r14+8, r1
	bne r1, r11, .LBB9_7
.LBB9_35:
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
	stw r14+8, r1
	addi r1, r0, 0
	stw r14+4, r1
	addi r18, r18, 1
	jal r0, .LBB9_8
.LBB9_36:
	addi r1, r0, 0
	beq r20, r1, .LBB9_38
.LBB9_37:
	ldhu r1, r20+34
	andi r1, r1, 32
.LBB9_38:
	stb r14+39, r1
	addi r18, r18, 1
	jal r0, .LBB9_8
.LBB9_39:
	stw r14+16, r24
	stw r14+20, r25
	stw r14+28, r26
	stw r14+32, r26
	add r1, r27, r0
	jal r0, .LBB9_50
.LBB9_40:
	beq r1, r21, .LBB9_51
.LBB9_41:
	add r12, r21, r0
	addi r18, r18, 1
	jal r0, .LBB9_8
.LBB9_42:
	stb r14+36, r1
.LBB9_43:
	addi r3, r0, 1
	stb r14+38, r3
	stb r14+37, r1
	addi r18, r18, 1
	jal r0, .LBB9_8
.LBB9_44:
	addi r5, r6, 40
	add r6, r26, r0
	ldw r5, r5+0
	bge r6, r4, .LBB9_47
.LBB9_45:
	xor r4, r6, r26
	add r3, r4, r3
	add r1, r1, r6
	addi r1, r1, 1
.LBB9_46:
	ldb r4, r1+0
	add r5, r5, r4
	addi r3, r3, -1
	addi r1, r1, 1
	bne r3, r21, .LBB9_46
.LBB9_47:
	stw r14+24, r5
	addi r18, r18, 1
	jal r0, .LBB9_8
.LBB9_48:
	ldw r3, fp+-100
	stw r14+16, r3
	ldw r4, fp+-104
.LBB9_49:
	stw r14+20, r4
	ldw r3, r1+40
	stw r14+28, r3
	ldw r1, r1+44
	stw r14+32, r1
	seq r1, r3, r21
	sub r1, r21, r1
	and r1, r28, r1
	xor r1, r1, r13
.LBB9_50:
	stw r14+12, r1
	ldw r4, r14+16
	ldw r5, r14+20
	add r3, r16, r0
	jal r31, luaO_chunkid
	addi r18, r18, 1
	jal r0, .LBB9_8
.LBB9_51:
	addi r4, r0, 102
	ldw r13, fp+-108
	add r3, r13, r0
	jal r31, strchr
	addi r18, r0, 0
	ldw r11, fp+-92
	beq r1, r18, .LBB9_53
.LBB9_52:
	ldw r1, r11+12
	ldw r5, fp+-112
	ldw r3, r5+0
	ldw r4, r5+4
	stw r1+4, r4
	stw r1+0, r3
	ldbu r3, r5+8
	stb r1+8, r3
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
.LBB9_53:
	addi r4, r0, 76
	add r3, r13, r0
	jal r31, strchr
	beq r1, r18, .LBB9_69
.LBB9_54:
	beq r17, r18, .LBB9_68
.LBB9_55:
	ldbu r1, r17+4
	bne r1, r19, .LBB9_68
.LBB9_56:
	ldw r16, r17+12
	ldw r14, r16+40
	add r3, r11, r0
	jal r31, luaH_new
	add r13, r1, r0
	ldw r1, r11+12
	stw r1+0, r13
	addi r3, r0, 69
	stb r1+8, r3
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
	ldw r1, r16+64
	beq r1, r18, .LBB9_69
.LBB9_57:
	addi r17, r16, 40
	addi r3, r0, 17
	addi r4, fp, -88
	stb r4+8, r3
	ldbu r3, r16+7
	beq r3, r18, .LBB9_73
.LBB9_58:
	ldb r3, r1+0
	addi r4, r0, -128
	bne r3, r4, .LBB9_70
.LBB9_59:
	ldw r7, r16+36
	addi r3, r0, -1
	addi r5, r0, 0
	add r6, r3, r0
	add r8, r17, r0
	beq r7, r5, .LBB9_66
.LBB9_60:
	ldw r4, r16+68
	ldw r9, r4+0
	add r6, r3, r0
	add r8, r17, r0
	bgt r9, r5, .LBB9_66
.LBB9_61:
	sgt r6, r7, r5
	sub r6, r5, r6
	and r7, r7, r6
	addi r6, r7, -1
	addi r8, r0, 1
	add r9, r4, r0
.LBB9_62:
	beq r7, r5, .LBB9_65
.LBB9_63:
	ldw r10, r9+0
	addi r5, r5, 1
	addi r9, r9, 8
	blt r10, r8, .LBB9_62
.LBB9_64:
	addi r6, r5, -2
.LBB9_65:
	slli r5, r6, 3
	add r4, r4, r5
	ldw r6, r4+0
	addi r8, r4, 4
.LBB9_66:
	ldw r14, r8+0
	ble r6, r3, .LBB9_71
.LBB9_67:
	addi r18, r0, 1
	jal r0, .LBB9_73
.LBB9_68:
	ldw r1, r11+12
	addi r3, r0, 0
	stb r1+8, r3
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
.LBB9_69:
	add r1, r12, r0
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
.LBB9_70:
	add r14, r14, r3
	addi r18, r0, 1
	jal r0, .LBB9_73
.LBB9_71:
	addi r1, r1, 1
	addi r18, r0, 1
.LBB9_72:
	addi r3, r6, 1
	sltu r4, r3, r6
	add r5, r1, r6
	ldb r5, r5+0
	add r14, r14, r5
	add r6, r3, r0
	bne r4, r18, .LBB9_72
.LBB9_73:
	ldw r1, r16+24
	bge r18, r1, .LBB9_69
.LBB9_74:
	addi r19, r0, -128
	addi r20, r0, -1
	addi r21, r0, 0
	addi r15, fp, -88
	jal r0, .LBB9_77
.LBB9_75:
	add r14, r14, r3
.LBB9_76:
	add r3, r11, r0
	add r4, r13, r0
	add r5, r14, r0
	add r6, r15, r0
	jal r31, luaH_setint
	addi r18, r18, 1
	ldw r1, r16+24
	bge r18, r1, .LBB9_69
.LBB9_77:
	ldw r1, r16+64
	add r3, r1, r18
	ldb r3, r3+0
	bne r3, r19, .LBB9_75
.LBB9_78:
	add r14, r20, r0
	beq r1, r21, .LBB9_76
.LBB9_79:
	ldw r5, r16+36
	add r4, r20, r0
	add r6, r17, r0
	beq r5, r21, .LBB9_86
.LBB9_80:
	ldw r3, r16+68
	ldw r7, r3+0
	add r4, r20, r0
	add r6, r17, r0
	blt r18, r7, .LBB9_86
.LBB9_81:
	srli r4, r18, 7
	slli r7, r4, 3
	xor r6, r5, r4
	sgt r5, r5, r4
	sub r5, r21, r5
	and r5, r6, r5
	xor r6, r4, r5
	addi r5, r6, -1
	add r7, r3, r7
.LBB9_82:
	beq r6, r4, .LBB9_85
.LBB9_83:
	ldw r8, r7+0
	addi r4, r4, 1
	addi r7, r7, 8
	bge r18, r8, .LBB9_82
.LBB9_84:
	addi r5, r4, -2
.LBB9_85:
	slli r4, r5, 3
	add r3, r3, r4
	ldw r4, r3+0
	addi r6, r3, 4
.LBB9_86:
	ldw r14, r6+0
	bge r4, r18, .LBB9_76
.LBB9_87:
	addi r1, r1, 1
.LBB9_88:
	addi r3, r4, 1
	add r4, r1, r4
	ldb r4, r4+0
	add r14, r14, r4
	add r4, r3, r0
	bne r18, r3, .LBB9_88
	jal r0, .LBB9_76
.Lfunc_end9:
	.size	lua_getinfo, .Lfunc_end9-lua_getinfo
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI9_0:
	.word	.LBB9_7
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_10
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_7
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_21
	.word	.LBB9_41
	.word	.LBB9_31
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_41
	.word	.LBB9_15
	.word	.LBB9_41
	.word	.LBB9_36
	.word	.LBB9_18
                                        # -- End function
	.text
	.hidden	luaG_typeerror                  # -- Begin function luaG_typeerror
	.globl	luaG_typeerror
	.p2align	2
	.type	luaG_typeerror,@function
luaG_typeerror:                         # @luaG_typeerror
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
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	jal r31, varinfo
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	add r6, r1, r0
	jal r31, typeerror
.Lfunc_end10:
	.size	luaG_typeerror, .Lfunc_end10-luaG_typeerror
                                        # -- End function
	.p2align	2                               # -- Begin function typeerror
	.type	typeerror,@function
typeerror:                              # @typeerror
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
	add r11, r6, r0
	add r12, r5, r0
	add r13, r3, r0
	jal r31, luaT_objtypename
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	add r3, r13, r0
	add r5, r12, r0
	add r6, r1, r0
	add r7, r11, r0
	jal r31, luaG_runerror
.Lfunc_end11:
	.size	typeerror, .Lfunc_end11-typeerror
                                        # -- End function
	.p2align	2                               # -- Begin function varinfo
	.type	varinfo,@function
varinfo:                                # @varinfo
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
	ldw r6, r3+20
	addi r12, r0, 0
	addi r11, fp, -28
	stw r11+0, r12
	ldbu r1, r6+34
	andi r1, r1, 2
	add r5, r12, r0
	bne r1, r12, .LBB12_14
.LBB12_1:
	ldw r1, r6+0
	ldw r7, r1+0
	ldbu r5, r7+6
	addi r8, r0, 0
	addi r9, r0, 1
	beq r5, r8, .LBB12_5
.LBB12_2:
	addi r10, r7, 16
	slli r14, r5, 3
	addi r5, r0, 0
	add r13, r5, r0
.LBB12_3:
	ldw r15, r10+0
	ldw r15, r15+8
	beq r15, r4, .LBB12_7
.LBB12_4:
	addi r13, r13, 8
	addi r10, r10, 4
	bne r14, r13, .LBB12_3
	jal r0, .LBB12_6
.LBB12_5:
	add r5, r8, r0
.LBB12_6:
	bne r9, r8, .LBB12_8
	jal r0, .LBB12_14
.LBB12_7:
	ldw r5, r7+12
	ldw r5, r5+60
	add r5, r5, r13
	ldw r5, r5+0
	addi r9, r0, 0
	seq r10, r5, r9
	addi r5, r5, 16
	sub r10, r9, r10
	lui r13, %hi(.L.str.18)
	addi r13, r13, %lo(.L.str.18)
	xor r13, r5, r13
	and r10, r13, r10
	xor r5, r5, r10
	stw r11+0, r5
	lui r5, %hi(.L.str.17)
	addi r5, r5, %lo(.L.str.17)
	beq r9, r8, .LBB12_14
.LBB12_8:
	addi r5, r1, 12
	ldw r8, r6+4
	addi r1, r0, -1
	bgeu r5, r8, .LBB12_12
.LBB12_9:
	addi r9, r0, 0
.LBB12_10:
	beq r5, r4, .LBB12_18
.LBB12_11:
	addi r9, r9, 1
	addi r5, r5, 12
	bltu r5, r8, .LBB12_10
.LBB12_12:
	addi r5, r0, 0
	blt r1, r5, .LBB12_14
.LBB12_13:
	ldw r5, r7+12
	ldw r4, r6+16
	ldw r6, r5+52
	sub r4, r4, r6
	srai r4, r4, 2
	addi r4, r4, -1
	addi r6, fp, -28
	add r13, r3, r0
	add r3, r5, r0
	add r5, r1, r0
	jal r31, getobjname
	add r3, r13, r0
	add r5, r1, r0
.LBB12_14:
	beq r5, r12, .LBB12_16
.LBB12_15:
	ldw r6, r11+0
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	jal r31, luaO_pushfstring
	jal r0, .LBB12_17
.LBB12_16:
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
.LBB12_17:
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
.LBB12_18:
	add r1, r9, r0
	addi r5, r0, 0
	bge r1, r5, .LBB12_13
	jal r0, .LBB12_14
.Lfunc_end12:
	.size	varinfo, .Lfunc_end12-varinfo
                                        # -- End function
	.hidden	luaG_callerror                  # -- Begin function luaG_callerror
	.globl	luaG_callerror
	.p2align	2
	.type	luaG_callerror,@function
luaG_callerror:                         # @luaG_callerror
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
	ldw r4, r3+20
	addi r14, r0, 0
	addi r13, fp, -24
	stw r13+0, r14
	add r5, r13, r0
	jal r31, funcnamefromcall
	bne r1, r14, .LBB13_2
.LBB13_1:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, varinfo
	jal r0, .LBB13_3
.LBB13_2:
	ldw r6, r13+0
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	add r3, r12, r0
	add r5, r1, r0
	jal r31, luaO_pushfstring
.LBB13_3:
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	add r3, r12, r0
	add r4, r11, r0
	add r6, r1, r0
	jal r31, typeerror
.Lfunc_end13:
	.size	luaG_callerror, .Lfunc_end13-luaG_callerror
                                        # -- End function
	.p2align	2                               # -- Begin function funcnamefromcall
	.type	funcnamefromcall,@function
funcnamefromcall:                       # @funcnamefromcall
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	add r6, r5, r0
	ldhu r5, r4+34
	andi r7, r5, 8
	addi r1, r0, 0
	bne r7, r1, .LBB14_6
.LBB14_1:
	andi r7, r5, 128
	bne r7, r1, .LBB14_7
.LBB14_2:
	andi r5, r5, 2
	bne r5, r1, .LBB14_21
.LBB14_3:
	ldw r1, r4+0
	ldw r1, r1+0
	ldw r7, r1+12
	ldw r1, r4+16
	ldw r5, r7+52
	sub r1, r1, r5
	srai r1, r1, 2
	addi r4, r1, -1
	slli r1, r4, 2
	add r1, r5, r1
	ldw r5, r1+0
	andi r8, r5, 127
	addi r1, r0, 0
	addi r8, r8, -11
	addi r9, r0, 65
	bgtu r8, r9, .LBB14_21
.LBB14_4:
	slli r8, r8, 2
	lui r9, %hi(.LJTI14_0)
	addi r9, r9, %lo(.LJTI14_0)
	add r8, r9, r8
	ldw r8, r8+0
	jalr r0, r8, 0
.LBB14_5:
	addi r1, r0, 1
	jal r0, .LBB14_19
.LBB14_6:
	lui r1, %hi(.L.str.18)
	addi r1, r1, %lo(.L.str.18)
	stw r6+0, r1
	lui r1, %hi(.L.str.26)
	addi r1, r1, %lo(.L.str.26)
	jal r0, .LBB14_21
.LBB14_7:
	lui r1, %hi(.L.str.27)
	addi r1, r1, %lo(.L.str.27)
	jal r0, .LBB14_20
.LBB14_8:
	addi r1, r0, 21
	jal r0, .LBB14_19
.LBB14_9:
	srli r1, r5, 24
	jal r0, .LBB14_19
.LBB14_10:
	addi r1, r0, 20
	jal r0, .LBB14_19
.LBB14_11:
	addi r1, r0, 24
	jal r0, .LBB14_19
.LBB14_12:
	srli r1, r5, 7
	andi r5, r1, 255
	add r3, r7, r0
	jal r31, getobjname
	jal r0, .LBB14_21
.LBB14_13:
	addi r1, r0, 19
	jal r0, .LBB14_19
.LBB14_14:
	addi r1, r0, 18
	jal r0, .LBB14_19
.LBB14_15:
	lui r1, %hi(.L.str.29)
	addi r1, r1, %lo(.L.str.29)
	stw r6+0, r1
	jal r0, .LBB14_21
.LBB14_16:
	addi r1, r0, 5
	jal r0, .LBB14_19
.LBB14_17:
	addi r1, r0, 4
	jal r0, .LBB14_19
.LBB14_18:
	addi r1, r0, 22
.LBB14_19:
	ldw r3, r3+16
	slli r1, r1, 2
	add r1, r3, r1
	ldw r1, r1+160
	addi r1, r1, 18
.LBB14_20:
	stw r6+0, r1
	lui r1, %hi(.L.str.28)
	addi r1, r1, %lo(.L.str.28)
.LBB14_21:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end14:
	.size	funcnamefromcall, .Lfunc_end14-funcnamefromcall
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI14_0:
	.word	.LBB14_19
	.word	.LBB14_19
	.word	.LBB14_19
	.word	.LBB14_19
	.word	.LBB14_5
	.word	.LBB14_5
	.word	.LBB14_5
	.word	.LBB14_5
	.word	.LBB14_21
	.word	.LBB14_19
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_9
	.word	.LBB14_9
	.word	.LBB14_9
	.word	.LBB14_14
	.word	.LBB14_13
	.word	.LBB14_21
	.word	.LBB14_17
	.word	.LBB14_18
	.word	.LBB14_11
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_16
	.word	.LBB14_10
	.word	.LBB14_8
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_10
	.word	.LBB14_8
	.word	.LBB14_10
	.word	.LBB14_8
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_12
	.word	.LBB14_12
	.word	.LBB14_11
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_21
	.word	.LBB14_15
                                        # -- End function
	.text
	.hidden	luaG_forerror                   # -- Begin function luaG_forerror
	.globl	luaG_forerror
	.p2align	2
	.type	luaG_forerror,@function
luaG_forerror:                          # @luaG_forerror
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
	jal r31, luaT_objtypename
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	add r3, r12, r0
	add r5, r11, r0
	add r6, r1, r0
	jal r31, luaG_runerror
.Lfunc_end15:
	.size	luaG_forerror, .Lfunc_end15-luaG_forerror
                                        # -- End function
	.hidden	luaG_runerror                   # -- Begin function luaG_runerror
	.globl	luaG_runerror
	.p2align	2
	.type	luaG_runerror,@function
luaG_runerror:                          # @luaG_runerror
# %bb.0:
	addi sp, sp, -120
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 120
	stw fp+-28, r11
	stw fp+-32, r12
	stw fp+-36, r13
	stw fp+-40, r14
	stw fp+-44, lr
	add r11, r3, r0
	addi r12, fp, -24
	stw r12+20, r10
	stw r12+16, r9
	stw r12+12, r8
	stw r12+8, r7
	addi r1, r12, 4
	stw r1+0, r6
	stw r12+0, r5
	ldw r13, r3+20
	ldw r1, r3+16
	ldw r1, r1+12
	addi r3, r0, 1
	blt r1, r3, .LBB16_2
.LBB16_1:
	add r3, r11, r0
	add r14, r4, r0
	jal r31, luaC_step
	add r4, r14, r0
.LBB16_2:
	addi r1, fp, -108
	stw r1+0, r12
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaO_pushvfstring
	ldbu r3, r13+34
	andi r3, r3, 2
	addi r4, r0, 0
	bne r3, r4, .LBB16_21
.LBB16_3:
	ldw r3, r13+0
	ldw r3, r3+0
	ldw r9, r3+12
	ldw r3, r9+76
	ldw r5, r9+64
	bne r5, r4, .LBB16_5
.LBB16_4:
	addi r12, r0, -1
	jal r0, .LBB16_15
.LBB16_5:
	ldw r6, r13+16
	ldw r7, r9+52
	sub r6, r6, r7
	srai r6, r6, 2
	addi r7, r6, -1
	ldw r10, r9+36
	beq r10, r4, .LBB16_12
.LBB16_6:
	ldw r8, r9+68
	ldw r12, r8+0
	ble r6, r12, .LBB16_12
.LBB16_7:
	srli r9, r7, 7
	xor r12, r10, r9
	sgt r10, r10, r9
	sub r10, r4, r10
	and r10, r12, r10
	xor r12, r9, r10
	addi r10, r12, -1
	slli r13, r9, 3
	add r13, r8, r13
.LBB16_8:
	beq r12, r9, .LBB16_11
.LBB16_9:
	ldw r14, r13+0
	addi r9, r9, 1
	addi r13, r13, 8
	bgt r6, r14, .LBB16_8
.LBB16_10:
	addi r10, r9, -2
.LBB16_11:
	slli r9, r10, 3
	add r9, r8, r9
	ldw r8, r9+0
	addi r9, r9, 4
	ldw r12, r9+0
	blt r8, r7, .LBB16_13
	jal r0, .LBB16_15
.LBB16_12:
	addi r9, r9, 40
	addi r8, r0, -1
	ldw r12, r9+0
	bge r8, r7, .LBB16_15
.LBB16_13:
	addi r7, r0, -1
	xor r7, r8, r7
	add r6, r7, r6
	add r5, r8, r5
	addi r5, r5, 1
.LBB16_14:
	ldb r7, r5+0
	add r12, r12, r7
	addi r6, r6, -1
	addi r5, r5, 1
	bne r6, r4, .LBB16_14
.LBB16_15:
	bne r3, r4, .LBB16_17
.LBB16_16:
	addi r3, r0, 63
	addi r4, fp, -104
	sth r4+0, r3
	jal r0, .LBB16_20
.LBB16_17:
	add r13, r1, r0
	addi r4, r3, 16
	ldbu r5, r3+7
	addi r1, r0, 255
	bne r5, r1, .LBB16_19
.LBB16_18:
	ldw r5, r3+12
.LBB16_19:
	addi r3, fp, -104
	jal r31, luaO_chunkid
	add r1, r13, r0
.LBB16_20:
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	addi r5, fp, -104
	add r3, r11, r0
	add r6, r12, r0
	add r7, r1, r0
	jal r31, luaO_pushfstring
	ldw r1, r11+12
	ldw r3, r1+-12
	ldw r4, r1+-8
	stw r1+-20, r4
	stw r1+-24, r3
	ldbu r3, r1+-4
	stb r1+-16, r3
	ldw r1, r11+12
	addi r1, r1, -12
	stw r11+12, r1
.LBB16_21:
	add r3, r11, r0
	jal r31, luaG_errormsg
.Lfunc_end16:
	.size	luaG_runerror, .Lfunc_end16-luaG_runerror
                                        # -- End function
	.hidden	luaG_concaterror                # -- Begin function luaG_concaterror
	.globl	luaG_concaterror
	.p2align	2
	.type	luaG_concaterror,@function
luaG_concaterror:                       # @luaG_concaterror
# %bb.0:
	addi sp, sp, -8
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 8
	ldbu r1, r4+8
	andi r1, r1, 15
	addi r1, r1, -3
	addi r6, r0, 2
	sltu r1, r1, r6
	xor r5, r5, r4
	addi r6, r0, 0
	sub r1, r6, r1
	and r1, r5, r1
	xor r4, r4, r1
	lui r5, %hi(.L.str.4)
	addi r5, r5, %lo(.L.str.4)
	jal r31, luaG_typeerror
.Lfunc_end17:
	.size	luaG_concaterror, .Lfunc_end17-luaG_concaterror
                                        # -- End function
	.hidden	luaG_opinterror                 # -- Begin function luaG_opinterror
	.globl	luaG_opinterror
	.p2align	2
	.type	luaG_opinterror,@function
luaG_opinterror:                        # @luaG_opinterror
# %bb.0:
	addi sp, sp, -8
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 8
	ldbu r1, r4+8
	andi r1, r1, 15
	addi r7, r0, 3
	seq r1, r1, r7
	xor r5, r5, r4
	addi r7, r0, 0
	sub r1, r7, r1
	and r1, r5, r1
	xor r4, r4, r1
	add r5, r6, r0
	jal r31, luaG_typeerror
.Lfunc_end18:
	.size	luaG_opinterror, .Lfunc_end18-luaG_opinterror
                                        # -- End function
	.hidden	luaG_tointerror                 # -- Begin function luaG_tointerror
	.globl	luaG_tointerror
	.p2align	2
	.type	luaG_tointerror,@function
luaG_tointerror:                        # @luaG_tointerror
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
	addi r4, fp, -24
	addi r14, r0, 0
	add r3, r12, r0
	add r5, r14, r0
	jal r31, luaV_tointegerns
	seq r1, r1, r14
	sub r1, r14, r1
	xor r3, r12, r11
	and r1, r3, r1
	xor r4, r11, r1
	add r3, r13, r0
	jal r31, varinfo
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	add r3, r13, r0
	add r5, r1, r0
	jal r31, luaG_runerror
.Lfunc_end19:
	.size	luaG_tointerror, .Lfunc_end19-luaG_tointerror
                                        # -- End function
	.hidden	luaG_ordererror                 # -- Begin function luaG_ordererror
	.globl	luaG_ordererror
	.p2align	2
	.type	luaG_ordererror,@function
luaG_ordererror:                        # @luaG_ordererror
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
	add r11, r3, r0
	jal r31, luaT_objtypename
	add r12, r1, r0
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaT_objtypename
	add r13, r1, r0
	add r3, r12, r0
	add r4, r1, r0
	jal r31, strcmp
	addi r3, r0, 0
	bne r1, r3, .LBB20_2
.LBB20_1:
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaG_runerror
.LBB20_2:
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	add r3, r11, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, luaG_runerror
.Lfunc_end20:
	.size	luaG_ordererror, .Lfunc_end20-luaG_ordererror
                                        # -- End function
	.hidden	luaG_addinfo                    # -- Begin function luaG_addinfo
	.globl	luaG_addinfo
	.p2align	2
	.type	luaG_addinfo,@function
luaG_addinfo:                           # @luaG_addinfo
# %bb.0:
	addi sp, sp, -88
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 88
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, lr
	add r11, r4, r0
	addi r1, r0, 0
	beq r5, r1, .LBB21_4
.LBB21_1:
	add r12, r3, r0
	add r13, r6, r0
	addi r4, r5, 16
	ldbu r1, r5+7
	addi r3, r0, 255
	bne r1, r3, .LBB21_3
.LBB21_2:
	ldw r1, r5+12
.LBB21_3:
	addi r3, fp, -76
	add r5, r1, r0
	jal r31, luaO_chunkid
	add r6, r13, r0
	add r3, r12, r0
	jal r0, .LBB21_5
.LBB21_4:
	addi r1, r0, 63
	addi r4, fp, -76
	sth r4+0, r1
.LBB21_5:
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	addi r5, fp, -76
	add r7, r11, r0
	jal r31, luaO_pushfstring
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 88
	jalr r0, r31, 0
.Lfunc_end21:
	.size	luaG_addinfo, .Lfunc_end21-luaG_addinfo
                                        # -- End function
	.hidden	luaG_errormsg                   # -- Begin function luaG_errormsg
	.globl	luaG_errormsg
	.p2align	2
	.type	luaG_errormsg,@function
luaG_errormsg:                          # @luaG_errormsg
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+92
	addi r4, r0, 0
	beq r1, r4, .LBB22_2
.LBB22_1:
	ldw r4, r3+28
	add r1, r4, r1
	ldw r4, r3+12
	ldw r5, r4+-12
	ldw r6, r4+-8
	stw r4+4, r6
	stw r4+0, r5
	ldbu r5, r4+-4
	stb r4+8, r5
	ldw r4, r3+12
	ldw r5, r1+0
	ldw r6, r1+4
	stw r4+-8, r6
	stw r4+-12, r5
	ldbu r1, r1+8
	stb r4+-4, r1
	ldw r1, r3+12
	addi r4, r1, 12
	stw r3+12, r4
	addi r4, r1, -12
	addi r5, r0, 1
	add r11, r3, r0
	jal r31, luaD_callnoyield
	add r3, r11, r0
.LBB22_2:
	addi r4, r0, 2
	jal r31, luaD_throw
.Lfunc_end22:
	.size	luaG_errormsg, .Lfunc_end22-luaG_errormsg
                                        # -- End function
	.hidden	luaG_tracecall                  # -- Begin function luaG_tracecall
	.globl	luaG_tracecall
	.p2align	2
	.type	luaG_tracecall,@function
luaG_tracecall:                         # @luaG_tracecall
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r4, r3+20
	ldw r1, r4+0
	ldw r1, r1+0
	ldw r1, r1+12
	addi r11, r0, 1
	stw r4+20, r11
	ldw r5, r4+16
	ldw r6, r1+52
	beq r5, r6, .LBB23_2
.LBB23_1:
	add r1, r11, r0
	jal r0, .LBB23_3
.LBB23_2:
	ldbu r5, r1+7
	addi r1, r0, 0
	beq r5, r1, .LBB23_4
.LBB23_3:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB23_4:
	ldbu r1, r4+34
	andi r1, r1, 64
	addi r5, r0, 0
	bne r1, r5, .LBB23_1
.LBB23_5:
	jal r31, luaD_hookcall
	jal r0, .LBB23_1
.Lfunc_end23:
	.size	luaG_tracecall, .Lfunc_end23-luaG_tracecall
                                        # -- End function
	.hidden	luaG_traceexec                  # -- Begin function luaG_traceexec
	.globl	luaG_traceexec
	.p2align	2
	.type	luaG_traceexec,@function
luaG_traceexec:                         # @luaG_traceexec
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
	ldw r12, r3+20
	ldw r17, r3+112
	andi r5, r17, 12
	addi r1, r0, 0
	beq r5, r1, .LBB24_5
.LBB24_1:
	ldw r5, r12+0
	ldw r5, r5+0
	ldw r16, r5+12
	addi r15, r4, 4
	stw r12+16, r15
	andi r4, r17, 8
	add r13, r1, r0
	beq r4, r1, .LBB24_3
.LBB24_2:
	ldw r4, r3+108
	addi r4, r4, -1
	stw r3+108, r4
	addi r5, r0, 0
	seq r13, r4, r5
.LBB24_3:
	beq r13, r1, .LBB24_6
.LBB24_4:
	ldw r1, r3+104
	stw r3+108, r1
	jal r0, .LBB24_7
.LBB24_5:
	addi r1, r0, 0
	stw r12+20, r1
	jal r0, .LBB24_68
.LBB24_6:
	andi r1, r17, 4
	addi r4, r0, 0
	beq r1, r4, .LBB24_13
.LBB24_7:
	ldhu r1, r12+34
	andi r4, r1, 64
	addi r14, r0, 0
	bne r4, r14, .LBB24_12
.LBB24_8:
	ldw r1, r12+16
	ldw r1, r1+-4
	lui r4, 4080
	and r4, r1, r4
	bne r4, r14, .LBB24_14
.LBB24_9:
	andi r1, r1, 127
	lui r4, %hi(luaP_opmodes)
	addi r4, r4, %lo(luaP_opmodes)
	add r1, r1, r4
	ldbu r1, r1+0
	andi r1, r1, 32
	addi r4, r0, 0
	beq r1, r4, .LBB24_14
.LBB24_10:
	bne r13, r14, .LBB24_15
.LBB24_11:
	andi r1, r17, 4
	bne r1, r14, .LBB24_16
	jal r0, .LBB24_67
.LBB24_12:
	lui r3, 16
	addi r3, r3, -65
	and r1, r1, r3
	sth r12+34, r1
.LBB24_13:
	addi r1, r0, 1
	jal r0, .LBB24_68
.LBB24_14:
	ldw r1, r12+4
	stw r3+12, r1
	beq r13, r14, .LBB24_11
.LBB24_15:
	addi r4, r0, 3
	addi r5, r0, -1
	addi r6, r0, 0
	add r11, r3, r0
	add r7, r6, r0
	jal r31, luaD_hook
	add r3, r11, r0
	andi r1, r17, 4
	beq r1, r14, .LBB24_67
.LBB24_16:
	ldw r1, r3+100
	ldw r4, r16+20
	slt r4, r1, r4
	sub r4, r14, r4
	and r6, r1, r4
	ldw r1, r16+52
	sub r1, r15, r1
	srai r1, r1, 2
	addi r15, r1, -1
	ble r15, r6, .LBB24_52
.LBB24_17:
	ldw r4, r16+64
	add r5, r14, r0
	beq r4, r14, .LBB24_51
.LBB24_18:
	sub r5, r15, r6
	addi r7, r0, 63
	bgt r5, r7, .LBB24_28
.LBB24_19:
	sub r5, r1, r6
	addi r7, r5, -2
	add r5, r6, r4
	addi r8, r5, 1
	addi r9, r0, 0
	addi r10, r0, 2
	addi r11, r0, -128
	addi r17, r0, 1
	add r18, r9, r0
                                        # implicit-def: $r5
	jal r0, .LBB24_22
.LBB24_20:
	add r19, r9, r0
	add r20, r10, r0
.LBB24_21:
	addi r7, r7, -1
	addi r8, r8, 1
	bne r20, r9, .LBB24_26
.LBB24_22:
	ldb r19, r8+0
	beq r19, r11, .LBB24_20
.LBB24_23:
	add r18, r18, r19
	beq r7, r9, .LBB24_25
.LBB24_24:
	add r19, r17, r0
	add r20, r9, r0
	jal r0, .LBB24_21
.LBB24_25:
	sne r5, r18, r9
	add r19, r9, r0
	add r20, r17, r0
	jal r0, .LBB24_21
.LBB24_26:
	addi r7, r0, 2
	beq r20, r7, .LBB24_28
.LBB24_27:
	addi r7, r0, 0
	beq r19, r7, .LBB24_51
.LBB24_28:
	ldw r8, r16+36
	addi r5, r0, 0
	beq r8, r5, .LBB24_30
.LBB24_29:
	ldw r7, r16+68
	ldw r9, r7+0
	bge r6, r9, .LBB24_31
.LBB24_30:
	addi r7, r16, 40
	addi r9, r0, -1
	jal r0, .LBB24_36
.LBB24_31:
	srli r9, r6, 7
	xor r10, r8, r9
	sgt r11, r8, r9
	sub r11, r5, r11
	and r10, r10, r11
	xor r11, r9, r10
	addi r10, r11, -1
	slli r17, r9, 3
	add r17, r7, r17
.LBB24_32:
	beq r11, r9, .LBB24_35
.LBB24_33:
	ldw r18, r17+0
	addi r9, r9, 1
	addi r17, r17, 8
	bge r6, r18, .LBB24_32
.LBB24_34:
	addi r10, r9, -2
.LBB24_35:
	slli r9, r10, 3
	add r7, r7, r9
	ldw r9, r7+0
	addi r7, r7, 4
.LBB24_36:
	ldw r7, r7+0
	ble r6, r9, .LBB24_39
.LBB24_37:
	sub r6, r6, r9
	add r9, r9, r4
	addi r9, r9, 1
.LBB24_38:
	ldb r10, r9+0
	add r7, r7, r10
	addi r6, r6, -1
	addi r9, r9, 1
	bne r6, r5, .LBB24_38
.LBB24_39:
	beq r8, r5, .LBB24_46
.LBB24_40:
	ldw r6, r16+68
	ldw r9, r6+0
	ble r1, r9, .LBB24_46
.LBB24_41:
	srli r9, r15, 7
	xor r10, r8, r9
	sgt r8, r8, r9
	sub r8, r5, r8
	and r8, r10, r8
	xor r10, r9, r8
	addi r8, r10, -1
	slli r11, r9, 3
	add r11, r6, r11
.LBB24_42:
	beq r10, r9, .LBB24_45
.LBB24_43:
	ldw r17, r11+0
	addi r9, r9, 1
	addi r11, r11, 8
	bgt r1, r17, .LBB24_42
.LBB24_44:
	addi r8, r9, -2
.LBB24_45:
	slli r8, r8, 3
	add r6, r6, r8
	ldw r9, r6+0
	addi r6, r6, 4
	jal r0, .LBB24_47
.LBB24_46:
	addi r6, r16, 40
	addi r9, r0, -1
.LBB24_47:
	ldw r6, r6+0
	bge r9, r15, .LBB24_50
.LBB24_48:
	addi r8, r0, -1
	xor r8, r9, r8
	add r8, r8, r1
	add r4, r9, r4
	addi r4, r4, 1
.LBB24_49:
	ldb r9, r4+0
	add r6, r6, r9
	addi r8, r8, -1
	addi r4, r4, 1
	bne r8, r5, .LBB24_49
.LBB24_50:
	sne r5, r7, r6
.LBB24_51:
	beq r5, r14, .LBB24_66
.LBB24_52:
	ldw r4, r16+64
	beq r4, r14, .LBB24_64
.LBB24_53:
	ldw r7, r16+36
	beq r7, r14, .LBB24_60
.LBB24_54:
	ldw r5, r16+68
	ldw r6, r5+0
	ble r1, r6, .LBB24_60
.LBB24_55:
	srli r6, r15, 7
	xor r8, r7, r6
	sgt r7, r7, r6
	sub r7, r14, r7
	and r7, r8, r7
	xor r8, r6, r7
	addi r7, r8, -1
	slli r9, r6, 3
	add r9, r5, r9
.LBB24_56:
	beq r8, r6, .LBB24_59
.LBB24_57:
	ldw r10, r9+0
	addi r6, r6, 1
	addi r9, r9, 8
	bgt r1, r10, .LBB24_56
.LBB24_58:
	addi r7, r6, -2
.LBB24_59:
	slli r6, r7, 3
	add r5, r5, r6
	ldw r6, r5+0
	addi r5, r5, 4
	jal r0, .LBB24_61
.LBB24_60:
	addi r5, r16, 40
	addi r6, r0, -1
.LBB24_61:
	ldw r5, r5+0
	bge r6, r15, .LBB24_65
.LBB24_62:
	addi r7, r0, -1
	xor r7, r6, r7
	add r1, r7, r1
	add r4, r6, r4
	addi r4, r4, 1
.LBB24_63:
	ldb r6, r4+0
	add r5, r5, r6
	addi r1, r1, -1
	addi r4, r4, 1
	bne r1, r14, .LBB24_63
	jal r0, .LBB24_65
.LBB24_64:
	addi r5, r0, -1
.LBB24_65:
	addi r4, r0, 2
	addi r6, r0, 0
	add r11, r3, r0
	add r7, r6, r0
	jal r31, luaD_hook
	add r3, r11, r0
.LBB24_66:
	stw r3+100, r15
.LBB24_67:
	ldbu r4, r3+6
	addi r1, r0, 1
	beq r4, r1, .LBB24_69
.LBB24_68:
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
.LBB24_69:
	addi r1, r0, 0
	beq r13, r1, .LBB24_71
.LBB24_70:
	addi r1, r0, 1
	stw r3+108, r1
.LBB24_71:
	ldhu r1, r12+34
	ori  r1, r1, 64
	sth r12+34, r1
	addi r4, r0, 1
	jal r31, luaD_throw
.Lfunc_end24:
	.size	luaG_traceexec, .Lfunc_end24-luaG_traceexec
                                        # -- End function
	.p2align	2                               # -- Begin function getobjname
	.type	getobjname,@function
getobjname:                             # @getobjname
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
	add r11, r6, r0
	add r12, r3, r0
	addi r13, fp, -44
	stw r13+0, r4
	add r4, r13, r0
	jal r31, basicgetobjname
	addi r14, r0, 0
	bne r1, r14, .LBB25_31
.LBB25_1:
	ldw r17, r13+0
	addi r1, r0, -1
	beq r17, r1, .LBB25_30
.LBB25_2:
	ldw r1, r12+52
	slli r3, r17, 2
	add r1, r1, r3
	ldw r16, r1+0
	andi r1, r16, 127
	addi r15, r0, 1
	addi r1, r1, -11
	addi r3, r0, 9
	bgtu r1, r3, .LBB25_21
.LBB25_3:
	slli r1, r1, 2
	lui r3, %hi(.LJTI25_0)
	addi r3, r3, %lo(.LJTI25_0)
	add r1, r3, r1
	ldw r3, r1+0
                                        # implicit-def: $r1
	jalr r0, r3, 0
.LBB25_4:
	srli r1, r16, 24
	ldw r3, r12+48
	addi r4, r0, 12
	mul r1, r1, r4
	add r3, r3, r1
	ldbu r1, r3+8
	andi r5, r1, 15
	lui r1, %hi(.L.str.18)
	addi r1, r1, %lo(.L.str.18)
	addi r6, r0, 4
	add r4, r1, r0
	bne r5, r6, .LBB25_6
.LBB25_5:
	ldw r3, r3+0
	addi r4, r3, 16
.LBB25_6:
	stw r11+0, r4
	ldw r3, r12+60
	srli r4, r16, 13
	andi r4, r4, 2040
	add r3, r3, r4
	ldw r3, r3+0
	addi r15, r0, 0
	seq r4, r3, r15
	addi r3, r3, 16
	sub r4, r15, r4
	xor r1, r3, r1
	and r1, r1, r4
	xor r3, r3, r1
	lui r4, %hi(.L.str.24)
	addi r4, r4, %lo(.L.str.24)
	jal r31, strcmp
	seq r1, r1, r15
	sub r1, r15, r1
	lui r3, %hi(.L.str.20)
	addi r3, r3, %lo(.L.str.20)
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	xor r4, r4, r3
	jal r0, .LBB25_13
.LBB25_7:
	lui r1, %hi(.L.str.19)
	addi r1, r1, %lo(.L.str.19)
	stw r11+0, r1
	lui r1, %hi(.L.str.20)
	addi r1, r1, %lo(.L.str.20)
	add r15, r14, r0
	jal r0, .LBB25_29
.LBB25_8:
	srli r5, r16, 24
	addi r13, fp, -36
	stw r13+0, r17
	add r3, r12, r0
	add r4, r13, r0
	add r6, r11, r0
	jal r31, basicgetobjname
	addi r15, r0, 0
	beq r1, r15, .LBB25_10
.LBB25_9:
	ldbu r1, r1+0
	addi r3, r0, 99
	beq r1, r3, .LBB25_11
.LBB25_10:
	lui r1, %hi(.L.str.18)
	addi r1, r1, %lo(.L.str.18)
	stw r11+0, r1
.LBB25_11:
	stw r13+0, r17
	srli r1, r16, 16
	andi r5, r1, 255
	addi r11, fp, -40
	add r3, r12, r0
	add r4, r13, r0
	add r6, r11, r0
	jal r31, basicgetobjname
	ldw r3, r11+0
	beq r3, r15, .LBB25_24
.LBB25_12:
	lui r4, %hi(.L.str.24)
	addi r4, r4, %lo(.L.str.24)
	jal r31, strcmp
	seq r1, r1, r15
	lui r3, %hi(.L.str.20)
	addi r3, r3, %lo(.L.str.20)
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	xor r4, r4, r3
	sub r1, r15, r1
.LBB25_13:
	and r1, r4, r1
	xor r1, r1, r3
	jal r0, .LBB25_29
.LBB25_14:
	srli r1, r16, 24
	ldw r3, r12+48
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	ldbu r3, r1+8
	andi r3, r3, 15
	addi r4, r0, 4
	bne r3, r4, .LBB25_22
.LBB25_15:
	ldw r1, r1+0
	addi r1, r1, 16
	jal r0, .LBB25_23
.LBB25_16:
	srli r5, r16, 24
	lui r1, 8
	and r1, r16, r1
	bne r1, r14, .LBB25_25
.LBB25_17:
	addi r4, fp, -36
	stw r4+0, r17
	add r3, r12, r0
	add r6, r11, r0
	jal r31, basicgetobjname
	addi r15, r0, 0
	beq r1, r15, .LBB25_19
.LBB25_18:
	ldbu r1, r1+0
	addi r3, r0, 99
	beq r1, r3, .LBB25_20
.LBB25_19:
	lui r1, %hi(.L.str.18)
	addi r1, r1, %lo(.L.str.18)
	stw r11+0, r1
.LBB25_20:
	lui r1, %hi(.L.str.21)
	addi r1, r1, %lo(.L.str.21)
	jal r0, .LBB25_29
.LBB25_21:
                                        # implicit-def: $r1
	jal r0, .LBB25_29
.LBB25_22:
	lui r1, %hi(.L.str.18)
	addi r1, r1, %lo(.L.str.18)
.LBB25_23:
	stw r11+0, r1
	addi r4, fp, -36
	stw r4+0, r17
	srli r1, r16, 16
	andi r5, r1, 255
	addi r11, fp, -40
	add r3, r12, r0
	add r6, r11, r0
	jal r31, basicgetobjname
	ldw r3, r11+0
	addi r15, r0, 0
	bne r3, r15, .LBB25_12
.LBB25_24:
	lui r1, %hi(.L.str.20)
	addi r1, r1, %lo(.L.str.20)
	jal r0, .LBB25_29
.LBB25_25:
	ldw r1, r12+48
	addi r3, r0, 12
	mul r3, r5, r3
	add r1, r1, r3
	ldbu r3, r1+8
	andi r3, r3, 15
	addi r4, r0, 4
	bne r3, r4, .LBB25_27
.LBB25_26:
	ldw r1, r1+0
	addi r1, r1, 16
	jal r0, .LBB25_28
.LBB25_27:
	lui r1, %hi(.L.str.18)
	addi r1, r1, %lo(.L.str.18)
.LBB25_28:
	stw r11+0, r1
	lui r1, %hi(.L.str.21)
	addi r1, r1, %lo(.L.str.21)
	add r15, r14, r0
.LBB25_29:
	beq r15, r14, .LBB25_31
.LBB25_30:
	addi r1, r0, 0
.LBB25_31:
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
.Lfunc_end25:
	.size	getobjname, .Lfunc_end25-getobjname
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI25_0:
	.word	.LBB25_4
	.word	.LBB25_8
	.word	.LBB25_7
	.word	.LBB25_14
	.word	.LBB25_29
	.word	.LBB25_29
	.word	.LBB25_29
	.word	.LBB25_29
	.word	.LBB25_29
	.word	.LBB25_16
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function basicgetobjname
	.type	basicgetobjname,@function
basicgetobjname:                        # @basicgetobjname
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
	stw fp+-72, lr
	add r11, r6, r0
	add r14, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	ldw r15, r4+0
	addi r4, r5, 1
	add r5, r15, r0
	jal r31, luaF_getlocalname
	stw r11+0, r1
	addi r16, r0, 0
	beq r1, r16, .LBB26_2
.LBB26_1:
	lui r1, %hi(.L.str.22)
	addi r1, r1, %lo(.L.str.22)
	jal r0, .LBB26_29
.LBB26_2:
	ldw r1, r12+52
	slli r3, r15, 2
	add r3, r1, r3
	ldw r3, r3+0
	andi r3, r3, 127
	lui r4, %hi(luaP_opmodes)
	addi r4, r4, %lo(luaP_opmodes)
	add r3, r3, r4
	ldb r3, r3+0
	srai r3, r3, 7
	add r6, r15, r3
	addi r5, r0, -1
	addi r3, r0, 1
	add r7, r5, r0
	blt r6, r3, .LBB26_15
.LBB26_3:
	addi r7, r0, -1
	addi r8, r0, 0
	addi r9, r0, 67
	addi r10, r0, 8
	addi r15, r0, 56
	lui r17, 1044480
	addi r17, r17, 2
	addi r18, r0, 1
	addi r19, r0, 2
	addi r20, r0, 76
	add r21, r1, r0
	add r22, r8, r0
	add r23, r8, r0
	jal r0, .LBB26_7
.LBB26_4:
	addi r24, r24, 2
.LBB26_5:
	sltu r24, r14, r24
.LBB26_6:
	slt r25, r22, r23
	sub r25, r8, r25
	or  r25, r25, r22
	sub r24, r8, r24
	xor r7, r7, r25
	and r7, r7, r24
	xor r7, r25, r7
	addi r22, r22, 1
	addi r21, r21, 4
	beq r6, r22, .LBB26_15
.LBB26_7:
	ldw r26, r21+0
	andi r25, r26, 127
	srli r27, r26, 7
	andi r24, r27, 255
	ble r25, r9, .LBB26_10
.LBB26_8:
	addi r26, r25, -68
	bltu r26, r19, .LBB26_5
.LBB26_9:
	beq r25, r20, .LBB26_4
	jal r0, .LBB26_14
.LBB26_10:
	beq r25, r10, .LBB26_13
.LBB26_11:
	bne r25, r15, .LBB26_14
.LBB26_12:
	add r24, r22, r27
	add r24, r24, r17
	sgt r25, r24, r6
	xor r26, r24, r23
	sgt r24, r24, r23
	sub r24, r8, r24
	and r24, r26, r24
	xor r23, r23, r24
	sub r25, r8, r25
	and r24, r24, r25
	xor r23, r23, r24
	add r24, r18, r0
	jal r0, .LBB26_6
.LBB26_13:
	sgtu r25, r24, r14
	srli r26, r26, 16
	andi r26, r26, 255
	add r24, r24, r26
	sgtu r24, r14, r24
	or  r24, r25, r24
	jal r0, .LBB26_6
.LBB26_14:
	add r25, r25, r4
	ldbu r25, r25+0
	andi r25, r25, 8
	seq r25, r25, r8
	sne r24, r14, r24
	or  r24, r24, r25
	jal r0, .LBB26_6
.LBB26_15:
	stw r13+0, r7
	beq r7, r5, .LBB26_28
.LBB26_16:
	slli r4, r7, 2
	add r5, r1, r4
	ldw r4, r5+0
	andi r1, r4, 127
	addi r6, r0, 9
	bgtu r1, r6, .LBB26_30
.LBB26_17:
	slli r1, r1, 2
	lui r6, %hi(.LJTI26_0)
	addi r6, r6, %lo(.LJTI26_0)
	add r1, r6, r1
	ldw r6, r1+0
                                        # implicit-def: $r1
	jalr r0, r6, 0
.LBB26_18:
	srli r1, r4, 16
	andi r5, r1, 255
	srli r1, r4, 7
	andi r4, r1, 255
                                        # implicit-def: $r1
	bgeu r5, r4, .LBB26_27
.LBB26_19:
	add r3, r12, r0
	add r4, r13, r0
	add r6, r11, r0
	jal r31, basicgetobjname
	add r3, r16, r0
	bne r3, r16, .LBB26_28
	jal r0, .LBB26_29
.LBB26_20:
	ldw r1, r5+4
	srli r1, r1, 7
	jal r0, .LBB26_22
.LBB26_21:
	srli r1, r4, 15
.LBB26_22:
	ldw r3, r12+48
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	ldbu r3, r1+8
	andi r4, r3, 15
	addi r3, r0, 0
	addi r5, r0, 4
	bne r4, r5, .LBB26_24
.LBB26_23:
	ldw r1, r1+0
	addi r4, r1, 16
	lui r1, %hi(.L.str.23)
	addi r1, r1, %lo(.L.str.23)
	jal r0, .LBB26_25
.LBB26_24:
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	add r1, r3, r0
.LBB26_25:
	stw r11+0, r4
	bne r3, r16, .LBB26_28
	jal r0, .LBB26_29
.LBB26_26:
	ldw r1, r12+60
	srli r3, r4, 13
	andi r3, r3, 2040
	add r1, r1, r3
	ldw r1, r1+0
	seq r3, r1, r16
	addi r1, r1, 16
	sub r3, r16, r3
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	xor r4, r1, r4
	and r3, r4, r3
	xor r1, r1, r3
	stw r11+0, r1
	lui r1, %hi(.L.str.17)
	addi r1, r1, %lo(.L.str.17)
	add r3, r16, r0
.LBB26_27:
	beq r3, r16, .LBB26_29
.LBB26_28:
	addi r1, r0, 0
.LBB26_29:
	ldw lr, fp+-72
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
.LBB26_30:
                                        # implicit-def: $r1
	bne r3, r16, .LBB26_28
	jal r0, .LBB26_29
.Lfunc_end26:
	.size	basicgetobjname, .Lfunc_end26-basicgetobjname
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI26_0:
	.word	.LBB26_18
	.word	.LBB26_27
	.word	.LBB26_27
	.word	.LBB26_21
	.word	.LBB26_20
	.word	.LBB26_27
	.word	.LBB26_27
	.word	.LBB26_27
	.word	.LBB26_27
	.word	.LBB26_26
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"(temporary)"
	.size	.L.str, 12

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"(C temporary)"
	.size	.L.str.1, 14

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"call"
	.size	.L.str.2, 5

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"bad 'for' %s (number expected, got %s)"
	.size	.L.str.3, 39

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"concatenate"
	.size	.L.str.4, 12

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"number%s has no integer representation"
	.size	.L.str.5, 39

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"attempt to compare two %s values"
	.size	.L.str.6, 33

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"attempt to compare %s with %s"
	.size	.L.str.7, 30

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"%s:%d: %s"
	.size	.L.str.8, 10

	.hidden	luaP_opmodes
	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"(vararg)"
	.size	.L.str.9, 9

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.zero	1
	.size	.L.str.10, 1

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"=[C]"
	.size	.L.str.11, 5

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"C"
	.size	.L.str.12, 2

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"=?"
	.size	.L.str.13, 3

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"main"
	.size	.L.str.14, 5

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"Lua"
	.size	.L.str.15, 4

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"attempt to %s a %s value%s"
	.size	.L.str.16, 27

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"upvalue"
	.size	.L.str.17, 8

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"?"
	.size	.L.str.18, 2

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"integer index"
	.size	.L.str.19, 14

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"field"
	.size	.L.str.20, 6

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"method"
	.size	.L.str.21, 7

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"local"
	.size	.L.str.22, 6

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"constant"
	.size	.L.str.23, 9

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"_ENV"
	.size	.L.str.24, 5

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"global"
	.size	.L.str.25, 7

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"hook"
	.size	.L.str.26, 5

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"__gc"
	.size	.L.str.27, 5

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"metamethod"
	.size	.L.str.28, 11

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"for iterator"
	.size	.L.str.29, 13

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	" (%s '%s')"
	.size	.L.str.30, 11

	.hidden	luaF_getlocalname
	.hidden	luaT_objtypename
	.hidden	luaV_tointegerns
	.hidden	luaO_chunkid
	.hidden	luaO_pushfstring
	.hidden	luaD_callnoyield
	.hidden	luaD_throw
	.hidden	luaC_step
	.hidden	luaO_pushvfstring
	.hidden	luaD_hookcall
	.hidden	luaD_hook
	.hidden	luaH_new
	.hidden	luaH_setint
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
