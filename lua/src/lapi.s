	.file	"lapi.c"
	.text
	.globl	lua_checkstack                  # -- Begin function lua_checkstack
	.p2align	2
	.type	lua_checkstack,@function
lua_checkstack:                         # @lua_checkstack
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
	ldw r13, r3+20
	ldw r1, r3+24
	ldw r3, r3+12
	sub r1, r1, r3
	srai r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r1, r1, r3
	ble r1, r4, .LBB0_2
.LBB0_1:
	addi r1, r0, 1
	addi r3, r0, 0
	bne r1, r3, .LBB0_3
	jal r0, .LBB0_5
.LBB0_2:
	addi r5, r0, 0
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaD_growstack
	addi r3, r0, 0
	beq r1, r3, .LBB0_5
.LBB0_3:
	ldw r4, r13+4
	ldw r3, r12+12
	addi r5, r0, 12
	mul r5, r11, r5
	add r3, r3, r5
	bgeu r4, r3, .LBB0_5
.LBB0_4:
	stw r13+4, r3
.LBB0_5:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	lua_checkstack, .Lfunc_end0-lua_checkstack
                                        # -- End function
	.globl	lua_xmove                       # -- Begin function lua_xmove
	.p2align	2
	.type	lua_xmove,@function
lua_xmove:                              # @lua_xmove
# %bb.0:
	beq r3, r4, .LBB1_4
.LBB1_1:
	ldw r6, r3+12
	addi r1, r0, 0
	sub r7, r1, r5
	addi r8, r0, 12
	mul r7, r7, r8
	add r6, r6, r7
	stw r3+12, r6
	addi r6, r0, 1
	blt r5, r6, .LBB1_4
.LBB1_2:
	add r6, r1, r0
.LBB1_3:
	ldw r7, r4+12
	ldw r8, r3+12
	add r8, r8, r6
	ldw r9, r8+0
	ldw r10, r8+4
	stw r7+4, r10
	stw r7+0, r9
	ldbu r8, r8+8
	stb r7+8, r8
	ldw r7, r4+12
	addi r7, r7, 12
	stw r4+12, r7
	addi r5, r5, -1
	addi r6, r6, 12
	bne r5, r1, .LBB1_3
.LBB1_4:
	jalr r0, r31, 0
.Lfunc_end1:
	.size	lua_xmove, .Lfunc_end1-lua_xmove
                                        # -- End function
	.globl	lua_atpanic                     # -- Begin function lua_atpanic
	.p2align	2
	.type	lua_atpanic,@function
lua_atpanic:                            # @lua_atpanic
# %bb.0:
	ldw r3, r3+16
	ldw r1, r3+148
	stw r3+148, r4
	jalr r0, r31, 0
.Lfunc_end2:
	.size	lua_atpanic, .Lfunc_end2-lua_atpanic
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function lua_version
.LCPI3_0:
	.quad	0x407f800000000000              # double 504
	.text
	.globl	lua_version
	.p2align	2
	.type	lua_version,@function
lua_version:                            # @lua_version
# %bb.0:
	lui r1, %hi(.LCPI3_0)
	addi r1, r1, %lo(.LCPI3_0)
	ldw r2, r1+4
	ldw r1, r1+0
	jalr r0, r31, 0
.Lfunc_end3:
	.size	lua_version, .Lfunc_end3-lua_version
                                        # -- End function
	.globl	lua_absindex                    # -- Begin function lua_absindex
	.p2align	2
	.type	lua_absindex,@function
lua_absindex:                           # @lua_absindex
# %bb.0:
	add r1, r4, r0
	addi r4, r4, -1
	lui r5, 1048572
	addi r5, r5, 384
	bltu r4, r5, .LBB4_2
.LBB4_1:
	ldw r4, r3+12
	ldw r3, r3+20
	ldw r3, r3+0
	sub r3, r4, r3
	srai r3, r3, 2
	lui r4, 699051
	addi r4, r4, -1365
	mul r3, r3, r4
	add r1, r3, r1
.LBB4_2:
	jalr r0, r31, 0
.Lfunc_end4:
	.size	lua_absindex, .Lfunc_end4-lua_absindex
                                        # -- End function
	.globl	lua_gettop                      # -- Begin function lua_gettop
	.p2align	2
	.type	lua_gettop,@function
lua_gettop:                             # @lua_gettop
# %bb.0:
	ldw r1, r3+12
	ldw r3, r3+20
	ldw r3, r3+0
	sub r1, r1, r3
	addi r1, r1, -12
	srai r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end5:
	.size	lua_gettop, .Lfunc_end5-lua_gettop
                                        # -- End function
	.globl	lua_settop                      # -- Begin function lua_settop
	.p2align	2
	.type	lua_settop,@function
lua_settop:                             # @lua_settop
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	addi r5, r0, 0
	addi r1, r0, 12
	blt r4, r5, .LBB6_5
.LBB6_1:
	ldw r6, r3+20
	ldw r6, r6+0
	mul r4, r4, r1
	add r4, r6, r4
	ldw r6, r3+12
	sub r4, r4, r6
	addi r7, r4, 12
	srai r4, r7, 2
	lui r6, 699051
	addi r6, r6, -1365
	mul r4, r4, r6
	addi r6, r0, 1
	blt r7, r6, .LBB6_6
.LBB6_2:
	addi r7, r4, 1
.LBB6_3:
	ldw r8, r3+12
	addi r9, r8, 12
	stw r3+12, r9
	stb r8+8, r5
	addi r7, r7, -1
	bgt r7, r6, .LBB6_3
.LBB6_4:
	slt r6, r4, r6
	sub r5, r5, r6
	xori r4, r4, 1
	and r4, r4, r5
	xori r4, r4, 1
	addi r4, r4, -1
	jal r0, .LBB6_6
.LBB6_5:
	addi r4, r4, 1
.LBB6_6:
	ldw r5, r3+12
	mul r1, r4, r1
	add r1, r5, r1
	addi r5, r0, -1
	bgt r4, r5, .LBB6_9
.LBB6_7:
	ldw r4, r3+36
	bltu r4, r1, .LBB6_9
.LBB6_8:
	addi r5, r0, -1
	addi r6, r0, 0
	add r11, r3, r0
	add r4, r1, r0
	jal r31, luaF_close
	add r3, r11, r0
.LBB6_9:
	stw r3+12, r1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	lua_settop, .Lfunc_end6-lua_settop
                                        # -- End function
	.globl	lua_closeslot                   # -- Begin function lua_closeslot
	.p2align	2
	.type	lua_closeslot,@function
lua_closeslot:                          # @lua_closeslot
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	addi r1, r0, 1
	blt r4, r1, .LBB7_2
.LBB7_1:
	ldw r1, r3+20
	jal r0, .LBB7_3
.LBB7_2:
	addi r1, r3, 12
.LBB7_3:
	ldw r1, r1+0
	addi r5, r0, 12
	mul r4, r4, r5
	add r4, r1, r4
	addi r5, r0, -1
	addi r11, r0, 0
	add r6, r11, r0
	jal r31, luaF_close
	stb r1+8, r11
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end7:
	.size	lua_closeslot, .Lfunc_end7-lua_closeslot
                                        # -- End function
	.globl	lua_rotate                      # -- Begin function lua_rotate
	.p2align	2
	.type	lua_rotate,@function
lua_rotate:                             # @lua_rotate
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	ldw r1, r3+12
	addi r1, r1, -12
	addi r6, r0, 1
	blt r4, r6, .LBB8_2
.LBB8_1:
	ldw r3, r3+20
	jal r0, .LBB8_3
.LBB8_2:
	addi r3, r3, 12
.LBB8_3:
	ldw r3, r3+0
	addi r6, r0, 12
	mul r4, r4, r6
	add r3, r3, r4
	addi r4, r0, 0
	sub r7, r4, r5
	mul r6, r7, r6
	add r7, r1, r6
	add r6, r3, r6
	addi r6, r6, -12
	slt r5, r5, r4
	xor r6, r6, r7
	sub r4, r4, r5
	and r4, r6, r4
	xor r4, r7, r4
	bgeu r3, r4, .LBB8_6
.LBB8_4:
	add r5, r3, r0
	add r6, r4, r0
.LBB8_5:
	ldw r7, r5+4
	ldw r8, r5+0
	ldbu r9, r5+8
	ldw r10, r6+4
	ldw r11, r6+0
	stw r5+0, r11
	stw r5+4, r10
	ldbu r10, r6+8
	stb r5+8, r10
	stw r6+4, r7
	stw r6+0, r8
	stb r6+8, r9
	addi r5, r5, 12
	addi r6, r6, -12
	bltu r5, r6, .LBB8_5
.LBB8_6:
	addi r4, r4, 12
	bgeu r4, r1, .LBB8_9
.LBB8_7:
	add r5, r1, r0
.LBB8_8:
	ldw r6, r4+4
	ldw r7, r4+0
	ldbu r8, r4+8
	ldw r9, r5+4
	ldw r10, r5+0
	stw r4+0, r10
	stw r4+4, r9
	ldbu r9, r5+8
	stb r4+8, r9
	stw r5+4, r6
	stw r5+0, r7
	stb r5+8, r8
	addi r4, r4, 12
	addi r5, r5, -12
	bltu r4, r5, .LBB8_8
.LBB8_9:
	bgeu r3, r1, .LBB8_11
.LBB8_10:
	ldw r4, r3+4
	ldw r5, r3+0
	ldbu r6, r3+8
	ldw r7, r1+4
	ldw r8, r1+0
	stw r3+0, r8
	stw r3+4, r7
	ldbu r7, r1+8
	stb r3+8, r7
	stw r1+4, r4
	stw r1+0, r5
	stb r1+8, r6
	addi r3, r3, 12
	addi r1, r1, -12
	bltu r3, r1, .LBB8_10
.LBB8_11:
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	lua_rotate, .Lfunc_end8-lua_rotate
                                        # -- End function
	.globl	lua_copy                        # -- Begin function lua_copy
	.p2align	2
	.type	lua_copy,@function
lua_copy:                               # @lua_copy
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r7, r3+20
	addi r8, r0, 1
	lui r1, 1048572
	blt r4, r8, .LBB9_3
.LBB9_1:
	ldw r6, r7+0
	addi r9, r0, 12
	mul r4, r4, r9
	add r6, r6, r4
	ldw r4, r3+12
	bltu r6, r4, .LBB9_7
.LBB9_2:
	ldw r4, r3+16
	addi r6, r4, 48
	bge r5, r8, .LBB9_8
	jal r0, .LBB9_13
.LBB9_3:
	addi r6, r1, 385
	bge r4, r6, .LBB9_6
.LBB9_4:
	addi r6, r1, 384
	bne r4, r6, .LBB9_10
.LBB9_5:
	ldw r4, r3+16
	addi r6, r4, 36
	bge r5, r8, .LBB9_8
	jal r0, .LBB9_13
.LBB9_6:
	ldw r6, r3+12
	addi r9, r0, 12
	mul r4, r4, r9
	add r6, r6, r4
.LBB9_7:
	blt r5, r8, .LBB9_13
.LBB9_8:
	ldw r4, r7+0
	addi r7, r0, 12
	mul r7, r5, r7
	add r4, r4, r7
	ldw r7, r3+12
	bltu r4, r7, .LBB9_17
.LBB9_9:
	ldw r4, r3+16
	addi r4, r4, 48
	jal r0, .LBB9_17
.LBB9_10:
	ldw r9, r7+0
	ldbu r10, r9+8
	addi r11, r0, 102
	bne r10, r11, .LBB9_2
.LBB9_11:
	sub r10, r6, r4
	ldw r6, r9+0
	ldbu r9, r6+6
	bgt r10, r9, .LBB9_2
.LBB9_12:
	addi r9, r1, 383
	sub r4, r9, r4
	addi r9, r0, 12
	mul r4, r4, r9
	add r4, r6, r4
	addi r6, r4, 16
	bge r5, r8, .LBB9_8
.LBB9_13:
	addi r4, r1, 385
	bge r5, r4, .LBB9_16
.LBB9_14:
	addi r4, r1, 384
	bne r5, r4, .LBB9_23
.LBB9_15:
	ldw r4, r3+16
	addi r4, r4, 36
	jal r0, .LBB9_17
.LBB9_16:
	ldw r4, r3+12
	addi r7, r0, 12
	mul r7, r5, r7
	add r4, r4, r7
.LBB9_17:
	ldw r7, r6+0
	ldw r8, r6+4
	stw r4+4, r8
	stw r4+0, r7
	ldbu r7, r6+8
	stb r4+8, r7
	addi r1, r1, 383
	bgt r5, r1, .LBB9_22
.LBB9_18:
	ldbu r1, r6+8
	andi r4, r1, 64
	addi r1, r0, 0
	beq r4, r1, .LBB9_22
.LBB9_19:
	ldw r4, r3+20
	ldw r4, r4+0
	ldw r4, r4+0
	ldbu r5, r4+5
	andi r5, r5, 32
	beq r5, r1, .LBB9_22
.LBB9_20:
	ldw r5, r6+0
	ldbu r6, r5+5
	andi r6, r6, 24
	beq r6, r1, .LBB9_22
.LBB9_21:
	jal r31, luaC_barrier_
.LBB9_22:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB9_23:
	ldw r7, r7+0
	ldbu r8, r7+8
	addi r9, r0, 102
	bne r8, r9, .LBB9_9
.LBB9_24:
	sub r8, r4, r5
	ldw r4, r7+0
	ldbu r7, r4+6
	bgt r8, r7, .LBB9_9
.LBB9_25:
	addi r7, r1, 383
	sub r7, r7, r5
	addi r8, r0, 12
	mul r7, r7, r8
	add r4, r4, r7
	addi r4, r4, 16
	jal r0, .LBB9_17
.Lfunc_end9:
	.size	lua_copy, .Lfunc_end9-lua_copy
                                        # -- End function
	.globl	lua_pushvalue                   # -- Begin function lua_pushvalue
	.p2align	2
	.type	lua_pushvalue,@function
lua_pushvalue:                          # @lua_pushvalue
# %bb.0:
	ldw r1, r3+12
	ldw r6, r3+20
	addi r5, r0, 1
	blt r4, r5, .LBB10_3
.LBB10_1:
	ldw r5, r6+0
	addi r6, r0, 12
	mul r4, r4, r6
	add r4, r5, r4
	bltu r4, r1, .LBB10_7
.LBB10_2:
	ldw r4, r3+16
	addi r4, r4, 48
	jal r0, .LBB10_7
.LBB10_3:
	lui r5, 1048572
	addi r7, r5, 385
	bge r4, r7, .LBB10_6
.LBB10_4:
	addi r7, r5, 384
	bne r4, r7, .LBB10_8
.LBB10_5:
	ldw r4, r3+16
	addi r4, r4, 36
	jal r0, .LBB10_7
.LBB10_6:
	addi r5, r0, 12
	mul r4, r4, r5
	add r4, r1, r4
.LBB10_7:
	ldw r5, r4+0
	ldw r6, r4+4
	stw r1+4, r6
	stw r1+0, r5
	ldbu r4, r4+8
	stb r1+8, r4
	ldw r1, r3+12
	addi r1, r1, 12
	stw r3+12, r1
	jalr r0, r31, 0
.LBB10_8:
	ldw r6, r6+0
	ldbu r8, r6+8
	addi r9, r0, 102
	bne r8, r9, .LBB10_2
.LBB10_9:
	sub r7, r7, r4
	ldw r6, r6+0
	ldbu r8, r6+6
	bgt r7, r8, .LBB10_2
.LBB10_10:
	addi r5, r5, 383
	sub r4, r5, r4
	addi r5, r0, 12
	mul r4, r4, r5
	add r4, r6, r4
	addi r4, r4, 16
	jal r0, .LBB10_7
.Lfunc_end10:
	.size	lua_pushvalue, .Lfunc_end10-lua_pushvalue
                                        # -- End function
	.globl	lua_type                        # -- Begin function lua_type
	.p2align	2
	.type	lua_type,@function
lua_type:                               # @lua_type
# %bb.0:
	ldw r5, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB11_3
.LBB11_1:
	ldw r1, r5+0
	addi r5, r0, 12
	mul r4, r4, r5
	add r4, r1, r4
	ldw r1, r3+12
	bltu r4, r1, .LBB11_7
.LBB11_2:
	ldw r1, r3+16
	addi r4, r1, 48
	jal r0, .LBB11_7
.LBB11_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB11_6
.LBB11_4:
	addi r6, r1, 384
	bne r4, r6, .LBB11_11
.LBB11_5:
	ldw r1, r3+16
	addi r4, r1, 36
	jal r0, .LBB11_7
.LBB11_6:
	ldw r1, r3+12
	addi r5, r0, 12
	mul r4, r4, r5
	add r4, r1, r4
.LBB11_7:
	ldbu r1, r4+8
	andi r1, r1, 15
	addi r5, r0, 0
	bne r1, r5, .LBB11_10
.LBB11_8:
	ldw r3, r3+16
	addi r3, r3, 48
	bne r4, r3, .LBB11_10
.LBB11_9:
	addi r1, r0, -1
.LBB11_10:
	jalr r0, r31, 0
.LBB11_11:
	ldw r5, r5+0
	ldbu r7, r5+8
	addi r8, r0, 102
	bne r7, r8, .LBB11_2
.LBB11_12:
	sub r6, r6, r4
	ldw r5, r5+0
	ldbu r7, r5+6
	bgt r6, r7, .LBB11_2
.LBB11_13:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r5, r1
	addi r4, r1, 16
	jal r0, .LBB11_7
.Lfunc_end11:
	.size	lua_type, .Lfunc_end11-lua_type
                                        # -- End function
	.globl	lua_typename                    # -- Begin function lua_typename
	.p2align	2
	.type	lua_typename,@function
lua_typename:                           # @lua_typename
# %bb.0:
	slli r1, r4, 2
	lui r3, %hi(luaT_typenames_+4)
	addi r3, r3, %lo(luaT_typenames_+4)
	add r1, r1, r3
	ldw r1, r1+0
	jalr r0, r31, 0
.Lfunc_end12:
	.size	lua_typename, .Lfunc_end12-lua_typename
                                        # -- End function
	.globl	lua_iscfunction                 # -- Begin function lua_iscfunction
	.p2align	2
	.type	lua_iscfunction,@function
lua_iscfunction:                        # @lua_iscfunction
# %bb.0:
	ldw r5, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB13_3
.LBB13_1:
	ldw r1, r5+0
	addi r5, r0, 12
	mul r4, r4, r5
	add r1, r1, r4
	ldw r4, r3+12
	bltu r1, r4, .LBB13_7
.LBB13_2:
	ldw r1, r3+16
	addi r1, r1, 48
	jal r0, .LBB13_7
.LBB13_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB13_6
.LBB13_4:
	addi r6, r1, 384
	bne r4, r6, .LBB13_8
.LBB13_5:
	ldw r1, r3+16
	addi r1, r1, 36
	jal r0, .LBB13_7
.LBB13_6:
	ldw r1, r3+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB13_7:
	ldbu r1, r1+8
	addi r3, r0, 22
	seq r3, r1, r3
	addi r4, r0, 102
	seq r1, r1, r4
	or  r1, r3, r1
	jalr r0, r31, 0
.LBB13_8:
	ldw r5, r5+0
	ldbu r7, r5+8
	addi r8, r0, 102
	bne r7, r8, .LBB13_2
.LBB13_9:
	sub r6, r6, r4
	ldw r5, r5+0
	ldbu r7, r5+6
	bgt r6, r7, .LBB13_2
.LBB13_10:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r5, r1
	addi r1, r1, 16
	jal r0, .LBB13_7
.Lfunc_end13:
	.size	lua_iscfunction, .Lfunc_end13-lua_iscfunction
                                        # -- End function
	.globl	lua_isinteger                   # -- Begin function lua_isinteger
	.p2align	2
	.type	lua_isinteger,@function
lua_isinteger:                          # @lua_isinteger
# %bb.0:
	ldw r5, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB14_3
.LBB14_1:
	ldw r1, r5+0
	addi r5, r0, 12
	mul r4, r4, r5
	add r1, r1, r4
	ldw r4, r3+12
	bltu r1, r4, .LBB14_7
.LBB14_2:
	ldw r1, r3+16
	addi r1, r1, 48
	jal r0, .LBB14_7
.LBB14_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB14_6
.LBB14_4:
	addi r6, r1, 384
	bne r4, r6, .LBB14_8
.LBB14_5:
	ldw r1, r3+16
	addi r1, r1, 36
	jal r0, .LBB14_7
.LBB14_6:
	ldw r1, r3+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB14_7:
	ldbu r1, r1+8
	addi r3, r0, 3
	seq r1, r1, r3
	jalr r0, r31, 0
.LBB14_8:
	ldw r5, r5+0
	ldbu r7, r5+8
	addi r8, r0, 102
	bne r7, r8, .LBB14_2
.LBB14_9:
	sub r6, r6, r4
	ldw r5, r5+0
	ldbu r7, r5+6
	bgt r6, r7, .LBB14_2
.LBB14_10:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r5, r1
	addi r1, r1, 16
	jal r0, .LBB14_7
.Lfunc_end14:
	.size	lua_isinteger, .Lfunc_end14-lua_isinteger
                                        # -- End function
	.globl	lua_isnumber                    # -- Begin function lua_isnumber
	.p2align	2
	.type	lua_isnumber,@function
lua_isnumber:                           # @lua_isnumber
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r6, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB15_3
.LBB15_1:
	ldw r5, r6+0
	addi r6, r0, 12
	mul r4, r4, r6
	add r5, r5, r4
	ldw r4, r3+12
	bltu r5, r4, .LBB15_7
.LBB15_2:
	ldw r3, r3+16
	addi r5, r3, 48
	jal r0, .LBB15_7
.LBB15_3:
	lui r5, 1048572
	addi r7, r5, 385
	bge r4, r7, .LBB15_6
.LBB15_4:
	addi r7, r5, 384
	bne r4, r7, .LBB15_11
.LBB15_5:
	ldw r3, r3+16
	addi r5, r3, 36
	jal r0, .LBB15_7
.LBB15_6:
	ldw r3, r3+12
	addi r5, r0, 12
	mul r4, r4, r5
	add r5, r3, r4
.LBB15_7:
	ldbu r3, r5+8
	addi r4, r0, 19
	bne r3, r4, .LBB15_9
.LBB15_8:
	ldw r3, r5+4
	ldw r4, r5+0
	addi r5, fp, -12
	stw r5+4, r3
	stw r5+0, r4
	jal r0, .LBB15_10
.LBB15_9:
	addi r4, fp, -12
	add r3, r5, r0
	jal r31, luaV_tonumber_
.LBB15_10:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB15_11:
	ldw r6, r6+0
	ldbu r8, r6+8
	addi r9, r0, 102
	bne r8, r9, .LBB15_2
.LBB15_12:
	sub r7, r7, r4
	ldw r6, r6+0
	ldbu r8, r6+6
	bgt r7, r8, .LBB15_2
.LBB15_13:
	addi r3, r5, 383
	sub r3, r3, r4
	addi r4, r0, 12
	mul r3, r3, r4
	add r3, r6, r3
	addi r5, r3, 16
	jal r0, .LBB15_7
.Lfunc_end15:
	.size	lua_isnumber, .Lfunc_end15-lua_isnumber
                                        # -- End function
	.globl	lua_isstring                    # -- Begin function lua_isstring
	.p2align	2
	.type	lua_isstring,@function
lua_isstring:                           # @lua_isstring
# %bb.0:
	ldw r5, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB16_3
.LBB16_1:
	ldw r1, r5+0
	addi r5, r0, 12
	mul r4, r4, r5
	add r1, r1, r4
	ldw r4, r3+12
	bltu r1, r4, .LBB16_7
.LBB16_2:
	ldw r1, r3+16
	addi r1, r1, 48
	jal r0, .LBB16_7
.LBB16_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB16_6
.LBB16_4:
	addi r6, r1, 384
	bne r4, r6, .LBB16_8
.LBB16_5:
	ldw r1, r3+16
	addi r1, r1, 36
	jal r0, .LBB16_7
.LBB16_6:
	ldw r1, r3+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB16_7:
	ldbu r1, r1+8
	andi r1, r1, 15
	addi r1, r1, -3
	addi r3, r0, 2
	sltu r1, r1, r3
	jalr r0, r31, 0
.LBB16_8:
	ldw r5, r5+0
	ldbu r7, r5+8
	addi r8, r0, 102
	bne r7, r8, .LBB16_2
.LBB16_9:
	sub r6, r6, r4
	ldw r5, r5+0
	ldbu r7, r5+6
	bgt r6, r7, .LBB16_2
.LBB16_10:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r5, r1
	addi r1, r1, 16
	jal r0, .LBB16_7
.Lfunc_end16:
	.size	lua_isstring, .Lfunc_end16-lua_isstring
                                        # -- End function
	.globl	lua_isuserdata                  # -- Begin function lua_isuserdata
	.p2align	2
	.type	lua_isuserdata,@function
lua_isuserdata:                         # @lua_isuserdata
# %bb.0:
	ldw r5, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB17_3
.LBB17_1:
	ldw r1, r5+0
	addi r5, r0, 12
	mul r4, r4, r5
	add r1, r1, r4
	ldw r4, r3+12
	bltu r1, r4, .LBB17_7
.LBB17_2:
	ldw r1, r3+16
	addi r1, r1, 48
	jal r0, .LBB17_7
.LBB17_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB17_6
.LBB17_4:
	addi r6, r1, 384
	bne r4, r6, .LBB17_8
.LBB17_5:
	ldw r1, r3+16
	addi r1, r1, 36
	jal r0, .LBB17_7
.LBB17_6:
	ldw r1, r3+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB17_7:
	ldbu r1, r1+8
	addi r3, r0, 71
	seq r3, r1, r3
	addi r4, r0, 2
	seq r1, r1, r4
	or  r1, r3, r1
	jalr r0, r31, 0
.LBB17_8:
	ldw r5, r5+0
	ldbu r7, r5+8
	addi r8, r0, 102
	bne r7, r8, .LBB17_2
.LBB17_9:
	sub r6, r6, r4
	ldw r5, r5+0
	ldbu r7, r5+6
	bgt r6, r7, .LBB17_2
.LBB17_10:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r5, r1
	addi r1, r1, 16
	jal r0, .LBB17_7
.Lfunc_end17:
	.size	lua_isuserdata, .Lfunc_end17-lua_isuserdata
                                        # -- End function
	.globl	lua_rawequal                    # -- Begin function lua_rawequal
	.p2align	2
	.type	lua_rawequal,@function
lua_rawequal:                           # @lua_rawequal
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+20
	addi r6, r0, 1
	blt r4, r6, .LBB18_3
.LBB18_1:
	ldw r7, r1+0
	addi r8, r0, 12
	mul r4, r4, r8
	add r4, r7, r4
	ldw r7, r3+12
	bltu r4, r7, .LBB18_7
.LBB18_2:
	ldw r4, r3+16
	addi r4, r4, 48
	bge r5, r6, .LBB18_8
	jal r0, .LBB18_13
.LBB18_3:
	lui r7, 1048572
	addi r8, r7, 385
	bge r4, r8, .LBB18_6
.LBB18_4:
	addi r8, r7, 384
	bne r4, r8, .LBB18_10
.LBB18_5:
	ldw r4, r3+16
	addi r4, r4, 36
	bge r5, r6, .LBB18_8
	jal r0, .LBB18_13
.LBB18_6:
	ldw r7, r3+12
	addi r8, r0, 12
	mul r4, r4, r8
	add r4, r7, r4
.LBB18_7:
	blt r5, r6, .LBB18_13
.LBB18_8:
	ldw r1, r1+0
	addi r6, r0, 12
	mul r5, r5, r6
	add r5, r1, r5
	ldw r1, r3+12
	bltu r5, r1, .LBB18_17
.LBB18_9:
	ldw r1, r3+16
	addi r5, r1, 48
	jal r0, .LBB18_17
.LBB18_10:
	ldw r9, r1+0
	ldbu r10, r9+8
	addi r11, r0, 102
	bne r10, r11, .LBB18_2
.LBB18_11:
	sub r10, r8, r4
	ldw r8, r9+0
	ldbu r9, r8+6
	bgt r10, r9, .LBB18_2
.LBB18_12:
	addi r7, r7, 383
	sub r4, r7, r4
	addi r7, r0, 12
	mul r4, r4, r7
	add r4, r8, r4
	addi r4, r4, 16
	bge r5, r6, .LBB18_8
.LBB18_13:
	lui r6, 1048572
	addi r7, r6, 385
	bge r5, r7, .LBB18_16
.LBB18_14:
	addi r7, r6, 384
	bne r5, r7, .LBB18_23
.LBB18_15:
	ldw r1, r3+16
	addi r5, r1, 36
	jal r0, .LBB18_17
.LBB18_16:
	ldw r1, r3+12
	addi r6, r0, 12
	mul r5, r5, r6
	add r5, r1, r5
.LBB18_17:
	ldbu r1, r4+8
	andi r6, r1, 15
	addi r1, r0, 0
	bne r6, r1, .LBB18_19
.LBB18_18:
	ldw r6, r3+16
	addi r6, r6, 48
	beq r4, r6, .LBB18_22
.LBB18_19:
	ldbu r6, r5+8
	andi r6, r6, 15
	bne r6, r1, .LBB18_21
.LBB18_20:
	ldw r3, r3+16
	addi r3, r3, 48
	beq r5, r3, .LBB18_22
.LBB18_21:
	addi r3, r0, 0
	jal r31, luaV_equalobj
.LBB18_22:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB18_23:
	ldw r1, r1+0
	ldbu r8, r1+8
	addi r9, r0, 102
	bne r8, r9, .LBB18_9
.LBB18_24:
	sub r7, r7, r5
	ldw r1, r1+0
	ldbu r8, r1+6
	bgt r7, r8, .LBB18_9
.LBB18_25:
	addi r6, r6, 383
	sub r5, r6, r5
	addi r6, r0, 12
	mul r5, r5, r6
	add r1, r1, r5
	addi r5, r1, 16
	jal r0, .LBB18_17
.Lfunc_end18:
	.size	lua_rawequal, .Lfunc_end18-lua_rawequal
                                        # -- End function
	.globl	lua_arith                       # -- Begin function lua_arith
	.p2align	2
	.type	lua_arith,@function
lua_arith:                              # @lua_arith
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	addi r1, r4, -14
	addi r3, r0, -2
	bltu r1, r3, .LBB19_2
.LBB19_1:
	ldw r1, r11+12
	ldw r3, r1+-12
	ldw r5, r1+-8
	stw r1+4, r5
	stw r1+0, r3
	ldbu r3, r1+-4
	stb r1+8, r3
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
.LBB19_2:
	ldw r1, r11+12
	addi r5, r1, -24
	addi r6, r1, -12
	add r3, r11, r0
	add r7, r5, r0
	jal r31, luaO_arith
	ldw r1, r11+12
	addi r1, r1, -12
	stw r11+12, r1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end19:
	.size	lua_arith, .Lfunc_end19-lua_arith
                                        # -- End function
	.globl	lua_compare                     # -- Begin function lua_compare
	.p2align	2
	.type	lua_compare,@function
lua_compare:                            # @lua_compare
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	ldw r1, r3+20
	addi r7, r0, 1
	blt r4, r7, .LBB20_3
.LBB20_1:
	ldw r8, r1+0
	addi r9, r0, 12
	mul r4, r4, r9
	add r4, r8, r4
	ldw r8, r3+12
	bltu r4, r8, .LBB20_7
.LBB20_2:
	ldw r4, r3+16
	addi r4, r4, 48
	bge r5, r7, .LBB20_8
	jal r0, .LBB20_13
.LBB20_3:
	lui r8, 1048572
	addi r9, r8, 385
	bge r4, r9, .LBB20_6
.LBB20_4:
	addi r9, r8, 384
	bne r4, r9, .LBB20_10
.LBB20_5:
	ldw r4, r3+16
	addi r4, r4, 36
	bge r5, r7, .LBB20_8
	jal r0, .LBB20_13
.LBB20_6:
	ldw r8, r3+12
	addi r9, r0, 12
	mul r4, r4, r9
	add r4, r8, r4
.LBB20_7:
	blt r5, r7, .LBB20_13
.LBB20_8:
	ldw r1, r1+0
	addi r8, r0, 12
	mul r5, r5, r8
	add r5, r1, r5
	ldw r1, r3+12
	bltu r5, r1, .LBB20_17
.LBB20_9:
	ldw r1, r3+16
	addi r5, r1, 48
	jal r0, .LBB20_17
.LBB20_10:
	ldw r10, r1+0
	ldbu r11, r10+8
	addi r12, r0, 102
	bne r11, r12, .LBB20_2
.LBB20_11:
	sub r11, r9, r4
	ldw r9, r10+0
	ldbu r10, r9+6
	bgt r11, r10, .LBB20_2
.LBB20_12:
	addi r8, r8, 383
	sub r4, r8, r4
	addi r8, r0, 12
	mul r4, r4, r8
	add r4, r9, r4
	addi r4, r4, 16
	bge r5, r7, .LBB20_8
.LBB20_13:
	lui r8, 1048572
	addi r9, r8, 385
	bge r5, r9, .LBB20_16
.LBB20_14:
	addi r9, r8, 384
	bne r5, r9, .LBB20_28
.LBB20_15:
	ldw r1, r3+16
	addi r5, r1, 36
	jal r0, .LBB20_17
.LBB20_16:
	ldw r1, r3+12
	addi r8, r0, 12
	mul r5, r5, r8
	add r5, r1, r5
.LBB20_17:
	ldbu r1, r4+8
	andi r8, r1, 15
	addi r1, r0, 0
	bne r8, r1, .LBB20_19
.LBB20_18:
	ldw r8, r3+16
	addi r8, r8, 48
	beq r4, r8, .LBB20_27
.LBB20_19:
	ldbu r8, r5+8
	andi r8, r8, 15
	bne r8, r1, .LBB20_21
.LBB20_20:
	ldw r8, r3+16
	addi r8, r8, 48
	beq r5, r8, .LBB20_27
.LBB20_21:
	addi r8, r0, 2
	beq r6, r8, .LBB20_26
.LBB20_22:
	beq r6, r7, .LBB20_25
.LBB20_23:
	addi r7, r0, 0
	bne r6, r7, .LBB20_27
.LBB20_24:
	jal r31, luaV_equalobj
	jal r0, .LBB20_27
.LBB20_25:
	jal r31, luaV_lessthan
	jal r0, .LBB20_27
.LBB20_26:
	jal r31, luaV_lessequal
.LBB20_27:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB20_28:
	ldw r1, r1+0
	ldbu r10, r1+8
	addi r11, r0, 102
	bne r10, r11, .LBB20_9
.LBB20_29:
	sub r9, r9, r5
	ldw r1, r1+0
	ldbu r10, r1+6
	bgt r9, r10, .LBB20_9
.LBB20_30:
	addi r8, r8, 383
	sub r5, r8, r5
	addi r8, r0, 12
	mul r5, r5, r8
	add r1, r1, r5
	addi r5, r1, 16
	jal r0, .LBB20_17
.Lfunc_end20:
	.size	lua_compare, .Lfunc_end20-lua_compare
                                        # -- End function
	.globl	lua_stringtonumber              # -- Begin function lua_stringtonumber
	.p2align	2
	.type	lua_stringtonumber,@function
lua_stringtonumber:                     # @lua_stringtonumber
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r1, r4, r0
	add r11, r3, r0
	ldw r4, r3+12
	add r3, r1, r0
	jal r31, luaO_str2num
	addi r3, r0, 0
	beq r1, r3, .LBB21_2
.LBB21_1:
	ldw r3, r11+12
	addi r3, r3, 12
	stw r11+12, r3
.LBB21_2:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end21:
	.size	lua_stringtonumber, .Lfunc_end21-lua_stringtonumber
                                        # -- End function
	.globl	lua_tonumberx                   # -- Begin function lua_tonumberx
	.p2align	2
	.type	lua_tonumberx,@function
lua_tonumberx:                          # @lua_tonumberx
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
	add r11, r5, r0
	addi r13, r0, 0
	addi r12, fp, -24
	stw r12+4, r13
	stw r12+0, r13
	ldw r6, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB22_3
.LBB22_1:
	ldw r5, r6+0
	addi r6, r0, 12
	mul r4, r4, r6
	add r5, r5, r4
	ldw r4, r3+12
	bltu r5, r4, .LBB22_7
.LBB22_2:
	ldw r3, r3+16
	addi r5, r3, 48
	jal r0, .LBB22_7
.LBB22_3:
	lui r5, 1048572
	addi r7, r5, 385
	bge r4, r7, .LBB22_6
.LBB22_4:
	addi r7, r5, 384
	bne r4, r7, .LBB22_12
.LBB22_5:
	ldw r3, r3+16
	addi r5, r3, 36
	jal r0, .LBB22_7
.LBB22_6:
	ldw r3, r3+12
	addi r5, r0, 12
	mul r4, r4, r5
	add r5, r3, r4
.LBB22_7:
	ldbu r3, r5+8
	addi r4, r0, 19
	bne r3, r4, .LBB22_9
.LBB22_8:
	ldw r3, r5+4
	ldw r4, r5+0
	stw r12+4, r3
	stw r12+0, r4
	bne r11, r13, .LBB22_10
	jal r0, .LBB22_11
.LBB22_9:
	addi r4, fp, -24
	add r3, r5, r0
	jal r31, luaV_tonumber_
	beq r11, r13, .LBB22_11
.LBB22_10:
	stw r11+0, r1
.LBB22_11:
	ldw r2, r12+4
	ldw r1, r12+0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB22_12:
	ldw r6, r6+0
	ldbu r8, r6+8
	addi r9, r0, 102
	bne r8, r9, .LBB22_2
.LBB22_13:
	sub r7, r7, r4
	ldw r6, r6+0
	ldbu r8, r6+6
	bgt r7, r8, .LBB22_2
.LBB22_14:
	addi r3, r5, 383
	sub r3, r3, r4
	addi r4, r0, 12
	mul r3, r3, r4
	add r3, r6, r3
	addi r5, r3, 16
	jal r0, .LBB22_7
.Lfunc_end22:
	.size	lua_tonumberx, .Lfunc_end22-lua_tonumberx
                                        # -- End function
	.globl	lua_tointegerx                  # -- Begin function lua_tointegerx
	.p2align	2
	.type	lua_tointegerx,@function
lua_tointegerx:                         # @lua_tointegerx
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
	addi r12, r0, 0
	addi r11, fp, -20
	stw r11+0, r12
	ldw r7, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB23_3
.LBB23_1:
	ldw r6, r7+0
	addi r7, r0, 12
	mul r4, r4, r7
	add r6, r6, r4
	ldw r4, r3+12
	bltu r6, r4, .LBB23_7
.LBB23_2:
	ldw r3, r3+16
	addi r6, r3, 48
	jal r0, .LBB23_7
.LBB23_3:
	lui r6, 1048572
	addi r8, r6, 385
	bge r4, r8, .LBB23_6
.LBB23_4:
	addi r8, r6, 384
	bne r4, r8, .LBB23_11
.LBB23_5:
	ldw r3, r3+16
	addi r6, r3, 36
	jal r0, .LBB23_7
.LBB23_6:
	ldw r3, r3+12
	addi r6, r0, 12
	mul r4, r4, r6
	add r6, r3, r4
.LBB23_7:
	ldbu r3, r6+8
	addi r4, r0, 3
	bne r3, r4, .LBB23_14
.LBB23_8:
	ldw r3, r6+0
	stw r11+0, r3
	beq r5, r12, .LBB23_10
.LBB23_9:
	stw r5+0, r1
.LBB23_10:
	ldw r1, r11+0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB23_11:
	ldw r7, r7+0
	ldbu r9, r7+8
	addi r10, r0, 102
	bne r9, r10, .LBB23_2
.LBB23_12:
	sub r8, r8, r4
	ldw r7, r7+0
	ldbu r9, r7+6
	bgt r8, r9, .LBB23_2
.LBB23_13:
	addi r3, r6, 383
	sub r3, r3, r4
	addi r4, r0, 12
	mul r3, r3, r4
	add r3, r7, r3
	addi r6, r3, 16
	jal r0, .LBB23_7
.LBB23_14:
	addi r4, fp, -20
	addi r1, r0, 0
	add r3, r6, r0
	add r13, r5, r0
	add r5, r1, r0
	jal r31, luaV_tointeger
	add r5, r13, r0
	bne r5, r12, .LBB23_9
	jal r0, .LBB23_10
.Lfunc_end23:
	.size	lua_tointegerx, .Lfunc_end23-lua_tointegerx
                                        # -- End function
	.globl	lua_toboolean                   # -- Begin function lua_toboolean
	.p2align	2
	.type	lua_toboolean,@function
lua_toboolean:                          # @lua_toboolean
# %bb.0:
	ldw r6, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB24_3
.LBB24_1:
	ldw r5, r6+0
	addi r6, r0, 12
	mul r4, r4, r6
	add r4, r5, r4
	ldw r5, r3+12
	bltu r4, r5, .LBB24_7
.LBB24_2:
	ldw r3, r3+16
	addi r4, r3, 48
	jal r0, .LBB24_7
.LBB24_3:
	lui r5, 1048572
	addi r7, r5, 385
	bge r4, r7, .LBB24_6
.LBB24_4:
	addi r7, r5, 384
	bne r4, r7, .LBB24_8
.LBB24_5:
	ldw r3, r3+16
	addi r4, r3, 36
	jal r0, .LBB24_7
.LBB24_6:
	ldw r3, r3+12
	addi r5, r0, 12
	mul r4, r4, r5
	add r4, r3, r4
.LBB24_7:
	ldbu r3, r4+8
	sne r1, r3, r1
	andi r3, r3, 15
	addi r4, r0, 0
	sne r3, r3, r4
	and r1, r1, r3
	jalr r0, r31, 0
.LBB24_8:
	ldw r6, r6+0
	ldbu r8, r6+8
	addi r9, r0, 102
	bne r8, r9, .LBB24_2
.LBB24_9:
	sub r7, r7, r4
	ldw r6, r6+0
	ldbu r8, r6+6
	bgt r7, r8, .LBB24_2
.LBB24_10:
	addi r3, r5, 383
	sub r3, r3, r4
	addi r4, r0, 12
	mul r3, r3, r4
	add r3, r6, r3
	addi r4, r3, 16
	jal r0, .LBB24_7
.Lfunc_end24:
	.size	lua_toboolean, .Lfunc_end24-lua_toboolean
                                        # -- End function
	.globl	lua_tolstring                   # -- Begin function lua_tolstring
	.p2align	2
	.type	lua_tolstring,@function
lua_tolstring:                          # @lua_tolstring
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
	ldw r3, r3+20
	addi r12, r0, 1
	blt r4, r12, .LBB25_3
.LBB25_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
	ldw r3, r11+12
	bltu r1, r3, .LBB25_7
.LBB25_2:
	ldw r1, r11+16
	addi r1, r1, 48
	jal r0, .LBB25_7
.LBB25_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB25_6
.LBB25_4:
	addi r6, r1, 384
	bne r4, r6, .LBB25_19
.LBB25_5:
	ldw r1, r11+16
	addi r1, r1, 36
	jal r0, .LBB25_7
.LBB25_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB25_7:
	ldbu r3, r1+8
	andi r3, r3, 15
	addi r6, r0, 3
	beq r3, r6, .LBB25_11
.LBB25_8:
	addi r4, r0, 4
	beq r3, r4, .LBB25_23
.LBB25_9:
	addi r1, r0, 0
	beq r5, r1, .LBB25_28
.LBB25_10:
	addi r1, r0, 0
	stw r5+0, r1
	jal r0, .LBB25_28
.LBB25_11:
	add r13, r4, r0
	add r14, r5, r0
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaO_tostring
	ldw r1, r11+16
	ldw r1, r1+12
	blt r1, r12, .LBB25_13
.LBB25_12:
	add r3, r11, r0
	jal r31, luaC_step
.LBB25_13:
	ldw r4, r11+20
	add r5, r14, r0
	add r1, r13, r0
	blt r13, r12, .LBB25_16
.LBB25_14:
	ldw r3, r4+0
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	ldw r3, r11+12
	bltu r1, r3, .LBB25_23
.LBB25_15:
	ldw r1, r11+16
	addi r1, r1, 48
	addi r3, r0, 0
	bne r5, r3, .LBB25_24
	jal r0, .LBB25_27
.LBB25_16:
	lui r3, 1048572
	addi r6, r3, 385
	bge r1, r6, .LBB25_22
.LBB25_17:
	addi r6, r3, 384
	bne r1, r6, .LBB25_29
.LBB25_18:
	ldw r1, r11+16
	addi r1, r1, 36
	addi r3, r0, 0
	bne r5, r3, .LBB25_24
	jal r0, .LBB25_27
.LBB25_19:
	ldw r3, r3+0
	ldbu r7, r3+8
	addi r8, r0, 102
	bne r7, r8, .LBB25_2
.LBB25_20:
	sub r6, r6, r4
	ldw r3, r3+0
	ldbu r7, r3+6
	bgt r6, r7, .LBB25_2
.LBB25_21:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r6, r0, 12
	mul r1, r1, r6
	add r1, r3, r1
	addi r1, r1, 16
	jal r0, .LBB25_7
.LBB25_22:
	ldw r3, r11+12
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
.LBB25_23:
	addi r3, r0, 0
	beq r5, r3, .LBB25_27
.LBB25_24:
	ldw r4, r1+0
	ldbu r3, r4+7
	addi r6, r0, 255
	bne r3, r6, .LBB25_26
.LBB25_25:
	ldw r3, r4+12
.LBB25_26:
	stw r5+0, r3
.LBB25_27:
	ldw r1, r1+0
	addi r1, r1, 16
.LBB25_28:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB25_29:
	ldw r4, r4+0
	ldbu r7, r4+8
	addi r8, r0, 102
	bne r7, r8, .LBB25_15
.LBB25_30:
	sub r6, r6, r1
	ldw r4, r4+0
	ldbu r7, r4+6
	bgt r6, r7, .LBB25_15
.LBB25_31:
	addi r3, r3, 383
	sub r1, r3, r1
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r4, r1
	addi r1, r1, 16
	addi r3, r0, 0
	bne r5, r3, .LBB25_24
	jal r0, .LBB25_27
.Lfunc_end25:
	.size	lua_tolstring, .Lfunc_end25-lua_tolstring
                                        # -- End function
	.globl	lua_rawlen                      # -- Begin function lua_rawlen
	.p2align	2
	.type	lua_rawlen,@function
lua_rawlen:                             # @lua_rawlen
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r5, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB26_3
.LBB26_1:
	ldw r1, r5+0
	addi r5, r0, 12
	mul r4, r4, r5
	add r4, r1, r4
	ldw r1, r3+12
	bltu r4, r1, .LBB26_7
.LBB26_2:
	ldw r1, r3+16
	addi r4, r1, 48
	jal r0, .LBB26_7
.LBB26_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB26_6
.LBB26_4:
	addi r6, r1, 384
	bne r4, r6, .LBB26_14
.LBB26_5:
	ldw r1, r3+16
	addi r4, r1, 36
	jal r0, .LBB26_7
.LBB26_6:
	ldw r1, r3+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r4, r1, r3
.LBB26_7:
	ldbu r1, r4+8
	andi r3, r1, 63
	addi r1, r0, 0
	addi r3, r3, -4
	addi r5, r0, 16
	bgtu r3, r5, .LBB26_13
.LBB26_8:
	slli r3, r3, 2
	lui r5, %hi(.LJTI26_0)
	addi r5, r5, %lo(.LJTI26_0)
	add r3, r5, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB26_9:
	ldw r1, r4+0
	ldbu r1, r1+7
	jal r0, .LBB26_13
.LBB26_10:
	ldw r1, r4+0
	ldw r1, r1+8
	jal r0, .LBB26_13
.LBB26_11:
	ldw r3, r4+0
	jal r31, luaH_getn
	jal r0, .LBB26_13
.LBB26_12:
	ldw r1, r4+0
	ldw r1, r1+12
.LBB26_13:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB26_14:
	ldw r5, r5+0
	ldbu r7, r5+8
	addi r8, r0, 102
	bne r7, r8, .LBB26_2
.LBB26_15:
	sub r6, r6, r4
	ldw r5, r5+0
	ldbu r7, r5+6
	bgt r6, r7, .LBB26_2
.LBB26_16:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r5, r1
	addi r4, r1, 16
	jal r0, .LBB26_7
.Lfunc_end26:
	.size	lua_rawlen, .Lfunc_end26-lua_rawlen
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI26_0:
	.word	.LBB26_9
	.word	.LBB26_11
	.word	.LBB26_13
	.word	.LBB26_10
	.word	.LBB26_13
	.word	.LBB26_13
	.word	.LBB26_13
	.word	.LBB26_13
	.word	.LBB26_13
	.word	.LBB26_13
	.word	.LBB26_13
	.word	.LBB26_13
	.word	.LBB26_13
	.word	.LBB26_13
	.word	.LBB26_13
	.word	.LBB26_13
	.word	.LBB26_12
                                        # -- End function
	.text
	.globl	lua_tocfunction                 # -- Begin function lua_tocfunction
	.p2align	2
	.type	lua_tocfunction,@function
lua_tocfunction:                        # @lua_tocfunction
# %bb.0:
	ldw r5, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB27_3
.LBB27_1:
	ldw r1, r5+0
	addi r5, r0, 12
	mul r4, r4, r5
	add r1, r1, r4
	ldw r4, r3+12
	bltu r1, r4, .LBB27_7
.LBB27_2:
	ldw r1, r3+16
	addi r1, r1, 48
	jal r0, .LBB27_7
.LBB27_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB27_6
.LBB27_4:
	addi r6, r1, 384
	bne r4, r6, .LBB27_13
.LBB27_5:
	ldw r1, r3+16
	addi r1, r1, 36
	jal r0, .LBB27_7
.LBB27_6:
	ldw r1, r3+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB27_7:
	ldbu r3, r1+8
	addi r4, r0, 102
	beq r3, r4, .LBB27_10
.LBB27_8:
	addi r4, r0, 22
	bne r3, r4, .LBB27_11
.LBB27_9:
	ldw r1, r1+0
	jal r0, .LBB27_12
.LBB27_10:
	ldw r1, r1+0
	ldw r1, r1+12
	jal r0, .LBB27_12
.LBB27_11:
	addi r1, r0, 0
.LBB27_12:
	jalr r0, r31, 0
.LBB27_13:
	ldw r5, r5+0
	ldbu r7, r5+8
	addi r8, r0, 102
	bne r7, r8, .LBB27_2
.LBB27_14:
	sub r6, r6, r4
	ldw r5, r5+0
	ldbu r7, r5+6
	bgt r6, r7, .LBB27_2
.LBB27_15:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r5, r1
	addi r1, r1, 16
	jal r0, .LBB27_7
.Lfunc_end27:
	.size	lua_tocfunction, .Lfunc_end27-lua_tocfunction
                                        # -- End function
	.globl	lua_touserdata                  # -- Begin function lua_touserdata
	.p2align	2
	.type	lua_touserdata,@function
lua_touserdata:                         # @lua_touserdata
# %bb.0:
	ldw r5, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB28_3
.LBB28_1:
	ldw r1, r5+0
	addi r5, r0, 12
	mul r4, r4, r5
	add r1, r1, r4
	ldw r4, r3+12
	bltu r1, r4, .LBB28_7
.LBB28_2:
	ldw r1, r3+16
	addi r1, r1, 48
	jal r0, .LBB28_7
.LBB28_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB28_6
.LBB28_4:
	addi r6, r1, 384
	bne r4, r6, .LBB28_13
.LBB28_5:
	ldw r1, r3+16
	addi r1, r1, 36
	jal r0, .LBB28_7
.LBB28_6:
	ldw r1, r3+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB28_7:
	ldbu r3, r1+8
	andi r3, r3, 15
	addi r4, r0, 2
	beq r3, r4, .LBB28_10
.LBB28_8:
	addi r4, r0, 7
	bne r3, r4, .LBB28_11
.LBB28_9:
	ldw r1, r1+0
	ldhu r3, r1+6
	addi r4, r0, 0
	seq r5, r3, r4
	addi r6, r0, 12
	mul r3, r3, r6
	addi r3, r3, 20
	sub r4, r4, r5
	xori r5, r3, 16
	and r4, r5, r4
	xor r3, r3, r4
	add r1, r1, r3
	jal r0, .LBB28_12
.LBB28_10:
	ldw r1, r1+0
	jal r0, .LBB28_12
.LBB28_11:
	addi r1, r0, 0
.LBB28_12:
	jalr r0, r31, 0
.LBB28_13:
	ldw r5, r5+0
	ldbu r7, r5+8
	addi r8, r0, 102
	bne r7, r8, .LBB28_2
.LBB28_14:
	sub r6, r6, r4
	ldw r5, r5+0
	ldbu r7, r5+6
	bgt r6, r7, .LBB28_2
.LBB28_15:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r5, r1
	addi r1, r1, 16
	jal r0, .LBB28_7
.Lfunc_end28:
	.size	lua_touserdata, .Lfunc_end28-lua_touserdata
                                        # -- End function
	.globl	lua_tothread                    # -- Begin function lua_tothread
	.p2align	2
	.type	lua_tothread,@function
lua_tothread:                           # @lua_tothread
# %bb.0:
	ldw r5, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB29_3
.LBB29_1:
	ldw r1, r5+0
	addi r5, r0, 12
	mul r4, r4, r5
	add r1, r1, r4
	ldw r4, r3+12
	bltu r1, r4, .LBB29_7
.LBB29_2:
	ldw r1, r3+16
	addi r1, r1, 48
	jal r0, .LBB29_7
.LBB29_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB29_6
.LBB29_4:
	addi r6, r1, 384
	bne r4, r6, .LBB29_11
.LBB29_5:
	ldw r1, r3+16
	addi r1, r1, 36
	jal r0, .LBB29_7
.LBB29_6:
	ldw r1, r3+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB29_7:
	ldbu r3, r1+8
	addi r4, r0, 72
	bne r3, r4, .LBB29_9
.LBB29_8:
	ldw r1, r1+0
	jal r0, .LBB29_10
.LBB29_9:
	addi r1, r0, 0
.LBB29_10:
	jalr r0, r31, 0
.LBB29_11:
	ldw r5, r5+0
	ldbu r7, r5+8
	addi r8, r0, 102
	bne r7, r8, .LBB29_2
.LBB29_12:
	sub r6, r6, r4
	ldw r5, r5+0
	ldbu r7, r5+6
	bgt r6, r7, .LBB29_2
.LBB29_13:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r5, r1
	addi r1, r1, 16
	jal r0, .LBB29_7
.Lfunc_end29:
	.size	lua_tothread, .Lfunc_end29-lua_tothread
                                        # -- End function
	.globl	lua_topointer                   # -- Begin function lua_topointer
	.p2align	2
	.type	lua_topointer,@function
lua_topointer:                          # @lua_topointer
# %bb.0:
	ldw r5, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB30_3
.LBB30_1:
	ldw r1, r5+0
	addi r5, r0, 12
	mul r4, r4, r5
	add r4, r1, r4
	ldw r1, r3+12
	bltu r4, r1, .LBB30_7
.LBB30_2:
	ldw r1, r3+16
	addi r4, r1, 48
	jal r0, .LBB30_7
.LBB30_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB30_6
.LBB30_4:
	addi r6, r1, 384
	bne r4, r6, .LBB30_17
.LBB30_5:
	ldw r1, r3+16
	addi r4, r1, 36
	jal r0, .LBB30_7
.LBB30_6:
	ldw r1, r3+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r4, r1, r3
.LBB30_7:
	ldbu r1, r4+8
	andi r3, r1, 63
	addi r5, r0, 2
	beq r3, r5, .LBB30_11
.LBB30_8:
	addi r6, r0, 7
	beq r3, r6, .LBB30_11
.LBB30_9:
	addi r5, r0, 22
	beq r3, r5, .LBB30_14
.LBB30_10:
	andi r3, r1, 64
	addi r1, r0, 0
	bne r3, r1, .LBB30_14
	jal r0, .LBB30_16
.LBB30_11:
	andi r1, r1, 15
	beq r1, r5, .LBB30_14
.LBB30_12:
	addi r3, r0, 7
	bne r1, r3, .LBB30_15
.LBB30_13:
	ldw r1, r4+0
	ldhu r3, r1+6
	addi r4, r0, 0
	seq r5, r3, r4
	addi r6, r0, 12
	mul r3, r3, r6
	addi r3, r3, 20
	sub r4, r4, r5
	xori r5, r3, 16
	and r4, r5, r4
	xor r3, r3, r4
	add r1, r1, r3
	jal r0, .LBB30_16
.LBB30_14:
	ldw r1, r4+0
	jal r0, .LBB30_16
.LBB30_15:
	addi r1, r0, 0
.LBB30_16:
	jalr r0, r31, 0
.LBB30_17:
	ldw r5, r5+0
	ldbu r7, r5+8
	addi r8, r0, 102
	bne r7, r8, .LBB30_2
.LBB30_18:
	sub r6, r6, r4
	ldw r5, r5+0
	ldbu r7, r5+6
	bgt r6, r7, .LBB30_2
.LBB30_19:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r5, r1
	addi r4, r1, 16
	jal r0, .LBB30_7
.Lfunc_end30:
	.size	lua_topointer, .Lfunc_end30-lua_topointer
                                        # -- End function
	.globl	lua_pushnil                     # -- Begin function lua_pushnil
	.p2align	2
	.type	lua_pushnil,@function
lua_pushnil:                            # @lua_pushnil
# %bb.0:
	ldw r1, r3+12
	addi r4, r0, 0
	stb r1+8, r4
	ldw r1, r3+12
	addi r1, r1, 12
	stw r3+12, r1
	jalr r0, r31, 0
.Lfunc_end31:
	.size	lua_pushnil, .Lfunc_end31-lua_pushnil
                                        # -- End function
	.globl	lua_pushnumber                  # -- Begin function lua_pushnumber
	.p2align	2
	.type	lua_pushnumber,@function
lua_pushnumber:                         # @lua_pushnumber
# %bb.0:
	ldw r1, r3+12
	stw r1+4, r6
	stw r1+0, r5
	addi r4, r0, 19
	stb r1+8, r4
	ldw r1, r3+12
	addi r1, r1, 12
	stw r3+12, r1
	jalr r0, r31, 0
.Lfunc_end32:
	.size	lua_pushnumber, .Lfunc_end32-lua_pushnumber
                                        # -- End function
	.globl	lua_pushinteger                 # -- Begin function lua_pushinteger
	.p2align	2
	.type	lua_pushinteger,@function
lua_pushinteger:                        # @lua_pushinteger
# %bb.0:
	ldw r1, r3+12
	stw r1+0, r4
	addi r4, r0, 3
	stb r1+8, r4
	ldw r1, r3+12
	addi r1, r1, 12
	stw r3+12, r1
	jalr r0, r31, 0
.Lfunc_end33:
	.size	lua_pushinteger, .Lfunc_end33-lua_pushinteger
                                        # -- End function
	.globl	lua_pushlstring                 # -- Begin function lua_pushlstring
	.p2align	2
	.type	lua_pushlstring,@function
lua_pushlstring:                        # @lua_pushlstring
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
	addi r1, r0, 0
	beq r5, r1, .LBB34_2
.LBB34_1:
	add r3, r11, r0
	jal r31, luaS_newlstr
	jal r0, .LBB34_3
.LBB34_2:
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	add r3, r11, r0
	jal r31, luaS_new
.LBB34_3:
	add r12, r1, r0
	ldw r1, r11+12
	stw r1+0, r12
	ldbu r3, r12+4
	ori  r3, r3, 64
	stb r1+8, r3
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
	ldw r1, r11+16
	ldw r1, r1+12
	addi r3, r0, 1
	blt r1, r3, .LBB34_5
.LBB34_4:
	add r3, r11, r0
	jal r31, luaC_step
.LBB34_5:
	addi r1, r12, 16
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end34:
	.size	lua_pushlstring, .Lfunc_end34-lua_pushlstring
                                        # -- End function
	.globl	lua_pushstring                  # -- Begin function lua_pushstring
	.p2align	2
	.type	lua_pushstring,@function
lua_pushstring:                         # @lua_pushstring
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
	addi r12, r0, 0
	beq r4, r12, .LBB35_2
.LBB35_1:
	add r3, r11, r0
	jal r31, luaS_new
	ldw r3, r11+12
	stw r3+0, r1
	ldbu r4, r1+4
	ori  r4, r4, 64
	stb r3+8, r4
	addi r12, r1, 16
	jal r0, .LBB35_3
.LBB35_2:
	ldw r1, r11+12
	stb r1+8, r12
.LBB35_3:
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
	ldw r1, r11+16
	ldw r1, r1+12
	addi r3, r0, 1
	blt r1, r3, .LBB35_5
.LBB35_4:
	add r3, r11, r0
	jal r31, luaC_step
.LBB35_5:
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end35:
	.size	lua_pushstring, .Lfunc_end35-lua_pushstring
                                        # -- End function
	.globl	lua_pushvfstring                # -- Begin function lua_pushvfstring
	.p2align	2
	.type	lua_pushvfstring,@function
lua_pushvfstring:                       # @lua_pushvfstring
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	jal r31, luaO_pushvfstring
	ldw r4, r11+16
	ldw r4, r4+12
	addi r5, r0, 1
	blt r4, r5, .LBB36_2
.LBB36_1:
	add r3, r11, r0
	add r11, r1, r0
	jal r31, luaC_step
	add r1, r11, r0
.LBB36_2:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end36:
	.size	lua_pushvfstring, .Lfunc_end36-lua_pushvfstring
                                        # -- End function
	.globl	lua_pushfstring                 # -- Begin function lua_pushfstring
	.p2align	2
	.type	lua_pushfstring,@function
lua_pushfstring:                        # @lua_pushfstring
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 56
	stw fp+-28, r11
	stw fp+-32, lr
	add r11, r3, r0
	addi r1, fp, -24
	stw r1+20, r10
	stw r1+16, r9
	stw r1+12, r8
	stw r1+8, r7
	addi r3, r1, 4
	stw r3+0, r6
	stw r1+0, r5
	addi r3, fp, -36
	stw r3+0, r1
	add r3, r11, r0
	add r5, r1, r0
	jal r31, luaO_pushvfstring
	ldw r4, r11+16
	ldw r4, r4+12
	addi r5, r0, 1
	blt r4, r5, .LBB37_2
.LBB37_1:
	add r3, r11, r0
	add r11, r1, r0
	jal r31, luaC_step
	add r1, r11, r0
.LBB37_2:
	ldw lr, fp+-32
	ldw r11, fp+-28
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end37:
	.size	lua_pushfstring, .Lfunc_end37-lua_pushfstring
                                        # -- End function
	.globl	lua_pushcclosure                # -- Begin function lua_pushcclosure
	.p2align	2
	.type	lua_pushcclosure,@function
lua_pushcclosure:                       # @lua_pushcclosure
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
	add r13, r4, r0
	add r11, r3, r0
	addi r14, r0, 0
	beq r5, r14, .LBB38_5
.LBB38_1:
	add r12, r5, r0
	add r3, r11, r0
	add r4, r5, r0
	jal r31, luaF_newCclosure
	stw r1+12, r13
	ldw r3, r11+12
	sub r4, r14, r12
	addi r5, r0, 12
	mul r4, r4, r5
	add r3, r3, r4
	stw r11+12, r3
	mul r3, r12, r5
.LBB38_2:
	addi r12, r12, -1
	add r4, r1, r3
	ldw r5, r11+12
	add r5, r5, r3
	ldw r6, r5+-12
	ldw r7, r5+-8
	stw r4+8, r7
	stw r4+4, r6
	ldbu r5, r5+-4
	stb r4+12, r5
	addi r3, r3, -12
	bne r12, r14, .LBB38_2
.LBB38_3:
	ldw r3, r11+12
	stw r3+0, r1
	addi r1, r0, 102
	stb r3+8, r1
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
	ldw r1, r11+16
	ldw r1, r1+12
	addi r3, r0, 1
	blt r1, r3, .LBB38_6
.LBB38_4:
	add r3, r11, r0
	jal r31, luaC_step
	jal r0, .LBB38_6
.LBB38_5:
	ldw r1, r11+12
	stw r1+0, r13
	addi r3, r0, 22
	stb r1+8, r3
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
.LBB38_6:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end38:
	.size	lua_pushcclosure, .Lfunc_end38-lua_pushcclosure
                                        # -- End function
	.globl	lua_pushboolean                 # -- Begin function lua_pushboolean
	.p2align	2
	.type	lua_pushboolean,@function
lua_pushboolean:                        # @lua_pushboolean
# %bb.0:
	addi r1, r0, 0
	seq r4, r4, r1
	ldw r5, r3+12
	sub r1, r1, r4
	andi r1, r1, 16
	xori r1, r1, 17
	stb r5+8, r1
	ldw r1, r3+12
	addi r1, r1, 12
	stw r3+12, r1
	jalr r0, r31, 0
.Lfunc_end39:
	.size	lua_pushboolean, .Lfunc_end39-lua_pushboolean
                                        # -- End function
	.globl	lua_pushlightuserdata           # -- Begin function lua_pushlightuserdata
	.p2align	2
	.type	lua_pushlightuserdata,@function
lua_pushlightuserdata:                  # @lua_pushlightuserdata
# %bb.0:
	ldw r1, r3+12
	stw r1+0, r4
	addi r4, r0, 2
	stb r1+8, r4
	ldw r1, r3+12
	addi r1, r1, 12
	stw r3+12, r1
	jalr r0, r31, 0
.Lfunc_end40:
	.size	lua_pushlightuserdata, .Lfunc_end40-lua_pushlightuserdata
                                        # -- End function
	.globl	lua_pushthread                  # -- Begin function lua_pushthread
	.p2align	2
	.type	lua_pushthread,@function
lua_pushthread:                         # @lua_pushthread
# %bb.0:
	ldw r1, r3+12
	stw r1+0, r3
	addi r4, r0, 72
	stb r1+8, r4
	ldw r1, r3+12
	addi r1, r1, 12
	stw r3+12, r1
	ldw r1, r3+16
	ldw r1, r1+152
	seq r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end41:
	.size	lua_pushthread, .Lfunc_end41-lua_pushthread
                                        # -- End function
	.globl	lua_getglobal                   # -- Begin function lua_getglobal
	.p2align	2
	.type	lua_getglobal,@function
lua_getglobal:                          # @lua_getglobal
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
	ldw r1, r3+16
	ldw r1, r1+36
	ldw r13, r1+12
	addi r12, r13, 12
	jal r31, luaS_new
	ldbu r3, r13+20
	addi r4, r0, 69
	bne r3, r4, .LBB42_3
.LBB42_1:
	ldw r3, r12+0
	add r13, r1, r0
	add r4, r1, r0
	jal r31, luaH_getstr
	add r7, r1, r0
	ldbu r1, r1+8
	andi r1, r1, 15
	addi r3, r0, 0
	beq r1, r3, .LBB42_4
.LBB42_2:
	ldw r1, r11+12
	ldw r3, r7+0
	ldw r4, r7+4
	stw r1+4, r4
	stw r1+0, r3
	ldbu r3, r7+8
	stb r1+8, r3
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
	jal r0, .LBB42_6
.LBB42_3:
	addi r7, r0, 0
	jal r0, .LBB42_5
.LBB42_4:
	add r1, r13, r0
.LBB42_5:
	ldw r3, r11+12
	stw r3+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r3+8, r1
	ldw r5, r11+12
	addi r1, r5, 12
	stw r11+12, r1
	add r3, r11, r0
	add r4, r12, r0
	add r6, r5, r0
	jal r31, luaV_finishget
.LBB42_6:
	ldw r1, r11+12
	ldbu r1, r1+-4
	andi r1, r1, 15
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end42:
	.size	lua_getglobal, .Lfunc_end42-lua_getglobal
                                        # -- End function
	.globl	lua_gettable                    # -- Begin function lua_gettable
	.p2align	2
	.type	lua_gettable,@function
lua_gettable:                           # @lua_gettable
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
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB43_3
.LBB43_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r12, r1, r3
	ldw r1, r11+12
	bltu r12, r1, .LBB43_7
.LBB43_2:
	ldw r1, r11+16
	addi r12, r1, 48
	jal r0, .LBB43_7
.LBB43_3:
	lui r1, 1048572
	addi r5, r1, 385
	bge r4, r5, .LBB43_6
.LBB43_4:
	addi r5, r1, 384
	bne r4, r5, .LBB43_13
.LBB43_5:
	ldw r1, r11+16
	addi r12, r1, 36
	jal r0, .LBB43_7
.LBB43_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r12, r1, r3
.LBB43_7:
	ldbu r1, r12+8
	addi r3, r0, 69
	bne r1, r3, .LBB43_10
.LBB43_8:
	ldw r3, r12+0
	ldw r1, r11+12
	addi r4, r1, -12
	jal r31, luaH_get
	add r7, r1, r0
	ldbu r1, r1+8
	andi r1, r1, 15
	addi r3, r0, 0
	beq r1, r3, .LBB43_11
.LBB43_9:
	ldw r1, r11+12
	ldw r3, r7+0
	ldw r4, r7+4
	stw r1+-8, r4
	stw r1+-12, r3
	ldbu r3, r7+8
	stb r1+-4, r3
	jal r0, .LBB43_12
.LBB43_10:
	addi r7, r0, 0
.LBB43_11:
	ldw r1, r11+12
	addi r5, r1, -12
	add r3, r11, r0
	add r4, r12, r0
	add r6, r5, r0
	jal r31, luaV_finishget
.LBB43_12:
	ldw r1, r11+12
	ldbu r1, r1+-4
	andi r1, r1, 15
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB43_13:
	ldw r3, r3+0
	ldbu r6, r3+8
	addi r7, r0, 102
	bne r6, r7, .LBB43_2
.LBB43_14:
	sub r5, r5, r4
	ldw r3, r3+0
	ldbu r6, r3+6
	bgt r5, r6, .LBB43_2
.LBB43_15:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r12, r1, 16
	jal r0, .LBB43_7
.Lfunc_end43:
	.size	lua_gettable, .Lfunc_end43-lua_gettable
                                        # -- End function
	.globl	lua_getfield                    # -- Begin function lua_getfield
	.p2align	2
	.type	lua_getfield,@function
lua_getfield:                           # @lua_getfield
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
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB44_3
.LBB44_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r12, r1, r3
	ldw r1, r11+12
	bltu r12, r1, .LBB44_7
.LBB44_2:
	ldw r1, r11+16
	addi r12, r1, 48
	jal r0, .LBB44_7
.LBB44_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB44_6
.LBB44_4:
	addi r6, r1, 384
	bne r4, r6, .LBB44_14
.LBB44_5:
	ldw r1, r11+16
	addi r12, r1, 36
	jal r0, .LBB44_7
.LBB44_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r12, r1, r3
.LBB44_7:
	add r3, r11, r0
	add r4, r5, r0
	jal r31, luaS_new
	ldbu r3, r12+8
	addi r4, r0, 69
	bne r3, r4, .LBB44_10
.LBB44_8:
	ldw r3, r12+0
	add r13, r1, r0
	add r4, r1, r0
	jal r31, luaH_getstr
	add r7, r1, r0
	ldbu r1, r1+8
	andi r1, r1, 15
	addi r3, r0, 0
	beq r1, r3, .LBB44_11
.LBB44_9:
	ldw r1, r11+12
	ldw r3, r7+0
	ldw r4, r7+4
	stw r1+4, r4
	stw r1+0, r3
	ldbu r3, r7+8
	stb r1+8, r3
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
	jal r0, .LBB44_13
.LBB44_10:
	addi r7, r0, 0
	jal r0, .LBB44_12
.LBB44_11:
	add r1, r13, r0
.LBB44_12:
	ldw r3, r11+12
	stw r3+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r3+8, r1
	ldw r5, r11+12
	addi r1, r5, 12
	stw r11+12, r1
	add r3, r11, r0
	add r4, r12, r0
	add r6, r5, r0
	jal r31, luaV_finishget
.LBB44_13:
	ldw r1, r11+12
	ldbu r1, r1+-4
	andi r1, r1, 15
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB44_14:
	ldw r3, r3+0
	ldbu r7, r3+8
	addi r8, r0, 102
	bne r7, r8, .LBB44_2
.LBB44_15:
	sub r6, r6, r4
	ldw r3, r3+0
	ldbu r7, r3+6
	bgt r6, r7, .LBB44_2
.LBB44_16:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r12, r1, 16
	jal r0, .LBB44_7
.Lfunc_end44:
	.size	lua_getfield, .Lfunc_end44-lua_getfield
                                        # -- End function
	.globl	lua_geti                        # -- Begin function lua_geti
	.p2align	2
	.type	lua_geti,@function
lua_geti:                               # @lua_geti
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
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB45_3
.LBB45_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r4, r1, r3
	ldw r1, r11+12
	bltu r4, r1, .LBB45_7
.LBB45_2:
	ldw r1, r11+16
	addi r4, r1, 48
	jal r0, .LBB45_7
.LBB45_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB45_6
.LBB45_4:
	addi r6, r1, 384
	bne r4, r6, .LBB45_16
.LBB45_5:
	ldw r1, r11+16
	addi r4, r1, 36
	jal r0, .LBB45_7
.LBB45_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r4, r1, r3
.LBB45_7:
	ldbu r1, r4+8
	addi r3, r0, 69
	bne r1, r3, .LBB45_10
.LBB45_8:
	addi r1, r5, -1
	ldw r3, r4+0
	ldw r6, r3+8
	bgeu r1, r6, .LBB45_11
.LBB45_9:
	ldw r3, r3+12
	addi r6, r0, 12
	mul r1, r1, r6
	add r7, r3, r1
	jal r0, .LBB45_12
.LBB45_10:
	addi r7, r0, 0
	jal r0, .LBB45_14
.LBB45_11:
	add r12, r4, r0
	add r4, r5, r0
	add r13, r5, r0
	jal r31, luaH_getint
	add r4, r12, r0
	add r5, r13, r0
	add r7, r1, r0
.LBB45_12:
	ldbu r1, r7+8
	andi r1, r1, 15
	addi r3, r0, 0
	beq r1, r3, .LBB45_14
.LBB45_13:
	ldw r1, r11+12
	ldw r3, r7+0
	ldw r4, r7+4
	stw r1+4, r4
	stw r1+0, r3
	ldbu r3, r7+8
	stb r1+8, r3
	jal r0, .LBB45_15
.LBB45_14:
	addi r1, fp, -28
	stw r1+0, r5
	addi r3, r0, 3
	stb r1+8, r3
	ldw r6, r11+12
	add r3, r11, r0
	add r5, r1, r0
	jal r31, luaV_finishget
.LBB45_15:
	ldw r1, r11+12
	addi r3, r1, 12
	stw r11+12, r3
	ldbu r1, r1+8
	andi r1, r1, 15
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB45_16:
	ldw r3, r3+0
	ldbu r7, r3+8
	addi r8, r0, 102
	bne r7, r8, .LBB45_2
.LBB45_17:
	sub r6, r6, r4
	ldw r3, r3+0
	ldbu r7, r3+6
	bgt r6, r7, .LBB45_2
.LBB45_18:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r4, r1, 16
	jal r0, .LBB45_7
.Lfunc_end45:
	.size	lua_geti, .Lfunc_end45-lua_geti
                                        # -- End function
	.globl	lua_rawget                      # -- Begin function lua_rawget
	.p2align	2
	.type	lua_rawget,@function
lua_rawget:                             # @lua_rawget
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB46_3
.LBB46_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
	ldw r3, r11+12
	bltu r1, r3, .LBB46_7
.LBB46_2:
	ldw r1, r11+16
	addi r1, r1, 48
	jal r0, .LBB46_7
.LBB46_3:
	lui r1, 1048572
	addi r5, r1, 385
	bge r4, r5, .LBB46_6
.LBB46_4:
	addi r5, r1, 384
	bne r4, r5, .LBB46_10
.LBB46_5:
	ldw r1, r11+16
	addi r1, r1, 36
	jal r0, .LBB46_7
.LBB46_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB46_7:
	ldw r3, r1+0
	ldw r1, r11+12
	addi r4, r1, -12
	jal r31, luaH_get
	ldw r3, r11+12
	addi r4, r3, -12
	stw r11+12, r4
	ldbu r5, r1+8
	andi r6, r5, 15
	addi r5, r0, 0
	beq r6, r5, .LBB46_9
.LBB46_8:
	ldw r5, r1+0
	ldw r6, r1+4
	stw r4+4, r6
	stw r4+0, r5
	ldbu r5, r1+8
.LBB46_9:
	stb r3+-4, r5
	ldw r1, r11+12
	addi r3, r1, 12
	stw r11+12, r3
	ldbu r1, r1+8
	andi r1, r1, 15
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB46_10:
	ldw r3, r3+0
	ldbu r6, r3+8
	addi r7, r0, 102
	bne r6, r7, .LBB46_2
.LBB46_11:
	sub r5, r5, r4
	ldw r3, r3+0
	ldbu r6, r3+6
	bgt r5, r6, .LBB46_2
.LBB46_12:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r1, r1, 16
	jal r0, .LBB46_7
.Lfunc_end46:
	.size	lua_rawget, .Lfunc_end46-lua_rawget
                                        # -- End function
	.globl	lua_rawgeti                     # -- Begin function lua_rawgeti
	.p2align	2
	.type	lua_rawgeti,@function
lua_rawgeti:                            # @lua_rawgeti
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB47_3
.LBB47_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
	ldw r3, r11+12
	bltu r1, r3, .LBB47_7
.LBB47_2:
	ldw r1, r11+16
	addi r1, r1, 48
	jal r0, .LBB47_7
.LBB47_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB47_6
.LBB47_4:
	addi r6, r1, 384
	bne r4, r6, .LBB47_10
.LBB47_5:
	ldw r1, r11+16
	addi r1, r1, 36
	jal r0, .LBB47_7
.LBB47_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB47_7:
	ldw r3, r1+0
	add r4, r5, r0
	jal r31, luaH_getint
	ldbu r3, r1+8
	andi r5, r3, 15
	ldw r3, r11+12
	addi r4, r0, 0
	beq r5, r4, .LBB47_9
.LBB47_8:
	ldw r4, r1+0
	ldw r5, r1+4
	stw r3+4, r5
	stw r3+0, r4
	ldbu r4, r1+8
.LBB47_9:
	stb r3+8, r4
	ldw r1, r11+12
	addi r3, r1, 12
	stw r11+12, r3
	ldbu r1, r1+8
	andi r1, r1, 15
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB47_10:
	ldw r3, r3+0
	ldbu r7, r3+8
	addi r8, r0, 102
	bne r7, r8, .LBB47_2
.LBB47_11:
	sub r6, r6, r4
	ldw r3, r3+0
	ldbu r7, r3+6
	bgt r6, r7, .LBB47_2
.LBB47_12:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r1, r1, 16
	jal r0, .LBB47_7
.Lfunc_end47:
	.size	lua_rawgeti, .Lfunc_end47-lua_rawgeti
                                        # -- End function
	.globl	lua_rawgetp                     # -- Begin function lua_rawgetp
	.p2align	2
	.type	lua_rawgetp,@function
lua_rawgetp:                            # @lua_rawgetp
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB48_3
.LBB48_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
	ldw r3, r11+12
	bltu r1, r3, .LBB48_7
.LBB48_2:
	ldw r1, r11+16
	addi r1, r1, 48
	jal r0, .LBB48_7
.LBB48_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB48_6
.LBB48_4:
	addi r6, r1, 384
	bne r4, r6, .LBB48_10
.LBB48_5:
	ldw r1, r11+16
	addi r1, r1, 36
	jal r0, .LBB48_7
.LBB48_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB48_7:
	ldw r3, r1+0
	addi r4, fp, -20
	stw r4+0, r5
	addi r1, r0, 2
	stb r4+8, r1
	jal r31, luaH_get
	ldbu r3, r1+8
	andi r5, r3, 15
	ldw r3, r11+12
	addi r4, r0, 0
	beq r5, r4, .LBB48_9
.LBB48_8:
	ldw r4, r1+0
	ldw r5, r1+4
	stw r3+4, r5
	stw r3+0, r4
	ldbu r4, r1+8
.LBB48_9:
	stb r3+8, r4
	ldw r1, r11+12
	addi r3, r1, 12
	stw r11+12, r3
	ldbu r1, r1+8
	andi r1, r1, 15
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB48_10:
	ldw r3, r3+0
	ldbu r7, r3+8
	addi r8, r0, 102
	bne r7, r8, .LBB48_2
.LBB48_11:
	sub r6, r6, r4
	ldw r3, r3+0
	ldbu r7, r3+6
	bgt r6, r7, .LBB48_2
.LBB48_12:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r1, r1, 16
	jal r0, .LBB48_7
.Lfunc_end48:
	.size	lua_rawgetp, .Lfunc_end48-lua_rawgetp
                                        # -- End function
	.globl	lua_createtable                 # -- Begin function lua_createtable
	.p2align	2
	.type	lua_createtable,@function
lua_createtable:                        # @lua_createtable
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
	add r13, r4, r0
	add r11, r3, r0
	jal r31, luaH_new
	ldw r3, r11+12
	stw r3+0, r1
	addi r4, r0, 69
	stb r3+8, r4
	ldw r3, r11+12
	addi r3, r3, 12
	stw r11+12, r3
	addi r3, r0, 0
	bgt r13, r3, .LBB49_2
.LBB49_1:
	addi r3, r0, 1
	blt r12, r3, .LBB49_3
.LBB49_2:
	add r3, r11, r0
	add r4, r1, r0
	add r5, r13, r0
	add r6, r12, r0
	jal r31, luaH_resize
.LBB49_3:
	ldw r1, r11+16
	ldw r1, r1+12
	addi r3, r0, 1
	blt r1, r3, .LBB49_5
.LBB49_4:
	add r3, r11, r0
	jal r31, luaC_step
.LBB49_5:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end49:
	.size	lua_createtable, .Lfunc_end49-lua_createtable
                                        # -- End function
	.globl	lua_getmetatable                # -- Begin function lua_getmetatable
	.p2align	2
	.type	lua_getmetatable,@function
lua_getmetatable:                       # @lua_getmetatable
# %bb.0:
	ldw r6, r3+20
	addi r5, r0, 1
	blt r4, r5, .LBB50_3
.LBB50_1:
	ldw r1, r6+0
	addi r6, r0, 12
	mul r4, r4, r6
	add r1, r1, r4
	ldw r4, r3+12
	bltu r1, r4, .LBB50_7
.LBB50_2:
	ldw r1, r3+16
	addi r1, r1, 48
	jal r0, .LBB50_7
.LBB50_3:
	lui r1, 1048572
	addi r7, r1, 385
	bge r4, r7, .LBB50_6
.LBB50_4:
	addi r7, r1, 384
	bne r4, r7, .LBB50_15
.LBB50_5:
	ldw r1, r3+16
	addi r1, r1, 36
	jal r0, .LBB50_7
.LBB50_6:
	ldw r1, r3+12
	addi r6, r0, 12
	mul r4, r4, r6
	add r1, r1, r4
.LBB50_7:
	ldbu r4, r1+8
	andi r4, r4, 15
	addi r6, r0, 7
	beq r4, r6, .LBB50_10
.LBB50_8:
	addi r6, r0, 5
	bne r4, r6, .LBB50_11
.LBB50_9:
	ldw r1, r1+0
	addi r1, r1, 24
	jal r0, .LBB50_12
.LBB50_10:
	ldw r1, r1+0
	addi r1, r1, 12
	jal r0, .LBB50_12
.LBB50_11:
	ldw r1, r3+16
	slli r4, r4, 2
	add r1, r1, r4
	addi r1, r1, 260
.LBB50_12:
	ldw r4, r1+0
	addi r1, r0, 0
	beq r4, r1, .LBB50_14
.LBB50_13:
	ldw r1, r3+12
	stw r1+0, r4
	addi r4, r0, 69
	stb r1+8, r4
	ldw r1, r3+12
	addi r1, r1, 12
	stw r3+12, r1
	add r1, r5, r0
.LBB50_14:
	jalr r0, r31, 0
.LBB50_15:
	ldw r6, r6+0
	ldbu r8, r6+8
	addi r9, r0, 102
	bne r8, r9, .LBB50_2
.LBB50_16:
	sub r7, r7, r4
	ldw r6, r6+0
	ldbu r8, r6+6
	bgt r7, r8, .LBB50_2
.LBB50_17:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r6, r1
	addi r1, r1, 16
	jal r0, .LBB50_7
.Lfunc_end50:
	.size	lua_getmetatable, .Lfunc_end50-lua_getmetatable
                                        # -- End function
	.globl	lua_getiuservalue               # -- Begin function lua_getiuservalue
	.p2align	2
	.type	lua_getiuservalue,@function
lua_getiuservalue:                      # @lua_getiuservalue
# %bb.0:
	ldw r7, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB51_3
.LBB51_1:
	ldw r6, r7+0
	addi r7, r0, 12
	mul r4, r4, r7
	add r4, r6, r4
	ldw r6, r3+12
	bltu r4, r6, .LBB51_7
.LBB51_2:
	ldw r4, r3+16
	addi r4, r4, 48
	bge r5, r1, .LBB51_8
	jal r0, .LBB51_9
.LBB51_3:
	lui r6, 1048572
	addi r8, r6, 385
	bge r4, r8, .LBB51_6
.LBB51_4:
	addi r8, r6, 384
	bne r4, r8, .LBB51_12
.LBB51_5:
	ldw r4, r3+16
	addi r4, r4, 36
	bge r5, r1, .LBB51_8
	jal r0, .LBB51_9
.LBB51_6:
	ldw r6, r3+12
	addi r7, r0, 12
	mul r4, r4, r7
	add r4, r6, r4
.LBB51_7:
	blt r5, r1, .LBB51_9
.LBB51_8:
	ldw r1, r4+0
	ldhu r4, r1+6
	ble r5, r4, .LBB51_10
.LBB51_9:
	ldw r1, r3+12
	addi r4, r0, 0
	stb r1+8, r4
	addi r1, r0, -1
	jal r0, .LBB51_11
.LBB51_10:
	ldw r4, r3+12
	addi r6, r0, 12
	mul r5, r5, r6
	add r1, r1, r5
	ldw r5, r1+8
	ldw r6, r1+12
	stw r4+4, r6
	stw r4+0, r5
	ldbu r1, r1+16
	stb r4+8, r1
	ldw r1, r3+12
	ldbu r1, r1+8
	andi r1, r1, 15
.LBB51_11:
	ldw r4, r3+12
	addi r4, r4, 12
	stw r3+12, r4
	jalr r0, r31, 0
.LBB51_12:
	ldw r7, r7+0
	ldbu r9, r7+8
	addi r10, r0, 102
	bne r9, r10, .LBB51_2
.LBB51_13:
	sub r8, r8, r4
	ldw r7, r7+0
	ldbu r9, r7+6
	bgt r8, r9, .LBB51_2
.LBB51_14:
	addi r6, r6, 383
	sub r4, r6, r4
	addi r6, r0, 12
	mul r4, r4, r6
	add r4, r7, r4
	addi r4, r4, 16
	bge r5, r1, .LBB51_8
	jal r0, .LBB51_9
.Lfunc_end51:
	.size	lua_getiuservalue, .Lfunc_end51-lua_getiuservalue
                                        # -- End function
	.globl	lua_setglobal                   # -- Begin function lua_setglobal
	.p2align	2
	.type	lua_setglobal,@function
lua_setglobal:                          # @lua_setglobal
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	add r5, r4, r0
	ldw r1, r3+16
	ldw r1, r1+36
	ldw r1, r1+12
	addi r4, r1, 12
	jal r31, auxsetstr
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end52:
	.size	lua_setglobal, .Lfunc_end52-lua_setglobal
                                        # -- End function
	.p2align	2                               # -- Begin function auxsetstr
	.type	auxsetstr,@function
auxsetstr:                              # @auxsetstr
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
	add r4, r5, r0
	jal r31, luaS_new
	ldbu r3, r12+8
	addi r4, r0, 69
	bne r3, r4, .LBB53_7
.LBB53_1:
	ldw r3, r12+0
	add r13, r1, r0
	add r4, r1, r0
	jal r31, luaH_getstr
	add r7, r1, r0
	ldbu r1, r1+8
	andi r3, r1, 15
	addi r1, r0, 0
	beq r3, r1, .LBB53_8
.LBB53_2:
	ldw r3, r11+12
	ldw r4, r3+-12
	ldw r5, r3+-8
	stw r7+4, r5
	stw r7+0, r4
	ldbu r3, r3+-4
	stb r7+8, r3
	ldw r3, r11+12
	ldbu r4, r3+-4
	andi r4, r4, 64
	beq r4, r1, .LBB53_6
.LBB53_3:
	ldw r4, r12+0
	ldbu r5, r4+5
	andi r5, r5, 32
	beq r5, r1, .LBB53_6
.LBB53_4:
	ldw r3, r3+-12
	ldbu r3, r3+5
	andi r3, r3, 24
	beq r3, r1, .LBB53_6
.LBB53_5:
	add r3, r11, r0
	jal r31, luaC_barrierback_
.LBB53_6:
	ldw r1, r11+12
	addi r1, r1, -12
	jal r0, .LBB53_10
.LBB53_7:
	addi r7, r0, 0
	jal r0, .LBB53_9
.LBB53_8:
	add r1, r13, r0
.LBB53_9:
	ldw r3, r11+12
	stw r3+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r3+8, r1
	ldw r5, r11+12
	addi r1, r5, 12
	stw r11+12, r1
	addi r6, r5, -12
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaV_finishset
	ldw r1, r11+12
	addi r1, r1, -24
.LBB53_10:
	stw r11+12, r1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end53:
	.size	auxsetstr, .Lfunc_end53-auxsetstr
                                        # -- End function
	.globl	lua_settable                    # -- Begin function lua_settable
	.p2align	2
	.type	lua_settable,@function
lua_settable:                           # @lua_settable
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
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB54_3
.LBB54_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r12, r1, r3
	ldw r1, r11+12
	bltu r12, r1, .LBB54_7
.LBB54_2:
	ldw r1, r11+16
	addi r12, r1, 48
	jal r0, .LBB54_7
.LBB54_3:
	lui r1, 1048572
	addi r5, r1, 385
	bge r4, r5, .LBB54_6
.LBB54_4:
	addi r5, r1, 384
	bne r4, r5, .LBB54_16
.LBB54_5:
	ldw r1, r11+16
	addi r12, r1, 36
	jal r0, .LBB54_7
.LBB54_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r12, r1, r3
.LBB54_7:
	ldbu r1, r12+8
	addi r3, r0, 69
	bne r1, r3, .LBB54_13
.LBB54_8:
	ldw r3, r12+0
	ldw r1, r11+12
	addi r4, r1, -24
	jal r31, luaH_get
	add r7, r1, r0
	ldbu r1, r1+8
	andi r3, r1, 15
	addi r1, r0, 0
	beq r3, r1, .LBB54_14
.LBB54_9:
	ldw r3, r11+12
	ldw r4, r3+-12
	ldw r5, r3+-8
	stw r7+4, r5
	stw r7+0, r4
	ldbu r3, r3+-4
	stb r7+8, r3
	ldw r3, r11+12
	ldbu r4, r3+-4
	andi r4, r4, 64
	beq r4, r1, .LBB54_15
.LBB54_10:
	ldw r4, r12+0
	ldbu r5, r4+5
	andi r5, r5, 32
	beq r5, r1, .LBB54_15
.LBB54_11:
	ldw r3, r3+-12
	ldbu r3, r3+5
	andi r3, r3, 24
	beq r3, r1, .LBB54_15
.LBB54_12:
	add r3, r11, r0
	jal r31, luaC_barrierback_
	jal r0, .LBB54_15
.LBB54_13:
	addi r7, r0, 0
.LBB54_14:
	ldw r1, r11+12
	addi r5, r1, -24
	addi r6, r1, -12
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaV_finishset
.LBB54_15:
	ldw r1, r11+12
	addi r1, r1, -24
	stw r11+12, r1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB54_16:
	ldw r3, r3+0
	ldbu r6, r3+8
	addi r7, r0, 102
	bne r6, r7, .LBB54_2
.LBB54_17:
	sub r5, r5, r4
	ldw r3, r3+0
	ldbu r6, r3+6
	bgt r5, r6, .LBB54_2
.LBB54_18:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r12, r1, 16
	jal r0, .LBB54_7
.Lfunc_end54:
	.size	lua_settable, .Lfunc_end54-lua_settable
                                        # -- End function
	.globl	lua_setfield                    # -- Begin function lua_setfield
	.p2align	2
	.type	lua_setfield,@function
lua_setfield:                           # @lua_setfield
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r6, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB55_3
.LBB55_1:
	ldw r1, r6+0
	addi r6, r0, 12
	mul r4, r4, r6
	add r4, r1, r4
	ldw r1, r3+12
	bltu r4, r1, .LBB55_7
.LBB55_2:
	ldw r1, r3+16
	addi r4, r1, 48
	jal r0, .LBB55_7
.LBB55_3:
	lui r1, 1048572
	addi r7, r1, 385
	bge r4, r7, .LBB55_6
.LBB55_4:
	addi r7, r1, 384
	bne r4, r7, .LBB55_8
.LBB55_5:
	ldw r1, r3+16
	addi r4, r1, 36
	jal r0, .LBB55_7
.LBB55_6:
	ldw r1, r3+12
	addi r6, r0, 12
	mul r4, r4, r6
	add r4, r1, r4
.LBB55_7:
	jal r31, auxsetstr
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB55_8:
	ldw r6, r6+0
	ldbu r8, r6+8
	addi r9, r0, 102
	bne r8, r9, .LBB55_2
.LBB55_9:
	sub r7, r7, r4
	ldw r6, r6+0
	ldbu r8, r6+6
	bgt r7, r8, .LBB55_2
.LBB55_10:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r6, r1
	addi r4, r1, 16
	jal r0, .LBB55_7
.Lfunc_end55:
	.size	lua_setfield, .Lfunc_end55-lua_setfield
                                        # -- End function
	.globl	lua_seti                        # -- Begin function lua_seti
	.p2align	2
	.type	lua_seti,@function
lua_seti:                               # @lua_seti
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
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB56_3
.LBB56_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r4, r1, r3
	ldw r1, r11+12
	bltu r4, r1, .LBB56_7
.LBB56_2:
	ldw r1, r11+16
	addi r4, r1, 48
	jal r0, .LBB56_7
.LBB56_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB56_6
.LBB56_4:
	addi r6, r1, 384
	bne r4, r6, .LBB56_19
.LBB56_5:
	ldw r1, r11+16
	addi r4, r1, 36
	jal r0, .LBB56_7
.LBB56_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r4, r1, r3
.LBB56_7:
	ldbu r1, r4+8
	addi r3, r0, 69
	bne r1, r3, .LBB56_10
.LBB56_8:
	addi r1, r5, -1
	ldw r3, r4+0
	ldw r6, r3+8
	bgeu r1, r6, .LBB56_11
.LBB56_9:
	ldw r3, r3+12
	addi r6, r0, 12
	mul r1, r1, r6
	add r7, r3, r1
	jal r0, .LBB56_12
.LBB56_10:
	addi r7, r0, 0
	jal r0, .LBB56_17
.LBB56_11:
	add r12, r4, r0
	add r4, r5, r0
	add r13, r5, r0
	jal r31, luaH_getint
	add r4, r12, r0
	add r5, r13, r0
	add r7, r1, r0
.LBB56_12:
	ldbu r1, r7+8
	andi r3, r1, 15
	addi r1, r0, 0
	beq r3, r1, .LBB56_17
.LBB56_13:
	ldw r3, r11+12
	ldw r5, r3+-12
	ldw r6, r3+-8
	stw r7+4, r6
	stw r7+0, r5
	ldbu r3, r3+-4
	stb r7+8, r3
	ldw r3, r11+12
	ldbu r5, r3+-4
	andi r5, r5, 64
	beq r5, r1, .LBB56_18
.LBB56_14:
	ldw r4, r4+0
	ldbu r5, r4+5
	andi r5, r5, 32
	beq r5, r1, .LBB56_18
.LBB56_15:
	ldw r3, r3+-12
	ldbu r3, r3+5
	andi r3, r3, 24
	beq r3, r1, .LBB56_18
.LBB56_16:
	add r3, r11, r0
	jal r31, luaC_barrierback_
	jal r0, .LBB56_18
.LBB56_17:
	addi r1, fp, -28
	stw r1+0, r5
	addi r3, r0, 3
	stb r1+8, r3
	ldw r3, r11+12
	addi r6, r3, -12
	add r3, r11, r0
	add r5, r1, r0
	jal r31, luaV_finishset
.LBB56_18:
	ldw r1, r11+12
	addi r1, r1, -12
	stw r11+12, r1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB56_19:
	ldw r3, r3+0
	ldbu r7, r3+8
	addi r8, r0, 102
	bne r7, r8, .LBB56_2
.LBB56_20:
	sub r6, r6, r4
	ldw r3, r3+0
	ldbu r7, r3+6
	bgt r6, r7, .LBB56_2
.LBB56_21:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r4, r1, 16
	jal r0, .LBB56_7
.Lfunc_end56:
	.size	lua_seti, .Lfunc_end56-lua_seti
                                        # -- End function
	.globl	lua_rawset                      # -- Begin function lua_rawset
	.p2align	2
	.type	lua_rawset,@function
lua_rawset:                             # @lua_rawset
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r3+12
	addi r5, r1, -24
	addi r6, r0, 2
	jal r31, aux_rawset
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end57:
	.size	lua_rawset, .Lfunc_end57-lua_rawset
                                        # -- End function
	.p2align	2                               # -- Begin function aux_rawset
	.type	aux_rawset,@function
aux_rawset:                             # @aux_rawset
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
	add r12, r6, r0
	add r11, r3, r0
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB58_3
.LBB58_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
	ldw r3, r11+12
	bltu r1, r3, .LBB58_7
.LBB58_2:
	ldw r1, r11+16
	addi r1, r1, 48
	jal r0, .LBB58_7
.LBB58_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB58_6
.LBB58_4:
	addi r6, r1, 384
	bne r4, r6, .LBB58_12
.LBB58_5:
	ldw r1, r11+16
	addi r1, r1, 36
	jal r0, .LBB58_7
.LBB58_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB58_7:
	ldw r13, r1+0
	ldw r1, r11+12
	addi r6, r1, -12
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaH_set
	ldbu r1, r13+6
	andi r1, r1, 192
	stb r13+6, r1
	ldw r1, r11+12
	ldbu r3, r1+-4
	andi r3, r3, 64
	addi r14, r0, 0
	beq r3, r14, .LBB58_11
.LBB58_8:
	ldbu r3, r13+5
	andi r3, r3, 32
	beq r3, r14, .LBB58_11
.LBB58_9:
	ldw r1, r1+-12
	ldbu r1, r1+5
	andi r1, r1, 24
	beq r1, r14, .LBB58_11
.LBB58_10:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaC_barrierback_
.LBB58_11:
	ldw r1, r11+12
	sub r3, r14, r12
	addi r4, r0, 12
	mul r3, r3, r4
	add r1, r1, r3
	stw r11+12, r1
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB58_12:
	ldw r3, r3+0
	ldbu r7, r3+8
	addi r8, r0, 102
	bne r7, r8, .LBB58_2
.LBB58_13:
	sub r6, r6, r4
	ldw r3, r3+0
	ldbu r7, r3+6
	bgt r6, r7, .LBB58_2
.LBB58_14:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r1, r1, 16
	jal r0, .LBB58_7
.Lfunc_end58:
	.size	aux_rawset, .Lfunc_end58-aux_rawset
                                        # -- End function
	.globl	lua_rawsetp                     # -- Begin function lua_rawsetp
	.p2align	2
	.type	lua_rawsetp,@function
lua_rawsetp:                            # @lua_rawsetp
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r1, fp, -16
	stw r1+0, r5
	addi r5, r0, 2
	stb r1+8, r5
	addi r6, r0, 1
	add r5, r1, r0
	jal r31, aux_rawset
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end59:
	.size	lua_rawsetp, .Lfunc_end59-lua_rawsetp
                                        # -- End function
	.globl	lua_rawseti                     # -- Begin function lua_rawseti
	.p2align	2
	.type	lua_rawseti,@function
lua_rawseti:                            # @lua_rawseti
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
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB60_3
.LBB60_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
	ldw r3, r11+12
	bltu r1, r3, .LBB60_7
.LBB60_2:
	ldw r1, r11+16
	addi r1, r1, 48
	jal r0, .LBB60_7
.LBB60_3:
	lui r1, 1048572
	addi r6, r1, 385
	bge r4, r6, .LBB60_6
.LBB60_4:
	addi r6, r1, 384
	bne r4, r6, .LBB60_12
.LBB60_5:
	ldw r1, r11+16
	addi r1, r1, 36
	jal r0, .LBB60_7
.LBB60_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB60_7:
	ldw r12, r1+0
	ldw r1, r11+12
	addi r6, r1, -12
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaH_setint
	ldw r1, r11+12
	ldbu r3, r1+-4
	andi r4, r3, 64
	addi r3, r0, 0
	beq r4, r3, .LBB60_11
.LBB60_8:
	ldbu r4, r12+5
	andi r4, r4, 32
	beq r4, r3, .LBB60_11
.LBB60_9:
	ldw r1, r1+-12
	ldbu r1, r1+5
	andi r1, r1, 24
	beq r1, r3, .LBB60_11
.LBB60_10:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaC_barrierback_
.LBB60_11:
	ldw r1, r11+12
	addi r1, r1, -12
	stw r11+12, r1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB60_12:
	ldw r3, r3+0
	ldbu r7, r3+8
	addi r8, r0, 102
	bne r7, r8, .LBB60_2
.LBB60_13:
	sub r6, r6, r4
	ldw r3, r3+0
	ldbu r7, r3+6
	bgt r6, r7, .LBB60_2
.LBB60_14:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r1, r1, 16
	jal r0, .LBB60_7
.Lfunc_end60:
	.size	lua_rawseti, .Lfunc_end60-lua_rawseti
                                        # -- End function
	.globl	lua_setmetatable                # -- Begin function lua_setmetatable
	.p2align	2
	.type	lua_setmetatable,@function
lua_setmetatable:                       # @lua_setmetatable
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
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB61_3
.LBB61_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r13, r1, r3
	ldw r1, r11+12
	bltu r13, r1, .LBB61_7
.LBB61_2:
	ldw r1, r11+16
	addi r13, r1, 48
	jal r0, .LBB61_7
.LBB61_3:
	lui r1, 1048572
	addi r5, r1, 385
	bge r4, r5, .LBB61_6
.LBB61_4:
	addi r5, r1, 384
	bne r4, r5, .LBB61_19
.LBB61_5:
	ldw r1, r11+16
	addi r13, r1, 36
	jal r0, .LBB61_7
.LBB61_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r13, r1, r3
.LBB61_7:
	ldw r1, r11+12
	ldbu r3, r1+-4
	andi r3, r3, 15
	addi r5, r0, 0
	beq r3, r5, .LBB61_9
.LBB61_8:
	ldw r5, r1+-12
.LBB61_9:
	ldbu r1, r13+8
	andi r1, r1, 15
	addi r3, r0, 7
	beq r1, r3, .LBB61_12
.LBB61_10:
	addi r3, r0, 5
	bne r1, r3, .LBB61_17
.LBB61_11:
	ldw r1, r13+0
	stw r1+24, r5
	addi r1, r0, 0
	bne r5, r1, .LBB61_13
	jal r0, .LBB61_18
.LBB61_12:
	ldw r1, r13+0
	stw r1+12, r5
	addi r1, r0, 0
	beq r5, r1, .LBB61_18
.LBB61_13:
	ldw r4, r13+0
	ldbu r3, r4+5
	andi r3, r3, 32
	beq r3, r1, .LBB61_16
.LBB61_14:
	ldbu r3, r5+5
	andi r3, r3, 24
	beq r3, r1, .LBB61_16
.LBB61_15:
	add r3, r11, r0
	add r12, r5, r0
	jal r31, luaC_barrier_
	add r5, r12, r0
.LBB61_16:
	ldw r4, r13+0
	add r3, r11, r0
	jal r31, luaC_checkfinalizer
	jal r0, .LBB61_18
.LBB61_17:
	ldw r3, r11+16
	slli r1, r1, 2
	add r1, r3, r1
	stw r1+260, r5
.LBB61_18:
	ldw r1, r11+12
	addi r1, r1, -12
	stw r11+12, r1
	addi r1, r0, 1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB61_19:
	ldw r3, r3+0
	ldbu r6, r3+8
	addi r7, r0, 102
	bne r6, r7, .LBB61_2
.LBB61_20:
	sub r5, r5, r4
	ldw r3, r3+0
	ldbu r6, r3+6
	bgt r5, r6, .LBB61_2
.LBB61_21:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r13, r1, 16
	jal r0, .LBB61_7
.Lfunc_end61:
	.size	lua_setmetatable, .Lfunc_end61-lua_setmetatable
                                        # -- End function
	.globl	lua_setiuservalue               # -- Begin function lua_setiuservalue
	.p2align	2
	.type	lua_setiuservalue,@function
lua_setiuservalue:                      # @lua_setiuservalue
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	ldw r7, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB62_3
.LBB62_1:
	ldw r6, r7+0
	addi r7, r0, 12
	mul r4, r4, r7
	add r4, r6, r4
	ldw r6, r3+12
	bltu r4, r6, .LBB62_7
.LBB62_2:
	ldw r4, r3+16
	addi r4, r4, 48
	jal r0, .LBB62_7
.LBB62_3:
	lui r6, 1048572
	addi r8, r6, 385
	bge r4, r8, .LBB62_6
.LBB62_4:
	addi r8, r6, 384
	bne r4, r8, .LBB62_14
.LBB62_5:
	ldw r4, r3+16
	addi r4, r4, 36
	jal r0, .LBB62_7
.LBB62_6:
	ldw r6, r3+12
	addi r7, r0, 12
	mul r4, r4, r7
	add r4, r6, r4
.LBB62_7:
	addi r5, r5, -1
	ldw r6, r4+0
	ldhu r7, r6+6
	bgeu r5, r7, .LBB62_12
.LBB62_8:
	addi r7, r0, 12
	mul r5, r5, r7
	add r5, r6, r5
	ldw r6, r3+12
	ldw r7, r6+-12
	ldw r8, r6+-8
	stw r5+24, r8
	stw r5+20, r7
	ldbu r6, r6+-4
	stb r5+28, r6
	ldw r5, r3+12
	ldbu r6, r5+-4
	andi r7, r6, 64
	addi r6, r0, 0
	beq r7, r6, .LBB62_13
.LBB62_9:
	ldw r4, r4+0
	ldbu r7, r4+5
	andi r7, r7, 32
	beq r7, r6, .LBB62_13
.LBB62_10:
	ldw r5, r5+-12
	ldbu r5, r5+5
	andi r5, r5, 24
	beq r5, r6, .LBB62_13
.LBB62_11:
	add r11, r3, r0
	add r12, r1, r0
	jal r31, luaC_barrierback_
	add r1, r12, r0
	add r3, r11, r0
	jal r0, .LBB62_13
.LBB62_12:
	addi r1, r0, 0
.LBB62_13:
	ldw r4, r3+12
	addi r4, r4, -12
	stw r3+12, r4
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB62_14:
	ldw r7, r7+0
	ldbu r9, r7+8
	addi r10, r0, 102
	bne r9, r10, .LBB62_2
.LBB62_15:
	sub r8, r8, r4
	ldw r7, r7+0
	ldbu r9, r7+6
	bgt r8, r9, .LBB62_2
.LBB62_16:
	addi r6, r6, 383
	sub r4, r6, r4
	addi r6, r0, 12
	mul r4, r4, r6
	add r4, r7, r4
	addi r4, r4, 16
	jal r0, .LBB62_7
.Lfunc_end62:
	.size	lua_setiuservalue, .Lfunc_end62-lua_setiuservalue
                                        # -- End function
	.globl	lua_callk                       # -- Begin function lua_callk
	.p2align	2
	.type	lua_callk,@function
lua_callk:                              # @lua_callk
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
	add r11, r3, r0
	ldw r1, r3+12
	addi r13, r0, -1
	xor r3, r4, r13
	addi r4, r0, 12
	mul r3, r3, r4
	add r4, r1, r3
	addi r1, r0, 0
	beq r7, r1, .LBB63_3
.LBB63_1:
	ldw r1, r11+96
	lui r3, 16
	addi r3, r3, -1
	bgtu r1, r3, .LBB63_3
.LBB63_2:
	ldw r1, r11+20
	stw r1+16, r7
	ldw r1, r11+20
	stw r1+24, r6
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaD_call
	bgt r12, r13, .LBB63_6
	jal r0, .LBB63_4
.LBB63_3:
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaD_callnoyield
	bgt r12, r13, .LBB63_6
.LBB63_4:
	ldw r1, r11+20
	ldw r4, r1+4
	ldw r3, r11+12
	bgeu r4, r3, .LBB63_6
.LBB63_5:
	stw r1+4, r3
.LBB63_6:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end63:
	.size	lua_callk, .Lfunc_end63-lua_callk
                                        # -- End function
	.globl	lua_pcallk                      # -- Begin function lua_pcallk
	.p2align	2
	.type	lua_pcallk,@function
lua_pcallk:                             # @lua_pcallk
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
	add r11, r3, r0
	addi r3, r0, 0
	add r1, r3, r0
	beq r6, r3, .LBB64_5
.LBB64_1:
	addi r1, r0, 1
	blt r6, r1, .LBB64_3
.LBB64_2:
	ldw r1, r11+20
	jal r0, .LBB64_4
.LBB64_3:
	addi r1, r11, 12
.LBB64_4:
	ldw r1, r1+0
	addi r5, r0, 12
	mul r5, r6, r5
	add r1, r1, r5
	ldw r5, r11+28
	sub r1, r1, r5
.LBB64_5:
	ldw r5, r11+12
	addi r13, r0, -1
	xor r4, r4, r13
	addi r6, r0, 12
	mul r4, r4, r6
	add r4, r5, r4
	addi r5, fp, -32
	stw r5+0, r4
	beq r8, r3, .LBB64_8
.LBB64_6:
	ldw r3, r11+96
	lui r14, 16
	bgeu r3, r14, .LBB64_8
.LBB64_7:
	ldw r15, r11+20
	stw r15+16, r8
	stw r15+24, r7
	ldw r3, r11+28
	sub r3, r4, r3
	stw r15+28, r3
	ldw r3, r11+92
	stw r15+20, r3
	stw r11+92, r1
	ldhu r1, r15+34
	addi r3, r14, -18
	and r1, r1, r3
	ldbu r3, r11+7
	or  r1, r1, r3
	ori  r1, r1, 16
	sth r15+34, r1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaD_call
	ldhu r1, r15+34
	addi r3, r14, -17
	and r1, r1, r3
	sth r15+34, r1
	ldw r1, r15+20
	stw r11+92, r1
	addi r1, r0, 0
	bgt r12, r13, .LBB64_11
	jal r0, .LBB64_9
.LBB64_8:
	stw r5+4, r12
	ldw r3, r11+28
	sub r6, r4, r3
	lui r4, %hi(f_call)
	addi r4, r4, %lo(f_call)
	add r3, r11, r0
	add r7, r1, r0
	jal r31, luaD_pcall
	bgt r12, r13, .LBB64_11
.LBB64_9:
	ldw r3, r11+20
	ldw r5, r3+4
	ldw r4, r11+12
	bgeu r5, r4, .LBB64_11
.LBB64_10:
	stw r3+4, r4
.LBB64_11:
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
.Lfunc_end64:
	.size	lua_pcallk, .Lfunc_end64-lua_pcallk
                                        # -- End function
	.p2align	2                               # -- Begin function f_call
	.type	f_call,@function
f_call:                                 # @f_call
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r4+0
	ldw r5, r4+4
	add r4, r1, r0
	jal r31, luaD_callnoyield
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end65:
	.size	f_call, .Lfunc_end65-f_call
                                        # -- End function
	.globl	lua_load                        # -- Begin function lua_load
	.p2align	2
	.type	lua_load,@function
lua_load:                               # @lua_load
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
	stw fp+-24, lr
	add r12, r7, r0
	add r1, r5, r0
	add r5, r4, r0
	add r11, r3, r0
	addi r15, r0, 0
	seq r3, r6, r15
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	xor r4, r6, r4
	sub r3, r15, r3
	and r3, r4, r3
	xor r13, r6, r3
	addi r14, fp, -44
	add r3, r11, r0
	add r4, r14, r0
	add r6, r1, r0
	jal r31, luaZ_init
	add r3, r11, r0
	add r4, r14, r0
	add r5, r13, r0
	add r6, r12, r0
	jal r31, luaD_protectedparser
	beq r1, r15, .LBB66_2
.LBB66_1:
	ldw lr, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.LBB66_2:
	ldw r3, r11+12
	ldw r3, r3+-12
	ldbu r4, r3+6
	beq r4, r15, .LBB66_1
.LBB66_3:
	ldw r4, r11+16
	ldw r4, r4+36
	ldw r5, r4+12
	ldw r4, r3+16
	ldw r4, r4+8
	ldw r6, r5+12
	ldw r7, r5+16
	stw r4+4, r7
	stw r4+0, r6
	ldbu r6, r5+20
	stb r4+8, r6
	ldbu r4, r5+20
	andi r4, r4, 64
	beq r4, r15, .LBB66_1
.LBB66_4:
	ldw r4, r3+16
	ldbu r3, r4+5
	andi r3, r3, 32
	beq r3, r15, .LBB66_1
.LBB66_5:
	ldw r5, r5+12
	ldbu r3, r5+5
	andi r3, r3, 24
	beq r3, r15, .LBB66_1
.LBB66_6:
	add r3, r11, r0
	add r11, r1, r0
	jal r31, luaC_barrier_
	add r1, r11, r0
	jal r0, .LBB66_1
.Lfunc_end66:
	.size	lua_load, .Lfunc_end66-lua_load
                                        # -- End function
	.globl	lua_dump                        # -- Begin function lua_dump
	.p2align	2
	.type	lua_dump,@function
lua_dump:                               # @lua_dump
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	add r7, r6, r0
	add r6, r5, r0
	add r5, r4, r0
	ldw r1, r3+12
	ldbu r4, r1+-4
	addi r8, r0, 70
	bne r4, r8, .LBB67_2
.LBB67_1:
	ldw r1, r1+-12
	ldw r4, r1+12
	jal r31, luaU_dump
	jal r0, .LBB67_3
.LBB67_2:
	addi r1, r0, 1
.LBB67_3:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end67:
	.size	lua_dump, .Lfunc_end67-lua_dump
                                        # -- End function
	.globl	lua_status                      # -- Begin function lua_status
	.p2align	2
	.type	lua_status,@function
lua_status:                             # @lua_status
# %bb.0:
	ldbu r1, r3+6
	jalr r0, r31, 0
.Lfunc_end68:
	.size	lua_status, .Lfunc_end68-lua_status
                                        # -- End function
	.globl	lua_gc                          # -- Begin function lua_gc
	.p2align	2
	.type	lua_gc,@function
lua_gc:                                 # @lua_gc
# %bb.0:
	addi sp, sp, -72
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 72
	stw fp+-28, r11
	stw fp+-32, r12
	stw fp+-36, r13
	stw fp+-40, r14
	stw fp+-44, r15
	stw fp+-48, lr
	addi r1, fp, -24
	stw r1+20, r10
	stw r1+16, r9
	stw r1+12, r8
	stw r1+8, r7
	addi r7, r1, 4
	stw r7+0, r6
	stw r1+0, r5
	ldw r11, r3+16
	ldbu r5, r11+70
	andi r5, r5, 2
	addi r13, r0, 0
	bne r5, r13, .LBB69_4
.LBB69_1:
	addi r5, fp, -52
	stw r5+0, r1
	addi r1, r0, -1
	addi r6, r0, 11
	bgtu r4, r6, .LBB69_5
.LBB69_2:
	slli r4, r4, 2
	lui r6, %hi(.LJTI69_0)
	addi r6, r6, %lo(.LJTI69_0)
	add r4, r6, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB69_3:
	addi r1, r0, 1
	stb r11+70, r1
	add r1, r13, r0
	jal r0, .LBB69_5
.LBB69_4:
	addi r1, r0, -1
.LBB69_5:
	ldw lr, fp+-48
	ldw r15, fp+-44
	ldw r14, fp+-40
	ldw r13, fp+-36
	ldw r12, fp+-32
	ldw r11, fp+-28
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB69_6:
	ldw r7, r5+0
	ldw r6, r7+0
	addi r1, r7, 4
	stw r5+0, r1
	ldw r4, r7+4
	addi r1, r7, 8
	stw r5+0, r1
	ldw r1, r7+8
	addi r7, r7, 12
	stw r5+0, r7
	ldbu r5, r11+66
	addi r7, r0, 1
	bne r5, r7, .LBB69_19
.LBB69_7:
	addi r5, r0, 10
	jal r0, .LBB69_20
.LBB69_8:
	ldw r1, r11+8
	ldw r3, r11+12
	add r1, r3, r1
	andi r1, r1, 1023
	jal r0, .LBB69_5
.LBB69_9:
	add r15, r3, r0
	ldw r3, r5+0
	ldw r1, r3+0
	addi r3, r3, 4
	stw r5+0, r3
	ldbu r14, r11+70
	stb r11+70, r13
	beq r1, r13, .LBB69_34
.LBB69_10:
	slli r1, r1, 10
	ldw r3, r11+12
	add r12, r3, r1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaE_setdebt
	add r3, r15, r0
	ldw r1, r15+16
	ldw r1, r1+12
	addi r4, r0, 1
	bge r1, r4, .LBB69_35
	jal r0, .LBB69_36
.LBB69_11:
	ldw r1, r11+8
	ldw r3, r11+12
	add r1, r3, r1
	srli r1, r1, 10
	jal r0, .LBB69_5
.LBB69_12:
	addi r11, r0, 0
	add r4, r11, r0
	jal r31, luaC_fullgc
	add r1, r11, r0
	jal r0, .LBB69_5
.LBB69_13:
	addi r12, r0, 0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaE_setdebt
	add r1, r12, r0
	stb r11+70, r12
	jal r0, .LBB69_5
.LBB69_14:
	ldw r1, r5+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r5+0, r1
	ldbu r1, r11+72
	slli r1, r1, 2
	srai r4, r3, 31
	srli r4, r4, 30
	add r3, r3, r4
	srli r3, r3, 2
	stb r11+72, r3
	jal r0, .LBB69_5
.LBB69_15:
	ldw r1, r5+0
	ldw r3, r1+0
	addi r1, r1, 4
	stw r5+0, r1
	ldbu r1, r11+73
	slli r1, r1, 2
	srai r4, r3, 31
	srli r4, r4, 30
	add r3, r3, r4
	srli r3, r3, 2
	stb r11+73, r3
	jal r0, .LBB69_5
.LBB69_16:
	ldw r6, r5+0
	ldw r4, r6+0
	addi r1, r6, 4
	stw r5+0, r1
	ldw r1, r6+4
	addi r6, r6, 8
	stw r5+0, r6
	ldbu r5, r11+66
	addi r6, r0, 1
	bne r5, r6, .LBB69_27
.LBB69_17:
	addi r12, r0, 10
	jal r0, .LBB69_28
.LBB69_18:
	ldbu r1, r11+70
	seq r1, r1, r13
	jal r0, .LBB69_5
.LBB69_19:
	ldw r5, r11+20
	seq r5, r5, r13
	ori  r5, r5, 10
.LBB69_20:
	beq r6, r13, .LBB69_22
.LBB69_21:
	srai r7, r6, 31
	srli r7, r7, 30
	add r6, r6, r7
	srli r6, r6, 2
	stb r11+72, r6
.LBB69_22:
	beq r4, r13, .LBB69_24
.LBB69_23:
	srai r6, r4, 31
	srli r6, r6, 30
	add r4, r4, r6
	srli r4, r4, 2
	stb r11+73, r4
.LBB69_24:
	add r12, r5, r0
	beq r1, r13, .LBB69_26
.LBB69_25:
	stb r11+74, r1
.LBB69_26:
	addi r4, r0, 0
	jal r0, .LBB69_33
.LBB69_27:
	ldw r5, r11+20
	seq r5, r5, r13
	ori  r12, r5, 10
.LBB69_28:
	beq r4, r13, .LBB69_30
.LBB69_29:
	stb r11+68, r4
.LBB69_30:
	beq r1, r13, .LBB69_32
.LBB69_31:
	srai r4, r1, 31
	srli r4, r4, 30
	add r1, r1, r4
	srli r1, r1, 2
	stb r11+69, r1
.LBB69_32:
	addi r4, r0, 1
.LBB69_33:
	jal r31, luaC_changemode
	add r1, r12, r0
	jal r0, .LBB69_5
.LBB69_34:
	addi r4, r0, 0
	add r3, r11, r0
	jal r31, luaE_setdebt
	addi r12, r0, 1
	add r3, r15, r0
.LBB69_35:
	jal r31, luaC_step
.LBB69_36:
	stb r11+70, r14
	addi r3, r0, 1
	add r1, r13, r0
	blt r12, r3, .LBB69_5
.LBB69_37:
	ldbu r1, r11+65
	addi r3, r0, 8
	seq r1, r1, r3
	jal r0, .LBB69_5
.Lfunc_end69:
	.size	lua_gc, .Lfunc_end69-lua_gc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI69_0:
	.word	.LBB69_3
	.word	.LBB69_13
	.word	.LBB69_12
	.word	.LBB69_11
	.word	.LBB69_8
	.word	.LBB69_9
	.word	.LBB69_14
	.word	.LBB69_15
	.word	.LBB69_5
	.word	.LBB69_18
	.word	.LBB69_16
	.word	.LBB69_6
                                        # -- End function
	.text
	.globl	lua_error                       # -- Begin function lua_error
	.p2align	2
	.type	lua_error,@function
lua_error:                              # @lua_error
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r3+12
	ldbu r4, r1+-4
	addi r5, r0, 68
	bne r4, r5, .LBB70_3
.LBB70_1:
	ldw r1, r1+-12
	ldw r4, r3+16
	ldw r4, r4+156
	bne r1, r4, .LBB70_3
.LBB70_2:
	addi r4, r0, 4
	jal r31, luaD_throw
.LBB70_3:
	jal r31, luaG_errormsg
.Lfunc_end70:
	.size	lua_error, .Lfunc_end70-lua_error
                                        # -- End function
	.globl	lua_next                        # -- Begin function lua_next
	.p2align	2
	.type	lua_next,@function
lua_next:                               # @lua_next
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB71_3
.LBB71_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
	ldw r3, r11+12
	bltu r1, r3, .LBB71_7
.LBB71_2:
	ldw r1, r11+16
	addi r1, r1, 48
	jal r0, .LBB71_7
.LBB71_3:
	lui r1, 1048572
	addi r5, r1, 385
	bge r4, r5, .LBB71_6
.LBB71_4:
	addi r5, r1, 384
	bne r4, r5, .LBB71_8
.LBB71_5:
	ldw r1, r11+16
	addi r1, r1, 36
	jal r0, .LBB71_7
.LBB71_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB71_7:
	ldw r4, r1+0
	ldw r1, r11+12
	addi r5, r1, -12
	add r3, r11, r0
	jal r31, luaH_next
	addi r3, r0, 0
	seq r4, r1, r3
	ldw r5, r11+12
	sub r3, r3, r4
	addi r4, r0, -8
	and r3, r3, r4
	xori r3, r3, 12
	add r3, r5, r3
	stw r11+12, r3
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB71_8:
	ldw r3, r3+0
	ldbu r6, r3+8
	addi r7, r0, 102
	bne r6, r7, .LBB71_2
.LBB71_9:
	sub r5, r5, r4
	ldw r3, r3+0
	ldbu r6, r3+6
	bgt r5, r6, .LBB71_2
.LBB71_10:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r1, r1, 16
	jal r0, .LBB71_7
.Lfunc_end71:
	.size	lua_next, .Lfunc_end71-lua_next
                                        # -- End function
	.globl	lua_toclose                     # -- Begin function lua_toclose
	.p2align	2
	.type	lua_toclose,@function
lua_toclose:                            # @lua_toclose
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
	addi r1, r0, 1
	blt r4, r1, .LBB72_2
.LBB72_1:
	ldw r1, r11+20
	jal r0, .LBB72_3
.LBB72_2:
	addi r1, r11, 12
.LBB72_3:
	ldw r1, r1+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r4, r1, r3
	ldw r1, r11+20
	ldh r12, r1+32
	add r3, r11, r0
	jal r31, luaF_newtbcupval
	addi r1, r0, -1
	blt r12, r1, .LBB72_5
.LBB72_4:
	addi r1, r0, -3
	sub r1, r1, r12
	ldw r3, r11+20
	sth r3+32, r1
.LBB72_5:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end72:
	.size	lua_toclose, .Lfunc_end72-lua_toclose
                                        # -- End function
	.globl	lua_concat                      # -- Begin function lua_concat
	.p2align	2
	.type	lua_concat,@function
lua_concat:                             # @lua_concat
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
	blt r4, r12, .LBB73_2
.LBB73_1:
	add r3, r11, r0
	jal r31, luaV_concat
	jal r0, .LBB73_3
.LBB73_2:
	ldw r13, r11+12
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, luaS_newlstr
	stw r13+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r13+8, r1
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
.LBB73_3:
	ldw r1, r11+16
	ldw r1, r1+12
	blt r1, r12, .LBB73_5
.LBB73_4:
	add r3, r11, r0
	jal r31, luaC_step
.LBB73_5:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end73:
	.size	lua_concat, .Lfunc_end73-lua_concat
                                        # -- End function
	.globl	lua_len                         # -- Begin function lua_len
	.p2align	2
	.type	lua_len,@function
lua_len:                                # @lua_len
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	ldw r3, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB74_3
.LBB74_1:
	ldw r1, r3+0
	addi r3, r0, 12
	mul r3, r4, r3
	add r5, r1, r3
	ldw r1, r11+12
	bltu r5, r1, .LBB74_7
.LBB74_2:
	ldw r1, r11+16
	addi r5, r1, 48
	jal r0, .LBB74_7
.LBB74_3:
	lui r1, 1048572
	addi r5, r1, 385
	bge r4, r5, .LBB74_6
.LBB74_4:
	addi r5, r1, 384
	bne r4, r5, .LBB74_8
.LBB74_5:
	ldw r1, r11+16
	addi r5, r1, 36
	jal r0, .LBB74_7
.LBB74_6:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r5, r1, r3
.LBB74_7:
	ldw r4, r11+12
	add r3, r11, r0
	jal r31, luaV_objlen
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB74_8:
	ldw r3, r3+0
	ldbu r6, r3+8
	addi r7, r0, 102
	bne r6, r7, .LBB74_2
.LBB74_9:
	sub r5, r5, r4
	ldw r3, r3+0
	ldbu r6, r3+6
	bgt r5, r6, .LBB74_2
.LBB74_10:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	addi r5, r1, 16
	jal r0, .LBB74_7
.Lfunc_end74:
	.size	lua_len, .Lfunc_end74-lua_len
                                        # -- End function
	.globl	lua_getallocf                   # -- Begin function lua_getallocf
	.p2align	2
	.type	lua_getallocf,@function
lua_getallocf:                          # @lua_getallocf
# %bb.0:
	addi r1, r0, 0
	beq r4, r1, .LBB75_2
.LBB75_1:
	ldw r1, r3+16
	ldw r1, r1+4
	stw r4+0, r1
.LBB75_2:
	ldw r1, r3+16
	ldw r1, r1+0
	jalr r0, r31, 0
.Lfunc_end75:
	.size	lua_getallocf, .Lfunc_end75-lua_getallocf
                                        # -- End function
	.globl	lua_setallocf                   # -- Begin function lua_setallocf
	.p2align	2
	.type	lua_setallocf,@function
lua_setallocf:                          # @lua_setallocf
# %bb.0:
	ldw r1, r3+16
	stw r1+4, r5
	stw r1+0, r4
	jalr r0, r31, 0
.Lfunc_end76:
	.size	lua_setallocf, .Lfunc_end76-lua_setallocf
                                        # -- End function
	.globl	lua_setwarnf                    # -- Begin function lua_setwarnf
	.p2align	2
	.type	lua_setwarnf,@function
lua_setwarnf:                           # @lua_setwarnf
# %bb.0:
	ldw r1, r3+16
	stw r1+724, r5
	stw r1+720, r4
	jalr r0, r31, 0
.Lfunc_end77:
	.size	lua_setwarnf, .Lfunc_end77-lua_setwarnf
                                        # -- End function
	.globl	lua_warning                     # -- Begin function lua_warning
	.p2align	2
	.type	lua_warning,@function
lua_warning:                            # @lua_warning
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	jal r31, luaE_warning
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end78:
	.size	lua_warning, .Lfunc_end78-lua_warning
                                        # -- End function
	.globl	lua_newuserdatauv               # -- Begin function lua_newuserdatauv
	.p2align	2
	.type	lua_newuserdatauv,@function
lua_newuserdatauv:                      # @lua_newuserdatauv
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	jal r31, luaS_newudata
	add r3, r11, r0
	add r11, r1, r0
	ldw r1, r3+12
	stw r1+0, r11
	addi r4, r0, 71
	stb r1+8, r4
	ldw r1, r3+12
	addi r1, r1, 12
	stw r3+12, r1
	ldw r1, r3+16
	ldw r1, r1+12
	addi r4, r0, 1
	blt r1, r4, .LBB79_2
.LBB79_1:
	jal r31, luaC_step
.LBB79_2:
	ldhu r1, r11+6
	addi r3, r0, 0
	seq r4, r1, r3
	addi r5, r0, 12
	mul r1, r1, r5
	addi r1, r1, 20
	sub r3, r3, r4
	xori r4, r1, 16
	and r3, r4, r3
	xor r1, r1, r3
	add r1, r11, r1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end79:
	.size	lua_newuserdatauv, .Lfunc_end79-lua_newuserdatauv
                                        # -- End function
	.globl	lua_getupvalue                  # -- Begin function lua_getupvalue
	.p2align	2
	.type	lua_getupvalue,@function
lua_getupvalue:                         # @lua_getupvalue
# %bb.0:
	ldw r6, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB80_3
.LBB80_1:
	ldw r1, r6+0
	addi r6, r0, 12
	mul r4, r4, r6
	add r7, r1, r4
	ldw r1, r3+12
	bltu r7, r1, .LBB80_7
.LBB80_2:
	ldw r1, r3+16
	addi r7, r1, 48
	jal r0, .LBB80_7
.LBB80_3:
	lui r1, 1048572
	addi r7, r1, 385
	bge r4, r7, .LBB80_6
.LBB80_4:
	addi r7, r1, 384
	bne r4, r7, .LBB80_16
.LBB80_5:
	ldw r1, r3+16
	addi r7, r1, 36
	jal r0, .LBB80_7
.LBB80_6:
	ldw r1, r3+12
	addi r6, r0, 12
	mul r4, r4, r6
	add r7, r1, r4
.LBB80_7:
	ldbu r1, r7+8
	andi r8, r1, 63
	addi r4, r0, 0
	addi r1, r0, 6
	beq r8, r1, .LBB80_11
.LBB80_8:
	addi r9, r0, 38
	add r6, r4, r0
	add r1, r4, r0
	bne r8, r9, .LBB80_13
.LBB80_9:
	ldw r7, r7+0
	addi r5, r5, -1
	ldbu r8, r7+6
	add r6, r4, r0
	add r1, r4, r0
	bgeu r5, r8, .LBB80_13
.LBB80_10:
	addi r1, r0, 12
	mul r1, r5, r1
	add r1, r7, r1
	addi r6, r1, 16
	lui r1, %hi(.L.str)
	addi r1, r1, %lo(.L.str)
	jal r0, .LBB80_13
.LBB80_11:
	ldw r8, r7+0
	ldw r7, r8+12
	addi r5, r5, -1
	ldw r9, r7+12
	add r6, r4, r0
	add r1, r4, r0
	bgeu r5, r9, .LBB80_13
.LBB80_12:
	slli r1, r5, 2
	add r1, r8, r1
	ldw r1, r1+16
	ldw r6, r1+8
	ldw r1, r7+60
	slli r5, r5, 3
	add r1, r1, r5
	ldw r1, r1+0
	addi r5, r0, 0
	seq r7, r1, r5
	addi r1, r1, 16
	sub r5, r5, r7
	lui r7, %hi(.L.str.2)
	addi r7, r7, %lo(.L.str.2)
	xor r7, r1, r7
	and r5, r7, r5
	xor r1, r1, r5
.LBB80_13:
	beq r1, r4, .LBB80_15
.LBB80_14:
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
.LBB80_15:
	jalr r0, r31, 0
.LBB80_16:
	ldw r6, r6+0
	ldbu r8, r6+8
	addi r9, r0, 102
	bne r8, r9, .LBB80_2
.LBB80_17:
	sub r7, r7, r4
	ldw r6, r6+0
	ldbu r8, r6+6
	bgt r7, r8, .LBB80_2
.LBB80_18:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r6, r1
	addi r7, r1, 16
	jal r0, .LBB80_7
.Lfunc_end80:
	.size	lua_getupvalue, .Lfunc_end80-lua_getupvalue
                                        # -- End function
	.globl	lua_setupvalue                  # -- Begin function lua_setupvalue
	.p2align	2
	.type	lua_setupvalue,@function
lua_setupvalue:                         # @lua_setupvalue
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r6, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB81_3
.LBB81_1:
	ldw r1, r6+0
	addi r6, r0, 12
	mul r4, r4, r6
	add r8, r1, r4
	ldw r1, r3+12
	bltu r8, r1, .LBB81_7
.LBB81_2:
	ldw r1, r3+16
	addi r8, r1, 48
	jal r0, .LBB81_7
.LBB81_3:
	lui r1, 1048572
	addi r7, r1, 385
	bge r4, r7, .LBB81_6
.LBB81_4:
	addi r7, r1, 384
	bne r4, r7, .LBB81_19
.LBB81_5:
	ldw r1, r3+16
	addi r8, r1, 36
	jal r0, .LBB81_7
.LBB81_6:
	ldw r1, r3+12
	addi r6, r0, 12
	mul r4, r4, r6
	add r8, r1, r4
.LBB81_7:
	ldbu r1, r8+8
	andi r9, r1, 63
	addi r6, r0, 0
	addi r1, r0, 6
	beq r9, r1, .LBB81_11
.LBB81_8:
	addi r10, r0, 38
	add r7, r6, r0
	add r4, r6, r0
	add r1, r6, r0
	bne r9, r10, .LBB81_13
.LBB81_9:
	ldw r8, r8+0
	addi r5, r5, -1
	ldbu r9, r8+6
	add r7, r6, r0
	add r4, r6, r0
	add r1, r6, r0
	bgeu r5, r9, .LBB81_13
.LBB81_10:
	addi r1, r0, 12
	mul r1, r5, r1
	add r1, r8, r1
	addi r7, r1, 16
	lui r1, %hi(.L.str)
	addi r1, r1, %lo(.L.str)
	add r4, r8, r0
	jal r0, .LBB81_13
.LBB81_11:
	ldw r9, r8+0
	ldw r8, r9+12
	addi r5, r5, -1
	ldw r10, r8+12
	add r7, r6, r0
	add r4, r6, r0
	add r1, r6, r0
	bgeu r5, r10, .LBB81_13
.LBB81_12:
	slli r1, r5, 2
	add r1, r9, r1
	ldw r4, r1+16
	ldw r7, r4+8
	ldw r1, r8+60
	slli r5, r5, 3
	add r1, r1, r5
	ldw r1, r1+0
	addi r5, r0, 0
	seq r8, r1, r5
	addi r1, r1, 16
	sub r5, r5, r8
	lui r8, %hi(.L.str.2)
	addi r8, r8, %lo(.L.str.2)
	xor r8, r1, r8
	and r5, r8, r5
	xor r1, r1, r5
.LBB81_13:
	beq r1, r6, .LBB81_18
.LBB81_14:
	ldw r5, r3+12
	addi r6, r5, -12
	stw r3+12, r6
	ldw r6, r5+-12
	ldw r8, r5+-8
	stw r7+4, r8
	stw r7+0, r6
	ldbu r5, r5+-4
	stb r7+8, r5
	andi r5, r5, 64
	addi r6, r0, 0
	beq r5, r6, .LBB81_18
.LBB81_15:
	ldbu r5, r4+5
	andi r5, r5, 32
	beq r5, r6, .LBB81_18
.LBB81_16:
	ldw r5, r7+0
	ldbu r7, r5+5
	andi r7, r7, 24
	beq r7, r6, .LBB81_18
.LBB81_17:
	add r11, r1, r0
	jal r31, luaC_barrier_
	add r1, r11, r0
.LBB81_18:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB81_19:
	ldw r6, r6+0
	ldbu r8, r6+8
	addi r9, r0, 102
	bne r8, r9, .LBB81_2
.LBB81_20:
	sub r7, r7, r4
	ldw r6, r6+0
	ldbu r8, r6+6
	bgt r7, r8, .LBB81_2
.LBB81_21:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r6, r1
	addi r8, r1, 16
	jal r0, .LBB81_7
.Lfunc_end81:
	.size	lua_setupvalue, .Lfunc_end81-lua_setupvalue
                                        # -- End function
	.globl	lua_upvalueid                   # -- Begin function lua_upvalueid
	.p2align	2
	.type	lua_upvalueid,@function
lua_upvalueid:                          # @lua_upvalueid
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	ldw r7, r3+20
	addi r6, r0, 1
	blt r4, r6, .LBB82_3
.LBB82_1:
	ldw r1, r7+0
	addi r8, r0, 12
	mul r8, r4, r8
	add r1, r1, r8
	ldw r8, r3+12
	bltu r1, r8, .LBB82_7
.LBB82_2:
	ldw r1, r3+16
	addi r1, r1, 48
	jal r0, .LBB82_7
.LBB82_3:
	lui r1, 1048572
	addi r8, r1, 385
	bge r4, r8, .LBB82_6
.LBB82_4:
	addi r8, r1, 384
	bne r4, r8, .LBB82_18
.LBB82_5:
	ldw r1, r3+16
	addi r1, r1, 36
	jal r0, .LBB82_7
.LBB82_6:
	ldw r1, r3+12
	addi r8, r0, 12
	mul r8, r4, r8
	add r1, r1, r8
.LBB82_7:
	ldbu r8, r1+8
	andi r9, r8, 63
	addi r8, r0, 0
	addi r10, r0, 38
	beq r9, r10, .LBB82_14
.LBB82_8:
	addi r1, r0, 22
	beq r9, r1, .LBB82_22
.LBB82_9:
	addi r10, r0, 6
	add r1, r8, r0
	bne r9, r10, .LBB82_30
.LBB82_10:
	blt r4, r6, .LBB82_23
.LBB82_11:
	ldw r1, r7+0
	addi r7, r0, 12
	mul r4, r4, r7
	add r1, r1, r4
	ldw r4, r3+12
	bltu r1, r4, .LBB82_27
.LBB82_12:
	ldw r1, r3+16
	addi r1, r1, 48
	bge r5, r6, .LBB82_28
.LBB82_13:
	lui r1, %hi(getupvalref.nullup)
	addi r1, r1, %lo(getupvalref.nullup)
	jal r0, .LBB82_29
.LBB82_14:
	blt r5, r6, .LBB82_21
.LBB82_15:
	ldw r3, r1+0
	ldbu r4, r3+6
                                        # implicit-def: $r1
	bgt r5, r4, .LBB82_17
.LBB82_16:
	addi r1, r0, 12
	mul r1, r5, r1
	add r1, r3, r1
	addi r1, r1, 4
	addi r6, r0, 0
.LBB82_17:
	addi r3, r0, 0
	bne r6, r3, .LBB82_22
	jal r0, .LBB82_30
.LBB82_18:
	ldw r9, r7+0
	ldbu r10, r9+8
	addi r11, r0, 102
	bne r10, r11, .LBB82_2
.LBB82_19:
	sub r10, r8, r4
	ldw r8, r9+0
	ldbu r9, r8+6
	bgt r10, r9, .LBB82_2
.LBB82_20:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r9, r0, 12
	mul r1, r1, r9
	add r1, r8, r1
	addi r1, r1, 16
	jal r0, .LBB82_7
.LBB82_21:
                                        # implicit-def: $r1
	addi r3, r0, 0
	beq r6, r3, .LBB82_30
.LBB82_22:
	add r1, r8, r0
	jal r0, .LBB82_30
.LBB82_23:
	lui r1, 1048572
	addi r8, r1, 385
	bge r4, r8, .LBB82_26
.LBB82_24:
	addi r8, r1, 384
	bne r4, r8, .LBB82_31
.LBB82_25:
	ldw r1, r3+16
	addi r1, r1, 36
	bge r5, r6, .LBB82_28
	jal r0, .LBB82_13
.LBB82_26:
	ldw r1, r3+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
.LBB82_27:
	blt r5, r6, .LBB82_13
.LBB82_28:
	ldw r1, r1+0
	addi r3, r1, 12
	ldw r1, r1+12
	ldw r1, r1+12
	sgt r1, r5, r1
	slli r4, r5, 2
	add r3, r3, r4
	lui r4, %hi(getupvalref.nullup)
	addi r4, r4, %lo(getupvalref.nullup)
	xor r4, r3, r4
	addi r5, r0, 0
	sub r1, r5, r1
	and r1, r4, r1
	xor r1, r3, r1
.LBB82_29:
	ldw r1, r1+0
.LBB82_30:
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB82_31:
	ldw r7, r7+0
	ldbu r9, r7+8
	addi r10, r0, 102
	bne r9, r10, .LBB82_12
.LBB82_32:
	sub r8, r8, r4
	ldw r7, r7+0
	ldbu r9, r7+6
	bgt r8, r9, .LBB82_12
.LBB82_33:
	addi r1, r1, 383
	sub r1, r1, r4
	addi r3, r0, 12
	mul r1, r1, r3
	add r1, r7, r1
	addi r1, r1, 16
	bge r5, r6, .LBB82_28
	jal r0, .LBB82_13
.Lfunc_end82:
	.size	lua_upvalueid, .Lfunc_end82-lua_upvalueid
                                        # -- End function
	.globl	lua_upvaluejoin                 # -- Begin function lua_upvaluejoin
	.p2align	2
	.type	lua_upvaluejoin,@function
lua_upvaluejoin:                        # @lua_upvaluejoin
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
	ldw r8, r3+20
	addi r1, r0, 1
	blt r4, r1, .LBB83_3
.LBB83_1:
	ldw r9, r8+0
	addi r10, r0, 12
	mul r4, r4, r10
	add r4, r9, r4
	ldw r9, r3+12
	bltu r4, r9, .LBB83_7
.LBB83_2:
	ldw r4, r3+16
	addi r4, r4, 48
	ldw r4, r4+0
	bge r5, r1, .LBB83_8
	jal r0, .LBB83_14
.LBB83_3:
	lui r9, 1048572
	addi r10, r9, 385
	bge r4, r10, .LBB83_6
.LBB83_4:
	addi r10, r9, 384
	bne r4, r10, .LBB83_11
.LBB83_5:
	ldw r4, r3+16
	addi r4, r4, 36
	ldw r4, r4+0
	bge r5, r1, .LBB83_8
	jal r0, .LBB83_14
.LBB83_6:
	ldw r9, r3+12
	addi r10, r0, 12
	mul r4, r4, r10
	add r4, r9, r4
.LBB83_7:
	ldw r4, r4+0
	blt r5, r1, .LBB83_14
.LBB83_8:
	addi r9, r4, 12
	ldw r10, r4+12
	ldw r10, r10+12
	sgt r10, r5, r10
	slli r5, r5, 2
	add r5, r9, r5
	lui r9, %hi(getupvalref.nullup)
	addi r9, r9, %lo(getupvalref.nullup)
	xor r9, r5, r9
	addi r11, r0, 0
	sub r10, r11, r10
	and r9, r9, r10
	xor r9, r5, r9
	blt r6, r1, .LBB83_15
.LBB83_9:
	ldw r5, r8+0
	addi r8, r0, 12
	mul r6, r6, r8
	add r5, r5, r6
	ldw r6, r3+12
	bltu r5, r6, .LBB83_19
.LBB83_10:
	ldw r5, r3+16
	addi r5, r5, 48
	bge r7, r1, .LBB83_20
	jal r0, .LBB83_24
.LBB83_11:
	ldw r11, r8+0
	ldbu r12, r11+8
	addi r13, r0, 102
	bne r12, r13, .LBB83_2
.LBB83_12:
	sub r12, r10, r4
	ldw r10, r11+0
	ldbu r11, r10+6
	bgt r12, r11, .LBB83_2
.LBB83_13:
	addi r9, r9, 383
	sub r4, r9, r4
	addi r9, r0, 12
	mul r4, r4, r9
	add r4, r10, r4
	addi r4, r4, 16
	ldw r4, r4+0
	bge r5, r1, .LBB83_8
.LBB83_14:
	lui r9, %hi(getupvalref.nullup)
	addi r9, r9, %lo(getupvalref.nullup)
	bge r6, r1, .LBB83_9
.LBB83_15:
	lui r5, 1048572
	addi r10, r5, 385
	bge r6, r10, .LBB83_18
.LBB83_16:
	addi r10, r5, 384
	bne r6, r10, .LBB83_21
.LBB83_17:
	ldw r5, r3+16
	addi r5, r5, 36
	bge r7, r1, .LBB83_20
	jal r0, .LBB83_24
.LBB83_18:
	ldw r5, r3+12
	addi r8, r0, 12
	mul r6, r6, r8
	add r5, r5, r6
.LBB83_19:
	blt r7, r1, .LBB83_24
.LBB83_20:
	ldw r1, r5+0
	addi r5, r1, 12
	ldw r1, r1+12
	ldw r1, r1+12
	sgt r1, r7, r1
	slli r6, r7, 2
	add r5, r5, r6
	lui r6, %hi(getupvalref.nullup)
	addi r6, r6, %lo(getupvalref.nullup)
	xor r6, r5, r6
	addi r7, r0, 0
	sub r1, r7, r1
	and r1, r6, r1
	xor r1, r5, r1
	jal r0, .LBB83_25
.LBB83_21:
	ldw r8, r8+0
	ldbu r11, r8+8
	addi r12, r0, 102
	bne r11, r12, .LBB83_10
.LBB83_22:
	sub r10, r10, r6
	ldw r8, r8+0
	ldbu r11, r8+6
	bgt r10, r11, .LBB83_10
.LBB83_23:
	addi r5, r5, 383
	sub r5, r5, r6
	addi r6, r0, 12
	mul r5, r5, r6
	add r5, r8, r5
	addi r5, r5, 16
	bge r7, r1, .LBB83_20
.LBB83_24:
	lui r1, %hi(getupvalref.nullup)
	addi r1, r1, %lo(getupvalref.nullup)
.LBB83_25:
	ldw r5, r1+0
	stw r9+0, r5
	ldbu r1, r4+5
	andi r6, r1, 32
	addi r1, r0, 0
	beq r6, r1, .LBB83_28
.LBB83_26:
	ldbu r6, r5+5
	andi r6, r6, 24
	beq r6, r1, .LBB83_28
.LBB83_27:
	jal r31, luaC_barrier_
.LBB83_28:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end83:
	.size	lua_upvaluejoin, .Lfunc_end83-lua_upvaluejoin
                                        # -- End function
	.type	lua_ident,@object               # @lua_ident
	.section	.rodata,"a",@progbits
	.globl	lua_ident
lua_ident:
	.asciz	"$LuaVersion: Lua 5.4.7  Copyright (C) 1994-2024 Lua.org, PUC-Rio $$LuaAuthors: R. Ierusalimschy, L. H. de Figueiredo, W. Celes $"
	.size	lua_ident, 129

	.hidden	luaT_typenames_
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.zero	1
	.size	.L.str, 1

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"?"
	.size	.L.str.1, 2

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"(no name)"
	.size	.L.str.2, 10

	.type	getupvalref.nullup,@object      # @getupvalref.nullup
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
getupvalref.nullup:
	.word	0
	.size	getupvalref.nullup, 4

	.hidden	luaD_growstack
	.hidden	luaF_close
	.hidden	luaC_barrier_
	.hidden	luaV_tonumber_
	.hidden	luaV_equalobj
	.hidden	luaO_arith
	.hidden	luaV_lessthan
	.hidden	luaV_lessequal
	.hidden	luaO_str2num
	.hidden	luaV_tointeger
	.hidden	luaO_tostring
	.hidden	luaC_step
	.hidden	luaH_getn
	.hidden	luaS_new
	.hidden	luaS_newlstr
	.hidden	luaO_pushvfstring
	.hidden	luaF_newCclosure
	.hidden	luaH_get
	.hidden	luaV_finishget
	.hidden	luaH_getint
	.hidden	luaH_new
	.hidden	luaH_resize
	.hidden	luaC_barrierback_
	.hidden	luaV_finishset
	.hidden	luaH_setint
	.hidden	luaC_checkfinalizer
	.hidden	luaD_call
	.hidden	luaD_callnoyield
	.hidden	luaD_pcall
	.hidden	luaZ_init
	.hidden	luaD_protectedparser
	.hidden	luaU_dump
	.hidden	luaE_setdebt
	.hidden	luaC_fullgc
	.hidden	luaC_changemode
	.hidden	luaD_throw
	.hidden	luaG_errormsg
	.hidden	luaH_next
	.hidden	luaF_newtbcupval
	.hidden	luaV_concat
	.hidden	luaV_objlen
	.hidden	luaE_warning
	.hidden	luaS_newudata
	.hidden	luaH_getstr
	.hidden	luaH_set
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
