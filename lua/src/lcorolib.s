	.file	"lcorolib.c"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaopen_coroutine
.LCPI0_0:
	.quad	0x407f800000000000              # double 504
	.text
	.globl	luaopen_coroutine
	.p2align	2
	.type	luaopen_coroutine,@function
luaopen_coroutine:                      # @luaopen_coroutine
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
	addi r5, r0, 8
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_createtable
	lui r4, %hi(co_funcs)
	addi r4, r4, %lo(co_funcs)
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
	.size	luaopen_coroutine, .Lfunc_end0-luaopen_coroutine
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_cocreate
	.type	luaB_cocreate,@function
luaB_cocreate:                          # @luaB_cocreate
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
	addi r5, r0, 6
	add r4, r12, r0
	jal r31, luaL_checktype
	add r3, r11, r0
	jal r31, lua_newthread
	add r13, r1, r0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushvalue
	add r3, r11, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, lua_xmove
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	luaB_cocreate, .Lfunc_end1-luaB_cocreate
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_coresume
	.type	luaB_coresume,@function
luaB_coresume:                          # @luaB_coresume
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
	addi r4, r0, 1
	jal r31, lua_tothread
	add r12, r1, r0
	addi r1, r0, 0
	beq r12, r1, .LBB2_4
.LBB2_1:
	add r3, r11, r0
	jal r31, lua_gettop
	addi r5, r1, -1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, auxresume
	addi r14, r0, -1
	ble r1, r14, .LBB2_5
.LBB2_2:
	add r13, r1, r0
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_pushboolean
	addi r12, r13, 1
	xor r4, r13, r14
.LBB2_3:
	addi r5, r0, 1
	add r3, r11, r0
	jal r31, lua_rotate
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
.LBB2_4:
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_typeerror
	jal r0, .LBB2_1
.LBB2_5:
	addi r4, r0, 0
	add r3, r11, r0
	jal r31, lua_pushboolean
	addi r12, r0, 2
	addi r4, r0, -2
	jal r0, .LBB2_3
.Lfunc_end2:
	.size	luaB_coresume, .Lfunc_end2-luaB_coresume
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_corunning
	.type	luaB_corunning,@function
luaB_corunning:                         # @luaB_corunning
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	jal r31, lua_pushthread
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushboolean
	addi r1, r0, 2
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	luaB_corunning, .Lfunc_end3-luaB_corunning
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_costatus
	.type	luaB_costatus,@function
luaB_costatus:                          # @luaB_costatus
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
	stw fp+-20, lr
	add r11, r3, r0
	addi r13, r0, 1
	add r4, r13, r0
	jal r31, lua_tothread
	add r12, r1, r0
	addi r14, r0, 0
	beq r1, r14, .LBB4_10
.LBB4_1:
	beq r11, r12, .LBB4_9
.LBB4_2:
	add r3, r12, r0
	jal r31, lua_status
	beq r1, r13, .LBB4_6
.LBB4_3:
	addi r14, r0, 0
	bne r1, r14, .LBB4_7
.LBB4_4:
	addi r5, fp, -128
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_getstack
	beq r1, r14, .LBB4_8
.LBB4_5:
	addi r14, r0, 3
	jal r0, .LBB4_9
.LBB4_6:
	addi r14, r0, 2
	jal r0, .LBB4_9
.LBB4_7:
	add r14, r13, r0
	jal r0, .LBB4_9
.LBB4_8:
	add r3, r12, r0
	jal r31, lua_gettop
	seq r1, r1, r14
	sub r1, r14, r1
	andi r1, r1, 3
	xori r14, r1, 2
.LBB4_9:
	slli r1, r14, 2
	lui r3, %hi(statname)
	addi r3, r3, %lo(statname)
	add r1, r1, r3
	ldw r4, r1+0
	add r3, r11, r0
	jal r31, lua_pushstring
	addi r1, r0, 1
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 136
	jalr r0, r31, 0
.LBB4_10:
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_typeerror
	bne r11, r12, .LBB4_2
	jal r0, .LBB4_9
.Lfunc_end4:
	.size	luaB_costatus, .Lfunc_end4-luaB_costatus
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_cowrap
	.type	luaB_cowrap,@function
luaB_cowrap:                            # @luaB_cowrap
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
	addi r5, r0, 6
	add r4, r12, r0
	jal r31, luaL_checktype
	add r3, r11, r0
	jal r31, lua_newthread
	add r13, r1, r0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushvalue
	add r3, r11, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, lua_xmove
	lui r4, %hi(luaB_auxwrap)
	addi r4, r4, %lo(luaB_auxwrap)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_pushcclosure
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	luaB_cowrap, .Lfunc_end5-luaB_cowrap
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_yield
	.type	luaB_yield,@function
luaB_yield:                             # @luaB_yield
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	jal r31, lua_gettop
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r1, r0
	add r6, r5, r0
	jal r31, lua_yieldk
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	luaB_yield, .Lfunc_end6-luaB_yield
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_yieldable
	.type	luaB_yieldable,@function
luaB_yieldable:                         # @luaB_yieldable
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
	addi r4, r0, -1
	add r3, r11, r0
	beq r1, r4, .LBB7_2
.LBB7_1:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_tothread
	add r3, r1, r0
	addi r12, r0, 0
	beq r1, r12, .LBB7_3
.LBB7_2:
	jal r31, lua_isyieldable
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lua_pushboolean
	addi r1, r0, 1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB7_3:
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_typeerror
	add r3, r12, r0
	jal r0, .LBB7_2
.Lfunc_end7:
	.size	luaB_yieldable, .Lfunc_end7-luaB_yieldable
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_close
	.type	luaB_close,@function
luaB_close:                             # @luaB_close
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
	stw fp+-20, lr
	add r11, r3, r0
	addi r13, r0, 1
	add r4, r13, r0
	jal r31, lua_tothread
	add r12, r1, r0
	addi r14, r0, 0
	beq r1, r14, .LBB8_15
.LBB8_1:
	beq r11, r12, .LBB8_9
.LBB8_2:
	add r3, r12, r0
	jal r31, lua_status
	beq r1, r13, .LBB8_6
.LBB8_3:
	addi r14, r0, 0
	bne r1, r14, .LBB8_7
.LBB8_4:
	addi r5, fp, -128
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_getstack
	beq r1, r14, .LBB8_8
.LBB8_5:
	addi r14, r0, 3
	jal r0, .LBB8_9
.LBB8_6:
	addi r14, r0, 2
	jal r0, .LBB8_9
.LBB8_7:
	add r14, r13, r0
	jal r0, .LBB8_9
.LBB8_8:
	add r3, r12, r0
	jal r31, lua_gettop
	seq r1, r1, r14
	sub r1, r14, r1
	andi r1, r1, 3
	xori r14, r1, 2
.LBB8_9:
	addi r1, r14, -1
	bgtu r1, r13, .LBB8_12
.LBB8_10:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_closethread
	addi r3, r0, 0
	beq r1, r3, .LBB8_13
.LBB8_11:
	addi r4, r0, 0
	add r3, r11, r0
	jal r31, lua_pushboolean
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_xmove
	addi r1, r0, 2
	jal r0, .LBB8_14
.LBB8_12:
	slli r1, r14, 2
	lui r3, %hi(statname)
	addi r3, r3, %lo(statname)
	add r1, r1, r3
	ldw r5, r1+0
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB8_14
.LBB8_13:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushboolean
	add r1, r12, r0
.LBB8_14:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 136
	jalr r0, r31, 0
.LBB8_15:
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_typeerror
	bne r11, r12, .LBB8_2
	jal r0, .LBB8_9
.Lfunc_end8:
	.size	luaB_close, .Lfunc_end8-luaB_close
                                        # -- End function
	.p2align	2                               # -- Begin function auxresume
	.type	auxresume,@function
auxresume:                              # @auxresume
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
	add r14, r5, r0
	add r12, r4, r0
	add r11, r3, r0
	add r3, r4, r0
	add r4, r5, r0
	jal r31, lua_checkstack
	addi r15, r0, 0
	beq r1, r15, .LBB9_5
.LBB9_1:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r14, r0
	jal r31, lua_xmove
	addi r13, fp, -28
	add r3, r12, r0
	add r4, r11, r0
	add r5, r14, r0
	add r6, r13, r0
	jal r31, lua_resume
	addi r3, r0, 1
	bgtu r1, r3, .LBB9_6
.LBB9_2:
	ldw r1, r13+0
	addi r4, r1, 1
	add r3, r11, r0
	jal r31, lua_checkstack
	ldw r5, r13+0
	beq r1, r15, .LBB9_8
.LBB9_3:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_xmove
	ldw r1, r13+0
.LBB9_4:
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
	lui r4, %hi(.L.str.9)
	addi r4, r4, %lo(.L.str.9)
	add r3, r11, r0
	jal r31, lua_pushstring
	jal r0, .LBB9_7
.LBB9_6:
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_xmove
.LBB9_7:
	addi r1, r0, -1
	jal r0, .LBB9_4
.LBB9_8:
	addi r13, r0, -1
	xor r4, r5, r13
	add r3, r12, r0
	jal r31, lua_settop
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	add r3, r11, r0
	jal r31, lua_pushstring
	add r1, r13, r0
	jal r0, .LBB9_4
.Lfunc_end9:
	.size	auxresume, .Lfunc_end9-auxresume
                                        # -- End function
	.p2align	2                               # -- Begin function luaB_auxwrap
	.type	luaB_auxwrap,@function
luaB_auxwrap:                           # @luaB_auxwrap
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
	lui r1, 1048572
	addi r4, r1, 383
	jal r31, lua_tothread
	add r12, r1, r0
	add r3, r11, r0
	jal r31, lua_gettop
	add r3, r11, r0
	add r4, r12, r0
	add r5, r1, r0
	jal r31, auxresume
	addi r3, r0, -1
	ble r1, r3, .LBB10_2
.LBB10_1:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB10_2:
	add r3, r12, r0
	jal r31, lua_status
	addi r3, r0, 2
	bltu r1, r3, .LBB10_4
.LBB10_3:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_closethread
	add r13, r1, r0
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, lua_xmove
	addi r1, r0, 4
	beq r13, r1, .LBB10_6
.LBB10_4:
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_type
	addi r3, r0, 4
	bne r1, r3, .LBB10_6
.LBB10_5:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_where
	addi r4, r0, -2
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_rotate
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, lua_concat
.LBB10_6:
	add r3, r11, r0
	jal r31, lua_error
	jal r0, .LBB10_1
.Lfunc_end10:
	.size	luaB_auxwrap, .Lfunc_end10-luaB_auxwrap
                                        # -- End function
	.type	co_funcs,@object                # @co_funcs
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
co_funcs:
	.word	.L.str
	.word	luaB_cocreate
	.word	.L.str.1
	.word	luaB_coresume
	.word	.L.str.2
	.word	luaB_corunning
	.word	.L.str.3
	.word	luaB_costatus
	.word	.L.str.4
	.word	luaB_cowrap
	.word	.L.str.5
	.word	luaB_yield
	.word	.L.str.6
	.word	luaB_yieldable
	.word	.L.str.7
	.word	luaB_close
	.zero	8
	.size	co_funcs, 72

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"create"
	.size	.L.str, 7

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"resume"
	.size	.L.str.1, 7

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"running"
	.size	.L.str.2, 8

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"status"
	.size	.L.str.3, 7

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"wrap"
	.size	.L.str.4, 5

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"yield"
	.size	.L.str.5, 6

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"isyieldable"
	.size	.L.str.6, 12

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"close"
	.size	.L.str.7, 6

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"thread"
	.size	.L.str.8, 7

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"too many arguments to resume"
	.size	.L.str.9, 29

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"too many results to resume"
	.size	.L.str.10, 27

	.type	statname,@object                # @statname
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
statname:
	.word	.L.str.2
	.word	.L.str.11
	.word	.L.str.12
	.word	.L.str.13
	.size	statname, 16

	.type	.L.str.11,@object               # @.str.11
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.11:
	.asciz	"dead"
	.size	.L.str.11, 5

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"suspended"
	.size	.L.str.12, 10

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"normal"
	.size	.L.str.13, 7

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"cannot close a %s coroutine"
	.size	.L.str.14, 28

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
