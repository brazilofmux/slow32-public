	.file	"lstate.c"
	.text
	.hidden	luaE_setdebt                    # -- Begin function luaE_setdebt
	.globl	luaE_setdebt
	.p2align	2
	.type	luaE_setdebt,@function
luaE_setdebt:                           # @luaE_setdebt
# %bb.0:
	ldw r1, r3+8
	ldw r5, r3+12
	add r1, r5, r1
	lui r5, 524288
	addi r5, r5, 1
	add r5, r1, r5
	xor r6, r4, r5
	sgt r4, r4, r5
	addi r7, r0, 0
	sub r4, r7, r4
	and r4, r6, r4
	xor r4, r5, r4
	sub r1, r1, r4
	stw r3+8, r1
	stw r3+12, r4
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaE_setdebt, .Lfunc_end0-luaE_setdebt
                                        # -- End function
	.globl	lua_setcstacklimit              # -- Begin function lua_setcstacklimit
	.p2align	2
	.type	lua_setcstacklimit,@function
lua_setcstacklimit:                     # @lua_setcstacklimit
# %bb.0:
	addi r1, r0, 200
	jalr r0, r31, 0
.Lfunc_end1:
	.size	lua_setcstacklimit, .Lfunc_end1-lua_setcstacklimit
                                        # -- End function
	.hidden	luaE_extendCI                   # -- Begin function luaE_extendCI
	.globl	luaE_extendCI
	.p2align	2
	.type	luaE_extendCI,@function
luaE_extendCI:                          # @luaE_extendCI
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
	addi r4, r0, 36
	addi r12, r0, 0
	add r5, r12, r0
	jal r31, luaM_malloc_
	ldw r3, r11+20
	stw r3+12, r1
	stw r1+8, r3
	stw r1+12, r12
	stw r1+20, r12
	ldhu r3, r11+8
	addi r3, r3, 1
	sth r11+8, r3
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	luaE_extendCI, .Lfunc_end2-luaE_extendCI
                                        # -- End function
	.hidden	luaE_shrinkCI                   # -- Begin function luaE_shrinkCI
	.globl	luaE_shrinkCI
	.p2align	2
	.type	luaE_shrinkCI,@function
luaE_shrinkCI:                          # @luaE_shrinkCI
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
	ldw r1, r3+20
	ldw r14, r1+12
	addi r13, r0, 0
	beq r14, r13, .LBB3_5
.LBB3_1:
	ldw r4, r14+12
	beq r4, r13, .LBB3_5
.LBB3_2:
	add r11, r3, r0
	addi r12, r0, 36
.LBB3_3:
	ldw r15, r4+12
	stw r14+12, r15
	ldhu r1, r11+8
	addi r1, r1, -1
	sth r11+8, r1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaM_free_
	beq r15, r13, .LBB3_5
.LBB3_4:
	stw r15+8, r14
	ldw r4, r15+12
	add r14, r15, r0
	bne r4, r13, .LBB3_3
.LBB3_5:
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
.Lfunc_end3:
	.size	luaE_shrinkCI, .Lfunc_end3-luaE_shrinkCI
                                        # -- End function
	.hidden	luaE_checkcstack                # -- Begin function luaE_checkcstack
	.globl	luaE_checkcstack
	.p2align	2
	.type	luaE_checkcstack,@function
luaE_checkcstack:                       # @luaE_checkcstack
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldhu r1, r3+96
	addi r4, r0, 200
	beq r1, r4, .LBB4_3
.LBB4_1:
	addi r4, r0, 220
	bgeu r1, r4, .LBB4_4
.LBB4_2:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB4_3:
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	jal r31, luaG_runerror
.LBB4_4:
	addi r4, r0, 5
	jal r31, luaD_throw
.Lfunc_end4:
	.size	luaE_checkcstack, .Lfunc_end4-luaE_checkcstack
                                        # -- End function
	.hidden	luaE_incCstack                  # -- Begin function luaE_incCstack
	.globl	luaE_incCstack
	.p2align	2
	.type	luaE_incCstack,@function
luaE_incCstack:                         # @luaE_incCstack
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r3+96
	addi r1, r1, 1
	stw r3+96, r1
	lui r4, 16
	addi r4, r4, -8
	and r1, r1, r4
	addi r4, r0, 200
	bgeu r1, r4, .LBB5_2
.LBB5_1:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB5_2:
	jal r31, luaE_checkcstack
	jal r0, .LBB5_1
.Lfunc_end5:
	.size	luaE_incCstack, .Lfunc_end5-luaE_incCstack
                                        # -- End function
	.globl	lua_newthread                   # -- Begin function lua_newthread
	.p2align	2
	.type	lua_newthread,@function
lua_newthread:                          # @lua_newthread
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
	add r12, r3, r0
	ldw r16, r3+16
	ldw r1, r16+12
	addi r3, r0, 1
	blt r1, r3, .LBB6_2
.LBB6_1:
	add r3, r12, r0
	jal r31, luaC_step
.LBB6_2:
	addi r11, r0, 8
	addi r5, r0, 120
	addi r14, r0, 4
	add r3, r12, r0
	add r4, r11, r0
	add r6, r14, r0
	jal r31, luaC_newobjdt
	add r15, r1, r0
	ldw r1, r12+12
	stw r1+0, r15
	addi r3, r0, 72
	stb r1+8, r3
	ldw r1, r12+12
	addi r1, r1, 12
	stw r12+12, r1
	stw r15+16, r16
	addi r13, r0, 0
	stw r15+28, r13
	stw r15+20, r13
	sth r15+8, r13
	stw r15+44, r15
	stw r15+96, r13
	stw r15+48, r13
	stw r15+88, r13
	stw r15+112, r13
	stw r15+104, r13
	stw r15+108, r13
	stw r15+32, r13
	addi r1, r0, 256
	sth r15+6, r1
	stw r15+92, r13
	stw r15+100, r13
	ldw r1, r12+112
	stw r15+112, r1
	ldw r1, r12+104
	stw r15+104, r1
	ldw r3, r12+88
	stw r15+88, r3
	stw r15+108, r1
	addi r3, r15, -4
	ldw r1, r16+152
	addi r4, r1, -4
	add r5, r14, r0
	jal r31, memcpy
	addi r4, r0, 540
	add r3, r12, r0
	add r5, r13, r0
	jal r31, luaM_malloc_
	stw r15+28, r1
	stw r15+36, r1
	addi r3, r0, 548
.LBB6_3:
	ldw r4, r15+28
	add r4, r4, r11
	stb r4+0, r13
	addi r11, r11, 12
	bne r11, r3, .LBB6_3
.LBB6_4:
	ldw r3, r15+28
	stw r15+12, r3
	addi r4, r3, 480
	stw r15+24, r4
	addi r4, r15, 52
	stw r15+60, r13
	stw r15+64, r13
	stw r15+52, r3
	stw r15+68, r13
	lui r5, 32
	stw r15+84, r5
	stb r3+8, r13
	ldw r3, r15+12
	addi r5, r3, 12
	stw r15+12, r5
	addi r3, r3, 252
	stw r15+56, r3
	stw r15+20, r4
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
.Lfunc_end6:
	.size	lua_newthread, .Lfunc_end6-lua_newthread
                                        # -- End function
	.hidden	luaE_freethread                 # -- Begin function luaE_freethread
	.globl	luaE_freethread
	.p2align	2
	.type	luaE_freethread,@function
luaE_freethread:                        # @luaE_freethread
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
	add r12, r4, r0
	add r11, r3, r0
	ldw r4, r4+28
	add r3, r12, r0
	jal r31, luaF_closeupval
	ldw r1, r12+28
	addi r14, r0, 0
	beq r1, r14, .LBB7_5
.LBB7_1:
	addi r1, r12, 52
	stw r12+20, r1
	ldw r4, r12+64
	stw r12+64, r14
	beq r4, r14, .LBB7_4
.LBB7_2:
	addi r13, r0, 36
.LBB7_3:
	ldw r15, r4+12
	add r3, r12, r0
	add r5, r13, r0
	jal r31, luaM_free_
	ldhu r1, r12+8
	addi r1, r1, -1
	sth r12+8, r1
	add r4, r15, r0
	bne r15, r14, .LBB7_3
.LBB7_4:
	ldw r4, r12+28
	ldw r1, r12+24
	sub r1, r1, r4
	addi r5, r1, 60
	add r3, r12, r0
	jal r31, luaM_free_
.LBB7_5:
	addi r4, r12, -4
	addi r5, r0, 120
	add r3, r11, r0
	jal r31, luaM_free_
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
.Lfunc_end7:
	.size	luaE_freethread, .Lfunc_end7-luaE_freethread
                                        # -- End function
	.hidden	luaE_resetthread                # -- Begin function luaE_resetthread
	.globl	luaE_resetthread
	.p2align	2
	.type	luaE_resetthread,@function
luaE_resetthread:                       # @luaE_resetthread
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
	addi r1, r3, 52
	stw r3+20, r1
	ldw r1, r3+28
	addi r13, r0, 0
	stb r1+8, r13
	ldw r1, r3+28
	stw r3+52, r1
	addi r1, r0, 2
	sth r3+86, r1
	addi r1, r0, 1
	seq r3, r4, r1
	sub r3, r13, r3
	and r3, r4, r3
	xor r5, r4, r3
	stb r11+6, r13
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaD_closeprotected
	add r12, r1, r0
	ldw r1, r11+28
	addi r5, r1, 12
	beq r12, r13, .LBB8_2
.LBB8_1:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaD_seterrorobj
	jal r0, .LBB8_3
.LBB8_2:
	stw r11+12, r5
.LBB8_3:
	ldw r1, r11+12
	addi r1, r1, 240
	stw r11+56, r1
	ldw r3, r11+28
	sub r1, r1, r3
	srai r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r4, r1, r3
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, luaD_reallocstack
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	luaE_resetthread, .Lfunc_end8-luaE_resetthread
                                        # -- End function
	.globl	lua_closethread                 # -- Begin function lua_closethread
	.p2align	2
	.type	lua_closethread,@function
lua_closethread:                        # @lua_closethread
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
	addi r13, r0, 0
	add r1, r13, r0
	beq r4, r13, .LBB9_2
.LBB9_1:
	ldhu r1, r4+96
.LBB9_2:
	stw r11+96, r1
	ldbu r1, r11+6
	addi r3, r11, 52
	stw r11+20, r3
	ldw r3, r11+28
	stb r3+8, r13
	ldw r3, r11+28
	stw r11+52, r3
	addi r3, r0, 2
	sth r11+86, r3
	addi r4, r0, 1
	seq r3, r1, r4
	sub r3, r13, r3
	and r3, r1, r3
	xor r5, r1, r3
	stb r11+6, r13
	add r3, r11, r0
	jal r31, luaD_closeprotected
	add r12, r1, r0
	ldw r1, r11+28
	addi r5, r1, 12
	beq r12, r13, .LBB9_4
.LBB9_3:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaD_seterrorobj
	jal r0, .LBB9_5
.LBB9_4:
	stw r11+12, r5
.LBB9_5:
	ldw r1, r11+12
	addi r1, r1, 240
	stw r11+56, r1
	ldw r3, r11+28
	sub r1, r1, r3
	srai r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r4, r1, r3
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, luaD_reallocstack
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end9:
	.size	lua_closethread, .Lfunc_end9-lua_closethread
                                        # -- End function
	.globl	lua_resetthread                 # -- Begin function lua_resetthread
	.p2align	2
	.type	lua_resetthread,@function
lua_resetthread:                        # @lua_resetthread
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
	addi r13, r0, 0
	stw r3+96, r13
	ldbu r1, r3+6
	addi r3, r3, 52
	stw r11+20, r3
	ldw r3, r11+28
	stb r3+8, r13
	ldw r3, r11+28
	stw r11+52, r3
	addi r3, r0, 2
	sth r11+86, r3
	addi r4, r0, 1
	seq r3, r1, r4
	sub r3, r13, r3
	and r3, r1, r3
	xor r5, r1, r3
	stb r11+6, r13
	add r3, r11, r0
	jal r31, luaD_closeprotected
	add r12, r1, r0
	ldw r1, r11+28
	addi r5, r1, 12
	beq r12, r13, .LBB10_2
.LBB10_1:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaD_seterrorobj
	jal r0, .LBB10_3
.LBB10_2:
	stw r11+12, r5
.LBB10_3:
	ldw r1, r11+12
	addi r1, r1, 240
	stw r11+56, r1
	ldw r3, r11+28
	sub r1, r1, r3
	srai r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r4, r1, r3
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, luaD_reallocstack
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end10:
	.size	lua_resetthread, .Lfunc_end10-lua_resetthread
                                        # -- End function
	.globl	lua_newstate                    # -- Begin function lua_newstate
	.p2align	2
	.type	lua_newstate,@function
lua_newstate:                           # @lua_newstate
# %bb.0:
	addi sp, sp, -72
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 72
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
	add r15, r4, r0
	add r16, r3, r0
	addi r12, r0, 0
	addi r14, r0, 8
	addi r13, r0, 848
	add r3, r4, r0
	add r4, r12, r0
	add r5, r14, r0
	add r6, r13, r0
	jalr lr, r16, 0
	beq r1, r12, .LBB11_6
.LBB11_1:
	addi r11, r1, 4
	addi r3, r1, 120
	lui r4, 4097
	addi r4, r4, -2040
	stw r1+8, r4
	stb r1+184, r14
	stw r1+20, r3
	addi r12, r0, 0
	stw r1+32, r12
	stw r1+24, r12
	sth r1+12, r12
	stw r1+48, r11
	stw r1+100, r12
	stw r1+52, r12
	stw r1+92, r12
	stw r1+116, r12
	stw r1+108, r12
	stw r1+112, r12
	stw r1+36, r12
	stw r1+96, r12
	stw r1+104, r12
	stw r1+196, r11
	stw r1+4, r12
	lui r3, 16
	stw r1+100, r3
	stw r1+120, r16
	stw r1+124, r15
	stw r1+840, r12
	stw r1+844, r12
	stw r1+272, r11
	add r3, r12, r0
	add r18, r1, r0
	jal r31, time
	addi r19, fp, -56
	stw r19+0, r1
	addi r15, fp, -60
	stw r15+0, r11
	addi r16, fp, -52
	addi r17, r0, 4
	add r3, r16, r0
	add r4, r15, r0
	add r5, r17, r0
	jal r31, memcpy
	stw r15+0, r19
	addi r3, r16, 4
	add r4, r15, r0
	add r5, r17, r0
	jal r31, memcpy
	lui r1, %hi(lua_newstate)
	addi r1, r1, %lo(lua_newstate)
	stw r15+0, r1
	addi r3, r16, 8
	add r4, r15, r0
	add r5, r17, r0
	jal r31, memcpy
	ldw r5, r19+0
	addi r4, r0, 12
	add r3, r16, r0
	jal r31, luaS_hash
	stw r18+180, r1
	addi r1, r0, 2
	sth r18+190, r1
	stw r18+148, r12
	stw r18+152, r12
	stw r18+144, r12
	stb r18+164, r12
	stw r18+268, r12
	stb r18+185, r14
	sth r18+186, r12
	stw r18+232, r12
	stw r18+228, r12
	stw r18+204, r12
	stw r18+244, r12
	stw r18+240, r12
	stw r18+236, r12
	stw r18+248, r12
	stw r18+260, r12
	stw r18+256, r12
	stw r18+252, r12
	stw r18+200, r12
	stw r18+212, r12
	stw r18+208, r12
	stw r18+224, r12
	stw r18+220, r12
	stw r18+216, r12
	stw r18+264, r12
	stw r18+128, r13
	stw r18+132, r12
	stw r18+140, r12
	stw r18+168, r12
	addi r1, r0, 3
	stb r18+176, r1
	lui r1, 2
	addi r3, r1, -1742
	sth r18+192, r3
	addi r3, r0, 13
	stb r18+194, r3
	addi r1, r1, -1772
	sth r18+188, r1
	addi r1, r18, 380
	addi r3, r0, 36
	add r4, r12, r0
.LBB11_2:
	add r5, r1, r4
	stw r5+0, r12
	addi r4, r4, 4
	bne r4, r3, .LBB11_2
.LBB11_3:
	lui r4, %hi(f_luaopen)
	addi r4, r4, %lo(f_luaopen)
	addi r12, r0, 0
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaD_rawrunprotected
	beq r1, r12, .LBB11_5
.LBB11_4:
	add r3, r11, r0
	jal r31, close_state
	jal r0, .LBB11_6
.LBB11_5:
	add r12, r11, r0
.LBB11_6:
	add r1, r12, r0
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
	addi sp, sp, 72
	jalr r0, r31, 0
.Lfunc_end11:
	.size	lua_newstate, .Lfunc_end11-lua_newstate
                                        # -- End function
	.p2align	2                               # -- Begin function f_luaopen
	.type	f_luaopen,@function
f_luaopen:                              # @f_luaopen
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
	ldw r14, r3+16
	addi r4, r0, 540
	addi r12, r0, 0
	add r5, r12, r0
	jal r31, luaM_malloc_
	stw r11+28, r1
	stw r11+36, r1
	addi r1, r0, 8
	addi r3, r0, 548
.LBB12_1:
	ldw r4, r11+28
	add r4, r4, r1
	stb r4+0, r12
	addi r1, r1, 12
	bne r1, r3, .LBB12_1
.LBB12_2:
	ldw r1, r11+28
	stw r11+12, r1
	addi r3, r1, 480
	stw r11+24, r3
	addi r3, r11, 52
	stw r11+60, r12
	stw r11+64, r12
	stw r11+52, r1
	stw r11+68, r12
	lui r4, 32
	stw r11+84, r4
	stb r1+8, r12
	ldw r1, r11+12
	addi r4, r1, 12
	stw r11+12, r4
	addi r1, r1, 252
	stw r11+56, r1
	stw r11+20, r3
	add r3, r11, r0
	jal r31, luaH_new
	add r13, r1, r0
	stw r14+36, r1
	addi r15, r0, 69
	stb r14+44, r15
	addi r5, r0, 2
	add r3, r11, r0
	add r4, r1, r0
	add r6, r12, r0
	jal r31, luaH_resize
	ldw r1, r13+12
	stw r1+0, r11
	addi r3, r0, 72
	stb r1+8, r3
	ldw r13, r13+12
	add r3, r11, r0
	jal r31, luaH_new
	stw r13+12, r1
	stb r13+20, r15
	add r3, r11, r0
	jal r31, luaS_init
	add r3, r11, r0
	jal r31, luaT_init
	add r3, r11, r0
	jal r31, luaX_init
	stb r14+70, r12
	stb r14+56, r12
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
.Lfunc_end12:
	.size	f_luaopen, .Lfunc_end12-f_luaopen
                                        # -- End function
	.p2align	2                               # -- Begin function close_state
	.type	close_state,@function
close_state:                            # @close_state
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
	ldw r13, r3+16
	ldbu r1, r13+56
	andi r1, r1, 15
	addi r14, r0, 0
	addi r12, r3, 52
	bne r1, r14, .LBB13_2
.LBB13_1:
	stw r11+20, r12
	addi r4, r0, 1
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, luaD_closeprotected
.LBB13_2:
	add r3, r11, r0
	jal r31, luaC_freeallobjects
	ldw r1, r11+16
	ldw r4, r1+24
	ldw r1, r1+32
	slli r5, r1, 2
	add r3, r11, r0
	jal r31, luaM_free_
	ldw r1, r11+28
	beq r1, r14, .LBB13_7
.LBB13_3:
	stw r11+20, r12
	ldw r4, r11+64
	stw r11+64, r14
	beq r4, r14, .LBB13_6
.LBB13_4:
	addi r12, r0, 36
.LBB13_5:
	ldw r15, r4+12
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaM_free_
	ldhu r1, r11+8
	addi r1, r1, -1
	sth r11+8, r1
	add r4, r15, r0
	bne r15, r14, .LBB13_5
.LBB13_6:
	ldw r4, r11+28
	ldw r1, r11+24
	sub r1, r1, r4
	addi r5, r1, 60
	add r3, r11, r0
	jal r31, luaM_free_
.LBB13_7:
	ldw r1, r13+0
	ldw r3, r13+4
	addi r4, r11, -4
	addi r5, r0, 848
	addi r6, r0, 0
	jalr lr, r1, 0
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
.Lfunc_end13:
	.size	close_state, .Lfunc_end13-close_state
                                        # -- End function
	.globl	lua_close                       # -- Begin function lua_close
	.p2align	2
	.type	lua_close,@function
lua_close:                              # @lua_close
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r3+16
	ldw r3, r1+152
	jal r31, close_state
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end14:
	.size	lua_close, .Lfunc_end14-lua_close
                                        # -- End function
	.hidden	luaE_warning                    # -- Begin function luaE_warning
	.globl	luaE_warning
	.p2align	2
	.type	luaE_warning,@function
luaE_warning:                           # @luaE_warning
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r3, r3+16
	ldw r1, r3+720
	addi r6, r0, 0
	beq r1, r6, .LBB15_2
.LBB15_1:
	ldw r3, r3+724
	jalr lr, r1, 0
.LBB15_2:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end15:
	.size	luaE_warning, .Lfunc_end15-luaE_warning
                                        # -- End function
	.hidden	luaE_warnerror                  # -- Begin function luaE_warnerror
	.globl	luaE_warnerror
	.p2align	2
	.type	luaE_warnerror,@function
luaE_warnerror:                         # @luaE_warnerror
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
	ldw r1, r3+12
	ldbu r3, r1+-4
	andi r3, r3, 15
	addi r4, r0, 4
	bne r3, r4, .LBB16_2
.LBB16_1:
	ldw r1, r1+-12
	addi r12, r1, 16
	jal r0, .LBB16_3
.LBB16_2:
	lui r12, %hi(.L.str.1)
	addi r12, r12, %lo(.L.str.1)
.LBB16_3:
	ldw r3, r11+16
	ldw r1, r3+720
	addi r14, r0, 0
	beq r1, r14, .LBB16_5
.LBB16_4:
	ldw r3, r3+724
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	addi r5, r0, 1
	jalr lr, r1, 0
.LBB16_5:
	ldw r3, r11+16
	ldw r1, r3+720
	beq r1, r14, .LBB16_7
.LBB16_6:
	ldw r3, r3+724
	addi r5, r0, 1
	add r4, r13, r0
	jalr lr, r1, 0
.LBB16_7:
	ldw r3, r11+16
	ldw r1, r3+720
	beq r1, r14, .LBB16_9
.LBB16_8:
	ldw r3, r3+724
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	addi r5, r0, 1
	jalr lr, r1, 0
.LBB16_9:
	ldw r3, r11+16
	ldw r1, r3+720
	beq r1, r14, .LBB16_11
.LBB16_10:
	ldw r3, r3+724
	addi r5, r0, 1
	add r4, r12, r0
	jalr lr, r1, 0
.LBB16_11:
	ldw r3, r11+16
	ldw r1, r3+720
	beq r1, r14, .LBB16_13
.LBB16_12:
	ldw r3, r3+724
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	addi r5, r0, 0
	jalr lr, r1, 0
.LBB16_13:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end16:
	.size	luaE_warnerror, .Lfunc_end16-luaE_warnerror
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"C stack overflow"
	.size	.L.str, 17

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"error object is not a string"
	.size	.L.str.1, 29

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"error in "
	.size	.L.str.2, 10

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	" ("
	.size	.L.str.3, 3

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	")"
	.size	.L.str.4, 2

	.hidden	luaM_malloc_
	.hidden	luaM_free_
	.hidden	luaG_runerror
	.hidden	luaD_throw
	.hidden	luaC_step
	.hidden	luaC_newobjdt
	.hidden	luaF_closeupval
	.hidden	luaD_closeprotected
	.hidden	luaD_seterrorobj
	.hidden	luaD_reallocstack
	.hidden	luaD_rawrunprotected
	.hidden	luaS_hash
	.hidden	luaS_init
	.hidden	luaT_init
	.hidden	luaX_init
	.hidden	luaH_new
	.hidden	luaH_resize
	.hidden	luaC_freeallobjects
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
