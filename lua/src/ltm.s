	.file	"ltm.c"
	.text
	.hidden	luaT_init                       # -- Begin function luaT_init
	.globl	luaT_init
	.p2align	2
	.type	luaT_init,@function
luaT_init:                              # @luaT_init
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
	addi r12, r0, 160
	lui r13, %hi(luaT_init.luaT_eventname-160)
	addi r13, r13, %lo(luaT_init.luaT_eventname-160)
	addi r14, r0, 260
.LBB0_1:
	add r1, r12, r13
	ldw r4, r1+0
	add r3, r11, r0
	jal r31, luaS_new
	ldw r3, r11+16
	add r3, r3, r12
	stw r3+0, r1
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaC_fix
	addi r12, r12, 4
	bne r12, r14, .LBB0_1
.LBB0_2:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaT_init, .Lfunc_end0-luaT_init
                                        # -- End function
	.hidden	luaT_gettm                      # -- Begin function luaT_gettm
	.globl	luaT_gettm
	.p2align	2
	.type	luaT_gettm,@function
luaT_gettm:                             # @luaT_gettm
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
	add r4, r5, r0
	jal r31, luaH_getshortstr
	ldbu r3, r1+8
	andi r4, r3, 15
	addi r3, r0, 0
	bne r4, r3, .LBB1_2
.LBB1_1:
	addi r1, r0, 1
	sll r1, r1, r12
	ldbu r4, r11+6
	or  r1, r4, r1
	stb r11+6, r1
	add r1, r3, r0
.LBB1_2:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	luaT_gettm, .Lfunc_end1-luaT_gettm
                                        # -- End function
	.hidden	luaT_gettmbyobj                 # -- Begin function luaT_gettmbyobj
	.globl	luaT_gettmbyobj
	.p2align	2
	.type	luaT_gettmbyobj,@function
luaT_gettmbyobj:                        # @luaT_gettmbyobj
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldbu r1, r4+8
	andi r1, r1, 15
	addi r6, r0, 7
	beq r1, r6, .LBB2_3
.LBB2_1:
	addi r6, r0, 5
	bne r1, r6, .LBB2_4
.LBB2_2:
	ldw r1, r4+0
	addi r1, r1, 24
	jal r0, .LBB2_5
.LBB2_3:
	ldw r1, r4+0
	addi r1, r1, 12
	jal r0, .LBB2_5
.LBB2_4:
	ldw r4, r3+16
	slli r1, r1, 2
	add r1, r4, r1
	addi r1, r1, 260
.LBB2_5:
	ldw r1, r1+0
	ldw r3, r3+16
	addi r4, r0, 0
	beq r1, r4, .LBB2_7
.LBB2_6:
	slli r4, r5, 2
	add r3, r3, r4
	ldw r4, r3+160
	add r3, r1, r0
	jal r31, luaH_getshortstr
	jal r0, .LBB2_8
.LBB2_7:
	addi r1, r3, 48
.LBB2_8:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	luaT_gettmbyobj, .Lfunc_end2-luaT_gettmbyobj
                                        # -- End function
	.hidden	luaT_objtypename                # -- Begin function luaT_objtypename
	.globl	luaT_objtypename
	.p2align	2
	.type	luaT_objtypename,@function
luaT_objtypename:                       # @luaT_objtypename
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
	ldbu r1, r4+8
	addi r4, r0, 71
	beq r1, r4, .LBB3_3
.LBB3_1:
	addi r4, r0, 69
	bne r1, r4, .LBB3_7
.LBB3_2:
	ldw r1, r11+0
	ldw r12, r1+24
	addi r1, r0, 0
	bne r12, r1, .LBB3_4
	jal r0, .LBB3_7
.LBB3_3:
	ldw r1, r11+0
	ldw r12, r1+12
	addi r1, r0, 0
	beq r12, r1, .LBB3_7
.LBB3_4:
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	jal r31, luaS_new
	add r3, r12, r0
	add r4, r1, r0
	jal r31, luaH_getshortstr
	add r3, r1, r0
	ldbu r1, r1+8
	andi r4, r1, 15
	addi r5, r0, 4
                                        # implicit-def: $r1
	bne r4, r5, .LBB3_6
.LBB3_5:
	ldw r1, r3+0
	addi r1, r1, 16
.LBB3_6:
	beq r4, r5, .LBB3_8
.LBB3_7:
	ldbu r1, r11+8
	andi r1, r1, 15
	slli r1, r1, 2
	lui r3, %hi(luaT_typenames_+4)
	addi r3, r3, %lo(luaT_typenames_+4)
	add r1, r1, r3
	ldw r1, r1+0
.LBB3_8:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	luaT_objtypename, .Lfunc_end3-luaT_objtypename
                                        # -- End function
	.hidden	luaT_callTM                     # -- Begin function luaT_callTM
	.globl	luaT_callTM
	.p2align	2
	.type	luaT_callTM,@function
luaT_callTM:                            # @luaT_callTM
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	add r1, r4, r0
	ldw r4, r3+12
	ldw r8, r1+0
	ldw r9, r1+4
	stw r4+4, r9
	stw r4+0, r8
	ldbu r1, r1+8
	stb r4+8, r1
	ldw r1, r5+0
	ldw r8, r5+4
	stw r4+16, r8
	stw r4+12, r1
	ldbu r1, r5+8
	stb r4+20, r1
	ldw r1, r6+0
	ldw r5, r6+4
	stw r4+28, r5
	stw r4+24, r1
	ldbu r1, r6+8
	stb r4+32, r1
	ldw r1, r7+0
	ldw r5, r7+4
	stw r4+40, r5
	stw r4+36, r1
	ldbu r1, r7+8
	stb r4+44, r1
	addi r1, r4, 48
	stw r3+12, r1
	ldw r1, r3+20
	ldbu r1, r1+34
	andi r1, r1, 10
	addi r5, r0, 0
	beq r1, r5, .LBB4_2
.LBB4_1:
	addi r5, r0, 0
	jal r31, luaD_callnoyield
	jal r0, .LBB4_3
.LBB4_2:
	addi r5, r0, 0
	jal r31, luaD_call
.LBB4_3:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	luaT_callTM, .Lfunc_end4-luaT_callTM
                                        # -- End function
	.hidden	luaT_callTMres                  # -- Begin function luaT_callTMres
	.globl	luaT_callTMres
	.p2align	2
	.type	luaT_callTMres,@function
luaT_callTMres:                         # @luaT_callTMres
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
	add r12, r7, r0
	add r1, r4, r0
	add r11, r3, r0
	ldw r13, r3+28
	ldw r4, r3+12
	ldw r3, r1+0
	ldw r7, r1+4
	stw r4+4, r7
	stw r4+0, r3
	ldbu r1, r1+8
	stb r4+8, r1
	ldw r1, r5+0
	ldw r3, r5+4
	stw r4+16, r3
	stw r4+12, r1
	ldbu r1, r5+8
	stb r4+20, r1
	ldw r1, r6+0
	ldw r3, r6+4
	stw r4+28, r3
	stw r4+24, r1
	ldbu r1, r6+8
	stb r4+32, r1
	ldw r1, r11+12
	addi r1, r1, 36
	stw r11+12, r1
	ldw r1, r11+20
	ldbu r1, r1+34
	andi r1, r1, 10
	addi r5, r0, 1
	addi r3, r0, 0
	beq r1, r3, .LBB5_2
.LBB5_1:
	add r3, r11, r0
	jal r31, luaD_callnoyield
	jal r0, .LBB5_3
.LBB5_2:
	add r3, r11, r0
	jal r31, luaD_call
.LBB5_3:
	sub r1, r12, r13
	ldw r3, r11+28
	add r1, r3, r1
	ldw r3, r11+12
	addi r4, r3, -12
	stw r11+12, r4
	ldw r4, r3+-12
	ldw r5, r3+-8
	stw r1+4, r5
	stw r1+0, r4
	ldbu r3, r3+-4
	stb r1+8, r3
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	luaT_callTMres, .Lfunc_end5-luaT_callTMres
                                        # -- End function
	.hidden	luaT_trybinTM                   # -- Begin function luaT_trybinTM
	.globl	luaT_trybinTM
	.p2align	2
	.type	luaT_trybinTM,@function
luaT_trybinTM:                          # @luaT_trybinTM
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
	add r14, r7, r0
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	jal r31, callbinTM
	addi r3, r0, 0
	beq r1, r3, .LBB6_2
.LBB6_1:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB6_2:
	addi r1, r14, -13
	addi r3, r0, 5
	bgeu r1, r3, .LBB6_8
.LBB6_3:
	ldbu r1, r12+8
	andi r3, r1, 15
	addi r1, r0, 3
	bne r3, r1, .LBB6_6
.LBB6_4:
	ldbu r3, r11+8
	andi r3, r3, 15
	bne r3, r1, .LBB6_6
.LBB6_5:
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, luaG_tointerror
.LBB6_6:
	lui r6, %hi(.L.str.36)
	addi r6, r6, %lo(.L.str.36)
.LBB6_7:
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, luaG_opinterror
.LBB6_8:
	addi r1, r0, 19
	beq r14, r1, .LBB6_3
.LBB6_9:
	lui r6, %hi(.L.str.37)
	addi r6, r6, %lo(.L.str.37)
	jal r0, .LBB6_7
.Lfunc_end6:
	.size	luaT_trybinTM, .Lfunc_end6-luaT_trybinTM
                                        # -- End function
	.p2align	2                               # -- Begin function callbinTM
	.type	callbinTM,@function
callbinTM:                              # @callbinTM
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
	add r15, r7, r0
	add r12, r6, r0
	add r13, r5, r0
	add r14, r4, r0
	add r11, r3, r0
	ldbu r1, r4+8
	andi r1, r1, 15
	addi r17, r0, 7
	beq r1, r17, .LBB7_3
.LBB7_1:
	addi r3, r0, 5
	bne r1, r3, .LBB7_4
.LBB7_2:
	ldw r1, r14+0
	addi r1, r1, 24
	jal r0, .LBB7_5
.LBB7_3:
	ldw r1, r14+0
	addi r1, r1, 12
	jal r0, .LBB7_5
.LBB7_4:
	ldw r3, r11+16
	slli r1, r1, 2
	add r1, r3, r1
	addi r1, r1, 260
.LBB7_5:
	ldw r3, r1+0
	ldw r1, r11+16
	addi r16, r0, 0
	beq r3, r16, .LBB7_7
.LBB7_6:
	slli r4, r15, 2
	add r1, r1, r4
	ldw r4, r1+160
	jal r31, luaH_getshortstr
	jal r0, .LBB7_8
.LBB7_7:
	addi r1, r1, 48
.LBB7_8:
	ldbu r3, r1+8
	andi r3, r3, 15
	bne r3, r16, .LBB7_17
.LBB7_9:
	ldbu r1, r13+8
	andi r1, r1, 15
	beq r1, r17, .LBB7_12
.LBB7_10:
	addi r3, r0, 5
	bne r1, r3, .LBB7_13
.LBB7_11:
	ldw r1, r13+0
	addi r1, r1, 24
	jal r0, .LBB7_14
.LBB7_12:
	ldw r1, r13+0
	addi r1, r1, 12
	jal r0, .LBB7_14
.LBB7_13:
	ldw r3, r11+16
	slli r1, r1, 2
	add r1, r3, r1
	addi r1, r1, 260
.LBB7_14:
	ldw r3, r1+0
	ldw r1, r11+16
	addi r4, r0, 0
	beq r3, r4, .LBB7_16
.LBB7_15:
	slli r4, r15, 2
	add r1, r1, r4
	ldw r4, r1+160
	jal r31, luaH_getshortstr
	jal r0, .LBB7_17
.LBB7_16:
	addi r1, r1, 48
.LBB7_17:
	ldbu r3, r1+8
	andi r3, r3, 15
	beq r3, r16, .LBB7_22
.LBB7_18:
	ldw r15, r11+28
	ldw r4, r11+12
	ldw r3, r1+0
	ldw r5, r1+4
	stw r4+4, r5
	stw r4+0, r3
	ldbu r1, r1+8
	stb r4+8, r1
	ldw r1, r14+0
	ldw r3, r14+4
	stw r4+16, r3
	stw r4+12, r1
	ldbu r1, r14+8
	stb r4+20, r1
	ldw r1, r13+0
	ldw r3, r13+4
	stw r4+28, r3
	stw r4+24, r1
	ldbu r1, r13+8
	stb r4+32, r1
	ldw r1, r11+12
	addi r1, r1, 36
	stw r11+12, r1
	ldw r1, r11+20
	ldbu r1, r1+34
	andi r1, r1, 10
	addi r5, r0, 1
	addi r3, r0, 0
	beq r1, r3, .LBB7_20
.LBB7_19:
	add r3, r11, r0
	jal r31, luaD_callnoyield
	jal r0, .LBB7_21
.LBB7_20:
	add r3, r11, r0
	jal r31, luaD_call
.LBB7_21:
	sub r1, r12, r15
	ldw r3, r11+28
	add r1, r3, r1
	ldw r3, r11+12
	addi r4, r3, -12
	stw r11+12, r4
	ldw r4, r3+-12
	ldw r5, r3+-8
	stw r1+4, r5
	stw r1+0, r4
	ldbu r3, r3+-4
	stb r1+8, r3
	addi r16, r0, 1
.LBB7_22:
	add r1, r16, r0
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
.Lfunc_end7:
	.size	callbinTM, .Lfunc_end7-callbinTM
                                        # -- End function
	.hidden	luaT_tryconcatTM                # -- Begin function luaT_tryconcatTM
	.globl	luaT_tryconcatTM
	.p2align	2
	.type	luaT_tryconcatTM,@function
luaT_tryconcatTM:                       # @luaT_tryconcatTM
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
	ldw r1, r3+12
	addi r12, r1, -24
	addi r13, r1, -12
	addi r7, r0, 22
	add r4, r12, r0
	add r5, r13, r0
	add r6, r12, r0
	jal r31, callbinTM
	addi r3, r0, 0
	beq r1, r3, .LBB8_2
.LBB8_1:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB8_2:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, luaG_concaterror
.Lfunc_end8:
	.size	luaT_tryconcatTM, .Lfunc_end8-luaT_tryconcatTM
                                        # -- End function
	.hidden	luaT_trybinassocTM              # -- Begin function luaT_trybinassocTM
	.globl	luaT_trybinassocTM
	.p2align	2
	.type	luaT_trybinassocTM,@function
luaT_trybinassocTM:                     # @luaT_trybinassocTM
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
	add r14, r8, r0
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	addi r15, r0, 0
	beq r6, r15, .LBB9_6
.LBB9_1:
	add r4, r11, r0
	add r5, r12, r0
	add r6, r7, r0
	add r7, r14, r0
	jal r31, callbinTM
	bne r1, r15, .LBB9_7
.LBB9_2:
	addi r1, r14, -13
	addi r3, r0, 5
	bgeu r1, r3, .LBB9_16
.LBB9_3:
	ldbu r1, r11+8
	andi r3, r1, 15
	addi r1, r0, 3
	bne r3, r1, .LBB9_8
.LBB9_4:
	ldbu r3, r12+8
	andi r3, r3, 15
	bne r3, r1, .LBB9_8
.LBB9_5:
	add r3, r13, r0
	add r4, r11, r0
	add r5, r12, r0
	jal r31, luaG_tointerror
.LBB9_6:
	add r4, r12, r0
	add r5, r11, r0
	add r6, r7, r0
	add r7, r14, r0
	jal r31, callbinTM
	beq r1, r15, .LBB9_10
.LBB9_7:
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
.LBB9_8:
	lui r6, %hi(.L.str.36)
	addi r6, r6, %lo(.L.str.36)
.LBB9_9:
	add r3, r13, r0
	add r4, r11, r0
	add r5, r12, r0
	jal r31, luaG_opinterror
.LBB9_10:
	addi r1, r14, -13
	addi r3, r0, 5
	bgeu r1, r3, .LBB9_18
.LBB9_11:
	ldbu r1, r12+8
	andi r3, r1, 15
	addi r1, r0, 3
	bne r3, r1, .LBB9_14
.LBB9_12:
	ldbu r3, r11+8
	andi r3, r3, 15
	bne r3, r1, .LBB9_14
.LBB9_13:
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, luaG_tointerror
.LBB9_14:
	lui r6, %hi(.L.str.36)
	addi r6, r6, %lo(.L.str.36)
.LBB9_15:
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, luaG_opinterror
.LBB9_16:
	addi r1, r0, 19
	beq r14, r1, .LBB9_3
.LBB9_17:
	lui r6, %hi(.L.str.37)
	addi r6, r6, %lo(.L.str.37)
	jal r0, .LBB9_9
.LBB9_18:
	addi r1, r0, 19
	beq r14, r1, .LBB9_11
.LBB9_19:
	lui r6, %hi(.L.str.37)
	addi r6, r6, %lo(.L.str.37)
	jal r0, .LBB9_15
.Lfunc_end9:
	.size	luaT_trybinassocTM, .Lfunc_end9-luaT_trybinassocTM
                                        # -- End function
	.hidden	luaT_trybiniTM                  # -- Begin function luaT_trybiniTM
	.globl	luaT_trybiniTM
	.p2align	2
	.type	luaT_trybiniTM,@function
luaT_trybiniTM:                         # @luaT_trybiniTM
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r1, fp, -16
	stw r1+0, r5
	addi r5, r0, 3
	stb r1+8, r5
	add r5, r1, r0
	jal r31, luaT_trybinassocTM
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end10:
	.size	luaT_trybiniTM, .Lfunc_end10-luaT_trybiniTM
                                        # -- End function
	.hidden	luaT_callorderTM                # -- Begin function luaT_callorderTM
	.globl	luaT_callorderTM
	.p2align	2
	.type	luaT_callorderTM,@function
luaT_callorderTM:                       # @luaT_callorderTM
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
	add r7, r6, r0
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	ldw r6, r3+12
	jal r31, callbinTM
	addi r3, r0, 0
	beq r1, r3, .LBB11_2
.LBB11_1:
	ldw r1, r13+12
	ldbu r1, r1+8
	addi r4, r0, 1
	sne r4, r1, r4
	andi r1, r1, 15
	sne r1, r1, r3
	and r1, r4, r1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB11_2:
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, luaG_ordererror
.Lfunc_end11:
	.size	luaT_callorderTM, .Lfunc_end11-luaT_callorderTM
                                        # -- End function
	.hidden	luaT_callorderiTM               # -- Begin function luaT_callorderiTM
	.globl	luaT_callorderiTM
	.p2align	2
	.type	luaT_callorderiTM,@function
luaT_callorderiTM:                      # @luaT_callorderiTM
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
	addi r14, r0, 0
	beq r7, r14, .LBB12_2
.LBB12_1:
	fcvt.d.w r12, r5
	addi r1, fp, -32
	stw r1+4, r13
	stw r1+0, r12
	addi r1, r0, 19
	jal r0, .LBB12_3
.LBB12_2:
	addi r1, fp, -32
	stw r1+0, r5
	addi r1, r0, 3
.LBB12_3:
	addi r3, fp, -32
	stb r3+8, r1
	seq r1, r6, r14
	xor r5, r4, r3
	sub r1, r14, r1
	and r1, r5, r1
	xor r12, r4, r1
	xor r13, r3, r1
	ldw r6, r11+12
	add r3, r11, r0
	add r4, r13, r0
	add r5, r12, r0
	add r7, r8, r0
	jal r31, callbinTM
	beq r1, r14, .LBB12_5
.LBB12_4:
	ldw r1, r11+12
	ldbu r1, r1+8
	addi r3, r0, 1
	sne r3, r1, r3
	andi r1, r1, 15
	sne r1, r1, r14
	and r1, r3, r1
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB12_5:
	add r3, r11, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, luaG_ordererror
.Lfunc_end12:
	.size	luaT_callorderiTM, .Lfunc_end12-luaT_callorderiTM
                                        # -- End function
	.hidden	luaT_adjustvarargs              # -- Begin function luaT_adjustvarargs
	.globl	luaT_adjustvarargs
	.p2align	2
	.type	luaT_adjustvarargs,@function
luaT_adjustvarargs:                     # @luaT_adjustvarargs
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
	ldw r1, r3+12
	ldw r7, r5+0
	sub r12, r1, r7
	srai r1, r12, 2
	lui r7, 699051
	addi r7, r7, -1365
	mul r1, r1, r7
	addi r8, r0, -1
	xor r8, r4, r8
	add r1, r1, r8
	stw r5+24, r1
	ldw r1, r3+24
	ldw r8, r3+12
	sub r1, r1, r8
	srai r1, r1, 2
	mul r7, r1, r7
	ldbu r1, r6+8
	addi r1, r1, 1
	ble r7, r1, .LBB13_5
.LBB13_1:
	ldw r1, r3+12
	addi r6, r1, 12
	stw r3+12, r6
	ldw r6, r5+0
	ldw r7, r6+0
	ldw r8, r6+4
	stw r1+4, r8
	stw r1+0, r7
	ldbu r6, r6+8
	stb r1+8, r6
	addi r1, r0, 1
	blt r4, r1, .LBB13_4
.LBB13_2:
	addi r1, r0, 20
	addi r6, r0, 0
.LBB13_3:
	ldw r7, r3+12
	addi r8, r7, 12
	stw r3+12, r8
	ldw r8, r5+0
	add r8, r8, r1
	ldw r9, r8+-8
	ldw r10, r8+-4
	stw r7+4, r10
	stw r7+0, r9
	ldbu r8, r8+0
	stb r7+8, r8
	ldw r7, r5+0
	add r7, r7, r1
	stb r7+0, r6
	addi r4, r4, -1
	addi r1, r1, 12
	bne r4, r6, .LBB13_3
.LBB13_4:
	ldw r1, r5+0
	add r1, r1, r12
	stw r5+0, r1
	ldw r1, r5+4
	add r1, r1, r12
	stw r5+4, r1
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB13_5:
	addi r6, r0, 1
	add r11, r3, r0
	add r13, r4, r0
	add r4, r1, r0
	add r14, r5, r0
	add r5, r6, r0
	jal r31, luaD_growstack
	add r3, r11, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r0, .LBB13_1
.Lfunc_end13:
	.size	luaT_adjustvarargs, .Lfunc_end13-luaT_adjustvarargs
                                        # -- End function
	.hidden	luaT_getvarargs                 # -- Begin function luaT_getvarargs
	.globl	luaT_getvarargs
	.p2align	2
	.type	luaT_getvarargs,@function
luaT_getvarargs:                        # @luaT_getvarargs
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
	ldw r11, r4+24
	addi r1, r0, -1
	bgt r6, r1, .LBB14_3
.LBB14_1:
	ldw r1, r3+24
	ldw r6, r3+12
	sub r1, r1, r6
	srai r1, r1, 2
	lui r6, 699051
	addi r6, r6, -1365
	mul r1, r1, r6
	ble r1, r11, .LBB14_10
.LBB14_2:
	addi r1, r0, 12
	mul r1, r11, r1
	add r1, r5, r1
	stw r3+12, r1
	add r6, r11, r0
.LBB14_3:
	xor r3, r6, r11
	slt r7, r6, r11
	addi r1, r0, 0
	sub r7, r1, r7
	and r3, r3, r7
	xor r3, r11, r3
	addi r7, r0, 1
	blt r3, r7, .LBB14_6
.LBB14_4:
	addi r1, r0, -12
	mul r7, r11, r1
	addi r8, r5, 8
	addi r1, r0, 0
.LBB14_5:
	ldw r9, r4+0
	add r9, r9, r7
	ldw r10, r9+0
	ldw r11, r9+4
	stw r8+-4, r11
	stw r8+-8, r10
	ldbu r9, r9+8
	stb r8+0, r9
	addi r1, r1, 1
	addi r7, r7, 12
	addi r8, r8, 12
	bne r3, r1, .LBB14_5
.LBB14_6:
	ble r6, r1, .LBB14_9
.LBB14_7:
	sub r3, r6, r1
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r1, r5
	addi r1, r1, 8
	addi r4, r0, 0
.LBB14_8:
	stb r1+0, r4
	addi r3, r3, -1
	addi r1, r1, 12
	bne r3, r4, .LBB14_8
.LBB14_9:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB14_10:
	add r13, r4, r0
	ldw r1, r3+28
	sub r14, r5, r1
	ldw r1, r3+16
	ldw r1, r1+12
	addi r4, r0, 1
	add r12, r3, r0
	blt r1, r4, .LBB14_12
.LBB14_11:
	add r3, r12, r0
	jal r31, luaC_step
.LBB14_12:
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaD_growstack
	add r3, r12, r0
	ldw r1, r12+28
	add r5, r1, r14
	add r4, r13, r0
	jal r0, .LBB14_2
.Lfunc_end14:
	.size	luaT_getvarargs, .Lfunc_end14-luaT_getvarargs
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"no value"
	.size	.L.str, 9

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"nil"
	.size	.L.str.1, 4

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"boolean"
	.size	.L.str.2, 8

	.type	udatatypename,@object           # @udatatypename
	.section	.rodata,"a",@progbits
udatatypename:
	.asciz	"userdata"
	.size	udatatypename, 9

	.type	.L.str.3,@object                # @.str.3
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.3:
	.asciz	"number"
	.size	.L.str.3, 7

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"string"
	.size	.L.str.4, 7

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"table"
	.size	.L.str.5, 6

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"function"
	.size	.L.str.6, 9

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"thread"
	.size	.L.str.7, 7

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"upvalue"
	.size	.L.str.8, 8

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"proto"
	.size	.L.str.9, 6

	.hidden	luaT_typenames_                 # @luaT_typenames_
	.type	luaT_typenames_,@object
	.section	.rodata,"a",@progbits
	.globl	luaT_typenames_
	.p2align	2, 0x0
luaT_typenames_:
	.word	.L.str
	.word	.L.str.1
	.word	.L.str.2
	.word	udatatypename
	.word	.L.str.3
	.word	.L.str.4
	.word	.L.str.5
	.word	.L.str.6
	.word	udatatypename
	.word	.L.str.7
	.word	.L.str.8
	.word	.L.str.9
	.size	luaT_typenames_, 48

	.type	luaT_init.luaT_eventname,@object # @luaT_init.luaT_eventname
	.p2align	2, 0x0
luaT_init.luaT_eventname:
	.word	.L.str.10
	.word	.L.str.11
	.word	.L.str.12
	.word	.L.str.13
	.word	.L.str.14
	.word	.L.str.15
	.word	.L.str.16
	.word	.L.str.17
	.word	.L.str.18
	.word	.L.str.19
	.word	.L.str.20
	.word	.L.str.21
	.word	.L.str.22
	.word	.L.str.23
	.word	.L.str.24
	.word	.L.str.25
	.word	.L.str.26
	.word	.L.str.27
	.word	.L.str.28
	.word	.L.str.29
	.word	.L.str.30
	.word	.L.str.31
	.word	.L.str.32
	.word	.L.str.33
	.word	.L.str.34
	.size	luaT_init.luaT_eventname, 100

	.type	.L.str.10,@object               # @.str.10
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.10:
	.asciz	"__index"
	.size	.L.str.10, 8

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"__newindex"
	.size	.L.str.11, 11

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"__gc"
	.size	.L.str.12, 5

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"__mode"
	.size	.L.str.13, 7

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"__len"
	.size	.L.str.14, 6

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"__eq"
	.size	.L.str.15, 5

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"__add"
	.size	.L.str.16, 6

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"__sub"
	.size	.L.str.17, 6

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"__mul"
	.size	.L.str.18, 6

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"__mod"
	.size	.L.str.19, 6

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"__pow"
	.size	.L.str.20, 6

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"__div"
	.size	.L.str.21, 6

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"__idiv"
	.size	.L.str.22, 7

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"__band"
	.size	.L.str.23, 7

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"__bor"
	.size	.L.str.24, 6

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"__bxor"
	.size	.L.str.25, 7

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"__shl"
	.size	.L.str.26, 6

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"__shr"
	.size	.L.str.27, 6

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"__unm"
	.size	.L.str.28, 6

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"__bnot"
	.size	.L.str.29, 7

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"__lt"
	.size	.L.str.30, 5

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"__le"
	.size	.L.str.31, 5

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"__concat"
	.size	.L.str.32, 9

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"__call"
	.size	.L.str.33, 7

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"__close"
	.size	.L.str.34, 8

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"__name"
	.size	.L.str.35, 7

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"perform bitwise operation on"
	.size	.L.str.36, 29

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"perform arithmetic on"
	.size	.L.str.37, 22

	.hidden	luaS_new
	.hidden	luaC_fix
	.hidden	luaH_getshortstr
	.hidden	luaD_call
	.hidden	luaD_callnoyield
	.hidden	luaG_tointerror
	.hidden	luaG_opinterror
	.hidden	luaG_concaterror
	.hidden	luaG_ordererror
	.hidden	luaD_growstack
	.hidden	luaC_step
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
