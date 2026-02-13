	.file	"lfunc.c"
	.text
	.hidden	luaF_newCclosure                # -- Begin function luaF_newCclosure
	.globl	luaF_newCclosure
	.p2align	2
	.type	luaF_newCclosure,@function
luaF_newCclosure:                       # @luaF_newCclosure
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r4, r0
	addi r1, r0, 12
	mul r1, r4, r1
	addi r5, r1, 16
	addi r4, r0, 38
	jal r31, luaC_newobj
	stb r1+6, r11
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaF_newCclosure, .Lfunc_end0-luaF_newCclosure
                                        # -- End function
	.hidden	luaF_newLclosure                # -- Begin function luaF_newLclosure
	.globl	luaF_newLclosure
	.p2align	2
	.type	luaF_newLclosure,@function
luaF_newLclosure:                       # @luaF_newLclosure
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
	slli r12, r4, 2
	addi r5, r12, 16
	addi r4, r0, 6
	jal r31, luaC_newobj
	addi r3, r0, 0
	stw r1+12, r3
	stb r1+6, r11
	beq r11, r3, .LBB1_3
.LBB1_1:
	add r4, r12, r1
	addi r4, r4, 12
.LBB1_2:
	addi r11, r11, -1
	stw r4+0, r3
	addi r4, r4, -4
	bne r11, r3, .LBB1_2
.LBB1_3:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	luaF_newLclosure, .Lfunc_end1-luaF_newLclosure
                                        # -- End function
	.hidden	luaF_initupvals                 # -- Begin function luaF_initupvals
	.globl	luaF_initupvals
	.p2align	2
	.type	luaF_initupvals,@function
luaF_initupvals:                        # @luaF_initupvals
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
	ldbu r1, r4+6
	addi r15, r0, 0
	beq r1, r15, .LBB2_6
.LBB2_1:
	add r11, r4, r0
	add r12, r3, r0
	addi r16, r4, 16
	addi r13, r0, 9
	addi r14, r0, 24
	add r17, r15, r0
	jal r0, .LBB2_3
.LBB2_2:
	addi r17, r17, 1
	ldbu r1, r11+6
	addi r16, r16, 4
	bgeu r17, r1, .LBB2_6
.LBB2_3:
	add r3, r12, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, luaC_newobj
	addi r3, r1, 12
	stw r1+8, r3
	stb r1+20, r15
	stw r16+0, r1
	ldbu r3, r11+5
	andi r3, r3, 32
	beq r3, r15, .LBB2_2
.LBB2_4:
	ldbu r3, r1+5
	andi r3, r3, 24
	beq r3, r15, .LBB2_2
.LBB2_5:
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, luaC_barrier_
	jal r0, .LBB2_2
.LBB2_6:
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
.Lfunc_end2:
	.size	luaF_initupvals, .Lfunc_end2-luaF_initupvals
                                        # -- End function
	.hidden	luaF_findupval                  # -- Begin function luaF_findupval
	.globl	luaF_findupval
	.p2align	2
	.type	luaF_findupval,@function
luaF_findupval:                         # @luaF_findupval
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
	addi r14, r3, 32
	ldw r3, r3+32
	addi r13, r0, 0
	beq r3, r13, .LBB3_6
.LBB3_1:
	ldw r4, r3+8
	bltu r4, r12, .LBB3_6
.LBB3_2:
	add r1, r3, r0
	beq r4, r12, .LBB3_10
.LBB3_3:
	ldw r3, r1+12
	beq r3, r13, .LBB3_5
.LBB3_4:
	ldw r4, r3+8
	bgeu r4, r12, .LBB3_2
.LBB3_5:
	addi r14, r1, 12
.LBB3_6:
	addi r4, r0, 9
	addi r5, r0, 24
	add r3, r11, r0
	jal r31, luaC_newobj
	ldw r3, r14+0
	stw r1+8, r12
	stw r1+12, r3
	stw r1+16, r14
	beq r3, r13, .LBB3_8
.LBB3_7:
	addi r4, r1, 12
	stw r3+16, r4
.LBB3_8:
	stw r14+0, r1
	ldw r3, r11+44
	bne r3, r11, .LBB3_10
.LBB3_9:
	ldw r3, r11+16
	ldw r4, r3+144
	stw r11+44, r4
	stw r3+144, r11
.LBB3_10:
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
	.size	luaF_findupval, .Lfunc_end3-luaF_findupval
                                        # -- End function
	.hidden	luaF_newtbcupval                # -- Begin function luaF_newtbcupval
	.globl	luaF_newtbcupval
	.p2align	2
	.type	luaF_newtbcupval,@function
luaF_newtbcupval:                       # @luaF_newtbcupval
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
	ldbu r1, r4+8
	addi r3, r0, 1
	beq r1, r3, .LBB4_7
.LBB4_1:
	andi r1, r1, 15
	addi r12, r0, 0
	beq r1, r12, .LBB4_7
.LBB4_2:
	add r13, r4, r0
	addi r5, r0, 24
	add r3, r11, r0
	jal r31, luaT_gettmbyobj
	ldbu r1, r1+8
	andi r1, r1, 15
	beq r1, r12, .LBB4_8
.LBB4_3:
	ldw r1, r11+36
	sub r3, r13, r1
	srai r4, r3, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r7, r4, r3
	lui r6, 16
	bltu r7, r6, .LBB4_6
.LBB4_4:
	lui r5, 192
	addi r4, r5, -12
	addi r5, r5, -2
	addi r6, r6, -1
.LBB4_5:
	add r7, r1, r4
	stw r11+36, r7
	add r1, r1, r5
	sth r1+0, r12
	ldw r1, r11+36
	sub r7, r13, r1
	srai r7, r7, 2
	mul r7, r7, r3
	bgtu r7, r6, .LBB4_5
.LBB4_6:
	sth r13+10, r7
	stw r11+36, r13
.LBB4_7:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB4_8:
	ldw r4, r11+20
	ldw r1, r4+0
	sub r1, r13, r1
	srai r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r5, r1, r3
	add r3, r11, r0
	add r6, r12, r0
	jal r31, luaG_findlocal
	seq r3, r1, r12
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	xor r4, r1, r4
	sub r3, r12, r3
	and r3, r4, r3
	xor r5, r1, r3
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	add r3, r11, r0
	jal r31, luaG_runerror
.Lfunc_end4:
	.size	luaF_newtbcupval, .Lfunc_end4-luaF_newtbcupval
                                        # -- End function
	.hidden	luaF_unlinkupval                # -- Begin function luaF_unlinkupval
	.globl	luaF_unlinkupval
	.p2align	2
	.type	luaF_unlinkupval,@function
luaF_unlinkupval:                       # @luaF_unlinkupval
# %bb.0:
	ldw r1, r3+12
	ldw r4, r3+16
	stw r4+0, r1
	ldw r1, r3+12
	addi r4, r0, 0
	beq r1, r4, .LBB5_2
.LBB5_1:
	ldw r3, r3+16
	stw r1+16, r3
.LBB5_2:
	jalr r0, r31, 0
.Lfunc_end5:
	.size	luaF_unlinkupval, .Lfunc_end5-luaF_unlinkupval
                                        # -- End function
	.hidden	luaF_closeupval                 # -- Begin function luaF_closeupval
	.globl	luaF_closeupval
	.p2align	2
	.type	luaF_closeupval,@function
luaF_closeupval:                        # @luaF_closeupval
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
	ldw r4, r3+32
	addi r13, r0, 0
	beq r4, r13, .LBB6_10
.LBB6_1:
	add r12, r3, r0
	jal r0, .LBB6_3
.LBB6_2:
	ldw r4, r12+32
	beq r4, r13, .LBB6_10
.LBB6_3:
	ldw r1, r4+8
	bltu r1, r11, .LBB6_10
.LBB6_4:
	ldw r1, r4+12
	ldw r3, r4+16
	stw r3+0, r1
	ldw r1, r4+12
	beq r1, r13, .LBB6_6
.LBB6_5:
	ldw r3, r4+16
	stw r1+16, r3
.LBB6_6:
	addi r1, r4, 12
	ldw r3, r4+8
	ldw r5, r3+0
	ldw r6, r3+4
	stw r4+16, r6
	stw r4+12, r5
	ldbu r3, r3+8
	stb r4+20, r3
	stw r4+8, r1
	ldbu r5, r4+5
	andi r6, r5, 24
	bne r6, r13, .LBB6_2
.LBB6_7:
	ori  r5, r5, 32
	stb r4+5, r5
	andi r3, r3, 64
	beq r3, r13, .LBB6_2
.LBB6_8:
	ldw r5, r1+0
	ldbu r1, r5+5
	andi r1, r1, 24
	beq r1, r13, .LBB6_2
.LBB6_9:
	add r3, r12, r0
	jal r31, luaC_barrier_
	jal r0, .LBB6_2
.LBB6_10:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	luaF_closeupval, .Lfunc_end6-luaF_closeupval
                                        # -- End function
	.hidden	luaF_close                      # -- Begin function luaF_close
	.globl	luaF_close
	.p2align	2
	.type	luaF_close,@function
luaF_close:                             # @luaF_close
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
	stw fp+-40, r20
	stw fp+-44, r21
	stw fp+-48, r22
	stw fp+-52, lr
	add r11, r6, r0
	add r12, r5, r0
	add r17, r4, r0
	add r13, r3, r0
	ldw r15, r3+28
	ldw r4, r3+32
	addi r14, r0, 0
	bne r4, r14, .LBB7_4
.LBB7_1:
	ldw r16, r13+36
	bgeu r16, r17, .LBB7_11
.LBB7_2:
	add r1, r17, r0
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
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB7_3:
	ldw r4, r13+32
	beq r4, r14, .LBB7_1
.LBB7_4:
	ldw r1, r4+8
	bltu r1, r17, .LBB7_1
.LBB7_5:
	ldw r1, r4+12
	ldw r3, r4+16
	stw r3+0, r1
	ldw r1, r4+12
	beq r1, r14, .LBB7_7
.LBB7_6:
	ldw r3, r4+16
	stw r1+16, r3
.LBB7_7:
	addi r1, r4, 12
	ldw r3, r4+8
	ldw r5, r3+0
	ldw r6, r3+4
	stw r4+16, r6
	stw r4+12, r5
	ldbu r3, r3+8
	stb r4+20, r3
	stw r4+8, r1
	ldbu r5, r4+5
	andi r6, r5, 24
	bne r6, r14, .LBB7_3
.LBB7_8:
	ori  r5, r5, 32
	stb r4+5, r5
	andi r3, r3, 64
	beq r3, r14, .LBB7_3
.LBB7_9:
	ldw r5, r1+0
	ldbu r1, r5+5
	andi r1, r1, 24
	beq r1, r14, .LBB7_3
.LBB7_10:
	add r3, r13, r0
	jal r31, luaC_barrier_
	jal r0, .LBB7_3
.LBB7_11:
	sub r19, r17, r15
	addi r20, r0, 12
	lui r1, 1048384
	addi r21, r1, 12
	addi r22, r0, -1
	addi r15, r0, 24
	jal r0, .LBB7_14
.LBB7_12:
	jal r31, luaD_callnoyield
.LBB7_13:
	ldw r1, r13+28
	add r17, r1, r19
	ldw r16, r13+36
	bltu r16, r17, .LBB7_2
.LBB7_14:
	ldhu r1, r16+10
	sub r1, r14, r1
	mul r1, r1, r20
	add r1, r16, r1
	ldw r3, r13+28
	bleu r1, r3, .LBB7_17
.LBB7_15:
	ldhu r4, r1+10
	bne r4, r14, .LBB7_17
.LBB7_16:
	add r1, r1, r21
	bgtu r1, r3, .LBB7_15
.LBB7_17:
	stw r13+36, r1
	beq r12, r22, .LBB7_19
.LBB7_18:
	addi r17, r16, 12
	add r3, r13, r0
	add r4, r12, r0
	add r5, r17, r0
	jal r31, luaD_seterrorobj
	jal r0, .LBB7_20
.LBB7_19:
	ldw r1, r13+16
	addi r17, r1, 48
.LBB7_20:
	ldw r18, r13+12
	add r3, r13, r0
	add r4, r16, r0
	add r5, r15, r0
	jal r31, luaT_gettmbyobj
	ldw r3, r1+0
	ldw r4, r1+4
	stw r18+4, r4
	stw r18+0, r3
	ldbu r1, r1+8
	stb r18+8, r1
	ldw r1, r16+0
	ldw r3, r16+4
	stw r18+16, r3
	stw r18+12, r1
	ldbu r1, r16+8
	stb r18+20, r1
	ldw r1, r17+0
	ldw r3, r17+4
	stw r18+28, r3
	stw r18+24, r1
	ldbu r1, r17+8
	stb r18+32, r1
	addi r1, r18, 36
	stw r13+12, r1
	add r3, r13, r0
	add r4, r18, r0
	add r5, r14, r0
	beq r11, r14, .LBB7_12
.LBB7_21:
	jal r31, luaD_call
	jal r0, .LBB7_13
.Lfunc_end7:
	.size	luaF_close, .Lfunc_end7-luaF_close
                                        # -- End function
	.hidden	luaF_newproto                   # -- Begin function luaF_newproto
	.globl	luaF_newproto
	.p2align	2
	.type	luaF_newproto,@function
luaF_newproto:                          # @luaF_newproto
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r4, r0, 10
	addi r5, r0, 84
	jal r31, luaC_newobj
	addi r3, r0, 0
	stw r1+48, r3
	stw r1+16, r3
	stw r1+56, r3
	stw r1+28, r3
	stw r1+52, r3
	stw r1+20, r3
	stw r1+64, r3
	stw r1+24, r3
	stw r1+68, r3
	stw r1+36, r3
	stw r1+60, r3
	stw r1+12, r3
	sth r1+6, r3
	stb r1+8, r3
	stw r1+72, r3
	stw r1+32, r3
	stw r1+40, r3
	stw r1+44, r3
	stw r1+76, r3
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	luaF_newproto, .Lfunc_end8-luaF_newproto
                                        # -- End function
	.hidden	luaF_freeproto                  # -- Begin function luaF_freeproto
	.globl	luaF_freeproto
	.p2align	2
	.type	luaF_freeproto,@function
luaF_freeproto:                         # @luaF_freeproto
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
	ldw r4, r4+52
	ldw r1, r11+20
	slli r5, r1, 2
	jal r31, luaM_free_
	ldw r4, r11+56
	ldw r1, r11+28
	slli r5, r1, 2
	add r3, r12, r0
	jal r31, luaM_free_
	ldw r4, r11+48
	ldw r1, r11+16
	addi r13, r0, 12
	mul r5, r1, r13
	add r3, r12, r0
	jal r31, luaM_free_
	ldw r4, r11+64
	ldw r5, r11+24
	add r3, r12, r0
	jal r31, luaM_free_
	ldw r4, r11+68
	ldw r1, r11+36
	slli r5, r1, 3
	add r3, r12, r0
	jal r31, luaM_free_
	ldw r4, r11+72
	ldw r1, r11+32
	mul r5, r1, r13
	add r3, r12, r0
	jal r31, luaM_free_
	ldw r4, r11+60
	ldw r1, r11+12
	slli r5, r1, 3
	add r3, r12, r0
	jal r31, luaM_free_
	addi r5, r0, 84
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaM_free_
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end9:
	.size	luaF_freeproto, .Lfunc_end9-luaF_freeproto
                                        # -- End function
	.hidden	luaF_getlocalname               # -- Begin function luaF_getlocalname
	.globl	luaF_getlocalname
	.p2align	2
	.type	luaF_getlocalname,@function
luaF_getlocalname:                      # @luaF_getlocalname
# %bb.0:
	ldw r6, r3+32
	addi r1, r0, 1
	blt r6, r1, .LBB10_7
.LBB10_1:
	ldw r1, r3+72
	addi r3, r1, 4
	addi r1, r0, 0
	jal r0, .LBB10_3
.LBB10_2:
	addi r6, r6, -1
	addi r3, r3, 12
	beq r6, r1, .LBB10_8
.LBB10_3:
	ldw r7, r3+0
	bgt r7, r5, .LBB10_8
.LBB10_4:
	ldw r7, r3+4
	bge r5, r7, .LBB10_2
.LBB10_5:
	addi r4, r4, -1
	bne r4, r1, .LBB10_2
.LBB10_6:
	ldw r1, r3+-4
	addi r1, r1, 16
	jal r0, .LBB10_8
.LBB10_7:
	addi r1, r0, 0
.LBB10_8:
	jalr r0, r31, 0
.Lfunc_end10:
	.size	luaF_getlocalname, .Lfunc_end10-luaF_getlocalname
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"?"
	.size	.L.str, 2

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"variable '%s' got a non-closable value"
	.size	.L.str.1, 39

	.hidden	luaC_newobj
	.hidden	luaC_barrier_
	.hidden	luaM_free_
	.hidden	luaT_gettmbyobj
	.hidden	luaG_findlocal
	.hidden	luaG_runerror
	.hidden	luaD_seterrorobj
	.hidden	luaD_call
	.hidden	luaD_callnoyield
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
