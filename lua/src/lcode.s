	.file	"lcode.c"
	.text
	.hidden	luaK_semerror                   # -- Begin function luaK_semerror
	.globl	luaK_semerror
	.p2align	2
	.type	luaK_semerror,@function
luaK_semerror:                          # @luaK_semerror
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r1, r0, 0
	stw r3+12, r1
	jal r31, luaX_syntaxerror
.Lfunc_end0:
	.size	luaK_semerror, .Lfunc_end0-luaK_semerror
                                        # -- End function
	.hidden	luaK_exp2const                  # -- Begin function luaK_exp2const
	.globl	luaK_exp2const
	.p2align	2
	.type	luaK_exp2const,@function
luaK_exp2const:                         # @luaK_exp2const
# %bb.0:
	ldw r6, r4+12
	ldw r7, r4+16
	addi r1, r0, 0
	bne r6, r7, .LBB1_12
.LBB1_1:
	ldw r6, r4+0
	addi r7, r6, -1
	addi r6, r0, 10
	bgtu r7, r6, .LBB1_12
.LBB1_2:
	addi r6, r0, 1
	slli r7, r7, 2
	lui r8, %hi(.LJTI1_0)
	addi r8, r8, %lo(.LJTI1_0)
	add r7, r8, r7
	ldw r8, r7+0
	add r7, r6, r0
	jalr r0, r8, 0
.LBB1_3:
	addi r7, r0, 0
	jal r0, .LBB1_11
.LBB1_4:
	ldw r1, r4+4
	stw r5+0, r1
	ldbu r1, r1+4
	ori  r7, r1, 64
	jal r0, .LBB1_11
.LBB1_5:
	ldw r1, r3+8
	ldw r1, r1+56
	ldw r1, r1+0
	ldw r3, r4+4
	addi r4, r0, 20
	mul r3, r3, r4
	add r1, r1, r3
	ldw r3, r1+0
	ldw r4, r1+4
	stw r5+4, r4
	stw r5+0, r3
	ldbu r7, r1+8
	jal r0, .LBB1_11
.LBB1_6:
	addi r7, r0, 17
	jal r0, .LBB1_11
.LBB1_7:
	addi r1, r0, 0
	beq r5, r1, .LBB1_13
.LBB1_8:
	ldw r1, r4+8
	ldw r3, r4+4
	stw r5+4, r1
	stw r5+0, r3
	addi r7, r0, 19
	jal r0, .LBB1_11
.LBB1_9:
	addi r1, r0, 0
	beq r5, r1, .LBB1_13
.LBB1_10:
	ldw r1, r4+4
	stw r5+0, r1
	addi r7, r0, 3
.LBB1_11:
	stb r5+8, r7
	add r1, r6, r0
.LBB1_12:
	jalr r0, r31, 0
.LBB1_13:
	addi r1, r0, 1
	jal r0, .LBB1_12
.Lfunc_end1:
	.size	luaK_exp2const, .Lfunc_end1-luaK_exp2const
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI1_0:
	.word	.LBB1_3
	.word	.LBB1_6
	.word	.LBB1_11
	.word	.LBB1_12
	.word	.LBB1_7
	.word	.LBB1_9
	.word	.LBB1_4
	.word	.LBB1_12
	.word	.LBB1_12
	.word	.LBB1_12
	.word	.LBB1_5
                                        # -- End function
	.text
	.hidden	luaK_nil                        # -- Begin function luaK_nil
	.globl	luaK_nil
	.p2align	2
	.type	luaK_nil,@function
luaK_nil:                               # @luaK_nil
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
	add r1, r5, r0
	ldw r5, r3+16
	ldw r6, r3+20
	ble r5, r6, .LBB2_2
.LBB2_1:
	ldw r6, r3+0
	ldw r6, r6+52
	slli r7, r5, 2
	add r6, r6, r7
	addi r6, r6, -4
	jal r0, .LBB2_3
.LBB2_2:
	lui r6, %hi(previousinstruction.invalidinstruction)
	addi r6, r6, %lo(previousinstruction.invalidinstruction)
.LBB2_3:
	ldw r7, r6+0
	andi r8, r7, 127
	addi r9, r0, 8
	bne r8, r9, .LBB2_9
.LBB2_4:
	add r8, r1, r4
	srli r9, r7, 7
	andi r10, r9, 255
	srli r9, r7, 16
	andi r9, r9, 255
	add r9, r10, r9
	bgt r10, r4, .LBB2_6
.LBB2_5:
	addi r11, r9, 1
	ble r4, r11, .LBB2_8
.LBB2_6:
	bgt r4, r10, .LBB2_9
.LBB2_7:
	bgt r10, r8, .LBB2_9
.LBB2_8:
	addi r1, r8, 255
	xor r3, r10, r4
	slt r5, r10, r4
	addi r10, r0, 0
	sub r5, r10, r5
	and r3, r3, r5
	xor r3, r4, r3
	slt r4, r9, r8
	xor r1, r1, r9
	sub r4, r10, r4
	and r1, r1, r4
	xor r1, r9, r1
	lui r4, 1044488
	addi r4, r4, 8
	and r4, r7, r4
	slli r5, r3, 7
	lui r7, 8
	addi r7, r7, -128
	and r5, r5, r7
	or  r4, r5, r4
	sub r1, r1, r3
	slli r1, r1, 16
	lui r3, 4080
	and r1, r1, r3
	or  r1, r4, r1
	stw r6+0, r1
	jal r0, .LBB2_10
.LBB2_9:
	slli r4, r4, 7
	slli r1, r1, 16
	lui r6, 1048560
	add r1, r1, r6
	or  r1, r4, r1
	addi r13, r1, 8
	ldw r11, r3+0
	ldw r1, r3+8
	ldw r1, r1+40
	ldw r4, r11+52
	addi r6, r11, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r7, 262144
	addi r8, r7, -1
	addi r7, r0, 4
	add r12, r3, r0
	add r3, r1, r0
	jal r31, luaM_growaux_
	stw r11+52, r1
	ldw r3, r12+16
	addi r4, r3, 1
	stw r12+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r13
	ldw r1, r12+8
	ldw r5, r1+8
	add r3, r12, r0
	add r4, r11, r0
	jal r31, savelineinfo
.LBB2_10:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	luaK_nil, .Lfunc_end2-luaK_nil
                                        # -- End function
	.hidden	luaK_codeABCk                   # -- Begin function luaK_codeABCk
	.globl	luaK_codeABCk
	.p2align	2
	.type	luaK_codeABCk,@function
luaK_codeABCk:                          # @luaK_codeABCk
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
	slli r1, r5, 7
	or  r1, r1, r4
	slli r3, r6, 16
	or  r1, r1, r3
	slli r3, r7, 24
	or  r1, r1, r3
	slli r3, r8, 15
	or  r13, r1, r3
	ldw r12, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r12+52
	ldw r5, r11+16
	addi r6, r12, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r12+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r13
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r12, r0
	jal r31, savelineinfo
	ldw r1, r11+16
	addi r1, r1, -1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	luaK_codeABCk, .Lfunc_end3-luaK_codeABCk
                                        # -- End function
	.hidden	luaK_concat                     # -- Begin function luaK_concat
	.globl	luaK_concat
	.p2align	2
	.type	luaK_concat,@function
luaK_concat:                            # @luaK_concat
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	addi r1, r0, -1
	beq r5, r1, .LBB4_7
.LBB4_1:
	ldw r10, r4+0
	beq r10, r1, .LBB4_6
.LBB4_2:
	ldw r4, r3+0
	ldw r7, r4+52
	lui r4, 1044480
	addi r8, r4, 1
	addi r9, r0, 0
.LBB4_3:
	add r11, r10, r0
	slli r4, r10, 2
	add r4, r7, r4
	ldw r6, r4+0
	srli r10, r6, 7
	add r10, r10, r8
	seq r12, r10, r1
	add r10, r11, r10
	addi r10, r10, 1
	sub r12, r9, r12
	or  r10, r12, r10
	bne r10, r1, .LBB4_3
.LBB4_4:
	xor r1, r11, r1
	add r1, r5, r1
	lui r5, 4096
	addi r5, r5, -1
	add r5, r1, r5
	lui r7, 8192
	bgeu r5, r7, .LBB4_8
.LBB4_5:
	andi r3, r6, 127
	slli r1, r1, 7
	or  r1, r1, r3
	lui r3, 524288
	addi r3, r3, -128
	add r1, r1, r3
	stw r4+0, r1
	jal r0, .LBB4_7
.LBB4_6:
	stw r4+0, r5
.LBB4_7:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB4_8:
	ldw r3, r3+8
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r31, luaX_syntaxerror
.Lfunc_end4:
	.size	luaK_concat, .Lfunc_end4-luaK_concat
                                        # -- End function
	.hidden	luaK_jump                       # -- Begin function luaK_jump
	.globl	luaK_jump
	.p2align	2
	.type	luaK_jump,@function
luaK_jump:                              # @luaK_jump
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
	ldw r12, r3+0
	ldw r1, r3+8
	ldw r3, r1+40
	ldw r4, r12+52
	ldw r5, r11+16
	addi r6, r12, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r12+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	lui r3, 524288
	addi r3, r3, -200
	stw r1+0, r3
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r12, r0
	jal r31, savelineinfo
	ldw r1, r11+16
	addi r1, r1, -1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	luaK_jump, .Lfunc_end5-luaK_jump
                                        # -- End function
	.hidden	luaK_ret                        # -- Begin function luaK_ret
	.globl	luaK_ret
	.p2align	2
	.type	luaK_ret,@function
luaK_ret:                               # @luaK_ret
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
	addi r1, r0, 1
	seq r1, r5, r1
	addi r3, r0, 0
	sub r1, r3, r1
	andi r1, r1, 14
	xori r6, r1, 70
	seq r7, r5, r3
	sub r3, r3, r7
	addi r1, r1, 1
	and r1, r1, r3
	xor r1, r6, r1
	slli r3, r4, 7
	or  r1, r1, r3
	slli r3, r5, 16
	lui r4, 16
	add r3, r3, r4
	or  r13, r1, r3
	ldw r12, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r12+52
	ldw r5, r11+16
	addi r6, r12, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r12+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r13
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r12, r0
	jal r31, savelineinfo
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	luaK_ret, .Lfunc_end6-luaK_ret
                                        # -- End function
	.hidden	luaK_getlabel                   # -- Begin function luaK_getlabel
	.globl	luaK_getlabel
	.p2align	2
	.type	luaK_getlabel,@function
luaK_getlabel:                          # @luaK_getlabel
# %bb.0:
	ldw r1, r3+16
	stw r3+20, r1
	jalr r0, r31, 0
.Lfunc_end7:
	.size	luaK_getlabel, .Lfunc_end7-luaK_getlabel
                                        # -- End function
	.hidden	luaK_patchlist                  # -- Begin function luaK_patchlist
	.globl	luaK_patchlist
	.p2align	2
	.type	luaK_patchlist,@function
luaK_patchlist:                         # @luaK_patchlist
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r6, r0, 255
	add r7, r5, r0
	jal r31, patchlistaux
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	luaK_patchlist, .Lfunc_end8-luaK_patchlist
                                        # -- End function
	.p2align	2                               # -- Begin function patchlistaux
	.type	patchlistaux,@function
patchlistaux:                           # @patchlistaux
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
	addi r1, r0, -1
	beq r4, r1, .LBB9_15
.LBB9_1:
	ldw r8, r3+0
	ldw r8, r8+52
	slli r10, r6, 7
	lui r17, 8
	addi r9, r17, -128
	and r10, r10, r9
	lui r11, 1044480
	addi r11, r11, 1
	addi r12, r0, 0
	addi r13, r0, 1
	lui r14, %hi(luaP_opmodes)
	addi r14, r14, %lo(luaP_opmodes)
	addi r15, r0, 67
	addi r16, r0, 255
	addi r17, r17, 66
	lui r18, 4096
	addi r18, r18, -1
	lui r19, 8192
	lui r20, 1048568
	addi r20, r20, 67
	lui r21, 524288
	addi r21, r21, -128
.LBB9_2:
	slli r22, r4, 2
	add r22, r8, r22
	blt r4, r13, .LBB9_4
.LBB9_3:
	ldw r23, r22+-4
	andi r23, r23, 127
	add r23, r23, r14
	ldbu r23, r23+0
	andi r23, r23, 16
	bne r23, r12, .LBB9_5
.LBB9_4:
	add r24, r22, r0
	jal r0, .LBB9_6
.LBB9_5:
	addi r24, r22, -4
.LBB9_6:
	ldw r23, r22+0
	ldw r26, r24+0
	andi r27, r26, 127
	xor r25, r4, r1
	bne r27, r15, .LBB9_10
.LBB9_7:
	beq r6, r16, .LBB9_9
.LBB9_8:
	srli r27, r26, 16
	andi r27, r27, 255
	bne r6, r27, .LBB9_11
.LBB9_9:
	srli r27, r26, 9
	and r27, r27, r9
	and r26, r26, r17
	or  r26, r27, r26
	jal r0, .LBB9_12
.LBB9_10:
	add r24, r7, r25
	jal r0, .LBB9_13
.LBB9_11:
	and r26, r26, r20
	or  r26, r26, r10
.LBB9_12:
	stw r24+0, r26
	add r24, r5, r25
.LBB9_13:
	add r25, r24, r18
	bgeu r25, r19, .LBB9_16
.LBB9_14:
	srli r23, r23, 7
	add r23, r23, r11
	seq r25, r23, r1
	add r4, r4, r23
	addi r4, r4, 1
	sub r23, r12, r25
	or  r4, r23, r4
	ldw r23, r22+0
	andi r23, r23, 127
	slli r24, r24, 7
	or  r23, r24, r23
	add r23, r23, r21
	stw r22+0, r23
	bne r4, r1, .LBB9_2
.LBB9_15:
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
.LBB9_16:
	ldw r3, r3+8
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r31, luaX_syntaxerror
.Lfunc_end9:
	.size	patchlistaux, .Lfunc_end9-patchlistaux
                                        # -- End function
	.hidden	luaK_patchtohere                # -- Begin function luaK_patchtohere
	.globl	luaK_patchtohere
	.p2align	2
	.type	luaK_patchtohere,@function
luaK_patchtohere:                       # @luaK_patchtohere
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r5, r3+16
	stw r3+20, r5
	addi r6, r0, 255
	add r7, r5, r0
	jal r31, patchlistaux
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end10:
	.size	luaK_patchtohere, .Lfunc_end10-luaK_patchtohere
                                        # -- End function
	.hidden	luaK_code                       # -- Begin function luaK_code
	.globl	luaK_code
	.p2align	2
	.type	luaK_code,@function
luaK_code:                              # @luaK_code
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
	ldw r13, r3+0
	ldw r1, r3+8
	ldw r3, r1+40
	ldw r4, r13+52
	ldw r5, r12+16
	addi r6, r13, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r13+52, r1
	ldw r3, r12+16
	addi r4, r3, 1
	stw r12+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r11
	ldw r1, r12+8
	ldw r5, r1+8
	add r3, r12, r0
	add r4, r13, r0
	jal r31, savelineinfo
	ldw r1, r12+16
	addi r1, r1, -1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end11:
	.size	luaK_code, .Lfunc_end11-luaK_code
                                        # -- End function
	.p2align	2                               # -- Begin function savelineinfo
	.type	savelineinfo,@function
savelineinfo:                           # @savelineinfo
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
	add r11, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	ldw r1, r3+24
	sub r15, r5, r1
	ldw r1, r3+16
	addi r14, r1, -1
	add r3, r15, r0
	jal r31, abs
	addi r3, r0, 127
	bgt r1, r3, .LBB12_2
.LBB12_1:
	ldb r1, r12+53
	addi r3, r1, 1
	stb r12+53, r3
	addi r3, r0, -1
	bgt r1, r3, .LBB12_3
.LBB12_2:
	ldw r1, r12+8
	ldw r3, r1+40
	ldw r4, r13+68
	ldw r5, r12+36
	addi r6, r13, 36
	lui r9, %hi(.L.str.3)
	addi r9, r9, %lo(.L.str.3)
	lui r1, 131072
	addi r8, r1, -1
	addi r7, r0, 8
	jal r31, luaM_growaux_
	stw r13+68, r1
	ldw r3, r12+36
	slli r4, r3, 3
	add r1, r1, r4
	stw r1+0, r14
	addi r3, r3, 1
	stw r12+36, r3
	stw r1+4, r11
	addi r1, r0, 1
	stb r12+53, r1
	addi r15, r0, -128
.LBB12_3:
	ldw r1, r12+8
	ldw r3, r1+40
	ldw r4, r13+64
	addi r6, r13, 24
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 524288
	addi r8, r1, -1
	addi r7, r0, 1
	add r5, r14, r0
	jal r31, luaM_growaux_
	stw r13+64, r1
	add r1, r1, r14
	stb r1+0, r15
	stw r12+24, r11
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
	.size	savelineinfo, .Lfunc_end12-savelineinfo
                                        # -- End function
	.hidden	luaK_codeABx                    # -- Begin function luaK_codeABx
	.globl	luaK_codeABx
	.p2align	2
	.type	luaK_codeABx,@function
luaK_codeABx:                           # @luaK_codeABx
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
	slli r1, r5, 7
	or  r1, r1, r4
	slli r3, r6, 15
	or  r13, r1, r3
	ldw r12, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r12+52
	ldw r5, r11+16
	addi r6, r12, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r12+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r13
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r12, r0
	jal r31, savelineinfo
	ldw r1, r11+16
	addi r1, r1, -1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end13:
	.size	luaK_codeABx, .Lfunc_end13-luaK_codeABx
                                        # -- End function
	.hidden	luaK_checkstack                 # -- Begin function luaK_checkstack
	.globl	luaK_checkstack
	.p2align	2
	.type	luaK_checkstack,@function
luaK_checkstack:                        # @luaK_checkstack
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldbu r1, r3+52
	add r1, r4, r1
	ldw r4, r3+0
	ldbu r5, r4+8
	ble r1, r5, .LBB14_3
.LBB14_1:
	addi r5, r0, 255
	bge r1, r5, .LBB14_4
.LBB14_2:
	stb r4+8, r1
.LBB14_3:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB14_4:
	ldw r3, r3+8
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, luaX_syntaxerror
.Lfunc_end14:
	.size	luaK_checkstack, .Lfunc_end14-luaK_checkstack
                                        # -- End function
	.hidden	luaK_reserveregs                # -- Begin function luaK_reserveregs
	.globl	luaK_reserveregs
	.p2align	2
	.type	luaK_reserveregs,@function
luaK_reserveregs:                       # @luaK_reserveregs
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldbu r1, r3+52
	add r5, r4, r1
	ldw r6, r3+0
	ldbu r7, r6+8
	ble r5, r7, .LBB15_3
.LBB15_1:
	addi r7, r0, 255
	bge r5, r7, .LBB15_4
.LBB15_2:
	stb r6+8, r5
.LBB15_3:
	add r1, r1, r4
	stb r3+52, r1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB15_4:
	ldw r3, r3+8
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, luaX_syntaxerror
.Lfunc_end15:
	.size	luaK_reserveregs, .Lfunc_end15-luaK_reserveregs
                                        # -- End function
	.hidden	luaK_int                        # -- Begin function luaK_int
	.globl	luaK_int
	.p2align	2
	.type	luaK_int,@function
luaK_int:                               # @luaK_int
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
	lui r1, 1048560
	addi r1, r1, -1
	add r1, r5, r1
	lui r6, 1048544
	bgeu r1, r6, .LBB16_2
.LBB16_1:
	addi r1, fp, -28
	stw r1+0, r5
	addi r5, r0, 3
	stb r1+8, r5
	add r11, r3, r0
	add r12, r4, r0
	add r4, r1, r0
	add r5, r1, r0
	jal r31, addk
	add r3, r11, r0
	add r4, r12, r0
	add r5, r1, r0
	jal r31, luaK_codek
	jal r0, .LBB16_3
.LBB16_2:
	slli r1, r4, 7
	slli r4, r5, 15
	lui r5, 524280
	add r4, r4, r5
	or  r1, r1, r4
	ori  r13, r1, 1
	ldw r11, r3+0
	ldw r1, r3+8
	ldw r1, r1+40
	ldw r4, r11+52
	ldw r5, r3+16
	addi r6, r11, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r7, 262144
	addi r8, r7, -1
	addi r7, r0, 4
	add r12, r3, r0
	add r3, r1, r0
	jal r31, luaM_growaux_
	stw r11+52, r1
	ldw r3, r12+16
	addi r4, r3, 1
	stw r12+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r13
	ldw r1, r12+8
	ldw r5, r1+8
	add r3, r12, r0
	add r4, r11, r0
	jal r31, savelineinfo
.LBB16_3:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end16:
	.size	luaK_int, .Lfunc_end16-luaK_int
                                        # -- End function
	.p2align	2                               # -- Begin function luaK_codek
	.type	luaK_codek,@function
luaK_codek:                             # @luaK_codek
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
	add r12, r5, r0
	add r11, r3, r0
	slli r1, r4, 7
	lui r3, 32
	addi r3, r3, -1
	bgt r5, r3, .LBB17_2
.LBB17_1:
	slli r3, r12, 15
	or  r1, r1, r3
	addi r13, r1, 3
	ldw r12, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r12+52
	ldw r5, r11+16
	addi r6, r12, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r0, .LBB17_3
.LBB17_2:
	addi r17, r1, 4
	ldw r13, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r13+52
	ldw r5, r11+16
	addi r6, r13, 20
	lui r14, %hi(.L.str)
	addi r14, r14, %lo(.L.str)
	lui r1, 262144
	addi r15, r1, -1
	addi r16, r0, 4
	add r7, r16, r0
	add r8, r15, r0
	add r9, r14, r0
	jal r31, luaM_growaux_
	stw r13+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r17
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r13, r0
	jal r31, savelineinfo
	ldw r5, r11+16
	slli r1, r12, 7
	addi r13, r1, 82
	ldw r12, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r12+52
	addi r6, r12, 20
	add r7, r16, r0
	add r8, r15, r0
	add r9, r14, r0
.LBB17_3:
	jal r31, luaM_growaux_
	stw r12+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r13
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r12, r0
	jal r31, savelineinfo
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
.Lfunc_end17:
	.size	luaK_codek, .Lfunc_end17-luaK_codek
                                        # -- End function
	.hidden	luaK_setreturns                 # -- Begin function luaK_setreturns
	.globl	luaK_setreturns
	.p2align	2
	.type	luaK_setreturns,@function
luaK_setreturns:                        # @luaK_setreturns
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r3+0
	ldw r6, r1+52
	ldw r7, r4+4
	slli r7, r7, 2
	add r6, r6, r7
	ldw r7, r4+0
	ldw r4, r6+0
	lui r8, 4096
	addi r9, r8, -1
	and r4, r4, r9
	slli r5, r5, 24
	or  r4, r5, r4
	add r4, r4, r8
	stw r6+0, r4
	addi r5, r0, 18
	beq r7, r5, .LBB18_5
.LBB18_1:
	lui r5, 1048568
	addi r5, r5, 127
	and r4, r4, r5
	ldbu r5, r3+52
	slli r7, r5, 7
	or  r4, r7, r4
	stw r6+0, r4
	addi r4, r5, 1
	ldbu r6, r1+8
	bltu r5, r6, .LBB18_4
.LBB18_2:
	addi r6, r0, 254
	bgeu r5, r6, .LBB18_6
.LBB18_3:
	stb r1+8, r4
.LBB18_4:
	stb r3+52, r4
.LBB18_5:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB18_6:
	ldw r3, r3+8
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, luaX_syntaxerror
.Lfunc_end18:
	.size	luaK_setreturns, .Lfunc_end18-luaK_setreturns
                                        # -- End function
	.hidden	luaK_setoneret                  # -- Begin function luaK_setoneret
	.globl	luaK_setoneret
	.p2align	2
	.type	luaK_setoneret,@function
luaK_setoneret:                         # @luaK_setoneret
# %bb.0:
	ldw r1, r4+0
	addi r5, r0, 19
	beq r1, r5, .LBB19_3
.LBB19_1:
	addi r5, r0, 18
	bne r1, r5, .LBB19_4
.LBB19_2:
	addi r1, r0, 8
	stw r4+0, r1
	ldw r1, r3+0
	ldw r1, r1+52
	ldw r3, r4+4
	slli r3, r3, 2
	add r1, r1, r3
	ldw r1, r1+0
	srli r1, r1, 7
	andi r1, r1, 255
	stw r4+4, r1
	jal r0, .LBB19_4
.LBB19_3:
	ldw r1, r3+0
	ldw r1, r1+52
	ldw r3, r4+4
	slli r3, r3, 2
	add r1, r1, r3
	addi r3, r0, 2
	stb r1+3, r3
	addi r1, r0, 17
	stw r4+0, r1
.LBB19_4:
	jalr r0, r31, 0
.Lfunc_end19:
	.size	luaK_setoneret, .Lfunc_end19-luaK_setoneret
                                        # -- End function
	.hidden	luaK_dischargevars              # -- Begin function luaK_dischargevars
	.globl	luaK_dischargevars
	.p2align	2
	.type	luaK_dischargevars,@function
luaK_dischargevars:                     # @luaK_dischargevars
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
	add r12, r3, r0
	ldw r1, r4+0
	addi r1, r1, -9
	addi r3, r0, 10
	bgtu r1, r3, .LBB20_28
.LBB20_1:
	add r11, r4, r0
	slli r1, r1, 2
	lui r3, %hi(.LJTI20_0)
	addi r3, r3, %lo(.LJTI20_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB20_2:
	ldbu r1, r11+4
	stw r11+4, r1
	addi r1, r0, 8
	jal r0, .LBB20_27
.LBB20_3:
	ldw r1, r12+8
	ldw r1, r1+56
	ldw r1, r1+0
	ldw r3, r11+4
	addi r4, r0, 20
	mul r3, r3, r4
	add r1, r1, r3
	ldbu r3, r1+8
	andi r3, r3, 63
	bgtu r3, r4, .LBB20_28
.LBB20_4:
	slli r3, r3, 2
	lui r4, %hi(.LJTI20_1)
	addi r4, r4, %lo(.LJTI20_1)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB20_5:
	addi r3, r0, 7
	jal r0, .LBB20_34
.LBB20_6:
	ldbu r13, r11+6
	add r3, r12, r0
	jal r31, luaY_nvarstack
	bgt r1, r13, .LBB20_8
.LBB20_7:
	ldbu r1, r12+52
	addi r1, r1, -1
	stb r12+52, r1
.LBB20_8:
	ldbu r1, r11+6
	ldhu r3, r11+4
	slli r1, r1, 16
	slli r3, r3, 24
	or  r1, r1, r3
	ori  r14, r1, 13
	jal r0, .LBB20_25
.LBB20_9:
	ldw r1, r11+4
	slli r1, r1, 16
	addi r14, r1, 9
	jal r0, .LBB20_25
.LBB20_10:
	ldbu r13, r11+6
	add r3, r12, r0
	jal r31, luaY_nvarstack
	bgt r1, r13, .LBB20_12
.LBB20_11:
	ldbu r1, r12+52
	addi r1, r1, -1
	stb r12+52, r1
.LBB20_12:
	ldbu r1, r11+6
	ldhu r3, r11+4
	slli r1, r1, 16
	slli r3, r3, 24
	or  r1, r1, r3
	ori  r14, r1, 14
	jal r0, .LBB20_25
.LBB20_13:
	addi r1, r0, 8
	stw r11+0, r1
	ldw r1, r12+0
	ldw r1, r1+52
	ldw r3, r11+4
	slli r3, r3, 2
	add r1, r1, r3
	ldw r1, r1+0
	srli r1, r1, 7
	andi r1, r1, 255
	stw r11+4, r1
	jal r0, .LBB20_28
.LBB20_14:
	ldbu r13, r11+6
	ldh r14, r11+4
	add r3, r12, r0
	jal r31, luaY_nvarstack
	ble r13, r14, .LBB20_20
.LBB20_15:
	bgt r1, r13, .LBB20_17
.LBB20_16:
	ldbu r1, r12+52
	addi r1, r1, -1
	stb r12+52, r1
.LBB20_17:
	add r3, r12, r0
	jal r31, luaY_nvarstack
	ble r1, r14, .LBB20_23
	jal r0, .LBB20_24
.LBB20_18:
	ldbu r1, r11+6
	ldhu r3, r11+4
	slli r1, r1, 16
	slli r3, r3, 24
	or  r1, r1, r3
	ori  r14, r1, 11
	jal r0, .LBB20_25
.LBB20_19:
	ldw r1, r12+0
	ldw r1, r1+52
	ldw r3, r11+4
	slli r3, r3, 2
	add r1, r1, r3
	addi r3, r0, 2
	stb r1+3, r3
	jal r0, .LBB20_26
.LBB20_20:
	bgt r1, r14, .LBB20_22
.LBB20_21:
	ldbu r1, r12+52
	addi r1, r1, -1
	stb r12+52, r1
.LBB20_22:
	add r3, r12, r0
	jal r31, luaY_nvarstack
	bgt r1, r13, .LBB20_24
.LBB20_23:
	ldbu r1, r12+52
	addi r1, r1, -1
	stb r12+52, r1
.LBB20_24:
	ldbu r1, r11+6
	ldhu r3, r11+4
	slli r1, r1, 16
	slli r3, r3, 24
	or  r1, r1, r3
	ori  r14, r1, 12
.LBB20_25:
	ldw r13, r12+0
	ldw r1, r12+8
	ldw r3, r1+40
	ldw r4, r13+52
	ldw r5, r12+16
	addi r6, r13, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r13+52, r1
	ldw r3, r12+16
	addi r4, r3, 1
	stw r12+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r14
	ldw r1, r12+8
	ldw r5, r1+8
	add r3, r12, r0
	add r4, r13, r0
	jal r31, savelineinfo
	ldw r1, r12+16
	addi r1, r1, -1
	stw r11+4, r1
.LBB20_26:
	addi r1, r0, 17
.LBB20_27:
	stw r11+0, r1
.LBB20_28:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB20_29:
	addi r1, r0, 2
	jal r0, .LBB20_27
.LBB20_30:
	addi r1, r0, 3
	jal r0, .LBB20_27
.LBB20_31:
	addi r1, r0, 1
	jal r0, .LBB20_27
.LBB20_32:
	addi r3, r0, 5
	stw r11+0, r3
	ldw r3, r1+4
	ldw r1, r1+0
	stw r11+8, r3
	stw r11+4, r1
	jal r0, .LBB20_28
.LBB20_33:
	addi r3, r0, 6
.LBB20_34:
	stw r11+0, r3
	ldw r1, r1+0
	stw r11+4, r1
	jal r0, .LBB20_28
.Lfunc_end20:
	.size	luaK_dischargevars, .Lfunc_end20-luaK_dischargevars
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI20_0:
	.word	.LBB20_2
	.word	.LBB20_9
	.word	.LBB20_3
	.word	.LBB20_14
	.word	.LBB20_18
	.word	.LBB20_6
	.word	.LBB20_10
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_13
	.word	.LBB20_19
.LJTI20_1:
	.word	.LBB20_31
	.word	.LBB20_30
	.word	.LBB20_28
	.word	.LBB20_33
	.word	.LBB20_5
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_28
	.word	.LBB20_29
	.word	.LBB20_28
	.word	.LBB20_32
	.word	.LBB20_5
                                        # -- End function
	.text
	.hidden	luaK_exp2nextreg                # -- Begin function luaK_exp2nextreg
	.globl	luaK_exp2nextreg
	.p2align	2
	.type	luaK_exp2nextreg,@function
luaK_exp2nextreg:                       # @luaK_exp2nextreg
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
	jal r31, luaK_dischargevars
	ldw r1, r11+0
	addi r3, r0, 8
	bne r1, r3, .LBB21_3
.LBB21_1:
	ldw r13, r11+4
	add r3, r12, r0
	jal r31, luaY_nvarstack
	blt r13, r1, .LBB21_3
.LBB21_2:
	ldbu r1, r12+52
	addi r1, r1, -1
	stb r12+52, r1
.LBB21_3:
	ldbu r4, r12+52
	addi r1, r4, 1
	ldw r3, r12+0
	ldbu r5, r3+8
	bltu r4, r5, .LBB21_6
.LBB21_4:
	addi r5, r0, 254
	bgeu r4, r5, .LBB21_7
.LBB21_5:
	stb r3+8, r1
.LBB21_6:
	stb r12+52, r1
	andi r1, r1, 255
	addi r5, r1, -1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, exp2reg
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB21_7:
	ldw r3, r12+8
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, luaX_syntaxerror
.Lfunc_end21:
	.size	luaK_exp2nextreg, .Lfunc_end21-luaK_exp2nextreg
                                        # -- End function
	.p2align	2                               # -- Begin function exp2reg
	.type	exp2reg,@function
exp2reg:                                # @exp2reg
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
	stw fp+-44, r21
	stw fp+-48, lr
	add r12, r5, r0
	add r11, r4, r0
	add r13, r3, r0
	jal r31, discharge2reg
	ldw r3, r11+0
	addi r1, r0, 16
	bne r3, r1, .LBB22_8
.LBB22_1:
	ldw r3, r11+4
	addi r4, r0, -1
	beq r3, r4, .LBB22_8
.LBB22_2:
	ldw r10, r11+12
	beq r10, r4, .LBB22_7
.LBB22_3:
	ldw r5, r13+0
	ldw r7, r5+52
	lui r5, 1044480
	addi r8, r5, 1
	addi r9, r0, 0
.LBB22_4:
	add r14, r10, r0
	slli r5, r10, 2
	add r5, r7, r5
	ldw r6, r5+0
	srli r10, r6, 7
	add r10, r10, r8
	seq r15, r10, r4
	add r10, r14, r10
	addi r10, r10, 1
	sub r15, r9, r15
	or  r10, r15, r10
	bne r10, r4, .LBB22_4
.LBB22_5:
	xor r4, r14, r4
	add r3, r3, r4
	lui r4, 4096
	addi r4, r4, -1
	add r4, r3, r4
	lui r7, 8192
	bgeu r4, r7, .LBB22_31
.LBB22_6:
	andi r4, r6, 127
	slli r3, r3, 7
	or  r3, r3, r4
	lui r4, 524288
	addi r4, r4, -128
	add r3, r3, r4
	stw r5+0, r3
	jal r0, .LBB22_8
.LBB22_7:
	stw r11+12, r3
.LBB22_8:
	ldw r6, r11+12
	ldw r3, r11+16
	beq r6, r3, .LBB22_30
.LBB22_9:
	addi r14, r0, -1
	lui r4, %hi(luaP_opmodes)
	addi r4, r4, %lo(luaP_opmodes)
	lui r5, 1044480
	beq r6, r14, .LBB22_17
.LBB22_10:
	ldw r7, r13+0
	ldw r7, r7+52
	addi r8, r0, 1
	addi r9, r0, 0
	addi r10, r0, 67
	addi r15, r5, 1
.LBB22_11:
	slli r16, r6, 2
	add r16, r7, r16
	blt r6, r8, .LBB22_13
.LBB22_12:
	ldw r17, r16+-4
	andi r17, r17, 127
	add r17, r17, r4
	ldbu r17, r17+0
	andi r17, r17, 16
	bne r17, r9, .LBB22_14
.LBB22_13:
	add r17, r16, r0
	jal r0, .LBB22_15
.LBB22_14:
	addi r17, r16, -4
.LBB22_15:
	ldw r17, r17+0
	andi r17, r17, 127
	bne r17, r10, .LBB22_26
.LBB22_16:
	ldw r16, r16+0
	srli r16, r16, 7
	add r16, r16, r15
	seq r17, r16, r14
	add r6, r6, r16
	addi r6, r6, 1
	sub r16, r9, r17
	or  r6, r16, r6
	bne r6, r14, .LBB22_11
.LBB22_17:
	addi r15, r0, -1
	add r16, r15, r0
	beq r3, r15, .LBB22_29
.LBB22_18:
	ldw r6, r13+0
	ldw r6, r6+52
	addi r7, r0, 1
	addi r8, r0, 0
	addi r9, r0, 67
	addi r5, r5, 1
	addi r15, r0, -1
.LBB22_19:
	slli r10, r3, 2
	add r10, r6, r10
	blt r3, r7, .LBB22_21
.LBB22_20:
	ldw r16, r10+-4
	andi r16, r16, 127
	add r16, r16, r4
	ldbu r16, r16+0
	andi r16, r16, 16
	bne r16, r8, .LBB22_22
.LBB22_21:
	add r16, r10, r0
	jal r0, .LBB22_23
.LBB22_22:
	addi r16, r10, -4
.LBB22_23:
	ldw r16, r16+0
	andi r16, r16, 127
	bne r16, r9, .LBB22_26
.LBB22_24:
	ldw r10, r10+0
	srli r10, r10, 7
	add r10, r10, r5
	seq r16, r10, r15
	add r3, r3, r10
	addi r3, r3, 1
	sub r10, r8, r16
	or  r3, r10, r3
	bne r3, r15, .LBB22_19
.LBB22_25:
	add r16, r15, r0
	jal r0, .LBB22_29
.LBB22_26:
	ldw r3, r11+0
	lui r16, 262144
	beq r3, r1, .LBB22_28
.LBB22_27:
	ldw r14, r13+0
	ldw r1, r13+8
	ldw r3, r1+40
	ldw r4, r14+52
	ldw r5, r13+16
	addi r6, r14, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	addi r8, r16, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r14+52, r1
	ldw r3, r13+16
	addi r4, r3, 1
	stw r13+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	lui r3, 524288
	addi r3, r3, -200
	stw r1+0, r3
	ldw r1, r13+8
	ldw r5, r1+8
	add r3, r13, r0
	add r4, r14, r0
	jal r31, savelineinfo
	ldw r1, r13+16
	addi r14, r1, -1
.LBB22_28:
	ldw r5, r13+16
	stw r13+20, r5
	slli r20, r12, 7
	addi r21, r20, 6
	ldw r15, r13+0
	ldw r1, r13+8
	ldw r3, r1+40
	ldw r4, r15+52
	addi r6, r15, 20
	lui r17, %hi(.L.str)
	addi r17, r17, %lo(.L.str)
	addi r18, r16, -1
	addi r19, r0, 4
	add r7, r19, r0
	add r8, r18, r0
	add r9, r17, r0
	jal r31, luaM_growaux_
	stw r15+52, r1
	ldw r3, r13+16
	addi r4, r3, 1
	stw r13+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r21
	ldw r1, r13+8
	ldw r5, r1+8
	add r3, r13, r0
	add r4, r15, r0
	jal r31, savelineinfo
	ldw r5, r13+16
	addi r16, r5, -1
	stw r13+20, r5
	addi r20, r20, 7
	ldw r15, r13+0
	ldw r1, r13+8
	ldw r3, r1+40
	ldw r4, r15+52
	addi r6, r15, 20
	add r7, r19, r0
	add r8, r18, r0
	add r9, r17, r0
	jal r31, luaM_growaux_
	stw r15+52, r1
	ldw r3, r13+16
	addi r4, r3, 1
	stw r13+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r20
	ldw r1, r13+8
	ldw r5, r1+8
	add r3, r13, r0
	add r4, r15, r0
	jal r31, savelineinfo
	ldw r5, r13+16
	addi r15, r5, -1
	stw r13+20, r5
	addi r6, r0, 255
	add r3, r13, r0
	add r4, r14, r0
	add r7, r5, r0
	jal r31, patchlistaux
.LBB22_29:
	ldw r14, r13+16
	stw r13+20, r14
	ldw r4, r11+16
	add r3, r13, r0
	add r5, r14, r0
	add r6, r12, r0
	add r7, r16, r0
	jal r31, patchlistaux
	ldw r4, r11+12
	add r3, r13, r0
	add r5, r14, r0
	add r6, r12, r0
	add r7, r15, r0
	jal r31, patchlistaux
.LBB22_30:
	addi r1, r0, -1
	stw r11+12, r1
	stw r11+16, r1
	stw r11+4, r12
	addi r1, r0, 8
	stw r11+0, r1
	ldw lr, fp+-48
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
	addi sp, sp, 56
	jalr r0, r31, 0
.LBB22_31:
	ldw r3, r13+8
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r31, luaX_syntaxerror
.Lfunc_end22:
	.size	exp2reg, .Lfunc_end22-exp2reg
                                        # -- End function
	.hidden	luaK_exp2anyreg                 # -- Begin function luaK_exp2anyreg
	.globl	luaK_exp2anyreg
	.p2align	2
	.type	luaK_exp2anyreg,@function
luaK_exp2anyreg:                        # @luaK_exp2anyreg
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
	jal r31, luaK_dischargevars
	ldw r1, r11+0
	addi r3, r0, 8
	bne r1, r3, .LBB23_3
.LBB23_1:
	ldw r1, r11+12
	ldw r3, r11+16
	addi r13, r11, 4
	beq r1, r3, .LBB23_4
.LBB23_2:
	ldw r14, r13+0
	add r3, r12, r0
	jal r31, luaY_nvarstack
	bge r14, r1, .LBB23_5
.LBB23_3:
	addi r13, r11, 4
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaK_exp2nextreg
.LBB23_4:
	ldw r1, r13+0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB23_5:
	ldw r5, r11+4
	add r3, r12, r0
	add r4, r11, r0
	jal r31, exp2reg
	jal r0, .LBB23_4
.Lfunc_end23:
	.size	luaK_exp2anyreg, .Lfunc_end23-luaK_exp2anyreg
                                        # -- End function
	.hidden	luaK_exp2anyregup               # -- Begin function luaK_exp2anyregup
	.globl	luaK_exp2anyregup
	.p2align	2
	.type	luaK_exp2anyregup,@function
luaK_exp2anyregup:                      # @luaK_exp2anyregup
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
	ldw r1, r4+0
	addi r3, r0, 10
	bne r1, r3, .LBB24_2
.LBB24_1:
	ldw r1, r11+12
	ldw r3, r11+16
	beq r1, r3, .LBB24_6
.LBB24_2:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaK_dischargevars
	ldw r1, r11+0
	addi r3, r0, 8
	bne r1, r3, .LBB24_5
.LBB24_3:
	ldw r1, r11+12
	ldw r3, r11+16
	beq r1, r3, .LBB24_6
.LBB24_4:
	ldw r13, r11+4
	add r3, r12, r0
	jal r31, luaY_nvarstack
	bge r13, r1, .LBB24_7
.LBB24_5:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaK_exp2nextreg
.LBB24_6:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB24_7:
	ldw r5, r11+4
	add r3, r12, r0
	add r4, r11, r0
	jal r31, exp2reg
	jal r0, .LBB24_6
.Lfunc_end24:
	.size	luaK_exp2anyregup, .Lfunc_end24-luaK_exp2anyregup
                                        # -- End function
	.hidden	luaK_exp2val                    # -- Begin function luaK_exp2val
	.globl	luaK_exp2val
	.p2align	2
	.type	luaK_exp2val,@function
luaK_exp2val:                           # @luaK_exp2val
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
	ldw r13, r4+12
	ldw r14, r4+16
	jal r31, luaK_dischargevars
	beq r13, r14, .LBB25_5
.LBB25_1:
	ldw r1, r11+0
	addi r3, r0, 8
	bne r1, r3, .LBB25_4
.LBB25_2:
	ldw r1, r11+12
	ldw r3, r11+16
	beq r1, r3, .LBB25_5
.LBB25_3:
	ldw r13, r11+4
	add r3, r12, r0
	jal r31, luaY_nvarstack
	bge r13, r1, .LBB25_6
.LBB25_4:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaK_exp2nextreg
.LBB25_5:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB25_6:
	ldw r5, r11+4
	add r3, r12, r0
	add r4, r11, r0
	jal r31, exp2reg
	jal r0, .LBB25_5
.Lfunc_end25:
	.size	luaK_exp2val, .Lfunc_end25-luaK_exp2val
                                        # -- End function
	.hidden	luaK_storevar                   # -- Begin function luaK_storevar
	.globl	luaK_storevar
	.p2align	2
	.type	luaK_storevar,@function
luaK_storevar:                          # @luaK_storevar
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
	ldw r1, r4+0
	addi r1, r1, -9
	addi r3, r0, 6
	bgtu r1, r3, .LBB26_16
.LBB26_1:
	slli r1, r1, 2
	lui r3, %hi(.LJTI26_0)
	addi r3, r3, %lo(.LJTI26_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB26_2:
	ldw r1, r12+0
	addi r3, r0, 8
	bne r1, r3, .LBB26_5
.LBB26_3:
	ldw r14, r12+4
	add r3, r11, r0
	add r13, r4, r0
	jal r31, luaY_nvarstack
	add r4, r13, r0
	blt r14, r1, .LBB26_5
.LBB26_4:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB26_5:
	ldbu r5, r4+4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, exp2reg
	jal r0, .LBB26_19
.LBB26_6:
	ldbu r13, r4+6
	ldhu r14, r4+4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, exp2RK
	ldw r3, r12+4
	slli r4, r13, 7
	slli r5, r14, 16
	or  r4, r4, r5
	slli r3, r3, 24
	slli r1, r1, 15
	or  r1, r4, r1
	or  r1, r3, r1
	ori  r14, r1, 16
	jal r0, .LBB26_15
.LBB26_7:
	ldbu r13, r4+6
	ldhu r14, r4+4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, exp2RK
	ldw r3, r12+4
	slli r4, r13, 7
	slli r5, r14, 16
	or  r4, r4, r5
	slli r3, r3, 24
	slli r1, r1, 15
	or  r1, r4, r1
	or  r1, r3, r1
	ori  r14, r1, 18
	jal r0, .LBB26_15
.LBB26_8:
	add r13, r4, r0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaK_dischargevars
	ldw r1, r12+0
	addi r3, r0, 8
	bne r1, r3, .LBB26_11
.LBB26_9:
	ldw r1, r12+12
	ldw r3, r12+16
	addi r14, r12, 4
	beq r1, r3, .LBB26_12
.LBB26_10:
	ldw r15, r14+0
	add r3, r11, r0
	jal r31, luaY_nvarstack
	bge r15, r1, .LBB26_20
.LBB26_11:
	addi r14, r12, 4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaK_exp2nextreg
.LBB26_12:
	ldw r1, r14+0
	ldw r3, r13+4
	slli r1, r1, 7
	slli r3, r3, 16
	or  r1, r1, r3
	addi r14, r1, 10
	jal r0, .LBB26_15
.LBB26_13:
	ldbu r13, r4+6
	ldhu r14, r4+4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, exp2RK
	ldw r3, r12+4
	slli r4, r13, 7
	slli r5, r14, 16
	or  r4, r4, r5
	slli r3, r3, 24
	slli r1, r1, 15
	or  r1, r4, r1
	or  r1, r3, r1
	ori  r14, r1, 15
	jal r0, .LBB26_15
.LBB26_14:
	ldbu r13, r4+6
	ldhu r14, r4+4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, exp2RK
	ldw r3, r12+4
	slli r4, r13, 7
	slli r5, r14, 16
	or  r4, r4, r5
	slli r3, r3, 24
	slli r1, r1, 15
	or  r1, r4, r1
	or  r1, r3, r1
	ori  r14, r1, 17
.LBB26_15:
	ldw r13, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r13+52
	ldw r5, r11+16
	addi r6, r13, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r13+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r14
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r13, r0
	jal r31, savelineinfo
.LBB26_16:
	ldw r1, r12+0
	addi r3, r0, 8
	bne r1, r3, .LBB26_19
.LBB26_17:
	ldw r12, r12+4
	add r3, r11, r0
	jal r31, luaY_nvarstack
	blt r12, r1, .LBB26_19
.LBB26_18:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB26_19:
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
.LBB26_20:
	ldw r5, r12+4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, exp2reg
	jal r0, .LBB26_12
.Lfunc_end26:
	.size	luaK_storevar, .Lfunc_end26-luaK_storevar
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI26_0:
	.word	.LBB26_2
	.word	.LBB26_8
	.word	.LBB26_16
	.word	.LBB26_6
	.word	.LBB26_13
	.word	.LBB26_14
	.word	.LBB26_7
                                        # -- End function
	.text
	.hidden	luaK_self                       # -- Begin function luaK_self
	.globl	luaK_self
	.p2align	2
	.type	luaK_self,@function
luaK_self:                              # @luaK_self
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
	jal r31, luaK_dischargevars
	ldw r1, r13+0
	addi r14, r0, 8
	bne r1, r14, .LBB27_3
.LBB27_1:
	ldw r1, r13+12
	ldw r3, r13+16
	beq r1, r3, .LBB27_4
.LBB27_2:
	ldw r15, r13+4
	add r3, r11, r0
	jal r31, luaY_nvarstack
	bge r15, r1, .LBB27_14
.LBB27_3:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_exp2nextreg
.LBB27_4:
	ldw r15, r13+4
	ldw r1, r13+0
	bne r1, r14, .LBB27_7
.LBB27_5:
	add r3, r11, r0
	jal r31, luaY_nvarstack
	blt r15, r1, .LBB27_7
.LBB27_6:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB27_7:
	ldbu r1, r11+52
	stw r13+4, r1
	stw r13+0, r14
	ldbu r4, r11+52
	addi r1, r4, 2
	ldw r3, r11+0
	ldbu r5, r3+8
	bleu r1, r5, .LBB27_10
.LBB27_8:
	addi r5, r0, 253
	bgeu r4, r5, .LBB27_15
.LBB27_9:
	stb r3+8, r1
.LBB27_10:
	stb r11+52, r1
	ldw r13, r13+4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, exp2RK
	ldw r3, r12+4
	slli r4, r13, 7
	slli r5, r15, 16
	slli r3, r3, 24
	slli r1, r1, 15
	or  r4, r5, r4
	or  r1, r4, r1
	or  r1, r1, r3
	addi r15, r1, 20
	ldw r13, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r13+52
	ldw r5, r11+16
	addi r6, r13, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r13+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r15
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r13, r0
	jal r31, savelineinfo
	ldw r1, r12+0
	bne r1, r14, .LBB27_13
.LBB27_11:
	ldw r12, r12+4
	add r3, r11, r0
	jal r31, luaY_nvarstack
	blt r12, r1, .LBB27_13
.LBB27_12:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB27_13:
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
.LBB27_14:
	ldw r5, r13+4
	add r3, r11, r0
	add r4, r13, r0
	jal r31, exp2reg
	jal r0, .LBB27_4
.LBB27_15:
	ldw r3, r11+8
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, luaX_syntaxerror
.Lfunc_end27:
	.size	luaK_self, .Lfunc_end27-luaK_self
                                        # -- End function
	.hidden	luaK_goiftrue                   # -- Begin function luaK_goiftrue
	.globl	luaK_goiftrue
	.p2align	2
	.type	luaK_goiftrue,@function
luaK_goiftrue:                          # @luaK_goiftrue
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
	jal r31, luaK_dischargevars
	ldw r3, r11+0
	addi r13, r0, -1
	addi r4, r3, -4
	addi r5, r0, 4
	add r1, r13, r0
	bgeu r4, r5, .LBB28_7
.LBB28_1:
	beq r1, r13, .LBB28_14
.LBB28_2:
	ldw r9, r11+16
	addi r4, r0, -1
	beq r9, r4, .LBB28_13
.LBB28_3:
	ldw r3, r12+0
	ldw r6, r3+52
	lui r3, 1044480
	addi r7, r3, 1
	addi r8, r0, 0
.LBB28_4:
	add r10, r9, r0
	slli r3, r9, 2
	add r3, r6, r3
	ldw r5, r3+0
	srli r9, r5, 7
	add r9, r9, r7
	seq r13, r9, r4
	add r9, r10, r9
	addi r9, r9, 1
	sub r13, r8, r13
	or  r9, r13, r9
	bne r9, r4, .LBB28_4
.LBB28_5:
	xor r4, r10, r4
	add r1, r1, r4
	lui r4, 4096
	addi r4, r4, -1
	add r4, r1, r4
	lui r6, 8192
	bgeu r4, r6, .LBB28_16
.LBB28_6:
	andi r4, r5, 127
	slli r1, r1, 7
	or  r1, r1, r4
	lui r4, 524288
	addi r4, r4, -128
	add r1, r1, r4
	stw r3+0, r1
	jal r0, .LBB28_14
.LBB28_7:
	addi r4, r0, 2
	add r1, r13, r0
	beq r3, r4, .LBB28_1
.LBB28_8:
	addi r1, r0, 16
	bne r3, r1, .LBB28_15
.LBB28_9:
	ldw r3, r11+4
	ldw r1, r12+0
	ldw r1, r1+52
	slli r4, r3, 2
	add r1, r1, r4
	addi r4, r0, 1
	blt r3, r4, .LBB28_12
.LBB28_10:
	ldw r3, r1+-4
	andi r3, r3, 127
	lui r4, %hi(luaP_opmodes)
	addi r4, r4, %lo(luaP_opmodes)
	add r3, r3, r4
	ldbu r3, r3+0
	andi r3, r3, 16
	addi r4, r0, 0
	beq r3, r4, .LBB28_12
.LBB28_11:
	addi r1, r1, -4
.LBB28_12:
	ldw r3, r1+0
	lui r4, 8
	xor r3, r3, r4
	stw r1+0, r3
	ldw r1, r11+4
	jal r0, .LBB28_1
.LBB28_13:
	stw r11+16, r1
.LBB28_14:
	ldw r4, r11+12
	ldw r5, r12+16
	stw r12+20, r5
	addi r6, r0, 255
	add r3, r12, r0
	add r7, r5, r0
	jal r31, patchlistaux
	addi r1, r0, -1
	stw r11+12, r1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB28_15:
	addi r5, r0, 0
	add r3, r12, r0
	add r4, r11, r0
	jal r31, jumponcond
	jal r0, .LBB28_1
.LBB28_16:
	ldw r3, r12+8
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r31, luaX_syntaxerror
.Lfunc_end28:
	.size	luaK_goiftrue, .Lfunc_end28-luaK_goiftrue
                                        # -- End function
	.p2align	2                               # -- Begin function jumponcond
	.type	jumponcond,@function
jumponcond:                             # @jumponcond
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
	add r12, r5, r0
	add r13, r4, r0
	add r11, r3, r0
	ldw r1, r4+0
	addi r3, r0, 17
	bne r1, r3, .LBB29_7
.LBB29_1:
	ldw r14, r11+0
	ldw r4, r14+52
	ldw r1, r13+4
	slli r1, r1, 2
	add r1, r4, r1
	ldw r3, r1+0
	andi r18, r3, 127
	addi r19, r0, 51
                                        # implicit-def: $r1
	bne r18, r19, .LBB29_6
.LBB29_2:
	ldw r1, r11+16
	ldw r5, r14+64
	add r5, r5, r1
	ldb r5, r5+-1
	addi r6, r0, -128
	bne r5, r6, .LBB29_4
.LBB29_3:
	ldw r5, r11+36
	addi r5, r5, -1
	stw r11+36, r5
	addi r5, r0, 129
	jal r0, .LBB29_5
.LBB29_4:
	ldw r6, r11+24
	sub r5, r6, r5
	stw r11+24, r5
	ldbu r5, r11+53
	addi r5, r5, -1
.LBB29_5:
	stb r11+53, r5
	addi r5, r1, -1
	stw r11+16, r5
	srli r1, r3, 9
	lui r3, 8
	addi r6, r3, -128
	and r1, r1, r6
	slli r6, r12, 15
	or  r1, r1, r6
	addi r3, r3, 66
	xor r20, r1, r3
	ldw r1, r11+8
	ldw r3, r1+40
	addi r6, r14, 20
	lui r15, %hi(.L.str)
	addi r15, r15, %lo(.L.str)
	lui r1, 262144
	addi r16, r1, -1
	addi r17, r0, 4
	add r7, r17, r0
	add r8, r16, r0
	add r9, r15, r0
	jal r31, luaM_growaux_
	stw r14+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r20
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r14, r0
	jal r31, savelineinfo
	ldw r5, r11+16
	ldw r14, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r14+52
	addi r6, r14, 20
	add r7, r17, r0
	add r8, r16, r0
	add r9, r15, r0
	jal r31, luaM_growaux_
	stw r14+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	lui r3, 524288
	addi r3, r3, -200
	stw r1+0, r3
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r14, r0
	jal r31, savelineinfo
	ldw r1, r11+16
	addi r1, r1, -1
.LBB29_6:
	beq r18, r19, .LBB29_16
.LBB29_7:
	ldw r1, r13+0
	addi r14, r0, 8
	beq r1, r14, .LBB29_12
.LBB29_8:
	ldbu r4, r11+52
	addi r1, r4, 1
	ldw r3, r11+0
	ldbu r5, r3+8
	bltu r4, r5, .LBB29_11
.LBB29_9:
	addi r5, r0, 254
	bgeu r4, r5, .LBB29_17
.LBB29_10:
	stb r3+8, r1
.LBB29_11:
	stb r11+52, r1
	andi r1, r1, 255
	addi r5, r1, -1
	add r3, r11, r0
	add r4, r13, r0
	jal r31, discharge2reg
.LBB29_12:
	ldw r1, r13+0
	bne r1, r14, .LBB29_15
.LBB29_13:
	ldw r14, r13+4
	add r3, r11, r0
	jal r31, luaY_nvarstack
	blt r14, r1, .LBB29_15
.LBB29_14:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB29_15:
	ldw r1, r13+4
	slli r1, r1, 16
	slli r3, r12, 15
	or  r1, r1, r3
	lui r3, 8
	addi r3, r3, -61
	or  r16, r1, r3
	ldw r12, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r12+52
	ldw r5, r11+16
	addi r6, r12, 20
	lui r13, %hi(.L.str)
	addi r13, r13, %lo(.L.str)
	lui r1, 262144
	addi r14, r1, -1
	addi r15, r0, 4
	add r7, r15, r0
	add r8, r14, r0
	add r9, r13, r0
	jal r31, luaM_growaux_
	stw r12+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r16
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r12, r0
	jal r31, savelineinfo
	ldw r5, r11+16
	ldw r12, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r12+52
	addi r6, r12, 20
	add r7, r15, r0
	add r8, r14, r0
	add r9, r13, r0
	jal r31, luaM_growaux_
	stw r12+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	lui r3, 524288
	addi r3, r3, -200
	stw r1+0, r3
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r12, r0
	jal r31, savelineinfo
	ldw r1, r11+16
	addi r1, r1, -1
.LBB29_16:
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
.LBB29_17:
	ldw r3, r11+8
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, luaX_syntaxerror
.Lfunc_end29:
	.size	jumponcond, .Lfunc_end29-jumponcond
                                        # -- End function
	.hidden	luaK_goiffalse                  # -- Begin function luaK_goiffalse
	.globl	luaK_goiffalse
	.p2align	2
	.type	luaK_goiffalse,@function
luaK_goiffalse:                         # @luaK_goiffalse
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
	jal r31, luaK_dischargevars
	ldw r3, r11+0
	addi r13, r0, -1
	addi r4, r0, 1
	add r1, r13, r0
	beq r3, r4, .LBB30_4
.LBB30_1:
	addi r4, r0, 3
	add r1, r13, r0
	beq r3, r4, .LBB30_4
.LBB30_2:
	addi r1, r0, 16
	bne r3, r1, .LBB30_10
.LBB30_3:
	ldw r1, r11+4
.LBB30_4:
	beq r1, r13, .LBB30_12
.LBB30_5:
	ldw r9, r11+12
	addi r4, r0, -1
	beq r9, r4, .LBB30_11
.LBB30_6:
	ldw r3, r12+0
	ldw r6, r3+52
	lui r3, 1044480
	addi r7, r3, 1
	addi r8, r0, 0
.LBB30_7:
	add r10, r9, r0
	slli r3, r9, 2
	add r3, r6, r3
	ldw r5, r3+0
	srli r9, r5, 7
	add r9, r9, r7
	seq r13, r9, r4
	add r9, r10, r9
	addi r9, r9, 1
	sub r13, r8, r13
	or  r9, r13, r9
	bne r9, r4, .LBB30_7
.LBB30_8:
	xor r4, r10, r4
	add r1, r1, r4
	lui r4, 4096
	addi r4, r4, -1
	add r4, r1, r4
	lui r6, 8192
	bgeu r4, r6, .LBB30_13
.LBB30_9:
	andi r4, r5, 127
	slli r1, r1, 7
	or  r1, r1, r4
	lui r4, 524288
	addi r4, r4, -128
	add r1, r1, r4
	stw r3+0, r1
	jal r0, .LBB30_12
.LBB30_10:
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, jumponcond
	bne r1, r13, .LBB30_5
	jal r0, .LBB30_12
.LBB30_11:
	stw r11+12, r1
.LBB30_12:
	ldw r4, r11+16
	ldw r5, r12+16
	stw r12+20, r5
	addi r6, r0, 255
	add r3, r12, r0
	add r7, r5, r0
	jal r31, patchlistaux
	addi r1, r0, -1
	stw r11+16, r1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB30_13:
	ldw r3, r12+8
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r31, luaX_syntaxerror
.Lfunc_end30:
	.size	luaK_goiffalse, .Lfunc_end30-luaK_goiffalse
                                        # -- End function
	.hidden	luaK_indexed                    # -- Begin function luaK_indexed
	.globl	luaK_indexed
	.p2align	2
	.type	luaK_indexed,@function
luaK_indexed:                           # @luaK_indexed
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
	add r12, r5, r0
	add r11, r4, r0
	add r13, r3, r0
	ldw r1, r5+0
	addi r3, r0, 7
	bne r1, r3, .LBB31_2
.LBB31_1:
	ldw r1, r12+4
	addi r4, fp, -36
	stw r4+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r4+8, r1
	add r3, r13, r0
	add r5, r4, r0
	jal r31, addk
	stw r12+4, r1
	addi r1, r0, 4
	stw r12+0, r1
.LBB31_2:
	ldw r1, r11+0
	addi r14, r0, 10
	bne r1, r14, .LBB31_11
.LBB31_3:
	ldw r1, r12+0
	addi r3, r0, 4
	bne r1, r3, .LBB31_7
.LBB31_4:
	ldw r1, r12+12
	ldw r3, r12+16
	bne r1, r3, .LBB31_7
.LBB31_5:
	ldw r1, r12+4
	addi r3, r0, 255
	bgt r1, r3, .LBB31_7
.LBB31_6:
	ldw r3, r13+0
	ldw r3, r3+48
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	ldbu r1, r1+8
	addi r3, r0, 68
	beq r1, r3, .LBB31_11
.LBB31_7:
	add r3, r13, r0
	add r4, r11, r0
	jal r31, luaK_dischargevars
	ldw r1, r11+0
	addi r3, r0, 8
	bne r1, r3, .LBB31_10
.LBB31_8:
	ldw r1, r11+12
	ldw r3, r11+16
	beq r1, r3, .LBB31_11
.LBB31_9:
	ldw r15, r11+4
	add r3, r13, r0
	jal r31, luaY_nvarstack
	bge r15, r1, .LBB31_31
.LBB31_10:
	add r3, r13, r0
	add r4, r11, r0
	jal r31, luaK_exp2nextreg
.LBB31_11:
	ldw r1, r11+0
	addi r3, r0, 9
	beq r1, r3, .LBB31_14
.LBB31_12:
	bne r1, r14, .LBB31_15
.LBB31_13:
	ldw r1, r11+4
	stb r11+6, r1
	ldw r1, r12+4
	sth r11+4, r1
	addi r1, r0, 13
	jal r0, .LBB31_29
.LBB31_14:
	ldbu r1, r11+4
	jal r0, .LBB31_16
.LBB31_15:
	ldw r1, r11+4
.LBB31_16:
	stb r11+6, r1
	ldw r1, r12+0
	addi r3, r0, 6
	beq r1, r3, .LBB31_22
.LBB31_17:
	addi r3, r0, 4
	bne r1, r3, .LBB31_24
.LBB31_18:
	ldw r1, r12+12
	ldw r3, r12+16
	bne r1, r3, .LBB31_24
.LBB31_19:
	ldw r1, r12+4
	addi r3, r0, 255
	bgt r1, r3, .LBB31_24
.LBB31_20:
	ldw r3, r13+0
	ldw r3, r3+48
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	ldbu r1, r1+8
	addi r3, r0, 68
	bne r1, r3, .LBB31_24
.LBB31_21:
	ldw r1, r12+4
	sth r11+4, r1
	addi r1, r0, 15
	jal r0, .LBB31_29
.LBB31_22:
	ldw r1, r12+12
	ldw r3, r12+16
	bne r1, r3, .LBB31_24
.LBB31_23:
	ldw r1, r12+4
	addi r3, r0, 255
	bleu r1, r3, .LBB31_32
.LBB31_24:
	add r3, r13, r0
	add r4, r12, r0
	jal r31, luaK_dischargevars
	ldw r1, r12+0
	addi r3, r0, 8
	bne r1, r3, .LBB31_27
.LBB31_25:
	ldw r1, r12+12
	ldw r3, r12+16
	addi r14, r12, 4
	beq r1, r3, .LBB31_28
.LBB31_26:
	ldw r15, r14+0
	add r3, r13, r0
	jal r31, luaY_nvarstack
	bge r15, r1, .LBB31_30
.LBB31_27:
	addi r14, r12, 4
	add r3, r13, r0
	add r4, r12, r0
	jal r31, luaK_exp2nextreg
.LBB31_28:
	ldw r1, r14+0
	sth r11+4, r1
	addi r1, r0, 12
.LBB31_29:
	stw r11+0, r1
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
.LBB31_30:
	ldw r5, r12+4
	add r3, r13, r0
	add r4, r12, r0
	jal r31, exp2reg
	jal r0, .LBB31_28
.LBB31_31:
	ldw r5, r11+4
	add r3, r13, r0
	add r4, r11, r0
	jal r31, exp2reg
	jal r0, .LBB31_11
.LBB31_32:
	ldw r1, r12+4
	sth r11+4, r1
	addi r1, r0, 14
	jal r0, .LBB31_29
.Lfunc_end31:
	.size	luaK_indexed, .Lfunc_end31-luaK_indexed
                                        # -- End function
	.hidden	luaK_prefix                     # -- Begin function luaK_prefix
	.globl	luaK_prefix
	.p2align	2
	.type	luaK_prefix,@function
luaK_prefix:                            # @luaK_prefix
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
	add r13, r6, r0
	add r12, r5, r0
	add r14, r4, r0
	add r11, r3, r0
	add r4, r5, r0
	jal r31, luaK_dischargevars
	addi r1, r0, 2
	bltu r14, r1, .LBB32_3
.LBB32_1:
	beq r14, r1, .LBB32_13
.LBB32_2:
	addi r1, r0, 3
	beq r14, r1, .LBB32_4
	jal r0, .LBB32_18
.LBB32_3:
	addi r4, r14, 12
	lui r6, %hi(luaK_prefix.ef)
	addi r6, r6, %lo(luaK_prefix.ef)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, constfolding
	addi r3, r0, 0
	bne r1, r3, .LBB32_18
.LBB32_4:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaK_dischargevars
	ldw r1, r12+0
	addi r15, r0, 8
	bne r1, r15, .LBB32_7
.LBB32_5:
	ldw r1, r12+12
	ldw r3, r12+16
	beq r1, r3, .LBB32_8
.LBB32_6:
	ldw r16, r12+4
	add r3, r11, r0
	jal r31, luaY_nvarstack
	bge r16, r1, .LBB32_19
.LBB32_7:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaK_exp2nextreg
.LBB32_8:
	addi r14, r14, 49
	ldw r16, r12+4
	ldw r1, r12+0
	bne r1, r15, .LBB32_11
.LBB32_9:
	ldw r15, r12+4
	add r3, r11, r0
	jal r31, luaY_nvarstack
	blt r15, r1, .LBB32_11
.LBB32_10:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB32_11:
	slli r1, r16, 16
	or  r15, r1, r14
	ldw r14, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r14+52
	ldw r5, r11+16
	addi r6, r14, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r14+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r15
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r14, r0
	jal r31, savelineinfo
	ldw r1, r11+16
	addi r1, r1, -1
	stw r12+4, r1
	addi r1, r0, 17
	stw r12+0, r1
	ldw r4, r11+0
	ldw r1, r11+16
	ldw r3, r4+64
	add r1, r3, r1
	ldb r1, r1+-1
	addi r3, r0, -128
	bne r1, r3, .LBB32_16
.LBB32_12:
	ldw r1, r11+36
	addi r1, r1, -1
	stw r11+36, r1
	addi r1, r0, 129
	jal r0, .LBB32_17
.LBB32_13:
	ldw r3, r12+0
	addi r3, r3, -1
	addi r4, r0, 16
	bgtu r3, r4, .LBB32_29
.LBB32_14:
	slli r3, r3, 2
	lui r4, %hi(.LJTI32_0)
	addi r4, r4, %lo(.LJTI32_0)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB32_15:
	addi r1, r0, 3
	jal r0, .LBB32_28
.LBB32_16:
	ldw r3, r11+24
	sub r1, r3, r1
	stw r11+24, r1
	ldbu r1, r11+53
	addi r1, r1, -1
.LBB32_17:
	stb r11+53, r1
	add r3, r11, r0
	add r5, r13, r0
	jal r31, savelineinfo
.LBB32_18:
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
.LBB32_19:
	ldw r5, r12+4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, exp2reg
	jal r0, .LBB32_8
.LBB32_20:
	ldbu r4, r11+52
	addi r1, r4, 1
	ldw r3, r11+0
	ldbu r5, r3+8
	bltu r4, r5, .LBB32_23
.LBB32_21:
	addi r5, r0, 254
	bgeu r4, r5, .LBB32_51
.LBB32_22:
	stb r3+8, r1
.LBB32_23:
	stb r11+52, r1
	andi r1, r1, 255
	addi r5, r1, -1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, discharge2reg
.LBB32_24:
	ldw r1, r12+0
	addi r3, r0, 8
	bne r1, r3, .LBB32_27
.LBB32_25:
	ldw r13, r12+4
	add r3, r11, r0
	jal r31, luaY_nvarstack
	blt r13, r1, .LBB32_27
.LBB32_26:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB32_27:
	ldw r1, r12+4
	slli r1, r1, 16
	addi r14, r1, 51
	ldw r13, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r13+52
	ldw r5, r11+16
	addi r6, r13, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r13+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r14
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r13, r0
	jal r31, savelineinfo
	ldw r1, r11+16
	addi r1, r1, -1
	stw r12+4, r1
	addi r1, r0, 17
.LBB32_28:
	stw r12+0, r1
.LBB32_29:
	ldw r1, r12+16
	ldw r4, r12+12
	stw r12+16, r4
	stw r12+12, r1
	addi r1, r0, -1
	lui r3, 1044480
	beq r4, r1, .LBB32_38
.LBB32_30:
	ldw r5, r11+0
	ldw r5, r5+52
	addi r6, r0, 1
	lui r7, %hi(luaP_opmodes)
	addi r7, r7, %lo(luaP_opmodes)
	addi r8, r0, 0
	addi r9, r0, 67
	lui r13, 8
	addi r10, r13, -128
	addi r13, r13, 66
	addi r14, r3, 1
	jal r0, .LBB32_32
.LBB32_31:
	ldw r15, r15+0
	srli r15, r15, 7
	add r15, r15, r14
	seq r16, r15, r1
	add r4, r4, r15
	addi r4, r4, 1
	sub r15, r8, r16
	or  r4, r15, r4
	beq r4, r1, .LBB32_38
.LBB32_32:
	slli r15, r4, 2
	add r15, r5, r15
	blt r4, r6, .LBB32_34
.LBB32_33:
	ldw r16, r15+-4
	andi r16, r16, 127
	add r16, r16, r7
	ldbu r16, r16+0
	andi r16, r16, 16
	bne r16, r8, .LBB32_35
.LBB32_34:
	add r16, r15, r0
	jal r0, .LBB32_36
.LBB32_35:
	addi r16, r15, -4
.LBB32_36:
	ldw r17, r16+0
	andi r18, r17, 127
	bne r18, r9, .LBB32_31
.LBB32_37:
	srli r18, r17, 9
	and r18, r18, r10
	and r17, r17, r13
	or  r17, r18, r17
	stw r16+0, r17
	jal r0, .LBB32_31
.LBB32_38:
	ldw r4, r12+12
	beq r4, r1, .LBB32_18
.LBB32_39:
	ldw r5, r11+0
	ldw r5, r5+52
	addi r6, r0, 1
	lui r7, %hi(luaP_opmodes)
	addi r7, r7, %lo(luaP_opmodes)
	addi r8, r0, 0
	addi r9, r0, 67
	lui r11, 8
	addi r10, r11, -128
	addi r11, r11, 66
	addi r3, r3, 1
	jal r0, .LBB32_41
.LBB32_40:
	ldw r12, r12+0
	srli r12, r12, 7
	add r12, r12, r3
	seq r13, r12, r1
	add r4, r4, r12
	addi r4, r4, 1
	sub r12, r8, r13
	or  r4, r12, r4
	beq r4, r1, .LBB32_18
.LBB32_41:
	slli r12, r4, 2
	add r12, r5, r12
	blt r4, r6, .LBB32_43
.LBB32_42:
	ldw r13, r12+-4
	andi r13, r13, 127
	add r13, r13, r7
	ldbu r13, r13+0
	andi r13, r13, 16
	bne r13, r8, .LBB32_44
.LBB32_43:
	add r13, r12, r0
	jal r0, .LBB32_45
.LBB32_44:
	addi r13, r12, -4
.LBB32_45:
	ldw r14, r13+0
	andi r15, r14, 127
	bne r15, r9, .LBB32_40
.LBB32_46:
	srli r15, r14, 9
	and r15, r15, r10
	and r14, r14, r11
	or  r14, r15, r14
	stw r13+0, r14
	jal r0, .LBB32_40
.LBB32_47:
	ldw r3, r12+4
	ldw r1, r11+0
	ldw r1, r1+52
	slli r4, r3, 2
	add r1, r1, r4
	addi r4, r0, 1
	blt r3, r4, .LBB32_50
.LBB32_48:
	ldw r3, r1+-4
	andi r3, r3, 127
	lui r4, %hi(luaP_opmodes)
	addi r4, r4, %lo(luaP_opmodes)
	add r3, r3, r4
	ldbu r3, r3+0
	andi r3, r3, 16
	addi r4, r0, 0
	beq r3, r4, .LBB32_50
.LBB32_49:
	addi r1, r1, -4
.LBB32_50:
	ldw r3, r1+0
	lui r4, 8
	xor r3, r3, r4
	stw r1+0, r3
	jal r0, .LBB32_29
.LBB32_51:
	ldw r3, r11+8
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, luaX_syntaxerror
.Lfunc_end32:
	.size	luaK_prefix, .Lfunc_end32-luaK_prefix
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI32_0:
	.word	.LBB32_28
	.word	.LBB32_15
	.word	.LBB32_28
	.word	.LBB32_15
	.word	.LBB32_15
	.word	.LBB32_15
	.word	.LBB32_15
	.word	.LBB32_24
	.word	.LBB32_29
	.word	.LBB32_29
	.word	.LBB32_29
	.word	.LBB32_29
	.word	.LBB32_29
	.word	.LBB32_29
	.word	.LBB32_29
	.word	.LBB32_47
	.word	.LBB32_20
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function constfolding
.LCPI33_0:
	.quad	0x0000000000000000              # double 0
	.text
	.p2align	2
	.type	constfolding,@function
constfolding:                           # @constfolding
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
	stw fp+-32, lr
	ldw r7, r5+12
	ldw r8, r5+16
	addi r1, r0, 0
	bne r7, r8, .LBB33_22
.LBB33_1:
	ldw r7, r5+0
	addi r12, r0, 5
	beq r7, r12, .LBB33_4
.LBB33_2:
	addi r8, r0, 6
	bne r7, r8, .LBB33_22
.LBB33_3:
	ldw r7, r5+4
	addi r8, fp, -44
	stw r8+0, r7
	addi r7, r0, 3
	jal r0, .LBB33_5
.LBB33_4:
	ldw r7, r5+8
	ldw r8, r5+4
	addi r9, fp, -44
	stw r9+4, r7
	stw r9+0, r8
	addi r7, r0, 19
.LBB33_5:
	addi r8, fp, -44
	stb r8+8, r7
	ldw r7, r6+12
	ldw r8, r6+16
	bne r7, r8, .LBB33_22
.LBB33_6:
	ldw r7, r6+0
	beq r7, r12, .LBB33_9
.LBB33_7:
	addi r8, r0, 6
	bne r7, r8, .LBB33_22
.LBB33_8:
	ldw r1, r6+4
	addi r6, fp, -56
	stw r6+0, r1
	addi r1, r0, 3
	jal r0, .LBB33_10
.LBB33_9:
	ldw r1, r6+8
	ldw r6, r6+4
	addi r7, fp, -56
	stw r7+4, r1
	stw r7+0, r6
	addi r1, r0, 19
.LBB33_10:
	addi r6, fp, -56
	stb r6+8, r1
	addi r1, r0, 13
	bgtu r4, r1, .LBB33_17
.LBB33_11:
	addi r13, r0, 1
	sll r7, r13, r4
	lui r1, 3
	addi r1, r1, -128
	and r8, r7, r1
	addi r1, r0, 0
	beq r8, r1, .LBB33_15
.LBB33_12:
	add r14, r1, r0
	add r15, r3, r0
	add r16, r4, r0
	add r17, r5, r0
	addi r3, fp, -44
	addi r4, fp, -68
	addi r11, r0, 0
	add r5, r11, r0
	jal r31, luaV_tointegerns
	beq r1, r11, .LBB33_14
.LBB33_13:
	addi r3, fp, -56
	addi r4, fp, -68
	add r5, r11, r0
	jal r31, luaV_tointegerns
	seq r13, r1, r11
.LBB33_14:
	add r5, r17, r0
	add r4, r16, r0
	add r3, r15, r0
	add r1, r14, r0
	beq r13, r1, .LBB33_17
	jal r0, .LBB33_22
.LBB33_15:
	andi r7, r7, 104
	beq r7, r1, .LBB33_17
.LBB33_16:
	ldbu r7, r6+8
	addi r8, r0, 3
	seq r7, r7, r8
	ldw r8, r6+0
	fcvt.d.w r10, r8
	ldw r6, r6+4
	sub r7, r1, r7
	xor r9, r11, r6
	and r9, r9, r7
	xor r15, r6, r9
	xor r6, r10, r8
	and r6, r6, r7
	xor r14, r8, r6
	lui r6, %hi(.LCPI33_0)
	addi r6, r6, %lo(.LCPI33_0)
	ldw r9, r6+4
	ldw r8, r6+0
	feq.d r13, r14, r8
	bne r13, r1, .LBB33_22
.LBB33_17:
	add r13, r5, r0
	ldw r1, r3+8
	ldw r3, r1+40
	addi r5, fp, -44
	addi r6, fp, -56
	addi r11, fp, -68
	add r7, r11, r0
	jal r31, luaO_rawarith
	ldbu r1, r11+8
	addi r3, r0, 3
	bne r1, r3, .LBB33_19
.LBB33_18:
	addi r1, r0, 6
	stw r13+0, r1
	ldw r1, r11+0
	stw r13+4, r1
	jal r0, .LBB33_21
.LBB33_19:
	ldw r5, r11+4
	ldw r4, r11+0
	lui r1, %hi(.LCPI33_0)
	addi r1, r1, %lo(.LCPI33_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r3, r4, r6
	addi r1, r0, 0
	bne r3, r1, .LBB33_22
.LBB33_20:
	stw r13+0, r12
	stw r13+8, r5
	stw r13+4, r4
.LBB33_21:
	addi r1, r0, 1
.LBB33_22:
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
	addi sp, sp, 88
	jalr r0, r31, 0
.Lfunc_end33:
	.size	constfolding, .Lfunc_end33-constfolding
                                        # -- End function
	.hidden	luaK_infix                      # -- Begin function luaK_infix
	.globl	luaK_infix
	.p2align	2
	.type	luaK_infix,@function
luaK_infix:                             # @luaK_infix
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
	add r13, r4, r0
	add r12, r3, r0
	add r4, r5, r0
	jal r31, luaK_dischargevars
	addi r1, r0, 20
	bgtu r13, r1, .LBB34_17
.LBB34_1:
	slli r1, r13, 2
	lui r3, %hi(.LJTI34_0)
	addi r3, r3, %lo(.LJTI34_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB34_2:
	ldw r1, r11+12
	ldw r3, r11+16
	bne r1, r3, .LBB34_13
.LBB34_3:
	ldw r1, r11+0
	addi r1, r1, -7
	addi r3, r0, -3
	bleu r1, r3, .LBB34_13
	jal r0, .LBB34_17
.LBB34_4:
	ldw r1, r11+0
	addi r3, r0, 5
	beq r1, r3, .LBB34_10
.LBB34_5:
	addi r3, r0, 6
	bne r1, r3, .LBB34_13
.LBB34_6:
	ldw r1, r11+4
	addi r3, fp, -20
	stw r3+0, r1
	jal r0, .LBB34_11
.LBB34_7:
	ldw r1, r11+12
	ldw r3, r11+16
	bne r1, r3, .LBB34_9
.LBB34_8:
	ldw r1, r11+0
	addi r1, r1, -7
	addi r3, r0, -3
	bgtu r1, r3, .LBB34_17
.LBB34_9:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, exp2RK
	jal r0, .LBB34_17
.LBB34_10:
	ldw r4, r11+8
	ldw r3, r11+4
	addi r5, fp, -20
	addi r13, r0, 0
	add r6, r13, r0
	jal r31, luaV_flttointeger
	beq r1, r13, .LBB34_13
.LBB34_11:
	ldw r1, r11+12
	ldw r3, r11+16
	bne r1, r3, .LBB34_13
.LBB34_12:
	addi r1, fp, -20
	ldw r1, r1+0
	addi r1, r1, -129
	addi r3, r0, -256
	bgeu r1, r3, .LBB34_17
.LBB34_13:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaK_dischargevars
	ldw r1, r11+0
	addi r3, r0, 8
	bne r1, r3, .LBB34_16
.LBB34_14:
	ldw r1, r11+12
	ldw r3, r11+16
	beq r1, r3, .LBB34_17
.LBB34_15:
	ldw r13, r11+4
	add r3, r12, r0
	jal r31, luaY_nvarstack
	bge r13, r1, .LBB34_18
.LBB34_16:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaK_exp2nextreg
.LBB34_17:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB34_18:
	ldw r5, r11+4
	add r3, r12, r0
	add r4, r11, r0
	jal r31, exp2reg
	jal r0, .LBB34_17
.LBB34_19:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaK_goiffalse
	jal r0, .LBB34_17
.LBB34_20:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaK_goiftrue
	jal r0, .LBB34_17
.Lfunc_end34:
	.size	luaK_infix, .Lfunc_end34-luaK_infix
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI34_0:
	.word	.LBB34_2
	.word	.LBB34_2
	.word	.LBB34_2
	.word	.LBB34_2
	.word	.LBB34_2
	.word	.LBB34_2
	.word	.LBB34_2
	.word	.LBB34_2
	.word	.LBB34_2
	.word	.LBB34_2
	.word	.LBB34_2
	.word	.LBB34_2
	.word	.LBB34_16
	.word	.LBB34_7
	.word	.LBB34_4
	.word	.LBB34_4
	.word	.LBB34_7
	.word	.LBB34_4
	.word	.LBB34_4
	.word	.LBB34_20
	.word	.LBB34_19
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function exp2RK
	.type	exp2RK,@function
exp2RK:                                 # @exp2RK
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
	add r13, r3, r0
	jal r31, luaK_exp2K
	addi r11, r0, 0
	beq r1, r11, .LBB35_2
.LBB35_1:
	addi r11, r0, 1
	jal r0, .LBB35_6
.LBB35_2:
	add r3, r13, r0
	add r4, r12, r0
	jal r31, luaK_dischargevars
	ldw r1, r12+0
	addi r3, r0, 8
	bne r1, r3, .LBB35_5
.LBB35_3:
	ldw r1, r12+12
	ldw r3, r12+16
	beq r1, r3, .LBB35_6
.LBB35_4:
	ldw r14, r12+4
	add r3, r13, r0
	jal r31, luaY_nvarstack
	bge r14, r1, .LBB35_7
.LBB35_5:
	add r3, r13, r0
	add r4, r12, r0
	jal r31, luaK_exp2nextreg
.LBB35_6:
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
.LBB35_7:
	ldw r5, r12+4
	add r3, r13, r0
	add r4, r12, r0
	jal r31, exp2reg
	jal r0, .LBB35_6
.Lfunc_end35:
	.size	exp2RK, .Lfunc_end35-exp2RK
                                        # -- End function
	.hidden	luaK_posfix                     # -- Begin function luaK_posfix
	.globl	luaK_posfix
	.p2align	2
	.type	luaK_posfix,@function
luaK_posfix:                            # @luaK_posfix
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
	stw fp+-44, lr
	add r15, r7, r0
	add r13, r6, r0
	add r12, r5, r0
	add r14, r4, r0
	add r11, r3, r0
	add r4, r6, r0
	jal r31, luaK_dischargevars
	addi r1, r0, 11
	bgtu r14, r1, .LBB36_2
.LBB36_1:
	add r3, r11, r0
	add r4, r14, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, constfolding
	addi r3, r0, 0
	bne r1, r3, .LBB36_152
.LBB36_2:
	addi r1, r0, 20
	bgtu r14, r1, .LBB36_152
.LBB36_3:
	slli r1, r14, 2
	lui r3, %hi(.LJTI36_0)
	addi r3, r3, %lo(.LJTI36_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB36_4:
	ldw r1, r13+12
	ldw r3, r13+16
	bne r1, r3, .LBB36_29
.LBB36_5:
	ldw r1, r13+0
	addi r1, r1, -7
	addi r3, r0, -2
	bltu r1, r3, .LBB36_29
.LBB36_6:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_exp2K
	addi r3, r0, 0
	beq r1, r3, .LBB36_29
.LBB36_7:
	addi r1, r14, 6
	ldw r7, r13+4
	addi r6, r14, 22
	addi sp, sp, -4
	stw sp+0, r1
	addi r8, r0, 0
	addi r10, r0, 48
	jal r0, .LBB36_150
.LBB36_8:
	ldw r17, r12+0
	addi r18, r0, 6
	bne r17, r18, .LBB36_25
.LBB36_9:
	addi r16, fp, -64
	addi r5, r0, 20
	add r3, r16, r0
	add r4, r12, r0
	jal r31, memcpy
	ldw r1, r13+16
	stw r12+16, r1
	ldw r1, r13+12
	stw r12+12, r1
	ldw r1, r13+8
	stw r12+8, r1
	ldw r1, r13+4
	stw r12+4, r1
	ldw r1, r13+0
	stw r12+0, r1
	ldw r1, r16+16
	stw r13+16, r1
	ldw r1, r16+12
	stw r13+12, r1
	ldw r1, r16+8
	stw r13+8, r1
	ldw r1, r16+4
	stw r13+4, r1
	ldw r1, r16+0
	stw r13+0, r1
	addi r16, r0, 1
	ldw r1, r13+0
	beq r1, r18, .LBB36_26
	jal r0, .LBB36_27
.LBB36_10:
	ldw r1, r12+12
	ldw r3, r12+16
	bne r1, r3, .LBB36_46
.LBB36_11:
	ldw r1, r12+0
	addi r1, r1, -7
	addi r3, r0, -2
	sltu r17, r1, r3
	jal r0, .LBB36_47
.LBB36_12:
	ldw r1, r12+0
	addi r16, r0, 8
	beq r1, r16, .LBB36_14
.LBB36_13:
	addi r15, fp, -64
	addi r5, r0, 20
	add r3, r15, r0
	add r4, r12, r0
	jal r31, memcpy
	ldw r1, r13+16
	stw r12+16, r1
	ldw r1, r13+12
	stw r12+12, r1
	ldw r1, r13+8
	stw r12+8, r1
	ldw r1, r13+4
	stw r12+4, r1
	ldw r1, r13+0
	stw r12+0, r1
	ldw r1, r15+16
	stw r13+16, r1
	ldw r1, r15+12
	stw r13+12, r1
	ldw r1, r15+8
	stw r13+8, r1
	ldw r1, r15+4
	stw r13+4, r1
	ldw r1, r15+0
	stw r13+0, r1
.LBB36_14:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaK_dischargevars
	ldw r1, r12+0
	bne r1, r16, .LBB36_17
.LBB36_15:
	ldw r1, r12+12
	ldw r3, r12+16
	addi r15, r12, 4
	beq r1, r3, .LBB36_18
.LBB36_16:
	ldw r17, r15+0
	add r3, r11, r0
	jal r31, luaY_nvarstack
	bge r17, r1, .LBB36_138
.LBB36_17:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaK_exp2nextreg
	addi r15, r12, 4
.LBB36_18:
	ldw r17, r15+0
	ldw r1, r13+0
	addi r20, r0, 1
	addi r18, r0, 0
	addi r3, r0, 5
	beq r1, r3, .LBB36_79
.LBB36_19:
	addi r3, r0, 6
	bne r1, r3, .LBB36_87
.LBB36_20:
	ldw r1, r13+4
	addi r3, fp, -64
	stw r3+0, r1
	addi r15, r0, 0
	jal r0, .LBB36_81
.LBB36_21:
	addi r15, fp, -64
	addi r5, r0, 20
	add r3, r15, r0
	add r4, r12, r0
	jal r31, memcpy
	ldw r1, r13+16
	stw r12+16, r1
	ldw r1, r13+12
	stw r12+12, r1
	ldw r1, r13+8
	stw r12+8, r1
	ldw r1, r13+4
	stw r12+4, r1
	ldw r1, r13+0
	stw r12+0, r1
	ldw r1, r15+16
	stw r13+16, r1
	ldw r1, r15+12
	stw r13+12, r1
	ldw r1, r15+8
	stw r13+8, r1
	ldw r1, r15+4
	stw r13+4, r1
	ldw r1, r15+0
	stw r13+0, r1
	addi r14, r14, -3
.LBB36_22:
	ldw r1, r13+0
	addi r19, r0, 1
	addi r18, r0, 0
	addi r16, r0, 5
	beq r1, r16, .LBB36_31
.LBB36_23:
	addi r3, r0, 6
	bne r1, r3, .LBB36_35
.LBB36_24:
	ldw r1, r13+4
	addi r3, fp, -64
	stw r3+0, r1
	addi r15, r0, 0
	jal r0, .LBB36_33
.LBB36_25:
	addi r16, r0, 0
	ldw r1, r13+0
	bne r1, r18, .LBB36_27
.LBB36_26:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_exp2K
	addi r3, r0, 0
	bne r1, r3, .LBB36_56
.LBB36_27:
	bne r17, r18, .LBB36_29
.LBB36_28:
	addi r16, fp, -64
	addi r5, r0, 20
	add r3, r16, r0
	add r4, r12, r0
	jal r31, memcpy
	ldw r1, r13+16
	stw r12+16, r1
	ldw r1, r13+12
	stw r12+12, r1
	ldw r1, r13+8
	stw r12+8, r1
	ldw r1, r13+4
	stw r12+4, r1
	ldw r1, r13+0
	stw r12+0, r1
	ldw r1, r16+16
	stw r13+16, r1
	ldw r1, r16+12
	stw r13+12, r1
	ldw r1, r16+8
	stw r13+8, r1
	ldw r1, r16+4
	stw r13+4, r1
	ldw r1, r16+0
	stw r13+0, r1
.LBB36_29:
	add r3, r11, r0
	add r4, r14, r0
.LBB36_30:
	add r5, r12, r0
	add r6, r13, r0
	add r7, r15, r0
	jal r31, codebinexpval
	jal r0, .LBB36_152
.LBB36_31:
	ldw r4, r13+8
	ldw r3, r13+4
	addi r5, fp, -64
	addi r15, r0, 0
	add r6, r15, r0
	jal r31, luaV_flttointeger
	addi r3, r0, 1
	beq r1, r15, .LBB36_117
.LBB36_32:
	add r15, r3, r0
.LBB36_33:
	ldw r1, r13+12
	ldw r3, r13+16
	bne r1, r3, .LBB36_36
.LBB36_34:
	addi r1, fp, -64
	ldw r1, r1+0
	addi r17, r1, 127
	addi r1, r0, 255
	sgtu r3, r17, r1
	bne r3, r18, .LBB36_37
	jal r0, .LBB36_118
.LBB36_35:
                                        # implicit-def: $r17
	add r15, r18, r0
	add r3, r19, r0
	bne r3, r18, .LBB36_37
	jal r0, .LBB36_118
.LBB36_36:
                                        # implicit-def: $r17
	add r3, r19, r0
	beq r3, r18, .LBB36_118
.LBB36_37:
	ldw r1, r12+0
	addi r18, r0, 1
	beq r1, r16, .LBB36_83
.LBB36_38:
	addi r3, r0, 6
	bne r1, r3, .LBB36_88
.LBB36_39:
	ldw r1, r12+4
	addi r3, fp, -64
	stw r3+0, r1
	jal r0, .LBB36_85
.LBB36_40:
	ldw r1, r13+0
	addi r3, r0, 6
	bne r1, r3, .LBB36_43
.LBB36_41:
	ldw r1, r13+12
	ldw r3, r13+16
	bne r1, r3, .LBB36_43
.LBB36_42:
	ldw r1, r13+4
	addi r1, r1, -129
	addi r3, r0, -256
	bgeu r1, r3, .LBB36_144
.LBB36_43:
	addi r4, r0, 11
	jal r0, .LBB36_68
.LBB36_44:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_exp2nextreg
	ldw r5, r11+16
	ldw r1, r11+20
	ble r5, r1, .LBB36_106
.LBB36_45:
	ldw r1, r11+0
	ldw r1, r1+52
	slli r3, r5, 2
	add r1, r1, r3
	addi r14, r1, -4
	jal r0, .LBB36_107
.LBB36_46:
	addi r17, r0, 1
.LBB36_47:
	addi r18, r0, 0
	add r16, r18, r0
	bne r17, r18, .LBB36_49
.LBB36_48:
	addi r16, fp, -64
	addi r5, r0, 20
	add r3, r16, r0
	add r4, r12, r0
	jal r31, memcpy
	ldw r1, r13+16
	stw r12+16, r1
	ldw r1, r13+12
	stw r12+12, r1
	ldw r1, r13+8
	stw r12+8, r1
	ldw r1, r13+4
	stw r12+4, r1
	ldw r1, r13+0
	stw r12+0, r1
	ldw r1, r16+16
	stw r13+16, r1
	ldw r1, r16+12
	stw r13+12, r1
	ldw r1, r16+8
	stw r13+8, r1
	ldw r1, r16+4
	stw r13+4, r1
	ldw r1, r16+0
	stw r13+0, r1
	addi r16, r0, 1
.LBB36_49:
	bne r14, r18, .LBB36_53
.LBB36_50:
	ldw r3, r13+0
	addi r1, r0, 6
	bne r3, r1, .LBB36_53
.LBB36_51:
	ldw r3, r13+12
	ldw r4, r13+16
	bne r3, r4, .LBB36_53
.LBB36_52:
	ldw r3, r13+4
	addi r4, r3, -129
	addi r5, r0, -256
	bgeu r4, r5, .LBB36_153
.LBB36_53:
	ldw r1, r13+12
	ldw r3, r13+16
	bne r1, r3, .LBB36_58
.LBB36_54:
	ldw r1, r13+0
	addi r1, r1, -7
	addi r3, r0, -2
	bltu r1, r3, .LBB36_58
.LBB36_55:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_exp2K
	addi r3, r0, 0
	beq r1, r3, .LBB36_58
.LBB36_56:
	addi r1, r14, 6
	ldw r7, r13+4
	addi r6, r14, 22
	addi sp, sp, -4
	stw sp+0, r1
	addi r10, r0, 48
.LBB36_57:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	add r8, r16, r0
	jal r0, .LBB36_151
.LBB36_58:
	addi r1, r0, 0
	beq r17, r1, .LBB36_28
	jal r0, .LBB36_29
.LBB36_59:
	ldw r1, r13+0
	addi r3, r0, 6
	bne r1, r3, .LBB36_4
.LBB36_60:
	ldw r1, r13+12
	ldw r3, r13+16
	bne r1, r3, .LBB36_4
.LBB36_61:
	ldw r16, r13+4
	addi r1, r0, 128
	sub r1, r1, r16
	addi r3, r0, 127
	sub r7, r3, r16
	or  r1, r1, r7
	addi r3, r0, 255
	bgtu r1, r3, .LBB36_4
.LBB36_62:
	addi sp, sp, -4
	addi r1, r0, 7
	stw sp+0, r1
	addi r6, r0, 21
	addi r8, r0, 0
	addi r10, r0, 47
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	add r9, r15, r0
	jal r31, finishbinexpval
	addi sp, sp, 4
	ldw r1, r11+0
	ldw r1, r1+52
	ldw r3, r11+16
	slli r3, r3, 2
	add r1, r1, r3
	slli r3, r16, 16
	lui r4, 2032
	add r3, r3, r4
	srli r3, r3, 16
	stb r1+-2, r3
	jal r0, .LBB36_152
.LBB36_63:
	ldw r1, r12+0
	addi r3, r0, 6
	bne r1, r3, .LBB36_66
.LBB36_64:
	ldw r1, r12+12
	ldw r3, r12+16
	bne r1, r3, .LBB36_66
.LBB36_65:
	ldw r1, r12+4
	addi r1, r1, -129
	addi r3, r0, -256
	bgeu r1, r3, .LBB36_148
.LBB36_66:
	addi r6, r0, 32
	addi r8, r0, 16
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	add r7, r15, r0
	jal r31, finishbinexpneg
	addi r3, r0, 0
	bne r1, r3, .LBB36_152
.LBB36_67:
	addi r4, r0, 10
.LBB36_68:
	add r3, r11, r0
	jal r0, .LBB36_30
.LBB36_69:
	ldw r1, r12+12
	addi r3, r0, -1
	beq r1, r3, .LBB36_142
.LBB36_70:
	ldw r9, r13+12
	beq r9, r3, .LBB36_140
.LBB36_71:
	ldw r4, r11+0
	ldw r6, r4+52
	lui r4, 1044480
	addi r7, r4, 1
	addi r8, r0, 0
.LBB36_72:
	add r10, r9, r0
	slli r4, r9, 2
	add r4, r6, r4
	ldw r5, r4+0
	srli r9, r5, 7
	add r9, r9, r7
	seq r14, r9, r3
	add r9, r10, r9
	addi r9, r9, 1
	sub r14, r8, r14
	or  r9, r14, r9
	bne r9, r3, .LBB36_72
	jal r0, .LBB36_77
.LBB36_73:
	ldw r1, r12+16
	addi r3, r0, -1
	beq r1, r3, .LBB36_142
.LBB36_74:
	ldw r9, r13+16
	beq r9, r3, .LBB36_141
.LBB36_75:
	ldw r4, r11+0
	ldw r6, r4+52
	lui r4, 1044480
	addi r7, r4, 1
	addi r8, r0, 0
.LBB36_76:
	add r10, r9, r0
	slli r4, r9, 2
	add r4, r6, r4
	ldw r5, r4+0
	srli r9, r5, 7
	add r9, r9, r7
	seq r14, r9, r3
	add r9, r10, r9
	addi r9, r9, 1
	sub r14, r8, r14
	or  r9, r14, r9
	bne r9, r3, .LBB36_76
.LBB36_77:
	xor r3, r10, r3
	add r1, r1, r3
	lui r3, 4096
	addi r3, r3, -1
	add r3, r1, r3
	lui r6, 8192
	bgeu r3, r6, .LBB36_155
.LBB36_78:
	andi r3, r5, 127
	slli r1, r1, 7
	or  r1, r1, r3
	lui r3, 524288
	addi r3, r3, -128
	add r1, r1, r3
	stw r4+0, r1
	jal r0, .LBB36_142
.LBB36_79:
	ldw r4, r13+8
	ldw r3, r13+4
	addi r5, fp, -64
	addi r15, r0, 0
	add r6, r15, r0
	jal r31, luaV_flttointeger
	addi r3, r0, 1
	beq r1, r15, .LBB36_126
.LBB36_80:
	add r15, r3, r0
.LBB36_81:
	ldw r1, r13+12
	ldw r3, r13+16
	bne r1, r3, .LBB36_99
.LBB36_82:
	addi r1, fp, -64
	ldw r1, r1+0
	addi r19, r1, 127
	addi r1, r0, 255
	sgtu r3, r19, r1
	jal r0, .LBB36_127
.LBB36_83:
	ldw r4, r12+8
	ldw r3, r12+4
	addi r5, fp, -64
	addi r16, r0, 0
	add r6, r16, r0
	jal r31, luaV_flttointeger
	addi r3, r0, 1
	beq r1, r16, .LBB36_89
.LBB36_84:
	add r15, r3, r0
.LBB36_85:
	ldw r1, r12+12
	ldw r3, r12+16
	bne r1, r3, .LBB36_100
.LBB36_86:
	addi r1, fp, -64
	ldw r1, r1+0
	addi r1, r1, 127
	addi r3, r0, 255
	sgtu r3, r1, r3
	xor r4, r17, r1
	addi r5, r0, 0
	sub r5, r5, r3
	and r4, r4, r5
	xor r17, r1, r4
	addi r1, r0, 0
	bne r3, r1, .LBB36_90
	jal r0, .LBB36_101
.LBB36_87:
                                        # implicit-def: $r19
	add r15, r18, r0
	add r3, r20, r0
	jal r0, .LBB36_127
.LBB36_88:
	add r3, r18, r0
.LBB36_89:
	addi r1, r0, 0
	beq r3, r1, .LBB36_101
.LBB36_90:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaK_dischargevars
	ldw r1, r12+0
	addi r17, r0, 8
	bne r1, r17, .LBB36_93
.LBB36_91:
	ldw r1, r12+12
	ldw r3, r12+16
	addi r16, r12, 4
	beq r1, r3, .LBB36_94
.LBB36_92:
	ldw r18, r16+0
	add r3, r11, r0
	jal r31, luaY_nvarstack
	bge r18, r1, .LBB36_145
.LBB36_93:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaK_exp2nextreg
	addi r16, r12, 4
.LBB36_94:
	ldw r16, r16+0
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_dischargevars
	ldw r1, r13+0
	bne r1, r17, .LBB36_97
.LBB36_95:
	ldw r1, r13+12
	ldw r3, r13+16
	addi r17, r13, 4
	beq r1, r3, .LBB36_98
.LBB36_96:
	ldw r18, r17+0
	add r3, r11, r0
	jal r31, luaY_nvarstack
	bge r18, r1, .LBB36_146
.LBB36_97:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_exp2nextreg
	addi r17, r13, 4
.LBB36_98:
	ldw r17, r17+0
	addi r1, r0, 44
	jal r0, .LBB36_123
.LBB36_99:
                                        # implicit-def: $r19
	add r3, r20, r0
	jal r0, .LBB36_127
.LBB36_100:
	add r3, r18, r0
	addi r1, r0, 0
	bne r3, r1, .LBB36_90
.LBB36_101:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_dischargevars
	ldw r1, r13+0
	addi r3, r0, 8
	bne r1, r3, .LBB36_104
.LBB36_102:
	ldw r1, r13+12
	ldw r3, r13+16
	addi r16, r13, 4
	beq r1, r3, .LBB36_105
.LBB36_103:
	ldw r18, r16+0
	add r3, r11, r0
	jal r31, luaY_nvarstack
	bge r18, r1, .LBB36_147
.LBB36_104:
	addi r16, r13, 4
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_exp2nextreg
.LBB36_105:
	ldw r16, r16+0
	addi r1, r0, 50
	jal r0, .LBB36_123
.LBB36_106:
	lui r14, %hi(previousinstruction.invalidinstruction)
	addi r14, r14, %lo(previousinstruction.invalidinstruction)
.LBB36_107:
	ldw r16, r14+0
	andi r1, r16, 127
	addi r3, r0, 53
	bne r1, r3, .LBB36_112
.LBB36_108:
	ldw r1, r13+0
	addi r3, r0, 8
	bne r1, r3, .LBB36_111
.LBB36_109:
	ldw r13, r13+4
	add r3, r11, r0
	jal r31, luaY_nvarstack
	blt r13, r1, .LBB36_111
.LBB36_110:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB36_111:
	ldw r1, r14+0
	lui r3, 1044488
	addi r3, r3, 127
	and r1, r1, r3
	ldw r3, r12+4
	slli r3, r3, 7
	lui r4, 8
	addi r4, r4, -128
	and r3, r3, r4
	lui r4, 16
	add r4, r16, r4
	lui r5, 4080
	and r4, r4, r5
	or  r1, r1, r4
	or  r1, r1, r3
	stw r14+0, r1
	jal r0, .LBB36_152
.LBB36_112:
	ldw r1, r12+4
	slli r1, r1, 7
	lui r3, 32
	addi r3, r3, 53
	or  r14, r1, r3
	ldw r12, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r12+52
	addi r6, r12, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r12+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r14
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r12, r0
	jal r31, savelineinfo
	ldw r1, r13+0
	addi r3, r0, 8
	bne r1, r3, .LBB36_115
.LBB36_113:
	ldw r12, r13+4
	add r3, r11, r0
	jal r31, luaY_nvarstack
	blt r12, r1, .LBB36_115
.LBB36_114:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB36_115:
	ldw r4, r11+0
	ldw r1, r11+16
	ldw r3, r4+64
	add r1, r3, r1
	ldb r1, r1+-1
	addi r3, r0, -128
	bne r1, r3, .LBB36_124
.LBB36_116:
	ldw r1, r11+36
	addi r1, r1, -1
	stw r11+36, r1
	addi r1, r0, 129
	jal r0, .LBB36_125
.LBB36_117:
                                        # implicit-def: $r17
	bne r3, r18, .LBB36_37
.LBB36_118:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaK_dischargevars
	ldw r1, r12+0
	addi r3, r0, 8
	bne r1, r3, .LBB36_121
.LBB36_119:
	ldw r1, r12+12
	ldw r3, r12+16
	addi r16, r12, 4
	beq r1, r3, .LBB36_122
.LBB36_120:
	ldw r18, r16+0
	add r3, r11, r0
	jal r31, luaY_nvarstack
	bge r18, r1, .LBB36_139
.LBB36_121:
	addi r16, r12, 4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaK_exp2nextreg
.LBB36_122:
	ldw r16, r16+0
	addi r1, r0, 48
.LBB36_123:
	add r14, r14, r1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, freeexps
	slli r1, r16, 7
	slli r3, r17, 16
	slli r4, r15, 24
	or  r1, r4, r1
	or  r1, r1, r14
	or  r1, r1, r3
	lui r3, 8
	jal r0, .LBB36_137
.LBB36_124:
	ldw r3, r11+24
	sub r1, r3, r1
	stw r11+24, r1
	ldbu r1, r11+53
	addi r1, r1, -1
.LBB36_125:
	stb r11+53, r1
	add r3, r11, r0
	add r5, r15, r0
	jal r31, savelineinfo
	jal r0, .LBB36_152
.LBB36_126:
                                        # implicit-def: $r19
.LBB36_127:
	beq r3, r18, .LBB36_130
.LBB36_128:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, exp2RK
	addi r3, r0, 0
	beq r1, r3, .LBB36_131
.LBB36_129:
	ldw r19, r13+4
	addi r16, r0, 60
	jal r0, .LBB36_136
.LBB36_130:
	addi r16, r0, 61
	jal r0, .LBB36_136
.LBB36_131:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_dischargevars
	ldw r1, r13+0
	bne r1, r16, .LBB36_134
.LBB36_132:
	ldw r1, r13+12
	ldw r3, r13+16
	addi r16, r13, 4
	beq r1, r3, .LBB36_135
.LBB36_133:
	ldw r18, r16+0
	add r3, r11, r0
	jal r31, luaY_nvarstack
	bge r18, r1, .LBB36_154
.LBB36_134:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_exp2nextreg
	addi r16, r13, 4
.LBB36_135:
	ldw r19, r16+0
	addi r16, r0, 57
.LBB36_136:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, freeexps
	addi r1, r0, 13
	seq r1, r14, r1
	slli r3, r17, 7
	or  r3, r16, r3
	slli r4, r19, 16
	slli r5, r15, 24
	addi r6, r0, 0
	sub r1, r6, r1
	lui r6, 8
	and r1, r1, r6
	or  r1, r5, r1
	or  r1, r1, r4
.LBB36_137:
	or  r17, r1, r3
	ldw r13, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r13+52
	ldw r5, r11+16
	addi r6, r13, 20
	lui r14, %hi(.L.str)
	addi r14, r14, %lo(.L.str)
	lui r1, 262144
	addi r15, r1, -1
	addi r16, r0, 4
	add r7, r16, r0
	add r8, r15, r0
	add r9, r14, r0
	jal r31, luaM_growaux_
	stw r13+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r17
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r13, r0
	jal r31, savelineinfo
	ldw r5, r11+16
	ldw r13, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r13+52
	addi r6, r13, 20
	add r7, r16, r0
	add r8, r15, r0
	add r9, r14, r0
	jal r31, luaM_growaux_
	stw r13+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	lui r3, 524288
	addi r3, r3, -200
	stw r1+0, r3
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r13, r0
	jal r31, savelineinfo
	ldw r1, r11+16
	addi r1, r1, -1
	stw r12+4, r1
	addi r1, r0, 16
	jal r0, .LBB36_143
.LBB36_138:
	ldw r5, r12+4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, exp2reg
	jal r0, .LBB36_18
.LBB36_139:
	ldw r5, r12+4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, exp2reg
	jal r0, .LBB36_122
.LBB36_140:
	stw r13+12, r1
	jal r0, .LBB36_142
.LBB36_141:
	stw r13+16, r1
.LBB36_142:
	ldw r1, r13+16
	stw r12+16, r1
	ldw r1, r13+12
	stw r12+12, r1
	ldw r1, r13+8
	stw r12+8, r1
	ldw r1, r13+4
	stw r12+4, r1
	ldw r1, r13+0
.LBB36_143:
	stw r12+0, r1
	jal r0, .LBB36_152
.LBB36_144:
	ldw r1, r13+4
	addi r7, r1, 127
	addi sp, sp, -4
	addi r1, r0, 17
	stw sp+0, r1
	addi r6, r0, 32
	addi r8, r0, 0
	jal r0, .LBB36_149
.LBB36_145:
	ldw r5, r12+4
	add r3, r11, r0
	add r4, r12, r0
	jal r31, exp2reg
	jal r0, .LBB36_94
.LBB36_146:
	ldw r5, r13+4
	add r3, r11, r0
	add r4, r13, r0
	jal r31, exp2reg
	jal r0, .LBB36_98
.LBB36_147:
	ldw r5, r13+4
	add r3, r11, r0
	add r4, r13, r0
	jal r31, exp2reg
	jal r0, .LBB36_105
.LBB36_148:
	addi r14, fp, -64
	addi r5, r0, 20
	add r3, r14, r0
	add r4, r12, r0
	jal r31, memcpy
	ldw r1, r13+16
	stw r12+16, r1
	ldw r1, r13+12
	stw r12+12, r1
	ldw r1, r13+8
	stw r12+8, r1
	ldw r1, r13+4
	stw r12+4, r1
	ldw r1, r13+0
	stw r12+0, r1
	ldw r1, r14+16
	stw r13+16, r1
	ldw r1, r14+12
	stw r13+12, r1
	ldw r1, r14+8
	stw r13+8, r1
	ldw r1, r14+4
	stw r13+4, r1
	ldw r1, r14+0
	stw r13+0, r1
	ldw r1, r13+4
	addi r7, r1, 127
	addi sp, sp, -4
	addi r1, r0, 16
	stw sp+0, r1
	addi r6, r0, 33
	addi r8, r0, 1
.LBB36_149:
	addi r10, r0, 47
.LBB36_150:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
.LBB36_151:
	add r9, r15, r0
	jal r31, finishbinexpval
	addi sp, sp, 4
.LBB36_152:
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
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB36_153:
	addi r7, r3, 127
	addi sp, sp, -4
	stw sp+0, r1
	addi r6, r0, 21
	addi r10, r0, 47
	jal r0, .LBB36_57
.LBB36_154:
	ldw r5, r13+4
	add r3, r11, r0
	add r4, r13, r0
	jal r31, exp2reg
	jal r0, .LBB36_135
.LBB36_155:
	ldw r3, r11+8
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r31, luaX_syntaxerror
.Lfunc_end36:
	.size	luaK_posfix, .Lfunc_end36-luaK_posfix
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI36_0:
	.word	.LBB36_10
	.word	.LBB36_59
	.word	.LBB36_10
	.word	.LBB36_4
	.word	.LBB36_4
	.word	.LBB36_4
	.word	.LBB36_4
	.word	.LBB36_8
	.word	.LBB36_8
	.word	.LBB36_8
	.word	.LBB36_63
	.word	.LBB36_40
	.word	.LBB36_44
	.word	.LBB36_12
	.word	.LBB36_22
	.word	.LBB36_22
	.word	.LBB36_12
	.word	.LBB36_21
	.word	.LBB36_21
	.word	.LBB36_73
	.word	.LBB36_69
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function finishbinexpneg
	.type	finishbinexpneg,@function
finishbinexpneg:                        # @finishbinexpneg
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r9, r7, r0
	ldw r7, r5+0
	addi r1, r0, 0
	addi r10, r0, 6
	bne r7, r10, .LBB37_4
.LBB37_1:
	ldw r7, r5+12
	ldw r10, r5+16
	bne r7, r10, .LBB37_4
.LBB37_2:
	ldw r12, r5+4
	addi r7, r0, 128
	sub r10, r7, r12
	addi r7, r0, 127
	sub r7, r7, r12
	or  r10, r10, r7
	addi r11, r0, 255
	bgtu r10, r11, .LBB37_4
.LBB37_3:
	addi sp, sp, -4
	stw sp+0, r8
	addi r8, r0, 0
	addi r10, r0, 47
	add r11, r3, r0
	jal r31, finishbinexpval
	addi sp, sp, 4
	ldw r1, r11+0
	ldw r1, r1+52
	ldw r3, r11+16
	slli r3, r3, 2
	add r1, r1, r3
	slli r3, r12, 16
	lui r4, 2032
	add r3, r3, r4
	srli r3, r3, 16
	stb r1+-2, r3
	addi r1, r0, 1
.LBB37_4:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end37:
	.size	finishbinexpneg, .Lfunc_end37-finishbinexpneg
                                        # -- End function
	.p2align	2                               # -- Begin function codebinexpval
	.type	codebinexpval,@function
codebinexpval:                          # @codebinexpval
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
	add r12, r6, r0
	add r13, r5, r0
	add r15, r4, r0
	add r14, r3, r0
	add r4, r6, r0
	jal r31, luaK_dischargevars
	ldw r1, r12+0
	addi r3, r0, 8
	bne r1, r3, .LBB38_3
.LBB38_1:
	ldw r1, r12+12
	ldw r3, r12+16
	beq r1, r3, .LBB38_4
.LBB38_2:
	ldw r16, r12+4
	add r3, r14, r0
	jal r31, luaY_nvarstack
	bge r16, r1, .LBB38_5
.LBB38_3:
	add r3, r14, r0
	add r4, r12, r0
	jal r31, luaK_exp2nextreg
.LBB38_4:
	addi r6, r15, 34
	ldw r7, r12+4
	addi r1, r15, 6
	addi sp, sp, -4
	stw sp+0, r1
	addi r8, r0, 0
	addi r10, r0, 46
	add r3, r14, r0
	add r4, r13, r0
	add r5, r12, r0
	add r9, r11, r0
	jal r31, finishbinexpval
	addi sp, sp, 4
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
.LBB38_5:
	ldw r5, r12+4
	add r3, r14, r0
	add r4, r12, r0
	jal r31, exp2reg
	jal r0, .LBB38_4
.Lfunc_end38:
	.size	codebinexpval, .Lfunc_end38-codebinexpval
                                        # -- End function
	.hidden	luaK_fixline                    # -- Begin function luaK_fixline
	.globl	luaK_fixline
	.p2align	2
	.type	luaK_fixline,@function
luaK_fixline:                           # @luaK_fixline
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	add r5, r4, r0
	ldw r4, r3+0
	ldw r1, r3+16
	ldw r6, r4+64
	add r1, r6, r1
	ldb r1, r1+-1
	addi r6, r0, -128
	bne r1, r6, .LBB39_2
.LBB39_1:
	ldw r1, r3+36
	addi r1, r1, -1
	stw r3+36, r1
	addi r1, r0, 129
	jal r0, .LBB39_3
.LBB39_2:
	ldw r6, r3+24
	sub r1, r6, r1
	stw r3+24, r1
	ldbu r1, r3+53
	addi r1, r1, -1
.LBB39_3:
	stb r3+53, r1
	jal r31, savelineinfo
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end39:
	.size	luaK_fixline, .Lfunc_end39-luaK_fixline
                                        # -- End function
	.hidden	luaK_settablesize               # -- Begin function luaK_settablesize
	.globl	luaK_settablesize
	.p2align	2
	.type	luaK_settablesize,@function
luaK_settablesize:                      # @luaK_settablesize
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
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	ldw r1, r3+0
	ldw r15, r1+52
	addi r14, r0, 0
	add r1, r14, r0
	beq r7, r14, .LBB40_2
.LBB40_1:
	add r3, r7, r0
	jal r31, luaO_ceillog2
	slli r1, r1, 16
	lui r3, 16
	add r1, r1, r3
.LBB40_2:
	slli r3, r13, 2
	add r3, r15, r3
	srai r4, r11, 31
	srli r4, r4, 24
	add r4, r11, r4
	srai r4, r4, 8
	addi r5, r0, 255
	sgt r5, r11, r5
	slli r6, r12, 7
	slli r7, r11, 24
	sub r5, r14, r5
	lui r8, 8
	and r5, r5, r8
	or  r6, r6, r7
	or  r5, r6, r5
	or  r1, r5, r1
	ori  r1, r1, 19
	stw r3+0, r1
	slli r1, r4, 7
	addi r1, r1, 82
	stw r3+4, r1
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
.Lfunc_end40:
	.size	luaK_settablesize, .Lfunc_end40-luaK_settablesize
                                        # -- End function
	.hidden	luaK_setlist                    # -- Begin function luaK_setlist
	.globl	luaK_setlist
	.p2align	2
	.type	luaK_setlist,@function
luaK_setlist:                           # @luaK_setlist
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
	add r14, r5, r0
	add r12, r4, r0
	add r11, r3, r0
	addi r1, r0, -1
	seq r1, r6, r1
	slli r3, r4, 7
	slli r4, r6, 16
	addi r5, r0, 0
	sub r1, r5, r1
	and r1, r4, r1
	xor r1, r4, r1
	slli r4, r14, 24
	or  r3, r3, r4
	or  r17, r3, r1
	ldw r13, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r13+52
	ldw r5, r11+16
	addi r6, r13, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r16, 262144
	addi r15, r16, -1
	addi r7, r0, 4
	add r8, r15, r0
	jal r31, luaM_growaux_
	stw r13+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	addi r3, r0, 255
	bgt r14, r3, .LBB41_2
.LBB41_1:
	addi r3, r17, 78
	stw r1+0, r3
	jal r0, .LBB41_3
.LBB41_2:
	lui r3, 8
	addi r3, r3, 78
	or  r3, r17, r3
	stw r1+0, r3
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r13, r0
	jal r31, savelineinfo
	ldw r5, r11+16
	srli r1, r14, 1
	addi r3, r16, -128
	and r1, r1, r3
	addi r14, r1, 82
	ldw r13, r11+0
	ldw r1, r11+8
	ldw r3, r1+40
	ldw r4, r13+52
	addi r6, r13, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	addi r7, r0, 4
	add r8, r15, r0
	jal r31, luaM_growaux_
	stw r13+52, r1
	ldw r3, r11+16
	addi r4, r3, 1
	stw r11+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r14
.LBB41_3:
	ldw r1, r11+8
	ldw r5, r1+8
	add r3, r11, r0
	add r4, r13, r0
	jal r31, savelineinfo
	addi r1, r12, 1
	stb r11+52, r1
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
.Lfunc_end41:
	.size	luaK_setlist, .Lfunc_end41-luaK_setlist
                                        # -- End function
	.hidden	luaK_finish                     # -- Begin function luaK_finish
	.globl	luaK_finish
	.p2align	2
	.type	luaK_finish,@function
luaK_finish:                            # @luaK_finish
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
	stw fp+-52, r23
	stw fp+-56, lr
	ldw r1, r3+16
	addi r4, r0, 1
	blt r1, r4, .LBB42_19
.LBB42_1:
	ldw r1, r3+0
	ldw r4, r1+52
	addi r5, r0, 0
	addi r6, r0, 2
	lui r7, 4096
	addi r8, r7, -1
	lui r9, 8
	addi r10, r0, -128
	addi r11, r0, 56
	addi r12, r0, 99
	lui r13, 1044480
	addi r13, r13, 2
	addi r14, r0, -1
	lui r15, 8192
	lui r16, 524288
	addi r16, r16, -72
	add r17, r5, r0
	jal r0, .LBB42_4
.LBB42_2:
	stw r18+0, r19
.LBB42_3:
	addi r17, r17, 1
	ldw r18, r3+16
	bge r17, r18, .LBB42_19
.LBB42_4:
	slli r18, r17, 2
	add r18, r4, r18
	ldw r19, r18+0
	andi r20, r19, 127
	addi r21, r20, -69
	bltu r21, r6, .LBB42_9
.LBB42_5:
	addi r21, r20, -71
	bgeu r21, r6, .LBB42_13
.LBB42_6:
	ldbu r20, r3+54
	bne r20, r5, .LBB42_8
.LBB42_7:
	ldbu r20, r1+7
	beq r20, r5, .LBB42_3
.LBB42_8:
	and r19, r19, r10
	addi r19, r19, 70
	stw r18+0, r19
.LBB42_9:
	ldbu r19, r3+54
	beq r19, r5, .LBB42_11
.LBB42_10:
	ldw r19, r18+0
	or  r19, r19, r9
	stw r18+0, r19
.LBB42_11:
	ldbu r19, r1+7
	beq r19, r5, .LBB42_3
.LBB42_12:
	ldw r19, r18+0
	and r19, r19, r8
	ldbu r20, r1+6
	slli r20, r20, 24
	or  r19, r20, r19
	add r19, r19, r7
	jal r0, .LBB42_2
.LBB42_13:
	bne r20, r11, .LBB42_3
.LBB42_14:
	add r21, r12, r0
	add r19, r17, r0
.LBB42_15:
	add r20, r21, r0
	slli r21, r19, 2
	add r21, r4, r21
	ldw r21, r21+0
	andi r22, r21, 127
	sne r23, r22, r11
	srli r21, r21, 7
	add r21, r21, r13
	sub r23, r5, r23
	and r23, r21, r23
	xor r21, r21, r23
	add r19, r21, r19
	bne r22, r11, .LBB42_17
.LBB42_16:
	addi r21, r20, -1
	bne r20, r5, .LBB42_15
.LBB42_17:
	xor r20, r17, r14
	add r19, r19, r20
	add r20, r19, r8
	bgeu r20, r15, .LBB42_20
.LBB42_18:
	slli r19, r19, 7
	add r19, r19, r16
	jal r0, .LBB42_2
.LBB42_19:
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
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB42_20:
	ldw r3, r3+8
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r31, luaX_syntaxerror
.Lfunc_end42:
	.size	luaK_finish, .Lfunc_end42-luaK_finish
                                        # -- End function
	.p2align	2                               # -- Begin function addk
	.type	addk,@function
addk:                                   # @addk
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
	add r14, r5, r0
	add r16, r4, r0
	add r15, r3, r0
	ldw r1, r3+8
	ldw r11, r1+40
	ldw r12, r3+0
	ldw r3, r1+52
	jal r31, luaH_get
	ldbu r3, r1+8
	addi r19, r0, 3
	bne r3, r19, .LBB43_3
.LBB43_1:
	ldw r13, r1+0
	ldw r3, r15+28
	bge r13, r3, .LBB43_3
.LBB43_2:
	ldw r3, r12+48
	addi r4, r0, 12
	mul r4, r13, r4
	add r4, r3, r4
	ldbu r3, r4+8
	ldbu r5, r14+8
	xor r3, r5, r3
	andi r3, r3, 63
	addi r17, r0, 0
	beq r3, r17, .LBB43_10
.LBB43_3:
	addi r17, r12, 16
	ldw r18, r12+16
	ldw r13, r15+28
	addi r7, fp, -52
	stw r7+0, r13
	stb r7+8, r19
	ldw r3, r15+8
	ldw r4, r3+52
	add r3, r11, r0
	add r5, r16, r0
	add r6, r1, r0
	jal r31, luaH_finishset
	ldw r4, r12+48
	lui r9, %hi(.L.str.4)
	addi r9, r9, %lo(.L.str.4)
	lui r1, 8192
	addi r8, r1, -1
	addi r16, r0, 12
	add r3, r11, r0
	add r5, r13, r0
	add r6, r17, r0
	add r7, r16, r0
	jal r31, luaM_growaux_
	stw r12+48, r1
	ldw r4, r12+16
	ble r4, r18, .LBB43_6
.LBB43_4:
	mul r3, r18, r16
	add r3, r3, r1
	addi r3, r3, 8
	sub r4, r4, r18
	addi r5, r0, 0
.LBB43_5:
	stb r3+0, r5
	addi r3, r3, 12
	addi r4, r4, -1
	bne r4, r5, .LBB43_5
.LBB43_6:
	mul r3, r13, r16
	add r1, r1, r3
	ldw r3, r14+0
	ldw r4, r14+4
	stw r1+4, r4
	stw r1+0, r3
	ldbu r3, r14+8
	stb r1+8, r3
	ldw r1, r15+28
	addi r1, r1, 1
	stw r15+28, r1
	ldbu r1, r14+8
	andi r3, r1, 64
	addi r1, r0, 0
	beq r3, r1, .LBB43_11
.LBB43_7:
	ldbu r3, r12+5
	andi r3, r3, 32
	beq r3, r1, .LBB43_11
.LBB43_8:
	ldw r5, r14+0
	ldbu r3, r5+5
	andi r3, r3, 24
	beq r3, r1, .LBB43_11
.LBB43_9:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaC_barrier_
	jal r0, .LBB43_11
.LBB43_10:
	add r3, r17, r0
	add r5, r14, r0
	add r18, r1, r0
	jal r31, luaV_equalobj
	add r3, r1, r0
	add r1, r18, r0
	beq r3, r17, .LBB43_3
.LBB43_11:
	add r1, r13, r0
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
.Lfunc_end43:
	.size	addk, .Lfunc_end43-addk
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function discharge2reg
.LCPI44_0:
	.quad	0x3ff0000000000000              # double 1
	.text
	.p2align	2
	.type	discharge2reg,@function
discharge2reg:                          # @discharge2reg
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
	add r12, r5, r0
	add r11, r4, r0
	add r13, r3, r0
	jal r31, luaK_dischargevars
	ldw r1, r11+0
	addi r1, r1, -1
	addi r3, r0, 16
	bgtu r1, r3, .LBB44_18
.LBB44_1:
	slli r1, r1, 2
	lui r3, %hi(.LJTI44_0)
	addi r3, r3, %lo(.LJTI44_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB44_2:
	addi r5, r0, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, luaK_nil
	jal r0, .LBB44_17
.LBB44_3:
	slli r1, r12, 7
	addi r15, r1, 5
	jal r0, .LBB44_16
.LBB44_4:
	ldw r5, r11+4
	add r3, r13, r0
	add r4, r12, r0
	jal r31, luaK_int
	jal r0, .LBB44_17
.LBB44_5:
	slli r1, r12, 7
	addi r15, r1, 7
	jal r0, .LBB44_16
.LBB44_6:
	ldw r1, r11+4
	addi r4, fp, -48
	stw r4+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r4+8, r1
	add r3, r13, r0
	add r5, r4, r0
	jal r31, addk
	stw r11+4, r1
	addi r1, r0, 4
	stw r11+0, r1
.LBB44_7:
	ldw r5, r11+4
	add r3, r13, r0
	add r4, r12, r0
	jal r31, luaK_codek
	jal r0, .LBB44_17
.LBB44_8:
	ldw r1, r11+4
	beq r12, r1, .LBB44_17
.LBB44_9:
	slli r3, r12, 7
	slli r1, r1, 16
	or  r15, r1, r3
	jal r0, .LBB44_16
.LBB44_10:
	ldw r17, r11+8
	ldw r16, r11+4
	addi r15, fp, -68
	addi r14, r0, 0
	add r3, r16, r0
	add r4, r17, r0
	add r5, r15, r0
	add r6, r14, r0
	jal r31, luaV_flttointeger
	beq r1, r14, .LBB44_12
.LBB44_11:
	ldw r1, r15+0
	lui r3, 1048560
	addi r3, r3, -1
	add r3, r1, r3
	lui r4, 1048544
	bgeu r3, r4, .LBB44_15
.LBB44_12:
	addi r1, fp, -48
	stw r1+4, r17
	stw r1+0, r16
	addi r18, r0, 19
	stb r1+8, r18
	addi r15, fp, -52
	add r3, r16, r0
	add r4, r17, r0
	add r5, r15, r0
	add r6, r14, r0
	jal r31, luaV_flttointeger
	beq r1, r14, .LBB44_19
.LBB44_13:
	lui r1, %hi(.LCPI44_0)
	addi r1, r1, %lo(.LCPI44_0)
	ldw r4, r1+4
	ldw r3, r1+0
	addi r5, r0, -52
	jal r31, ldexp
	add r4, r1, r0
	add r5, r2, r0
	ldw r1, r15+0
	seq r1, r1, r14
	fmul.d r6, r16, r4
	fadd.d r6, r6, r16
	xor r3, r2, r7
	sub r1, r14, r1
	and r3, r3, r1
	xor r3, r7, r3
	xor r4, r4, r6
	and r1, r4, r1
	xor r1, r6, r1
	addi r4, fp, -64
	stw r4+4, r3
	stw r4+0, r1
	stb r4+8, r18
	addi r5, fp, -48
	add r3, r13, r0
	jal r0, .LBB44_20
.LBB44_14:
	ldw r1, r13+0
	ldw r1, r1+52
	ldw r3, r11+4
	slli r3, r3, 2
	add r1, r1, r3
	ldw r3, r1+0
	lui r4, 1048568
	addi r4, r4, 127
	and r3, r3, r4
	slli r4, r12, 7
	lui r5, 8
	addi r5, r5, -128
	and r4, r4, r5
	or  r3, r3, r4
	stw r1+0, r3
	jal r0, .LBB44_17
.LBB44_15:
	slli r3, r12, 7
	slli r1, r1, 15
	lui r4, 524280
	add r1, r1, r4
	or  r1, r3, r1
	ori  r15, r1, 2
.LBB44_16:
	ldw r14, r13+0
	ldw r1, r13+8
	ldw r3, r1+40
	ldw r4, r14+52
	ldw r5, r13+16
	addi r6, r14, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r8, r1, -1
	addi r7, r0, 4
	jal r31, luaM_growaux_
	stw r14+52, r1
	ldw r3, r13+16
	addi r4, r3, 1
	stw r13+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r15
	ldw r1, r13+8
	ldw r5, r1+8
	add r3, r13, r0
	add r4, r14, r0
	jal r31, savelineinfo
.LBB44_17:
	stw r11+4, r12
	addi r1, r0, 8
	stw r11+0, r1
.LBB44_18:
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
.LBB44_19:
	addi r4, fp, -48
	add r3, r13, r0
	add r5, r4, r0
.LBB44_20:
	jal r31, addk
	add r3, r13, r0
	add r4, r12, r0
	add r5, r1, r0
	jal r31, luaK_codek
	jal r0, .LBB44_17
.Lfunc_end44:
	.size	discharge2reg, .Lfunc_end44-discharge2reg
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI44_0:
	.word	.LBB44_2
	.word	.LBB44_5
	.word	.LBB44_3
	.word	.LBB44_7
	.word	.LBB44_10
	.word	.LBB44_4
	.word	.LBB44_6
	.word	.LBB44_8
	.word	.LBB44_18
	.word	.LBB44_18
	.word	.LBB44_18
	.word	.LBB44_18
	.word	.LBB44_18
	.word	.LBB44_18
	.word	.LBB44_18
	.word	.LBB44_18
	.word	.LBB44_14
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaK_exp2K
.LCPI45_0:
	.quad	0x3ff0000000000000              # double 1
	.text
	.p2align	2
	.type	luaK_exp2K,@function
luaK_exp2K:                             # @luaK_exp2K
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
	stw fp+-28, r18
	stw fp+-32, r19
	stw fp+-36, lr
	ldw r1, r4+12
	ldw r5, r4+16
	bne r1, r5, .LBB45_18
.LBB45_1:
	ldw r1, r4+0
	addi r14, r0, 0
	addi r1, r1, -1
	addi r5, r0, 6
	bgtu r1, r5, .LBB45_9
.LBB45_2:
	slli r1, r1, 2
	lui r5, %hi(.LJTI45_0)
	addi r5, r5, %lo(.LJTI45_0)
	add r1, r5, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB45_3:
	add r15, r4, r0
	addi r1, r0, 0
	addi r5, fp, -64
	stb r5+8, r1
	ldw r1, r3+8
	ldw r1, r1+52
	addi r4, fp, -48
	stw r4+0, r1
	addi r1, r0, 69
	stb r4+8, r1
	jal r0, .LBB45_16
.LBB45_4:
	add r11, r3, r0
	ldw r19, r4+8
	add r15, r4, r0
	ldw r18, r4+4
	addi r1, fp, -48
	stw r1+4, r19
	stw r1+0, r18
	addi r16, r0, 19
	stb r1+8, r16
	addi r13, fp, -52
	addi r12, r0, 0
	add r3, r18, r0
	add r4, r19, r0
	add r5, r13, r0
	add r6, r12, r0
	jal r31, luaV_flttointeger
	beq r1, r12, .LBB45_20
.LBB45_5:
	lui r1, %hi(.LCPI45_0)
	addi r1, r1, %lo(.LCPI45_0)
	ldw r4, r1+4
	ldw r3, r1+0
	addi r5, r0, -52
	jal r31, ldexp
	add r4, r1, r0
	add r5, r2, r0
	ldw r1, r13+0
	seq r1, r1, r12
	fmul.d r6, r18, r4
	fadd.d r6, r6, r18
	xor r3, r2, r7
	sub r1, r12, r1
	and r3, r3, r1
	xor r3, r7, r3
	xor r4, r4, r6
	and r1, r4, r1
	xor r1, r6, r1
	addi r4, fp, -64
	stw r4+4, r3
	stw r4+0, r1
	stb r4+8, r16
	addi r5, fp, -48
	add r3, r11, r0
	jal r0, .LBB45_16
.LBB45_6:
	add r15, r4, r0
	addi r1, r0, 1
	jal r0, .LBB45_11
.LBB45_7:
	add r15, r4, r0
	ldw r1, r4+4
	addi r3, r0, 255
	ble r1, r3, .LBB45_17
.LBB45_8:
	addi r3, r0, 1
                                        # implicit-def: $r1
	bne r3, r14, .LBB45_18
	jal r0, .LBB45_19
.LBB45_9:
	add r1, r14, r0
	add r3, r14, r0
	beq r3, r14, .LBB45_19
	jal r0, .LBB45_18
.LBB45_10:
	add r15, r4, r0
	addi r1, r0, 17
.LBB45_11:
	addi r4, fp, -48
	jal r0, .LBB45_14
.LBB45_12:
	add r15, r4, r0
	ldw r1, r4+4
	addi r4, fp, -48
	stw r4+0, r1
	addi r1, r0, 3
	jal r0, .LBB45_14
.LBB45_13:
	add r15, r4, r0
	ldw r1, r4+4
	addi r4, fp, -48
	stw r4+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
.LBB45_14:
	stb r4+8, r1
.LBB45_15:
	add r5, r4, r0
.LBB45_16:
	jal r31, addk
	addi r3, r0, 255
	bgt r1, r3, .LBB45_8
.LBB45_17:
	addi r3, r0, 4
	stw r15+0, r3
	stw r15+4, r1
	addi r1, r0, 1
	add r3, r14, r0
	beq r3, r14, .LBB45_19
.LBB45_18:
	addi r1, r0, 0
.LBB45_19:
	ldw lr, fp+-36
	ldw r19, fp+-32
	ldw r18, fp+-28
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
.LBB45_20:
	addi r4, fp, -48
	add r3, r11, r0
	jal r0, .LBB45_15
.Lfunc_end45:
	.size	luaK_exp2K, .Lfunc_end45-luaK_exp2K
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI45_0:
	.word	.LBB45_3
	.word	.LBB45_10
	.word	.LBB45_6
	.word	.LBB45_7
	.word	.LBB45_4
	.word	.LBB45_12
	.word	.LBB45_13
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function finishbinexpval
	.type	finishbinexpval,@function
finishbinexpval:                        # @finishbinexpval
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
	add r14, r10, r0
	add r11, r9, r0
	add r13, r8, r0
	add r15, r7, r0
	add r18, r6, r0
	add r17, r5, r0
	add r16, r4, r0
	add r12, r3, r0
	addi r19, fp, 0
	jal r31, luaK_dischargevars
	ldw r1, r16+0
	addi r3, r0, 8
	bne r1, r3, .LBB46_3
.LBB46_1:
	ldw r1, r16+12
	ldw r3, r16+16
	beq r1, r3, .LBB46_4
.LBB46_2:
	ldw r20, r16+4
	add r3, r12, r0
	jal r31, luaY_nvarstack
	bge r20, r1, .LBB46_11
.LBB46_3:
	add r3, r12, r0
	add r4, r16, r0
	jal r31, luaK_exp2nextreg
.LBB46_4:
	ldw r20, r19+0
	ldw r21, r16+4
	slli r1, r21, 16
	slli r3, r15, 24
	or  r3, r3, r18
	or  r22, r3, r1
	ldw r19, r12+0
	ldw r1, r12+8
	ldw r3, r1+40
	ldw r4, r19+52
	ldw r5, r12+16
	addi r6, r19, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	lui r1, 262144
	addi r18, r1, -1
	addi r7, r0, 4
	add r8, r18, r0
	jal r31, luaM_growaux_
	stw r19+52, r1
	ldw r3, r12+16
	addi r4, r3, 1
	stw r12+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r22
	ldw r1, r12+8
	ldw r5, r1+8
	add r3, r12, r0
	add r4, r19, r0
	jal r31, savelineinfo
	ldw r1, r12+16
	addi r19, r1, -1
	add r3, r12, r0
	add r4, r16, r0
	add r5, r17, r0
	jal r31, freeexps
	stw r16+4, r19
	addi r1, r0, 17
	stw r16+0, r1
	ldw r4, r12+0
	ldw r1, r12+16
	ldw r3, r4+64
	add r1, r3, r1
	ldb r1, r1+-1
	addi r17, r0, -128
	addi r16, r0, 129
	bne r1, r17, .LBB46_6
.LBB46_5:
	ldw r1, r12+36
	addi r1, r1, -1
	stw r12+36, r1
	stb r12+53, r16
	jal r0, .LBB46_7
.LBB46_6:
	ldw r3, r12+24
	sub r1, r3, r1
	stw r12+24, r1
	ldbu r1, r12+53
	addi r1, r1, -1
	stb r12+53, r1
.LBB46_7:
	add r3, r12, r0
	add r5, r11, r0
	jal r31, savelineinfo
	slli r1, r21, 7
	or  r1, r1, r14
	slli r3, r15, 16
	slli r4, r20, 24
	slli r5, r13, 15
	or  r3, r5, r3
	or  r3, r3, r4
	or  r14, r3, r1
	ldw r13, r12+0
	ldw r1, r12+8
	ldw r3, r1+40
	ldw r4, r13+52
	ldw r5, r12+16
	addi r6, r13, 20
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	addi r7, r0, 4
	add r8, r18, r0
	jal r31, luaM_growaux_
	stw r13+52, r1
	ldw r3, r12+16
	addi r4, r3, 1
	stw r12+16, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r14
	ldw r1, r12+8
	ldw r5, r1+8
	add r3, r12, r0
	add r4, r13, r0
	jal r31, savelineinfo
	ldw r1, r12+16
	ldw r4, r12+0
	ldw r3, r4+64
	add r1, r3, r1
	ldb r1, r1+-1
	bne r1, r17, .LBB46_9
.LBB46_8:
	ldw r1, r12+36
	addi r1, r1, -1
	stw r12+36, r1
	stb r12+53, r16
	jal r0, .LBB46_10
.LBB46_9:
	ldw r3, r12+24
	sub r1, r3, r1
	stw r12+24, r1
	ldbu r1, r12+53
	addi r1, r1, -1
	stb r12+53, r1
.LBB46_10:
	add r3, r12, r0
	add r5, r11, r0
	jal r31, savelineinfo
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
.LBB46_11:
	ldw r5, r16+4
	add r3, r12, r0
	add r4, r16, r0
	jal r31, exp2reg
	jal r0, .LBB46_4
.Lfunc_end46:
	.size	finishbinexpval, .Lfunc_end46-finishbinexpval
                                        # -- End function
	.p2align	2                               # -- Begin function freeexps
	.type	freeexps,@function
freeexps:                               # @freeexps
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
	ldw r3, r4+0
	addi r12, r0, -1
	addi r1, r0, 8
	add r13, r12, r0
	bne r3, r1, .LBB47_2
.LBB47_1:
	ldw r13, r4+4
.LBB47_2:
	ldw r3, r5+0
	bne r3, r1, .LBB47_4
.LBB47_3:
	ldw r12, r5+4
.LBB47_4:
	add r3, r11, r0
	jal r31, luaY_nvarstack
	ble r13, r12, .LBB47_8
.LBB47_5:
	blt r13, r1, .LBB47_7
.LBB47_6:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB47_7:
	add r3, r11, r0
	jal r31, luaY_nvarstack
	bge r12, r1, .LBB47_11
	jal r0, .LBB47_12
.LBB47_8:
	blt r12, r1, .LBB47_10
.LBB47_9:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB47_10:
	add r3, r11, r0
	jal r31, luaY_nvarstack
	blt r13, r1, .LBB47_12
.LBB47_11:
	ldbu r1, r11+52
	addi r1, r1, -1
	stb r11+52, r1
.LBB47_12:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end47:
	.size	freeexps, .Lfunc_end47-freeexps
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"opcodes"
	.size	.L.str, 8

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"function or expression needs too many registers"
	.size	.L.str.1, 48

	.type	luaK_prefix.ef,@object          # @luaK_prefix.ef
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
luaK_prefix.ef:
	.word	6                               # 0x6
	.zero	8
	.word	4294967295                      # 0xffffffff
	.word	4294967295                      # 0xffffffff
	.size	luaK_prefix.ef, 20

	.type	previousinstruction.invalidinstruction,@object # @previousinstruction.invalidinstruction
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
previousinstruction.invalidinstruction:
	.word	4294967295                      # 0xffffffff
	.size	previousinstruction.invalidinstruction, 4

	.type	.L.str.2,@object                # @.str.2
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.2:
	.asciz	"control structure too long"
	.size	.L.str.2, 27

	.hidden	luaP_opmodes
	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"lines"
	.size	.L.str.3, 6

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"constants"
	.size	.L.str.4, 10

	.hidden	luaX_syntaxerror
	.hidden	luaM_growaux_
	.hidden	luaY_nvarstack
	.hidden	luaO_ceillog2
	.hidden	luaH_get
	.hidden	luaV_equalobj
	.hidden	luaH_finishset
	.hidden	luaC_barrier_
	.hidden	luaV_flttointeger
	.hidden	luaO_rawarith
	.hidden	luaV_tointegerns
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
