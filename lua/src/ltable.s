	.file	"ltable.c"
	.text
	.hidden	luaH_realasize                  # -- Begin function luaH_realasize
	.globl	luaH_realasize
	.p2align	2
	.type	luaH_realasize,@function
luaH_realasize:                         # @luaH_realasize
# %bb.0:
	ldb r1, r3+6
	addi r4, r0, -1
	ble r1, r4, .LBB0_2
.LBB0_1:
	ldw r1, r3+8
	jal r0, .LBB0_4
.LBB0_2:
	ldw r1, r3+8
	addi r4, r1, -1
	and r4, r1, r4
	addi r5, r0, 0
	beq r4, r5, .LBB0_1
.LBB0_3:
	srli r3, r1, 1
	or  r1, r3, r1
	srli r3, r1, 2
	or  r1, r3, r1
	srli r3, r1, 4
	or  r1, r3, r1
	srli r3, r1, 8
	or  r1, r3, r1
	srli r3, r1, 16
	or  r1, r3, r1
	addi r1, r1, 1
.LBB0_4:
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaH_realasize, .Lfunc_end0-luaH_realasize
                                        # -- End function
	.hidden	luaH_next                       # -- Begin function luaH_next
	.globl	luaH_next
	.p2align	2
	.type	luaH_next,@function
luaH_next:                              # @luaH_next
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
	ldb r1, r4+6
	addi r6, r0, -1
	ble r1, r6, .LBB1_2
.LBB1_1:
	ldw r12, r4+8
	jal r0, .LBB1_4
.LBB1_2:
	ldw r1, r4+8
	addi r6, r1, -1
	and r6, r1, r6
	addi r7, r0, 0
	beq r6, r7, .LBB1_1
.LBB1_3:
	srli r6, r1, 1
	or  r1, r6, r1
	srli r6, r1, 2
	or  r1, r6, r1
	srli r6, r1, 4
	or  r1, r6, r1
	srli r6, r1, 8
	or  r1, r6, r1
	srli r6, r1, 16
	or  r1, r6, r1
	addi r12, r1, 1
.LBB1_4:
	ldbu r6, r5+8
	andi r7, r6, 15
	addi r1, r0, 0
	beq r7, r1, .LBB1_7
.LBB1_5:
	addi r1, r0, 3
	bne r6, r1, .LBB1_8
.LBB1_6:
	ldw r1, r5+0
	addi r6, r1, -1
	lui r7, 87381
	addi r7, r7, 1365
	sltu r6, r6, r7
	addi r7, r0, 0
	sub r6, r7, r6
	and r1, r1, r6
	addi r6, r1, -1
	bgeu r6, r12, .LBB1_9
.LBB1_7:
	bltu r1, r12, .LBB1_11
	jal r0, .LBB1_15
.LBB1_8:
	addi r1, r0, 0
	addi r6, r1, -1
	bltu r6, r12, .LBB1_7
.LBB1_9:
	add r11, r3, r0
	addi r1, r0, 1
	add r14, r4, r0
	add r3, r4, r0
	add r13, r5, r0
	add r4, r5, r0
	add r5, r1, r0
	jal r31, getgeneric
	ldbu r3, r1+8
	addi r4, r0, 32
	beq r3, r4, .LBB1_24
.LBB1_10:
	add r4, r14, r0
	ldw r3, r14+16
	sub r1, r1, r3
	srai r1, r1, 3
	lui r3, 699051
	addi r3, r3, -1365
	mul r1, r1, r3
	add r1, r12, r1
	addi r1, r1, 1
	add r5, r13, r0
	bgeu r1, r12, .LBB1_15
.LBB1_11:
	ldw r6, r4+12
	addi r3, r0, 12
	mul r3, r1, r3
	addi r6, r6, 8
	addi r7, r0, 0
.LBB1_12:
	add r8, r6, r3
	ldbu r8, r8+0
	addi r1, r1, 1
	andi r8, r8, 15
	bne r8, r7, .LBB1_20
.LBB1_13:
	addi r3, r3, 12
	bne r12, r1, .LBB1_12
.LBB1_14:
	add r1, r12, r0
.LBB1_15:
	sub r3, r1, r12
	ldbu r6, r4+7
	addi r7, r0, 1
	sll r6, r7, r6
	bge r3, r6, .LBB1_19
.LBB1_16:
	ldw r3, r4+16
	addi r4, r0, 24
	mul r7, r1, r4
	mul r4, r12, r4
	sub r4, r7, r4
	add r3, r3, r4
	add r4, r12, r6
	sub r4, r4, r1
	addi r1, r0, 0
.LBB1_17:
	ldbu r6, r3+8
	andi r6, r6, 15
	bne r6, r1, .LBB1_21
.LBB1_18:
	addi r3, r3, 24
	addi r4, r4, -1
	bne r4, r1, .LBB1_17
	jal r0, .LBB1_23
.LBB1_19:
	addi r1, r0, 0
	jal r0, .LBB1_23
.LBB1_20:
	stw r5+0, r1
	addi r1, r0, 3
	stb r5+8, r1
	ldw r1, r4+12
	add r3, r1, r3
	jal r0, .LBB1_22
.LBB1_21:
	ldw r1, r3+16
	ldw r4, r3+20
	stw r5+4, r4
	stw r5+0, r1
	ldbu r1, r3+9
	stb r5+8, r1
.LBB1_22:
	ldw r1, r3+0
	ldw r4, r3+4
	stw r5+16, r4
	stw r5+12, r1
	ldbu r1, r3+8
	stb r5+20, r1
	addi r1, r0, 1
.LBB1_23:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB1_24:
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	add r3, r11, r0
	jal r31, luaG_runerror
.Lfunc_end1:
	.size	luaH_next, .Lfunc_end1-luaH_next
                                        # -- End function
	.hidden	luaH_resize                     # -- Begin function luaH_resize
	.globl	luaH_resize
	.p2align	2
	.type	luaH_resize,@function
luaH_resize:                            # @luaH_resize
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
	add r12, r4, r0
	add r11, r3, r0
	ldb r1, r4+6
	addi r18, r0, -1
	ble r1, r18, .LBB2_2
.LBB2_1:
	ldw r19, r12+8
	jal r0, .LBB2_4
.LBB2_2:
	ldw r3, r12+8
	addi r4, r3, -1
	and r4, r3, r4
	addi r5, r0, 0
	beq r4, r5, .LBB2_1
.LBB2_3:
	srli r4, r3, 1
	or  r3, r4, r3
	srli r4, r3, 2
	or  r3, r4, r3
	srli r4, r3, 4
	or  r3, r4, r3
	srli r4, r3, 8
	or  r3, r4, r3
	srli r4, r3, 16
	or  r3, r4, r3
	addi r19, r3, 1
.LBB2_4:
	stw r12+8, r19
	andi r1, r1, 127
	stb r12+6, r1
	addi r13, r0, 0
	beq r6, r13, .LBB2_10
.LBB2_5:
	add r3, r6, r0
	jal r31, luaO_ceillog2
	add r15, r1, r0
	addi r1, r0, 30
	bgt r15, r1, .LBB2_44
.LBB2_6:
	addi r1, r0, 28
	bgeu r15, r1, .LBB2_44
.LBB2_7:
	addi r1, r0, 1
	sll r16, r1, r15
	addi r20, r0, 24
	sll r4, r20, r15
	add r3, r11, r0
	add r5, r13, r0
	jal r31, luaM_malloc_
	add r17, r1, r0
	addi r1, fp, -120
	stw r1+16, r17
	addi r3, r17, 8
	addi r4, r0, 16
	add r5, r16, r0
.LBB2_8:
	stw r3+4, r13
	sth r3+0, r4
	addi r5, r5, -1
	addi r3, r3, 24
	bne r5, r13, .LBB2_8
.LBB2_9:
	stb r1+7, r15
	mul r1, r16, r20
	add r24, r17, r1
	jal r0, .LBB2_11
.LBB2_10:
	lui r17, %hi(dummynode_)
	addi r17, r17, %lo(dummynode_)
	addi r1, fp, -120
	stw r1+16, r17
	stb r1+7, r13
	add r24, r13, r0
	add r15, r13, r0
.LBB2_11:
	addi r20, fp, -120
	stw r20+20, r24
	bgeu r14, r19, .LBB2_28
.LBB2_12:
	stw r12+8, r14
	ldbu r1, r12+7
	ldw r22, r12+16
	ldw r23, r12+20
	stb r12+7, r15
	stw r12+16, r17
	stw r12+20, r24
	stw fp+-124, r1
	stb r20+7, r1
	stw r20+16, r22
	stw r20+20, r23
	addi r24, r0, 12
	addi r25, r0, 32
	addi r15, fp, -88
	addi r26, r0, 3
	addi r27, r0, 24
	lui r28, %hi(absentkey)
	addi r28, r28, %lo(absentkey)
	add r16, r14, r0
	jal r0, .LBB2_15
.LBB2_13:
	ldw r3, r17+0
	ldw r4, r17+4
	stw r1+4, r4
	stw r1+0, r3
	ldbu r3, r17+8
	stb r1+8, r3
.LBB2_14:
	beq r16, r19, .LBB2_27
.LBB2_15:
	add r3, r16, r0
	ldw r1, r12+12
	mul r4, r16, r24
	add r17, r1, r4
	ldbu r1, r17+8
	andi r1, r1, 15
	addi r16, r16, 1
	beq r1, r13, .LBB2_14
.LBB2_16:
	ldw r4, r12+8
	add r1, r17, r0
	bltu r3, r4, .LBB2_24
.LBB2_17:
	ldb r1, r12+6
	bgt r1, r18, .LBB2_20
.LBB2_18:
	sub r1, r13, r4
	and r1, r3, r1
	bgeu r1, r4, .LBB2_20
.LBB2_19:
	stw r12+8, r16
	add r1, r17, r0
	ldbu r3, r1+8
	bne r3, r25, .LBB2_13
	jal r0, .LBB2_26
.LBB2_20:
	ldw r21, r12+16
	ldbu r1, r12+7
	sll r1, r18, r1
	xor r1, r1, r18
	ori  r4, r1, 1
	add r3, r16, r0
	jal r31, __umodsi3
	mul r1, r1, r27
	add r1, r21, r1
	jal r0, .LBB2_22
.LBB2_21:
	ldw r3, r1+12
	mul r4, r3, r27
	add r1, r1, r4
	beq r3, r13, .LBB2_25
.LBB2_22:
	ldbu r3, r1+9
	bne r3, r26, .LBB2_21
.LBB2_23:
	ldw r3, r1+16
	bne r3, r16, .LBB2_21
.LBB2_24:
	ldbu r3, r1+8
	bne r3, r25, .LBB2_13
	jal r0, .LBB2_26
.LBB2_25:
	add r1, r28, r0
	ldbu r3, r1+8
	bne r3, r25, .LBB2_13
.LBB2_26:
	stw r15+0, r16
	stb r15+8, r26
	add r3, r11, r0
	add r4, r12, r0
	add r5, r15, r0
	add r6, r17, r0
	jal r31, luaH_newkey
	jal r0, .LBB2_14
.LBB2_27:
	stw r12+8, r19
	ldbu r15, r12+7
	ldw r17, r12+16
	ldw r24, r12+20
	ldw r1, fp+-124
	stb r12+7, r1
	stw r12+16, r22
	stw r12+20, r23
	stb r20+7, r15
	stw r20+16, r17
	stw r20+20, r24
.LBB2_28:
	ldw r4, r12+12
	addi r1, r0, 12
	mul r18, r19, r1
	mul r6, r14, r1
	add r3, r11, r0
	add r5, r18, r0
	jal r31, luaM_realloc_
	beq r14, r13, .LBB2_30
.LBB2_29:
	beq r1, r13, .LBB2_43
.LBB2_30:
	ldbu r21, r12+7
	ldw r16, r12+16
	ldw r22, r12+20
	stb r12+7, r15
	stw r12+16, r17
	stw r12+20, r24
	stb r20+7, r21
	stw r20+16, r16
	stw r20+20, r22
	stw r12+12, r1
	stw r12+8, r14
	bleu r14, r19, .LBB2_33
.LBB2_31:
	sub r3, r14, r19
	add r1, r18, r1
	addi r1, r1, 8
	addi r4, r0, 16
.LBB2_32:
	stb r1+0, r4
	addi r3, r3, -1
	addi r1, r1, 12
	bne r3, r13, .LBB2_32
.LBB2_33:
	addi r1, r0, 31
	bne r21, r1, .LBB2_37
.LBB2_34:
	beq r22, r13, .LBB2_36
.LBB2_35:
	addi r1, r0, 24
	sll r5, r1, r21
	add r3, r11, r0
	add r4, r16, r0
	jal r31, luaM_free_
.LBB2_36:
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
	addi sp, sp, 136
	jalr r0, r31, 0
.LBB2_37:
	addi r1, r0, 1
	sll r3, r1, r21
	sgt r1, r3, r1
	sub r1, r13, r1
	xori r3, r3, 1
	and r1, r3, r1
	xori r17, r1, 1
	addi r14, fp, -88
	addi r18, r0, 32
	add r15, r16, r0
	jal r0, .LBB2_40
.LBB2_38:
	ldw r3, r15+0
	ldw r4, r15+4
	stw r1+4, r4
	stw r1+0, r3
	ldbu r3, r15+8
	stb r1+8, r3
.LBB2_39:
	addi r17, r17, -1
	addi r15, r15, 24
	beq r17, r13, .LBB2_34
.LBB2_40:
	ldbu r1, r15+8
	andi r1, r1, 15
	beq r1, r13, .LBB2_39
.LBB2_41:
	ldw r1, r15+16
	ldw r3, r15+20
	stw r14+4, r3
	stw r14+0, r1
	ldbu r1, r15+9
	stb r14+8, r1
	add r3, r12, r0
	add r4, r14, r0
	jal r31, luaH_get
	ldbu r3, r1+8
	bne r3, r18, .LBB2_38
.LBB2_42:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r14, r0
	add r6, r15, r0
	jal r31, luaH_newkey
	jal r0, .LBB2_39
.LBB2_43:
	addi r4, fp, -120
	add r3, r11, r0
	jal r31, freehash
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, luaD_throw
.LBB2_44:
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	add r3, r11, r0
	jal r31, luaG_runerror
.Lfunc_end2:
	.size	luaH_resize, .Lfunc_end2-luaH_resize
                                        # -- End function
	.hidden	luaH_setint                     # -- Begin function luaH_setint
	.globl	luaH_setint
	.p2align	2
	.type	luaH_setint,@function
luaH_setint:                            # @luaH_setint
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
	add r11, r5, r0
	ldw r5, r4+8
	addi r1, r11, -1
	bgeu r1, r5, .LBB3_2
.LBB3_1:
	ldw r5, r4+12
	addi r7, r0, 12
	mul r1, r1, r7
	add r1, r5, r1
	jal r0, .LBB3_11
.LBB3_2:
	ldb r8, r4+6
	addi r7, r0, -1
	bgt r8, r7, .LBB3_5
.LBB3_3:
	addi r8, r0, 0
	sub r8, r8, r5
	and r8, r1, r8
	bgeu r8, r5, .LBB3_5
.LBB3_4:
	stw r4+8, r11
	jal r0, .LBB3_1
.LBB3_5:
	add r12, r3, r0
	add r13, r6, r0
	ldw r15, r4+16
	add r14, r4, r0
	ldbu r1, r4+7
	sll r1, r7, r1
	xor r1, r1, r7
	ori  r4, r1, 1
	add r3, r11, r0
	jal r31, __umodsi3
	addi r3, r0, 24
	mul r1, r1, r3
	add r1, r15, r1
	addi r5, r0, 3
	lui r4, %hi(absentkey)
	addi r4, r4, %lo(absentkey)
	addi r6, r0, 0
	jal r0, .LBB3_7
.LBB3_6:
	ldw r7, r1+12
	mul r8, r7, r3
	add r1, r1, r8
	beq r7, r6, .LBB3_9
.LBB3_7:
	ldbu r7, r1+9
	bne r7, r5, .LBB3_6
.LBB3_8:
	ldw r7, r1+16
	bne r7, r11, .LBB3_6
	jal r0, .LBB3_10
.LBB3_9:
	add r1, r4, r0
.LBB3_10:
	add r6, r13, r0
	add r4, r14, r0
	add r3, r12, r0
.LBB3_11:
	ldbu r5, r1+8
	addi r7, r0, 32
	bne r5, r7, .LBB3_13
.LBB3_12:
	addi r5, fp, -36
	stw r5+0, r11
	addi r1, r0, 3
	stb r5+8, r1
	jal r31, luaH_newkey
	jal r0, .LBB3_14
.LBB3_13:
	ldw r3, r6+0
	ldw r4, r6+4
	stw r1+4, r4
	stw r1+0, r3
	ldbu r3, r6+8
	stb r1+8, r3
.LBB3_14:
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
.Lfunc_end3:
	.size	luaH_setint, .Lfunc_end3-luaH_setint
                                        # -- End function
	.p2align	2                               # -- Begin function freehash
	.type	freehash,@function
freehash:                               # @freehash
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r4+20
	addi r5, r0, 0
	beq r1, r5, .LBB4_2
.LBB4_1:
	ldw r1, r4+16
	ldbu r4, r4+7
	addi r5, r0, 24
	sll r5, r5, r4
	add r4, r1, r0
	jal r31, luaM_free_
.LBB4_2:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	freehash, .Lfunc_end4-freehash
                                        # -- End function
	.hidden	luaH_resizearray                # -- Begin function luaH_resizearray
	.globl	luaH_resizearray
	.p2align	2
	.type	luaH_resizearray,@function
luaH_resizearray:                       # @luaH_resizearray
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r4+20
	addi r6, r0, 0
	beq r1, r6, .LBB5_2
.LBB5_1:
	ldbu r1, r4+7
	addi r6, r0, 1
	sll r6, r6, r1
.LBB5_2:
	jal r31, luaH_resize
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	luaH_resizearray, .Lfunc_end5-luaH_resizearray
                                        # -- End function
	.hidden	luaH_new                        # -- Begin function luaH_new
	.globl	luaH_new
	.p2align	2
	.type	luaH_new,@function
luaH_new:                               # @luaH_new
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r4, r0, 5
	addi r5, r0, 32
	jal r31, luaC_newobj
	addi r3, r0, 0
	stw r1+24, r3
	addi r4, r0, 63
	sth r1+6, r4
	stw r1+12, r3
	stw r1+8, r3
	lui r4, %hi(dummynode_)
	addi r4, r4, %lo(dummynode_)
	stw r1+16, r4
	stw r1+20, r3
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	luaH_new, .Lfunc_end6-luaH_new
                                        # -- End function
	.hidden	luaH_free                       # -- Begin function luaH_free
	.globl	luaH_free
	.p2align	2
	.type	luaH_free,@function
luaH_free:                              # @luaH_free
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
	ldw r1, r4+20
	addi r13, r0, 0
	beq r1, r13, .LBB7_2
.LBB7_1:
	ldw r4, r11+16
	ldbu r1, r11+7
	addi r3, r0, 24
	sll r5, r3, r1
	add r3, r12, r0
	jal r31, luaM_free_
.LBB7_2:
	ldw r4, r11+12
	ldb r1, r11+6
	addi r3, r0, -1
	ble r1, r3, .LBB7_4
.LBB7_3:
	ldw r1, r11+8
	jal r0, .LBB7_6
.LBB7_4:
	ldw r1, r11+8
	addi r3, r1, -1
	and r3, r1, r3
	beq r3, r13, .LBB7_3
.LBB7_5:
	srli r3, r1, 1
	or  r1, r3, r1
	srli r3, r1, 2
	or  r1, r3, r1
	srli r3, r1, 4
	or  r1, r3, r1
	srli r3, r1, 8
	or  r1, r3, r1
	srli r3, r1, 16
	or  r1, r3, r1
	addi r1, r1, 1
.LBB7_6:
	addi r3, r0, 12
	mul r5, r1, r3
	add r3, r12, r0
	jal r31, luaM_free_
	addi r5, r0, 32
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
.Lfunc_end7:
	.size	luaH_free, .Lfunc_end7-luaH_free
                                        # -- End function
	.hidden	luaH_getint                     # -- Begin function luaH_getint
	.globl	luaH_getint
	.p2align	2
	.type	luaH_getint,@function
luaH_getint:                            # @luaH_getint
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
	ldw r4, r3+8
	addi r1, r11, -1
	bgeu r1, r4, .LBB8_2
.LBB8_1:
	ldw r3, r3+12
	addi r4, r0, 12
	mul r1, r1, r4
	add r1, r3, r1
	jal r0, .LBB8_10
.LBB8_2:
	ldb r6, r3+6
	addi r5, r0, -1
	bgt r6, r5, .LBB8_5
.LBB8_3:
	addi r6, r0, 0
	sub r6, r6, r4
	and r6, r1, r6
	bgeu r6, r4, .LBB8_5
.LBB8_4:
	stw r3+8, r11
	jal r0, .LBB8_1
.LBB8_5:
	ldw r12, r3+16
	ldbu r1, r3+7
	sll r1, r5, r1
	xor r1, r1, r5
	ori  r4, r1, 1
	add r3, r11, r0
	jal r31, __umodsi3
	addi r3, r0, 24
	mul r1, r1, r3
	add r1, r12, r1
	addi r5, r0, 3
	lui r4, %hi(absentkey)
	addi r4, r4, %lo(absentkey)
	addi r6, r0, 0
	jal r0, .LBB8_7
.LBB8_6:
	ldw r7, r1+12
	mul r8, r7, r3
	add r1, r1, r8
	beq r7, r6, .LBB8_9
.LBB8_7:
	ldbu r7, r1+9
	bne r7, r5, .LBB8_6
.LBB8_8:
	ldw r7, r1+16
	bne r7, r11, .LBB8_6
	jal r0, .LBB8_10
.LBB8_9:
	add r1, r4, r0
.LBB8_10:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	luaH_getint, .Lfunc_end8-luaH_getint
                                        # -- End function
	.hidden	luaH_getshortstr                # -- Begin function luaH_getshortstr
	.globl	luaH_getshortstr
	.p2align	2
	.type	luaH_getshortstr,@function
luaH_getshortstr:                       # @luaH_getshortstr
# %bb.0:
	ldw r1, r3+16
	ldw r5, r4+8
	ldbu r3, r3+7
	addi r6, r0, -1
	sll r3, r6, r3
	xor r3, r3, r6
	and r5, r5, r3
	addi r3, r0, 24
	mul r5, r5, r3
	add r1, r1, r5
	addi r6, r0, 68
	lui r5, %hi(absentkey)
	addi r5, r5, %lo(absentkey)
	addi r7, r0, 0
	jal r0, .LBB9_2
.LBB9_1:
	ldw r8, r1+12
	mul r9, r8, r3
	add r1, r1, r9
	beq r8, r7, .LBB9_4
.LBB9_2:
	ldbu r8, r1+9
	bne r8, r6, .LBB9_1
.LBB9_3:
	ldw r8, r1+16
	bne r8, r4, .LBB9_1
	jal r0, .LBB9_5
.LBB9_4:
	add r1, r5, r0
.LBB9_5:
	jalr r0, r31, 0
.Lfunc_end9:
	.size	luaH_getshortstr, .Lfunc_end9-luaH_getshortstr
                                        # -- End function
	.hidden	luaH_getstr                     # -- Begin function luaH_getstr
	.globl	luaH_getstr
	.p2align	2
	.type	luaH_getstr,@function
luaH_getstr:                            # @luaH_getstr
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldbu r5, r4+4
	addi r1, r0, 4
	bne r5, r1, .LBB10_5
.LBB10_1:
	ldw r1, r3+16
	ldw r5, r4+8
	ldbu r3, r3+7
	addi r6, r0, -1
	sll r3, r6, r3
	xor r3, r3, r6
	and r5, r5, r3
	addi r3, r0, 24
	mul r5, r5, r3
	add r1, r1, r5
	addi r6, r0, 68
	lui r5, %hi(absentkey)
	addi r5, r5, %lo(absentkey)
	addi r7, r0, 0
	jal r0, .LBB10_3
.LBB10_2:
	ldw r8, r1+12
	mul r9, r8, r3
	add r1, r1, r9
	beq r8, r7, .LBB10_6
.LBB10_3:
	ldbu r8, r1+9
	bne r8, r6, .LBB10_2
.LBB10_4:
	ldw r8, r1+16
	bne r8, r4, .LBB10_2
	jal r0, .LBB10_7
.LBB10_5:
	addi r1, fp, -16
	stw r1+0, r4
	ori  r4, r5, 64
	stb r1+8, r4
	addi r5, r0, 0
	add r4, r1, r0
	jal r31, getgeneric
	jal r0, .LBB10_7
.LBB10_6:
	add r1, r5, r0
.LBB10_7:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end10:
	.size	luaH_getstr, .Lfunc_end10-luaH_getstr
                                        # -- End function
	.p2align	2                               # -- Begin function getgeneric
	.type	getgeneric,@function
getgeneric:                             # @getgeneric
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
	add r11, r5, r0
	add r12, r4, r0
	jal r31, mainpositionTV
	add r13, r1, r0
	addi r14, r0, 22
	addi r15, r0, 84
	addi r16, r0, 0
	lui r17, %hi(.LJTI11_0)
	addi r17, r17, %lo(.LJTI11_0)
	addi r19, r0, 24
	lui r18, %hi(absentkey)
	addi r18, r18, %lo(absentkey)
	addi r20, r0, 11
	jal r0, .LBB11_4
.LBB11_1:
	ldw r1, r12+0
	ldw r3, r13+16
	sne r1, r1, r3
.LBB11_2:
	beq r1, r16, .LBB11_14
.LBB11_3:
	ldw r1, r13+12
	mul r3, r1, r19
	add r13, r13, r3
	beq r1, r16, .LBB11_13
.LBB11_4:
	ldbu r3, r12+8
	ldbu r1, r13+9
	beq r3, r1, .LBB11_8
.LBB11_5:
	andi r3, r3, 64
	beq r3, r16, .LBB11_3
.LBB11_6:
	beq r11, r16, .LBB11_3
.LBB11_7:
	bne r1, r20, .LBB11_3
.LBB11_8:
	bgtu r1, r14, .LBB11_11
.LBB11_9:
	slli r1, r1, 2
	add r1, r17, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB11_10:
	ldw r5, r12+4
	ldw r4, r12+0
	ldw r7, r13+20
	ldw r6, r13+16
	feq.d r1, r4, r6
	xori r1, r1, 1
	jal r0, .LBB11_2
.LBB11_11:
	bne r1, r15, .LBB11_1
.LBB11_12:
	ldw r3, r12+0
	ldw r4, r13+16
	jal r31, luaS_eqlngstr
	seq r1, r1, r16
	jal r0, .LBB11_2
.LBB11_13:
	add r13, r18, r0
.LBB11_14:
	add r1, r13, r0
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
.Lfunc_end11:
	.size	getgeneric, .Lfunc_end11-getgeneric
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI11_0:
	.word	.LBB11_14
	.word	.LBB11_14
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_14
	.word	.LBB11_1
	.word	.LBB11_10
	.word	.LBB11_1
	.word	.LBB11_1
	.word	.LBB11_1
                                        # -- End function
	.text
	.hidden	luaH_get                        # -- Begin function luaH_get
	.globl	luaH_get
	.p2align	2
	.type	luaH_get,@function
luaH_get:                               # @luaH_get
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
	ldbu r1, r4+8
	andi r5, r1, 63
	addi r1, r0, 19
	bgtu r5, r1, .LBB12_21
.LBB12_1:
	lui r1, %hi(absentkey)
	addi r1, r1, %lo(absentkey)
	slli r5, r5, 2
	lui r6, %hi(.LJTI12_0)
	addi r6, r6, %lo(.LJTI12_0)
	add r5, r6, r5
	ldw r5, r5+0
	jalr r0, r5, 0
.LBB12_2:
	ldw r11, r4+0
	ldw r5, r3+8
	addi r4, r11, -1
	bgeu r4, r5, .LBB12_12
.LBB12_3:
	ldw r1, r3+12
	addi r3, r0, 12
	mul r3, r4, r3
	add r1, r1, r3
	jal r0, .LBB12_22
.LBB12_4:
	ldw r4, r4+0
	ldw r5, r3+16
	ldw r6, r4+8
	ldbu r3, r3+7
	addi r7, r0, -1
	sll r3, r7, r3
	xor r3, r3, r7
	and r6, r6, r3
	addi r3, r0, 24
	mul r6, r6, r3
	add r5, r5, r6
	addi r6, r0, 68
	addi r7, r0, 0
	jal r0, .LBB12_6
.LBB12_5:
	ldw r8, r5+12
	mul r9, r8, r3
	add r5, r5, r9
	beq r8, r7, .LBB12_22
.LBB12_6:
	ldbu r8, r5+9
	bne r8, r6, .LBB12_5
.LBB12_7:
	ldw r8, r5+16
	bne r8, r4, .LBB12_5
.LBB12_8:
	add r1, r5, r0
	jal r0, .LBB12_22
.LBB12_9:
	add r14, r3, r0
	ldw r1, r4+4
	add r15, r4, r0
	ldw r3, r4+0
	addi r13, fp, -32
	addi r11, r0, 0
	add r4, r1, r0
	add r5, r13, r0
	add r6, r11, r0
	jal r31, luaV_flttointeger
	add r12, r1, r0
	beq r1, r11, .LBB12_20
.LBB12_10:
	ldw r13, r13+0
	add r3, r14, r0
	ldw r4, r14+8
	addi r1, r13, -1
	bgeu r1, r4, .LBB12_23
.LBB12_11:
	ldw r4, r3+12
	addi r5, r0, 12
	mul r1, r1, r5
	add r1, r4, r1
	add r4, r15, r0
	bne r12, r11, .LBB12_22
	jal r0, .LBB12_21
.LBB12_12:
	ldb r7, r3+6
	addi r6, r0, -1
	bgt r7, r6, .LBB12_15
.LBB12_13:
	addi r7, r0, 0
	sub r7, r7, r5
	and r7, r4, r7
	bgeu r7, r5, .LBB12_15
.LBB12_14:
	stw r3+8, r11
	jal r0, .LBB12_3
.LBB12_15:
	ldw r13, r3+16
	ldbu r3, r3+7
	sll r3, r6, r3
	xor r3, r3, r6
	ori  r4, r3, 1
	add r3, r11, r0
	add r12, r1, r0
	jal r31, __umodsi3
	add r3, r1, r0
	add r1, r12, r0
	addi r4, r0, 24
	mul r3, r3, r4
	add r3, r13, r3
	addi r5, r0, 3
	addi r6, r0, 0
	jal r0, .LBB12_17
.LBB12_16:
	ldw r7, r3+12
	mul r8, r7, r4
	add r3, r3, r8
	beq r7, r6, .LBB12_22
.LBB12_17:
	ldbu r7, r3+9
	bne r7, r5, .LBB12_16
.LBB12_18:
	ldw r7, r3+16
	bne r7, r11, .LBB12_16
.LBB12_19:
	add r1, r3, r0
	jal r0, .LBB12_22
.LBB12_20:
                                        # implicit-def: $r1
	add r3, r14, r0
	add r4, r15, r0
	bne r12, r11, .LBB12_22
.LBB12_21:
	addi r5, r0, 0
	jal r31, getgeneric
.LBB12_22:
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
.LBB12_23:
	ldb r6, r3+6
	addi r5, r0, -1
	bgt r6, r5, .LBB12_26
.LBB12_24:
	sub r6, r11, r4
	and r6, r1, r6
	bgeu r6, r4, .LBB12_26
.LBB12_25:
	stw r3+8, r13
	jal r0, .LBB12_11
.LBB12_26:
	ldw r16, r3+16
	ldbu r1, r3+7
	sll r1, r5, r1
	xor r1, r1, r5
	ori  r4, r1, 1
	add r3, r13, r0
	jal r31, __umodsi3
	addi r3, r0, 24
	mul r1, r1, r3
	add r1, r16, r1
	addi r6, r0, 3
	lui r5, %hi(absentkey)
	addi r5, r5, %lo(absentkey)
	add r4, r15, r0
	jal r0, .LBB12_28
.LBB12_27:
	ldw r7, r1+12
	mul r8, r7, r3
	add r1, r1, r8
	beq r7, r11, .LBB12_30
.LBB12_28:
	ldbu r7, r1+9
	bne r7, r6, .LBB12_27
.LBB12_29:
	ldw r7, r1+16
	bne r7, r13, .LBB12_27
	jal r0, .LBB12_31
.LBB12_30:
	add r1, r5, r0
.LBB12_31:
	add r3, r14, r0
	bne r12, r11, .LBB12_22
	jal r0, .LBB12_21
.Lfunc_end12:
	.size	luaH_get, .Lfunc_end12-luaH_get
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI12_0:
	.word	.LBB12_22
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_2
	.word	.LBB12_4
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_21
	.word	.LBB12_9
                                        # -- End function
	.text
	.hidden	luaH_finishset                  # -- Begin function luaH_finishset
	.globl	luaH_finishset
	.p2align	2
	.type	luaH_finishset,@function
luaH_finishset:                         # @luaH_finishset
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldbu r1, r6+8
	addi r8, r0, 32
	bne r1, r8, .LBB13_2
.LBB13_1:
	add r6, r7, r0
	jal r31, luaH_newkey
	jal r0, .LBB13_3
.LBB13_2:
	ldw r1, r7+0
	ldw r3, r7+4
	stw r6+4, r3
	stw r6+0, r1
	ldbu r1, r7+8
	stb r6+8, r1
.LBB13_3:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end13:
	.size	luaH_finishset, .Lfunc_end13-luaH_finishset
                                        # -- End function
	.p2align	2                               # -- Begin function luaH_newkey
	.type	luaH_newkey,@function
luaH_newkey:                            # @luaH_newkey
# %bb.0:
	addi sp, sp, -232
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 232
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
	ldbu r1, r5+8
	andi r3, r1, 15
	addi r17, r0, 0
	beq r3, r17, .LBB14_66
.LBB14_1:
	add r11, r6, r0
	add r14, r5, r0
	add r13, r4, r0
	addi r3, r0, 19
	bne r1, r3, .LBB14_5
.LBB14_2:
	ldw r19, r14+4
	ldw r18, r14+0
	addi r16, fp, -204
	addi r15, r0, 0
	add r3, r18, r0
	add r4, r19, r0
	add r5, r16, r0
	add r6, r15, r0
	jal r31, luaV_flttointeger
	beq r1, r15, .LBB14_4
.LBB14_3:
	ldw r1, r16+0
	addi r14, fp, -216
	stw r14+0, r1
	addi r1, r0, 3
	stb r14+8, r1
	jal r0, .LBB14_5
.LBB14_4:
	feq.d r1, r18, r18
	and r1, r1, r1
	beq r1, r15, .LBB14_67
.LBB14_5:
	ldbu r1, r11+8
	andi r1, r1, 15
	beq r1, r17, .LBB14_65
.LBB14_6:
	add r3, r13, r0
	add r4, r14, r0
	jal r31, mainpositionTV
	add r15, r1, r0
	ldbu r1, r1+8
	andi r1, r1, 15
	beq r1, r17, .LBB14_19
.LBB14_7:
	ldw r1, r13+20
	add r18, r17, r0
	beq r1, r17, .LBB14_12
.LBB14_8:
	ldw r3, r13+16
	addi r1, r1, -24
	addi r18, r0, 0
.LBB14_9:
	addi r4, r1, 24
	bleu r4, r3, .LBB14_12
.LBB14_10:
	stw r13+20, r1
	ldbu r4, r1+9
	addi r1, r1, -24
	bne r4, r18, .LBB14_9
.LBB14_11:
	addi r18, r1, 24
.LBB14_12:
	addi r16, fp, -204
	beq r18, r17, .LBB14_20
.LBB14_13:
	ldw r1, r15+16
	ldw r3, r15+20
	stw r16+4, r3
	stw r16+0, r1
	ldbu r1, r15+9
	stb r16+8, r1
	add r3, r13, r0
	add r4, r16, r0
	jal r31, mainpositionTV
	beq r1, r15, .LBB14_24
.LBB14_14:
	addi r4, r0, 24
.LBB14_15:
	add r3, r1, r0
	ldw r1, r1+12
	mul r1, r1, r4
	add r1, r3, r1
	bne r1, r15, .LBB14_15
.LBB14_16:
	sub r1, r18, r3
	srai r4, r1, 3
	lui r1, 699051
	addi r1, r1, -1365
	mul r4, r4, r1
	stw r3+12, r4
	ldw r3, r15+20
	stw r18+20, r3
	ldw r3, r15+16
	stw r18+16, r3
	ldw r3, r15+12
	stw r18+12, r3
	ldw r3, r15+8
	stw r18+8, r3
	ldw r3, r15+4
	stw r18+4, r3
	ldw r3, r15+0
	stw r18+0, r3
	ldw r4, r15+12
	addi r3, r0, 0
	beq r4, r3, .LBB14_18
.LBB14_17:
	sub r4, r15, r18
	srai r4, r4, 3
	mul r1, r4, r1
	ldw r4, r18+12
	add r1, r4, r1
	stw r18+12, r1
	stw r15+12, r3
.LBB14_18:
	addi r1, r0, 16
	stb r15+8, r1
	addi r1, r0, 0
	bne r18, r1, .LBB14_60
	jal r0, .LBB14_65
.LBB14_19:
	ldw r1, r13+20
	addi r3, r0, 0
	bne r1, r3, .LBB14_60
	jal r0, .LBB14_7
.LBB14_20:
	addi r1, r0, 0
	addi r19, r0, 128
	add r3, r1, r0
.LBB14_21:
	add r4, r16, r3
	stw r4+0, r1
	addi r3, r3, 4
	bne r3, r19, .LBB14_21
.LBB14_22:
	ldb r4, r13+6
	addi r1, r0, -1
	ble r4, r1, .LBB14_27
.LBB14_23:
	ldw r3, r13+8
	jal r0, .LBB14_29
.LBB14_24:
	ldw r1, r15+12
	addi r3, r0, 0
	beq r1, r3, .LBB14_26
.LBB14_25:
	addi r3, r0, 24
	mul r1, r1, r3
	add r1, r15, r1
	sub r1, r1, r18
	srai r1, r1, 3
	lui r3, 699051
	addi r3, r3, -1365
	mul r1, r1, r3
	stw r18+12, r1
.LBB14_26:
	sub r1, r18, r15
	srai r1, r1, 3
	lui r3, 699051
	addi r3, r3, -1365
	mul r1, r1, r3
	stw r15+12, r1
	add r15, r18, r0
	addi r1, r0, 0
	bne r18, r1, .LBB14_60
	jal r0, .LBB14_65
.LBB14_27:
	ldw r3, r13+8
	addi r5, r3, -1
	and r5, r3, r5
	addi r6, r0, 0
	beq r5, r6, .LBB14_23
.LBB14_28:
	srli r5, r3, 1
	or  r3, r5, r3
	srli r5, r3, 2
	or  r3, r5, r3
	srli r5, r3, 4
	or  r3, r5, r3
	srli r5, r3, 8
	or  r3, r5, r3
	srli r5, r3, 16
	or  r3, r5, r3
	addi r3, r3, 1
.LBB14_29:
	stw fp+-224, r12
	stw fp+-220, r11
	stw r13+8, r3
	andi r4, r4, 127
	stb r13+6, r4
	addi r4, r0, 1
	addi r5, r0, 0
	addi r20, r0, 32
	addi r6, r0, 12
	add r7, r5, r0
	add r9, r4, r0
	add r21, r5, r0
	add r8, r4, r0
.LBB14_30:
	add r10, r8, r0
	bleu r8, r3, .LBB14_33
.LBB14_31:
	add r10, r3, r0
	bleu r9, r3, .LBB14_33
.LBB14_32:
	add r10, r5, r0
	bne r10, r5, .LBB14_39
	jal r0, .LBB14_40
.LBB14_33:
	bleu r9, r10, .LBB14_35
.LBB14_34:
	add r10, r5, r0
	jal r0, .LBB14_38
.LBB14_35:
	ldw r22, r13+12
	addi r11, r9, 1
	addi r10, r10, 1
	xor r12, r11, r10
	sgtu r11, r11, r10
	sub r11, r5, r11
	and r11, r12, r11
	xor r11, r10, r11
	sub r12, r11, r9
	mul r9, r9, r6
	add r9, r22, r9
	addi r9, r9, -4
	add r10, r5, r0
.LBB14_36:
	ldbu r22, r9+0
	andi r22, r22, 15
	sne r22, r22, r5
	add r10, r10, r22
	addi r12, r12, -1
	addi r9, r9, 12
	bne r12, r5, .LBB14_36
.LBB14_37:
	add r9, r11, r0
.LBB14_38:
	slli r11, r7, 2
	add r11, r16, r11
	ldw r12, r11+0
	add r12, r12, r10
	stw r11+0, r12
	add r21, r10, r21
	add r10, r4, r0
	beq r10, r5, .LBB14_40
.LBB14_39:
	addi r7, r7, 1
	slli r8, r8, 1
	bne r7, r20, .LBB14_30
.LBB14_40:
	ldbu r3, r13+7
	sll r25, r1, r3
	addi r1, r0, 24
	mul r1, r25, r1
	addi r3, r0, -16
	sub r27, r3, r1
	addi r28, r0, 0
	addi r12, r0, 1
	addi r26, r0, 3
	lui r1, 961195
	addi r23, r1, -1366
	addi r24, r1, -1365
	add r22, r28, r0
	add r11, r28, r0
	jal r0, .LBB14_44
.LBB14_41:
	add r11, r1, r11
.LBB14_42:
	addi r22, r22, 1
.LBB14_43:
	addi r1, r25, 1
	sltu r3, r1, r25
	addi r27, r27, -24
	add r25, r1, r0
	beq r3, r12, .LBB14_48
.LBB14_44:
	ldw r1, r13+16
	add r1, r1, r27
	ldbu r3, r1+0
	andi r3, r3, 15
	beq r3, r28, .LBB14_43
.LBB14_45:
	ldbu r3, r1+1
	bne r3, r26, .LBB14_42
.LBB14_46:
	ldw r3, r1+8
	add r4, r3, r23
	add r1, r28, r0
	bltu r4, r24, .LBB14_41
.LBB14_47:
	jal r31, luaO_ceillog2
	slli r1, r1, 2
	add r1, r16, r1
	ldw r3, r1+0
	addi r3, r3, 1
	stw r1+0, r3
	add r1, r12, r0
	jal r0, .LBB14_41
.LBB14_48:
	add r25, r11, r21
	ldbu r1, r14+8
	bne r1, r26, .LBB14_51
.LBB14_49:
	ldw r3, r14+0
	add r1, r3, r23
	ldw r11, fp+-220
	ldw r12, fp+-224
	bgeu r1, r24, .LBB14_52
.LBB14_50:
	addi r1, r0, 0
	jal r0, .LBB14_53
.LBB14_51:
	ldw r11, fp+-220
	ldw r12, fp+-224
	jal r0, .LBB14_54
.LBB14_52:
	jal r31, luaO_ceillog2
	slli r1, r1, 2
	add r1, r16, r1
	ldw r3, r1+0
	addi r3, r3, 1
	stw r1+0, r3
	addi r1, r0, 1
.LBB14_53:
	add r25, r1, r25
.LBB14_54:
	addi r3, r0, 1
	addi r4, r0, 0
	add r6, r4, r0
	add r5, r4, r0
	add r1, r4, r0
	add r7, r4, r0
.LBB14_55:
	srli r8, r3, 1
	bleu r25, r8, .LBB14_57
.LBB14_56:
	add r9, r16, r6
	ldw r9, r9+0
	add r7, r9, r7
	sgtu r8, r7, r8
	xor r9, r7, r1
	sub r8, r4, r8
	and r9, r9, r8
	xor r1, r1, r9
	xor r9, r3, r5
	and r8, r9, r8
	xor r5, r5, r8
	slli r3, r3, 1
	addi r6, r6, 4
	bne r6, r19, .LBB14_55
.LBB14_57:
	add r3, r21, r22
	sub r1, r3, r1
	addi r6, r1, 1
	add r3, r12, r0
	add r4, r13, r0
	jal r31, luaH_resize
	add r3, r13, r0
	add r4, r14, r0
	jal r31, luaH_get
	ldbu r3, r1+8
	bne r3, r20, .LBB14_59
.LBB14_58:
	add r3, r12, r0
	add r4, r13, r0
	add r5, r14, r0
	add r6, r11, r0
	jal r31, luaH_newkey
	addi r1, r0, 0
	bne r18, r1, .LBB14_60
	jal r0, .LBB14_65
.LBB14_59:
	ldw r3, r11+0
	ldw r4, r11+4
	stw r1+4, r4
	stw r1+0, r3
	ldbu r3, r11+8
	stb r1+8, r3
	addi r1, r0, 0
	beq r18, r1, .LBB14_65
.LBB14_60:
	ldw r1, r14+0
	ldw r3, r14+4
	stw r15+20, r3
	stw r15+16, r1
	ldbu r1, r14+8
	stb r15+9, r1
	ldbu r1, r14+8
	andi r1, r1, 64
	beq r1, r17, .LBB14_64
.LBB14_61:
	ldbu r1, r13+5
	andi r1, r1, 32
	beq r1, r17, .LBB14_64
.LBB14_62:
	ldw r1, r14+0
	ldbu r1, r1+5
	andi r1, r1, 24
	beq r1, r17, .LBB14_64
.LBB14_63:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, luaC_barrierback_
.LBB14_64:
	ldw r1, r11+0
	ldw r3, r11+4
	stw r15+4, r3
	stw r15+0, r1
	ldbu r1, r11+8
	stb r15+8, r1
.LBB14_65:
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
	addi sp, sp, 232
	jalr r0, r31, 0
.LBB14_66:
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	add r3, r12, r0
	jal r31, luaG_runerror
.LBB14_67:
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	add r3, r12, r0
	jal r31, luaG_runerror
.Lfunc_end14:
	.size	luaH_newkey, .Lfunc_end14-luaH_newkey
                                        # -- End function
	.hidden	luaH_set                        # -- Begin function luaH_set
	.globl	luaH_set
	.p2align	2
	.type	luaH_set,@function
luaH_set:                               # @luaH_set
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
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	add r14, r3, r0
	add r3, r4, r0
	add r4, r5, r0
	jal r31, luaH_get
	ldbu r3, r1+8
	addi r4, r0, 32
	bne r3, r4, .LBB15_2
.LBB15_1:
	add r3, r14, r0
	add r4, r13, r0
	add r5, r12, r0
	add r6, r11, r0
	jal r31, luaH_newkey
	jal r0, .LBB15_3
.LBB15_2:
	ldw r3, r11+0
	ldw r4, r11+4
	stw r1+4, r4
	stw r1+0, r3
	ldbu r3, r11+8
	stb r1+8, r3
.LBB15_3:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end15:
	.size	luaH_set, .Lfunc_end15-luaH_set
                                        # -- End function
	.hidden	luaH_getn                       # -- Begin function luaH_getn
	.globl	luaH_getn
	.p2align	2
	.type	luaH_getn,@function
luaH_getn:                              # @luaH_getn
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
	stw fp+-56, r24
	stw fp+-60, lr
	add r11, r3, r0
	ldw r1, r3+8
	addi r3, r0, 0
	beq r1, r3, .LBB16_2
.LBB16_1:
	ldw r4, r11+12
	addi r5, r0, 12
	mul r6, r1, r5
	add r7, r6, r4
	ldbu r7, r7+-4
	andi r7, r7, 15
	beq r7, r3, .LBB16_9
.LBB16_2:
	ldb r14, r11+6
	addi r15, r0, -1
	add r13, r1, r0
	bgt r14, r15, .LBB16_6
.LBB16_3:
	addi r3, r1, -1
	and r4, r1, r3
	addi r3, r0, 0
	add r13, r1, r0
	beq r4, r3, .LBB16_6
.LBB16_4:
	ldw r4, r11+12
	addi r5, r0, 12
	mul r6, r1, r5
	add r6, r4, r6
	ldbu r6, r6+8
	andi r6, r6, 15
	beq r6, r3, .LBB16_18
.LBB16_5:
	srli r6, r1, 1
	or  r6, r6, r1
	srli r7, r6, 2
	or  r6, r7, r6
	srli r7, r6, 4
	or  r6, r7, r6
	srli r7, r6, 8
	or  r6, r7, r6
	srli r7, r6, 16
	or  r6, r7, r6
	addi r13, r6, 1
	mul r6, r6, r5
	add r6, r6, r4
	ldbu r6, r6+8
	andi r6, r6, 15
	beq r6, r3, .LBB16_23
.LBB16_6:
	ldw r3, r11+20
	addi r16, r0, 0
	beq r3, r16, .LBB16_65
.LBB16_7:
	bgeu r13, r1, .LBB16_15
.LBB16_8:
	ldw r1, r11+12
	addi r3, r0, 12
	mul r3, r13, r3
	add r1, r1, r3
	jal r0, .LBB16_28
.LBB16_9:
	addi r7, r0, 1
	add r12, r3, r0
	beq r1, r7, .LBB16_50
.LBB16_10:
	add r6, r4, r6
	ldbu r6, r6+-16
	andi r8, r6, 15
	addi r6, r0, 0
	beq r8, r6, .LBB16_48
.LBB16_11:
	addi r12, r1, -1
	ldb r3, r11+6
	blt r3, r6, .LBB16_13
.LBB16_12:
	addi r4, r1, -1
	and r4, r1, r4
	bne r4, r6, .LBB16_66
.LBB16_13:
	addi r1, r1, -2
	and r1, r12, r1
	beq r1, r6, .LBB16_66
.LBB16_14:
	stw r11+8, r12
	ori  r1, r3, 128
	stb r11+6, r1
	jal r0, .LBB16_66
.LBB16_15:
	addi r12, r13, 1
	bgt r14, r15, .LBB16_19
.LBB16_16:
	sub r3, r16, r1
	and r3, r13, r3
	bgeu r3, r1, .LBB16_19
.LBB16_17:
	stw r11+8, r12
	jal r0, .LBB16_8
.LBB16_18:
	add r12, r1, r0
	jal r0, .LBB16_66
.LBB16_19:
	lui r17, %hi(absentkey)
	addi r17, r17, %lo(absentkey)
	ldw r18, r11+16
	ldbu r1, r11+7
	sll r1, r15, r1
	xor r1, r1, r15
	ori  r4, r1, 1
	add r3, r12, r0
	jal r31, __umodsi3
	addi r3, r0, 24
	mul r1, r1, r3
	add r1, r18, r1
	addi r4, r0, 3
	jal r0, .LBB16_21
.LBB16_20:
	ldw r5, r1+12
	mul r6, r5, r3
	add r1, r1, r6
	beq r5, r16, .LBB16_27
.LBB16_21:
	ldbu r5, r1+9
	bne r5, r4, .LBB16_20
.LBB16_22:
	ldw r5, r1+16
	bne r5, r12, .LBB16_20
	jal r0, .LBB16_28
.LBB16_23:
	sub r6, r13, r1
	addi r7, r0, 2
	bltu r6, r7, .LBB16_47
.LBB16_24:
	addi r6, r0, 1
	add r12, r1, r0
.LBB16_25:
	add r1, r13, r12
	srli r1, r1, 1
	mul r7, r1, r5
	add r7, r4, r7
	ldbu r7, r7+-4
	andi r7, r7, 15
	seq r7, r7, r3
	xor r8, r1, r13
	sub r7, r3, r7
	and r8, r8, r7
	xor r13, r13, r8
	xor r8, r12, r1
	and r7, r8, r7
	xor r12, r1, r7
	sub r1, r13, r12
	bgtu r1, r6, .LBB16_25
.LBB16_26:
	stw r11+8, r12
	jal r0, .LBB16_66
.LBB16_27:
	add r1, r17, r0
.LBB16_28:
	ldbu r1, r1+8
	andi r1, r1, 15
	beq r1, r16, .LBB16_65
.LBB16_29:
	addi r17, r0, 1
	sgtu r1, r13, r17
	sub r1, r16, r1
	xori r3, r13, 1
	and r1, r3, r1
	xori r12, r1, 1
	lui r1, 262144
	addi r21, r1, -1
	addi r18, r0, 12
	addi r19, r0, 24
	addi r20, r0, 3
	lui r22, %hi(absentkey)
	addi r22, r22, %lo(absentkey)
	jal r0, .LBB16_33
.LBB16_30:
	stw r11+8, r12
.LBB16_31:
	ldw r3, r11+12
	mul r1, r1, r18
	add r1, r3, r1
.LBB16_32:
	ldbu r1, r1+8
	andi r1, r1, 15
	beq r1, r16, .LBB16_63
.LBB16_33:
	add r13, r12, r0
	bgtu r12, r21, .LBB16_42
.LBB16_34:
	slli r12, r13, 1
	ldw r3, r11+8
	addi r1, r12, -1
	bltu r1, r3, .LBB16_31
.LBB16_35:
	bgt r14, r15, .LBB16_37
.LBB16_36:
	sub r4, r16, r3
	and r4, r1, r4
	bltu r4, r3, .LBB16_30
.LBB16_37:
	ldw r23, r11+16
	ldbu r1, r11+7
	sll r1, r15, r1
	xor r1, r1, r15
	ori  r4, r1, 1
	add r3, r12, r0
	jal r31, __umodsi3
	mul r1, r1, r19
	add r1, r23, r1
	jal r0, .LBB16_39
.LBB16_38:
	ldw r3, r1+12
	mul r4, r3, r19
	add r1, r1, r4
	beq r3, r16, .LBB16_41
.LBB16_39:
	ldbu r3, r1+9
	bne r3, r20, .LBB16_38
.LBB16_40:
	ldw r3, r1+16
	bne r3, r12, .LBB16_38
	jal r0, .LBB16_32
.LBB16_41:
	add r1, r22, r0
	jal r0, .LBB16_32
.LBB16_42:
	ldw r1, r11+8
	lui r21, 524288
	addi r3, r21, -2
	bleu r1, r3, .LBB16_44
.LBB16_43:
	ldw r1, r11+12
	addi r1, r1, -24
	jal r0, .LBB16_62
.LBB16_44:
	bgt r14, r15, .LBB16_57
.LBB16_45:
	sub r4, r16, r1
	and r3, r4, r3
	bgeu r3, r1, .LBB16_57
.LBB16_46:
	addi r1, r21, -1
	stw r11+8, r1
	jal r0, .LBB16_43
.LBB16_47:
	add r12, r1, r0
	stw r11+8, r1
	jal r0, .LBB16_66
.LBB16_48:
	add r12, r6, r0
	add r8, r1, r0
.LBB16_49:
	add r9, r8, r12
	srli r9, r9, 1
	mul r10, r9, r5
	add r10, r4, r10
	ldbu r10, r10+-4
	andi r10, r10, 15
	seq r10, r10, r6
	xor r13, r9, r8
	sub r10, r6, r10
	and r13, r13, r10
	xor r8, r8, r13
	xor r12, r12, r9
	and r10, r12, r10
	xor r12, r9, r10
	sub r9, r8, r12
	bgtu r9, r7, .LBB16_49
.LBB16_50:
	ldb r4, r11+6
	addi r5, r1, -1
	and r5, r1, r5
	blt r4, r3, .LBB16_52
.LBB16_51:
	addi r3, r0, 0
	bne r5, r3, .LBB16_66
.LBB16_52:
	addi r3, r0, -1
	bgt r4, r3, .LBB16_55
.LBB16_53:
	addi r3, r0, 0
	beq r5, r3, .LBB16_55
.LBB16_54:
	srli r3, r1, 1
	or  r1, r3, r1
	srli r3, r1, 2
	or  r1, r3, r1
	srli r3, r1, 4
	or  r1, r3, r1
	srli r3, r1, 8
	or  r1, r3, r1
	srli r3, r1, 16
	or  r1, r3, r1
	addi r1, r1, 1
.LBB16_55:
	srli r1, r1, 1
	bleu r12, r1, .LBB16_66
.LBB16_56:
	stw r11+8, r12
	ori  r1, r4, 128
	stb r11+6, r1
	jal r0, .LBB16_66
.LBB16_57:
	ldw r22, r11+16
	ldbu r1, r11+7
	sll r1, r15, r1
	xor r1, r1, r15
	ori  r4, r1, 1
	addi r12, r21, -1
	add r3, r12, r0
	jal r31, __umodsi3
	mul r1, r1, r19
	add r1, r22, r1
	lui r3, %hi(absentkey)
	addi r3, r3, %lo(absentkey)
	jal r0, .LBB16_59
.LBB16_58:
	ldw r4, r1+12
	mul r5, r4, r19
	add r1, r1, r5
	beq r4, r16, .LBB16_61
.LBB16_59:
	ldbu r4, r1+9
	bne r4, r20, .LBB16_58
.LBB16_60:
	ldw r4, r1+16
	bne r4, r12, .LBB16_58
	jal r0, .LBB16_62
.LBB16_61:
	add r1, r3, r0
.LBB16_62:
	ldbu r1, r1+8
	andi r1, r1, 15
	addi r12, r21, -1
	bne r1, r16, .LBB16_66
.LBB16_63:
	sub r1, r12, r13
	addi r3, r0, 2
	bltu r1, r3, .LBB16_65
.LBB16_64:
	add r21, r12, r0
	ldw r23, r11+8
	lui r22, %hi(absentkey)
	addi r22, r22, %lo(absentkey)
	add r12, r13, r0
	jal r0, .LBB16_70
.LBB16_65:
	add r12, r13, r0
.LBB16_66:
	add r1, r12, r0
	ldw lr, fp+-60
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
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB16_67:
	stw r11+8, r13
	add r23, r13, r0
.LBB16_68:
	ldw r3, r11+12
	mul r1, r1, r18
	add r1, r3, r1
.LBB16_69:
	ldbu r1, r1+8
	andi r1, r1, 15
	seq r1, r1, r16
	xor r3, r13, r21
	sub r1, r16, r1
	and r3, r3, r1
	xor r21, r21, r3
	xor r3, r12, r13
	and r1, r3, r1
	xor r12, r13, r1
	sub r1, r21, r12
	bleu r1, r17, .LBB16_66
.LBB16_70:
	add r1, r21, r12
	srli r13, r1, 1
	addi r1, r13, -1
	bltu r1, r23, .LBB16_68
.LBB16_71:
	bgt r14, r15, .LBB16_73
.LBB16_72:
	sub r3, r16, r23
	and r3, r1, r3
	bltu r3, r23, .LBB16_67
.LBB16_73:
	ldw r24, r11+16
	ldbu r1, r11+7
	sll r1, r15, r1
	xor r1, r1, r15
	ori  r4, r1, 1
	add r3, r13, r0
	jal r31, __umodsi3
	mul r1, r1, r19
	add r1, r24, r1
	jal r0, .LBB16_75
.LBB16_74:
	ldw r3, r1+12
	mul r4, r3, r19
	add r1, r1, r4
	beq r3, r16, .LBB16_77
.LBB16_75:
	ldbu r3, r1+9
	bne r3, r20, .LBB16_74
.LBB16_76:
	ldw r3, r1+16
	bne r3, r13, .LBB16_74
	jal r0, .LBB16_69
.LBB16_77:
	add r1, r22, r0
	jal r0, .LBB16_69
.Lfunc_end16:
	.size	luaH_getn, .Lfunc_end16-luaH_getn
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function mainpositionTV
.LCPI17_0:
	.quad	0x41e0000000000000              # double 2147483648
.LCPI17_1:
	.quad	0xc1e0000000000000              # double -2147483648
	.text
	.p2align	2
	.type	mainpositionTV,@function
mainpositionTV:                         # @mainpositionTV
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
	ldbu r1, r4+8
	andi r1, r1, 63
	addi r1, r1, -1
	addi r5, r0, 21
	bgtu r1, r5, .LBB17_2
.LBB17_1:
	slli r1, r1, 2
	lui r5, %hi(.LJTI17_0)
	addi r5, r5, %lo(.LJTI17_0)
	add r1, r5, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB17_2:
	ldw r1, r4+0
	ldw r11, r3+16
	ldbu r3, r3+7
	addi r4, r0, -1
	sll r3, r4, r3
	xor r3, r3, r4
	ori  r4, r3, 1
	add r3, r1, r0
	jal r31, __umodsi3
.LBB17_3:
	addi r3, r0, 24
	mul r1, r1, r3
	add r1, r11, r1
.LBB17_4:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB17_5:
	ldw r1, r3+16
	ldbu r3, r3+7
	addi r4, r0, 0
	sne r3, r3, r4
	addi r4, r0, 24
	mul r3, r3, r4
	add r1, r1, r3
	jal r0, .LBB17_4
.LBB17_6:
	ldw r1, r4+4
	ldw r4, r4+0
	add r13, r3, r0
	ldw r12, r3+16
	addi r11, fp, -20
	add r3, r4, r0
	add r4, r1, r0
	add r5, r11, r0
	jal r31, frexp
	add r4, r1, r0
	add r5, r2, r0
	lui r1, %hi(.LCPI17_0)
	addi r1, r1, %lo(.LCPI17_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	lui r1, %hi(.LCPI17_1)
	addi r1, r1, %lo(.LCPI17_1)
	ldw r9, r1+4
	ldw r8, r1+0
	flt.d r3, r4, r8
	addi r1, r0, 0
	bne r3, r1, .LBB17_9
.LBB17_7:
	fle.d r3, r6, r4
	addi r6, r0, 0
	bne r3, r6, .LBB17_9
.LBB17_8:
	fcvt.w.d r1, r4
	ldw r3, r11+0
	add r1, r3, r1
	srai r3, r1, 31
	xor r1, r3, r1
.LBB17_9:
	ldbu r3, r13+7
	addi r4, r0, -1
	sll r3, r4, r3
	xor r3, r3, r4
	ori  r3, r3, 1
	rem r1, r1, r3
	addi r3, r0, 24
	mul r1, r1, r3
	add r1, r12, r1
	jal r0, .LBB17_4
.LBB17_10:
	ldw r1, r4+0
	ldw r11, r3+16
	add r12, r3, r0
	add r3, r1, r0
	jal r31, luaS_hashlongstr
	ldbu r3, r12+7
	addi r4, r0, -1
	sll r3, r4, r3
	xor r3, r3, r4
	and r1, r1, r3
	jal r0, .LBB17_3
.LBB17_11:
	ldw r1, r3+16
	jal r0, .LBB17_4
.LBB17_12:
	ldw r1, r4+0
	ldw r4, r3+16
	ldw r1, r1+8
	ldbu r3, r3+7
	addi r5, r0, -1
	sll r3, r5, r3
	xor r3, r3, r5
	and r1, r1, r3
	addi r3, r0, 24
	mul r1, r1, r3
	add r1, r4, r1
	jal r0, .LBB17_4
.Lfunc_end17:
	.size	mainpositionTV, .Lfunc_end17-mainpositionTV
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI17_0:
	.word	.LBB17_11
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_12
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_2
	.word	.LBB17_5
	.word	.LBB17_2
	.word	.LBB17_6
	.word	.LBB17_10
	.word	.LBB17_2
	.word	.LBB17_2
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"invalid key to 'next'"
	.size	.L.str, 22

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"table overflow"
	.size	.L.str.1, 15

	.type	dummynode_,@object              # @dummynode_
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
dummynode_:
	.zero	8
	.byte	16                              # 0x10
	.byte	0                               # 0x0
	.zero	2
	.word	0                               # 0x0
	.zero	8
	.size	dummynode_, 24

	.type	absentkey,@object               # @absentkey
	.p2align	2, 0x0
absentkey:
	.zero	8
	.byte	32                              # 0x20
	.zero	3
	.size	absentkey, 12

	.type	.L.str.4,@object                # @.str.4
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.4:
	.asciz	"table index is nil"
	.size	.L.str.4, 19

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"table index is NaN"
	.size	.L.str.5, 19

	.hidden	luaM_realloc_
	.hidden	luaD_throw
	.hidden	luaC_newobj
	.hidden	luaM_free_
	.hidden	luaV_flttointeger
	.hidden	luaG_runerror
	.hidden	luaO_ceillog2
	.hidden	luaM_malloc_
	.hidden	luaS_hashlongstr
	.hidden	luaS_eqlngstr
	.hidden	luaC_barrierback_
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
