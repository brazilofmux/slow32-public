	.file	"lgc.c"
	.text
	.hidden	luaC_barrier_                   # -- Begin function luaC_barrier_
	.globl	luaC_barrier_
	.p2align	2
	.type	luaC_barrier_,@function
luaC_barrier_:                          # @luaC_barrier_
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
	ldw r3, r3+16
	ldbu r1, r3+65
	addi r4, r0, 2
	bgtu r1, r4, .LBB0_3
.LBB0_1:
	add r12, r5, r0
	add r4, r5, r0
	jal r31, reallymarkobject
	ldbu r1, r11+5
	andi r1, r1, 6
	addi r3, r0, 0
	beq r1, r3, .LBB0_5
.LBB0_2:
	ldbu r1, r12+5
	andi r1, r1, 248
	ori  r1, r1, 2
	stb r12+5, r1
	jal r0, .LBB0_5
.LBB0_3:
	ldbu r1, r3+66
	addi r4, r0, 0
	bne r1, r4, .LBB0_5
.LBB0_4:
	ldbu r1, r11+5
	andi r1, r1, 199
	ldbu r3, r3+64
	andi r3, r3, 24
	or  r1, r3, r1
	stb r11+5, r1
.LBB0_5:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaC_barrier_, .Lfunc_end0-luaC_barrier_
                                        # -- End function
	.p2align	2                               # -- Begin function reallymarkobject
	.type	reallymarkobject,@function
reallymarkobject:                       # @reallymarkobject
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	addi r1, r0, 9
	addi r6, r0, -57
	addi r5, r0, 0
.LBB1_1:
	ldbu r7, r4+4
	bne r7, r1, .LBB1_4
.LBB1_2:
	ldw r7, r4+8
	addi r8, r4, 12
	seq r8, r7, r8
	ldbu r9, r4+5
	and r9, r9, r6
	ori  r10, r9, 32
	xor r10, r10, r9
	sub r8, r5, r8
	and r8, r10, r8
	xor r8, r9, r8
	stb r4+5, r8
	ldbu r4, r7+8
	andi r4, r4, 64
	beq r4, r5, .LBB1_18
.LBB1_3:
	ldw r4, r7+0
	ldbu r7, r4+5
	andi r7, r7, 24
	bne r7, r5, .LBB1_1
	jal r0, .LBB1_18
.LBB1_4:
	addi r1, r7, -4
	addi r6, r0, 34
	bgtu r1, r6, .LBB1_18
.LBB1_5:
	slli r1, r1, 2
	lui r6, %hi(.LJTI1_0)
	addi r6, r6, %lo(.LJTI1_0)
	add r1, r6, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB1_6:
	ldbu r5, r4+4
	addi r1, r0, 28
	addi r5, r5, -5
	slli r5, r5, 2
	lui r6, %hi(.LJTI1_1)
	addi r6, r6, %lo(.LJTI1_1)
	add r5, r6, r5
	ldw r5, r5+0
	jalr r0, r5, 0
.LBB1_7:
	addi r1, r0, 8
	jal r0, .LBB1_16
.LBB1_8:
	ldhu r1, r4+6
	bne r1, r5, .LBB1_6
.LBB1_9:
	ldw r1, r4+12
	beq r1, r5, .LBB1_12
.LBB1_10:
	ldbu r6, r1+5
	andi r6, r6, 24
	beq r6, r5, .LBB1_12
.LBB1_11:
	add r11, r4, r0
	add r4, r1, r0
	jal r31, reallymarkobject
	add r4, r11, r0
.LBB1_12:
	ldbu r1, r4+5
	andi r1, r1, 199
	ori  r1, r1, 32
	jal r0, .LBB1_17
.LBB1_13:
	addi r1, r0, 16
	jal r0, .LBB1_16
.LBB1_14:
	addi r1, r0, 80
	jal r0, .LBB1_16
.LBB1_15:
	addi r1, r0, 40
.LBB1_16:
	add r1, r4, r1
	ldw r5, r3+88
	stw r1+0, r5
	stw r3+88, r4
	ldbu r1, r4+5
	andi r1, r1, 199
.LBB1_17:
	stb r4+5, r1
.LBB1_18:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB1_19:
.Lfunc_end1:
	.size	reallymarkobject, .Lfunc_end1-reallymarkobject
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI1_0:
	.word	.LBB1_12
	.word	.LBB1_6
	.word	.LBB1_6
	.word	.LBB1_8
	.word	.LBB1_6
	.word	.LBB1_18
	.word	.LBB1_6
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_12
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_18
	.word	.LBB1_6
.LJTI1_1:
	.word	.LBB1_16
	.word	.LBB1_7
	.word	.LBB1_13
	.word	.LBB1_15
	.word	.LBB1_19
	.word	.LBB1_14
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_19
	.word	.LBB1_7
                                        # -- End function
	.text
	.hidden	luaC_barrierback_               # -- Begin function luaC_barrierback_
	.globl	luaC_barrierback_
	.p2align	2
	.type	luaC_barrierback_,@function
luaC_barrierback_:                      # @luaC_barrierback_
# %bb.0:
	ldbu r1, r4+5
	andi r5, r1, 7
	addi r6, r0, 6
	bne r5, r6, .LBB2_2
.LBB2_1:
	addi r3, r0, 198
	jal r0, .LBB2_8
.LBB2_2:
	ldw r3, r3+16
	ldbu r6, r4+4
	addi r5, r0, 28
	addi r6, r6, -5
	slli r6, r6, 2
	lui r7, %hi(.LJTI2_0)
	addi r7, r7, %lo(.LJTI2_0)
	add r6, r7, r6
	ldw r6, r6+0
	jalr r0, r6, 0
.LBB2_3:
	addi r5, r0, 8
	jal r0, .LBB2_7
.LBB2_4:
	addi r5, r0, 16
	jal r0, .LBB2_7
.LBB2_5:
	addi r5, r0, 80
	jal r0, .LBB2_7
.LBB2_6:
	addi r5, r0, 40
.LBB2_7:
	add r5, r4, r5
	ldw r6, r3+92
	stw r5+0, r6
	stw r3+92, r4
	addi r3, r0, 199
.LBB2_8:
	and r3, r1, r3
	andi r5, r1, 6
	addi r6, r0, 0
	seq r5, r5, r6
	andi r1, r1, 192
	ori  r1, r1, 5
	sub r5, r6, r5
	xor r3, r3, r1
	and r3, r3, r5
	xor r1, r1, r3
	stb r4+5, r1
	jalr r0, r31, 0
.LBB2_9:
.Lfunc_end2:
	.size	luaC_barrierback_, .Lfunc_end2-luaC_barrierback_
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI2_0:
	.word	.LBB2_7
	.word	.LBB2_3
	.word	.LBB2_4
	.word	.LBB2_6
	.word	.LBB2_9
	.word	.LBB2_5
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_9
	.word	.LBB2_3
                                        # -- End function
	.text
	.hidden	luaC_fix                        # -- Begin function luaC_fix
	.globl	luaC_fix
	.p2align	2
	.type	luaC_fix,@function
luaC_fix:                               # @luaC_fix
# %bb.0:
	ldw r1, r3+16
	ldbu r3, r4+5
	andi r3, r3, 192
	ori  r3, r3, 4
	stb r4+5, r3
	ldw r3, r4+0
	stw r1+76, r3
	ldw r3, r1+112
	stw r4+0, r3
	stw r1+112, r4
	jalr r0, r31, 0
.Lfunc_end3:
	.size	luaC_fix, .Lfunc_end3-luaC_fix
                                        # -- End function
	.hidden	luaC_newobjdt                   # -- Begin function luaC_newobjdt
	.globl	luaC_newobjdt
	.p2align	2
	.type	luaC_newobjdt,@function
luaC_newobjdt:                          # @luaC_newobjdt
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
	add r1, r5, r0
	add r12, r4, r0
	ldw r13, r3+16
	andi r5, r4, 15
	add r4, r1, r0
	jal r31, luaM_malloc_
	add r1, r1, r11
	ldbu r3, r13+64
	andi r3, r3, 24
	stb r1+5, r3
	stb r1+4, r12
	ldw r3, r13+76
	stw r1+0, r3
	stw r13+76, r1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	luaC_newobjdt, .Lfunc_end4-luaC_newobjdt
                                        # -- End function
	.hidden	luaC_newobj                     # -- Begin function luaC_newobj
	.globl	luaC_newobj
	.p2align	2
	.type	luaC_newobj,@function
luaC_newobj:                            # @luaC_newobj
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r1, r5, r0
	add r11, r4, r0
	ldw r12, r3+16
	andi r5, r4, 15
	add r4, r1, r0
	jal r31, luaM_malloc_
	ldbu r3, r12+64
	andi r3, r3, 24
	stb r1+5, r3
	stb r1+4, r11
	ldw r3, r12+76
	stw r1+0, r3
	stw r12+76, r1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	luaC_newobj, .Lfunc_end5-luaC_newobj
                                        # -- End function
	.hidden	luaC_checkfinalizer             # -- Begin function luaC_checkfinalizer
	.globl	luaC_checkfinalizer
	.p2align	2
	.type	luaC_checkfinalizer,@function
luaC_checkfinalizer:                    # @luaC_checkfinalizer
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
	addi r13, r0, 0
	beq r5, r13, .LBB6_15
.LBB6_1:
	ldbu r1, r4+5
	andi r1, r1, 64
	bne r1, r13, .LBB6_15
.LBB6_2:
	ldbu r1, r5+6
	andi r1, r1, 4
	bne r1, r13, .LBB6_15
.LBB6_3:
	ldw r11, r3+16
	ldw r1, r11+168
	addi r6, r0, 2
	add r14, r3, r0
	add r3, r5, r0
	add r12, r4, r0
	add r4, r6, r0
	add r5, r1, r0
	jal r31, luaT_gettm
	beq r1, r13, .LBB6_15
.LBB6_4:
	ldbu r1, r11+70
	andi r1, r1, 4
	bne r1, r13, .LBB6_15
.LBB6_5:
	add r5, r12, r0
	ldbu r1, r11+65
	addi r1, r1, -3
	andi r1, r1, 255
	addi r4, r0, 3
	bgtu r1, r4, .LBB6_8
.LBB6_6:
	ldbu r1, r5+5
	andi r1, r1, 199
	ldbu r4, r11+64
	andi r4, r4, 24
	or  r1, r4, r1
	stb r5+5, r1
	ldw r4, r11+80
	bne r4, r5, .LBB6_12
.LBB6_7:
	add r3, r14, r0
	jal r31, sweeptolive
	add r5, r12, r0
	stw r11+80, r1
	jal r0, .LBB6_12
.LBB6_8:
	ldw r1, r11+116
	beq r5, r1, .LBB6_16
.LBB6_9:
	ldw r1, r11+120
	beq r5, r1, .LBB6_17
.LBB6_10:
	ldw r1, r11+124
	beq r5, r1, .LBB6_18
.LBB6_11:
	ldw r1, r11+128
	beq r5, r1, .LBB6_19
.LBB6_12:
	addi r3, r11, 76
.LBB6_13:
	add r1, r3, r0
	ldw r3, r3+0
	bne r3, r5, .LBB6_13
.LBB6_14:
	ldw r3, r5+0
	stw r1+0, r3
	ldw r1, r11+84
	stw r5+0, r1
	stw r11+84, r5
	ldbu r1, r5+5
	ori  r1, r1, 64
	stb r5+5, r1
.LBB6_15:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB6_16:
	ldw r1, r5+0
	stw r11+116, r1
	ldw r1, r11+120
	bne r5, r1, .LBB6_10
.LBB6_17:
	ldw r1, r5+0
	stw r11+120, r1
	ldw r1, r11+124
	bne r5, r1, .LBB6_11
.LBB6_18:
	ldw r1, r5+0
	stw r11+124, r1
	ldw r1, r11+128
	bne r5, r1, .LBB6_12
.LBB6_19:
	ldw r1, r5+0
	stw r11+128, r1
	jal r0, .LBB6_12
.Lfunc_end6:
	.size	luaC_checkfinalizer, .Lfunc_end6-luaC_checkfinalizer
                                        # -- End function
	.p2align	2                               # -- Begin function sweeptolive
	.type	sweeptolive,@function
sweeptolive:                            # @sweeptolive
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
	addi r13, r0, 0
	add r14, r4, r0
	jal r0, .LBB7_4
.LBB7_1:
	andi r1, r1, 24
	andi r3, r3, 192
	or  r1, r3, r1
	stb r4+5, r1
	add r14, r4, r0
.LBB7_2:
	ldw r1, r14+0
	seq r1, r1, r13
	sub r1, r13, r1
	and r1, r14, r1
	xor r1, r14, r1
.LBB7_3:
	add r14, r1, r0
	bne r1, r11, .LBB7_7
.LBB7_4:
	ldw r4, r14+0
	add r1, r13, r0
	beq r4, r13, .LBB7_3
.LBB7_5:
	ldw r1, r12+16
	ldbu r1, r1+64
	xori r5, r1, 24
	ldbu r3, r4+5
	and r5, r3, r5
	beq r5, r13, .LBB7_1
.LBB7_6:
	ldw r1, r4+0
	stw r14+0, r1
	add r3, r12, r0
	jal r31, freeobj
	jal r0, .LBB7_2
.LBB7_7:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end7:
	.size	sweeptolive, .Lfunc_end7-sweeptolive
                                        # -- End function
	.hidden	luaC_changemode                 # -- Begin function luaC_changemode
	.globl	luaC_changemode
	.p2align	2
	.type	luaC_changemode,@function
luaC_changemode:                        # @luaC_changemode
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r11, r3+16
	ldbu r1, r11+66
	beq r4, r1, .LBB8_10
.LBB8_1:
	addi r1, r0, 1
	bne r4, r1, .LBB8_3
.LBB8_2:
	add r4, r11, r0
	jal r31, entergen
	jal r0, .LBB8_10
.LBB8_3:
	ldw r4, r11+76
	ldbu r1, r11+64
	andi r3, r1, 24
	addi r1, r0, 0
	beq r4, r1, .LBB8_5
.LBB8_4:
	ldbu r5, r4+5
	andi r5, r5, 192
	or  r5, r5, r3
	stb r4+5, r5
	ldw r4, r4+0
	bne r4, r1, .LBB8_4
.LBB8_5:
	stw r11+116, r1
	stw r11+120, r1
	stw r11+124, r1
	ldw r4, r11+84
	beq r4, r1, .LBB8_7
.LBB8_6:
	ldbu r5, r4+5
	andi r5, r5, 192
	or  r5, r5, r3
	stb r4+5, r5
	ldw r4, r4+0
	bne r4, r1, .LBB8_6
.LBB8_7:
	ldw r4, r11+108
	beq r4, r1, .LBB8_9
.LBB8_8:
	ldbu r5, r4+5
	andi r5, r5, 192
	or  r5, r5, r3
	stb r4+5, r5
	ldw r4, r4+0
	bne r4, r1, .LBB8_8
.LBB8_9:
	stw r11+132, r1
	stw r11+136, r1
	stw r11+140, r1
	addi r3, r0, 8
	stb r11+65, r3
	stb r11+66, r1
	stw r11+20, r1
.LBB8_10:
	addi r1, r0, 0
	stw r11+20, r1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	luaC_changemode, .Lfunc_end8-luaC_changemode
                                        # -- End function
	.p2align	2                               # -- Begin function entergen
	.type	entergen,@function
entergen:                               # @entergen
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
	ldw r13, r3+16
	ldbu r1, r13+65
	addi r14, r0, 8
	beq r1, r14, .LBB9_2
.LBB9_1:
	add r3, r12, r0
	jal r31, singlestep
	ldbu r1, r13+65
	bne r1, r14, .LBB9_1
.LBB9_2:
	ldw r13, r12+16
	ldbu r1, r13+65
	addi r14, r0, 0
	beq r1, r14, .LBB9_4
.LBB9_3:
	add r3, r12, r0
	jal r31, singlestep
	ldbu r1, r13+65
	bne r1, r14, .LBB9_3
.LBB9_4:
	add r3, r12, r0
	jal r31, atomic
	add r13, r1, r0
	add r3, r12, r0
	add r4, r11, r0
	jal r31, atomic2gen
	ldw r1, r11+8
	ldw r3, r11+12
	add r1, r3, r1
	lui r3, 335544
	addi r3, r3, 1311
	mulhu r1, r1, r3
	srli r1, r1, 5
	ldbu r3, r11+68
	mul r1, r1, r3
	sub r4, r14, r1
	add r3, r11, r0
	jal r31, luaE_setdebt
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
.Lfunc_end9:
	.size	entergen, .Lfunc_end9-entergen
                                        # -- End function
	.hidden	luaC_freeallobjects             # -- Begin function luaC_freeallobjects
	.globl	luaC_freeallobjects
	.p2align	2
	.type	luaC_freeallobjects,@function
luaC_freeallobjects:                    # @luaC_freeallobjects
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
	addi r1, r0, 4
	stb r13+70, r1
	ldbu r1, r13+66
	addi r12, r0, 0
	beq r1, r12, .LBB10_8
.LBB10_1:
	ldw r3, r13+76
	ldbu r1, r13+64
	andi r1, r1, 24
	beq r3, r12, .LBB10_3
.LBB10_2:
	ldbu r4, r3+5
	andi r4, r4, 192
	or  r4, r4, r1
	stb r3+5, r4
	ldw r3, r3+0
	bne r3, r12, .LBB10_2
.LBB10_3:
	stw r13+116, r12
	stw r13+120, r12
	stw r13+124, r12
	ldw r3, r13+84
	beq r3, r12, .LBB10_5
.LBB10_4:
	ldbu r4, r3+5
	andi r4, r4, 192
	or  r4, r4, r1
	stb r3+5, r4
	ldw r3, r3+0
	bne r3, r12, .LBB10_4
.LBB10_5:
	ldw r3, r13+108
	beq r3, r12, .LBB10_7
.LBB10_6:
	ldbu r4, r3+5
	andi r4, r4, 192
	or  r4, r4, r1
	stb r3+5, r4
	ldw r3, r3+0
	bne r3, r12, .LBB10_6
.LBB10_7:
	stw r13+132, r12
	stw r13+136, r12
	stw r13+140, r12
	addi r1, r0, 8
	stb r13+65, r1
	stb r13+66, r12
	stw r13+20, r12
.LBB10_8:
	stw r13+20, r12
	addi r14, r13, 108
	add r3, r14, r0
.LBB10_9:
	add r1, r3, r0
	ldw r3, r3+0
	bne r3, r12, .LBB10_9
.LBB10_10:
	ldw r4, r13+84
	ldw r3, r13+136
	bne r4, r3, .LBB10_12
	jal r0, .LBB10_15
.LBB10_11:
	ldw r4, r3+0
	stw r13+84, r4
	ldw r4, r1+0
	stw r3+0, r4
	stw r1+0, r3
	ldw r4, r13+84
	ldw r5, r13+136
	add r1, r3, r0
	beq r4, r5, .LBB10_15
.LBB10_12:
	add r3, r4, r0
	ldw r4, r13+132
	bne r3, r4, .LBB10_11
.LBB10_13:
	ldw r4, r3+0
	stw r13+132, r4
	jal r0, .LBB10_11
.LBB10_14:
	add r3, r11, r0
	jal r31, GCTM
.LBB10_15:
	ldw r1, r14+0
	bne r1, r12, .LBB10_14
.LBB10_16:
	ldw r4, r13+76
	ldw r14, r13+152
	beq r4, r14, .LBB10_18
.LBB10_17:
	ldw r15, r4+0
	add r3, r11, r0
	jal r31, freeobj
	add r4, r15, r0
	bne r15, r14, .LBB10_17
.LBB10_18:
	ldw r4, r13+112
	beq r4, r12, .LBB10_20
.LBB10_19:
	ldw r13, r4+0
	add r3, r11, r0
	jal r31, freeobj
	add r4, r13, r0
	bne r13, r12, .LBB10_19
.LBB10_20:
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
.Lfunc_end10:
	.size	luaC_freeallobjects, .Lfunc_end10-luaC_freeallobjects
                                        # -- End function
	.hidden	luaC_runtilstate                # -- Begin function luaC_runtilstate
	.globl	luaC_runtilstate
	.p2align	2
	.type	luaC_runtilstate,@function
luaC_runtilstate:                       # @luaC_runtilstate
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
	ldw r13, r3+16
	ldbu r1, r13+65
	srl r1, r4, r1
	andi r1, r1, 1
	addi r14, r0, 0
	bne r1, r14, .LBB11_3
.LBB11_1:
	add r11, r4, r0
	add r12, r3, r0
.LBB11_2:
	add r3, r12, r0
	jal r31, singlestep
	ldbu r1, r13+65
	srl r1, r11, r1
	andi r1, r1, 1
	beq r1, r14, .LBB11_2
.LBB11_3:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end11:
	.size	luaC_runtilstate, .Lfunc_end11-luaC_runtilstate
                                        # -- End function
	.p2align	2                               # -- Begin function singlestep
	.type	singlestep,@function
singlestep:                             # @singlestep
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
	add r12, r3, r0
	ldw r11, r3+16
	addi r14, r0, 1
	stb r11+67, r14
	ldbu r3, r11+65
	addi r13, r0, 0
	addi r1, r0, 8
	bgtu r3, r1, .LBB12_67
.LBB12_1:
	slli r3, r3, 2
	lui r4, %hi(.LJTI12_0)
	addi r4, r4, %lo(.LJTI12_0)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB12_2:
	ldw r1, r11+88
	addi r13, r0, 0
	beq r1, r13, .LBB12_62
.LBB12_3:
	add r3, r11, r0
	jal r31, propagatemark
	add r13, r1, r0
	jal r0, .LBB12_66
.LBB12_4:
	ldbu r1, r11+71
	addi r13, r0, 0
	bne r1, r13, .LBB12_7
.LBB12_5:
	ldw r3, r11+28
	ldw r1, r11+32
	srai r4, r1, 31
	srli r4, r4, 30
	add r4, r1, r4
	srai r4, r4, 2
	bge r3, r4, .LBB12_7
.LBB12_6:
	ldw r14, r11+12
	srli r3, r1, 31
	add r1, r1, r3
	srai r4, r1, 1
	add r3, r12, r0
	jal r31, luaS_resize
	ldw r1, r11+12
	sub r1, r1, r14
	ldw r3, r11+16
	add r1, r1, r3
	stw r11+16, r1
.LBB12_7:
	addi r1, r0, 7
	stb r11+65, r1
	jal r0, .LBB12_66
.LBB12_8:
	add r3, r12, r0
	jal r31, atomic
	add r13, r1, r0
	ldw r14, r12+16
	addi r1, r0, 3
	stb r14+65, r1
	addi r15, r14, 76
	addi r16, r0, 0
	add r17, r15, r0
	jal r0, .LBB12_12
.LBB12_9:
	andi r1, r1, 24
	andi r3, r3, 192
	or  r1, r3, r1
	stb r4+5, r1
	add r17, r4, r0
.LBB12_10:
	ldw r1, r17+0
	seq r1, r1, r16
	sub r1, r16, r1
	and r1, r17, r1
	xor r1, r17, r1
.LBB12_11:
	add r17, r1, r0
	bne r1, r15, .LBB12_15
.LBB12_12:
	ldw r4, r17+0
	add r1, r16, r0
	beq r4, r16, .LBB12_11
.LBB12_13:
	ldw r1, r12+16
	ldbu r1, r1+64
	xori r5, r1, 24
	ldbu r3, r4+5
	and r5, r3, r5
	beq r5, r16, .LBB12_9
.LBB12_14:
	ldw r1, r4+0
	stw r17+0, r1
	add r3, r12, r0
	jal r31, freeobj
	jal r0, .LBB12_10
.LBB12_15:
	stw r14+80, r1
	ldw r1, r11+8
	ldw r3, r11+12
	add r1, r3, r1
	stw r11+16, r1
	jal r0, .LBB12_66
.LBB12_16:
	ldw r3, r11+108
	addi r13, r0, 0
	beq r3, r13, .LBB12_18
.LBB12_17:
	ldbu r3, r11+71
	beq r3, r13, .LBB12_69
.LBB12_18:
	stb r11+65, r1
	jal r0, .LBB12_65
.LBB12_19:
	addi r13, r0, 0
	stw r11+92, r13
	stw r11+88, r13
	stw r11+100, r13
	stw r11+104, r13
	stw r11+96, r13
	ldw r4, r11+152
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB12_21
.LBB12_20:
	add r3, r11, r0
	jal r31, reallymarkobject
.LBB12_21:
	ldbu r1, r11+44
	andi r1, r1, 64
	beq r1, r13, .LBB12_24
.LBB12_22:
	ldw r4, r11+36
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB12_24
.LBB12_23:
	add r3, r11, r0
	jal r31, reallymarkobject
.LBB12_24:
	addi r12, r0, 260
	addi r15, r0, 296
	jal r0, .LBB12_26
.LBB12_25:
	addi r12, r12, 4
	beq r12, r15, .LBB12_29
.LBB12_26:
	add r1, r11, r12
	ldw r4, r1+0
	beq r4, r13, .LBB12_25
.LBB12_27:
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB12_25
.LBB12_28:
	add r3, r11, r0
	jal r31, reallymarkobject
	jal r0, .LBB12_25
.LBB12_29:
	ldw r12, r11+108
	bne r12, r13, .LBB12_32
.LBB12_30:
	stb r11+65, r13
	add r13, r14, r0
	jal r0, .LBB12_66
.LBB12_31:
	ldw r12, r12+0
	beq r12, r13, .LBB12_30
.LBB12_32:
	ldbu r1, r12+5
	andi r1, r1, 24
	beq r1, r13, .LBB12_31
.LBB12_33:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB12_31
.LBB12_34:
	ldw r15, r11+80
	addi r13, r0, 0
	beq r15, r13, .LBB12_61
.LBB12_35:
	ldw r14, r11+12
	ldw r4, r15+0
	beq r4, r13, .LBB12_59
.LBB12_36:
	ldbu r1, r11+64
	xori r16, r1, 24
	andi r17, r1, 24
	addi r18, r0, 0
	addi r19, r0, 99
	add r13, r18, r0
.LBB12_37:
	add r20, r13, r0
	ldbu r1, r4+5
	and r3, r1, r16
	beq r3, r18, .LBB12_39
.LBB12_38:
	ldw r1, r4+0
	stw r15+0, r1
	add r3, r12, r0
	jal r31, freeobj
	jal r0, .LBB12_40
.LBB12_39:
	andi r1, r1, 192
	or  r1, r1, r17
	stb r4+5, r1
	add r15, r4, r0
.LBB12_40:
	addi r13, r20, 1
	ldw r4, r15+0
	beq r4, r18, .LBB12_58
.LBB12_41:
	bltu r20, r19, .LBB12_37
	jal r0, .LBB12_58
.LBB12_42:
	ldw r15, r11+80
	addi r13, r0, 0
	beq r15, r13, .LBB12_63
.LBB12_43:
	ldw r14, r11+12
	ldw r4, r15+0
	beq r4, r13, .LBB12_59
.LBB12_44:
	ldbu r1, r11+64
	xori r16, r1, 24
	andi r17, r1, 24
	addi r18, r0, 0
	addi r19, r0, 99
	add r13, r18, r0
.LBB12_45:
	add r20, r13, r0
	ldbu r1, r4+5
	and r3, r1, r16
	beq r3, r18, .LBB12_47
.LBB12_46:
	ldw r1, r4+0
	stw r15+0, r1
	add r3, r12, r0
	jal r31, freeobj
	jal r0, .LBB12_48
.LBB12_47:
	andi r1, r1, 192
	or  r1, r1, r17
	stb r4+5, r1
	add r15, r4, r0
.LBB12_48:
	addi r13, r20, 1
	ldw r4, r15+0
	beq r4, r18, .LBB12_58
.LBB12_49:
	bltu r20, r19, .LBB12_45
	jal r0, .LBB12_58
.LBB12_50:
	ldw r15, r11+80
	addi r13, r0, 0
	beq r15, r13, .LBB12_68
.LBB12_51:
	ldw r14, r11+12
	ldw r4, r15+0
	beq r4, r13, .LBB12_59
.LBB12_52:
	ldbu r1, r11+64
	xori r16, r1, 24
	andi r17, r1, 24
	addi r18, r0, 0
	addi r19, r0, 99
	add r13, r18, r0
.LBB12_53:
	add r20, r13, r0
	ldbu r1, r4+5
	and r3, r1, r16
	beq r3, r18, .LBB12_55
.LBB12_54:
	ldw r1, r4+0
	stw r15+0, r1
	add r3, r12, r0
	jal r31, freeobj
	jal r0, .LBB12_56
.LBB12_55:
	andi r1, r1, 192
	or  r1, r1, r17
	stb r4+5, r1
	add r15, r4, r0
.LBB12_56:
	addi r13, r20, 1
	ldw r4, r15+0
	beq r4, r18, .LBB12_58
.LBB12_57:
	bltu r20, r19, .LBB12_53
.LBB12_58:
	addi r1, r0, 0
	seq r3, r4, r1
	sub r1, r1, r3
	and r1, r15, r1
	xor r1, r15, r1
	jal r0, .LBB12_60
.LBB12_59:
	add r1, r13, r0
.LBB12_60:
	stw r11+80, r1
	ldw r1, r11+12
	sub r1, r1, r14
	ldw r3, r11+16
	add r1, r1, r3
	stw r11+16, r1
	jal r0, .LBB12_66
.LBB12_61:
	addi r1, r11, 84
	addi r3, r0, 4
	jal r0, .LBB12_64
.LBB12_62:
	addi r1, r0, 1
	stb r11+65, r1
	jal r0, .LBB12_66
.LBB12_63:
	addi r1, r11, 108
	addi r3, r0, 5
.LBB12_64:
	stb r11+65, r3
	stw r11+80, r1
.LBB12_65:
	addi r13, r0, 0
.LBB12_66:
	addi r1, r0, 0
	stb r11+67, r1
.LBB12_67:
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
.LBB12_68:
	addi r1, r0, 6
	stb r11+65, r1
	addi r13, r0, 0
	stw r11+80, r13
	jal r0, .LBB12_66
.LBB12_69:
	stb r11+67, r13
	addi r15, r0, 10
	add r14, r13, r0
.LBB12_70:
	ldw r1, r11+108
	beq r1, r13, .LBB12_73
.LBB12_71:
	add r3, r12, r0
	jal r31, GCTM
	addi r14, r14, 1
	bne r14, r15, .LBB12_70
.LBB12_72:
	add r14, r15, r0
.LBB12_73:
	addi r1, r0, 50
	mul r13, r14, r1
	jal r0, .LBB12_66
.Lfunc_end12:
	.size	singlestep, .Lfunc_end12-singlestep
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI12_0:
	.word	.LBB12_2
	.word	.LBB12_8
	.word	.LBB12_67
	.word	.LBB12_34
	.word	.LBB12_42
	.word	.LBB12_50
	.word	.LBB12_4
	.word	.LBB12_16
	.word	.LBB12_19
                                        # -- End function
	.text
	.hidden	luaC_step                       # -- Begin function luaC_step
	.globl	luaC_step
	.p2align	2
	.type	luaC_step,@function
luaC_step:                              # @luaC_step
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
	ldw r11, r3+16
	ldbu r1, r11+70
	addi r14, r0, 0
	beq r1, r14, .LBB13_4
.LBB13_1:
	addi r4, r0, -2000
.LBB13_2:
	add r3, r11, r0
	jal r31, luaE_setdebt
.LBB13_3:
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
.LBB13_4:
	add r12, r3, r0
	ldbu r3, r11+66
	addi r1, r0, 1
	beq r3, r1, .LBB13_6
.LBB13_5:
	ldw r4, r11+20
	beq r4, r14, .LBB13_46
.LBB13_6:
	ldw r15, r11+20
	beq r15, r14, .LBB13_19
.LBB13_7:
	bne r3, r1, .LBB13_16
.LBB13_8:
	ldw r4, r11+76
	ldbu r1, r11+64
	andi r3, r1, 24
	addi r1, r0, 0
	beq r4, r1, .LBB13_10
.LBB13_9:
	ldbu r5, r4+5
	andi r5, r5, 192
	or  r5, r5, r3
	stb r4+5, r5
	ldw r4, r4+0
	bne r4, r1, .LBB13_9
.LBB13_10:
	stw r11+116, r1
	stw r11+120, r1
	stw r11+124, r1
	ldw r4, r11+84
	beq r4, r1, .LBB13_12
.LBB13_11:
	ldbu r5, r4+5
	andi r5, r5, 192
	or  r5, r5, r3
	stb r4+5, r5
	ldw r4, r4+0
	bne r4, r1, .LBB13_11
.LBB13_12:
	ldw r4, r11+108
	beq r4, r1, .LBB13_14
.LBB13_13:
	ldbu r5, r4+5
	andi r5, r5, 192
	or  r5, r5, r3
	stb r4+5, r5
	ldw r4, r4+0
	bne r4, r1, .LBB13_13
.LBB13_14:
	stw r11+132, r1
	stw r11+136, r1
	stw r11+140, r1
	addi r3, r0, 8
	stb r11+65, r3
	stb r11+66, r1
	stw r11+20, r1
	jal r0, .LBB13_16
.LBB13_15:
	add r3, r12, r0
	jal r31, singlestep
.LBB13_16:
	ldbu r1, r11+65
	bne r1, r14, .LBB13_15
.LBB13_17:
	add r3, r12, r0
	jal r31, atomic
	add r13, r1, r0
	srli r1, r15, 3
	add r1, r1, r15
	bgeu r13, r1, .LBB13_29
.LBB13_18:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, atomic2gen
	ldw r1, r11+8
	ldw r3, r11+12
	add r1, r3, r1
	lui r3, 335544
	addi r3, r3, 1311
	mulhu r1, r1, r3
	srli r1, r1, 5
	ldbu r3, r11+68
	mul r1, r1, r3
	addi r3, r0, 0
	sub r4, r3, r1
	jal r0, .LBB13_2
.LBB13_19:
	ldw r14, r11+16
	lui r3, 335544
	addi r15, r3, 1311
	ldw r3, r11+12
	blt r3, r1, .LBB13_39
.LBB13_20:
	mulhu r1, r14, r15
	srli r1, r1, 5
	ldbu r4, r11+69
	mul r1, r4, r1
	slli r13, r1, 2
	ldw r1, r11+8
	add r1, r1, r3
	add r3, r13, r14
	bleu r1, r3, .LBB13_39
.LBB13_21:
	ldw r3, r11+76
	ldbu r1, r11+64
	andi r1, r1, 24
	addi r16, r0, 0
	beq r3, r16, .LBB13_23
.LBB13_22:
	ldbu r4, r3+5
	andi r4, r4, 192
	or  r4, r4, r1
	stb r3+5, r4
	ldw r3, r3+0
	bne r3, r16, .LBB13_22
.LBB13_23:
	stw r11+116, r16
	stw r11+120, r16
	stw r11+124, r16
	ldw r3, r11+84
	beq r3, r16, .LBB13_25
.LBB13_24:
	ldbu r4, r3+5
	andi r4, r4, 192
	or  r4, r4, r1
	stb r3+5, r4
	ldw r3, r3+0
	bne r3, r16, .LBB13_24
.LBB13_25:
	ldw r3, r11+108
	beq r3, r16, .LBB13_27
.LBB13_26:
	ldbu r4, r3+5
	andi r4, r4, 192
	or  r4, r4, r1
	stb r3+5, r4
	ldw r3, r3+0
	bne r3, r16, .LBB13_26
.LBB13_27:
	stw r11+132, r16
	stw r11+136, r16
	stw r11+140, r16
	addi r1, r0, 8
	stb r11+65, r1
	stb r11+66, r16
	stw r11+20, r16
	add r3, r12, r0
	add r4, r11, r0
	jal r31, entergen
	ldw r3, r11+8
	ldw r4, r11+12
	add r17, r4, r3
	srli r3, r13, 1
	add r3, r3, r14
	bltu r17, r3, .LBB13_3
.LBB13_28:
	stw r11+20, r1
	ldbu r1, r11+72
	slli r14, r1, 2
	ldw r1, r11+16
	mulhu r1, r1, r15
	srli r12, r1, 5
	lui r1, 524288
	addi r13, r1, -1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, __udivsi3
	sltu r1, r14, r1
	mul r3, r12, r14
	xor r3, r3, r13
	sub r1, r16, r1
	and r1, r3, r1
	xor r1, r1, r13
	sub r1, r17, r1
	slt r3, r1, r16
	sub r3, r16, r3
	and r4, r1, r3
	jal r0, .LBB13_2
.LBB13_29:
	ldw r1, r11+8
	ldw r3, r11+12
	add r1, r3, r1
	stw r11+16, r1
	ldw r15, r12+16
	addi r1, r0, 3
	stb r15+65, r1
	addi r16, r15, 76
	add r17, r16, r0
	jal r0, .LBB13_33
.LBB13_30:
	andi r1, r1, 24
	andi r3, r3, 192
	or  r1, r3, r1
	stb r4+5, r1
	add r17, r4, r0
.LBB13_31:
	ldw r1, r17+0
	seq r1, r1, r14
	sub r1, r14, r1
	and r1, r17, r1
	xor r1, r17, r1
.LBB13_32:
	add r17, r1, r0
	bne r1, r16, .LBB13_36
.LBB13_33:
	ldw r4, r17+0
	add r1, r14, r0
	beq r4, r14, .LBB13_32
.LBB13_34:
	ldw r1, r12+16
	ldbu r1, r1+64
	xori r5, r1, 24
	ldbu r3, r4+5
	and r5, r3, r5
	beq r5, r14, .LBB13_30
.LBB13_35:
	ldw r1, r4+0
	stw r17+0, r1
	add r3, r12, r0
	jal r31, freeobj
	jal r0, .LBB13_31
.LBB13_36:
	stw r15+80, r1
	ldw r14, r12+16
	ldbu r1, r14+65
	addi r15, r0, 8
	beq r1, r15, .LBB13_38
.LBB13_37:
	add r3, r12, r0
	jal r31, singlestep
	ldbu r1, r14+65
	bne r1, r15, .LBB13_37
.LBB13_38:
	ldbu r1, r11+72
	slli r15, r1, 2
	ldw r1, r11+16
	lui r3, 335544
	addi r3, r3, 1311
	mulhu r1, r1, r3
	srli r12, r1, 5
	lui r1, 524288
	addi r14, r1, -1
	add r3, r14, r0
	add r4, r12, r0
	jal r31, __udivsi3
	sltu r1, r15, r1
	mul r3, r12, r15
	xor r3, r3, r14
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	xor r1, r1, r14
	ldw r3, r11+8
	ldw r5, r11+12
	add r3, r5, r3
	sub r1, r3, r1
	slt r3, r1, r4
	sub r3, r4, r3
	and r4, r1, r3
	add r3, r11, r0
	jal r31, luaE_setdebt
	stw r11+20, r13
	jal r0, .LBB13_3
.LBB13_39:
	ldw r13, r11+128
	addi r16, r0, 0
	beq r13, r16, .LBB13_49
.LBB13_40:
	ldw r17, r11+124
	beq r13, r17, .LBB13_48
.LBB13_41:
	addi r18, r0, 3
	jal r0, .LBB13_43
.LBB13_42:
	ldw r13, r13+0
	beq r13, r17, .LBB13_48
.LBB13_43:
	ldbu r1, r13+5
	andi r3, r1, 7
	bne r3, r18, .LBB13_42
.LBB13_44:
	xori r3, r1, 7
	stb r13+5, r3
	andi r1, r1, 32
	beq r1, r16, .LBB13_42
.LBB13_45:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, reallymarkobject
	jal r0, .LBB13_42
.LBB13_46:
	ldbu r3, r11+73
	slli r3, r3, 2
	addi r15, r3, 1
	ldw r3, r11+12
	lui r4, 699051
	addi r4, r4, -1365
	mulhu r3, r3, r4
	srli r3, r3, 3
	ldbu r4, r11+74
	lui r13, 524288
	addi r5, r0, 30
	bgtu r4, r5, .LBB13_106
.LBB13_47:
	sll r1, r1, r4
	lui r4, 174763
	addi r4, r4, -1365
	mulhu r1, r1, r4
	srli r1, r1, 1
	mul r1, r1, r15
	jal r0, .LBB13_107
.LBB13_48:
	stw r11+128, r16
.LBB13_49:
	ldw r13, r11+84
	ldw r17, r11+140
	beq r13, r17, .LBB13_55
.LBB13_50:
	addi r18, r0, 3
	jal r0, .LBB13_52
.LBB13_51:
	ldw r13, r13+0
	beq r13, r17, .LBB13_55
.LBB13_52:
	ldbu r1, r13+5
	andi r3, r1, 7
	bne r3, r18, .LBB13_51
.LBB13_53:
	xori r3, r1, 7
	stb r13+5, r3
	andi r1, r1, 32
	beq r1, r16, .LBB13_51
.LBB13_54:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, reallymarkobject
	jal r0, .LBB13_51
.LBB13_55:
	ldw r13, r11+108
	beq r13, r16, .LBB13_61
.LBB13_56:
	addi r17, r0, 3
	jal r0, .LBB13_58
.LBB13_57:
	ldw r13, r13+0
	beq r13, r16, .LBB13_61
.LBB13_58:
	ldbu r1, r13+5
	andi r3, r1, 7
	bne r3, r17, .LBB13_57
.LBB13_59:
	xori r3, r1, 7
	stb r13+5, r3
	andi r1, r1, 32
	beq r1, r16, .LBB13_57
.LBB13_60:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, reallymarkobject
	jal r0, .LBB13_57
.LBB13_61:
	add r3, r12, r0
	jal r31, atomic
	addi r1, r0, 3
	stb r11+65, r1
	addi r17, r11, 76
	ldw r18, r11+116
	ldw r4, r11+76
	lui r13, %hi(sweepgen.nextage)
	addi r13, r13, %lo(sweepgen.nextage)
	beq r4, r18, .LBB13_71
.LBB13_62:
	ldbu r1, r11+64
	andi r1, r1, 24
	ori  r19, r1, 1
	addi r20, r0, 2
	jal r0, .LBB13_64
.LBB13_63:
	ldw r1, r4+0
	stw r17+0, r1
	add r3, r12, r0
	jal r31, freeobj
	ldw r4, r17+0
	beq r4, r18, .LBB13_71
.LBB13_64:
	ldbu r1, r4+5
	andi r3, r1, 24
	bne r3, r16, .LBB13_63
.LBB13_65:
	andi r3, r1, 7
	beq r3, r16, .LBB13_69
.LBB13_66:
	andi r1, r1, 224
	add r5, r3, r13
	ldbu r5, r5+0
	or  r1, r5, r1
	stb r4+5, r1
	bgtu r3, r20, .LBB13_70
.LBB13_67:
	ldw r1, r11+128
	bne r1, r16, .LBB13_70
.LBB13_68:
	stw r11+128, r4
	jal r0, .LBB13_70
.LBB13_69:
	andi r1, r1, 192
	or  r1, r1, r19
	stb r4+5, r1
.LBB13_70:
	add r17, r4, r0
	ldw r4, r17+0
	bne r4, r18, .LBB13_64
.LBB13_71:
	ldw r18, r11+120
	ldw r4, r17+0
	beq r4, r18, .LBB13_81
.LBB13_72:
	ldbu r1, r11+64
	andi r1, r1, 24
	ori  r19, r1, 1
	addi r20, r0, 2
	add r21, r17, r0
	jal r0, .LBB13_74
.LBB13_73:
	ldw r1, r4+0
	stw r21+0, r1
	add r3, r12, r0
	jal r31, freeobj
	ldw r4, r21+0
	beq r4, r18, .LBB13_81
.LBB13_74:
	ldbu r1, r4+5
	andi r3, r1, 24
	bne r3, r16, .LBB13_73
.LBB13_75:
	andi r3, r1, 7
	beq r3, r16, .LBB13_79
.LBB13_76:
	andi r1, r1, 224
	add r5, r3, r13
	ldbu r5, r5+0
	or  r1, r5, r1
	stb r4+5, r1
	bgtu r3, r20, .LBB13_80
.LBB13_77:
	ldw r1, r11+128
	bne r1, r16, .LBB13_80
.LBB13_78:
	stw r11+128, r4
	jal r0, .LBB13_80
.LBB13_79:
	andi r1, r1, 192
	or  r1, r1, r19
	stb r4+5, r1
.LBB13_80:
	add r21, r4, r0
	ldw r4, r21+0
	bne r4, r18, .LBB13_74
.LBB13_81:
	addi r18, r11, 84
	ldw r1, r11+120
	stw r11+124, r1
	ldw r1, r17+0
	stw r11+120, r1
	ldw r1, r11+76
	stw r11+116, r1
	ldw r17, r11+132
	ldw r4, r11+84
	beq r4, r17, .LBB13_89
.LBB13_82:
	ldbu r1, r11+64
	andi r1, r1, 24
	ori  r19, r1, 1
	jal r0, .LBB13_84
.LBB13_83:
	ldw r1, r4+0
	stw r18+0, r1
	add r3, r12, r0
	jal r31, freeobj
	ldw r4, r18+0
	beq r4, r17, .LBB13_89
.LBB13_84:
	ldbu r1, r4+5
	andi r3, r1, 24
	bne r3, r16, .LBB13_83
.LBB13_85:
	andi r3, r1, 7
	beq r3, r16, .LBB13_87
.LBB13_86:
	andi r1, r1, 224
	add r3, r3, r13
	ldbu r3, r3+0
	or  r1, r3, r1
	jal r0, .LBB13_88
.LBB13_87:
	andi r1, r1, 192
	or  r1, r1, r19
.LBB13_88:
	stb r4+5, r1
	add r18, r4, r0
	ldw r4, r18+0
	bne r4, r17, .LBB13_84
.LBB13_89:
	ldw r17, r11+136
	ldw r4, r18+0
	beq r4, r17, .LBB13_97
.LBB13_90:
	ldbu r1, r11+64
	andi r1, r1, 24
	ori  r19, r1, 1
	add r20, r18, r0
	jal r0, .LBB13_92
.LBB13_91:
	ldw r1, r4+0
	stw r20+0, r1
	add r3, r12, r0
	jal r31, freeobj
	ldw r4, r20+0
	beq r4, r17, .LBB13_97
.LBB13_92:
	ldbu r1, r4+5
	andi r3, r1, 24
	bne r3, r16, .LBB13_91
.LBB13_93:
	andi r3, r1, 7
	beq r3, r16, .LBB13_95
.LBB13_94:
	andi r1, r1, 224
	add r3, r3, r13
	ldbu r3, r3+0
	or  r1, r3, r1
	jal r0, .LBB13_96
.LBB13_95:
	andi r1, r1, 192
	or  r1, r1, r19
.LBB13_96:
	stb r4+5, r1
	add r20, r4, r0
	ldw r4, r20+0
	bne r4, r17, .LBB13_92
.LBB13_97:
	ldw r1, r11+136
	stw r11+140, r1
	ldw r1, r18+0
	stw r11+136, r1
	ldw r1, r11+84
	stw r11+132, r1
	ldw r4, r11+108
	beq r4, r16, .LBB13_105
.LBB13_98:
	addi r18, r11, 108
	ldbu r1, r11+64
	andi r1, r1, 24
	ori  r17, r1, 1
	jal r0, .LBB13_100
.LBB13_99:
	ldw r1, r4+0
	stw r18+0, r1
	add r3, r12, r0
	jal r31, freeobj
	ldw r4, r18+0
	beq r4, r16, .LBB13_105
.LBB13_100:
	ldbu r1, r4+5
	andi r3, r1, 24
	bne r3, r16, .LBB13_99
.LBB13_101:
	andi r3, r1, 7
	beq r3, r16, .LBB13_103
.LBB13_102:
	andi r1, r1, 224
	add r3, r3, r13
	ldbu r3, r3+0
	or  r1, r3, r1
	jal r0, .LBB13_104
.LBB13_103:
	andi r1, r1, 192
	or  r1, r1, r17
.LBB13_104:
	stb r4+5, r1
	add r18, r4, r0
	ldw r4, r18+0
	bne r4, r16, .LBB13_100
.LBB13_105:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, finishgencycle
	ldw r1, r11+8
	ldw r3, r11+12
	add r1, r3, r1
	mulhu r1, r1, r15
	srli r1, r1, 5
	ldbu r3, r11+68
	mul r1, r1, r3
	sub r4, r16, r1
	add r3, r11, r0
	jal r31, luaE_setdebt
	stw r11+16, r14
	jal r0, .LBB13_3
.LBB13_106:
	addi r1, r13, -1
.LBB13_107:
	mul r16, r15, r3
	sub r18, r14, r1
	addi r17, r0, 8
.LBB13_108:
	add r3, r12, r0
	jal r31, singlestep
	sub r16, r16, r1
	ble r16, r18, .LBB13_110
.LBB13_109:
	ldbu r1, r11+65
	bne r1, r17, .LBB13_108
.LBB13_110:
	ldbu r1, r11+65
	bne r1, r17, .LBB13_112
.LBB13_111:
	ldbu r1, r11+72
	slli r15, r1, 2
	ldw r1, r11+16
	lui r3, 335544
	addi r3, r3, 1311
	mulhu r1, r1, r3
	srli r12, r1, 5
	addi r13, r13, -1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, __udivsi3
	sltu r1, r15, r1
	mul r3, r12, r15
	xor r3, r3, r13
	sub r1, r14, r1
	and r1, r3, r1
	xor r1, r1, r13
	ldw r3, r11+8
	ldw r4, r11+12
	add r3, r4, r3
	sub r1, r3, r1
	slt r3, r1, r14
	sub r3, r14, r3
	and r4, r1, r3
	jal r0, .LBB13_2
.LBB13_112:
	div r1, r16, r15
	addi r3, r0, 12
	mul r4, r1, r3
	jal r0, .LBB13_2
.Lfunc_end13:
	.size	luaC_step, .Lfunc_end13-luaC_step
                                        # -- End function
	.hidden	luaC_fullgc                     # -- Begin function luaC_fullgc
	.globl	luaC_fullgc
	.p2align	2
	.type	luaC_fullgc,@function
luaC_fullgc:                            # @luaC_fullgc
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
	ldw r11, r3+16
	stb r11+71, r4
	ldbu r1, r11+66
	addi r14, r0, 0
	beq r1, r14, .LBB14_8
.LBB14_1:
	ldw r3, r11+76
	ldbu r1, r11+64
	andi r1, r1, 24
	beq r3, r14, .LBB14_3
.LBB14_2:
	ldbu r4, r3+5
	andi r4, r4, 192
	or  r4, r4, r1
	stb r3+5, r4
	ldw r3, r3+0
	bne r3, r14, .LBB14_2
.LBB14_3:
	stw r11+116, r14
	stw r11+120, r14
	stw r11+124, r14
	ldw r3, r11+84
	beq r3, r14, .LBB14_5
.LBB14_4:
	ldbu r4, r3+5
	andi r4, r4, 192
	or  r4, r4, r1
	stb r3+5, r4
	ldw r3, r3+0
	bne r3, r14, .LBB14_4
.LBB14_5:
	ldw r3, r11+108
	beq r3, r14, .LBB14_7
.LBB14_6:
	ldbu r4, r3+5
	andi r4, r4, 192
	or  r4, r4, r1
	stb r3+5, r4
	ldw r3, r3+0
	bne r3, r14, .LBB14_6
.LBB14_7:
	stw r11+132, r14
	stw r11+136, r14
	stw r11+140, r14
	addi r1, r0, 8
	stb r11+65, r1
	stb r11+66, r14
	stw r11+20, r14
	add r3, r12, r0
	add r4, r11, r0
	jal r31, entergen
	jal r0, .LBB14_26
.LBB14_8:
	ldbu r1, r11+65
	addi r3, r0, 2
	bgtu r1, r3, .LBB14_17
.LBB14_9:
	addi r1, r0, 3
	stb r11+65, r1
	addi r13, r11, 76
	add r15, r13, r0
	jal r0, .LBB14_13
.LBB14_10:
	ldw r1, r4+0
	stw r15+0, r1
	add r3, r12, r0
	jal r31, freeobj
.LBB14_11:
	ldw r1, r15+0
	seq r1, r1, r14
	sub r1, r14, r1
	and r1, r15, r1
	xor r1, r15, r1
.LBB14_12:
	add r15, r1, r0
	bne r1, r13, .LBB14_16
.LBB14_13:
	ldw r4, r15+0
	add r1, r14, r0
	beq r4, r14, .LBB14_12
.LBB14_14:
	ldw r1, r12+16
	ldbu r1, r1+64
	xori r5, r1, 24
	ldbu r3, r4+5
	and r5, r3, r5
	bne r5, r14, .LBB14_10
.LBB14_15:
	andi r1, r1, 24
	andi r3, r3, 192
	or  r1, r3, r1
	stb r4+5, r1
	add r15, r4, r0
	jal r0, .LBB14_11
.LBB14_16:
	stw r11+80, r1
.LBB14_17:
	ldw r15, r12+16
	ldbu r1, r15+65
	addi r13, r0, 8
	beq r1, r13, .LBB14_19
.LBB14_18:
	add r3, r12, r0
	jal r31, singlestep
	ldbu r1, r15+65
	bne r1, r13, .LBB14_18
.LBB14_19:
	ldw r15, r12+16
	ldbu r1, r15+65
	beq r1, r14, .LBB14_21
.LBB14_20:
	add r3, r12, r0
	jal r31, singlestep
	ldbu r1, r15+65
	bne r1, r14, .LBB14_20
.LBB14_21:
	addi r1, r0, 1
	stb r11+65, r1
	ldw r15, r12+16
	ldbu r1, r15+65
	addi r16, r0, 7
	beq r1, r16, .LBB14_23
.LBB14_22:
	add r3, r12, r0
	jal r31, singlestep
	ldbu r1, r15+65
	bne r1, r16, .LBB14_22
.LBB14_23:
	ldw r15, r12+16
	ldbu r1, r15+65
	beq r1, r13, .LBB14_25
.LBB14_24:
	add r3, r12, r0
	jal r31, singlestep
	ldbu r1, r15+65
	bne r1, r13, .LBB14_24
.LBB14_25:
	ldbu r1, r11+72
	slli r15, r1, 2
	ldw r1, r11+16
	lui r3, 335544
	addi r3, r3, 1311
	mulhu r1, r1, r3
	srli r12, r1, 5
	lui r1, 524288
	addi r13, r1, -1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, __udivsi3
	sltu r1, r15, r1
	mul r3, r12, r15
	xor r3, r3, r13
	sub r1, r14, r1
	and r1, r3, r1
	xor r1, r1, r13
	ldw r3, r11+8
	ldw r4, r11+12
	add r3, r4, r3
	sub r1, r3, r1
	slt r3, r1, r14
	sub r3, r14, r3
	and r4, r1, r3
	add r3, r11, r0
	jal r31, luaE_setdebt
.LBB14_26:
	stb r11+71, r14
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
.Lfunc_end14:
	.size	luaC_fullgc, .Lfunc_end14-luaC_fullgc
                                        # -- End function
	.p2align	2                               # -- Begin function freeobj
	.type	freeobj,@function
freeobj:                                # @freeobj
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	ldbu r1, r4+4
	addi r1, r1, -4
	addi r5, r0, 34
	bgtu r1, r5, .LBB15_15
.LBB15_1:
	slli r1, r1, 2
	lui r5, %hi(.LJTI15_0)
	addi r5, r5, %lo(.LJTI15_0)
	add r1, r5, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB15_2:
	add r11, r3, r0
	add r12, r4, r0
	jal r31, luaS_remove
	ldbu r1, r12+7
	addi r5, r1, 17
	add r3, r11, r0
	add r4, r12, r0
	jal r0, .LBB15_14
.LBB15_3:
	ldbu r1, r4+6
	slli r1, r1, 2
	jal r0, .LBB15_13
.LBB15_4:
	ldw r1, r4+8
	addi r5, r4, 12
	beq r1, r5, .LBB15_6
.LBB15_5:
	add r11, r3, r0
	add r3, r4, r0
	add r12, r4, r0
	jal r31, luaF_unlinkupval
	add r3, r11, r0
	add r4, r12, r0
.LBB15_6:
	addi r5, r0, 24
	jal r0, .LBB15_14
.LBB15_7:
	jal r31, luaH_free
	jal r0, .LBB15_15
.LBB15_8:
	jal r31, luaF_freeproto
	jal r0, .LBB15_15
.LBB15_9:
	ldw r1, r4+12
	addi r5, r1, 17
	jal r0, .LBB15_14
.LBB15_10:
	ldhu r1, r4+6
	addi r5, r0, 0
	seq r6, r1, r5
	addi r7, r0, 12
	mul r1, r1, r7
	addi r1, r1, 20
	sub r5, r5, r6
	xori r6, r1, 16
	and r5, r6, r5
	xor r1, r1, r5
	ldw r5, r4+8
	add r5, r1, r5
	jal r0, .LBB15_14
.LBB15_11:
	jal r31, luaE_freethread
	jal r0, .LBB15_15
.LBB15_12:
	ldbu r1, r4+6
	addi r5, r0, 12
	mul r1, r1, r5
.LBB15_13:
	addi r5, r1, 16
.LBB15_14:
	jal r31, luaM_free_
.LBB15_15:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end15:
	.size	freeobj, .Lfunc_end15-freeobj
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI15_0:
	.word	.LBB15_2
	.word	.LBB15_7
	.word	.LBB15_3
	.word	.LBB15_10
	.word	.LBB15_11
	.word	.LBB15_4
	.word	.LBB15_8
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_9
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_15
	.word	.LBB15_12
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function atomic
	.type	atomic,@function
atomic:                                 # @atomic
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
	ldw r11, r3+16
	ldw r12, r11+92
	addi r13, r0, 0
	stw r11+92, r13
	addi r1, r0, 2
	stb r11+65, r1
	ldbu r1, r3+5
	andi r1, r1, 24
	beq r1, r13, .LBB16_2
.LBB16_1:
	add r4, r3, r0
	add r3, r11, r0
	jal r31, reallymarkobject
.LBB16_2:
	ldbu r1, r11+44
	andi r1, r1, 64
	beq r1, r13, .LBB16_5
.LBB16_3:
	ldw r4, r11+36
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB16_5
.LBB16_4:
	add r3, r11, r0
	jal r31, reallymarkobject
.LBB16_5:
	addi r14, r0, 260
	addi r15, r0, 296
	jal r0, .LBB16_7
.LBB16_6:
	addi r14, r14, 4
	beq r14, r15, .LBB16_10
.LBB16_7:
	add r1, r11, r14
	ldw r4, r1+0
	beq r4, r13, .LBB16_6
.LBB16_8:
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB16_6
.LBB16_9:
	add r3, r11, r0
	jal r31, reallymarkobject
	jal r0, .LBB16_6
.LBB16_10:
	ldw r1, r11+88
	add r16, r13, r0
	beq r1, r13, .LBB16_13
.LBB16_11:
	addi r14, r0, 0
	add r16, r14, r0
.LBB16_12:
	add r3, r11, r0
	jal r31, propagatemark
	add r16, r1, r16
	ldw r1, r11+88
	bne r1, r14, .LBB16_12
.LBB16_13:
	ldw r1, r11+144
	add r17, r13, r0
	beq r1, r13, .LBB16_25
.LBB16_14:
	addi r15, r11, 144
	addi r14, r0, 0
	add r17, r14, r0
	jal r0, .LBB16_18
.LBB16_15:
	ldw r3, r1+32
	beq r3, r14, .LBB16_19
.LBB16_16:
	addi r15, r1, 44
.LBB16_17:
	ldw r1, r15+0
	beq r1, r14, .LBB16_25
.LBB16_18:
	addi r17, r17, 1
	ldbu r3, r1+5
	andi r3, r3, 24
	beq r3, r14, .LBB16_15
.LBB16_19:
	ldw r3, r1+44
	stw r15+0, r3
	stw r1+44, r1
	ldw r18, r1+32
	bne r18, r14, .LBB16_21
	jal r0, .LBB16_17
.LBB16_20:
	ldw r18, r18+12
	addi r17, r17, 1
	beq r18, r14, .LBB16_17
.LBB16_21:
	ldbu r1, r18+5
	andi r1, r1, 24
	bne r1, r14, .LBB16_20
.LBB16_22:
	ldw r1, r18+8
	ldbu r3, r1+8
	andi r3, r3, 64
	beq r3, r14, .LBB16_20
.LBB16_23:
	ldw r4, r1+0
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r14, .LBB16_20
.LBB16_24:
	add r3, r11, r0
	jal r31, reallymarkobject
	jal r0, .LBB16_20
.LBB16_25:
	ldw r1, r11+88
	add r18, r13, r0
	beq r1, r13, .LBB16_28
.LBB16_26:
	addi r14, r0, 0
	add r18, r14, r0
.LBB16_27:
	add r3, r11, r0
	jal r31, propagatemark
	add r18, r1, r18
	ldw r1, r11+88
	bne r1, r14, .LBB16_27
.LBB16_28:
	stw r11+88, r12
	add r19, r13, r0
	beq r12, r13, .LBB16_31
.LBB16_29:
	addi r12, r0, 0
	add r19, r12, r0
.LBB16_30:
	add r3, r11, r0
	jal r31, propagatemark
	add r19, r1, r19
	ldw r1, r11+88
	bne r1, r12, .LBB16_30
.LBB16_31:
	addi r14, r0, 1
	add r12, r13, r0
	jal r0, .LBB16_33
.LBB16_32:
	xori r12, r12, 1
	beq r20, r13, .LBB16_40
.LBB16_33:
	ldw r15, r11+100
	stw r11+100, r13
	beq r15, r13, .LBB16_40
.LBB16_34:
	add r20, r13, r0
	jal r0, .LBB16_37
.LBB16_35:
	add r20, r14, r0
.LBB16_36:
	beq r15, r13, .LBB16_32
.LBB16_37:
	add r4, r15, r0
	ldw r15, r15+28
	ldbu r1, r4+5
	ori  r1, r1, 32
	stb r4+5, r1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, traverseephemeron
	beq r1, r13, .LBB16_36
.LBB16_38:
	ldw r1, r11+88
	add r20, r14, r0
	beq r1, r13, .LBB16_36
.LBB16_39:
	add r3, r11, r0
	jal r31, propagatemark
	ldw r1, r11+88
	bne r1, r13, .LBB16_39
	jal r0, .LBB16_35
.LBB16_40:
	ldw r4, r11+96
	addi r14, r0, 0
	add r3, r11, r0
	add r5, r14, r0
	jal r31, clearbyvalues
	ldw r4, r11+104
	add r3, r11, r0
	add r5, r14, r0
	jal r31, clearbyvalues
	ldw r13, r11+96
	ldw r12, r11+104
	addi r1, r11, 108
	add r4, r1, r0
.LBB16_41:
	add r3, r4, r0
	ldw r4, r4+0
	bne r4, r14, .LBB16_41
.LBB16_42:
	ldw r5, r11+84
	ldw r4, r11+136
	beq r5, r4, .LBB16_50
.LBB16_43:
	addi r4, r11, 84
	jal r0, .LBB16_46
.LBB16_44:
	ldw r6, r5+0
	stw r4+0, r6
	ldw r6, r3+0
	stw r5+0, r6
	stw r3+0, r5
	add r3, r5, r0
.LBB16_45:
	ldw r5, r4+0
	ldw r6, r11+136
	beq r5, r6, .LBB16_50
.LBB16_46:
	ldbu r6, r5+5
	andi r6, r6, 24
	beq r6, r14, .LBB16_49
.LBB16_47:
	ldw r6, r11+132
	bne r5, r6, .LBB16_44
.LBB16_48:
	ldw r6, r5+0
	stw r11+132, r6
	jal r0, .LBB16_44
.LBB16_49:
	add r4, r5, r0
	jal r0, .LBB16_45
.LBB16_50:
	ldw r15, r1+0
	add r20, r14, r0
	beq r15, r14, .LBB16_55
.LBB16_51:
	addi r21, r0, 0
	add r20, r21, r0
	jal r0, .LBB16_53
.LBB16_52:
	ldw r15, r15+0
	addi r20, r20, 1
	beq r15, r21, .LBB16_55
.LBB16_53:
	ldbu r1, r15+5
	andi r1, r1, 24
	beq r1, r21, .LBB16_52
.LBB16_54:
	add r3, r11, r0
	add r4, r15, r0
	jal r31, reallymarkobject
	jal r0, .LBB16_52
.LBB16_55:
	ldw r1, r11+88
	add r21, r14, r0
	beq r1, r14, .LBB16_58
.LBB16_56:
	addi r15, r0, 0
	add r21, r15, r0
.LBB16_57:
	add r3, r11, r0
	jal r31, propagatemark
	add r21, r1, r21
	ldw r1, r11+88
	bne r1, r15, .LBB16_57
.LBB16_58:
	addi r22, r0, 1
	add r15, r14, r0
	jal r0, .LBB16_60
.LBB16_59:
	xori r15, r15, 1
	beq r24, r14, .LBB16_67
.LBB16_60:
	ldw r23, r11+100
	stw r11+100, r14
	beq r23, r14, .LBB16_67
.LBB16_61:
	add r24, r14, r0
	jal r0, .LBB16_64
.LBB16_62:
	add r24, r22, r0
.LBB16_63:
	beq r23, r14, .LBB16_59
.LBB16_64:
	add r4, r23, r0
	ldw r23, r23+28
	ldbu r1, r4+5
	ori  r1, r1, 32
	stb r4+5, r1
	add r3, r11, r0
	add r5, r15, r0
	jal r31, traverseephemeron
	beq r1, r14, .LBB16_63
.LBB16_65:
	ldw r1, r11+88
	add r24, r22, r0
	beq r1, r14, .LBB16_63
.LBB16_66:
	add r3, r11, r0
	jal r31, propagatemark
	ldw r1, r11+88
	bne r1, r14, .LBB16_66
	jal r0, .LBB16_62
.LBB16_67:
	ldw r14, r11+100
	addi r15, r0, 0
	beq r14, r15, .LBB16_83
.LBB16_68:
	addi r22, r0, 24
	addi r23, r0, 11
	addi r24, r0, 4
	addi r25, r0, 16
	jal r0, .LBB16_70
.LBB16_69:
	ldw r14, r14+28
	beq r14, r15, .LBB16_83
.LBB16_70:
	ldw r26, r14+16
	ldbu r1, r14+7
	sll r1, r22, r1
	add r27, r26, r1
	jal r0, .LBB16_73
.LBB16_71:
	ldbu r1, r26+8
	andi r1, r1, 15
	beq r1, r15, .LBB16_79
.LBB16_72:
	addi r26, r26, 24
	bgeu r26, r27, .LBB16_69
.LBB16_73:
	ldbu r1, r26+9
	andi r1, r1, 64
	add r4, r15, r0
	beq r1, r15, .LBB16_75
.LBB16_74:
	ldw r4, r26+16
.LBB16_75:
	beq r4, r15, .LBB16_71
.LBB16_76:
	ldbu r1, r4+4
	andi r3, r1, 15
	ldbu r1, r4+5
	andi r1, r1, 24
	bne r3, r24, .LBB16_81
.LBB16_77:
	beq r1, r15, .LBB16_71
.LBB16_78:
	add r3, r11, r0
	jal r31, reallymarkobject
	jal r0, .LBB16_71
.LBB16_79:
	ldbu r1, r26+9
	andi r1, r1, 64
	beq r1, r15, .LBB16_72
.LBB16_80:
	stb r26+9, r23
	jal r0, .LBB16_72
.LBB16_81:
	beq r1, r15, .LBB16_71
.LBB16_82:
	stb r26+8, r25
	jal r0, .LBB16_71
.LBB16_83:
	ldw r14, r11+104
	addi r15, r0, 0
	beq r14, r15, .LBB16_99
.LBB16_84:
	addi r22, r0, 24
	addi r23, r0, 11
	addi r24, r0, 4
	addi r25, r0, 16
	jal r0, .LBB16_86
.LBB16_85:
	ldw r14, r14+28
	beq r14, r15, .LBB16_99
.LBB16_86:
	ldw r26, r14+16
	ldbu r1, r14+7
	sll r1, r22, r1
	add r27, r26, r1
	jal r0, .LBB16_89
.LBB16_87:
	ldbu r1, r26+8
	andi r1, r1, 15
	beq r1, r15, .LBB16_95
.LBB16_88:
	addi r26, r26, 24
	bgeu r26, r27, .LBB16_85
.LBB16_89:
	ldbu r1, r26+9
	andi r1, r1, 64
	add r4, r15, r0
	beq r1, r15, .LBB16_91
.LBB16_90:
	ldw r4, r26+16
.LBB16_91:
	beq r4, r15, .LBB16_87
.LBB16_92:
	ldbu r1, r4+4
	andi r3, r1, 15
	ldbu r1, r4+5
	andi r1, r1, 24
	bne r3, r24, .LBB16_97
.LBB16_93:
	beq r1, r15, .LBB16_87
.LBB16_94:
	add r3, r11, r0
	jal r31, reallymarkobject
	jal r0, .LBB16_87
.LBB16_95:
	ldbu r1, r26+9
	andi r1, r1, 64
	beq r1, r15, .LBB16_88
.LBB16_96:
	stb r26+9, r23
	jal r0, .LBB16_88
.LBB16_97:
	beq r1, r15, .LBB16_87
.LBB16_98:
	stb r26+8, r25
	jal r0, .LBB16_87
.LBB16_99:
	add r1, r17, r16
	add r1, r1, r18
	add r1, r1, r19
	add r1, r1, r20
	add r14, r1, r21
	ldw r4, r11+96
	add r3, r11, r0
	add r5, r13, r0
	jal r31, clearbyvalues
	ldw r4, r11+104
	add r3, r11, r0
	add r5, r12, r0
	jal r31, clearbyvalues
	add r3, r11, r0
	jal r31, luaS_clearcache
	ldbu r1, r11+64
	xori r1, r1, 24
	stb r11+64, r1
	add r1, r14, r0
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
.Lfunc_end16:
	.size	atomic, .Lfunc_end16-atomic
                                        # -- End function
	.p2align	2                               # -- Begin function atomic2gen
	.type	atomic2gen,@function
atomic2gen:                             # @atomic2gen
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
	add r11, r4, r0
	add r12, r3, r0
	addi r13, r0, 0
	stw r4+92, r13
	stw r4+88, r13
	stw r4+100, r13
	stw r4+104, r13
	stw r4+96, r13
	addi r1, r0, 3
	stb r4+65, r1
	ldw r4, r4+76
	beq r4, r13, .LBB17_11
.LBB17_1:
	addi r17, r11, 76
	ldw r14, r12+16
	addi r15, r0, 9
	addi r16, r0, 8
	jal r0, .LBB17_3
.LBB17_2:
	ldw r1, r4+0
	stw r17+0, r1
	add r3, r12, r0
	jal r31, freeobj
	ldw r4, r17+0
	beq r4, r13, .LBB17_11
.LBB17_3:
	ldbu r1, r4+5
	andi r3, r1, 24
	bne r3, r13, .LBB17_2
.LBB17_4:
	andi r3, r1, 224
	ori  r1, r3, 4
	stb r4+5, r1
	ldbu r5, r4+4
	beq r5, r15, .LBB17_7
.LBB17_5:
	bne r5, r16, .LBB17_9
.LBB17_6:
	ldw r3, r14+92
	stw r4+40, r3
	stw r14+92, r4
	andi r1, r1, 196
	jal r0, .LBB17_10
.LBB17_7:
	ldw r5, r4+8
	addi r6, r4, 12
	beq r5, r6, .LBB17_9
.LBB17_8:
	andi r1, r1, 196
	jal r0, .LBB17_10
.LBB17_9:
	ori  r1, r3, 36
.LBB17_10:
	stb r4+5, r1
	add r17, r4, r0
	ldw r4, r17+0
	bne r4, r13, .LBB17_3
.LBB17_11:
	ldw r1, r11+76
	stw r11+116, r1
	stw r11+120, r1
	stw r11+124, r1
	stw r11+128, r13
	ldw r4, r11+84
	beq r4, r13, .LBB17_22
.LBB17_12:
	addi r17, r11, 84
	ldw r14, r12+16
	addi r15, r0, 9
	addi r16, r0, 8
	jal r0, .LBB17_14
.LBB17_13:
	ldw r1, r4+0
	stw r17+0, r1
	add r3, r12, r0
	jal r31, freeobj
	ldw r4, r17+0
	beq r4, r13, .LBB17_22
.LBB17_14:
	ldbu r1, r4+5
	andi r3, r1, 24
	bne r3, r13, .LBB17_13
.LBB17_15:
	andi r3, r1, 224
	ori  r1, r3, 4
	stb r4+5, r1
	ldbu r5, r4+4
	beq r5, r15, .LBB17_18
.LBB17_16:
	bne r5, r16, .LBB17_20
.LBB17_17:
	ldw r3, r14+92
	stw r4+40, r3
	stw r14+92, r4
	andi r1, r1, 196
	jal r0, .LBB17_21
.LBB17_18:
	ldw r5, r4+8
	addi r6, r4, 12
	beq r5, r6, .LBB17_20
.LBB17_19:
	andi r1, r1, 196
	jal r0, .LBB17_21
.LBB17_20:
	ori  r1, r3, 36
.LBB17_21:
	stb r4+5, r1
	add r17, r4, r0
	ldw r4, r17+0
	bne r4, r13, .LBB17_14
.LBB17_22:
	ldw r1, r11+84
	stw r11+132, r1
	stw r11+136, r1
	stw r11+140, r1
	ldw r4, r11+108
	beq r4, r13, .LBB17_33
.LBB17_23:
	addi r17, r11, 108
	ldw r14, r12+16
	addi r15, r0, 9
	addi r16, r0, 8
	jal r0, .LBB17_25
.LBB17_24:
	ldw r1, r4+0
	stw r17+0, r1
	add r3, r12, r0
	jal r31, freeobj
	ldw r4, r17+0
	beq r4, r13, .LBB17_33
.LBB17_25:
	ldbu r1, r4+5
	andi r3, r1, 24
	bne r3, r13, .LBB17_24
.LBB17_26:
	andi r3, r1, 224
	ori  r1, r3, 4
	stb r4+5, r1
	ldbu r5, r4+4
	beq r5, r15, .LBB17_29
.LBB17_27:
	bne r5, r16, .LBB17_31
.LBB17_28:
	ldw r3, r14+92
	stw r4+40, r3
	stw r14+92, r4
	andi r1, r1, 196
	jal r0, .LBB17_32
.LBB17_29:
	ldw r5, r4+8
	addi r6, r4, 12
	beq r5, r6, .LBB17_31
.LBB17_30:
	andi r1, r1, 196
	jal r0, .LBB17_32
.LBB17_31:
	ori  r1, r3, 36
.LBB17_32:
	stb r4+5, r1
	add r17, r4, r0
	ldw r4, r17+0
	bne r4, r13, .LBB17_25
.LBB17_33:
	addi r1, r0, 1
	stb r11+66, r1
	stw r11+20, r13
	ldw r1, r11+8
	ldw r3, r11+12
	add r1, r3, r1
	stw r11+16, r1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, finishgencycle
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
	.size	atomic2gen, .Lfunc_end17-atomic2gen
                                        # -- End function
	.p2align	2                               # -- Begin function clearbyvalues
	.type	clearbyvalues,@function
clearbyvalues:                          # @clearbyvalues
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
	beq r4, r5, .LBB18_27
.LBB18_1:
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	addi r15, r0, 1
	addi r16, r0, 24
	addi r17, r0, 0
	addi r18, r0, 11
	addi r19, r0, 4
	addi r20, r0, 16
	jal r0, .LBB18_3
.LBB18_2:
	ldw r12, r12+28
	beq r12, r11, .LBB18_27
.LBB18_3:
	ldw r21, r12+16
	ldbu r22, r12+7
	add r3, r12, r0
	jal r31, luaH_realasize
	beq r1, r17, .LBB18_14
.LBB18_4:
	add r14, r1, r0
	add r23, r17, r0
	jal r0, .LBB18_6
.LBB18_5:
	addi r14, r14, -1
	addi r23, r23, 12
	beq r14, r17, .LBB18_14
.LBB18_6:
	ldw r1, r12+12
	add r1, r1, r23
	ldbu r3, r1+8
	andi r3, r3, 64
	add r4, r17, r0
	beq r3, r17, .LBB18_8
.LBB18_7:
	ldw r4, r1+0
.LBB18_8:
	beq r4, r17, .LBB18_5
.LBB18_9:
	ldbu r3, r4+4
	andi r5, r3, 15
	ldbu r3, r4+5
	andi r3, r3, 24
	bne r5, r19, .LBB18_12
.LBB18_10:
	beq r3, r17, .LBB18_5
.LBB18_11:
	add r3, r13, r0
	jal r31, reallymarkobject
	jal r0, .LBB18_5
.LBB18_12:
	beq r3, r17, .LBB18_5
.LBB18_13:
	stb r1+8, r20
	jal r0, .LBB18_5
.LBB18_14:
	sll r1, r15, r22
	mul r1, r1, r16
	add r14, r21, r1
	ldw r21, r12+16
	bltu r21, r14, .LBB18_17
	jal r0, .LBB18_2
.LBB18_15:
	ldbu r1, r21+8
	andi r1, r1, 15
	beq r1, r17, .LBB18_23
.LBB18_16:
	addi r21, r21, 24
	bgeu r21, r14, .LBB18_2
.LBB18_17:
	ldbu r1, r21+8
	andi r1, r1, 64
	add r4, r17, r0
	beq r1, r17, .LBB18_19
.LBB18_18:
	ldw r4, r21+0
.LBB18_19:
	beq r4, r17, .LBB18_15
.LBB18_20:
	ldbu r1, r4+4
	andi r3, r1, 15
	ldbu r1, r4+5
	andi r1, r1, 24
	bne r3, r19, .LBB18_25
.LBB18_21:
	beq r1, r17, .LBB18_15
.LBB18_22:
	add r3, r13, r0
	jal r31, reallymarkobject
	jal r0, .LBB18_15
.LBB18_23:
	ldbu r1, r21+9
	andi r1, r1, 64
	beq r1, r17, .LBB18_16
.LBB18_24:
	stb r21+9, r18
	jal r0, .LBB18_16
.LBB18_25:
	beq r1, r17, .LBB18_15
.LBB18_26:
	stb r21+8, r20
	jal r0, .LBB18_15
.LBB18_27:
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
.Lfunc_end18:
	.size	clearbyvalues, .Lfunc_end18-clearbyvalues
                                        # -- End function
	.p2align	2                               # -- Begin function propagatemark
	.type	propagatemark,@function
propagatemark:                          # @propagatemark
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
	add r12, r3, r0
	ldw r11, r3+88
	ldbu r3, r11+5
	ori  r1, r3, 32
	stb r11+5, r1
	ldbu r4, r11+4
	addi r1, r0, 28
	addi r5, r4, -5
	slli r4, r5, 2
	lui r6, %hi(.LJTI19_0)
	addi r6, r6, %lo(.LJTI19_0)
	add r6, r6, r4
	ldw r6, r6+0
	jalr r0, r6, 0
.LBB19_1:
	addi r1, r0, 8
	jal r0, .LBB19_5
.LBB19_2:
	addi r1, r0, 16
	jal r0, .LBB19_5
.LBB19_3:
	addi r1, r0, 80
	jal r0, .LBB19_5
.LBB19_4:
	addi r1, r0, 40
.LBB19_5:
	add r1, r11, r1
	ldw r1, r1+0
	stw r12+88, r1
	addi r1, r0, 0
	addi r6, r0, 33
	bgtu r5, r6, .LBB19_161
.LBB19_6:
	lui r5, %hi(.LJTI19_1)
	addi r5, r5, %lo(.LJTI19_1)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB19_7:
	ldw r3, r11+24
	addi r14, r0, 0
	add r13, r14, r0
	beq r3, r14, .LBB19_10
.LBB19_8:
	ldbu r1, r3+6
	andi r1, r1, 8
	addi r13, r0, 0
	bne r1, r13, .LBB19_10
.LBB19_9:
	ldw r5, r12+172
	addi r4, r0, 3
	jal r31, luaT_gettm
	add r13, r1, r0
.LBB19_10:
	ldw r4, r11+24
	beq r4, r14, .LBB19_13
.LBB19_11:
	ldbu r1, r4+5
	andi r1, r1, 24
	addi r3, r0, 0
	beq r1, r3, .LBB19_13
.LBB19_12:
	add r3, r12, r0
	jal r31, reallymarkobject
.LBB19_13:
	addi r15, r0, 0
	beq r13, r15, .LBB19_77
.LBB19_14:
	ldbu r1, r13+8
	addi r3, r0, 68
	bne r1, r3, .LBB19_77
.LBB19_15:
	ldw r1, r13+0
	addi r14, r1, 16
	addi r4, r0, 107
	add r3, r14, r0
	jal r31, strchr
	add r13, r1, r0
	addi r4, r0, 118
	add r3, r14, r0
	jal r31, strchr
	or  r3, r13, r1
	beq r3, r15, .LBB19_77
.LBB19_16:
	beq r13, r15, .LBB19_123
.LBB19_17:
	addi r3, r0, 0
	beq r1, r3, .LBB19_152
.LBB19_18:
	ldw r1, r12+104
	stw r11+28, r1
	stw r12+104, r11
	jal r0, .LBB19_154
.LBB19_19:
	ldw r4, r11+12
	addi r13, r0, 0
	beq r4, r13, .LBB19_22
.LBB19_20:
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB19_22
.LBB19_21:
	add r3, r12, r0
	jal r31, reallymarkobject
.LBB19_22:
	ldhu r1, r11+6
	beq r1, r13, .LBB19_28
.LBB19_23:
	addi r14, r11, 20
	add r15, r13, r0
	jal r0, .LBB19_25
.LBB19_24:
	addi r15, r15, 1
	ldhu r1, r11+6
	addi r14, r14, 12
	bgeu r15, r1, .LBB19_28
.LBB19_25:
	ldbu r1, r14+8
	andi r1, r1, 64
	beq r1, r13, .LBB19_24
.LBB19_26:
	ldw r4, r14+0
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB19_24
.LBB19_27:
	add r3, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB19_24
.LBB19_28:
	ldbu r3, r11+5
	andi r4, r3, 7
	addi r5, r0, 6
	beq r4, r5, .LBB19_117
.LBB19_29:
	addi r5, r0, 5
	bne r4, r5, .LBB19_160
.LBB19_30:
	ldbu r5, r11+4
	addi r4, r0, 28
	addi r5, r5, -5
	slli r5, r5, 2
	lui r6, %hi(.LJTI19_2)
	addi r6, r6, %lo(.LJTI19_2)
	add r5, r6, r5
	ldw r5, r5+0
	jalr r0, r5, 0
.LBB19_31:
	addi r4, r0, 8
	jal r0, .LBB19_146
.LBB19_32:
	ldbu r1, r11+6
	addi r13, r0, 0
	beq r1, r13, .LBB19_160
.LBB19_33:
	addi r14, r11, 16
	add r15, r13, r0
	jal r0, .LBB19_35
.LBB19_34:
	addi r15, r15, 1
	ldbu r1, r11+6
	addi r14, r14, 12
	bgeu r15, r1, .LBB19_160
.LBB19_35:
	ldbu r1, r14+8
	andi r1, r1, 64
	beq r1, r13, .LBB19_34
.LBB19_36:
	ldw r4, r14+0
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB19_34
.LBB19_37:
	add r3, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB19_34
.LBB19_38:
	ldw r4, r11+12
	addi r13, r0, 0
	beq r4, r13, .LBB19_41
.LBB19_39:
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB19_41
.LBB19_40:
	add r3, r12, r0
	jal r31, reallymarkobject
.LBB19_41:
	ldbu r1, r11+6
	beq r1, r13, .LBB19_160
.LBB19_42:
	addi r14, r11, 16
	add r15, r13, r0
	jal r0, .LBB19_44
.LBB19_43:
	addi r15, r15, 1
	ldbu r1, r11+6
	addi r14, r14, 4
	bgeu r15, r1, .LBB19_160
.LBB19_44:
	ldw r4, r14+0
	beq r4, r13, .LBB19_43
.LBB19_45:
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB19_43
.LBB19_46:
	add r3, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB19_43
.LBB19_47:
	ldw r13, r11+28
	andi r1, r3, 6
	addi r14, r0, 0
	beq r1, r14, .LBB19_99
.LBB19_48:
	ldw r1, r12+92
	stw r11+40, r1
	stw r12+92, r11
	andi r1, r3, 199
	stb r11+5, r1
	jal r0, .LBB19_100
.LBB19_49:
	ldw r4, r11+76
	addi r13, r0, 0
	beq r4, r13, .LBB19_52
.LBB19_50:
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB19_52
.LBB19_51:
	add r3, r12, r0
	jal r31, reallymarkobject
.LBB19_52:
	ldw r1, r11+16
	addi r14, r0, 1
	blt r1, r14, .LBB19_58
.LBB19_53:
	addi r15, r0, 0
	add r16, r15, r0
	add r17, r15, r0
	jal r0, .LBB19_55
.LBB19_54:
	addi r17, r17, 1
	ldw r1, r11+16
	addi r16, r16, 12
	bge r17, r1, .LBB19_58
.LBB19_55:
	ldw r1, r11+48
	add r1, r1, r16
	ldbu r3, r1+8
	andi r3, r3, 64
	beq r3, r15, .LBB19_54
.LBB19_56:
	ldw r4, r1+0
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r15, .LBB19_54
.LBB19_57:
	add r3, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB19_54
.LBB19_58:
	ldw r1, r11+12
	blt r1, r14, .LBB19_64
.LBB19_59:
	addi r15, r0, 0
	add r16, r15, r0
	add r17, r15, r0
	jal r0, .LBB19_61
.LBB19_60:
	addi r17, r17, 1
	ldw r1, r11+12
	addi r16, r16, 8
	bge r17, r1, .LBB19_64
.LBB19_61:
	ldw r1, r11+60
	add r1, r1, r16
	ldw r4, r1+0
	beq r4, r15, .LBB19_60
.LBB19_62:
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r15, .LBB19_60
.LBB19_63:
	add r3, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB19_60
.LBB19_64:
	ldw r1, r11+28
	blt r1, r14, .LBB19_70
.LBB19_65:
	addi r15, r0, 0
	add r16, r15, r0
	add r17, r15, r0
	jal r0, .LBB19_67
.LBB19_66:
	addi r17, r17, 1
	ldw r1, r11+28
	addi r16, r16, 4
	bge r17, r1, .LBB19_70
.LBB19_67:
	ldw r1, r11+56
	add r1, r1, r16
	ldw r4, r1+0
	beq r4, r15, .LBB19_66
.LBB19_68:
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r15, .LBB19_66
.LBB19_69:
	add r3, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB19_66
.LBB19_70:
	ldw r1, r11+32
	blt r1, r14, .LBB19_76
.LBB19_71:
	add r14, r13, r0
	add r15, r13, r0
	jal r0, .LBB19_73
.LBB19_72:
	addi r15, r15, 1
	ldw r1, r11+32
	addi r14, r14, 12
	bge r15, r1, .LBB19_76
.LBB19_73:
	ldw r1, r11+72
	add r1, r1, r14
	ldw r4, r1+0
	beq r4, r13, .LBB19_72
.LBB19_74:
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r13, .LBB19_72
.LBB19_75:
	add r3, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB19_72
.LBB19_76:
	ldw r3, r11+16
	ldw r4, r11+12
	ldw r5, r11+28
	add r1, r1, r3
	add r1, r1, r4
	add r1, r1, r5
	jal r0, .LBB19_160
.LBB19_77:
	addi r14, r11, 16
	ldw r1, r11+16
	ldbu r3, r11+7
	addi r4, r0, 1
	sll r3, r4, r3
	addi r4, r0, 24
	mul r3, r3, r4
	add r15, r1, r3
	add r3, r11, r0
	jal r31, luaH_realasize
	addi r16, r0, 0
	beq r1, r16, .LBB19_83
.LBB19_78:
	add r13, r1, r0
	add r17, r16, r0
	jal r0, .LBB19_80
.LBB19_79:
	addi r13, r13, -1
	addi r17, r17, 12
	beq r13, r16, .LBB19_83
.LBB19_80:
	ldw r1, r11+12
	add r1, r1, r17
	ldbu r3, r1+8
	andi r3, r3, 64
	beq r3, r16, .LBB19_79
.LBB19_81:
	ldw r4, r1+0
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r16, .LBB19_79
.LBB19_82:
	add r3, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB19_79
.LBB19_83:
	ldw r13, r14+0
	bgeu r13, r15, .LBB19_95
.LBB19_84:
	addi r16, r0, 0
	addi r17, r0, 11
	jal r0, .LBB19_86
.LBB19_85:
	addi r13, r13, 24
	bgeu r13, r15, .LBB19_95
.LBB19_86:
	ldbu r1, r13+8
	andi r3, r1, 15
	ldbu r1, r13+9
	andi r1, r1, 64
	beq r3, r16, .LBB19_93
.LBB19_87:
	beq r1, r16, .LBB19_90
.LBB19_88:
	ldw r4, r13+16
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r16, .LBB19_90
.LBB19_89:
	add r3, r12, r0
	jal r31, reallymarkobject
.LBB19_90:
	ldbu r1, r13+8
	andi r1, r1, 64
	beq r1, r16, .LBB19_85
.LBB19_91:
	ldw r4, r13+0
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r16, .LBB19_85
.LBB19_92:
	add r3, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB19_85
.LBB19_93:
	beq r1, r16, .LBB19_85
.LBB19_94:
	stb r13+9, r17
	jal r0, .LBB19_85
.LBB19_95:
	ldbu r1, r11+5
	andi r3, r1, 7
	addi r4, r0, 6
	beq r3, r4, .LBB19_122
.LBB19_96:
	addi r4, r0, 5
	bne r3, r4, .LBB19_157
.LBB19_97:
	ldbu r3, r11+4
	addi r3, r3, -5
	slli r3, r3, 2
	lui r4, %hi(.LJTI19_3)
	addi r4, r4, %lo(.LJTI19_3)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB19_98:
	addi r14, r11, 8
	jal r0, .LBB19_151
.LBB19_99:
	ldbu r1, r12+65
	beq r1, r14, .LBB19_48
.LBB19_100:
	bne r13, r14, .LBB19_103
.LBB19_101:
	addi r1, r0, 1
	jal r0, .LBB19_161
.LBB19_102:
	addi r13, r13, 12
.LBB19_103:
	ldw r1, r11+12
	bgeu r13, r1, .LBB19_107
.LBB19_104:
	ldbu r1, r13+8
	andi r1, r1, 64
	beq r1, r14, .LBB19_102
.LBB19_105:
	ldw r4, r13+0
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r14, .LBB19_102
.LBB19_106:
	add r3, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB19_102
.LBB19_107:
	ldw r13, r11+32
	bne r13, r14, .LBB19_115
.LBB19_108:
	ldbu r1, r12+65
	addi r3, r0, 2
	bne r1, r3, .LBB19_121
.LBB19_109:
	ldbu r1, r12+71
	bne r1, r14, .LBB19_111
.LBB19_110:
	add r3, r11, r0
	jal r31, luaD_shrinkstack
.LBB19_111:
	ldw r1, r11+12
.LBB19_112:
	ldw r3, r11+24
	addi r3, r3, 60
	bgeu r1, r3, .LBB19_118
.LBB19_113:
	stb r1+8, r14
	addi r1, r1, 12
	jal r0, .LBB19_112
.LBB19_114:
	ldw r13, r13+12
	beq r13, r14, .LBB19_108
.LBB19_115:
	ldbu r1, r13+5
	andi r1, r1, 24
	beq r1, r14, .LBB19_114
.LBB19_116:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, reallymarkobject
	jal r0, .LBB19_114
.LBB19_117:
	addi r4, r0, 252
	jal r0, .LBB19_147
.LBB19_118:
	ldw r1, r11+44
	bne r1, r11, .LBB19_121
.LBB19_119:
	ldw r1, r11+32
	beq r1, r14, .LBB19_121
.LBB19_120:
	ldw r1, r12+144
	stw r11+44, r1
	stw r12+144, r11
.LBB19_121:
	ldw r1, r11+24
	ldw r3, r11+28
	sub r1, r1, r3
	srai r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r1, r1, r3
	jal r0, .LBB19_160
.LBB19_122:
	andi r1, r1, 252
	jal r0, .LBB19_156
.LBB19_123:
	ldw r13, r11+16
	ldbu r1, r11+7
	addi r3, r0, 24
	sll r1, r3, r1
	add r14, r13, r1
	ldw r1, r11+8
	sne r1, r1, r15
	addi r16, r0, 11
	addi r17, r0, 1
	addi r18, r0, 4
	jal r0, .LBB19_126
.LBB19_124:
	add r1, r19, r0
.LBB19_125:
	addi r13, r13, 24
	bgeu r13, r14, .LBB19_140
.LBB19_126:
	add r19, r1, r0
	ldbu r1, r13+8
	andi r3, r1, 15
	ldbu r1, r13+9
	andi r1, r1, 64
	beq r3, r15, .LBB19_137
.LBB19_127:
	beq r1, r15, .LBB19_130
.LBB19_128:
	ldw r4, r13+16
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r15, .LBB19_130
.LBB19_129:
	add r3, r12, r0
	jal r31, reallymarkobject
.LBB19_130:
	add r1, r17, r0
	bne r19, r15, .LBB19_125
.LBB19_131:
	ldbu r1, r13+8
	andi r1, r1, 64
	add r4, r15, r0
	beq r1, r15, .LBB19_133
.LBB19_132:
	ldw r4, r13+0
.LBB19_133:
	add r1, r15, r0
	beq r4, r15, .LBB19_125
.LBB19_134:
	ldbu r1, r4+4
	andi r1, r1, 15
	ldbu r3, r4+5
	andi r3, r3, 24
	bne r1, r18, .LBB19_139
.LBB19_135:
	add r1, r15, r0
	beq r3, r15, .LBB19_125
.LBB19_136:
	add r3, r12, r0
	jal r31, reallymarkobject
	add r1, r15, r0
	jal r0, .LBB19_125
.LBB19_137:
	beq r1, r15, .LBB19_124
.LBB19_138:
	stb r13+9, r16
	jal r0, .LBB19_124
.LBB19_139:
	sne r1, r3, r15
	jal r0, .LBB19_125
.LBB19_140:
	ldbu r3, r12+65
	addi r4, r0, 2
	bne r3, r4, .LBB19_153
.LBB19_141:
	addi r3, r0, 0
	beq r1, r3, .LBB19_153
.LBB19_142:
	ldw r1, r12+96
	stw r11+28, r1
	stw r12+96, r11
	jal r0, .LBB19_154
.LBB19_143:
	addi r4, r0, 16
	jal r0, .LBB19_146
.LBB19_144:
	addi r4, r0, 80
	jal r0, .LBB19_146
.LBB19_145:
	addi r4, r0, 40
.LBB19_146:
	add r4, r11, r4
	ldw r5, r12+92
	stw r4+0, r5
	stw r12+92, r11
	addi r4, r0, 199
.LBB19_147:
	and r3, r3, r4
	stb r11+5, r3
	jal r0, .LBB19_160
.LBB19_148:
	addi r14, r11, 80
	jal r0, .LBB19_151
.LBB19_149:
	addi r14, r11, 40
	jal r0, .LBB19_151
.LBB19_150:
	addi r14, r11, 28
.LBB19_151:
	ldw r3, r12+92
	stw r14+0, r3
	stw r12+92, r11
	jal r0, .LBB19_155
.LBB19_152:
	addi r5, r0, 0
	add r3, r12, r0
	add r4, r11, r0
	jal r31, traverseephemeron
	jal r0, .LBB19_157
.LBB19_153:
	ldw r1, r12+92
	stw r11+28, r1
	stw r12+92, r11
.LBB19_154:
	ldbu r1, r11+5
.LBB19_155:
	andi r1, r1, 199
.LBB19_156:
	stb r11+5, r1
.LBB19_157:
	ldw r1, r11+8
	ldw r4, r11+20
	addi r3, r0, 0
	beq r4, r3, .LBB19_159
.LBB19_158:
	ldbu r3, r11+7
	addi r4, r0, 2
	sll r3, r4, r3
.LBB19_159:
	add r1, r1, r3
.LBB19_160:
	addi r1, r1, 1
.LBB19_161:
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
.LBB19_162:
.LBB19_163:
.LBB19_164:
.Lfunc_end19:
	.size	propagatemark, .Lfunc_end19-propagatemark
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI19_0:
	.word	.LBB19_5
	.word	.LBB19_1
	.word	.LBB19_2
	.word	.LBB19_4
	.word	.LBB19_162
	.word	.LBB19_3
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_162
	.word	.LBB19_1
.LJTI19_1:
	.word	.LBB19_7
	.word	.LBB19_38
	.word	.LBB19_19
	.word	.LBB19_47
	.word	.LBB19_161
	.word	.LBB19_49
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_161
	.word	.LBB19_32
.LJTI19_2:
	.word	.LBB19_146
	.word	.LBB19_31
	.word	.LBB19_143
	.word	.LBB19_145
	.word	.LBB19_164
	.word	.LBB19_144
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_164
	.word	.LBB19_31
.LJTI19_3:
	.word	.LBB19_150
	.word	.LBB19_98
	.word	.LBB19_151
	.word	.LBB19_149
	.word	.LBB19_163
	.word	.LBB19_148
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_163
	.word	.LBB19_98
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function traverseephemeron
	.type	traverseephemeron,@function
traverseephemeron:                      # @traverseephemeron
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
	add r14, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	add r3, r4, r0
	jal r31, luaH_realasize
	add r15, r1, r0
	ldbu r1, r11+7
	addi r18, r0, 1
	sll r16, r18, r1
	addi r17, r0, 0
	add r13, r17, r0
	beq r15, r17, .LBB20_6
.LBB20_1:
	addi r19, r0, 0
	add r20, r19, r0
	add r13, r19, r0
	jal r0, .LBB20_3
.LBB20_2:
	addi r15, r15, -1
	addi r20, r20, 12
	beq r15, r19, .LBB20_6
.LBB20_3:
	ldw r1, r11+12
	add r1, r1, r20
	ldbu r3, r1+8
	andi r3, r3, 64
	beq r3, r19, .LBB20_2
.LBB20_4:
	ldw r4, r1+0
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r19, .LBB20_2
.LBB20_5:
	add r3, r12, r0
	jal r31, reallymarkobject
	add r13, r18, r0
	jal r0, .LBB20_2
.LBB20_6:
	addi r15, r0, 11
	addi r18, r0, 1
	addi r19, r0, 4
	addi r20, r0, -1
	addi r21, r0, 24
	mul r22, r16, r21
	add r24, r17, r0
	add r26, r17, r0
	add r23, r17, r0
	add r25, r17, r0
	jal r0, .LBB20_9
.LBB20_7:
	add r26, r18, r0
.LBB20_8:
	addi r25, r25, 1
	addi r16, r16, -1
	addi r24, r24, 24
	beq r16, r17, .LBB20_27
.LBB20_9:
	beq r14, r17, .LBB20_11
.LBB20_10:
	ldw r1, r11+16
	xor r3, r25, r20
	add r1, r1, r22
	mul r3, r3, r21
	add r27, r1, r3
	jal r0, .LBB20_12
.LBB20_11:
	ldw r1, r11+16
	add r27, r1, r24
.LBB20_12:
	ldbu r1, r27+8
	andi r3, r1, 15
	ldbu r1, r27+9
	andi r1, r1, 64
	beq r3, r17, .LBB20_19
.LBB20_13:
	add r4, r17, r0
	beq r1, r17, .LBB20_15
.LBB20_14:
	ldw r4, r27+16
.LBB20_15:
	beq r4, r17, .LBB20_24
.LBB20_16:
	ldbu r1, r4+4
	andi r3, r1, 15
	ldbu r1, r4+5
	andi r1, r1, 24
	bne r3, r19, .LBB20_21
.LBB20_17:
	beq r1, r17, .LBB20_24
.LBB20_18:
	add r3, r12, r0
	jal r31, reallymarkobject
	jal r0, .LBB20_24
.LBB20_19:
	beq r1, r17, .LBB20_8
.LBB20_20:
	stb r27+9, r15
	jal r0, .LBB20_8
.LBB20_21:
	beq r1, r17, .LBB20_24
.LBB20_22:
	ldbu r1, r27+8
	andi r1, r1, 64
	beq r1, r17, .LBB20_7
.LBB20_23:
	ldw r1, r27+0
	ldbu r1, r1+5
	andi r1, r1, 24
	seq r1, r1, r17
	sub r1, r17, r1
	xori r3, r23, 1
	and r1, r3, r1
	xori r23, r1, 1
	jal r0, .LBB20_7
.LBB20_24:
	ldbu r1, r27+8
	andi r1, r1, 64
	beq r1, r17, .LBB20_8
.LBB20_25:
	ldw r4, r27+0
	ldbu r1, r4+5
	andi r1, r1, 24
	beq r1, r17, .LBB20_8
.LBB20_26:
	add r3, r12, r0
	jal r31, reallymarkobject
	add r13, r18, r0
	jal r0, .LBB20_8
.LBB20_27:
	ldbu r3, r12+65
	addi r1, r0, 0
	beq r3, r1, .LBB20_30
.LBB20_28:
	beq r23, r1, .LBB20_31
.LBB20_29:
	ldw r1, r12+100
	stw r11+28, r1
	stw r12+100, r11
	jal r0, .LBB20_33
.LBB20_30:
	ldw r1, r12+92
	stw r11+28, r1
	stw r12+92, r11
	jal r0, .LBB20_33
.LBB20_31:
	beq r26, r1, .LBB20_37
.LBB20_32:
	ldw r1, r12+104
	stw r11+28, r1
	stw r12+104, r11
.LBB20_33:
	ldbu r1, r11+5
.LBB20_34:
	andi r1, r1, 199
.LBB20_35:
	stb r11+5, r1
.LBB20_36:
	add r1, r13, r0
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
.LBB20_37:
	ldbu r1, r11+5
	andi r3, r1, 7
	addi r4, r0, 6
	beq r3, r4, .LBB20_41
.LBB20_38:
	addi r4, r0, 5
	bne r3, r4, .LBB20_36
.LBB20_39:
	ldbu r4, r11+4
	addi r3, r0, 28
	addi r4, r4, -5
	slli r4, r4, 2
	lui r5, %hi(.LJTI20_0)
	addi r5, r5, %lo(.LJTI20_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB20_40:
	addi r3, r0, 8
	jal r0, .LBB20_45
.LBB20_41:
	andi r1, r1, 252
	jal r0, .LBB20_35
.LBB20_42:
	addi r3, r0, 16
	jal r0, .LBB20_45
.LBB20_43:
	addi r3, r0, 80
	jal r0, .LBB20_45
.LBB20_44:
	addi r3, r0, 40
.LBB20_45:
	add r3, r11, r3
	ldw r4, r12+92
	stw r3+0, r4
	stw r12+92, r11
	jal r0, .LBB20_34
.LBB20_46:
.Lfunc_end20:
	.size	traverseephemeron, .Lfunc_end20-traverseephemeron
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI20_0:
	.word	.LBB20_45
	.word	.LBB20_40
	.word	.LBB20_42
	.word	.LBB20_44
	.word	.LBB20_46
	.word	.LBB20_43
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_46
	.word	.LBB20_40
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function finishgencycle
	.type	finishgencycle,@function
finishgencycle:                         # @finishgencycle
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
	addi r3, r4, 92
	jal r31, correctgraylist
	ldw r3, r12+96
	stw r1+0, r3
	addi r13, r0, 0
	stw r12+96, r13
	add r3, r1, r0
	jal r31, correctgraylist
	ldw r3, r12+104
	stw r1+0, r3
	stw r12+104, r13
	add r3, r1, r0
	jal r31, correctgraylist
	ldw r3, r12+100
	stw r1+0, r3
	stw r12+100, r13
	add r3, r1, r0
	jal r31, correctgraylist
	ldbu r1, r12+71
	bne r1, r13, .LBB21_3
.LBB21_1:
	ldw r3, r12+28
	ldw r1, r12+32
	srai r4, r1, 31
	srli r4, r4, 30
	add r4, r1, r4
	srai r4, r4, 2
	bge r3, r4, .LBB21_3
.LBB21_2:
	ldw r14, r12+12
	srli r3, r1, 31
	add r1, r1, r3
	srai r4, r1, 1
	add r3, r11, r0
	jal r31, luaS_resize
	ldw r1, r12+12
	sub r1, r1, r14
	ldw r3, r12+16
	add r1, r1, r3
	stw r12+16, r1
.LBB21_3:
	stb r12+65, r13
	ldbu r1, r12+71
	beq r1, r13, .LBB21_5
.LBB21_4:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB21_5:
	ldw r12, r11+16
	ldw r1, r12+108
	beq r1, r13, .LBB21_4
.LBB21_6:
	add r3, r11, r0
	jal r31, GCTM
	ldw r1, r12+108
	bne r1, r13, .LBB21_6
	jal r0, .LBB21_4
.Lfunc_end21:
	.size	finishgencycle, .Lfunc_end21-finishgencycle
                                        # -- End function
	.p2align	2                               # -- Begin function correctgraylist
	.type	correctgraylist,@function
correctgraylist:                        # @correctgraylist
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	add r1, r3, r0
	ldw r9, r3+0
	addi r3, r0, 0
	beq r9, r3, .LBB22_19
.LBB22_1:
	addi r4, r0, 33
	addi r5, r0, 5
	addi r6, r0, 8
	addi r7, r0, 6
	lui r8, %hi(.LJTI22_0)
	addi r8, r8, %lo(.LJTI22_0)
	jal r0, .LBB22_5
.LBB22_2:
	ldbu r11, r9+5
	ori  r11, r11, 32
	stb r9+5, r11
.LBB22_3:
	ldw r9, r10+0
	stw r1+0, r9
.LBB22_4:
	ldw r9, r1+0
	beq r9, r3, .LBB22_19
.LBB22_5:
	ldbu r11, r9+4
	addi r12, r11, -5
	add r10, r3, r0
	bgtu r12, r4, .LBB22_12
.LBB22_6:
	slli r10, r12, 2
	add r10, r8, r10
	ldw r12, r10+0
	add r10, r3, r0
	jalr r0, r12, 0
.LBB22_7:
	addi r10, r9, 8
	jal r0, .LBB22_12
.LBB22_8:
	addi r10, r9, 40
	jal r0, .LBB22_12
.LBB22_9:
	addi r10, r9, 28
	jal r0, .LBB22_12
.LBB22_10:
	addi r10, r9, 16
	jal r0, .LBB22_12
.LBB22_11:
	addi r10, r9, 80
.LBB22_12:
	ldbu r12, r9+5
	andi r13, r12, 24
	bne r13, r3, .LBB22_3
.LBB22_13:
	andi r13, r12, 7
	bne r13, r5, .LBB22_15
.LBB22_14:
	andi r1, r12, 223
	xori r1, r1, 35
	stb r9+5, r1
	add r1, r10, r0
	jal r0, .LBB22_4
.LBB22_15:
	bne r11, r6, .LBB22_17
.LBB22_16:
	add r1, r10, r0
	jal r0, .LBB22_4
.LBB22_17:
	bne r13, r7, .LBB22_2
.LBB22_18:
	andi r11, r12, 228
	stb r9+5, r11
	jal r0, .LBB22_2
.LBB22_19:
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end22:
	.size	correctgraylist, .Lfunc_end22-correctgraylist
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI22_0:
	.word	.LBB22_9
	.word	.LBB22_7
	.word	.LBB22_10
	.word	.LBB22_8
	.word	.LBB22_12
	.word	.LBB22_11
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_12
	.word	.LBB22_7
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function GCTM
	.type	GCTM,@function
GCTM:                                   # @GCTM
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
	stw fp+-28, lr
	add r11, r3, r0
	ldw r14, r3+16
	ldw r1, r14+108
	ldw r3, r1+0
	stw r14+108, r3
	ldw r3, r14+76
	stw r1+0, r3
	stw r14+76, r1
	ldbu r3, r1+5
	andi r4, r3, 191
	stb r1+5, r4
	ldbu r4, r14+65
	addi r4, r4, -3
	andi r5, r4, 255
	addi r4, r0, 3
	bgtu r5, r4, .LBB23_2
.LBB23_1:
	andi r3, r3, 135
	ldbu r4, r14+64
	andi r4, r4, 24
	or  r3, r4, r3
	stb r1+5, r3
	jal r0, .LBB23_4
.LBB23_2:
	andi r3, r3, 7
	bne r3, r4, .LBB23_4
.LBB23_3:
	stw r14+128, r1
.LBB23_4:
	addi r13, fp, -40
	stw r13+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r13+8, r1
	addi r5, r0, 2
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaT_gettmbyobj
	ldbu r3, r1+8
	andi r3, r3, 15
	addi r12, r0, 0
	beq r3, r12, .LBB23_6
.LBB23_5:
	ldbu r15, r11+7
	ldbu r16, r14+70
	ori  r3, r16, 2
	stb r14+70, r3
	stb r11+7, r12
	ldw r3, r11+12
	addi r4, r3, 12
	stw r11+12, r4
	ldw r4, r1+0
	ldw r5, r1+4
	stw r3+4, r5
	stw r3+0, r4
	ldbu r1, r1+8
	stb r3+8, r1
	ldw r1, r11+12
	addi r3, r1, 12
	stw r11+12, r3
	ldw r3, r13+0
	ldw r4, r13+4
	stw r1+4, r4
	stw r1+0, r3
	ldbu r3, r13+8
	stb r1+8, r3
	ldw r1, r11+20
	ldhu r3, r1+34
	ori  r3, r3, 128
	sth r1+34, r3
	ldw r1, r11+12
	ldw r3, r11+28
	sub r1, r1, r3
	addi r6, r1, -24
	lui r4, %hi(dothecall)
	addi r4, r4, %lo(dothecall)
	add r3, r11, r0
	add r5, r12, r0
	add r7, r12, r0
	jal r31, luaD_pcall
	ldw r3, r11+20
	ldhu r4, r3+34
	lui r5, 16
	addi r5, r5, -129
	and r4, r4, r5
	sth r3+34, r4
	stb r11+7, r15
	stb r14+70, r16
	bne r1, r12, .LBB23_7
.LBB23_6:
	ldw lr, fp+-28
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
.LBB23_7:
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	add r3, r11, r0
	jal r31, luaE_warnerror
	ldw r1, r11+12
	addi r1, r1, -12
	stw r11+12, r1
	jal r0, .LBB23_6
.Lfunc_end23:
	.size	GCTM, .Lfunc_end23-GCTM
                                        # -- End function
	.p2align	2                               # -- Begin function dothecall
	.type	dothecall,@function
dothecall:                              # @dothecall
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r3+12
	addi r4, r1, -24
	addi r5, r0, 0
	jal r31, luaD_callnoyield
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end24:
	.size	dothecall, .Lfunc_end24-dothecall
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"__gc"
	.size	.L.str, 5

	.type	sweepgen.nextage,@object        # @sweepgen.nextage
	.section	.rodata,"a",@progbits
sweepgen.nextage:
	.ascii	"\001\003\003\004\004\005\006"
	.size	sweepgen.nextage, 7

	.hidden	luaM_malloc_
	.hidden	luaT_gettm
	.hidden	luaE_setdebt
	.hidden	luaF_freeproto
	.hidden	luaM_free_
	.hidden	luaH_free
	.hidden	luaE_freethread
	.hidden	luaS_remove
	.hidden	luaF_unlinkupval
	.hidden	luaS_clearcache
	.hidden	luaH_realasize
	.hidden	luaD_shrinkstack
	.hidden	luaS_resize
	.hidden	luaT_gettmbyobj
	.hidden	luaD_pcall
	.hidden	luaE_warnerror
	.hidden	luaD_callnoyield
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
