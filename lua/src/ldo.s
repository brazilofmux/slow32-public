	.file	"ldo.c"
	.text
	.hidden	luaD_seterrorobj                # -- Begin function luaD_seterrorobj
	.globl	luaD_seterrorobj
	.p2align	2
	.type	luaD_seterrorobj,@function
luaD_seterrorobj:                       # @luaD_seterrorobj
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	addi r1, r0, 0
	beq r4, r1, .LBB0_7
.LBB0_1:
	addi r1, r0, 5
	beq r4, r1, .LBB0_4
.LBB0_2:
	addi r1, r0, 4
	bne r4, r1, .LBB0_6
.LBB0_3:
	ldw r1, r3+16
	ldw r1, r1+156
	stw r5+0, r1
	jal r0, .LBB0_5
.LBB0_4:
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	addi r1, r0, 23
	add r11, r3, r0
	add r12, r5, r0
	add r5, r1, r0
	jal r31, luaS_newlstr
	add r3, r11, r0
	add r5, r12, r0
	stw r12+0, r1
.LBB0_5:
	ldbu r1, r1+4
	ori  r1, r1, 64
	jal r0, .LBB0_7
.LBB0_6:
	ldw r1, r3+12
	ldw r4, r1+-12
	ldw r6, r1+-8
	stw r5+4, r6
	stw r5+0, r4
	ldbu r1, r1+-4
.LBB0_7:
	stb r5+8, r1
	addi r1, r5, 12
	stw r3+12, r1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaD_seterrorobj, .Lfunc_end0-luaD_seterrorobj
                                        # -- End function
	.hidden	luaD_throw                      # -- Begin function luaD_throw
	.globl	luaD_throw
	.p2align	2
	.type	luaD_throw,@function
luaD_throw:                             # @luaD_throw
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
	ldw r1, r3+48
	addi r13, r0, 0
	bne r1, r13, .LBB1_3
.LBB1_1:
	add r11, r3, r0
	ldw r12, r3+16
	jal r31, luaE_resetthread
	ldw r3, r12+152
	ldw r4, r3+48
	beq r4, r13, .LBB1_4
.LBB1_2:
	ldw r4, r3+12
	addi r5, r4, 12
	stw r3+12, r5
	ldw r3, r11+12
	ldw r5, r3+-12
	ldw r6, r3+-8
	stw r4+4, r6
	stw r4+0, r5
	ldbu r3, r3+-4
	stb r4+8, r3
	ldw r3, r12+152
	add r4, r1, r0
	jal r31, luaD_throw
.LBB1_3:
	stw r1+88, r4
	addi r3, r1, 4
	addi r4, r0, 1
	jal r31, longjmp
.LBB1_4:
	ldw r1, r12+148
	beq r1, r13, .LBB1_6
.LBB1_5:
	add r3, r11, r0
	jalr lr, r1, 0
.LBB1_6:
	jal r31, abort
.Lfunc_end1:
	.size	luaD_throw, .Lfunc_end1-luaD_throw
                                        # -- End function
	.hidden	luaD_rawrunprotected            # -- Begin function luaD_rawrunprotected
	.globl	luaD_rawrunprotected
	.p2align	2
	.type	luaD_rawrunprotected,@function
luaD_rawrunprotected:                   # @luaD_rawrunprotected
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
	stw fp+-28, lr
	add r13, r5, r0
	add r12, r4, r0
	add r11, r3, r0
	ldw r14, r3+96
	addi r16, r0, 0
	addi r15, fp, -120
	stw r15+88, r16
	ldw r1, r3+48
	stw r15+0, r1
	stw r3+48, r15
	addi r3, r15, 4
	jal r31, setjmp
	bne r1, r16, .LBB2_2
.LBB2_1:
	add r3, r11, r0
	add r4, r13, r0
	jalr lr, r12, 0
.LBB2_2:
	ldw r1, r15+0
	stw r11+48, r1
	stw r11+96, r14
	ldw r1, r15+88
	ldw lr, fp+-28
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
.Lfunc_end2:
	.size	luaD_rawrunprotected, .Lfunc_end2-luaD_rawrunprotected
                                        # -- End function
	.hidden	luaD_reallocstack               # -- Begin function luaD_reallocstack
	.globl	luaD_reallocstack
	.p2align	2
	.type	luaD_reallocstack,@function
luaD_reallocstack:                      # @luaD_reallocstack
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
	add r14, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	ldw r1, r3+24
	ldw r3, r3+28
	ldw r4, r12+16
	ldbu r18, r4+67
	ldw r4, r12+12
	sub r4, r4, r3
	stw r12+12, r4
	ldw r4, r12+36
	sub r4, r4, r3
	stw r12+36, r4
	ldw r4, r12+32
	addi r11, r0, 0
	beq r4, r11, .LBB3_2
.LBB3_1:
	ldw r5, r4+8
	ldw r6, r12+28
	sub r5, r5, r6
	stw r4+8, r5
	ldw r4, r4+12
	bne r4, r11, .LBB3_1
.LBB3_2:
	sub r19, r1, r3
	ldw r1, r12+20
	beq r1, r11, .LBB3_4
.LBB3_3:
	ldw r3, r1+4
	ldw r4, r12+28
	sub r3, r3, r4
	stw r1+4, r3
	ldw r3, r1+0
	ldw r4, r12+28
	sub r3, r3, r4
	stw r1+0, r3
	ldw r1, r1+8
	bne r1, r11, .LBB3_3
.LBB3_4:
	ldw r1, r12+16
	addi r15, r0, 1
	stb r1+67, r15
	ldw r4, r12+28
	addi r5, r19, 60
	addi r16, r13, 5
	addi r17, r0, 12
	mul r6, r16, r17
	add r3, r12, r0
	jal r31, luaM_realloc_
	ldw r3, r12+16
	stb r3+67, r18
	beq r1, r11, .LBB3_16
.LBB3_5:
	srai r3, r19, 2
	lui r4, 699051
	addi r4, r4, -1365
	stw r12+28, r1
	ldw r5, r12+12
	add r5, r1, r5
	stw r12+12, r5
	ldw r5, r12+36
	add r5, r1, r5
	stw r12+36, r5
	ldw r5, r12+32
	beq r5, r11, .LBB3_7
.LBB3_6:
	ldw r6, r12+28
	ldw r7, r5+8
	add r6, r6, r7
	stw r5+8, r6
	ldw r5, r5+12
	bne r5, r11, .LBB3_6
.LBB3_7:
	mul r3, r3, r4
	ldw r4, r12+20
	bne r4, r11, .LBB3_14
.LBB3_8:
	ldw r4, r12+28
	mul r5, r13, r17
	add r4, r4, r5
	stw r12+24, r4
	bge r3, r13, .LBB3_11
.LBB3_9:
	addi r4, r3, 5
	mul r3, r3, r17
	add r1, r3, r1
	addi r1, r1, 68
.LBB3_10:
	stb r1+0, r11
	addi r4, r4, 1
	addi r1, r1, 12
	blt r4, r16, .LBB3_10
.LBB3_11:
	add r11, r15, r0
.LBB3_12:
	add r1, r11, r0
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
.LBB3_13:
	ldw r4, r4+8
	beq r4, r11, .LBB3_8
.LBB3_14:
	ldw r5, r12+28
	ldw r6, r4+4
	add r5, r5, r6
	stw r4+4, r5
	ldw r5, r12+28
	ldw r6, r4+0
	add r5, r5, r6
	stw r4+0, r5
	ldbu r5, r4+34
	andi r5, r5, 2
	bne r5, r11, .LBB3_13
.LBB3_15:
	stw r4+20, r15
	jal r0, .LBB3_13
.LBB3_16:
	add r3, r12, r0
	jal r31, correctstack
	beq r14, r11, .LBB3_12
.LBB3_17:
	addi r4, r0, 4
	add r3, r12, r0
	jal r31, luaD_throw
.Lfunc_end3:
	.size	luaD_reallocstack, .Lfunc_end3-luaD_reallocstack
                                        # -- End function
	.p2align	2                               # -- Begin function correctstack
	.type	correctstack,@function
correctstack:                           # @correctstack
# %bb.0:
	ldw r1, r3+28
	ldw r4, r3+12
	add r4, r1, r4
	stw r3+12, r4
	ldw r4, r3+36
	add r1, r1, r4
	stw r3+36, r1
	ldw r4, r3+32
	addi r1, r0, 0
	beq r4, r1, .LBB4_2
.LBB4_1:
	ldw r5, r3+28
	ldw r6, r4+8
	add r5, r5, r6
	stw r4+8, r5
	ldw r4, r4+12
	bne r4, r1, .LBB4_1
.LBB4_2:
	ldw r4, r3+20
	beq r4, r1, .LBB4_7
.LBB4_3:
	addi r5, r0, 1
	jal r0, .LBB4_5
.LBB4_4:
	ldw r4, r4+8
	beq r4, r1, .LBB4_7
.LBB4_5:
	ldw r6, r3+28
	ldw r7, r4+4
	add r6, r6, r7
	stw r4+4, r6
	ldw r6, r3+28
	ldw r7, r4+0
	add r6, r6, r7
	stw r4+0, r6
	ldbu r6, r4+34
	andi r6, r6, 2
	bne r6, r1, .LBB4_4
.LBB4_6:
	stw r4+20, r5
	jal r0, .LBB4_4
.LBB4_7:
	jalr r0, r31, 0
.Lfunc_end4:
	.size	correctstack, .Lfunc_end4-correctstack
                                        # -- End function
	.hidden	luaD_growstack                  # -- Begin function luaD_growstack
	.globl	luaD_growstack
	.p2align	2
	.type	luaD_growstack,@function
luaD_growstack:                         # @luaD_growstack
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
	ldw r1, r3+24
	ldw r3, r3+28
	sub r1, r1, r3
	lui r5, 44
	addi r5, r5, -223
	bge r1, r5, .LBB5_6
.LBB5_1:
	lui r13, 4
	addi r5, r13, -1385
	bgt r4, r5, .LBB5_4
.LBB5_2:
	lui r5, 699051
	addi r5, r5, -1365
	ldw r6, r11+12
	sub r3, r6, r3
	srai r3, r3, 2
	mul r3, r3, r5
	add r14, r3, r4
	addi r3, r13, -1384
	bgt r14, r3, .LBB5_8
.LBB5_3:
	srai r1, r1, 2
	mul r1, r1, r5
	slli r1, r1, 1
	xor r4, r1, r3
	slt r1, r1, r3
	addi r5, r0, 0
	sub r1, r5, r1
	and r1, r4, r1
	xor r1, r1, r3
	xor r3, r1, r14
	sgt r1, r1, r14
	sub r1, r5, r1
	and r1, r3, r1
	xor r4, r14, r1
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaD_reallocstack
	addi r3, r13, -1383
	blt r14, r3, .LBB5_5
.LBB5_4:
	addi r4, r13, -1184
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaD_reallocstack
	addi r1, r0, 0
	bne r12, r1, .LBB5_9
.LBB5_5:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB5_6:
	addi r1, r0, 0
	beq r12, r1, .LBB5_5
.LBB5_7:
	addi r4, r0, 5
	add r3, r11, r0
	jal r31, luaD_throw
.LBB5_8:
                                        # implicit-def: $r1
	addi r3, r13, -1383
	bge r14, r3, .LBB5_4
	jal r0, .LBB5_5
.LBB5_9:
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	add r3, r11, r0
	jal r31, luaG_runerror
.Lfunc_end5:
	.size	luaD_growstack, .Lfunc_end5-luaD_growstack
                                        # -- End function
	.hidden	luaD_shrinkstack                # -- Begin function luaD_shrinkstack
	.globl	luaD_shrinkstack
	.p2align	2
	.type	luaD_shrinkstack,@function
luaD_shrinkstack:                       # @luaD_shrinkstack
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r4, r3+12
	ldw r1, r3+20
	addi r5, r0, 0
	beq r1, r5, .LBB6_2
.LBB6_1:
	ldw r6, r1+4
	sltu r7, r4, r6
	xor r6, r6, r4
	sub r7, r5, r7
	and r6, r6, r7
	xor r4, r4, r6
	ldw r1, r1+8
	bne r1, r5, .LBB6_1
.LBB6_2:
	ldw r1, r3+28
	sub r4, r4, r1
	lui r6, 44
	addi r6, r6, -225
	bgt r4, r6, .LBB6_5
.LBB6_3:
	srai r4, r4, 2
	lui r6, 699051
	addi r6, r6, -1365
	mul r4, r4, r6
	addi r7, r0, 19
	sgt r7, r4, r7
	sub r7, r5, r7
	xori r4, r4, 19
	and r4, r4, r7
	xori r4, r4, 19
	addi r4, r4, 1
	lui r7, 1
	addi r7, r7, 904
	xor r8, r4, r7
	slt r9, r4, r7
	sub r9, r5, r9
	and r8, r8, r9
	xor r7, r8, r7
	slli r8, r7, 1
	add r7, r8, r7
	ldw r8, r3+24
	sub r1, r8, r1
	srai r1, r1, 2
	mul r1, r1, r6
	ble r1, r7, .LBB6_5
.LBB6_4:
	lui r1, 2
	addi r1, r1, -692
	xor r6, r4, r1
	slt r4, r4, r1
	sub r4, r5, r4
	and r4, r6, r4
	xor r1, r4, r1
	slli r4, r1, 1
	add r11, r3, r0
	jal r31, luaD_reallocstack
	add r3, r11, r0
.LBB6_5:
	jal r31, luaE_shrinkCI
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	luaD_shrinkstack, .Lfunc_end6-luaD_shrinkstack
                                        # -- End function
	.hidden	luaD_inctop                     # -- Begin function luaD_inctop
	.globl	luaD_inctop
	.p2align	2
	.type	luaD_inctop,@function
luaD_inctop:                            # @luaD_inctop
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+24
	ldw r4, r3+12
	sub r1, r1, r4
	addi r4, r0, 23
	ble r1, r4, .LBB7_2
.LBB7_1:
	ldw r1, r3+12
	addi r1, r1, 12
	stw r3+12, r1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB7_2:
	addi r4, r0, 1
	add r11, r3, r0
	add r5, r4, r0
	jal r31, luaD_growstack
	add r3, r11, r0
	jal r0, .LBB7_1
.Lfunc_end7:
	.size	luaD_inctop, .Lfunc_end7-luaD_inctop
                                        # -- End function
	.hidden	luaD_hook                       # -- Begin function luaD_hook
	.globl	luaD_hook
	.p2align	2
	.type	luaD_hook,@function
luaD_hook:                              # @luaD_hook
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
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, lr
	ldw r13, r3+88
	addi r14, r0, 0
	beq r13, r14, .LBB8_12
.LBB8_1:
	ldbu r1, r3+7
	beq r1, r14, .LBB8_12
.LBB8_2:
	ldw r12, r3+20
	ldw r16, r3+12
	ldw r17, r3+28
	ldw r18, r12+4
	addi r1, fp, -144
	stw r1+0, r4
	stw r1+24, r5
	stw r1+104, r12
	beq r7, r14, .LBB8_4
.LBB8_3:
	sth r12+28, r6
	sth r12+30, r7
	addi r15, r0, 264
	jal r0, .LBB8_5
.LBB8_4:
	addi r15, r0, 8
.LBB8_5:
	ldbu r1, r12+34
	andi r1, r1, 2
	bne r1, r14, .LBB8_8
.LBB8_6:
	ldw r4, r3+12
	ldw r1, r12+4
	bgeu r4, r1, .LBB8_8
.LBB8_7:
	stw r3+12, r1
.LBB8_8:
	ldw r1, r3+24
	ldw r4, r3+12
	sub r1, r1, r4
	addi r4, r0, 251
	ble r1, r4, .LBB8_13
.LBB8_9:
	sub r16, r16, r17
	sub r17, r18, r17
	ldw r4, r12+4
	ldw r1, r3+12
	addi r1, r1, 240
	bgeu r4, r1, .LBB8_11
.LBB8_10:
	stw r12+4, r1
.LBB8_11:
	stb r3+7, r14
	ldhu r1, r12+34
	or  r1, r1, r15
	sth r12+34, r1
	addi r4, fp, -144
	add r11, r3, r0
	jalr lr, r13, 0
	addi r1, r0, 1
	stb r11+7, r1
	ldw r1, r11+28
	add r1, r1, r17
	stw r12+4, r1
	ldw r1, r11+28
	add r1, r1, r16
	stw r11+12, r1
	ldhu r1, r12+34
	addi r3, r0, -1
	xor r3, r15, r3
	and r1, r1, r3
	sth r12+34, r1
.LBB8_12:
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
	addi sp, sp, 152
	jalr r0, r31, 0
.LBB8_13:
	addi r4, r0, 20
	addi r5, r0, 1
	add r11, r3, r0
	jal r31, luaD_growstack
	add r3, r11, r0
	jal r0, .LBB8_9
.Lfunc_end8:
	.size	luaD_hook, .Lfunc_end8-luaD_hook
                                        # -- End function
	.hidden	luaD_hookcall                   # -- Begin function luaD_hookcall
	.globl	luaD_hookcall
	.p2align	2
	.type	luaD_hookcall,@function
luaD_hookcall:                          # @luaD_hookcall
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	addi r1, r0, 0
	stw r3+100, r1
	ldw r5, r3+112
	andi r5, r5, 1
	beq r5, r1, .LBB9_2
.LBB9_1:
	ldhu r1, r4+34
	srli r1, r1, 3
	andi r1, r1, 4
	ldw r5, r4+0
	ldw r5, r5+0
	ldw r5, r5+12
	ldw r6, r4+16
	addi r6, r6, 4
	stw r4+16, r6
	ldbu r7, r5+6
	addi r5, r0, -1
	addi r6, r0, 1
	add r11, r4, r0
	add r4, r1, r0
	jal r31, luaD_hook
	ldw r1, r11+16
	addi r1, r1, -4
	stw r11+16, r1
.LBB9_2:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end9:
	.size	luaD_hookcall, .Lfunc_end9-luaD_hookcall
                                        # -- End function
	.hidden	luaD_poscall                    # -- Begin function luaD_poscall
	.globl	luaD_poscall
	.p2align	2
	.type	luaD_poscall,@function
luaD_poscall:                           # @luaD_poscall
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
	ldh r14, r4+32
	lui r16, 16
	addi r18, r16, -1
	and r17, r14, r18
	ldw r1, r3+112
	addi r15, r0, 0
	beq r1, r15, .LBB10_2
.LBB10_1:
	addi r1, r0, -1
	bge r14, r1, .LBB10_12
.LBB10_2:
	ldw r1, r4+0
	beq r17, r18, .LBB10_13
.LBB10_3:
	addi r6, r0, 1
	beq r17, r6, .LBB10_9
.LBB10_4:
	beq r17, r15, .LBB10_21
.LBB10_5:
	addi r13, r0, -2
	bgt r14, r13, .LBB10_14
.LBB10_6:
	add r17, r4, r0
	ldw r4, r3+20
	ldhu r6, r4+34
	ori  r6, r6, 512
	sth r4+34, r6
	add r11, r5, r0
	stw r4+28, r5
	addi r5, r0, -1
	addi r6, r0, 1
	add r12, r3, r0
	add r4, r1, r0
	jal r31, luaF_close
	add r3, r12, r0
	ldw r4, r12+20
	ldhu r5, r4+34
	addi r6, r16, -513
	and r5, r5, r6
	sth r4+34, r5
	ldw r5, r12+112
	beq r5, r15, .LBB10_8
.LBB10_7:
	ldw r3, r3+28
	sub r16, r1, r3
	add r3, r12, r0
	add r5, r11, r0
	jal r31, rethook
	add r3, r12, r0
	ldw r1, r12+28
	add r1, r1, r16
.LBB10_8:
	add r4, r17, r0
	addi r5, r0, -3
	sub r6, r5, r14
	seq r5, r14, r13
	sub r7, r15, r5
	add r5, r11, r0
	xor r8, r11, r6
	and r7, r8, r7
	xor r14, r6, r7
	jal r0, .LBB10_14
.LBB10_9:
	beq r5, r15, .LBB10_11
.LBB10_10:
	ldw r6, r3+12
	addi r7, r0, 0
	sub r5, r7, r5
	addi r7, r0, 12
	mul r5, r5, r7
	add r5, r6, r5
	ldw r6, r5+0
	ldw r7, r5+4
	stw r1+4, r7
	stw r1+0, r6
	ldbu r15, r5+8
.LBB10_11:
	stb r1+8, r15
	addi r1, r1, 12
	jal r0, .LBB10_21
.LBB10_12:
	add r11, r3, r0
	add r12, r4, r0
	add r13, r5, r0
	jal r31, rethook
	add r5, r13, r0
	add r4, r12, r0
	add r3, r11, r0
	ldw r1, r4+0
	bne r17, r18, .LBB10_3
.LBB10_13:
	add r14, r5, r0
.LBB10_14:
	xor r7, r5, r14
	slt r8, r5, r14
	addi r6, r0, 0
	sub r8, r6, r8
	and r7, r7, r8
	xor r7, r14, r7
	addi r8, r0, 1
	blt r7, r8, .LBB10_17
.LBB10_15:
	ldw r6, r3+12
	addi r8, r0, 12
	mul r5, r5, r8
	sub r5, r6, r5
	addi r5, r5, 8
	addi r8, r1, 8
	addi r6, r0, 0
.LBB10_16:
	ldw r9, r5+-8
	ldw r10, r5+-4
	stw r8+-4, r10
	stw r8+-8, r9
	ldbu r9, r5+0
	stb r8+0, r9
	addi r6, r6, 1
	addi r5, r5, 12
	addi r8, r8, 12
	bne r7, r6, .LBB10_16
.LBB10_17:
	ble r14, r6, .LBB10_20
.LBB10_18:
	sub r5, r14, r6
	addi r7, r0, 12
	mul r6, r6, r7
	add r6, r6, r1
	addi r6, r6, 8
	addi r7, r0, 0
.LBB10_19:
	stb r6+0, r7
	addi r5, r5, -1
	addi r6, r6, 12
	bne r5, r7, .LBB10_19
.LBB10_20:
	addi r5, r0, 12
	mul r5, r14, r5
	add r1, r1, r5
.LBB10_21:
	stw r3+12, r1
	ldw r1, r4+8
	stw r3+20, r1
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
.Lfunc_end10:
	.size	luaD_poscall, .Lfunc_end10-luaD_poscall
                                        # -- End function
	.p2align	2                               # -- Begin function rethook
	.type	rethook,@function
rethook:                                # @rethook
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
	ldw r1, r3+112
	andi r1, r1, 2
	addi r13, r0, 0
	beq r1, r13, .LBB11_5
.LBB11_1:
	add r7, r5, r0
	ldw r1, r11+12
	sub r3, r13, r5
	addi r14, r0, 12
	mul r3, r3, r14
	add r1, r1, r3
	ldbu r3, r12+34
	andi r3, r3, 2
	add r15, r13, r0
	bne r3, r13, .LBB11_4
.LBB11_2:
	ldw r3, r12+0
	ldw r3, r3+0
	ldw r3, r3+12
	ldbu r4, r3+7
	addi r15, r0, 0
	beq r4, r15, .LBB11_4
.LBB11_3:
	ldw r4, r12+24
	ldbu r3, r3+6
	add r3, r4, r3
	addi r15, r3, 1
.LBB11_4:
	ldw r3, r12+0
	mul r4, r15, r14
	add r3, r3, r4
	stw r12+0, r3
	sub r1, r1, r3
	srli r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r1, r1, r3
	lui r3, 16
	addi r3, r3, -1
	and r6, r1, r3
	addi r4, r0, 1
	addi r5, r0, -1
	add r3, r11, r0
	jal r31, luaD_hook
	ldw r1, r12+0
	sub r3, r13, r15
	mul r3, r3, r14
	add r1, r1, r3
	stw r12+0, r1
.LBB11_5:
	ldw r1, r12+8
	ldbu r3, r1+34
	andi r3, r3, 2
	addi r4, r0, 0
	bne r3, r4, .LBB11_7
.LBB11_6:
	ldw r3, r1+16
	ldw r1, r1+0
	ldw r1, r1+0
	ldw r1, r1+12
	ldw r1, r1+52
	sub r1, r3, r1
	srai r1, r1, 2
	addi r1, r1, -1
	stw r11+100, r1
.LBB11_7:
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
.Lfunc_end11:
	.size	rethook, .Lfunc_end11-rethook
                                        # -- End function
	.hidden	luaD_pretailcall                # -- Begin function luaD_pretailcall
	.globl	luaD_pretailcall
	.p2align	2
	.type	luaD_pretailcall,@function
luaD_pretailcall:                       # @luaD_pretailcall
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
	add r14, r7, r0
	add r12, r6, r0
	add r1, r5, r0
	add r13, r4, r0
	add r11, r3, r0
	addi r16, r0, 12
	mul r3, r6, r16
	addi r17, r3, 8
	addi r15, r0, 6
	addi r18, r0, 22
	addi r19, r0, 38
.LBB12_1:
	ldbu r3, r1+8
	andi r3, r3, 63
	beq r3, r15, .LBB12_5
.LBB12_2:
	beq r3, r18, .LBB12_14
.LBB12_3:
	beq r3, r19, .LBB12_13
.LBB12_4:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, tryfuncTM
	addi r12, r12, 1
	addi r17, r17, 12
	jal r0, .LBB12_1
.LBB12_5:
	ldw r3, r1+0
	ldw r18, r3+12
	ldbu r19, r18+8
	ldbu r20, r18+6
	ldw r3, r11+24
	ldw r4, r11+12
	sub r3, r3, r4
	srai r3, r3, 2
	lui r4, 699051
	addi r4, r4, -1365
	mul r3, r3, r4
	sub r15, r19, r14
	ble r3, r15, .LBB12_17
.LBB12_6:
	ldw r4, r13+0
	addi r3, r0, 0
	sub r5, r3, r14
	mul r5, r5, r16
	add r4, r4, r5
	stw r13+0, r4
	addi r4, r0, 1
	blt r12, r4, .LBB12_9
.LBB12_7:
	add r4, r3, r0
	add r5, r12, r0
.LBB12_8:
	ldw r6, r13+0
	add r6, r6, r4
	add r7, r1, r4
	ldw r8, r7+0
	ldw r9, r7+4
	stw r6+4, r9
	stw r6+0, r8
	ldbu r7, r7+8
	stb r6+8, r7
	addi r5, r5, -1
	addi r4, r4, 12
	bne r5, r3, .LBB12_8
.LBB12_9:
	ldw r1, r13+0
	bgt r12, r20, .LBB12_12
.LBB12_10:
	addi r4, r20, 1
	add r5, r1, r17
.LBB12_11:
	stb r5+0, r3
	addi r12, r12, 1
	addi r5, r5, 12
	bne r4, r12, .LBB12_11
.LBB12_12:
	mul r3, r19, r16
	add r3, r1, r3
	addi r3, r3, 12
	stw r13+4, r3
	ldw r3, r18+52
	stw r13+16, r3
	ldhu r3, r13+34
	ori  r3, r3, 32
	sth r13+34, r3
	mul r3, r12, r16
	add r1, r1, r3
	stw r11+12, r1
	addi r1, r0, -1
	jal r0, .LBB12_16
.LBB12_13:
	ldw r3, r1+0
	ldw r6, r3+12
	jal r0, .LBB12_15
.LBB12_14:
	ldw r6, r1+0
.LBB12_15:
	addi r5, r0, -1
	add r3, r11, r0
	add r4, r1, r0
	jal r31, precallC
.LBB12_16:
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
.LBB12_17:
	ldw r3, r11+28
	sub r21, r1, r3
	ldw r1, r11+16
	ldw r1, r1+12
	addi r3, r0, 1
	blt r1, r3, .LBB12_19
.LBB12_18:
	add r3, r11, r0
	jal r31, luaC_step
.LBB12_19:
	addi r5, r0, 1
	add r3, r11, r0
	add r4, r15, r0
	jal r31, luaD_growstack
	ldw r1, r11+28
	add r1, r1, r21
	jal r0, .LBB12_6
.Lfunc_end12:
	.size	luaD_pretailcall, .Lfunc_end12-luaD_pretailcall
                                        # -- End function
	.p2align	2                               # -- Begin function precallC
	.type	precallC,@function
precallC:                               # @precallC
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
	add r12, r6, r0
	add r13, r5, r0
	add r15, r4, r0
	add r11, r3, r0
	ldw r1, r3+24
	ldw r3, r3+12
	sub r1, r1, r3
	addi r3, r0, 251
	ble r1, r3, .LBB13_5
.LBB13_1:
	ldw r1, r11+12
	addi r17, r1, 240
	ldw r1, r11+20
	ldw r14, r1+12
	addi r16, r0, 0
	bne r14, r16, .LBB13_3
.LBB13_2:
	add r3, r11, r0
	jal r31, luaE_extendCI
	add r14, r1, r0
.LBB13_3:
	stw r11+20, r14
	stw r14+0, r15
	sth r14+32, r13
	addi r1, r0, 2
	sth r14+34, r1
	stw r14+4, r17
	stw r11+20, r14
	ldw r1, r11+112
	andi r1, r1, 1
	bne r1, r16, .LBB13_8
.LBB13_4:
	add r3, r11, r0
	jalr lr, r12, 0
	add r12, r1, r0
	add r3, r11, r0
	add r4, r14, r0
	add r5, r1, r0
	jal r31, luaD_poscall
	add r1, r12, r0
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
.LBB13_5:
	ldw r1, r11+28
	sub r14, r15, r1
	ldw r1, r11+16
	ldw r1, r1+12
	addi r3, r0, 1
	blt r1, r3, .LBB13_7
.LBB13_6:
	add r3, r11, r0
	jal r31, luaC_step
.LBB13_7:
	addi r4, r0, 20
	addi r5, r0, 1
	add r3, r11, r0
	jal r31, luaD_growstack
	ldw r1, r11+28
	add r15, r1, r14
	jal r0, .LBB13_1
.LBB13_8:
	ldw r1, r11+12
	sub r1, r1, r15
	srai r1, r1, 2
	lui r3, 699051
	addi r3, r3, -1365
	mul r1, r1, r3
	addi r7, r1, -1
	addi r4, r0, 0
	addi r5, r0, -1
	addi r6, r0, 1
	add r3, r11, r0
	jal r31, luaD_hook
	jal r0, .LBB13_4
.Lfunc_end13:
	.size	precallC, .Lfunc_end13-precallC
                                        # -- End function
	.p2align	2                               # -- Begin function tryfuncTM
	.type	tryfuncTM,@function
tryfuncTM:                              # @tryfuncTM
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
	ldw r1, r3+24
	ldw r3, r3+12
	sub r1, r1, r3
	addi r3, r0, 23
	ble r1, r3, .LBB14_6
.LBB14_1:
	addi r5, r0, 23
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaT_gettmbyobj
	ldbu r3, r1+8
	andi r3, r3, 15
	addi r4, r0, 0
	beq r3, r4, .LBB14_9
.LBB14_2:
	ldw r3, r11+12
	bleu r3, r12, .LBB14_5
.LBB14_3:
	addi r3, r3, 8
.LBB14_4:
	addi r4, r3, -20
	ldw r5, r3+-20
	ldw r6, r3+-16
	stw r3+-4, r6
	stw r3+-8, r5
	addi r5, r3, -12
	ldbu r6, r3+-12
	stb r3+0, r6
	add r3, r5, r0
	bgtu r4, r12, .LBB14_4
.LBB14_5:
	ldw r3, r11+12
	addi r3, r3, 12
	stw r11+12, r3
	ldw r3, r1+0
	ldw r4, r1+4
	stw r12+4, r4
	stw r12+0, r3
	ldbu r1, r1+8
	stb r12+8, r1
	add r1, r12, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB14_6:
	ldw r1, r11+28
	sub r12, r12, r1
	ldw r1, r11+16
	ldw r1, r1+12
	addi r3, r0, 1
	blt r1, r3, .LBB14_8
.LBB14_7:
	add r3, r11, r0
	jal r31, luaC_step
.LBB14_8:
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r4, r0
	jal r31, luaD_growstack
	ldw r1, r11+28
	add r12, r1, r12
	jal r0, .LBB14_1
.LBB14_9:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaG_callerror
.Lfunc_end14:
	.size	tryfuncTM, .Lfunc_end14-tryfuncTM
                                        # -- End function
	.hidden	luaD_precall                    # -- Begin function luaD_precall
	.globl	luaD_precall
	.p2align	2
	.type	luaD_precall,@function
luaD_precall:                           # @luaD_precall
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
	add r11, r3, r0
	addi r13, r0, 6
	addi r14, r0, 22
	addi r15, r0, 38
.LBB15_1:
	ldbu r1, r4+8
	andi r1, r1, 63
	beq r1, r13, .LBB15_5
.LBB15_2:
	beq r1, r14, .LBB15_12
.LBB15_3:
	beq r1, r15, .LBB15_11
.LBB15_4:
	add r3, r11, r0
	jal r31, tryfuncTM
	add r4, r1, r0
	jal r0, .LBB15_1
.LBB15_5:
	ldw r1, r4+0
	ldw r14, r1+12
	ldw r1, r11+12
	sub r3, r1, r4
	srai r16, r3, 2
	lui r3, 699051
	addi r17, r3, -1365
	ldbu r15, r14+6
	ldbu r13, r14+8
	ldw r3, r11+24
	sub r1, r3, r1
	srai r1, r1, 2
	mul r1, r1, r17
	ble r1, r13, .LBB15_15
.LBB15_6:
	mul r16, r16, r17
	addi r1, r0, 12
	mul r1, r13, r1
	add r1, r4, r1
	addi r17, r1, 12
	ldw r1, r11+20
	ldw r1, r1+12
	addi r13, r0, 0
	bne r1, r13, .LBB15_8
.LBB15_7:
	add r3, r11, r0
	add r18, r4, r0
	jal r31, luaE_extendCI
	add r4, r18, r0
.LBB15_8:
	stw r11+20, r1
	stw r1+0, r4
	sth r1+32, r12
	sth r1+34, r13
	stw r1+4, r17
	stw r11+20, r1
	ldw r3, r14+52
	stw r1+16, r3
	bgt r16, r15, .LBB15_14
.LBB15_9:
	sub r3, r15, r16
	addi r3, r3, 1
.LBB15_10:
	ldw r4, r11+12
	addi r5, r4, 12
	stw r11+12, r5
	stb r4+8, r13
	addi r3, r3, -1
	bne r3, r13, .LBB15_10
	jal r0, .LBB15_14
.LBB15_11:
	ldw r1, r4+0
	addi r1, r1, 12
	jal r0, .LBB15_13
.LBB15_12:
	add r1, r4, r0
.LBB15_13:
	ldw r6, r1+0
	add r3, r11, r0
	add r5, r12, r0
	jal r31, precallC
	addi r1, r0, 0
.LBB15_14:
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
.LBB15_15:
	ldw r1, r11+28
	sub r18, r4, r1
	ldw r1, r11+16
	ldw r1, r1+12
	addi r3, r0, 1
	blt r1, r3, .LBB15_17
.LBB15_16:
	add r3, r11, r0
	jal r31, luaC_step
.LBB15_17:
	addi r5, r0, 1
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaD_growstack
	ldw r1, r11+28
	add r4, r1, r18
	jal r0, .LBB15_6
.Lfunc_end15:
	.size	luaD_precall, .Lfunc_end15-luaD_precall
                                        # -- End function
	.hidden	luaD_call                       # -- Begin function luaD_call
	.globl	luaD_call
	.p2align	2
	.type	luaD_call,@function
luaD_call:                              # @luaD_call
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
	ldw r1, r3+96
	addi r1, r1, 1
	stw r3+96, r1
	lui r3, 16
	addi r3, r3, -8
	and r1, r1, r3
	addi r3, r0, 200
	bgeu r1, r3, .LBB16_4
.LBB16_1:
	add r3, r11, r0
	jal r31, luaD_precall
	addi r3, r0, 0
	beq r1, r3, .LBB16_3
.LBB16_2:
	addi r3, r0, 4
	sth r1+34, r3
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaV_execute
.LBB16_3:
	ldw r1, r11+96
	addi r1, r1, -1
	stw r11+96, r1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB16_4:
	ldw r1, r11+24
	ldw r3, r11+12
	sub r1, r1, r3
	addi r3, r0, 11
	ble r1, r3, .LBB16_7
.LBB16_5:
	add r13, r4, r0
	add r12, r5, r0
.LBB16_6:
	add r3, r11, r0
	jal r31, luaE_checkcstack
	add r5, r12, r0
	add r4, r13, r0
	jal r0, .LBB16_1
.LBB16_7:
	add r12, r5, r0
	ldw r1, r11+28
	sub r13, r4, r1
	addi r4, r0, 0
	addi r5, r0, 1
	add r3, r11, r0
	jal r31, luaD_growstack
	ldw r1, r11+28
	add r13, r1, r13
	jal r0, .LBB16_6
.Lfunc_end16:
	.size	luaD_call, .Lfunc_end16-luaD_call
                                        # -- End function
	.hidden	luaD_callnoyield                # -- Begin function luaD_callnoyield
	.globl	luaD_callnoyield
	.p2align	2
	.type	luaD_callnoyield,@function
luaD_callnoyield:                       # @luaD_callnoyield
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
	ldw r1, r3+96
	lui r3, 16
	addi r6, r3, 1
	add r1, r1, r6
	stw r11+96, r1
	addi r3, r3, -8
	and r1, r1, r3
	addi r3, r0, 200
	bgeu r1, r3, .LBB17_4
.LBB17_1:
	add r3, r11, r0
	jal r31, luaD_precall
	addi r3, r0, 0
	beq r1, r3, .LBB17_3
.LBB17_2:
	addi r3, r0, 4
	sth r1+34, r3
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaV_execute
.LBB17_3:
	ldw r1, r11+96
	lui r3, 1048560
	addi r3, r3, -1
	add r1, r1, r3
	stw r11+96, r1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB17_4:
	ldw r1, r11+24
	ldw r3, r11+12
	sub r1, r1, r3
	addi r3, r0, 11
	ble r1, r3, .LBB17_7
.LBB17_5:
	add r13, r4, r0
	add r12, r5, r0
.LBB17_6:
	add r3, r11, r0
	jal r31, luaE_checkcstack
	add r5, r12, r0
	add r4, r13, r0
	jal r0, .LBB17_1
.LBB17_7:
	add r12, r5, r0
	ldw r1, r11+28
	sub r13, r4, r1
	addi r4, r0, 0
	addi r5, r0, 1
	add r3, r11, r0
	jal r31, luaD_growstack
	ldw r1, r11+28
	add r13, r1, r13
	jal r0, .LBB17_6
.Lfunc_end17:
	.size	luaD_callnoyield, .Lfunc_end17-luaD_callnoyield
                                        # -- End function
	.globl	lua_resume                      # -- Begin function lua_resume
	.p2align	2
	.type	lua_resume,@function
lua_resume:                             # @lua_resume
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
	add r12, r6, r0
	add r11, r3, r0
	addi r1, fp, -36
	stw r1+0, r5
	ldbu r3, r3+6
	addi r15, r0, 1
	beq r3, r15, .LBB18_7
.LBB18_1:
	addi r1, r0, 0
	bne r3, r1, .LBB18_4
.LBB18_2:
	ldw r6, r11+20
	addi r7, r11, 52
	ldw r3, r11+12
	beq r6, r7, .LBB18_6
.LBB18_3:
	sub r1, r1, r5
	addi r4, r0, 12
	mul r1, r1, r4
	add r12, r3, r1
	stw r11+12, r12
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r0, .LBB18_11
.LBB18_4:
	ldw r3, r11+12
.LBB18_5:
	sub r1, r1, r5
	addi r4, r0, 12
	mul r1, r1, r4
	add r12, r3, r1
	stw r11+12, r12
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r0, .LBB18_11
.LBB18_6:
	ldw r6, r6+0
	sub r6, r3, r6
	addi r6, r6, -12
	srai r6, r6, 2
	lui r7, 699051
	addi r7, r7, -1365
	mul r6, r6, r7
	beq r6, r5, .LBB18_5
.LBB18_7:
	addi r1, r0, 0
	beq r4, r1, .LBB18_9
.LBB18_8:
	ldhu r1, r4+96
.LBB18_9:
	stw r11+96, r1
	addi r3, r0, 200
	bltu r1, r3, .LBB18_13
.LBB18_10:
	ldw r1, r11+12
	addi r3, r0, 0
	sub r3, r3, r5
	addi r4, r0, 12
	mul r3, r3, r4
	add r12, r1, r3
	stw r11+12, r12
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
.LBB18_11:
	add r3, r11, r0
	jal r31, luaS_new
	stw r12+0, r1
	ldbu r1, r1+4
	ori  r1, r1, 64
	stb r12+8, r1
	ldw r1, r11+12
	addi r1, r1, 12
	stw r11+12, r1
	addi r1, r0, 2
.LBB18_12:
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
.LBB18_13:
	addi r1, r1, 1
	stw r11+96, r1
	lui r4, %hi(resume)
	addi r4, r4, %lo(resume)
	addi r5, fp, -36
	add r3, r11, r0
	jal r31, luaD_rawrunprotected
	addi r16, r0, 2
	blt r1, r16, .LBB18_22
.LBB18_14:
	addi r13, r0, 0
	lui r3, 14
	addi r17, r3, 1023
	lui r14, %hi(unroll)
	addi r14, r14, %lo(unroll)
.LBB18_15:
	ldw r3, r11+20
	add r4, r13, r0
	beq r3, r13, .LBB18_20
.LBB18_16:
	ldbu r4, r3+34
	andi r4, r4, 16
	bne r4, r13, .LBB18_19
.LBB18_17:
	ldw r3, r3+8
	bne r3, r13, .LBB18_16
.LBB18_18:
	add r4, r13, r0
	jal r0, .LBB18_20
.LBB18_19:
	add r4, r3, r0
.LBB18_20:
	beq r4, r13, .LBB18_22
.LBB18_21:
	stw r11+20, r4
	ldhu r3, r4+34
	and r3, r3, r17
	slli r1, r1, 10
	or  r1, r3, r1
	sth r4+34, r1
	add r3, r11, r0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, luaD_rawrunprotected
	bgt r1, r15, .LBB18_15
.LBB18_22:
	bge r1, r16, .LBB18_25
.LBB18_23:
	bne r1, r15, .LBB18_26
.LBB18_24:
	ldw r3, r11+20
	ldw r3, r3+28
	jal r0, .LBB18_27
.LBB18_25:
	stb r11+6, r1
	ldw r5, r11+12
	add r3, r11, r0
	add r4, r1, r0
	add r13, r1, r0
	jal r31, luaD_seterrorobj
	add r1, r13, r0
	ldw r3, r11+12
	ldw r4, r11+20
	stw r4+4, r3
	beq r1, r15, .LBB18_24
.LBB18_26:
	ldw r3, r11+12
	ldw r4, r11+20
	ldw r4, r4+0
	sub r3, r3, r4
	addi r3, r3, -12
	srai r3, r3, 2
	lui r4, 699051
	addi r4, r4, -1365
	mul r3, r3, r4
.LBB18_27:
	stw r12+0, r3
	jal r0, .LBB18_12
.Lfunc_end18:
	.size	lua_resume, .Lfunc_end18-lua_resume
                                        # -- End function
	.p2align	2                               # -- Begin function resume
	.type	resume,@function
resume:                                 # @resume
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
	ldw r5, r4+0
	ldw r1, r3+12
	addi r12, r0, 0
	sub r3, r12, r5
	addi r4, r0, 12
	mul r3, r3, r4
	add r3, r1, r3
	ldbu r4, r11+6
	beq r4, r12, .LBB19_3
.LBB19_1:
	ldw r4, r11+20
	stb r11+6, r12
	ldbu r1, r4+34
	andi r1, r1, 2
	ldw r6, r4+16
	bne r1, r12, .LBB19_6
.LBB19_2:
	addi r1, r6, -4
	stw r4+16, r1
	stw r11+12, r3
	add r3, r11, r0
	jal r31, luaV_execute
	jal r0, .LBB19_9
.LBB19_3:
	addi r4, r3, -12
	ldw r3, r11+96
	lui r5, 16
	addi r5, r5, -8
	and r3, r3, r5
	addi r5, r0, 200
	bgeu r3, r5, .LBB19_11
.LBB19_4:
	addi r5, r0, -1
	add r3, r11, r0
	jal r31, luaD_precall
	beq r1, r12, .LBB19_10
.LBB19_5:
	addi r3, r0, 4
	sth r1+34, r3
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaV_execute
	jal r0, .LBB19_10
.LBB19_6:
	beq r6, r12, .LBB19_8
.LBB19_7:
	ldw r5, r4+24
	addi r1, r0, 1
	add r3, r11, r0
	add r12, r4, r0
	add r4, r1, r0
	jalr lr, r6, 0
	add r4, r12, r0
	add r5, r1, r0
.LBB19_8:
	add r3, r11, r0
	jal r31, luaD_poscall
.LBB19_9:
	add r3, r11, r0
	jal r31, unroll
.LBB19_10:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB19_11:
	ldw r3, r11+24
	sub r1, r3, r1
	addi r3, r0, 11
	ble r1, r3, .LBB19_14
.LBB19_12:
	add r13, r4, r0
.LBB19_13:
	add r3, r11, r0
	jal r31, luaE_checkcstack
	add r4, r13, r0
	jal r0, .LBB19_4
.LBB19_14:
	ldw r1, r11+28
	sub r13, r4, r1
	addi r4, r0, 0
	addi r5, r0, 1
	add r3, r11, r0
	jal r31, luaD_growstack
	ldw r1, r11+28
	add r13, r1, r13
	jal r0, .LBB19_13
.Lfunc_end19:
	.size	resume, .Lfunc_end19-resume
                                        # -- End function
	.globl	lua_isyieldable                 # -- Begin function lua_isyieldable
	.p2align	2
	.type	lua_isyieldable,@function
lua_isyieldable:                        # @lua_isyieldable
# %bb.0:
	ldw r1, r3+96
	lui r3, 16
	sltu r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end20:
	.size	lua_isyieldable, .Lfunc_end20-lua_isyieldable
                                        # -- End function
	.globl	lua_yieldk                      # -- Begin function lua_yieldk
	.p2align	2
	.type	lua_yieldk,@function
lua_yieldk:                             # @lua_yieldk
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r3+96
	lui r7, 16
	bgeu r1, r7, .LBB21_3
.LBB21_1:
	ldw r1, r3+20
	addi r7, r0, 1
	stb r3+6, r7
	stw r1+28, r4
	ldbu r4, r1+34
	andi r7, r4, 2
	addi r4, r0, 0
	bne r7, r4, .LBB21_6
.LBB21_2:
	addi r1, r0, 0
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB21_3:
	ldw r1, r3+16
	ldw r1, r1+152
	bne r3, r1, .LBB21_5
.LBB21_4:
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	jal r31, luaG_runerror
.LBB21_5:
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	jal r31, luaG_runerror
.LBB21_6:
	stw r1+16, r6
	beq r6, r4, .LBB21_8
.LBB21_7:
	stw r1+24, r5
.LBB21_8:
	addi r4, r0, 1
	jal r31, luaD_throw
.Lfunc_end21:
	.size	lua_yieldk, .Lfunc_end21-lua_yieldk
                                        # -- End function
	.hidden	luaD_closeprotected             # -- Begin function luaD_closeprotected
	.globl	luaD_closeprotected
	.p2align	2
	.type	luaD_closeprotected,@function
luaD_closeprotected:                    # @luaD_closeprotected
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
	add r1, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	ldw r16, r3+20
	ldbu r17, r3+7
	addi r13, fp, -44
	lui r14, %hi(closepaux)
	addi r14, r14, %lo(closepaux)
	addi r18, r0, 0
                                        # implicit-def: $r15
.LBB22_1:
	ldw r3, r12+28
	add r3, r3, r11
	stw r13+0, r3
	stw r13+4, r1
	add r3, r12, r0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, luaD_rawrunprotected
	bne r1, r18, .LBB22_3
.LBB22_2:
	ldw r15, r13+4
	bne r1, r18, .LBB22_1
	jal r0, .LBB22_4
.LBB22_3:
	stw r12+20, r16
	stb r12+7, r17
	bne r1, r18, .LBB22_1
.LBB22_4:
	add r1, r15, r0
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
.Lfunc_end22:
	.size	luaD_closeprotected, .Lfunc_end22-luaD_closeprotected
                                        # -- End function
	.p2align	2                               # -- Begin function closepaux
	.type	closepaux,@function
closepaux:                              # @closepaux
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r4+0
	ldw r5, r4+4
	addi r6, r0, 0
	add r4, r1, r0
	jal r31, luaF_close
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end23:
	.size	closepaux, .Lfunc_end23-closepaux
                                        # -- End function
	.hidden	luaD_pcall                      # -- Begin function luaD_pcall
	.globl	luaD_pcall
	.p2align	2
	.type	luaD_pcall,@function
luaD_pcall:                             # @luaD_pcall
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
	add r11, r3, r0
	ldw r13, r3+20
	ldbu r15, r3+7
	ldw r14, r3+92
	stw r3+92, r7
	jal r31, luaD_rawrunprotected
	addi r3, r0, 0
	bne r1, r3, .LBB24_2
.LBB24_1:
	stw r11+92, r14
	add r1, r3, r0
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
.LBB24_2:
	stw r11+20, r13
	stb r11+7, r15
	add r3, r11, r0
	add r4, r12, r0
	add r5, r1, r0
	jal r31, luaD_closeprotected
	add r13, r1, r0
	ldw r1, r11+28
	add r5, r1, r12
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaD_seterrorobj
	add r3, r11, r0
	jal r31, luaD_shrinkstack
	add r3, r13, r0
	jal r0, .LBB24_1
.Lfunc_end24:
	.size	luaD_pcall, .Lfunc_end24-luaD_pcall
                                        # -- End function
	.hidden	luaD_protectedparser            # -- Begin function luaD_protectedparser
	.globl	luaD_protectedparser
	.p2align	2
	.type	luaD_protectedparser,@function
luaD_protectedparser:                   # @luaD_protectedparser
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
	stw fp+-40, lr
	add r11, r3, r0
	ldw r1, r3+96
	lui r3, 16
	add r1, r1, r3
	stw r11+96, r1
	addi r12, fp, -100
	stw r12+0, r4
	stw r12+56, r5
	stw r12+52, r6
	addi r13, r0, 0
	stw r12+16, r13
	stw r12+24, r13
	stw r12+28, r13
	stw r12+36, r13
	stw r12+40, r13
	stw r12+48, r13
	stw r12+4, r13
	stw r12+12, r13
	ldw r15, r11+12
	ldw r17, r11+28
	ldw r16, r11+92
	ldw r18, r11+20
	ldbu r19, r11+7
	lui r4, %hi(f_parser)
	addi r4, r4, %lo(f_parser)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaD_rawrunprotected
	add r14, r13, r0
	bne r1, r13, .LBB25_2
.LBB25_1:
	stw r11+92, r16
	ldw r4, r12+4
	ldw r5, r12+12
	add r3, r11, r0
	add r6, r13, r0
	jal r31, luaM_saferealloc_
	stw r12+4, r1
	stw r12+12, r13
	ldw r4, r12+16
	ldw r1, r12+24
	addi r3, r0, 20
	mul r5, r1, r3
	add r3, r11, r0
	jal r31, luaM_free_
	ldw r4, r12+28
	ldw r1, r12+36
	slli r5, r1, 4
	add r3, r11, r0
	jal r31, luaM_free_
	ldw r4, r12+40
	ldw r1, r12+48
	slli r5, r1, 4
	add r3, r11, r0
	jal r31, luaM_free_
	ldw r1, r11+96
	lui r3, 1048560
	add r1, r1, r3
	stw r11+96, r1
	add r1, r14, r0
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
	addi sp, sp, 120
	jalr r0, r31, 0
.LBB25_2:
	sub r15, r15, r17
	stw r11+20, r18
	stb r11+7, r19
	add r3, r11, r0
	add r4, r15, r0
	add r5, r1, r0
	jal r31, luaD_closeprotected
	add r14, r1, r0
	ldw r1, r11+28
	add r5, r1, r15
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaD_seterrorobj
	add r3, r11, r0
	jal r31, luaD_shrinkstack
	jal r0, .LBB25_1
.Lfunc_end25:
	.size	luaD_protectedparser, .Lfunc_end25-luaD_protectedparser
                                        # -- End function
	.p2align	2                               # -- Begin function f_parser
	.type	f_parser,@function
f_parser:                               # @f_parser
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
	ldw r3, r4+0
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r15, r0, 0
	beq r1, r15, .LBB26_2
.LBB26_1:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r14, r1+0
	jal r0, .LBB26_3
.LBB26_2:
	jal r31, luaZ_fill
	add r14, r1, r0
.LBB26_3:
	ldw r13, r12+52
	addi r1, r0, 27
	bne r14, r1, .LBB26_7
.LBB26_4:
	beq r13, r15, .LBB26_6
.LBB26_5:
	addi r4, r0, 98
	add r3, r13, r0
	jal r31, strchr
	beq r1, r15, .LBB26_11
.LBB26_6:
	ldw r4, r12+0
	ldw r5, r12+56
	add r3, r11, r0
	jal r31, luaU_undump
	jal r0, .LBB26_10
.LBB26_7:
	beq r13, r15, .LBB26_9
.LBB26_8:
	addi r4, r0, 116
	add r3, r13, r0
	jal r31, strchr
	beq r1, r15, .LBB26_12
.LBB26_9:
	ldw r4, r12+0
	addi r5, r12, 4
	addi r6, r12, 16
	ldw r7, r12+56
	add r3, r11, r0
	add r8, r14, r0
	jal r31, luaY_parser
.LBB26_10:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaF_initupvals
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
.LBB26_11:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	jal r0, .LBB26_13
.LBB26_12:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
.LBB26_13:
	add r3, r11, r0
	add r6, r13, r0
	jal r31, luaO_pushfstring
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, luaD_throw
.Lfunc_end26:
	.size	f_parser, .Lfunc_end26-f_parser
                                        # -- End function
	.p2align	2                               # -- Begin function unroll
	.type	unroll,@function
unroll:                                 # @unroll
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
	addi r15, r3, 52
	ldw r13, r3+20
	beq r13, r15, .LBB27_14
.LBB27_1:
	add r11, r3, r0
	addi r16, r0, 0
	addi r12, r0, 1
	lui r1, 16
	addi r17, r1, -17
	lui r1, 14
	addi r18, r1, 1023
	jal r0, .LBB27_3
.LBB27_2:
	add r3, r11, r0
	jal r31, luaV_finishOp
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaV_execute
	ldw r13, r11+20
	beq r13, r15, .LBB27_14
.LBB27_3:
	ldhu r1, r13+34
	andi r3, r1, 2
	beq r3, r16, .LBB27_2
.LBB27_4:
	andi r3, r1, 512
	bne r3, r16, .LBB27_11
.LBB27_5:
	andi r3, r1, 16
	add r4, r12, r0
	beq r3, r16, .LBB27_8
.LBB27_6:
	srli r3, r1, 10
	andi r14, r3, 7
	add r4, r12, r0
	bne r14, r16, .LBB27_13
.LBB27_7:
	ldhu r1, r13+34
	and r1, r1, r17
	sth r13+34, r1
	ldw r1, r13+20
	stw r11+92, r1
.LBB27_8:
	ldw r1, r11+20
	ldw r5, r1+4
	ldw r3, r11+12
	bgeu r5, r3, .LBB27_10
.LBB27_9:
	stw r1+4, r3
.LBB27_10:
	ldw r1, r13+16
	ldw r5, r13+24
	add r3, r11, r0
	jalr lr, r1, 0
	add r5, r1, r0
	jal r0, .LBB27_12
.LBB27_11:
	ldw r5, r13+28
.LBB27_12:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaD_poscall
	ldw r13, r11+20
	bne r13, r15, .LBB27_3
	jal r0, .LBB27_14
.LBB27_13:
	ldw r3, r11+28
	ldw r4, r13+28
	add r4, r3, r4
	andi r1, r1, 1
	stb r11+7, r1
	add r3, r11, r0
	add r5, r14, r0
	add r6, r12, r0
	jal r31, luaF_close
	add r3, r11, r0
	add r4, r14, r0
	add r5, r1, r0
	jal r31, luaD_seterrorobj
	add r3, r11, r0
	jal r31, luaD_shrinkstack
	ldhu r1, r13+34
	and r1, r1, r18
	sth r13+34, r1
	add r4, r14, r0
	jal r0, .LBB27_7
.LBB27_14:
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
.Lfunc_end27:
	.size	unroll, .Lfunc_end27-unroll
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"error in error handling"
	.size	.L.str, 24

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"stack overflow"
	.size	.L.str.1, 15

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"cannot resume non-suspended coroutine"
	.size	.L.str.2, 38

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"cannot resume dead coroutine"
	.size	.L.str.3, 29

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"C stack overflow"
	.size	.L.str.4, 17

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"attempt to yield across a C-call boundary"
	.size	.L.str.5, 42

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"attempt to yield from outside a coroutine"
	.size	.L.str.6, 42

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"binary"
	.size	.L.str.8, 7

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"text"
	.size	.L.str.9, 5

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"attempt to load a %s chunk (mode is '%s')"
	.size	.L.str.10, 42

	.hidden	luaS_newlstr
	.hidden	luaE_resetthread
	.hidden	luaM_realloc_
	.hidden	luaG_runerror
	.hidden	luaE_shrinkCI
	.hidden	luaC_step
	.hidden	luaM_saferealloc_
	.hidden	luaM_free_
	.hidden	luaF_close
	.hidden	luaT_gettmbyobj
	.hidden	luaG_callerror
	.hidden	luaE_extendCI
	.hidden	luaE_checkcstack
	.hidden	luaV_execute
	.hidden	luaS_new
	.hidden	luaV_finishOp
	.hidden	luaZ_fill
	.hidden	luaU_undump
	.hidden	luaY_parser
	.hidden	luaF_initupvals
	.hidden	luaO_pushfstring
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
