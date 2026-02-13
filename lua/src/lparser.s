	.file	"lparser.c"
	.text
	.hidden	luaY_nvarstack                  # -- Begin function luaY_nvarstack
	.globl	luaY_nvarstack
	.p2align	2
	.type	luaY_nvarstack,@function
luaY_nvarstack:                         # @luaY_nvarstack
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	ldbu r1, r3+50
	addi r4, r1, 1
	addi r5, r0, 20
	mul r1, r1, r5
	addi r7, r1, -11
	addi r6, r0, 0
	addi r8, r0, 1
	addi r9, r0, 3
                                        # implicit-def: $r1
	jal r0, .LBB0_2
.LBB0_1:
	addi r7, r7, -20
	bne r10, r9, .LBB0_6
.LBB0_2:
	addi r4, r4, -1
	blt r4, r8, .LBB0_5
.LBB0_3:
	ldw r10, r3+8
	ldw r10, r10+56
	ldw r10, r10+0
	ldw r11, r3+40
	mul r11, r11, r5
	add r10, r10, r11
	add r11, r10, r7
	ldbu r10, r11+0
	beq r10, r9, .LBB0_1
.LBB0_4:
	ldbu r1, r11+1
	addi r1, r1, 1
	jal r0, .LBB0_1
.LBB0_5:
	add r1, r6, r0
.LBB0_6:
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaY_nvarstack, .Lfunc_end0-luaY_nvarstack
                                        # -- End function
	.hidden	luaY_parser                     # -- Begin function luaY_parser
	.globl	luaY_parser
	.p2align	2
	.type	luaY_parser,@function
luaY_parser:                            # @luaY_parser
# %bb.0:
	addi sp, sp, -200
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 200
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
	add r16, r8, r0
	add r20, r7, r0
	add r18, r6, r0
	add r19, r5, r0
	add r17, r4, r0
	add r11, r3, r0
	addi r13, r0, 1
	add r4, r13, r0
	jal r31, luaF_newLclosure
	add r12, r1, r0
	ldw r1, r11+12
	stw r1+0, r12
	addi r3, r0, 70
	stb r1+8, r3
	add r3, r11, r0
	jal r31, luaD_inctop
	add r3, r11, r0
	jal r31, luaH_new
	addi r14, fp, -132
	stw r14+52, r1
	ldw r3, r11+12
	stw r3+0, r1
	addi r1, r0, 69
	stb r3+8, r1
	add r3, r11, r0
	jal r31, luaD_inctop
	add r3, r11, r0
	jal r31, luaF_newproto
	stw r12+12, r1
	addi r15, fp, -188
	stw r15+0, r1
	ldbu r3, r12+5
	andi r3, r3, 32
	addi r21, r0, 0
	beq r3, r21, .LBB1_3
.LBB1_1:
	ldbu r3, r1+5
	andi r3, r3, 24
	beq r3, r21, .LBB1_3
.LBB1_2:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r1, r0
	jal r31, luaC_barrier_
.LBB1_3:
	add r3, r11, r0
	add r4, r20, r0
	jal r31, luaS_new
	ldw r4, r15+0
	stw r4+76, r1
	ldbu r3, r4+5
	andi r3, r3, 32
	beq r3, r21, .LBB1_6
.LBB1_4:
	ldbu r3, r1+5
	andi r3, r3, 24
	beq r3, r21, .LBB1_6
.LBB1_5:
	add r3, r11, r0
	add r5, r1, r0
	jal r31, luaC_barrier_
.LBB1_6:
	stw r14+48, r19
	stw r14+56, r18
	stw r18+28, r21
	stw r18+16, r21
	stw r18+4, r21
	ldw r1, r15+0
	ldw r6, r1+76
	add r3, r11, r0
	add r4, r14, r0
	add r5, r17, r0
	add r7, r16, r0
	jal r31, luaX_setinput
	ldw r16, r15+0
	ldw r1, r14+36
	stw r15+4, r1
	stw r15+8, r14
	stw r14+36, r15
	stw r15+16, r21
	ldw r1, r16+40
	stw r15+24, r1
	stw r15+20, r21
	sth r15+52, r21
	stw r15+28, r21
	stw r15+36, r21
	stw r15+32, r21
	stw r15+48, r21
	stb r15+54, r21
	ldw r1, r14+56
	ldw r3, r1+4
	stw r15+40, r3
	ldw r1, r1+28
	stw r15+44, r1
	stw r15+12, r21
	ldw r5, r14+60
	stw r16+76, r5
	ldbu r1, r16+5
	andi r1, r1, 32
	beq r1, r21, .LBB1_9
.LBB1_7:
	ldbu r1, r5+5
	andi r1, r1, 24
	beq r1, r21, .LBB1_9
.LBB1_8:
	ldw r3, r14+40
	add r4, r16, r0
	jal r31, luaC_barrier_
.LBB1_9:
	addi r1, r0, 2
	stb r16+8, r1
	addi r1, fp, -64
	stb r1+14, r21
	ldbu r3, r15+50
	stb r1+12, r3
	ldw r3, r15+8
	ldw r3, r3+56
	ldw r4, r3+28
	stw r1+4, r4
	ldw r3, r3+16
	stw r1+8, r3
	stb r1+13, r21
	ldw r3, r15+12
	beq r3, r21, .LBB1_11
.LBB1_10:
	ldbu r4, r3+15
	addi r5, r0, 0
	sne r21, r4, r5
.LBB1_11:
	stb r1+15, r21
	stw r1+0, r3
	stw r15+12, r1
	ldw r1, r15+0
	stb r1+7, r13
	addi r4, r0, 81
	addi r16, r0, 0
	add r3, r15, r0
	add r5, r16, r0
	add r6, r16, r0
	add r7, r16, r0
	add r8, r16, r0
	jal r31, luaK_codeABCk
	ldbu r5, r15+51
	addi r1, r0, 255
	beq r5, r1, .LBB1_26
.LBB1_12:
	ldw r18, r15+0
	addi r6, r18, 12
	ldw r17, r18+12
	ldw r1, r15+8
	ldw r3, r1+40
	ldw r4, r18+60
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	addi r7, r0, 8
	addi r8, r0, 255
	jal r31, luaM_growaux_
	stw r18+60, r1
	ldw r4, r18+12
	ble r4, r17, .LBB1_15
.LBB1_13:
	slli r3, r17, 3
	add r3, r1, r3
	sub r4, r4, r17
.LBB1_14:
	stw r3+0, r16
	addi r3, r3, 8
	addi r4, r4, -1
	bne r4, r16, .LBB1_14
.LBB1_15:
	ldbu r3, r15+51
	addi r4, r3, 1
	stb r15+51, r4
	slli r3, r3, 3
	add r1, r1, r3
	sth r1+4, r13
	stb r1+6, r16
	ldw r5, r14+64
	stw r1+0, r5
	ldw r4, r15+0
	ldbu r1, r4+5
	andi r1, r1, 32
	beq r1, r16, .LBB1_18
.LBB1_16:
	ldbu r1, r5+5
	andi r1, r1, 24
	beq r1, r16, .LBB1_18
.LBB1_17:
	ldw r3, r14+40
	jal r31, luaC_barrier_
.LBB1_18:
	add r3, r14, r0
	jal r31, luaX_next
	addi r15, r0, 29
	lui r1, 131104
	addi r17, r1, 7
	addi r18, r0, 14
	jal r0, .LBB1_20
.LBB1_19:
	add r3, r14, r0
	jal r31, statement
.LBB1_20:
	ldw r1, r14+12
	addi r1, r1, -259
	bgtu r1, r15, .LBB1_19
.LBB1_21:
	sll r3, r13, r1
	and r3, r3, r17
	bne r3, r16, .LBB1_24
.LBB1_22:
	bne r1, r18, .LBB1_19
.LBB1_23:
	addi r3, fp, -132
	jal r31, statement
.LBB1_24:
	ldw r1, r14+12
	addi r3, r0, 288
	bne r1, r3, .LBB1_27
.LBB1_25:
	addi r3, fp, -132
	jal r31, close_func
	ldw r1, r11+12
	addi r1, r1, -12
	stw r11+12, r1
	add r1, r12, r0
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
	addi sp, sp, 200
	jalr r0, r31, 0
.LBB1_26:
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	addi r3, fp, -188
	addi r4, r0, 255
	jal r31, errorlimit
.LBB1_27:
	addi r3, fp, -132
	addi r4, r0, 288
	jal r31, error_expected
.Lfunc_end1:
	.size	luaY_parser, .Lfunc_end1-luaY_parser
                                        # -- End function
	.p2align	2                               # -- Begin function close_func
	.type	close_func,@function
close_func:                             # @close_func
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
	add r12, r3, r0
	ldw r11, r3+40
	ldw r13, r3+36
	ldw r18, r13+0
	ldbu r4, r13+50
	addi r1, r4, 1
	addi r3, r0, 20
	mul r4, r4, r3
	addi r6, r4, -11
	addi r5, r0, 0
	addi r14, r0, 1
	addi r7, r0, 3
                                        # implicit-def: $r4
	jal r0, .LBB2_2
.LBB2_1:
	addi r6, r6, -20
	bne r8, r7, .LBB2_6
.LBB2_2:
	addi r1, r1, -1
	blt r1, r14, .LBB2_5
.LBB2_3:
	ldw r8, r13+8
	ldw r8, r8+56
	ldw r8, r8+0
	ldw r9, r13+40
	mul r9, r9, r3
	add r8, r8, r9
	add r9, r8, r6
	ldbu r8, r9+0
	beq r8, r7, .LBB2_1
.LBB2_4:
	ldbu r4, r9+1
	addi r4, r4, 1
	jal r0, .LBB2_1
.LBB2_5:
	add r4, r5, r0
.LBB2_6:
	addi r5, r0, 0
	add r3, r13, r0
	jal r31, luaK_ret
	add r3, r13, r0
	jal r31, leaveblock
	add r3, r13, r0
	jal r31, luaK_finish
	ldw r4, r18+52
	addi r5, r18, 20
	ldw r6, r13+16
	addi r15, r0, 4
	add r3, r11, r0
	add r7, r15, r0
	jal r31, luaM_shrinkvector_
	stw r18+52, r1
	ldw r4, r18+64
	addi r5, r18, 24
	ldw r6, r13+16
	add r3, r11, r0
	add r7, r14, r0
	jal r31, luaM_shrinkvector_
	stw r18+64, r1
	ldw r4, r18+68
	addi r5, r18, 36
	ldw r6, r13+36
	addi r16, r0, 8
	add r3, r11, r0
	add r7, r16, r0
	jal r31, luaM_shrinkvector_
	stw r18+68, r1
	ldw r4, r18+48
	addi r5, r18, 16
	ldw r6, r13+28
	addi r17, r0, 12
	add r3, r11, r0
	add r7, r17, r0
	jal r31, luaM_shrinkvector_
	stw r18+48, r1
	ldw r4, r18+56
	addi r5, r18, 28
	ldw r6, r13+32
	add r3, r11, r0
	add r7, r15, r0
	jal r31, luaM_shrinkvector_
	stw r18+56, r1
	ldw r4, r18+72
	addi r5, r18, 32
	ldh r6, r13+48
	add r3, r11, r0
	add r7, r17, r0
	jal r31, luaM_shrinkvector_
	stw r18+72, r1
	ldw r4, r18+60
	addi r5, r18, 12
	ldbu r6, r13+51
	add r3, r11, r0
	add r7, r16, r0
	jal r31, luaM_shrinkvector_
	stw r18+60, r1
	ldw r1, r13+4
	stw r12+36, r1
	ldw r1, r11+16
	ldw r1, r1+12
	blt r1, r14, .LBB2_8
.LBB2_7:
	add r3, r11, r0
	jal r31, luaC_step
.LBB2_8:
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
.Lfunc_end2:
	.size	close_func, .Lfunc_end2-close_func
                                        # -- End function
	.p2align	2                               # -- Begin function errorlimit
	.type	errorlimit,@function
errorlimit:                             # @errorlimit
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
	ldw r1, r3+8
	ldw r13, r1+40
	ldw r1, r3+0
	ldw r1, r1+40
	addi r3, r0, 0
	bne r1, r3, .LBB3_2
.LBB3_1:
	lui r7, %hi(.L.str.1)
	addi r7, r7, %lo(.L.str.1)
	jal r0, .LBB3_3
.LBB3_2:
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	add r3, r13, r0
	add r14, r5, r0
	add r5, r1, r0
	jal r31, luaO_pushfstring
	add r5, r14, r0
	add r7, r1, r0
.LBB3_3:
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	add r3, r13, r0
	add r6, r12, r0
	jal r31, luaO_pushfstring
	ldw r3, r11+8
	add r4, r1, r0
	jal r31, luaX_syntaxerror
.Lfunc_end3:
	.size	errorlimit, .Lfunc_end3-errorlimit
                                        # -- End function
	.p2align	2                               # -- Begin function statement
	.type	statement,@function
statement:                              # @statement
# %bb.0:
	addi sp, sp, -168
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 168
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
	add r11, r3, r0
	ldw r12, r3+4
	ldw r3, r3+40
	jal r31, luaE_incCstack
	ldw r1, r11+12
	addi r3, r1, -257
	addi r14, r0, 30
	bgtu r3, r14, .LBB4_3
.LBB4_1:
	slli r1, r3, 2
	lui r3, %hi(.LJTI4_0)
	addi r3, r3, %lo(.LJTI4_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB4_2:
	ldw r15, r11+4
	add r3, r11, r0
	jal r31, luaX_next
	ldw r3, r11+40
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	addi r5, r0, 5
	jal r31, luaS_newlstr
	add r12, r1, r0
	ldw r3, r11+36
	jal r31, luaK_jump
	add r13, r1, r0
	ldw r16, r11+56
	ldw r14, r16+16
	ldw r3, r11+40
	ldw r4, r16+12
	addi r6, r16, 20
	lui r9, %hi(.L.str.13)
	addi r9, r9, %lo(.L.str.13)
	lui r1, 8
	addi r8, r1, -1
	addi r7, r0, 16
	add r5, r14, r0
	jal r31, luaM_growaux_
	stw r16+12, r1
	slli r3, r14, 4
	add r1, r1, r3
	stw r1+0, r12
	stw r1+8, r15
	ldw r3, r11+36
	ldbu r3, r3+50
	stb r1+12, r3
	addi r3, r0, 0
	stb r1+13, r3
	stw r1+4, r13
	addi r1, r14, 1
	stw r16+16, r1
	jal r0, .LBB4_229
.LBB4_3:
	addi r3, r0, 59
	bne r1, r3, .LBB4_4
	jal r0, .LBB4_228
.LBB4_4:
	ldw r13, r11+36
	addi r12, fp, -100
	addi r4, r12, 4
	add r3, r11, r0
	jal r31, suffixedexp
	ldw r1, r11+12
	addi r3, r0, 61
	beq r1, r3, .LBB4_6
.LBB4_5:
	addi r3, r0, 44
	beq r1, r3, .LBB4_6
	jal r0, .LBB4_162
.LBB4_6:
	addi r1, r0, 0
	stw r12+0, r1
	addi r5, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, restassign
	jal r0, .LBB4_229
.LBB4_7:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	addi r3, r0, 264
	beq r1, r3, .LBB4_8
	jal r0, .LBB4_141
.LBB4_8:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	addi r3, r0, 291
	beq r1, r3, .LBB4_9
	jal r0, .LBB4_154
.LBB4_9:
	ldw r14, r11+36
	ldbu r15, r14+50
	ldw r16, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	ldw r3, r11+36
	ldw r13, r11+56
	ldw r1, r13+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	addi r4, r0, 201
	blt r1, r4, .LBB4_10
	jal r0, .LBB4_238
.LBB4_10:
	ldw r3, r11+40
	ldw r4, r13+0
	addi r6, r13, 8
	lui r9, %hi(.L.str.9)
	addi r9, r9, %lo(.L.str.9)
	lui r1, 16
	addi r8, r1, -1
	addi r12, r0, 20
	add r7, r12, r0
	jal r31, luaM_growaux_
	stw r13+0, r1
	ldw r3, r13+4
	addi r4, r3, 1
	stw r13+4, r4
	mul r3, r3, r12
	add r1, r1, r3
	addi r13, r0, 0
	stb r1+9, r13
	stw r1+16, r16
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, adjustlocalvars
	ldw r6, r11+4
	addi r4, fp, -100
	add r3, r11, r0
	add r5, r13, r0
	jal r31, body
	ldw r1, r14+16
	ldw r3, r14+8
	ldw r3, r3+56
	ldw r3, r3+0
	ldw r4, r14+40
	mul r4, r4, r12
	add r3, r3, r4
	mul r4, r15, r12
	add r3, r3, r4
	ldh r3, r3+12
	ldw r4, r14+0
	ldw r4, r4+72
	addi r5, r0, 12
	mul r3, r3, r5
	add r3, r4, r3
	stw r3+4, r1
	jal r0, .LBB4_229
.LBB4_11:
	ldw r13, r11+36
	addi r1, r0, -1
	addi r14, fp, -120
	stw r14+0, r1
	addi r15, r0, 260
.LBB4_12:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, test_then_block
	ldw r1, r11+12
	beq r1, r15, .LBB4_12
.LBB4_13:
	addi r3, r0, 259
	bne r1, r3, .LBB4_23
.LBB4_14:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r15, r11+36
	addi r16, r0, 0
	addi r1, fp, -100
	stb r1+14, r16
	ldbu r3, r15+50
	stb r1+12, r3
	ldw r3, r15+8
	ldw r3, r3+56
	ldw r4, r3+28
	stw r1+4, r4
	ldw r3, r3+16
	stw r1+8, r3
	stb r1+13, r16
	ldw r3, r15+12
	add r4, r16, r0
	beq r3, r16, .LBB4_16
.LBB4_15:
	ldbu r4, r3+15
	addi r5, r0, 0
	sne r4, r4, r5
.LBB4_16:
	stb r1+15, r4
	stw r1+0, r3
	stw r15+12, r1
	addi r17, r0, 29
	addi r18, r0, 1
	lui r1, 131104
	addi r19, r1, 7
	addi r20, r0, 14
	jal r0, .LBB4_18
.LBB4_17:
	add r3, r11, r0
	jal r31, statement
.LBB4_18:
	ldw r1, r11+12
	addi r1, r1, -259
	bgtu r1, r17, .LBB4_17
.LBB4_19:
	sll r3, r18, r1
	and r3, r3, r19
	bne r3, r16, .LBB4_22
.LBB4_20:
	bne r1, r20, .LBB4_17
.LBB4_21:
	add r3, r11, r0
	jal r31, statement
.LBB4_22:
	add r3, r15, r0
	jal r31, leaveblock
.LBB4_23:
	addi r4, r0, 261
	addi r5, r0, 266
	add r3, r11, r0
	add r6, r12, r0
	jal r31, check_match
	ldw r4, r14+0
	add r3, r13, r0
	jal r31, luaK_patchtohere
	jal r0, .LBB4_229
.LBB4_24:
	add r3, r11, r0
	jal r31, luaX_next
	addi r13, fp, -120
	add r3, r11, r0
	add r4, r13, r0
	jal r31, singlevar
	addi r19, r0, 46
	addi r18, r0, 291
	addi r16, r0, -1
	addi r14, fp, -100
	addi r17, r0, 7
	ldw r1, r11+12
	bne r1, r19, .LBB4_27
.LBB4_25:
	ldw r15, r11+36
	add r3, r15, r0
	add r4, r13, r0
	jal r31, luaK_exp2anyregup
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	beq r1, r18, .LBB4_26
	jal r0, .LBB4_154
.LBB4_26:
	ldw r20, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	stw r14+12, r16
	stw r14+16, r16
	stw r14+0, r17
	stw r14+4, r20
	add r3, r15, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, luaK_indexed
	ldw r1, r11+12
	beq r1, r19, .LBB4_25
.LBB4_27:
	addi r3, r0, 58
	bne r1, r3, .LBB4_155
.LBB4_28:
	ldw r15, r11+36
	addi r4, fp, -120
	add r3, r15, r0
	jal r31, luaK_exp2anyregup
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	bne r1, r18, .LBB4_154
.LBB4_29:
	ldw r18, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	stw r14+12, r16
	stw r14+16, r16
	stw r14+0, r17
	stw r14+4, r18
	addi r4, fp, -120
	add r3, r15, r0
	add r5, r14, r0
	jal r31, luaK_indexed
	addi r5, r0, 1
	jal r0, .LBB4_156
.LBB4_30:
	ldw r13, r11+36
	addi r14, r0, 1
	addi r1, fp, -120
	stb r1+14, r14
	ldbu r3, r13+50
	stb r1+12, r3
	ldw r3, r13+8
	ldw r3, r3+56
	ldw r4, r3+28
	stw r1+4, r4
	ldw r3, r3+16
	stw r1+8, r3
	addi r4, r0, 0
	stb r1+13, r4
	ldw r3, r13+12
	beq r3, r4, .LBB4_32
.LBB4_31:
	ldbu r4, r3+15
	addi r5, r0, 0
	sne r4, r4, r5
.LBB4_32:
	stb r1+15, r4
	stw r1+0, r3
	stw r13+12, r1
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	addi r21, r0, 291
	bne r1, r21, .LBB4_154
.LBB4_33:
	ldw r20, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	addi r22, r0, 44
	beq r1, r22, .LBB4_116
.LBB4_34:
	addi r3, r0, 267
	beq r1, r3, .LBB4_116
.LBB4_35:
	addi r19, r0, 61
	beq r1, r19, .LBB4_36
	jal r0, .LBB4_248
.LBB4_36:
	ldw r15, r11+36
	ldbu r14, r15+52
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	addi r5, r0, 11
	add r3, r11, r0
	jal r31, luaX_newstring
	add r18, r1, r0
	ldw r3, r11+36
	ldw r23, r11+56
	ldw r1, r23+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	addi r21, r0, 201
	blt r1, r21, .LBB4_37
	jal r0, .LBB4_238
.LBB4_37:
	ldw r3, r11+40
	ldw r4, r23+0
	addi r6, r23, 8
	lui r9, %hi(.L.str.9)
	addi r9, r9, %lo(.L.str.9)
	lui r1, 16
	addi r17, r1, -1
	addi r16, r0, 20
	add r7, r16, r0
	add r8, r17, r0
	jal r31, luaM_growaux_
	stw r23+0, r1
	ldw r3, r23+4
	addi r4, r3, 1
	stw r23+4, r4
	mul r3, r3, r16
	add r1, r1, r3
	addi r23, r0, 0
	stb r1+9, r23
	stw r1+16, r18
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	addi r5, r0, 11
	add r3, r11, r0
	jal r31, luaX_newstring
	add r18, r1, r0
	ldw r3, r11+36
	ldw r24, r11+56
	ldw r1, r24+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	blt r1, r21, .LBB4_38
	jal r0, .LBB4_238
.LBB4_38:
	ldw r3, r11+40
	ldw r4, r24+0
	addi r6, r24, 8
	lui r9, %hi(.L.str.9)
	addi r9, r9, %lo(.L.str.9)
	add r7, r16, r0
	add r8, r17, r0
	jal r31, luaM_growaux_
	stw r24+0, r1
	ldw r3, r24+4
	addi r4, r3, 1
	stw r24+4, r4
	mul r3, r3, r16
	add r1, r1, r3
	stb r1+9, r23
	stw r1+16, r18
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	addi r5, r0, 11
	add r3, r11, r0
	jal r31, luaX_newstring
	add r18, r1, r0
	ldw r3, r11+36
	ldw r24, r11+56
	ldw r1, r24+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	blt r1, r21, .LBB4_39
	jal r0, .LBB4_238
.LBB4_39:
	ldw r3, r11+40
	ldw r4, r24+0
	addi r6, r24, 8
	lui r9, %hi(.L.str.9)
	addi r9, r9, %lo(.L.str.9)
	add r7, r16, r0
	add r8, r17, r0
	jal r31, luaM_growaux_
	stw r24+0, r1
	ldw r3, r24+4
	addi r4, r3, 1
	stw r24+4, r4
	mul r3, r3, r16
	add r1, r1, r3
	stb r1+9, r23
	stw r1+16, r18
	ldw r3, r11+36
	ldw r18, r11+56
	ldw r1, r18+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	blt r1, r21, .LBB4_40
	jal r0, .LBB4_238
.LBB4_40:
	ldw r3, r11+40
	ldw r4, r18+0
	addi r6, r18, 8
	lui r9, %hi(.L.str.9)
	addi r9, r9, %lo(.L.str.9)
	add r7, r16, r0
	add r8, r17, r0
	jal r31, luaM_growaux_
	stw r18+0, r1
	ldw r3, r18+4
	addi r4, r3, 1
	stw r18+4, r4
	mul r3, r3, r16
	add r1, r1, r3
	stb r1+9, r23
	stw r1+16, r20
	ldw r1, r11+12
	beq r1, r19, .LBB4_41
	jal r0, .LBB4_249
.LBB4_41:
	add r3, r11, r0
	jal r31, luaX_next
	addi r16, fp, -100
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r16, r0
	jal r31, subexpr
	ldw r3, r11+36
	add r4, r16, r0
	jal r31, luaK_exp2nextreg
	ldw r1, r11+12
	beq r1, r22, .LBB4_42
	jal r0, .LBB4_250
.LBB4_42:
	add r3, r11, r0
	jal r31, luaX_next
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r16, r0
	jal r31, subexpr
	ldw r3, r11+36
	add r4, r16, r0
	jal r31, luaK_exp2nextreg
	ldw r1, r11+12
	beq r1, r22, .LBB4_43
	jal r0, .LBB4_207
.LBB4_43:
	add r3, r11, r0
	jal r31, luaX_next
	addi r15, fp, -100
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r15, r0
	jal r31, subexpr
	ldw r3, r11+36
	add r4, r15, r0
	jal r31, luaK_exp2nextreg
	jal r0, .LBB4_208
.LBB4_44:
	ldw r13, r11+36
	add r3, r13, r0
	jal r31, luaK_getlabel
	add r14, r1, r0
	addi r17, r0, 1
	addi r1, fp, -120
	stb r1+14, r17
	ldbu r3, r13+50
	stb r1+12, r3
	ldw r4, r13+8
	ldw r5, r4+56
	ldw r4, r5+28
	stw r1+4, r4
	ldw r5, r5+16
	stw r1+8, r5
	addi r15, r0, 0
	stb r1+13, r15
	ldw r7, r13+12
	add r6, r15, r0
	beq r7, r15, .LBB4_46
.LBB4_45:
	ldbu r6, r7+15
	addi r8, r0, 0
	sne r6, r6, r8
.LBB4_46:
	stb r1+15, r6
	stw r1+0, r7
	stw r13+12, r1
	addi r18, fp, -136
	stb r18+14, r15
	stb r18+12, r3
	stw r18+4, r4
	stw r18+8, r5
	stb r18+13, r15
	stb r18+15, r6
	stw r18+0, r1
	stw r13+12, r18
	add r3, r11, r0
	jal r31, luaX_next
	addi r16, r0, 29
	lui r1, 131104
	addi r19, r1, 7
	addi r20, r0, 14
	jal r0, .LBB4_48
.LBB4_47:
	add r3, r11, r0
	jal r31, statement
.LBB4_48:
	ldw r1, r11+12
	addi r1, r1, -259
	bgtu r1, r16, .LBB4_47
.LBB4_49:
	sll r3, r17, r1
	and r3, r3, r19
	bne r3, r15, .LBB4_52
.LBB4_50:
	bne r1, r20, .LBB4_47
.LBB4_51:
	add r3, r11, r0
	jal r31, statement
.LBB4_52:
	addi r4, r0, 276
	addi r5, r0, 272
	add r3, r11, r0
	add r6, r12, r0
	jal r31, check_match
	addi r15, fp, -100
	addi r12, r0, 0
	add r3, r11, r0
	add r4, r15, r0
	add r5, r12, r0
	jal r31, subexpr
	ldw r1, r15+0
	bne r1, r17, .LBB4_54
.LBB4_53:
	addi r1, r0, 3
	stw r15+0, r1
.LBB4_54:
	ldw r3, r11+36
	add r4, r15, r0
	jal r31, luaK_goiftrue
	ldw r16, r15+16
	add r3, r13, r0
	jal r31, leaveblock
	ldbu r1, r18+13
	beq r1, r12, .LBB4_174
.LBB4_55:
	add r3, r13, r0
	jal r31, luaK_jump
	add r15, r1, r0
	add r3, r13, r0
	add r4, r16, r0
	jal r31, luaK_patchtohere
	ldbu r4, r18+12
	addi r1, r4, 1
	addi r3, r0, 20
	mul r4, r4, r3
	addi r4, r4, -11
	addi r6, r0, 3
                                        # implicit-def: $r5
	jal r0, .LBB4_57
.LBB4_56:
	addi r4, r4, -20
	bne r7, r6, .LBB4_173
.LBB4_57:
	addi r1, r1, -1
	blt r1, r17, .LBB4_172
.LBB4_58:
	ldw r7, r13+8
	ldw r7, r7+56
	ldw r7, r7+0
	ldw r8, r13+40
	mul r8, r8, r3
	add r7, r7, r8
	add r8, r7, r4
	ldbu r7, r8+0
	beq r7, r6, .LBB4_56
.LBB4_59:
	ldbu r5, r8+1
	addi r5, r5, 1
	jal r0, .LBB4_56
.LBB4_60:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	addi r3, r0, 291
	bne r1, r3, .LBB4_154
.LBB4_61:
	ldw r12, r11+36
	ldw r14, r11+4
	ldw r15, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	ldw r3, r11+56
	ldw r1, r11+36
	ldw r4, r1+44
	ldw r5, r3+28
	addi r1, r0, 0
	add r13, r1, r0
	ble r5, r4, .LBB4_66
.LBB4_62:
	ldw r3, r3+24
	slli r6, r4, 4
	add r3, r3, r6
	sub r5, r5, r4
	addi r4, r0, 0
                                        # implicit-def: $r13
.LBB4_63:
	ldw r6, r3+0
	seq r7, r6, r15
	xor r8, r3, r13
	sub r7, r4, r7
	and r7, r8, r7
	xor r13, r13, r7
	beq r6, r15, .LBB4_66
.LBB4_64:
	addi r3, r3, 16
	addi r5, r5, -1
	bne r5, r4, .LBB4_63
.LBB4_65:
	add r13, r4, r0
.LBB4_66:
	beq r13, r1, .LBB4_164
.LBB4_67:
	ldbu r3, r13+12
	addi r7, r3, 1
	addi r1, r0, 20
	mul r3, r3, r1
	addi r8, r3, -11
	addi r3, r0, 0
	addi r4, r0, 1
	addi r6, r0, 3
                                        # implicit-def: $r5
	jal r0, .LBB4_69
.LBB4_68:
	addi r8, r8, -20
	bne r9, r6, .LBB4_176
.LBB4_69:
	addi r7, r7, -1
	blt r7, r4, .LBB4_175
.LBB4_70:
	ldw r9, r12+8
	ldw r9, r9+56
	ldw r9, r9+0
	ldw r10, r12+40
	mul r10, r10, r1
	add r9, r9, r10
	add r10, r9, r8
	ldbu r9, r10+0
	beq r9, r6, .LBB4_68
.LBB4_71:
	ldbu r5, r10+1
	addi r5, r5, 1
	jal r0, .LBB4_68
.LBB4_72:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	addi r3, r0, 291
	bne r1, r3, .LBB4_154
.LBB4_73:
	ldw r13, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	addi r14, r0, 287
	beq r1, r14, .LBB4_74
	jal r0, .LBB4_244
.LBB4_74:
	add r3, r11, r0
	jal r31, luaX_next
	addi r15, r0, 59
	jal r0, .LBB4_76
.LBB4_75:
	add r3, r11, r0
	jal r31, statement
.LBB4_76:
	ldw r1, r11+12
	beq r1, r14, .LBB4_75
.LBB4_77:
	beq r1, r15, .LBB4_75
.LBB4_78:
	ldw r5, r11+56
	ldw r3, r11+36
	ldw r6, r3+44
	ldw r7, r5+28
	addi r3, r0, 0
	add r4, r3, r0
	ble r7, r6, .LBB4_83
.LBB4_79:
	ldw r4, r5+24
	slli r5, r6, 4
	add r5, r4, r5
	sub r7, r7, r6
	addi r6, r0, 0
                                        # implicit-def: $r4
.LBB4_80:
	ldw r8, r5+0
	seq r9, r8, r13
	xor r10, r5, r4
	sub r9, r6, r9
	and r9, r10, r9
	xor r4, r4, r9
	beq r8, r13, .LBB4_83
.LBB4_81:
	addi r5, r5, 16
	addi r7, r7, -1
	bne r7, r6, .LBB4_80
.LBB4_82:
	add r4, r6, r0
.LBB4_83:
	bne r4, r3, .LBB4_236
.LBB4_84:
	addi r1, r1, -259
	addi r3, r0, 29
	bgtu r1, r3, .LBB4_206
.LBB4_85:
	addi r3, r0, 1
	sll r1, r3, r1
	lui r3, 131072
	addi r3, r3, 7
	and r1, r1, r3
	addi r3, r0, 0
	beq r1, r3, .LBB4_206
.LBB4_86:
	addi r6, r0, 1
.LBB4_87:
	add r3, r11, r0
	add r4, r13, r0
	add r5, r12, r0
	jal r31, createlabel
	jal r0, .LBB4_229
.LBB4_88:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r13, r11+36
	addi r14, r0, 0
	addi r1, fp, -100
	stb r1+14, r14
	ldbu r3, r13+50
	stb r1+12, r3
	ldw r3, r13+8
	ldw r3, r3+56
	ldw r4, r3+28
	stw r1+4, r4
	ldw r3, r3+16
	stw r1+8, r3
	stb r1+13, r14
	ldw r3, r13+12
	add r4, r14, r0
	beq r3, r14, .LBB4_90
.LBB4_89:
	ldbu r4, r3+15
	addi r5, r0, 0
	sne r4, r4, r5
.LBB4_90:
	stb r1+15, r4
	stw r1+0, r3
	stw r13+12, r1
	addi r15, r0, 29
	addi r16, r0, 1
	lui r1, 131104
	addi r17, r1, 7
	addi r18, r0, 14
	jal r0, .LBB4_92
.LBB4_91:
	add r3, r11, r0
	jal r31, statement
.LBB4_92:
	ldw r1, r11+12
	addi r1, r1, -259
	bgtu r1, r15, .LBB4_91
.LBB4_93:
	sll r3, r16, r1
	and r3, r3, r17
	bne r3, r14, .LBB4_96
.LBB4_94:
	bne r1, r18, .LBB4_91
.LBB4_95:
	add r3, r11, r0
	jal r31, statement
.LBB4_96:
	add r3, r13, r0
	jal r31, leaveblock
	addi r4, r0, 261
	addi r5, r0, 258
	add r3, r11, r0
	add r6, r12, r0
	jal r31, check_match
	jal r0, .LBB4_229
.LBB4_97:
	ldw r13, r11+36
	add r3, r11, r0
	jal r31, luaX_next
	add r3, r13, r0
	jal r31, luaK_getlabel
	add r14, r1, r0
	addi r16, fp, -100
	addi r17, r0, 0
	add r3, r11, r0
	add r4, r16, r0
	add r5, r17, r0
	jal r31, subexpr
	ldw r1, r16+0
	addi r18, r0, 1
	bne r1, r18, .LBB4_99
.LBB4_98:
	addi r1, r0, 3
	stw r16+0, r1
.LBB4_99:
	ldw r3, r11+36
	add r4, r16, r0
	jal r31, luaK_goiftrue
	ldw r15, r16+16
	addi r1, fp, -120
	stb r1+14, r18
	ldbu r3, r13+50
	stb r1+12, r3
	ldw r3, r13+8
	ldw r3, r3+56
	ldw r4, r3+28
	stw r1+4, r4
	ldw r3, r3+16
	stw r1+8, r3
	stb r1+13, r17
	ldw r3, r13+12
	beq r3, r17, .LBB4_101
.LBB4_100:
	ldbu r4, r3+15
	addi r5, r0, 0
	sne r17, r4, r5
.LBB4_101:
	stb r1+15, r17
	stw r1+0, r3
	stw r13+12, r1
	ldw r1, r11+12
	addi r3, r0, 258
	bne r1, r3, .LBB4_243
.LBB4_102:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r17, r11+36
	addi r19, r0, 0
	stb r16+14, r19
	ldbu r1, r17+50
	stb r16+12, r1
	ldw r1, r17+8
	ldw r1, r1+56
	ldw r3, r1+28
	stw r16+4, r3
	ldw r1, r1+16
	stw r16+8, r1
	stb r16+13, r19
	ldw r1, r17+12
	add r3, r19, r0
	beq r1, r19, .LBB4_104
.LBB4_103:
	ldbu r3, r1+15
	addi r4, r0, 0
	sne r3, r3, r4
.LBB4_104:
	stb r16+15, r3
	stw r16+0, r1
	stw r17+12, r16
	addi r16, r0, 29
	lui r1, 131104
	addi r20, r1, 7
	addi r21, r0, 14
	jal r0, .LBB4_106
.LBB4_105:
	add r3, r11, r0
	jal r31, statement
.LBB4_106:
	ldw r1, r11+12
	addi r1, r1, -259
	bgtu r1, r16, .LBB4_105
.LBB4_107:
	sll r3, r18, r1
	and r3, r3, r20
	bne r3, r19, .LBB4_110
.LBB4_108:
	bne r1, r21, .LBB4_105
.LBB4_109:
	add r3, r11, r0
	jal r31, statement
.LBB4_110:
	add r3, r17, r0
	jal r31, leaveblock
	add r3, r13, r0
	jal r31, luaK_jump
	add r3, r13, r0
	add r4, r1, r0
	add r5, r14, r0
	jal r31, luaK_patchlist
	addi r4, r0, 261
	addi r5, r0, 277
	add r3, r11, r0
	add r6, r12, r0
	jal r31, check_match
	add r3, r13, r0
	jal r31, leaveblock
	add r3, r13, r0
	add r4, r15, r0
	jal r31, luaK_patchtohere
	jal r0, .LBB4_229
.LBB4_111:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r12, r11+36
	ldbu r4, r12+50
	addi r1, r4, 1
	addi r3, r0, 20
	mul r4, r4, r3
	addi r4, r4, -11
	addi r5, r0, 0
	addi r19, r0, 1
	addi r6, r0, 3
                                        # implicit-def: $r13
	jal r0, .LBB4_113
.LBB4_112:
	addi r4, r4, -20
	bne r7, r6, .LBB4_131
.LBB4_113:
	addi r1, r1, -1
	blt r1, r19, .LBB4_130
.LBB4_114:
	ldw r7, r12+8
	ldw r7, r7+56
	ldw r7, r7+0
	ldw r8, r12+40
	mul r8, r8, r3
	add r7, r7, r8
	add r8, r7, r4
	ldbu r7, r8+0
	beq r7, r6, .LBB4_112
.LBB4_115:
	ldbu r8, r8+1
	addi r13, r8, 1
	jal r0, .LBB4_112
.LBB4_116:
	ldw r16, r11+36
	ldbu r15, r16+52
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	addi r5, r0, 11
	add r3, r11, r0
	jal r31, luaX_newstring
	add r19, r1, r0
	ldw r3, r11+36
	ldw r24, r11+56
	ldw r1, r24+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	addi r23, r0, 201
	bge r1, r23, .LBB4_238
.LBB4_117:
	ldw r3, r11+40
	ldw r4, r24+0
	addi r6, r24, 8
	lui r9, %hi(.L.str.9)
	addi r9, r9, %lo(.L.str.9)
	lui r1, 16
	addi r17, r1, -1
	addi r18, r0, 20
	add r7, r18, r0
	add r8, r17, r0
	jal r31, luaM_growaux_
	stw r24+0, r1
	ldw r3, r24+4
	addi r4, r3, 1
	stw r24+4, r4
	mul r3, r3, r18
	add r1, r1, r3
	addi r24, r0, 0
	stb r1+9, r24
	stw r1+16, r19
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	addi r5, r0, 11
	add r3, r11, r0
	jal r31, luaX_newstring
	add r19, r1, r0
	ldw r3, r11+36
	ldw r25, r11+56
	ldw r1, r25+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	bge r1, r23, .LBB4_238
.LBB4_118:
	ldw r3, r11+40
	ldw r4, r25+0
	addi r6, r25, 8
	lui r9, %hi(.L.str.9)
	addi r9, r9, %lo(.L.str.9)
	add r7, r18, r0
	add r8, r17, r0
	jal r31, luaM_growaux_
	stw r25+0, r1
	ldw r3, r25+4
	addi r4, r3, 1
	stw r25+4, r4
	mul r3, r3, r18
	add r1, r1, r3
	stb r1+9, r24
	stw r1+16, r19
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	addi r5, r0, 11
	add r3, r11, r0
	jal r31, luaX_newstring
	add r19, r1, r0
	ldw r3, r11+36
	ldw r25, r11+56
	ldw r1, r25+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	bge r1, r23, .LBB4_238
.LBB4_119:
	ldw r3, r11+40
	ldw r4, r25+0
	addi r6, r25, 8
	lui r9, %hi(.L.str.9)
	addi r9, r9, %lo(.L.str.9)
	add r7, r18, r0
	add r8, r17, r0
	jal r31, luaM_growaux_
	stw r25+0, r1
	ldw r3, r25+4
	addi r4, r3, 1
	stw r25+4, r4
	mul r3, r3, r18
	add r1, r1, r3
	stb r1+9, r24
	stw r1+16, r19
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	addi r5, r0, 11
	add r3, r11, r0
	jal r31, luaX_newstring
	add r19, r1, r0
	ldw r3, r11+36
	ldw r25, r11+56
	ldw r1, r25+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	bge r1, r23, .LBB4_238
.LBB4_120:
	ldw r3, r11+40
	ldw r4, r25+0
	addi r6, r25, 8
	lui r9, %hi(.L.str.9)
	addi r9, r9, %lo(.L.str.9)
	add r7, r18, r0
	add r8, r17, r0
	jal r31, luaM_growaux_
	stw r25+0, r1
	ldw r3, r25+4
	addi r4, r3, 1
	stw r25+4, r4
	mul r3, r3, r18
	add r1, r1, r3
	stb r1+9, r24
	stw r1+16, r19
	ldw r3, r11+36
	ldw r19, r11+56
	ldw r1, r19+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	bge r1, r23, .LBB4_238
.LBB4_121:
	ldw r3, r11+40
	ldw r4, r19+0
	addi r6, r19, 8
	lui r9, %hi(.L.str.9)
	addi r9, r9, %lo(.L.str.9)
	add r7, r18, r0
	add r8, r17, r0
	jal r31, luaM_growaux_
	stw r19+0, r1
	ldw r3, r19+4
	addi r4, r3, 1
	stw r19+4, r4
	mul r3, r3, r18
	add r1, r1, r3
	stb r1+9, r24
	stw r1+16, r20
	ldw r1, r11+12
	bne r1, r22, .LBB4_126
.LBB4_122:
	addi r14, r0, 1
	lui r19, %hi(.L.str.9)
	addi r19, r19, %lo(.L.str.9)
.LBB4_123:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	bne r1, r21, .LBB4_154
.LBB4_124:
	ldw r20, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	ldw r3, r11+36
	ldw r25, r11+56
	ldw r1, r25+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	bge r1, r23, .LBB4_238
.LBB4_125:
	ldw r3, r11+40
	ldw r4, r25+0
	addi r6, r25, 8
	add r7, r18, r0
	add r8, r17, r0
	add r9, r19, r0
	jal r31, luaM_growaux_
	stw r25+0, r1
	ldw r3, r25+4
	addi r4, r3, 1
	stw r25+4, r4
	mul r3, r3, r18
	add r1, r1, r3
	stb r1+9, r24
	stw r1+16, r20
	ldw r1, r11+12
	addi r14, r14, 1
	beq r1, r22, .LBB4_123
.LBB4_126:
	ldw r1, r11+12
	addi r3, r0, 267
	bne r1, r3, .LBB4_246
.LBB4_127:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r17, r11+4
	addi r21, fp, -100
	addi r18, r0, 0
	add r3, r11, r0
	add r4, r21, r0
	add r5, r18, r0
	jal r31, subexpr
	ldw r1, r11+12
	bne r1, r22, .LBB4_165
.LBB4_128:
	addi r23, r0, 1
	addi r19, fp, -100
	addi r20, r0, 0
.LBB4_129:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r3, r11+36
	add r4, r19, r0
	jal r31, luaK_exp2nextreg
	add r3, r11, r0
	add r4, r19, r0
	add r5, r20, r0
	jal r31, subexpr
	addi r23, r23, 1
	ldw r1, r11+12
	beq r1, r22, .LBB4_129
	jal r0, .LBB4_166
.LBB4_130:
	add r13, r5, r0
.LBB4_131:
	ldw r1, r11+12
	addi r18, r0, 59
	beq r1, r18, .LBB4_227
.LBB4_132:
	addi r1, r1, -259
	sltu r3, r1, r14
	lui r4, 262144
	addi r4, r4, -1
	and r1, r1, r4
	lui r4, 131104
	addi r4, r4, 7
	srl r1, r4, r1
	and r1, r3, r1
	addi r3, r0, 0
	bne r1, r3, .LBB4_227
.LBB4_133:
	addi r14, fp, -100
	addi r15, r0, 0
	add r3, r11, r0
	add r4, r14, r0
	add r5, r15, r0
	jal r31, subexpr
	ldw r1, r11+12
	addi r20, r0, 44
	bne r1, r20, .LBB4_136
.LBB4_134:
	addi r19, r0, 1
	addi r16, fp, -100
	addi r17, r0, 0
.LBB4_135:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r3, r11+36
	add r4, r16, r0
	jal r31, luaK_exp2nextreg
	add r3, r11, r0
	add r4, r16, r0
	add r5, r17, r0
	jal r31, subexpr
	addi r19, r19, 1
	ldw r1, r11+12
	beq r1, r20, .LBB4_135
.LBB4_136:
	ldw r1, r14+0
	addi r3, r0, -2
	and r1, r1, r3
	addi r17, r0, 18
	bne r1, r17, .LBB4_185
.LBB4_137:
	addi r5, r0, -1
	add r3, r12, r0
	add r4, r14, r0
	add r16, r5, r0
	jal r31, luaK_setreturns
	ldw r1, r14+0
	bne r1, r17, .LBB4_225
.LBB4_138:
	addi r1, r0, 1
	bne r19, r1, .LBB4_225
.LBB4_139:
	ldw r1, r12+12
	ldbu r1, r1+15
	add r5, r16, r0
	bne r1, r15, .LBB4_227
.LBB4_140:
	ldw r1, r12+0
	ldw r1, r1+52
	ldw r3, r14+4
	slli r3, r3, 2
	add r1, r1, r3
	ldw r3, r1+0
	addi r4, r0, -128
	and r3, r3, r4
	addi r3, r3, 69
	stw r1+0, r3
	jal r0, .LBB4_227
.LBB4_141:
	addi r23, r0, 291
	bne r1, r23, .LBB4_154
.LBB4_142:
	ldw r12, r11+36
	addi r24, r0, -1
	addi r25, r0, 1
	addi r21, r0, 255
	addi r26, r0, 201
	lui r15, %hi(.L.str.9)
	addi r15, r15, %lo(.L.str.9)
	lui r1, 16
	addi r16, r1, -1
	addi r13, r0, 20
	addi r27, r0, 0
	addi r28, r0, 60
	addi r1, r0, 62
	stw fp+-140, r1
	lui r1, %hi(.L.str.24)
	addi r1, r1, %lo(.L.str.24)
	stw fp+-144, r1
	lui r1, %hi(.L.str.25)
	addi r1, r1, %lo(.L.str.25)
	stw fp+-152, r1
	addi r1, r0, 2
	stw fp+-148, r1
	addi r18, r0, 44
	add r14, r25, r0
	add r17, r24, r0
.LBB4_143:
	ldw r20, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	ldw r19, r11+36
	ldw r22, r11+56
	ldw r1, r22+4
	addi r5, r1, 1
	ldw r1, r19+40
	sub r1, r5, r1
	bge r1, r26, .LBB4_239
.LBB4_144:
	ldw r3, r11+40
	ldw r4, r22+0
	addi r6, r22, 8
	add r7, r13, r0
	add r8, r16, r0
	add r9, r15, r0
	jal r31, luaM_growaux_
	stw r22+0, r1
	ldw r3, r22+4
	addi r4, r3, 1
	stw r22+4, r4
	mul r3, r3, r13
	add r1, r1, r3
	stb r1+9, r27
	stw r1+16, r20
	ldw r1, r22+4
	ldw r3, r19+40
	xor r3, r3, r24
	add r20, r1, r3
	ldw r1, r11+12
	add r3, r27, r0
	bne r1, r28, .LBB4_151
.LBB4_145:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	bne r1, r23, .LBB4_154
.LBB4_146:
	ldw r19, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	ldw r3, fp+-140
	bne r1, r3, .LBB4_240
.LBB4_147:
	addi r19, r19, 16
	add r3, r11, r0
	jal r31, luaX_next
	add r3, r19, r0
	ldw r4, fp+-144
	jal r31, strcmp
	add r3, r25, r0
	beq r1, r27, .LBB4_151
.LBB4_148:
	add r3, r19, r0
	ldw r4, fp+-152
	jal r31, strcmp
	bne r1, r27, .LBB4_241
.LBB4_149:
	ldw r1, r12+8
	ldw r1, r1+56
	ldw r1, r1+0
	ldw r3, r12+40
	mul r3, r3, r13
	add r1, r1, r3
	mul r3, r20, r13
	add r1, r1, r3
	ldw r3, fp+-148
	stb r1+9, r3
	bne r17, r24, .LBB4_242
.LBB4_150:
	ldbu r1, r12+50
	add r1, r14, r1
	addi r17, r1, -1
	jal r0, .LBB4_152
.LBB4_151:
	ldw r1, r12+8
	ldw r1, r1+56
	ldw r1, r1+0
	ldw r4, r12+40
	mul r4, r4, r13
	add r1, r1, r4
	mul r4, r20, r13
	add r1, r1, r4
	stb r1+9, r3
.LBB4_152:
	ldw r1, r11+12
	bne r1, r18, .LBB4_160
.LBB4_153:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	addi r14, r14, 1
	addi r21, r21, -1
	beq r1, r23, .LBB4_143
.LBB4_154:
	addi r4, r0, 291
	add r3, r11, r0
	jal r31, error_expected
.LBB4_155:
	addi r5, r0, 0
.LBB4_156:
	addi r4, fp, -100
	add r3, r11, r0
	add r6, r12, r0
	jal r31, body
	ldw r3, r11+36
	ldw r1, r13+0
	addi r4, r0, 9
	beq r1, r4, .LBB4_189
.LBB4_157:
	addi r4, r0, 10
	beq r1, r4, .LBB4_187
.LBB4_158:
	addi r4, r0, 11
	bne r1, r4, .LBB4_192
.LBB4_159:
	ldw r1, r11+56
	ldw r1, r1+0
	ldw r4, r13+4
	addi r5, r0, 20
	mul r4, r4, r5
	add r1, r1, r4
	ldw r1, r1+16
	jal r0, .LBB4_191
.LBB4_160:
	addi r3, r0, 61
	bne r1, r3, .LBB4_193
.LBB4_161:
	add r3, r11, r0
	jal r31, luaX_next
	addi r4, fp, -100
	add r3, r11, r0
	jal r31, explist
	add r15, r1, r0
	jal r0, .LBB4_194
.LBB4_162:
	ldw r1, r12+4
	addi r3, r0, 18
	bne r1, r3, .LBB4_247
.LBB4_163:
	ldw r1, r13+0
	ldw r1, r1+52
	ldw r3, r12+8
	slli r3, r3, 2
	add r1, r1, r3
	addi r3, r0, 1
	stb r1+3, r3
	jal r0, .LBB4_229
.LBB4_164:
	add r3, r12, r0
	jal r31, luaK_jump
	add r12, r1, r0
	ldw r16, r11+56
	ldw r13, r16+16
	ldw r3, r11+40
	ldw r4, r16+12
	addi r6, r16, 20
	lui r9, %hi(.L.str.13)
	addi r9, r9, %lo(.L.str.13)
	lui r1, 8
	addi r8, r1, -1
	addi r7, r0, 16
	add r5, r13, r0
	jal r31, luaM_growaux_
	stw r16+12, r1
	slli r3, r13, 4
	add r1, r1, r3
	stw r1+0, r15
	stw r1+8, r14
	ldw r3, r11+36
	ldbu r3, r3+50
	stb r1+12, r3
	addi r3, r0, 0
	stb r1+13, r3
	stw r1+4, r12
	addi r1, r13, 1
	stw r16+16, r1
	jal r0, .LBB4_229
.LBB4_165:
	addi r23, r0, 1
.LBB4_166:
	ldw r19, r11+36
	addi r1, r0, 4
	sub r20, r1, r23
	ldw r1, r21+0
	addi r3, r1, -18
	addi r4, r0, 2
	bgeu r3, r4, .LBB4_169
.LBB4_167:
	addi r1, r0, -1
	xor r3, r20, r1
	sgt r1, r20, r1
	sub r1, r18, r1
	and r1, r3, r1
	sub r5, r18, r1
	addi r4, fp, -100
	add r3, r19, r0
	jal r31, luaK_setreturns
	addi r1, r0, 3
	bleu r23, r1, .LBB4_211
.LBB4_168:
	ldbu r1, r19+52
	add r1, r1, r20
	stb r19+52, r1
	jal r0, .LBB4_212
.LBB4_169:
	bne r1, r18, .LBB4_209
.LBB4_170:
	addi r1, r0, 3
	bgtu r23, r1, .LBB4_210
.LBB4_171:
	ldbu r4, r19+52
	add r3, r19, r0
	add r5, r20, r0
	jal r31, luaK_nil
	addi r1, r0, 3
	bleu r23, r1, .LBB4_211
	jal r0, .LBB4_168
.LBB4_172:
	add r5, r12, r0
.LBB4_173:
	addi r4, r0, 54
	addi r6, r0, 0
	add r3, r13, r0
	add r7, r6, r0
	add r8, r6, r0
	jal r31, luaK_codeABCk
	add r3, r13, r0
	jal r31, luaK_jump
	add r16, r1, r0
	add r3, r13, r0
	add r4, r15, r0
	jal r31, luaK_patchtohere
.LBB4_174:
	add r3, r13, r0
	add r4, r16, r0
	add r5, r14, r0
	jal r31, luaK_patchlist
	jal r0, .LBB4_214
.LBB4_175:
	add r5, r3, r0
.LBB4_176:
	ldbu r8, r12+50
	addi r7, r8, 1
	mul r8, r8, r1
	addi r8, r8, -11
                                        # implicit-def: $r9
	jal r0, .LBB4_178
.LBB4_177:
	addi r8, r8, -20
	bne r10, r6, .LBB4_182
.LBB4_178:
	addi r7, r7, -1
	blt r7, r4, .LBB4_181
.LBB4_179:
	ldw r10, r12+8
	ldw r10, r10+56
	ldw r10, r10+0
	ldw r14, r12+40
	mul r14, r14, r1
	add r10, r10, r14
	add r14, r10, r8
	ldbu r10, r14+0
	beq r10, r6, .LBB4_177
.LBB4_180:
	ldbu r9, r14+1
	addi r9, r9, 1
	jal r0, .LBB4_177
.LBB4_181:
	add r9, r3, r0
.LBB4_182:
	ble r9, r5, .LBB4_184
.LBB4_183:
	addi r4, r0, 54
	addi r6, r0, 0
	add r3, r12, r0
	add r7, r6, r0
	add r8, r6, r0
	jal r31, luaK_codeABCk
.LBB4_184:
	add r3, r12, r0
	jal r31, luaK_jump
	ldw r5, r13+4
	add r3, r12, r0
	add r4, r1, r0
	jal r31, luaK_patchlist
	jal r0, .LBB4_229
.LBB4_185:
	addi r4, fp, -100
	add r3, r12, r0
	addi r1, r0, 1
	bne r19, r1, .LBB4_226
.LBB4_186:
	add r13, r1, r0
	jal r31, luaK_exp2anyreg
	add r5, r13, r0
	add r13, r1, r0
	jal r0, .LBB4_227
.LBB4_187:
	ldw r1, r3+0
	ldw r1, r1+60
	ldw r4, r13+4
	slli r4, r4, 3
	add r4, r1, r4
	ldbu r5, r4+6
	addi r1, r0, 0
	beq r5, r1, .LBB4_191
.LBB4_188:
	ldw r1, r4+0
	jal r0, .LBB4_191
.LBB4_189:
	ldhu r1, r13+6
	ldw r4, r3+8
	ldw r4, r4+56
	ldw r4, r4+0
	ldw r5, r3+40
	addi r6, r0, 20
	mul r5, r5, r6
	add r4, r4, r5
	mul r1, r1, r6
	add r4, r4, r1
	ldbu r5, r4+9
	addi r1, r0, 0
	beq r5, r1, .LBB4_191
.LBB4_190:
	ldw r1, r4+16
.LBB4_191:
	addi r4, r0, 0
	bne r1, r4, .LBB4_245
.LBB4_192:
	addi r4, fp, -120
	addi r5, fp, -100
	jal r31, luaK_storevar
	ldw r3, r11+36
	add r4, r12, r0
	jal r31, luaK_fixline
	jal r0, .LBB4_229
.LBB4_193:
	addi r15, r0, 0
	addi r1, fp, -100
	stw r1+0, r15
.LBB4_194:
	add r19, r17, r0
	sub r16, r14, r15
	addi r18, r0, 0
	bne r16, r18, .LBB4_198
.LBB4_195:
	ldw r1, r12+8
	ldw r1, r1+56
	ldw r1, r1+0
	ldw r3, r12+40
	mul r3, r3, r13
	add r1, r1, r3
	mul r3, r20, r13
	add r17, r1, r3
	ldbu r1, r17+9
	addi r3, r0, 1
	bne r1, r3, .LBB4_198
.LBB4_196:
	addi r4, fp, -100
	add r3, r12, r0
	add r5, r17, r0
	jal r31, luaK_exp2const
	addi r3, r0, 0
	beq r1, r3, .LBB4_198
.LBB4_197:
	addi r4, r14, -1
	addi r1, r0, 3
	stb r17+9, r1
	add r3, r11, r0
	jal r31, adjustlocalvars
	ldbu r1, r12+50
	addi r1, r1, 1
	stb r12+50, r1
	jal r0, .LBB4_217
.LBB4_198:
	ldw r17, r11+36
	addi r1, fp, -100
	ldw r1, r1+0
	addi r3, r1, -18
	ldw r4, fp+-148
	bgeu r3, r4, .LBB4_200
.LBB4_199:
	addi r1, r0, -1
	xor r3, r16, r1
	sgt r1, r16, r1
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	sub r5, r4, r1
	addi r4, fp, -100
	add r3, r17, r0
	jal r31, luaK_setreturns
	jal r0, .LBB4_204
.LBB4_200:
	addi r3, r0, 0
	beq r1, r3, .LBB4_202
.LBB4_201:
	addi r4, fp, -100
	add r3, r17, r0
	jal r31, luaK_exp2nextreg
.LBB4_202:
	addi r1, r0, 1
	blt r16, r1, .LBB4_204
.LBB4_203:
	ldbu r4, r17+52
	add r3, r17, r0
	add r5, r16, r0
	jal r31, luaK_nil
.LBB4_204:
	addi r1, r0, 1
	blt r16, r1, .LBB4_215
.LBB4_205:
	add r3, r17, r0
	add r4, r16, r0
	jal r31, luaK_reserveregs
	jal r0, .LBB4_216
.LBB4_206:
	addi r6, r0, 0
	jal r0, .LBB4_87
.LBB4_207:
	ldbu r4, r15+52
	addi r16, r0, 1
	add r3, r15, r0
	add r5, r16, r0
	jal r31, luaK_int
	add r3, r15, r0
	add r4, r16, r0
	jal r31, luaK_reserveregs
.LBB4_208:
	addi r4, r0, 3
	add r3, r11, r0
	jal r31, adjustlocalvars
	addi r6, r0, 1
	addi r7, r0, 0
	add r3, r11, r0
	add r4, r14, r0
	add r5, r12, r0
	jal r0, .LBB4_213
.LBB4_209:
	addi r4, fp, -100
	add r3, r19, r0
	jal r31, luaK_exp2nextreg
	addi r1, r0, 3
	bleu r23, r1, .LBB4_171
.LBB4_210:
	addi r1, r0, 3
	bgtu r23, r1, .LBB4_168
.LBB4_211:
	add r3, r19, r0
	add r4, r20, r0
	jal r31, luaK_reserveregs
.LBB4_212:
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, adjustlocalvars
	ldw r1, r16+12
	addi r18, r0, 1
	stb r1+13, r18
	stb r1+15, r18
	stb r16+54, r18
	addi r4, r0, 3
	add r3, r16, r0
	jal r31, luaK_checkstack
	add r3, r11, r0
	add r4, r15, r0
	add r5, r17, r0
	add r6, r14, r0
	add r7, r18, r0
.LBB4_213:
	jal r31, forbody
	addi r4, r0, 261
	addi r5, r0, 263
	add r3, r11, r0
	add r6, r12, r0
	jal r31, check_match
.LBB4_214:
	add r3, r13, r0
	jal r31, leaveblock
	jal r0, .LBB4_229
.LBB4_215:
	ldbu r1, r17+52
	sub r1, r1, r15
	sub r1, r1, r21
	stb r17+52, r1
.LBB4_216:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, adjustlocalvars
.LBB4_217:
	addi r1, r0, -1
	beq r19, r1, .LBB4_229
.LBB4_218:
	ldw r3, r12+12
	addi r1, r0, 1
	stb r3+13, r1
	stb r3+15, r1
	stb r12+54, r1
	addi r3, r19, 1
	mul r4, r19, r13
	addi r4, r4, -11
	addi r6, r0, 3
                                        # implicit-def: $r5
	jal r0, .LBB4_220
.LBB4_219:
	addi r4, r4, -20
	bne r7, r6, .LBB4_224
.LBB4_220:
	addi r3, r3, -1
	blt r3, r1, .LBB4_223
.LBB4_221:
	ldw r7, r12+8
	ldw r7, r7+56
	ldw r7, r7+0
	ldw r8, r12+40
	mul r8, r8, r13
	add r7, r7, r8
	add r8, r7, r4
	ldbu r7, r8+0
	beq r7, r6, .LBB4_219
.LBB4_222:
	ldbu r5, r8+1
	addi r5, r5, 1
	jal r0, .LBB4_219
.LBB4_223:
	add r5, r18, r0
.LBB4_224:
	addi r4, r0, 55
	addi r6, r0, 0
	add r3, r12, r0
	add r7, r6, r0
	add r8, r6, r0
	jal r31, luaK_codeABCk
	jal r0, .LBB4_229
.LBB4_225:
	add r5, r16, r0
	jal r0, .LBB4_227
.LBB4_226:
	jal r31, luaK_exp2nextreg
	add r5, r19, r0
.LBB4_227:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, luaK_ret
	ldw r1, r11+12
	bne r1, r18, .LBB4_229
.LBB4_228:
	add r3, r11, r0
	jal r31, luaX_next
.LBB4_229:
	ldw r1, r11+36
	ldbu r5, r1+50
	addi r3, r5, 1
	addi r4, r0, 20
	mul r5, r5, r4
	addi r6, r5, -11
	addi r5, r0, 0
	addi r7, r0, 1
	addi r8, r0, 3
                                        # implicit-def: $r9
	jal r0, .LBB4_231
.LBB4_230:
	addi r6, r6, -20
	bne r10, r8, .LBB4_235
.LBB4_231:
	addi r3, r3, -1
	blt r3, r7, .LBB4_234
.LBB4_232:
	ldw r10, r1+8
	ldw r10, r10+56
	ldw r10, r10+0
	ldw r12, r1+40
	mul r12, r12, r4
	add r10, r10, r12
	add r12, r10, r6
	ldbu r10, r12+0
	beq r10, r8, .LBB4_230
.LBB4_233:
	ldbu r9, r12+1
	addi r9, r9, 1
	jal r0, .LBB4_230
.LBB4_234:
	add r9, r5, r0
.LBB4_235:
	stb r1+52, r9
	ldw r1, r11+40
	ldw r3, r1+96
	addi r3, r3, -1
	stw r1+96, r3
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
	addi sp, sp, 168
	jalr r0, r31, 0
.LBB4_236:
	ldw r3, r11+40
	addi r5, r13, 16
	ldw r6, r4+8
	lui r4, %hi(.L.str.27)
	addi r4, r4, %lo(.L.str.27)
.LBB4_237:
	jal r31, luaO_pushfstring
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaK_semerror
.LBB4_238:
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
	addi r4, r0, 200
	jal r31, errorlimit
.LBB4_239:
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
	addi r4, r0, 200
	add r3, r19, r0
	jal r31, errorlimit
.LBB4_240:
	addi r4, r0, 62
	add r3, r11, r0
	jal r31, error_expected
.LBB4_241:
	ldw r3, r11+40
	lui r4, %hi(.L.str.26)
	addi r4, r4, %lo(.L.str.26)
	add r5, r19, r0
	jal r0, .LBB4_237
.LBB4_242:
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	add r3, r11, r0
	jal r31, luaK_semerror
.LBB4_243:
	addi r4, r0, 258
	add r3, r11, r0
	jal r31, error_expected
.LBB4_244:
	addi r4, r0, 287
	add r3, r11, r0
	jal r31, error_expected
.LBB4_245:
	ldw r3, r11+40
	addi r5, r1, 16
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	jal r0, .LBB4_237
.LBB4_246:
	addi r4, r0, 267
	add r3, r11, r0
	jal r31, error_expected
.LBB4_247:
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	add r3, r11, r0
	jal r31, luaX_syntaxerror
.LBB4_248:
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r3, r11, r0
	jal r31, luaX_syntaxerror
.LBB4_249:
	addi r4, r0, 61
	add r3, r11, r0
	jal r31, error_expected
.LBB4_250:
	addi r4, r0, 44
	add r3, r11, r0
	jal r31, error_expected
.Lfunc_end4:
	.size	statement, .Lfunc_end4-statement
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI4_0:
	.word	.LBB4_2
	.word	.LBB4_88
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_30
	.word	.LBB4_24
	.word	.LBB4_60
	.word	.LBB4_11
	.word	.LBB4_4
	.word	.LBB4_7
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_44
	.word	.LBB4_111
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_97
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_72
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function check_match
	.type	check_match,@function
check_match:                            # @check_match
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
	ldw r1, r3+12
	bne r1, r4, .LBB5_2
.LBB5_1:
	jal r31, luaX_next
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
.LBB5_2:
	add r13, r3, r0
	ldw r3, r3+4
	bne r6, r3, .LBB5_4
.LBB5_3:
	add r3, r13, r0
	jal r31, error_expected
.LBB5_4:
	add r12, r5, r0
	add r11, r6, r0
	ldw r14, r13+40
	add r3, r13, r0
	jal r31, luaX_token2str
	add r15, r1, r0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, luaX_token2str
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r14, r0
	add r5, r15, r0
	add r6, r1, r0
	add r7, r11, r0
	jal r31, luaO_pushfstring
	add r3, r13, r0
	add r4, r1, r0
	jal r31, luaX_syntaxerror
.Lfunc_end5:
	.size	check_match, .Lfunc_end5-check_match
                                        # -- End function
	.p2align	2                               # -- Begin function test_then_block
	.type	test_then_block,@function
test_then_block:                        # @test_then_block
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
	stw fp+-40, lr
	add r12, r4, r0
	add r13, r3, r0
	ldw r11, r3+36
	jal r31, luaX_next
	addi r4, fp, -76
	addi r14, r0, 0
	add r3, r13, r0
	add r5, r14, r0
	jal r31, subexpr
	ldw r1, r13+12
	addi r3, r0, 274
	bne r1, r3, .LBB6_22
.LBB6_1:
	add r3, r13, r0
	jal r31, luaX_next
	ldw r1, r13+12
	addi r3, r0, 257
	bne r1, r3, .LBB6_8
.LBB6_2:
	ldw r16, r13+4
	ldw r3, r13+36
	addi r14, fp, -76
	add r4, r14, r0
	jal r31, luaK_goiffalse
	add r3, r13, r0
	jal r31, luaX_next
	addi r17, r0, 0
	addi r1, fp, -56
	stb r1+14, r17
	ldbu r3, r11+50
	stb r1+12, r3
	ldw r3, r11+8
	ldw r3, r3+56
	ldw r4, r3+28
	stw r1+4, r4
	ldw r3, r3+16
	stw r1+8, r3
	stb r1+13, r17
	ldw r3, r11+12
	add r4, r17, r0
	beq r3, r17, .LBB6_4
.LBB6_3:
	ldbu r4, r3+15
	addi r5, r0, 0
	sne r4, r4, r5
.LBB6_4:
	stb r1+15, r4
	stw r1+0, r3
	stw r11+12, r1
	ldw r3, r13+40
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	addi r5, r0, 5
	jal r31, luaS_newlstr
	add r15, r1, r0
	ldw r18, r14+12
	ldw r19, r13+56
	ldw r14, r19+16
	ldw r3, r13+40
	ldw r4, r19+12
	addi r6, r19, 20
	lui r9, %hi(.L.str.13)
	addi r9, r9, %lo(.L.str.13)
	lui r1, 8
	addi r8, r1, -1
	addi r7, r0, 16
	add r5, r14, r0
	jal r31, luaM_growaux_
	stw r19+12, r1
	slli r3, r14, 4
	add r1, r1, r3
	stw r1+0, r15
	stw r1+8, r16
	ldw r3, r13+36
	ldbu r3, r3+50
	stb r1+12, r3
	stb r1+13, r17
	stw r1+4, r18
	addi r1, r14, 1
	stw r19+16, r1
	ldw r1, r13+12
	addi r14, r0, 59
	bne r1, r14, .LBB6_6
.LBB6_5:
	add r3, r13, r0
	jal r31, luaX_next
	ldw r1, r13+12
	beq r1, r14, .LBB6_5
.LBB6_6:
	ldw r1, r13+12
	addi r1, r1, -259
	addi r3, r0, 29
	sgtu r3, r1, r3
	lui r4, 262144
	addi r4, r4, -1
	and r1, r1, r4
	lui r4, 131072
	addi r4, r4, -8
	srl r1, r4, r1
	or  r1, r3, r1
	andi r15, r1, 1
	add r3, r11, r0
	addi r16, r0, 0
	beq r15, r16, .LBB6_21
.LBB6_7:
	jal r31, luaK_jump
	add r14, r1, r0
	bne r15, r16, .LBB6_11
	jal r0, .LBB6_20
.LBB6_8:
	ldw r3, r13+36
	addi r15, fp, -76
	add r4, r15, r0
	jal r31, luaK_goiftrue
	addi r1, fp, -56
	stb r1+14, r14
	ldbu r3, r11+50
	stb r1+12, r3
	ldw r3, r11+8
	ldw r3, r3+56
	ldw r4, r3+28
	stw r1+4, r4
	ldw r3, r3+16
	stw r1+8, r3
	stb r1+13, r14
	ldw r3, r11+12
	beq r3, r14, .LBB6_10
.LBB6_9:
	ldbu r4, r3+15
	addi r5, r0, 0
	sne r14, r4, r5
.LBB6_10:
	stb r1+15, r14
	stw r1+0, r3
	stw r11+12, r1
	ldw r14, r15+16
.LBB6_11:
	addi r16, r0, 29
	addi r15, r0, 1
	lui r1, 131104
	addi r17, r1, 7
	addi r18, r0, 0
	addi r19, r0, 14
	jal r0, .LBB6_13
.LBB6_12:
	add r3, r13, r0
	jal r31, statement
.LBB6_13:
	ldw r1, r13+12
	addi r1, r1, -259
	bgtu r1, r16, .LBB6_12
.LBB6_14:
	sll r3, r15, r1
	and r3, r3, r17
	bne r3, r18, .LBB6_17
.LBB6_15:
	bne r1, r19, .LBB6_12
.LBB6_16:
	add r3, r13, r0
	jal r31, statement
.LBB6_17:
	add r3, r11, r0
	jal r31, leaveblock
	ldw r1, r13+12
	addi r1, r1, -259
	bgtu r1, r15, .LBB6_19
.LBB6_18:
	add r3, r11, r0
	jal r31, luaK_jump
	add r3, r11, r0
	add r4, r12, r0
	add r5, r1, r0
	jal r31, luaK_concat
.LBB6_19:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaK_patchtohere
.LBB6_20:
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
	addi sp, sp, 88
	jalr r0, r31, 0
.LBB6_21:
	jal r31, leaveblock
                                        # implicit-def: $r14
	bne r15, r16, .LBB6_11
	jal r0, .LBB6_20
.LBB6_22:
	addi r4, r0, 274
	add r3, r13, r0
	jal r31, error_expected
.Lfunc_end6:
	.size	test_then_block, .Lfunc_end6-test_then_block
                                        # -- End function
	.p2align	2                               # -- Begin function leaveblock
	.type	leaveblock,@function
leaveblock:                             # @leaveblock
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
	add r11, r3, r0
	ldw r15, r3+12
	ldw r12, r3+8
	ldbu r1, r15+12
	addi r3, r1, 1
	addi r16, r0, 20
	mul r4, r1, r16
	addi r5, r4, -11
	addi r4, r0, 0
	addi r17, r0, 1
	addi r18, r0, 3
                                        # implicit-def: $r14
	jal r0, .LBB7_2
.LBB7_1:
	addi r5, r5, -20
	bne r6, r18, .LBB7_6
.LBB7_2:
	addi r3, r3, -1
	blt r3, r17, .LBB7_5
.LBB7_3:
	ldw r6, r12+56
	ldw r6, r6+0
	ldw r7, r11+40
	mul r7, r7, r16
	add r6, r6, r7
	add r7, r6, r5
	ldbu r6, r7+0
	beq r6, r18, .LBB7_1
.LBB7_4:
	ldbu r7, r7+1
	addi r14, r7, 1
	jal r0, .LBB7_1
.LBB7_5:
	add r14, r4, r0
.LBB7_6:
	ldbu r3, r11+50
	sub r5, r1, r3
	ldw r4, r12+56
	ldw r6, r4+4
	add r5, r5, r6
	stw r4+4, r5
	bgeu r1, r3, .LBB7_13
.LBB7_7:
	ldw r4, r4+0
	ldw r5, r11+40
	mul r5, r5, r16
	addi r6, r3, -1
	andi r6, r6, 255
	mul r6, r6, r16
	add r5, r5, r6
	add r4, r5, r4
	addi r4, r4, 12
	addi r5, r0, 0
	addi r6, r0, 12
	jal r0, .LBB7_9
.LBB7_8:
	andi r7, r3, 255
	addi r4, r4, -20
	bleu r7, r1, .LBB7_13
.LBB7_9:
	addi r3, r3, -1
	stb r11+50, r3
	ldbu r8, r4+-3
	add r7, r5, r0
	beq r8, r18, .LBB7_11
.LBB7_10:
	ldh r7, r4+0
	ldw r8, r11+0
	ldw r8, r8+72
	mul r7, r7, r6
	add r7, r8, r7
.LBB7_11:
	beq r7, r5, .LBB7_8
.LBB7_12:
	ldw r8, r11+16
	stw r7+8, r8
	jal r0, .LBB7_8
.LBB7_13:
	ldbu r1, r15+14
	addi r13, r0, 0
	beq r1, r13, .LBB7_15
.LBB7_14:
	ldw r3, r12+40
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	addi r5, r0, 5
	jal r31, luaS_newlstr
	add r3, r12, r0
	add r4, r1, r0
	add r5, r13, r0
	add r6, r13, r0
	jal r31, createlabel
	bne r1, r13, .LBB7_18
.LBB7_15:
	ldw r3, r15+0
	addi r1, r0, 0
	beq r3, r1, .LBB7_18
.LBB7_16:
	ldbu r3, r15+13
	beq r3, r1, .LBB7_18
.LBB7_17:
	addi r4, r0, 54
	addi r6, r0, 0
	add r3, r11, r0
	add r5, r14, r0
	add r7, r6, r0
	add r8, r6, r0
	jal r31, luaK_codeABCk
.LBB7_18:
	stb r11+52, r14
	ldw r3, r15+4
	ldw r1, r12+56
	stw r1+28, r3
	ldw r3, r15+0
	stw r11+12, r3
	beq r3, r13, .LBB7_36
.LBB7_19:
	ldw r1, r11+8
	ldw r1, r1+56
	ldw r3, r15+8
	ldw r4, r1+16
	bge r3, r4, .LBB7_37
.LBB7_20:
	ldw r5, r1+12
	ldbu r6, r15+12
	addi r7, r6, 1
	mul r8, r6, r16
	addi r8, r8, -11
	jal r0, .LBB7_22
.LBB7_21:
	stb r9+12, r6
	addi r3, r3, 1
	beq r3, r4, .LBB7_37
.LBB7_22:
	slli r9, r3, 4
	add r9, r5, r9
	ldbu r10, r9+12
	addi r12, r10, 1
	mul r10, r10, r16
	addi r14, r10, -11
                                        # implicit-def: $r10
	jal r0, .LBB7_24
.LBB7_23:
	addi r14, r14, -20
	bne r19, r18, .LBB7_28
.LBB7_24:
	addi r12, r12, -1
	blt r12, r17, .LBB7_27
.LBB7_25:
	ldw r19, r1+0
	ldw r20, r11+40
	mul r20, r20, r16
	add r19, r19, r20
	add r20, r19, r14
	ldbu r19, r20+0
	beq r19, r18, .LBB7_23
.LBB7_26:
	ldbu r10, r20+1
	addi r10, r10, 1
	jal r0, .LBB7_23
.LBB7_27:
	add r10, r13, r0
.LBB7_28:
	add r12, r8, r0
	add r19, r7, r0
                                        # implicit-def: $r14
	jal r0, .LBB7_30
.LBB7_29:
	addi r12, r12, -20
	bne r20, r18, .LBB7_34
.LBB7_30:
	addi r19, r19, -1
	blt r19, r17, .LBB7_33
.LBB7_31:
	ldw r20, r1+0
	ldw r21, r11+40
	mul r21, r21, r16
	add r20, r20, r21
	add r21, r20, r12
	ldbu r20, r21+0
	beq r20, r18, .LBB7_29
.LBB7_32:
	ldbu r14, r21+1
	addi r14, r14, 1
	jal r0, .LBB7_29
.LBB7_33:
	add r14, r13, r0
.LBB7_34:
	ble r10, r14, .LBB7_21
.LBB7_35:
	ldbu r10, r15+13
	ldbu r12, r9+13
	or  r10, r12, r10
	stb r9+13, r10
	jal r0, .LBB7_21
.LBB7_36:
	ldw r3, r15+8
	ldw r4, r1+16
	blt r3, r4, .LBB7_38
.LBB7_37:
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
.LBB7_38:
	ldw r1, r1+12
	slli r3, r3, 4
	add r4, r1, r3
	add r3, r12, r0
	jal r31, undefgoto
.Lfunc_end7:
	.size	leaveblock, .Lfunc_end7-leaveblock
                                        # -- End function
	.p2align	2                               # -- Begin function subexpr
	.type	subexpr,@function
subexpr:                                # @subexpr
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
	add r13, r5, r0
	add r12, r4, r0
	add r11, r3, r0
	ldw r3, r3+40
	jal r31, luaE_incCstack
	ldw r1, r11+12
	addi r3, r0, 125
	ble r1, r3, .LBB8_4
.LBB8_1:
	addi r3, r1, -262
	addi r4, r0, 30
	bgtu r3, r4, .LBB8_10
.LBB8_2:
	addi r14, r0, 2
	slli r1, r3, 2
	lui r3, %hi(.LJTI8_0)
	addi r3, r3, %lo(.LJTI8_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB8_3:
	addi r1, r0, -1
	stw r12+12, r1
	stw r12+16, r1
	addi r1, r0, 3
	jal r0, .LBB8_20
.LBB8_4:
	addi r3, r0, 35
	beq r1, r3, .LBB8_8
.LBB8_5:
	addi r3, r0, 45
	beq r1, r3, .LBB8_9
.LBB8_6:
	addi r3, r0, 123
	bne r1, r3, .LBB8_13
.LBB8_7:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, constructor
	jal r0, .LBB8_26
.LBB8_8:
	addi r14, r0, 3
	jal r0, .LBB8_12
.LBB8_9:
	addi r14, r0, 0
	jal r0, .LBB8_12
.LBB8_10:
	addi r3, r0, 126
	bne r1, r3, .LBB8_13
.LBB8_11:
	addi r14, r0, 1
.LBB8_12:
	ldw r15, r11+4
	add r3, r11, r0
	jal r31, luaX_next
	addi r5, r0, 12
	add r3, r11, r0
	add r4, r12, r0
	jal r31, subexpr
	ldw r3, r11+36
	add r4, r14, r0
	add r5, r12, r0
	add r6, r15, r0
	jal r31, luaK_prefix
	jal r0, .LBB8_26
.LBB8_13:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, suffixedexp
	jal r0, .LBB8_26
.LBB8_14:
	addi r1, r0, -1
	stw r12+12, r1
	stw r12+16, r1
	addi r1, r0, 2
	jal r0, .LBB8_20
.LBB8_15:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r6, r11+4
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, body
	jal r0, .LBB8_26
.LBB8_16:
	addi r1, r0, -1
	stw r12+12, r1
	stw r12+16, r1
	addi r1, r0, 6
	stw r12+0, r1
	addi r1, r0, 0
	stw r12+4, r1
	ldw r1, r11+16
	jal r0, .LBB8_24
.LBB8_17:
	ldw r3, r11+36
	ldw r1, r3+0
	ldbu r1, r1+7
	addi r4, r0, 0
	beq r1, r4, .LBB8_57
.LBB8_18:
	addi r4, r0, 80
	addi r7, r0, 1
	addi r5, r0, 0
	add r6, r5, r0
	add r8, r5, r0
	jal r31, luaK_codeABCk
	addi r3, r0, -1
	stw r12+12, r3
	stw r12+16, r3
	addi r3, r0, 19
	jal r0, .LBB8_23
.LBB8_19:
	addi r1, r0, -1
	stw r12+12, r1
	stw r12+16, r1
	addi r1, r0, 1
.LBB8_20:
	stw r12+0, r1
	addi r1, r0, 0
	jal r0, .LBB8_24
.LBB8_21:
	addi r1, r0, -1
	stw r12+12, r1
	stw r12+16, r1
	addi r1, r0, 5
	stw r12+0, r1
	addi r1, r0, 0
	stw r12+4, r1
	ldw r1, r11+20
	ldw r3, r11+16
	stw r12+8, r1
	stw r12+4, r3
	jal r0, .LBB8_25
.LBB8_22:
	ldw r1, r11+16
	addi r3, r0, -1
	stw r12+12, r3
	stw r12+16, r3
	addi r3, r0, 7
.LBB8_23:
	stw r12+0, r3
.LBB8_24:
	stw r12+4, r1
.LBB8_25:
	add r3, r11, r0
	jal r31, luaX_next
.LBB8_26:
	ldw r3, r11+12
	addi r1, r0, 0
	addi r4, r3, -37
	addi r5, r0, 89
	bgtu r4, r5, .LBB8_29
.LBB8_27:
	slli r3, r4, 2
	lui r4, %hi(.LJTI8_1)
	addi r4, r4, %lo(.LJTI8_1)
	add r3, r4, r3
	ldw r4, r3+0
	add r3, r1, r0
	add r14, r1, r0
	jalr r0, r4, 0
.LBB8_28:
	addi r14, r0, 3
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_29:
	addi r3, r3, -256
	addi r4, r0, 30
	bgtu r3, r4, .LBB8_41
.LBB8_30:
	slli r3, r3, 2
	lui r4, %hi(.LJTI8_2)
	addi r4, r4, %lo(.LJTI8_2)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB8_31:
	addi r14, r0, 19
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_32:
	addi r14, r0, 9
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_33:
	addi r14, r0, 17
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_34:
	addi r14, r0, 8
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_35:
	addi r14, r0, 14
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_36:
	addi r14, r0, 4
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_37:
	addi r14, r0, 5
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_38:
	addi r14, r0, 1
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_39:
	addi r14, r0, 7
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_40:
	addi r14, r0, 2
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_41:
	addi r14, r0, 21
	addi r3, r0, 1
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_42:
	addi r14, r0, 13
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_43:
	addi r14, r0, 12
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_44:
	addi r14, r0, 18
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_45:
	addi r14, r0, 20
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_46:
	addi r14, r0, 11
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_47:
	addi r14, r0, 6
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_48:
	addi r14, r0, 15
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_49:
	addi r14, r0, 16
	addi r3, r0, 0
.LBB8_50:
	bne r3, r1, .LBB8_55
.LBB8_51:
	andi r17, r13, 255
	lui r18, %hi(priority)
	addi r18, r18, %lo(priority)
	addi r13, fp, -60
	addi r19, r0, 21
.LBB8_52:
	slli r1, r14, 1
	add r16, r1, r18
	ldbu r1, r16+0
	bleu r1, r17, .LBB8_55
.LBB8_53:
	ldw r15, r11+4
	add r3, r11, r0
	jal r31, luaX_next
	ldw r3, r11+36
	add r4, r14, r0
	add r5, r12, r0
	jal r31, luaK_infix
	ldbu r5, r16+1
	add r3, r11, r0
	add r4, r13, r0
	jal r31, subexpr
	add r16, r1, r0
	ldw r3, r11+36
	add r4, r14, r0
	add r5, r12, r0
	add r6, r13, r0
	add r7, r15, r0
	jal r31, luaK_posfix
	add r14, r16, r0
	bne r16, r19, .LBB8_52
.LBB8_54:
	add r14, r19, r0
.LBB8_55:
	ldw r1, r11+40
	ldw r3, r1+96
	addi r3, r3, -1
	stw r1+96, r3
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
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB8_56:
	addi r14, r0, 10
	addi r3, r0, 0
	beq r3, r1, .LBB8_51
	jal r0, .LBB8_55
.LBB8_57:
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	add r3, r11, r0
	jal r31, luaX_syntaxerror
.Lfunc_end8:
	.size	subexpr, .Lfunc_end8-subexpr
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI8_0:
	.word	.LBB8_3
	.word	.LBB8_13
	.word	.LBB8_15
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_19
	.word	.LBB8_12
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_14
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_17
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_13
	.word	.LBB8_21
	.word	.LBB8_16
	.word	.LBB8_13
	.word	.LBB8_22
.LJTI8_1:
	.word	.LBB8_28
	.word	.LBB8_39
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_40
	.word	.LBB8_50
	.word	.LBB8_41
	.word	.LBB8_38
	.word	.LBB8_41
	.word	.LBB8_37
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_35
	.word	.LBB8_41
	.word	.LBB8_33
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_36
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_34
	.word	.LBB8_41
	.word	.LBB8_32
.LJTI8_2:
	.word	.LBB8_31
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_45
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_41
	.word	.LBB8_47
	.word	.LBB8_43
	.word	.LBB8_41
	.word	.LBB8_42
	.word	.LBB8_44
	.word	.LBB8_48
	.word	.LBB8_49
	.word	.LBB8_56
	.word	.LBB8_46
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function constructor
	.type	constructor,@function
constructor:                            # @constructor
# %bb.0:
	addi sp, sp, -104
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 104
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
	add r12, r4, r0
	add r16, r3, r0
	ldw r11, r3+36
	ldw r17, r3+4
	addi r4, r0, 19
	addi r15, r0, 0
	add r3, r11, r0
	add r5, r15, r0
	add r6, r15, r0
	add r7, r15, r0
	add r8, r15, r0
	jal r31, luaK_codeABCk
	add r13, r1, r0
	add r3, r11, r0
	add r4, r15, r0
	jal r31, luaK_code
	addi r14, fp, -96
	stw r14+32, r15
	stw r14+24, r15
	stw r14+28, r15
	stw r14+20, r12
	ldbu r1, r11+52
	addi r18, r0, -1
	stw r12+12, r18
	stw r12+16, r18
	addi r3, r0, 8
	stw r12+0, r3
	stw r12+4, r1
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaK_reserveregs
	stw r14+12, r18
	stw r14+16, r18
	stw r14+0, r15
	stw r14+4, r15
	ldw r1, r16+12
	addi r3, r0, 123
	bne r1, r3, .LBB9_21
.LBB9_1:
	add r3, r16, r0
	jal r31, luaX_next
	ldw r1, r16+12
	addi r19, r0, 125
	beq r1, r19, .LBB9_13
.LBB9_2:
	addi r20, r0, 91
	addi r21, r0, 44
	addi r22, r0, 59
	addi r23, r0, 291
	addi r24, r0, 61
	addi r18, r0, 50
	jal r0, .LBB9_4
.LBB9_3:
	add r3, r16, r0
	jal r31, luaX_next
	ldw r1, r16+12
	beq r1, r19, .LBB9_13
.LBB9_4:
	ldw r1, r14+0
	beq r1, r15, .LBB9_7
.LBB9_5:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, luaK_exp2nextreg
	stw r14+0, r15
	ldw r1, r14+32
	bne r1, r18, .LBB9_7
.LBB9_6:
	ldw r1, r14+20
	ldw r4, r1+4
	ldw r5, r14+28
	add r3, r11, r0
	add r6, r18, r0
	jal r31, luaK_setlist
	ldw r1, r14+32
	ldw r3, r14+28
	add r1, r3, r1
	stw r14+28, r1
	stw r14+32, r15
.LBB9_7:
	ldw r1, r16+12
	beq r1, r20, .LBB9_10
.LBB9_8:
	bne r1, r23, .LBB9_11
.LBB9_9:
	add r3, r16, r0
	jal r31, luaX_lookahead
	bne r1, r24, .LBB9_11
.LBB9_10:
	add r3, r16, r0
	add r4, r14, r0
	jal r31, recfield
	ldw r1, r16+12
	beq r1, r21, .LBB9_3
	jal r0, .LBB9_12
.LBB9_11:
	add r3, r16, r0
	add r4, r14, r0
	add r5, r15, r0
	jal r31, subexpr
	ldw r1, r14+32
	addi r1, r1, 1
	stw r14+32, r1
	ldw r1, r16+12
	beq r1, r21, .LBB9_3
.LBB9_12:
	ldw r1, r16+12
	beq r1, r22, .LBB9_3
.LBB9_13:
	addi r4, r0, 125
	addi r5, r0, 123
	add r3, r16, r0
	add r6, r17, r0
	jal r31, check_match
	ldw r1, r14+32
	beq r1, r15, .LBB9_20
.LBB9_14:
	ldw r1, r14+0
	addi r3, r1, -18
	addi r4, r0, 2
	bgeu r3, r4, .LBB9_16
.LBB9_15:
	addi r15, r0, -1
	add r3, r11, r0
	add r4, r14, r0
	add r5, r15, r0
	jal r31, luaK_setreturns
	ldw r1, r14+20
	ldw r4, r1+4
	ldw r5, r14+28
	add r3, r11, r0
	add r6, r15, r0
	jal r31, luaK_setlist
	ldw r1, r14+28
	addi r1, r1, -1
	stw r14+28, r1
	jal r0, .LBB9_19
.LBB9_16:
	beq r1, r15, .LBB9_18
.LBB9_17:
	addi r4, fp, -96
	add r3, r11, r0
	jal r31, luaK_exp2nextreg
.LBB9_18:
	ldw r1, r14+20
	ldw r4, r1+4
	ldw r5, r14+28
	ldw r6, r14+32
	add r3, r11, r0
	jal r31, luaK_setlist
.LBB9_19:
	ldw r1, r14+32
	ldw r3, r14+28
	add r1, r3, r1
	stw r14+28, r1
.LBB9_20:
	ldw r5, r12+4
	ldw r6, r14+28
	ldw r7, r14+24
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_settablesize
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
	addi sp, sp, 104
	jalr r0, r31, 0
.LBB9_21:
	addi r4, r0, 123
	add r3, r16, r0
	jal r31, error_expected
.Lfunc_end9:
	.size	constructor, .Lfunc_end9-constructor
                                        # -- End function
	.p2align	2                               # -- Begin function body
	.type	body,@function
body:                                   # @body
# %bb.0:
	addi sp, sp, -168
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 168
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
	add r24, r6, r0
	add r14, r5, r0
	add r23, r4, r0
	add r11, r3, r0
	ldw r15, r3+40
	ldw r18, r3+36
	ldw r16, r18+0
	ldw r5, r18+32
	ldw r13, r16+28
	blt r5, r13, .LBB10_4
.LBB10_1:
	addi r6, r16, 28
	ldw r4, r16+56
	lui r9, %hi(.L.str.8)
	addi r9, r9, %lo(.L.str.8)
	lui r1, 32
	addi r8, r1, -1
	addi r7, r0, 4
	add r3, r15, r0
	jal r31, luaM_growaux_
	stw r16+56, r1
	ldw r3, r16+28
	ble r3, r13, .LBB10_4
.LBB10_2:
	slli r4, r13, 2
	add r1, r1, r4
	sub r3, r3, r13
	addi r4, r0, 0
.LBB10_3:
	stw r1+0, r4
	addi r1, r1, 4
	addi r3, r3, -1
	bne r3, r4, .LBB10_3
.LBB10_4:
	add r3, r15, r0
	jal r31, luaF_newproto
	add r17, r1, r0
	ldw r1, r16+56
	ldw r3, r18+32
	addi r4, r3, 1
	stw r18+32, r4
	slli r3, r3, 2
	add r1, r1, r3
	stw r1+0, r17
	ldbu r1, r16+5
	andi r1, r1, 32
	addi r18, r0, 0
	beq r1, r18, .LBB10_7
.LBB10_5:
	ldbu r1, r17+5
	andi r1, r1, 24
	beq r1, r18, .LBB10_7
.LBB10_6:
	add r3, r15, r0
	add r4, r16, r0
	add r5, r17, r0
	jal r31, luaC_barrier_
.LBB10_7:
	addi r19, fp, -132
	stw r19+0, r17
	stw r17+40, r24
	ldw r1, r11+36
	stw r19+4, r1
	stw r19+8, r11
	stw r11+36, r19
	stw r19+16, r18
	stw r19+24, r24
	stw r19+20, r18
	sth r19+52, r18
	stw r19+28, r18
	stw r19+36, r18
	stw r19+32, r18
	stw r19+48, r18
	stb r19+54, r18
	ldw r1, r11+56
	ldw r3, r1+4
	stw r19+40, r3
	ldw r1, r1+28
	stw r19+44, r1
	stw r19+12, r18
	ldw r5, r11+60
	stw r17+76, r5
	ldbu r1, r17+5
	andi r1, r1, 32
	beq r1, r18, .LBB10_10
.LBB10_8:
	ldbu r1, r5+5
	andi r1, r1, 24
	beq r1, r18, .LBB10_10
.LBB10_9:
	ldw r3, r11+40
	add r4, r17, r0
	jal r31, luaC_barrier_
.LBB10_10:
	addi r1, r0, 2
	stb r17+8, r1
	addi r1, fp, -148
	stb r1+14, r18
	ldbu r3, r19+50
	stb r1+12, r3
	ldw r3, r19+8
	ldw r3, r3+56
	ldw r4, r3+28
	stw r1+4, r4
	ldw r3, r3+16
	stw r1+8, r3
	stb r1+13, r18
	ldw r3, r19+12
	beq r3, r18, .LBB10_12
.LBB10_11:
	ldbu r4, r3+15
	addi r5, r0, 0
	sne r18, r4, r5
.LBB10_12:
	stb r1+15, r18
	stw r1+0, r3
	stw r19+12, r1
	ldw r1, r11+12
	addi r3, r0, 40
	bne r1, r3, .LBB10_38
.LBB10_13:
	add r3, r11, r0
	jal r31, luaX_next
	addi r20, r0, 0
	lui r17, 16
	beq r14, r20, .LBB10_16
.LBB10_14:
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	addi r5, r0, 4
	add r3, r11, r0
	jal r31, luaX_newstring
	add r14, r1, r0
	ldw r3, r11+36
	ldw r13, r11+56
	ldw r1, r13+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	addi r4, r0, 201
	bge r1, r4, .LBB10_36
.LBB10_15:
	ldw r3, r11+40
	ldw r4, r13+0
	addi r6, r13, 8
	lui r9, %hi(.L.str.9)
	addi r9, r9, %lo(.L.str.9)
	addi r8, r17, -1
	addi r15, r0, 20
	add r7, r15, r0
	jal r31, luaM_growaux_
	stw r13+0, r1
	ldw r3, r13+4
	addi r4, r3, 1
	stw r13+4, r4
	mul r3, r3, r15
	add r1, r1, r3
	stb r1+9, r20
	stw r1+16, r14
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, adjustlocalvars
.LBB10_16:
	ldw r14, r11+36
	ldw r22, r14+0
	ldw r1, r11+12
	addi r21, r0, 41
	bne r1, r21, .LBB10_18
.LBB10_17:
	addi r13, r0, 1
	add r15, r20, r0
	jal r0, .LBB10_26
.LBB10_18:
	stw fp+-156, r24
	stw fp+-152, r23
	addi r23, r0, 0
	addi r24, r0, 280
	addi r25, r0, 44
	addi r26, r0, 291
	addi r27, r0, 201
	lui r16, %hi(.L.str.9)
	addi r16, r16, %lo(.L.str.9)
	addi r17, r17, -1
	addi r18, r0, 20
	addi r28, r0, 1
	add r15, r23, r0
	ldw r1, r11+12
	beq r1, r24, .LBB10_22
.LBB10_19:
	bne r1, r26, .LBB10_37
.LBB10_20:
	ldw r13, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	ldw r3, r11+36
	ldw r12, r11+56
	ldw r1, r12+4
	addi r5, r1, 1
	ldw r1, r3+40
	sub r1, r5, r1
	bge r1, r27, .LBB10_36
.LBB10_21:
	ldw r3, r11+40
	ldw r4, r12+0
	addi r6, r12, 8
	add r7, r18, r0
	add r8, r17, r0
	add r9, r16, r0
	jal r31, luaM_growaux_
	stw r12+0, r1
	ldw r3, r12+4
	addi r4, r3, 1
	stw r12+4, r4
	mul r3, r3, r18
	add r1, r1, r3
	stb r1+9, r23
	stw r1+16, r13
	addi r15, r15, 1
	add r13, r28, r0
	bne r13, r23, .LBB10_23
	jal r0, .LBB10_25
.LBB10_22:
	add r3, r11, r0
	jal r31, luaX_next
	add r13, r23, r0
	beq r13, r23, .LBB10_25
.LBB10_23:
	ldw r1, r11+12
	bne r1, r25, .LBB10_25
.LBB10_24:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	bne r1, r24, .LBB10_19
	jal r0, .LBB10_22
.LBB10_25:
	ldw r23, fp+-152
	ldw r24, fp+-156
.LBB10_26:
	add r3, r11, r0
	add r4, r15, r0
	jal r31, adjustlocalvars
	ldbu r5, r14+50
	stb r22+6, r5
	bne r13, r20, .LBB10_28
.LBB10_27:
	ldw r1, r14+0
	addi r3, r0, 1
	stb r1+7, r3
	addi r4, r0, 81
	addi r6, r0, 0
	add r3, r14, r0
	add r7, r6, r0
	add r8, r6, r0
	jal r31, luaK_codeABCk
.LBB10_28:
	ldbu r4, r14+50
	add r3, r14, r0
	jal r31, luaK_reserveregs
	ldw r1, r11+12
	bne r1, r21, .LBB10_39
.LBB10_29:
	add r3, r11, r0
	jal r31, luaX_next
	addi r12, r0, 29
	addi r13, r0, 1
	lui r1, 131104
	addi r14, r1, 7
	addi r15, r0, 0
	addi r16, r0, 14
	jal r0, .LBB10_31
.LBB10_30:
	add r3, r11, r0
	jal r31, statement
.LBB10_31:
	ldw r1, r11+12
	addi r1, r1, -259
	bgtu r1, r12, .LBB10_30
.LBB10_32:
	sll r3, r13, r1
	and r3, r3, r14
	bne r3, r15, .LBB10_35
.LBB10_33:
	bne r1, r16, .LBB10_30
.LBB10_34:
	add r3, r11, r0
	jal r31, statement
.LBB10_35:
	ldw r1, r11+4
	ldw r3, r19+0
	stw r3+44, r1
	addi r4, r0, 261
	addi r5, r0, 264
	add r3, r11, r0
	add r6, r24, r0
	jal r31, check_match
	ldw r1, r11+36
	ldw r13, r1+4
	ldw r1, r13+32
	addi r6, r1, -1
	addi r4, r0, 79
	addi r5, r0, 0
	add r3, r13, r0
	jal r31, luaK_codeABx
	addi r3, r0, -1
	stw r23+12, r3
	stw r23+16, r3
	addi r3, r0, 17
	stw r23+0, r3
	stw r23+4, r1
	add r3, r13, r0
	add r4, r23, r0
	jal r31, luaK_exp2nextreg
	add r3, r11, r0
	jal r31, close_func
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
	addi sp, sp, 168
	jalr r0, r31, 0
.LBB10_36:
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
	addi r4, r0, 200
	jal r31, errorlimit
.LBB10_37:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	add r3, r11, r0
	jal r31, luaX_syntaxerror
.LBB10_38:
	addi r4, r0, 40
	add r3, r11, r0
	jal r31, error_expected
.LBB10_39:
	addi r4, r0, 41
	add r3, r11, r0
	jal r31, error_expected
.Lfunc_end10:
	.size	body, .Lfunc_end10-body
                                        # -- End function
	.p2align	2                               # -- Begin function suffixedexp
	.type	suffixedexp,@function
suffixedexp:                            # @suffixedexp
# %bb.0:
	addi sp, sp, -104
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 104
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
	add r12, r4, r0
	add r11, r3, r0
	ldw r13, r3+36
	ldw r1, r3+12
	addi r17, r0, 291
	beq r1, r17, .LBB11_3
.LBB11_1:
	addi r3, r0, 40
	bne r1, r3, .LBB11_23
.LBB11_2:
	ldw r14, r11+4
	add r3, r11, r0
	jal r31, luaX_next
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, subexpr
	addi r4, r0, 41
	addi r5, r0, 40
	add r3, r11, r0
	add r6, r14, r0
	jal r31, check_match
	ldw r3, r11+36
	add r4, r12, r0
	jal r31, luaK_dischargevars
	jal r0, .LBB11_4
.LBB11_3:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, singlevar
.LBB11_4:
	addi r18, r0, 90
	addi r19, r0, 40
	addi r20, r0, 46
	addi r21, r0, -1
	addi r14, fp, -96
	addi r22, r0, 7
	addi r23, r0, 58
	addi r24, r0, 91
	addi r15, r0, 0
	addi r25, r0, 93
	addi r26, r0, 123
	addi r27, r0, 292
	ldw r1, r11+12
	bgt r1, r18, .LBB11_14
	jal r0, .LBB11_7
.LBB11_5:
	add r3, r13, r0
	add r4, r12, r0
	jal r31, luaK_exp2nextreg
.LBB11_6:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, funcargs
	ldw r1, r11+12
	bgt r1, r18, .LBB11_14
.LBB11_7:
	beq r1, r19, .LBB11_5
.LBB11_8:
	beq r1, r20, .LBB11_12
.LBB11_9:
	bne r1, r23, .LBB11_20
.LBB11_10:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	bne r1, r17, .LBB11_21
.LBB11_11:
	ldw r16, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	stw r14+12, r21
	stw r14+16, r21
	stw r14+0, r22
	stw r14+4, r16
	add r3, r13, r0
	add r4, r12, r0
	add r5, r14, r0
	jal r31, luaK_self
	jal r0, .LBB11_6
.LBB11_12:
	ldw r16, r11+36
	add r3, r16, r0
	add r4, r12, r0
	jal r31, luaK_exp2anyregup
	add r3, r11, r0
	jal r31, luaX_next
	ldw r1, r11+12
	bne r1, r17, .LBB11_21
.LBB11_13:
	ldw r28, r11+16
	add r3, r11, r0
	jal r31, luaX_next
	stw r14+12, r21
	stw r14+16, r21
	stw r14+0, r22
	stw r14+4, r28
	add r3, r16, r0
	jal r0, .LBB11_19
.LBB11_14:
	beq r1, r24, .LBB11_17
.LBB11_15:
	beq r1, r26, .LBB11_5
.LBB11_16:
	beq r1, r27, .LBB11_5
	jal r0, .LBB11_20
.LBB11_17:
	add r3, r13, r0
	add r4, r12, r0
	jal r31, luaK_exp2anyregup
	add r3, r11, r0
	jal r31, luaX_next
	add r3, r11, r0
	add r4, r14, r0
	add r5, r15, r0
	jal r31, subexpr
	ldw r3, r11+36
	add r4, r14, r0
	jal r31, luaK_exp2val
	ldw r1, r11+12
	bne r1, r25, .LBB11_22
.LBB11_18:
	add r3, r11, r0
	jal r31, luaX_next
	add r3, r13, r0
.LBB11_19:
	add r4, r12, r0
	add r5, r14, r0
	jal r31, luaK_indexed
	ldw r1, r11+12
	bgt r1, r18, .LBB11_14
	jal r0, .LBB11_7
.LBB11_20:
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
	addi sp, sp, 104
	jalr r0, r31, 0
.LBB11_21:
	addi r4, r0, 291
	add r3, r11, r0
	jal r31, error_expected
.LBB11_22:
	addi r4, r0, 93
	add r3, r11, r0
	jal r31, error_expected
.LBB11_23:
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	add r3, r11, r0
	jal r31, luaX_syntaxerror
.Lfunc_end11:
	.size	suffixedexp, .Lfunc_end11-suffixedexp
                                        # -- End function
	.p2align	2                               # -- Begin function recfield
	.type	recfield,@function
recfield:                               # @recfield
# %bb.0:
	addi sp, sp, -104
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 104
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, lr
	add r13, r4, r0
	add r12, r3, r0
	ldw r11, r3+36
	ldbu r15, r11+52
	ldw r1, r3+12
	addi r3, r0, 291
	bne r1, r3, .LBB12_2
.LBB12_1:
	ldw r14, r12+16
	add r3, r12, r0
	jal r31, luaX_next
	addi r1, r0, -1
	addi r3, fp, -64
	stw r3+12, r1
	stw r3+16, r1
	addi r1, r0, 7
	stw r3+0, r1
	stw r3+4, r14
	jal r0, .LBB12_4
.LBB12_2:
	add r3, r12, r0
	jal r31, luaX_next
	addi r14, fp, -64
	addi r5, r0, 0
	add r3, r12, r0
	add r4, r14, r0
	jal r31, subexpr
	ldw r3, r12+36
	add r4, r14, r0
	jal r31, luaK_exp2val
	ldw r1, r12+12
	addi r3, r0, 93
	bne r1, r3, .LBB12_7
.LBB12_3:
	add r3, r12, r0
	jal r31, luaX_next
.LBB12_4:
	ldw r1, r13+24
	addi r1, r1, 1
	stw r13+24, r1
	ldw r1, r12+12
	addi r3, r0, 61
	bne r1, r3, .LBB12_6
.LBB12_5:
	add r3, r12, r0
	jal r31, luaX_next
	ldw r4, r13+20
	addi r13, fp, -44
	addi r5, r0, 20
	add r3, r13, r0
	jal r31, memcpy
	addi r5, fp, -64
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaK_indexed
	addi r14, fp, -84
	addi r5, r0, 0
	add r3, r12, r0
	add r4, r14, r0
	jal r31, subexpr
	add r3, r11, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, luaK_storevar
	stb r11+52, r15
	ldw lr, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 104
	jalr r0, r31, 0
.LBB12_6:
	addi r4, r0, 61
	add r3, r12, r0
	jal r31, error_expected
.LBB12_7:
	addi r4, r0, 93
	add r3, r12, r0
	jal r31, error_expected
.Lfunc_end12:
	.size	recfield, .Lfunc_end12-recfield
                                        # -- End function
	.p2align	2                               # -- Begin function adjustlocalvars
	.type	adjustlocalvars,@function
adjustlocalvars:                        # @adjustlocalvars
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
	add r11, r4, r0
	add r12, r3, r0
	ldw r18, r3+36
	ldbu r1, r18+50
	addi r3, r1, 1
	addi r19, r0, 20
	mul r1, r1, r19
	addi r5, r1, -11
	addi r4, r0, 0
	addi r1, r0, 1
	addi r6, r0, 3
                                        # implicit-def: $r20
	jal r0, .LBB13_2
.LBB13_1:
	addi r5, r5, -20
	bne r7, r6, .LBB13_6
.LBB13_2:
	addi r3, r3, -1
	blt r3, r1, .LBB13_5
.LBB13_3:
	ldw r7, r18+8
	ldw r7, r7+56
	ldw r7, r7+0
	ldw r8, r18+40
	mul r8, r8, r19
	add r7, r7, r8
	add r8, r7, r5
	ldbu r7, r8+0
	beq r7, r6, .LBB13_1
.LBB13_4:
	ldbu r8, r8+1
	addi r20, r8, 1
	jal r0, .LBB13_1
.LBB13_5:
	add r20, r4, r0
.LBB13_6:
	blt r11, r1, .LBB13_15
.LBB13_7:
	addi r21, r0, 0
	lui r13, %hi(.L.str.9)
	addi r13, r13, %lo(.L.str.9)
	lui r1, 8
	addi r14, r1, -1
	addi r15, r0, 12
	add r22, r21, r0
	jal r0, .LBB13_9
.LBB13_8:
	addi r20, r20, 1
	ldhu r1, r18+48
	addi r3, r1, 1
	sth r18+48, r3
	sth r23+12, r1
	addi r22, r22, 1
	beq r22, r11, .LBB13_15
.LBB13_9:
	ldbu r1, r18+50
	addi r3, r1, 1
	stb r18+50, r3
	ldw r3, r18+8
	ldw r3, r3+56
	ldw r3, r3+0
	ldw r4, r18+40
	mul r4, r4, r19
	add r3, r3, r4
	mul r1, r1, r19
	add r23, r3, r1
	stb r23+10, r20
	ldw r16, r23+16
	ldw r17, r18+0
	addi r6, r17, 32
	ldw r24, r17+32
	ldw r3, r12+40
	ldw r4, r17+72
	ldh r5, r18+48
	add r7, r15, r0
	add r8, r14, r0
	add r9, r13, r0
	jal r31, luaM_growaux_
	stw r17+72, r1
	ldw r4, r17+32
	ble r4, r24, .LBB13_12
.LBB13_10:
	mul r3, r24, r15
	add r3, r1, r3
	sub r4, r4, r24
.LBB13_11:
	stw r3+0, r21
	addi r3, r3, 12
	addi r4, r4, -1
	bne r4, r21, .LBB13_11
.LBB13_12:
	ldh r3, r18+48
	mul r3, r3, r15
	add r1, r1, r3
	stw r1+0, r16
	ldw r3, r18+16
	stw r1+4, r3
	ldbu r1, r17+5
	andi r1, r1, 32
	beq r1, r21, .LBB13_8
.LBB13_13:
	ldbu r1, r16+5
	andi r1, r1, 24
	beq r1, r21, .LBB13_8
.LBB13_14:
	ldw r3, r12+40
	add r4, r17, r0
	add r5, r16, r0
	jal r31, luaC_barrier_
	jal r0, .LBB13_8
.LBB13_15:
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
.Lfunc_end13:
	.size	adjustlocalvars, .Lfunc_end13-adjustlocalvars
                                        # -- End function
	.p2align	2                               # -- Begin function funcargs
	.type	funcargs,@function
funcargs:                               # @funcargs
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
	stw fp+-36, lr
	add r13, r4, r0
	add r14, r3, r0
	ldw r11, r3+36
	ldw r12, r3+4
	ldw r1, r3+12
	addi r3, r0, 292
	beq r1, r3, .LBB14_6
.LBB14_1:
	addi r3, r0, 123
	beq r1, r3, .LBB14_5
.LBB14_2:
	addi r3, r0, 40
	bne r1, r3, .LBB14_19
.LBB14_3:
	add r3, r14, r0
	jal r31, luaX_next
	ldw r1, r14+12
	addi r3, r0, 41
	bne r1, r3, .LBB14_7
.LBB14_4:
	addi r1, r0, 0
	addi r3, fp, -56
	stw r3+0, r1
	jal r0, .LBB14_12
.LBB14_5:
	addi r4, fp, -56
	add r3, r14, r0
	jal r31, constructor
	jal r0, .LBB14_13
.LBB14_6:
	ldw r1, r14+16
	addi r3, r0, -1
	addi r4, fp, -56
	stw r4+12, r3
	stw r4+16, r3
	addi r3, r0, 7
	stw r4+0, r3
	stw r4+4, r1
	add r3, r14, r0
	jal r31, luaX_next
	jal r0, .LBB14_13
.LBB14_7:
	addi r15, fp, -56
	addi r5, r0, 0
	add r3, r14, r0
	add r4, r15, r0
	jal r31, subexpr
	ldw r1, r14+12
	addi r18, r0, 44
	bne r1, r18, .LBB14_10
.LBB14_8:
	addi r16, fp, -56
	addi r17, r0, 0
.LBB14_9:
	add r3, r14, r0
	jal r31, luaX_next
	ldw r3, r14+36
	add r4, r16, r0
	jal r31, luaK_exp2nextreg
	add r3, r14, r0
	add r4, r16, r0
	add r5, r17, r0
	jal r31, subexpr
	ldw r1, r14+12
	beq r1, r18, .LBB14_9
.LBB14_10:
	ldw r1, r15+0
	addi r3, r0, -2
	and r1, r1, r3
	addi r3, r0, 18
	bne r1, r3, .LBB14_12
.LBB14_11:
	addi r4, fp, -56
	addi r5, r0, -1
	add r3, r11, r0
	jal r31, luaK_setreturns
.LBB14_12:
	addi r4, r0, 41
	addi r5, r0, 40
	add r3, r14, r0
	add r6, r12, r0
	jal r31, check_match
.LBB14_13:
	ldw r14, r13+4
	addi r1, fp, -56
	ldw r1, r1+0
	addi r3, r0, -2
	and r3, r1, r3
	addi r15, r0, 18
	bne r3, r15, .LBB14_15
.LBB14_14:
	addi r6, r0, 0
	jal r0, .LBB14_18
.LBB14_15:
	addi r3, r0, 0
	beq r1, r3, .LBB14_17
.LBB14_16:
	addi r4, fp, -56
	add r3, r11, r0
	jal r31, luaK_exp2nextreg
.LBB14_17:
	ldbu r1, r11+52
	sub r6, r1, r14
.LBB14_18:
	addi r4, r0, 68
	addi r7, r0, 2
	addi r8, r0, 0
	add r3, r11, r0
	add r5, r14, r0
	jal r31, luaK_codeABCk
	addi r3, r0, -1
	stw r13+12, r3
	stw r13+16, r3
	stw r13+0, r15
	stw r13+4, r1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaK_fixline
	addi r1, r14, 1
	stb r11+52, r1
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
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB14_19:
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
	add r3, r14, r0
	jal r31, luaX_syntaxerror
.Lfunc_end14:
	.size	funcargs, .Lfunc_end14-funcargs
                                        # -- End function
	.p2align	2                               # -- Begin function singlevar
	.type	singlevar,@function
singlevar:                              # @singlevar
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
	stw fp+-20, lr
	add r14, r3, r0
	ldw r1, r3+12
	addi r3, r0, 291
	bne r1, r3, .LBB15_4
.LBB15_1:
	add r11, r4, r0
	ldw r12, r14+16
	add r3, r14, r0
	jal r31, luaX_next
	ldw r13, r14+36
	addi r6, r0, 1
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, singlevaraux
	ldw r1, r11+0
	addi r3, r0, 0
	bne r1, r3, .LBB15_3
.LBB15_2:
	ldw r4, r14+64
	addi r6, r0, 1
	add r3, r13, r0
	add r5, r11, r0
	jal r31, singlevaraux
	add r3, r13, r0
	add r4, r11, r0
	jal r31, luaK_exp2anyregup
	addi r1, r0, -1
	addi r5, fp, -40
	stw r5+12, r1
	stw r5+16, r1
	addi r1, r0, 7
	stw r5+0, r1
	stw r5+4, r12
	add r3, r13, r0
	add r4, r11, r0
	jal r31, luaK_indexed
.LBB15_3:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.LBB15_4:
	addi r4, r0, 291
	add r3, r14, r0
	jal r31, error_expected
.Lfunc_end15:
	.size	singlevar, .Lfunc_end15-singlevar
                                        # -- End function
	.p2align	2                               # -- Begin function singlevaraux
	.type	singlevaraux,@function
singlevaraux:                           # @singlevaraux
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
	addi r1, r0, 0
	beq r3, r1, .LBB16_9
.LBB16_1:
	addi r7, r0, -1
	ldbu r8, r3+50
	addi r14, r0, 20
	mul r9, r8, r14
	addi r9, r9, -11
	addi r10, r8, -1
	addi r13, r0, 1
	addi r11, r0, 3
	addi r12, r0, 11
	addi r15, r0, 9
                                        # implicit-def: $r16
	jal r0, .LBB16_5
.LBB16_2:
	stw r5+12, r7
	stw r5+16, r7
	stw r5+0, r15
	sth r5+6, r10
	ldw r16, r3+8
	ldw r16, r16+56
	ldw r16, r16+0
	ldw r18, r3+40
	mul r18, r18, r14
	add r16, r16, r18
	add r16, r16, r9
	ldbu r16, r16+1
	stb r5+4, r16
.LBB16_3:
	ldw r16, r5+0
.LBB16_4:
	addi r9, r9, -20
	addi r10, r10, -1
	addi r8, r8, -1
	beq r4, r17, .LBB16_11
.LBB16_5:
	blt r8, r13, .LBB16_10
.LBB16_6:
	ldw r17, r3+8
	ldw r17, r17+56
	ldw r17, r17+0
	ldw r18, r3+40
	mul r19, r18, r14
	add r17, r17, r19
	add r19, r17, r9
	ldw r17, r19+7
	bne r4, r17, .LBB16_4
.LBB16_7:
	ldbu r16, r19+0
	bne r16, r11, .LBB16_2
.LBB16_8:
	add r16, r8, r18
	addi r16, r16, -1
	stw r5+12, r7
	stw r5+16, r7
	stw r5+0, r12
	stw r5+4, r16
	jal r0, .LBB16_3
.LBB16_9:
	addi r1, r0, -1
	stw r5+12, r1
	stw r5+16, r1
	addi r1, r0, 0
	stw r5+0, r1
	stw r5+4, r1
	jal r0, .LBB16_37
.LBB16_10:
	add r16, r7, r0
.LBB16_11:
	blt r16, r1, .LBB16_17
.LBB16_12:
	addi r1, r0, 0
	bne r6, r1, .LBB16_37
.LBB16_13:
	bne r16, r15, .LBB16_37
.LBB16_14:
	ldhu r4, r5+6
	addi r1, r3, 12
	lui r5, 16
	addi r5, r5, -1
	and r4, r4, r5
.LBB16_15:
	ldw r1, r1+0
	ldbu r5, r1+12
	bltu r4, r5, .LBB16_15
.LBB16_16:
	stb r1+13, r13
	stb r3+54, r13
	jal r0, .LBB16_37
.LBB16_17:
	ldbu r7, r3+51
	addi r6, r0, -1
	add r8, r6, r0
	beq r7, r1, .LBB16_22
.LBB16_18:
	ldw r8, r3+0
	ldw r8, r8+60
.LBB16_19:
	ldw r9, r8+0
	beq r9, r4, .LBB16_23
.LBB16_20:
	addi r1, r1, 1
	addi r8, r8, 8
	bne r7, r1, .LBB16_19
.LBB16_21:
	add r8, r6, r0
.LBB16_22:
	bgt r8, r6, .LBB16_36
	jal r0, .LBB16_24
.LBB16_23:
	add r8, r1, r0
	bgt r8, r6, .LBB16_36
.LBB16_24:
	ldw r1, r3+4
	addi r12, r0, 0
	add r16, r3, r0
	add r3, r1, r0
	add r17, r4, r0
	add r11, r5, r0
	add r6, r12, r0
	jal r31, singlevaraux
	ldw r1, r11+0
	addi r1, r1, -9
	bgtu r1, r13, .LBB16_37
.LBB16_25:
	add r3, r16, r0
	ldbu r5, r16+51
	addi r1, r0, 255
	beq r5, r1, .LBB16_38
.LBB16_26:
	ldw r19, r3+0
	addi r6, r19, 12
	ldw r18, r19+12
	ldw r1, r3+8
	ldw r3, r1+40
	ldw r4, r19+60
	lui r9, %hi(.L.str)
	addi r9, r9, %lo(.L.str)
	addi r7, r0, 8
	addi r8, r0, 255
	jal r31, luaM_growaux_
	stw r19+60, r1
	ldw r4, r19+12
	ble r4, r18, .LBB16_29
.LBB16_27:
	slli r3, r18, 3
	add r3, r1, r3
	sub r4, r4, r18
.LBB16_28:
	stw r3+0, r12
	addi r3, r3, 8
	addi r4, r4, -1
	bne r4, r12, .LBB16_28
.LBB16_29:
	add r3, r16, r0
	ldbu r4, r16+51
	addi r5, r4, 1
	stb r16+51, r5
	slli r4, r4, 3
	add r1, r1, r4
	ldw r4, r16+4
	add r5, r11, r0
	ldw r6, r11+0
	bne r6, r15, .LBB16_31
.LBB16_30:
	stb r1+4, r13
	ldbu r6, r5+4
	stb r1+5, r6
	ldhu r6, r5+6
	ldw r7, r4+8
	ldw r7, r7+56
	ldw r7, r7+0
	ldw r4, r4+40
	mul r4, r4, r14
	add r4, r7, r4
	mul r6, r6, r14
	add r4, r4, r6
	addi r4, r4, 9
	jal r0, .LBB16_32
.LBB16_31:
	stb r1+4, r12
	ldw r6, r5+4
	stb r1+5, r6
	ldw r4, r4+0
	ldw r4, r4+60
	ldw r6, r5+4
	slli r6, r6, 3
	add r4, r4, r6
	addi r4, r4, 6
.LBB16_32:
	ldbu r4, r4+0
	stb r1+6, r4
	stw r1+0, r17
	ldw r4, r3+0
	ldbu r1, r4+5
	andi r1, r1, 32
	beq r1, r12, .LBB16_35
.LBB16_33:
	ldbu r1, r17+5
	andi r1, r1, 24
	beq r1, r12, .LBB16_35
.LBB16_34:
	ldw r1, r3+8
	ldw r3, r1+40
	add r5, r17, r0
	jal r31, luaC_barrier_
	add r5, r11, r0
	add r3, r16, r0
.LBB16_35:
	ldbu r1, r3+51
	addi r8, r1, -1
.LBB16_36:
	addi r1, r0, -1
	stw r5+12, r1
	stw r5+16, r1
	addi r1, r0, 10
	stw r5+0, r1
	stw r5+4, r8
.LBB16_37:
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
.LBB16_38:
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	addi r4, r0, 255
	jal r31, errorlimit
.Lfunc_end16:
	.size	singlevaraux, .Lfunc_end16-singlevaraux
                                        # -- End function
	.p2align	2                               # -- Begin function explist
	.type	explist,@function
explist:                                # @explist
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
	add r11, r4, r0
	add r12, r3, r0
	addi r5, r0, 0
	jal r31, subexpr
	ldw r1, r12+12
	addi r13, r0, 1
	addi r15, r0, 44
	bne r1, r15, .LBB17_3
.LBB17_1:
	addi r14, r0, 0
.LBB17_2:
	add r3, r12, r0
	jal r31, luaX_next
	ldw r3, r12+36
	add r4, r11, r0
	jal r31, luaK_exp2nextreg
	add r3, r12, r0
	add r4, r11, r0
	add r5, r14, r0
	jal r31, subexpr
	addi r13, r13, 1
	ldw r1, r12+12
	beq r1, r15, .LBB17_2
.LBB17_3:
	add r1, r13, r0
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
.Lfunc_end17:
	.size	explist, .Lfunc_end17-explist
                                        # -- End function
	.p2align	2                               # -- Begin function createlabel
	.type	createlabel,@function
createlabel:                            # @createlabel
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
	add r13, r6, r0
	add r14, r5, r0
	add r15, r4, r0
	add r12, r3, r0
	ldw r11, r3+36
	ldw r19, r3+56
	add r3, r11, r0
	jal r31, luaK_getlabel
	add r16, r1, r0
	ldw r17, r19+28
	ldw r3, r12+40
	ldw r4, r19+24
	addi r6, r19, 32
	lui r9, %hi(.L.str.13)
	addi r9, r9, %lo(.L.str.13)
	lui r1, 8
	addi r8, r1, -1
	addi r7, r0, 16
	add r5, r17, r0
	jal r31, luaM_growaux_
	stw r19+24, r1
	slli r3, r17, 4
	add r18, r1, r3
	stw r18+0, r15
	stw r18+8, r14
	ldw r3, r12+36
	ldbu r1, r3+50
	stb r18+12, r1
	addi r1, r0, 0
	stb r18+13, r1
	stw r18+4, r16
	addi r4, r17, 1
	stw r19+28, r4
	beq r13, r1, .LBB18_2
.LBB18_1:
	ldw r4, r11+12
	ldbu r4, r4+12
	stb r18+12, r4
.LBB18_2:
	ldw r13, r12+56
	ldw r3, r3+12
	ldw r14, r3+8
	ldw r3, r13+16
	bge r14, r3, .LBB18_19
.LBB18_3:
	addi r15, r0, 0
	jal r0, .LBB18_5
.LBB18_4:
	addi r14, r14, 1
	ldw r1, r13+16
	bge r14, r1, .LBB18_11
.LBB18_5:
	ldw r1, r13+12
	slli r16, r14, 4
	add r1, r1, r16
	ldw r3, r1+0
	ldw r4, r18+0
	bne r3, r4, .LBB18_4
.LBB18_6:
	ldw r17, r12+56
	ldw r3, r17+12
	add r4, r3, r16
	ldbu r3, r4+12
	ldbu r5, r18+12
	bltu r3, r5, .LBB18_20
.LBB18_7:
	ldbu r19, r1+13
	ldw r3, r12+36
	ldw r4, r4+4
	ldw r5, r18+4
	jal r31, luaK_patchlist
	ldw r1, r17+16
	addi r3, r1, -1
	bge r14, r3, .LBB18_10
.LBB18_8:
	add r1, r14, r0
.LBB18_9:
	ldw r3, r17+12
	add r3, r3, r16
	addi r1, r1, 1
	ldw r4, r3+28
	stw r3+12, r4
	ldw r4, r3+24
	stw r3+8, r4
	ldw r4, r3+20
	stw r3+4, r4
	ldw r4, r3+16
	stw r3+0, r4
	ldw r3, r17+16
	addi r3, r3, -1
	addi r16, r16, 16
	blt r1, r3, .LBB18_9
.LBB18_10:
	or  r15, r15, r19
	stw r17+16, r3
	ldw r1, r13+16
	blt r14, r1, .LBB18_5
.LBB18_11:
	addi r1, r0, 0
	beq r15, r1, .LBB18_19
.LBB18_12:
	ldbu r1, r11+50
	addi r3, r1, 1
	addi r4, r0, 20
	mul r1, r1, r4
	addi r7, r1, -11
	addi r6, r0, 0
	addi r1, r0, 1
	addi r8, r0, 3
                                        # implicit-def: $r5
	jal r0, .LBB18_14
.LBB18_13:
	addi r7, r7, -20
	bne r9, r8, .LBB18_18
.LBB18_14:
	addi r3, r3, -1
	blt r3, r1, .LBB18_17
.LBB18_15:
	ldw r9, r11+8
	ldw r9, r9+56
	ldw r9, r9+0
	ldw r10, r11+40
	mul r10, r10, r4
	add r9, r9, r10
	add r10, r9, r7
	ldbu r9, r10+0
	beq r9, r8, .LBB18_13
.LBB18_16:
	ldbu r5, r10+1
	addi r5, r5, 1
	jal r0, .LBB18_13
.LBB18_17:
	add r5, r6, r0
.LBB18_18:
	addi r4, r0, 54
	addi r6, r0, 0
	add r3, r11, r0
	add r7, r6, r0
	add r8, r6, r0
	add r11, r1, r0
	jal r31, luaK_codeABCk
	add r1, r11, r0
.LBB18_19:
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
.LBB18_20:
	add r3, r12, r0
	jal r31, jumpscopeerror
.Lfunc_end18:
	.size	createlabel, .Lfunc_end18-createlabel
                                        # -- End function
	.p2align	2                               # -- Begin function undefgoto
	.type	undefgoto,@function
undefgoto:                              # @undefgoto
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
	ldw r13, r4+0
	ldw r3, r3+40
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	addi r5, r0, 5
	jal r31, luaS_newlstr
	ldw r3, r11+40
	bne r13, r1, .LBB19_2
.LBB19_1:
	ldw r5, r12+8
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	jal r0, .LBB19_3
.LBB19_2:
	ldw r1, r12+0
	addi r5, r1, 16
	ldw r6, r12+8
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
.LBB19_3:
	jal r31, luaO_pushfstring
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaK_semerror
.Lfunc_end19:
	.size	undefgoto, .Lfunc_end19-undefgoto
                                        # -- End function
	.p2align	2                               # -- Begin function jumpscopeerror
	.type	jumpscopeerror,@function
jumpscopeerror:                         # @jumpscopeerror
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	ldw r1, r3+36
	ldbu r3, r4+12
	ldw r5, r1+8
	ldw r5, r5+56
	ldw r5, r5+0
	ldw r1, r1+40
	addi r6, r0, 20
	mul r1, r1, r6
	add r1, r5, r1
	mul r3, r3, r6
	add r1, r1, r3
	ldw r1, r1+16
	addi r7, r1, 16
	ldw r3, r11+40
	ldw r1, r4+0
	addi r5, r1, 16
	ldw r6, r4+8
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	jal r31, luaO_pushfstring
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaK_semerror
.Lfunc_end20:
	.size	jumpscopeerror, .Lfunc_end20-jumpscopeerror
                                        # -- End function
	.p2align	2                               # -- Begin function error_expected
	.type	error_expected,@function
error_expected:                         # @error_expected
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
	ldw r12, r3+40
	jal r31, luaX_token2str
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	add r3, r12, r0
	add r5, r1, r0
	jal r31, luaO_pushfstring
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaX_syntaxerror
.Lfunc_end21:
	.size	error_expected, .Lfunc_end21-error_expected
                                        # -- End function
	.p2align	2                               # -- Begin function forbody
	.type	forbody,@function
forbody:                                # @forbody
# %bb.0:
	addi sp, sp, -104
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 104
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
	add r17, r3, r0
	ldw r1, r3+12
	addi r3, r0, 258
	bne r1, r3, .LBB22_17
.LBB22_1:
	add r16, r7, r0
	add r14, r6, r0
	add r11, r5, r0
	add r13, r4, r0
	ldw r12, r17+36
	add r3, r17, r0
	jal r31, luaX_next
	slli r20, r16, 2
	lui r1, %hi(forbody.forprep)
	addi r1, r1, %lo(forbody.forprep)
	add r1, r20, r1
	ldw r4, r1+0
	addi r18, r0, 0
	add r3, r12, r0
	add r5, r13, r0
	add r6, r18, r0
	jal r31, luaK_codeABx
	add r15, r1, r0
	addi r1, fp, -92
	stb r1+14, r18
	ldbu r3, r12+50
	stb r1+12, r3
	ldw r3, r12+8
	ldw r3, r3+56
	ldw r4, r3+28
	stw r1+4, r4
	ldw r3, r3+16
	stw r1+8, r3
	stb r1+13, r18
	ldw r3, r12+12
	add r4, r18, r0
	beq r3, r18, .LBB22_3
.LBB22_2:
	ldbu r4, r3+15
	addi r5, r0, 0
	sne r4, r4, r5
.LBB22_3:
	stb r1+15, r4
	stw r1+0, r3
	stw r12+12, r1
	add r3, r17, r0
	add r4, r14, r0
	jal r31, adjustlocalvars
	add r3, r12, r0
	add r4, r14, r0
	jal r31, luaK_reserveregs
	ldw r19, r17+36
	addi r1, fp, -76
	stb r1+14, r18
	ldbu r3, r19+50
	stb r1+12, r3
	ldw r3, r19+8
	ldw r3, r3+56
	ldw r4, r3+28
	stw r1+4, r4
	ldw r3, r3+16
	stw r1+8, r3
	stb r1+13, r18
	ldw r3, r19+12
	add r4, r18, r0
	beq r3, r18, .LBB22_5
.LBB22_4:
	ldbu r4, r3+15
	addi r5, r0, 0
	sne r4, r4, r5
.LBB22_5:
	stb r1+15, r4
	stw r1+0, r3
	stw r19+12, r1
	addi r21, r0, 29
	addi r22, r0, 1
	lui r1, 131104
	addi r23, r1, 7
	addi r24, r0, 14
	jal r0, .LBB22_7
.LBB22_6:
	add r3, r17, r0
	jal r31, statement
.LBB22_7:
	ldw r1, r17+12
	addi r1, r1, -259
	bgtu r1, r21, .LBB22_6
.LBB22_8:
	sll r3, r22, r1
	and r3, r3, r23
	bne r3, r18, .LBB22_11
.LBB22_9:
	bne r1, r24, .LBB22_6
.LBB22_10:
	add r3, r17, r0
	jal r31, statement
.LBB22_11:
	add r3, r19, r0
	jal r31, leaveblock
	add r3, r12, r0
	jal r31, leaveblock
	add r3, r12, r0
	jal r31, luaK_getlabel
	addi r3, r0, -1
	xor r3, r15, r3
	add r1, r1, r3
	lui r17, 32
	bge r1, r17, .LBB22_16
.LBB22_12:
	ldw r3, r12+0
	ldw r3, r3+52
	slli r4, r15, 2
	add r3, r3, r4
	ldw r4, r3+0
	lui r5, 8
	addi r18, r5, -1
	and r4, r4, r18
	slli r1, r1, 15
	or  r1, r4, r1
	stw r3+0, r1
	addi r1, r0, 0
	beq r16, r1, .LBB22_14
.LBB22_13:
	addi r4, r0, 76
	addi r6, r0, 0
	add r3, r12, r0
	add r5, r13, r0
	add r7, r14, r0
	add r8, r6, r0
	jal r31, luaK_codeABCk
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaK_fixline
.LBB22_14:
	lui r1, %hi(forbody.forloop)
	addi r1, r1, %lo(forbody.forloop)
	add r1, r20, r1
	ldw r4, r1+0
	addi r6, r0, 0
	add r3, r12, r0
	add r5, r13, r0
	jal r31, luaK_codeABx
	sub r3, r1, r15
	bge r3, r17, .LBB22_16
.LBB22_15:
	ldw r4, r12+0
	ldw r4, r4+52
	slli r1, r1, 2
	add r1, r4, r1
	ldw r4, r1+0
	and r4, r4, r18
	slli r3, r3, 15
	or  r3, r4, r3
	stw r1+0, r3
	add r3, r12, r0
	add r4, r11, r0
	jal r31, luaK_fixline
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
	addi sp, sp, 104
	jalr r0, r31, 0
.LBB22_16:
	ldw r3, r12+8
	lui r4, %hi(.L.str.21)
	addi r4, r4, %lo(.L.str.21)
	jal r31, luaX_syntaxerror
.LBB22_17:
	addi r4, r0, 258
	add r3, r17, r0
	jal r31, error_expected
.Lfunc_end22:
	.size	forbody, .Lfunc_end22-forbody
                                        # -- End function
	.p2align	2                               # -- Begin function restassign
	.type	restassign,@function
restassign:                             # @restassign
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
	stw fp+-44, lr
	add r14, r4, r0
	add r11, r3, r0
	ldw r1, r4+4
	addi r3, r1, -9
	addi r4, r0, 7
	bgeu r3, r4, .LBB23_50
.LBB23_1:
	add r12, r5, r0
	ldw r3, r11+36
	addi r16, r0, 9
	beq r1, r16, .LBB23_7
.LBB23_2:
	addi r4, r0, 10
	beq r1, r4, .LBB23_5
.LBB23_3:
	addi r3, r0, 11
	bne r1, r3, .LBB23_10
.LBB23_4:
	ldw r1, r11+56
	ldw r1, r1+0
	ldw r3, r14+8
	addi r4, r0, 20
	mul r3, r3, r4
	add r1, r1, r3
	ldw r1, r1+16
	jal r0, .LBB23_9
.LBB23_5:
	ldw r1, r3+0
	ldw r1, r1+60
	ldw r3, r14+8
	slli r3, r3, 3
	add r3, r1, r3
	ldbu r4, r3+6
	addi r1, r0, 0
	beq r4, r1, .LBB23_9
.LBB23_6:
	ldw r1, r3+0
	jal r0, .LBB23_9
.LBB23_7:
	ldhu r1, r14+10
	ldw r4, r3+8
	ldw r4, r4+56
	ldw r4, r4+0
	ldw r3, r3+40
	addi r5, r0, 20
	mul r3, r3, r5
	add r3, r4, r3
	mul r1, r1, r5
	add r3, r3, r1
	ldbu r4, r3+9
	addi r1, r0, 0
	beq r4, r1, .LBB23_9
.LBB23_8:
	ldw r1, r3+16
.LBB23_9:
	addi r3, r0, 0
	bne r1, r3, .LBB23_51
.LBB23_10:
	addi r13, r14, 4
	ldw r1, r11+12
	addi r3, r0, 61
	beq r1, r3, .LBB23_28
.LBB23_11:
	addi r3, r0, 44
	bne r1, r3, .LBB23_52
.LBB23_12:
	add r3, r11, r0
	jal r31, luaX_next
	addi r17, fp, -68
	stw r17+0, r14
	addi r4, r17, 4
	add r3, r11, r0
	jal r31, suffixedexp
	ldw r3, r17+4
	addi r1, r0, -4
	and r4, r3, r1
	addi r3, r0, 12
	beq r4, r3, .LBB23_47
.LBB23_13:
	ldw r15, r11+36
	ldbu r5, r15+52
	addi r4, r0, 0
	addi r6, r0, 13
	addi r7, r0, 10
	addi r8, r0, 15
	addi r9, r0, 1
	add r10, r4, r0
	jal r0, .LBB23_16
.LBB23_14:
	add r10, r9, r0
.LBB23_15:
	ldw r14, r14+0
	beq r14, r4, .LBB23_35
.LBB23_16:
	ldw r19, r14+4
	and r18, r19, r1
	bne r18, r3, .LBB23_15
.LBB23_17:
	ldw r18, r17+4
	bne r19, r6, .LBB23_21
.LBB23_18:
	bne r18, r7, .LBB23_15
.LBB23_19:
	ldbu r18, r14+10
	ldw r19, r17+8
	bne r19, r18, .LBB23_15
.LBB23_20:
	stw r14+4, r8
	stb r14+10, r5
	jal r0, .LBB23_14
.LBB23_21:
	bne r18, r16, .LBB23_24
.LBB23_22:
	ldbu r19, r14+10
	ldbu r20, r17+8
	bne r19, r20, .LBB23_24
.LBB23_23:
	stb r14+10, r5
	add r10, r9, r0
.LBB23_24:
	bne r18, r16, .LBB23_15
.LBB23_25:
	ldw r18, r14+4
	bne r18, r3, .LBB23_15
.LBB23_26:
	ldh r18, r14+8
	ldbu r19, r17+8
	bne r18, r19, .LBB23_15
.LBB23_27:
	sth r14+8, r5
	jal r0, .LBB23_14
.LBB23_28:
	add r3, r11, r0
	jal r31, luaX_next
	addi r17, fp, -68
	addi r14, r0, 0
	add r3, r11, r0
	add r4, r17, r0
	add r5, r14, r0
	jal r31, subexpr
	ldw r1, r11+12
	addi r18, r0, 1
	addi r19, r0, 44
	bne r1, r19, .LBB23_31
.LBB23_29:
	addi r15, fp, -68
	addi r16, r0, 0
.LBB23_30:
	add r3, r11, r0
	jal r31, luaX_next
	ldw r3, r11+36
	add r4, r15, r0
	jal r31, luaK_exp2nextreg
	add r3, r11, r0
	add r4, r15, r0
	add r5, r16, r0
	jal r31, subexpr
	addi r18, r18, 1
	ldw r1, r11+12
	beq r1, r19, .LBB23_30
.LBB23_31:
	ldw r15, r11+36
	bne r12, r18, .LBB23_33
.LBB23_32:
	addi r14, fp, -68
	add r3, r15, r0
	add r4, r14, r0
	jal r31, luaK_setoneret
	ldw r3, r11+36
	add r4, r13, r0
	add r5, r14, r0
	jal r31, luaK_storevar
	bne r12, r18, .LBB23_48
	jal r0, .LBB23_49
.LBB23_33:
	sub r16, r12, r18
	ldw r1, r17+0
	addi r3, r1, -18
	addi r4, r0, 2
	bgeu r3, r4, .LBB23_38
.LBB23_34:
	addi r1, r0, -1
	xor r3, r16, r1
	sgt r1, r16, r1
	sub r1, r14, r1
	and r1, r3, r1
	sub r5, r14, r1
	addi r4, fp, -68
	add r3, r15, r0
	jal r31, luaK_setreturns
	jal r0, .LBB23_42
.LBB23_35:
	addi r1, r0, 0
	beq r10, r1, .LBB23_47
.LBB23_36:
	ldw r1, r17+4
	bne r1, r16, .LBB23_45
.LBB23_37:
	ldbu r6, r17+8
	addi r4, r0, 0
	add r3, r15, r0
	add r7, r4, r0
	add r8, r4, r0
	jal r0, .LBB23_46
.LBB23_38:
	beq r1, r14, .LBB23_40
.LBB23_39:
	addi r4, fp, -68
	add r3, r15, r0
	jal r31, luaK_exp2nextreg
.LBB23_40:
	addi r1, r0, 1
	blt r16, r1, .LBB23_42
.LBB23_41:
	ldbu r4, r15+52
	add r3, r15, r0
	add r5, r16, r0
	jal r31, luaK_nil
.LBB23_42:
	addi r1, r0, 1
	blt r16, r1, .LBB23_44
.LBB23_43:
	add r3, r15, r0
	add r4, r16, r0
	jal r31, luaK_reserveregs
	bne r12, r18, .LBB23_48
	jal r0, .LBB23_49
.LBB23_44:
	ldbu r1, r15+52
	add r1, r1, r16
	stb r15+52, r1
	bne r12, r18, .LBB23_48
	jal r0, .LBB23_49
.LBB23_45:
	ldw r6, r17+8
	addi r4, r0, 9
	addi r7, r0, 0
	add r3, r15, r0
	add r8, r7, r0
.LBB23_46:
	jal r31, luaK_codeABCk
	addi r4, r0, 1
	add r3, r15, r0
	jal r31, luaK_reserveregs
.LBB23_47:
	ldw r3, r11+40
	jal r31, luaE_incCstack
	addi r5, r12, 1
	addi r4, fp, -68
	add r3, r11, r0
	jal r31, restassign
	ldw r1, r11+40
	ldw r3, r1+96
	addi r3, r3, -1
	stw r1+96, r3
.LBB23_48:
	ldw r3, r11+36
	ldbu r1, r3+52
	addi r1, r1, -1
	addi r4, r0, -1
	addi r5, fp, -68
	stw r5+12, r4
	stw r5+16, r4
	addi r4, r0, 8
	stw r5+0, r4
	stw r5+4, r1
	add r4, r13, r0
	jal r31, luaK_storevar
.LBB23_49:
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
	addi sp, sp, 88
	jalr r0, r31, 0
.LBB23_50:
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	add r3, r11, r0
	jal r31, luaX_syntaxerror
.LBB23_51:
	ldw r3, r11+40
	addi r5, r1, 16
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	jal r31, luaO_pushfstring
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaK_semerror
.LBB23_52:
	addi r4, r0, 61
	add r3, r11, r0
	jal r31, error_expected
.Lfunc_end23:
	.size	restassign, .Lfunc_end23-restassign
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"upvalues"
	.size	.L.str, 9

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"main function"
	.size	.L.str.1, 14

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"function at line %d"
	.size	.L.str.2, 20

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"too many %s (limit is %d) in %s"
	.size	.L.str.3, 32

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"break"
	.size	.L.str.4, 6

	.type	priority,@object                # @priority
	.section	.rodata,"a",@progbits
priority:
	.byte	10                              # 0xa
	.byte	10                              # 0xa
	.byte	10                              # 0xa
	.byte	10                              # 0xa
	.byte	11                              # 0xb
	.byte	11                              # 0xb
	.byte	11                              # 0xb
	.byte	11                              # 0xb
	.byte	14                              # 0xe
	.byte	13                              # 0xd
	.byte	11                              # 0xb
	.byte	11                              # 0xb
	.byte	11                              # 0xb
	.byte	11                              # 0xb
	.byte	6                               # 0x6
	.byte	6                               # 0x6
	.byte	4                               # 0x4
	.byte	4                               # 0x4
	.byte	5                               # 0x5
	.byte	5                               # 0x5
	.byte	7                               # 0x7
	.byte	7                               # 0x7
	.byte	7                               # 0x7
	.byte	7                               # 0x7
	.byte	9                               # 0x9
	.byte	8                               # 0x8
	.byte	3                               # 0x3
	.byte	3                               # 0x3
	.byte	3                               # 0x3
	.byte	3                               # 0x3
	.byte	3                               # 0x3
	.byte	3                               # 0x3
	.byte	3                               # 0x3
	.byte	3                               # 0x3
	.byte	3                               # 0x3
	.byte	3                               # 0x3
	.byte	3                               # 0x3
	.byte	3                               # 0x3
	.byte	2                               # 0x2
	.byte	2                               # 0x2
	.byte	1                               # 0x1
	.byte	1                               # 0x1
	.size	priority, 42

	.type	.L.str.5,@object                # @.str.5
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.5:
	.asciz	"cannot use '...' outside a vararg function"
	.size	.L.str.5, 43

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"self"
	.size	.L.str.7, 5

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"functions"
	.size	.L.str.8, 10

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"local variables"
	.size	.L.str.9, 16

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"<name> or '...' expected"
	.size	.L.str.10, 25

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"unexpected symbol"
	.size	.L.str.11, 18

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"function arguments expected"
	.size	.L.str.12, 28

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"labels/gotos"
	.size	.L.str.13, 13

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"<goto %s> at line %d jumps into the scope of local '%s'"
	.size	.L.str.14, 56

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"break outside loop at line %d"
	.size	.L.str.15, 30

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"no visible label '%s' for <goto> at line %d"
	.size	.L.str.16, 44

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"%s expected (to close %s at line %d)"
	.size	.L.str.17, 37

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"%s expected"
	.size	.L.str.18, 12

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"'=' or 'in' expected"
	.size	.L.str.19, 21

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"(for state)"
	.size	.L.str.20, 12

	.type	forbody.forprep,@object         # @forbody.forprep
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0
forbody.forprep:
	.word	74                              # 0x4a
	.word	75                              # 0x4b
	.size	forbody.forprep, 8

	.type	forbody.forloop,@object         # @forbody.forloop
	.p2align	2, 0x0
forbody.forloop:
	.word	73                              # 0x49
	.word	77                              # 0x4d
	.size	forbody.forloop, 8

	.type	.L.str.21,@object               # @.str.21
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.21:
	.asciz	"control structure too long"
	.size	.L.str.21, 27

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"attempt to assign to const variable '%s'"
	.size	.L.str.22, 41

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"multiple to-be-closed variables in local list"
	.size	.L.str.23, 46

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"const"
	.size	.L.str.24, 6

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"close"
	.size	.L.str.25, 6

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"unknown attribute '%s'"
	.size	.L.str.26, 23

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"label '%s' already defined on line %d"
	.size	.L.str.27, 38

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"syntax error"
	.size	.L.str.28, 13

	.hidden	luaF_newLclosure
	.hidden	luaD_inctop
	.hidden	luaH_new
	.hidden	luaF_newproto
	.hidden	luaC_barrier_
	.hidden	luaS_new
	.hidden	luaX_setinput
	.hidden	luaX_next
	.hidden	luaK_codeABCk
	.hidden	luaM_growaux_
	.hidden	luaO_pushfstring
	.hidden	luaX_syntaxerror
	.hidden	luaE_incCstack
	.hidden	luaK_patchtohere
	.hidden	luaK_goiffalse
	.hidden	luaS_newlstr
	.hidden	luaK_jump
	.hidden	luaK_goiftrue
	.hidden	luaK_concat
	.hidden	luaK_prefix
	.hidden	luaK_infix
	.hidden	luaK_posfix
	.hidden	luaK_code
	.hidden	luaK_reserveregs
	.hidden	luaK_settablesize
	.hidden	luaK_exp2nextreg
	.hidden	luaK_setlist
	.hidden	luaX_lookahead
	.hidden	luaK_indexed
	.hidden	luaK_storevar
	.hidden	luaK_exp2val
	.hidden	luaK_setreturns
	.hidden	luaX_newstring
	.hidden	luaK_codeABx
	.hidden	luaK_exp2anyregup
	.hidden	luaK_self
	.hidden	luaK_dischargevars
	.hidden	luaK_fixline
	.hidden	luaK_getlabel
	.hidden	luaK_patchlist
	.hidden	luaK_semerror
	.hidden	luaX_token2str
	.hidden	luaK_int
	.hidden	luaK_checkstack
	.hidden	luaK_nil
	.hidden	luaK_exp2const
	.hidden	luaK_exp2anyreg
	.hidden	luaK_ret
	.hidden	luaK_setoneret
	.hidden	luaK_finish
	.hidden	luaM_shrinkvector_
	.hidden	luaC_step
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
