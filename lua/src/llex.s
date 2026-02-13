	.file	"llex.c"
	.text
	.hidden	luaX_init                       # -- Begin function luaX_init
	.globl	luaX_init
	.p2align	2
	.type	luaX_init,@function
luaX_init:                              # @luaX_init
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
	add r11, r3, r0
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	addi r5, r0, 4
	jal r31, luaS_newlstr
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaC_fix
	addi r13, r0, -88
	addi r14, r0, 1
	lui r15, %hi(luaX_tokens+88)
	addi r15, r15, %lo(luaX_tokens+88)
	addi r16, r0, 0
.LBB0_1:
	add r1, r13, r15
	ldw r4, r1+0
	add r3, r11, r0
	jal r31, luaS_new
	add r12, r1, r0
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaC_fix
	stb r12+6, r14
	addi r13, r13, 4
	addi r14, r14, 1
	bne r13, r16, .LBB0_1
.LBB0_2:
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
.Lfunc_end0:
	.size	luaX_init, .Lfunc_end0-luaX_init
                                        # -- End function
	.hidden	luaX_token2str                  # -- Begin function luaX_token2str
	.globl	luaX_token2str
	.p2align	2
	.type	luaX_token2str,@function
luaX_token2str:                         # @luaX_token2str
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	add r5, r4, r0
	addi r1, r0, 255
	bgt r4, r1, .LBB1_3
.LBB1_1:
	lui r1, %hi(luai_ctype_+1)
	addi r1, r1, %lo(luai_ctype_+1)
	add r1, r5, r1
	ldbu r1, r1+0
	andi r1, r1, 4
	ldw r3, r3+40
	addi r4, r0, 0
	bne r1, r4, .LBB1_5
.LBB1_2:
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r0, .LBB1_6
.LBB1_3:
	slli r1, r5, 2
	lui r4, %hi(luaX_tokens-1024)
	addi r4, r4, %lo(luaX_tokens-1024)
	add r1, r1, r4
	ldw r1, r1+0
	addi r4, r0, 287
	bgt r5, r4, .LBB1_7
.LBB1_4:
	ldw r3, r3+40
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	add r5, r1, r0
	jal r0, .LBB1_6
.LBB1_5:
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
.LBB1_6:
	jal r31, luaO_pushfstring
.LBB1_7:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	luaX_token2str, .Lfunc_end1-luaX_token2str
                                        # -- End function
	.hidden	luaX_syntaxerror                # -- Begin function luaX_syntaxerror
	.globl	luaX_syntaxerror
	.p2align	2
	.type	luaX_syntaxerror,@function
luaX_syntaxerror:                       # @luaX_syntaxerror
# %bb.0:
	addi sp, sp, -8
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 8
	ldw r5, r3+12
	jal r31, lexerror
.Lfunc_end2:
	.size	luaX_syntaxerror, .Lfunc_end2-luaX_syntaxerror
                                        # -- End function
	.p2align	2                               # -- Begin function lexerror
	.type	lexerror,@function
lexerror:                               # @lexerror
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
	add r14, r5, r0
	add r11, r3, r0
	ldw r3, r3+40
	ldw r5, r11+60
	ldw r6, r11+4
	jal r31, luaG_addinfo
	addi r15, r0, 0
	beq r14, r15, .LBB3_16
.LBB3_1:
	add r12, r1, r0
	ldw r13, r11+40
	addi r1, r14, -289
	addi r3, r0, 3
	bgtu r1, r3, .LBB3_5
.LBB3_2:
	ldw r16, r11+48
	ldw r1, r16+4
	addi r1, r1, 1
	ldw r5, r16+8
	bleu r1, r5, .LBB3_11
.LBB3_3:
	lui r1, 262144
	addi r1, r1, -1
	bltu r5, r1, .LBB3_10
.LBB3_4:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lexerror
.LBB3_5:
	addi r1, r0, 255
	bgt r14, r1, .LBB3_8
.LBB3_6:
	lui r1, %hi(luai_ctype_+1)
	addi r1, r1, %lo(luai_ctype_+1)
	add r1, r14, r1
	ldbu r1, r1+0
	andi r1, r1, 4
	bne r1, r15, .LBB3_12
.LBB3_7:
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r0, .LBB3_13
.LBB3_8:
	slli r1, r14, 2
	lui r3, %hi(luaX_tokens-1024)
	addi r3, r3, %lo(luaX_tokens-1024)
	add r1, r1, r3
	ldw r6, r1+0
	addi r1, r0, 287
	bgt r14, r1, .LBB3_15
.LBB3_9:
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	add r3, r13, r0
	add r5, r6, r0
	jal r0, .LBB3_14
.LBB3_10:
	slli r14, r5, 1
	ldw r4, r16+0
	add r3, r13, r0
	add r6, r14, r0
	jal r31, luaM_saferealloc_
	stw r16+0, r1
	stw r16+8, r14
.LBB3_11:
	ldw r1, r16+0
	ldw r3, r16+4
	addi r4, r3, 1
	stw r16+4, r4
	add r1, r1, r3
	stb r1+0, r15
	ldw r3, r11+40
	ldw r1, r11+48
	ldw r5, r1+0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r0, .LBB3_14
.LBB3_12:
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
.LBB3_13:
	add r3, r13, r0
	add r5, r14, r0
.LBB3_14:
	jal r31, luaO_pushfstring
	add r6, r1, r0
.LBB3_15:
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	add r3, r13, r0
	add r5, r12, r0
	jal r31, luaO_pushfstring
.LBB3_16:
	ldw r3, r11+40
	addi r4, r0, 3
	jal r31, luaD_throw
.Lfunc_end3:
	.size	lexerror, .Lfunc_end3-lexerror
                                        # -- End function
	.hidden	luaX_newstring                  # -- Begin function luaX_newstring
	.globl	luaX_newstring
	.p2align	2
	.type	luaX_newstring,@function
luaX_newstring:                         # @luaX_newstring
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
	add r13, r3, r0
	ldw r11, r3+40
	add r3, r11, r0
	jal r31, luaS_newlstr
	add r12, r1, r0
	ldw r3, r13+52
	add r4, r1, r0
	jal r31, luaH_getstr
	ldbu r3, r1+8
	andi r3, r3, 15
	addi r4, r0, 0
	beq r3, r4, .LBB4_2
.LBB4_1:
	ldw r12, r1+16
	jal r0, .LBB4_5
.LBB4_2:
	ldw r5, r11+12
	addi r3, r5, 12
	stw r11+12, r3
	stw r5+0, r12
	ldbu r3, r12+4
	ori  r3, r3, 64
	stb r5+8, r3
	ldw r4, r13+52
	add r3, r11, r0
	add r6, r1, r0
	add r7, r5, r0
	jal r31, luaH_finishset
	ldw r1, r11+16
	ldw r1, r1+12
	addi r3, r0, 1
	blt r1, r3, .LBB4_4
.LBB4_3:
	add r3, r11, r0
	jal r31, luaC_step
.LBB4_4:
	ldw r1, r11+12
	addi r1, r1, -12
	stw r11+12, r1
.LBB4_5:
	add r1, r12, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	luaX_newstring, .Lfunc_end4-luaX_newstring
                                        # -- End function
	.hidden	luaX_setinput                   # -- Begin function luaX_setinput
	.globl	luaX_setinput
	.p2align	2
	.type	luaX_setinput,@function
luaX_setinput:                          # @luaX_setinput
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
	addi r1, r0, 0
	stw r4+12, r1
	stw r4+40, r3
	stw r4+0, r7
	addi r4, r0, 288
	stw r11+24, r4
	stw r11+44, r5
	stw r11+36, r1
	addi r1, r0, 1
	stw r11+4, r1
	stw r11+8, r1
	stw r11+60, r6
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	addi r5, r0, 4
	jal r31, luaS_newlstr
	stw r11+64, r1
	ldw r3, r11+40
	ldw r1, r11+48
	ldw r4, r1+0
	ldw r5, r1+8
	addi r12, r0, 32
	add r6, r12, r0
	jal r31, luaM_saferealloc_
	ldw r3, r11+48
	stw r3+0, r1
	stw r3+8, r12
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	luaX_setinput, .Lfunc_end5-luaX_setinput
                                        # -- End function
	.hidden	luaX_next                       # -- Begin function luaX_next
	.globl	luaX_next
	.p2align	2
	.type	luaX_next,@function
luaX_next:                              # @luaX_next
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+4
	stw r3+8, r1
	ldw r4, r3+24
	addi r1, r0, 288
	bne r4, r1, .LBB6_2
.LBB6_1:
	addi r4, r3, 16
	add r11, r3, r0
	jal r31, llex
	stw r11+12, r1
	jal r0, .LBB6_3
.LBB6_2:
	addi r4, r3, 24
	addi r3, r3, 12
	ldw r5, r4+8
	stw r3+8, r5
	ldw r5, r4+4
	stw r3+4, r5
	ldw r5, r4+0
	stw r3+0, r5
	stw r4+0, r1
.LBB6_3:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	luaX_next, .Lfunc_end6-luaX_next
                                        # -- End function
	.p2align	2                               # -- Begin function llex
	.type	llex,@function
llex:                                   # @llex
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
	add r13, r4, r0
	add r11, r3, r0
	ldw r1, r3+48
	addi r12, r0, 0
	stw r1+4, r12
	addi r14, r0, 288
	addi r19, r0, 127
	lui r21, %hi(.LJTI7_0)
	addi r21, r21, %lo(.LJTI7_0)
	addi r20, r0, 45
	addi r15, r0, 91
	addi r18, r0, 2
	addi r22, r0, 14
	addi r16, r0, 1
	lui r1, 5
	addi r23, r1, -2047
	jal r0, .LBB7_2
.LBB7_1:
	add r3, r11, r0
	jal r31, inclinenumber
.LBB7_2:
	ldw r17, r11+0
	addi r1, r17, 1
	bgtu r1, r19, .LBB7_26
.LBB7_3:
	slli r1, r1, 2
	add r1, r21, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB7_4:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_6
.LBB7_5:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	stw r11+0, r1
	jal r0, .LBB7_2
.LBB7_6:
	jal r31, luaZ_fill
	stw r11+0, r1
	jal r0, .LBB7_2
.LBB7_7:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_9
.LBB7_8:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	stw r11+0, r1
	beq r1, r20, .LBB7_10
	jal r0, .LBB7_50
.LBB7_9:
	jal r31, luaZ_fill
	stw r11+0, r1
	bne r1, r20, .LBB7_50
.LBB7_10:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_12
.LBB7_11:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	stw r11+0, r1
	bne r1, r15, .LBB7_17
	jal r0, .LBB7_13
.LBB7_12:
	jal r31, luaZ_fill
	stw r11+0, r1
	bne r1, r15, .LBB7_17
.LBB7_13:
	add r3, r11, r0
	jal r31, skip_sep
	ldw r3, r11+48
	stw r3+4, r12
	bltu r1, r18, .LBB7_17
.LBB7_14:
	add r3, r11, r0
	add r4, r12, r0
	add r5, r1, r0
	jal r31, read_long_string
	ldw r1, r11+48
	stw r1+4, r12
	jal r0, .LBB7_2
.LBB7_15:
	jal r31, luaZ_fill
.LBB7_16:
	stw r11+0, r1
.LBB7_17:
	ldw r1, r11+0
	addi r1, r1, 1
	bgtu r1, r22, .LBB7_19
.LBB7_18:
	sll r1, r16, r1
	and r1, r1, r23
	bne r1, r12, .LBB7_2
.LBB7_19:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_15
.LBB7_20:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_16
.LBB7_21:
	ldw r15, r11+48
	ldw r1, r15+4
	addi r1, r1, 1
	ldw r5, r15+8
	bleu r1, r5, .LBB7_24
.LBB7_22:
	lui r1, 262144
	addi r1, r1, -1
	bgeu r5, r1, .LBB7_216
.LBB7_23:
	slli r14, r5, 1
	ldw r3, r11+40
	ldw r4, r15+0
	add r6, r14, r0
	jal r31, luaM_saferealloc_
	stw r15+0, r1
	stw r15+8, r14
.LBB7_24:
	ldw r1, r15+0
	ldw r3, r15+4
	addi r4, r3, 1
	stw r15+4, r4
	add r1, r1, r3
	stb r1+0, r17
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_51
.LBB7_25:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r14, r1+0
	stw r11+0, r14
	beq r14, r17, .LBB7_52
	jal r0, .LBB7_57
.LBB7_26:
	lui r14, %hi(luai_ctype_+1)
	addi r14, r14, %lo(luai_ctype_+1)
	add r1, r17, r14
	ldbu r1, r1+0
	andi r1, r1, 1
	addi r15, r0, 0
	bne r1, r15, .LBB7_151
.LBB7_27:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r15, .LBB7_195
.LBB7_28:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_196
.LBB7_29:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_164
.LBB7_30:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_165
.LBB7_31:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_168
.LBB7_32:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_169
.LBB7_33:
	ldw r15, r11+48
	ldw r1, r15+4
	addi r1, r1, 1
	ldw r5, r15+8
	bleu r1, r5, .LBB7_36
.LBB7_34:
	lui r1, 262144
	addi r1, r1, -1
	bgeu r5, r1, .LBB7_216
.LBB7_35:
	slli r14, r5, 1
	ldw r3, r11+40
	ldw r4, r15+0
	add r6, r14, r0
	jal r31, luaM_saferealloc_
	stw r15+0, r1
	stw r15+8, r14
.LBB7_36:
	ldw r1, r15+0
	ldw r3, r15+4
	addi r4, r3, 1
	stw r15+4, r4
	add r1, r1, r3
	addi r14, r0, 46
	stb r1+0, r14
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_191
.LBB7_37:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	stw r11+0, r1
	bne r1, r14, .LBB7_192
.LBB7_38:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_209
.LBB7_39:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_210
.LBB7_40:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_173
.LBB7_41:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_174
.LBB7_42:
	add r3, r11, r0
	jal r31, skip_sep
	bltu r1, r18, .LBB7_159
.LBB7_43:
	add r3, r11, r0
	add r4, r13, r0
	add r5, r1, r0
	jal r31, read_long_string
	addi r14, r0, 292
	jal r0, .LBB7_194
.LBB7_44:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_178
.LBB7_45:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_179
.LBB7_46:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_182
.LBB7_47:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_183
.LBB7_48:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_187
.LBB7_49:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_188
.LBB7_50:
	add r14, r20, r0
	jal r0, .LBB7_194
.LBB7_51:
	jal r31, luaZ_fill
	add r14, r1, r0
	stw r11+0, r14
	bne r14, r17, .LBB7_57
.LBB7_52:
	ldw r15, r11+48
	ldw r1, r15+4
	addi r1, r1, 1
	ldw r5, r15+8
	bleu r1, r5, .LBB7_55
.LBB7_53:
	lui r1, 262144
	addi r1, r1, -1
	bgeu r5, r1, .LBB7_216
.LBB7_54:
	slli r12, r5, 1
	ldw r3, r11+40
	ldw r4, r15+0
	add r6, r12, r0
	jal r31, luaM_saferealloc_
	stw r15+0, r1
	stw r15+8, r12
.LBB7_55:
	ldw r1, r15+0
	ldw r3, r15+4
	addi r4, r3, 1
	stw r15+4, r4
	add r1, r1, r3
	stb r1+0, r14
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r4, r0, 0
	beq r1, r4, .LBB7_149
.LBB7_56:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_150
.LBB7_57:
	addi r1, fp, -84
	addi r1, r1, 8
	stw fp+-100, r1
	addi r20, r0, 91
	addi r25, r0, -1
	addi r21, r0, 10
	addi r22, r0, 13
	lui r1, 262144
	addi r23, r1, -1
	addi r24, r0, 92
	addi r19, r0, 7
	addi r26, r0, 123
	lui r27, %hi(luai_ctype_+1)
	addi r27, r27, %lo(luai_ctype_+1)
	addi r1, r0, 256
	stw fp+-92, r1
	stw fp+-88, r25
	jal r0, .LBB7_61
.LBB7_58:
	slli r14, r5, 1
	ldw r3, r11+40
	ldw r4, r18+0
	add r6, r14, r0
	jal r31, luaM_saferealloc_
	stw r18+0, r1
	stw r18+8, r14
.LBB7_59:
	ldw r1, r18+0
	ldw r3, r18+4
	addi r4, r3, 1
	stw r18+4, r4
	add r1, r1, r3
	stb r1+0, r15
.LBB7_60:
	ldw r14, r11+0
	beq r14, r17, .LBB7_52
.LBB7_61:
	ble r14, r20, .LBB7_68
.LBB7_62:
	bne r14, r24, .LBB7_71
.LBB7_63:
	ldw r15, r11+48
	ldw r1, r15+4
	addi r1, r1, 1
	ldw r5, r15+8
	bleu r1, r5, .LBB7_66
.LBB7_64:
	bgeu r5, r23, .LBB7_216
.LBB7_65:
	slli r14, r5, 1
	ldw r3, r11+40
	ldw r4, r15+0
	add r6, r14, r0
	jal r31, luaM_saferealloc_
	stw r15+0, r1
	stw r15+8, r14
.LBB7_66:
	ldw r1, r15+0
	ldw r3, r15+4
	addi r4, r3, 1
	stw r15+4, r4
	add r1, r1, r3
	stb r1+0, r24
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_78
.LBB7_67:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_79
.LBB7_68:
	beq r14, r25, .LBB7_221
.LBB7_69:
	beq r14, r21, .LBB7_217
.LBB7_70:
	beq r14, r22, .LBB7_217
.LBB7_71:
	ldw r18, r11+48
	ldw r1, r18+4
	addi r1, r1, 1
	ldw r5, r18+8
	bleu r1, r5, .LBB7_74
.LBB7_72:
	bgeu r5, r23, .LBB7_216
.LBB7_73:
	slli r15, r5, 1
	ldw r3, r11+40
	ldw r4, r18+0
	add r6, r15, r0
	jal r31, luaM_saferealloc_
	stw r18+0, r1
	stw r18+8, r15
.LBB7_74:
	ldw r1, r18+0
	ldw r3, r18+4
	addi r4, r3, 1
	stw r18+4, r4
	add r1, r1, r3
	stb r1+0, r14
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_76
.LBB7_75:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_77
.LBB7_76:
	jal r31, luaZ_fill
.LBB7_77:
	stw r11+0, r1
	jal r0, .LBB7_60
.LBB7_78:
	jal r31, luaZ_fill
.LBB7_79:
	stw r11+0, r1
	addi r3, r1, 1
	bgtu r3, r26, .LBB7_103
.LBB7_80:
	slli r3, r3, 2
	lui r4, %hi(.LJTI7_1)
	addi r4, r4, %lo(.LJTI7_1)
	add r3, r4, r3
	ldw r3, r3+0
	add r15, r19, r0
	jalr r0, r3, 0
.LBB7_81:
	add r15, r1, r0
	jal r0, .LBB7_97
.LBB7_82:
	add r3, r11, r0
	jal r31, inclinenumber
	addi r15, r0, 10
	jal r0, .LBB7_101
.LBB7_83:
	addi r15, r0, 11
	jal r0, .LBB7_97
.LBB7_84:
	add r15, r22, r0
	jal r0, .LBB7_97
.LBB7_85:
	ldw r15, r11+48
	ldw r1, r15+4
	addi r1, r1, 1
	ldw r5, r15+8
	bleu r1, r5, .LBB7_88
.LBB7_86:
	bgeu r5, r23, .LBB7_216
.LBB7_87:
	slli r14, r5, 1
	ldw r3, r11+40
	ldw r4, r15+0
	add r6, r14, r0
	jal r31, luaM_saferealloc_
	stw r15+0, r1
	stw r15+8, r14
.LBB7_88:
	stw fp+-96, r19
	ldw r1, r15+0
	ldw r3, r15+4
	addi r4, r3, 1
	stw r15+4, r4
	add r1, r1, r3
	addi r3, r0, 117
	stb r1+0, r3
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r19, r0, 0
	beq r1, r19, .LBB7_126
.LBB7_89:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_127
.LBB7_90:
	addi r15, r0, 9
	jal r0, .LBB7_97
.LBB7_91:
	addi r15, r0, 8
	jal r0, .LBB7_97
.LBB7_92:
	add r3, r11, r0
	jal r31, gethexa
	slli r14, r1, 4
	add r3, r11, r0
	jal r31, gethexa
	add r15, r14, r1
	ldw r1, r11+48
	ldw r3, r1+4
	addi r3, r3, -2
	stw r1+4, r3
	jal r0, .LBB7_97
.LBB7_93:
	ldw r1, r11+48
	ldw r3, r1+4
	addi r3, r3, -1
	stw r1+4, r3
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r14, r0, 0
	beq r1, r14, .LBB7_116
.LBB7_94:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_117
.LBB7_95:
	add r15, r21, r0
	jal r0, .LBB7_97
.LBB7_96:
	addi r15, r0, 12
.LBB7_97:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r4, r0, 0
	beq r1, r4, .LBB7_99
.LBB7_98:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_100
.LBB7_99:
	jal r31, luaZ_fill
.LBB7_100:
	stw r11+0, r1
.LBB7_101:
	ldw r18, r11+48
	ldw r1, r18+4
	addi r3, r1, -1
	stw r18+4, r3
	ldw r5, r18+8
	bleu r1, r5, .LBB7_59
.LBB7_102:
	bltu r5, r23, .LBB7_58
	jal r0, .LBB7_216
.LBB7_103:
	add r3, r1, r27
	ldbu r3, r3+0
	andi r3, r3, 2
	beq r3, r12, .LBB7_235
.LBB7_104:
	add r25, r19, r0
	add r15, r12, r0
	add r19, r12, r0
	jal r0, .LBB7_107
.LBB7_105:
	jal r31, luaZ_fill
.LBB7_106:
	mul r3, r15, r21
	add r3, r3, r28
	addi r15, r3, -48
	stw r11+0, r1
	addi r19, r19, 1
	addi r1, r0, 3
	beq r19, r1, .LBB7_114
.LBB7_107:
	ldw r28, r11+0
	add r1, r28, r27
	ldbu r1, r1+0
	andi r1, r1, 2
	beq r1, r12, .LBB7_113
.LBB7_108:
	ldw r18, r11+48
	ldw r1, r18+4
	addi r1, r1, 1
	ldw r5, r18+8
	bleu r1, r5, .LBB7_111
.LBB7_109:
	bgeu r5, r23, .LBB7_216
.LBB7_110:
	slli r14, r5, 1
	ldw r3, r11+40
	ldw r4, r18+0
	add r6, r14, r0
	jal r31, luaM_saferealloc_
	stw r18+0, r1
	stw r18+8, r14
.LBB7_111:
	ldw r1, r18+0
	ldw r3, r18+4
	addi r4, r3, 1
	stw r18+4, r4
	add r1, r1, r3
	stb r1+0, r28
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r4, r0, 0
	beq r1, r4, .LBB7_105
.LBB7_112:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_106
.LBB7_113:
	add r1, r19, r0
.LBB7_114:
	ldw r3, fp+-92
	bge r15, r3, .LBB7_232
.LBB7_115:
	ldw r3, r11+48
	ldw r4, r3+4
	sub r1, r4, r1
	stw r3+4, r1
	add r19, r25, r0
	ldw r25, fp+-88
	jal r0, .LBB7_101
.LBB7_116:
	jal r31, luaZ_fill
.LBB7_117:
	stw r11+0, r1
	jal r0, .LBB7_119
.LBB7_118:
	add r3, r11, r0
	jal r31, inclinenumber
	ldw r1, r11+0
.LBB7_119:
	add r3, r1, r27
	ldbu r3, r3+0
	andi r3, r3, 8
	beq r3, r14, .LBB7_60
.LBB7_120:
	beq r1, r22, .LBB7_118
.LBB7_121:
	beq r1, r21, .LBB7_118
.LBB7_122:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r14, .LBB7_124
.LBB7_123:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_125
.LBB7_124:
	jal r31, luaZ_fill
.LBB7_125:
	stw r11+0, r1
	ldw r1, r11+0
	jal r0, .LBB7_119
.LBB7_126:
	jal r31, luaZ_fill
.LBB7_127:
	stw r11+0, r1
	bne r1, r26, .LBB7_226
.LBB7_128:
	add r3, r11, r0
	jal r31, gethexa
	add r14, r1, r0
	addi r28, r0, -4
.LBB7_129:
	ldw r18, r11+0
	ldw r25, r11+48
	ldw r1, r25+4
	addi r1, r1, 1
	ldw r5, r25+8
	bleu r1, r5, .LBB7_132
.LBB7_130:
	bgeu r5, r23, .LBB7_216
.LBB7_131:
	slli r15, r5, 1
	ldw r3, r11+40
	ldw r4, r25+0
	add r6, r15, r0
	jal r31, luaM_saferealloc_
	stw r25+0, r1
	stw r25+8, r15
.LBB7_132:
	ldw r1, r25+0
	ldw r3, r25+4
	addi r4, r3, 1
	stw r25+4, r4
	add r1, r1, r3
	stb r1+0, r18
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r19, .LBB7_134
.LBB7_133:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_135
.LBB7_134:
	jal r31, luaZ_fill
.LBB7_135:
	stw r11+0, r1
	add r3, r1, r27
	ldbu r3, r3+0
	andi r3, r3, 16
	beq r3, r19, .LBB7_138
.LBB7_136:
	lui r3, 32768
	bgeu r14, r3, .LBB7_218
.LBB7_137:
	slli r14, r14, 4
	add r3, r1, r0
	jal r31, luaO_hexavalue
	add r14, r1, r14
	addi r28, r28, -1
	jal r0, .LBB7_129
.LBB7_138:
	addi r3, r0, 125
	bne r1, r3, .LBB7_229
.LBB7_139:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r19, .LBB7_141
.LBB7_140:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_142
.LBB7_141:
	jal r31, luaZ_fill
.LBB7_142:
	stw r11+0, r1
	ldw r1, r11+48
	ldw r3, r1+4
	add r3, r3, r28
	stw r1+4, r3
	addi r3, fp, -84
	add r4, r14, r0
	jal r31, luaO_utf8esc
	blt r1, r16, .LBB7_148
.LBB7_143:
	addi r15, r1, 1
	ldw r3, fp+-100
	sub r19, r3, r1
	jal r0, .LBB7_146
.LBB7_144:
	slli r14, r5, 1
	ldw r3, r11+40
	ldw r4, r25+0
	add r6, r14, r0
	jal r31, luaM_saferealloc_
	stw r25+0, r1
	stw r25+8, r14
.LBB7_145:
	ldw r1, r25+0
	ldw r3, r25+4
	addi r4, r3, 1
	stw r25+4, r4
	add r1, r1, r3
	stb r1+0, r18
	addi r15, r15, -1
	addi r19, r19, 1
	ble r15, r16, .LBB7_148
.LBB7_146:
	ldbu r18, r19+0
	ldw r25, r11+48
	ldw r1, r25+4
	addi r1, r1, 1
	ldw r5, r25+8
	bleu r1, r5, .LBB7_145
.LBB7_147:
	bltu r5, r23, .LBB7_144
	jal r0, .LBB7_216
.LBB7_148:
	ldw r25, fp+-88
	ldw r19, fp+-96
	jal r0, .LBB7_60
.LBB7_149:
	jal r31, luaZ_fill
.LBB7_150:
	stw r11+0, r1
	ldw r1, r11+48
	ldw r3, r1+0
	addi r4, r3, 1
	ldw r1, r1+4
	addi r5, r1, -2
	add r3, r11, r0
	jal r31, luaX_newstring
	stw r13+0, r1
	addi r14, r0, 292
	jal r0, .LBB7_194
.LBB7_151:
	lui r1, 262144
	addi r16, r1, -1
	jal r0, .LBB7_156
.LBB7_152:
	slli r12, r5, 1
	ldw r3, r11+40
	ldw r4, r18+0
	add r6, r12, r0
	jal r31, luaM_saferealloc_
	stw r18+0, r1
	stw r18+8, r12
.LBB7_153:
	ldw r1, r18+0
	ldw r3, r18+4
	addi r4, r3, 1
	stw r18+4, r4
	add r1, r1, r3
	stb r1+0, r17
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r15, .LBB7_158
.LBB7_154:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
.LBB7_155:
	stw r11+0, r1
	add r1, r1, r14
	ldbu r1, r1+0
	andi r1, r1, 3
	beq r1, r15, .LBB7_161
.LBB7_156:
	ldw r17, r11+0
	ldw r18, r11+48
	ldw r1, r18+4
	addi r1, r1, 1
	ldw r5, r18+8
	bleu r1, r5, .LBB7_153
.LBB7_157:
	bltu r5, r16, .LBB7_152
	jal r0, .LBB7_216
.LBB7_158:
	jal r31, luaZ_fill
	jal r0, .LBB7_155
.LBB7_159:
	add r14, r15, r0
	bne r1, r12, .LBB7_194
.LBB7_160:
	lui r4, %hi(.L.str.43)
	addi r4, r4, %lo(.L.str.43)
	jal r0, .LBB7_225
.LBB7_161:
	ldw r1, r11+48
	ldw r4, r1+0
	ldw r5, r1+4
	add r3, r11, r0
	jal r31, luaX_newstring
	stw r13+0, r1
	ldbu r3, r1+4
	addi r14, r0, 291
	addi r4, r0, 4
	bne r3, r4, .LBB7_194
.LBB7_162:
	ldbu r1, r1+6
	beq r1, r15, .LBB7_194
.LBB7_163:
	addi r14, r1, 255
	jal r0, .LBB7_194
.LBB7_164:
	jal r31, luaZ_fill
.LBB7_165:
	stw r11+0, r1
	addi r14, r0, 58
	bne r1, r14, .LBB7_194
.LBB7_166:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_197
.LBB7_167:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_198
.LBB7_168:
	jal r31, luaZ_fill
.LBB7_169:
	stw r11+0, r1
	addi r3, r0, 61
	bne r1, r3, .LBB7_172
.LBB7_170:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_199
.LBB7_171:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_200
.LBB7_172:
	addi r4, r0, 62
	add r3, r11, r0
	jal r31, check_next1
	seq r1, r1, r12
	sub r1, r12, r1
	andi r1, r1, 288
	xori r14, r1, 286
	jal r0, .LBB7_194
.LBB7_173:
	jal r31, luaZ_fill
.LBB7_174:
	stw r11+0, r1
	addi r3, r0, 61
	bne r1, r3, .LBB7_177
.LBB7_175:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_201
.LBB7_176:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_202
.LBB7_177:
	addi r14, r0, 126
	jal r0, .LBB7_194
.LBB7_178:
	jal r31, luaZ_fill
.LBB7_179:
	stw r11+0, r1
	addi r14, r0, 61
	bne r1, r14, .LBB7_194
.LBB7_180:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_203
.LBB7_181:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_204
.LBB7_182:
	jal r31, luaZ_fill
.LBB7_183:
	stw r11+0, r1
	addi r3, r0, 61
	bne r1, r3, .LBB7_186
.LBB7_184:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_205
.LBB7_185:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_206
.LBB7_186:
	addi r4, r0, 60
	add r3, r11, r0
	jal r31, check_next1
	seq r1, r1, r12
	sub r1, r12, r1
	andi r1, r1, 289
	xori r14, r1, 285
	jal r0, .LBB7_194
.LBB7_187:
	jal r31, luaZ_fill
.LBB7_188:
	stw r11+0, r1
	addi r14, r0, 47
	bne r1, r14, .LBB7_194
.LBB7_189:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_207
.LBB7_190:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_208
.LBB7_191:
	jal r31, luaZ_fill
	stw r11+0, r1
	beq r1, r14, .LBB7_38
.LBB7_192:
	ldw r1, r11+0
	lui r3, %hi(luai_ctype_+1)
	addi r3, r3, %lo(luai_ctype_+1)
	add r1, r1, r3
	ldbu r1, r1+0
	andi r1, r1, 2
	beq r1, r12, .LBB7_194
.LBB7_193:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, read_numeral
	add r14, r1, r0
.LBB7_194:
	add r1, r14, r0
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
	addi sp, sp, 120
	jalr r0, r31, 0
.LBB7_195:
	jal r31, luaZ_fill
.LBB7_196:
	stw r11+0, r1
	add r14, r17, r0
	jal r0, .LBB7_194
.LBB7_197:
	jal r31, luaZ_fill
.LBB7_198:
	stw r11+0, r1
	addi r14, r0, 287
	jal r0, .LBB7_194
.LBB7_199:
	jal r31, luaZ_fill
.LBB7_200:
	stw r11+0, r1
	addi r14, r0, 282
	jal r0, .LBB7_194
.LBB7_201:
	jal r31, luaZ_fill
.LBB7_202:
	stw r11+0, r1
	addi r14, r0, 284
	jal r0, .LBB7_194
.LBB7_203:
	jal r31, luaZ_fill
.LBB7_204:
	stw r11+0, r1
	addi r14, r0, 281
	jal r0, .LBB7_194
.LBB7_205:
	jal r31, luaZ_fill
.LBB7_206:
	stw r11+0, r1
	addi r14, r0, 283
	jal r0, .LBB7_194
.LBB7_207:
	jal r31, luaZ_fill
.LBB7_208:
	stw r11+0, r1
	addi r14, r0, 278
	jal r0, .LBB7_194
.LBB7_209:
	jal r31, luaZ_fill
.LBB7_210:
	stw r11+0, r1
	addi r3, r0, 46
	bne r1, r3, .LBB7_213
.LBB7_211:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB7_214
.LBB7_212:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB7_215
.LBB7_213:
	addi r14, r0, 279
	jal r0, .LBB7_194
.LBB7_214:
	jal r31, luaZ_fill
.LBB7_215:
	stw r11+0, r1
	addi r14, r0, 280
	jal r0, .LBB7_194
.LBB7_216:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lexerror
.LBB7_217:
	lui r4, %hi(.L.str.48)
	addi r4, r4, %lo(.L.str.48)
	jal r0, .LBB7_225
.LBB7_218:
	ldw r3, fp+-88
	beq r1, r3, .LBB7_224
.LBB7_219:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, save
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	bne r1, r19, .LBB7_222
.LBB7_220:
	jal r31, luaZ_fill
	jal r0, .LBB7_223
.LBB7_221:
	lui r4, %hi(.L.str.48)
	addi r4, r4, %lo(.L.str.48)
	addi r5, r0, 288
	add r3, r11, r0
	jal r31, lexerror
.LBB7_222:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
.LBB7_223:
	stw r11+0, r1
.LBB7_224:
	lui r4, %hi(.L.str.52)
	addi r4, r4, %lo(.L.str.52)
.LBB7_225:
	addi r5, r0, 292
	add r3, r11, r0
	jal r31, lexerror
.LBB7_226:
	beq r1, r25, .LBB7_240
.LBB7_227:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, save
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	bne r1, r19, .LBB7_238
.LBB7_228:
	jal r31, luaZ_fill
	jal r0, .LBB7_239
.LBB7_229:
	ldw r3, fp+-88
	beq r1, r3, .LBB7_243
.LBB7_230:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, save
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	bne r1, r19, .LBB7_241
.LBB7_231:
	jal r31, luaZ_fill
	jal r0, .LBB7_242
.LBB7_232:
	ldw r4, r11+0
	ldw r1, fp+-88
	beq r4, r1, .LBB7_246
.LBB7_233:
	add r3, r11, r0
	jal r31, save
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r4, r0, 0
	bne r1, r4, .LBB7_244
.LBB7_234:
	jal r31, luaZ_fill
	jal r0, .LBB7_245
.LBB7_235:
	ldw r3, fp+-88
	beq r1, r3, .LBB7_249
.LBB7_236:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, save
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r4, r0, 0
	bne r1, r4, .LBB7_247
.LBB7_237:
	jal r31, luaZ_fill
	jal r0, .LBB7_248
.LBB7_238:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
.LBB7_239:
	stw r11+0, r1
.LBB7_240:
	lui r4, %hi(.L.str.51)
	addi r4, r4, %lo(.L.str.51)
	jal r0, .LBB7_225
.LBB7_241:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
.LBB7_242:
	stw r11+0, r1
.LBB7_243:
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	jal r0, .LBB7_225
.LBB7_244:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
.LBB7_245:
	stw r11+0, r1
.LBB7_246:
	lui r4, %hi(.L.str.54)
	addi r4, r4, %lo(.L.str.54)
	jal r0, .LBB7_225
.LBB7_247:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
.LBB7_248:
	stw r11+0, r1
.LBB7_249:
	lui r4, %hi(.L.str.49)
	addi r4, r4, %lo(.L.str.49)
	jal r0, .LBB7_225
.Lfunc_end7:
	.size	llex, .Lfunc_end7-llex
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI7_0:
	.word	.LBB7_194
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_4
	.word	.LBB7_1
	.word	.LBB7_4
	.word	.LBB7_4
	.word	.LBB7_1
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_4
	.word	.LBB7_26
	.word	.LBB7_21
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_21
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_7
	.word	.LBB7_33
	.word	.LBB7_48
	.word	.LBB7_193
	.word	.LBB7_193
	.word	.LBB7_193
	.word	.LBB7_193
	.word	.LBB7_193
	.word	.LBB7_193
	.word	.LBB7_193
	.word	.LBB7_193
	.word	.LBB7_193
	.word	.LBB7_193
	.word	.LBB7_29
	.word	.LBB7_26
	.word	.LBB7_46
	.word	.LBB7_44
	.word	.LBB7_31
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_42
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_26
	.word	.LBB7_40
.LJTI7_1:
	.word	.LBB7_60
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_82
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_82
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_81
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_81
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_81
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_97
	.word	.LBB7_91
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_96
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_95
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_103
	.word	.LBB7_84
	.word	.LBB7_103
	.word	.LBB7_90
	.word	.LBB7_85
	.word	.LBB7_83
	.word	.LBB7_103
	.word	.LBB7_92
	.word	.LBB7_103
	.word	.LBB7_93
                                        # -- End function
	.text
	.hidden	luaX_lookahead                  # -- Begin function luaX_lookahead
	.globl	luaX_lookahead
	.p2align	2
	.type	luaX_lookahead,@function
luaX_lookahead:                         # @luaX_lookahead
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	addi r4, r3, 28
	jal r31, llex
	stw r11+24, r1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	luaX_lookahead, .Lfunc_end8-luaX_lookahead
                                        # -- End function
	.p2align	2                               # -- Begin function save
	.type	save,@function
save:                                   # @save
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
	ldw r13, r3+48
	ldw r1, r13+4
	addi r1, r1, 1
	ldw r5, r13+8
	bleu r1, r5, .LBB9_3
.LBB9_1:
	lui r1, 262144
	addi r1, r1, -1
	bgeu r5, r1, .LBB9_4
.LBB9_2:
	slli r12, r5, 1
	ldw r3, r3+40
	ldw r4, r13+0
	add r6, r12, r0
	jal r31, luaM_saferealloc_
	stw r13+0, r1
	stw r13+8, r12
.LBB9_3:
	ldw r1, r13+0
	ldw r3, r13+4
	addi r4, r3, 1
	stw r13+4, r4
	add r1, r1, r3
	stb r1+0, r11
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB9_4:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r5, r0, 0
	jal r31, lexerror
.Lfunc_end9:
	.size	save, .Lfunc_end9-save
                                        # -- End function
	.p2align	2                               # -- Begin function inclinenumber
	.type	inclinenumber,@function
inclinenumber:                          # @inclinenumber
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
	ldw r13, r3+0
	ldw r3, r3+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r12, r0, 0
	beq r1, r12, .LBB10_2
.LBB10_1:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB10_3
.LBB10_2:
	jal r31, luaZ_fill
.LBB10_3:
	stw r11+0, r1
	addi r3, r0, 13
	beq r1, r3, .LBB10_5
.LBB10_4:
	addi r3, r0, 10
	bne r1, r3, .LBB10_10
.LBB10_5:
	beq r1, r13, .LBB10_10
.LBB10_6:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r12, .LBB10_8
.LBB10_7:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB10_9
.LBB10_8:
	jal r31, luaZ_fill
.LBB10_9:
	stw r11+0, r1
.LBB10_10:
	ldw r1, r11+4
	addi r1, r1, 1
	stw r11+4, r1
	lui r3, 524288
	addi r3, r3, -1
	beq r1, r3, .LBB10_12
.LBB10_11:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB10_12:
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lexerror
.Lfunc_end10:
	.size	inclinenumber, .Lfunc_end10-inclinenumber
                                        # -- End function
	.p2align	2                               # -- Begin function skip_sep
	.type	skip_sep,@function
skip_sep:                               # @skip_sep
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
	add r11, r3, r0
	ldw r13, r3+0
	ldw r14, r3+48
	ldw r1, r14+4
	addi r1, r1, 1
	ldw r5, r14+8
	bleu r1, r5, .LBB11_3
.LBB11_1:
	lui r1, 262144
	addi r1, r1, -1
	bgeu r5, r1, .LBB11_16
.LBB11_2:
	slli r12, r5, 1
	ldw r3, r11+40
	ldw r4, r14+0
	add r6, r12, r0
	jal r31, luaM_saferealloc_
	stw r14+0, r1
	stw r14+8, r12
.LBB11_3:
	ldw r1, r14+0
	ldw r3, r14+4
	addi r4, r3, 1
	stw r14+4, r4
	add r1, r1, r3
	stb r1+0, r13
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r14, r0, 0
	beq r1, r14, .LBB11_5
.LBB11_4:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB11_6
.LBB11_5:
	jal r31, luaZ_fill
.LBB11_6:
	stw r11+0, r1
	addi r15, r0, 61
	add r16, r14, r0
	bne r1, r15, .LBB11_15
.LBB11_7:
	addi r17, r0, 0
	lui r1, 262144
	addi r18, r1, -1
	add r16, r17, r0
	jal r0, .LBB11_10
.LBB11_8:
	jal r31, luaZ_fill
.LBB11_9:
	stw r11+0, r1
	addi r16, r16, 1
	bne r1, r15, .LBB11_15
.LBB11_10:
	ldw r19, r11+48
	ldw r1, r19+4
	addi r1, r1, 1
	ldw r5, r19+8
	bleu r1, r5, .LBB11_13
.LBB11_11:
	bgeu r5, r18, .LBB11_16
.LBB11_12:
	slli r12, r5, 1
	ldw r3, r11+40
	ldw r4, r19+0
	add r6, r12, r0
	jal r31, luaM_saferealloc_
	stw r19+0, r1
	stw r19+8, r12
.LBB11_13:
	ldw r1, r19+0
	ldw r3, r19+4
	addi r4, r3, 1
	stw r19+4, r4
	add r1, r1, r3
	stb r1+0, r15
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r17, .LBB11_8
.LBB11_14:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB11_9
.LBB11_15:
	seq r1, r1, r13
	addi r3, r16, 2
	seq r4, r16, r14
	sub r1, r14, r1
	xor r3, r3, r4
	and r1, r3, r1
	xor r1, r4, r1
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
.LBB11_16:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lexerror
.Lfunc_end11:
	.size	skip_sep, .Lfunc_end11-skip_sep
                                        # -- End function
	.p2align	2                               # -- Begin function read_long_string
	.type	read_long_string,@function
read_long_string:                       # @read_long_string
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
	add r13, r5, r0
	add r12, r4, r0
	add r11, r3, r0
	ldw r14, r3+4
	ldw r16, r3+0
	ldw r17, r3+48
	ldw r1, r17+4
	addi r1, r1, 1
	ldw r5, r17+8
	lui r18, 262144
	bleu r1, r5, .LBB12_3
.LBB12_1:
	addi r1, r18, -1
	bgeu r5, r1, .LBB12_37
.LBB12_2:
	slli r15, r5, 1
	ldw r3, r11+40
	ldw r4, r17+0
	add r6, r15, r0
	jal r31, luaM_saferealloc_
	stw r17+0, r1
	stw r17+8, r15
.LBB12_3:
	ldw r1, r17+0
	ldw r3, r17+4
	addi r4, r3, 1
	stw r17+4, r4
	add r1, r1, r3
	stb r1+0, r16
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r16, r0, 0
	beq r1, r16, .LBB12_5
.LBB12_4:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB12_6
.LBB12_5:
	jal r31, luaZ_fill
.LBB12_6:
	stw r11+0, r1
	addi r17, r0, 13
	beq r1, r17, .LBB12_8
.LBB12_7:
	addi r3, r0, 10
	bne r1, r3, .LBB12_9
.LBB12_8:
	add r3, r11, r0
	jal r31, inclinenumber
.LBB12_9:
	addi r19, r0, 12
	addi r20, r0, 10
	addi r21, r0, -1
	addi r18, r18, -1
	addi r22, r0, 93
	jal r0, .LBB12_12
.LBB12_10:
	slli r15, r5, 1
	ldw r3, r11+40
	ldw r4, r23+0
	add r6, r15, r0
	jal r31, luaM_saferealloc_
	stw r23+0, r1
	stw r23+8, r15
.LBB12_11:
	ldw r1, r23+0
	ldw r3, r23+4
	addi r4, r3, 1
	stw r23+4, r4
	add r1, r1, r3
	stb r1+0, r20
	add r3, r11, r0
	jal r31, inclinenumber
	beq r12, r16, .LBB12_19
.LBB12_12:
	ldw r23, r11+0
	ble r23, r19, .LBB12_16
.LBB12_13:
	beq r23, r17, .LBB12_17
.LBB12_14:
	bne r23, r22, .LBB12_21
.LBB12_15:
	add r3, r11, r0
	jal r31, skip_sep
	bne r1, r13, .LBB12_12
	jal r0, .LBB12_29
.LBB12_16:
	bne r23, r20, .LBB12_20
.LBB12_17:
	ldw r23, r11+48
	ldw r1, r23+4
	addi r1, r1, 1
	ldw r5, r23+8
	bleu r1, r5, .LBB12_11
.LBB12_18:
	bltu r5, r18, .LBB12_10
	jal r0, .LBB12_37
.LBB12_19:
	ldw r1, r11+48
	stw r1+4, r16
	jal r0, .LBB12_12
.LBB12_20:
	beq r23, r21, .LBB12_38
.LBB12_21:
	beq r12, r16, .LBB12_26
.LBB12_22:
	ldw r24, r11+48
	ldw r1, r24+4
	addi r1, r1, 1
	ldw r5, r24+8
	bleu r1, r5, .LBB12_25
.LBB12_23:
	bgeu r5, r18, .LBB12_37
.LBB12_24:
	slli r15, r5, 1
	ldw r3, r11+40
	ldw r4, r24+0
	add r6, r15, r0
	jal r31, luaM_saferealloc_
	stw r24+0, r1
	stw r24+8, r15
.LBB12_25:
	ldw r1, r24+0
	ldw r3, r24+4
	addi r4, r3, 1
	stw r24+4, r4
	add r1, r1, r3
	stb r1+0, r23
.LBB12_26:
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r16, .LBB12_28
.LBB12_27:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	stw r11+0, r1
	jal r0, .LBB12_12
.LBB12_28:
	jal r31, luaZ_fill
	stw r11+0, r1
	jal r0, .LBB12_12
.LBB12_29:
	ldw r15, r11+0
	ldw r17, r11+48
	ldw r1, r17+4
	addi r1, r1, 1
	ldw r5, r17+8
	bleu r1, r5, .LBB12_32
.LBB12_30:
	bgeu r5, r18, .LBB12_37
.LBB12_31:
	slli r14, r5, 1
	ldw r3, r11+40
	ldw r4, r17+0
	add r6, r14, r0
	jal r31, luaM_saferealloc_
	stw r17+0, r1
	stw r17+8, r14
.LBB12_32:
	ldw r1, r17+0
	ldw r3, r17+4
	addi r4, r3, 1
	stw r17+4, r4
	add r1, r1, r3
	stb r1+0, r15
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r16, .LBB12_36
.LBB12_33:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	stw r11+0, r1
	beq r12, r16, .LBB12_35
.LBB12_34:
	ldw r1, r11+48
	ldw r3, r1+0
	add r4, r3, r13
	ldw r1, r1+4
	slli r3, r13, 1
	sub r5, r1, r3
	add r3, r11, r0
	jal r31, luaX_newstring
	stw r12+0, r1
.LBB12_35:
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
.LBB12_36:
	jal r31, luaZ_fill
	stw r11+0, r1
	bne r12, r16, .LBB12_34
	jal r0, .LBB12_35
.LBB12_37:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lexerror
.LBB12_38:
	seq r1, r12, r16
	lui r3, %hi(.L.str.45)
	addi r3, r3, %lo(.L.str.45)
	lui r4, %hi(.L.str.46)
	addi r4, r4, %lo(.L.str.46)
	xor r4, r4, r3
	sub r1, r16, r1
	and r1, r4, r1
	xor r5, r1, r3
	ldw r3, r11+40
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
	add r6, r14, r0
	jal r31, luaO_pushfstring
	addi r5, r0, 288
	add r3, r11, r0
	add r4, r1, r0
	jal r31, lexerror
.Lfunc_end12:
	.size	read_long_string, .Lfunc_end12-read_long_string
                                        # -- End function
	.p2align	2                               # -- Begin function check_next1
	.type	check_next1,@function
check_next1:                            # @check_next1
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+0
	bne r1, r4, .LBB13_3
.LBB13_1:
	ldw r1, r3+44
	ldw r4, r1+0
	addi r5, r4, -1
	stw r1+0, r5
	addi r5, r0, 0
	beq r4, r5, .LBB13_4
.LBB13_2:
	ldw r4, r1+4
	addi r5, r4, 1
	stw r1+4, r5
	ldbu r1, r4+0
	jal r0, .LBB13_5
.LBB13_3:
	addi r1, r0, 0
	jal r0, .LBB13_6
.LBB13_4:
	add r11, r3, r0
	add r3, r1, r0
	jal r31, luaZ_fill
	add r3, r11, r0
.LBB13_5:
	stw r3+0, r1
	addi r1, r0, 1
.LBB13_6:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end13:
	.size	check_next1, .Lfunc_end13-check_next1
                                        # -- End function
	.p2align	2                               # -- Begin function read_numeral
	.type	read_numeral,@function
read_numeral:                           # @read_numeral
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
	stw fp+-48, lr
	add r12, r4, r0
	add r11, r3, r0
	ldw r15, r3+0
	ldw r16, r3+48
	ldw r1, r16+4
	addi r1, r1, 1
	ldw r5, r16+8
	lui r14, 262144
	bleu r1, r5, .LBB14_3
.LBB14_1:
	addi r1, r14, -1
	bgeu r5, r1, .LBB14_36
.LBB14_2:
	slli r13, r5, 1
	ldw r3, r11+40
	ldw r4, r16+0
	add r6, r13, r0
	jal r31, luaM_saferealloc_
	stw r16+0, r1
	stw r16+8, r13
.LBB14_3:
	ldw r1, r16+0
	ldw r3, r16+4
	addi r4, r3, 1
	stw r16+4, r4
	add r1, r1, r3
	stb r1+0, r15
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r16, r0, 0
	beq r1, r16, .LBB14_5
.LBB14_4:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB14_6
.LBB14_5:
	jal r31, luaZ_fill
.LBB14_6:
	stw r11+0, r1
	addi r1, r0, 48
	bne r15, r1, .LBB14_8
.LBB14_7:
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	add r3, r11, r0
	jal r31, check_next2
	seq r1, r1, r16
	lui r3, %hi(.L.str.57)
	addi r3, r3, %lo(.L.str.57)
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	xor r4, r4, r3
	sub r1, r16, r1
	and r1, r4, r1
	xor r13, r1, r3
	jal r0, .LBB14_9
.LBB14_8:
	lui r13, %hi(.L.str.55)
	addi r13, r13, %lo(.L.str.55)
.LBB14_9:
	lui r18, %hi(luai_ctype_+1)
	addi r18, r18, %lo(luai_ctype_+1)
	addi r19, r0, 46
	addi r17, r14, -1
	lui r14, %hi(.L.str.58)
	addi r14, r14, %lo(.L.str.58)
	jal r0, .LBB14_11
.LBB14_10:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, check_next2
.LBB14_11:
	add r3, r11, r0
	add r4, r13, r0
	jal r31, check_next2
	bne r1, r16, .LBB14_10
.LBB14_12:
	ldw r20, r11+0
	beq r20, r19, .LBB14_14
.LBB14_13:
	add r1, r20, r18
	ldbu r1, r1+0
	andi r3, r1, 16
	beq r3, r16, .LBB14_20
.LBB14_14:
	ldw r21, r11+48
	ldw r1, r21+4
	addi r1, r1, 1
	ldw r5, r21+8
	bleu r1, r5, .LBB14_17
.LBB14_15:
	bgeu r5, r17, .LBB14_36
.LBB14_16:
	slli r15, r5, 1
	ldw r3, r11+40
	ldw r4, r21+0
	add r6, r15, r0
	jal r31, luaM_saferealloc_
	stw r21+0, r1
	stw r21+8, r15
.LBB14_17:
	ldw r1, r21+0
	ldw r3, r21+4
	addi r4, r3, 1
	stw r21+4, r4
	add r1, r1, r3
	stb r1+0, r20
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r16, .LBB14_19
.LBB14_18:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	stw r11+0, r1
	jal r0, .LBB14_11
.LBB14_19:
	jal r31, luaZ_fill
	stw r11+0, r1
	jal r0, .LBB14_11
.LBB14_20:
	andi r1, r1, 1
	beq r1, r16, .LBB14_28
.LBB14_21:
	ldw r14, r11+48
	ldw r1, r14+4
	addi r1, r1, 1
	ldw r5, r14+8
	bleu r1, r5, .LBB14_24
.LBB14_22:
	bgeu r5, r17, .LBB14_36
.LBB14_23:
	slli r13, r5, 1
	ldw r3, r11+40
	ldw r4, r14+0
	add r6, r13, r0
	jal r31, luaM_saferealloc_
	stw r14+0, r1
	stw r14+8, r13
.LBB14_24:
	ldw r1, r14+0
	ldw r3, r14+4
	addi r4, r3, 1
	stw r14+4, r4
	add r1, r1, r3
	stb r1+0, r20
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	beq r1, r16, .LBB14_26
.LBB14_25:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB14_27
.LBB14_26:
	jal r31, luaZ_fill
.LBB14_27:
	stw r11+0, r1
.LBB14_28:
	ldw r14, r11+48
	ldw r1, r14+4
	addi r1, r1, 1
	ldw r5, r14+8
	bleu r1, r5, .LBB14_31
.LBB14_29:
	bgeu r5, r17, .LBB14_36
.LBB14_30:
	slli r13, r5, 1
	ldw r3, r11+40
	ldw r4, r14+0
	add r6, r13, r0
	jal r31, luaM_saferealloc_
	stw r14+0, r1
	stw r14+8, r13
.LBB14_31:
	ldw r1, r14+0
	ldw r3, r14+4
	addi r4, r3, 1
	stw r14+4, r4
	add r1, r1, r3
	stb r1+0, r16
	ldw r1, r11+48
	ldw r3, r1+0
	addi r13, fp, -60
	add r4, r13, r0
	jal r31, luaO_str2num
	beq r1, r16, .LBB14_37
.LBB14_32:
	ldbu r1, r13+8
	addi r3, r0, 3
	bne r1, r3, .LBB14_34
.LBB14_33:
	ldw r1, r13+0
	stw r12+0, r1
	addi r1, r0, 290
	jal r0, .LBB14_35
.LBB14_34:
	ldw r1, r13+4
	ldw r3, r13+0
	stw r12+4, r1
	stw r12+0, r3
	addi r1, r0, 289
.LBB14_35:
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
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB14_36:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lexerror
.LBB14_37:
	lui r4, %hi(.L.str.59)
	addi r4, r4, %lo(.L.str.59)
	addi r5, r0, 289
	add r3, r11, r0
	jal r31, lexerror
.Lfunc_end14:
	.size	read_numeral, .Lfunc_end14-read_numeral
                                        # -- End function
	.p2align	2                               # -- Begin function gethexa
	.type	gethexa,@function
gethexa:                                # @gethexa
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
	ldw r13, r3+0
	ldw r14, r3+48
	ldw r1, r14+4
	addi r1, r1, 1
	ldw r5, r14+8
	bleu r1, r5, .LBB15_3
.LBB15_1:
	lui r1, 262144
	addi r1, r1, -1
	bgeu r5, r1, .LBB15_11
.LBB15_2:
	slli r12, r5, 1
	ldw r3, r11+40
	ldw r4, r14+0
	add r6, r12, r0
	jal r31, luaM_saferealloc_
	stw r14+0, r1
	stw r14+8, r12
.LBB15_3:
	ldw r1, r14+0
	ldw r3, r14+4
	addi r4, r3, 1
	stw r14+4, r4
	add r1, r1, r3
	stb r1+0, r13
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r12, r0, 0
	beq r1, r12, .LBB15_5
.LBB15_4:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB15_6
.LBB15_5:
	jal r31, luaZ_fill
.LBB15_6:
	stw r11+0, r1
	lui r3, %hi(luai_ctype_+1)
	addi r3, r3, %lo(luai_ctype_+1)
	add r3, r1, r3
	ldbu r3, r3+0
	andi r3, r3, 16
	beq r3, r12, .LBB15_8
.LBB15_7:
	add r3, r1, r0
	jal r31, luaO_hexavalue
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB15_8:
	addi r3, r0, -1
	beq r1, r3, .LBB15_14
.LBB15_9:
	add r3, r11, r0
	add r4, r1, r0
	jal r31, save
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	bne r1, r12, .LBB15_12
.LBB15_10:
	jal r31, luaZ_fill
	jal r0, .LBB15_13
.LBB15_11:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lexerror
.LBB15_12:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
.LBB15_13:
	stw r11+0, r1
.LBB15_14:
	lui r4, %hi(.L.str.50)
	addi r4, r4, %lo(.L.str.50)
	addi r5, r0, 292
	add r3, r11, r0
	jal r31, lexerror
.Lfunc_end15:
	.size	gethexa, .Lfunc_end15-gethexa
                                        # -- End function
	.p2align	2                               # -- Begin function check_next2
	.type	check_next2,@function
check_next2:                            # @check_next2
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
	ldw r13, r3+0
	ldb r1, r4+0
	beq r13, r1, .LBB16_2
.LBB16_1:
	ldb r1, r4+1
	bne r13, r1, .LBB16_9
.LBB16_2:
	ldw r14, r11+48
	ldw r1, r14+4
	addi r1, r1, 1
	ldw r5, r14+8
	bleu r1, r5, .LBB16_5
.LBB16_3:
	lui r1, 262144
	addi r1, r1, -1
	bgeu r5, r1, .LBB16_11
.LBB16_4:
	slli r12, r5, 1
	ldw r3, r11+40
	ldw r4, r14+0
	add r6, r12, r0
	jal r31, luaM_saferealloc_
	stw r14+0, r1
	stw r14+8, r12
.LBB16_5:
	ldw r1, r14+0
	ldw r3, r14+4
	addi r4, r3, 1
	stw r14+4, r4
	add r1, r1, r3
	stb r1+0, r13
	ldw r3, r11+44
	ldw r1, r3+0
	addi r4, r1, -1
	stw r3+0, r4
	addi r4, r0, 0
	beq r1, r4, .LBB16_7
.LBB16_6:
	ldw r1, r3+4
	addi r4, r1, 1
	stw r3+4, r4
	ldbu r1, r1+0
	jal r0, .LBB16_8
.LBB16_7:
	jal r31, luaZ_fill
.LBB16_8:
	stw r11+0, r1
	addi r1, r0, 1
	jal r0, .LBB16_10
.LBB16_9:
	addi r1, r0, 0
.LBB16_10:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB16_11:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lexerror
.Lfunc_end16:
	.size	check_next2, .Lfunc_end16-check_next2
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"_ENV"
	.size	.L.str, 5

	.type	luaX_tokens,@object             # @luaX_tokens
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
luaX_tokens:
	.word	.L.str.4
	.word	.L.str.5
	.word	.L.str.6
	.word	.L.str.7
	.word	.L.str.8
	.word	.L.str.9
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
	.word	.L.str.35
	.word	.L.str.36
	.word	.L.str.37
	.word	.L.str.38
	.word	.L.str.39
	.word	.L.str.40
	.size	luaX_tokens, 148

	.hidden	luai_ctype_
	.type	.L.str.1,@object                # @.str.1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.1:
	.asciz	"'%c'"
	.size	.L.str.1, 5

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"'<\\%d>'"
	.size	.L.str.2, 8

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"'%s'"
	.size	.L.str.3, 5

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"and"
	.size	.L.str.4, 4

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"break"
	.size	.L.str.5, 6

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"do"
	.size	.L.str.6, 3

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"else"
	.size	.L.str.7, 5

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"elseif"
	.size	.L.str.8, 7

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"end"
	.size	.L.str.9, 4

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"false"
	.size	.L.str.10, 6

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"for"
	.size	.L.str.11, 4

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"function"
	.size	.L.str.12, 9

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"goto"
	.size	.L.str.13, 5

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"if"
	.size	.L.str.14, 3

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"in"
	.size	.L.str.15, 3

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"local"
	.size	.L.str.16, 6

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"nil"
	.size	.L.str.17, 4

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"not"
	.size	.L.str.18, 4

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"or"
	.size	.L.str.19, 3

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"repeat"
	.size	.L.str.20, 7

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"return"
	.size	.L.str.21, 7

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"then"
	.size	.L.str.22, 5

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"true"
	.size	.L.str.23, 5

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"until"
	.size	.L.str.24, 6

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"while"
	.size	.L.str.25, 6

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"//"
	.size	.L.str.26, 3

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	".."
	.size	.L.str.27, 3

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"..."
	.size	.L.str.28, 4

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"=="
	.size	.L.str.29, 3

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	">="
	.size	.L.str.30, 3

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"<="
	.size	.L.str.31, 3

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"~="
	.size	.L.str.32, 3

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"<<"
	.size	.L.str.33, 3

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	">>"
	.size	.L.str.34, 3

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"::"
	.size	.L.str.35, 3

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"<eof>"
	.size	.L.str.36, 6

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"<number>"
	.size	.L.str.37, 9

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"<integer>"
	.size	.L.str.38, 10

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"<name>"
	.size	.L.str.39, 7

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"<string>"
	.size	.L.str.40, 9

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"%s near %s"
	.size	.L.str.41, 11

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"lexical element too long"
	.size	.L.str.42, 25

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"invalid long string delimiter"
	.size	.L.str.43, 30

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"chunk has too many lines"
	.size	.L.str.44, 25

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"string"
	.size	.L.str.45, 7

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"comment"
	.size	.L.str.46, 8

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"unfinished long %s (starting at line %d)"
	.size	.L.str.47, 41

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"unfinished string"
	.size	.L.str.48, 18

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"invalid escape sequence"
	.size	.L.str.49, 24

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"hexadecimal digit expected"
	.size	.L.str.50, 27

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"missing '{'"
	.size	.L.str.51, 12

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"UTF-8 value too large"
	.size	.L.str.52, 22

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"missing '}'"
	.size	.L.str.53, 12

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"decimal escape too large"
	.size	.L.str.54, 25

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"Ee"
	.size	.L.str.55, 3

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"xX"
	.size	.L.str.56, 3

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"Pp"
	.size	.L.str.57, 3

	.type	.L.str.58,@object               # @.str.58
.L.str.58:
	.asciz	"-+"
	.size	.L.str.58, 3

	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"malformed number"
	.size	.L.str.59, 17

	.hidden	luaS_newlstr
	.hidden	luaC_fix
	.hidden	luaS_new
	.hidden	luaO_pushfstring
	.hidden	luaH_getstr
	.hidden	luaH_finishset
	.hidden	luaC_step
	.hidden	luaM_saferealloc_
	.hidden	luaG_addinfo
	.hidden	luaD_throw
	.hidden	luaZ_fill
	.hidden	luaO_hexavalue
	.hidden	luaO_utf8esc
	.hidden	luaO_str2num
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
