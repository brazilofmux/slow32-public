	.file	"eval.c"
	.text
	.globl	eval_init                       # -- Begin function eval_init
	.p2align	2
	.type	eval_init,@function
eval_init:                              # @eval_init
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	jal r31, symbol_intern
	lui r3, %hi(sym_quote)
	addi r3, r3, %lo(sym_quote)
	stw r3+0, r1
	lui r3, %hi(.L.str.1)
	addi r3, r3, %lo(.L.str.1)
	jal r31, symbol_intern
	lui r3, %hi(sym_if)
	addi r3, r3, %lo(sym_if)
	stw r3+0, r1
	lui r3, %hi(.L.str.2)
	addi r3, r3, %lo(.L.str.2)
	jal r31, symbol_intern
	lui r3, %hi(sym_cond)
	addi r3, r3, %lo(sym_cond)
	stw r3+0, r1
	lui r3, %hi(.L.str.3)
	addi r3, r3, %lo(.L.str.3)
	jal r31, symbol_intern
	lui r3, %hi(sym_define)
	addi r3, r3, %lo(sym_define)
	stw r3+0, r1
	lui r3, %hi(.L.str.4)
	addi r3, r3, %lo(.L.str.4)
	jal r31, symbol_intern
	lui r3, %hi(sym_lambda)
	addi r3, r3, %lo(sym_lambda)
	stw r3+0, r1
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	jal r31, symbol_intern
	lui r3, %hi(sym_begin)
	addi r3, r3, %lo(sym_begin)
	stw r3+0, r1
	lui r3, %hi(.L.str.6)
	addi r3, r3, %lo(.L.str.6)
	jal r31, symbol_intern
	lui r3, %hi(sym_let)
	addi r3, r3, %lo(sym_let)
	stw r3+0, r1
	lui r3, %hi(.L.str.7)
	addi r3, r3, %lo(.L.str.7)
	jal r31, symbol_intern
	lui r3, %hi(sym_letstar)
	addi r3, r3, %lo(sym_letstar)
	stw r3+0, r1
	lui r3, %hi(.L.str.8)
	addi r3, r3, %lo(.L.str.8)
	jal r31, symbol_intern
	lui r3, %hi(sym_and)
	addi r3, r3, %lo(sym_and)
	stw r3+0, r1
	lui r3, %hi(.L.str.9)
	addi r3, r3, %lo(.L.str.9)
	jal r31, symbol_intern
	lui r3, %hi(sym_or)
	addi r3, r3, %lo(sym_or)
	stw r3+0, r1
	lui r3, %hi(.L.str.10)
	addi r3, r3, %lo(.L.str.10)
	jal r31, symbol_intern
	lui r3, %hi(sym_set)
	addi r3, r3, %lo(sym_set)
	stw r3+0, r1
	lui r3, %hi(.L.str.11)
	addi r3, r3, %lo(.L.str.11)
	jal r31, symbol_intern
	lui r3, %hi(sym_else)
	addi r3, r3, %lo(sym_else)
	stw r3+0, r1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	eval_init, .Lfunc_end0-eval_init
                                        # -- End function
	.globl	eval                            # -- Begin function eval
	.p2align	2
	.type	eval,@function
eval:                                   # @eval
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
	addi r14, fp, -80
	stw r14+0, r4
	lui r16, %hi(g_error)
	addi r16, r16, %lo(g_error)
	ldw r1, r16+0
	addi r12, r0, 0
	beq r1, r12, .LBB1_2
.LBB1_1:
	add r1, r12, r0
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
.LBB1_2:
	add r11, r3, r0
	addi r21, r0, 0
	addi r20, fp, -84
	addi r22, r0, 1
	lui r1, %hi(sym_quote)
	addi r1, r1, %lo(sym_quote)
	stw fp+-108, r1
	lui r1, %hi(sym_if)
	addi r1, r1, %lo(sym_if)
	stw fp+-112, r1
	lui r17, %hi(root_sp)
	addi r17, r17, %lo(root_sp)
	lui r18, %hi(root_stack)
	addi r18, r18, %lo(root_stack)
	lui r1, %hi(sym_cond)
	addi r1, r1, %lo(sym_cond)
	stw fp+-116, r1
	addi r15, fp, -88
	addi r19, fp, -92
	lui r26, %hi(sym_else)
	addi r26, r26, %lo(sym_else)
	addi r27, r0, 2
                                        # implicit-def: $r24
.LBB1_3:
	beq r11, r21, .LBB1_98
.LBB1_4:
	andi r1, r11, 1
	bne r1, r21, .LBB1_98
.LBB1_5:
	ldw r1, r11+0
	bne r1, r21, .LBB1_96
.LBB1_6:
	add r28, r24, r0
	ldw r3, r11+12
	ldw r1, r11+16
	stw r20+0, r1
	beq r3, r21, .LBB1_86
.LBB1_7:
	andi r4, r3, 1
	bne r4, r21, .LBB1_86
.LBB1_8:
	ldw r4, r3+0
	bne r4, r22, .LBB1_86
.LBB1_9:
	ldw r4, fp+-108
	ldw r4, r4+0
	beq r3, r4, .LBB1_105
.LBB1_10:
	ldw r4, fp+-112
	ldw r4, r4+0
	bne r3, r4, .LBB1_13
.LBB1_11:
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r5, r3, 2
	add r5, r5, r18
	stw r5+0, r20
	addi r3, r3, 2
	stw r17+0, r3
	slli r3, r4, 2
	add r3, r3, r18
	stw r3+0, r14
	ldw r3, r1+12
	ldw r4, r14+0
	jal r31, eval
	ldw r3, r17+0
	addi r3, r3, -2
	stw r17+0, r3
	ldw r3, r16+0
	beq r3, r21, .LBB1_43
.LBB1_12:
	add r1, r22, r0
	add r24, r21, r0
	jal r0, .LBB1_46
.LBB1_13:
	ldw r4, fp+-116
	ldw r4, r4+0
	bne r3, r4, .LBB1_34
.LBB1_14:
	stw r15+0, r1
	add r24, r28, r0
	jal r0, .LBB1_18
.LBB1_15:
	ldw r1, r17+0
	addi r4, r1, 1
	stw r17+0, r4
	slli r5, r1, 2
	add r5, r5, r18
	stw r5+0, r15
	addi r1, r1, 2
	stw r17+0, r1
	slli r1, r4, 2
	add r1, r1, r18
	stw r1+0, r14
	ldw r4, r14+0
	jal r31, eval
	ldw r3, r17+0
	addi r3, r3, -2
	stw r17+0, r3
	ldw r3, r16+0
	addi r12, r0, 0
	beq r3, r12, .LBB1_25
.LBB1_16:
	add r3, r22, r0
	add r24, r12, r0
.LBB1_17:
	beq r12, r21, .LBB1_48
.LBB1_18:
	ldw r1, r15+0
	beq r1, r21, .LBB1_47
.LBB1_19:
	ldw r1, r1+12
	ldw r3, r1+12
	ldw r1, r1+16
	stw r19+0, r1
	ldw r1, r26+0
	bne r3, r1, .LBB1_15
.LBB1_20:
	ldw r1, r19+0
	ldw r3, r1+16
	beq r3, r21, .LBB1_23
.LBB1_21:
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r5, r3, 2
	add r5, r5, r18
	stw r5+0, r19
	addi r3, r3, 2
	stw r17+0, r3
	slli r3, r4, 2
	add r3, r3, r18
	stw r3+0, r14
	ldw r3, r1+12
	ldw r4, r14+0
	jal r31, eval
	ldw r1, r17+0
	addi r1, r1, -2
	stw r17+0, r1
	ldw r1, r16+0
	bne r1, r21, .LBB1_31
.LBB1_22:
	ldw r1, r19+0
	ldw r1, r1+16
	stw r19+0, r1
	ldw r3, r1+16
	addi r4, r0, 0
	bne r3, r4, .LBB1_21
.LBB1_23:
	ldw r11, r1+12
	add r12, r21, r0
.LBB1_24:
	add r3, r27, r0
	jal r0, .LBB1_17
.LBB1_25:
	addi r12, r0, 0
	beq r1, r12, .LBB1_32
.LBB1_26:
	ldw r3, r19+0
	beq r3, r12, .LBB1_33
.LBB1_27:
	ldw r1, r19+0
	ldw r3, r1+16
	addi r12, r0, 0
	beq r3, r12, .LBB1_30
.LBB1_28:
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r5, r3, 2
	add r5, r5, r18
	stw r5+0, r19
	addi r3, r3, 2
	stw r17+0, r3
	slli r3, r4, 2
	add r3, r3, r18
	stw r3+0, r14
	ldw r3, r1+12
	ldw r4, r14+0
	jal r31, eval
	ldw r1, r17+0
	addi r1, r1, -2
	stw r17+0, r1
	ldw r1, r16+0
	bne r1, r12, .LBB1_16
.LBB1_29:
	ldw r1, r19+0
	ldw r1, r1+16
	stw r19+0, r1
	ldw r3, r1+16
	addi r4, r0, 0
	bne r3, r4, .LBB1_28
.LBB1_30:
	ldw r11, r1+12
	addi r12, r0, 0
	jal r0, .LBB1_24
.LBB1_31:
	add r12, r21, r0
	add r3, r22, r0
	add r24, r21, r0
	jal r0, .LBB1_17
.LBB1_32:
	ldw r1, r15+0
	ldw r1, r1+16
	stw r15+0, r1
	addi r3, r0, 0
	addi r12, r0, 1
	jal r0, .LBB1_17
.LBB1_33:
	add r3, r22, r0
	add r24, r1, r0
	jal r0, .LBB1_17
.LBB1_34:
	lui r4, %hi(sym_define)
	addi r4, r4, %lo(sym_define)
	ldw r4, r4+0
	beq r3, r4, .LBB1_108
.LBB1_35:
	lui r4, %hi(sym_lambda)
	addi r4, r4, %lo(sym_lambda)
	ldw r4, r4+0
	beq r3, r4, .LBB1_113
.LBB1_36:
	lui r4, %hi(sym_begin)
	addi r4, r4, %lo(sym_begin)
	ldw r4, r4+0
	bne r3, r4, .LBB1_49
.LBB1_37:
	addi r12, r0, 0
	beq r1, r12, .LBB1_1
.LBB1_38:
	ldw r1, r20+0
	ldw r3, r1+16
	addi r12, r0, 0
	beq r3, r12, .LBB1_41
.LBB1_39:
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r5, r3, 2
	add r5, r5, r18
	stw r5+0, r20
	addi r3, r3, 2
	stw r17+0, r3
	slli r3, r4, 2
	add r3, r3, r18
	stw r3+0, r14
	ldw r3, r1+12
	ldw r4, r14+0
	jal r31, eval
	ldw r1, r17+0
	addi r1, r1, -2
	stw r17+0, r1
	ldw r1, r16+0
	bne r1, r12, .LBB1_1
.LBB1_40:
	ldw r1, r20+0
	ldw r1, r1+16
	stw r20+0, r1
	ldw r3, r1+16
	addi r4, r0, 0
	bne r3, r4, .LBB1_39
.LBB1_41:
	add r24, r28, r0
.LBB1_42:
	ldw r11, r1+12
	jal r0, .LBB1_95
.LBB1_43:
	ldw r3, r20+0
	ldw r3, r3+16
	stw r20+0, r3
	bne r1, r21, .LBB1_45
.LBB1_44:
	ldw r3, r3+16
	stw r20+0, r3
	add r1, r22, r0
	add r24, r21, r0
	beq r3, r21, .LBB1_46
.LBB1_45:
	ldw r11, r3+12
	add r1, r21, r0
	add r24, r28, r0
.LBB1_46:
	add r12, r24, r0
	beq r1, r21, .LBB1_95
	jal r0, .LBB1_1
.LBB1_47:
	add r3, r22, r0
	add r24, r21, r0
.LBB1_48:
	add r12, r24, r0
	bne r3, r22, .LBB1_95
	jal r0, .LBB1_1
.LBB1_49:
	lui r4, %hi(sym_let)
	addi r4, r4, %lo(sym_let)
	ldw r4, r4+0
	bne r3, r4, .LBB1_55
.LBB1_50:
	ldw r3, r1+12
	stw r15+0, r3
	ldw r1, r1+16
	stw r19+0, r1
	ldw r1, r17+0
	addi r3, r1, 1
	stw r17+0, r3
	slli r4, r1, 2
	add r4, r4, r18
	stw r4+0, r15
	addi r4, r1, 2
	stw r17+0, r4
	slli r3, r3, 2
	add r3, r3, r18
	stw r3+0, r14
	addi r1, r1, 3
	stw r17+0, r1
	slli r1, r4, 2
	add r1, r1, r18
	stw r1+0, r19
	ldw r3, r14+0
	jal r31, env_create
	addi r12, fp, -96
	stw r12+0, r1
	ldw r1, r17+0
	addi r3, r1, 1
	stw r17+0, r3
	slli r1, r1, 2
	add r1, r1, r18
	stw r1+0, r12
	add r24, r28, r0
	jal r0, .LBB1_52
.LBB1_51:
	add r24, r3, r0
	bne r25, r13, .LBB1_61
.LBB1_52:
	ldw r1, r15+0
	addi r13, r0, 0
	beq r1, r13, .LBB1_62
.LBB1_53:
	ldw r1, r1+12
	ldw r3, r1+12
	addi r23, fp, -100
	stw r23+0, r3
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r3, r3, 2
	add r3, r3, r18
	stw r3+0, r23
	ldw r1, r1+16
	ldw r3, r1+12
	ldw r4, r14+0
	jal r31, eval
	ldw r3, r17+0
	addi r3, r3, -1
	stw r17+0, r3
	ldw r25, r16+0
	add r3, r13, r0
	bne r25, r13, .LBB1_51
.LBB1_54:
	ldw r3, r12+0
	ldw r4, r23+0
	add r5, r1, r0
	jal r31, env_define
	ldw r1, r15+0
	ldw r1, r1+16
	stw r15+0, r1
	add r3, r24, r0
	jal r0, .LBB1_51
.LBB1_55:
	lui r4, %hi(sym_letstar)
	addi r4, r4, %lo(sym_letstar)
	ldw r4, r4+0
	bne r3, r4, .LBB1_65
.LBB1_56:
	ldw r3, r1+12
	stw r15+0, r3
	ldw r1, r1+16
	stw r19+0, r1
	ldw r1, r17+0
	addi r3, r1, 1
	stw r17+0, r3
	slli r4, r1, 2
	add r4, r4, r18
	stw r4+0, r15
	addi r4, r1, 2
	stw r17+0, r4
	slli r3, r3, 2
	add r3, r3, r18
	stw r3+0, r14
	addi r1, r1, 3
	stw r17+0, r1
	slli r1, r4, 2
	add r1, r1, r18
	stw r1+0, r19
	ldw r3, r14+0
	jal r31, env_create
	addi r12, fp, -96
	stw r12+0, r1
	ldw r1, r17+0
	addi r3, r1, 1
	stw r17+0, r3
	slli r1, r1, 2
	add r1, r1, r18
	stw r1+0, r12
	add r24, r28, r0
	jal r0, .LBB1_58
.LBB1_57:
	add r24, r3, r0
	bne r25, r13, .LBB1_61
.LBB1_58:
	ldw r1, r15+0
	addi r13, r0, 0
	beq r1, r13, .LBB1_71
.LBB1_59:
	ldw r1, r1+12
	ldw r3, r1+12
	addi r23, fp, -100
	stw r23+0, r3
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r3, r3, 2
	add r3, r3, r18
	stw r3+0, r23
	ldw r1, r1+16
	ldw r3, r1+12
	ldw r4, r12+0
	jal r31, eval
	ldw r3, r17+0
	addi r3, r3, -1
	stw r17+0, r3
	ldw r25, r16+0
	add r3, r13, r0
	bne r25, r13, .LBB1_57
.LBB1_60:
	ldw r3, r12+0
	ldw r4, r23+0
	add r5, r1, r0
	jal r31, env_define
	ldw r1, r15+0
	ldw r1, r1+16
	stw r15+0, r1
	add r3, r24, r0
	jal r0, .LBB1_57
.LBB1_61:
	addi r1, r0, 1
	add r24, r3, r0
	jal r0, .LBB1_84
.LBB1_62:
	ldw r1, r17+0
	addi r1, r1, -4
	stw r17+0, r1
	ldw r1, r12+0
	stw r14+0, r1
	ldw r1, r19+0
	ldw r3, r1+16
	addi r12, r0, 0
	beq r3, r12, .LBB1_74
.LBB1_63:
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r5, r3, 2
	add r5, r5, r18
	stw r5+0, r19
	addi r3, r3, 2
	stw r17+0, r3
	slli r3, r4, 2
	add r3, r3, r18
	stw r3+0, r14
	ldw r3, r1+12
	ldw r4, r14+0
	jal r31, eval
	ldw r1, r17+0
	addi r1, r1, -2
	stw r17+0, r1
	ldw r1, r16+0
	bne r1, r12, .LBB1_83
.LBB1_64:
	ldw r1, r19+0
	ldw r1, r1+16
	stw r19+0, r1
	ldw r3, r1+16
	addi r4, r0, 0
	bne r3, r4, .LBB1_63
	jal r0, .LBB1_74
.LBB1_65:
	lui r4, %hi(sym_and)
	addi r4, r4, %lo(sym_and)
	ldw r4, r4+0
	bne r3, r4, .LBB1_75
.LBB1_66:
	addi r11, r0, 0
	bne r1, r11, .LBB1_68
	jal r0, .LBB1_123
.LBB1_67:
	add r28, r12, r0
	beq r1, r11, .LBB1_1
.LBB1_68:
	ldw r1, r20+0
	ldw r3, r1+16
	beq r3, r11, .LBB1_41
.LBB1_69:
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r5, r3, 2
	add r5, r5, r18
	stw r5+0, r20
	addi r3, r3, 2
	stw r17+0, r3
	slli r3, r4, 2
	add r3, r3, r18
	stw r3+0, r14
	ldw r3, r1+12
	ldw r4, r14+0
	jal r31, eval
	ldw r3, r17+0
	addi r3, r3, -2
	stw r17+0, r3
	ldw r3, r16+0
	seq r3, r3, r11
	sne r1, r1, r11
	and r1, r3, r1
	addi r3, r0, 1
	add r12, r11, r0
	bne r1, r3, .LBB1_67
.LBB1_70:
	ldw r3, r20+0
	ldw r3, r3+16
	stw r20+0, r3
	add r12, r28, r0
	jal r0, .LBB1_67
.LBB1_71:
	ldw r1, r17+0
	addi r1, r1, -4
	stw r17+0, r1
	ldw r1, r12+0
	stw r14+0, r1
	ldw r1, r19+0
	ldw r3, r1+16
	addi r12, r0, 0
	beq r3, r12, .LBB1_74
.LBB1_72:
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r5, r3, 2
	add r5, r5, r18
	stw r5+0, r19
	addi r3, r3, 2
	stw r17+0, r3
	slli r3, r4, 2
	add r3, r3, r18
	stw r3+0, r14
	ldw r3, r1+12
	ldw r4, r14+0
	jal r31, eval
	ldw r1, r17+0
	addi r1, r1, -2
	stw r17+0, r1
	ldw r1, r16+0
	bne r1, r12, .LBB1_83
.LBB1_73:
	ldw r1, r19+0
	ldw r1, r1+16
	stw r19+0, r1
	ldw r3, r1+16
	addi r4, r0, 0
	bne r3, r4, .LBB1_72
.LBB1_74:
	ldw r11, r1+12
	addi r1, r0, 0
	jal r0, .LBB1_84
.LBB1_75:
	lui r4, %hi(sym_or)
	addi r4, r4, %lo(sym_or)
	ldw r4, r4+0
	bne r3, r4, .LBB1_85
.LBB1_76:
	addi r12, r0, 0
	beq r1, r12, .LBB1_1
.LBB1_77:
	add r12, r28, r0
	jal r0, .LBB1_79
.LBB1_78:
	beq r3, r11, .LBB1_1
.LBB1_79:
	add r24, r12, r0
	ldw r1, r20+0
	ldw r3, r1+16
	addi r11, r0, 0
	beq r3, r11, .LBB1_42
.LBB1_80:
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r5, r3, 2
	add r5, r5, r18
	stw r5+0, r20
	addi r3, r3, 2
	stw r17+0, r3
	slli r3, r4, 2
	add r3, r3, r18
	stw r3+0, r14
	ldw r3, r1+12
	ldw r4, r14+0
	jal r31, eval
	ldw r3, r17+0
	addi r3, r3, -2
	stw r17+0, r3
	ldw r4, r16+0
	add r3, r11, r0
	add r12, r11, r0
	bne r4, r11, .LBB1_78
.LBB1_81:
	addi r3, r0, 0
	add r12, r1, r0
	bne r1, r3, .LBB1_78
.LBB1_82:
	ldw r1, r20+0
	ldw r1, r1+16
	stw r20+0, r1
	addi r3, r0, 1
	add r12, r24, r0
	jal r0, .LBB1_78
.LBB1_83:
	addi r1, r0, 1
	add r24, r12, r0
.LBB1_84:
	addi r3, r0, 0
	add r12, r24, r0
	beq r1, r3, .LBB1_95
	jal r0, .LBB1_1
.LBB1_85:
	lui r4, %hi(sym_set)
	addi r4, r4, %lo(sym_set)
	ldw r4, r4+0
	beq r3, r4, .LBB1_124
.LBB1_86:
	ldw r1, r17+0
	addi r4, r1, 1
	stw r17+0, r4
	slli r5, r1, 2
	add r5, r5, r18
	stw r5+0, r20
	addi r1, r1, 2
	stw r17+0, r1
	slli r1, r4, 2
	add r1, r1, r18
	stw r1+0, r14
	ldw r4, r14+0
	jal r31, eval
	add r13, r1, r0
	addi r1, fp, -104
	stw r1+0, r13
	ldw r4, r17+0
	addi r3, r4, -2
	stw r17+0, r3
	ldw r5, r16+0
	addi r12, r0, 0
	bne r5, r12, .LBB1_1
.LBB1_87:
	addi r12, r0, 0
	beq r13, r12, .LBB1_102
.LBB1_88:
	andi r5, r13, 1
	addi r23, r0, 0
	bne r5, r23, .LBB1_102
.LBB1_89:
	ldw r5, r13+0
	addi r6, r0, 3
	bne r5, r6, .LBB1_99
.LBB1_90:
	addi r4, r4, -1
	stw r17+0, r4
	slli r3, r3, 2
	add r3, r3, r18
	stw r3+0, r1
	ldw r3, r20+0
	ldw r4, r14+0
	jal r31, eval_list
	ldw r3, r17+0
	addi r3, r3, -1
	stw r17+0, r3
	ldw r3, r16+0
	addi r25, r0, 0
	addi r23, r0, 1
	beq r3, r25, .LBB1_92
.LBB1_91:
	add r24, r25, r0
	jal r0, .LBB1_94
.LBB1_92:
	ldw r3, r13+20
	ldw r4, r13+12
	add r5, r1, r0
	jal r31, env_extend
	ldw r3, r16+0
	addi r24, r0, 0
	bne r3, r24, .LBB1_94
.LBB1_93:
	ldw r11, r13+16
	stw r14+0, r1
	addi r23, r0, 0
	add r24, r28, r0
.LBB1_94:
	add r12, r24, r0
	bne r23, r25, .LBB1_1
.LBB1_95:
	ldw r1, r16+0
	addi r12, r0, 0
	beq r1, r12, .LBB1_3
	jal r0, .LBB1_1
.LBB1_96:
	addi r3, r0, 1
	bne r1, r3, .LBB1_98
.LBB1_97:
	lui r1, %hi(sym_true)
	addi r1, r1, %lo(sym_true)
	ldw r1, r1+0
	bne r11, r1, .LBB1_104
.LBB1_98:
	add r12, r11, r0
	jal r0, .LBB1_1
.LBB1_99:
	addi r6, r0, 4
	bne r5, r6, .LBB1_103
.LBB1_100:
	addi r4, r4, -1
	stw r17+0, r4
	slli r3, r3, 2
	add r3, r3, r18
	stw r3+0, r1
	ldw r3, r20+0
	ldw r4, r14+0
	jal r31, eval_list
	ldw r3, r17+0
	addi r3, r3, -1
	stw r17+0, r3
	ldw r3, r16+0
	addi r12, r0, 0
	bne r3, r12, .LBB1_1
.LBB1_101:
	ldw r4, r13+16
	add r3, r1, r0
	jalr lr, r4, 0
	add r12, r1, r0
	jal r0, .LBB1_1
.LBB1_102:
	lui r3, %hi(.L.str.14)
	addi r3, r3, %lo(.L.str.14)
	jal r31, lisp_error
	jal r0, .LBB1_1
.LBB1_103:
	lui r3, %hi(.L.str.14)
	addi r3, r3, %lo(.L.str.14)
	jal r31, lisp_error
	add r12, r23, r0
	jal r0, .LBB1_1
.LBB1_104:
	ldw r3, r14+0
	add r4, r11, r0
	jal r31, env_lookup
	add r12, r1, r0
	jal r0, .LBB1_1
.LBB1_105:
	addi r12, r0, 0
	beq r1, r12, .LBB1_107
.LBB1_106:
	ldw r12, r1+12
	jal r0, .LBB1_1
.LBB1_107:
	lui r3, %hi(.L.str.12)
	addi r3, r3, %lo(.L.str.12)
	jal r31, lisp_error
	jal r0, .LBB1_1
.LBB1_108:
	ldw r3, r1+12
	addi r11, r0, 0
	beq r3, r11, .LBB1_111
.LBB1_109:
	andi r4, r3, 1
	addi r12, r0, 0
	bne r4, r12, .LBB1_111
.LBB1_110:
	ldw r4, r3+0
	beq r4, r12, .LBB1_118
.LBB1_111:
	stw r15+0, r3
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r5, r3, 2
	add r5, r5, r18
	stw r5+0, r15
	addi r3, r3, 2
	stw r17+0, r3
	slli r3, r4, 2
	add r3, r3, r18
	stw r3+0, r14
	ldw r1, r1+16
	ldw r3, r1+12
	ldw r4, r14+0
	jal r31, eval
	ldw r3, r17+0
	addi r3, r3, -2
	stw r17+0, r3
	ldw r3, r16+0
	bne r3, r11, .LBB1_98
.LBB1_112:
	ldw r3, r14+0
	ldw r4, r15+0
	add r5, r1, r0
	jal r31, env_define
	lui r1, 524288
	addi r11, r1, -1
	jal r0, .LBB1_98
.LBB1_113:
	ldw r3, r1+12
	stw r15+0, r3
	ldw r4, r1+16
	stw r19+0, r4
	ldw r1, r4+16
	addi r12, r0, 0
	beq r1, r12, .LBB1_115
.LBB1_114:
	ldw r1, r17+0
	addi r3, r1, 1
	stw r17+0, r3
	slli r5, r1, 2
	add r5, r5, r18
	stw r5+0, r15
	addi r5, r1, 2
	stw r17+0, r5
	slli r3, r3, 2
	add r3, r3, r18
	stw r3+0, r14
	addi r1, r1, 3
	stw r17+0, r1
	slli r1, r5, 2
	add r1, r1, r18
	stw r1+0, r19
	lui r1, %hi(sym_begin)
	addi r1, r1, %lo(sym_begin)
	ldw r3, r1+0
	jal r31, cons_alloc
	ldw r3, r17+0
	addi r3, r3, -3
	stw r17+0, r3
	jal r0, .LBB1_116
.LBB1_115:
	ldw r1, r4+12
.LBB1_116:
	ldw r3, r16+0
	bne r3, r12, .LBB1_1
.LBB1_117:
	ldw r3, r15+0
	ldw r5, r14+0
	add r4, r1, r0
	jal r31, lambda_alloc
	add r12, r1, r0
	jal r0, .LBB1_1
.LBB1_118:
	ldw r4, r3+12
	stw r15+0, r4
	ldw r3, r3+16
	stw r19+0, r3
	ldw r4, r1+16
	addi r1, fp, -96
	stw r1+0, r4
	ldw r3, r17+0
	addi r5, r3, 1
	stw r17+0, r5
	slli r6, r3, 2
	add r6, r6, r18
	stw r6+0, r15
	addi r6, r3, 2
	stw r17+0, r6
	slli r5, r5, 2
	add r5, r5, r18
	stw r5+0, r14
	addi r5, r3, 3
	stw r17+0, r5
	slli r6, r6, 2
	add r6, r6, r18
	stw r6+0, r19
	addi r3, r3, 4
	stw r17+0, r3
	slli r3, r5, 2
	add r3, r3, r18
	stw r3+0, r1
	ldw r1, r4+16
	beq r1, r12, .LBB1_120
.LBB1_119:
	lui r1, %hi(sym_begin)
	addi r1, r1, %lo(sym_begin)
	ldw r3, r1+0
	jal r31, cons_alloc
	add r4, r1, r0
	jal r0, .LBB1_121
.LBB1_120:
	ldw r4, r4+12
.LBB1_121:
	addi r1, fp, -100
	stw r1+0, r4
	ldw r3, r17+0
	addi r5, r3, 1
	stw r17+0, r5
	slli r3, r3, 2
	add r3, r3, r18
	stw r3+0, r1
	ldw r3, r19+0
	ldw r5, r14+0
	jal r31, lambda_alloc
	ldw r3, r17+0
	addi r3, r3, -5
	stw r17+0, r3
	ldw r3, r16+0
	bne r3, r12, .LBB1_1
.LBB1_122:
	ldw r3, r14+0
	ldw r4, r15+0
	add r5, r1, r0
	jal r31, env_define
	lui r1, 524288
	addi r12, r1, -1
	jal r0, .LBB1_1
.LBB1_123:
	lui r1, %hi(sym_true)
	addi r1, r1, %lo(sym_true)
	ldw r12, r1+0
	jal r0, .LBB1_1
.LBB1_124:
	ldw r3, r1+12
	stw r15+0, r3
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r5, r3, 2
	add r5, r5, r18
	stw r5+0, r15
	addi r3, r3, 2
	stw r17+0, r3
	slli r3, r4, 2
	add r3, r3, r18
	stw r3+0, r14
	ldw r1, r1+16
	ldw r3, r1+12
	ldw r4, r14+0
	jal r31, eval
	ldw r3, r17+0
	addi r3, r3, -2
	stw r17+0, r3
	ldw r3, r16+0
	addi r12, r0, 0
	bne r3, r12, .LBB1_1
.LBB1_125:
	ldw r3, r14+0
	ldw r4, r15+0
	add r5, r1, r0
	jal r31, env_set
	lui r3, 524288
	addi r12, r3, -1
	addi r3, r0, 0
	bne r1, r3, .LBB1_1
.LBB1_126:
	ldw r1, r15+0
	ldw r4, r1+12
	lui r3, %hi(.L.str.13)
	addi r3, r3, %lo(.L.str.13)
	jal r31, lisp_error2
	jal r0, .LBB1_1
.Lfunc_end1:
	.size	eval, .Lfunc_end1-eval
                                        # -- End function
	.p2align	2                               # -- Begin function eval_list
	.type	eval_list,@function
eval_list:                              # @eval_list
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
	addi r12, fp, -44
	stw r12+0, r3
	addi r13, fp, -48
	stw r13+0, r4
	addi r11, r0, 0
	beq r3, r11, .LBB2_9
.LBB2_1:
	lui r14, %hi(root_sp)
	addi r14, r14, %lo(root_sp)
	ldw r1, r14+0
	addi r3, r1, 1
	stw r14+0, r3
	slli r4, r1, 2
	lui r15, %hi(root_stack)
	addi r15, r15, %lo(root_stack)
	add r4, r4, r15
	stw r4+0, r12
	addi r4, r1, 2
	stw r14+0, r4
	slli r3, r3, 2
	add r3, r3, r15
	stw r3+0, r13
	addi r11, r0, 0
	addi r16, fp, -52
	stw r16+0, r11
	addi r17, fp, -56
	stw r17+0, r11
	addi r3, r1, 3
	stw r14+0, r3
	slli r4, r4, 2
	add r4, r4, r15
	stw r4+0, r16
	addi r1, r1, 4
	stw r14+0, r1
	slli r1, r3, 2
	add r1, r1, r15
	stw r1+0, r17
	ldw r1, r12+0
	beq r1, r11, .LBB2_8
.LBB2_2:
	addi r18, fp, -60
	lui r19, %hi(g_error)
	addi r19, r19, %lo(g_error)
	jal r0, .LBB2_5
.LBB2_3:
	ldw r3, r17+0
	stw r3+16, r1
.LBB2_4:
	stw r17+0, r1
	ldw r1, r12+0
	ldw r1, r1+16
	stw r12+0, r1
	beq r1, r11, .LBB2_8
.LBB2_5:
	ldw r3, r1+12
	ldw r4, r13+0
	jal r31, eval
	stw r18+0, r1
	ldw r3, r19+0
	bne r3, r11, .LBB2_9
.LBB2_6:
	ldw r3, r14+0
	addi r4, r3, 1
	stw r14+0, r4
	slli r3, r3, 2
	add r3, r3, r15
	stw r3+0, r18
	add r3, r1, r0
	add r4, r11, r0
	jal r31, cons_alloc
	ldw r3, r14+0
	addi r3, r3, -1
	stw r14+0, r3
	ldw r3, r16+0
	bne r3, r11, .LBB2_3
.LBB2_7:
	stw r16+0, r1
	jal r0, .LBB2_4
.LBB2_8:
	ldw r1, r14+0
	addi r1, r1, -4
	stw r14+0, r1
	ldw r11, r16+0
.LBB2_9:
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
	addi sp, sp, 72
	jalr r0, r31, 0
.Lfunc_end2:
	.size	eval_list, .Lfunc_end2-eval_list
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"quote"
	.size	.L.str, 6

	.type	sym_quote,@object               # @sym_quote
	.local	sym_quote
	.comm	sym_quote,4,4
	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"if"
	.size	.L.str.1, 3

	.type	sym_if,@object                  # @sym_if
	.local	sym_if
	.comm	sym_if,4,4
	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"cond"
	.size	.L.str.2, 5

	.type	sym_cond,@object                # @sym_cond
	.local	sym_cond
	.comm	sym_cond,4,4
	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"define"
	.size	.L.str.3, 7

	.type	sym_define,@object              # @sym_define
	.local	sym_define
	.comm	sym_define,4,4
	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"lambda"
	.size	.L.str.4, 7

	.type	sym_lambda,@object              # @sym_lambda
	.local	sym_lambda
	.comm	sym_lambda,4,4
	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"begin"
	.size	.L.str.5, 6

	.type	sym_begin,@object               # @sym_begin
	.local	sym_begin
	.comm	sym_begin,4,4
	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"let"
	.size	.L.str.6, 4

	.type	sym_let,@object                 # @sym_let
	.local	sym_let
	.comm	sym_let,4,4
	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"let*"
	.size	.L.str.7, 5

	.type	sym_letstar,@object             # @sym_letstar
	.local	sym_letstar
	.comm	sym_letstar,4,4
	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"and"
	.size	.L.str.8, 4

	.type	sym_and,@object                 # @sym_and
	.local	sym_and
	.comm	sym_and,4,4
	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"or"
	.size	.L.str.9, 3

	.type	sym_or,@object                  # @sym_or
	.local	sym_or
	.comm	sym_or,4,4
	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"set!"
	.size	.L.str.10, 5

	.type	sym_set,@object                 # @sym_set
	.local	sym_set
	.comm	sym_set,4,4
	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"else"
	.size	.L.str.11, 5

	.type	sym_else,@object                # @sym_else
	.local	sym_else
	.comm	sym_else,4,4
	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"quote: missing argument"
	.size	.L.str.12, 24

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"set!: unbound variable"
	.size	.L.str.13, 23

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"not a procedure"
	.size	.L.str.14, 16

	.type	global_env,@object              # @global_env
	.bss
	.globl	global_env
	.p2align	2, 0x0
global_env:
	.word	0                               # 0x0
	.size	global_env, 4

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
