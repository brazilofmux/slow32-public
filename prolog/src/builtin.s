	.file	"builtin.c"
	.text
	.globl	try_builtin                     # -- Begin function try_builtin
	.p2align	2
	.type	try_builtin,@function
try_builtin:                            # @try_builtin
# %bb.0:
	addi sp, sp, -2048
	addi sp, sp, -200
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 2047
	addi fp, fp, 201
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
	lui r23, %hi(ATOM_TRUE)
	addi r23, r23, %lo(ATOM_TRUE)
	addi r11, r0, 1
	addi r1, r0, 0
	bne r5, r1, .LBB0_3
.LBB0_1:
	ldw r6, r23+0
	bne r4, r6, .LBB0_3
.LBB0_2:
	add r1, r11, r0
	jal r0, .LBB0_20
.LBB0_3:
	lui r24, %hi(ATOM_FAIL)
	addi r24, r24, %lo(ATOM_FAIL)
	addi r19, r0, -1
	bne r5, r1, .LBB0_6
.LBB0_4:
	ldw r6, r24+0
	bne r4, r6, .LBB0_6
.LBB0_5:
	add r1, r19, r0
	jal r0, .LBB0_20
.LBB0_6:
	lui r6, %hi(ATOM_HALT)
	addi r6, r6, %lo(ATOM_HALT)
	ldw r6, r6+0
	bne r5, r1, .LBB0_9
.LBB0_7:
	bne r4, r6, .LBB0_9
.LBB0_8:
	addi r3, r0, 0
	jal r31, exit
	add r1, r11, r0
	jal r0, .LBB0_20
.LBB0_9:
	addi r12, r0, 1
	bne r5, r12, .LBB0_12
.LBB0_10:
	bne r4, r6, .LBB0_12
.LBB0_11:
	addi r11, r0, 0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	andi r3, r1, 3
	addi r12, r0, 1
	seq r3, r3, r12
	srai r1, r1, 2
	sub r3, r11, r3
	and r3, r1, r3
	jal r31, exit
	add r1, r12, r0
	jal r0, .LBB0_20
.LBB0_12:
	bne r5, r12, .LBB0_15
.LBB0_13:
	lui r6, %hi(ATOM_WRITE)
	addi r6, r6, %lo(ATOM_WRITE)
	ldw r6, r6+0
	bne r4, r6, .LBB0_15
.LBB0_14:
	addi r4, r0, 0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r3, r1, r0
	jal r31, write_term
	jal r0, .LBB0_19
.LBB0_15:
	bne r5, r12, .LBB0_21
.LBB0_16:
	lui r6, %hi(ATOM_WRITELN)
	addi r6, r6, %lo(ATOM_WRITELN)
	ldw r6, r6+0
	bne r4, r6, .LBB0_21
.LBB0_17:
	addi r4, r0, 0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r3, r1, r0
	jal r31, write_term
.LBB0_18:
	addi r3, r0, 10
	jal r31, putchar
.LBB0_19:
	addi r1, r0, 1
.LBB0_20:
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
	addi sp, sp, 2047
	addi sp, sp, 201
	jalr r0, r31, 0
.LBB0_21:
	bne r5, r12, .LBB0_23
.LBB0_22:
	lui r6, %hi(ATOM_WRITE_CANONICAL)
	addi r6, r6, %lo(ATOM_WRITE_CANONICAL)
	ldw r6, r6+0
	beq r4, r6, .LBB0_14
.LBB0_23:
	bne r5, r1, .LBB0_25
.LBB0_24:
	lui r6, %hi(ATOM_NL)
	addi r6, r6, %lo(ATOM_NL)
	ldw r6, r6+0
	beq r4, r6, .LBB0_18
.LBB0_25:
	addi r11, r0, 2
	bne r5, r11, .LBB0_33
.LBB0_26:
	lui r6, %hi(ATOM_UNIFY)
	addi r6, r6, %lo(ATOM_UNIFY)
	ldw r6, r6+0
	bne r4, r6, .LBB0_33
.LBB0_27:
	addi r11, r0, 0
	add r12, r3, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r13, r1, r0
	addi r4, r0, 1
	add r3, r12, r0
	jal r31, compound_arg
.LBB0_28:
	add r3, r13, r0
.LBB0_29:
	add r4, r1, r0
.LBB0_30:
	jal r31, unify
	seq r1, r1, r11
.LBB0_31:
	sub r1, r11, r1
.LBB0_32:
	ori  r1, r1, 1
	jal r0, .LBB0_20
.LBB0_33:
	bne r5, r11, .LBB0_37
.LBB0_34:
	lui r6, %hi(ATOM_NOT_UNIFY)
	addi r6, r6, %lo(ATOM_NOT_UNIFY)
	ldw r6, r6+0
	bne r4, r6, .LBB0_37
.LBB0_35:
	addi r12, r0, 0
	add r11, r3, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r13, r1, r0
	addi r14, r0, 1
	add r3, r11, r0
	add r4, r14, r0
	jal r31, compound_arg
	lui r3, %hi(trail_top)
	addi r3, r3, %lo(trail_top)
	ldw r11, r3+0
	add r3, r13, r0
	add r4, r1, r0
	jal r31, unify
	add r3, r1, r0
	add r1, r14, r0
	beq r3, r12, .LBB0_20
.LBB0_36:
	add r3, r11, r0
	jal r31, trail_undo
	add r1, r19, r0
	jal r0, .LBB0_20
.LBB0_37:
	bne r5, r11, .LBB0_40
.LBB0_38:
	lui r6, %hi(ATOM_EQ)
	addi r6, r6, %lo(ATOM_EQ)
	ldw r6, r6+0
	bne r4, r6, .LBB0_40
.LBB0_39:
	addi r4, r0, 0
	add r11, r3, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r12, r1, r0
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	seq r1, r12, r1
	addi r1, r1, -1
	jal r0, .LBB0_32
.LBB0_40:
	bne r5, r11, .LBB0_43
.LBB0_41:
	lui r6, %hi(ATOM_NEQ)
	addi r6, r6, %lo(ATOM_NEQ)
	ldw r6, r6+0
	bne r4, r6, .LBB0_43
.LBB0_42:
	addi r11, r0, 0
	add r12, r3, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	addi r4, r0, 1
	add r3, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	seq r1, r13, r1
	jal r0, .LBB0_31
.LBB0_43:
	bne r5, r11, .LBB0_47
.LBB0_44:
	lui r6, %hi(ATOM_IS)
	addi r6, r6, %lo(ATOM_IS)
	ldw r6, r6+0
	bne r4, r6, .LBB0_47
.LBB0_45:
	addi r11, r0, 0
	add r13, r3, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r12, r1, r0
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	addi r13, fp, -1100
	add r3, r1, r0
	add r4, r13, r0
	jal r31, eval_arith
	bne r1, r11, .LBB0_46
	jal r0, .LBB0_308
.LBB0_46:
	ldw r1, r13+0
	slli r1, r1, 2
	addi r4, r1, 1
	add r3, r12, r0
	jal r0, .LBB0_30
.LBB0_47:
	bne r5, r11, .LBB0_53
.LBB0_48:
	lui r6, %hi(ATOM_ARITH_EQ)
	addi r6, r6, %lo(ATOM_ARITH_EQ)
	ldw r6, r6+0
	bne r4, r6, .LBB0_53
.LBB0_49:
	addi r11, r0, 0
	add r13, r3, r0
	add r4, r11, r0
	jal r31, compound_arg
	addi r12, fp, -1100
	add r3, r1, r0
	add r4, r12, r0
	jal r31, eval_arith
	addi r14, r0, -1
	beq r1, r11, .LBB0_60
.LBB0_50:
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r3, fp, r0
	addi r3, r3, -2048
	addi r13, r3, -76
	add r3, r1, r0
	add r4, r13, r0
	jal r31, eval_arith
	beq r1, r11, .LBB0_60
.LBB0_51:
	ldw r1, r12+0
	ldw r3, r13+0
	seq r1, r1, r3
.LBB0_52:
	addi r1, r1, -1
	jal r0, .LBB0_59
.LBB0_53:
	add r13, r3, r0
	bne r5, r11, .LBB0_61
.LBB0_54:
	lui r3, %hi(ATOM_ARITH_NEQ)
	addi r3, r3, %lo(ATOM_ARITH_NEQ)
	ldw r3, r3+0
	bne r4, r3, .LBB0_61
.LBB0_55:
	addi r11, r0, 0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	addi r12, fp, -1100
	add r3, r1, r0
	add r4, r12, r0
	jal r31, eval_arith
	addi r14, r0, -1
	beq r1, r11, .LBB0_60
.LBB0_56:
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r3, fp, r0
	addi r3, r3, -2048
	addi r13, r3, -76
	add r3, r1, r0
	add r4, r13, r0
	jal r31, eval_arith
	beq r1, r11, .LBB0_60
.LBB0_57:
	ldw r1, r12+0
	ldw r3, r13+0
	seq r1, r1, r3
.LBB0_58:
	sub r1, r11, r1
.LBB0_59:
	ori  r14, r1, 1
.LBB0_60:
	add r1, r14, r0
	jal r0, .LBB0_20
.LBB0_61:
	bne r5, r11, .LBB0_66
.LBB0_62:
	lui r3, %hi(ATOM_LT)
	addi r3, r3, %lo(ATOM_LT)
	ldw r3, r3+0
	bne r4, r3, .LBB0_66
.LBB0_63:
	addi r11, r0, 0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	addi r12, fp, -1100
	add r3, r1, r0
	add r4, r12, r0
	jal r31, eval_arith
	addi r14, r0, -1
	beq r1, r11, .LBB0_60
.LBB0_64:
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r3, fp, r0
	addi r3, r3, -2048
	addi r13, r3, -76
	add r3, r1, r0
	add r4, r13, r0
	jal r31, eval_arith
	beq r1, r11, .LBB0_60
.LBB0_65:
	ldw r1, r12+0
	ldw r3, r13+0
	slt r1, r1, r3
	jal r0, .LBB0_52
.LBB0_66:
	bne r5, r11, .LBB0_71
.LBB0_67:
	lui r3, %hi(ATOM_GT)
	addi r3, r3, %lo(ATOM_GT)
	ldw r3, r3+0
	bne r4, r3, .LBB0_71
.LBB0_68:
	addi r11, r0, 0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	addi r12, fp, -1100
	add r3, r1, r0
	add r4, r12, r0
	jal r31, eval_arith
	addi r14, r0, -1
	beq r1, r11, .LBB0_60
.LBB0_69:
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r3, fp, r0
	addi r3, r3, -2048
	addi r13, r3, -76
	add r3, r1, r0
	add r4, r13, r0
	jal r31, eval_arith
	beq r1, r11, .LBB0_60
.LBB0_70:
	ldw r1, r12+0
	ldw r3, r13+0
	sgt r1, r1, r3
	jal r0, .LBB0_52
.LBB0_71:
	bne r5, r11, .LBB0_76
.LBB0_72:
	lui r3, %hi(ATOM_LE)
	addi r3, r3, %lo(ATOM_LE)
	ldw r3, r3+0
	bne r4, r3, .LBB0_76
.LBB0_73:
	addi r11, r0, 0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	addi r12, fp, -1100
	add r3, r1, r0
	add r4, r12, r0
	jal r31, eval_arith
	addi r14, r0, -1
	beq r1, r11, .LBB0_60
.LBB0_74:
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r3, fp, r0
	addi r3, r3, -2048
	addi r13, r3, -76
	add r3, r1, r0
	add r4, r13, r0
	jal r31, eval_arith
	beq r1, r11, .LBB0_60
.LBB0_75:
	ldw r1, r12+0
	ldw r3, r13+0
	sgt r1, r1, r3
	jal r0, .LBB0_58
.LBB0_76:
	bne r5, r11, .LBB0_81
.LBB0_77:
	lui r3, %hi(ATOM_GE)
	addi r3, r3, %lo(ATOM_GE)
	ldw r3, r3+0
	bne r4, r3, .LBB0_81
.LBB0_78:
	addi r11, r0, 0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	addi r12, fp, -1100
	add r3, r1, r0
	add r4, r12, r0
	jal r31, eval_arith
	addi r14, r0, -1
	beq r1, r11, .LBB0_60
.LBB0_79:
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r3, fp, r0
	addi r3, r3, -2048
	addi r13, r3, -76
	add r3, r1, r0
	add r4, r13, r0
	jal r31, eval_arith
	beq r1, r11, .LBB0_60
.LBB0_80:
	ldw r1, r12+0
	ldw r3, r13+0
	slt r1, r1, r3
	jal r0, .LBB0_58
.LBB0_81:
	bne r5, r12, .LBB0_84
.LBB0_82:
	lui r3, %hi(ATOM_ATOM)
	addi r3, r3, %lo(ATOM_ATOM)
	ldw r3, r3+0
	bne r4, r3, .LBB0_84
.LBB0_83:
	addi r4, r0, 0
	add r3, r13, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	andi r1, r1, 3
	seq r1, r1, r11
	addi r1, r1, -1
	jal r0, .LBB0_32
.LBB0_84:
	bne r5, r12, .LBB0_86
.LBB0_85:
	lui r3, %hi(ATOM_INTEGER)
	addi r3, r3, %lo(ATOM_INTEGER)
	ldw r3, r3+0
	beq r4, r3, .LBB0_88
.LBB0_86:
	bne r5, r12, .LBB0_90
.LBB0_87:
	lui r3, %hi(ATOM_NUMBER)
	addi r3, r3, %lo(ATOM_NUMBER)
	ldw r3, r3+0
	bne r4, r3, .LBB0_90
.LBB0_88:
	addi r4, r0, 0
	add r3, r13, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	andi r1, r1, 3
	addi r3, r0, 1
.LBB0_89:
	seq r1, r1, r3
	addi r1, r1, -1
	jal r0, .LBB0_32
.LBB0_90:
	bne r5, r12, .LBB0_93
.LBB0_91:
	lui r3, %hi(ATOM_VAR)
	addi r3, r3, %lo(ATOM_VAR)
	ldw r3, r3+0
	bne r4, r3, .LBB0_93
.LBB0_92:
	addi r4, r0, 0
	add r3, r13, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	andi r1, r1, 3
	addi r3, r0, 3
	jal r0, .LBB0_89
.LBB0_93:
	bne r5, r12, .LBB0_96
.LBB0_94:
	lui r3, %hi(ATOM_NONVAR)
	addi r3, r3, %lo(ATOM_NONVAR)
	ldw r3, r3+0
	bne r4, r3, .LBB0_96
.LBB0_95:
	addi r11, r0, 0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	andi r1, r1, 3
	addi r3, r0, 3
	seq r1, r1, r3
	jal r0, .LBB0_31
.LBB0_96:
	bne r5, r12, .LBB0_99
.LBB0_97:
	lui r3, %hi(ATOM_COMPOUND)
	addi r3, r3, %lo(ATOM_COMPOUND)
	ldw r3, r3+0
	bne r4, r3, .LBB0_99
.LBB0_98:
	addi r11, r0, 0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	andi r3, r1, 3
	seq r3, r3, r11
	sne r1, r1, r11
	sub r1, r11, r1
	sub r3, r11, r3
	jal r0, .LBB0_128
.LBB0_99:
	bne r5, r12, .LBB0_108
.LBB0_100:
	lui r3, %hi(ATOM_IS_LIST)
	addi r3, r3, %lo(ATOM_IS_LIST)
	ldw r3, r3+0
	bne r4, r3, .LBB0_108
.LBB0_101:
	addi r12, r0, 0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	andi r1, r1, 3
	beq r13, r12, .LBB0_127
.LBB0_102:
	bne r1, r12, .LBB0_127
.LBB0_103:
	lui r15, %hi(ATOM_DOT)
	addi r15, r15, %lo(ATOM_DOT)
	addi r14, r0, 1
.LBB0_104:
	add r3, r13, r0
	jal r31, compound_functor
	ldw r3, r15+0
	bne r1, r3, .LBB0_126
.LBB0_105:
	add r3, r13, r0
	jal r31, compound_arity
	bne r1, r11, .LBB0_126
.LBB0_106:
	add r3, r13, r0
	add r4, r14, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	andi r1, r1, 3
	beq r13, r12, .LBB0_127
.LBB0_107:
	beq r1, r12, .LBB0_104
	jal r0, .LBB0_127
.LBB0_108:
	addi r17, r0, 3
	bne r5, r17, .LBB0_118
.LBB0_109:
	lui r3, %hi(ATOM_FUNCTOR)
	addi r3, r3, %lo(ATOM_FUNCTOR)
	ldw r3, r3+0
	bne r4, r3, .LBB0_118
.LBB0_110:
	addi r11, r0, 0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r12, r1, r0
	addi r15, r0, 1
	add r3, r13, r0
	add r4, r15, r0
	jal r31, compound_arg
	add r14, r1, r0
	addi r16, r0, 2
	add r3, r13, r0
	add r4, r16, r0
	jal r31, compound_arg
	add r13, r1, r0
	andi r1, r12, 3
	bne r1, r17, .LBB0_129
.LBB0_111:
	add r3, r14, r0
	jal r31, deref
	add r14, r1, r0
	add r3, r13, r0
	jal r31, deref
	andi r3, r1, 3
	bne r3, r15, .LBB0_5
.LBB0_112:
	srai r13, r1, 2
	beq r13, r11, .LBB0_165
.LBB0_113:
	andi r3, r14, 3
	add r1, r19, r0
	bne r3, r16, .LBB0_20
.LBB0_114:
	blt r13, r15, .LBB0_117
.LBB0_115:
	addi r1, r0, 32
	slt r1, r13, r1
	sub r1, r11, r1
	xori r3, r13, 32
	and r1, r3, r1
	xori r15, r1, 32
	addi r16, fp, -1100
.LBB0_116:
	jal r31, fresh_var
	stw r16+0, r1
	addi r15, r15, -1
	addi r16, r16, 4
	bne r15, r11, .LBB0_116
.LBB0_117:
	srli r3, r14, 2
	addi r5, fp, -1100
	add r4, r13, r0
	jal r31, make_compound
	add r3, r12, r0
	jal r0, .LBB0_29
.LBB0_118:
	bne r5, r17, .LBB0_132
.LBB0_119:
	lui r3, %hi(ATOM_ARG)
	addi r3, r3, %lo(ATOM_ARG)
	ldw r3, r3+0
	bne r4, r3, .LBB0_132
.LBB0_120:
	addi r11, r0, 0
	add r14, r13, r0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r15, r1, r0
	addi r13, r0, 1
	add r3, r14, r0
	add r4, r13, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r12, r1, r0
	addi r4, r0, 2
	add r3, r14, r0
	jal r31, compound_arg
	add r14, r1, r0
	andi r1, r15, 3
	bne r1, r13, .LBB0_5
.LBB0_121:
	beq r12, r11, .LBB0_5
.LBB0_122:
	andi r1, r12, 3
	bne r1, r11, .LBB0_5
.LBB0_123:
	srai r15, r15, 2
	add r3, r12, r0
	jal r31, compound_arity
	blt r15, r13, .LBB0_5
.LBB0_124:
	add r3, r1, r0
	add r1, r19, r0
	bgt r15, r3, .LBB0_20
.LBB0_125:
	addi r4, r15, -1
	add r3, r12, r0
	jal r31, compound_arg
	add r3, r14, r0
	jal r0, .LBB0_29
.LBB0_126:
	add r1, r12, r0
.LBB0_127:
	seq r1, r1, r11
	srli r3, r13, 2
	lui r4, %hi(ATOM_NIL_LIST)
	addi r4, r4, %lo(ATOM_NIL_LIST)
	ldw r4, r4+0
	seq r3, r3, r4
	sub r1, r12, r1
	sub r3, r12, r3
.LBB0_128:
	and r1, r3, r1
	addi r3, r0, -1
	xor r1, r1, r3
	jal r0, .LBB0_32
.LBB0_129:
	add r3, r12, r0
	jal r31, term_functor
	add r16, r1, r0
	add r3, r12, r0
	jal r31, term_arity
	add r15, r1, r0
	addi r1, r0, -1
	add r17, r1, r0
	ble r16, r1, .LBB0_155
.LBB0_130:
	slli r1, r16, 2
	addi r4, r1, 2
	add r3, r14, r0
	jal r31, unify
	add r3, r1, r0
	add r1, r17, r0
	beq r3, r11, .LBB0_20
.LBB0_131:
	slli r1, r15, 2
	addi r4, r1, 1
	add r3, r13, r0
	jal r0, .LBB0_30
.LBB0_132:
	bne r5, r11, .LBB0_152
.LBB0_133:
	lui r3, %hi(ATOM_UNIV)
	addi r3, r3, %lo(ATOM_UNIV)
	ldw r3, r3+0
	bne r4, r3, .LBB0_152
.LBB0_134:
	addi r14, r0, 0
	add r3, r13, r0
	add r4, r14, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r12, r1, r0
	addi r15, r0, 1
	add r3, r13, r0
	add r4, r15, r0
	jal r31, compound_arg
	add r13, r1, r0
	andi r1, r12, 3
	bne r1, r17, .LBB0_157
.LBB0_135:
	add r3, r13, r0
	jal r31, deref
	beq r1, r14, .LBB0_5
.LBB0_136:
	add r15, r1, r0
	andi r1, r1, 3
	bne r1, r14, .LBB0_5
.LBB0_137:
	add r3, r15, r0
	jal r31, compound_functor
	lui r20, %hi(ATOM_DOT)
	addi r20, r20, %lo(ATOM_DOT)
	ldw r3, r20+0
	bne r1, r3, .LBB0_5
.LBB0_138:
	addi r16, r0, 0
	add r3, r15, r0
	add r4, r16, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	addi r4, r0, 1
	add r3, r15, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r14, r1, r0
	andi r21, r13, 3
	bne r21, r11, .LBB0_140
.LBB0_139:
	srli r1, r13, 2
	lui r3, %hi(ATOM_NIL_LIST)
	addi r3, r3, %lo(ATOM_NIL_LIST)
	ldw r3, r3+0
	beq r1, r3, .LBB0_5
.LBB0_140:
	add r15, r16, r0
	beq r14, r16, .LBB0_147
.LBB0_141:
	andi r1, r14, 3
	addi r22, r0, 0
	add r15, r16, r0
	bne r1, r22, .LBB0_147
.LBB0_142:
	addi r17, r0, 1
	add r15, r22, r0
	add r18, r14, r0
.LBB0_143:
	add r3, r18, r0
	jal r31, compound_functor
	ldw r3, r20+0
	bne r1, r3, .LBB0_147
.LBB0_144:
	add r3, r18, r0
	jal r31, compound_arity
	bne r1, r11, .LBB0_147
.LBB0_145:
	addi r15, r15, 1
	add r3, r18, r0
	add r4, r17, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	beq r1, r22, .LBB0_147
.LBB0_146:
	add r18, r1, r0
	andi r1, r1, 3
	beq r1, r22, .LBB0_143
.LBB0_147:
	beq r15, r16, .LBB0_254
.LBB0_148:
	add r1, r19, r0
	bne r21, r11, .LBB0_20
.LBB0_149:
	addi r1, r0, 32
	sltu r1, r15, r1
	addi r11, r0, 0
	sub r1, r11, r1
	xori r3, r15, 32
	and r1, r3, r1
	xori r1, r1, 32
	slli r17, r1, 2
	addi r18, fp, -1100
	addi r16, r0, 1
	add r19, r11, r0
.LBB0_150:
	add r3, r14, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, r18, r19
	stw r3+0, r1
	add r3, r14, r0
	add r4, r16, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r14, r1, r0
	addi r19, r19, 4
	bne r17, r19, .LBB0_150
.LBB0_151:
	srli r3, r13, 2
	addi r5, fp, -1100
	add r4, r15, r0
	jal r31, make_compound
	add r3, r12, r0
	add r4, r1, r0
	jal r0, .LBB0_255
.LBB0_152:
	bne r5, r11, .LBB0_163
.LBB0_153:
	lui r3, %hi(ATOM_COPY_TERM)
	addi r3, r3, %lo(ATOM_COPY_TERM)
	ldw r3, r3+0
	bne r4, r3, .LBB0_163
.LBB0_154:
	addi r11, r0, 0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r12, r1, r0
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r13, r1, r0
	addi r14, fp, -1100
	addi r4, r0, -1
	addi r5, r0, 1024
	add r3, r14, r0
	jal r31, memset
	addi r5, r0, 256
	add r3, r12, r0
	add r4, r14, r0
	jal r31, copy_term
	jal r0, .LBB0_28
.LBB0_155:
	add r3, r14, r0
	add r4, r12, r0
	jal r31, unify
	add r3, r1, r0
	add r1, r17, r0
	beq r3, r11, .LBB0_20
.LBB0_156:
	addi r4, r0, 1
	add r3, r13, r0
	jal r0, .LBB0_30
.LBB0_157:
	beq r12, r14, .LBB0_166
.LBB0_158:
	bne r1, r14, .LBB0_166
.LBB0_159:
	add r3, r12, r0
	jal r31, compound_functor
	add r11, r1, r0
	add r3, r12, r0
	jal r31, compound_arity
	lui r3, %hi(ATOM_NIL_LIST)
	addi r3, r3, %lo(ATOM_NIL_LIST)
	ldw r3, r3+0
	slli r3, r3, 2
	addi r16, r3, 2
	blt r1, r15, .LBB0_162
.LBB0_160:
	addi r17, r1, 1
.LBB0_161:
	addi r4, r17, -2
	add r3, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	add r4, r16, r0
	jal r31, make_list_cons
	add r16, r1, r0
	addi r17, r17, -1
	bgt r17, r15, .LBB0_161
.LBB0_162:
	slli r1, r11, 2
	addi r3, r1, 2
	add r4, r16, r0
	jal r0, .LBB0_167
.LBB0_163:
	lui r3, %hi(ATOM_ASSERT)
	addi r3, r3, %lo(ATOM_ASSERT)
	ldw r3, r3+0
	bne r4, r3, .LBB0_168
.LBB0_164:
	addi r3, r0, 1
	beq r5, r3, .LBB0_170
	jal r0, .LBB0_175
.LBB0_165:
	add r3, r12, r0
	add r4, r14, r0
	jal r0, .LBB0_30
.LBB0_166:
	lui r1, %hi(ATOM_NIL_LIST)
	addi r1, r1, %lo(ATOM_NIL_LIST)
	ldw r1, r1+0
	slli r1, r1, 2
	addi r4, r1, 2
	add r3, r12, r0
.LBB0_167:
	jal r31, make_list_cons
	add r3, r13, r0
	add r4, r1, r0
	jal r31, unify
	seq r1, r1, r14
	sub r1, r14, r1
	jal r0, .LBB0_32
.LBB0_168:
	bne r5, r12, .LBB0_175
.LBB0_169:
	lui r3, %hi(ATOM_ASSERTZ)
	addi r3, r3, %lo(ATOM_ASSERTZ)
	ldw r3, r3+0
	bne r4, r3, .LBB0_175
.LBB0_170:
	addi r12, r0, 0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	beq r1, r12, .LBB0_182
.LBB0_171:
	andi r1, r13, 3
	addi r3, r0, 0
	bne r1, r3, .LBB0_182
.LBB0_172:
	add r3, r13, r0
	jal r31, compound_functor
	lui r3, %hi(ATOM_CLAUSE)
	addi r3, r3, %lo(ATOM_CLAUSE)
	ldw r3, r3+0
	bne r1, r3, .LBB0_182
.LBB0_173:
	add r3, r13, r0
	jal r31, compound_arity
	bne r1, r11, .LBB0_182
.LBB0_174:
	addi r4, r0, 0
	add r3, r13, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r11, r1, r0
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r15, r1, r0
	add r13, r11, r0
	jal r0, .LBB0_183
.LBB0_175:
	bne r5, r12, .LBB0_186
.LBB0_176:
	lui r3, %hi(ATOM_ASSERTA)
	addi r3, r3, %lo(ATOM_ASSERTA)
	ldw r3, r3+0
	bne r4, r3, .LBB0_186
.LBB0_177:
	addi r12, r0, 0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	beq r1, r12, .LBB0_193
.LBB0_178:
	andi r1, r13, 3
	addi r3, r0, 0
	bne r1, r3, .LBB0_193
.LBB0_179:
	add r3, r13, r0
	jal r31, compound_functor
	lui r3, %hi(ATOM_CLAUSE)
	addi r3, r3, %lo(ATOM_CLAUSE)
	ldw r3, r3+0
	bne r1, r3, .LBB0_193
.LBB0_180:
	add r3, r13, r0
	jal r31, compound_arity
	bne r1, r11, .LBB0_193
.LBB0_181:
	addi r4, r0, 0
	add r3, r13, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r11, r1, r0
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r15, r1, r0
	add r13, r11, r0
	jal r0, .LBB0_194
.LBB0_182:
	ldw r1, r23+0
	slli r1, r1, 2
	addi r15, r1, 2
.LBB0_183:
	addi r11, fp, -1100
	addi r4, r0, -1
	addi r14, r0, 1024
	add r3, r11, r0
	add r5, r14, r0
	jal r31, memset
	addi r16, r0, 256
	add r3, r13, r0
	add r4, r11, r0
	add r5, r16, r0
	jal r31, copy_term_code
	add r13, r1, r0
	add r3, r15, r0
	add r4, r11, r0
	add r5, r16, r0
	jal r31, copy_term_code
	add r3, r12, r0
	add r5, r12, r0
.LBB0_184:
	add r4, r11, r3
	ldw r4, r4+0
	addi r6, r4, 1
	sltu r7, r6, r4
	slt r4, r4, r5
	sub r7, r12, r7
	sub r4, r12, r4
	xor r8, r5, r6
	and r4, r8, r4
	xor r4, r6, r4
	xor r5, r5, r4
	and r5, r5, r7
	xor r5, r4, r5
	addi r3, r3, 4
	bne r3, r14, .LBB0_184
.LBB0_185:
	addi r6, r0, 0
	add r3, r13, r0
	add r4, r1, r0
	jal r31, db_add_clause
	jal r0, .LBB0_19
.LBB0_186:
	bne r5, r12, .LBB0_197
.LBB0_187:
	lui r3, %hi(ATOM_RETRACT)
	addi r3, r3, %lo(ATOM_RETRACT)
	ldw r3, r3+0
	bne r4, r3, .LBB0_197
.LBB0_188:
	addi r12, r0, 0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	beq r1, r12, .LBB0_256
.LBB0_189:
	andi r1, r13, 3
	addi r14, r0, 0
	bne r1, r14, .LBB0_256
.LBB0_190:
	add r3, r13, r0
	jal r31, compound_functor
	lui r3, %hi(ATOM_CLAUSE)
	addi r3, r3, %lo(ATOM_CLAUSE)
	ldw r3, r3+0
	bne r1, r3, .LBB0_257
.LBB0_191:
	add r3, r13, r0
	jal r31, compound_arity
	bne r1, r11, .LBB0_257
.LBB0_192:
	addi r4, r0, 0
	add r3, r13, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r11, r1, r0
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r14, r1, r0
	jal r0, .LBB0_258
.LBB0_193:
	ldw r1, r23+0
	slli r1, r1, 2
	addi r15, r1, 2
.LBB0_194:
	addi r11, fp, -1100
	addi r4, r0, -1
	addi r14, r0, 1024
	add r3, r11, r0
	add r5, r14, r0
	jal r31, memset
	addi r16, r0, 256
	add r3, r13, r0
	add r4, r11, r0
	add r5, r16, r0
	jal r31, copy_term_code
	add r13, r1, r0
	add r3, r15, r0
	add r4, r11, r0
	add r5, r16, r0
	jal r31, copy_term_code
	add r3, r12, r0
	add r5, r12, r0
.LBB0_195:
	add r4, r11, r3
	ldw r4, r4+0
	addi r6, r4, 1
	sltu r7, r6, r4
	slt r4, r4, r5
	sub r7, r12, r7
	sub r4, r12, r4
	xor r8, r5, r6
	and r4, r8, r4
	xor r4, r6, r4
	xor r5, r5, r4
	and r5, r5, r7
	xor r5, r4, r5
	addi r3, r3, 4
	bne r3, r14, .LBB0_195
.LBB0_196:
	addi r11, r0, 1
	add r3, r13, r0
	add r4, r1, r0
	add r6, r11, r0
	jal r31, db_add_clause
	add r1, r11, r0
	jal r0, .LBB0_20
.LBB0_197:
	bne r5, r17, .LBB0_260
.LBB0_198:
	lui r3, %hi(ATOM_FINDALL)
	addi r3, r3, %lo(ATOM_FINDALL)
	ldw r3, r3+0
	bne r4, r3, .LBB0_260
.LBB0_199:
	addi r15, r0, 0
	add r3, r13, r0
	add r4, r15, r0
	jal r31, compound_arg
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-128, r1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r17, r1, r0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-176, r1
	lui r1, %hi(trail_top)
	addi r1, r1, %lo(trail_top)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-120, r1
	ldw r1, r1+0
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-152, r1
	lui r1, %hi(hp)
	addi r1, r1, %lo(hp)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-104, r1
	ldw r1, r1+0
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-148, r1
	lui r1, %hi(var_count)
	addi r1, r1, %lo(var_count)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-112, r1
	ldw r1, r1+0
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-156, r1
	lui r13, %hi(goal_sp)
	addi r13, r13, %lo(goal_sp)
	ldw r1, r13+0
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-160, r1
	lui r28, %hi(choice_top)
	addi r28, r28, %lo(choice_top)
	ldw r1, r28+0
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-164, r1
	lui r1, %hi(cut_barrier)
	addi r1, r1, %lo(cut_barrier)
	ldw r3, r1+0
	add r4, fp, r0
	addi r4, r4, -2048
	stw r4+-168, r3
	stw r28+0, r15
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-116, r1
	stw r1+0, r15
	stw r13+0, r12
	lui r19, %hi(goal_stack)
	addi r19, r19, %lo(goal_stack)
	stw r19+0, r17
	addi r17, r0, 23
	lui r18, %hi(.LJTI0_0)
	addi r18, r18, %lo(.LJTI0_0)
	lui r1, %hi(ATOM_COMMA)
	addi r1, r1, %lo(ATOM_COMMA)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-100, r1
	lui r1, 4
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-172, r1
	addi r1, r1, -1
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-124, r1
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-80, r1
	lui r1, %hi(g_errmsg)
	addi r1, r1, %lo(g_errmsg)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-132, r1
	lui r1, %hi(.L.str)
	addi r1, r1, %lo(.L.str)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-136, r1
	addi r1, r0, 256
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-140, r1
	addi r1, r0, 26
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-180, r1
	slli r1, r1, 2
	add r1, r18, r1
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-144, r1
	addi r26, r0, 25
	add r1, fp, r0
	addi r1, r1, -2048
	stw r1+-108, r15
	lui r25, %hi(choices)
	addi r25, r25, %lo(choices)
	jal r0, .LBB0_201
.LBB0_200:
	ldw r1, r13+0
	addi r3, r1, 1
	stw r13+0, r3
	slli r1, r1, 2
	add r1, r1, r19
	stw r1+0, r20
.LBB0_201:
	ldw r1, r13+0
	lui r3, %hi(choices+8)
	addi r3, r3, %lo(choices+8)
	add r4, fp, r0
	addi r4, r4, -2048
	stw r4+-96, r3
	lui r3, %hi(choices+20)
	addi r3, r3, %lo(choices+20)
	add r4, fp, r0
	addi r4, r4, -2048
	stw r4+-92, r3
	lui r3, %hi(choices+24)
	addi r3, r3, %lo(choices+24)
	add r4, fp, r0
	addi r4, r4, -2048
	stw r4+-88, r3
	lui r3, %hi(choices+4)
	addi r3, r3, %lo(choices+4)
	add r4, fp, r0
	addi r4, r4, -2048
	stw r4+-84, r3
	blt r1, r12, .LBB0_236
.LBB0_202:
	addi r1, r1, -1
	stw r13+0, r1
	slli r1, r1, 2
	add r1, r1, r19
	ldw r3, r1+0
	jal r31, deref
	add r16, r17, r0
	beq r1, r15, .LBB0_234
.LBB0_203:
	add r20, r1, r0
	andi r1, r1, 3
	beq r1, r15, .LBB0_207
.LBB0_204:
	bne r1, r11, .LBB0_211
.LBB0_205:
	srli r1, r20, 2
	ldw r3, r23+0
	add r16, r17, r0
	beq r1, r3, .LBB0_234
.LBB0_206:
	ldw r3, r24+0
	add r16, r26, r0
	beq r1, r3, .LBB0_234
	jal r0, .LBB0_211
.LBB0_207:
	add r3, r20, r0
	jal r31, compound_functor
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-100
	ldw r3, r3+0
	bne r1, r3, .LBB0_211
.LBB0_208:
	add r3, r20, r0
	jal r31, compound_arity
	bne r1, r11, .LBB0_211
.LBB0_209:
	ldw r1, r13+0
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-124
	blt r1, r3, .LBB0_221
.LBB0_210:
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-80
	stw r1+0, r12
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r3, r1+-132
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r4, r1+-140
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r5, r1+-136
	jal r31, snprintf
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-144
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB0_211:
	add r3, r20, r0
	jal r31, term_functor
	add r21, r1, r0
	add r3, r20, r0
	jal r31, term_arity
	add r3, r20, r0
	add r4, r21, r0
	add r5, r1, r0
	jal r31, try_builtin
	add r3, r15, r0
	add r16, r17, r0
	beq r1, r12, .LBB0_215
.LBB0_212:
	addi r3, r0, -1
	bne r1, r3, .LBB0_214
.LBB0_213:
	addi r16, r0, 25
	add r3, r15, r0
	jal r0, .LBB0_215
.LBB0_214:
	addi r16, r0, 0
	add r3, r12, r0
.LBB0_215:
	beq r3, r15, .LBB0_234
.LBB0_216:
	add r3, r20, r0
	jal r31, term_functor
	add r21, r1, r0
	add r3, r20, r0
	jal r31, term_arity
	add r3, r21, r0
	add r4, r1, r0
	jal r31, db_lookup
	add r16, r26, r0
	beq r1, r15, .LBB0_234
.LBB0_217:
	ldw r27, r1+8
	addi r21, r0, 0
	add r16, r26, r0
	beq r27, r21, .LBB0_234
.LBB0_218:
	ldw r1, r27+12
	beq r1, r21, .LBB0_223
.LBB0_219:
	ldw r3, r28+0
	lui r4, 1
	addi r4, r4, -2048
	blt r3, r4, .LBB0_222
.LBB0_220:
	addi r1, r0, 1
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-80
	stw r3+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r4, r0, 256
	jal r31, snprintf
	addi r1, r0, 26
	slli r1, r1, 2
	add r1, r18, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB0_221:
	add r3, r20, r0
	add r4, r12, r0
	jal r31, compound_arg
	ldw r3, r13+0
	addi r4, r3, 1
	stw r13+0, r4
	slli r3, r3, 2
	add r3, r3, r19
	stw r3+0, r1
	add r3, r20, r0
	add r4, r15, r0
	jal r31, compound_arg
	ldw r3, r13+0
	addi r4, r3, 1
	stw r13+0, r4
	slli r3, r3, 2
	add r3, r3, r19
	stw r3+0, r1
	slli r1, r17, 2
	add r1, r18, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB0_222:
	addi r4, r3, 1
	stw r28+0, r4
	addi r4, r0, 28
	mul r14, r3, r4
	add r3, r14, r25
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-120
	ldw r4, r4+0
	stw r3+0, r4
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-112
	ldw r3, r3+0
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-96
	add r4, r14, r4
	stw r4+0, r3
	lui r3, %hi(choices+12)
	addi r3, r3, %lo(choices+12)
	add r3, r14, r3
	stw r3+0, r1
	lui r1, %hi(choices+16)
	addi r1, r1, %lo(choices+16)
	add r1, r14, r1
	stw r1+0, r20
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-116
	ldw r1, r1+0
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-92
	add r3, r14, r3
	stw r3+0, r1
	jal r31, build_continuation_from_stack
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-88
	add r3, r14, r3
	stw r3+0, r1
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-104
	ldw r1, r1+0
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-84
	add r3, r14, r3
	stw r3+0, r1
.LBB0_223:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r14, r1, -76
	addi r4, r0, -1
	addi r5, r0, 1024
	add r3, r14, r0
	jal r31, memset
	ldw r3, r27+0
	addi r5, r0, 256
	add r4, r14, r0
	jal r31, copy_term_code
	add r22, r1, r0
	ldw r3, r27+4
	add r14, r21, r0
	beq r3, r21, .LBB0_225
.LBB0_224:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r4, r1, -76
	addi r5, r0, 256
	jal r31, copy_term_code
	add r14, r1, r0
.LBB0_225:
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-80
	ldw r1, r1+0
	addi r16, r0, 25
	bne r1, r21, .LBB0_234
.LBB0_226:
	add r3, r20, r0
	add r4, r22, r0
	jal r31, unify
	addi r3, r0, 0
	beq r1, r3, .LBB0_234
.LBB0_227:
	beq r14, r3, .LBB0_233
.LBB0_228:
	andi r1, r14, 3
	bne r1, r11, .LBB0_230
.LBB0_229:
	srli r1, r14, 2
	ldw r4, r23+0
	beq r1, r4, .LBB0_233
.LBB0_230:
	ldw r1, r13+0
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-172
	blt r1, r4, .LBB0_232
.LBB0_231:
	addi r1, r0, 1
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-80
	stw r3+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	addi r4, r0, 256
	jal r31, snprintf
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r16, r1+-180
	jal r0, .LBB0_234
.LBB0_232:
	addi r4, r1, 1
	stw r13+0, r4
	slli r1, r1, 2
	add r1, r1, r19
	stw r1+0, r14
.LBB0_233:
	add r16, r3, r0
.LBB0_234:
	slli r1, r16, 2
	add r1, r18, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB0_235:
	ldw r1, r13+0
	bgt r1, r15, .LBB0_202
.LBB0_236:
	addi r1, r0, 255
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-108
	bgt r3, r1, .LBB0_238
.LBB0_237:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r20, r1, -76
	addi r4, r0, -1
	addi r5, r0, 1024
	add r3, r20, r0
	jal r31, memset
	addi r5, r0, 256
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r3, r1+-128
	add r4, r20, r0
	jal r31, copy_term
	add r3, r1, r0
	jal r31, persist_term
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-108
	addi r14, r3, 1
	slli r3, r3, 2
	addi r4, fp, -1100
	add r3, r4, r3
	stw r3+0, r1
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-80
	ldw r1, r1+0
	addi r3, r0, 0
	add r4, fp, r0
	addi r4, r4, -2048
	stw r4+-108, r14
	bne r1, r3, .LBB0_307
.LBB0_238:
	ldw r1, r28+0
	blt r1, r12, .LBB0_306
.LBB0_239:
	addi r1, r1, -1
	stw r28+0, r1
	addi r14, r0, 28
	mul r20, r1, r14
	add r16, r20, r25
	ldw r3, r16+0
	jal r31, trail_undo
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-84
	add r1, r20, r1
	ldw r1, r1+0
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-104
	stw r3+0, r1
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-96
	add r1, r20, r1
	ldw r3, r1+0
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r1, r4+-112
	stw r1+0, r3
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r1, r4+-92
	add r1, r20, r1
	ldw r1, r1+0
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-116
	stw r4+0, r1
	addi r21, r0, 0
	stw r13+0, r21
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-88
	add r4, r20, r4
	ldw r4, r4+0
	beq r4, r21, .LBB0_241
.LBB0_240:
	stw r13+0, r12
	stw r19+0, r4
.LBB0_241:
	ldw r27, r16+12
	ldw r20, r16+16
	beq r27, r21, .LBB0_200
.LBB0_242:
	ldw r4, r27+12
	beq r4, r21, .LBB0_245
.LBB0_243:
	ldw r5, r28+0
	lui r6, 1
	addi r6, r6, -2048
	bge r5, r6, .LBB0_304
.LBB0_244:
	addi r6, r5, 1
	stw r28+0, r6
	mul r14, r5, r14
	add r5, r14, r25
	add r7, fp, r0
	addi r7, r7, -2048
	ldw r6, r7+-120
	ldw r6, r6+0
	stw r5+0, r6
	add r6, fp, r0
	addi r6, r6, -2048
	ldw r5, r6+-96
	add r5, r14, r5
	stw r5+0, r3
	lui r3, %hi(choices+12)
	addi r3, r3, %lo(choices+12)
	add r3, r14, r3
	stw r3+0, r4
	lui r3, %hi(choices+16)
	addi r3, r3, %lo(choices+16)
	add r3, r14, r3
	stw r3+0, r20
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-92
	add r3, r14, r3
	stw r3+0, r1
	jal r31, build_continuation_from_stack
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-88
	add r3, r14, r3
	stw r3+0, r1
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-104
	ldw r1, r1+0
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-84
	add r3, r14, r3
	stw r3+0, r1
.LBB0_245:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r14, r1, -76
	addi r4, r0, -1
	addi r5, r0, 1024
	add r3, r14, r0
	jal r31, memset
	ldw r3, r27+0
	addi r5, r0, 256
	add r4, r14, r0
	jal r31, copy_term_code
	add r22, r1, r0
	ldw r3, r27+4
	add r14, r21, r0
	beq r3, r21, .LBB0_247
.LBB0_246:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r4, r1, -76
	addi r5, r0, 256
	jal r31, copy_term_code
	add r14, r1, r0
.LBB0_247:
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-80
	ldw r1, r1+0
	bne r1, r21, .LBB0_201
.LBB0_248:
	add r3, r20, r0
	add r4, r22, r0
	jal r31, unify
	addi r3, r0, 0
	beq r1, r3, .LBB0_201
.LBB0_249:
	beq r14, r3, .LBB0_201
.LBB0_250:
	andi r1, r14, 3
	bne r1, r11, .LBB0_252
.LBB0_251:
	srli r1, r14, 2
	ldw r3, r23+0
	beq r1, r3, .LBB0_201
.LBB0_252:
	ldw r1, r13+0
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-172
	bge r1, r3, .LBB0_323
.LBB0_253:
	addi r3, r1, 1
	stw r13+0, r3
	slli r1, r1, 2
	add r1, r1, r19
	stw r1+0, r14
	jal r0, .LBB0_201
.LBB0_254:
	add r3, r12, r0
	add r4, r13, r0
.LBB0_255:
	jal r31, unify
	addi r3, r0, 0
	seq r1, r1, r3
	sub r1, r3, r1
	jal r0, .LBB0_32
.LBB0_256:
	add r14, r12, r0
.LBB0_257:
	add r11, r13, r0
.LBB0_258:
	add r3, r11, r0
	add r4, r14, r0
	jal r31, db_retract
.LBB0_259:
	seq r1, r1, r12
	sub r1, r12, r1
	jal r0, .LBB0_32
.LBB0_260:
	bne r5, r11, .LBB0_264
.LBB0_261:
	lui r3, %hi(ATOM_ATOM_LENGTH)
	addi r3, r3, %lo(ATOM_ATOM_LENGTH)
	ldw r3, r3+0
	bne r4, r3, .LBB0_264
.LBB0_262:
	addi r12, r0, 0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r14, r1, r0
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r13, r1, r0
	andi r3, r14, 3
	add r1, r19, r0
	beq r3, r11, .LBB0_263
	jal r0, .LBB0_20
.LBB0_263:
	srli r3, r14, 2
	jal r31, atom_name
	add r3, r1, r0
	jal r31, strlen
	slli r1, r1, 2
	addi r4, r1, 1
	jal r0, .LBB0_297
.LBB0_264:
	bne r5, r11, .LBB0_271
.LBB0_265:
	lui r3, %hi(ATOM_ATOM_CHARS)
	addi r3, r3, %lo(ATOM_ATOM_CHARS)
	ldw r3, r3+0
	bne r4, r3, .LBB0_271
.LBB0_266:
	addi r12, r0, 0
	add r14, r13, r0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	addi r15, r0, 1
	add r3, r14, r0
	add r4, r15, r0
	jal r31, compound_arg
	add r14, r1, r0
	andi r1, r13, 3
	bne r1, r11, .LBB0_275
.LBB0_267:
	srli r3, r13, 2
	jal r31, atom_name
	add r13, r1, r0
	lui r1, %hi(ATOM_NIL_LIST)
	addi r1, r1, %lo(ATOM_NIL_LIST)
	ldw r1, r1+0
	slli r1, r1, 2
	addi r11, r1, 2
	add r3, r13, r0
	jal r31, strlen
	blt r1, r15, .LBB0_270
.LBB0_268:
	add r12, r1, r0
	addi r16, r13, -1
	addi r13, fp, -1100
	addi r17, r13, 1
	addi r18, r0, 0
.LBB0_269:
	add r19, r12, r0
	addi r12, r12, -1
	add r1, r16, r19
	ldbu r1, r1+0
	stb r13+0, r1
	stb r17+0, r18
	add r3, r13, r0
	jal r31, atom_intern
	slli r1, r1, 2
	addi r3, r1, 2
	add r4, r11, r0
	jal r31, make_list_cons
	add r11, r1, r0
	bgt r19, r15, .LBB0_269
.LBB0_270:
	add r3, r14, r0
	add r4, r11, r0
	jal r0, .LBB0_255
.LBB0_271:
	bne r5, r11, .LBB0_286
.LBB0_272:
	lui r3, %hi(ATOM_CHAR_CODE)
	addi r3, r3, %lo(ATOM_CHAR_CODE)
	ldw r3, r3+0
	bne r4, r3, .LBB0_286
.LBB0_273:
	addi r12, r0, 0
	add r14, r13, r0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	addi r15, r0, 1
	add r3, r14, r0
	add r4, r15, r0
	jal r31, compound_arg
	add r14, r1, r0
	andi r1, r13, 3
	bne r1, r11, .LBB0_293
.LBB0_274:
	srli r3, r13, 2
	jal r31, atom_name
	ldb r1, r1+0
	slli r1, r1, 2
	addi r4, r1, 1
	add r3, r14, r0
	jal r0, .LBB0_298
.LBB0_275:
	add r3, r14, r0
	jal r31, deref
	add r17, r12, r0
	beq r1, r12, .LBB0_285
.LBB0_276:
	add r15, r1, r0
	andi r1, r1, 3
	addi r14, r0, 0
	add r17, r12, r0
	bne r1, r14, .LBB0_285
.LBB0_277:
	lui r18, %hi(ATOM_DOT)
	addi r18, r18, %lo(ATOM_DOT)
	addi r19, r0, 254
	addi r20, fp, -1100
	addi r16, r0, 1
	add r17, r14, r0
.LBB0_278:
	add r3, r15, r0
	jal r31, compound_functor
	ldw r3, r18+0
	bne r1, r3, .LBB0_285
.LBB0_279:
	add r3, r15, r0
	jal r31, compound_arity
	bne r1, r11, .LBB0_285
.LBB0_280:
	bgt r17, r19, .LBB0_285
.LBB0_281:
	add r3, r15, r0
	add r4, r14, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	andi r3, r1, 3
	bne r3, r11, .LBB0_283
.LBB0_282:
	srli r3, r1, 2
	jal r31, atom_name
	ldbu r1, r1+0
	addi r3, r17, 1
	add r4, r20, r17
	stb r4+0, r1
	add r17, r3, r0
.LBB0_283:
	add r3, r15, r0
	add r4, r16, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	beq r1, r14, .LBB0_285
.LBB0_284:
	add r15, r1, r0
	andi r1, r1, 3
	beq r1, r14, .LBB0_278
.LBB0_285:
	addi r3, fp, -1100
	add r1, r3, r17
	jal r0, .LBB0_295
.LBB0_286:
	bne r5, r11, .LBB0_299
.LBB0_287:
	lui r3, %hi(ATOM_NUMBER_CHARS)
	addi r3, r3, %lo(ATOM_NUMBER_CHARS)
	ldw r3, r3+0
	bne r4, r3, .LBB0_299
.LBB0_288:
	addi r11, r0, 0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r14, r1, r0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r13, r1, r0
	andi r3, r14, 3
	add r1, r19, r0
	beq r3, r12, .LBB0_289
	jal r0, .LBB0_20
.LBB0_289:
	srai r6, r14, 2
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	addi r16, fp, -1100
	addi r4, r0, 32
	add r3, r16, r0
	jal r31, snprintf
	lui r1, %hi(ATOM_NIL_LIST)
	addi r1, r1, %lo(ATOM_NIL_LIST)
	ldw r1, r1+0
	slli r1, r1, 2
	addi r14, r1, 2
	add r3, r16, r0
	jal r31, strlen
	blt r1, r12, .LBB0_292
.LBB0_290:
	add r15, r1, r0
	addi r17, r16, -1
	add r1, fp, r0
	addi r1, r1, -2048
	addi r16, r1, -76
	addi r18, r16, 1
.LBB0_291:
	add r19, r15, r0
	addi r15, r15, -1
	add r1, r17, r19
	ldbu r1, r1+0
	stb r16+0, r1
	stb r18+0, r11
	add r3, r16, r0
	jal r31, atom_intern
	slli r1, r1, 2
	addi r3, r1, 2
	add r4, r14, r0
	jal r31, make_list_cons
	add r14, r1, r0
	bgt r19, r12, .LBB0_291
.LBB0_292:
	add r3, r13, r0
	add r4, r14, r0
	jal r0, .LBB0_30
.LBB0_293:
	add r3, r14, r0
	jal r31, deref
	andi r3, r1, 3
	add r1, r19, r0
	beq r3, r15, .LBB0_294
	jal r0, .LBB0_20
.LBB0_294:
	add r3, r14, r0
	jal r31, deref
	srli r1, r1, 2
	addi r3, fp, -1100
	stb r3+0, r1
	addi r1, r3, 1
.LBB0_295:
	stb r1+0, r12
.LBB0_296:
	jal r31, atom_intern
	slli r1, r1, 2
	addi r4, r1, 2
.LBB0_297:
	add r3, r13, r0
.LBB0_298:
	jal r31, unify
	jal r0, .LBB0_259
.LBB0_299:
	bne r5, r17, .LBB0_313
.LBB0_300:
	lui r3, %hi(ATOM_ATOM_CONCAT)
	addi r3, r3, %lo(ATOM_ATOM_CONCAT)
	ldw r3, r3+0
	bne r4, r3, .LBB0_313
.LBB0_301:
	addi r12, r0, 0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r14, r1, r0
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r15, r1, r0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r13, r1, r0
	andi r1, r14, 3
	beq r1, r11, .LBB0_302
	jal r0, .LBB0_5
.LBB0_302:
	andi r3, r15, 3
	add r1, r19, r0
	beq r3, r11, .LBB0_303
	jal r0, .LBB0_20
.LBB0_303:
	srli r3, r14, 2
	jal r31, atom_name
	add r11, r1, r0
	srli r3, r15, 2
	jal r31, atom_name
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	addi r14, fp, -1100
	addi r4, r0, 512
	add r3, r14, r0
	add r6, r11, r0
	add r7, r1, r0
	jal r31, snprintf
	add r3, r14, r0
	jal r0, .LBB0_296
.LBB0_304:
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-80
	stw r1+0, r12
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
.LBB0_305:
	addi r4, r0, 256
	jal r31, snprintf
.LBB0_306:
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r14, r1+-108
.LBB0_307:
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r3, r1+-152
	jal r31, trail_undo
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-104
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-148
	stw r1+0, r3
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-112
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-156
	stw r1+0, r3
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-160
	stw r13+0, r1
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-164
	stw r28+0, r1
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-116
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-168
	stw r1+0, r3
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-80
	ldw r1, r1+0
	addi r11, r0, 0
	beq r1, r11, .LBB0_309
.LBB0_308:
	addi r1, r0, -1
	jal r0, .LBB0_20
.LBB0_309:
	lui r1, %hi(ATOM_NIL_LIST)
	addi r1, r1, %lo(ATOM_NIL_LIST)
	ldw r1, r1+0
	slli r1, r1, 2
	addi r1, r1, 2
	blt r14, r12, .LBB0_312
.LBB0_310:
	addi r13, r14, 1
	slli r3, r14, 2
	addi r4, fp, -1100
	add r3, r3, r4
	addi r14, r3, -4
.LBB0_311:
	ldw r3, r14+0
	add r4, r1, r0
	jal r31, make_list_cons
	addi r13, r13, -1
	addi r14, r14, -4
	bgt r13, r12, .LBB0_311
.LBB0_312:
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-176
	jal r0, .LBB0_29
.LBB0_313:
	bne r5, r11, .LBB0_318
.LBB0_314:
	lui r3, %hi(ATOM_SUCC)
	addi r3, r3, %lo(ATOM_SUCC)
	ldw r3, r3+0
	bne r4, r3, .LBB0_318
.LBB0_315:
	addi r11, r0, 0
	add r14, r13, r0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	add r3, r14, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	andi r1, r13, 3
	bne r1, r12, .LBB0_324
.LBB0_316:
	add r1, r19, r0
	bge r13, r11, .LBB0_317
	jal r0, .LBB0_20
.LBB0_317:
	addi r4, r13, 4
	jal r0, .LBB0_30
.LBB0_318:
	beq r5, r17, .LBB0_319
	jal r0, .LBB0_20
.LBB0_319:
	lui r3, %hi(ATOM_PLUS2)
	addi r3, r3, %lo(ATOM_PLUS2)
	ldw r3, r3+0
	beq r4, r3, .LBB0_320
	jal r0, .LBB0_20
.LBB0_320:
	addi r11, r0, 0
	add r15, r13, r0
	add r3, r13, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	add r3, r15, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r14, r1, r0
	addi r4, r0, 2
	add r3, r15, r0
	jal r31, compound_arg
	add r3, r1, r0
	andi r1, r13, 3
	beq r1, r12, .LBB0_321
	jal r0, .LBB0_5
.LBB0_321:
	andi r4, r14, 3
	add r1, r19, r0
	beq r4, r12, .LBB0_322
	jal r0, .LBB0_20
.LBB0_322:
	addi r1, r0, -4
	and r1, r13, r1
	add r4, r14, r1
	jal r0, .LBB0_30
.LBB0_323:
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-80
	stw r1+0, r12
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	jal r0, .LBB0_305
.LBB0_324:
	jal r31, deref
	add r3, r1, r0
	addi r1, r0, 4
	bge r3, r1, .LBB0_325
	jal r0, .LBB0_5
.LBB0_325:
	andi r4, r3, 3
	add r1, r19, r0
	beq r4, r12, .LBB0_326
	jal r0, .LBB0_20
.LBB0_326:
	addi r4, r3, -4
	add r3, r13, r0
	jal r0, .LBB0_30
.LBB0_327:
.Lfunc_end0:
	.size	try_builtin, .Lfunc_end0-try_builtin
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI0_0:
	.word	.LBB0_235
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_327
	.word	.LBB0_235
	.word	.LBB0_327
	.word	.LBB0_238
	.word	.LBB0_306
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function eval_arith
	.type	eval_arith,@function
eval_arith:                             # @eval_arith
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
	add r11, r4, r0
	jal r31, deref
	andi r3, r1, 3
	addi r17, r0, 2
	beq r3, r17, .LBB1_3
.LBB1_1:
	addi r16, r0, 1
	bne r3, r16, .LBB1_4
.LBB1_2:
	srai r1, r1, 2
	stw r11+0, r1
	addi r1, r0, 1
	jal r0, .LBB1_23
.LBB1_3:
	lui r3, %hi(g_error)
	addi r3, r3, %lo(g_error)
	addi r4, r0, 1
	stw r3+0, r4
	srli r3, r1, 2
	jal r31, atom_name
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.4)
	addi r5, r5, %lo(.L.str.4)
	addi r4, r0, 256
	add r6, r1, r0
	jal r31, snprintf
	addi r1, r0, 0
	jal r0, .LBB1_23
.LBB1_4:
	addi r6, r0, 0
	beq r1, r6, .LBB1_11
.LBB1_5:
	addi r15, r0, 0
	bne r3, r15, .LBB1_11
.LBB1_6:
	add r3, r1, r0
	add r13, r1, r0
	jal r31, compound_functor
	add r12, r1, r0
	add r14, r13, r0
	add r3, r13, r0
	jal r31, compound_arity
	beq r1, r17, .LBB1_13
.LBB1_7:
	bne r1, r16, .LBB1_17
.LBB1_8:
	addi r15, r0, 0
	add r3, r14, r0
	add r4, r15, r0
	jal r31, compound_arg
	addi r13, fp, -36
	add r3, r1, r0
	add r4, r13, r0
	jal r31, eval_arith
	add r3, r1, r0
	add r1, r15, r0
	beq r3, r15, .LBB1_23
.LBB1_9:
	lui r3, %hi(ATOM_MINUS)
	addi r3, r3, %lo(ATOM_MINUS)
	ldw r3, r3+0
	bne r12, r3, .LBB1_20
.LBB1_10:
	ldw r1, r13+0
	addi r3, r0, 0
	sub r1, r3, r1
	jal r0, .LBB1_22
.LBB1_11:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	stw r1+0, r16
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.5)
	addi r5, r5, %lo(.L.str.5)
	addi r4, r0, 256
	add r11, r6, r0
.LBB1_12:
	jal r31, snprintf
	add r1, r11, r0
	jal r0, .LBB1_23
.LBB1_13:
	addi r15, r0, 0
	add r3, r14, r0
	add r4, r15, r0
	jal r31, compound_arg
	addi r13, fp, -40
	add r3, r1, r0
	add r4, r13, r0
	jal r31, eval_arith
	add r3, r1, r0
	add r1, r15, r0
	beq r3, r15, .LBB1_23
.LBB1_14:
	addi r4, r0, 1
	add r3, r14, r0
	add r16, r4, r0
	jal r31, compound_arg
	addi r14, fp, -44
	add r3, r1, r0
	add r4, r14, r0
	jal r31, eval_arith
	addi r15, r0, 0
	beq r1, r15, .LBB1_19
.LBB1_15:
	lui r1, %hi(ATOM_PLUS)
	addi r1, r1, %lo(ATOM_PLUS)
	ldw r1, r1+0
	bne r12, r1, .LBB1_24
.LBB1_16:
	ldw r1, r13+0
	ldw r3, r14+0
	add r1, r3, r1
	jal r0, .LBB1_22
.LBB1_17:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	stw r1+0, r16
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
	addi r4, r0, 256
.LBB1_18:
	jal r31, snprintf
.LBB1_19:
	add r1, r15, r0
	jal r0, .LBB1_23
.LBB1_20:
	lui r3, %hi(ATOM_ABS)
	addi r3, r3, %lo(ATOM_ABS)
	ldw r3, r3+0
	bne r12, r3, .LBB1_26
.LBB1_21:
	ldw r1, r13+0
	srai r3, r1, 31
	xor r1, r1, r3
	sub r1, r1, r3
.LBB1_22:
	stw r11+0, r1
	add r1, r16, r0
.LBB1_23:
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
.LBB1_24:
	lui r1, %hi(ATOM_MINUS)
	addi r1, r1, %lo(ATOM_MINUS)
	ldw r3, r1+0
	add r1, r16, r0
	bne r12, r3, .LBB1_27
.LBB1_25:
	ldw r3, r13+0
	ldw r4, r14+0
	sub r3, r3, r4
	stw r11+0, r3
	jal r0, .LBB1_23
.LBB1_26:
	lui r3, %hi(g_error)
	addi r3, r3, %lo(g_error)
	stw r3+0, r16
	add r3, r12, r0
	add r11, r1, r0
	jal r31, atom_name
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.6)
	addi r5, r5, %lo(.L.str.6)
	addi r4, r0, 256
	add r6, r1, r0
	jal r0, .LBB1_12
.LBB1_27:
	lui r3, %hi(ATOM_STAR)
	addi r3, r3, %lo(ATOM_STAR)
	ldw r3, r3+0
	bne r12, r3, .LBB1_29
.LBB1_28:
	ldw r3, r13+0
	ldw r4, r14+0
	mul r3, r4, r3
	stw r11+0, r3
	jal r0, .LBB1_23
.LBB1_29:
	lui r3, %hi(ATOM_SLASH2)
	addi r3, r3, %lo(ATOM_SLASH2)
	ldw r3, r3+0
	bne r12, r3, .LBB1_32
.LBB1_30:
	ldw r3, r14+0
	addi r12, r0, 0
	beq r3, r12, .LBB1_35
.LBB1_31:
	ldw r4, r13+0
	div r3, r4, r3
	stw r11+0, r3
	jal r0, .LBB1_23
.LBB1_32:
	lui r3, %hi(ATOM_MOD)
	addi r3, r3, %lo(ATOM_MOD)
	ldw r3, r3+0
	bne r12, r3, .LBB1_36
.LBB1_33:
	ldw r3, r14+0
	addi r12, r0, 0
	beq r3, r12, .LBB1_35
.LBB1_34:
	ldw r4, r13+0
	rem r3, r4, r3
	stw r11+0, r3
	jal r0, .LBB1_23
.LBB1_35:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.7)
	addi r5, r5, %lo(.L.str.7)
	addi r4, r0, 256
	jal r31, snprintf
	add r1, r12, r0
	jal r0, .LBB1_23
.LBB1_36:
	lui r3, %hi(ATOM_MIN)
	addi r3, r3, %lo(ATOM_MIN)
	ldw r3, r3+0
	bne r12, r3, .LBB1_38
.LBB1_37:
	ldw r3, r13+0
	ldw r4, r14+0
	xor r5, r3, r4
	slt r3, r3, r4
	jal r0, .LBB1_40
.LBB1_38:
	lui r3, %hi(ATOM_MAX)
	addi r3, r3, %lo(ATOM_MAX)
	ldw r3, r3+0
	bne r12, r3, .LBB1_41
.LBB1_39:
	ldw r3, r13+0
	ldw r4, r14+0
	xor r5, r3, r4
	sgt r3, r3, r4
.LBB1_40:
	addi r6, r0, 0
	sub r3, r6, r3
	and r3, r5, r3
	xor r3, r4, r3
	stw r11+0, r3
	jal r0, .LBB1_23
.LBB1_41:
	lui r3, %hi(g_error)
	addi r3, r3, %lo(g_error)
	stw r3+0, r1
	add r3, r12, r0
	jal r31, atom_name
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	addi r4, r0, 256
	add r6, r1, r0
	jal r0, .LBB1_18
.Lfunc_end1:
	.size	eval_arith, .Lfunc_end1-eval_arith
                                        # -- End function
	.p2align	2                               # -- Begin function build_continuation_from_stack
	.type	build_continuation_from_stack,@function
build_continuation_from_stack:          # @build_continuation_from_stack
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
	lui r13, %hi(goal_sp)
	addi r13, r13, %lo(goal_sp)
	ldw r1, r13+0
	addi r14, r0, 1
	blt r1, r14, .LBB2_4
.LBB2_1:
	lui r1, %hi(goal_stack)
	addi r1, r1, %lo(goal_stack)
	ldw r1, r1+0
	ldw r3, r13+0
	addi r4, r0, 2
	blt r3, r4, .LBB2_5
.LBB2_2:
	lui r15, %hi(goal_stack+4)
	addi r15, r15, %lo(goal_stack+4)
	addi r11, fp, -36
	lui r16, %hi(ATOM_COMMA)
	addi r16, r16, %lo(ATOM_COMMA)
	addi r12, r0, 2
.LBB2_3:
	ldw r3, r15+0
	stw r11+0, r3
	stw r11+4, r1
	ldw r3, r16+0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, make_compound
	addi r14, r14, 1
	ldw r3, r13+0
	addi r15, r15, 4
	blt r14, r3, .LBB2_3
	jal r0, .LBB2_5
.LBB2_4:
	addi r1, r0, 0
.LBB2_5:
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
.Lfunc_end2:
	.size	build_continuation_from_stack, .Lfunc_end2-build_continuation_from_stack
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"goal stack overflow"
	.size	.L.str, 20

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"choice stack overflow"
	.size	.L.str.1, 22

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"%d"
	.size	.L.str.2, 3

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"%s%s"
	.size	.L.str.3, 5

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"arithmetic: not evaluable: %s/0"
	.size	.L.str.4, 32

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"arithmetic: invalid expression"
	.size	.L.str.5, 31

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"arithmetic: unknown op %s/1"
	.size	.L.str.6, 28

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"arithmetic: division by zero"
	.size	.L.str.7, 29

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"arithmetic: unknown op %s/2"
	.size	.L.str.8, 28

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"arithmetic: bad expression"
	.size	.L.str.9, 27

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
