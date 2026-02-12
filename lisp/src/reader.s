	.file	"reader.c"
	.text
	.globl	reader_init                     # -- Begin function reader_init
	.p2align	2
	.type	reader_init,@function
reader_init:                            # @reader_init
# %bb.0:
	lui r1, %hi(has_peek)
	addi r1, r1, %lo(has_peek)
	addi r3, r0, 0
	stb r1+0, r3
	lui r1, %hi(peek_ch)
	addi r1, r1, %lo(peek_ch)
	stw r1+0, r3
	jalr r0, r31, 0
.Lfunc_end0:
	.size	reader_init, .Lfunc_end0-reader_init
                                        # -- End function
	.globl	lisp_read                       # -- Begin function lisp_read
	.p2align	2
	.type	lisp_read,@function
lisp_read:                              # @lisp_read
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r1, r0, 0
	stw r3+0, r1
	jal r31, lisp_read_expr
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	lisp_read, .Lfunc_end1-lisp_read
                                        # -- End function
	.p2align	2                               # -- Begin function lisp_read_expr
	.type	lisp_read_expr,@function
lisp_read_expr:                         # @lisp_read_expr
# %bb.0:
	addi sp, sp, -360
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 360
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
	jal r31, skip_whitespace_and_comments
	lui r19, %hi(has_peek)
	addi r19, r19, %lo(has_peek)
	ldbu r1, r19+0
	addi r11, r0, 0
	lui r20, %hi(peek_ch)
	addi r20, r20, %lo(peek_ch)
	bne r1, r11, .LBB2_2
.LBB2_1:
	jal r31, getchar
	stw r20+0, r1
	addi r1, r0, 1
	stb r19+0, r1
.LBB2_2:
	ldw r1, r20+0
	addi r22, r0, -1
	ble r1, r22, .LBB2_5
.LBB2_3:
	ldbu r3, r19+0
	addi r21, r0, 1
	bne r3, r21, .LBB2_6
.LBB2_4:
	addi r3, r0, 0
	stb r19+0, r3
	jal r0, .LBB2_7
.LBB2_5:
	addi r1, r0, 1
	stw r12+0, r1
	addi r12, r0, 0
	jal r0, .LBB2_122
.LBB2_6:
	jal r31, getchar
.LBB2_7:
	addi r3, r1, -34
	addi r4, r0, 6
	bgtu r3, r4, .LBB2_76
.LBB2_8:
	slli r3, r3, 2
	lui r4, %hi(.LJTI2_0)
	addi r4, r4, %lo(.LJTI2_0)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB2_9:
	addi r11, r0, 256
	add r3, r11, r0
	jal r31, malloc
	addi r16, r0, 0
	beq r1, r16, .LBB2_97
.LBB2_10:
	add r13, r1, r0
	addi r17, r0, 2
	addi r18, r0, 34
	addi r22, r0, 92
	addi r23, r0, 10
	addi r24, r0, 110
	addi r25, r0, 116
	addi r26, r0, 9
	lui r14, %hi(.L.str.5)
	addi r14, r14, %lo(.L.str.5)
	add r12, r16, r0
	jal r0, .LBB2_13
.LBB2_11:
	add r1, r17, r0
.LBB2_12:
	bne r1, r16, .LBB2_86
.LBB2_13:
	ldbu r1, r19+0
	bne r1, r21, .LBB2_15
.LBB2_14:
	stb r19+0, r16
	ldw r15, r20+0
	bge r15, r16, .LBB2_16
	jal r0, .LBB2_11
.LBB2_15:
	jal r31, getchar
	add r15, r1, r0
	blt r15, r16, .LBB2_11
.LBB2_16:
	beq r15, r18, .LBB2_11
.LBB2_17:
	bne r15, r22, .LBB2_20
.LBB2_18:
	ldbu r1, r19+0
	bne r1, r21, .LBB2_22
.LBB2_19:
	stb r19+0, r16
	ldw r1, r20+0
	add r15, r23, r0
	bne r1, r24, .LBB2_23
.LBB2_20:
	addi r27, r12, 1
	bge r27, r11, .LBB2_25
.LBB2_21:
	add r3, r13, r0
	jal r0, .LBB2_27
.LBB2_22:
	jal r31, getchar
	add r15, r23, r0
	beq r1, r24, .LBB2_20
.LBB2_23:
	bne r1, r25, .LBB2_30
.LBB2_24:
	add r15, r26, r0
	addi r27, r12, 1
	blt r27, r11, .LBB2_21
.LBB2_25:
	slli r11, r11, 1
	add r3, r13, r0
	add r4, r11, r0
	jal r31, realloc
	add r3, r1, r0
	beq r1, r16, .LBB2_28
.LBB2_26:
	beq r1, r16, .LBB2_29
.LBB2_27:
	add r1, r3, r12
	stb r1+0, r15
	add r12, r27, r0
	add r13, r3, r0
	add r1, r16, r0
	jal r0, .LBB2_12
.LBB2_28:
	add r3, r13, r0
	add r28, r1, r0
	jal r31, free
	add r3, r14, r0
	jal r31, lisp_error
	add r1, r28, r0
	add r3, r13, r0
	bne r1, r16, .LBB2_27
.LBB2_29:
	add r13, r3, r0
	add r1, r21, r0
	jal r0, .LBB2_12
.LBB2_30:
	add r15, r1, r0
	addi r27, r12, 1
	blt r27, r11, .LBB2_21
	jal r0, .LBB2_25
.LBB2_31:
	addi r12, r0, 0
	addi r11, fp, -332
	stw r11+0, r12
	add r3, r11, r0
	jal r31, push_root_checked
	addi r3, fp, -336
	stw r3+0, r12
	jal r31, lisp_read_expr
	stw r11+0, r1
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	ldw r1, r1+0
	beq r1, r12, .LBB2_96
.LBB2_32:
	lui r1, %hi(root_sp)
	addi r1, r1, %lo(root_sp)
	ldw r3, r1+0
	addi r3, r3, -1
	stw r1+0, r3
	jal r0, .LBB2_122
.LBB2_33:
	ldbu r1, r19+0
	bne r1, r21, .LBB2_88
.LBB2_34:
	addi r1, r0, 0
	stb r19+0, r1
	ldw r1, r20+0
	addi r3, r0, 116
	beq r1, r3, .LBB2_89
.LBB2_35:
	addi r3, r0, 102
	bne r1, r3, .LBB2_99
.LBB2_36:
	addi r12, r0, 0
	ldbu r1, r19+0
	bne r1, r12, .LBB2_41
	jal r0, .LBB2_39
.LBB2_37:
	ldbu r1, r19+0
	bne r1, r21, .LBB2_40
.LBB2_38:
	stb r19+0, r12
	ldbu r1, r19+0
	bne r1, r12, .LBB2_41
.LBB2_39:
	jal r31, getchar
	stw r20+0, r1
	stb r19+0, r21
	jal r0, .LBB2_41
.LBB2_40:
	jal r31, getchar
	ldbu r1, r19+0
	beq r1, r12, .LBB2_39
.LBB2_41:
	ldw r3, r20+0
	jal r31, is_symbol_char
	bne r1, r12, .LBB2_37
	jal r0, .LBB2_122
.LBB2_42:
	addi r13, fp, -332
	stw r13+0, r11
	addi r14, fp, -336
	stw r14+0, r11
	add r3, r13, r0
	jal r31, push_root_checked
	add r3, r14, r0
	jal r31, push_root_checked
	lui r15, %hi(.L.str.3)
	addi r15, r15, %lo(.L.str.3)
	lui r23, %hi(root_sp)
	addi r23, r23, %lo(root_sp)
	addi r24, r0, 46
	addi r25, r0, 32
	lui r1, %hi(.L.str.4)
	addi r1, r1, %lo(.L.str.4)
	stw fp+-348, r1
	lui r26, %hi(.LJTI2_1)
	addi r26, r26, %lo(.LJTI2_1)
	addi r17, fp, -340
	lui r27, %hi(g_error)
	addi r27, r27, %lo(g_error)
	addi r28, r0, 41
	addi r18, fp, -344
                                        # implicit-def: $r12
	jal r0, .LBB2_45
.LBB2_43:
	jal r31, getchar
.LBB2_44:
	ldw r1, r23+0
	addi r1, r1, -2
	stw r23+0, r1
	ldw r12, r13+0
	beq r11, r11, .LBB2_122
.LBB2_45:
	jal r31, skip_whitespace_and_comments
	ldbu r1, r19+0
	bne r1, r11, .LBB2_47
.LBB2_46:
	jal r31, getchar
	stw r20+0, r1
	stb r19+0, r21
.LBB2_47:
	ldw r1, r20+0
	ble r1, r22, .LBB2_52
.LBB2_48:
	beq r1, r24, .LBB2_54
.LBB2_49:
	bne r1, r28, .LBB2_56
.LBB2_50:
	ldbu r1, r19+0
	bne r1, r21, .LBB2_43
.LBB2_51:
	stb r19+0, r11
	jal r0, .LBB2_44
.LBB2_52:
	add r3, r15, r0
.LBB2_53:
	jal r31, lisp_error
	jal r0, .LBB2_63
.LBB2_54:
	ldbu r1, r19+0
	bne r1, r21, .LBB2_58
.LBB2_55:
	stb r19+0, r11
	ldbu r1, r19+0
	beq r1, r11, .LBB2_59
	jal r0, .LBB2_60
.LBB2_56:
	stw r17+0, r11
	add r3, r17, r0
	jal r31, lisp_read_expr
	stw r18+0, r1
	ldw r16, r27+0
	beq r16, r11, .LBB2_64
.LBB2_57:
	ldw r1, r23+0
	addi r1, r1, -2
	stw r23+0, r1
	add r12, r11, r0
	jal r0, .LBB2_71
.LBB2_58:
	jal r31, getchar
	ldbu r1, r19+0
	bne r1, r11, .LBB2_60
.LBB2_59:
	jal r31, getchar
	stw r20+0, r1
	stb r19+0, r21
.LBB2_60:
	ldw r1, r20+0
	addi r1, r1, -9
	bgtu r1, r25, .LBB2_68
.LBB2_61:
	slli r1, r1, 2
	add r1, r26, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB2_62:
	stw r17+0, r11
	add r3, r17, r0
	jal r31, lisp_read_expr
	ldw r3, r27+0
	beq r3, r11, .LBB2_66
.LBB2_63:
	ldw r1, r23+0
	addi r1, r1, -2
	stw r23+0, r1
	add r12, r11, r0
	bne r11, r11, .LBB2_45
	jal r0, .LBB2_122
.LBB2_64:
	add r3, r18, r0
	jal r31, push_root_checked
	ldw r3, r18+0
	add r4, r11, r0
	jal r31, cons_alloc
	ldw r3, r23+0
	addi r3, r3, -1
	stw r23+0, r3
	ldw r3, r13+0
	beq r3, r11, .LBB2_69
.LBB2_65:
	ldw r3, r14+0
	stw r3+16, r1
	jal r0, .LBB2_70
.LBB2_66:
	ldw r3, r14+0
	beq r3, r11, .LBB2_72
.LBB2_67:
	stw r3+16, r1
	jal r0, .LBB2_73
.LBB2_68:
	ldw r3, fp+-348
	jal r0, .LBB2_53
.LBB2_69:
	stw r13+0, r1
.LBB2_70:
	stw r14+0, r1
.LBB2_71:
	seq r1, r16, r11
	bne r1, r11, .LBB2_45
	jal r0, .LBB2_122
.LBB2_72:
	stw r13+0, r1
.LBB2_73:
	jal r31, skip_whitespace_and_comments
	ldbu r1, r19+0
	bne r1, r11, .LBB2_75
.LBB2_74:
	jal r31, getchar
	stw r20+0, r1
	stb r19+0, r21
.LBB2_75:
	ldw r1, r20+0
	bne r1, r28, .LBB2_44
	jal r0, .LBB2_50
.LBB2_76:
	add r11, r1, r0
	add r3, r1, r0
	jal r31, is_symbol_char
	addi r12, r0, 0
	beq r1, r12, .LBB2_98
.LBB2_77:
	addi r13, fp, -332
	stb r13+0, r11
	addi r15, r0, 1
	addi r16, r0, 255
	add r14, r15, r0
	jal r0, .LBB2_80
.LBB2_78:
	jal r31, getchar
	add r11, r1, r0
.LBB2_79:
	addi r1, r14, 1
	add r3, r13, r14
	stb r3+0, r11
	add r14, r1, r0
.LBB2_80:
	ldbu r1, r19+0
	bne r1, r12, .LBB2_82
.LBB2_81:
	jal r31, getchar
	stw r20+0, r1
	stb r19+0, r15
.LBB2_82:
	ldw r11, r20+0
	add r3, r11, r0
	jal r31, is_symbol_char
	beq r1, r12, .LBB2_102
.LBB2_83:
	beq r14, r16, .LBB2_105
.LBB2_84:
	ldbu r1, r19+0
	bne r1, r15, .LBB2_78
.LBB2_85:
	stb r19+0, r12
	jal r0, .LBB2_79
.LBB2_86:
	addi r3, r0, 2
	bne r1, r3, .LBB2_101
.LBB2_87:
	add r1, r13, r12
	addi r3, r0, 0
	stb r1+0, r3
	add r3, r13, r0
	add r4, r12, r0
	jal r31, string_alloc
	add r12, r1, r0
	add r3, r13, r0
	jal r31, free
	jal r0, .LBB2_122
.LBB2_88:
	jal r31, getchar
	addi r3, r0, 116
	bne r1, r3, .LBB2_35
.LBB2_89:
	addi r11, r0, 0
	ldbu r1, r19+0
	bne r1, r11, .LBB2_94
	jal r0, .LBB2_92
.LBB2_90:
	ldbu r1, r19+0
	bne r1, r21, .LBB2_93
.LBB2_91:
	stb r19+0, r11
	ldbu r1, r19+0
	bne r1, r11, .LBB2_94
.LBB2_92:
	jal r31, getchar
	stw r20+0, r1
	stb r19+0, r21
	jal r0, .LBB2_94
.LBB2_93:
	jal r31, getchar
	ldbu r1, r19+0
	beq r1, r11, .LBB2_92
.LBB2_94:
	ldw r3, r20+0
	jal r31, is_symbol_char
	bne r1, r11, .LBB2_90
.LBB2_95:
	lui r1, %hi(sym_true)
	addi r1, r1, %lo(sym_true)
	ldw r12, r1+0
	jal r0, .LBB2_122
.LBB2_96:
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	jal r31, symbol_intern
	addi r12, fp, -340
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	ldw r3, r11+0
	addi r4, r0, 0
	jal r31, cons_alloc
	addi r11, fp, -344
	stw r11+0, r1
	add r3, r11, r0
	jal r31, push_root_checked
	ldw r3, r12+0
	ldw r4, r11+0
	jal r31, cons_alloc
	add r12, r1, r0
	lui r1, %hi(root_sp)
	addi r1, r1, %lo(root_sp)
	ldw r3, r1+0
	addi r3, r3, -3
	stw r1+0, r3
	jal r0, .LBB2_122
.LBB2_97:
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	jal r0, .LBB2_100
.LBB2_98:
	lui r3, %hi(.L.str.2)
	addi r3, r3, %lo(.L.str.2)
	jal r0, .LBB2_106
.LBB2_99:
	lui r3, %hi(.L.str.1)
	addi r3, r3, %lo(.L.str.1)
.LBB2_100:
	jal r31, lisp_error
.LBB2_101:
	addi r12, r0, 0
	jal r0, .LBB2_122
.LBB2_102:
	add r1, r13, r14
	addi r4, r0, 0
	stb r1+0, r4
	ldbu r1, r13+0
	addi r3, r0, 45
	beq r1, r3, .LBB2_104
.LBB2_103:
	addi r5, r0, 43
	bne r1, r5, .LBB2_107
.LBB2_104:
	addi r4, r0, 1
	sne r5, r14, r4
	jal r0, .LBB2_108
.LBB2_105:
	lui r3, %hi(.L.str.6)
	addi r3, r3, %lo(.L.str.6)
.LBB2_106:
	jal r31, lisp_error
	jal r0, .LBB2_122
.LBB2_107:
	addi r5, r0, 1
.LBB2_108:
	bgeu r4, r14, .LBB2_113
.LBB2_109:
	addi r6, r0, 0
	addi r7, r0, 245
.LBB2_110:
	add r8, r13, r4
	ldbu r8, r8+0
	addi r8, r8, -58
	andi r8, r8, 255
	bleu r8, r7, .LBB2_112
.LBB2_111:
	addi r4, r4, 1
	bne r14, r4, .LBB2_110
	jal r0, .LBB2_113
.LBB2_112:
	add r5, r6, r0
.LBB2_113:
	addi r4, r0, 0
	beq r5, r4, .LBB2_117
.LBB2_114:
	addi r5, r0, 1
	bne r14, r5, .LBB2_118
.LBB2_115:
	addi r5, r0, 43
	beq r1, r5, .LBB2_117
.LBB2_116:
	bne r1, r3, .LBB2_118
.LBB2_117:
	addi r3, fp, -332
	jal r31, symbol_intern
	add r12, r1, r0
	jal r0, .LBB2_122
.LBB2_118:
	seq r3, r1, r3
	addi r5, r0, 43
	seq r1, r1, r5
	or  r1, r3, r1
	add r5, r4, r0
	bleu r14, r1, .LBB2_121
.LBB2_119:
	addi r5, r0, 0
	addi r6, r0, 10
.LBB2_120:
	mul r5, r5, r6
	add r7, r13, r1
	ldb r7, r7+0
	add r5, r5, r7
	addi r5, r5, -48
	addi r1, r1, 1
	bne r14, r1, .LBB2_120
.LBB2_121:
	sub r1, r4, r5
	sub r3, r4, r3
	xor r1, r1, r5
	and r1, r1, r3
	xor r1, r5, r1
	slli r1, r1, 1
	addi r12, r1, 1
.LBB2_122:
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
	addi sp, sp, 360
	jalr r0, r31, 0
.Lfunc_end2:
	.size	lisp_read_expr, .Lfunc_end2-lisp_read_expr
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI2_0:
	.word	.LBB2_9
	.word	.LBB2_33
	.word	.LBB2_76
	.word	.LBB2_76
	.word	.LBB2_76
	.word	.LBB2_31
	.word	.LBB2_42
.LJTI2_1:
	.word	.LBB2_62
	.word	.LBB2_62
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_62
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_62
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_68
	.word	.LBB2_62
	.word	.LBB2_62
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function skip_whitespace_and_comments
	.type	skip_whitespace_and_comments,@function
skip_whitespace_and_comments:           # @skip_whitespace_and_comments
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
	lui r11, %hi(has_peek)
	addi r11, r11, %lo(has_peek)
	addi r12, r0, 0
	lui r13, %hi(peek_ch)
	addi r13, r13, %lo(peek_ch)
	addi r14, r0, 23
	addi r15, r0, 59
	addi r16, r0, 1
	addi r17, r0, 10
	lui r1, 2048
	addi r18, r1, 19
	jal r0, .LBB3_2
.LBB3_1:
	jal r31, getchar
.LBB3_2:
	ldbu r1, r11+0
	bne r1, r12, .LBB3_4
.LBB3_3:
	jal r31, getchar
	stw r13+0, r1
	stb r11+0, r16
.LBB3_4:
	ldw r1, r13+0
	blt r1, r12, .LBB3_16
.LBB3_5:
	addi r3, r1, -9
	bgtu r3, r14, .LBB3_9
.LBB3_6:
	sll r3, r16, r3
	and r3, r3, r18
	beq r3, r12, .LBB3_9
.LBB3_7:
	ldbu r1, r11+0
	bne r1, r16, .LBB3_1
.LBB3_8:
	stb r11+0, r12
	jal r0, .LBB3_2
.LBB3_9:
	bne r1, r15, .LBB3_16
.LBB3_10:
	ldbu r1, r11+0
	bne r1, r16, .LBB3_12
.LBB3_11:
	stb r11+0, r12
	ldbu r1, r11+0
	beq r1, r12, .LBB3_13
	jal r0, .LBB3_14
.LBB3_12:
	jal r31, getchar
	ldbu r1, r11+0
	bne r1, r12, .LBB3_14
.LBB3_13:
	jal r31, getchar
	stw r13+0, r1
	stb r11+0, r16
.LBB3_14:
	ldw r1, r13+0
	blt r1, r12, .LBB3_2
.LBB3_15:
	bne r1, r17, .LBB3_10
	jal r0, .LBB3_2
.LBB3_16:
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
.Lfunc_end3:
	.size	skip_whitespace_and_comments, .Lfunc_end3-skip_whitespace_and_comments
                                        # -- End function
	.p2align	2                               # -- Begin function is_symbol_char
	.type	is_symbol_char,@function
is_symbol_char:                         # @is_symbol_char
# %bb.0:
	addi r4, r3, -48
	addi r1, r0, 1
	addi r5, r0, 10
	bltu r4, r5, .LBB4_4
.LBB4_1:
	addi r4, r0, -33
	and r4, r3, r4
	addi r4, r4, -65
	addi r5, r0, 26
	bltu r4, r5, .LBB4_4
.LBB4_2:
	addi r3, r3, -33
	addi r4, r0, 62
	bgtu r3, r4, .LBB4_5
.LBB4_3:
	slli r3, r3, 2
	lui r4, %hi(.LJTI4_0)
	addi r4, r4, %lo(.LJTI4_0)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB4_4:
	jalr r0, r31, 0
.LBB4_5:
	addi r1, r0, 0
	jal r0, .LBB4_4
.Lfunc_end4:
	.size	is_symbol_char, .Lfunc_end4-is_symbol_char
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI4_0:
	.word	.LBB4_4
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_5
	.word	.LBB4_4
	.word	.LBB4_5
	.word	.LBB4_4
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_4
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_5
	.word	.LBB4_4
                                        # -- End function
	.type	has_peek,@object                # @has_peek
	.local	has_peek
	.comm	has_peek,1,4
	.type	peek_ch,@object                 # @peek_ch
	.local	peek_ch
	.comm	peek_ch,4,4
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"quote"
	.size	.L.str, 6

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"unknown # syntax"
	.size	.L.str.1, 17

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"unexpected character"
	.size	.L.str.2, 21

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"unexpected EOF in list"
	.size	.L.str.3, 23

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"unexpected '.'"
	.size	.L.str.4, 15

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"out of memory"
	.size	.L.str.5, 14

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"symbol too long"
	.size	.L.str.6, 16

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
