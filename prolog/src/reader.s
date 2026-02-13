	.file	"reader.c"
	.text
	.globl	reader_init                     # -- Begin function reader_init
	.p2align	2
	.type	reader_init,@function
reader_init:                            # @reader_init
# %bb.0:
	lui r1, %hi(peek_char)
	addi r1, r1, %lo(peek_char)
	addi r3, r0, -2
	stw r1+0, r3
	lui r1, %hi(cur_tok)
	addi r1, r1, %lo(cur_tok)
	addi r3, r0, 11
	stw r1+0, r3
	jalr r0, r31, 0
.Lfunc_end0:
	.size	reader_init, .Lfunc_end0-reader_init
                                        # -- End function
	.globl	next_token                      # -- Begin function next_token
	.p2align	2
	.type	next_token,@function
next_token:                             # @next_token
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
	lui r12, %hi(peek_char)
	addi r12, r12, %lo(peek_char)
	addi r13, r0, -2
	addi r18, r0, 47
	addi r19, r0, 42
	addi r20, r0, 0
	addi r16, r0, -1
	lui r14, %hi(cur_tok)
	addi r14, r14, %lo(cur_tok)
	addi r21, r0, 11
	addi r22, r0, 37
	addi r15, r0, 10
	addi r17, r0, 95
	addi r23, r0, 91
	lui r24, %hi(.LJTI1_0)
	addi r24, r24, %lo(.LJTI1_0)
	jal r0, .LBB1_2
.LBB1_1:
	stw r14+0, r21
	beq r1, r16, .LBB1_209
.LBB1_2:
	ldw r1, r12+0
	bne r1, r13, .LBB1_4
.LBB1_3:
	jal r31, getchar
	stw r12+0, r1
.LBB1_4:
	ldw r1, r12+0
	beq r1, r18, .LBB1_14
.LBB1_5:
	beq r1, r22, .LBB1_10
.LBB1_6:
	add r3, r1, r0
	jal r31, isspace
	ldw r3, r12+0
	beq r1, r20, .LBB1_22
.LBB1_7:
	bne r3, r13, .LBB1_34
.LBB1_8:
	jal r31, getchar
	jal r0, .LBB1_2
.LBB1_9:
	stw r12+0, r13
.LBB1_10:
	beq r1, r16, .LBB1_2
.LBB1_11:
	beq r1, r15, .LBB1_2
.LBB1_12:
	ldw r1, r12+0
	bne r1, r13, .LBB1_9
.LBB1_13:
	jal r31, getchar
	bne r1, r16, .LBB1_11
	jal r0, .LBB1_2
.LBB1_14:
	stw r12+0, r13
	jal r31, getchar
	stw r12+0, r1
	bne r1, r19, .LBB1_36
.LBB1_15:
	stw r12+0, r13
	add r1, r20, r0
.LBB1_16:
	add r11, r1, r0
	ldw r1, r12+0
	bne r1, r13, .LBB1_18
.LBB1_17:
	jal r31, getchar
	bne r1, r16, .LBB1_19
	jal r0, .LBB1_1
.LBB1_18:
	stw r12+0, r13
	beq r1, r16, .LBB1_1
.LBB1_19:
	bne r11, r19, .LBB1_16
.LBB1_20:
	bne r1, r18, .LBB1_16
.LBB1_21:
	bne r1, r16, .LBB1_2
	jal r0, .LBB1_209
.LBB1_22:
	bne r3, r13, .LBB1_24
.LBB1_23:
	jal r31, getchar
	stw r12+0, r1
.LBB1_24:
	ldw r11, r12+0
	beq r11, r16, .LBB1_45
.LBB1_25:
	add r3, r11, r0
	jal r31, isdigit
	bne r1, r20, .LBB1_46
.LBB1_26:
	add r3, r11, r0
	jal r31, isupper
	beq r11, r17, .LBB1_58
.LBB1_27:
	bne r1, r20, .LBB1_58
.LBB1_28:
	add r3, r11, r0
	jal r31, islower
	bne r1, r20, .LBB1_71
.LBB1_29:
	addi r1, r11, -33
	bgtu r1, r23, .LBB1_31
.LBB1_30:
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB1_31:
	add r3, r11, r0
	jal r31, is_op_char
	bne r1, r20, .LBB1_109
.LBB1_32:
	ldw r1, r12+0
	bne r1, r13, .LBB1_35
.LBB1_33:
	jal r31, getchar
	jal r0, .LBB1_2
.LBB1_34:
	stw r12+0, r13
	jal r0, .LBB1_2
.LBB1_35:
	stw r12+0, r13
	jal r0, .LBB1_2
.LBB1_36:
	addi r15, r0, 0
	stw r14+0, r15
	lui r1, %hi(cur_tok+4)
	addi r1, r1, %lo(cur_tok+4)
	stb r1+0, r18
	addi r16, r0, 5
	addi r17, r0, 254
	jal r0, .LBB1_39
.LBB1_37:
	jal r31, getchar
	add r11, r1, r0
.LBB1_38:
	add r1, r16, r14
	stb r1+0, r11
	addi r16, r16, 1
.LBB1_39:
	ldw r1, r12+0
	bne r1, r13, .LBB1_41
.LBB1_40:
	jal r31, getchar
	stw r12+0, r1
.LBB1_41:
	ldw r11, r12+0
	add r3, r11, r0
	jal r31, is_op_char
	beq r1, r15, .LBB1_208
.LBB1_42:
	addi r1, r16, -4
	bgtu r1, r17, .LBB1_208
.LBB1_43:
	beq r11, r13, .LBB1_37
.LBB1_44:
	stw r12+0, r13
	jal r0, .LBB1_38
.LBB1_45:
	stw r14+0, r21
	jal r0, .LBB1_209
.LBB1_46:
	addi r1, r0, 2
	stw r14+0, r1
	addi r16, r0, 0
	addi r17, r0, 254
	lui r11, %hi(cur_tok+4)
	addi r11, r11, %lo(cur_tok+4)
	add r14, r16, r0
	jal r0, .LBB1_49
.LBB1_47:
	jal r31, getchar
.LBB1_48:
	addi r3, r14, 1
	add r4, r14, r11
	stb r4+0, r1
	add r14, r3, r0
.LBB1_49:
	ldw r1, r12+0
	bne r1, r13, .LBB1_51
.LBB1_50:
	jal r31, getchar
	stw r12+0, r1
.LBB1_51:
	ldw r3, r12+0
	jal r31, isdigit
	beq r1, r16, .LBB1_55
.LBB1_52:
	bgtu r14, r17, .LBB1_55
.LBB1_53:
	ldw r1, r12+0
	beq r1, r13, .LBB1_47
.LBB1_54:
	stw r12+0, r13
	jal r0, .LBB1_48
.LBB1_55:
	add r3, r14, r11
	addi r1, r0, 0
	stb r3+0, r1
	lui r3, %hi(cur_tok+260)
	addi r3, r3, %lo(cur_tok+260)
	stw r3+0, r1
	beq r14, r1, .LBB1_209
.LBB1_56:
	add r4, r1, r0
.LBB1_57:
	mul r4, r4, r15
	add r5, r1, r11
	ldb r5, r5+0
	add r4, r4, r5
	addi r4, r4, -48
	stw r3+0, r4
	addi r1, r1, 1
	bne r14, r1, .LBB1_57
	jal r0, .LBB1_209
.LBB1_58:
	addi r1, r0, 1
	stw r14+0, r1
	addi r15, r0, 4
	addi r11, r0, 0
	addi r16, r0, 255
	addi r18, r0, 254
	jal r0, .LBB1_61
.LBB1_59:
	jal r31, getchar
.LBB1_60:
	add r3, r15, r14
	stb r3+0, r1
	addi r15, r15, 1
.LBB1_61:
	ldw r1, r12+0
	bne r1, r13, .LBB1_63
.LBB1_62:
	jal r31, getchar
	stw r12+0, r1
.LBB1_63:
	addi r19, r15, -4
	ldw r3, r12+0
	jal r31, isalnum
	beq r1, r11, .LBB1_65
.LBB1_64:
	bleu r19, r18, .LBB1_69
	jal r0, .LBB1_84
.LBB1_65:
	ldw r1, r12+0
	bne r1, r13, .LBB1_67
.LBB1_66:
	jal r31, getchar
	stw r12+0, r1
.LBB1_67:
	ldw r1, r12+0
	bne r1, r17, .LBB1_84
.LBB1_68:
	bgeu r19, r16, .LBB1_84
.LBB1_69:
	ldw r1, r12+0
	beq r1, r13, .LBB1_59
.LBB1_70:
	stw r12+0, r13
	jal r0, .LBB1_60
.LBB1_71:
	addi r11, r0, 0
	stw r14+0, r11
	addi r15, r0, 4
	addi r16, r0, 255
	addi r18, r0, 254
	jal r0, .LBB1_74
.LBB1_72:
	jal r31, getchar
.LBB1_73:
	add r3, r15, r14
	stb r3+0, r1
	addi r15, r15, 1
.LBB1_74:
	ldw r1, r12+0
	bne r1, r13, .LBB1_76
.LBB1_75:
	jal r31, getchar
	stw r12+0, r1
.LBB1_76:
	addi r19, r15, -4
	ldw r3, r12+0
	jal r31, isalnum
	beq r1, r11, .LBB1_78
.LBB1_77:
	bleu r19, r18, .LBB1_82
	jal r0, .LBB1_84
.LBB1_78:
	ldw r1, r12+0
	bne r1, r13, .LBB1_80
.LBB1_79:
	jal r31, getchar
	stw r12+0, r1
.LBB1_80:
	ldw r1, r12+0
	bne r1, r17, .LBB1_84
.LBB1_81:
	bgeu r19, r16, .LBB1_84
.LBB1_82:
	ldw r1, r12+0
	beq r1, r13, .LBB1_72
.LBB1_83:
	stw r12+0, r13
	jal r0, .LBB1_73
.LBB1_84:
	add r1, r15, r14
	stb r1+0, r11
	jal r0, .LBB1_209
.LBB1_85:
	ldw r1, r12+0
	bne r1, r13, .LBB1_118
.LBB1_86:
	jal r31, getchar
	jal r0, .LBB1_119
.LBB1_87:
	ldw r1, r12+0
	bne r1, r13, .LBB1_120
.LBB1_88:
	jal r31, getchar
	jal r0, .LBB1_121
.LBB1_89:
	ldw r1, r12+0
	bne r1, r13, .LBB1_122
.LBB1_90:
	jal r31, getchar
	jal r0, .LBB1_123
.LBB1_91:
	ldw r1, r12+0
	bne r1, r13, .LBB1_139
.LBB1_92:
	jal r31, getchar
	jal r0, .LBB1_140
.LBB1_93:
	ldw r1, r12+0
	bne r1, r13, .LBB1_141
.LBB1_94:
	jal r31, getchar
	jal r0, .LBB1_142
.LBB1_95:
	ldw r1, r12+0
	bne r1, r13, .LBB1_144
.LBB1_96:
	jal r31, getchar
	jal r0, .LBB1_145
.LBB1_97:
	ldw r1, r12+0
	bne r1, r13, .LBB1_160
.LBB1_98:
	jal r31, getchar
	jal r0, .LBB1_161
.LBB1_99:
	ldw r1, r12+0
	bne r1, r13, .LBB1_166
.LBB1_100:
	jal r31, getchar
	jal r0, .LBB1_167
.LBB1_101:
	ldw r1, r12+0
	bne r1, r13, .LBB1_182
.LBB1_102:
	jal r31, getchar
	jal r0, .LBB1_183
.LBB1_103:
	ldw r1, r12+0
	bne r1, r13, .LBB1_184
.LBB1_104:
	jal r31, getchar
	jal r0, .LBB1_185
.LBB1_105:
	ldw r1, r12+0
	bne r1, r13, .LBB1_186
.LBB1_106:
	jal r31, getchar
	jal r0, .LBB1_187
.LBB1_107:
	ldw r1, r12+0
	bne r1, r13, .LBB1_188
.LBB1_108:
	jal r31, getchar
	jal r0, .LBB1_189
.LBB1_109:
	addi r15, r0, 0
	stw r14+0, r15
	addi r16, r0, 4
	addi r17, r0, 254
	jal r0, .LBB1_112
.LBB1_110:
	jal r31, getchar
	add r11, r1, r0
.LBB1_111:
	add r1, r16, r14
	stb r1+0, r11
	addi r16, r16, 1
.LBB1_112:
	ldw r1, r12+0
	bne r1, r13, .LBB1_114
.LBB1_113:
	jal r31, getchar
	stw r12+0, r1
.LBB1_114:
	ldw r11, r12+0
	add r3, r11, r0
	jal r31, is_op_char
	beq r1, r15, .LBB1_208
.LBB1_115:
	addi r1, r16, -4
	bgtu r1, r17, .LBB1_208
.LBB1_116:
	beq r11, r13, .LBB1_110
.LBB1_117:
	stw r12+0, r13
	jal r0, .LBB1_111
.LBB1_118:
	stw r12+0, r13
.LBB1_119:
	addi r1, r0, 6
	stw r14+0, r1
	jal r0, .LBB1_209
.LBB1_120:
	stw r12+0, r13
.LBB1_121:
	addi r1, r0, 0
	stw r14+0, r1
	addi r1, r0, 59
	jal r0, .LBB1_143
.LBB1_122:
	stw r12+0, r13
.LBB1_123:
	ldw r1, r12+0
	bne r1, r13, .LBB1_125
.LBB1_124:
	jal r31, getchar
	stw r12+0, r1
.LBB1_125:
	ldw r3, r12+0
	jal r31, isdigit
	addi r16, r0, 0
	beq r1, r16, .LBB1_190
.LBB1_126:
	addi r1, r0, 2
	stw r14+0, r1
	addi r17, r0, 254
	lui r11, %hi(cur_tok+4)
	addi r11, r11, %lo(cur_tok+4)
	add r14, r16, r0
	jal r0, .LBB1_129
.LBB1_127:
	jal r31, getchar
.LBB1_128:
	addi r3, r14, 1
	add r4, r14, r11
	stb r4+0, r1
	add r14, r3, r0
.LBB1_129:
	ldw r1, r12+0
	bne r1, r13, .LBB1_131
.LBB1_130:
	jal r31, getchar
	stw r12+0, r1
.LBB1_131:
	ldw r3, r12+0
	jal r31, isdigit
	beq r1, r16, .LBB1_135
.LBB1_132:
	bgtu r14, r17, .LBB1_135
.LBB1_133:
	ldw r1, r12+0
	beq r1, r13, .LBB1_127
.LBB1_134:
	stw r12+0, r13
	jal r0, .LBB1_128
.LBB1_135:
	add r1, r14, r11
	addi r3, r0, 0
	stb r1+0, r3
	lui r1, %hi(cur_tok+260)
	addi r1, r1, %lo(cur_tok+260)
	stw r1+0, r3
	beq r14, r3, .LBB1_138
.LBB1_136:
	add r4, r3, r0
.LBB1_137:
	mul r4, r4, r15
	add r5, r3, r11
	ldb r5, r5+0
	add r4, r4, r5
	addi r4, r4, -48
	stw r1+0, r4
	addi r3, r3, 1
	bne r14, r3, .LBB1_137
.LBB1_138:
	ldw r3, r1+0
	addi r4, r0, 0
	sub r3, r4, r3
	stw r1+0, r3
	jal r0, .LBB1_209
.LBB1_139:
	stw r12+0, r13
.LBB1_140:
	addi r1, r0, 3
	stw r14+0, r1
	jal r0, .LBB1_209
.LBB1_141:
	stw r12+0, r13
.LBB1_142:
	addi r1, r0, 0
	stw r14+0, r1
	addi r1, r0, 33
.LBB1_143:
	lui r3, %hi(cur_tok+4)
	addi r3, r3, %lo(cur_tok+4)
	sth r3+0, r1
	jal r0, .LBB1_209
.LBB1_144:
	stw r12+0, r13
.LBB1_145:
	stw r14+0, r15
	addi r11, r0, 0
	addi r14, r0, 92
	addi r17, r0, 110
	addi r18, r0, 116
	addi r19, r0, 9
	addi r21, r0, 254
	lui r20, %hi(cur_tok+4)
	addi r20, r20, %lo(cur_tok+4)
	addi r22, r0, 34
	ldw r1, r12+0
	beq r1, r13, .LBB1_149
	jal r0, .LBB1_153
.LBB1_146:
	add r1, r3, r0
.LBB1_147:
	bleu r11, r21, .LBB1_152
.LBB1_148:
	ldw r1, r12+0
	bne r1, r13, .LBB1_153
.LBB1_149:
	jal r31, getchar
	beq r1, r14, .LBB1_154
.LBB1_150:
	beq r1, r16, .LBB1_159
.LBB1_151:
	bne r1, r22, .LBB1_147
	jal r0, .LBB1_159
.LBB1_152:
	addi r3, r11, 1
	add r4, r11, r20
	stb r4+0, r1
	add r11, r3, r0
	ldw r1, r12+0
	beq r1, r13, .LBB1_149
.LBB1_153:
	stw r12+0, r13
	bne r1, r14, .LBB1_150
.LBB1_154:
	ldw r3, r12+0
	bne r3, r13, .LBB1_156
.LBB1_155:
	jal r31, getchar
	add r3, r1, r0
	add r1, r15, r0
	bne r3, r17, .LBB1_157
	jal r0, .LBB1_147
.LBB1_156:
	stw r12+0, r13
	add r1, r15, r0
	beq r3, r17, .LBB1_147
.LBB1_157:
	bne r3, r18, .LBB1_146
.LBB1_158:
	add r1, r19, r0
	jal r0, .LBB1_147
.LBB1_159:
	add r1, r11, r20
	jal r0, .LBB1_181
.LBB1_160:
	stw r12+0, r13
.LBB1_161:
	ldw r1, r12+0
	bne r1, r13, .LBB1_163
.LBB1_162:
	jal r31, getchar
	stw r12+0, r1
.LBB1_163:
	ldw r3, r12+0
	beq r3, r16, .LBB1_165
.LBB1_164:
	jal r31, isspace
	addi r15, r0, 0
	beq r1, r15, .LBB1_199
.LBB1_165:
	addi r1, r0, 7
	stw r14+0, r1
	jal r0, .LBB1_209
.LBB1_166:
	stw r12+0, r13
.LBB1_167:
	addi r11, r0, 0
	stw r14+0, r11
	addi r14, r0, 39
	addi r18, r0, 255
	lui r17, %hi(cur_tok+4)
	addi r17, r17, %lo(cur_tok+4)
	addi r19, r0, 254
.LBB1_168:
	ldw r1, r12+0
	bne r1, r13, .LBB1_173
.LBB1_169:
	jal r31, getchar
	beq r1, r14, .LBB1_174
.LBB1_170:
	beq r1, r16, .LBB1_180
.LBB1_171:
	beq r1, r15, .LBB1_180
.LBB1_172:
	bgtu r11, r19, .LBB1_168
	jal r0, .LBB1_179
.LBB1_173:
	stw r12+0, r13
	bne r1, r14, .LBB1_170
.LBB1_174:
	ldw r1, r12+0
	bne r1, r13, .LBB1_176
.LBB1_175:
	jal r31, getchar
	stw r12+0, r1
.LBB1_176:
	ldw r1, r12+0
	bne r1, r14, .LBB1_180
.LBB1_177:
	stw r12+0, r13
	bgeu r11, r18, .LBB1_168
.LBB1_178:
	add r1, r14, r0
.LBB1_179:
	addi r3, r11, 1
	add r4, r11, r17
	stb r4+0, r1
	add r11, r3, r0
	jal r0, .LBB1_168
.LBB1_180:
	add r1, r11, r17
.LBB1_181:
	addi r3, r0, 0
	stb r1+0, r3
	jal r0, .LBB1_209
.LBB1_182:
	stw r12+0, r13
.LBB1_183:
	addi r1, r0, 4
	stw r14+0, r1
	jal r0, .LBB1_209
.LBB1_184:
	stw r12+0, r13
.LBB1_185:
	addi r1, r0, 8
	stw r14+0, r1
	jal r0, .LBB1_209
.LBB1_186:
	stw r12+0, r13
.LBB1_187:
	addi r1, r0, 5
	stw r14+0, r1
	jal r0, .LBB1_209
.LBB1_188:
	stw r12+0, r13
.LBB1_189:
	addi r1, r0, 9
	stw r14+0, r1
	jal r0, .LBB1_209
.LBB1_190:
	addi r15, r0, 0
	stw r14+0, r15
	addi r1, r0, 45
	lui r3, %hi(cur_tok+4)
	addi r3, r3, %lo(cur_tok+4)
	stb r3+0, r1
	addi r16, r0, 5
	addi r17, r0, 254
	jal r0, .LBB1_193
.LBB1_191:
	jal r31, getchar
	add r11, r1, r0
.LBB1_192:
	add r1, r16, r14
	stb r1+0, r11
	addi r16, r16, 1
.LBB1_193:
	ldw r1, r12+0
	bne r1, r13, .LBB1_195
.LBB1_194:
	jal r31, getchar
	stw r12+0, r1
.LBB1_195:
	ldw r11, r12+0
	add r3, r11, r0
	jal r31, is_op_char
	beq r1, r15, .LBB1_208
.LBB1_196:
	addi r1, r16, -4
	bgtu r1, r17, .LBB1_208
.LBB1_197:
	beq r11, r13, .LBB1_191
.LBB1_198:
	stw r12+0, r13
	jal r0, .LBB1_192
.LBB1_199:
	stw r14+0, r15
	addi r1, r0, 46
	lui r3, %hi(cur_tok+4)
	addi r3, r3, %lo(cur_tok+4)
	stb r3+0, r1
	addi r16, r0, 5
	addi r17, r0, 254
	jal r0, .LBB1_202
.LBB1_200:
	jal r31, getchar
	add r11, r1, r0
.LBB1_201:
	add r1, r16, r14
	stb r1+0, r11
	addi r16, r16, 1
.LBB1_202:
	ldw r1, r12+0
	bne r1, r13, .LBB1_204
.LBB1_203:
	jal r31, getchar
	stw r12+0, r1
.LBB1_204:
	ldw r11, r12+0
	add r3, r11, r0
	jal r31, is_op_char
	beq r1, r15, .LBB1_208
.LBB1_205:
	addi r1, r16, -4
	bgtu r1, r17, .LBB1_208
.LBB1_206:
	beq r11, r13, .LBB1_200
.LBB1_207:
	stw r12+0, r13
	jal r0, .LBB1_201
.LBB1_208:
	add r1, r16, r14
	stb r1+0, r15
.LBB1_209:
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
.Lfunc_end1:
	.size	next_token, .Lfunc_end1-next_token
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI1_0:
	.word	.LBB1_93
	.word	.LBB1_95
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_99
	.word	.LBB1_91
	.word	.LBB1_101
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_103
	.word	.LBB1_89
	.word	.LBB1_97
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_87
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_105
	.word	.LBB1_31
	.word	.LBB1_85
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_31
	.word	.LBB1_107
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function is_op_char
	.type	is_op_char,@function
is_op_char:                             # @is_op_char
# %bb.0:
	addi r4, r3, -35
	addi r1, r0, 57
	bgtu r4, r1, .LBB2_3
.LBB2_1:
	addi r1, r0, 1
	slli r4, r4, 2
	lui r5, %hi(.LJTI2_0)
	addi r5, r5, %lo(.LJTI2_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB2_2:
	jalr r0, r31, 0
.LBB2_3:
	addi r1, r0, 94
	seq r1, r3, r1
	jal r0, .LBB2_2
.Lfunc_end2:
	.size	is_op_char, .Lfunc_end2-is_op_char
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI2_0:
	.word	.LBB2_2
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_2
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_2
	.word	.LBB2_2
	.word	.LBB2_3
	.word	.LBB2_2
	.word	.LBB2_2
	.word	.LBB2_2
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_2
	.word	.LBB2_3
	.word	.LBB2_2
	.word	.LBB2_2
	.word	.LBB2_2
	.word	.LBB2_2
	.word	.LBB2_2
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_3
	.word	.LBB2_2
                                        # -- End function
	.type	peek_char,@object               # @peek_char
	.data
	.p2align	2, 0x0
peek_char:
	.word	4294967294                      # 0xfffffffe
	.size	peek_char, 4

	.type	cur_tok,@object                 # @cur_tok
	.bss
	.globl	cur_tok
	.p2align	2, 0x0
cur_tok:
	.zero	264
	.size	cur_tok, 264

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
