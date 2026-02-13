	.file	"parser.c"
	.text
	.globl	parser_init                     # -- Begin function parser_init
	.p2align	2
	.type	parser_init,@function
parser_init:                            # @parser_init
# %bb.0:
	lui r1, %hi(clause_var_count)
	addi r1, r1, %lo(clause_var_count)
	addi r3, r0, 0
	stw r1+0, r3
	lui r1, %hi(clause_max_var)
	addi r1, r1, %lo(clause_max_var)
	stw r1+0, r3
	jalr r0, r31, 0
.Lfunc_end0:
	.size	parser_init, .Lfunc_end0-parser_init
                                        # -- End function
	.globl	init_operators                  # -- Begin function init_operators
	.p2align	2
	.type	init_operators,@function
init_operators:                         # @init_operators
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
	lui r11, %hi(op_count)
	addi r11, r11, %lo(op_count)
	addi r17, r0, 0
	stw r11+0, r17
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	jal r31, atom_intern
	ldw r3, r11+0
	addi r12, r0, 12
	mul r4, r3, r12
	lui r13, %hi(ops)
	addi r13, r13, %lo(ops)
	add r5, r4, r13
	stw r5+0, r1
	lui r14, %hi(ops+4)
	addi r14, r14, %lo(ops+4)
	add r1, r4, r14
	addi r16, r0, 1200
	stw r1+0, r16
	lui r15, %hi(ops+8)
	addi r15, r15, %lo(ops+8)
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	addi r1, r0, 62
	bgt r3, r1, .LBB1_2
.LBB1_1:
	lui r3, %hi(.L.str.1)
	addi r3, r3, %lo(.L.str.1)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	stw r1+0, r16
	add r1, r4, r15
	addi r4, r0, 3
	stw r1+0, r4
	addi r1, r3, 1
	stw r11+0, r1
.LBB1_2:
	ldw r1, r11+0
	addi r16, r0, 63
	ble r1, r16, .LBB1_25
.LBB1_3:
	ldw r1, r11+0
	ble r1, r16, .LBB1_26
.LBB1_4:
	ldw r1, r11+0
	ble r1, r16, .LBB1_27
.LBB1_5:
	ldw r1, r11+0
	ble r1, r16, .LBB1_28
.LBB1_6:
	ldw r1, r11+0
	ble r1, r16, .LBB1_29
.LBB1_7:
	ldw r1, r11+0
	ble r1, r16, .LBB1_30
.LBB1_8:
	ldw r1, r11+0
	ble r1, r16, .LBB1_31
.LBB1_9:
	ldw r1, r11+0
	ble r1, r16, .LBB1_32
.LBB1_10:
	ldw r1, r11+0
	ble r1, r16, .LBB1_33
.LBB1_11:
	ldw r1, r11+0
	ble r1, r16, .LBB1_34
.LBB1_12:
	ldw r1, r11+0
	ble r1, r16, .LBB1_35
.LBB1_13:
	ldw r1, r11+0
	ble r1, r16, .LBB1_36
.LBB1_14:
	ldw r1, r11+0
	ble r1, r16, .LBB1_37
.LBB1_15:
	ldw r1, r11+0
	ble r1, r16, .LBB1_38
.LBB1_16:
	ldw r1, r11+0
	ble r1, r16, .LBB1_39
.LBB1_17:
	ldw r1, r11+0
	ble r1, r16, .LBB1_40
.LBB1_18:
	ldw r1, r11+0
	ble r1, r16, .LBB1_41
.LBB1_19:
	ldw r1, r11+0
	ble r1, r16, .LBB1_42
.LBB1_20:
	ldw r1, r11+0
	ble r1, r16, .LBB1_43
.LBB1_21:
	ldw r1, r11+0
	ble r1, r16, .LBB1_44
.LBB1_22:
	ldw r1, r11+0
	bgt r1, r16, .LBB1_24
.LBB1_23:
	lui r3, %hi(.L.str.22)
	addi r3, r3, %lo(.L.str.22)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 400
	stw r1+0, r5
	add r1, r4, r15
	addi r4, r0, 2
	stw r1+0, r4
	addi r1, r3, 1
	stw r11+0, r1
.LBB1_24:
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
.LBB1_25:
	lui r3, %hi(.L.str.2)
	addi r3, r3, %lo(.L.str.2)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 1100
	stw r1+0, r5
	add r1, r4, r15
	addi r4, r0, 1
	stw r1+0, r4
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_4
.LBB1_26:
	lui r3, %hi(.L.str.3)
	addi r3, r3, %lo(.L.str.3)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 1050
	stw r1+0, r5
	add r1, r4, r15
	addi r4, r0, 1
	stw r1+0, r4
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_5
.LBB1_27:
	lui r3, %hi(.L.str.4)
	addi r3, r3, %lo(.L.str.4)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 1000
	stw r1+0, r5
	add r1, r4, r15
	addi r4, r0, 1
	stw r1+0, r4
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_6
.LBB1_28:
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 900
	stw r1+0, r5
	add r1, r4, r15
	addi r4, r0, 4
	stw r1+0, r4
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_7
.LBB1_29:
	lui r3, %hi(.L.str.6)
	addi r3, r3, %lo(.L.str.6)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_8
.LBB1_30:
	lui r3, %hi(.L.str.7)
	addi r3, r3, %lo(.L.str.7)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_9
.LBB1_31:
	lui r3, %hi(.L.str.8)
	addi r3, r3, %lo(.L.str.8)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_10
.LBB1_32:
	lui r3, %hi(.L.str.9)
	addi r3, r3, %lo(.L.str.9)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_11
.LBB1_33:
	lui r3, %hi(.L.str.10)
	addi r3, r3, %lo(.L.str.10)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_12
.LBB1_34:
	lui r3, %hi(.L.str.11)
	addi r3, r3, %lo(.L.str.11)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_13
.LBB1_35:
	lui r3, %hi(.L.str.12)
	addi r3, r3, %lo(.L.str.12)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_14
.LBB1_36:
	lui r3, %hi(.L.str.13)
	addi r3, r3, %lo(.L.str.13)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_15
.LBB1_37:
	lui r3, %hi(.L.str.14)
	addi r3, r3, %lo(.L.str.14)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_16
.LBB1_38:
	lui r3, %hi(.L.str.15)
	addi r3, r3, %lo(.L.str.15)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_17
.LBB1_39:
	lui r3, %hi(.L.str.16)
	addi r3, r3, %lo(.L.str.16)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_18
.LBB1_40:
	lui r3, %hi(.L.str.17)
	addi r3, r3, %lo(.L.str.17)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 700
	stw r1+0, r5
	add r1, r4, r15
	stw r1+0, r17
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_19
.LBB1_41:
	lui r3, %hi(.L.str.18)
	addi r3, r3, %lo(.L.str.18)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 500
	stw r1+0, r5
	add r1, r4, r15
	addi r4, r0, 2
	stw r1+0, r4
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_20
.LBB1_42:
	lui r3, %hi(.L.str.19)
	addi r3, r3, %lo(.L.str.19)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 500
	stw r1+0, r5
	add r1, r4, r15
	addi r4, r0, 2
	stw r1+0, r4
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_21
.LBB1_43:
	lui r3, %hi(.L.str.20)
	addi r3, r3, %lo(.L.str.20)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 400
	stw r1+0, r5
	add r1, r4, r15
	addi r4, r0, 2
	stw r1+0, r4
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	bgt r1, r16, .LBB1_22
.LBB1_44:
	lui r3, %hi(.L.str.21)
	addi r3, r3, %lo(.L.str.21)
	jal r31, atom_intern
	ldw r3, r11+0
	mul r4, r3, r12
	add r5, r4, r13
	stw r5+0, r1
	add r1, r4, r14
	addi r5, r0, 400
	stw r1+0, r5
	add r1, r4, r15
	addi r4, r0, 2
	stw r1+0, r4
	addi r1, r3, 1
	stw r11+0, r1
	ldw r1, r11+0
	ble r1, r16, .LBB1_23
	jal r0, .LBB1_24
.Lfunc_end1:
	.size	init_operators, .Lfunc_end1-init_operators
                                        # -- End function
	.globl	parse_term                      # -- Begin function parse_term
	.p2align	2
	.type	parse_term,@function
parse_term:                             # @parse_term
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
	lui r1, %hi(clause_var_count)
	addi r1, r1, %lo(clause_var_count)
	addi r11, r0, 0
	stw r1+0, r11
	lui r12, %hi(clause_max_var)
	addi r12, r12, %lo(clause_max_var)
	stw r12+0, r11
	jal r31, next_token
	lui r13, %hi(cur_tok)
	addi r13, r13, %lo(cur_tok)
	ldw r1, r13+0
	addi r3, r0, 11
	beq r1, r3, .LBB2_5
.LBB2_1:
	addi r3, r0, 1200
	jal r31, parse_expr
	lui r3, %hi(g_error)
	addi r3, r3, %lo(g_error)
	ldw r4, r3+0
	addi r11, r0, 0
	bne r4, r11, .LBB2_5
.LBB2_2:
	ldw r4, r13+0
	addi r5, r0, 7
	bne r4, r5, .LBB2_4
.LBB2_3:
	ldw r3, r12+0
	lui r4, %hi(var_count)
	addi r4, r4, %lo(var_count)
	stw r4+0, r3
	add r11, r1, r0
	jal r0, .LBB2_5
.LBB2_4:
	addi r1, r0, 1
	stw r3+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.23)
	addi r5, r5, %lo(.L.str.23)
	addi r4, r0, 256
	jal r31, snprintf
.LBB2_5:
	add r1, r11, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	parse_term, .Lfunc_end2-parse_term
                                        # -- End function
	.p2align	2                               # -- Begin function parse_expr
	.type	parse_expr,@function
parse_expr:                             # @parse_expr
# %bb.0:
	addi sp, sp, -216
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 216
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
	stw fp+-68, lr
	add r12, r3, r0
	lui r18, %hi(g_error)
	addi r18, r18, %lo(g_error)
	ldw r1, r18+0
	addi r11, r0, 0
	add r13, r11, r0
	beq r1, r11, .LBB3_22
.LBB3_1:
	ldw r1, r18+0
	bne r1, r11, .LBB3_31
.LBB3_2:
	lui r19, %hi(cur_tok)
	addi r19, r19, %lo(cur_tok)
	addi r20, r0, -9
	addi r11, r0, 0
	addi r21, r0, 8
	lui r22, %hi(ATOM_COMMA)
	addi r22, r22, %lo(ATOM_COMMA)
	lui r23, %hi(op_count)
	addi r23, r23, %lo(op_count)
	addi r24, r0, 1
	lui r25, %hi(ops)
	addi r25, r25, %lo(ops)
	addi r14, r0, 2
	addi r15, fp, -196
	lui r16, %hi(cur_tok+4)
	addi r16, r16, %lo(cur_tok+4)
	jal r0, .LBB3_5
.LBB3_3:
	add r1, r14, r0
.LBB3_4:
	bne r1, r11, .LBB3_29
.LBB3_5:
	ldw r1, r19+0
	and r3, r1, r20
	bne r3, r11, .LBB3_30
.LBB3_6:
	bne r1, r21, .LBB3_8
.LBB3_7:
	ldw r17, r22+0
	jal r0, .LBB3_9
.LBB3_8:
	add r3, r16, r0
	jal r31, atom_intern
	add r17, r1, r0
.LBB3_9:
	ldw r1, r23+0
	add r26, r11, r0
	blt r1, r24, .LBB3_16
.LBB3_10:
	add r26, r25, r0
	jal r0, .LBB3_12
.LBB3_11:
	addi r1, r1, -1
	addi r26, r26, 12
	beq r1, r11, .LBB3_15
.LBB3_12:
	ldw r3, r26+0
	bne r3, r17, .LBB3_11
.LBB3_13:
	ldw r3, r26+8
	bgt r3, r14, .LBB3_11
.LBB3_14:
	ldw r3, r26+4
	bgt r3, r12, .LBB3_11
	jal r0, .LBB3_16
.LBB3_15:
	add r26, r11, r0
.LBB3_16:
	beq r26, r11, .LBB3_3
.LBB3_17:
	jal r31, next_token
	ldw r1, r26+8
	ldw r3, r26+4
	beq r1, r24, .LBB3_19
.LBB3_18:
	addi r3, r3, -1
.LBB3_19:
	jal r31, parse_expr
	ldw r3, r18+0
	beq r3, r11, .LBB3_21
.LBB3_20:
	add r1, r24, r0
	jal r0, .LBB3_4
.LBB3_21:
	stw r15+0, r13
	stw r15+4, r1
	add r3, r17, r0
	add r4, r14, r0
	add r5, r15, r0
	jal r31, make_compound
	add r13, r1, r0
	add r1, r11, r0
	jal r0, .LBB3_4
.LBB3_22:
	lui r17, %hi(cur_tok)
	addi r17, r17, %lo(cur_tok)
	ldw r1, r17+0
	addi r3, r0, 11
	bgtu r1, r3, .LBB3_32
.LBB3_23:
	slli r1, r1, 2
	lui r3, %hi(.LJTI3_0)
	addi r3, r3, %lo(.LJTI3_0)
	add r1, r3, r1
	ldw r1, r1+0
	add r13, r11, r0
	jalr r0, r1, 0
.LBB3_24:
	lui r3, %hi(cur_tok+4)
	addi r3, r3, %lo(cur_tok+4)
	jal r31, atom_intern
	add r14, r1, r0
	jal r31, next_token
	lui r1, %hi(op_count)
	addi r1, r1, %lo(op_count)
	ldw r3, r1+0
	addi r15, r0, 1
	blt r3, r15, .LBB3_58
.LBB3_25:
	lui r1, %hi(ops)
	addi r1, r1, %lo(ops)
	addi r5, r0, 2
	addi r4, r0, 0
	jal r0, .LBB3_27
.LBB3_26:
	addi r3, r3, -1
	addi r1, r1, 12
	beq r3, r4, .LBB3_57
.LBB3_27:
	ldw r6, r1+0
	bne r6, r14, .LBB3_26
.LBB3_28:
	ldw r6, r1+8
	addi r6, r6, -3
	bgeu r6, r5, .LBB3_26
	jal r0, .LBB3_59
.LBB3_29:
	addi r3, r0, 2
	bne r1, r3, .LBB3_31
.LBB3_30:
	add r11, r13, r0
.LBB3_31:
	add r1, r11, r0
	ldw lr, fp+-68
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
	addi sp, sp, 216
	jalr r0, r31, 0
.LBB3_32:
	addi r1, r0, 1
	stw r18+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.25)
	addi r5, r5, %lo(.L.str.25)
	jal r0, .LBB3_56
.LBB3_33:
	jal r31, next_token
	ldw r1, r17+0
	addi r15, r0, 6
	bne r1, r15, .LBB3_42
.LBB3_34:
	jal r31, next_token
	lui r1, %hi(ATOM_NIL_LIST)
	addi r1, r1, %lo(ATOM_NIL_LIST)
	ldw r1, r1+0
	slli r1, r1, 2
	addi r13, r1, 2
	jal r0, .LBB3_1
.LBB3_35:
	lui r3, %hi(cur_tok+4)
	addi r3, r3, %lo(cur_tok+4)
	jal r31, atom_intern
	add r13, r1, r0
	jal r31, next_token
	slli r1, r13, 2
	addi r13, r1, 2
	jal r0, .LBB3_1
.LBB3_36:
	lui r13, %hi(cur_tok+4)
	addi r13, r13, %lo(cur_tok+4)
	ldbu r1, r13+0
	addi r16, r0, 95
	bne r1, r16, .LBB3_49
.LBB3_37:
	lui r1, %hi(cur_tok+5)
	addi r1, r1, %lo(cur_tok+5)
	ldbu r1, r1+0
	andi r1, r1, 255
	addi r3, r0, 0
	bne r1, r3, .LBB3_49
.LBB3_38:
	lui r1, %hi(clause_max_var)
	addi r1, r1, %lo(clause_max_var)
	ldw r14, r1+0
	addi r3, r14, 1
	stw r1+0, r3
	jal r0, .LBB3_71
.LBB3_39:
	lui r1, %hi(cur_tok+260)
	addi r1, r1, %lo(cur_tok+260)
	ldw r1, r1+0
	slli r1, r1, 2
	addi r13, r1, 1
	jal r31, next_token
	jal r0, .LBB3_1
.LBB3_40:
	jal r31, next_token
	addi r3, r0, 1200
	jal r31, parse_expr
	add r13, r1, r0
	ldw r1, r17+0
	addi r3, r0, 4
	bne r1, r3, .LBB3_55
.LBB3_41:
	jal r31, next_token
	jal r0, .LBB3_1
.LBB3_42:
	addi r3, r0, 999
	jal r31, parse_expr
	ldw r3, r18+0
	addi r13, r0, 0
	bne r3, r13, .LBB3_1
.LBB3_43:
	ldw r3, r17+0
	beq r3, r15, .LBB3_77
.LBB3_44:
	addi r4, r0, 8
	beq r3, r4, .LBB3_76
.LBB3_45:
	addi r4, r0, 9
	bne r3, r4, .LBB3_79
.LBB3_46:
	add r14, r1, r0
	jal r31, next_token
	addi r3, r0, 999
	jal r31, parse_expr
	ldw r3, r18+0
	addi r13, r0, 0
	bne r3, r13, .LBB3_1
.LBB3_47:
	ldw r3, r17+0
	bne r3, r15, .LBB3_82
.LBB3_48:
	add r13, r1, r0
	jal r31, next_token
	add r3, r14, r0
	add r4, r13, r0
	jal r0, .LBB3_78
.LBB3_49:
	lui r17, %hi(clause_var_count)
	addi r17, r17, %lo(clause_var_count)
	ldw r1, r17+0
	addi r19, r0, 1
	blt r1, r19, .LBB3_53
.LBB3_50:
	addi r20, r0, 0
	lui r14, %hi(clause_vars)
	addi r14, r14, %lo(clause_vars)
	lui r15, %hi(cur_tok+4)
	addi r15, r15, %lo(cur_tok+4)
	add r21, r20, r0
.LBB3_51:
	add r3, r14, r0
	add r4, r15, r0
	jal r31, strcmp
	beq r1, r20, .LBB3_70
.LBB3_52:
	addi r21, r21, 1
	ldw r1, r17+0
	addi r14, r14, 72
	blt r21, r1, .LBB3_51
.LBB3_53:
	addi r3, r0, 256
	blt r1, r3, .LBB3_69
.LBB3_54:
	stw r18+0, r19
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.30)
	addi r5, r5, %lo(.L.str.30)
	addi r4, r0, 256
	jal r31, snprintf
	addi r14, r0, 0
	jal r0, .LBB3_71
.LBB3_55:
	addi r1, r0, 1
	stw r18+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.24)
	addi r5, r5, %lo(.L.str.24)
.LBB3_56:
	addi r4, r0, 256
	jal r31, snprintf
	add r13, r11, r0
	jal r0, .LBB3_1
.LBB3_57:
	add r1, r4, r0
	jal r0, .LBB3_59
.LBB3_58:
	addi r1, r0, 0
.LBB3_59:
	ldw r4, r17+0
	addi r3, r0, 3
	bne r4, r3, .LBB3_65
.LBB3_60:
	jal r31, next_token
	addi r3, r0, 999
	jal r31, parse_expr
	addi r3, fp, -196
	stw r3+0, r1
	ldw r1, r18+0
	addi r13, r0, 0
	bne r1, r13, .LBB3_1
.LBB3_61:
	addi r19, r3, 4
	addi r20, r0, 8
	addi r21, r0, 32
	addi r16, r0, 999
	addi r13, r0, 0
.LBB3_62:
	ldw r1, r17+0
	bne r1, r20, .LBB3_72
.LBB3_63:
	jal r31, next_token
	beq r15, r21, .LBB3_80
.LBB3_64:
	add r3, r16, r0
	jal r31, parse_expr
	addi r15, r15, 1
	stw r19+0, r1
	ldw r1, r18+0
	addi r19, r19, 4
	beq r1, r13, .LBB3_62
	jal r0, .LBB3_1
.LBB3_65:
	addi r13, r0, 0
	beq r1, r13, .LBB3_68
.LBB3_66:
	ldw r4, r1+8
	seq r3, r4, r3
	ldw r1, r1+4
	sub r3, r1, r3
	jal r31, parse_expr
	ldw r3, r18+0
	bne r3, r13, .LBB3_1
.LBB3_67:
	addi r5, fp, -196
	stw r5+0, r1
	addi r4, r0, 1
	add r3, r14, r0
	jal r0, .LBB3_74
.LBB3_68:
	slli r1, r14, 2
	addi r13, r1, 2
	jal r0, .LBB3_1
.LBB3_69:
	lui r3, %hi(clause_max_var)
	addi r3, r3, %lo(clause_max_var)
	ldw r14, r3+0
	addi r4, r14, 1
	stw r3+0, r4
	addi r15, r0, 72
	mul r1, r1, r15
	lui r3, %hi(clause_vars)
	addi r3, r3, %lo(clause_vars)
	add r3, r1, r3
	addi r5, r0, 63
	add r4, r13, r0
	jal r31, strncpy
	ldw r1, r17+0
	mul r3, r1, r15
	lui r4, %hi(clause_vars+63)
	addi r4, r4, %lo(clause_vars+63)
	add r4, r3, r4
	addi r5, r0, 0
	stb r4+0, r5
	lui r4, %hi(clause_vars+64)
	addi r4, r4, %lo(clause_vars+64)
	add r4, r3, r4
	stw r4+0, r14
	ldbu r4, r13+0
	sne r4, r4, r16
	lui r5, %hi(clause_vars+68)
	addi r5, r5, %lo(clause_vars+68)
	add r3, r3, r5
	stw r3+0, r4
	addi r1, r1, 1
	stw r17+0, r1
	jal r0, .LBB3_71
.LBB3_70:
	ldw r14, r14+64
.LBB3_71:
	jal r31, next_token
	slli r1, r14, 2
	addi r13, r1, 3
	jal r0, .LBB3_1
.LBB3_72:
	addi r3, r0, 4
	bne r1, r3, .LBB3_75
.LBB3_73:
	jal r31, next_token
	addi r5, fp, -196
	add r3, r14, r0
	add r4, r15, r0
.LBB3_74:
	jal r31, make_compound
	add r13, r1, r0
	jal r0, .LBB3_1
.LBB3_75:
	addi r1, r0, 1
	stw r18+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.24)
	addi r5, r5, %lo(.L.str.24)
	jal r0, .LBB3_81
.LBB3_76:
	add r13, r1, r0
	jal r31, next_token
	jal r31, parse_list_tail
	add r3, r13, r0
	add r4, r1, r0
	jal r0, .LBB3_78
.LBB3_77:
	add r13, r1, r0
	jal r31, next_token
	lui r1, %hi(ATOM_NIL_LIST)
	addi r1, r1, %lo(ATOM_NIL_LIST)
	ldw r1, r1+0
	slli r1, r1, 2
	addi r4, r1, 2
	add r3, r13, r0
.LBB3_78:
	jal r31, make_list_cons
	add r13, r1, r0
	jal r0, .LBB3_1
.LBB3_79:
	addi r1, r0, 1
	stw r18+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.27)
	addi r5, r5, %lo(.L.str.27)
	jal r0, .LBB3_81
.LBB3_80:
	addi r1, r0, 1
	stw r18+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.31)
	addi r5, r5, %lo(.L.str.31)
.LBB3_81:
	addi r4, r0, 256
	jal r31, snprintf
	jal r0, .LBB3_1
.LBB3_82:
	addi r1, r0, 1
	stw r18+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.26)
	addi r5, r5, %lo(.L.str.26)
	jal r0, .LBB3_81
.Lfunc_end3:
	.size	parse_expr, .Lfunc_end3-parse_expr
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI3_0:
	.word	.LBB3_24
	.word	.LBB3_36
	.word	.LBB3_39
	.word	.LBB3_40
	.word	.LBB3_32
	.word	.LBB3_33
	.word	.LBB3_32
	.word	.LBB3_32
	.word	.LBB3_32
	.word	.LBB3_32
	.word	.LBB3_35
	.word	.LBB3_1
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function parse_list_tail
	.type	parse_list_tail,@function
parse_list_tail:                        # @parse_list_tail
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
	addi r3, r0, 999
	jal r31, parse_expr
	add r11, r1, r0
	lui r13, %hi(g_error)
	addi r13, r13, %lo(g_error)
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB4_12
.LBB4_1:
	lui r14, %hi(cur_tok)
	addi r14, r14, %lo(cur_tok)
	ldw r3, r14+0
	addi r15, r0, 6
	beq r3, r15, .LBB4_8
.LBB4_2:
	addi r4, r0, 8
	beq r3, r4, .LBB4_7
.LBB4_3:
	addi r4, r0, 9
	bne r3, r4, .LBB4_10
.LBB4_4:
	jal r31, next_token
	addi r3, r0, 999
	jal r31, parse_expr
	add r12, r1, r0
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB4_12
.LBB4_5:
	ldw r3, r14+0
	bne r3, r15, .LBB4_13
.LBB4_6:
	jal r31, next_token
	add r3, r11, r0
	add r4, r12, r0
	jal r0, .LBB4_9
.LBB4_7:
	jal r31, next_token
	jal r31, parse_list_tail
	add r3, r11, r0
	add r4, r1, r0
	jal r0, .LBB4_9
.LBB4_8:
	jal r31, next_token
	lui r1, %hi(ATOM_NIL_LIST)
	addi r1, r1, %lo(ATOM_NIL_LIST)
	ldw r1, r1+0
	slli r1, r1, 2
	addi r4, r1, 2
	add r3, r11, r0
.LBB4_9:
	jal r31, make_list_cons
	jal r0, .LBB4_12
.LBB4_10:
	addi r3, r0, 1
	stw r13+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.29)
	addi r5, r5, %lo(.L.str.29)
.LBB4_11:
	addi r4, r0, 256
	add r11, r1, r0
	jal r31, snprintf
	add r1, r11, r0
.LBB4_12:
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
.LBB4_13:
	addi r3, r0, 1
	stw r13+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.28)
	addi r5, r5, %lo(.L.str.28)
	jal r0, .LBB4_11
.Lfunc_end4:
	.size	parse_list_tail, .Lfunc_end4-parse_list_tail
                                        # -- End function
	.type	clause_var_count,@object        # @clause_var_count
	.bss
	.globl	clause_var_count
	.p2align	2, 0x0
clause_var_count:
	.word	0                               # 0x0
	.size	clause_var_count, 4

	.type	clause_max_var,@object          # @clause_max_var
	.globl	clause_max_var
	.p2align	2, 0x0
clause_max_var:
	.word	0                               # 0x0
	.size	clause_max_var, 4

	.type	op_count,@object                # @op_count
	.local	op_count
	.comm	op_count,4,4
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	":-"
	.size	.L.str, 3

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"?-"
	.size	.L.str.1, 3

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	";"
	.size	.L.str.2, 2

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"->"
	.size	.L.str.3, 3

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	","
	.size	.L.str.4, 2

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"\\+"
	.size	.L.str.5, 3

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"is"
	.size	.L.str.6, 3

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"="
	.size	.L.str.7, 2

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"\\="
	.size	.L.str.8, 3

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"=="
	.size	.L.str.9, 3

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"\\=="
	.size	.L.str.10, 4

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"<"
	.size	.L.str.11, 2

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	">"
	.size	.L.str.12, 2

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"=<"
	.size	.L.str.13, 3

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	">="
	.size	.L.str.14, 3

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"=:="
	.size	.L.str.15, 4

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"=\\="
	.size	.L.str.16, 4

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"=.."
	.size	.L.str.17, 4

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"+"
	.size	.L.str.18, 2

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"-"
	.size	.L.str.19, 2

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"*"
	.size	.L.str.20, 2

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"//"
	.size	.L.str.21, 3

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"mod"
	.size	.L.str.22, 4

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"expected '.' at end of term"
	.size	.L.str.23, 28

	.type	clause_vars,@object             # @clause_vars
	.bss
	.globl	clause_vars
	.p2align	2, 0x0
clause_vars:
	.zero	18432
	.size	clause_vars, 18432

	.type	ops,@object                     # @ops
	.local	ops
	.comm	ops,768,4
	.type	.L.str.24,@object               # @.str.24
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.24:
	.asciz	"expected ')'"
	.size	.L.str.24, 13

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"unexpected token"
	.size	.L.str.25, 17

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"expected ']' after list tail"
	.size	.L.str.26, 29

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"expected ',', '|', or ']' in list"
	.size	.L.str.27, 34

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"expected ']'"
	.size	.L.str.28, 13

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"unexpected token in list"
	.size	.L.str.29, 25

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"too many variables in clause"
	.size	.L.str.30, 29

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"too many arguments"
	.size	.L.str.31, 19

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
