	.file	"print.c"
	.text
	.globl	write_term                      # -- Begin function write_term
	.p2align	2
	.type	write_term,@function
write_term:                             # @write_term
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r4, r0, 0
	jal r31, write_term_depth
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	write_term, .Lfunc_end0-write_term
                                        # -- End function
	.p2align	2                               # -- Begin function write_term_depth
	.type	write_term_depth,@function
write_term_depth:                       # @write_term_depth
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
	stw fp+-72, r28
	stw fp+-76, lr
	add r12, r4, r0
	addi r19, r0, 101
	addi r11, r0, 0
	lui r20, %hi(.LJTI1_0)
	addi r20, r20, %lo(.LJTI1_0)
	lui r21, %hi(ATOM_DOT)
	addi r21, r21, %lo(ATOM_DOT)
	addi r22, r0, 2
	addi r13, r0, 1
	lui r23, %hi(ATOM_COMMA)
	addi r23, r23, %lo(ATOM_COMMA)
	addi r14, r0, 44
	lui r15, %hi(.L.str.5)
	addi r15, r15, %lo(.L.str.5)
	lui r24, %hi(ATOM_NOT)
	addi r24, r24, %lo(ATOM_NOT)
	lui r25, %hi(ATOM_MINUS)
	addi r25, r25, %lo(ATOM_MINUS)
	addi r26, r0, -4
	lui r27, %hi(var_binding)
	addi r27, r27, %lo(var_binding)
	jal r0, .LBB1_2
.LBB1_1:
	add r3, r18, r0
	jal r31, atom_name
	add r3, r15, r0
	add r4, r1, r0
	jal r31, printf
	add r3, r16, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r3, r1, r0
	addi r12, r12, 1
.LBB1_2:
	bge r12, r19, .LBB1_17
.LBB1_3:
	jal r31, deref
	beq r1, r11, .LBB1_18
.LBB1_4:
	add r16, r1, r0
	andi r1, r1, 3
	slli r1, r1, 2
	add r1, r20, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB1_5:
	add r3, r16, r0
	jal r31, compound_functor
	add r18, r1, r0
	add r3, r16, r0
	jal r31, compound_arity
	add r17, r1, r0
	ldw r1, r21+0
	bne r18, r1, .LBB1_7
.LBB1_6:
	beq r17, r22, .LBB1_29
.LBB1_7:
	bne r17, r22, .LBB1_13
.LBB1_8:
	add r3, r18, r0
	jal r31, is_infix_op
	beq r1, r11, .LBB1_13
.LBB1_9:
	add r3, r16, r0
	add r4, r11, r0
	jal r31, compound_arg
	add r17, r1, r0
	add r3, r16, r0
	add r4, r13, r0
	jal r31, compound_arg
	add r16, r1, r0
	ldw r28, r23+0
	addi r4, r12, 1
	add r3, r17, r0
	jal r31, write_term_depth
	bne r18, r28, .LBB1_16
.LBB1_10:
	add r3, r14, r0
	jal r31, putchar
	add r3, r16, r0
	addi r12, r12, 1
	jal r0, .LBB1_2
.LBB1_11:
	and r1, r16, r26
	add r1, r1, r27
	ldw r3, r1+0
	beq r3, r11, .LBB1_28
.LBB1_12:
	addi r12, r12, 1
	jal r0, .LBB1_2
.LBB1_13:
	bne r17, r13, .LBB1_19
.LBB1_14:
	ldw r1, r24+0
	beq r18, r1, .LBB1_1
.LBB1_15:
	ldw r1, r25+0
	beq r18, r1, .LBB1_1
	jal r0, .LBB1_19
.LBB1_16:
	add r3, r18, r0
	jal r31, atom_name
	add r3, r15, r0
	add r4, r1, r0
	jal r31, printf
	add r3, r16, r0
	addi r12, r12, 1
	jal r0, .LBB1_2
.LBB1_17:
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	jal r31, printf
	jal r0, .LBB1_31
.LBB1_18:
	lui r3, %hi(.L.str.1)
	addi r3, r3, %lo(.L.str.1)
	jal r31, printf
	jal r0, .LBB1_31
.LBB1_19:
	add r3, r18, r0
	jal r31, atom_name
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	add r4, r1, r0
	jal r31, printf
	addi r3, r0, 40
	jal r31, putchar
	blt r17, r13, .LBB1_24
.LBB1_20:
	addi r12, r12, 1
	addi r13, r0, 44
	add r14, r11, r0
	jal r0, .LBB1_22
.LBB1_21:
	add r3, r16, r0
	add r4, r14, r0
	jal r31, compound_arg
	add r3, r1, r0
	add r4, r12, r0
	jal r31, write_term_depth
	addi r14, r14, 1
	beq r17, r14, .LBB1_24
.LBB1_22:
	beq r14, r11, .LBB1_21
.LBB1_23:
	add r3, r13, r0
	jal r31, putchar
	jal r0, .LBB1_21
.LBB1_24:
	addi r3, r0, 41
	jal r0, .LBB1_30
.LBB1_25:
	srai r4, r16, 2
	lui r3, %hi(.L.str.2)
	addi r3, r3, %lo(.L.str.2)
	jal r31, printf
	jal r0, .LBB1_31
.LBB1_26:
	srli r3, r16, 2
	lui r1, %hi(ATOM_NIL_LIST)
	addi r1, r1, %lo(ATOM_NIL_LIST)
	ldw r1, r1+0
	bne r3, r1, .LBB1_32
.LBB1_27:
	lui r3, %hi(.L.str.3)
	addi r3, r3, %lo(.L.str.3)
	jal r31, printf
	jal r0, .LBB1_31
.LBB1_28:
	srli r4, r16, 2
	lui r3, %hi(.L.str.4)
	addi r3, r3, %lo(.L.str.4)
	jal r31, printf
	jal r0, .LBB1_31
.LBB1_29:
	addi r3, r0, 91
	jal r31, putchar
	add r3, r16, r0
	add r4, r12, r0
	jal r31, write_list
	addi r3, r0, 93
.LBB1_30:
	jal r31, putchar
.LBB1_31:
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
	addi sp, sp, 88
	jalr r0, r31, 0
.LBB1_32:
	jal r31, atom_name
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	add r4, r1, r0
	jal r31, printf
	jal r0, .LBB1_31
.Lfunc_end1:
	.size	write_term_depth, .Lfunc_end1-write_term_depth
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI1_0:
	.word	.LBB1_5
	.word	.LBB1_25
	.word	.LBB1_26
	.word	.LBB1_11
                                        # -- End function
	.text
	.globl	writeln_term                    # -- Begin function writeln_term
	.p2align	2
	.type	writeln_term,@function
writeln_term:                           # @writeln_term
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r4, r0, 0
	jal r31, write_term_depth
	addi r3, r0, 10
	jal r31, putchar
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	writeln_term, .Lfunc_end2-writeln_term
                                        # -- End function
	.p2align	2                               # -- Begin function write_list
	.type	write_list,@function
write_list:                             # @write_list
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
	add r13, r3, r0
	addi r12, r0, 0
	addi r14, r0, 1
	addi r11, r4, 1
	addi r17, r0, 2
	lui r18, %hi(ATOM_NIL_LIST)
	addi r18, r18, %lo(ATOM_NIL_LIST)
	lui r19, %hi(ATOM_DOT)
	addi r19, r19, %lo(ATOM_DOT)
	addi r15, r0, 44
.LBB3_1:
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r16, r1, r0
	add r3, r13, r0
	add r4, r14, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, deref
	add r13, r1, r0
	add r3, r16, r0
	add r4, r11, r0
	jal r31, write_term_depth
	andi r1, r13, 3
	bne r1, r17, .LBB3_3
.LBB3_2:
	srli r3, r13, 2
	ldw r4, r18+0
	beq r3, r4, .LBB3_9
.LBB3_3:
	beq r13, r12, .LBB3_8
.LBB3_4:
	bne r1, r12, .LBB3_8
.LBB3_5:
	add r3, r13, r0
	jal r31, compound_functor
	ldw r3, r19+0
	bne r1, r3, .LBB3_8
.LBB3_6:
	add r3, r13, r0
	jal r31, compound_arity
	bne r1, r17, .LBB3_8
.LBB3_7:
	add r3, r15, r0
	jal r31, putchar
	jal r0, .LBB3_1
.LBB3_8:
	addi r3, r0, 124
	jal r31, putchar
	add r3, r13, r0
	add r4, r11, r0
	jal r31, write_term_depth
.LBB3_9:
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
.Lfunc_end3:
	.size	write_list, .Lfunc_end3-write_list
                                        # -- End function
	.p2align	2                               # -- Begin function is_infix_op
	.type	is_infix_op,@function
is_infix_op:                            # @is_infix_op
# %bb.0:
	lui r1, %hi(ATOM_COMMA)
	addi r1, r1, %lo(ATOM_COMMA)
	ldw r1, r1+0
	seq r1, r3, r1
	lui r4, %hi(ATOM_SEMI)
	addi r4, r4, %lo(ATOM_SEMI)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_ARROW)
	addi r4, r4, %lo(ATOM_ARROW)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_PLUS)
	addi r4, r4, %lo(ATOM_PLUS)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_MINUS)
	addi r4, r4, %lo(ATOM_MINUS)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_STAR)
	addi r4, r4, %lo(ATOM_STAR)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_SLASH2)
	addi r4, r4, %lo(ATOM_SLASH2)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_MOD)
	addi r4, r4, %lo(ATOM_MOD)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_IS)
	addi r4, r4, %lo(ATOM_IS)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_UNIFY)
	addi r4, r4, %lo(ATOM_UNIFY)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_NOT_UNIFY)
	addi r4, r4, %lo(ATOM_NOT_UNIFY)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_EQ)
	addi r4, r4, %lo(ATOM_EQ)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_NEQ)
	addi r4, r4, %lo(ATOM_NEQ)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_LT)
	addi r4, r4, %lo(ATOM_LT)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_GT)
	addi r4, r4, %lo(ATOM_GT)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_LE)
	addi r4, r4, %lo(ATOM_LE)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_GE)
	addi r4, r4, %lo(ATOM_GE)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_ARITH_EQ)
	addi r4, r4, %lo(ATOM_ARITH_EQ)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_ARITH_NEQ)
	addi r4, r4, %lo(ATOM_ARITH_NEQ)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_CLAUSE)
	addi r4, r4, %lo(ATOM_CLAUSE)
	ldw r4, r4+0
	seq r4, r3, r4
	or  r1, r1, r4
	lui r4, %hi(ATOM_UNIV)
	addi r4, r4, %lo(ATOM_UNIV)
	ldw r4, r4+0
	seq r3, r3, r4
	or  r1, r1, r3
	andi r1, r1, 1
	jalr r0, r31, 0
.Lfunc_end4:
	.size	is_infix_op, .Lfunc_end4-is_infix_op
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"..."
	.size	.L.str, 4

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"nil"
	.size	.L.str.1, 4

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"%d"
	.size	.L.str.2, 3

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"[]"
	.size	.L.str.3, 3

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"_G%d"
	.size	.L.str.4, 5

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"%s"
	.size	.L.str.5, 3

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
