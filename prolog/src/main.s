	.file	"main.c"
	.text
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -2048
	addi sp, sp, -136
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 2047
	addi fp, fp, 145
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
	jal r31, init_atoms
	jal r31, db_init
	jal r31, reader_init
	jal r31, init_operators
	lui r25, %hi(g_error)
	addi r25, r25, %lo(g_error)
	addi r22, r0, 0
	lui r26, %hi(ATOM_CLAUSE)
	addi r26, r26, %lo(ATOM_CLAUSE)
	addi r14, r0, 1
	lui r1, %hi(.L.str.1)
	addi r1, r1, %lo(.L.str.1)
	add r3, fp, r0
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	stw r3+-108, r1
	lui r12, %hi(ATOM_QUERY)
	addi r12, r12, %lo(ATOM_QUERY)
	lui r28, %hi(clause_var_count)
	addi r28, r28, %lo(clause_var_count)
	addi r24, r0, 72
	lui r11, %hi(clause_vars)
	addi r11, r11, %lo(clause_vars)
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r27, r1, -76
	lui r1, %hi(.L.str.3)
	addi r1, r1, %lo(.L.str.3)
	add r3, fp, r0
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	stw r3+-120, r1
	lui r17, %hi(.L.str)
	addi r17, r17, %lo(.L.str)
	lui r18, %hi(g_errmsg)
	addi r18, r18, %lo(g_errmsg)
	lui r1, %hi(.L.str.6)
	addi r1, r1, %lo(.L.str.6)
	add r3, fp, r0
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	stw r3+-116, r1
	addi r19, r0, 3
	lui r1, %hi(var_count)
	addi r1, r1, %lo(var_count)
	add r3, fp, r0
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	stw r3+-112, r1
	lui r1, %hi(var_binding)
	addi r1, r1, %lo(var_binding)
	add r3, fp, r0
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	stw r3+-80, r1
	lui r20, %hi(.L.str.5)
	addi r20, r20, %lo(.L.str.5)
	lui r21, %hi(.L.str.4)
	addi r21, r21, %lo(.L.str.4)
	lui r1, %hi(.L.str.2)
	addi r1, r1, %lo(.L.str.2)
	add r3, fp, r0
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	stw r3+-104, r1
	addi r1, r0, 2
	add r3, fp, r0
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	addi r3, r3, -2048
	stw r3+-84, r1
	lui r16, %hi(clause_max_var)
	addi r16, r16, %lo(clause_max_var)
	lui r15, %hi(ATOM_TRUE)
	addi r15, r15, %lo(ATOM_TRUE)
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	stw r1+-96, r11
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	stw r1+-88, r27
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	stw r1+-100, r24
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	stw r1+-92, r12
	jal r0, .LBB0_2
.LBB0_1:
	add r3, r17, r0
	add r4, r18, r0
	jal r31, printf
	stw r25+0, r22
.LBB0_2:
	stw r25+0, r22
	jal r31, engine_reset
	jal r31, parse_term
	ldw r3, r25+0
	bne r3, r22, .LBB0_1
.LBB0_3:
	beq r1, r22, .LBB0_37
.LBB0_4:
	add r3, r1, r0
	jal r31, deref
	add r23, r1, r0
	beq r1, r22, .LBB0_27
.LBB0_5:
	andi r1, r23, 3
	bne r1, r22, .LBB0_27
.LBB0_6:
	add r3, r23, r0
	jal r31, compound_functor
	ldw r3, r26+0
	bne r1, r3, .LBB0_11
.LBB0_7:
	add r3, r23, r0
	jal r31, compound_arity
	bne r1, r14, .LBB0_11
.LBB0_8:
	add r3, r23, r0
	add r4, r22, r0
	jal r31, compound_arg
	add r3, r1, r0
	jal r31, solve
	bne r1, r22, .LBB0_10
.LBB0_9:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	ldw r3, r1+-108
	jal r31, printf
.LBB0_10:
	stw r25+0, r22
	jal r0, .LBB0_2
.LBB0_11:
	add r3, r23, r0
	jal r31, compound_functor
	ldw r3, r12+0
	bne r1, r3, .LBB0_24
.LBB0_12:
	add r3, r23, r0
	jal r31, compound_arity
	bne r1, r14, .LBB0_24
.LBB0_13:
	add r3, r23, r0
	add r4, r22, r0
	jal r31, compound_arg
	add r13, r1, r0
	add r12, r11, r0
	ldw r11, r28+0
	mul r23, r11, r24
	add r3, r27, r0
	add r4, r12, r0
	add r5, r23, r0
	jal r31, memcpy
	add r3, r13, r0
	jal r31, solve
	beq r1, r22, .LBB0_32
.LBB0_14:
	stw r28+0, r11
	add r3, r12, r0
	add r4, r27, r0
	add r5, r23, r0
	jal r31, memcpy
	ldw r1, r28+0
	add r11, r12, r0
	blt r1, r14, .LBB0_31
.LBB0_15:
	add r23, r11, r0
	add r11, r22, r0
	add r12, r22, r0
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	ldw r27, r1+-112
	jal r0, .LBB0_18
.LBB0_16:
	add r3, r20, r0
	add r4, r23, r0
	jal r31, printf
	add r3, r24, r0
	jal r31, write_term
	add r12, r14, r0
.LBB0_17:
	addi r11, r11, 1
	ldw r1, r28+0
	addi r23, r23, 72
	bge r11, r1, .LBB0_29
.LBB0_18:
	ldw r1, r23+68
	beq r1, r22, .LBB0_17
.LBB0_19:
	ldw r1, r23+64
	slli r1, r1, 2
	addi r3, r1, 3
	jal r31, deref
	add r24, r1, r0
	andi r1, r1, 3
	bne r1, r19, .LBB0_22
.LBB0_20:
	srli r1, r24, 2
	ldw r3, r27+0
	bge r1, r3, .LBB0_22
.LBB0_21:
	slli r1, r1, 2
	add r4, fp, r0
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	ldw r3, r4+-80
	add r1, r1, r3
	ldw r1, r1+0
	beq r1, r22, .LBB0_17
.LBB0_22:
	beq r12, r22, .LBB0_16
.LBB0_23:
	add r3, r21, r0
	jal r31, printf
	jal r0, .LBB0_16
.LBB0_24:
	add r3, r23, r0
	jal r31, compound_functor
	ldw r3, r26+0
	bne r1, r3, .LBB0_27
.LBB0_25:
	add r3, r23, r0
	jal r31, compound_arity
	add r4, fp, r0
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	addi r4, r4, -2048
	ldw r3, r4+-84
	bne r1, r3, .LBB0_27
.LBB0_26:
	add r3, r23, r0
	add r4, r22, r0
	jal r31, compound_arg
	add r13, r24, r0
	add r24, r1, r0
	add r3, r23, r0
	add r4, r14, r0
	jal r31, compound_arg
	ldw r5, r16+0
	add r3, r24, r0
	add r24, r13, r0
	add r4, r1, r0
	jal r0, .LBB0_28
.LBB0_27:
	ldw r1, r15+0
	slli r1, r1, 2
	addi r4, r1, 2
	ldw r5, r16+0
	add r3, r23, r0
.LBB0_28:
	add r6, r22, r0
	jal r31, db_add_clause
	ldw r1, r25+0
	bne r1, r22, .LBB0_1
	jal r0, .LBB0_2
.LBB0_29:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	ldw r11, r1+-96
	beq r12, r22, .LBB0_31
.LBB0_30:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	ldw r3, r1+-116
	jal r31, printf
.LBB0_31:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	ldw r3, r1+-104
	jal r31, printf
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	ldw r24, r1+-100
	jal r0, .LBB0_36
.LBB0_32:
	ldw r1, r25+0
	beq r1, r22, .LBB0_34
.LBB0_33:
	add r3, r17, r0
	add r4, r18, r0
	jal r31, printf
	stw r25+0, r22
	jal r0, .LBB0_35
.LBB0_34:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	ldw r3, r1+-120
	jal r31, printf
.LBB0_35:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	ldw r11, r1+-96
.LBB0_36:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	ldw r27, r1+-88
	add r1, fp, r0
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	addi r1, r1, -2048
	ldw r12, r1+-92
	jal r0, .LBB0_2
.LBB0_37:
	addi r1, r0, 0
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
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 2047
	addi sp, sp, 145
	jalr r0, r31, 0
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Error: %s\n"
	.size	.L.str, 11

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"Error: directive failed\n"
	.size	.L.str.1, 25

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"true.\n"
	.size	.L.str.2, 7

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"false.\n"
	.size	.L.str.3, 8

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	","
	.size	.L.str.4, 2

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"%s = "
	.size	.L.str.5, 6

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"\n"
	.size	.L.str.6, 2

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
