	.file	"database.c"
	.text
	.globl	db_init                         # -- Begin function db_init
	.p2align	2
	.type	db_init,@function
db_init:                                # @db_init
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	lui r3, %hi(pred_table)
	addi r3, r3, %lo(pred_table)
	addi r11, r0, 0
	addi r5, r0, 1024
	add r4, r11, r0
	jal r31, memset
	lui r1, %hi(pred_arena_count)
	addi r1, r1, %lo(pred_arena_count)
	stw r1+0, r11
	lui r1, %hi(clause_arena_count)
	addi r1, r1, %lo(clause_arena_count)
	stw r1+0, r11
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	db_init, .Lfunc_end0-db_init
                                        # -- End function
	.globl	db_lookup                       # -- Begin function db_lookup
	.p2align	2
	.type	db_lookup,@function
db_lookup:                              # @db_lookup
# %bb.0:
	addi r1, r0, 31
	mul r1, r3, r1
	add r1, r1, r4
	andi r1, r1, 255
	slli r1, r1, 2
	lui r5, %hi(pred_table)
	addi r5, r5, %lo(pred_table)
	add r1, r1, r5
	ldw r5, r1+0
	addi r1, r0, 0
	beq r5, r1, .LBB1_6
.LBB1_1:
	addi r1, r0, 0
	jal r0, .LBB1_3
.LBB1_2:
	ldw r5, r5+16
	beq r5, r1, .LBB1_6
.LBB1_3:
	ldw r6, r5+0
	bne r6, r3, .LBB1_2
.LBB1_4:
	ldw r6, r5+4
	bne r6, r4, .LBB1_2
.LBB1_5:
	add r1, r5, r0
.LBB1_6:
	jalr r0, r31, 0
.Lfunc_end1:
	.size	db_lookup, .Lfunc_end1-db_lookup
                                        # -- End function
	.globl	db_alloc_clause                 # -- Begin function db_alloc_clause
	.p2align	2
	.type	db_alloc_clause,@function
db_alloc_clause:                        # @db_alloc_clause
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r3, %hi(clause_arena_count)
	addi r3, r3, %lo(clause_arena_count)
	ldw r1, r3+0
	lui r4, 1
	blt r1, r4, .LBB2_2
.LBB2_1:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	addi r4, r0, 256
	jal r31, snprintf
	addi r1, r0, 0
	jal r0, .LBB2_3
.LBB2_2:
	addi r4, r1, 1
	stw r3+0, r4
	slli r3, r1, 4
	lui r1, %hi(clause_arena)
	addi r1, r1, %lo(clause_arena)
	add r1, r3, r1
	addi r4, r0, 0
	stw r1+0, r4
	lui r5, %hi(clause_arena+4)
	addi r5, r5, %lo(clause_arena+4)
	add r5, r3, r5
	stw r5+0, r4
	lui r5, %hi(clause_arena+8)
	addi r5, r5, %lo(clause_arena+8)
	add r5, r3, r5
	stw r5+0, r4
	lui r5, %hi(clause_arena+12)
	addi r5, r5, %lo(clause_arena+12)
	add r3, r3, r5
	stw r3+0, r4
.LBB2_3:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	db_alloc_clause, .Lfunc_end2-db_alloc_clause
                                        # -- End function
	.globl	db_add_clause                   # -- Begin function db_add_clause
	.p2align	2
	.type	db_add_clause,@function
db_add_clause:                          # @db_add_clause
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
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	add r14, r3, r0
	jal r31, term_functor
	add r15, r1, r0
	add r3, r14, r0
	jal r31, term_arity
	addi r3, r0, -1
	ble r15, r3, .LBB3_7
.LBB3_1:
	addi r3, r0, 31
	mul r3, r15, r3
	add r3, r3, r1
	andi r3, r3, 255
	slli r3, r3, 2
	lui r4, %hi(pred_table)
	addi r4, r4, %lo(pred_table)
	add r3, r3, r4
	ldw r4, r3+0
	addi r5, r0, 0
	add r16, r5, r0
	beq r4, r5, .LBB3_6
.LBB3_2:
	addi r6, r0, 0
	add r16, r4, r0
	jal r0, .LBB3_4
.LBB3_3:
	ldw r16, r16+16
	beq r16, r6, .LBB3_8
.LBB3_4:
	ldw r7, r16+0
	bne r7, r15, .LBB3_3
.LBB3_5:
	ldw r7, r16+4
	bne r7, r1, .LBB3_3
.LBB3_6:
	bne r16, r5, .LBB3_11
	jal r0, .LBB3_9
.LBB3_7:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r4, r0, 256
	jal r31, snprintf
	jal r0, .LBB3_22
.LBB3_8:
	add r16, r6, r0
	bne r16, r5, .LBB3_11
.LBB3_9:
	lui r6, %hi(pred_arena_count)
	addi r6, r6, %lo(pred_arena_count)
	ldw r5, r6+0
	addi r7, r0, 512
	blt r5, r7, .LBB3_17
.LBB3_10:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	addi r4, r0, 256
	jal r31, snprintf
	addi r16, r0, 0
.LBB3_11:
	addi r15, r0, 0
	beq r16, r15, .LBB3_22
.LBB3_12:
	lui r3, %hi(clause_arena_count)
	addi r3, r3, %lo(clause_arena_count)
	ldw r1, r3+0
	lui r4, 1
	blt r1, r4, .LBB3_14
.LBB3_13:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	addi r4, r0, 256
	jal r31, snprintf
	add r17, r15, r0
	bne r17, r15, .LBB3_15
	jal r0, .LBB3_22
.LBB3_14:
	addi r4, r1, 1
	stw r3+0, r4
	slli r1, r1, 4
	lui r3, %hi(clause_arena)
	addi r3, r3, %lo(clause_arena)
	add r17, r1, r3
	stw r17+0, r15
	lui r3, %hi(clause_arena+4)
	addi r3, r3, %lo(clause_arena+4)
	add r3, r1, r3
	stw r3+0, r15
	lui r3, %hi(clause_arena+8)
	addi r3, r3, %lo(clause_arena+8)
	add r3, r1, r3
	stw r3+0, r15
	lui r3, %hi(clause_arena+12)
	addi r3, r3, %lo(clause_arena+12)
	add r1, r1, r3
	stw r1+0, r15
	beq r17, r15, .LBB3_22
.LBB3_15:
	add r3, r14, r0
	jal r31, persist_term
	stw r17+0, r1
	add r3, r13, r0
	jal r31, persist_term
	stw r17+4, r1
	stw r17+8, r12
	beq r11, r15, .LBB3_18
.LBB3_16:
	ldw r1, r16+8
	stw r17+12, r1
	stw r16+8, r17
	ldw r1, r16+12
	bne r1, r15, .LBB3_22
	jal r0, .LBB3_21
.LBB3_17:
	addi r7, r5, 1
	stw r6+0, r7
	addi r6, r0, 20
	mul r5, r5, r6
	lui r6, %hi(pred_arena)
	addi r6, r6, %lo(pred_arena)
	add r16, r5, r6
	stw r16+0, r15
	lui r6, %hi(pred_arena+4)
	addi r6, r6, %lo(pred_arena+4)
	add r6, r5, r6
	stw r6+0, r1
	lui r1, %hi(pred_arena+8)
	addi r1, r1, %lo(pred_arena+8)
	add r1, r5, r1
	addi r6, r0, 0
	stw r1+0, r6
	lui r1, %hi(pred_arena+12)
	addi r1, r1, %lo(pred_arena+12)
	add r1, r5, r1
	stw r1+0, r6
	lui r1, %hi(pred_arena+16)
	addi r1, r1, %lo(pred_arena+16)
	add r1, r5, r1
	stw r1+0, r4
	stw r3+0, r16
	addi r15, r0, 0
	bne r16, r15, .LBB3_12
	jal r0, .LBB3_22
.LBB3_18:
	stw r17+12, r15
	ldw r1, r16+12
	beq r1, r15, .LBB3_20
.LBB3_19:
	stw r1+12, r17
	jal r0, .LBB3_21
.LBB3_20:
	stw r16+8, r17
.LBB3_21:
	stw r16+12, r17
.LBB3_22:
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
.Lfunc_end3:
	.size	db_add_clause, .Lfunc_end3-db_add_clause
                                        # -- End function
	.globl	db_retract                      # -- Begin function db_retract
	.p2align	2
	.type	db_retract,@function
db_retract:                             # @db_retract
# %bb.0:
	addi sp, sp, -1112
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 1112
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
	add r11, r4, r0
	add r12, r3, r0
	jal r31, term_functor
	add r14, r1, r0
	add r3, r12, r0
	jal r31, term_arity
	addi r3, r0, 31
	mul r3, r14, r3
	add r3, r3, r1
	andi r3, r3, 255
	slli r3, r3, 2
	lui r4, %hi(pred_table)
	addi r4, r4, %lo(pred_table)
	add r3, r3, r4
	ldw r3, r3+0
	addi r13, r0, 0
	add r21, r13, r0
	beq r3, r13, .LBB4_6
.LBB4_1:
	addi r21, r0, 0
	jal r0, .LBB4_3
.LBB4_2:
	ldw r3, r3+16
	beq r3, r21, .LBB4_6
.LBB4_3:
	ldw r4, r3+0
	bne r4, r14, .LBB4_2
.LBB4_4:
	ldw r4, r3+4
	bne r4, r1, .LBB4_2
.LBB4_5:
	add r21, r3, r0
.LBB4_6:
	beq r21, r13, .LBB4_18
.LBB4_7:
	ldw r22, r21+8
	addi r13, r0, 0
	beq r22, r13, .LBB4_18
.LBB4_8:
	addi r13, r0, 0
	lui r23, %hi(trail_top)
	addi r23, r23, %lo(trail_top)
	lui r24, %hi(hp)
	addi r24, r24, %lo(hp)
	lui r26, %hi(var_count)
	addi r26, r26, %lo(var_count)
	addi r14, fp, -1100
	addi r15, r0, -1
	addi r16, r0, 1024
	addi r17, r0, 256
	lui r1, %hi(ATOM_TRUE)
	addi r1, r1, %lo(ATOM_TRUE)
	stw fp+-1104, r1
	add r25, r13, r0
	jal r0, .LBB4_10
.LBB4_9:
	add r3, r18, r0
	jal r31, trail_undo
	stw r24+0, r28
	stw r26+0, r27
	ldw r1, r22+12
	add r25, r22, r0
	add r22, r1, r0
	beq r1, r13, .LBB4_18
.LBB4_10:
	ldw r18, r23+0
	ldw r28, r24+0
	ldw r27, r26+0
	add r3, r14, r0
	add r4, r15, r0
	add r5, r16, r0
	jal r31, memset
	ldw r3, r22+0
	add r4, r14, r0
	add r5, r17, r0
	jal r31, copy_term_code
	add r20, r1, r0
	ldw r3, r22+4
	add r4, r14, r0
	add r5, r17, r0
	jal r31, copy_term_code
	add r19, r1, r0
	add r3, r12, r0
	add r4, r20, r0
	jal r31, unify
	beq r1, r13, .LBB4_9
.LBB4_11:
	beq r11, r13, .LBB4_14
.LBB4_12:
	ldw r1, fp+-1104
	ldw r1, r1+0
	slli r1, r1, 2
	addi r1, r1, 2
	beq r11, r1, .LBB4_14
.LBB4_13:
	add r3, r11, r0
	add r4, r19, r0
	jal r31, unify
	beq r1, r13, .LBB4_9
.LBB4_14:
	ldw r1, r22+12
	addi r3, r0, 0
	beq r25, r3, .LBB4_19
.LBB4_15:
	stw r25+12, r1
	ldw r1, r21+12
	bne r1, r22, .LBB4_17
.LBB4_16:
	stw r21+12, r25
.LBB4_17:
	addi r13, r0, 1
.LBB4_18:
	add r1, r13, r0
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
	addi sp, sp, 1112
	jalr r0, r31, 0
.LBB4_19:
	stw r21+8, r1
	ldw r1, r21+12
	bne r1, r22, .LBB4_17
	jal r0, .LBB4_16
.Lfunc_end4:
	.size	db_retract, .Lfunc_end4-db_retract
                                        # -- End function
	.type	pred_table,@object              # @pred_table
	.local	pred_table
	.comm	pred_table,1024,4
	.type	pred_arena_count,@object        # @pred_arena_count
	.local	pred_arena_count
	.comm	pred_arena_count,4,4
	.type	clause_arena_count,@object      # @clause_arena_count
	.local	clause_arena_count
	.comm	clause_arena_count,4,4
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"too many clauses"
	.size	.L.str, 17

	.type	clause_arena,@object            # @clause_arena
	.local	clause_arena
	.comm	clause_arena,65536,4
	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"invalid clause head"
	.size	.L.str.1, 20

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"too many predicates"
	.size	.L.str.2, 20

	.type	pred_arena,@object              # @pred_arena
	.local	pred_arena
	.comm	pred_arena,10240,4
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
