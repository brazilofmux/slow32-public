	.file	"engine.c"
	.text
	.globl	deref                           # -- Begin function deref
	.p2align	2
	.type	deref,@function
deref:                                  # @deref
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	lui r1, %hi(var_count)
	addi r1, r1, %lo(var_count)
	ldw r4, r1+0
	addi r5, r0, 3
	addi r6, r0, 0
	lui r1, 32
	addi r7, r1, -1
	lui r8, %hi(var_binding)
	addi r8, r8, %lo(var_binding)
                                        # implicit-def: $r1
	jal r0, .LBB0_2
.LBB0_1:
	add r9, r6, r0
	add r1, r3, r0
	beq r9, r6, .LBB0_7
.LBB0_2:
	andi r9, r3, 3
	bne r9, r5, .LBB0_6
.LBB0_3:
	bgtu r3, r7, .LBB0_1
.LBB0_4:
	srli r9, r3, 2
	bge r9, r4, .LBB0_1
.LBB0_5:
	slli r9, r9, 2
	add r9, r9, r8
	ldw r10, r9+0
	sne r9, r10, r6
	xor r10, r10, r3
	sub r11, r6, r9
	and r10, r10, r11
	xor r10, r3, r10
	xor r1, r1, r3
	and r1, r1, r11
	xor r1, r3, r1
	add r3, r10, r0
	bne r9, r6, .LBB0_2
	jal r0, .LBB0_7
.LBB0_6:
	add r1, r3, r0
.LBB0_7:
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	deref, .Lfunc_end0-deref
                                        # -- End function
	.globl	bind                            # -- Begin function bind
	.p2align	2
	.type	bind,@function
bind:                                   # @bind
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r1, 8
	addi r1, r1, -1
	bgtu r3, r1, .LBB1_4
.LBB1_1:
	lui r1, %hi(var_count)
	addi r1, r1, %lo(var_count)
	ldw r1, r1+0
	bge r3, r1, .LBB1_4
.LBB1_2:
	lui r5, %hi(trail_top)
	addi r5, r5, %lo(trail_top)
	ldw r1, r5+0
	lui r6, 16
	blt r1, r6, .LBB1_6
.LBB1_3:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	jal r0, .LBB1_5
.LBB1_4:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
.LBB1_5:
	addi r4, r0, 256
	jal r31, snprintf
	jal r0, .LBB1_7
.LBB1_6:
	addi r6, r1, 1
	stw r5+0, r6
	slli r1, r1, 2
	lui r5, %hi(trail)
	addi r5, r5, %lo(trail)
	add r1, r1, r5
	stw r1+0, r3
	slli r1, r3, 2
	lui r3, %hi(var_binding)
	addi r3, r3, %lo(var_binding)
	add r1, r1, r3
	stw r1+0, r4
.LBB1_7:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	bind, .Lfunc_end1-bind
                                        # -- End function
	.globl	trail_undo                      # -- Begin function trail_undo
	.p2align	2
	.type	trail_undo,@function
trail_undo:                             # @trail_undo
# %bb.0:
	lui r1, %hi(trail_top)
	addi r1, r1, %lo(trail_top)
	ldw r4, r1+0
	ble r4, r3, .LBB2_4
.LBB2_1:
	slli r5, r4, 2
	lui r6, %hi(trail-4)
	addi r6, r6, %lo(trail-4)
	add r5, r5, r6
	lui r6, %hi(var_binding)
	addi r6, r6, %lo(var_binding)
	addi r7, r0, 0
.LBB2_2:
	addi r4, r4, -1
	ldw r8, r5+0
	slli r8, r8, 2
	add r8, r8, r6
	stw r8+0, r7
	addi r5, r5, -4
	bgt r4, r3, .LBB2_2
.LBB2_3:
	stw r1+0, r3
.LBB2_4:
	jalr r0, r31, 0
.Lfunc_end2:
	.size	trail_undo, .Lfunc_end2-trail_undo
                                        # -- End function
	.globl	fresh_var                       # -- Begin function fresh_var
	.p2align	2
	.type	fresh_var,@function
fresh_var:                              # @fresh_var
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r3, %hi(var_count)
	addi r3, r3, %lo(var_count)
	ldw r1, r3+0
	lui r4, 8
	blt r1, r4, .LBB3_2
.LBB3_1:
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
	addi r1, r0, 0
	jal r0, .LBB3_3
.LBB3_2:
	addi r4, r1, 1
	stw r3+0, r4
	slli r1, r1, 2
	lui r3, %hi(var_binding)
	addi r3, r3, %lo(var_binding)
	add r3, r1, r3
	addi r4, r0, 0
	stw r3+0, r4
	addi r1, r1, 3
.LBB3_3:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	fresh_var, .Lfunc_end3-fresh_var
                                        # -- End function
	.globl	copy_term                       # -- Begin function copy_term
	.p2align	2
	.type	copy_term,@function
copy_term:                              # @copy_term
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r6, r0, 1
	jal r31, copy_term_impl
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	copy_term, .Lfunc_end4-copy_term
                                        # -- End function
	.p2align	2                               # -- Begin function copy_term_impl
	.type	copy_term_impl,@function
copy_term_impl:                         # @copy_term_impl
# %bb.0:
	addi sp, sp, -184
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 184
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
	add r14, r6, r0
	add r12, r5, r0
	add r11, r4, r0
	addi r1, r0, 0
	beq r6, r1, .LBB5_7
.LBB5_1:
	lui r4, %hi(var_count)
	addi r4, r4, %lo(var_count)
	ldw r4, r4+0
	addi r5, r0, 3
	lui r6, 32
	addi r6, r6, -1
	lui r7, %hi(var_binding)
	addi r7, r7, %lo(var_binding)
                                        # implicit-def: $r13
	jal r0, .LBB5_3
.LBB5_2:
	add r8, r1, r0
	add r13, r3, r0
	beq r8, r1, .LBB5_8
.LBB5_3:
	andi r8, r3, 3
	bne r8, r5, .LBB5_7
.LBB5_4:
	bgtu r3, r6, .LBB5_2
.LBB5_5:
	srli r8, r3, 2
	bge r8, r4, .LBB5_2
.LBB5_6:
	slli r8, r8, 2
	add r8, r8, r7
	ldw r9, r8+0
	sne r8, r9, r1
	xor r9, r9, r3
	sub r10, r1, r8
	and r9, r9, r10
	xor r9, r3, r9
	xor r13, r13, r3
	and r10, r13, r10
	xor r13, r3, r10
	add r3, r9, r0
	bne r8, r1, .LBB5_3
	jal r0, .LBB5_8
.LBB5_7:
	add r13, r3, r0
.LBB5_8:
	andi r3, r13, 3
	addi r1, r3, -1
	addi r4, r0, 2
	bgeu r1, r4, .LBB5_10
.LBB5_9:
	add r1, r13, r0
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
	addi sp, sp, 184
	jalr r0, r31, 0
.LBB5_10:
	addi r1, r0, 0
	bne r3, r1, .LBB5_14
.LBB5_11:
	addi r17, r0, 0
	beq r13, r17, .LBB5_18
.LBB5_12:
	add r3, r13, r0
	jal r31, compound_functor
	add r15, r1, r0
	add r3, r13, r0
	jal r31, compound_arity
	add r16, r1, r0
	addi r1, r0, 33
	bltu r16, r1, .LBB5_19
.LBB5_13:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.4)
	addi r5, r5, %lo(.L.str.4)
	addi r4, r0, 256
	jal r31, snprintf
	add r13, r17, r0
	jal r0, .LBB5_9
.LBB5_14:
	srli r3, r13, 2
	bge r3, r12, .LBB5_9
.LBB5_15:
	slli r3, r3, 2
	add r3, r11, r3
	ldw r4, r3+0
	addi r5, r0, -1
	bne r4, r5, .LBB5_27
.LBB5_16:
	lui r5, %hi(var_count)
	addi r5, r5, %lo(var_count)
	ldw r4, r5+0
	lui r6, 8
	blt r4, r6, .LBB5_26
.LBB5_17:
	lui r3, %hi(g_error)
	addi r3, r3, %lo(g_error)
	addi r4, r0, 1
	stw r3+0, r4
	jal r0, .LBB5_24
.LBB5_18:
	add r13, r17, r0
	jal r0, .LBB5_9
.LBB5_19:
	addi r18, r0, 0
	beq r16, r18, .LBB5_23
.LBB5_20:
	addi r19, fp, -172
	lui r20, %hi(g_error)
	addi r20, r20, %lo(g_error)
	add r17, r18, r0
.LBB5_21:
	add r3, r13, r0
	add r4, r17, r0
	jal r31, compound_arg
	add r3, r1, r0
	add r4, r11, r0
	add r5, r12, r0
	add r6, r14, r0
	jal r31, copy_term_impl
	stw r19+0, r1
	ldw r1, r20+0
	bne r1, r18, .LBB5_25
.LBB5_22:
	addi r17, r17, 1
	addi r19, r19, 4
	bne r16, r17, .LBB5_21
.LBB5_23:
	addi r5, fp, -172
	add r3, r15, r0
	add r4, r16, r0
	jal r31, make_compound
.LBB5_24:
	add r13, r1, r0
	jal r0, .LBB5_9
.LBB5_25:
	add r13, r18, r0
	jal r0, .LBB5_9
.LBB5_26:
	addi r6, r4, 1
	stw r5+0, r6
	stw r3+0, r4
	slli r4, r4, 2
	lui r5, %hi(var_binding)
	addi r5, r5, %lo(var_binding)
	add r4, r4, r5
	stw r4+0, r1
.LBB5_27:
	ldw r1, r3+0
	slli r1, r1, 2
	addi r13, r1, 3
	jal r0, .LBB5_9
.Lfunc_end5:
	.size	copy_term_impl, .Lfunc_end5-copy_term_impl
                                        # -- End function
	.globl	copy_term_code                  # -- Begin function copy_term_code
	.p2align	2
	.type	copy_term_code,@function
copy_term_code:                         # @copy_term_code
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r6, r0, 0
	jal r31, copy_term_impl
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	copy_term_code, .Lfunc_end6-copy_term_code
                                        # -- End function
	.globl	engine_reset                    # -- Begin function engine_reset
	.p2align	2
	.type	engine_reset,@function
engine_reset:                           # @engine_reset
# %bb.0:
	lui r3, %hi(trail_top)
	addi r3, r3, %lo(trail_top)
	addi r1, r0, 0
	stw r3+0, r1
	lui r3, %hi(goal_sp)
	addi r3, r3, %lo(goal_sp)
	stw r3+0, r1
	lui r3, %hi(choice_top)
	addi r3, r3, %lo(choice_top)
	stw r3+0, r1
	lui r3, %hi(cut_barrier)
	addi r3, r3, %lo(cut_barrier)
	stw r3+0, r1
	lui r3, %hi(hp)
	addi r3, r3, %lo(hp)
	addi r5, r0, 1
	stw r3+0, r5
	lui r3, %hi(var_count)
	addi r3, r3, %lo(var_count)
	ldw r4, r3+0
	blt r4, r5, .LBB7_3
.LBB7_1:
	lui r5, 8
	xor r6, r4, r5
	slt r4, r4, r5
	sub r4, r1, r4
	and r4, r6, r4
	xor r4, r4, r5
	lui r5, %hi(var_binding)
	addi r5, r5, %lo(var_binding)
.LBB7_2:
	stw r5+0, r1
	addi r4, r4, -1
	addi r5, r5, 4
	bne r4, r1, .LBB7_2
.LBB7_3:
	stw r3+0, r1
	jalr r0, r31, 0
.Lfunc_end7:
	.size	engine_reset, .Lfunc_end7-engine_reset
                                        # -- End function
	.globl	solve                           # -- Begin function solve
	.p2align	2
	.type	solve,@function
solve:                                  # @solve
# %bb.0:
	addi sp, sp, -2048
	addi sp, sp, -152
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 2047
	addi fp, fp, 153
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
	lui r17, %hi(goal_sp)
	addi r17, r17, %lo(goal_sp)
	lui r18, %hi(choice_top)
	addi r18, r18, %lo(choice_top)
	addi r11, r0, 0
	stw r18+0, r11
	lui r8, %hi(cut_barrier)
	addi r8, r8, %lo(cut_barrier)
	stw r8+0, r11
	addi r20, r0, 1
	stw r17+0, r20
	lui r21, %hi(goal_stack)
	addi r21, r21, %lo(goal_stack)
	stw r21+0, r3
	lui r27, %hi(g_error)
	addi r27, r27, %lo(g_error)
	ldw r1, r27+0
	bne r1, r11, .LBB8_134
.LBB8_1:
	lui r23, %hi(var_count)
	addi r23, r23, %lo(var_count)
	addi r24, r0, 3
	addi r11, r0, 0
	lui r1, 32
	addi r25, r1, -1
	lui r26, %hi(var_binding)
	addi r26, r26, %lo(var_binding)
	addi r1, r0, 2
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-100, r1
	lui r1, %hi(ATOM_TRUE)
	addi r1, r1, %lo(ATOM_TRUE)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-108, r1
	lui r1, %hi(ATOM_FAIL)
	addi r1, r1, %lo(ATOM_FAIL)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-112, r1
	addi r1, r0, 28
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-88, r1
	lui r1, %hi(choices)
	addi r1, r1, %lo(choices)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-84, r1
	lui r28, %hi(trail_top)
	addi r28, r28, %lo(trail_top)
	lui r1, %hi(trail-4)
	addi r1, r1, %lo(trail-4)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-104, r1
	lui r1, %hi(hp)
	addi r1, r1, %lo(hp)
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-80, r1
	add r1, fp, r0
	addi r1, r1, -2048
	stw r1+-92, r8
.LBB8_2:
	ldw r1, r17+0
	blt r1, r20, .LBB8_133
.LBB8_3:
	addi r1, r1, -1
	stw r17+0, r1
	slli r1, r1, 2
	add r1, r1, r21
	ldw r1, r1+0
	ldw r3, r23+0
                                        # implicit-def: $r13
	jal r0, .LBB8_5
.LBB8_4:
	add r4, r11, r0
	add r13, r1, r0
	beq r4, r11, .LBB8_10
.LBB8_5:
	andi r4, r1, 3
	bne r4, r24, .LBB8_9
.LBB8_6:
	bgtu r1, r25, .LBB8_4
.LBB8_7:
	srli r4, r1, 2
	bge r4, r3, .LBB8_4
.LBB8_8:
	slli r4, r4, 2
	add r4, r4, r26
	ldw r5, r4+0
	sne r4, r5, r11
	xor r5, r5, r1
	sub r6, r11, r4
	and r5, r5, r6
	xor r5, r1, r5
	xor r7, r13, r1
	and r6, r7, r6
	xor r13, r1, r6
	add r1, r5, r0
	bne r4, r11, .LBB8_5
	jal r0, .LBB8_10
.LBB8_9:
	add r13, r1, r0
.LBB8_10:
	add r12, r20, r0
	beq r13, r11, .LBB8_131
.LBB8_11:
	andi r15, r13, 3
	lui r1, 1
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-96, r1
	lui r19, %hi(ATOM_COMMA)
	addi r19, r19, %lo(ATOM_COMMA)
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-100
	bne r15, r1, .LBB8_14
.LBB8_12:
	srli r1, r13, 2
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-108
	ldw r3, r3+0
	add r12, r20, r0
	beq r1, r3, .LBB8_131
.LBB8_13:
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-112
	ldw r3, r3+0
	beq r1, r3, .LBB8_100
.LBB8_14:
	addi r14, r0, 0
	beq r15, r14, .LBB8_27
.LBB8_15:
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-100
	bne r15, r1, .LBB8_19
.LBB8_16:
	srli r1, r13, 2
	lui r3, %hi(ATOM_CUT)
	addi r3, r3, %lo(ATOM_CUT)
	ldw r3, r3+0
	bne r1, r3, .LBB8_19
.LBB8_17:
	ldw r3, r18+0
	ldw r1, r8+0
	add r12, r20, r0
	ble r3, r1, .LBB8_131
.LBB8_18:
	stw r18+0, r1
	add r12, r20, r0
	jal r0, .LBB8_131
.LBB8_19:
	add r3, r13, r0
	jal r31, term_functor
	add r15, r1, r0
	add r3, r13, r0
	jal r31, term_arity
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r8, r3+-92
	addi r12, r0, -1
	ble r15, r12, .LBB8_100
.LBB8_20:
	add r16, r1, r0
	add r3, r13, r0
	add r4, r15, r0
	add r5, r1, r0
	jal r31, try_builtin
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r8, r3+-92
	seq r3, r1, r12
	sub r3, r14, r3
	andi r3, r3, 4
	addi r22, r0, 1
	seq r12, r1, r22
	sub r1, r14, r12
	ori  r4, r3, 3
	and r1, r4, r1
	xor r1, r3, r1
	addi r4, r0, 4
	beq r1, r4, .LBB8_100
.LBB8_21:
	bne r1, r14, .LBB8_131
.LBB8_22:
	add r3, r15, r0
	add r1, fp, r0
	addi r1, r1, -2048
	stw r1+-116, r4
	add r4, r16, r0
	jal r31, db_lookup
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r4, r3+-116
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r8, r3+-92
	add r3, r4, r0
	beq r1, r14, .LBB8_86
.LBB8_23:
	ldw r16, r1+8
	add r3, r4, r0
	beq r16, r14, .LBB8_86
.LBB8_24:
	ldw r1, r16+12
	beq r1, r14, .LBB8_82
.LBB8_25:
	ldw r3, r18+0
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-96
	addi r4, r4, -2048
	blt r3, r4, .LBB8_77
.LBB8_26:
	addi r3, r0, 1
	stw r27+0, r3
	jal r0, .LBB8_97
.LBB8_27:
	add r3, r13, r0
	jal r31, compound_functor
	ldw r3, r19+0
	bne r1, r3, .LBB8_31
.LBB8_28:
	add r3, r13, r0
	jal r31, compound_arity
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-100
	bne r1, r3, .LBB8_31
.LBB8_29:
	addi r12, r0, 0
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r14, r1, r0
	addi r15, r0, 1
	add r3, r13, r0
	add r4, r15, r0
	jal r31, compound_arg
	ldw r3, r17+0
	lui r4, 4
	addi r4, r4, -1
	blt r3, r4, .LBB8_61
.LBB8_30:
	addi r1, r0, 1
	stw r27+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	addi r4, r0, 256
	jal r31, snprintf
	jal r0, .LBB8_76
.LBB8_31:
	add r3, r13, r0
	jal r31, compound_functor
	lui r3, %hi(ATOM_SEMI)
	addi r3, r3, %lo(ATOM_SEMI)
	ldw r3, r3+0
	bne r1, r3, .LBB8_39
.LBB8_32:
	add r3, r13, r0
	jal r31, compound_arity
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-100
	bne r1, r3, .LBB8_39
.LBB8_33:
	addi r14, r0, 0
	add r3, r13, r0
	add r4, r14, r0
	jal r31, compound_arg
	add r12, r1, r0
	addi r15, r0, 1
	add r3, r13, r0
	add r4, r15, r0
	jal r31, compound_arg
	add r13, r1, r0
	ldw r1, r23+0
	add r3, r12, r0
                                        # implicit-def: $r16
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r8, r4+-84
	jal r0, .LBB8_35
.LBB8_34:
	add r5, r4, r0
	add r16, r3, r0
	beq r5, r4, .LBB8_63
.LBB8_35:
	andi r4, r3, 3
	bne r4, r24, .LBB8_62
.LBB8_36:
	addi r4, r0, 0
	bgtu r3, r25, .LBB8_34
.LBB8_37:
	srli r5, r3, 2
	bge r5, r1, .LBB8_34
.LBB8_38:
	slli r5, r5, 2
	add r5, r5, r26
	ldw r6, r5+0
	addi r7, r0, 0
	sne r5, r6, r7
	xor r6, r6, r3
	sub r7, r7, r5
	and r6, r6, r7
	xor r6, r3, r6
	xor r8, r16, r3
	and r7, r8, r7
	add r9, fp, r0
	addi r9, r9, -2048
	ldw r8, r9+-84
	xor r16, r3, r7
	add r3, r6, r0
	bne r5, r4, .LBB8_35
	jal r0, .LBB8_63
.LBB8_39:
	add r3, r13, r0
	jal r31, compound_functor
	lui r3, %hi(ATOM_ARROW)
	addi r3, r3, %lo(ATOM_ARROW)
	ldw r3, r3+0
	bne r1, r3, .LBB8_42
.LBB8_40:
	add r3, r13, r0
	jal r31, compound_arity
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-100
	bne r1, r3, .LBB8_42
.LBB8_41:
	addi r12, r0, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, compound_arg
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r3, r3, 2
	add r3, r3, r21
	stw r3+0, r1
	addi r4, r0, 0
	add r3, r13, r0
	jal r31, compound_arg
	ldw r3, r17+0
	addi r4, r3, 1
	stw r17+0, r4
	slli r3, r3, 2
	add r3, r3, r21
	stw r3+0, r1
	jal r0, .LBB8_76
.LBB8_42:
	add r3, r13, r0
	jal r31, compound_functor
	lui r3, %hi(ATOM_NOT)
	addi r3, r3, %lo(ATOM_NOT)
	ldw r3, r3+0
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r8, r4+-92
	bne r1, r3, .LBB8_15
.LBB8_43:
	add r3, r13, r0
	jal r31, compound_arity
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r8, r3+-92
	addi r12, r0, 1
	bne r1, r12, .LBB8_15
.LBB8_44:
	addi r14, r0, 0
	add r3, r13, r0
	add r4, r14, r0
	jal r31, compound_arg
	add r9, r14, r0
	ldw r6, r17+0
	addi r3, r0, 64
	slt r4, r6, r3
	sub r4, r14, r4
	xori r5, r6, 64
	and r4, r5, r4
	xori r14, r4, 64
	add r4, fp, r0
	addi r4, r4, -2048
	stw r4+-120, r6
	blt r6, r12, .LBB8_47
.LBB8_45:
	addi r4, r0, 0
	add r5, r4, r0
	add r6, r14, r0
.LBB8_46:
	add r7, r5, r21
	ldw r7, r7+0
	addi r8, fp, -332
	add r8, r8, r5
	stw r8+0, r7
	addi r6, r6, -1
	addi r5, r5, 4
	bne r6, r4, .LBB8_46
.LBB8_47:
	ldw r22, r28+0
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-80
	ldw r4, r4+0
	add r5, fp, r0
	addi r5, r5, -2048
	stw r5+-132, r4
	ldw r4, r23+0
	add r5, fp, r0
	addi r5, r5, -2048
	stw r5+-136, r4
	ldw r5, r18+0
	add r6, fp, r0
	addi r6, r6, -2048
	ldw r4, r6+-92
	ldw r4, r4+0
	add r6, fp, r0
	addi r6, r6, -2048
	stw r6+-128, r4
	slt r3, r5, r3
	add r4, fp, r0
	addi r4, r4, -2048
	stw r4+-116, r9
	sub r3, r9, r3
	xori r4, r5, 64
	and r3, r4, r3
	xori r13, r3, 64
	lui r15, %hi(choices+24)
	addi r15, r15, %lo(choices+24)
	lui r16, %hi(choices+4)
	addi r16, r16, %lo(choices+4)
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r9, r3+-84
	add r3, fp, r0
	addi r3, r3, -2048
	stw r3+-124, r5
	blt r5, r12, .LBB8_50
.LBB8_48:
	addi r3, r0, 0
	add r4, r3, r0
	add r5, r13, r0
.LBB8_49:
	add r7, fp, r0
	addi r7, r7, -2048
	addi r6, r7, -76
	add r6, r6, r4
	add r7, r4, r9
	add r8, r4, r15
	ldw r8, r8+0
	stw r6+24, r8
	lui r8, %hi(choices+20)
	addi r8, r8, %lo(choices+20)
	add r8, r4, r8
	ldw r8, r8+0
	stw r6+20, r8
	lui r8, %hi(choices+16)
	addi r8, r8, %lo(choices+16)
	add r8, r4, r8
	ldw r8, r8+0
	stw r6+16, r8
	lui r8, %hi(choices+12)
	addi r8, r8, %lo(choices+12)
	add r8, r4, r8
	ldw r8, r8+0
	stw r6+12, r8
	lui r8, %hi(choices+8)
	addi r8, r8, %lo(choices+8)
	add r8, r4, r8
	ldw r8, r8+0
	stw r6+8, r8
	add r8, r4, r16
	ldw r8, r8+0
	stw r6+4, r8
	ldw r7, r7+0
	stw r6+0, r7
	addi r5, r5, -1
	addi r4, r4, 28
	bne r5, r3, .LBB8_49
.LBB8_50:
	add r3, r1, r0
	jal r31, solve
	ldw r3, r28+0
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r8, r4+-92
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r7, r4+-124
	ble r3, r22, .LBB8_54
.LBB8_51:
	slli r4, r3, 2
	add r6, fp, r0
	addi r6, r6, -2048
	ldw r5, r6+-104
	add r4, r4, r5
.LBB8_52:
	addi r3, r3, -1
	ldw r5, r4+0
	slli r5, r5, 2
	add r5, r5, r26
	addi r6, r0, 0
	stw r5+0, r6
	addi r4, r4, -4
	bgt r3, r22, .LBB8_52
.LBB8_53:
	stw r28+0, r22
.LBB8_54:
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-80
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-132
	stw r3+0, r4
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-136
	stw r23+0, r3
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-120
	stw r17+0, r3
	stw r18+0, r7
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-128
	stw r8+0, r4
	blt r3, r12, .LBB8_57
.LBB8_55:
	addi r3, r0, 0
	add r4, r3, r0
.LBB8_56:
	addi r5, fp, -332
	add r5, r5, r4
	ldw r5, r5+0
	add r6, r4, r21
	stw r6+0, r5
	addi r14, r14, -1
	addi r4, r4, 4
	bne r14, r3, .LBB8_56
.LBB8_57:
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r9, r3+-84
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r10, r3+-116
	blt r7, r12, .LBB8_60
.LBB8_58:
	add r3, r10, r0
.LBB8_59:
	add r4, r3, r9
	add r6, fp, r0
	addi r6, r6, -2048
	addi r5, r6, -76
	add r5, r5, r3
	add r6, r3, r15
	ldw r7, r5+24
	stw r6+0, r7
	lui r6, %hi(choices+20)
	addi r6, r6, %lo(choices+20)
	add r6, r3, r6
	ldw r7, r5+20
	stw r6+0, r7
	lui r6, %hi(choices+16)
	addi r6, r6, %lo(choices+16)
	add r6, r3, r6
	ldw r7, r5+16
	stw r6+0, r7
	lui r6, %hi(choices+12)
	addi r6, r6, %lo(choices+12)
	add r6, r3, r6
	ldw r7, r5+12
	stw r6+0, r7
	lui r6, %hi(choices+8)
	addi r6, r6, %lo(choices+8)
	add r6, r3, r6
	ldw r7, r5+8
	stw r6+0, r7
	add r6, r3, r16
	ldw r7, r5+4
	stw r6+0, r7
	ldw r5, r5+0
	stw r4+0, r5
	addi r13, r13, -1
	addi r3, r3, 28
	bne r13, r10, .LBB8_59
.LBB8_60:
	bne r1, r10, .LBB8_100
	jal r0, .LBB8_131
.LBB8_61:
	addi r4, r3, 1
	stw r17+0, r4
	slli r5, r3, 2
	add r5, r5, r21
	stw r5+0, r1
	addi r1, r3, 2
	stw r17+0, r1
	slli r1, r4, 2
	add r1, r1, r21
	stw r1+0, r14
	add r12, r15, r0
	jal r0, .LBB8_76
.LBB8_62:
	add r16, r3, r0
.LBB8_63:
	beq r16, r14, .LBB8_69
.LBB8_64:
	andi r1, r16, 3
	addi r3, r0, 0
	bne r1, r3, .LBB8_69
.LBB8_65:
	add r3, r16, r0
	jal r31, compound_functor
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r8, r3+-84
	lui r3, %hi(ATOM_ARROW)
	addi r3, r3, %lo(ATOM_ARROW)
	ldw r3, r3+0
	bne r1, r3, .LBB8_69
.LBB8_66:
	add r3, r16, r0
	jal r31, compound_arity
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r8, r3+-84
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-100
	bne r1, r3, .LBB8_69
.LBB8_67:
	addi r12, r0, 0
	add r3, r16, r0
	add r4, r12, r0
	jal r31, compound_arg
	add r14, r1, r0
	addi r15, r0, 1
	add r3, r16, r0
	add r4, r15, r0
	jal r31, compound_arg
	add r16, r1, r0
	ldw r1, r18+0
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-96
	addi r3, r3, -2048
	blt r1, r3, .LBB8_95
.LBB8_68:
	addi r1, r0, 1
	stw r27+0, r1
	jal r0, .LBB8_76
.LBB8_69:
	ldw r1, r18+0
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-96
	addi r3, r3, -2048
	blt r1, r3, .LBB8_71
.LBB8_70:
	addi r1, r0, 1
	stw r27+0, r1
	addi r12, r0, 0
	jal r0, .LBB8_76
.LBB8_71:
	addi r3, r1, 1
	stw r18+0, r3
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-88
	mul r1, r1, r3
	add r16, r1, r8
	ldw r3, r28+0
	stw r16+0, r3
	ldw r3, r23+0
	lui r4, %hi(choices+8)
	addi r4, r4, %lo(choices+8)
	add r4, r1, r4
	stw r4+0, r3
	lui r3, %hi(choices+12)
	addi r3, r3, %lo(choices+12)
	add r3, r1, r3
	stw r3+0, r14
	lui r3, %hi(choices+16)
	addi r3, r3, %lo(choices+16)
	add r3, r1, r3
	stw r3+0, r13
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-92
	ldw r3, r3+0
	lui r4, %hi(choices+20)
	addi r4, r4, %lo(choices+20)
	add r1, r1, r4
	stw r1+0, r3
	ldw r1, r17+0
	blt r1, r15, .LBB8_75
.LBB8_72:
	ldw r14, r21+0
	beq r1, r15, .LBB8_75
.LBB8_73:
	lui r13, %hi(goal_stack+4)
	addi r13, r13, %lo(goal_stack+4)
.LBB8_74:
	ldw r1, r13+0
	add r3, fp, r0
	addi r3, r3, -2048
	addi r5, r3, -76
	stw r5+0, r1
	stw r5+4, r14
	ldw r3, r19+0
	addi r4, r0, 2
	jal r31, make_compound
	add r14, r1, r0
	addi r15, r15, 1
	ldw r1, r17+0
	addi r13, r13, 4
	blt r15, r1, .LBB8_74
.LBB8_75:
	stw r16+24, r14
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-80
	ldw r1, r1+0
	stw r16+4, r1
	ldw r1, r17+0
	addi r3, r1, 1
	stw r17+0, r3
	slli r1, r1, 2
	add r1, r1, r21
	stw r1+0, r12
	addi r12, r0, 1
.LBB8_76:
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r8, r1+-92
	jal r0, .LBB8_131
.LBB8_77:
	addi r4, r3, 1
	stw r18+0, r4
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-88
	mul r3, r3, r4
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-84
	add r12, r3, r4
	ldw r4, r28+0
	stw r12+0, r4
	ldw r4, r23+0
	lui r5, %hi(choices+8)
	addi r5, r5, %lo(choices+8)
	add r5, r3, r5
	stw r5+0, r4
	lui r4, %hi(choices+12)
	addi r4, r4, %lo(choices+12)
	add r4, r3, r4
	stw r4+0, r1
	lui r1, %hi(choices+16)
	addi r1, r1, %lo(choices+16)
	add r1, r3, r1
	stw r1+0, r13
	ldw r1, r8+0
	lui r4, %hi(choices+20)
	addi r4, r4, %lo(choices+20)
	add r3, r3, r4
	stw r3+0, r1
	ldw r1, r17+0
	blt r1, r22, .LBB8_81
.LBB8_78:
	ldw r14, r21+0
	beq r1, r22, .LBB8_81
.LBB8_79:
	lui r15, %hi(goal_stack+4)
	addi r15, r15, %lo(goal_stack+4)
.LBB8_80:
	ldw r1, r15+0
	add r3, fp, r0
	addi r3, r3, -2048
	addi r5, r3, -76
	stw r5+0, r1
	stw r5+4, r14
	ldw r3, r19+0
	addi r4, r0, 2
	jal r31, make_compound
	add r14, r1, r0
	addi r22, r22, 1
	ldw r1, r17+0
	addi r15, r15, 4
	blt r22, r1, .LBB8_80
.LBB8_81:
	stw r12+24, r14
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-80
	ldw r1, r1+0
	stw r12+4, r1
.LBB8_82:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r12, r1, -76
	addi r4, r0, -1
	addi r5, r0, 1024
	add r3, r12, r0
	jal r31, memset
	ldw r3, r16+0
	addi r5, r0, 256
	addi r14, r0, 0
	add r4, r12, r0
	add r6, r14, r0
	jal r31, copy_term_impl
	add r15, r1, r0
	ldw r3, r16+4
	add r12, r14, r0
	beq r3, r14, .LBB8_84
.LBB8_83:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r4, r1, -76
	addi r5, r0, 256
	addi r6, r0, 0
	jal r31, copy_term_impl
	add r12, r1, r0
.LBB8_84:
	ldw r1, r27+0
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r8, r3+-92
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r4, r3+-116
	beq r1, r14, .LBB8_90
.LBB8_85:
	addi r3, r0, 1
.LBB8_86:
	beq r3, r4, .LBB8_100
.LBB8_87:
	addi r1, r0, 0
	bne r3, r1, .LBB8_89
.LBB8_88:
	addi r12, r0, 1
	jal r0, .LBB8_131
.LBB8_89:
	addi r12, r0, 0
	jal r0, .LBB8_131
.LBB8_90:
	add r3, r13, r0
	add r4, r15, r0
	jal r31, unify
	addi r3, r0, 0
	beq r1, r3, .LBB8_96
.LBB8_91:
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r8, r1+-92
	beq r12, r3, .LBB8_97
.LBB8_92:
	andi r1, r12, 3
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-100
	bne r1, r4, .LBB8_94
.LBB8_93:
	srli r1, r12, 2
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-108
	ldw r4, r4+0
	beq r1, r4, .LBB8_97
.LBB8_94:
	ldw r1, r17+0
	addi r4, r1, 1
	stw r17+0, r4
	slli r1, r1, 2
	add r1, r1, r21
	stw r1+0, r12
	jal r0, .LBB8_97
.LBB8_95:
	addi r3, r1, 1
	stw r18+0, r3
	add r4, fp, r0
	addi r4, r4, -2048
	ldw r3, r4+-88
	mul r19, r1, r3
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-84
	add r1, r19, r1
	ldw r3, r28+0
	stw r1+0, r3
	ldw r1, r23+0
	lui r3, %hi(choices+8)
	addi r3, r3, %lo(choices+8)
	add r3, r19, r3
	stw r3+0, r1
	lui r1, %hi(choices+12)
	addi r1, r1, %lo(choices+12)
	add r1, r19, r1
	stw r1+0, r12
	lui r1, %hi(choices+16)
	addi r1, r1, %lo(choices+16)
	add r1, r19, r1
	stw r1+0, r13
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r12, r1+-92
	ldw r1, r12+0
	lui r3, %hi(choices+20)
	addi r3, r3, %lo(choices+20)
	add r3, r19, r3
	stw r3+0, r1
	jal r31, build_continuation
	add r8, r12, r0
	lui r3, %hi(choices+24)
	addi r3, r3, %lo(choices+24)
	add r3, r19, r3
	stw r3+0, r1
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-80
	ldw r1, r1+0
	lui r3, %hi(choices+4)
	addi r3, r3, %lo(choices+4)
	add r3, r19, r3
	stw r3+0, r1
	ldw r1, r17+0
	addi r3, r1, 1
	stw r17+0, r3
	slli r4, r1, 2
	add r4, r4, r21
	stw r4+0, r16
	lui r4, %hi(ATOM_CUT)
	addi r4, r4, %lo(ATOM_CUT)
	ldw r4, r4+0
	slli r4, r4, 2
	addi r4, r4, 2
	addi r5, r1, 2
	stw r17+0, r5
	slli r3, r3, 2
	add r3, r3, r21
	stw r3+0, r4
	addi r1, r1, 3
	stw r17+0, r1
	slli r1, r5, 2
	add r1, r1, r21
	stw r1+0, r14
	add r12, r15, r0
	jal r0, .LBB8_131
.LBB8_96:
	addi r3, r0, 4
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r8, r1+-92
.LBB8_97:
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r4, r1+-116
	bne r3, r4, .LBB8_87
	jal r0, .LBB8_100
.LBB8_98:
	ldw r1, r17+0
	addi r3, r1, 1
	stw r17+0, r3
	slli r1, r1, 2
	add r1, r1, r21
	stw r1+0, r13
	add r3, r24, r0
.LBB8_99:
	addi r1, r0, 4
	bne r3, r1, .LBB8_127
.LBB8_100:
	ldw r3, r18+0
	sgt r12, r3, r11
	blt r3, r20, .LBB8_131
.LBB8_101:
	addi r1, r3, -1
	stw r18+0, r1
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-88
	mul r1, r1, r4
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-84
	add r14, r1, r4
	ldw r1, r14+0
	ldw r4, r28+0
	ble r4, r1, .LBB8_105
.LBB8_102:
	slli r5, r4, 2
	add r7, fp, r0
	addi r7, r7, -2048
	ldw r6, r7+-104
	add r5, r5, r6
.LBB8_103:
	addi r4, r4, -1
	ldw r6, r5+0
	slli r6, r6, 2
	add r6, r6, r26
	stw r6+0, r11
	addi r5, r5, -4
	bgt r4, r1, .LBB8_103
.LBB8_104:
	stw r28+0, r1
.LBB8_105:
	ldw r1, r14+4
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-80
	stw r4+0, r1
	ldw r1, r14+8
	stw r23+0, r1
	ldw r1, r14+20
	stw r8+0, r1
	stw r17+0, r11
	ldw r1, r14+24
	beq r1, r11, .LBB8_107
.LBB8_106:
	addi r4, r0, 1
	stw r17+0, r4
	stw r21+0, r1
.LBB8_107:
	ldw r22, r14+12
	ldw r13, r14+16
	beq r22, r11, .LBB8_98
.LBB8_108:
	ldw r4, r22+12
	addi r1, r0, 0
	beq r4, r1, .LBB8_116
.LBB8_109:
	add r6, fp, r0
	addi r6, r6, -2048
	ldw r5, r6+-96
	addi r5, r5, -2047
	blt r3, r5, .LBB8_111
.LBB8_110:
	addi r3, r0, 1
	stw r27+0, r3
	jal r0, .LBB8_99
.LBB8_111:
	stw r18+0, r3
	ldw r3, r28+0
	stw r14+0, r3
	stw r14+12, r4
	ldw r3, r17+0
	addi r15, r0, 1
	blt r3, r15, .LBB8_115
.LBB8_112:
	ldw r1, r21+0
	beq r3, r15, .LBB8_115
.LBB8_113:
	lui r16, %hi(goal_stack+4)
	addi r16, r16, %lo(goal_stack+4)
.LBB8_114:
	ldw r3, r16+0
	add r4, fp, r0
	addi r4, r4, -2048
	addi r5, r4, -76
	stw r5+0, r3
	stw r5+4, r1
	ldw r3, r19+0
	addi r4, r0, 2
	jal r31, make_compound
	addi r15, r15, 1
	ldw r3, r17+0
	addi r16, r16, 4
	blt r15, r3, .LBB8_114
.LBB8_115:
	stw r14+24, r1
	add r3, fp, r0
	addi r3, r3, -2048
	ldw r1, r3+-80
	ldw r1, r1+0
	stw r14+4, r1
.LBB8_116:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r14, r1, -76
	addi r4, r0, -1
	addi r5, r0, 1024
	add r3, r14, r0
	jal r31, memset
	ldw r3, r22+0
	addi r5, r0, 256
	addi r15, r0, 0
	add r4, r14, r0
	add r6, r15, r0
	jal r31, copy_term_impl
	add r16, r1, r0
	ldw r3, r22+4
	add r14, r15, r0
	beq r3, r15, .LBB8_118
.LBB8_117:
	add r1, fp, r0
	addi r1, r1, -2048
	addi r4, r1, -76
	addi r5, r0, 256
	addi r6, r0, 0
	jal r31, copy_term_impl
	add r14, r1, r0
.LBB8_118:
	ldw r1, r27+0
	beq r1, r15, .LBB8_121
.LBB8_119:
	addi r3, r0, 1
.LBB8_120:
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r8, r1+-92
	jal r0, .LBB8_99
.LBB8_121:
	add r3, r13, r0
	add r4, r16, r0
	jal r31, unify
	addi r3, r0, 0
	beq r1, r3, .LBB8_126
.LBB8_122:
	add r1, fp, r0
	addi r1, r1, -2048
	ldw r8, r1+-92
	beq r14, r3, .LBB8_99
.LBB8_123:
	andi r1, r14, 3
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-100
	bne r1, r4, .LBB8_125
.LBB8_124:
	srli r1, r14, 2
	add r5, fp, r0
	addi r5, r5, -2048
	ldw r4, r5+-108
	ldw r4, r4+0
	beq r1, r4, .LBB8_99
.LBB8_125:
	ldw r1, r17+0
	addi r4, r1, 1
	stw r17+0, r4
	slli r1, r1, 2
	add r1, r1, r21
	stw r1+0, r14
	jal r0, .LBB8_99
.LBB8_126:
	addi r3, r0, 4
	jal r0, .LBB8_120
.LBB8_127:
	addi r1, r0, 0
	beq r3, r1, .LBB8_131
.LBB8_128:
	bne r3, r24, .LBB8_130
.LBB8_129:
	add r12, r20, r0
	jal r0, .LBB8_131
.LBB8_130:
	add r12, r1, r0
.LBB8_131:
	beq r12, r11, .LBB8_134
.LBB8_132:
	ldw r1, r27+0
	beq r1, r11, .LBB8_2
	jal r0, .LBB8_134
.LBB8_133:
	add r11, r20, r0
.LBB8_134:
	add r1, r11, r0
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
	addi sp, sp, 153
	jalr r0, r31, 0
.Lfunc_end8:
	.size	solve, .Lfunc_end8-solve
                                        # -- End function
	.p2align	2                               # -- Begin function build_continuation
	.type	build_continuation,@function
build_continuation:                     # @build_continuation
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
	blt r1, r14, .LBB9_4
.LBB9_1:
	lui r1, %hi(goal_stack)
	addi r1, r1, %lo(goal_stack)
	ldw r1, r1+0
	ldw r3, r13+0
	addi r4, r0, 2
	blt r3, r4, .LBB9_5
.LBB9_2:
	lui r15, %hi(goal_stack+4)
	addi r15, r15, %lo(goal_stack+4)
	addi r11, fp, -36
	lui r16, %hi(ATOM_COMMA)
	addi r16, r16, %lo(ATOM_COMMA)
	addi r12, r0, 2
.LBB9_3:
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
	blt r14, r3, .LBB9_3
	jal r0, .LBB9_5
.LBB9_4:
	addi r1, r0, 0
.LBB9_5:
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
.Lfunc_end9:
	.size	build_continuation, .Lfunc_end9-build_continuation
                                        # -- End function
	.type	var_count,@object               # @var_count
	.bss
	.globl	var_count
	.p2align	2, 0x0
var_count:
	.word	0                               # 0x0
	.size	var_count, 4

	.type	trail_top,@object               # @trail_top
	.globl	trail_top
	.p2align	2, 0x0
trail_top:
	.word	0                               # 0x0
	.size	trail_top, 4

	.type	goal_sp,@object                 # @goal_sp
	.globl	goal_sp
	.p2align	2, 0x0
goal_sp:
	.word	0                               # 0x0
	.size	goal_sp, 4

	.type	choice_top,@object              # @choice_top
	.globl	choice_top
	.p2align	2, 0x0
choice_top:
	.word	0                               # 0x0
	.size	choice_top, 4

	.type	cut_barrier,@object             # @cut_barrier
	.globl	cut_barrier
	.p2align	2, 0x0
cut_barrier:
	.word	0                               # 0x0
	.size	cut_barrier, 4

	.type	var_binding,@object             # @var_binding
	.globl	var_binding
	.p2align	2, 0x0
var_binding:
	.zero	131072
	.size	var_binding, 131072

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"invalid variable id"
	.size	.L.str, 20

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"trail overflow"
	.size	.L.str.1, 15

	.type	trail,@object                   # @trail
	.bss
	.globl	trail
	.p2align	2, 0x0
trail:
	.zero	262144
	.size	trail, 262144

	.type	.L.str.2,@object                # @.str.2
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.2:
	.asciz	"too many variables"
	.size	.L.str.2, 19

	.type	goal_stack,@object              # @goal_stack
	.bss
	.globl	goal_stack
	.p2align	2, 0x0
goal_stack:
	.zero	65536
	.size	goal_stack, 65536

	.type	.L.str.3,@object                # @.str.3
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.3:
	.asciz	"goal stack overflow"
	.size	.L.str.3, 20

	.type	choices,@object                 # @choices
	.bss
	.globl	choices
	.p2align	2, 0x0
choices:
	.zero	57344
	.size	choices, 57344

	.type	.L.str.4,@object                # @.str.4
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.4:
	.asciz	"arity overflow"
	.size	.L.str.4, 15

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
