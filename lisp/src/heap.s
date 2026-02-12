	.file	"heap.c"
	.text
	.globl	lisp_error                      # -- Begin function lisp_error
	.p2align	2
	.type	lisp_error,@function
lisp_error:                             # @lisp_error
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	add r4, r3, r0
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	addi r5, r0, 255
	jal r31, strncpy
	addi r1, r0, 0
	lui r3, %hi(g_errmsg+255)
	addi r3, r3, %lo(g_errmsg+255)
	stb r3+0, r1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	lisp_error, .Lfunc_end0-lisp_error
                                        # -- End function
	.globl	lisp_error2                     # -- Begin function lisp_error2
	.p2align	2
	.type	lisp_error2,@function
lisp_error2:                            # @lisp_error2
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r5, r0, 1
	stw r1+0, r5
	ldbu r7, r3+0
	addi r5, r0, 0
	lui r1, %hi(g_errmsg)
	addi r1, r1, %lo(g_errmsg)
	add r6, r5, r0
	beq r7, r5, .LBB1_4
.LBB1_1:
	addi r3, r3, 1
	addi r8, r0, 0
	addi r9, r0, 199
	add r6, r8, r0
.LBB1_2:
	add r10, r6, r0
	add r11, r3, r6
	addi r6, r6, 1
	add r12, r10, r1
	stb r12+0, r7
	ldbu r7, r11+0
	beq r7, r8, .LBB1_4
.LBB1_3:
	bltu r10, r9, .LBB1_2
.LBB1_4:
	add r3, r6, r1
	addi r7, r0, 58
	stb r3+0, r7
	addi r3, r6, 2
	lui r7, %hi(g_errmsg+1)
	addi r7, r7, %lo(g_errmsg+1)
	add r7, r6, r7
	addi r8, r0, 32
	stb r7+0, r8
	ldbu r7, r4+0
	beq r7, r5, .LBB1_9
.LBB1_5:
	addi r5, r0, 252
	bgtu r6, r5, .LBB1_9
.LBB1_6:
	addi r4, r4, 1
	addi r5, r0, 0
	addi r6, r0, 254
.LBB1_7:
	add r8, r3, r0
	addi r3, r3, 1
	add r9, r8, r1
	stb r9+0, r7
	ldbu r7, r4+0
	beq r7, r5, .LBB1_9
.LBB1_8:
	addi r4, r4, 1
	bltu r8, r6, .LBB1_7
.LBB1_9:
	add r1, r3, r1
	addi r3, r0, 0
	stb r1+0, r3
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	lisp_error2, .Lfunc_end1-lisp_error2
                                        # -- End function
	.globl	gc_collect                      # -- Begin function gc_collect
	.p2align	2
	.type	gc_collect,@function
gc_collect:                             # @gc_collect
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
	lui r11, %hi(root_sp)
	addi r11, r11, %lo(root_sp)
	ldw r1, r11+0
	addi r13, r0, 1
	addi r12, r0, 0
	blt r1, r13, .LBB2_3
.LBB2_1:
	lui r14, %hi(root_stack)
	addi r14, r14, %lo(root_stack)
	add r15, r12, r0
.LBB2_2:
	ldw r1, r14+0
	ldw r3, r1+0
	jal r31, mark_val
	addi r15, r15, 1
	ldw r1, r11+0
	addi r14, r14, 4
	blt r15, r1, .LBB2_2
.LBB2_3:
	lui r1, %hi(sym_count)
	addi r1, r1, %lo(sym_count)
	ldw r11, r1+0
	blt r11, r13, .LBB2_6
.LBB2_4:
	lui r14, %hi(sym_table)
	addi r14, r14, %lo(sym_table)
.LBB2_5:
	ldw r3, r14+0
	jal r31, mark_val
	addi r11, r11, -1
	addi r14, r14, 4
	bne r11, r12, .LBB2_5
.LBB2_6:
	lui r15, %hi(gc_list)
	addi r15, r15, %lo(gc_list)
	ldw r11, r15+0
	lui r14, %hi(obj_count)
	addi r14, r14, %lo(obj_count)
	bne r11, r12, .LBB2_9
.LBB2_7:
	ldw r1, r14+0
	slli r1, r1, 1
	addi r3, r0, 256
	sgt r3, r1, r3
	sub r3, r12, r3
	xori r1, r1, 256
	and r1, r1, r3
	xori r1, r1, 256
	lui r3, %hi(gc_threshold)
	addi r3, r3, %lo(gc_threshold)
	stw r3+0, r1
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
.LBB2_8:
	add r3, r11, r0
	jal r31, free
	ldw r1, r14+0
	addi r1, r1, -1
	stw r14+0, r1
	ldw r11, r15+0
	beq r11, r12, .LBB2_7
.LBB2_9:
	ldw r1, r11+4
	beq r1, r12, .LBB2_11
.LBB2_10:
	stw r11+4, r12
	addi r15, r11, 8
	ldw r11, r15+0
	bne r11, r12, .LBB2_9
	jal r0, .LBB2_7
.LBB2_11:
	ldw r1, r11+8
	stw r15+0, r1
	ldw r1, r11+0
	addi r1, r1, -1
	bgtu r1, r13, .LBB2_8
.LBB2_12:
	ldw r3, r11+12
	jal r31, free
	jal r0, .LBB2_8
.Lfunc_end2:
	.size	gc_collect, .Lfunc_end2-gc_collect
                                        # -- End function
	.p2align	2                               # -- Begin function mark_val
	.type	mark_val,@function
mark_val:                               # @mark_val
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	lui r1, %hi(mark_sp)
	addi r1, r1, %lo(mark_sp)
	addi r4, r0, 0
	lui r5, %hi(mark_stack)
	addi r5, r5, %lo(mark_stack)
	beq r3, r4, .LBB3_4
.LBB3_1:
	andi r6, r3, 1
	bne r6, r4, .LBB3_4
.LBB3_2:
	ldw r6, r1+0
	addi r7, r0, 1023
	bgt r6, r7, .LBB3_4
.LBB3_3:
	addi r7, r6, 1
	stw r1+0, r7
	slli r6, r6, 2
	add r6, r6, r5
	stw r6+0, r3
.LBB3_4:
	ldw r10, r1+0
	addi r3, r0, 1
	blt r10, r3, .LBB3_32
.LBB3_5:
	addi r6, r0, 3
	addi r7, r0, 1024
	addi r8, r0, 1023
	jal r0, .LBB3_8
.LBB3_6:
	add r10, r9, r0
.LBB3_7:
	ble r10, r4, .LBB3_32
.LBB3_8:
	addi r9, r10, -1
	stw r1+0, r9
	slli r11, r9, 2
	add r12, r11, r5
	ldw r11, r12+0
	beq r11, r4, .LBB3_6
.LBB3_9:
	andi r13, r11, 1
	bne r13, r4, .LBB3_6
.LBB3_10:
	ldw r13, r11+4
	bne r13, r4, .LBB3_6
.LBB3_11:
	stw r11+4, r3
	ldw r13, r11+0
	beq r13, r6, .LBB3_20
.LBB3_12:
	bne r13, r4, .LBB3_6
.LBB3_13:
	bgtu r10, r7, .LBB3_17
.LBB3_14:
	ldw r13, r11+12
	beq r13, r4, .LBB3_17
.LBB3_15:
	andi r14, r13, 1
	bne r14, r4, .LBB3_17
.LBB3_16:
	stw r1+0, r10
	stw r12+0, r13
	add r9, r10, r0
.LBB3_17:
	ldw r11, r11+16
	beq r11, r4, .LBB3_6
.LBB3_18:
	andi r10, r11, 1
	bne r10, r4, .LBB3_6
.LBB3_19:
	add r10, r9, r0
	blt r9, r7, .LBB3_31
	jal r0, .LBB3_7
.LBB3_20:
	bgtu r10, r7, .LBB3_24
.LBB3_21:
	ldw r13, r11+12
	beq r13, r4, .LBB3_24
.LBB3_22:
	andi r14, r13, 1
	bne r14, r4, .LBB3_24
.LBB3_23:
	stw r1+0, r10
	stw r12+0, r13
	add r9, r10, r0
.LBB3_24:
	ldw r10, r11+16
	beq r10, r4, .LBB3_28
.LBB3_25:
	andi r12, r10, 1
	bne r12, r4, .LBB3_28
.LBB3_26:
	bgt r9, r8, .LBB3_28
.LBB3_27:
	addi r12, r9, 1
	stw r1+0, r12
	slli r9, r9, 2
	add r9, r9, r5
	stw r9+0, r10
	add r9, r12, r0
.LBB3_28:
	ldw r11, r11+20
	beq r11, r4, .LBB3_6
.LBB3_29:
	andi r10, r11, 1
	bne r10, r4, .LBB3_6
.LBB3_30:
	add r10, r9, r0
	bgt r9, r8, .LBB3_7
.LBB3_31:
	addi r10, r9, 1
	stw r1+0, r10
	slli r9, r9, 2
	add r9, r9, r5
	stw r9+0, r11
	jal r0, .LBB3_7
.LBB3_32:
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	mark_val, .Lfunc_end3-mark_val
                                        # -- End function
	.globl	heap_init                       # -- Begin function heap_init
	.p2align	2
	.type	heap_init,@function
heap_init:                              # @heap_init
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r1, %hi(gc_list)
	addi r1, r1, %lo(gc_list)
	addi r3, r0, 0
	stw r1+0, r3
	lui r1, %hi(obj_count)
	addi r1, r1, %lo(obj_count)
	stw r1+0, r3
	lui r1, %hi(gc_threshold)
	addi r1, r1, %lo(gc_threshold)
	addi r4, r0, 256
	stw r1+0, r4
	lui r1, %hi(root_sp)
	addi r1, r1, %lo(root_sp)
	stw r1+0, r3
	lui r1, %hi(sym_count)
	addi r1, r1, %lo(sym_count)
	stw r1+0, r3
	lui r1, %hi(mark_sp)
	addi r1, r1, %lo(mark_sp)
	stw r1+0, r3
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	jal r31, symbol_intern
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	stw r3+0, r1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	heap_init, .Lfunc_end4-heap_init
                                        # -- End function
	.globl	symbol_intern                   # -- Begin function symbol_intern
	.p2align	2
	.type	symbol_intern,@function
symbol_intern:                          # @symbol_intern
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
	add r11, r3, r0
	lui r14, %hi(sym_count)
	addi r14, r14, %lo(sym_count)
	ldw r1, r14+0
	addi r15, r0, 1
	blt r1, r15, .LBB5_8
.LBB5_1:
	addi r13, r0, 0
	lui r16, %hi(sym_table)
	addi r16, r16, %lo(sym_table)
                                        # implicit-def: $r12
	add r17, r13, r0
.LBB5_2:
	ldw r1, r16+0
	ldw r3, r1+12
	add r4, r11, r0
	jal r31, strcmp
	bne r1, r13, .LBB5_4
.LBB5_3:
	ldw r12, r16+0
.LBB5_4:
	beq r1, r13, .LBB5_17
.LBB5_5:
	addi r17, r17, 1
	ldw r1, r14+0
	addi r16, r16, 4
	blt r17, r1, .LBB5_2
.LBB5_6:
	addi r3, r0, 512
	blt r1, r3, .LBB5_8
.LBB5_7:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	stw r1+0, r15
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	addi r5, r0, 255
	jal r31, strncpy
	addi r12, r0, 0
	jal r0, .LBB5_16
.LBB5_8:
	addi r12, r0, 0
	lui r16, %hi(obj_count)
	addi r16, r16, %lo(obj_count)
	ldw r1, r16+0
	lui r3, %hi(gc_threshold)
	addi r3, r3, %lo(gc_threshold)
	ldw r3, r3+0
	blt r1, r3, .LBB5_10
.LBB5_9:
	jal r31, gc_collect
.LBB5_10:
	addi r3, r0, 24
	jal r31, malloc
	beq r1, r12, .LBB5_12
.LBB5_11:
	add r13, r1, r0
	addi r5, r0, 24
	add r3, r1, r0
	add r4, r12, r0
	jal r31, memset
	stw r13+0, r15
	stw r13+4, r12
	lui r1, %hi(gc_list)
	addi r1, r1, %lo(gc_list)
	ldw r3, r1+0
	stw r13+8, r3
	stw r1+0, r13
	ldw r1, r16+0
	addi r1, r1, 1
	stw r16+0, r1
	bne r13, r12, .LBB5_13
	jal r0, .LBB5_17
.LBB5_12:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	stw r1+0, r15
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	addi r5, r0, 255
	jal r31, strncpy
	addi r13, r0, 0
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r13
	beq r13, r12, .LBB5_17
.LBB5_13:
	add r3, r11, r0
	jal r31, strlen
	addi r3, r1, 1
	jal r31, malloc
	stw r13+12, r1
	addi r12, r0, 0
	beq r1, r12, .LBB5_15
.LBB5_14:
	add r3, r1, r0
	add r4, r11, r0
	jal r31, strcpy
	ldw r1, r14+0
	addi r3, r1, 1
	stw r14+0, r3
	slli r1, r1, 2
	lui r3, %hi(sym_table)
	addi r3, r3, %lo(sym_table)
	add r1, r1, r3
	stw r1+0, r13
	add r12, r13, r0
	jal r0, .LBB5_17
.LBB5_15:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	stw r1+0, r15
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	addi r5, r0, 255
	jal r31, strncpy
.LBB5_16:
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r12
.LBB5_17:
	add r1, r12, r0
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
.Lfunc_end5:
	.size	symbol_intern, .Lfunc_end5-symbol_intern
                                        # -- End function
	.globl	cons_alloc                      # -- Begin function cons_alloc
	.p2align	2
	.type	cons_alloc,@function
cons_alloc:                             # @cons_alloc
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
	addi r13, fp, -32
	stw r13+0, r3
	addi r14, fp, -36
	stw r14+0, r4
	lui r15, %hi(root_sp)
	addi r15, r15, %lo(root_sp)
	ldw r1, r15+0
	addi r3, r1, 1
	stw r15+0, r3
	slli r4, r1, 2
	lui r5, %hi(root_stack)
	addi r5, r5, %lo(root_stack)
	add r4, r4, r5
	stw r4+0, r13
	addi r1, r1, 2
	stw r15+0, r1
	slli r1, r3, 2
	add r1, r1, r5
	stw r1+0, r14
	lui r16, %hi(obj_count)
	addi r16, r16, %lo(obj_count)
	ldw r1, r16+0
	lui r3, %hi(gc_threshold)
	addi r3, r3, %lo(gc_threshold)
	ldw r3, r3+0
	blt r1, r3, .LBB6_2
.LBB6_1:
	jal r31, gc_collect
.LBB6_2:
	addi r3, r0, 24
	jal r31, malloc
	addi r11, r0, 0
	beq r1, r11, .LBB6_4
.LBB6_3:
	add r12, r1, r0
	addi r5, r0, 24
	add r3, r1, r0
	add r4, r11, r0
	jal r31, memset
	stw r12+0, r11
	stw r12+4, r11
	lui r1, %hi(gc_list)
	addi r1, r1, %lo(gc_list)
	ldw r3, r1+0
	stw r12+8, r3
	stw r1+0, r12
	ldw r1, r16+0
	addi r1, r1, 1
	stw r16+0, r1
	jal r0, .LBB6_5
.LBB6_4:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	addi r5, r0, 255
	jal r31, strncpy
	addi r12, r0, 0
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r12
.LBB6_5:
	ldw r1, r15+0
	addi r1, r1, -2
	stw r15+0, r1
	beq r12, r11, .LBB6_7
.LBB6_6:
	ldw r1, r13+0
	stw r12+12, r1
	ldw r1, r14+0
	stw r12+16, r1
	add r11, r12, r0
.LBB6_7:
	add r1, r11, r0
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
.Lfunc_end6:
	.size	cons_alloc, .Lfunc_end6-cons_alloc
                                        # -- End function
	.globl	string_alloc                    # -- Begin function string_alloc
	.p2align	2
	.type	string_alloc,@function
string_alloc:                           # @string_alloc
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
	add r11, r4, r0
	add r12, r3, r0
	lui r15, %hi(obj_count)
	addi r15, r15, %lo(obj_count)
	ldw r1, r15+0
	lui r3, %hi(gc_threshold)
	addi r3, r3, %lo(gc_threshold)
	ldw r3, r3+0
	blt r1, r3, .LBB7_2
.LBB7_1:
	jal r31, gc_collect
.LBB7_2:
	addi r3, r0, 24
	jal r31, malloc
	addi r13, r0, 0
	beq r1, r13, .LBB7_4
.LBB7_3:
	add r14, r1, r0
	addi r5, r0, 24
	add r3, r1, r0
	add r4, r13, r0
	jal r31, memset
	addi r1, r0, 2
	stw r14+0, r1
	stw r14+4, r13
	lui r1, %hi(gc_list)
	addi r1, r1, %lo(gc_list)
	ldw r3, r1+0
	stw r14+8, r3
	stw r1+0, r14
	ldw r1, r15+0
	addi r1, r1, 1
	stw r15+0, r1
	bne r14, r13, .LBB7_5
	jal r0, .LBB7_8
.LBB7_4:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	addi r5, r0, 255
	jal r31, strncpy
	addi r14, r0, 0
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r14
	beq r14, r13, .LBB7_8
.LBB7_5:
	addi r3, r11, 1
	jal r31, malloc
	stw r14+12, r1
	addi r13, r0, 0
	beq r1, r13, .LBB7_7
.LBB7_6:
	add r3, r1, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r14+12
	add r1, r1, r11
	stb r1+0, r13
	stw r14+16, r11
	add r13, r14, r0
	jal r0, .LBB7_8
.LBB7_7:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	addi r5, r0, 255
	jal r31, strncpy
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r13
.LBB7_8:
	add r1, r13, r0
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
.Lfunc_end7:
	.size	string_alloc, .Lfunc_end7-string_alloc
                                        # -- End function
	.globl	lambda_alloc                    # -- Begin function lambda_alloc
	.p2align	2
	.type	lambda_alloc,@function
lambda_alloc:                           # @lambda_alloc
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
	addi r13, fp, -36
	stw r13+0, r3
	addi r14, fp, -40
	stw r14+0, r4
	addi r15, fp, -44
	stw r15+0, r5
	lui r16, %hi(root_sp)
	addi r16, r16, %lo(root_sp)
	ldw r1, r16+0
	addi r3, r1, 1
	stw r16+0, r3
	slli r4, r1, 2
	lui r5, %hi(root_stack)
	addi r5, r5, %lo(root_stack)
	add r4, r4, r5
	stw r4+0, r13
	addi r4, r1, 2
	stw r16+0, r4
	slli r3, r3, 2
	add r3, r3, r5
	stw r3+0, r14
	addi r1, r1, 3
	stw r16+0, r1
	slli r1, r4, 2
	add r1, r1, r5
	stw r1+0, r15
	lui r17, %hi(obj_count)
	addi r17, r17, %lo(obj_count)
	ldw r1, r17+0
	lui r3, %hi(gc_threshold)
	addi r3, r3, %lo(gc_threshold)
	ldw r3, r3+0
	blt r1, r3, .LBB8_2
.LBB8_1:
	jal r31, gc_collect
.LBB8_2:
	addi r3, r0, 24
	jal r31, malloc
	addi r11, r0, 0
	beq r1, r11, .LBB8_4
.LBB8_3:
	add r12, r1, r0
	addi r5, r0, 24
	add r3, r1, r0
	add r4, r11, r0
	jal r31, memset
	addi r1, r0, 3
	stw r12+0, r1
	stw r12+4, r11
	lui r1, %hi(gc_list)
	addi r1, r1, %lo(gc_list)
	ldw r3, r1+0
	stw r12+8, r3
	stw r1+0, r12
	ldw r1, r17+0
	addi r1, r1, 1
	stw r17+0, r1
	jal r0, .LBB8_5
.LBB8_4:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	addi r5, r0, 255
	jal r31, strncpy
	addi r12, r0, 0
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r12
.LBB8_5:
	ldw r1, r16+0
	addi r1, r1, -3
	stw r16+0, r1
	beq r12, r11, .LBB8_7
.LBB8_6:
	ldw r1, r13+0
	stw r12+12, r1
	ldw r1, r14+0
	stw r12+16, r1
	ldw r1, r15+0
	stw r12+20, r1
	add r11, r12, r0
.LBB8_7:
	add r1, r11, r0
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
.Lfunc_end8:
	.size	lambda_alloc, .Lfunc_end8-lambda_alloc
                                        # -- End function
	.globl	builtin_alloc                   # -- Begin function builtin_alloc
	.p2align	2
	.type	builtin_alloc,@function
builtin_alloc:                          # @builtin_alloc
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
	add r11, r4, r0
	add r12, r3, r0
	lui r15, %hi(obj_count)
	addi r15, r15, %lo(obj_count)
	ldw r1, r15+0
	lui r3, %hi(gc_threshold)
	addi r3, r3, %lo(gc_threshold)
	ldw r3, r3+0
	blt r1, r3, .LBB9_2
.LBB9_1:
	jal r31, gc_collect
.LBB9_2:
	addi r3, r0, 24
	jal r31, malloc
	addi r13, r0, 0
	beq r1, r13, .LBB9_6
.LBB9_3:
	add r14, r1, r0
	addi r5, r0, 24
	add r3, r1, r0
	add r4, r13, r0
	jal r31, memset
	addi r1, r0, 4
	stw r14+0, r1
	stw r14+4, r13
	lui r1, %hi(gc_list)
	addi r1, r1, %lo(gc_list)
	ldw r3, r1+0
	stw r14+8, r3
	stw r1+0, r14
	ldw r1, r15+0
	addi r1, r1, 1
	stw r15+0, r1
	beq r14, r13, .LBB9_5
.LBB9_4:
	stw r14+12, r12
	stw r14+16, r11
	add r13, r14, r0
.LBB9_5:
	add r1, r13, r0
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
.LBB9_6:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	addi r5, r0, 255
	jal r31, strncpy
	addi r14, r0, 0
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r14
	bne r14, r13, .LBB9_4
	jal r0, .LBB9_5
.Lfunc_end9:
	.size	builtin_alloc, .Lfunc_end9-builtin_alloc
                                        # -- End function
	.type	g_error,@object                 # @g_error
	.bss
	.globl	g_error
	.p2align	2, 0x0
g_error:
	.word	0                               # 0x0
	.size	g_error, 4

	.type	g_errmsg,@object                # @g_errmsg
	.globl	g_errmsg
g_errmsg:
	.zero	256
	.size	g_errmsg, 256

	.type	root_sp,@object                 # @root_sp
	.globl	root_sp
	.p2align	2, 0x0
root_sp:
	.word	0                               # 0x0
	.size	root_sp, 4

	.type	root_stack,@object              # @root_stack
	.globl	root_stack
	.p2align	2, 0x0
root_stack:
	.zero	2048
	.size	root_stack, 2048

	.type	sym_count,@object               # @sym_count
	.local	sym_count
	.comm	sym_count,4,4
	.type	sym_table,@object               # @sym_table
	.local	sym_table
	.comm	sym_table,2048,4
	.type	gc_list,@object                 # @gc_list
	.local	gc_list
	.comm	gc_list,4,4
	.type	obj_count,@object               # @obj_count
	.local	obj_count
	.comm	obj_count,4,4
	.type	gc_threshold,@object            # @gc_threshold
	.data
	.p2align	2, 0x0
gc_threshold:
	.word	256                             # 0x100
	.size	gc_threshold, 4

	.type	mark_sp,@object                 # @mark_sp
	.local	mark_sp
	.comm	mark_sp,4,4
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"#t"
	.size	.L.str, 3

	.type	sym_true,@object                # @sym_true
	.bss
	.globl	sym_true
	.p2align	2, 0x0
sym_true:
	.word	0                               # 0x0
	.size	sym_true, 4

	.type	.L.str.1,@object                # @.str.1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.1:
	.asciz	"too many symbols"
	.size	.L.str.1, 17

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"out of memory"
	.size	.L.str.2, 14

	.type	mark_stack,@object              # @mark_stack
	.local	mark_stack
	.comm	mark_stack,4096,4
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
