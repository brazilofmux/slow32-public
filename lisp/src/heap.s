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
	.globl	push_root_checked               # -- Begin function push_root_checked
	.p2align	2
	.type	push_root_checked,@function
push_root_checked:                      # @push_root_checked
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	lui r12, %hi(root_sp)
	addi r12, r12, %lo(root_sp)
	ldw r1, r12+0
	addi r3, r0, 512
	blt r1, r3, .LBB2_2
.LBB2_1:
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	addi r4, r0, 512
	jal r31, printf
	addi r3, r0, 1
	jal r31, exit
.LBB2_2:
	ldw r1, r12+0
	addi r3, r1, 1
	stw r12+0, r3
	slli r1, r1, 2
	lui r3, %hi(root_stack)
	addi r3, r3, %lo(root_stack)
	add r1, r1, r3
	stw r1+0, r11
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	push_root_checked, .Lfunc_end2-push_root_checked
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
	addi r12, r0, 1
	blt r1, r12, .LBB3_3
.LBB3_1:
	addi r13, r0, 0
	lui r14, %hi(root_stack)
	addi r14, r14, %lo(root_stack)
.LBB3_2:
	ldw r1, r14+0
	ldw r3, r1+0
	jal r31, mark_val
	addi r13, r13, 1
	ldw r1, r11+0
	addi r14, r14, 4
	blt r13, r1, .LBB3_2
.LBB3_3:
	addi r13, r0, 0
	lui r11, %hi(sym_count)
	addi r11, r11, %lo(sym_count)
	ldw r1, r11+0
	blt r1, r12, .LBB3_6
.LBB3_4:
	lui r14, %hi(sym_table)
	addi r14, r14, %lo(sym_table)
	add r15, r13, r0
.LBB3_5:
	ldw r3, r14+0
	jal r31, mark_val
	addi r15, r15, 1
	ldw r1, r11+0
	addi r14, r14, 4
	blt r15, r1, .LBB3_5
.LBB3_6:
	lui r15, %hi(gc_list)
	addi r15, r15, %lo(gc_list)
	ldw r11, r15+0
	lui r14, %hi(obj_count)
	addi r14, r14, %lo(obj_count)
	bne r11, r13, .LBB3_9
.LBB3_7:
	ldw r1, r14+0
	slli r1, r1, 1
	addi r3, r0, 256
	sgt r3, r1, r3
	sub r3, r13, r3
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
.LBB3_8:
	add r3, r11, r0
	jal r31, free
	ldw r1, r14+0
	addi r1, r1, -1
	stw r14+0, r1
	ldw r11, r15+0
	beq r11, r13, .LBB3_7
.LBB3_9:
	ldw r1, r11+4
	beq r1, r13, .LBB3_11
.LBB3_10:
	stw r11+4, r13
	addi r15, r11, 8
	ldw r11, r15+0
	bne r11, r13, .LBB3_9
	jal r0, .LBB3_7
.LBB3_11:
	ldw r1, r11+8
	stw r15+0, r1
	ldw r1, r11+0
	addi r1, r1, -1
	bgtu r1, r12, .LBB3_8
.LBB3_12:
	ldw r3, r11+12
	jal r31, free
	jal r0, .LBB3_8
.Lfunc_end3:
	.size	gc_collect, .Lfunc_end3-gc_collect
                                        # -- End function
	.p2align	2                               # -- Begin function mark_val
	.type	mark_val,@function
mark_val:                               # @mark_val
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
	stw fp+-40, r20
	stw fp+-44, r21
	stw fp+-48, lr
	addi r14, r0, 0
	lui r15, %hi(mark_sp)
	addi r15, r15, %lo(mark_sp)
	lui r16, %hi(mark_stack)
	addi r16, r16, %lo(mark_stack)
	beq r3, r14, .LBB4_5
.LBB4_1:
	andi r1, r3, 1
	bne r1, r14, .LBB4_5
.LBB4_2:
	ldw r1, r15+0
	addi r4, r0, 1024
	blt r1, r4, .LBB4_4
.LBB4_3:
	lui r1, %hi(.L.str.4)
	addi r1, r1, %lo(.L.str.4)
	addi r4, r0, 1024
	add r11, r3, r0
	add r3, r1, r0
	jal r31, printf
	addi r3, r0, 1
	jal r31, exit
	add r3, r11, r0
.LBB4_4:
	ldw r1, r15+0
	addi r4, r1, 1
	stw r15+0, r4
	slli r1, r1, 2
	add r1, r1, r16
	stw r1+0, r3
.LBB4_5:
	ldw r1, r15+0
	addi r11, r0, 1
	blt r1, r11, .LBB4_36
.LBB4_6:
	addi r17, r0, 3
	addi r18, r0, 1025
	lui r12, %hi(.L.str.4)
	addi r12, r12, %lo(.L.str.4)
	addi r13, r0, 1024
	addi r19, r0, 1023
	jal r0, .LBB4_10
.LBB4_7:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, printf
	add r3, r11, r0
	jal r31, exit
.LBB4_8:
	ldw r1, r15+0
	addi r3, r1, 1
	stw r15+0, r3
	slli r1, r1, 2
	add r1, r1, r16
	stw r1+0, r20
.LBB4_9:
	ldw r1, r15+0
	ble r1, r14, .LBB4_36
.LBB4_10:
	addi r3, r1, -1
	stw r15+0, r3
	slli r3, r3, 2
	add r3, r3, r16
	ldw r20, r3+0
	beq r20, r14, .LBB4_9
.LBB4_11:
	andi r3, r20, 1
	bne r3, r14, .LBB4_9
.LBB4_12:
	ldw r3, r20+4
	bne r3, r14, .LBB4_9
.LBB4_13:
	stw r20+4, r11
	ldw r3, r20+0
	beq r3, r17, .LBB4_23
.LBB4_14:
	bne r3, r14, .LBB4_9
.LBB4_15:
	ldw r21, r20+12
	beq r21, r14, .LBB4_20
.LBB4_16:
	andi r3, r21, 1
	bne r3, r14, .LBB4_20
.LBB4_17:
	bltu r1, r18, .LBB4_19
.LBB4_18:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, printf
	add r3, r11, r0
	jal r31, exit
.LBB4_19:
	ldw r1, r15+0
	addi r3, r1, 1
	stw r15+0, r3
	slli r1, r1, 2
	add r1, r1, r16
	stw r1+0, r21
.LBB4_20:
	ldw r20, r20+16
	beq r20, r14, .LBB4_9
.LBB4_21:
	andi r1, r20, 1
	bne r1, r14, .LBB4_9
.LBB4_22:
	ldw r1, r15+0
	bgt r1, r19, .LBB4_7
	jal r0, .LBB4_8
.LBB4_23:
	ldw r21, r20+12
	beq r21, r14, .LBB4_28
.LBB4_24:
	andi r3, r21, 1
	bne r3, r14, .LBB4_28
.LBB4_25:
	bltu r1, r18, .LBB4_27
.LBB4_26:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, printf
	add r3, r11, r0
	jal r31, exit
.LBB4_27:
	ldw r1, r15+0
	addi r3, r1, 1
	stw r15+0, r3
	slli r1, r1, 2
	add r1, r1, r16
	stw r1+0, r21
.LBB4_28:
	ldw r21, r20+16
	beq r21, r14, .LBB4_33
.LBB4_29:
	andi r1, r21, 1
	bne r1, r14, .LBB4_33
.LBB4_30:
	ldw r1, r15+0
	blt r1, r13, .LBB4_32
.LBB4_31:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, printf
	add r3, r11, r0
	jal r31, exit
.LBB4_32:
	ldw r1, r15+0
	addi r3, r1, 1
	stw r15+0, r3
	slli r1, r1, 2
	add r1, r1, r16
	stw r1+0, r21
.LBB4_33:
	ldw r20, r20+20
	beq r20, r14, .LBB4_9
.LBB4_34:
	andi r1, r20, 1
	bne r1, r14, .LBB4_9
.LBB4_35:
	ldw r1, r15+0
	bge r1, r13, .LBB4_7
	jal r0, .LBB4_8
.LBB4_36:
	ldw lr, fp+-48
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
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end4:
	.size	mark_val, .Lfunc_end4-mark_val
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
	lui r3, %hi(.L.str.1)
	addi r3, r3, %lo(.L.str.1)
	jal r31, symbol_intern
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	stw r3+0, r1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	heap_init, .Lfunc_end5-heap_init
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
	blt r1, r15, .LBB6_8
.LBB6_1:
	addi r13, r0, 0
	lui r16, %hi(sym_table)
	addi r16, r16, %lo(sym_table)
                                        # implicit-def: $r12
	add r17, r13, r0
.LBB6_2:
	ldw r1, r16+0
	ldw r3, r1+12
	add r4, r11, r0
	jal r31, strcmp
	bne r1, r13, .LBB6_4
.LBB6_3:
	ldw r12, r16+0
.LBB6_4:
	beq r1, r13, .LBB6_17
.LBB6_5:
	addi r17, r17, 1
	ldw r1, r14+0
	addi r16, r16, 4
	blt r17, r1, .LBB6_2
.LBB6_6:
	addi r3, r0, 512
	blt r1, r3, .LBB6_8
.LBB6_7:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	stw r1+0, r15
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	addi r5, r0, 255
	jal r31, strncpy
	addi r12, r0, 0
	jal r0, .LBB6_16
.LBB6_8:
	addi r12, r0, 0
	lui r16, %hi(obj_count)
	addi r16, r16, %lo(obj_count)
	ldw r1, r16+0
	lui r3, %hi(gc_threshold)
	addi r3, r3, %lo(gc_threshold)
	ldw r3, r3+0
	blt r1, r3, .LBB6_10
.LBB6_9:
	jal r31, gc_collect
.LBB6_10:
	addi r3, r0, 24
	jal r31, malloc
	beq r1, r12, .LBB6_12
.LBB6_11:
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
	bne r13, r12, .LBB6_13
	jal r0, .LBB6_17
.LBB6_12:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	stw r1+0, r15
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	addi r5, r0, 255
	jal r31, strncpy
	addi r13, r0, 0
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r13
	beq r13, r12, .LBB6_17
.LBB6_13:
	add r3, r11, r0
	jal r31, strlen
	addi r3, r1, 1
	jal r31, malloc
	stw r13+12, r1
	addi r12, r0, 0
	beq r1, r12, .LBB6_15
.LBB6_14:
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
	jal r0, .LBB6_17
.LBB6_15:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	stw r1+0, r15
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	addi r5, r0, 255
	jal r31, strncpy
.LBB6_16:
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r12
.LBB6_17:
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
.Lfunc_end6:
	.size	symbol_intern, .Lfunc_end6-symbol_intern
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
	addi r3, r0, 512
	blt r1, r3, .LBB7_2
.LBB7_1:
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	addi r4, r0, 512
	jal r31, printf
	addi r3, r0, 1
	jal r31, exit
.LBB7_2:
	ldw r1, r15+0
	addi r3, r1, 1
	stw r15+0, r3
	slli r3, r1, 2
	lui r11, %hi(root_stack)
	addi r11, r11, %lo(root_stack)
	add r3, r3, r11
	stw r3+0, r13
	addi r3, r0, 511
	blt r1, r3, .LBB7_4
.LBB7_3:
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	addi r4, r0, 512
	jal r31, printf
	addi r3, r0, 1
	jal r31, exit
.LBB7_4:
	ldw r1, r15+0
	addi r3, r1, 1
	stw r15+0, r3
	slli r1, r1, 2
	add r1, r1, r11
	stw r1+0, r14
	lui r16, %hi(obj_count)
	addi r16, r16, %lo(obj_count)
	ldw r1, r16+0
	lui r3, %hi(gc_threshold)
	addi r3, r3, %lo(gc_threshold)
	ldw r3, r3+0
	blt r1, r3, .LBB7_6
.LBB7_5:
	jal r31, gc_collect
.LBB7_6:
	addi r3, r0, 24
	jal r31, malloc
	addi r11, r0, 0
	beq r1, r11, .LBB7_8
.LBB7_7:
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
	jal r0, .LBB7_9
.LBB7_8:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	addi r5, r0, 255
	jal r31, strncpy
	addi r12, r0, 0
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r12
.LBB7_9:
	ldw r1, r15+0
	addi r1, r1, -2
	stw r15+0, r1
	beq r12, r11, .LBB7_11
.LBB7_10:
	ldw r1, r13+0
	stw r12+12, r1
	ldw r1, r14+0
	stw r12+16, r1
	add r11, r12, r0
.LBB7_11:
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
.Lfunc_end7:
	.size	cons_alloc, .Lfunc_end7-cons_alloc
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
	blt r1, r3, .LBB8_2
.LBB8_1:
	jal r31, gc_collect
.LBB8_2:
	addi r3, r0, 24
	jal r31, malloc
	addi r13, r0, 0
	beq r1, r13, .LBB8_4
.LBB8_3:
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
	bne r14, r13, .LBB8_5
	jal r0, .LBB8_8
.LBB8_4:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	addi r5, r0, 255
	jal r31, strncpy
	addi r14, r0, 0
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r14
	beq r14, r13, .LBB8_8
.LBB8_5:
	addi r3, r11, 1
	jal r31, malloc
	stw r14+12, r1
	addi r13, r0, 0
	beq r1, r13, .LBB8_7
.LBB8_6:
	add r3, r1, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r14+12
	add r1, r1, r11
	stb r1+0, r13
	stw r14+16, r11
	add r13, r14, r0
	jal r0, .LBB8_8
.LBB8_7:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	addi r5, r0, 255
	jal r31, strncpy
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r13
.LBB8_8:
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
.Lfunc_end8:
	.size	string_alloc, .Lfunc_end8-string_alloc
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
	addi r3, r0, 512
	blt r1, r3, .LBB9_2
.LBB9_1:
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	addi r4, r0, 512
	jal r31, printf
	addi r3, r0, 1
	jal r31, exit
.LBB9_2:
	ldw r1, r16+0
	addi r3, r1, 1
	stw r16+0, r3
	slli r3, r1, 2
	lui r11, %hi(root_stack)
	addi r11, r11, %lo(root_stack)
	add r3, r3, r11
	stw r3+0, r13
	addi r12, r0, 511
	blt r1, r12, .LBB9_4
.LBB9_3:
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	addi r4, r0, 512
	jal r31, printf
	addi r3, r0, 1
	jal r31, exit
.LBB9_4:
	ldw r1, r16+0
	addi r3, r1, 1
	stw r16+0, r3
	slli r3, r1, 2
	add r3, r3, r11
	stw r3+0, r14
	blt r1, r12, .LBB9_6
.LBB9_5:
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	addi r4, r0, 512
	jal r31, printf
	addi r3, r0, 1
	jal r31, exit
.LBB9_6:
	ldw r1, r16+0
	addi r3, r1, 1
	stw r16+0, r3
	slli r1, r1, 2
	add r1, r1, r11
	stw r1+0, r15
	lui r17, %hi(obj_count)
	addi r17, r17, %lo(obj_count)
	ldw r1, r17+0
	lui r3, %hi(gc_threshold)
	addi r3, r3, %lo(gc_threshold)
	ldw r3, r3+0
	blt r1, r3, .LBB9_8
.LBB9_7:
	jal r31, gc_collect
.LBB9_8:
	addi r3, r0, 24
	jal r31, malloc
	addi r11, r0, 0
	beq r1, r11, .LBB9_10
.LBB9_9:
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
	jal r0, .LBB9_11
.LBB9_10:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	addi r5, r0, 255
	jal r31, strncpy
	addi r12, r0, 0
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r12
.LBB9_11:
	ldw r1, r16+0
	addi r1, r1, -3
	stw r16+0, r1
	beq r12, r11, .LBB9_13
.LBB9_12:
	ldw r1, r13+0
	stw r12+12, r1
	ldw r1, r14+0
	stw r12+16, r1
	ldw r1, r15+0
	stw r12+20, r1
	add r11, r12, r0
.LBB9_13:
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
.Lfunc_end9:
	.size	lambda_alloc, .Lfunc_end9-lambda_alloc
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
	blt r1, r3, .LBB10_2
.LBB10_1:
	jal r31, gc_collect
.LBB10_2:
	addi r3, r0, 24
	jal r31, malloc
	addi r13, r0, 0
	beq r1, r13, .LBB10_6
.LBB10_3:
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
	beq r14, r13, .LBB10_5
.LBB10_4:
	stw r14+12, r12
	stw r14+16, r11
	add r13, r14, r0
.LBB10_5:
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
.LBB10_6:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	addi r5, r0, 255
	jal r31, strncpy
	addi r14, r0, 0
	lui r1, %hi(g_errmsg+255)
	addi r1, r1, %lo(g_errmsg+255)
	stb r1+0, r14
	bne r14, r13, .LBB10_4
	jal r0, .LBB10_5
.Lfunc_end10:
	.size	builtin_alloc, .Lfunc_end10-builtin_alloc
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

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Fatal: GC root stack overflow (%d)\n"
	.size	.L.str, 36

	.type	root_stack,@object              # @root_stack
	.bss
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
	.type	.L.str.1,@object                # @.str.1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.1:
	.asciz	"#t"
	.size	.L.str.1, 3

	.type	sym_true,@object                # @sym_true
	.bss
	.globl	sym_true
	.p2align	2, 0x0
sym_true:
	.word	0                               # 0x0
	.size	sym_true, 4

	.type	.L.str.2,@object                # @.str.2
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.2:
	.asciz	"too many symbols"
	.size	.L.str.2, 17

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"out of memory"
	.size	.L.str.3, 14

	.type	mark_stack,@object              # @mark_stack
	.local	mark_stack
	.comm	mark_stack,4096,4
	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"Fatal: GC mark stack overflow (%d)\n"
	.size	.L.str.4, 36

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
