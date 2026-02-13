	.file	"term.c"
	.text
	.globl	atom_intern                     # -- Begin function atom_intern
	.p2align	2
	.type	atom_intern,@function
atom_intern:                            # @atom_intern
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
	lui r14, %hi(atom_count)
	addi r14, r14, %lo(atom_count)
	ldw r1, r14+0
	addi r13, r0, 1
	lui r15, %hi(atom_names)
	addi r15, r15, %lo(atom_names)
	blt r1, r13, .LBB0_6
.LBB0_1:
	addi r16, r0, 0
	add r17, r15, r0
	add r12, r16, r0
.LBB0_2:
	ldw r3, r17+0
	add r4, r11, r0
	jal r31, strcmp
	beq r1, r16, .LBB0_10
.LBB0_3:
	addi r12, r12, 1
	ldw r1, r14+0
	addi r17, r17, 4
	blt r12, r1, .LBB0_2
.LBB0_4:
	lui r3, 1
	addi r3, r3, -2048
	blt r1, r3, .LBB0_6
.LBB0_5:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	stw r1+0, r13
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	addi r4, r0, 256
	jal r31, snprintf
	addi r12, r0, 0
	jal r0, .LBB0_10
.LBB0_6:
	add r3, r11, r0
	jal r31, strlen
	addi r12, r1, 1
	lui r16, %hi(atom_store_pos)
	addi r16, r16, %lo(atom_store_pos)
	ldw r1, r16+0
	add r3, r1, r12
	lui r4, 8
	addi r4, r4, 1
	blt r3, r4, .LBB0_8
.LBB0_7:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	stw r1+0, r13
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.67)
	addi r5, r5, %lo(.L.str.67)
	addi r4, r0, 256
	jal r31, snprintf
	ldw r13, r15+0
	jal r0, .LBB0_9
.LBB0_8:
	lui r3, %hi(atom_store)
	addi r3, r3, %lo(atom_store)
	add r13, r1, r3
	add r3, r13, r0
	add r4, r11, r0
	add r5, r12, r0
	jal r31, memcpy
	ldw r1, r16+0
	add r1, r1, r12
	stw r16+0, r1
.LBB0_9:
	ldw r12, r14+0
	slli r1, r12, 2
	add r1, r1, r15
	stw r1+0, r13
	addi r1, r12, 1
	stw r14+0, r1
.LBB0_10:
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
.Lfunc_end0:
	.size	atom_intern, .Lfunc_end0-atom_intern
                                        # -- End function
	.globl	atom_name                       # -- Begin function atom_name
	.p2align	2
	.type	atom_name,@function
atom_name:                              # @atom_name
# %bb.0:
	lui r1, %hi(.L.str.1)
	addi r1, r1, %lo(.L.str.1)
	addi r4, r0, 0
	blt r3, r4, .LBB1_3
.LBB1_1:
	lui r4, %hi(atom_count)
	addi r4, r4, %lo(atom_count)
	ldw r4, r4+0
	bge r3, r4, .LBB1_3
.LBB1_2:
	slli r1, r3, 2
	lui r3, %hi(atom_names)
	addi r3, r3, %lo(atom_names)
	add r1, r1, r3
	ldw r1, r1+0
.LBB1_3:
	jalr r0, r31, 0
.Lfunc_end1:
	.size	atom_name, .Lfunc_end1-atom_name
                                        # -- End function
	.globl	init_atoms                      # -- Begin function init_atoms
	.p2align	2
	.type	init_atoms,@function
init_atoms:                             # @init_atoms
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r1, %hi(atom_count)
	addi r1, r1, %lo(atom_count)
	addi r3, r0, 0
	stw r1+0, r3
	lui r1, %hi(atom_store_pos)
	addi r1, r1, %lo(atom_store_pos)
	stw r1+0, r3
	lui r3, %hi(.L.str.2)
	addi r3, r3, %lo(.L.str.2)
	jal r31, atom_intern
	lui r3, %hi(ATOM_NIL_LIST)
	addi r3, r3, %lo(ATOM_NIL_LIST)
	stw r3+0, r1
	lui r3, %hi(.L.str.3)
	addi r3, r3, %lo(.L.str.3)
	jal r31, atom_intern
	lui r3, %hi(ATOM_DOT)
	addi r3, r3, %lo(ATOM_DOT)
	stw r3+0, r1
	lui r3, %hi(.L.str.4)
	addi r3, r3, %lo(.L.str.4)
	jal r31, atom_intern
	lui r3, %hi(ATOM_TRUE)
	addi r3, r3, %lo(ATOM_TRUE)
	stw r3+0, r1
	lui r3, %hi(.L.str.5)
	addi r3, r3, %lo(.L.str.5)
	jal r31, atom_intern
	lui r3, %hi(ATOM_FAIL)
	addi r3, r3, %lo(ATOM_FAIL)
	stw r3+0, r1
	lui r3, %hi(.L.str.6)
	addi r3, r3, %lo(.L.str.6)
	jal r31, atom_intern
	lui r3, %hi(ATOM_CLAUSE)
	addi r3, r3, %lo(ATOM_CLAUSE)
	stw r3+0, r1
	lui r3, %hi(.L.str.7)
	addi r3, r3, %lo(.L.str.7)
	jal r31, atom_intern
	lui r3, %hi(ATOM_QUERY)
	addi r3, r3, %lo(ATOM_QUERY)
	stw r3+0, r1
	lui r3, %hi(.L.str.8)
	addi r3, r3, %lo(.L.str.8)
	jal r31, atom_intern
	lui r3, %hi(ATOM_COMMA)
	addi r3, r3, %lo(ATOM_COMMA)
	stw r3+0, r1
	lui r3, %hi(.L.str.9)
	addi r3, r3, %lo(.L.str.9)
	jal r31, atom_intern
	lui r3, %hi(ATOM_SEMI)
	addi r3, r3, %lo(ATOM_SEMI)
	stw r3+0, r1
	lui r3, %hi(.L.str.10)
	addi r3, r3, %lo(.L.str.10)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ARROW)
	addi r3, r3, %lo(ATOM_ARROW)
	stw r3+0, r1
	lui r3, %hi(.L.str.11)
	addi r3, r3, %lo(.L.str.11)
	jal r31, atom_intern
	lui r3, %hi(ATOM_NOT)
	addi r3, r3, %lo(ATOM_NOT)
	stw r3+0, r1
	lui r3, %hi(.L.str.12)
	addi r3, r3, %lo(.L.str.12)
	jal r31, atom_intern
	lui r3, %hi(ATOM_IS)
	addi r3, r3, %lo(ATOM_IS)
	stw r3+0, r1
	lui r3, %hi(.L.str.13)
	addi r3, r3, %lo(.L.str.13)
	jal r31, atom_intern
	lui r3, %hi(ATOM_UNIFY)
	addi r3, r3, %lo(ATOM_UNIFY)
	stw r3+0, r1
	lui r3, %hi(.L.str.14)
	addi r3, r3, %lo(.L.str.14)
	jal r31, atom_intern
	lui r3, %hi(ATOM_NOT_UNIFY)
	addi r3, r3, %lo(ATOM_NOT_UNIFY)
	stw r3+0, r1
	lui r3, %hi(.L.str.15)
	addi r3, r3, %lo(.L.str.15)
	jal r31, atom_intern
	lui r3, %hi(ATOM_EQ)
	addi r3, r3, %lo(ATOM_EQ)
	stw r3+0, r1
	lui r3, %hi(.L.str.16)
	addi r3, r3, %lo(.L.str.16)
	jal r31, atom_intern
	lui r3, %hi(ATOM_NEQ)
	addi r3, r3, %lo(ATOM_NEQ)
	stw r3+0, r1
	lui r3, %hi(.L.str.17)
	addi r3, r3, %lo(.L.str.17)
	jal r31, atom_intern
	lui r3, %hi(ATOM_CUT)
	addi r3, r3, %lo(ATOM_CUT)
	stw r3+0, r1
	lui r3, %hi(.L.str.18)
	addi r3, r3, %lo(.L.str.18)
	jal r31, atom_intern
	lui r3, %hi(ATOM_PLUS)
	addi r3, r3, %lo(ATOM_PLUS)
	stw r3+0, r1
	lui r3, %hi(.L.str.19)
	addi r3, r3, %lo(.L.str.19)
	jal r31, atom_intern
	lui r3, %hi(ATOM_MINUS)
	addi r3, r3, %lo(ATOM_MINUS)
	stw r3+0, r1
	lui r3, %hi(.L.str.20)
	addi r3, r3, %lo(.L.str.20)
	jal r31, atom_intern
	lui r3, %hi(ATOM_STAR)
	addi r3, r3, %lo(ATOM_STAR)
	stw r3+0, r1
	lui r3, %hi(.L.str.21)
	addi r3, r3, %lo(.L.str.21)
	jal r31, atom_intern
	lui r3, %hi(ATOM_SLASH2)
	addi r3, r3, %lo(ATOM_SLASH2)
	stw r3+0, r1
	lui r3, %hi(.L.str.22)
	addi r3, r3, %lo(.L.str.22)
	jal r31, atom_intern
	lui r3, %hi(ATOM_MOD)
	addi r3, r3, %lo(ATOM_MOD)
	stw r3+0, r1
	lui r3, %hi(.L.str.23)
	addi r3, r3, %lo(.L.str.23)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ABS)
	addi r3, r3, %lo(ATOM_ABS)
	stw r3+0, r1
	lui r3, %hi(.L.str.24)
	addi r3, r3, %lo(.L.str.24)
	jal r31, atom_intern
	lui r3, %hi(ATOM_MIN)
	addi r3, r3, %lo(ATOM_MIN)
	stw r3+0, r1
	lui r3, %hi(.L.str.25)
	addi r3, r3, %lo(.L.str.25)
	jal r31, atom_intern
	lui r3, %hi(ATOM_MAX)
	addi r3, r3, %lo(ATOM_MAX)
	stw r3+0, r1
	lui r3, %hi(.L.str.26)
	addi r3, r3, %lo(.L.str.26)
	jal r31, atom_intern
	lui r3, %hi(ATOM_LT)
	addi r3, r3, %lo(ATOM_LT)
	stw r3+0, r1
	lui r3, %hi(.L.str.27)
	addi r3, r3, %lo(.L.str.27)
	jal r31, atom_intern
	lui r3, %hi(ATOM_GT)
	addi r3, r3, %lo(ATOM_GT)
	stw r3+0, r1
	lui r3, %hi(.L.str.28)
	addi r3, r3, %lo(.L.str.28)
	jal r31, atom_intern
	lui r3, %hi(ATOM_LE)
	addi r3, r3, %lo(ATOM_LE)
	stw r3+0, r1
	lui r3, %hi(.L.str.29)
	addi r3, r3, %lo(.L.str.29)
	jal r31, atom_intern
	lui r3, %hi(ATOM_GE)
	addi r3, r3, %lo(ATOM_GE)
	stw r3+0, r1
	lui r3, %hi(.L.str.30)
	addi r3, r3, %lo(.L.str.30)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ARITH_EQ)
	addi r3, r3, %lo(ATOM_ARITH_EQ)
	stw r3+0, r1
	lui r3, %hi(.L.str.31)
	addi r3, r3, %lo(.L.str.31)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ARITH_NEQ)
	addi r3, r3, %lo(ATOM_ARITH_NEQ)
	stw r3+0, r1
	lui r3, %hi(.L.str.32)
	addi r3, r3, %lo(.L.str.32)
	jal r31, atom_intern
	lui r3, %hi(ATOM_WRITE)
	addi r3, r3, %lo(ATOM_WRITE)
	stw r3+0, r1
	lui r3, %hi(.L.str.33)
	addi r3, r3, %lo(.L.str.33)
	jal r31, atom_intern
	lui r3, %hi(ATOM_WRITELN)
	addi r3, r3, %lo(ATOM_WRITELN)
	stw r3+0, r1
	lui r3, %hi(.L.str.34)
	addi r3, r3, %lo(.L.str.34)
	jal r31, atom_intern
	lui r3, %hi(ATOM_NL)
	addi r3, r3, %lo(ATOM_NL)
	stw r3+0, r1
	lui r3, %hi(.L.str.35)
	addi r3, r3, %lo(.L.str.35)
	jal r31, atom_intern
	lui r3, %hi(ATOM_HALT)
	addi r3, r3, %lo(ATOM_HALT)
	stw r3+0, r1
	lui r3, %hi(.L.str.36)
	addi r3, r3, %lo(.L.str.36)
	jal r31, atom_intern
	lui r3, %hi(ATOM_READ)
	addi r3, r3, %lo(ATOM_READ)
	stw r3+0, r1
	lui r3, %hi(.L.str.37)
	addi r3, r3, %lo(.L.str.37)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ASSERT)
	addi r3, r3, %lo(ATOM_ASSERT)
	stw r3+0, r1
	lui r3, %hi(.L.str.38)
	addi r3, r3, %lo(.L.str.38)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ASSERTA)
	addi r3, r3, %lo(ATOM_ASSERTA)
	stw r3+0, r1
	lui r3, %hi(.L.str.39)
	addi r3, r3, %lo(.L.str.39)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ASSERTZ)
	addi r3, r3, %lo(ATOM_ASSERTZ)
	stw r3+0, r1
	lui r3, %hi(.L.str.40)
	addi r3, r3, %lo(.L.str.40)
	jal r31, atom_intern
	lui r3, %hi(ATOM_RETRACT)
	addi r3, r3, %lo(ATOM_RETRACT)
	stw r3+0, r1
	lui r3, %hi(.L.str.41)
	addi r3, r3, %lo(.L.str.41)
	jal r31, atom_intern
	lui r3, %hi(ATOM_FINDALL)
	addi r3, r3, %lo(ATOM_FINDALL)
	stw r3+0, r1
	lui r3, %hi(.L.str.42)
	addi r3, r3, %lo(.L.str.42)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ATOM)
	addi r3, r3, %lo(ATOM_ATOM)
	stw r3+0, r1
	lui r3, %hi(.L.str.43)
	addi r3, r3, %lo(.L.str.43)
	jal r31, atom_intern
	lui r3, %hi(ATOM_INTEGER)
	addi r3, r3, %lo(ATOM_INTEGER)
	stw r3+0, r1
	lui r3, %hi(.L.str.44)
	addi r3, r3, %lo(.L.str.44)
	jal r31, atom_intern
	lui r3, %hi(ATOM_VAR)
	addi r3, r3, %lo(ATOM_VAR)
	stw r3+0, r1
	lui r3, %hi(.L.str.45)
	addi r3, r3, %lo(.L.str.45)
	jal r31, atom_intern
	lui r3, %hi(ATOM_NONVAR)
	addi r3, r3, %lo(ATOM_NONVAR)
	stw r3+0, r1
	lui r3, %hi(.L.str.46)
	addi r3, r3, %lo(.L.str.46)
	jal r31, atom_intern
	lui r3, %hi(ATOM_COMPOUND)
	addi r3, r3, %lo(ATOM_COMPOUND)
	stw r3+0, r1
	lui r3, %hi(.L.str.47)
	addi r3, r3, %lo(.L.str.47)
	jal r31, atom_intern
	lui r3, %hi(ATOM_NUMBER)
	addi r3, r3, %lo(ATOM_NUMBER)
	stw r3+0, r1
	lui r3, %hi(.L.str.48)
	addi r3, r3, %lo(.L.str.48)
	jal r31, atom_intern
	lui r3, %hi(ATOM_IS_LIST)
	addi r3, r3, %lo(ATOM_IS_LIST)
	stw r3+0, r1
	lui r3, %hi(.L.str.49)
	addi r3, r3, %lo(.L.str.49)
	jal r31, atom_intern
	lui r3, %hi(ATOM_FUNCTOR)
	addi r3, r3, %lo(ATOM_FUNCTOR)
	stw r3+0, r1
	lui r3, %hi(.L.str.50)
	addi r3, r3, %lo(.L.str.50)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ARG)
	addi r3, r3, %lo(ATOM_ARG)
	stw r3+0, r1
	lui r3, %hi(.L.str.51)
	addi r3, r3, %lo(.L.str.51)
	jal r31, atom_intern
	lui r3, %hi(ATOM_UNIV)
	addi r3, r3, %lo(ATOM_UNIV)
	stw r3+0, r1
	lui r3, %hi(.L.str.52)
	addi r3, r3, %lo(.L.str.52)
	jal r31, atom_intern
	lui r3, %hi(ATOM_COPY_TERM)
	addi r3, r3, %lo(ATOM_COPY_TERM)
	stw r3+0, r1
	lui r3, %hi(.L.str.53)
	addi r3, r3, %lo(.L.str.53)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ATOM_LENGTH)
	addi r3, r3, %lo(ATOM_ATOM_LENGTH)
	stw r3+0, r1
	lui r3, %hi(.L.str.54)
	addi r3, r3, %lo(.L.str.54)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ATOM_CHARS)
	addi r3, r3, %lo(ATOM_ATOM_CHARS)
	stw r3+0, r1
	lui r3, %hi(.L.str.55)
	addi r3, r3, %lo(.L.str.55)
	jal r31, atom_intern
	lui r3, %hi(ATOM_CHAR_CODE)
	addi r3, r3, %lo(ATOM_CHAR_CODE)
	stw r3+0, r1
	lui r3, %hi(.L.str.56)
	addi r3, r3, %lo(.L.str.56)
	jal r31, atom_intern
	lui r3, %hi(ATOM_NUMBER_CHARS)
	addi r3, r3, %lo(ATOM_NUMBER_CHARS)
	stw r3+0, r1
	lui r3, %hi(.L.str.57)
	addi r3, r3, %lo(.L.str.57)
	jal r31, atom_intern
	lui r3, %hi(ATOM_ATOM_CONCAT)
	addi r3, r3, %lo(ATOM_ATOM_CONCAT)
	stw r3+0, r1
	lui r3, %hi(.L.str.58)
	addi r3, r3, %lo(.L.str.58)
	jal r31, atom_intern
	lui r3, %hi(ATOM_WRITE_CANONICAL)
	addi r3, r3, %lo(ATOM_WRITE_CANONICAL)
	stw r3+0, r1
	lui r3, %hi(.L.str.59)
	addi r3, r3, %lo(.L.str.59)
	jal r31, atom_intern
	lui r3, %hi(ATOM_SUCC)
	addi r3, r3, %lo(ATOM_SUCC)
	stw r3+0, r1
	lui r3, %hi(.L.str.60)
	addi r3, r3, %lo(.L.str.60)
	jal r31, atom_intern
	lui r3, %hi(ATOM_PLUS2)
	addi r3, r3, %lo(ATOM_PLUS2)
	stw r3+0, r1
	lui r3, %hi(.L.str.61)
	addi r3, r3, %lo(.L.str.61)
	jal r31, atom_intern
	lui r3, %hi(ATOM_LENGTH)
	addi r3, r3, %lo(ATOM_LENGTH)
	stw r3+0, r1
	lui r3, %hi(.L.str.62)
	addi r3, r3, %lo(.L.str.62)
	jal r31, atom_intern
	lui r3, %hi(ATOM_APPEND)
	addi r3, r3, %lo(ATOM_APPEND)
	stw r3+0, r1
	lui r3, %hi(.L.str.63)
	addi r3, r3, %lo(.L.str.63)
	jal r31, atom_intern
	lui r3, %hi(ATOM_LAST)
	addi r3, r3, %lo(ATOM_LAST)
	stw r3+0, r1
	lui r3, %hi(.L.str.64)
	addi r3, r3, %lo(.L.str.64)
	jal r31, atom_intern
	lui r3, %hi(ATOM_BETWEEN)
	addi r3, r3, %lo(ATOM_BETWEEN)
	stw r3+0, r1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end2:
	.size	init_atoms, .Lfunc_end2-init_atoms
                                        # -- End function
	.globl	heap_alloc                      # -- Begin function heap_alloc
	.p2align	2
	.type	heap_alloc,@function
heap_alloc:                             # @heap_alloc
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r4, %hi(hp)
	addi r4, r4, %lo(hp)
	ldw r1, r4+0
	add r3, r1, r3
	lui r5, 64
	addi r5, r5, 1
	blt r3, r5, .LBB3_2
.LBB3_1:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.65)
	addi r5, r5, %lo(.L.str.65)
	addi r4, r0, 256
	jal r31, snprintf
	addi r1, r0, 0
	jal r0, .LBB3_3
.LBB3_2:
	stw r4+0, r3
.LBB3_3:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	heap_alloc, .Lfunc_end3-heap_alloc
                                        # -- End function
	.globl	code_heap_alloc                 # -- Begin function code_heap_alloc
	.p2align	2
	.type	code_heap_alloc,@function
code_heap_alloc:                        # @code_heap_alloc
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r4, %hi(code_hp)
	addi r4, r4, %lo(code_hp)
	ldw r1, r4+0
	add r3, r1, r3
	lui r5, 32
	addi r5, r5, 1
	blt r3, r5, .LBB4_2
.LBB4_1:
	lui r1, %hi(g_error)
	addi r1, r1, %lo(g_error)
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.66)
	addi r5, r5, %lo(.L.str.66)
	addi r4, r0, 256
	jal r31, snprintf
	addi r1, r0, 0
	jal r0, .LBB4_3
.LBB4_2:
	stw r4+0, r3
.LBB4_3:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	code_heap_alloc, .Lfunc_end4-code_heap_alloc
                                        # -- End function
	.globl	make_compound                   # -- Begin function make_compound
	.p2align	2
	.type	make_compound,@function
make_compound:                          # @make_compound
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
	stw fp+-20, lr
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	lui r1, %hi(hp)
	addi r1, r1, %lo(hp)
	ldw r3, r1+0
	add r4, r4, r3
	addi r4, r4, 2
	lui r5, 64
	addi r5, r5, 1
	lui r14, %hi(g_error)
	addi r14, r14, %lo(g_error)
	blt r4, r5, .LBB5_2
.LBB5_1:
	addi r1, r0, 1
	stw r14+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.65)
	addi r5, r5, %lo(.L.str.65)
	addi r4, r0, 256
	jal r31, snprintf
	addi r3, r0, 0
	jal r0, .LBB5_3
.LBB5_2:
	stw r1+0, r4
.LBB5_3:
	ldw r4, r14+0
	addi r1, r0, 0
	bne r4, r1, .LBB5_7
.LBB5_4:
	slli r1, r13, 2
	addi r4, r1, 2
	slli r1, r3, 2
	lui r3, %hi(heap)
	addi r3, r3, %lo(heap)
	add r3, r1, r3
	stw r3+0, r4
	slli r4, r12, 2
	addi r4, r4, 1
	lui r5, %hi(heap+4)
	addi r5, r5, %lo(heap+4)
	add r5, r1, r5
	stw r5+0, r4
	addi r4, r0, 1
	blt r12, r4, .LBB5_7
.LBB5_5:
	addi r3, r3, 8
	addi r4, r0, 0
.LBB5_6:
	ldw r5, r11+0
	stw r3+0, r5
	addi r12, r12, -1
	addi r3, r3, 4
	addi r11, r11, 4
	bne r12, r4, .LBB5_6
.LBB5_7:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end5:
	.size	make_compound, .Lfunc_end5-make_compound
                                        # -- End function
	.globl	make_compound_on_code           # -- Begin function make_compound_on_code
	.p2align	2
	.type	make_compound_on_code,@function
make_compound_on_code:                  # @make_compound_on_code
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
	stw fp+-20, lr
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	lui r1, %hi(code_hp)
	addi r1, r1, %lo(code_hp)
	ldw r3, r1+0
	add r4, r4, r3
	addi r4, r4, 2
	lui r5, 32
	addi r5, r5, 1
	lui r14, %hi(g_error)
	addi r14, r14, %lo(g_error)
	blt r4, r5, .LBB6_2
.LBB6_1:
	addi r1, r0, 1
	stw r14+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.66)
	addi r5, r5, %lo(.L.str.66)
	addi r4, r0, 256
	jal r31, snprintf
	addi r3, r0, 0
	jal r0, .LBB6_3
.LBB6_2:
	stw r1+0, r4
.LBB6_3:
	ldw r4, r14+0
	addi r1, r0, 0
	bne r4, r1, .LBB6_8
.LBB6_4:
	slli r1, r13, 2
	addi r4, r1, 2
	slli r1, r3, 2
	lui r3, %hi(code_heap)
	addi r3, r3, %lo(code_heap)
	add r3, r1, r3
	stw r3+0, r4
	slli r4, r12, 2
	addi r4, r4, 1
	lui r5, %hi(code_heap+4)
	addi r5, r5, %lo(code_heap+4)
	add r5, r1, r5
	stw r5+0, r4
	addi r4, r0, 1
	blt r12, r4, .LBB6_7
.LBB6_5:
	addi r3, r3, 8
	addi r4, r0, 0
.LBB6_6:
	ldw r5, r11+0
	stw r3+0, r5
	addi r12, r12, -1
	addi r3, r3, 4
	addi r11, r11, 4
	bne r12, r4, .LBB6_6
.LBB6_7:
	lui r3, 256
	add r1, r1, r3
.LBB6_8:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end6:
	.size	make_compound_on_code, .Lfunc_end6-make_compound_on_code
                                        # -- End function
	.globl	make_list_cons                  # -- Begin function make_list_cons
	.p2align	2
	.type	make_list_cons,@function
make_list_cons:                         # @make_list_cons
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, lr
	addi r11, fp, -24
	stw r11+0, r3
	stw r11+4, r4
	lui r1, %hi(ATOM_DOT)
	addi r1, r1, %lo(ATOM_DOT)
	ldw r12, r1+0
	lui r1, %hi(hp)
	addi r1, r1, %lo(hp)
	ldw r3, r1+0
	lui r4, 64
	addi r4, r4, -3
	lui r13, %hi(g_error)
	addi r13, r13, %lo(g_error)
	blt r3, r4, .LBB7_2
.LBB7_1:
	addi r1, r0, 1
	stw r13+0, r1
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.65)
	addi r5, r5, %lo(.L.str.65)
	addi r4, r0, 256
	jal r31, snprintf
	addi r3, r0, 0
	jal r0, .LBB7_3
.LBB7_2:
	addi r4, r3, 4
	stw r1+0, r4
.LBB7_3:
	ldw r4, r13+0
	addi r1, r0, 0
	bne r4, r1, .LBB7_6
.LBB7_4:
	slli r1, r12, 2
	addi r4, r1, 2
	slli r1, r3, 2
	lui r3, %hi(heap)
	addi r3, r3, %lo(heap)
	add r3, r1, r3
	stw r3+0, r4
	lui r3, %hi(heap+4)
	addi r3, r3, %lo(heap+4)
	add r3, r1, r3
	addi r4, r0, 9
	stw r3+0, r4
	lui r3, %hi(heap+8)
	addi r3, r3, %lo(heap+8)
	add r3, r1, r3
	addi r4, r0, 0
	addi r5, r0, 8
.LBB7_5:
	add r6, r11, r4
	ldw r6, r6+0
	add r7, r3, r4
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r5, .LBB7_5
.LBB7_6:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end7:
	.size	make_list_cons, .Lfunc_end7-make_list_cons
                                        # -- End function
	.globl	compound_functor                # -- Begin function compound_functor
	.p2align	2
	.type	compound_functor,@function
compound_functor:                       # @compound_functor
# %bb.0:
	srai r1, r3, 2
	lui r4, 64
	addi r4, r4, -1
	sgt r1, r1, r4
	addi r4, r0, -4
	and r3, r3, r4
	lui r4, %hi(heap)
	addi r4, r4, %lo(heap)
	lui r5, %hi(code_heap-1048576)
	addi r5, r5, %lo(code_heap-1048576)
	xor r5, r5, r4
	addi r6, r0, 0
	sub r1, r6, r1
	and r1, r5, r1
	xor r1, r1, r4
	add r1, r3, r1
	ldw r1, r1+0
	srli r1, r1, 2
	jalr r0, r31, 0
.Lfunc_end8:
	.size	compound_functor, .Lfunc_end8-compound_functor
                                        # -- End function
	.globl	compound_arity                  # -- Begin function compound_arity
	.p2align	2
	.type	compound_arity,@function
compound_arity:                         # @compound_arity
# %bb.0:
	srai r1, r3, 2
	lui r4, 64
	addi r4, r4, -1
	sgt r1, r1, r4
	addi r4, r0, -4
	and r3, r3, r4
	lui r4, %hi(heap)
	addi r4, r4, %lo(heap)
	lui r5, %hi(code_heap-1048576)
	addi r5, r5, %lo(code_heap-1048576)
	xor r5, r5, r4
	addi r6, r0, 0
	sub r1, r6, r1
	and r1, r5, r1
	xor r1, r1, r4
	add r1, r3, r1
	ldw r1, r1+4
	srai r1, r1, 2
	jalr r0, r31, 0
.Lfunc_end9:
	.size	compound_arity, .Lfunc_end9-compound_arity
                                        # -- End function
	.globl	compound_arg                    # -- Begin function compound_arg
	.p2align	2
	.type	compound_arg,@function
compound_arg:                           # @compound_arg
# %bb.0:
	srai r1, r3, 2
	lui r5, 64
	addi r5, r5, -1
	sgt r1, r1, r5
	addi r5, r0, -4
	and r3, r3, r5
	lui r5, %hi(heap)
	addi r5, r5, %lo(heap)
	lui r6, %hi(code_heap-1048576)
	addi r6, r6, %lo(code_heap-1048576)
	xor r6, r6, r5
	addi r7, r0, 0
	sub r1, r7, r1
	and r1, r6, r1
	xor r1, r1, r5
	add r1, r3, r1
	slli r3, r4, 2
	add r1, r1, r3
	ldw r1, r1+8
	jalr r0, r31, 0
.Lfunc_end10:
	.size	compound_arg, .Lfunc_end10-compound_arg
                                        # -- End function
	.globl	compound_set_arg                # -- Begin function compound_set_arg
	.p2align	2
	.type	compound_set_arg,@function
compound_set_arg:                       # @compound_set_arg
# %bb.0:
	srai r1, r3, 2
	lui r6, 64
	addi r6, r6, -1
	sgt r1, r1, r6
	addi r6, r0, -4
	and r3, r3, r6
	lui r6, %hi(heap)
	addi r6, r6, %lo(heap)
	lui r7, %hi(code_heap-1048576)
	addi r7, r7, %lo(code_heap-1048576)
	xor r7, r7, r6
	addi r8, r0, 0
	sub r1, r8, r1
	and r1, r7, r1
	xor r1, r1, r6
	add r1, r3, r1
	slli r3, r4, 2
	add r1, r1, r3
	stw r1+8, r5
	jalr r0, r31, 0
.Lfunc_end11:
	.size	compound_set_arg, .Lfunc_end11-compound_set_arg
                                        # -- End function
	.globl	term_functor                    # -- Begin function term_functor
	.p2align	2
	.type	term_functor,@function
term_functor:                           # @term_functor
# %bb.0:
	andi r5, r3, 3
	addi r1, r0, 2
	bne r5, r1, .LBB12_2
.LBB12_1:
	srli r1, r3, 2
	jal r0, .LBB12_5
.LBB12_2:
	addi r1, r0, -1
	addi r4, r0, 0
	beq r3, r4, .LBB12_5
.LBB12_3:
	bne r5, r4, .LBB12_5
.LBB12_4:
	lui r1, 256
	addi r1, r1, -4
	sgt r1, r3, r1
	lui r5, %hi(heap)
	addi r5, r5, %lo(heap)
	lui r6, %hi(code_heap-1048576)
	addi r6, r6, %lo(code_heap-1048576)
	xor r6, r6, r5
	sub r1, r4, r1
	and r1, r6, r1
	xor r1, r1, r5
	add r1, r3, r1
	ldw r1, r1+0
	srli r1, r1, 2
.LBB12_5:
	jalr r0, r31, 0
.Lfunc_end12:
	.size	term_functor, .Lfunc_end12-term_functor
                                        # -- End function
	.globl	term_arity                      # -- Begin function term_arity
	.p2align	2
	.type	term_arity,@function
term_arity:                             # @term_arity
# %bb.0:
	andi r5, r3, 3
	addi r1, r0, 2
	bne r5, r1, .LBB13_2
.LBB13_1:
	addi r1, r0, 0
	jal r0, .LBB13_5
.LBB13_2:
	addi r1, r0, -1
	addi r4, r0, 0
	beq r3, r4, .LBB13_5
.LBB13_3:
	bne r5, r4, .LBB13_5
.LBB13_4:
	lui r1, 256
	addi r1, r1, -4
	sgt r1, r3, r1
	lui r5, %hi(heap)
	addi r5, r5, %lo(heap)
	lui r6, %hi(code_heap-1048576)
	addi r6, r6, %lo(code_heap-1048576)
	xor r6, r6, r5
	sub r1, r4, r1
	and r1, r6, r1
	xor r1, r1, r5
	add r1, r3, r1
	ldw r1, r1+4
	srai r1, r1, 2
.LBB13_5:
	jalr r0, r31, 0
.Lfunc_end13:
	.size	term_arity, .Lfunc_end13-term_arity
                                        # -- End function
	.globl	persist_term                    # -- Begin function persist_term
	.p2align	2
	.type	persist_term,@function
persist_term:                           # @persist_term
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
	addi r11, r0, 0
	beq r3, r11, .LBB14_7
.LBB14_1:
	andi r1, r3, 3
	bne r1, r11, .LBB14_7
.LBB14_2:
	lui r12, 256
	addi r1, r12, -4
	sgt r1, r3, r1
	lui r4, %hi(heap)
	addi r4, r4, %lo(heap)
	lui r5, %hi(code_heap-1048576)
	addi r5, r5, %lo(code_heap-1048576)
	xor r5, r5, r4
	sub r1, r11, r1
	and r1, r5, r1
	xor r1, r1, r4
	add r1, r3, r1
	ldw r16, r1+0
	ldw r15, r1+4
	srai r13, r15, 2
	addi r14, r0, 1
	blt r13, r14, .LBB14_5
.LBB14_3:
	addi r3, r0, 32
	slt r3, r13, r3
	sub r3, r11, r3
	xori r4, r13, 32
	and r3, r4, r3
	xori r17, r3, 32
	addi r18, r1, 8
	addi r19, fp, -172
	addi r20, r0, 0
.LBB14_4:
	ldw r3, r18+0
	jal r31, persist_term
	stw r19+0, r1
	addi r17, r17, -1
	addi r19, r19, 4
	addi r18, r18, 4
	bne r17, r20, .LBB14_4
.LBB14_5:
	lui r3, %hi(code_hp)
	addi r3, r3, %lo(code_hp)
	ldw r1, r3+0
	add r4, r13, r1
	addi r4, r4, 2
	lui r5, 32
	addi r5, r5, 1
	lui r17, %hi(g_error)
	addi r17, r17, %lo(g_error)
	blt r4, r5, .LBB14_8
.LBB14_6:
	stw r17+0, r14
	lui r3, %hi(g_errmsg)
	addi r3, r3, %lo(g_errmsg)
	lui r5, %hi(.L.str.66)
	addi r5, r5, %lo(.L.str.66)
	addi r4, r0, 256
	jal r31, snprintf
	addi r1, r0, 0
	ldw r3, r17+0
	bne r3, r11, .LBB14_13
	jal r0, .LBB14_9
.LBB14_7:
	add r11, r3, r0
	jal r0, .LBB14_13
.LBB14_8:
	stw r3+0, r4
	ldw r3, r17+0
	bne r3, r11, .LBB14_13
.LBB14_9:
	addi r4, r0, -4
	and r3, r16, r4
	addi r5, r3, 2
	slli r1, r1, 2
	lui r3, %hi(code_heap)
	addi r3, r3, %lo(code_heap)
	add r3, r1, r3
	stw r3+0, r5
	and r4, r15, r4
	addi r4, r4, 1
	lui r5, %hi(code_heap+4)
	addi r5, r5, %lo(code_heap+4)
	add r5, r1, r5
	stw r5+0, r4
	blt r13, r14, .LBB14_12
.LBB14_10:
	addi r3, r3, 8
	addi r4, fp, -172
	addi r5, r0, 0
.LBB14_11:
	ldw r6, r4+0
	stw r3+0, r6
	addi r13, r13, -1
	addi r3, r3, 4
	addi r4, r4, 4
	bne r13, r5, .LBB14_11
.LBB14_12:
	add r11, r1, r12
.LBB14_13:
	add r1, r11, r0
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
.Lfunc_end14:
	.size	persist_term, .Lfunc_end14-persist_term
                                        # -- End function
	.type	hp,@object                      # @hp
	.data
	.globl	hp
	.p2align	2, 0x0
hp:
	.word	1                               # 0x1
	.size	hp, 4

	.type	code_hp,@object                 # @code_hp
	.globl	code_hp
	.p2align	2, 0x0
code_hp:
	.word	1                               # 0x1
	.size	code_hp, 4

	.type	atom_count,@object              # @atom_count
	.bss
	.globl	atom_count
	.p2align	2, 0x0
atom_count:
	.word	0                               # 0x0
	.size	atom_count, 4

	.type	g_error,@object                 # @g_error
	.globl	g_error
	.p2align	2, 0x0
g_error:
	.word	0                               # 0x0
	.size	g_error, 4

	.type	atom_names,@object              # @atom_names
	.globl	atom_names
	.p2align	2, 0x0
atom_names:
	.zero	8192
	.size	atom_names, 8192

	.type	g_errmsg,@object                # @g_errmsg
	.globl	g_errmsg
g_errmsg:
	.zero	256
	.size	g_errmsg, 256

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"too many atoms"
	.size	.L.str, 15

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"?"
	.size	.L.str.1, 2

	.type	atom_store_pos,@object          # @atom_store_pos
	.local	atom_store_pos
	.comm	atom_store_pos,4,4
	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"[]"
	.size	.L.str.2, 3

	.type	ATOM_NIL_LIST,@object           # @ATOM_NIL_LIST
	.bss
	.globl	ATOM_NIL_LIST
	.p2align	2, 0x0
ATOM_NIL_LIST:
	.word	0                               # 0x0
	.size	ATOM_NIL_LIST, 4

	.type	.L.str.3,@object                # @.str.3
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.3:
	.asciz	"."
	.size	.L.str.3, 2

	.type	ATOM_DOT,@object                # @ATOM_DOT
	.bss
	.globl	ATOM_DOT
	.p2align	2, 0x0
ATOM_DOT:
	.word	0                               # 0x0
	.size	ATOM_DOT, 4

	.type	.L.str.4,@object                # @.str.4
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.4:
	.asciz	"true"
	.size	.L.str.4, 5

	.type	ATOM_TRUE,@object               # @ATOM_TRUE
	.bss
	.globl	ATOM_TRUE
	.p2align	2, 0x0
ATOM_TRUE:
	.word	0                               # 0x0
	.size	ATOM_TRUE, 4

	.type	.L.str.5,@object                # @.str.5
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.5:
	.asciz	"fail"
	.size	.L.str.5, 5

	.type	ATOM_FAIL,@object               # @ATOM_FAIL
	.bss
	.globl	ATOM_FAIL
	.p2align	2, 0x0
ATOM_FAIL:
	.word	0                               # 0x0
	.size	ATOM_FAIL, 4

	.type	.L.str.6,@object                # @.str.6
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.6:
	.asciz	":-"
	.size	.L.str.6, 3

	.type	ATOM_CLAUSE,@object             # @ATOM_CLAUSE
	.bss
	.globl	ATOM_CLAUSE
	.p2align	2, 0x0
ATOM_CLAUSE:
	.word	0                               # 0x0
	.size	ATOM_CLAUSE, 4

	.type	.L.str.7,@object                # @.str.7
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.7:
	.asciz	"?-"
	.size	.L.str.7, 3

	.type	ATOM_QUERY,@object              # @ATOM_QUERY
	.bss
	.globl	ATOM_QUERY
	.p2align	2, 0x0
ATOM_QUERY:
	.word	0                               # 0x0
	.size	ATOM_QUERY, 4

	.type	.L.str.8,@object                # @.str.8
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.8:
	.asciz	","
	.size	.L.str.8, 2

	.type	ATOM_COMMA,@object              # @ATOM_COMMA
	.bss
	.globl	ATOM_COMMA
	.p2align	2, 0x0
ATOM_COMMA:
	.word	0                               # 0x0
	.size	ATOM_COMMA, 4

	.type	.L.str.9,@object                # @.str.9
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.9:
	.asciz	";"
	.size	.L.str.9, 2

	.type	ATOM_SEMI,@object               # @ATOM_SEMI
	.bss
	.globl	ATOM_SEMI
	.p2align	2, 0x0
ATOM_SEMI:
	.word	0                               # 0x0
	.size	ATOM_SEMI, 4

	.type	.L.str.10,@object               # @.str.10
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.10:
	.asciz	"->"
	.size	.L.str.10, 3

	.type	ATOM_ARROW,@object              # @ATOM_ARROW
	.bss
	.globl	ATOM_ARROW
	.p2align	2, 0x0
ATOM_ARROW:
	.word	0                               # 0x0
	.size	ATOM_ARROW, 4

	.type	.L.str.11,@object               # @.str.11
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.11:
	.asciz	"\\+"
	.size	.L.str.11, 3

	.type	ATOM_NOT,@object                # @ATOM_NOT
	.bss
	.globl	ATOM_NOT
	.p2align	2, 0x0
ATOM_NOT:
	.word	0                               # 0x0
	.size	ATOM_NOT, 4

	.type	.L.str.12,@object               # @.str.12
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.12:
	.asciz	"is"
	.size	.L.str.12, 3

	.type	ATOM_IS,@object                 # @ATOM_IS
	.bss
	.globl	ATOM_IS
	.p2align	2, 0x0
ATOM_IS:
	.word	0                               # 0x0
	.size	ATOM_IS, 4

	.type	.L.str.13,@object               # @.str.13
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.13:
	.asciz	"="
	.size	.L.str.13, 2

	.type	ATOM_UNIFY,@object              # @ATOM_UNIFY
	.bss
	.globl	ATOM_UNIFY
	.p2align	2, 0x0
ATOM_UNIFY:
	.word	0                               # 0x0
	.size	ATOM_UNIFY, 4

	.type	.L.str.14,@object               # @.str.14
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.14:
	.asciz	"\\="
	.size	.L.str.14, 3

	.type	ATOM_NOT_UNIFY,@object          # @ATOM_NOT_UNIFY
	.bss
	.globl	ATOM_NOT_UNIFY
	.p2align	2, 0x0
ATOM_NOT_UNIFY:
	.word	0                               # 0x0
	.size	ATOM_NOT_UNIFY, 4

	.type	.L.str.15,@object               # @.str.15
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.15:
	.asciz	"=="
	.size	.L.str.15, 3

	.type	ATOM_EQ,@object                 # @ATOM_EQ
	.bss
	.globl	ATOM_EQ
	.p2align	2, 0x0
ATOM_EQ:
	.word	0                               # 0x0
	.size	ATOM_EQ, 4

	.type	.L.str.16,@object               # @.str.16
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.16:
	.asciz	"\\=="
	.size	.L.str.16, 4

	.type	ATOM_NEQ,@object                # @ATOM_NEQ
	.bss
	.globl	ATOM_NEQ
	.p2align	2, 0x0
ATOM_NEQ:
	.word	0                               # 0x0
	.size	ATOM_NEQ, 4

	.type	.L.str.17,@object               # @.str.17
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.17:
	.asciz	"!"
	.size	.L.str.17, 2

	.type	ATOM_CUT,@object                # @ATOM_CUT
	.bss
	.globl	ATOM_CUT
	.p2align	2, 0x0
ATOM_CUT:
	.word	0                               # 0x0
	.size	ATOM_CUT, 4

	.type	.L.str.18,@object               # @.str.18
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.18:
	.asciz	"+"
	.size	.L.str.18, 2

	.type	ATOM_PLUS,@object               # @ATOM_PLUS
	.bss
	.globl	ATOM_PLUS
	.p2align	2, 0x0
ATOM_PLUS:
	.word	0                               # 0x0
	.size	ATOM_PLUS, 4

	.type	.L.str.19,@object               # @.str.19
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.19:
	.asciz	"-"
	.size	.L.str.19, 2

	.type	ATOM_MINUS,@object              # @ATOM_MINUS
	.bss
	.globl	ATOM_MINUS
	.p2align	2, 0x0
ATOM_MINUS:
	.word	0                               # 0x0
	.size	ATOM_MINUS, 4

	.type	.L.str.20,@object               # @.str.20
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.20:
	.asciz	"*"
	.size	.L.str.20, 2

	.type	ATOM_STAR,@object               # @ATOM_STAR
	.bss
	.globl	ATOM_STAR
	.p2align	2, 0x0
ATOM_STAR:
	.word	0                               # 0x0
	.size	ATOM_STAR, 4

	.type	.L.str.21,@object               # @.str.21
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.21:
	.asciz	"//"
	.size	.L.str.21, 3

	.type	ATOM_SLASH2,@object             # @ATOM_SLASH2
	.bss
	.globl	ATOM_SLASH2
	.p2align	2, 0x0
ATOM_SLASH2:
	.word	0                               # 0x0
	.size	ATOM_SLASH2, 4

	.type	.L.str.22,@object               # @.str.22
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.22:
	.asciz	"mod"
	.size	.L.str.22, 4

	.type	ATOM_MOD,@object                # @ATOM_MOD
	.bss
	.globl	ATOM_MOD
	.p2align	2, 0x0
ATOM_MOD:
	.word	0                               # 0x0
	.size	ATOM_MOD, 4

	.type	.L.str.23,@object               # @.str.23
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.23:
	.asciz	"abs"
	.size	.L.str.23, 4

	.type	ATOM_ABS,@object                # @ATOM_ABS
	.bss
	.globl	ATOM_ABS
	.p2align	2, 0x0
ATOM_ABS:
	.word	0                               # 0x0
	.size	ATOM_ABS, 4

	.type	.L.str.24,@object               # @.str.24
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.24:
	.asciz	"min"
	.size	.L.str.24, 4

	.type	ATOM_MIN,@object                # @ATOM_MIN
	.bss
	.globl	ATOM_MIN
	.p2align	2, 0x0
ATOM_MIN:
	.word	0                               # 0x0
	.size	ATOM_MIN, 4

	.type	.L.str.25,@object               # @.str.25
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.25:
	.asciz	"max"
	.size	.L.str.25, 4

	.type	ATOM_MAX,@object                # @ATOM_MAX
	.bss
	.globl	ATOM_MAX
	.p2align	2, 0x0
ATOM_MAX:
	.word	0                               # 0x0
	.size	ATOM_MAX, 4

	.type	.L.str.26,@object               # @.str.26
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.26:
	.asciz	"<"
	.size	.L.str.26, 2

	.type	ATOM_LT,@object                 # @ATOM_LT
	.bss
	.globl	ATOM_LT
	.p2align	2, 0x0
ATOM_LT:
	.word	0                               # 0x0
	.size	ATOM_LT, 4

	.type	.L.str.27,@object               # @.str.27
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.27:
	.asciz	">"
	.size	.L.str.27, 2

	.type	ATOM_GT,@object                 # @ATOM_GT
	.bss
	.globl	ATOM_GT
	.p2align	2, 0x0
ATOM_GT:
	.word	0                               # 0x0
	.size	ATOM_GT, 4

	.type	.L.str.28,@object               # @.str.28
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.28:
	.asciz	"=<"
	.size	.L.str.28, 3

	.type	ATOM_LE,@object                 # @ATOM_LE
	.bss
	.globl	ATOM_LE
	.p2align	2, 0x0
ATOM_LE:
	.word	0                               # 0x0
	.size	ATOM_LE, 4

	.type	.L.str.29,@object               # @.str.29
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.29:
	.asciz	">="
	.size	.L.str.29, 3

	.type	ATOM_GE,@object                 # @ATOM_GE
	.bss
	.globl	ATOM_GE
	.p2align	2, 0x0
ATOM_GE:
	.word	0                               # 0x0
	.size	ATOM_GE, 4

	.type	.L.str.30,@object               # @.str.30
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.30:
	.asciz	"=:="
	.size	.L.str.30, 4

	.type	ATOM_ARITH_EQ,@object           # @ATOM_ARITH_EQ
	.bss
	.globl	ATOM_ARITH_EQ
	.p2align	2, 0x0
ATOM_ARITH_EQ:
	.word	0                               # 0x0
	.size	ATOM_ARITH_EQ, 4

	.type	.L.str.31,@object               # @.str.31
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.31:
	.asciz	"=\\="
	.size	.L.str.31, 4

	.type	ATOM_ARITH_NEQ,@object          # @ATOM_ARITH_NEQ
	.bss
	.globl	ATOM_ARITH_NEQ
	.p2align	2, 0x0
ATOM_ARITH_NEQ:
	.word	0                               # 0x0
	.size	ATOM_ARITH_NEQ, 4

	.type	.L.str.32,@object               # @.str.32
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.32:
	.asciz	"write"
	.size	.L.str.32, 6

	.type	ATOM_WRITE,@object              # @ATOM_WRITE
	.bss
	.globl	ATOM_WRITE
	.p2align	2, 0x0
ATOM_WRITE:
	.word	0                               # 0x0
	.size	ATOM_WRITE, 4

	.type	.L.str.33,@object               # @.str.33
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.33:
	.asciz	"writeln"
	.size	.L.str.33, 8

	.type	ATOM_WRITELN,@object            # @ATOM_WRITELN
	.bss
	.globl	ATOM_WRITELN
	.p2align	2, 0x0
ATOM_WRITELN:
	.word	0                               # 0x0
	.size	ATOM_WRITELN, 4

	.type	.L.str.34,@object               # @.str.34
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.34:
	.asciz	"nl"
	.size	.L.str.34, 3

	.type	ATOM_NL,@object                 # @ATOM_NL
	.bss
	.globl	ATOM_NL
	.p2align	2, 0x0
ATOM_NL:
	.word	0                               # 0x0
	.size	ATOM_NL, 4

	.type	.L.str.35,@object               # @.str.35
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.35:
	.asciz	"halt"
	.size	.L.str.35, 5

	.type	ATOM_HALT,@object               # @ATOM_HALT
	.bss
	.globl	ATOM_HALT
	.p2align	2, 0x0
ATOM_HALT:
	.word	0                               # 0x0
	.size	ATOM_HALT, 4

	.type	.L.str.36,@object               # @.str.36
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.36:
	.asciz	"read"
	.size	.L.str.36, 5

	.type	ATOM_READ,@object               # @ATOM_READ
	.bss
	.globl	ATOM_READ
	.p2align	2, 0x0
ATOM_READ:
	.word	0                               # 0x0
	.size	ATOM_READ, 4

	.type	.L.str.37,@object               # @.str.37
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.37:
	.asciz	"assert"
	.size	.L.str.37, 7

	.type	ATOM_ASSERT,@object             # @ATOM_ASSERT
	.bss
	.globl	ATOM_ASSERT
	.p2align	2, 0x0
ATOM_ASSERT:
	.word	0                               # 0x0
	.size	ATOM_ASSERT, 4

	.type	.L.str.38,@object               # @.str.38
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.38:
	.asciz	"asserta"
	.size	.L.str.38, 8

	.type	ATOM_ASSERTA,@object            # @ATOM_ASSERTA
	.bss
	.globl	ATOM_ASSERTA
	.p2align	2, 0x0
ATOM_ASSERTA:
	.word	0                               # 0x0
	.size	ATOM_ASSERTA, 4

	.type	.L.str.39,@object               # @.str.39
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.39:
	.asciz	"assertz"
	.size	.L.str.39, 8

	.type	ATOM_ASSERTZ,@object            # @ATOM_ASSERTZ
	.bss
	.globl	ATOM_ASSERTZ
	.p2align	2, 0x0
ATOM_ASSERTZ:
	.word	0                               # 0x0
	.size	ATOM_ASSERTZ, 4

	.type	.L.str.40,@object               # @.str.40
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.40:
	.asciz	"retract"
	.size	.L.str.40, 8

	.type	ATOM_RETRACT,@object            # @ATOM_RETRACT
	.bss
	.globl	ATOM_RETRACT
	.p2align	2, 0x0
ATOM_RETRACT:
	.word	0                               # 0x0
	.size	ATOM_RETRACT, 4

	.type	.L.str.41,@object               # @.str.41
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.41:
	.asciz	"findall"
	.size	.L.str.41, 8

	.type	ATOM_FINDALL,@object            # @ATOM_FINDALL
	.bss
	.globl	ATOM_FINDALL
	.p2align	2, 0x0
ATOM_FINDALL:
	.word	0                               # 0x0
	.size	ATOM_FINDALL, 4

	.type	.L.str.42,@object               # @.str.42
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.42:
	.asciz	"atom"
	.size	.L.str.42, 5

	.type	ATOM_ATOM,@object               # @ATOM_ATOM
	.bss
	.globl	ATOM_ATOM
	.p2align	2, 0x0
ATOM_ATOM:
	.word	0                               # 0x0
	.size	ATOM_ATOM, 4

	.type	.L.str.43,@object               # @.str.43
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.43:
	.asciz	"integer"
	.size	.L.str.43, 8

	.type	ATOM_INTEGER,@object            # @ATOM_INTEGER
	.bss
	.globl	ATOM_INTEGER
	.p2align	2, 0x0
ATOM_INTEGER:
	.word	0                               # 0x0
	.size	ATOM_INTEGER, 4

	.type	.L.str.44,@object               # @.str.44
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.44:
	.asciz	"var"
	.size	.L.str.44, 4

	.type	ATOM_VAR,@object                # @ATOM_VAR
	.bss
	.globl	ATOM_VAR
	.p2align	2, 0x0
ATOM_VAR:
	.word	0                               # 0x0
	.size	ATOM_VAR, 4

	.type	.L.str.45,@object               # @.str.45
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.45:
	.asciz	"nonvar"
	.size	.L.str.45, 7

	.type	ATOM_NONVAR,@object             # @ATOM_NONVAR
	.bss
	.globl	ATOM_NONVAR
	.p2align	2, 0x0
ATOM_NONVAR:
	.word	0                               # 0x0
	.size	ATOM_NONVAR, 4

	.type	.L.str.46,@object               # @.str.46
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.46:
	.asciz	"compound"
	.size	.L.str.46, 9

	.type	ATOM_COMPOUND,@object           # @ATOM_COMPOUND
	.bss
	.globl	ATOM_COMPOUND
	.p2align	2, 0x0
ATOM_COMPOUND:
	.word	0                               # 0x0
	.size	ATOM_COMPOUND, 4

	.type	.L.str.47,@object               # @.str.47
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.47:
	.asciz	"number"
	.size	.L.str.47, 7

	.type	ATOM_NUMBER,@object             # @ATOM_NUMBER
	.bss
	.globl	ATOM_NUMBER
	.p2align	2, 0x0
ATOM_NUMBER:
	.word	0                               # 0x0
	.size	ATOM_NUMBER, 4

	.type	.L.str.48,@object               # @.str.48
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.48:
	.asciz	"is_list"
	.size	.L.str.48, 8

	.type	ATOM_IS_LIST,@object            # @ATOM_IS_LIST
	.bss
	.globl	ATOM_IS_LIST
	.p2align	2, 0x0
ATOM_IS_LIST:
	.word	0                               # 0x0
	.size	ATOM_IS_LIST, 4

	.type	.L.str.49,@object               # @.str.49
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.49:
	.asciz	"functor"
	.size	.L.str.49, 8

	.type	ATOM_FUNCTOR,@object            # @ATOM_FUNCTOR
	.bss
	.globl	ATOM_FUNCTOR
	.p2align	2, 0x0
ATOM_FUNCTOR:
	.word	0                               # 0x0
	.size	ATOM_FUNCTOR, 4

	.type	.L.str.50,@object               # @.str.50
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.50:
	.asciz	"arg"
	.size	.L.str.50, 4

	.type	ATOM_ARG,@object                # @ATOM_ARG
	.bss
	.globl	ATOM_ARG
	.p2align	2, 0x0
ATOM_ARG:
	.word	0                               # 0x0
	.size	ATOM_ARG, 4

	.type	.L.str.51,@object               # @.str.51
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.51:
	.asciz	"=.."
	.size	.L.str.51, 4

	.type	ATOM_UNIV,@object               # @ATOM_UNIV
	.bss
	.globl	ATOM_UNIV
	.p2align	2, 0x0
ATOM_UNIV:
	.word	0                               # 0x0
	.size	ATOM_UNIV, 4

	.type	.L.str.52,@object               # @.str.52
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.52:
	.asciz	"copy_term"
	.size	.L.str.52, 10

	.type	ATOM_COPY_TERM,@object          # @ATOM_COPY_TERM
	.bss
	.globl	ATOM_COPY_TERM
	.p2align	2, 0x0
ATOM_COPY_TERM:
	.word	0                               # 0x0
	.size	ATOM_COPY_TERM, 4

	.type	.L.str.53,@object               # @.str.53
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.53:
	.asciz	"atom_length"
	.size	.L.str.53, 12

	.type	ATOM_ATOM_LENGTH,@object        # @ATOM_ATOM_LENGTH
	.bss
	.globl	ATOM_ATOM_LENGTH
	.p2align	2, 0x0
ATOM_ATOM_LENGTH:
	.word	0                               # 0x0
	.size	ATOM_ATOM_LENGTH, 4

	.type	.L.str.54,@object               # @.str.54
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.54:
	.asciz	"atom_chars"
	.size	.L.str.54, 11

	.type	ATOM_ATOM_CHARS,@object         # @ATOM_ATOM_CHARS
	.bss
	.globl	ATOM_ATOM_CHARS
	.p2align	2, 0x0
ATOM_ATOM_CHARS:
	.word	0                               # 0x0
	.size	ATOM_ATOM_CHARS, 4

	.type	.L.str.55,@object               # @.str.55
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.55:
	.asciz	"char_code"
	.size	.L.str.55, 10

	.type	ATOM_CHAR_CODE,@object          # @ATOM_CHAR_CODE
	.bss
	.globl	ATOM_CHAR_CODE
	.p2align	2, 0x0
ATOM_CHAR_CODE:
	.word	0                               # 0x0
	.size	ATOM_CHAR_CODE, 4

	.type	.L.str.56,@object               # @.str.56
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.56:
	.asciz	"number_chars"
	.size	.L.str.56, 13

	.type	ATOM_NUMBER_CHARS,@object       # @ATOM_NUMBER_CHARS
	.bss
	.globl	ATOM_NUMBER_CHARS
	.p2align	2, 0x0
ATOM_NUMBER_CHARS:
	.word	0                               # 0x0
	.size	ATOM_NUMBER_CHARS, 4

	.type	.L.str.57,@object               # @.str.57
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.57:
	.asciz	"atom_concat"
	.size	.L.str.57, 12

	.type	ATOM_ATOM_CONCAT,@object        # @ATOM_ATOM_CONCAT
	.bss
	.globl	ATOM_ATOM_CONCAT
	.p2align	2, 0x0
ATOM_ATOM_CONCAT:
	.word	0                               # 0x0
	.size	ATOM_ATOM_CONCAT, 4

	.type	.L.str.58,@object               # @.str.58
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.58:
	.asciz	"write_canonical"
	.size	.L.str.58, 16

	.type	ATOM_WRITE_CANONICAL,@object    # @ATOM_WRITE_CANONICAL
	.bss
	.globl	ATOM_WRITE_CANONICAL
	.p2align	2, 0x0
ATOM_WRITE_CANONICAL:
	.word	0                               # 0x0
	.size	ATOM_WRITE_CANONICAL, 4

	.type	.L.str.59,@object               # @.str.59
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.59:
	.asciz	"succ"
	.size	.L.str.59, 5

	.type	ATOM_SUCC,@object               # @ATOM_SUCC
	.bss
	.globl	ATOM_SUCC
	.p2align	2, 0x0
ATOM_SUCC:
	.word	0                               # 0x0
	.size	ATOM_SUCC, 4

	.type	.L.str.60,@object               # @.str.60
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.60:
	.asciz	"plus"
	.size	.L.str.60, 5

	.type	ATOM_PLUS2,@object              # @ATOM_PLUS2
	.bss
	.globl	ATOM_PLUS2
	.p2align	2, 0x0
ATOM_PLUS2:
	.word	0                               # 0x0
	.size	ATOM_PLUS2, 4

	.type	.L.str.61,@object               # @.str.61
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.61:
	.asciz	"length"
	.size	.L.str.61, 7

	.type	ATOM_LENGTH,@object             # @ATOM_LENGTH
	.bss
	.globl	ATOM_LENGTH
	.p2align	2, 0x0
ATOM_LENGTH:
	.word	0                               # 0x0
	.size	ATOM_LENGTH, 4

	.type	.L.str.62,@object               # @.str.62
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.62:
	.asciz	"append"
	.size	.L.str.62, 7

	.type	ATOM_APPEND,@object             # @ATOM_APPEND
	.bss
	.globl	ATOM_APPEND
	.p2align	2, 0x0
ATOM_APPEND:
	.word	0                               # 0x0
	.size	ATOM_APPEND, 4

	.type	.L.str.63,@object               # @.str.63
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.63:
	.asciz	"last"
	.size	.L.str.63, 5

	.type	ATOM_LAST,@object               # @ATOM_LAST
	.bss
	.globl	ATOM_LAST
	.p2align	2, 0x0
ATOM_LAST:
	.word	0                               # 0x0
	.size	ATOM_LAST, 4

	.type	.L.str.64,@object               # @.str.64
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.64:
	.asciz	"between"
	.size	.L.str.64, 8

	.type	ATOM_BETWEEN,@object            # @ATOM_BETWEEN
	.bss
	.globl	ATOM_BETWEEN
	.p2align	2, 0x0
ATOM_BETWEEN:
	.word	0                               # 0x0
	.size	ATOM_BETWEEN, 4

	.type	.L.str.65,@object               # @.str.65
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.65:
	.asciz	"heap overflow"
	.size	.L.str.65, 14

	.type	.L.str.66,@object               # @.str.66
.L.str.66:
	.asciz	"code heap overflow"
	.size	.L.str.66, 19

	.type	heap,@object                    # @heap
	.bss
	.globl	heap
	.p2align	2, 0x0
heap:
	.zero	1048576
	.size	heap, 1048576

	.type	code_heap,@object               # @code_heap
	.globl	code_heap
	.p2align	2, 0x0
code_heap:
	.zero	524288
	.size	code_heap, 524288

	.type	.L.str.67,@object               # @.str.67
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.67:
	.asciz	"atom store full"
	.size	.L.str.67, 16

	.type	atom_store,@object              # @atom_store
	.local	atom_store
	.comm	atom_store,32768,1
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
