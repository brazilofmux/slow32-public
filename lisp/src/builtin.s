	.file	"builtin.c"
	.text
	.globl	builtins_register               # -- Begin function builtins_register
	.p2align	2
	.type	builtins_register,@function
builtins_register:                      # @builtins_register
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
	add r13, r3, r0
	addi r11, fp, -28
	stw r11+0, r3
	lui r14, %hi(.L.str)
	addi r14, r14, %lo(.L.str)
	add r3, r14, r0
	jal r31, symbol_intern
	addi r12, fp, -32
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_add)
	addi r4, r4, %lo(bi_add)
	add r3, r14, r0
	jal r31, builtin_alloc
	lui r15, %hi(root_sp)
	addi r15, r15, %lo(root_sp)
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.1)
	addi r14, r14, %lo(.L.str.1)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_sub)
	addi r4, r4, %lo(bi_sub)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.2)
	addi r14, r14, %lo(.L.str.2)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_mul)
	addi r4, r4, %lo(bi_mul)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.3)
	addi r14, r14, %lo(.L.str.3)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_div)
	addi r4, r4, %lo(bi_div)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.4)
	addi r14, r14, %lo(.L.str.4)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_modulo)
	addi r4, r4, %lo(bi_modulo)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.5)
	addi r14, r14, %lo(.L.str.5)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_abs)
	addi r4, r4, %lo(bi_abs)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.6)
	addi r14, r14, %lo(.L.str.6)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_min)
	addi r4, r4, %lo(bi_min)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.7)
	addi r14, r14, %lo(.L.str.7)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_max)
	addi r4, r4, %lo(bi_max)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.8)
	addi r14, r14, %lo(.L.str.8)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_num_eq)
	addi r4, r4, %lo(bi_num_eq)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.9)
	addi r14, r14, %lo(.L.str.9)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_lt)
	addi r4, r4, %lo(bi_lt)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.10)
	addi r14, r14, %lo(.L.str.10)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_gt)
	addi r4, r4, %lo(bi_gt)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.11)
	addi r14, r14, %lo(.L.str.11)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_le)
	addi r4, r4, %lo(bi_le)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.12)
	addi r14, r14, %lo(.L.str.12)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_ge)
	addi r4, r4, %lo(bi_ge)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.13)
	addi r14, r14, %lo(.L.str.13)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_null)
	addi r4, r4, %lo(bi_null)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.14)
	addi r14, r14, %lo(.L.str.14)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_pair)
	addi r4, r4, %lo(bi_pair)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.15)
	addi r14, r14, %lo(.L.str.15)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_number)
	addi r4, r4, %lo(bi_number)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.16)
	addi r14, r14, %lo(.L.str.16)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_symbol)
	addi r4, r4, %lo(bi_symbol)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.17)
	addi r14, r14, %lo(.L.str.17)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_string_p)
	addi r4, r4, %lo(bi_string_p)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.18)
	addi r14, r14, %lo(.L.str.18)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_procedure)
	addi r4, r4, %lo(bi_procedure)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.19)
	addi r14, r14, %lo(.L.str.19)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_zero)
	addi r4, r4, %lo(bi_zero)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.20)
	addi r14, r14, %lo(.L.str.20)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_positive)
	addi r4, r4, %lo(bi_positive)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.21)
	addi r14, r14, %lo(.L.str.21)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_negative)
	addi r4, r4, %lo(bi_negative)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.22)
	addi r14, r14, %lo(.L.str.22)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_not)
	addi r4, r4, %lo(bi_not)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.23)
	addi r14, r14, %lo(.L.str.23)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_boolean)
	addi r4, r4, %lo(bi_boolean)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.24)
	addi r14, r14, %lo(.L.str.24)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_eq)
	addi r4, r4, %lo(bi_eq)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.25)
	addi r14, r14, %lo(.L.str.25)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_equal)
	addi r4, r4, %lo(bi_equal)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.26)
	addi r14, r14, %lo(.L.str.26)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_cons)
	addi r4, r4, %lo(bi_cons)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.27)
	addi r14, r14, %lo(.L.str.27)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_car)
	addi r4, r4, %lo(bi_car)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.28)
	addi r14, r14, %lo(.L.str.28)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_cdr)
	addi r4, r4, %lo(bi_cdr)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.29)
	addi r14, r14, %lo(.L.str.29)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_set_car)
	addi r4, r4, %lo(bi_set_car)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.30)
	addi r14, r14, %lo(.L.str.30)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_set_cdr)
	addi r4, r4, %lo(bi_set_cdr)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.31)
	addi r14, r14, %lo(.L.str.31)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_list)
	addi r4, r4, %lo(bi_list)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.32)
	addi r14, r14, %lo(.L.str.32)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_length)
	addi r4, r4, %lo(bi_length)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.33)
	addi r14, r14, %lo(.L.str.33)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_append)
	addi r4, r4, %lo(bi_append)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.34)
	addi r14, r14, %lo(.L.str.34)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_reverse)
	addi r4, r4, %lo(bi_reverse)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.35)
	addi r14, r14, %lo(.L.str.35)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_map)
	addi r4, r4, %lo(bi_map)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.36)
	addi r14, r14, %lo(.L.str.36)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_string_length)
	addi r4, r4, %lo(bi_string_length)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.37)
	addi r14, r14, %lo(.L.str.37)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_string_append)
	addi r4, r4, %lo(bi_string_append)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.38)
	addi r14, r14, %lo(.L.str.38)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_substring)
	addi r4, r4, %lo(bi_substring)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.39)
	addi r14, r14, %lo(.L.str.39)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_string_to_number)
	addi r4, r4, %lo(bi_string_to_number)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.40)
	addi r14, r14, %lo(.L.str.40)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_number_to_string)
	addi r4, r4, %lo(bi_number_to_string)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.41)
	addi r14, r14, %lo(.L.str.41)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_display)
	addi r4, r4, %lo(bi_display)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.42)
	addi r14, r14, %lo(.L.str.42)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_newline)
	addi r4, r4, %lo(bi_newline)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.43)
	addi r14, r14, %lo(.L.str.43)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_read)
	addi r4, r4, %lo(bi_read)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r14, %hi(.L.str.44)
	addi r14, r14, %lo(.L.str.44)
	add r3, r14, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_apply)
	addi r4, r4, %lo(bi_apply)
	add r3, r14, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
	stw r11+0, r13
	lui r13, %hi(.L.str.45)
	addi r13, r13, %lo(.L.str.45)
	add r3, r13, r0
	jal r31, symbol_intern
	stw r12+0, r1
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r11, r0
	jal r31, push_root_checked
	lui r4, %hi(bi_error)
	addi r4, r4, %lo(bi_error)
	add r3, r13, r0
	jal r31, builtin_alloc
	ldw r3, r15+0
	addi r3, r3, -2
	stw r15+0, r3
	ldw r3, r11+0
	ldw r4, r12+0
	add r5, r1, r0
	jal r31, env_define
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
.Lfunc_end0:
	.size	builtins_register, .Lfunc_end0-builtins_register
                                        # -- End function
	.p2align	2                               # -- Begin function bi_add
	.type	bi_add,@function
bi_add:                                 # @bi_add
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
	add r12, r3, r0
	addi r11, r0, 0
	lui r13, %hi(.L.str.46)
	addi r13, r13, %lo(.L.str.46)
	add r14, r11, r0
	jal r0, .LBB1_2
.LBB1_1:
	add r3, r13, r0
	jal r31, lisp_error
	beq r15, r11, .LBB1_6
.LBB1_2:
	beq r12, r11, .LBB1_5
.LBB1_3:
	ldw r1, r12+12
	andi r15, r1, 1
	beq r15, r11, .LBB1_1
.LBB1_4:
	srai r1, r1, 1
	add r14, r1, r14
	ldw r12, r12+16
	bne r15, r11, .LBB1_2
	jal r0, .LBB1_6
.LBB1_5:
	slli r1, r14, 1
	addi r11, r1, 1
.LBB1_6:
	add r1, r11, r0
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
.Lfunc_end1:
	.size	bi_add, .Lfunc_end1-bi_add
                                        # -- End function
	.p2align	2                               # -- Begin function bi_sub
	.type	bi_sub,@function
bi_sub:                                 # @bi_sub
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
	addi r11, r0, 0
	beq r3, r11, .LBB2_3
.LBB2_1:
	ldw r1, r3+12
	andi r4, r1, 1
	bne r4, r11, .LBB2_4
.LBB2_2:
	lui r3, %hi(.L.str.48)
	addi r3, r3, %lo(.L.str.48)
	jal r31, lisp_error
	jal r0, .LBB2_12
.LBB2_3:
	lui r3, %hi(.L.str.47)
	addi r3, r3, %lo(.L.str.47)
	jal r31, lisp_error
	jal r0, .LBB2_12
.LBB2_4:
	ldw r13, r3+16
	beq r13, r11, .LBB2_10
.LBB2_5:
	srai r14, r1, 1
	lui r12, %hi(.L.str.48)
	addi r12, r12, %lo(.L.str.48)
	jal r0, .LBB2_7
.LBB2_6:
	add r3, r12, r0
	jal r31, lisp_error
	beq r15, r11, .LBB2_12
.LBB2_7:
	beq r13, r11, .LBB2_11
.LBB2_8:
	ldw r1, r13+12
	andi r15, r1, 1
	beq r15, r11, .LBB2_6
.LBB2_9:
	srai r1, r1, 1
	sub r14, r14, r1
	ldw r13, r13+16
	bne r15, r11, .LBB2_7
	jal r0, .LBB2_12
.LBB2_10:
	addi r3, r0, -2
	and r1, r1, r3
	addi r3, r0, 1
	sub r11, r3, r1
	jal r0, .LBB2_12
.LBB2_11:
	slli r1, r14, 1
	addi r11, r1, 1
.LBB2_12:
	add r1, r11, r0
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
.Lfunc_end2:
	.size	bi_sub, .Lfunc_end2-bi_sub
                                        # -- End function
	.p2align	2                               # -- Begin function bi_mul
	.type	bi_mul,@function
bi_mul:                                 # @bi_mul
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
	add r12, r3, r0
	addi r14, r0, 1
	addi r11, r0, 0
	lui r13, %hi(.L.str.49)
	addi r13, r13, %lo(.L.str.49)
	jal r0, .LBB3_2
.LBB3_1:
	add r3, r13, r0
	jal r31, lisp_error
	beq r15, r11, .LBB3_6
.LBB3_2:
	beq r12, r11, .LBB3_5
.LBB3_3:
	ldw r1, r12+12
	andi r15, r1, 1
	beq r15, r11, .LBB3_1
.LBB3_4:
	srai r1, r1, 1
	mul r14, r1, r14
	ldw r12, r12+16
	bne r15, r11, .LBB3_2
	jal r0, .LBB3_6
.LBB3_5:
	slli r1, r14, 1
	addi r11, r1, 1
.LBB3_6:
	add r1, r11, r0
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
.Lfunc_end3:
	.size	bi_mul, .Lfunc_end3-bi_mul
                                        # -- End function
	.p2align	2                               # -- Begin function bi_div
	.type	bi_div,@function
bi_div:                                 # @bi_div
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
	addi r11, r0, 0
	beq r3, r11, .LBB4_6
.LBB4_1:
	addi r1, r0, 1
	add r4, r3, r0
.LBB4_2:
	ldw r4, r4+16
	addi r1, r1, -1
	bne r4, r11, .LBB4_2
.LBB4_3:
	beq r1, r11, .LBB4_6
.LBB4_4:
	ldw r1, r3+12
	andi r4, r1, 1
	bne r4, r11, .LBB4_9
.LBB4_5:
	lui r3, %hi(.L.str.51)
	addi r3, r3, %lo(.L.str.51)
	jal r0, .LBB4_7
.LBB4_6:
	lui r3, %hi(.L.str.50)
	addi r3, r3, %lo(.L.str.50)
.LBB4_7:
	jal r31, lisp_error
.LBB4_8:
	add r1, r11, r0
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
.LBB4_9:
	srai r13, r1, 1
	ldw r14, r3+16
	lui r12, %hi(.L.str.52)
	addi r12, r12, %lo(.L.str.52)
	jal r0, .LBB4_11
.LBB4_10:
	add r3, r12, r0
	jal r31, lisp_error
	beq r15, r11, .LBB4_8
.LBB4_11:
	beq r14, r11, .LBB4_15
.LBB4_12:
	ldw r1, r14+12
	andi r3, r1, 1
	beq r3, r11, .LBB4_5
.LBB4_13:
	srai r15, r1, 1
	beq r15, r11, .LBB4_10
.LBB4_14:
	div r13, r13, r15
	ldw r14, r14+16
	bne r15, r11, .LBB4_11
	jal r0, .LBB4_8
.LBB4_15:
	slli r1, r13, 1
	addi r11, r1, 1
	jal r0, .LBB4_8
.Lfunc_end4:
	.size	bi_div, .Lfunc_end4-bi_div
                                        # -- End function
	.p2align	2                               # -- Begin function bi_modulo
	.type	bi_modulo,@function
bi_modulo:                              # @bi_modulo
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	addi r11, r0, 0
	beq r3, r11, .LBB5_4
.LBB5_1:
	addi r1, r0, 2
	add r4, r3, r0
.LBB5_2:
	ldw r4, r4+16
	addi r1, r1, -1
	bne r4, r11, .LBB5_2
.LBB5_3:
	beq r1, r11, .LBB5_7
.LBB5_4:
	lui r3, %hi(.L.str.53)
	addi r3, r3, %lo(.L.str.53)
.LBB5_5:
	jal r31, lisp_error
.LBB5_6:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB5_7:
	ldw r1, r3+12
	andi r4, r1, 1
	beq r4, r11, .LBB5_11
.LBB5_8:
	ldw r3, r3+16
	ldw r3, r3+12
	andi r4, r3, 1
	beq r4, r11, .LBB5_11
.LBB5_9:
	srai r3, r3, 1
	beq r3, r11, .LBB5_12
.LBB5_10:
	srai r1, r1, 1
	rem r1, r1, r3
	seq r4, r1, r11
	xor r5, r1, r3
	srai r5, r5, 31
	and r3, r5, r3
	add r1, r3, r1
	slli r1, r1, 1
	addi r3, r1, 1
	sub r4, r11, r4
	and r1, r1, r4
	xor r11, r3, r1
	jal r0, .LBB5_6
.LBB5_11:
	lui r3, %hi(.L.str.54)
	addi r3, r3, %lo(.L.str.54)
	jal r0, .LBB5_5
.LBB5_12:
	lui r3, %hi(.L.str.55)
	addi r3, r3, %lo(.L.str.55)
	jal r0, .LBB5_5
.Lfunc_end5:
	.size	bi_modulo, .Lfunc_end5-bi_modulo
                                        # -- End function
	.p2align	2                               # -- Begin function bi_abs
	.type	bi_abs,@function
bi_abs:                                 # @bi_abs
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	andi r3, r1, 1
	addi r11, r0, 0
	bne r3, r11, .LBB6_2
.LBB6_1:
	lui r3, %hi(.L.str.56)
	addi r3, r3, %lo(.L.str.56)
	jal r31, lisp_error
	jal r0, .LBB6_3
.LBB6_2:
	srli r3, r1, 1
	srai r1, r1, 31
	xor r3, r3, r1
	sub r1, r3, r1
	slli r1, r1, 1
	ori  r11, r1, 1
.LBB6_3:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	bi_abs, .Lfunc_end6-bi_abs
                                        # -- End function
	.p2align	2                               # -- Begin function bi_min
	.type	bi_min,@function
bi_min:                                 # @bi_min
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	addi r11, r0, 0
	beq r3, r11, .LBB7_3
.LBB7_1:
	ldw r1, r3+12
	andi r4, r1, 1
	bne r4, r11, .LBB7_4
.LBB7_2:
	lui r3, %hi(.L.str.58)
	addi r3, r3, %lo(.L.str.58)
	jal r31, lisp_error
	jal r0, .LBB7_8
.LBB7_3:
	lui r3, %hi(.L.str.57)
	addi r3, r3, %lo(.L.str.57)
	jal r31, lisp_error
	jal r0, .LBB7_8
.LBB7_4:
	srai r1, r1, 1
	ldw r3, r3+16
	beq r3, r11, .LBB7_7
.LBB7_5:
	ldw r4, r3+12
	andi r5, r4, 1
	beq r5, r11, .LBB7_2
.LBB7_6:
	srai r4, r4, 1
	xor r5, r4, r1
	slt r4, r4, r1
	sub r4, r11, r4
	and r4, r5, r4
	xor r1, r1, r4
	ldw r3, r3+16
	bne r3, r11, .LBB7_5
.LBB7_7:
	slli r1, r1, 1
	addi r11, r1, 1
.LBB7_8:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end7:
	.size	bi_min, .Lfunc_end7-bi_min
                                        # -- End function
	.p2align	2                               # -- Begin function bi_max
	.type	bi_max,@function
bi_max:                                 # @bi_max
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	addi r11, r0, 0
	beq r3, r11, .LBB8_3
.LBB8_1:
	ldw r1, r3+12
	andi r4, r1, 1
	bne r4, r11, .LBB8_4
.LBB8_2:
	lui r3, %hi(.L.str.60)
	addi r3, r3, %lo(.L.str.60)
	jal r31, lisp_error
	jal r0, .LBB8_8
.LBB8_3:
	lui r3, %hi(.L.str.59)
	addi r3, r3, %lo(.L.str.59)
	jal r31, lisp_error
	jal r0, .LBB8_8
.LBB8_4:
	srai r1, r1, 1
	ldw r3, r3+16
	beq r3, r11, .LBB8_7
.LBB8_5:
	ldw r4, r3+12
	andi r5, r4, 1
	beq r5, r11, .LBB8_2
.LBB8_6:
	srai r4, r4, 1
	xor r5, r4, r1
	sgt r4, r4, r1
	sub r4, r11, r4
	and r4, r5, r4
	xor r1, r1, r4
	ldw r3, r3+16
	bne r3, r11, .LBB8_5
.LBB8_7:
	slli r1, r1, 1
	addi r11, r1, 1
.LBB8_8:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	bi_max, .Lfunc_end8-bi_max
                                        # -- End function
	.p2align	2                               # -- Begin function bi_num_eq
	.type	bi_num_eq,@function
bi_num_eq:                              # @bi_num_eq
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	andi r4, r1, 1
	addi r11, r0, 0
	beq r4, r11, .LBB9_3
.LBB9_1:
	ldw r3, r3+16
	ldw r3, r3+12
	andi r4, r3, 1
	beq r4, r11, .LBB9_3
.LBB9_2:
	xor r1, r3, r1
	addi r3, r0, 2
	sltu r1, r1, r3
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	sub r1, r11, r1
	and r11, r3, r1
	jal r0, .LBB9_4
.LBB9_3:
	lui r3, %hi(.L.str.61)
	addi r3, r3, %lo(.L.str.61)
	jal r31, lisp_error
.LBB9_4:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end9:
	.size	bi_num_eq, .Lfunc_end9-bi_num_eq
                                        # -- End function
	.p2align	2                               # -- Begin function bi_lt
	.type	bi_lt,@function
bi_lt:                                  # @bi_lt
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	andi r4, r1, 1
	addi r11, r0, 0
	beq r4, r11, .LBB10_3
.LBB10_1:
	ldw r3, r3+16
	ldw r3, r3+12
	andi r4, r3, 1
	beq r4, r11, .LBB10_3
.LBB10_2:
	srai r1, r1, 1
	srai r3, r3, 1
	slt r1, r1, r3
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	sub r1, r11, r1
	and r11, r3, r1
	jal r0, .LBB10_4
.LBB10_3:
	lui r3, %hi(.L.str.62)
	addi r3, r3, %lo(.L.str.62)
	jal r31, lisp_error
.LBB10_4:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end10:
	.size	bi_lt, .Lfunc_end10-bi_lt
                                        # -- End function
	.p2align	2                               # -- Begin function bi_gt
	.type	bi_gt,@function
bi_gt:                                  # @bi_gt
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	andi r4, r1, 1
	addi r11, r0, 0
	beq r4, r11, .LBB11_3
.LBB11_1:
	ldw r3, r3+16
	ldw r3, r3+12
	andi r4, r3, 1
	beq r4, r11, .LBB11_3
.LBB11_2:
	srai r1, r1, 1
	srai r3, r3, 1
	sgt r1, r1, r3
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	sub r1, r11, r1
	and r11, r3, r1
	jal r0, .LBB11_4
.LBB11_3:
	lui r3, %hi(.L.str.63)
	addi r3, r3, %lo(.L.str.63)
	jal r31, lisp_error
.LBB11_4:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end11:
	.size	bi_gt, .Lfunc_end11-bi_gt
                                        # -- End function
	.p2align	2                               # -- Begin function bi_le
	.type	bi_le,@function
bi_le:                                  # @bi_le
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	andi r4, r1, 1
	addi r11, r0, 0
	beq r4, r11, .LBB12_3
.LBB12_1:
	ldw r3, r3+16
	ldw r3, r3+12
	andi r4, r3, 1
	beq r4, r11, .LBB12_3
.LBB12_2:
	srai r1, r1, 1
	srai r3, r3, 1
	sgt r1, r1, r3
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	sub r1, r11, r1
	and r1, r3, r1
	xor r11, r3, r1
	jal r0, .LBB12_4
.LBB12_3:
	lui r3, %hi(.L.str.64)
	addi r3, r3, %lo(.L.str.64)
	jal r31, lisp_error
.LBB12_4:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end12:
	.size	bi_le, .Lfunc_end12-bi_le
                                        # -- End function
	.p2align	2                               # -- Begin function bi_ge
	.type	bi_ge,@function
bi_ge:                                  # @bi_ge
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	andi r4, r1, 1
	addi r11, r0, 0
	beq r4, r11, .LBB13_3
.LBB13_1:
	ldw r3, r3+16
	ldw r3, r3+12
	andi r4, r3, 1
	beq r4, r11, .LBB13_3
.LBB13_2:
	srai r1, r1, 1
	srai r3, r3, 1
	slt r1, r1, r3
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	sub r1, r11, r1
	and r1, r3, r1
	xor r11, r3, r1
	jal r0, .LBB13_4
.LBB13_3:
	lui r3, %hi(.L.str.65)
	addi r3, r3, %lo(.L.str.65)
	jal r31, lisp_error
.LBB13_4:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end13:
	.size	bi_ge, .Lfunc_end13-bi_ge
                                        # -- End function
	.p2align	2                               # -- Begin function bi_null
	.type	bi_null,@function
bi_null:                                # @bi_null
# %bb.0:
	ldw r1, r3+12
	addi r3, r0, 0
	seq r1, r1, r3
	lui r4, %hi(sym_true)
	addi r4, r4, %lo(sym_true)
	ldw r4, r4+0
	sub r1, r3, r1
	and r1, r4, r1
	jalr r0, r31, 0
.Lfunc_end14:
	.size	bi_null, .Lfunc_end14-bi_null
                                        # -- End function
	.p2align	2                               # -- Begin function bi_pair
	.type	bi_pair,@function
bi_pair:                                # @bi_pair
# %bb.0:
	ldw r3, r3+12
	addi r1, r0, 0
	beq r3, r1, .LBB15_3
.LBB15_1:
	andi r5, r3, 1
	addi r4, r0, 0
	bne r5, r4, .LBB15_3
.LBB15_2:
	ldw r1, r3+0
	seq r1, r1, r4
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	sub r1, r4, r1
	and r1, r3, r1
.LBB15_3:
	jalr r0, r31, 0
.Lfunc_end15:
	.size	bi_pair, .Lfunc_end15-bi_pair
                                        # -- End function
	.p2align	2                               # -- Begin function bi_number
	.type	bi_number,@function
bi_number:                              # @bi_number
# %bb.0:
	ldw r1, r3+12
	andi r1, r1, 1
	addi r3, r0, 0
	sub r1, r3, r1
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	and r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end16:
	.size	bi_number, .Lfunc_end16-bi_number
                                        # -- End function
	.p2align	2                               # -- Begin function bi_symbol
	.type	bi_symbol,@function
bi_symbol:                              # @bi_symbol
# %bb.0:
	ldw r3, r3+12
	addi r1, r0, 0
	beq r3, r1, .LBB17_3
.LBB17_1:
	andi r5, r3, 1
	addi r4, r0, 0
	bne r5, r4, .LBB17_3
.LBB17_2:
	ldw r1, r3+0
	addi r3, r0, 1
	seq r1, r1, r3
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	sub r1, r4, r1
	and r1, r3, r1
.LBB17_3:
	jalr r0, r31, 0
.Lfunc_end17:
	.size	bi_symbol, .Lfunc_end17-bi_symbol
                                        # -- End function
	.p2align	2                               # -- Begin function bi_string_p
	.type	bi_string_p,@function
bi_string_p:                            # @bi_string_p
# %bb.0:
	ldw r3, r3+12
	addi r1, r0, 0
	beq r3, r1, .LBB18_3
.LBB18_1:
	andi r5, r3, 1
	addi r4, r0, 0
	bne r5, r4, .LBB18_3
.LBB18_2:
	ldw r1, r3+0
	addi r3, r0, 2
	seq r1, r1, r3
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	sub r1, r4, r1
	and r1, r3, r1
.LBB18_3:
	jalr r0, r31, 0
.Lfunc_end18:
	.size	bi_string_p, .Lfunc_end18-bi_string_p
                                        # -- End function
	.p2align	2                               # -- Begin function bi_procedure
	.type	bi_procedure,@function
bi_procedure:                           # @bi_procedure
# %bb.0:
	ldw r4, r3+12
	addi r1, r0, 0
	beq r4, r1, .LBB19_3
.LBB19_1:
	andi r5, r4, 1
	addi r3, r0, 0
	bne r5, r3, .LBB19_3
.LBB19_2:
	ldw r1, r4+0
	addi r1, r1, -3
	addi r4, r0, 2
	sltu r1, r1, r4
	lui r4, %hi(sym_true)
	addi r4, r4, %lo(sym_true)
	ldw r4, r4+0
	sub r1, r3, r1
	and r1, r4, r1
.LBB19_3:
	jalr r0, r31, 0
.Lfunc_end19:
	.size	bi_procedure, .Lfunc_end19-bi_procedure
                                        # -- End function
	.p2align	2                               # -- Begin function bi_zero
	.type	bi_zero,@function
bi_zero:                                # @bi_zero
# %bb.0:
	ldw r1, r3+12
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	addi r4, r0, 1
	seq r1, r1, r4
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	jalr r0, r31, 0
.Lfunc_end20:
	.size	bi_zero, .Lfunc_end20-bi_zero
                                        # -- End function
	.p2align	2                               # -- Begin function bi_positive
	.type	bi_positive,@function
bi_positive:                            # @bi_positive
# %bb.0:
	ldw r1, r3+12
	andi r3, r1, 1
	addi r4, r0, 0
	sub r3, r4, r3
	addi r5, r0, 2
	slt r1, r1, r5
	lui r5, %hi(sym_true)
	addi r5, r5, %lo(sym_true)
	ldw r5, r5+0
	sub r1, r4, r1
	and r3, r3, r5
	and r1, r3, r1
	xor r1, r3, r1
	jalr r0, r31, 0
.Lfunc_end21:
	.size	bi_positive, .Lfunc_end21-bi_positive
                                        # -- End function
	.p2align	2                               # -- Begin function bi_negative
	.type	bi_negative,@function
bi_negative:                            # @bi_negative
# %bb.0:
	ldw r1, r3+12
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	lui r4, 524288
	addi r4, r4, 1
	and r1, r1, r4
	seq r1, r1, r4
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	jalr r0, r31, 0
.Lfunc_end22:
	.size	bi_negative, .Lfunc_end22-bi_negative
                                        # -- End function
	.p2align	2                               # -- Begin function bi_not
	.type	bi_not,@function
bi_not:                                 # @bi_not
# %bb.0:
	ldw r1, r3+12
	addi r3, r0, 0
	seq r1, r1, r3
	lui r4, %hi(sym_true)
	addi r4, r4, %lo(sym_true)
	ldw r4, r4+0
	sub r1, r3, r1
	and r1, r4, r1
	jalr r0, r31, 0
.Lfunc_end23:
	.size	bi_not, .Lfunc_end23-bi_not
                                        # -- End function
	.p2align	2                               # -- Begin function bi_boolean
	.type	bi_boolean,@function
bi_boolean:                             # @bi_boolean
# %bb.0:
	ldw r1, r3+12
	addi r3, r0, 0
	seq r4, r1, r3
	lui r5, %hi(sym_true)
	addi r5, r5, %lo(sym_true)
	ldw r5, r5+0
	seq r1, r1, r5
	sub r4, r3, r4
	sub r1, r3, r1
	and r1, r5, r1
	xor r3, r5, r1
	and r3, r3, r4
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end24:
	.size	bi_boolean, .Lfunc_end24-bi_boolean
                                        # -- End function
	.p2align	2                               # -- Begin function bi_eq
	.type	bi_eq,@function
bi_eq:                                  # @bi_eq
# %bb.0:
	ldw r1, r3+12
	ldw r3, r3+16
	ldw r3, r3+12
	seq r1, r1, r3
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	addi r4, r0, 0
	sub r1, r4, r1
	and r1, r3, r1
	jalr r0, r31, 0
.Lfunc_end25:
	.size	bi_eq, .Lfunc_end25-bi_eq
                                        # -- End function
	.p2align	2                               # -- Begin function bi_equal
	.type	bi_equal,@function
bi_equal:                               # @bi_equal
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	ldw r3, r3+16
	ldw r4, r3+12
	addi r11, r0, 0
	add r3, r1, r0
	add r5, r11, r0
	jal r31, equal_deep
	seq r1, r1, r11
	lui r3, %hi(sym_true)
	addi r3, r3, %lo(sym_true)
	ldw r3, r3+0
	sub r1, r11, r1
	and r1, r3, r1
	xor r1, r3, r1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end26:
	.size	bi_equal, .Lfunc_end26-bi_equal
                                        # -- End function
	.p2align	2                               # -- Begin function bi_cons
	.type	bi_cons,@function
bi_cons:                                # @bi_cons
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r3+12
	ldw r3, r3+16
	ldw r4, r3+12
	add r3, r1, r0
	jal r31, cons_alloc
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end27:
	.size	bi_cons, .Lfunc_end27-bi_cons
                                        # -- End function
	.p2align	2                               # -- Begin function bi_car
	.type	bi_car,@function
bi_car:                                 # @bi_car
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	addi r11, r0, 0
	beq r1, r11, .LBB28_3
.LBB28_1:
	andi r4, r1, 1
	addi r3, r0, 0
	bne r4, r3, .LBB28_3
.LBB28_2:
	ldw r4, r1+0
	beq r4, r3, .LBB28_5
.LBB28_3:
	lui r3, %hi(.L.str.66)
	addi r3, r3, %lo(.L.str.66)
	jal r31, lisp_error
.LBB28_4:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB28_5:
	ldw r11, r1+12
	jal r0, .LBB28_4
.Lfunc_end28:
	.size	bi_car, .Lfunc_end28-bi_car
                                        # -- End function
	.p2align	2                               # -- Begin function bi_cdr
	.type	bi_cdr,@function
bi_cdr:                                 # @bi_cdr
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	addi r11, r0, 0
	beq r1, r11, .LBB29_3
.LBB29_1:
	andi r4, r1, 1
	addi r3, r0, 0
	bne r4, r3, .LBB29_3
.LBB29_2:
	ldw r4, r1+0
	beq r4, r3, .LBB29_5
.LBB29_3:
	lui r3, %hi(.L.str.67)
	addi r3, r3, %lo(.L.str.67)
	jal r31, lisp_error
.LBB29_4:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB29_5:
	ldw r11, r1+16
	jal r0, .LBB29_4
.Lfunc_end29:
	.size	bi_cdr, .Lfunc_end29-bi_cdr
                                        # -- End function
	.p2align	2                               # -- Begin function bi_set_car
	.type	bi_set_car,@function
bi_set_car:                             # @bi_set_car
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	addi r11, r0, 0
	beq r1, r11, .LBB30_3
.LBB30_1:
	andi r5, r1, 1
	addi r4, r0, 0
	bne r5, r4, .LBB30_3
.LBB30_2:
	ldw r5, r1+0
	beq r5, r4, .LBB30_5
.LBB30_3:
	lui r3, %hi(.L.str.68)
	addi r3, r3, %lo(.L.str.68)
	jal r31, lisp_error
.LBB30_4:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB30_5:
	ldw r3, r3+16
	ldw r3, r3+12
	stw r1+12, r3
	lui r1, 524288
	addi r11, r1, -1
	jal r0, .LBB30_4
.Lfunc_end30:
	.size	bi_set_car, .Lfunc_end30-bi_set_car
                                        # -- End function
	.p2align	2                               # -- Begin function bi_set_cdr
	.type	bi_set_cdr,@function
bi_set_cdr:                             # @bi_set_cdr
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	addi r11, r0, 0
	beq r1, r11, .LBB31_3
.LBB31_1:
	andi r5, r1, 1
	addi r4, r0, 0
	bne r5, r4, .LBB31_3
.LBB31_2:
	ldw r5, r1+0
	beq r5, r4, .LBB31_5
.LBB31_3:
	lui r3, %hi(.L.str.69)
	addi r3, r3, %lo(.L.str.69)
	jal r31, lisp_error
.LBB31_4:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB31_5:
	ldw r3, r3+16
	ldw r3, r3+12
	stw r1+16, r3
	lui r1, 524288
	addi r11, r1, -1
	jal r0, .LBB31_4
.Lfunc_end31:
	.size	bi_set_cdr, .Lfunc_end31-bi_set_cdr
                                        # -- End function
	.p2align	2                               # -- Begin function bi_list
	.type	bi_list,@function
bi_list:                                # @bi_list
# %bb.0:
	add r1, r3, r0
	jalr r0, r31, 0
.Lfunc_end32:
	.size	bi_list, .Lfunc_end32-bi_list
                                        # -- End function
	.p2align	2                               # -- Begin function bi_length
	.type	bi_length,@function
bi_length:                              # @bi_length
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r3, r3+12
	addi r1, r0, 1
	addi r11, r0, 0
	beq r3, r11, .LBB33_5
.LBB33_1:
	andi r4, r3, 1
	bne r4, r11, .LBB33_4
.LBB33_2:
	ldw r4, r3+0
	bne r4, r11, .LBB33_4
.LBB33_3:
	ldw r3, r3+16
	addi r1, r1, 2
	bne r3, r11, .LBB33_1
	jal r0, .LBB33_5
.LBB33_4:
	lui r3, %hi(.L.str.70)
	addi r3, r3, %lo(.L.str.70)
	jal r31, lisp_error
	add r1, r11, r0
.LBB33_5:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end33:
	.size	bi_length, .Lfunc_end33-bi_length
                                        # -- End function
	.p2align	2                               # -- Begin function bi_append
	.type	bi_append,@function
bi_append:                              # @bi_append
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
	addi r1, r0, 0
	beq r3, r1, .LBB34_17
.LBB34_1:
	ldw r1, r3+16
	addi r4, r0, 0
	beq r1, r4, .LBB34_13
.LBB34_2:
	ldw r5, r3+12
	addi r16, fp, -32
	stw r16+0, r5
	ldw r5, r1+12
	addi r11, fp, -36
	stw r11+0, r5
	ldw r1, r1+16
	lui r15, %hi(root_sp)
	addi r15, r15, %lo(root_sp)
	beq r1, r4, .LBB34_5
.LBB34_3:
	addi r1, fp, -32
	add r13, r3, r0
	add r3, r1, r0
	add r12, r4, r0
	jal r31, push_root_checked
	ldw r3, r13+16
	jal r31, bi_append
	add r3, r1, r0
	add r1, r12, r0
	ldw r4, r15+0
	addi r4, r4, -1
	stw r15+0, r4
	lui r4, %hi(g_error)
	addi r4, r4, %lo(g_error)
	ldw r4, r4+0
	bne r4, r12, .LBB34_17
.LBB34_4:
	stw r11+0, r3
.LBB34_5:
	ldw r1, r16+0
	addi r12, r0, 0
	beq r1, r12, .LBB34_16
.LBB34_6:
	addi r11, fp, -40
	stw r11+0, r12
	addi r13, fp, -44
	stw r13+0, r12
	add r3, r11, r0
	jal r31, push_root_checked
	add r3, r13, r0
	jal r31, push_root_checked
	addi r14, fp, -36
	add r3, r14, r0
	jal r31, push_root_checked
	ldw r16, r16+0
	bne r16, r12, .LBB34_11
.LBB34_7:
	ldw r3, r13+0
	ldw r1, r14+0
	beq r3, r12, .LBB34_14
.LBB34_8:
	stw r3+16, r1
	jal r0, .LBB34_15
.LBB34_9:
	ldw r3, r13+0
	stw r3+16, r1
.LBB34_10:
	stw r13+0, r1
	ldw r16, r16+16
	beq r16, r12, .LBB34_7
.LBB34_11:
	ldw r3, r16+12
	add r4, r12, r0
	jal r31, cons_alloc
	ldw r3, r11+0
	bne r3, r12, .LBB34_9
.LBB34_12:
	stw r11+0, r1
	jal r0, .LBB34_10
.LBB34_13:
	ldw r1, r3+12
	jal r0, .LBB34_17
.LBB34_14:
	stw r11+0, r1
.LBB34_15:
	ldw r1, r15+0
	addi r1, r1, -3
	stw r15+0, r1
.LBB34_16:
	ldw r1, r11+0
.LBB34_17:
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
.Lfunc_end34:
	.size	bi_append, .Lfunc_end34-bi_append
                                        # -- End function
	.p2align	2                               # -- Begin function bi_reverse
	.type	bi_reverse,@function
bi_reverse:                             # @bi_reverse
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
	ldw r12, r3+12
	addi r13, r0, 0
	addi r11, fp, -20
	stw r11+0, r13
	add r3, r11, r0
	jal r31, push_root_checked
	beq r12, r13, .LBB35_2
.LBB35_1:
	ldw r3, r12+12
	ldw r4, r11+0
	jal r31, cons_alloc
	stw r11+0, r1
	ldw r12, r12+16
	bne r12, r13, .LBB35_1
.LBB35_2:
	lui r1, %hi(root_sp)
	addi r1, r1, %lo(root_sp)
	ldw r3, r1+0
	addi r3, r3, -1
	stw r1+0, r3
	ldw r1, r11+0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end35:
	.size	bi_reverse, .Lfunc_end35-bi_reverse
                                        # -- End function
	.p2align	2                               # -- Begin function bi_map
	.type	bi_map,@function
bi_map:                                 # @bi_map
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
	stw fp+-56, lr
	ldw r1, r3+12
	addi r12, fp, -60
	stw r12+0, r1
	ldw r1, r3+16
	ldw r1, r1+12
	addi r14, fp, -64
	stw r14+0, r1
	addi r11, r0, 0
	addi r13, fp, -68
	stw r13+0, r11
	addi r15, fp, -72
	stw r15+0, r11
	add r3, r12, r0
	jal r31, push_root_checked
	add r3, r14, r0
	jal r31, push_root_checked
	add r3, r13, r0
	jal r31, push_root_checked
	add r3, r15, r0
	jal r31, push_root_checked
	ldw r1, r14+0
	lui r18, %hi(root_sp)
	addi r18, r18, %lo(root_sp)
	beq r1, r11, .LBB36_13
.LBB36_1:
	addi r16, fp, -76
	addi r19, r0, 3
	lui r20, %hi(g_error)
	addi r20, r20, %lo(g_error)
	addi r17, fp, -80
	addi r21, r0, 4
	jal r0, .LBB36_4
.LBB36_2:
	ldw r3, r15+0
	stw r3+16, r1
.LBB36_3:
	stw r15+0, r1
	ldw r1, r14+0
	ldw r1, r1+16
	stw r14+0, r1
	beq r1, r11, .LBB36_13
.LBB36_4:
	ldw r3, r1+12
	add r4, r11, r0
	jal r31, cons_alloc
	stw r16+0, r1
	add r3, r16, r0
	jal r31, push_root_checked
	ldw r22, r12+0
	ldw r1, r22+0
	beq r1, r19, .LBB36_7
.LBB36_5:
	bne r1, r21, .LBB36_15
.LBB36_6:
	ldw r1, r22+16
	ldw r3, r16+0
	jalr lr, r1, 0
	stw r17+0, r1
	jal r0, .LBB36_10
.LBB36_7:
	ldw r3, r22+20
	ldw r4, r22+12
	ldw r5, r16+0
	jal r31, env_extend
	ldw r23, r20+0
	beq r23, r11, .LBB36_9
.LBB36_8:
	ldw r1, r18+0
	addi r1, r1, -5
	stw r18+0, r1
	beq r23, r11, .LBB36_10
	jal r0, .LBB36_16
.LBB36_9:
	ldw r3, r22+16
	add r4, r1, r0
	jal r31, eval
	stw r17+0, r1
	bne r23, r11, .LBB36_16
.LBB36_10:
	ldw r1, r18+0
	addi r3, r1, -1
	stw r18+0, r3
	ldw r3, r20+0
	bne r3, r11, .LBB36_14
.LBB36_11:
	add r3, r17, r0
	jal r31, push_root_checked
	ldw r3, r17+0
	add r4, r11, r0
	jal r31, cons_alloc
	ldw r3, r18+0
	addi r3, r3, -1
	stw r18+0, r3
	ldw r3, r13+0
	bne r3, r11, .LBB36_2
.LBB36_12:
	stw r13+0, r1
	jal r0, .LBB36_3
.LBB36_13:
	ldw r1, r18+0
	addi r1, r1, -4
	stw r18+0, r1
	ldw r11, r13+0
	jal r0, .LBB36_16
.LBB36_14:
	addi r1, r1, -5
	stw r18+0, r1
	jal r0, .LBB36_16
.LBB36_15:
	ldw r1, r18+0
	addi r1, r1, -5
	stw r18+0, r1
	lui r3, %hi(.L.str.71)
	addi r3, r3, %lo(.L.str.71)
	jal r31, lisp_error
.LBB36_16:
	add r1, r11, r0
	ldw lr, fp+-56
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
.Lfunc_end36:
	.size	bi_map, .Lfunc_end36-bi_map
                                        # -- End function
	.p2align	2                               # -- Begin function bi_string_length
	.type	bi_string_length,@function
bi_string_length:                       # @bi_string_length
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	addi r11, r0, 0
	beq r1, r11, .LBB37_4
.LBB37_1:
	andi r3, r1, 1
	addi r4, r0, 0
	bne r3, r4, .LBB37_4
.LBB37_2:
	ldw r3, r1+0
	addi r4, r0, 2
	bne r3, r4, .LBB37_4
.LBB37_3:
	ldw r1, r1+16
	slli r1, r1, 1
	addi r11, r1, 1
	jal r0, .LBB37_5
.LBB37_4:
	lui r3, %hi(.L.str.72)
	addi r3, r3, %lo(.L.str.72)
	jal r31, lisp_error
.LBB37_5:
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end37:
	.size	bi_string_length, .Lfunc_end37-bi_string_length
                                        # -- End function
	.p2align	2                               # -- Begin function bi_string_append
	.type	bi_string_append,@function
bi_string_append:                       # @bi_string_append
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
	stw fp+-28, lr
	add r12, r3, r0
	addi r14, r0, 0
	add r11, r14, r0
	beq r3, r14, .LBB38_6
.LBB38_1:
	addi r1, r0, 0
	addi r3, r0, 2
	add r11, r1, r0
	add r4, r12, r0
.LBB38_2:
	ldw r5, r4+12
	beq r5, r1, .LBB38_11
.LBB38_3:
	andi r6, r5, 1
	bne r6, r1, .LBB38_11
.LBB38_4:
	ldw r6, r5+0
	bne r6, r3, .LBB38_11
.LBB38_5:
	ldw r5, r5+16
	add r11, r5, r11
	ldw r4, r4+16
	bne r4, r1, .LBB38_2
.LBB38_6:
	addi r3, r11, 1
	jal r31, malloc
	beq r1, r14, .LBB38_12
.LBB38_7:
	add r13, r1, r0
	addi r14, r0, 0
	beq r12, r14, .LBB38_10
.LBB38_8:
	add r15, r14, r0
.LBB38_9:
	ldw r16, r12+12
	add r3, r13, r15
	ldw r4, r16+12
	ldw r5, r16+16
	jal r31, memcpy
	ldw r1, r16+16
	add r15, r1, r15
	ldw r12, r12+16
	bne r12, r14, .LBB38_9
.LBB38_10:
	add r1, r13, r11
	addi r3, r0, 0
	stb r1+0, r3
	add r3, r13, r0
	add r4, r11, r0
	jal r31, string_alloc
	add r11, r1, r0
	add r3, r13, r0
	jal r31, free
	add r1, r11, r0
	jal r0, .LBB38_14
.LBB38_11:
	lui r3, %hi(.L.str.73)
	addi r3, r3, %lo(.L.str.73)
	jal r0, .LBB38_13
.LBB38_12:
	lui r3, %hi(.L.str.74)
	addi r3, r3, %lo(.L.str.74)
.LBB38_13:
	jal r31, lisp_error
	addi r1, r0, 0
.LBB38_14:
	ldw lr, fp+-28
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
.Lfunc_end38:
	.size	bi_string_append, .Lfunc_end38-bi_string_append
                                        # -- End function
	.p2align	2                               # -- Begin function bi_substring
	.type	bi_substring,@function
bi_substring:                           # @bi_substring
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	ldw r1, r3+12
	addi r11, r0, 0
	beq r1, r11, .LBB39_9
.LBB39_1:
	andi r4, r1, 1
	addi r12, r0, 0
	bne r4, r12, .LBB39_9
.LBB39_2:
	ldw r4, r1+0
	addi r5, r0, 2
	bne r4, r5, .LBB39_9
.LBB39_3:
	ldw r4, r3+16
	ldw r3, r4+12
	andi r5, r3, 1
	beq r5, r12, .LBB39_11
.LBB39_4:
	ldw r4, r4+16
	ldw r5, r4+12
	andi r4, r5, 1
	beq r4, r12, .LBB39_11
.LBB39_5:
	srai r4, r3, 1
	blt r4, r12, .LBB39_12
.LBB39_6:
	srai r5, r5, 1
	blt r5, r4, .LBB39_12
.LBB39_7:
	ldw r3, r1+16
	bgt r5, r3, .LBB39_12
.LBB39_8:
	ldw r1, r1+12
	add r3, r1, r4
	sub r4, r5, r4
	jal r31, string_alloc
	add r11, r1, r0
	jal r0, .LBB39_10
.LBB39_9:
	lui r3, %hi(.L.str.75)
	addi r3, r3, %lo(.L.str.75)
	jal r31, lisp_error
.LBB39_10:
	add r1, r11, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB39_11:
	lui r3, %hi(.L.str.76)
	addi r3, r3, %lo(.L.str.76)
	jal r0, .LBB39_13
.LBB39_12:
	lui r3, %hi(.L.str.77)
	addi r3, r3, %lo(.L.str.77)
.LBB39_13:
	jal r31, lisp_error
	add r11, r12, r0
	jal r0, .LBB39_10
.Lfunc_end39:
	.size	bi_substring, .Lfunc_end39-bi_substring
                                        # -- End function
	.p2align	2                               # -- Begin function bi_string_to_number
	.type	bi_string_to_number,@function
bi_string_to_number:                    # @bi_string_to_number
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	ldw r1, r3+12
	addi r11, r0, 0
	beq r1, r11, .LBB40_4
.LBB40_1:
	andi r3, r1, 1
	addi r12, r0, 0
	bne r3, r12, .LBB40_4
.LBB40_2:
	ldw r3, r1+0
	addi r4, r0, 2
	bne r3, r4, .LBB40_4
.LBB40_3:
	ldw r3, r1+12
	addi r11, fp, -16
	addi r5, r0, 10
	add r4, r11, r0
	jal r31, strtol
	ldw r3, r11+0
	ldbu r3, r3+0
	seq r3, r3, r12
	slli r1, r1, 1
	addi r1, r1, 1
	sub r3, r12, r3
	and r11, r1, r3
	jal r0, .LBB40_5
.LBB40_4:
	lui r3, %hi(.L.str.78)
	addi r3, r3, %lo(.L.str.78)
	jal r31, lisp_error
.LBB40_5:
	add r1, r11, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end40:
	.size	bi_string_to_number, .Lfunc_end40-bi_string_to_number
                                        # -- End function
	.p2align	2                               # -- Begin function bi_number_to_string
	.type	bi_number_to_string,@function
bi_number_to_string:                    # @bi_number_to_string
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 56
	stw fp+-4, r11
	stw fp+-8, lr
	ldw r1, r3+12
	andi r3, r1, 1
	addi r4, r0, 0
	bne r3, r4, .LBB41_2
.LBB41_1:
	lui r3, %hi(.L.str.79)
	addi r3, r3, %lo(.L.str.79)
	add r11, r4, r0
	jal r31, lisp_error
	add r1, r11, r0
	jal r0, .LBB41_8
.LBB41_2:
	srai r3, r1, 1
	addi r1, fp, -40
	stb r1+31, r4
	beq r3, r4, .LBB41_9
.LBB41_3:
	srai r4, r3, 31
	xor r5, r3, r4
	sub r6, r5, r4
	addi r4, r0, 30
	lui r5, 838861
	addi r5, r5, -819
	addi r7, r0, 10
	addi r8, r0, 9
.LBB41_4:
	add r9, r6, r0
	mulhu r6, r6, r5
	srli r6, r6, 3
	mul r10, r6, r7
	sub r10, r9, r10
	ori  r10, r10, 48
	add r11, r1, r4
	stb r11+0, r10
	addi r4, r4, -1
	bgtu r9, r8, .LBB41_4
.LBB41_5:
	addi r4, r4, 1
	addi r5, r0, -1
	bgt r3, r5, .LBB41_7
.LBB41_6:
	addi r4, r4, -1
	add r3, r1, r4
	addi r5, r0, 45
	stb r3+0, r5
.LBB41_7:
	add r3, r1, r4
	addi r1, r0, 31
	sub r4, r1, r4
	jal r31, string_alloc
.LBB41_8:
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.LBB41_9:
	addi r4, r0, 48
	stb r1+30, r4
	addi r4, r0, 30
	addi r5, r0, -1
	bgt r3, r5, .LBB41_7
	jal r0, .LBB41_6
.Lfunc_end41:
	.size	bi_number_to_string, .Lfunc_end41-bi_number_to_string
                                        # -- End function
	.p2align	2                               # -- Begin function bi_display
	.type	bi_display,@function
bi_display:                             # @bi_display
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r3, r3+12
	addi r1, r0, 0
	beq r3, r1, .LBB42_4
.LBB42_1:
	andi r4, r3, 1
	bne r4, r1, .LBB42_4
.LBB42_2:
	ldw r1, r3+0
	addi r4, r0, 2
	bne r1, r4, .LBB42_4
.LBB42_3:
	ldw r4, r3+12
	lui r3, %hi(.L.str.80)
	addi r3, r3, %lo(.L.str.80)
	jal r31, printf
	jal r0, .LBB42_5
.LBB42_4:
	jal r31, lisp_print
.LBB42_5:
	lui r1, 524288
	addi r1, r1, -1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end42:
	.size	bi_display, .Lfunc_end42-bi_display
                                        # -- End function
	.p2align	2                               # -- Begin function bi_newline
	.type	bi_newline,@function
bi_newline:                             # @bi_newline
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r3, r0, 10
	jal r31, putchar
	lui r1, 524288
	addi r1, r1, -1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end43:
	.size	bi_newline, .Lfunc_end43-bi_newline
                                        # -- End function
	.p2align	2                               # -- Begin function bi_read
	.type	bi_read,@function
bi_read:                                # @bi_read
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	addi r12, r0, 0
	addi r11, fp, -16
	stw r11+0, r12
	add r3, r11, r0
	jal r31, lisp_read
	ldw r3, r11+0
	beq r3, r12, .LBB44_2
.LBB44_1:
	lui r3, %hi(.L.str.81)
	addi r3, r3, %lo(.L.str.81)
	jal r31, symbol_intern
.LBB44_2:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end44:
	.size	bi_read, .Lfunc_end44-bi_read
                                        # -- End function
	.p2align	2                               # -- Begin function bi_apply
	.type	bi_apply,@function
bi_apply:                               # @bi_apply
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
	ldw r12, r3+12
	addi r11, r0, 0
	beq r12, r11, .LBB45_5
.LBB45_1:
	andi r1, r12, 1
	addi r13, r0, 0
	bne r1, r13, .LBB45_5
.LBB45_2:
	ldw r1, r3+16
	ldw r5, r1+12
	ldw r1, r12+0
	addi r3, r0, 3
	beq r1, r3, .LBB45_6
.LBB45_3:
	addi r3, r0, 4
	bne r1, r3, .LBB45_8
.LBB45_4:
	ldw r1, r12+16
	add r3, r5, r0
	jalr lr, r1, 0
	add r11, r1, r0
	jal r0, .LBB45_9
.LBB45_5:
	lui r3, %hi(.L.str.82)
	addi r3, r3, %lo(.L.str.82)
	jal r31, lisp_error
	jal r0, .LBB45_9
.LBB45_6:
	ldw r3, r12+20
	ldw r4, r12+12
	jal r31, env_extend
	lui r3, %hi(g_error)
	addi r3, r3, %lo(g_error)
	ldw r3, r3+0
	addi r11, r0, 0
	bne r3, r11, .LBB45_9
.LBB45_7:
	ldw r3, r12+16
	add r4, r1, r0
	jal r31, eval
	add r11, r1, r0
	jal r0, .LBB45_9
.LBB45_8:
	lui r3, %hi(.L.str.82)
	addi r3, r3, %lo(.L.str.82)
	jal r31, lisp_error
	add r11, r13, r0
.LBB45_9:
	add r1, r11, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end45:
	.size	bi_apply, .Lfunc_end45-bi_apply
                                        # -- End function
	.p2align	2                               # -- Begin function bi_error
	.type	bi_error,@function
bi_error:                               # @bi_error
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	ldw r1, r3+12
	lui r3, %hi(.L.str.45)
	addi r3, r3, %lo(.L.str.45)
	addi r4, r0, 0
	beq r1, r4, .LBB46_4
.LBB46_1:
	andi r5, r1, 1
	bne r5, r4, .LBB46_4
.LBB46_2:
	ldw r4, r1+0
	addi r5, r0, 2
	bne r4, r5, .LBB46_4
.LBB46_3:
	ldw r3, r1+12
.LBB46_4:
	jal r31, lisp_error
	addi r1, r0, 0
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end46:
	.size	bi_error, .Lfunc_end46-bi_error
                                        # -- End function
	.p2align	2                               # -- Begin function equal_deep
	.type	equal_deep,@function
equal_deep:                             # @equal_deep
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
	stw fp+-28, lr
	add r14, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	addi r11, r0, 0
	addi r16, r0, 1000
	addi r15, r0, 1
	bgt r14, r16, .LBB47_13
.LBB47_1:
	beq r13, r12, .LBB47_12
.LBB47_2:
	or  r1, r12, r13
	andi r1, r1, 1
	bne r1, r11, .LBB47_13
.LBB47_3:
	beq r13, r11, .LBB47_13
.LBB47_4:
	beq r12, r11, .LBB47_13
.LBB47_5:
	ldw r1, r13+0
	ldw r3, r12+0
	bne r1, r3, .LBB47_13
.LBB47_6:
	bne r1, r11, .LBB47_9
.LBB47_7:
	ldw r3, r13+12
	ldw r4, r12+12
	addi r14, r14, 1
	add r5, r14, r0
	jal r31, equal_deep
	beq r1, r11, .LBB47_13
.LBB47_8:
	ldw r13, r13+16
	ldw r12, r12+16
	ble r14, r16, .LBB47_1
	jal r0, .LBB47_13
.LBB47_9:
	addi r3, r0, 2
	bne r1, r3, .LBB47_13
.LBB47_10:
	ldw r5, r13+16
	ldw r1, r12+16
	bne r5, r1, .LBB47_13
.LBB47_11:
	ldw r3, r13+12
	ldw r4, r12+12
	jal r31, memcmp
	addi r3, r0, 0
	seq r11, r1, r3
	jal r0, .LBB47_13
.LBB47_12:
	add r11, r15, r0
.LBB47_13:
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
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end47:
	.size	equal_deep, .Lfunc_end47-equal_deep
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"+"
	.size	.L.str, 2

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"-"
	.size	.L.str.1, 2

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"*"
	.size	.L.str.2, 2

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"/"
	.size	.L.str.3, 2

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"modulo"
	.size	.L.str.4, 7

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"abs"
	.size	.L.str.5, 4

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"min"
	.size	.L.str.6, 4

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"max"
	.size	.L.str.7, 4

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"="
	.size	.L.str.8, 2

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"<"
	.size	.L.str.9, 2

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	">"
	.size	.L.str.10, 2

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"<="
	.size	.L.str.11, 3

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	">="
	.size	.L.str.12, 3

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"null?"
	.size	.L.str.13, 6

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"pair?"
	.size	.L.str.14, 6

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"number?"
	.size	.L.str.15, 8

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"symbol?"
	.size	.L.str.16, 8

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"string?"
	.size	.L.str.17, 8

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"procedure?"
	.size	.L.str.18, 11

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"zero?"
	.size	.L.str.19, 6

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"positive?"
	.size	.L.str.20, 10

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"negative?"
	.size	.L.str.21, 10

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"not"
	.size	.L.str.22, 4

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"boolean?"
	.size	.L.str.23, 9

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"eq?"
	.size	.L.str.24, 4

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"equal?"
	.size	.L.str.25, 7

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"cons"
	.size	.L.str.26, 5

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"car"
	.size	.L.str.27, 4

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"cdr"
	.size	.L.str.28, 4

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"set-car!"
	.size	.L.str.29, 9

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"set-cdr!"
	.size	.L.str.30, 9

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"list"
	.size	.L.str.31, 5

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"length"
	.size	.L.str.32, 7

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"append"
	.size	.L.str.33, 7

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"reverse"
	.size	.L.str.34, 8

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"map"
	.size	.L.str.35, 4

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"string-length"
	.size	.L.str.36, 14

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"string-append"
	.size	.L.str.37, 14

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"substring"
	.size	.L.str.38, 10

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"string->number"
	.size	.L.str.39, 15

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"number->string"
	.size	.L.str.40, 15

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"display"
	.size	.L.str.41, 8

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"newline"
	.size	.L.str.42, 8

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"read"
	.size	.L.str.43, 5

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"apply"
	.size	.L.str.44, 6

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"error"
	.size	.L.str.45, 6

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"+: not a number"
	.size	.L.str.46, 16

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"-: need at least 1 arg"
	.size	.L.str.47, 23

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"-: not a number"
	.size	.L.str.48, 16

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"*: not a number"
	.size	.L.str.49, 16

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"/: need 2+ args"
	.size	.L.str.50, 16

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"/: not a number"
	.size	.L.str.51, 16

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"/: division by zero"
	.size	.L.str.52, 20

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"modulo: need 2 args"
	.size	.L.str.53, 20

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"modulo: not a number"
	.size	.L.str.54, 21

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"modulo: division by zero"
	.size	.L.str.55, 25

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"abs: not a number"
	.size	.L.str.56, 18

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"min: need args"
	.size	.L.str.57, 15

	.type	.L.str.58,@object               # @.str.58
.L.str.58:
	.asciz	"min: not a number"
	.size	.L.str.58, 18

	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"max: need args"
	.size	.L.str.59, 15

	.type	.L.str.60,@object               # @.str.60
.L.str.60:
	.asciz	"max: not a number"
	.size	.L.str.60, 18

	.type	.L.str.61,@object               # @.str.61
.L.str.61:
	.asciz	"=: not a number"
	.size	.L.str.61, 16

	.type	.L.str.62,@object               # @.str.62
.L.str.62:
	.asciz	"<: not a number"
	.size	.L.str.62, 16

	.type	.L.str.63,@object               # @.str.63
.L.str.63:
	.asciz	">: not a number"
	.size	.L.str.63, 16

	.type	.L.str.64,@object               # @.str.64
.L.str.64:
	.asciz	"<=: not a number"
	.size	.L.str.64, 17

	.type	.L.str.65,@object               # @.str.65
.L.str.65:
	.asciz	">=: not a number"
	.size	.L.str.65, 17

	.type	.L.str.66,@object               # @.str.66
.L.str.66:
	.asciz	"car: not a pair"
	.size	.L.str.66, 16

	.type	.L.str.67,@object               # @.str.67
.L.str.67:
	.asciz	"cdr: not a pair"
	.size	.L.str.67, 16

	.type	.L.str.68,@object               # @.str.68
.L.str.68:
	.asciz	"set-car!: not a pair"
	.size	.L.str.68, 21

	.type	.L.str.69,@object               # @.str.69
.L.str.69:
	.asciz	"set-cdr!: not a pair"
	.size	.L.str.69, 21

	.type	.L.str.70,@object               # @.str.70
.L.str.70:
	.asciz	"length: not a proper list"
	.size	.L.str.70, 26

	.type	.L.str.71,@object               # @.str.71
.L.str.71:
	.asciz	"map: not a procedure"
	.size	.L.str.71, 21

	.type	.L.str.72,@object               # @.str.72
.L.str.72:
	.asciz	"string-length: not a string"
	.size	.L.str.72, 28

	.type	.L.str.73,@object               # @.str.73
.L.str.73:
	.asciz	"string-append: not a string"
	.size	.L.str.73, 28

	.type	.L.str.74,@object               # @.str.74
.L.str.74:
	.asciz	"out of memory"
	.size	.L.str.74, 14

	.type	.L.str.75,@object               # @.str.75
.L.str.75:
	.asciz	"substring: not a string"
	.size	.L.str.75, 24

	.type	.L.str.76,@object               # @.str.76
.L.str.76:
	.asciz	"substring: bad index"
	.size	.L.str.76, 21

	.type	.L.str.77,@object               # @.str.77
.L.str.77:
	.asciz	"substring: index out of range"
	.size	.L.str.77, 30

	.type	.L.str.78,@object               # @.str.78
.L.str.78:
	.asciz	"string->number: not a string"
	.size	.L.str.78, 29

	.type	.L.str.79,@object               # @.str.79
.L.str.79:
	.asciz	"number->string: not a number"
	.size	.L.str.79, 29

	.type	.L.str.80,@object               # @.str.80
.L.str.80:
	.asciz	"%s"
	.size	.L.str.80, 3

	.type	.L.str.81,@object               # @.str.81
.L.str.81:
	.asciz	"eof"
	.size	.L.str.81, 4

	.type	.L.str.82,@object               # @.str.82
.L.str.82:
	.asciz	"apply: not a procedure"
	.size	.L.str.82, 23

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
