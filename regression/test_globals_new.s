	.file	"test_globals.ll"
	.text
	.globl	test_global_read                # -- Begin function test_global_read
	.type	test_global_read,@function
test_global_read:                       # @test_global_read
# %bb.0:
	ori  r1, r0, %lo(global_const)
	lui r2, %hi(global_const)
	or  r1, r2, r1
	ldw r1, r1+0
	jalr r0, r31, 0
.Lfunc_end0:
	.size	test_global_read, .Lfunc_end0-test_global_read
                                        # -- End function
	.globl	test_global_write               # -- Begin function test_global_write
	.type	test_global_write,@function
test_global_write:                      # @test_global_write
# %bb.0:
	ori  r1, r0, %lo(global_counter)
	lui r2, %hi(global_counter)
	or  r1, r2, r1
	stw r1+0, r3
	jalr r0, r31, 0
.Lfunc_end1:
	.size	test_global_write, .Lfunc_end1-test_global_write
                                        # -- End function
	.globl	test_global_increment           # -- Begin function test_global_increment
	.type	test_global_increment,@function
test_global_increment:                  # @test_global_increment
# %bb.0:
	ori  r1, r0, %lo(global_counter)
	lui r2, %hi(global_counter)
	or  r2, r2, r1
	ldw r1, r2+0
	addi r1, r1, 1
	stw r2+0, r1
	jalr r0, r31, 0
.Lfunc_end2:
	.size	test_global_increment, .Lfunc_end2-test_global_increment
                                        # -- End function
	.globl	test_global_array_access        # -- Begin function test_global_array_access
	.type	test_global_array_access,@function
test_global_array_access:               # @test_global_array_access
# %bb.0:
	ori  r1, r0, %lo(global_array)
	lui r2, %hi(global_array)
	or  r1, r2, r1
	slli r2, r3, 2
	add r1, r1, r2
	ldw r1, r1+0
	jalr r0, r31, 0
.Lfunc_end3:
	.size	test_global_array_access, .Lfunc_end3-test_global_array_access
                                        # -- End function
	.globl	sum_global_array                # -- Begin function sum_global_array
	.type	sum_global_array,@function
sum_global_array:                       # @sum_global_array
# %bb.0:
	ori  r1, r0, %lo(global_array)
	lui r2, %hi(global_array)
	or  r1, r2, r1
	ldw r2, r1+0
	addi r3, r1, 4
	ldw r3, r3+0
	addi r4, r1, 8
	ldw r4, r4+0
	addi r5, r1, 12
	ldw r5, r5+0
	addi r1, r1, 16
	ldw r1, r1+0
	add r2, r2, r3
	add r2, r2, r4
	add r2, r2, r5
	add r1, r2, r1
	jalr r0, r31, 0
.Lfunc_end4:
	.size	sum_global_array, .Lfunc_end4-sum_global_array
                                        # -- End function
	.globl	main                            # -- Begin function main
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	jal r31, test_global_read
	ori r2, r0, 42
	beq r1, r2, .LBB5_6
	jal r0, .LBB5_7
.LBB5_6:
	ori r3, r0, 100
	jal r31, test_global_write
	ori  r1, r0, %lo(global_counter)
	lui r2, %hi(global_counter)
	or  r1, r2, r1
	ldw r1, r1+0
	beq r1, r3, .LBB5_1
.LBB5_7:
	ori r3, r0, 70
	jal r31, debug_char
	ori r3, r0, 65
	jal r31, debug_char
	ori r3, r0, 73
	jal r31, debug_char
	ori r3, r0, 76
	jal r31, debug_char
	ori r3, r0, 10
	jal r31, debug_char
	ori r1, r0, 1
	jal r0, .LBB5_5
.LBB5_1:
	jal r31, test_global_increment
	ori r2, r0, 101
	beq r1, r2, .LBB5_2
	jal r0, .LBB5_7
.LBB5_2:
	ori r3, r0, 2
	jal r31, test_global_array_access
	ori r2, r0, 30
	beq r1, r2, .LBB5_3
	jal r0, .LBB5_7
.LBB5_3:
	jal r31, sum_global_array
	ori r2, r0, 150
	beq r1, r2, .LBB5_4
	jal r0, .LBB5_7
.LBB5_4:
	ori r3, r0, 80
	jal r31, debug_char
	ori r3, r0, 65
	jal r31, debug_char
	ori r3, r0, 83
	jal r31, debug_char
	jal r31, debug_char
	ori r3, r0, 10
	jal r31, debug_char
	ori r1, r0, 0
.LBB5_5:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	main, .Lfunc_end5-main
                                        # -- End function
	.type	global_counter,@object          # @global_counter
	.bss
	.globl	global_counter
	.p2align	2, 0x0
global_counter:
	.word	0                               # 0x0
	.size	global_counter, 4

	.type	global_array,@object            # @global_array
	.data
	.globl	global_array
	.p2align	4, 0x0
global_array:
	.word	10                              # 0xa
	.word	20                              # 0x14
	.word	30                              # 0x1e
	.word	40                              # 0x28
	.word	50                              # 0x32
	.size	global_array, 20

	.type	global_const,@object            # @global_const
	.section	.rodata,"a",@progbits
	.globl	global_const
	.p2align	2, 0x0
global_const:
	.word	42                              # 0x2a
	.size	global_const, 4

	.type	hello_string,@object            # @hello_string
	.globl	hello_string
	.p2align	2, 0x0
hello_string:
	.asciz	"Hello"
	.size	hello_string, 6

	.section	".note.GNU-stack","",@progbits
