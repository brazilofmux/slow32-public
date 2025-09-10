	.file	"switch_patterns.c"
	.text
	.globl	dense_switch                    # -- Begin function dense_switch
	.p2align	2
	.type	dense_switch,@function
dense_switch:                           # @dense_switch
# %bb.0:
	addi r1, r0, 6
	sltu r1, r3, r1
	addi r2, r3, 100
	addi r3, r0, 0
	sub r1, r3, r1
	addi r3, r0, -1
	xor r2, r2, r3
	and r1, r2, r1
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end0:
	.size	dense_switch, .Lfunc_end0-dense_switch
                                        # -- End function
	.globl	sparse_switch                   # -- Begin function sparse_switch
	.p2align	2
	.type	sparse_switch,@function
sparse_switch:                          # @sparse_switch
# %bb.0:
	ori r1, r0, 99
	sgt r1, r3, r1
	addi r2, r0, 0
	jal r0, .LBB1_1
.LBB1_1:
	addi r1, r0, 1
	jal r0, .LBB1_3
.LBB1_3:
	addi r1, r0, 10
	jal r0, .LBB1_9
.LBB1_6:
	addi r1, r0, 20
	jalr r0, r31, 0
.LBB1_4:
	addi r1, r0, 100
	jal r0, .LBB1_5
.LBB1_5:
	addi r1, r0, 1000
	jal r0, .LBB1_9
.LBB1_8:
	addi r1, r0, 40
	jalr r0, r31, 0
.LBB1_9:
	addi r1, r0, -1
.LBB1_10:
	jalr r0, r31, 0
.LBB1_2:
	addi r1, r0, 10
	jalr r0, r31, 0
.LBB1_7:
	addi r1, r0, 30
	jalr r0, r31, 0
.Lfunc_end1:
	.size	sparse_switch, .Lfunc_end1-sparse_switch
                                        # -- End function
	.globl	gap_switch                      # -- Begin function gap_switch
	.p2align	2
	.type	gap_switch,@function
gap_switch:                             # @gap_switch
# %bb.0:
	addi r1, r3, -1
	addi r2, r0, 5
	sgtu r2, r1, r2
	addi r3, r0, 0
	jal r0, .LBB2_2
.LBB2_2:
	ori  r2, r0, %lo(.Lswitch.table.gap_switch)
	lui r3, %hi(.Lswitch.table.gap_switch)
	add r2, r3, r2
	slli r1, r1, 2
	add r1, r1, r2
	ldw r1, r1+0
.LBB2_3:
	jalr r0, r31, 0
.LBB2_1:
	addi r1, r0, -1
	jalr r0, r31, 0
.Lfunc_end2:
	.size	gap_switch, .Lfunc_end2-gap_switch
                                        # -- End function
	.globl	char_switch                     # -- Begin function char_switch
	.p2align	2
	.type	char_switch,@function
char_switch:                            # @char_switch
# %bb.0:
	addi r1, r3, -97
	addi r2, r0, 5
	sltu r1, r1, r2
	addi r2, r3, -96
	addi r3, r0, 0
	sub r1, r3, r1
	and r1, r2, r1
	jalr r0, r31, 0
.Lfunc_end3:
	.size	char_switch, .Lfunc_end3-char_switch
                                        # -- End function
	.globl	large_switch                    # -- Begin function large_switch
	.p2align	2
	.type	large_switch,@function
large_switch:                           # @large_switch
# %bb.0:
	addi r1, r0, 16
	sltu r1, r3, r1
	addi r2, r3, 1000
	addi r3, r0, 0
	sub r1, r3, r1
	addi r3, r0, -1
	xor r2, r2, r3
	and r1, r2, r1
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end4:
	.size	large_switch, .Lfunc_end4-large_switch
                                        # -- End function
	.globl	binary_switch                   # -- Begin function binary_switch
	.p2align	2
	.type	binary_switch,@function
binary_switch:                          # @binary_switch
# %bb.0:
	addi r2, r3, -1
	addi r1, r0, 63
	sgtu r4, r2, r1
	addi r1, r0, 0
	jal r0, .LBB5_2
.LBB5_2:
	slli r2, r2, 2
	ori  r3, r0, %lo(.LJTI5_0)
	lui r4, %hi(.LJTI5_0)
	add r3, r4, r3
	add r2, r3, r2
	ldw r2, r2+0
	jalr r31, r2, 0
.LBB5_3:
	addi r1, r0, 10
	jalr r0, r31, 0
.LBB5_1:
	addi r2, r0, 128
	jal r0, .LBB5_11
.LBB5_10:
	addi r1, r0, 1280
	jal r0, .LBB5_11
.LBB5_11:
	jalr r0, r31, 0
.LBB5_9:
	addi r1, r0, 640
	jalr r0, r31, 0
.LBB5_5:
	addi r1, r0, 40
	jalr r0, r31, 0
.LBB5_6:
	addi r1, r0, 80
	jalr r0, r31, 0
.LBB5_4:
	addi r1, r0, 20
	jalr r0, r31, 0
.LBB5_7:
	addi r1, r0, 160
	jalr r0, r31, 0
.LBB5_8:
	addi r1, r0, 320
	jalr r0, r31, 0
.Lfunc_end5:
	.size	binary_switch, .Lfunc_end5-binary_switch
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI5_0:
	.word	.LBB5_11
	.word	.LBB5_4
	.word	.LBB5_11
	.word	.LBB5_5
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_6
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_7
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_8
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_11
	.word	.LBB5_9
                                        # -- End function
	.text
	.globl	state_machine                   # -- Begin function state_machine
	.p2align	2
	.type	state_machine,@function
state_machine:                          # @state_machine
# %bb.0:
	addi r1, r0, 2
	jal r0, .LBB6_1
.LBB6_1:
	addi r2, r0, 1
	jal r0, .LBB6_2
.LBB6_2:
	addi r1, r0, 0
	jal r0, .LBB6_3
.LBB6_3:
	ori r1, r0, 0
	sgt r1, r4, r1
	jalr r0, r31, 0
.LBB6_4:
	ori r1, r0, 0
	seq r1, r4, r1
	ori  r1, r1, 2
	jal r0, .LBB6_5
.LBB6_5:
	jalr r0, r31, 0
.Lfunc_end6:
	.size	state_machine, .Lfunc_end6-state_machine
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi r1, r0, 1359
	jalr r0, r31, 0
.Lfunc_end7:
	.size	main, .Lfunc_end7-main
                                        # -- End function
	.type	.Lswitch.table.gap_switch,@object # @switch.table.gap_switch
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.Lswitch.table.gap_switch:
	.word	10                              # 0xa
	.word	20                              # 0x14
	.word	30                              # 0x1e
	.word	4294967295                      # 0xffffffff
	.word	50                              # 0x32
	.word	60                              # 0x3c
	.size	.Lswitch.table.gap_switch, 24

	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
