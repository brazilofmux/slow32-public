# Nested .cfi_remember_state deeper than the old 8-slot save stack.
# Each remember is paired with restore so the CFA tracker stays inverted.
	.text
	.globl main
	.p2align 2
main:
	.cfi_startproc
	addi sp, sp, -16
	.cfi_adjust_cfa_offset 16
	.cfi_remember_state
	addi sp, sp, -4
	.cfi_adjust_cfa_offset 4
	.cfi_remember_state
	addi sp, sp, -4
	.cfi_adjust_cfa_offset 4
	.cfi_remember_state
	addi sp, sp, -4
	.cfi_adjust_cfa_offset 4
	.cfi_remember_state
	addi sp, sp, -4
	.cfi_adjust_cfa_offset 4
	.cfi_remember_state
	addi sp, sp, -4
	.cfi_adjust_cfa_offset 4
	.cfi_remember_state
	addi sp, sp, -4
	.cfi_adjust_cfa_offset 4
	.cfi_remember_state
	addi sp, sp, -4
	.cfi_adjust_cfa_offset 4
	.cfi_remember_state
	addi sp, sp, -4
	.cfi_adjust_cfa_offset 4
	.cfi_remember_state
	addi sp, sp, -4
	.cfi_adjust_cfa_offset 4
	.cfi_remember_state
	addi sp, sp, -4
	.cfi_adjust_cfa_offset 4
	.cfi_remember_state
	addi sp, sp, -4
	.cfi_adjust_cfa_offset 4
	.cfi_remember_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 4
	.cfi_restore_state
	addi sp, sp, 16
	.cfi_adjust_cfa_offset -16
	addi r3, r0, 0x53
	debug r3
	addi r3, r0, 10
	debug r3
	halt
	.cfi_endproc
