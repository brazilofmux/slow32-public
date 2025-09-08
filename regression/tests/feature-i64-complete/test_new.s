	.file	"test.ll"
	.text
	.globl	test_add                        # -- Begin function test_add
	.p2align	2
	.type	test_add,@function
test_add:                               # @test_add
# %bb.0:
	add r2, r4, r6
	add r1, r3, r5
	sltu r3, r1, r3
	add r2, r2, r3
	jalr r0, r31, 0
.Lfunc_end0:
	.size	test_add, .Lfunc_end0-test_add
                                        # -- End function
	.globl	test_sub                        # -- Begin function test_sub
	.p2align	2
	.type	test_sub,@function
test_sub:                               # @test_sub
# %bb.0:
	sltu r1, r3, r5
	sub r2, r4, r6
	sub r2, r2, r1
	sub r1, r3, r5
	jalr r0, r31, 0
.Lfunc_end1:
	.size	test_sub, .Lfunc_end1-test_sub
                                        # -- End function
	.globl	test_mul                        # -- Begin function test_mul
	.p2align	2
	.type	test_mul,@function
test_mul:                               # @test_mul
# %bb.0:
	andi r1, r5, 65535
	srli r2, r3, 16
	mul r7, r2, r1
	srli r8, r5, 16
	andi r9, r3, 65535
	mul r10, r9, r8
	add r7, r10, r7
	mul r1, r9, r1
	srli r9, r1, 16
	add r7, r7, r9
	slli r9, r7, 16
	andi r1, r1, 65535
	or  r1, r1, r9
	srli r7, r7, 16
	mul r2, r2, r8
	add r2, r2, r7
	mul r3, r3, r6
	add r2, r2, r3
	mul r3, r4, r5
	add r2, r2, r3
	jalr r0, r31, 0
.Lfunc_end2:
	.size	test_mul, .Lfunc_end2-test_mul
                                        # -- End function
	.globl	test_udiv                       # -- Begin function test_udiv
	.p2align	2
	.type	test_udiv,@function
test_udiv:                              # @test_udiv
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	jal r31, __udivdi3
	addi r2, r0, 0
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	test_udiv, .Lfunc_end3-test_udiv
                                        # -- End function
	.globl	test_sdiv                       # -- Begin function test_sdiv
	.p2align	2
	.type	test_sdiv,@function
test_sdiv:                              # @test_sdiv
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	jal r31, __divdi3
	addi r2, r0, 0
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	test_sdiv, .Lfunc_end4-test_sdiv
                                        # -- End function
	.globl	test_urem                       # -- Begin function test_urem
	.p2align	2
	.type	test_urem,@function
test_urem:                              # @test_urem
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	jal r31, __umoddi3
	addi r2, r0, 0
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	test_urem, .Lfunc_end5-test_urem
                                        # -- End function
	.globl	test_srem                       # -- Begin function test_srem
	.p2align	2
	.type	test_srem,@function
test_srem:                              # @test_srem
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	jal r31, __moddi3
	addi r2, r0, 0
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	test_srem, .Lfunc_end6-test_srem
                                        # -- End function
	.globl	test_and                        # -- Begin function test_and
	.p2align	2
	.type	test_and,@function
test_and:                               # @test_and
# %bb.0:
	and r1, r3, r5
	and r2, r4, r6
	jalr r0, r31, 0
.Lfunc_end7:
	.size	test_and, .Lfunc_end7-test_and
                                        # -- End function
	.globl	test_or                         # -- Begin function test_or
	.p2align	2
	.type	test_or,@function
test_or:                                # @test_or
# %bb.0:
	or  r1, r3, r5
	or  r2, r4, r6
	jalr r0, r31, 0
.Lfunc_end8:
	.size	test_or, .Lfunc_end8-test_or
                                        # -- End function
	.globl	test_xor                        # -- Begin function test_xor
	.p2align	2
	.type	test_xor,@function
test_xor:                               # @test_xor
# %bb.0:
	xor r1, r3, r5
	xor r2, r4, r6
	jalr r0, r31, 0
.Lfunc_end9:
	.size	test_xor, .Lfunc_end9-test_xor
                                        # -- End function
	.globl	test_shl                        # -- Begin function test_shl
	.p2align	2
	.type	test_shl,@function
test_shl:                               # @test_shl
# %bb.0:
	sll r1, r4, r5
	addi r2, r0, 32
	sub r2, r2, r5
	srl r2, r3, r2
	or  r1, r1, r2
	addi r2, r5, -32
	sll r2, r3, r2
	xor r2, r2, r1
	addi r4, r0, 31
	sgtu r4, r5, r4
	addi r6, r0, 0
	sub r4, r6, r4
	and r2, r2, r4
	xor r2, r1, r2
	sll r1, r3, r5
	and r3, r1, r4
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end10:
	.size	test_shl, .Lfunc_end10-test_shl
                                        # -- End function
	.globl	test_lshr                       # -- Begin function test_lshr
	.p2align	2
	.type	test_lshr,@function
test_lshr:                              # @test_lshr
# %bb.0:
	srl r1, r3, r5
	addi r2, r0, 32
	sub r2, r2, r5
	sll r2, r4, r2
	or  r1, r1, r2
	addi r2, r5, -32
	srl r2, r4, r2
	xor r2, r2, r1
	addi r3, r0, 31
	sgtu r3, r5, r3
	addi r6, r0, 0
	sub r3, r6, r3
	and r2, r2, r3
	xor r1, r1, r2
	srl r2, r4, r5
	and r3, r2, r3
	xor r2, r2, r3
	jalr r0, r31, 0
.Lfunc_end11:
	.size	test_lshr, .Lfunc_end11-test_lshr
                                        # -- End function
	.globl	test_ashr                       # -- Begin function test_ashr
	.p2align	2
	.type	test_ashr,@function
test_ashr:                              # @test_ashr
# %bb.0:
	srl r1, r3, r5
	addi r2, r0, 32
	sub r2, r2, r5
	sll r2, r4, r2
	or  r1, r1, r2
	addi r2, r5, -32
	sra r2, r4, r2
	xor r2, r2, r1
	addi r3, r0, 31
	sgtu r3, r5, r3
	addi r6, r0, 0
	sub r3, r6, r3
	and r2, r2, r3
	xor r1, r1, r2
	sra r2, r4, r5
	srai r4, r4, 31
	xor r4, r4, r2
	and r3, r4, r3
	xor r2, r2, r3
	jalr r0, r31, 0
.Lfunc_end12:
	.size	test_ashr, .Lfunc_end12-test_ashr
                                        # -- End function
	.globl	test_eq                         # -- Begin function test_eq
	.p2align	2
	.type	test_eq,@function
test_eq:                                # @test_eq
# %bb.0:
	xor r1, r4, r6
	xor r2, r3, r5
	or  r1, r2, r1
	ori r2, r0, 0
	seq r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end13:
	.size	test_eq, .Lfunc_end13-test_eq
                                        # -- End function
	.globl	test_ne                         # -- Begin function test_ne
	.p2align	2
	.type	test_ne,@function
test_ne:                                # @test_ne
# %bb.0:
	xor r1, r4, r6
	xor r2, r3, r5
	or  r1, r2, r1
	ori r2, r0, 0
	sne r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end14:
	.size	test_ne, .Lfunc_end14-test_ne
                                        # -- End function
	.globl	test_slt                        # -- Begin function test_slt
	.p2align	2
	.type	test_slt,@function
test_slt:                               # @test_slt
# %bb.0:
	slt r1, r4, r6
	sltu r2, r3, r5
	xor r2, r2, r1
	seq r3, r4, r6
	addi r4, r0, 0
	sub r3, r4, r3
	and r2, r2, r3
	xor r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end15:
	.size	test_slt, .Lfunc_end15-test_slt
                                        # -- End function
	.globl	test_sle                        # -- Begin function test_sle
	.p2align	2
	.type	test_sle,@function
test_sle:                               # @test_sle
# %bb.0:
	sle r1, r4, r6
	sleu r2, r3, r5
	xor r2, r2, r1
	seq r3, r4, r6
	addi r4, r0, 0
	sub r3, r4, r3
	and r2, r2, r3
	xor r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end16:
	.size	test_sle, .Lfunc_end16-test_sle
                                        # -- End function
	.globl	test_sgt                        # -- Begin function test_sgt
	.p2align	2
	.type	test_sgt,@function
test_sgt:                               # @test_sgt
# %bb.0:
	sgt r1, r4, r6
	sgtu r2, r3, r5
	xor r2, r2, r1
	seq r3, r4, r6
	addi r4, r0, 0
	sub r3, r4, r3
	and r2, r2, r3
	xor r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end17:
	.size	test_sgt, .Lfunc_end17-test_sgt
                                        # -- End function
	.globl	test_sge                        # -- Begin function test_sge
	.p2align	2
	.type	test_sge,@function
test_sge:                               # @test_sge
# %bb.0:
	sge r1, r4, r6
	sgeu r2, r3, r5
	xor r2, r2, r1
	seq r3, r4, r6
	addi r4, r0, 0
	sub r3, r4, r3
	and r2, r2, r3
	xor r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end18:
	.size	test_sge, .Lfunc_end18-test_sge
                                        # -- End function
	.globl	test_ult                        # -- Begin function test_ult
	.p2align	2
	.type	test_ult,@function
test_ult:                               # @test_ult
# %bb.0:
	sltu r1, r4, r6
	sltu r2, r3, r5
	xor r2, r2, r1
	seq r3, r4, r6
	addi r4, r0, 0
	sub r3, r4, r3
	and r2, r2, r3
	xor r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end19:
	.size	test_ult, .Lfunc_end19-test_ult
                                        # -- End function
	.globl	test_ule                        # -- Begin function test_ule
	.p2align	2
	.type	test_ule,@function
test_ule:                               # @test_ule
# %bb.0:
	sleu r1, r4, r6
	sleu r2, r3, r5
	xor r2, r2, r1
	seq r3, r4, r6
	addi r4, r0, 0
	sub r3, r4, r3
	and r2, r2, r3
	xor r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end20:
	.size	test_ule, .Lfunc_end20-test_ule
                                        # -- End function
	.globl	test_ugt                        # -- Begin function test_ugt
	.p2align	2
	.type	test_ugt,@function
test_ugt:                               # @test_ugt
# %bb.0:
	sgtu r1, r4, r6
	sgtu r2, r3, r5
	xor r2, r2, r1
	seq r3, r4, r6
	addi r4, r0, 0
	sub r3, r4, r3
	and r2, r2, r3
	xor r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end21:
	.size	test_ugt, .Lfunc_end21-test_ugt
                                        # -- End function
	.globl	test_uge                        # -- Begin function test_uge
	.p2align	2
	.type	test_uge,@function
test_uge:                               # @test_uge
# %bb.0:
	sgeu r1, r4, r6
	sgeu r2, r3, r5
	xor r2, r2, r1
	seq r3, r4, r6
	addi r4, r0, 0
	sub r3, r4, r3
	and r2, r2, r3
	xor r1, r1, r2
	jalr r0, r31, 0
.Lfunc_end22:
	.size	test_uge, .Lfunc_end22-test_uge
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, lr
	addi r3, r0, -1
	addi r11, r0, 1
	addi r12, r0, 0
	add r4, r12, r0
	add r5, r11, r0
	add r6, r12, r0
	jal r31, test_add
	beq r11, r12, .LBB23_22
	jal r0, .LBB23_23
.LBB23_22:
	add r3, r12, r0
	add r4, r11, r0
	add r5, r11, r0
	add r6, r12, r0
	jal r31, test_sub
	beq r11, r12, .LBB23_1
.LBB23_23:
	addi r1, r0, 1
	jal r0, .LBB23_21
.LBB23_1:
	lui r3, 16
	add r4, r12, r0
	add r5, r3, r0
	add r6, r12, r0
	jal r31, test_mul
	beq r11, r12, .LBB23_2
	jal r0, .LBB23_23
.LBB23_2:
	lui r1, 244
	addi r14, r1, 576
	lui r3, 870993
	addi r4, r0, 232
	add r5, r14, r0
	add r6, r12, r0
	jal r31, test_udiv
	beq r11, r12, .LBB23_3
	jal r0, .LBB23_23
.LBB23_3:
	lui r3, 177583
	addi r4, r0, -233
	add r5, r14, r0
	add r6, r12, r0
	jal r31, test_sdiv
	beq r11, r12, .LBB23_4
	jal r0, .LBB23_23
.LBB23_4:
	lui r15, 466864
	addi r13, r15, 1227
	addi r4, r0, 287
	add r3, r13, r0
	add r5, r14, r0
	add r6, r12, r0
	jal r31, test_urem
	beq r11, r12, .LBB23_5
	jal r0, .LBB23_23
.LBB23_5:
	lui r1, 581712
	addi r3, r1, 2869
	addi r4, r0, -288
	add r5, r14, r0
	add r6, r12, r0
	jal r31, test_srem
	beq r11, r12, .LBB23_6
	jal r0, .LBB23_23
.LBB23_6:
	addi r3, r0, -1
	add r4, r3, r0
	add r5, r3, r0
	add r6, r12, r0
	jal r31, test_and
	beq r11, r12, .LBB23_7
	jal r0, .LBB23_23
.LBB23_7:
	addi r5, r0, 255
	add r3, r12, r0
	add r4, r11, r0
	add r6, r12, r0
	jal r31, test_or
	beq r11, r12, .LBB23_8
	jal r0, .LBB23_23
.LBB23_8:
	addi r3, r0, -1
	add r4, r3, r0
	add r5, r3, r0
	add r6, r12, r0
	jal r31, test_xor
	beq r11, r12, .LBB23_9
	jal r0, .LBB23_23
.LBB23_9:
	addi r5, r0, 32
	add r3, r11, r0
	add r4, r12, r0
	add r6, r12, r0
	jal r31, test_shl
	beq r11, r12, .LBB23_10
	jal r0, .LBB23_23
.LBB23_10:
	addi r5, r0, 32
	add r3, r12, r0
	add r4, r11, r0
	add r6, r12, r0
	jal r31, test_lshr
	beq r11, r12, .LBB23_11
	jal r0, .LBB23_23
.LBB23_11:
	addi r4, r0, -1
	addi r5, r0, 32
	add r3, r12, r0
	add r6, r12, r0
	jal r31, test_ashr
	beq r11, r12, .LBB23_12
	jal r0, .LBB23_23
.LBB23_12:
	addi r4, r0, 287
	add r3, r13, r0
	add r5, r13, r0
	add r6, r4, r0
	jal r31, test_eq
	beq r1, r11, .LBB23_13
	jal r0, .LBB23_23
.LBB23_13:
	addi r5, r15, 1228
	addi r4, r0, 287
	add r3, r13, r0
	add r6, r4, r0
	jal r31, test_ne
	beq r1, r11, .LBB23_14
	jal r0, .LBB23_23
.LBB23_14:
	addi r3, r0, -1
	addi r6, r0, 0
	add r4, r3, r0
	add r5, r11, r0
	jal r31, test_slt
	beq r1, r11, .LBB23_15
	jal r0, .LBB23_23
.LBB23_15:
	addi r4, r0, 0
	addi r5, r0, -1
	add r3, r11, r0
	add r6, r5, r0
	jal r31, test_ult
	beq r1, r11, .LBB23_16
	jal r0, .LBB23_23
.LBB23_16:
	addi r4, r0, 0
	addi r5, r0, -1
	add r3, r11, r0
	add r6, r5, r0
	jal r31, test_sgt
	beq r1, r11, .LBB23_17
	jal r0, .LBB23_23
.LBB23_17:
	addi r3, r0, -1
	addi r6, r0, 0
	add r4, r3, r0
	add r5, r11, r0
	jal r31, test_ugt
	beq r1, r11, .LBB23_18
	jal r0, .LBB23_23
.LBB23_18:
	addi r3, r0, 100
	addi r4, r0, 0
	add r5, r3, r0
	add r6, r4, r0
	jal r31, test_sle
	beq r1, r11, .LBB23_19
	jal r0, .LBB23_23
.LBB23_19:
	addi r3, r0, 100
	addi r4, r0, 0
	add r5, r3, r0
	add r6, r4, r0
	jal r31, test_sge
	beq r1, r11, .LBB23_20
	jal r0, .LBB23_23
.LBB23_20:
	addi r1, r0, 0
.LBB23_21:
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
.Lfunc_end23:
	.size	main, .Lfunc_end23-main
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
