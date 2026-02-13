	.file	"lzio.c"
	.text
	.hidden	luaZ_fill                       # -- Begin function luaZ_fill
	.globl	luaZ_fill
	.p2align	2
	.type	luaZ_fill,@function
luaZ_fill:                              # @luaZ_fill
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
	ldw r3, r3+16
	ldw r1, r11+8
	ldw r4, r11+12
	addi r12, fp, -16
	add r5, r12, r0
	jalr lr, r1, 0
	addi r3, r0, -1
	addi r5, r0, 0
	beq r1, r5, .LBB0_3
.LBB0_1:
	ldw r4, r12+0
	beq r4, r5, .LBB0_3
.LBB0_2:
	addi r3, r4, -1
	stw r11+0, r3
	addi r3, r1, 1
	stw r11+4, r3
	ldbu r3, r1+0
.LBB0_3:
	add r1, r3, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaZ_fill, .Lfunc_end0-luaZ_fill
                                        # -- End function
	.hidden	luaZ_init                       # -- Begin function luaZ_init
	.globl	luaZ_init
	.p2align	2
	.type	luaZ_init,@function
luaZ_init:                              # @luaZ_init
# %bb.0:
	stw r4+16, r3
	stw r4+8, r5
	stw r4+12, r6
	addi r1, r0, 0
	stw r4+0, r1
	stw r4+4, r1
	jalr r0, r31, 0
.Lfunc_end1:
	.size	luaZ_init, .Lfunc_end1-luaZ_init
                                        # -- End function
	.hidden	luaZ_read                       # -- Begin function luaZ_read
	.globl	luaZ_read
	.p2align	2
	.type	luaZ_read,@function
luaZ_read:                              # @luaZ_read
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
	stw fp+-36, lr
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	addi r17, r0, 0
	addi r14, fp, -40
	addi r18, r0, 1
                                        # implicit-def: $r15
	jal r0, .LBB2_3
.LBB2_1:
	addi r4, r3, -1
	stw r13+0, r4
	addi r4, r1, 1
	stw r13+4, r4
	stw r13+0, r3
	stw r13+4, r1
.LBB2_2:
	ldw r1, r13+0
	xor r3, r11, r1
	sltu r4, r11, r1
	sub r4, r17, r4
	and r3, r3, r4
	xor r16, r1, r3
	ldw r4, r13+4
	add r3, r12, r0
	add r5, r16, r0
	jal r31, memcpy
	ldw r1, r13+0
	sub r1, r1, r16
	stw r13+0, r1
	ldw r1, r13+4
	add r1, r1, r16
	stw r13+4, r1
	add r12, r12, r16
	sub r11, r11, r16
	add r1, r18, r0
	beq r1, r17, .LBB2_9
.LBB2_3:
	beq r11, r17, .LBB2_8
.LBB2_4:
	ldw r1, r13+0
	bne r1, r17, .LBB2_2
.LBB2_5:
	ldw r3, r13+16
	ldw r1, r13+8
	ldw r4, r13+12
	add r5, r14, r0
	jalr lr, r1, 0
	beq r1, r17, .LBB2_7
.LBB2_6:
	ldw r3, r14+0
	bne r3, r17, .LBB2_1
.LBB2_7:
	add r15, r11, r0
	add r1, r17, r0
	bne r1, r17, .LBB2_3
	jal r0, .LBB2_9
.LBB2_8:
	add r15, r17, r0
.LBB2_9:
	add r1, r15, r0
	ldw lr, fp+-36
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
.Lfunc_end2:
	.size	luaZ_read, .Lfunc_end2-luaZ_read
                                        # -- End function
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
