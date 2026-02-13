	.file	"lmem.c"
	.text
	.hidden	luaM_growaux_                   # -- Begin function luaM_growaux_
	.globl	luaM_growaux_
	.p2align	2
	.type	luaM_growaux_,@function
luaM_growaux_:                          # @luaM_growaux_
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
	ldw r1, r6+0
	blt r5, r1, .LBB0_10
.LBB0_1:
	add r13, r8, r0
	add r12, r6, r0
	add r11, r3, r0
	srli r3, r8, 31
	add r3, r8, r3
	srai r3, r3, 1
	bge r1, r3, .LBB0_3
.LBB0_2:
	slli r3, r1, 1
	addi r5, r0, 4
	sgt r5, r3, r5
	addi r6, r0, 0
	sub r5, r6, r5
	xori r3, r3, 4
	and r3, r3, r5
	xori r13, r3, 4
	jal r0, .LBB0_4
.LBB0_3:
	bge r1, r13, .LBB0_13
.LBB0_4:
	addi r16, r0, 0
	mul r15, r1, r7
	mul r14, r13, r7
	ldw r17, r11+16
	ldw r1, r17+0
	ldw r3, r17+4
	add r18, r4, r0
	add r5, r15, r0
	add r6, r14, r0
	jalr lr, r1, 0
	beq r14, r16, .LBB0_6
.LBB0_5:
	beq r1, r16, .LBB0_11
.LBB0_6:
	ldw r3, r17+12
	sub r4, r14, r15
	add r3, r4, r3
	stw r17+12, r3
	add r4, r1, r0
.LBB0_7:
	beq r14, r16, .LBB0_9
.LBB0_8:
	beq r4, r16, .LBB0_12
.LBB0_9:
	stw r12+0, r13
.LBB0_10:
	add r1, r4, r0
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
.LBB0_11:
	add r4, r18, r0
	add r3, r11, r0
	add r5, r15, r0
	add r6, r14, r0
	jal r31, tryagain
	add r4, r16, r0
	bne r1, r16, .LBB0_6
	jal r0, .LBB0_7
.LBB0_12:
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, luaD_throw
.LBB0_13:
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	add r3, r11, r0
	add r5, r9, r0
	add r6, r13, r0
	jal r31, luaG_runerror
.Lfunc_end0:
	.size	luaM_growaux_, .Lfunc_end0-luaM_growaux_
                                        # -- End function
	.hidden	luaM_saferealloc_               # -- Begin function luaM_saferealloc_
	.globl	luaM_saferealloc_
	.p2align	2
	.type	luaM_saferealloc_,@function
luaM_saferealloc_:                      # @luaM_saferealloc_
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
	add r12, r6, r0
	add r13, r5, r0
	add r14, r4, r0
	add r11, r3, r0
	ldw r15, r3+16
	ldw r1, r15+0
	ldw r3, r15+4
	jalr lr, r1, 0
	addi r16, r0, 0
	beq r12, r16, .LBB1_2
.LBB1_1:
	beq r1, r16, .LBB1_6
.LBB1_2:
	ldw r3, r15+12
	sub r4, r12, r13
	add r3, r4, r3
	stw r15+12, r3
	add r3, r1, r0
.LBB1_3:
	beq r12, r16, .LBB1_5
.LBB1_4:
	beq r3, r16, .LBB1_7
.LBB1_5:
	add r1, r3, r0
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
.LBB1_6:
	add r3, r11, r0
	add r4, r14, r0
	add r5, r13, r0
	add r6, r12, r0
	jal r31, tryagain
	add r3, r16, r0
	bne r1, r16, .LBB1_2
	jal r0, .LBB1_3
.LBB1_7:
	addi r4, r0, 4
	add r3, r11, r0
	jal r31, luaD_throw
.Lfunc_end1:
	.size	luaM_saferealloc_, .Lfunc_end1-luaM_saferealloc_
                                        # -- End function
	.hidden	luaM_shrinkvector_              # -- Begin function luaM_shrinkvector_
	.globl	luaM_shrinkvector_
	.p2align	2
	.type	luaM_shrinkvector_,@function
luaM_shrinkvector_:                     # @luaM_shrinkvector_
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
	add r11, r6, r0
	add r13, r5, r0
	add r16, r4, r0
	add r12, r3, r0
	ldw r1, r5+0
	mul r15, r1, r7
	mul r14, r7, r6
	ldw r17, r3+16
	ldw r1, r17+0
	ldw r3, r17+4
	add r5, r15, r0
	add r6, r14, r0
	jalr lr, r1, 0
	addi r18, r0, 0
	beq r14, r18, .LBB2_2
.LBB2_1:
	beq r1, r18, .LBB2_6
.LBB2_2:
	ldw r3, r17+12
	sub r4, r14, r15
	add r3, r4, r3
	stw r17+12, r3
	add r3, r1, r0
.LBB2_3:
	beq r14, r18, .LBB2_5
.LBB2_4:
	beq r3, r18, .LBB2_7
.LBB2_5:
	stw r13+0, r11
	add r1, r3, r0
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
.LBB2_6:
	add r3, r12, r0
	add r4, r16, r0
	add r5, r15, r0
	add r6, r14, r0
	jal r31, tryagain
	add r3, r18, r0
	bne r1, r18, .LBB2_2
	jal r0, .LBB2_3
.LBB2_7:
	addi r4, r0, 4
	add r3, r12, r0
	jal r31, luaD_throw
.Lfunc_end2:
	.size	luaM_shrinkvector_, .Lfunc_end2-luaM_shrinkvector_
                                        # -- End function
	.hidden	luaM_toobig                     # -- Begin function luaM_toobig
	.globl	luaM_toobig
	.p2align	2
	.type	luaM_toobig,@function
luaM_toobig:                            # @luaM_toobig
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, luaG_runerror
.Lfunc_end3:
	.size	luaM_toobig, .Lfunc_end3-luaM_toobig
                                        # -- End function
	.hidden	luaM_free_                      # -- Begin function luaM_free_
	.globl	luaM_free_
	.p2align	2
	.type	luaM_free_,@function
luaM_free_:                             # @luaM_free_
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r5, r0
	ldw r12, r3+16
	ldw r1, r12+0
	ldw r3, r12+4
	addi r6, r0, 0
	jalr lr, r1, 0
	ldw r1, r12+12
	sub r1, r1, r11
	stw r12+12, r1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	luaM_free_, .Lfunc_end4-luaM_free_
                                        # -- End function
	.hidden	luaM_realloc_                   # -- Begin function luaM_realloc_
	.globl	luaM_realloc_
	.p2align	2
	.type	luaM_realloc_,@function
luaM_realloc_:                          # @luaM_realloc_
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
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	add r15, r3, r0
	ldw r16, r3+16
	ldw r1, r16+0
	ldw r3, r16+4
	jalr lr, r1, 0
	addi r14, r0, 0
	beq r11, r14, .LBB5_2
.LBB5_1:
	beq r1, r14, .LBB5_4
.LBB5_2:
	ldw r3, r16+12
	sub r4, r11, r12
	add r3, r4, r3
	stw r16+12, r3
	add r14, r1, r0
.LBB5_3:
	add r1, r14, r0
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
.LBB5_4:
	add r3, r15, r0
	add r4, r13, r0
	add r5, r12, r0
	add r6, r11, r0
	jal r31, tryagain
	bne r1, r14, .LBB5_2
	jal r0, .LBB5_3
.Lfunc_end5:
	.size	luaM_realloc_, .Lfunc_end5-luaM_realloc_
                                        # -- End function
	.p2align	2                               # -- Begin function tryagain
	.type	tryagain,@function
tryagain:                               # @tryagain
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
	ldw r14, r3+16
	ldbu r1, r14+56
	andi r7, r1, 15
	addi r1, r0, 0
	bne r7, r1, .LBB6_3
.LBB6_1:
	ldbu r7, r14+67
	addi r1, r0, 0
	bne r7, r1, .LBB6_3
.LBB6_2:
	addi r1, r0, 1
	add r11, r4, r0
	add r4, r1, r0
	add r12, r6, r0
	add r13, r5, r0
	jal r31, luaC_fullgc
	ldw r1, r14+0
	ldw r3, r14+4
	add r4, r11, r0
	add r5, r13, r0
	add r6, r12, r0
	jalr lr, r1, 0
.LBB6_3:
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
	.size	tryagain, .Lfunc_end6-tryagain
                                        # -- End function
	.hidden	luaM_malloc_                    # -- Begin function luaM_malloc_
	.globl	luaM_malloc_
	.p2align	2
	.type	luaM_malloc_,@function
luaM_malloc_:                           # @luaM_malloc_
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
	addi r1, r0, 0
	beq r4, r1, .LBB7_3
.LBB7_1:
	add r11, r4, r0
	add r12, r3, r0
	ldw r15, r3+16
	ldw r1, r15+0
	ldw r3, r15+4
	addi r13, r0, 0
	add r4, r13, r0
	add r14, r5, r0
	add r6, r11, r0
	jalr lr, r1, 0
	beq r1, r13, .LBB7_4
.LBB7_2:
	ldw r3, r15+12
	add r3, r3, r11
	stw r15+12, r3
.LBB7_3:
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
.LBB7_4:
	add r3, r12, r0
	add r4, r13, r0
	add r5, r14, r0
	add r6, r11, r0
	jal r31, tryagain
	bne r1, r13, .LBB7_2
.LBB7_5:
	addi r4, r0, 4
	add r3, r12, r0
	jal r31, luaD_throw
.Lfunc_end7:
	.size	luaM_malloc_, .Lfunc_end7-luaM_malloc_
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"too many %s (limit is %d)"
	.size	.L.str, 26

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"memory allocation error: block too big"
	.size	.L.str.1, 39

	.hidden	luaG_runerror
	.hidden	luaD_throw
	.hidden	luaC_fullgc
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
