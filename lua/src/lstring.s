	.file	"lstring.c"
	.text
	.hidden	luaS_eqlngstr                   # -- Begin function luaS_eqlngstr
	.globl	luaS_eqlngstr
	.p2align	2
	.type	luaS_eqlngstr,@function
luaS_eqlngstr:                          # @luaS_eqlngstr
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	beq r3, r4, .LBB0_3
.LBB0_1:
	ldw r5, r3+12
	ldw r1, r4+12
	bne r5, r1, .LBB0_4
.LBB0_2:
	addi r3, r3, 16
	addi r4, r4, 16
	jal r31, memcmp
	addi r3, r0, 0
	seq r1, r1, r3
	jal r0, .LBB0_5
.LBB0_3:
	addi r1, r0, 1
	jal r0, .LBB0_5
.LBB0_4:
	addi r1, r0, 0
.LBB0_5:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaS_eqlngstr, .Lfunc_end0-luaS_eqlngstr
                                        # -- End function
	.hidden	luaS_hash                       # -- Begin function luaS_hash
	.globl	luaS_hash
	.p2align	2
	.type	luaS_hash,@function
luaS_hash:                              # @luaS_hash
# %bb.0:
	xor r1, r5, r4
	addi r5, r0, 0
	beq r4, r5, .LBB1_3
.LBB1_1:
	addi r3, r3, -1
.LBB1_2:
	slli r6, r1, 5
	srli r7, r1, 2
	add r6, r6, r7
	add r7, r3, r4
	ldbu r7, r7+0
	add r6, r6, r7
	xor r1, r6, r1
	addi r4, r4, -1
	bne r4, r5, .LBB1_2
.LBB1_3:
	jalr r0, r31, 0
.Lfunc_end1:
	.size	luaS_hash, .Lfunc_end1-luaS_hash
                                        # -- End function
	.hidden	luaS_hashlongstr                # -- Begin function luaS_hashlongstr
	.globl	luaS_hashlongstr
	.p2align	2
	.type	luaS_hashlongstr,@function
luaS_hashlongstr:                       # @luaS_hashlongstr
# %bb.0:
	ldbu r4, r3+6
	addi r1, r0, 0
	bne r4, r1, .LBB2_5
.LBB2_1:
	ldw r5, r3+12
	ldw r4, r3+8
	xor r4, r4, r5
	beq r5, r1, .LBB2_4
.LBB2_2:
	addi r6, r3, 15
.LBB2_3:
	slli r7, r4, 5
	srli r8, r4, 2
	add r7, r7, r8
	add r8, r6, r5
	ldbu r8, r8+0
	add r7, r7, r8
	xor r4, r7, r4
	addi r5, r5, -1
	bne r5, r1, .LBB2_3
.LBB2_4:
	stw r3+8, r4
	addi r1, r0, 1
	stb r3+6, r1
.LBB2_5:
	ldw r1, r3+8
	jalr r0, r31, 0
.Lfunc_end2:
	.size	luaS_hashlongstr, .Lfunc_end2-luaS_hashlongstr
                                        # -- End function
	.hidden	luaS_resize                     # -- Begin function luaS_resize
	.globl	luaS_resize
	.p2align	2
	.type	luaS_resize,@function
luaS_resize:                            # @luaS_resize
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
	add r12, r4, r0
	ldw r14, r3+16
	ldw r11, r14+32
	bge r4, r11, .LBB3_6
.LBB3_1:
	addi r1, r0, 1
	blt r11, r1, .LBB3_6
.LBB3_2:
	ldw r1, r14+24
	addi r4, r12, -1
	addi r5, r0, 0
	add r6, r5, r0
	jal r0, .LBB3_4
.LBB3_3:
	addi r6, r6, 1
	beq r6, r11, .LBB3_6
.LBB3_4:
	slli r7, r6, 2
	add r8, r1, r7
	ldw r7, r8+0
	stw r8+0, r5
	beq r7, r5, .LBB3_3
.LBB3_5:
	ldw r8, r7+12
	ldw r9, r7+8
	and r9, r9, r4
	slli r9, r9, 2
	add r9, r1, r9
	ldw r10, r9+0
	stw r7+12, r10
	stw r9+0, r7
	add r7, r8, r0
	bne r8, r5, .LBB3_5
	jal r0, .LBB3_3
.LBB3_6:
	ldw r4, r14+24
	slli r13, r11, 2
	slli r6, r12, 2
	add r5, r13, r0
	jal r31, luaM_realloc_
	addi r3, r0, 0
	beq r1, r3, .LBB3_15
.LBB3_7:
	stw r14+24, r1
	stw r14+32, r12
	ble r12, r11, .LBB3_17
.LBB3_8:
	add r4, r1, r13
	add r5, r12, r0
.LBB3_9:
	stw r4+0, r3
	addi r5, r5, -1
	addi r4, r4, 4
	bne r11, r5, .LBB3_9
.LBB3_10:
	addi r4, r0, 1
	blt r11, r4, .LBB3_17
.LBB3_11:
	addi r4, r12, -1
	add r5, r3, r0
	jal r0, .LBB3_13
.LBB3_12:
	addi r5, r5, 1
	beq r5, r11, .LBB3_17
.LBB3_13:
	slli r6, r5, 2
	add r7, r1, r6
	ldw r6, r7+0
	stw r7+0, r3
	beq r6, r3, .LBB3_12
.LBB3_14:
	ldw r7, r6+12
	ldw r8, r6+8
	and r8, r8, r4
	slli r8, r8, 2
	add r8, r1, r8
	ldw r9, r8+0
	stw r6+12, r9
	stw r8+0, r6
	add r6, r7, r0
	bne r7, r3, .LBB3_14
	jal r0, .LBB3_12
.LBB3_15:
	bge r12, r11, .LBB3_17
.LBB3_16:
	ldw r3, r14+24
	add r4, r12, r0
	add r5, r11, r0
	jal r31, tablerehash
.LBB3_17:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end3:
	.size	luaS_resize, .Lfunc_end3-luaS_resize
                                        # -- End function
	.p2align	2                               # -- Begin function tablerehash
	.type	tablerehash,@function
tablerehash:                            # @tablerehash
# %bb.0:
	ble r5, r4, .LBB4_3
.LBB4_1:
	sub r1, r5, r4
	slli r6, r4, 2
	add r6, r3, r6
	addi r7, r0, 0
.LBB4_2:
	stw r6+0, r7
	addi r1, r1, -1
	addi r6, r6, 4
	bne r1, r7, .LBB4_2
.LBB4_3:
	addi r1, r0, 1
	blt r4, r1, .LBB4_8
.LBB4_4:
	addi r1, r0, 0
	addi r5, r5, -1
	add r6, r1, r0
	jal r0, .LBB4_6
.LBB4_5:
	addi r6, r6, 1
	beq r6, r4, .LBB4_8
.LBB4_6:
	slli r7, r6, 2
	add r8, r3, r7
	ldw r7, r8+0
	stw r8+0, r1
	beq r7, r1, .LBB4_5
.LBB4_7:
	ldw r8, r7+12
	ldw r9, r7+8
	and r9, r9, r5
	slli r9, r9, 2
	add r9, r3, r9
	ldw r10, r9+0
	stw r7+12, r10
	stw r9+0, r7
	add r7, r8, r0
	bne r8, r1, .LBB4_7
	jal r0, .LBB4_5
.LBB4_8:
	jalr r0, r31, 0
.Lfunc_end4:
	.size	tablerehash, .Lfunc_end4-tablerehash
                                        # -- End function
	.hidden	luaS_clearcache                 # -- Begin function luaS_clearcache
	.globl	luaS_clearcache
	.p2align	2
	.type	luaS_clearcache,@function
luaS_clearcache:                        # @luaS_clearcache
# %bb.0:
	addi r1, r3, 296
	addi r4, r0, 0
	addi r5, r0, 4
	addi r6, r0, 53
	add r7, r4, r0
	jal r0, .LBB5_2
.LBB5_1:
	addi r7, r7, 1
	addi r1, r1, 8
	beq r7, r6, .LBB5_6
.LBB5_2:
	add r8, r4, r0
	jal r0, .LBB5_4
.LBB5_3:
	addi r8, r8, 4
	bne r8, r5, .LBB5_1
.LBB5_4:
	add r9, r1, r8
	ldw r10, r9+0
	ldbu r10, r10+5
	andi r10, r10, 24
	beq r10, r4, .LBB5_3
.LBB5_5:
	ldw r10, r3+156
	stw r9+0, r10
	jal r0, .LBB5_3
.LBB5_6:
	jalr r0, r31, 0
.Lfunc_end5:
	.size	luaS_clearcache, .Lfunc_end5-luaS_clearcache
                                        # -- End function
	.hidden	luaS_init                       # -- Begin function luaS_init
	.globl	luaS_init
	.p2align	2
	.type	luaS_init,@function
luaS_init:                              # @luaS_init
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
	add r11, r3, r0
	ldw r14, r3+16
	addi r12, r0, 512
	addi r13, r0, 0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, luaM_malloc_
	stw r14+24, r1
	add r3, r13, r0
.LBB6_1:
	add r4, r1, r3
	stw r4+0, r13
	addi r3, r3, 4
	bne r3, r12, .LBB6_1
.LBB6_2:
	addi r1, r0, 128
	stw r14+32, r1
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	addi r5, r0, 17
	add r3, r11, r0
	jal r31, luaS_newlstr
	stw r14+156, r1
	add r3, r11, r0
	add r4, r1, r0
	jal r31, luaC_fix
	addi r1, r14, 296
	addi r3, r0, 0
	addi r4, r0, 4
	addi r5, r0, 53
	add r6, r3, r0
.LBB6_3:
	add r7, r3, r0
.LBB6_4:
	ldw r8, r14+156
	add r9, r1, r7
	stw r9+0, r8
	addi r7, r7, 4
	beq r7, r4, .LBB6_4
.LBB6_5:
	addi r6, r6, 1
	addi r1, r1, 8
	bne r6, r5, .LBB6_3
.LBB6_6:
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
	.size	luaS_init, .Lfunc_end6-luaS_init
                                        # -- End function
	.hidden	luaS_newlstr                    # -- Begin function luaS_newlstr
	.globl	luaS_newlstr
	.p2align	2
	.type	luaS_newlstr,@function
luaS_newlstr:                           # @luaS_newlstr
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
	addi r1, r0, 40
	bgtu r5, r1, .LBB7_16
.LBB7_1:
	ldw r15, r13+16
	ldw r1, r15+60
	xor r17, r1, r11
	addi r16, r0, 0
	beq r11, r16, .LBB7_4
.LBB7_2:
	addi r1, r12, -1
	add r3, r11, r0
.LBB7_3:
	slli r4, r17, 5
	srli r5, r17, 2
	add r4, r4, r5
	add r5, r1, r3
	ldbu r5, r5+0
	add r4, r4, r5
	xor r17, r4, r17
	addi r3, r3, -1
	bne r3, r16, .LBB7_3
.LBB7_4:
	ldw r1, r15+24
	ldw r3, r15+32
	addi r3, r3, -1
	and r3, r3, r17
	slli r3, r3, 2
	add r18, r1, r3
	ldw r14, r18+0
	bne r14, r16, .LBB7_12
.LBB7_5:
	ldw r1, r15+28
	ldw r3, r15+32
	blt r1, r3, .LBB7_10
.LBB7_6:
	lui r3, 524288
	addi r14, r3, -1
	beq r1, r14, .LBB7_20
.LBB7_7:
	ldw r1, r15+32
	lui r3, 131072
	addi r3, r3, -1
	bgt r1, r3, .LBB7_9
.LBB7_8:
	slli r4, r1, 1
	add r3, r13, r0
	jal r31, luaS_resize
.LBB7_9:
	ldw r1, r15+24
	ldw r3, r15+32
	addi r3, r3, -1
	and r3, r3, r17
	slli r3, r3, 2
	add r18, r1, r3
.LBB7_10:
	addi r5, r11, 17
	addi r4, r0, 4
	add r3, r13, r0
	jal r31, luaC_newobj
	add r14, r1, r0
	stw r1+8, r17
	stb r1+6, r16
	addi r3, r1, 16
	add r1, r3, r11
	stb r1+0, r16
	stb r14+7, r11
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r18+0
	stw r14+12, r1
	stw r18+0, r14
	ldw r1, r15+28
	addi r1, r1, 1
	stw r15+28, r1
	jal r0, .LBB7_18
.LBB7_11:
	ldw r14, r14+12
	beq r14, r16, .LBB7_5
.LBB7_12:
	ldbu r1, r14+7
	bne r11, r1, .LBB7_11
.LBB7_13:
	addi r4, r14, 16
	add r3, r12, r0
	add r5, r11, r0
	jal r31, memcmp
	bne r1, r16, .LBB7_11
.LBB7_14:
	ldbu r1, r14+5
	ldbu r3, r15+64
	xori r3, r3, 24
	and r3, r3, r1
	andi r3, r3, 255
	beq r3, r16, .LBB7_18
.LBB7_15:
	xori r1, r1, 24
	stb r14+5, r1
	jal r0, .LBB7_18
.LBB7_16:
	lui r1, 524288
	addi r1, r1, -21
	bgeu r11, r1, .LBB7_19
.LBB7_17:
	ldw r1, r13+16
	ldw r15, r1+60
	addi r5, r11, 17
	addi r4, r0, 20
	add r3, r13, r0
	jal r31, luaC_newobj
	add r14, r1, r0
	stw r1+8, r15
	addi r1, r0, 0
	stb r14+6, r1
	addi r3, r14, 16
	add r4, r3, r11
	stb r4+0, r1
	stw r14+12, r11
	addi r1, r0, 255
	stb r14+7, r1
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memcpy
.LBB7_18:
	add r1, r14, r0
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
.LBB7_19:
	add r3, r13, r0
	jal r31, luaM_toobig
.LBB7_20:
	addi r4, r0, 1
	add r3, r13, r0
	jal r31, luaC_fullgc
	ldw r1, r15+28
	bne r1, r14, .LBB7_7
.LBB7_21:
	addi r4, r0, 4
	add r3, r13, r0
	jal r31, luaD_throw
.Lfunc_end7:
	.size	luaS_newlstr, .Lfunc_end7-luaS_newlstr
                                        # -- End function
	.hidden	luaS_createlngstrobj            # -- Begin function luaS_createlngstrobj
	.globl	luaS_createlngstrobj
	.p2align	2
	.type	luaS_createlngstrobj,@function
luaS_createlngstrobj:                   # @luaS_createlngstrobj
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r4, r0
	ldw r1, r3+16
	ldw r12, r1+60
	addi r5, r4, 17
	addi r4, r0, 20
	jal r31, luaC_newobj
	stw r1+8, r12
	addi r3, r0, 0
	stb r1+6, r3
	add r4, r1, r11
	stb r4+16, r3
	stw r1+12, r11
	addi r3, r0, 255
	stb r1+7, r3
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	luaS_createlngstrobj, .Lfunc_end8-luaS_createlngstrobj
                                        # -- End function
	.hidden	luaS_remove                     # -- Begin function luaS_remove
	.globl	luaS_remove
	.p2align	2
	.type	luaS_remove,@function
luaS_remove:                            # @luaS_remove
# %bb.0:
	ldw r1, r3+16
	ldw r3, r1+24
	ldw r5, r4+8
	ldw r6, r1+32
	addi r6, r6, -1
	and r5, r6, r5
	slli r5, r5, 2
	add r5, r3, r5
.LBB9_1:
	add r3, r5, r0
	ldw r6, r5+0
	addi r5, r6, 12
	bne r6, r4, .LBB9_1
.LBB9_2:
	ldw r4, r6+12
	stw r3+0, r4
	ldw r3, r1+28
	addi r3, r3, -1
	stw r1+28, r3
	jalr r0, r31, 0
.Lfunc_end9:
	.size	luaS_remove, .Lfunc_end9-luaS_remove
                                        # -- End function
	.hidden	luaS_new                        # -- Begin function luaS_new
	.globl	luaS_new
	.p2align	2
	.type	luaS_new,@function
luaS_new:                               # @luaS_new
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
	add r11, r4, r0
	add r12, r3, r0
	lui r1, 217629
	addi r1, r1, -77
	mulhu r1, r4, r1
	sub r3, r4, r1
	srli r3, r3, 1
	add r1, r3, r1
	srli r1, r1, 5
	addi r3, r0, 53
	mul r1, r1, r3
	sub r1, r4, r1
	ldw r3, r12+16
	slli r1, r1, 3
	add r1, r3, r1
	addi r13, r1, 296
	addi r14, r0, 0
	addi r15, r0, 4
	add r16, r14, r0
.LBB10_1:
	add r17, r13, r16
	ldw r1, r17+0
	addi r4, r1, 16
	add r3, r11, r0
	jal r31, strcmp
	beq r1, r14, .LBB10_4
.LBB10_2:
	addi r16, r16, 4
	beq r16, r15, .LBB10_1
.LBB10_3:
	ldw r1, r13+0
	stw r13+4, r1
	add r3, r11, r0
	jal r31, strlen
	add r3, r12, r0
	add r4, r11, r0
	add r5, r1, r0
	jal r31, luaS_newlstr
	stw r13+0, r1
	jal r0, .LBB10_5
.LBB10_4:
	ldw r1, r17+0
.LBB10_5:
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
.Lfunc_end10:
	.size	luaS_new, .Lfunc_end10-luaS_new
                                        # -- End function
	.hidden	luaS_newudata                   # -- Begin function luaS_newudata
	.globl	luaS_newudata
	.p2align	2
	.type	luaS_newudata,@function
luaS_newudata:                          # @luaS_newudata
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
	add r11, r5, r0
	add r12, r4, r0
	addi r13, r0, 0
	seq r1, r5, r13
	addi r4, r0, 12
	mul r4, r5, r4
	addi r4, r4, 20
	sub r1, r13, r1
	xori r5, r4, 16
	and r1, r5, r1
	xor r1, r4, r1
	lui r4, 524288
	addi r4, r4, -1
	xor r4, r1, r4
	bgtu r12, r4, .LBB11_5
.LBB11_1:
	add r5, r1, r12
	addi r4, r0, 7
	jal r31, luaC_newobj
	stw r1+8, r12
	sth r1+6, r11
	stw r1+12, r13
	addi r3, r0, 1
	blt r11, r3, .LBB11_4
.LBB11_2:
	addi r3, r1, 28
.LBB11_3:
	stb r3+0, r13
	addi r11, r11, -1
	addi r3, r3, 12
	bne r11, r13, .LBB11_3
.LBB11_4:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB11_5:
	jal r31, luaM_toobig
.Lfunc_end11:
	.size	luaS_newudata, .Lfunc_end11-luaS_newudata
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"not enough memory"
	.size	.L.str, 18

	.hidden	luaM_realloc_
	.hidden	luaM_malloc_
	.hidden	luaC_fix
	.hidden	luaM_toobig
	.hidden	luaC_newobj
	.hidden	luaC_fullgc
	.hidden	luaD_throw
	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
