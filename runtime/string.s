	.file	"string.c"
	.text
	.globl	strlen                          # -- Begin function strlen
	.p2align	2
	.type	strlen,@function
strlen:                                 # @strlen
# %bb.0:
	ldbu r2, r3+0
	addi r1, r0, 0
	beq r2, r1, .LBB0_3
.LBB0_1:
	addi r2, r3, 1
	addi r3, r0, 0
	add r1, r3, r0
.LBB0_2:
	add r4, r2, r1
	addi r1, r1, 1
	ldbu r4, r4+0
	bne r4, r3, .LBB0_2
.LBB0_3:
	jalr r0, r31, 0
.Lfunc_end0:
	.size	strlen, .Lfunc_end0-strlen
                                        # -- End function
	.globl	strcpy                          # -- Begin function strcpy
	.p2align	2
	.type	strcpy,@function
strcpy:                                 # @strcpy
# %bb.0:
	add r1, r3, r0
	addi r2, r0, 0
	add r3, r2, r0
.LBB1_1:
	add r5, r4, r3
	add r6, r1, r3
	ldbu r5, r5+0
	stb r6+0, r5
	addi r3, r3, 1
	bne r5, r2, .LBB1_1
.LBB1_2:
	jalr r0, r31, 0
.Lfunc_end1:
	.size	strcpy, .Lfunc_end1-strcpy
                                        # -- End function
	.globl	strncpy                         # -- Begin function strncpy
	.p2align	2
	.type	strncpy,@function
strncpy:                                # @strncpy
# %bb.0:
	add r1, r3, r0
	addi r6, r0, 0
	add r2, r6, r0
	beq r5, r6, .LBB2_5
.LBB2_1:
	addi r7, r1, 1
	addi r2, r0, 0
.LBB2_2:
	add r3, r7, r0
	ldbu r7, r4+0
	stb r3+-1, r7
	beq r7, r2, .LBB2_3
.LBB2_4:
	addi r4, r4, 1
	addi r5, r5, -1
	addi r7, r3, 1
	beq r5, r2, .LBB2_5
.LBB2_5:
	beq r2, r6, .LBB2_9
	beq r2, r6, .LBB2_9
.LBB2_6:
	addi r4, r0, 0
	beq r2, r4, .LBB2_9
.LBB2_7:
	add r5, r4, r0
.LBB2_8:
	add r6, r3, r5
	stb r6+0, r4
	addi r5, r5, 1
	sltu r6, r5, r2
	bne r6, r4, .LBB2_8
.LBB2_9:
	jalr r0, r31, 0
.LBB2_3:
	add r2, r5, r0
	jal r0, .LBB2_5
.Lfunc_end2:
	.size	strncpy, .Lfunc_end2-strncpy
                                        # -- End function
	.globl	strcmp                          # -- Begin function strcmp
	.p2align	2
	.type	strcmp,@function
strcmp:                                 # @strcmp
# %bb.0:
	ldbu r1, r3+0
	addi r2, r0, 0
	beq r1, r2, .LBB3_1
.LBB3_2:
	addi r3, r3, 1
	addi r2, r0, 0
.LBB3_3:
	ldbu r5, r4+0
	andi r6, r1, 255
	beq r6, r5, .LBB3_5
.LBB3_4:
	jal r0, .LBB3_7
	jal r0, .LBB3_7
.LBB3_5:
	addi r4, r4, 1
	ldbu r1, r3+0
	addi r3, r3, 1
	beq r1, r2, .LBB3_6
	jal r0, .LBB3_3
.LBB3_6:
	add r1, r2, r0
.LBB3_7:
	andi r2, r1, 255
.LBB3_8:
	ldbu r1, r4+0
	sub r1, r2, r1
	jalr r0, r31, 0
.LBB3_1:
	jal r0, .LBB3_8
	jal r0, .LBB3_8
.Lfunc_end3:
	.size	strcmp, .Lfunc_end3-strcmp
                                        # -- End function
	.globl	strncmp                         # -- Begin function strncmp
	.p2align	2
	.type	strncmp,@function
strncmp:                                # @strncmp
# %bb.0:
	addi r1, r0, 0
	beq r5, r1, .LBB4_8
.LBB4_1:
	addi r1, r0, 0
.LBB4_2:
	ldbu r2, r3+0
	beq r2, r1, .LBB4_3
.LBB4_6:
	ldbu r6, r4+0
	beq r2, r6, .LBB4_4
.LBB4_7:
	ldbu r1, r4+0
	sub r1, r2, r1
.LBB4_8:
	jalr r0, r31, 0
.LBB4_4:
	addi r5, r5, -1
	addi r3, r3, 1
	addi r4, r4, 1
	beq r5, r1, .LBB4_5
	jal r0, .LBB4_2
.LBB4_5:
	jal r0, .LBB4_8
	jalr r0, r31, 0
.LBB4_3:
	add r2, r1, r0
	jal r0, .LBB4_7
.Lfunc_end4:
	.size	strncmp, .Lfunc_end4-strncmp
                                        # -- End function
	.globl	strcat                          # -- Begin function strcat
	.p2align	2
	.type	strcat,@function
strcat:                                 # @strcat
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r12, r4, r0
	add r11, r3, r0
	jal r31, strlen
	add r2, r11, r1
	addi r1, r0, 0
.LBB5_1:
	addi r3, r12, 1
	ldbu r4, r12+0
	addi r5, r2, 1
	stb r2+0, r4
	add r12, r3, r0
	add r2, r5, r0
	bne r4, r1, .LBB5_1
.LBB5_2:
	add r1, r11, r0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end5:
	.size	strcat, .Lfunc_end5-strcat
                                        # -- End function
	.globl	strncat                         # -- Begin function strncat
	.p2align	2
	.type	strncat,@function
strncat:                                # @strncat
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, lr
	add r13, r5, r0
	add r12, r4, r0
	add r11, r3, r0
	jal r31, strlen
	add r3, r11, r1
	addi r1, r0, 0
.LBB6_1:
	add r2, r13, r0
	beq r13, r1, .LBB6_2
.LBB6_3:
	addi r13, r2, -1
	addi r5, r12, 1
	ldbu r6, r12+0
	addi r4, r3, 1
	stb r3+0, r6
	add r12, r5, r0
	add r3, r4, r0
	beq r6, r1, .LBB6_4
.LBB6_4:
	bne r2, r1, .LBB6_6
	beq r2, r1, .LBB6_5
	jal r0, .LBB6_6
.LBB6_5:
	stb r4+0, r1
.LBB6_6:
	add r1, r11, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB6_2:
	add r4, r3, r0
	jal r0, .LBB6_4
.Lfunc_end6:
	.size	strncat, .Lfunc_end6-strncat
                                        # -- End function
	.globl	strchr                          # -- Begin function strchr
	.p2align	2
	.type	strchr,@function
strchr:                                 # @strchr
# %bb.0:
	add r1, r3, r0
	ldbu r5, r3+0
	addi r2, r0, 0
	beq r5, r2, .LBB7_1
.LBB7_2:
	slli r3, r4, 24
	srai r3, r3, 24
.LBB7_3:
	slli r5, r5, 24
	srai r5, r5, 24
	beq r3, r5, .LBB7_4
.LBB7_5:
	addi r6, r1, 1
	ldbu r5, r1+1
	add r1, r6, r0
	beq r5, r2, .LBB7_6
.LBB7_6:
	ori r1, r0, 0
	seq r1, r4, r1
	sub r1, r2, r1
	and r1, r6, r1
.LBB7_7:
	jalr r0, r31, 0
.LBB7_1:
	add r6, r1, r0
	jal r0, .LBB7_6
.LBB7_4:
	jal r0, .LBB7_7
	jalr r0, r31, 0
.Lfunc_end7:
	.size	strchr, .Lfunc_end7-strchr
                                        # -- End function
	.globl	strrchr                         # -- Begin function strrchr
	.p2align	2
	.type	strrchr,@function
strrchr:                                # @strrchr
# %bb.0:
	ldbu r7, r3+0
	addi r1, r0, 0
	beq r7, r1, .LBB8_1
.LBB8_2:
	slli r2, r4, 24
	srai r5, r2, 24
	addi r6, r0, 0
	add r2, r6, r0
.LBB8_3:
	slli r7, r7, 24
	srai r7, r7, 24
	seq r7, r5, r7
	xor r8, r3, r2
	sub r7, r6, r7
	and r7, r8, r7
	xor r2, r2, r7
	addi r8, r3, 1
	ldbu r7, r3+1
	add r3, r8, r0
	bne r7, r6, .LBB8_3
	jal r0, .LBB8_4
.LBB8_1:
	add r8, r3, r0
	add r2, r1, r0
.LBB8_4:
	ori r3, r0, 0
	seq r3, r4, r3
	xor r4, r8, r2
	sub r1, r1, r3
	and r1, r4, r1
	xor r1, r2, r1
	jalr r0, r31, 0
.Lfunc_end8:
	.size	strrchr, .Lfunc_end8-strrchr
                                        # -- End function
	.globl	strstr                          # -- Begin function strstr
	.p2align	2
	.type	strstr,@function
strstr:                                 # @strstr
# %bb.0:
	ldbu r1, r4+0
	addi r2, r0, 0
	beq r1, r2, .LBB9_1
.LBB9_2:
	ori r5, r0, 0
                                        # implicit-def: $r1
.LBB9_3:
	ldbu r6, r3+0
	beq r6, r2, .LBB9_4
.LBB9_5:
	ldbu r7, r3+0
	add r8, r4, r0
	beq r7, r2, .LBB9_10
.LBB9_6:
	add r6, r2, r0
.LBB9_7:
	add r8, r4, r6
	ldbu r9, r8+0
	andi r7, r7, 255
	beq r7, r9, .LBB9_8
	jal r0, .LBB9_10
.LBB9_8:
	add r7, r3, r6
	ldbu r7, r7+1
	addi r6, r6, 1
	beq r7, r2, .LBB9_9
	jal r0, .LBB9_7
.LBB9_9:
	add r8, r4, r6
.LBB9_10:
	ldbu r6, r8+0
	sne r7, r6, r5
	xor r1, r1, r3
	sub r7, r2, r7
	and r1, r1, r7
	xor r1, r3, r1
	addi r3, r3, 1
	beq r6, r2, .LBB9_11
.LBB9_11:
	jalr r0, r31, 0
.LBB9_1:
	add r1, r3, r0
	jalr r0, r31, 0
.LBB9_4:
	add r1, r2, r0
	jalr r0, r31, 0
.Lfunc_end9:
	.size	strstr, .Lfunc_end9-strstr
                                        # -- End function
	.globl	memset                          # -- Begin function memset
	.p2align	2
	.type	memset,@function
memset:                                 # @memset
# %bb.0:
	add r1, r3, r0
	addi r2, r0, 0
	beq r5, r2, .LBB10_3
.LBB10_1:
	add r3, r1, r0
.LBB10_2:
	addi r5, r5, -1
	addi r6, r3, 1
	stb r3+0, r4
	add r3, r6, r0
	bne r5, r2, .LBB10_2
.LBB10_3:
	jalr r0, r31, 0
.Lfunc_end10:
	.size	memset, .Lfunc_end10-memset
                                        # -- End function
	.globl	memcmp                          # -- Begin function memcmp
	.p2align	2
	.type	memcmp,@function
memcmp:                                 # @memcmp
# %bb.0:
	addi r1, r0, 0
	beq r5, r1, .LBB11_4
.LBB11_1:
	addi r1, r0, 0
.LBB11_2:
	ldbu r2, r3+0
	ldbu r6, r4+0
	beq r2, r6, .LBB11_5
	jal r0, .LBB11_3
.LBB11_5:
	addi r5, r5, -1
	addi r3, r3, 1
	addi r4, r4, 1
	beq r5, r1, .LBB11_4
	jal r0, .LBB11_4
.LBB11_3:
	sub r1, r2, r6
.LBB11_4:
	jalr r0, r31, 0
.Lfunc_end11:
	.size	memcmp, .Lfunc_end11-memcmp
                                        # -- End function
	.globl	memchr                          # -- Begin function memchr
	.p2align	2
	.type	memchr,@function
memchr:                                 # @memchr
# %bb.0:
	addi r1, r0, 0
	beq r5, r1, .LBB12_4
.LBB12_1:
	andi r2, r4, 255
	addi r1, r0, 0
.LBB12_2:
	ldbu r4, r3+0
	beq r4, r2, .LBB12_3
.LBB12_5:
	addi r5, r5, -1
	addi r3, r3, 1
	beq r5, r1, .LBB12_4
	jal r0, .LBB12_4
.LBB12_3:
	add r1, r3, r0
.LBB12_4:
	jalr r0, r31, 0
.Lfunc_end12:
	.size	memchr, .Lfunc_end12-memchr
                                        # -- End function
	.globl	memmove                         # -- Begin function memmove
	.p2align	2
	.type	memmove,@function
memmove:                                # @memmove
# %bb.0:
	add r1, r3, r0
	sgeu r3, r3, r4
	addi r2, r0, 0
	bne r3, r2, .LBB13_4
.LBB13_1:
	beq r5, r2, .LBB13_8
	beq r5, r2, .LBB13_8
.LBB13_2:
	add r3, r1, r0
.LBB13_3:
	addi r5, r5, -1
	addi r6, r4, 1
	ldbu r4, r4+0
	addi r7, r3, 1
	stb r3+0, r4
	add r4, r6, r0
	add r3, r7, r0
	beq r5, r2, .LBB13_8
	jal r0, .LBB13_3
.LBB13_4:
	sleu r3, r1, r4
	beq r3, r2, .LBB13_5
	jal r0, .LBB13_8
.LBB13_5:
	beq r5, r2, .LBB13_8
	beq r5, r2, .LBB13_8
.LBB13_6:
	addi r3, r4, -1
	addi r4, r1, -1
.LBB13_7:
	addi r6, r5, -1
	add r7, r3, r5
	ldbu r7, r7+0
	add r5, r4, r5
	stb r5+0, r7
	add r5, r6, r0
	bne r6, r2, .LBB13_7
.LBB13_8:
	jalr r0, r31, 0
.Lfunc_end13:
	.size	memmove, .Lfunc_end13-memmove
                                        # -- End function
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
