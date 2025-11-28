	.file	"stdio.c"
	.text
	.globl	putchar                         # -- Begin function putchar
	.p2align	2
	.type	putchar,@function
putchar:                                # @putchar
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	stb r1+0, r3
	addi r3, r0, 1
	addi r5, r0, 0
	add r4, r3, r0
	add r6, r5, r0
	jal r31, s32_mmio_request
	add r1, r11, r0
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	putchar, .Lfunc_end0-putchar
                                        # -- End function
	.globl	getchar                         # -- Begin function getchar
	.p2align	2
	.type	getchar,@function
getchar:                                # @getchar
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r3, r0, 2
	addi r4, r0, 0
	add r5, r4, r0
	add r6, r4, r0
	jal r31, s32_mmio_request
	add r3, r1, r0
	addi r1, r0, -1
	beq r3, r1, .LBB1_2
.LBB1_1:
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	ldbu r1, r1+0
.LBB1_2:
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	getchar, .Lfunc_end1-getchar
                                        # -- End function
	.globl	puts                            # -- Begin function puts
	.p2align	2
	.type	puts,@function
puts:                                   # @puts
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
	ldbu r1, r3+0
	addi r11, r0, 0
	lui r13, %hi(__mmio_base+16384)
	addi r13, r13, %lo(__mmio_base+16384)
	beq r1, r11, .LBB2_3
.LBB2_1:
	addi r14, r3, 1
	addi r12, r0, 1
.LBB2_2:
	stb r13+0, r1
	add r3, r12, r0
	add r4, r12, r0
	add r5, r11, r0
	add r6, r11, r0
	jal r31, s32_mmio_request
	ldbu r1, r14+0
	addi r14, r14, 1
	bne r1, r11, .LBB2_2
.LBB2_3:
	addi r1, r0, 10
	stb r13+0, r1
	addi r3, r0, 1
	addi r11, r0, 0
	add r4, r3, r0
	add r5, r11, r0
	add r6, r11, r0
	jal r31, s32_mmio_request
	add r1, r11, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end2:
	.size	puts, .Lfunc_end2-puts
                                        # -- End function
	.globl	fopen                           # -- Begin function fopen
	.p2align	2
	.type	fopen,@function
fopen:                                  # @fopen
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
	add r14, r4, r0
	add r12, r3, r0
	addi r3, r0, 32
	jal r31, malloc
	addi r15, r0, 0
	beq r1, r15, .LBB3_10
.LBB3_1:
	add r11, r1, r0
	addi r4, r0, 114
	add r3, r14, r0
	jal r31, strchr
	ori r3, r0, 0
	sne r13, r1, r3
	stw r11+4, r13
	addi r4, r0, 119
	add r3, r14, r0
	jal r31, strchr
	addi r15, r0, 0
	beq r1, r15, .LBB3_3
.LBB3_2:
	ori  r13, r13, 26
	stw r11+4, r13
.LBB3_3:
	addi r4, r0, 97
	add r3, r14, r0
	jal r31, strchr
	beq r1, r15, .LBB3_5
.LBB3_4:
	ori  r13, r13, 6
	stw r11+4, r13
.LBB3_5:
	addi r4, r0, 43
	add r3, r14, r0
	jal r31, strchr
	beq r1, r15, .LBB3_7
.LBB3_6:
	ori  r13, r13, 3
	stw r11+4, r13
.LBB3_7:
	add r3, r12, r0
	jal r31, strlen
	addi r14, r1, 1
	lui r3, %hi(__mmio_base+16384)
	addi r3, r3, %lo(__mmio_base+16384)
	add r4, r12, r0
	add r5, r14, r0
	jal r31, memcpy
	addi r3, r0, 5
	addi r15, r0, 0
	add r4, r14, r0
	add r5, r15, r0
	add r6, r13, r0
	jal r31, s32_mmio_request
	stw r11+0, r1
	addi r3, r0, -1
	ble r1, r3, .LBB3_9
.LBB3_8:
	stw r11+28, r15
	stw r11+24, r15
	stw r11+20, r15
	stw r11+16, r15
	stw r11+12, r15
	stw r11+8, r15
	add r15, r11, r0
	jal r0, .LBB3_10
.LBB3_9:
	add r3, r11, r0
	jal r31, free
.LBB3_10:
	add r1, r15, r0
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
	.size	fopen, .Lfunc_end3-fopen
                                        # -- End function
	.globl	fclose                          # -- Begin function fclose
	.p2align	2
	.type	fclose,@function
fclose:                                 # @fclose
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	addi r1, r0, 0
	beq r3, r1, .LBB4_8
.LBB4_1:
	add r11, r3, r0
	ldw r3, r3+16
	beq r3, r1, .LBB4_3
.LBB4_2:
	jal r31, free
.LBB4_3:
	ldw r6, r11+0
	addi r3, r0, 6
	addi r4, r0, 0
	add r5, r4, r0
	jal r31, s32_mmio_request
	add r12, r1, r0
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	ldw r1, r1+0
	beq r11, r1, .LBB4_7
.LBB4_4:
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	beq r11, r1, .LBB4_7
.LBB4_5:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	beq r11, r1, .LBB4_7
.LBB4_6:
	add r3, r11, r0
	jal r31, free
.LBB4_7:
	srai r1, r12, 31
	jal r0, .LBB4_9
.LBB4_8:
	addi r1, r0, -1
.LBB4_9:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end4:
	.size	fclose, .Lfunc_end4-fclose
                                        # -- End function
	.globl	fwrite                          # -- Begin function fwrite
	.p2align	2
	.type	fwrite,@function
fwrite:                                 # @fwrite
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
	addi r1, r0, 0
	beq r3, r1, .LBB5_10
.LBB5_1:
	addi r7, r0, 0
	beq r6, r7, .LBB5_10
.LBB5_2:
	add r11, r3, r0
	mul r3, r5, r4
	add r1, r7, r0
	beq r3, r7, .LBB5_10
.LBB5_3:
	add r15, r5, r0
	lui r1, 12
	xor r4, r3, r1
	sltu r3, r3, r1
	addi r13, r0, 0
	sub r3, r13, r3
	and r3, r4, r3
	xor r12, r3, r1
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	beq r6, r1, .LBB5_7
.LBB5_4:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	beq r6, r1, .LBB5_7
.LBB5_5:
	lui r3, %hi(__mmio_base+16384)
	addi r3, r3, %lo(__mmio_base+16384)
	add r4, r11, r0
	add r5, r12, r0
	add r13, r6, r0
	jal r31, memcpy
	ldw r6, r13+0
	addi r3, r0, 3
	addi r11, r0, 0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, s32_mmio_request
	add r3, r1, r0
	add r1, r15, r0
	beq r3, r12, .LBB5_10
.LBB5_6:
	addi r1, r0, 1
	stw r13+8, r1
	add r1, r11, r0
	jal r0, .LBB5_10
.LBB5_7:
	lui r16, %hi(__mmio_base+16384)
	addi r16, r16, %lo(__mmio_base+16384)
	addi r14, r0, 1
.LBB5_8:
	ldbu r1, r11+0
	stb r16+0, r1
	add r3, r14, r0
	add r4, r14, r0
	add r5, r13, r0
	add r6, r13, r0
	jal r31, s32_mmio_request
	addi r12, r12, -1
	addi r11, r11, 1
	bne r12, r13, .LBB5_8
.LBB5_9:
	add r1, r15, r0
.LBB5_10:
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
.Lfunc_end5:
	.size	fwrite, .Lfunc_end5-fwrite
                                        # -- End function
	.globl	fread                           # -- Begin function fread
	.p2align	2
	.type	fread,@function
fread:                                  # @fread
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
	addi r11, r0, 0
	beq r3, r11, .LBB6_15
.LBB6_1:
	add r12, r3, r0
	addi r3, r0, 0
	beq r6, r3, .LBB6_15
.LBB6_2:
	mul r1, r5, r4
	add r11, r3, r0
	beq r1, r3, .LBB6_15
.LBB6_3:
	lui r3, 12
	xor r7, r1, r3
	sltu r1, r1, r3
	addi r11, r0, 0
	sub r1, r11, r1
	and r1, r7, r1
	xor r13, r1, r3
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	ldw r1, r1+0
	beq r6, r1, .LBB6_6
.LBB6_4:
	add r14, r4, r0
	add r15, r6, r0
	ldw r6, r6+0
	addi r3, r0, 4
	addi r11, r0, 0
	add r4, r13, r0
	add r5, r11, r0
	jal r31, s32_mmio_request
	addi r3, r0, -2
	bltu r1, r3, .LBB6_10
.LBB6_5:
	addi r1, r0, 1
	stw r15+8, r1
	jal r0, .LBB6_15
.LBB6_6:
	add r16, r5, r0
	add r15, r6, r0
	addi r14, r0, 2
	addi r17, r0, -1
	lui r18, %hi(__mmio_base+16384)
	addi r18, r18, %lo(__mmio_base+16384)
.LBB6_7:
	add r3, r14, r0
	add r4, r11, r0
	add r5, r11, r0
	add r6, r11, r0
	jal r31, s32_mmio_request
	beq r1, r17, .LBB6_14
.LBB6_8:
	ldbu r1, r18+0
	stb r12+0, r1
	addi r13, r13, -1
	addi r12, r12, 1
	bne r13, r11, .LBB6_7
.LBB6_9:
	add r11, r16, r0
	jal r0, .LBB6_15
.LBB6_10:
	beq r1, r11, .LBB6_14
.LBB6_11:
	bgeu r1, r13, .LBB6_13
.LBB6_12:
	addi r3, r0, 1
	stw r15+12, r3
.LBB6_13:
	lui r4, %hi(__mmio_base+16384)
	addi r4, r4, %lo(__mmio_base+16384)
	add r3, r12, r0
	add r5, r1, r0
	add r11, r1, r0
	jal r31, memcpy
	add r3, r11, r0
	add r4, r14, r0
	jal r31, __udivsi3
	add r11, r1, r0
	jal r0, .LBB6_15
.LBB6_14:
	addi r1, r0, 1
	stw r15+12, r1
.LBB6_15:
	add r1, r11, r0
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
.Lfunc_end6:
	.size	fread, .Lfunc_end6-fread
                                        # -- End function
	.globl	fgetc                           # -- Begin function fgetc
	.p2align	2
	.type	fgetc,@function
fgetc:                                  # @fgetc
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
	addi r11, r0, -1
	addi r1, r0, 0
	beq r3, r1, .LBB7_10
.LBB7_1:
	add r12, r3, r0
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	ldw r1, r1+0
	beq r3, r1, .LBB7_4
.LBB7_2:
	ldw r6, r12+0
	addi r3, r0, 4
	addi r14, r0, 1
	addi r13, r0, 0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, s32_mmio_request
	addi r3, r0, -2
	bltu r1, r3, .LBB7_6
.LBB7_3:
	stw r12+8, r14
	jal r0, .LBB7_10
.LBB7_4:
	addi r3, r0, 2
	addi r4, r0, 0
	add r5, r4, r0
	add r6, r4, r0
	jal r31, s32_mmio_request
	addi r11, r0, -1
	beq r1, r11, .LBB7_8
.LBB7_5:
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	ldbu r11, r1+0
	jal r0, .LBB7_10
.LBB7_6:
	beq r1, r13, .LBB7_9
.LBB7_7:
	addi r11, fp, -24
	lui r4, %hi(__mmio_base+16384)
	addi r4, r4, %lo(__mmio_base+16384)
	add r3, r11, r0
	add r5, r1, r0
	add r12, r1, r0
	jal r31, memcpy
	ori r1, r0, 1
	seq r1, r12, r1
	ldbu r3, r11+0
	sub r1, r13, r1
	addi r4, r0, -1
	xor r3, r3, r4
	and r1, r3, r1
	xor r11, r1, r4
	jal r0, .LBB7_10
.LBB7_8:
	addi r1, r0, 1
	stw r12+12, r1
	jal r0, .LBB7_10
.LBB7_9:
	stw r12+12, r14
.LBB7_10:
	add r1, r11, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end7:
	.size	fgetc, .Lfunc_end7-fgetc
                                        # -- End function
	.globl	getc                            # -- Begin function getc
	.p2align	2
	.type	getc,@function
getc:                                   # @getc
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
	addi r11, r0, -1
	addi r1, r0, 0
	beq r3, r1, .LBB8_10
.LBB8_1:
	add r12, r3, r0
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	ldw r1, r1+0
	beq r3, r1, .LBB8_4
.LBB8_2:
	ldw r6, r12+0
	addi r3, r0, 4
	addi r14, r0, 1
	addi r13, r0, 0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, s32_mmio_request
	addi r3, r0, -2
	bltu r1, r3, .LBB8_6
.LBB8_3:
	stw r12+8, r14
	jal r0, .LBB8_10
.LBB8_4:
	addi r3, r0, 2
	addi r4, r0, 0
	add r5, r4, r0
	add r6, r4, r0
	jal r31, s32_mmio_request
	addi r11, r0, -1
	beq r1, r11, .LBB8_8
.LBB8_5:
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	ldbu r11, r1+0
	jal r0, .LBB8_10
.LBB8_6:
	beq r1, r13, .LBB8_9
.LBB8_7:
	addi r11, fp, -24
	lui r4, %hi(__mmio_base+16384)
	addi r4, r4, %lo(__mmio_base+16384)
	add r3, r11, r0
	add r5, r1, r0
	add r12, r1, r0
	jal r31, memcpy
	ori r1, r0, 1
	seq r1, r12, r1
	ldbu r3, r11+0
	sub r1, r13, r1
	addi r4, r0, -1
	xor r3, r3, r4
	and r1, r3, r1
	xor r11, r1, r4
	jal r0, .LBB8_10
.LBB8_8:
	addi r1, r0, 1
	stw r12+12, r1
	jal r0, .LBB8_10
.LBB8_9:
	stw r12+12, r14
.LBB8_10:
	add r1, r11, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end8:
	.size	getc, .Lfunc_end8-getc
                                        # -- End function
	.globl	fputc                           # -- Begin function fputc
	.p2align	2
	.type	fputc,@function
fputc:                                  # @fputc
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
	addi r13, r0, -1
	addi r1, r0, 0
	beq r4, r1, .LBB9_5
.LBB9_1:
	add r11, r3, r0
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	beq r4, r1, .LBB9_6
.LBB9_2:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	beq r4, r1, .LBB9_6
.LBB9_3:
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	stb r1+0, r11
	add r14, r4, r0
	ldw r6, r4+0
	addi r3, r0, 3
	addi r12, r0, 1
	addi r5, r0, 0
	add r4, r12, r0
	jal r31, s32_mmio_request
	beq r1, r12, .LBB9_7
.LBB9_4:
	stw r14+8, r12
.LBB9_5:
	add r11, r13, r0
	jal r0, .LBB9_7
.LBB9_6:
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	stb r1+0, r11
	addi r3, r0, 1
	addi r5, r0, 0
	add r4, r3, r0
	add r6, r5, r0
	jal r31, s32_mmio_request
.LBB9_7:
	add r1, r11, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end9:
	.size	fputc, .Lfunc_end9-fputc
                                        # -- End function
	.globl	putc                            # -- Begin function putc
	.p2align	2
	.type	putc,@function
putc:                                   # @putc
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
	addi r13, r0, -1
	addi r1, r0, 0
	beq r4, r1, .LBB10_5
.LBB10_1:
	add r11, r3, r0
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	beq r4, r1, .LBB10_6
.LBB10_2:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	beq r4, r1, .LBB10_6
.LBB10_3:
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	stb r1+0, r11
	add r14, r4, r0
	ldw r6, r4+0
	addi r3, r0, 3
	addi r12, r0, 1
	addi r5, r0, 0
	add r4, r12, r0
	jal r31, s32_mmio_request
	beq r1, r12, .LBB10_7
.LBB10_4:
	stw r14+8, r12
.LBB10_5:
	add r11, r13, r0
	jal r0, .LBB10_7
.LBB10_6:
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	stb r1+0, r11
	addi r3, r0, 1
	addi r5, r0, 0
	add r4, r3, r0
	add r6, r5, r0
	jal r31, s32_mmio_request
.LBB10_7:
	add r1, r11, r0
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end10:
	.size	putc, .Lfunc_end10-putc
                                        # -- End function
	.globl	fgets                           # -- Begin function fgets
	.p2align	2
	.type	fgets,@function
fgets:                                  # @fgets
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
	stw fp+-56, r24
	stw fp+-60, r25
	stw fp+-64, r26
	stw fp+-68, r27
	stw fp+-72, lr
	addi r1, r0, 0
	beq r3, r1, .LBB11_19
.LBB11_1:
	addi r13, r0, 1
	blt r4, r13, .LBB11_19
.LBB11_2:
	add r12, r5, r0
	add r11, r3, r0
	addi r21, r4, -1
	addi r14, r0, 0
	lui r24, %hi(stdin)
	addi r24, r24, %lo(stdin)
	addi r15, r0, 2
	addi r23, r0, 12
	addi r25, r0, -1
	lui r16, %hi(__mmio_base+16384)
	addi r16, r16, %lo(__mmio_base+16384)
	addi r26, r0, 10
	addi r17, r0, 4
	addi r22, r0, 8
	addi r27, r0, -3
	addi r18, fp, -76
	add r20, r14, r0
	jal r0, .LBB11_4
.LBB11_3:
	addi r3, r20, 1
	add r4, r11, r20
	stb r4+0, r1
	andi r1, r1, 255
	add r20, r3, r0
	beq r1, r26, .LBB11_18
.LBB11_4:
	beq r21, r20, .LBB11_13
.LBB11_5:
	beq r12, r14, .LBB11_14
.LBB11_6:
	ldw r1, r24+0
	beq r12, r1, .LBB11_11
.LBB11_7:
	ldw r6, r12+0
	add r3, r17, r0
	add r4, r13, r0
	add r5, r14, r0
	jal r31, s32_mmio_request
	bgtu r1, r27, .LBB11_16
.LBB11_8:
	add r19, r1, r0
	beq r1, r14, .LBB11_15
.LBB11_9:
	add r3, r18, r0
	add r4, r16, r0
	add r5, r19, r0
	jal r31, memcpy
	bne r19, r13, .LBB11_17
.LBB11_10:
	ldbu r1, r18+0
	jal r0, .LBB11_3
.LBB11_11:
	add r3, r15, r0
	add r4, r14, r0
	add r5, r14, r0
	add r6, r14, r0
	jal r31, s32_mmio_request
	beq r1, r25, .LBB11_15
.LBB11_12:
	ldbu r1, r16+0
	jal r0, .LBB11_3
.LBB11_13:
	add r3, r21, r0
	jal r0, .LBB11_18
.LBB11_14:
	addi r1, r0, 0
	jal r0, .LBB11_19
.LBB11_15:
	add r22, r23, r0
.LBB11_16:
	add r1, r12, r22
	stw r1+0, r13
.LBB11_17:
	addi r1, r0, 0
	add r3, r20, r0
	beq r20, r1, .LBB11_19
.LBB11_18:
	add r1, r11, r3
	addi r3, r0, 0
	stb r1+0, r3
	add r1, r11, r0
.LBB11_19:
	ldw lr, fp+-72
	ldw r27, fp+-68
	ldw r26, fp+-64
	ldw r25, fp+-60
	ldw r24, fp+-56
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
.Lfunc_end11:
	.size	fgets, .Lfunc_end11-fgets
                                        # -- End function
	.globl	fputs                           # -- Begin function fputs
	.p2align	2
	.type	fputs,@function
fputs:                                  # @fputs
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
	add r13, r4, r0
	add r12, r3, r0
	jal r31, strlen
	add r11, r1, r0
	addi r16, r0, 0
	add r3, r16, r0
	beq r13, r16, .LBB12_9
.LBB12_1:
	addi r15, r0, 0
	add r3, r16, r0
	beq r11, r15, .LBB12_9
.LBB12_2:
	lui r1, 12
	xor r3, r11, r1
	sltu r4, r11, r1
	sub r4, r15, r4
	and r3, r3, r4
	xor r14, r3, r1
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	beq r13, r1, .LBB12_6
.LBB12_3:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	beq r13, r1, .LBB12_6
.LBB12_4:
	lui r3, %hi(__mmio_base+16384)
	addi r3, r3, %lo(__mmio_base+16384)
	add r4, r12, r0
	add r5, r14, r0
	jal r31, memcpy
	ldw r6, r13+0
	addi r3, r0, 3
	addi r12, r0, 0
	add r4, r14, r0
	add r5, r12, r0
	jal r31, s32_mmio_request
	add r3, r11, r0
	beq r1, r14, .LBB12_9
.LBB12_5:
	addi r1, r0, 1
	stw r13+8, r1
	add r3, r12, r0
	jal r0, .LBB12_9
.LBB12_6:
	lui r17, %hi(__mmio_base+16384)
	addi r17, r17, %lo(__mmio_base+16384)
	addi r13, r0, 1
.LBB12_7:
	ldbu r1, r12+0
	stb r17+0, r1
	add r3, r13, r0
	add r4, r13, r0
	add r5, r15, r0
	add r6, r15, r0
	jal r31, s32_mmio_request
	addi r14, r14, -1
	addi r12, r12, 1
	bne r14, r15, .LBB12_7
.LBB12_8:
	add r3, r11, r0
.LBB12_9:
	sne r1, r3, r11
	sub r1, r16, r1
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
.Lfunc_end12:
	.size	fputs, .Lfunc_end12-fputs
                                        # -- End function
	.globl	fseek                           # -- Begin function fseek
	.p2align	2
	.type	fseek,@function
fseek:                                  # @fseek
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	addi r11, r0, 0
	beq r3, r11, .LBB13_2
.LBB13_1:
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	stb r1+0, r5
	lui r1, %hi(__mmio_base+16388)
	addi r1, r1, %lo(__mmio_base+16388)
	stw r1+0, r4
	ldw r6, r3+0
	addi r1, r0, 7
	addi r4, r0, 8
	add r12, r3, r0
	add r3, r1, r0
	add r5, r11, r0
	jal r31, s32_mmio_request
	ori r3, r0, 0
	slt r3, r1, r3
	sub r3, r11, r3
	andi r3, r3, 4
	xori r3, r3, 12
	srli r4, r1, 31
	srai r1, r1, 31
	add r3, r12, r3
	stw r3+0, r4
	jal r0, .LBB13_3
.LBB13_2:
	addi r1, r0, -1
.LBB13_3:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end13:
	.size	fseek, .Lfunc_end13-fseek
                                        # -- End function
	.globl	ftell                           # -- Begin function ftell
	.p2align	2
	.type	ftell,@function
ftell:                                  # @ftell
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	addi r11, r0, 0
	beq r3, r11, .LBB14_2
.LBB14_1:
	addi r1, r0, 1
	lui r4, %hi(__mmio_base+16384)
	addi r4, r4, %lo(__mmio_base+16384)
	stb r4+0, r1
	lui r1, %hi(__mmio_base+16388)
	addi r1, r1, %lo(__mmio_base+16388)
	stw r1+0, r11
	ldw r6, r3+0
	addi r1, r0, 7
	addi r4, r0, 8
	add r12, r3, r0
	add r3, r1, r0
	add r5, r11, r0
	jal r31, s32_mmio_request
	ori r3, r0, 0
	slt r3, r1, r3
	sub r3, r11, r3
	andi r3, r3, 4
	xori r3, r3, 12
	srli r4, r1, 31
	srai r1, r1, 31
	add r3, r12, r3
	stw r3+0, r4
	jal r0, .LBB14_3
.LBB14_2:
	addi r1, r0, -1
.LBB14_3:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end14:
	.size	ftell, .Lfunc_end14-ftell
                                        # -- End function
	.globl	rewind                          # -- Begin function rewind
	.p2align	2
	.type	rewind,@function
rewind:                                 # @rewind
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	addi r11, r0, 0
	beq r3, r11, .LBB15_2
.LBB15_1:
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	stb r1+0, r11
	lui r1, %hi(__mmio_base+16388)
	addi r1, r1, %lo(__mmio_base+16388)
	stw r1+0, r11
	ldw r6, r3+0
	addi r1, r0, 7
	addi r4, r0, 8
	add r12, r3, r0
	add r3, r1, r0
	add r5, r11, r0
	jal r31, s32_mmio_request
	ori r3, r0, 0
	slt r3, r1, r3
	sub r3, r11, r3
	andi r3, r3, 4
	xori r3, r3, 12
	srli r1, r1, 31
	add r3, r12, r3
	stw r3+0, r1
	stw r12+8, r11
	stw r12+12, r11
.LBB15_2:
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end15:
	.size	rewind, .Lfunc_end15-rewind
                                        # -- End function
	.globl	clearerr                        # -- Begin function clearerr
	.p2align	2
	.type	clearerr,@function
clearerr:                               # @clearerr
# %bb.0:
	addi r1, r0, 0
	beq r3, r1, .LBB16_2
.LBB16_1:
	stw r3+8, r1
	stw r3+12, r1
.LBB16_2:
	jalr r0, r31, 0
.Lfunc_end16:
	.size	clearerr, .Lfunc_end16-clearerr
                                        # -- End function
	.globl	feof                            # -- Begin function feof
	.p2align	2
	.type	feof,@function
feof:                                   # @feof
# %bb.0:
	addi r1, r0, 0
	beq r3, r1, .LBB17_2
.LBB17_1:
	ldw r1, r3+12
.LBB17_2:
	jalr r0, r31, 0
.Lfunc_end17:
	.size	feof, .Lfunc_end17-feof
                                        # -- End function
	.globl	ferror                          # -- Begin function ferror
	.p2align	2
	.type	ferror,@function
ferror:                                 # @ferror
# %bb.0:
	addi r1, r0, 0
	beq r3, r1, .LBB18_2
.LBB18_1:
	ldw r1, r3+8
.LBB18_2:
	jalr r0, r31, 0
.Lfunc_end18:
	.size	ferror, .Lfunc_end18-ferror
                                        # -- End function
	.globl	fflush                          # -- Begin function fflush
	.p2align	2
	.type	fflush,@function
fflush:                                 # @fflush
# %bb.0:
	addi r1, r0, 0
	jalr r0, r31, 0
.Lfunc_end19:
	.size	fflush, .Lfunc_end19-fflush
                                        # -- End function
	.globl	perror                          # -- Begin function perror
	.p2align	2
	.type	perror,@function
perror:                                 # @perror
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
	addi r11, r0, 0
	lui r15, %hi(stderr)
	addi r15, r15, %lo(stderr)
	lui r14, %hi(__mmio_base+16384)
	addi r14, r14, %lo(__mmio_base+16384)
	beq r3, r11, .LBB20_9
.LBB20_1:
	add r12, r3, r0
	ldbu r1, r3+0
	beq r1, r11, .LBB20_9
.LBB20_2:
	ldw r13, r15+0
	add r3, r12, r0
	jal r31, strlen
	beq r13, r11, .LBB20_7
.LBB20_3:
	beq r1, r11, .LBB20_7
.LBB20_4:
	lui r3, 12
	xor r4, r1, r3
	sltu r1, r1, r3
	sub r1, r11, r1
	and r1, r4, r1
	xor r16, r1, r3
	addi r13, r0, 1
.LBB20_5:
	ldbu r1, r12+0
	stb r14+0, r1
	add r3, r13, r0
	add r4, r13, r0
	add r5, r11, r0
	add r6, r11, r0
	jal r31, s32_mmio_request
	addi r16, r16, -1
	addi r12, r12, 1
	bne r16, r11, .LBB20_5
.LBB20_6:
	ldw r13, r15+0
.LBB20_7:
	beq r13, r11, .LBB20_9
.LBB20_8:
	addi r1, r0, 58
	stb r14+0, r1
	addi r12, r0, 1
	addi r13, r0, 0
	add r3, r12, r0
	add r4, r12, r0
	add r5, r13, r0
	add r6, r13, r0
	jal r31, s32_mmio_request
	addi r1, r0, 32
	stb r14+0, r1
	add r3, r12, r0
	add r4, r12, r0
	add r5, r13, r0
	add r6, r13, r0
	jal r31, s32_mmio_request
.LBB20_9:
	ldw r1, r15+0
	beq r1, r11, .LBB20_11
.LBB20_10:
	addi r1, r0, 101
	stb r14+0, r1
	addi r11, r0, 1
	addi r12, r0, 0
	add r3, r11, r0
	add r4, r11, r0
	add r5, r12, r0
	add r6, r12, r0
	jal r31, s32_mmio_request
	addi r13, r0, 114
	stb r14+0, r13
	add r3, r11, r0
	add r4, r11, r0
	add r5, r12, r0
	add r6, r12, r0
	jal r31, s32_mmio_request
	stb r14+0, r13
	add r3, r11, r0
	add r4, r11, r0
	add r5, r12, r0
	add r6, r12, r0
	jal r31, s32_mmio_request
	addi r1, r0, 111
	stb r14+0, r1
	add r3, r11, r0
	add r4, r11, r0
	add r5, r12, r0
	add r6, r12, r0
	jal r31, s32_mmio_request
	stb r14+0, r13
	add r3, r11, r0
	add r4, r11, r0
	add r5, r12, r0
	add r6, r12, r0
	jal r31, s32_mmio_request
	addi r1, r0, 10
	stb r14+0, r1
	add r3, r11, r0
	add r4, r11, r0
	add r5, r12, r0
	add r6, r12, r0
	jal r31, s32_mmio_request
.LBB20_11:
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
.Lfunc_end20:
	.size	perror, .Lfunc_end20-perror
                                        # -- End function
	.type	_stdin,@object                  # @_stdin
	.data
	.p2align	2, 0x0
_stdin:
	.word	0                               # 0x0
	.word	1                               # 0x1
	.word	0                               # 0x0
	.word	0                               # 0x0
	.word	0
	.word	0                               # 0x0
	.word	0                               # 0x0
	.word	0                               # 0x0
	.size	_stdin, 32

	.type	stdin,@object                   # @stdin
	.globl	stdin
	.p2align	2, 0x0
stdin:
	.word	_stdin
	.size	stdin, 4

	.type	_stdout,@object                 # @_stdout
	.p2align	2, 0x0
_stdout:
	.word	1                               # 0x1
	.word	2                               # 0x2
	.word	0                               # 0x0
	.word	0                               # 0x0
	.word	0
	.word	0                               # 0x0
	.word	0                               # 0x0
	.word	0                               # 0x0
	.size	_stdout, 32

	.type	stdout,@object                  # @stdout
	.globl	stdout
	.p2align	2, 0x0
stdout:
	.word	_stdout
	.size	stdout, 4

	.type	_stderr,@object                 # @_stderr
	.p2align	2, 0x0
_stderr:
	.word	2                               # 0x2
	.word	2                               # 0x2
	.word	0                               # 0x0
	.word	0                               # 0x0
	.word	0
	.word	0                               # 0x0
	.word	0                               # 0x0
	.word	0                               # 0x0
	.size	_stderr, 32

	.type	stderr,@object                  # @stderr
	.globl	stderr
	.p2align	2, 0x0
stderr:
	.word	_stderr
	.size	stderr, 4

	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 64733ade54e83caa20942f58250ac6667cdd1ee7)"
	.section	".note.GNU-stack","",@progbits
