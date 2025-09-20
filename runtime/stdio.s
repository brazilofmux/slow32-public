	.file	"stdio.c"
	.text
	.globl	putchar                         # -- Begin function putchar
	.p2align	2
	.type	putchar,@function
putchar:                                # @putchar
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
	add r11, r3, r0
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	stb r1+0, r3
	lui r12, %hi(__mmio_base)
	addi r12, r12, %lo(__mmio_base)
	ldw r14, r12+0
	addi r1, r14, 1
	andi r13, r1, 255
	lui r15, %hi(__mmio_base+4)
	addi r15, r15, %lo(__mmio_base+4)
	ldw r1, r15+0
	bne r13, r1, .LBB0_3
.LBB0_1:
	jal r31, yield
	ldw r1, r15+0
	beq r13, r1, .LBB0_1
.LBB0_3:
	slli r1, r14, 4
	lui r2, %hi(__mmio_base+4096)
	addi r2, r2, %lo(__mmio_base+4096)
	add r2, r1, r2
	addi r3, r0, 1
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4100)
	addi r2, r2, %lo(__mmio_base+4100)
	add r2, r1, r2
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4104)
	addi r2, r2, %lo(__mmio_base+4104)
	add r2, r1, r2
	addi r3, r0, 0
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4108)
	addi r2, r2, %lo(__mmio_base+4108)
	add r1, r1, r2
	stw r1+0, r3
	stw r12+0, r13
	lui r13, %hi(__mmio_base+8192)
	addi r13, r13, %lo(__mmio_base+8192)
	ldw r2, r13+0
	lui r12, %hi(__mmio_base+8196)
	addi r12, r12, %lo(__mmio_base+8196)
	ldw r1, r12+0
	bne r2, r1, .LBB0_6
.LBB0_4:
	jal r31, yield
	ldw r2, r13+0
	ldw r1, r12+0
	beq r2, r1, .LBB0_4
.LBB0_6:
	slli r2, r2, 4
	lui r3, %hi(__mmio_base+12300)
	addi r3, r3, %lo(__mmio_base+12300)
	add r2, r2, r3
	ldw r2, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r12+0, r1
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
.Lfunc_end0:
	.size	putchar, .Lfunc_end0-putchar
                                        # -- End function
	.globl	getchar                         # -- Begin function getchar
	.p2align	2
	.type	getchar,@function
getchar:                                # @getchar
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
	lui r11, %hi(__mmio_base)
	addi r11, r11, %lo(__mmio_base)
	ldw r13, r11+0
	addi r1, r13, 1
	andi r12, r1, 255
	lui r14, %hi(__mmio_base+4)
	addi r14, r14, %lo(__mmio_base+4)
	ldw r1, r14+0
	bne r12, r1, .LBB1_3
.LBB1_1:
	jal r31, yield
	ldw r1, r14+0
	beq r12, r1, .LBB1_1
.LBB1_3:
	slli r1, r13, 4
	lui r2, %hi(__mmio_base+4096)
	addi r2, r2, %lo(__mmio_base+4096)
	add r2, r1, r2
	addi r3, r0, 2
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4100)
	addi r2, r2, %lo(__mmio_base+4100)
	add r2, r1, r2
	addi r3, r0, 0
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4104)
	addi r2, r2, %lo(__mmio_base+4104)
	add r2, r1, r2
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4108)
	addi r2, r2, %lo(__mmio_base+4108)
	add r1, r1, r2
	stw r1+0, r3
	stw r11+0, r12
	lui r12, %hi(__mmio_base+8192)
	addi r12, r12, %lo(__mmio_base+8192)
	ldw r2, r12+0
	lui r11, %hi(__mmio_base+8196)
	addi r11, r11, %lo(__mmio_base+8196)
	ldw r1, r11+0
	bne r2, r1, .LBB1_6
.LBB1_4:
	jal r31, yield
	ldw r2, r12+0
	ldw r1, r11+0
	beq r2, r1, .LBB1_4
.LBB1_6:
	slli r2, r2, 4
	lui r3, %hi(__mmio_base+12300)
	addi r3, r3, %lo(__mmio_base+12300)
	add r2, r2, r3
	ldw r2, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r11+0, r1
	addi r1, r0, -1
	beq r2, r1, .LBB1_8
.LBB1_7:
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	ldbu r1, r1+0
.LBB1_8:
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end1:
	.size	getchar, .Lfunc_end1-getchar
                                        # -- End function
	.globl	puts                            # -- Begin function puts
	.p2align	2
	.type	puts,@function
puts:                                   # @puts
# %bb.0:
	addi sp, sp, -72
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 72
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
	stw fp+-64, lr
	ldbu r1, r3+0
	addi r15, r0, 0
	lui r22, %hi(__mmio_base+16384)
	addi r22, r22, %lo(__mmio_base+16384)
	lui r16, %hi(__mmio_base)
	addi r16, r16, %lo(__mmio_base)
	lui r21, %hi(__mmio_base+4)
	addi r21, r21, %lo(__mmio_base+4)
	lui r20, %hi(__mmio_base+4096)
	addi r20, r20, %lo(__mmio_base+4096)
	lui r19, %hi(__mmio_base+4100)
	addi r19, r19, %lo(__mmio_base+4100)
	lui r18, %hi(__mmio_base+4104)
	addi r18, r18, %lo(__mmio_base+4104)
	lui r17, %hi(__mmio_base+4108)
	addi r17, r17, %lo(__mmio_base+4108)
	lui r14, %hi(__mmio_base+8192)
	addi r14, r14, %lo(__mmio_base+8192)
	lui r12, %hi(__mmio_base+8196)
	addi r12, r12, %lo(__mmio_base+8196)
	lui r13, %hi(__mmio_base+12300)
	addi r13, r13, %lo(__mmio_base+12300)
	beq r1, r15, .LBB2_9
.LBB2_1:
	add r11, r3, r0
	addi r23, r0, 1
	jal r0, .LBB2_2
.LBB2_8:
	addi r11, r11, 1
	slli r2, r2, 4
	add r2, r2, r13
	ldw r2, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r12+0, r1
	ldbu r1, r11+0
	beq r1, r15, .LBB2_9
.LBB2_2:
	stb r22+0, r1
	ldw r25, r16+0
	addi r1, r25, 1
	andi r24, r1, 255
	ldw r1, r21+0
	bne r24, r1, .LBB2_5
.LBB2_3:
	jal r31, yield
	ldw r1, r21+0
	beq r24, r1, .LBB2_3
.LBB2_5:
	slli r1, r25, 4
	add r2, r1, r20
	stw r2+0, r23
	add r2, r1, r19
	stw r2+0, r23
	add r2, r1, r18
	stw r2+0, r15
	add r1, r1, r17
	stw r1+0, r15
	stw r16+0, r24
.LBB2_7:
	ldw r2, r14+0
	ldw r1, r12+0
	bne r2, r1, .LBB2_8
.LBB2_6:
	jal r31, yield
	jal r0, .LBB2_7
.LBB2_9:
	addi r1, r0, 10
	stb r22+0, r1
	ldw r22, r16+0
	addi r1, r22, 1
	andi r11, r1, 255
	ldw r1, r21+0
	bne r11, r1, .LBB2_12
.LBB2_10:
	jal r31, yield
	ldw r1, r21+0
	beq r11, r1, .LBB2_10
.LBB2_12:
	slli r1, r22, 4
	add r2, r1, r20
	addi r3, r0, 1
	stw r2+0, r3
	add r2, r1, r19
	stw r2+0, r3
	add r2, r1, r18
	stw r2+0, r15
	add r1, r1, r17
	stw r1+0, r15
	stw r16+0, r11
.LBB2_14:
	ldw r2, r14+0
	ldw r1, r12+0
	bne r2, r1, .LBB2_15
.LBB2_13:
	jal r31, yield
	jal r0, .LBB2_14
.LBB2_15:
	slli r2, r2, 4
	add r2, r2, r13
	ldw r2, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r12+0, r1
	addi r1, r0, 0
	ldw lr, fp+-64
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
	addi sp, sp, 72
	jalr r0, r31, 0
.Lfunc_end2:
	.size	puts, .Lfunc_end2-puts
                                        # -- End function
	.globl	fopen                           # -- Begin function fopen
	.p2align	2
	.type	fopen,@function
fopen:                                  # @fopen
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
	add r14, r4, r0
	add r13, r3, r0
	addi r3, r0, 32
	jal r31, malloc
	addi r12, r0, 0
	beq r1, r12, .LBB3_16
.LBB3_1:
	add r11, r1, r0
	addi r4, r0, 114
	add r3, r14, r0
	jal r31, strchr
	ori r2, r0, 0
	sne r15, r1, r2
	stw r11+4, r15
	addi r4, r0, 119
	add r3, r14, r0
	jal r31, strchr
	addi r12, r0, 0
	beq r1, r12, .LBB3_3
.LBB3_2:
	ori  r15, r15, 26
	stw r11+4, r15
.LBB3_3:
	addi r4, r0, 97
	add r3, r14, r0
	jal r31, strchr
	beq r1, r12, .LBB3_5
.LBB3_4:
	ori  r15, r15, 6
	stw r11+4, r15
.LBB3_5:
	addi r4, r0, 43
	add r3, r14, r0
	jal r31, strchr
	beq r1, r12, .LBB3_7
.LBB3_6:
	ori  r15, r15, 3
	stw r11+4, r15
.LBB3_7:
	add r3, r13, r0
	jal r31, strlen
	addi r14, r1, 1
	lui r3, %hi(__mmio_base+16384)
	addi r3, r3, %lo(__mmio_base+16384)
	add r4, r13, r0
	add r5, r14, r0
	jal r31, memcpy
	lui r13, %hi(__mmio_base)
	addi r13, r13, %lo(__mmio_base)
	ldw r17, r13+0
	addi r1, r17, 1
	andi r16, r1, 255
	lui r18, %hi(__mmio_base+4)
	addi r18, r18, %lo(__mmio_base+4)
	ldw r1, r18+0
	bne r16, r1, .LBB3_10
.LBB3_8:
	jal r31, yield
	ldw r1, r18+0
	beq r16, r1, .LBB3_8
.LBB3_10:
	slli r1, r17, 4
	lui r2, %hi(__mmio_base+4096)
	addi r2, r2, %lo(__mmio_base+4096)
	add r2, r1, r2
	addi r3, r0, 5
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4100)
	addi r2, r2, %lo(__mmio_base+4100)
	add r2, r1, r2
	stw r2+0, r14
	lui r2, %hi(__mmio_base+4104)
	addi r2, r2, %lo(__mmio_base+4104)
	add r2, r1, r2
	stw r2+0, r12
	lui r2, %hi(__mmio_base+4108)
	addi r2, r2, %lo(__mmio_base+4108)
	add r1, r1, r2
	stw r1+0, r15
	stw r13+0, r16
	lui r14, %hi(__mmio_base+8192)
	addi r14, r14, %lo(__mmio_base+8192)
	ldw r2, r14+0
	lui r13, %hi(__mmio_base+8196)
	addi r13, r13, %lo(__mmio_base+8196)
	ldw r1, r13+0
	bne r2, r1, .LBB3_13
.LBB3_11:
	jal r31, yield
	ldw r2, r14+0
	ldw r1, r13+0
	beq r2, r1, .LBB3_11
.LBB3_13:
	slli r2, r2, 4
	lui r3, %hi(__mmio_base+12300)
	addi r3, r3, %lo(__mmio_base+12300)
	add r2, r2, r3
	ldw r2, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r13+0, r1
	stw r11+0, r2
	addi r1, r0, -1
	ble r2, r1, .LBB3_14
.LBB3_15:
	stw r11+28, r12
	stw r11+24, r12
	stw r11+20, r12
	stw r11+16, r12
	stw r11+12, r12
	stw r11+8, r12
	add r12, r11, r0
	jal r0, .LBB3_16
.LBB3_14:
	add r3, r11, r0
	jal r31, free
.LBB3_16:
	add r1, r12, r0
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
.Lfunc_end3:
	.size	fopen, .Lfunc_end3-fopen
                                        # -- End function
	.globl	fclose                          # -- Begin function fclose
	.p2align	2
	.type	fclose,@function
fclose:                                 # @fclose
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
	addi r12, r0, 0
	beq r3, r12, .LBB4_1
.LBB4_2:
	add r11, r3, r0
	ldw r3, r3+16
	beq r3, r12, .LBB4_4
.LBB4_3:
	jal r31, free
.LBB4_4:
	ldw r13, r11+0
	lui r14, %hi(__mmio_base)
	addi r14, r14, %lo(__mmio_base)
	ldw r16, r14+0
	addi r1, r16, 1
	andi r15, r1, 255
	lui r17, %hi(__mmio_base+4)
	addi r17, r17, %lo(__mmio_base+4)
	ldw r1, r17+0
	bne r15, r1, .LBB4_7
.LBB4_5:
	jal r31, yield
	ldw r1, r17+0
	beq r15, r1, .LBB4_5
.LBB4_7:
	slli r1, r16, 4
	lui r2, %hi(__mmio_base+4096)
	addi r2, r2, %lo(__mmio_base+4096)
	add r2, r1, r2
	addi r3, r0, 6
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4100)
	addi r2, r2, %lo(__mmio_base+4100)
	add r2, r1, r2
	stw r2+0, r12
	lui r2, %hi(__mmio_base+4104)
	addi r2, r2, %lo(__mmio_base+4104)
	add r2, r1, r2
	stw r2+0, r12
	lui r2, %hi(__mmio_base+4108)
	addi r2, r2, %lo(__mmio_base+4108)
	add r1, r1, r2
	stw r1+0, r13
	stw r14+0, r15
	lui r13, %hi(__mmio_base+8192)
	addi r13, r13, %lo(__mmio_base+8192)
	ldw r2, r13+0
	lui r12, %hi(__mmio_base+8196)
	addi r12, r12, %lo(__mmio_base+8196)
	ldw r1, r12+0
	bne r2, r1, .LBB4_10
.LBB4_8:
	jal r31, yield
	ldw r2, r13+0
	ldw r1, r12+0
	beq r2, r1, .LBB4_8
.LBB4_10:
	slli r2, r2, 4
	lui r3, %hi(__mmio_base+12300)
	addi r3, r3, %lo(__mmio_base+12300)
	add r2, r2, r3
	ldw r13, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r12+0, r1
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	ldw r1, r1+0
	beq r11, r1, .LBB4_14
.LBB4_11:
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	beq r11, r1, .LBB4_14
.LBB4_12:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	beq r11, r1, .LBB4_14
.LBB4_13:
	add r3, r11, r0
	jal r31, free
.LBB4_14:
	srai r1, r13, 31
	jal r0, .LBB4_15
.LBB4_1:
	addi r1, r0, -1
.LBB4_15:
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
.Lfunc_end4:
	.size	fclose, .Lfunc_end4-fclose
                                        # -- End function
	.globl	fwrite                          # -- Begin function fwrite
	.p2align	2
	.type	fwrite,@function
fwrite:                                 # @fwrite
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
	stw fp+-72, r28
	stw fp+-76, lr
	addi r1, r0, 0
	beq r3, r1, .LBB5_22
.LBB5_1:
	add r12, r3, r0
	addi r3, r0, 0
	beq r6, r3, .LBB5_22
.LBB5_2:
	mul r2, r5, r4
	add r1, r3, r0
	beq r2, r3, .LBB5_22
.LBB5_3:
	add r13, r5, r0
	lui r1, 12
	xor r3, r2, r1
	sltu r2, r2, r1
	addi r14, r0, 0
	sub r2, r14, r2
	and r2, r3, r2
	xor r11, r2, r1
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	beq r6, r1, .LBB5_5
.LBB5_4:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	beq r6, r1, .LBB5_5
.LBB5_14:
	lui r3, %hi(__mmio_base+16384)
	addi r3, r3, %lo(__mmio_base+16384)
	add r4, r12, r0
	add r5, r11, r0
	add r16, r6, r0
	jal r31, memcpy
	lui r15, %hi(__mmio_base)
	addi r15, r15, %lo(__mmio_base)
	add r12, r16, r0
	ldw r16, r16+0
	ldw r18, r15+0
	addi r1, r18, 1
	andi r17, r1, 255
	lui r19, %hi(__mmio_base+4)
	addi r19, r19, %lo(__mmio_base+4)
	ldw r1, r19+0
	bne r17, r1, .LBB5_17
.LBB5_15:
	jal r31, yield
	ldw r1, r19+0
	beq r17, r1, .LBB5_15
.LBB5_17:
	slli r1, r18, 4
	lui r2, %hi(__mmio_base+4096)
	addi r2, r2, %lo(__mmio_base+4096)
	add r2, r1, r2
	addi r3, r0, 3
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4100)
	addi r2, r2, %lo(__mmio_base+4100)
	add r2, r1, r2
	stw r2+0, r11
	lui r2, %hi(__mmio_base+4104)
	addi r2, r2, %lo(__mmio_base+4104)
	add r2, r1, r2
	stw r2+0, r14
	lui r2, %hi(__mmio_base+4108)
	addi r2, r2, %lo(__mmio_base+4108)
	add r1, r1, r2
	stw r1+0, r16
	stw r15+0, r17
	lui r16, %hi(__mmio_base+8192)
	addi r16, r16, %lo(__mmio_base+8192)
	ldw r2, r16+0
	lui r15, %hi(__mmio_base+8196)
	addi r15, r15, %lo(__mmio_base+8196)
	ldw r1, r15+0
	bne r2, r1, .LBB5_20
.LBB5_18:
	jal r31, yield
	ldw r2, r16+0
	ldw r1, r15+0
	beq r2, r1, .LBB5_18
.LBB5_20:
	slli r2, r2, 4
	lui r3, %hi(__mmio_base+12300)
	addi r3, r3, %lo(__mmio_base+12300)
	add r2, r2, r3
	ldw r2, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r15+0, r1
	add r1, r13, r0
	beq r2, r11, .LBB5_22
.LBB5_21:
	addi r1, r0, 1
	stw r12+8, r1
	add r1, r14, r0
	jal r0, .LBB5_22
.LBB5_5:
	addi r14, r0, 0
	lui r15, %hi(__mmio_base+16384)
	addi r15, r15, %lo(__mmio_base+16384)
	lui r16, %hi(__mmio_base)
	addi r16, r16, %lo(__mmio_base)
	lui r17, %hi(__mmio_base+4)
	addi r17, r17, %lo(__mmio_base+4)
	lui r18, %hi(__mmio_base+4096)
	addi r18, r18, %lo(__mmio_base+4096)
	addi r19, r0, 1
	lui r20, %hi(__mmio_base+4100)
	addi r20, r20, %lo(__mmio_base+4100)
	lui r21, %hi(__mmio_base+4104)
	addi r21, r21, %lo(__mmio_base+4104)
	lui r22, %hi(__mmio_base+4108)
	addi r22, r22, %lo(__mmio_base+4108)
	lui r23, %hi(__mmio_base+8192)
	addi r23, r23, %lo(__mmio_base+8192)
	lui r24, %hi(__mmio_base+8196)
	addi r24, r24, %lo(__mmio_base+8196)
	lui r25, %hi(__mmio_base+12300)
	addi r25, r25, %lo(__mmio_base+12300)
	add r26, r14, r0
	jal r0, .LBB5_6
.LBB5_12:
	slli r2, r2, 4
	add r2, r2, r25
	ldw r2, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r24+0, r1
	addi r26, r26, 1
	beq r26, r11, .LBB5_13
.LBB5_6:
	add r1, r12, r26
	ldbu r1, r1+0
	stb r15+0, r1
	ldw r28, r16+0
	addi r1, r28, 1
	andi r27, r1, 255
	ldw r1, r17+0
	bne r27, r1, .LBB5_9
.LBB5_7:
	jal r31, yield
	ldw r1, r17+0
	beq r27, r1, .LBB5_7
.LBB5_9:
	slli r1, r28, 4
	add r2, r1, r18
	stw r2+0, r19
	add r2, r1, r20
	stw r2+0, r19
	add r2, r1, r21
	stw r2+0, r14
	add r1, r1, r22
	stw r1+0, r14
	stw r16+0, r27
.LBB5_11:
	ldw r2, r23+0
	ldw r1, r24+0
	bne r2, r1, .LBB5_12
.LBB5_10:
	jal r31, yield
	jal r0, .LBB5_11
.LBB5_13:
	add r1, r13, r0
.LBB5_22:
	ldw lr, fp+-76
	ldw r28, fp+-72
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
.Lfunc_end5:
	.size	fwrite, .Lfunc_end5-fwrite
                                        # -- End function
	.globl	fread                           # -- Begin function fread
	.p2align	2
	.type	fread,@function
fread:                                  # @fread
# %bb.0:
	addi sp, sp, -104
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 104
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
	stw fp+-72, r28
	stw fp+-76, lr
	addi r14, r0, 0
	beq r3, r14, .LBB6_26
.LBB6_1:
	addi r2, r0, 0
	beq r6, r2, .LBB6_26
.LBB6_2:
	mul r1, r5, r4
	add r14, r2, r0
	beq r1, r2, .LBB6_26
.LBB6_3:
	add r13, r3, r0
	lui r2, 12
	xor r3, r1, r2
	sltu r1, r1, r2
	addi r14, r0, 0
	sub r1, r14, r1
	and r1, r3, r1
	xor r16, r1, r2
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	ldw r1, r1+0
	beq r6, r1, .LBB6_4
.LBB6_15:
	add r19, r5, r0
	add r20, r6, r0
	ldw r15, r6+0
	lui r17, %hi(__mmio_base)
	addi r17, r17, %lo(__mmio_base)
	ldw r11, r17+0
	addi r1, r11, 1
	andi r18, r1, 255
	lui r12, %hi(__mmio_base+4)
	addi r12, r12, %lo(__mmio_base+4)
	ldw r1, r12+0
	bne r18, r1, .LBB6_18
.LBB6_16:
	jal r31, yield
	ldw r1, r12+0
	beq r18, r1, .LBB6_16
.LBB6_18:
	slli r1, r11, 4
	lui r2, %hi(__mmio_base+4096)
	addi r2, r2, %lo(__mmio_base+4096)
	add r2, r1, r2
	addi r3, r0, 4
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4100)
	addi r2, r2, %lo(__mmio_base+4100)
	add r2, r1, r2
	stw r2+0, r16
	lui r2, %hi(__mmio_base+4104)
	addi r2, r2, %lo(__mmio_base+4104)
	add r2, r1, r2
	stw r2+0, r14
	lui r2, %hi(__mmio_base+4108)
	addi r2, r2, %lo(__mmio_base+4108)
	add r1, r1, r2
	stw r1+0, r15
	stw r17+0, r18
	lui r11, %hi(__mmio_base+8192)
	addi r11, r11, %lo(__mmio_base+8192)
	ldw r2, r11+0
	lui r17, %hi(__mmio_base+8196)
	addi r17, r17, %lo(__mmio_base+8196)
	ldw r1, r17+0
	bne r2, r1, .LBB6_21
.LBB6_19:
	jal r31, yield
	ldw r2, r11+0
	ldw r1, r17+0
	beq r2, r1, .LBB6_19
.LBB6_21:
	slli r2, r2, 4
	lui r3, %hi(__mmio_base+12300)
	addi r3, r3, %lo(__mmio_base+12300)
	add r2, r2, r3
	ldw r15, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r17+0, r1
	beq r15, r14, .LBB6_22
.LBB6_23:
	bgeu r15, r16, .LBB6_25
.LBB6_24:
	addi r1, r0, 1
	stw r20+12, r1
.LBB6_25:
	lui r4, %hi(__mmio_base+16384)
	addi r4, r4, %lo(__mmio_base+16384)
	add r3, r13, r0
	add r5, r15, r0
	jal r31, memcpy
	seq r1, r15, r16
	sub r1, r14, r1
	and r14, r19, r1
	jal r0, .LBB6_26
.LBB6_4:
	stw fp+-80, r6
	stw fp+-84, r5
	addi r14, r0, 0
	lui r15, %hi(__mmio_base)
	addi r15, r15, %lo(__mmio_base)
	lui r17, %hi(__mmio_base+4)
	addi r17, r17, %lo(__mmio_base+4)
	lui r18, %hi(__mmio_base+4096)
	addi r18, r18, %lo(__mmio_base+4096)
	addi r19, r0, 2
	lui r20, %hi(__mmio_base+4100)
	addi r20, r20, %lo(__mmio_base+4100)
	lui r21, %hi(__mmio_base+4104)
	addi r21, r21, %lo(__mmio_base+4104)
	lui r22, %hi(__mmio_base+4108)
	addi r22, r22, %lo(__mmio_base+4108)
	lui r23, %hi(__mmio_base+8192)
	addi r23, r23, %lo(__mmio_base+8192)
	lui r24, %hi(__mmio_base+8196)
	addi r24, r24, %lo(__mmio_base+8196)
	lui r25, %hi(__mmio_base+12300)
	addi r25, r25, %lo(__mmio_base+12300)
	addi r26, r0, -1
	lui r27, %hi(__mmio_base+16384)
	addi r27, r27, %lo(__mmio_base+16384)
	add r28, r14, r0
.LBB6_5:
	ldw r12, r15+0
	addi r1, r12, 1
	andi r11, r1, 255
	ldw r1, r17+0
	bne r11, r1, .LBB6_8
.LBB6_6:
	jal r31, yield
	ldw r1, r17+0
	beq r11, r1, .LBB6_6
.LBB6_8:
	slli r1, r12, 4
	add r2, r1, r18
	stw r2+0, r19
	add r2, r1, r20
	stw r2+0, r14
	add r2, r1, r21
	stw r2+0, r14
	add r1, r1, r22
	stw r1+0, r14
	stw r15+0, r11
.LBB6_10:
	ldw r2, r23+0
	ldw r1, r24+0
	bne r2, r1, .LBB6_11
.LBB6_9:
	jal r31, yield
	jal r0, .LBB6_10
.LBB6_11:
	slli r2, r2, 4
	add r2, r2, r25
	ldw r2, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r24+0, r1
	beq r2, r26, .LBB6_14
.LBB6_12:
	ldbu r1, r27+0
	add r2, r13, r28
	stb r2+0, r1
	addi r28, r28, 1
	bne r28, r16, .LBB6_5
.LBB6_13:
	ldw r14, fp+-84
	jal r0, .LBB6_26
.LBB6_22:
	addi r1, r0, 1
	stw r20+12, r1
	jal r0, .LBB6_26
.LBB6_14:
	addi r1, r0, 1
	ldw r2, fp+-80
	stw r2+12, r1
	addi r14, r0, 0
.LBB6_26:
	add r1, r14, r0
	ldw lr, fp+-76
	ldw r28, fp+-72
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
	addi sp, sp, 104
	jalr r0, r31, 0
.Lfunc_end6:
	.size	fread, .Lfunc_end6-fread
                                        # -- End function
	.globl	fgetc                           # -- Begin function fgetc
	.p2align	2
	.type	fgetc,@function
fgetc:                                  # @fgetc
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r6, r3, r0
	addi r11, fp, -12
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r4, r0
	jal r31, fread
	ori r2, r0, 1
	seq r1, r1, r2
	ldbu r2, r11+0
	addi r3, r0, 0
	sub r1, r3, r1
	addi r3, r0, -1
	xor r2, r2, r3
	and r1, r2, r1
	xor r1, r1, r3
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end7:
	.size	fgetc, .Lfunc_end7-fgetc
                                        # -- End function
	.globl	getc                            # -- Begin function getc
	.p2align	2
	.type	getc,@function
getc:                                   # @getc
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r6, r3, r0
	addi r11, fp, -12
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r4, r0
	jal r31, fread
	ori r2, r0, 1
	seq r1, r1, r2
	ldbu r2, r11+0
	addi r3, r0, 0
	sub r1, r3, r1
	addi r3, r0, -1
	xor r2, r2, r3
	and r1, r2, r1
	xor r1, r1, r3
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end8:
	.size	getc, .Lfunc_end8-getc
                                        # -- End function
	.globl	fputc                           # -- Begin function fputc
	.p2align	2
	.type	fputc,@function
fputc:                                  # @fputc
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r6, r4, r0
	add r11, r3, r0
	addi r3, fp, -12
	stb r3+0, r11
	addi r4, r0, 1
	add r5, r4, r0
	jal r31, fwrite
	ori r2, r0, 1
	seq r1, r1, r2
	addi r2, r0, 0
	sub r1, r2, r1
	addi r2, r0, -1
	xor r3, r11, r2
	and r1, r3, r1
	xor r1, r1, r2
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end9:
	.size	fputc, .Lfunc_end9-fputc
                                        # -- End function
	.globl	putc                            # -- Begin function putc
	.p2align	2
	.type	putc,@function
putc:                                   # @putc
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r6, r4, r0
	add r11, r3, r0
	addi r3, fp, -12
	stb r3+0, r11
	addi r4, r0, 1
	add r5, r4, r0
	jal r31, fwrite
	ori r2, r0, 1
	seq r1, r1, r2
	addi r2, r0, 0
	sub r1, r2, r1
	addi r2, r0, -1
	xor r3, r11, r2
	and r1, r3, r1
	xor r1, r1, r2
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end10:
	.size	putc, .Lfunc_end10-putc
                                        # -- End function
	.globl	fgets                           # -- Begin function fgets
	.p2align	2
	.type	fgets,@function
fgets:                                  # @fgets
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
	stw fp+-32, lr
	addi r1, r0, 0
	beq r3, r1, .LBB11_9
.LBB11_1:
	addi r13, r0, 1
	blt r4, r13, .LBB11_9
.LBB11_2:
	add r12, r5, r0
	add r11, r3, r0
	addi r15, r4, -1
	addi r16, r0, 0
	addi r14, fp, -36
	addi r17, r0, 10
.LBB11_3:
	beq r15, r16, .LBB11_4
.LBB11_5:
	add r3, r14, r0
	add r4, r13, r0
	add r5, r13, r0
	add r6, r12, r0
	jal r31, fread
	bne r1, r13, .LBB11_6
.LBB11_7:
	ldbu r1, r14+0
	addi r2, r16, 1
	add r3, r11, r16
	stb r3+0, r1
	andi r1, r1, 255
	add r16, r2, r0
	bne r1, r17, .LBB11_3
	jal r0, .LBB11_8
.LBB11_4:
	add r2, r15, r0
	jal r0, .LBB11_8
.LBB11_6:
	addi r1, r0, 0
	add r2, r16, r0
	beq r16, r1, .LBB11_9
.LBB11_8:
	add r1, r11, r2
	addi r2, r0, 0
	stb r1+0, r2
	add r1, r11, r0
.LBB11_9:
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
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end11:
	.size	fgets, .Lfunc_end11-fgets
                                        # -- End function
	.globl	fputs                           # -- Begin function fputs
	.p2align	2
	.type	fputs,@function
fputs:                                  # @fputs
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
	add r11, r4, r0
	add r12, r3, r0
	jal r31, strlen
	add r13, r1, r0
	addi r4, r0, 1
	add r3, r12, r0
	add r5, r1, r0
	add r6, r11, r0
	jal r31, fwrite
	sne r1, r1, r13
	addi r2, r0, 0
	sub r1, r2, r1
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end12:
	.size	fputs, .Lfunc_end12-fputs
                                        # -- End function
	.globl	fseek                           # -- Begin function fseek
	.p2align	2
	.type	fseek,@function
fseek:                                  # @fseek
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
	addi r12, r0, 0
	beq r3, r12, .LBB13_1
.LBB13_2:
	add r11, r3, r0
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	stb r1+0, r5
	lui r13, %hi(__mmio_base)
	addi r13, r13, %lo(__mmio_base)
	lui r1, %hi(__mmio_base+16388)
	addi r1, r1, %lo(__mmio_base+16388)
	stw r1+0, r4
	ldw r14, r3+0
	ldw r16, r13+0
	addi r1, r16, 1
	andi r15, r1, 255
	lui r17, %hi(__mmio_base+4)
	addi r17, r17, %lo(__mmio_base+4)
	ldw r1, r17+0
	bne r15, r1, .LBB13_5
.LBB13_3:
	jal r31, yield
	ldw r1, r17+0
	beq r15, r1, .LBB13_3
.LBB13_5:
	slli r1, r16, 4
	lui r2, %hi(__mmio_base+4096)
	addi r2, r2, %lo(__mmio_base+4096)
	add r2, r1, r2
	addi r3, r0, 7
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4100)
	addi r2, r2, %lo(__mmio_base+4100)
	add r2, r1, r2
	addi r3, r0, 8
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4104)
	addi r2, r2, %lo(__mmio_base+4104)
	add r2, r1, r2
	stw r2+0, r12
	lui r2, %hi(__mmio_base+4108)
	addi r2, r2, %lo(__mmio_base+4108)
	add r1, r1, r2
	stw r1+0, r14
	stw r13+0, r15
	lui r14, %hi(__mmio_base+8192)
	addi r14, r14, %lo(__mmio_base+8192)
	ldw r2, r14+0
	lui r13, %hi(__mmio_base+8196)
	addi r13, r13, %lo(__mmio_base+8196)
	ldw r1, r13+0
	bne r2, r1, .LBB13_8
.LBB13_6:
	jal r31, yield
	ldw r2, r14+0
	ldw r1, r13+0
	beq r2, r1, .LBB13_6
.LBB13_8:
	slli r2, r2, 4
	lui r3, %hi(__mmio_base+12300)
	addi r3, r3, %lo(__mmio_base+12300)
	add r2, r2, r3
	ldw r2, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r13+0, r1
	ori r1, r0, 0
	slt r1, r2, r1
	sub r1, r12, r1
	andi r1, r1, 4
	xori r3, r1, 12
	srli r4, r2, 31
	srai r1, r2, 31
	add r2, r11, r3
	stw r2+0, r4
	jal r0, .LBB13_9
.LBB13_1:
	addi r1, r0, -1
.LBB13_9:
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
.Lfunc_end13:
	.size	fseek, .Lfunc_end13-fseek
                                        # -- End function
	.globl	ftell                           # -- Begin function ftell
	.p2align	2
	.type	ftell,@function
ftell:                                  # @ftell
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
	addi r12, r0, 0
	beq r3, r12, .LBB14_1
.LBB14_2:
	add r11, r3, r0
	addi r1, r0, 1
	lui r2, %hi(__mmio_base+16384)
	addi r2, r2, %lo(__mmio_base+16384)
	stb r2+0, r1
	lui r13, %hi(__mmio_base)
	addi r13, r13, %lo(__mmio_base)
	lui r1, %hi(__mmio_base+16388)
	addi r1, r1, %lo(__mmio_base+16388)
	stw r1+0, r12
	ldw r14, r3+0
	ldw r16, r13+0
	addi r1, r16, 1
	andi r15, r1, 255
	lui r17, %hi(__mmio_base+4)
	addi r17, r17, %lo(__mmio_base+4)
	ldw r1, r17+0
	bne r15, r1, .LBB14_5
.LBB14_3:
	jal r31, yield
	ldw r1, r17+0
	beq r15, r1, .LBB14_3
.LBB14_5:
	slli r1, r16, 4
	lui r2, %hi(__mmio_base+4096)
	addi r2, r2, %lo(__mmio_base+4096)
	add r2, r1, r2
	addi r3, r0, 7
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4100)
	addi r2, r2, %lo(__mmio_base+4100)
	add r2, r1, r2
	addi r3, r0, 8
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4104)
	addi r2, r2, %lo(__mmio_base+4104)
	add r2, r1, r2
	stw r2+0, r12
	lui r2, %hi(__mmio_base+4108)
	addi r2, r2, %lo(__mmio_base+4108)
	add r1, r1, r2
	stw r1+0, r14
	stw r13+0, r15
	lui r14, %hi(__mmio_base+8192)
	addi r14, r14, %lo(__mmio_base+8192)
	ldw r2, r14+0
	lui r13, %hi(__mmio_base+8196)
	addi r13, r13, %lo(__mmio_base+8196)
	ldw r1, r13+0
	bne r2, r1, .LBB14_8
.LBB14_6:
	jal r31, yield
	ldw r2, r14+0
	ldw r1, r13+0
	beq r2, r1, .LBB14_6
.LBB14_8:
	slli r2, r2, 4
	lui r3, %hi(__mmio_base+12300)
	addi r3, r3, %lo(__mmio_base+12300)
	add r2, r2, r3
	ldw r2, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r13+0, r1
	ori r1, r0, 0
	slt r1, r2, r1
	sub r1, r12, r1
	andi r1, r1, 4
	xori r3, r1, 12
	srli r4, r2, 31
	srai r1, r2, 31
	add r2, r11, r3
	stw r2+0, r4
	jal r0, .LBB14_9
.LBB14_1:
	addi r1, r0, -1
.LBB14_9:
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
.Lfunc_end14:
	.size	ftell, .Lfunc_end14-ftell
                                        # -- End function
	.globl	rewind                          # -- Begin function rewind
	.p2align	2
	.type	rewind,@function
rewind:                                 # @rewind
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
	addi r12, r0, 0
	beq r3, r12, .LBB15_8
.LBB15_1:
	add r11, r3, r0
	lui r1, %hi(__mmio_base+16384)
	addi r1, r1, %lo(__mmio_base+16384)
	stb r1+0, r12
	lui r13, %hi(__mmio_base)
	addi r13, r13, %lo(__mmio_base)
	lui r1, %hi(__mmio_base+16388)
	addi r1, r1, %lo(__mmio_base+16388)
	stw r1+0, r12
	ldw r14, r3+0
	ldw r16, r13+0
	addi r1, r16, 1
	andi r15, r1, 255
	lui r17, %hi(__mmio_base+4)
	addi r17, r17, %lo(__mmio_base+4)
	ldw r1, r17+0
	bne r15, r1, .LBB15_4
.LBB15_2:
	jal r31, yield
	ldw r1, r17+0
	beq r15, r1, .LBB15_2
.LBB15_4:
	slli r1, r16, 4
	lui r2, %hi(__mmio_base+4096)
	addi r2, r2, %lo(__mmio_base+4096)
	add r2, r1, r2
	addi r3, r0, 7
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4100)
	addi r2, r2, %lo(__mmio_base+4100)
	add r2, r1, r2
	addi r3, r0, 8
	stw r2+0, r3
	lui r2, %hi(__mmio_base+4104)
	addi r2, r2, %lo(__mmio_base+4104)
	add r2, r1, r2
	stw r2+0, r12
	lui r2, %hi(__mmio_base+4108)
	addi r2, r2, %lo(__mmio_base+4108)
	add r1, r1, r2
	stw r1+0, r14
	stw r13+0, r15
	lui r14, %hi(__mmio_base+8192)
	addi r14, r14, %lo(__mmio_base+8192)
	ldw r2, r14+0
	lui r13, %hi(__mmio_base+8196)
	addi r13, r13, %lo(__mmio_base+8196)
	ldw r1, r13+0
	bne r2, r1, .LBB15_7
.LBB15_5:
	jal r31, yield
	ldw r2, r14+0
	ldw r1, r13+0
	beq r2, r1, .LBB15_5
.LBB15_7:
	slli r2, r2, 4
	lui r3, %hi(__mmio_base+12300)
	addi r3, r3, %lo(__mmio_base+12300)
	add r2, r2, r3
	ldw r2, r2+0
	addi r1, r1, 1
	andi r1, r1, 255
	stw r13+0, r1
	ori r1, r0, 0
	slt r1, r2, r1
	sub r1, r12, r1
	andi r1, r1, 4
	xori r1, r1, 12
	srli r2, r2, 31
	add r1, r11, r1
	stw r1+0, r2
	stw r11+8, r12
	stw r11+12, r12
.LBB15_8:
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
	stw fp+-20, lr
	addi r1, r0, 0
	lui r14, %hi(stderr)
	addi r14, r14, %lo(stderr)
	beq r3, r1, .LBB20_3
.LBB20_1:
	ldbu r2, r3+0
	beq r2, r1, .LBB20_3
.LBB20_2:
	ldw r11, r14+0
	add r12, r3, r0
	jal r31, strlen
	addi r13, r0, 1
	add r3, r12, r0
	add r4, r13, r0
	add r5, r1, r0
	add r6, r11, r0
	jal r31, fwrite
	ldw r6, r14+0
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	addi r4, r0, 2
	add r5, r13, r0
	jal r31, fwrite
.LBB20_3:
	ldw r6, r14+0
	lui r3, %hi(.L.str.1)
	addi r3, r3, %lo(.L.str.1)
	addi r4, r0, 6
	addi r5, r0, 1
	jal r31, fwrite
	ldw lr, fp+-20
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

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	": "
	.size	.L.str, 3

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"error\n"
	.size	.L.str.1, 7

	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git f4f5c1e34ae9343d25641431f4d691b10fec3591)"
	.section	".note.GNU-stack","",@progbits
