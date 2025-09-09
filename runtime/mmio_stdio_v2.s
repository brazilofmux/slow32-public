	.file	"mmio_stdio_v2.c"
	.text
	.globl	putchar                         # -- Begin function putchar
	.p2align	2
	.type	putchar,@function
putchar:                                # @putchar
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
	stw fp+-24, r16
	stw fp+-28, lr
	ori  r1, r0, %lo(putchar.buffer_pos)
	lui r2, %hi(putchar.buffer_pos)
	add r11, r2, r1
	ldw r2, r11+0
	addi r12, r2, 1
	stw r11+0, r12
	ori  r1, r0, %lo(putchar.line_buffer)
	lui r4, %hi(putchar.line_buffer)
	add r1, r4, r1
	add r2, r2, r1
	stb r2+0, r3
	addi r2, r0, 10
	beq r3, r2, .LBB0_2
.LBB0_1:
	addi r2, r0, 256
	sltu r2, r12, r2
	addi r3, r0, 0
	beq r2, r3, .LBB0_2
.LBB0_2:
	addi r2, r0, 0
	beq r12, r2, .LBB0_7
.LBB0_3:
	lui r3, 1
	sltu r3, r12, r3
	sub r3, r2, r3
	xori r4, r12, 4096
	and r3, r4, r3
	xori r3, r3, 4096
	lui r4, 65540
.LBB0_6:
	add r5, r2, r1
	ldbu r5, r5+0
	add r6, r2, r4
	stb r6+0, r5
	addi r2, r2, 1
	beq r3, r2, .LBB0_7
	jal r0, .LBB0_6
.LBB0_7:
	lui r13, 65536
	ldw r15, r13+0
	addi r1, r15, 1
	andi r14, r1, 255
	addi r16, r13, 4
	ldw r1, r16+0
	beq r14, r1, .LBB0_4
	jal r0, .LBB0_5
.LBB0_4:
	jal r31, yield
	ldw r1, r16+0
	beq r14, r1, .LBB0_4
.LBB0_5:
	slli r1, r15, 4
	lui r2, 65537
	add r1, r1, r2
	addi r2, r0, 3
	stw r1+0, r2
	ori  r2, r1, 4
	stw r2+0, r12
	ori  r2, r1, 8
	addi r12, r0, 0
	stw r2+0, r12
	ori  r1, r1, 12
	addi r2, r0, 1
	stw r1+0, r2
	stw r13+0, r14
	jal r31, yield
	lui r15, 65538
	addi r13, r15, 4
	ldw r14, r13+0
	ldw r1, r15+0
	beq r1, r14, .LBB0_8
	jal r0, .LBB0_9
.LBB0_8:
	jal r31, yield
	ldw r1, r15+0
	beq r1, r14, .LBB0_8
.LBB0_9:
	slli r1, r14, 4
	lui r2, 65539
	addi r2, r2, 12
	add r1, r1, r2
	ldw r1, r1+0
	addi r1, r14, 1
	andi r1, r1, 255
	stw r13+0, r1
	stw r11+0, r12
.LBB0_10:
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
	addi fp, sp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, lr
	lui r11, 65536
	ldw r13, r11+0
	addi r1, r13, 1
	andi r12, r1, 255
	addi r14, r11, 4
	ldw r1, r14+0
	beq r12, r1, .LBB1_1
.LBB1_1:
	jal r31, yield
	ldw r1, r14+0
	beq r12, r1, .LBB1_1
.LBB1_2:
	slli r1, r13, 4
	lui r2, 65537
	add r1, r1, r2
	addi r2, r0, 2
	stw r1+0, r2
	ori  r2, r1, 4
	addi r3, r0, 1
	stw r2+0, r3
	ori  r2, r1, 8
	addi r3, r0, 0
	stw r2+0, r3
	ori  r1, r1, 12
	stw r1+0, r3
	stw r11+0, r12
	jal r31, yield
	lui r13, 65538
	addi r11, r13, 4
	ldw r12, r11+0
	ldw r1, r13+0
	beq r1, r12, .LBB1_3
.LBB1_3:
	jal r31, yield
	ldw r1, r13+0
	beq r1, r12, .LBB1_3
.LBB1_4:
	slli r1, r12, 4
	lui r2, 65539
	addi r2, r2, 12
	add r1, r1, r2
	ldw r1, r1+0
	addi r1, r12, 1
	andi r1, r1, 255
	stw r11+0, r1
	lui r1, 65540
	ldbu r1, r1+0
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
	.p2align	2                               # -- Begin function mmio_request
	.type	mmio_request,@function
mmio_request:                           # @mmio_request
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
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, lr
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	lui r14, 65536
	ldw r16, r14+0
	addi r1, r16, 1
	andi r15, r1, 255
	addi r17, r14, 4
	ldw r1, r17+0
	beq r15, r1, .LBB2_1
.LBB2_1:
	jal r31, yield
	ldw r1, r17+0
	beq r15, r1, .LBB2_1
.LBB2_2:
	slli r1, r16, 4
	lui r2, 65537
	add r1, r1, r2
	stw r1+0, r13
	ori  r2, r1, 4
	stw r2+0, r12
	ori  r2, r1, 8
	addi r3, r0, 0
	stw r2+0, r3
	ori  r1, r1, 12
	stw r1+0, r11
	stw r14+0, r15
	jal r31, yield
	lui r13, 65538
	addi r11, r13, 4
	ldw r12, r11+0
	ldw r1, r13+0
	beq r1, r12, .LBB2_3
.LBB2_3:
	jal r31, yield
	ldw r1, r13+0
	beq r1, r12, .LBB2_3
.LBB2_4:
	slli r1, r12, 4
	lui r2, 65539
	addi r2, r2, 12
	add r1, r1, r2
	ldw r1, r1+0
	addi r1, r12, 1
	andi r1, r1, 255
	stw r11+0, r1
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
.Lfunc_end2:
	.size	mmio_request, .Lfunc_end2-mmio_request
                                        # -- End function
	.globl	puts                            # -- Begin function puts
	.p2align	2
	.type	puts,@function
puts:                                   # @puts
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
	stw fp+-24, r16
	stw fp+-28, lr
	addi r1, r0, 0
	addi r2, r0, 1024
	add r4, r1, r0
.LBB3_1:
	add r5, r3, r4
	ldbu r5, r5+0
	addi r11, r4, 1
	beq r5, r1, .LBB3_3
.LBB3_2:
	sltu r5, r4, r2
	add r4, r11, r0
	beq r5, r1, .LBB3_3
.LBB3_3:
	addi r12, r0, 1
	lui r1, 65540
	beq r11, r12, .LBB3_10
.LBB3_4:
	addi r2, r11, -1
	addi r4, r0, 0
.LBB3_9:
	add r5, r3, r4
	ldbu r5, r5+0
	add r6, r4, r1
	stb r6+0, r5
	addi r4, r4, 1
	beq r2, r4, .LBB3_10
	jal r0, .LBB3_9
.LBB3_10:
	addi r1, r1, 4095
	add r1, r11, r1
	addi r2, r0, 10
	stb r1+0, r2
	lui r13, 65536
	ldw r15, r13+0
	addi r1, r15, 1
	andi r14, r1, 255
	addi r16, r13, 4
	ldw r1, r16+0
	beq r14, r1, .LBB3_5
	jal r0, .LBB3_6
.LBB3_5:
	jal r31, yield
	ldw r1, r16+0
	beq r14, r1, .LBB3_5
.LBB3_6:
	slli r1, r15, 4
	lui r2, 65537
	add r1, r1, r2
	addi r2, r0, 3
	stw r1+0, r2
	ori  r2, r1, 4
	stw r2+0, r11
	ori  r2, r1, 8
	addi r3, r0, 0
	stw r2+0, r3
	ori  r1, r1, 12
	stw r1+0, r12
	stw r13+0, r14
	jal r31, yield
	lui r13, 65538
	addi r11, r13, 4
	ldw r12, r11+0
	ldw r1, r13+0
	beq r1, r12, .LBB3_7
.LBB3_7:
	jal r31, yield
	ldw r1, r13+0
	beq r1, r12, .LBB3_7
.LBB3_8:
	slli r1, r12, 4
	lui r2, 65539
	addi r2, r2, 12
	add r1, r1, r2
	ldw r1, r1+0
	addi r1, r12, 1
	andi r1, r1, 255
	stw r11+0, r1
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
.Lfunc_end3:
	.size	puts, .Lfunc_end3-puts
                                        # -- End function
	.globl	write                           # -- Begin function write
	.p2align	2
	.type	write,@function
write:                                  # @write
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
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, lr
	add r12, r3, r0
	lui r1, 1
	sltu r1, r5, r1
	addi r13, r0, 0
	sub r1, r13, r1
	xori r2, r5, 4096
	and r1, r2, r1
	xori r11, r1, 4096
	beq r5, r13, .LBB4_7
.LBB4_1:
	lui r1, 65540
	add r2, r11, r0
.LBB4_6:
	ldbu r3, r4+0
	stb r1+0, r3
	addi r2, r2, -1
	addi r1, r1, 1
	addi r4, r4, 1
	beq r2, r13, .LBB4_7
	jal r0, .LBB4_6
.LBB4_7:
	lui r14, 65536
	ldw r16, r14+0
	addi r1, r16, 1
	andi r15, r1, 255
	addi r17, r14, 4
	ldw r1, r17+0
	beq r15, r1, .LBB4_2
	jal r0, .LBB4_3
.LBB4_2:
	jal r31, yield
	ldw r1, r17+0
	beq r15, r1, .LBB4_2
.LBB4_3:
	slli r1, r16, 4
	lui r2, 65537
	add r1, r1, r2
	addi r2, r0, 3
	stw r1+0, r2
	ori  r2, r1, 4
	stw r2+0, r11
	ori  r2, r1, 8
	stw r2+0, r13
	ori  r1, r1, 12
	stw r1+0, r12
	stw r14+0, r15
	jal r31, yield
	lui r14, 65538
	addi r12, r14, 4
	ldw r13, r12+0
	ldw r1, r14+0
	beq r1, r13, .LBB4_4
.LBB4_4:
	jal r31, yield
	ldw r1, r14+0
	beq r1, r13, .LBB4_4
.LBB4_5:
	slli r1, r13, 4
	lui r2, 65539
	addi r2, r2, 12
	add r1, r1, r2
	ldw r1, r1+0
	addi r1, r13, 1
	andi r1, r1, 255
	stw r12+0, r1
	add r1, r11, r0
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
	.size	write, .Lfunc_end4-write
                                        # -- End function
	.globl	read                            # -- Begin function read
	.p2align	2
	.type	read,@function
read:                                   # @read
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 56
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, lr
	add r11, r4, r0
	add r12, r3, r0
	lui r1, 1
	sltu r1, r5, r1
	addi r13, r0, 0
	sub r1, r13, r1
	xori r2, r5, 4096
	and r1, r2, r1
	xori r14, r1, 4096
	lui r15, 65536
	ldw r17, r15+0
	addi r1, r17, 1
	andi r16, r1, 255
	addi r18, r15, 4
	ldw r1, r18+0
	beq r16, r1, .LBB5_1
.LBB5_1:
	jal r31, yield
	ldw r1, r18+0
	beq r16, r1, .LBB5_1
.LBB5_2:
	slli r1, r17, 4
	lui r2, 65537
	add r1, r1, r2
	addi r2, r0, 4
	stw r1+0, r2
	ori  r2, r1, 4
	stw r2+0, r14
	ori  r2, r1, 8
	stw r2+0, r13
	ori  r1, r1, 12
	stw r1+0, r12
	stw r15+0, r16
	jal r31, yield
	lui r16, 65538
	addi r12, r16, 4
	ldw r15, r12+0
	ldw r1, r16+0
	beq r1, r15, .LBB5_3
.LBB5_3:
	jal r31, yield
	ldw r1, r16+0
	beq r1, r15, .LBB5_3
.LBB5_4:
	slli r1, r15, 4
	lui r2, 65539
	addi r2, r2, 12
	add r1, r1, r2
	ldw r1, r1+0
	addi r2, r15, 1
	andi r2, r2, 255
	stw r12+0, r2
	xor r2, r14, r1
	sltu r3, r14, r1
	sub r3, r13, r3
	and r2, r2, r3
	xor r2, r1, r2
	beq r2, r13, .LBB5_7
.LBB5_5:
	lui r3, 65540
.LBB5_6:
	ldbu r4, r3+0
	stb r11+0, r4
	addi r2, r2, -1
	addi r11, r11, 1
	addi r3, r3, 1
	beq r2, r13, .LBB5_7
	jal r0, .LBB5_6
.LBB5_7:
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
.Lfunc_end5:
	.size	read, .Lfunc_end5-read
                                        # -- End function
	.globl	exit                            # -- Begin function exit
	.p2align	2
	.type	exit,@function
exit:                                   # @exit
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	add r5, r3, r0
	addi r3, r0, 9
	addi r4, r0, 0
	jal r31, mmio_request
	jal r31, halt
.Lfunc_end6:
	.size	exit, .Lfunc_end6-exit
                                        # -- End function
	.globl	fflush_stdout                   # -- Begin function fflush_stdout
	.p2align	2
	.type	fflush_stdout,@function
fflush_stdout:                          # @fflush_stdout
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, lr
	lui r11, 65536
	ldw r13, r11+0
	addi r1, r13, 1
	andi r12, r1, 255
	addi r14, r11, 4
	ldw r1, r14+0
	beq r12, r1, .LBB7_1
.LBB7_1:
	jal r31, yield
	ldw r1, r14+0
	beq r12, r1, .LBB7_1
.LBB7_2:
	slli r1, r13, 4
	lui r2, 65537
	add r1, r1, r2
	addi r2, r0, 11
	stw r1+0, r2
	ori  r2, r1, 4
	addi r3, r0, 0
	stw r2+0, r3
	ori  r2, r1, 8
	stw r2+0, r3
	ori  r1, r1, 12
	stw r1+0, r3
	stw r11+0, r12
	jal r31, yield
	lui r13, 65538
	addi r11, r13, 4
	ldw r12, r11+0
	ldw r1, r13+0
	beq r1, r12, .LBB7_3
.LBB7_3:
	jal r31, yield
	ldw r1, r13+0
	beq r1, r12, .LBB7_3
.LBB7_4:
	slli r1, r12, 4
	lui r2, 65539
	addi r2, r2, 12
	add r1, r1, r2
	ldw r1, r1+0
	addi r1, r12, 1
	andi r1, r1, 255
	stw r11+0, r1
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
	.size	fflush_stdout, .Lfunc_end7-fflush_stdout
                                        # -- End function
	.globl	sbrk                            # -- Begin function sbrk
	.p2align	2
	.type	sbrk,@function
sbrk:                                   # @sbrk
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 56
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
	stw fp+-48, lr
	add r11, r3, r0
	ori  r1, r0, %lo(heap_current)
	lui r2, %hi(heap_current)
	add r13, r2, r1
	ldw r1, r13+0
	addi r17, r0, 0
	lui r16, 65536
	lui r18, 65537
	lui r15, 65538
	lui r14, 65539
	beq r1, r17, .LBB8_1
	jal r0, .LBB8_6
.LBB8_1:
	ldw r19, r16+0
	addi r1, r19, 1
	andi r12, r1, 255
	addi r20, r16, 4
	ldw r1, r20+0
	beq r12, r1, .LBB8_2
.LBB8_2:
	jal r31, yield
	ldw r1, r20+0
	beq r12, r1, .LBB8_2
.LBB8_3:
	slli r1, r19, 4
	add r1, r1, r18
	addi r2, r0, 8
	stw r1+0, r2
	ori  r2, r1, 4
	stw r2+0, r17
	ori  r2, r1, 8
	stw r2+0, r17
	ori  r1, r1, 12
	stw r1+0, r17
	stw r16+0, r12
	jal r31, yield
	addi r12, r15, 4
	ldw r19, r12+0
	ldw r1, r15+0
	beq r1, r19, .LBB8_4
.LBB8_4:
	jal r31, yield
	ldw r1, r15+0
	beq r1, r19, .LBB8_4
.LBB8_5:
	slli r1, r19, 4
	addi r2, r14, 12
	add r1, r1, r2
	ldw r1, r1+0
	addi r2, r19, 1
	andi r2, r2, 255
	stw r12+0, r2
	stw r13+0, r1
.LBB8_6:
	ldw r12, r13+0
	beq r11, r17, .LBB8_14
.LBB8_7:
	add r11, r12, r11
	ldw r20, r16+0
	addi r1, r20, 1
	andi r19, r1, 255
	addi r21, r16, 4
	ldw r1, r21+0
	beq r19, r1, .LBB8_8
.LBB8_8:
	jal r31, yield
	ldw r1, r21+0
	beq r19, r1, .LBB8_8
.LBB8_9:
	slli r1, r20, 4
	add r1, r1, r18
	addi r2, r0, 8
	stw r1+0, r2
	ori  r2, r1, 4
	stw r2+0, r17
	ori  r2, r1, 8
	stw r2+0, r17
	ori  r1, r1, 12
	stw r1+0, r11
	stw r16+0, r19
	jal r31, yield
	addi r16, r15, 4
	ldw r17, r16+0
	ldw r1, r15+0
	beq r1, r17, .LBB8_10
.LBB8_10:
	jal r31, yield
	ldw r1, r15+0
	beq r1, r17, .LBB8_10
.LBB8_11:
	slli r1, r17, 4
	addi r2, r14, 12
	add r1, r1, r2
	ldw r1, r1+0
	addi r2, r17, 1
	andi r2, r2, 255
	stw r16+0, r2
	beq r1, r11, .LBB8_13
.LBB8_12:
	addi r12, r0, -1
	jal r0, .LBB8_14
.LBB8_13:
	stw r13+0, r11
.LBB8_14:
	add r1, r12, r0
	ldw lr, fp+-48
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
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end8:
	.size	sbrk, .Lfunc_end8-sbrk
                                        # -- End function
	.globl	malloc                          # -- Begin function malloc
	.p2align	2
	.type	malloc,@function
malloc:                                 # @malloc
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	addi fp, sp, 24
	stw fp+-4, lr
	addi r1, r3, 7
	addi r2, r0, -8
	and r3, r1, r2
	jal r31, sbrk
	ori r2, r0, -1
	seq r2, r1, r2
	addi r3, r0, 0
	sub r2, r3, r2
	and r2, r1, r2
	xor r1, r1, r2
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end9:
	.size	malloc, .Lfunc_end9-malloc
                                        # -- End function
	.globl	free                            # -- Begin function free
	.p2align	2
	.type	free,@function
free:                                   # @free
# %bb.0:
	jalr r0, r31, 0
.Lfunc_end10:
	.size	free, .Lfunc_end10-free
                                        # -- End function
	.type	putchar.line_buffer,@object     # @putchar.line_buffer
	.local	putchar.line_buffer
	.comm	putchar.line_buffer,256,1
	.type	putchar.buffer_pos,@object      # @putchar.buffer_pos
	.local	putchar.buffer_pos
	.comm	putchar.buffer_pos,4,4
	.type	heap_current,@object            # @heap_current
	.local	heap_current
	.comm	heap_current,4,4
	.ident	"clang version 22.0.0git (https://github.com/llvm/llvm-project.git 31563aca0fde0b27cd859f61bf8ce82f8544c7f3)"
	.section	".note.GNU-stack","",@progbits
