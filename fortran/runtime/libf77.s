	.file	"libf77.c"
	.text
	.globl	f77_wr_begin                    # -- Begin function f77_wr_begin
	.p2align	2
	.type	f77_wr_begin,@function
f77_wr_begin:                           # @f77_wr_begin
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	add r5, r3, r0
	addi r1, r0, 31
	bgtu r3, r1, .LBB0_2
.LBB0_1:
	slli r1, r5, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r1, r1, r3
	ldw r11, r1+0
	addi r1, r0, 0
	bne r11, r1, .LBB0_8
.LBB0_2:
	addi r11, r0, 0
	beq r5, r11, .LBB0_6
.LBB0_3:
	addi r1, r0, 5
	beq r5, r1, .LBB0_5
.LBB0_4:
	addi r1, r0, 6
	bne r5, r1, .LBB0_7
.LBB0_5:
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r11, r1+0
	jal r0, .LBB0_8
.LBB0_6:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r11, r1+0
	jal r0, .LBB0_8
.LBB0_7:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r1, %hi(.L.str.24)
	addi r1, r1, %lo(.L.str.24)
	lui r6, %hi(.L.str.25)
	addi r6, r6, %lo(.L.str.25)
	add r12, r4, r0
	add r4, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r4, r12, r0
.LBB0_8:
	lui r1, %hi(fio_out)
	addi r1, r1, %lo(fio_out)
	stw r1+0, r11
	lui r3, %hi(fio_reading)
	addi r3, r3, %lo(fio_reading)
	addi r1, r0, 0
	stb r3+0, r1
	lui r3, %hi(fio_len)
	addi r3, r3, %lo(fio_len)
	stw r3+0, r1
	lui r3, %hi(fio_gdepth)
	addi r3, r3, %lo(fio_gdepth)
	stw r3+0, r1
	lui r3, %hi(fio_scale)
	addi r3, r3, %lo(fio_scale)
	stw r3+0, r1
	lui r3, %hi(fio_rep)
	addi r3, r3, %lo(fio_rep)
	stw r3+0, r1
	lui r3, %hi(fio_desc)
	addi r3, r3, %lo(fio_desc)
	stw r3+0, r1
	beq r4, r1, .LBB0_14
.LBB0_9:
	lui r3, %hi(fio_listed)
	addi r3, r3, %lo(fio_listed)
	stw r3+0, r1
	lui r3, %hi(fio_fmt)
	addi r3, r3, %lo(fio_fmt)
	stw r3+0, r4
	lui r3, %hi(fio_pos)
	addi r3, r3, %lo(fio_pos)
	stw r3+0, r1
	lui r5, %hi(fio_revert)
	addi r5, r5, %lo(fio_revert)
	addi r6, r0, -1
	stw r5+0, r6
	addi r6, r0, 32
.LBB0_10:
	add r7, r4, r1
	ldbu r7, r7+0
	bne r7, r6, .LBB0_12
.LBB0_11:
	addi r1, r1, 1
	stw r3+0, r1
	jal r0, .LBB0_10
.LBB0_12:
	addi r4, r0, 40
	bne r7, r4, .LBB0_15
.LBB0_13:
	stw r5+0, r1
	addi r1, r1, 1
	stw r3+0, r1
	jal r0, .LBB0_15
.LBB0_14:
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	addi r3, r0, 1
	stw r1+0, r3
	lui r1, %hi(.L.str)
	addi r1, r1, %lo(.L.str)
	lui r3, %hi(fio_fmt)
	addi r3, r3, %lo(fio_fmt)
	stw r3+0, r1
	lui r1, %hi(fio_pos)
	addi r1, r1, %lo(fio_pos)
	addi r3, r0, 0
	stw r1+0, r3
	lui r1, %hi(fio_revert)
	addi r1, r1, %lo(fio_revert)
	addi r3, r0, -1
	stw r1+0, r3
.LBB0_15:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end0:
	.size	f77_wr_begin, .Lfunc_end0-f77_wr_begin
                                        # -- End function
	.globl	f77_wr_i                        # -- Begin function f77_wr_i
	.p2align	2
	.type	f77_wr_i,@function
f77_wr_i:                               # @f77_wr_i
# %bb.0:
	addi sp, sp, -80
	stw sp+0, lr
	stw sp+76, r11
	stw sp+72, r12
	stw sp+68, r13
	stw sp+64, r14
	stw sp+60, r15
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r13, r0, 0
	beq r1, r13, .LBB1_8
.LBB1_1:
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r3, sp, 28
	addi r12, r0, 32
	add r4, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r5, r1+0
	addi r3, r0, 1022
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	bgt r5, r3, .LBB1_3
.LBB1_2:
	addi r6, r5, 1
	stw r1+0, r6
	add r5, r5, r4
	stb r5+0, r12
.LBB1_3:
	ldbu r7, sp+28
	beq r7, r13, .LBB1_25
.LBB1_4:
	ldw r6, r1+0
	addi r5, sp, 28
	addi r5, r5, 1
	jal r0, .LBB1_6
.LBB1_5:
	ldbu r7, r5+0
	addi r5, r5, 1
	beq r7, r13, .LBB1_25
.LBB1_6:
	bgt r6, r3, .LBB1_5
.LBB1_7:
	addi r8, r6, 1
	stw r1+0, r8
	add r6, r6, r4
	stb r6+0, r7
	add r6, r8, r0
	jal r0, .LBB1_5
.LBB1_8:
	lui r14, %hi(fio_desc)
	addi r14, r14, %lo(fio_desc)
	ldw r1, r14+0
	lui r15, %hi(fio_rep)
	addi r15, r15, %lo(fio_rep)
	beq r1, r13, .LBB1_10
.LBB1_9:
	ldw r3, r15+0
	bgt r3, r13, .LBB1_11
.LBB1_10:
	stw r14+0, r13
	jal r31, fio_next_desc
.LBB1_11:
	beq r1, r13, .LBB1_25
.LBB1_12:
	addi r3, r0, 76
	bne r1, r3, .LBB1_20
.LBB1_13:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	addi r1, r0, 2
	blt r4, r1, .LBB1_18
.LBB1_14:
	lui r3, %hi(fio_len)
	addi r3, r3, %lo(fio_len)
	ldw r6, r3+0
	addi r4, r4, 1
	addi r5, r0, 1022
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
	jal r0, .LBB1_16
.LBB1_15:
	addi r4, r4, -1
	ble r4, r1, .LBB1_18
.LBB1_16:
	bgt r6, r5, .LBB1_15
.LBB1_17:
	addi r9, r6, 1
	stw r3+0, r9
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	jal r0, .LBB1_15
.LBB1_18:
	lui r3, %hi(fio_len)
	addi r3, r3, %lo(fio_len)
	ldw r1, r3+0
	addi r4, r0, 1022
	bgt r1, r4, .LBB1_21
.LBB1_19:
	seq r4, r11, r13
	sub r4, r0, r4
	andi r4, r4, 18
	xori r4, r4, 84
	addi r5, r1, 1
	stw r3+0, r5
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	add r1, r1, r3
	stb r1+0, r4
	jal r0, .LBB1_21
.LBB1_20:
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r12, sp, 28
	addi r4, r0, 32
	add r3, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	add r3, r12, r0
	jal r31, fio_field
.LBB1_21:
	ldw r1, r15+0
	addi r3, r0, 1
	blt r1, r3, .LBB1_23
.LBB1_22:
	addi r1, r1, -1
	stw r15+0, r1
.LBB1_23:
	ldw r1, r15+0
	bne r1, r13, .LBB1_25
.LBB1_24:
	stw r14+0, r13
.LBB1_25:
	ldw r15, sp+60
	ldw r14, sp+64
	ldw r13, sp+68
	ldw r12, sp+72
	ldw r11, sp+76
	ldw lr, sp+0
	addi sp, sp, 80
	jalr r0, r31, 0
.Lfunc_end1:
	.size	f77_wr_i, .Lfunc_end1-f77_wr_i
                                        # -- End function
	.p2align	2                               # -- Begin function fio_field
	.type	fio_field,@function
fio_field:                              # @fio_field
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	add r12, r4, r0
	add r11, r3, r0
	jal r31, strlen
	addi r3, r0, 0
	ble r12, r3, .LBB2_6
.LBB2_1:
	bge r12, r1, .LBB2_11
.LBB2_2:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r5, r1+0
	addi r3, r12, 1
	addi r4, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 42
	addi r8, r0, 1
	jal r0, .LBB2_4
.LBB2_3:
	addi r3, r3, -1
	ble r3, r8, .LBB2_21
.LBB2_4:
	bgt r5, r4, .LBB2_3
.LBB2_5:
	addi r9, r5, 1
	stw r1+0, r9
	add r5, r5, r6
	stb r5+0, r7
	add r5, r9, r0
	jal r0, .LBB2_3
.LBB2_6:
	ldbu r6, r11+0
	beq r6, r3, .LBB2_21
.LBB2_7:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r7, r1+0
	addi r4, r11, 1
	addi r5, r0, 1022
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	jal r0, .LBB2_9
.LBB2_8:
	ldbu r6, r4+0
	addi r4, r4, 1
	beq r6, r3, .LBB2_21
.LBB2_9:
	bgt r7, r5, .LBB2_8
.LBB2_10:
	addi r9, r7, 1
	stw r1+0, r9
	add r7, r7, r8
	stb r7+0, r6
	add r7, r9, r0
	jal r0, .LBB2_8
.LBB2_11:
	sub r6, r12, r1
	addi r5, r0, 1
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	blt r6, r5, .LBB2_16
.LBB2_12:
	ldw r8, r1+0
	addi r6, r6, 1
	addi r7, r0, 1022
	addi r9, r0, 32
	jal r0, .LBB2_14
.LBB2_13:
	addi r6, r6, -1
	ble r6, r5, .LBB2_16
.LBB2_14:
	bgt r8, r7, .LBB2_13
.LBB2_15:
	addi r10, r8, 1
	stw r1+0, r10
	add r8, r8, r4
	stb r8+0, r9
	add r8, r10, r0
	jal r0, .LBB2_13
.LBB2_16:
	ldbu r8, r11+0
	beq r8, r3, .LBB2_21
.LBB2_17:
	ldw r7, r1+0
	addi r5, r11, 1
	addi r6, r0, 1022
	jal r0, .LBB2_19
.LBB2_18:
	ldbu r8, r5+0
	addi r5, r5, 1
	beq r8, r3, .LBB2_21
.LBB2_19:
	bgt r7, r6, .LBB2_18
.LBB2_20:
	addi r9, r7, 1
	stw r1+0, r9
	add r7, r7, r4
	stb r7+0, r8
	add r7, r9, r0
	jal r0, .LBB2_18
.LBB2_21:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end2:
	.size	fio_field, .Lfunc_end2-fio_field
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_wr_d
	.type	.LCPI3_0,@object
.LCPI3_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI3_0, 8
	.type	.LCPI3_1,@object
.LCPI3_1:
	.quad	0x4024000000000000              # double 10
	.size	.LCPI3_1, 8
	.type	.LCPI3_2,@object
.LCPI3_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI3_2, 8
	.type	.LCPI3_3,@object
.LCPI3_3:
	.quad	0x3fb999999999999a              # double 0.10000000000000001
	.size	.LCPI3_3, 8
	.text
	.globl	f77_wr_d
	.p2align	2
	.type	f77_wr_d,@function
f77_wr_d:                               # @f77_wr_d
# %bb.0:
	addi sp, sp, -160
	stw sp+0, lr
	stw sp+156, r11
	stw sp+152, r12
	stw sp+148, r13
	stw sp+144, r14
	stw sp+140, r15
	stw sp+136, r16
	stw sp+132, r17
	stw sp+128, r18
	stw sp+124, r19
	stw sp+120, r20
	stw sp+116, r21
	stw sp+112, r22
	stw sp+108, r23
	stw sp+104, r24
	stw sp+100, r25
	add r13, r4, r0
	add r12, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r14, r0, 0
	beq r1, r14, .LBB3_8
.LBB3_1:
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	addi r3, sp, 36
	addi r4, r0, 64
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r5, r1+0
	addi r3, r0, 1022
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	bgt r5, r3, .LBB3_3
.LBB3_2:
	addi r6, r5, 1
	stw r1+0, r6
	add r5, r5, r4
	addi r6, r0, 32
	stb r5+0, r6
.LBB3_3:
	ldbu r7, sp+36
	beq r7, r14, .LBB3_54
.LBB3_4:
	ldw r6, r1+0
	addi r5, sp, 36
	addi r5, r5, 1
	jal r0, .LBB3_6
.LBB3_5:
	ldbu r7, r5+0
	addi r5, r5, 1
	beq r7, r14, .LBB3_54
.LBB3_6:
	bgt r6, r3, .LBB3_5
.LBB3_7:
	addi r8, r6, 1
	stw r1+0, r8
	add r6, r6, r4
	stb r6+0, r7
	add r6, r8, r0
	jal r0, .LBB3_5
.LBB3_8:
	lui r16, %hi(fio_desc)
	addi r16, r16, %lo(fio_desc)
	ldw r1, r16+0
	lui r17, %hi(fio_rep)
	addi r17, r17, %lo(fio_rep)
	beq r1, r14, .LBB3_21
.LBB3_9:
	ldw r3, r17+0
	addi r4, r0, 0
	ble r3, r4, .LBB3_21
.LBB3_10:
	addi r3, r0, 70
	bgt r1, r3, .LBB3_22
.LBB3_11:
	addi r4, r0, 0
	beq r1, r4, .LBB3_54
.LBB3_12:
	bne r1, r3, .LBB3_25
.LBB3_13:
	lui r1, %hi(fio_scale)
	addi r1, r1, %lo(fio_scale)
	ldw r3, r1+0
	addi r1, r0, 1
	blt r3, r1, .LBB3_17
.LBB3_14:
	addi r3, r3, 1
	lui r5, %hi(.LCPI3_1)
	addi r5, r5, %lo(.LCPI3_1)
	ldw r7, r5+4
	ldw r6, r5+0
.LBB3_15:
	fmul.d r12, r12, r6
	addi r3, r3, -1
	bgt r3, r1, .LBB3_15
.LBB3_16:
	add r3, r4, r0
.LBB3_17:
	addi r4, r0, -1
	bgt r3, r4, .LBB3_20
.LBB3_18:
	lui r6, %hi(.LCPI3_1)
	addi r6, r6, %lo(.LCPI3_1)
	ldw r5, r6+4
	ldw r4, r6+0
.LBB3_19:
	fdiv.d r12, r12, r4
	addi r6, r3, 1
	sltu r7, r6, r3
	add r3, r6, r0
	bne r7, r1, .LBB3_19
.LBB3_20:
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r6, r1+0
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	addi r11, sp, 20
	addi r4, r0, 16
	add r3, r11, r0
	jal r31, snprintf
	addi r3, sp, 36
	addi r4, r0, 64
	add r5, r11, r0
	jal r0, .LBB3_36
.LBB3_21:
	stw r16+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 70
	ble r1, r3, .LBB3_11
.LBB3_22:
	addi r3, r0, 71
	beq r1, r3, .LBB3_27
.LBB3_23:
	addi r3, r0, 73
	bne r1, r3, .LBB3_25
.LBB3_24:
	fcvt.w.d r6, r12
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r3, sp, 36
	addi r4, r0, 64
	jal r31, snprintf
	jal r0, .LBB3_50
.LBB3_25:
	addi r3, r0, -2
	and r3, r1, r3
	addi r4, r0, 68
	bne r3, r4, .LBB3_35
.LBB3_26:
	lui r3, %hi(fio_d)
	addi r3, r3, %lo(fio_d)
	ldw r4, r3+0
	addi r3, sp, 36
	add r5, r12, r0
	add r6, r13, r0
	add r7, r1, r0
	jal r31, fio_efmt
	jal r0, .LBB3_50
.LBB3_27:
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r1, r1+0
	addi r18, r0, 1
	sgt r3, r1, r18
	sub r3, r0, r3
	xori r4, r1, 1
	and r3, r4, r3
	xori r10, r3, 1
	lui r4, %hi(.LCPI3_0)
	addi r4, r4, %lo(.LCPI3_0)
	ldw r9, r4+4
	ldw r8, r4+0
	lui r4, %hi(.LCPI3_1)
	addi r4, r4, %lo(.LCPI3_1)
	ldw r7, r4+4
	ldw r6, r4+0
	add r4, r8, r0
	add r5, r9, r0
.LBB3_28:
	fmul.d r4, r4, r6
	addi r10, r10, -1
	bne r10, r14, .LBB3_28
.LBB3_29:
	lui r10, %hi(.LCPI3_2)
	addi r10, r10, %lo(.LCPI3_2)
	ldw r23, r10+4
	ldw r22, r10+0
	flt.d r10, r12, r22
	fneg.d r24, r12
	xor lr, r25, r13
	sub r10, r0, r10
	and lr, lr, r10
	xor r21, r13, lr
	xor lr, r24, r12
	and r10, lr, r10
	xor r20, r12, r10
	feq.d r10, r12, r22
	bne r10, r14, .LBB3_32
.LBB3_30:
	lui r10, %hi(.LCPI3_3)
	addi r10, r10, %lo(.LCPI3_3)
	ldw r23, r10+4
	ldw r22, r10+0
	fle.d r10, r22, r20
	xori r10, r10, 1
	bne r10, r14, .LBB3_33
.LBB3_31:
	flt.d r4, r20, r4
	xori r4, r4, 1
	bne r4, r14, .LBB3_33
.LBB3_32:
	addi r18, r0, 0
.LBB3_33:
	xori r4, r3, 1
	beq r18, r14, .LBB3_37
.LBB3_34:
	addi r3, sp, 36
	addi r7, r0, 69
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fio_efmt
	jal r0, .LBB3_49
.LBB3_35:
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	addi r3, sp, 36
	addi r4, r0, 64
.LBB3_36:
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	jal r0, .LBB3_50
.LBB3_37:
	lui r19, %hi(fio_w)
	addi r19, r19, %lo(fio_w)
	ldw r3, r19+0
	addi r5, r0, 3
	sgt r5, r3, r5
	addi r10, r3, -4
	xor r10, r10, r3
	sub r5, r0, r5
	and r5, r10, r5
	xor r11, r3, r5
	fle.d r3, r8, r20
	xori r5, r3, 1
	add r3, r14, r0
	bne r5, r14, .LBB3_44
.LBB3_38:
	fle.d r3, r6, r20
	xori r5, r3, 1
	addi r3, r0, 0
	beq r5, r3, .LBB3_40
.LBB3_39:
	addi r3, r0, -1
	jal r0, .LBB3_44
.LBB3_40:
	addi r5, r0, 1
.LBB3_41:
	add r8, r5, r0
	fdiv.d r20, r20, r6
	fle.d r5, r6, r20
	xori r9, r5, 1
	addi r5, r8, 1
	bne r9, r3, .LBB3_43
.LBB3_42:
	bgt r1, r8, .LBB3_41
.LBB3_43:
	sub r3, r0, r5
.LBB3_44:
	add r1, r3, r4
	sgt r3, r1, r14
	sub r3, r0, r3
	and r6, r1, r3
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	addi r14, sp, 20
	addi r4, r0, 16
	add r3, r14, r0
	jal r31, snprintf
	addi r15, sp, 36
	addi r4, r0, 64
	add r3, r15, r0
	add r5, r14, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	add r3, r15, r0
	add r4, r11, r0
	jal r31, fio_field
	ldw r1, r19+0
	addi r3, r0, 4
	bge r1, r3, .LBB3_55
.LBB3_45:
	ldw r1, r17+0
	addi r3, r0, 1
	blt r1, r3, .LBB3_47
.LBB3_46:
	addi r1, r1, -1
	stw r17+0, r1
.LBB3_47:
	ldw r3, r17+0
	addi r1, r0, 0
	bne r3, r1, .LBB3_49
.LBB3_48:
	stw r16+0, r1
.LBB3_49:
	addi r1, r0, 0
	beq r18, r1, .LBB3_54
.LBB3_50:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	addi r3, sp, 36
	jal r31, fio_field
	ldw r1, r17+0
	addi r3, r0, 1
	blt r1, r3, .LBB3_52
.LBB3_51:
	addi r1, r1, -1
	stw r17+0, r1
.LBB3_52:
	ldw r3, r17+0
	addi r1, r0, 0
	bne r3, r1, .LBB3_54
.LBB3_53:
	stw r16+0, r1
.LBB3_54:
	ldw r25, sp+100
	ldw r24, sp+104
	ldw r23, sp+108
	ldw r22, sp+112
	ldw r21, sp+116
	ldw r20, sp+120
	ldw r19, sp+124
	ldw r18, sp+128
	ldw r17, sp+132
	ldw r16, sp+136
	ldw r15, sp+140
	ldw r14, sp+144
	ldw r13, sp+148
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jalr r0, r31, 0
.LBB3_55:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r5, r1+0
	addi r3, r0, 5
	addi r4, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	addi r8, r0, 1
	jal r0, .LBB3_57
.LBB3_56:
	addi r3, r3, -1
	bleu r3, r8, .LBB3_45
.LBB3_57:
	bgt r5, r4, .LBB3_56
.LBB3_58:
	addi r9, r5, 1
	stw r1+0, r9
	add r5, r5, r6
	stb r5+0, r7
	add r5, r9, r0
	jal r0, .LBB3_56
.Lfunc_end3:
	.size	f77_wr_d, .Lfunc_end3-f77_wr_d
                                        # -- End function
	.p2align	2                               # -- Begin function fio_efmt
	.type	fio_efmt,@function
fio_efmt:                               # @fio_efmt
# %bb.0:
	addi sp, sp, -144
	stw sp+0, lr
	stw sp+140, r11
	stw sp+136, r12
	stw sp+132, r13
	stw sp+128, r14
	stw sp+124, r15
	stw sp+120, r16
	stw sp+116, r17
	stw sp+112, r18
	stw sp+108, r19
	stw sp+104, r20
	add r11, r7, r0
	add r13, r6, r0
	add r14, r5, r0
	add r12, r3, r0
	addi r18, r0, 1
	slt r1, r4, r18
	sub r1, r0, r1
	xori r3, r4, 6
	and r1, r3, r1
	xor r1, r4, r1
	lui r3, %hi(fio_scale)
	addi r3, r3, %lo(fio_scale)
	ldw r3, r3+0
	sub r4, r0, r1
	sgt r4, r3, r4
	addi r5, r1, 2
	slt r5, r3, r5
	sub r5, r0, r5
	and r3, r3, r5
	sub r4, r0, r4
	and r17, r3, r4
	addi r20, r0, 0
	sgt r3, r17, r20
	add r4, r17, r1
	xori r5, r4, 1
	sgt r4, r4, r18
	sub r4, r0, r4
	and r4, r5, r4
	xori r4, r4, 1
	addi r4, r4, -1
	xor r1, r1, r4
	sub r3, r0, r3
	and r1, r1, r3
	xor r6, r4, r1
	lui r5, %hi(.L.str.27)
	addi r5, r5, %lo(.L.str.27)
	addi r15, sp, 24
	addi r4, r0, 16
	add r3, r15, r0
	jal r31, snprintf
	addi r16, sp, 40
	addi r4, r0, 64
	add r3, r16, r0
	add r5, r15, r0
	add r6, r14, r0
	add r7, r13, r0
	jal r31, snprintf
	ldbu r19, sp+40
	addi r13, r0, 45
	seq r1, r19, r13
	addi r14, r0, 43
	seq r3, r19, r14
	or  r1, r1, r3
	or  r1, r1, r16
	addi r3, r1, 1
	addi r1, r0, 68
	addi r16, r0, 46
	addi r4, r0, 69
	addi r5, r0, 101
	add r15, r20, r0
	jal r0, .LBB4_3
.LBB4_1:
	addi r7, sp, 40
	add r7, r7, r15
	stb r7+40, r6
	addi r15, r15, 1
.LBB4_2:
	addi r3, r3, 1
.LBB4_3:
	ldbu r6, r3+-1
	bgt r6, r1, .LBB4_6
.LBB4_4:
	beq r6, r16, .LBB4_2
.LBB4_5:
	bne r6, r20, .LBB4_1
	jal r0, .LBB4_9
.LBB4_6:
	beq r6, r4, .LBB4_8
.LBB4_7:
	bne r6, r5, .LBB4_1
.LBB4_8:
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	addi r1, r1, 1
	addi r4, r0, 0
	bne r15, r4, .LBB4_10
	jal r0, .LBB4_12
.LBB4_9:
	add r1, r18, r0
	addi r4, r0, 0
	beq r15, r4, .LBB4_12
.LBB4_10:
	bne r15, r18, .LBB4_13
.LBB4_11:
	ldbu r3, sp+80
	andi r3, r3, 255
	addi r5, r0, 48
	bne r3, r5, .LBB4_13
.LBB4_12:
	addi r1, r0, 0
.LBB4_13:
	add r5, r12, r0
	bne r19, r13, .LBB4_15
.LBB4_14:
	addi r5, r12, 1
	stb r12+0, r13
.LBB4_15:
	blt r17, r18, .LBB4_20
.LBB4_16:
	blt r15, r18, .LBB4_25
.LBB4_17:
	xor r3, r17, r15
	slt r4, r17, r15
	sub r4, r0, r4
	and r3, r3, r4
	xor r3, r15, r3
	addi r4, sp, 40
	addi r6, r4, 40
	addi r4, r0, 0
.LBB4_18:
	add r7, r5, r4
	add r8, r6, r4
	ldbu r8, r8+0
	stb r7+0, r8
	addi r4, r4, 1
	bne r3, r4, .LBB4_18
.LBB4_19:
	add r5, r5, r4
	jal r0, .LBB4_26
.LBB4_20:
	addi r7, r0, 48
	stb r5+0, r7
	addi r8, r5, 2
	stb r5+1, r16
	addi r6, r0, 55
	beq r17, r4, .LBB4_31
.LBB4_21:
	sub r3, r8, r12
	bgt r3, r6, .LBB4_31
.LBB4_22:
	sub r3, r5, r12
	addi lr, r3, 3
	addi r5, r0, -2
	addi r9, r0, 56
	add r10, r17, r0
.LBB4_23:
	addi r3, r8, 1
	stb r8+0, r7
	bgt r10, r5, .LBB4_32
.LBB4_24:
	add r16, lr, r0
	addi r10, r10, 1
	addi lr, lr, 1
	add r8, r3, r0
	blt r16, r9, .LBB4_23
	jal r0, .LBB4_32
.LBB4_25:
	addi r4, r0, 0
.LBB4_26:
	stb r5+0, r16
	addi r3, r5, 1
	bge r4, r15, .LBB4_38
.LBB4_27:
	sub r6, r3, r12
	addi r7, r0, 55
	bgt r6, r7, .LBB4_38
.LBB4_28:
	sub r5, r5, r12
	addi r7, r5, 2
	addi r5, sp, 40
	addi r5, r5, 40
	addi r6, r0, 56
.LBB4_29:
	add r8, r7, r0
	add r7, r5, r4
	ldbu r7, r7+0
	stb r3+0, r7
	addi r4, r4, 1
	addi r3, r3, 1
	bge r4, r15, .LBB4_38
.LBB4_30:
	addi r7, r8, 1
	blt r8, r6, .LBB4_29
	jal r0, .LBB4_38
.LBB4_31:
	add r3, r8, r0
.LBB4_32:
	blt r15, r18, .LBB4_38
.LBB4_33:
	sub r7, r3, r12
	bgt r7, r6, .LBB4_38
.LBB4_34:
	addi r5, sp, 40
	addi r5, r5, 40
	addi r6, r7, 1
	addi r7, r0, 56
.LBB4_35:
	add r8, r3, r4
	add r9, r5, r4
	ldbu r9, r9+0
	stb r8+0, r9
	addi r8, r4, 1
	bge r8, r15, .LBB4_37
.LBB4_36:
	add r9, r6, r4
	add r4, r8, r0
	blt r9, r7, .LBB4_35
.LBB4_37:
	add r3, r3, r8
.LBB4_38:
	sub r1, r1, r17
	stb r3+0, r11
	addi r4, r0, -1
	ble r1, r4, .LBB4_41
.LBB4_39:
	stb r3+1, r14
	addi r4, r0, 100
	blt r1, r4, .LBB4_42
.LBB4_40:
	lui r4, 335544
	addi r4, r4, 1311
	mulhu r4, r1, r4
	srli r4, r4, 5
	addi r4, r4, 48
	stb r3+2, r4
	lui r4, 838861
	addi r4, r4, -819
	mulhu r4, r1, r4
	srli r4, r4, 3
	lui r5, 104858
	addi r5, r5, -1638
	mulhu r5, r4, r5
	addi r6, r0, 10
	mul r5, r5, r6
	sub r5, r4, r5
	ori  r5, r5, 48
	stb r3+3, r5
	mul r4, r4, r6
	sub r1, r1, r4
	ori  r1, r1, 48
	addi r4, r3, 5
	stb r3+4, r1
	jal r0, .LBB4_43
.LBB4_41:
	stb r3+1, r13
	sub r1, r0, r1
	addi r4, r0, 100
	bge r1, r4, .LBB4_40
.LBB4_42:
	addi r4, r3, 4
	lui r5, 838861
	addi r5, r5, -819
	mulhu r5, r1, r5
	srli r5, r5, 3
	lui r6, 104858
	addi r6, r6, -1638
	mulhu r6, r5, r6
	addi r7, r0, 10
	mul r6, r6, r7
	sub r6, r5, r6
	ori  r6, r6, 48
	stb r3+2, r6
	mul r5, r5, r7
	sub r1, r1, r5
	ori  r1, r1, 48
	stb r3+3, r1
.LBB4_43:
	addi r1, r0, 0
	stb r4+0, r1
	ldw r20, sp+104
	ldw r19, sp+108
	ldw r18, sp+112
	ldw r17, sp+116
	ldw r16, sp+120
	ldw r15, sp+124
	ldw r14, sp+128
	ldw r13, sp+132
	ldw r12, sp+136
	ldw r11, sp+140
	ldw lr, sp+0
	addi sp, sp, 144
	jalr r0, r31, 0
.Lfunc_end4:
	.size	fio_efmt, .Lfunc_end4-fio_efmt
                                        # -- End function
	.globl	f77_wr_r                        # -- Begin function f77_wr_r
	.p2align	2
	.type	f77_wr_r,@function
f77_wr_r:                               # @f77_wr_r
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	fcvt.d.s r4, r3
	# ADJCALLSTACKDOWN 0, 0
	add r3, r4, r0
	add r4, r5, r0
	jal r31, f77_wr_d
	# ADJCALLSTACKUP 0, 0
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end5:
	.size	f77_wr_r, .Lfunc_end5-f77_wr_r
                                        # -- End function
	.globl	f77_wr_a                        # -- Begin function f77_wr_a
	.p2align	2
	.type	f77_wr_a,@function
f77_wr_a:                               # @f77_wr_a
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r11, r0, 0
	beq r1, r11, .LBB6_8
.LBB6_1:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r7, r1+0
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	bgt r7, r5, .LBB6_3
.LBB6_2:
	addi r8, r7, 1
	stw r1+0, r8
	add r7, r7, r6
	addi r8, r0, 32
	stb r7+0, r8
.LBB6_3:
	addi r7, r0, 1
	blt r4, r7, .LBB6_29
.LBB6_4:
	ldw r7, r1+0
	jal r0, .LBB6_6
.LBB6_5:
	addi r4, r4, -1
	addi r3, r3, 1
	beq r4, r11, .LBB6_29
.LBB6_6:
	bgt r7, r5, .LBB6_5
.LBB6_7:
	ldbu r8, r3+0
	addi r9, r7, 1
	stw r1+0, r9
	add r7, r7, r6
	stb r7+0, r8
	add r7, r9, r0
	jal r0, .LBB6_5
.LBB6_8:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	beq r1, r11, .LBB6_10
.LBB6_9:
	ldw r1, r13+0
	bgt r1, r11, .LBB6_11
.LBB6_10:
	stw r12+0, r11
	add r14, r3, r0
	add r15, r4, r0
	jal r31, fio_next_desc
	add r4, r15, r0
	add r3, r14, r0
	beq r1, r11, .LBB6_29
.LBB6_11:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r5, r1+0
	addi r1, r0, 1
	blt r5, r1, .LBB6_13
.LBB6_12:
	blt r5, r4, .LBB6_20
.LBB6_13:
	ble r5, r4, .LBB6_19
.LBB6_14:
	sub r6, r5, r4
	blt r6, r1, .LBB6_19
.LBB6_15:
	lui r5, %hi(fio_len)
	addi r5, r5, %lo(fio_len)
	ldw r8, r5+0
	addi r6, r6, 1
	addi r7, r0, 1022
	lui r9, %hi(fio_line)
	addi r9, r9, %lo(fio_line)
	addi r10, r0, 32
	jal r0, .LBB6_17
.LBB6_16:
	addi r6, r6, -1
	ble r6, r1, .LBB6_19
.LBB6_17:
	bgt r8, r7, .LBB6_16
.LBB6_18:
	addi lr, r8, 1
	stw r5+0, lr
	add r8, r8, r9
	stb r8+0, r10
	add r8, lr, r0
	jal r0, .LBB6_16
.LBB6_19:
	add r5, r4, r0
.LBB6_20:
	blt r5, r1, .LBB6_25
.LBB6_21:
	lui r4, %hi(fio_len)
	addi r4, r4, %lo(fio_len)
	ldw r8, r4+0
	addi r6, r0, 1022
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	jal r0, .LBB6_23
.LBB6_22:
	addi r5, r5, -1
	addi r3, r3, 1
	beq r5, r11, .LBB6_25
.LBB6_23:
	bgt r8, r6, .LBB6_22
.LBB6_24:
	ldbu r9, r3+0
	addi r10, r8, 1
	stw r4+0, r10
	add r8, r8, r7
	stb r8+0, r9
	add r8, r10, r0
	jal r0, .LBB6_22
.LBB6_25:
	ldw r3, r13+0
	blt r3, r1, .LBB6_27
.LBB6_26:
	addi r1, r3, -1
	stw r13+0, r1
.LBB6_27:
	ldw r1, r13+0
	bne r1, r11, .LBB6_29
.LBB6_28:
	stw r12+0, r11
.LBB6_29:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end6:
	.size	f77_wr_a, .Lfunc_end6-f77_wr_a
                                        # -- End function
	.globl	f77_wr_l                        # -- Begin function f77_wr_l
	.p2align	2
	.type	f77_wr_l,@function
f77_wr_l:                               # @f77_wr_l
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r11, r3, r0
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r14, %hi(fio_rep)
	addi r14, r14, %lo(fio_rep)
	addi r13, r0, 0
	beq r1, r13, .LBB7_2
.LBB7_1:
	ldw r1, r14+0
	bgt r1, r13, .LBB7_3
.LBB7_2:
	stw r12+0, r13
	jal r31, fio_next_desc
	beq r1, r13, .LBB7_14
.LBB7_3:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r6, r1+0
	addi r5, r0, 2
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	addi r4, r0, 1022
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	blt r6, r5, .LBB7_8
.LBB7_4:
	ldw r8, r1+0
	addi r6, r6, 1
	addi r7, r0, 32
	jal r0, .LBB7_6
.LBB7_5:
	addi r6, r6, -1
	ble r6, r5, .LBB7_8
.LBB7_6:
	bgt r8, r4, .LBB7_5
.LBB7_7:
	addi r9, r8, 1
	stw r1+0, r9
	add r8, r8, r3
	stb r8+0, r7
	add r8, r9, r0
	jal r0, .LBB7_5
.LBB7_8:
	ldw r5, r1+0
	bgt r5, r4, .LBB7_10
.LBB7_9:
	seq r4, r11, r13
	sub r4, r0, r4
	andi r4, r4, 18
	xori r4, r4, 84
	addi r6, r5, 1
	stw r1+0, r6
	add r1, r5, r3
	stb r1+0, r4
.LBB7_10:
	ldw r1, r14+0
	addi r3, r0, 1
	blt r1, r3, .LBB7_12
.LBB7_11:
	addi r1, r1, -1
	stw r14+0, r1
.LBB7_12:
	ldw r1, r14+0
	bne r1, r13, .LBB7_14
.LBB7_13:
	stw r12+0, r13
.LBB7_14:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end7:
	.size	f77_wr_l, .Lfunc_end7-f77_wr_l
                                        # -- End function
	.globl	f77_wr_end                      # -- Begin function f77_wr_end
	.p2align	2
	.type	f77_wr_end,@function
f77_wr_end:                             # @f77_wr_end
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r12, r0, 0
	bne r1, r12, .LBB8_2
.LBB8_1:
	lui r11, %hi(fio_revert)
	addi r11, r11, %lo(fio_revert)
	ldw r13, r11+0
	addi r1, r0, -1
	stw r11+0, r1
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	stw r1+0, r12
	lui r1, %hi(fio_rep)
	addi r1, r1, %lo(fio_rep)
	stw r1+0, r12
	jal r31, fio_next_desc
	stw r11+0, r13
.LBB8_2:
	lui r1, %hi(fio_out)
	addi r1, r1, %lo(fio_out)
	ldw r1, r1+0
	seq r3, r1, r12
	lui r4, %hi(stdout)
	addi r4, r4, %lo(stdout)
	ldw r4, r4+0
	xor r4, r4, r1
	sub r3, r0, r3
	and r3, r4, r3
	xor r11, r1, r3
	lui r13, %hi(fio_len)
	addi r13, r13, %lo(fio_len)
	ldw r1, r13+0
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	add r1, r1, r3
	stb r1+0, r12
	add r4, r11, r0
	jal r31, fputs
	addi r3, r0, 10
	add r4, r11, r0
	jal r31, fputc
	stw r13+0, r12
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end8:
	.size	f77_wr_end, .Lfunc_end8-f77_wr_end
                                        # -- End function
	.p2align	2                               # -- Begin function fio_next_desc
	.type	fio_next_desc,@function
fio_next_desc:                          # @fio_next_desc
# %bb.0:
	addi sp, sp, -128
	stw sp+0, lr
	stw sp+4, fp
	stw sp+124, r11
	stw sp+120, r12
	stw sp+116, r13
	stw sp+112, r14
	stw sp+108, r15
	stw sp+104, r16
	stw sp+100, r17
	stw sp+96, r18
	stw sp+92, r19
	stw sp+88, r20
	stw sp+84, r21
	stw sp+80, r22
	stw sp+76, r23
	stw sp+72, r24
	stw sp+68, r25
	stw sp+64, r26
	stw sp+60, r27
	stw sp+56, r28
	addi r20, r0, 1
	lui r16, %hi(fio_fmt)
	addi r16, r16, %lo(fio_fmt)
	lui r13, %hi(fio_pos)
	addi r13, r13, %lo(fio_pos)
	addi r17, r0, 47
	addi r14, r0, -10
	addi r18, r0, 40
	lui r19, %hi(fio_gdepth)
	addi r19, r19, %lo(fio_gdepth)
	addi r25, r0, 15
	lui r26, %hi(fio_gstart)
	addi r26, r26, %lo(fio_gstart)
	lui r22, %hi(fio_gcount)
	addi r22, r22, %lo(fio_gcount)
	addi r23, r0, 0
	lui r24, %hi(fio_revert)
	addi r24, r24, %lo(fio_revert)
	addi r21, r0, -33
	addi r27, r0, 88
	lui r12, %hi(fio_reading)
	addi r12, r12, %lo(fio_reading)
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	stw sp+28, r1
	lui fp, %hi(fio_len)
	addi fp, fp, %lo(fio_len)
	addi r28, r0, 1022
	lui r11, %hi(fio_line)
	addi r11, r11, %lo(fio_line)
	addi r1, r0, 32
	stw sp+52, r1
	addi r1, r0, 80
	stw sp+44, r1
	lui r1, %hi(fio_scale)
	addi r1, r1, %lo(fio_scale)
	stw sp+32, r1
	addi r1, r0, 72
	stw sp+40, r1
	add r15, r20, r0
	stw sp+48, r12
	stw sp+36, r17
	jal r0, .LBB9_3
.LBB9_1:
	addi r1, r5, 1
	stw r13+0, r1
.LBB9_2:
	addi r15, r15, 1
	lui r1, 24
	addi r1, r1, 1697
	beq r15, r1, .LBB9_60
.LBB9_3:
	ldw r3, r16+0
	ldw r5, r13+0
	add r4, r3, r5
	ldbu r6, r4+0
	slli r1, r6, 24
	srai r1, r1, 24
	bgtu r6, r17, .LBB9_13
.LBB9_4:
	slli r6, r6, 2
	lui r7, %hi(.LJTI9_0)
	addi r7, r7, %lo(.LJTI9_0)
	add r6, r7, r6
	ldw r7, r6+0
	add r6, r20, r0
	jalr r0, r7, 0
.LBB9_5:
	ldw r1, r19+0
	addi r4, r0, 1
	blt r1, r4, .LBB9_7
.LBB9_6:
	addi r1, r0, 0
	stw r19+0, r1
.LBB9_7:
	ldw r5, r24+0
	addi r1, r0, 0
	blt r5, r1, .LBB9_79
.LBB9_8:
	stw r13+0, r5
	add r1, r3, r5
	ldbu r3, r1+0
	addi r1, r0, 0
	beq r3, r1, .LBB9_79
.LBB9_9:
	ldbu r1, r12+0
	beq r1, r4, .LBB9_15
.LBB9_10:
	lui r1, %hi(fio_out)
	addi r1, r1, %lo(fio_out)
	ldw r1, r1+0
	add r17, r16, r0
	add r16, r19, r0
	add r19, r27, r0
	add r27, r23, r0
	add r23, r14, r0
	add r14, r18, r0
	add r18, r21, r0
	add r21, r24, r0
	add r24, r22, r0
	add r22, r26, r0
	add r26, r25, r0
	addi r25, r0, 0
	seq r3, r1, r25
	lui r4, %hi(stdout)
	addi r4, r4, %lo(stdout)
	ldw r4, r4+0
	xor r4, r4, r1
	sub r3, r0, r3
	and r3, r4, r3
	xor r12, r1, r3
	ldw r1, fp+0
	add r1, r1, r11
	stb r1+0, r25
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fputs
	addi r3, r0, 10
	add r4, r12, r0
	ldw r12, sp+48
	jal r31, fputc
	stw fp+0, r25
	add r25, r26, r0
	add r26, r22, r0
	add r22, r24, r0
	add r24, r21, r0
	add r21, r18, r0
	add r18, r14, r0
	add r14, r23, r0
	add r23, r27, r0
	add r27, r19, r0
	add r19, r16, r0
	add r16, r17, r0
	ldw r17, sp+36
	jal r0, .LBB9_2
.LBB9_11:
	ldb r1, r4+1
	addi r1, r1, -58
	bgeu r1, r14, .LBB9_27
.LBB9_12:
	addi r1, r0, 45
.LBB9_13:
	add r6, r20, r0
	jal r0, .LBB9_28
.LBB9_14:
	addi r1, r5, 1
	stw r13+0, r1
	ldbu r1, r12+0
	addi r3, r0, 1
	bne r1, r3, .LBB9_47
.LBB9_15:
	jal r31, fio_next_record
	jal r0, .LBB9_2
.LBB9_16:
	addi r1, r5, 1
	stw r13+0, r1
	ldw r5, fp+0
	ldbu r4, r12+0
	jal r0, .LBB9_19
.LBB9_17:
	addi r8, r5, 1
	stw fp+0, r8
	add r5, r5, r11
	stb r5+0, r7
	add r5, r8, r0
.LBB9_18:
	add r1, r1, r6
	stw r13+0, r1
.LBB9_19:
	add r6, r3, r1
	ldbu r8, r6+0
	addi r7, r0, 39
	beq r8, r7, .LBB9_22
.LBB9_20:
	addi r9, r0, 0
	beq r8, r9, .LBB9_2
.LBB9_21:
	sgt r6, r5, r28
	or  r6, r4, r6
	andi r10, r6, 1
	addi r6, r0, 1
	add r7, r8, r0
	beq r10, r9, .LBB9_17
	jal r0, .LBB9_18
.LBB9_22:
	addi r6, r1, 1
	add r8, r3, r6
	ldbu r8, r8+0
	bne r8, r7, .LBB9_48
.LBB9_23:
	sgt r6, r5, r28
	or  r6, r4, r6
	andi r8, r6, 1
	addi r6, r0, 2
	addi r9, r0, 0
	beq r8, r9, .LBB9_17
	jal r0, .LBB9_18
.LBB9_24:
	addi r1, r5, 1
	stw r13+0, r1
	ldw r1, r19+0
	addi r3, r0, 1
	blt r1, r3, .LBB9_2
.LBB9_25:
	addi r3, r1, -1
	stw r19+0, r3
	slli r3, r3, 2
	add r4, r3, r22
	ldw r5, r4+0
	addi r6, r5, -1
	stw r4+0, r6
	addi r4, r0, 2
	blt r5, r4, .LBB9_2
.LBB9_26:
	add r3, r3, r26
	ldw r3, r3+0
	stw r13+0, r3
	stw r19+0, r1
	jal r0, .LBB9_2
.LBB9_27:
	addi r1, r5, 1
	stw r13+0, r1
	add r1, r3, r1
	ldb r1, r1+0
	add r6, r23, r0
.LBB9_28:
	addi r4, r1, -58
	add r1, r20, r0
	bltu r4, r14, .LBB9_34
.LBB9_29:
	ldw r4, r13+0
	add r1, r3, r4
	ldb r1, r1+0
	addi r7, r1, -48
	addi r1, r0, 9
	bleu r7, r1, .LBB9_31
.LBB9_30:
	addi r1, r0, 0
	jal r0, .LBB9_34
.LBB9_31:
	addi r8, r3, 1
	addi r1, r0, 0
.LBB9_32:
	addi r10, r0, 10
	mul r1, r1, r10
	addi r9, r4, 1
	add r1, r1, r7
	add r4, r8, r4
	ldb r4, r4+0
	addi r7, r4, -48
	add r4, r9, r0
	bltu r7, r10, .LBB9_32
.LBB9_33:
	stw r13+0, r9
.LBB9_34:
	ldw r7, r13+0
	add r4, r3, r7
	ldb r4, r4+0
	andi r8, r4, 255
	bne r8, r18, .LBB9_38
.LBB9_35:
	addi r4, r7, 1
	stw r13+0, r4
	ldw r3, r19+0
	bgt r3, r25, .LBB9_2
.LBB9_36:
	slli r6, r3, 2
	add r7, r6, r26
	stw r7+0, r4
	add r4, r6, r22
	stw r4+0, r1
	addi r1, r3, 1
	stw r19+0, r1
	bne r3, r23, .LBB9_2
.LBB9_37:
	stw r24+0, r5
	jal r0, .LBB9_2
.LBB9_38:
	and r5, r4, r21
	beq r5, r27, .LBB9_50
.LBB9_39:
	andi r9, r4, 223
	ldw r5, sp+44
	beq r9, r5, .LBB9_49
.LBB9_40:
	addi r5, r7, 1
	ldw r6, sp+40
	bne r9, r6, .LBB9_52
.LBB9_41:
	blt r1, r20, .LBB9_54
.LBB9_42:
	ldw r6, fp+0
	ldbu r4, r12+0
	addi r1, r1, 1
	jal r0, .LBB9_44
.LBB9_43:
	addi r5, r5, 1
	addi r1, r1, -1
	addi r7, r0, 1
	ble r1, r7, .LBB9_54
.LBB9_44:
	add r7, r3, r5
	ldbu r7, r7+0
	addi r8, r0, 0
	beq r7, r8, .LBB9_54
.LBB9_45:
	sgt r9, r6, r28
	or  r9, r4, r9
	andi r9, r9, 1
	bne r9, r8, .LBB9_43
.LBB9_46:
	addi r8, r6, 1
	stw fp+0, r8
	add r6, r6, r11
	stb r6+0, r7
	add r6, r8, r0
	jal r0, .LBB9_43
.LBB9_47:
	lui r1, %hi(fio_out)
	addi r1, r1, %lo(fio_out)
	ldw r1, r1+0
	seq r3, r1, r23
	lui r4, %hi(stdout)
	addi r4, r4, %lo(stdout)
	ldw r4, r4+0
	xor r4, r4, r1
	sub r3, r0, r3
	and r3, r4, r3
	xor r12, r1, r3
	ldw r1, fp+0
	add r1, r1, r11
	stb r1+0, r23
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fputs
	addi r3, r0, 10
	add r4, r12, r0
	ldw r12, sp+48
	jal r31, fputc
	stw fp+0, r23
	jal r0, .LBB9_2
.LBB9_48:
	stw r13+0, r6
	jal r0, .LBB9_2
.LBB9_49:
	addi r3, r7, 1
	stw r13+0, r3
	sub r3, r0, r1
	xor r1, r1, r3
	sub r4, r0, r6
	and r1, r1, r4
	xor r1, r3, r1
	ldw r3, sp+32
	stw r3+0, r1
	jal r0, .LBB9_2
.LBB9_50:
	addi r3, r7, 1
	stw r13+0, r3
	ldbu r3, r12+0
	bne r3, r20, .LBB9_55
.LBB9_51:
	ldw r4, sp+28
	ldw r3, r4+0
	add r1, r3, r1
	stw r4+0, r1
	jal r0, .LBB9_2
.LBB9_52:
	addi r6, r8, -65
	addi r8, r0, 43
	bgtu r6, r8, .LBB9_54
.LBB9_53:
	slli r6, r6, 2
	lui r8, %hi(.LJTI9_1)
	addi r8, r8, %lo(.LJTI9_1)
	add r6, r8, r6
	ldw r6, r6+0
	jalr r0, r6, 0
.LBB9_54:
	stw r13+0, r5
	jal r0, .LBB9_2
.LBB9_55:
	blt r1, r20, .LBB9_2
.LBB9_56:
	ldw r3, fp+0
	addi r1, r1, 1
	jal r0, .LBB9_58
.LBB9_57:
	addi r1, r1, -1
	ble r1, r20, .LBB9_2
.LBB9_58:
	bgt r3, r28, .LBB9_57
.LBB9_59:
	addi r4, r3, 1
	stw fp+0, r4
	add r3, r3, r11
	ldw r5, sp+52
	stb r3+0, r5
	add r3, r4, r0
	jal r0, .LBB9_57
.LBB9_60:
	addi r1, r0, 0
	jal r0, .LBB9_79
.LBB9_61:
	stw r13+0, r5
	add r5, r3, r5
	ldb r5, r5+0
	addi r9, r5, -48
	addi r8, r0, 0
	addi r5, r0, 9
	add r6, r8, r0
	bgtu r9, r5, .LBB9_65
.LBB9_62:
	addi r10, r3, 2
	addi r6, r0, 0
	addi lr, r0, 10
.LBB9_63:
	mul r6, r6, lr
	add r6, r6, r9
	add r9, r10, r7
	ldb r9, r9+0
	addi r9, r9, -48
	addi r7, r7, 1
	bltu r9, lr, .LBB9_63
.LBB9_64:
	addi r7, r7, 1
	stw r13+0, r7
.LBB9_65:
	lui r7, %hi(fio_w)
	addi r7, r7, %lo(fio_w)
	stw r7+0, r6
	lui r6, %hi(fio_d)
	addi r6, r6, %lo(fio_d)
	stw r6+0, r8
	ldw r7, r13+0
	add r8, r3, r7
	ldbu r8, r8+0
	addi r9, r0, 46
	bne r8, r9, .LBB9_72
.LBB9_66:
	addi r8, r7, 1
	stw r13+0, r8
	add r8, r3, r8
	ldb r8, r8+0
	addi r9, r8, -48
	bleu r9, r5, .LBB9_68
.LBB9_67:
	addi r5, r0, 0
	jal r0, .LBB9_71
.LBB9_68:
	addi r8, r3, 2
	addi r5, r0, 0
	addi r10, r0, 10
.LBB9_69:
	mul r5, r5, r10
	add r5, r5, r9
	add r9, r8, r7
	ldb r9, r9+0
	addi r9, r9, -48
	addi r7, r7, 1
	bltu r9, r10, .LBB9_69
.LBB9_70:
	addi r7, r7, 1
	stw r13+0, r7
.LBB9_71:
	stw r6+0, r5
.LBB9_72:
	ldw r5, r13+0
	add r6, r3, r5
	ldbu r6, r6+0
	ori  r6, r6, 32
	addi r7, r0, 101
	bne r6, r7, .LBB9_78
.LBB9_73:
	addi r6, r5, 1
	stw r13+0, r6
	add r6, r3, r6
	ldb r6, r6+0
	addi r6, r6, -58
	bltu r6, r14, .LBB9_77
.LBB9_74:
	addi r3, r3, 2
	addi r6, r0, -11
.LBB9_75:
	add r7, r3, r5
	ldb r7, r7+0
	addi r7, r7, -58
	addi r5, r5, 1
	bgtu r7, r6, .LBB9_75
.LBB9_76:
	addi r5, r5, 1
.LBB9_77:
	stw r13+0, r5
.LBB9_78:
	lui r3, %hi(fio_rep)
	addi r3, r3, %lo(fio_rep)
	stw r3+0, r1
	addi r1, r4, -97
	andi r1, r1, 255
	addi r3, r0, 26
	sltu r1, r1, r3
	addi r3, r4, -32
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r1, r4, r1
	lui r3, %hi(fio_desc)
	addi r3, r3, %lo(fio_desc)
	stw r3+0, r1
.LBB9_79:
	ldw r28, sp+56
	ldw r27, sp+60
	ldw r26, sp+64
	ldw r25, sp+68
	ldw r24, sp+72
	ldw r23, sp+76
	ldw r22, sp+80
	ldw r21, sp+84
	ldw r20, sp+88
	ldw r19, sp+92
	ldw r18, sp+96
	ldw r17, sp+100
	ldw r16, sp+104
	ldw r15, sp+108
	ldw r14, sp+112
	ldw r13, sp+116
	ldw r12, sp+120
	ldw r11, sp+124
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 128
	jalr r0, r31, 0
.Lfunc_end9:
	.size	fio_next_desc, .Lfunc_end9-fio_next_desc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI9_0,@object
.LJTI9_0:
	.word	.LBB9_5
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_1
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_16
	.word	.LBB9_28
	.word	.LBB9_24
	.word	.LBB9_28
	.word	.LBB9_28
	.word	.LBB9_1
	.word	.LBB9_11
	.word	.LBB9_28
	.word	.LBB9_14
	.size	.LJTI9_0, 192
	.type	.LJTI9_1,@object
.LJTI9_1:
	.word	.LBB9_61
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_61
	.word	.LBB9_61
	.word	.LBB9_61
	.word	.LBB9_61
	.word	.LBB9_54
	.word	.LBB9_61
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_61
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_61
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_61
	.word	.LBB9_61
	.word	.LBB9_61
	.word	.LBB9_61
	.word	.LBB9_54
	.word	.LBB9_61
	.word	.LBB9_54
	.word	.LBB9_54
	.word	.LBB9_61
	.size	.LJTI9_1, 176
                                        # -- End function
	.text
	.globl	f77_rd_begin                    # -- Begin function f77_rd_begin
	.p2align	2
	.type	f77_rd_begin,@function
f77_rd_begin:                           # @f77_rd_begin
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	add r11, r5, r0
	add r5, r3, r0
	addi r1, r0, 31
	lui r14, %hi(stdin)
	addi r14, r14, %lo(stdin)
	bgtu r3, r1, .LBB10_2
.LBB10_1:
	slli r1, r5, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r1, r1, r3
	ldw r15, r1+0
	addi r1, r0, 0
	bne r15, r1, .LBB10_6
.LBB10_2:
	addi r1, r0, 5
	beq r5, r1, .LBB10_4
.LBB10_3:
	addi r15, r0, 0
	bne r5, r15, .LBB10_5
.LBB10_4:
	ldw r15, r14+0
	jal r0, .LBB10_6
.LBB10_5:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r1, %hi(.L.str.24)
	addi r1, r1, %lo(.L.str.24)
	lui r6, %hi(.L.str.26)
	addi r6, r6, %lo(.L.str.26)
	add r12, r4, r0
	add r4, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r4, r12, r0
.LBB10_6:
	lui r1, %hi(fio_in)
	addi r1, r1, %lo(fio_in)
	stw r1+0, r15
	lui r16, %hi(fio_reading)
	addi r16, r16, %lo(fio_reading)
	addi r13, r0, 1
	stb r16+0, r13
	lui r1, %hi(fio_ldone)
	addi r1, r1, %lo(fio_ldone)
	addi r12, r0, 0
	stb r1+0, r12
	lui r1, %hi(fio_l_pending)
	addi r1, r1, %lo(fio_l_pending)
	stw r1+0, r12
	lui r1, %hi(fio_gdepth)
	addi r1, r1, %lo(fio_gdepth)
	stw r1+0, r12
	lui r1, %hi(fio_scale)
	addi r1, r1, %lo(fio_scale)
	stw r1+0, r12
	lui r1, %hi(fio_rep)
	addi r1, r1, %lo(fio_rep)
	stw r1+0, r12
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	stw r1+0, r12
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	stw r1+0, r12
	seq r1, r4, r12
	lui r3, %hi(fio_listed)
	addi r3, r3, %lo(fio_listed)
	stw r3+0, r1
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	xor r3, r4, r3
	sub r1, r0, r1
	and r1, r3, r1
	xor r5, r4, r1
	lui r1, %hi(fio_fmt)
	addi r1, r1, %lo(fio_fmt)
	stw r1+0, r5
	lui r1, %hi(fio_pos)
	addi r1, r1, %lo(fio_pos)
	stw r1+0, r12
	lui r3, %hi(fio_revert)
	addi r3, r3, %lo(fio_revert)
	addi r6, r0, -1
	stw r3+0, r6
	beq r4, r12, .LBB10_12
.LBB10_7:
	ldw r4, r1+0
	addi r6, r0, 32
.LBB10_8:
	add r7, r5, r4
	ldbu r7, r7+0
	bne r7, r6, .LBB10_10
.LBB10_9:
	addi r4, r4, 1
	stw r1+0, r4
	jal r0, .LBB10_8
.LBB10_10:
	addi r5, r0, 40
	bne r7, r5, .LBB10_12
.LBB10_11:
	stw r3+0, r4
	addi r3, r4, 1
	stw r1+0, r3
.LBB10_12:
	seq r1, r15, r12
	ldw r3, r14+0
	xor r3, r3, r15
	sub r1, r0, r1
	and r1, r3, r1
	xor r5, r15, r1
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	addi r4, r0, 1024
	jal r31, fgets
	beq r1, r12, .LBB10_20
.LBB10_13:
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	jal r31, strlen
	blt r1, r13, .LBB10_19
.LBB10_14:
	addi r3, r1, 1
	lui r4, %hi(fio_rec-2)
	addi r4, r4, %lo(fio_rec-2)
	addi r5, r0, 13
	addi r1, r0, 0
	addi r6, r0, 10
	jal r0, .LBB10_16
.LBB10_15:
	addi r3, r3, -1
	ble r3, r13, .LBB10_19
.LBB10_16:
	add r7, r3, r4
	ldbu r7, r7+0
	beq r7, r5, .LBB10_15
.LBB10_17:
	beq r7, r6, .LBB10_15
.LBB10_18:
	addi r1, r3, -1
.LBB10_19:
	lui r3, %hi(fio_rlen)
	addi r3, r3, %lo(fio_rlen)
	stw r3+0, r1
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	stw r1+0, r12
	jal r0, .LBB10_22
.LBB10_20:
	addi r14, r0, 0
	stb r16+0, r14
	add r12, r13, r0
	bne r11, r14, .LBB10_22
.LBB10_21:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r12, r14, r0
.LBB10_22:
	add r1, r12, r0
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end10:
	.size	f77_rd_begin, .Lfunc_end10-f77_rd_begin
                                        # -- End function
	.globl	f77_rd_i                        # -- Begin function f77_rd_i
	.p2align	2
	.type	f77_rd_i,@function
f77_rd_i:                               # @f77_rd_i
# %bb.0:
	addi sp, sp, -1088
	stw sp+0, lr
	stw sp+1084, r11
	stw sp+1080, r12
	stw sp+1076, r13
	stw sp+1072, r14
	stw sp+1068, r15
	stw sp+1064, r16
	stw sp+1060, r17
	stw sp+1056, r18
	stw sp+1052, r19
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r14, r0, 0
	beq r1, r14, .LBB11_4
.LBB11_1:
	lui r12, %hi(fio_ldone)
	addi r12, r12, %lo(fio_ldone)
	ldbu r1, r12+0
	bne r1, r14, .LBB11_50
.LBB11_2:
	jal r31, fio_list_tok
	beq r1, r14, .LBB11_22
.LBB11_3:
	addi r1, r0, 1
	stb r12+0, r1
	jal r0, .LBB11_50
.LBB11_4:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	beq r1, r14, .LBB11_10
.LBB11_5:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB11_10
.LBB11_6:
	addi r3, r0, 72
	bgt r1, r3, .LBB11_11
.LBB11_7:
	addi r3, r0, 0
	beq r1, r3, .LBB11_50
.LBB11_8:
	addi r3, r0, 65
	bne r1, r3, .LBB11_25
.LBB11_9:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB11_46
.LBB11_10:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB11_7
.LBB11_11:
	addi r3, r0, 73
	beq r1, r3, .LBB11_30
.LBB11_12:
	addi r3, r0, 76
	bne r1, r3, .LBB11_25
.LBB11_13:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	addi r1, r0, 1
	lui r4, %hi(fio_rpos)
	addi r4, r4, %lo(fio_rpos)
	add lr, r14, r0
	blt r3, r1, .LBB11_26
.LBB11_14:
	ldw r5, r4+0
	lui r6, %hi(fio_rlen)
	addi r6, r6, %lo(fio_rlen)
	ldw r6, r6+0
	addi r7, r0, 0
	addi r8, r0, 32
	lui r9, %hi(fio_rec)
	addi r9, r9, %lo(fio_rec)
	addi r10, r0, 1022
	addi r15, r0, 9
	add r16, r3, r0
	add lr, r7, r0
	jal r0, .LBB11_16
.LBB11_15:
	addi r16, r16, -1
	addi r5, r5, 1
	beq r16, r7, .LBB11_26
.LBB11_16:
	add r17, r8, r0
	bge r5, r6, .LBB11_18
.LBB11_17:
	add r17, r5, r9
	ldb r17, r17+0
.LBB11_18:
	bgt lr, r10, .LBB11_15
.LBB11_19:
	beq r17, r15, .LBB11_15
.LBB11_20:
	beq r17, r8, .LBB11_15
.LBB11_21:
	addi r18, lr, 1
	addi r19, sp, 28
	add lr, r19, lr
	stb lr+0, r17
	add lr, r18, r0
	jal r0, .LBB11_15
.LBB11_22:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	ldbu r3, r1+0
	addi r4, r3, -46
	andi r5, r4, 254
	slli r4, r4, 7
	srli r5, r5, 1
	or  r4, r4, r5
	andi r4, r4, 255
	addi r5, r0, 35
	bgtu r4, r5, .LBB11_51
.LBB11_23:
	slli r4, r4, 2
	lui r5, %hi(.LJTI11_0)
	addi r5, r5, %lo(.LJTI11_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB11_24:
	andi r3, r3, 255
	addi r4, r0, 46
	seq r3, r3, r4
	add r1, r3, r1
	ldbu r1, r1+0
	andi r1, r1, 223
	addi r3, r0, 84
	seq r1, r1, r3
	stw r11+0, r1
	jal r0, .LBB11_50
.LBB11_25:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r4, r1+0
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.w.d r1, r4
	jal r0, .LBB11_45
.LBB11_26:
	addi r5, sp, 28
	add r6, r5, lr
	stb r6+0, r14
	ldw r6, r4+0
	add r3, r6, r3
	stw r4+0, r3
	ldbu r3, sp+28
	addi r4, r0, 46
	seq r3, r3, r4
	or  r3, r5, r3
	ldbu r3, r3+0
	addi r4, r0, 83
	ble r3, r4, .LBB11_41
.LBB11_27:
	addi r4, r0, 84
	beq r3, r4, .LBB11_45
.LBB11_28:
	addi r4, r0, 116
	beq r3, r4, .LBB11_45
.LBB11_29:
	addi r1, r0, 102
	beq r3, r1, .LBB11_44
	jal r0, .LBB11_43
.LBB11_30:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	addi r1, r0, 0
	addi r4, r0, 1
	add r10, r1, r0
	blt r3, r4, .LBB11_39
.LBB11_31:
	lui r4, %hi(fio_rpos)
	addi r4, r4, %lo(fio_rpos)
	ldw r4, r4+0
	lui r5, %hi(fio_rlen)
	addi r5, r5, %lo(fio_rlen)
	ldw r5, r5+0
	addi r6, r0, 0
	addi r7, r0, 32
	lui r8, %hi(fio_rec)
	addi r8, r8, %lo(fio_rec)
	addi r9, r0, 1022
	addi lr, r0, 9
	add r14, r3, r0
	add r10, r6, r0
	jal r0, .LBB11_33
.LBB11_32:
	addi r14, r14, -1
	addi r4, r4, 1
	beq r14, r6, .LBB11_39
.LBB11_33:
	add r15, r7, r0
	bge r4, r5, .LBB11_35
.LBB11_34:
	add r15, r4, r8
	ldb r15, r15+0
.LBB11_35:
	bgt r10, r9, .LBB11_32
.LBB11_36:
	beq r15, lr, .LBB11_32
.LBB11_37:
	beq r15, r7, .LBB11_32
.LBB11_38:
	addi r16, r10, 1
	addi r17, sp, 28
	add r10, r17, r10
	stb r10+0, r15
	add r10, r16, r0
	jal r0, .LBB11_32
.LBB11_39:
	addi r4, sp, 28
	add r4, r4, r10
	stb r4+0, r1
	lui r4, %hi(fio_rpos)
	addi r4, r4, %lo(fio_rpos)
	ldw r5, r4+0
	add r3, r5, r3
	stw r4+0, r3
	ldbu r3, sp+28
	beq r3, r1, .LBB11_45
.LBB11_40:
	addi r3, sp, 28
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	jal r0, .LBB11_45
.LBB11_41:
	addi r1, r0, 0
	beq r3, r1, .LBB11_44
.LBB11_42:
	addi r1, r0, 70
	beq r3, r1, .LBB11_44
.LBB11_43:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB11_44:
	addi r1, r0, 0
.LBB11_45:
	stw r11+0, r1
.LBB11_46:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB11_48
.LBB11_47:
	addi r1, r1, -1
	stw r13+0, r1
.LBB11_48:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB11_50
.LBB11_49:
	stw r12+0, r1
.LBB11_50:
	ldw r19, sp+1052
	ldw r18, sp+1056
	ldw r17, sp+1060
	ldw r16, sp+1064
	ldw r15, sp+1068
	ldw r14, sp+1072
	ldw r13, sp+1076
	ldw r12, sp+1080
	ldw r11, sp+1084
	ldw lr, sp+0
	addi sp, sp, 1088
	jalr r0, r31, 0
.LBB11_51:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	stw r11+0, r1
	jal r0, .LBB11_50
.Lfunc_end11:
	.size	f77_rd_i, .Lfunc_end11-f77_rd_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI11_0,@object
.LJTI11_0:
	.word	.LBB11_24
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_24
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_24
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_24
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_51
	.word	.LBB11_24
	.size	.LJTI11_0, 144
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function fio_list_tok
	.type	fio_list_tok,@function
fio_list_tok:                           # @fio_list_tok
# %bb.0:
	addi sp, sp, -64
	stw sp+0, lr
	stw sp+60, r11
	stw sp+56, r12
	stw sp+52, r13
	stw sp+48, r14
	stw sp+44, r15
	stw sp+40, r16
	stw sp+36, r17
	stw sp+32, r18
	stw sp+28, r19
	stw sp+24, r20
	stw sp+20, r21
	lui r13, %hi(fio_l_pending)
	addi r13, r13, %lo(fio_l_pending)
	ldw r1, r13+0
	addi r17, r0, 0
	ble r1, r17, .LBB12_2
.LBB12_1:
	addi r1, r1, -1
	stw r13+0, r1
	addi r1, r0, 0
	jal r0, .LBB12_27
.LBB12_2:
	addi r18, r0, 1
	lui r19, %hi(fio_rlen)
	addi r19, r19, %lo(fio_rlen)
	lui r14, %hi(fio_rpos)
	addi r14, r14, %lo(fio_rpos)
	lui r15, %hi(fio_rec)
	addi r15, r15, %lo(fio_rec)
	addi r16, r0, 38
	lui r20, %hi(.LJTI12_0)
	addi r20, r20, %lo(.LJTI12_0)
	lui r21, %hi(stderr)
	addi r21, r21, %lo(stderr)
	lui r12, %hi(.L.str.28)
	addi r12, r12, %lo(.L.str.28)
	addi r11, r0, 2
	jal r0, .LBB12_4
.LBB12_3:
	jal r31, fio_next_record
.LBB12_4:
	add r3, r18, r0
.LBB12_5:
	ldw r1, r19+0
	ldw r4, r14+0
	bge r4, r1, .LBB12_3
.LBB12_6:
	add r5, r4, r15
	ldbu r5, r5+0
	addi r5, r5, -9
	bgtu r5, r16, .LBB12_12
.LBB12_7:
	slli r5, r5, 2
	add r5, r20, r5
	ldw r5, r5+0
	jalr r0, r5, 0
.LBB12_8:
	addi r4, r4, 1
	stw r14+0, r4
	bne r1, r4, .LBB12_6
	jal r0, .LBB12_3
.LBB12_9:
	andi r1, r3, 1
	bne r1, r17, .LBB12_11
.LBB12_10:
	ldw r3, r21+0
	add r4, r12, r0
	jal r31, fprintf
	add r3, r11, r0
	jal r31, exit
.LBB12_11:
	ldw r1, r14+0
	addi r1, r1, 1
	stw r14+0, r1
	add r3, r17, r0
	jal r0, .LBB12_5
.LBB12_12:
	ldw r4, r14+0
	addi r17, r0, 0
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	add r5, r17, r0
	bge r4, r1, .LBB12_19
.LBB12_13:
	addi r5, r0, 0
	addi r6, r0, 126
	lui r7, %hi(.LJTI12_1)
	addi r7, r7, %lo(.LJTI12_1)
	jal r0, .LBB12_15
.LBB12_14:
	addi r4, r4, 1
	stw r14+0, r4
	beq r1, r4, .LBB12_19
.LBB12_15:
	add r8, r4, r15
	ldbu r8, r8+0
	addi r9, r8, -9
	bgtu r9, r16, .LBB12_17
.LBB12_16:
	slli r9, r9, 2
	add r9, r7, r9
	ldw r9, r9+0
	jalr r0, r9, 0
.LBB12_17:
	bgt r5, r6, .LBB12_14
.LBB12_18:
	addi r9, r5, 1
	add r5, r5, r3
	stb r5+0, r8
	add r5, r9, r0
	jal r0, .LBB12_14
.LBB12_19:
	add r1, r5, r3
	stb r1+0, r17
	addi r1, r0, -11
.LBB12_20:
	add r4, r17, r3
	ldb r4, r4+0
	addi r5, r4, -58
	addi r17, r17, 1
	bgtu r5, r1, .LBB12_20
.LBB12_21:
	addi r1, r0, 0
	addi r3, r0, 1
	beq r17, r3, .LBB12_27
.LBB12_22:
	addi r3, r0, 42
	bne r4, r3, .LBB12_27
.LBB12_23:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	addi r4, r0, 0
	addi r5, r0, 10
	add r3, r12, r0
	add r14, r4, r0
	jal r31, strtol
	blt r1, r11, .LBB12_25
.LBB12_24:
	addi r1, r1, -1
	stw r13+0, r1
.LBB12_25:
	add r11, r17, r12
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	add r1, r14, r0
	jal r0, .LBB12_27
.LBB12_26:
	addi r1, r4, 1
	stw r14+0, r1
	addi r1, r0, 1
.LBB12_27:
	ldw r21, sp+20
	ldw r20, sp+24
	ldw r19, sp+28
	ldw r18, sp+32
	ldw r17, sp+36
	ldw r16, sp+40
	ldw r15, sp+44
	ldw r14, sp+48
	ldw r13, sp+52
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.Lfunc_end12:
	.size	fio_list_tok, .Lfunc_end12-fio_list_tok
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI12_0,@object
.LJTI12_0:
	.word	.LBB12_8
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_8
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_9
	.word	.LBB12_12
	.word	.LBB12_12
	.word	.LBB12_26
	.size	.LJTI12_0, 156
	.type	.LJTI12_1,@object
.LJTI12_1:
	.word	.LBB12_19
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_19
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_19
	.word	.LBB12_17
	.word	.LBB12_17
	.word	.LBB12_19
	.size	.LJTI12_1, 156
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_in_real
	.type	.LCPI13_0,@object
.LCPI13_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI13_0, 8
	.text
	.p2align	2
	.type	fio_in_real,@function
fio_in_real:                            # @fio_in_real
# %bb.0:
	addi sp, sp, -2048
	addi sp, sp, -1120
	stw sp+0, lr
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1117, r11
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1113, r12
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1109, r13
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1105, r14
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1101, r15
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1097, r16
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1093, r17
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1089, r18
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1085, r19
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1081, r20
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1077, r21
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1073, r22
	addi r1, sp, 32
	addi r5, r0, 0
	addi r8, r0, 1
	lui r6, %hi(fio_rpos)
	addi r6, r6, %lo(fio_rpos)
	add r13, r5, r0
	blt r3, r8, .LBB13_9
.LBB13_1:
	ldw r7, r6+0
	lui r9, %hi(fio_rlen)
	addi r9, r9, %lo(fio_rlen)
	ldw r9, r9+0
	addi r10, r0, 0
	addi lr, r0, 32
	lui r11, %hi(fio_rec)
	addi r11, r11, %lo(fio_rec)
	addi r12, r0, 1022
	addi r14, r0, 9
	add r15, r3, r0
	add r13, r10, r0
	jal r0, .LBB13_3
.LBB13_2:
	addi r15, r15, -1
	addi r7, r7, 1
	beq r15, r10, .LBB13_9
.LBB13_3:
	add r16, lr, r0
	bge r7, r9, .LBB13_5
.LBB13_4:
	add r16, r7, r11
	ldb r16, r16+0
.LBB13_5:
	bgt r13, r12, .LBB13_2
.LBB13_6:
	beq r16, r14, .LBB13_2
.LBB13_7:
	beq r16, lr, .LBB13_2
.LBB13_8:
	addi r17, r13, 1
	add r19, sp, r0
	addi r19, r19, 2047
	addi r18, r19, 49
	add r13, r18, r13
	stb r13+0, r16
	add r13, r17, r0
	jal r0, .LBB13_2
.LBB13_9:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r7, r9, 49
	add r7, r7, r13
	stb r7+0, r5
	ldw r7, r6+0
	add r3, r7, r3
	stw r6+0, r3
	add r6, sp, r0
	addi r6, r6, 2047
	ldbu r3, r6+49
	lui r9, %hi(.LCPI13_0)
	addi r9, r9, %lo(.LCPI13_0)
	ldw r7, r9+4
	ldw r6, r9+0
	beq r3, r5, .LBB13_62
.LBB13_10:
	addi r11, r0, 43
	beq r3, r11, .LBB13_13
.LBB13_11:
	addi r5, r0, 45
	bne r3, r5, .LBB13_14
.LBB13_12:
	addi r5, r0, 0
	add lr, r8, r0
	jal r0, .LBB13_15
.LBB13_13:
	add r5, r8, r0
	add lr, r8, r0
	jal r0, .LBB13_15
.LBB13_14:
	addi lr, r0, 0
	addi r5, r0, 1
.LBB13_15:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r3, r9, 49
	or  r3, r3, lr
	ldbu r21, r3+0
	addi r9, r0, 0
	beq r21, r9, .LBB13_38
.LBB13_16:
	addi r13, r0, 0
	addi r14, r0, 1
	addi r15, r0, 46
	addi r16, r0, -10
	addi r17, r0, 70
	lui r18, %hi(.LJTI13_0)
	addi r18, r18, %lo(.LJTI13_0)
	addi r19, r0, -1
	add r10, r14, r0
	add r12, r13, r0
	add r20, r13, r0
	add r3, r13, r0
.LBB13_17:
	andi r8, r21, 255
	bne r8, r15, .LBB13_19
.LBB13_18:
	add r20, r14, r0
	jal r0, .LBB13_28
.LBB13_19:
	slli r22, r21, 24
	srai r22, r22, 24
	addi r22, r22, -58
	bgeu r22, r16, .LBB13_25
.LBB13_20:
	addi r8, r8, -43
	bgtu r8, r17, .LBB13_36
.LBB13_21:
	slli r8, r8, 2
	add r8, r18, r8
	ldw r22, r8+0
	add r8, r13, r0
	add r21, r14, r0
	jalr r0, r22, 0
.LBB13_22:
	addi r21, lr, 1
	add r22, sp, r0
	addi r22, r22, 2047
	addi r8, r22, 49
	add r8, r8, r21
	ldbu r8, r8+0
	beq r8, r11, .LBB13_32
.LBB13_23:
	addi r22, r0, 45
	bne r8, r22, .LBB13_34
.LBB13_24:
	add r8, r13, r0
	add r10, r19, r0
	jal r0, .LBB13_33
.LBB13_25:
	addi r8, r0, 1022
	bgt r3, r8, .LBB13_27
.LBB13_26:
	addi r8, r3, 1
	addi r22, sp, 1072
	add r3, r22, r3
	stb r3+0, r21
	add r3, r8, r0
.LBB13_27:
	addi r8, r0, 0
	sne r8, r20, r8
	add r12, r12, r8
.LBB13_28:
	add r8, r14, r0
.LBB13_29:
	add r21, r14, r0
.LBB13_30:
	beq r21, r13, .LBB13_37
.LBB13_31:
	addi lr, lr, 1
	add r22, sp, r0
	addi r22, r22, 2047
	addi r21, r22, 49
	add r21, r21, lr
	ldbu r21, r21+0
	sne r22, r21, r13
	and r22, r22, r8
	bne r22, r13, .LBB13_17
	jal r0, .LBB13_37
.LBB13_32:
	add r8, r13, r0
.LBB13_33:
	add lr, r21, r0
	jal r0, .LBB13_29
.LBB13_34:
	add r8, r13, r0
	jal r0, .LBB13_29
.LBB13_35:
	add r8, r13, r0
	add r10, r19, r0
	jal r0, .LBB13_29
.LBB13_36:
	add r8, r14, r0
	add r21, r13, r0
	bne r21, r13, .LBB13_31
.LBB13_37:
	addi r11, r0, 0
	seq r11, r20, r11
	xor r4, r4, r12
	sub r11, r0, r11
	and r4, r4, r11
	xor r4, r12, r4
	add r11, r9, r0
	beq r8, r9, .LBB13_39
	jal r0, .LBB13_43
.LBB13_38:
	add r3, r9, r0
	add r10, r8, r0
	add r11, r9, r0
	bne r8, r9, .LBB13_43
.LBB13_39:
	add r12, sp, r0
	addi r12, r12, 2047
	addi r11, r12, 49
	add r11, r11, lr
	ldbu r13, r11+0
	addi r11, r0, 0
	beq r13, r11, .LBB13_43
.LBB13_40:
	add r12, sp, r0
	addi r12, r12, 2047
	addi r11, r12, 49
	add lr, lr, r11
	addi lr, lr, 1
	addi r11, r0, 0
	addi r12, r0, 9
	addi r14, r0, 10
	add r15, r11, r0
.LBB13_41:
	slli r13, r13, 24
	srai r13, r13, 24
	addi r13, r13, -48
	sgtu r16, r13, r12
	mul r17, r15, r14
	add r13, r13, r17
	xor r15, r15, r13
	sub r16, r0, r16
	and r15, r15, r16
	xor r15, r13, r15
	ldbu r13, lr+0
	addi lr, lr, 1
	bne r13, r11, .LBB13_41
.LBB13_42:
	mul r11, r15, r10
.LBB13_43:
	addi r10, sp, 1072
	add r10, r10, r3
	stb r10+0, r9
	beq r3, r9, .LBB13_62
.LBB13_44:
	sub r9, r3, r4
	lui r6, %hi(fio_scale)
	addi r6, r6, %lo(fio_scale)
	ldw r6, r6+0
	sub r7, r0, r8
	and r6, r6, r7
	addi r7, r0, 0
	addi r10, sp, 32
	bne r5, r7, .LBB13_46
.LBB13_45:
	addi r10, r1, 1
	addi r1, r0, 45
	stb sp+32, r1
	addi r7, r0, 1
.LBB13_46:
	sub r6, r11, r6
	addi r5, r0, 1
	blt r9, r5, .LBB13_51
.LBB13_47:
	blt r3, r5, .LBB13_57
.LBB13_48:
	xor r1, r9, r3
	slt r4, r9, r3
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r3, r1
	addi r4, sp, 32
	or  r5, r4, r7
	addi r4, r0, 0
.LBB13_49:
	addi r8, sp, 1072
	add r8, r8, r4
	ldbu r8, r8+0
	add r9, r5, r4
	stb r9+0, r8
	addi r4, r4, 1
	bne r1, r4, .LBB13_49
.LBB13_50:
	add r7, r7, r4
	jal r0, .LBB13_58
.LBB13_51:
	addi r1, sp, 32
	or  r1, r1, r7
	addi r8, r0, 48
	stb r1+0, r8
	addi r1, r7, 2
	addi r7, r0, 46
	stb r10+1, r7
	addi r7, r0, -1
	bgt r9, r7, .LBB13_54
.LBB13_52:
	sub r4, r4, r3
	addi r7, r0, 0
.LBB13_53:
	add r9, r1, r0
	addi r1, r1, 1
	addi r10, sp, 32
	add r9, r10, r9
	stb r9+0, r8
	addi r4, r4, -1
	bne r4, r7, .LBB13_53
.LBB13_54:
	blt r3, r5, .LBB13_61
.LBB13_55:
	addi r4, sp, 1072
	addi r5, r0, 0
.LBB13_56:
	add r7, r1, r0
	ldbu r8, r4+0
	addi r1, r1, 1
	addi r9, sp, 32
	add r7, r9, r7
	stb r7+0, r8
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r5, .LBB13_56
	jal r0, .LBB13_61
.LBB13_57:
	addi r4, r0, 0
.LBB13_58:
	addi r1, sp, 32
	add r1, r1, r7
	addi r5, r0, 46
	stb r1+0, r5
	addi r1, r7, 1
	ble r3, r4, .LBB13_61
.LBB13_59:
	sub r3, r3, r4
	addi r5, sp, 1072
	add r4, r5, r4
	addi r5, r0, 0
.LBB13_60:
	ldbu r7, r4+0
	addi r8, sp, 32
	add r8, r8, r1
	stb r8+0, r7
	addi r1, r1, 1
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r5, .LBB13_60
.LBB13_61:
	addi r11, sp, 32
	add r3, r11, r1
	addi r4, r0, 1040
	sub r4, r4, r1
	lui r5, %hi(.L.str.30)
	addi r5, r5, %lo(.L.str.30)
	jal r31, snprintf
	addi r4, r0, 0
	add r3, r11, r0
	jal r31, strtod
	add r6, r1, r0
	add r7, r2, r0
.LBB13_62:
	add r1, r6, r0
	add r2, r7, r0
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r22, r3+1073
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r21, r3+1077
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r20, r3+1081
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r19, r3+1085
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r18, r3+1089
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r17, r3+1093
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r16, r3+1097
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r15, r3+1101
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r14, r3+1105
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r13, r3+1109
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r12, r3+1113
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r11, r3+1117
	ldw lr, sp+0
	addi sp, sp, 2047
	addi sp, sp, 1121
	jalr r0, r31, 0
.Lfunc_end13:
	.size	fio_in_real, .Lfunc_end13-fio_in_real
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI13_0,@object
.LJTI13_0:
	.word	.LBB13_30
	.word	.LBB13_36
	.word	.LBB13_35
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_22
	.word	.LBB13_22
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_22
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_22
	.word	.LBB13_22
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_22
	.size	.LJTI13_0, 284
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_rd_d
	.type	.LCPI14_0,@object
.LCPI14_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI14_0, 8
	.text
	.globl	f77_rd_d
	.p2align	2
	.type	f77_rd_d,@function
f77_rd_d:                               # @f77_rd_d
# %bb.0:
	addi sp, sp, -1088
	stw sp+0, lr
	stw sp+1084, r11
	stw sp+1080, r12
	stw sp+1076, r13
	stw sp+1072, r14
	stw sp+1068, r15
	stw sp+1064, r16
	stw sp+1060, r17
	stw sp+1056, r18
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r12, r0, 0
	beq r1, r12, .LBB14_4
.LBB14_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r12, .LBB14_35
.LBB14_2:
	jal r31, fio_list_tok
	beq r1, r12, .LBB14_22
.LBB14_3:
	addi r1, r0, 1
	stb r13+0, r1
	jal r0, .LBB14_35
.LBB14_4:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB14_9
.LBB14_5:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB14_9
.LBB14_6:
	addi r3, r0, 72
	bgt r1, r3, .LBB14_10
.LBB14_7:
	addi r3, r0, 0
	beq r1, r3, .LBB14_35
.LBB14_8:
	addi r3, r0, 65
	beq r1, r3, .LBB14_21
	jal r0, .LBB14_26
.LBB14_9:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB14_7
.LBB14_10:
	addi r3, r0, 76
	beq r1, r3, .LBB14_21
.LBB14_11:
	addi r3, r0, 73
	bne r1, r3, .LBB14_26
.LBB14_12:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	addi r3, r3, %lo(fio_rpos)
	add r10, r14, r0
	blt r1, r4, .LBB14_27
.LBB14_13:
	ldw r4, r3+0
	lui r5, %hi(fio_rlen)
	addi r5, r5, %lo(fio_rlen)
	ldw r5, r5+0
	addi r6, r0, 0
	addi r7, r0, 32
	lui r8, %hi(fio_rec)
	addi r8, r8, %lo(fio_rec)
	addi r9, r0, 1022
	addi lr, r0, 9
	add r15, r1, r0
	add r10, r6, r0
	jal r0, .LBB14_15
.LBB14_14:
	addi r15, r15, -1
	addi r4, r4, 1
	beq r15, r6, .LBB14_27
.LBB14_15:
	add r16, r7, r0
	bge r4, r5, .LBB14_17
.LBB14_16:
	add r16, r4, r8
	ldb r16, r16+0
.LBB14_17:
	bgt r10, r9, .LBB14_14
.LBB14_18:
	beq r16, lr, .LBB14_14
.LBB14_19:
	beq r16, r7, .LBB14_14
.LBB14_20:
	addi r17, r10, 1
	addi r18, sp, 32
	add r10, r18, r10
	stb r10+0, r16
	add r10, r17, r0
	jal r0, .LBB14_14
.LBB14_21:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB14_31
.LBB14_22:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	ldbu r6, r1+0
	add r7, r12, r0
	beq r6, r12, .LBB14_25
.LBB14_23:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r7, r1, r0
.LBB14_24:
	add r8, r7, r0
	andi r7, r6, 223
	seq r9, r7, r3
	seq r7, r7, r4
	sub r9, r0, r9
	sub r7, r0, r7
	xori r10, r6, 69
	and r7, r10, r7
	xor r6, r6, r7
	xori r7, r6, 69
	and r7, r7, r9
	xor r6, r6, r7
	addi r7, sp, 32
	add r7, r7, r8
	stb r7+0, r6
	addi r7, r8, 1
	add r6, r8, r5
	ldbu r6, r6+0
	bne r6, r1, .LBB14_24
.LBB14_25:
	addi r3, sp, 32
	add r1, r3, r7
	stb r1+0, r12
	add r4, r12, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
	jal r0, .LBB14_35
.LBB14_26:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r4, r1+0
	jal r31, fio_in_real
	stw r11+4, r2
	stw r11+0, r1
	jal r0, .LBB14_31
.LBB14_27:
	addi r4, sp, 32
	add r4, r4, r10
	stb r4+0, r14
	ldw r4, r3+0
	add r1, r4, r1
	stw r3+0, r1
	ldbu r1, sp+32
	beq r1, r14, .LBB14_29
.LBB14_28:
	addi r3, sp, 32
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	fcvt.d.w r4, r1
	jal r0, .LBB14_30
.LBB14_29:
	lui r1, %hi(.LCPI14_0)
	addi r1, r1, %lo(.LCPI14_0)
	ldw r5, r1+4
	ldw r4, r1+0
.LBB14_30:
	stw r11+4, r5
	stw r11+0, r4
.LBB14_31:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB14_33
.LBB14_32:
	addi r1, r1, -1
	stw r13+0, r1
.LBB14_33:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB14_35
.LBB14_34:
	stw r12+0, r1
.LBB14_35:
	ldw r18, sp+1056
	ldw r17, sp+1060
	ldw r16, sp+1064
	ldw r15, sp+1068
	ldw r14, sp+1072
	ldw r13, sp+1076
	ldw r12, sp+1080
	ldw r11, sp+1084
	ldw lr, sp+0
	addi sp, sp, 1088
	jalr r0, r31, 0
.Lfunc_end14:
	.size	f77_rd_d, .Lfunc_end14-f77_rd_d
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rd_r
	.type	.LCPI15_0,@object
.LCPI15_0:
	.word	0x00000000                      # float 0
	.size	.LCPI15_0, 4
	.text
	.globl	f77_rd_r
	.p2align	2
	.type	f77_rd_r,@function
f77_rd_r:                               # @f77_rd_r
# %bb.0:
	addi sp, sp, -1088
	stw sp+0, lr
	stw sp+1084, r11
	stw sp+1080, r12
	stw sp+1076, r13
	stw sp+1072, r14
	stw sp+1068, r15
	stw sp+1064, r16
	stw sp+1060, r17
	stw sp+1056, r18
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r12, r0, 0
	beq r1, r12, .LBB15_4
.LBB15_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r12, .LBB15_35
.LBB15_2:
	jal r31, fio_list_tok
	beq r1, r12, .LBB15_22
.LBB15_3:
	addi r1, r0, 1
	stb r13+0, r1
	jal r0, .LBB15_35
.LBB15_4:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB15_9
.LBB15_5:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB15_9
.LBB15_6:
	addi r3, r0, 72
	bgt r1, r3, .LBB15_10
.LBB15_7:
	addi r3, r0, 0
	beq r1, r3, .LBB15_35
.LBB15_8:
	addi r3, r0, 65
	beq r1, r3, .LBB15_21
	jal r0, .LBB15_26
.LBB15_9:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB15_7
.LBB15_10:
	addi r3, r0, 76
	beq r1, r3, .LBB15_21
.LBB15_11:
	addi r3, r0, 73
	bne r1, r3, .LBB15_26
.LBB15_12:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	addi r3, r3, %lo(fio_rpos)
	add r10, r14, r0
	blt r1, r4, .LBB15_27
.LBB15_13:
	ldw r4, r3+0
	lui r5, %hi(fio_rlen)
	addi r5, r5, %lo(fio_rlen)
	ldw r5, r5+0
	addi r6, r0, 0
	addi r7, r0, 32
	lui r8, %hi(fio_rec)
	addi r8, r8, %lo(fio_rec)
	addi r9, r0, 1022
	addi lr, r0, 9
	add r15, r1, r0
	add r10, r6, r0
	jal r0, .LBB15_15
.LBB15_14:
	addi r15, r15, -1
	addi r4, r4, 1
	beq r15, r6, .LBB15_27
.LBB15_15:
	add r16, r7, r0
	bge r4, r5, .LBB15_17
.LBB15_16:
	add r16, r4, r8
	ldb r16, r16+0
.LBB15_17:
	bgt r10, r9, .LBB15_14
.LBB15_18:
	beq r16, lr, .LBB15_14
.LBB15_19:
	beq r16, r7, .LBB15_14
.LBB15_20:
	addi r17, r10, 1
	addi r18, sp, 32
	add r10, r18, r10
	stb r10+0, r16
	add r10, r17, r0
	jal r0, .LBB15_14
.LBB15_21:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB15_31
.LBB15_22:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	ldbu r6, r1+0
	add r7, r12, r0
	beq r6, r12, .LBB15_25
.LBB15_23:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r7, r1, r0
.LBB15_24:
	add r8, r7, r0
	andi r7, r6, 223
	seq r9, r7, r3
	seq r7, r7, r4
	sub r9, r0, r9
	sub r7, r0, r7
	xori r10, r6, 69
	and r7, r10, r7
	xor r6, r6, r7
	xori r7, r6, 69
	and r7, r7, r9
	xor r6, r6, r7
	addi r7, sp, 32
	add r7, r7, r8
	stb r7+0, r6
	addi r7, r8, 1
	add r6, r8, r5
	ldbu r6, r6+0
	bne r6, r1, .LBB15_24
.LBB15_25:
	addi r3, sp, 32
	add r1, r3, r7
	stb r1+0, r12
	add r4, r12, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	jal r0, .LBB15_35
.LBB15_26:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r4, r1+0
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	jal r0, .LBB15_30
.LBB15_27:
	addi r4, sp, 32
	add r4, r4, r10
	stb r4+0, r14
	ldw r4, r3+0
	add r1, r4, r1
	stw r3+0, r1
	ldbu r1, sp+32
	beq r1, r14, .LBB15_29
.LBB15_28:
	addi r3, sp, 32
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	fcvt.s.w r1, r1
	jal r0, .LBB15_30
.LBB15_29:
	lui r1, %hi(.LCPI15_0)
	addi r1, r1, %lo(.LCPI15_0)
	ldw r1, r1+0
.LBB15_30:
	stw r11+0, r1
.LBB15_31:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB15_33
.LBB15_32:
	addi r1, r1, -1
	stw r13+0, r1
.LBB15_33:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB15_35
.LBB15_34:
	stw r12+0, r1
.LBB15_35:
	ldw r18, sp+1056
	ldw r17, sp+1060
	ldw r16, sp+1064
	ldw r15, sp+1068
	ldw r14, sp+1072
	ldw r13, sp+1076
	ldw r12, sp+1080
	ldw r11, sp+1084
	ldw lr, sp+0
	addi sp, sp, 1088
	jalr r0, r31, 0
.Lfunc_end15:
	.size	f77_rd_r, .Lfunc_end15-f77_rd_r
                                        # -- End function
	.globl	f77_rd_end                      # -- Begin function f77_rd_end
	.p2align	2
	.type	f77_rd_end,@function
f77_rd_end:                             # @f77_rd_end
# %bb.0:
	lui r1, %hi(fio_reading)
	addi r1, r1, %lo(fio_reading)
	addi r3, r0, 0
	stb r1+0, r3
	jalr r0, r31, 0
.Lfunc_end16:
	.size	f77_rd_end, .Lfunc_end16-f77_rd_end
                                        # -- End function
	.globl	f77_open                        # -- Begin function f77_open
	.p2align	2
	.type	f77_open,@function
f77_open:                               # @f77_open
# %bb.0:
	addi sp, sp, -304
	stw sp+0, lr
	stw sp+300, r11
	stw sp+296, r12
	stw sp+292, r13
	stw sp+288, r14
	stw sp+284, r15
	stw sp+280, r16
	add r12, r7, r0
	add r13, r6, r0
	add r14, r5, r0
	add r15, r4, r0
	add r11, r3, r0
	addi r1, r3, -32
	addi r3, r0, -31
	bltu r1, r3, .LBB17_2
.LBB17_1:
	addi r1, r11, -5
	addi r3, r0, 1
	bgtu r1, r3, .LBB17_3
.LBB17_2:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB17_3:
	addi r1, r14, -256
	addi r3, r0, -256
	bgtu r1, r3, .LBB17_5
.LBB17_4:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB17_5:
	addi r16, sp, 24
	add r3, r16, r0
	add r4, r15, r0
	add r5, r14, r0
	jal r31, memcpy
	add r1, r16, r14
	addi r15, r0, 0
	stb r1+0, r15
	addi r16, r0, 1
	blt r14, r16, .LBB17_9
.LBB17_6:
	addi r1, sp, 24
	addi r1, r1, -1
	addi r3, r0, 32
	addi r4, r0, 0
.LBB17_7:
	add r6, r1, r14
	ldbu r7, r6+0
	bne r7, r3, .LBB17_9
.LBB17_8:
	add r5, r14, r0
	addi r14, r14, -1
	stb r6+0, r4
	bgt r5, r16, .LBB17_7
.LBB17_9:
	slli r1, r11, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r14, r1, r3
	ldw r3, r14+0
	beq r3, r15, .LBB17_11
.LBB17_10:
	jal r31, fclose
	stw r14+0, r15
.LBB17_11:
	beq r13, r15, .LBB17_41
.LBB17_12:
	blt r12, r16, .LBB17_41
.LBB17_13:
	addi r4, r0, 0
	addi r3, r0, 3
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
	addi r1, r0, 26
.LBB17_14:
	beq r4, r3, .LBB17_18
.LBB17_15:
	add r6, r4, r5
	ldb r6, r6+0
	add r7, r13, r4
	ldb r7, r7+0
	addi r8, r7, -97
	andi r8, r8, 255
	sltu r8, r8, r1
	addi r9, r7, -32
	xor r9, r9, r7
	sub r8, r0, r8
	and r8, r9, r8
	xor r7, r7, r8
	bne r7, r6, .LBB17_27
.LBB17_16:
	addi r4, r4, 1
	bne r12, r4, .LBB17_14
.LBB17_17:
	add r4, r12, r0
	beq r4, r3, .LBB17_19
	jal r0, .LBB17_27
.LBB17_18:
	add r4, r3, r0
	bne r4, r3, .LBB17_27
.LBB17_19:
	addi r3, r0, 4
	blt r12, r3, .LBB17_23
.LBB17_20:
	addi r3, r13, 3
	addi r4, r12, -3
	addi r5, r0, 32
	addi r6, r0, 0
.LBB17_21:
	ldbu r7, r3+0
	bne r7, r5, .LBB17_27
.LBB17_22:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r6, .LBB17_21
.LBB17_23:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	addi r3, sp, 24
	jal r31, fopen
	addi r12, r0, 0
	bne r1, r12, .LBB17_25
.LBB17_24:
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	addi r3, sp, 24
	jal r31, fopen
.LBB17_25:
	bne r1, r12, .LBB17_60
.LBB17_26:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
	jal r0, .LBB17_58
.LBB17_27:
	addi r4, r0, 0
	addi r3, r0, 3
	lui r5, %hi(.L.str.13)
	addi r5, r5, %lo(.L.str.13)
.LBB17_28:
	beq r4, r3, .LBB17_32
.LBB17_29:
	add r6, r4, r5
	ldb r6, r6+0
	add r7, r13, r4
	ldb r7, r7+0
	addi r8, r7, -97
	andi r8, r8, 255
	sltu r8, r8, r1
	addi r9, r7, -32
	xor r9, r9, r7
	sub r8, r0, r8
	and r8, r9, r8
	xor r7, r7, r8
	bne r7, r6, .LBB17_41
.LBB17_30:
	addi r4, r4, 1
	bne r12, r4, .LBB17_28
.LBB17_31:
	add r1, r12, r0
	beq r1, r3, .LBB17_33
	jal r0, .LBB17_41
.LBB17_32:
	add r1, r3, r0
	bne r1, r3, .LBB17_41
.LBB17_33:
	addi r1, r0, 4
	blt r12, r1, .LBB17_37
.LBB17_34:
	addi r1, r13, 3
	addi r3, r12, -3
	addi r4, r0, 32
	addi r5, r0, 0
.LBB17_35:
	ldbu r6, r1+0
	bne r6, r4, .LBB17_41
.LBB17_36:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB17_35
.LBB17_37:
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	addi r3, sp, 24
	jal r31, fopen
	addi r12, r0, 0
	beq r1, r12, .LBB17_39
.LBB17_38:
	add r3, r1, r0
	jal r31, fclose
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	addi r5, sp, 24
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB17_39:
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	addi r3, sp, 24
	jal r31, fopen
	bne r1, r12, .LBB17_60
.LBB17_40:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	jal r0, .LBB17_58
.LBB17_41:
	beq r13, r15, .LBB17_53
.LBB17_42:
	blt r12, r16, .LBB17_53
.LBB17_43:
	addi r1, r0, 7
	lui r3, %hi(.L.str.17)
	addi r3, r3, %lo(.L.str.17)
	addi r4, r0, 26
.LBB17_44:
	beq r15, r1, .LBB17_48
.LBB17_45:
	add r5, r15, r3
	ldb r5, r5+0
	add r6, r13, r15
	ldb r6, r6+0
	addi r7, r6, -97
	andi r7, r7, 255
	sltu r7, r7, r4
	addi r8, r6, -32
	xor r8, r8, r6
	sub r7, r0, r7
	and r7, r8, r7
	xor r6, r6, r7
	bne r6, r5, .LBB17_55
.LBB17_46:
	addi r15, r15, 1
	bne r12, r15, .LBB17_44
.LBB17_47:
	add r3, r12, r0
	beq r3, r1, .LBB17_49
	jal r0, .LBB17_55
.LBB17_48:
	add r3, r1, r0
	bne r3, r1, .LBB17_55
.LBB17_49:
	addi r1, r0, 8
	blt r12, r1, .LBB17_53
.LBB17_50:
	addi r1, r13, 7
	addi r3, r12, -7
	addi r4, r0, 32
	addi r5, r0, 0
.LBB17_51:
	ldbu r6, r1+0
	bne r6, r4, .LBB17_55
.LBB17_52:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB17_51
.LBB17_53:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	addi r3, sp, 24
	jal r31, fopen
	addi r12, r0, 0
	beq r1, r12, .LBB17_56
.LBB17_54:
	bne r1, r12, .LBB17_60
	jal r0, .LBB17_57
.LBB17_55:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	jal r0, .LBB17_59
.LBB17_56:
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	addi r3, sp, 24
	jal r31, fopen
	bne r1, r12, .LBB17_60
.LBB17_57:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
.LBB17_58:
	addi r5, sp, 24
.LBB17_59:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	addi r1, r0, 0
.LBB17_60:
	stw r14+0, r1
	slli r1, r11, 8
	lui r3, %hi(fio_ufname)
	addi r3, r3, %lo(fio_ufname)
	add r3, r1, r3
	addi r4, sp, 24
	jal r31, strcpy
	ldw r16, sp+280
	ldw r15, sp+284
	ldw r14, sp+288
	ldw r13, sp+292
	ldw r12, sp+296
	ldw r11, sp+300
	ldw lr, sp+0
	addi sp, sp, 304
	jalr r0, r31, 0
.Lfunc_end17:
	.size	f77_open, .Lfunc_end17-f77_open
                                        # -- End function
	.globl	f77_close                       # -- Begin function f77_close
	.p2align	2
	.type	f77_close,@function
f77_close:                              # @f77_close
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	addi r1, r0, 31
	bgtu r3, r1, .LBB18_32
.LBB18_1:
	slli r1, r3, 2
	lui r6, %hi(fio_ufile)
	addi r6, r6, %lo(fio_ufile)
	add r11, r1, r6
	ldw r6, r11+0
	addi r1, r0, 0
	beq r6, r1, .LBB18_32
.LBB18_2:
	addi r6, r0, 1
	beq r4, r1, .LBB18_9
.LBB18_3:
	addi r7, r0, 1
	add r12, r1, r0
	blt r5, r7, .LBB18_16
.LBB18_4:
	addi r6, r0, 0
	addi r8, r0, 6
	lui r9, %hi(.L.str.20)
	addi r9, r9, %lo(.L.str.20)
	addi r10, r0, 26
	add lr, r6, r0
.LBB18_5:
	beq lr, r8, .LBB18_10
.LBB18_6:
	add r12, lr, r9
	ldb r12, r12+0
	add r13, r4, lr
	ldb r13, r13+0
	addi r14, r13, -97
	andi r14, r14, 255
	sltu r14, r14, r10
	addi r15, r13, -32
	xor r15, r15, r13
	sub r14, r0, r14
	and r14, r15, r14
	xor r13, r13, r14
	bne r13, r12, .LBB18_15
.LBB18_7:
	addi lr, lr, 1
	bne r5, lr, .LBB18_5
.LBB18_8:
	add r9, r5, r0
	add r12, r6, r0
	beq r9, r8, .LBB18_11
	jal r0, .LBB18_16
.LBB18_9:
	add r12, r1, r0
	add r13, r3, r0
	beq r6, r1, .LBB18_17
	jal r0, .LBB18_29
.LBB18_10:
	add r9, r8, r0
	add r12, r6, r0
	bne r9, r8, .LBB18_16
.LBB18_11:
	addi r8, r0, 7
	add r6, r7, r0
	add r12, r7, r0
	blt r5, r8, .LBB18_16
.LBB18_12:
	addi r10, r5, -7
	addi r7, r4, 6
	addi r8, r0, 32
	addi r9, r0, 1
.LBB18_13:
	ldbu lr, r7+0
	seq r6, lr, r8
	addi r12, r10, -1
	sltu r10, r12, r10
	bne r10, r9, .LBB18_33
.LBB18_14:
	addi r7, r7, 1
	add r10, r12, r0
	add r12, r6, r0
	beq lr, r8, .LBB18_13
	jal r0, .LBB18_16
.LBB18_15:
	add r12, r6, r0
.LBB18_16:
	add r13, r3, r0
	bne r6, r1, .LBB18_29
.LBB18_17:
	addi r1, r0, 1
	blt r5, r1, .LBB18_28
.LBB18_18:
	addi r3, r0, 0
	addi r1, r0, 4
	lui r6, %hi(.L.str.21)
	addi r6, r6, %lo(.L.str.21)
	addi r7, r0, 26
.LBB18_19:
	beq r3, r1, .LBB18_23
.LBB18_20:
	add r8, r3, r6
	ldb r8, r8+0
	add r9, r4, r3
	ldb r9, r9+0
	addi r10, r9, -97
	andi r10, r10, 255
	sltu r10, r10, r7
	addi lr, r9, -32
	xor lr, lr, r9
	sub r10, r0, r10
	and r10, lr, r10
	xor r9, r9, r10
	bne r9, r8, .LBB18_28
.LBB18_21:
	addi r3, r3, 1
	bne r5, r3, .LBB18_19
.LBB18_22:
	add r3, r5, r0
	beq r3, r1, .LBB18_24
	jal r0, .LBB18_28
.LBB18_23:
	add r3, r1, r0
	bne r3, r1, .LBB18_28
.LBB18_24:
	addi r1, r0, 5
	blt r5, r1, .LBB18_29
.LBB18_25:
	addi r1, r4, 4
	addi r3, r5, -4
	addi r4, r0, 32
	addi r5, r0, 0
.LBB18_26:
	ldbu r6, r1+0
	bne r6, r4, .LBB18_28
.LBB18_27:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB18_26
	jal r0, .LBB18_29
.LBB18_28:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB18_29:
	ldw r3, r11+0
	jal r31, fclose
	addi r14, r0, 0
	stw r11+0, r14
	slli r11, r13, 8
	lui r13, %hi(fio_ufname)
	addi r13, r13, %lo(fio_ufname)
	beq r12, r14, .LBB18_31
.LBB18_30:
	add r3, r11, r13
	jal r31, remove
.LBB18_31:
	add r1, r11, r13
	stb r1+0, r14
.LBB18_32:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB18_33:
	add r12, r6, r0
	add r13, r3, r0
	beq r6, r1, .LBB18_17
	jal r0, .LBB18_29
.Lfunc_end18:
	.size	f77_close, .Lfunc_end18-f77_close
                                        # -- End function
	.globl	f77_rewind                      # -- Begin function f77_rewind
	.p2align	2
	.type	f77_rewind,@function
f77_rewind:                             # @f77_rewind
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	add r5, r3, r0
	addi r1, r0, 31
	slli r11, r3, 2
	lui r12, %hi(fio_ufile)
	addi r12, r12, %lo(fio_ufile)
	bgtu r3, r1, .LBB19_2
.LBB19_1:
	add r1, r11, r12
	ldw r1, r1+0
	addi r3, r0, 0
	bne r1, r3, .LBB19_3
.LBB19_2:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB19_3:
	add r1, r11, r12
	ldw r3, r1+0
	addi r4, r0, 0
	add r5, r4, r0
	jal r31, fseek
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end19:
	.size	f77_rewind, .Lfunc_end19-f77_rewind
                                        # -- End function
	.p2align	2                               # -- Begin function fio_next_record
	.type	fio_next_record,@function
fio_next_record:                        # @fio_next_record
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	lui r1, %hi(fio_in)
	addi r1, r1, %lo(fio_in)
	ldw r1, r1+0
	addi r11, r0, 0
	seq r3, r1, r11
	lui r4, %hi(stdin)
	addi r4, r4, %lo(stdin)
	ldw r4, r4+0
	xor r4, r4, r1
	sub r3, r0, r3
	and r3, r4, r3
	xor r5, r1, r3
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	addi r4, r0, 1024
	jal r31, fgets
	beq r1, r11, .LBB20_7
.LBB20_1:
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	jal r31, strlen
	addi r3, r0, 1
	blt r1, r3, .LBB20_9
.LBB20_2:
	addi r1, r1, 1
	lui r4, %hi(fio_rec-2)
	addi r4, r4, %lo(fio_rec-2)
	addi r5, r0, 13
	addi r6, r0, 10
	jal r0, .LBB20_4
.LBB20_3:
	addi r1, r1, -1
	ble r1, r3, .LBB20_8
.LBB20_4:
	add r7, r1, r4
	ldbu r7, r7+0
	beq r7, r5, .LBB20_3
.LBB20_5:
	beq r7, r6, .LBB20_3
.LBB20_6:
	addi r1, r1, -1
	jal r0, .LBB20_9
.LBB20_7:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB20_10
.LBB20_8:
	add r1, r11, r0
.LBB20_9:
	lui r3, %hi(fio_rlen)
	addi r3, r3, %lo(fio_rlen)
	stw r3+0, r1
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	stw r1+0, r11
.LBB20_10:
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end20:
	.size	fio_next_record, .Lfunc_end20-fio_next_record
                                        # -- End function
	.type	fio_out,@object                 # @fio_out
	.local	fio_out
	.comm	fio_out,4,4
	.type	fio_reading,@object             # @fio_reading
	.local	fio_reading
	.comm	fio_reading,1,4
	.type	fio_len,@object                 # @fio_len
	.local	fio_len
	.comm	fio_len,4,4
	.type	fio_gdepth,@object              # @fio_gdepth
	.local	fio_gdepth
	.comm	fio_gdepth,4,4
	.type	fio_scale,@object               # @fio_scale
	.local	fio_scale
	.comm	fio_scale,4,4
	.type	fio_rep,@object                 # @fio_rep
	.local	fio_rep
	.comm	fio_rep,4,4
	.type	fio_desc,@object                # @fio_desc
	.local	fio_desc
	.comm	fio_desc,4,4
	.type	fio_listed,@object              # @fio_listed
	.local	fio_listed
	.comm	fio_listed,4,4
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.zero	1
	.size	.L.str, 1

	.type	fio_fmt,@object                 # @fio_fmt
	.local	fio_fmt
	.comm	fio_fmt,4,4
	.type	fio_pos,@object                 # @fio_pos
	.local	fio_pos
	.comm	fio_pos,4,4
	.type	fio_revert,@object              # @fio_revert
	.local	fio_revert
	.comm	fio_revert,4,4
	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"%d"
	.size	.L.str.1, 3

	.type	fio_w,@object                   # @fio_w
	.local	fio_w
	.comm	fio_w,4,4
	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"%g"
	.size	.L.str.2, 3

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"%%.%df"
	.size	.L.str.3, 7

	.type	fio_d,@object                   # @fio_d
	.local	fio_d
	.comm	fio_d,4,4
	.type	fio_in,@object                  # @fio_in
	.local	fio_in
	.comm	fio_in,4,4
	.type	fio_ldone,@object               # @fio_ldone
	.local	fio_ldone
	.comm	fio_ldone,1,4
	.type	fio_l_pending,@object           # @fio_l_pending
	.local	fio_l_pending
	.comm	fio_l_pending,4,4
	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"f77: end of file on READ\n"
	.size	.L.str.4, 26

	.type	fio_ltok,@object                # @fio_ltok
	.local	fio_ltok
	.comm	fio_ltok,128,4
	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"f77: A editing on READ needs CHARACTER (not implemented)\n"
	.size	.L.str.5, 58

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"f77: %c editing does not match a numeric READ item\n"
	.size	.L.str.6, 52

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"f77: cannot OPEN unit %d\n"
	.size	.L.str.7, 26

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"f77: bad FILE= name in OPEN\n"
	.size	.L.str.8, 29

	.type	fio_ufile,@object               # @fio_ufile
	.local	fio_ufile
	.comm	fio_ufile,128,4
	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"OLD"
	.size	.L.str.9, 4

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"r+"
	.size	.L.str.10, 3

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"r"
	.size	.L.str.11, 2

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"f77: OPEN STATUS='OLD': no such file '%s'\n"
	.size	.L.str.12, 43

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"NEW"
	.size	.L.str.13, 4

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"f77: OPEN STATUS='NEW': '%s' already exists\n"
	.size	.L.str.14, 45

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"w+"
	.size	.L.str.15, 3

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"f77: OPEN cannot create '%s'\n"
	.size	.L.str.16, 30

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"UNKNOWN"
	.size	.L.str.17, 8

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"f77: OPEN cannot open '%s'\n"
	.size	.L.str.18, 28

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"f77: OPEN STATUS value is not supported\n"
	.size	.L.str.19, 41

	.type	fio_ufname,@object              # @fio_ufname
	.local	fio_ufname
	.comm	fio_ufname,8192,1
	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"DELETE"
	.size	.L.str.20, 7

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"KEEP"
	.size	.L.str.21, 5

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"f77: CLOSE STATUS value is not supported\n"
	.size	.L.str.22, 42

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"f77: REWIND: unit %d is not open\n"
	.size	.L.str.23, 34

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"f77: unit %d is not open for %s\n"
	.size	.L.str.24, 33

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"WRITE"
	.size	.L.str.25, 6

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"READ"
	.size	.L.str.26, 5

	.type	fio_line,@object                # @fio_line
	.local	fio_line
	.comm	fio_line,1024,1
	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"%%.%dE"
	.size	.L.str.27, 7

	.type	fio_gcount,@object              # @fio_gcount
	.local	fio_gcount
	.comm	fio_gcount,64,4
	.type	fio_gstart,@object              # @fio_gstart
	.local	fio_gstart
	.comm	fio_gstart,64,4
	.type	fio_rpos,@object                # @fio_rpos
	.local	fio_rpos
	.comm	fio_rpos,4,4
	.type	fio_rec,@object                 # @fio_rec
	.local	fio_rec
	.comm	fio_rec,1024,1
	.type	fio_rlen,@object                # @fio_rlen
	.local	fio_rlen
	.comm	fio_rlen,4,4
	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"f77: null value in list-directed input is not supported\n"
	.size	.L.str.28, 57

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"f77: bad LOGICAL input field '%s'\n"
	.size	.L.str.29, 35

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"E%d"
	.size	.L.str.30, 4

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e507704cf3c4d36284ffcb21f50e8531ceb63f7f)"
	.section	".note.GNU-stack","",@progbits
