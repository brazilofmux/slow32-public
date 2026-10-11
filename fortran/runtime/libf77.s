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
	stw sp+20, r13
	stw sp+16, r14
	add r11, r4, r0
	add r12, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	addi r14, r0, 0
	bne r3, r14, .LBB0_4
.LBB0_1:
	addi r3, r0, -128
	lui r4, %hi(fio_uend+128)
	addi r4, r4, %lo(fio_uend+128)
	addi r5, r0, -1
.LBB0_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r14, .LBB0_2
.LBB0_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB0_4:
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r14
	addi r13, r0, 1
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fio_resolve
	lui r3, %hi(fio_out)
	addi r3, r3, %lo(fio_out)
	stw r3+0, r1
	lui r1, %hi(fio_unit)
	addi r1, r1, %lo(fio_unit)
	stw r1+0, r12
	lui r1, %hi(fio_reading)
	addi r1, r1, %lo(fio_reading)
	stb r1+0, r14
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	stw r1+0, r14
	lui r1, %hi(fio_gdepth)
	addi r1, r1, %lo(fio_gdepth)
	stw r1+0, r14
	lui r1, %hi(fio_scale)
	addi r1, r1, %lo(fio_scale)
	stw r1+0, r14
	lui r1, %hi(fio_rep)
	addi r1, r1, %lo(fio_rep)
	stw r1+0, r14
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	stw r1+0, r14
	lui r1, %hi(fio_first_item)
	addi r1, r1, %lo(fio_first_item)
	stb r1+0, r13
	lui r1, %hi(fio_prev_char)
	addi r1, r1, %lo(fio_prev_char)
	stb r1+0, r14
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	beq r11, r14, .LBB0_10
.LBB0_5:
	stw r1+0, r14
	lui r1, %hi(fio_fmt)
	addi r1, r1, %lo(fio_fmt)
	stw r1+0, r11
	lui r1, %hi(fio_pos)
	addi r1, r1, %lo(fio_pos)
	stw r1+0, r14
	lui r3, %hi(fio_revert)
	addi r3, r3, %lo(fio_revert)
	addi r4, r0, -1
	stw r3+0, r4
	addi r4, r0, 32
.LBB0_6:
	add r5, r11, r14
	ldbu r5, r5+0
	bne r5, r4, .LBB0_8
.LBB0_7:
	addi r14, r14, 1
	stw r1+0, r14
	jal r0, .LBB0_6
.LBB0_8:
	addi r4, r0, 40
	bne r5, r4, .LBB0_12
.LBB0_9:
	stw r3+0, r14
	addi r3, r14, 1
	jal r0, .LBB0_11
.LBB0_10:
	stw r1+0, r13
	lui r1, %hi(.L.str.31)
	addi r1, r1, %lo(.L.str.31)
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
.LBB0_11:
	stw r1+0, r3
.LBB0_12:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end0:
	.size	f77_wr_begin, .Lfunc_end0-f77_wr_begin
                                        # -- End function
	.p2align	2                               # -- Begin function fio_resolve
	.type	fio_resolve,@function
fio_resolve:                            # @fio_resolve
# %bb.0:
	addi sp, sp, -288
	stw sp+0, lr
	stw sp+284, r11
	stw sp+280, r12
	stw sp+276, r13
	stw sp+272, r14
	addi r1, r0, 31
	bgtu r3, r1, .LBB1_2
.LBB1_1:
	slli r1, r3, 2
	lui r5, %hi(fio_ufile)
	addi r5, r5, %lo(fio_ufile)
	add r1, r1, r5
	ldw r1, r1+0
	addi r5, r0, 0
	bne r1, r5, .LBB1_19
.LBB1_2:
	addi r1, r0, 0
	beq r4, r1, .LBB1_7
.LBB1_3:
	beq r3, r1, .LBB1_15
.LBB1_4:
	addi r5, r0, 5
	beq r3, r5, .LBB1_6
.LBB1_5:
	addi r5, r0, 6
	bne r3, r5, .LBB1_10
.LBB1_6:
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	jal r0, .LBB1_19
.LBB1_7:
	addi r5, r0, 5
	beq r3, r5, .LBB1_9
.LBB1_8:
	addi r5, r0, 0
	bne r3, r5, .LBB1_10
.LBB1_9:
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	ldw r1, r1+0
	jal r0, .LBB1_19
.LBB1_10:
	add r11, r3, r0
	addi r3, r3, -1
	addi r5, r0, 30
	bgtu r3, r5, .LBB1_17
.LBB1_11:
	add r13, r4, r0
	add r14, r1, r0
	lui r5, %hi(.L.str.27)
	addi r5, r5, %lo(.L.str.27)
	addi r12, sp, 16
	addi r4, r0, 256
	add r3, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	add r3, r12, r0
	jal r31, fopen
	addi r12, r0, 0
	bne r1, r12, .LBB1_13
.LBB1_12:
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	addi r3, sp, 16
	jal r31, fopen
.LBB1_13:
	beq r1, r12, .LBB1_16
.LBB1_14:
	slli r12, r11, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r3, r12, r3
	stw r3+0, r1
	slli r3, r11, 8
	lui r4, %hi(fio_ufname)
	addi r4, r4, %lo(fio_ufname)
	add r3, r3, r4
	addi r4, sp, 16
	add r11, r1, r0
	jal r31, strcpy
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r1, r12, r1
	addi r3, r0, -1
	stw r1+0, r3
	jal r0, .LBB1_18
.LBB1_15:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	jal r0, .LBB1_19
.LBB1_16:
	add r1, r14, r0
	add r4, r13, r0
.LBB1_17:
	seq r4, r4, r1
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r5, %hi(.L.str.29)
	addi r5, r5, %lo(.L.str.29)
	lui r6, %hi(.L.str.30)
	addi r6, r6, %lo(.L.str.30)
	xor r6, r6, r5
	sub r4, r0, r4
	and r4, r6, r4
	xor r6, r4, r5
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	add r5, r11, r0
	add r11, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB1_18:
	add r1, r11, r0
.LBB1_19:
	ldw r14, sp+272
	ldw r13, sp+276
	ldw r12, sp+280
	ldw r11, sp+284
	ldw lr, sp+0
	addi sp, sp, 288
	jalr r0, r31, 0
.Lfunc_end1:
	.size	fio_resolve, .Lfunc_end1-fio_resolve
                                        # -- End function
	.globl	f77_wr_begin_int                # -- Begin function f77_wr_begin_int
	.p2align	2
	.type	f77_wr_begin_int,@function
f77_wr_begin_int:                       # @f77_wr_begin_int
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	lui r7, %hi(fio_uend_init)
	addi r7, r7, %lo(fio_uend_init)
	ldbu r8, r7+0
	addi r1, r0, 0
	bne r8, r1, .LBB2_4
.LBB2_1:
	addi r8, r0, -128
	lui r9, %hi(fio_uend+128)
	addi r9, r9, %lo(fio_uend+128)
	addi r10, r0, -1
.LBB2_2:
	add lr, r8, r9
	stw lr+0, r10
	addi r8, r8, 4
	bne r8, r1, .LBB2_2
.LBB2_3:
	addi r8, r0, 1
	stb r7+0, r8
.LBB2_4:
	lui r7, %hi(stdout)
	addi r7, r7, %lo(stdout)
	ldw r7, r7+0
	lui r8, %hi(fio_out)
	addi r8, r8, %lo(fio_out)
	stw r8+0, r7
	lui r8, %hi(fio_unit)
	addi r8, r8, %lo(fio_unit)
	addi r7, r0, -1
	stw r8+0, r7
	lui r8, %hi(fio_ibuf)
	addi r8, r8, %lo(fio_ibuf)
	stw r8+0, r3
	lui r3, %hi(fio_ilen)
	addi r3, r3, %lo(fio_ilen)
	stw r3+0, r4
	lui r3, %hi(fio_inrec)
	addi r3, r3, %lo(fio_inrec)
	stw r3+0, r5
	lui r3, %hi(fio_irec)
	addi r3, r3, %lo(fio_irec)
	stw r3+0, r1
	lui r3, %hi(fio_reading)
	addi r3, r3, %lo(fio_reading)
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
	lui r4, %hi(fio_first_item)
	addi r4, r4, %lo(fio_first_item)
	addi r3, r0, 1
	stb r4+0, r3
	lui r4, %hi(fio_prev_char)
	addi r4, r4, %lo(fio_prev_char)
	stb r4+0, r1
	beq r6, r1, .LBB2_10
.LBB2_5:
	lui r3, %hi(fio_listed)
	addi r3, r3, %lo(fio_listed)
	stw r3+0, r1
	lui r3, %hi(fio_fmt)
	addi r3, r3, %lo(fio_fmt)
	stw r3+0, r6
	lui r3, %hi(fio_pos)
	addi r3, r3, %lo(fio_pos)
	stw r3+0, r1
	lui r4, %hi(fio_revert)
	addi r4, r4, %lo(fio_revert)
	stw r4+0, r7
	addi r5, r0, 32
.LBB2_6:
	add r7, r6, r1
	ldbu r7, r7+0
	bne r7, r5, .LBB2_8
.LBB2_7:
	addi r1, r1, 1
	stw r3+0, r1
	jal r0, .LBB2_6
.LBB2_8:
	addi r5, r0, 40
	bne r7, r5, .LBB2_11
.LBB2_9:
	stw r4+0, r1
	addi r1, r1, 1
	stw r3+0, r1
	jal r0, .LBB2_11
.LBB2_10:
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	stw r1+0, r3
	lui r1, %hi(.L.str.31)
	addi r1, r1, %lo(.L.str.31)
	lui r3, %hi(fio_fmt)
	addi r3, r3, %lo(fio_fmt)
	stw r3+0, r1
	lui r1, %hi(fio_pos)
	addi r1, r1, %lo(fio_pos)
	addi r3, r0, 0
	stw r1+0, r3
	lui r1, %hi(fio_revert)
	addi r1, r1, %lo(fio_revert)
	stw r1+0, r7
.LBB2_11:
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end2:
	.size	f77_wr_begin_int, .Lfunc_end2-f77_wr_begin_int
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
	beq r1, r13, .LBB3_8
.LBB3_1:
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	addi r3, sp, 28
	addi r4, r0, 32
	add r6, r11, r0
	jal r31, snprintf
	ldbu r4, sp+28
	beq r4, r13, .LBB3_6
.LBB3_2:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r5, r1+0
	addi r3, sp, 28
	addi r3, r3, 1
	addi r6, r0, 1022
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	jal r0, .LBB3_4
.LBB3_3:
	ldbu r4, r3+0
	addi r3, r3, 1
	beq r4, r13, .LBB3_6
.LBB3_4:
	bgt r5, r6, .LBB3_3
.LBB3_5:
	addi r8, r5, 1
	stw r1+0, r8
	add r5, r5, r7
	stb r5+0, r4
	add r5, r8, r0
	jal r0, .LBB3_3
.LBB3_6:
	lui r1, %hi(fio_first_item)
	addi r1, r1, %lo(fio_first_item)
	stb r1+0, r13
	lui r1, %hi(fio_prev_char)
	addi r1, r1, %lo(fio_prev_char)
	stb r1+0, r13
.LBB3_7:
	ldw r15, sp+60
	ldw r14, sp+64
	ldw r13, sp+68
	ldw r12, sp+72
	ldw r11, sp+76
	ldw lr, sp+0
	addi sp, sp, 80
	jalr r0, r31, 0
.LBB3_8:
	lui r14, %hi(fio_desc)
	addi r14, r14, %lo(fio_desc)
	ldw r1, r14+0
	lui r15, %hi(fio_rep)
	addi r15, r15, %lo(fio_rep)
	beq r1, r13, .LBB3_10
.LBB3_9:
	ldw r3, r15+0
	bgt r3, r13, .LBB3_11
.LBB3_10:
	stw r14+0, r13
	jal r31, fio_next_desc
.LBB3_11:
	beq r1, r13, .LBB3_7
.LBB3_12:
	addi r3, r0, 76
	bne r1, r3, .LBB3_20
.LBB3_13:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	addi r1, r0, 2
	blt r4, r1, .LBB3_18
.LBB3_14:
	lui r3, %hi(fio_len)
	addi r3, r3, %lo(fio_len)
	ldw r6, r3+0
	addi r4, r4, 1
	addi r5, r0, 1022
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
	jal r0, .LBB3_16
.LBB3_15:
	addi r4, r4, -1
	ble r4, r1, .LBB3_18
.LBB3_16:
	bgt r6, r5, .LBB3_15
.LBB3_17:
	addi r9, r6, 1
	stw r3+0, r9
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	jal r0, .LBB3_15
.LBB3_18:
	lui r3, %hi(fio_len)
	addi r3, r3, %lo(fio_len)
	ldw r1, r3+0
	addi r4, r0, 1022
	bgt r1, r4, .LBB3_21
.LBB3_19:
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
	jal r0, .LBB3_21
.LBB3_20:
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
.LBB3_21:
	ldw r1, r15+0
	addi r3, r0, 1
	blt r1, r3, .LBB3_23
.LBB3_22:
	addi r1, r1, -1
	stw r15+0, r1
.LBB3_23:
	ldw r1, r15+0
	bne r1, r13, .LBB3_7
.LBB3_24:
	stw r14+0, r13
	jal r0, .LBB3_7
.Lfunc_end3:
	.size	f77_wr_i, .Lfunc_end3-f77_wr_i
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
	ble r12, r3, .LBB4_6
.LBB4_1:
	bge r12, r1, .LBB4_11
.LBB4_2:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r5, r1+0
	addi r3, r12, 1
	addi r4, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 42
	addi r8, r0, 1
	jal r0, .LBB4_4
.LBB4_3:
	addi r3, r3, -1
	ble r3, r8, .LBB4_21
.LBB4_4:
	bgt r5, r4, .LBB4_3
.LBB4_5:
	addi r9, r5, 1
	stw r1+0, r9
	add r5, r5, r6
	stb r5+0, r7
	add r5, r9, r0
	jal r0, .LBB4_3
.LBB4_6:
	ldbu r6, r11+0
	beq r6, r3, .LBB4_21
.LBB4_7:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r7, r1+0
	addi r4, r11, 1
	addi r5, r0, 1022
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	jal r0, .LBB4_9
.LBB4_8:
	ldbu r6, r4+0
	addi r4, r4, 1
	beq r6, r3, .LBB4_21
.LBB4_9:
	bgt r7, r5, .LBB4_8
.LBB4_10:
	addi r9, r7, 1
	stw r1+0, r9
	add r7, r7, r8
	stb r7+0, r6
	add r7, r9, r0
	jal r0, .LBB4_8
.LBB4_11:
	sub r6, r12, r1
	addi r5, r0, 1
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	blt r6, r5, .LBB4_16
.LBB4_12:
	ldw r8, r1+0
	addi r6, r6, 1
	addi r7, r0, 1022
	addi r9, r0, 32
	jal r0, .LBB4_14
.LBB4_13:
	addi r6, r6, -1
	ble r6, r5, .LBB4_16
.LBB4_14:
	bgt r8, r7, .LBB4_13
.LBB4_15:
	addi r10, r8, 1
	stw r1+0, r10
	add r8, r8, r4
	stb r8+0, r9
	add r8, r10, r0
	jal r0, .LBB4_13
.LBB4_16:
	ldbu r8, r11+0
	beq r8, r3, .LBB4_21
.LBB4_17:
	ldw r7, r1+0
	addi r5, r11, 1
	addi r6, r0, 1022
	jal r0, .LBB4_19
.LBB4_18:
	ldbu r8, r5+0
	addi r5, r5, 1
	beq r8, r3, .LBB4_21
.LBB4_19:
	bgt r7, r6, .LBB4_18
.LBB4_20:
	addi r9, r7, 1
	stw r1+0, r9
	add r7, r7, r4
	stb r7+0, r8
	add r7, r9, r0
	jal r0, .LBB4_18
.LBB4_21:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end4:
	.size	fio_field, .Lfunc_end4-fio_field
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_wr_d
	.type	.LCPI5_0,@object
.LCPI5_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI5_0, 8
	.type	.LCPI5_1,@object
.LCPI5_1:
	.quad	0x4024000000000000              # double 10
	.size	.LCPI5_1, 8
	.type	.LCPI5_2,@object
.LCPI5_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI5_2, 8
	.type	.LCPI5_3,@object
.LCPI5_3:
	.quad	0x3fb999999999999a              # double 0.10000000000000001
	.size	.LCPI5_3, 8
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
	add r13, r4, r0
	add r12, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r18, r0, 0
	beq r1, r18, .LBB5_2
.LBB5_1:
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r13, r0
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
	jal r0, fio_list_out_real
.LBB5_2:
	lui r15, %hi(fio_desc)
	addi r15, r15, %lo(fio_desc)
	ldw r1, r15+0
	lui r16, %hi(fio_rep)
	addi r16, r16, %lo(fio_rep)
	beq r1, r18, .LBB5_18
.LBB5_3:
	ldw r3, r16+0
	addi r4, r0, 0
	ble r3, r4, .LBB5_18
.LBB5_4:
	addi r3, r0, 70
	bgt r1, r3, .LBB5_19
.LBB5_5:
	addi r17, r0, 0
	beq r1, r17, .LBB5_79
.LBB5_6:
	bne r1, r3, .LBB5_22
.LBB5_7:
	lui r1, %hi(fio_scale)
	addi r1, r1, %lo(fio_scale)
	ldw r1, r1+0
	addi r14, r0, 1
	blt r1, r14, .LBB5_11
.LBB5_8:
	addi r1, r1, 1
	lui r3, %hi(.LCPI5_1)
	addi r3, r3, %lo(.LCPI5_1)
	ldw r5, r3+4
	ldw r4, r3+0
.LBB5_9:
	fmul.d r12, r12, r4
	addi r1, r1, -1
	bgt r1, r14, .LBB5_9
.LBB5_10:
	add r1, r17, r0
.LBB5_11:
	addi r3, r0, -1
	bgt r1, r3, .LBB5_14
.LBB5_12:
	lui r3, %hi(.LCPI5_1)
	addi r3, r3, %lo(.LCPI5_1)
	ldw r5, r3+4
	ldw r4, r3+0
.LBB5_13:
	fdiv.d r12, r12, r4
	addi r3, r1, 1
	sltu r6, r3, r1
	add r1, r3, r0
	bne r6, r14, .LBB5_13
.LBB5_14:
	lui r18, %hi(fio_d)
	addi r18, r18, %lo(fio_d)
	ldw r6, r18+0
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	addi r11, sp, 28
	addi r4, r0, 16
	add r3, r11, r0
	jal r31, snprintf
	addi r3, sp, 44
	addi r4, r0, 64
	add r5, r11, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	ldw r1, r18+0
	bne r1, r17, .LBB5_16
.LBB5_15:
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	addi r3, sp, 44
	jal r31, strcat
.LBB5_16:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r11, r1+0
	addi r3, sp, 44
	jal r31, strlen
	blt r11, r14, .LBB5_75
.LBB5_17:
	addi r3, r11, 1
	beq r1, r3, .LBB5_25
	jal r0, .LBB5_75
.LBB5_18:
	stw r15+0, r18
	jal r31, fio_next_desc
	addi r3, r0, 70
	ble r1, r3, .LBB5_5
.LBB5_19:
	addi r3, r0, 71
	beq r1, r3, .LBB5_29
.LBB5_20:
	addi r3, r0, 73
	bne r1, r3, .LBB5_22
.LBB5_21:
	fcvt.w.d r6, r12
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r3, sp, 44
	addi r4, r0, 64
	jal r31, snprintf
	jal r0, .LBB5_75
.LBB5_22:
	addi r3, r0, -2
	and r3, r1, r3
	addi r4, r0, 68
	bne r3, r4, .LBB5_42
.LBB5_23:
	lui r3, %hi(fio_d)
	addi r3, r3, %lo(fio_d)
	ldw r4, r3+0
	addi r11, sp, 44
	add r3, r11, r0
	add r5, r12, r0
	add r6, r13, r0
	add r7, r1, r0
	jal r31, fio_efmt
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r12, r1+0
	add r3, r11, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB5_75
.LBB5_24:
	addi r3, r12, 1
	bne r1, r3, .LBB5_75
.LBB5_25:
	ldbu r3, sp+44
	addi r4, r0, 45
	beq r3, r4, .LBB5_46
.LBB5_26:
	addi r4, r0, 48
	bne r3, r4, .LBB5_75
.LBB5_27:
	addi r3, sp, 44
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB5_75
.LBB5_28:
	addi r3, sp, 44
	add r5, r1, r0
	jal r31, memmove
	jal r0, .LBB5_75
.LBB5_29:
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r1, r1+0
	addi r17, r0, 1
	sgt r3, r1, r17
	sub r3, r0, r3
	xori r4, r1, 1
	and r3, r4, r3
	xori r10, r3, 1
	lui r4, %hi(.LCPI5_0)
	addi r4, r4, %lo(.LCPI5_0)
	ldw r9, r4+4
	ldw r8, r4+0
	lui r4, %hi(.LCPI5_1)
	addi r4, r4, %lo(.LCPI5_1)
	ldw r7, r4+4
	ldw r6, r4+0
	add r4, r8, r0
	add r5, r9, r0
.LBB5_30:
	fmul.d r4, r4, r6
	addi r10, r10, -1
	bne r10, r18, .LBB5_30
.LBB5_31:
	lui r10, %hi(.LCPI5_2)
	addi r10, r10, %lo(.LCPI5_2)
	ldw r21, r10+4
	ldw r20, r10+0
	flt.d r10, r12, r20
	fneg.d r22, r12
	xor lr, r23, r13
	sub r14, r0, r10
	and r10, lr, r14
	xor r11, r13, r10
	xor lr, r22, r12
	and lr, lr, r14
	xor r10, r12, lr
	feq.d lr, r12, r20
	bne lr, r18, .LBB5_34
.LBB5_32:
	lui lr, %hi(.LCPI5_3)
	addi lr, lr, %lo(.LCPI5_3)
	ldw r21, lr+4
	ldw r20, lr+0
	fle.d lr, r20, r10
	xori lr, lr, 1
	bne lr, r18, .LBB5_35
.LBB5_33:
	flt.d r4, r10, r4
	xori r4, r4, 1
	bne r4, r18, .LBB5_35
.LBB5_34:
	addi r17, r0, 0
.LBB5_35:
	xori r4, r3, 1
	beq r17, r18, .LBB5_43
.LBB5_36:
	addi r11, sp, 44
	addi r7, r0, 69
	add r3, r11, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fio_efmt
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r12, r1+0
	add r3, r11, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB5_74
.LBB5_37:
	addi r3, r12, 1
	bne r1, r3, .LBB5_74
.LBB5_38:
	ldbu r3, sp+44
	addi r4, r0, 45
	beq r3, r4, .LBB5_61
.LBB5_39:
	addi r4, r0, 48
	bne r3, r4, .LBB5_74
.LBB5_40:
	addi r3, sp, 44
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB5_74
.LBB5_41:
	addi r3, sp, 44
	add r5, r1, r0
	jal r0, .LBB5_64
.LBB5_42:
	lui r5, %hi(.L.str.4)
	addi r5, r5, %lo(.L.str.4)
	addi r3, sp, 44
	addi r4, r0, 64
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	jal r0, .LBB5_75
.LBB5_43:
	lui r19, %hi(fio_w)
	addi r19, r19, %lo(fio_w)
	ldw r3, r19+0
	addi r5, r0, 3
	sgt r5, r3, r5
	addi lr, r3, -4
	xor lr, lr, r3
	sub r5, r0, r5
	and r5, lr, r5
	fle.d r8, r8, r10
	xori r9, r8, 1
	add r8, r18, r0
	bne r9, r18, .LBB5_53
.LBB5_44:
	fle.d r8, r6, r10
	xori r9, r8, 1
	addi r8, r0, 0
	beq r9, r8, .LBB5_49
.LBB5_45:
	addi r8, r0, -1
	jal r0, .LBB5_53
.LBB5_46:
	addi r3, sp, 44
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB5_75
.LBB5_47:
	addi r4, sp, 44
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB5_75
.LBB5_48:
	addi r5, r1, -1
	jal r31, memmove
	jal r0, .LBB5_75
.LBB5_49:
	addi r9, r0, 1
.LBB5_50:
	add lr, r9, r0
	fdiv.d r10, r10, r6
	fle.d r9, r6, r10
	xori r14, r9, 1
	addi r9, lr, 1
	bne r14, r8, .LBB5_52
.LBB5_51:
	bgt r1, lr, .LBB5_50
.LBB5_52:
	sub r8, r0, r9
.LBB5_53:
	xor r11, r3, r5
	add r20, r8, r4
	sgt r1, r20, r18
	sub r1, r0, r1
	and r6, r20, r1
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	addi r14, sp, 28
	addi r4, r0, 16
	add r3, r14, r0
	jal r31, snprintf
	addi r3, sp, 44
	addi r4, r0, 64
	add r5, r14, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	bgt r20, r18, .LBB5_55
.LBB5_54:
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	addi r3, sp, 44
	jal r31, strcat
.LBB5_55:
	addi r3, sp, 44
	jal r31, strlen
	addi r12, r0, 1
	blt r11, r12, .LBB5_69
.LBB5_56:
	addi r3, r11, 1
	bne r1, r3, .LBB5_69
.LBB5_57:
	ldbu r3, sp+44
	addi r4, r0, 45
	beq r3, r4, .LBB5_65
.LBB5_58:
	addi r4, r0, 48
	bne r3, r4, .LBB5_69
.LBB5_59:
	addi r3, sp, 44
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB5_69
.LBB5_60:
	addi r3, sp, 44
	add r5, r1, r0
	jal r0, .LBB5_68
.LBB5_61:
	addi r3, sp, 44
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB5_74
.LBB5_62:
	addi r4, sp, 44
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB5_74
.LBB5_63:
	addi r5, r1, -1
.LBB5_64:
	jal r31, memmove
	jal r0, .LBB5_74
.LBB5_65:
	addi r3, sp, 44
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB5_69
.LBB5_66:
	addi r4, sp, 44
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB5_69
.LBB5_67:
	addi r5, r1, -1
.LBB5_68:
	jal r31, memmove
.LBB5_69:
	addi r3, sp, 44
	add r4, r11, r0
	jal r31, fio_field
	ldw r1, r19+0
	addi r3, r0, 4
	bge r1, r3, .LBB5_80
.LBB5_70:
	ldw r1, r16+0
	blt r1, r12, .LBB5_72
.LBB5_71:
	addi r1, r1, -1
	stw r16+0, r1
.LBB5_72:
	ldw r3, r16+0
	addi r1, r0, 0
	bne r3, r1, .LBB5_74
.LBB5_73:
	stw r15+0, r1
.LBB5_74:
	addi r1, r0, 0
	beq r17, r1, .LBB5_79
.LBB5_75:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	addi r3, sp, 44
	jal r31, fio_field
	ldw r1, r16+0
	addi r3, r0, 1
	blt r1, r3, .LBB5_77
.LBB5_76:
	addi r1, r1, -1
	stw r16+0, r1
.LBB5_77:
	ldw r3, r16+0
	addi r1, r0, 0
	bne r3, r1, .LBB5_79
.LBB5_78:
	stw r15+0, r1
.LBB5_79:
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
.LBB5_80:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r5, r1+0
	addi r3, r0, 5
	addi r4, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	jal r0, .LBB5_82
.LBB5_81:
	addi r3, r3, -1
	bleu r3, r12, .LBB5_70
.LBB5_82:
	bgt r5, r4, .LBB5_81
.LBB5_83:
	addi r8, r5, 1
	stw r1+0, r8
	add r5, r5, r6
	stb r5+0, r7
	add r5, r8, r0
	jal r0, .LBB5_81
.Lfunc_end5:
	.size	f77_wr_d, .Lfunc_end5-f77_wr_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_list_out_real
	.type	.LCPI6_0,@object
.LCPI6_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI6_0, 8
	.type	.LCPI6_2,@object
.LCPI6_2:
	.quad	0x3fb999999999999a              # double 0.10000000000000001
	.size	.LCPI6_2, 8
	.type	.LCPI6_3,@object
.LCPI6_3:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI6_3, 8
	.type	.LCPI6_4,@object
.LCPI6_4:
	.quad	0x4024000000000000              # double 10
	.size	.LCPI6_4, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI6_1,@object
.LCPI6_1:
	.quad	0x41cdcd6500000000              # double 1.0E+9
	.quad	0x4376345785d8a000              # double 1.0E+17
	.size	.LCPI6_1, 16
	.text
	.p2align	2
	.type	fio_list_out_real,@function
fio_list_out_real:                      # @fio_list_out_real
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
	stw sp+100, r26
	stw sp+96, r27
	add r11, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	lui r1, %hi(.LCPI6_0)
	addi r1, r1, %lo(.LCPI6_0)
	ldw r21, r1+4
	ldw r20, r1+0
	flt.d r1, r12, r20
	fneg.d r4, r12
	xor r3, r5, r13
	sub r1, r0, r1
	and r3, r3, r1
	xor r23, r13, r3
	xor r3, r4, r12
	and r1, r3, r1
	xor r22, r12, r1
	addi r17, r0, 0
	sne r1, r11, r17
	sub r3, r0, r1
	andi r4, r3, 24
	lui r15, %hi(fio_len)
	addi r15, r15, %lo(fio_len)
	ldw r5, r15+0
	addi r16, r0, 1022
	lui r18, %hi(fio_line)
	addi r18, r18, %lo(fio_line)
	bgt r5, r16, .LBB6_2
.LBB6_1:
	addi r6, r5, 1
	stw r15+0, r6
	add r5, r5, r18
	addi r6, r0, 32
	stb r5+0, r6
.LBB6_2:
	xori r19, r4, 9
	feq.d r4, r12, r20
	bne r4, r17, .LBB6_5
.LBB6_3:
	lui r4, %hi(.LCPI6_2)
	addi r4, r4, %lo(.LCPI6_2)
	ldw r7, r4+4
	ldw r6, r4+0
	fle.d r4, r6, r22
	xori r4, r4, 1
	bne r4, r17, .LBB6_17
.LBB6_4:
	andi r4, r3, 8
	lui r5, %hi(.LCPI6_1)
	addi r5, r5, %lo(.LCPI6_1)
	add r4, r5, r4
	ldw r7, r4+4
	ldw r6, r4+0
	flt.d r4, r22, r6
	xori r4, r4, 1
	bne r4, r17, .LBB6_17
.LBB6_5:
	lui r3, %hi(.LCPI6_3)
	addi r3, r3, %lo(.LCPI6_3)
	ldw r27, r3+4
	ldw r26, r3+0
	fle.d r3, r26, r22
	xori r3, r3, 1
	beq r3, r17, .LBB6_7
.LBB6_6:
	addi r3, r0, 1
	jal r0, .LBB6_9
.LBB6_7:
	lui r3, %hi(.LCPI6_4)
	addi r3, r3, %lo(.LCPI6_4)
	ldw r5, r3+4
	ldw r4, r3+0
	add r6, r22, r0
	add r7, r23, r0
	add r3, r17, r0
.LBB6_8:
	fdiv.d r6, r6, r4
	addi r3, r3, 1
	fle.d r8, r26, r6
	bne r8, r17, .LBB6_8
.LBB6_9:
	addi r24, r1, 4
	flt.d r1, r22, r26
	sub r1, r0, r1
	and r1, r3, r1
	xor r1, r3, r1
	sub r6, r19, r1
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	addi r14, sp, 16
	addi r4, r0, 16
	add r3, r14, r0
	jal r31, snprintf
	addi r3, sp, 32
	addi r4, r0, 64
	add r5, r14, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	feq.d r1, r12, r20
	addi r14, r0, 0
	bne r1, r14, .LBB6_13
.LBB6_10:
	flt.d r1, r22, r26
	xori r1, r1, 1
	bne r1, r14, .LBB6_13
.LBB6_11:
	flt.d r12, r12, r20
	addi r3, sp, 32
	jal r31, strlen
	add r3, r12, r19
	addi r3, r3, 2
	bleu r1, r3, .LBB6_13
.LBB6_12:
	addi r1, sp, 32
	add r1, r1, r3
	stb r1+0, r14
.LBB6_13:
	sne r1, r11, r14
	sub r1, r0, r1
	andi r1, r1, 24
	xori r4, r1, 12
	addi r3, sp, 32
	jal r31, fio_field
	ldw r4, r15+0
	addi r1, r24, 1
	addi r3, r0, 32
	addi r5, r0, 1
	jal r0, .LBB6_15
.LBB6_14:
	addi r1, r1, -1
	bleu r1, r5, .LBB6_22
.LBB6_15:
	bgt r4, r16, .LBB6_14
.LBB6_16:
	addi r6, r4, 1
	stw r15+0, r6
	add r4, r4, r18
	stb r4+0, r3
	add r4, r6, r0
	jal r0, .LBB6_14
.LBB6_17:
	andi r1, r3, 9
	addi r14, r1, 16
	addi r6, r19, -1
	lui r5, %hi(.L.str.32)
	addi r5, r5, %lo(.L.str.32)
	addi r15, sp, 16
	addi r4, r0, 16
	add r3, r15, r0
	jal r31, snprintf
	addi r16, sp, 32
	addi r4, r0, 64
	add r3, r16, r0
	add r5, r15, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	addi r4, r0, 69
	add r3, r16, r0
	jal r31, strchr
	beq r11, r17, .LBB6_21
.LBB6_18:
	beq r1, r17, .LBB6_21
.LBB6_19:
	addi r11, r1, 2
	add r3, r11, r0
	jal r31, strlen
	addi r3, r0, 2
	bgt r1, r3, .LBB6_21
.LBB6_20:
	addi r3, r0, 3
	sub r12, r3, r1
	add r3, r11, r12
	addi r5, r1, 1
	add r4, r11, r0
	jal r31, memmove
	addi r4, r0, 48
	add r3, r11, r0
	add r5, r12, r0
	jal r31, memset
.LBB6_21:
	addi r3, sp, 32
	add r4, r14, r0
	jal r31, fio_field
.LBB6_22:
	lui r1, %hi(fio_first_item)
	addi r1, r1, %lo(fio_first_item)
	stb r1+0, r17
	lui r1, %hi(fio_prev_char)
	addi r1, r1, %lo(fio_prev_char)
	stb r1+0, r17
	ldw r27, sp+96
	ldw r26, sp+100
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
.Lfunc_end6:
	.size	fio_list_out_real, .Lfunc_end6-fio_list_out_real
                                        # -- End function
	.p2align	2                               # -- Begin function fio_efmt
	.type	fio_efmt,@function
fio_efmt:                               # @fio_efmt
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
	add r11, r7, r0
	add r13, r6, r0
	add r14, r5, r0
	add r12, r3, r0
	addi r17, r0, 1
	slt r1, r4, r17
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
	and r18, r3, r4
	addi r20, r0, 0
	sgt r3, r18, r20
	add r4, r18, r1
	xori r5, r4, 1
	sgt r4, r4, r17
	sub r4, r0, r4
	and r4, r5, r4
	xori r4, r4, 1
	addi r4, r4, -1
	xor r1, r1, r4
	sub r3, r0, r3
	and r1, r1, r3
	xor r6, r4, r1
	lui r5, %hi(.L.str.32)
	addi r5, r5, %lo(.L.str.32)
	addi r15, sp, 40
	addi r4, r0, 16
	add r3, r15, r0
	jal r31, snprintf
	addi r16, sp, 56
	addi r4, r0, 64
	add r3, r16, r0
	add r5, r15, r0
	add r6, r14, r0
	add r7, r13, r0
	jal r31, snprintf
	ldbu r19, sp+56
	addi r15, r0, 45
	seq r1, r19, r15
	addi r3, r0, 43
	seq r3, r19, r3
	or  r1, r1, r3
	or  r1, r1, r16
	addi r3, r1, 1
	addi r1, r0, 68
	addi r14, r0, 46
	addi r4, r0, 69
	addi r5, r0, 101
	add r13, r20, r0
	jal r0, .LBB7_3
.LBB7_1:
	addi r7, sp, 56
	add r7, r7, r13
	stb r7+40, r6
	addi r13, r13, 1
.LBB7_2:
	addi r3, r3, 1
.LBB7_3:
	ldbu r6, r3+-1
	bgt r6, r1, .LBB7_6
.LBB7_4:
	beq r6, r14, .LBB7_2
.LBB7_5:
	bne r6, r20, .LBB7_1
	jal r0, .LBB7_9
.LBB7_6:
	beq r6, r4, .LBB7_8
.LBB7_7:
	bne r6, r5, .LBB7_1
.LBB7_8:
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	addi r1, r1, 1
	addi r4, r0, 0
	bne r13, r4, .LBB7_10
	jal r0, .LBB7_12
.LBB7_9:
	add r1, r17, r0
	addi r4, r0, 0
	beq r13, r4, .LBB7_12
.LBB7_10:
	bne r13, r17, .LBB7_13
.LBB7_11:
	ldbu r3, sp+96
	andi r3, r3, 255
	addi r5, r0, 48
	bne r3, r5, .LBB7_13
.LBB7_12:
	addi r1, r0, 0
.LBB7_13:
	add r5, r12, r0
	bne r19, r15, .LBB7_15
.LBB7_14:
	addi r5, r12, 1
	stb r12+0, r15
.LBB7_15:
	blt r18, r17, .LBB7_20
.LBB7_16:
	blt r13, r17, .LBB7_25
.LBB7_17:
	xor r3, r18, r13
	slt r4, r18, r13
	sub r4, r0, r4
	and r3, r3, r4
	xor r3, r13, r3
	addi r4, sp, 56
	addi r6, r4, 40
	addi r4, r0, 0
.LBB7_18:
	add r7, r5, r4
	add r8, r6, r4
	ldbu r8, r8+0
	stb r7+0, r8
	addi r4, r4, 1
	bne r3, r4, .LBB7_18
.LBB7_19:
	add r5, r5, r4
	jal r0, .LBB7_26
.LBB7_20:
	addi r7, r0, 48
	stb r5+0, r7
	addi r8, r5, 2
	stb r5+1, r14
	addi r6, r0, 55
	beq r18, r4, .LBB7_31
.LBB7_21:
	sub r3, r8, r12
	bgt r3, r6, .LBB7_31
.LBB7_22:
	sub r3, r5, r12
	addi lr, r3, 3
	addi r5, r0, -2
	addi r9, r0, 56
	add r10, r18, r0
.LBB7_23:
	addi r3, r8, 1
	stb r8+0, r7
	bgt r10, r5, .LBB7_32
.LBB7_24:
	add r14, lr, r0
	addi r10, r10, 1
	addi lr, lr, 1
	add r8, r3, r0
	blt r14, r9, .LBB7_23
	jal r0, .LBB7_32
.LBB7_25:
	addi r4, r0, 0
.LBB7_26:
	stb r5+0, r14
	addi r3, r5, 1
	bge r4, r13, .LBB7_38
.LBB7_27:
	sub r6, r3, r12
	addi r7, r0, 55
	bgt r6, r7, .LBB7_38
.LBB7_28:
	sub r5, r5, r12
	addi r7, r5, 2
	addi r5, sp, 56
	addi r5, r5, 40
	addi r6, r0, 56
.LBB7_29:
	add r8, r7, r0
	add r7, r5, r4
	ldbu r7, r7+0
	stb r3+0, r7
	addi r4, r4, 1
	addi r3, r3, 1
	bge r4, r13, .LBB7_38
.LBB7_30:
	addi r7, r8, 1
	blt r8, r6, .LBB7_29
	jal r0, .LBB7_38
.LBB7_31:
	add r3, r8, r0
.LBB7_32:
	blt r13, r17, .LBB7_38
.LBB7_33:
	sub r7, r3, r12
	bgt r7, r6, .LBB7_38
.LBB7_34:
	addi r5, sp, 56
	addi r5, r5, 40
	addi r6, r7, 1
	addi r7, r0, 56
.LBB7_35:
	add r8, r3, r4
	add r9, r5, r4
	ldbu r9, r9+0
	stb r8+0, r9
	addi r8, r4, 1
	bge r8, r13, .LBB7_37
.LBB7_36:
	add r9, r6, r4
	add r4, r8, r0
	blt r9, r7, .LBB7_35
.LBB7_37:
	add r3, r3, r8
.LBB7_38:
	sub r8, r1, r18
	srai r1, r8, 31
	xor r4, r8, r1
	sub r6, r4, r1
	lui r1, %hi(fio_e)
	addi r1, r1, %lo(fio_e)
	ldw r5, r1+0
	addi r1, r0, 100
	sltu r7, r6, r1
	addi r1, r0, 0
	bne r5, r1, .LBB7_41
.LBB7_39:
	addi r4, r6, -100
	addi r9, r0, 900
	bgeu r4, r9, .LBB7_41
.LBB7_40:
	add r4, r3, r0
	jal r0, .LBB7_42
.LBB7_41:
	addi r4, r3, 1
	stb r3+0, r11
.LBB7_42:
	xori r7, r7, 3
	slt r3, r8, r1
	sub r3, r0, r3
	andi r3, r3, 6
	xori r3, r3, 43
	stb r4+0, r3
	lui r3, 838861
	addi r3, r3, -819
	addi r8, r0, 10
	addi r9, r0, 14
.LBB7_43:
	add r10, r1, r0
	mulhu r1, r6, r3
	srli lr, r1, 3
	mul r1, lr, r8
	sub r1, r6, r1
	ori  r11, r1, 48
	addi r1, r10, 1
	addi r12, sp, 24
	add r12, r12, r10
	stb r12+0, r11
	bltu r6, r8, .LBB7_45
.LBB7_44:
	add r6, lr, r0
	bltu r10, r9, .LBB7_43
.LBB7_45:
	addi r3, r0, 0
	sgt r6, r5, r3
	addi r8, r0, 15
	slt r8, r5, r8
	sub r8, r0, r8
	xori r5, r5, 15
	and r5, r5, r8
	xor r5, r5, r7
	sub r6, r0, r6
	xori r5, r5, 15
	and r5, r5, r6
	xor r5, r7, r5
	addi r6, sp, 24
	bge r1, r5, .LBB7_49
.LBB7_46:
	add r6, r6, r1
	addi r7, r0, 48
	add r8, r5, r0
.LBB7_47:
	stb r6+0, r7
	addi r6, r6, 1
	addi r8, r8, -1
	bne r1, r8, .LBB7_47
.LBB7_48:
	add r1, r5, r0
.LBB7_49:
	addi r4, r4, 1
	addi r5, sp, 24
	addi r5, r5, -1
.LBB7_50:
	add r6, r1, r0
	addi r1, r1, -1
	add r7, r5, r6
	ldbu r7, r7+0
	stb r4+0, r7
	addi r4, r4, 1
	bgt r6, r17, .LBB7_50
.LBB7_51:
	stb r4+0, r3
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
.Lfunc_end7:
	.size	fio_efmt, .Lfunc_end7-fio_efmt
                                        # -- End function
	.globl	f77_wr_r                        # -- Begin function f77_wr_r
	.p2align	2
	.type	f77_wr_r,@function
f77_wr_r:                               # @f77_wr_r
# %bb.0:
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	fcvt.d.s r6, r3
	addi r3, r0, 0
	beq r1, r3, .LBB8_2
.LBB8_1:
	addi r5, r0, 0
	add r3, r6, r0
	add r4, r7, r0
	jal r0, fio_list_out_real
.LBB8_2:
	add r3, r6, r0
	add r4, r7, r0
	jal r0, f77_wr_d
.Lfunc_end8:
	.size	f77_wr_r, .Lfunc_end8-f77_wr_r
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
	beq r1, r11, .LBB9_11
.LBB9_1:
	lui r1, %hi(fio_first_item)
	addi r1, r1, %lo(fio_first_item)
	ldbu r7, r1+0
	lui r5, %hi(fio_prev_char)
	addi r5, r5, %lo(fio_prev_char)
	lui r6, %hi(fio_len)
	addi r6, r6, %lo(fio_len)
	bne r7, r11, .LBB9_3
.LBB9_2:
	ldbu r7, r5+0
	andi r7, r7, 1
	bne r7, r11, .LBB9_5
.LBB9_3:
	ldw r7, r6+0
	addi r8, r0, 1022
	bgt r7, r8, .LBB9_5
.LBB9_4:
	addi r8, r7, 1
	stw r6+0, r8
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	add r7, r7, r8
	addi r8, r0, 32
	stb r7+0, r8
.LBB9_5:
	addi r7, r0, 1
	blt r4, r7, .LBB9_10
.LBB9_6:
	ldw r10, r6+0
	addi r8, r0, 1022
	lui r9, %hi(fio_line)
	addi r9, r9, %lo(fio_line)
	jal r0, .LBB9_8
.LBB9_7:
	addi r4, r4, -1
	addi r3, r3, 1
	beq r4, r11, .LBB9_10
.LBB9_8:
	bgt r10, r8, .LBB9_7
.LBB9_9:
	ldbu lr, r3+0
	addi r12, r10, 1
	stw r6+0, r12
	add r10, r10, r9
	stb r10+0, lr
	add r10, r12, r0
	jal r0, .LBB9_7
.LBB9_10:
	stb r1+0, r11
	stb r5+0, r7
	jal r0, .LBB9_32
.LBB9_11:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	beq r1, r11, .LBB9_13
.LBB9_12:
	ldw r1, r13+0
	bgt r1, r11, .LBB9_14
.LBB9_13:
	stw r12+0, r11
	add r14, r3, r0
	add r15, r4, r0
	jal r31, fio_next_desc
	add r4, r15, r0
	add r3, r14, r0
	beq r1, r11, .LBB9_32
.LBB9_14:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r5, r1+0
	addi r1, r0, 1
	blt r5, r1, .LBB9_16
.LBB9_15:
	blt r5, r4, .LBB9_23
.LBB9_16:
	ble r5, r4, .LBB9_22
.LBB9_17:
	sub r6, r5, r4
	blt r6, r1, .LBB9_22
.LBB9_18:
	lui r5, %hi(fio_len)
	addi r5, r5, %lo(fio_len)
	ldw r8, r5+0
	addi r6, r6, 1
	addi r7, r0, 1022
	lui r9, %hi(fio_line)
	addi r9, r9, %lo(fio_line)
	addi r10, r0, 32
	jal r0, .LBB9_20
.LBB9_19:
	addi r6, r6, -1
	ble r6, r1, .LBB9_22
.LBB9_20:
	bgt r8, r7, .LBB9_19
.LBB9_21:
	addi lr, r8, 1
	stw r5+0, lr
	add r8, r8, r9
	stb r8+0, r10
	add r8, lr, r0
	jal r0, .LBB9_19
.LBB9_22:
	add r5, r4, r0
.LBB9_23:
	blt r5, r1, .LBB9_28
.LBB9_24:
	lui r4, %hi(fio_len)
	addi r4, r4, %lo(fio_len)
	ldw r8, r4+0
	addi r6, r0, 1022
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	jal r0, .LBB9_26
.LBB9_25:
	addi r5, r5, -1
	addi r3, r3, 1
	beq r5, r11, .LBB9_28
.LBB9_26:
	bgt r8, r6, .LBB9_25
.LBB9_27:
	ldbu r9, r3+0
	addi r10, r8, 1
	stw r4+0, r10
	add r8, r8, r7
	stb r8+0, r9
	add r8, r10, r0
	jal r0, .LBB9_25
.LBB9_28:
	ldw r3, r13+0
	blt r3, r1, .LBB9_30
.LBB9_29:
	addi r1, r3, -1
	stw r13+0, r1
.LBB9_30:
	ldw r1, r13+0
	bne r1, r11, .LBB9_32
.LBB9_31:
	stw r12+0, r11
.LBB9_32:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end9:
	.size	f77_wr_a, .Lfunc_end9-f77_wr_a
                                        # -- End function
	.globl	f77_strcpy                      # -- Begin function f77_strcpy
	.p2align	2
	.type	f77_strcpy,@function
f77_strcpy:                             # @f77_strcpy
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r13, r6, r0
	add r11, r4, r0
	add r12, r3, r0
	xor r1, r6, r4
	slt r3, r6, r4
	sub r3, r0, r3
	and r1, r1, r3
	xor r14, r4, r1
	addi r1, r0, 1
	blt r14, r1, .LBB10_2
.LBB10_1:
	add r3, r12, r0
	add r4, r5, r0
	add r5, r14, r0
	jal r31, memmove
.LBB10_2:
	bge r13, r11, .LBB10_5
.LBB10_3:
	add r1, r12, r14
	sub r3, r11, r14
	addi r4, r0, 32
	addi r5, r0, 0
.LBB10_4:
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB10_4
.LBB10_5:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end10:
	.size	f77_strcpy, .Lfunc_end10-f77_strcpy
                                        # -- End function
	.globl	f77_strcmp                      # -- Begin function f77_strcmp
	.p2align	2
	.type	f77_strcmp,@function
f77_strcmp:                             # @f77_strcmp
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	xor r1, r4, r6
	sgt r7, r4, r6
	sub r7, r0, r7
	and r1, r1, r7
	xor r7, r6, r1
	addi r1, r0, 1
	blt r7, r1, .LBB11_9
.LBB11_1:
	addi r8, r0, 0
	addi r9, r0, 32
	add r10, r8, r0
                                        # implicit-def: $r1
.LBB11_2:
	add lr, r9, r0
	bge r10, r4, .LBB11_4
.LBB11_3:
	add lr, r3, r10
	ldbu lr, lr+0
.LBB11_4:
	add r11, r9, r0
	bge r10, r6, .LBB11_6
.LBB11_5:
	add r11, r5, r10
	ldbu r11, r11+0
.LBB11_6:
	seq r12, lr, r11
	sltu r13, lr, r11
	sub r13, r0, r13
	ori  r13, r13, 1
	xor r1, r1, r13
	sub r12, r0, r12
	and r1, r1, r12
	xor r1, r13, r1
	bne lr, r11, .LBB11_10
.LBB11_7:
	addi r10, r10, 1
	bne r7, r10, .LBB11_2
.LBB11_8:
	add r1, r8, r0
	jal r0, .LBB11_10
.LBB11_9:
	addi r1, r0, 0
.LBB11_10:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end11:
	.size	f77_strcmp, .Lfunc_end11-f77_strcmp
                                        # -- End function
	.globl	f77_cat                         # -- Begin function f77_cat
	.p2align	2
	.type	f77_cat,@function
f77_cat:                                # @f77_cat
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	stw sp+20, r17
	stw sp+16, r18
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	add r14, r3, r0
	addi r16, r0, 0
	sgt r1, r4, r16
	sub r1, r0, r1
	and r17, r4, r1
	sgt r1, r6, r16
	sub r1, r0, r1
	and r1, r6, r1
	add r15, r1, r17
	lui r1, 16
	addi r18, r1, 1
	bltu r15, r18, .LBB12_2
.LBB12_1:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	add r5, r15, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB12_2:
	lui r1, %hi(fio_catpos)
	addi r1, r1, %lo(fio_catpos)
	ldw r3, r1+0
	add r3, r3, r15
	blt r3, r18, .LBB12_4
.LBB12_3:
	stw r1+0, r16
.LBB12_4:
	ldw r3, r1+0
	lui r4, %hi(fio_catring)
	addi r4, r4, %lo(fio_catring)
	add r16, r3, r4
	addi r4, r15, 3
	lui r5, 524288
	addi r5, r5, -4
	and r4, r4, r5
	add r3, r3, r4
	stw r1+0, r3
	addi r15, r0, 1
	blt r13, r15, .LBB12_6
.LBB12_5:
	add r3, r16, r0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, memmove
.LBB12_6:
	blt r11, r15, .LBB12_8
.LBB12_7:
	add r3, r16, r17
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memmove
.LBB12_8:
	add r1, r16, r0
	ldw r18, sp+16
	ldw r17, sp+20
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end12:
	.size	f77_cat, .Lfunc_end12-f77_cat
                                        # -- End function
	.globl	f77_index                       # -- Begin function f77_index
	.p2align	2
	.type	f77_index,@function
f77_index:                              # @f77_index
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	add r12, r4, r0
	addi r11, r0, 0
	beq r6, r11, .LBB13_5
.LBB13_1:
	add r13, r6, r0
	bgt r6, r12, .LBB13_7
.LBB13_2:
	add r14, r5, r0
	add r15, r3, r0
	addi r11, r0, 0
	add r16, r11, r0
.LBB13_3:
	add r3, r15, r16
	add r4, r14, r0
	add r5, r13, r0
	jal r31, memcmp
	beq r1, r11, .LBB13_6
.LBB13_4:
	addi r16, r16, 1
	add r1, r13, r16
	ble r1, r12, .LBB13_3
	jal r0, .LBB13_7
.LBB13_5:
	addi r1, r0, 0
	sgt r11, r12, r1
	jal r0, .LBB13_7
.LBB13_6:
	addi r11, r16, 1
.LBB13_7:
	add r1, r11, r0
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end13:
	.size	f77_index, .Lfunc_end13-f77_index
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
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r11, r0, 0
	beq r1, r11, .LBB14_6
.LBB14_1:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r6, r1+0
	addi r5, r0, 1022
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	bgt r6, r5, .LBB14_3
.LBB14_2:
	addi r7, r6, 1
	stw r1+0, r7
	add r6, r6, r4
	addi r7, r0, 32
	stb r6+0, r7
.LBB14_3:
	ldw r6, r1+0
	bgt r6, r5, .LBB14_5
.LBB14_4:
	seq r3, r3, r11
	sub r3, r0, r3
	andi r3, r3, 18
	xori r3, r3, 84
	addi r5, r6, 1
	stw r1+0, r5
	add r1, r6, r4
	stb r1+0, r3
.LBB14_5:
	lui r1, %hi(fio_first_item)
	addi r1, r1, %lo(fio_first_item)
	stb r1+0, r11
	lui r1, %hi(fio_prev_char)
	addi r1, r1, %lo(fio_prev_char)
	stb r1+0, r11
	jal r0, .LBB14_20
.LBB14_6:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	beq r1, r11, .LBB14_8
.LBB14_7:
	ldw r1, r13+0
	bgt r1, r11, .LBB14_9
.LBB14_8:
	stw r12+0, r11
	add r14, r3, r0
	jal r31, fio_next_desc
	add r3, r14, r0
	beq r1, r11, .LBB14_20
.LBB14_9:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r5, r1+0
	addi r1, r0, 2
	blt r5, r1, .LBB14_14
.LBB14_10:
	lui r4, %hi(fio_len)
	addi r4, r4, %lo(fio_len)
	ldw r7, r4+0
	addi r5, r5, 1
	addi r6, r0, 1022
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	addi r9, r0, 32
	jal r0, .LBB14_12
.LBB14_11:
	addi r5, r5, -1
	ble r5, r1, .LBB14_14
.LBB14_12:
	bgt r7, r6, .LBB14_11
.LBB14_13:
	addi r10, r7, 1
	stw r4+0, r10
	add r7, r7, r8
	stb r7+0, r9
	add r7, r10, r0
	jal r0, .LBB14_11
.LBB14_14:
	lui r4, %hi(fio_len)
	addi r4, r4, %lo(fio_len)
	ldw r1, r4+0
	addi r5, r0, 1022
	bgt r1, r5, .LBB14_16
.LBB14_15:
	seq r3, r3, r11
	sub r3, r0, r3
	andi r3, r3, 18
	xori r3, r3, 84
	addi r5, r1, 1
	stw r4+0, r5
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	add r1, r1, r4
	stb r1+0, r3
.LBB14_16:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB14_18
.LBB14_17:
	addi r1, r1, -1
	stw r13+0, r1
.LBB14_18:
	ldw r1, r13+0
	bne r1, r11, .LBB14_20
.LBB14_19:
	stw r12+0, r11
.LBB14_20:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end14:
	.size	f77_wr_l, .Lfunc_end14-f77_wr_l
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
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r11, r0, 0
	bne r1, r11, .LBB15_2
.LBB15_1:
	lui r12, %hi(fio_draining)
	addi r12, r12, %lo(fio_draining)
	addi r1, r0, 1
	stb r12+0, r1
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	stw r1+0, r11
	lui r1, %hi(fio_rep)
	addi r1, r1, %lo(fio_rep)
	stw r1+0, r11
	jal r31, fio_next_desc
	stb r12+0, r11
.LBB15_2:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r11
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end15:
	.size	f77_wr_end, .Lfunc_end15-f77_wr_end
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
	addi r12, r0, 1
	lui r17, %hi(fio_fmt)
	addi r17, r17, %lo(fio_fmt)
	lui r14, %hi(fio_pos)
	addi r14, r14, %lo(fio_pos)
	addi r18, r0, 44
	addi r11, r0, 0
	lui r1, 24
	addi r19, r1, 1697
	addi r20, r0, 32
	lui r21, %hi(fio_gdepth)
	addi r21, r21, %lo(fio_gdepth)
	lui r22, %hi(fio_draining)
	addi r22, r22, %lo(fio_draining)
	lui r23, %hi(fio_revert)
	addi r23, r23, %lo(fio_revert)
	lui r24, %hi(fio_reading)
	addi r24, r24, %lo(fio_reading)
	lui r15, %hi(stderr)
	addi r15, r15, %lo(stderr)
	lui r1, %hi(.L.str.33)
	addi r1, r1, %lo(.L.str.33)
	stw sp+36, r1
	addi r13, r0, 2
	addi r26, r0, -33
	addi r27, r0, 66
	addi r1, r0, 11
	stw sp+48, r1
	addi r1, r0, 90
	stw sp+44, r1
	lui r1, %hi(fio_bz)
	addi r1, r1, %lo(fio_bz)
	stw sp+40, r1
	addi r1, r0, 8
	stw sp+52, r1
	addi r28, r0, -10
	addi r1, r0, 40
	stw sp+32, r1
	addi r1, r0, 15
	stw sp+28, r1
	lui r1, %hi(fio_gstart)
	addi r1, r1, %lo(fio_gstart)
	stw sp+24, r1
	add r16, r12, r0
	jal r0, .LBB16_3
.LBB16_1:
	jal r31, fio_flush_line
.LBB16_2:
	addi r16, r16, 1
	beq r16, r19, .LBB16_84
.LBB16_3:
	ldw fp, r17+0
	ldw r3, r14+0
	add r6, fp, r3
	ldb r5, r6+0
	andi r1, r5, 255
	beq r1, r18, .LBB16_14
.LBB16_4:
	beq r1, r20, .LBB16_14
.LBB16_5:
	bne r1, r11, .LBB16_16
.LBB16_6:
	ldw r1, r21+0
	blt r1, r12, .LBB16_8
.LBB16_7:
	stw r21+0, r11
.LBB16_8:
	ldbu r1, r22+0
	bne r1, r11, .LBB16_84
.LBB16_9:
	ldw r1, r23+0
	blt r1, r11, .LBB16_84
.LBB16_10:
	stw r14+0, r1
	add r1, fp, r1
	ldbu r1, r1+0
	beq r1, r11, .LBB16_84
.LBB16_11:
	ldbu r1, r24+0
	bne r1, r12, .LBB16_1
.LBB16_12:
	jal r31, fio_getrec
	beq r1, r11, .LBB16_2
.LBB16_13:
	ldw r3, r15+0
	ldw r4, sp+36
	jal r31, fprintf
	add r3, r13, r0
	jal r31, exit
	jal r0, .LBB16_2
.LBB16_14:
	addi r1, r3, 1
.LBB16_15:
	stw r14+0, r1
	jal r0, .LBB16_2
.LBB16_16:
	and r4, r5, r26
	bne r4, r27, .LBB16_20
.LBB16_17:
	ldbu r4, r6+1
	addi r7, r4, -78
	andi r8, r7, 252
	slli r7, r7, 6
	srli r8, r8, 2
	or  r7, r7, r8
	andi r7, r7, 255
	ldw r8, sp+48
	bgtu r7, r8, .LBB16_20
.LBB16_18:
	sll r7, r12, r7
	andi r7, r7, 2313
	beq r7, r11, .LBB16_20
.LBB16_19:
	andi r1, r4, 223
	ldw r4, sp+44
	seq r1, r1, r4
	ldw r4, sp+40
	stw r4+0, r1
	addi r1, r3, 2
	jal r0, .LBB16_15
.LBB16_20:
	addi r4, r1, -39
	lui r1, %hi(fio_line)
	addi r1, r1, %lo(fio_line)
	ldw r7, sp+52
	bgtu r4, r7, .LBB16_33
.LBB16_21:
	slli r4, r4, 2
	lui r7, %hi(.LJTI16_0)
	addi r7, r7, %lo(.LJTI16_0)
	add r4, r7, r4
	ldw r7, r4+0
	add r4, r12, r0
	jalr r0, r7, 0
.LBB16_22:
	addi r3, r3, 1
	stw r14+0, r3
	lui r4, %hi(fio_len)
	addi r4, r4, %lo(fio_len)
	ldw r6, r4+0
	ldbu r5, r24+0
	jal r0, .LBB16_25
.LBB16_23:
	add r8, r13, r0
.LBB16_24:
	add r3, r3, r8
	stw r14+0, r3
.LBB16_25:
	add r7, fp, r3
	ldbu r9, r7+0
	addi r7, r0, 39
	beq r9, r7, .LBB16_28
.LBB16_26:
	addi r10, r0, 0
	beq r9, r10, .LBB16_2
.LBB16_27:
	addi r7, r0, 1022
	sgt r7, r6, r7
	or  r7, r5, r7
	andi lr, r7, 1
	addi r8, r0, 1
	add r7, r9, r0
	bne lr, r10, .LBB16_24
	jal r0, .LBB16_30
.LBB16_28:
	addi r8, r3, 1
	add r9, fp, r8
	ldbu r9, r9+0
	bne r9, r7, .LBB16_60
.LBB16_29:
	addi r8, r0, 1022
	sgt r8, r6, r8
	or  r8, r5, r8
	andi r9, r8, 1
	addi r10, r0, 0
	add r8, r13, r0
	bne r9, r10, .LBB16_23
.LBB16_30:
	addi r9, r6, 1
	stw r4+0, r9
	add r6, r6, r1
	stb r6+0, r7
	add r6, r9, r0
	jal r0, .LBB16_24
.LBB16_31:
	ldb r4, r6+1
	addi r4, r4, -58
	bgeu r4, r28, .LBB16_40
.LBB16_32:
	addi r5, r0, 45
.LBB16_33:
	add r4, r12, r0
	jal r0, .LBB16_41
.LBB16_34:
	addi r1, r3, 1
	stw r14+0, r1
	ldw r1, r21+0
	addi r3, r0, 1
	blt r1, r3, .LBB16_2
.LBB16_35:
	addi r3, r1, -1
	stw r21+0, r3
	slli r3, r3, 2
	lui r4, %hi(fio_gcount)
	addi r4, r4, %lo(fio_gcount)
	add r4, r3, r4
	ldw r5, r4+0
	addi r6, r5, -1
	stw r4+0, r6
	blt r5, r13, .LBB16_2
.LBB16_36:
	ldw r4, sp+24
	add r3, r3, r4
	ldw r3, r3+0
	stw r14+0, r3
	stw r21+0, r1
	jal r0, .LBB16_2
.LBB16_37:
	addi r1, r3, 1
	stw r14+0, r1
	ldbu r1, r24+0
	addi r3, r0, 1
	bne r1, r3, .LBB16_1
.LBB16_38:
	jal r31, fio_getrec
	addi r3, r0, 0
	beq r1, r3, .LBB16_2
.LBB16_39:
	ldw r3, r15+0
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB16_2
.LBB16_40:
	addi r4, r3, 1
	stw r14+0, r4
	add r4, fp, r4
	ldb r5, r4+0
	addi r4, r0, 0
.LBB16_41:
	addi r5, r5, -58
	add r25, r12, r0
	bltu r5, r28, .LBB16_47
.LBB16_42:
	ldw r5, r14+0
	add r6, fp, r5
	ldb r6, r6+0
	addi r6, r6, -48
	addi r7, r0, 9
	bleu r6, r7, .LBB16_44
.LBB16_43:
	addi r25, r0, 0
	jal r0, .LBB16_47
.LBB16_44:
	addi r7, fp, 1
	addi r25, r0, 0
.LBB16_45:
	addi r9, r0, 10
	mul r10, r25, r9
	addi r8, r5, 1
	add r25, r10, r6
	add r5, r7, r5
	ldb r5, r5+0
	addi r6, r5, -48
	add r5, r8, r0
	bltu r6, r9, .LBB16_45
.LBB16_46:
	stw r14+0, r8
.LBB16_47:
	add r9, r15, r0
	add r15, r28, r0
	ldw r5, r14+0
	add r6, fp, r5
	ldb r28, r6+0
	andi r6, r28, 255
	ldw r7, sp+32
	bne r6, r7, .LBB16_51
.LBB16_48:
	addi r4, r5, 1
	stw r14+0, r4
	ldw r1, r21+0
	add r28, r15, r0
	add r15, r9, r0
	ldw r5, sp+28
	bgt r1, r5, .LBB16_2
.LBB16_49:
	slli r5, r1, 2
	ldw r6, sp+24
	add r6, r5, r6
	stw r6+0, r4
	lui r4, %hi(fio_gcount)
	addi r4, r4, %lo(fio_gcount)
	add r4, r5, r4
	stw r4+0, r25
	addi r4, r1, 1
	stw r21+0, r4
	addi r4, r0, 0
	bne r1, r4, .LBB16_2
.LBB16_50:
	stw r23+0, r3
	jal r0, .LBB16_2
.LBB16_51:
	and r3, r28, r26
	addi r7, r0, 88
	beq r3, r7, .LBB16_62
.LBB16_52:
	andi r7, r28, 223
	addi r3, r0, 80
	beq r7, r3, .LBB16_61
.LBB16_53:
	addi r4, r0, 72
	addi r3, r5, 1
	bne r7, r4, .LBB16_64
.LBB16_54:
	addi r4, r0, 1
	blt r25, r4, .LBB16_74
.LBB16_55:
	lui r5, %hi(fio_len)
	addi r5, r5, %lo(fio_len)
	ldw r7, r5+0
	ldbu r6, r24+0
	addi r8, r25, 1
	add r28, r15, r0
	add r15, r9, r0
	jal r0, .LBB16_57
.LBB16_56:
	addi r3, r3, 1
	addi r8, r8, -1
	ble r8, r4, .LBB16_68
.LBB16_57:
	add r9, fp, r3
	ldbu r9, r9+0
	addi r10, r0, 0
	beq r9, r10, .LBB16_68
.LBB16_58:
	addi lr, r0, 1022
	sgt lr, r7, lr
	or  lr, r6, lr
	andi lr, lr, 1
	bne lr, r10, .LBB16_56
.LBB16_59:
	addi r10, r7, 1
	stw r5+0, r10
	add r7, r7, r1
	stb r7+0, r9
	add r7, r10, r0
	jal r0, .LBB16_56
.LBB16_60:
	stw r14+0, r8
	jal r0, .LBB16_2
.LBB16_61:
	addi r1, r5, 1
	stw r14+0, r1
	sub r1, r0, r25
	xor r3, r25, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	lui r3, %hi(fio_scale)
	addi r3, r3, %lo(fio_scale)
	stw r3+0, r1
	jal r0, .LBB16_67
.LBB16_62:
	addi r3, r5, 1
	stw r14+0, r3
	ldbu r4, r24+0
	addi r3, r0, 1
	add r28, r15, r0
	bne r4, r3, .LBB16_69
.LBB16_63:
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	ldw r3, r1+0
	add r3, r3, r25
	stw r1+0, r3
	add r15, r9, r0
	jal r0, .LBB16_2
.LBB16_64:
	addi r1, r6, -65
	addi r4, r0, 43
	bgtu r1, r4, .LBB16_66
.LBB16_65:
	slli r1, r1, 2
	lui r4, %hi(.LJTI16_1)
	addi r4, r4, %lo(.LJTI16_1)
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB16_66:
	stw r14+0, r3
.LBB16_67:
	add r28, r15, r0
	add r15, r9, r0
	jal r0, .LBB16_2
.LBB16_68:
	stw r14+0, r3
	jal r0, .LBB16_2
.LBB16_69:
	add r15, r9, r0
	blt r25, r3, .LBB16_2
.LBB16_70:
	lui r4, %hi(fio_len)
	addi r4, r4, %lo(fio_len)
	ldw r6, r4+0
	addi r5, r25, 1
	jal r0, .LBB16_72
.LBB16_71:
	addi r5, r5, -1
	ble r5, r3, .LBB16_2
.LBB16_72:
	addi r7, r0, 1022
	bgt r6, r7, .LBB16_71
.LBB16_73:
	addi r7, r6, 1
	stw r4+0, r7
	add r6, r6, r1
	stb r6+0, r20
	add r6, r7, r0
	jal r0, .LBB16_71
.LBB16_74:
	add r28, r15, r0
	add r15, r9, r0
	stw r14+0, r3
	jal r0, .LBB16_2
.LBB16_75:
	stw r14+0, r3
	add r1, fp, r3
	ldb r1, r1+0
	addi r3, r1, -58
	addi r11, r0, 0
	add r1, r11, r0
	bltu r3, r15, .LBB16_77
.LBB16_76:
	jal r31, fio_number
.LBB16_77:
	lui r3, %hi(fio_w)
	addi r3, r3, %lo(fio_w)
	stw r3+0, r1
	lui r12, %hi(fio_d)
	addi r12, r12, %lo(fio_d)
	stw r12+0, r11
	ldw r1, r14+0
	add r3, fp, r1
	ldbu r3, r3+0
	addi r4, r0, 46
	bne r3, r4, .LBB16_79
.LBB16_78:
	addi r1, r1, 1
	stw r14+0, r1
	jal r31, fio_number
	stw r12+0, r1
.LBB16_79:
	lui r11, %hi(fio_e)
	addi r11, r11, %lo(fio_e)
	addi r1, r0, 0
	stw r11+0, r1
	ldw r1, r14+0
	add r3, fp, r1
	ldbu r3, r3+0
	ori  r3, r3, 32
	addi r4, r0, 101
	bne r3, r4, .LBB16_83
.LBB16_80:
	addi r3, r1, 1
	stw r14+0, r3
	add r3, fp, r3
	ldb r3, r3+0
	addi r3, r3, -58
	bgeu r3, r15, .LBB16_82
.LBB16_81:
	stw r14+0, r1
	jal r0, .LBB16_83
.LBB16_82:
	jal r31, fio_number
	stw r11+0, r1
.LBB16_83:
	lui r1, %hi(fio_rep)
	addi r1, r1, %lo(fio_rep)
	stw r1+0, r25
	addi r1, r28, -97
	andi r1, r1, 255
	addi r3, r0, 26
	sltu r1, r1, r3
	addi r3, r28, -32
	xor r3, r3, r28
	sub r1, r0, r1
	and r1, r3, r1
	xor r11, r28, r1
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	stw r1+0, r11
.LBB16_84:
	add r1, r11, r0
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
.Lfunc_end16:
	.size	fio_next_desc, .Lfunc_end16-fio_next_desc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI16_0,@object
.LJTI16_0:
	.word	.LBB16_22
	.word	.LBB16_41
	.word	.LBB16_34
	.word	.LBB16_41
	.word	.LBB16_41
	.word	.LBB16_41
	.word	.LBB16_31
	.word	.LBB16_41
	.word	.LBB16_37
	.size	.LJTI16_0, 36
	.type	.LJTI16_1,@object
.LJTI16_1:
	.word	.LBB16_75
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_75
	.word	.LBB16_75
	.word	.LBB16_75
	.word	.LBB16_75
	.word	.LBB16_66
	.word	.LBB16_75
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_75
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_75
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_75
	.word	.LBB16_75
	.word	.LBB16_75
	.word	.LBB16_75
	.word	.LBB16_66
	.word	.LBB16_75
	.word	.LBB16_66
	.word	.LBB16_66
	.word	.LBB16_75
	.size	.LJTI16_1, 176
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function fio_flush_line
	.type	fio_flush_line,@function
fio_flush_line:                         # @fio_flush_line
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	lui r1, %hi(fio_out)
	addi r1, r1, %lo(fio_out)
	ldw r1, r1+0
	addi r12, r0, 0
	lui r3, %hi(stdout)
	addi r3, r3, %lo(stdout)
	ldw r3, r3+0
	lui r13, %hi(fio_len)
	addi r13, r13, %lo(fio_len)
	ldw r4, r13+0
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	add r4, r4, r5
	stb r4+0, r12
	lui r14, %hi(fio_ibuf)
	addi r14, r14, %lo(fio_ibuf)
	ldw r4, r14+0
	beq r4, r12, .LBB17_9
.LBB17_1:
	lui r11, %hi(fio_irec)
	addi r11, r11, %lo(fio_irec)
	ldw r1, r11+0
	lui r3, %hi(fio_inrec)
	addi r3, r3, %lo(fio_inrec)
	ldw r3, r3+0
	lui r16, %hi(stderr)
	addi r16, r16, %lo(stderr)
	blt r1, r3, .LBB17_3
.LBB17_2:
	ldw r3, r16+0
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB17_3:
	ldw r5, r13+0
	lui r15, %hi(fio_ilen)
	addi r15, r15, %lo(fio_ilen)
	ldw r6, r15+0
	ble r5, r6, .LBB17_5
.LBB17_4:
	ldw r3, r16+0
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB17_5:
	ldw r1, r14+0
	ldw r3, r11+0
	ldw r4, r15+0
	mul r3, r4, r3
	add r3, r1, r3
	ldw r5, r13+0
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	jal r31, memcpy
	ldw r3, r13+0
	ldw r1, r15+0
	ble r1, r3, .LBB17_8
.LBB17_6:
	ldw r4, r14+0
	ldw r5, r11+0
	mul r5, r5, r1
	sub r1, r1, r3
	add r3, r3, r5
	add r3, r4, r3
	addi r4, r0, 32
.LBB17_7:
	stb r3+0, r4
	addi r1, r1, -1
	addi r3, r3, 1
	bne r1, r12, .LBB17_7
.LBB17_8:
	ldw r1, r11+0
	addi r1, r1, 1
	stw r11+0, r1
	stw r13+0, r12
	jal r0, .LBB17_12
.LBB17_9:
	seq r4, r1, r12
	xor r3, r3, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r11, r1, r3
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	add r4, r11, r0
	jal r31, fputs
	addi r3, r0, 10
	add r4, r11, r0
	jal r31, fputc
	stw r13+0, r12
	lui r13, %hi(fio_unit)
	addi r13, r13, %lo(fio_unit)
	ldw r1, r13+0
	addi r3, r1, -1
	addi r4, r0, 30
	bgtu r3, r4, .LBB17_12
.LBB17_10:
	slli r1, r1, 2
	lui r14, %hi(fio_uend)
	addi r14, r14, %lo(fio_uend)
	add r1, r1, r14
	ldw r1, r1+0
	blt r1, r12, .LBB17_12
.LBB17_11:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r13+0
	slli r3, r3, 2
	add r3, r3, r14
	stw r3+0, r1
.LBB17_12:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end17:
	.size	fio_flush_line, .Lfunc_end17-fio_flush_line
                                        # -- End function
	.globl	f77_endfile                     # -- Begin function f77_endfile
	.p2align	2
	.type	f77_endfile,@function
f77_endfile:                            # @f77_endfile
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r4, r1+0
	addi r3, r0, 0
	bne r4, r3, .LBB18_4
.LBB18_1:
	addi r4, r0, -128
	lui r5, %hi(fio_uend+128)
	addi r5, r5, %lo(fio_uend+128)
	addi r6, r0, -1
.LBB18_2:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r3, .LBB18_2
.LBB18_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB18_4:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, fio_resolve
	add r12, r1, r0
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	beq r12, r1, .LBB18_8
.LBB18_5:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	beq r12, r1, .LBB18_8
.LBB18_6:
	add r3, r12, r0
	jal r31, fflush
	addi r1, r11, -1
	addi r3, r0, 30
	bgtu r1, r3, .LBB18_8
.LBB18_7:
	add r3, r12, r0
	jal r31, ftell
	slli r3, r11, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
.LBB18_8:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end18:
	.size	f77_endfile, .Lfunc_end18-f77_endfile
                                        # -- End function
	.globl	f77_pause                       # -- Begin function f77_pause
	.p2align	2
	.type	f77_pause,@function
f77_pause:                              # @f77_pause
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	add r11, r4, r0
	add r12, r3, r0
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r3, r1+0
	jal r31, fflush
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	add r5, r11, r0
	add r6, r12, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fprintf
.Lfunc_end19:
	.size	f77_pause, .Lfunc_end19-f77_pause
                                        # -- End function
	.globl	f77_rd_begin                    # -- Begin function f77_rd_begin
	.p2align	2
	.type	f77_rd_begin,@function
f77_rd_begin:                           # @f77_rd_begin
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	addi r4, r0, 0
	bne r3, r4, .LBB20_4
.LBB20_1:
	addi r3, r0, -128
	lui r5, %hi(fio_uend+128)
	addi r5, r5, %lo(fio_uend+128)
	addi r6, r0, -1
.LBB20_2:
	add r7, r3, r5
	stw r7+0, r6
	addi r3, r3, 4
	bne r3, r4, .LBB20_2
.LBB20_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB20_4:
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r4
	add r3, r13, r0
	jal r31, fio_resolve
	lui r3, %hi(fio_in)
	addi r3, r3, %lo(fio_in)
	stw r3+0, r1
	lui r1, %hi(fio_unit)
	addi r1, r1, %lo(fio_unit)
	stw r1+0, r13
	add r3, r12, r0
	add r4, r11, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fio_rd_setup
.Lfunc_end20:
	.size	f77_rd_begin, .Lfunc_end20-f77_rd_begin
                                        # -- End function
	.p2align	2                               # -- Begin function fio_rd_setup
	.type	fio_rd_setup,@function
fio_rd_setup:                           # @fio_rd_setup
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	add r11, r4, r0
	lui r1, %hi(fio_bz)
	addi r1, r1, %lo(fio_bz)
	addi r12, r0, 0
	stw r1+0, r12
	lui r13, %hi(fio_reading)
	addi r13, r13, %lo(fio_reading)
	addi r14, r0, 1
	stb r13+0, r14
	lui r1, %hi(fio_ldone)
	addi r1, r1, %lo(fio_ldone)
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
	seq r1, r3, r12
	lui r4, %hi(fio_listed)
	addi r4, r4, %lo(fio_listed)
	stw r4+0, r1
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	xor r4, r3, r4
	sub r1, r0, r1
	and r1, r4, r1
	xor r5, r3, r1
	lui r1, %hi(fio_fmt)
	addi r1, r1, %lo(fio_fmt)
	stw r1+0, r5
	lui r1, %hi(fio_pos)
	addi r1, r1, %lo(fio_pos)
	stw r1+0, r12
	lui r4, %hi(fio_revert)
	addi r4, r4, %lo(fio_revert)
	addi r6, r0, -1
	stw r4+0, r6
	beq r3, r12, .LBB21_6
.LBB21_1:
	ldw r3, r1+0
	addi r6, r0, 32
.LBB21_2:
	add r7, r5, r3
	ldbu r7, r7+0
	bne r7, r6, .LBB21_4
.LBB21_3:
	addi r3, r3, 1
	stw r1+0, r3
	jal r0, .LBB21_2
.LBB21_4:
	addi r5, r0, 40
	bne r7, r5, .LBB21_6
.LBB21_5:
	stw r4+0, r3
	addi r3, r3, 1
	stw r1+0, r3
.LBB21_6:
	jal r31, fio_getrec
	beq r1, r12, .LBB21_9
.LBB21_7:
	addi r15, r0, 0
	stb r13+0, r15
	add r12, r14, r0
	bne r11, r15, .LBB21_9
.LBB21_8:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r12, r15, r0
.LBB21_9:
	add r1, r12, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end21:
	.size	fio_rd_setup, .Lfunc_end21-fio_rd_setup
                                        # -- End function
	.globl	f77_rd_begin_int                # -- Begin function f77_rd_begin_int
	.p2align	2
	.type	f77_rd_begin_int,@function
f77_rd_begin_int:                       # @f77_rd_begin_int
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	lui r8, %hi(fio_uend_init)
	addi r8, r8, %lo(fio_uend_init)
	ldbu r9, r8+0
	addi r1, r0, 0
	bne r9, r1, .LBB22_4
.LBB22_1:
	addi r9, r0, -128
	lui r10, %hi(fio_uend+128)
	addi r10, r10, %lo(fio_uend+128)
	addi lr, r0, -1
.LBB22_2:
	add r11, r9, r10
	stw r11+0, lr
	addi r9, r9, 4
	bne r9, r1, .LBB22_2
.LBB22_3:
	addi r9, r0, 1
	stb r8+0, r9
.LBB22_4:
	lui r8, %hi(stdin)
	addi r8, r8, %lo(stdin)
	ldw r8, r8+0
	lui r9, %hi(fio_in)
	addi r9, r9, %lo(fio_in)
	stw r9+0, r8
	lui r8, %hi(fio_unit)
	addi r8, r8, %lo(fio_unit)
	addi r9, r0, -1
	stw r8+0, r9
	lui r8, %hi(fio_ibuf)
	addi r8, r8, %lo(fio_ibuf)
	stw r8+0, r3
	lui r3, %hi(fio_ilen)
	addi r3, r3, %lo(fio_ilen)
	stw r3+0, r4
	lui r3, %hi(fio_inrec)
	addi r3, r3, %lo(fio_inrec)
	stw r3+0, r5
	lui r3, %hi(fio_irec)
	addi r3, r3, %lo(fio_irec)
	stw r3+0, r1
	add r3, r6, r0
	add r4, r7, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fio_rd_setup
.Lfunc_end22:
	.size	f77_rd_begin_int, .Lfunc_end22-f77_rd_begin_int
                                        # -- End function
	.globl	f77_rd_i                        # -- Begin function f77_rd_i
	.p2align	2
	.type	f77_rd_i,@function
f77_rd_i:                               # @f77_rd_i
# %bb.0:
	addi sp, sp, -1104
	stw sp+0, lr
	stw sp+1100, r11
	stw sp+1096, r12
	stw sp+1092, r13
	stw sp+1088, r14
	stw sp+1084, r15
	stw sp+1080, r16
	stw sp+1076, r17
	stw sp+1072, r18
	stw sp+1068, r19
	stw sp+1064, r20
	stw sp+1060, r21
	stw sp+1056, r22
	stw sp+1052, r23
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r14, r0, 0
	beq r1, r14, .LBB23_5
.LBB23_1:
	lui r12, %hi(fio_ldone)
	addi r12, r12, %lo(fio_ldone)
	ldbu r1, r12+0
	bne r1, r14, .LBB23_59
.LBB23_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB23_59
.LBB23_3:
	addi r3, r0, 1
	bne r1, r3, .LBB23_27
.LBB23_4:
	stb r12+0, r3
	jal r0, .LBB23_59
.LBB23_5:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	beq r1, r14, .LBB23_11
.LBB23_6:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB23_11
.LBB23_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB23_12
.LBB23_8:
	addi r3, r0, 0
	beq r1, r3, .LBB23_59
.LBB23_9:
	addi r3, r0, 65
	bne r1, r3, .LBB23_30
.LBB23_10:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB23_55
.LBB23_11:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB23_8
.LBB23_12:
	addi r3, r0, 73
	beq r1, r3, .LBB23_35
.LBB23_13:
	addi r3, r0, 76
	bne r1, r3, .LBB23_30
.LBB23_14:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	addi r1, r0, 1
	lui r4, %hi(fio_rpos)
	addi r4, r4, %lo(fio_rpos)
	add r19, r14, r0
	blt r3, r1, .LBB23_31
.LBB23_15:
	ldw r5, r4+0
	lui r6, %hi(fio_rlen)
	addi r6, r6, %lo(fio_rlen)
	ldw r6, r6+0
	lui r7, %hi(fio_bz)
	addi r7, r7, %lo(fio_bz)
	ldw r7, r7+0
	addi r8, r0, 0
	addi r9, r0, 32
	lui r10, %hi(fio_rec)
	addi r10, r10, %lo(fio_rec)
	addi lr, r0, 48
	addi r15, r0, 1023
	addi r16, r0, 1
	addi r17, r0, 9
	addi r18, r0, 1022
	add r20, r3, r0
	add r21, r8, r0
	add r19, r8, r0
	jal r0, .LBB23_19
.LBB23_16:
	addi r21, r19, 1
	addi r23, sp, 28
	add r19, r23, r19
	stb r19+0, r22
	add r19, r21, r0
.LBB23_17:
	add r21, r16, r0
.LBB23_18:
	addi r20, r20, -1
	addi r5, r5, 1
	beq r20, r8, .LBB23_31
.LBB23_19:
	add r22, r9, r0
	bge r5, r6, .LBB23_21
.LBB23_20:
	add r22, r5, r10
	ldb r22, r22+0
.LBB23_21:
	beq r22, r9, .LBB23_23
.LBB23_22:
	bne r22, r17, .LBB23_26
.LBB23_23:
	beq r7, r8, .LBB23_18
.LBB23_24:
	beq r21, r8, .LBB23_18
.LBB23_25:
	add r22, lr, r0
	blt r19, r15, .LBB23_16
	jal r0, .LBB23_18
.LBB23_26:
	ble r19, r18, .LBB23_16
	jal r0, .LBB23_17
.LBB23_27:
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
	bgtu r4, r5, .LBB23_60
.LBB23_28:
	slli r4, r4, 2
	lui r5, %hi(.LJTI23_0)
	addi r5, r5, %lo(.LJTI23_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB23_29:
	andi r3, r3, 255
	addi r4, r0, 46
	seq r3, r3, r4
	add r1, r3, r1
	ldbu r1, r1+0
	andi r1, r1, 223
	addi r3, r0, 84
	seq r1, r1, r3
	stw r11+0, r1
	jal r0, .LBB23_59
.LBB23_30:
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
	jal r0, .LBB23_54
.LBB23_31:
	addi r5, sp, 28
	add r6, r5, r19
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
	ble r3, r4, .LBB23_50
.LBB23_32:
	addi r4, r0, 84
	beq r3, r4, .LBB23_54
.LBB23_33:
	addi r4, r0, 116
	beq r3, r4, .LBB23_54
.LBB23_34:
	addi r1, r0, 102
	beq r3, r1, .LBB23_53
	jal r0, .LBB23_52
.LBB23_35:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	addi r1, r0, 0
	addi r4, r0, 1
	add r17, r1, r0
	blt r3, r4, .LBB23_48
.LBB23_36:
	lui r5, %hi(fio_rpos)
	addi r5, r5, %lo(fio_rpos)
	ldw r5, r5+0
	lui r6, %hi(fio_rlen)
	addi r6, r6, %lo(fio_rlen)
	ldw r6, r6+0
	lui r7, %hi(fio_bz)
	addi r7, r7, %lo(fio_bz)
	ldw r7, r7+0
	addi r8, r0, 0
	addi r9, r0, 32
	lui r10, %hi(fio_rec)
	addi r10, r10, %lo(fio_rec)
	addi lr, r0, 48
	addi r14, r0, 1023
	addi r15, r0, 9
	addi r16, r0, 1022
	add r18, r3, r0
	add r19, r8, r0
	add r17, r8, r0
	jal r0, .LBB23_40
.LBB23_37:
	addi r19, r17, 1
	addi r21, sp, 28
	add r17, r21, r17
	stb r17+0, r20
	add r17, r19, r0
.LBB23_38:
	add r19, r4, r0
.LBB23_39:
	addi r18, r18, -1
	addi r5, r5, 1
	beq r18, r8, .LBB23_48
.LBB23_40:
	add r20, r9, r0
	bge r5, r6, .LBB23_42
.LBB23_41:
	add r20, r5, r10
	ldb r20, r20+0
.LBB23_42:
	beq r20, r9, .LBB23_44
.LBB23_43:
	bne r20, r15, .LBB23_47
.LBB23_44:
	beq r7, r8, .LBB23_39
.LBB23_45:
	beq r19, r8, .LBB23_39
.LBB23_46:
	add r20, lr, r0
	blt r17, r14, .LBB23_37
	jal r0, .LBB23_39
.LBB23_47:
	ble r17, r16, .LBB23_37
	jal r0, .LBB23_38
.LBB23_48:
	addi r4, sp, 28
	add r4, r4, r17
	stb r4+0, r1
	lui r4, %hi(fio_rpos)
	addi r4, r4, %lo(fio_rpos)
	ldw r5, r4+0
	add r3, r5, r3
	stw r4+0, r3
	ldbu r3, sp+28
	beq r3, r1, .LBB23_54
.LBB23_49:
	addi r3, sp, 28
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	jal r0, .LBB23_54
.LBB23_50:
	addi r1, r0, 0
	beq r3, r1, .LBB23_53
.LBB23_51:
	addi r1, r0, 70
	beq r3, r1, .LBB23_53
.LBB23_52:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB23_53:
	addi r1, r0, 0
.LBB23_54:
	stw r11+0, r1
.LBB23_55:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB23_57
.LBB23_56:
	addi r1, r1, -1
	stw r13+0, r1
.LBB23_57:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB23_59
.LBB23_58:
	stw r12+0, r1
.LBB23_59:
	ldw r23, sp+1052
	ldw r22, sp+1056
	ldw r21, sp+1060
	ldw r20, sp+1064
	ldw r19, sp+1068
	ldw r18, sp+1072
	ldw r17, sp+1076
	ldw r16, sp+1080
	ldw r15, sp+1084
	ldw r14, sp+1088
	ldw r13, sp+1092
	ldw r12, sp+1096
	ldw r11, sp+1100
	ldw lr, sp+0
	addi sp, sp, 1104
	jalr r0, r31, 0
.LBB23_60:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	stw r11+0, r1
	jal r0, .LBB23_59
.Lfunc_end23:
	.size	f77_rd_i, .Lfunc_end23-f77_rd_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI23_0,@object
.LJTI23_0:
	.word	.LBB23_29
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_29
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_29
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_29
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_60
	.word	.LBB23_29
	.size	.LJTI23_0, 144
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
	lui r14, %hi(fio_l_pending)
	addi r14, r14, %lo(fio_l_pending)
	ldw r1, r14+0
	addi r18, r0, 0
	ble r1, r18, .LBB24_2
.LBB24_1:
	addi r1, r1, -1
	stw r14+0, r1
	addi r11, r0, 0
	jal r0, .LBB24_28
.LBB24_2:
	addi r19, r0, 1
	lui r20, %hi(fio_rlen)
	addi r20, r20, %lo(fio_rlen)
	lui r13, %hi(fio_rpos)
	addi r13, r13, %lo(fio_rpos)
	lui r16, %hi(fio_rec)
	addi r16, r16, %lo(fio_rec)
	addi r17, r0, 38
	lui r21, %hi(.LJTI24_0)
	addi r21, r21, %lo(.LJTI24_0)
	addi r11, r0, 2
	lui r15, %hi(stderr)
	addi r15, r15, %lo(stderr)
	lui r12, %hi(.L.str.33)
	addi r12, r12, %lo(.L.str.33)
	jal r0, .LBB24_10
.LBB24_3:
	ldw r1, r20+0
	ldw r4, r13+0
	bge r4, r1, .LBB24_7
.LBB24_4:
	add r5, r4, r16
	ldbu r5, r5+0
	addi r5, r5, -9
	bgtu r5, r17, .LBB24_12
.LBB24_5:
	slli r5, r5, 2
	add r5, r21, r5
	ldw r5, r5+0
	jalr r0, r5, 0
.LBB24_6:
	addi r4, r4, 1
	stw r13+0, r4
	bne r1, r4, .LBB24_4
.LBB24_7:
	jal r31, fio_getrec
	add r3, r19, r0
	bne r1, r18, .LBB24_9
	jal r0, .LBB24_3
.LBB24_8:
	addi r1, r4, 1
	stw r13+0, r1
	andi r1, r3, 1
	add r3, r18, r0
	bne r1, r18, .LBB24_3
	jal r0, .LBB24_28
.LBB24_9:
	ldw r3, r15+0
	add r4, r12, r0
	jal r31, fprintf
	add r3, r11, r0
	jal r31, exit
.LBB24_10:
	add r3, r19, r0
	jal r0, .LBB24_3
.LBB24_11:
	addi r1, r4, 1
	stw r13+0, r1
	addi r11, r0, 1
	jal r0, .LBB24_28
.LBB24_12:
	ldw r4, r13+0
	addi r18, r0, 0
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	add r5, r18, r0
	bge r4, r1, .LBB24_19
.LBB24_13:
	addi r5, r0, 0
	addi r6, r0, 126
	lui r7, %hi(.LJTI24_1)
	addi r7, r7, %lo(.LJTI24_1)
	jal r0, .LBB24_15
.LBB24_14:
	addi r4, r4, 1
	stw r13+0, r4
	beq r1, r4, .LBB24_19
.LBB24_15:
	add r8, r4, r16
	ldbu r8, r8+0
	addi r9, r8, -9
	bgtu r9, r17, .LBB24_17
.LBB24_16:
	slli r9, r9, 2
	add r9, r7, r9
	ldw r9, r9+0
	jalr r0, r9, 0
.LBB24_17:
	bgt r5, r6, .LBB24_14
.LBB24_18:
	addi r9, r5, 1
	add r5, r5, r3
	stb r5+0, r8
	add r5, r9, r0
	jal r0, .LBB24_14
.LBB24_19:
	add r1, r5, r3
	stb r1+0, r18
	addi r4, r0, -11
.LBB24_20:
	add r1, r18, r3
	ldb r1, r1+0
	addi r5, r1, -58
	addi r18, r18, 1
	bgtu r5, r4, .LBB24_20
.LBB24_21:
	addi r11, r0, 0
	addi r3, r0, 1
	beq r18, r3, .LBB24_28
.LBB24_22:
	addi r3, r0, 42
	bne r1, r3, .LBB24_28
.LBB24_23:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	addi r11, r0, 0
	addi r5, r0, 10
	add r3, r12, r0
	add r4, r11, r0
	jal r31, strtol
	add r13, r1, r0
	bne r1, r11, .LBB24_25
.LBB24_24:
	ldw r3, r15+0
	lui r4, %hi(.L.str.36)
	addi r4, r4, %lo(.L.str.36)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB24_25:
	addi r1, r0, 2
	blt r13, r1, .LBB24_27
.LBB24_26:
	addi r1, r13, -1
	stw r14+0, r1
.LBB24_27:
	add r13, r18, r12
	add r3, r13, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r13, r0
	jal r31, memmove
.LBB24_28:
	add r1, r11, r0
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
.Lfunc_end24:
	.size	fio_list_tok, .Lfunc_end24-fio_list_tok
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI24_0,@object
.LJTI24_0:
	.word	.LBB24_6
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_6
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_8
	.word	.LBB24_12
	.word	.LBB24_12
	.word	.LBB24_11
	.size	.LJTI24_0, 156
	.type	.LJTI24_1,@object
.LJTI24_1:
	.word	.LBB24_19
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_19
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_19
	.word	.LBB24_17
	.word	.LBB24_17
	.word	.LBB24_19
	.size	.LJTI24_1, 156
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_in_real
	.type	.LCPI25_0,@object
.LCPI25_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI25_0, 8
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
	addi r8, r0, 0
	addi r5, r0, 1
	lui r6, %hi(fio_rpos)
	addi r6, r6, %lo(fio_rpos)
	add r18, r8, r0
	blt r3, r5, .LBB25_13
.LBB25_1:
	ldw r7, r6+0
	lui r9, %hi(fio_rlen)
	addi r9, r9, %lo(fio_rlen)
	ldw r9, r9+0
	lui r10, %hi(fio_bz)
	addi r10, r10, %lo(fio_bz)
	ldw r10, r10+0
	addi lr, r0, 0
	addi r11, r0, 32
	lui r12, %hi(fio_rec)
	addi r12, r12, %lo(fio_rec)
	addi r13, r0, 48
	addi r14, r0, 1023
	addi r15, r0, 1
	addi r16, r0, 9
	addi r17, r0, 1022
	add r19, r3, r0
	add r20, lr, r0
	add r18, lr, r0
	jal r0, .LBB25_5
.LBB25_2:
	addi r20, r18, 1
	stw sp+16, r1
	add r1, sp, r0
	addi r1, r1, 2047
	addi r22, r1, 49
	ldw r1, sp+16
	add r18, r22, r18
	stb r18+0, r21
	add r18, r20, r0
.LBB25_3:
	add r20, r15, r0
.LBB25_4:
	addi r19, r19, -1
	addi r7, r7, 1
	beq r19, lr, .LBB25_13
.LBB25_5:
	add r21, r11, r0
	bge r7, r9, .LBB25_7
.LBB25_6:
	add r21, r7, r12
	ldb r21, r21+0
.LBB25_7:
	beq r21, r11, .LBB25_9
.LBB25_8:
	bne r21, r16, .LBB25_12
.LBB25_9:
	beq r10, lr, .LBB25_4
.LBB25_10:
	beq r20, lr, .LBB25_4
.LBB25_11:
	add r21, r13, r0
	blt r18, r14, .LBB25_2
	jal r0, .LBB25_4
.LBB25_12:
	bgt r18, r17, .LBB25_3
	jal r0, .LBB25_2
.LBB25_13:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r7, r9, 49
	add r7, r7, r18
	stb r7+0, r8
	ldw r7, r6+0
	add r3, r7, r3
	stw r6+0, r3
	add r6, sp, r0
	addi r6, r6, 2047
	ldbu r3, r6+49
	lui r9, %hi(.LCPI25_0)
	addi r9, r9, %lo(.LCPI25_0)
	ldw r7, r9+4
	ldw r6, r9+0
	beq r3, r8, .LBB25_66
.LBB25_14:
	addi r11, r0, 43
	beq r3, r11, .LBB25_17
.LBB25_15:
	addi r8, r0, 45
	bne r3, r8, .LBB25_18
.LBB25_16:
	addi r8, r0, 0
	addi r10, r0, 1
	jal r0, .LBB25_20
.LBB25_17:
	add r10, r5, r0
	jal r0, .LBB25_19
.LBB25_18:
	addi r10, r0, 0
.LBB25_19:
	add r8, r5, r0
.LBB25_20:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r3, r9, 49
	or  r3, r3, r10
	ldbu r21, r3+0
	addi r9, r0, 0
	beq r21, r9, .LBB25_42
.LBB25_21:
	addi r13, r0, 0
	addi r14, r0, 1
	addi r15, r0, 46
	addi r16, r0, -10
	addi r17, r0, 70
	lui r18, %hi(.LJTI25_0)
	addi r18, r18, %lo(.LJTI25_0)
	addi r19, r0, -1
	add lr, r14, r0
	add r12, r13, r0
	add r20, r13, r0
	add r3, r13, r0
.LBB25_22:
	andi r5, r21, 255
	bne r5, r15, .LBB25_24
.LBB25_23:
	add r20, r14, r0
	jal r0, .LBB25_34
.LBB25_24:
	slli r22, r21, 24
	srai r22, r22, 24
	addi r22, r22, -58
	bgeu r22, r16, .LBB25_31
.LBB25_25:
	addi r5, r5, -43
	bgtu r5, r17, .LBB25_40
.LBB25_26:
	slli r5, r5, 2
	add r5, r18, r5
	ldw r22, r5+0
	add r5, r13, r0
	add r21, r14, r0
	jalr r0, r22, 0
.LBB25_27:
	addi r5, r10, 1
	add r22, sp, r0
	addi r22, r22, 2047
	addi r21, r22, 49
	add r21, r21, r5
	ldbu r21, r21+0
	beq r21, r11, .LBB25_38
.LBB25_28:
	addi r22, r0, 45
	bne r21, r22, .LBB25_39
.LBB25_29:
	add r10, r5, r0
.LBB25_30:
	add r5, r13, r0
	add lr, r19, r0
	jal r0, .LBB25_35
.LBB25_31:
	addi r5, r0, 1022
	bgt r3, r5, .LBB25_33
.LBB25_32:
	addi r5, r3, 1
	addi r22, sp, 1072
	add r3, r22, r3
	stb r3+0, r21
	add r3, r5, r0
.LBB25_33:
	addi r5, r0, 0
	sne r5, r20, r5
	add r12, r12, r5
.LBB25_34:
	add r5, r14, r0
.LBB25_35:
	add r21, r14, r0
.LBB25_36:
	beq r21, r13, .LBB25_41
.LBB25_37:
	addi r10, r10, 1
	add r22, sp, r0
	addi r22, r22, 2047
	addi r21, r22, 49
	add r21, r21, r10
	ldbu r21, r21+0
	sne r22, r21, r13
	and r22, r22, r5
	bne r22, r13, .LBB25_22
	jal r0, .LBB25_41
.LBB25_38:
	add r10, r5, r0
.LBB25_39:
	add r5, r13, r0
	jal r0, .LBB25_35
.LBB25_40:
	add r5, r14, r0
	add r21, r13, r0
	bne r21, r13, .LBB25_37
.LBB25_41:
	addi r11, r0, 0
	seq r11, r20, r11
	xor r4, r4, r12
	sub r11, r0, r11
	and r4, r4, r11
	xor r4, r12, r4
	add r11, r9, r0
	beq r5, r9, .LBB25_43
	jal r0, .LBB25_47
.LBB25_42:
	add r3, r9, r0
	add lr, r5, r0
	add r11, r9, r0
	bne r5, r9, .LBB25_47
.LBB25_43:
	add r12, sp, r0
	addi r12, r12, 2047
	addi r11, r12, 49
	add r11, r11, r10
	ldbu r13, r11+0
	addi r11, r0, 0
	beq r13, r11, .LBB25_47
.LBB25_44:
	add r12, sp, r0
	addi r12, r12, 2047
	addi r11, r12, 49
	add r10, r10, r11
	addi r10, r10, 1
	addi r11, r0, 0
	addi r12, r0, 9
	addi r14, r0, 10
	add r15, r11, r0
.LBB25_45:
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
	ldbu r13, r10+0
	addi r10, r10, 1
	bne r13, r11, .LBB25_45
.LBB25_46:
	mul r11, r15, lr
.LBB25_47:
	addi r10, sp, 1072
	add r10, r10, r3
	stb r10+0, r9
	beq r3, r9, .LBB25_66
.LBB25_48:
	sub r7, r3, r4
	lui r6, %hi(fio_scale)
	addi r6, r6, %lo(fio_scale)
	ldw r6, r6+0
	sub r5, r0, r5
	and r6, r6, r5
	addi r5, r0, 0
	addi r10, sp, 32
	bne r8, r5, .LBB25_50
.LBB25_49:
	addi r10, r1, 1
	addi r1, r0, 45
	stb sp+32, r1
	addi r5, r0, 1
.LBB25_50:
	sub r6, r11, r6
	addi r8, r0, 1
	blt r7, r8, .LBB25_55
.LBB25_51:
	blt r3, r8, .LBB25_61
.LBB25_52:
	xor r1, r7, r3
	slt r4, r7, r3
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r3, r1
	addi r4, sp, 32
	or  r7, r4, r5
	addi r4, r0, 0
.LBB25_53:
	addi r8, sp, 1072
	add r8, r8, r4
	ldbu r8, r8+0
	add r9, r7, r4
	stb r9+0, r8
	addi r4, r4, 1
	bne r1, r4, .LBB25_53
.LBB25_54:
	add r5, r5, r4
	jal r0, .LBB25_62
.LBB25_55:
	addi r1, sp, 32
	or  r1, r1, r5
	addi r9, r0, 48
	stb r1+0, r9
	addi r1, r5, 2
	addi r5, r0, 46
	stb r10+1, r5
	addi r5, r0, -1
	bgt r7, r5, .LBB25_58
.LBB25_56:
	sub r4, r4, r3
	addi r5, r0, 0
.LBB25_57:
	add r7, r1, r0
	addi r1, r1, 1
	addi r10, sp, 32
	add r7, r10, r7
	stb r7+0, r9
	addi r4, r4, -1
	bne r4, r5, .LBB25_57
.LBB25_58:
	blt r3, r8, .LBB25_65
.LBB25_59:
	addi r4, sp, 1072
	addi r5, r0, 0
.LBB25_60:
	add r7, r1, r0
	ldbu r8, r4+0
	addi r1, r1, 1
	addi r9, sp, 32
	add r7, r9, r7
	stb r7+0, r8
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r5, .LBB25_60
	jal r0, .LBB25_65
.LBB25_61:
	addi r4, r0, 0
.LBB25_62:
	addi r1, sp, 32
	add r1, r1, r5
	addi r7, r0, 46
	stb r1+0, r7
	addi r1, r5, 1
	ble r3, r4, .LBB25_65
.LBB25_63:
	sub r3, r3, r4
	addi r5, sp, 1072
	add r4, r5, r4
	addi r5, r0, 0
.LBB25_64:
	ldbu r7, r4+0
	addi r8, sp, 32
	add r8, r8, r1
	stb r8+0, r7
	addi r1, r1, 1
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r5, .LBB25_64
.LBB25_65:
	addi r11, sp, 32
	add r3, r11, r1
	addi r4, r0, 1040
	sub r4, r4, r1
	lui r5, %hi(.L.str.38)
	addi r5, r5, %lo(.L.str.38)
	jal r31, snprintf
	addi r4, r0, 0
	add r3, r11, r0
	jal r31, strtod
	add r6, r1, r0
	add r7, r2, r0
.LBB25_66:
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
.Lfunc_end25:
	.size	fio_in_real, .Lfunc_end25-fio_in_real
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI25_0,@object
.LJTI25_0:
	.word	.LBB25_36
	.word	.LBB25_40
	.word	.LBB25_30
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_27
	.word	.LBB25_27
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_27
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_27
	.word	.LBB25_27
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_40
	.word	.LBB25_27
	.size	.LJTI25_0, 284
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_rd_d
	.type	.LCPI26_0,@object
.LCPI26_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI26_0, 8
	.text
	.globl	f77_rd_d
	.p2align	2
	.type	f77_rd_d,@function
f77_rd_d:                               # @f77_rd_d
# %bb.0:
	addi sp, sp, -1104
	stw sp+0, lr
	stw sp+1100, r11
	stw sp+1096, r12
	stw sp+1092, r13
	stw sp+1088, r14
	stw sp+1084, r15
	stw sp+1080, r16
	stw sp+1076, r17
	stw sp+1072, r18
	stw sp+1068, r19
	stw sp+1064, r20
	stw sp+1060, r21
	stw sp+1056, r22
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r12, r0, 0
	beq r1, r12, .LBB26_5
.LBB26_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r12, .LBB26_40
.LBB26_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB26_40
.LBB26_3:
	addi r3, r0, 1
	bne r1, r3, .LBB26_27
.LBB26_4:
	stb r13+0, r3
	jal r0, .LBB26_40
.LBB26_5:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB26_10
.LBB26_6:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB26_10
.LBB26_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB26_11
.LBB26_8:
	addi r3, r0, 0
	beq r1, r3, .LBB26_40
.LBB26_9:
	addi r3, r0, 65
	beq r1, r3, .LBB26_26
	jal r0, .LBB26_31
.LBB26_10:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB26_8
.LBB26_11:
	addi r3, r0, 76
	beq r1, r3, .LBB26_26
.LBB26_12:
	addi r3, r0, 73
	bne r1, r3, .LBB26_31
.LBB26_13:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	addi r3, r3, %lo(fio_rpos)
	add r18, r14, r0
	blt r1, r4, .LBB26_32
.LBB26_14:
	ldw r5, r3+0
	lui r6, %hi(fio_rlen)
	addi r6, r6, %lo(fio_rlen)
	ldw r6, r6+0
	lui r7, %hi(fio_bz)
	addi r7, r7, %lo(fio_bz)
	ldw r7, r7+0
	addi r8, r0, 0
	addi r9, r0, 32
	lui r10, %hi(fio_rec)
	addi r10, r10, %lo(fio_rec)
	addi lr, r0, 48
	addi r15, r0, 1023
	addi r16, r0, 9
	addi r17, r0, 1022
	add r19, r1, r0
	add r20, r8, r0
	add r18, r8, r0
	jal r0, .LBB26_18
.LBB26_15:
	addi r20, r18, 1
	addi r22, sp, 32
	add r18, r22, r18
	stb r18+0, r21
	add r18, r20, r0
.LBB26_16:
	add r20, r4, r0
.LBB26_17:
	addi r19, r19, -1
	addi r5, r5, 1
	beq r19, r8, .LBB26_32
.LBB26_18:
	add r21, r9, r0
	bge r5, r6, .LBB26_20
.LBB26_19:
	add r21, r5, r10
	ldb r21, r21+0
.LBB26_20:
	beq r21, r9, .LBB26_22
.LBB26_21:
	bne r21, r16, .LBB26_25
.LBB26_22:
	beq r7, r8, .LBB26_17
.LBB26_23:
	beq r20, r8, .LBB26_17
.LBB26_24:
	add r21, lr, r0
	blt r18, r15, .LBB26_15
	jal r0, .LBB26_17
.LBB26_25:
	ble r18, r17, .LBB26_15
	jal r0, .LBB26_16
.LBB26_26:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB26_36
.LBB26_27:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	ldbu r6, r1+0
	add r7, r12, r0
	beq r6, r12, .LBB26_30
.LBB26_28:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r7, r1, r0
.LBB26_29:
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
	bne r6, r1, .LBB26_29
.LBB26_30:
	addi r3, sp, 32
	add r1, r3, r7
	stb r1+0, r12
	add r4, r12, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
	jal r0, .LBB26_40
.LBB26_31:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r4, r1+0
	jal r31, fio_in_real
	stw r11+4, r2
	stw r11+0, r1
	jal r0, .LBB26_36
.LBB26_32:
	addi r4, sp, 32
	add r4, r4, r18
	stb r4+0, r14
	ldw r4, r3+0
	add r1, r4, r1
	stw r3+0, r1
	ldbu r1, sp+32
	beq r1, r14, .LBB26_34
.LBB26_33:
	addi r3, sp, 32
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	fcvt.d.w r4, r1
	jal r0, .LBB26_35
.LBB26_34:
	lui r1, %hi(.LCPI26_0)
	addi r1, r1, %lo(.LCPI26_0)
	ldw r5, r1+4
	ldw r4, r1+0
.LBB26_35:
	stw r11+4, r5
	stw r11+0, r4
.LBB26_36:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB26_38
.LBB26_37:
	addi r1, r1, -1
	stw r13+0, r1
.LBB26_38:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB26_40
.LBB26_39:
	stw r12+0, r1
.LBB26_40:
	ldw r22, sp+1056
	ldw r21, sp+1060
	ldw r20, sp+1064
	ldw r19, sp+1068
	ldw r18, sp+1072
	ldw r17, sp+1076
	ldw r16, sp+1080
	ldw r15, sp+1084
	ldw r14, sp+1088
	ldw r13, sp+1092
	ldw r12, sp+1096
	ldw r11, sp+1100
	ldw lr, sp+0
	addi sp, sp, 1104
	jalr r0, r31, 0
.Lfunc_end26:
	.size	f77_rd_d, .Lfunc_end26-f77_rd_d
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rd_r
	.type	.LCPI27_0,@object
.LCPI27_0:
	.word	0x00000000                      # float 0
	.size	.LCPI27_0, 4
	.text
	.globl	f77_rd_r
	.p2align	2
	.type	f77_rd_r,@function
f77_rd_r:                               # @f77_rd_r
# %bb.0:
	addi sp, sp, -1104
	stw sp+0, lr
	stw sp+1100, r11
	stw sp+1096, r12
	stw sp+1092, r13
	stw sp+1088, r14
	stw sp+1084, r15
	stw sp+1080, r16
	stw sp+1076, r17
	stw sp+1072, r18
	stw sp+1068, r19
	stw sp+1064, r20
	stw sp+1060, r21
	stw sp+1056, r22
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r12, r0, 0
	beq r1, r12, .LBB27_5
.LBB27_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r12, .LBB27_40
.LBB27_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB27_40
.LBB27_3:
	addi r3, r0, 1
	bne r1, r3, .LBB27_27
.LBB27_4:
	stb r13+0, r3
	jal r0, .LBB27_40
.LBB27_5:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB27_10
.LBB27_6:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB27_10
.LBB27_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB27_11
.LBB27_8:
	addi r3, r0, 0
	beq r1, r3, .LBB27_40
.LBB27_9:
	addi r3, r0, 65
	beq r1, r3, .LBB27_26
	jal r0, .LBB27_31
.LBB27_10:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB27_8
.LBB27_11:
	addi r3, r0, 76
	beq r1, r3, .LBB27_26
.LBB27_12:
	addi r3, r0, 73
	bne r1, r3, .LBB27_31
.LBB27_13:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	addi r3, r3, %lo(fio_rpos)
	add r18, r14, r0
	blt r1, r4, .LBB27_32
.LBB27_14:
	ldw r5, r3+0
	lui r6, %hi(fio_rlen)
	addi r6, r6, %lo(fio_rlen)
	ldw r6, r6+0
	lui r7, %hi(fio_bz)
	addi r7, r7, %lo(fio_bz)
	ldw r7, r7+0
	addi r8, r0, 0
	addi r9, r0, 32
	lui r10, %hi(fio_rec)
	addi r10, r10, %lo(fio_rec)
	addi lr, r0, 48
	addi r15, r0, 1023
	addi r16, r0, 9
	addi r17, r0, 1022
	add r19, r1, r0
	add r20, r8, r0
	add r18, r8, r0
	jal r0, .LBB27_18
.LBB27_15:
	addi r20, r18, 1
	addi r22, sp, 32
	add r18, r22, r18
	stb r18+0, r21
	add r18, r20, r0
.LBB27_16:
	add r20, r4, r0
.LBB27_17:
	addi r19, r19, -1
	addi r5, r5, 1
	beq r19, r8, .LBB27_32
.LBB27_18:
	add r21, r9, r0
	bge r5, r6, .LBB27_20
.LBB27_19:
	add r21, r5, r10
	ldb r21, r21+0
.LBB27_20:
	beq r21, r9, .LBB27_22
.LBB27_21:
	bne r21, r16, .LBB27_25
.LBB27_22:
	beq r7, r8, .LBB27_17
.LBB27_23:
	beq r20, r8, .LBB27_17
.LBB27_24:
	add r21, lr, r0
	blt r18, r15, .LBB27_15
	jal r0, .LBB27_17
.LBB27_25:
	ble r18, r17, .LBB27_15
	jal r0, .LBB27_16
.LBB27_26:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB27_36
.LBB27_27:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	ldbu r6, r1+0
	add r7, r12, r0
	beq r6, r12, .LBB27_30
.LBB27_28:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r7, r1, r0
.LBB27_29:
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
	bne r6, r1, .LBB27_29
.LBB27_30:
	addi r3, sp, 32
	add r1, r3, r7
	stb r1+0, r12
	add r4, r12, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	jal r0, .LBB27_40
.LBB27_31:
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
	jal r0, .LBB27_35
.LBB27_32:
	addi r4, sp, 32
	add r4, r4, r18
	stb r4+0, r14
	ldw r4, r3+0
	add r1, r4, r1
	stw r3+0, r1
	ldbu r1, sp+32
	beq r1, r14, .LBB27_34
.LBB27_33:
	addi r3, sp, 32
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	fcvt.s.w r1, r1
	jal r0, .LBB27_35
.LBB27_34:
	lui r1, %hi(.LCPI27_0)
	addi r1, r1, %lo(.LCPI27_0)
	ldw r1, r1+0
.LBB27_35:
	stw r11+0, r1
.LBB27_36:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB27_38
.LBB27_37:
	addi r1, r1, -1
	stw r13+0, r1
.LBB27_38:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB27_40
.LBB27_39:
	stw r12+0, r1
.LBB27_40:
	ldw r22, sp+1056
	ldw r21, sp+1060
	ldw r20, sp+1064
	ldw r19, sp+1068
	ldw r18, sp+1072
	ldw r17, sp+1076
	ldw r16, sp+1080
	ldw r15, sp+1084
	ldw r14, sp+1088
	ldw r13, sp+1092
	ldw r12, sp+1096
	ldw r11, sp+1100
	ldw lr, sp+0
	addi sp, sp, 1104
	jalr r0, r31, 0
.Lfunc_end27:
	.size	f77_rd_r, .Lfunc_end27-f77_rd_r
                                        # -- End function
	.globl	f77_rd_s                        # -- Begin function f77_rd_s
	.p2align	2
	.type	f77_rd_s,@function
f77_rd_s:                               # @f77_rd_s
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	add r12, r4, r0
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r15, r0, 0
	beq r1, r15, .LBB28_5
.LBB28_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r15, .LBB28_41
.LBB28_2:
	jal r31, fio_list_tok
	addi r16, r0, 2
	beq r1, r16, .LBB28_41
.LBB28_3:
	addi r14, r0, 1
	bne r1, r14, .LBB28_27
.LBB28_4:
	addi r1, r0, 1
	stb r13+0, r1
	jal r0, .LBB28_41
.LBB28_5:
	lui r13, %hi(fio_desc)
	addi r13, r13, %lo(fio_desc)
	ldw r1, r13+0
	lui r15, %hi(fio_rep)
	addi r15, r15, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB28_7
.LBB28_6:
	ldw r3, r15+0
	addi r4, r0, 0
	bgt r3, r4, .LBB28_8
.LBB28_7:
	stw r13+0, r14
	jal r31, fio_next_desc
.LBB28_8:
	beq r1, r14, .LBB28_41
.LBB28_9:
	addi r3, r0, 65
	beq r1, r3, .LBB28_11
.LBB28_10:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.9)
	addi r4, r4, %lo(.L.str.9)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB28_11:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	sgt r3, r1, r14
	xor r1, r1, r12
	sub r3, r0, r3
	and r1, r1, r3
	xor r1, r12, r1
	lui r3, %hi(fio_rpos)
	addi r3, r3, %lo(fio_rpos)
	addi r4, r0, 1
	bge r1, r12, .LBB28_18
.LBB28_12:
	blt r12, r4, .LBB28_23
.LBB28_13:
	ldw r4, r3+0
	lui r5, %hi(fio_rlen)
	addi r5, r5, %lo(fio_rlen)
	ldw r5, r5+0
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	add r8, r14, r0
	jal r0, .LBB28_15
.LBB28_14:
	add r10, r11, r8
	stb r10+0, r9
	addi r8, r8, 1
	beq r12, r8, .LBB28_23
.LBB28_15:
	add r9, r6, r0
	bge r8, r1, .LBB28_14
.LBB28_16:
	add r10, r4, r8
	add r9, r6, r0
	bge r10, r5, .LBB28_14
.LBB28_17:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB28_14
.LBB28_18:
	blt r12, r4, .LBB28_23
.LBB28_19:
	ldw r5, r3+0
	lui r4, %hi(fio_rlen)
	addi r4, r4, %lo(fio_rlen)
	ldw r4, r4+0
	sub r8, r0, r12
	add r5, r1, r5
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	jal r0, .LBB28_21
.LBB28_20:
	stb r11+0, r9
	addi r9, r8, 1
	sltu r10, r9, r8
	addi r11, r11, 1
	add r8, r9, r0
	bne r10, r14, .LBB28_23
.LBB28_21:
	add r10, r5, r8
	add r9, r6, r0
	bge r10, r4, .LBB28_20
.LBB28_22:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB28_20
.LBB28_23:
	ldw r4, r3+0
	add r1, r4, r1
	stw r3+0, r1
	ldw r1, r15+0
	addi r3, r0, 1
	blt r1, r3, .LBB28_25
.LBB28_24:
	addi r1, r1, -1
	stw r15+0, r1
.LBB28_25:
	ldw r1, r15+0
	bne r1, r14, .LBB28_41
.LBB28_26:
	stw r13+0, r14
	jal r0, .LBB28_41
.LBB28_27:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	add r3, r13, r0
	jal r31, strlen
	blt r1, r16, .LBB28_36
.LBB28_28:
	ldbu r3, r13+0
	addi r4, r0, 39
	beq r3, r4, .LBB28_30
.LBB28_29:
	addi r4, r0, 34
	bne r3, r4, .LBB28_36
.LBB28_30:
	lui r4, %hi(fio_ltok-1)
	addi r4, r4, %lo(fio_ltok-1)
	add r4, r1, r4
	ldbu r4, r4+0
	bne r4, r3, .LBB28_36
.LBB28_31:
	bne r1, r16, .LBB28_42
.LBB28_32:
	addi r1, r0, 0
.LBB28_33:
	ble r12, r1, .LBB28_41
.LBB28_34:
	add r3, r11, r1
	sub r1, r12, r1
	addi r4, r0, 32
	addi r5, r0, 0
.LBB28_35:
	stb r3+0, r4
	addi r3, r3, 1
	addi r1, r1, -1
	bne r1, r5, .LBB28_35
	jal r0, .LBB28_41
.LBB28_36:
	blt r12, r14, .LBB28_41
.LBB28_37:
	addi r3, r0, 32
	jal r0, .LBB28_39
.LBB28_38:
	add r5, r11, r15
	stb r5+0, r4
	addi r15, r15, 1
	beq r12, r15, .LBB28_41
.LBB28_39:
	add r4, r3, r0
	bge r15, r1, .LBB28_38
.LBB28_40:
	add r4, r15, r13
	ldbu r4, r4+0
	jal r0, .LBB28_38
.LBB28_41:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB28_42:
	addi r4, r1, -1
	addi r1, r0, 0
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	jal r0, .LBB28_44
.LBB28_43:
	addi r14, r14, 1
	bge r14, r4, .LBB28_33
.LBB28_44:
	add r6, r14, r13
	ldbu r6, r6+0
	bne r6, r3, .LBB28_46
.LBB28_45:
	addi r6, r14, 1
	add r7, r14, r5
	ldbu r7, r7+0
	seq r7, r7, r3
	xor r6, r6, r14
	sub r7, r0, r7
	and r6, r6, r7
	xor r14, r14, r6
.LBB28_46:
	bge r1, r12, .LBB28_43
.LBB28_47:
	add r6, r14, r13
	ldbu r6, r6+0
	addi r7, r1, 1
	add r1, r11, r1
	stb r1+0, r6
	add r1, r7, r0
	jal r0, .LBB28_43
.Lfunc_end28:
	.size	f77_rd_s, .Lfunc_end28-f77_rd_s
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
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r3
	jalr r0, r31, 0
.Lfunc_end29:
	.size	f77_rd_end, .Lfunc_end29-f77_rd_end
                                        # -- End function
	.globl	f77_ipow                        # -- Begin function f77_ipow
	.p2align	2
	.type	f77_ipow,@function
f77_ipow:                               # @f77_ipow
# %bb.0:
	addi r1, r0, 1
	addi r5, r0, 0
	blt r4, r5, .LBB30_3
.LBB30_1:
	beq r4, r5, .LBB30_8
.LBB30_2:
	andi r6, r4, 1
	seq r6, r6, r5
	sub r6, r0, r6
	xori r7, r3, 1
	and r6, r7, r6
	xor r6, r3, r6
	mul r1, r6, r1
	mul r3, r3, r3
	srli r4, r4, 1
	bne r4, r5, .LBB30_2
	jal r0, .LBB30_8
.LBB30_3:
	beq r3, r1, .LBB30_6
.LBB30_4:
	addi r1, r0, -1
	bne r3, r1, .LBB30_7
.LBB30_5:
	andi r1, r4, 1
	addi r3, r0, 0
	seq r1, r1, r3
	addi r1, r1, -1
	ori  r1, r1, 1
	jal r0, .LBB30_8
.LBB30_6:
	add r1, r3, r0
	jal r0, .LBB30_8
.LBB30_7:
	add r1, r5, r0
.LBB30_8:
	jalr r0, r31, 0
.Lfunc_end30:
	.size	f77_ipow, .Lfunc_end30-f77_ipow
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_dpow_i
	.type	.LCPI31_0,@object
.LCPI31_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI31_0, 8
	.text
	.globl	f77_dpow_i
	.p2align	2
	.type	f77_dpow_i,@function
f77_dpow_i:                             # @f77_dpow_i
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	lui r1, %hi(.LCPI31_0)
	addi r1, r1, %lo(.LCPI31_0)
	ldw r9, r1+4
	ldw r8, r1+0
	addi r1, r0, 0
	add r10, r8, r0
	add r11, r9, r0
	beq r5, r1, .LBB31_3
.LBB31_1:
	add r7, r4, r0
	add r6, r3, r0
	srai r3, r5, 31
	xor r4, r5, r3
	sub r3, r4, r3
	add r10, r8, r0
	add r11, r9, r0
.LBB31_2:
	andi r4, r3, 1
	seq r4, r4, r1
	fmul.d r12, r6, r10
	xor lr, r11, r13
	sub r4, r0, r4
	and lr, lr, r4
	xor r11, r13, lr
	xor lr, r10, r12
	and r4, lr, r4
	xor r10, r12, r4
	fmul.d r6, r6, r6
	srli r3, r3, 1
	bne r3, r1, .LBB31_2
.LBB31_3:
	fdiv.d r6, r8, r10
	slt r1, r5, r1
	xor r3, r7, r11
	sub r1, r0, r1
	and r3, r3, r1
	xor r2, r11, r3
	xor r3, r6, r10
	and r1, r3, r1
	xor r1, r10, r1
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end31:
	.size	f77_dpow_i, .Lfunc_end31-f77_dpow_i
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rpow_i
	.type	.LCPI32_0,@object
.LCPI32_0:
	.word	0x3f800000                      # float 1
	.size	.LCPI32_0, 4
	.text
	.globl	f77_rpow_i
	.p2align	2
	.type	f77_rpow_i,@function
f77_rpow_i:                             # @f77_rpow_i
# %bb.0:
	lui r8, %hi(.LCPI32_0)
	addi r8, r8, %lo(.LCPI32_0)
	ldw r1, r8+0
	addi r5, r0, 0
	add r6, r1, r0
	beq r4, r5, .LBB32_3
.LBB32_1:
	srai r6, r4, 31
	xor r7, r4, r6
	sub r7, r7, r6
	ldw r6, r8+0
.LBB32_2:
	andi r8, r7, 1
	seq r8, r8, r5
	fmul.s r9, r3, r6
	xor r6, r6, r9
	sub r8, r0, r8
	and r6, r6, r8
	xor r6, r9, r6
	fmul.s r3, r3, r3
	srli r7, r7, 1
	bne r7, r5, .LBB32_2
.LBB32_3:
	fdiv.s r1, r1, r6
	slt r3, r4, r5
	xor r1, r1, r6
	sub r3, r0, r3
	and r1, r1, r3
	xor r1, r6, r1
	jalr r0, r31, 0
.Lfunc_end32:
	.size	f77_rpow_i, .Lfunc_end32-f77_rpow_i
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
	stw sp+276, r17
	add r12, r7, r0
	add r13, r6, r0
	add r14, r5, r0
	add r15, r4, r0
	add r11, r3, r0
	addi r1, r3, -32
	addi r3, r0, -31
	bltu r1, r3, .LBB33_2
.LBB33_1:
	addi r1, r11, -5
	addi r3, r0, 1
	bgtu r1, r3, .LBB33_3
.LBB33_2:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB33_3:
	addi r1, r14, -256
	addi r3, r0, -256
	bgtu r1, r3, .LBB33_5
.LBB33_4:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB33_5:
	addi r16, sp, 20
	add r3, r16, r0
	add r4, r15, r0
	add r5, r14, r0
	jal r31, memcpy
	add r1, r16, r14
	addi r17, r0, 0
	stb r1+0, r17
	addi r15, r0, 1
	blt r14, r15, .LBB33_9
.LBB33_6:
	addi r1, sp, 20
	addi r1, r1, -1
	addi r3, r0, 32
	addi r4, r0, 0
.LBB33_7:
	add r6, r1, r14
	ldbu r7, r6+0
	bne r7, r3, .LBB33_9
.LBB33_8:
	add r5, r14, r0
	addi r14, r14, -1
	stb r6+0, r4
	bgt r5, r15, .LBB33_7
.LBB33_9:
	slli r14, r11, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r16, r14, r1
	ldw r3, r16+0
	beq r3, r17, .LBB33_11
.LBB33_10:
	jal r31, fclose
	stw r16+0, r17
.LBB33_11:
	beq r13, r17, .LBB33_41
.LBB33_12:
	blt r12, r15, .LBB33_41
.LBB33_13:
	addi r4, r0, 0
	addi r3, r0, 3
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r1, r0, 26
.LBB33_14:
	beq r4, r3, .LBB33_18
.LBB33_15:
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
	bne r7, r6, .LBB33_27
.LBB33_16:
	addi r4, r4, 1
	bne r12, r4, .LBB33_14
.LBB33_17:
	add r4, r12, r0
	beq r4, r3, .LBB33_19
	jal r0, .LBB33_27
.LBB33_18:
	add r4, r3, r0
	bne r4, r3, .LBB33_27
.LBB33_19:
	addi r3, r0, 4
	blt r12, r3, .LBB33_23
.LBB33_20:
	addi r3, r13, 3
	addi r4, r12, -3
	addi r5, r0, 32
	addi r6, r0, 0
.LBB33_21:
	ldbu r7, r3+0
	bne r7, r5, .LBB33_27
.LBB33_22:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r6, .LBB33_21
.LBB33_23:
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	addi r3, sp, 20
	jal r31, fopen
	addi r12, r0, 0
	bne r1, r12, .LBB33_25
.LBB33_24:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	addi r3, sp, 20
	jal r31, fopen
.LBB33_25:
	bne r1, r12, .LBB33_60
.LBB33_26:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	jal r0, .LBB33_58
.LBB33_27:
	addi r4, r0, 0
	addi r3, r0, 3
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
.LBB33_28:
	beq r4, r3, .LBB33_32
.LBB33_29:
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
	bne r7, r6, .LBB33_41
.LBB33_30:
	addi r4, r4, 1
	bne r12, r4, .LBB33_28
.LBB33_31:
	add r1, r12, r0
	beq r1, r3, .LBB33_33
	jal r0, .LBB33_41
.LBB33_32:
	add r1, r3, r0
	bne r1, r3, .LBB33_41
.LBB33_33:
	addi r1, r0, 4
	blt r12, r1, .LBB33_37
.LBB33_34:
	addi r1, r13, 3
	addi r3, r12, -3
	addi r4, r0, 32
	addi r5, r0, 0
.LBB33_35:
	ldbu r6, r1+0
	bne r6, r4, .LBB33_41
.LBB33_36:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB33_35
.LBB33_37:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	addi r3, sp, 20
	jal r31, fopen
	addi r12, r0, 0
	beq r1, r12, .LBB33_39
.LBB33_38:
	add r3, r1, r0
	jal r31, fclose
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	addi r5, sp, 20
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB33_39:
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	addi r3, sp, 20
	jal r31, fopen
	bne r1, r12, .LBB33_60
.LBB33_40:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	jal r0, .LBB33_58
.LBB33_41:
	beq r13, r17, .LBB33_53
.LBB33_42:
	blt r12, r15, .LBB33_53
.LBB33_43:
	addi r1, r0, 7
	lui r3, %hi(.L.str.20)
	addi r3, r3, %lo(.L.str.20)
	addi r4, r0, 26
.LBB33_44:
	beq r17, r1, .LBB33_48
.LBB33_45:
	add r5, r17, r3
	ldb r5, r5+0
	add r6, r13, r17
	ldb r6, r6+0
	addi r7, r6, -97
	andi r7, r7, 255
	sltu r7, r7, r4
	addi r8, r6, -32
	xor r8, r8, r6
	sub r7, r0, r7
	and r7, r8, r7
	xor r6, r6, r7
	bne r6, r5, .LBB33_55
.LBB33_46:
	addi r17, r17, 1
	bne r12, r17, .LBB33_44
.LBB33_47:
	add r3, r12, r0
	beq r3, r1, .LBB33_49
	jal r0, .LBB33_55
.LBB33_48:
	add r3, r1, r0
	bne r3, r1, .LBB33_55
.LBB33_49:
	addi r1, r0, 8
	blt r12, r1, .LBB33_53
.LBB33_50:
	addi r1, r13, 7
	addi r3, r12, -7
	addi r4, r0, 32
	addi r5, r0, 0
.LBB33_51:
	ldbu r6, r1+0
	bne r6, r4, .LBB33_55
.LBB33_52:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB33_51
.LBB33_53:
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	addi r3, sp, 20
	jal r31, fopen
	addi r12, r0, 0
	beq r1, r12, .LBB33_56
.LBB33_54:
	bne r1, r12, .LBB33_60
	jal r0, .LBB33_57
.LBB33_55:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	jal r0, .LBB33_59
.LBB33_56:
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	addi r3, sp, 20
	jal r31, fopen
	bne r1, r12, .LBB33_60
.LBB33_57:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.21)
	addi r4, r4, %lo(.L.str.21)
.LBB33_58:
	addi r5, sp, 20
.LBB33_59:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	addi r1, r0, 0
.LBB33_60:
	stw r16+0, r1
	slli r1, r11, 8
	lui r3, %hi(fio_ufname)
	addi r3, r3, %lo(fio_ufname)
	add r3, r1, r3
	addi r4, sp, 20
	jal r31, strcpy
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r4, r1+0
	addi r3, r0, 0
	bne r4, r3, .LBB33_64
.LBB33_61:
	addi r4, r0, -128
	lui r5, %hi(fio_uend+128)
	addi r5, r5, %lo(fio_uend+128)
	addi r6, r0, -1
.LBB33_62:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r3, .LBB33_62
.LBB33_63:
	stb r1+0, r15
.LBB33_64:
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r1, r14, r1
	addi r3, r0, -1
	stw r1+0, r3
	ldw r17, sp+276
	ldw r16, sp+280
	ldw r15, sp+284
	ldw r14, sp+288
	ldw r13, sp+292
	ldw r12, sp+296
	ldw r11, sp+300
	ldw lr, sp+0
	addi sp, sp, 304
	jalr r0, r31, 0
.Lfunc_end33:
	.size	f77_open, .Lfunc_end33-f77_open
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
	stw sp+24, r16
	addi r1, r0, 31
	bgtu r3, r1, .LBB34_32
.LBB34_1:
	slli r11, r3, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r12, r11, r1
	ldw r6, r12+0
	addi r1, r0, 0
	beq r6, r1, .LBB34_32
.LBB34_2:
	addi r6, r0, 1
	beq r4, r1, .LBB34_9
.LBB34_3:
	addi r7, r0, 1
	add r13, r1, r0
	blt r5, r7, .LBB34_16
.LBB34_4:
	addi r6, r0, 0
	addi r8, r0, 6
	lui r9, %hi(.L.str.23)
	addi r9, r9, %lo(.L.str.23)
	addi r10, r0, 26
	add lr, r6, r0
.LBB34_5:
	beq lr, r8, .LBB34_10
.LBB34_6:
	add r13, lr, r9
	ldb r13, r13+0
	add r14, r4, lr
	ldb r14, r14+0
	addi r15, r14, -97
	andi r15, r15, 255
	sltu r15, r15, r10
	addi r16, r14, -32
	xor r16, r16, r14
	sub r15, r0, r15
	and r15, r16, r15
	xor r14, r14, r15
	bne r14, r13, .LBB34_15
.LBB34_7:
	addi lr, lr, 1
	bne r5, lr, .LBB34_5
.LBB34_8:
	add r9, r5, r0
	add r13, r6, r0
	beq r9, r8, .LBB34_11
	jal r0, .LBB34_16
.LBB34_9:
	add r13, r1, r0
	add r14, r3, r0
	beq r6, r1, .LBB34_17
	jal r0, .LBB34_29
.LBB34_10:
	add r9, r8, r0
	add r13, r6, r0
	bne r9, r8, .LBB34_16
.LBB34_11:
	addi r8, r0, 7
	add r6, r7, r0
	add r13, r7, r0
	blt r5, r8, .LBB34_16
.LBB34_12:
	addi r10, r5, -7
	addi r7, r4, 6
	addi r8, r0, 32
	addi r9, r0, 1
.LBB34_13:
	ldbu lr, r7+0
	seq r6, lr, r8
	addi r13, r10, -1
	sltu r10, r13, r10
	bne r10, r9, .LBB34_33
.LBB34_14:
	addi r7, r7, 1
	add r10, r13, r0
	add r13, r6, r0
	beq lr, r8, .LBB34_13
	jal r0, .LBB34_16
.LBB34_15:
	add r13, r6, r0
.LBB34_16:
	add r14, r3, r0
	bne r6, r1, .LBB34_29
.LBB34_17:
	addi r1, r0, 1
	blt r5, r1, .LBB34_28
.LBB34_18:
	addi r3, r0, 0
	addi r1, r0, 4
	lui r6, %hi(.L.str.24)
	addi r6, r6, %lo(.L.str.24)
	addi r7, r0, 26
.LBB34_19:
	beq r3, r1, .LBB34_23
.LBB34_20:
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
	bne r9, r8, .LBB34_28
.LBB34_21:
	addi r3, r3, 1
	bne r5, r3, .LBB34_19
.LBB34_22:
	add r3, r5, r0
	beq r3, r1, .LBB34_24
	jal r0, .LBB34_28
.LBB34_23:
	add r3, r1, r0
	bne r3, r1, .LBB34_28
.LBB34_24:
	addi r1, r0, 5
	blt r5, r1, .LBB34_29
.LBB34_25:
	addi r1, r4, 4
	addi r3, r5, -4
	addi r4, r0, 32
	addi r5, r0, 0
.LBB34_26:
	ldbu r6, r1+0
	bne r6, r4, .LBB34_28
.LBB34_27:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB34_26
	jal r0, .LBB34_29
.LBB34_28:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB34_29:
	ldw r3, r12+0
	jal r31, fclose
	addi r15, r0, 0
	stw r12+0, r15
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r1, r11, r1
	addi r3, r0, -1
	stw r1+0, r3
	slli r11, r14, 8
	lui r12, %hi(fio_ufname)
	addi r12, r12, %lo(fio_ufname)
	beq r13, r15, .LBB34_31
.LBB34_30:
	add r3, r11, r12
	jal r31, remove
.LBB34_31:
	add r1, r11, r12
	stb r1+0, r15
.LBB34_32:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB34_33:
	add r13, r6, r0
	add r14, r3, r0
	beq r6, r1, .LBB34_17
	jal r0, .LBB34_29
.Lfunc_end34:
	.size	f77_close, .Lfunc_end34-f77_close
                                        # -- End function
	.globl	f77_rewind                      # -- Begin function f77_rewind
	.p2align	2
	.type	f77_rewind,@function
f77_rewind:                             # @f77_rewind
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r4, r1+0
	addi r3, r0, 0
	bne r4, r3, .LBB35_4
.LBB35_1:
	addi r4, r0, -128
	lui r5, %hi(fio_uend+128)
	addi r5, r5, %lo(fio_uend+128)
	addi r6, r0, -1
.LBB35_2:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r3, .LBB35_2
.LBB35_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB35_4:
	addi r1, r11, -32
	addi r3, r0, -31
	bltu r1, r3, .LBB35_6
.LBB35_5:
	addi r1, r11, -5
	addi r3, r0, 1
	bgtu r1, r3, .LBB35_7
.LBB35_6:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.26)
	addi r4, r4, %lo(.L.str.26)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB35_7:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, fio_resolve
	add r11, r1, r0
	add r3, r1, r0
	jal r31, fflush
	addi r4, r0, 0
	add r3, r11, r0
	add r5, r4, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fseek
.Lfunc_end35:
	.size	f77_rewind, .Lfunc_end35-f77_rewind
                                        # -- End function
	.p2align	2                               # -- Begin function fio_number
	.type	fio_number,@function
fio_number:                             # @fio_number
# %bb.0:
	lui r1, %hi(fio_fmt)
	addi r1, r1, %lo(fio_fmt)
	ldw r1, r1+0
	lui r3, %hi(fio_pos)
	addi r3, r3, %lo(fio_pos)
	ldw r4, r3+0
	add r5, r1, r4
	ldb r5, r5+0
	addi r5, r5, -48
	addi r6, r0, 9
	bleu r5, r6, .LBB36_2
.LBB36_1:
	addi r1, r0, 0
	jal r0, .LBB36_5
.LBB36_2:
	addi r6, r1, 1
	addi r1, r0, 0
	addi r7, r0, 10
.LBB36_3:
	mul r1, r1, r7
	addi r8, r4, 1
	add r1, r5, r1
	add r4, r6, r4
	ldb r4, r4+0
	addi r5, r4, -48
	add r4, r8, r0
	bltu r5, r7, .LBB36_3
.LBB36_4:
	stw r3+0, r8
.LBB36_5:
	jalr r0, r31, 0
.Lfunc_end36:
	.size	fio_number, .Lfunc_end36-fio_number
                                        # -- End function
	.p2align	2                               # -- Begin function fio_getrec
	.type	fio_getrec,@function
fio_getrec:                             # @fio_getrec
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	ldw r1, r1+0
	addi r11, r0, 0
	beq r1, r11, .LBB37_3
.LBB37_1:
	lui r12, %hi(fio_irec)
	addi r12, r12, %lo(fio_irec)
	ldw r3, r12+0
	lui r4, %hi(fio_inrec)
	addi r4, r4, %lo(fio_inrec)
	ldw r4, r4+0
	bge r3, r4, .LBB37_15
.LBB37_2:
	lui r13, %hi(fio_ilen)
	addi r13, r13, %lo(fio_ilen)
	ldw r5, r13+0
	mul r3, r5, r3
	add r4, r1, r3
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	jal r31, memcpy
	ldw r1, r13+0
	lui r3, %hi(fio_rlen)
	addi r3, r3, %lo(fio_rlen)
	stw r3+0, r1
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	stw r1+0, r11
	ldw r1, r12+0
	addi r1, r1, 1
	stw r12+0, r1
	jal r0, .LBB37_16
.LBB37_3:
	lui r12, %hi(fio_unit)
	addi r12, r12, %lo(fio_unit)
	ldw r1, r12+0
	addi r3, r1, -1
	addi r4, r0, 30
	lui r11, %hi(fio_in)
	addi r11, r11, %lo(fio_in)
	bgtu r3, r4, .LBB37_7
.LBB37_4:
	slli r1, r1, 2
	lui r13, %hi(fio_uend)
	addi r13, r13, %lo(fio_uend)
	add r1, r1, r13
	ldw r3, r1+0
	addi r1, r0, 0
	blt r3, r1, .LBB37_7
.LBB37_5:
	ldw r3, r11+0
	beq r3, r1, .LBB37_7
.LBB37_6:
	jal r31, ftell
	ldw r3, r12+0
	slli r3, r3, 2
	add r3, r3, r13
	ldw r3, r3+0
	bge r1, r3, .LBB37_15
.LBB37_7:
	ldw r1, r11+0
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
	beq r1, r11, .LBB37_15
.LBB37_8:
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	jal r31, strlen
	addi r3, r0, 1
	blt r1, r3, .LBB37_14
.LBB37_9:
	addi r4, r1, 1
	lui r5, %hi(fio_rec-2)
	addi r5, r5, %lo(fio_rec-2)
	addi r6, r0, 13
	addi r1, r0, 0
	addi r7, r0, 10
	jal r0, .LBB37_11
.LBB37_10:
	addi r4, r4, -1
	ble r4, r3, .LBB37_14
.LBB37_11:
	add r8, r4, r5
	ldbu r8, r8+0
	beq r8, r6, .LBB37_10
.LBB37_12:
	beq r8, r7, .LBB37_10
.LBB37_13:
	addi r1, r4, -1
.LBB37_14:
	lui r3, %hi(fio_rlen)
	addi r3, r3, %lo(fio_rlen)
	stw r3+0, r1
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	stw r1+0, r11
	jal r0, .LBB37_16
.LBB37_15:
	addi r11, r0, 1
.LBB37_16:
	add r1, r11, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end37:
	.size	fio_getrec, .Lfunc_end37-fio_getrec
                                        # -- End function
	.type	fio_ibuf,@object                # @fio_ibuf
	.local	fio_ibuf
	.comm	fio_ibuf,4,4
	.type	fio_out,@object                 # @fio_out
	.local	fio_out
	.comm	fio_out,4,4
	.type	fio_unit,@object                # @fio_unit
	.local	fio_unit
	.comm	fio_unit,4,4
	.type	fio_ilen,@object                # @fio_ilen
	.local	fio_ilen
	.comm	fio_ilen,4,4
	.type	fio_inrec,@object               # @fio_inrec
	.local	fio_inrec
	.comm	fio_inrec,4,4
	.type	fio_irec,@object                # @fio_irec
	.local	fio_irec
	.comm	fio_irec,4,4
	.type	fio_listed,@object              # @fio_listed
	.local	fio_listed
	.comm	fio_listed,4,4
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%12d"
	.size	.L.str, 5

	.type	fio_first_item,@object          # @fio_first_item
	.local	fio_first_item
	.comm	fio_first_item,1,4
	.type	fio_prev_char,@object           # @fio_prev_char
	.local	fio_prev_char
	.comm	fio_prev_char,1,4
	.type	fio_w,@object                   # @fio_w
	.local	fio_w
	.comm	fio_w,4,4
	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"%d"
	.size	.L.str.1, 3

	.type	fio_scale,@object               # @fio_scale
	.local	fio_scale
	.comm	fio_scale,4,4
	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"%%.%df"
	.size	.L.str.2, 7

	.type	fio_d,@object                   # @fio_d
	.local	fio_d
	.comm	fio_d,4,4
	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"."
	.size	.L.str.3, 2

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"%g"
	.size	.L.str.4, 3

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"f77: concatenation result of %d bytes is too long\n"
	.size	.L.str.5, 51

	.type	fio_catpos,@object              # @fio_catpos
	.local	fio_catpos
	.comm	fio_catpos,4,4
	.type	fio_catring,@object             # @fio_catring
	.local	fio_catring
	.comm	fio_catring,65536,1
	.type	fio_draining,@object            # @fio_draining
	.local	fio_draining
	.comm	fio_draining,1,4
	.type	fio_desc,@object                # @fio_desc
	.local	fio_desc
	.comm	fio_desc,4,4
	.type	fio_rep,@object                 # @fio_rep
	.local	fio_rep
	.comm	fio_rep,4,4
	.type	fio_uend,@object                # @fio_uend
	.local	fio_uend
	.comm	fio_uend,128,4
	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"PAUSE %.*s\n"
	.size	.L.str.6, 12

	.type	fio_in,@object                  # @fio_in
	.local	fio_in
	.comm	fio_in,4,4
	.type	fio_ldone,@object               # @fio_ldone
	.local	fio_ldone
	.comm	fio_ldone,1,4
	.type	fio_ltok,@object                # @fio_ltok
	.local	fio_ltok
	.comm	fio_ltok,128,4
	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"f77: A editing on READ needs CHARACTER (not implemented)\n"
	.size	.L.str.7, 58

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"f77: %c editing does not match a numeric READ item\n"
	.size	.L.str.8, 52

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"f77: %c editing does not match a CHARACTER READ item\n"
	.size	.L.str.9, 54

	.type	fio_rpos,@object                # @fio_rpos
	.local	fio_rpos
	.comm	fio_rpos,4,4
	.type	fio_rlen,@object                # @fio_rlen
	.local	fio_rlen
	.comm	fio_rlen,4,4
	.type	fio_rec,@object                 # @fio_rec
	.local	fio_rec
	.comm	fio_rec,1024,1
	.type	fio_reading,@object             # @fio_reading
	.local	fio_reading
	.comm	fio_reading,1,4
	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"f77: cannot OPEN unit %d\n"
	.size	.L.str.10, 26

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"f77: bad FILE= name in OPEN\n"
	.size	.L.str.11, 29

	.type	fio_ufile,@object               # @fio_ufile
	.local	fio_ufile
	.comm	fio_ufile,128,4
	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"OLD"
	.size	.L.str.12, 4

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"r+"
	.size	.L.str.13, 3

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"r"
	.size	.L.str.14, 2

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"f77: OPEN STATUS='OLD': no such file '%s'\n"
	.size	.L.str.15, 43

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"NEW"
	.size	.L.str.16, 4

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"f77: OPEN STATUS='NEW': '%s' already exists\n"
	.size	.L.str.17, 45

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"w+"
	.size	.L.str.18, 3

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"f77: OPEN cannot create '%s'\n"
	.size	.L.str.19, 30

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"UNKNOWN"
	.size	.L.str.20, 8

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"f77: OPEN cannot open '%s'\n"
	.size	.L.str.21, 28

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"f77: OPEN STATUS value is not supported\n"
	.size	.L.str.22, 41

	.type	fio_ufname,@object              # @fio_ufname
	.local	fio_ufname
	.comm	fio_ufname,8192,1
	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"DELETE"
	.size	.L.str.23, 7

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"KEEP"
	.size	.L.str.24, 5

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"f77: CLOSE STATUS value is not supported\n"
	.size	.L.str.25, 42

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"f77: REWIND: unit %d cannot be rewound\n"
	.size	.L.str.26, 40

	.type	fio_uend_init,@object           # @fio_uend_init
	.local	fio_uend_init
	.comm	fio_uend_init,1,4
	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"fort.%d"
	.size	.L.str.27, 8

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"f77: unit %d is not open for %s\n"
	.size	.L.str.28, 33

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"WRITE"
	.size	.L.str.29, 6

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"READ"
	.size	.L.str.30, 5

	.type	fio_len,@object                 # @fio_len
	.local	fio_len
	.comm	fio_len,4,4
	.type	fio_gdepth,@object              # @fio_gdepth
	.local	fio_gdepth
	.comm	fio_gdepth,4,4
	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.zero	1
	.size	.L.str.31, 1

	.type	fio_fmt,@object                 # @fio_fmt
	.local	fio_fmt
	.comm	fio_fmt,4,4
	.type	fio_pos,@object                 # @fio_pos
	.local	fio_pos
	.comm	fio_pos,4,4
	.type	fio_revert,@object              # @fio_revert
	.local	fio_revert
	.comm	fio_revert,4,4
	.type	fio_line,@object                # @fio_line
	.local	fio_line
	.comm	fio_line,1024,1
	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"%%.%dE"
	.size	.L.str.32, 7

	.type	fio_e,@object                   # @fio_e
	.local	fio_e
	.comm	fio_e,4,4
	.type	fio_bz,@object                  # @fio_bz
	.local	fio_bz
	.comm	fio_bz,4,4
	.type	fio_gcount,@object              # @fio_gcount
	.local	fio_gcount
	.comm	fio_gcount,64,4
	.type	fio_gstart,@object              # @fio_gstart
	.local	fio_gstart
	.comm	fio_gstart,64,4
	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"f77: end of file on READ\n"
	.size	.L.str.33, 26

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"f77: WRITE past the end of an internal file\n"
	.size	.L.str.34, 45

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"f77: record longer than the internal file (%d > %d)\n"
	.size	.L.str.35, 53

	.type	fio_l_pending,@object           # @fio_l_pending
	.local	fio_l_pending
	.comm	fio_l_pending,4,4
	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"f77: zero repeat count in list-directed input is not supported\n"
	.size	.L.str.36, 64

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"f77: bad LOGICAL input field '%s'\n"
	.size	.L.str.37, 35

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"E%d"
	.size	.L.str.38, 4

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e2b547612936be39d9f8cbfc73bb496c3ce978d8)"
	.section	".note.GNU-stack","",@progbits
