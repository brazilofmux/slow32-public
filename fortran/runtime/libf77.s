	.file	"libf77.c"
	.text
	.globl	f77_wr_begin                    # -- Begin function f77_wr_begin
	.p2align	2
	.type	f77_wr_begin,@function
f77_wr_begin:                           # @f77_wr_begin
# %bb.0:
	lui r3, %hi(fio_len)
	addi r3, r3, %lo(fio_len)
	addi r1, r0, 0
	stw r3+0, r1
	lui r3, %hi(fio_gdepth)
	addi r3, r3, %lo(fio_gdepth)
	stw r3+0, r1
	lui r3, %hi(fio_rep)
	addi r3, r3, %lo(fio_rep)
	stw r3+0, r1
	lui r3, %hi(fio_desc)
	addi r3, r3, %lo(fio_desc)
	stw r3+0, r1
	beq r4, r1, .LBB0_6
.LBB0_1:
	lui r3, %hi(fio_listed)
	addi r3, r3, %lo(fio_listed)
	stb r3+0, r1
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
.LBB0_2:
	add r7, r4, r1
	ldbu r7, r7+0
	bne r7, r6, .LBB0_4
.LBB0_3:
	addi r1, r1, 1
	stw r3+0, r1
	jal r0, .LBB0_2
.LBB0_4:
	addi r4, r0, 40
	bne r7, r4, .LBB0_7
.LBB0_5:
	stw r5+0, r1
	addi r1, r1, 1
	stw r3+0, r1
	jal r0, .LBB0_7
.LBB0_6:
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	addi r3, r0, 1
	stb r1+0, r3
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
.LBB0_7:
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
	stw sp+56, r16
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldbu r1, r1+0
	addi r15, r0, 1
	bne r1, r15, .LBB1_8
.LBB1_1:
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r3, sp, 24
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
	ldbu r7, sp+24
	addi r5, r0, 0
	beq r7, r5, .LBB1_15
.LBB1_4:
	ldw r8, r1+0
	addi r6, sp, 24
	addi r6, r6, 1
	jal r0, .LBB1_6
.LBB1_5:
	ldbu r7, r6+0
	addi r6, r6, 1
	beq r7, r5, .LBB1_15
.LBB1_6:
	bgt r8, r3, .LBB1_5
.LBB1_7:
	addi r9, r8, 1
	stw r1+0, r9
	add r8, r8, r4
	stb r8+0, r7
	add r8, r9, r0
	jal r0, .LBB1_5
.LBB1_8:
	lui r13, %hi(fio_desc)
	addi r13, r13, %lo(fio_desc)
	ldw r1, r13+0
	lui r16, %hi(fio_rep)
	addi r16, r16, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB1_10
.LBB1_9:
	ldw r1, r16+0
	bgt r1, r14, .LBB1_11
.LBB1_10:
	stw r13+0, r14
	jal r31, fio_next_desc
	beq r1, r14, .LBB1_15
.LBB1_11:
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r12, sp, 24
	addi r4, r0, 32
	add r3, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	add r3, r12, r0
	jal r31, fio_field
	ldw r1, r16+0
	blt r1, r15, .LBB1_13
.LBB1_12:
	addi r1, r1, -1
	stw r16+0, r1
.LBB1_13:
	ldw r1, r16+0
	bne r1, r14, .LBB1_15
.LBB1_14:
	stw r13+0, r14
.LBB1_15:
	ldw r16, sp+56
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
	.globl	f77_wr_d                        # -- Begin function f77_wr_d
	.p2align	2
	.type	f77_wr_d,@function
f77_wr_d:                               # @f77_wr_d
# %bb.0:
	addi sp, sp, -208
	stw sp+0, lr
	stw sp+204, r11
	stw sp+200, r12
	stw sp+196, r13
	stw sp+192, r14
	stw sp+188, r15
	stw sp+184, r16
	stw sp+180, r17
	stw sp+176, r18
	stw sp+172, r19
	stw sp+168, r20
	add r13, r4, r0
	add r12, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldbu r1, r1+0
	addi r18, r0, 1
	bne r1, r18, .LBB3_8
.LBB3_1:
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	addi r3, sp, 24
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
	ldbu r7, sp+24
	addi r5, r0, 0
	beq r7, r5, .LBB3_50
.LBB3_4:
	ldw r8, r1+0
	addi r6, sp, 24
	addi r6, r6, 1
	jal r0, .LBB3_6
.LBB3_5:
	ldbu r7, r6+0
	addi r6, r6, 1
	beq r7, r5, .LBB3_50
.LBB3_6:
	bgt r8, r3, .LBB3_5
.LBB3_7:
	addi r9, r8, 1
	stw r1+0, r9
	add r8, r8, r4
	stb r8+0, r7
	add r8, r9, r0
	jal r0, .LBB3_5
.LBB3_8:
	lui r16, %hi(fio_desc)
	addi r16, r16, %lo(fio_desc)
	ldw r11, r16+0
	lui r17, %hi(fio_rep)
	addi r17, r17, %lo(fio_rep)
	addi r19, r0, 0
	beq r11, r19, .LBB3_10
.LBB3_9:
	ldw r1, r17+0
	addi r3, r0, 0
	bgt r1, r3, .LBB3_11
.LBB3_10:
	stw r16+0, r19
	jal r31, fio_next_desc
	add r11, r1, r0
.LBB3_11:
	addi r1, r11, -68
	addi r3, r0, 5
	bgtu r1, r3, .LBB3_22
.LBB3_12:
	slli r1, r1, 2
	lui r3, %hi(.LJTI3_0)
	addi r3, r3, %lo(.LJTI3_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB3_13:
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r1, r1+0
	slt r3, r1, r18
	addi r1, r1, -1
	sub r3, r0, r3
	xori r4, r1, 5
	and r3, r4, r3
	xor r6, r1, r3
	lui r5, %hi(.L.str.4)
	addi r5, r5, %lo(.L.str.4)
	addi r14, sp, 88
	addi r4, r0, 16
	add r3, r14, r0
	jal r31, snprintf
	addi r15, sp, 104
	addi r4, r0, 64
	add r3, r15, r0
	add r5, r14, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	ldbu r20, sp+104
	addi r12, r0, 45
	seq r1, r20, r12
	addi r13, r0, 43
	seq r3, r20, r13
	or  r1, r1, r3
	or  r1, r1, r15
	addi r3, r1, 1
	addi r1, r0, 68
	addi r15, r0, 46
	addi r4, r0, 69
	addi r5, r0, 101
	add r14, r19, r0
	jal r0, .LBB3_16
.LBB3_14:
	addi r7, sp, 104
	add r7, r7, r14
	stb r7+40, r6
	addi r14, r14, 1
.LBB3_15:
	addi r3, r3, 1
.LBB3_16:
	ldbu r6, r3+-1
	bgt r6, r1, .LBB3_19
.LBB3_17:
	beq r6, r15, .LBB3_15
.LBB3_18:
	bne r6, r19, .LBB3_14
	jal r0, .LBB3_25
.LBB3_19:
	beq r6, r4, .LBB3_21
.LBB3_20:
	bne r6, r5, .LBB3_14
.LBB3_21:
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	addi r3, r1, 1
	addi r1, r0, 0
	bne r14, r1, .LBB3_26
	jal r0, .LBB3_28
.LBB3_22:
	addi r1, r0, 0
	beq r11, r1, .LBB3_50
.LBB3_23:
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	addi r3, sp, 24
	addi r4, r0, 64
	jal r0, .LBB3_44
.LBB3_24:
	fcvt.w.d r6, r12
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	addi r3, sp, 24
	addi r4, r0, 64
	jal r0, .LBB3_45
.LBB3_25:
	add r3, r18, r0
	addi r1, r0, 0
	beq r14, r1, .LBB3_28
.LBB3_26:
	bne r14, r18, .LBB3_29
.LBB3_27:
	ldbu r4, sp+144
	andi r4, r4, 255
	addi r5, r0, 48
	bne r4, r5, .LBB3_29
.LBB3_28:
	addi r3, r0, 0
.LBB3_29:
	addi r5, sp, 24
	bne r20, r12, .LBB3_31
.LBB3_30:
	addi r4, sp, 24
	addi r5, r4, 1
	stb sp+24, r12
.LBB3_31:
	addi r4, r0, 48
	stb r5+0, r4
	addi r4, r5, 2
	stb r5+1, r15
	addi r6, sp, 24
	blt r14, r18, .LBB3_37
.LBB3_32:
	sub r6, r4, r6
	addi r7, r0, 55
	bgt r6, r7, .LBB3_37
.LBB3_33:
	addi r6, sp, 104
	addi r6, r6, 40
	addi r7, sp, 24
	sub r5, r5, r7
	addi r5, r5, 3
	addi r7, r0, 56
	add r8, r1, r0
.LBB3_34:
	add r9, r4, r8
	add r10, r6, r8
	ldbu r10, r10+0
	stb r9+0, r10
	addi r9, r8, 1
	bge r9, r14, .LBB3_36
.LBB3_35:
	add r10, r5, r8
	add r8, r9, r0
	blt r10, r7, .LBB3_34
.LBB3_36:
	add r4, r4, r9
.LBB3_37:
	stb r4+0, r11
	addi r5, r0, -1
	ble r3, r5, .LBB3_40
.LBB3_38:
	stb r4+1, r13
	addi r5, r0, 100
	blt r3, r5, .LBB3_41
.LBB3_39:
	lui r5, 335544
	addi r5, r5, 1311
	mulhu r5, r3, r5
	srli r5, r5, 5
	addi r5, r5, 48
	stb r4+2, r5
	lui r5, 838861
	addi r5, r5, -819
	mulhu r5, r3, r5
	srli r5, r5, 3
	lui r6, 104858
	addi r6, r6, -1638
	mulhu r6, r5, r6
	addi r7, r0, 10
	mul r6, r6, r7
	sub r6, r5, r6
	ori  r6, r6, 48
	stb r4+3, r6
	mul r5, r5, r7
	sub r3, r3, r5
	ori  r3, r3, 48
	addi r5, r4, 5
	stb r4+4, r3
	jal r0, .LBB3_42
.LBB3_40:
	stb r4+1, r12
	sub r3, r0, r3
	addi r5, r0, 100
	bge r3, r5, .LBB3_39
.LBB3_41:
	addi r5, r4, 4
	lui r6, 838861
	addi r6, r6, -819
	mulhu r6, r3, r6
	srli r6, r6, 3
	lui r7, 104858
	addi r7, r7, -1638
	mulhu r7, r6, r7
	addi r8, r0, 10
	mul r7, r7, r8
	sub r7, r6, r7
	ori  r7, r7, 48
	stb r4+2, r7
	mul r6, r6, r8
	sub r3, r3, r6
	ori  r3, r3, 48
	stb r4+3, r3
.LBB3_42:
	stb r5+0, r1
	jal r0, .LBB3_46
.LBB3_43:
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r6, r1+0
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	addi r11, sp, 104
	addi r4, r0, 16
	add r3, r11, r0
	jal r31, snprintf
	addi r3, sp, 24
	addi r4, r0, 64
	add r5, r11, r0
.LBB3_44:
	add r6, r12, r0
	add r7, r13, r0
.LBB3_45:
	jal r31, snprintf
.LBB3_46:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	addi r3, sp, 24
	jal r31, fio_field
	ldw r1, r17+0
	addi r3, r0, 1
	blt r1, r3, .LBB3_48
.LBB3_47:
	addi r1, r1, -1
	stw r17+0, r1
.LBB3_48:
	ldw r3, r17+0
	addi r1, r0, 0
	bne r3, r1, .LBB3_50
.LBB3_49:
	stw r16+0, r1
.LBB3_50:
	ldw r20, sp+168
	ldw r19, sp+172
	ldw r18, sp+176
	ldw r17, sp+180
	ldw r16, sp+184
	ldw r15, sp+188
	ldw r14, sp+192
	ldw r13, sp+196
	ldw r12, sp+200
	ldw r11, sp+204
	ldw lr, sp+0
	addi sp, sp, 208
	jalr r0, r31, 0
.Lfunc_end3:
	.size	f77_wr_d, .Lfunc_end3-f77_wr_d
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI3_0,@object
.LJTI3_0:
	.word	.LBB3_13
	.word	.LBB3_13
	.word	.LBB3_43
	.word	.LBB3_13
	.word	.LBB3_23
	.word	.LBB3_24
	.size	.LJTI3_0, 24
                                        # -- End function
	.text
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
.Lfunc_end4:
	.size	f77_wr_r, .Lfunc_end4-f77_wr_r
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
	stw sp+24, r16
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldbu r1, r1+0
	addi r11, r0, 1
	bne r1, r11, .LBB5_8
.LBB5_1:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r6, r1+0
	addi r5, r0, 1022
	bgt r6, r5, .LBB5_3
.LBB5_2:
	addi r7, r6, 1
	stw r1+0, r7
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	add r6, r6, r7
	addi r7, r0, 32
	stb r6+0, r7
.LBB5_3:
	blt r4, r11, .LBB5_29
.LBB5_4:
	ldw r8, r1+0
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 0
	jal r0, .LBB5_6
.LBB5_5:
	addi r4, r4, -1
	addi r3, r3, 1
	beq r4, r7, .LBB5_29
.LBB5_6:
	bgt r8, r5, .LBB5_5
.LBB5_7:
	ldbu r9, r3+0
	addi r10, r8, 1
	stw r1+0, r10
	add r8, r8, r6
	stb r8+0, r9
	add r8, r10, r0
	jal r0, .LBB5_5
.LBB5_8:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r14, %hi(fio_rep)
	addi r14, r14, %lo(fio_rep)
	addi r13, r0, 0
	beq r1, r13, .LBB5_10
.LBB5_9:
	ldw r1, r14+0
	bgt r1, r13, .LBB5_11
.LBB5_10:
	stw r12+0, r13
	add r15, r3, r0
	add r16, r4, r0
	jal r31, fio_next_desc
	add r4, r16, r0
	add r3, r15, r0
	beq r1, r13, .LBB5_29
.LBB5_11:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	blt r1, r11, .LBB5_13
.LBB5_12:
	blt r1, r4, .LBB5_20
.LBB5_13:
	ble r1, r4, .LBB5_19
.LBB5_14:
	sub r5, r1, r4
	blt r5, r11, .LBB5_19
.LBB5_15:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r7, r1+0
	addi r5, r5, 1
	addi r6, r0, 1022
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	addi r9, r0, 32
	jal r0, .LBB5_17
.LBB5_16:
	addi r5, r5, -1
	ble r5, r11, .LBB5_19
.LBB5_17:
	bgt r7, r6, .LBB5_16
.LBB5_18:
	addi r10, r7, 1
	stw r1+0, r10
	add r7, r7, r8
	stb r7+0, r9
	add r7, r10, r0
	jal r0, .LBB5_16
.LBB5_19:
	add r1, r4, r0
.LBB5_20:
	blt r1, r11, .LBB5_25
.LBB5_21:
	lui r4, %hi(fio_len)
	addi r4, r4, %lo(fio_len)
	ldw r7, r4+0
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	jal r0, .LBB5_23
.LBB5_22:
	addi r1, r1, -1
	addi r3, r3, 1
	beq r1, r13, .LBB5_25
.LBB5_23:
	bgt r7, r5, .LBB5_22
.LBB5_24:
	ldbu r8, r3+0
	addi r9, r7, 1
	stw r4+0, r9
	add r7, r7, r6
	stb r7+0, r8
	add r7, r9, r0
	jal r0, .LBB5_22
.LBB5_25:
	ldw r1, r14+0
	blt r1, r11, .LBB5_27
.LBB5_26:
	addi r1, r1, -1
	stw r14+0, r1
.LBB5_27:
	ldw r1, r14+0
	bne r1, r13, .LBB5_29
.LBB5_28:
	stw r12+0, r13
.LBB5_29:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end5:
	.size	f77_wr_a, .Lfunc_end5-f77_wr_a
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
	beq r1, r13, .LBB6_2
.LBB6_1:
	ldw r1, r14+0
	bgt r1, r13, .LBB6_3
.LBB6_2:
	stw r12+0, r13
	jal r31, fio_next_desc
	beq r1, r13, .LBB6_14
.LBB6_3:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r6, r1+0
	addi r5, r0, 2
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	addi r4, r0, 1022
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	blt r6, r5, .LBB6_8
.LBB6_4:
	ldw r8, r1+0
	addi r6, r6, 1
	addi r7, r0, 32
	jal r0, .LBB6_6
.LBB6_5:
	addi r6, r6, -1
	ble r6, r5, .LBB6_8
.LBB6_6:
	bgt r8, r4, .LBB6_5
.LBB6_7:
	addi r9, r8, 1
	stw r1+0, r9
	add r8, r8, r3
	stb r8+0, r7
	add r8, r9, r0
	jal r0, .LBB6_5
.LBB6_8:
	ldw r5, r1+0
	bgt r5, r4, .LBB6_10
.LBB6_9:
	seq r4, r11, r13
	sub r4, r0, r4
	andi r4, r4, 18
	xori r4, r4, 84
	addi r6, r5, 1
	stw r1+0, r6
	add r1, r5, r3
	stb r1+0, r4
.LBB6_10:
	ldw r1, r14+0
	addi r3, r0, 1
	blt r1, r3, .LBB6_12
.LBB6_11:
	addi r1, r1, -1
	stw r14+0, r1
.LBB6_12:
	ldw r1, r14+0
	bne r1, r13, .LBB6_14
.LBB6_13:
	stw r12+0, r13
.LBB6_14:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end6:
	.size	f77_wr_l, .Lfunc_end6-f77_wr_l
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
	ldbu r1, r1+0
	addi r11, r0, 0
	bne r1, r11, .LBB7_2
.LBB7_1:
	lui r12, %hi(fio_revert)
	addi r12, r12, %lo(fio_revert)
	ldw r13, r12+0
	addi r1, r0, -1
	stw r12+0, r1
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	stw r1+0, r11
	lui r1, %hi(fio_rep)
	addi r1, r1, %lo(fio_rep)
	stw r1+0, r11
	jal r31, fio_next_desc
	stw r12+0, r13
.LBB7_2:
	lui r12, %hi(fio_len)
	addi r12, r12, %lo(fio_len)
	ldw r1, r12+0
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	add r1, r1, r3
	stb r1+0, r11
	lui r13, %hi(stdout)
	addi r13, r13, %lo(stdout)
	ldw r4, r13+0
	jal r31, fputs
	ldw r4, r13+0
	addi r3, r0, 10
	jal r31, fputc
	stw r12+0, r11
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end7:
	.size	f77_wr_end, .Lfunc_end7-f77_wr_end
                                        # -- End function
	.p2align	2                               # -- Begin function fio_next_desc
	.type	fio_next_desc,@function
fio_next_desc:                          # @fio_next_desc
# %bb.0:
	addi sp, sp, -112
	stw sp+0, lr
	stw sp+4, fp
	stw sp+108, r11
	stw sp+104, r12
	stw sp+100, r13
	stw sp+96, r14
	stw sp+92, r15
	stw sp+88, r16
	stw sp+84, r17
	stw sp+80, r18
	stw sp+76, r19
	stw sp+72, r20
	stw sp+68, r21
	stw sp+64, r22
	stw sp+60, r23
	stw sp+56, r24
	stw sp+52, r25
	stw sp+48, r26
	stw sp+44, r27
	stw sp+40, r28
	addi fp, r0, 1
	lui r15, %hi(fio_fmt)
	addi r15, r15, %lo(fio_fmt)
	lui r12, %hi(fio_pos)
	addi r12, r12, %lo(fio_pos)
	addi r16, r0, 47
	addi r13, r0, 9
	addi r17, r0, 40
	lui r18, %hi(fio_gdepth)
	addi r18, r18, %lo(fio_gdepth)
	addi r1, r0, 15
	stw sp+36, r1
	lui r21, %hi(fio_gstart)
	addi r21, r21, %lo(fio_gstart)
	lui r22, %hi(fio_gcount)
	addi r22, r22, %lo(fio_gcount)
	addi r20, r0, 0
	lui r23, %hi(fio_revert)
	addi r23, r23, %lo(fio_revert)
	addi r24, r0, -33
	addi r25, r0, 72
	lui r26, %hi(fio_len)
	addi r26, r26, %lo(fio_len)
	addi r27, r0, 1022
	lui r11, %hi(fio_line)
	addi r11, r11, %lo(fio_line)
	addi r1, r0, 80
	stw sp+32, r1
	addi r1, r0, 88
	stw sp+28, r1
	add r14, fp, r0
	jal r0, .LBB8_3
.LBB8_1:
	addi r1, r6, 1
	stw r12+0, r1
	ldw r1, r26+0
	add r1, r1, r11
	stb r1+0, r20
	lui r19, %hi(stdout)
	addi r19, r19, %lo(stdout)
	ldw r4, r19+0
	add r3, r11, r0
	jal r31, fputs
	ldw r4, r19+0
	addi r3, r0, 10
	jal r31, fputc
	stw r26+0, r20
.LBB8_2:
	addi r14, r14, 1
	lui r1, 24
	addi r1, r1, 1697
	beq r14, r1, .LBB8_49
.LBB8_3:
	ldw r1, r15+0
	ldw r6, r12+0
	add r3, r1, r6
	ldbu r3, r3+0
	bgtu r3, r16, .LBB8_11
.LBB8_4:
	slli r4, r3, 2
	lui r5, %hi(.LJTI8_0)
	addi r5, r5, %lo(.LJTI8_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB8_5:
	addi r1, r6, 1
	stw r12+0, r1
	jal r0, .LBB8_2
.LBB8_6:
	ldw r3, r18+0
	addi r4, r0, 1
	blt r3, r4, .LBB8_8
.LBB8_7:
	addi r3, r0, 0
	stw r18+0, r3
.LBB8_8:
	ldw r3, r23+0
	blt r3, r20, .LBB8_49
.LBB8_9:
	stw r12+0, r3
	add r1, r1, r3
	ldbu r3, r1+0
	addi r1, r0, 0
	beq r3, r1, .LBB8_50
.LBB8_10:
	ldw r1, r26+0
	add r1, r1, r11
	addi r28, r0, 0
	stb r1+0, r28
	lui r19, %hi(stdout)
	addi r19, r19, %lo(stdout)
	ldw r4, r19+0
	add r3, r11, r0
	jal r31, fputs
	ldw r4, r19+0
	addi r3, r0, 10
	jal r31, fputc
	stw r26+0, r28
	jal r0, .LBB8_2
.LBB8_11:
	slli r3, r3, 24
	srai r3, r3, 24
	addi r4, r3, -48
	add r3, fp, r0
	bgtu r4, r13, .LBB8_15
.LBB8_12:
	addi r5, r1, 1
	addi r3, r0, 0
	add r7, r6, r0
.LBB8_13:
	addi r9, r0, 10
	mul r3, r3, r9
	addi r8, r7, 1
	add r3, r3, r4
	add r4, r5, r7
	ldb r4, r4+0
	addi r4, r4, -48
	add r7, r8, r0
	bltu r4, r9, .LBB8_13
.LBB8_14:
	stw r12+0, r8
.LBB8_15:
	ldw r5, r12+0
	add r4, r1, r5
	ldb r4, r4+0
	andi r7, r4, 255
	bne r7, r17, .LBB8_31
.LBB8_16:
	addi r4, r5, 1
	stw r12+0, r4
	ldw r1, r18+0
	ldw r5, sp+36
	bgt r1, r5, .LBB8_2
.LBB8_17:
	slli r5, r1, 2
	add r7, r5, r21
	stw r7+0, r4
	add r4, r5, r22
	stw r4+0, r3
	addi r3, r1, 1
	stw r18+0, r3
	bne r1, r20, .LBB8_2
.LBB8_18:
	stw r23+0, r6
	jal r0, .LBB8_2
.LBB8_19:
	addi r3, r6, 1
	stw r12+0, r3
	ldw r4, r26+0
.LBB8_20:
	add r7, r1, r3
	ldbu r5, r7+0
	beq r5, r20, .LBB8_2
.LBB8_21:
	addi r6, r0, 39
	beq r5, r6, .LBB8_24
.LBB8_22:
	addi r3, r3, 1
	stw r12+0, r3
	bgt r4, r27, .LBB8_20
.LBB8_23:
	addi r6, r4, 1
	stw r26+0, r6
	add r4, r4, r11
	stb r4+0, r5
	add r4, r6, r0
	jal r0, .LBB8_20
.LBB8_24:
	ldbu r5, r7+1
	bne r5, r6, .LBB8_48
.LBB8_25:
	bgt r4, r27, .LBB8_27
.LBB8_26:
	addi r5, r4, 1
	stw r26+0, r5
	add r4, r4, r11
	stb r4+0, r6
	add r4, r5, r0
.LBB8_27:
	addi r3, r3, 2
	stw r12+0, r3
	jal r0, .LBB8_20
.LBB8_28:
	addi r1, r6, 1
	stw r12+0, r1
	ldw r1, r18+0
	addi r3, r0, 1
	blt r1, r3, .LBB8_2
.LBB8_29:
	addi r3, r1, -1
	stw r18+0, r3
	slli r3, r3, 2
	add r4, r3, r22
	ldw r5, r4+0
	addi r6, r5, -1
	stw r4+0, r6
	addi r4, r0, 2
	blt r5, r4, .LBB8_2
.LBB8_30:
	add r3, r3, r21
	ldw r3, r3+0
	stw r12+0, r3
	stw r18+0, r1
	jal r0, .LBB8_2
.LBB8_31:
	and r6, r4, r24
	beq r6, r25, .LBB8_39
.LBB8_32:
	andi r6, r4, 223
	ldw r8, sp+32
	beq r6, r8, .LBB8_47
.LBB8_33:
	ldw r8, sp+28
	bne r6, r8, .LBB8_45
.LBB8_34:
	addi r1, r5, 1
	stw r12+0, r1
	blt r3, fp, .LBB8_2
.LBB8_35:
	ldw r4, r26+0
	addi r1, r3, 1
	jal r0, .LBB8_37
.LBB8_36:
	addi r1, r1, -1
	addi r3, r0, 1
	ble r1, r3, .LBB8_2
.LBB8_37:
	bgt r4, r27, .LBB8_36
.LBB8_38:
	addi r3, r4, 1
	stw r26+0, r3
	add r4, r4, r11
	addi r5, r0, 32
	stb r4+0, r5
	add r4, r3, r0
	jal r0, .LBB8_36
.LBB8_39:
	addi r4, r5, 1
	stw r12+0, r4
	blt r3, fp, .LBB8_2
.LBB8_40:
	ldw r4, r26+0
	addi r3, r3, 1
	addi r1, r1, 1
	jal r0, .LBB8_42
.LBB8_41:
	addi r3, r3, -1
	addi r5, r5, 1
	ble r3, fp, .LBB8_2
.LBB8_42:
	add r6, r1, r5
	ldbu r6, r6+0
	beq r6, r20, .LBB8_2
.LBB8_43:
	addi r7, r5, 2
	stw r12+0, r7
	bgt r4, r27, .LBB8_41
.LBB8_44:
	addi r7, r4, 1
	stw r26+0, r7
	add r4, r4, r11
	stb r4+0, r6
	add r4, r7, r0
	jal r0, .LBB8_41
.LBB8_45:
	addi r6, r7, -65
	addi r7, r0, 43
	bgtu r6, r7, .LBB8_47
.LBB8_46:
	slli r6, r6, 2
	lui r7, %hi(.LJTI8_1)
	addi r7, r7, %lo(.LJTI8_1)
	add r6, r7, r6
	ldw r6, r6+0
	jalr r0, r6, 0
.LBB8_47:
	addi r1, r5, 1
	stw r12+0, r1
	jal r0, .LBB8_2
.LBB8_48:
	addi r1, r3, 1
	stw r12+0, r1
	jal r0, .LBB8_2
.LBB8_49:
	add r1, r20, r0
.LBB8_50:
	ldw r28, sp+40
	ldw r27, sp+44
	ldw r26, sp+48
	ldw r25, sp+52
	ldw r24, sp+56
	ldw r23, sp+60
	ldw r22, sp+64
	ldw r21, sp+68
	ldw r20, sp+72
	ldw r19, sp+76
	ldw r18, sp+80
	ldw r17, sp+84
	ldw r16, sp+88
	ldw r15, sp+92
	ldw r14, sp+96
	ldw r13, sp+100
	ldw r12, sp+104
	ldw r11, sp+108
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 112
	jalr r0, r31, 0
.LBB8_51:
	addi r6, r5, 1
	stw r12+0, r6
	add r6, r1, r6
	ldb r6, r6+0
	addi r8, r6, -48
	addi r6, r0, 0
	add r7, r6, r0
	bgtu r8, r13, .LBB8_55
.LBB8_52:
	addi r9, r1, 2
	addi r7, r0, 0
	addi r10, r0, 10
.LBB8_53:
	mul r7, r7, r10
	add r7, r7, r8
	add r8, r9, r5
	ldb r8, r8+0
	addi r8, r8, -48
	addi r5, r5, 1
	bltu r8, r10, .LBB8_53
.LBB8_54:
	addi r5, r5, 1
	stw r12+0, r5
.LBB8_55:
	lui r5, %hi(fio_w)
	addi r5, r5, %lo(fio_w)
	stw r5+0, r7
	lui r5, %hi(fio_d)
	addi r5, r5, %lo(fio_d)
	stw r5+0, r6
	ldw r6, r12+0
	add r7, r1, r6
	ldbu r7, r7+0
	addi r8, r0, 46
	bne r7, r8, .LBB8_62
.LBB8_56:
	addi r7, r6, 1
	stw r12+0, r7
	add r7, r1, r7
	ldb r7, r7+0
	addi r9, r7, -48
	bleu r9, r13, .LBB8_58
.LBB8_57:
	addi r7, r0, 0
	jal r0, .LBB8_61
.LBB8_58:
	addi r8, r1, 2
	addi r7, r0, 0
	addi r10, r0, 10
.LBB8_59:
	mul r7, r7, r10
	add r7, r7, r9
	add r9, r8, r6
	ldb r9, r9+0
	addi r9, r9, -48
	addi r6, r6, 1
	bltu r9, r10, .LBB8_59
.LBB8_60:
	addi r6, r6, 1
	stw r12+0, r6
.LBB8_61:
	stw r5+0, r7
.LBB8_62:
	ldw r5, r12+0
	add r6, r1, r5
	ldbu r6, r6+0
	ori  r6, r6, 32
	addi r7, r0, 101
	bne r6, r7, .LBB8_68
.LBB8_63:
	addi r6, r5, 1
	stw r12+0, r6
	add r6, r1, r6
	ldb r6, r6+0
	addi r6, r6, -58
	addi r7, r0, -10
	bltu r6, r7, .LBB8_67
.LBB8_64:
	addi r1, r1, 2
	addi r6, r0, -11
.LBB8_65:
	add r7, r1, r5
	ldb r7, r7+0
	addi r7, r7, -58
	addi r5, r5, 1
	bgtu r7, r6, .LBB8_65
.LBB8_66:
	addi r5, r5, 1
.LBB8_67:
	stw r12+0, r5
.LBB8_68:
	lui r1, %hi(fio_rep)
	addi r1, r1, %lo(fio_rep)
	stw r1+0, r3
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
	jal r0, .LBB8_50
.Lfunc_end8:
	.size	fio_next_desc, .Lfunc_end8-fio_next_desc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI8_0,@object
.LJTI8_0:
	.word	.LBB8_6
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_5
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_19
	.word	.LBB8_11
	.word	.LBB8_28
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_5
	.word	.LBB8_11
	.word	.LBB8_11
	.word	.LBB8_1
	.size	.LJTI8_0, 192
	.type	.LJTI8_1,@object
.LJTI8_1:
	.word	.LBB8_51
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_51
	.word	.LBB8_51
	.word	.LBB8_51
	.word	.LBB8_51
	.word	.LBB8_47
	.word	.LBB8_51
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_51
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_51
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_51
	.word	.LBB8_51
	.word	.LBB8_51
	.word	.LBB8_51
	.word	.LBB8_47
	.word	.LBB8_51
	.word	.LBB8_47
	.word	.LBB8_47
	.word	.LBB8_51
	.size	.LJTI8_1, 176
                                        # -- End function
	.type	fio_len,@object                 # @fio_len
	.local	fio_len
	.comm	fio_len,4,4
	.type	fio_gdepth,@object              # @fio_gdepth
	.local	fio_gdepth
	.comm	fio_gdepth,4,4
	.type	fio_rep,@object                 # @fio_rep
	.local	fio_rep
	.comm	fio_rep,4,4
	.type	fio_desc,@object                # @fio_desc
	.local	fio_desc
	.comm	fio_desc,4,4
	.type	fio_listed,@object              # @fio_listed
	.local	fio_listed
	.comm	fio_listed,1,4
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
	.type	fio_line,@object                # @fio_line
	.local	fio_line
	.comm	fio_line,1024,1
	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"%%.%dE"
	.size	.L.str.4, 7

	.type	fio_gcount,@object              # @fio_gcount
	.local	fio_gcount
	.comm	fio_gcount,64,4
	.type	fio_gstart,@object              # @fio_gstart
	.local	fio_gstart
	.comm	fio_gstart,64,4
	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e507704cf3c4d36284ffcb21f50e8531ceb63f7f)"
	.section	".note.GNU-stack","",@progbits
