	.file	"libf77.c"
	.text
	.globl	f77_set_rec                     # -- Begin function f77_set_rec
	.p2align	2
	.type	f77_set_rec,@function
f77_set_rec:                            # @f77_set_rec
# %bb.0:
	lui r1, %hi(fio_rec_next)
	addi r1, r1, %lo(fio_rec_next)
	stw r1+0, r3
	jalr r0, r31, 0
.Lfunc_end0:
	.size	f77_set_rec, .Lfunc_end0-f77_set_rec
                                        # -- End function
	.globl	f77_wr_begin                    # -- Begin function f77_wr_begin
	.p2align	2
	.type	f77_wr_begin,@function
f77_wr_begin:                           # @f77_wr_begin
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	add r11, r4, r0
	add r13, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	addi r14, r0, 0
	bne r3, r14, .LBB1_4
.LBB1_1:
	addi r3, r0, -128
	lui r4, %hi(fio_uend+128)
	addi r4, r4, %lo(fio_uend+128)
	addi r5, r0, -1
.LBB1_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r14, .LBB1_2
.LBB1_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB1_4:
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r14
	addi r12, r0, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, fio_resolve
	lui r3, %hi(fio_out)
	addi r3, r3, %lo(fio_out)
	stw r3+0, r1
	lui r3, %hi(fio_unit)
	addi r3, r3, %lo(fio_unit)
	stw r3+0, r13
	addi r3, r13, -1
	addi r4, r0, 30
	slli r15, r13, 2
	bgtu r3, r4, .LBB1_6
.LBB1_5:
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r15, r3
	addi r4, r0, 0
	stw r3+0, r4
.LBB1_6:
	lui r3, %hi(fio_rec_next)
	addi r3, r3, %lo(fio_rec_next)
	ldw r16, r3+0
	stw r3+0, r14
	addi r3, r13, -32
	addi r4, r0, -31
	bltu r3, r4, .LBB1_11
.LBB1_7:
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r15, r3
	ldw r3, r3+0
	beq r3, r14, .LBB1_11
.LBB1_8:
	bgt r16, r14, .LBB1_10
.LBB1_9:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.57)
	addi r4, r4, %lo(.L.str.57)
	add r13, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r1, r13, r0
.LBB1_10:
	addi r3, r16, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r4, r15, r4
	ldw r4, r4+0
	mul r4, r4, r3
	addi r5, r0, 0
	add r3, r1, r0
	jal r31, fseek
.LBB1_11:
	lui r1, %hi(fio_reading)
	addi r1, r1, %lo(fio_reading)
	stb r1+0, r14
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	stw r1+0, r14
	lui r1, %hi(fio_col)
	addi r1, r1, %lo(fio_col)
	stw r1+0, r14
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	stb r1+0, r14
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
	stb r1+0, r12
	lui r1, %hi(fio_prev_char)
	addi r1, r1, %lo(fio_prev_char)
	stb r1+0, r14
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	beq r11, r14, .LBB1_17
.LBB1_12:
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
.LBB1_13:
	add r5, r11, r14
	ldbu r5, r5+0
	bne r5, r4, .LBB1_15
.LBB1_14:
	addi r14, r14, 1
	stw r1+0, r14
	jal r0, .LBB1_13
.LBB1_15:
	addi r4, r0, 40
	bne r5, r4, .LBB1_19
.LBB1_16:
	stw r3+0, r14
	addi r3, r14, 1
	jal r0, .LBB1_18
.LBB1_17:
	stw r1+0, r12
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
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
.LBB1_18:
	stw r1+0, r3
.LBB1_19:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end1:
	.size	f77_wr_begin, .Lfunc_end1-f77_wr_begin
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
	bgtu r3, r1, .LBB2_2
.LBB2_1:
	slli r1, r3, 2
	lui r5, %hi(fio_ufile)
	addi r5, r5, %lo(fio_ufile)
	add r1, r1, r5
	ldw r1, r1+0
	addi r5, r0, 0
	bne r1, r5, .LBB2_19
.LBB2_2:
	addi r1, r0, 0
	beq r4, r1, .LBB2_7
.LBB2_3:
	beq r3, r1, .LBB2_15
.LBB2_4:
	addi r5, r0, 5
	beq r3, r5, .LBB2_6
.LBB2_5:
	addi r5, r0, 6
	bne r3, r5, .LBB2_10
.LBB2_6:
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	jal r0, .LBB2_19
.LBB2_7:
	addi r5, r0, 5
	beq r3, r5, .LBB2_9
.LBB2_8:
	addi r5, r0, 0
	bne r3, r5, .LBB2_10
.LBB2_9:
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	ldw r1, r1+0
	jal r0, .LBB2_19
.LBB2_10:
	add r11, r3, r0
	addi r3, r3, -1
	addi r5, r0, 30
	bgtu r3, r5, .LBB2_17
.LBB2_11:
	add r13, r4, r0
	add r14, r1, r0
	lui r5, %hi(.L.str.27)
	addi r5, r5, %lo(.L.str.27)
	addi r12, sp, 16
	addi r4, r0, 256
	add r3, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	add r3, r12, r0
	jal r31, fopen
	addi r12, r0, 0
	bne r1, r12, .LBB2_13
.LBB2_12:
	lui r4, %hi(.L.str.36)
	addi r4, r4, %lo(.L.str.36)
	addi r3, sp, 16
	jal r31, fopen
.LBB2_13:
	beq r1, r12, .LBB2_16
.LBB2_14:
	slli r13, r11, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r3, r13, r3
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
	add r1, r13, r1
	addi r3, r0, -1
	stw r1+0, r3
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r13, r1
	stw r1+0, r12
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r13, r1
	stw r1+0, r12
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r13, r1
	stw r1+0, r12
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r13, r1
	stw r1+0, r12
	jal r0, .LBB2_18
.LBB2_15:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	jal r0, .LBB2_19
.LBB2_16:
	add r1, r14, r0
	add r4, r13, r0
.LBB2_17:
	seq r4, r4, r1
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r5, %hi(.L.str.55)
	addi r5, r5, %lo(.L.str.55)
	lui r6, %hi(.L.str.56)
	addi r6, r6, %lo(.L.str.56)
	xor r6, r6, r5
	sub r4, r0, r4
	and r4, r6, r4
	xor r6, r4, r5
	lui r4, %hi(.L.str.54)
	addi r4, r4, %lo(.L.str.54)
	add r5, r11, r0
	add r11, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB2_18:
	add r1, r11, r0
.LBB2_19:
	ldw r14, sp+272
	ldw r13, sp+276
	ldw r12, sp+280
	ldw r11, sp+284
	ldw lr, sp+0
	addi sp, sp, 288
	jalr r0, r31, 0
.Lfunc_end2:
	.size	fio_resolve, .Lfunc_end2-fio_resolve
                                        # -- End function
	.globl	f77_wu_begin                    # -- Begin function f77_wu_begin
	.p2align	2
	.type	f77_wu_begin,@function
f77_wu_begin:                           # @f77_wu_begin
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	addi r13, r0, 0
	bne r3, r13, .LBB3_4
.LBB3_1:
	addi r3, r0, -128
	lui r4, %hi(fio_uend+128)
	addi r4, r4, %lo(fio_uend+128)
	addi r5, r0, -1
.LBB3_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r13, .LBB3_2
.LBB3_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB3_4:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_resolve
	lui r3, %hi(fio_out)
	addi r3, r3, %lo(fio_out)
	stw r3+0, r1
	lui r3, %hi(fio_unit)
	addi r3, r3, %lo(fio_unit)
	stw r3+0, r11
	addi r3, r11, -1
	addi r4, r0, 30
	slli r14, r11, 2
	bgtu r3, r4, .LBB3_6
.LBB3_5:
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r14, r3
	stw r3+0, r12
.LBB3_6:
	lui r3, %hi(fio_rec_next)
	addi r3, r3, %lo(fio_rec_next)
	ldw r12, r3+0
	stw r3+0, r13
	addi r3, r11, -32
	addi r4, r0, -31
	bltu r3, r4, .LBB3_11
.LBB3_7:
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r14, r3
	ldw r3, r3+0
	beq r3, r13, .LBB3_11
.LBB3_8:
	bgt r12, r13, .LBB3_10
.LBB3_9:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.57)
	addi r4, r4, %lo(.L.str.57)
	add r11, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r1, r11, r0
.LBB3_10:
	addi r3, r12, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r4, r14, r4
	ldw r4, r4+0
	mul r4, r4, r3
	addi r5, r0, 0
	add r3, r1, r0
	jal r31, fseek
.LBB3_11:
	lui r1, %hi(fio_ulen)
	addi r1, r1, %lo(fio_ulen)
	stw r1+0, r13
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end3:
	.size	f77_wu_begin, .Lfunc_end3-f77_wu_begin
                                        # -- End function
	.globl	f77_wu                          # -- Begin function f77_wu
	.p2align	2
	.type	f77_wu,@function
f77_wu:                                 # @f77_wu
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	addi r1, r0, 1
	blt r4, r1, .LBB4_4
.LBB4_1:
	add r11, r4, r0
	add r12, r3, r0
	lui r13, %hi(fio_ulen)
	addi r13, r13, %lo(fio_ulen)
	ldw r1, r13+0
	add r1, r1, r4
	lui r3, 16
	addi r3, r3, 1
	blt r1, r3, .LBB4_3
.LBB4_2:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB4_3:
	ldw r1, r13+0
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	add r3, r1, r3
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r13+0
	add r1, r1, r11
	stw r13+0, r1
.LBB4_4:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end4:
	.size	f77_wu, .Lfunc_end4-f77_wu
                                        # -- End function
	.globl	f77_wu_end                      # -- Begin function f77_wu_end
	.p2align	2
	.type	f77_wu_end,@function
f77_wu_end:                             # @f77_wu_end
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
	lui r1, %hi(fio_out)
	addi r1, r1, %lo(fio_out)
	ldw r1, r1+0
	addi r16, r0, 0
	seq r3, r1, r16
	lui r4, %hi(stdout)
	addi r4, r4, %lo(stdout)
	ldw r4, r4+0
	xor r4, r4, r1
	sub r3, r0, r3
	and r3, r4, r3
	xor r11, r1, r3
	lui r17, %hi(fio_unit)
	addi r17, r17, %lo(fio_unit)
	ldw r1, r17+0
	addi r3, r1, -1
	addi r4, r0, 30
	bgtu r3, r4, .LBB5_7
.LBB5_1:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r16, .LBB5_7
.LBB5_2:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r13, r1+0
	lui r14, %hi(fio_ulen)
	addi r14, r14, %lo(fio_ulen)
	ldw r1, r14+0
	ble r1, r13, .LBB5_4
.LBB5_3:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB5_4:
	ldw r5, r14+0
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	ldw r1, r14+0
	bge r1, r13, .LBB5_9
.LBB5_5:
	addi r12, r0, 0
.LBB5_6:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fputc
	ldw r1, r14+0
	addi r1, r1, 1
	stw r14+0, r1
	blt r1, r13, .LBB5_6
	jal r0, .LBB5_9
.LBB5_7:
	lui r1, %hi(fio_ulen)
	addi r1, r1, %lo(fio_ulen)
	ldw r12, r1+0
	stw sp+16, r12
	addi r13, sp, 16
	addi r14, r0, 1
	addi r15, r0, 4
	add r3, r13, r0
	add r4, r14, r0
	add r5, r15, r0
	add r6, r11, r0
	jal r31, fwrite
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	add r4, r14, r0
	add r5, r12, r0
	add r6, r11, r0
	jal r31, fwrite
	add r3, r13, r0
	add r4, r14, r0
	add r5, r15, r0
	add r6, r11, r0
	jal r31, fwrite
	ldw r1, r17+0
	slli r1, r1, 2
	lui r12, %hi(fio_uend)
	addi r12, r12, %lo(fio_uend)
	add r1, r1, r12
	ldw r1, r1+0
	blt r1, r16, .LBB5_9
.LBB5_8:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r17+0
	slli r3, r3, 2
	add r3, r3, r12
	stw r3+0, r1
.LBB5_9:
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
.Lfunc_end5:
	.size	f77_wu_end, .Lfunc_end5-f77_wu_end
                                        # -- End function
	.globl	f77_ru_begin                    # -- Begin function f77_ru_begin
	.p2align	2
	.type	f77_ru_begin,@function
f77_ru_begin:                           # @f77_ru_begin
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
	add r11, r4, r0
	add r14, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r4, r1+0
	addi r3, r0, 0
	bne r4, r3, .LBB6_4
.LBB6_1:
	addi r4, r0, -128
	lui r5, %hi(fio_uend+128)
	addi r5, r5, %lo(fio_uend+128)
	addi r6, r0, -1
.LBB6_2:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r3, .LBB6_2
.LBB6_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB6_4:
	addi r13, r0, 0
	add r3, r14, r0
	add r4, r13, r0
	jal r31, fio_resolve
	add r12, r1, r0
	lui r1, %hi(fio_in)
	addi r1, r1, %lo(fio_in)
	stw r1+0, r12
	lui r1, %hi(fio_unit)
	addi r1, r1, %lo(fio_unit)
	stw r1+0, r14
	addi r1, r14, -1
	addi r3, r0, 30
	bgtu r1, r3, .LBB6_14
.LBB6_5:
	slli r16, r14, 2
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r16, r1
	addi r3, r0, 1
	stw r1+0, r3
	lui r1, %hi(fio_rec_next)
	addi r1, r1, %lo(fio_rec_next)
	ldw r17, r1+0
	addi r15, r0, 0
	stw r1+0, r15
	beq r14, r15, .LBB6_10
.LBB6_6:
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r16, r1
	ldw r1, r1+0
	beq r1, r15, .LBB6_10
.LBB6_7:
	bgt r17, r15, .LBB6_9
.LBB6_8:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.57)
	addi r4, r4, %lo(.L.str.57)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB6_9:
	addi r1, r17, -1
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r16, r3
	ldw r3, r3+0
	mul r4, r3, r1
	addi r5, r0, 0
	add r3, r12, r0
	jal r31, fseek
.LBB6_10:
	lui r1, %hi(fio_upos)
	addi r1, r1, %lo(fio_upos)
	stw r1+0, r15
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r16, r1
	ldw r1, r1+0
	beq r1, r15, .LBB6_20
.LBB6_11:
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r16, r1
	ldw r5, r1+0
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	addi r4, r0, 1
	add r6, r12, r0
	jal r31, fread
	lui r3, %hi(fio_ulen)
	addi r3, r3, %lo(fio_ulen)
	stw r3+0, r1
	bne r1, r15, .LBB6_33
.LBB6_12:
	andi r1, r11, 2
	addi r15, r0, 0
	bne r1, r15, .LBB6_32
.LBB6_13:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB6_33
.LBB6_14:
	lui r1, %hi(fio_rec_next)
	addi r1, r1, %lo(fio_rec_next)
	ldw r15, r1+0
	stw r1+0, r13
	addi r1, r14, -32
	addi r3, r0, -31
	bltu r1, r3, .LBB6_19
.LBB6_15:
	slli r16, r14, 2
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r16, r1
	ldw r1, r1+0
	beq r1, r13, .LBB6_19
.LBB6_16:
	bgt r15, r13, .LBB6_18
.LBB6_17:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.57)
	addi r4, r4, %lo(.L.str.57)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB6_18:
	addi r1, r15, -1
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r16, r3
	ldw r3, r3+0
	mul r4, r3, r1
	addi r5, r0, 0
	add r3, r12, r0
	jal r31, fseek
.LBB6_19:
	lui r1, %hi(fio_upos)
	addi r1, r1, %lo(fio_upos)
	stw r1+0, r13
.LBB6_20:
	slli r16, r14, 2
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r14, r16, r1
	ldw r1, r14+0
	blt r1, r13, .LBB6_24
.LBB6_21:
	add r3, r12, r0
	jal r31, ftell
	ldw r3, r14+0
	blt r1, r3, .LBB6_24
.LBB6_22:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r16, r1
	addi r15, r0, 1
	stw r1+0, r15
	andi r1, r11, 3
	addi r3, r0, 0
	bne r1, r3, .LBB6_33
.LBB6_23:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB6_24:
	addi r3, sp, 16
	addi r15, r0, 1
	addi r14, r0, 4
	add r4, r15, r0
	add r5, r14, r0
	add r6, r12, r0
	jal r31, fread
	beq r1, r14, .LBB6_27
.LBB6_25:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r16, r1
	stw r1+0, r15
	andi r1, r11, 3
	bne r1, r13, .LBB6_33
.LBB6_26:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB6_27:
	ldhu r1, sp+16
	addi r3, sp, 16
	addi r4, r3, 2
	ldbu r4, r4+0
	slli r4, r4, 16
	or  r1, r1, r4
	addi r3, r3, 3
	ldbu r3, r3+0
	slli r3, r3, 24
	or  r5, r1, r3
	blt r3, r13, .LBB6_29
.LBB6_28:
	lui r1, 16
	addi r1, r1, 1
	blt r5, r1, .LBB6_31
.LBB6_29:
	andi r1, r11, 2
	addi r3, r0, 0
	bne r1, r3, .LBB6_32
.LBB6_30:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	add r11, r5, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r5, r11, r0
.LBB6_31:
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	addi r11, r0, 1
	add r4, r11, r0
	add r6, r12, r0
	jal r31, fread
	lui r3, %hi(fio_ulen)
	addi r3, r3, %lo(fio_ulen)
	stw r3+0, r1
	addi r3, sp, 16
	addi r5, r0, 4
	add r4, r11, r0
	add r6, r12, r0
	jal r31, fread
	add r15, r13, r0
	jal r0, .LBB6_33
.LBB6_32:
	addi r15, r0, 2
.LBB6_33:
	add r1, r15, r0
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
.Lfunc_end6:
	.size	f77_ru_begin, .Lfunc_end6-f77_ru_begin
                                        # -- End function
	.globl	f77_ru                          # -- Begin function f77_ru
	.p2align	2
	.type	f77_ru,@function
f77_ru:                                 # @f77_ru
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	addi r1, r0, 1
	blt r4, r1, .LBB7_4
.LBB7_1:
	add r11, r4, r0
	lui r12, %hi(fio_upos)
	addi r12, r12, %lo(fio_upos)
	ldw r1, r12+0
	add r1, r1, r4
	lui r4, %hi(fio_ulen)
	addi r4, r4, %lo(fio_ulen)
	ldw r4, r4+0
	ble r1, r4, .LBB7_3
.LBB7_2:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	add r13, r3, r0
	add r3, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r3, r13, r0
.LBB7_3:
	ldw r1, r12+0
	lui r4, %hi(fio_urec)
	addi r4, r4, %lo(fio_urec)
	add r4, r1, r4
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r12+0
	add r1, r1, r11
	stw r12+0, r1
.LBB7_4:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end7:
	.size	f77_ru, .Lfunc_end7-f77_ru
                                        # -- End function
	.globl	f77_ru_end                      # -- Begin function f77_ru_end
	.p2align	2
	.type	f77_ru_end,@function
f77_ru_end:                             # @f77_ru_end
# %bb.0:
	jalr r0, r31, 0
.Lfunc_end8:
	.size	f77_ru_end, .Lfunc_end8-f77_ru_end
                                        # -- End function
	.globl	f77_backspace                   # -- Begin function f77_backspace
	.p2align	2
	.type	f77_backspace,@function
f77_backspace:                          # @f77_backspace
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r4, r1+0
	addi r12, r0, 0
	bne r4, r12, .LBB9_4
.LBB9_1:
	addi r4, r0, -128
	lui r5, %hi(fio_uend+128)
	addi r5, r5, %lo(fio_uend+128)
	addi r6, r0, -1
.LBB9_2:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r12, .LBB9_2
.LBB9_3:
	addi r4, r0, 1
	stb r1+0, r4
.LBB9_4:
	addi r1, r3, -32
	addi r4, r0, -31
	bltu r1, r4, .LBB9_8
.LBB9_5:
	addi r1, r3, -5
	addi r4, r0, 2
	bltu r1, r4, .LBB9_8
.LBB9_6:
	addi r13, r0, 1
	add r14, r3, r0
	add r4, r13, r0
	jal r31, fio_resolve
	add r11, r1, r0
	add r3, r1, r0
	jal r31, fflush
	add r3, r11, r0
	jal r31, ftell
	slli r3, r14, 2
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r4, r3, r4
	ldw r5, r4+0
	beq r5, r12, .LBB9_9
.LBB9_7:
	stw r4+0, r12
.LBB9_8:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB9_9:
	blt r1, r13, .LBB9_8
.LBB9_10:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r12, .LBB9_14
.LBB9_11:
	add r14, r1, r0
	addi r4, r1, -4
	add r3, r11, r0
	add r5, r12, r0
	jal r31, fseek
	addi r3, sp, 20
	addi r4, r0, 1
	addi r13, r0, 4
	add r5, r13, r0
	add r6, r11, r0
	jal r31, fread
	bne r1, r13, .LBB9_13
.LBB9_12:
	ldbu r1, sp+20
	addi r3, sp, 20
	addi r4, r3, 1
	ldbu r4, r4+0
	slli r4, r4, 8
	addi r5, r3, 2
	ldbu r5, r5+0
	slli r5, r5, 16
	or  r1, r5, r1
	or  r1, r4, r1
	addi r3, r3, 3
	ldbu r3, r3+0
	sub r3, r0, r3
	slli r3, r3, 24
	sub r1, r3, r1
	add r1, r14, r1
	addi r12, r1, -8
.LBB9_13:
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fseek
	jal r0, .LBB9_8
.LBB9_14:
	addi r12, r1, -1
	addi r14, r0, 0
	addi r16, r0, 10
.LBB9_15:
	blt r12, r13, .LBB9_17
.LBB9_16:
	addi r15, r12, -1
	add r3, r11, r0
	add r4, r15, r0
	add r5, r14, r0
	jal r31, fseek
	add r3, r11, r0
	jal r31, fgetc
	seq r3, r1, r16
	xor r4, r12, r15
	sub r3, r0, r3
	and r3, r4, r3
	xor r12, r15, r3
	bne r1, r16, .LBB9_15
	jal r0, .LBB9_18
.LBB9_17:
	add r12, r14, r0
.LBB9_18:
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r12, r0
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, fseek
.Lfunc_end9:
	.size	f77_backspace, .Lfunc_end9-f77_backspace
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
	bne r8, r1, .LBB10_4
.LBB10_1:
	addi r8, r0, -128
	lui r9, %hi(fio_uend+128)
	addi r9, r9, %lo(fio_uend+128)
	addi r10, r0, -1
.LBB10_2:
	add lr, r8, r9
	stw lr+0, r10
	addi r8, r8, 4
	bne r8, r1, .LBB10_2
.LBB10_3:
	addi r8, r0, 1
	stb r7+0, r8
.LBB10_4:
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
	lui r3, %hi(fio_col)
	addi r3, r3, %lo(fio_col)
	stw r3+0, r1
	lui r3, %hi(fio_sp)
	addi r3, r3, %lo(fio_sp)
	stb r3+0, r1
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
	beq r6, r1, .LBB10_10
.LBB10_5:
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
.LBB10_6:
	add r7, r6, r1
	ldbu r7, r7+0
	bne r7, r5, .LBB10_8
.LBB10_7:
	addi r1, r1, 1
	stw r3+0, r1
	jal r0, .LBB10_6
.LBB10_8:
	addi r5, r0, 40
	bne r7, r5, .LBB10_11
.LBB10_9:
	stw r4+0, r1
	addi r1, r1, 1
	stw r3+0, r1
	jal r0, .LBB10_11
.LBB10_10:
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	stw r1+0, r3
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
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
.LBB10_11:
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end10:
	.size	f77_wr_begin_int, .Lfunc_end10-f77_wr_begin_int
                                        # -- End function
	.globl	f77_wr_i                        # -- Begin function f77_wr_i
	.p2align	2
	.type	f77_wr_i,@function
f77_wr_i:                               # @f77_wr_i
# %bb.0:
	addi sp, sp, -128
	stw sp+0, lr
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
	add r12, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r11, r0, 0
	beq r1, r11, .LBB11_13
.LBB11_1:
	lui r5, %hi(.L.str.6)
	addi r5, r5, %lo(.L.str.6)
	addi r3, sp, 56
	addi r13, r0, 32
	add r4, r13, r0
	add r6, r12, r0
	jal r31, snprintf
	ldbu r9, sp+56
	beq r9, r11, .LBB11_11
.LBB11_2:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r7, r1+0
	lui r3, %hi(fio_col)
	addi r3, r3, %lo(fio_col)
	ldw r8, r3+0
	addi r4, sp, 56
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	jal r0, .LBB11_5
.LBB11_3:
	add r8, r9, r0
.LBB11_4:
	addi r4, r4, 1
	ldbu r9, r4+0
	beq r9, r11, .LBB11_11
.LBB11_5:
	bgt r8, r5, .LBB11_4
.LBB11_6:
	bge r7, r8, .LBB11_9
.LBB11_7:
	addi r10, r7, 1
	add r7, r7, r6
	stb r7+0, r13
	add r7, r10, r0
	bne r8, r10, .LBB11_7
.LBB11_8:
	stw r1+0, r8
	add r7, r8, r0
.LBB11_9:
	add r10, r8, r6
	stb r10+0, r9
	addi r9, r8, 1
	stw r3+0, r9
	blt r8, r7, .LBB11_3
.LBB11_10:
	stw r1+0, r9
	add r7, r9, r0
	jal r0, .LBB11_3
.LBB11_11:
	lui r1, %hi(fio_first_item)
	addi r1, r1, %lo(fio_first_item)
	stb r1+0, r11
	lui r1, %hi(fio_prev_char)
	addi r1, r1, %lo(fio_prev_char)
	stb r1+0, r11
.LBB11_12:
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
	addi sp, sp, 128
	jalr r0, r31, 0
.LBB11_13:
	lui r16, %hi(fio_desc)
	addi r16, r16, %lo(fio_desc)
	ldw r1, r16+0
	lui r17, %hi(fio_rep)
	addi r17, r17, %lo(fio_rep)
	beq r1, r11, .LBB11_15
.LBB11_14:
	ldw r3, r17+0
	bgt r3, r11, .LBB11_16
.LBB11_15:
	stw r16+0, r11
	jal r31, fio_next_desc
.LBB11_16:
	beq r1, r11, .LBB11_12
.LBB11_17:
	addi r3, r0, 76
	bne r1, r3, .LBB11_31
.LBB11_18:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r3, r0, 2
	blt r1, r3, .LBB11_20
.LBB11_19:
	lui r3, %hi(fio_col)
	addi r3, r3, %lo(fio_col)
	ldw r4, r3+0
	add r1, r1, r4
	addi r1, r1, -1
	stw r3+0, r1
.LBB11_20:
	lui r3, %hi(fio_col)
	addi r3, r3, %lo(fio_col)
	ldw r1, r3+0
	addi r4, r0, 1022
	bgt r1, r4, .LBB11_27
.LBB11_21:
	lui r4, %hi(fio_len)
	addi r4, r4, %lo(fio_len)
	ldw r5, r4+0
	bge r5, r1, .LBB11_25
.LBB11_22:
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
.LBB11_23:
	addi r8, r5, 1
	add r5, r5, r6
	stb r5+0, r7
	add r5, r8, r0
	bne r1, r8, .LBB11_23
.LBB11_24:
	stw r4+0, r1
	add r5, r1, r0
.LBB11_25:
	seq r6, r12, r11
	sub r6, r0, r6
	andi r6, r6, 18
	xori r6, r6, 84
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	add r7, r1, r7
	stb r7+0, r6
	addi r6, r1, 1
	stw r3+0, r6
	blt r1, r5, .LBB11_27
.LBB11_26:
	stw r4+0, r6
.LBB11_27:
	ldw r1, r17+0
	addi r3, r0, 1
	blt r1, r3, .LBB11_29
.LBB11_28:
	addi r1, r1, -1
	stw r17+0, r1
.LBB11_29:
	ldw r1, r17+0
	bne r1, r11, .LBB11_12
.LBB11_30:
	stw r16+0, r11
	jal r0, .LBB11_12
.LBB11_31:
	lui r19, %hi(fio_d)
	addi r19, r19, %lo(fio_d)
	ldw r1, r19+0
	addi r18, r0, 1
	blt r1, r18, .LBB11_34
.LBB11_32:
	slt r20, r12, r11
	srai r1, r12, 31
	xor r3, r12, r1
	sub r6, r3, r1
	lui r5, %hi(.L.str.7)
	addi r5, r5, %lo(.L.str.7)
	addi r13, sp, 24
	addi r14, r0, 32
	add r3, r13, r0
	add r4, r14, r0
	jal r31, snprintf
	add r3, r13, r0
	jal r31, strlen
	add r15, r1, r0
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
	lui r3, %hi(.L.str.9)
	addi r3, r3, %lo(.L.str.9)
	xor r3, r3, r1
	sub r4, r0, r20
	and r3, r3, r4
	xor r12, r3, r1
	ldw r1, r19+0
	sgt r3, r1, r15
	sub r1, r1, r15
	sub r3, r0, r3
	and r7, r1, r3
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	addi r3, sp, 56
	add r4, r14, r0
	add r6, r12, r0
	add r8, r11, r0
	add r9, r13, r0
	jal r31, snprintf
	ldw r1, r19+0
	blt r15, r1, .LBB11_40
.LBB11_33:
	lui r5, %hi(.L.str.11)
	addi r5, r5, %lo(.L.str.11)
	addi r3, sp, 56
	addi r4, r0, 32
	addi r7, sp, 24
	jal r0, .LBB11_39
.LBB11_34:
	lui r1, %hi(fio_fmt)
	addi r1, r1, %lo(fio_fmt)
	ldw r1, r1+0
	lui r3, %hi(fio_pos)
	addi r3, r3, %lo(fio_pos)
	ldw r3, r3+0
	add r1, r1, r3
	ldbu r3, r1+-1
	addi r4, r0, 48
	bne r3, r4, .LBB11_38
.LBB11_35:
	bne r12, r11, .LBB11_38
.LBB11_36:
	ldbu r1, r1+-2
	andi r1, r1, 255
	addi r3, r0, 46
	bne r1, r3, .LBB11_38
.LBB11_37:
	stb sp+56, r11
	jal r0, .LBB11_40
.LBB11_38:
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r3, sp, 56
	addi r4, r0, 32
.LBB11_39:
	add r6, r12, r0
	jal r31, snprintf
.LBB11_40:
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	ldbu r1, r1+0
	bne r1, r18, .LBB11_45
.LBB11_41:
	ldbu r1, sp+56
	beq r1, r11, .LBB11_45
.LBB11_42:
	addi r14, r0, 43
	beq r1, r14, .LBB11_45
.LBB11_43:
	addi r3, r0, 45
	beq r1, r3, .LBB11_45
.LBB11_44:
	addi r12, sp, 56
	addi r13, r12, 1
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, memmove
	stb sp+56, r14
.LBB11_45:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	addi r3, sp, 56
	jal r31, fio_field
	ldw r1, r17+0
	bge r1, r18, .LBB11_28
	jal r0, .LBB11_29
.Lfunc_end11:
	.size	f77_wr_i, .Lfunc_end11-f77_wr_i
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
	ble r12, r3, .LBB12_11
.LBB12_1:
	bge r12, r1, .LBB12_21
.LBB12_2:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r8, r1+0
	lui r3, %hi(fio_col)
	addi r3, r3, %lo(fio_col)
	ldw r10, r3+0
	addi r4, r0, 1022
	addi r5, r0, 1
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	addi r9, r0, 42
	jal r0, .LBB12_5
.LBB12_3:
	add r10, r11, r0
.LBB12_4:
	addi r12, lr, -1
	ble lr, r5, .LBB12_33
.LBB12_5:
	add lr, r12, r0
	bgt r10, r4, .LBB12_4
.LBB12_6:
	bge r8, r10, .LBB12_9
.LBB12_7:
	addi r11, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, r11, r0
	bne r10, r11, .LBB12_7
.LBB12_8:
	stw r1+0, r10
	add r8, r10, r0
.LBB12_9:
	add r11, r10, r6
	stb r11+0, r9
	addi r11, r10, 1
	stw r3+0, r11
	blt r10, r8, .LBB12_3
.LBB12_10:
	stw r1+0, r11
	add r8, r11, r0
	jal r0, .LBB12_3
.LBB12_11:
	ldbu r10, r11+0
	beq r10, r3, .LBB12_33
.LBB12_12:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r8, r1+0
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r9, r4+0
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	jal r0, .LBB12_15
.LBB12_13:
	add r9, r10, r0
.LBB12_14:
	addi r11, r11, 1
	ldbu r10, r11+0
	beq r10, r3, .LBB12_33
.LBB12_15:
	bgt r9, r5, .LBB12_14
.LBB12_16:
	bge r8, r9, .LBB12_19
.LBB12_17:
	addi lr, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, lr, r0
	bne r9, lr, .LBB12_17
.LBB12_18:
	stw r1+0, r9
	add r8, r9, r0
.LBB12_19:
	add lr, r9, r6
	stb lr+0, r10
	addi r10, r9, 1
	stw r4+0, r10
	blt r9, r8, .LBB12_13
.LBB12_20:
	stw r1+0, r10
	add r8, r10, r0
	jal r0, .LBB12_13
.LBB12_21:
	sub r4, r12, r1
	addi r5, r0, 1
	lui r1, %hi(fio_col)
	addi r1, r1, %lo(fio_col)
	blt r4, r5, .LBB12_23
.LBB12_22:
	ldw r5, r1+0
	add r4, r5, r4
	stw r1+0, r4
.LBB12_23:
	ldbu r10, r11+0
	beq r10, r3, .LBB12_33
.LBB12_24:
	lui r4, %hi(fio_len)
	addi r4, r4, %lo(fio_len)
	ldw r8, r4+0
	ldw r9, r1+0
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	jal r0, .LBB12_27
.LBB12_25:
	add r9, r10, r0
.LBB12_26:
	addi r11, r11, 1
	ldbu r10, r11+0
	beq r10, r3, .LBB12_33
.LBB12_27:
	bgt r9, r5, .LBB12_26
.LBB12_28:
	bge r8, r9, .LBB12_31
.LBB12_29:
	addi lr, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, lr, r0
	bne r9, lr, .LBB12_29
.LBB12_30:
	stw r4+0, r9
	add r8, r9, r0
.LBB12_31:
	add lr, r9, r6
	stb lr+0, r10
	addi r10, r9, 1
	stw r1+0, r10
	blt r9, r8, .LBB12_25
.LBB12_32:
	stw r4+0, r10
	add r8, r10, r0
	jal r0, .LBB12_25
.LBB12_33:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end12:
	.size	fio_field, .Lfunc_end12-fio_field
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_wr_d
	.type	.LCPI13_0,@object
.LCPI13_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI13_0, 8
	.type	.LCPI13_1,@object
.LCPI13_1:
	.quad	0x4024000000000000              # double 10
	.size	.LCPI13_1, 8
	.type	.LCPI13_2,@object
.LCPI13_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI13_2, 8
	.type	.LCPI13_3,@object
.LCPI13_3:
	.quad	0x3fb999999999999a              # double 0.10000000000000001
	.size	.LCPI13_3, 8
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
	beq r1, r18, .LBB13_2
.LBB13_1:
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
.LBB13_2:
	lui r14, %hi(fio_desc)
	addi r14, r14, %lo(fio_desc)
	ldw r1, r14+0
	lui r15, %hi(fio_rep)
	addi r15, r15, %lo(fio_rep)
	beq r1, r18, .LBB13_22
.LBB13_3:
	ldw r3, r15+0
	addi r4, r0, 0
	ble r3, r4, .LBB13_22
.LBB13_4:
	addi r3, r0, 70
	bgt r1, r3, .LBB13_23
.LBB13_5:
	addi r17, r0, 0
	beq r1, r17, .LBB13_107
.LBB13_6:
	bne r1, r3, .LBB13_26
.LBB13_7:
	lui r1, %hi(fio_scale)
	addi r1, r1, %lo(fio_scale)
	ldw r1, r1+0
	addi r16, r0, 1
	blt r1, r16, .LBB13_11
.LBB13_8:
	addi r1, r1, 1
	lui r3, %hi(.LCPI13_1)
	addi r3, r3, %lo(.LCPI13_1)
	ldw r5, r3+4
	ldw r4, r3+0
.LBB13_9:
	fmul.d r12, r12, r4
	addi r1, r1, -1
	bgt r1, r16, .LBB13_9
.LBB13_10:
	add r1, r17, r0
.LBB13_11:
	addi r3, r0, -1
	bgt r1, r3, .LBB13_14
.LBB13_12:
	lui r3, %hi(.LCPI13_1)
	addi r3, r3, %lo(.LCPI13_1)
	ldw r5, r3+4
	ldw r4, r3+0
.LBB13_13:
	fdiv.d r12, r12, r4
	addi r3, r1, 1
	sltu r6, r3, r1
	add r1, r3, r0
	bne r6, r16, .LBB13_13
.LBB13_14:
	lui r18, %hi(fio_d)
	addi r18, r18, %lo(fio_d)
	ldw r6, r18+0
	lui r5, %hi(.L.str.13)
	addi r5, r5, %lo(.L.str.13)
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
	bne r1, r17, .LBB13_16
.LBB13_15:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	addi r3, sp, 44
	jal r31, strcat
.LBB13_16:
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	ldbu r1, r1+0
	bne r1, r16, .LBB13_21
.LBB13_17:
	ldbu r1, sp+44
	beq r1, r17, .LBB13_21
.LBB13_18:
	addi r13, r0, 43
	beq r1, r13, .LBB13_21
.LBB13_19:
	addi r3, r0, 45
	beq r1, r3, .LBB13_21
.LBB13_20:
	addi r11, sp, 44
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+44, r13
.LBB13_21:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r11, r1+0
	addi r3, sp, 44
	jal r31, strlen
	bge r11, r16, .LBB13_33
	jal r0, .LBB13_103
.LBB13_22:
	stw r14+0, r18
	jal r31, fio_next_desc
	addi r3, r0, 70
	ble r1, r3, .LBB13_5
.LBB13_23:
	addi r3, r0, 71
	beq r1, r3, .LBB13_38
.LBB13_24:
	addi r3, r0, 73
	bne r1, r3, .LBB13_26
.LBB13_25:
	fcvt.w.d r6, r12
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r3, sp, 44
	addi r4, r0, 64
	jal r31, snprintf
	jal r0, .LBB13_103
.LBB13_26:
	addi r3, r0, -2
	and r3, r1, r3
	addi r4, r0, 68
	bne r3, r4, .LBB13_56
.LBB13_27:
	lui r3, %hi(fio_d)
	addi r3, r3, %lo(fio_d)
	ldw r4, r3+0
	addi r3, sp, 44
	add r5, r12, r0
	add r6, r13, r0
	add r7, r1, r0
	jal r31, fio_efmt
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	ldbu r1, r1+0
	addi r13, r0, 1
	bne r1, r13, .LBB13_32
.LBB13_28:
	ldbu r1, sp+44
	addi r3, r0, 0
	beq r1, r3, .LBB13_32
.LBB13_29:
	addi r16, r0, 43
	beq r1, r16, .LBB13_32
.LBB13_30:
	addi r3, r0, 45
	beq r1, r3, .LBB13_32
.LBB13_31:
	addi r11, sp, 44
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+44, r16
.LBB13_32:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r11, r1+0
	addi r3, sp, 44
	jal r31, strlen
	blt r11, r13, .LBB13_103
.LBB13_33:
	addi r3, r11, 1
	bne r1, r3, .LBB13_103
.LBB13_34:
	ldbu r3, sp+44
	addi r4, r0, 45
	beq r3, r4, .LBB13_60
.LBB13_35:
	addi r4, r0, 48
	bne r3, r4, .LBB13_103
.LBB13_36:
	addi r3, sp, 44
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB13_103
.LBB13_37:
	addi r3, sp, 44
	add r5, r1, r0
	jal r31, memmove
	jal r0, .LBB13_103
.LBB13_38:
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r1, r1+0
	addi r16, r0, 1
	sgt r3, r1, r16
	sub r3, r0, r3
	xori r4, r1, 1
	and r3, r4, r3
	xori r10, r3, 1
	lui r4, %hi(.LCPI13_0)
	addi r4, r4, %lo(.LCPI13_0)
	ldw r9, r4+4
	ldw r8, r4+0
	lui r4, %hi(.LCPI13_1)
	addi r4, r4, %lo(.LCPI13_1)
	ldw r7, r4+4
	ldw r6, r4+0
	add r4, r8, r0
	add r5, r9, r0
.LBB13_39:
	fmul.d r4, r4, r6
	addi r10, r10, -1
	bne r10, r18, .LBB13_39
.LBB13_40:
	lui r10, %hi(.LCPI13_2)
	addi r10, r10, %lo(.LCPI13_2)
	ldw r21, r10+4
	ldw r20, r10+0
	flt.d r10, r12, r20
	fneg.d r22, r12
	xor lr, r23, r13
	sub r17, r0, r10
	and r10, lr, r17
	xor r11, r13, r10
	xor lr, r22, r12
	and lr, lr, r17
	xor r10, r12, lr
	feq.d lr, r12, r20
	bne lr, r18, .LBB13_43
.LBB13_41:
	lui lr, %hi(.LCPI13_3)
	addi lr, lr, %lo(.LCPI13_3)
	ldw r21, lr+4
	ldw r20, lr+0
	fle.d lr, r20, r10
	xori lr, lr, 1
	bne lr, r18, .LBB13_44
.LBB13_42:
	flt.d r4, r10, r4
	xori r4, r4, 1
	bne r4, r18, .LBB13_44
.LBB13_43:
	addi r16, r0, 0
.LBB13_44:
	xori r4, r3, 1
	lui r17, %hi(fio_w)
	addi r17, r17, %lo(fio_w)
	beq r16, r18, .LBB13_57
.LBB13_45:
	addi r3, sp, 44
	addi r7, r0, 69
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fio_efmt
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	ldbu r1, r1+0
	addi r13, r0, 1
	bne r1, r13, .LBB13_50
.LBB13_46:
	ldbu r1, sp+44
	addi r3, r0, 0
	beq r1, r3, .LBB13_50
.LBB13_47:
	addi r18, r0, 43
	beq r1, r18, .LBB13_50
.LBB13_48:
	addi r3, r0, 45
	beq r1, r3, .LBB13_50
.LBB13_49:
	addi r11, sp, 44
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+44, r18
.LBB13_50:
	ldw r11, r17+0
	addi r3, sp, 44
	jal r31, strlen
	blt r11, r13, .LBB13_102
.LBB13_51:
	addi r3, r11, 1
	bne r1, r3, .LBB13_102
.LBB13_52:
	ldbu r3, sp+44
	addi r4, r0, 45
	beq r3, r4, .LBB13_80
.LBB13_53:
	addi r4, r0, 48
	bne r3, r4, .LBB13_102
.LBB13_54:
	addi r3, sp, 44
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB13_102
.LBB13_55:
	addi r3, sp, 44
	add r5, r1, r0
	jal r0, .LBB13_83
.LBB13_56:
	lui r5, %hi(.L.str.15)
	addi r5, r5, %lo(.L.str.15)
	addi r3, sp, 44
	addi r4, r0, 64
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	jal r0, .LBB13_103
.LBB13_57:
	lui r3, %hi(fio_e)
	addi r3, r3, %lo(fio_e)
	ldw r3, r3+0
	sgt r5, r3, r18
	addi r3, r3, 2
	sub r5, r0, r5
	xori r3, r3, 4
	and r3, r3, r5
	xori r19, r3, 4
	ldw r20, r17+0
	slt r3, r20, r19
	sub r3, r0, r3
	and r3, r19, r3
	fle.d r5, r8, r10
	xori r8, r5, 1
	add r5, r18, r0
	bne r8, r18, .LBB13_67
.LBB13_58:
	fle.d r5, r6, r10
	xori r8, r5, 1
	addi r5, r0, 0
	beq r8, r5, .LBB13_63
.LBB13_59:
	addi r5, r0, -1
	jal r0, .LBB13_67
.LBB13_60:
	addi r3, sp, 44
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB13_103
.LBB13_61:
	addi r4, sp, 44
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB13_103
.LBB13_62:
	addi r5, r1, -1
	jal r31, memmove
	jal r0, .LBB13_103
.LBB13_63:
	addi r8, r0, 1
.LBB13_64:
	add r9, r8, r0
	fdiv.d r10, r10, r6
	fle.d r8, r6, r10
	xori lr, r8, 1
	addi r8, r9, 1
	bne lr, r5, .LBB13_66
.LBB13_65:
	bgt r1, r9, .LBB13_64
.LBB13_66:
	sub r5, r0, r8
.LBB13_67:
	xor r21, r19, r3
	add r22, r5, r4
	sgt r1, r22, r18
	sub r1, r0, r1
	and r6, r22, r1
	lui r5, %hi(.L.str.13)
	addi r5, r5, %lo(.L.str.13)
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
	bgt r22, r18, .LBB13_69
.LBB13_68:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	addi r3, sp, 44
	jal r31, strcat
.LBB13_69:
	sub r11, r20, r21
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	ldbu r1, r1+0
	addi r18, r0, 1
	bne r1, r18, .LBB13_74
.LBB13_70:
	ldbu r1, sp+44
	addi r3, r0, 0
	beq r1, r3, .LBB13_74
.LBB13_71:
	addi r20, r0, 43
	beq r1, r20, .LBB13_74
.LBB13_72:
	addi r3, r0, 45
	beq r1, r3, .LBB13_74
.LBB13_73:
	addi r12, sp, 44
	addi r13, r12, 1
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, memmove
	stb sp+44, r20
.LBB13_74:
	addi r3, sp, 44
	jal r31, strlen
	blt r11, r18, .LBB13_88
.LBB13_75:
	addi r3, r11, 1
	bne r1, r3, .LBB13_88
.LBB13_76:
	ldbu r3, sp+44
	addi r4, r0, 45
	beq r3, r4, .LBB13_84
.LBB13_77:
	addi r4, r0, 48
	bne r3, r4, .LBB13_88
.LBB13_78:
	addi r3, sp, 44
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB13_88
.LBB13_79:
	addi r3, sp, 44
	add r5, r1, r0
	jal r0, .LBB13_87
.LBB13_80:
	addi r3, sp, 44
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB13_102
.LBB13_81:
	addi r4, sp, 44
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB13_102
.LBB13_82:
	addi r5, r1, -1
.LBB13_83:
	jal r31, memmove
	jal r0, .LBB13_102
.LBB13_84:
	addi r3, sp, 44
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB13_88
.LBB13_85:
	addi r4, sp, 44
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB13_88
.LBB13_86:
	addi r5, r1, -1
.LBB13_87:
	jal r31, memmove
.LBB13_88:
	addi r3, sp, 44
	add r4, r11, r0
	jal r31, fio_field
	ldw r1, r17+0
	slt r1, r1, r19
	sub r1, r0, r1
	and r1, r19, r1
	xor r10, r19, r1
	blt r10, r18, .LBB13_98
.LBB13_89:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r7, r1+0
	lui r3, %hi(fio_col)
	addi r3, r3, %lo(fio_col)
	ldw r8, r3+0
	addi r4, r0, 1022
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	addi r6, r0, 32
	jal r0, .LBB13_92
.LBB13_90:
	add r8, r10, r0
.LBB13_91:
	addi r10, r9, -1
	ble r9, r18, .LBB13_98
.LBB13_92:
	add r9, r10, r0
	bgt r8, r4, .LBB13_91
.LBB13_93:
	bge r7, r8, .LBB13_96
.LBB13_94:
	addi r10, r7, 1
	add r7, r7, r5
	stb r7+0, r6
	add r7, r10, r0
	bne r8, r10, .LBB13_94
.LBB13_95:
	stw r1+0, r8
	add r7, r8, r0
.LBB13_96:
	add r10, r8, r5
	stb r10+0, r6
	addi r10, r8, 1
	stw r3+0, r10
	blt r8, r7, .LBB13_90
.LBB13_97:
	stw r1+0, r10
	add r7, r10, r0
	jal r0, .LBB13_90
.LBB13_98:
	ldw r1, r15+0
	blt r1, r18, .LBB13_100
.LBB13_99:
	addi r1, r1, -1
	stw r15+0, r1
.LBB13_100:
	ldw r3, r15+0
	addi r1, r0, 0
	bne r3, r1, .LBB13_102
.LBB13_101:
	stw r14+0, r1
.LBB13_102:
	addi r1, r0, 0
	beq r16, r1, .LBB13_107
.LBB13_103:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	addi r3, sp, 44
	jal r31, fio_field
	ldw r1, r15+0
	addi r3, r0, 1
	blt r1, r3, .LBB13_105
.LBB13_104:
	addi r1, r1, -1
	stw r15+0, r1
.LBB13_105:
	ldw r3, r15+0
	addi r1, r0, 0
	bne r3, r1, .LBB13_107
.LBB13_106:
	stw r14+0, r1
.LBB13_107:
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
.Lfunc_end13:
	.size	f77_wr_d, .Lfunc_end13-f77_wr_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_list_out_real
	.type	.LCPI14_0,@object
.LCPI14_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI14_0, 8
	.type	.LCPI14_2,@object
.LCPI14_2:
	.quad	0x3fb999999999999a              # double 0.10000000000000001
	.size	.LCPI14_2, 8
	.type	.LCPI14_3,@object
.LCPI14_3:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI14_3, 8
	.type	.LCPI14_4,@object
.LCPI14_4:
	.quad	0x4024000000000000              # double 10
	.size	.LCPI14_4, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI14_1,@object
.LCPI14_1:
	.quad	0x41cdcd6500000000              # double 1.0E+9
	.quad	0x4376345785d8a000              # double 1.0E+17
	.size	.LCPI14_1, 16
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
	add r11, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	lui r1, %hi(.LCPI14_0)
	addi r1, r1, %lo(.LCPI14_0)
	ldw r19, r1+4
	ldw r18, r1+0
	flt.d r1, r12, r18
	fneg.d r6, r12
	xor r3, r7, r13
	sub r1, r0, r1
	and r3, r3, r1
	xor r5, r13, r3
	xor r3, r6, r12
	and r1, r3, r1
	xor r4, r12, r1
	addi r17, r0, 0
	sne r1, r11, r17
	sub r3, r0, r1
	andi r6, r3, 24
	lui r15, %hi(fio_col)
	addi r15, r15, %lo(fio_col)
	ldw r7, r15+0
	addi r8, r0, 1022
	bgt r7, r8, .LBB14_6
.LBB14_1:
	lui r8, %hi(fio_len)
	addi r8, r8, %lo(fio_len)
	ldw r9, r8+0
	lui r10, %hi(fio_line)
	addi r10, r10, %lo(fio_line)
	addi lr, r0, 32
	bge r9, r7, .LBB14_4
.LBB14_2:
	addi r14, r9, 1
	add r9, r9, r10
	stb r9+0, lr
	add r9, r14, r0
	bne r7, r14, .LBB14_2
.LBB14_3:
	stw r8+0, r7
	add r9, r7, r0
.LBB14_4:
	add r10, r7, r10
	stb r10+0, lr
	addi r10, r7, 1
	stw r15+0, r10
	blt r7, r9, .LBB14_6
.LBB14_5:
	stw r8+0, r10
.LBB14_6:
	xori r16, r6, 9
	feq.d r6, r12, r18
	bne r6, r17, .LBB14_9
.LBB14_7:
	lui r6, %hi(.LCPI14_2)
	addi r6, r6, %lo(.LCPI14_2)
	ldw r9, r6+4
	ldw r8, r6+0
	fle.d r6, r8, r4
	xori r6, r6, 1
	bne r6, r17, .LBB14_17
.LBB14_8:
	andi r6, r3, 8
	lui r7, %hi(.LCPI14_1)
	addi r7, r7, %lo(.LCPI14_1)
	add r6, r7, r6
	ldw r9, r6+4
	ldw r8, r6+0
	flt.d r6, r4, r8
	xori r6, r6, 1
	bne r6, r17, .LBB14_17
.LBB14_9:
	lui r3, %hi(.LCPI14_3)
	addi r3, r3, %lo(.LCPI14_3)
	ldw r7, r3+4
	ldw r6, r3+0
	fle.d r3, r6, r4
	xori r8, r3, 1
	addi r21, r0, 1
	add r3, r21, r0
	bne r8, r17, .LBB14_12
.LBB14_10:
	lui r3, %hi(.LCPI14_4)
	addi r3, r3, %lo(.LCPI14_4)
	ldw r9, r3+4
	ldw r8, r3+0
	add r22, r4, r0
	add r23, r5, r0
	add r3, r17, r0
.LBB14_11:
	fdiv.d r22, r22, r8
	addi r3, r3, 1
	fle.d r10, r6, r22
	bne r10, r17, .LBB14_11
.LBB14_12:
	addi r20, r1, 4
	flt.d r1, r4, r6
	feq.d r4, r12, r18
	xori r4, r4, 1
	and r22, r4, r1
	sub r1, r0, r22
	and r1, r3, r1
	xor r1, r3, r1
	sub r6, r16, r1
	lui r5, %hi(.L.str.13)
	addi r5, r5, %lo(.L.str.13)
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
	bne r22, r21, .LBB14_15
.LBB14_13:
	flt.d r12, r12, r18
	addi r3, sp, 44
	jal r31, strlen
	add r3, r12, r16
	addi r3, r3, 2
	bleu r1, r3, .LBB14_15
.LBB14_14:
	addi r1, sp, 44
	add r1, r1, r3
	addi r3, r0, 0
	stb r1+0, r3
.LBB14_15:
	addi r1, r0, 0
	sne r1, r11, r1
	sub r1, r0, r1
	andi r1, r1, 24
	xori r4, r1, 12
	addi r3, sp, 44
	jal r31, fio_field
	ldw r1, r15+0
	add r1, r1, r20
	stw r15+0, r1
.LBB14_16:
	lui r1, %hi(fio_first_item)
	addi r1, r1, %lo(fio_first_item)
	stb r1+0, r17
	lui r1, %hi(fio_prev_char)
	addi r1, r1, %lo(fio_prev_char)
	stb r1+0, r17
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
.LBB14_17:
	andi r1, r3, 9
	addi r14, r1, 16
	addi r6, r16, -1
	lui r5, %hi(.L.str.58)
	addi r5, r5, %lo(.L.str.58)
	addi r15, sp, 28
	addi r4, r0, 16
	add r3, r15, r0
	jal r31, snprintf
	addi r16, sp, 44
	addi r4, r0, 64
	add r3, r16, r0
	add r5, r15, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	addi r4, r0, 69
	add r3, r16, r0
	jal r31, strchr
	beq r11, r17, .LBB14_21
.LBB14_18:
	beq r1, r17, .LBB14_21
.LBB14_19:
	addi r11, r1, 2
	add r3, r11, r0
	jal r31, strlen
	addi r3, r0, 2
	bgt r1, r3, .LBB14_21
.LBB14_20:
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
.LBB14_21:
	addi r3, sp, 44
	add r4, r14, r0
	jal r31, fio_field
	jal r0, .LBB14_16
.Lfunc_end14:
	.size	fio_list_out_real, .Lfunc_end14-fio_list_out_real
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
	lui r5, %hi(.L.str.58)
	addi r5, r5, %lo(.L.str.58)
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
	jal r0, .LBB15_3
.LBB15_1:
	addi r7, sp, 56
	add r7, r7, r13
	stb r7+40, r6
	addi r13, r13, 1
.LBB15_2:
	addi r3, r3, 1
.LBB15_3:
	ldbu r6, r3+-1
	bgt r6, r1, .LBB15_6
.LBB15_4:
	beq r6, r14, .LBB15_2
.LBB15_5:
	bne r6, r20, .LBB15_1
	jal r0, .LBB15_9
.LBB15_6:
	beq r6, r4, .LBB15_8
.LBB15_7:
	bne r6, r5, .LBB15_1
.LBB15_8:
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	addi r1, r1, 1
	addi r4, r0, 0
	bne r13, r4, .LBB15_10
	jal r0, .LBB15_12
.LBB15_9:
	add r1, r17, r0
	addi r4, r0, 0
	beq r13, r4, .LBB15_12
.LBB15_10:
	bne r13, r17, .LBB15_13
.LBB15_11:
	ldbu r3, sp+96
	andi r3, r3, 255
	addi r5, r0, 48
	bne r3, r5, .LBB15_13
.LBB15_12:
	addi r1, r0, 0
.LBB15_13:
	add r5, r12, r0
	bne r19, r15, .LBB15_15
.LBB15_14:
	addi r5, r12, 1
	stb r12+0, r15
.LBB15_15:
	blt r18, r17, .LBB15_20
.LBB15_16:
	blt r13, r17, .LBB15_25
.LBB15_17:
	xor r3, r18, r13
	slt r4, r18, r13
	sub r4, r0, r4
	and r3, r3, r4
	xor r3, r13, r3
	addi r4, sp, 56
	addi r6, r4, 40
	addi r4, r0, 0
.LBB15_18:
	add r7, r5, r4
	add r8, r6, r4
	ldbu r8, r8+0
	stb r7+0, r8
	addi r4, r4, 1
	bne r3, r4, .LBB15_18
.LBB15_19:
	add r5, r5, r4
	jal r0, .LBB15_26
.LBB15_20:
	addi r7, r0, 48
	stb r5+0, r7
	addi r8, r5, 2
	stb r5+1, r14
	addi r6, r0, 55
	beq r18, r4, .LBB15_31
.LBB15_21:
	sub r3, r8, r12
	bgt r3, r6, .LBB15_31
.LBB15_22:
	sub r3, r5, r12
	addi lr, r3, 3
	addi r5, r0, -2
	addi r9, r0, 56
	add r10, r18, r0
.LBB15_23:
	addi r3, r8, 1
	stb r8+0, r7
	bgt r10, r5, .LBB15_32
.LBB15_24:
	add r14, lr, r0
	addi r10, r10, 1
	addi lr, lr, 1
	add r8, r3, r0
	blt r14, r9, .LBB15_23
	jal r0, .LBB15_32
.LBB15_25:
	addi r4, r0, 0
.LBB15_26:
	stb r5+0, r14
	addi r3, r5, 1
	bge r4, r13, .LBB15_38
.LBB15_27:
	sub r6, r3, r12
	addi r7, r0, 55
	bgt r6, r7, .LBB15_38
.LBB15_28:
	sub r5, r5, r12
	addi r7, r5, 2
	addi r5, sp, 56
	addi r5, r5, 40
	addi r6, r0, 56
.LBB15_29:
	add r8, r7, r0
	add r7, r5, r4
	ldbu r7, r7+0
	stb r3+0, r7
	addi r4, r4, 1
	addi r3, r3, 1
	bge r4, r13, .LBB15_38
.LBB15_30:
	addi r7, r8, 1
	blt r8, r6, .LBB15_29
	jal r0, .LBB15_38
.LBB15_31:
	add r3, r8, r0
.LBB15_32:
	blt r13, r17, .LBB15_38
.LBB15_33:
	sub r7, r3, r12
	bgt r7, r6, .LBB15_38
.LBB15_34:
	addi r5, sp, 56
	addi r5, r5, 40
	addi r6, r7, 1
	addi r7, r0, 56
.LBB15_35:
	add r8, r3, r4
	add r9, r5, r4
	ldbu r9, r9+0
	stb r8+0, r9
	addi r8, r4, 1
	bge r8, r13, .LBB15_37
.LBB15_36:
	add r9, r6, r4
	add r4, r8, r0
	blt r9, r7, .LBB15_35
.LBB15_37:
	add r3, r3, r8
.LBB15_38:
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
	bne r5, r1, .LBB15_41
.LBB15_39:
	addi r4, r6, -100
	addi r9, r0, 900
	bgeu r4, r9, .LBB15_41
.LBB15_40:
	add r4, r3, r0
	jal r0, .LBB15_42
.LBB15_41:
	addi r4, r3, 1
	stb r3+0, r11
.LBB15_42:
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
.LBB15_43:
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
	bltu r6, r8, .LBB15_45
.LBB15_44:
	add r6, lr, r0
	bltu r10, r9, .LBB15_43
.LBB15_45:
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
	bge r1, r5, .LBB15_49
.LBB15_46:
	add r6, r6, r1
	addi r7, r0, 48
	add r8, r5, r0
.LBB15_47:
	stb r6+0, r7
	addi r6, r6, 1
	addi r8, r8, -1
	bne r1, r8, .LBB15_47
.LBB15_48:
	add r1, r5, r0
.LBB15_49:
	addi r4, r4, 1
	addi r5, sp, 24
	addi r5, r5, -1
.LBB15_50:
	add r6, r1, r0
	addi r1, r1, -1
	add r7, r5, r6
	ldbu r7, r7+0
	stb r4+0, r7
	addi r4, r4, 1
	bgt r6, r17, .LBB15_50
.LBB15_51:
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
.Lfunc_end15:
	.size	fio_efmt, .Lfunc_end15-fio_efmt
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
	beq r1, r3, .LBB16_2
.LBB16_1:
	addi r5, r0, 0
	add r3, r6, r0
	add r4, r7, r0
	jal r0, fio_list_out_real
.LBB16_2:
	add r3, r6, r0
	add r4, r7, r0
	jal r0, f77_wr_d
.Lfunc_end16:
	.size	f77_wr_r, .Lfunc_end16-f77_wr_r
                                        # -- End function
	.globl	f77_wr_c                        # -- Begin function f77_wr_c
	.p2align	2
	.type	f77_wr_c,@function
f77_wr_c:                               # @f77_wr_c
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
	add r11, r4, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r4, r0, 0
	beq r1, r4, .LBB17_48
.LBB17_1:
	lui r14, %hi(fio_col)
	addi r14, r14, %lo(fio_col)
	ldw r1, r14+0
	addi r16, r0, 1022
	lui r15, %hi(fio_len)
	addi r15, r15, %lo(fio_len)
	lui r13, %hi(fio_line)
	addi r13, r13, %lo(fio_line)
	bgt r1, r16, .LBB17_7
.LBB17_2:
	ldw r5, r15+0
	addi r4, r0, 32
	bge r5, r1, .LBB17_5
.LBB17_3:
	addi r6, r5, 1
	add r5, r5, r13
	stb r5+0, r4
	add r5, r6, r0
	bne r1, r6, .LBB17_3
.LBB17_4:
	stw r15+0, r1
	add r5, r1, r0
.LBB17_5:
	add r6, r1, r13
	stb r6+0, r4
	addi r4, r1, 1
	stw r14+0, r4
	blt r1, r5, .LBB17_7
.LBB17_6:
	stw r15+0, r4
.LBB17_7:
	ldw r17, r15+0
	ldw r1, r14+0
	bgt r1, r16, .LBB17_14
.LBB17_8:
	add r4, r17, r0
	bge r17, r1, .LBB17_12
.LBB17_9:
	addi r4, r0, 32
	add r5, r17, r0
.LBB17_10:
	addi r6, r5, 1
	add r5, r5, r13
	stb r5+0, r4
	add r5, r6, r0
	bne r1, r6, .LBB17_10
.LBB17_11:
	stw r15+0, r1
	add r4, r1, r0
.LBB17_12:
	add r5, r1, r13
	addi r6, r0, 40
	stb r5+0, r6
	addi r5, r1, 1
	stw r14+0, r5
	blt r1, r4, .LBB17_14
.LBB17_13:
	stw r15+0, r5
.LBB17_14:
	ldw r18, r15+0
	fcvt.d.s r6, r3
	addi r5, r0, 0
	add r3, r6, r0
	add r4, r7, r0
	jal r31, fio_list_out_real
	ldw r1, r15+0
	add r3, r18, r0
	bge r18, r1, .LBB17_19
.LBB17_15:
	addi r4, r0, 32
	add r3, r18, r0
.LBB17_16:
	add r5, r3, r13
	ldbu r5, r5+0
	bne r5, r4, .LBB17_19
.LBB17_17:
	addi r3, r3, 1
	bne r1, r3, .LBB17_16
.LBB17_18:
	add r3, r1, r0
.LBB17_19:
	ble r1, r3, .LBB17_24
.LBB17_20:
	lui r4, %hi(fio_line-1)
	addi r4, r4, %lo(fio_line-1)
	addi r5, r0, 32
.LBB17_21:
	add r6, r1, r4
	ldbu r6, r6+0
	bne r6, r5, .LBB17_24
.LBB17_22:
	addi r1, r1, -1
	bgt r1, r3, .LBB17_21
.LBB17_23:
	add r1, r3, r0
.LBB17_24:
	sub r4, r1, r3
	addi r12, r0, 1
	blt r4, r12, .LBB17_27
.LBB17_25:
	add r5, r18, r13
.LBB17_26:
	add r6, r3, r13
	ldbu r6, r6+0
	stb r5+0, r6
	addi r3, r3, 1
	addi r5, r5, 1
	bne r1, r3, .LBB17_26
.LBB17_27:
	add r1, r4, r18
	stw r15+0, r1
	stw r14+0, r1
	bgt r1, r16, .LBB17_29
.LBB17_28:
	add r3, r1, r13
	addi r4, r0, 44
	stb r3+0, r4
	addi r1, r1, 1
	stw r14+0, r1
	stw r15+0, r1
.LBB17_29:
	ldw r18, r15+0
	fcvt.d.s r4, r11
	addi r11, r0, 0
	add r3, r4, r0
	add r4, r5, r0
	add r5, r11, r0
	jal r31, fio_list_out_real
	ldw r1, r15+0
	add r3, r18, r0
	bge r18, r1, .LBB17_34
.LBB17_30:
	addi r4, r0, 32
	add r3, r18, r0
.LBB17_31:
	add r5, r3, r13
	ldbu r5, r5+0
	bne r5, r4, .LBB17_34
.LBB17_32:
	addi r3, r3, 1
	bne r1, r3, .LBB17_31
.LBB17_33:
	add r3, r1, r0
.LBB17_34:
	ble r1, r3, .LBB17_39
.LBB17_35:
	lui r4, %hi(fio_line-1)
	addi r4, r4, %lo(fio_line-1)
	addi r5, r0, 32
.LBB17_36:
	add r6, r1, r4
	ldbu r6, r6+0
	bne r6, r5, .LBB17_39
.LBB17_37:
	addi r1, r1, -1
	bgt r1, r3, .LBB17_36
.LBB17_38:
	add r1, r3, r0
.LBB17_39:
	sub r4, r1, r3
	blt r4, r12, .LBB17_42
.LBB17_40:
	add r5, r18, r13
.LBB17_41:
	add r6, r3, r13
	ldbu r6, r6+0
	stb r5+0, r6
	addi r3, r3, 1
	addi r5, r5, 1
	bne r1, r3, .LBB17_41
.LBB17_42:
	add r1, r4, r18
	stw r15+0, r1
	stw r14+0, r1
	bgt r1, r16, .LBB17_44
.LBB17_43:
	add r3, r1, r13
	addi r4, r0, 41
	stb r3+0, r4
	addi r1, r1, 1
	stw r14+0, r1
	stw r15+0, r1
.LBB17_44:
	ldw r1, r15+0
	sub r5, r1, r17
	addi r3, r0, 34
	bgt r5, r3, .LBB17_47
.LBB17_45:
	addi r3, r0, 35
	sub r12, r3, r5
	add r1, r12, r1
	bgt r1, r16, .LBB17_47
.LBB17_46:
	add r13, r17, r13
	add r3, r13, r12
	add r4, r13, r0
	jal r31, memmove
	addi r4, r0, 32
	add r3, r13, r0
	add r5, r12, r0
	jal r31, memset
	ldw r1, r15+0
	add r1, r1, r12
	stw r15+0, r1
.LBB17_47:
	ldw r1, r15+0
	stw r14+0, r1
	lui r1, %hi(fio_first_item)
	addi r1, r1, %lo(fio_first_item)
	stb r1+0, r11
	lui r1, %hi(fio_prev_char)
	addi r1, r1, %lo(fio_prev_char)
	stb r1+0, r11
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
.LBB17_48:
	fcvt.d.s r4, r3
	add r3, r4, r0
	add r4, r5, r0
	jal r31, f77_wr_d
	fcvt.d.s r4, r11
	add r3, r4, r0
	add r4, r5, r0
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
	jal r0, f77_wr_d
.Lfunc_end17:
	.size	f77_wr_c, .Lfunc_end17-f77_wr_c
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
	ldw r5, r1+0
	addi r1, r0, 0
	beq r5, r1, .LBB18_20
.LBB18_1:
	lui r5, %hi(fio_first_item)
	addi r5, r5, %lo(fio_first_item)
	ldbu r8, r5+0
	lui r6, %hi(fio_prev_char)
	addi r6, r6, %lo(fio_prev_char)
	lui r7, %hi(fio_col)
	addi r7, r7, %lo(fio_col)
	bne r8, r1, .LBB18_3
.LBB18_2:
	ldbu r8, r6+0
	andi r8, r8, 1
	addi r9, r0, 0
	bne r8, r9, .LBB18_9
.LBB18_3:
	ldw r8, r7+0
	addi r9, r0, 1022
	bgt r8, r9, .LBB18_9
.LBB18_4:
	lui r9, %hi(fio_len)
	addi r9, r9, %lo(fio_len)
	ldw r10, r9+0
	lui lr, %hi(fio_line)
	addi lr, lr, %lo(fio_line)
	addi r11, r0, 32
	bge r10, r8, .LBB18_7
.LBB18_5:
	addi r12, r10, 1
	add r10, r10, lr
	stb r10+0, r11
	add r10, r12, r0
	bne r8, r12, .LBB18_5
.LBB18_6:
	stw r9+0, r8
	add r10, r8, r0
.LBB18_7:
	add lr, r8, lr
	stb lr+0, r11
	addi lr, r8, 1
	stw r7+0, lr
	blt r8, r10, .LBB18_9
.LBB18_8:
	stw r9+0, lr
.LBB18_9:
	addi r8, r0, 1
	blt r4, r8, .LBB18_19
.LBB18_10:
	lui r9, %hi(fio_len)
	addi r9, r9, %lo(fio_len)
	ldw r13, r9+0
	ldw r14, r7+0
	addi r10, r0, 1022
	lui lr, %hi(fio_line)
	addi lr, lr, %lo(fio_line)
	addi r11, r0, 32
	add r12, r1, r0
	jal r0, .LBB18_13
.LBB18_11:
	add r14, r15, r0
.LBB18_12:
	addi r12, r12, 1
	beq r12, r4, .LBB18_19
.LBB18_13:
	bgt r14, r10, .LBB18_12
.LBB18_14:
	add r15, r3, r12
	ldbu r15, r15+0
	bge r13, r14, .LBB18_17
.LBB18_15:
	addi r16, r13, 1
	add r13, r13, lr
	stb r13+0, r11
	add r13, r16, r0
	bne r14, r16, .LBB18_15
.LBB18_16:
	stw r9+0, r14
	add r13, r14, r0
.LBB18_17:
	add r16, r14, lr
	stb r16+0, r15
	addi r15, r14, 1
	stw r7+0, r15
	blt r14, r13, .LBB18_11
.LBB18_18:
	stw r9+0, r15
	add r13, r15, r0
	jal r0, .LBB18_11
.LBB18_19:
	stb r5+0, r1
	stb r6+0, r8
	jal r0, .LBB18_43
.LBB18_20:
	lui r11, %hi(fio_desc)
	addi r11, r11, %lo(fio_desc)
	ldw r1, r11+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	addi r12, r0, 0
	beq r1, r12, .LBB18_22
.LBB18_21:
	ldw r1, r13+0
	addi r5, r0, 0
	bgt r1, r5, .LBB18_23
.LBB18_22:
	stw r11+0, r12
	add r14, r3, r0
	add r15, r4, r0
	jal r31, fio_next_desc
	add r4, r15, r0
	add r3, r14, r0
	beq r1, r12, .LBB18_43
.LBB18_23:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r5, r1+0
	addi r1, r0, 1
	blt r5, r1, .LBB18_26
.LBB18_24:
	bge r5, r4, .LBB18_26
.LBB18_25:
	add r4, r5, r0
	jal r0, .LBB18_29
.LBB18_26:
	ble r5, r4, .LBB18_29
.LBB18_27:
	sub r5, r5, r4
	blt r5, r1, .LBB18_29
.LBB18_28:
	lui r6, %hi(fio_col)
	addi r6, r6, %lo(fio_col)
	ldw r7, r6+0
	add r5, r7, r5
	stw r6+0, r5
.LBB18_29:
	blt r4, r1, .LBB18_39
.LBB18_30:
	lui r5, %hi(fio_len)
	addi r5, r5, %lo(fio_len)
	ldw lr, r5+0
	lui r6, %hi(fio_col)
	addi r6, r6, %lo(fio_col)
	ldw r14, r6+0
	addi r7, r0, 1022
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	addi r9, r0, 32
	add r10, r12, r0
	jal r0, .LBB18_33
.LBB18_31:
	add r14, r15, r0
.LBB18_32:
	addi r10, r10, 1
	beq r10, r4, .LBB18_39
.LBB18_33:
	bgt r14, r7, .LBB18_32
.LBB18_34:
	add r15, r3, r10
	ldbu r15, r15+0
	bge lr, r14, .LBB18_37
.LBB18_35:
	addi r16, lr, 1
	add lr, lr, r8
	stb lr+0, r9
	add lr, r16, r0
	bne r14, r16, .LBB18_35
.LBB18_36:
	stw r5+0, r14
	add lr, r14, r0
.LBB18_37:
	add r16, r14, r8
	stb r16+0, r15
	addi r15, r14, 1
	stw r6+0, r15
	blt r14, lr, .LBB18_31
.LBB18_38:
	stw r5+0, r15
	add lr, r15, r0
	jal r0, .LBB18_31
.LBB18_39:
	ldw r3, r13+0
	blt r3, r1, .LBB18_41
.LBB18_40:
	addi r1, r3, -1
	stw r13+0, r1
.LBB18_41:
	ldw r1, r13+0
	bne r1, r12, .LBB18_43
.LBB18_42:
	stw r11+0, r12
.LBB18_43:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end18:
	.size	f77_wr_a, .Lfunc_end18-f77_wr_a
                                        # -- End function
	.globl	f77_cabs                        # -- Begin function f77_cabs
	.p2align	2
	.type	f77_cabs,@function
f77_cabs:                               # @f77_cabs
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	fcvt.d.s r6, r3
	fcvt.d.s r4, r4
	fmul.d r4, r4, r4
	fmul.d r6, r6, r6
	fadd.d r4, r6, r4
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end19:
	.size	f77_cabs, .Lfunc_end19-f77_cabs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpow_i
	.type	.LCPI20_0,@object
.LCPI20_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI20_0, 8
	.type	.LCPI20_1,@object
.LCPI20_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI20_1, 8
	.text
	.globl	f77_cpow_i
	.p2align	2
	.type	f77_cpow_i,@function
f77_cpow_i:                             # @f77_cpow_i
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
	lui r1, %hi(.LCPI20_0)
	addi r1, r1, %lo(.LCPI20_0)
	ldw r9, r1+4
	ldw r8, r1+0
	lui r1, %hi(.LCPI20_1)
	addi r1, r1, %lo(.LCPI20_1)
	ldw r11, r1+4
	ldw r10, r1+0
	addi r1, r0, 0
	beq r6, r1, .LBB20_7
.LBB20_1:
	srai r7, r6, 31
	xor lr, r6, r7
	sub r7, lr, r7
	fcvt.d.s r12, r5
	fcvt.d.s r4, r4
	jal r0, .LBB20_3
.LBB20_2:
	beq r7, r1, .LBB20_7
.LBB20_3:
	andi lr, r7, 1
	beq lr, r1, .LBB20_5
.LBB20_4:
	fmul.d r14, r10, r12
	fmul.d r16, r8, r4
	fsub.d r14, r16, r14
	fmul.d r10, r4, r10
	fmul.d r8, r8, r12
	fadd.d r10, r8, r10
	add r8, r14, r0
	add r9, r15, r0
.LBB20_5:
	srli r7, r7, 1
	beq r7, r1, .LBB20_2
.LBB20_6:
	fmul.d r14, r12, r12
	fmul.d r16, r4, r4
	fsub.d r14, r16, r14
	fadd.d r4, r4, r4
	fmul.d r12, r12, r4
	add r4, r14, r0
	add r5, r15, r0
	jal r0, .LBB20_2
.LBB20_7:
	addi r1, r0, -1
	bgt r6, r1, .LBB20_9
.LBB20_8:
	fmul.d r4, r10, r10
	fmul.d r6, r8, r8
	fadd.d r4, r6, r4
	fdiv.d r8, r8, r4
	fneg.d r6, r10
	fdiv.d r10, r6, r4
.LBB20_9:
	fcvt.s.d r1, r8
	stw r3+0, r1
	fcvt.s.d r1, r10
	stw r3+4, r1
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
.Lfunc_end20:
	.size	f77_cpow_i, .Lfunc_end20-f77_cpow_i
                                        # -- End function
	.globl	f77_clog                        # -- Begin function f77_clog
	.p2align	2
	.type	f77_clog,@function
f77_clog:                               # @f77_clog
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	add r11, r3, r0
	fcvt.d.s r12, r4
	fcvt.d.s r14, r5
	fmul.d r4, r14, r14
	fmul.d r6, r12, r12
	fadd.d r4, r6, r4
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r3, r1, r0
	add r4, r2, r0
	jal r31, log
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	add r3, r14, r0
	add r4, r15, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, atan2
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+4, r1
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end21:
	.size	f77_clog, .Lfunc_end21-f77_clog
                                        # -- End function
	.globl	f77_cexp                        # -- Begin function f77_cexp
	.p2align	2
	.type	f77_cexp,@function
f77_cexp:                               # @f77_cexp
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r14
	stw sp+32, r15
	stw sp+28, r16
	stw sp+24, r17
	add r11, r5, r0
	add r12, r3, r0
	fcvt.d.s r4, r4
	add r3, r4, r0
	add r4, r5, r0
	jal r31, exp
	add r14, r1, r0
	add r15, r2, r0
	fcvt.d.s r16, r11
	add r3, r16, r0
	add r4, r17, r0
	jal r31, cos
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r14, r4
	fcvt.s.d r1, r4
	stw r12+0, r1
	add r3, r16, r0
	add r4, r17, r0
	jal r31, sin
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r14, r4
	fcvt.s.d r1, r4
	stw r12+4, r1
	ldw r17, sp+24
	ldw r16, sp+28
	ldw r15, sp+32
	ldw r14, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end22:
	.size	f77_cexp, .Lfunc_end22-f77_cexp
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_cpow_c
	.type	.LCPI23_0,@object
.LCPI23_0:
	.word	0x00000000                      # float 0
	.size	.LCPI23_0, 4
	.text
	.globl	f77_cpow_c
	.p2align	2
	.type	f77_cpow_c,@function
f77_cpow_c:                             # @f77_cpow_c
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
	add r12, r7, r0
	add r13, r6, r0
	add r11, r3, r0
	lui r1, %hi(.LCPI23_0)
	addi r1, r1, %lo(.LCPI23_0)
	ldw r1, r1+0
	feq.s r3, r4, r1
	xori r6, r3, 1
	addi r3, r0, 0
	bne r6, r3, .LBB23_3
.LBB23_1:
	feq.s r6, r5, r1
	xori r6, r6, 1
	bne r6, r3, .LBB23_3
.LBB23_2:
	stw r11+0, r3
	jal r0, .LBB23_4
.LBB23_3:
	fcvt.d.s r14, r4
	fcvt.d.s r16, r5
	fmul.d r4, r16, r16
	fmul.d r6, r14, r14
	fadd.d r4, r6, r4
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r3, r1, r0
	add r4, r2, r0
	jal r31, log
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r18, r4
	add r3, r16, r0
	add r4, r17, r0
	add r5, r14, r0
	add r6, r15, r0
	jal r31, atan2
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	fcvt.d.s r4, r13
	fcvt.d.s r6, r18
	fcvt.d.s r8, r12
	fcvt.d.s r12, r1
	fmul.d r14, r8, r12
	fmul.d r16, r4, r6
	fsub.d r14, r16, r14
	fmul.d r6, r8, r6
	fmul.d r4, r4, r12
	fadd.d r4, r4, r6
	fcvt.s.d r1, r14
	fcvt.s.d r14, r4
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, exp
	add r12, r1, r0
	add r13, r2, r0
	fcvt.d.s r14, r14
	add r3, r14, r0
	add r4, r15, r0
	jal r31, cos
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r12, r4
	fcvt.s.d r1, r4
	stw r11+0, r1
	add r3, r14, r0
	add r4, r15, r0
	jal r31, sin
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r12, r4
	fcvt.s.d r1, r4
.LBB23_4:
	stw r11+4, r1
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
.Lfunc_end23:
	.size	f77_cpow_c, .Lfunc_end23-f77_cpow_c
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_csqrt
	.type	.LCPI24_0,@object
.LCPI24_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI24_0, 8
	.type	.LCPI24_2,@object
.LCPI24_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI24_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI24_1,@object
.LCPI24_1:
	.word	0x00000000                      # float 0
	.size	.LCPI24_1, 4
	.text
	.globl	f77_csqrt
	.p2align	2
	.type	f77_csqrt,@function
f77_csqrt:                              # @f77_csqrt
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
	add r12, r5, r0
	add r13, r4, r0
	add r11, r3, r0
	fcvt.d.s r16, r4
	fcvt.d.s r14, r5
	fmul.d r4, r14, r14
	fmul.d r6, r16, r16
	fadd.d r4, r6, r4
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r18, r1, r0
	add r19, r2, r0
	add r3, r16, r0
	add r4, r17, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r4, r18, r4
	lui r1, %hi(.LCPI24_0)
	addi r1, r1, %lo(.LCPI24_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r16, r1, r0
	add r17, r2, r0
	lui r4, %hi(.LCPI24_1)
	addi r4, r4, %lo(.LCPI24_1)
	ldw r1, r4+0
	fle.s r3, r1, r13
	xori r5, r3, 1
	addi r3, r0, 0
	beq r5, r3, .LBB24_4
.LBB24_1:
	lui r1, %hi(.LCPI24_2)
	addi r1, r1, %lo(.LCPI24_2)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r5, r16, r6
	ldw r13, r4+0
	add r1, r13, r0
	bne r5, r3, .LBB24_3
.LBB24_2:
	add r3, r14, r0
	add r4, r15, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r16, r16
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
.LBB24_3:
	stw r11+0, r1
	flt.s r1, r12, r13
	fneg.d r4, r16
	xor r3, r5, r17
	sub r1, r0, r1
	and r3, r3, r1
	xor r7, r17, r3
	xor r3, r4, r16
	and r1, r3, r1
	xor r6, r16, r1
	fcvt.s.d r1, r6
	jal r0, .LBB24_6
.LBB24_4:
	fcvt.s.d r4, r16
	stw r11+0, r4
	lui r4, %hi(.LCPI24_2)
	addi r4, r4, %lo(.LCPI24_2)
	ldw r7, r4+4
	ldw r6, r4+0
	feq.d r4, r16, r6
	bne r4, r3, .LBB24_6
.LBB24_5:
	fadd.d r4, r16, r16
	fdiv.d r4, r14, r4
	fcvt.s.d r1, r4
.LBB24_6:
	stw r11+4, r1
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
	.size	f77_csqrt, .Lfunc_end24-f77_csqrt
                                        # -- End function
	.globl	f77_csin                        # -- Begin function f77_csin
	.p2align	2
	.type	f77_csin,@function
f77_csin:                               # @f77_csin
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r14
	stw sp+32, r15
	stw sp+28, r16
	stw sp+24, r17
	stw sp+20, r18
	stw sp+16, r19
	add r11, r5, r0
	add r12, r3, r0
	fcvt.d.s r14, r4
	add r3, r14, r0
	add r4, r15, r0
	jal r31, sin
	add r16, r1, r0
	add r17, r2, r0
	fcvt.d.s r18, r11
	add r3, r18, r0
	add r4, r19, r0
	jal r31, cosh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r16, r4
	fcvt.s.d r1, r4
	stw r12+0, r1
	add r3, r14, r0
	add r4, r15, r0
	jal r31, cos
	add r14, r1, r0
	add r15, r2, r0
	add r3, r18, r0
	add r4, r19, r0
	jal r31, sinh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r14, r4
	fcvt.s.d r1, r4
	stw r12+4, r1
	ldw r19, sp+16
	ldw r18, sp+20
	ldw r17, sp+24
	ldw r16, sp+28
	ldw r15, sp+32
	ldw r14, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end25:
	.size	f77_csin, .Lfunc_end25-f77_csin
                                        # -- End function
	.globl	f77_ccos                        # -- Begin function f77_ccos
	.p2align	2
	.type	f77_ccos,@function
f77_ccos:                               # @f77_ccos
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r14
	stw sp+32, r15
	stw sp+28, r16
	stw sp+24, r17
	stw sp+20, r18
	stw sp+16, r19
	add r11, r5, r0
	add r12, r3, r0
	fcvt.d.s r14, r4
	add r3, r14, r0
	add r4, r15, r0
	jal r31, cos
	add r18, r1, r0
	add r19, r2, r0
	fcvt.d.s r16, r11
	add r3, r16, r0
	add r4, r17, r0
	jal r31, cosh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r18, r4
	fcvt.s.d r1, r4
	stw r12+0, r1
	add r3, r14, r0
	add r4, r15, r0
	jal r31, sin
	add r4, r1, r0
	add r5, r2, r0
	fneg.d r14, r4
	add r3, r16, r0
	add r4, r17, r0
	jal r31, sinh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r4, r14
	fcvt.s.d r1, r4
	stw r12+4, r1
	ldw r19, sp+16
	ldw r18, sp+20
	ldw r17, sp+24
	ldw r16, sp+28
	ldw r15, sp+32
	ldw r14, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end26:
	.size	f77_ccos, .Lfunc_end26-f77_ccos
                                        # -- End function
	.globl	f77i_iabs                       # -- Begin function f77i_iabs
	.p2align	2
	.type	f77i_iabs,@function
f77i_iabs:                              # @f77i_iabs
# %bb.0:
	ldw r1, r3+0
	srai r3, r1, 31
	xor r1, r1, r3
	sub r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end27:
	.size	f77i_iabs, .Lfunc_end27-f77i_iabs
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_abs
	.type	.LCPI28_0,@object
.LCPI28_0:
	.word	0x00000000                      # float 0
	.size	.LCPI28_0, 4
	.text
	.globl	f77i_abs
	.p2align	2
	.type	f77i_abs,@function
f77i_abs:                               # @f77i_abs
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI28_0)
	addi r3, r3, %lo(.LCPI28_0)
	ldw r3, r3+0
	flt.s r3, r1, r3
	sub r3, r0, r3
	lui r4, 524288
	and r3, r3, r4
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end28:
	.size	f77i_abs, .Lfunc_end28-f77i_abs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dabs
	.type	.LCPI29_0,@object
.LCPI29_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI29_0, 8
	.text
	.globl	f77i_dabs
	.p2align	2
	.type	f77i_dabs,@function
f77i_dabs:                              # @f77i_dabs
# %bb.0:
	ldw r5, r3+4
	ldw r4, r3+0
	lui r1, %hi(.LCPI29_0)
	addi r1, r1, %lo(.LCPI29_0)
	ldw r7, r1+4
	ldw r6, r1+0
	flt.d r1, r4, r6
	fneg.d r6, r4
	xor r3, r7, r5
	sub r1, r0, r1
	and r3, r3, r1
	xor r2, r5, r3
	xor r3, r6, r4
	and r1, r3, r1
	xor r1, r4, r1
	jalr r0, r31, 0
.Lfunc_end29:
	.size	f77i_dabs, .Lfunc_end29-f77i_dabs
                                        # -- End function
	.globl	f77i_sqrt                       # -- Begin function f77i_sqrt
	.p2align	2
	.type	f77i_sqrt,@function
f77i_sqrt:                              # @f77i_sqrt
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end30:
	.size	f77i_sqrt, .Lfunc_end30-f77i_sqrt
                                        # -- End function
	.globl	f77i_dsqrt                      # -- Begin function f77i_dsqrt
	.p2align	2
	.type	f77i_dsqrt,@function
f77i_dsqrt:                             # @f77i_dsqrt
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sqrt
.Lfunc_end31:
	.size	f77i_dsqrt, .Lfunc_end31-f77i_dsqrt
                                        # -- End function
	.globl	f77i_exp                        # -- Begin function f77i_exp
	.p2align	2
	.type	f77i_exp,@function
f77i_exp:                               # @f77i_exp
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, exp
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end32:
	.size	f77i_exp, .Lfunc_end32-f77i_exp
                                        # -- End function
	.globl	f77i_dexp                       # -- Begin function f77i_dexp
	.p2align	2
	.type	f77i_dexp,@function
f77i_dexp:                              # @f77i_dexp
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, exp
.Lfunc_end33:
	.size	f77i_dexp, .Lfunc_end33-f77i_dexp
                                        # -- End function
	.globl	f77i_alog                       # -- Begin function f77i_alog
	.p2align	2
	.type	f77i_alog,@function
f77i_alog:                              # @f77i_alog
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, log
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end34:
	.size	f77i_alog, .Lfunc_end34-f77i_alog
                                        # -- End function
	.globl	f77i_dlog                       # -- Begin function f77i_dlog
	.p2align	2
	.type	f77i_dlog,@function
f77i_dlog:                              # @f77i_dlog
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log
.Lfunc_end35:
	.size	f77i_dlog, .Lfunc_end35-f77i_dlog
                                        # -- End function
	.globl	f77i_alog10                     # -- Begin function f77i_alog10
	.p2align	2
	.type	f77i_alog10,@function
f77i_alog10:                            # @f77i_alog10
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, log10
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end36:
	.size	f77i_alog10, .Lfunc_end36-f77i_alog10
                                        # -- End function
	.globl	f77i_dlog10                     # -- Begin function f77i_dlog10
	.p2align	2
	.type	f77i_dlog10,@function
f77i_dlog10:                            # @f77i_dlog10
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log10
.Lfunc_end37:
	.size	f77i_dlog10, .Lfunc_end37-f77i_dlog10
                                        # -- End function
	.globl	f77i_sin                        # -- Begin function f77i_sin
	.p2align	2
	.type	f77i_sin,@function
f77i_sin:                               # @f77i_sin
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sin
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end38:
	.size	f77i_sin, .Lfunc_end38-f77i_sin
                                        # -- End function
	.globl	f77i_dsin                       # -- Begin function f77i_dsin
	.p2align	2
	.type	f77i_dsin,@function
f77i_dsin:                              # @f77i_dsin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sin
.Lfunc_end39:
	.size	f77i_dsin, .Lfunc_end39-f77i_dsin
                                        # -- End function
	.globl	f77i_cos                        # -- Begin function f77i_cos
	.p2align	2
	.type	f77i_cos,@function
f77i_cos:                               # @f77i_cos
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, cos
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end40:
	.size	f77i_cos, .Lfunc_end40-f77i_cos
                                        # -- End function
	.globl	f77i_dcos                       # -- Begin function f77i_dcos
	.p2align	2
	.type	f77i_dcos,@function
f77i_dcos:                              # @f77i_dcos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cos
.Lfunc_end41:
	.size	f77i_dcos, .Lfunc_end41-f77i_dcos
                                        # -- End function
	.globl	f77i_tan                        # -- Begin function f77i_tan
	.p2align	2
	.type	f77i_tan,@function
f77i_tan:                               # @f77i_tan
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, tan
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end42:
	.size	f77i_tan, .Lfunc_end42-f77i_tan
                                        # -- End function
	.globl	f77i_dtan                       # -- Begin function f77i_dtan
	.p2align	2
	.type	f77i_dtan,@function
f77i_dtan:                              # @f77i_dtan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tan
.Lfunc_end43:
	.size	f77i_dtan, .Lfunc_end43-f77i_dtan
                                        # -- End function
	.globl	f77i_asin                       # -- Begin function f77i_asin
	.p2align	2
	.type	f77i_asin,@function
f77i_asin:                              # @f77i_asin
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, asin
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end44:
	.size	f77i_asin, .Lfunc_end44-f77i_asin
                                        # -- End function
	.globl	f77i_dasin                      # -- Begin function f77i_dasin
	.p2align	2
	.type	f77i_dasin,@function
f77i_dasin:                             # @f77i_dasin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, asin
.Lfunc_end45:
	.size	f77i_dasin, .Lfunc_end45-f77i_dasin
                                        # -- End function
	.globl	f77i_acos                       # -- Begin function f77i_acos
	.p2align	2
	.type	f77i_acos,@function
f77i_acos:                              # @f77i_acos
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, acos
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end46:
	.size	f77i_acos, .Lfunc_end46-f77i_acos
                                        # -- End function
	.globl	f77i_dacos                      # -- Begin function f77i_dacos
	.p2align	2
	.type	f77i_dacos,@function
f77i_dacos:                             # @f77i_dacos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, acos
.Lfunc_end47:
	.size	f77i_dacos, .Lfunc_end47-f77i_dacos
                                        # -- End function
	.globl	f77i_atan                       # -- Begin function f77i_atan
	.p2align	2
	.type	f77i_atan,@function
f77i_atan:                              # @f77i_atan
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, atan
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end48:
	.size	f77i_atan, .Lfunc_end48-f77i_atan
                                        # -- End function
	.globl	f77i_datan                      # -- Begin function f77i_datan
	.p2align	2
	.type	f77i_datan,@function
f77i_datan:                             # @f77i_datan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, atan
.Lfunc_end49:
	.size	f77i_datan, .Lfunc_end49-f77i_datan
                                        # -- End function
	.globl	f77i_atan2                      # -- Begin function f77i_atan2
	.p2align	2
	.type	f77i_atan2,@function
f77i_atan2:                             # @f77i_atan2
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r6, r1
	ldw r1, r4+0
	fcvt.d.s r8, r1
	add r3, r6, r0
	add r4, r7, r0
	add r5, r8, r0
	add r6, r9, r0
	jal r31, atan2
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end50:
	.size	f77i_atan2, .Lfunc_end50-f77i_atan2
                                        # -- End function
	.globl	f77i_datan2                     # -- Begin function f77i_datan2
	.p2align	2
	.type	f77i_datan2,@function
f77i_datan2:                            # @f77i_datan2
# %bb.0:
	ldw r1, r3+4
	ldw r3, r3+0
	ldw r6, r4+4
	ldw r5, r4+0
	add r4, r1, r0
	jal r0, atan2
.Lfunc_end51:
	.size	f77i_datan2, .Lfunc_end51-f77i_datan2
                                        # -- End function
	.globl	f77i_sinh                       # -- Begin function f77i_sinh
	.p2align	2
	.type	f77i_sinh,@function
f77i_sinh:                              # @f77i_sinh
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sinh
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end52:
	.size	f77i_sinh, .Lfunc_end52-f77i_sinh
                                        # -- End function
	.globl	f77i_dsinh                      # -- Begin function f77i_dsinh
	.p2align	2
	.type	f77i_dsinh,@function
f77i_dsinh:                             # @f77i_dsinh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sinh
.Lfunc_end53:
	.size	f77i_dsinh, .Lfunc_end53-f77i_dsinh
                                        # -- End function
	.globl	f77i_cosh                       # -- Begin function f77i_cosh
	.p2align	2
	.type	f77i_cosh,@function
f77i_cosh:                              # @f77i_cosh
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, cosh
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end54:
	.size	f77i_cosh, .Lfunc_end54-f77i_cosh
                                        # -- End function
	.globl	f77i_dcosh                      # -- Begin function f77i_dcosh
	.p2align	2
	.type	f77i_dcosh,@function
f77i_dcosh:                             # @f77i_dcosh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cosh
.Lfunc_end55:
	.size	f77i_dcosh, .Lfunc_end55-f77i_dcosh
                                        # -- End function
	.globl	f77i_tanh                       # -- Begin function f77i_tanh
	.p2align	2
	.type	f77i_tanh,@function
f77i_tanh:                              # @f77i_tanh
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, tanh
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end56:
	.size	f77i_tanh, .Lfunc_end56-f77i_tanh
                                        # -- End function
	.globl	f77i_dtanh                      # -- Begin function f77i_dtanh
	.p2align	2
	.type	f77i_dtanh,@function
f77i_dtanh:                             # @f77i_dtanh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tanh
.Lfunc_end57:
	.size	f77i_dtanh, .Lfunc_end57-f77i_dtanh
                                        # -- End function
	.globl	f77i_aint                       # -- Begin function f77i_aint
	.p2align	2
	.type	f77i_aint,@function
f77i_aint:                              # @f77i_aint
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, trunc
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end58:
	.size	f77i_aint, .Lfunc_end58-f77i_aint
                                        # -- End function
	.globl	f77i_dint                       # -- Begin function f77i_dint
	.p2align	2
	.type	f77i_dint,@function
f77i_dint:                              # @f77i_dint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, trunc
.Lfunc_end59:
	.size	f77i_dint, .Lfunc_end59-f77i_dint
                                        # -- End function
	.globl	f77i_anint                      # -- Begin function f77i_anint
	.p2align	2
	.type	f77i_anint,@function
f77i_anint:                             # @f77i_anint
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, round
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end60:
	.size	f77i_anint, .Lfunc_end60-f77i_anint
                                        # -- End function
	.globl	f77i_dnint                      # -- Begin function f77i_dnint
	.p2align	2
	.type	f77i_dnint,@function
f77i_dnint:                             # @f77i_dnint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, round
.Lfunc_end61:
	.size	f77i_dnint, .Lfunc_end61-f77i_dnint
                                        # -- End function
	.globl	f77i_nint                       # -- Begin function f77i_nint
	.p2align	2
	.type	f77i_nint,@function
f77i_nint:                              # @f77i_nint
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, round
	add r4, r1, r0
	add r5, r2, r0
	fcvt.w.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end62:
	.size	f77i_nint, .Lfunc_end62-f77i_nint
                                        # -- End function
	.globl	f77i_idnint                     # -- Begin function f77i_idnint
	.p2align	2
	.type	f77i_idnint,@function
f77i_idnint:                            # @f77i_idnint
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r4, r3+4
	ldw r3, r3+0
	jal r31, round
	add r4, r1, r0
	add r5, r2, r0
	fcvt.w.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end63:
	.size	f77i_idnint, .Lfunc_end63-f77i_idnint
                                        # -- End function
	.globl	f77i_mod                        # -- Begin function f77i_mod
	.p2align	2
	.type	f77i_mod,@function
f77i_mod:                               # @f77i_mod
# %bb.0:
	ldw r1, r3+0
	ldw r3, r4+0
	rem r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end64:
	.size	f77i_mod, .Lfunc_end64-f77i_mod
                                        # -- End function
	.globl	f77i_amod                       # -- Begin function f77i_amod
	.p2align	2
	.type	f77i_amod,@function
f77i_amod:                              # @f77i_amod
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r6, r1
	ldw r1, r4+0
	fcvt.d.s r8, r1
	add r3, r6, r0
	add r4, r7, r0
	add r5, r8, r0
	add r6, r9, r0
	jal r31, fmod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end65:
	.size	f77i_amod, .Lfunc_end65-f77i_amod
                                        # -- End function
	.globl	f77i_dmod                       # -- Begin function f77i_dmod
	.p2align	2
	.type	f77i_dmod,@function
f77i_dmod:                              # @f77i_dmod
# %bb.0:
	ldw r1, r3+4
	ldw r3, r3+0
	ldw r6, r4+4
	ldw r5, r4+0
	add r4, r1, r0
	jal r0, fmod
.Lfunc_end66:
	.size	f77i_dmod, .Lfunc_end66-f77i_dmod
                                        # -- End function
	.globl	f77i_isign                      # -- Begin function f77i_isign
	.p2align	2
	.type	f77i_isign,@function
f77i_isign:                             # @f77i_isign
# %bb.0:
	ldw r1, r3+0
	srai r3, r1, 31
	xor r1, r1, r3
	sub r1, r1, r3
	ldw r3, r4+0
	addi r4, r0, 0
	slt r3, r3, r4
	sub r4, r0, r1
	xor r4, r4, r1
	sub r3, r0, r3
	and r3, r4, r3
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end67:
	.size	f77i_isign, .Lfunc_end67-f77i_isign
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_sign
	.type	.LCPI68_0,@object
.LCPI68_0:
	.word	0x00000000                      # float 0
	.size	.LCPI68_0, 4
	.text
	.globl	f77i_sign
	.p2align	2
	.type	f77i_sign,@function
f77i_sign:                              # @f77i_sign
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI68_0)
	addi r3, r3, %lo(.LCPI68_0)
	ldw r3, r3+0
	flt.s r5, r1, r3
	sub r5, r0, r5
	lui r6, 524288
	and r5, r5, r6
	xor r1, r1, r5
	ldw r4, r4+0
	flt.s r3, r4, r3
	sub r3, r0, r3
	and r3, r3, r6
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end68:
	.size	f77i_sign, .Lfunc_end68-f77i_sign
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dsign
	.type	.LCPI69_0,@object
.LCPI69_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI69_0, 8
	.text
	.globl	f77i_dsign
	.p2align	2
	.type	f77i_dsign,@function
f77i_dsign:                             # @f77i_dsign
# %bb.0:
	addi sp, sp, -16
	stw sp+12, r11
	stw sp+8, r12
	stw sp+4, r13
	ldw r7, r3+4
	ldw r6, r3+0
	lui r1, %hi(.LCPI69_0)
	addi r1, r1, %lo(.LCPI69_0)
	ldw r9, r1+4
	ldw r8, r1+0
	flt.d r1, r6, r8
	fneg.d r10, r6
	xor r3, r11, r7
	sub r1, r0, r1
	and r3, r3, r1
	xor r13, r7, r3
	xor r3, r10, r6
	and r1, r3, r1
	xor r12, r6, r1
	ldw r7, r4+4
	ldw r6, r4+0
	flt.d r1, r6, r8
	fneg.d r4, r12
	xor r3, r5, r13
	sub r1, r0, r1
	and r3, r3, r1
	xor r2, r13, r3
	xor r3, r4, r12
	and r1, r3, r1
	xor r1, r12, r1
	ldw r13, sp+4
	ldw r12, sp+8
	ldw r11, sp+12
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end69:
	.size	f77i_dsign, .Lfunc_end69-f77i_dsign
                                        # -- End function
	.globl	f77i_idim                       # -- Begin function f77i_idim
	.p2align	2
	.type	f77i_idim,@function
f77i_idim:                              # @f77i_idim
# %bb.0:
	ldw r1, r3+0
	ldw r3, r4+0
	sgt r4, r1, r3
	sub r1, r1, r3
	sub r3, r0, r4
	and r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end70:
	.size	f77i_idim, .Lfunc_end70-f77i_idim
                                        # -- End function
	.globl	f77i_dim                        # -- Begin function f77i_dim
	.p2align	2
	.type	f77i_dim,@function
f77i_dim:                               # @f77i_dim
# %bb.0:
	ldw r1, r3+0
	ldw r3, r4+0
	flt.s r4, r3, r1
	fsub.s r1, r1, r3
	sub r3, r0, r4
	and r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end71:
	.size	f77i_dim, .Lfunc_end71-f77i_dim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_ddim
	.type	.LCPI72_0,@object
.LCPI72_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI72_0, 8
	.text
	.globl	f77i_ddim
	.p2align	2
	.type	f77i_ddim,@function
f77i_ddim:                              # @f77i_ddim
# %bb.0:
	ldw r7, r3+4
	ldw r6, r3+0
	ldw r9, r4+4
	ldw r8, r4+0
	flt.d r1, r8, r6
	fsub.d r4, r6, r8
	lui r3, %hi(.LCPI72_0)
	addi r3, r3, %lo(.LCPI72_0)
	ldw r6, r3+4
	ldw r3, r3+0
	xor r7, r5, r6
	sub r1, r0, r1
	and r7, r7, r1
	xor r2, r6, r7
	xor r4, r4, r3
	and r1, r4, r1
	xor r1, r3, r1
	jalr r0, r31, 0
.Lfunc_end72:
	.size	f77i_ddim, .Lfunc_end72-f77i_ddim
                                        # -- End function
	.globl	f77i_dprod                      # -- Begin function f77i_dprod
	.p2align	2
	.type	f77i_dprod,@function
f77i_dprod:                             # @f77i_dprod
# %bb.0:
	ldw r1, r3+0
	fcvt.d.s r6, r1
	ldw r1, r4+0
	fcvt.d.s r4, r1
	fmul.d r4, r6, r4
	add r1, r4, r0
	add r2, r5, r0
	jalr r0, r31, 0
.Lfunc_end73:
	.size	f77i_dprod, .Lfunc_end73-f77i_dprod
                                        # -- End function
	.globl	f77i_len                        # -- Begin function f77i_len
	.p2align	2
	.type	f77i_len,@function
f77i_len:                               # @f77i_len
# %bb.0:
	add r1, r4, r0
	jalr r0, r31, 0
.Lfunc_end74:
	.size	f77i_len, .Lfunc_end74-f77i_len
                                        # -- End function
	.globl	f77i_index                      # -- Begin function f77i_index
	.p2align	2
	.type	f77i_index,@function
f77i_index:                             # @f77i_index
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	add r12, r5, r0
	addi r11, r0, 0
	beq r6, r11, .LBB75_5
.LBB75_1:
	add r13, r6, r0
	bgt r6, r12, .LBB75_7
.LBB75_2:
	add r14, r4, r0
	add r15, r3, r0
	addi r11, r0, 0
	add r16, r11, r0
.LBB75_3:
	add r3, r15, r16
	add r4, r14, r0
	add r5, r13, r0
	jal r31, memcmp
	beq r1, r11, .LBB75_6
.LBB75_4:
	addi r16, r16, 1
	add r1, r13, r16
	ble r1, r12, .LBB75_3
	jal r0, .LBB75_7
.LBB75_5:
	addi r1, r0, 0
	sgt r11, r12, r1
	jal r0, .LBB75_7
.LBB75_6:
	addi r11, r16, 1
.LBB75_7:
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
.Lfunc_end75:
	.size	f77i_index, .Lfunc_end75-f77i_index
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
	beq r6, r11, .LBB76_5
.LBB76_1:
	add r13, r6, r0
	bgt r6, r12, .LBB76_7
.LBB76_2:
	add r14, r5, r0
	add r15, r3, r0
	addi r11, r0, 0
	add r16, r11, r0
.LBB76_3:
	add r3, r15, r16
	add r4, r14, r0
	add r5, r13, r0
	jal r31, memcmp
	beq r1, r11, .LBB76_6
.LBB76_4:
	addi r16, r16, 1
	add r1, r13, r16
	ble r1, r12, .LBB76_3
	jal r0, .LBB76_7
.LBB76_5:
	addi r1, r0, 0
	sgt r11, r12, r1
	jal r0, .LBB76_7
.LBB76_6:
	addi r11, r16, 1
.LBB76_7:
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
.Lfunc_end76:
	.size	f77_index, .Lfunc_end76-f77_index
                                        # -- End function
	.globl	f77i_aimag                      # -- Begin function f77i_aimag
	.p2align	2
	.type	f77i_aimag,@function
f77i_aimag:                             # @f77i_aimag
# %bb.0:
	ldw r1, r3+4
	jalr r0, r31, 0
.Lfunc_end77:
	.size	f77i_aimag, .Lfunc_end77-f77i_aimag
                                        # -- End function
	.globl	f77i_cabs                       # -- Begin function f77i_cabs
	.p2align	2
	.type	f77i_cabs,@function
f77i_cabs:                              # @f77i_cabs
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	ldw r1, r3+0
	fcvt.d.s r4, r1
	ldw r1, r3+4
	fcvt.d.s r6, r1
	fmul.d r6, r6, r6
	fmul.d r4, r4, r4
	fadd.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end78:
	.size	f77i_cabs, .Lfunc_end78-f77i_cabs
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
	blt r14, r1, .LBB79_2
.LBB79_1:
	add r3, r12, r0
	add r4, r5, r0
	add r5, r14, r0
	jal r31, memmove
.LBB79_2:
	bge r13, r11, .LBB79_5
.LBB79_3:
	add r1, r12, r14
	sub r3, r11, r14
	addi r4, r0, 32
	addi r5, r0, 0
.LBB79_4:
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB79_4
.LBB79_5:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end79:
	.size	f77_strcpy, .Lfunc_end79-f77_strcpy
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
	blt r7, r1, .LBB80_9
.LBB80_1:
	addi r8, r0, 0
	addi r9, r0, 32
	add r10, r8, r0
                                        # implicit-def: $r1
.LBB80_2:
	add lr, r9, r0
	bge r10, r4, .LBB80_4
.LBB80_3:
	add lr, r3, r10
	ldbu lr, lr+0
.LBB80_4:
	add r11, r9, r0
	bge r10, r6, .LBB80_6
.LBB80_5:
	add r11, r5, r10
	ldbu r11, r11+0
.LBB80_6:
	seq r12, lr, r11
	sltu r13, lr, r11
	sub r13, r0, r13
	ori  r13, r13, 1
	xor r1, r1, r13
	sub r12, r0, r12
	and r1, r1, r12
	xor r1, r13, r1
	bne lr, r11, .LBB80_10
.LBB80_7:
	addi r10, r10, 1
	bne r7, r10, .LBB80_2
.LBB80_8:
	add r1, r8, r0
	jal r0, .LBB80_10
.LBB80_9:
	addi r1, r0, 0
.LBB80_10:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end80:
	.size	f77_strcmp, .Lfunc_end80-f77_strcmp
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
	bltu r15, r18, .LBB81_2
.LBB81_1:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	add r5, r15, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB81_2:
	lui r1, %hi(fio_catpos)
	addi r1, r1, %lo(fio_catpos)
	ldw r3, r1+0
	add r3, r3, r15
	blt r3, r18, .LBB81_4
.LBB81_3:
	stw r1+0, r16
.LBB81_4:
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
	blt r13, r15, .LBB81_6
.LBB81_5:
	add r3, r16, r0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, memmove
.LBB81_6:
	blt r11, r15, .LBB81_8
.LBB81_7:
	add r3, r16, r17
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memmove
.LBB81_8:
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
.Lfunc_end81:
	.size	f77_cat, .Lfunc_end81-f77_cat
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
	beq r1, r11, .LBB82_15
.LBB82_1:
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r7, r4+0
	addi r6, r0, 1022
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	bgt r7, r6, .LBB82_7
.LBB82_2:
	ldw r9, r1+0
	addi r8, r0, 32
	bge r9, r7, .LBB82_5
.LBB82_3:
	addi r10, r9, 1
	add r9, r9, r5
	stb r9+0, r8
	add r9, r10, r0
	bne r7, r10, .LBB82_3
.LBB82_4:
	stw r1+0, r7
	add r9, r7, r0
.LBB82_5:
	add r10, r7, r5
	stb r10+0, r8
	addi r8, r7, 1
	stw r4+0, r8
	blt r7, r9, .LBB82_7
.LBB82_6:
	stw r1+0, r8
.LBB82_7:
	ldw r7, r4+0
	bgt r7, r6, .LBB82_14
.LBB82_8:
	ldw r6, r1+0
	bge r6, r7, .LBB82_12
.LBB82_9:
	addi r8, r0, 32
.LBB82_10:
	addi r9, r6, 1
	add r6, r6, r5
	stb r6+0, r8
	add r6, r9, r0
	bne r7, r9, .LBB82_10
.LBB82_11:
	stw r1+0, r7
	add r6, r7, r0
.LBB82_12:
	seq r3, r3, r11
	sub r3, r0, r3
	andi r3, r3, 18
	xori r3, r3, 84
	add r5, r7, r5
	stb r5+0, r3
	addi r3, r7, 1
	stw r4+0, r3
	blt r7, r6, .LBB82_14
.LBB82_13:
	stw r1+0, r3
.LBB82_14:
	lui r1, %hi(fio_first_item)
	addi r1, r1, %lo(fio_first_item)
	stb r1+0, r11
	lui r1, %hi(fio_prev_char)
	addi r1, r1, %lo(fio_prev_char)
	stb r1+0, r11
	jal r0, .LBB82_31
.LBB82_15:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	beq r1, r11, .LBB82_17
.LBB82_16:
	ldw r1, r13+0
	bgt r1, r11, .LBB82_18
.LBB82_17:
	stw r12+0, r11
	add r14, r3, r0
	jal r31, fio_next_desc
	add r3, r14, r0
	beq r1, r11, .LBB82_31
.LBB82_18:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r4, r0, 2
	blt r1, r4, .LBB82_20
.LBB82_19:
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r5, r4+0
	add r1, r1, r5
	addi r1, r1, -1
	stw r4+0, r1
.LBB82_20:
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r1, r4+0
	addi r5, r0, 1022
	bgt r1, r5, .LBB82_27
.LBB82_21:
	lui r5, %hi(fio_len)
	addi r5, r5, %lo(fio_len)
	ldw r6, r5+0
	bge r6, r1, .LBB82_25
.LBB82_22:
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
.LBB82_23:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r1, r9, .LBB82_23
.LBB82_24:
	stw r5+0, r1
	add r6, r1, r0
.LBB82_25:
	seq r3, r3, r11
	sub r3, r0, r3
	andi r3, r3, 18
	xori r3, r3, 84
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	add r7, r1, r7
	stb r7+0, r3
	addi r3, r1, 1
	stw r4+0, r3
	blt r1, r6, .LBB82_27
.LBB82_26:
	stw r5+0, r3
.LBB82_27:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB82_29
.LBB82_28:
	addi r1, r1, -1
	stw r13+0, r1
.LBB82_29:
	ldw r1, r13+0
	bne r1, r11, .LBB82_31
.LBB82_30:
	stw r12+0, r11
.LBB82_31:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end82:
	.size	f77_wr_l, .Lfunc_end82-f77_wr_l
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
	bne r1, r11, .LBB83_2
.LBB83_1:
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
.LBB83_2:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r11
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end83:
	.size	f77_wr_end, .Lfunc_end83-f77_wr_end
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
	addi r24, r0, 1
	lui r16, %hi(fio_fmt)
	addi r16, r16, %lo(fio_fmt)
	lui r14, %hi(fio_pos)
	addi r14, r14, %lo(fio_pos)
	addi r17, r0, 44
	addi r11, r0, 0
	lui r1, 24
	addi r18, r1, 1695
	addi r19, r0, 32
	lui r20, %hi(fio_gdepth)
	addi r20, r20, %lo(fio_gdepth)
	lui r21, %hi(fio_draining)
	addi r21, r21, %lo(fio_draining)
	lui r22, %hi(fio_revert)
	addi r22, r22, %lo(fio_revert)
	lui r23, %hi(fio_reading)
	addi r23, r23, %lo(fio_reading)
	lui r28, %hi(stderr)
	addi r28, r28, %lo(stderr)
	lui r1, %hi(.L.str.3)
	addi r1, r1, %lo(.L.str.3)
	stw sp+40, r1
	addi r13, r0, 2
	addi r25, r0, -33
	addi r26, r0, 83
	addi r1, r0, 35
	stw sp+52, r1
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	stw sp+48, r1
	lui r1, %hi(.LJTI84_4)
	addi r1, r1, %lo(.LJTI84_4)
	stw sp+44, r1
	addi r12, r0, 58
	addi r1, r0, 66
	stw sp+36, r1
	addi r1, r0, 11
	stw sp+32, r1
	addi r1, r0, 90
	stw sp+28, r1
	add r15, r24, r0
.LBB84_1:
	ldw fp, r16+0
	ldw r3, r14+0
	add r6, fp, r3
	ldb r5, r6+0
	andi r1, r5, 255
	beq r1, r17, .LBB84_12
.LBB84_2:
	beq r1, r19, .LBB84_12
.LBB84_3:
	bne r1, r11, .LBB84_16
.LBB84_4:
	ldw r1, r20+0
	blt r1, r24, .LBB84_6
.LBB84_5:
	stw r20+0, r11
.LBB84_6:
	ldbu r1, r21+0
	bne r1, r11, .LBB84_118
.LBB84_7:
	ldw r1, r22+0
	blt r1, r11, .LBB84_118
.LBB84_8:
	stw r14+0, r1
	add r1, fp, r1
	ldbu r1, r1+0
	beq r1, r11, .LBB84_118
.LBB84_9:
	ldbu r1, r23+0
	bne r1, r24, .LBB84_59
.LBB84_10:
	jal r31, fio_getrec
	beq r1, r11, .LBB84_14
.LBB84_11:
	ldw r3, r28+0
	ldw r4, sp+40
	jal r31, fprintf
	add r3, r13, r0
	jal r31, exit
	jal r0, .LBB84_14
.LBB84_12:
	addi r1, r3, 1
.LBB84_13:
	stw r14+0, r1
.LBB84_14:
	bgtu r15, r18, .LBB84_118
.LBB84_15:
	addi r15, r15, 1
	jal r0, .LBB84_1
.LBB84_16:
	and r4, r5, r25
	bne r4, r26, .LBB84_20
.LBB84_17:
	addi r1, r3, 1
	stw r14+0, r1
	add r1, fp, r1
	ldbu r1, r1+0
	addi r1, r1, -80
	ldw r4, sp+52
	bgtu r1, r4, .LBB84_49
.LBB84_18:
	slli r1, r1, 2
	ldw r4, sp+44
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB84_19:
	ldw r1, sp+48
	stb r1+0, r24
	jal r0, .LBB84_27
.LBB84_20:
	bne r5, r12, .LBB84_22
.LBB84_21:
	addi r1, r3, 1
	stw r14+0, r1
	ldbu r1, r21+0
	beq r1, r11, .LBB84_14
	jal r0, .LBB84_118
.LBB84_22:
	andi r4, r5, 223
	ldw r7, sp+36
	bne r4, r7, .LBB84_28
.LBB84_23:
	ldbu r4, r6+1
	addi r7, r4, -78
	andi r8, r7, 252
	slli r7, r7, 6
	srli r8, r8, 2
	or  r7, r7, r8
	andi r7, r7, 255
	ldw r8, sp+32
	bgtu r7, r8, .LBB84_28
.LBB84_24:
	sll r7, r24, r7
	andi r7, r7, 2313
	beq r7, r11, .LBB84_28
.LBB84_25:
	andi r1, r4, 223
	ldw r4, sp+28
	seq r1, r1, r4
	lui r4, %hi(fio_bz)
	addi r4, r4, %lo(fio_bz)
	stw r4+0, r1
	jal r0, .LBB84_27
.LBB84_26:
	ldw r1, sp+48
	stb r1+0, r11
.LBB84_27:
	addi r1, r3, 2
	jal r0, .LBB84_13
.LBB84_28:
	addi r27, r0, 1
	addi r4, r1, -39
	addi r7, r0, 8
	lui r1, %hi(fio_line)
	addi r1, r1, %lo(fio_line)
	bgtu r4, r7, .LBB84_52
.LBB84_29:
	slli r4, r4, 2
	lui r7, %hi(.LJTI84_0)
	addi r7, r7, %lo(.LJTI84_0)
	add r4, r7, r4
	ldw r7, r4+0
	add r4, r27, r0
	jalr r0, r7, 0
.LBB84_30:
	addi r3, r3, 1
	stw r14+0, r3
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r9, r4+0
	lui r5, %hi(fio_len)
	addi r5, r5, %lo(fio_len)
	ldw r7, r5+0
	ldbu r6, r23+0
	jal r0, .LBB84_34
.LBB84_31:
	addi r10, r0, 1
.LBB84_32:
	add r9, r8, r0
.LBB84_33:
	add r3, r3, r10
	stw r14+0, r3
.LBB84_34:
	add r8, r9, r0
	add r9, fp, r3
	ldbu r10, r9+0
	addi r9, r0, 39
	beq r10, r9, .LBB84_41
.LBB84_35:
	addi r9, r0, 0
	beq r10, r9, .LBB84_14
.LBB84_36:
	addi lr, r0, 1022
	sgt lr, r8, lr
	or  lr, r6, lr
	andi lr, lr, 1
	bne lr, r9, .LBB84_31
.LBB84_37:
	bge r7, r8, .LBB84_40
.LBB84_38:
	addi r9, r7, 1
	add r7, r7, r1
	stb r7+0, r19
	add r7, r9, r0
	bne r8, r9, .LBB84_38
.LBB84_39:
	stw r5+0, r8
	add r7, r8, r0
.LBB84_40:
	add r9, r8, r1
	stb r9+0, r10
	addi r9, r8, 1
	stw r4+0, r9
	addi r10, r0, 1
	blt r8, r7, .LBB84_33
	jal r0, .LBB84_48
.LBB84_41:
	addi r10, r3, 1
	add lr, fp, r10
	ldbu lr, lr+0
	bne lr, r9, .LBB84_87
.LBB84_42:
	addi r10, r0, 1022
	sgt r10, r8, r10
	or  r10, r6, r10
	andi r10, r10, 1
	addi lr, r0, 0
	beq r10, lr, .LBB84_44
.LBB84_43:
	add r10, r13, r0
	jal r0, .LBB84_32
.LBB84_44:
	bge r7, r8, .LBB84_47
.LBB84_45:
	addi r10, r7, 1
	add r7, r7, r1
	stb r7+0, r19
	add r7, r10, r0
	bne r8, r10, .LBB84_45
.LBB84_46:
	stw r5+0, r8
	add r7, r8, r0
.LBB84_47:
	add r10, r8, r1
	stb r10+0, r9
	addi r9, r8, 1
	stw r4+0, r9
	addi r10, r0, 2
	blt r8, r7, .LBB84_33
.LBB84_48:
	stw r5+0, r9
	add r7, r9, r0
	jal r0, .LBB84_33
.LBB84_49:
	ldw r1, sp+48
	stb r1+0, r11
	jal r0, .LBB84_14
.LBB84_50:
	ldb r4, r6+1
	addi r4, r4, -58
	addi r5, r0, -10
	bgeu r4, r5, .LBB84_60
.LBB84_51:
	addi r5, r0, 45
.LBB84_52:
	add r4, r27, r0
	jal r0, .LBB84_61
.LBB84_53:
	addi r1, r3, 1
	stw r14+0, r1
	ldw r1, r20+0
	addi r3, r0, 1
	blt r1, r3, .LBB84_14
.LBB84_54:
	addi r3, r1, -1
	stw r20+0, r3
	slli r3, r3, 2
	lui r4, %hi(fio_gcount)
	addi r4, r4, %lo(fio_gcount)
	add r4, r3, r4
	ldw r5, r4+0
	addi r6, r5, -1
	stw r4+0, r6
	blt r5, r13, .LBB84_14
.LBB84_55:
	lui r4, %hi(fio_gstart)
	addi r4, r4, %lo(fio_gstart)
	add r3, r3, r4
	ldw r3, r3+0
	stw r14+0, r3
	stw r20+0, r1
	jal r0, .LBB84_14
.LBB84_56:
	addi r1, r3, 1
	stw r14+0, r1
	ldbu r1, r23+0
	addi r3, r0, 1
	bne r1, r3, .LBB84_59
.LBB84_57:
	jal r31, fio_getrec
	addi r3, r0, 0
	beq r1, r3, .LBB84_14
.LBB84_58:
	ldw r3, r28+0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB84_14
.LBB84_59:
	jal r31, fio_flush_line
	jal r0, .LBB84_14
.LBB84_60:
	addi r4, r3, 1
	stw r14+0, r4
	add r4, fp, r4
	ldb r5, r4+0
	addi r4, r0, 0
.LBB84_61:
	stw sp+24, r28
	addi r5, r5, -58
	addi lr, r0, -10
	bltu r5, lr, .LBB84_67
.LBB84_62:
	ldw r5, r14+0
	add r6, fp, r5
	ldb r6, r6+0
	addi r6, r6, -48
	addi r7, r0, 9
	bleu r6, r7, .LBB84_64
.LBB84_63:
	addi r27, r0, 0
	jal r0, .LBB84_67
.LBB84_64:
	addi r7, fp, 1
	addi r27, r0, 0
.LBB84_65:
	addi r9, r0, 10
	mul r10, r27, r9
	addi r8, r5, 1
	add r27, r10, r6
	add r5, r7, r5
	ldb r5, r5+0
	addi r6, r5, -48
	add r5, r8, r0
	bltu r6, r9, .LBB84_65
.LBB84_66:
	stw r14+0, r8
.LBB84_67:
	ldw r5, r14+0
	add r6, fp, r5
	ldb r28, r6+0
	andi r6, r28, 255
	addi r7, r0, 40
	bne r6, r7, .LBB84_71
.LBB84_68:
	addi r4, r5, 1
	stw r14+0, r4
	ldw r1, r20+0
	addi r5, r0, 15
	ldw r28, sp+24
	bgt r1, r5, .LBB84_14
.LBB84_69:
	slli r5, r1, 2
	lui r6, %hi(fio_gstart)
	addi r6, r6, %lo(fio_gstart)
	add r6, r5, r6
	stw r6+0, r4
	lui r4, %hi(fio_gcount)
	addi r4, r4, %lo(fio_gcount)
	add r4, r5, r4
	stw r4+0, r27
	addi r4, r1, 1
	stw r20+0, r4
	addi r4, r0, 0
	bne r1, r4, .LBB84_14
.LBB84_70:
	stw r22+0, r3
	jal r0, .LBB84_14
.LBB84_71:
	andi r3, r28, 223
	addi r3, r3, -72
	addi r7, r0, 16
	bgtu r3, r7, .LBB84_99
.LBB84_72:
	slli r3, r3, 2
	lui r7, %hi(.LJTI84_1)
	addi r7, r7, %lo(.LJTI84_1)
	add r3, r7, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB84_73:
	addi r3, r5, 1
	addi r4, r0, 1
	blt r27, r4, .LBB84_107
.LBB84_74:
	lui r5, %hi(fio_len)
	addi r5, r5, %lo(fio_len)
	ldw r8, r5+0
	lui r6, %hi(fio_col)
	addi r6, r6, %lo(fio_col)
	ldw r9, r6+0
	ldbu r7, r23+0
	ldw r28, sp+24
	jal r0, .LBB84_77
.LBB84_75:
	ldw r28, sp+24
.LBB84_76:
	addi r27, r10, -1
	addi r3, r3, 1
	ble r10, r4, .LBB84_102
.LBB84_77:
	add r10, r27, r0
	add lr, fp, r3
	ldbu lr, lr+0
	addi r27, r0, 0
	beq lr, r27, .LBB84_102
.LBB84_78:
	addi r28, r0, 1022
	sgt r28, r9, r28
	or  r28, r7, r28
	andi r28, r28, 1
	bne r28, r27, .LBB84_75
.LBB84_79:
	bge r8, r9, .LBB84_83
.LBB84_80:
	ldw r28, sp+24
.LBB84_81:
	addi r27, r8, 1
	add r8, r8, r1
	stb r8+0, r19
	add r8, r27, r0
	bne r9, r27, .LBB84_81
.LBB84_82:
	stw r5+0, r9
	add r8, r9, r0
	jal r0, .LBB84_84
.LBB84_83:
	ldw r28, sp+24
.LBB84_84:
	add r27, r9, r1
	stb r27+0, lr
	addi lr, r9, 1
	stw r6+0, lr
	blt r9, r8, .LBB84_86
.LBB84_85:
	stw r5+0, lr
	add r8, lr, r0
.LBB84_86:
	add r9, lr, r0
	jal r0, .LBB84_76
.LBB84_87:
	stw r14+0, r10
	jal r0, .LBB84_14
.LBB84_88:
	addi r1, r5, 1
	stw r14+0, r1
	add r1, fp, r1
	ldbu r1, r1+0
	addi r28, r0, 0
	addi r27, r0, 1
	addi r3, r1, -76
	addi r1, r0, 38
	bgtu r3, r1, .LBB84_108
.LBB84_89:
	addi r1, r0, -1
	slli r3, r3, 2
	lui r4, %hi(.LJTI84_2)
	addi r4, r4, %lo(.LJTI84_2)
	add r3, r4, r3
	ldw r3, r3+0
	add r6, r27, r0
	jalr r0, r3, 0
.LBB84_90:
	addi r1, r0, 1
.LBB84_91:
	addi r3, r5, 2
	stw r14+0, r3
	addi r6, r0, 0
	add r28, r1, r0
.LBB84_92:
	ldw r1, r14+0
	add r1, fp, r1
	ldb r1, r1+0
	addi r3, r1, -58
	add r1, r27, r0
	bltu r3, lr, .LBB84_94
.LBB84_93:
	add fp, r6, r0
	jal r31, fio_number
	add r6, fp, r0
.LBB84_94:
	ldbu r4, r23+0
	mul r3, r1, r28
	addi r1, r1, -1
	bne r4, r27, .LBB84_103
.LBB84_95:
	lui r4, %hi(fio_rpos)
	addi r4, r4, %lo(fio_rpos)
	jal r0, .LBB84_104
.LBB84_96:
	addi r1, r5, 1
	stw r14+0, r1
	sub r1, r0, r27
	xor r3, r27, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	lui r3, %hi(fio_scale)
	addi r3, r3, %lo(fio_scale)
	stw r3+0, r1
	ldw r28, sp+24
	jal r0, .LBB84_14
.LBB84_97:
	addi r1, r5, 1
	stw r14+0, r1
	ldbu r3, r23+0
	addi r1, r0, 1
	bne r3, r1, .LBB84_105
.LBB84_98:
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	ldw r3, r1+0
	add r3, r3, r27
	stw r1+0, r3
	ldw r28, sp+24
	jal r0, .LBB84_14
.LBB84_99:
	addi r1, r6, -65
	addi r3, r0, 43
	bgtu r1, r3, .LBB84_101
.LBB84_100:
	slli r1, r1, 2
	lui r3, %hi(.LJTI84_3)
	addi r3, r3, %lo(.LJTI84_3)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB84_101:
	addi r1, r5, 1
	stw r14+0, r1
	ldw r28, sp+24
	jal r0, .LBB84_14
.LBB84_102:
	stw r14+0, r3
	jal r0, .LBB84_14
.LBB84_103:
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
.LBB84_104:
	ldw r5, r4+0
	add r3, r5, r3
	xor r1, r1, r3
	sub r5, r0, r6
	and r1, r1, r5
	xor r1, r3, r1
	addi r3, r0, 0
	sgt r3, r1, r3
	sub r3, r0, r3
	and r1, r1, r3
	stw r4+0, r1
	ldw r28, sp+24
	jal r0, .LBB84_14
.LBB84_105:
	ldw r28, sp+24
	blt r27, r1, .LBB84_14
.LBB84_106:
	lui r1, %hi(fio_col)
	addi r1, r1, %lo(fio_col)
	ldw r3, r1+0
	add r3, r3, r27
	stw r1+0, r3
	jal r0, .LBB84_14
.LBB84_107:
	ldw r28, sp+24
	stw r14+0, r3
	jal r0, .LBB84_14
.LBB84_108:
	add r6, r27, r0
	jal r0, .LBB84_92
.LBB84_109:
	addi r1, r5, 1
	stw r14+0, r1
	add r1, fp, r1
	ldb r1, r1+0
	addi r3, r1, -58
	addi r11, r0, 0
	add r1, r11, r0
	bltu r3, lr, .LBB84_111
.LBB84_110:
	add r12, lr, r0
	jal r31, fio_number
	add lr, r12, r0
.LBB84_111:
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
	bne r3, r4, .LBB84_113
.LBB84_112:
	addi r1, r1, 1
	stw r14+0, r1
	add r11, lr, r0
	jal r31, fio_number
	add lr, r11, r0
	stw r12+0, r1
.LBB84_113:
	lui r11, %hi(fio_e)
	addi r11, r11, %lo(fio_e)
	addi r1, r0, 0
	stw r11+0, r1
	ldw r1, r14+0
	add r3, fp, r1
	ldbu r3, r3+0
	ori  r3, r3, 32
	addi r4, r0, 101
	bne r3, r4, .LBB84_117
.LBB84_114:
	addi r3, r1, 1
	stw r14+0, r3
	add r3, fp, r3
	ldb r3, r3+0
	addi r3, r3, -58
	bgeu r3, lr, .LBB84_116
.LBB84_115:
	stw r14+0, r1
	jal r0, .LBB84_117
.LBB84_116:
	jal r31, fio_number
	stw r11+0, r1
.LBB84_117:
	lui r1, %hi(fio_rep)
	addi r1, r1, %lo(fio_rep)
	stw r1+0, r27
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
.LBB84_118:
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
.Lfunc_end84:
	.size	fio_next_desc, .Lfunc_end84-fio_next_desc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI84_0,@object
.LJTI84_0:
	.word	.LBB84_30
	.word	.LBB84_61
	.word	.LBB84_53
	.word	.LBB84_61
	.word	.LBB84_61
	.word	.LBB84_61
	.word	.LBB84_50
	.word	.LBB84_61
	.word	.LBB84_56
	.size	.LJTI84_0, 36
	.type	.LJTI84_1,@object
.LJTI84_1:
	.word	.LBB84_73
	.word	.LBB84_99
	.word	.LBB84_99
	.word	.LBB84_99
	.word	.LBB84_99
	.word	.LBB84_99
	.word	.LBB84_99
	.word	.LBB84_99
	.word	.LBB84_96
	.word	.LBB84_99
	.word	.LBB84_99
	.word	.LBB84_99
	.word	.LBB84_88
	.word	.LBB84_99
	.word	.LBB84_99
	.word	.LBB84_99
	.word	.LBB84_97
	.size	.LJTI84_1, 68
	.type	.LJTI84_2,@object
.LJTI84_2:
	.word	.LBB84_91
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_90
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_91
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_92
	.word	.LBB84_90
	.size	.LJTI84_2, 156
	.type	.LJTI84_3,@object
.LJTI84_3:
	.word	.LBB84_109
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_109
	.word	.LBB84_109
	.word	.LBB84_109
	.word	.LBB84_109
	.word	.LBB84_101
	.word	.LBB84_109
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_109
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_109
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_109
	.word	.LBB84_109
	.word	.LBB84_109
	.word	.LBB84_109
	.word	.LBB84_101
	.word	.LBB84_109
	.word	.LBB84_101
	.word	.LBB84_101
	.word	.LBB84_109
	.size	.LJTI84_3, 176
	.type	.LJTI84_4,@object
.LJTI84_4:
	.word	.LBB84_19
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_26
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_19
	.word	.LBB84_49
	.word	.LBB84_49
	.word	.LBB84_26
	.size	.LJTI84_4, 144
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
	addi r13, r0, 0
	lui r3, %hi(stdout)
	addi r3, r3, %lo(stdout)
	ldw r3, r3+0
	lui r14, %hi(fio_len)
	addi r14, r14, %lo(fio_len)
	ldw r5, r14+0
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	add r4, r5, r4
	stb r4+0, r13
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	stw r4+0, r13
	lui r12, %hi(fio_ibuf)
	addi r12, r12, %lo(fio_ibuf)
	ldw r4, r12+0
	beq r4, r13, .LBB85_11
.LBB85_1:
	lui r11, %hi(fio_irec)
	addi r11, r11, %lo(fio_irec)
	ldw r1, r11+0
	lui r3, %hi(fio_inrec)
	addi r3, r3, %lo(fio_inrec)
	ldw r3, r3+0
	lui r16, %hi(stderr)
	addi r16, r16, %lo(stderr)
	blt r1, r3, .LBB85_3
.LBB85_2:
	ldw r3, r16+0
	lui r4, %hi(.L.str.59)
	addi r4, r4, %lo(.L.str.59)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB85_3:
	ldw r5, r14+0
	lui r15, %hi(fio_ilen)
	addi r15, r15, %lo(fio_ilen)
	ldw r6, r15+0
	ble r5, r6, .LBB85_5
.LBB85_4:
	ldw r3, r16+0
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB85_5:
	ldw r1, r12+0
	ldw r3, r11+0
	ldw r4, r15+0
	mul r3, r4, r3
	add r3, r1, r3
	ldw r5, r14+0
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	jal r31, memcpy
	ldw r3, r14+0
	ldw r1, r15+0
	ble r1, r3, .LBB85_8
.LBB85_6:
	ldw r4, r12+0
	ldw r5, r11+0
	mul r5, r5, r1
	sub r1, r1, r3
	add r3, r3, r5
	add r3, r4, r3
	addi r4, r0, 32
.LBB85_7:
	stb r3+0, r4
	addi r1, r1, -1
	addi r3, r3, 1
	bne r1, r13, .LBB85_7
.LBB85_8:
	ldw r1, r11+0
	addi r1, r1, 1
	stw r11+0, r1
.LBB85_9:
	stw r14+0, r13
.LBB85_10:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB85_11:
	seq r4, r1, r13
	xor r3, r3, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r11, r1, r3
	lui r12, %hi(fio_unit)
	addi r12, r12, %lo(fio_unit)
	ldw r1, r12+0
	addi r3, r1, -1
	addi r15, r0, 30
	bgtu r3, r15, .LBB85_18
.LBB85_12:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r13, .LBB85_18
.LBB85_13:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r12, r1+0
	ble r5, r12, .LBB85_15
.LBB85_14:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.61)
	addi r4, r4, %lo(.L.str.61)
	add r6, r12, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB85_15:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	add r4, r11, r0
	jal r31, fputs
	ldw r1, r14+0
	ble r12, r1, .LBB85_9
.LBB85_16:
	sub r15, r12, r1
	addi r12, r0, 32
.LBB85_17:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fputc
	addi r15, r15, -1
	bne r15, r13, .LBB85_17
	jal r0, .LBB85_9
.LBB85_18:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	add r4, r11, r0
	jal r31, fputs
	addi r3, r0, 10
	add r4, r11, r0
	jal r31, fputc
	stw r14+0, r13
	ldw r1, r12+0
	addi r3, r1, -1
	bgtu r3, r15, .LBB85_10
.LBB85_19:
	slli r1, r1, 2
	lui r14, %hi(fio_uend)
	addi r14, r14, %lo(fio_uend)
	add r1, r1, r14
	ldw r1, r1+0
	blt r1, r13, .LBB85_10
.LBB85_20:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r12+0
	slli r3, r3, 2
	add r3, r3, r14
	stw r3+0, r1
	jal r0, .LBB85_10
.Lfunc_end85:
	.size	fio_flush_line, .Lfunc_end85-fio_flush_line
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
	bne r4, r3, .LBB86_4
.LBB86_1:
	addi r4, r0, -128
	lui r5, %hi(fio_uend+128)
	addi r5, r5, %lo(fio_uend+128)
	addi r6, r0, -1
.LBB86_2:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r3, .LBB86_2
.LBB86_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB86_4:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, fio_resolve
	add r12, r1, r0
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	beq r12, r1, .LBB86_8
.LBB86_5:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	beq r12, r1, .LBB86_8
.LBB86_6:
	add r3, r12, r0
	jal r31, fflush
	addi r1, r11, -1
	addi r3, r0, 30
	bgtu r1, r3, .LBB86_8
.LBB86_7:
	add r3, r12, r0
	jal r31, ftell
	slli r3, r11, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
.LBB86_8:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end86:
	.size	f77_endfile, .Lfunc_end86-f77_endfile
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
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r5, r11, r0
	add r6, r12, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fprintf
.Lfunc_end87:
	.size	f77_pause, .Lfunc_end87-f77_pause
                                        # -- End function
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
	add r12, r4, r0
	add r14, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	addi r13, r0, 0
	bne r3, r13, .LBB88_4
.LBB88_1:
	addi r3, r0, -128
	lui r4, %hi(fio_uend+128)
	addi r4, r4, %lo(fio_uend+128)
	addi r5, r0, -1
.LBB88_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r13, .LBB88_2
.LBB88_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB88_4:
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r13
	add r3, r14, r0
	add r4, r13, r0
	jal r31, fio_resolve
	lui r3, %hi(fio_in)
	addi r3, r3, %lo(fio_in)
	stw r3+0, r1
	lui r3, %hi(fio_unit)
	addi r3, r3, %lo(fio_unit)
	stw r3+0, r14
	addi r3, r14, -1
	addi r4, r0, 30
	bgtu r3, r4, .LBB88_11
.LBB88_5:
	slli r13, r14, 2
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r4, r13, r3
	addi r3, r0, 0
	stw r4+0, r3
	lui r4, %hi(fio_rec_next)
	addi r4, r4, %lo(fio_rec_next)
	ldw r15, r4+0
	stw r4+0, r3
	beq r14, r3, .LBB88_10
.LBB88_6:
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r13, r4
	ldw r4, r4+0
	beq r4, r3, .LBB88_10
.LBB88_7:
	bgt r15, r3, .LBB88_9
.LBB88_8:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.57)
	addi r4, r4, %lo(.L.str.57)
	add r14, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r1, r14, r0
.LBB88_9:
	addi r3, r15, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r4, r13, r4
	ldw r4, r4+0
	mul r4, r4, r3
	addi r5, r0, 0
	add r3, r1, r0
	jal r31, fseek
.LBB88_10:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r13, r1
	ldw r13, r1+0
	jal r0, .LBB88_16
.LBB88_11:
	lui r3, %hi(fio_rec_next)
	addi r3, r3, %lo(fio_rec_next)
	ldw r15, r3+0
	stw r3+0, r13
	addi r3, r14, -32
	addi r4, r0, -31
	bltu r3, r4, .LBB88_16
.LBB88_12:
	slli r14, r14, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r14, r3
	ldw r3, r3+0
	addi r13, r0, 0
	beq r3, r13, .LBB88_16
.LBB88_13:
	addi r13, r0, 0
	bgt r15, r13, .LBB88_15
.LBB88_14:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.57)
	addi r4, r4, %lo(.L.str.57)
	add r16, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r1, r16, r0
.LBB88_15:
	addi r3, r15, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r4, r14, r4
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r13, r0
	jal r31, fseek
.LBB88_16:
	lui r1, %hi(fio_bz)
	addi r1, r1, %lo(fio_bz)
	stw r1+0, r13
	add r3, r12, r0
	add r4, r11, r0
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, fio_rd_setup
.Lfunc_end88:
	.size	f77_rd_begin, .Lfunc_end88-f77_rd_begin
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
	lui r1, %hi(fio_unit)
	addi r1, r1, %lo(fio_unit)
	ldw r1, r1+0
	addi r12, r0, 0
	bgt r1, r12, .LBB89_2
.LBB89_1:
	lui r1, %hi(fio_bz)
	addi r1, r1, %lo(fio_bz)
	addi r4, r0, 0
	stw r1+0, r4
.LBB89_2:
	lui r1, %hi(fio_l_nulls)
	addi r1, r1, %lo(fio_l_nulls)
	stw r1+0, r12
	lui r1, %hi(fio_l_after)
	addi r1, r1, %lo(fio_l_after)
	stb r1+0, r12
	lui r14, %hi(fio_reading)
	addi r14, r14, %lo(fio_reading)
	addi r13, r0, 1
	stb r14+0, r13
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
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
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
	beq r3, r12, .LBB89_8
.LBB89_3:
	ldw r3, r1+0
	addi r6, r0, 32
.LBB89_4:
	add r7, r5, r3
	ldbu r7, r7+0
	bne r7, r6, .LBB89_6
.LBB89_5:
	addi r3, r3, 1
	stw r1+0, r3
	jal r0, .LBB89_4
.LBB89_6:
	addi r5, r0, 40
	bne r7, r5, .LBB89_8
.LBB89_7:
	stw r4+0, r3
	addi r3, r3, 1
	stw r1+0, r3
.LBB89_8:
	jal r31, fio_getrec
	beq r1, r12, .LBB89_11
.LBB89_9:
	addi r15, r0, 0
	stb r14+0, r15
	andi r1, r11, 3
	add r12, r13, r0
	bne r1, r15, .LBB89_11
.LBB89_10:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r12, r15, r0
.LBB89_11:
	add r1, r12, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end89:
	.size	fio_rd_setup, .Lfunc_end89-fio_rd_setup
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
	bne r9, r1, .LBB90_4
.LBB90_1:
	addi r9, r0, -128
	lui r10, %hi(fio_uend+128)
	addi r10, r10, %lo(fio_uend+128)
	addi lr, r0, -1
.LBB90_2:
	add r11, r9, r10
	stw r11+0, lr
	addi r9, r9, 4
	bne r9, r1, .LBB90_2
.LBB90_3:
	addi r9, r0, 1
	stb r8+0, r9
.LBB90_4:
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
.Lfunc_end90:
	.size	f77_rd_begin_int, .Lfunc_end90-f77_rd_begin_int
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
	beq r1, r14, .LBB91_5
.LBB91_1:
	lui r12, %hi(fio_ldone)
	addi r12, r12, %lo(fio_ldone)
	ldbu r1, r12+0
	bne r1, r14, .LBB91_59
.LBB91_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB91_59
.LBB91_3:
	addi r3, r0, 1
	bne r1, r3, .LBB91_27
.LBB91_4:
	stb r12+0, r3
	jal r0, .LBB91_59
.LBB91_5:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	beq r1, r14, .LBB91_11
.LBB91_6:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB91_11
.LBB91_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB91_12
.LBB91_8:
	addi r3, r0, 0
	beq r1, r3, .LBB91_59
.LBB91_9:
	addi r3, r0, 65
	bne r1, r3, .LBB91_30
.LBB91_10:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB91_55
.LBB91_11:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB91_8
.LBB91_12:
	addi r3, r0, 73
	beq r1, r3, .LBB91_35
.LBB91_13:
	addi r3, r0, 76
	bne r1, r3, .LBB91_30
.LBB91_14:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	addi r1, r0, 1
	lui r4, %hi(fio_rpos)
	addi r4, r4, %lo(fio_rpos)
	add r19, r14, r0
	blt r3, r1, .LBB91_31
.LBB91_15:
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
	jal r0, .LBB91_19
.LBB91_16:
	addi r21, r19, 1
	addi r23, sp, 28
	add r19, r23, r19
	stb r19+0, r22
	add r19, r21, r0
.LBB91_17:
	add r21, r16, r0
.LBB91_18:
	addi r20, r20, -1
	addi r5, r5, 1
	beq r20, r8, .LBB91_31
.LBB91_19:
	add r22, r9, r0
	bge r5, r6, .LBB91_21
.LBB91_20:
	add r22, r5, r10
	ldb r22, r22+0
.LBB91_21:
	beq r22, r9, .LBB91_23
.LBB91_22:
	bne r22, r17, .LBB91_26
.LBB91_23:
	beq r7, r8, .LBB91_18
.LBB91_24:
	beq r21, r8, .LBB91_18
.LBB91_25:
	add r22, lr, r0
	blt r19, r15, .LBB91_16
	jal r0, .LBB91_18
.LBB91_26:
	ble r19, r18, .LBB91_16
	jal r0, .LBB91_17
.LBB91_27:
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
	bgtu r4, r5, .LBB91_60
.LBB91_28:
	slli r4, r4, 2
	lui r5, %hi(.LJTI91_0)
	addi r5, r5, %lo(.LJTI91_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB91_29:
	andi r3, r3, 255
	addi r4, r0, 46
	seq r3, r3, r4
	add r1, r3, r1
	ldbu r1, r1+0
	andi r1, r1, 223
	addi r3, r0, 84
	seq r1, r1, r3
	stw r11+0, r1
	jal r0, .LBB91_59
.LBB91_30:
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
	jal r0, .LBB91_54
.LBB91_31:
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
	ble r3, r4, .LBB91_50
.LBB91_32:
	addi r4, r0, 84
	beq r3, r4, .LBB91_54
.LBB91_33:
	addi r4, r0, 116
	beq r3, r4, .LBB91_54
.LBB91_34:
	addi r1, r0, 102
	beq r3, r1, .LBB91_53
	jal r0, .LBB91_52
.LBB91_35:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	addi r1, r0, 0
	addi r4, r0, 1
	add r17, r1, r0
	blt r3, r4, .LBB91_48
.LBB91_36:
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
	jal r0, .LBB91_40
.LBB91_37:
	addi r19, r17, 1
	addi r21, sp, 28
	add r17, r21, r17
	stb r17+0, r20
	add r17, r19, r0
.LBB91_38:
	add r19, r4, r0
.LBB91_39:
	addi r18, r18, -1
	addi r5, r5, 1
	beq r18, r8, .LBB91_48
.LBB91_40:
	add r20, r9, r0
	bge r5, r6, .LBB91_42
.LBB91_41:
	add r20, r5, r10
	ldb r20, r20+0
.LBB91_42:
	beq r20, r9, .LBB91_44
.LBB91_43:
	bne r20, r15, .LBB91_47
.LBB91_44:
	beq r7, r8, .LBB91_39
.LBB91_45:
	beq r19, r8, .LBB91_39
.LBB91_46:
	add r20, lr, r0
	blt r17, r14, .LBB91_37
	jal r0, .LBB91_39
.LBB91_47:
	ble r17, r16, .LBB91_37
	jal r0, .LBB91_38
.LBB91_48:
	addi r4, sp, 28
	add r4, r4, r17
	stb r4+0, r1
	lui r4, %hi(fio_rpos)
	addi r4, r4, %lo(fio_rpos)
	ldw r5, r4+0
	add r3, r5, r3
	stw r4+0, r3
	ldbu r3, sp+28
	beq r3, r1, .LBB91_54
.LBB91_49:
	addi r3, sp, 28
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	jal r0, .LBB91_54
.LBB91_50:
	addi r1, r0, 0
	beq r3, r1, .LBB91_53
.LBB91_51:
	addi r1, r0, 70
	beq r3, r1, .LBB91_53
.LBB91_52:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.63)
	addi r4, r4, %lo(.L.str.63)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB91_53:
	addi r1, r0, 0
.LBB91_54:
	stw r11+0, r1
.LBB91_55:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB91_57
.LBB91_56:
	addi r1, r1, -1
	stw r13+0, r1
.LBB91_57:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB91_59
.LBB91_58:
	stw r12+0, r1
.LBB91_59:
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
.LBB91_60:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	stw r11+0, r1
	jal r0, .LBB91_59
.Lfunc_end91:
	.size	f77_rd_i, .Lfunc_end91-f77_rd_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI91_0,@object
.LJTI91_0:
	.word	.LBB91_29
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_29
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_29
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_29
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_60
	.word	.LBB91_29
	.size	.LJTI91_0, 144
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function fio_list_tok
	.type	fio_list_tok,@function
fio_list_tok:                           # @fio_list_tok
# %bb.0:
	addi sp, sp, -80
	stw sp+0, lr
	stw sp+76, r11
	stw sp+72, r12
	stw sp+68, r13
	stw sp+64, r14
	stw sp+60, r15
	stw sp+56, r16
	stw sp+52, r17
	stw sp+48, r18
	stw sp+44, r19
	stw sp+40, r20
	stw sp+36, r21
	stw sp+32, r22
	stw sp+28, r23
	stw sp+24, r24
	lui r16, %hi(fio_l_nulls)
	addi r16, r16, %lo(fio_l_nulls)
	ldw r1, r16+0
	addi r23, r0, 1
	blt r1, r23, .LBB92_2
.LBB92_1:
	addi r1, r1, -1
	stw r16+0, r1
	addi r11, r0, 2
	jal r0, .LBB92_5
.LBB92_2:
	lui r17, %hi(fio_l_pending)
	addi r17, r17, %lo(fio_l_pending)
	ldw r1, r17+0
	addi r19, r0, 0
	ble r1, r19, .LBB92_6
.LBB92_3:
	addi r1, r1, -1
	stw r17+0, r1
.LBB92_4:
	addi r11, r0, 0
.LBB92_5:
	add r1, r11, r0
	ldw r24, sp+24
	ldw r23, sp+28
	ldw r22, sp+32
	ldw r21, sp+36
	ldw r20, sp+40
	ldw r19, sp+44
	ldw r18, sp+48
	ldw r17, sp+52
	ldw r16, sp+56
	ldw r15, sp+60
	ldw r14, sp+64
	ldw r13, sp+68
	ldw r12, sp+72
	ldw r11, sp+76
	ldw lr, sp+0
	addi sp, sp, 80
	jalr r0, r31, 0
.LBB92_6:
	lui r14, %hi(fio_rpos)
	addi r14, r14, %lo(fio_rpos)
	lui r20, %hi(fio_rlen)
	addi r20, r20, %lo(fio_rlen)
	lui r18, %hi(fio_rec)
	addi r18, r18, %lo(fio_rec)
	addi r21, r0, 38
	lui r24, %hi(.LJTI92_0)
	addi r24, r24, %lo(.LJTI92_0)
	lui r13, %hi(fio_l_after)
	addi r13, r13, %lo(fio_l_after)
	addi r11, r0, 2
	lui r15, %hi(stderr)
	addi r15, r15, %lo(stderr)
	lui r12, %hi(.L.str.3)
	addi r12, r12, %lo(.L.str.3)
	jal r0, .LBB92_8
.LBB92_7:
	addi r1, r3, 1
	stw r14+0, r1
.LBB92_8:
	ldw r3, r14+0
	ldw r1, r20+0
	bge r3, r1, .LBB92_13
.LBB92_9:
	add r4, r3, r18
	ldbu r22, r4+0
	addi r4, r22, -9
	bgtu r4, r21, .LBB92_27
.LBB92_10:
	slli r4, r4, 2
	add r4, r24, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB92_11:
	addi r1, r3, 1
	stw r14+0, r1
	ldbu r1, r13+0
	bne r1, r23, .LBB92_5
.LBB92_12:
	stb r13+0, r19
	jal r0, .LBB92_8
.LBB92_13:
	jal r31, fio_getrec
	beq r1, r19, .LBB92_8
.LBB92_14:
	ldw r3, r15+0
	add r4, r12, r0
	jal r31, fprintf
	add r3, r11, r0
	jal r31, exit
	jal r0, .LBB92_8
.LBB92_15:
	addi r1, r3, 1
	stw r14+0, r1
	addi r17, r0, 126
	lui r16, %hi(fio_ltok)
	addi r16, r16, %lo(fio_ltok)
	lui r11, %hi(.L.str.3)
	addi r11, r11, %lo(.L.str.3)
	addi r12, r0, 2
	add r21, r19, r0
	jal r0, .LBB92_17
.LBB92_16:
	bleu r21, r17, .LBB92_24
.LBB92_17:
	ldw r1, r14+0
	ldw r4, r20+0
	blt r1, r4, .LBB92_20
.LBB92_18:
	jal r31, fio_getrec
	beq r1, r19, .LBB92_17
.LBB92_19:
	ldw r3, r15+0
	add r4, r11, r0
	jal r31, fprintf
	add r3, r12, r0
	jal r31, exit
	jal r0, .LBB92_17
.LBB92_20:
	addi r5, r1, 1
	stw r14+0, r5
	add r3, r1, r18
	ldbu r3, r3+0
	bne r3, r22, .LBB92_16
.LBB92_21:
	bge r5, r4, .LBB92_25
.LBB92_22:
	add r4, r5, r18
	ldbu r4, r4+0
	bne r4, r22, .LBB92_25
.LBB92_23:
	addi r1, r1, 2
	stw r14+0, r1
	jal r0, .LBB92_16
.LBB92_24:
	addi r1, r21, 1
	add r4, r21, r16
	stb r4+0, r3
	add r21, r1, r0
	jal r0, .LBB92_17
.LBB92_25:
	add r1, r21, r16
	addi r11, r0, 0
	stb r1+0, r11
	addi r1, r0, 1
	stb r13+0, r1
	jal r0, .LBB92_5
.LBB92_26:
	addi r1, r3, 1
	stw r14+0, r1
	add r11, r23, r0
	jal r0, .LBB92_5
.LBB92_27:
	ldw r3, r14+0
	addi r19, r0, 0
	add r4, r19, r0
	bge r3, r1, .LBB92_34
.LBB92_28:
	addi r4, r0, 0
	addi r5, r0, 126
	lui r6, %hi(fio_ltok)
	addi r6, r6, %lo(fio_ltok)
	lui r7, %hi(.LJTI92_1)
	addi r7, r7, %lo(.LJTI92_1)
	jal r0, .LBB92_30
.LBB92_29:
	addi r3, r3, 1
	stw r14+0, r3
	beq r1, r3, .LBB92_34
.LBB92_30:
	add r8, r3, r18
	ldbu r8, r8+0
	addi r9, r8, -9
	bgtu r9, r21, .LBB92_32
.LBB92_31:
	slli r9, r9, 2
	add r9, r7, r9
	ldw r9, r9+0
	jalr r0, r9, 0
.LBB92_32:
	bgt r4, r5, .LBB92_29
.LBB92_33:
	addi r9, r4, 1
	add r4, r4, r6
	stb r4+0, r8
	add r4, r9, r0
	jal r0, .LBB92_29
.LBB92_34:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	add r3, r4, r1
	stb r3+0, r19
	addi r3, r0, 1
	stb r13+0, r3
	addi r5, r0, -11
.LBB92_35:
	add r4, r19, r1
	ldb r4, r4+0
	addi r6, r4, -58
	addi r19, r19, 1
	bgtu r6, r5, .LBB92_35
.LBB92_36:
	beq r19, r3, .LBB92_4
.LBB92_37:
	addi r1, r0, 42
	bne r4, r1, .LBB92_4
.LBB92_38:
	lui r14, %hi(fio_ltok)
	addi r14, r14, %lo(fio_ltok)
	addi r12, r0, 0
	addi r5, r0, 10
	add r3, r14, r0
	add r4, r12, r0
	jal r31, strtol
	add r13, r1, r0
	bne r1, r12, .LBB92_40
.LBB92_39:
	ldw r3, r15+0
	lui r4, %hi(.L.str.62)
	addi r4, r4, %lo(.L.str.62)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB92_40:
	add r14, r19, r14
	ldbu r15, r14+0
	beq r15, r12, .LBB92_44
.LBB92_41:
	addi r1, r0, 2
	blt r13, r1, .LBB92_43
.LBB92_42:
	addi r1, r13, -1
	stw r17+0, r1
.LBB92_43:
	add r3, r14, r0
	jal r31, strlen
	addi r5, r1, 1
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	add r4, r14, r0
	jal r31, memmove
	jal r0, .LBB92_45
.LBB92_44:
	addi r1, r13, -1
	stw r16+0, r1
.LBB92_45:
	bne r15, r12, .LBB92_4
	jal r0, .LBB92_5
.Lfunc_end92:
	.size	fio_list_tok, .Lfunc_end92-fio_list_tok
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI92_0,@object
.LJTI92_0:
	.word	.LBB92_7
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_7
	.word	.LBB92_27
	.word	.LBB92_15
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_15
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_11
	.word	.LBB92_27
	.word	.LBB92_27
	.word	.LBB92_26
	.size	.LJTI92_0, 156
	.type	.LJTI92_1,@object
.LJTI92_1:
	.word	.LBB92_34
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_34
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_34
	.word	.LBB92_32
	.word	.LBB92_32
	.word	.LBB92_34
	.size	.LJTI92_1, 156
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_in_real
	.type	.LCPI93_0,@object
.LCPI93_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI93_0, 8
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
	blt r3, r5, .LBB93_13
.LBB93_1:
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
	jal r0, .LBB93_5
.LBB93_2:
	addi r20, r18, 1
	stw sp+16, r1
	add r1, sp, r0
	addi r1, r1, 2047
	addi r22, r1, 49
	ldw r1, sp+16
	add r18, r22, r18
	stb r18+0, r21
	add r18, r20, r0
.LBB93_3:
	add r20, r15, r0
.LBB93_4:
	addi r19, r19, -1
	addi r7, r7, 1
	beq r19, lr, .LBB93_13
.LBB93_5:
	add r21, r11, r0
	bge r7, r9, .LBB93_7
.LBB93_6:
	add r21, r7, r12
	ldb r21, r21+0
.LBB93_7:
	beq r21, r11, .LBB93_9
.LBB93_8:
	bne r21, r16, .LBB93_12
.LBB93_9:
	beq r10, lr, .LBB93_4
.LBB93_10:
	beq r20, lr, .LBB93_4
.LBB93_11:
	add r21, r13, r0
	blt r18, r14, .LBB93_2
	jal r0, .LBB93_4
.LBB93_12:
	bgt r18, r17, .LBB93_3
	jal r0, .LBB93_2
.LBB93_13:
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
	lui r9, %hi(.LCPI93_0)
	addi r9, r9, %lo(.LCPI93_0)
	ldw r7, r9+4
	ldw r6, r9+0
	beq r3, r8, .LBB93_66
.LBB93_14:
	addi r11, r0, 43
	beq r3, r11, .LBB93_17
.LBB93_15:
	addi r8, r0, 45
	bne r3, r8, .LBB93_18
.LBB93_16:
	addi r8, r0, 0
	addi r10, r0, 1
	jal r0, .LBB93_20
.LBB93_17:
	add r10, r5, r0
	jal r0, .LBB93_19
.LBB93_18:
	addi r10, r0, 0
.LBB93_19:
	add r8, r5, r0
.LBB93_20:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r3, r9, 49
	or  r3, r3, r10
	ldbu r21, r3+0
	addi r9, r0, 0
	beq r21, r9, .LBB93_42
.LBB93_21:
	addi r13, r0, 0
	addi r14, r0, 1
	addi r15, r0, 46
	addi r16, r0, -10
	addi r17, r0, 70
	lui r18, %hi(.LJTI93_0)
	addi r18, r18, %lo(.LJTI93_0)
	addi r19, r0, -1
	add lr, r14, r0
	add r12, r13, r0
	add r20, r13, r0
	add r3, r13, r0
.LBB93_22:
	andi r5, r21, 255
	bne r5, r15, .LBB93_24
.LBB93_23:
	add r20, r14, r0
	jal r0, .LBB93_34
.LBB93_24:
	slli r22, r21, 24
	srai r22, r22, 24
	addi r22, r22, -58
	bgeu r22, r16, .LBB93_31
.LBB93_25:
	addi r5, r5, -43
	bgtu r5, r17, .LBB93_40
.LBB93_26:
	slli r5, r5, 2
	add r5, r18, r5
	ldw r22, r5+0
	add r5, r13, r0
	add r21, r14, r0
	jalr r0, r22, 0
.LBB93_27:
	addi r5, r10, 1
	add r22, sp, r0
	addi r22, r22, 2047
	addi r21, r22, 49
	add r21, r21, r5
	ldbu r21, r21+0
	beq r21, r11, .LBB93_38
.LBB93_28:
	addi r22, r0, 45
	bne r21, r22, .LBB93_39
.LBB93_29:
	add r10, r5, r0
.LBB93_30:
	add r5, r13, r0
	add lr, r19, r0
	jal r0, .LBB93_35
.LBB93_31:
	addi r5, r0, 1022
	bgt r3, r5, .LBB93_33
.LBB93_32:
	addi r5, r3, 1
	addi r22, sp, 1072
	add r3, r22, r3
	stb r3+0, r21
	add r3, r5, r0
.LBB93_33:
	addi r5, r0, 0
	sne r5, r20, r5
	add r12, r12, r5
.LBB93_34:
	add r5, r14, r0
.LBB93_35:
	add r21, r14, r0
.LBB93_36:
	beq r21, r13, .LBB93_41
.LBB93_37:
	addi r10, r10, 1
	add r22, sp, r0
	addi r22, r22, 2047
	addi r21, r22, 49
	add r21, r21, r10
	ldbu r21, r21+0
	sne r22, r21, r13
	and r22, r22, r5
	bne r22, r13, .LBB93_22
	jal r0, .LBB93_41
.LBB93_38:
	add r10, r5, r0
.LBB93_39:
	add r5, r13, r0
	jal r0, .LBB93_35
.LBB93_40:
	add r5, r14, r0
	add r21, r13, r0
	bne r21, r13, .LBB93_37
.LBB93_41:
	addi r11, r0, 0
	seq r11, r20, r11
	xor r4, r4, r12
	sub r11, r0, r11
	and r4, r4, r11
	xor r4, r12, r4
	add r11, r9, r0
	beq r5, r9, .LBB93_43
	jal r0, .LBB93_47
.LBB93_42:
	add r3, r9, r0
	add lr, r5, r0
	add r11, r9, r0
	bne r5, r9, .LBB93_47
.LBB93_43:
	add r12, sp, r0
	addi r12, r12, 2047
	addi r11, r12, 49
	add r11, r11, r10
	ldbu r13, r11+0
	addi r11, r0, 0
	beq r13, r11, .LBB93_47
.LBB93_44:
	add r12, sp, r0
	addi r12, r12, 2047
	addi r11, r12, 49
	add r10, r10, r11
	addi r10, r10, 1
	addi r11, r0, 0
	addi r12, r0, 9
	addi r14, r0, 10
	add r15, r11, r0
.LBB93_45:
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
	bne r13, r11, .LBB93_45
.LBB93_46:
	mul r11, r15, lr
.LBB93_47:
	addi r10, sp, 1072
	add r10, r10, r3
	stb r10+0, r9
	beq r3, r9, .LBB93_66
.LBB93_48:
	sub r7, r3, r4
	lui r6, %hi(fio_scale)
	addi r6, r6, %lo(fio_scale)
	ldw r6, r6+0
	sub r5, r0, r5
	and r6, r6, r5
	addi r5, r0, 0
	addi r10, sp, 32
	bne r8, r5, .LBB93_50
.LBB93_49:
	addi r10, r1, 1
	addi r1, r0, 45
	stb sp+32, r1
	addi r5, r0, 1
.LBB93_50:
	sub r6, r11, r6
	addi r8, r0, 1
	blt r7, r8, .LBB93_55
.LBB93_51:
	blt r3, r8, .LBB93_61
.LBB93_52:
	xor r1, r7, r3
	slt r4, r7, r3
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r3, r1
	addi r4, sp, 32
	or  r7, r4, r5
	addi r4, r0, 0
.LBB93_53:
	addi r8, sp, 1072
	add r8, r8, r4
	ldbu r8, r8+0
	add r9, r7, r4
	stb r9+0, r8
	addi r4, r4, 1
	bne r1, r4, .LBB93_53
.LBB93_54:
	add r5, r5, r4
	jal r0, .LBB93_62
.LBB93_55:
	addi r1, sp, 32
	or  r1, r1, r5
	addi r9, r0, 48
	stb r1+0, r9
	addi r1, r5, 2
	addi r5, r0, 46
	stb r10+1, r5
	addi r5, r0, -1
	bgt r7, r5, .LBB93_58
.LBB93_56:
	sub r4, r4, r3
	addi r5, r0, 0
.LBB93_57:
	add r7, r1, r0
	addi r1, r1, 1
	addi r10, sp, 32
	add r7, r10, r7
	stb r7+0, r9
	addi r4, r4, -1
	bne r4, r5, .LBB93_57
.LBB93_58:
	blt r3, r8, .LBB93_65
.LBB93_59:
	addi r4, sp, 1072
	addi r5, r0, 0
.LBB93_60:
	add r7, r1, r0
	ldbu r8, r4+0
	addi r1, r1, 1
	addi r9, sp, 32
	add r7, r9, r7
	stb r7+0, r8
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r5, .LBB93_60
	jal r0, .LBB93_65
.LBB93_61:
	addi r4, r0, 0
.LBB93_62:
	addi r1, sp, 32
	add r1, r1, r5
	addi r7, r0, 46
	stb r1+0, r7
	addi r1, r5, 1
	ble r3, r4, .LBB93_65
.LBB93_63:
	sub r3, r3, r4
	addi r5, sp, 1072
	add r4, r5, r4
	addi r5, r0, 0
.LBB93_64:
	ldbu r7, r4+0
	addi r8, sp, 32
	add r8, r8, r1
	stb r8+0, r7
	addi r1, r1, 1
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r5, .LBB93_64
.LBB93_65:
	addi r11, sp, 32
	add r3, r11, r1
	addi r4, r0, 1040
	sub r4, r4, r1
	lui r5, %hi(.L.str.64)
	addi r5, r5, %lo(.L.str.64)
	jal r31, snprintf
	addi r4, r0, 0
	add r3, r11, r0
	jal r31, strtod
	add r6, r1, r0
	add r7, r2, r0
.LBB93_66:
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
.Lfunc_end93:
	.size	fio_in_real, .Lfunc_end93-fio_in_real
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI93_0,@object
.LJTI93_0:
	.word	.LBB93_36
	.word	.LBB93_40
	.word	.LBB93_30
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_27
	.word	.LBB93_27
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_27
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_27
	.word	.LBB93_27
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_40
	.word	.LBB93_27
	.size	.LJTI93_0, 284
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_rd_d
	.type	.LCPI94_0,@object
.LCPI94_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI94_0, 8
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
	beq r1, r12, .LBB94_5
.LBB94_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r12, .LBB94_40
.LBB94_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB94_40
.LBB94_3:
	addi r3, r0, 1
	bne r1, r3, .LBB94_27
.LBB94_4:
	stb r13+0, r3
	jal r0, .LBB94_40
.LBB94_5:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB94_10
.LBB94_6:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB94_10
.LBB94_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB94_11
.LBB94_8:
	addi r3, r0, 0
	beq r1, r3, .LBB94_40
.LBB94_9:
	addi r3, r0, 65
	beq r1, r3, .LBB94_26
	jal r0, .LBB94_31
.LBB94_10:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB94_8
.LBB94_11:
	addi r3, r0, 76
	beq r1, r3, .LBB94_26
.LBB94_12:
	addi r3, r0, 73
	bne r1, r3, .LBB94_31
.LBB94_13:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	addi r3, r3, %lo(fio_rpos)
	add r18, r14, r0
	blt r1, r4, .LBB94_32
.LBB94_14:
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
	jal r0, .LBB94_18
.LBB94_15:
	addi r20, r18, 1
	addi r22, sp, 32
	add r18, r22, r18
	stb r18+0, r21
	add r18, r20, r0
.LBB94_16:
	add r20, r4, r0
.LBB94_17:
	addi r19, r19, -1
	addi r5, r5, 1
	beq r19, r8, .LBB94_32
.LBB94_18:
	add r21, r9, r0
	bge r5, r6, .LBB94_20
.LBB94_19:
	add r21, r5, r10
	ldb r21, r21+0
.LBB94_20:
	beq r21, r9, .LBB94_22
.LBB94_21:
	bne r21, r16, .LBB94_25
.LBB94_22:
	beq r7, r8, .LBB94_17
.LBB94_23:
	beq r20, r8, .LBB94_17
.LBB94_24:
	add r21, lr, r0
	blt r18, r15, .LBB94_15
	jal r0, .LBB94_17
.LBB94_25:
	ble r18, r17, .LBB94_15
	jal r0, .LBB94_16
.LBB94_26:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB94_36
.LBB94_27:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	ldbu r6, r1+0
	add r7, r12, r0
	beq r6, r12, .LBB94_30
.LBB94_28:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r7, r1, r0
.LBB94_29:
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
	bne r6, r1, .LBB94_29
.LBB94_30:
	addi r3, sp, 32
	add r1, r3, r7
	stb r1+0, r12
	add r4, r12, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
	jal r0, .LBB94_40
.LBB94_31:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r4, r1+0
	jal r31, fio_in_real
	stw r11+4, r2
	stw r11+0, r1
	jal r0, .LBB94_36
.LBB94_32:
	addi r4, sp, 32
	add r4, r4, r18
	stb r4+0, r14
	ldw r4, r3+0
	add r1, r4, r1
	stw r3+0, r1
	ldbu r1, sp+32
	beq r1, r14, .LBB94_34
.LBB94_33:
	addi r3, sp, 32
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	fcvt.d.w r4, r1
	jal r0, .LBB94_35
.LBB94_34:
	lui r1, %hi(.LCPI94_0)
	addi r1, r1, %lo(.LCPI94_0)
	ldw r5, r1+4
	ldw r4, r1+0
.LBB94_35:
	stw r11+4, r5
	stw r11+0, r4
.LBB94_36:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB94_38
.LBB94_37:
	addi r1, r1, -1
	stw r13+0, r1
.LBB94_38:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB94_40
.LBB94_39:
	stw r12+0, r1
.LBB94_40:
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
.Lfunc_end94:
	.size	f77_rd_d, .Lfunc_end94-f77_rd_d
                                        # -- End function
	.p2align	2                               # -- Begin function fio_list_real
	.type	fio_list_real,@function
fio_list_real:                          # @fio_list_real
# %bb.0:
	addi sp, sp, -144
	stw sp+0, lr
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	ldbu r7, r1+0
	addi r4, r0, 0
	add r8, r4, r0
	beq r7, r4, .LBB95_3
.LBB95_1:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r5, r0, 81
	lui r6, %hi(fio_ltok+1)
	addi r6, r6, %lo(fio_ltok+1)
	add r8, r1, r0
.LBB95_2:
	add r9, r8, r0
	andi r8, r7, 223
	seq r10, r8, r3
	seq r8, r8, r5
	sub r10, r0, r10
	sub r8, r0, r8
	xori lr, r7, 69
	and r8, lr, r8
	xor r7, r7, r8
	xori r8, r7, 69
	and r8, r8, r10
	xor r7, r7, r8
	addi r8, sp, 16
	add r8, r8, r9
	stb r8+0, r7
	addi r8, r9, 1
	add r7, r9, r6
	ldbu r7, r7+0
	bne r7, r1, .LBB95_2
.LBB95_3:
	addi r3, sp, 16
	add r1, r3, r8
	stb r1+0, r4
	jal r31, strtod
	ldw lr, sp+0
	addi sp, sp, 144
	jalr r0, r31, 0
.Lfunc_end95:
	.size	fio_list_real, .Lfunc_end95-fio_list_real
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rd_r
	.type	.LCPI96_0,@object
.LCPI96_0:
	.word	0x00000000                      # float 0
	.size	.LCPI96_0, 4
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
	beq r1, r12, .LBB96_5
.LBB96_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r12, .LBB96_40
.LBB96_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB96_40
.LBB96_3:
	addi r3, r0, 1
	bne r1, r3, .LBB96_27
.LBB96_4:
	stb r13+0, r3
	jal r0, .LBB96_40
.LBB96_5:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB96_10
.LBB96_6:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB96_10
.LBB96_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB96_11
.LBB96_8:
	addi r3, r0, 0
	beq r1, r3, .LBB96_40
.LBB96_9:
	addi r3, r0, 65
	beq r1, r3, .LBB96_26
	jal r0, .LBB96_31
.LBB96_10:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB96_8
.LBB96_11:
	addi r3, r0, 76
	beq r1, r3, .LBB96_26
.LBB96_12:
	addi r3, r0, 73
	bne r1, r3, .LBB96_31
.LBB96_13:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	addi r3, r3, %lo(fio_rpos)
	add r18, r14, r0
	blt r1, r4, .LBB96_32
.LBB96_14:
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
	jal r0, .LBB96_18
.LBB96_15:
	addi r20, r18, 1
	addi r22, sp, 32
	add r18, r22, r18
	stb r18+0, r21
	add r18, r20, r0
.LBB96_16:
	add r20, r4, r0
.LBB96_17:
	addi r19, r19, -1
	addi r5, r5, 1
	beq r19, r8, .LBB96_32
.LBB96_18:
	add r21, r9, r0
	bge r5, r6, .LBB96_20
.LBB96_19:
	add r21, r5, r10
	ldb r21, r21+0
.LBB96_20:
	beq r21, r9, .LBB96_22
.LBB96_21:
	bne r21, r16, .LBB96_25
.LBB96_22:
	beq r7, r8, .LBB96_17
.LBB96_23:
	beq r20, r8, .LBB96_17
.LBB96_24:
	add r21, lr, r0
	blt r18, r15, .LBB96_15
	jal r0, .LBB96_17
.LBB96_25:
	ble r18, r17, .LBB96_15
	jal r0, .LBB96_16
.LBB96_26:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	jal r0, .LBB96_36
.LBB96_27:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	ldbu r6, r1+0
	add r7, r12, r0
	beq r6, r12, .LBB96_30
.LBB96_28:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r7, r1, r0
.LBB96_29:
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
	bne r6, r1, .LBB96_29
.LBB96_30:
	addi r3, sp, 32
	add r1, r3, r7
	stb r1+0, r12
	add r4, r12, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	jal r0, .LBB96_40
.LBB96_31:
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
	jal r0, .LBB96_35
.LBB96_32:
	addi r4, sp, 32
	add r4, r4, r18
	stb r4+0, r14
	ldw r4, r3+0
	add r1, r4, r1
	stw r3+0, r1
	ldbu r1, sp+32
	beq r1, r14, .LBB96_34
.LBB96_33:
	addi r3, sp, 32
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	fcvt.s.w r1, r1
	jal r0, .LBB96_35
.LBB96_34:
	lui r1, %hi(.LCPI96_0)
	addi r1, r1, %lo(.LCPI96_0)
	ldw r1, r1+0
.LBB96_35:
	stw r11+0, r1
.LBB96_36:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB96_38
.LBB96_37:
	addi r1, r1, -1
	stw r13+0, r1
.LBB96_38:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB96_40
.LBB96_39:
	stw r12+0, r1
.LBB96_40:
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
.Lfunc_end96:
	.size	f77_rd_r, .Lfunc_end96-f77_rd_r
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
	add r12, r4, r0
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r14, r0, 0
	beq r1, r14, .LBB97_5
.LBB97_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r14, .LBB97_32
.LBB97_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB97_32
.LBB97_3:
	addi r15, r0, 1
	bne r1, r15, .LBB97_27
.LBB97_4:
	stb r13+0, r15
	jal r0, .LBB97_32
.LBB97_5:
	lui r13, %hi(fio_desc)
	addi r13, r13, %lo(fio_desc)
	ldw r1, r13+0
	lui r15, %hi(fio_rep)
	addi r15, r15, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB97_7
.LBB97_6:
	ldw r3, r15+0
	addi r4, r0, 0
	bgt r3, r4, .LBB97_8
.LBB97_7:
	stw r13+0, r14
	jal r31, fio_next_desc
.LBB97_8:
	beq r1, r14, .LBB97_32
.LBB97_9:
	addi r3, r0, 65
	beq r1, r3, .LBB97_11
.LBB97_10:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB97_11:
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
	bge r1, r12, .LBB97_18
.LBB97_12:
	blt r12, r4, .LBB97_23
.LBB97_13:
	ldw r4, r3+0
	lui r5, %hi(fio_rlen)
	addi r5, r5, %lo(fio_rlen)
	ldw r5, r5+0
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	add r8, r14, r0
	jal r0, .LBB97_15
.LBB97_14:
	add r10, r11, r8
	stb r10+0, r9
	addi r8, r8, 1
	beq r12, r8, .LBB97_23
.LBB97_15:
	add r9, r6, r0
	bge r8, r1, .LBB97_14
.LBB97_16:
	add r10, r4, r8
	add r9, r6, r0
	bge r10, r5, .LBB97_14
.LBB97_17:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB97_14
.LBB97_18:
	blt r12, r4, .LBB97_23
.LBB97_19:
	ldw r5, r3+0
	lui r4, %hi(fio_rlen)
	addi r4, r4, %lo(fio_rlen)
	ldw r4, r4+0
	sub r8, r0, r12
	add r5, r1, r5
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	jal r0, .LBB97_21
.LBB97_20:
	stb r11+0, r9
	addi r9, r8, 1
	sltu r10, r9, r8
	addi r11, r11, 1
	add r8, r9, r0
	bne r10, r14, .LBB97_23
.LBB97_21:
	add r10, r5, r8
	add r9, r6, r0
	bge r10, r4, .LBB97_20
.LBB97_22:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB97_20
.LBB97_23:
	ldw r4, r3+0
	add r1, r4, r1
	stw r3+0, r1
	ldw r1, r15+0
	addi r3, r0, 1
	blt r1, r3, .LBB97_25
.LBB97_24:
	addi r1, r1, -1
	stw r15+0, r1
.LBB97_25:
	ldw r1, r15+0
	bne r1, r14, .LBB97_32
.LBB97_26:
	stw r13+0, r14
	jal r0, .LBB97_32
.LBB97_27:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r15, .LBB97_32
.LBB97_28:
	addi r3, r0, 32
	jal r0, .LBB97_30
.LBB97_29:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB97_32
.LBB97_30:
	add r4, r3, r0
	bge r14, r1, .LBB97_29
.LBB97_31:
	add r4, r14, r13
	ldbu r4, r4+0
	jal r0, .LBB97_29
.LBB97_32:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end97:
	.size	f77_rd_s, .Lfunc_end97-f77_rd_s
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rd_c
	.type	.LCPI98_0,@object
.LCPI98_0:
	.word	0x00000000                      # float 0
	.size	.LCPI98_0, 4
	.text
	.globl	f77_rd_c
	.p2align	2
	.type	f77_rd_c,@function
f77_rd_c:                               # @f77_rd_c
# %bb.0:
	addi sp, sp, -336
	stw sp+0, lr
	stw sp+332, r11
	stw sp+328, r12
	stw sp+324, r13
	stw sp+320, r14
	stw sp+316, r15
	stw sp+312, r16
	stw sp+308, r17
	stw sp+304, r18
	stw sp+300, r19
	stw sp+296, r20
	stw sp+292, r21
	stw sp+288, r22
	stw sp+284, r23
	stw sp+280, r24
	stw sp+276, r25
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r13, r0, 0
	beq r1, r13, .LBB98_5
.LBB98_1:
	lui r12, %hi(fio_ldone)
	addi r12, r12, %lo(fio_ldone)
	ldbu r1, r12+0
	bne r1, r13, .LBB98_35
.LBB98_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB98_35
.LBB98_3:
	addi r15, r0, 1
	bne r1, r15, .LBB98_6
.LBB98_4:
	stb r12+0, r15
	jal r0, .LBB98_35
.LBB98_5:
	add r3, r11, r0
	jal r31, f77_rd_r
	addi r3, r11, 4
	ldw r25, sp+276
	ldw r24, sp+280
	ldw r23, sp+284
	ldw r22, sp+288
	ldw r21, sp+292
	ldw r20, sp+296
	ldw r19, sp+300
	ldw r18, sp+304
	ldw r17, sp+308
	ldw r16, sp+312
	ldw r15, sp+316
	ldw r14, sp+320
	ldw r13, sp+324
	ldw r12, sp+328
	ldw r11, sp+332
	ldw lr, sp+0
	addi sp, sp, 336
	jal r0, f77_rd_r
.LBB98_6:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	ldbu r3, r12+0
	addi r1, r0, 40
	bne r3, r1, .LBB98_29
.LBB98_7:
	lui r16, %hi(fio_ltok+1)
	addi r16, r16, %lo(fio_ltok+1)
	ldbu r4, r16+0
	addi r17, r0, 0
	add r18, r17, r0
	beq r4, r17, .LBB98_12
.LBB98_8:
	addi r1, r0, 0
	lui r3, %hi(fio_ltok+2)
	addi r3, r3, %lo(fio_ltok+2)
	addi r5, r0, 126
	add r18, r1, r0
	jal r0, .LBB98_10
.LBB98_9:
	ldbu r4, r3+0
	addi r3, r3, 1
	beq r4, r1, .LBB98_12
.LBB98_10:
	bgt r18, r5, .LBB98_9
.LBB98_11:
	addi r6, r18, 1
	addi r7, sp, 20
	add r7, r7, r18
	stb r7+0, r4
	add r18, r6, r0
	jal r0, .LBB98_9
.LBB98_12:
	lui r19, %hi(fio_rpos)
	addi r19, r19, %lo(fio_rpos)
	lui r20, %hi(fio_rlen)
	addi r20, r20, %lo(fio_rlen)
	addi r21, r0, 126
	lui r22, %hi(fio_rec)
	addi r22, r22, %lo(fio_rec)
	lui r23, %hi(stderr)
	addi r23, r23, %lo(stderr)
	lui r13, %hi(.L.str.3)
	addi r13, r13, %lo(.L.str.3)
	addi r14, r0, 2
	addi r24, r0, 41
.LBB98_13:
	addi r1, sp, 20
	add r25, r1, r18
	jal r0, .LBB98_15
.LBB98_14:
	addi r3, r1, 1
	stw r19+0, r3
	ble r18, r21, .LBB98_20
.LBB98_15:
	beq r18, r17, .LBB98_17
.LBB98_16:
	ldbu r1, r25+-1
	beq r1, r24, .LBB98_21
.LBB98_17:
	ldw r1, r19+0
	ldw r3, r20+0
	blt r1, r3, .LBB98_14
.LBB98_18:
	jal r31, fio_getrec
	beq r1, r17, .LBB98_15
.LBB98_19:
	ldw r3, r23+0
	add r4, r13, r0
	jal r31, fprintf
	add r3, r14, r0
	jal r31, exit
	jal r0, .LBB98_15
.LBB98_20:
	add r1, r1, r22
	ldbu r1, r1+0
	addi r3, r18, 1
	addi r4, sp, 20
	add r4, r4, r18
	stb r4+0, r1
	add r18, r3, r0
	jal r0, .LBB98_13
.LBB98_21:
	addi r13, r0, 0
	stb r25+-1, r13
	addi r3, sp, 20
	addi r4, r0, 44
	jal r31, strchr
	beq r1, r13, .LBB98_33
.LBB98_22:
	add r14, r1, r0
	stb r1+0, r13
	addi r4, sp, 20
	add r3, r12, r0
	jal r31, strcpy
	ldbu r5, r12+0
	add r6, r13, r0
	beq r5, r13, .LBB98_25
.LBB98_23:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	add r6, r1, r0
.LBB98_24:
	add r7, r6, r0
	andi r6, r5, 223
	seq r8, r6, r3
	seq r6, r6, r4
	sub r8, r0, r8
	sub r6, r0, r6
	xori r9, r5, 69
	and r6, r9, r6
	xor r5, r5, r6
	xori r6, r5, 69
	and r6, r6, r8
	xor r5, r5, r6
	addi r6, sp, 148
	add r6, r6, r7
	stb r6+0, r5
	addi r6, r7, 1
	add r5, r7, r16
	ldbu r5, r5+0
	bne r5, r1, .LBB98_24
.LBB98_25:
	addi r3, sp, 148
	add r1, r3, r6
	stb r1+0, r13
	add r4, r13, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	addi r4, r14, 1
	add r3, r12, r0
	jal r31, strcpy
	ldbu r5, r12+0
	add r6, r13, r0
	beq r5, r13, .LBB98_28
.LBB98_26:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	add r6, r1, r0
.LBB98_27:
	add r7, r6, r0
	andi r6, r5, 223
	seq r8, r6, r3
	seq r6, r6, r4
	sub r8, r0, r8
	sub r6, r0, r6
	xori r9, r5, 69
	and r6, r9, r6
	xor r5, r5, r6
	xori r6, r5, 69
	and r6, r6, r8
	xor r5, r5, r6
	addi r6, sp, 148
	add r6, r6, r7
	stb r6+0, r5
	addi r6, r7, 1
	add r5, r7, r16
	ldbu r5, r5+0
	bne r5, r1, .LBB98_27
.LBB98_28:
	addi r3, sp, 148
	add r1, r3, r6
	stb r1+0, r13
	add r4, r13, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	jal r0, .LBB98_34
.LBB98_29:
	add r7, r13, r0
	beq r3, r13, .LBB98_32
.LBB98_30:
	addi r1, r0, 0
	addi r4, r0, 68
	addi r5, r0, 81
	lui r6, %hi(fio_ltok+1)
	addi r6, r6, %lo(fio_ltok+1)
	add r7, r1, r0
.LBB98_31:
	add r8, r7, r0
	andi r7, r3, 223
	seq r9, r7, r4
	seq r7, r7, r5
	sub r9, r0, r9
	sub r7, r0, r7
	xori r10, r3, 69
	and r7, r10, r7
	xor r3, r3, r7
	xori r7, r3, 69
	and r7, r7, r9
	xor r3, r3, r7
	addi r7, sp, 148
	add r7, r7, r8
	stb r7+0, r3
	addi r7, r8, 1
	add r3, r8, r6
	ldbu r3, r3+0
	bne r3, r1, .LBB98_31
.LBB98_32:
	addi r3, sp, 148
	add r1, r3, r7
	stb r1+0, r13
	add r4, r13, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	stw r11+4, r13
	jal r0, .LBB98_35
.LBB98_33:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r4, sp, 20
	jal r31, strcpy
	jal r31, fio_list_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	lui r1, %hi(.LCPI98_0)
	addi r1, r1, %lo(.LCPI98_0)
	ldw r1, r1+0
.LBB98_34:
	stw r11+4, r1
	lui r1, %hi(fio_l_after)
	addi r1, r1, %lo(fio_l_after)
	stb r1+0, r15
.LBB98_35:
	ldw r25, sp+276
	ldw r24, sp+280
	ldw r23, sp+284
	ldw r22, sp+288
	ldw r21, sp+292
	ldw r20, sp+296
	ldw r19, sp+300
	ldw r18, sp+304
	ldw r17, sp+308
	ldw r16, sp+312
	ldw r15, sp+316
	ldw r14, sp+320
	ldw r13, sp+324
	ldw r12, sp+328
	ldw r11, sp+332
	ldw lr, sp+0
	addi sp, sp, 336
	jalr r0, r31, 0
.Lfunc_end98:
	.size	f77_rd_c, .Lfunc_end98-f77_rd_c
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
.Lfunc_end99:
	.size	f77_rd_end, .Lfunc_end99-f77_rd_end
                                        # -- End function
	.globl	f77_ipow                        # -- Begin function f77_ipow
	.p2align	2
	.type	f77_ipow,@function
f77_ipow:                               # @f77_ipow
# %bb.0:
	addi r1, r0, 1
	addi r5, r0, 0
	blt r4, r5, .LBB100_3
.LBB100_1:
	beq r4, r5, .LBB100_8
.LBB100_2:
	andi r6, r4, 1
	seq r6, r6, r5
	sub r6, r0, r6
	xori r7, r3, 1
	and r6, r7, r6
	xor r6, r3, r6
	mul r1, r6, r1
	mul r3, r3, r3
	srli r4, r4, 1
	bne r4, r5, .LBB100_2
	jal r0, .LBB100_8
.LBB100_3:
	beq r3, r1, .LBB100_6
.LBB100_4:
	addi r1, r0, -1
	bne r3, r1, .LBB100_7
.LBB100_5:
	andi r1, r4, 1
	addi r3, r0, 0
	seq r1, r1, r3
	addi r1, r1, -1
	ori  r1, r1, 1
	jal r0, .LBB100_8
.LBB100_6:
	add r1, r3, r0
	jal r0, .LBB100_8
.LBB100_7:
	add r1, r5, r0
.LBB100_8:
	jalr r0, r31, 0
.Lfunc_end100:
	.size	f77_ipow, .Lfunc_end100-f77_ipow
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_dpow_i
	.type	.LCPI101_0,@object
.LCPI101_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI101_0, 8
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
	lui r1, %hi(.LCPI101_0)
	addi r1, r1, %lo(.LCPI101_0)
	ldw r9, r1+4
	ldw r8, r1+0
	addi r1, r0, 0
	add r10, r8, r0
	add r11, r9, r0
	beq r5, r1, .LBB101_3
.LBB101_1:
	add r7, r4, r0
	add r6, r3, r0
	srai r3, r5, 31
	xor r4, r5, r3
	sub r3, r4, r3
	add r10, r8, r0
	add r11, r9, r0
.LBB101_2:
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
	bne r3, r1, .LBB101_2
.LBB101_3:
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
.Lfunc_end101:
	.size	f77_dpow_i, .Lfunc_end101-f77_dpow_i
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rpow_i
	.type	.LCPI102_0,@object
.LCPI102_0:
	.word	0x3f800000                      # float 1
	.size	.LCPI102_0, 4
	.text
	.globl	f77_rpow_i
	.p2align	2
	.type	f77_rpow_i,@function
f77_rpow_i:                             # @f77_rpow_i
# %bb.0:
	lui r8, %hi(.LCPI102_0)
	addi r8, r8, %lo(.LCPI102_0)
	ldw r1, r8+0
	addi r5, r0, 0
	add r6, r1, r0
	beq r4, r5, .LBB102_3
.LBB102_1:
	srai r6, r4, 31
	xor r7, r4, r6
	sub r7, r7, r6
	ldw r6, r8+0
.LBB102_2:
	andi r8, r7, 1
	seq r8, r8, r5
	fmul.s r9, r3, r6
	xor r6, r6, r9
	sub r8, r0, r8
	and r6, r6, r8
	xor r6, r9, r6
	fmul.s r3, r3, r3
	srli r7, r7, 1
	bne r7, r5, .LBB102_2
.LBB102_3:
	fdiv.s r1, r1, r6
	slt r3, r4, r5
	xor r1, r1, r6
	sub r3, r0, r3
	and r1, r1, r3
	xor r1, r6, r1
	jalr r0, r31, 0
.Lfunc_end102:
	.size	f77_rpow_i, .Lfunc_end102-f77_rpow_i
                                        # -- End function
	.globl	f77_open                        # -- Begin function f77_open
	.p2align	2
	.type	f77_open,@function
f77_open:                               # @f77_open
# %bb.0:
	addi sp, sp, -336
	stw sp+0, lr
	stw sp+332, r11
	stw sp+328, r12
	stw sp+324, r13
	stw sp+320, r14
	stw sp+316, r15
	stw sp+312, r16
	stw sp+308, r17
	stw sp+304, r18
	stw sp+300, r19
	stw sp+296, r20
	stw sp+292, r21
	stw sp+288, r22
	stw sp+284, r23
	stw sp+280, r24
	stw sp+276, r25
	stw sp+272, r26
	add r12, r7, r0
	add r13, r6, r0
	add r15, r5, r0
	add r14, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	addi r11, r0, 0
	bne r3, r11, .LBB103_4
.LBB103_1:
	addi r3, r0, -128
	lui r5, %hi(fio_uend+128)
	addi r5, r5, %lo(fio_uend+128)
	addi r6, r0, -1
.LBB103_2:
	add r7, r3, r5
	stw r7+0, r6
	addi r3, r3, 4
	bne r3, r11, .LBB103_2
.LBB103_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB103_4:
	ldw r18, sp+352
	addi r1, r14, -32
	addi r3, r0, -31
	bltu r1, r3, .LBB103_7
.LBB103_5:
	addi r1, r14, -5
	addi r3, r0, 1
	bleu r1, r3, .LBB103_7
.LBB103_6:
	add r17, r11, r0
	bne r8, r11, .LBB103_9
	jal r0, .LBB103_20
.LBB103_7:
	addi r1, r0, 0
	bne r18, r1, .LBB103_122
.LBB103_8:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r1, %hi(.L.str.21)
	addi r1, r1, %lo(.L.str.21)
	add r16, r4, r0
	add r4, r1, r0
	add r5, r14, r0
	add r17, r10, r0
	add r19, r8, r0
	add r20, r9, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r9, r20, r0
	add r8, r19, r0
	add r10, r17, r0
	add r4, r16, r0
	add r17, r11, r0
	beq r8, r11, .LBB103_20
.LBB103_9:
	addi r1, r0, 1
	add r17, r11, r0
	blt r9, r1, .LBB103_20
.LBB103_10:
	addi r17, r0, 0
	addi r3, r0, 6
	lui r5, %hi(.L.str.22)
	addi r5, r5, %lo(.L.str.22)
	addi r6, r0, 26
	add r7, r17, r0
.LBB103_11:
	beq r7, r3, .LBB103_15
.LBB103_12:
	add lr, r7, r5
	ldb lr, lr+0
	add r16, r8, r7
	ldb r16, r16+0
	addi r19, r16, -97
	andi r19, r19, 255
	sltu r19, r19, r6
	addi r20, r16, -32
	xor r20, r20, r16
	sub r19, r0, r19
	and r19, r20, r19
	xor r16, r16, r19
	bne r16, lr, .LBB103_20
.LBB103_13:
	addi r7, r7, 1
	bne r9, r7, .LBB103_11
.LBB103_14:
	add r5, r9, r0
	beq r5, r3, .LBB103_16
	jal r0, .LBB103_20
.LBB103_15:
	add r5, r3, r0
	bne r5, r3, .LBB103_20
.LBB103_16:
	addi r3, r0, 7
	add r17, r1, r0
	blt r9, r3, .LBB103_20
.LBB103_17:
	addi r6, r9, -7
	addi r1, r8, 6
	addi r3, r0, 32
	addi r5, r0, 1
.LBB103_18:
	ldbu r7, r1+0
	seq r17, r7, r3
	addi r8, r6, -1
	sltu r6, r8, r6
	bne r6, r5, .LBB103_20
.LBB103_19:
	addi r1, r1, 1
	add r6, r8, r0
	beq r7, r3, .LBB103_18
.LBB103_20:
	add r19, r17, r0
	beq r10, r11, .LBB103_33
.LBB103_21:
	ldw r3, sp+336
	addi r1, r0, 1
	add r19, r17, r0
	blt r3, r1, .LBB103_33
.LBB103_22:
	addi r19, r0, 0
	addi r5, r0, 11
	lui r6, %hi(.L.str.23)
	addi r6, r6, %lo(.L.str.23)
	addi r7, r0, 26
	add r8, r19, r0
.LBB103_23:
	beq r8, r5, .LBB103_27
.LBB103_24:
	add r9, r8, r6
	ldb r9, r9+0
	add lr, r10, r8
	ldb lr, lr+0
	addi r11, lr, -97
	andi r11, r11, 255
	sltu r11, r11, r7
	addi r16, lr, -32
	xor r16, r16, lr
	sub r11, r0, r11
	and r11, r16, r11
	xor lr, lr, r11
	bne lr, r9, .LBB103_33
.LBB103_25:
	addi r8, r8, 1
	bne r3, r8, .LBB103_23
.LBB103_26:
	add r6, r3, r0
	beq r6, r5, .LBB103_28
	jal r0, .LBB103_33
.LBB103_27:
	add r6, r5, r0
	bne r6, r5, .LBB103_33
.LBB103_28:
	addi r5, r0, 12
	blt r3, r5, .LBB103_32
.LBB103_29:
	addi r5, r10, 11
	addi r3, r3, -11
	addi r19, r0, 0
	addi r6, r0, 32
.LBB103_30:
	ldbu r7, r5+0
	bne r7, r6, .LBB103_33
.LBB103_31:
	addi r5, r5, 1
	addi r3, r3, -1
	bne r3, r19, .LBB103_30
.LBB103_32:
	add r19, r1, r0
.LBB103_33:
	ldw r20, sp+348
	addi r1, r0, 0
	sne r3, r13, r1
	sgt r5, r12, r1
	and r25, r3, r5
	addi r24, r0, 1
	add r26, r1, r0
	bne r25, r24, .LBB103_45
.LBB103_34:
	addi r26, r0, 0
	addi r3, r0, 7
	lui r5, %hi(.L.str.24)
	addi r5, r5, %lo(.L.str.24)
	addi r6, r0, 26
	add r7, r26, r0
.LBB103_35:
	beq r7, r3, .LBB103_39
.LBB103_36:
	add r8, r7, r5
	ldb r8, r8+0
	add r9, r13, r7
	ldb r9, r9+0
	addi r10, r9, -97
	andi r10, r10, 255
	sltu r10, r10, r6
	addi lr, r9, -32
	xor lr, lr, r9
	sub r10, r0, r10
	and r10, lr, r10
	xor r9, r9, r10
	bne r9, r8, .LBB103_45
.LBB103_37:
	addi r7, r7, 1
	bne r12, r7, .LBB103_35
.LBB103_38:
	add r5, r12, r0
	beq r5, r3, .LBB103_40
	jal r0, .LBB103_45
.LBB103_39:
	add r5, r3, r0
	bne r5, r3, .LBB103_45
.LBB103_40:
	addi r3, r0, 8
	blt r12, r3, .LBB103_44
.LBB103_41:
	addi r7, r12, -8
	addi r3, r13, 7
	addi r5, r0, 32
	addi r6, r0, 1
.LBB103_42:
	ldbu r8, r3+0
	seq r26, r8, r5
	addi r9, r7, -1
	sltu r7, r9, r7
	bne r7, r6, .LBB103_45
.LBB103_43:
	addi r3, r3, 1
	add r7, r9, r0
	beq r8, r5, .LBB103_42
	jal r0, .LBB103_45
.LBB103_44:
	addi r26, r0, 1
.LBB103_45:
	sgt r3, r20, r1
	xori r5, r17, 1
	or  r3, r3, r5
	bne r3, r1, .LBB103_48
.LBB103_46:
	addi r1, r0, 0
	bne r18, r1, .LBB103_122
.LBB103_47:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r1, %hi(.L.str.25)
	addi r1, r1, %lo(.L.str.25)
	add r11, r4, r0
	add r4, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r4, r11, r0
.LBB103_48:
	addi r11, r0, 0
	beq r4, r11, .LBB103_56
.LBB103_49:
	addi r21, r0, 1
	blt r15, r21, .LBB103_56
.LBB103_50:
	addi r1, r0, 256
	blt r15, r1, .LBB103_53
.LBB103_51:
	addi r1, r0, 0
	bne r18, r1, .LBB103_122
.LBB103_52:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r1, %hi(.L.str.26)
	addi r1, r1, %lo(.L.str.26)
	add r16, r4, r0
	add r4, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
	add r4, r16, r0
.LBB103_53:
	addi r16, sp, 16
	add r3, r16, r0
	add r5, r15, r0
	jal r31, memcpy
	add r3, r16, r15
	addi r1, r0, 0
	stb r3+0, r1
	addi r3, r16, -1
	addi r4, r0, 32
.LBB103_54:
	add r6, r3, r15
	ldbu r7, r6+0
	bne r7, r4, .LBB103_57
.LBB103_55:
	add r5, r15, r0
	addi r15, r15, -1
	stb r6+0, r1
	bgt r5, r21, .LBB103_54
	jal r0, .LBB103_57
.LBB103_56:
	lui r5, %hi(.L.str.27)
	addi r5, r5, %lo(.L.str.27)
	addi r3, sp, 16
	addi r4, r0, 256
	add r6, r14, r0
	jal r31, snprintf
.LBB103_57:
	ldw r16, sp+344
	ldw r21, sp+340
	slli r15, r14, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r22, r15, r1
	ldw r1, r22+0
	slli r14, r14, 8
	lui r23, %hi(fio_ufname)
	addi r23, r23, %lo(fio_ufname)
	beq r1, r11, .LBB103_60
.LBB103_58:
	add r3, r14, r23
	addi r4, sp, 16
	jal r31, strcmp
	sne r1, r1, r11
	or  r1, r26, r1
	beq r1, r11, .LBB103_69
.LBB103_59:
	ldw r3, r22+0
	jal r31, fclose
	addi r1, r0, 0
	stw r22+0, r1
.LBB103_60:
	addi r24, r0, 0
	beq r26, r24, .LBB103_63
.LBB103_61:
	jal r31, tmpfile
	addi r3, r0, 0
	beq r1, r3, .LBB103_76
.LBB103_62:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	jal r0, .LBB103_125
.LBB103_63:
	beq r25, r24, .LBB103_95
.LBB103_64:
	addi r3, r0, 0
	addi r1, r0, 3
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	addi r5, r0, 26
.LBB103_65:
	beq r3, r1, .LBB103_78
.LBB103_66:
	add r6, r3, r4
	ldb r6, r6+0
	add r7, r13, r3
	ldb r7, r7+0
	addi r8, r7, -97
	andi r8, r8, 255
	sltu r8, r8, r5
	addi r9, r7, -32
	xor r9, r9, r7
	sub r8, r0, r8
	and r8, r9, r8
	xor r7, r7, r8
	bne r7, r6, .LBB103_88
.LBB103_67:
	addi r3, r3, 1
	bne r12, r3, .LBB103_65
.LBB103_68:
	add r3, r12, r0
	beq r3, r1, .LBB103_79
	jal r0, .LBB103_88
.LBB103_69:
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r15, r1
	stw r1+0, r17
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r15, r1
	stw r1+0, r19
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r15, r1
	stw r1+0, r20
	add r1, r11, r0
	beq r21, r11, .LBB103_139
.LBB103_70:
	add r1, r11, r0
	blt r16, r24, .LBB103_139
.LBB103_71:
	addi r1, r0, 0
	addi r3, r0, 4
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	addi r5, r0, 26
	add r6, r1, r0
.LBB103_72:
	beq r6, r3, .LBB103_102
.LBB103_73:
	add r7, r6, r4
	ldb r7, r7+0
	add r8, r21, r6
	ldb r8, r8+0
	addi r9, r8, -97
	andi r9, r9, 255
	sltu r9, r9, r5
	addi r10, r8, -32
	xor r10, r10, r8
	sub r9, r0, r9
	and r9, r10, r9
	xor r8, r8, r9
	bne r8, r7, .LBB103_139
.LBB103_74:
	addi r6, r6, 1
	bne r16, r6, .LBB103_72
.LBB103_75:
	add r4, r16, r0
	jal r0, .LBB103_103
.LBB103_76:
	bne r18, r3, .LBB103_122
.LBB103_77:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	jal r31, fprintf
	lui r12, %hi(.L.str.10)
	addi r12, r12, %lo(.L.str.10)
	jal r0, .LBB103_124
.LBB103_78:
	add r3, r1, r0
	bne r3, r1, .LBB103_88
.LBB103_79:
	addi r1, r0, 4
	blt r12, r1, .LBB103_83
.LBB103_80:
	addi r1, r13, 3
	addi r3, r12, -3
	addi r4, r0, 32
	addi r5, r0, 0
.LBB103_81:
	ldbu r6, r1+0
	bne r6, r4, .LBB103_88
.LBB103_82:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB103_81
.LBB103_83:
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	addi r3, sp, 16
	jal r31, fopen
	addi r12, r0, 0
	bne r1, r12, .LBB103_85
.LBB103_84:
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
	addi r3, sp, 16
	jal r31, fopen
.LBB103_85:
	addi r4, sp, 16
	bne r1, r12, .LBB103_125
.LBB103_86:
	bne r18, r12, .LBB103_122
.LBB103_87:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	jal r0, .LBB103_120
.LBB103_88:
	lui r5, %hi(.L.str.34)
	addi r5, r5, %lo(.L.str.34)
	add r3, r13, r0
	add r4, r12, r0
	jal r31, fio_str_is
	addi r25, r0, 0
	beq r1, r25, .LBB103_95
.LBB103_89:
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
	addi r3, sp, 16
	jal r31, fopen
	beq r1, r25, .LBB103_92
.LBB103_90:
	add r3, r1, r0
	jal r31, fclose
	bne r18, r25, .LBB103_122
.LBB103_91:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	addi r5, sp, 16
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB103_92:
	lui r4, %hi(.L.str.36)
	addi r4, r4, %lo(.L.str.36)
	addi r12, sp, 16
	add r3, r12, r0
	jal r31, fopen
	add r4, r12, r0
	bne r1, r25, .LBB103_125
.LBB103_93:
	bne r18, r25, .LBB103_122
.LBB103_94:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	jal r0, .LBB103_120
.LBB103_95:
	beq r13, r24, .LBB103_115
.LBB103_96:
	addi r1, r0, 1
	blt r12, r1, .LBB103_115
.LBB103_97:
	addi r1, r0, 7
	lui r3, %hi(.L.str.38)
	addi r3, r3, %lo(.L.str.38)
	addi r4, r0, 26
.LBB103_98:
	beq r24, r1, .LBB103_109
.LBB103_99:
	add r5, r24, r3
	ldb r5, r5+0
	add r6, r13, r24
	ldb r6, r6+0
	addi r7, r6, -97
	andi r7, r7, 255
	sltu r7, r7, r4
	addi r8, r6, -32
	xor r8, r8, r6
	sub r7, r0, r7
	and r7, r8, r7
	xor r6, r6, r7
	bne r6, r5, .LBB103_121
.LBB103_100:
	addi r24, r24, 1
	bne r12, r24, .LBB103_98
.LBB103_101:
	add r3, r12, r0
	jal r0, .LBB103_110
.LBB103_102:
	add r4, r3, r0
.LBB103_103:
	bne r4, r3, .LBB103_139
.LBB103_104:
	addi r1, r0, 5
	blt r16, r1, .LBB103_108
.LBB103_105:
	addi r3, r21, 4
	addi r4, r16, -4
	addi r1, r0, 0
	addi r5, r0, 32
.LBB103_106:
	ldbu r6, r3+0
	bne r6, r5, .LBB103_139
.LBB103_107:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r1, .LBB103_106
.LBB103_108:
	add r1, r24, r0
	jal r0, .LBB103_139
.LBB103_109:
	add r3, r1, r0
.LBB103_110:
	bne r3, r1, .LBB103_121
.LBB103_111:
	addi r1, r0, 8
	blt r12, r1, .LBB103_115
.LBB103_112:
	addi r1, r13, 7
	addi r3, r12, -7
	addi r4, r0, 32
	addi r5, r0, 0
.LBB103_113:
	ldbu r6, r1+0
	bne r6, r4, .LBB103_121
.LBB103_114:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB103_113
.LBB103_115:
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	addi r3, sp, 16
	jal r31, fopen
	addi r12, r0, 0
	bne r1, r12, .LBB103_117
.LBB103_116:
	lui r4, %hi(.L.str.36)
	addi r4, r4, %lo(.L.str.36)
	addi r3, sp, 16
	jal r31, fopen
.LBB103_117:
	addi r4, sp, 16
	bne r1, r12, .LBB103_125
.LBB103_118:
	bne r18, r12, .LBB103_122
.LBB103_119:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.39)
	addi r4, r4, %lo(.L.str.39)
.LBB103_120:
	addi r5, sp, 16
	add r12, r5, r0
	jal r31, fprintf
	jal r0, .LBB103_124
.LBB103_121:
	addi r1, r0, 0
	beq r18, r1, .LBB103_123
.LBB103_122:
	addi r11, r0, 2
	jal r0, .LBB103_140
.LBB103_123:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.40)
	addi r4, r4, %lo(.L.str.40)
	jal r31, fprintf
	addi r12, sp, 16
.LBB103_124:
	addi r3, r0, 2
	jal r31, exit
	addi r1, r0, 0
	add r4, r12, r0
.LBB103_125:
	stw r22+0, r1
	add r3, r14, r23
	jal r31, strcpy
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r1, r15, r1
	addi r3, r0, -1
	stw r1+0, r3
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r15, r1
	stw r1+0, r17
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r15, r1
	stw r1+0, r19
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r15, r1
	stw r1+0, r20
	addi r1, r0, 0
	beq r21, r1, .LBB103_139
.LBB103_126:
	addi r3, r0, 1
	blt r16, r3, .LBB103_139
.LBB103_127:
	addi r1, r0, 0
	addi r4, r0, 4
	lui r5, %hi(.L.str.28)
	addi r5, r5, %lo(.L.str.28)
	addi r6, r0, 26
	add r7, r1, r0
.LBB103_128:
	beq r7, r4, .LBB103_132
.LBB103_129:
	add r8, r7, r5
	ldb r8, r8+0
	add r9, r21, r7
	ldb r9, r9+0
	addi r10, r9, -97
	andi r10, r10, 255
	sltu r10, r10, r6
	addi lr, r9, -32
	xor lr, lr, r9
	sub r10, r0, r10
	and r10, lr, r10
	xor r9, r9, r10
	bne r9, r8, .LBB103_139
.LBB103_130:
	addi r7, r7, 1
	bne r16, r7, .LBB103_128
.LBB103_131:
	add r5, r16, r0
	jal r0, .LBB103_133
.LBB103_132:
	add r5, r4, r0
.LBB103_133:
	bne r5, r4, .LBB103_139
.LBB103_134:
	addi r1, r0, 5
	blt r16, r1, .LBB103_138
.LBB103_135:
	addi r4, r21, 4
	addi r5, r16, -4
	addi r1, r0, 0
	addi r6, r0, 32
.LBB103_136:
	ldbu r7, r4+0
	bne r7, r6, .LBB103_139
.LBB103_137:
	addi r4, r4, 1
	addi r5, r5, -1
	bne r5, r1, .LBB103_136
.LBB103_138:
	add r1, r3, r0
.LBB103_139:
	lui r3, %hi(fio_ubz)
	addi r3, r3, %lo(fio_ubz)
	add r3, r15, r3
	stw r3+0, r1
.LBB103_140:
	add r1, r11, r0
	ldw r26, sp+272
	ldw r25, sp+276
	ldw r24, sp+280
	ldw r23, sp+284
	ldw r22, sp+288
	ldw r21, sp+292
	ldw r20, sp+296
	ldw r19, sp+300
	ldw r18, sp+304
	ldw r17, sp+308
	ldw r16, sp+312
	ldw r15, sp+316
	ldw r14, sp+320
	ldw r13, sp+324
	ldw r12, sp+328
	ldw r11, sp+332
	ldw lr, sp+0
	addi sp, sp, 336
	jalr r0, r31, 0
.Lfunc_end103:
	.size	f77_open, .Lfunc_end103-f77_open
                                        # -- End function
	.p2align	2                               # -- Begin function fio_str_is
	.type	fio_str_is,@function
fio_str_is:                             # @fio_str_is
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	addi r1, r0, 0
	addi r6, r0, 1
	add r7, r1, r0
	blt r4, r6, .LBB104_6
.LBB104_1:
	addi r8, r0, 0
	addi r9, r0, 26
	add r7, r8, r0
.LBB104_2:
	add r10, r5, r7
	ldb r10, r10+0
	beq r10, r8, .LBB104_6
.LBB104_3:
	add lr, r3, r7
	ldb lr, lr+0
	addi r11, lr, -97
	andi r11, r11, 255
	sltu r11, r11, r9
	addi r12, lr, -32
	xor r12, r12, lr
	sub r11, r0, r11
	and r11, r12, r11
	xor lr, lr, r11
	bne lr, r10, .LBB104_12
.LBB104_4:
	addi r7, r7, 1
	bne r4, r7, .LBB104_2
.LBB104_5:
	add r7, r4, r0
.LBB104_6:
	add r5, r5, r7
	ldbu r5, r5+0
	bne r5, r1, .LBB104_13
.LBB104_7:
	ble r4, r7, .LBB104_11
.LBB104_8:
	add r3, r3, r7
	sub r4, r4, r7
	addi r1, r0, 0
	addi r5, r0, 32
.LBB104_9:
	ldbu r7, r3+0
	bne r7, r5, .LBB104_13
.LBB104_10:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r1, .LBB104_9
.LBB104_11:
	add r1, r6, r0
	jal r0, .LBB104_13
.LBB104_12:
	add r1, r8, r0
.LBB104_13:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end104:
	.size	fio_str_is, .Lfunc_end104-fio_str_is
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
	bgtu r3, r1, .LBB105_32
.LBB105_1:
	slli r11, r3, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r12, r11, r1
	ldw r6, r12+0
	addi r1, r0, 0
	beq r6, r1, .LBB105_32
.LBB105_2:
	addi r6, r0, 1
	beq r4, r1, .LBB105_9
.LBB105_3:
	addi r7, r0, 1
	add r13, r1, r0
	blt r5, r7, .LBB105_16
.LBB105_4:
	addi r6, r0, 0
	addi r8, r0, 6
	lui r9, %hi(.L.str.41)
	addi r9, r9, %lo(.L.str.41)
	addi r10, r0, 26
	add lr, r6, r0
.LBB105_5:
	beq lr, r8, .LBB105_10
.LBB105_6:
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
	bne r14, r13, .LBB105_15
.LBB105_7:
	addi lr, lr, 1
	bne r5, lr, .LBB105_5
.LBB105_8:
	add r9, r5, r0
	add r13, r6, r0
	beq r9, r8, .LBB105_11
	jal r0, .LBB105_16
.LBB105_9:
	add r13, r1, r0
	add r14, r3, r0
	beq r6, r1, .LBB105_17
	jal r0, .LBB105_29
.LBB105_10:
	add r9, r8, r0
	add r13, r6, r0
	bne r9, r8, .LBB105_16
.LBB105_11:
	addi r8, r0, 7
	add r6, r7, r0
	add r13, r7, r0
	blt r5, r8, .LBB105_16
.LBB105_12:
	addi r10, r5, -7
	addi r7, r4, 6
	addi r8, r0, 32
	addi r9, r0, 1
.LBB105_13:
	ldbu lr, r7+0
	seq r6, lr, r8
	addi r13, r10, -1
	sltu r10, r13, r10
	bne r10, r9, .LBB105_33
.LBB105_14:
	addi r7, r7, 1
	add r10, r13, r0
	add r13, r6, r0
	beq lr, r8, .LBB105_13
	jal r0, .LBB105_16
.LBB105_15:
	add r13, r6, r0
.LBB105_16:
	add r14, r3, r0
	bne r6, r1, .LBB105_29
.LBB105_17:
	addi r1, r0, 1
	blt r5, r1, .LBB105_28
.LBB105_18:
	addi r3, r0, 0
	addi r1, r0, 4
	lui r6, %hi(.L.str.42)
	addi r6, r6, %lo(.L.str.42)
	addi r7, r0, 26
.LBB105_19:
	beq r3, r1, .LBB105_23
.LBB105_20:
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
	bne r9, r8, .LBB105_28
.LBB105_21:
	addi r3, r3, 1
	bne r5, r3, .LBB105_19
.LBB105_22:
	add r3, r5, r0
	beq r3, r1, .LBB105_24
	jal r0, .LBB105_28
.LBB105_23:
	add r3, r1, r0
	bne r3, r1, .LBB105_28
.LBB105_24:
	addi r1, r0, 5
	blt r5, r1, .LBB105_29
.LBB105_25:
	addi r1, r4, 4
	addi r3, r5, -4
	addi r4, r0, 32
	addi r5, r0, 0
.LBB105_26:
	ldbu r6, r1+0
	bne r6, r4, .LBB105_28
.LBB105_27:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB105_26
	jal r0, .LBB105_29
.LBB105_28:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.43)
	addi r4, r4, %lo(.L.str.43)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB105_29:
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
	beq r13, r15, .LBB105_31
.LBB105_30:
	add r3, r11, r12
	jal r31, remove
.LBB105_31:
	add r1, r11, r12
	stb r1+0, r15
.LBB105_32:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB105_33:
	add r13, r6, r0
	add r14, r3, r0
	beq r6, r1, .LBB105_17
	jal r0, .LBB105_29
.Lfunc_end105:
	.size	f77_close, .Lfunc_end105-f77_close
                                        # -- End function
	.globl	f77_inquire                     # -- Begin function f77_inquire
	.p2align	2
	.type	f77_inquire,@function
f77_inquire:                            # @f77_inquire
# %bb.0:
	addi sp, sp, -336
	stw sp+0, lr
	stw sp+332, r11
	stw sp+328, r12
	stw sp+324, r13
	stw sp+320, r14
	stw sp+316, r15
	stw sp+312, r16
	stw sp+308, r17
	stw sp+304, r18
	stw sp+300, r19
	stw sp+296, r20
	stw sp+292, r21
	stw sp+288, r22
	stw sp+284, r23
	stw sp+280, r24
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	addi r14, r0, 0
	bne r3, r14, .LBB106_4
.LBB106_1:
	addi r3, r0, -128
	lui r6, %hi(fio_uend+128)
	addi r6, r6, %lo(fio_uend+128)
	addi r7, r0, -1
.LBB106_2:
	add r8, r3, r6
	stw r8+0, r7
	addi r3, r3, 4
	bne r3, r14, .LBB106_2
.LBB106_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB106_4:
	lui r21, %hi(inq_unit)
	addi r21, r21, %lo(inq_unit)
	addi r1, r0, -1
	stw r21+0, r1
	lui r24, %hi(inq_exist)
	addi r24, r24, %lo(inq_exist)
	stb r24+0, r14
	lui r20, %hi(inq_opened)
	addi r20, r20, %lo(inq_opened)
	stb r20+0, r14
	lui r12, %hi(inq_name)
	addi r12, r12, %lo(inq_name)
	stb r12+0, r14
	lui r22, %hi(inq_named)
	addi r22, r22, %lo(inq_named)
	stw r22+0, r14
	lui r19, %hi(inq_direct)
	addi r19, r19, %lo(inq_direct)
	stw r19+0, r14
	lui r18, %hi(inq_unf)
	addi r18, r18, %lo(inq_unf)
	stw r18+0, r14
	lui r16, %hi(inq_recl)
	addi r16, r16, %lo(inq_recl)
	stw r16+0, r14
	lui r15, %hi(inq_nextrec)
	addi r15, r15, %lo(inq_nextrec)
	stw r15+0, r14
	lui r17, %hi(inq_bz)
	addi r17, r17, %lo(inq_bz)
	stw r17+0, r14
	beq r4, r14, .LBB106_21
.LBB106_5:
	addi r23, r0, 1
	blt r5, r23, .LBB106_21
.LBB106_6:
	addi r1, r0, 255
	slt r1, r5, r1
	sub r1, r0, r1
	xori r3, r5, 255
	and r1, r3, r1
	xori r12, r1, 255
	addi r13, sp, 24
	add r3, r13, r0
	add r5, r12, r0
	jal r31, memcpy
	add r1, r13, r12
	stb r1+0, r14
	addi r1, r12, -1
	addi r13, r0, 32
.LBB106_7:
	addi r3, sp, 24
	add r3, r3, r1
	ldbu r4, r3+0
	bne r4, r13, .LBB106_9
.LBB106_8:
	stb r3+0, r14
	addi r3, r1, -1
	addi r4, r1, 1
	add r1, r3, r0
	bgt r4, r23, .LBB106_7
.LBB106_9:
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	addi r12, sp, 24
	add r4, r12, r0
	jal r31, strcpy
	stw r22+0, r23
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
	add r3, r12, r0
	jal r31, fopen
	beq r1, r14, .LBB106_11
.LBB106_10:
	stb r24+0, r23
	add r3, r1, r0
	jal r31, fclose
.LBB106_11:
	lui r12, %hi(fio_ufname+256)
	addi r12, r12, %lo(fio_ufname+256)
	lui r22, %hi(fio_ufile+4)
	addi r22, r22, %lo(fio_ufile+4)
	jal r0, .LBB106_13
.LBB106_12:
	addi r23, r23, 1
	addi r12, r12, 256
	addi r22, r22, 4
	beq r23, r13, .LBB106_16
.LBB106_13:
	ldw r1, r22+0
	beq r1, r14, .LBB106_12
.LBB106_14:
	addi r4, sp, 24
	add r3, r12, r0
	jal r31, strcmp
	bne r1, r14, .LBB106_12
.LBB106_15:
	addi r1, r0, 1
	stb r20+0, r1
	stw r21+0, r23
.LBB106_16:
	ldbu r1, r20+0
	beq r1, r14, .LBB106_26
.LBB106_17:
	ldw r3, r21+0
	xor r3, r3, r11
	sub r1, r0, r1
	and r1, r3, r1
	xor r11, r11, r1
.LBB106_18:
	slli r1, r11, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r4, r3+0
	stw r19+0, r4
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r1, r3
	ldw r3, r3+0
	stw r18+0, r3
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r1, r3
	ldw r3, r3+0
	stw r16+0, r3
	lui r5, %hi(fio_ubz)
	addi r5, r5, %lo(fio_ubz)
	add r5, r1, r5
	ldw r5, r5+0
	stw r17+0, r5
	beq r4, r14, .LBB106_26
.LBB106_19:
	addi r4, r0, 1
	blt r3, r4, .LBB106_26
.LBB106_20:
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r11, r1, r3
	ldw r3, r11+0
	jal r31, fflush
	ldw r3, r11+0
	jal r31, ftell
	ldw r3, r16+0
	div r1, r1, r3
	addi r1, r1, 1
	stw r15+0, r1
	jal r0, .LBB106_26
.LBB106_21:
	addi r1, r0, 32
	bltu r11, r1, .LBB106_23
.LBB106_22:
	stb r24+0, r14
	jal r0, .LBB106_26
.LBB106_23:
	addi r1, r0, 1
	stb r24+0, r1
	stw r21+0, r11
	addi r3, r0, 6
	bgtu r11, r3, .LBB106_27
.LBB106_24:
	sll r4, r1, r11
	andi r4, r4, 97
	beq r4, r14, .LBB106_27
.LBB106_25:
	seq r3, r11, r3
	addi r4, r0, 5
	seq r4, r11, r4
	stb r20+0, r1
	lui r1, %hi(.L.str.46)
	addi r1, r1, %lo(.L.str.46)
	lui r5, %hi(.L.str.45)
	addi r5, r5, %lo(.L.str.45)
	xor r5, r5, r1
	sub r3, r0, r3
	and r3, r5, r3
	xor r1, r3, r1
	lui r3, %hi(.L.str.44)
	addi r3, r3, %lo(.L.str.44)
	xor r3, r1, r3
	sub r4, r0, r4
	and r3, r3, r4
	xor r4, r1, r3
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	jal r31, strcpy
	stw r22+0, r14
.LBB106_26:
	addi r1, r0, 0
	ldw r24, sp+280
	ldw r23, sp+284
	ldw r22, sp+288
	ldw r21, sp+292
	ldw r20, sp+296
	ldw r19, sp+300
	ldw r18, sp+304
	ldw r17, sp+308
	ldw r16, sp+312
	ldw r15, sp+316
	ldw r14, sp+320
	ldw r13, sp+324
	ldw r12, sp+328
	ldw r11, sp+332
	ldw lr, sp+0
	addi sp, sp, 336
	jalr r0, r31, 0
.LBB106_27:
	slli r3, r11, 2
	lui r4, %hi(fio_ufile)
	addi r4, r4, %lo(fio_ufile)
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r14, .LBB106_29
.LBB106_28:
	stb r20+0, r1
	slli r1, r11, 8
	lui r3, %hi(fio_ufname)
	addi r3, r3, %lo(fio_ufname)
	add r4, r1, r3
	add r3, r12, r0
	jal r31, strcpy
	ldbu r1, r12+0
	sne r1, r1, r14
	stw r22+0, r1
	jal r0, .LBB106_18
.LBB106_29:
	stb r20+0, r14
	jal r0, .LBB106_26
.Lfunc_end106:
	.size	f77_inquire, .Lfunc_end106-f77_inquire
                                        # -- End function
	.globl	f77_inq                         # -- Begin function f77_inq
	.p2align	2
	.type	f77_inq,@function
f77_inq:                                # @f77_inq
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	addi r1, r3, -1
	addi r3, r0, 13
	bgtu r1, r3, .LBB107_31
.LBB107_1:
	add r12, r5, r0
	add r11, r4, r0
	slli r1, r1, 2
	lui r3, %hi(.LJTI107_0)
	addi r3, r3, %lo(.LJTI107_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB107_2:
	lui r1, %hi(inq_exist)
	addi r1, r1, %lo(inq_exist)
	jal r0, .LBB107_29
.LBB107_3:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	addi r3, r0, 1
	bne r1, r3, .LBB107_31
.LBB107_4:
	lui r1, %hi(inq_direct)
	addi r1, r1, %lo(inq_direct)
	ldw r1, r1+0
	addi r3, r0, 0
	beq r1, r3, .LBB107_31
.LBB107_5:
	lui r1, %hi(inq_recl)
	addi r1, r1, %lo(inq_recl)
	ldw r1, r1+0
	jal r0, .LBB107_30
.LBB107_6:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	addi r15, r0, 1
	bne r1, r15, .LBB107_31
.LBB107_7:
	lui r1, %hi(inq_named)
	addi r1, r1, %lo(inq_named)
	ldw r1, r1+0
	addi r14, r0, 0
	beq r1, r14, .LBB107_31
.LBB107_8:
	lui r13, %hi(inq_name)
	addi r13, r13, %lo(inq_name)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r15, .LBB107_31
.LBB107_9:
	addi r3, r0, 32
	jal r0, .LBB107_11
.LBB107_10:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB107_31
.LBB107_11:
	add r4, r3, r0
	bge r14, r1, .LBB107_10
.LBB107_12:
	add r4, r14, r13
	ldbu r4, r4+0
	jal r0, .LBB107_10
.LBB107_13:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	addi r3, r0, 1
	bne r1, r3, .LBB107_31
.LBB107_14:
	lui r1, %hi(inq_direct)
	addi r1, r1, %lo(inq_direct)
	ldw r1, r1+0
	addi r3, r0, 0
	beq r1, r3, .LBB107_31
.LBB107_15:
	lui r1, %hi(inq_nextrec)
	addi r1, r1, %lo(inq_nextrec)
	ldw r1, r1+0
	jal r0, .LBB107_30
.LBB107_16:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_unf)
	addi r3, r3, %lo(inq_unf)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.49)
	addi r4, r4, %lo(.L.str.49)
	lui r5, %hi(.L.str.50)
	addi r5, r5, %lo(.L.str.50)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r3, r4
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r1, r4
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB107_31
.LBB107_17:
	addi r3, r0, 32
	jal r0, .LBB107_19
.LBB107_18:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB107_31
.LBB107_19:
	add r4, r3, r0
	bge r14, r1, .LBB107_18
.LBB107_20:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB107_18
.LBB107_21:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_unit)
	addi r3, r3, %lo(inq_unit)
	ldw r3, r3+0
	srai r4, r3, 31
	or  r4, r3, r4
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r1, r4, r1
	jal r0, .LBB107_30
.LBB107_22:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_named)
	addi r3, r3, %lo(inq_named)
	ldw r3, r3+0
	sub r1, r0, r1
	and r1, r3, r1
	jal r0, .LBB107_30
.LBB107_23:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_direct)
	addi r3, r3, %lo(inq_direct)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.50)
	addi r4, r4, %lo(.L.str.50)
	lui r5, %hi(.L.str.49)
	addi r5, r5, %lo(.L.str.49)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r3, r4
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r1, r4
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB107_31
.LBB107_24:
	addi r3, r0, 32
	jal r0, .LBB107_26
.LBB107_25:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB107_31
.LBB107_26:
	add r4, r3, r0
	bge r14, r1, .LBB107_25
.LBB107_27:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB107_25
.LBB107_28:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
.LBB107_29:
	ldbu r1, r1+0
.LBB107_30:
	stw r11+0, r1
.LBB107_31:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB107_32:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_direct)
	addi r3, r3, %lo(inq_direct)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	lui r5, %hi(.L.str.48)
	addi r5, r5, %lo(.L.str.48)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r3, r4
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r1, r4
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB107_31
.LBB107_33:
	addi r3, r0, 32
	jal r0, .LBB107_35
.LBB107_34:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB107_31
.LBB107_35:
	add r4, r3, r0
	bge r14, r1, .LBB107_34
.LBB107_36:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB107_34
.LBB107_37:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_unf)
	addi r3, r3, %lo(inq_unf)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(inq_bz)
	addi r4, r4, %lo(inq_bz)
	ldw r4, r4+0
	seq r4, r4, r14
	lui r5, %hi(.L.str.28)
	addi r5, r5, %lo(.L.str.28)
	lui r6, %hi(.L.str.52)
	addi r6, r6, %lo(.L.str.52)
	xor r6, r6, r5
	sub r4, r0, r4
	and r4, r6, r4
	xor r4, r4, r5
	lui r5, %hi(.L.str.47)
	addi r5, r5, %lo(.L.str.47)
	xor r4, r4, r5
	sub r3, r0, r3
	and r3, r4, r3
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r1, r5
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB107_31
.LBB107_38:
	addi r3, r0, 32
	jal r0, .LBB107_40
.LBB107_39:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB107_31
.LBB107_40:
	add r4, r3, r0
	bge r14, r1, .LBB107_39
.LBB107_41:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB107_39
.LBB107_42:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_direct)
	addi r3, r3, %lo(inq_direct)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.49)
	addi r4, r4, %lo(.L.str.49)
	lui r5, %hi(.L.str.50)
	addi r5, r5, %lo(.L.str.50)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r3, r4
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r1, r4
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB107_31
.LBB107_43:
	addi r3, r0, 32
	jal r0, .LBB107_45
.LBB107_44:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB107_31
.LBB107_45:
	add r4, r3, r0
	bge r14, r1, .LBB107_44
.LBB107_46:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB107_44
.LBB107_47:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_unf)
	addi r3, r3, %lo(inq_unf)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.50)
	addi r4, r4, %lo(.L.str.50)
	lui r5, %hi(.L.str.49)
	addi r5, r5, %lo(.L.str.49)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r3, r4
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r1, r4
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB107_31
.LBB107_48:
	addi r3, r0, 32
	jal r0, .LBB107_50
.LBB107_49:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB107_31
.LBB107_50:
	add r4, r3, r0
	bge r14, r1, .LBB107_49
.LBB107_51:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB107_49
.LBB107_52:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_unf)
	addi r3, r3, %lo(inq_unf)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	lui r5, %hi(.L.str.51)
	addi r5, r5, %lo(.L.str.51)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r3, r4
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r1, r4
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB107_31
.LBB107_53:
	addi r3, r0, 32
	jal r0, .LBB107_55
.LBB107_54:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB107_31
.LBB107_55:
	add r4, r3, r0
	bge r14, r1, .LBB107_54
.LBB107_56:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB107_54
.Lfunc_end107:
	.size	f77_inq, .Lfunc_end107-f77_inq
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI107_0,@object
.LJTI107_0:
	.word	.LBB107_2
	.word	.LBB107_28
	.word	.LBB107_21
	.word	.LBB107_22
	.word	.LBB107_6
	.word	.LBB107_32
	.word	.LBB107_42
	.word	.LBB107_23
	.word	.LBB107_52
	.word	.LBB107_16
	.word	.LBB107_47
	.word	.LBB107_3
	.word	.LBB107_13
	.word	.LBB107_37
	.size	.LJTI107_0, 56
                                        # -- End function
	.text
	.globl	f77_rewind                      # -- Begin function f77_rewind
	.p2align	2
	.type	f77_rewind,@function
f77_rewind:                             # @f77_rewind
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	addi r12, r0, 0
	bne r3, r12, .LBB108_4
.LBB108_1:
	addi r3, r0, -128
	lui r4, %hi(fio_uend+128)
	addi r4, r4, %lo(fio_uend+128)
	addi r5, r0, -1
.LBB108_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r12, .LBB108_2
.LBB108_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB108_4:
	addi r1, r11, -32
	addi r3, r0, -31
	bltu r1, r3, .LBB108_6
.LBB108_5:
	addi r1, r11, -5
	addi r3, r0, 1
	bgtu r1, r3, .LBB108_7
.LBB108_6:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB108_7:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, fio_resolve
	add r13, r1, r0
	add r3, r1, r0
	jal r31, fflush
	add r3, r13, r0
	add r4, r12, r0
	add r5, r12, r0
	jal r31, fseek
	slli r1, r11, 2
	lui r3, %hi(fio_uaeof)
	addi r3, r3, %lo(fio_uaeof)
	add r1, r1, r3
	stw r1+0, r12
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end108:
	.size	f77_rewind, .Lfunc_end108-f77_rewind
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
	bleu r5, r6, .LBB109_2
.LBB109_1:
	addi r1, r0, 0
	jal r0, .LBB109_5
.LBB109_2:
	addi r6, r1, 1
	addi r1, r0, 0
	addi r7, r0, 10
.LBB109_3:
	mul r1, r1, r7
	addi r8, r4, 1
	add r1, r5, r1
	add r4, r6, r4
	ldb r4, r4+0
	addi r5, r4, -48
	add r4, r8, r0
	bltu r5, r7, .LBB109_3
.LBB109_4:
	stw r3+0, r8
.LBB109_5:
	jalr r0, r31, 0
.Lfunc_end109:
	.size	fio_number, .Lfunc_end109-fio_number
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
	stw sp+16, r14
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	ldw r1, r1+0
	addi r11, r0, 0
	beq r1, r11, .LBB110_3
.LBB110_1:
	lui r12, %hi(fio_irec)
	addi r12, r12, %lo(fio_irec)
	ldw r3, r12+0
	lui r4, %hi(fio_inrec)
	addi r4, r4, %lo(fio_inrec)
	ldw r4, r4+0
	bge r3, r4, .LBB110_21
.LBB110_2:
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
	jal r0, .LBB110_25
.LBB110_3:
	lui r12, %hi(fio_unit)
	addi r12, r12, %lo(fio_unit)
	ldw r1, r12+0
	addi r3, r1, -1
	addi r13, r0, 30
	lui r11, %hi(fio_in)
	addi r11, r11, %lo(fio_in)
	bgtu r3, r13, .LBB110_7
.LBB110_4:
	slli r1, r1, 2
	lui r14, %hi(fio_uend)
	addi r14, r14, %lo(fio_uend)
	add r1, r1, r14
	ldw r3, r1+0
	addi r1, r0, 0
	blt r3, r1, .LBB110_7
.LBB110_5:
	ldw r3, r11+0
	beq r3, r1, .LBB110_7
.LBB110_6:
	jal r31, ftell
	ldw r3, r12+0
	slli r3, r3, 2
	add r4, r3, r14
	ldw r4, r4+0
	bge r1, r4, .LBB110_22
.LBB110_7:
	ldw r1, r12+0
	addi r3, r1, -1
	bgtu r3, r13, .LBB110_11
.LBB110_8:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	addi r14, r0, 0
	beq r3, r14, .LBB110_11
.LBB110_9:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r1, r1+0
	xori r3, r1, 1023
	addi r4, r0, 1023
	slt r1, r1, r4
	sub r1, r0, r1
	and r1, r3, r1
	xori r5, r1, 1023
	ldw r6, r11+0
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	addi r11, r0, 1
	add r4, r11, r0
	jal r31, fread
	beq r1, r14, .LBB110_25
.LBB110_10:
	lui r3, %hi(fio_rlen)
	addi r3, r3, %lo(fio_rlen)
	stw r3+0, r1
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	stw r1+0, r14
	add r11, r14, r0
	jal r0, .LBB110_25
.LBB110_11:
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
	beq r1, r11, .LBB110_19
.LBB110_12:
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	jal r31, strlen
	addi r3, r0, 1
	blt r1, r3, .LBB110_18
.LBB110_13:
	addi r4, r1, 1
	lui r5, %hi(fio_rec-2)
	addi r5, r5, %lo(fio_rec-2)
	addi r6, r0, 13
	addi r1, r0, 0
	addi r7, r0, 10
	jal r0, .LBB110_15
.LBB110_14:
	addi r4, r4, -1
	ble r4, r3, .LBB110_18
.LBB110_15:
	add r8, r4, r5
	ldbu r8, r8+0
	beq r8, r6, .LBB110_14
.LBB110_16:
	beq r8, r7, .LBB110_14
.LBB110_17:
	addi r1, r4, -1
.LBB110_18:
	lui r3, %hi(fio_rlen)
	addi r3, r3, %lo(fio_rlen)
	stw r3+0, r1
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	jal r0, .LBB110_24
.LBB110_19:
	ldw r1, r12+0
	addi r3, r1, -1
	bgtu r3, r13, .LBB110_21
.LBB110_20:
	slli r1, r1, 2
	lui r3, %hi(fio_uaeof)
	addi r3, r3, %lo(fio_uaeof)
	add r1, r1, r3
	jal r0, .LBB110_23
.LBB110_21:
	addi r11, r0, 1
	jal r0, .LBB110_25
.LBB110_22:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r3, r1
.LBB110_23:
	addi r11, r0, 1
.LBB110_24:
	stw r1+0, r11
.LBB110_25:
	add r1, r11, r0
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end110:
	.size	fio_getrec, .Lfunc_end110-fio_getrec
                                        # -- End function
	.type	fio_rec_next,@object            # @fio_rec_next
	.local	fio_rec_next
	.comm	fio_rec_next,4,4
	.type	fio_ibuf,@object                # @fio_ibuf
	.local	fio_ibuf
	.comm	fio_ibuf,4,4
	.type	fio_out,@object                 # @fio_out
	.local	fio_out
	.comm	fio_out,4,4
	.type	fio_unit,@object                # @fio_unit
	.local	fio_unit
	.comm	fio_unit,4,4
	.type	fio_uunf,@object                # @fio_uunf
	.local	fio_uunf
	.comm	fio_uunf,128,4
	.type	fio_ulen,@object                # @fio_ulen
	.local	fio_ulen
	.comm	fio_ulen,4,4
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"f77: unformatted record too long\n"
	.size	.L.str, 34

	.type	fio_urec,@object                # @fio_urec
	.local	fio_urec
	.comm	fio_urec,65536,1
	.type	fio_udirect,@object             # @fio_udirect
	.local	fio_udirect
	.comm	fio_udirect,128,4
	.type	fio_urecl,@object               # @fio_urecl
	.local	fio_urecl
	.comm	fio_urecl,128,4
	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"f77: unformatted record longer than RECL\n"
	.size	.L.str.1, 42

	.type	fio_uend,@object                # @fio_uend
	.local	fio_uend
	.comm	fio_uend,128,4
	.type	fio_in,@object                  # @fio_in
	.local	fio_in
	.comm	fio_in,4,4
	.type	fio_upos,@object                # @fio_upos
	.local	fio_upos
	.comm	fio_upos,4,4
	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"f77: READ past the end of a direct-access file\n"
	.size	.L.str.2, 48

	.type	fio_uaeof,@object               # @fio_uaeof
	.local	fio_uaeof
	.comm	fio_uaeof,128,4
	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"f77: end of file on READ\n"
	.size	.L.str.3, 26

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"f77: bad unformatted record\n"
	.size	.L.str.4, 29

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"f77: unformatted READ past the end of the record\n"
	.size	.L.str.5, 50

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
	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"%12d"
	.size	.L.str.6, 5

	.type	fio_first_item,@object          # @fio_first_item
	.local	fio_first_item
	.comm	fio_first_item,1,4
	.type	fio_prev_char,@object           # @fio_prev_char
	.local	fio_prev_char
	.comm	fio_prev_char,1,4
	.type	fio_w,@object                   # @fio_w
	.local	fio_w
	.comm	fio_w,4,4
	.type	fio_d,@object                   # @fio_d
	.local	fio_d
	.comm	fio_d,4,4
	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"%u"
	.size	.L.str.7, 3

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"%s%0*d%s"
	.size	.L.str.8, 9

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"-"
	.size	.L.str.9, 2

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.zero	1
	.size	.L.str.10, 1

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"%s%s"
	.size	.L.str.11, 5

	.type	fio_fmt,@object                 # @fio_fmt
	.local	fio_fmt
	.comm	fio_fmt,4,4
	.type	fio_pos,@object                 # @fio_pos
	.local	fio_pos
	.comm	fio_pos,4,4
	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"%d"
	.size	.L.str.12, 3

	.type	fio_scale,@object               # @fio_scale
	.local	fio_scale
	.comm	fio_scale,4,4
	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"%%.%df"
	.size	.L.str.13, 7

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"."
	.size	.L.str.14, 2

	.type	fio_e,@object                   # @fio_e
	.local	fio_e
	.comm	fio_e,4,4
	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"%g"
	.size	.L.str.15, 3

	.type	fio_len,@object                 # @fio_len
	.local	fio_len
	.comm	fio_len,4,4
	.type	fio_line,@object                # @fio_line
	.local	fio_line
	.comm	fio_line,1024,1
	.type	fio_col,@object                 # @fio_col
	.local	fio_col
	.comm	fio_col,4,4
	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"f77: concatenation result of %d bytes is too long\n"
	.size	.L.str.16, 51

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
	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"PAUSE %.*s\n"
	.size	.L.str.17, 12

	.type	fio_ubz,@object                 # @fio_ubz
	.local	fio_ubz
	.comm	fio_ubz,128,4
	.type	fio_bz,@object                  # @fio_bz
	.local	fio_bz
	.comm	fio_bz,4,4
	.type	fio_ldone,@object               # @fio_ldone
	.local	fio_ldone
	.comm	fio_ldone,1,4
	.type	fio_ltok,@object                # @fio_ltok
	.local	fio_ltok
	.comm	fio_ltok,128,4
	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"f77: A editing on READ needs CHARACTER (not implemented)\n"
	.size	.L.str.18, 58

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"f77: %c editing does not match a numeric READ item\n"
	.size	.L.str.19, 52

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"f77: %c editing does not match a CHARACTER READ item\n"
	.size	.L.str.20, 54

	.type	fio_rpos,@object                # @fio_rpos
	.local	fio_rpos
	.comm	fio_rpos,4,4
	.type	fio_rlen,@object                # @fio_rlen
	.local	fio_rlen
	.comm	fio_rlen,4,4
	.type	fio_rec,@object                 # @fio_rec
	.local	fio_rec
	.comm	fio_rec,1024,1
	.type	fio_l_after,@object             # @fio_l_after
	.local	fio_l_after
	.comm	fio_l_after,1,4
	.type	fio_reading,@object             # @fio_reading
	.local	fio_reading
	.comm	fio_reading,1,4
	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"f77: cannot OPEN unit %d\n"
	.size	.L.str.21, 26

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"DIRECT"
	.size	.L.str.22, 7

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"UNFORMATTED"
	.size	.L.str.23, 12

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"SCRATCH"
	.size	.L.str.24, 8

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"f77: OPEN ACCESS='DIRECT' needs RECL=\n"
	.size	.L.str.25, 39

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"f77: bad FILE= name in OPEN\n"
	.size	.L.str.26, 29

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"fort.%d"
	.size	.L.str.27, 8

	.type	fio_ufile,@object               # @fio_ufile
	.local	fio_ufile
	.comm	fio_ufile,128,4
	.type	fio_ufname,@object              # @fio_ufname
	.local	fio_ufname
	.comm	fio_ufname,8192,1
	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"ZERO"
	.size	.L.str.28, 5

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"f77: OPEN cannot create a scratch file\n"
	.size	.L.str.29, 40

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"OLD"
	.size	.L.str.30, 4

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"r+"
	.size	.L.str.31, 3

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"r"
	.size	.L.str.32, 2

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"f77: OPEN STATUS='OLD': no such file '%s'\n"
	.size	.L.str.33, 43

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"NEW"
	.size	.L.str.34, 4

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"f77: OPEN STATUS='NEW': '%s' already exists\n"
	.size	.L.str.35, 45

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"w+"
	.size	.L.str.36, 3

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"f77: OPEN cannot create '%s'\n"
	.size	.L.str.37, 30

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"UNKNOWN"
	.size	.L.str.38, 8

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"f77: OPEN cannot open '%s'\n"
	.size	.L.str.39, 28

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"f77: OPEN STATUS value is not supported\n"
	.size	.L.str.40, 41

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"DELETE"
	.size	.L.str.41, 7

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"KEEP"
	.size	.L.str.42, 5

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"f77: CLOSE STATUS value is not supported\n"
	.size	.L.str.43, 42

	.type	inq_unit,@object                # @inq_unit
	.local	inq_unit
	.comm	inq_unit,4,4
	.type	inq_exist,@object               # @inq_exist
	.local	inq_exist
	.comm	inq_exist,1,4
	.type	inq_opened,@object              # @inq_opened
	.local	inq_opened
	.comm	inq_opened,1,4
	.type	inq_name,@object                # @inq_name
	.local	inq_name
	.comm	inq_name,256,4
	.type	inq_named,@object               # @inq_named
	.local	inq_named
	.comm	inq_named,4,4
	.type	inq_direct,@object              # @inq_direct
	.local	inq_direct
	.comm	inq_direct,4,4
	.type	inq_unf,@object                 # @inq_unf
	.local	inq_unf
	.comm	inq_unf,4,4
	.type	inq_recl,@object                # @inq_recl
	.local	inq_recl
	.comm	inq_recl,4,4
	.type	inq_nextrec,@object             # @inq_nextrec
	.local	inq_nextrec
	.comm	inq_nextrec,4,4
	.type	inq_bz,@object                  # @inq_bz
	.local	inq_bz
	.comm	inq_bz,4,4
	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"stdin"
	.size	.L.str.44, 6

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"stdout"
	.size	.L.str.45, 7

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"stderr"
	.size	.L.str.46, 7

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"UNDEFINED"
	.size	.L.str.47, 10

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"SEQUENTIAL"
	.size	.L.str.48, 11

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"NO"
	.size	.L.str.49, 3

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"YES"
	.size	.L.str.50, 4

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"FORMATTED"
	.size	.L.str.51, 10

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"NULL"
	.size	.L.str.52, 5

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"f77: REWIND: unit %d cannot be rewound\n"
	.size	.L.str.53, 40

	.type	fio_uend_init,@object           # @fio_uend_init
	.local	fio_uend_init
	.comm	fio_uend_init,1,4
	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"f77: unit %d is not open for %s\n"
	.size	.L.str.54, 33

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"WRITE"
	.size	.L.str.55, 6

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"READ"
	.size	.L.str.56, 5

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"f77: a direct-access unit needs REC=\n"
	.size	.L.str.57, 38

	.type	fio_sp,@object                  # @fio_sp
	.local	fio_sp
	.comm	fio_sp,1,4
	.type	fio_gdepth,@object              # @fio_gdepth
	.local	fio_gdepth
	.comm	fio_gdepth,4,4
	.type	fio_revert,@object              # @fio_revert
	.local	fio_revert
	.comm	fio_revert,4,4
	.type	.L.str.58,@object               # @.str.58
.L.str.58:
	.asciz	"%%.%dE"
	.size	.L.str.58, 7

	.type	fio_gcount,@object              # @fio_gcount
	.local	fio_gcount
	.comm	fio_gcount,64,4
	.type	fio_gstart,@object              # @fio_gstart
	.local	fio_gstart
	.comm	fio_gstart,64,4
	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"f77: WRITE past the end of an internal file\n"
	.size	.L.str.59, 45

	.type	.L.str.60,@object               # @.str.60
.L.str.60:
	.asciz	"f77: record longer than the internal file (%d > %d)\n"
	.size	.L.str.60, 53

	.type	.L.str.61,@object               # @.str.61
.L.str.61:
	.asciz	"f77: record longer than RECL (%d > %d)\n"
	.size	.L.str.61, 40

	.type	fio_l_nulls,@object             # @fio_l_nulls
	.local	fio_l_nulls
	.comm	fio_l_nulls,4,4
	.type	fio_l_pending,@object           # @fio_l_pending
	.local	fio_l_pending
	.comm	fio_l_pending,4,4
	.type	.L.str.62,@object               # @.str.62
.L.str.62:
	.asciz	"f77: zero repeat count in list-directed input\n"
	.size	.L.str.62, 47

	.type	.L.str.63,@object               # @.str.63
.L.str.63:
	.asciz	"f77: bad LOGICAL input field '%s'\n"
	.size	.L.str.63, 35

	.type	.L.str.64,@object               # @.str.64
.L.str.64:
	.asciz	"E%d"
	.size	.L.str.64, 4

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e2b547612936be39d9f8cbfc73bb496c3ce978d8)"
	.section	".note.GNU-stack","",@progbits
