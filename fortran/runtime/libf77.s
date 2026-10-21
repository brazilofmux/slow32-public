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
	add r11, r4, r0
	add r13, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	addi r14, r0, 0
	bne r3, r14, .LBB1_4
.LBB1_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
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
	addi r4, r13, -1
	addi r5, r0, 98
	slli r3, r13, 2
	bgtu r4, r5, .LBB1_6
.LBB1_5:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r4, r3, r4
	addi r5, r0, 0
	stw r4+0, r5
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r4, r3, r4
	stw r4+0, r5
.LBB1_6:
	lui r5, %hi(fio_rec_next)
	addi r5, r5, %lo(fio_rec_next)
	ldw r4, r5+0
	stw r5+0, r14
	addi r5, r13, -100
	addi r6, r0, -99
	bltu r5, r6, .LBB1_10
.LBB1_7:
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r3, r5
	ldw r5, r5+0
	beq r5, r14, .LBB1_10
.LBB1_8:
	ble r4, r14, .LBB1_20
.LBB1_9:
	addi r4, r4, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	addi r5, r0, 0
	add r3, r1, r0
	jal r31, fseek
.LBB1_10:
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
	lui r13, %hi(fio_pos)
	addi r13, r13, %lo(fio_pos)
	lui r15, %hi(fio_revert)
	addi r15, r15, %lo(fio_revert)
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	beq r11, r14, .LBB1_17
.LBB1_11:
	stw r1+0, r14
	add r3, r11, r0
	jal r31, fio_fmt_use
	addi r3, r0, -1
	ble r1, r3, .LBB1_21
.LBB1_12:
	stw r13+0, r14
	stw r15+0, r3
	lui r1, %hi(fio_since_data)
	addi r1, r1, %lo(fio_since_data)
	stb r1+0, r14
	lui r1, %hi(fio_fmt)
	addi r1, r1, %lo(fio_fmt)
	ldw r1, r1+0
	addi r3, r0, 32
.LBB1_13:
	add r4, r1, r14
	ldbu r4, r4+0
	bne r4, r3, .LBB1_15
.LBB1_14:
	addi r14, r14, 1
	stw r13+0, r14
	jal r0, .LBB1_13
.LBB1_15:
	addi r1, r0, 40
	bne r4, r1, .LBB1_19
.LBB1_16:
	addi r1, r14, 1
	add r3, r1, r0
	jal r0, .LBB1_18
.LBB1_17:
	stw r1+0, r12
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
	lui r3, %hi(fio_fmt)
	addi r3, r3, %lo(fio_fmt)
	stw r3+0, r1
	addi r3, r0, -1
	addi r1, r0, 0
.LBB1_18:
	stw r13+0, r1
	stw r15+0, r3
.LBB1_19:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB1_20:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	jal r31, fprintf
.LBB1_21:
	addi r3, r0, 2
	jal r31, exit
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
	add r11, r3, r0
	addi r1, r0, 99
	bgtu r3, r1, .LBB2_2
.LBB2_1:
	slli r1, r11, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r1, r1, r3
	ldw r1, r1+0
	addi r3, r0, 0
	bne r1, r3, .LBB2_17
.LBB2_2:
	addi r13, r0, 0
	beq r4, r13, .LBB2_7
.LBB2_3:
	beq r11, r13, .LBB2_15
.LBB2_4:
	addi r1, r0, 5
	beq r11, r1, .LBB2_6
.LBB2_5:
	addi r1, r0, 6
	bne r11, r1, .LBB2_10
.LBB2_6:
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	jal r0, .LBB2_16
.LBB2_7:
	addi r1, r0, 5
	beq r11, r1, .LBB2_9
.LBB2_8:
	bne r11, r13, .LBB2_10
.LBB2_9:
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	jal r0, .LBB2_16
.LBB2_10:
	add r14, r4, r0
	addi r1, r11, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB2_18
.LBB2_11:
	lui r5, %hi(.L.str.36)
	addi r5, r5, %lo(.L.str.36)
	addi r12, sp, 16
	addi r4, r0, 256
	add r3, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	lui r4, %hi(.L.str.40)
	addi r4, r4, %lo(.L.str.40)
	add r3, r12, r0
	jal r31, fopen
	bne r1, r13, .LBB2_13
.LBB2_12:
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	addi r3, sp, 16
	jal r31, fopen
.LBB2_13:
	beq r1, r13, .LBB2_18
.LBB2_14:
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
	add r1, r11, r0
	lui r3, %hi(fio_uend)
	addi r3, r3, %lo(fio_uend)
	add r3, r12, r3
	addi r4, r0, -1
	stw r3+0, r4
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r12, r3
	stw r3+0, r13
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r12, r3
	stw r3+0, r13
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r12, r3
	stw r3+0, r13
	lui r3, %hi(fio_ubz)
	addi r3, r3, %lo(fio_ubz)
	add r3, r12, r3
	stw r3+0, r13
	jal r0, .LBB2_17
.LBB2_15:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
.LBB2_16:
	ldw r1, r1+0
.LBB2_17:
	ldw r14, sp+272
	ldw r13, sp+276
	ldw r12, sp+280
	ldw r11, sp+284
	ldw lr, sp+0
	addi sp, sp, 288
	jalr r0, r31, 0
.LBB2_18:
	seq r1, r14, r13
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.64)
	addi r4, r4, %lo(.L.str.64)
	lui r5, %hi(.L.str.65)
	addi r5, r5, %lo(.L.str.65)
	xor r5, r5, r4
	sub r1, r0, r1
	and r1, r5, r1
	xor r6, r1, r4
	lui r4, %hi(.L.str.63)
	addi r4, r4, %lo(.L.str.63)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
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
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	addi r13, r0, 0
	bne r3, r13, .LBB3_4
.LBB3_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
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
	addi r4, r11, -1
	addi r5, r0, 98
	slli r3, r11, 2
	bgtu r4, r5, .LBB3_6
.LBB3_5:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r4, r3, r4
	stw r4+0, r12
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r4, r3, r4
	stw r4+0, r13
.LBB3_6:
	lui r5, %hi(fio_rec_next)
	addi r5, r5, %lo(fio_rec_next)
	ldw r4, r5+0
	stw r5+0, r13
	addi r5, r11, -100
	addi r6, r0, -99
	bltu r5, r6, .LBB3_10
.LBB3_7:
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r3, r5
	ldw r5, r5+0
	beq r5, r13, .LBB3_10
.LBB3_8:
	ble r4, r13, .LBB3_11
.LBB3_9:
	addi r4, r4, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	addi r5, r0, 0
	add r3, r1, r0
	jal r31, fseek
.LBB3_10:
	lui r1, %hi(fio_ulen)
	addi r1, r1, %lo(fio_ulen)
	stw r1+0, r13
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB3_11:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
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
	add r11, r4, r0
	addi r1, r0, 1
	blt r4, r1, .LBB4_3
.LBB4_1:
	add r4, r3, r0
	lui r12, %hi(fio_ulen)
	addi r12, r12, %lo(fio_ulen)
	ldw r1, r12+0
	add r3, r1, r11
	lui r5, 16
	addi r5, r5, 1
	bge r3, r5, .LBB4_4
.LBB4_2:
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	add r3, r1, r3
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r12+0
	add r1, r1, r11
	stw r12+0, r1
.LBB4_3:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB4_4:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
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
	ldw r3, r1+0
	addi r1, r0, 0
	seq r4, r3, r1
	lui r5, %hi(stdout)
	addi r5, r5, %lo(stdout)
	ldw r5, r5+0
	xor r5, r5, r3
	sub r4, r0, r4
	and r4, r5, r4
	xor r11, r3, r4
	lui r16, %hi(fio_unit)
	addi r16, r16, %lo(fio_unit)
	ldw r3, r16+0
	addi r4, r3, -1
	addi r17, r0, 98
	bgtu r4, r17, .LBB5_6
.LBB5_1:
	slli r3, r3, 2
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r3, r4
	ldw r4, r4+0
	beq r4, r1, .LBB5_6
.LBB5_2:
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r3, r1
	ldw r13, r1+0
	lui r14, %hi(fio_ulen)
	addi r14, r14, %lo(fio_ulen)
	ldw r5, r14+0
	bgt r5, r13, .LBB5_9
.LBB5_3:
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	ldw r1, r14+0
	bge r1, r13, .LBB5_8
.LBB5_4:
	addi r12, r0, 0
.LBB5_5:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fputc
	ldw r1, r14+0
	addi r1, r1, 1
	stw r14+0, r1
	blt r1, r13, .LBB5_5
	jal r0, .LBB5_8
.LBB5_6:
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
	ldw r1, r16+0
	addi r1, r1, -1
	bgtu r1, r17, .LBB5_8
.LBB5_7:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r16+0
	slli r3, r3, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
.LBB5_8:
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
.LBB5_9:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
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
	add r11, r4, r0
	add r14, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r4, r1+0
	addi r3, r0, 0
	bne r4, r3, .LBB6_4
.LBB6_1:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
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
	addi r12, r0, 0
	add r3, r14, r0
	add r4, r12, r0
	jal r31, fio_resolve
	add r13, r1, r0
	lui r1, %hi(fio_in)
	addi r1, r1, %lo(fio_in)
	stw r1+0, r13
	lui r1, %hi(fio_unit)
	addi r1, r1, %lo(fio_unit)
	stw r1+0, r14
	addi r1, r14, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB6_13
.LBB6_5:
	slli r16, r14, 2
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r16, r1
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(fio_rec_next)
	addi r3, r3, %lo(fio_rec_next)
	ldw r1, r3+0
	addi r15, r0, 0
	stw r3+0, r15
	addi r3, r14, -100
	addi r4, r0, -99
	bltu r3, r4, .LBB6_9
.LBB6_6:
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r16, r3
	ldw r3, r3+0
	beq r3, r15, .LBB6_9
.LBB6_7:
	ble r1, r15, .LBB6_30
.LBB6_8:
	addi r1, r1, -1
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r16, r3
	ldw r3, r3+0
	mul r4, r3, r1
	addi r5, r0, 0
	add r3, r13, r0
	jal r31, fseek
.LBB6_9:
	lui r1, %hi(fio_upos)
	addi r1, r1, %lo(fio_upos)
	stw r1+0, r15
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r16, r1
	ldw r1, r1+0
	beq r1, r15, .LBB6_18
.LBB6_10:
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r16, r1
	ldw r5, r1+0
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	addi r4, r0, 1
	add r6, r13, r0
	jal r31, fread
	lui r3, %hi(fio_ulen)
	addi r3, r3, %lo(fio_ulen)
	stw r3+0, r1
	bne r1, r15, .LBB6_29
.LBB6_11:
	andi r1, r11, 2
	addi r3, r0, 0
	bne r1, r3, .LBB6_28
.LBB6_12:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r0, .LBB6_32
.LBB6_13:
	lui r3, %hi(fio_rec_next)
	addi r3, r3, %lo(fio_rec_next)
	ldw r1, r3+0
	stw r3+0, r12
	addi r3, r14, -100
	addi r4, r0, -99
	bltu r3, r4, .LBB6_17
.LBB6_14:
	slli r3, r14, 2
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r3, r4
	ldw r4, r4+0
	beq r4, r12, .LBB6_17
.LBB6_15:
	ble r1, r12, .LBB6_30
.LBB6_16:
	addi r1, r1, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r3, r3, r4
	ldw r3, r3+0
	mul r4, r3, r1
	addi r5, r0, 0
	add r3, r13, r0
	jal r31, fseek
.LBB6_17:
	lui r1, %hi(fio_upos)
	addi r1, r1, %lo(fio_upos)
	stw r1+0, r12
.LBB6_18:
	slli r16, r14, 2
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r14, r16, r1
	ldw r1, r14+0
	blt r1, r12, .LBB6_20
.LBB6_19:
	add r3, r13, r0
	jal r31, ftell
	ldw r3, r14+0
	bge r1, r3, .LBB6_25
.LBB6_20:
	addi r3, sp, 20
	addi r15, r0, 1
	addi r14, r0, 4
	add r4, r15, r0
	add r5, r14, r0
	add r6, r13, r0
	jal r31, fread
	bne r1, r14, .LBB6_24
.LBB6_21:
	addi r3, sp, 20
	addi r1, r3, 3
	ldbu r1, r1+0
	slli r1, r1, 24
	blt r1, r12, .LBB6_27
.LBB6_22:
	ldhu r4, sp+20
	addi r3, r3, 2
	ldbu r3, r3+0
	slli r3, r3, 16
	or  r3, r4, r3
	or  r5, r3, r1
	lui r1, 16
	addi r1, r1, 1
	bge r5, r1, .LBB6_27
.LBB6_23:
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	addi r11, r0, 1
	add r4, r11, r0
	add r6, r13, r0
	jal r31, fread
	lui r3, %hi(fio_ulen)
	addi r3, r3, %lo(fio_ulen)
	stw r3+0, r1
	addi r3, sp, 20
	addi r5, r0, 4
	add r4, r11, r0
	add r6, r13, r0
	jal r31, fread
	add r15, r12, r0
	jal r0, .LBB6_29
.LBB6_24:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r16, r1
	stw r1+0, r15
	andi r1, r11, 3
	bne r1, r12, .LBB6_29
	jal r0, .LBB6_26
.LBB6_25:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r16, r1
	addi r15, r0, 1
	stw r1+0, r15
	andi r1, r11, 3
	addi r3, r0, 0
	bne r1, r3, .LBB6_29
.LBB6_26:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r0, .LBB6_32
.LBB6_27:
	andi r1, r11, 2
	addi r3, r0, 0
	beq r1, r3, .LBB6_31
.LBB6_28:
	addi r15, r0, 2
.LBB6_29:
	add r1, r15, r0
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB6_30:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	jal r0, .LBB6_32
.LBB6_31:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
.LBB6_32:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
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
	addi r1, r0, 1
	blt r4, r1, .LBB7_3
.LBB7_1:
	add r11, r4, r0
	lui r12, %hi(fio_upos)
	addi r12, r12, %lo(fio_upos)
	ldw r1, r12+0
	add r4, r1, r4
	lui r5, %hi(fio_ulen)
	addi r5, r5, %lo(fio_ulen)
	ldw r5, r5+0
	bgt r4, r5, .LBB7_4
.LBB7_2:
	lui r4, %hi(fio_urec)
	addi r4, r4, %lo(fio_urec)
	add r4, r1, r4
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r12+0
	add r1, r1, r11
	stw r12+0, r1
.LBB7_3:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB7_4:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.5)
	addi r4, r4, %lo(.L.str.5)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
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
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
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
	addi r1, r3, -100
	addi r4, r0, -99
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
	ldw r1, sp+20
	sub r1, r14, r1
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
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r7, r1+0
	addi r11, r0, 0
	bne r7, r11, .LBB10_4
.LBB10_1:
	addi r7, r0, -400
	lui r8, %hi(fio_uend+400)
	addi r8, r8, %lo(fio_uend+400)
	addi r9, r0, -1
.LBB10_2:
	add r10, r7, r8
	stw r10+0, r9
	addi r7, r7, 4
	bne r7, r11, .LBB10_2
.LBB10_3:
	addi r7, r0, 1
	stb r1+0, r7
.LBB10_4:
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	lui r7, %hi(fio_out)
	addi r7, r7, %lo(fio_out)
	stw r7+0, r1
	lui r1, %hi(fio_unit)
	addi r1, r1, %lo(fio_unit)
	addi r12, r0, -1
	stw r1+0, r12
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r3
	lui r1, %hi(fio_ilen)
	addi r1, r1, %lo(fio_ilen)
	stw r1+0, r4
	lui r1, %hi(fio_inrec)
	addi r1, r1, %lo(fio_inrec)
	stw r1+0, r5
	lui r1, %hi(fio_irec)
	addi r1, r1, %lo(fio_irec)
	stw r1+0, r11
	lui r1, %hi(fio_reading)
	addi r1, r1, %lo(fio_reading)
	stb r1+0, r11
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	stw r1+0, r11
	lui r1, %hi(fio_col)
	addi r1, r1, %lo(fio_col)
	stw r1+0, r11
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	stb r1+0, r11
	lui r1, %hi(fio_gdepth)
	addi r1, r1, %lo(fio_gdepth)
	stw r1+0, r11
	lui r1, %hi(fio_scale)
	addi r1, r1, %lo(fio_scale)
	stw r1+0, r11
	lui r1, %hi(fio_rep)
	addi r1, r1, %lo(fio_rep)
	stw r1+0, r11
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	stw r1+0, r11
	lui r3, %hi(fio_first_item)
	addi r3, r3, %lo(fio_first_item)
	addi r1, r0, 1
	stb r3+0, r1
	lui r3, %hi(fio_prev_char)
	addi r3, r3, %lo(fio_prev_char)
	stb r3+0, r11
	lui r13, %hi(fio_pos)
	addi r13, r13, %lo(fio_pos)
	lui r14, %hi(fio_revert)
	addi r14, r14, %lo(fio_revert)
	beq r6, r11, .LBB10_11
.LBB10_5:
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	stw r1+0, r11
	add r3, r6, r0
	jal r31, fio_fmt_use
	ble r1, r12, .LBB10_14
.LBB10_6:
	stw r13+0, r11
	stw r14+0, r12
	lui r1, %hi(fio_since_data)
	addi r1, r1, %lo(fio_since_data)
	stb r1+0, r11
	lui r1, %hi(fio_fmt)
	addi r1, r1, %lo(fio_fmt)
	ldw r1, r1+0
	addi r3, r0, 32
.LBB10_7:
	add r4, r1, r11
	ldbu r4, r4+0
	bne r4, r3, .LBB10_9
.LBB10_8:
	addi r11, r11, 1
	stw r13+0, r11
	jal r0, .LBB10_7
.LBB10_9:
	addi r1, r0, 40
	bne r4, r1, .LBB10_13
.LBB10_10:
	addi r1, r11, 1
	add r12, r1, r0
	jal r0, .LBB10_12
.LBB10_11:
	lui r3, %hi(fio_listed)
	addi r3, r3, %lo(fio_listed)
	stw r3+0, r1
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
	lui r3, %hi(fio_fmt)
	addi r3, r3, %lo(fio_fmt)
	stw r3+0, r1
	addi r1, r0, 0
.LBB10_12:
	stw r13+0, r1
	stw r14+0, r12
.LBB10_13:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB10_14:
	addi r3, r0, 2
	jal r31, exit
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
	add r12, r3, r0
	stw sp+88, r3
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
	addi r4, r0, 0
	bgt r3, r4, .LBB11_16
.LBB11_15:
	stw r16+0, r11
	jal r31, fio_next_desc
.LBB11_16:
	beq r1, r11, .LBB11_12
.LBB11_17:
	addi r3, r0, 76
	beq r1, r3, .LBB11_21
.LBB11_18:
	addi r3, r0, 65
	bne r1, r3, .LBB11_34
.LBB11_19:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	addi r1, r3, -1
	addi r4, r0, 3
	bgeu r1, r4, .LBB11_37
.LBB11_20:
	add r1, r3, r0
	jal r0, .LBB11_39
.LBB11_21:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r3, r0, 2
	blt r1, r3, .LBB11_23
.LBB11_22:
	lui r3, %hi(fio_col)
	addi r3, r3, %lo(fio_col)
	ldw r4, r3+0
	add r1, r1, r4
	addi r1, r1, -1
	stw r3+0, r1
.LBB11_23:
	lui r3, %hi(fio_col)
	addi r3, r3, %lo(fio_col)
	ldw r1, r3+0
	addi r4, r0, 1022
	bgt r1, r4, .LBB11_30
.LBB11_24:
	lui r4, %hi(fio_len)
	addi r4, r4, %lo(fio_len)
	ldw r5, r4+0
	bge r5, r1, .LBB11_28
.LBB11_25:
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
.LBB11_26:
	addi r8, r5, 1
	add r5, r5, r6
	stb r5+0, r7
	add r5, r8, r0
	bne r1, r8, .LBB11_26
.LBB11_27:
	stw r4+0, r1
	add r5, r1, r0
.LBB11_28:
	addi r6, r0, 0
	seq r6, r12, r6
	sub r6, r0, r6
	andi r6, r6, 18
	xori r6, r6, 84
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	add r7, r1, r7
	stb r7+0, r6
	addi r6, r1, 1
	stw r3+0, r6
	blt r1, r5, .LBB11_30
.LBB11_29:
	stw r4+0, r6
.LBB11_30:
	ldw r1, r17+0
	addi r3, r0, 1
	blt r1, r3, .LBB11_32
.LBB11_31:
	addi r1, r1, -1
	stw r17+0, r1
.LBB11_32:
	ldw r3, r17+0
	addi r1, r0, 0
	bne r3, r1, .LBB11_12
.LBB11_33:
	stw r16+0, r1
	jal r0, .LBB11_12
.LBB11_34:
	lui r19, %hi(fio_d)
	addi r19, r19, %lo(fio_d)
	ldw r1, r19+0
	addi r18, r0, 1
	blt r1, r18, .LBB11_48
.LBB11_35:
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
	slt r1, r12, r11
	lui r3, %hi(.L.str.10)
	addi r3, r3, %lo(.L.str.10)
	lui r4, %hi(.L.str.9)
	addi r4, r4, %lo(.L.str.9)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r12, r1, r3
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
	blt r15, r1, .LBB11_54
.LBB11_36:
	lui r5, %hi(.L.str.11)
	addi r5, r5, %lo(.L.str.11)
	addi r3, sp, 56
	addi r4, r0, 32
	addi r7, sp, 24
	jal r0, .LBB11_53
.LBB11_37:
	addi r1, r0, 4
	addi r4, r0, 5
	blt r3, r4, .LBB11_39
.LBB11_38:
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r5, r4+0
	add r3, r3, r5
	addi r3, r3, -4
	stw r4+0, r3
.LBB11_39:
	lui r3, %hi(fio_len)
	addi r3, r3, %lo(fio_len)
	ldw r8, r3+0
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r9, r4+0
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	jal r0, .LBB11_42
.LBB11_40:
	add r9, r10, r0
.LBB11_41:
	addi r11, r11, 1
	beq r11, r1, .LBB11_30
.LBB11_42:
	addi r10, sp, 88
	bgt r9, r5, .LBB11_41
.LBB11_43:
	bge r8, r9, .LBB11_46
.LBB11_44:
	addi lr, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, lr, r0
	bne r9, lr, .LBB11_44
.LBB11_45:
	stw r3+0, r9
	add r8, r9, r0
.LBB11_46:
	add r10, r10, r11
	ldbu r10, r10+0
	add lr, r9, r6
	stb lr+0, r10
	addi r10, r9, 1
	stw r4+0, r10
	blt r9, r8, .LBB11_40
.LBB11_47:
	stw r3+0, r10
	add r8, r10, r0
	jal r0, .LBB11_40
.LBB11_48:
	lui r1, %hi(fio_fmt)
	addi r1, r1, %lo(fio_fmt)
	ldw r1, r1+0
	lui r3, %hi(fio_pos)
	addi r3, r3, %lo(fio_pos)
	ldw r3, r3+0
	add r1, r1, r3
	ldbu r3, r1+-1
	addi r4, r0, 48
	bne r3, r4, .LBB11_52
.LBB11_49:
	bne r12, r11, .LBB11_52
.LBB11_50:
	ldbu r1, r1+-2
	andi r1, r1, 255
	addi r3, r0, 46
	bne r1, r3, .LBB11_52
.LBB11_51:
	stb sp+56, r11
	jal r0, .LBB11_54
.LBB11_52:
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r3, sp, 56
	addi r4, r0, 32
.LBB11_53:
	add r6, r12, r0
	jal r31, snprintf
.LBB11_54:
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	ldbu r1, r1+0
	bne r1, r18, .LBB11_59
.LBB11_55:
	ldbu r1, sp+56
	beq r1, r11, .LBB11_59
.LBB11_56:
	addi r14, r0, 43
	beq r1, r14, .LBB11_59
.LBB11_57:
	addi r3, r0, 45
	beq r1, r3, .LBB11_59
.LBB11_58:
	addi r12, sp, 56
	addi r13, r12, 1
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, memmove
	stb sp+56, r14
.LBB11_59:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	addi r3, sp, 56
	jal r31, fio_field
	ldw r1, r17+0
	blt r1, r18, .LBB11_61
.LBB11_60:
	addi r1, r1, -1
	stw r17+0, r1
.LBB11_61:
	ldw r1, r17+0
	bne r1, r11, .LBB11_12
.LBB11_62:
	stw r16+0, r11
	jal r0, .LBB11_12
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
	stw sp+104, r13
	stw sp+100, r3
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
	beq r1, r18, .LBB13_4
.LBB13_3:
	ldw r3, r15+0
	addi r4, r0, 0
	bgt r3, r4, .LBB13_5
.LBB13_4:
	stw r14+0, r18
	jal r31, fio_next_desc
.LBB13_5:
	addi r4, r1, -65
	addi r3, r0, 8
	bgtu r4, r3, .LBB13_9
.LBB13_6:
	slli r4, r4, 2
	lui r5, %hi(.LJTI13_0)
	addi r5, r5, %lo(.LJTI13_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB13_7:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r4, r1, -1
	addi r5, r0, 7
	bgeu r4, r5, .LBB13_61
.LBB13_8:
	add r3, r1, r0
	jal r0, .LBB13_63
.LBB13_9:
	addi r3, r0, 0
	beq r1, r3, .LBB13_121
.LBB13_10:
	addi r3, r0, -2
	and r3, r1, r3
	addi r4, r0, 68
	bne r3, r4, .LBB13_57
.LBB13_11:
	lui r3, %hi(fio_d)
	addi r3, r3, %lo(fio_d)
	ldw r4, r3+0
	addi r3, sp, 36
	add r5, r12, r0
	add r6, r13, r0
	add r7, r1, r0
	jal r31, fio_efmt
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	ldbu r1, r1+0
	addi r13, r0, 1
	bne r1, r13, .LBB13_16
.LBB13_12:
	ldbu r1, sp+36
	addi r3, r0, 0
	beq r1, r3, .LBB13_16
.LBB13_13:
	addi r16, r0, 43
	beq r1, r16, .LBB13_16
.LBB13_14:
	addi r3, r0, 45
	beq r1, r3, .LBB13_16
.LBB13_15:
	addi r11, sp, 36
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+36, r16
.LBB13_16:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r11, r1+0
	addi r3, sp, 36
	jal r31, strlen
	bge r11, r13, .LBB13_51
	jal r0, .LBB13_116
.LBB13_17:
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
.LBB13_18:
	fmul.d r4, r4, r6
	addi r10, r10, -1
	bne r10, r18, .LBB13_18
.LBB13_19:
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
	bne lr, r18, .LBB13_22
.LBB13_20:
	lui lr, %hi(.LCPI13_3)
	addi lr, lr, %lo(.LCPI13_3)
	ldw r21, lr+4
	ldw r20, lr+0
	fle.d lr, r20, r10
	xori lr, lr, 1
	bne lr, r18, .LBB13_23
.LBB13_21:
	flt.d r4, r10, r4
	xori r4, r4, 1
	bne r4, r18, .LBB13_23
.LBB13_22:
	addi r16, r0, 0
.LBB13_23:
	xori r4, r3, 1
	lui r17, %hi(fio_w)
	addi r17, r17, %lo(fio_w)
	beq r16, r18, .LBB13_58
.LBB13_24:
	addi r3, sp, 36
	addi r7, r0, 69
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fio_efmt
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	ldbu r1, r1+0
	addi r13, r0, 1
	bne r1, r13, .LBB13_29
.LBB13_25:
	ldbu r1, sp+36
	addi r3, r0, 0
	beq r1, r3, .LBB13_29
.LBB13_26:
	addi r18, r0, 43
	beq r1, r18, .LBB13_29
.LBB13_27:
	addi r3, r0, 45
	beq r1, r3, .LBB13_29
.LBB13_28:
	addi r11, sp, 36
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+36, r18
.LBB13_29:
	ldw r11, r17+0
	addi r3, sp, 36
	jal r31, strlen
	blt r11, r13, .LBB13_115
.LBB13_30:
	addi r3, r11, 1
	bne r1, r3, .LBB13_115
.LBB13_31:
	ldbu r3, sp+36
	addi r4, r0, 43
	beq r3, r4, .LBB13_75
.LBB13_32:
	addi r4, r0, 45
	beq r3, r4, .LBB13_75
.LBB13_33:
	addi r4, r0, 48
	bne r3, r4, .LBB13_115
.LBB13_34:
	addi r3, sp, 36
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB13_115
.LBB13_35:
	addi r3, sp, 36
	add r5, r1, r0
	jal r0, .LBB13_78
.LBB13_36:
	fcvt.w.d r6, r12
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r3, sp, 36
	addi r4, r0, 64
	jal r31, snprintf
	jal r0, .LBB13_116
.LBB13_37:
	lui r1, %hi(fio_scale)
	addi r1, r1, %lo(fio_scale)
	ldw r1, r1+0
	addi r16, r0, 1
	blt r1, r16, .LBB13_40
.LBB13_38:
	addi r3, r1, 1
	lui r1, %hi(.LCPI13_1)
	addi r1, r1, %lo(.LCPI13_1)
	ldw r5, r1+4
	ldw r4, r1+0
	addi r1, r0, 0
.LBB13_39:
	fmul.d r12, r12, r4
	addi r3, r3, -1
	bgt r3, r16, .LBB13_39
.LBB13_40:
	addi r3, r0, -1
	bgt r1, r3, .LBB13_43
.LBB13_41:
	lui r3, %hi(.LCPI13_1)
	addi r3, r3, %lo(.LCPI13_1)
	ldw r5, r3+4
	ldw r4, r3+0
.LBB13_42:
	fdiv.d r12, r12, r4
	addi r3, r1, 1
	sltu r6, r3, r1
	add r1, r3, r0
	bne r6, r16, .LBB13_42
.LBB13_43:
	lui r17, %hi(fio_d)
	addi r17, r17, %lo(fio_d)
	ldw r6, r17+0
	lui r5, %hi(.L.str.13)
	addi r5, r5, %lo(.L.str.13)
	addi r11, sp, 20
	addi r4, r0, 16
	add r3, r11, r0
	jal r31, snprintf
	addi r3, sp, 36
	addi r4, r0, 64
	add r5, r11, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	ldw r1, r17+0
	addi r11, r0, 0
	bne r1, r11, .LBB13_45
.LBB13_44:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	addi r3, sp, 36
	jal r31, strcat
.LBB13_45:
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	ldbu r1, r1+0
	bne r1, r16, .LBB13_50
.LBB13_46:
	ldbu r1, sp+36
	beq r1, r11, .LBB13_50
.LBB13_47:
	addi r13, r0, 43
	beq r1, r13, .LBB13_50
.LBB13_48:
	addi r3, r0, 45
	beq r1, r3, .LBB13_50
.LBB13_49:
	addi r11, sp, 36
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+36, r13
.LBB13_50:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r11, r1+0
	addi r3, sp, 36
	jal r31, strlen
	blt r11, r16, .LBB13_116
.LBB13_51:
	addi r3, r11, 1
	bne r1, r3, .LBB13_116
.LBB13_52:
	ldbu r3, sp+36
	addi r4, r0, 43
	beq r3, r4, .LBB13_72
.LBB13_53:
	addi r4, r0, 45
	beq r3, r4, .LBB13_72
.LBB13_54:
	addi r4, r0, 48
	bne r3, r4, .LBB13_116
.LBB13_55:
	addi r3, sp, 36
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB13_116
.LBB13_56:
	addi r3, sp, 36
	add r5, r1, r0
	jal r31, memmove
	jal r0, .LBB13_116
.LBB13_57:
	lui r5, %hi(.L.str.15)
	addi r5, r5, %lo(.L.str.15)
	addi r3, sp, 36
	addi r4, r0, 64
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	jal r0, .LBB13_116
.LBB13_58:
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
	bne r8, r18, .LBB13_83
.LBB13_59:
	fle.d r5, r6, r10
	xori r8, r5, 1
	addi r5, r0, 0
	beq r8, r5, .LBB13_79
.LBB13_60:
	addi r5, r0, -1
	jal r0, .LBB13_83
.LBB13_61:
	addi r4, r0, 9
	blt r1, r4, .LBB13_63
.LBB13_62:
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r5, r4+0
	add r1, r1, r5
	addi r1, r1, -8
	stw r4+0, r1
.LBB13_63:
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	ldw r9, r1+0
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r10, r4+0
	addi r5, r0, 0
	addi r6, r0, 1022
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
	jal r0, .LBB13_66
.LBB13_64:
	add r10, lr, r0
.LBB13_65:
	addi r5, r5, 1
	beq r5, r3, .LBB13_117
.LBB13_66:
	addi lr, sp, 100
	bgt r10, r6, .LBB13_65
.LBB13_67:
	bge r9, r10, .LBB13_70
.LBB13_68:
	addi r11, r9, 1
	add r9, r9, r7
	stb r9+0, r8
	add r9, r11, r0
	bne r10, r11, .LBB13_68
.LBB13_69:
	stw r1+0, r10
	add r9, r10, r0
.LBB13_70:
	add lr, lr, r5
	ldbu lr, lr+0
	add r11, r10, r7
	stb r11+0, lr
	addi lr, r10, 1
	stw r4+0, lr
	blt r10, r9, .LBB13_64
.LBB13_71:
	stw r1+0, lr
	add r9, lr, r0
	jal r0, .LBB13_64
.LBB13_72:
	addi r3, sp, 36
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB13_116
.LBB13_73:
	addi r4, sp, 36
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB13_116
.LBB13_74:
	addi r5, r1, -1
	jal r31, memmove
	jal r0, .LBB13_116
.LBB13_75:
	addi r3, sp, 36
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB13_115
.LBB13_76:
	addi r4, sp, 36
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB13_115
.LBB13_77:
	addi r5, r1, -1
.LBB13_78:
	jal r31, memmove
	jal r0, .LBB13_115
.LBB13_79:
	addi r8, r0, 1
.LBB13_80:
	add r9, r8, r0
	fdiv.d r10, r10, r6
	fle.d r8, r6, r10
	xori lr, r8, 1
	addi r8, r9, 1
	bne lr, r5, .LBB13_82
.LBB13_81:
	bgt r1, r9, .LBB13_80
.LBB13_82:
	sub r5, r0, r8
.LBB13_83:
	xor r21, r19, r3
	add r22, r5, r4
	sgt r1, r22, r18
	sub r1, r0, r1
	and r6, r22, r1
	lui r5, %hi(.L.str.13)
	addi r5, r5, %lo(.L.str.13)
	addi r11, sp, 20
	addi r4, r0, 16
	add r3, r11, r0
	jal r31, snprintf
	addi r3, sp, 36
	addi r4, r0, 64
	add r5, r11, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	bgt r22, r18, .LBB13_85
.LBB13_84:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	addi r3, sp, 36
	jal r31, strcat
.LBB13_85:
	sub r11, r20, r21
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	ldbu r1, r1+0
	addi r18, r0, 1
	bne r1, r18, .LBB13_90
.LBB13_86:
	ldbu r1, sp+36
	addi r3, r0, 0
	beq r1, r3, .LBB13_90
.LBB13_87:
	addi r20, r0, 43
	beq r1, r20, .LBB13_90
.LBB13_88:
	addi r3, r0, 45
	beq r1, r3, .LBB13_90
.LBB13_89:
	addi r12, sp, 36
	addi r13, r12, 1
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, memmove
	stb sp+36, r20
.LBB13_90:
	addi r3, sp, 36
	jal r31, strlen
	blt r11, r18, .LBB13_101
.LBB13_91:
	addi r3, r11, 1
	bne r1, r3, .LBB13_101
.LBB13_92:
	ldbu r3, sp+36
	addi r4, r0, 43
	beq r3, r4, .LBB13_97
.LBB13_93:
	addi r4, r0, 45
	beq r3, r4, .LBB13_97
.LBB13_94:
	addi r4, r0, 48
	bne r3, r4, .LBB13_101
.LBB13_95:
	addi r3, sp, 36
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB13_101
.LBB13_96:
	addi r3, sp, 36
	add r5, r1, r0
	jal r0, .LBB13_100
.LBB13_97:
	addi r3, sp, 36
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB13_101
.LBB13_98:
	addi r4, sp, 36
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB13_101
.LBB13_99:
	addi r5, r1, -1
.LBB13_100:
	jal r31, memmove
.LBB13_101:
	addi r3, sp, 36
	add r4, r11, r0
	jal r31, fio_field
	ldw r1, r17+0
	slt r1, r1, r19
	sub r1, r0, r1
	and r1, r19, r1
	xor r10, r19, r1
	blt r10, r18, .LBB13_111
.LBB13_102:
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
	jal r0, .LBB13_105
.LBB13_103:
	add r8, r10, r0
.LBB13_104:
	addi r10, r9, -1
	ble r9, r18, .LBB13_111
.LBB13_105:
	add r9, r10, r0
	bgt r8, r4, .LBB13_104
.LBB13_106:
	bge r7, r8, .LBB13_109
.LBB13_107:
	addi r10, r7, 1
	add r7, r7, r5
	stb r7+0, r6
	add r7, r10, r0
	bne r8, r10, .LBB13_107
.LBB13_108:
	stw r1+0, r8
	add r7, r8, r0
.LBB13_109:
	add r10, r8, r5
	stb r10+0, r6
	addi r10, r8, 1
	stw r3+0, r10
	blt r8, r7, .LBB13_103
.LBB13_110:
	stw r1+0, r10
	add r7, r10, r0
	jal r0, .LBB13_103
.LBB13_111:
	ldw r1, r15+0
	blt r1, r18, .LBB13_113
.LBB13_112:
	addi r1, r1, -1
	stw r15+0, r1
.LBB13_113:
	ldw r3, r15+0
	addi r1, r0, 0
	bne r3, r1, .LBB13_115
.LBB13_114:
	stw r14+0, r1
.LBB13_115:
	addi r1, r0, 0
	beq r16, r1, .LBB13_121
.LBB13_116:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	addi r3, sp, 36
	jal r31, fio_field
.LBB13_117:
	ldw r1, r15+0
	addi r3, r0, 1
	blt r1, r3, .LBB13_119
.LBB13_118:
	addi r1, r1, -1
	stw r15+0, r1
.LBB13_119:
	ldw r3, r15+0
	addi r1, r0, 0
	bne r3, r1, .LBB13_121
.LBB13_120:
	stw r14+0, r1
.LBB13_121:
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
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI13_0,@object
.LJTI13_0:
	.word	.LBB13_7
	.word	.LBB13_10
	.word	.LBB13_10
	.word	.LBB13_10
	.word	.LBB13_10
	.word	.LBB13_37
	.word	.LBB13_17
	.word	.LBB13_10
	.word	.LBB13_36
	.size	.LJTI13_0, 36
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
	addi sp, sp, -176
	stw sp+0, lr
	stw sp+172, r11
	stw sp+168, r12
	stw sp+164, r13
	stw sp+160, r14
	stw sp+156, r15
	stw sp+152, r16
	stw sp+148, r17
	stw sp+144, r18
	stw sp+140, r19
	stw sp+136, r20
	stw sp+132, r21
	stw sp+128, r22
	stw sp+124, r23
	stw sp+120, r24
	stw sp+116, r25
	stw sp+112, r26
	stw sp+108, r27
	add r11, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	lui r1, %hi(.LCPI14_0)
	addi r1, r1, %lo(.LCPI14_0)
	ldw r21, r1+4
	ldw r20, r1+0
	flt.d r1, r12, r20
	fneg.d r6, r12
	xor r3, r7, r13
	sub r1, r0, r1
	and r3, r3, r1
	xor r5, r13, r3
	xor r3, r6, r12
	and r1, r3, r1
	xor r4, r12, r1
	addi r17, r0, 0
	sne r22, r11, r17
	sub r1, r0, r22
	andi r3, r1, 24
	lui r15, %hi(fio_col)
	addi r15, r15, %lo(fio_col)
	ldw r6, r15+0
	addi r16, r0, 1022
	lui r18, %hi(fio_len)
	addi r18, r18, %lo(fio_len)
	lui r19, %hi(fio_line)
	addi r19, r19, %lo(fio_line)
	bgt r6, r16, .LBB14_6
.LBB14_1:
	ldw r8, r18+0
	addi r7, r0, 32
	bge r8, r6, .LBB14_4
.LBB14_2:
	addi r9, r8, 1
	add r8, r8, r19
	stb r8+0, r7
	add r8, r9, r0
	bne r6, r9, .LBB14_2
.LBB14_3:
	stw r18+0, r6
	add r8, r6, r0
.LBB14_4:
	add r9, r6, r19
	stb r9+0, r7
	addi r7, r6, 1
	stw r15+0, r7
	blt r6, r8, .LBB14_6
.LBB14_5:
	stw r18+0, r7
.LBB14_6:
	xori r23, r3, 9
	feq.d r3, r12, r20
	bne r3, r17, .LBB14_9
.LBB14_7:
	lui r3, %hi(.LCPI14_2)
	addi r3, r3, %lo(.LCPI14_2)
	ldw r7, r3+4
	ldw r6, r3+0
	fle.d r3, r6, r4
	xori r3, r3, 1
	bne r3, r17, .LBB14_24
.LBB14_8:
	andi r3, r1, 8
	lui r6, %hi(.LCPI14_1)
	addi r6, r6, %lo(.LCPI14_1)
	add r3, r6, r3
	ldw r7, r3+4
	ldw r6, r3+0
	flt.d r3, r4, r6
	xori r3, r3, 1
	bne r3, r17, .LBB14_24
.LBB14_9:
	lui r1, %hi(.LCPI14_3)
	addi r1, r1, %lo(.LCPI14_3)
	ldw r7, r1+4
	ldw r6, r1+0
	fle.d r1, r6, r4
	xori r3, r1, 1
	addi r24, r0, 1
	add r1, r24, r0
	bne r3, r17, .LBB14_12
.LBB14_10:
	lui r1, %hi(.LCPI14_4)
	addi r1, r1, %lo(.LCPI14_4)
	ldw r9, r1+4
	ldw r8, r1+0
	add r26, r4, r0
	add r27, r5, r0
	add r1, r17, r0
.LBB14_11:
	fdiv.d r26, r26, r8
	addi r1, r1, 1
	fle.d r3, r6, r26
	bne r3, r17, .LBB14_11
.LBB14_12:
	flt.d r3, r4, r6
	feq.d r4, r12, r20
	xori r4, r4, 1
	and r25, r4, r3
	sub r3, r0, r25
	and r3, r1, r3
	xor r1, r1, r3
	sub r6, r23, r1
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
	bne r25, r24, .LBB14_15
.LBB14_13:
	flt.d r12, r12, r20
	addi r3, sp, 44
	jal r31, strlen
	add r3, r12, r23
	addi r3, r3, 2
	bleu r1, r3, .LBB14_15
.LBB14_14:
	addi r1, sp, 44
	add r1, r1, r3
	addi r3, r0, 0
	stb r1+0, r3
.LBB14_15:
	addi r12, r22, 4
	addi r1, r0, 0
	sne r1, r11, r1
	sub r1, r0, r1
	andi r1, r1, 24
	xori r4, r1, 12
	addi r3, sp, 44
	jal r31, fio_field
	ldw r4, r18+0
	ldw r5, r15+0
	addi r1, r0, 1
	addi r3, r0, 32
	jal r0, .LBB14_18
.LBB14_16:
	add r5, r7, r0
.LBB14_17:
	addi r12, r6, -1
	bleu r6, r1, .LBB14_29
.LBB14_18:
	add r6, r12, r0
	bgt r5, r16, .LBB14_17
.LBB14_19:
	bge r4, r5, .LBB14_22
.LBB14_20:
	addi r7, r4, 1
	add r4, r4, r19
	stb r4+0, r3
	add r4, r7, r0
	bne r5, r7, .LBB14_20
.LBB14_21:
	stw r18+0, r5
	add r4, r5, r0
.LBB14_22:
	add r7, r5, r19
	stb r7+0, r3
	addi r7, r5, 1
	stw r15+0, r7
	blt r5, r4, .LBB14_16
.LBB14_23:
	stw r18+0, r7
	add r4, r7, r0
	jal r0, .LBB14_16
.LBB14_24:
	andi r1, r1, 9
	addi r14, r1, 16
	addi r6, r23, -1
	lui r5, %hi(.L.str.85)
	addi r5, r5, %lo(.L.str.85)
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
	beq r11, r17, .LBB14_28
.LBB14_25:
	beq r1, r17, .LBB14_28
.LBB14_26:
	addi r11, r1, 2
	add r3, r11, r0
	jal r31, strlen
	addi r3, r0, 2
	bgt r1, r3, .LBB14_28
.LBB14_27:
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
.LBB14_28:
	addi r3, sp, 44
	add r4, r14, r0
	jal r31, fio_field
.LBB14_29:
	lui r1, %hi(fio_first_item)
	addi r1, r1, %lo(fio_first_item)
	stb r1+0, r17
	lui r1, %hi(fio_prev_char)
	addi r1, r1, %lo(fio_prev_char)
	stb r1+0, r17
	ldw r27, sp+108
	ldw r26, sp+112
	ldw r25, sp+116
	ldw r24, sp+120
	ldw r23, sp+124
	ldw r22, sp+128
	ldw r21, sp+132
	ldw r20, sp+136
	ldw r19, sp+140
	ldw r18, sp+144
	ldw r17, sp+148
	ldw r16, sp+152
	ldw r15, sp+156
	ldw r14, sp+160
	ldw r13, sp+164
	ldw r12, sp+168
	ldw r11, sp+172
	ldw lr, sp+0
	addi sp, sp, 176
	jalr r0, r31, 0
.Lfunc_end14:
	.size	fio_list_out_real, .Lfunc_end14-fio_list_out_real
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_efmt
	.type	.LCPI15_0,@object
.LCPI15_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI15_0, 8
	.text
	.p2align	2
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
	stw sp+116, r21
	add r11, r7, r0
	add r13, r6, r0
	add r12, r5, r0
	add r14, r3, r0
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
	addi r19, r0, 0
	sgt r3, r18, r19
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
	lui r5, %hi(.L.str.85)
	addi r5, r5, %lo(.L.str.85)
	addi r15, sp, 36
	addi r4, r0, 16
	add r3, r15, r0
	jal r31, snprintf
	addi r16, sp, 52
	addi r4, r0, 64
	add r3, r16, r0
	add r5, r15, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	ldbu r21, sp+52
	addi r20, r0, 45
	seq r1, r21, r20
	addi r3, r0, 43
	seq r3, r21, r3
	or  r1, r1, r3
	or  r1, r1, r16
	addi r3, r1, 1
	addi r1, r0, 68
	addi r16, r0, 46
	addi r4, r0, 69
	addi r5, r0, 101
	add r15, r19, r0
	jal r0, .LBB15_3
.LBB15_1:
	addi r7, sp, 52
	add r7, r7, r15
	stb r7+40, r6
	addi r15, r15, 1
.LBB15_2:
	addi r3, r3, 1
.LBB15_3:
	ldbu r6, r3+-1
	bgt r6, r1, .LBB15_6
.LBB15_4:
	beq r6, r16, .LBB15_2
.LBB15_5:
	bne r6, r19, .LBB15_1
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
	jal r0, .LBB15_10
.LBB15_9:
	add r1, r17, r0
.LBB15_10:
	lui r3, %hi(.LCPI15_0)
	addi r3, r3, %lo(.LCPI15_0)
	ldw r5, r3+4
	ldw r4, r3+0
	feq.d r3, r12, r4
	sub r3, r0, r3
	and r3, r1, r3
	add r7, r14, r0
	bne r21, r20, .LBB15_12
.LBB15_11:
	addi r7, r14, 1
	stb r14+0, r20
.LBB15_12:
	xor r1, r1, r3
	blt r18, r17, .LBB15_17
.LBB15_13:
	blt r15, r17, .LBB15_22
.LBB15_14:
	xor r3, r18, r15
	slt r6, r18, r15
	sub r6, r0, r6
	and r3, r3, r6
	xor r3, r15, r3
	addi r6, sp, 52
	addi r8, r6, 40
	addi r6, r0, 0
.LBB15_15:
	add r9, r7, r6
	add r10, r8, r6
	ldbu r10, r10+0
	stb r9+0, r10
	addi r6, r6, 1
	bne r3, r6, .LBB15_15
.LBB15_16:
	add r7, r7, r6
	jal r0, .LBB15_23
.LBB15_17:
	addi r9, r0, 48
	stb r7+0, r9
	addi r10, r7, 2
	stb r7+1, r16
	addi r6, r0, 0
	addi r8, r0, 55
	beq r18, r6, .LBB15_28
.LBB15_18:
	sub r3, r10, r14
	bgt r3, r8, .LBB15_28
.LBB15_19:
	sub r3, r7, r14
	addi r19, r3, 3
	addi r7, r0, -2
	addi lr, r0, 56
	add r16, r18, r0
.LBB15_20:
	addi r3, r10, 1
	stb r10+0, r9
	bgt r16, r7, .LBB15_29
.LBB15_21:
	add r20, r19, r0
	addi r16, r16, 1
	addi r19, r19, 1
	add r10, r3, r0
	blt r20, lr, .LBB15_20
	jal r0, .LBB15_29
.LBB15_22:
	addi r6, r0, 0
.LBB15_23:
	stb r7+0, r16
	addi r3, r7, 1
	bge r6, r15, .LBB15_35
.LBB15_24:
	sub r8, r3, r14
	addi r9, r0, 55
	bgt r8, r9, .LBB15_35
.LBB15_25:
	sub r7, r7, r14
	addi r9, r7, 2
	addi r7, sp, 52
	addi r7, r7, 40
	addi r8, r0, 56
.LBB15_26:
	add r10, r9, r0
	add r9, r7, r6
	ldbu r9, r9+0
	stb r3+0, r9
	addi r6, r6, 1
	addi r3, r3, 1
	bge r6, r15, .LBB15_35
.LBB15_27:
	addi r9, r10, 1
	blt r10, r8, .LBB15_26
	jal r0, .LBB15_35
.LBB15_28:
	add r3, r10, r0
.LBB15_29:
	blt r15, r17, .LBB15_35
.LBB15_30:
	sub r9, r3, r14
	bgt r9, r8, .LBB15_35
.LBB15_31:
	addi r7, sp, 52
	addi r7, r7, 40
	addi r8, r9, 1
	addi r9, r0, 56
.LBB15_32:
	add r10, r3, r6
	add lr, r7, r6
	ldbu lr, lr+0
	stb r10+0, lr
	addi r10, r6, 1
	bge r10, r15, .LBB15_34
.LBB15_33:
	add lr, r8, r6
	add r6, r10, r0
	blt lr, r9, .LBB15_32
.LBB15_34:
	add r3, r3, r10
.LBB15_35:
	feq.d r4, r12, r4
	xori r4, r4, 1
	sub r4, r0, r4
	and r4, r18, r4
	sub r8, r1, r4
	srai r1, r8, 31
	xor r4, r8, r1
	sub r6, r4, r1
	lui r1, %hi(fio_e)
	addi r1, r1, %lo(fio_e)
	ldw r5, r1+0
	addi r1, r0, 100
	sltu r7, r6, r1
	addi r1, r0, 0
	bne r5, r1, .LBB15_38
.LBB15_36:
	addi r4, r6, -100
	addi r9, r0, 900
	bgeu r4, r9, .LBB15_38
.LBB15_37:
	add r4, r3, r0
	jal r0, .LBB15_39
.LBB15_38:
	addi r4, r3, 1
	stb r3+0, r11
.LBB15_39:
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
.LBB15_40:
	add r10, r1, r0
	mulhu r1, r6, r3
	srli lr, r1, 3
	mul r1, lr, r8
	sub r1, r6, r1
	ori  r11, r1, 48
	addi r1, r10, 1
	addi r12, sp, 20
	add r12, r12, r10
	stb r12+0, r11
	bltu r6, r8, .LBB15_42
.LBB15_41:
	add r6, lr, r0
	bltu r10, r9, .LBB15_40
.LBB15_42:
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
	addi r6, sp, 20
	bge r1, r5, .LBB15_46
.LBB15_43:
	add r6, r6, r1
	addi r7, r0, 48
	add r8, r5, r0
.LBB15_44:
	stb r6+0, r7
	addi r6, r6, 1
	addi r8, r8, -1
	bne r1, r8, .LBB15_44
.LBB15_45:
	add r1, r5, r0
.LBB15_46:
	addi r4, r4, 1
	addi r5, sp, 20
	addi r5, r5, -1
.LBB15_47:
	add r6, r1, r0
	addi r1, r1, -1
	add r7, r5, r6
	ldbu r7, r7+0
	stb r4+0, r7
	addi r4, r4, 1
	bgt r6, r17, .LBB15_47
.LBB15_48:
	stb r4+0, r3
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
.Lfunc_end15:
	.size	fio_efmt, .Lfunc_end15-fio_efmt
                                        # -- End function
	.globl	f77_wr_r                        # -- Begin function f77_wr_r
	.p2align	2
	.type	f77_wr_r,@function
f77_wr_r:                               # @f77_wr_r
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r3
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r13, r0, 0
	beq r1, r13, .LBB16_2
.LBB16_1:
	fcvt.d.s r6, r3
	addi r5, r0, 0
	add r3, r6, r0
	add r4, r7, r0
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, fio_list_out_real
.LBB16_2:
	lui r11, %hi(fio_desc)
	addi r11, r11, %lo(fio_desc)
	ldw r1, r11+0
	lui r12, %hi(fio_rep)
	addi r12, r12, %lo(fio_rep)
	beq r1, r13, .LBB16_7
.LBB16_3:
	ldw r4, r12+0
	addi r5, r0, 0
	ble r4, r5, .LBB16_7
.LBB16_4:
	addi r4, r0, 65
	bne r1, r4, .LBB16_8
.LBB16_5:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r4, r1+0
	addi r3, r4, -1
	addi r5, r0, 3
	lui r1, %hi(fio_col)
	addi r1, r1, %lo(fio_col)
	bgeu r3, r5, .LBB16_9
.LBB16_6:
	add r3, r4, r0
	jal r0, .LBB16_11
.LBB16_7:
	stw r11+0, r13
	add r14, r3, r0
	jal r31, fio_next_desc
	add r3, r14, r0
	addi r4, r0, 65
	beq r1, r4, .LBB16_5
.LBB16_8:
	fcvt.d.s r4, r3
	add r3, r4, r0
	add r4, r5, r0
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_wr_d
.LBB16_9:
	addi r3, r0, 4
	addi r5, r0, 5
	blt r4, r5, .LBB16_11
.LBB16_10:
	ldw r5, r1+0
	add r4, r4, r5
	addi r4, r4, -4
	stw r1+0, r4
.LBB16_11:
	lui r4, %hi(fio_len)
	addi r4, r4, %lo(fio_len)
	ldw r8, r4+0
	ldw r9, r1+0
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	jal r0, .LBB16_14
.LBB16_12:
	add r9, r10, r0
.LBB16_13:
	addi r13, r13, 1
	beq r13, r3, .LBB16_20
.LBB16_14:
	addi r10, sp, 28
	bgt r9, r5, .LBB16_13
.LBB16_15:
	bge r8, r9, .LBB16_18
.LBB16_16:
	addi lr, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, lr, r0
	bne r9, lr, .LBB16_16
.LBB16_17:
	stw r4+0, r9
	add r8, r9, r0
.LBB16_18:
	add r10, r10, r13
	ldbu r10, r10+0
	add lr, r9, r6
	stb lr+0, r10
	addi r10, r9, 1
	stw r1+0, r10
	blt r9, r8, .LBB16_12
.LBB16_19:
	stw r4+0, r10
	add r8, r10, r0
	jal r0, .LBB16_12
.LBB16_20:
	ldw r1, r12+0
	addi r3, r0, 1
	blt r1, r3, .LBB16_22
.LBB16_21:
	addi r1, r1, -1
	stw r12+0, r1
.LBB16_22:
	ldw r3, r12+0
	addi r1, r0, 0
	bne r3, r1, .LBB16_24
.LBB16_23:
	stw r11+0, r1
.LBB16_24:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
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
	.p2align	2, 0x0                          # -- Begin function f77_cdiv
	.type	.LCPI20_0,@object
.LCPI20_0:
	.quad	0x43f0000000000000              # double 1.8446744073709552E+19
	.size	.LCPI20_0, 8
	.type	.LCPI20_1,@object
.LCPI20_1:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI20_1, 8
	.type	.LCPI20_2,@object
.LCPI20_2:
	.quad	0x3bf0000000000000              # double 5.4210108624275222E-20
	.size	.LCPI20_2, 8
	.type	.LCPI20_3,@object
.LCPI20_3:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI20_3, 8
	.text
	.globl	f77_cdiv
	.p2align	2
	.type	f77_cdiv,@function
f77_cdiv:                               # @f77_cdiv
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
	add r12, r5, r0
	add r13, r4, r0
	add r11, r3, r0
	fcvt.d.s r14, r6
	fcvt.d.s r16, r7
	add r3, r14, r0
	add r4, r15, r0
	jal r31, fabs
	add r18, r1, r0
	add r19, r2, r0
	add r3, r16, r0
	add r4, r17, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	flt.d r1, r4, r18
	xor r3, r15, r17
	sub r1, r0, r1
	and r3, r3, r1
	xor r4, r17, r3
	xor r3, r14, r16
	and r1, r3, r1
	xor r3, r16, r1
	jal r31, fabs
	add r6, r1, r0
	add r7, r2, r0
	lui r1, %hi(.LCPI20_0)
	addi r1, r1, %lo(.LCPI20_0)
	ldw r9, r1+4
	ldw r8, r1+0
	flt.d r1, r8, r6
	xori r10, r1, 1
	lui r1, %hi(.LCPI20_1)
	addi r1, r1, %lo(.LCPI20_1)
	ldw r5, r1+4
	ldw r4, r1+0
	addi r1, r0, 0
	lui r3, %hi(.LCPI20_2)
	addi r3, r3, %lo(.LCPI20_2)
	bne r10, r1, .LBB20_2
.LBB20_1:
	ldw r19, r3+4
	ldw r18, r3+0
	fmul.d r14, r14, r18
	fmul.d r16, r16, r18
	fmul.d r6, r6, r18
	fmul.d r4, r4, r18
	flt.d r10, r8, r6
	bne r10, r1, .LBB20_1
.LBB20_2:
	lui r10, %hi(.LCPI20_3)
	addi r10, r10, %lo(.LCPI20_3)
	ldw r19, r10+4
	ldw r18, r10+0
	flt.d r10, r18, r6
	xori r10, r10, 1
	bne r10, r1, .LBB20_6
.LBB20_3:
	ldw r21, r3+4
	ldw r20, r3+0
	flt.d r3, r6, r20
	xori r3, r3, 1
	bne r3, r1, .LBB20_6
.LBB20_4:
	fmul.d r14, r14, r8
	fmul.d r16, r16, r8
	fmul.d r6, r6, r8
	fmul.d r4, r4, r8
	fle.d r3, r6, r18
	bne r3, r1, .LBB20_6
.LBB20_5:
	flt.d r3, r6, r20
	bne r3, r1, .LBB20_4
.LBB20_6:
	fmul.d r6, r16, r16
	fmul.d r8, r14, r14
	fadd.d r6, r8, r6
	fcvt.d.s r8, r13
	fcvt.d.s r12, r12
	fmul.d r18, r16, r12
	fmul.d r20, r8, r14
	fadd.d r18, r20, r18
	fdiv.d r18, r18, r6
	fmul.d r18, r4, r18
	fcvt.s.d r1, r18
	stw r11+0, r1
	fmul.d r8, r16, r8
	fmul.d r12, r12, r14
	fsub.d r8, r12, r8
	fdiv.d r6, r8, r6
	fmul.d r4, r4, r6
	fcvt.s.d r1, r4
	stw r11+4, r1
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
.Lfunc_end20:
	.size	f77_cdiv, .Lfunc_end20-f77_cdiv
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpow_i
	.type	.LCPI21_0,@object
.LCPI21_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI21_0, 8
	.type	.LCPI21_1,@object
.LCPI21_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI21_1, 8
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
	lui r1, %hi(.LCPI21_0)
	addi r1, r1, %lo(.LCPI21_0)
	ldw r9, r1+4
	ldw r8, r1+0
	lui r1, %hi(.LCPI21_1)
	addi r1, r1, %lo(.LCPI21_1)
	ldw r11, r1+4
	ldw r10, r1+0
	addi r1, r0, 0
	beq r6, r1, .LBB21_7
.LBB21_1:
	srai r7, r6, 31
	xor lr, r6, r7
	sub r7, lr, r7
	fcvt.d.s r12, r5
	fcvt.d.s r4, r4
	jal r0, .LBB21_3
.LBB21_2:
	beq r7, r1, .LBB21_7
.LBB21_3:
	andi lr, r7, 1
	beq lr, r1, .LBB21_5
.LBB21_4:
	fmul.d r14, r10, r12
	fmul.d r16, r8, r4
	fsub.d r14, r16, r14
	fmul.d r10, r4, r10
	fmul.d r8, r8, r12
	fadd.d r10, r8, r10
	add r8, r14, r0
	add r9, r15, r0
.LBB21_5:
	srli r7, r7, 1
	beq r7, r1, .LBB21_2
.LBB21_6:
	fmul.d r14, r12, r12
	fmul.d r16, r4, r4
	fsub.d r14, r16, r14
	fadd.d r4, r4, r4
	fmul.d r12, r12, r4
	add r4, r14, r0
	add r5, r15, r0
	jal r0, .LBB21_2
.LBB21_7:
	addi r1, r0, -1
	bgt r6, r1, .LBB21_9
.LBB21_8:
	fmul.d r4, r10, r10
	fmul.d r6, r8, r8
	fadd.d r4, r6, r4
	fdiv.d r8, r8, r4
	fneg.d r6, r10
	fdiv.d r10, r6, r4
.LBB21_9:
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
.Lfunc_end21:
	.size	f77_cpow_i, .Lfunc_end21-f77_cpow_i
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
.Lfunc_end22:
	.size	f77_clog, .Lfunc_end22-f77_clog
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
.Lfunc_end23:
	.size	f77_cexp, .Lfunc_end23-f77_cexp
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_cpow_c
	.type	.LCPI24_0,@object
.LCPI24_0:
	.word	0x00000000                      # float 0
	.size	.LCPI24_0, 4
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
	lui r1, %hi(.LCPI24_0)
	addi r1, r1, %lo(.LCPI24_0)
	ldw r1, r1+0
	feq.s r3, r4, r1
	xori r6, r3, 1
	addi r3, r0, 0
	bne r6, r3, .LBB24_3
.LBB24_1:
	feq.s r6, r5, r1
	xori r6, r6, 1
	bne r6, r3, .LBB24_3
.LBB24_2:
	stw r11+0, r3
	jal r0, .LBB24_4
.LBB24_3:
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
.LBB24_4:
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
.Lfunc_end24:
	.size	f77_cpow_c, .Lfunc_end24-f77_cpow_c
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_csqrt
	.type	.LCPI25_0,@object
.LCPI25_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI25_0, 8
	.type	.LCPI25_2,@object
.LCPI25_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI25_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI25_1,@object
.LCPI25_1:
	.word	0x00000000                      # float 0
	.size	.LCPI25_1, 4
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
	lui r1, %hi(.LCPI25_0)
	addi r1, r1, %lo(.LCPI25_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r16, r1, r0
	add r17, r2, r0
	lui r4, %hi(.LCPI25_1)
	addi r4, r4, %lo(.LCPI25_1)
	ldw r1, r4+0
	fle.s r3, r1, r13
	xori r5, r3, 1
	addi r3, r0, 0
	beq r5, r3, .LBB25_4
.LBB25_1:
	lui r1, %hi(.LCPI25_2)
	addi r1, r1, %lo(.LCPI25_2)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r5, r16, r6
	ldw r13, r4+0
	add r1, r13, r0
	bne r5, r3, .LBB25_3
.LBB25_2:
	add r3, r14, r0
	add r4, r15, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r16, r16
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
.LBB25_3:
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
	jal r0, .LBB25_6
.LBB25_4:
	fcvt.s.d r4, r16
	stw r11+0, r4
	lui r4, %hi(.LCPI25_2)
	addi r4, r4, %lo(.LCPI25_2)
	ldw r7, r4+4
	ldw r6, r4+0
	feq.d r4, r16, r6
	bne r4, r3, .LBB25_6
.LBB25_5:
	fadd.d r4, r16, r16
	fdiv.d r4, r14, r4
	fcvt.s.d r1, r4
.LBB25_6:
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
.Lfunc_end25:
	.size	f77_csqrt, .Lfunc_end25-f77_csqrt
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
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	stw sp+20, r17
	add r12, r5, r0
	add r11, r3, r0
	fcvt.d.s r14, r4
	add r3, r14, r0
	add r4, r15, r0
	jal r31, sin
	add r16, r1, r0
	add r17, r2, r0
	fcvt.d.s r12, r12
	add r3, r12, r0
	add r4, r13, r0
	jal r31, cosh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r16, r4
	fcvt.s.d r1, r4
	stw r11+0, r1
	add r3, r14, r0
	add r4, r15, r0
	jal r31, cos
	add r14, r1, r0
	add r15, r2, r0
	add r3, r12, r0
	add r4, r13, r0
	jal r31, sinh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r14, r4
	fcvt.s.d r1, r4
	stw r11+4, r1
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
.Lfunc_end26:
	.size	f77_csin, .Lfunc_end26-f77_csin
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
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	stw sp+20, r17
	add r12, r5, r0
	add r11, r3, r0
	fcvt.d.s r14, r4
	add r3, r14, r0
	add r4, r15, r0
	jal r31, cos
	add r16, r1, r0
	add r17, r2, r0
	fcvt.d.s r12, r12
	add r3, r12, r0
	add r4, r13, r0
	jal r31, cosh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r16, r4
	fcvt.s.d r1, r4
	stw r11+0, r1
	add r3, r14, r0
	add r4, r15, r0
	jal r31, sin
	add r4, r1, r0
	add r5, r2, r0
	fneg.d r14, r4
	add r3, r12, r0
	add r4, r13, r0
	jal r31, sinh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r4, r14
	fcvt.s.d r1, r4
	stw r11+4, r1
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
.Lfunc_end27:
	.size	f77_ccos, .Lfunc_end27-f77_ccos
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
.Lfunc_end28:
	.size	f77i_iabs, .Lfunc_end28-f77i_iabs
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_abs
	.type	.LCPI29_0,@object
.LCPI29_0:
	.word	0x00000000                      # float 0
	.size	.LCPI29_0, 4
	.text
	.globl	f77i_abs
	.p2align	2
	.type	f77i_abs,@function
f77i_abs:                               # @f77i_abs
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI29_0)
	addi r3, r3, %lo(.LCPI29_0)
	ldw r3, r3+0
	flt.s r3, r1, r3
	sub r3, r0, r3
	lui r4, 524288
	and r3, r3, r4
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end29:
	.size	f77i_abs, .Lfunc_end29-f77i_abs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dabs
	.type	.LCPI30_0,@object
.LCPI30_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI30_0, 8
	.text
	.globl	f77i_dabs
	.p2align	2
	.type	f77i_dabs,@function
f77i_dabs:                              # @f77i_dabs
# %bb.0:
	ldw r5, r3+4
	ldw r4, r3+0
	lui r1, %hi(.LCPI30_0)
	addi r1, r1, %lo(.LCPI30_0)
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
.Lfunc_end30:
	.size	f77i_dabs, .Lfunc_end30-f77i_dabs
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
.Lfunc_end31:
	.size	f77i_sqrt, .Lfunc_end31-f77i_sqrt
                                        # -- End function
	.globl	f77i_dsqrt                      # -- Begin function f77i_dsqrt
	.p2align	2
	.type	f77i_dsqrt,@function
f77i_dsqrt:                             # @f77i_dsqrt
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sqrt
.Lfunc_end32:
	.size	f77i_dsqrt, .Lfunc_end32-f77i_dsqrt
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
.Lfunc_end33:
	.size	f77i_exp, .Lfunc_end33-f77i_exp
                                        # -- End function
	.globl	f77i_dexp                       # -- Begin function f77i_dexp
	.p2align	2
	.type	f77i_dexp,@function
f77i_dexp:                              # @f77i_dexp
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, exp
.Lfunc_end34:
	.size	f77i_dexp, .Lfunc_end34-f77i_dexp
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
.Lfunc_end35:
	.size	f77i_alog, .Lfunc_end35-f77i_alog
                                        # -- End function
	.globl	f77i_dlog                       # -- Begin function f77i_dlog
	.p2align	2
	.type	f77i_dlog,@function
f77i_dlog:                              # @f77i_dlog
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log
.Lfunc_end36:
	.size	f77i_dlog, .Lfunc_end36-f77i_dlog
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
.Lfunc_end37:
	.size	f77i_alog10, .Lfunc_end37-f77i_alog10
                                        # -- End function
	.globl	f77i_dlog10                     # -- Begin function f77i_dlog10
	.p2align	2
	.type	f77i_dlog10,@function
f77i_dlog10:                            # @f77i_dlog10
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log10
.Lfunc_end38:
	.size	f77i_dlog10, .Lfunc_end38-f77i_dlog10
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
.Lfunc_end39:
	.size	f77i_sin, .Lfunc_end39-f77i_sin
                                        # -- End function
	.globl	f77i_dsin                       # -- Begin function f77i_dsin
	.p2align	2
	.type	f77i_dsin,@function
f77i_dsin:                              # @f77i_dsin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sin
.Lfunc_end40:
	.size	f77i_dsin, .Lfunc_end40-f77i_dsin
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
.Lfunc_end41:
	.size	f77i_cos, .Lfunc_end41-f77i_cos
                                        # -- End function
	.globl	f77i_dcos                       # -- Begin function f77i_dcos
	.p2align	2
	.type	f77i_dcos,@function
f77i_dcos:                              # @f77i_dcos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cos
.Lfunc_end42:
	.size	f77i_dcos, .Lfunc_end42-f77i_dcos
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
.Lfunc_end43:
	.size	f77i_tan, .Lfunc_end43-f77i_tan
                                        # -- End function
	.globl	f77i_dtan                       # -- Begin function f77i_dtan
	.p2align	2
	.type	f77i_dtan,@function
f77i_dtan:                              # @f77i_dtan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tan
.Lfunc_end44:
	.size	f77i_dtan, .Lfunc_end44-f77i_dtan
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
.Lfunc_end45:
	.size	f77i_asin, .Lfunc_end45-f77i_asin
                                        # -- End function
	.globl	f77i_dasin                      # -- Begin function f77i_dasin
	.p2align	2
	.type	f77i_dasin,@function
f77i_dasin:                             # @f77i_dasin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, asin
.Lfunc_end46:
	.size	f77i_dasin, .Lfunc_end46-f77i_dasin
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
.Lfunc_end47:
	.size	f77i_acos, .Lfunc_end47-f77i_acos
                                        # -- End function
	.globl	f77i_dacos                      # -- Begin function f77i_dacos
	.p2align	2
	.type	f77i_dacos,@function
f77i_dacos:                             # @f77i_dacos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, acos
.Lfunc_end48:
	.size	f77i_dacos, .Lfunc_end48-f77i_dacos
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
.Lfunc_end49:
	.size	f77i_atan, .Lfunc_end49-f77i_atan
                                        # -- End function
	.globl	f77i_datan                      # -- Begin function f77i_datan
	.p2align	2
	.type	f77i_datan,@function
f77i_datan:                             # @f77i_datan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, atan
.Lfunc_end50:
	.size	f77i_datan, .Lfunc_end50-f77i_datan
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
.Lfunc_end51:
	.size	f77i_atan2, .Lfunc_end51-f77i_atan2
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
.Lfunc_end52:
	.size	f77i_datan2, .Lfunc_end52-f77i_datan2
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
.Lfunc_end53:
	.size	f77i_sinh, .Lfunc_end53-f77i_sinh
                                        # -- End function
	.globl	f77i_dsinh                      # -- Begin function f77i_dsinh
	.p2align	2
	.type	f77i_dsinh,@function
f77i_dsinh:                             # @f77i_dsinh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sinh
.Lfunc_end54:
	.size	f77i_dsinh, .Lfunc_end54-f77i_dsinh
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
.Lfunc_end55:
	.size	f77i_cosh, .Lfunc_end55-f77i_cosh
                                        # -- End function
	.globl	f77i_dcosh                      # -- Begin function f77i_dcosh
	.p2align	2
	.type	f77i_dcosh,@function
f77i_dcosh:                             # @f77i_dcosh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cosh
.Lfunc_end56:
	.size	f77i_dcosh, .Lfunc_end56-f77i_dcosh
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
.Lfunc_end57:
	.size	f77i_tanh, .Lfunc_end57-f77i_tanh
                                        # -- End function
	.globl	f77i_dtanh                      # -- Begin function f77i_dtanh
	.p2align	2
	.type	f77i_dtanh,@function
f77i_dtanh:                             # @f77i_dtanh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tanh
.Lfunc_end58:
	.size	f77i_dtanh, .Lfunc_end58-f77i_dtanh
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
.Lfunc_end59:
	.size	f77i_aint, .Lfunc_end59-f77i_aint
                                        # -- End function
	.globl	f77i_dint                       # -- Begin function f77i_dint
	.p2align	2
	.type	f77i_dint,@function
f77i_dint:                              # @f77i_dint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, trunc
.Lfunc_end60:
	.size	f77i_dint, .Lfunc_end60-f77i_dint
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
.Lfunc_end61:
	.size	f77i_anint, .Lfunc_end61-f77i_anint
                                        # -- End function
	.globl	f77i_dnint                      # -- Begin function f77i_dnint
	.p2align	2
	.type	f77i_dnint,@function
f77i_dnint:                             # @f77i_dnint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, round
.Lfunc_end62:
	.size	f77i_dnint, .Lfunc_end62-f77i_dnint
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
.Lfunc_end63:
	.size	f77i_nint, .Lfunc_end63-f77i_nint
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
.Lfunc_end64:
	.size	f77i_idnint, .Lfunc_end64-f77i_idnint
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
.Lfunc_end65:
	.size	f77i_mod, .Lfunc_end65-f77i_mod
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
.Lfunc_end66:
	.size	f77i_amod, .Lfunc_end66-f77i_amod
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
.Lfunc_end67:
	.size	f77i_dmod, .Lfunc_end67-f77i_dmod
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
.Lfunc_end68:
	.size	f77i_isign, .Lfunc_end68-f77i_isign
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_sign
	.type	.LCPI69_0,@object
.LCPI69_0:
	.word	0x00000000                      # float 0
	.size	.LCPI69_0, 4
	.text
	.globl	f77i_sign
	.p2align	2
	.type	f77i_sign,@function
f77i_sign:                              # @f77i_sign
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI69_0)
	addi r3, r3, %lo(.LCPI69_0)
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
.Lfunc_end69:
	.size	f77i_sign, .Lfunc_end69-f77i_sign
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dsign
	.type	.LCPI70_0,@object
.LCPI70_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI70_0, 8
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
	ldw r11, r3+4
	ldw r10, r3+0
	lui r1, %hi(.LCPI70_0)
	addi r1, r1, %lo(.LCPI70_0)
	ldw r7, r1+4
	ldw r6, r1+0
	flt.d r1, r10, r6
	fneg.d r12, r10
	xor r3, r13, r11
	sub r1, r0, r1
	and r3, r3, r1
	xor r9, r11, r3
	xor r3, r12, r10
	and r1, r3, r1
	xor r8, r10, r1
	ldw r11, r4+4
	ldw r10, r4+0
	flt.d r1, r10, r6
	fneg.d r4, r8
	xor r3, r5, r9
	sub r1, r0, r1
	and r3, r3, r1
	xor r2, r9, r3
	xor r3, r4, r8
	and r1, r3, r1
	xor r1, r8, r1
	ldw r13, sp+4
	ldw r12, sp+8
	ldw r11, sp+12
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end70:
	.size	f77i_dsign, .Lfunc_end70-f77i_dsign
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
.Lfunc_end71:
	.size	f77i_idim, .Lfunc_end71-f77i_idim
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
.Lfunc_end72:
	.size	f77i_dim, .Lfunc_end72-f77i_dim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_ddim
	.type	.LCPI73_0,@object
.LCPI73_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI73_0, 8
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
	lui r3, %hi(.LCPI73_0)
	addi r3, r3, %lo(.LCPI73_0)
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
.Lfunc_end73:
	.size	f77i_ddim, .Lfunc_end73-f77i_ddim
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
.Lfunc_end74:
	.size	f77i_dprod, .Lfunc_end74-f77i_dprod
                                        # -- End function
	.globl	f77i_len                        # -- Begin function f77i_len
	.p2align	2
	.type	f77i_len,@function
f77i_len:                               # @f77i_len
# %bb.0:
	add r1, r4, r0
	jalr r0, r31, 0
.Lfunc_end75:
	.size	f77i_len, .Lfunc_end75-f77i_len
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
	beq r6, r11, .LBB76_5
.LBB76_1:
	add r13, r6, r0
	bgt r6, r12, .LBB76_7
.LBB76_2:
	add r14, r4, r0
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
	.size	f77i_index, .Lfunc_end76-f77i_index
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
	beq r6, r11, .LBB77_5
.LBB77_1:
	add r13, r6, r0
	bgt r6, r12, .LBB77_7
.LBB77_2:
	add r14, r5, r0
	add r15, r3, r0
	addi r11, r0, 0
	add r16, r11, r0
.LBB77_3:
	add r3, r15, r16
	add r4, r14, r0
	add r5, r13, r0
	jal r31, memcmp
	beq r1, r11, .LBB77_6
.LBB77_4:
	addi r16, r16, 1
	add r1, r13, r16
	ble r1, r12, .LBB77_3
	jal r0, .LBB77_7
.LBB77_5:
	addi r1, r0, 0
	sgt r11, r12, r1
	jal r0, .LBB77_7
.LBB77_6:
	addi r11, r16, 1
.LBB77_7:
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
.Lfunc_end77:
	.size	f77_index, .Lfunc_end77-f77_index
                                        # -- End function
	.globl	f77i_aimag                      # -- Begin function f77i_aimag
	.p2align	2
	.type	f77i_aimag,@function
f77i_aimag:                             # @f77i_aimag
# %bb.0:
	ldw r1, r3+4
	jalr r0, r31, 0
.Lfunc_end78:
	.size	f77i_aimag, .Lfunc_end78-f77i_aimag
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
.Lfunc_end79:
	.size	f77i_cabs, .Lfunc_end79-f77i_cabs
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
	blt r14, r1, .LBB80_2
.LBB80_1:
	add r3, r12, r0
	add r4, r5, r0
	add r5, r14, r0
	jal r31, memmove
.LBB80_2:
	bge r13, r11, .LBB80_5
.LBB80_3:
	add r1, r12, r14
	sub r3, r11, r14
	addi r4, r0, 32
	addi r5, r0, 0
.LBB80_4:
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB80_4
.LBB80_5:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end80:
	.size	f77_strcpy, .Lfunc_end80-f77_strcpy
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
	blt r7, r1, .LBB81_9
.LBB81_1:
	addi r8, r0, 0
	addi r9, r0, 32
	add r10, r8, r0
                                        # implicit-def: $r1
.LBB81_2:
	add lr, r9, r0
	bge r10, r4, .LBB81_4
.LBB81_3:
	add lr, r3, r10
	ldbu lr, lr+0
.LBB81_4:
	add r11, r9, r0
	bge r10, r6, .LBB81_6
.LBB81_5:
	add r11, r5, r10
	ldbu r11, r11+0
.LBB81_6:
	seq r12, lr, r11
	sltu r13, lr, r11
	sub r13, r0, r13
	ori  r13, r13, 1
	xor r1, r1, r13
	sub r12, r0, r12
	and r1, r1, r12
	xor r1, r13, r1
	bne lr, r11, .LBB81_10
.LBB81_7:
	addi r10, r10, 1
	bne r7, r10, .LBB81_2
.LBB81_8:
	add r1, r8, r0
	jal r0, .LBB81_10
.LBB81_9:
	addi r1, r0, 0
.LBB81_10:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end81:
	.size	f77_strcmp, .Lfunc_end81-f77_strcmp
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
	add r11, r6, r0
	add r12, r5, r0
	add r1, r4, r0
	add r4, r3, r0
	addi r3, r0, 0
	sgt r5, r1, r3
	sub r5, r0, r5
	and r14, r1, r5
	sgt r5, r6, r3
	sub r5, r0, r5
	and r5, r6, r5
	add r5, r5, r14
	lui r6, 16
	addi r7, r6, 1
	bgeu r5, r7, .LBB82_8
.LBB82_1:
	lui r6, %hi(fio_catpos)
	addi r6, r6, %lo(fio_catpos)
	ldw r8, r6+0
	add r8, r8, r5
	blt r8, r7, .LBB82_3
.LBB82_2:
	stw r6+0, r3
.LBB82_3:
	ldw r3, r6+0
	lui r7, %hi(fio_catring)
	addi r7, r7, %lo(fio_catring)
	add r13, r3, r7
	addi r5, r5, 3
	lui r7, 64
	addi r7, r7, -4
	and r5, r5, r7
	add r3, r3, r5
	stw r6+0, r3
	addi r15, r0, 1
	blt r1, r15, .LBB82_5
.LBB82_4:
	add r3, r13, r0
	add r5, r1, r0
	jal r31, memmove
.LBB82_5:
	blt r11, r15, .LBB82_7
.LBB82_6:
	add r3, r13, r14
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memmove
.LBB82_7:
	add r1, r13, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB82_8:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end82:
	.size	f77_cat, .Lfunc_end82-f77_cat
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
	beq r1, r11, .LBB83_15
.LBB83_1:
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r7, r4+0
	addi r6, r0, 1022
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	bgt r7, r6, .LBB83_7
.LBB83_2:
	ldw r9, r1+0
	addi r8, r0, 32
	bge r9, r7, .LBB83_5
.LBB83_3:
	addi r10, r9, 1
	add r9, r9, r5
	stb r9+0, r8
	add r9, r10, r0
	bne r7, r10, .LBB83_3
.LBB83_4:
	stw r1+0, r7
	add r9, r7, r0
.LBB83_5:
	add r10, r7, r5
	stb r10+0, r8
	addi r8, r7, 1
	stw r4+0, r8
	blt r7, r9, .LBB83_7
.LBB83_6:
	stw r1+0, r8
.LBB83_7:
	ldw r7, r4+0
	bgt r7, r6, .LBB83_14
.LBB83_8:
	ldw r6, r1+0
	bge r6, r7, .LBB83_12
.LBB83_9:
	addi r8, r0, 32
.LBB83_10:
	addi r9, r6, 1
	add r6, r6, r5
	stb r6+0, r8
	add r6, r9, r0
	bne r7, r9, .LBB83_10
.LBB83_11:
	stw r1+0, r7
	add r6, r7, r0
.LBB83_12:
	seq r3, r3, r11
	sub r3, r0, r3
	andi r3, r3, 18
	xori r3, r3, 84
	add r5, r7, r5
	stb r5+0, r3
	addi r3, r7, 1
	stw r4+0, r3
	blt r7, r6, .LBB83_14
.LBB83_13:
	stw r1+0, r3
.LBB83_14:
	lui r1, %hi(fio_first_item)
	addi r1, r1, %lo(fio_first_item)
	stb r1+0, r11
	lui r1, %hi(fio_prev_char)
	addi r1, r1, %lo(fio_prev_char)
	stb r1+0, r11
	jal r0, .LBB83_31
.LBB83_15:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	beq r1, r11, .LBB83_17
.LBB83_16:
	ldw r1, r13+0
	bgt r1, r11, .LBB83_18
.LBB83_17:
	stw r12+0, r11
	add r14, r3, r0
	jal r31, fio_next_desc
	add r3, r14, r0
	beq r1, r11, .LBB83_31
.LBB83_18:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r4, r0, 2
	blt r1, r4, .LBB83_20
.LBB83_19:
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r5, r4+0
	add r1, r1, r5
	addi r1, r1, -1
	stw r4+0, r1
.LBB83_20:
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r1, r4+0
	addi r5, r0, 1022
	bgt r1, r5, .LBB83_27
.LBB83_21:
	lui r5, %hi(fio_len)
	addi r5, r5, %lo(fio_len)
	ldw r6, r5+0
	bge r6, r1, .LBB83_25
.LBB83_22:
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
.LBB83_23:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r1, r9, .LBB83_23
.LBB83_24:
	stw r5+0, r1
	add r6, r1, r0
.LBB83_25:
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
	blt r1, r6, .LBB83_27
.LBB83_26:
	stw r5+0, r3
.LBB83_27:
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB83_29
.LBB83_28:
	addi r1, r1, -1
	stw r13+0, r1
.LBB83_29:
	ldw r1, r13+0
	bne r1, r11, .LBB83_31
.LBB83_30:
	stw r12+0, r11
.LBB83_31:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end83:
	.size	f77_wr_l, .Lfunc_end83-f77_wr_l
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
	bne r1, r11, .LBB84_4
.LBB84_1:
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	ldw r4, r1+0
	lui r3, %hi(fio_rep)
	addi r3, r3, %lo(fio_rep)
	beq r4, r11, .LBB84_3
.LBB84_2:
	ldw r4, r3+0
	bgt r4, r11, .LBB84_4
.LBB84_3:
	lui r12, %hi(fio_draining)
	addi r12, r12, %lo(fio_draining)
	addi r4, r0, 1
	stb r12+0, r4
	stw r1+0, r11
	stw r3+0, r11
	jal r31, fio_next_desc
	stb r12+0, r11
.LBB84_4:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r11
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end84:
	.size	f77_wr_end, .Lfunc_end84-f77_wr_end
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
	addi r27, r0, 1
	lui r16, %hi(fio_fmt)
	addi r16, r16, %lo(fio_fmt)
	lui r13, %hi(fio_pos)
	addi r13, r13, %lo(fio_pos)
	addi r17, r0, 44
	addi r14, r0, 0
	lui r1, 24
	addi r18, r1, 1695
	addi r19, r0, 32
	lui r20, %hi(fio_gdepth)
	addi r20, r20, %lo(fio_gdepth)
	lui r21, %hi(fio_draining)
	addi r21, r21, %lo(fio_draining)
	lui r22, %hi(fio_revert)
	addi r22, r22, %lo(fio_revert)
	lui r12, %hi(fio_since_data)
	addi r12, r12, %lo(fio_since_data)
	lui r23, %hi(fio_reading)
	addi r23, r23, %lo(fio_reading)
	addi r24, r0, -33
	addi r25, r0, 83
	addi r1, r0, 35
	stw sp+52, r1
	lui r1, %hi(fio_sp)
	addi r1, r1, %lo(fio_sp)
	stw sp+48, r1
	lui r1, %hi(.LJTI85_4)
	addi r1, r1, %lo(.LJTI85_4)
	stw sp+44, r1
	addi fp, r0, 58
	addi r1, r0, 66
	stw sp+40, r1
	addi r1, r0, 11
	stw sp+32, r1
	addi r1, r0, 90
	stw sp+28, r1
	lui r1, %hi(fio_bz)
	addi r1, r1, %lo(fio_bz)
	stw sp+24, r1
	addi r1, r0, 8
	stw sp+36, r1
	add r15, r27, r0
	jal r0, .LBB85_5
.LBB85_1:
	addi r1, r3, 1
.LBB85_2:
	stw r13+0, r1
.LBB85_3:
	bgtu r15, r18, .LBB85_110
.LBB85_4:
	addi r15, r15, 1
.LBB85_5:
	ldw r26, r16+0
	ldw r3, r13+0
	add r6, r26, r3
	ldb r5, r6+0
	andi r1, r5, 255
	beq r1, r17, .LBB85_1
.LBB85_6:
	beq r1, r19, .LBB85_1
.LBB85_7:
	bne r1, r14, .LBB85_16
.LBB85_8:
	ldw r1, r20+0
	blt r1, r27, .LBB85_10
.LBB85_9:
	stw r20+0, r14
.LBB85_10:
	ldbu r1, r21+0
	bne r1, r14, .LBB85_110
.LBB85_11:
	ldw r1, r22+0
	blt r1, r14, .LBB85_110
.LBB85_12:
	ldbu r3, r12+0
	beq r3, r14, .LBB85_121
.LBB85_13:
	stb r12+0, r14
	stw r13+0, r1
	add r1, r26, r1
	ldbu r1, r1+0
	beq r1, r14, .LBB85_110
.LBB85_14:
	ldbu r1, r23+0
	bne r1, r27, .LBB85_89
.LBB85_15:
	jal r31, fio_getrec
	beq r1, r14, .LBB85_3
	jal r0, .LBB85_122
.LBB85_16:
	and r4, r5, r24
	bne r4, r25, .LBB85_20
.LBB85_17:
	addi r1, r3, 1
	stw r13+0, r1
	add r1, r26, r1
	ldbu r1, r1+0
	addi r1, r1, -80
	ldw r4, sp+52
	bgtu r1, r4, .LBB85_50
.LBB85_18:
	slli r1, r1, 2
	ldw r4, sp+44
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB85_19:
	ldw r1, sp+48
	stb r1+0, r27
	jal r0, .LBB85_28
.LBB85_20:
	bne r5, fp, .LBB85_23
.LBB85_21:
	addi r1, r3, 1
	stw r13+0, r1
	ldbu r1, r21+0
	bne r1, r14, .LBB85_110
.LBB85_22:
	bleu r15, r18, .LBB85_4
	jal r0, .LBB85_110
.LBB85_23:
	andi r4, r5, 223
	ldw r7, sp+40
	bne r4, r7, .LBB85_29
.LBB85_24:
	ldbu r4, r6+1
	addi r7, r4, -78
	andi r8, r7, 252
	slli r7, r7, 6
	srli r8, r8, 2
	or  r7, r7, r8
	andi r7, r7, 255
	ldw r8, sp+32
	bgtu r7, r8, .LBB85_29
.LBB85_25:
	sll r7, r27, r7
	andi r7, r7, 2313
	beq r7, r14, .LBB85_29
.LBB85_26:
	andi r1, r4, 223
	ldw r4, sp+28
	seq r1, r1, r4
	ldw r4, sp+24
	stw r4+0, r1
	jal r0, .LBB85_28
.LBB85_27:
	ldw r1, sp+48
	stb r1+0, r14
.LBB85_28:
	addi r1, r3, 2
	jal r0, .LBB85_2
.LBB85_29:
	addi r4, r1, -39
	lui r1, %hi(fio_line)
	addi r1, r1, %lo(fio_line)
	ldw r7, sp+36
	bgtu r4, r7, .LBB85_53
.LBB85_30:
	slli r4, r4, 2
	lui r7, %hi(.LJTI85_0)
	addi r7, r7, %lo(.LJTI85_0)
	add r4, r7, r4
	ldw r7, r4+0
	add r4, r27, r0
	jalr r0, r7, 0
.LBB85_31:
	addi r3, r3, 1
	stw r13+0, r3
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
	ldw r9, r4+0
	lui r5, %hi(fio_len)
	addi r5, r5, %lo(fio_len)
	ldw r7, r5+0
	ldbu r6, r23+0
	jal r0, .LBB85_35
.LBB85_32:
	addi r10, r0, 1
.LBB85_33:
	add r9, r8, r0
.LBB85_34:
	add r3, r3, r10
	stw r13+0, r3
.LBB85_35:
	add r8, r9, r0
	add r9, r26, r3
	ldbu r10, r9+0
	addi r9, r0, 39
	beq r10, r9, .LBB85_42
.LBB85_36:
	addi r9, r0, 0
	beq r10, r9, .LBB85_3
.LBB85_37:
	addi lr, r0, 1022
	sgt lr, r8, lr
	or  lr, r6, lr
	andi lr, lr, 1
	bne lr, r9, .LBB85_32
.LBB85_38:
	bge r7, r8, .LBB85_41
.LBB85_39:
	addi r9, r7, 1
	add r7, r7, r1
	stb r7+0, r19
	add r7, r9, r0
	bne r8, r9, .LBB85_39
.LBB85_40:
	stw r5+0, r8
	add r7, r8, r0
.LBB85_41:
	add r9, r8, r1
	stb r9+0, r10
	addi r9, r8, 1
	stw r4+0, r9
	addi r10, r0, 1
	blt r8, r7, .LBB85_34
	jal r0, .LBB85_49
.LBB85_42:
	addi r10, r3, 1
	add lr, r26, r10
	ldbu lr, lr+0
	bne lr, r9, .LBB85_82
.LBB85_43:
	addi r10, r0, 1022
	sgt r10, r8, r10
	or  r10, r6, r10
	andi r10, r10, 1
	addi lr, r0, 0
	beq r10, lr, .LBB85_45
.LBB85_44:
	addi r10, r0, 2
	jal r0, .LBB85_33
.LBB85_45:
	bge r7, r8, .LBB85_48
.LBB85_46:
	addi r10, r7, 1
	add r7, r7, r1
	stb r7+0, r19
	add r7, r10, r0
	bne r8, r10, .LBB85_46
.LBB85_47:
	stw r5+0, r8
	add r7, r8, r0
.LBB85_48:
	add r10, r8, r1
	stb r10+0, r9
	addi r9, r8, 1
	stw r4+0, r9
	addi r10, r0, 2
	blt r8, r7, .LBB85_34
.LBB85_49:
	stw r5+0, r9
	add r7, r9, r0
	jal r0, .LBB85_34
.LBB85_50:
	ldw r1, sp+48
	stb r1+0, r14
	jal r0, .LBB85_3
.LBB85_51:
	ldb r4, r6+1
	addi r4, r4, -58
	addi r5, r0, -10
	bgeu r4, r5, .LBB85_58
.LBB85_52:
	addi r5, r0, 45
.LBB85_53:
	add r4, r27, r0
	jal r0, .LBB85_59
.LBB85_54:
	ldw r4, r20+0
	addi r1, r0, 0
	beq r4, r1, .LBB85_83
.LBB85_55:
	addi r1, r3, 1
	stw r13+0, r1
	addi r1, r4, -1
	stw r20+0, r1
	slli r1, r1, 2
	lui r3, %hi(fio_gcount)
	addi r3, r3, %lo(fio_gcount)
	add r3, r1, r3
	ldw r5, r3+0
	addi r6, r5, -1
	stw r3+0, r6
	addi r3, r0, 2
	blt r5, r3, .LBB85_3
.LBB85_56:
	lui r3, %hi(fio_gstart)
	addi r3, r3, %lo(fio_gstart)
	add r1, r1, r3
	ldw r1, r1+0
	stw r13+0, r1
	stw r20+0, r4
	jal r0, .LBB85_3
.LBB85_57:
	addi r1, r3, 1
	stw r13+0, r1
	jal r0, .LBB85_87
.LBB85_58:
	addi r4, r3, 1
	stw r13+0, r4
	add r4, r26, r4
	ldb r5, r4+0
	addi r4, r0, 0
.LBB85_59:
	addi r5, r5, -58
	addi lr, r0, -10
	add r28, r27, r0
	bltu r5, lr, .LBB85_65
.LBB85_60:
	ldw r5, r13+0
	add r6, r26, r5
	ldb r6, r6+0
	addi r6, r6, -48
	addi r7, r0, 9
	bleu r6, r7, .LBB85_62
.LBB85_61:
	addi r28, r0, 0
	jal r0, .LBB85_65
.LBB85_62:
	addi r7, r26, 1
	addi r28, r0, 0
.LBB85_63:
	addi r9, r0, 10
	mul r10, r28, r9
	addi r8, r5, 1
	add r28, r10, r6
	add r5, r7, r5
	ldb r5, r5+0
	addi r6, r5, -48
	add r5, r8, r0
	bltu r6, r9, .LBB85_63
.LBB85_64:
	stw r13+0, r8
.LBB85_65:
	ldw r5, r13+0
	add r6, r26, r5
	ldbu r11, r6+0
	addi r6, r0, 40
	bne r11, r6, .LBB85_69
.LBB85_66:
	addi r4, r5, 1
	stw r13+0, r4
	ldw r1, r20+0
	addi r5, r0, 15
	bgt r1, r5, .LBB85_3
.LBB85_67:
	slli r5, r1, 2
	lui r6, %hi(fio_gstart)
	addi r6, r6, %lo(fio_gstart)
	add r6, r5, r6
	stw r6+0, r4
	lui r4, %hi(fio_gcount)
	addi r4, r4, %lo(fio_gcount)
	add r4, r5, r4
	stw r4+0, r28
	addi r4, r1, 1
	stw r20+0, r4
	addi r4, r0, 0
	bne r1, r4, .LBB85_3
.LBB85_68:
	stw r22+0, r3
	jal r0, .LBB85_3
.LBB85_69:
	addi r3, r5, 1
	andi r6, r11, 223
	addi r6, r6, -72
	addi r7, r0, 16
	bgtu r6, r7, .LBB85_102
.LBB85_70:
	slli r6, r6, 2
	lui r7, %hi(.LJTI85_1)
	addi r7, r7, %lo(.LJTI85_1)
	add r6, r7, r6
	ldw r6, r6+0
	jalr r0, r6, 0
.LBB85_71:
	addi r4, r0, 1
	blt r28, r4, .LBB85_98
.LBB85_72:
	lui r5, %hi(fio_len)
	addi r5, r5, %lo(fio_len)
	ldw r8, r5+0
	lui r6, %hi(fio_col)
	addi r6, r6, %lo(fio_col)
	ldw r9, r6+0
	ldbu r7, r23+0
	jal r0, .LBB85_75
.LBB85_73:
	add r9, lr, r0
.LBB85_74:
	addi r28, r10, -1
	addi r3, r3, 1
	ble r10, r4, .LBB85_98
.LBB85_75:
	add lr, r26, r3
	ldbu lr, lr+0
	addi r11, r0, 0
	beq lr, r11, .LBB85_98
.LBB85_76:
	add r10, r28, r0
	addi r28, r0, 1022
	sgt r28, r9, r28
	or  r28, r7, r28
	andi r28, r28, 1
	bne r28, r11, .LBB85_74
.LBB85_77:
	bge r8, r9, .LBB85_80
.LBB85_78:
	addi r11, r8, 1
	add r8, r8, r1
	stb r8+0, r19
	add r8, r11, r0
	bne r9, r11, .LBB85_78
.LBB85_79:
	stw r5+0, r9
	add r8, r9, r0
.LBB85_80:
	add r11, r9, r1
	stb r11+0, lr
	addi lr, r9, 1
	stw r6+0, lr
	blt r9, r8, .LBB85_73
.LBB85_81:
	stw r5+0, lr
	add r8, lr, r0
	jal r0, .LBB85_73
.LBB85_82:
	stw r13+0, r10
	jal r0, .LBB85_3
.LBB85_83:
	ldbu r3, r21+0
	bne r3, r1, .LBB85_111
.LBB85_84:
	ldw r4, r22+0
	addi r3, r0, 0
	blt r4, r3, .LBB85_111
.LBB85_85:
	ldbu r1, r12+0
	beq r1, r3, .LBB85_121
.LBB85_86:
	stb r12+0, r3
	stw r13+0, r4
	add r1, r26, r4
	ldbu r4, r1+0
	add r1, r3, r0
	beq r4, r3, .LBB85_111
.LBB85_87:
	ldbu r1, r23+0
	addi r3, r0, 1
	bne r1, r3, .LBB85_89
.LBB85_88:
	jal r31, fio_getrec
	addi r3, r0, 0
	beq r1, r3, .LBB85_3
	jal r0, .LBB85_122
.LBB85_89:
	jal r31, fio_flush_line
	jal r0, .LBB85_3
.LBB85_90:
	stw r13+0, r3
	add r1, r26, r3
	ldbu r1, r1+0
	addi r28, r0, 0
	addi r11, r0, 1
	addi r3, r1, -76
	addi r1, r0, 38
	bgtu r3, r1, .LBB85_109
.LBB85_91:
	addi r1, r0, -1
	slli r3, r3, 2
	lui r4, %hi(.LJTI85_2)
	addi r4, r4, %lo(.LJTI85_2)
	add r3, r4, r3
	ldw r3, r3+0
	add r6, r11, r0
	jalr r0, r3, 0
.LBB85_92:
	addi r1, r0, 1
.LBB85_93:
	addi r3, r5, 2
	stw r13+0, r3
	addi r6, r0, 0
	add r28, r1, r0
.LBB85_94:
	ldw r1, r13+0
	add r1, r26, r1
	ldb r1, r1+0
	addi r3, r1, -58
	add r1, r11, r0
	bltu r3, lr, .LBB85_96
.LBB85_95:
	add r26, r6, r0
	jal r31, fio_number
	add r6, r26, r0
.LBB85_96:
	ldbu r4, r23+0
	mul r3, r1, r28
	addi r1, r1, -1
	bne r4, r11, .LBB85_104
.LBB85_97:
	lui r4, %hi(fio_rpos)
	addi r4, r4, %lo(fio_rpos)
	jal r0, .LBB85_105
.LBB85_98:
	stw r13+0, r3
	jal r0, .LBB85_3
.LBB85_99:
	stw r13+0, r3
	sub r1, r0, r28
	xor r3, r28, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	lui r3, %hi(fio_scale)
	addi r3, r3, %lo(fio_scale)
	stw r3+0, r1
	jal r0, .LBB85_3
.LBB85_100:
	stw r13+0, r3
	ldbu r3, r23+0
	addi r1, r0, 1
	bne r3, r1, .LBB85_106
.LBB85_101:
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	jal r0, .LBB85_108
.LBB85_102:
	stw r13+0, r3
	addi r1, r11, -65
	addi r4, r0, 43
	bgtu r1, r4, .LBB85_3
.LBB85_103:
	slli r1, r1, 2
	lui r4, %hi(.LJTI85_3)
	addi r4, r4, %lo(.LJTI85_3)
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB85_104:
	lui r4, %hi(fio_col)
	addi r4, r4, %lo(fio_col)
.LBB85_105:
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
	jal r0, .LBB85_3
.LBB85_106:
	blt r28, r1, .LBB85_3
.LBB85_107:
	lui r1, %hi(fio_col)
	addi r1, r1, %lo(fio_col)
.LBB85_108:
	ldw r3, r1+0
	add r3, r3, r28
	stw r1+0, r3
	jal r0, .LBB85_3
.LBB85_109:
	add r6, r11, r0
	jal r0, .LBB85_94
.LBB85_110:
	add r1, r14, r0
.LBB85_111:
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
.LBB85_112:
	add r1, r26, r3
	ldb r1, r1+0
	addi r3, r1, -58
	addi r14, r0, 0
	add r1, r14, r0
	bltu r3, lr, .LBB85_114
.LBB85_113:
	add r15, lr, r0
	jal r31, fio_number
	add lr, r15, r0
.LBB85_114:
	lui r3, %hi(fio_w)
	addi r3, r3, %lo(fio_w)
	stw r3+0, r1
	lui r15, %hi(fio_d)
	addi r15, r15, %lo(fio_d)
	stw r15+0, r14
	ldw r1, r13+0
	add r3, r26, r1
	ldbu r3, r3+0
	addi r4, r0, 46
	bne r3, r4, .LBB85_116
.LBB85_115:
	addi r1, r1, 1
	stw r13+0, r1
	add r14, lr, r0
	jal r31, fio_number
	add lr, r14, r0
	stw r15+0, r1
.LBB85_116:
	lui r14, %hi(fio_e)
	addi r14, r14, %lo(fio_e)
	addi r1, r0, 0
	stw r14+0, r1
	ldw r1, r13+0
	add r3, r26, r1
	ldbu r3, r3+0
	ori  r3, r3, 32
	addi r4, r0, 101
	bne r3, r4, .LBB85_120
.LBB85_117:
	addi r3, r1, 1
	stw r13+0, r3
	add r3, r26, r3
	ldb r3, r3+0
	addi r3, r3, -58
	bgeu r3, lr, .LBB85_119
.LBB85_118:
	stw r13+0, r1
	jal r0, .LBB85_120
.LBB85_119:
	jal r31, fio_number
	stw r14+0, r1
.LBB85_120:
	lui r1, %hi(fio_rep)
	addi r1, r1, %lo(fio_rep)
	stw r1+0, r28
	add r3, r11, r0
	jal r31, toupper
	lui r3, %hi(fio_desc)
	addi r3, r3, %lo(fio_desc)
	stw r3+0, r1
	addi r3, r0, 1
	stb r12+0, r3
	jal r0, .LBB85_111
.LBB85_121:
	jal r31, fio_flush_line
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.86)
	addi r4, r4, %lo(.L.str.86)
	jal r0, .LBB85_123
.LBB85_122:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
.LBB85_123:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end85:
	.size	fio_next_desc, .Lfunc_end85-fio_next_desc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI85_0,@object
.LJTI85_0:
	.word	.LBB85_31
	.word	.LBB85_59
	.word	.LBB85_54
	.word	.LBB85_59
	.word	.LBB85_59
	.word	.LBB85_59
	.word	.LBB85_51
	.word	.LBB85_59
	.word	.LBB85_57
	.size	.LJTI85_0, 36
	.type	.LJTI85_1,@object
.LJTI85_1:
	.word	.LBB85_71
	.word	.LBB85_102
	.word	.LBB85_102
	.word	.LBB85_102
	.word	.LBB85_102
	.word	.LBB85_102
	.word	.LBB85_102
	.word	.LBB85_102
	.word	.LBB85_99
	.word	.LBB85_102
	.word	.LBB85_102
	.word	.LBB85_102
	.word	.LBB85_90
	.word	.LBB85_102
	.word	.LBB85_102
	.word	.LBB85_102
	.word	.LBB85_100
	.size	.LJTI85_1, 68
	.type	.LJTI85_2,@object
.LJTI85_2:
	.word	.LBB85_93
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_92
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_93
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_94
	.word	.LBB85_92
	.size	.LJTI85_2, 156
	.type	.LJTI85_3,@object
.LJTI85_3:
	.word	.LBB85_112
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_112
	.word	.LBB85_112
	.word	.LBB85_112
	.word	.LBB85_112
	.word	.LBB85_3
	.word	.LBB85_112
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_112
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_112
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_112
	.word	.LBB85_112
	.word	.LBB85_112
	.word	.LBB85_112
	.word	.LBB85_3
	.word	.LBB85_112
	.word	.LBB85_3
	.word	.LBB85_3
	.word	.LBB85_112
	.size	.LJTI85_3, 176
	.type	.LJTI85_4,@object
.LJTI85_4:
	.word	.LBB85_19
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_27
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_19
	.word	.LBB85_50
	.word	.LBB85_50
	.word	.LBB85_27
	.size	.LJTI85_4, 144
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
	lui r1, %hi(fio_out)
	addi r1, r1, %lo(fio_out)
	ldw r1, r1+0
	addi r13, r0, 0
	lui r3, %hi(stdout)
	addi r3, r3, %lo(stdout)
	ldw r4, r3+0
	lui r14, %hi(fio_len)
	addi r14, r14, %lo(fio_len)
	ldw r5, r14+0
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	add r3, r5, r3
	stb r3+0, r13
	lui r3, %hi(fio_col)
	addi r3, r3, %lo(fio_col)
	stw r3+0, r13
	lui r12, %hi(fio_ibuf)
	addi r12, r12, %lo(fio_ibuf)
	ldw r3, r12+0
	beq r3, r13, .LBB86_9
.LBB86_1:
	lui r11, %hi(fio_irec)
	addi r11, r11, %lo(fio_irec)
	ldw r1, r11+0
	lui r4, %hi(fio_inrec)
	addi r4, r4, %lo(fio_inrec)
	ldw r4, r4+0
	bge r1, r4, .LBB86_18
.LBB86_2:
	lui r15, %hi(fio_ilen)
	addi r15, r15, %lo(fio_ilen)
	ldw r6, r15+0
	bgt r5, r6, .LBB86_19
.LBB86_3:
	mul r1, r6, r1
	add r3, r3, r1
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	jal r31, memcpy
	ldw r3, r14+0
	ldw r1, r15+0
	ble r1, r3, .LBB86_6
.LBB86_4:
	ldw r4, r12+0
	ldw r5, r11+0
	mul r5, r5, r1
	sub r1, r1, r3
	add r3, r3, r5
	add r3, r4, r3
	addi r4, r0, 32
.LBB86_5:
	stb r3+0, r4
	addi r1, r1, -1
	addi r3, r3, 1
	bne r1, r13, .LBB86_5
.LBB86_6:
	ldw r1, r11+0
	addi r1, r1, 1
	stw r11+0, r1
.LBB86_7:
	stw r14+0, r13
.LBB86_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB86_9:
	seq r3, r1, r13
	xor r4, r4, r1
	sub r3, r0, r3
	and r3, r4, r3
	xor r11, r1, r3
	lui r12, %hi(fio_unit)
	addi r12, r12, %lo(fio_unit)
	ldw r1, r12+0
	addi r3, r1, -1
	addi r15, r0, 98
	bgtu r3, r15, .LBB86_15
.LBB86_10:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r13, .LBB86_15
.LBB86_11:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r12, r1+0
	bgt r5, r12, .LBB86_21
.LBB86_12:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	add r4, r11, r0
	jal r31, fputs
	ldw r1, r14+0
	ble r12, r1, .LBB86_7
.LBB86_13:
	sub r15, r12, r1
	addi r12, r0, 32
.LBB86_14:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fputc
	addi r15, r15, -1
	bne r15, r13, .LBB86_14
	jal r0, .LBB86_7
.LBB86_15:
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
	bgtu r3, r15, .LBB86_8
.LBB86_16:
	addi r1, r1, -7
	addi r3, r0, -3
	bgtu r1, r3, .LBB86_8
.LBB86_17:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r12+0
	slli r3, r3, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
	jal r0, .LBB86_8
.LBB86_18:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.87)
	addi r4, r4, %lo(.L.str.87)
	jal r0, .LBB86_20
.LBB86_19:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.88)
	addi r4, r4, %lo(.L.str.88)
.LBB86_20:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB86_21:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.89)
	addi r4, r4, %lo(.L.str.89)
	add r6, r12, r0
	jal r0, .LBB86_20
.Lfunc_end86:
	.size	fio_flush_line, .Lfunc_end86-fio_flush_line
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
	stw sp+20, r13
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r4, r1+0
	addi r3, r0, 0
	bne r4, r3, .LBB87_4
.LBB87_1:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
	addi r6, r0, -1
.LBB87_2:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r3, .LBB87_2
.LBB87_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB87_4:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_resolve
	add r13, r1, r0
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r1, r1+0
	beq r13, r1, .LBB87_8
.LBB87_5:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r1, r1+0
	beq r13, r1, .LBB87_8
.LBB87_6:
	add r3, r13, r0
	jal r31, fflush
	addi r1, r11, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB87_8
.LBB87_7:
	add r3, r13, r0
	jal r31, ftell
	slli r3, r11, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r4, r3, r4
	stw r4+0, r1
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r3, r1
	stw r1+0, r12
.LBB87_8:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end87:
	.size	f77_endfile, .Lfunc_end87-f77_endfile
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
.Lfunc_end88:
	.size	f77_pause, .Lfunc_end88-f77_pause
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
	stw sp+16, r14
	add r11, r5, r0
	add r12, r4, r0
	add r14, r3, r0
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	addi r13, r0, 0
	bne r3, r13, .LBB89_4
.LBB89_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB89_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r13, .LBB89_2
.LBB89_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB89_4:
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
	addi r4, r0, 98
	bgtu r3, r4, .LBB89_10
.LBB89_5:
	slli r13, r14, 2
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r13, r3
	addi r4, r0, 0
	stw r3+0, r4
	lui r5, %hi(fio_rec_next)
	addi r5, r5, %lo(fio_rec_next)
	ldw r3, r5+0
	stw r5+0, r4
	addi r5, r14, -100
	addi r6, r0, -99
	bltu r5, r6, .LBB89_9
.LBB89_6:
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r13, r5
	ldw r5, r5+0
	beq r5, r4, .LBB89_9
.LBB89_7:
	ble r3, r4, .LBB89_15
.LBB89_8:
	addi r3, r3, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r4, r13, r4
	ldw r4, r4+0
	mul r4, r4, r3
	addi r5, r0, 0
	add r3, r1, r0
	jal r31, fseek
.LBB89_9:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r13, r1
	ldw r13, r1+0
	jal r0, .LBB89_14
.LBB89_10:
	lui r4, %hi(fio_rec_next)
	addi r4, r4, %lo(fio_rec_next)
	ldw r3, r4+0
	stw r4+0, r13
	addi r4, r14, -100
	addi r5, r0, -99
	bltu r4, r5, .LBB89_14
.LBB89_11:
	slli r4, r14, 2
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r4, r5
	ldw r5, r5+0
	addi r13, r0, 0
	beq r5, r13, .LBB89_14
.LBB89_12:
	addi r13, r0, 0
	ble r3, r13, .LBB89_15
.LBB89_13:
	addi r3, r3, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r4, r4, r5
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r13, r0
	jal r31, fseek
.LBB89_14:
	lui r1, %hi(fio_bz)
	addi r1, r1, %lo(fio_bz)
	stw r1+0, r13
	add r3, r12, r0
	add r4, r11, r0
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fio_rd_setup
.LBB89_15:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end89:
	.size	f77_rd_begin, .Lfunc_end89-f77_rd_begin
                                        # -- End function
	.p2align	2                               # -- Begin function fio_rd_setup
	.type	fio_rd_setup,@function
fio_rd_setup:                           # @fio_rd_setup
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
	add r12, r4, r0
	lui r1, %hi(fio_unit)
	addi r1, r1, %lo(fio_unit)
	ldw r1, r1+0
	addi r11, r0, 0
	bgt r1, r11, .LBB90_2
.LBB90_1:
	lui r1, %hi(fio_bz)
	addi r1, r1, %lo(fio_bz)
	addi r4, r0, 0
	stw r1+0, r4
.LBB90_2:
	lui r1, %hi(fio_l_nulls)
	addi r1, r1, %lo(fio_l_nulls)
	stw r1+0, r11
	lui r1, %hi(fio_l_after)
	addi r1, r1, %lo(fio_l_after)
	stb r1+0, r11
	lui r13, %hi(fio_reading)
	addi r13, r13, %lo(fio_reading)
	addi r14, r0, 1
	stb r13+0, r14
	lui r1, %hi(fio_ldone)
	addi r1, r1, %lo(fio_ldone)
	stb r1+0, r11
	lui r1, %hi(fio_l_pending)
	addi r1, r1, %lo(fio_l_pending)
	stw r1+0, r11
	lui r1, %hi(fio_gdepth)
	addi r1, r1, %lo(fio_gdepth)
	stw r1+0, r11
	lui r1, %hi(fio_scale)
	addi r1, r1, %lo(fio_scale)
	stw r1+0, r11
	lui r1, %hi(fio_rep)
	addi r1, r1, %lo(fio_rep)
	stw r1+0, r11
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	stw r1+0, r11
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	stw r1+0, r11
	seq r1, r3, r11
	lui r18, %hi(fio_listed)
	addi r18, r18, %lo(fio_listed)
	stw r18+0, r1
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
	lui r17, %hi(fio_fmt)
	addi r17, r17, %lo(fio_fmt)
	stw r17+0, r1
	lui r15, %hi(fio_pos)
	addi r15, r15, %lo(fio_pos)
	stw r15+0, r11
	lui r16, %hi(fio_revert)
	addi r16, r16, %lo(fio_revert)
	addi r19, r0, -1
	stw r16+0, r19
	lui r1, %hi(fio_since_data)
	addi r1, r1, %lo(fio_since_data)
	stb r1+0, r11
	beq r3, r11, .LBB90_4
.LBB90_3:
	jal r31, fio_fmt_use
	ble r1, r19, .LBB90_13
.LBB90_4:
	ldw r1, r18+0
	bne r1, r11, .LBB90_10
.LBB90_5:
	ldw r3, r17+0
	ldw r1, r15+0
	addi r4, r0, 32
.LBB90_6:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB90_8
.LBB90_7:
	addi r1, r1, 1
	stw r15+0, r1
	jal r0, .LBB90_6
.LBB90_8:
	addi r3, r0, 40
	bne r5, r3, .LBB90_10
.LBB90_9:
	addi r1, r1, 1
	stw r15+0, r1
	stw r16+0, r1
.LBB90_10:
	jal r31, fio_getrec
	beq r1, r11, .LBB90_15
.LBB90_11:
	addi r1, r0, 0
	stb r13+0, r1
	andi r3, r12, 3
	add r11, r14, r0
	bne r3, r1, .LBB90_15
.LBB90_12:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB90_13:
	stb r13+0, r11
	andi r1, r12, 2
	beq r1, r11, .LBB90_16
.LBB90_14:
	addi r11, r0, 2
.LBB90_15:
	add r1, r11, r0
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
.LBB90_16:
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end90:
	.size	fio_rd_setup, .Lfunc_end90-fio_rd_setup
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
	bne r9, r1, .LBB91_4
.LBB91_1:
	addi r9, r0, -400
	lui r10, %hi(fio_uend+400)
	addi r10, r10, %lo(fio_uend+400)
	addi lr, r0, -1
.LBB91_2:
	add r11, r9, r10
	stw r11+0, lr
	addi r9, r9, 4
	bne r9, r1, .LBB91_2
.LBB91_3:
	addi r9, r0, 1
	stb r8+0, r9
.LBB91_4:
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
.Lfunc_end91:
	.size	f77_rd_begin_int, .Lfunc_end91-f77_rd_begin_int
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
	beq r1, r14, .LBB92_5
.LBB92_1:
	lui r12, %hi(fio_ldone)
	addi r12, r12, %lo(fio_ldone)
	ldbu r1, r12+0
	bne r1, r14, .LBB92_57
.LBB92_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB92_57
.LBB92_3:
	addi r3, r0, 1
	bne r1, r3, .LBB92_27
.LBB92_4:
	stb r12+0, r3
	jal r0, .LBB92_57
.LBB92_5:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	beq r1, r14, .LBB92_11
.LBB92_6:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB92_11
.LBB92_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB92_12
.LBB92_8:
	addi r3, r0, 0
	beq r1, r3, .LBB92_57
.LBB92_9:
	addi r3, r0, 65
	bne r1, r3, .LBB92_30
.LBB92_10:
	addi r4, r0, 4
	add r3, r11, r0
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
	jal r0, f77_rd_s
.LBB92_11:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB92_8
.LBB92_12:
	addi r3, r0, 73
	beq r1, r3, .LBB92_35
.LBB92_13:
	addi r3, r0, 76
	bne r1, r3, .LBB92_30
.LBB92_14:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	addi r1, r0, 1
	lui r4, %hi(fio_rpos)
	addi r4, r4, %lo(fio_rpos)
	add r18, r14, r0
	blt r3, r1, .LBB92_31
.LBB92_15:
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
	addi r19, r0, 1022
	add r20, r3, r0
	add r21, r8, r0
	add r18, r8, r0
	jal r0, .LBB92_19
.LBB92_16:
	addi r21, r18, 1
	addi r23, sp, 28
	add r18, r23, r18
	stb r18+0, r22
	add r18, r21, r0
.LBB92_17:
	add r21, r16, r0
.LBB92_18:
	addi r20, r20, -1
	addi r5, r5, 1
	beq r20, r8, .LBB92_31
.LBB92_19:
	add r22, r9, r0
	bge r5, r6, .LBB92_21
.LBB92_20:
	add r22, r5, r10
	ldb r22, r22+0
.LBB92_21:
	beq r22, r9, .LBB92_23
.LBB92_22:
	bne r22, r17, .LBB92_26
.LBB92_23:
	beq r7, r8, .LBB92_18
.LBB92_24:
	beq r21, r8, .LBB92_18
.LBB92_25:
	add r22, lr, r0
	blt r18, r15, .LBB92_16
	jal r0, .LBB92_18
.LBB92_26:
	ble r18, r19, .LBB92_16
	jal r0, .LBB92_17
.LBB92_27:
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
	bgtu r4, r5, .LBB92_58
.LBB92_28:
	slli r4, r4, 2
	lui r5, %hi(.LJTI92_0)
	addi r5, r5, %lo(.LJTI92_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB92_29:
	andi r3, r3, 255
	addi r4, r0, 46
	seq r3, r3, r4
	add r1, r3, r1
	ldbu r1, r1+0
	andi r1, r1, 223
	addi r3, r0, 84
	seq r1, r1, r3
	stw r11+0, r1
	jal r0, .LBB92_57
.LBB92_30:
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
	jal r0, .LBB92_53
.LBB92_31:
	addi r5, sp, 28
	add r6, r5, r18
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
	ble r3, r4, .LBB92_50
.LBB92_32:
	addi r4, r0, 84
	beq r3, r4, .LBB92_53
.LBB92_33:
	addi r4, r0, 116
	beq r3, r4, .LBB92_53
.LBB92_34:
	addi r1, r0, 102
	beq r3, r1, .LBB92_52
	jal r0, .LBB92_59
.LBB92_35:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	addi r1, r0, 0
	addi r4, r0, 1
	add r16, r1, r0
	blt r3, r4, .LBB92_48
.LBB92_36:
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
	addi r17, r0, 1022
	add r18, r3, r0
	add r19, r8, r0
	add r16, r8, r0
	jal r0, .LBB92_40
.LBB92_37:
	addi r19, r16, 1
	addi r21, sp, 28
	add r16, r21, r16
	stb r16+0, r20
	add r16, r19, r0
.LBB92_38:
	add r19, r4, r0
.LBB92_39:
	addi r18, r18, -1
	addi r5, r5, 1
	beq r18, r8, .LBB92_48
.LBB92_40:
	add r20, r9, r0
	bge r5, r6, .LBB92_42
.LBB92_41:
	add r20, r5, r10
	ldb r20, r20+0
.LBB92_42:
	beq r20, r9, .LBB92_44
.LBB92_43:
	bne r20, r15, .LBB92_47
.LBB92_44:
	beq r7, r8, .LBB92_39
.LBB92_45:
	beq r19, r8, .LBB92_39
.LBB92_46:
	add r20, lr, r0
	blt r16, r14, .LBB92_37
	jal r0, .LBB92_39
.LBB92_47:
	ble r16, r17, .LBB92_37
	jal r0, .LBB92_38
.LBB92_48:
	addi r4, sp, 28
	add r4, r4, r16
	stb r4+0, r1
	lui r4, %hi(fio_rpos)
	addi r4, r4, %lo(fio_rpos)
	ldw r5, r4+0
	add r3, r5, r3
	stw r4+0, r3
	ldbu r3, sp+28
	beq r3, r1, .LBB92_53
.LBB92_49:
	addi r3, sp, 28
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	jal r0, .LBB92_53
.LBB92_50:
	addi r1, r0, 0
	beq r3, r1, .LBB92_52
.LBB92_51:
	addi r1, r0, 70
	bne r3, r1, .LBB92_59
.LBB92_52:
	addi r1, r0, 0
.LBB92_53:
	stw r11+0, r1
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB92_55
.LBB92_54:
	addi r1, r1, -1
	stw r13+0, r1
.LBB92_55:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB92_57
.LBB92_56:
	stw r12+0, r1
.LBB92_57:
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
.LBB92_58:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	stw r11+0, r1
	jal r0, .LBB92_57
.LBB92_59:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.91)
	addi r4, r4, %lo(.L.str.91)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end92:
	.size	f77_rd_i, .Lfunc_end92-f77_rd_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI92_0,@object
.LJTI92_0:
	.word	.LBB92_29
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_29
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_29
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_29
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_58
	.word	.LBB92_29
	.size	.LJTI92_0, 144
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
	stw sp+16, r22
	lui r14, %hi(fio_l_nulls)
	addi r14, r14, %lo(fio_l_nulls)
	ldw r1, r14+0
	addi r21, r0, 1
	blt r1, r21, .LBB93_2
.LBB93_1:
	addi r1, r1, -1
	stw r14+0, r1
	addi r11, r0, 2
	jal r0, .LBB93_5
.LBB93_2:
	lui r15, %hi(fio_l_pending)
	addi r15, r15, %lo(fio_l_pending)
	ldw r1, r15+0
	addi r17, r0, 0
	ble r1, r17, .LBB93_6
.LBB93_3:
	addi r1, r1, -1
	stw r15+0, r1
.LBB93_4:
	addi r11, r0, 0
.LBB93_5:
	add r1, r11, r0
	ldw r22, sp+16
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
.LBB93_6:
	lui r13, %hi(fio_rpos)
	addi r13, r13, %lo(fio_rpos)
	lui r18, %hi(fio_rlen)
	addi r18, r18, %lo(fio_rlen)
	lui r16, %hi(fio_rec)
	addi r16, r16, %lo(fio_rec)
	addi r19, r0, 38
	lui r22, %hi(.LJTI93_0)
	addi r22, r22, %lo(.LJTI93_0)
	lui r12, %hi(fio_l_after)
	addi r12, r12, %lo(fio_l_after)
	addi r11, r0, 2
	jal r0, .LBB93_8
.LBB93_7:
	jal r31, fio_getrec
	bne r1, r17, .LBB93_43
.LBB93_8:
	ldw r3, r13+0
	ldw r1, r18+0
	bge r3, r1, .LBB93_7
.LBB93_9:
	add r4, r3, r16
	ldbu r20, r4+0
	addi r4, r20, -9
	bgtu r4, r19, .LBB93_25
.LBB93_10:
	slli r4, r4, 2
	add r4, r22, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB93_11:
	addi r1, r3, 1
	stw r13+0, r1
	jal r0, .LBB93_8
.LBB93_12:
	addi r1, r3, 1
	stw r13+0, r1
	ldbu r1, r12+0
	bne r1, r21, .LBB93_5
.LBB93_13:
	stb r12+0, r17
	jal r0, .LBB93_8
.LBB93_14:
	addi r1, r3, 1
	stw r13+0, r1
	addi r14, r0, 126
	lui r11, %hi(fio_ltok)
	addi r11, r11, %lo(fio_ltok)
	add r15, r17, r0
	jal r0, .LBB93_16
.LBB93_15:
	jal r31, fio_getrec
	bne r1, r17, .LBB93_43
.LBB93_16:
	ldw r1, r13+0
	ldw r4, r18+0
	bge r1, r4, .LBB93_15
.LBB93_17:
	addi r5, r1, 1
	stw r13+0, r5
	add r3, r1, r16
	ldbu r3, r3+0
	bne r3, r20, .LBB93_21
.LBB93_18:
	bge r5, r4, .LBB93_23
.LBB93_19:
	add r4, r5, r16
	ldbu r4, r4+0
	bne r4, r20, .LBB93_23
.LBB93_20:
	addi r1, r1, 2
	stw r13+0, r1
.LBB93_21:
	bgtu r15, r14, .LBB93_16
.LBB93_22:
	addi r1, r15, 1
	add r4, r15, r11
	stb r4+0, r3
	add r15, r1, r0
	jal r0, .LBB93_16
.LBB93_23:
	add r1, r15, r11
	addi r11, r0, 0
	stb r1+0, r11
	addi r1, r0, 1
	stb r12+0, r1
	jal r0, .LBB93_5
.LBB93_24:
	addi r1, r3, 1
	stw r13+0, r1
	add r11, r21, r0
	jal r0, .LBB93_5
.LBB93_25:
	ldw r3, r13+0
	addi r17, r0, 0
	add r4, r17, r0
	bge r3, r1, .LBB93_32
.LBB93_26:
	addi r4, r0, 0
	addi r5, r0, 126
	lui r6, %hi(fio_ltok)
	addi r6, r6, %lo(fio_ltok)
	lui r7, %hi(.LJTI93_1)
	addi r7, r7, %lo(.LJTI93_1)
	jal r0, .LBB93_28
.LBB93_27:
	addi r3, r3, 1
	stw r13+0, r3
	beq r1, r3, .LBB93_32
.LBB93_28:
	add r8, r3, r16
	ldbu r8, r8+0
	addi r9, r8, -9
	bgtu r9, r19, .LBB93_30
.LBB93_29:
	slli r9, r9, 2
	add r9, r7, r9
	ldw r9, r9+0
	jalr r0, r9, 0
.LBB93_30:
	bgt r4, r5, .LBB93_27
.LBB93_31:
	addi r9, r4, 1
	add r4, r4, r6
	stb r4+0, r8
	add r4, r9, r0
	jal r0, .LBB93_27
.LBB93_32:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	add r3, r4, r1
	stb r3+0, r17
	addi r3, r0, 1
	stb r12+0, r3
	addi r4, r0, -11
.LBB93_33:
	add r5, r17, r1
	ldb r5, r5+0
	addi r6, r5, -58
	addi r17, r17, 1
	bgtu r6, r4, .LBB93_33
.LBB93_34:
	beq r17, r3, .LBB93_4
.LBB93_35:
	addi r1, r0, 42
	bne r5, r1, .LBB93_4
.LBB93_36:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	addi r12, r0, 0
	addi r5, r0, 10
	add r3, r13, r0
	add r4, r12, r0
	jal r31, strtol
	beq r1, r12, .LBB93_45
.LBB93_37:
	add r13, r17, r13
	ldbu r16, r13+0
	beq r16, r12, .LBB93_41
.LBB93_38:
	addi r3, r0, 2
	blt r1, r3, .LBB93_40
.LBB93_39:
	addi r1, r1, -1
	stw r15+0, r1
.LBB93_40:
	add r3, r13, r0
	jal r31, strlen
	addi r5, r1, 1
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	add r4, r13, r0
	jal r31, memmove
	jal r0, .LBB93_42
.LBB93_41:
	addi r1, r1, -1
	stw r14+0, r1
.LBB93_42:
	bne r16, r12, .LBB93_4
	jal r0, .LBB93_5
.LBB93_43:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
.LBB93_44:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB93_45:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.90)
	addi r4, r4, %lo(.L.str.90)
	jal r0, .LBB93_44
.Lfunc_end93:
	.size	fio_list_tok, .Lfunc_end93-fio_list_tok
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI93_0,@object
.LJTI93_0:
	.word	.LBB93_11
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_11
	.word	.LBB93_25
	.word	.LBB93_14
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_14
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_12
	.word	.LBB93_25
	.word	.LBB93_25
	.word	.LBB93_24
	.size	.LJTI93_0, 156
	.type	.LJTI93_1,@object
.LJTI93_1:
	.word	.LBB93_32
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_32
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_32
	.word	.LBB93_30
	.word	.LBB93_30
	.word	.LBB93_32
	.size	.LJTI93_1, 156
                                        # -- End function
	.text
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
	beq r1, r14, .LBB94_5
.LBB94_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r14, .LBB94_31
.LBB94_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB94_31
.LBB94_3:
	addi r15, r0, 1
	bne r1, r15, .LBB94_17
.LBB94_4:
	stb r13+0, r15
	jal r0, .LBB94_31
.LBB94_5:
	lui r13, %hi(fio_desc)
	addi r13, r13, %lo(fio_desc)
	ldw r1, r13+0
	lui r15, %hi(fio_rep)
	addi r15, r15, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB94_8
.LBB94_6:
	ldw r3, r15+0
	addi r4, r0, 0
	ble r3, r4, .LBB94_8
.LBB94_7:
	bne r1, r14, .LBB94_9
	jal r0, .LBB94_31
.LBB94_8:
	stw r13+0, r14
	jal r31, fio_next_desc
	beq r1, r14, .LBB94_31
.LBB94_9:
	addi r3, r0, 65
	bne r1, r3, .LBB94_32
.LBB94_10:
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
	bge r1, r12, .LBB94_22
.LBB94_11:
	blt r12, r4, .LBB94_27
.LBB94_12:
	ldw r4, r3+0
	lui r5, %hi(fio_rlen)
	addi r5, r5, %lo(fio_rlen)
	ldw r5, r5+0
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	add r8, r14, r0
	jal r0, .LBB94_14
.LBB94_13:
	add r10, r11, r8
	stb r10+0, r9
	addi r8, r8, 1
	beq r12, r8, .LBB94_27
.LBB94_14:
	add r9, r6, r0
	bge r8, r1, .LBB94_13
.LBB94_15:
	add r10, r4, r8
	add r9, r6, r0
	bge r10, r5, .LBB94_13
.LBB94_16:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB94_13
.LBB94_17:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r15, .LBB94_31
.LBB94_18:
	addi r3, r0, 32
	jal r0, .LBB94_20
.LBB94_19:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB94_31
.LBB94_20:
	add r4, r3, r0
	bge r14, r1, .LBB94_19
.LBB94_21:
	add r4, r14, r13
	ldbu r4, r4+0
	jal r0, .LBB94_19
.LBB94_22:
	blt r12, r4, .LBB94_27
.LBB94_23:
	ldw r5, r3+0
	lui r4, %hi(fio_rlen)
	addi r4, r4, %lo(fio_rlen)
	ldw r4, r4+0
	sub r8, r0, r12
	add r5, r1, r5
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	jal r0, .LBB94_25
.LBB94_24:
	stb r11+0, r9
	addi r9, r8, 1
	sltu r10, r9, r8
	addi r11, r11, 1
	add r8, r9, r0
	bne r10, r14, .LBB94_27
.LBB94_25:
	add r10, r5, r8
	add r9, r6, r0
	bge r10, r4, .LBB94_24
.LBB94_26:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB94_24
.LBB94_27:
	ldw r4, r3+0
	add r1, r4, r1
	stw r3+0, r1
	ldw r1, r15+0
	addi r3, r0, 1
	blt r1, r3, .LBB94_29
.LBB94_28:
	addi r1, r1, -1
	stw r15+0, r1
.LBB94_29:
	ldw r1, r15+0
	bne r1, r14, .LBB94_31
.LBB94_30:
	stw r13+0, r14
.LBB94_31:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB94_32:
	lui r3, %hi(stderr)
	addi r3, r3, %lo(stderr)
	ldw r3, r3+0
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end94:
	.size	f77_rd_s, .Lfunc_end94-f77_rd_s
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_in_real
	.type	.LCPI95_0,@object
.LCPI95_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI95_0, 8
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
	add r17, r8, r0
	blt r3, r5, .LBB95_13
.LBB95_1:
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
	addi r18, r0, 1022
	add r19, r3, r0
	add r20, lr, r0
	add r17, lr, r0
	jal r0, .LBB95_5
.LBB95_2:
	addi r20, r17, 1
	stw sp+16, r1
	add r1, sp, r0
	addi r1, r1, 2047
	addi r22, r1, 49
	ldw r1, sp+16
	add r17, r22, r17
	stb r17+0, r21
	add r17, r20, r0
.LBB95_3:
	add r20, r15, r0
.LBB95_4:
	addi r19, r19, -1
	addi r7, r7, 1
	beq r19, lr, .LBB95_13
.LBB95_5:
	add r21, r11, r0
	bge r7, r9, .LBB95_7
.LBB95_6:
	add r21, r7, r12
	ldb r21, r21+0
.LBB95_7:
	beq r21, r11, .LBB95_9
.LBB95_8:
	bne r21, r16, .LBB95_12
.LBB95_9:
	beq r10, lr, .LBB95_4
.LBB95_10:
	beq r20, lr, .LBB95_4
.LBB95_11:
	add r21, r13, r0
	blt r17, r14, .LBB95_2
	jal r0, .LBB95_4
.LBB95_12:
	bgt r17, r18, .LBB95_3
	jal r0, .LBB95_2
.LBB95_13:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r7, r9, 49
	add r7, r7, r17
	stb r7+0, r8
	ldw r7, r6+0
	add r3, r7, r3
	stw r6+0, r3
	add r6, sp, r0
	addi r6, r6, 2047
	ldbu r3, r6+49
	lui r9, %hi(.LCPI95_0)
	addi r9, r9, %lo(.LCPI95_0)
	ldw r7, r9+4
	ldw r6, r9+0
	beq r3, r8, .LBB95_66
.LBB95_14:
	addi r11, r0, 43
	beq r3, r11, .LBB95_17
.LBB95_15:
	addi r8, r0, 45
	bne r3, r8, .LBB95_18
.LBB95_16:
	addi r8, r0, 0
	addi r10, r0, 1
	jal r0, .LBB95_20
.LBB95_17:
	add r10, r5, r0
	jal r0, .LBB95_19
.LBB95_18:
	addi r10, r0, 0
.LBB95_19:
	add r8, r5, r0
.LBB95_20:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r3, r9, 49
	or  r3, r3, r10
	ldbu r21, r3+0
	addi r9, r0, 0
	beq r21, r9, .LBB95_42
.LBB95_21:
	addi r13, r0, 0
	addi r14, r0, 1
	addi r15, r0, 46
	addi r16, r0, -10
	addi r17, r0, 70
	lui r18, %hi(.LJTI95_0)
	addi r18, r18, %lo(.LJTI95_0)
	addi r19, r0, -1
	add lr, r14, r0
	add r12, r13, r0
	add r20, r13, r0
	add r3, r13, r0
.LBB95_22:
	andi r5, r21, 255
	bne r5, r15, .LBB95_24
.LBB95_23:
	add r20, r14, r0
	jal r0, .LBB95_34
.LBB95_24:
	slli r22, r21, 24
	srai r22, r22, 24
	addi r22, r22, -58
	bgeu r22, r16, .LBB95_31
.LBB95_25:
	addi r5, r5, -43
	bgtu r5, r17, .LBB95_40
.LBB95_26:
	slli r5, r5, 2
	add r5, r18, r5
	ldw r22, r5+0
	add r5, r13, r0
	add r21, r14, r0
	jalr r0, r22, 0
.LBB95_27:
	addi r5, r10, 1
	add r22, sp, r0
	addi r22, r22, 2047
	addi r21, r22, 49
	add r21, r21, r5
	ldbu r21, r21+0
	beq r21, r11, .LBB95_38
.LBB95_28:
	addi r22, r0, 45
	bne r21, r22, .LBB95_39
.LBB95_29:
	add r10, r5, r0
.LBB95_30:
	add r5, r13, r0
	add lr, r19, r0
	jal r0, .LBB95_35
.LBB95_31:
	addi r5, r0, 1022
	bgt r3, r5, .LBB95_33
.LBB95_32:
	addi r5, r3, 1
	addi r22, sp, 1072
	add r3, r22, r3
	stb r3+0, r21
	add r3, r5, r0
.LBB95_33:
	addi r5, r0, 0
	sne r5, r20, r5
	add r12, r12, r5
.LBB95_34:
	add r5, r14, r0
.LBB95_35:
	add r21, r14, r0
.LBB95_36:
	beq r21, r13, .LBB95_41
.LBB95_37:
	addi r10, r10, 1
	add r22, sp, r0
	addi r22, r22, 2047
	addi r21, r22, 49
	add r21, r21, r10
	ldbu r21, r21+0
	sne r22, r21, r13
	and r22, r22, r5
	bne r22, r13, .LBB95_22
	jal r0, .LBB95_41
.LBB95_38:
	add r10, r5, r0
.LBB95_39:
	add r5, r13, r0
	jal r0, .LBB95_35
.LBB95_40:
	add r5, r14, r0
	add r21, r13, r0
	bne r21, r13, .LBB95_37
.LBB95_41:
	addi r11, r0, 0
	seq r11, r20, r11
	xor r4, r4, r12
	sub r11, r0, r11
	and r4, r4, r11
	xor r4, r12, r4
	add r11, r9, r0
	beq r5, r9, .LBB95_43
	jal r0, .LBB95_47
.LBB95_42:
	add r3, r9, r0
	add lr, r5, r0
	add r11, r9, r0
	bne r5, r9, .LBB95_47
.LBB95_43:
	add r12, sp, r0
	addi r12, r12, 2047
	addi r11, r12, 49
	add r11, r11, r10
	ldbu r13, r11+0
	addi r11, r0, 0
	beq r13, r11, .LBB95_47
.LBB95_44:
	add r12, sp, r0
	addi r12, r12, 2047
	addi r11, r12, 49
	add r10, r10, r11
	addi r10, r10, 1
	addi r11, r0, 0
	addi r12, r0, 9
	addi r14, r0, 10
	add r15, r11, r0
.LBB95_45:
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
	bne r13, r11, .LBB95_45
.LBB95_46:
	mul r11, r15, lr
.LBB95_47:
	addi r10, sp, 1072
	add r10, r10, r3
	stb r10+0, r9
	beq r3, r9, .LBB95_66
.LBB95_48:
	sub r9, r3, r4
	lui r6, %hi(fio_scale)
	addi r6, r6, %lo(fio_scale)
	ldw r6, r6+0
	sub r5, r0, r5
	and r6, r6, r5
	addi r5, r0, 0
	addi r10, sp, 32
	bne r8, r5, .LBB95_50
.LBB95_49:
	addi r10, r1, 1
	addi r1, r0, 45
	stb sp+32, r1
	addi r5, r0, 1
.LBB95_50:
	sub r6, r11, r6
	addi r7, r0, 1
	blt r9, r7, .LBB95_55
.LBB95_51:
	blt r3, r7, .LBB95_61
.LBB95_52:
	xor r1, r9, r3
	slt r4, r9, r3
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r3, r1
	addi r4, sp, 32
	or  r7, r4, r5
	addi r4, r0, 0
.LBB95_53:
	addi r8, sp, 1072
	add r8, r8, r4
	ldbu r8, r8+0
	add r9, r7, r4
	stb r9+0, r8
	addi r4, r4, 1
	bne r1, r4, .LBB95_53
.LBB95_54:
	add r5, r5, r4
	jal r0, .LBB95_62
.LBB95_55:
	addi r1, sp, 32
	or  r1, r1, r5
	addi r8, r0, 48
	stb r1+0, r8
	addi r1, r5, 2
	addi r5, r0, 46
	stb r10+1, r5
	addi r5, r0, -1
	bgt r9, r5, .LBB95_58
.LBB95_56:
	sub r4, r4, r3
	addi r5, r0, 0
.LBB95_57:
	add r9, r1, r0
	addi r1, r1, 1
	addi r10, sp, 32
	add r9, r10, r9
	stb r9+0, r8
	addi r4, r4, -1
	bne r4, r5, .LBB95_57
.LBB95_58:
	blt r3, r7, .LBB95_65
.LBB95_59:
	addi r4, sp, 1072
	addi r5, r0, 0
.LBB95_60:
	add r7, r1, r0
	ldbu r8, r4+0
	addi r1, r1, 1
	addi r9, sp, 32
	add r7, r9, r7
	stb r7+0, r8
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r5, .LBB95_60
	jal r0, .LBB95_65
.LBB95_61:
	addi r4, r0, 0
.LBB95_62:
	addi r1, sp, 32
	add r1, r1, r5
	addi r7, r0, 46
	stb r1+0, r7
	addi r1, r5, 1
	ble r3, r4, .LBB95_65
.LBB95_63:
	sub r3, r3, r4
	addi r5, sp, 1072
	add r4, r5, r4
	addi r5, r0, 0
.LBB95_64:
	ldbu r7, r4+0
	addi r8, sp, 32
	add r8, r8, r1
	stb r8+0, r7
	addi r1, r1, 1
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r5, .LBB95_64
.LBB95_65:
	addi r11, sp, 32
	add r3, r11, r1
	addi r4, r0, 1040
	sub r4, r4, r1
	lui r5, %hi(.L.str.92)
	addi r5, r5, %lo(.L.str.92)
	jal r31, snprintf
	addi r4, r0, 0
	add r3, r11, r0
	jal r31, strtod
	add r6, r1, r0
	add r7, r2, r0
.LBB95_66:
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
.Lfunc_end95:
	.size	fio_in_real, .Lfunc_end95-fio_in_real
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI95_0,@object
.LJTI95_0:
	.word	.LBB95_36
	.word	.LBB95_40
	.word	.LBB95_30
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_27
	.word	.LBB95_27
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_27
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_27
	.word	.LBB95_27
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_40
	.word	.LBB95_27
	.size	.LJTI95_0, 284
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_rd_d
	.type	.LCPI96_0,@object
.LCPI96_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI96_0, 8
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
	beq r1, r12, .LBB96_5
.LBB96_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r12, .LBB96_39
.LBB96_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB96_39
.LBB96_3:
	addi r3, r0, 1
	bne r1, r3, .LBB96_26
.LBB96_4:
	stb r13+0, r3
	jal r0, .LBB96_39
.LBB96_5:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB96_11
.LBB96_6:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB96_11
.LBB96_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB96_12
.LBB96_8:
	addi r3, r0, 0
	beq r1, r3, .LBB96_39
.LBB96_9:
	addi r3, r0, 65
	bne r1, r3, .LBB96_33
.LBB96_10:
	addi r4, r0, 8
	add r3, r11, r0
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
	jal r0, f77_rd_s
.LBB96_11:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB96_8
.LBB96_12:
	addi r3, r0, 73
	bne r1, r3, .LBB96_32
.LBB96_13:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	addi r3, r3, %lo(fio_rpos)
	add r17, r14, r0
	blt r1, r4, .LBB96_30
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
	addi r18, r0, 1022
	add r19, r1, r0
	add r20, r8, r0
	add r17, r8, r0
	jal r0, .LBB96_18
.LBB96_15:
	addi r20, r17, 1
	addi r22, sp, 32
	add r17, r22, r17
	stb r17+0, r21
	add r17, r20, r0
.LBB96_16:
	add r20, r4, r0
.LBB96_17:
	addi r19, r19, -1
	addi r5, r5, 1
	beq r19, r8, .LBB96_30
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
	blt r17, r15, .LBB96_15
	jal r0, .LBB96_17
.LBB96_25:
	ble r17, r18, .LBB96_15
	jal r0, .LBB96_16
.LBB96_26:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	ldbu r6, r1+0
	add r7, r12, r0
	beq r6, r12, .LBB96_29
.LBB96_27:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r7, r1, r0
.LBB96_28:
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
	bne r6, r1, .LBB96_28
.LBB96_29:
	addi r3, sp, 32
	add r1, r3, r7
	stb r1+0, r12
	add r4, r12, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
	jal r0, .LBB96_39
.LBB96_30:
	addi r4, sp, 32
	add r4, r4, r17
	stb r4+0, r14
	ldw r4, r3+0
	add r1, r4, r1
	stw r3+0, r1
	ldbu r1, sp+32
	beq r1, r14, .LBB96_34
.LBB96_31:
	addi r3, sp, 32
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	fcvt.d.w r4, r1
	jal r0, .LBB96_35
.LBB96_32:
	addi r3, r0, 76
	beq r1, r3, .LBB96_40
.LBB96_33:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r3, r1+0
	lui r1, %hi(fio_d)
	addi r1, r1, %lo(fio_d)
	ldw r4, r1+0
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	jal r0, .LBB96_35
.LBB96_34:
	lui r1, %hi(.LCPI96_0)
	addi r1, r1, %lo(.LCPI96_0)
	ldw r5, r1+4
	ldw r4, r1+0
.LBB96_35:
	stw r11+4, r5
	stw r11+0, r4
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB96_37
.LBB96_36:
	addi r1, r1, -1
	stw r13+0, r1
.LBB96_37:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB96_39
.LBB96_38:
	stw r12+0, r1
.LBB96_39:
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
.LBB96_40:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end96:
	.size	f77_rd_d, .Lfunc_end96-f77_rd_d
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
	beq r7, r4, .LBB97_3
.LBB97_1:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r5, r0, 81
	lui r6, %hi(fio_ltok+1)
	addi r6, r6, %lo(fio_ltok+1)
	add r8, r1, r0
.LBB97_2:
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
	bne r7, r1, .LBB97_2
.LBB97_3:
	addi r3, sp, 16
	add r1, r3, r8
	stb r1+0, r4
	jal r31, strtod
	ldw lr, sp+0
	addi sp, sp, 144
	jalr r0, r31, 0
.Lfunc_end97:
	.size	fio_list_real, .Lfunc_end97-fio_list_real
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rd_r
	.type	.LCPI98_0,@object
.LCPI98_0:
	.word	0x00000000                      # float 0
	.size	.LCPI98_0, 4
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
	beq r1, r12, .LBB98_5
.LBB98_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r12, .LBB98_39
.LBB98_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB98_39
.LBB98_3:
	addi r3, r0, 1
	bne r1, r3, .LBB98_26
.LBB98_4:
	stb r13+0, r3
	jal r0, .LBB98_39
.LBB98_5:
	lui r12, %hi(fio_desc)
	addi r12, r12, %lo(fio_desc)
	ldw r1, r12+0
	lui r13, %hi(fio_rep)
	addi r13, r13, %lo(fio_rep)
	addi r14, r0, 0
	beq r1, r14, .LBB98_11
.LBB98_6:
	ldw r3, r13+0
	addi r4, r0, 0
	ble r3, r4, .LBB98_11
.LBB98_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB98_12
.LBB98_8:
	addi r3, r0, 0
	beq r1, r3, .LBB98_39
.LBB98_9:
	addi r3, r0, 65
	bne r1, r3, .LBB98_33
.LBB98_10:
	addi r4, r0, 4
	add r3, r11, r0
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
	jal r0, f77_rd_s
.LBB98_11:
	stw r12+0, r14
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB98_8
.LBB98_12:
	addi r3, r0, 73
	bne r1, r3, .LBB98_32
.LBB98_13:
	lui r1, %hi(fio_w)
	addi r1, r1, %lo(fio_w)
	ldw r1, r1+0
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	addi r3, r3, %lo(fio_rpos)
	add r17, r14, r0
	blt r1, r4, .LBB98_30
.LBB98_14:
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
	addi r18, r0, 1022
	add r19, r1, r0
	add r20, r8, r0
	add r17, r8, r0
	jal r0, .LBB98_18
.LBB98_15:
	addi r20, r17, 1
	addi r22, sp, 32
	add r17, r22, r17
	stb r17+0, r21
	add r17, r20, r0
.LBB98_16:
	add r20, r4, r0
.LBB98_17:
	addi r19, r19, -1
	addi r5, r5, 1
	beq r19, r8, .LBB98_30
.LBB98_18:
	add r21, r9, r0
	bge r5, r6, .LBB98_20
.LBB98_19:
	add r21, r5, r10
	ldb r21, r21+0
.LBB98_20:
	beq r21, r9, .LBB98_22
.LBB98_21:
	bne r21, r16, .LBB98_25
.LBB98_22:
	beq r7, r8, .LBB98_17
.LBB98_23:
	beq r20, r8, .LBB98_17
.LBB98_24:
	add r21, lr, r0
	blt r17, r15, .LBB98_15
	jal r0, .LBB98_17
.LBB98_25:
	ble r17, r18, .LBB98_15
	jal r0, .LBB98_16
.LBB98_26:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	ldbu r6, r1+0
	add r7, r12, r0
	beq r6, r12, .LBB98_29
.LBB98_27:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r7, r1, r0
.LBB98_28:
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
	bne r6, r1, .LBB98_28
.LBB98_29:
	addi r3, sp, 32
	add r1, r3, r7
	stb r1+0, r12
	add r4, r12, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	jal r0, .LBB98_39
.LBB98_30:
	addi r4, sp, 32
	add r4, r4, r17
	stb r4+0, r14
	ldw r4, r3+0
	add r1, r4, r1
	stw r3+0, r1
	ldbu r1, sp+32
	beq r1, r14, .LBB98_34
.LBB98_31:
	addi r3, sp, 32
	addi r4, r0, 0
	addi r5, r0, 10
	jal r31, strtol
	fcvt.s.w r1, r1
	jal r0, .LBB98_35
.LBB98_32:
	addi r3, r0, 76
	beq r1, r3, .LBB98_40
.LBB98_33:
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
	jal r0, .LBB98_35
.LBB98_34:
	lui r1, %hi(.LCPI98_0)
	addi r1, r1, %lo(.LCPI98_0)
	ldw r1, r1+0
.LBB98_35:
	stw r11+0, r1
	ldw r1, r13+0
	addi r3, r0, 1
	blt r1, r3, .LBB98_37
.LBB98_36:
	addi r1, r1, -1
	stw r13+0, r1
.LBB98_37:
	ldw r3, r13+0
	addi r1, r0, 0
	bne r3, r1, .LBB98_39
.LBB98_38:
	stw r12+0, r1
.LBB98_39:
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
.LBB98_40:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end98:
	.size	f77_rd_r, .Lfunc_end98-f77_rd_r
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rd_c
	.type	.LCPI99_0,@object
.LCPI99_0:
	.word	0x00000000                      # float 0
	.size	.LCPI99_0, 4
	.text
	.globl	f77_rd_c
	.p2align	2
	.type	f77_rd_c,@function
f77_rd_c:                               # @f77_rd_c
# %bb.0:
	addi sp, sp, -320
	stw sp+0, lr
	stw sp+316, r11
	stw sp+312, r12
	stw sp+308, r13
	stw sp+304, r14
	stw sp+300, r15
	stw sp+296, r16
	stw sp+292, r17
	stw sp+288, r18
	stw sp+284, r19
	stw sp+280, r20
	stw sp+276, r21
	stw sp+272, r22
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r13, r0, 0
	beq r1, r13, .LBB99_5
.LBB99_1:
	lui r12, %hi(fio_ldone)
	addi r12, r12, %lo(fio_ldone)
	ldbu r1, r12+0
	bne r1, r13, .LBB99_34
.LBB99_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB99_34
.LBB99_3:
	addi r15, r0, 1
	bne r1, r15, .LBB99_6
.LBB99_4:
	stb r12+0, r15
	jal r0, .LBB99_34
.LBB99_5:
	add r3, r11, r0
	jal r31, f77_rd_r
	addi r3, r11, 4
	ldw r22, sp+272
	ldw r21, sp+276
	ldw r20, sp+280
	ldw r19, sp+284
	ldw r18, sp+288
	ldw r17, sp+292
	ldw r16, sp+296
	ldw r15, sp+300
	ldw r14, sp+304
	ldw r13, sp+308
	ldw r12, sp+312
	ldw r11, sp+316
	ldw lr, sp+0
	addi sp, sp, 320
	jal r0, f77_rd_r
.LBB99_6:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	ldbu r3, r12+0
	addi r1, r0, 40
	bne r3, r1, .LBB99_12
.LBB99_7:
	lui r16, %hi(fio_ltok+1)
	addi r16, r16, %lo(fio_ltok+1)
	ldbu r5, r16+0
	addi r13, r0, 0
	add r14, r13, r0
	beq r5, r13, .LBB99_16
.LBB99_8:
	addi r1, r0, 0
	lui r3, %hi(fio_ltok+2)
	addi r3, r3, %lo(fio_ltok+2)
	addi r4, r0, 126
	add r14, r1, r0
	jal r0, .LBB99_10
.LBB99_9:
	ldbu r5, r3+0
	addi r3, r3, 1
	beq r5, r1, .LBB99_16
.LBB99_10:
	bgt r14, r4, .LBB99_9
.LBB99_11:
	addi r6, r14, 1
	addi r7, sp, 16
	add r7, r7, r14
	stb r7+0, r5
	add r14, r6, r0
	jal r0, .LBB99_9
.LBB99_12:
	add r7, r13, r0
	beq r3, r13, .LBB99_15
.LBB99_13:
	addi r1, r0, 0
	addi r4, r0, 68
	addi r5, r0, 81
	lui r6, %hi(fio_ltok+1)
	addi r6, r6, %lo(fio_ltok+1)
	add r7, r1, r0
.LBB99_14:
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
	addi r7, sp, 144
	add r7, r7, r8
	stb r7+0, r3
	addi r7, r8, 1
	add r3, r8, r6
	ldbu r3, r3+0
	bne r3, r1, .LBB99_14
.LBB99_15:
	addi r3, sp, 144
	add r1, r3, r7
	stb r1+0, r13
	add r4, r13, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	stw r11+4, r13
	jal r0, .LBB99_34
.LBB99_16:
	lui r17, %hi(fio_rpos)
	addi r17, r17, %lo(fio_rpos)
	lui r18, %hi(fio_rlen)
	addi r18, r18, %lo(fio_rlen)
	addi r19, r0, 126
	lui r20, %hi(fio_rec)
	addi r20, r20, %lo(fio_rec)
	addi r21, r0, 41
.LBB99_17:
	addi r1, sp, 16
	add r22, r1, r14
	jal r0, .LBB99_19
.LBB99_18:
	jal r31, fio_getrec
	bne r1, r13, .LBB99_35
.LBB99_19:
	beq r14, r13, .LBB99_21
.LBB99_20:
	ldbu r1, r22+-1
	beq r1, r21, .LBB99_24
.LBB99_21:
	ldw r1, r17+0
	ldw r3, r18+0
	bge r1, r3, .LBB99_18
.LBB99_22:
	addi r3, r1, 1
	stw r17+0, r3
	bgt r14, r19, .LBB99_19
.LBB99_23:
	add r1, r1, r20
	ldbu r1, r1+0
	addi r3, r14, 1
	addi r4, sp, 16
	add r4, r4, r14
	stb r4+0, r1
	add r14, r3, r0
	jal r0, .LBB99_17
.LBB99_24:
	addi r13, r0, 0
	stb r22+-1, r13
	addi r3, sp, 16
	addi r4, r0, 44
	jal r31, strchr
	beq r1, r13, .LBB99_32
.LBB99_25:
	add r14, r1, r0
	stb r1+0, r13
	addi r4, sp, 16
	add r3, r12, r0
	jal r31, strcpy
	ldbu r5, r12+0
	add r6, r13, r0
	beq r5, r13, .LBB99_28
.LBB99_26:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	add r6, r1, r0
.LBB99_27:
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
	addi r6, sp, 144
	add r6, r6, r7
	stb r6+0, r5
	addi r6, r7, 1
	add r5, r7, r16
	ldbu r5, r5+0
	bne r5, r1, .LBB99_27
.LBB99_28:
	addi r3, sp, 144
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
	beq r5, r13, .LBB99_31
.LBB99_29:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	add r6, r1, r0
.LBB99_30:
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
	addi r6, sp, 144
	add r6, r6, r7
	stb r6+0, r5
	addi r6, r7, 1
	add r5, r7, r16
	ldbu r5, r5+0
	bne r5, r1, .LBB99_30
.LBB99_31:
	addi r3, sp, 144
	add r1, r3, r6
	stb r1+0, r13
	add r4, r13, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	jal r0, .LBB99_33
.LBB99_32:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r4, sp, 16
	jal r31, strcpy
	jal r31, fio_list_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	lui r1, %hi(.LCPI99_0)
	addi r1, r1, %lo(.LCPI99_0)
	ldw r1, r1+0
.LBB99_33:
	stw r11+4, r1
	lui r1, %hi(fio_l_after)
	addi r1, r1, %lo(fio_l_after)
	stb r1+0, r15
.LBB99_34:
	ldw r22, sp+272
	ldw r21, sp+276
	ldw r20, sp+280
	ldw r19, sp+284
	ldw r18, sp+288
	ldw r17, sp+292
	ldw r16, sp+296
	ldw r15, sp+300
	ldw r14, sp+304
	ldw r13, sp+308
	ldw r12, sp+312
	ldw r11, sp+316
	ldw lr, sp+0
	addi sp, sp, 320
	jalr r0, r31, 0
.LBB99_35:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end99:
	.size	f77_rd_c, .Lfunc_end99-f77_rd_c
                                        # -- End function
	.globl	f77_rd_z                        # -- Begin function f77_rd_z
	.p2align	2
	.type	f77_rd_z,@function
f77_rd_z:                               # @f77_rd_z
# %bb.0:
	addi sp, sp, -176
	stw sp+0, lr
	stw sp+172, r11
	stw sp+168, r12
	stw sp+164, r13
	stw sp+160, r14
	stw sp+156, r15
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r12, r0, 0
	beq r1, r12, .LBB100_5
.LBB100_1:
	lui r13, %hi(fio_ldone)
	addi r13, r13, %lo(fio_ldone)
	ldbu r1, r13+0
	bne r1, r12, .LBB100_23
.LBB100_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB100_23
.LBB100_3:
	addi r3, r0, 1
	bne r1, r3, .LBB100_6
.LBB100_4:
	stb r13+0, r3
	jal r0, .LBB100_23
.LBB100_5:
	add r3, r11, r0
	jal r31, f77_rd_d
	addi r3, r11, 8
	ldw r15, sp+156
	ldw r14, sp+160
	ldw r13, sp+164
	ldw r12, sp+168
	ldw r11, sp+172
	ldw lr, sp+0
	addi sp, sp, 176
	jal r0, f77_rd_d
.LBB100_6:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	ldbu r3, r1+0
	addi r1, r0, 40
	bne r3, r1, .LBB100_17
.LBB100_7:
	lui r14, %hi(fio_ltok)
	addi r14, r14, %lo(fio_ltok)
	addi r4, r0, 41
	add r3, r14, r0
	jal r31, strchr
	addi r12, r0, 0
	beq r1, r12, .LBB100_9
.LBB100_8:
	stb r1+0, r12
.LBB100_9:
	lui r3, %hi(fio_ltok+1)
	addi r3, r3, %lo(fio_ltok+1)
	addi r4, r0, 44
	jal r31, strchr
	beq r1, r12, .LBB100_21
.LBB100_10:
	add r15, r1, r0
	stb r1+0, r12
	lui r13, %hi(fio_ltok+1)
	addi r13, r13, %lo(fio_ltok+1)
	add r3, r13, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r14, r0
	add r4, r13, r0
	jal r31, memmove
	ldbu r5, r14+0
	add r6, r12, r0
	beq r5, r12, .LBB100_13
.LBB100_11:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	add r6, r1, r0
.LBB100_12:
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
	addi r6, sp, 28
	add r6, r6, r7
	stb r6+0, r5
	addi r6, r7, 1
	add r5, r7, r13
	ldbu r5, r5+0
	bne r5, r1, .LBB100_12
.LBB100_13:
	addi r3, sp, 28
	add r1, r3, r6
	stb r1+0, r12
	add r4, r12, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
	addi r4, r15, 1
	add r3, r14, r0
	jal r31, strcpy
	ldbu r5, r14+0
	add r6, r12, r0
	beq r5, r12, .LBB100_16
.LBB100_14:
	addi r1, r0, 0
	addi r3, r0, 68
	addi r4, r0, 81
	add r6, r1, r0
.LBB100_15:
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
	addi r6, sp, 28
	add r6, r6, r7
	stb r6+0, r5
	addi r6, r7, 1
	add r5, r7, r13
	ldbu r5, r5+0
	bne r5, r1, .LBB100_15
.LBB100_16:
	addi r3, sp, 28
	add r1, r3, r6
	stb r1+0, r12
	add r4, r12, r0
	jal r31, strtod
	stw r11+12, r2
	jal r0, .LBB100_22
.LBB100_17:
	add r7, r12, r0
	beq r3, r12, .LBB100_20
.LBB100_18:
	addi r1, r0, 0
	addi r4, r0, 68
	addi r5, r0, 81
	lui r6, %hi(fio_ltok+1)
	addi r6, r6, %lo(fio_ltok+1)
	add r7, r1, r0
.LBB100_19:
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
	addi r7, sp, 28
	add r7, r7, r8
	stb r7+0, r3
	addi r7, r8, 1
	add r3, r8, r6
	ldbu r3, r3+0
	bne r3, r1, .LBB100_19
.LBB100_20:
	addi r3, sp, 28
	add r1, r3, r7
	stb r1+0, r12
	add r4, r12, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
	stw r11+12, r12
	stw r11+8, r12
	jal r0, .LBB100_23
.LBB100_21:
	lui r12, %hi(fio_ltok+1)
	addi r12, r12, %lo(fio_ltok+1)
	add r3, r12, r0
	jal r31, strlen
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r5, r1, 1
	add r4, r12, r0
	jal r31, memmove
	jal r31, fio_list_real
	stw r11+4, r2
	stw r11+0, r1
	addi r1, r0, 0
	stw r11+12, r1
.LBB100_22:
	stw r11+8, r1
.LBB100_23:
	ldw r15, sp+156
	ldw r14, sp+160
	ldw r13, sp+164
	ldw r12, sp+168
	ldw r11, sp+172
	ldw lr, sp+0
	addi sp, sp, 176
	jalr r0, r31, 0
.Lfunc_end100:
	.size	f77_rd_z, .Lfunc_end100-f77_rd_z
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
.Lfunc_end101:
	.size	f77_rd_end, .Lfunc_end101-f77_rd_end
                                        # -- End function
	.globl	f77_nml_begin                   # -- Begin function f77_nml_begin
	.p2align	2
	.type	f77_nml_begin,@function
f77_nml_begin:                          # @f77_nml_begin
# %bb.0:
	addi sp, sp, -1168
	stw sp+0, lr
	stw sp+4, fp
	stw sp+1164, r11
	stw sp+1160, r12
	stw sp+1156, r13
	stw sp+1152, r14
	stw sp+1148, r15
	stw sp+1144, r16
	stw sp+1140, r17
	stw sp+1136, r18
	stw sp+1132, r19
	stw sp+1128, r20
	stw sp+1124, r21
	stw sp+1120, r22
	stw sp+1116, r23
	stw sp+1112, r24
	stw sp+1108, r25
	stw sp+1104, r26
	stw sp+1100, r27
	stw sp+1096, r28
	add r13, r6, r0
	lui r26, %hi(nml_unit)
	addi r26, r26, %lo(nml_unit)
	stw r26+0, r3
	lui r1, %hi(nml_reading)
	addi r1, r1, %lo(nml_reading)
	stw r1+0, r6
	lui r1, %hi(nml_flags)
	addi r1, r1, %lo(nml_flags)
	stw r1+0, r7
	lui r22, %hi(nml_status)
	addi r22, r22, %lo(nml_status)
	addi r11, r0, 0
	stw r22+0, r11
	lui r1, %hi(nml_ndim)
	addi r1, r1, %lo(nml_ndim)
	stw r1+0, r11
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stb r1+0, r11
	addi r1, r0, 31
	slt r1, r5, r1
	sub r1, r0, r1
	xori r3, r5, 31
	and r1, r3, r1
	xori r15, r1, 31
	addi fp, r0, 1
	lui r12, %hi(nml_grp)
	addi r12, r12, %lo(nml_grp)
	blt r5, fp, .LBB102_3
.LBB102_1:
	add r14, r4, r0
	addi r16, r0, 0
.LBB102_2:
	add r1, r14, r16
	ldbu r3, r1+0
	jal r31, toupper
	add r3, r16, r12
	stb r3+0, r1
	addi r16, r16, 1
	bne r15, r16, .LBB102_2
.LBB102_3:
	add r1, r15, r12
	stb r1+0, r11
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw sp+68, r1
	beq r13, r11, .LBB102_64
.LBB102_4:
	lui r1, %hi(nml_nasg)
	addi r1, r1, %lo(nml_nasg)
	stw sp+36, r1
	stw r1+0, r11
	lui r1, %hi(nml_nval)
	addi r1, r1, %lo(nml_nval)
	stw sp+44, r1
	stw r1+0, r11
	add r3, r12, r0
	jal r31, strlen
	stw sp+48, r1
	addi r13, r1, 2
	lui r1, %hi(nml_tlen)
	addi r1, r1, %lo(nml_tlen)
	stw sp+40, r1
	stw r1+0, r11
	lui r3, %hi(.L.str.10)
	addi r3, r3, %lo(.L.str.10)
	add r4, r11, r0
	jal r31, nml_text_add
	addi r1, sp, 72
	stw sp+28, r13
	add r3, r1, r13
	stw sp+24, r3
	addi r1, r1, 2
	stw sp+32, r1
	lui r14, %hi(.L.str.93)
	addi r14, r14, %lo(.L.str.93)
	addi r13, r0, 3
	addi r1, r0, 1023
	stw sp+60, r1
	lui r1, %hi(fio_reading)
	addi r1, r1, %lo(fio_reading)
	stw sp+56, r1
	addi r21, r0, 1022
	addi r24, r0, 32
	addi r25, r0, 2
	addi r1, r0, 36
	stw sp+52, r1
                                        # implicit-def: $r20
	add r18, r11, r0
	stw sp+64, r22
	jal r0, .LBB102_7
.LBB102_5:
	add r20, r1, r0
	add r15, r1, r0
	addi r17, r0, 0
	beq r15, r17, .LBB102_7
.LBB102_6:
	addi r1, r0, 3
	bne r15, r1, .LBB102_62
.LBB102_7:
	ldw r3, r26+0
	add r4, r14, r0
	add r5, r13, r0
	jal r31, f77_rd_begin
	beq r1, r11, .LBB102_11
.LBB102_8:
	addi r15, r0, 1
	beq r1, r15, .LBB102_5
.LBB102_9:
	ldw r1, r22+0
	addi r20, r0, 0
	beq r1, r20, .LBB102_22
.LBB102_10:
	addi r17, r0, 0
	bne r15, r17, .LBB102_6
	jal r0, .LBB102_7
.LBB102_11:
	addi r3, sp, 72
	ldw r4, sp+60
	jal r31, f77_rd_s
	ldw r1, sp+56
	stb r1+0, r11
	ldw r1, sp+68
	stw r1+0, r11
	add r1, r21, r0
.LBB102_12:
	addi r3, sp, 72
	add r3, r3, r1
	ldbu r3, r3+0
	bne r3, r24, .LBB102_15
.LBB102_13:
	addi r3, r1, -1
	addi r4, r1, 1
	add r1, r3, r0
	bgtu r4, r25, .LBB102_12
.LBB102_14:
	add r28, fp, r0
	jal r0, .LBB102_16
.LBB102_15:
	addi r28, r1, 1
.LBB102_16:
	add r27, fp, r0
	add r23, fp, r0
	bne r18, r11, .LBB102_38
.LBB102_17:
	blt r28, r25, .LBB102_23
.LBB102_18:
	addi r1, r28, -1
	add r15, r11, r0
.LBB102_19:
	addi r3, sp, 72
	add r3, r3, r15
	ldbu r3, r3+1
	bne r3, r24, .LBB102_24
.LBB102_20:
	addi r15, r15, 1
	bne r1, r15, .LBB102_19
.LBB102_21:
	add r27, r11, r0
	add r23, r28, r0
	add r15, r13, r0
	bne r27, r11, .LBB102_38
	jal r0, .LBB102_60
.LBB102_22:
	stw r22+0, r25
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	addi r5, r0, 95
	jal r31, strncpy
	addi r20, r0, 0
	lui r1, %hi(nml_err+95)
	addi r1, r1, %lo(nml_err+95)
	stb r1+0, r20
	addi r17, r0, 0
	bne r15, r17, .LBB102_6
	jal r0, .LBB102_7
.LBB102_23:
	add r27, r11, r0
	add r23, fp, r0
	add r15, r13, r0
	bne r27, r11, .LBB102_38
	jal r0, .LBB102_60
.LBB102_24:
	ldw r1, sp+52
	beq r3, r1, .LBB102_26
.LBB102_25:
	addi r1, r0, 38
	bne r3, r1, .LBB102_58
.LBB102_26:
	addi r23, r15, 1
	ldw r1, sp+48
	blt r1, fp, .LBB102_31
.LBB102_27:
	ldw r1, sp+32
	add r16, r1, r15
	sub r1, r15, r28
	addi r17, r1, 2
	add r18, r11, r0
.LBB102_28:
	add r1, r17, r18
	beq r1, r11, .LBB102_61
.LBB102_29:
	add r1, r16, r18
	ldbu r3, r1+0
	jal r31, toupper
	add r3, r18, r12
	ldb r3, r3+0
	bne r1, r3, .LBB102_61
.LBB102_30:
	addi r18, r18, 1
	ldw r1, sp+48
	bne r1, r18, .LBB102_28
.LBB102_31:
	ldw r1, sp+28
	add r16, r1, r15
	bge r16, r28, .LBB102_36
.LBB102_32:
	ldw r1, sp+24
	add r1, r1, r15
	ldbu r17, r1+0
	add r3, r17, r0
	jal r31, isalnum
	addi r27, r0, 0
	bne r1, r27, .LBB102_59
.LBB102_33:
	addi r27, r0, 0
	addi r1, r17, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB102_59
.LBB102_34:
	addi r1, r0, 64
	beq r17, r1, .LBB102_59
.LBB102_35:
	addi r1, r0, 95
	add r15, r13, r0
	beq r17, r1, .LBB102_37
.LBB102_36:
	addi r15, r0, 0
	addi r27, r0, 1
	add r23, r16, r0
.LBB102_37:
	beq r27, r11, .LBB102_60
.LBB102_38:
	sub r17, r28, r23
	ble r28, r23, .LBB102_56
.LBB102_39:
	addi r1, sp, 72
	add r18, r1, r23
	addi r22, r0, 0
	add r19, r22, r0
	add r3, r22, r0
	jal r0, .LBB102_41
.LBB102_40:
	addi r19, r19, 1
	beq r17, r19, .LBB102_56
.LBB102_41:
	add r16, r18, r19
	ldb r1, r16+0
	beq r3, r22, .LBB102_43
.LBB102_42:
	seq r1, r3, r1
	sub r1, r0, r1
	and r1, r3, r1
	xor r3, r3, r1
	addi r15, r0, 13
	jal r0, .LBB102_54
.LBB102_43:
	andi r3, r1, 255
	addi r15, r0, 13
	addi r4, r0, 34
	beq r3, r4, .LBB102_47
.LBB102_44:
	addi r4, r0, 39
	beq r3, r4, .LBB102_47
.LBB102_45:
	addi r4, r0, 47
	bne r3, r4, .LBB102_48
.LBB102_46:
	add r3, r18, r0
	add r4, r19, r0
	jal r31, nml_text_add
	addi r15, r0, 1
	addi r20, r0, 0
	add r3, r20, r0
	jal r0, .LBB102_54
.LBB102_47:
	add r3, r1, r0
	jal r0, .LBB102_54
.LBB102_48:
	addi r3, r0, -3
	and r1, r1, r3
	addi r15, r0, 0
	ldw r3, sp+52
	bne r1, r3, .LBB102_53
.LBB102_49:
	add r1, r23, r19
	addi r1, r1, 3
	bgt r1, r28, .LBB102_53
.LBB102_50:
	ldbu r3, r16+1
	jal r31, toupper
	addi r3, r0, 69
	bne r1, r3, .LBB102_53
.LBB102_51:
	ldbu r3, r16+2
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB102_53
.LBB102_52:
	ldbu r3, r16+3
	jal r31, toupper
	addi r4, r0, 68
	add r3, r15, r0
	bne r1, r4, .LBB102_54
	jal r0, .LBB102_46
.LBB102_53:
	add r3, r15, r0
.LBB102_54:
	addi r1, r0, 13
	beq r15, r1, .LBB102_40
.LBB102_55:
	addi r1, r0, 0
	beq r15, r1, .LBB102_40
	jal r0, .LBB102_57
.LBB102_56:
	addi r1, sp, 72
	add r3, r1, r23
	add r4, r17, r0
	jal r31, nml_text_add
	lui r3, %hi(.L.str.95)
	addi r3, r3, %lo(.L.str.95)
	addi r4, r0, 1
	jal r31, nml_text_add
	addi r15, r0, 0
.LBB102_57:
	add r18, r27, r0
	ldw r22, sp+64
	addi r17, r0, 0
	bne r15, r17, .LBB102_6
	jal r0, .LBB102_7
.LBB102_58:
	addi r23, r15, 1
	addi r27, r0, 0
.LBB102_59:
	add r15, r13, r0
	bne r27, r11, .LBB102_38
.LBB102_60:
	add r18, r27, r0
	addi r17, r0, 0
	bne r15, r17, .LBB102_6
	jal r0, .LBB102_7
.LBB102_61:
	add r27, r11, r0
	add r15, r13, r0
	bne r27, r11, .LBB102_38
	jal r0, .LBB102_60
.LBB102_62:
	beq r20, r17, .LBB102_70
.LBB102_63:
	addi r1, r0, 1
	stw r22+0, r1
	jal r0, .LBB102_69
.LBB102_64:
	lui r1, 2
	addi r1, r1, 1568
	sth sp+72, r1
	addi r11, sp, 72
	addi r3, r11, 2
	lui r4, %hi(nml_grp)
	addi r4, r4, %lo(nml_grp)
	add r5, r15, r0
	jal r31, memcpy
	addi r12, r15, 2
	ldw r3, r26+0
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	jal r31, f77_wr_begin
	add r3, r11, r0
	add r4, r12, r0
	jal r31, f77_wr_a
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r11, r0, 0
	bne r1, r11, .LBB102_68
.LBB102_65:
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	ldw r4, r1+0
	lui r3, %hi(fio_rep)
	addi r3, r3, %lo(fio_rep)
	beq r4, r11, .LBB102_67
.LBB102_66:
	ldw r4, r3+0
	bgt r4, r11, .LBB102_68
.LBB102_67:
	lui r12, %hi(fio_draining)
	addi r12, r12, %lo(fio_draining)
	addi r4, r0, 1
	stb r12+0, r4
	stw r1+0, r11
	stw r3+0, r11
	jal r31, fio_next_desc
	stb r12+0, r11
.LBB102_68:
	jal r31, fio_flush_line
	ldw r1, sp+68
	stw r1+0, r11
	lui r1, %hi(nml_width)
	addi r1, r1, %lo(nml_width)
	addi r3, r0, 1
	stb r1+0, r3
	lui r1, %hi(nml_line)
	addi r1, r1, %lo(nml_line)
	addi r4, r0, 32
	stb r1+0, r4
	lui r1, %hi(nml_len)
	addi r1, r1, %lo(nml_len)
	stw r1+0, r3
	lui r1, %hi(nml_npend)
	addi r1, r1, %lo(nml_npend)
	addi r3, r0, -1
	stw r1+0, r3
.LBB102_69:
	ldw r28, sp+1096
	ldw r27, sp+1100
	ldw r26, sp+1104
	ldw r25, sp+1108
	ldw r24, sp+1112
	ldw r23, sp+1116
	ldw r22, sp+1120
	ldw r21, sp+1124
	ldw r20, sp+1128
	ldw r19, sp+1132
	ldw r18, sp+1136
	ldw r17, sp+1140
	ldw r16, sp+1144
	ldw r15, sp+1148
	ldw r14, sp+1152
	ldw r13, sp+1156
	ldw r12, sp+1160
	ldw r11, sp+1164
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 1168
	jalr r0, r31, 0
.LBB102_70:
	ldw r1, sp+36
	stw r1+0, r17
	ldw r1, sp+44
	stw r1+0, r17
	ldw r26, sp+40
	ldw r1, r26+0
	addi r16, r0, 1
	blt r1, r16, .LBB102_69
.LBB102_71:
	ldw r1, r22+0
	bne r1, r17, .LBB102_69
.LBB102_72:
	lui r18, %hi(nml_text)
	addi r18, r18, %lo(nml_text)
	addi r19, r0, 44
	lui r1, %hi(nml_cval)
	addi r1, r1, %lo(nml_cval)
	stw sp+68, r1
	lui r27, %hi(nml_val)
	addi r27, r27, %lo(nml_val)
	addi r28, r0, 12
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+52, r1
	lui r1, %hi(.L.str.98)
	addi r1, r1, %lo(.L.str.98)
	stw sp+48, r1
	addi r13, r0, 95
	lui r1, %hi(nml_err+95)
	addi r1, r1, %lo(nml_err+95)
	stw sp+60, r1
	addi r1, r0, 42
	stw sp+56, r1
	lui r1, %hi(.L.str.99)
	addi r1, r1, %lo(.L.str.99)
	stw sp+32, r1
	add r14, r16, r0
	add r12, r17, r0
	add fp, r17, r0
.LBB102_73:
	ldw r1, r18+0
	add r1, r1, fp
	ldbu r3, r1+0
	beq r3, r19, .LBB102_76
.LBB102_74:
	bne r3, r24, .LBB102_85
.LBB102_75:
	addi r11, fp, 1
	jal r0, .LBB102_82
.LBB102_76:
	beq r14, r17, .LBB102_81
.LBB102_77:
	beq r12, r17, .LBB102_81
.LBB102_78:
	ldw r1, sp+44
	ldw r3, r1+0
	ldw r1, sp+68
	ldw r1, r1+0
	blt r3, r1, .LBB102_80
.LBB102_79:
	seq r3, r1, r17
	slli r1, r1, 1
	sub r3, r0, r3
	xori r4, r1, 64
	and r3, r4, r3
	xor r1, r1, r3
	ldw r3, sp+68
	stw r3+0, r1
	ldw r3, r27+0
	mul r4, r1, r28
	jal r31, realloc
	stw r27+0, r1
.LBB102_80:
	ldw r1, r27+0
	ldw r5, sp+44
	ldw r3, r5+0
	mul r4, r3, r28
	add r1, r1, r4
	stw r1+0, fp
	stw r1+4, r17
	stw r1+8, r16
	addi r1, r3, 1
	stw r5+0, r1
	ldw r1, r12+68
	addi r1, r1, 1
	stw r12+68, r1
.LBB102_81:
	addi r11, fp, 1
	add r14, r16, r0
.LBB102_82:
	add r15, r25, r0
	ori  r1, r15, 2
	bne r1, r25, .LBB102_69
.LBB102_83:
	ldw r1, r26+0
	bge r11, r1, .LBB102_69
.LBB102_84:
	ldw r1, r22+0
	add fp, r11, r0
	beq r1, r17, .LBB102_73
	jal r0, .LBB102_69
.LBB102_85:
	jal r31, isalpha
                                        # implicit-def: $r11
	add r5, r16, r0
	beq r1, r17, .LBB102_106
.LBB102_86:
	ldw r1, r26+0
	add r20, fp, r0
	bge fp, r1, .LBB102_93
.LBB102_87:
	add r20, fp, r0
	jal r0, .LBB102_89
.LBB102_88:
	addi r20, r20, 1
	ldw r1, r26+0
	bge r20, r1, .LBB102_93
.LBB102_89:
	ldw r1, r18+0
	add r1, r1, r20
	ldbu r15, r1+0
	add r3, r15, r0
	jal r31, isalnum
	addi r3, r0, 0
	bne r1, r3, .LBB102_88
.LBB102_90:
	addi r1, r15, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB102_88
.LBB102_91:
	beq r15, r13, .LBB102_88
.LBB102_92:
	addi r1, r0, 64
	beq r15, r1, .LBB102_88
.LBB102_93:
	ldw r1, r26+0
	bge r20, r1, .LBB102_103
.LBB102_94:
	ldw r3, r18+0
.LBB102_95:
	add r4, r3, r20
	ldbu r4, r4+0
	bne r4, r24, .LBB102_98
.LBB102_96:
	addi r20, r20, 1
	bne r1, r20, .LBB102_95
.LBB102_97:
	add r4, r1, r0
	jal r0, .LBB102_104
.LBB102_98:
	addi r5, r0, 40
	bne r4, r5, .LBB102_103
.LBB102_99:
                                        # implicit-def: $r11
	add r5, r16, r0
	bge r20, r1, .LBB102_106
.LBB102_100:
	add r5, r3, r20
	ldbu r6, r5+0
	addi r4, r20, 1
	addi r7, r0, 41
	beq r6, r7, .LBB102_199
.LBB102_101:
	add r20, r4, r0
	blt r4, r1, .LBB102_100
.LBB102_102:
                                        # implicit-def: $r11
	add r5, r16, r0
	jal r0, .LBB102_106
.LBB102_103:
	add r4, r20, r0
.LBB102_104:
                                        # implicit-def: $r11
	add r5, r16, r0
	bge r4, r1, .LBB102_106
.LBB102_105:
	ldw r1, r18+0
	add r1, r1, r4
	ldbu r1, r1+0
	addi r3, r0, 61
	sne r5, r1, r3
	addi r11, r4, 1
.LBB102_106:
	beq r5, r17, .LBB102_116
.LBB102_107:
	beq r12, r17, .LBB102_159
.LBB102_108:
	ldw r1, r26+0
	add r11, fp, r0
	bge fp, r1, .LBB102_111
.LBB102_109:
	ldw r1, r18+0
	add r1, r1, r11
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r17, .LBB102_111
.LBB102_110:
	addi r11, r11, 1
	ldw r1, r26+0
	blt r11, r1, .LBB102_109
.LBB102_111:
	ble r11, fp, .LBB102_128
.LBB102_112:
	ldw r1, r26+0
	bge r11, r1, .LBB102_128
.LBB102_113:
	ldw r1, r18+0
	add r3, r1, r11
	ldbu r3, r3+0
	ldw r4, sp+56
	bne r3, r4, .LBB102_128
.LBB102_114:
	add r3, r1, fp
	jal r31, atoi
	ble r1, r17, .LBB102_196
.LBB102_115:
	add r15, r1, r0
	addi fp, r11, 1
	jal r0, .LBB102_129
.LBB102_116:
	ldw r1, sp+36
	ldw r4, r1+0
	lui r1, %hi(nml_casg)
	addi r1, r1, %lo(nml_casg)
	ldw r3, r1+0
	lui r12, %hi(nml_asg)
	addi r12, r12, %lo(nml_asg)
	blt r4, r3, .LBB102_118
.LBB102_117:
	addi r4, r0, 0
	seq r4, r3, r4
	slli r3, r3, 1
	sub r4, r0, r4
	xori r5, r3, 16
	and r4, r5, r4
	xor r4, r3, r4
	stw r1+0, r4
	ldw r3, r12+0
	addi r1, r0, 76
	mul r4, r4, r1
	jal r31, realloc
	stw r12+0, r1
.LBB102_118:
	ldw r1, r12+0
	ldw r5, sp+36
	ldw r3, r5+0
	addi r4, r3, 1
	stw r5+0, r4
	addi r4, r0, 76
	mul r3, r3, r4
	add r12, r1, r3
	ldw r1, r26+0
	addi r20, r0, 0
	add r21, fp, r0
	add r22, r20, r0
	bge fp, r1, .LBB102_165
.LBB102_119:
	addi r23, r0, 0
	add r22, r23, r0
.LBB102_120:
	ldw r1, r18+0
	add r1, r1, fp
	add r1, r1, r22
	ldbu r15, r1+0
	add r3, r15, r0
	jal r31, isalnum
	addi r3, r0, 1
	beq r1, r23, .LBB102_124
.LBB102_121:
	addi r1, r0, 0
	beq r3, r1, .LBB102_163
.LBB102_122:
	addi r1, r0, 30
	bgtu r22, r1, .LBB102_163
.LBB102_123:
	ldw r1, r18+0
	add r1, r1, fp
	add r1, r1, r22
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r22, 1
	add r4, r12, r22
	stb r4+0, r1
	ldw r1, r26+0
	add r21, fp, r3
	add r22, r3, r0
	blt r21, r1, .LBB102_120
	jal r0, .LBB102_164
.LBB102_124:
	addi r1, r15, -35
	addi r4, r0, 2
	bltu r1, r4, .LBB102_121
.LBB102_125:
	addi r1, r0, 64
	beq r15, r1, .LBB102_121
.LBB102_126:
	beq r15, r13, .LBB102_121
.LBB102_127:
	addi r3, r0, 0
	jal r0, .LBB102_121
.LBB102_128:
	add r15, r16, r0
.LBB102_129:
	add r14, r17, r0
	add r1, r17, r0
	add r11, fp, r0
	jal r0, .LBB102_134
.LBB102_130:
	addi r1, r0, 0
.LBB102_131:
	add r4, r11, r0
.LBB102_132:
	addi r11, r4, 1
	add r3, r20, r0
.LBB102_133:
	bne r3, r20, .LBB102_156
.LBB102_134:
	ldw r3, r26+0
	bge r11, r3, .LBB102_156
.LBB102_135:
	ldw r6, r18+0
	add r4, r6, r11
	ldb r5, r4+0
	addi r20, r0, 0
	beq r1, r20, .LBB102_139
.LBB102_136:
	bne r1, r5, .LBB102_131
.LBB102_137:
	addi r4, r11, 1
	bge r4, r3, .LBB102_130
.LBB102_138:
	add r3, r6, r4
	ldb r3, r3+0
	bne r1, r3, .LBB102_130
	jal r0, .LBB102_132
.LBB102_139:
	andi r21, r5, 255
	addi r1, r21, -32
	addi r6, r0, 9
	bgtu r1, r6, .LBB102_149
.LBB102_140:
	slli r1, r1, 2
	lui r6, %hi(.LJTI102_0)
	addi r6, r6, %lo(.LJTI102_0)
	add r1, r6, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB102_141:
	addi r11, r11, 1
	add r3, r20, r0
	add r1, r5, r0
	ldw r22, sp+64
	ldw r26, sp+40
	jal r0, .LBB102_133
.LBB102_142:
	addi r1, r0, 2
	blt r11, r1, .LBB102_149
.LBB102_143:
	addi r22, r11, 1
	bge r22, r3, .LBB102_149
.LBB102_144:
	ldbu r3, r4+-1
	jal r31, toupper
	addi r1, r1, -70
	addi r3, r0, -2
	bltu r1, r3, .LBB102_149
.LBB102_145:
	ldw r1, r18+0
	addi r26, r11, -2
	add r1, r1, r26
	ldbu r3, r1+0
	jal r31, isdigit
	addi r23, r0, 0
	bne r1, r23, .LBB102_147
.LBB102_146:
	ldw r1, r18+0
	add r1, r1, r26
	ldbu r1, r1+0
	addi r3, r0, 46
	bne r1, r3, .LBB102_149
.LBB102_147:
	ldw r1, r18+0
	add r1, r1, r22
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r23, .LBB102_149
.LBB102_148:
	ldw r1, r18+0
	add r1, r1, r11
	addi r3, r0, 43
	stb r1+0, r3
	jal r0, .LBB102_154
.LBB102_149:
	addi r1, r0, 0
	bgt r14, r1, .LBB102_154
.LBB102_150:
	addi r3, r0, 1
	ldw r22, sp+64
	ldw r26, sp+40
	beq r21, r24, .LBB102_133
.LBB102_151:
	beq r21, r19, .LBB102_133
	jal r0, .LBB102_155
.LBB102_152:
	addi r14, r14, 1
	jal r0, .LBB102_154
.LBB102_153:
	addi r14, r14, -1
.LBB102_154:
	ldw r22, sp+64
	ldw r26, sp+40
.LBB102_155:
	addi r11, r11, 1
	addi r3, r0, 0
	add r1, r3, r0
	jal r0, .LBB102_133
.LBB102_156:
	ldw r1, sp+44
	ldw r3, r1+0
	ldw r1, sp+68
	ldw r1, r1+0
	blt r3, r1, .LBB102_158
.LBB102_157:
	addi r3, r0, 0
	seq r3, r1, r3
	slli r1, r1, 1
	sub r3, r0, r3
	xori r4, r1, 64
	and r3, r4, r3
	xor r1, r1, r3
	ldw r3, sp+68
	stw r3+0, r1
	ldw r3, r27+0
	mul r4, r1, r28
	jal r31, realloc
	stw r27+0, r1
.LBB102_158:
	ldw r1, r27+0
	ldw r5, sp+44
	ldw r3, r5+0
	mul r4, r3, r28
	add r1, r1, r4
	stw r1+0, fp
	sub r4, r11, fp
	stw r1+4, r4
	stw r1+8, r15
	addi r1, r3, 1
	stw r5+0, r1
	ldw r1, r12+68
	addi r1, r1, 1
	stw r12+68, r1
	addi r14, r0, 0
	add r15, r14, r0
	ori  r1, r15, 2
	beq r1, r25, .LBB102_83
	jal r0, .LBB102_69
.LBB102_159:
	ldw r1, r22+0
	bne r1, r17, .LBB102_161
.LBB102_160:
	stw r22+0, r25
	ldw r3, sp+52
	ldw r4, sp+48
	add r5, r13, r0
	jal r31, strncpy
	ldw r1, sp+60
	stb r1+0, r17
.LBB102_161:
	add r11, fp, r0
	add r12, r17, r0
.LBB102_162:
	add r15, r16, r0
	ori  r1, r15, 2
	beq r1, r25, .LBB102_83
	jal r0, .LBB102_69
.LBB102_163:
	add r21, fp, r22
	jal r0, .LBB102_165
.LBB102_164:
	add r22, r3, r0
.LBB102_165:
	add r1, r12, r22
	stb r1+0, r20
	jal r0, .LBB102_167
.LBB102_166:
	addi r21, r21, 1
.LBB102_167:
	ldw r1, r26+0
	bge r21, r1, .LBB102_172
.LBB102_168:
	ldw r1, r18+0
	add r1, r1, r21
	ldbu r15, r1+0
	add r3, r15, r0
	jal r31, isalnum
	addi r3, r0, 0
	bne r1, r3, .LBB102_166
.LBB102_169:
	addi r1, r15, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB102_166
.LBB102_170:
	beq r15, r13, .LBB102_166
.LBB102_171:
	addi r1, r0, 64
	beq r15, r1, .LBB102_166
.LBB102_172:
	ldw r1, r26+0
	bge r21, r1, .LBB102_177
.LBB102_173:
	ldw r3, r18+0
.LBB102_174:
	add r4, r3, r21
	ldbu r4, r4+0
	bne r4, r24, .LBB102_177
.LBB102_175:
	addi r21, r21, 1
	bne r1, r21, .LBB102_174
.LBB102_176:
	add r21, r1, r0
.LBB102_177:
	addi r20, r0, 0
	stw r12+32, r20
	ldw r1, r18+0
	add r1, r1, r21
	ldbu r1, r1+0
	addi r3, r0, 40
	bne r1, r3, .LBB102_193
.LBB102_178:
	addi r22, r21, 1
	addi r21, r12, 36
	jal r0, .LBB102_181
.LBB102_179:
	ldw r3, sp+64
	ldw r1, r3+0
	addi r15, r0, 1
	beq r1, r20, .LBB102_191
.LBB102_180:
	addi r1, r0, 11
	bne r15, r1, .LBB102_192
.LBB102_181:
	ldw r1, r18+0
	addi r22, r22, -1
.LBB102_182:
	add r3, r1, r22
	ldbu r3, r3+1
	addi r22, r22, 1
	beq r3, r24, .LBB102_182
.LBB102_183:
	add r3, r1, r22
	addi r4, sp, 72
	addi r15, r0, 10
	add r5, r15, r0
	jal r31, strtol
	ldw r4, sp+72
	ldw r3, r18+0
	add r5, r3, r22
	beq r5, r4, .LBB102_179
.LBB102_184:
	ldw r5, r12+32
	addi r6, r0, 6
	bgt r5, r6, .LBB102_186
.LBB102_185:
	addi r6, r5, 1
	stw r12+32, r6
	slli r5, r5, 2
	add r5, r21, r5
	stw r5+0, r1
.LBB102_186:
	sub r22, r4, r3
.LBB102_187:
	add r1, r3, r22
	ldbu r1, r1+0
	bne r1, r24, .LBB102_189
.LBB102_188:
	addi r22, r22, 1
	jal r0, .LBB102_187
.LBB102_189:
	bne r1, r19, .LBB102_180
.LBB102_190:
	addi r22, r22, 1
	addi r15, r0, 11
	jal r0, .LBB102_180
.LBB102_191:
	addi r1, r0, 2
	stw r3+0, r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.97)
	addi r4, r4, %lo(.L.str.97)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+60
	stb r1+0, r20
	jal r0, .LBB102_180
.LBB102_192:
	addi r1, r0, 10
	bne r15, r1, .LBB102_194
.LBB102_193:
	ldw r1, sp+44
	ldw r1, r1+0
	stw r12+64, r1
	stw r12+68, r20
	stw r12+72, r20
	add r14, r16, r0
	add r15, r25, r0
	jal r0, .LBB102_195
.LBB102_194:
	add r11, fp, r0
.LBB102_195:
	ldw r22, sp+64
	ori  r1, r15, 2
	beq r1, r25, .LBB102_83
	jal r0, .LBB102_69
.LBB102_196:
	ldw r1, r22+0
	bne r1, r17, .LBB102_198
.LBB102_197:
	stw r22+0, r25
	ldw r3, sp+52
	ldw r4, sp+32
	add r5, r13, r0
	jal r31, strncpy
	ldw r1, sp+60
	stb r1+0, r17
.LBB102_198:
	add r11, fp, r0
	jal r0, .LBB102_162
.LBB102_199:
	bge r4, r1, .LBB102_104
.LBB102_200:
	addi r3, r0, 0
.LBB102_201:
	add r4, r5, r3
	ldbu r4, r4+1
	bne r4, r24, .LBB102_203
.LBB102_202:
	addi r3, r3, 1
	add r4, r20, r3
	addi r4, r4, 1
	blt r4, r1, .LBB102_201
	jal r0, .LBB102_104
.LBB102_203:
	add r3, r20, r3
	addi r4, r3, 1
	jal r0, .LBB102_104
.Lfunc_end102:
	.size	f77_nml_begin, .Lfunc_end102-f77_nml_begin
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI102_0,@object
.LJTI102_0:
	.word	.LBB102_142
	.word	.LBB102_149
	.word	.LBB102_141
	.word	.LBB102_149
	.word	.LBB102_149
	.word	.LBB102_149
	.word	.LBB102_149
	.word	.LBB102_141
	.word	.LBB102_152
	.word	.LBB102_153
	.size	.LJTI102_0, 40
                                        # -- End function
	.text
	.globl	f77_nml_dim                     # -- Begin function f77_nml_dim
	.p2align	2
	.type	f77_nml_dim,@function
f77_nml_dim:                            # @f77_nml_dim
# %bb.0:
	lui r1, %hi(nml_ndim)
	addi r1, r1, %lo(nml_ndim)
	ldw r5, r1+0
	addi r6, r0, 6
	bgt r5, r6, .LBB103_2
.LBB103_1:
	slli r6, r5, 2
	lui r7, %hi(nml_lo)
	addi r7, r7, %lo(nml_lo)
	add r7, r6, r7
	stw r7+0, r3
	lui r3, %hi(nml_ext)
	addi r3, r3, %lo(nml_ext)
	add r3, r6, r3
	stw r3+0, r4
	addi r3, r5, 1
	stw r1+0, r3
.LBB103_2:
	jalr r0, r31, 0
.Lfunc_end103:
	.size	f77_nml_dim, .Lfunc_end103-f77_nml_dim
                                        # -- End function
	.globl	f77_nml_item                    # -- Begin function f77_nml_item
	.p2align	2
	.type	f77_nml_item,@function
f77_nml_item:                           # @f77_nml_item
# %bb.0:
	addi sp, sp, -1376
	stw sp+0, lr
	stw sp+4, fp
	stw sp+1372, r11
	stw sp+1368, r12
	stw sp+1364, r13
	stw sp+1360, r14
	stw sp+1356, r15
	stw sp+1352, r16
	stw sp+1348, r17
	stw sp+1344, r18
	stw sp+1340, r19
	stw sp+1336, r20
	stw sp+1332, r21
	stw sp+1328, r22
	stw sp+1324, r23
	stw sp+1320, r24
	stw sp+1316, r25
	stw sp+1312, r26
	stw sp+1308, r27
	stw sp+1304, r28
	stw sp+88, r8
	add r12, r7, r0
	add r18, r6, r0
	add r14, r5, r0
	add r15, r3, r0
	addi r1, r0, 31
	slt r1, r4, r1
	sub r1, r0, r1
	xori r3, r4, 31
	and r1, r3, r1
	xori r17, r1, 31
	addi r19, r0, 1
	addi r11, sp, 248
	addi r23, r0, 0
	blt r4, r19, .LBB104_3
.LBB104_1:
	addi r13, r0, 0
	add r16, r17, r0
.LBB104_2:
	ldbu r3, r15+0
	jal r31, toupper
	stb r11+0, r1
	addi r16, r16, -1
	addi r11, r11, 1
	addi r15, r15, 1
	bne r16, r13, .LBB104_2
.LBB104_3:
	addi r1, sp, 248
	add r1, r1, r17
	stb r1+0, r23
	addi r1, r0, 6
	stw sp+108, r1
	bgtu r14, r1, .LBB104_58
.LBB104_4:
	sll r1, r19, r14
	andi r1, r1, 70
	beq r1, r23, .LBB104_58
.LBB104_5:
	addi r28, r0, 4
.LBB104_6:
	lui r1, %hi(nml_reading)
	addi r1, r1, %lo(nml_reading)
	ldw r1, r1+0
	lui r3, %hi(fio_listed)
	addi r3, r3, %lo(fio_listed)
	stw sp+100, r3
	lui r3, %hi(fio_desc)
	addi r3, r3, %lo(fio_desc)
	stw sp+96, r3
	lui r3, %hi(fio_rep)
	addi r3, r3, %lo(fio_rep)
	stw sp+92, r3
	lui r3, %hi(fio_ibuf)
	addi r3, r3, %lo(fio_ibuf)
	stw sp+104, r3
	beq r1, r23, .LBB104_9
.LBB104_7:
	stw sp+60, r19
	lui r26, %hi(nml_status)
	addi r26, r26, %lo(nml_status)
	ldw r1, r26+0
	lui r13, %hi(nml_ndim)
	addi r13, r13, %lo(nml_ndim)
	ldw r17, sp+88
	beq r1, r23, .LBB104_60
.LBB104_8:
	stw r13+0, r23
	jal r0, .LBB104_57
.LBB104_9:
	addi r15, sp, 144
	addi r4, sp, 248
	add r3, r15, r0
	add r5, r17, r0
	jal r31, memcpy
	add r1, r15, r17
	addi r3, r0, 61
	stb r1+0, r3
	lui r21, %hi(nml_npend)
	addi r21, r21, %lo(nml_npend)
	ldw r1, r21+0
	addi r19, r0, 0
	lui r27, %hi(nml_pend)
	addi r27, r27, %lo(nml_pend)
	blt r1, r19, .LBB104_11
.LBB104_10:
	addi r4, r1, 1
	stw r21+0, r4
	add r1, r1, r27
	addi r3, r0, 44
	stb r1+0, r3
	add r3, r27, r0
	jal r31, nml_put
	addi r1, r0, -1
	stw r21+0, r1
.LBB104_11:
	addi r4, r17, 1
	addi r3, sp, 144
	jal r31, nml_put
	addi r20, r0, 1
	ldw r17, sp+88
	blt r17, r20, .LBB104_56
.LBB104_12:
	addi r24, r0, 5
	seq r3, r14, r24
	addi r1, r0, -2
	and r25, r14, r1
	stw sp+60, r3
	sub r1, r0, r3
	andi r1, r1, 16
	addi r1, r1, 33
	stw sp+52, r1
	addi fp, r0, 3
	addi r26, r0, 23
	addi r1, r0, 44
	stw sp+84, r1
	addi r16, r0, -1
	addi r23, r0, 2
	addi r1, r0, 15
	stw sp+72, r1
	lui r1, %hi(.L.str.20)
	addi r1, r1, %lo(.L.str.20)
	stw sp+68, r1
	addi r22, r0, 11
	add r13, r18, r0
	add r15, r19, r0
	stw sp+64, r24
	stw sp+56, r25
	stw sp+48, r26
	stw sp+80, r16
	stw sp+76, r23
	jal r0, .LBB104_15
.LBB104_13:
	addi r4, sp, 184
	add r3, r27, r0
	ldw r11, sp+72
	add r5, r11, r0
	jal r31, memcpy
	stw r21+0, r11
.LBB104_14:
	addi r15, r15, 1
	add r13, r13, r28
	beq r15, r17, .LBB104_56
.LBB104_15:
	mul r1, r15, r28
	add r11, r18, r1
	beq r14, fp, .LBB104_27
.LBB104_16:
	beq r14, r23, .LBB104_25
.LBB104_17:
	bne r14, r20, .LBB104_30
.LBB104_18:
	addi r3, sp, 184
	add r4, r22, r0
	add r5, r20, r0
	ldw r6, sp+68
	jal r31, f77_wr_begin_int
	ldw r3, r11+0
	jal r31, f77_wr_i
	ldw r1, sp+100
	ldw r1, r1+0
	bne r1, r19, .LBB104_22
.LBB104_19:
	ldw r1, sp+96
	ldw r1, r1+0
	addi r11, r0, 0
	beq r1, r11, .LBB104_21
.LBB104_20:
	ldw r1, sp+92
	ldw r1, r1+0
	bgt r1, r11, .LBB104_22
.LBB104_21:
	lui r16, %hi(fio_draining)
	addi r16, r16, %lo(fio_draining)
	addi r1, r0, 1
	stb r16+0, r1
	ldw r1, sp+96
	stw r1+0, r11
	ldw r1, sp+92
	stw r1+0, r11
	jal r31, fio_next_desc
	stb r16+0, r11
	ldw r16, sp+80
.LBB104_22:
	jal r31, fio_flush_line
	addi r3, r0, 0
	ldw r1, sp+104
	stw r1+0, r3
	stb sp+195, r3
	ldw r1, r21+0
	blt r1, r3, .LBB104_24
.LBB104_23:
	addi r4, r1, 1
	stw r21+0, r4
	add r1, r1, r27
	ldw r3, sp+84
	stb r1+0, r3
	add r3, r27, r0
	jal r31, nml_put
	stw r21+0, r16
.LBB104_24:
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r4, sp, 184
	add r5, r22, r0
	jal r31, memcpy
	stw r21+0, r22
	jal r0, .LBB104_14
.LBB104_25:
	ldw r1, r11+0
	fcvt.d.s r6, r1
	addi r3, sp, 184
	add r5, r6, r0
	add r6, r7, r0
	add r4, r19, r0
	jal r31, nml_real
	ldw r1, r21+0
	blt r1, r19, .LBB104_13
.LBB104_26:
	addi r4, r1, 1
	stw r21+0, r4
	add r1, r1, r27
	ldw r3, sp+84
	stb r1+0, r3
	add r3, r27, r0
	jal r31, nml_put
	stw r21+0, r16
	jal r0, .LBB104_13
.LBB104_27:
	ldw r6, r11+4
	ldw r5, r11+0
	addi r3, sp, 184
	add r4, r20, r0
	jal r31, nml_real
	ldw r1, r21+0
	blt r1, r19, .LBB104_29
.LBB104_28:
	addi r4, r1, 1
	stw r21+0, r4
	add r1, r1, r27
	ldw r3, sp+84
	stb r1+0, r3
	add r3, r27, r0
	jal r31, nml_put
	stw r21+0, r16
.LBB104_29:
	addi r4, sp, 184
	add r3, r27, r0
	add r5, r26, r0
	jal r31, memcpy
	stw r21+0, r26
	jal r0, .LBB104_14
.LBB104_30:
	addi r1, r0, 4
	bne r25, r1, .LBB104_34
.LBB104_31:
	bne r14, r24, .LBB104_38
.LBB104_32:
	ldw r7, r11+4
	ldw r6, r11+0
	beq r14, r24, .LBB104_39
.LBB104_33:
	ldw r1, r11+4
	fcvt.d.s r16, r1
	jal r0, .LBB104_40
.LBB104_34:
	ldw r1, sp+108
	bne r14, r1, .LBB104_45
.LBB104_35:
	ldw r11, r11+0
	addi r16, r0, 0
	ldw r1, r21+0
	blt r1, r16, .LBB104_37
.LBB104_36:
	addi r4, r1, 1
	stw r21+0, r4
	add r1, r1, r27
	ldw r3, sp+84
	stb r1+0, r3
	add r3, r27, r0
	jal r31, nml_put
	ldw r1, sp+80
	stw r21+0, r1
.LBB104_37:
	seq r1, r11, r16
	lui r3, %hi(.L.str.21)
	addi r3, r3, %lo(.L.str.21)
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r4, r1, r3
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r23, r0, 1
	add r5, r23, r0
	jal r31, memcpy
	stw r21+0, r23
	ldw r16, sp+80
	jal r0, .LBB104_55
.LBB104_38:
	ldw r1, r11+0
	fcvt.d.s r6, r1
	bne r14, r24, .LBB104_33
.LBB104_39:
	ldw r17, r11+12
	ldw r16, r11+8
.LBB104_40:
	addi r1, r0, 40
	stb sp+280, r1
	addi r23, sp, 112
	add r3, r23, r0
	add r5, r6, r0
	add r6, r7, r0
	ldw r11, sp+60
	add r4, r11, r0
	jal r31, nml_real
	addi r24, sp, 280
	addi r25, r0, 1
	add r3, r24, r0
	add r4, r25, r0
	add r5, r23, r0
	jal r31, nml_trim_cat
	addi r26, r1, 1
	add r1, r24, r1
	ldw r3, sp+84
	stb r1+0, r3
	add r3, r23, r0
	add r5, r16, r0
	add r6, r17, r0
	add r4, r11, r0
	jal r31, nml_real
	add r3, r24, r0
	add r4, r26, r0
	add r5, r23, r0
	jal r31, nml_trim_cat
	addi r23, r1, 1
	add r1, r24, r1
	addi r3, r0, 41
	stb r1+0, r3
	ldw r11, sp+52
	sub r24, r11, r23
	blt r24, r25, .LBB104_42
.LBB104_41:
	addi r25, sp, 280
	add r3, r25, r24
	add r4, r25, r0
	add r5, r23, r0
	jal r31, memmove
	addi r4, r0, 32
	add r3, r25, r0
	add r5, r24, r0
	jal r31, memset
	add r23, r11, r0
.LBB104_42:
	ldw r17, sp+88
	ldw r25, sp+56
	ldw r26, sp+48
	ldw r16, sp+80
	ldw r1, r21+0
	addi r3, r0, 0
	ldw r24, sp+64
	blt r1, r3, .LBB104_44
.LBB104_43:
	addi r4, r1, 1
	stw r21+0, r4
	add r1, r1, r27
	ldw r3, sp+84
	stb r1+0, r3
	add r3, r27, r0
	jal r31, nml_put
	stw r21+0, r16
.LBB104_44:
	addi r1, r0, 1022
	slt r1, r23, r1
	sub r1, r0, r1
	xori r3, r23, 1022
	and r1, r3, r1
	xori r23, r1, 1022
	jal r0, .LBB104_54
.LBB104_45:
	addi r1, r0, 39
	stb sp+280, r1
	add r11, r20, r0
	blt r12, r20, .LBB104_51
.LBB104_46:
	addi r11, r0, 1
	addi r3, r0, 0
.LBB104_47:
	add r4, r13, r3
	ldbu r5, r4+0
	addi r4, r11, 1
	addi r6, sp, 280
	add r6, r6, r11
	stb r6+0, r5
	bne r5, r1, .LBB104_49
.LBB104_48:
	addi r11, r11, 2
	addi r5, sp, 280
	add r4, r5, r4
	stb r4+0, r1
	addi r3, r3, 1
	blt r3, r12, .LBB104_50
	jal r0, .LBB104_51
.LBB104_49:
	add r11, r4, r0
	addi r3, r3, 1
	bge r3, r12, .LBB104_51
.LBB104_50:
	addi r4, r0, 1021
	blt r11, r4, .LBB104_47
.LBB104_51:
	addi r3, sp, 280
	add r3, r3, r11
	stb r3+0, r1
	ldw r1, r21+0
	addi r3, r0, 0
	blt r1, r3, .LBB104_53
.LBB104_52:
	addi r4, r1, 1
	stw r21+0, r4
	add r1, r1, r27
	ldw r3, sp+84
	stb r1+0, r3
	add r3, r27, r0
	jal r31, nml_put
	stw r21+0, r16
.LBB104_53:
	addi r1, r0, 1021
	slt r1, r11, r1
	sub r1, r0, r1
	xori r3, r11, 1021
	and r1, r3, r1
	xori r1, r1, 1021
	addi r23, r1, 1
.LBB104_54:
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r4, sp, 280
	add r5, r23, r0
	jal r31, memcpy
	stw r21+0, r23
.LBB104_55:
	ldw r23, sp+76
	jal r0, .LBB104_14
.LBB104_56:
	lui r1, %hi(nml_ndim)
	addi r1, r1, %lo(nml_ndim)
	stw r1+0, r19
.LBB104_57:
	ldw r28, sp+1304
	ldw r27, sp+1308
	ldw r26, sp+1312
	ldw r25, sp+1316
	ldw r24, sp+1320
	ldw r23, sp+1324
	ldw r22, sp+1328
	ldw r21, sp+1332
	ldw r20, sp+1336
	ldw r19, sp+1340
	ldw r18, sp+1344
	ldw r17, sp+1348
	ldw r16, sp+1352
	ldw r15, sp+1356
	ldw r14, sp+1360
	ldw r13, sp+1364
	ldw r12, sp+1368
	ldw r11, sp+1372
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 1376
	jalr r0, r31, 0
.LBB104_58:
	addi r1, r14, -3
	addi r3, r0, 2
	bgeu r1, r3, .LBB104_137
.LBB104_59:
	addi r28, r0, 8
	jal r0, .LBB104_6
.LBB104_60:
	lui r15, %hi(nml_nasg)
	addi r15, r15, %lo(nml_nasg)
	ldw r1, r15+0
	ldw r3, sp+60
	blt r1, r3, .LBB104_8
.LBB104_61:
	ldw r1, r26+0
	bne r1, r23, .LBB104_8
.LBB104_62:
	addi r1, r0, 36
	stw sp+76, r1
	lui r1, %hi(nml_asg)
	addi r1, r1, %lo(nml_asg)
	stw sp+72, r1
	addi r1, r0, 76
	stw sp+68, r1
	addi r1, r0, 13
	stw sp+64, r1
	lui r1, %hi(nml_lo)
	addi r1, r1, %lo(nml_lo)
	stw sp+32, r1
	addi r1, r0, 2
	stw sp+84, r1
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+40, r1
	lui r1, %hi(.L.str.24)
	addi r1, r1, %lo(.L.str.24)
	stw sp+28, r1
	addi r1, r0, 95
	stw sp+24, r1
	lui r1, %hi(nml_err+95)
	addi r1, r1, %lo(nml_err+95)
	stw sp+80, r1
	lui r1, %hi(nml_ext)
	addi r1, r1, %lo(nml_ext)
	stw sp+56, r1
	addi r1, r0, 11
	stw sp+36, r1
	lui r1, %hi(.L.str.23)
	addi r1, r1, %lo(.L.str.23)
	stw sp+20, r1
	add r24, r23, r0
	stw sp+48, r13
	stw sp+44, r15
.LBB104_63:
	ldw r1, sp+72
	ldw r11, r1+0
	ldw r1, sp+68
	mul r1, r24, r1
	add r19, r11, r1
	addi r4, sp, 248
	add r3, r19, r0
	jal r31, strcmp
	ldw r3, sp+64
	beq r1, r23, .LBB104_67
.LBB104_64:
	addi r1, r0, 11
	beq r3, r1, .LBB104_8
.LBB104_65:
	addi r24, r24, 1
	ldw r1, r15+0
	bge r24, r1, .LBB104_8
.LBB104_66:
	ldw r1, r26+0
	ldw r3, sp+76
	addi r3, r3, 76
	stw sp+76, r3
	addi r3, r0, 0
	beq r1, r3, .LBB104_63
	jal r0, .LBB104_8
.LBB104_67:
	ldw r3, sp+60
	stw r19+72, r3
	ldw r1, r19+32
	add r25, r23, r0
	blt r1, r3, .LBB104_82
.LBB104_68:
	ldw r3, r13+0
	bne r1, r3, .LBB104_79
.LBB104_69:
	ldw r1, r19+32
	add r25, r23, r0
	ldw r20, sp+60
	ldw r21, sp+32
	ldw r22, sp+40
	ldw r27, sp+28
	ldw fp, sp+24
	blt r1, r20, .LBB104_78
.LBB104_70:
	ldw r1, sp+76
	add r11, r11, r1
	add r13, r23, r0
	add r15, r23, r0
	add r16, r20, r0
	add r25, r23, r0
.LBB104_71:
	add r1, r11, r13
	ldw r1, r1+0
	add r3, r13, r21
	ldw r3, r3+0
	sub r1, r1, r3
	blt r1, r23, .LBB104_74
.LBB104_72:
	ldw r3, sp+56
	add r3, r13, r3
	ldw r3, r3+0
	bge r1, r3, .LBB104_74
.LBB104_73:
	mul r1, r1, r16
	add r25, r1, r25
	mul r16, r3, r16
	add r1, r20, r0
	bne r1, r23, .LBB104_77
	jal r0, .LBB104_78
.LBB104_74:
	ldw r1, r26+0
	beq r1, r23, .LBB104_76
.LBB104_75:
	add r1, r23, r0
	bne r1, r23, .LBB104_77
	jal r0, .LBB104_78
.LBB104_76:
	ldw r1, sp+84
	stw r26+0, r1
	add r3, r22, r0
	add r4, r27, r0
	add r5, fp, r0
	jal r31, strncpy
	ldw r1, sp+80
	stb r1+0, r23
	add r1, r23, r0
	beq r1, r23, .LBB104_78
.LBB104_77:
	addi r15, r15, 1
	ldw r1, r19+32
	addi r13, r13, 4
	blt r15, r1, .LBB104_71
.LBB104_78:
	ldw r1, r26+0
	seq r1, r1, r23
	addi r3, r1, -1
	andi r3, r3, 11
	ldw r13, sp+48
	ldw r15, sp+44
	jal r0, .LBB104_81
.LBB104_79:
	ldw r4, r26+0
	add r1, r23, r0
	ldw r3, sp+36
	add r25, r23, r0
	bne r4, r23, .LBB104_81
.LBB104_80:
	ldw r1, sp+84
	stw r26+0, r1
	addi r5, r0, 95
	ldw r3, sp+40
	ldw r4, sp+20
	jal r31, strncpy
	addi r1, r0, 0
	ldw r3, sp+80
	stb r3+0, r1
	ldw r3, sp+36
	add r25, r1, r0
.LBB104_81:
	beq r1, r23, .LBB104_64
.LBB104_82:
	ldw r1, r19+68
	addi fp, r0, 1
	add r3, r23, r0
	blt r1, fp, .LBB104_64
.LBB104_83:
	ldw r1, r26+0
	addi r4, r0, 0
	add r3, r23, r0
	stw sp+52, r4
	bne r1, r4, .LBB104_64
.LBB104_84:
	ldw r15, r19+64
.LBB104_85:
	lui r13, %hi(nml_val)
	addi r13, r13, %lo(nml_val)
	ldw r1, r13+0
	addi r3, r0, 12
	mul r16, r15, r3
	add r3, r1, r16
	ldw r1, r3+8
	blt r1, fp, .LBB104_134
.LBB104_86:
	bge r25, r17, .LBB104_131
.LBB104_87:
	xor r1, r25, r17
	sgt r4, r25, r17
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r17, r1
	sub r27, r1, r25
	mul r1, r28, r25
	add r17, r18, r1
	addi r11, r0, 0
.LBB104_88:
	ldw r1, r3+4
	blt r1, fp, .LBB104_129
.LBB104_89:
	mul r4, r25, r28
	add r20, r18, r4
	lui r4, %hi(nml_text)
	addi r4, r4, %lo(nml_text)
	ldw r4, r4+0
	ldw r3, r3+0
	add r4, r4, r3
	addi r3, r0, 7
	beq r14, r3, .LBB104_94
.LBB104_90:
	ldw r3, sp+108
	bne r14, r3, .LBB104_100
.LBB104_91:
	ldbu r3, r4+0
	addi r5, r0, 46
	seq r3, r3, r5
	ble r1, r3, .LBB104_121
.LBB104_92:
	add r21, r4, r3
	ldbu r3, r21+0
	jal r31, toupper
	addi r3, r0, 84
	bne r1, r3, .LBB104_119
.LBB104_93:
	stw r20+0, fp
	jal r0, .LBB104_129
.LBB104_94:
	ldbu r5, r4+0
	addi r3, r0, 34
	beq r5, r3, .LBB104_96
.LBB104_95:
	addi r3, r0, 39
	bne r5, r3, .LBB104_117
.LBB104_96:
	addi r3, r0, 0
	bne r1, fp, .LBB104_110
.LBB104_97:
	ble r12, r3, .LBB104_129
.LBB104_98:
	add r1, r17, r3
	sub r3, r12, r3
.LBB104_99:
	addi r4, r0, 32
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	addi r4, r0, 0
	bne r3, r4, .LBB104_99
	jal r0, .LBB104_129
.LBB104_100:
	addi r3, r0, 1023
	slt r3, r1, r3
	sub r3, r0, r3
	xori r1, r1, 1023
	and r1, r1, r3
	xori r21, r1, 1023
	addi r22, sp, 280
	add r3, r22, r0
	add r5, r21, r0
	jal r31, memcpy
	add r1, r22, r21
	addi r22, r0, 0
	stb r1+0, r22
	lui r1, %hi(fio_uend_init)
	addi r1, r1, %lo(fio_uend_init)
	ldbu r3, r1+0
	bne r3, r22, .LBB104_104
.LBB104_101:
	addi r3, r0, -400
.LBB104_102:
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	add r4, r3, r4
	addi r5, r0, -1
	stw r4+0, r5
	addi r3, r3, 4
	bne r3, r22, .LBB104_102
.LBB104_103:
	stb r1+0, fp
.LBB104_104:
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	ldw r1, r1+0
	lui r3, %hi(fio_in)
	addi r3, r3, %lo(fio_in)
	stw r3+0, r1
	lui r1, %hi(fio_unit)
	addi r1, r1, %lo(fio_unit)
	addi r3, r0, -1
	stw r1+0, r3
	addi r1, sp, 280
	ldw r4, sp+104
	stw r4+0, r1
	lui r1, %hi(fio_ilen)
	addi r1, r1, %lo(fio_ilen)
	stw r1+0, r21
	lui r1, %hi(fio_inrec)
	addi r1, r1, %lo(fio_inrec)
	stw r1+0, fp
	lui r1, %hi(fio_irec)
	addi r1, r1, %lo(fio_irec)
	stw r1+0, r22
	lui r1, %hi(fio_bz)
	addi r1, r1, %lo(fio_bz)
	stw r1+0, r22
	lui r1, %hi(fio_l_nulls)
	addi r1, r1, %lo(fio_l_nulls)
	stw r1+0, r22
	lui r1, %hi(fio_l_after)
	addi r1, r1, %lo(fio_l_after)
	stb r1+0, r22
	lui r21, %hi(fio_reading)
	addi r21, r21, %lo(fio_reading)
	stb r21+0, fp
	lui r1, %hi(fio_ldone)
	addi r1, r1, %lo(fio_ldone)
	stb r1+0, r22
	lui r1, %hi(fio_l_pending)
	addi r1, r1, %lo(fio_l_pending)
	stw r1+0, r22
	lui r1, %hi(fio_gdepth)
	addi r1, r1, %lo(fio_gdepth)
	stw r1+0, r22
	lui r1, %hi(fio_scale)
	addi r1, r1, %lo(fio_scale)
	stw r1+0, r22
	ldw r1, sp+92
	stw r1+0, r22
	ldw r1, sp+96
	stw r1+0, r22
	lui r1, %hi(fio_len)
	addi r1, r1, %lo(fio_len)
	stw r1+0, r22
	ldw r1, sp+100
	stw r1+0, fp
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
	lui r4, %hi(fio_fmt)
	addi r4, r4, %lo(fio_fmt)
	stw r4+0, r1
	lui r1, %hi(fio_pos)
	addi r1, r1, %lo(fio_pos)
	stw r1+0, r22
	lui r1, %hi(fio_revert)
	addi r1, r1, %lo(fio_revert)
	stw r1+0, r3
	lui r1, %hi(fio_since_data)
	addi r1, r1, %lo(fio_since_data)
	stb r1+0, r22
	jal r31, fio_getrec
	beq r1, r22, .LBB104_107
.LBB104_105:
	stb r21+0, r22
	ldw r1, r26+0
	bne r1, r22, .LBB104_129
.LBB104_106:
	ldw r1, sp+84
	stw r26+0, r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.105)
	addi r4, r4, %lo(.L.str.105)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+80
	stb r1+0, r22
	jal r0, .LBB104_129
.LBB104_107:
	addi r1, r14, -1
	addi r3, r0, 3
	bgtu r1, r3, .LBB104_127
.LBB104_108:
	slli r1, r1, 2
	lui r3, %hi(.LJTI104_0)
	addi r3, r3, %lo(.LJTI104_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB104_109:
	add r3, r20, r0
	jal r31, f77_rd_i
	jal r0, .LBB104_128
.LBB104_110:
	add r6, fp, r0
	jal r0, .LBB104_112
.LBB104_111:
	addi r6, r6, 1
	bge r6, r1, .LBB104_97
.LBB104_112:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r5, .LBB104_115
.LBB104_113:
	addi r6, r6, 1
	bge r6, r1, .LBB104_97
.LBB104_114:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r5, .LBB104_97
.LBB104_115:
	bge r3, r12, .LBB104_111
.LBB104_116:
	add r7, r4, r6
	ldbu r7, r7+0
	addi r8, r3, 1
	add r3, r20, r3
	stb r3+0, r7
	add r3, r8, r0
	jal r0, .LBB104_111
.LBB104_117:
	ldw r1, r26+0
	addi r20, r0, 0
	bne r1, r20, .LBB104_129
.LBB104_118:
	ldw r1, sp+84
	stw r26+0, r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.104)
	addi r4, r4, %lo(.L.str.104)
	jal r0, .LBB104_123
.LBB104_119:
	ldbu r3, r21+0
	jal r31, toupper
	addi r3, r0, 70
	bne r1, r3, .LBB104_121
.LBB104_120:
	addi r1, r0, 0
	stw r20+0, r1
	jal r0, .LBB104_129
.LBB104_121:
	ldw r1, r26+0
	addi r20, r0, 0
	bne r1, r20, .LBB104_129
.LBB104_122:
	ldw r1, sp+84
	stw r26+0, r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.103)
	addi r4, r4, %lo(.L.str.103)
.LBB104_123:
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+80
	stb r1+0, r20
	jal r0, .LBB104_129
.LBB104_124:
	add r3, r20, r0
	jal r31, f77_rd_d
	jal r0, .LBB104_128
.LBB104_125:
	add r3, r20, r0
	jal r31, f77_rd_c
	jal r0, .LBB104_128
.LBB104_126:
	add r3, r20, r0
	jal r31, f77_rd_r
	jal r0, .LBB104_128
.LBB104_127:
	add r3, r20, r0
	jal r31, f77_rd_z
.LBB104_128:
	stb r21+0, r22
	ldw r1, sp+104
	stw r1+0, r22
.LBB104_129:
	addi r25, r25, 1
	addi r11, r11, 1
	ldw r1, r13+0
	add r3, r1, r16
	ldw r1, r3+8
	bge r11, r1, .LBB104_133
.LBB104_130:
	add r17, r17, r28
	bne r11, r27, .LBB104_88
.LBB104_131:
	ldw r1, r26+0
	addi r11, r0, 0
	bne r1, r11, .LBB104_133
.LBB104_132:
	ldw r1, sp+84
	stw r26+0, r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+80
	stb r1+0, r11
.LBB104_133:
	ldw r17, sp+88
.LBB104_134:
	addi r15, r15, 1
	ldw r1, r19+64
	ldw r3, r19+68
	add r1, r3, r1
	bge r15, r1, .LBB104_136
.LBB104_135:
	ldw r1, r26+0
	addi r3, r0, 0
	beq r1, r3, .LBB104_85
.LBB104_136:
	ldw r3, sp+52
	ldw r13, sp+48
	ldw r15, sp+44
	jal r0, .LBB104_64
.LBB104_137:
	addi r1, r0, 5
	seq r1, r14, r1
	sub r1, r0, r1
	xori r3, r12, 16
	and r1, r3, r1
	xor r28, r12, r1
	jal r0, .LBB104_6
.Lfunc_end104:
	.size	f77_nml_item, .Lfunc_end104-f77_nml_item
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI104_0,@object
.LJTI104_0:
	.word	.LBB104_109
	.word	.LBB104_126
	.word	.LBB104_124
	.word	.LBB104_125
	.size	.LJTI104_0, 16
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function nml_put
	.type	nml_put,@function
nml_put:                                # @nml_put
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
	add r11, r3, r0
	lui r14, %hi(nml_len)
	addi r14, r14, %lo(nml_len)
	ldw r13, r14+0
	addi r1, r0, 2
	lui r15, %hi(nml_line)
	addi r15, r15, %lo(nml_line)
	blt r13, r1, .LBB105_7
.LBB105_1:
	add r1, r13, r4
	lui r3, %hi(nml_width)
	addi r3, r3, %lo(nml_width)
	ldbu r3, r3+0
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB105_7
.LBB105_2:
	add r16, r4, r0
	lui r1, %hi(nml_unit)
	addi r1, r1, %lo(nml_unit)
	ldw r3, r1+0
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	jal r31, f77_wr_begin
	lui r12, %hi(nml_line)
	addi r12, r12, %lo(nml_line)
	add r3, r12, r0
	add r4, r13, r0
	jal r31, f77_wr_a
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	addi r13, r0, 0
	bne r1, r13, .LBB105_6
.LBB105_3:
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	ldw r4, r1+0
	lui r3, %hi(fio_rep)
	addi r3, r3, %lo(fio_rep)
	beq r4, r13, .LBB105_5
.LBB105_4:
	ldw r4, r3+0
	bgt r4, r13, .LBB105_6
.LBB105_5:
	lui r17, %hi(fio_draining)
	addi r17, r17, %lo(fio_draining)
	addi r4, r0, 1
	stb r17+0, r4
	stw r1+0, r13
	stw r3+0, r13
	jal r31, fio_next_desc
	stb r17+0, r13
.LBB105_6:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r13
	addi r1, r0, 32
	stb r12+0, r1
	addi r1, r0, 1
	stw r14+0, r1
	add r4, r16, r0
.LBB105_7:
	ldw r1, r14+0
	add r3, r1, r4
	addi r5, r0, 1024
	sgt r3, r3, r5
	sub r5, r5, r1
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r12, r4, r3
	add r3, r1, r15
	add r4, r11, r0
	add r5, r12, r0
	jal r31, memcpy
	ldw r1, r14+0
	add r1, r12, r1
	stw r14+0, r1
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
.Lfunc_end105:
	.size	nml_put, .Lfunc_end105-nml_put
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function nml_real
	.type	.LCPI106_0,@object
.LCPI106_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI106_0, 8
	.text
	.p2align	2
	.type	nml_real,@function
nml_real:                               # @nml_real
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
	add r17, r6, r0
	add r16, r5, r0
	add r11, r3, r0
	addi r15, r0, 0
	seq r1, r4, r15
	sub r1, r0, r1
	andi r1, r1, 24
	xori r12, r1, 23
	lui r1, %hi(.LCPI106_0)
	addi r1, r1, %lo(.LCPI106_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r16, r6
	xori r1, r1, 1
	bne r1, r15, .LBB106_4
.LBB106_1:
	addi r4, r0, 32
	add r3, r11, r0
	add r5, r12, r0
	jal r31, memset
	addi r3, r11, 1
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
	addi r5, r0, 3
	jal r31, memcpy
.LBB106_2:
	add r1, r11, r12
	stb r1+0, r15
.LBB106_3:
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
.LBB106_4:
	beq r4, r15, .LBB106_13
.LBB106_5:
	lui r6, %hi(.L.str.101)
	addi r6, r6, %lo(.L.str.101)
	addi r13, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, f77_wr_begin_int
	add r3, r16, r0
	add r4, r17, r0
	jal r31, f77_wr_d
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	bne r1, r15, .LBB106_9
.LBB106_6:
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	ldw r4, r1+0
	lui r3, %hi(fio_rep)
	addi r3, r3, %lo(fio_rep)
	beq r4, r15, .LBB106_8
.LBB106_7:
	ldw r4, r3+0
	bgt r4, r15, .LBB106_9
.LBB106_8:
	lui r14, %hi(fio_draining)
	addi r14, r14, %lo(fio_draining)
	stb r14+0, r13
	stw r1+0, r15
	stw r3+0, r15
	jal r31, fio_next_desc
	stb r14+0, r15
.LBB106_9:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r15
	add r1, r11, r12
	stb r1+0, r15
	addi r1, r0, 69
	addi r3, r0, 68
	jal r0, .LBB106_11
.LBB106_10:
	addi r12, r12, -1
	addi r11, r11, 1
	beq r12, r15, .LBB106_3
.LBB106_11:
	ldbu r4, r11+0
	bne r4, r1, .LBB106_10
.LBB106_12:
	stb r11+0, r3
	jal r0, .LBB106_10
.LBB106_13:
	fcvt.s.d r14, r16
	lui r6, %hi(.L.str.102)
	addi r6, r6, %lo(.L.str.102)
	addi r13, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, f77_wr_begin_int
	add r3, r14, r0
	jal r31, f77_wr_r
	lui r1, %hi(fio_listed)
	addi r1, r1, %lo(fio_listed)
	ldw r1, r1+0
	bne r1, r15, .LBB106_17
.LBB106_14:
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	ldw r4, r1+0
	lui r3, %hi(fio_rep)
	addi r3, r3, %lo(fio_rep)
	beq r4, r15, .LBB106_16
.LBB106_15:
	ldw r4, r3+0
	bgt r4, r15, .LBB106_17
.LBB106_16:
	lui r14, %hi(fio_draining)
	addi r14, r14, %lo(fio_draining)
	stb r14+0, r13
	stw r1+0, r15
	stw r3+0, r15
	jal r31, fio_next_desc
	stb r14+0, r15
.LBB106_17:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	addi r1, r1, %lo(fio_ibuf)
	stw r1+0, r15
	jal r0, .LBB106_2
.Lfunc_end106:
	.size	nml_real, .Lfunc_end106-nml_real
                                        # -- End function
	.p2align	2                               # -- Begin function nml_trim_cat
	.type	nml_trim_cat,@function
nml_trim_cat:                           # @nml_trim_cat
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
	add r14, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	add r3, r5, r0
	jal r31, strlen
	add r13, r1, r0
	addi r1, r0, 1
	addi r17, r0, 32
	addi r16, r0, 0
	blt r13, r1, .LBB107_4
.LBB107_1:
	add r1, r14, r16
	ldbu r1, r1+0
	bne r1, r17, .LBB107_4
.LBB107_2:
	addi r16, r16, 1
	bne r13, r16, .LBB107_1
.LBB107_3:
	add r16, r13, r0
.LBB107_4:
	sub r1, r13, r16
	addi r3, r0, 3
	add r15, r14, r16
	blt r1, r3, .LBB107_6
.LBB107_5:
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
	addi r5, r0, 3
	add r3, r15, r0
	jal r31, memcmp
	addi r3, r0, 0
	beq r1, r3, .LBB107_11
.LBB107_6:
	ble r13, r16, .LBB107_12
.LBB107_7:
	addi r1, r13, -1
	xor r3, r16, r1
	slt r4, r16, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	addi r3, r14, -1
.LBB107_8:
	add r4, r3, r13
	ldbu r4, r4+0
	bne r4, r17, .LBB107_12
.LBB107_9:
	addi r13, r13, -1
	bgt r13, r16, .LBB107_8
.LBB107_10:
	add r13, r1, r0
	jal r0, .LBB107_12
.LBB107_11:
	addi r1, r16, 3
	bne r1, r13, .LBB107_13
.LBB107_12:
	add r3, r12, r11
	sub r12, r13, r16
	add r4, r15, r0
	add r5, r12, r0
	jal r31, memcpy
	add r1, r12, r11
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
.LBB107_13:
	add r1, r14, r1
	ldbu r1, r1+0
	bne r1, r17, .LBB107_6
	jal r0, .LBB107_12
.Lfunc_end107:
	.size	nml_trim_cat, .Lfunc_end107-nml_trim_cat
                                        # -- End function
	.globl	f77_nml_end                     # -- Begin function f77_nml_end
	.p2align	2
	.type	f77_nml_end,@function
f77_nml_end:                            # @f77_nml_end
# %bb.0:
	addi sp, sp, -144
	stw sp+0, lr
	stw sp+140, r11
	stw sp+136, r12
	stw sp+132, r13
	stw sp+128, r14
	stw sp+124, r15
	stw sp+120, r16
	lui r1, %hi(nml_reading)
	addi r1, r1, %lo(nml_reading)
	ldw r1, r1+0
	addi r12, r0, 0
	beq r1, r12, .LBB108_6
.LBB108_1:
	lui r11, %hi(nml_status)
	addi r11, r11, %lo(nml_status)
	ldw r1, r11+0
	beq r1, r12, .LBB108_19
.LBB108_2:
	ldw r1, r11+0
	addi r3, r0, 2
	beq r1, r3, .LBB108_23
.LBB108_3:
	addi r3, r0, 1
	bne r1, r3, .LBB108_24
.LBB108_4:
	lui r1, %hi(nml_flags)
	addi r1, r1, %lo(nml_flags)
	ldbu r1, r1+0
	andi r4, r1, 1
	add r1, r3, r0
	bne r4, r12, .LBB108_24
.LBB108_5:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r1, %hi(nml_unit)
	addi r1, r1, %lo(nml_unit)
	ldw r5, r1+0
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	lui r6, %hi(nml_grp)
	addi r6, r6, %lo(nml_grp)
	jal r0, .LBB108_28
.LBB108_6:
	lui r11, %hi(nml_npend)
	addi r11, r11, %lo(nml_npend)
	ldw r4, r11+0
	blt r4, r12, .LBB108_8
.LBB108_7:
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	jal r31, nml_put
	addi r1, r0, -1
	stw r11+0, r1
.LBB108_8:
	lui r15, %hi(nml_len)
	addi r15, r15, %lo(nml_len)
	ldw r11, r15+0
	addi r1, r0, 2
	lui r14, %hi(fio_listed)
	addi r14, r14, %lo(fio_listed)
	lui r13, %hi(fio_ibuf)
	addi r13, r13, %lo(fio_ibuf)
	blt r11, r1, .LBB108_14
.LBB108_9:
	lui r1, %hi(nml_unit)
	addi r1, r1, %lo(nml_unit)
	ldw r3, r1+0
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	jal r31, f77_wr_begin
	lui r3, %hi(nml_line)
	addi r3, r3, %lo(nml_line)
	add r4, r11, r0
	jal r31, f77_wr_a
	ldw r1, r14+0
	addi r11, r0, 0
	bne r1, r11, .LBB108_13
.LBB108_10:
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	ldw r4, r1+0
	lui r3, %hi(fio_rep)
	addi r3, r3, %lo(fio_rep)
	beq r4, r11, .LBB108_12
.LBB108_11:
	ldw r4, r3+0
	bgt r4, r11, .LBB108_13
.LBB108_12:
	lui r16, %hi(fio_draining)
	addi r16, r16, %lo(fio_draining)
	addi r4, r0, 1
	stb r16+0, r4
	stw r1+0, r11
	stw r3+0, r11
	jal r31, fio_next_desc
	stb r16+0, r11
.LBB108_13:
	jal r31, fio_flush_line
	stw r13+0, r11
.LBB108_14:
	lui r1, %hi(nml_line)
	addi r1, r1, %lo(nml_line)
	addi r3, r0, 32
	stb r1+0, r3
	addi r11, r0, 1
	stw r15+0, r11
	lui r1, %hi(nml_unit)
	addi r1, r1, %lo(nml_unit)
	ldw r3, r1+0
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	jal r31, f77_wr_begin
	lui r3, %hi(.L.str.26)
	addi r3, r3, %lo(.L.str.26)
	addi r4, r0, 5
	jal r31, f77_wr_a
	ldw r1, r14+0
	bne r1, r12, .LBB108_18
.LBB108_15:
	lui r1, %hi(fio_desc)
	addi r1, r1, %lo(fio_desc)
	ldw r4, r1+0
	lui r3, %hi(fio_rep)
	addi r3, r3, %lo(fio_rep)
	addi r14, r0, 0
	beq r4, r14, .LBB108_17
.LBB108_16:
	ldw r4, r3+0
	bgt r4, r14, .LBB108_18
.LBB108_17:
	lui r15, %hi(fio_draining)
	addi r15, r15, %lo(fio_draining)
	stb r15+0, r11
	stw r1+0, r14
	stw r3+0, r14
	jal r31, fio_next_desc
	stb r15+0, r14
.LBB108_18:
	jal r31, fio_flush_line
	stw r13+0, r12
	add r1, r12, r0
	jal r0, .LBB108_24
.LBB108_19:
	lui r1, %hi(nml_nasg)
	addi r1, r1, %lo(nml_nasg)
	ldw r1, r1+0
	addi r3, r0, 1
	blt r1, r3, .LBB108_2
.LBB108_20:
	lui r3, %hi(nml_asg)
	addi r3, r3, %lo(nml_asg)
	ldw r5, r3+0
.LBB108_21:
	ldw r3, r5+72
	beq r3, r12, .LBB108_25
.LBB108_22:
	addi r5, r5, 76
	addi r1, r1, -1
	bne r1, r12, .LBB108_21
	jal r0, .LBB108_2
.LBB108_23:
	lui r3, %hi(nml_flags)
	addi r3, r3, %lo(nml_flags)
	ldbu r3, r3+0
	andi r3, r3, 2
	beq r3, r12, .LBB108_27
.LBB108_24:
	ldw r16, sp+120
	ldw r15, sp+124
	ldw r14, sp+128
	ldw r13, sp+132
	ldw r12, sp+136
	ldw r11, sp+140
	ldw lr, sp+0
	addi sp, sp, 144
	jalr r0, r31, 0
.LBB108_25:
	lui r4, %hi(.L.str.27)
	addi r4, r4, %lo(.L.str.27)
	addi r3, sp, 24
	jal r31, sprintf
	ldw r1, r11+0
	bne r1, r12, .LBB108_2
.LBB108_26:
	addi r1, r0, 2
	stw r11+0, r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	addi r4, sp, 24
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	addi r1, r1, %lo(nml_err+95)
	stb r1+0, r12
	jal r0, .LBB108_2
.LBB108_27:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r1, %hi(nml_unit)
	addi r1, r1, %lo(nml_unit)
	ldw r6, r1+0
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	lui r5, %hi(nml_grp)
	addi r5, r5, %lo(nml_grp)
	lui r7, %hi(nml_err)
	addi r7, r7, %lo(nml_err)
.LBB108_28:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end108:
	.size	f77_nml_end, .Lfunc_end108-f77_nml_end
                                        # -- End function
	.globl	f77_ipow                        # -- Begin function f77_ipow
	.p2align	2
	.type	f77_ipow,@function
f77_ipow:                               # @f77_ipow
# %bb.0:
	addi r1, r0, 1
	addi r5, r0, 0
	blt r4, r5, .LBB109_3
.LBB109_1:
	beq r4, r5, .LBB109_8
.LBB109_2:
	andi r6, r4, 1
	seq r6, r6, r5
	sub r6, r0, r6
	xori r7, r3, 1
	and r6, r7, r6
	xor r6, r3, r6
	mul r1, r6, r1
	srli r4, r4, 1
	seq r6, r4, r5
	sub r6, r0, r6
	and r6, r7, r6
	xor r6, r3, r6
	mul r3, r6, r3
	bne r4, r5, .LBB109_2
	jal r0, .LBB109_8
.LBB109_3:
	beq r3, r1, .LBB109_6
.LBB109_4:
	addi r1, r0, -1
	bne r3, r1, .LBB109_7
.LBB109_5:
	andi r1, r4, 1
	addi r3, r0, 0
	seq r1, r1, r3
	addi r1, r1, -1
	ori  r1, r1, 1
	jal r0, .LBB109_8
.LBB109_6:
	add r1, r3, r0
	jal r0, .LBB109_8
.LBB109_7:
	add r1, r5, r0
.LBB109_8:
	jalr r0, r31, 0
.Lfunc_end109:
	.size	f77_ipow, .Lfunc_end109-f77_ipow
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_dpow_i
	.type	.LCPI110_0,@object
.LCPI110_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI110_0, 8
	.type	.LCPI110_1,@object
.LCPI110_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI110_1, 8
	.type	.LCPI110_2,@object
.LCPI110_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI110_2, 8
	.type	.LCPI110_3,@object
.LCPI110_3:
	.quad	0x7fefdcf158adbb99              # double 1.79E+308
	.size	.LCPI110_3, 8
	.text
	.globl	f77_dpow_i
	.p2align	2
	.type	f77_dpow_i,@function
f77_dpow_i:                             # @f77_dpow_i
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
	stw sp+20, r25
	add r7, r4, r0
	add r6, r3, r0
	addi r1, r0, 0
	blt r5, r1, .LBB110_3
.LBB110_1:
	andi r3, r5, 1
	seq r3, r3, r1
	lui r4, %hi(.LCPI110_0)
	addi r4, r4, %lo(.LCPI110_0)
	ldw r8, r4+4
	ldw r4, r4+0
	xor r8, r8, r7
	sub r3, r0, r3
	and r8, r8, r3
	xor r9, r7, r8
	xor r4, r4, r6
	and r3, r4, r3
	xor r8, r6, r3
	srli r3, r5, 1
	beq r3, r1, .LBB110_16
.LBB110_2:
	fmul.d r6, r6, r6
	andi r4, r3, 1
	seq r4, r4, r1
	fmul.d r10, r6, r8
	xor r5, r9, r11
	sub r4, r0, r4
	and r5, r5, r4
	xor r9, r11, r5
	xor r5, r8, r10
	and r4, r5, r4
	xor r8, r10, r4
	srli r3, r3, 1
	bne r3, r1, .LBB110_2
	jal r0, .LBB110_16
.LBB110_3:
	sub r3, r0, r5
	lui r8, %hi(.LCPI110_0)
	addi r8, r8, %lo(.LCPI110_0)
	ldw r5, r8+4
	ldw r4, r8+0
	lui r10, %hi(.LCPI110_1)
	addi r10, r10, %lo(.LCPI110_1)
	ldw r9, r10+4
	ldw r8, r10+0
	lui lr, %hi(.LCPI110_2)
	addi lr, lr, %lo(.LCPI110_2)
	ldw r11, lr+4
	ldw r10, lr+0
	lui lr, %hi(.LCPI110_3)
	addi lr, lr, %lo(.LCPI110_3)
	ldw r15, lr+4
	ldw r14, lr+0
	add r16, r6, r0
	add r17, r7, r0
	add lr, r3, r0
	add r12, r4, r0
	add r13, r5, r0
	jal r0, .LBB110_5
.LBB110_4:
	fmul.d r16, r16, r16
.LBB110_5:
	andi r20, lr, 1
	fneg.d r18, r16
	beq r20, r1, .LBB110_10
.LBB110_6:
	flt.d r20, r12, r8
	fneg.d r22, r12
	xor r21, r23, r13
	sub r24, r0, r20
	and r20, r21, r24
	xor r21, r13, r20
	xor r22, r22, r12
	and r22, r22, r24
	xor r20, r12, r22
	flt.d r22, r16, r8
	xor r23, r19, r17
	sub r24, r0, r22
	and r22, r23, r24
	xor r23, r17, r22
	xor r25, r18, r16
	and r24, r25, r24
	xor r22, r16, r24
	fdiv.d r24, r14, r20
	fle.d r24, r22, r24
	bne r24, r1, .LBB110_9
.LBB110_7:
	fle.d r24, r20, r4
	bne r24, r1, .LBB110_9
.LBB110_8:
	fle.d r20, r20, r10
	xori r20, r20, 1
	fle.d r21, r22, r10
	xori r21, r21, 1
	or  r20, r21, r20
	bne r20, r1, .LBB110_13
.LBB110_9:
	fmul.d r12, r16, r12
.LBB110_10:
	srli lr, lr, 1
	beq lr, r1, .LBB110_15
.LBB110_11:
	flt.d r20, r16, r8
	xor r21, r19, r17
	sub r22, r0, r20
	and r20, r21, r22
	xor r21, r17, r20
	xor r18, r18, r16
	and r18, r18, r22
	xor r20, r16, r18
	fle.d r18, r20, r10
	bne r18, r1, .LBB110_4
.LBB110_12:
	fdiv.d r18, r14, r20
	fle.d r18, r20, r18
	xori r18, r18, 1
	beq r18, r1, .LBB110_4
.LBB110_13:
	fdiv.d r6, r4, r6
	andi r8, r3, 1
	seq r8, r8, r1
	xor r9, r5, r7
	sub r10, r0, r8
	and r8, r9, r10
	xor r9, r7, r8
	xor r4, r4, r6
	and r4, r4, r10
	xor r8, r6, r4
	srli r3, r3, 1
	beq r3, r1, .LBB110_16
.LBB110_14:
	fmul.d r6, r6, r6
	andi r4, r3, 1
	seq r4, r4, r1
	fmul.d r10, r6, r8
	xor r5, r9, r11
	sub r4, r0, r4
	and r5, r5, r4
	xor r9, r11, r5
	xor r5, r8, r10
	and r4, r5, r4
	xor r8, r10, r4
	srli r3, r3, 1
	bne r3, r1, .LBB110_14
	jal r0, .LBB110_16
.LBB110_15:
	fdiv.d r8, r4, r12
.LBB110_16:
	add r1, r8, r0
	add r2, r9, r0
	ldw r25, sp+20
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
.Lfunc_end110:
	.size	f77_dpow_i, .Lfunc_end110-f77_dpow_i
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rpow_i
	.type	.LCPI111_0,@object
.LCPI111_0:
	.word	0x3f800000                      # float 1
	.size	.LCPI111_0, 4
	.type	.LCPI111_1,@object
.LCPI111_1:
	.word	0x00000000                      # float 0
	.size	.LCPI111_1, 4
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0
	.type	.LCPI111_2,@object
.LCPI111_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI111_2, 8
	.type	.LCPI111_3,@object
.LCPI111_3:
	.quad	0x47eff933c78cdfad              # double 3.4E+38
	.size	.LCPI111_3, 8
	.type	.LCPI111_4,@object
.LCPI111_4:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI111_4, 8
	.text
	.globl	f77_rpow_i
	.p2align	2
	.type	f77_rpow_i,@function
f77_rpow_i:                             # @f77_rpow_i
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
	addi r5, r0, 0
	blt r4, r5, .LBB111_3
.LBB111_1:
	andi r1, r4, 1
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
	srli r4, r4, 1
	beq r4, r5, .LBB111_17
.LBB111_2:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	srli r4, r4, 1
	bne r4, r5, .LBB111_2
	jal r0, .LBB111_17
.LBB111_3:
	sub r4, r0, r4
	lui r1, %hi(.LCPI111_0)
	addi r1, r1, %lo(.LCPI111_0)
	ldw r6, r1+0
	lui r7, %hi(.LCPI111_1)
	addi r7, r7, %lo(.LCPI111_1)
	ldw r7, r7+0
	lui r10, %hi(.LCPI111_2)
	addi r10, r10, %lo(.LCPI111_2)
	ldw r9, r10+4
	ldw r8, r10+0
	lui lr, %hi(.LCPI111_3)
	addi lr, lr, %lo(.LCPI111_3)
	ldw r11, lr+4
	ldw r10, lr+0
	add lr, r3, r0
	add r12, r4, r0
	jal r0, .LBB111_5
.LBB111_4:
	fmul.s lr, lr, lr
.LBB111_5:
	andi r16, r12, 1
	fcvt.d.s r14, lr
	flt.s r13, lr, r7
	beq r16, r5, .LBB111_10
.LBB111_6:
	fcvt.d.s r18, r6
	flt.s r16, r6, r7
	fneg.d r20, r18
	xor r17, r21, r19
	sub r22, r0, r16
	and r16, r17, r22
	xor r17, r19, r16
	xor r20, r20, r18
	and r20, r20, r22
	xor r16, r18, r20
	fneg.d r20, r14
	xor r18, r21, r15
	sub r22, r0, r13
	and r18, r18, r22
	xor r19, r15, r18
	xor r20, r20, r14
	and r20, r20, r22
	xor r18, r14, r20
	fdiv.d r20, r10, r16
	fle.d r20, r18, r20
	bne r20, r5, .LBB111_9
.LBB111_7:
	lui r20, %hi(.LCPI111_4)
	addi r20, r20, %lo(.LCPI111_4)
	ldw r23, r20+4
	ldw r22, r20+0
	fle.d r20, r16, r22
	bne r20, r5, .LBB111_9
.LBB111_8:
	fle.d r16, r16, r8
	xori r16, r16, 1
	fle.d r17, r18, r8
	xori r17, r17, 1
	or  r16, r17, r16
	bne r16, r5, .LBB111_13
.LBB111_9:
	fmul.s r6, lr, r6
.LBB111_10:
	srli r12, r12, 1
	beq r12, r5, .LBB111_16
.LBB111_11:
	fneg.d r18, r14
	xor r16, r19, r15
	sub r13, r0, r13
	and r16, r16, r13
	xor r17, r15, r16
	xor r18, r18, r14
	and r13, r18, r13
	xor r16, r14, r13
	fle.d r13, r16, r8
	bne r13, r5, .LBB111_4
.LBB111_12:
	fdiv.d r14, r10, r16
	fle.d r13, r16, r14
	xori r13, r13, 1
	beq r13, r5, .LBB111_4
.LBB111_13:
	ldw r1, r1+0
	fdiv.s r3, r1, r3
	andi r1, r4, 1
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
.LBB111_14:
	srli r4, r4, 1
	beq r4, r5, .LBB111_17
.LBB111_15:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	jal r0, .LBB111_14
.LBB111_16:
	ldw r1, r1+0
	fdiv.s r1, r1, r6
.LBB111_17:
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
.Lfunc_end111:
	.size	f77_rpow_i, .Lfunc_end111-f77_rpow_i
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
	add r11, r3, r0
	lui r3, %hi(fio_uend_init)
	addi r3, r3, %lo(fio_uend_init)
	ldbu lr, r3+0
	addi r1, r0, 0
	bne lr, r1, .LBB112_4
.LBB112_1:
	addi lr, r0, -400
	lui r12, %hi(fio_uend+400)
	addi r12, r12, %lo(fio_uend+400)
	addi r13, r0, -1
.LBB112_2:
	add r14, lr, r12
	stw r14+0, r13
	addi lr, lr, 4
	bne lr, r1, .LBB112_2
.LBB112_3:
	addi lr, r0, 1
	stb r3+0, lr
.LBB112_4:
	ldw r15, sp+352
	addi r3, r11, -100
	addi lr, r0, -99
	bltu r3, lr, .LBB112_13
.LBB112_5:
	addi r3, r11, -5
	addi r19, r0, 1
	bleu r3, r19, .LBB112_13
.LBB112_6:
	add r16, r1, r0
	beq r8, r1, .LBB112_20
.LBB112_7:
	addi r3, r0, 1
	add r16, r1, r0
	blt r9, r3, .LBB112_20
.LBB112_8:
	addi r16, r0, 0
	addi lr, r0, 6
	lui r12, %hi(.L.str.31)
	addi r12, r12, %lo(.L.str.31)
	addi r13, r0, 26
	add r14, r16, r0
.LBB112_9:
	beq r14, lr, .LBB112_15
.LBB112_10:
	add r17, r14, r12
	ldb r17, r17+0
	add r18, r8, r14
	ldb r18, r18+0
	addi r20, r18, -97
	andi r20, r20, 255
	sltu r20, r20, r13
	addi r21, r18, -32
	xor r21, r21, r18
	sub r20, r0, r20
	and r20, r21, r20
	xor r18, r18, r20
	bne r18, r17, .LBB112_20
.LBB112_11:
	addi r14, r14, 1
	bne r9, r14, .LBB112_9
.LBB112_12:
	add r12, r9, r0
	beq r12, lr, .LBB112_16
	jal r0, .LBB112_20
.LBB112_13:
	addi r1, r0, 0
	bne r15, r1, .LBB112_47
.LBB112_14:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	add r5, r11, r0
	jal r0, .LBB112_141
.LBB112_15:
	add r12, lr, r0
	bne r12, lr, .LBB112_20
.LBB112_16:
	addi lr, r0, 7
	add r16, r3, r0
	blt r9, lr, .LBB112_20
.LBB112_17:
	addi lr, r9, -7
	addi r3, r8, 6
	addi r8, r0, 32
	addi r9, r0, 1
.LBB112_18:
	ldbu r12, r3+0
	seq r16, r12, r8
	addi r13, lr, -1
	sltu lr, r13, lr
	bne lr, r9, .LBB112_20
.LBB112_19:
	addi r3, r3, 1
	add lr, r13, r0
	beq r12, r8, .LBB112_18
.LBB112_20:
	add r17, r16, r0
	beq r10, r1, .LBB112_33
.LBB112_21:
	ldw r3, sp+336
	addi r1, r0, 1
	add r17, r16, r0
	blt r3, r1, .LBB112_33
.LBB112_22:
	addi r17, r0, 0
	addi r8, r0, 11
	lui r9, %hi(.L.str.32)
	addi r9, r9, %lo(.L.str.32)
	addi lr, r0, 26
	add r12, r17, r0
.LBB112_23:
	beq r12, r8, .LBB112_27
.LBB112_24:
	add r13, r12, r9
	ldb r13, r13+0
	add r14, r10, r12
	ldb r14, r14+0
	addi r18, r14, -97
	andi r18, r18, 255
	sltu r18, r18, lr
	addi r20, r14, -32
	xor r20, r20, r14
	sub r18, r0, r18
	and r18, r20, r18
	xor r14, r14, r18
	bne r14, r13, .LBB112_33
.LBB112_25:
	addi r12, r12, 1
	bne r3, r12, .LBB112_23
.LBB112_26:
	add r9, r3, r0
	beq r9, r8, .LBB112_28
	jal r0, .LBB112_33
.LBB112_27:
	add r9, r8, r0
	bne r9, r8, .LBB112_33
.LBB112_28:
	addi r8, r0, 12
	blt r3, r8, .LBB112_32
.LBB112_29:
	addi r8, r10, 11
	addi r3, r3, -11
	addi r17, r0, 0
	addi r9, r0, 32
.LBB112_30:
	ldbu r10, r8+0
	bne r10, r9, .LBB112_33
.LBB112_31:
	addi r8, r8, 1
	addi r3, r3, -1
	bne r3, r17, .LBB112_30
.LBB112_32:
	add r17, r1, r0
.LBB112_33:
	ldw r18, sp+348
	addi r1, r0, 0
	sne r3, r6, r1
	sgt r8, r7, r1
	and r23, r3, r8
	add r24, r1, r0
	bne r23, r19, .LBB112_45
.LBB112_34:
	addi r24, r0, 0
	addi r3, r0, 7
	lui r8, %hi(.L.str.33)
	addi r8, r8, %lo(.L.str.33)
	addi r9, r0, 26
	add r10, r24, r0
.LBB112_35:
	beq r10, r3, .LBB112_39
.LBB112_36:
	add lr, r10, r8
	ldb lr, lr+0
	add r12, r6, r10
	ldb r12, r12+0
	addi r13, r12, -97
	andi r13, r13, 255
	sltu r13, r13, r9
	addi r14, r12, -32
	xor r14, r14, r12
	sub r13, r0, r13
	and r13, r14, r13
	xor r12, r12, r13
	bne r12, lr, .LBB112_45
.LBB112_37:
	addi r10, r10, 1
	bne r7, r10, .LBB112_35
.LBB112_38:
	add r8, r7, r0
	beq r8, r3, .LBB112_40
	jal r0, .LBB112_45
.LBB112_39:
	add r8, r3, r0
	bne r8, r3, .LBB112_45
.LBB112_40:
	addi r3, r0, 8
	blt r7, r3, .LBB112_44
.LBB112_41:
	addi r10, r7, -8
	addi r3, r6, 7
	addi r8, r0, 32
	addi r9, r0, 1
.LBB112_42:
	ldbu lr, r3+0
	seq r24, lr, r8
	addi r12, r10, -1
	sltu r10, r12, r10
	bne r10, r9, .LBB112_45
.LBB112_43:
	addi r3, r3, 1
	add r10, r12, r0
	beq lr, r8, .LBB112_42
	jal r0, .LBB112_45
.LBB112_44:
	addi r24, r0, 1
.LBB112_45:
	sgt r3, r18, r1
	xori r8, r16, 1
	or  r3, r3, r8
	bne r3, r1, .LBB112_49
.LBB112_46:
	addi r1, r0, 0
	beq r15, r1, .LBB112_140
.LBB112_47:
	addi r12, r0, 2
.LBB112_48:
	add r1, r12, r0
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
.LBB112_49:
	addi r12, r0, 0
	beq r4, r12, .LBB112_54
.LBB112_50:
	addi r20, r0, 1
	blt r5, r20, .LBB112_54
.LBB112_51:
	addi r1, r0, 256
	blt r5, r1, .LBB112_67
.LBB112_52:
	addi r1, r0, 0
	bne r15, r1, .LBB112_47
.LBB112_53:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	jal r0, .LBB112_141
.LBB112_54:
	add r25, r6, r0
	add r26, r7, r0
	lui r5, %hi(.L.str.36)
	addi r5, r5, %lo(.L.str.36)
	addi r3, sp, 16
	addi r4, r0, 256
	add r6, r11, r0
	jal r31, snprintf
.LBB112_55:
	ldw r20, sp+344
	ldw r21, sp+340
	slli r14, r11, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r22, r14, r1
	ldw r1, r22+0
	beq r1, r12, .LBB112_58
.LBB112_56:
	slli r1, r11, 8
	lui r3, %hi(fio_ufname)
	addi r3, r3, %lo(fio_ufname)
	add r3, r1, r3
	addi r4, sp, 16
	jal r31, strcmp
	sne r1, r1, r12
	or  r1, r24, r1
	beq r1, r12, .LBB112_70
.LBB112_57:
	ldw r3, r22+0
	jal r31, fclose
	addi r1, r0, 0
	stw r22+0, r1
.LBB112_58:
	addi r13, r0, 0
	beq r24, r13, .LBB112_61
.LBB112_59:
	jal r31, tmpfile
	addi r3, r0, 0
	beq r1, r3, .LBB112_77
.LBB112_60:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	jal r0, .LBB112_117
.LBB112_61:
	beq r23, r13, .LBB112_94
.LBB112_62:
	addi r5, r0, 0
	addi r1, r0, 3
	lui r6, %hi(.L.str.39)
	addi r6, r6, %lo(.L.str.39)
	addi r7, r0, 26
	add r4, r26, r0
	add r3, r25, r0
.LBB112_63:
	beq r5, r1, .LBB112_79
.LBB112_64:
	add r8, r5, r6
	ldb r8, r8+0
	add r9, r3, r5
	ldb r9, r9+0
	addi r10, r9, -97
	andi r10, r10, 255
	sltu r10, r10, r7
	addi lr, r9, -32
	xor lr, lr, r9
	sub r10, r0, r10
	and r10, lr, r10
	xor r9, r9, r10
	bne r9, r8, .LBB112_90
.LBB112_65:
	addi r5, r5, 1
	bne r4, r5, .LBB112_63
.LBB112_66:
	add r5, r4, r0
	jal r0, .LBB112_80
.LBB112_67:
	add r25, r6, r0
	add r26, r7, r0
	addi r14, sp, 16
	add r3, r14, r0
	add r13, r5, r0
	jal r31, memcpy
	add r3, r14, r13
	addi r1, r0, 0
	stb r3+0, r1
	addi r3, r14, -1
	addi r4, r0, 32
.LBB112_68:
	add r6, r3, r13
	ldbu r7, r6+0
	bne r7, r4, .LBB112_55
.LBB112_69:
	add r5, r13, r0
	addi r13, r13, -1
	stb r6+0, r1
	bgt r5, r20, .LBB112_68
	jal r0, .LBB112_55
.LBB112_70:
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r14, r1
	stw r1+0, r16
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r14, r1
	stw r1+0, r17
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r14, r1
	stw r1+0, r18
	add r1, r12, r0
	beq r21, r12, .LBB112_133
.LBB112_71:
	add r1, r12, r0
	blt r20, r19, .LBB112_133
.LBB112_72:
	addi r1, r0, 0
	addi r3, r0, 4
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	addi r5, r0, 26
	add r6, r1, r0
.LBB112_73:
	beq r6, r3, .LBB112_101
.LBB112_74:
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
	bne r8, r7, .LBB112_133
.LBB112_75:
	addi r6, r6, 1
	bne r20, r6, .LBB112_73
.LBB112_76:
	add r4, r20, r0
	jal r0, .LBB112_102
.LBB112_77:
	bne r15, r3, .LBB112_47
.LBB112_78:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	jal r0, .LBB112_141
.LBB112_79:
	add r5, r1, r0
.LBB112_80:
	bne r5, r1, .LBB112_90
.LBB112_81:
	addi r1, r0, 4
	blt r4, r1, .LBB112_85
.LBB112_82:
	addi r1, r3, 3
	addi r5, r4, -3
	addi r6, r0, 32
	addi r7, r0, 0
.LBB112_83:
	ldbu r8, r1+0
	bne r8, r6, .LBB112_90
.LBB112_84:
	addi r1, r1, 1
	addi r5, r5, -1
	bne r5, r7, .LBB112_83
.LBB112_85:
	lui r4, %hi(.L.str.40)
	addi r4, r4, %lo(.L.str.40)
	addi r3, sp, 16
	jal r31, fopen
	addi r13, r0, 0
	bne r1, r13, .LBB112_87
.LBB112_86:
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	addi r3, sp, 16
	jal r31, fopen
.LBB112_87:
	addi r4, sp, 16
	bne r1, r13, .LBB112_117
.LBB112_88:
	bne r15, r13, .LBB112_47
.LBB112_89:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	jal r0, .LBB112_136
.LBB112_90:
	lui r5, %hi(.L.str.43)
	addi r5, r5, %lo(.L.str.43)
	add r25, r3, r0
	add r26, r4, r0
	jal r31, fio_str_is
	addi r19, r0, 0
	beq r1, r19, .LBB112_94
.LBB112_91:
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	addi r3, sp, 16
	jal r31, fopen
	beq r1, r19, .LBB112_137
.LBB112_92:
	add r3, r1, r0
	jal r31, fclose
	bne r15, r19, .LBB112_47
.LBB112_93:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	jal r0, .LBB112_136
.LBB112_94:
	beq r25, r13, .LBB112_114
.LBB112_95:
	addi r1, r0, 1
	blt r26, r1, .LBB112_114
.LBB112_96:
	addi r1, r0, 7
	lui r3, %hi(.L.str.47)
	addi r3, r3, %lo(.L.str.47)
	addi r4, r0, 26
.LBB112_97:
	beq r13, r1, .LBB112_108
.LBB112_98:
	add r5, r13, r3
	ldb r5, r5+0
	add r6, r25, r13
	ldb r6, r6+0
	addi r7, r6, -97
	andi r7, r7, 255
	sltu r7, r7, r4
	addi r8, r6, -32
	xor r8, r8, r6
	sub r7, r0, r7
	and r7, r8, r7
	xor r6, r6, r7
	bne r6, r5, .LBB112_124
.LBB112_99:
	addi r13, r13, 1
	bne r26, r13, .LBB112_97
.LBB112_100:
	add r3, r26, r0
	jal r0, .LBB112_109
.LBB112_101:
	add r4, r3, r0
.LBB112_102:
	bne r4, r3, .LBB112_133
.LBB112_103:
	addi r1, r0, 5
	blt r20, r1, .LBB112_107
.LBB112_104:
	addi r3, r21, 4
	addi r4, r20, -4
	addi r1, r0, 0
	addi r5, r0, 32
.LBB112_105:
	ldbu r6, r3+0
	bne r6, r5, .LBB112_133
.LBB112_106:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r1, .LBB112_105
.LBB112_107:
	add r1, r19, r0
	jal r0, .LBB112_133
.LBB112_108:
	add r3, r1, r0
.LBB112_109:
	bne r3, r1, .LBB112_124
.LBB112_110:
	addi r1, r0, 8
	blt r26, r1, .LBB112_114
.LBB112_111:
	addi r1, r25, 7
	addi r3, r26, -7
	addi r4, r0, 32
	addi r5, r0, 0
.LBB112_112:
	ldbu r6, r1+0
	bne r6, r4, .LBB112_124
.LBB112_113:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r5, .LBB112_112
.LBB112_114:
	lui r4, %hi(.L.str.40)
	addi r4, r4, %lo(.L.str.40)
	addi r3, sp, 16
	jal r31, fopen
	addi r13, r0, 0
	bne r1, r13, .LBB112_116
.LBB112_115:
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	addi r3, sp, 16
	jal r31, fopen
.LBB112_116:
	addi r4, sp, 16
	beq r1, r13, .LBB112_134
.LBB112_117:
	stw r22+0, r1
	slli r1, r11, 8
	lui r3, %hi(fio_ufname)
	addi r3, r3, %lo(fio_ufname)
	add r3, r1, r3
	jal r31, strcpy
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r1, r14, r1
	addi r3, r0, -1
	stw r1+0, r3
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r14, r1
	stw r1+0, r16
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r14, r1
	stw r1+0, r17
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r14, r1
	stw r1+0, r18
	addi r1, r0, 0
	beq r21, r1, .LBB112_133
.LBB112_118:
	addi r3, r0, 1
	blt r20, r3, .LBB112_133
.LBB112_119:
	addi r1, r0, 0
	addi r4, r0, 4
	lui r5, %hi(.L.str.37)
	addi r5, r5, %lo(.L.str.37)
	addi r6, r0, 26
	add r7, r1, r0
.LBB112_120:
	beq r7, r4, .LBB112_126
.LBB112_121:
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
	bne r9, r8, .LBB112_133
.LBB112_122:
	addi r7, r7, 1
	bne r20, r7, .LBB112_120
.LBB112_123:
	add r5, r20, r0
	jal r0, .LBB112_127
.LBB112_124:
	addi r1, r0, 0
	bne r15, r1, .LBB112_47
.LBB112_125:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.49)
	addi r4, r4, %lo(.L.str.49)
	jal r0, .LBB112_141
.LBB112_126:
	add r5, r4, r0
.LBB112_127:
	bne r5, r4, .LBB112_133
.LBB112_128:
	addi r1, r0, 5
	blt r20, r1, .LBB112_132
.LBB112_129:
	addi r4, r21, 4
	addi r5, r20, -4
	addi r1, r0, 0
	addi r6, r0, 32
.LBB112_130:
	ldbu r7, r4+0
	bne r7, r6, .LBB112_133
.LBB112_131:
	addi r4, r4, 1
	addi r5, r5, -1
	bne r5, r1, .LBB112_130
.LBB112_132:
	add r1, r3, r0
.LBB112_133:
	lui r3, %hi(fio_ubz)
	addi r3, r3, %lo(fio_ubz)
	add r3, r14, r3
	stw r3+0, r1
	jal r0, .LBB112_48
.LBB112_134:
	bne r15, r13, .LBB112_47
.LBB112_135:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.48)
	addi r4, r4, %lo(.L.str.48)
.LBB112_136:
	addi r5, sp, 16
	jal r0, .LBB112_141
.LBB112_137:
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	addi r13, sp, 16
	add r3, r13, r0
	jal r31, fopen
	add r4, r13, r0
	bne r1, r19, .LBB112_117
.LBB112_138:
	bne r15, r19, .LBB112_47
.LBB112_139:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.46)
	addi r4, r4, %lo(.L.str.46)
	jal r0, .LBB112_136
.LBB112_140:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
.LBB112_141:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end112:
	.size	f77_open, .Lfunc_end112-f77_open
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
	blt r4, r6, .LBB113_6
.LBB113_1:
	addi r8, r0, 0
	addi r9, r0, 26
	add r7, r8, r0
.LBB113_2:
	add r10, r5, r7
	ldb r10, r10+0
	beq r10, r8, .LBB113_6
.LBB113_3:
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
	bne lr, r10, .LBB113_12
.LBB113_4:
	addi r7, r7, 1
	bne r4, r7, .LBB113_2
.LBB113_5:
	add r7, r4, r0
.LBB113_6:
	add r5, r5, r7
	ldbu r5, r5+0
	bne r5, r1, .LBB113_13
.LBB113_7:
	ble r4, r7, .LBB113_11
.LBB113_8:
	add r3, r3, r7
	sub r4, r4, r7
	addi r1, r0, 0
	addi r5, r0, 32
.LBB113_9:
	ldbu r7, r3+0
	bne r7, r5, .LBB113_13
.LBB113_10:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r1, .LBB113_9
.LBB113_11:
	add r1, r6, r0
	jal r0, .LBB113_13
.LBB113_12:
	add r1, r8, r0
.LBB113_13:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end113:
	.size	fio_str_is, .Lfunc_end113-fio_str_is
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
	stw sp+20, r17
	add r1, r3, r0
	addi r3, r0, 99
	bgtu r1, r3, .LBB114_32
.LBB114_1:
	slli r11, r1, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r12, r11, r3
	ldw r3, r12+0
	addi r6, r0, 0
	beq r3, r6, .LBB114_32
.LBB114_2:
	addi r7, r0, 1
	beq r4, r6, .LBB114_9
.LBB114_3:
	addi r8, r0, 1
	add r13, r6, r0
	blt r5, r8, .LBB114_16
.LBB114_4:
	addi r7, r0, 0
	addi r9, r0, 6
	lui r10, %hi(.L.str.50)
	addi r10, r10, %lo(.L.str.50)
	addi lr, r0, 26
	add r13, r7, r0
.LBB114_5:
	beq r13, r9, .LBB114_10
.LBB114_6:
	add r14, r13, r10
	ldb r14, r14+0
	add r15, r4, r13
	ldb r15, r15+0
	addi r16, r15, -97
	andi r16, r16, 255
	sltu r16, r16, lr
	addi r17, r15, -32
	xor r17, r17, r15
	sub r16, r0, r16
	and r16, r17, r16
	xor r15, r15, r16
	bne r15, r14, .LBB114_15
.LBB114_7:
	addi r13, r13, 1
	bne r5, r13, .LBB114_5
.LBB114_8:
	add r10, r5, r0
	add r13, r7, r0
	beq r10, r9, .LBB114_11
	jal r0, .LBB114_16
.LBB114_9:
	add r13, r6, r0
	add r14, r1, r0
	beq r7, r6, .LBB114_17
	jal r0, .LBB114_29
.LBB114_10:
	add r10, r9, r0
	add r13, r7, r0
	bne r10, r9, .LBB114_16
.LBB114_11:
	addi r9, r0, 7
	add r7, r8, r0
	add r13, r8, r0
	blt r5, r9, .LBB114_16
.LBB114_12:
	addi lr, r5, -7
	addi r8, r4, 6
	addi r9, r0, 32
	addi r10, r0, 1
.LBB114_13:
	ldbu r14, r8+0
	seq r7, r14, r9
	addi r13, lr, -1
	sltu lr, r13, lr
	bne lr, r10, .LBB114_33
.LBB114_14:
	addi r8, r8, 1
	add lr, r13, r0
	add r13, r7, r0
	beq r14, r9, .LBB114_13
	jal r0, .LBB114_16
.LBB114_15:
	add r13, r7, r0
.LBB114_16:
	add r14, r1, r0
	bne r7, r6, .LBB114_29
.LBB114_17:
	addi r1, r0, 1
	blt r5, r1, .LBB114_34
.LBB114_18:
	addi r6, r0, 0
	addi r1, r0, 4
	lui r7, %hi(.L.str.51)
	addi r7, r7, %lo(.L.str.51)
	addi r8, r0, 26
.LBB114_19:
	beq r6, r1, .LBB114_23
.LBB114_20:
	add r9, r6, r7
	ldb r9, r9+0
	add r10, r4, r6
	ldb r10, r10+0
	addi lr, r10, -97
	andi lr, lr, 255
	sltu lr, lr, r8
	addi r15, r10, -32
	xor r15, r15, r10
	sub lr, r0, lr
	and lr, r15, lr
	xor r10, r10, lr
	bne r10, r9, .LBB114_34
.LBB114_21:
	addi r6, r6, 1
	bne r5, r6, .LBB114_19
.LBB114_22:
	add r6, r5, r0
	jal r0, .LBB114_24
.LBB114_23:
	add r6, r1, r0
.LBB114_24:
	bne r6, r1, .LBB114_34
.LBB114_25:
	addi r1, r0, 5
	blt r5, r1, .LBB114_29
.LBB114_26:
	addi r1, r4, 4
	addi r4, r5, -4
	addi r5, r0, 32
	addi r6, r0, 0
.LBB114_27:
	ldbu r7, r1+0
	bne r7, r5, .LBB114_34
.LBB114_28:
	addi r1, r1, 1
	addi r4, r4, -1
	bne r4, r6, .LBB114_27
.LBB114_29:
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
	beq r13, r15, .LBB114_31
.LBB114_30:
	add r3, r11, r12
	jal r31, remove
.LBB114_31:
	add r1, r11, r12
	stb r1+0, r15
.LBB114_32:
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
.LBB114_33:
	add r13, r7, r0
	add r14, r1, r0
	beq r7, r6, .LBB114_17
	jal r0, .LBB114_29
.LBB114_34:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.52)
	addi r4, r4, %lo(.L.str.52)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end114:
	.size	f77_close, .Lfunc_end114-f77_close
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
	bne r3, r14, .LBB115_4
.LBB115_1:
	addi r3, r0, -400
	lui r6, %hi(fio_uend+400)
	addi r6, r6, %lo(fio_uend+400)
	addi r7, r0, -1
.LBB115_2:
	add r8, r3, r6
	stw r8+0, r7
	addi r3, r3, 4
	bne r3, r14, .LBB115_2
.LBB115_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB115_4:
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
	beq r4, r14, .LBB115_21
.LBB115_5:
	addi r23, r0, 1
	blt r5, r23, .LBB115_21
.LBB115_6:
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
	addi r3, r12, -1
	addi r1, r0, 32
.LBB115_7:
	addi r4, sp, 24
	add r4, r4, r3
	ldbu r5, r4+0
	bne r5, r1, .LBB115_9
.LBB115_8:
	stb r4+0, r14
	addi r4, r3, -1
	addi r5, r3, 1
	add r3, r4, r0
	bgt r5, r23, .LBB115_7
.LBB115_9:
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	addi r12, sp, 24
	add r4, r12, r0
	jal r31, strcpy
	stw r22+0, r23
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	add r3, r12, r0
	jal r31, fopen
	beq r1, r14, .LBB115_11
.LBB115_10:
	stb r24+0, r23
	add r3, r1, r0
	jal r31, fclose
.LBB115_11:
	lui r12, %hi(fio_ufname+256)
	addi r12, r12, %lo(fio_ufname+256)
	lui r13, %hi(fio_ufile+4)
	addi r13, r13, %lo(fio_ufile+4)
	addi r22, r0, 100
	jal r0, .LBB115_13
.LBB115_12:
	addi r23, r23, 1
	addi r12, r12, 256
	addi r13, r13, 4
	beq r23, r22, .LBB115_16
.LBB115_13:
	ldw r1, r13+0
	beq r1, r14, .LBB115_12
.LBB115_14:
	addi r4, sp, 24
	add r3, r12, r0
	jal r31, strcmp
	bne r1, r14, .LBB115_12
.LBB115_15:
	addi r1, r0, 1
	stb r20+0, r1
	stw r21+0, r23
.LBB115_16:
	ldbu r1, r20+0
	beq r1, r14, .LBB115_26
.LBB115_17:
	ldw r3, r21+0
	xor r3, r3, r11
	sub r1, r0, r1
	and r1, r3, r1
	xor r11, r11, r1
.LBB115_18:
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
	beq r4, r14, .LBB115_26
.LBB115_19:
	addi r4, r0, 1
	blt r3, r4, .LBB115_26
.LBB115_20:
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
	jal r0, .LBB115_26
.LBB115_21:
	addi r1, r0, 100
	bltu r11, r1, .LBB115_23
.LBB115_22:
	stb r24+0, r14
	jal r0, .LBB115_26
.LBB115_23:
	addi r1, r0, 1
	stb r24+0, r1
	stw r21+0, r11
	addi r3, r0, 6
	bgtu r11, r3, .LBB115_27
.LBB115_24:
	sll r4, r1, r11
	andi r4, r4, 97
	beq r4, r14, .LBB115_27
.LBB115_25:
	seq r3, r11, r3
	addi r4, r0, 5
	seq r4, r11, r4
	stb r20+0, r1
	lui r1, %hi(.L.str.55)
	addi r1, r1, %lo(.L.str.55)
	lui r5, %hi(.L.str.54)
	addi r5, r5, %lo(.L.str.54)
	xor r5, r5, r1
	sub r3, r0, r3
	and r3, r5, r3
	xor r1, r3, r1
	lui r3, %hi(.L.str.53)
	addi r3, r3, %lo(.L.str.53)
	xor r3, r1, r3
	sub r4, r0, r4
	and r3, r3, r4
	xor r4, r1, r3
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	jal r31, strcpy
	stw r22+0, r14
.LBB115_26:
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
.LBB115_27:
	slli r3, r11, 2
	lui r4, %hi(fio_ufile)
	addi r4, r4, %lo(fio_ufile)
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r14, .LBB115_29
.LBB115_28:
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
	jal r0, .LBB115_18
.LBB115_29:
	stb r20+0, r14
	jal r0, .LBB115_26
.Lfunc_end115:
	.size	f77_inquire, .Lfunc_end115-f77_inquire
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
	bgtu r1, r3, .LBB116_31
.LBB116_1:
	add r12, r5, r0
	add r11, r4, r0
	slli r1, r1, 2
	lui r3, %hi(.LJTI116_0)
	addi r3, r3, %lo(.LJTI116_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB116_2:
	lui r1, %hi(inq_exist)
	addi r1, r1, %lo(inq_exist)
	jal r0, .LBB116_29
.LBB116_3:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	addi r3, r0, 1
	bne r1, r3, .LBB116_31
.LBB116_4:
	lui r1, %hi(inq_direct)
	addi r1, r1, %lo(inq_direct)
	ldw r1, r1+0
	addi r3, r0, 0
	beq r1, r3, .LBB116_31
.LBB116_5:
	lui r1, %hi(inq_recl)
	addi r1, r1, %lo(inq_recl)
	ldw r1, r1+0
	jal r0, .LBB116_30
.LBB116_6:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	addi r15, r0, 1
	bne r1, r15, .LBB116_31
.LBB116_7:
	lui r1, %hi(inq_named)
	addi r1, r1, %lo(inq_named)
	ldw r1, r1+0
	addi r14, r0, 0
	beq r1, r14, .LBB116_31
.LBB116_8:
	lui r13, %hi(inq_name)
	addi r13, r13, %lo(inq_name)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r15, .LBB116_31
.LBB116_9:
	addi r3, r0, 32
	jal r0, .LBB116_11
.LBB116_10:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB116_31
.LBB116_11:
	add r4, r3, r0
	bge r14, r1, .LBB116_10
.LBB116_12:
	add r4, r14, r13
	ldbu r4, r4+0
	jal r0, .LBB116_10
.LBB116_13:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	addi r3, r0, 1
	bne r1, r3, .LBB116_31
.LBB116_14:
	lui r1, %hi(inq_direct)
	addi r1, r1, %lo(inq_direct)
	ldw r1, r1+0
	addi r3, r0, 0
	beq r1, r3, .LBB116_31
.LBB116_15:
	lui r1, %hi(inq_nextrec)
	addi r1, r1, %lo(inq_nextrec)
	ldw r1, r1+0
	jal r0, .LBB116_30
.LBB116_16:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_unf)
	addi r3, r3, %lo(inq_unf)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.58)
	addi r4, r4, %lo(.L.str.58)
	lui r5, %hi(.L.str.59)
	addi r5, r5, %lo(.L.str.59)
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
	blt r12, r3, .LBB116_31
.LBB116_17:
	addi r3, r0, 32
	jal r0, .LBB116_19
.LBB116_18:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB116_31
.LBB116_19:
	add r4, r3, r0
	bge r14, r1, .LBB116_18
.LBB116_20:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB116_18
.LBB116_21:
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
	jal r0, .LBB116_30
.LBB116_22:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_named)
	addi r3, r3, %lo(inq_named)
	ldw r3, r3+0
	sub r1, r0, r1
	and r1, r3, r1
	jal r0, .LBB116_30
.LBB116_23:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_direct)
	addi r3, r3, %lo(inq_direct)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.59)
	addi r4, r4, %lo(.L.str.59)
	lui r5, %hi(.L.str.58)
	addi r5, r5, %lo(.L.str.58)
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
	blt r12, r3, .LBB116_31
.LBB116_24:
	addi r3, r0, 32
	jal r0, .LBB116_26
.LBB116_25:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB116_31
.LBB116_26:
	add r4, r3, r0
	bge r14, r1, .LBB116_25
.LBB116_27:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB116_25
.LBB116_28:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
.LBB116_29:
	ldbu r1, r1+0
.LBB116_30:
	stw r11+0, r1
.LBB116_31:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB116_32:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_direct)
	addi r3, r3, %lo(inq_direct)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	lui r5, %hi(.L.str.57)
	addi r5, r5, %lo(.L.str.57)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r3, r4
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r1, r4
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB116_31
.LBB116_33:
	addi r3, r0, 32
	jal r0, .LBB116_35
.LBB116_34:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB116_31
.LBB116_35:
	add r4, r3, r0
	bge r14, r1, .LBB116_34
.LBB116_36:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB116_34
.LBB116_37:
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
	lui r5, %hi(.L.str.37)
	addi r5, r5, %lo(.L.str.37)
	lui r6, %hi(.L.str.61)
	addi r6, r6, %lo(.L.str.61)
	xor r6, r6, r5
	sub r4, r0, r4
	and r4, r6, r4
	xor r4, r4, r5
	lui r5, %hi(.L.str.56)
	addi r5, r5, %lo(.L.str.56)
	xor r4, r4, r5
	sub r3, r0, r3
	and r3, r4, r3
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r1, r5
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB116_31
.LBB116_38:
	addi r3, r0, 32
	jal r0, .LBB116_40
.LBB116_39:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB116_31
.LBB116_40:
	add r4, r3, r0
	bge r14, r1, .LBB116_39
.LBB116_41:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB116_39
.LBB116_42:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_direct)
	addi r3, r3, %lo(inq_direct)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.58)
	addi r4, r4, %lo(.L.str.58)
	lui r5, %hi(.L.str.59)
	addi r5, r5, %lo(.L.str.59)
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
	blt r12, r3, .LBB116_31
.LBB116_43:
	addi r3, r0, 32
	jal r0, .LBB116_45
.LBB116_44:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB116_31
.LBB116_45:
	add r4, r3, r0
	bge r14, r1, .LBB116_44
.LBB116_46:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB116_44
.LBB116_47:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_unf)
	addi r3, r3, %lo(inq_unf)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.59)
	addi r4, r4, %lo(.L.str.59)
	lui r5, %hi(.L.str.58)
	addi r5, r5, %lo(.L.str.58)
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
	blt r12, r3, .LBB116_31
.LBB116_48:
	addi r3, r0, 32
	jal r0, .LBB116_50
.LBB116_49:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB116_31
.LBB116_50:
	add r4, r3, r0
	bge r14, r1, .LBB116_49
.LBB116_51:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB116_49
.LBB116_52:
	lui r1, %hi(inq_opened)
	addi r1, r1, %lo(inq_opened)
	ldbu r1, r1+0
	lui r3, %hi(inq_unf)
	addi r3, r3, %lo(inq_unf)
	ldw r3, r3+0
	addi r14, r0, 0
	seq r3, r3, r14
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
	lui r5, %hi(.L.str.60)
	addi r5, r5, %lo(.L.str.60)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r3, r4
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r1, r4
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB116_31
.LBB116_53:
	addi r3, r0, 32
	jal r0, .LBB116_55
.LBB116_54:
	add r5, r11, r14
	stb r5+0, r4
	addi r14, r14, 1
	beq r12, r14, .LBB116_31
.LBB116_55:
	add r4, r3, r0
	bge r14, r1, .LBB116_54
.LBB116_56:
	add r4, r13, r14
	ldbu r4, r4+0
	jal r0, .LBB116_54
.Lfunc_end116:
	.size	f77_inq, .Lfunc_end116-f77_inq
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI116_0,@object
.LJTI116_0:
	.word	.LBB116_2
	.word	.LBB116_28
	.word	.LBB116_21
	.word	.LBB116_22
	.word	.LBB116_6
	.word	.LBB116_32
	.word	.LBB116_42
	.word	.LBB116_23
	.word	.LBB116_52
	.word	.LBB116_16
	.word	.LBB116_47
	.word	.LBB116_3
	.word	.LBB116_13
	.word	.LBB116_37
	.size	.LJTI116_0, 56
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
	bne r3, r12, .LBB117_4
.LBB117_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB117_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r12, .LBB117_2
.LBB117_3:
	addi r3, r0, 1
	stb r1+0, r3
.LBB117_4:
	addi r1, r11, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB117_7
.LBB117_5:
	addi r1, r11, -5
	addi r3, r0, 1
	bleu r1, r3, .LBB117_7
.LBB117_6:
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
.LBB117_7:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.62)
	addi r4, r4, %lo(.L.str.62)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end117:
	.size	f77_rewind, .Lfunc_end117-f77_rewind
                                        # -- End function
	.p2align	2                               # -- Begin function fio_fmt_use
	.type	fio_fmt_use,@function
fio_fmt_use:                            # @fio_fmt_use
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
	lui r16, %hi(fio_fbuf)
	addi r16, r16, %lo(fio_fbuf)
	beq r3, r16, .LBB118_145
.LBB118_1:
	add r12, r3, r0
	lui r10, %hi(fio_ftext)
	addi r10, r10, %lo(fio_ftext)
	addi r1, r0, -1
	stw sp+52, r1
	stw r10+0, r1
	addi r25, r0, 0
	addi r21, r0, 32
.LBB118_2:
	add r1, r12, r25
	ldbu r1, r1+0
	bne r1, r21, .LBB118_4
.LBB118_3:
	addi r25, r25, 1
	jal r0, .LBB118_2
.LBB118_4:
	addi r15, r0, 40
	lui r14, %hi(fio_fmt_why)
	addi r14, r14, %lo(fio_fmt_why)
	bne r1, r15, .LBB118_146
.LBB118_5:
	addi lr, r0, 0
	addi r23, r0, 1022
	lui r24, %hi(.L.str.68)
	addi r24, r24, %lo(.L.str.68)
	addi r1, r0, 1
	stw sp+48, r1
	slli r1, r1, 2
	lui r26, %hi(.LJTI118_1)
	addi r26, r26, %lo(.LJTI118_1)
	add r27, r26, r1
	addi r17, r0, 39
	addi r18, r0, -10
	addi r20, r0, 41
	addi r19, r0, 10
	add r22, lr, r0
	add fp, lr, r0
.LBB118_6:
	blt r22, r23, .LBB118_8
.LBB118_7:
	stw r14+0, r24
	ldw r3, r27+0
	add r1, r22, r0
	jalr r0, r3, 0
.LBB118_8:
	add r1, r12, r25
	ldb r11, r1+0
	andi r3, r11, 255
	bgtu r3, r17, .LBB118_22
.LBB118_9:
	slli r1, r3, 2
	lui r4, %hi(.LJTI118_0)
	addi r4, r4, %lo(.LJTI118_0)
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB118_10:
	add r1, r22, r16
	stb r1+0, r17
	addi r1, r22, 1
	addi r4, r25, 1
	addi r5, r0, 1019
	bgt r22, r5, .LBB118_19
.LBB118_11:
	add r6, r12, r4
	ldbu r7, r6+0
	andi r8, r7, 255
	addi r6, r0, 0
	beq r8, r6, .LBB118_19
.LBB118_12:
	andi r7, r7, 255
	bne r7, r3, .LBB118_14
.LBB118_13:
	addi r4, r25, 2
	add r7, r12, r4
	ldbu r7, r7+0
	bne r7, r3, .LBB118_37
.LBB118_14:
	add r25, r4, r0
	add r4, r12, r4
	ldbu r7, r4+0
	bne r7, r17, .LBB118_16
.LBB118_15:
	addi r22, r22, 2
	add r1, r1, r16
	stb r1+0, r17
	jal r0, .LBB118_17
.LBB118_16:
	add r22, r1, r0
.LBB118_17:
	ldbu r1, r4+0
	add r4, r22, r16
	stb r4+0, r1
	addi r1, r22, 1
	addi r4, r25, 1
	bgt r22, r5, .LBB118_19
.LBB118_18:
	add r7, r12, r4
	ldbu r7, r7+0
	andi r8, r7, 255
	bne r8, r6, .LBB118_12
.LBB118_19:
	lui r3, %hi(.L.str.70)
	addi r3, r3, %lo(.L.str.70)
	stw r14+0, r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r26, r3
	ldw r3, r3+0
	add r25, r4, r0
	add r22, r1, r0
	jalr r0, r3, 0
.LBB118_20:
	addi r25, r25, 1
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r26, r1
	ldw r3, r1+0
	add r1, r22, r0
	jalr r0, r3, 0
.LBB118_21:
	lui r1, %hi(.L.str.69)
	addi r1, r1, %lo(.L.str.69)
	stw r14+0, r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r26, r1
	ldw r3, r1+0
	add r1, r22, r0
	jalr r0, r3, 0
.LBB118_22:
	addi r1, r11, -58
	bltu r1, r18, .LBB118_33
.LBB118_23:
	add r1, lr, r0
	beq r22, lr, .LBB118_32
.LBB118_24:
	ldw r3, r10+0
	add r1, lr, r0
	beq r22, r3, .LBB118_32
.LBB118_25:
	stw sp+44, r15
	add r15, r14, r0
	add r14, lr, r0
	add r28, r18, r0
	add r18, r10, r0
	lui r1, %hi(fio_fbuf-1)
	addi r1, r1, %lo(fio_fbuf-1)
	add r1, r22, r1
	ldbu r3, r1+0
	jal r31, toupper
	add r13, r1, r0
	addi r3, r0, 2
	ldw r1, sp+48
	blt r22, r3, .LBB118_27
.LBB118_26:
	lui r1, %hi(fio_fbuf-2)
	addi r1, r1, %lo(fio_fbuf-2)
	add r1, r22, r1
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 66
	sne r1, r1, r3
.LBB118_27:
	addi r3, r0, 90
	add r10, r18, r0
	add r18, r28, r0
	add lr, r14, r0
	add r14, r15, r0
	ldw r15, sp+44
	beq r13, r3, .LBB118_32
.LBB118_28:
	addi r1, r0, -4
	and r3, r13, r1
	addi r1, r0, 1
	addi r4, r0, 68
	beq r3, r4, .LBB118_32
.LBB118_29:
	addi r3, r13, -65
	addi r4, r0, 19
	bgtu r3, r4, .LBB118_31
.LBB118_30:
	lui r4, 165
	addi r4, r4, -1791
	srl r3, r4, r3
	andi r3, r3, 1
	addi r4, r0, 0
	bne r3, r4, .LBB118_32
.LBB118_31:
	addi r1, r0, 46
	seq r1, r13, r1
.LBB118_32:
	beq r1, lr, .LBB118_38
.LBB118_33:
	addi r1, r22, 1
	add r3, r22, r16
	stb r3+0, r11
	addi r25, r25, 1
	seq r3, r11, r15
	add fp, fp, r3
	bne r11, r20, .LBB118_35
.LBB118_34:
	addi fp, fp, -1
	seq r3, fp, lr
	sub r3, r0, r3
	andi r3, r3, 4
	slli r3, r3, 2
	add r3, r26, r3
	ldw r3, r3+0
	add r22, r1, r0
	jalr r0, r3, 0
.LBB118_35:
	add r22, r1, r0
	add r3, lr, r0
.LBB118_36:
	slli r3, r3, 2
	add r3, r26, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB118_37:
	addi r22, r22, 2
	add r1, r1, r16
	stb r1+0, r17
	stw r10+0, r22
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r26, r1
	ldw r3, r1+0
	add r25, r4, r0
	add r1, r22, r0
	jalr r0, r3, 0
.LBB118_38:
	add r1, lr, r0
	jal r0, .LBB118_41
.LBB118_39:
	andi r5, r4, 255
	bne r5, r21, .LBB118_45
.LBB118_40:
	addi r25, r25, 1
.LBB118_41:
	add r3, r12, r25
	ldb r4, r3+0
	addi r5, r4, -48
	bgeu r5, r19, .LBB118_39
.LBB118_42:
	beq r4, r21, .LBB118_40
.LBB118_43:
	bge r22, r23, .LBB118_57
.LBB118_44:
	mul r1, r1, r19
	add r1, r5, r1
	addi r3, r22, 1
	add r5, r22, r16
	stb r5+0, r4
	add r22, r3, r0
	jal r0, .LBB118_40
.LBB118_45:
	addi r6, r0, 72
	beq r5, r6, .LBB118_47
.LBB118_46:
	addi r6, r0, 104
	bne r5, r6, .LBB118_50
.LBB118_47:
	addi r5, r0, 0
	beq r1, r5, .LBB118_51
.LBB118_48:
	add r6, r1, r22
	addi r7, r0, 1022
	blt r6, r7, .LBB118_52
.LBB118_49:
	lui r1, %hi(.L.str.71)
	addi r1, r1, %lo(.L.str.71)
	stw r14+0, r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r26, r1
	ldw r3, r1+0
	add r1, r22, r0
	jalr r0, r3, 0
.LBB118_50:
	addi r3, r0, 5
	add r1, r22, r0
	jal r0, .LBB118_36
.LBB118_51:
	lui r1, %hi(.L.str.72)
	addi r1, r1, %lo(.L.str.72)
	stw r14+0, r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r26, r1
	ldw r3, r1+0
	add r1, r22, r0
	jalr r0, r3, 0
.LBB118_52:
	add r6, r22, r16
	stb r6+0, r4
	addi r22, r22, 1
	addi r4, r0, 1
	blt r1, r4, .LBB118_58
.LBB118_53:
	addi r6, r1, 1
	add r1, r5, r0
.LBB118_54:
	add r7, r3, r1
	ldbu r7, r7+1
	beq r7, r5, .LBB118_60
.LBB118_55:
	add r8, r22, r1
	add r8, r8, r16
	stb r8+0, r7
	addi r6, r6, -1
	addi r1, r1, 1
	bgt r6, r4, .LBB118_54
.LBB118_56:
	add r3, r25, r1
	addi r25, r3, 1
	add r22, r22, r1
	jal r0, .LBB118_59
.LBB118_57:
	lui r1, %hi(.L.str.71)
	addi r1, r1, %lo(.L.str.71)
	stw r14+0, r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r26, r1
	ldw r3, r1+0
	add r22, r23, r0
	add r1, r23, r0
	jalr r0, r3, 0
.LBB118_58:
	addi r25, r25, 1
.LBB118_59:
	stw r10+0, r22
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r26, r1
	ldw r3, r1+0
	add r1, r22, r0
	jalr r0, r3, 0
.LBB118_60:
	lui r3, %hi(.L.str.73)
	addi r3, r3, %lo(.L.str.73)
	stw r14+0, r3
	add r3, r25, r1
	addi r25, r3, 1
	add r22, r22, r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r26, r1
	ldw r3, r1+0
	add r1, r22, r0
	jalr r0, r3, 0
.LBB118_61:
	add r1, r1, r16
	addi r26, r0, 0
	stb r1+0, r26
	addi r13, r0, 1
	addi r22, r0, 58
	addi r23, r0, -1
	addi r24, r0, 9
	lui r25, %hi(fio_fbuf+1)
	addi r25, r25, %lo(fio_fbuf+1)
	addi r27, r0, 72
	lui r1, %hi(.L.str.75)
	addi r1, r1, %lo(.L.str.75)
	stw sp+32, r1
	addi r28, r0, 80
	addi fp, r0, 88
	addi r1, r0, 25
	stw sp+48, r1
	addi r1, r0, -4
	stw sp+40, r1
	addi r1, r0, 68
	stw sp+44, r1
	addi r1, r0, 46
	stw sp+36, r1
	lui r1, %hi(.L.str.79)
	addi r1, r1, %lo(.L.str.79)
	stw sp+24, r1
	stw sp+28, r13
	jal r0, .LBB118_63
.LBB118_62:
	addi r13, r13, 1
.LBB118_63:
	add r1, r13, r16
	ldbu r1, r1+0
	bgtu r1, r22, .LBB118_75
.LBB118_64:
	slli r3, r1, 2
	lui r4, %hi(.LJTI118_2)
	addi r4, r4, %lo(.LJTI118_2)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB118_65:
	addi r21, r13, 1
	add r1, r13, r25
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r18, .LBB118_71
.LBB118_66:
	addi r20, r0, -1
	jal r0, .LBB118_73
.LBB118_67:
	addi r13, r13, 1
.LBB118_68:
	add r1, r13, r0
	add r3, r13, r16
	ldbu r3, r3+0
	addi r13, r13, 1
	bne r3, r17, .LBB118_68
.LBB118_69:
	add r3, r13, r16
	ldbu r3, r3+0
	bne r3, r17, .LBB118_63
.LBB118_70:
	addi r13, r1, 2
	jal r0, .LBB118_68
.LBB118_71:
	addi r20, r0, 0
	addi r1, r1, -48
	bgtu r1, r24, .LBB118_73
.LBB118_72:
	add r3, r21, r0
	mul r4, r20, r19
	add r20, r4, r1
	addi r21, r21, 1
	add r1, r3, r25
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r19, .LBB118_72
.LBB118_73:
	lui r13, %hi(.L.str.74)
	addi r13, r13, %lo(.L.str.74)
	addi r1, r0, 0
	blt r20, r1, .LBB118_147
.LBB118_74:
	add r1, r21, r16
	ldbu r3, r1+0
	jal r31, toupper
	beq r1, r28, .LBB118_81
	jal r0, .LBB118_147
.LBB118_75:
	slli r1, r1, 24
	srai r1, r1, 24
	addi r3, r1, -58
	bgeu r3, r18, .LBB118_77
.LBB118_76:
	add r21, r13, r0
	add r20, r23, r0
	jal r0, .LBB118_81
.LBB118_77:
	addi r1, r1, -48
	bleu r1, r24, .LBB118_79
.LBB118_78:
	add r21, r13, r0
	add r20, r26, r0
	jal r0, .LBB118_81
.LBB118_79:
	add r20, r26, r0
	add r21, r13, r0
.LBB118_80:
	add r3, r21, r0
	mul r4, r20, r19
	add r20, r4, r1
	addi r21, r21, 1
	add r1, r3, r25
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r19, .LBB118_80
.LBB118_81:
	add r11, r21, r16
	ldbu r3, r11+0
	jal r31, toupper
	bne r1, r27, .LBB118_84
.LBB118_82:
	blt r20, r26, .LBB118_157
.LBB118_83:
	add r1, r21, r20
	addi r13, r1, 1
	jal r0, .LBB118_63
.LBB118_84:
	blt r20, r26, .LBB118_87
.LBB118_85:
	bne r1, r15, .LBB118_87
.LBB118_86:
	addi r13, r21, 1
	jal r0, .LBB118_63
.LBB118_87:
	beq r1, fp, .LBB118_94
.LBB118_88:
	sgt r3, r20, r23
	seq r4, r1, r28
	and r3, r3, r4
	bne r3, r26, .LBB118_94
.LBB118_89:
	addi r3, r1, -65
	ldw r4, sp+48
	bgtu r3, r4, .LBB118_98
.LBB118_90:
	slli r3, r3, 2
	lui r4, %hi(.LJTI118_3)
	addi r4, r4, %lo(.LJTI118_3)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB118_91:
	addi r13, r21, 1
	add r1, r21, r25
.LBB118_92:
	ldb r1, r1+0
	addi r1, r1, -58
	bltu r1, r18, .LBB118_63
.LBB118_93:
	add r1, r13, r0
	addi r13, r13, 1
	add r1, r1, r25
	jal r0, .LBB118_92
.LBB118_94:
	addi r13, r21, 1
	jal r0, .LBB118_63
.LBB118_95:
	add r1, r21, r25
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 82
	beq r1, r3, .LBB118_97
.LBB118_96:
	addi r3, r0, 76
	bne r1, r3, .LBB118_118
.LBB118_97:
	addi r13, r21, 2
	jal r0, .LBB118_119
.LBB118_98:
	ldw r3, sp+40
	and r3, r1, r3
	ldw r4, sp+44
	beq r3, r4, .LBB118_101
.LBB118_99:
	addi r3, r0, 76
	beq r1, r3, .LBB118_101
.LBB118_100:
	addi r3, r0, 73
	bne r1, r3, .LBB118_162
.LBB118_101:
	add r3, r21, r25
	ldb r4, r3+0
	addi r3, r4, -58
	bltu r3, r18, .LBB118_158
.LBB118_102:
	addi r3, r21, 1
	addi r4, r4, -48
	bleu r4, r24, .LBB118_110
.LBB118_103:
	add r4, r26, r0
	jal r0, .LBB118_113
.LBB118_104:
	add r1, r21, r25
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 90
	beq r1, r3, .LBB118_106
.LBB118_105:
	addi r3, r0, 78
	bne r1, r3, .LBB118_160
.LBB118_106:
	addi r13, r21, 2
	jal r0, .LBB118_63
.LBB118_107:
	add r1, r21, r25
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 83
	beq r1, r3, .LBB118_109
.LBB118_108:
	addi r13, r21, 1
	bne r1, r28, .LBB118_63
.LBB118_109:
	addi r13, r21, 2
	jal r0, .LBB118_63
.LBB118_110:
	addi r5, r0, 0
.LBB118_111:
	add r6, r3, r0
	mul r3, r5, r19
	add r5, r3, r4
	addi r3, r6, 1
	add r4, r6, r25
	ldb r4, r4+0
	addi r4, r4, -48
	bltu r4, r19, .LBB118_111
.LBB118_112:
	srli r4, r5, 31
.LBB118_113:
	bne r4, r26, .LBB118_158
.LBB118_114:
	add r4, r3, r16
	ldbu r4, r4+0
	ldw r5, sp+36
	bne r4, r5, .LBB118_117
.LBB118_115:
	addi r13, r3, 1
	add r3, r3, r25
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r18, .LBB118_127
.LBB118_116:
	ldw r3, sp+28
	jal r0, .LBB118_132
.LBB118_117:
	add r13, r3, r0
	jal r0, .LBB118_133
.LBB118_118:
	addi r13, r21, 1
.LBB118_119:
	add r1, r13, r16
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r18, .LBB118_121
.LBB118_120:
	addi r1, r0, 1
	jal r0, .LBB118_126
.LBB118_121:
	addi r3, r1, -48
	bleu r3, r24, .LBB118_123
.LBB118_122:
	addi r1, r0, 0
	jal r0, .LBB118_126
.LBB118_123:
	addi r1, r0, 0
.LBB118_124:
	add r4, r13, r0
	mul r1, r1, r19
	add r1, r1, r3
	addi r13, r13, 1
	add r3, r4, r25
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r19, .LBB118_124
.LBB118_125:
	srli r1, r1, 31
.LBB118_126:
	addi r3, r0, 0
	beq r1, r3, .LBB118_63
	jal r0, .LBB118_159
.LBB118_127:
	addi r4, r3, -48
	bleu r4, r24, .LBB118_129
.LBB118_128:
	addi r3, r0, 0
	jal r0, .LBB118_132
.LBB118_129:
	addi r3, r0, 0
.LBB118_130:
	add r5, r13, r0
	mul r3, r3, r19
	add r3, r3, r4
	addi r13, r13, 1
	add r4, r5, r25
	ldb r4, r4+0
	addi r4, r4, -48
	bltu r4, r19, .LBB118_130
.LBB118_131:
	srli r3, r3, 31
.LBB118_132:
	bne r3, r26, .LBB118_161
.LBB118_133:
	ldw r3, sp+44
	beq r1, r3, .LBB118_135
.LBB118_134:
	andi r1, r1, 77
	addi r3, r0, 69
	bne r1, r3, .LBB118_63
.LBB118_135:
	add r1, r13, r16
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 69
	bne r1, r3, .LBB118_63
.LBB118_136:
	addi r1, r13, 1
	add r3, r13, r25
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r18, .LBB118_138
.LBB118_137:
	addi r3, r0, 1
	jal r0, .LBB118_143
.LBB118_138:
	addi r4, r3, -48
	bleu r4, r24, .LBB118_140
.LBB118_139:
	addi r3, r0, 0
	jal r0, .LBB118_143
.LBB118_140:
	addi r3, r0, 0
.LBB118_141:
	add r5, r1, r0
	mul r1, r3, r19
	add r3, r1, r4
	addi r1, r5, 1
	add r4, r5, r25
	ldb r4, r4+0
	addi r4, r4, -48
	bltu r4, r19, .LBB118_141
.LBB118_142:
	srli r3, r3, 31
.LBB118_143:
	add r13, r1, r0
	addi r1, r0, 0
	beq r3, r1, .LBB118_63
.LBB118_144:
	lui r13, %hi(.L.str.80)
	addi r13, r13, %lo(.L.str.80)
	jal r0, .LBB118_147
.LBB118_145:
	lui r1, %hi(fio_fmt)
	addi r1, r1, %lo(fio_fmt)
	stw r1+0, r16
	addi r1, r0, 0
	jal r0, .LBB118_156
.LBB118_146:
	lui r13, %hi(.L.str.67)
	addi r13, r13, %lo(.L.str.67)
.LBB118_147:
	stw r14+0, r13
.LBB118_148:
	ldbu r7, r12+0
	addi r6, r0, 0
	beq r7, r6, .LBB118_155
.LBB118_149:
	addi r1, r0, 0
	addi r6, r0, 1
	addi r3, r0, 41
	addi r4, r0, 79
	add r5, r1, r0
.LBB118_150:
	andi r7, r7, 255
	seq r8, r7, r15
	add r5, r5, r8
	bne r7, r3, .LBB118_152
.LBB118_151:
	addi r5, r5, -1
	beq r5, r1, .LBB118_155
.LBB118_152:
	add r7, r12, r6
	ldbu r7, r7+0
	addi r8, r6, 1
	beq r7, r1, .LBB118_154
.LBB118_153:
	addi r9, r6, -1
	add r6, r8, r0
	bltu r9, r4, .LBB118_150
.LBB118_154:
	addi r6, r8, -1
.LBB118_155:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	ldw r5, r14+0
	lui r4, %hi(.L.str.84)
	addi r4, r4, %lo(.L.str.84)
	add r7, r12, r0
	jal r31, fprintf
	ldw r1, sp+52
.LBB118_156:
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
.LBB118_157:
	ldw r13, sp+32
	jal r0, .LBB118_147
.LBB118_158:
	lui r13, %hi(fio_fmt_check.why)
	addi r13, r13, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.78)
	addi r4, r4, %lo(.L.str.78)
	add r3, r13, r0
	add r5, r1, r0
	jal r31, sprintf
	jal r0, .LBB118_147
.LBB118_159:
	lui r13, %hi(.L.str.76)
	addi r13, r13, %lo(.L.str.76)
	jal r0, .LBB118_147
.LBB118_160:
	lui r13, %hi(.L.str.77)
	addi r13, r13, %lo(.L.str.77)
	jal r0, .LBB118_147
.LBB118_161:
	ldw r13, sp+24
	jal r0, .LBB118_147
.LBB118_162:
	addi r3, r0, 0
	blt r20, r3, .LBB118_165
.LBB118_163:
	addi r1, r1, -41
	addi r3, r0, 17
	bgtu r1, r3, .LBB118_166
.LBB118_164:
	slli r1, r1, 2
	lui r3, %hi(.Lswitch.table.fio_fmt_use)
	addi r3, r3, %lo(.Lswitch.table.fio_fmt_use)
	add r1, r1, r3
	ldw r13, r1+0
	jal r0, .LBB118_147
.LBB118_165:
	ldb r5, r11+0
	lui r13, %hi(fio_fmt_check.why)
	addi r13, r13, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.83)
	addi r4, r4, %lo(.L.str.83)
	add r3, r13, r0
	jal r31, sprintf
	jal r0, .LBB118_147
.LBB118_166:
	lui r13, %hi(.L.str.82)
	addi r13, r13, %lo(.L.str.82)
	jal r0, .LBB118_147
.LBB118_167:
.Lfunc_end118:
	.size	fio_fmt_use, .Lfunc_end118-fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI118_0,@object
.LJTI118_0:
	.word	.LBB118_21
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_20
	.word	.LBB118_22
	.word	.LBB118_10
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_22
	.word	.LBB118_10
	.size	.LJTI118_0, 160
	.type	.LJTI118_1,@object
.LJTI118_1:
	.word	.LBB118_6
	.word	.LBB118_148
	.word	.LBB118_167
	.word	.LBB118_167
	.word	.LBB118_61
	.word	.LBB118_6
	.size	.LJTI118_1, 24
	.type	.LJTI118_2,@object
.LJTI118_2:
	.word	.LBB118_145
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_67
	.word	.LBB118_62
	.word	.LBB118_62
	.word	.LBB118_75
	.word	.LBB118_65
	.word	.LBB118_62
	.word	.LBB118_65
	.word	.LBB118_75
	.word	.LBB118_62
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_75
	.word	.LBB118_62
	.size	.LJTI118_2, 236
	.type	.LJTI118_3,@object
.LJTI118_3:
	.word	.LBB118_91
	.word	.LBB118_104
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_91
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_107
	.word	.LBB118_95
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_98
	.word	.LBB118_91
	.size	.LJTI118_3, 104
                                        # -- End function
	.text
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
	bleu r5, r6, .LBB119_2
.LBB119_1:
	addi r1, r0, 0
	jal r0, .LBB119_5
.LBB119_2:
	addi r6, r1, 1
	addi r1, r0, 0
	addi r7, r0, 10
.LBB119_3:
	mul r1, r1, r7
	addi r8, r4, 1
	add r1, r5, r1
	add r4, r6, r4
	ldb r4, r4+0
	addi r5, r4, -48
	add r4, r8, r0
	bltu r5, r7, .LBB119_3
.LBB119_4:
	stw r3+0, r8
.LBB119_5:
	jalr r0, r31, 0
.Lfunc_end119:
	.size	fio_number, .Lfunc_end119-fio_number
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
	beq r1, r11, .LBB120_3
.LBB120_1:
	lui r12, %hi(fio_irec)
	addi r12, r12, %lo(fio_irec)
	ldw r3, r12+0
	lui r4, %hi(fio_inrec)
	addi r4, r4, %lo(fio_inrec)
	ldw r4, r4+0
	bge r3, r4, .LBB120_21
.LBB120_2:
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
	jal r0, .LBB120_25
.LBB120_3:
	lui r12, %hi(fio_unit)
	addi r12, r12, %lo(fio_unit)
	ldw r1, r12+0
	addi r3, r1, -1
	addi r13, r0, 98
	lui r11, %hi(fio_in)
	addi r11, r11, %lo(fio_in)
	bgtu r3, r13, .LBB120_7
.LBB120_4:
	slli r1, r1, 2
	lui r14, %hi(fio_uend)
	addi r14, r14, %lo(fio_uend)
	add r1, r1, r14
	ldw r3, r1+0
	addi r1, r0, 0
	blt r3, r1, .LBB120_7
.LBB120_5:
	ldw r3, r11+0
	beq r3, r1, .LBB120_7
.LBB120_6:
	jal r31, ftell
	ldw r3, r12+0
	slli r3, r3, 2
	add r4, r3, r14
	ldw r4, r4+0
	bge r1, r4, .LBB120_22
.LBB120_7:
	ldw r1, r12+0
	addi r3, r1, -1
	bgtu r3, r13, .LBB120_11
.LBB120_8:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	addi r14, r0, 0
	beq r3, r14, .LBB120_11
.LBB120_9:
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
	beq r1, r14, .LBB120_25
.LBB120_10:
	lui r3, %hi(fio_rlen)
	addi r3, r3, %lo(fio_rlen)
	stw r3+0, r1
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	stw r1+0, r14
	add r11, r14, r0
	jal r0, .LBB120_25
.LBB120_11:
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
	beq r1, r11, .LBB120_19
.LBB120_12:
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	jal r31, strlen
	addi r3, r0, 1
	blt r1, r3, .LBB120_18
.LBB120_13:
	addi r4, r1, 1
	lui r5, %hi(fio_rec-2)
	addi r5, r5, %lo(fio_rec-2)
	addi r6, r0, 13
	addi r1, r0, 0
	addi r7, r0, 10
	jal r0, .LBB120_15
.LBB120_14:
	addi r4, r4, -1
	ble r4, r3, .LBB120_18
.LBB120_15:
	add r8, r4, r5
	ldbu r8, r8+0
	beq r8, r6, .LBB120_14
.LBB120_16:
	beq r8, r7, .LBB120_14
.LBB120_17:
	addi r1, r4, -1
.LBB120_18:
	lui r3, %hi(fio_rlen)
	addi r3, r3, %lo(fio_rlen)
	stw r3+0, r1
	lui r1, %hi(fio_rpos)
	addi r1, r1, %lo(fio_rpos)
	jal r0, .LBB120_24
.LBB120_19:
	ldw r1, r12+0
	addi r3, r1, -1
	bgtu r3, r13, .LBB120_21
.LBB120_20:
	slli r1, r1, 2
	lui r3, %hi(fio_uaeof)
	addi r3, r3, %lo(fio_uaeof)
	add r1, r1, r3
	jal r0, .LBB120_23
.LBB120_21:
	addi r11, r0, 1
	jal r0, .LBB120_25
.LBB120_22:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r3, r1
.LBB120_23:
	addi r11, r0, 1
.LBB120_24:
	stw r1+0, r11
.LBB120_25:
	add r1, r11, r0
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end120:
	.size	fio_getrec, .Lfunc_end120-fio_getrec
                                        # -- End function
	.p2align	2                               # -- Begin function nml_text_add
	.type	nml_text_add,@function
nml_text_add:                           # @nml_text_add
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
	add r12, r3, r0
	lui r14, %hi(nml_tlen)
	addi r14, r14, %lo(nml_tlen)
	ldw r1, r14+0
	add r1, r4, r1
	addi r1, r1, 2
	lui r17, %hi(nml_tcap)
	addi r17, r17, %lo(nml_tcap)
	ldw r3, r17+0
	addi r15, r0, 0
	lui r16, %hi(nml_text)
	addi r16, r16, %lo(nml_text)
	ble r1, r3, .LBB121_5
.LBB121_1:
	seq r4, r3, r15
	slli r3, r3, 1
	lui r5, 1
	xor r5, r3, r5
	sub r4, r0, r4
	and r4, r5, r4
	xor r3, r3, r4
.LBB121_2:
	add r13, r3, r0
	slli r3, r3, 1
	blt r13, r1, .LBB121_2
.LBB121_3:
	ldw r3, r16+0
	add r4, r13, r0
	jal r31, realloc
	beq r1, r15, .LBB121_6
.LBB121_4:
	stw r16+0, r1
	stw r17+0, r13
.LBB121_5:
	ldw r1, r16+0
	ldw r3, r14+0
	add r3, r1, r3
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r14+0
	add r1, r1, r11
	stw r14+0, r1
	ldw r3, r16+0
	add r1, r3, r1
	stb r1+0, r15
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
.LBB121_6:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end121:
	.size	nml_text_add, .Lfunc_end121-nml_text_add
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
	.comm	fio_uunf,400,4
	.type	fio_uaeof,@object               # @fio_uaeof
	.local	fio_uaeof
	.comm	fio_uaeof,400,4
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
	.comm	fio_udirect,400,4
	.type	fio_urecl,@object               # @fio_urecl
	.local	fio_urecl
	.comm	fio_urecl,400,4
	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"f77: unformatted record longer than RECL\n"
	.size	.L.str.1, 42

	.type	fio_uend,@object                # @fio_uend
	.local	fio_uend
	.comm	fio_uend,400,4
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
	.type	fio_desc,@object                # @fio_desc
	.local	fio_desc
	.comm	fio_desc,4,4
	.type	fio_rep,@object                 # @fio_rep
	.local	fio_rep
	.comm	fio_rep,4,4
	.type	fio_draining,@object            # @fio_draining
	.local	fio_draining
	.comm	fio_draining,1,4
	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"PAUSE %.*s\n"
	.size	.L.str.17, 12

	.type	fio_ubz,@object                 # @fio_ubz
	.local	fio_ubz
	.comm	fio_ubz,400,4
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
	.asciz	"f77: %c editing does not match a numeric READ item\n"
	.size	.L.str.18, 52

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"f77: %c editing does not match a CHARACTER READ item\n"
	.size	.L.str.19, 54

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
	.type	nml_unit,@object                # @nml_unit
	.local	nml_unit
	.comm	nml_unit,4,4
	.type	nml_reading,@object             # @nml_reading
	.local	nml_reading
	.comm	nml_reading,4,4
	.type	nml_flags,@object               # @nml_flags
	.local	nml_flags
	.comm	nml_flags,4,4
	.type	nml_status,@object              # @nml_status
	.local	nml_status
	.comm	nml_status,4,4
	.type	nml_ndim,@object                # @nml_ndim
	.local	nml_ndim
	.comm	nml_ndim,4,4
	.type	nml_err,@object                 # @nml_err
	.local	nml_err
	.comm	nml_err,96,4
	.type	nml_grp,@object                 # @nml_grp
	.local	nml_grp
	.comm	nml_grp,32,1
	.type	nml_width,@object               # @nml_width
	.local	nml_width
	.comm	nml_width,1,4
	.type	nml_line,@object                # @nml_line
	.local	nml_line
	.comm	nml_line,1024,4
	.type	nml_len,@object                 # @nml_len
	.local	nml_len
	.comm	nml_len,4,4
	.type	nml_npend,@object               # @nml_npend
	.local	nml_npend
	.comm	nml_npend,4,4
	.type	nml_nasg,@object                # @nml_nasg
	.local	nml_nasg
	.comm	nml_nasg,4,4
	.type	nml_nval,@object                # @nml_nval
	.local	nml_nval
	.comm	nml_nval,4,4
	.type	nml_lo,@object                  # @nml_lo
	.local	nml_lo
	.comm	nml_lo,28,4
	.type	nml_ext,@object                 # @nml_ext
	.local	nml_ext
	.comm	nml_ext,28,4
	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"(I11)"
	.size	.L.str.20, 6

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"T"
	.size	.L.str.21, 2

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"F"
	.size	.L.str.22, 2

	.type	nml_asg,@object                 # @nml_asg
	.local	nml_asg
	.comm	nml_asg,4,4
	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"the subscripts do not match the array"
	.size	.L.str.23, 38

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"a subscript is out of range"
	.size	.L.str.24, 28

	.type	nml_val,@object                 # @nml_val
	.local	nml_val
	.comm	nml_val,4,4
	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"more values than the item has elements"
	.size	.L.str.25, 39

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	" &END"
	.size	.L.str.26, 6

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"%s is no item of the NAMELIST"
	.size	.L.str.27, 30

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"f77: end of file on unit %d looking for NAMELIST &%s\n"
	.size	.L.str.28, 54

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"f77: NAMELIST &%s on unit %d: %s\n"
	.size	.L.str.29, 34

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"f77: cannot OPEN unit %d\n"
	.size	.L.str.30, 26

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"DIRECT"
	.size	.L.str.31, 7

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"UNFORMATTED"
	.size	.L.str.32, 12

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"SCRATCH"
	.size	.L.str.33, 8

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"f77: OPEN ACCESS='DIRECT' needs RECL=\n"
	.size	.L.str.34, 39

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"f77: bad FILE= name in OPEN\n"
	.size	.L.str.35, 29

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"fort.%d"
	.size	.L.str.36, 8

	.type	fio_ufile,@object               # @fio_ufile
	.local	fio_ufile
	.comm	fio_ufile,400,4
	.type	fio_ufname,@object              # @fio_ufname
	.local	fio_ufname
	.comm	fio_ufname,25600,1
	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"ZERO"
	.size	.L.str.37, 5

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"f77: OPEN cannot create a scratch file\n"
	.size	.L.str.38, 40

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"OLD"
	.size	.L.str.39, 4

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"r+"
	.size	.L.str.40, 3

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"r"
	.size	.L.str.41, 2

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"f77: OPEN STATUS='OLD': no such file '%s'\n"
	.size	.L.str.42, 43

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"NEW"
	.size	.L.str.43, 4

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"f77: OPEN STATUS='NEW': '%s' already exists\n"
	.size	.L.str.44, 45

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"w+"
	.size	.L.str.45, 3

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"f77: OPEN cannot create '%s'\n"
	.size	.L.str.46, 30

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"UNKNOWN"
	.size	.L.str.47, 8

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"f77: OPEN cannot open '%s'\n"
	.size	.L.str.48, 28

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"f77: OPEN STATUS value is not supported\n"
	.size	.L.str.49, 41

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"DELETE"
	.size	.L.str.50, 7

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"KEEP"
	.size	.L.str.51, 5

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"f77: CLOSE STATUS value is not supported\n"
	.size	.L.str.52, 42

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
	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"stdin"
	.size	.L.str.53, 6

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"stdout"
	.size	.L.str.54, 7

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"stderr"
	.size	.L.str.55, 7

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"UNDEFINED"
	.size	.L.str.56, 10

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"SEQUENTIAL"
	.size	.L.str.57, 11

	.type	.L.str.58,@object               # @.str.58
.L.str.58:
	.asciz	"NO"
	.size	.L.str.58, 3

	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"YES"
	.size	.L.str.59, 4

	.type	.L.str.60,@object               # @.str.60
.L.str.60:
	.asciz	"FORMATTED"
	.size	.L.str.60, 10

	.type	.L.str.61,@object               # @.str.61
.L.str.61:
	.asciz	"NULL"
	.size	.L.str.61, 5

	.type	.L.str.62,@object               # @.str.62
.L.str.62:
	.asciz	"f77: REWIND: unit %d cannot be rewound\n"
	.size	.L.str.62, 40

	.type	fio_uend_init,@object           # @fio_uend_init
	.local	fio_uend_init
	.comm	fio_uend_init,1,4
	.type	.L.str.63,@object               # @.str.63
.L.str.63:
	.asciz	"f77: unit %d is not open for %s\n"
	.size	.L.str.63, 33

	.type	.L.str.64,@object               # @.str.64
.L.str.64:
	.asciz	"WRITE"
	.size	.L.str.64, 6

	.type	.L.str.65,@object               # @.str.65
.L.str.65:
	.asciz	"READ"
	.size	.L.str.65, 5

	.type	.L.str.66,@object               # @.str.66
.L.str.66:
	.asciz	"f77: a direct-access unit needs REC=\n"
	.size	.L.str.66, 38

	.type	fio_sp,@object                  # @fio_sp
	.local	fio_sp
	.comm	fio_sp,1,4
	.type	fio_gdepth,@object              # @fio_gdepth
	.local	fio_gdepth
	.comm	fio_gdepth,4,4
	.type	fio_revert,@object              # @fio_revert
	.local	fio_revert
	.comm	fio_revert,4,4
	.type	fio_since_data,@object          # @fio_since_data
	.local	fio_since_data
	.comm	fio_since_data,1,4
	.type	fio_fbuf,@object                # @fio_fbuf
	.local	fio_fbuf
	.comm	fio_fbuf,1024,1
	.type	fio_ftext,@object               # @fio_ftext
	.local	fio_ftext
	.comm	fio_ftext,4,4
	.type	.L.str.67,@object               # @.str.67
.L.str.67:
	.asciz	"a format begins with ("
	.size	.L.str.67, 23

	.type	fio_fmt_why,@object             # @fio_fmt_why
	.local	fio_fmt_why
	.comm	fio_fmt_why,4,4
	.type	.L.str.68,@object               # @.str.68
.L.str.68:
	.asciz	"the format is too long, or ends without its )"
	.size	.L.str.68, 46

	.type	.L.str.69,@object               # @.str.69
.L.str.69:
	.asciz	"the format ends without its )"
	.size	.L.str.69, 30

	.type	.L.str.70,@object               # @.str.70
.L.str.70:
	.asciz	"a quoted string runs past the end of the format"
	.size	.L.str.70, 48

	.type	.L.str.71,@object               # @.str.71
.L.str.71:
	.asciz	"the format is too long"
	.size	.L.str.71, 23

	.type	.L.str.72,@object               # @.str.72
.L.str.72:
	.asciz	"0H is no Hollerith string"
	.size	.L.str.72, 26

	.type	.L.str.73,@object               # @.str.73
.L.str.73:
	.asciz	"nH wants more characters than the format has"
	.size	.L.str.73, 45

	.type	fio_fmt_check.why,@object       # @fio_fmt_check.why
	.local	fio_fmt_check.why
	.comm	fio_fmt_check.why,64,1
	.type	.L.str.74,@object               # @.str.74
.L.str.74:
	.asciz	"a sign belongs to a scale factor, nP"
	.size	.L.str.74, 37

	.type	.L.str.75,@object               # @.str.75
.L.str.75:
	.asciz	"H needs its count, nH"
	.size	.L.str.75, 22

	.type	.L.str.76,@object               # @.str.76
.L.str.76:
	.asciz	"T, TL and TR take a column"
	.size	.L.str.76, 27

	.type	.L.str.77,@object               # @.str.77
.L.str.77:
	.asciz	"B is BN or BZ"
	.size	.L.str.77, 14

	.type	.L.str.78,@object               # @.str.78
.L.str.78:
	.asciz	"%c needs a width"
	.size	.L.str.78, 17

	.type	.L.str.79,@object               # @.str.79
.L.str.79:
	.asciz	"a . needs digits after it"
	.size	.L.str.79, 26

	.type	.L.str.80,@object               # @.str.80
.L.str.80:
	.asciz	"Ee needs its digits"
	.size	.L.str.80, 20

	.type	.L.str.81,@object               # @.str.81
.L.str.81:
	.asciz	"a count with no descriptor after it"
	.size	.L.str.81, 36

	.type	.L.str.82,@object               # @.str.82
.L.str.82:
	.asciz	"a count before something that is no edit descriptor"
	.size	.L.str.82, 52

	.type	.L.str.83,@object               # @.str.83
.L.str.83:
	.asciz	"%c is no edit descriptor"
	.size	.L.str.83, 25

	.type	.L.str.84,@object               # @.str.84
.L.str.84:
	.asciz	"f77: FORMAT: %s\n  in: %.*s\n"
	.size	.L.str.84, 28

	.type	.L.str.85,@object               # @.str.85
.L.str.85:
	.asciz	"%%.%dE"
	.size	.L.str.85, 7

	.type	fio_gcount,@object              # @fio_gcount
	.local	fio_gcount
	.comm	fio_gcount,64,4
	.type	fio_gstart,@object              # @fio_gstart
	.local	fio_gstart
	.comm	fio_gstart,64,4
	.type	.L.str.86,@object               # @.str.86
.L.str.86:
	.asciz	"f77: FORMAT: no data edit descriptor for the items left\n"
	.size	.L.str.86, 57

	.type	.L.str.87,@object               # @.str.87
.L.str.87:
	.asciz	"f77: WRITE past the end of an internal file\n"
	.size	.L.str.87, 45

	.type	.L.str.88,@object               # @.str.88
.L.str.88:
	.asciz	"f77: record longer than the internal file (%d > %d)\n"
	.size	.L.str.88, 53

	.type	.L.str.89,@object               # @.str.89
.L.str.89:
	.asciz	"f77: record longer than RECL (%d > %d)\n"
	.size	.L.str.89, 40

	.type	fio_l_nulls,@object             # @fio_l_nulls
	.local	fio_l_nulls
	.comm	fio_l_nulls,4,4
	.type	fio_l_pending,@object           # @fio_l_pending
	.local	fio_l_pending
	.comm	fio_l_pending,4,4
	.type	.L.str.90,@object               # @.str.90
.L.str.90:
	.asciz	"f77: zero repeat count in list-directed input\n"
	.size	.L.str.90, 47

	.type	.L.str.91,@object               # @.str.91
.L.str.91:
	.asciz	"f77: bad LOGICAL input field '%s'\n"
	.size	.L.str.91, 35

	.type	.L.str.92,@object               # @.str.92
.L.str.92:
	.asciz	"E%d"
	.size	.L.str.92, 4

	.type	.L.str.93,@object               # @.str.93
.L.str.93:
	.asciz	"(A)"
	.size	.L.str.93, 4

	.type	nml_tlen,@object                # @nml_tlen
	.local	nml_tlen
	.comm	nml_tlen,4,4
	.type	.L.str.94,@object               # @.str.94
.L.str.94:
	.asciz	"the record could not be read"
	.size	.L.str.94, 29

	.type	.L.str.95,@object               # @.str.95
.L.str.95:
	.asciz	" "
	.size	.L.str.95, 2

	.type	nml_tcap,@object                # @nml_tcap
	.local	nml_tcap
	.comm	nml_tcap,4,4
	.type	nml_text,@object                # @nml_text
	.local	nml_text
	.comm	nml_text,4,4
	.type	.L.str.96,@object               # @.str.96
.L.str.96:
	.asciz	"f77: out of storage for a NAMELIST\n"
	.size	.L.str.96, 36

	.type	nml_cval,@object                # @nml_cval
	.local	nml_cval
	.comm	nml_cval,4,4
	.type	nml_casg,@object                # @nml_casg
	.local	nml_casg
	.comm	nml_casg,4,4
	.type	.L.str.97,@object               # @.str.97
.L.str.97:
	.asciz	"a subscript is no integer"
	.size	.L.str.97, 26

	.type	.L.str.98,@object               # @.str.98
.L.str.98:
	.asciz	"a value before the first NAME="
	.size	.L.str.98, 31

	.type	.L.str.99,@object               # @.str.99
.L.str.99:
	.asciz	"a repeat count is not positive"
	.size	.L.str.99, 31

	.type	nml_pend,@object                # @nml_pend
	.local	nml_pend
	.comm	nml_pend,1024,1
	.type	.L.str.100,@object              # @.str.100
.L.str.100:
	.asciz	"0.0"
	.size	.L.str.100, 4

	.type	.L.str.101,@object              # @.str.101
.L.str.101:
	.asciz	"(G23.16)"
	.size	.L.str.101, 9

	.type	.L.str.102,@object              # @.str.102
.L.str.102:
	.asciz	"(G15.8)"
	.size	.L.str.102, 8

	.type	.L.str.103,@object              # @.str.103
.L.str.103:
	.asciz	"a LOGICAL value is not T or F"
	.size	.L.str.103, 30

	.type	.L.str.104,@object              # @.str.104
.L.str.104:
	.asciz	"a CHARACTER value is not in quotes"
	.size	.L.str.104, 35

	.type	.L.str.105,@object              # @.str.105
.L.str.105:
	.asciz	"a value could not be read"
	.size	.L.str.105, 26

	.type	.Lswitch.table.fio_fmt_use,@object # @switch.table.fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.Lswitch.table.fio_fmt_use:
	.word	.L.str.81
	.word	.L.str.82
	.word	.L.str.82
	.word	.L.str.81
	.word	.L.str.82
	.word	.L.str.82
	.word	.L.str.81
	.word	.L.str.82
	.word	.L.str.82
	.word	.L.str.82
	.word	.L.str.82
	.word	.L.str.82
	.word	.L.str.82
	.word	.L.str.82
	.word	.L.str.82
	.word	.L.str.82
	.word	.L.str.82
	.word	.L.str.81
	.size	.Lswitch.table.fio_fmt_use, 72

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git 53ed108f6aa13a0be178e54f68d36210ead648de)"
	.section	".note.GNU-stack","",@progbits
