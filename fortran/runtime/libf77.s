	.file	"libf77.c"
	.text
	.globl	f77_set_rec                     # -- Begin function f77_set_rec
	.p2align	2
	.type	f77_set_rec,@function
f77_set_rec:                            # @f77_set_rec
# %bb.0:
	lui r1, %hi(fio_rec_next)
	stw r1+%lo(fio_rec_next), r3
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
	add r11, r5, r0
	add r12, r4, r0
	add r14, r3, r0
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB1_4
.LBB1_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB1_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB1_2
.LBB1_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB1_4:
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	addi r13, r0, 1
	add r3, r14, r0
	add r4, r13, r0
	jal r31, fio_resolve
	lui r3, %hi(fio_out)
	stw r3+%lo(fio_out), r1
	lui r3, %hi(fio_unit)
	stw r3+%lo(fio_unit), r14
	addi r4, r14, -1
	addi r5, r0, 98
	slli r3, r14, 2
	bgtu r4, r5, .LBB1_6
.LBB1_5:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r4, r3, r4
	stw r4+0, r0
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r4, r3, r4
	stw r4+0, r0
.LBB1_6:
	lui r5, %hi(fio_rec_next)
	ldw r4, r5+%lo(fio_rec_next)
	stw r5+%lo(fio_rec_next), r0
	addi r5, r14, -100
	addi r6, r0, -99
	bltu r5, r6, .LBB1_10
.LBB1_7:
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r3, r5
	ldw r5, r5+0
	beq r5, r0, .LBB1_10
.LBB1_8:
	ble r4, r0, .LBB1_20
.LBB1_9:
	addi r4, r4, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB1_10:
	lui r1, %hi(fio_reading)
	stb r1+%lo(fio_reading), r0
	lui r1, %hi(fio_len)
	stw r1+%lo(fio_len), r0
	lui r1, %hi(fio_col)
	stw r1+%lo(fio_col), r0
	lui r1, %hi(fio_sp)
	stb r1+%lo(fio_sp), r0
	lui r1, %hi(fio_gdepth)
	stw r1+%lo(fio_gdepth), r0
	lui r1, %hi(fio_scale)
	stw r1+%lo(fio_scale), r0
	lui r1, %hi(fio_rep)
	stw r1+%lo(fio_rep), r0
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r13
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	lui r14, %hi(fio_pos)
	lui r15, %hi(fio_revert)
	lui r1, %hi(fio_listed)
	beq r12, r0, .LBB1_17
.LBB1_11:
	stw r1+%lo(fio_listed), r0
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fio_fmt_use
	addi r3, r0, -1
	ble r1, r3, .LBB1_21
.LBB1_12:
	stw r14+%lo(fio_pos), r0
	stw r15+%lo(fio_revert), r3
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	lui r1, %hi(fio_fmt)
	ldw r3, r1+%lo(fio_fmt)
	addi r4, r0, 32
	add r1, r0, r0
.LBB1_13:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB1_15
.LBB1_14:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB1_13
.LBB1_15:
	addi r3, r0, 40
	bne r5, r3, .LBB1_19
.LBB1_16:
	addi r3, r1, 1
	add r1, r3, r0
	jal r0, .LBB1_18
.LBB1_17:
	stw r1+%lo(fio_listed), r13
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
	lui r3, %hi(fio_fmt)
	stw r3+%lo(fio_fmt), r1
	addi r1, r0, -1
	add r3, r0, r0
.LBB1_18:
	stw r14+%lo(fio_pos), r3
	stw r15+%lo(fio_revert), r1
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
	ldw r3, r1+%lo(stderr)
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
	add r11, r3, r0
	addi r1, r0, 99
	bgtu r3, r1, .LBB2_2
.LBB2_1:
	slli r1, r11, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r1, r1, r3
	ldw r1, r1+0
	bne r1, r0, .LBB2_16
.LBB2_2:
	beq r4, r0, .LBB2_7
.LBB2_3:
	beq r11, r0, .LBB2_15
.LBB2_4:
	addi r1, r0, 5
	beq r11, r1, .LBB2_6
.LBB2_5:
	addi r1, r0, 6
	bne r11, r1, .LBB2_10
.LBB2_6:
	lui r1, %hi(stdout)
	ldw r1, r1+%lo(stdout)
	jal r0, .LBB2_16
.LBB2_7:
	addi r1, r0, 5
	beq r11, r1, .LBB2_9
.LBB2_8:
	bne r11, r0, .LBB2_10
.LBB2_9:
	lui r1, %hi(stdin)
	ldw r1, r1+%lo(stdin)
	jal r0, .LBB2_16
.LBB2_10:
	add r13, r4, r0
	addi r1, r11, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB2_17
.LBB2_11:
	lui r5, %hi(.L.str.36)
	addi r5, r5, %lo(.L.str.36)
	addi r12, sp, 20
	addi r4, r0, 256
	add r3, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	lui r4, %hi(.L.str.40)
	addi r4, r4, %lo(.L.str.40)
	add r3, r12, r0
	jal r31, fopen
	bne r1, r0, .LBB2_13
.LBB2_12:
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	addi r3, sp, 20
	jal r31, fopen
.LBB2_13:
	beq r1, r0, .LBB2_17
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
	addi r4, sp, 20
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
	stw r3+0, r0
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r12, r3
	stw r3+0, r0
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r12, r3
	stw r3+0, r0
	lui r3, %hi(fio_ubz)
	addi r3, r3, %lo(fio_ubz)
	add r3, r12, r3
	stw r3+0, r0
	jal r0, .LBB2_16
.LBB2_15:
	lui r1, %hi(stderr)
	ldw r1, r1+%lo(stderr)
.LBB2_16:
	ldw r13, sp+276
	ldw r12, sp+280
	ldw r11, sp+284
	ldw lr, sp+0
	addi sp, sp, 288
	jalr r0, r31, 0
.LBB2_17:
	addi r1, r0, 0
	seq r1, r13, r1
	lui r3, %hi(stderr)
	ldw r3, r3+%lo(stderr)
	lui r4, %hi(.L.str.64)
	addi r4, r4, %lo(.L.str.64)
	lui r5, %hi(.L.str.65)
	addi r5, r5, %lo(.L.str.65)
	xor r5, r5, r4
	sub r1, r0, r1
	and r1, r5, r1
	xor r6, r4, r1
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
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB3_4
.LBB3_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB3_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB3_2
.LBB3_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB3_4:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_resolve
	lui r3, %hi(fio_out)
	stw r3+%lo(fio_out), r1
	lui r3, %hi(fio_unit)
	stw r3+%lo(fio_unit), r11
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
	stw r4+0, r0
.LBB3_6:
	lui r5, %hi(fio_rec_next)
	ldw r4, r5+%lo(fio_rec_next)
	stw r5+%lo(fio_rec_next), r0
	addi r5, r11, -100
	addi r6, r0, -99
	bltu r5, r6, .LBB3_10
.LBB3_7:
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r3, r5
	ldw r5, r5+0
	beq r5, r0, .LBB3_10
.LBB3_8:
	ble r4, r0, .LBB3_11
.LBB3_9:
	addi r4, r4, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB3_10:
	lui r1, %hi(fio_ulen)
	stw r1+%lo(fio_ulen), r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB3_11:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
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
	ldw r1, r12+%lo(fio_ulen)
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
	ldw r1, r12+%lo(fio_ulen)
	add r1, r1, r11
	stw r12+%lo(fio_ulen), r1
.LBB4_3:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB4_4:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
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
	ldw r1, r1+%lo(fio_out)
	addi r3, r0, 0
	seq r3, r1, r3
	lui r4, %hi(stdout)
	ldw r4, r4+%lo(stdout)
	xor r4, r4, r1
	sub r3, r0, r3
	and r3, r4, r3
	xor r11, r1, r3
	lui r16, %hi(fio_unit)
	ldw r1, r16+%lo(fio_unit)
	addi r3, r1, -1
	addi r17, r0, 98
	bgtu r3, r17, .LBB5_5
.LBB5_1:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB5_5
.LBB5_2:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r12, r1+0
	lui r13, %hi(fio_ulen)
	ldw r5, r13+%lo(fio_ulen)
	bgt r5, r12, .LBB5_8
.LBB5_3:
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	ldw r1, r13+%lo(fio_ulen)
	bge r1, r12, .LBB5_7
.LBB5_4:
	add r3, r0, r0
	add r4, r11, r0
	jal r31, fputc
	ldw r1, r13+%lo(fio_ulen)
	addi r1, r1, 1
	stw r13+%lo(fio_ulen), r1
	blt r1, r12, .LBB5_4
	jal r0, .LBB5_7
.LBB5_5:
	lui r1, %hi(fio_ulen)
	ldw r12, r1+%lo(fio_ulen)
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
	ldw r1, r16+%lo(fio_unit)
	addi r1, r1, -1
	bgtu r1, r17, .LBB5_7
.LBB5_6:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r16+%lo(fio_unit)
	slli r3, r3, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
.LBB5_7:
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
.LBB5_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
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
	add r11, r4, r0
	add r13, r3, r0
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB6_4
.LBB6_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB6_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB6_2
.LBB6_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB6_4:
	add r3, r13, r0
	add r4, r0, r0
	jal r31, fio_resolve
	add r12, r1, r0
	lui r1, %hi(fio_in)
	stw r1+%lo(fio_in), r12
	lui r1, %hi(fio_unit)
	stw r1+%lo(fio_unit), r13
	addi r1, r13, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB6_13
.LBB6_5:
	slli r14, r13, 2
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r14, r1
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(fio_rec_next)
	ldw r1, r3+%lo(fio_rec_next)
	stw r3+%lo(fio_rec_next), r0
	addi r3, r13, -100
	addi r4, r0, -99
	bltu r3, r4, .LBB6_9
.LBB6_6:
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r14, r3
	ldw r3, r3+0
	beq r3, r0, .LBB6_9
.LBB6_7:
	ble r1, r0, .LBB6_30
.LBB6_8:
	addi r1, r1, -1
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r14, r3
	ldw r3, r3+0
	mul r4, r3, r1
	add r3, r12, r0
	add r5, r0, r0
	jal r31, fseek
.LBB6_9:
	lui r1, %hi(fio_upos)
	stw r1+%lo(fio_upos), r0
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r14, r1
	ldw r1, r1+0
	beq r1, r0, .LBB6_18
.LBB6_10:
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r14, r1
	ldw r5, r1+0
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	addi r4, r0, 1
	add r6, r12, r0
	jal r31, fread
	add r3, r1, r0
	lui r1, %hi(fio_ulen)
	stw r1+%lo(fio_ulen), r3
	add r1, r0, r0
	bne r3, r0, .LBB6_29
.LBB6_11:
	andi r1, r11, 2
	bne r1, r0, .LBB6_28
.LBB6_12:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r0, .LBB6_32
.LBB6_13:
	lui r3, %hi(fio_rec_next)
	ldw r1, r3+%lo(fio_rec_next)
	stw r3+%lo(fio_rec_next), r0
	addi r3, r13, -100
	addi r4, r0, -99
	bltu r3, r4, .LBB6_17
.LBB6_14:
	slli r3, r13, 2
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r3, r4
	ldw r4, r4+0
	beq r4, r0, .LBB6_17
.LBB6_15:
	ble r1, r0, .LBB6_30
.LBB6_16:
	addi r1, r1, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r3, r3, r4
	ldw r3, r3+0
	mul r4, r3, r1
	add r3, r12, r0
	add r5, r0, r0
	jal r31, fseek
.LBB6_17:
	lui r1, %hi(fio_upos)
	stw r1+%lo(fio_upos), r0
.LBB6_18:
	slli r14, r13, 2
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r13, r14, r1
	ldw r1, r13+0
	blt r1, r0, .LBB6_20
.LBB6_19:
	add r3, r12, r0
	jal r31, ftell
	ldw r3, r13+0
	bge r1, r3, .LBB6_25
.LBB6_20:
	addi r3, sp, 24
	addi r4, r0, 1
	addi r13, r0, 4
	add r15, r4, r0
	add r5, r13, r0
	add r6, r12, r0
	jal r31, fread
	bne r1, r13, .LBB6_24
.LBB6_21:
	addi r3, sp, 24
	addi r1, r3, 3
	ldbu r1, r1+0
	slli r1, r1, 24
	blt r1, r0, .LBB6_27
.LBB6_22:
	ldhu r4, sp+24
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
	add r6, r12, r0
	jal r31, fread
	lui r3, %hi(fio_ulen)
	stw r3+%lo(fio_ulen), r1
	addi r3, sp, 24
	addi r5, r0, 4
	add r4, r11, r0
	add r6, r12, r0
	jal r31, fread
	add r1, r0, r0
	jal r0, .LBB6_29
.LBB6_24:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r3, r14, r1
	add r1, r15, r0
	stw r3+0, r15
	andi r3, r11, 3
	bne r3, r0, .LBB6_29
	jal r0, .LBB6_26
.LBB6_25:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r3, r14, r1
	addi r1, r0, 1
	stw r3+0, r1
	andi r3, r11, 3
	bne r3, r0, .LBB6_29
.LBB6_26:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r0, .LBB6_32
.LBB6_27:
	andi r1, r11, 2
	beq r1, r0, .LBB6_31
.LBB6_28:
	addi r1, r0, 2
.LBB6_29:
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
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	jal r0, .LBB6_32
.LBB6_31:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
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
	ldw r1, r12+%lo(fio_upos)
	add r4, r1, r4
	lui r5, %hi(fio_ulen)
	ldw r5, r5+%lo(fio_ulen)
	bgt r4, r5, .LBB7_4
.LBB7_2:
	lui r4, %hi(fio_urec)
	addi r4, r4, %lo(fio_urec)
	add r4, r1, r4
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r12+%lo(fio_upos)
	add r1, r1, r11
	stw r12+%lo(fio_upos), r1
.LBB7_3:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB7_4:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
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
	lui r1, %hi(fio_uend_init)
	ldbu r4, r1+%lo(fio_uend_init)
	bne r4, r0, .LBB9_4
.LBB9_1:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
	addi r6, r0, -1
.LBB9_2:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r0, .LBB9_2
.LBB9_3:
	addi r4, r0, 1
	stb r1+%lo(fio_uend_init), r4
.LBB9_4:
	addi r1, r3, -100
	addi r4, r0, -99
	bltu r1, r4, .LBB9_8
.LBB9_5:
	addi r1, r3, -5
	addi r4, r0, 2
	bltu r1, r4, .LBB9_8
.LBB9_6:
	addi r12, r0, 1
	add r13, r3, r0
	add r4, r12, r0
	jal r31, fio_resolve
	add r11, r1, r0
	add r3, r1, r0
	jal r31, fflush
	add r3, r11, r0
	jal r31, ftell
	slli r3, r13, 2
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r4, r3, r4
	ldw r5, r4+0
	beq r5, r0, .LBB9_9
.LBB9_7:
	stw r4+0, r0
.LBB9_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB9_9:
	blt r1, r12, .LBB9_8
.LBB9_10:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r0, .LBB9_14
.LBB9_11:
	add r13, r1, r0
	addi r4, r1, -4
	add r3, r11, r0
	add r5, r0, r0
	jal r31, fseek
	addi r3, sp, 24
	addi r4, r0, 1
	addi r12, r0, 4
	add r5, r12, r0
	add r6, r11, r0
	jal r31, fread
	add r4, r0, r0
	bne r1, r12, .LBB9_13
.LBB9_12:
	ldw r1, sp+24
	sub r1, r13, r1
	addi r4, r1, -8
.LBB9_13:
	add r3, r11, r0
	add r5, r0, r0
	jal r31, fseek
	jal r0, .LBB9_8
.LBB9_14:
	addi r13, r1, -1
	addi r15, r0, 10
.LBB9_15:
	blt r13, r12, .LBB9_17
.LBB9_16:
	addi r14, r13, -1
	add r3, r11, r0
	add r4, r14, r0
	add r5, r0, r0
	jal r31, fseek
	add r3, r11, r0
	jal r31, fgetc
	seq r3, r1, r15
	xor r4, r13, r14
	sub r3, r0, r3
	and r3, r4, r3
	xor r13, r14, r3
	bne r1, r15, .LBB9_15
	jal r0, .LBB9_18
.LBB9_17:
	add r13, r0, r0
.LBB9_18:
	add r3, r11, r0
	add r4, r13, r0
	add r5, r0, r0
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
	lui r1, %hi(fio_uend_init)
	ldbu r8, r1+%lo(fio_uend_init)
	bne r8, r0, .LBB10_4
.LBB10_1:
	addi r8, r0, -400
	lui r9, %hi(fio_uend+400)
	addi r9, r9, %lo(fio_uend+400)
	addi r10, r0, -1
.LBB10_2:
	add lr, r8, r9
	stw lr+0, r10
	addi r8, r8, 4
	bne r8, r0, .LBB10_2
.LBB10_3:
	addi r8, r0, 1
	stb r1+%lo(fio_uend_init), r8
.LBB10_4:
	lui r1, %hi(stdout)
	ldw r1, r1+%lo(stdout)
	lui r8, %hi(fio_out)
	stw r8+%lo(fio_out), r1
	lui r1, %hi(fio_unit)
	addi r11, r0, -1
	stw r1+%lo(fio_unit), r11
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r3
	lui r1, %hi(fio_ilen)
	stw r1+%lo(fio_ilen), r4
	lui r1, %hi(fio_inrec)
	stw r1+%lo(fio_inrec), r5
	lui r1, %hi(fio_irec)
	stw r1+%lo(fio_irec), r0
	lui r1, %hi(fio_reading)
	stb r1+%lo(fio_reading), r0
	lui r1, %hi(fio_len)
	stw r1+%lo(fio_len), r0
	lui r1, %hi(fio_col)
	stw r1+%lo(fio_col), r0
	lui r1, %hi(fio_sp)
	stb r1+%lo(fio_sp), r0
	lui r1, %hi(fio_gdepth)
	stw r1+%lo(fio_gdepth), r0
	lui r1, %hi(fio_scale)
	stw r1+%lo(fio_scale), r0
	lui r1, %hi(fio_rep)
	stw r1+%lo(fio_rep), r0
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
	lui r3, %hi(fio_first_item)
	addi r1, r0, 1
	stb r3+%lo(fio_first_item), r1
	lui r3, %hi(fio_prev_char)
	stb r3+%lo(fio_prev_char), r0
	lui r12, %hi(fio_pos)
	lui r13, %hi(fio_revert)
	beq r6, r0, .LBB10_11
.LBB10_5:
	lui r1, %hi(fio_listed)
	stw r1+%lo(fio_listed), r0
	add r3, r6, r0
	add r4, r7, r0
	jal r31, fio_fmt_use
	ble r1, r11, .LBB10_14
.LBB10_6:
	stw r12+%lo(fio_pos), r0
	stw r13+%lo(fio_revert), r11
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	lui r1, %hi(fio_fmt)
	ldw r3, r1+%lo(fio_fmt)
	addi r4, r0, 32
	add r1, r0, r0
.LBB10_7:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB10_9
.LBB10_8:
	addi r1, r1, 1
	stw r12+%lo(fio_pos), r1
	jal r0, .LBB10_7
.LBB10_9:
	addi r3, r0, 40
	bne r5, r3, .LBB10_13
.LBB10_10:
	addi r1, r1, 1
	add r11, r1, r0
	jal r0, .LBB10_12
.LBB10_11:
	lui r3, %hi(fio_listed)
	stw r3+%lo(fio_listed), r1
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
	lui r3, %hi(fio_fmt)
	stw r3+%lo(fio_fmt), r1
	add r1, r0, r0
.LBB10_12:
	stw r12+%lo(fio_pos), r1
	stw r13+%lo(fio_revert), r11
.LBB10_13:
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
	add r11, r3, r0
	stw sp+92, r3
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB11_12
.LBB11_1:
	lui r5, %hi(.L.str.6)
	addi r5, r5, %lo(.L.str.6)
	addi r3, sp, 60
	addi r12, r0, 32
	add r4, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	ldbu r9, sp+60
	beq r9, r0, .LBB11_11
.LBB11_2:
	lui r1, %hi(fio_len)
	ldw r7, r1+%lo(fio_len)
	lui r3, %hi(fio_col)
	ldw r8, r3+%lo(fio_col)
	addi r4, sp, 60
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	jal r0, .LBB11_5
.LBB11_3:
	add r8, r9, r0
.LBB11_4:
	addi r4, r4, 1
	ldbu r9, r4+0
	beq r9, r0, .LBB11_11
.LBB11_5:
	bgt r8, r5, .LBB11_4
.LBB11_6:
	bge r7, r8, .LBB11_9
.LBB11_7:
	addi r10, r7, 1
	add r7, r7, r6
	stb r7+0, r12
	add r7, r10, r0
	bne r8, r10, .LBB11_7
.LBB11_8:
	stw r1+%lo(fio_len), r8
	add r7, r8, r0
.LBB11_9:
	add r10, r8, r6
	stb r10+0, r9
	addi r9, r8, 1
	stw r3+%lo(fio_col), r9
	blt r8, r7, .LBB11_3
.LBB11_10:
	stw r1+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB11_3
.LBB11_11:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	jal r0, .LBB11_33
.LBB11_12:
	lui r15, %hi(fio_desc)
	ldw r1, r15+%lo(fio_desc)
	lui r16, %hi(fio_rep)
	beq r1, r0, .LBB11_14
.LBB11_13:
	ldw r3, r16+%lo(fio_rep)
	bgt r3, r0, .LBB11_15
.LBB11_14:
	stw r15+%lo(fio_desc), r0
	jal r31, fio_next_desc
.LBB11_15:
	beq r1, r0, .LBB11_33
.LBB11_16:
	addi r3, r0, 76
	beq r1, r3, .LBB11_20
.LBB11_17:
	addi r3, r0, 65
	bne r1, r3, .LBB11_34
.LBB11_18:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	addi r1, r3, -1
	addi r4, r0, 3
	bgeu r1, r4, .LBB11_37
.LBB11_19:
	add r1, r3, r0
	jal r0, .LBB11_39
.LBB11_20:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r3, r0, 2
	blt r1, r3, .LBB11_22
.LBB11_21:
	lui r3, %hi(fio_col)
	ldw r4, r3+%lo(fio_col)
	add r1, r1, r4
	addi r1, r1, -1
	stw r3+%lo(fio_col), r1
.LBB11_22:
	lui r3, %hi(fio_col)
	ldw r1, r3+%lo(fio_col)
	addi r4, r0, 1022
	bgt r1, r4, .LBB11_29
.LBB11_23:
	lui r4, %hi(fio_len)
	ldw r5, r4+%lo(fio_len)
	bge r5, r1, .LBB11_27
.LBB11_24:
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
.LBB11_25:
	addi r8, r5, 1
	add r5, r5, r6
	stb r5+0, r7
	add r5, r8, r0
	bne r1, r8, .LBB11_25
.LBB11_26:
	stw r4+%lo(fio_len), r1
	add r5, r1, r0
.LBB11_27:
	addi r6, r0, 0
	seq r6, r11, r6
	sub r6, r0, r6
	andi r6, r6, 18
	xori r6, r6, 84
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	add r7, r1, r7
	stb r7+0, r6
	addi r6, r1, 1
	stw r3+%lo(fio_col), r6
	blt r1, r5, .LBB11_29
.LBB11_28:
	stw r4+%lo(fio_len), r6
.LBB11_29:
	ldw r1, r16+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB11_31
.LBB11_30:
	addi r1, r1, -1
	stw r16+%lo(fio_rep), r1
.LBB11_31:
	ldw r1, r16+%lo(fio_rep)
	bne r1, r0, .LBB11_33
.LBB11_32:
	stw r15+%lo(fio_desc), r0
.LBB11_33:
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
.LBB11_34:
	lui r18, %hi(fio_d)
	ldw r1, r18+%lo(fio_d)
	addi r17, r0, 1
	blt r1, r17, .LBB11_48
.LBB11_35:
	srai r1, r11, 31
	xor r3, r11, r1
	sub r6, r3, r1
	lui r5, %hi(.L.str.7)
	addi r5, r5, %lo(.L.str.7)
	addi r12, sp, 28
	addi r13, r0, 32
	add r3, r12, r0
	add r4, r13, r0
	jal r31, snprintf
	add r3, r12, r0
	jal r31, strlen
	add r14, r1, r0
	addi r1, r0, 0
	slt r1, r11, r1
	lui r3, %hi(.L.str.10)
	addi r3, r3, %lo(.L.str.10)
	lui r4, %hi(.L.str.9)
	addi r4, r4, %lo(.L.str.9)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r11, r3, r1
	ldw r1, r18+%lo(fio_d)
	sgt r3, r1, r14
	sub r1, r1, r14
	sub r3, r0, r3
	and r7, r1, r3
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
	addi r3, sp, 60
	add r4, r13, r0
	add r6, r11, r0
	add r8, r0, r0
	add r9, r12, r0
	jal r31, snprintf
	ldw r1, r18+%lo(fio_d)
	blt r14, r1, .LBB11_54
.LBB11_36:
	lui r5, %hi(.L.str.11)
	addi r5, r5, %lo(.L.str.11)
	addi r3, sp, 60
	addi r4, r0, 32
	addi r7, sp, 28
	jal r0, .LBB11_53
.LBB11_37:
	addi r1, r0, 4
	addi r4, r0, 5
	blt r3, r4, .LBB11_39
.LBB11_38:
	lui r4, %hi(fio_col)
	ldw r5, r4+%lo(fio_col)
	add r3, r3, r5
	addi r3, r3, -4
	stw r4+%lo(fio_col), r3
.LBB11_39:
	lui r3, %hi(fio_len)
	ldw r7, r3+%lo(fio_len)
	lui r4, %hi(fio_col)
	ldw r10, r4+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r8, r0, 32
	add r9, r0, r0
	jal r0, .LBB11_42
.LBB11_40:
	add r10, lr, r0
.LBB11_41:
	addi r9, r9, 1
	beq r9, r1, .LBB11_29
.LBB11_42:
	addi lr, sp, 92
	bgt r10, r5, .LBB11_41
.LBB11_43:
	bge r7, r10, .LBB11_46
.LBB11_44:
	addi r11, r7, 1
	add r7, r7, r6
	stb r7+0, r8
	add r7, r11, r0
	bne r10, r11, .LBB11_44
.LBB11_45:
	stw r3+%lo(fio_len), r10
	add r7, r10, r0
.LBB11_46:
	add lr, lr, r9
	ldbu lr, lr+0
	add r11, r10, r6
	stb r11+0, lr
	addi lr, r10, 1
	stw r4+%lo(fio_col), lr
	blt r10, r7, .LBB11_40
.LBB11_47:
	stw r3+%lo(fio_len), lr
	add r7, lr, r0
	jal r0, .LBB11_40
.LBB11_48:
	lui r1, %hi(fio_fmt)
	ldw r1, r1+%lo(fio_fmt)
	lui r3, %hi(fio_pos)
	ldw r3, r3+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+-1
	addi r4, r0, 48
	bne r3, r4, .LBB11_52
.LBB11_49:
	bne r11, r0, .LBB11_52
.LBB11_50:
	ldbu r1, r1+-2
	andi r1, r1, 255
	addi r3, r0, 46
	bne r1, r3, .LBB11_52
.LBB11_51:
	stb sp+60, r0
	jal r0, .LBB11_54
.LBB11_52:
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r3, sp, 60
	addi r4, r0, 32
.LBB11_53:
	add r6, r11, r0
	jal r31, snprintf
.LBB11_54:
	lui r1, %hi(fio_sp)
	ldbu r1, r1+%lo(fio_sp)
	bne r1, r17, .LBB11_59
.LBB11_55:
	ldbu r1, sp+60
	beq r1, r0, .LBB11_59
.LBB11_56:
	addi r13, r0, 43
	beq r1, r13, .LBB11_59
.LBB11_57:
	addi r3, r0, 45
	beq r1, r3, .LBB11_59
.LBB11_58:
	addi r11, sp, 60
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+60, r13
.LBB11_59:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 60
	jal r31, fio_field
	ldw r1, r16+%lo(fio_rep)
	bge r1, r17, .LBB11_30
	jal r0, .LBB11_31
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
	ble r12, r0, .LBB12_11
.LBB12_1:
	bge r12, r1, .LBB12_21
.LBB12_2:
	lui r1, %hi(fio_len)
	ldw r8, r1+%lo(fio_len)
	lui r3, %hi(fio_col)
	ldw r10, r3+%lo(fio_col)
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
	stw r1+%lo(fio_len), r10
	add r8, r10, r0
.LBB12_9:
	add r11, r10, r6
	stb r11+0, r9
	addi r11, r10, 1
	stw r3+%lo(fio_col), r11
	blt r10, r8, .LBB12_3
.LBB12_10:
	stw r1+%lo(fio_len), r11
	add r8, r11, r0
	jal r0, .LBB12_3
.LBB12_11:
	ldbu r9, r11+0
	beq r9, r0, .LBB12_33
.LBB12_12:
	lui r1, %hi(fio_len)
	ldw r7, r1+%lo(fio_len)
	lui r3, %hi(fio_col)
	ldw r8, r3+%lo(fio_col)
	addi r4, r0, 1022
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	addi r6, r0, 32
	jal r0, .LBB12_15
.LBB12_13:
	add r8, r9, r0
.LBB12_14:
	addi r11, r11, 1
	ldbu r9, r11+0
	beq r9, r0, .LBB12_33
.LBB12_15:
	bgt r8, r4, .LBB12_14
.LBB12_16:
	bge r7, r8, .LBB12_19
.LBB12_17:
	addi r10, r7, 1
	add r7, r7, r5
	stb r7+0, r6
	add r7, r10, r0
	bne r8, r10, .LBB12_17
.LBB12_18:
	stw r1+%lo(fio_len), r8
	add r7, r8, r0
.LBB12_19:
	add r10, r8, r5
	stb r10+0, r9
	addi r9, r8, 1
	stw r3+%lo(fio_col), r9
	blt r8, r7, .LBB12_13
.LBB12_20:
	stw r1+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB12_13
.LBB12_21:
	sub r3, r12, r1
	addi r4, r0, 1
	lui r1, %hi(fio_col)
	blt r3, r4, .LBB12_23
.LBB12_22:
	ldw r4, r1+%lo(fio_col)
	add r3, r4, r3
	stw r1+%lo(fio_col), r3
.LBB12_23:
	ldbu r9, r11+0
	beq r9, r0, .LBB12_33
.LBB12_24:
	lui r3, %hi(fio_len)
	ldw r7, r3+%lo(fio_len)
	ldw r8, r1+%lo(fio_col)
	addi r4, r0, 1022
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	addi r6, r0, 32
	jal r0, .LBB12_27
.LBB12_25:
	add r8, r9, r0
.LBB12_26:
	addi r11, r11, 1
	ldbu r9, r11+0
	beq r9, r0, .LBB12_33
.LBB12_27:
	bgt r8, r4, .LBB12_26
.LBB12_28:
	bge r7, r8, .LBB12_31
.LBB12_29:
	addi r10, r7, 1
	add r7, r7, r5
	stb r7+0, r6
	add r7, r10, r0
	bne r8, r10, .LBB12_29
.LBB12_30:
	stw r3+%lo(fio_len), r8
	add r7, r8, r0
.LBB12_31:
	add r10, r8, r5
	stb r10+0, r9
	addi r9, r8, 1
	stw r1+%lo(fio_col), r9
	blt r8, r7, .LBB12_25
.LBB12_32:
	stw r3+%lo(fio_len), r9
	add r7, r9, r0
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
	add r13, r4, r0
	add r12, r3, r0
	stw sp+112, r13
	stw sp+108, r3
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB13_2
.LBB13_1:
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r13, r0
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
	ldw r1, r14+%lo(fio_desc)
	lui r15, %hi(fio_rep)
	beq r1, r0, .LBB13_4
.LBB13_3:
	ldw r3, r15+%lo(fio_rep)
	bgt r3, r0, .LBB13_5
.LBB13_4:
	stw r14+%lo(fio_desc), r0
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
	ldw r1, r1+%lo(fio_w)
	addi r4, r1, -1
	addi r5, r0, 7
	bgeu r4, r5, .LBB13_62
.LBB13_8:
	add r3, r1, r0
	jal r0, .LBB13_64
.LBB13_9:
	beq r1, r0, .LBB13_121
.LBB13_10:
	addi r3, r0, -2
	and r3, r1, r3
	addi r4, r0, 68
	bne r3, r4, .LBB13_58
.LBB13_11:
	lui r3, %hi(fio_d)
	ldw r4, r3+%lo(fio_d)
	addi r3, sp, 44
	add r5, r12, r0
	add r6, r13, r0
	add r7, r1, r0
	jal r31, fio_efmt
	lui r1, %hi(fio_sp)
	ldbu r1, r1+%lo(fio_sp)
	addi r13, r0, 1
	bne r1, r13, .LBB13_16
.LBB13_12:
	ldbu r1, sp+44
	beq r1, r0, .LBB13_16
.LBB13_13:
	addi r16, r0, 43
	beq r1, r16, .LBB13_16
.LBB13_14:
	addi r3, r0, 45
	beq r1, r3, .LBB13_16
.LBB13_15:
	addi r11, sp, 44
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+44, r16
.LBB13_16:
	lui r1, %hi(fio_w)
	ldw r11, r1+%lo(fio_w)
	addi r3, sp, 44
	jal r31, strlen
	bge r11, r13, .LBB13_52
	jal r0, .LBB13_116
.LBB13_17:
	lui r1, %hi(fio_d)
	ldw r1, r1+%lo(fio_d)
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
	bne r10, r0, .LBB13_18
.LBB13_19:
	lui r10, %hi(.LCPI13_2)
	addi r10, r10, %lo(.LCPI13_2)
	ldw r19, r10+4
	ldw r18, r10+0
	flt.d r10, r12, r18
	fneg.d r20, r12
	xor lr, r21, r13
	sub r17, r0, r10
	and r10, lr, r17
	xor r11, r13, r10
	xor lr, r20, r12
	and lr, lr, r17
	xor r10, r12, lr
	feq.d lr, r12, r18
	bne lr, r0, .LBB13_22
.LBB13_20:
	lui lr, %hi(.LCPI13_3)
	addi lr, lr, %lo(.LCPI13_3)
	ldw r19, lr+4
	ldw r18, lr+0
	fle.d lr, r18, r10
	xori lr, lr, 1
	bne lr, r0, .LBB13_23
.LBB13_21:
	flt.d r4, r10, r4
	xori r4, r4, 1
	bne r4, r0, .LBB13_23
.LBB13_22:
	add r16, r0, r0
.LBB13_23:
	xori r4, r3, 1
	lui r17, %hi(fio_w)
	beq r16, r0, .LBB13_59
.LBB13_24:
	addi r3, sp, 44
	addi r7, r0, 69
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fio_efmt
	lui r1, %hi(fio_sp)
	ldbu r1, r1+%lo(fio_sp)
	addi r13, r0, 1
	bne r1, r13, .LBB13_29
.LBB13_25:
	ldbu r1, sp+44
	beq r1, r0, .LBB13_29
.LBB13_26:
	addi r18, r0, 43
	beq r1, r18, .LBB13_29
.LBB13_27:
	addi r3, r0, 45
	beq r1, r3, .LBB13_29
.LBB13_28:
	addi r11, sp, 44
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+44, r18
.LBB13_29:
	ldw r11, r17+%lo(fio_w)
	addi r3, sp, 44
	jal r31, strlen
	blt r11, r13, .LBB13_115
.LBB13_30:
	addi r3, r11, 1
	bne r1, r3, .LBB13_115
.LBB13_31:
	ldbu r3, sp+44
	addi r4, r0, 43
	beq r3, r4, .LBB13_76
.LBB13_32:
	addi r4, r0, 45
	beq r3, r4, .LBB13_76
.LBB13_33:
	addi r4, r0, 48
	bne r3, r4, .LBB13_115
.LBB13_34:
	addi r3, sp, 44
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB13_115
.LBB13_35:
	addi r3, sp, 44
	add r5, r1, r0
	jal r0, .LBB13_79
.LBB13_36:
	fcvt.w.d r6, r12
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	addi r3, sp, 44
	addi r4, r0, 64
	jal r31, snprintf
	jal r0, .LBB13_116
.LBB13_37:
	lui r1, %hi(fio_scale)
	ldw r1, r1+%lo(fio_scale)
	addi r16, r0, 1
	blt r1, r16, .LBB13_41
.LBB13_38:
	addi r1, r1, 1
	lui r3, %hi(.LCPI13_1)
	addi r3, r3, %lo(.LCPI13_1)
	ldw r5, r3+4
	ldw r4, r3+0
.LBB13_39:
	fmul.d r12, r12, r4
	addi r1, r1, -1
	bgt r1, r16, .LBB13_39
.LBB13_40:
	add r1, r0, r0
.LBB13_41:
	addi r3, r0, -1
	bgt r1, r3, .LBB13_44
.LBB13_42:
	lui r3, %hi(.LCPI13_1)
	addi r3, r3, %lo(.LCPI13_1)
	ldw r5, r3+4
	ldw r4, r3+0
.LBB13_43:
	fdiv.d r12, r12, r4
	addi r3, r1, 1
	sltu r6, r3, r1
	add r1, r3, r0
	bne r6, r16, .LBB13_43
.LBB13_44:
	lui r17, %hi(fio_d)
	ldw r6, r17+%lo(fio_d)
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
	ldw r1, r17+%lo(fio_d)
	bne r1, r0, .LBB13_46
.LBB13_45:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	addi r3, sp, 44
	jal r31, strcat
.LBB13_46:
	lui r1, %hi(fio_sp)
	ldbu r1, r1+%lo(fio_sp)
	bne r1, r16, .LBB13_51
.LBB13_47:
	ldbu r1, sp+44
	beq r1, r0, .LBB13_51
.LBB13_48:
	addi r13, r0, 43
	beq r1, r13, .LBB13_51
.LBB13_49:
	addi r3, r0, 45
	beq r1, r3, .LBB13_51
.LBB13_50:
	addi r11, sp, 44
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+44, r13
.LBB13_51:
	lui r1, %hi(fio_w)
	ldw r11, r1+%lo(fio_w)
	addi r3, sp, 44
	jal r31, strlen
	blt r11, r16, .LBB13_116
.LBB13_52:
	addi r3, r11, 1
	bne r1, r3, .LBB13_116
.LBB13_53:
	ldbu r3, sp+44
	addi r4, r0, 43
	beq r3, r4, .LBB13_73
.LBB13_54:
	addi r4, r0, 45
	beq r3, r4, .LBB13_73
.LBB13_55:
	addi r4, r0, 48
	bne r3, r4, .LBB13_116
.LBB13_56:
	addi r3, sp, 44
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB13_116
.LBB13_57:
	addi r3, sp, 44
	add r5, r1, r0
	jal r31, memmove
	jal r0, .LBB13_116
.LBB13_58:
	lui r5, %hi(.L.str.15)
	addi r5, r5, %lo(.L.str.15)
	addi r3, sp, 44
	addi r4, r0, 64
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	jal r0, .LBB13_116
.LBB13_59:
	lui r3, %hi(fio_e)
	ldw r5, r3+%lo(fio_e)
	addi r3, r0, 0
	sgt lr, r5, r3
	addi r5, r5, 2
	sub lr, r0, lr
	xori r5, r5, 4
	and r5, r5, lr
	xori r18, r5, 4
	ldw r19, r17+%lo(fio_w)
	slt r5, r19, r18
	sub r5, r0, r5
	and r5, r18, r5
	fle.d r8, r8, r10
	xori r9, r8, 1
	add r8, r0, r0
	bne r9, r0, .LBB13_84
.LBB13_60:
	fle.d r8, r6, r10
	xori r8, r8, 1
	beq r8, r0, .LBB13_80
.LBB13_61:
	addi r8, r0, -1
	jal r0, .LBB13_84
.LBB13_62:
	addi r4, r0, 9
	blt r1, r4, .LBB13_64
.LBB13_63:
	lui r4, %hi(fio_col)
	ldw r5, r4+%lo(fio_col)
	add r1, r1, r5
	addi r1, r1, -8
	stw r4+%lo(fio_col), r1
.LBB13_64:
	lui r1, %hi(fio_len)
	ldw r7, r1+%lo(fio_len)
	lui r4, %hi(fio_col)
	ldw r10, r4+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r8, r0, 32
	add r9, r0, r0
	jal r0, .LBB13_67
.LBB13_65:
	add r10, lr, r0
.LBB13_66:
	addi r9, r9, 1
	beq r9, r3, .LBB13_117
.LBB13_67:
	addi lr, sp, 108
	bgt r10, r5, .LBB13_66
.LBB13_68:
	bge r7, r10, .LBB13_71
.LBB13_69:
	addi r11, r7, 1
	add r7, r7, r6
	stb r7+0, r8
	add r7, r11, r0
	bne r10, r11, .LBB13_69
.LBB13_70:
	stw r1+%lo(fio_len), r10
	add r7, r10, r0
.LBB13_71:
	add lr, lr, r9
	ldbu lr, lr+0
	add r11, r10, r6
	stb r11+0, lr
	addi lr, r10, 1
	stw r4+%lo(fio_col), lr
	blt r10, r7, .LBB13_65
.LBB13_72:
	stw r1+%lo(fio_len), lr
	add r7, lr, r0
	jal r0, .LBB13_65
.LBB13_73:
	addi r3, sp, 44
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB13_116
.LBB13_74:
	addi r4, sp, 44
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB13_116
.LBB13_75:
	addi r5, r1, -1
	jal r31, memmove
	jal r0, .LBB13_116
.LBB13_76:
	addi r3, sp, 44
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB13_115
.LBB13_77:
	addi r4, sp, 44
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB13_115
.LBB13_78:
	addi r5, r1, -1
.LBB13_79:
	jal r31, memmove
	jal r0, .LBB13_115
.LBB13_80:
	addi r8, r0, 1
.LBB13_81:
	add r9, r8, r0
	fdiv.d r10, r10, r6
	fle.d r8, r6, r10
	xori lr, r8, 1
	addi r8, r9, 1
	bne lr, r0, .LBB13_83
.LBB13_82:
	bgt r1, r9, .LBB13_81
.LBB13_83:
	sub r8, r0, r8
.LBB13_84:
	xor r20, r18, r5
	add r21, r8, r4
	sgt r1, r21, r3
	sub r1, r0, r1
	and r6, r21, r1
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
	bgt r21, r0, .LBB13_86
.LBB13_85:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	addi r3, sp, 44
	jal r31, strcat
.LBB13_86:
	sub r11, r19, r20
	lui r1, %hi(fio_sp)
	ldbu r1, r1+%lo(fio_sp)
	addi r19, r0, 1
	bne r1, r19, .LBB13_91
.LBB13_87:
	ldbu r1, sp+44
	beq r1, r0, .LBB13_91
.LBB13_88:
	addi r20, r0, 43
	beq r1, r20, .LBB13_91
.LBB13_89:
	addi r3, r0, 45
	beq r1, r3, .LBB13_91
.LBB13_90:
	addi r12, sp, 44
	addi r13, r12, 1
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, memmove
	stb sp+44, r20
.LBB13_91:
	addi r3, sp, 44
	jal r31, strlen
	blt r11, r19, .LBB13_102
.LBB13_92:
	addi r3, r11, 1
	bne r1, r3, .LBB13_102
.LBB13_93:
	ldbu r3, sp+44
	addi r4, r0, 43
	beq r3, r4, .LBB13_98
.LBB13_94:
	addi r4, r0, 45
	beq r3, r4, .LBB13_98
.LBB13_95:
	addi r4, r0, 48
	bne r3, r4, .LBB13_102
.LBB13_96:
	addi r3, sp, 44
	addi r4, r3, 1
	ldbu r3, r4+0
	addi r5, r0, 46
	bne r3, r5, .LBB13_102
.LBB13_97:
	addi r3, sp, 44
	add r5, r1, r0
	jal r0, .LBB13_101
.LBB13_98:
	addi r3, sp, 44
	addi r3, r3, 1
	ldbu r4, r3+0
	addi r5, r0, 48
	bne r4, r5, .LBB13_102
.LBB13_99:
	addi r4, sp, 44
	addi r4, r4, 2
	ldbu r5, r4+0
	addi r6, r0, 46
	bne r5, r6, .LBB13_102
.LBB13_100:
	addi r5, r1, -1
.LBB13_101:
	jal r31, memmove
.LBB13_102:
	addi r3, sp, 44
	add r4, r11, r0
	jal r31, fio_field
	ldw r1, r17+%lo(fio_w)
	slt r1, r1, r18
	sub r1, r0, r1
	and r1, r18, r1
	xor r10, r18, r1
	blt r10, r19, .LBB13_112
.LBB13_103:
	lui r1, %hi(fio_len)
	ldw r7, r1+%lo(fio_len)
	lui r3, %hi(fio_col)
	ldw r8, r3+%lo(fio_col)
	addi r4, r0, 1022
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	addi r6, r0, 32
	jal r0, .LBB13_106
.LBB13_104:
	add r8, r10, r0
.LBB13_105:
	addi r10, r9, -1
	ble r9, r19, .LBB13_112
.LBB13_106:
	add r9, r10, r0
	bgt r8, r4, .LBB13_105
.LBB13_107:
	bge r7, r8, .LBB13_110
.LBB13_108:
	addi r10, r7, 1
	add r7, r7, r5
	stb r7+0, r6
	add r7, r10, r0
	bne r8, r10, .LBB13_108
.LBB13_109:
	stw r1+%lo(fio_len), r8
	add r7, r8, r0
.LBB13_110:
	add r10, r8, r5
	stb r10+0, r6
	addi r10, r8, 1
	stw r3+%lo(fio_col), r10
	blt r8, r7, .LBB13_104
.LBB13_111:
	stw r1+%lo(fio_len), r10
	add r7, r10, r0
	jal r0, .LBB13_104
.LBB13_112:
	ldw r1, r15+%lo(fio_rep)
	blt r1, r19, .LBB13_114
.LBB13_113:
	addi r1, r1, -1
	stw r15+%lo(fio_rep), r1
.LBB13_114:
	ldw r1, r15+%lo(fio_rep)
	beq r1, r0, .LBB13_122
.LBB13_115:
	beq r16, r0, .LBB13_121
.LBB13_116:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 44
	jal r31, fio_field
.LBB13_117:
	ldw r1, r15+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB13_119
.LBB13_118:
	addi r1, r1, -1
	stw r15+%lo(fio_rep), r1
.LBB13_119:
	ldw r1, r15+%lo(fio_rep)
	bne r1, r0, .LBB13_121
.LBB13_120:
	stw r14+%lo(fio_desc), r0
.LBB13_121:
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
.LBB13_122:
	stw r14+%lo(fio_desc), r0
	bne r16, r0, .LBB13_116
	jal r0, .LBB13_121
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
	add r13, r4, r0
	add r12, r3, r0
	lui r1, %hi(.LCPI14_0)
	addi r1, r1, %lo(.LCPI14_0)
	ldw r19, r1+4
	ldw r18, r1+0
	flt.d r1, r12, r18
	fneg.d r8, r12
	xor r3, r9, r13
	sub r1, r0, r1
	and r3, r3, r1
	xor r7, r13, r3
	xor r3, r8, r12
	and r1, r3, r1
	xor r6, r12, r1
	addi r1, r0, 0
	sne r20, r5, r1
	sub r1, r0, r20
	andi r21, r1, 24
	lui r14, %hi(fio_col)
	ldw r3, r14+%lo(fio_col)
	addi r15, r0, 1022
	lui r16, %hi(fio_len)
	lui r17, %hi(fio_line)
	addi r17, r17, %lo(fio_line)
	bgt r3, r15, .LBB14_6
.LBB14_1:
	ldw r8, r16+%lo(fio_len)
	addi r4, r0, 32
	bge r8, r3, .LBB14_4
.LBB14_2:
	addi r9, r8, 1
	add r8, r8, r17
	stb r8+0, r4
	add r8, r9, r0
	bne r3, r9, .LBB14_2
.LBB14_3:
	stw r16+%lo(fio_len), r3
	add r8, r3, r0
.LBB14_4:
	add r9, r3, r17
	stb r9+0, r4
	addi r4, r3, 1
	stw r14+%lo(fio_col), r4
	blt r3, r8, .LBB14_6
.LBB14_5:
	stw r16+%lo(fio_len), r4
.LBB14_6:
	xori r22, r21, 9
	feq.d r3, r12, r18
	bne r3, r0, .LBB14_9
.LBB14_7:
	lui r3, %hi(.LCPI14_2)
	addi r3, r3, %lo(.LCPI14_2)
	ldw r9, r3+4
	ldw r8, r3+0
	fle.d r3, r8, r6
	xori r3, r3, 1
	bne r3, r0, .LBB14_24
.LBB14_8:
	andi r3, r1, 8
	lui r4, %hi(.LCPI14_1)
	addi r4, r4, %lo(.LCPI14_1)
	add r3, r4, r3
	ldw r9, r3+4
	ldw r8, r3+0
	flt.d r3, r6, r8
	xori r3, r3, 1
	bne r3, r0, .LBB14_24
.LBB14_9:
	lui r1, %hi(.LCPI14_3)
	addi r1, r1, %lo(.LCPI14_3)
	ldw r5, r1+4
	ldw r4, r1+0
	fle.d r1, r4, r6
	xori r3, r1, 1
	addi r23, r0, 1
	add r1, r23, r0
	bne r3, r0, .LBB14_12
.LBB14_10:
	lui r1, %hi(.LCPI14_4)
	addi r1, r1, %lo(.LCPI14_4)
	ldw r9, r1+4
	ldw r8, r1+0
	add r10, r6, r0
	add r11, r7, r0
	add r1, r0, r0
.LBB14_11:
	fdiv.d r10, r10, r8
	addi r1, r1, 1
	fle.d r3, r4, r10
	bne r3, r0, .LBB14_11
.LBB14_12:
	flt.d r3, r6, r4
	feq.d r4, r12, r18
	xori r4, r4, 1
	and r24, r4, r3
	sub r3, r0, r24
	and r3, r1, r3
	xor r1, r1, r3
	sub r6, r22, r1
	lui r5, %hi(.L.str.13)
	addi r5, r5, %lo(.L.str.13)
	addi r11, sp, 24
	addi r4, r0, 16
	add r3, r11, r0
	jal r31, snprintf
	addi r3, sp, 40
	addi r4, r0, 64
	add r5, r11, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	bne r24, r23, .LBB14_15
.LBB14_13:
	flt.d r11, r12, r18
	addi r3, sp, 40
	jal r31, strlen
	add r3, r11, r22
	addi r3, r3, 2
	bleu r1, r3, .LBB14_15
.LBB14_14:
	addi r1, sp, 40
	add r1, r1, r3
	stb r1+0, r0
.LBB14_15:
	addi r11, r20, 4
	xori r4, r21, 12
	addi r3, sp, 40
	jal r31, fio_field
	ldw r4, r16+%lo(fio_len)
	ldw r5, r14+%lo(fio_col)
	addi r1, r0, 1
	addi r3, r0, 32
	jal r0, .LBB14_18
.LBB14_16:
	add r5, r7, r0
.LBB14_17:
	addi r11, r6, -1
	bleu r6, r1, .LBB14_29
.LBB14_18:
	add r6, r11, r0
	bgt r5, r15, .LBB14_17
.LBB14_19:
	bge r4, r5, .LBB14_22
.LBB14_20:
	addi r7, r4, 1
	add r4, r4, r17
	stb r4+0, r3
	add r4, r7, r0
	bne r5, r7, .LBB14_20
.LBB14_21:
	stw r16+%lo(fio_len), r5
	add r4, r5, r0
.LBB14_22:
	add r7, r5, r17
	stb r7+0, r3
	addi r7, r5, 1
	stw r14+%lo(fio_col), r7
	blt r5, r4, .LBB14_16
.LBB14_23:
	stw r16+%lo(fio_len), r7
	add r4, r7, r0
	jal r0, .LBB14_16
.LBB14_24:
	andi r1, r1, 9
	addi r11, r1, 16
	addi r6, r22, -1
	lui r1, %hi(.L.str.85)
	addi r1, r1, %lo(.L.str.85)
	addi r14, sp, 24
	addi r4, r0, 16
	add r3, r14, r0
	add r16, r5, r0
	add r5, r1, r0
	jal r31, snprintf
	addi r15, sp, 40
	addi r4, r0, 64
	add r3, r15, r0
	add r5, r14, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	addi r4, r0, 69
	add r3, r15, r0
	jal r31, strchr
	beq r16, r0, .LBB14_28
.LBB14_25:
	beq r1, r0, .LBB14_28
.LBB14_26:
	addi r12, r1, 2
	add r3, r12, r0
	jal r31, strlen
	addi r3, r0, 2
	bgt r1, r3, .LBB14_28
.LBB14_27:
	addi r3, r0, 3
	sub r13, r3, r1
	add r3, r12, r13
	addi r5, r1, 1
	add r4, r12, r0
	jal r31, memmove
	addi r4, r0, 48
	add r3, r12, r0
	add r5, r13, r0
	jal r31, memset
.LBB14_28:
	addi r3, sp, 40
	add r4, r11, r0
	jal r31, fio_field
.LBB14_29:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
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
	stw sp+112, r22
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
	ldw r3, r3+%lo(fio_scale)
	sub r4, r0, r1
	sgt r4, r3, r4
	addi r5, r1, 2
	slt r5, r3, r5
	sub r5, r0, r5
	and r3, r3, r5
	sub r4, r0, r4
	and r19, r3, r4
	add r20, r19, r0
	addi r18, r0, 0
	sgt r3, r19, r18
	add r4, r19, r1
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
	addi r15, sp, 32
	addi r4, r0, 16
	add r3, r15, r0
	jal r31, snprintf
	addi r16, sp, 48
	addi r4, r0, 64
	add r3, r16, r0
	add r5, r15, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	ldbu r22, sp+48
	addi r21, r0, 45
	seq r1, r22, r21
	addi r3, r0, 43
	seq r3, r22, r3
	or  r1, r1, r3
	or  r1, r1, r16
	addi r3, r1, 1
	addi r1, r0, 68
	addi r16, r0, 46
	addi r4, r0, 69
	addi r5, r0, 101
	add r15, r0, r0
	jal r0, .LBB15_3
.LBB15_1:
	addi r7, sp, 48
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
	bne r6, r0, .LBB15_1
	jal r0, .LBB15_9
.LBB15_6:
	beq r6, r4, .LBB15_8
.LBB15_7:
	bne r6, r5, .LBB15_1
.LBB15_8:
	addi r5, r0, 10
	add r4, r0, r0
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
	add r6, r14, r0
	bne r22, r21, .LBB15_12
.LBB15_11:
	addi r6, r14, 1
	stb r14+0, r21
.LBB15_12:
	xor r1, r1, r3
	blt r20, r17, .LBB15_17
.LBB15_13:
	blt r15, r17, .LBB15_22
.LBB15_14:
	xor r3, r19, r15
	slt r7, r19, r15
	sub r7, r0, r7
	and r3, r3, r7
	xor r3, r15, r3
	addi r7, sp, 48
	addi r8, r7, 40
	add r7, r0, r0
.LBB15_15:
	add r9, r6, r7
	add r10, r8, r7
	ldbu r10, r10+0
	stb r9+0, r10
	addi r7, r7, 1
	bne r3, r7, .LBB15_15
.LBB15_16:
	add r6, r6, r7
	jal r0, .LBB15_23
.LBB15_17:
	addi r8, r0, 48
	stb r6+0, r8
	addi r9, r6, 2
	stb r6+1, r16
	addi r7, r0, 55
	beq r20, r0, .LBB15_28
.LBB15_18:
	sub r3, r9, r14
	bgt r3, r7, .LBB15_28
.LBB15_19:
	sub r3, r6, r14
	addi lr, r3, 3
	addi r6, r0, -2
	addi r10, r0, 56
.LBB15_20:
	addi r3, r9, 1
	stb r9+0, r8
	bgt r20, r6, .LBB15_29
.LBB15_21:
	add r16, lr, r0
	addi r20, r20, 1
	addi lr, lr, 1
	add r9, r3, r0
	blt r16, r10, .LBB15_20
	jal r0, .LBB15_29
.LBB15_22:
	add r7, r0, r0
.LBB15_23:
	stb r6+0, r16
	addi r3, r6, 1
	bge r7, r15, .LBB15_35
.LBB15_24:
	sub r8, r3, r14
	addi r9, r0, 55
	bgt r8, r9, .LBB15_35
.LBB15_25:
	sub r6, r6, r14
	addi r9, r6, 2
	addi r6, sp, 48
	addi r6, r6, 40
	addi r8, r0, 56
.LBB15_26:
	add r10, r9, r0
	add r9, r6, r7
	ldbu r9, r9+0
	stb r3+0, r9
	addi r7, r7, 1
	addi r3, r3, 1
	bge r7, r15, .LBB15_35
.LBB15_27:
	addi r9, r10, 1
	blt r10, r8, .LBB15_26
	jal r0, .LBB15_35
.LBB15_28:
	add r3, r9, r0
.LBB15_29:
	blt r15, r17, .LBB15_35
.LBB15_30:
	sub r8, r3, r14
	bgt r8, r7, .LBB15_35
.LBB15_31:
	addi r6, sp, 48
	addi r6, r6, 40
	addi r7, r8, 1
	addi r8, r0, 56
	add r9, r0, r0
.LBB15_32:
	add r10, r3, r9
	add lr, r6, r9
	ldbu lr, lr+0
	stb r10+0, lr
	addi r10, r9, 1
	bge r10, r15, .LBB15_34
.LBB15_33:
	add lr, r7, r9
	add r9, r10, r0
	blt lr, r8, .LBB15_32
.LBB15_34:
	add r3, r3, r10
.LBB15_35:
	feq.d r4, r12, r4
	xori r4, r4, 1
	sub r4, r0, r4
	and r4, r19, r4
	sub r7, r1, r4
	srai r1, r7, 31
	xor r4, r7, r1
	sub r5, r4, r1
	lui r1, %hi(fio_e)
	ldw r4, r1+%lo(fio_e)
	addi r1, r0, 100
	sltu r6, r5, r1
	bne r4, r0, .LBB15_38
.LBB15_36:
	addi r1, r5, -100
	addi r8, r0, 900
	bgeu r1, r8, .LBB15_38
.LBB15_37:
	add r1, r3, r0
	jal r0, .LBB15_39
.LBB15_38:
	addi r1, r3, 1
	stb r3+0, r11
.LBB15_39:
	xori r6, r6, 3
	slt r3, r7, r18
	sub r3, r0, r3
	andi r3, r3, 6
	xori r3, r3, 43
	stb r1+0, r3
	lui r3, 838861
	addi r7, r3, -819
	addi r8, r0, 10
	addi r9, r0, 14
	add r3, r0, r0
.LBB15_40:
	add r10, r3, r0
	mulhu r3, r5, r7
	srli lr, r3, 3
	mul r3, lr, r8
	sub r3, r5, r3
	ori  r11, r3, 48
	addi r3, r10, 1
	addi r12, sp, 16
	add r12, r12, r10
	stb r12+0, r11
	bltu r5, r8, .LBB15_42
.LBB15_41:
	add r5, lr, r0
	bltu r10, r9, .LBB15_40
.LBB15_42:
	sgt r5, r4, r18
	addi r7, r0, 15
	slt r7, r4, r7
	sub r7, r0, r7
	xori r4, r4, 15
	and r4, r4, r7
	xor r4, r4, r6
	sub r5, r0, r5
	xori r4, r4, 15
	and r4, r4, r5
	xor r4, r6, r4
	addi r5, sp, 16
	bge r3, r4, .LBB15_46
.LBB15_43:
	add r5, r5, r3
	addi r6, r0, 48
	add r7, r4, r0
.LBB15_44:
	stb r5+0, r6
	addi r5, r5, 1
	addi r7, r7, -1
	bne r3, r7, .LBB15_44
.LBB15_45:
	add r3, r4, r0
.LBB15_46:
	addi r1, r1, 1
	addi r4, sp, 16
	addi r4, r4, -1
.LBB15_47:
	add r5, r3, r0
	addi r3, r3, -1
	add r6, r4, r5
	ldbu r6, r6+0
	stb r1+0, r6
	addi r1, r1, 1
	bgt r5, r17, .LBB15_47
.LBB15_48:
	stb r1+0, r0
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
.Lfunc_end15:
	.size	fio_efmt, .Lfunc_end15-fio_efmt
                                        # -- End function
	.globl	f77_wr_r                        # -- Begin function f77_wr_r
	.p2align	2
	.type	f77_wr_r,@function
f77_wr_r:                               # @f77_wr_r
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r3
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB16_2
.LBB16_1:
	fcvt.d.s r4, r3
	add r3, r4, r0
	add r4, r5, r0
	add r5, r0, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fio_list_out_real
.LBB16_2:
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB16_7
.LBB16_3:
	ldw r4, r12+%lo(fio_rep)
	ble r4, r0, .LBB16_7
.LBB16_4:
	addi r4, r0, 65
	bne r1, r4, .LBB16_8
.LBB16_5:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, r4, -1
	addi r5, r0, 3
	lui r1, %hi(fio_col)
	bgeu r3, r5, .LBB16_9
.LBB16_6:
	add r3, r4, r0
	jal r0, .LBB16_11
.LBB16_7:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	jal r31, fio_next_desc
	add r3, r13, r0
	addi r4, r0, 65
	beq r1, r4, .LBB16_5
.LBB16_8:
	fcvt.d.s r4, r3
	add r3, r4, r0
	add r4, r5, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_wr_d
.LBB16_9:
	addi r3, r0, 4
	addi r5, r0, 5
	blt r4, r5, .LBB16_11
.LBB16_10:
	ldw r5, r1+%lo(fio_col)
	add r4, r4, r5
	addi r4, r4, -4
	stw r1+%lo(fio_col), r4
.LBB16_11:
	lui r4, %hi(fio_len)
	ldw r8, r4+%lo(fio_len)
	ldw r10, r1+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	add r9, r0, r0
	jal r0, .LBB16_14
.LBB16_12:
	add r10, lr, r0
.LBB16_13:
	addi r9, r9, 1
	beq r9, r3, .LBB16_20
.LBB16_14:
	addi lr, sp, 16
	bgt r10, r5, .LBB16_13
.LBB16_15:
	bge r8, r10, .LBB16_18
.LBB16_16:
	addi r13, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, r13, r0
	bne r10, r13, .LBB16_16
.LBB16_17:
	stw r4+%lo(fio_len), r10
	add r8, r10, r0
.LBB16_18:
	add lr, lr, r9
	ldbu lr, lr+0
	add r13, r10, r6
	stb r13+0, lr
	addi lr, r10, 1
	stw r1+%lo(fio_col), lr
	blt r10, r8, .LBB16_12
.LBB16_19:
	stw r4+%lo(fio_len), lr
	add r8, lr, r0
	jal r0, .LBB16_12
.LBB16_20:
	ldw r1, r12+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB16_22
.LBB16_21:
	addi r1, r1, -1
	stw r12+%lo(fio_rep), r1
.LBB16_22:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB16_24
.LBB16_23:
	stw r11+%lo(fio_desc), r0
.LBB16_24:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
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
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB17_48
.LBB17_1:
	lui r13, %hi(fio_col)
	ldw r1, r13+%lo(fio_col)
	addi r15, r0, 1022
	lui r14, %hi(fio_len)
	lui r12, %hi(fio_line)
	addi r12, r12, %lo(fio_line)
	bgt r1, r15, .LBB17_7
.LBB17_2:
	ldw r5, r14+%lo(fio_len)
	addi r4, r0, 32
	bge r5, r1, .LBB17_5
.LBB17_3:
	addi r6, r5, 1
	add r5, r5, r12
	stb r5+0, r4
	add r5, r6, r0
	bne r1, r6, .LBB17_3
.LBB17_4:
	stw r14+%lo(fio_len), r1
	add r5, r1, r0
.LBB17_5:
	add r6, r1, r12
	stb r6+0, r4
	addi r4, r1, 1
	stw r13+%lo(fio_col), r4
	blt r1, r5, .LBB17_7
.LBB17_6:
	stw r14+%lo(fio_len), r4
.LBB17_7:
	ldw r16, r14+%lo(fio_len)
	ldw r1, r13+%lo(fio_col)
	bgt r1, r15, .LBB17_14
.LBB17_8:
	add r4, r16, r0
	bge r16, r1, .LBB17_12
.LBB17_9:
	addi r4, r0, 32
	add r5, r16, r0
.LBB17_10:
	addi r6, r5, 1
	add r5, r5, r12
	stb r5+0, r4
	add r5, r6, r0
	bne r1, r6, .LBB17_10
.LBB17_11:
	stw r14+%lo(fio_len), r1
	add r4, r1, r0
.LBB17_12:
	add r5, r1, r12
	addi r6, r0, 40
	stb r5+0, r6
	addi r5, r1, 1
	stw r13+%lo(fio_col), r5
	blt r1, r4, .LBB17_14
.LBB17_13:
	stw r14+%lo(fio_len), r5
.LBB17_14:
	ldw r18, r14+%lo(fio_len)
	fcvt.d.s r4, r3
	add r3, r4, r0
	add r4, r5, r0
	add r5, r0, r0
	jal r31, fio_list_out_real
	ldw r1, r14+%lo(fio_len)
	add r3, r18, r0
	bge r18, r1, .LBB17_19
.LBB17_15:
	addi r4, r0, 32
	add r3, r18, r0
.LBB17_16:
	add r5, r3, r12
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
	addi r17, r0, 1
	blt r4, r17, .LBB17_27
.LBB17_25:
	add r5, r18, r12
.LBB17_26:
	add r6, r3, r12
	ldbu r6, r6+0
	stb r5+0, r6
	addi r3, r3, 1
	addi r5, r5, 1
	bne r1, r3, .LBB17_26
.LBB17_27:
	add r1, r4, r18
	stw r14+%lo(fio_len), r1
	stw r13+%lo(fio_col), r1
	bgt r1, r15, .LBB17_29
.LBB17_28:
	add r3, r1, r12
	addi r4, r0, 44
	stb r3+0, r4
	addi r1, r1, 1
	stw r13+%lo(fio_col), r1
	stw r14+%lo(fio_len), r1
.LBB17_29:
	ldw r18, r14+%lo(fio_len)
	fcvt.d.s r4, r11
	add r3, r4, r0
	add r4, r5, r0
	add r5, r0, r0
	jal r31, fio_list_out_real
	ldw r1, r14+%lo(fio_len)
	add r3, r18, r0
	bge r18, r1, .LBB17_34
.LBB17_30:
	addi r4, r0, 32
	add r3, r18, r0
.LBB17_31:
	add r5, r3, r12
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
	blt r4, r17, .LBB17_42
.LBB17_40:
	add r5, r18, r12
.LBB17_41:
	add r6, r3, r12
	ldbu r6, r6+0
	stb r5+0, r6
	addi r3, r3, 1
	addi r5, r5, 1
	bne r1, r3, .LBB17_41
.LBB17_42:
	add r1, r4, r18
	stw r14+%lo(fio_len), r1
	stw r13+%lo(fio_col), r1
	bgt r1, r15, .LBB17_44
.LBB17_43:
	add r3, r1, r12
	addi r4, r0, 41
	stb r3+0, r4
	addi r1, r1, 1
	stw r13+%lo(fio_col), r1
	stw r14+%lo(fio_len), r1
.LBB17_44:
	ldw r1, r14+%lo(fio_len)
	sub r5, r1, r16
	addi r3, r0, 34
	bgt r5, r3, .LBB17_47
.LBB17_45:
	addi r3, r0, 35
	sub r11, r3, r5
	add r1, r11, r1
	bgt r1, r15, .LBB17_47
.LBB17_46:
	add r12, r16, r12
	add r3, r12, r11
	add r4, r12, r0
	jal r31, memmove
	addi r4, r0, 32
	add r3, r12, r0
	add r5, r11, r0
	jal r31, memset
	ldw r1, r14+%lo(fio_len)
	add r1, r1, r11
	stw r14+%lo(fio_len), r1
.LBB17_47:
	ldw r1, r14+%lo(fio_len)
	stw r13+%lo(fio_col), r1
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
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
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB18_20
.LBB18_1:
	lui r1, %hi(fio_first_item)
	ldbu r7, r1+%lo(fio_first_item)
	lui r5, %hi(fio_prev_char)
	lui r6, %hi(fio_col)
	bne r7, r0, .LBB18_3
.LBB18_2:
	ldbu r7, r5+%lo(fio_prev_char)
	andi r7, r7, 1
	bne r7, r0, .LBB18_9
.LBB18_3:
	ldw r7, r6+%lo(fio_col)
	addi r8, r0, 1022
	bgt r7, r8, .LBB18_9
.LBB18_4:
	lui r8, %hi(fio_len)
	ldw r9, r8+%lo(fio_len)
	lui r10, %hi(fio_line)
	addi r10, r10, %lo(fio_line)
	addi lr, r0, 32
	bge r9, r7, .LBB18_7
.LBB18_5:
	addi r11, r9, 1
	add r9, r9, r10
	stb r9+0, lr
	add r9, r11, r0
	bne r7, r11, .LBB18_5
.LBB18_6:
	stw r8+%lo(fio_len), r7
	add r9, r7, r0
.LBB18_7:
	add r10, r7, r10
	stb r10+0, lr
	addi r10, r7, 1
	stw r6+%lo(fio_col), r10
	blt r7, r9, .LBB18_9
.LBB18_8:
	stw r8+%lo(fio_len), r10
.LBB18_9:
	addi r7, r0, 1
	blt r4, r7, .LBB18_19
.LBB18_10:
	lui r8, %hi(fio_len)
	ldw r11, r8+%lo(fio_len)
	ldw r13, r6+%lo(fio_col)
	addi r9, r0, 1022
	lui r10, %hi(fio_line)
	addi r10, r10, %lo(fio_line)
	addi lr, r0, 32
	add r12, r0, r0
	jal r0, .LBB18_13
.LBB18_11:
	add r13, r14, r0
.LBB18_12:
	addi r12, r12, 1
	beq r12, r4, .LBB18_19
.LBB18_13:
	bgt r13, r9, .LBB18_12
.LBB18_14:
	add r14, r3, r12
	ldbu r14, r14+0
	bge r11, r13, .LBB18_17
.LBB18_15:
	addi r15, r11, 1
	add r11, r11, r10
	stb r11+0, lr
	add r11, r15, r0
	bne r13, r15, .LBB18_15
.LBB18_16:
	stw r8+%lo(fio_len), r13
	add r11, r13, r0
.LBB18_17:
	add r15, r13, r10
	stb r15+0, r14
	addi r14, r13, 1
	stw r6+%lo(fio_col), r14
	blt r13, r11, .LBB18_11
.LBB18_18:
	stw r8+%lo(fio_len), r14
	add r11, r14, r0
	jal r0, .LBB18_11
.LBB18_19:
	stb r1+%lo(fio_first_item), r0
	stb r5+%lo(fio_prev_char), r7
	jal r0, .LBB18_43
.LBB18_20:
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB18_22
.LBB18_21:
	ldw r1, r12+%lo(fio_rep)
	bgt r1, r0, .LBB18_23
.LBB18_22:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	add r14, r4, r0
	jal r31, fio_next_desc
	add r4, r14, r0
	add r3, r13, r0
	beq r1, r0, .LBB18_43
.LBB18_23:
	lui r1, %hi(fio_w)
	ldw r5, r1+%lo(fio_w)
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
	ldw r7, r6+%lo(fio_col)
	add r5, r7, r5
	stw r6+%lo(fio_col), r5
.LBB18_29:
	blt r4, r1, .LBB18_39
.LBB18_30:
	lui r5, %hi(fio_len)
	ldw r9, r5+%lo(fio_len)
	lui r6, %hi(fio_col)
	ldw r13, r6+%lo(fio_col)
	addi r7, r0, 1022
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	addi r10, r0, 32
	add lr, r0, r0
	jal r0, .LBB18_33
.LBB18_31:
	add r13, r14, r0
.LBB18_32:
	addi lr, lr, 1
	beq lr, r4, .LBB18_39
.LBB18_33:
	bgt r13, r7, .LBB18_32
.LBB18_34:
	add r14, r3, lr
	ldbu r14, r14+0
	bge r9, r13, .LBB18_37
.LBB18_35:
	addi r15, r9, 1
	add r9, r9, r8
	stb r9+0, r10
	add r9, r15, r0
	bne r13, r15, .LBB18_35
.LBB18_36:
	stw r5+%lo(fio_len), r13
	add r9, r13, r0
.LBB18_37:
	add r15, r13, r8
	stb r15+0, r14
	addi r14, r13, 1
	stw r6+%lo(fio_col), r14
	blt r13, r9, .LBB18_31
.LBB18_38:
	stw r5+%lo(fio_len), r14
	add r9, r14, r0
	jal r0, .LBB18_31
.LBB18_39:
	ldw r3, r12+%lo(fio_rep)
	blt r3, r1, .LBB18_41
.LBB18_40:
	addi r1, r3, -1
	stw r12+%lo(fio_rep), r1
.LBB18_41:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB18_43
.LBB18_42:
	stw r11+%lo(fio_desc), r0
.LBB18_43:
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
	sub r1, r0, r1
	xor r3, r15, r17
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
	xori r3, r1, 1
	lui r1, %hi(.LCPI20_1)
	addi r1, r1, %lo(.LCPI20_1)
	ldw r5, r1+4
	ldw r4, r1+0
	lui r1, %hi(.LCPI20_2)
	addi r1, r1, %lo(.LCPI20_2)
	bne r3, r0, .LBB20_2
.LBB20_1:
	ldw r19, r1+4
	ldw r18, r1+0
	fmul.d r14, r14, r18
	fmul.d r16, r16, r18
	fmul.d r6, r6, r18
	fmul.d r4, r4, r18
	flt.d r3, r8, r6
	bne r3, r0, .LBB20_1
.LBB20_2:
	lui r3, %hi(.LCPI20_3)
	addi r3, r3, %lo(.LCPI20_3)
	ldw r19, r3+4
	ldw r18, r3+0
	flt.d r3, r18, r6
	xori r3, r3, 1
	bne r3, r0, .LBB20_6
.LBB20_3:
	ldw r21, r1+4
	ldw r20, r1+0
	flt.d r1, r6, r20
	xori r1, r1, 1
	bne r1, r0, .LBB20_6
.LBB20_4:
	fmul.d r14, r14, r8
	fmul.d r16, r16, r8
	fmul.d r6, r6, r8
	fmul.d r4, r4, r8
	fle.d r1, r6, r18
	bne r1, r0, .LBB20_6
.LBB20_5:
	flt.d r1, r6, r20
	bne r1, r0, .LBB20_4
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
	addi sp, sp, -32
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	stw sp+12, r15
	stw sp+8, r16
	stw sp+4, r17
	lui r1, %hi(.LCPI21_0)
	addi r1, r1, %lo(.LCPI21_0)
	ldw r9, r1+4
	ldw r8, r1+0
	lui r1, %hi(.LCPI21_1)
	addi r1, r1, %lo(.LCPI21_1)
	ldw r11, r1+4
	ldw r10, r1+0
	beq r6, r0, .LBB21_7
.LBB21_1:
	srai r1, r6, 31
	xor r7, r6, r1
	sub r1, r7, r1
	fcvt.d.s r12, r5
	fcvt.d.s r4, r4
	jal r0, .LBB21_3
.LBB21_2:
	beq r1, r0, .LBB21_7
.LBB21_3:
	andi r7, r1, 1
	beq r7, r0, .LBB21_5
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
	srli r1, r1, 1
	beq r1, r0, .LBB21_2
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
	ldw r17, sp+4
	ldw r16, sp+8
	ldw r15, sp+12
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	addi sp, sp, 32
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
	ldw r1, r1+%lo(.LCPI24_0)
	feq.s r3, r4, r1
	xori r3, r3, 1
	bne r3, r0, .LBB24_3
.LBB24_1:
	feq.s r3, r5, r1
	xori r3, r3, 1
	bne r3, r0, .LBB24_3
.LBB24_2:
	stw r11+0, r0
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
	lui r3, %hi(.LCPI25_1)
	ldw r1, r3+%lo(.LCPI25_1)
	fle.s r4, r1, r13
	xori r4, r4, 1
	beq r4, r0, .LBB25_4
.LBB25_1:
	lui r1, %hi(.LCPI25_2)
	addi r1, r1, %lo(.LCPI25_2)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r4, r16, r4
	ldw r13, r3+%lo(.LCPI25_1)
	add r1, r13, r0
	bne r4, r0, .LBB25_3
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
	fcvt.s.d r3, r16
	stw r11+0, r3
	lui r3, %hi(.LCPI25_2)
	addi r3, r3, %lo(.LCPI25_2)
	ldw r5, r3+4
	ldw r4, r3+0
	feq.d r3, r16, r4
	bne r3, r0, .LBB25_6
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
	ldw r3, r3+%lo(.LCPI29_0)
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
	ldw r3, r3+%lo(.LCPI69_0)
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
	add r11, r5, r0
	beq r6, r0, .LBB76_6
.LBB76_1:
	add r12, r6, r0
	bgt r6, r11, .LBB76_5
.LBB76_2:
	add r13, r4, r0
	add r14, r3, r0
	add r15, r0, r0
.LBB76_3:
	add r3, r14, r15
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcmp
	beq r1, r0, .LBB76_7
.LBB76_4:
	addi r15, r15, 1
	add r1, r12, r15
	ble r1, r11, .LBB76_3
.LBB76_5:
	add r1, r0, r0
	jal r0, .LBB76_8
.LBB76_6:
	addi r1, r0, 0
	sgt r1, r11, r1
	jal r0, .LBB76_8
.LBB76_7:
	addi r1, r15, 1
.LBB76_8:
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
	add r11, r4, r0
	beq r6, r0, .LBB77_6
.LBB77_1:
	add r12, r6, r0
	bgt r6, r11, .LBB77_5
.LBB77_2:
	add r13, r5, r0
	add r14, r3, r0
	add r15, r0, r0
.LBB77_3:
	add r3, r14, r15
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcmp
	beq r1, r0, .LBB77_7
.LBB77_4:
	addi r15, r15, 1
	add r1, r12, r15
	ble r1, r11, .LBB77_3
.LBB77_5:
	add r1, r0, r0
	jal r0, .LBB77_8
.LBB77_6:
	addi r1, r0, 0
	sgt r1, r11, r1
	jal r0, .LBB77_8
.LBB77_7:
	addi r1, r15, 1
.LBB77_8:
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
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_csqrt
	.type	.LCPI80_0,@object
.LCPI80_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI80_0, 8
	.type	.LCPI80_2,@object
.LCPI80_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI80_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI80_1,@object
.LCPI80_1:
	.word	0x00000000                      # float 0
	.size	.LCPI80_1, 4
	.text
	.globl	f77i_csqrt
	.p2align	2
	.type	f77i_csqrt,@function
f77i_csqrt:                             # @f77i_csqrt
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	stw sp+20, r18
	stw sp+16, r19
	ldw r16, r3+0
	ldw r11, r3+4
	fcvt.d.s r14, r16
	fcvt.d.s r12, r11
	fmul.d r4, r12, r12
	fmul.d r6, r14, r14
	fadd.d r4, r6, r4
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r18, r1, r0
	add r19, r2, r0
	add r3, r14, r0
	add r4, r15, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r4, r18, r4
	lui r1, %hi(.LCPI80_0)
	addi r1, r1, %lo(.LCPI80_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r14, r1, r0
	add r15, r2, r0
	lui r1, %hi(.LCPI80_1)
	ldw r3, r1+%lo(.LCPI80_1)
	fle.s r4, r3, r16
	xori r4, r4, 1
	beq r4, r0, .LBB80_4
.LBB80_1:
	lui r3, %hi(.LCPI80_2)
	addi r3, r3, %lo(.LCPI80_2)
	ldw r5, r3+4
	ldw r4, r3+0
	feq.d r3, r14, r4
	ldw r16, r1+%lo(.LCPI80_1)
	add r1, r16, r0
	bne r3, r0, .LBB80_3
.LBB80_2:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r14, r14
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
.LBB80_3:
	flt.s r3, r11, r16
	fneg.d r4, r14
	xor r6, r5, r15
	sub r3, r0, r3
	and r6, r6, r3
	xor r7, r15, r6
	xor r4, r4, r14
	and r3, r4, r3
	xor r6, r14, r3
	fcvt.s.d r3, r6
	jal r0, .LBB80_6
.LBB80_4:
	fcvt.s.d r1, r14
	lui r4, %hi(.LCPI80_2)
	addi r4, r4, %lo(.LCPI80_2)
	ldw r7, r4+4
	ldw r6, r4+0
	feq.d r4, r14, r6
	bne r4, r0, .LBB80_6
.LBB80_5:
	fadd.d r4, r14, r14
	fdiv.d r4, r12, r4
	fcvt.s.d r3, r4
.LBB80_6:
	add r2, r3, r0
	ldw r19, sp+16
	ldw r18, sp+20
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end80:
	.size	f77i_csqrt, .Lfunc_end80-f77i_csqrt
                                        # -- End function
	.globl	f77i_cexp                       # -- Begin function f77i_cexp
	.p2align	2
	.type	f77i_cexp,@function
f77i_cexp:                              # @f77i_cexp
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	ldw r1, r3+0
	ldw r11, r3+4
	fcvt.d.s r4, r1
	add r3, r4, r0
	add r4, r5, r0
	jal r31, exp
	add r12, r1, r0
	add r13, r2, r0
	fcvt.d.s r14, r11
	add r3, r14, r0
	add r4, r15, r0
	jal r31, cos
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r12, r4
	fcvt.s.d r11, r4
	add r3, r14, r0
	add r4, r15, r0
	jal r31, sin
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r12, r4
	fcvt.s.d r2, r4
	add r1, r11, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end81:
	.size	f77i_cexp, .Lfunc_end81-f77i_cexp
                                        # -- End function
	.globl	f77i_clog                       # -- Begin function f77i_clog
	.p2align	2
	.type	f77i_clog,@function
f77i_clog:                              # @f77i_clog
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	ldw r1, r3+0
	ldw r3, r3+4
	fcvt.d.s r12, r1
	fcvt.d.s r14, r3
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
	fcvt.s.d r11, r4
	add r3, r14, r0
	add r4, r15, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, atan2
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r2, r4
	add r1, r11, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end82:
	.size	f77i_clog, .Lfunc_end82-f77i_clog
                                        # -- End function
	.globl	f77i_csin                       # -- Begin function f77i_csin
	.p2align	2
	.type	f77i_csin,@function
f77i_csin:                              # @f77i_csin
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
	ldw r1, r3+0
	ldw r11, r3+4
	fcvt.d.s r12, r1
	add r3, r12, r0
	add r4, r13, r0
	jal r31, sin
	add r16, r1, r0
	add r17, r2, r0
	fcvt.d.s r14, r11
	add r3, r14, r0
	add r4, r15, r0
	jal r31, cosh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r16, r4
	fcvt.s.d r11, r4
	add r3, r12, r0
	add r4, r13, r0
	jal r31, cos
	add r12, r1, r0
	add r13, r2, r0
	add r3, r14, r0
	add r4, r15, r0
	jal r31, sinh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r12, r4
	fcvt.s.d r2, r4
	add r1, r11, r0
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
.Lfunc_end83:
	.size	f77i_csin, .Lfunc_end83-f77i_csin
                                        # -- End function
	.globl	f77i_ccos                       # -- Begin function f77i_ccos
	.p2align	2
	.type	f77i_ccos,@function
f77i_ccos:                              # @f77i_ccos
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
	ldw r1, r3+0
	ldw r11, r3+4
	fcvt.d.s r12, r1
	add r3, r12, r0
	add r4, r13, r0
	jal r31, cos
	add r16, r1, r0
	add r17, r2, r0
	fcvt.d.s r14, r11
	add r3, r14, r0
	add r4, r15, r0
	jal r31, cosh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r16, r4
	fcvt.s.d r11, r4
	add r3, r12, r0
	add r4, r13, r0
	jal r31, sin
	add r4, r1, r0
	add r5, r2, r0
	fneg.d r12, r4
	add r3, r14, r0
	add r4, r15, r0
	jal r31, sinh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r4, r12
	fcvt.s.d r2, r4
	add r1, r11, r0
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
.Lfunc_end84:
	.size	f77i_ccos, .Lfunc_end84-f77i_ccos
                                        # -- End function
	.globl	f77i_conjg                      # -- Begin function f77i_conjg
	.p2align	2
	.type	f77i_conjg,@function
f77i_conjg:                             # @f77i_conjg
# %bb.0:
	ldw r1, r3+0
	ldw r3, r3+4
	lui r4, 524288
	xor r2, r3, r4
	jalr r0, r31, 0
.Lfunc_end85:
	.size	f77i_conjg, .Lfunc_end85-f77i_conjg
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
	blt r14, r1, .LBB86_2
.LBB86_1:
	add r3, r12, r0
	add r4, r5, r0
	add r5, r14, r0
	jal r31, memmove
.LBB86_2:
	bge r13, r11, .LBB86_5
.LBB86_3:
	add r1, r12, r14
	sub r3, r11, r14
	addi r4, r0, 32
.LBB86_4:
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB86_4
.LBB86_5:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end86:
	.size	f77_strcpy, .Lfunc_end86-f77_strcpy
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
	xor r1, r4, r6
	sgt r7, r4, r6
	sub r7, r0, r7
	and r1, r1, r7
	xor r7, r6, r1
	addi r1, r0, 1
	blt r7, r1, .LBB87_8
.LBB87_1:
	addi r8, r0, 32
	add r9, r0, r0
                                        # implicit-def: $r1
.LBB87_2:
	add r10, r8, r0
	bge r9, r4, .LBB87_4
.LBB87_3:
	add r10, r3, r9
	ldbu r10, r10+0
.LBB87_4:
	add lr, r8, r0
	bge r9, r6, .LBB87_6
.LBB87_5:
	add lr, r5, r9
	ldbu lr, lr+0
.LBB87_6:
	seq r11, r10, lr
	sltu r12, r10, lr
	sub r12, r0, r12
	ori  r12, r12, 1
	xor r1, r1, r12
	sub r11, r0, r11
	and r1, r1, r11
	xor r1, r12, r1
	bne r10, lr, .LBB87_9
.LBB87_7:
	addi r9, r9, 1
	bne r7, r9, .LBB87_2
.LBB87_8:
	add r1, r0, r0
.LBB87_9:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end87:
	.size	f77_strcmp, .Lfunc_end87-f77_strcmp
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
	sgt r3, r6, r3
	sub r3, r0, r3
	and r3, r6, r3
	add r5, r3, r14
	lui r3, 16
	addi r6, r3, 1
	bgeu r5, r6, .LBB88_8
.LBB88_1:
	lui r3, %hi(fio_catpos)
	ldw r7, r3+%lo(fio_catpos)
	add r7, r7, r5
	blt r7, r6, .LBB88_3
.LBB88_2:
	stw r3+%lo(fio_catpos), r0
.LBB88_3:
	ldw r6, r3+%lo(fio_catpos)
	lui r7, %hi(fio_catring)
	addi r7, r7, %lo(fio_catring)
	add r13, r6, r7
	addi r5, r5, 3
	lui r7, 64
	addi r7, r7, -4
	and r5, r5, r7
	add r5, r6, r5
	stw r3+%lo(fio_catpos), r5
	addi r15, r0, 1
	blt r1, r15, .LBB88_5
.LBB88_4:
	add r3, r13, r0
	add r5, r1, r0
	jal r31, memmove
.LBB88_5:
	blt r11, r15, .LBB88_7
.LBB88_6:
	add r3, r13, r14
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memmove
.LBB88_7:
	add r1, r13, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB88_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end88:
	.size	f77_cat, .Lfunc_end88-f77_cat
                                        # -- End function
	.globl	f77_wr_l                        # -- Begin function f77_wr_l
	.p2align	2
	.type	f77_wr_l,@function
f77_wr_l:                               # @f77_wr_l
# %bb.0:
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB89_15
.LBB89_1:
	lui r4, %hi(fio_col)
	ldw r7, r4+%lo(fio_col)
	addi r6, r0, 1022
	lui r1, %hi(fio_len)
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	bgt r7, r6, .LBB89_7
.LBB89_2:
	ldw r9, r1+%lo(fio_len)
	addi r8, r0, 32
	bge r9, r7, .LBB89_5
.LBB89_3:
	addi r10, r9, 1
	add r9, r9, r5
	stb r9+0, r8
	add r9, r10, r0
	bne r7, r10, .LBB89_3
.LBB89_4:
	stw r1+%lo(fio_len), r7
	add r9, r7, r0
.LBB89_5:
	add r10, r7, r5
	stb r10+0, r8
	addi r8, r7, 1
	stw r4+%lo(fio_col), r8
	blt r7, r9, .LBB89_7
.LBB89_6:
	stw r1+%lo(fio_len), r8
.LBB89_7:
	ldw r7, r4+%lo(fio_col)
	bgt r7, r6, .LBB89_14
.LBB89_8:
	ldw r6, r1+%lo(fio_len)
	bge r6, r7, .LBB89_12
.LBB89_9:
	addi r8, r0, 32
.LBB89_10:
	addi r9, r6, 1
	add r6, r6, r5
	stb r6+0, r8
	add r6, r9, r0
	bne r7, r9, .LBB89_10
.LBB89_11:
	stw r1+%lo(fio_len), r7
	add r6, r7, r0
.LBB89_12:
	addi r8, r0, 0
	seq r3, r3, r8
	sub r3, r0, r3
	andi r3, r3, 18
	xori r3, r3, 84
	add r5, r7, r5
	stb r5+0, r3
	addi r3, r7, 1
	stw r4+%lo(fio_col), r3
	blt r7, r6, .LBB89_14
.LBB89_13:
	stw r1+%lo(fio_len), r3
.LBB89_14:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	jal r0, .LBB89_32
.LBB89_15:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB89_17
.LBB89_16:
	ldw r1, r12+%lo(fio_rep)
	bgt r1, r0, .LBB89_18
.LBB89_17:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	jal r31, fio_next_desc
	add r3, r13, r0
	beq r1, r0, .LBB89_31
.LBB89_18:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r4, r0, 2
	blt r1, r4, .LBB89_20
.LBB89_19:
	lui r4, %hi(fio_col)
	ldw r5, r4+%lo(fio_col)
	add r1, r1, r5
	addi r1, r1, -1
	stw r4+%lo(fio_col), r1
.LBB89_20:
	lui r4, %hi(fio_col)
	ldw r1, r4+%lo(fio_col)
	addi r5, r0, 1022
	bgt r1, r5, .LBB89_27
.LBB89_21:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	bge r6, r1, .LBB89_25
.LBB89_22:
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
.LBB89_23:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r1, r9, .LBB89_23
.LBB89_24:
	stw r5+%lo(fio_len), r1
	add r6, r1, r0
.LBB89_25:
	addi r7, r0, 0
	seq r3, r3, r7
	sub r3, r0, r3
	andi r3, r3, 18
	xori r3, r3, 84
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	add r7, r1, r7
	stb r7+0, r3
	addi r3, r1, 1
	stw r4+%lo(fio_col), r3
	blt r1, r6, .LBB89_27
.LBB89_26:
	stw r5+%lo(fio_len), r3
.LBB89_27:
	ldw r1, r12+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB89_29
.LBB89_28:
	addi r1, r1, -1
	stw r12+%lo(fio_rep), r1
.LBB89_29:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB89_31
.LBB89_30:
	stw r11+%lo(fio_desc), r0
.LBB89_31:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB89_32:
	jalr r0, r31, 0
.Lfunc_end89:
	.size	f77_wr_l, .Lfunc_end89-f77_wr_l
                                        # -- End function
	.globl	f77_wr_end                      # -- Begin function f77_wr_end
	.p2align	2
	.type	f77_wr_end,@function
f77_wr_end:                             # @f77_wr_end
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB90_4
.LBB90_1:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB90_3
.LBB90_2:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB90_4
.LBB90_3:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB90_4:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end90:
	.size	f77_wr_end, .Lfunc_end90-f77_wr_end
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
	addi r25, r0, 1
	lui r15, %hi(fio_fmt)
	lui r13, %hi(fio_pos)
	addi r16, r0, 44
	lui r1, 24
	addi r17, r1, 1695
	addi r18, r0, 32
	lui r19, %hi(fio_gdepth)
	lui r20, %hi(fio_draining)
	lui r21, %hi(fio_revert)
	lui r12, %hi(fio_since_data)
	lui r22, %hi(fio_reading)
	addi r23, r0, -33
	addi r24, r0, 83
	addi r1, r0, 35
	stw sp+52, r1
	lui r26, %hi(fio_sp)
	lui r1, %hi(.LJTI91_4)
	addi r1, r1, %lo(.LJTI91_4)
	stw sp+48, r1
	addi r28, r0, 58
	addi r1, r0, 66
	stw sp+44, r1
	addi r1, r0, 11
	stw sp+36, r1
	addi r1, r0, 90
	stw sp+32, r1
	lui r1, %hi(fio_bz)
	stw sp+28, r1
	addi r1, r0, 8
	stw sp+40, r1
	add r14, r25, r0
	jal r0, .LBB91_5
.LBB91_1:
	addi r1, r3, 1
.LBB91_2:
	stw r13+%lo(fio_pos), r1
.LBB91_3:
	bgtu r14, r17, .LBB91_107
.LBB91_4:
	addi r14, r14, 1
.LBB91_5:
	ldw r27, r15+%lo(fio_fmt)
	ldw r3, r13+%lo(fio_pos)
	add r6, r27, r3
	ldb r5, r6+0
	andi r1, r5, 255
	beq r1, r16, .LBB91_1
.LBB91_6:
	beq r1, r18, .LBB91_1
.LBB91_7:
	bne r1, r0, .LBB91_16
.LBB91_8:
	ldw r1, r19+%lo(fio_gdepth)
	blt r1, r25, .LBB91_10
.LBB91_9:
	stw r19+%lo(fio_gdepth), r0
.LBB91_10:
	ldbu r1, r20+%lo(fio_draining)
	bne r1, r0, .LBB91_107
.LBB91_11:
	ldw r3, r21+%lo(fio_revert)
	add r1, r0, r0
	blt r3, r0, .LBB91_108
.LBB91_12:
	ldbu r1, r12+%lo(fio_since_data)
	beq r1, r0, .LBB91_119
.LBB91_13:
	stb r12+%lo(fio_since_data), r0
	stw r13+%lo(fio_pos), r3
	add r1, r27, r3
	ldbu r1, r1+0
	beq r1, r0, .LBB91_107
.LBB91_14:
	ldbu r1, r22+%lo(fio_reading)
	bne r1, r25, .LBB91_88
.LBB91_15:
	jal r31, fio_getrec
	beq r1, r0, .LBB91_3
	jal r0, .LBB91_120
.LBB91_16:
	and r4, r5, r23
	bne r4, r24, .LBB91_20
.LBB91_17:
	addi r1, r3, 1
	stw r13+%lo(fio_pos), r1
	add r1, r27, r1
	ldbu r1, r1+0
	addi r1, r1, -80
	ldw r4, sp+52
	bgtu r1, r4, .LBB91_50
.LBB91_18:
	slli r1, r1, 2
	ldw r4, sp+48
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB91_19:
	stb r26+%lo(fio_sp), r25
	jal r0, .LBB91_28
.LBB91_20:
	bne r5, r28, .LBB91_23
.LBB91_21:
	addi r1, r3, 1
	stw r13+%lo(fio_pos), r1
	ldbu r1, r20+%lo(fio_draining)
	bne r1, r0, .LBB91_107
.LBB91_22:
	add r1, r0, r0
	bleu r14, r17, .LBB91_4
	jal r0, .LBB91_108
.LBB91_23:
	andi r4, r5, 223
	ldw r7, sp+44
	bne r4, r7, .LBB91_29
.LBB91_24:
	ldbu r4, r6+1
	addi r7, r4, -78
	andi r8, r7, 252
	slli r7, r7, 6
	srli r8, r8, 2
	or  r7, r7, r8
	andi r7, r7, 255
	ldw r8, sp+36
	bgtu r7, r8, .LBB91_29
.LBB91_25:
	sll r7, r25, r7
	andi r7, r7, 2313
	beq r7, r0, .LBB91_29
.LBB91_26:
	andi r1, r4, 223
	ldw r4, sp+32
	seq r1, r1, r4
	ldw r4, sp+28
	stw r4+%lo(fio_bz), r1
	jal r0, .LBB91_28
.LBB91_27:
	stb r26+%lo(fio_sp), r0
.LBB91_28:
	addi r1, r3, 2
	jal r0, .LBB91_2
.LBB91_29:
	addi r4, r1, -39
	lui r1, %hi(fio_line)
	addi r1, r1, %lo(fio_line)
	ldw r7, sp+40
	bgtu r4, r7, .LBB91_53
.LBB91_30:
	slli r4, r4, 2
	lui r7, %hi(.LJTI91_0)
	addi r7, r7, %lo(.LJTI91_0)
	add r4, r7, r4
	ldw r7, r4+0
	add r4, r25, r0
	jalr r0, r7, 0
.LBB91_31:
	addi r3, r3, 1
	stw r13+%lo(fio_pos), r3
	lui r4, %hi(fio_col)
	ldw r9, r4+%lo(fio_col)
	lui r5, %hi(fio_len)
	ldw r7, r5+%lo(fio_len)
	ldbu r6, r22+%lo(fio_reading)
	jal r0, .LBB91_35
.LBB91_32:
	addi r10, r0, 1
.LBB91_33:
	add r9, r8, r0
.LBB91_34:
	add r3, r3, r10
	stw r13+%lo(fio_pos), r3
.LBB91_35:
	add r8, r9, r0
	add r9, r27, r3
	ldbu r9, r9+0
	addi r10, r0, 39
	beq r9, r10, .LBB91_42
.LBB91_36:
	beq r9, r0, .LBB91_3
.LBB91_37:
	addi r10, r0, 1022
	sgt r10, r8, r10
	or  r10, r6, r10
	andi r10, r10, 1
	bne r10, r0, .LBB91_32
.LBB91_38:
	bge r7, r8, .LBB91_41
.LBB91_39:
	addi r10, r7, 1
	add r7, r7, r1
	stb r7+0, r18
	add r7, r10, r0
	bne r8, r10, .LBB91_39
.LBB91_40:
	stw r5+%lo(fio_len), r8
	add r7, r8, r0
.LBB91_41:
	add r10, r8, r1
	stb r10+0, r9
	addi r9, r8, 1
	stw r4+%lo(fio_col), r9
	addi r10, r0, 1
	blt r8, r7, .LBB91_34
	jal r0, .LBB91_49
.LBB91_42:
	addi r9, r3, 1
	add lr, r27, r9
	ldbu lr, lr+0
	bne lr, r10, .LBB91_82
.LBB91_43:
	addi r9, r0, 1022
	sgt r9, r8, r9
	or  r9, r6, r9
	andi r9, r9, 1
	beq r9, r0, .LBB91_45
.LBB91_44:
	addi r10, r0, 2
	jal r0, .LBB91_33
.LBB91_45:
	bge r7, r8, .LBB91_48
.LBB91_46:
	addi r9, r7, 1
	add r7, r7, r1
	stb r7+0, r18
	add r7, r9, r0
	bne r8, r9, .LBB91_46
.LBB91_47:
	stw r5+%lo(fio_len), r8
	add r7, r8, r0
.LBB91_48:
	add r9, r8, r1
	stb r9+0, r10
	addi r9, r8, 1
	stw r4+%lo(fio_col), r9
	addi r10, r0, 2
	blt r8, r7, .LBB91_34
.LBB91_49:
	stw r5+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB91_34
.LBB91_50:
	stb r26+%lo(fio_sp), r0
	jal r0, .LBB91_3
.LBB91_51:
	ldb r4, r6+1
	addi r4, r4, -58
	addi r5, r0, -10
	bgeu r4, r5, .LBB91_58
.LBB91_52:
	addi r5, r0, 45
.LBB91_53:
	add r4, r25, r0
	jal r0, .LBB91_59
.LBB91_54:
	ldw r1, r19+%lo(fio_gdepth)
	beq r1, r0, .LBB91_83
.LBB91_55:
	addi r3, r3, 1
	stw r13+%lo(fio_pos), r3
	addi r3, r1, -1
	stw r19+%lo(fio_gdepth), r3
	slli r3, r3, 2
	lui r4, %hi(fio_gcount)
	addi r4, r4, %lo(fio_gcount)
	add r4, r3, r4
	ldw r5, r4+0
	addi r6, r5, -1
	stw r4+0, r6
	addi r4, r0, 2
	blt r5, r4, .LBB91_3
.LBB91_56:
	lui r4, %hi(fio_gstart)
	addi r4, r4, %lo(fio_gstart)
	add r3, r3, r4
	ldw r3, r3+0
	stw r13+%lo(fio_pos), r3
	stw r19+%lo(fio_gdepth), r1
	jal r0, .LBB91_3
.LBB91_57:
	addi r1, r3, 1
	stw r13+%lo(fio_pos), r1
	jal r0, .LBB91_87
.LBB91_58:
	addi r4, r3, 1
	stw r13+%lo(fio_pos), r4
	add r4, r27, r4
	ldb r5, r4+0
	add r4, r0, r0
.LBB91_59:
	addi r5, r5, -58
	addi lr, r0, -10
	add fp, r25, r0
	bltu r5, lr, .LBB91_65
.LBB91_60:
	ldw r5, r13+%lo(fio_pos)
	add r6, r27, r5
	ldb r6, r6+0
	addi r6, r6, -48
	addi r7, r0, 9
	bleu r6, r7, .LBB91_62
.LBB91_61:
	add fp, r0, r0
	jal r0, .LBB91_65
.LBB91_62:
	addi r7, r27, 1
	add fp, r0, r0
.LBB91_63:
	addi r9, r0, 10
	mul r10, fp, r9
	addi r8, r5, 1
	add fp, r10, r6
	add r5, r7, r5
	ldb r5, r5+0
	addi r6, r5, -48
	add r5, r8, r0
	bltu r6, r9, .LBB91_63
.LBB91_64:
	stw r13+%lo(fio_pos), r8
.LBB91_65:
	ldw r5, r13+%lo(fio_pos)
	add r6, r27, r5
	ldbu r11, r6+0
	addi r6, r0, 40
	bne r11, r6, .LBB91_69
.LBB91_66:
	addi r4, r5, 1
	stw r13+%lo(fio_pos), r4
	ldw r1, r19+%lo(fio_gdepth)
	addi r5, r0, 15
	bgt r1, r5, .LBB91_3
.LBB91_67:
	slli r5, r1, 2
	lui r6, %hi(fio_gstart)
	addi r6, r6, %lo(fio_gstart)
	add r6, r5, r6
	stw r6+0, r4
	lui r4, %hi(fio_gcount)
	addi r4, r4, %lo(fio_gcount)
	add r4, r5, r4
	stw r4+0, fp
	addi r4, r1, 1
	stw r19+%lo(fio_gdepth), r4
	bne r1, r0, .LBB91_3
.LBB91_68:
	stw r21+%lo(fio_revert), r3
	jal r0, .LBB91_3
.LBB91_69:
	addi r3, r5, 1
	andi r6, r11, 223
	addi r6, r6, -72
	addi r7, r0, 16
	bgtu r6, r7, .LBB91_101
.LBB91_70:
	slli r6, r6, 2
	lui r7, %hi(.LJTI91_1)
	addi r7, r7, %lo(.LJTI91_1)
	add r6, r7, r6
	ldw r6, r6+0
	jalr r0, r6, 0
.LBB91_71:
	addi r4, r0, 1
	blt fp, r4, .LBB91_97
.LBB91_72:
	lui r5, %hi(fio_len)
	ldw r8, r5+%lo(fio_len)
	lui r6, %hi(fio_col)
	ldw r9, r6+%lo(fio_col)
	ldbu r7, r22+%lo(fio_reading)
	jal r0, .LBB91_75
.LBB91_73:
	add r9, lr, r0
.LBB91_74:
	addi fp, r10, -1
	addi r3, r3, 1
	ble r10, r4, .LBB91_97
.LBB91_75:
	add lr, r27, r3
	ldbu lr, lr+0
	beq lr, r0, .LBB91_97
.LBB91_76:
	add r10, fp, r0
	addi r11, r0, 1022
	sgt r11, r9, r11
	or  r11, r7, r11
	andi r11, r11, 1
	bne r11, r0, .LBB91_74
.LBB91_77:
	bge r8, r9, .LBB91_80
.LBB91_78:
	addi r11, r8, 1
	add r8, r8, r1
	stb r8+0, r18
	add r8, r11, r0
	bne r9, r11, .LBB91_78
.LBB91_79:
	stw r5+%lo(fio_len), r9
	add r8, r9, r0
.LBB91_80:
	add r11, r9, r1
	stb r11+0, lr
	addi lr, r9, 1
	stw r6+%lo(fio_col), lr
	blt r9, r8, .LBB91_73
.LBB91_81:
	stw r5+%lo(fio_len), lr
	add r8, lr, r0
	jal r0, .LBB91_73
.LBB91_82:
	stw r13+%lo(fio_pos), r9
	jal r0, .LBB91_3
.LBB91_83:
	ldbu r1, r20+%lo(fio_draining)
	bne r1, r0, .LBB91_107
.LBB91_84:
	ldw r3, r21+%lo(fio_revert)
	add r1, r0, r0
	blt r3, r0, .LBB91_108
.LBB91_85:
	ldbu r1, r12+%lo(fio_since_data)
	beq r1, r0, .LBB91_119
.LBB91_86:
	stb r12+%lo(fio_since_data), r0
	stw r13+%lo(fio_pos), r3
	add r1, r27, r3
	ldbu r1, r1+0
	beq r1, r0, .LBB91_107
.LBB91_87:
	ldbu r1, r22+%lo(fio_reading)
	addi r3, r0, 1
	beq r1, r3, .LBB91_15
.LBB91_88:
	jal r31, fio_flush_line
	jal r0, .LBB91_3
.LBB91_89:
	stw r13+%lo(fio_pos), r3
	add r1, r27, r3
	ldbu r1, r1+0
	add fp, r0, r0
	addi r11, r0, 1
	addi r3, r1, -76
	addi r1, r0, 38
	bgtu r3, r1, .LBB91_106
.LBB91_90:
	addi r1, r0, -1
	slli r3, r3, 2
	lui r4, %hi(.LJTI91_2)
	addi r4, r4, %lo(.LJTI91_2)
	add r3, r4, r3
	ldw r3, r3+0
	add r6, r11, r0
	jalr r0, r3, 0
.LBB91_91:
	addi r1, r0, 1
.LBB91_92:
	addi r3, r5, 2
	stw r13+%lo(fio_pos), r3
	add r6, r0, r0
	add fp, r1, r0
.LBB91_93:
	ldw r1, r13+%lo(fio_pos)
	add r1, r27, r1
	ldb r1, r1+0
	addi r3, r1, -58
	add r1, r11, r0
	bltu r3, lr, .LBB91_95
.LBB91_94:
	add r27, r6, r0
	jal r31, fio_number
	add r6, r27, r0
.LBB91_95:
	ldbu r4, r22+%lo(fio_reading)
	mul r3, r1, fp
	addi r1, r1, -1
	bne r4, r11, .LBB91_103
.LBB91_96:
	lui r4, %hi(fio_rpos)
	ldw r5, r4+%lo(fio_rpos)
	add r3, r5, r3
	xor r1, r1, r3
	sub r5, r0, r6
	and r1, r1, r5
	xor r1, r3, r1
	addi r3, r0, 0
	sgt r3, r1, r3
	sub r3, r0, r3
	and r1, r1, r3
	stw r4+%lo(fio_rpos), r1
	jal r0, .LBB91_3
.LBB91_97:
	stw r13+%lo(fio_pos), r3
	jal r0, .LBB91_3
.LBB91_98:
	stw r13+%lo(fio_pos), r3
	sub r1, r0, fp
	xor r3, fp, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	lui r3, %hi(fio_scale)
	stw r3+%lo(fio_scale), r1
	jal r0, .LBB91_3
.LBB91_99:
	stw r13+%lo(fio_pos), r3
	ldbu r3, r22+%lo(fio_reading)
	addi r1, r0, 1
	bne r3, r1, .LBB91_104
.LBB91_100:
	lui r1, %hi(fio_rpos)
	ldw r3, r1+%lo(fio_rpos)
	add r3, r3, fp
	stw r1+%lo(fio_rpos), r3
	jal r0, .LBB91_3
.LBB91_101:
	stw r13+%lo(fio_pos), r3
	addi r1, r11, -65
	addi r4, r0, 43
	bgtu r1, r4, .LBB91_3
.LBB91_102:
	slli r1, r1, 2
	lui r4, %hi(.LJTI91_3)
	addi r4, r4, %lo(.LJTI91_3)
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB91_103:
	lui r4, %hi(fio_col)
	ldw r5, r4+%lo(fio_col)
	add r3, r5, r3
	xor r1, r1, r3
	sub r5, r0, r6
	and r1, r1, r5
	xor r1, r3, r1
	addi r3, r0, 0
	sgt r3, r1, r3
	sub r3, r0, r3
	and r1, r1, r3
	stw r4+%lo(fio_col), r1
	jal r0, .LBB91_3
.LBB91_104:
	blt fp, r1, .LBB91_3
.LBB91_105:
	lui r1, %hi(fio_col)
	ldw r3, r1+%lo(fio_col)
	add r3, r3, fp
	stw r1+%lo(fio_col), r3
	jal r0, .LBB91_3
.LBB91_106:
	add r6, r11, r0
	jal r0, .LBB91_93
.LBB91_107:
	add r1, r0, r0
.LBB91_108:
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
.LBB91_109:
	add r1, r27, r3
	ldb r1, r1+0
	addi r1, r1, -58
	bgeu r1, lr, .LBB91_111
.LBB91_110:
	add r1, r0, r0
	jal r0, .LBB91_112
.LBB91_111:
	add r14, lr, r0
	jal r31, fio_number
	add lr, r14, r0
.LBB91_112:
	lui r3, %hi(fio_w)
	stw r3+%lo(fio_w), r1
	lui r14, %hi(fio_d)
	stw r14+%lo(fio_d), r0
	ldw r1, r13+%lo(fio_pos)
	add r3, r27, r1
	ldbu r3, r3+0
	addi r4, r0, 46
	bne r3, r4, .LBB91_114
.LBB91_113:
	addi r1, r1, 1
	stw r13+%lo(fio_pos), r1
	add r15, lr, r0
	jal r31, fio_number
	add lr, r15, r0
	stw r14+%lo(fio_d), r1
.LBB91_114:
	lui r14, %hi(fio_e)
	stw r14+%lo(fio_e), r0
	ldw r1, r13+%lo(fio_pos)
	add r3, r27, r1
	ldbu r3, r3+0
	ori  r3, r3, 32
	addi r4, r0, 101
	bne r3, r4, .LBB91_118
.LBB91_115:
	addi r3, r1, 1
	stw r13+%lo(fio_pos), r3
	add r3, r27, r3
	ldb r3, r3+0
	addi r3, r3, -58
	bgeu r3, lr, .LBB91_117
.LBB91_116:
	stw r13+%lo(fio_pos), r1
	jal r0, .LBB91_118
.LBB91_117:
	jal r31, fio_number
	stw r14+%lo(fio_e), r1
.LBB91_118:
	lui r1, %hi(fio_rep)
	stw r1+%lo(fio_rep), fp
	add r3, r11, r0
	jal r31, toupper
	lui r3, %hi(fio_desc)
	stw r3+%lo(fio_desc), r1
	addi r3, r0, 1
	stb r12+%lo(fio_since_data), r3
	jal r0, .LBB91_108
.LBB91_119:
	jal r31, fio_flush_line
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.86)
	addi r4, r4, %lo(.L.str.86)
	jal r0, .LBB91_121
.LBB91_120:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
.LBB91_121:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end91:
	.size	fio_next_desc, .Lfunc_end91-fio_next_desc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI91_0,@object
.LJTI91_0:
	.word	.LBB91_31
	.word	.LBB91_59
	.word	.LBB91_54
	.word	.LBB91_59
	.word	.LBB91_59
	.word	.LBB91_59
	.word	.LBB91_51
	.word	.LBB91_59
	.word	.LBB91_57
	.size	.LJTI91_0, 36
	.type	.LJTI91_1,@object
.LJTI91_1:
	.word	.LBB91_71
	.word	.LBB91_101
	.word	.LBB91_101
	.word	.LBB91_101
	.word	.LBB91_101
	.word	.LBB91_101
	.word	.LBB91_101
	.word	.LBB91_101
	.word	.LBB91_98
	.word	.LBB91_101
	.word	.LBB91_101
	.word	.LBB91_101
	.word	.LBB91_89
	.word	.LBB91_101
	.word	.LBB91_101
	.word	.LBB91_101
	.word	.LBB91_99
	.size	.LJTI91_1, 68
	.type	.LJTI91_2,@object
.LJTI91_2:
	.word	.LBB91_92
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_91
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_92
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_93
	.word	.LBB91_91
	.size	.LJTI91_2, 156
	.type	.LJTI91_3,@object
.LJTI91_3:
	.word	.LBB91_109
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_109
	.word	.LBB91_109
	.word	.LBB91_109
	.word	.LBB91_109
	.word	.LBB91_3
	.word	.LBB91_109
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_109
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_109
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_109
	.word	.LBB91_109
	.word	.LBB91_109
	.word	.LBB91_109
	.word	.LBB91_3
	.word	.LBB91_109
	.word	.LBB91_3
	.word	.LBB91_3
	.word	.LBB91_109
	.size	.LJTI91_3, 176
	.type	.LJTI91_4,@object
.LJTI91_4:
	.word	.LBB91_19
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_27
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_19
	.word	.LBB91_50
	.word	.LBB91_50
	.word	.LBB91_27
	.size	.LJTI91_4, 144
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function fio_flush_line
	.type	fio_flush_line,@function
fio_flush_line:                         # @fio_flush_line
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	lui r1, %hi(fio_out)
	ldw r1, r1+%lo(fio_out)
	lui r3, %hi(stdout)
	ldw r4, r3+%lo(stdout)
	lui r13, %hi(fio_len)
	ldw r5, r13+%lo(fio_len)
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	add r3, r5, r3
	stb r3+0, r0
	lui r3, %hi(fio_col)
	stw r3+%lo(fio_col), r0
	lui r12, %hi(fio_ibuf)
	ldw r3, r12+%lo(fio_ibuf)
	beq r3, r0, .LBB92_9
.LBB92_1:
	lui r11, %hi(fio_irec)
	ldw r1, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r1, r4, .LBB92_18
.LBB92_2:
	lui r14, %hi(fio_ilen)
	ldw r6, r14+%lo(fio_ilen)
	bgt r5, r6, .LBB92_19
.LBB92_3:
	mul r1, r6, r1
	add r3, r3, r1
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	jal r31, memcpy
	ldw r3, r13+%lo(fio_len)
	ldw r1, r14+%lo(fio_ilen)
	ble r1, r3, .LBB92_6
.LBB92_4:
	ldw r4, r12+%lo(fio_ibuf)
	ldw r5, r11+%lo(fio_irec)
	mul r5, r5, r1
	sub r1, r1, r3
	add r3, r3, r5
	add r3, r4, r3
	addi r4, r0, 32
.LBB92_5:
	stb r3+0, r4
	addi r1, r1, -1
	addi r3, r3, 1
	bne r1, r0, .LBB92_5
.LBB92_6:
	ldw r1, r11+%lo(fio_irec)
	addi r1, r1, 1
	stw r11+%lo(fio_irec), r1
.LBB92_7:
	stw r13+%lo(fio_len), r0
.LBB92_8:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB92_9:
	addi r3, r0, 0
	seq r3, r1, r3
	xor r4, r4, r1
	sub r3, r0, r3
	and r3, r4, r3
	xor r11, r1, r3
	lui r12, %hi(fio_unit)
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	addi r14, r0, 98
	bgtu r3, r14, .LBB92_15
.LBB92_10:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB92_15
.LBB92_11:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r12, r1+0
	bgt r5, r12, .LBB92_21
.LBB92_12:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	ldw r1, r13+%lo(fio_len)
	ble r12, r1, .LBB92_7
.LBB92_13:
	sub r14, r12, r1
	addi r12, r0, 32
.LBB92_14:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fputc
	addi r14, r14, -1
	bne r14, r0, .LBB92_14
	jal r0, .LBB92_7
.LBB92_15:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	addi r3, r0, 10
	add r4, r11, r0
	jal r31, fputc
	stw r13+%lo(fio_len), r0
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	bgtu r3, r14, .LBB92_8
.LBB92_16:
	addi r1, r1, -7
	addi r3, r0, -3
	bgtu r1, r3, .LBB92_8
.LBB92_17:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
	jal r0, .LBB92_8
.LBB92_18:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.87)
	addi r4, r4, %lo(.L.str.87)
	jal r0, .LBB92_20
.LBB92_19:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.88)
	addi r4, r4, %lo(.L.str.88)
.LBB92_20:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB92_21:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.89)
	addi r4, r4, %lo(.L.str.89)
	add r6, r12, r0
	jal r0, .LBB92_20
.Lfunc_end92:
	.size	fio_flush_line, .Lfunc_end92-fio_flush_line
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
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB93_4
.LBB93_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB93_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB93_2
.LBB93_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB93_4:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_resolve
	add r13, r1, r0
	lui r1, %hi(stdout)
	ldw r1, r1+%lo(stdout)
	beq r13, r1, .LBB93_8
.LBB93_5:
	lui r1, %hi(stderr)
	ldw r1, r1+%lo(stderr)
	beq r13, r1, .LBB93_8
.LBB93_6:
	add r3, r13, r0
	jal r31, fflush
	addi r1, r11, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB93_8
.LBB93_7:
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
.LBB93_8:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end93:
	.size	f77_endfile, .Lfunc_end93-f77_endfile
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
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r5, r11, r0
	add r6, r12, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fprintf
.Lfunc_end94:
	.size	f77_pause, .Lfunc_end94-f77_pause
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
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	add r14, r3, r0
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB95_4
.LBB95_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB95_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB95_2
.LBB95_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB95_4:
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	add r3, r14, r0
	add r4, r0, r0
	jal r31, fio_resolve
	lui r3, %hi(fio_in)
	stw r3+%lo(fio_in), r1
	lui r3, %hi(fio_unit)
	stw r3+%lo(fio_unit), r14
	addi r3, r14, -1
	addi r4, r0, 98
	bgtu r3, r4, .LBB95_10
.LBB95_5:
	slli r15, r14, 2
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r15, r3
	stw r3+0, r0
	lui r4, %hi(fio_rec_next)
	ldw r3, r4+%lo(fio_rec_next)
	stw r4+%lo(fio_rec_next), r0
	addi r4, r14, -100
	addi r5, r0, -99
	bltu r4, r5, .LBB95_9
.LBB95_6:
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r15, r4
	ldw r4, r4+0
	beq r4, r0, .LBB95_9
.LBB95_7:
	ble r3, r0, .LBB95_16
.LBB95_8:
	addi r3, r3, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r4, r15, r4
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB95_9:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r15, r1
	ldw r1, r1+0
	jal r0, .LBB95_15
.LBB95_10:
	lui r4, %hi(fio_rec_next)
	ldw r3, r4+%lo(fio_rec_next)
	stw r4+%lo(fio_rec_next), r0
	addi r4, r14, -100
	addi r5, r0, -99
	bltu r4, r5, .LBB95_14
.LBB95_11:
	slli r4, r14, 2
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r4, r5
	ldw r5, r5+0
	beq r5, r0, .LBB95_14
.LBB95_12:
	ble r3, r0, .LBB95_16
.LBB95_13:
	addi r3, r3, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r4, r4, r5
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB95_14:
	add r1, r0, r0
.LBB95_15:
	lui r3, %hi(fio_bz)
	stw r3+%lo(fio_bz), r1
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, fio_rd_setup
.LBB95_16:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end95:
	.size	f77_rd_begin, .Lfunc_end95-f77_rd_begin
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
	stw sp+24, r16
	stw sp+20, r17
	stw sp+16, r18
	add r11, r5, r0
	lui r1, %hi(fio_unit)
	ldw r1, r1+%lo(fio_unit)
	bgt r1, r0, .LBB96_2
.LBB96_1:
	lui r1, %hi(fio_bz)
	stw r1+%lo(fio_bz), r0
.LBB96_2:
	lui r1, %hi(fio_l_nulls)
	stw r1+%lo(fio_l_nulls), r0
	lui r1, %hi(fio_l_after)
	stb r1+%lo(fio_l_after), r0
	lui r13, %hi(fio_reading)
	addi r12, r0, 1
	stb r13+%lo(fio_reading), r12
	lui r1, %hi(fio_ldone)
	stb r1+%lo(fio_ldone), r0
	lui r1, %hi(fio_l_pending)
	stw r1+%lo(fio_l_pending), r0
	lui r1, %hi(fio_gdepth)
	stw r1+%lo(fio_gdepth), r0
	lui r1, %hi(fio_scale)
	stw r1+%lo(fio_scale), r0
	lui r1, %hi(fio_rep)
	stw r1+%lo(fio_rep), r0
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
	lui r1, %hi(fio_len)
	stw r1+%lo(fio_len), r0
	addi r1, r0, 0
	seq r1, r3, r1
	lui r17, %hi(fio_listed)
	stw r17+%lo(fio_listed), r1
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
	lui r16, %hi(fio_fmt)
	stw r16+%lo(fio_fmt), r1
	lui r14, %hi(fio_pos)
	stw r14+%lo(fio_pos), r0
	lui r15, %hi(fio_revert)
	addi r18, r0, -1
	stw r15+%lo(fio_revert), r18
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	beq r3, r0, .LBB96_4
.LBB96_3:
	jal r31, fio_fmt_use
	ble r1, r18, .LBB96_14
.LBB96_4:
	ldw r1, r17+%lo(fio_listed)
	bne r1, r0, .LBB96_10
.LBB96_5:
	ldw r3, r16+%lo(fio_fmt)
	ldw r1, r14+%lo(fio_pos)
	addi r4, r0, 32
.LBB96_6:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB96_8
.LBB96_7:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB96_6
.LBB96_8:
	addi r3, r0, 40
	bne r5, r3, .LBB96_10
.LBB96_9:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	stw r15+%lo(fio_revert), r1
.LBB96_10:
	jal r31, fio_getrec
	beq r1, r0, .LBB96_13
.LBB96_11:
	stb r13+%lo(fio_reading), r0
	andi r1, r11, 3
	bne r1, r0, .LBB96_16
.LBB96_12:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB96_13:
	add r12, r0, r0
	jal r0, .LBB96_16
.LBB96_14:
	stb r13+%lo(fio_reading), r0
	andi r1, r11, 2
	beq r1, r0, .LBB96_17
.LBB96_15:
	addi r12, r0, 2
.LBB96_16:
	add r1, r12, r0
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
.LBB96_17:
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end96:
	.size	fio_rd_setup, .Lfunc_end96-fio_rd_setup
                                        # -- End function
	.globl	f77_rd_begin_int                # -- Begin function f77_rd_begin_int
	.p2align	2
	.type	f77_rd_begin_int,@function
f77_rd_begin_int:                       # @f77_rd_begin_int
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	lui r1, %hi(fio_uend_init)
	ldbu r9, r1+%lo(fio_uend_init)
	bne r9, r0, .LBB97_4
.LBB97_1:
	addi r9, r0, -400
	lui r10, %hi(fio_uend+400)
	addi r10, r10, %lo(fio_uend+400)
	addi lr, r0, -1
.LBB97_2:
	add r11, r9, r10
	stw r11+0, lr
	addi r9, r9, 4
	bne r9, r0, .LBB97_2
.LBB97_3:
	addi r9, r0, 1
	stb r1+%lo(fio_uend_init), r9
.LBB97_4:
	lui r1, %hi(stdin)
	ldw r1, r1+%lo(stdin)
	lui r9, %hi(fio_in)
	stw r9+%lo(fio_in), r1
	lui r1, %hi(fio_unit)
	addi r9, r0, -1
	stw r1+%lo(fio_unit), r9
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r3
	lui r1, %hi(fio_ilen)
	stw r1+%lo(fio_ilen), r4
	lui r1, %hi(fio_inrec)
	stw r1+%lo(fio_inrec), r5
	lui r1, %hi(fio_irec)
	stw r1+%lo(fio_irec), r0
	add r3, r6, r0
	add r4, r7, r0
	add r5, r8, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fio_rd_setup
.Lfunc_end97:
	.size	f77_rd_begin_int, .Lfunc_end97-f77_rd_begin_int
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
	stw sp+1048, r20
	stw sp+1044, r21
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB98_5
.LBB98_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB98_60
.LBB98_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB98_60
.LBB98_3:
	addi r3, r0, 1
	bne r1, r3, .LBB98_27
.LBB98_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB98_60
.LBB98_5:
	lui r12, %hi(fio_desc)
	ldw r1, r12+%lo(fio_desc)
	lui r13, %hi(fio_rep)
	beq r1, r0, .LBB98_11
.LBB98_6:
	ldw r3, r13+%lo(fio_rep)
	ble r3, r0, .LBB98_11
.LBB98_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB98_12
.LBB98_8:
	beq r1, r0, .LBB98_60
.LBB98_9:
	addi r3, r0, 65
	bne r1, r3, .LBB98_30
.LBB98_10:
	addi r4, r0, 4
	add r3, r11, r0
	ldw r21, sp+1044
	ldw r20, sp+1048
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
	jal r0, f77_rd_s
.LBB98_11:
	stw r12+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB98_8
.LBB98_12:
	addi r3, r0, 73
	beq r1, r3, .LBB98_31
.LBB98_13:
	addi r3, r0, 76
	bne r1, r3, .LBB98_30
.LBB98_14:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	addi r1, r0, 1
	lui r4, %hi(fio_rpos)
	blt r3, r1, .LBB98_44
.LBB98_15:
	ldw r5, r4+%lo(fio_rpos)
	lui r6, %hi(fio_rlen)
	ldw r6, r6+%lo(fio_rlen)
	lui r7, %hi(fio_bz)
	ldw r7, r7+%lo(fio_bz)
	addi r8, r0, 32
	lui r9, %hi(fio_rec)
	addi r9, r9, %lo(fio_rec)
	addi r10, r0, 48
	addi lr, r0, 1023
	addi r14, r0, 1
	addi r15, r0, 9
	addi r16, r0, 1022
	add r18, r3, r0
	add r19, r0, r0
	add r17, r0, r0
	jal r0, .LBB98_19
.LBB98_16:
	addi r19, r17, 1
	addi r21, sp, 20
	add r17, r21, r17
	stb r17+0, r20
	add r17, r19, r0
.LBB98_17:
	add r19, r14, r0
.LBB98_18:
	addi r18, r18, -1
	addi r5, r5, 1
	beq r18, r0, .LBB98_45
.LBB98_19:
	add r20, r8, r0
	bge r5, r6, .LBB98_21
.LBB98_20:
	add r20, r5, r9
	ldb r20, r20+0
.LBB98_21:
	beq r20, r8, .LBB98_23
.LBB98_22:
	bne r20, r15, .LBB98_26
.LBB98_23:
	beq r7, r0, .LBB98_18
.LBB98_24:
	beq r19, r0, .LBB98_18
.LBB98_25:
	add r20, r10, r0
	blt r17, lr, .LBB98_16
	jal r0, .LBB98_18
.LBB98_26:
	bgt r17, r16, .LBB98_17
	jal r0, .LBB98_16
.LBB98_27:
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
	bgtu r4, r5, .LBB98_61
.LBB98_28:
	slli r4, r4, 2
	lui r5, %hi(.LJTI98_0)
	addi r5, r5, %lo(.LJTI98_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB98_29:
	andi r3, r3, 255
	addi r4, r0, 46
	seq r3, r3, r4
	add r1, r3, r1
	ldbu r1, r1+0
	andi r1, r1, 223
	addi r3, r0, 84
	seq r1, r1, r3
	stw r11+0, r1
	jal r0, .LBB98_60
.LBB98_30:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.w.d r1, r4
	jal r0, .LBB98_56
.LBB98_31:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r3, r0, 1
	blt r1, r3, .LBB98_52
.LBB98_32:
	lui r4, %hi(fio_rpos)
	ldw r4, r4+%lo(fio_rpos)
	lui r5, %hi(fio_rlen)
	ldw r5, r5+%lo(fio_rlen)
	lui r6, %hi(fio_bz)
	ldw r6, r6+%lo(fio_bz)
	addi r7, r0, 32
	lui r8, %hi(fio_rec)
	addi r8, r8, %lo(fio_rec)
	addi r9, r0, 48
	addi r10, r0, 1023
	addi lr, r0, 9
	addi r14, r0, 1022
	add r16, r1, r0
	add r17, r0, r0
	add r15, r0, r0
	jal r0, .LBB98_36
.LBB98_33:
	addi r17, r15, 1
	addi r19, sp, 20
	add r15, r19, r15
	stb r15+0, r18
	add r15, r17, r0
.LBB98_34:
	add r17, r3, r0
.LBB98_35:
	addi r16, r16, -1
	addi r4, r4, 1
	beq r16, r0, .LBB98_53
.LBB98_36:
	add r18, r7, r0
	bge r4, r5, .LBB98_38
.LBB98_37:
	add r18, r4, r8
	ldb r18, r18+0
.LBB98_38:
	beq r18, r7, .LBB98_40
.LBB98_39:
	bne r18, lr, .LBB98_43
.LBB98_40:
	beq r6, r0, .LBB98_35
.LBB98_41:
	beq r17, r0, .LBB98_35
.LBB98_42:
	add r18, r9, r0
	blt r15, r10, .LBB98_33
	jal r0, .LBB98_35
.LBB98_43:
	bgt r15, r14, .LBB98_34
	jal r0, .LBB98_33
.LBB98_44:
	add r17, r0, r0
.LBB98_45:
	addi r5, sp, 20
	add r6, r5, r17
	stb r6+0, r0
	ldw r6, r4+%lo(fio_rpos)
	add r3, r6, r3
	stw r4+%lo(fio_rpos), r3
	ldbu r3, sp+20
	addi r4, r0, 46
	seq r3, r3, r4
	or  r3, r5, r3
	ldbu r3, r3+0
	addi r4, r0, 83
	ble r3, r4, .LBB98_49
.LBB98_46:
	addi r4, r0, 84
	beq r3, r4, .LBB98_56
.LBB98_47:
	addi r4, r0, 116
	beq r3, r4, .LBB98_56
.LBB98_48:
	addi r1, r0, 102
	beq r3, r1, .LBB98_55
	jal r0, .LBB98_51
.LBB98_49:
	beq r3, r0, .LBB98_55
.LBB98_50:
	addi r1, r0, 70
	beq r3, r1, .LBB98_55
.LBB98_51:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.91)
	addi r4, r4, %lo(.L.str.91)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB98_52:
	add r15, r0, r0
.LBB98_53:
	addi r3, sp, 20
	add r3, r3, r15
	stb r3+0, r0
	lui r3, %hi(fio_rpos)
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldbu r1, sp+20
	beq r1, r0, .LBB98_55
.LBB98_54:
	addi r3, sp, 20
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	jal r0, .LBB98_56
.LBB98_55:
	add r1, r0, r0
.LBB98_56:
	stw r11+0, r1
	ldw r1, r13+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB98_58
.LBB98_57:
	addi r1, r1, -1
	stw r13+%lo(fio_rep), r1
.LBB98_58:
	ldw r1, r13+%lo(fio_rep)
	bne r1, r0, .LBB98_60
.LBB98_59:
	stw r12+%lo(fio_desc), r0
.LBB98_60:
	ldw r21, sp+1044
	ldw r20, sp+1048
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
.LBB98_61:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	stw r11+0, r1
	jal r0, .LBB98_60
.Lfunc_end98:
	.size	f77_rd_i, .Lfunc_end98-f77_rd_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI98_0,@object
.LJTI98_0:
	.word	.LBB98_29
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_29
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_29
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_29
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_61
	.word	.LBB98_29
	.size	.LJTI98_0, 144
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
	lui r13, %hi(fio_l_nulls)
	ldw r1, r13+%lo(fio_l_nulls)
	addi r20, r0, 1
	blt r1, r20, .LBB99_2
.LBB99_1:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
	addi r11, r0, 2
	jal r0, .LBB99_5
.LBB99_2:
	lui r14, %hi(fio_l_pending)
	ldw r1, r14+%lo(fio_l_pending)
	ble r1, r0, .LBB99_6
.LBB99_3:
	addi r1, r1, -1
	stw r14+%lo(fio_l_pending), r1
.LBB99_4:
	add r11, r0, r0
.LBB99_5:
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
.LBB99_6:
	lui r15, %hi(fio_rpos)
	lui r17, %hi(fio_rlen)
	lui r16, %hi(fio_rec)
	addi r16, r16, %lo(fio_rec)
	addi r18, r0, 38
	lui r21, %hi(.LJTI99_0)
	addi r21, r21, %lo(.LJTI99_0)
	lui r12, %hi(fio_l_after)
	addi r11, r0, 2
	jal r0, .LBB99_8
.LBB99_7:
	jal r31, fio_getrec
	bne r1, r0, .LBB99_44
.LBB99_8:
	ldw r3, r15+%lo(fio_rpos)
	ldw r1, r17+%lo(fio_rlen)
	bge r3, r1, .LBB99_7
.LBB99_9:
	add r4, r3, r16
	ldbu r19, r4+0
	addi r4, r19, -9
	bgtu r4, r18, .LBB99_25
.LBB99_10:
	slli r4, r4, 2
	add r4, r21, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB99_11:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	jal r0, .LBB99_8
.LBB99_12:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	ldbu r1, r12+%lo(fio_l_after)
	bne r1, r20, .LBB99_5
.LBB99_13:
	stb r12+%lo(fio_l_after), r0
	jal r0, .LBB99_8
.LBB99_14:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	addi r13, r0, 126
	lui r11, %hi(fio_ltok)
	addi r11, r11, %lo(fio_ltok)
	add r14, r0, r0
	jal r0, .LBB99_16
.LBB99_15:
	jal r31, fio_getrec
	bne r1, r0, .LBB99_44
.LBB99_16:
	ldw r1, r15+%lo(fio_rpos)
	ldw r4, r17+%lo(fio_rlen)
	bge r1, r4, .LBB99_15
.LBB99_17:
	addi r5, r1, 1
	stw r15+%lo(fio_rpos), r5
	add r3, r1, r16
	ldbu r3, r3+0
	bne r3, r19, .LBB99_21
.LBB99_18:
	bge r5, r4, .LBB99_23
.LBB99_19:
	add r4, r5, r16
	ldbu r4, r4+0
	bne r4, r19, .LBB99_23
.LBB99_20:
	addi r1, r1, 2
	stw r15+%lo(fio_rpos), r1
.LBB99_21:
	bgtu r14, r13, .LBB99_16
.LBB99_22:
	addi r1, r14, 1
	add r4, r14, r11
	stb r4+0, r3
	add r14, r1, r0
	jal r0, .LBB99_16
.LBB99_23:
	add r1, r14, r11
	stb r1+0, r0
	addi r1, r0, 1
	stb r12+%lo(fio_l_after), r1
	jal r0, .LBB99_4
.LBB99_24:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	add r11, r20, r0
	jal r0, .LBB99_5
.LBB99_25:
	ldw r3, r15+%lo(fio_rpos)
	bge r3, r1, .LBB99_32
.LBB99_26:
	addi r5, r0, 126
	lui r6, %hi(fio_ltok)
	addi r6, r6, %lo(fio_ltok)
	lui r7, %hi(.LJTI99_1)
	addi r7, r7, %lo(.LJTI99_1)
	add r4, r0, r0
	jal r0, .LBB99_28
.LBB99_27:
	addi r3, r3, 1
	stw r15+%lo(fio_rpos), r3
	beq r1, r3, .LBB99_33
.LBB99_28:
	add r8, r3, r16
	ldbu r8, r8+0
	addi r9, r8, -9
	bgtu r9, r18, .LBB99_30
.LBB99_29:
	slli r9, r9, 2
	add r9, r7, r9
	ldw r9, r9+0
	jalr r0, r9, 0
.LBB99_30:
	bgt r4, r5, .LBB99_27
.LBB99_31:
	addi r9, r4, 1
	add r4, r4, r6
	stb r4+0, r8
	add r4, r9, r0
	jal r0, .LBB99_27
.LBB99_32:
	add r4, r0, r0
.LBB99_33:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	add r3, r4, r1
	stb r3+0, r0
	addi r3, r0, 1
	stb r12+%lo(fio_l_after), r3
	addi r4, r0, -11
	add r15, r0, r0
.LBB99_34:
	add r5, r15, r1
	ldb r5, r5+0
	addi r6, r5, -58
	addi r15, r15, 1
	bgtu r6, r4, .LBB99_34
.LBB99_35:
	beq r15, r3, .LBB99_4
.LBB99_36:
	addi r1, r0, 42
	bne r5, r1, .LBB99_4
.LBB99_37:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	addi r5, r0, 10
	add r3, r12, r0
	add r4, r0, r0
	jal r31, strtol
	beq r1, r0, .LBB99_46
.LBB99_38:
	add r12, r15, r12
	ldbu r15, r12+0
	beq r15, r0, .LBB99_42
.LBB99_39:
	addi r3, r0, 2
	blt r1, r3, .LBB99_41
.LBB99_40:
	addi r1, r1, -1
	stw r14+%lo(fio_l_pending), r1
.LBB99_41:
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	add r4, r12, r0
	jal r31, memmove
	jal r0, .LBB99_43
.LBB99_42:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
.LBB99_43:
	bne r15, r0, .LBB99_4
	jal r0, .LBB99_5
.LBB99_44:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
.LBB99_45:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB99_46:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.90)
	addi r4, r4, %lo(.L.str.90)
	jal r0, .LBB99_45
.Lfunc_end99:
	.size	fio_list_tok, .Lfunc_end99-fio_list_tok
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI99_0,@object
.LJTI99_0:
	.word	.LBB99_11
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_11
	.word	.LBB99_25
	.word	.LBB99_14
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_14
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_12
	.word	.LBB99_25
	.word	.LBB99_25
	.word	.LBB99_24
	.size	.LJTI99_0, 156
	.type	.LJTI99_1,@object
.LJTI99_1:
	.word	.LBB99_33
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_33
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_33
	.word	.LBB99_30
	.word	.LBB99_30
	.word	.LBB99_33
	.size	.LJTI99_1, 156
                                        # -- End function
	.text
	.globl	f77_rd_s                        # -- Begin function f77_rd_s
	.p2align	2
	.type	f77_rd_s,@function
f77_rd_s:                               # @f77_rd_s
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r12, r4, r0
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB100_5
.LBB100_1:
	lui r13, %hi(fio_ldone)
	ldbu r1, r13+%lo(fio_ldone)
	bne r1, r0, .LBB100_31
.LBB100_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB100_31
.LBB100_3:
	addi r14, r0, 1
	bne r1, r14, .LBB100_17
.LBB100_4:
	stb r13+%lo(fio_ldone), r14
	jal r0, .LBB100_31
.LBB100_5:
	lui r13, %hi(fio_desc)
	ldw r1, r13+%lo(fio_desc)
	lui r14, %hi(fio_rep)
	beq r1, r0, .LBB100_8
.LBB100_6:
	ldw r3, r14+%lo(fio_rep)
	ble r3, r0, .LBB100_8
.LBB100_7:
	bne r1, r0, .LBB100_9
	jal r0, .LBB100_31
.LBB100_8:
	stw r13+%lo(fio_desc), r0
	jal r31, fio_next_desc
	beq r1, r0, .LBB100_31
.LBB100_9:
	addi r3, r0, 65
	bne r1, r3, .LBB100_32
.LBB100_10:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r3, r0, 0
	sgt r3, r1, r3
	xor r1, r1, r12
	sub r3, r0, r3
	and r1, r1, r3
	xor r1, r12, r1
	lui r3, %hi(fio_rpos)
	addi r4, r0, 1
	bge r1, r12, .LBB100_22
.LBB100_11:
	blt r12, r4, .LBB100_27
.LBB100_12:
	ldw r4, r3+%lo(fio_rpos)
	lui r5, %hi(fio_rlen)
	ldw r5, r5+%lo(fio_rlen)
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	add r8, r0, r0
	jal r0, .LBB100_14
.LBB100_13:
	add r10, r11, r8
	stb r10+0, r9
	addi r8, r8, 1
	beq r12, r8, .LBB100_27
.LBB100_14:
	add r9, r6, r0
	bge r8, r1, .LBB100_13
.LBB100_15:
	add r10, r4, r8
	add r9, r6, r0
	bge r10, r5, .LBB100_13
.LBB100_16:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB100_13
.LBB100_17:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB100_31
.LBB100_18:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB100_20
.LBB100_19:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB100_31
.LBB100_20:
	add r5, r3, r0
	bge r4, r1, .LBB100_19
.LBB100_21:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB100_19
.LBB100_22:
	blt r12, r4, .LBB100_27
.LBB100_23:
	ldw r5, r3+%lo(fio_rpos)
	lui r4, %hi(fio_rlen)
	ldw r4, r4+%lo(fio_rlen)
	sub r8, r0, r12
	add r5, r1, r5
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	jal r0, .LBB100_25
.LBB100_24:
	stb r11+0, r9
	addi r9, r8, 1
	sltu r10, r9, r8
	addi r11, r11, 1
	add r8, r9, r0
	bne r10, r0, .LBB100_27
.LBB100_25:
	add r10, r5, r8
	add r9, r6, r0
	bge r10, r4, .LBB100_24
.LBB100_26:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB100_24
.LBB100_27:
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldw r1, r14+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB100_29
.LBB100_28:
	addi r1, r1, -1
	stw r14+%lo(fio_rep), r1
.LBB100_29:
	ldw r1, r14+%lo(fio_rep)
	bne r1, r0, .LBB100_31
.LBB100_30:
	stw r13+%lo(fio_desc), r0
.LBB100_31:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB100_32:
	lui r3, %hi(stderr)
	ldw r3, r3+%lo(stderr)
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end100:
	.size	f77_rd_s, .Lfunc_end100-f77_rd_s
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_in_real
	.type	.LCPI101_0,@object
.LCPI101_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI101_0, 8
	.text
	.p2align	2
	.type	fio_in_real,@function
fio_in_real:                            # @fio_in_real
# %bb.0:
	addi sp, sp, -2048
	addi sp, sp, -1104
	stw sp+0, lr
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1101, r11
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1097, r12
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1093, r13
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1089, r14
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1085, r15
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1081, r16
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1077, r17
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1073, r18
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1069, r19
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1065, r20
	addi r1, sp, 24
	addi r5, r0, 1
	lui r6, %hi(fio_rpos)
	blt r3, r5, .LBB101_13
.LBB101_1:
	ldw r7, r6+%lo(fio_rpos)
	lui r8, %hi(fio_rlen)
	ldw r8, r8+%lo(fio_rlen)
	lui r9, %hi(fio_bz)
	ldw r9, r9+%lo(fio_bz)
	addi r10, r0, 32
	lui lr, %hi(fio_rec)
	addi lr, lr, %lo(fio_rec)
	addi r11, r0, 48
	addi r12, r0, 1023
	addi r13, r0, 1
	addi r14, r0, 9
	addi r15, r0, 1022
	add r17, r3, r0
	add r18, r0, r0
	add r16, r0, r0
	jal r0, .LBB101_5
.LBB101_2:
	addi r18, r16, 1
	stw sp+16, r1
	add r1, sp, r0
	addi r1, r1, 2047
	addi r20, r1, 41
	ldw r1, sp+16
	add r16, r20, r16
	stb r16+0, r19
	add r16, r18, r0
.LBB101_3:
	add r18, r13, r0
.LBB101_4:
	addi r17, r17, -1
	addi r7, r7, 1
	beq r17, r0, .LBB101_14
.LBB101_5:
	add r19, r10, r0
	bge r7, r8, .LBB101_7
.LBB101_6:
	add r19, r7, lr
	ldb r19, r19+0
.LBB101_7:
	beq r19, r10, .LBB101_9
.LBB101_8:
	bne r19, r14, .LBB101_12
.LBB101_9:
	beq r9, r0, .LBB101_4
.LBB101_10:
	beq r18, r0, .LBB101_4
.LBB101_11:
	add r19, r11, r0
	blt r16, r12, .LBB101_2
	jal r0, .LBB101_4
.LBB101_12:
	bgt r16, r15, .LBB101_3
	jal r0, .LBB101_2
.LBB101_13:
	add r16, r0, r0
.LBB101_14:
	add r8, sp, r0
	addi r8, r8, 2047
	addi r7, r8, 41
	add r7, r7, r16
	stb r7+0, r0
	ldw r7, r6+%lo(fio_rpos)
	add r3, r7, r3
	stw r6+%lo(fio_rpos), r3
	add r6, sp, r0
	addi r6, r6, 2047
	ldbu r3, r6+41
	lui r8, %hi(.LCPI101_0)
	addi r8, r8, %lo(.LCPI101_0)
	ldw r7, r8+4
	ldw r6, r8+0
	beq r3, r0, .LBB101_67
.LBB101_15:
	addi lr, r0, 43
	beq r3, lr, .LBB101_18
.LBB101_16:
	addi r8, r0, 45
	bne r3, r8, .LBB101_19
.LBB101_17:
	addi r10, r0, 1
	add r8, r0, r0
	jal r0, .LBB101_21
.LBB101_18:
	add r10, r5, r0
	jal r0, .LBB101_20
.LBB101_19:
	add r10, r0, r0
.LBB101_20:
	add r8, r5, r0
.LBB101_21:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r3, r9, 41
	or  r3, r3, r10
	ldbu r19, r3+0
	beq r19, r0, .LBB101_42
.LBB101_22:
	addi r13, r0, 1
	addi r14, r0, 46
	addi r11, r0, 0
	addi r15, r0, -10
	addi r16, r0, 70
	lui r17, %hi(.LJTI101_0)
	addi r17, r17, %lo(.LJTI101_0)
	add r9, r13, r0
	add r12, r0, r0
	add r18, r0, r0
	add r3, r0, r0
.LBB101_23:
	andi r5, r19, 255
	bne r5, r14, .LBB101_25
.LBB101_24:
	add r18, r13, r0
	jal r0, .LBB101_34
.LBB101_25:
	slli r20, r19, 24
	srai r20, r20, 24
	addi r20, r20, -58
	bgeu r20, r15, .LBB101_31
.LBB101_26:
	addi r5, r5, -43
	bgtu r5, r16, .LBB101_40
.LBB101_27:
	slli r5, r5, 2
	add r5, r17, r5
	ldw r20, r5+0
	add r5, r0, r0
	add r19, r13, r0
	jalr r0, r20, 0
.LBB101_28:
	addi r5, r10, 1
	add r20, sp, r0
	addi r20, r20, 2047
	addi r19, r20, 41
	add r19, r19, r5
	ldbu r19, r19+0
	beq r19, lr, .LBB101_35
.LBB101_29:
	addi r20, r0, 45
	bne r19, r20, .LBB101_37
.LBB101_30:
	addi r9, r0, -1
	add r10, r5, r0
	jal r0, .LBB101_37
.LBB101_31:
	addi r5, r0, 1022
	bgt r3, r5, .LBB101_33
.LBB101_32:
	addi r5, r3, 1
	addi r20, sp, 1064
	add r3, r20, r3
	stb r3+0, r19
	add r3, r5, r0
.LBB101_33:
	sne r5, r18, r11
	add r12, r12, r5
.LBB101_34:
	add r5, r13, r0
	add r19, r13, r0
	bne r19, r0, .LBB101_39
	jal r0, .LBB101_41
.LBB101_35:
	add r10, r5, r0
	jal r0, .LBB101_37
.LBB101_36:
	addi r9, r0, -1
.LBB101_37:
	add r5, r0, r0
	add r19, r13, r0
.LBB101_38:
	beq r19, r0, .LBB101_41
.LBB101_39:
	addi r10, r10, 1
	add r20, sp, r0
	addi r20, r20, 2047
	addi r19, r20, 41
	add r19, r19, r10
	ldbu r19, r19+0
	sne r20, r19, r11
	and r20, r20, r5
	bne r20, r0, .LBB101_23
	jal r0, .LBB101_41
.LBB101_40:
	add r5, r13, r0
	add r19, r0, r0
	bne r19, r0, .LBB101_39
.LBB101_41:
	seq lr, r18, r11
	xor r4, r4, r12
	sub lr, r0, lr
	and r4, r4, lr
	xor r4, r12, r4
	beq r5, r0, .LBB101_43
	jal r0, .LBB101_47
.LBB101_42:
	add r3, r0, r0
	add r9, r5, r0
	bne r5, r0, .LBB101_47
.LBB101_43:
	add r11, sp, r0
	addi r11, r11, 2047
	addi lr, r11, 41
	add lr, lr, r10
	ldbu r12, lr+0
	beq r12, r0, .LBB101_47
.LBB101_44:
	add r11, sp, r0
	addi r11, r11, 2047
	addi lr, r11, 41
	add r10, r10, lr
	addi r10, r10, 1
	addi lr, r0, 9
	addi r11, r0, 10
	add r13, r0, r0
.LBB101_45:
	slli r12, r12, 24
	srai r12, r12, 24
	addi r12, r12, -48
	sgtu r14, r12, lr
	mul r15, r13, r11
	add r12, r12, r15
	xor r13, r13, r12
	sub r14, r0, r14
	and r13, r13, r14
	xor r13, r12, r13
	ldbu r12, r10+0
	addi r10, r10, 1
	bne r12, r0, .LBB101_45
.LBB101_46:
	mul r10, r13, r9
	jal r0, .LBB101_48
.LBB101_47:
	add r10, r0, r0
.LBB101_48:
	addi r9, sp, 1064
	add r9, r9, r3
	stb r9+0, r0
	beq r3, r0, .LBB101_67
.LBB101_49:
	sub r9, r3, r4
	lui r6, %hi(fio_scale)
	ldw r6, r6+%lo(fio_scale)
	sub r5, r0, r5
	and r6, r6, r5
	addi lr, sp, 24
	add r5, r0, r0
	bne r8, r0, .LBB101_51
.LBB101_50:
	addi lr, r1, 1
	addi r1, r0, 45
	stb sp+24, r1
	addi r5, r0, 1
.LBB101_51:
	sub r6, r10, r6
	addi r7, r0, 1
	blt r9, r7, .LBB101_56
.LBB101_52:
	blt r3, r7, .LBB101_62
.LBB101_53:
	xor r1, r9, r3
	slt r4, r9, r3
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r3, r1
	addi r4, sp, 24
	or  r7, r4, r5
	add r4, r0, r0
.LBB101_54:
	addi r8, sp, 1064
	add r8, r8, r4
	ldbu r8, r8+0
	add r9, r7, r4
	stb r9+0, r8
	addi r4, r4, 1
	bne r1, r4, .LBB101_54
.LBB101_55:
	add r5, r5, r4
	jal r0, .LBB101_63
.LBB101_56:
	addi r1, sp, 24
	or  r1, r1, r5
	addi r8, r0, 48
	stb r1+0, r8
	addi r1, r5, 2
	addi r5, r0, 46
	stb lr+1, r5
	addi r5, r0, -1
	bgt r9, r5, .LBB101_59
.LBB101_57:
	sub r4, r4, r3
.LBB101_58:
	add r5, r1, r0
	addi r1, r1, 1
	addi r9, sp, 24
	add r5, r9, r5
	stb r5+0, r8
	addi r4, r4, -1
	bne r4, r0, .LBB101_58
.LBB101_59:
	blt r3, r7, .LBB101_66
.LBB101_60:
	addi r4, sp, 1064
.LBB101_61:
	add r5, r1, r0
	ldbu r7, r4+0
	addi r1, r1, 1
	addi r8, sp, 24
	add r5, r8, r5
	stb r5+0, r7
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r0, .LBB101_61
	jal r0, .LBB101_66
.LBB101_62:
	add r4, r0, r0
.LBB101_63:
	addi r1, sp, 24
	add r1, r1, r5
	addi r7, r0, 46
	stb r1+0, r7
	addi r1, r5, 1
	ble r3, r4, .LBB101_66
.LBB101_64:
	sub r3, r3, r4
	addi r5, sp, 1064
	add r4, r5, r4
.LBB101_65:
	ldbu r5, r4+0
	addi r7, sp, 24
	add r7, r7, r1
	stb r7+0, r5
	addi r1, r1, 1
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r0, .LBB101_65
.LBB101_66:
	addi r11, sp, 24
	add r3, r11, r1
	addi r4, r0, 1040
	sub r4, r4, r1
	lui r5, %hi(.L.str.92)
	addi r5, r5, %lo(.L.str.92)
	jal r31, snprintf
	add r3, r11, r0
	add r4, r0, r0
	jal r31, strtod
	add r6, r1, r0
	add r7, r2, r0
.LBB101_67:
	add r1, r6, r0
	add r2, r7, r0
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r20, r3+1065
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r19, r3+1069
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r18, r3+1073
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r17, r3+1077
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r16, r3+1081
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r15, r3+1085
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r14, r3+1089
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r13, r3+1093
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r12, r3+1097
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r11, r3+1101
	ldw lr, sp+0
	addi sp, sp, 2047
	addi sp, sp, 1105
	jalr r0, r31, 0
.Lfunc_end101:
	.size	fio_in_real, .Lfunc_end101-fio_in_real
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI101_0,@object
.LJTI101_0:
	.word	.LBB101_38
	.word	.LBB101_40
	.word	.LBB101_36
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_28
	.word	.LBB101_28
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_28
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_28
	.word	.LBB101_28
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_40
	.word	.LBB101_28
	.size	.LJTI101_0, 284
                                        # -- End function
	.text
	.globl	f77_rd_in                       # -- Begin function f77_rd_in
	.p2align	2
	.type	f77_rd_in,@function
f77_rd_in:                              # @f77_rd_in
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	add r12, r4, r0
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB102_6
.LBB102_1:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB102_5
.LBB102_2:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB102_5
.LBB102_3:
	addi r3, r0, 65
	bne r1, r3, .LBB102_6
.LBB102_4:
	add r3, r11, r0
	add r4, r12, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_rd_s
.LBB102_5:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 65
	beq r1, r3, .LBB102_4
.LBB102_6:
	addi r13, r0, 2
	bne r12, r13, .LBB102_8
.LBB102_7:
	ldh r1, r11+0
	jal r0, .LBB102_9
.LBB102_8:
	ldbu r1, r11+0
.LBB102_9:
	stw sp+16, r1
	addi r3, sp, 16
	jal r31, f77_rd_i
	ldw r1, sp+16
	bne r12, r13, .LBB102_11
.LBB102_10:
	sth r11+0, r1
	jal r0, .LBB102_12
.LBB102_11:
	stb r11+0, r1
.LBB102_12:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end102:
	.size	f77_rd_in, .Lfunc_end102-f77_rd_in
                                        # -- End function
	.globl	f77_wr_in                       # -- Begin function f77_wr_in
	.p2align	2
	.type	f77_wr_in,@function
f77_wr_in:                              # @f77_wr_in
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB103_19
.LBB103_1:
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB103_18
.LBB103_2:
	ldw r5, r12+%lo(fio_rep)
	ble r5, r0, .LBB103_18
.LBB103_3:
	addi r5, r0, 65
	bne r1, r5, .LBB103_19
.LBB103_4:
	sth sp+28, r3
	lui r1, %hi(fio_w)
	ldw r5, r1+%lo(fio_w)
	addi r6, r0, 2
	bne r4, r6, .LBB103_20
.LBB103_5:
	addi r1, r0, 1
	beq r5, r1, .LBB103_9
.LBB103_6:
	addi r1, r0, 3
	blt r5, r1, .LBB103_8
.LBB103_7:
	lui r1, %hi(fio_col)
	ldw r3, r1+%lo(fio_col)
	add r3, r5, r3
	addi r3, r3, -2
	stw r1+%lo(fio_col), r3
.LBB103_8:
	add r1, r6, r0
.LBB103_9:
	lui r3, %hi(fio_len)
	ldw r7, r3+%lo(fio_len)
	lui r4, %hi(fio_col)
	ldw r10, r4+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r8, r0, 32
	add r9, r0, r0
	jal r0, .LBB103_12
.LBB103_10:
	add r10, lr, r0
.LBB103_11:
	addi r9, r9, 1
	beq r9, r1, .LBB103_29
.LBB103_12:
	addi lr, sp, 28
	bgt r10, r5, .LBB103_11
.LBB103_13:
	bge r7, r10, .LBB103_16
.LBB103_14:
	addi r13, r7, 1
	add r7, r7, r6
	stb r7+0, r8
	add r7, r13, r0
	bne r10, r13, .LBB103_14
.LBB103_15:
	stw r3+%lo(fio_len), r10
	add r7, r10, r0
.LBB103_16:
	add lr, lr, r9
	ldbu lr, lr+0
	add r13, r10, r6
	stb r13+0, lr
	addi lr, r10, 1
	stw r4+%lo(fio_col), lr
	blt r10, r7, .LBB103_10
.LBB103_17:
	stw r3+%lo(fio_len), lr
	add r7, lr, r0
	jal r0, .LBB103_10
.LBB103_18:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	add r14, r4, r0
	jal r31, fio_next_desc
	add r4, r14, r0
	add r3, r13, r0
	addi r5, r0, 65
	beq r1, r5, .LBB103_4
.LBB103_19:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_wr_i
.LBB103_20:
	lui r1, %hi(fio_col)
	blt r5, r6, .LBB103_22
.LBB103_21:
	ldw r4, r1+%lo(fio_col)
	add r4, r5, r4
	addi r4, r4, -1
	stw r1+%lo(fio_col), r4
.LBB103_22:
	ldw r4, r1+%lo(fio_col)
	addi r5, r0, 1022
	bgt r4, r5, .LBB103_29
.LBB103_23:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	bge r6, r4, .LBB103_27
.LBB103_24:
	addi r8, r0, 32
.LBB103_25:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r4, r9, .LBB103_25
.LBB103_26:
	stw r5+%lo(fio_len), r4
	add r6, r4, r0
.LBB103_27:
	add r7, r4, r7
	stb r7+0, r3
	addi r3, r4, 1
	stw r1+%lo(fio_col), r3
	blt r4, r6, .LBB103_29
.LBB103_28:
	stw r5+%lo(fio_len), r3
.LBB103_29:
	ldw r1, r12+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB103_31
.LBB103_30:
	addi r1, r1, -1
	stw r12+%lo(fio_rep), r1
.LBB103_31:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB103_33
.LBB103_32:
	stw r11+%lo(fio_desc), r0
.LBB103_33:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end103:
	.size	f77_wr_in, .Lfunc_end103-f77_wr_in
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_rd_d
	.type	.LCPI104_0,@object
.LCPI104_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI104_0, 8
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
	stw sp+1052, r19
	stw sp+1048, r20
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB104_5
.LBB104_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB104_41
.LBB104_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB104_41
.LBB104_3:
	addi r3, r0, 1
	bne r1, r3, .LBB104_26
.LBB104_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB104_41
.LBB104_5:
	lui r12, %hi(fio_desc)
	ldw r1, r12+%lo(fio_desc)
	lui r13, %hi(fio_rep)
	beq r1, r0, .LBB104_11
.LBB104_6:
	ldw r3, r13+%lo(fio_rep)
	ble r3, r0, .LBB104_11
.LBB104_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB104_12
.LBB104_8:
	beq r1, r0, .LBB104_41
.LBB104_9:
	addi r3, r0, 65
	bne r1, r3, .LBB104_30
.LBB104_10:
	addi r4, r0, 8
	add r3, r11, r0
	ldw r20, sp+1048
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
	jal r0, f77_rd_s
.LBB104_11:
	stw r12+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB104_8
.LBB104_12:
	addi r3, r0, 73
	bne r1, r3, .LBB104_29
.LBB104_13:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	blt r1, r4, .LBB104_33
.LBB104_14:
	ldw r5, r3+%lo(fio_rpos)
	lui r6, %hi(fio_rlen)
	ldw r6, r6+%lo(fio_rlen)
	lui r7, %hi(fio_bz)
	ldw r7, r7+%lo(fio_bz)
	addi r8, r0, 32
	lui r9, %hi(fio_rec)
	addi r9, r9, %lo(fio_rec)
	addi r10, r0, 48
	addi lr, r0, 1023
	addi r14, r0, 9
	addi r15, r0, 1022
	add r17, r1, r0
	add r18, r0, r0
	add r16, r0, r0
	jal r0, .LBB104_18
.LBB104_15:
	addi r18, r16, 1
	addi r20, sp, 24
	add r16, r20, r16
	stb r16+0, r19
	add r16, r18, r0
.LBB104_16:
	add r18, r4, r0
.LBB104_17:
	addi r17, r17, -1
	addi r5, r5, 1
	beq r17, r0, .LBB104_34
.LBB104_18:
	add r19, r8, r0
	bge r5, r6, .LBB104_20
.LBB104_19:
	add r19, r5, r9
	ldb r19, r19+0
.LBB104_20:
	beq r19, r8, .LBB104_22
.LBB104_21:
	bne r19, r14, .LBB104_25
.LBB104_22:
	beq r7, r0, .LBB104_17
.LBB104_23:
	beq r18, r0, .LBB104_17
.LBB104_24:
	add r19, r10, r0
	blt r16, lr, .LBB104_15
	jal r0, .LBB104_17
.LBB104_25:
	ble r16, r15, .LBB104_15
	jal r0, .LBB104_16
.LBB104_26:
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB104_31
.LBB104_27:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB104_28:
	add r7, r6, r0
	andi r6, r5, 223
	seq r8, r6, r1
	seq r6, r6, r3
	sub r8, r0, r8
	sub r6, r0, r6
	xori r9, r5, 69
	and r6, r9, r6
	xor r5, r5, r6
	xori r6, r5, 69
	and r6, r6, r8
	xor r5, r5, r6
	addi r6, sp, 24
	add r6, r6, r7
	stb r6+0, r5
	addi r6, r7, 1
	add r5, r7, r4
	ldbu r5, r5+0
	bne r5, r0, .LBB104_28
	jal r0, .LBB104_32
.LBB104_29:
	addi r3, r0, 76
	beq r1, r3, .LBB104_42
.LBB104_30:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	jal r0, .LBB104_37
.LBB104_31:
	add r6, r0, r0
.LBB104_32:
	addi r3, sp, 24
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
	jal r0, .LBB104_41
.LBB104_33:
	add r16, r0, r0
.LBB104_34:
	addi r4, sp, 24
	add r4, r4, r16
	stb r4+0, r0
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldbu r1, sp+24
	beq r1, r0, .LBB104_36
.LBB104_35:
	addi r3, sp, 24
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	fcvt.d.w r4, r1
	jal r0, .LBB104_37
.LBB104_36:
	lui r1, %hi(.LCPI104_0)
	addi r1, r1, %lo(.LCPI104_0)
	ldw r5, r1+4
	ldw r4, r1+0
.LBB104_37:
	stw r11+4, r5
	stw r11+0, r4
	ldw r1, r13+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB104_39
.LBB104_38:
	addi r1, r1, -1
	stw r13+%lo(fio_rep), r1
.LBB104_39:
	ldw r1, r13+%lo(fio_rep)
	bne r1, r0, .LBB104_41
.LBB104_40:
	stw r12+%lo(fio_desc), r0
.LBB104_41:
	ldw r20, sp+1048
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
.LBB104_42:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end104:
	.size	f77_rd_d, .Lfunc_end104-f77_rd_d
                                        # -- End function
	.p2align	2                               # -- Begin function fio_list_real
	.type	fio_list_real,@function
fio_list_real:                          # @fio_list_real
# %bb.0:
	addi sp, sp, -144
	stw sp+0, lr
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB105_3
.LBB105_1:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB105_2:
	add r7, r6, r0
	andi r6, r5, 223
	seq r8, r6, r1
	seq r6, r6, r3
	sub r8, r0, r8
	sub r6, r0, r6
	xori r9, r5, 69
	and r6, r9, r6
	xor r5, r5, r6
	xori r6, r5, 69
	and r6, r6, r8
	xor r5, r5, r6
	addi r6, sp, 16
	add r6, r6, r7
	stb r6+0, r5
	addi r6, r7, 1
	add r5, r7, r4
	ldbu r5, r5+0
	bne r5, r0, .LBB105_2
	jal r0, .LBB105_4
.LBB105_3:
	add r6, r0, r0
.LBB105_4:
	addi r3, sp, 16
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	ldw lr, sp+0
	addi sp, sp, 144
	jalr r0, r31, 0
.Lfunc_end105:
	.size	fio_list_real, .Lfunc_end105-fio_list_real
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rd_r
	.type	.LCPI106_0,@object
.LCPI106_0:
	.word	0x00000000                      # float 0
	.size	.LCPI106_0, 4
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
	stw sp+1052, r19
	stw sp+1048, r20
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB106_5
.LBB106_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB106_41
.LBB106_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB106_41
.LBB106_3:
	addi r3, r0, 1
	bne r1, r3, .LBB106_26
.LBB106_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB106_41
.LBB106_5:
	lui r12, %hi(fio_desc)
	ldw r1, r12+%lo(fio_desc)
	lui r13, %hi(fio_rep)
	beq r1, r0, .LBB106_11
.LBB106_6:
	ldw r3, r13+%lo(fio_rep)
	ble r3, r0, .LBB106_11
.LBB106_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB106_12
.LBB106_8:
	beq r1, r0, .LBB106_41
.LBB106_9:
	addi r3, r0, 65
	bne r1, r3, .LBB106_30
.LBB106_10:
	addi r4, r0, 4
	add r3, r11, r0
	ldw r20, sp+1048
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
	jal r0, f77_rd_s
.LBB106_11:
	stw r12+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB106_8
.LBB106_12:
	addi r3, r0, 73
	bne r1, r3, .LBB106_29
.LBB106_13:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	blt r1, r4, .LBB106_33
.LBB106_14:
	ldw r5, r3+%lo(fio_rpos)
	lui r6, %hi(fio_rlen)
	ldw r6, r6+%lo(fio_rlen)
	lui r7, %hi(fio_bz)
	ldw r7, r7+%lo(fio_bz)
	addi r8, r0, 32
	lui r9, %hi(fio_rec)
	addi r9, r9, %lo(fio_rec)
	addi r10, r0, 48
	addi lr, r0, 1023
	addi r14, r0, 9
	addi r15, r0, 1022
	add r17, r1, r0
	add r18, r0, r0
	add r16, r0, r0
	jal r0, .LBB106_18
.LBB106_15:
	addi r18, r16, 1
	addi r20, sp, 24
	add r16, r20, r16
	stb r16+0, r19
	add r16, r18, r0
.LBB106_16:
	add r18, r4, r0
.LBB106_17:
	addi r17, r17, -1
	addi r5, r5, 1
	beq r17, r0, .LBB106_34
.LBB106_18:
	add r19, r8, r0
	bge r5, r6, .LBB106_20
.LBB106_19:
	add r19, r5, r9
	ldb r19, r19+0
.LBB106_20:
	beq r19, r8, .LBB106_22
.LBB106_21:
	bne r19, r14, .LBB106_25
.LBB106_22:
	beq r7, r0, .LBB106_17
.LBB106_23:
	beq r18, r0, .LBB106_17
.LBB106_24:
	add r19, r10, r0
	blt r16, lr, .LBB106_15
	jal r0, .LBB106_17
.LBB106_25:
	ble r16, r15, .LBB106_15
	jal r0, .LBB106_16
.LBB106_26:
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB106_31
.LBB106_27:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB106_28:
	add r7, r6, r0
	andi r6, r5, 223
	seq r8, r6, r1
	seq r6, r6, r3
	sub r8, r0, r8
	sub r6, r0, r6
	xori r9, r5, 69
	and r6, r9, r6
	xor r5, r5, r6
	xori r6, r5, 69
	and r6, r6, r8
	xor r5, r5, r6
	addi r6, sp, 24
	add r6, r6, r7
	stb r6+0, r5
	addi r6, r7, 1
	add r5, r7, r4
	ldbu r5, r5+0
	bne r5, r0, .LBB106_28
	jal r0, .LBB106_32
.LBB106_29:
	addi r3, r0, 76
	beq r1, r3, .LBB106_42
.LBB106_30:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	jal r0, .LBB106_37
.LBB106_31:
	add r6, r0, r0
.LBB106_32:
	addi r3, sp, 24
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	jal r0, .LBB106_41
.LBB106_33:
	add r16, r0, r0
.LBB106_34:
	addi r4, sp, 24
	add r4, r4, r16
	stb r4+0, r0
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldbu r1, sp+24
	beq r1, r0, .LBB106_36
.LBB106_35:
	addi r3, sp, 24
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	fcvt.s.w r1, r1
	jal r0, .LBB106_37
.LBB106_36:
	lui r1, %hi(.LCPI106_0)
	ldw r1, r1+%lo(.LCPI106_0)
.LBB106_37:
	stw r11+0, r1
	ldw r1, r13+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB106_39
.LBB106_38:
	addi r1, r1, -1
	stw r13+%lo(fio_rep), r1
.LBB106_39:
	ldw r1, r13+%lo(fio_rep)
	bne r1, r0, .LBB106_41
.LBB106_40:
	stw r12+%lo(fio_desc), r0
.LBB106_41:
	ldw r20, sp+1048
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
.LBB106_42:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end106:
	.size	f77_rd_r, .Lfunc_end106-f77_rd_r
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rd_c
	.type	.LCPI107_0,@object
.LCPI107_0:
	.word	0x00000000                      # float 0
	.size	.LCPI107_0, 4
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
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB107_5
.LBB107_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB107_38
.LBB107_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB107_38
.LBB107_3:
	addi r14, r0, 1
	bne r1, r14, .LBB107_6
.LBB107_4:
	stb r12+%lo(fio_ldone), r14
	jal r0, .LBB107_38
.LBB107_5:
	add r3, r11, r0
	jal r31, f77_rd_r
	addi r3, r11, 4
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
.LBB107_6:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	ldbu r1, r12+0
	addi r3, r0, 40
	bne r1, r3, .LBB107_12
.LBB107_7:
	lui r15, %hi(fio_ltok+1)
	addi r15, r15, %lo(fio_ltok+1)
	ldbu r4, r15+0
	beq r4, r0, .LBB107_15
.LBB107_8:
	lui r1, %hi(fio_ltok+2)
	addi r1, r1, %lo(fio_ltok+2)
	addi r3, r0, 126
	add r13, r0, r0
	jal r0, .LBB107_10
.LBB107_9:
	ldbu r4, r1+0
	addi r1, r1, 1
	beq r4, r0, .LBB107_16
.LBB107_10:
	bgt r13, r3, .LBB107_9
.LBB107_11:
	addi r5, r13, 1
	addi r6, sp, 20
	add r6, r6, r13
	stb r6+0, r4
	add r13, r5, r0
	jal r0, .LBB107_9
.LBB107_12:
	beq r1, r0, .LBB107_28
.LBB107_13:
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB107_14:
	add r7, r6, r0
	andi r6, r1, 223
	seq r8, r6, r3
	seq r6, r6, r4
	sub r8, r0, r8
	sub r6, r0, r6
	xori r9, r1, 69
	and r6, r9, r6
	xor r1, r1, r6
	xori r6, r1, 69
	and r6, r6, r8
	xor r1, r1, r6
	addi r6, sp, 148
	add r6, r6, r7
	stb r6+0, r1
	addi r6, r7, 1
	add r1, r7, r5
	ldbu r1, r1+0
	bne r1, r0, .LBB107_14
	jal r0, .LBB107_29
.LBB107_15:
	add r13, r0, r0
.LBB107_16:
	lui r16, %hi(fio_rpos)
	lui r17, %hi(fio_rlen)
	addi r18, r0, 126
	lui r19, %hi(fio_rec)
	addi r19, r19, %lo(fio_rec)
	addi r20, r0, 41
.LBB107_17:
	addi r1, sp, 20
	add r21, r1, r13
	jal r0, .LBB107_19
.LBB107_18:
	jal r31, fio_getrec
	bne r1, r0, .LBB107_39
.LBB107_19:
	beq r13, r0, .LBB107_21
.LBB107_20:
	ldbu r1, r21+-1
	beq r1, r20, .LBB107_24
.LBB107_21:
	ldw r1, r16+%lo(fio_rpos)
	ldw r3, r17+%lo(fio_rlen)
	bge r1, r3, .LBB107_18
.LBB107_22:
	addi r3, r1, 1
	stw r16+%lo(fio_rpos), r3
	bgt r13, r18, .LBB107_19
.LBB107_23:
	add r1, r1, r19
	ldbu r1, r1+0
	addi r3, r13, 1
	addi r4, sp, 20
	add r4, r4, r13
	stb r4+0, r1
	add r13, r3, r0
	jal r0, .LBB107_17
.LBB107_24:
	stb r21+-1, r0
	addi r3, sp, 20
	addi r4, r0, 44
	jal r31, strchr
	beq r1, r0, .LBB107_30
.LBB107_25:
	add r13, r1, r0
	stb r1+0, r0
	addi r4, sp, 20
	add r3, r12, r0
	jal r31, strcpy
	ldbu r4, r12+0
	beq r4, r0, .LBB107_31
.LBB107_26:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB107_27:
	add r6, r5, r0
	andi r5, r4, 223
	seq r7, r5, r1
	seq r5, r5, r3
	sub r7, r0, r7
	sub r5, r0, r5
	xori r8, r4, 69
	and r5, r8, r5
	xor r4, r4, r5
	xori r5, r4, 69
	and r5, r5, r7
	xor r4, r4, r5
	addi r5, sp, 148
	add r5, r5, r6
	stb r5+0, r4
	addi r5, r6, 1
	add r4, r6, r15
	ldbu r4, r4+0
	bne r4, r0, .LBB107_27
	jal r0, .LBB107_32
.LBB107_28:
	add r6, r0, r0
.LBB107_29:
	addi r3, sp, 148
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	stw r11+4, r0
	jal r0, .LBB107_38
.LBB107_30:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r4, sp, 20
	jal r31, strcpy
	jal r31, fio_list_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	lui r1, %hi(.LCPI107_0)
	ldw r1, r1+%lo(.LCPI107_0)
	jal r0, .LBB107_37
.LBB107_31:
	add r5, r0, r0
.LBB107_32:
	addi r3, sp, 148
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	addi r4, r13, 1
	add r3, r12, r0
	jal r31, strcpy
	ldbu r4, r12+0
	beq r4, r0, .LBB107_35
.LBB107_33:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB107_34:
	add r6, r5, r0
	andi r5, r4, 223
	seq r7, r5, r1
	seq r5, r5, r3
	sub r7, r0, r7
	sub r5, r0, r5
	xori r8, r4, 69
	and r5, r8, r5
	xor r4, r4, r5
	xori r5, r4, 69
	and r5, r5, r7
	xor r4, r4, r5
	addi r5, sp, 148
	add r5, r5, r6
	stb r5+0, r4
	addi r5, r6, 1
	add r4, r6, r15
	ldbu r4, r4+0
	bne r4, r0, .LBB107_34
	jal r0, .LBB107_36
.LBB107_35:
	add r5, r0, r0
.LBB107_36:
	addi r3, sp, 148
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
.LBB107_37:
	stw r11+4, r1
	lui r1, %hi(fio_l_after)
	stb r1+%lo(fio_l_after), r14
.LBB107_38:
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
.LBB107_39:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end107:
	.size	f77_rd_c, .Lfunc_end107-f77_rd_c
                                        # -- End function
	.globl	f77_rd_z                        # -- Begin function f77_rd_z
	.p2align	2
	.type	f77_rd_z,@function
f77_rd_z:                               # @f77_rd_z
# %bb.0:
	addi sp, sp, -160
	stw sp+0, lr
	stw sp+156, r11
	stw sp+152, r12
	stw sp+148, r13
	stw sp+144, r14
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB108_5
.LBB108_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB108_26
.LBB108_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB108_26
.LBB108_3:
	addi r3, r0, 1
	bne r1, r3, .LBB108_6
.LBB108_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB108_26
.LBB108_5:
	add r3, r11, r0
	jal r31, f77_rd_d
	addi r3, r11, 8
	ldw r14, sp+144
	ldw r13, sp+148
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jal r0, f77_rd_d
.LBB108_6:
	lui r1, %hi(fio_ltok)
	ldbu r1, r1+%lo(fio_ltok)
	addi r3, r0, 40
	bne r1, r3, .LBB108_13
.LBB108_7:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	addi r4, r0, 41
	add r3, r13, r0
	jal r31, strchr
	beq r1, r0, .LBB108_9
.LBB108_8:
	stb r1+0, r0
.LBB108_9:
	lui r3, %hi(fio_ltok+1)
	addi r3, r3, %lo(fio_ltok+1)
	addi r4, r0, 44
	jal r31, strchr
	beq r1, r0, .LBB108_16
.LBB108_10:
	add r14, r1, r0
	stb r1+0, r0
	lui r12, %hi(fio_ltok+1)
	addi r12, r12, %lo(fio_ltok+1)
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, memmove
	ldbu r4, r13+0
	beq r4, r0, .LBB108_20
.LBB108_11:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB108_12:
	add r6, r5, r0
	andi r5, r4, 223
	seq r7, r5, r1
	seq r5, r5, r3
	sub r7, r0, r7
	sub r5, r0, r5
	xori r8, r4, 69
	and r5, r8, r5
	xor r4, r4, r5
	xori r5, r4, 69
	and r5, r5, r7
	xor r4, r4, r5
	addi r5, sp, 16
	add r5, r5, r6
	stb r5+0, r4
	addi r5, r6, 1
	add r4, r6, r12
	ldbu r4, r4+0
	bne r4, r0, .LBB108_12
	jal r0, .LBB108_21
.LBB108_13:
	beq r1, r0, .LBB108_17
.LBB108_14:
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB108_15:
	add r7, r6, r0
	andi r6, r1, 223
	seq r8, r6, r3
	seq r6, r6, r4
	sub r8, r0, r8
	sub r6, r0, r6
	xori r9, r1, 69
	and r6, r9, r6
	xor r1, r1, r6
	xori r6, r1, 69
	and r6, r6, r8
	xor r1, r1, r6
	addi r6, sp, 16
	add r6, r6, r7
	stb r6+0, r1
	addi r6, r7, 1
	add r1, r7, r5
	ldbu r1, r1+0
	bne r1, r0, .LBB108_15
	jal r0, .LBB108_18
.LBB108_16:
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
	jal r0, .LBB108_19
.LBB108_17:
	add r6, r0, r0
.LBB108_18:
	addi r3, sp, 16
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
.LBB108_19:
	stw r11+4, r2
	stw r11+0, r1
	stw r11+12, r0
	stw r11+8, r0
	jal r0, .LBB108_26
.LBB108_20:
	add r5, r0, r0
.LBB108_21:
	addi r3, sp, 16
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
	addi r4, r14, 1
	add r3, r13, r0
	jal r31, strcpy
	ldbu r4, r13+0
	beq r4, r0, .LBB108_24
.LBB108_22:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB108_23:
	add r6, r5, r0
	andi r5, r4, 223
	seq r7, r5, r1
	seq r5, r5, r3
	sub r7, r0, r7
	sub r5, r0, r5
	xori r8, r4, 69
	and r5, r8, r5
	xor r4, r4, r5
	xori r5, r4, 69
	and r5, r5, r7
	xor r4, r4, r5
	addi r5, sp, 16
	add r5, r5, r6
	stb r5+0, r4
	addi r5, r6, 1
	add r4, r6, r12
	ldbu r4, r4+0
	bne r4, r0, .LBB108_23
	jal r0, .LBB108_25
.LBB108_24:
	add r5, r0, r0
.LBB108_25:
	addi r3, sp, 16
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r11+12, r2
	stw r11+8, r1
.LBB108_26:
	ldw r14, sp+144
	ldw r13, sp+148
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jalr r0, r31, 0
.Lfunc_end108:
	.size	f77_rd_z, .Lfunc_end108-f77_rd_z
                                        # -- End function
	.globl	f77_rd_end                      # -- Begin function f77_rd_end
	.p2align	2
	.type	f77_rd_end,@function
f77_rd_end:                             # @f77_rd_end
# %bb.0:
	lui r1, %hi(fio_reading)
	stb r1+%lo(fio_reading), r0
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	jalr r0, r31, 0
.Lfunc_end109:
	.size	f77_rd_end, .Lfunc_end109-f77_rd_end
                                        # -- End function
	.globl	f77_nml_begin                   # -- Begin function f77_nml_begin
	.p2align	2
	.type	f77_nml_begin,@function
f77_nml_begin:                          # @f77_nml_begin
# %bb.0:
	addi sp, sp, -1152
	stw sp+0, lr
	stw sp+4, fp
	stw sp+1148, r11
	stw sp+1144, r12
	stw sp+1140, r13
	stw sp+1136, r14
	stw sp+1132, r15
	stw sp+1128, r16
	stw sp+1124, r17
	stw sp+1120, r18
	stw sp+1116, r19
	stw sp+1112, r20
	stw sp+1108, r21
	stw sp+1104, r22
	stw sp+1100, r23
	stw sp+1096, r24
	stw sp+1092, r25
	stw sp+1088, r26
	stw sp+1084, r27
	stw sp+1080, r28
	add r11, r6, r0
	lui r25, %hi(nml_unit)
	stw r25+%lo(nml_unit), r3
	lui r1, %hi(nml_reading)
	stw r1+%lo(nml_reading), r6
	lui r1, %hi(nml_flags)
	stw r1+%lo(nml_flags), r7
	lui r18, %hi(nml_status)
	stw r18+%lo(nml_status), r0
	lui r1, %hi(nml_ndim)
	stw r1+%lo(nml_ndim), r0
	lui r1, %hi(nml_err)
	stb r1+%lo(nml_err), r0
	addi r1, r0, 31
	slt r1, r5, r1
	sub r1, r0, r1
	xori r3, r5, 31
	and r1, r3, r1
	xori r12, r1, 31
	addi r22, r0, 1
	lui r14, %hi(nml_grp)
	addi r14, r14, %lo(nml_grp)
	blt r5, r22, .LBB110_3
.LBB110_1:
	add r13, r4, r0
	add r15, r0, r0
.LBB110_2:
	add r1, r13, r15
	ldbu r3, r1+0
	jal r31, toupper
	add r3, r15, r14
	stb r3+0, r1
	addi r15, r15, 1
	bne r12, r15, .LBB110_2
.LBB110_3:
	add r1, r12, r14
	stb r1+0, r0
	lui r26, %hi(fio_ibuf)
	beq r11, r0, .LBB110_65
.LBB110_4:
	lui r1, %hi(nml_nasg)
	stw sp+20, r1
	stw r1+%lo(nml_nasg), r0
	lui r1, %hi(nml_nval)
	stw sp+40, r1
	stw r1+%lo(nml_nval), r0
	lui r3, %hi(nml_grp)
	addi r3, r3, %lo(nml_grp)
	stw sp+44, r3
	jal r31, strlen
	stw sp+48, r1
	addi r11, r1, 2
	lui r1, %hi(nml_tlen)
	stw sp+36, r1
	stw r1+%lo(nml_tlen), r0
	lui r3, %hi(.L.str.10)
	addi r3, r3, %lo(.L.str.10)
	add r4, r0, r0
	jal r31, nml_text_add
	addi r1, sp, 56
	stw sp+28, r11
	add r3, r1, r11
	stw sp+24, r3
	addi r1, r1, 2
	stw sp+32, r1
	lui r13, %hi(.L.str.93)
	addi r13, r13, %lo(.L.str.93)
	addi r11, r0, 3
	addi r15, r0, 1023
	lui fp, %hi(fio_reading)
	addi r21, r0, 1022
	addi r23, r0, 32
	addi r24, r0, 2
                                        # implicit-def: $r20
	add r16, r0, r0
	stw sp+52, r18
	jal r0, .LBB110_7
.LBB110_5:
	add r20, r1, r0
	add r14, r1, r0
	beq r14, r0, .LBB110_7
.LBB110_6:
	addi r1, r0, 3
	bne r14, r1, .LBB110_63
.LBB110_7:
	ldw r3, r25+%lo(nml_unit)
	add r4, r13, r0
	add r5, r11, r0
	add r6, r11, r0
	jal r31, f77_rd_begin
	beq r1, r0, .LBB110_11
.LBB110_8:
	addi r14, r0, 1
	beq r1, r14, .LBB110_5
.LBB110_9:
	ldw r1, r18+%lo(nml_status)
	beq r1, r0, .LBB110_22
.LBB110_10:
	add r20, r0, r0
	bne r14, r0, .LBB110_6
	jal r0, .LBB110_7
.LBB110_11:
	addi r3, sp, 56
	add r4, r15, r0
	jal r31, f77_rd_s
	stb fp+%lo(fio_reading), r0
	stw r26+%lo(fio_ibuf), r0
	add r1, r21, r0
.LBB110_12:
	addi r3, sp, 56
	add r3, r3, r1
	ldbu r3, r3+0
	bne r3, r23, .LBB110_15
.LBB110_13:
	addi r3, r1, -1
	addi r4, r1, 1
	add r1, r3, r0
	bgtu r4, r24, .LBB110_12
.LBB110_14:
	add r27, r22, r0
	jal r0, .LBB110_16
.LBB110_15:
	addi r27, r1, 1
.LBB110_16:
	add r19, r22, r0
	add r28, r22, r0
	bne r16, r0, .LBB110_39
.LBB110_17:
	blt r27, r24, .LBB110_23
.LBB110_18:
	addi r1, r27, -1
	add r14, r0, r0
.LBB110_19:
	addi r3, sp, 56
	add r3, r3, r14
	ldbu r3, r3+1
	bne r3, r23, .LBB110_24
.LBB110_20:
	addi r14, r14, 1
	bne r1, r14, .LBB110_19
.LBB110_21:
	add r19, r0, r0
	add r28, r27, r0
	jal r0, .LBB110_61
.LBB110_22:
	stw r18+%lo(nml_status), r24
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
	add r20, r0, r0
	bne r14, r0, .LBB110_6
	jal r0, .LBB110_7
.LBB110_23:
	add r19, r0, r0
	add r28, r22, r0
	jal r0, .LBB110_61
.LBB110_24:
	addi r1, r0, 36
	beq r3, r1, .LBB110_26
.LBB110_25:
	addi r1, r0, 38
	bne r3, r1, .LBB110_59
.LBB110_26:
	addi r28, r14, 1
	addi r19, r0, 1
	ldw r1, sp+48
	blt r1, r19, .LBB110_31
.LBB110_27:
	ldw r1, sp+32
	add r12, r1, r14
	sub r1, r14, r27
	addi r16, r1, 2
	add r17, r0, r0
.LBB110_28:
	add r1, r16, r17
	beq r1, r0, .LBB110_60
.LBB110_29:
	add r1, r12, r17
	ldbu r3, r1+0
	jal r31, toupper
	ldw r3, sp+44
	add r3, r17, r3
	ldb r3, r3+0
	bne r1, r3, .LBB110_60
.LBB110_30:
	addi r17, r17, 1
	ldw r1, sp+48
	bne r1, r17, .LBB110_28
.LBB110_31:
	ldw r1, sp+28
	add r12, r1, r14
	bge r12, r27, .LBB110_37
.LBB110_32:
	ldw r1, sp+24
	add r1, r1, r14
	ldbu r16, r1+0
	add r3, r16, r0
	jal r31, isalnum
	bne r1, r0, .LBB110_60
.LBB110_33:
	addi r1, r16, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB110_60
.LBB110_34:
	addi r1, r0, 64
	beq r16, r1, .LBB110_60
.LBB110_35:
	addi r3, r0, 95
	add r19, r0, r0
	add r1, r0, r0
	add r14, r11, r0
	beq r16, r3, .LBB110_38
.LBB110_36:
	addi r19, r0, 1
.LBB110_37:
	add r28, r12, r0
	add r1, r19, r0
	add r14, r0, r0
.LBB110_38:
	beq r1, r0, .LBB110_62
.LBB110_39:
	sub r16, r27, r28
	ble r27, r28, .LBB110_58
.LBB110_40:
	addi r1, sp, 56
	add r17, r1, r28
	add r18, r0, r0
	add r1, r0, r0
	jal r0, .LBB110_42
.LBB110_41:
	addi r18, r18, 1
	beq r16, r18, .LBB110_58
.LBB110_42:
	add r12, r17, r18
	ldb r3, r12+0
	beq r1, r0, .LBB110_44
.LBB110_43:
	seq r3, r1, r3
	sub r3, r0, r3
	and r3, r1, r3
	xor r1, r1, r3
	addi r14, r0, 13
	addi r3, r0, 13
	bne r14, r3, .LBB110_56
	jal r0, .LBB110_41
.LBB110_44:
	andi r1, r3, 255
	addi r14, r0, 13
	addi r4, r0, 34
	beq r1, r4, .LBB110_48
.LBB110_45:
	addi r4, r0, 39
	beq r1, r4, .LBB110_48
.LBB110_46:
	addi r4, r0, 47
	bne r1, r4, .LBB110_49
.LBB110_47:
	add r3, r17, r0
	add r4, r18, r0
	jal r31, nml_text_add
	addi r14, r0, 1
	add r20, r0, r0
	jal r0, .LBB110_55
.LBB110_48:
	add r1, r3, r0
	addi r3, r0, 13
	bne r14, r3, .LBB110_56
	jal r0, .LBB110_41
.LBB110_49:
	addi r1, r0, -3
	and r1, r3, r1
	addi r3, r0, 36
	bne r1, r3, .LBB110_54
.LBB110_50:
	add r1, r28, r18
	addi r1, r1, 3
	bge r1, r27, .LBB110_54
.LBB110_51:
	ldbu r3, r12+1
	jal r31, toupper
	addi r3, r0, 69
	bne r1, r3, .LBB110_54
.LBB110_52:
	ldbu r3, r12+2
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB110_54
.LBB110_53:
	ldbu r3, r12+3
	jal r31, toupper
	addi r3, r0, 68
	beq r1, r3, .LBB110_47
.LBB110_54:
	add r14, r0, r0
.LBB110_55:
	add r1, r0, r0
	addi r3, r0, 13
	beq r14, r3, .LBB110_41
.LBB110_56:
	beq r14, r0, .LBB110_41
.LBB110_57:
	add r16, r19, r0
	ldw r18, sp+52
	bne r14, r0, .LBB110_6
	jal r0, .LBB110_7
.LBB110_58:
	addi r1, sp, 56
	add r3, r1, r28
	add r4, r16, r0
	jal r31, nml_text_add
	lui r3, %hi(.L.str.95)
	addi r3, r3, %lo(.L.str.95)
	addi r4, r0, 1
	jal r31, nml_text_add
	add r16, r19, r0
	add r14, r0, r0
	ldw r18, sp+52
	bne r14, r0, .LBB110_6
	jal r0, .LBB110_7
.LBB110_59:
	addi r28, r14, 1
.LBB110_60:
	add r19, r0, r0
.LBB110_61:
	add r1, r0, r0
	add r14, r11, r0
	bne r1, r0, .LBB110_39
.LBB110_62:
	add r16, r19, r0
	bne r14, r0, .LBB110_6
	jal r0, .LBB110_7
.LBB110_63:
	beq r20, r0, .LBB110_71
.LBB110_64:
	addi r1, r0, 1
	stw r18+%lo(nml_status), r1
	jal r0, .LBB110_70
.LBB110_65:
	lui r1, 2
	addi r1, r1, 1568
	sth sp+56, r1
	addi r11, sp, 56
	addi r3, r11, 2
	lui r4, %hi(nml_grp)
	addi r4, r4, %lo(nml_grp)
	add r5, r12, r0
	jal r31, memcpy
	addi r12, r12, 2
	ldw r3, r25+%lo(nml_unit)
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	add r3, r11, r0
	add r4, r12, r0
	jal r31, f77_wr_a
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB110_69
.LBB110_66:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB110_68
.LBB110_67:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB110_69
.LBB110_68:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB110_69:
	jal r31, fio_flush_line
	stw r26+%lo(fio_ibuf), r0
	lui r1, %hi(nml_width)
	addi r3, r0, 1
	stb r1+%lo(nml_width), r3
	lui r1, %hi(nml_line)
	addi r4, r0, 32
	stb r1+%lo(nml_line), r4
	lui r1, %hi(nml_len)
	stw r1+%lo(nml_len), r3
	lui r1, %hi(nml_npend)
	addi r3, r0, -1
	stw r1+%lo(nml_npend), r3
.LBB110_70:
	ldw r28, sp+1080
	ldw r27, sp+1084
	ldw r26, sp+1088
	ldw r25, sp+1092
	ldw r24, sp+1096
	ldw r23, sp+1100
	ldw r22, sp+1104
	ldw r21, sp+1108
	ldw r20, sp+1112
	ldw r19, sp+1116
	ldw r18, sp+1120
	ldw r17, sp+1124
	ldw r16, sp+1128
	ldw r15, sp+1132
	ldw r14, sp+1136
	ldw r13, sp+1140
	ldw r12, sp+1144
	ldw r11, sp+1148
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 1152
	jalr r0, r31, 0
.LBB110_71:
	ldw r22, sp+20
	stw r22+%lo(nml_nasg), r0
	ldw r1, sp+40
	stw r1+%lo(nml_nval), r0
	ldw r21, sp+36
	ldw r1, r21+%lo(nml_tlen)
	addi r15, r0, 1
	blt r1, r15, .LBB110_70
.LBB110_72:
	ldw r1, sp+52
	ldw r1, r1+%lo(nml_status)
	bne r1, r0, .LBB110_70
.LBB110_73:
	lui r16, %hi(nml_text)
	addi r17, r0, 44
	lui r18, %hi(nml_cval)
	lui r25, %hi(nml_val)
	addi r26, r0, 12
	addi r1, r0, 0
	stw sp+48, r1
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+44, r1
	lui r1, %hi(.L.str.98)
	addi r1, r1, %lo(.L.str.98)
	stw sp+32, r1
	addi r13, r0, 95
	lui r28, %hi(nml_err+95)
	add r12, r15, r0
	add fp, r0, r0
	add r27, r0, r0
.LBB110_74:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r27
	ldbu r3, r1+0
	beq r3, r17, .LBB110_77
.LBB110_75:
	bne r3, r23, .LBB110_84
.LBB110_76:
	addi r11, r27, 1
	jal r0, .LBB110_197
.LBB110_77:
	beq r12, r0, .LBB110_83
.LBB110_78:
	beq fp, r0, .LBB110_83
.LBB110_79:
	ldw r1, sp+40
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB110_82
.LBB110_80:
	ldw r3, sp+48
	seq r3, r1, r3
	slli r1, r1, 1
	sub r3, r0, r3
	xori r4, r1, 64
	and r3, r4, r3
	xor r1, r1, r3
	stw r18+%lo(nml_cval), r1
	ldw r3, r25+%lo(nml_val)
	mul r4, r1, r26
	jal r31, realloc
	beq r1, r0, .LBB110_210
.LBB110_81:
	stw r25+%lo(nml_val), r1
.LBB110_82:
	ldw r1, r25+%lo(nml_val)
	ldw r5, sp+40
	ldw r3, r5+%lo(nml_nval)
	mul r4, r3, r26
	add r1, r1, r4
	stw r1+0, r27
	stw r1+4, r0
	stw r1+8, r15
	addi r1, r3, 1
	stw r5+%lo(nml_nval), r1
	ldw r1, fp+68
	addi r1, r1, 1
	stw fp+68, r1
.LBB110_83:
	addi r11, r27, 1
	jal r0, .LBB110_196
.LBB110_84:
	jal r31, isalpha
                                        # implicit-def: $r11
	add r5, r15, r0
	beq r1, r0, .LBB110_105
.LBB110_85:
	ldw r1, r21+%lo(nml_tlen)
	add r19, r27, r0
	bge r27, r1, .LBB110_92
.LBB110_86:
	add r19, r27, r0
	jal r0, .LBB110_88
.LBB110_87:
	addi r19, r19, 1
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB110_92
.LBB110_88:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r19
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB110_87
.LBB110_89:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB110_87
.LBB110_90:
	beq r14, r13, .LBB110_87
.LBB110_91:
	addi r1, r0, 64
	beq r14, r1, .LBB110_87
.LBB110_92:
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB110_102
.LBB110_93:
	ldw r3, r16+%lo(nml_text)
.LBB110_94:
	add r4, r3, r19
	ldbu r4, r4+0
	bne r4, r23, .LBB110_97
.LBB110_95:
	addi r19, r19, 1
	bne r1, r19, .LBB110_94
.LBB110_96:
	add r4, r1, r0
	jal r0, .LBB110_103
.LBB110_97:
	addi r5, r0, 40
	bne r4, r5, .LBB110_102
.LBB110_98:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r19, r1, .LBB110_105
.LBB110_99:
	add r5, r3, r19
	ldbu r6, r5+0
	addi r4, r19, 1
	addi r7, r0, 41
	beq r6, r7, .LBB110_205
.LBB110_100:
	add r19, r4, r0
	blt r4, r1, .LBB110_99
.LBB110_101:
                                        # implicit-def: $r11
	add r5, r15, r0
	jal r0, .LBB110_105
.LBB110_102:
	add r4, r19, r0
.LBB110_103:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r4, r1, .LBB110_105
.LBB110_104:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r4
	ldbu r1, r1+0
	addi r3, r0, 61
	sne r5, r1, r3
	addi r11, r4, 1
.LBB110_105:
	beq r5, r0, .LBB110_115
.LBB110_106:
	beq fp, r0, .LBB110_162
.LBB110_107:
	ldw r1, r21+%lo(nml_tlen)
	add r11, r27, r0
	bge r27, r1, .LBB110_110
.LBB110_108:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB110_110
.LBB110_109:
	addi r11, r11, 1
	ldw r1, r21+%lo(nml_tlen)
	blt r11, r1, .LBB110_108
.LBB110_110:
	ble r11, r27, .LBB110_129
.LBB110_111:
	ldw r1, r21+%lo(nml_tlen)
	bge r11, r1, .LBB110_129
.LBB110_112:
	ldw r1, r16+%lo(nml_text)
	add r3, r1, r11
	ldbu r3, r3+0
	addi r4, r0, 42
	bne r3, r4, .LBB110_129
.LBB110_113:
	add r3, r1, r27
	jal r31, atoi
	ble r1, r0, .LBB110_202
.LBB110_114:
	add r14, r1, r0
	addi r27, r11, 1
	jal r0, .LBB110_130
.LBB110_115:
	ldw r4, r22+%lo(nml_nasg)
	lui r1, %hi(nml_casg)
	ldw r3, r1+%lo(nml_casg)
	lui r14, %hi(nml_asg)
	blt r4, r3, .LBB110_118
.LBB110_116:
	ldw r4, sp+48
	seq r4, r3, r4
	slli r3, r3, 1
	sub r4, r0, r4
	xori r5, r3, 16
	and r4, r5, r4
	xor r4, r3, r4
	stw r1+%lo(nml_casg), r4
	ldw r3, r14+%lo(nml_asg)
	addi r1, r0, 76
	mul r4, r4, r1
	jal r31, realloc
	beq r1, r0, .LBB110_210
.LBB110_117:
	stw r14+%lo(nml_asg), r1
.LBB110_118:
	ldw r1, r14+%lo(nml_asg)
	ldw r3, r22+%lo(nml_nasg)
	addi r4, r3, 1
	stw r22+%lo(nml_nasg), r4
	addi r4, r0, 76
	mul r3, r3, r4
	add fp, r1, r3
	ldw r1, r21+%lo(nml_tlen)
	bge r27, r1, .LBB110_128
.LBB110_119:
	add r20, r0, r0
.LBB110_120:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r27
	add r1, r1, r20
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	addi r3, r0, 1
	beq r1, r0, .LBB110_124
.LBB110_121:
	beq r3, r0, .LBB110_165
.LBB110_122:
	addi r1, r0, 30
	bgtu r20, r1, .LBB110_165
.LBB110_123:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r27
	add r1, r1, r20
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r20, 1
	add r4, fp, r20
	stb r4+0, r1
	ldw r1, r21+%lo(nml_tlen)
	add r19, r27, r3
	add r20, r3, r0
	blt r19, r1, .LBB110_120
	jal r0, .LBB110_166
.LBB110_124:
	addi r1, r14, -35
	addi r4, r0, 2
	bltu r1, r4, .LBB110_121
.LBB110_125:
	addi r1, r0, 64
	beq r14, r1, .LBB110_121
.LBB110_126:
	beq r14, r13, .LBB110_121
.LBB110_127:
	add r3, r0, r0
	jal r0, .LBB110_121
.LBB110_128:
	add r19, r27, r0
	add r20, r0, r0
	jal r0, .LBB110_167
.LBB110_129:
	add r14, r15, r0
.LBB110_130:
	add r12, r0, r0
	add r1, r0, r0
	add r11, r27, r0
	jal r0, .LBB110_135
.LBB110_131:
	add r1, r0, r0
.LBB110_132:
	add r4, r11, r0
.LBB110_133:
	addi r11, r4, 1
	add r3, r0, r0
.LBB110_134:
	bne r3, r0, .LBB110_158
.LBB110_135:
	ldw r3, r21+%lo(nml_tlen)
	bge r11, r3, .LBB110_158
.LBB110_136:
	ldw r6, r16+%lo(nml_text)
	add r4, r6, r11
	ldb r5, r4+0
	beq r1, r0, .LBB110_140
.LBB110_137:
	bne r1, r5, .LBB110_132
.LBB110_138:
	addi r4, r11, 1
	bge r4, r3, .LBB110_131
.LBB110_139:
	add r3, r6, r4
	ldb r3, r3+0
	bne r1, r3, .LBB110_131
	jal r0, .LBB110_133
.LBB110_140:
	andi r19, r5, 255
	addi r1, r19, -32
	addi r6, r0, 9
	bgtu r1, r6, .LBB110_150
.LBB110_141:
	slli r1, r1, 2
	lui r6, %hi(.LJTI110_0)
	addi r6, r6, %lo(.LJTI110_0)
	add r1, r6, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB110_142:
	addi r11, r11, 1
	add r3, r0, r0
	add r1, r5, r0
	ldw r21, sp+36
	jal r0, .LBB110_134
.LBB110_143:
	addi r1, r0, 2
	blt r11, r1, .LBB110_150
.LBB110_144:
	addi r20, r11, 1
	bge r20, r3, .LBB110_150
.LBB110_145:
	ldbu r3, r4+-1
	jal r31, toupper
	addi r1, r1, -70
	addi r3, r0, -2
	bltu r1, r3, .LBB110_150
.LBB110_146:
	ldw r1, r16+%lo(nml_text)
	addi r21, r11, -2
	add r1, r1, r21
	ldbu r3, r1+0
	jal r31, isdigit
	bne r1, r0, .LBB110_148
.LBB110_147:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r21
	ldbu r1, r1+0
	addi r3, r0, 46
	bne r1, r3, .LBB110_150
.LBB110_148:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB110_150
.LBB110_149:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	addi r3, r0, 43
	stb r1+0, r3
	jal r0, .LBB110_155
.LBB110_150:
	bgt r12, r0, .LBB110_155
.LBB110_151:
	addi r3, r0, 1
	ldw r21, sp+36
	beq r19, r23, .LBB110_157
.LBB110_152:
	beq r19, r17, .LBB110_157
	jal r0, .LBB110_156
.LBB110_153:
	addi r12, r12, 1
	jal r0, .LBB110_155
.LBB110_154:
	addi r12, r12, -1
.LBB110_155:
	ldw r21, sp+36
.LBB110_156:
	addi r11, r11, 1
	add r3, r0, r0
.LBB110_157:
	add r1, r0, r0
	jal r0, .LBB110_134
.LBB110_158:
	ldw r1, sp+40
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB110_161
.LBB110_159:
	ldw r3, sp+48
	seq r3, r1, r3
	slli r1, r1, 1
	sub r3, r0, r3
	xori r4, r1, 64
	and r3, r4, r3
	xor r1, r1, r3
	stw r18+%lo(nml_cval), r1
	ldw r3, r25+%lo(nml_val)
	mul r4, r1, r26
	jal r31, realloc
	beq r1, r0, .LBB110_210
.LBB110_160:
	stw r25+%lo(nml_val), r1
.LBB110_161:
	ldw r1, r25+%lo(nml_val)
	ldw r5, sp+40
	ldw r3, r5+%lo(nml_nval)
	mul r4, r3, r26
	add r1, r1, r4
	stw r1+0, r27
	sub r4, r11, r27
	stw r1+4, r4
	stw r1+8, r14
	addi r1, r3, 1
	stw r5+%lo(nml_nval), r1
	ldw r1, fp+68
	addi r1, r1, 1
	stw fp+68, r1
	add r12, r0, r0
	add r14, r0, r0
	jal r0, .LBB110_198
.LBB110_162:
	ldw r3, sp+52
	ldw r1, r3+%lo(nml_status)
	bne r1, r0, .LBB110_164
.LBB110_163:
	stw r3+%lo(nml_status), r24
	ldw r3, sp+44
	ldw r4, sp+32
	add r5, r13, r0
	jal r31, strncpy
	stb r28+%lo(nml_err+95), r0
.LBB110_164:
	add r11, r27, r0
	add fp, r0, r0
	add r14, r15, r0
	jal r0, .LBB110_198
.LBB110_165:
	add r19, r27, r20
	jal r0, .LBB110_167
.LBB110_166:
	add r20, r3, r0
.LBB110_167:
	add r1, fp, r20
	stb r1+0, r0
	jal r0, .LBB110_169
.LBB110_168:
	addi r19, r19, 1
.LBB110_169:
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB110_174
.LBB110_170:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r19
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB110_168
.LBB110_171:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB110_168
.LBB110_172:
	beq r14, r13, .LBB110_168
.LBB110_173:
	addi r1, r0, 64
	beq r14, r1, .LBB110_168
.LBB110_174:
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB110_179
.LBB110_175:
	ldw r3, r16+%lo(nml_text)
.LBB110_176:
	add r4, r3, r19
	ldbu r4, r4+0
	bne r4, r23, .LBB110_179
.LBB110_177:
	addi r19, r19, 1
	bne r1, r19, .LBB110_176
.LBB110_178:
	add r19, r1, r0
.LBB110_179:
	stw fp+32, r0
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r19
	ldbu r1, r1+0
	addi r3, r0, 40
	bne r1, r3, .LBB110_195
.LBB110_180:
	addi r20, r19, 1
	addi r19, fp, 36
	jal r0, .LBB110_183
.LBB110_181:
	ldw r3, sp+52
	ldw r1, r3+%lo(nml_status)
	addi r14, r0, 1
	beq r1, r0, .LBB110_193
.LBB110_182:
	addi r1, r0, 11
	bne r14, r1, .LBB110_194
.LBB110_183:
	ldw r1, r16+%lo(nml_text)
	addi r20, r20, -1
.LBB110_184:
	add r3, r1, r20
	ldbu r3, r3+1
	addi r20, r20, 1
	beq r3, r23, .LBB110_184
.LBB110_185:
	add r3, r1, r20
	addi r4, sp, 56
	addi r14, r0, 10
	add r5, r14, r0
	jal r31, strtol
	ldw r4, sp+56
	ldw r3, r16+%lo(nml_text)
	add r5, r3, r20
	beq r5, r4, .LBB110_181
.LBB110_186:
	ldw r5, fp+32
	addi r6, r0, 6
	bgt r5, r6, .LBB110_188
.LBB110_187:
	addi r6, r5, 1
	stw fp+32, r6
	slli r5, r5, 2
	add r5, r19, r5
	stw r5+0, r1
.LBB110_188:
	sub r20, r4, r3
.LBB110_189:
	add r1, r3, r20
	ldbu r1, r1+0
	bne r1, r23, .LBB110_191
.LBB110_190:
	addi r20, r20, 1
	jal r0, .LBB110_189
.LBB110_191:
	bne r1, r17, .LBB110_182
.LBB110_192:
	addi r20, r20, 1
	addi r14, r0, 11
	jal r0, .LBB110_182
.LBB110_193:
	addi r1, r0, 2
	stw r3+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.97)
	addi r4, r4, %lo(.L.str.97)
	addi r5, r0, 95
	jal r31, strncpy
	stb r28+%lo(nml_err+95), r0
	jal r0, .LBB110_182
.LBB110_194:
	addi r1, r0, 10
	bne r14, r1, .LBB110_201
.LBB110_195:
	ldw r1, sp+40
	ldw r1, r1+%lo(nml_nval)
	stw fp+64, r1
	stw fp+68, r0
	stw fp+72, r0
.LBB110_196:
	add r12, r15, r0
.LBB110_197:
	add r14, r24, r0
.LBB110_198:
	ori  r1, r14, 2
	bne r1, r24, .LBB110_70
.LBB110_199:
	ldw r1, r21+%lo(nml_tlen)
	bge r11, r1, .LBB110_70
.LBB110_200:
	ldw r1, sp+52
	ldw r1, r1+%lo(nml_status)
	add r27, r11, r0
	beq r1, r0, .LBB110_74
	jal r0, .LBB110_70
.LBB110_201:
	add r11, r27, r0
	jal r0, .LBB110_198
.LBB110_202:
	ldw r3, sp+52
	ldw r1, r3+%lo(nml_status)
	bne r1, r0, .LBB110_204
.LBB110_203:
	addi r1, r0, 2
	stw r3+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.99)
	addi r4, r4, %lo(.L.str.99)
	addi r5, r0, 95
	jal r31, strncpy
	stb r28+%lo(nml_err+95), r0
.LBB110_204:
	add r11, r27, r0
	add r14, r15, r0
	jal r0, .LBB110_198
.LBB110_205:
	bge r4, r1, .LBB110_103
.LBB110_206:
	add r3, r0, r0
.LBB110_207:
	add r4, r5, r3
	ldbu r4, r4+1
	bne r4, r23, .LBB110_209
.LBB110_208:
	addi r3, r3, 1
	add r4, r19, r3
	addi r4, r4, 1
	blt r4, r1, .LBB110_207
	jal r0, .LBB110_103
.LBB110_209:
	add r3, r19, r3
	addi r4, r3, 1
	jal r0, .LBB110_103
.LBB110_210:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end110:
	.size	f77_nml_begin, .Lfunc_end110-f77_nml_begin
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI110_0,@object
.LJTI110_0:
	.word	.LBB110_143
	.word	.LBB110_150
	.word	.LBB110_142
	.word	.LBB110_150
	.word	.LBB110_150
	.word	.LBB110_150
	.word	.LBB110_150
	.word	.LBB110_142
	.word	.LBB110_153
	.word	.LBB110_154
	.size	.LJTI110_0, 40
                                        # -- End function
	.text
	.globl	f77_nml_dim                     # -- Begin function f77_nml_dim
	.p2align	2
	.type	f77_nml_dim,@function
f77_nml_dim:                            # @f77_nml_dim
# %bb.0:
	lui r1, %hi(nml_ndim)
	ldw r5, r1+%lo(nml_ndim)
	addi r6, r0, 6
	bgt r5, r6, .LBB111_2
.LBB111_1:
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
	stw r1+%lo(nml_ndim), r3
.LBB111_2:
	jalr r0, r31, 0
.Lfunc_end111:
	.size	f77_nml_dim, .Lfunc_end111-f77_nml_dim
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
	add r22, r6, r0
	add r14, r5, r0
	add r15, r3, r0
	addi r1, r0, 31
	slt r1, r4, r1
	sub r1, r0, r1
	xori r3, r4, 31
	and r1, r3, r1
	xori r16, r1, 31
	addi r17, r0, 1
	addi r11, sp, 248
	blt r4, r17, .LBB112_3
.LBB112_1:
	add r13, r16, r0
.LBB112_2:
	ldbu r3, r15+0
	jal r31, toupper
	stb r11+0, r1
	addi r13, r13, -1
	addi r11, r11, 1
	addi r15, r15, 1
	bne r13, r0, .LBB112_2
.LBB112_3:
	addi r1, sp, 248
	add r1, r1, r16
	stb r1+0, r0
	addi r1, r0, 6
	ldw r11, sp+88
	stw sp+108, r1
	beq r14, r1, .LBB112_6
.LBB112_4:
	addi r1, r0, 2
	beq r14, r1, .LBB112_7
.LBB112_5:
	bne r14, r17, .LBB112_8
.LBB112_6:
	addi r1, r0, 0
	sgt r1, r12, r1
	sub r1, r0, r1
	xori r3, r12, 4
	and r1, r3, r1
	xori r28, r1, 4
	jal r0, .LBB112_11
.LBB112_7:
	addi r28, r0, 4
	jal r0, .LBB112_11
.LBB112_8:
	addi r3, r14, -3
	bgeu r3, r1, .LBB112_10
.LBB112_9:
	addi r28, r0, 8
	jal r0, .LBB112_11
.LBB112_10:
	addi r1, r0, 5
	seq r1, r14, r1
	sub r1, r0, r1
	xori r3, r12, 16
	and r1, r3, r1
	xor r28, r12, r1
.LBB112_11:
	lui r1, %hi(nml_reading)
	ldw r1, r1+%lo(nml_reading)
	lui r3, %hi(fio_listed)
	stw sp+100, r3
	lui r3, %hi(fio_desc)
	stw sp+96, r3
	lui r3, %hi(fio_ibuf)
	stw sp+104, r3
	lui r3, %hi(fio_rep)
	stw sp+92, r3
	beq r1, r0, .LBB112_14
.LBB112_12:
	stw sp+56, r17
	lui r24, %hi(nml_status)
	ldw r1, r24+%lo(nml_status)
	lui r3, %hi(nml_ndim)
	stw sp+60, r3
	beq r1, r0, .LBB112_72
.LBB112_13:
	ldw r1, sp+60
	jal r0, .LBB112_71
.LBB112_14:
	addi r15, sp, 144
	addi r4, sp, 248
	add r3, r15, r0
	add r5, r16, r0
	jal r31, memcpy
	add r1, r15, r16
	addi r3, r0, 61
	stb r1+0, r3
	lui r21, %hi(nml_npend)
	ldw r1, r21+%lo(nml_npend)
	lui r15, %hi(nml_pend)
	addi r15, r15, %lo(nml_pend)
	blt r1, r0, .LBB112_16
.LBB112_15:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	addi r3, r0, 44
	stb r1+0, r3
	add r3, r15, r0
	jal r31, nml_put
	addi r1, r0, -1
	stw r21+%lo(nml_npend), r1
.LBB112_16:
	addi r4, r16, 1
	addi r3, sp, 144
	jal r31, nml_put
	addi r27, r0, 1
	blt r11, r27, .LBB112_70
.LBB112_17:
	addi r16, r0, 5
	seq r3, r14, r16
	addi r1, r0, -2
	and r17, r14, r1
	stw sp+68, r3
	sub r1, r0, r3
	andi r1, r1, 16
	addi r1, r1, 33
	stw sp+60, r1
	addi fp, r0, 3
	addi r19, r0, 23
	addi r24, r0, 44
	addi r25, r0, -1
	addi r20, r0, 2
	addi r26, r0, 15
	lui r1, %hi(.L.str.20)
	addi r1, r1, %lo(.L.str.20)
	stw sp+76, r1
	addi r1, r0, 11
	stw sp+84, r1
	add r13, r22, r0
	add r18, r0, r0
	stw sp+72, r16
	stw sp+64, r17
	stw sp+56, r19
	stw sp+80, r24
	stw sp+52, r25
	stw sp+48, r26
	jal r0, .LBB112_21
.LBB112_18:
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r4, sp, 184
	ldw r23, sp+84
.LBB112_19:
	add r5, r23, r0
	jal r31, memcpy
	stw r21+%lo(nml_npend), r23
.LBB112_20:
	addi r18, r18, 1
	add r13, r13, r28
	beq r18, r11, .LBB112_70
.LBB112_21:
	mul r1, r18, r28
	add r1, r22, r1
	beq r14, fp, .LBB112_30
.LBB112_22:
	beq r14, r20, .LBB112_27
.LBB112_23:
	bne r14, r27, .LBB112_33
.LBB112_24:
	beq r12, r20, .LBB112_43
.LBB112_25:
	addi r3, r0, 1
	bne r12, r3, .LBB112_44
.LBB112_26:
	ldbu r23, r1+0
	jal r0, .LBB112_45
.LBB112_27:
	ldw r1, r1+0
	fcvt.d.s r6, r1
	addi r3, sp, 184
	add r5, r6, r0
	add r6, r7, r0
	add r4, r0, r0
	jal r31, nml_real
	ldw r1, r21+%lo(nml_npend)
	blt r1, r0, .LBB112_29
.LBB112_28:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r24
	add r3, r15, r0
	jal r31, nml_put
	stw r21+%lo(nml_npend), r25
.LBB112_29:
	addi r4, sp, 184
	add r3, r15, r0
	add r5, r26, r0
	jal r31, memcpy
	stw r21+%lo(nml_npend), r26
	jal r0, .LBB112_20
.LBB112_30:
	ldw r6, r1+4
	ldw r5, r1+0
	addi r3, sp, 184
	add r4, r27, r0
	jal r31, nml_real
	ldw r1, r21+%lo(nml_npend)
	blt r1, r0, .LBB112_32
.LBB112_31:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r24
	add r3, r15, r0
	jal r31, nml_put
	stw r21+%lo(nml_npend), r25
.LBB112_32:
	addi r4, sp, 184
	add r3, r15, r0
	add r5, r19, r0
	jal r31, memcpy
	stw r21+%lo(nml_npend), r19
	jal r0, .LBB112_20
.LBB112_33:
	addi r3, r0, 4
	bne r17, r3, .LBB112_37
.LBB112_34:
	bne r14, r16, .LBB112_51
.LBB112_35:
	ldw r7, r1+4
	ldw r6, r1+0
	beq r14, r16, .LBB112_52
.LBB112_36:
	ldw r1, r1+4
	fcvt.d.s r16, r1
	jal r0, .LBB112_53
.LBB112_37:
	ldw r3, sp+108
	bne r14, r3, .LBB112_58
.LBB112_38:
	beq r12, r20, .LBB112_68
.LBB112_39:
	addi r3, r0, 1
	bne r12, r3, .LBB112_69
.LBB112_40:
	ldbu r11, r1+0
	ldw r1, r21+%lo(nml_npend)
	blt r1, r0, .LBB112_42
.LBB112_41:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r24
	add r3, r15, r0
	jal r31, nml_put
	stw r21+%lo(nml_npend), r25
.LBB112_42:
	addi r1, r0, 0
	seq r1, r11, r1
	lui r3, %hi(.L.str.21)
	addi r3, r3, %lo(.L.str.21)
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r4, r3, r1
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r23, r0, 1
	jal r0, .LBB112_67
.LBB112_43:
	ldh r23, r1+0
	jal r0, .LBB112_45
.LBB112_44:
	ldw r23, r1+0
.LBB112_45:
	addi r3, sp, 184
	ldw r4, sp+84
	add r5, r27, r0
	ldw r6, sp+76
	add r7, r25, r0
	jal r31, f77_wr_begin_int
	add r3, r23, r0
	jal r31, f77_wr_i
	ldw r1, sp+100
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB112_49
.LBB112_46:
	ldw r1, sp+96
	ldw r1, r1+%lo(fio_desc)
	beq r1, r0, .LBB112_48
.LBB112_47:
	ldw r1, sp+92
	ldw r1, r1+%lo(fio_rep)
	bgt r1, r0, .LBB112_49
.LBB112_48:
	lui r11, %hi(fio_draining)
	addi r1, r0, 1
	stb r11+%lo(fio_draining), r1
	ldw r1, sp+96
	stw r1+%lo(fio_desc), r0
	ldw r1, sp+92
	stw r1+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
	ldw r11, sp+88
.LBB112_49:
	jal r31, fio_flush_line
	ldw r1, sp+104
	stw r1+%lo(fio_ibuf), r0
	stb sp+195, r0
	ldw r1, r21+%lo(nml_npend)
	blt r1, r0, .LBB112_18
.LBB112_50:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r24
	add r3, r15, r0
	jal r31, nml_put
	stw r21+%lo(nml_npend), r25
	jal r0, .LBB112_18
.LBB112_51:
	ldw r3, r1+0
	fcvt.d.s r6, r3
	bne r14, r16, .LBB112_36
.LBB112_52:
	ldw r17, r1+12
	ldw r16, r1+8
.LBB112_53:
	addi r1, r0, 40
	stb sp+280, r1
	addi r23, sp, 112
	add r3, r23, r0
	add r5, r6, r0
	add r6, r7, r0
	ldw r19, sp+68
	add r4, r19, r0
	jal r31, nml_real
	addi r24, sp, 280
	addi r25, r0, 1
	add r3, r24, r0
	add r4, r25, r0
	add r5, r23, r0
	jal r31, nml_trim_cat
	addi r26, r1, 1
	add r1, r24, r1
	ldw r3, sp+80
	stb r1+0, r3
	add r3, r23, r0
	add r5, r16, r0
	add r6, r17, r0
	add r4, r19, r0
	jal r31, nml_real
	add r3, r24, r0
	add r4, r26, r0
	add r5, r23, r0
	jal r31, nml_trim_cat
	addi r23, r1, 1
	add r1, r24, r1
	addi r3, r0, 41
	stb r1+0, r3
	ldw r16, sp+60
	sub r24, r16, r23
	blt r24, r25, .LBB112_55
.LBB112_54:
	addi r25, sp, 280
	add r3, r25, r24
	add r4, r25, r0
	add r5, r23, r0
	jal r31, memmove
	addi r4, r0, 32
	add r3, r25, r0
	add r5, r24, r0
	jal r31, memset
	add r23, r16, r0
.LBB112_55:
	ldw r17, sp+64
	ldw r19, sp+56
	ldw r25, sp+52
	ldw r26, sp+48
	ldw r1, r21+%lo(nml_npend)
	ldw r16, sp+72
	ldw r24, sp+80
	blt r1, r0, .LBB112_57
.LBB112_56:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r24
	add r3, r15, r0
	jal r31, nml_put
	stw r21+%lo(nml_npend), r25
.LBB112_57:
	addi r1, r0, 1022
	slt r1, r23, r1
	sub r1, r0, r1
	xori r3, r23, 1022
	and r1, r3, r1
	xori r23, r1, 1022
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r4, sp, 280
	jal r0, .LBB112_19
.LBB112_58:
	addi r1, r0, 39
	stb sp+280, r1
	add r11, r27, r0
	blt r12, r27, .LBB112_64
.LBB112_59:
	addi r11, r0, 1
	add r3, r0, r0
	jal r0, .LBB112_62
.LBB112_60:
	add r11, r4, r0
	addi r3, r3, 1
	bge r3, r12, .LBB112_64
.LBB112_61:
	addi r4, r0, 1021
	bge r11, r4, .LBB112_64
.LBB112_62:
	add r4, r13, r3
	ldbu r5, r4+0
	addi r4, r11, 1
	addi r6, sp, 280
	add r6, r6, r11
	stb r6+0, r5
	bne r5, r1, .LBB112_60
.LBB112_63:
	addi r11, r11, 2
	addi r5, sp, 280
	add r4, r5, r4
	stb r4+0, r1
	addi r3, r3, 1
	blt r3, r12, .LBB112_61
.LBB112_64:
	addi r3, sp, 280
	add r3, r3, r11
	stb r3+0, r1
	ldw r1, r21+%lo(nml_npend)
	blt r1, r0, .LBB112_66
.LBB112_65:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r24
	add r3, r15, r0
	jal r31, nml_put
	stw r21+%lo(nml_npend), r25
.LBB112_66:
	addi r1, r0, 1021
	slt r1, r11, r1
	sub r1, r0, r1
	xori r3, r11, 1021
	and r1, r3, r1
	xori r1, r1, 1021
	addi r23, r1, 1
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r4, sp, 280
.LBB112_67:
	add r5, r23, r0
	jal r31, memcpy
	stw r21+%lo(nml_npend), r23
	ldw r11, sp+88
	jal r0, .LBB112_20
.LBB112_68:
	ldh r11, r1+0
	ldw r1, r21+%lo(nml_npend)
	bge r1, r0, .LBB112_41
	jal r0, .LBB112_42
.LBB112_69:
	ldw r11, r1+0
	ldw r1, r21+%lo(nml_npend)
	bge r1, r0, .LBB112_41
	jal r0, .LBB112_42
.LBB112_70:
	lui r1, %hi(nml_ndim)
.LBB112_71:
	stw r1+%lo(nml_ndim), r0
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
.LBB112_72:
	lui r1, %hi(nml_nasg)
	stw sp+76, r1
	ldw r1, r1+%lo(nml_nasg)
	ldw r3, sp+56
	blt r1, r3, .LBB112_13
.LBB112_73:
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB112_13
.LBB112_74:
	addi r1, r0, 36
	stw sp+80, r1
	lui r1, %hi(nml_asg)
	stw sp+72, r1
	addi r1, r0, 76
	stw sp+68, r1
	addi r1, r0, 13
	stw sp+64, r1
	lui r1, %hi(nml_lo)
	addi r1, r1, %lo(nml_lo)
	stw sp+44, r1
	addi r17, r0, 2
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+40, r1
	lui r1, %hi(.L.str.24)
	addi r1, r1, %lo(.L.str.24)
	stw sp+36, r1
	addi r1, r0, 95
	stw sp+32, r1
	lui r1, %hi(nml_err+95)
	stw sp+84, r1
	lui r1, %hi(nml_ext)
	addi r1, r1, %lo(nml_ext)
	stw sp+52, r1
	addi r1, r0, 0
	stw sp+48, r1
	add r26, r0, r0
.LBB112_75:
	ldw r1, sp+72
	ldw r13, r1+%lo(nml_asg)
	ldw r1, sp+68
	mul r1, r26, r1
	add r18, r13, r1
	addi r4, sp, 248
	add r3, r18, r0
	jal r31, strcmp
	ldw r11, sp+64
	beq r1, r0, .LBB112_79
.LBB112_76:
	addi r1, r0, 11
	beq r11, r1, .LBB112_13
.LBB112_77:
	addi r26, r26, 1
	ldw r1, sp+76
	ldw r1, r1+%lo(nml_nasg)
	bge r26, r1, .LBB112_13
.LBB112_78:
	ldw r1, r24+%lo(nml_status)
	ldw r3, sp+80
	addi r3, r3, 76
	stw sp+80, r3
	beq r1, r0, .LBB112_75
	jal r0, .LBB112_13
.LBB112_79:
	ldw r20, sp+56
	stw r18+72, r20
	ldw r1, r18+32
	blt r1, r20, .LBB112_90
.LBB112_80:
	ldw r3, sp+60
	ldw r3, r3+%lo(nml_ndim)
	bne r1, r3, .LBB112_91
.LBB112_81:
	ldw r1, r18+32
	ldw fp, sp+52
	blt r1, r20, .LBB112_94
.LBB112_82:
	ldw r1, sp+80
	add r11, r13, r1
	add r13, r0, r0
	add r15, r0, r0
	add r19, r20, r0
	add r16, r0, r0
	ldw r21, sp+44
	ldw r23, sp+40
	ldw r25, sp+36
	ldw r27, sp+32
	jal r0, .LBB112_85
.LBB112_83:
	mul r1, r1, r19
	add r16, r1, r16
	mul r19, r3, r19
	add r1, r20, r0
	beq r1, r0, .LBB112_95
.LBB112_84:
	addi r15, r15, 1
	ldw r1, r18+32
	addi r13, r13, 4
	bge r15, r1, .LBB112_95
.LBB112_85:
	add r1, r11, r13
	ldw r1, r1+0
	add r3, r13, r21
	ldw r3, r3+0
	sub r1, r1, r3
	blt r1, r0, .LBB112_87
.LBB112_86:
	add r3, r13, fp
	ldw r3, r3+0
	blt r1, r3, .LBB112_83
.LBB112_87:
	ldw r1, r24+%lo(nml_status)
	beq r1, r0, .LBB112_89
.LBB112_88:
	add r1, r0, r0
	bne r1, r0, .LBB112_84
	jal r0, .LBB112_95
.LBB112_89:
	stw r24+%lo(nml_status), r17
	add r3, r23, r0
	add r4, r25, r0
	add r5, r27, r0
	jal r31, strncpy
	ldw r1, sp+84
	stb r1+%lo(nml_err+95), r0
	add r1, r0, r0
	bne r1, r0, .LBB112_84
	jal r0, .LBB112_95
.LBB112_90:
	add r16, r0, r0
	jal r0, .LBB112_96
.LBB112_91:
	ldw r1, r24+%lo(nml_status)
	addi r11, r0, 11
	bne r1, r0, .LBB112_93
.LBB112_92:
	stw r24+%lo(nml_status), r17
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+84
	stb r1+%lo(nml_err+95), r0
.LBB112_93:
	add r1, r0, r0
	add r16, r0, r0
	beq r1, r0, .LBB112_76
	jal r0, .LBB112_96
.LBB112_94:
	add r16, r0, r0
.LBB112_95:
	ldw r1, r24+%lo(nml_status)
	ldw r3, sp+48
	seq r1, r1, r3
	addi r3, r1, -1
	andi r11, r3, 11
	beq r1, r0, .LBB112_76
.LBB112_96:
	ldw r1, r18+68
	addi r23, r0, 1
	blt r1, r23, .LBB112_156
.LBB112_97:
	ldw r1, r24+%lo(nml_status)
	add r11, r0, r0
	bne r1, r0, .LBB112_76
.LBB112_98:
	ldw fp, r18+64
.LBB112_99:
	lui r13, %hi(nml_val)
	ldw r1, r13+%lo(nml_val)
	addi r3, r0, 12
	mul r25, fp, r3
	add r3, r1, r25
	ldw r1, r3+8
	blt r1, r23, .LBB112_154
.LBB112_100:
	ldw r5, sp+88
	bge r16, r5, .LBB112_152
.LBB112_101:
	xor r1, r16, r5
	sgt r4, r16, r5
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r5, r1
	sub r27, r1, r16
	mul r1, r28, r16
	add r15, r22, r1
	add r11, r0, r0
.LBB112_102:
	ldw r1, r3+4
	blt r1, r23, .LBB112_150
.LBB112_103:
	mul r4, r16, r28
	add r19, r22, r4
	lui r4, %hi(nml_text)
	ldw r4, r4+%lo(nml_text)
	ldw r3, r3+0
	add r4, r4, r3
	addi r3, r0, 7
	beq r14, r3, .LBB112_114
.LBB112_104:
	ldw r3, sp+108
	bne r14, r3, .LBB112_126
.LBB112_105:
	ldbu r3, r4+0
	addi r5, r0, 46
	seq r3, r3, r5
	ble r1, r3, .LBB112_108
.LBB112_106:
	add r20, r4, r3
	ldbu r3, r20+0
	jal r31, toupper
	addi r4, r0, 84
	add r3, r23, r0
	beq r1, r4, .LBB112_111
.LBB112_107:
	ldbu r3, r20+0
	jal r31, toupper
	addi r3, r0, 70
	beq r1, r3, .LBB112_110
.LBB112_108:
	ldw r1, r24+%lo(nml_status)
	add r3, r0, r0
	bne r1, r0, .LBB112_111
.LBB112_109:
	stw r24+%lo(nml_status), r17
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.103)
	addi r4, r4, %lo(.L.str.103)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+84
	stb r1+%lo(nml_err+95), r0
.LBB112_110:
	add r3, r0, r0
.LBB112_111:
	beq r12, r17, .LBB112_138
.LBB112_112:
	bne r12, r23, .LBB112_139
.LBB112_113:
	stb r19+0, r3
	jal r0, .LBB112_150
.LBB112_114:
	ldbu r3, r4+0
	addi r5, r0, 34
	beq r3, r5, .LBB112_116
.LBB112_115:
	addi r5, r0, 39
	bne r3, r5, .LBB112_140
.LBB112_116:
	add r5, r0, r0
	addi r6, r0, 1
	bne r1, r6, .LBB112_121
.LBB112_117:
	ble r12, r5, .LBB112_150
.LBB112_118:
	add r1, r15, r5
	sub r3, r12, r5
.LBB112_119:
	addi r4, r0, 32
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB112_119
	jal r0, .LBB112_150
.LBB112_120:
	addi r6, r6, 1
	bge r6, r1, .LBB112_117
.LBB112_121:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB112_124
.LBB112_122:
	addi r6, r6, 1
	bge r6, r1, .LBB112_117
.LBB112_123:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB112_117
.LBB112_124:
	bge r5, r12, .LBB112_120
.LBB112_125:
	add r7, r4, r6
	ldbu r7, r7+0
	addi r8, r5, 1
	add r5, r19, r5
	stb r5+0, r7
	add r5, r8, r0
	jal r0, .LBB112_120
.LBB112_126:
	addi r3, r0, 1023
	slt r3, r1, r3
	sub r3, r0, r3
	xori r1, r1, 1023
	and r1, r1, r3
	xori r20, r1, 1023
	addi r21, sp, 280
	add r3, r21, r0
	add r5, r20, r0
	jal r31, memcpy
	add r1, r21, r20
	stb r1+0, r0
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB112_130
.LBB112_127:
	addi r3, r0, -400
.LBB112_128:
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	add r4, r3, r4
	addi r5, r0, -1
	stw r4+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB112_128
.LBB112_129:
	stb r1+%lo(fio_uend_init), r23
.LBB112_130:
	lui r1, %hi(stdin)
	ldw r1, r1+%lo(stdin)
	lui r3, %hi(fio_in)
	stw r3+%lo(fio_in), r1
	lui r1, %hi(fio_unit)
	addi r3, r0, -1
	stw r1+%lo(fio_unit), r3
	addi r1, sp, 280
	ldw r4, sp+104
	stw r4+%lo(fio_ibuf), r1
	lui r1, %hi(fio_ilen)
	stw r1+%lo(fio_ilen), r20
	lui r1, %hi(fio_inrec)
	stw r1+%lo(fio_inrec), r23
	lui r1, %hi(fio_irec)
	stw r1+%lo(fio_irec), r0
	lui r1, %hi(fio_bz)
	stw r1+%lo(fio_bz), r0
	lui r1, %hi(fio_l_nulls)
	stw r1+%lo(fio_l_nulls), r0
	lui r1, %hi(fio_l_after)
	stb r1+%lo(fio_l_after), r0
	lui r20, %hi(fio_reading)
	stb r20+%lo(fio_reading), r23
	lui r1, %hi(fio_ldone)
	stb r1+%lo(fio_ldone), r0
	lui r1, %hi(fio_l_pending)
	stw r1+%lo(fio_l_pending), r0
	lui r1, %hi(fio_gdepth)
	stw r1+%lo(fio_gdepth), r0
	lui r1, %hi(fio_scale)
	stw r1+%lo(fio_scale), r0
	ldw r1, sp+92
	stw r1+%lo(fio_rep), r0
	ldw r1, sp+96
	stw r1+%lo(fio_desc), r0
	lui r1, %hi(fio_len)
	stw r1+%lo(fio_len), r0
	ldw r1, sp+100
	stw r1+%lo(fio_listed), r23
	lui r1, %hi(.L.str.10)
	addi r1, r1, %lo(.L.str.10)
	lui r4, %hi(fio_fmt)
	stw r4+%lo(fio_fmt), r1
	lui r1, %hi(fio_pos)
	stw r1+%lo(fio_pos), r0
	lui r1, %hi(fio_revert)
	stw r1+%lo(fio_revert), r3
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	jal r31, fio_getrec
	beq r1, r0, .LBB112_133
.LBB112_131:
	stb r20+%lo(fio_reading), r0
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB112_150
.LBB112_132:
	stw r24+%lo(nml_status), r17
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.105)
	addi r4, r4, %lo(.L.str.105)
	jal r0, .LBB112_142
.LBB112_133:
	addi r1, r14, -1
	addi r3, r0, 3
	bgtu r1, r3, .LBB112_146
.LBB112_134:
	slli r1, r1, 2
	lui r3, %hi(.LJTI112_0)
	addi r3, r3, %lo(.LJTI112_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB112_135:
	stw sp+184, r0
	addi r3, sp, 184
	jal r31, f77_rd_i
	ldw r1, sp+184
	beq r12, r17, .LBB112_147
.LBB112_136:
	bne r12, r23, .LBB112_148
.LBB112_137:
	stb r19+0, r1
	jal r0, .LBB112_149
.LBB112_138:
	sth r19+0, r3
	jal r0, .LBB112_150
.LBB112_139:
	stw r19+0, r3
	jal r0, .LBB112_150
.LBB112_140:
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB112_150
.LBB112_141:
	stw r24+%lo(nml_status), r17
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.104)
	addi r4, r4, %lo(.L.str.104)
.LBB112_142:
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+84
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB112_150
.LBB112_143:
	add r3, r19, r0
	jal r31, f77_rd_d
	jal r0, .LBB112_149
.LBB112_144:
	add r3, r19, r0
	jal r31, f77_rd_c
	jal r0, .LBB112_149
.LBB112_145:
	add r3, r19, r0
	jal r31, f77_rd_r
	jal r0, .LBB112_149
.LBB112_146:
	add r3, r19, r0
	jal r31, f77_rd_z
	jal r0, .LBB112_149
.LBB112_147:
	sth r19+0, r1
	jal r0, .LBB112_149
.LBB112_148:
	stw r19+0, r1
.LBB112_149:
	stb r20+%lo(fio_reading), r0
	ldw r1, sp+104
	stw r1+%lo(fio_ibuf), r0
.LBB112_150:
	addi r16, r16, 1
	addi r11, r11, 1
	ldw r1, r13+%lo(nml_val)
	add r3, r1, r25
	ldw r1, r3+8
	bge r11, r1, .LBB112_154
.LBB112_151:
	add r15, r15, r28
	bne r11, r27, .LBB112_102
.LBB112_152:
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB112_154
.LBB112_153:
	stw r24+%lo(nml_status), r17
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+84
	stb r1+%lo(nml_err+95), r0
.LBB112_154:
	addi fp, fp, 1
	ldw r1, r18+64
	ldw r3, r18+68
	add r1, r3, r1
	bge fp, r1, .LBB112_156
.LBB112_155:
	ldw r1, r24+%lo(nml_status)
	add r11, r0, r0
	beq r1, r0, .LBB112_99
	jal r0, .LBB112_76
.LBB112_156:
	add r11, r0, r0
	jal r0, .LBB112_76
.Lfunc_end112:
	.size	f77_nml_item, .Lfunc_end112-f77_nml_item
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI112_0,@object
.LJTI112_0:
	.word	.LBB112_135
	.word	.LBB112_145
	.word	.LBB112_143
	.word	.LBB112_144
	.size	.LJTI112_0, 16
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
	add r11, r3, r0
	lui r14, %hi(nml_len)
	ldw r13, r14+%lo(nml_len)
	addi r1, r0, 2
	lui r15, %hi(nml_line)
	addi r15, r15, %lo(nml_line)
	blt r13, r1, .LBB113_7
.LBB113_1:
	add r1, r13, r4
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB113_7
.LBB113_2:
	add r16, r4, r0
	lui r1, %hi(nml_unit)
	ldw r3, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	lui r12, %hi(nml_line)
	addi r12, r12, %lo(nml_line)
	add r3, r12, r0
	add r4, r13, r0
	jal r31, f77_wr_a
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB113_6
.LBB113_3:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB113_5
.LBB113_4:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB113_6
.LBB113_5:
	lui r13, %hi(fio_draining)
	addi r4, r0, 1
	stb r13+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r13+%lo(fio_draining), r0
.LBB113_6:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	addi r1, r0, 32
	stb r12+0, r1
	addi r1, r0, 1
	stw r14+%lo(nml_len), r1
	add r4, r16, r0
.LBB113_7:
	ldw r1, r14+%lo(nml_len)
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
	ldw r1, r14+%lo(nml_len)
	add r1, r12, r1
	stw r14+%lo(nml_len), r1
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end113:
	.size	nml_put, .Lfunc_end113-nml_put
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function nml_real
	.type	.LCPI114_0,@object
.LCPI114_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI114_0, 8
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
	add r15, r6, r0
	add r14, r5, r0
	add r11, r3, r0
	addi r1, r0, 0
	seq r1, r4, r1
	sub r1, r0, r1
	andi r1, r1, 24
	xori r12, r1, 23
	lui r1, %hi(.LCPI114_0)
	addi r1, r1, %lo(.LCPI114_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r14, r6
	xori r1, r1, 1
	bne r1, r0, .LBB114_4
.LBB114_1:
	addi r4, r0, 32
	add r3, r11, r0
	add r5, r12, r0
	jal r31, memset
	addi r3, r11, 1
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
	addi r5, r0, 3
	jal r31, memcpy
.LBB114_2:
	add r1, r11, r12
	stb r1+0, r0
.LBB114_3:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB114_4:
	beq r4, r0, .LBB114_13
.LBB114_5:
	lui r6, %hi(.L.str.101)
	addi r6, r6, %lo(.L.str.101)
	addi r13, r0, 1
	addi r7, r0, -1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, f77_wr_begin_int
	add r3, r14, r0
	add r4, r15, r0
	jal r31, f77_wr_d
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB114_9
.LBB114_6:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB114_8
.LBB114_7:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB114_9
.LBB114_8:
	lui r14, %hi(fio_draining)
	stb r14+%lo(fio_draining), r13
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r14+%lo(fio_draining), r0
.LBB114_9:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	add r1, r11, r12
	stb r1+0, r0
	addi r1, r0, 69
	addi r3, r0, 68
	jal r0, .LBB114_11
.LBB114_10:
	addi r12, r12, -1
	addi r11, r11, 1
	beq r12, r0, .LBB114_3
.LBB114_11:
	ldbu r4, r11+0
	bne r4, r1, .LBB114_10
.LBB114_12:
	stb r11+0, r3
	jal r0, .LBB114_10
.LBB114_13:
	fcvt.s.d r14, r14
	lui r6, %hi(.L.str.102)
	addi r6, r6, %lo(.L.str.102)
	addi r13, r0, 1
	addi r7, r0, -1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, f77_wr_begin_int
	add r3, r14, r0
	jal r31, f77_wr_r
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB114_17
.LBB114_14:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB114_16
.LBB114_15:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB114_17
.LBB114_16:
	lui r14, %hi(fio_draining)
	stb r14+%lo(fio_draining), r13
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r14+%lo(fio_draining), r0
.LBB114_17:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	jal r0, .LBB114_2
.Lfunc_end114:
	.size	nml_real, .Lfunc_end114-nml_real
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
	add r16, r0, r0
	blt r13, r1, .LBB115_4
.LBB115_1:
	add r1, r14, r16
	ldbu r1, r1+0
	bne r1, r17, .LBB115_4
.LBB115_2:
	addi r16, r16, 1
	bne r13, r16, .LBB115_1
.LBB115_3:
	add r16, r13, r0
.LBB115_4:
	sub r1, r13, r16
	addi r3, r0, 3
	add r15, r14, r16
	blt r1, r3, .LBB115_6
.LBB115_5:
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
	addi r5, r0, 3
	add r3, r15, r0
	jal r31, memcmp
	beq r1, r0, .LBB115_11
.LBB115_6:
	ble r13, r16, .LBB115_12
.LBB115_7:
	addi r1, r13, -1
	xor r3, r16, r1
	slt r4, r16, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	addi r3, r14, -1
.LBB115_8:
	add r4, r3, r13
	ldbu r4, r4+0
	bne r4, r17, .LBB115_12
.LBB115_9:
	addi r13, r13, -1
	bgt r13, r16, .LBB115_8
.LBB115_10:
	add r13, r1, r0
	jal r0, .LBB115_12
.LBB115_11:
	addi r1, r16, 3
	bne r1, r13, .LBB115_13
.LBB115_12:
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
.LBB115_13:
	add r1, r14, r1
	ldbu r1, r1+0
	bne r1, r17, .LBB115_6
	jal r0, .LBB115_12
.Lfunc_end115:
	.size	nml_trim_cat, .Lfunc_end115-nml_trim_cat
                                        # -- End function
	.globl	f77_nml_end                     # -- Begin function f77_nml_end
	.p2align	2
	.type	f77_nml_end,@function
f77_nml_end:                            # @f77_nml_end
# %bb.0:
	addi sp, sp, -128
	stw sp+0, lr
	stw sp+124, r11
	stw sp+120, r12
	stw sp+116, r13
	stw sp+112, r14
	lui r1, %hi(nml_reading)
	ldw r1, r1+%lo(nml_reading)
	beq r1, r0, .LBB116_6
.LBB116_1:
	lui r11, %hi(nml_status)
	ldw r1, r11+%lo(nml_status)
	beq r1, r0, .LBB116_19
.LBB116_2:
	ldw r1, r11+%lo(nml_status)
	addi r3, r0, 2
	beq r1, r3, .LBB116_23
.LBB116_3:
	addi r3, r0, 1
	bne r1, r3, .LBB116_24
.LBB116_4:
	lui r1, %hi(nml_flags)
	ldbu r1, r1+%lo(nml_flags)
	andi r4, r1, 1
	add r1, r3, r0
	bne r4, r0, .LBB116_24
.LBB116_5:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r1, %hi(nml_unit)
	ldw r5, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	lui r6, %hi(nml_grp)
	addi r6, r6, %lo(nml_grp)
	jal r0, .LBB116_28
.LBB116_6:
	lui r11, %hi(nml_npend)
	ldw r4, r11+%lo(nml_npend)
	blt r4, r0, .LBB116_8
.LBB116_7:
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	jal r31, nml_put
	addi r1, r0, -1
	stw r11+%lo(nml_npend), r1
.LBB116_8:
	lui r14, %hi(nml_len)
	ldw r11, r14+%lo(nml_len)
	addi r1, r0, 2
	lui r13, %hi(fio_listed)
	lui r12, %hi(fio_ibuf)
	blt r11, r1, .LBB116_14
.LBB116_9:
	lui r1, %hi(nml_unit)
	ldw r3, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	lui r3, %hi(nml_line)
	addi r3, r3, %lo(nml_line)
	add r4, r11, r0
	jal r31, f77_wr_a
	ldw r1, r13+%lo(fio_listed)
	bne r1, r0, .LBB116_13
.LBB116_10:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB116_12
.LBB116_11:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB116_13
.LBB116_12:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB116_13:
	jal r31, fio_flush_line
	stw r12+%lo(fio_ibuf), r0
.LBB116_14:
	lui r1, %hi(nml_line)
	addi r3, r0, 32
	stb r1+%lo(nml_line), r3
	addi r11, r0, 1
	stw r14+%lo(nml_len), r11
	lui r1, %hi(nml_unit)
	ldw r3, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	lui r3, %hi(.L.str.26)
	addi r3, r3, %lo(.L.str.26)
	addi r4, r0, 5
	jal r31, f77_wr_a
	ldw r1, r13+%lo(fio_listed)
	bne r1, r0, .LBB116_18
.LBB116_15:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB116_17
.LBB116_16:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB116_18
.LBB116_17:
	lui r13, %hi(fio_draining)
	stb r13+%lo(fio_draining), r11
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r13+%lo(fio_draining), r0
.LBB116_18:
	jal r31, fio_flush_line
	stw r12+%lo(fio_ibuf), r0
	add r1, r0, r0
	jal r0, .LBB116_24
.LBB116_19:
	lui r1, %hi(nml_nasg)
	ldw r1, r1+%lo(nml_nasg)
	addi r3, r0, 1
	blt r1, r3, .LBB116_2
.LBB116_20:
	lui r3, %hi(nml_asg)
	ldw r5, r3+%lo(nml_asg)
.LBB116_21:
	ldw r3, r5+72
	beq r3, r0, .LBB116_25
.LBB116_22:
	addi r5, r5, 76
	addi r1, r1, -1
	bne r1, r0, .LBB116_21
	jal r0, .LBB116_2
.LBB116_23:
	lui r3, %hi(nml_flags)
	ldbu r3, r3+%lo(nml_flags)
	andi r3, r3, 2
	beq r3, r0, .LBB116_27
.LBB116_24:
	ldw r14, sp+112
	ldw r13, sp+116
	ldw r12, sp+120
	ldw r11, sp+124
	ldw lr, sp+0
	addi sp, sp, 128
	jalr r0, r31, 0
.LBB116_25:
	lui r4, %hi(.L.str.27)
	addi r4, r4, %lo(.L.str.27)
	addi r3, sp, 16
	jal r31, sprintf
	ldw r1, r11+%lo(nml_status)
	bne r1, r0, .LBB116_2
.LBB116_26:
	addi r1, r0, 2
	stw r11+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	addi r4, sp, 16
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB116_2
.LBB116_27:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r1, %hi(nml_unit)
	ldw r6, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	lui r5, %hi(nml_grp)
	addi r5, r5, %lo(nml_grp)
	lui r7, %hi(nml_err)
	addi r7, r7, %lo(nml_err)
.LBB116_28:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end116:
	.size	f77_nml_end, .Lfunc_end116-f77_nml_end
                                        # -- End function
	.globl	f77_ipow                        # -- Begin function f77_ipow
	.p2align	2
	.type	f77_ipow,@function
f77_ipow:                               # @f77_ipow
# %bb.0:
	addi r1, r0, 1
	blt r4, r0, .LBB117_4
.LBB117_1:
	beq r4, r0, .LBB117_9
.LBB117_2:
	addi r5, r0, 0
.LBB117_3:
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
	bne r4, r0, .LBB117_3
	jal r0, .LBB117_9
.LBB117_4:
	beq r3, r1, .LBB117_7
.LBB117_5:
	addi r1, r0, -1
	bne r3, r1, .LBB117_8
.LBB117_6:
	andi r1, r4, 1
	addi r3, r0, 0
	seq r1, r1, r3
	addi r1, r1, -1
	ori  r1, r1, 1
	jal r0, .LBB117_9
.LBB117_7:
	add r1, r3, r0
	jal r0, .LBB117_9
.LBB117_8:
	add r1, r0, r0
.LBB117_9:
	jalr r0, r31, 0
.Lfunc_end117:
	.size	f77_ipow, .Lfunc_end117-f77_ipow
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_dpow_i
	.type	.LCPI118_0,@object
.LCPI118_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI118_0, 8
	.type	.LCPI118_1,@object
.LCPI118_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI118_1, 8
	.type	.LCPI118_2,@object
.LCPI118_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI118_2, 8
	.type	.LCPI118_3,@object
.LCPI118_3:
	.quad	0x7fefdcf158adbb99              # double 1.79E+308
	.size	.LCPI118_3, 8
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
	blt r5, r0, .LBB118_3
.LBB118_1:
	andi r3, r5, 1
	addi r1, r0, 0
	seq r3, r3, r1
	lui r4, %hi(.LCPI118_0)
	addi r4, r4, %lo(.LCPI118_0)
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
	beq r3, r0, .LBB118_16
.LBB118_2:
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
	bne r3, r0, .LBB118_2
	jal r0, .LBB118_16
.LBB118_3:
	sub r1, r0, r5
	lui r3, %hi(.LCPI118_0)
	addi r3, r3, %lo(.LCPI118_0)
	ldw r5, r3+4
	ldw r4, r3+0
	lui r3, %hi(.LCPI118_1)
	addi r3, r3, %lo(.LCPI118_1)
	ldw r9, r3+4
	ldw r8, r3+0
	lui r3, %hi(.LCPI118_2)
	addi r3, r3, %lo(.LCPI118_2)
	ldw r11, r3+4
	ldw r10, r3+0
	lui r3, %hi(.LCPI118_3)
	addi r3, r3, %lo(.LCPI118_3)
	ldw r15, r3+4
	ldw r14, r3+0
	add r16, r6, r0
	add r17, r7, r0
	add r3, r1, r0
	add r12, r4, r0
	add r13, r5, r0
	jal r0, .LBB118_5
.LBB118_4:
	fmul.d r16, r16, r16
.LBB118_5:
	andi lr, r3, 1
	fneg.d r18, r16
	beq lr, r0, .LBB118_10
.LBB118_6:
	flt.d lr, r12, r8
	fneg.d r22, r12
	xor r20, r23, r13
	sub lr, r0, lr
	and r20, r20, lr
	xor r21, r13, r20
	xor r22, r22, r12
	and lr, r22, lr
	xor r20, r12, lr
	flt.d lr, r16, r8
	xor r22, r19, r17
	sub lr, r0, lr
	and r22, r22, lr
	xor r23, r17, r22
	xor r24, r18, r16
	and lr, r24, lr
	xor r22, r16, lr
	fdiv.d r24, r14, r20
	fle.d lr, r22, r24
	bne lr, r0, .LBB118_9
.LBB118_7:
	fle.d lr, r20, r4
	bne lr, r0, .LBB118_9
.LBB118_8:
	fle.d lr, r20, r10
	xori lr, lr, 1
	fle.d r20, r22, r10
	xori r20, r20, 1
	or  lr, r20, lr
	bne lr, r0, .LBB118_13
.LBB118_9:
	fmul.d r12, r16, r12
.LBB118_10:
	srli r3, r3, 1
	beq r3, r0, .LBB118_15
.LBB118_11:
	flt.d lr, r16, r8
	xor r20, r19, r17
	sub lr, r0, lr
	and r20, r20, lr
	xor r21, r17, r20
	xor r18, r18, r16
	and lr, r18, lr
	xor r20, r16, lr
	fle.d lr, r20, r10
	bne lr, r0, .LBB118_4
.LBB118_12:
	fdiv.d r18, r14, r20
	fle.d lr, r20, r18
	xori lr, lr, 1
	beq lr, r0, .LBB118_4
.LBB118_13:
	fdiv.d r6, r4, r6
	andi r8, r1, 1
	addi r3, r0, 0
	seq r8, r8, r3
	xor r9, r5, r7
	sub r10, r0, r8
	and r8, r9, r10
	xor r9, r7, r8
	xor r4, r4, r6
	and r4, r4, r10
	xor r8, r6, r4
	srli r1, r1, 1
	beq r1, r0, .LBB118_16
.LBB118_14:
	fmul.d r6, r6, r6
	andi r4, r1, 1
	seq r4, r4, r3
	fmul.d r10, r6, r8
	xor r5, r9, r11
	sub r4, r0, r4
	and r5, r5, r4
	xor r9, r11, r5
	xor r5, r8, r10
	and r4, r5, r4
	xor r8, r10, r4
	srli r1, r1, 1
	bne r1, r0, .LBB118_14
	jal r0, .LBB118_16
.LBB118_15:
	fdiv.d r8, r4, r12
.LBB118_16:
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
.Lfunc_end118:
	.size	f77_dpow_i, .Lfunc_end118-f77_dpow_i
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rpow_i
	.type	.LCPI119_0,@object
.LCPI119_0:
	.word	0x3f800000                      # float 1
	.size	.LCPI119_0, 4
	.type	.LCPI119_1,@object
.LCPI119_1:
	.word	0x00000000                      # float 0
	.size	.LCPI119_1, 4
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0
	.type	.LCPI119_2,@object
.LCPI119_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI119_2, 8
	.type	.LCPI119_3,@object
.LCPI119_3:
	.quad	0x47eff933c78cdfad              # double 3.4E+38
	.size	.LCPI119_3, 8
	.type	.LCPI119_4,@object
.LCPI119_4:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI119_4, 8
	.text
	.globl	f77_rpow_i
	.p2align	2
	.type	f77_rpow_i,@function
f77_rpow_i:                             # @f77_rpow_i
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
	blt r4, r0, .LBB119_3
.LBB119_1:
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
	srli r4, r4, 1
	beq r4, r0, .LBB119_17
.LBB119_2:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	srli r4, r4, 1
	bne r4, r0, .LBB119_2
	jal r0, .LBB119_17
.LBB119_3:
	sub r4, r0, r4
	lui r1, %hi(.LCPI119_0)
	ldw r5, r1+%lo(.LCPI119_0)
	lui r6, %hi(.LCPI119_1)
	ldw r6, r6+%lo(.LCPI119_1)
	lui r7, %hi(.LCPI119_2)
	addi r7, r7, %lo(.LCPI119_2)
	ldw r9, r7+4
	ldw r8, r7+0
	lui r7, %hi(.LCPI119_3)
	addi r7, r7, %lo(.LCPI119_3)
	ldw r11, r7+4
	ldw r10, r7+0
	add r7, r3, r0
	add lr, r4, r0
	jal r0, .LBB119_5
.LBB119_4:
	fmul.s r7, r7, r7
.LBB119_5:
	andi r15, lr, 1
	fcvt.d.s r12, r7
	flt.s r14, r7, r6
	beq r15, r0, .LBB119_10
.LBB119_6:
	fcvt.d.s r18, r5
	flt.s r15, r5, r6
	fneg.d r20, r18
	xor r16, r21, r19
	sub r15, r0, r15
	and r16, r16, r15
	xor r17, r19, r16
	xor r20, r20, r18
	and r15, r20, r15
	xor r16, r18, r15
	fneg.d r20, r12
	xor r15, r21, r13
	sub r22, r0, r14
	and r15, r15, r22
	xor r19, r13, r15
	xor r15, r20, r12
	and r15, r15, r22
	xor r18, r12, r15
	fdiv.d r20, r10, r16
	fle.d r15, r18, r20
	bne r15, r0, .LBB119_9
.LBB119_7:
	lui r15, %hi(.LCPI119_4)
	addi r15, r15, %lo(.LCPI119_4)
	ldw r21, r15+4
	ldw r20, r15+0
	fle.d r15, r16, r20
	bne r15, r0, .LBB119_9
.LBB119_8:
	fle.d r15, r16, r8
	xori r15, r15, 1
	fle.d r16, r18, r8
	xori r16, r16, 1
	or  r15, r16, r15
	bne r15, r0, .LBB119_13
.LBB119_9:
	fmul.s r5, r7, r5
.LBB119_10:
	srli lr, lr, 1
	beq lr, r0, .LBB119_16
.LBB119_11:
	fneg.d r16, r12
	xor r15, r17, r13
	sub r18, r0, r14
	and r14, r15, r18
	xor r15, r13, r14
	xor r16, r16, r12
	and r16, r16, r18
	xor r14, r12, r16
	fle.d r12, r14, r8
	bne r12, r0, .LBB119_4
.LBB119_12:
	fdiv.d r12, r10, r14
	fle.d r12, r14, r12
	xori r12, r12, 1
	beq r12, r0, .LBB119_4
.LBB119_13:
	ldw r1, r1+%lo(.LCPI119_0)
	fdiv.s r3, r1, r3
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
.LBB119_14:
	srli r4, r4, 1
	beq r4, r0, .LBB119_17
.LBB119_15:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	jal r0, .LBB119_14
.LBB119_16:
	ldw r1, r1+%lo(.LCPI119_0)
	fdiv.s r1, r1, r5
.LBB119_17:
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
.Lfunc_end119:
	.size	f77_rpow_i, .Lfunc_end119-f77_rpow_i
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
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB120_4
.LBB120_1:
	addi r3, r0, -400
	lui lr, %hi(fio_uend+400)
	addi lr, lr, %lo(fio_uend+400)
	addi r12, r0, -1
.LBB120_2:
	add r13, r3, lr
	stw r13+0, r12
	addi r3, r3, 4
	bne r3, r0, .LBB120_2
.LBB120_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB120_4:
	ldw r14, sp+352
	addi r1, r11, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB120_21
.LBB120_5:
	addi r1, r11, -5
	addi r18, r0, 1
	bleu r1, r18, .LBB120_21
.LBB120_6:
	beq r8, r0, .LBB120_13
.LBB120_7:
	addi r1, r0, 1
	add r15, r0, r0
	blt r9, r1, .LBB120_14
.LBB120_8:
	addi r3, r0, 6
	lui lr, %hi(.L.str.31)
	addi lr, lr, %lo(.L.str.31)
	addi r12, r0, 26
	add r13, r0, r0
.LBB120_9:
	beq r13, r3, .LBB120_23
.LBB120_10:
	add r15, r13, lr
	ldb r15, r15+0
	add r16, r8, r13
	ldb r16, r16+0
	addi r17, r16, -97
	andi r17, r17, 255
	sltu r17, r17, r12
	addi r19, r16, -32
	xor r19, r19, r16
	sub r17, r0, r17
	and r17, r19, r17
	xor r16, r16, r17
	bne r16, r15, .LBB120_13
.LBB120_11:
	addi r13, r13, 1
	bne r9, r13, .LBB120_9
.LBB120_12:
	add lr, r9, r0
	beq lr, r3, .LBB120_24
.LBB120_13:
	add r15, r0, r0
.LBB120_14:
	add r16, r15, r0
	beq r10, r0, .LBB120_35
.LBB120_15:
	ldw r3, sp+336
	addi r1, r0, 1
	add r16, r15, r0
	blt r3, r1, .LBB120_35
.LBB120_16:
	addi r8, r0, 11
	lui r9, %hi(.L.str.32)
	addi r9, r9, %lo(.L.str.32)
	addi lr, r0, 26
	add r12, r0, r0
.LBB120_17:
	beq r12, r8, .LBB120_28
.LBB120_18:
	add r13, r12, r9
	ldb r13, r13+0
	add r16, r10, r12
	ldb r16, r16+0
	addi r17, r16, -97
	andi r17, r17, 255
	sltu r17, r17, lr
	addi r19, r16, -32
	xor r19, r19, r16
	sub r17, r0, r17
	and r17, r19, r17
	xor r16, r16, r17
	bne r16, r13, .LBB120_34
.LBB120_19:
	addi r12, r12, 1
	bne r3, r12, .LBB120_17
.LBB120_20:
	add r9, r3, r0
	beq r9, r8, .LBB120_29
	jal r0, .LBB120_34
.LBB120_21:
	bne r14, r0, .LBB120_44
.LBB120_22:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	add r5, r11, r0
	jal r0, .LBB120_145
.LBB120_23:
	add lr, r3, r0
	bne lr, r3, .LBB120_13
.LBB120_24:
	addi r3, r0, 7
	add r15, r1, r0
	blt r9, r3, .LBB120_14
.LBB120_25:
	addi r9, r9, -7
	addi r1, r8, 6
	addi r3, r0, 32
	addi r8, r0, 1
.LBB120_26:
	ldbu lr, r1+0
	seq r15, lr, r3
	addi r12, r9, -1
	sltu r9, r12, r9
	bne r9, r8, .LBB120_14
.LBB120_27:
	addi r1, r1, 1
	add r9, r12, r0
	beq lr, r3, .LBB120_26
	jal r0, .LBB120_14
.LBB120_28:
	add r9, r8, r0
	bne r9, r8, .LBB120_34
.LBB120_29:
	addi r8, r0, 12
	blt r3, r8, .LBB120_33
.LBB120_30:
	addi r8, r10, 11
	addi r3, r3, -11
	addi r9, r0, 32
.LBB120_31:
	ldbu r10, r8+0
	bne r10, r9, .LBB120_34
.LBB120_32:
	addi r8, r8, 1
	addi r3, r3, -1
	bne r3, r0, .LBB120_31
.LBB120_33:
	add r16, r1, r0
	jal r0, .LBB120_35
.LBB120_34:
	add r16, r0, r0
.LBB120_35:
	ldw r17, sp+348
	addi r23, r0, 0
	sne r1, r6, r23
	sgt r3, r7, r23
	and r22, r1, r3
	bne r22, r18, .LBB120_41
.LBB120_36:
	addi r1, r0, 7
	lui r3, %hi(.L.str.33)
	addi r3, r3, %lo(.L.str.33)
	addi r8, r0, 26
	add r9, r0, r0
.LBB120_37:
	beq r9, r1, .LBB120_58
.LBB120_38:
	add r10, r9, r3
	ldb r10, r10+0
	add lr, r6, r9
	ldb lr, lr+0
	addi r12, lr, -97
	andi r12, r12, 255
	sltu r12, r12, r8
	addi r13, lr, -32
	xor r13, r13, lr
	sub r12, r0, r12
	and r12, r13, r12
	xor lr, lr, r12
	bne lr, r10, .LBB120_41
.LBB120_39:
	addi r9, r9, 1
	bne r7, r9, .LBB120_37
.LBB120_40:
	add r3, r7, r0
	beq r3, r1, .LBB120_59
.LBB120_41:
	add r24, r0, r0
.LBB120_42:
	sgt r1, r17, r23
	xori r3, r15, 1
	or  r1, r1, r3
	bne r1, r0, .LBB120_46
.LBB120_43:
	beq r14, r0, .LBB120_144
.LBB120_44:
	addi r1, r0, 2
.LBB120_45:
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
.LBB120_46:
	beq r4, r0, .LBB120_51
.LBB120_47:
	addi r19, r0, 1
	blt r5, r19, .LBB120_51
.LBB120_48:
	addi r1, r0, 256
	blt r5, r1, .LBB120_69
.LBB120_49:
	bne r14, r0, .LBB120_44
.LBB120_50:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	jal r0, .LBB120_145
.LBB120_51:
	add r25, r6, r0
	add r26, r7, r0
	lui r5, %hi(.L.str.36)
	addi r5, r5, %lo(.L.str.36)
	addi r3, sp, 16
	addi r4, r0, 256
	add r6, r11, r0
	jal r31, snprintf
.LBB120_52:
	ldw r19, sp+344
	ldw r20, sp+340
	slli r13, r11, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r21, r13, r1
	ldw r1, r21+0
	beq r1, r0, .LBB120_55
.LBB120_53:
	slli r1, r11, 8
	lui r3, %hi(fio_ufname)
	addi r3, r3, %lo(fio_ufname)
	add r3, r1, r3
	addi r4, sp, 16
	jal r31, strcmp
	sne r1, r1, r23
	or  r1, r24, r1
	beq r1, r0, .LBB120_72
.LBB120_54:
	ldw r3, r21+0
	jal r31, fclose
	stw r21+0, r0
.LBB120_55:
	beq r24, r0, .LBB120_63
.LBB120_56:
	jal r31, tmpfile
	beq r1, r0, .LBB120_80
.LBB120_57:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	jal r0, .LBB120_120
.LBB120_58:
	add r3, r1, r0
	bne r3, r1, .LBB120_41
.LBB120_59:
	addi r1, r0, 8
	blt r7, r1, .LBB120_79
.LBB120_60:
	addi r9, r7, -8
	addi r1, r6, 7
	addi r3, r0, 32
	addi r8, r0, 1
.LBB120_61:
	ldbu r10, r1+0
	seq r24, r10, r3
	addi lr, r9, -1
	sltu r9, lr, r9
	bne r9, r8, .LBB120_42
.LBB120_62:
	addi r1, r1, 1
	add r9, lr, r0
	beq r10, r3, .LBB120_61
	jal r0, .LBB120_42
.LBB120_63:
	beq r22, r0, .LBB120_97
.LBB120_64:
	addi r1, r0, 3
	lui r5, %hi(.L.str.39)
	addi r5, r5, %lo(.L.str.39)
	addi r6, r0, 26
	add r7, r0, r0
	add r4, r26, r0
	add r3, r25, r0
.LBB120_65:
	beq r7, r1, .LBB120_82
.LBB120_66:
	add r8, r7, r5
	ldb r8, r8+0
	add r9, r3, r7
	ldb r9, r9+0
	addi r10, r9, -97
	andi r10, r10, 255
	sltu r10, r10, r6
	addi lr, r9, -32
	xor lr, lr, r9
	sub r10, r0, r10
	and r10, lr, r10
	xor r9, r9, r10
	bne r9, r8, .LBB120_93
.LBB120_67:
	addi r7, r7, 1
	bne r4, r7, .LBB120_65
.LBB120_68:
	add r5, r4, r0
	jal r0, .LBB120_83
.LBB120_69:
	add r25, r6, r0
	add r26, r7, r0
	addi r13, sp, 16
	add r3, r13, r0
	add r12, r5, r0
	jal r31, memcpy
	add r1, r13, r12
	stb r1+0, r0
	addi r1, r13, -1
	addi r3, r0, 32
.LBB120_70:
	add r5, r1, r12
	ldbu r6, r5+0
	bne r6, r3, .LBB120_52
.LBB120_71:
	add r4, r12, r0
	addi r12, r12, -1
	stb r5+0, r0
	bgt r4, r19, .LBB120_70
	jal r0, .LBB120_52
.LBB120_72:
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r13, r1
	stw r1+0, r15
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r13, r1
	stw r1+0, r16
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r13, r1
	stw r1+0, r17
	beq r20, r0, .LBB120_136
.LBB120_73:
	add r3, r0, r0
	blt r19, r18, .LBB120_137
.LBB120_74:
	addi r1, r0, 4
	lui r3, %hi(.L.str.37)
	addi r3, r3, %lo(.L.str.37)
	addi r4, r0, 26
	add r5, r0, r0
.LBB120_75:
	beq r5, r1, .LBB120_104
.LBB120_76:
	add r6, r5, r3
	ldb r6, r6+0
	add r7, r20, r5
	ldb r7, r7+0
	addi r8, r7, -97
	andi r8, r8, 255
	sltu r8, r8, r4
	addi r9, r7, -32
	xor r9, r9, r7
	sub r8, r0, r8
	and r8, r9, r8
	xor r7, r7, r8
	bne r7, r6, .LBB120_136
.LBB120_77:
	addi r5, r5, 1
	bne r19, r5, .LBB120_75
.LBB120_78:
	add r3, r19, r0
	jal r0, .LBB120_105
.LBB120_79:
	addi r24, r0, 1
	jal r0, .LBB120_42
.LBB120_80:
	bne r14, r0, .LBB120_44
.LBB120_81:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	jal r0, .LBB120_145
.LBB120_82:
	add r5, r1, r0
.LBB120_83:
	bne r5, r1, .LBB120_93
.LBB120_84:
	addi r1, r0, 4
	blt r4, r1, .LBB120_88
.LBB120_85:
	addi r1, r3, 3
	addi r5, r4, -3
	addi r6, r0, 32
.LBB120_86:
	ldbu r7, r1+0
	bne r7, r6, .LBB120_93
.LBB120_87:
	addi r1, r1, 1
	addi r5, r5, -1
	bne r5, r0, .LBB120_86
.LBB120_88:
	lui r4, %hi(.L.str.40)
	addi r4, r4, %lo(.L.str.40)
	addi r3, sp, 16
	jal r31, fopen
	bne r1, r0, .LBB120_90
.LBB120_89:
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	addi r3, sp, 16
	jal r31, fopen
.LBB120_90:
	addi r4, sp, 16
	bne r1, r0, .LBB120_120
.LBB120_91:
	bne r14, r0, .LBB120_44
.LBB120_92:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	jal r0, .LBB120_140
.LBB120_93:
	lui r5, %hi(.L.str.43)
	addi r5, r5, %lo(.L.str.43)
	add r25, r3, r0
	add r26, r4, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB120_97
.LBB120_94:
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	addi r3, sp, 16
	jal r31, fopen
	beq r1, r0, .LBB120_141
.LBB120_95:
	add r3, r1, r0
	jal r31, fclose
	bne r14, r0, .LBB120_44
.LBB120_96:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	jal r0, .LBB120_140
.LBB120_97:
	beq r25, r0, .LBB120_117
.LBB120_98:
	addi r1, r0, 1
	blt r26, r1, .LBB120_117
.LBB120_99:
	addi r1, r0, 7
	lui r3, %hi(.L.str.47)
	addi r3, r3, %lo(.L.str.47)
	addi r4, r0, 26
	add r5, r0, r0
.LBB120_100:
	beq r5, r1, .LBB120_111
.LBB120_101:
	add r6, r5, r3
	ldb r6, r6+0
	add r7, r25, r5
	ldb r7, r7+0
	addi r8, r7, -97
	andi r8, r8, 255
	sltu r8, r8, r4
	addi r9, r7, -32
	xor r9, r9, r7
	sub r8, r0, r8
	and r8, r9, r8
	xor r7, r7, r8
	bne r7, r6, .LBB120_127
.LBB120_102:
	addi r5, r5, 1
	bne r26, r5, .LBB120_100
.LBB120_103:
	add r3, r26, r0
	jal r0, .LBB120_112
.LBB120_104:
	add r3, r1, r0
.LBB120_105:
	bne r3, r1, .LBB120_136
.LBB120_106:
	addi r1, r0, 5
	blt r19, r1, .LBB120_110
.LBB120_107:
	addi r1, r20, 4
	addi r3, r19, -4
	addi r4, r0, 32
.LBB120_108:
	ldbu r5, r1+0
	bne r5, r4, .LBB120_136
.LBB120_109:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB120_108
.LBB120_110:
	add r3, r18, r0
	jal r0, .LBB120_137
.LBB120_111:
	add r3, r1, r0
.LBB120_112:
	bne r3, r1, .LBB120_127
.LBB120_113:
	addi r1, r0, 8
	blt r26, r1, .LBB120_117
.LBB120_114:
	addi r1, r25, 7
	addi r3, r26, -7
	addi r4, r0, 32
.LBB120_115:
	ldbu r5, r1+0
	bne r5, r4, .LBB120_127
.LBB120_116:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB120_115
.LBB120_117:
	lui r4, %hi(.L.str.40)
	addi r4, r4, %lo(.L.str.40)
	addi r3, sp, 16
	jal r31, fopen
	bne r1, r0, .LBB120_119
.LBB120_118:
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	addi r3, sp, 16
	jal r31, fopen
.LBB120_119:
	addi r4, sp, 16
	beq r1, r0, .LBB120_138
.LBB120_120:
	stw r21+0, r1
	slli r1, r11, 8
	lui r3, %hi(fio_ufname)
	addi r3, r3, %lo(fio_ufname)
	add r3, r1, r3
	jal r31, strcpy
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r1, r13, r1
	addi r3, r0, -1
	stw r1+0, r3
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r13, r1
	stw r1+0, r15
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r13, r1
	stw r1+0, r16
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r13, r1
	stw r1+0, r17
	beq r20, r0, .LBB120_136
.LBB120_121:
	addi r1, r0, 1
	add r3, r0, r0
	blt r19, r1, .LBB120_137
.LBB120_122:
	addi r3, r0, 4
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	addi r5, r0, 26
	add r6, r0, r0
.LBB120_123:
	beq r6, r3, .LBB120_129
.LBB120_124:
	add r7, r6, r4
	ldb r7, r7+0
	add r8, r20, r6
	ldb r8, r8+0
	addi r9, r8, -97
	andi r9, r9, 255
	sltu r9, r9, r5
	addi r10, r8, -32
	xor r10, r10, r8
	sub r9, r0, r9
	and r9, r10, r9
	xor r8, r8, r9
	bne r8, r7, .LBB120_136
.LBB120_125:
	addi r6, r6, 1
	bne r19, r6, .LBB120_123
.LBB120_126:
	add r4, r19, r0
	jal r0, .LBB120_130
.LBB120_127:
	bne r14, r0, .LBB120_44
.LBB120_128:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.49)
	addi r4, r4, %lo(.L.str.49)
	jal r0, .LBB120_145
.LBB120_129:
	add r4, r3, r0
.LBB120_130:
	bne r4, r3, .LBB120_136
.LBB120_131:
	addi r3, r0, 5
	blt r19, r3, .LBB120_135
.LBB120_132:
	addi r3, r20, 4
	addi r4, r19, -4
	addi r5, r0, 32
.LBB120_133:
	ldbu r6, r3+0
	bne r6, r5, .LBB120_136
.LBB120_134:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB120_133
.LBB120_135:
	add r3, r1, r0
	jal r0, .LBB120_137
.LBB120_136:
	add r3, r0, r0
.LBB120_137:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r13, r1
	stw r1+0, r3
	add r1, r0, r0
	jal r0, .LBB120_45
.LBB120_138:
	bne r14, r0, .LBB120_44
.LBB120_139:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.48)
	addi r4, r4, %lo(.L.str.48)
.LBB120_140:
	addi r5, sp, 16
	jal r0, .LBB120_145
.LBB120_141:
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	addi r12, sp, 16
	add r3, r12, r0
	jal r31, fopen
	add r4, r12, r0
	bne r1, r0, .LBB120_120
.LBB120_142:
	bne r14, r0, .LBB120_44
.LBB120_143:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.46)
	addi r4, r4, %lo(.L.str.46)
	jal r0, .LBB120_140
.LBB120_144:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
.LBB120_145:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end120:
	.size	f77_open, .Lfunc_end120-f77_open
                                        # -- End function
	.p2align	2                               # -- Begin function fio_str_is
	.type	fio_str_is,@function
fio_str_is:                             # @fio_str_is
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	addi r1, r0, 1
	blt r4, r1, .LBB121_6
.LBB121_1:
	addi r7, r0, 26
	add r6, r0, r0
.LBB121_2:
	add r8, r5, r6
	ldb r8, r8+0
	beq r8, r0, .LBB121_7
.LBB121_3:
	add r9, r3, r6
	ldb r9, r9+0
	addi r10, r9, -97
	andi r10, r10, 255
	sltu r10, r10, r7
	addi lr, r9, -32
	xor lr, lr, r9
	sub r10, r0, r10
	and r10, lr, r10
	xor r9, r9, r10
	bne r9, r8, .LBB121_8
.LBB121_4:
	addi r6, r6, 1
	bne r4, r6, .LBB121_2
.LBB121_5:
	add r6, r4, r0
	jal r0, .LBB121_7
.LBB121_6:
	add r6, r0, r0
.LBB121_7:
	add r5, r5, r6
	ldbu r5, r5+0
	beq r5, r0, .LBB121_10
.LBB121_8:
	add r1, r0, r0
.LBB121_9:
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.LBB121_10:
	ble r4, r6, .LBB121_9
.LBB121_11:
	add r3, r3, r6
	sub r4, r4, r6
	addi r5, r0, 32
.LBB121_12:
	ldbu r6, r3+0
	bne r6, r5, .LBB121_8
.LBB121_13:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB121_12
	jal r0, .LBB121_9
.Lfunc_end121:
	.size	fio_str_is, .Lfunc_end121-fio_str_is
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
	add r1, r3, r0
	addi r3, r0, 99
	bgtu r1, r3, .LBB122_27
.LBB122_1:
	slli r11, r1, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r12, r11, r3
	ldw r3, r12+0
	beq r3, r0, .LBB122_27
.LBB122_2:
	addi r7, r0, 1
	beq r4, r0, .LBB122_10
.LBB122_3:
	addi r6, r0, 1
	add r13, r0, r0
	blt r5, r6, .LBB122_11
.LBB122_4:
	addi r7, r0, 6
	lui r8, %hi(.L.str.50)
	addi r8, r8, %lo(.L.str.50)
	addi r9, r0, 26
	add r10, r0, r0
.LBB122_5:
	beq r10, r7, .LBB122_28
.LBB122_6:
	add lr, r10, r8
	ldb lr, lr+0
	add r13, r4, r10
	ldb r13, r13+0
	addi r14, r13, -97
	andi r14, r14, 255
	sltu r14, r14, r9
	addi r15, r13, -32
	xor r15, r15, r13
	sub r14, r0, r14
	and r14, r15, r14
	xor r13, r13, r14
	bne r13, lr, .LBB122_9
.LBB122_7:
	addi r10, r10, 1
	bne r5, r10, .LBB122_5
.LBB122_8:
	add r8, r5, r0
	beq r8, r7, .LBB122_29
.LBB122_9:
	add r7, r0, r0
.LBB122_10:
	add r13, r0, r0
.LBB122_11:
	add r14, r1, r0
	bne r7, r0, .LBB122_24
.LBB122_12:
	addi r1, r0, 1
	blt r5, r1, .LBB122_34
.LBB122_13:
	addi r1, r0, 4
	lui r6, %hi(.L.str.51)
	addi r6, r6, %lo(.L.str.51)
	addi r7, r0, 26
	add r8, r0, r0
.LBB122_14:
	beq r8, r1, .LBB122_18
.LBB122_15:
	add r9, r8, r6
	ldb r9, r9+0
	add r10, r4, r8
	ldb r10, r10+0
	addi lr, r10, -97
	andi lr, lr, 255
	sltu lr, lr, r7
	addi r15, r10, -32
	xor r15, r15, r10
	sub lr, r0, lr
	and lr, r15, lr
	xor r10, r10, lr
	bne r10, r9, .LBB122_34
.LBB122_16:
	addi r8, r8, 1
	bne r5, r8, .LBB122_14
.LBB122_17:
	add r6, r5, r0
	jal r0, .LBB122_19
.LBB122_18:
	add r6, r1, r0
.LBB122_19:
	bne r6, r1, .LBB122_34
.LBB122_20:
	addi r1, r0, 5
	blt r5, r1, .LBB122_24
.LBB122_21:
	addi r1, r4, 4
	addi r4, r5, -4
	addi r5, r0, 32
.LBB122_22:
	ldbu r6, r1+0
	bne r6, r5, .LBB122_34
.LBB122_23:
	addi r1, r1, 1
	addi r4, r4, -1
	bne r4, r0, .LBB122_22
.LBB122_24:
	jal r31, fclose
	stw r12+0, r0
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r1, r11, r1
	addi r3, r0, -1
	stw r1+0, r3
	slli r11, r14, 8
	lui r12, %hi(fio_ufname)
	addi r12, r12, %lo(fio_ufname)
	beq r13, r0, .LBB122_26
.LBB122_25:
	add r3, r11, r12
	jal r31, remove
.LBB122_26:
	add r1, r11, r12
	stb r1+0, r0
.LBB122_27:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB122_28:
	add r8, r7, r0
	bne r8, r7, .LBB122_9
.LBB122_29:
	addi r8, r0, 7
	add r7, r6, r0
	add r13, r6, r0
	blt r5, r8, .LBB122_11
.LBB122_30:
	addi r10, r5, -7
	addi r6, r4, 6
	addi r8, r0, 32
	addi r9, r0, 1
.LBB122_31:
	ldbu lr, r6+0
	seq r7, lr, r8
	addi r13, r10, -1
	sltu r10, r13, r10
	bne r10, r9, .LBB122_33
.LBB122_32:
	addi r6, r6, 1
	add r10, r13, r0
	add r13, r7, r0
	beq lr, r8, .LBB122_31
	jal r0, .LBB122_11
.LBB122_33:
	add r13, r7, r0
	jal r0, .LBB122_11
.LBB122_34:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.52)
	addi r4, r4, %lo(.L.str.52)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end122:
	.size	f77_close, .Lfunc_end122-f77_close
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
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB123_4
.LBB123_1:
	addi r3, r0, -400
	lui r6, %hi(fio_uend+400)
	addi r6, r6, %lo(fio_uend+400)
	addi r7, r0, -1
.LBB123_2:
	add r8, r3, r6
	stw r8+0, r7
	addi r3, r3, 4
	bne r3, r0, .LBB123_2
.LBB123_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB123_4:
	lui r20, %hi(inq_unit)
	addi r1, r0, -1
	stw r20+%lo(inq_unit), r1
	lui r23, %hi(inq_exist)
	stb r23+%lo(inq_exist), r0
	lui r19, %hi(inq_opened)
	stb r19+%lo(inq_opened), r0
	lui r12, %hi(inq_name)
	addi r12, r12, %lo(inq_name)
	stb r12+0, r0
	lui r21, %hi(inq_named)
	stw r21+%lo(inq_named), r0
	lui r18, %hi(inq_direct)
	stw r18+%lo(inq_direct), r0
	lui r17, %hi(inq_unf)
	stw r17+%lo(inq_unf), r0
	lui r15, %hi(inq_recl)
	stw r15+%lo(inq_recl), r0
	lui r14, %hi(inq_nextrec)
	stw r14+%lo(inq_nextrec), r0
	lui r16, %hi(inq_bz)
	stw r16+%lo(inq_bz), r0
	beq r4, r0, .LBB123_21
.LBB123_5:
	addi r22, r0, 1
	blt r5, r22, .LBB123_21
.LBB123_6:
	addi r1, r0, 255
	slt r1, r5, r1
	sub r1, r0, r1
	xori r3, r5, 255
	and r1, r3, r1
	xori r12, r1, 255
	addi r13, sp, 28
	add r3, r13, r0
	add r5, r12, r0
	jal r31, memcpy
	add r1, r13, r12
	stb r1+0, r0
	addi r3, r12, -1
	addi r1, r0, 32
.LBB123_7:
	addi r4, sp, 28
	add r4, r4, r3
	ldbu r5, r4+0
	bne r5, r1, .LBB123_9
.LBB123_8:
	stb r4+0, r0
	addi r4, r3, -1
	addi r5, r3, 1
	add r3, r4, r0
	bgt r5, r22, .LBB123_7
.LBB123_9:
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	addi r12, sp, 28
	add r4, r12, r0
	jal r31, strcpy
	stw r21+%lo(inq_named), r22
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	add r3, r12, r0
	jal r31, fopen
	beq r1, r0, .LBB123_11
.LBB123_10:
	stb r23+%lo(inq_exist), r22
	add r3, r1, r0
	jal r31, fclose
.LBB123_11:
	lui r12, %hi(fio_ufname+256)
	addi r12, r12, %lo(fio_ufname+256)
	lui r13, %hi(fio_ufile+4)
	addi r13, r13, %lo(fio_ufile+4)
	addi r21, r0, 100
	jal r0, .LBB123_13
.LBB123_12:
	addi r22, r22, 1
	addi r12, r12, 256
	addi r13, r13, 4
	beq r22, r21, .LBB123_16
.LBB123_13:
	ldw r1, r13+0
	beq r1, r0, .LBB123_12
.LBB123_14:
	addi r4, sp, 28
	add r3, r12, r0
	jal r31, strcmp
	bne r1, r0, .LBB123_12
.LBB123_15:
	addi r1, r0, 1
	stb r19+%lo(inq_opened), r1
	stw r20+%lo(inq_unit), r22
.LBB123_16:
	ldbu r1, r19+%lo(inq_opened)
	beq r1, r0, .LBB123_26
.LBB123_17:
	ldw r3, r20+%lo(inq_unit)
	xor r3, r3, r11
	sub r1, r0, r1
	and r1, r3, r1
	xor r11, r11, r1
.LBB123_18:
	slli r1, r11, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r4, r3+0
	stw r18+%lo(inq_direct), r4
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r1, r3
	ldw r3, r3+0
	stw r17+%lo(inq_unf), r3
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r1, r3
	ldw r3, r3+0
	stw r15+%lo(inq_recl), r3
	lui r5, %hi(fio_ubz)
	addi r5, r5, %lo(fio_ubz)
	add r5, r1, r5
	ldw r5, r5+0
	stw r16+%lo(inq_bz), r5
	beq r4, r0, .LBB123_26
.LBB123_19:
	addi r4, r0, 1
	blt r3, r4, .LBB123_26
.LBB123_20:
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r11, r1, r3
	ldw r3, r11+0
	jal r31, fflush
	ldw r3, r11+0
	jal r31, ftell
	ldw r3, r15+%lo(inq_recl)
	div r1, r1, r3
	addi r1, r1, 1
	stw r14+%lo(inq_nextrec), r1
	jal r0, .LBB123_26
.LBB123_21:
	addi r1, r0, 100
	bltu r11, r1, .LBB123_23
.LBB123_22:
	stb r23+%lo(inq_exist), r0
	jal r0, .LBB123_26
.LBB123_23:
	addi r1, r0, 1
	stb r23+%lo(inq_exist), r1
	stw r20+%lo(inq_unit), r11
	addi r3, r0, 6
	bgtu r11, r3, .LBB123_27
.LBB123_24:
	sll r4, r1, r11
	andi r4, r4, 97
	beq r4, r0, .LBB123_27
.LBB123_25:
	seq r3, r11, r3
	addi r4, r0, 5
	seq r4, r11, r4
	stb r19+%lo(inq_opened), r1
	lui r1, %hi(.L.str.55)
	addi r1, r1, %lo(.L.str.55)
	lui r5, %hi(.L.str.54)
	addi r5, r5, %lo(.L.str.54)
	xor r5, r5, r1
	sub r3, r0, r3
	and r3, r5, r3
	xor r1, r1, r3
	lui r3, %hi(.L.str.53)
	addi r3, r3, %lo(.L.str.53)
	xor r3, r3, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r4, r1, r3
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	jal r31, strcpy
	stw r21+%lo(inq_named), r0
.LBB123_26:
	add r1, r0, r0
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
.LBB123_27:
	slli r3, r11, 2
	lui r4, %hi(fio_ufile)
	addi r4, r4, %lo(fio_ufile)
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r0, .LBB123_29
.LBB123_28:
	stb r19+%lo(inq_opened), r1
	slli r1, r11, 8
	lui r3, %hi(fio_ufname)
	addi r3, r3, %lo(fio_ufname)
	add r4, r1, r3
	add r3, r12, r0
	jal r31, strcpy
	ldbu r1, r12+0
	addi r3, r0, 0
	sne r1, r1, r3
	stw r21+%lo(inq_named), r1
	jal r0, .LBB123_18
.LBB123_29:
	stb r19+%lo(inq_opened), r0
	jal r0, .LBB123_26
.Lfunc_end123:
	.size	f77_inquire, .Lfunc_end123-f77_inquire
                                        # -- End function
	.globl	f77_inq                         # -- Begin function f77_inq
	.p2align	2
	.type	f77_inq,@function
f77_inq:                                # @f77_inq
# %bb.0:
	addi r1, r3, -1
	addi r3, r0, 13
	bgtu r1, r3, .LBB124_31
.LBB124_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r12, r5, r0
	add r11, r4, r0
	slli r1, r1, 2
	lui r3, %hi(.LJTI124_0)
	addi r3, r3, %lo(.LJTI124_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB124_2:
	lui r1, %hi(inq_exist)
	ldbu r1, r1+%lo(inq_exist)
	jal r0, .LBB124_29
.LBB124_3:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB124_30
.LBB124_4:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB124_30
.LBB124_5:
	lui r1, %hi(inq_recl)
	ldw r1, r1+%lo(inq_recl)
	jal r0, .LBB124_29
.LBB124_6:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r14, r0, 1
	bne r1, r14, .LBB124_30
.LBB124_7:
	lui r1, %hi(inq_named)
	ldw r1, r1+%lo(inq_named)
	beq r1, r0, .LBB124_30
.LBB124_8:
	lui r13, %hi(inq_name)
	addi r13, r13, %lo(inq_name)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB124_30
.LBB124_9:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB124_11
.LBB124_10:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB124_30
.LBB124_11:
	add r5, r3, r0
	bge r4, r1, .LBB124_10
.LBB124_12:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB124_10
.LBB124_13:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB124_30
.LBB124_14:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB124_30
.LBB124_15:
	lui r1, %hi(inq_nextrec)
	ldw r1, r1+%lo(inq_nextrec)
	jal r0, .LBB124_29
.LBB124_16:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.58)
	addi r4, r4, %lo(.L.str.58)
	lui r5, %hi(.L.str.59)
	addi r5, r5, %lo(.L.str.59)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB124_30
.LBB124_17:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB124_19
.LBB124_18:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB124_30
.LBB124_19:
	add r5, r3, r0
	bge r4, r1, .LBB124_18
.LBB124_20:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB124_18
.LBB124_21:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unit)
	ldw r3, r3+%lo(inq_unit)
	srai r4, r3, 31
	or  r4, r3, r4
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r1, r4, r1
	jal r0, .LBB124_29
.LBB124_22:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_named)
	ldw r3, r3+%lo(inq_named)
	sub r1, r0, r1
	and r1, r3, r1
	jal r0, .LBB124_29
.LBB124_23:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.59)
	addi r4, r4, %lo(.L.str.59)
	lui r5, %hi(.L.str.58)
	addi r5, r5, %lo(.L.str.58)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB124_30
.LBB124_24:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB124_26
.LBB124_25:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB124_30
.LBB124_26:
	add r5, r3, r0
	bge r4, r1, .LBB124_25
.LBB124_27:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB124_25
.LBB124_28:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
.LBB124_29:
	stw r11+0, r1
.LBB124_30:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB124_31:
	jalr r0, r31, 0
.LBB124_32:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	lui r5, %hi(.L.str.57)
	addi r5, r5, %lo(.L.str.57)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB124_30
.LBB124_33:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB124_35
.LBB124_34:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB124_30
.LBB124_35:
	add r5, r3, r0
	bge r4, r1, .LBB124_34
.LBB124_36:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB124_34
.LBB124_37:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r5, %hi(inq_bz)
	ldw r5, r5+%lo(inq_bz)
	seq r4, r5, r4
	lui r5, %hi(.L.str.37)
	addi r5, r5, %lo(.L.str.37)
	lui r6, %hi(.L.str.61)
	addi r6, r6, %lo(.L.str.61)
	xor r6, r6, r5
	sub r4, r0, r4
	and r4, r6, r4
	xor r4, r5, r4
	lui r5, %hi(.L.str.56)
	addi r5, r5, %lo(.L.str.56)
	xor r4, r4, r5
	sub r3, r0, r3
	and r3, r4, r3
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r5, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB124_30
.LBB124_38:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB124_40
.LBB124_39:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB124_30
.LBB124_40:
	add r5, r3, r0
	bge r4, r1, .LBB124_39
.LBB124_41:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB124_39
.LBB124_42:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.58)
	addi r4, r4, %lo(.L.str.58)
	lui r5, %hi(.L.str.59)
	addi r5, r5, %lo(.L.str.59)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB124_30
.LBB124_43:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB124_45
.LBB124_44:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB124_30
.LBB124_45:
	add r5, r3, r0
	bge r4, r1, .LBB124_44
.LBB124_46:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB124_44
.LBB124_47:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.59)
	addi r4, r4, %lo(.L.str.59)
	lui r5, %hi(.L.str.58)
	addi r5, r5, %lo(.L.str.58)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB124_30
.LBB124_48:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB124_50
.LBB124_49:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB124_30
.LBB124_50:
	add r5, r3, r0
	bge r4, r1, .LBB124_49
.LBB124_51:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB124_49
.LBB124_52:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
	lui r5, %hi(.L.str.60)
	addi r5, r5, %lo(.L.str.60)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB124_30
.LBB124_53:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB124_55
.LBB124_54:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB124_30
.LBB124_55:
	add r5, r3, r0
	bge r4, r1, .LBB124_54
.LBB124_56:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB124_54
.Lfunc_end124:
	.size	f77_inq, .Lfunc_end124-f77_inq
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI124_0,@object
.LJTI124_0:
	.word	.LBB124_2
	.word	.LBB124_28
	.word	.LBB124_21
	.word	.LBB124_22
	.word	.LBB124_6
	.word	.LBB124_32
	.word	.LBB124_42
	.word	.LBB124_23
	.word	.LBB124_52
	.word	.LBB124_16
	.word	.LBB124_47
	.word	.LBB124_3
	.word	.LBB124_13
	.word	.LBB124_37
	.size	.LJTI124_0, 56
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
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB125_4
.LBB125_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB125_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB125_2
.LBB125_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB125_4:
	addi r1, r11, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB125_7
.LBB125_5:
	addi r1, r11, -5
	addi r3, r0, 1
	bleu r1, r3, .LBB125_7
.LBB125_6:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, fio_resolve
	add r12, r1, r0
	add r3, r1, r0
	jal r31, fflush
	add r3, r12, r0
	add r4, r0, r0
	add r5, r0, r0
	jal r31, fseek
	slli r1, r11, 2
	lui r3, %hi(fio_uaeof)
	addi r3, r3, %lo(fio_uaeof)
	add r1, r1, r3
	stw r1+0, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB125_7:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.62)
	addi r4, r4, %lo(.L.str.62)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end125:
	.size	f77_rewind, .Lfunc_end125-f77_rewind
                                        # -- End function
	.p2align	2                               # -- Begin function fio_fmt_use
	.type	fio_fmt_use,@function
fio_fmt_use:                            # @fio_fmt_use
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
	lui r16, %hi(fio_fbuf)
	addi r16, r16, %lo(fio_fbuf)
	beq r3, r16, .LBB126_187
.LBB126_1:
	add r13, r4, r0
	add r12, r3, r0
	lui lr, %hi(fio_ftext)
	addi r11, r0, -1
	stw lr+%lo(fio_ftext), r11
	addi r20, r0, 32
	add r1, r0, r0
	jal r0, .LBB126_3
.LBB126_2:
	add r1, r12, r28
	ldbu r3, r1+0
	addi r1, r28, 1
	bne r3, r20, .LBB126_5
.LBB126_3:
	add r28, r1, r0
	blt r13, r0, .LBB126_2
.LBB126_4:
	bltu r28, r13, .LBB126_2
.LBB126_5:
	lui r14, %hi(.L.str.67)
	addi r14, r14, %lo(.L.str.67)
	lui r15, %hi(fio_fmt_why)
	blt r13, r0, .LBB126_7
.LBB126_6:
	bgeu r28, r13, .LBB126_189
.LBB126_7:
	add r1, r12, r28
	ldbu r1, r1+0
	addi r3, r0, 40
	stw sp+36, r3
	bne r1, r3, .LBB126_189
.LBB126_8:
	addi r23, r0, 1022
	lui r19, %hi(.L.str.68)
	addi r19, r19, %lo(.L.str.68)
	addi r1, r0, 1
	stw sp+32, r1
	slli r1, r1, 2
	lui r24, %hi(.LJTI126_1)
	addi r24, r24, %lo(.LJTI126_1)
	add r22, r24, r1
	addi r26, r0, 39
	addi r9, r0, 246
	addi r10, r0, 41
	addi r25, r0, 0
	add r21, r0, r0
	add fp, r0, r0
	lui r18, %hi(fio_fbuf+1)
	addi r18, r18, %lo(fio_fbuf+1)
.LBB126_9:
	blt r13, r0, .LBB126_13
.LBB126_10:
	add r17, r0, r0
	bltu r28, r13, .LBB126_13
.LBB126_11:
	blt r21, r23, .LBB126_14
.LBB126_12:
	stw r15+%lo(fio_fmt_why), r19
	ldw r1, r22+0
	jalr r0, r1, 0
.LBB126_13:
	add r1, r12, r28
	ldbu r17, r1+0
	bge r21, r23, .LBB126_12
.LBB126_14:
	andi r27, r17, 255
	bgtu r27, r26, .LBB126_38
.LBB126_15:
	slli r1, r27, 2
	lui r3, %hi(.LJTI126_0)
	addi r3, r3, %lo(.LJTI126_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB126_16:
	add r1, r21, r0
	add r5, r26, r0
	jal r0, .LBB126_18
.LBB126_17:
	ldbu r5, r4+0
	add r1, r21, r0
.LBB126_18:
	add r3, r28, r0
.LBB126_19:
	add r4, r1, r16
	stb r4+0, r5
	addi r28, r3, 1
	blt r13, r0, .LBB126_21
.LBB126_20:
	add r4, r0, r0
	bgeu r28, r13, .LBB126_22
.LBB126_21:
	add r4, r12, r28
	ldbu r4, r4+0
.LBB126_22:
	addi r21, r1, 1
	andi r4, r4, 255
	beq r4, r0, .LBB126_35
.LBB126_23:
	addi r5, r0, 1020
	bge r1, r5, .LBB126_35
.LBB126_24:
	bne r4, r27, .LBB126_29
.LBB126_25:
	addi r28, r3, 2
	blt r13, r0, .LBB126_27
.LBB126_26:
	add r3, r0, r0
	bgeu r28, r13, .LBB126_28
.LBB126_27:
	add r3, r12, r28
	ldbu r3, r3+0
.LBB126_28:
	andi r3, r3, 255
	bne r3, r27, .LBB126_48
.LBB126_29:
	add r4, r12, r28
	blt r13, r0, .LBB126_31
.LBB126_30:
	bgeu r28, r13, .LBB126_33
.LBB126_31:
	ldbu r5, r4+0
	addi r3, r0, 39
	bne r5, r3, .LBB126_33
.LBB126_32:
	addi r21, r1, 2
	add r1, r1, r18
	stb r1+0, r3
.LBB126_33:
	blt r13, r0, .LBB126_17
.LBB126_34:
	add r1, r21, r0
	add r5, r0, r0
	add r3, r28, r0
	bgeu r28, r13, .LBB126_19
	jal r0, .LBB126_17
.LBB126_35:
	lui r1, %hi(.L.str.70)
	addi r1, r1, %lo(.L.str.70)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB126_36:
	addi r28, r28, 1
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB126_37:
	lui r1, %hi(.L.str.69)
	addi r1, r1, %lo(.L.str.69)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB126_38:
	addi r1, r17, -58
	andi r1, r1, 255
	bltu r1, r9, .LBB126_51
.LBB126_39:
	beq r21, r0, .LBB126_49
.LBB126_40:
	ldw r3, lr+%lo(fio_ftext)
	add r1, r0, r0
	beq r21, r3, .LBB126_50
.LBB126_41:
	stw sp+20, fp
	stw sp+24, r25
	stw sp+28, r24
	add r24, r22, r0
	add r25, r19, r0
	add r22, r15, r0
	add r15, lr, r0
	add r19, r10, r0
	add fp, r11, r0
	add r11, r9, r0
	lui r1, %hi(fio_fbuf-1)
	addi r1, r1, %lo(fio_fbuf-1)
	add r1, r21, r1
	ldbu r3, r1+0
	jal r31, toupper
	add r14, r1, r0
	addi r3, r0, 2
	ldw r1, sp+32
	blt r21, r3, .LBB126_43
.LBB126_42:
	lui r1, %hi(fio_fbuf-2)
	addi r1, r1, %lo(fio_fbuf-2)
	add r1, r21, r1
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 66
	sne r1, r1, r3
.LBB126_43:
	addi r3, r0, 90
	add r9, r11, r0
	add r11, fp, r0
	add r10, r19, r0
	add lr, r15, r0
	add r15, r22, r0
	add r19, r25, r0
	add r22, r24, r0
	ldw r24, sp+28
	ldw r25, sp+24
	ldw fp, sp+20
	beq r14, r3, .LBB126_50
.LBB126_44:
	addi r1, r0, -4
	and r3, r14, r1
	addi r1, r0, 1
	addi r4, r0, 68
	beq r3, r4, .LBB126_50
.LBB126_45:
	addi r3, r14, -65
	addi r4, r0, 19
	bgtu r3, r4, .LBB126_47
.LBB126_46:
	lui r4, 165
	addi r4, r4, -1791
	srl r3, r4, r3
	andi r3, r3, 1
	bne r3, r0, .LBB126_50
.LBB126_47:
	addi r1, r0, 46
	seq r1, r14, r1
	jal r0, .LBB126_50
.LBB126_48:
	addi r21, r1, 2
	add r1, r1, r18
	addi r3, r0, 39
	stb r1+0, r3
	stw lr+%lo(fio_ftext), r21
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB126_49:
	add r1, r0, r0
.LBB126_50:
	beq r1, r0, .LBB126_55
.LBB126_51:
	addi r1, r21, 1
	add r3, r21, r16
	stb r3+0, r17
	addi r28, r28, 1
	ldw r3, sp+36
	seq r3, r27, r3
	add fp, fp, r3
	bne r27, r10, .LBB126_53
.LBB126_52:
	addi fp, fp, -1
	seq r3, fp, r25
	sub r3, r0, r3
	andi r5, r3, 4
	add r21, r1, r0
	jal r0, .LBB126_54
.LBB126_53:
	add r21, r1, r0
	add r5, r0, r0
.LBB126_54:
	slli r1, r5, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB126_55:
	add r1, r28, r0
	add r4, r0, r0
	jal r0, .LBB126_59
.LBB126_56:
	ldbu r6, r3+0
.LBB126_57:
	addi r3, r0, 10
	mul r3, r4, r3
	add r3, r3, r5
	addi r4, r3, -48
	addi r3, r21, 1
	add r5, r21, r16
	stb r5+0, r6
	add r21, r3, r0
.LBB126_58:
	addi r1, r1, 1
.LBB126_59:
	add r3, r12, r1
	blt r13, r0, .LBB126_61
.LBB126_60:
	bgeu r1, r13, .LBB126_62
.LBB126_61:
	ldbu r5, r3+0
	addi r5, r5, -58
	andi r5, r5, 255
	addi r6, r0, 245
	bgtu r5, r6, .LBB126_65
.LBB126_62:
	blt r13, r0, .LBB126_64
.LBB126_63:
	bgeu r1, r13, .LBB126_74
.LBB126_64:
	ldbu r5, r3+0
	bne r5, r20, .LBB126_74
.LBB126_65:
	blt r13, r0, .LBB126_67
.LBB126_66:
	bgeu r1, r13, .LBB126_68
.LBB126_67:
	ldbu r5, r3+0
	beq r5, r20, .LBB126_58
.LBB126_68:
	bge r21, r23, .LBB126_84
.LBB126_69:
	blt r13, r0, .LBB126_71
.LBB126_70:
	add r5, r0, r0
	bgeu r1, r13, .LBB126_72
.LBB126_71:
	ldbu r5, r3+0
.LBB126_72:
	blt r13, r0, .LBB126_56
.LBB126_73:
	add r6, r0, r0
	bgeu r1, r13, .LBB126_57
	jal r0, .LBB126_56
.LBB126_74:
	blt r13, r0, .LBB126_76
.LBB126_75:
	bgeu r1, r13, .LBB126_77
.LBB126_76:
	ldbu r5, r3+0
	addi r6, r0, 72
	beq r5, r6, .LBB126_80
.LBB126_77:
	addi r5, r0, 5
	blt r13, r0, .LBB126_79
.LBB126_78:
	bgeu r1, r13, .LBB126_83
.LBB126_79:
	ldbu r6, r3+0
	addi r7, r0, 104
	bne r6, r7, .LBB126_83
.LBB126_80:
	beq r4, r0, .LBB126_85
.LBB126_81:
	add r5, r4, r21
	addi r6, r0, 1022
	blt r5, r6, .LBB126_86
.LBB126_82:
	lui r3, %hi(.L.str.71)
	addi r3, r3, %lo(.L.str.71)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB126_83:
	add r28, r1, r0
	jal r0, .LBB126_54
.LBB126_84:
	lui r3, %hi(.L.str.71)
	addi r3, r3, %lo(.L.str.71)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	add r21, r23, r0
	jalr r0, r3, 0
.LBB126_85:
	lui r3, %hi(.L.str.72)
	addi r3, r3, %lo(.L.str.72)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB126_86:
	blt r13, r0, .LBB126_88
.LBB126_87:
	add r5, r0, r0
	bgeu r1, r13, .LBB126_89
.LBB126_88:
	ldbu r5, r3+0
.LBB126_89:
	add r6, r21, r16
	stb r6+0, r5
	addi r21, r21, 1
	addi r5, r0, 1
	blt r4, r5, .LBB126_99
.LBB126_90:
	addi r6, r4, 1
	add r4, r0, r0
	jal r0, .LBB126_93
.LBB126_91:
	ldbu r8, r7+1
.LBB126_92:
	add r7, r21, r4
	add r7, r7, r16
	stb r7+0, r8
	addi r6, r6, -1
	addi r4, r4, 1
	ble r6, r5, .LBB126_100
.LBB126_93:
	add r7, r1, r4
	addi r28, r7, 1
	blt r13, r0, .LBB126_95
.LBB126_94:
	bgeu r28, r13, .LBB126_98
.LBB126_95:
	add r7, r3, r4
	ldbu r8, r7+1
	beq r8, r0, .LBB126_98
.LBB126_96:
	blt r13, r0, .LBB126_91
.LBB126_97:
	add r8, r0, r0
	bgeu r28, r13, .LBB126_92
	jal r0, .LBB126_91
.LBB126_98:
	lui r1, %hi(.L.str.73)
	addi r1, r1, %lo(.L.str.73)
	stw r15+%lo(fio_fmt_why), r1
	add r21, r21, r4
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB126_99:
	addi r28, r1, 1
	jal r0, .LBB126_101
.LBB126_100:
	add r1, r1, r4
	addi r28, r1, 1
	add r21, r21, r4
.LBB126_101:
	stw lr+%lo(fio_ftext), r21
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB126_102:
	add r1, r21, r16
	stb r1+0, r0
	addi r14, r0, 1
	addi r19, r0, 58
	addi r24, r0, -1
	addi r21, r0, -10
	addi r22, r0, 9
	addi r23, r0, 10
	addi r25, r0, 72
	lui r17, %hi(.L.str.75)
	addi r17, r17, %lo(.L.str.75)
	addi r26, r0, 80
	addi r27, r0, 88
	addi r28, r0, 25
	addi r1, r0, -4
	stw sp+28, r1
	addi r1, r0, 68
	stw sp+32, r1
	addi r1, r0, 46
	stw sp+24, r1
	jal r0, .LBB126_104
.LBB126_103:
	addi r14, r14, 1
.LBB126_104:
	add r1, r14, r16
	ldbu r1, r1+0
	bgtu r1, r19, .LBB126_116
.LBB126_105:
	slli r3, r1, 2
	lui r4, %hi(.LJTI126_2)
	addi r4, r4, %lo(.LJTI126_2)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB126_106:
	addi fp, r14, 1
	add r1, r14, r18
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB126_112
.LBB126_107:
	addi r20, r0, -1
	jal r0, .LBB126_114
.LBB126_108:
	addi r14, r14, 1
.LBB126_109:
	add r1, r14, r0
	add r3, r14, r16
	ldbu r4, r3+0
	addi r14, r14, 1
	addi r3, r0, 39
	bne r4, r3, .LBB126_109
.LBB126_110:
	add r4, r14, r16
	ldbu r4, r4+0
	bne r4, r3, .LBB126_104
.LBB126_111:
	addi r14, r1, 2
	jal r0, .LBB126_109
.LBB126_112:
	add r20, r0, r0
	addi r1, r1, -48
	bgtu r1, r22, .LBB126_114
.LBB126_113:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB126_113
.LBB126_114:
	lui r14, %hi(.L.str.74)
	addi r14, r14, %lo(.L.str.74)
	blt r20, r0, .LBB126_189
.LBB126_115:
	add r1, fp, r16
	ldbu r3, r1+0
	jal r31, toupper
	beq r1, r26, .LBB126_122
	jal r0, .LBB126_189
.LBB126_116:
	slli r1, r1, 24
	srai r1, r1, 24
	addi r3, r1, -58
	bgeu r3, r21, .LBB126_118
.LBB126_117:
	add fp, r14, r0
	add r20, r24, r0
	jal r0, .LBB126_122
.LBB126_118:
	addi r1, r1, -48
	bleu r1, r22, .LBB126_120
.LBB126_119:
	add fp, r14, r0
	add r20, r0, r0
	jal r0, .LBB126_122
.LBB126_120:
	add r20, r0, r0
	add fp, r14, r0
.LBB126_121:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB126_121
.LBB126_122:
	add r14, fp, r16
	ldbu r3, r14+0
	jal r31, toupper
	bne r1, r25, .LBB126_125
.LBB126_123:
	blt r20, r0, .LBB126_188
.LBB126_124:
	add r1, fp, r20
	addi r14, r1, 1
	jal r0, .LBB126_104
.LBB126_125:
	blt r20, r0, .LBB126_128
.LBB126_126:
	ldw r3, sp+36
	bne r1, r3, .LBB126_128
.LBB126_127:
	addi r14, fp, 1
	jal r0, .LBB126_104
.LBB126_128:
	beq r1, r27, .LBB126_135
.LBB126_129:
	sgt r3, r20, r24
	seq r4, r1, r26
	and r3, r3, r4
	bne r3, r0, .LBB126_135
.LBB126_130:
	addi r3, r1, -65
	bgtu r3, r28, .LBB126_139
.LBB126_131:
	slli r3, r3, 2
	lui r4, %hi(.LJTI126_3)
	addi r4, r4, %lo(.LJTI126_3)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB126_132:
	addi r14, fp, 1
	add r1, fp, r18
.LBB126_133:
	ldb r1, r1+0
	addi r1, r1, -58
	bltu r1, r21, .LBB126_104
.LBB126_134:
	add r1, r14, r0
	addi r14, r14, 1
	add r1, r1, r18
	jal r0, .LBB126_133
.LBB126_135:
	addi r14, fp, 1
	jal r0, .LBB126_104
.LBB126_136:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 82
	beq r1, r3, .LBB126_138
.LBB126_137:
	addi r3, r0, 76
	bne r1, r3, .LBB126_159
.LBB126_138:
	addi r14, fp, 2
	jal r0, .LBB126_160
.LBB126_139:
	ldw r3, sp+28
	and r3, r1, r3
	ldw r4, sp+32
	beq r3, r4, .LBB126_142
.LBB126_140:
	addi r3, r0, 76
	beq r1, r3, .LBB126_142
.LBB126_141:
	addi r3, r0, 73
	bne r1, r3, .LBB126_207
.LBB126_142:
	add r3, fp, r18
	ldb r4, r3+0
	addi r3, r4, -58
	bltu r3, r21, .LBB126_202
.LBB126_143:
	addi r3, fp, 1
	addi r4, r4, -48
	bleu r4, r22, .LBB126_151
.LBB126_144:
	add r4, r0, r0
	jal r0, .LBB126_154
.LBB126_145:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 90
	beq r1, r3, .LBB126_147
.LBB126_146:
	addi r3, r0, 78
	bne r1, r3, .LBB126_205
.LBB126_147:
	addi r14, fp, 2
	jal r0, .LBB126_104
.LBB126_148:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 83
	beq r1, r3, .LBB126_150
.LBB126_149:
	addi r14, fp, 1
	bne r1, r26, .LBB126_104
.LBB126_150:
	addi r14, fp, 2
	jal r0, .LBB126_104
.LBB126_151:
	add r5, r0, r0
.LBB126_152:
	add r6, r3, r0
	mul r3, r5, r23
	add r5, r3, r4
	addi r3, r6, 1
	add r4, r6, r18
	ldb r4, r4+0
	addi r4, r4, -48
	bltu r4, r23, .LBB126_152
.LBB126_153:
	srli r4, r5, 31
.LBB126_154:
	bne r4, r0, .LBB126_202
.LBB126_155:
	add r4, r3, r16
	ldbu r4, r4+0
	ldw r5, sp+24
	bne r4, r5, .LBB126_158
.LBB126_156:
	addi r14, r3, 1
	add r3, r3, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB126_168
.LBB126_157:
	addi r3, r0, 1
	jal r0, .LBB126_173
.LBB126_158:
	add r14, r3, r0
	jal r0, .LBB126_174
.LBB126_159:
	addi r14, fp, 1
.LBB126_160:
	add r1, r14, r16
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB126_162
.LBB126_161:
	addi r1, r0, 1
	jal r0, .LBB126_167
.LBB126_162:
	addi r1, r1, -48
	bleu r1, r22, .LBB126_164
.LBB126_163:
	add r1, r0, r0
	jal r0, .LBB126_167
.LBB126_164:
	add r3, r0, r0
.LBB126_165:
	add r4, r14, r0
	mul r3, r3, r23
	add r3, r3, r1
	addi r14, r14, 1
	add r1, r4, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB126_165
.LBB126_166:
	srli r1, r3, 31
.LBB126_167:
	beq r1, r0, .LBB126_104
	jal r0, .LBB126_204
.LBB126_168:
	addi r3, r3, -48
	bleu r3, r22, .LBB126_170
.LBB126_169:
	add r3, r0, r0
	jal r0, .LBB126_173
.LBB126_170:
	add r4, r0, r0
.LBB126_171:
	add r5, r14, r0
	mul r4, r4, r23
	add r4, r4, r3
	addi r14, r14, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB126_171
.LBB126_172:
	srli r3, r4, 31
.LBB126_173:
	bne r3, r0, .LBB126_206
.LBB126_174:
	ldw r3, sp+32
	beq r1, r3, .LBB126_176
.LBB126_175:
	andi r1, r1, 77
	addi r3, r0, 69
	bne r1, r3, .LBB126_104
.LBB126_176:
	add r1, r14, r16
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 69
	bne r1, r3, .LBB126_104
.LBB126_177:
	addi r1, r14, 1
	add r3, r14, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB126_179
.LBB126_178:
	addi r3, r0, 1
	jal r0, .LBB126_184
.LBB126_179:
	addi r3, r3, -48
	bleu r3, r22, .LBB126_181
.LBB126_180:
	add r14, r1, r0
	add r3, r0, r0
	jal r0, .LBB126_185
.LBB126_181:
	add r4, r0, r0
.LBB126_182:
	add r5, r1, r0
	mul r1, r4, r23
	add r4, r1, r3
	addi r1, r5, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB126_182
.LBB126_183:
	srli r3, r4, 31
.LBB126_184:
	add r14, r1, r0
.LBB126_185:
	beq r3, r0, .LBB126_104
.LBB126_186:
	lui r14, %hi(.L.str.80)
	addi r14, r14, %lo(.L.str.80)
	jal r0, .LBB126_189
.LBB126_187:
	lui r1, %hi(fio_fmt)
	stw r1+%lo(fio_fmt), r16
	add r11, r0, r0
	jal r0, .LBB126_201
.LBB126_188:
	add r14, r17, r0
.LBB126_189:
	stw r15+%lo(fio_fmt_why), r14
.LBB126_190:
	addi r1, r0, 79
	addi r3, r0, 40
	addi r4, r0, 41
	add r6, r0, r0
	add r5, r0, r0
	jal r0, .LBB126_192
.LBB126_191:
	addi r6, r6, 1
.LBB126_192:
	blt r13, r0, .LBB126_194
.LBB126_193:
	add r7, r0, r0
	bgeu r6, r13, .LBB126_195
.LBB126_194:
	add r7, r12, r6
	ldbu r7, r7+0
.LBB126_195:
	andi r7, r7, 255
	beq r7, r0, .LBB126_200
.LBB126_196:
	bgtu r6, r1, .LBB126_200
.LBB126_197:
	seq r8, r7, r3
	add r5, r5, r8
	bne r7, r4, .LBB126_191
.LBB126_198:
	addi r5, r5, -1
	bne r5, r0, .LBB126_191
.LBB126_199:
	addi r6, r6, 1
.LBB126_200:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	ldw r5, r15+%lo(fio_fmt_why)
	lui r4, %hi(.L.str.84)
	addi r4, r4, %lo(.L.str.84)
	add r7, r12, r0
	jal r31, fprintf
.LBB126_201:
	add r1, r11, r0
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
.LBB126_202:
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.78)
	addi r4, r4, %lo(.L.str.78)
	add r3, r14, r0
	add r5, r1, r0
.LBB126_203:
	jal r31, sprintf
	jal r0, .LBB126_189
.LBB126_204:
	lui r14, %hi(.L.str.76)
	addi r14, r14, %lo(.L.str.76)
	jal r0, .LBB126_189
.LBB126_205:
	lui r14, %hi(.L.str.77)
	addi r14, r14, %lo(.L.str.77)
	jal r0, .LBB126_189
.LBB126_206:
	lui r14, %hi(.L.str.79)
	addi r14, r14, %lo(.L.str.79)
	jal r0, .LBB126_189
.LBB126_207:
	blt r20, r0, .LBB126_210
.LBB126_208:
	addi r1, r1, -41
	addi r3, r0, 17
	bgtu r1, r3, .LBB126_211
.LBB126_209:
	slli r1, r1, 2
	lui r3, %hi(.Lswitch.table.fio_fmt_use)
	addi r3, r3, %lo(.Lswitch.table.fio_fmt_use)
	add r1, r1, r3
	ldw r14, r1+0
	jal r0, .LBB126_189
.LBB126_210:
	ldb r5, r14+0
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.83)
	addi r4, r4, %lo(.L.str.83)
	add r3, r14, r0
	jal r0, .LBB126_203
.LBB126_211:
	lui r14, %hi(.L.str.82)
	addi r14, r14, %lo(.L.str.82)
	jal r0, .LBB126_189
.LBB126_212:
.Lfunc_end126:
	.size	fio_fmt_use, .Lfunc_end126-fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI126_0,@object
.LJTI126_0:
	.word	.LBB126_37
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_36
	.word	.LBB126_38
	.word	.LBB126_16
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_38
	.word	.LBB126_16
	.size	.LJTI126_0, 160
	.type	.LJTI126_1,@object
.LJTI126_1:
	.word	.LBB126_9
	.word	.LBB126_190
	.word	.LBB126_212
	.word	.LBB126_212
	.word	.LBB126_102
	.word	.LBB126_9
	.size	.LJTI126_1, 24
	.type	.LJTI126_2,@object
.LJTI126_2:
	.word	.LBB126_187
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_108
	.word	.LBB126_103
	.word	.LBB126_103
	.word	.LBB126_116
	.word	.LBB126_106
	.word	.LBB126_103
	.word	.LBB126_106
	.word	.LBB126_116
	.word	.LBB126_103
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_116
	.word	.LBB126_103
	.size	.LJTI126_2, 236
	.type	.LJTI126_3,@object
.LJTI126_3:
	.word	.LBB126_132
	.word	.LBB126_145
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_132
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_148
	.word	.LBB126_136
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_139
	.word	.LBB126_132
	.size	.LJTI126_3, 104
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function fio_number
	.type	fio_number,@function
fio_number:                             # @fio_number
# %bb.0:
	lui r1, %hi(fio_fmt)
	ldw r1, r1+%lo(fio_fmt)
	lui r3, %hi(fio_pos)
	ldw r4, r3+%lo(fio_pos)
	add r5, r1, r4
	ldb r5, r5+0
	addi r5, r5, -48
	addi r6, r0, 9
	bleu r5, r6, .LBB127_2
.LBB127_1:
	add r1, r0, r0
	jal r0, .LBB127_5
.LBB127_2:
	addi r6, r1, 1
	addi r7, r0, 10
	add r1, r0, r0
.LBB127_3:
	mul r1, r1, r7
	addi r8, r4, 1
	add r1, r5, r1
	add r4, r6, r4
	ldb r4, r4+0
	addi r5, r4, -48
	add r4, r8, r0
	bltu r5, r7, .LBB127_3
.LBB127_4:
	stw r3+%lo(fio_pos), r8
.LBB127_5:
	jalr r0, r31, 0
.Lfunc_end127:
	.size	fio_number, .Lfunc_end127-fio_number
                                        # -- End function
	.p2align	2                               # -- Begin function fio_getrec
	.type	fio_getrec,@function
fio_getrec:                             # @fio_getrec
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
	lui r1, %hi(fio_ibuf)
	ldw r1, r1+%lo(fio_ibuf)
	beq r1, r0, .LBB128_3
.LBB128_1:
	lui r11, %hi(fio_irec)
	ldw r3, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r3, r4, .LBB128_28
.LBB128_2:
	lui r12, %hi(fio_ilen)
	ldw r5, r12+%lo(fio_ilen)
	mul r3, r5, r3
	add r4, r1, r3
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	jal r31, memcpy
	ldw r1, r12+%lo(fio_ilen)
	lui r3, %hi(fio_rlen)
	stw r3+%lo(fio_rlen), r1
	lui r1, %hi(fio_rpos)
	stw r1+%lo(fio_rpos), r0
	ldw r1, r11+%lo(fio_irec)
	addi r1, r1, 1
	stw r11+%lo(fio_irec), r1
	jal r0, .LBB128_27
.LBB128_3:
	lui r12, %hi(fio_unit)
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	addi r13, r0, 98
	lui r11, %hi(fio_in)
	bgtu r3, r13, .LBB128_7
.LBB128_4:
	slli r1, r1, 2
	lui r14, %hi(fio_uend)
	addi r14, r14, %lo(fio_uend)
	add r1, r1, r14
	ldw r1, r1+0
	blt r1, r0, .LBB128_7
.LBB128_5:
	ldw r3, r11+%lo(fio_in)
	beq r3, r0, .LBB128_7
.LBB128_6:
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	add r4, r3, r14
	ldw r4, r4+0
	bge r1, r4, .LBB128_30
.LBB128_7:
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	bgtu r3, r13, .LBB128_11
.LBB128_8:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB128_11
.LBB128_9:
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
	ldw r6, r11+%lo(fio_in)
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	addi r11, r0, 1
	add r4, r11, r0
	jal r31, fread
	add r3, r1, r0
	add r1, r11, r0
	beq r3, r0, .LBB128_29
.LBB128_10:
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB128_26
.LBB128_11:
	ldw r1, r11+%lo(fio_in)
	addi r3, r0, 0
	seq r3, r1, r3
	lui r4, %hi(stdin)
	ldw r4, r4+%lo(stdin)
	xor r4, r4, r1
	sub r3, r0, r3
	and r3, r4, r3
	xor r11, r1, r3
	addi r16, r0, -1
	lui r15, %hi(fio_rec)
	addi r15, r15, %lo(fio_rec)
	addi r14, r0, 10
	addi r17, r0, 1022
	add r3, r0, r0
.LBB128_12:
	add r18, r3, r0
	add r3, r11, r0
	jal r31, getc
	beq r1, r16, .LBB128_17
.LBB128_13:
	addi r3, r18, 1
	add r4, r18, r15
	stb r4+0, r1
	beq r1, r14, .LBB128_15
.LBB128_14:
	bltu r18, r17, .LBB128_12
.LBB128_15:
	beq r3, r0, .LBB128_18
.LBB128_16:
	add r1, r3, r15
	stb r1+0, r0
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB128_20
.LBB128_17:
	add r3, r18, r0
	bne r3, r0, .LBB128_16
.LBB128_18:
	ldw r1, r12+%lo(fio_unit)
	addi r4, r1, -1
	bgtu r4, r13, .LBB128_20
.LBB128_19:
	slli r1, r1, 2
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r1, r1, r4
	addi r4, r0, 1
	stw r1+0, r4
.LBB128_20:
	beq r3, r0, .LBB128_28
.LBB128_21:
	lui r1, %hi(fio_rlen)
	ldw r4, r1+%lo(fio_rlen)
	addi r3, r0, 1
	blt r4, r3, .LBB128_26
.LBB128_22:
	addi r4, r4, 1
	lui r5, %hi(fio_rec-2)
	addi r5, r5, %lo(fio_rec-2)
	addi r6, r0, 13
	jal r0, .LBB128_24
.LBB128_23:
	addi r7, r4, -2
	stw r1+%lo(fio_rlen), r7
	addi r4, r4, -1
	ble r4, r3, .LBB128_26
.LBB128_24:
	add r7, r4, r5
	ldbu r7, r7+0
	beq r7, r6, .LBB128_23
.LBB128_25:
	beq r7, r14, .LBB128_23
.LBB128_26:
	lui r1, %hi(fio_rpos)
	stw r1+%lo(fio_rpos), r0
.LBB128_27:
	add r1, r0, r0
	jal r0, .LBB128_29
.LBB128_28:
	addi r1, r0, 1
.LBB128_29:
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
.LBB128_30:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r3, r3, r1
	addi r1, r0, 1
	stw r3+0, r1
	jal r0, .LBB128_29
.Lfunc_end128:
	.size	fio_getrec, .Lfunc_end128-fio_getrec
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
	add r11, r4, r0
	add r12, r3, r0
	lui r14, %hi(nml_tlen)
	ldw r1, r14+%lo(nml_tlen)
	add r1, r4, r1
	addi r1, r1, 2
	lui r16, %hi(nml_tcap)
	ldw r3, r16+%lo(nml_tcap)
	lui r15, %hi(nml_text)
	ble r1, r3, .LBB129_5
.LBB129_1:
	addi r4, r0, 0
	seq r4, r3, r4
	slli r3, r3, 1
	lui r5, 1
	xor r5, r3, r5
	sub r4, r0, r4
	and r4, r5, r4
	xor r3, r3, r4
.LBB129_2:
	add r13, r3, r0
	slli r3, r3, 1
	blt r13, r1, .LBB129_2
.LBB129_3:
	ldw r3, r15+%lo(nml_text)
	add r4, r13, r0
	jal r31, realloc
	beq r1, r0, .LBB129_6
.LBB129_4:
	stw r15+%lo(nml_text), r1
	stw r16+%lo(nml_tcap), r13
.LBB129_5:
	ldw r1, r15+%lo(nml_text)
	ldw r3, r14+%lo(nml_tlen)
	add r3, r1, r3
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r14+%lo(nml_tlen)
	add r1, r1, r11
	stw r14+%lo(nml_tlen), r1
	ldw r3, r15+%lo(nml_text)
	add r1, r3, r1
	stb r1+0, r0
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB129_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end129:
	.size	nml_text_add, .Lfunc_end129-nml_text_add
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

	.type	nml_cval,@object                # @nml_cval
	.local	nml_cval
	.comm	nml_cval,4,4
	.type	nml_casg,@object                # @nml_casg
	.local	nml_casg
	.comm	nml_casg,4,4
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

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git b9746eee61313b56b6d9c51df3861c6fdd173efa)"
	.section	".note.GNU-stack","",@progbits
