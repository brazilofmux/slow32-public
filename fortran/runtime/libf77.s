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
	add r12, r4, r0
	add r14, r3, r0
	lui r3, %hi(fio_busy)
	ldbu r4, r3+%lo(fio_busy)
	addi r1, r0, 1
	beq r4, r1, .LBB1_21
.LBB1_1:
	add r11, r5, r0
	stb r3+%lo(fio_busy), r1
	lui r3, %hi(fio_uend_init)
	ldbu r4, r3+%lo(fio_uend_init)
	bne r4, r0, .LBB1_5
.LBB1_2:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
	addi r6, r0, -1
.LBB1_3:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r0, .LBB1_3
.LBB1_4:
	stb r3+%lo(fio_uend_init), r1
.LBB1_5:
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
	bgtu r4, r5, .LBB1_7
.LBB1_6:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r4, r3, r4
	stw r4+0, r0
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r4, r3, r4
	stw r4+0, r0
.LBB1_7:
	lui r5, %hi(fio_rec_next)
	ldw r4, r5+%lo(fio_rec_next)
	stw r5+%lo(fio_rec_next), r0
	addi r5, r14, -100
	addi r6, r0, -99
	bltu r5, r6, .LBB1_11
.LBB1_8:
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r3, r5
	ldw r5, r5+0
	beq r5, r0, .LBB1_11
.LBB1_9:
	ble r4, r0, .LBB1_23
.LBB1_10:
	addi r4, r4, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB1_11:
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
	beq r12, r0, .LBB1_18
.LBB1_12:
	stw r1+%lo(fio_listed), r0
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fio_fmt_use
	addi r3, r0, -1
	ble r1, r3, .LBB1_22
.LBB1_13:
	stw r14+%lo(fio_pos), r0
	stw r15+%lo(fio_revert), r3
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	lui r1, %hi(fio_fmt)
	ldw r3, r1+%lo(fio_fmt)
	addi r4, r0, 32
	add r1, r0, r0
.LBB1_14:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB1_16
.LBB1_15:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB1_14
.LBB1_16:
	addi r3, r0, 40
	bne r5, r3, .LBB1_20
.LBB1_17:
	addi r3, r1, 1
	add r1, r3, r0
	jal r0, .LBB1_19
.LBB1_18:
	stw r1+%lo(fio_listed), r13
	lui r1, %hi(.L.str.7)
	addi r1, r1, %lo(.L.str.7)
	lui r3, %hi(fio_fmt)
	stw r3+%lo(fio_fmt), r1
	addi r1, r0, -1
	add r3, r0, r0
.LBB1_19:
	stw r14+%lo(fio_pos), r3
	stw r15+%lo(fio_revert), r1
.LBB1_20:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB1_21:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	jal r0, .LBB1_24
.LBB1_22:
	addi r3, r0, 2
	jal r31, exit
.LBB1_23:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.64)
	addi r4, r4, %lo(.L.str.64)
.LBB1_24:
	jal r31, fprintf
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
	lui r5, %hi(.L.str.33)
	addi r5, r5, %lo(.L.str.33)
	addi r12, sp, 20
	addi r4, r0, 256
	add r3, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	add r3, r12, r0
	jal r31, fopen
	bne r1, r0, .LBB2_13
.LBB2_12:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
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
	lui r4, %hi(.L.str.62)
	addi r4, r4, %lo(.L.str.62)
	lui r5, %hi(.L.str.63)
	addi r5, r5, %lo(.L.str.63)
	xor r5, r5, r4
	sub r1, r0, r1
	and r1, r5, r1
	xor r6, r4, r1
	lui r4, %hi(.L.str.61)
	addi r4, r4, %lo(.L.str.61)
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
	lui r3, %hi(fio_busy)
	ldbu r4, r3+%lo(fio_busy)
	addi r1, r0, 1
	beq r4, r1, .LBB3_12
.LBB3_1:
	stb r3+%lo(fio_busy), r1
	lui r3, %hi(fio_uend_init)
	ldbu r4, r3+%lo(fio_uend_init)
	bne r4, r0, .LBB3_5
.LBB3_2:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
	addi r6, r0, -1
.LBB3_3:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r0, .LBB3_3
.LBB3_4:
	stb r3+%lo(fio_uend_init), r1
.LBB3_5:
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
	bgtu r4, r5, .LBB3_7
.LBB3_6:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r4, r3, r4
	stw r4+0, r12
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r4, r3, r4
	stw r4+0, r0
.LBB3_7:
	lui r5, %hi(fio_rec_next)
	ldw r4, r5+%lo(fio_rec_next)
	stw r5+%lo(fio_rec_next), r0
	addi r5, r11, -100
	addi r6, r0, -99
	bltu r5, r6, .LBB3_11
.LBB3_8:
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r3, r5
	ldw r5, r5+0
	beq r5, r0, .LBB3_11
.LBB3_9:
	ble r4, r0, .LBB3_13
.LBB3_10:
	addi r4, r4, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB3_11:
	lui r1, %hi(fio_ulen)
	stw r1+%lo(fio_ulen), r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB3_12:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	jal r0, .LBB3_14
.LBB3_13:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.64)
	addi r4, r4, %lo(.L.str.64)
.LBB3_14:
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
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
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
	stw sp+24, r16
	lui r15, %hi(fio_busy)
	ldbu r1, r15+%lo(fio_busy)
	addi r12, r0, 1
	beq r1, r12, .LBB6_31
.LBB6_1:
	add r11, r4, r0
	add r14, r3, r0
	stb r15+%lo(fio_busy), r12
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB6_5
.LBB6_2:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB6_3:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB6_3
.LBB6_4:
	stb r1+%lo(fio_uend_init), r12
.LBB6_5:
	add r3, r14, r0
	add r4, r0, r0
	jal r31, fio_resolve
	add r13, r1, r0
	lui r1, %hi(fio_in)
	stw r1+%lo(fio_in), r13
	lui r1, %hi(fio_unit)
	stw r1+%lo(fio_unit), r14
	addi r1, r14, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB6_14
.LBB6_6:
	slli r16, r14, 2
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r16, r1
	addi r3, r0, 1
	stw r1+0, r3
	lui r3, %hi(fio_rec_next)
	ldw r1, r3+%lo(fio_rec_next)
	stw r3+%lo(fio_rec_next), r0
	addi r3, r14, -100
	addi r4, r0, -99
	bltu r3, r4, .LBB6_10
.LBB6_7:
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r16, r3
	ldw r3, r3+0
	beq r3, r0, .LBB6_10
.LBB6_8:
	ble r1, r0, .LBB6_33
.LBB6_9:
	addi r1, r1, -1
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r16, r3
	ldw r3, r3+0
	mul r4, r3, r1
	add r3, r13, r0
	add r5, r0, r0
	jal r31, fseek
.LBB6_10:
	lui r1, %hi(fio_upos)
	stw r1+%lo(fio_upos), r0
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r16, r1
	ldw r1, r1+0
	beq r1, r0, .LBB6_19
.LBB6_11:
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
	stw r3+%lo(fio_ulen), r1
	add r12, r0, r0
	bne r1, r0, .LBB6_26
.LBB6_12:
	andi r1, r11, 2
	bne r1, r0, .LBB6_30
.LBB6_13:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.65)
	addi r4, r4, %lo(.L.str.65)
	jal r0, .LBB6_35
.LBB6_14:
	lui r3, %hi(fio_rec_next)
	ldw r1, r3+%lo(fio_rec_next)
	stw r3+%lo(fio_rec_next), r0
	addi r3, r14, -100
	addi r4, r0, -99
	bltu r3, r4, .LBB6_18
.LBB6_15:
	slli r3, r14, 2
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r3, r4
	ldw r4, r4+0
	beq r4, r0, .LBB6_18
.LBB6_16:
	ble r1, r0, .LBB6_33
.LBB6_17:
	addi r1, r1, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r3, r3, r4
	ldw r3, r3+0
	mul r4, r3, r1
	add r3, r13, r0
	add r5, r0, r0
	jal r31, fseek
.LBB6_18:
	lui r1, %hi(fio_upos)
	stw r1+%lo(fio_upos), r0
.LBB6_19:
	slli r16, r14, 2
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r14, r16, r1
	ldw r1, r14+0
	blt r1, r0, .LBB6_21
.LBB6_20:
	add r3, r13, r0
	jal r31, ftell
	ldw r3, r14+0
	bge r1, r3, .LBB6_25
.LBB6_21:
	addi r3, sp, 20
	addi r12, r0, 1
	addi r14, r0, 4
	add r4, r12, r0
	add r5, r14, r0
	add r6, r13, r0
	jal r31, fread
	bne r1, r14, .LBB6_25
.LBB6_22:
	addi r3, sp, 20
	addi r1, r3, 3
	ldbu r1, r1+0
	slli r1, r1, 24
	blt r1, r0, .LBB6_29
.LBB6_23:
	ldhu r4, sp+20
	addi r3, r3, 2
	ldbu r3, r3+0
	slli r3, r3, 16
	or  r3, r4, r3
	or  r5, r3, r1
	lui r1, 16
	addi r1, r1, 1
	bge r5, r1, .LBB6_29
.LBB6_24:
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	addi r11, r0, 1
	add r4, r11, r0
	add r6, r13, r0
	jal r31, fread
	lui r3, %hi(fio_ulen)
	stw r3+%lo(fio_ulen), r1
	addi r3, sp, 20
	addi r5, r0, 4
	add r4, r11, r0
	add r6, r13, r0
	jal r31, fread
	add r12, r0, r0
	bne r12, r0, .LBB6_27
	jal r0, .LBB6_28
.LBB6_25:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r16, r1
	stw r1+0, r12
	andi r1, r11, 3
	beq r1, r0, .LBB6_32
.LBB6_26:
	beq r12, r0, .LBB6_28
.LBB6_27:
	stb r15+%lo(fio_busy), r0
.LBB6_28:
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
.LBB6_29:
	andi r1, r11, 2
	beq r1, r0, .LBB6_34
.LBB6_30:
	addi r12, r0, 2
	bne r12, r0, .LBB6_27
	jal r0, .LBB6_28
.LBB6_31:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	jal r0, .LBB6_35
.LBB6_32:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	jal r0, .LBB6_35
.LBB6_33:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.64)
	addi r4, r4, %lo(.L.str.64)
	jal r0, .LBB6_35
.LBB6_34:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.67)
	addi r4, r4, %lo(.L.str.67)
.LBB6_35:
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
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
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
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
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
	lui r8, %hi(fio_busy)
	ldbu r9, r8+%lo(fio_busy)
	addi r1, r0, 1
	beq r9, r1, .LBB10_15
.LBB10_1:
	stb r8+%lo(fio_busy), r1
	lui r8, %hi(fio_uend_init)
	ldbu r9, r8+%lo(fio_uend_init)
	bne r9, r0, .LBB10_5
.LBB10_2:
	addi r9, r0, -400
	lui r10, %hi(fio_uend+400)
	addi r10, r10, %lo(fio_uend+400)
	addi lr, r0, -1
.LBB10_3:
	add r11, r9, r10
	stw r11+0, lr
	addi r9, r9, 4
	bne r9, r0, .LBB10_3
.LBB10_4:
	stb r8+%lo(fio_uend_init), r1
.LBB10_5:
	lui r8, %hi(stdout)
	ldw r8, r8+%lo(stdout)
	lui r9, %hi(fio_out)
	stw r9+%lo(fio_out), r8
	lui r8, %hi(fio_unit)
	addi r11, r0, -1
	stw r8+%lo(fio_unit), r11
	lui r8, %hi(fio_ibuf)
	stw r8+%lo(fio_ibuf), r3
	lui r3, %hi(fio_ilen)
	stw r3+%lo(fio_ilen), r4
	lui r3, %hi(fio_inrec)
	stw r3+%lo(fio_inrec), r5
	lui r3, %hi(fio_irec)
	stw r3+%lo(fio_irec), r0
	lui r3, %hi(fio_reading)
	stb r3+%lo(fio_reading), r0
	lui r3, %hi(fio_len)
	stw r3+%lo(fio_len), r0
	lui r3, %hi(fio_col)
	stw r3+%lo(fio_col), r0
	lui r3, %hi(fio_sp)
	stb r3+%lo(fio_sp), r0
	lui r3, %hi(fio_gdepth)
	stw r3+%lo(fio_gdepth), r0
	lui r3, %hi(fio_scale)
	stw r3+%lo(fio_scale), r0
	lui r3, %hi(fio_rep)
	stw r3+%lo(fio_rep), r0
	lui r3, %hi(fio_desc)
	stw r3+%lo(fio_desc), r0
	lui r3, %hi(fio_first_item)
	stb r3+%lo(fio_first_item), r1
	lui r3, %hi(fio_prev_char)
	stb r3+%lo(fio_prev_char), r0
	lui r12, %hi(fio_pos)
	lui r13, %hi(fio_revert)
	beq r6, r0, .LBB10_12
.LBB10_6:
	lui r1, %hi(fio_listed)
	stw r1+%lo(fio_listed), r0
	add r3, r6, r0
	add r4, r7, r0
	jal r31, fio_fmt_use
	ble r1, r11, .LBB10_16
.LBB10_7:
	stw r12+%lo(fio_pos), r0
	stw r13+%lo(fio_revert), r11
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	lui r1, %hi(fio_fmt)
	ldw r3, r1+%lo(fio_fmt)
	addi r4, r0, 32
	add r1, r0, r0
.LBB10_8:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB10_10
.LBB10_9:
	addi r1, r1, 1
	stw r12+%lo(fio_pos), r1
	jal r0, .LBB10_8
.LBB10_10:
	addi r3, r0, 40
	bne r5, r3, .LBB10_14
.LBB10_11:
	addi r1, r1, 1
	add r11, r1, r0
	jal r0, .LBB10_13
.LBB10_12:
	lui r3, %hi(fio_listed)
	stw r3+%lo(fio_listed), r1
	lui r1, %hi(.L.str.7)
	addi r1, r1, %lo(.L.str.7)
	lui r3, %hi(fio_fmt)
	stw r3+%lo(fio_fmt), r1
	add r1, r0, r0
.LBB10_13:
	stw r12+%lo(fio_pos), r1
	stw r13+%lo(fio_revert), r11
.LBB10_14:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB10_15:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	jal r31, fprintf
.LBB10_16:
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
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
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
	lui r5, %hi(.L.str.4)
	addi r5, r5, %lo(.L.str.4)
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
	lui r3, %hi(.L.str.7)
	addi r3, r3, %lo(.L.str.7)
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r11, r3, r1
	ldw r1, r18+%lo(fio_d)
	sgt r3, r1, r14
	sub r1, r1, r14
	sub r3, r0, r3
	and r7, r1, r3
	lui r5, %hi(.L.str.5)
	addi r5, r5, %lo(.L.str.5)
	addi r3, sp, 60
	add r4, r13, r0
	add r6, r11, r0
	add r8, r0, r0
	add r9, r12, r0
	jal r31, snprintf
	ldw r1, r18+%lo(fio_d)
	blt r14, r1, .LBB11_54
.LBB11_36:
	lui r5, %hi(.L.str.8)
	addi r5, r5, %lo(.L.str.8)
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
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
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
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
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
	lui r5, %hi(.L.str.10)
	addi r5, r5, %lo(.L.str.10)
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
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
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
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
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
	addi r8, r8, 1
	fle.d lr, r6, r10
	xori lr, lr, 1
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
	lui r5, %hi(.L.str.10)
	addi r5, r5, %lo(.L.str.10)
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
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
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
	addi r23, r0, 1
	lui r1, %hi(.LCPI14_3)
	addi r1, r1, %lo(.LCPI14_3)
	ldw r5, r1+4
	ldw r4, r1+0
	fle.d r1, r4, r6
	xori r3, r1, 1
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
	lui r5, %hi(.L.str.10)
	addi r5, r5, %lo(.L.str.10)
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
	lui r1, %hi(.L.str.86)
	addi r1, r1, %lo(.L.str.86)
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
	lui r5, %hi(.L.str.86)
	addi r5, r5, %lo(.L.str.86)
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
	fmul.d r6, r6, r6
	fcvt.d.s r4, r4
	fmul.d r4, r4, r4
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
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI20_0, 8
	.type	.LCPI20_1,@object
.LCPI20_1:
	.quad	0x43f0000000000000              # double 1.8446744073709552E+19
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
	ldw r5, r1+4
	ldw r4, r1+0
	lui r1, %hi(.LCPI20_1)
	addi r1, r1, %lo(.LCPI20_1)
	ldw r9, r1+4
	ldw r8, r1+0
	flt.d r1, r8, r6
	xori r3, r1, 1
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
	fmul.d r6, r14, r14
	fmul.d r8, r16, r16
	fadd.d r6, r6, r8
	fcvt.d.s r8, r13
	fmul.d r18, r14, r8
	fcvt.d.s r12, r12
	fmul.d r20, r16, r12
	fadd.d r18, r18, r20
	fdiv.d r18, r18, r6
	fmul.d r18, r4, r18
	fcvt.s.d r1, r18
	stw r11+0, r1
	fmul.d r12, r14, r12
	fmul.d r8, r16, r8
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
	ldw r11, r1+4
	ldw r10, r1+0
	lui r1, %hi(.LCPI21_1)
	addi r1, r1, %lo(.LCPI21_1)
	ldw r9, r1+4
	ldw r8, r1+0
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
	fmul.d r14, r4, r10
	fmul.d r16, r12, r8
	fsub.d r14, r14, r16
	fmul.d r10, r12, r10
	fmul.d r8, r4, r8
	fadd.d r8, r8, r10
	add r10, r14, r0
	add r11, r15, r0
.LBB21_5:
	srli r1, r1, 1
	beq r1, r0, .LBB21_2
.LBB21_6:
	fmul.d r14, r4, r4
	fmul.d r16, r12, r12
	fsub.d r14, r14, r16
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
	fdiv.d r10, r10, r4
	fneg.d r6, r8
	fdiv.d r8, r6, r4
.LBB21_9:
	fcvt.s.d r1, r10
	stw r3+0, r1
	fcvt.s.d r1, r8
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
	fmul.d r6, r12, r12
	fcvt.d.s r14, r5
	fmul.d r4, r14, r14
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
	fmul.d r6, r14, r14
	fcvt.d.s r16, r5
	fmul.d r4, r16, r16
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
	fmul.d r8, r4, r6
	fcvt.d.s r12, r12
	fcvt.d.s r14, r1
	fmul.d r16, r12, r14
	fsub.d r8, r8, r16
	fmul.d r4, r4, r14
	fmul.d r6, r12, r6
	fadd.d r4, r6, r4
	fcvt.s.d r1, r8
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
	fmul.d r4, r16, r16
	fcvt.d.s r14, r12
	fmul.d r6, r14, r14
	fadd.d r4, r4, r6
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
	ldw r13, r3+%lo(.LCPI25_1)
	lui r1, %hi(.LCPI25_2)
	addi r1, r1, %lo(.LCPI25_2)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r16, r4
	add r1, r13, r0
	bne r3, r0, .LBB25_3
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
	.globl	f77_zdiv                        # -- Begin function f77_zdiv
	.p2align	2
	.type	f77_zdiv,@function
f77_zdiv:                               # @f77_zdiv
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
	add r11, r3, r0
	ldw r13, r4+4
	ldw r12, r4+0
	ldw r15, r4+12
	ldw r14, r4+8
	ldw r17, r5+4
	ldw r16, r5+0
	ldw r19, r5+12
	ldw r18, r5+8
	add r3, r16, r0
	add r4, r17, r0
	jal r31, fabs
	add r20, r1, r0
	add r21, r2, r0
	add r3, r18, r0
	add r4, r19, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fle.d r1, r4, r20
	xori r1, r1, 1
	beq r1, r0, .LBB28_2
.LBB28_1:
	fdiv.d r6, r16, r18
	fmul.d r4, r16, r6
	fadd.d r8, r18, r4
	fmul.d r4, r12, r6
	fadd.d r4, r14, r4
	fdiv.d r4, r4, r8
	fmul.d r6, r14, r6
	fsub.d r6, r6, r12
	jal r0, .LBB28_3
.LBB28_2:
	fdiv.d r6, r18, r16
	fmul.d r4, r18, r6
	fadd.d r8, r16, r4
	fmul.d r4, r14, r6
	fadd.d r4, r12, r4
	fdiv.d r4, r4, r8
	fmul.d r6, r12, r6
	fsub.d r6, r14, r6
.LBB28_3:
	fdiv.d r6, r6, r8
	stw r11+4, r5
	stw r11+0, r4
	stw r11+12, r7
	stw r11+8, r6
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
.Lfunc_end28:
	.size	f77_zdiv, .Lfunc_end28-f77_zdiv
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zpow_i
	.type	.LCPI29_0,@object
.LCPI29_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI29_0, 8
	.type	.LCPI29_1,@object
.LCPI29_1:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI29_1, 8
	.type	.LCPI29_2,@object
.LCPI29_2:
	.quad	0xbff0000000000000              # double -1
	.size	.LCPI29_2, 8
	.text
	.globl	f77_zpow_i
	.p2align	2
	.type	f77_zpow_i,@function
f77_zpow_i:                             # @f77_zpow_i
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
	stw sp+20, r22
	stw sp+16, r23
	lui r1, %hi(.LCPI29_0)
	addi r1, r1, %lo(.LCPI29_0)
	ldw r13, r1+4
	ldw r12, r1+0
	lui r1, %hi(.LCPI29_1)
	addi r1, r1, %lo(.LCPI29_1)
	ldw r15, r1+4
	ldw r14, r1+0
	beq r5, r0, .LBB29_11
.LBB29_1:
	ldw r17, r4+4
	ldw r16, r4+0
	ldw r19, r4+12
	ldw r18, r4+8
	addi r1, r0, -1
	bgt r5, r1, .LBB29_6
.LBB29_2:
	add r11, r3, r0
	sub r20, r0, r5
	add r3, r16, r0
	add r4, r17, r0
	jal r31, fabs
	add r22, r1, r0
	add r23, r2, r0
	add r3, r18, r0
	add r4, r19, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fle.d r1, r4, r22
	xori r1, r1, 1
	beq r1, r0, .LBB29_4
.LBB29_3:
	fdiv.d r4, r16, r18
	fmul.d r6, r16, r4
	fadd.d r6, r18, r6
	fadd.d r8, r4, r12
	fdiv.d r16, r8, r6
	fmul.d r4, r4, r12
	lui r1, %hi(.LCPI29_2)
	addi r1, r1, %lo(.LCPI29_2)
	ldw r9, r1+4
	ldw r8, r1+0
	fadd.d r4, r4, r8
	jal r0, .LBB29_5
.LBB29_4:
	fdiv.d r4, r18, r16
	fmul.d r6, r18, r4
	fadd.d r6, r16, r6
	fmul.d r8, r4, r12
	fadd.d r8, r8, r14
	fdiv.d r16, r8, r6
	fsub.d r4, r12, r4
.LBB29_5:
	fdiv.d r18, r4, r6
	add r3, r11, r0
	add r5, r20, r0
.LBB29_6:
	andi r1, r5, 1
	bne r1, r0, .LBB29_8
.LBB29_7:
	add r6, r14, r0
	add r7, r15, r0
	srli r5, r5, 1
	bne r5, r0, .LBB29_9
	jal r0, .LBB29_10
.LBB29_8:
	fmul.d r6, r14, r16
	fmul.d r8, r12, r18
	fsub.d r6, r6, r8
	fmul.d r8, r14, r18
	fmul.d r10, r12, r16
	fadd.d r12, r10, r8
	srli r5, r5, 1
	beq r5, r0, .LBB29_10
.LBB29_9:
	fmul.d r8, r16, r16
	fmul.d r10, r18, r18
	fsub.d r8, r8, r10
	fmul.d r10, r16, r18
	fadd.d r18, r10, r10
	add r14, r6, r0
	add r15, r7, r0
	add r16, r8, r0
	add r17, r9, r0
	andi r1, r5, 1
	beq r1, r0, .LBB29_7
	jal r0, .LBB29_8
.LBB29_10:
	add r14, r6, r0
	add r15, r7, r0
.LBB29_11:
	stw r3+4, r15
	stw r3+0, r14
	stw r3+12, r13
	stw r3+8, r12
	ldw r23, sp+16
	ldw r22, sp+20
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
.Lfunc_end29:
	.size	f77_zpow_i, .Lfunc_end29-f77_zpow_i
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zpow_z
	.type	.LCPI30_0,@object
.LCPI30_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI30_0, 8
	.text
	.globl	f77_zpow_z
	.p2align	2
	.type	f77_zpow_z,@function
f77_zpow_z:                             # @f77_zpow_z
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
	ldw r9, r4+4
	ldw r8, r4+0
	lui r1, %hi(.LCPI30_0)
	addi r1, r1, %lo(.LCPI30_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r8, r6
	xori r1, r1, 1
	bne r1, r0, .LBB30_3
.LBB30_1:
	ldw r15, r4+12
	ldw r14, r4+8
	feq.d r1, r14, r6
	xori r1, r1, 1
	bne r1, r0, .LBB30_3
.LBB30_2:
	stw r11+12, r0
	stw r11+8, r0
	stw r11+4, r0
	stw r11+0, r0
	jal r0, .LBB30_4
.LBB30_3:
	ldw r6, r4+12
	ldw r5, r4+8
	add r3, r8, r0
	add r13, r4, r0
	add r4, r9, r0
	jal r31, hypot
	add r3, r1, r0
	add r4, r2, r0
	jal r31, log
	add r14, r1, r0
	add r15, r2, r0
	ldw r4, r13+12
	ldw r3, r13+8
	ldw r6, r13+4
	ldw r5, r13+0
	jal r31, atan2
	add r4, r1, r0
	add r5, r2, r0
	ldw r7, r12+4
	ldw r6, r12+0
	fmul.d r8, r14, r6
	ldw r17, r12+12
	ldw r16, r12+8
	fmul.d r12, r4, r16
	fsub.d r8, r8, r12
	fmul.d r4, r4, r6
	fmul.d r6, r14, r16
	fadd.d r14, r4, r6
	add r3, r8, r0
	add r4, r9, r0
	jal r31, exp
	add r12, r1, r0
	add r13, r2, r0
	add r3, r14, r0
	add r4, r15, r0
	jal r31, cos
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r12, r4
	stw r11+4, r5
	stw r11+0, r4
	add r3, r14, r0
	add r4, r15, r0
	jal r31, sin
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r12, r4
	stw r11+12, r5
	stw r11+8, r4
.LBB30_4:
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
.Lfunc_end30:
	.size	f77_zpow_z, .Lfunc_end30-f77_zpow_z
                                        # -- End function
	.globl	f77_zabs                        # -- Begin function f77_zabs
	.p2align	2
	.type	f77_zabs,@function
f77_zabs:                               # @f77_zabs
# %bb.0:
	ldw r4, r3+4
	ldw r1, r3+0
	ldw r6, r3+12
	ldw r5, r3+8
	add r3, r1, r0
	jal r0, hypot
.Lfunc_end31:
	.size	f77_zabs, .Lfunc_end31-f77_zabs
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
.Lfunc_end32:
	.size	f77i_iabs, .Lfunc_end32-f77i_iabs
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_abs
	.type	.LCPI33_0,@object
.LCPI33_0:
	.word	0x00000000                      # float 0
	.size	.LCPI33_0, 4
	.text
	.globl	f77i_abs
	.p2align	2
	.type	f77i_abs,@function
f77i_abs:                               # @f77i_abs
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI33_0)
	ldw r3, r3+%lo(.LCPI33_0)
	flt.s r3, r1, r3
	sub r3, r0, r3
	lui r4, 524288
	and r3, r3, r4
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end33:
	.size	f77i_abs, .Lfunc_end33-f77i_abs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dabs
	.type	.LCPI34_0,@object
.LCPI34_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI34_0, 8
	.text
	.globl	f77i_dabs
	.p2align	2
	.type	f77i_dabs,@function
f77i_dabs:                              # @f77i_dabs
# %bb.0:
	ldw r5, r3+4
	ldw r4, r3+0
	lui r1, %hi(.LCPI34_0)
	addi r1, r1, %lo(.LCPI34_0)
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
.Lfunc_end34:
	.size	f77i_dabs, .Lfunc_end34-f77i_dabs
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
.Lfunc_end35:
	.size	f77i_sqrt, .Lfunc_end35-f77i_sqrt
                                        # -- End function
	.globl	f77i_dsqrt                      # -- Begin function f77i_dsqrt
	.p2align	2
	.type	f77i_dsqrt,@function
f77i_dsqrt:                             # @f77i_dsqrt
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sqrt
.Lfunc_end36:
	.size	f77i_dsqrt, .Lfunc_end36-f77i_dsqrt
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
.Lfunc_end37:
	.size	f77i_exp, .Lfunc_end37-f77i_exp
                                        # -- End function
	.globl	f77i_dexp                       # -- Begin function f77i_dexp
	.p2align	2
	.type	f77i_dexp,@function
f77i_dexp:                              # @f77i_dexp
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, exp
.Lfunc_end38:
	.size	f77i_dexp, .Lfunc_end38-f77i_dexp
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
.Lfunc_end39:
	.size	f77i_alog, .Lfunc_end39-f77i_alog
                                        # -- End function
	.globl	f77i_dlog                       # -- Begin function f77i_dlog
	.p2align	2
	.type	f77i_dlog,@function
f77i_dlog:                              # @f77i_dlog
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log
.Lfunc_end40:
	.size	f77i_dlog, .Lfunc_end40-f77i_dlog
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
.Lfunc_end41:
	.size	f77i_alog10, .Lfunc_end41-f77i_alog10
                                        # -- End function
	.globl	f77i_dlog10                     # -- Begin function f77i_dlog10
	.p2align	2
	.type	f77i_dlog10,@function
f77i_dlog10:                            # @f77i_dlog10
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log10
.Lfunc_end42:
	.size	f77i_dlog10, .Lfunc_end42-f77i_dlog10
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
.Lfunc_end43:
	.size	f77i_sin, .Lfunc_end43-f77i_sin
                                        # -- End function
	.globl	f77i_dsin                       # -- Begin function f77i_dsin
	.p2align	2
	.type	f77i_dsin,@function
f77i_dsin:                              # @f77i_dsin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sin
.Lfunc_end44:
	.size	f77i_dsin, .Lfunc_end44-f77i_dsin
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
.Lfunc_end45:
	.size	f77i_cos, .Lfunc_end45-f77i_cos
                                        # -- End function
	.globl	f77i_dcos                       # -- Begin function f77i_dcos
	.p2align	2
	.type	f77i_dcos,@function
f77i_dcos:                              # @f77i_dcos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cos
.Lfunc_end46:
	.size	f77i_dcos, .Lfunc_end46-f77i_dcos
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
.Lfunc_end47:
	.size	f77i_tan, .Lfunc_end47-f77i_tan
                                        # -- End function
	.globl	f77i_dtan                       # -- Begin function f77i_dtan
	.p2align	2
	.type	f77i_dtan,@function
f77i_dtan:                              # @f77i_dtan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tan
.Lfunc_end48:
	.size	f77i_dtan, .Lfunc_end48-f77i_dtan
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
.Lfunc_end49:
	.size	f77i_asin, .Lfunc_end49-f77i_asin
                                        # -- End function
	.globl	f77i_dasin                      # -- Begin function f77i_dasin
	.p2align	2
	.type	f77i_dasin,@function
f77i_dasin:                             # @f77i_dasin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, asin
.Lfunc_end50:
	.size	f77i_dasin, .Lfunc_end50-f77i_dasin
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
.Lfunc_end51:
	.size	f77i_acos, .Lfunc_end51-f77i_acos
                                        # -- End function
	.globl	f77i_dacos                      # -- Begin function f77i_dacos
	.p2align	2
	.type	f77i_dacos,@function
f77i_dacos:                             # @f77i_dacos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, acos
.Lfunc_end52:
	.size	f77i_dacos, .Lfunc_end52-f77i_dacos
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
.Lfunc_end53:
	.size	f77i_atan, .Lfunc_end53-f77i_atan
                                        # -- End function
	.globl	f77i_datan                      # -- Begin function f77i_datan
	.p2align	2
	.type	f77i_datan,@function
f77i_datan:                             # @f77i_datan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, atan
.Lfunc_end54:
	.size	f77i_datan, .Lfunc_end54-f77i_datan
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
.Lfunc_end55:
	.size	f77i_atan2, .Lfunc_end55-f77i_atan2
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
.Lfunc_end56:
	.size	f77i_datan2, .Lfunc_end56-f77i_datan2
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
.Lfunc_end57:
	.size	f77i_sinh, .Lfunc_end57-f77i_sinh
                                        # -- End function
	.globl	f77i_dsinh                      # -- Begin function f77i_dsinh
	.p2align	2
	.type	f77i_dsinh,@function
f77i_dsinh:                             # @f77i_dsinh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sinh
.Lfunc_end58:
	.size	f77i_dsinh, .Lfunc_end58-f77i_dsinh
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
.Lfunc_end59:
	.size	f77i_cosh, .Lfunc_end59-f77i_cosh
                                        # -- End function
	.globl	f77i_dcosh                      # -- Begin function f77i_dcosh
	.p2align	2
	.type	f77i_dcosh,@function
f77i_dcosh:                             # @f77i_dcosh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cosh
.Lfunc_end60:
	.size	f77i_dcosh, .Lfunc_end60-f77i_dcosh
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
.Lfunc_end61:
	.size	f77i_tanh, .Lfunc_end61-f77i_tanh
                                        # -- End function
	.globl	f77i_dtanh                      # -- Begin function f77i_dtanh
	.p2align	2
	.type	f77i_dtanh,@function
f77i_dtanh:                             # @f77i_dtanh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tanh
.Lfunc_end62:
	.size	f77i_dtanh, .Lfunc_end62-f77i_dtanh
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
.Lfunc_end63:
	.size	f77i_aint, .Lfunc_end63-f77i_aint
                                        # -- End function
	.globl	f77i_dint                       # -- Begin function f77i_dint
	.p2align	2
	.type	f77i_dint,@function
f77i_dint:                              # @f77i_dint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, trunc
.Lfunc_end64:
	.size	f77i_dint, .Lfunc_end64-f77i_dint
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
.Lfunc_end65:
	.size	f77i_anint, .Lfunc_end65-f77i_anint
                                        # -- End function
	.globl	f77i_dnint                      # -- Begin function f77i_dnint
	.p2align	2
	.type	f77i_dnint,@function
f77i_dnint:                             # @f77i_dnint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, round
.Lfunc_end66:
	.size	f77i_dnint, .Lfunc_end66-f77i_dnint
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
.Lfunc_end67:
	.size	f77i_nint, .Lfunc_end67-f77i_nint
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
.Lfunc_end68:
	.size	f77i_idnint, .Lfunc_end68-f77i_idnint
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
.Lfunc_end69:
	.size	f77i_mod, .Lfunc_end69-f77i_mod
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
.Lfunc_end70:
	.size	f77i_amod, .Lfunc_end70-f77i_amod
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
.Lfunc_end71:
	.size	f77i_dmod, .Lfunc_end71-f77i_dmod
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
.Lfunc_end72:
	.size	f77i_isign, .Lfunc_end72-f77i_isign
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_sign
	.type	.LCPI73_0,@object
.LCPI73_0:
	.word	0x00000000                      # float 0
	.size	.LCPI73_0, 4
	.text
	.globl	f77i_sign
	.p2align	2
	.type	f77i_sign,@function
f77i_sign:                              # @f77i_sign
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI73_0)
	ldw r3, r3+%lo(.LCPI73_0)
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
.Lfunc_end73:
	.size	f77i_sign, .Lfunc_end73-f77i_sign
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dsign
	.type	.LCPI74_0,@object
.LCPI74_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI74_0, 8
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
	lui r1, %hi(.LCPI74_0)
	addi r1, r1, %lo(.LCPI74_0)
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
.Lfunc_end74:
	.size	f77i_dsign, .Lfunc_end74-f77i_dsign
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
.Lfunc_end75:
	.size	f77i_idim, .Lfunc_end75-f77i_idim
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
.Lfunc_end76:
	.size	f77i_dim, .Lfunc_end76-f77i_dim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_ddim
	.type	.LCPI77_0,@object
.LCPI77_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI77_0, 8
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
	lui r3, %hi(.LCPI77_0)
	addi r3, r3, %lo(.LCPI77_0)
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
.Lfunc_end77:
	.size	f77i_ddim, .Lfunc_end77-f77i_ddim
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
.Lfunc_end78:
	.size	f77i_dprod, .Lfunc_end78-f77i_dprod
                                        # -- End function
	.globl	f77i_len                        # -- Begin function f77i_len
	.p2align	2
	.type	f77i_len,@function
f77i_len:                               # @f77i_len
# %bb.0:
	add r1, r4, r0
	jalr r0, r31, 0
.Lfunc_end79:
	.size	f77i_len, .Lfunc_end79-f77i_len
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
	beq r6, r0, .LBB80_6
.LBB80_1:
	add r12, r6, r0
	bgt r6, r11, .LBB80_5
.LBB80_2:
	add r13, r4, r0
	add r14, r3, r0
	add r15, r0, r0
.LBB80_3:
	add r3, r14, r15
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcmp
	beq r1, r0, .LBB80_7
.LBB80_4:
	addi r15, r15, 1
	add r1, r12, r15
	ble r1, r11, .LBB80_3
.LBB80_5:
	add r1, r0, r0
	jal r0, .LBB80_8
.LBB80_6:
	addi r1, r0, 0
	sgt r1, r11, r1
	jal r0, .LBB80_8
.LBB80_7:
	addi r1, r15, 1
.LBB80_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end80:
	.size	f77i_index, .Lfunc_end80-f77i_index
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
	beq r6, r0, .LBB81_6
.LBB81_1:
	add r12, r6, r0
	bgt r6, r11, .LBB81_5
.LBB81_2:
	add r13, r5, r0
	add r14, r3, r0
	add r15, r0, r0
.LBB81_3:
	add r3, r14, r15
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcmp
	beq r1, r0, .LBB81_7
.LBB81_4:
	addi r15, r15, 1
	add r1, r12, r15
	ble r1, r11, .LBB81_3
.LBB81_5:
	add r1, r0, r0
	jal r0, .LBB81_8
.LBB81_6:
	addi r1, r0, 0
	sgt r1, r11, r1
	jal r0, .LBB81_8
.LBB81_7:
	addi r1, r15, 1
.LBB81_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end81:
	.size	f77_index, .Lfunc_end81-f77_index
                                        # -- End function
	.globl	f77i_aimag                      # -- Begin function f77i_aimag
	.p2align	2
	.type	f77i_aimag,@function
f77i_aimag:                             # @f77i_aimag
# %bb.0:
	ldw r1, r3+4
	jalr r0, r31, 0
.Lfunc_end82:
	.size	f77i_aimag, .Lfunc_end82-f77i_aimag
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
	fmul.d r4, r4, r4
	ldw r1, r3+4
	fcvt.d.s r6, r1
	fmul.d r6, r6, r6
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
.Lfunc_end83:
	.size	f77i_cabs, .Lfunc_end83-f77i_cabs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_csqrt
	.type	.LCPI84_0,@object
.LCPI84_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI84_0, 8
	.type	.LCPI84_2,@object
.LCPI84_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI84_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI84_1,@object
.LCPI84_1:
	.word	0x00000000                      # float 0
	.size	.LCPI84_1, 4
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
	fmul.d r4, r14, r14
	fcvt.d.s r12, r11
	fmul.d r6, r12, r12
	fadd.d r4, r4, r6
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
	lui r1, %hi(.LCPI84_0)
	addi r1, r1, %lo(.LCPI84_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r14, r1, r0
	add r15, r2, r0
	lui r1, %hi(.LCPI84_1)
	ldw r3, r1+%lo(.LCPI84_1)
	fle.s r4, r3, r16
	xori r4, r4, 1
	beq r4, r0, .LBB84_4
.LBB84_1:
	ldw r16, r1+%lo(.LCPI84_1)
	lui r1, %hi(.LCPI84_2)
	addi r1, r1, %lo(.LCPI84_2)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r14, r4
	add r1, r16, r0
	bne r3, r0, .LBB84_3
.LBB84_2:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r14, r14
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
.LBB84_3:
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
	jal r0, .LBB84_6
.LBB84_4:
	fcvt.s.d r1, r14
	lui r4, %hi(.LCPI84_2)
	addi r4, r4, %lo(.LCPI84_2)
	ldw r7, r4+4
	ldw r6, r4+0
	feq.d r4, r14, r6
	bne r4, r0, .LBB84_6
.LBB84_5:
	fadd.d r4, r14, r14
	fdiv.d r4, r12, r4
	fcvt.s.d r3, r4
.LBB84_6:
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
.Lfunc_end84:
	.size	f77i_csqrt, .Lfunc_end84-f77i_csqrt
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
.Lfunc_end85:
	.size	f77i_cexp, .Lfunc_end85-f77i_cexp
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
	fmul.d r4, r12, r12
	fcvt.d.s r14, r3
	fmul.d r6, r14, r14
	fadd.d r4, r4, r6
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
.Lfunc_end86:
	.size	f77i_clog, .Lfunc_end86-f77i_clog
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
.Lfunc_end87:
	.size	f77i_csin, .Lfunc_end87-f77i_csin
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
.Lfunc_end88:
	.size	f77i_ccos, .Lfunc_end88-f77i_ccos
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
.Lfunc_end89:
	.size	f77i_conjg, .Lfunc_end89-f77i_conjg
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
	blt r14, r1, .LBB90_2
.LBB90_1:
	add r3, r12, r0
	add r4, r5, r0
	add r5, r14, r0
	jal r31, memmove
.LBB90_2:
	bge r13, r11, .LBB90_5
.LBB90_3:
	add r1, r12, r14
	sub r3, r11, r14
	addi r4, r0, 32
.LBB90_4:
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB90_4
.LBB90_5:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end90:
	.size	f77_strcpy, .Lfunc_end90-f77_strcpy
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
	blt r7, r1, .LBB91_8
.LBB91_1:
	addi r8, r0, 32
	add r9, r0, r0
                                        # implicit-def: $r1
.LBB91_2:
	add r10, r8, r0
	bge r9, r4, .LBB91_4
.LBB91_3:
	add r10, r3, r9
	ldbu r10, r10+0
.LBB91_4:
	add lr, r8, r0
	bge r9, r6, .LBB91_6
.LBB91_5:
	add lr, r5, r9
	ldbu lr, lr+0
.LBB91_6:
	seq r11, r10, lr
	sltu r12, r10, lr
	sub r12, r0, r12
	ori  r12, r12, 1
	xor r1, r1, r12
	sub r11, r0, r11
	and r1, r1, r11
	xor r1, r12, r1
	bne r10, lr, .LBB91_9
.LBB91_7:
	addi r9, r9, 1
	bne r7, r9, .LBB91_2
.LBB91_8:
	add r1, r0, r0
.LBB91_9:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end91:
	.size	f77_strcmp, .Lfunc_end91-f77_strcmp
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
	bgeu r5, r6, .LBB92_8
.LBB92_1:
	lui r3, %hi(fio_catpos)
	ldw r7, r3+%lo(fio_catpos)
	add r7, r7, r5
	blt r7, r6, .LBB92_3
.LBB92_2:
	stw r3+%lo(fio_catpos), r0
.LBB92_3:
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
	blt r1, r15, .LBB92_5
.LBB92_4:
	add r3, r13, r0
	add r5, r1, r0
	jal r31, memmove
.LBB92_5:
	blt r11, r15, .LBB92_7
.LBB92_6:
	add r3, r13, r14
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memmove
.LBB92_7:
	add r1, r13, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB92_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end92:
	.size	f77_cat, .Lfunc_end92-f77_cat
                                        # -- End function
	.globl	f77_wr_l                        # -- Begin function f77_wr_l
	.p2align	2
	.type	f77_wr_l,@function
f77_wr_l:                               # @f77_wr_l
# %bb.0:
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB93_15
.LBB93_1:
	lui r4, %hi(fio_col)
	ldw r7, r4+%lo(fio_col)
	addi r6, r0, 1022
	lui r1, %hi(fio_len)
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	bgt r7, r6, .LBB93_7
.LBB93_2:
	ldw r9, r1+%lo(fio_len)
	addi r8, r0, 32
	bge r9, r7, .LBB93_5
.LBB93_3:
	addi r10, r9, 1
	add r9, r9, r5
	stb r9+0, r8
	add r9, r10, r0
	bne r7, r10, .LBB93_3
.LBB93_4:
	stw r1+%lo(fio_len), r7
	add r9, r7, r0
.LBB93_5:
	add r10, r7, r5
	stb r10+0, r8
	addi r8, r7, 1
	stw r4+%lo(fio_col), r8
	blt r7, r9, .LBB93_7
.LBB93_6:
	stw r1+%lo(fio_len), r8
.LBB93_7:
	ldw r7, r4+%lo(fio_col)
	bgt r7, r6, .LBB93_14
.LBB93_8:
	ldw r6, r1+%lo(fio_len)
	bge r6, r7, .LBB93_12
.LBB93_9:
	addi r8, r0, 32
.LBB93_10:
	addi r9, r6, 1
	add r6, r6, r5
	stb r6+0, r8
	add r6, r9, r0
	bne r7, r9, .LBB93_10
.LBB93_11:
	stw r1+%lo(fio_len), r7
	add r6, r7, r0
.LBB93_12:
	addi r8, r0, 0
	seq r3, r3, r8
	sub r3, r0, r3
	andi r3, r3, 18
	xori r3, r3, 84
	add r5, r7, r5
	stb r5+0, r3
	addi r3, r7, 1
	stw r4+%lo(fio_col), r3
	blt r7, r6, .LBB93_14
.LBB93_13:
	stw r1+%lo(fio_len), r3
.LBB93_14:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	jal r0, .LBB93_32
.LBB93_15:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB93_17
.LBB93_16:
	ldw r1, r12+%lo(fio_rep)
	bgt r1, r0, .LBB93_18
.LBB93_17:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	jal r31, fio_next_desc
	add r3, r13, r0
	beq r1, r0, .LBB93_31
.LBB93_18:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r4, r0, 2
	blt r1, r4, .LBB93_20
.LBB93_19:
	lui r4, %hi(fio_col)
	ldw r5, r4+%lo(fio_col)
	add r1, r1, r5
	addi r1, r1, -1
	stw r4+%lo(fio_col), r1
.LBB93_20:
	lui r4, %hi(fio_col)
	ldw r1, r4+%lo(fio_col)
	addi r5, r0, 1022
	bgt r1, r5, .LBB93_27
.LBB93_21:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	bge r6, r1, .LBB93_25
.LBB93_22:
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
.LBB93_23:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r1, r9, .LBB93_23
.LBB93_24:
	stw r5+%lo(fio_len), r1
	add r6, r1, r0
.LBB93_25:
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
	blt r1, r6, .LBB93_27
.LBB93_26:
	stw r5+%lo(fio_len), r3
.LBB93_27:
	ldw r1, r12+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB93_29
.LBB93_28:
	addi r1, r1, -1
	stw r12+%lo(fio_rep), r1
.LBB93_29:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB93_31
.LBB93_30:
	stw r11+%lo(fio_desc), r0
.LBB93_31:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB93_32:
	jalr r0, r31, 0
.Lfunc_end93:
	.size	f77_wr_l, .Lfunc_end93-f77_wr_l
                                        # -- End function
	.globl	f77_wr_end                      # -- Begin function f77_wr_end
	.p2align	2
	.type	f77_wr_end,@function
f77_wr_end:                             # @f77_wr_end
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB94_4
.LBB94_1:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB94_3
.LBB94_2:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB94_4
.LBB94_3:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB94_4:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end94:
	.size	f77_wr_end, .Lfunc_end94-f77_wr_end
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
	lui r1, %hi(.LJTI95_4)
	addi r1, r1, %lo(.LJTI95_4)
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
	jal r0, .LBB95_5
.LBB95_1:
	addi r1, r3, 1
.LBB95_2:
	stw r13+%lo(fio_pos), r1
.LBB95_3:
	bgtu r14, r17, .LBB95_107
.LBB95_4:
	addi r14, r14, 1
.LBB95_5:
	ldw r27, r15+%lo(fio_fmt)
	ldw r3, r13+%lo(fio_pos)
	add r6, r27, r3
	ldb r5, r6+0
	andi r1, r5, 255
	beq r1, r16, .LBB95_1
.LBB95_6:
	beq r1, r18, .LBB95_1
.LBB95_7:
	bne r1, r0, .LBB95_16
.LBB95_8:
	ldw r1, r19+%lo(fio_gdepth)
	blt r1, r25, .LBB95_10
.LBB95_9:
	stw r19+%lo(fio_gdepth), r0
.LBB95_10:
	ldbu r1, r20+%lo(fio_draining)
	bne r1, r0, .LBB95_107
.LBB95_11:
	ldw r3, r21+%lo(fio_revert)
	add r1, r0, r0
	blt r3, r0, .LBB95_108
.LBB95_12:
	ldbu r1, r12+%lo(fio_since_data)
	beq r1, r0, .LBB95_119
.LBB95_13:
	stb r12+%lo(fio_since_data), r0
	stw r13+%lo(fio_pos), r3
	add r1, r27, r3
	ldbu r1, r1+0
	beq r1, r0, .LBB95_107
.LBB95_14:
	ldbu r1, r22+%lo(fio_reading)
	bne r1, r25, .LBB95_88
.LBB95_15:
	jal r31, fio_getrec
	beq r1, r0, .LBB95_3
	jal r0, .LBB95_120
.LBB95_16:
	and r4, r5, r23
	bne r4, r24, .LBB95_20
.LBB95_17:
	addi r1, r3, 1
	stw r13+%lo(fio_pos), r1
	add r1, r27, r1
	ldbu r1, r1+0
	addi r1, r1, -80
	ldw r4, sp+52
	bgtu r1, r4, .LBB95_50
.LBB95_18:
	slli r1, r1, 2
	ldw r4, sp+48
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB95_19:
	stb r26+%lo(fio_sp), r25
	jal r0, .LBB95_28
.LBB95_20:
	bne r5, r28, .LBB95_23
.LBB95_21:
	addi r1, r3, 1
	stw r13+%lo(fio_pos), r1
	ldbu r1, r20+%lo(fio_draining)
	bne r1, r0, .LBB95_107
.LBB95_22:
	add r1, r0, r0
	bleu r14, r17, .LBB95_4
	jal r0, .LBB95_108
.LBB95_23:
	andi r4, r5, 223
	ldw r7, sp+44
	bne r4, r7, .LBB95_29
.LBB95_24:
	ldbu r4, r6+1
	addi r7, r4, -78
	andi r8, r7, 252
	slli r7, r7, 6
	srli r8, r8, 2
	or  r7, r7, r8
	andi r7, r7, 255
	ldw r8, sp+36
	bgtu r7, r8, .LBB95_29
.LBB95_25:
	sll r7, r25, r7
	andi r7, r7, 2313
	beq r7, r0, .LBB95_29
.LBB95_26:
	andi r1, r4, 223
	ldw r4, sp+32
	seq r1, r1, r4
	ldw r4, sp+28
	stw r4+%lo(fio_bz), r1
	jal r0, .LBB95_28
.LBB95_27:
	stb r26+%lo(fio_sp), r0
.LBB95_28:
	addi r1, r3, 2
	jal r0, .LBB95_2
.LBB95_29:
	addi r4, r1, -39
	lui r1, %hi(fio_line)
	addi r1, r1, %lo(fio_line)
	ldw r7, sp+40
	bgtu r4, r7, .LBB95_53
.LBB95_30:
	slli r4, r4, 2
	lui r7, %hi(.LJTI95_0)
	addi r7, r7, %lo(.LJTI95_0)
	add r4, r7, r4
	ldw r7, r4+0
	add r4, r25, r0
	jalr r0, r7, 0
.LBB95_31:
	addi r3, r3, 1
	stw r13+%lo(fio_pos), r3
	lui r4, %hi(fio_col)
	ldw r9, r4+%lo(fio_col)
	lui r5, %hi(fio_len)
	ldw r7, r5+%lo(fio_len)
	ldbu r6, r22+%lo(fio_reading)
	jal r0, .LBB95_35
.LBB95_32:
	addi r10, r0, 1
.LBB95_33:
	add r9, r8, r0
.LBB95_34:
	add r3, r3, r10
	stw r13+%lo(fio_pos), r3
.LBB95_35:
	add r8, r9, r0
	add r9, r27, r3
	ldbu r9, r9+0
	addi r10, r0, 39
	beq r9, r10, .LBB95_42
.LBB95_36:
	beq r9, r0, .LBB95_3
.LBB95_37:
	addi r10, r0, 1022
	sgt r10, r8, r10
	or  r10, r6, r10
	andi r10, r10, 1
	bne r10, r0, .LBB95_32
.LBB95_38:
	bge r7, r8, .LBB95_41
.LBB95_39:
	addi r10, r7, 1
	add r7, r7, r1
	stb r7+0, r18
	add r7, r10, r0
	bne r8, r10, .LBB95_39
.LBB95_40:
	stw r5+%lo(fio_len), r8
	add r7, r8, r0
.LBB95_41:
	add r10, r8, r1
	stb r10+0, r9
	addi r9, r8, 1
	stw r4+%lo(fio_col), r9
	addi r10, r0, 1
	blt r8, r7, .LBB95_34
	jal r0, .LBB95_49
.LBB95_42:
	addi r9, r3, 1
	add lr, r27, r9
	ldbu lr, lr+0
	bne lr, r10, .LBB95_82
.LBB95_43:
	addi r9, r0, 1022
	sgt r9, r8, r9
	or  r9, r6, r9
	andi r9, r9, 1
	beq r9, r0, .LBB95_45
.LBB95_44:
	addi r10, r0, 2
	jal r0, .LBB95_33
.LBB95_45:
	bge r7, r8, .LBB95_48
.LBB95_46:
	addi r9, r7, 1
	add r7, r7, r1
	stb r7+0, r18
	add r7, r9, r0
	bne r8, r9, .LBB95_46
.LBB95_47:
	stw r5+%lo(fio_len), r8
	add r7, r8, r0
.LBB95_48:
	add r9, r8, r1
	stb r9+0, r10
	addi r9, r8, 1
	stw r4+%lo(fio_col), r9
	addi r10, r0, 2
	blt r8, r7, .LBB95_34
.LBB95_49:
	stw r5+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB95_34
.LBB95_50:
	stb r26+%lo(fio_sp), r0
	jal r0, .LBB95_3
.LBB95_51:
	ldb r4, r6+1
	addi r4, r4, -58
	addi r5, r0, -10
	bgeu r4, r5, .LBB95_58
.LBB95_52:
	addi r5, r0, 45
.LBB95_53:
	add r4, r25, r0
	jal r0, .LBB95_59
.LBB95_54:
	ldw r1, r19+%lo(fio_gdepth)
	beq r1, r0, .LBB95_83
.LBB95_55:
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
	blt r5, r4, .LBB95_3
.LBB95_56:
	lui r4, %hi(fio_gstart)
	addi r4, r4, %lo(fio_gstart)
	add r3, r3, r4
	ldw r3, r3+0
	stw r13+%lo(fio_pos), r3
	stw r19+%lo(fio_gdepth), r1
	jal r0, .LBB95_3
.LBB95_57:
	addi r1, r3, 1
	stw r13+%lo(fio_pos), r1
	jal r0, .LBB95_87
.LBB95_58:
	addi r4, r3, 1
	stw r13+%lo(fio_pos), r4
	add r4, r27, r4
	ldb r5, r4+0
	add r4, r0, r0
.LBB95_59:
	addi r5, r5, -58
	addi lr, r0, -10
	add fp, r25, r0
	bltu r5, lr, .LBB95_65
.LBB95_60:
	ldw r5, r13+%lo(fio_pos)
	add r6, r27, r5
	ldb r6, r6+0
	addi r6, r6, -48
	addi r7, r0, 9
	bleu r6, r7, .LBB95_62
.LBB95_61:
	add fp, r0, r0
	jal r0, .LBB95_65
.LBB95_62:
	addi r7, r27, 1
	add fp, r0, r0
.LBB95_63:
	addi r9, r0, 10
	mul r10, fp, r9
	addi r8, r5, 1
	add fp, r10, r6
	add r5, r7, r5
	ldb r5, r5+0
	addi r6, r5, -48
	add r5, r8, r0
	bltu r6, r9, .LBB95_63
.LBB95_64:
	stw r13+%lo(fio_pos), r8
.LBB95_65:
	ldw r5, r13+%lo(fio_pos)
	add r6, r27, r5
	ldbu r11, r6+0
	addi r6, r0, 40
	bne r11, r6, .LBB95_69
.LBB95_66:
	addi r4, r5, 1
	stw r13+%lo(fio_pos), r4
	ldw r1, r19+%lo(fio_gdepth)
	addi r5, r0, 15
	bgt r1, r5, .LBB95_3
.LBB95_67:
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
	bne r1, r0, .LBB95_3
.LBB95_68:
	stw r21+%lo(fio_revert), r3
	jal r0, .LBB95_3
.LBB95_69:
	addi r3, r5, 1
	andi r6, r11, 223
	addi r6, r6, -72
	addi r7, r0, 16
	bgtu r6, r7, .LBB95_101
.LBB95_70:
	slli r6, r6, 2
	lui r7, %hi(.LJTI95_1)
	addi r7, r7, %lo(.LJTI95_1)
	add r6, r7, r6
	ldw r6, r6+0
	jalr r0, r6, 0
.LBB95_71:
	addi r4, r0, 1
	blt fp, r4, .LBB95_97
.LBB95_72:
	lui r5, %hi(fio_len)
	ldw r8, r5+%lo(fio_len)
	lui r6, %hi(fio_col)
	ldw r9, r6+%lo(fio_col)
	ldbu r7, r22+%lo(fio_reading)
	jal r0, .LBB95_75
.LBB95_73:
	add r9, lr, r0
.LBB95_74:
	addi fp, r10, -1
	addi r3, r3, 1
	ble r10, r4, .LBB95_97
.LBB95_75:
	add lr, r27, r3
	ldbu lr, lr+0
	beq lr, r0, .LBB95_97
.LBB95_76:
	add r10, fp, r0
	addi r11, r0, 1022
	sgt r11, r9, r11
	or  r11, r7, r11
	andi r11, r11, 1
	bne r11, r0, .LBB95_74
.LBB95_77:
	bge r8, r9, .LBB95_80
.LBB95_78:
	addi r11, r8, 1
	add r8, r8, r1
	stb r8+0, r18
	add r8, r11, r0
	bne r9, r11, .LBB95_78
.LBB95_79:
	stw r5+%lo(fio_len), r9
	add r8, r9, r0
.LBB95_80:
	add r11, r9, r1
	stb r11+0, lr
	addi lr, r9, 1
	stw r6+%lo(fio_col), lr
	blt r9, r8, .LBB95_73
.LBB95_81:
	stw r5+%lo(fio_len), lr
	add r8, lr, r0
	jal r0, .LBB95_73
.LBB95_82:
	stw r13+%lo(fio_pos), r9
	jal r0, .LBB95_3
.LBB95_83:
	ldbu r1, r20+%lo(fio_draining)
	bne r1, r0, .LBB95_107
.LBB95_84:
	ldw r3, r21+%lo(fio_revert)
	add r1, r0, r0
	blt r3, r0, .LBB95_108
.LBB95_85:
	ldbu r1, r12+%lo(fio_since_data)
	beq r1, r0, .LBB95_119
.LBB95_86:
	stb r12+%lo(fio_since_data), r0
	stw r13+%lo(fio_pos), r3
	add r1, r27, r3
	ldbu r1, r1+0
	beq r1, r0, .LBB95_107
.LBB95_87:
	ldbu r1, r22+%lo(fio_reading)
	addi r3, r0, 1
	beq r1, r3, .LBB95_15
.LBB95_88:
	jal r31, fio_flush_line
	jal r0, .LBB95_3
.LBB95_89:
	stw r13+%lo(fio_pos), r3
	add r1, r27, r3
	ldbu r1, r1+0
	add fp, r0, r0
	addi r11, r0, 1
	addi r3, r1, -76
	addi r1, r0, 38
	bgtu r3, r1, .LBB95_106
.LBB95_90:
	addi r1, r0, -1
	slli r3, r3, 2
	lui r4, %hi(.LJTI95_2)
	addi r4, r4, %lo(.LJTI95_2)
	add r3, r4, r3
	ldw r3, r3+0
	add r6, r11, r0
	jalr r0, r3, 0
.LBB95_91:
	addi r1, r0, 1
.LBB95_92:
	addi r3, r5, 2
	stw r13+%lo(fio_pos), r3
	add r6, r0, r0
	add fp, r1, r0
.LBB95_93:
	ldw r1, r13+%lo(fio_pos)
	add r1, r27, r1
	ldb r1, r1+0
	addi r3, r1, -58
	add r1, r11, r0
	bltu r3, lr, .LBB95_95
.LBB95_94:
	add r27, r6, r0
	jal r31, fio_number
	add r6, r27, r0
.LBB95_95:
	ldbu r4, r22+%lo(fio_reading)
	mul r3, r1, fp
	addi r1, r1, -1
	bne r4, r11, .LBB95_103
.LBB95_96:
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
	jal r0, .LBB95_3
.LBB95_97:
	stw r13+%lo(fio_pos), r3
	jal r0, .LBB95_3
.LBB95_98:
	stw r13+%lo(fio_pos), r3
	sub r1, r0, fp
	xor r3, fp, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	lui r3, %hi(fio_scale)
	stw r3+%lo(fio_scale), r1
	jal r0, .LBB95_3
.LBB95_99:
	stw r13+%lo(fio_pos), r3
	ldbu r3, r22+%lo(fio_reading)
	addi r1, r0, 1
	bne r3, r1, .LBB95_104
.LBB95_100:
	lui r1, %hi(fio_rpos)
	ldw r3, r1+%lo(fio_rpos)
	add r3, r3, fp
	stw r1+%lo(fio_rpos), r3
	jal r0, .LBB95_3
.LBB95_101:
	stw r13+%lo(fio_pos), r3
	addi r1, r11, -65
	addi r4, r0, 43
	bgtu r1, r4, .LBB95_3
.LBB95_102:
	slli r1, r1, 2
	lui r4, %hi(.LJTI95_3)
	addi r4, r4, %lo(.LJTI95_3)
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB95_103:
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
	jal r0, .LBB95_3
.LBB95_104:
	blt fp, r1, .LBB95_3
.LBB95_105:
	lui r1, %hi(fio_col)
	ldw r3, r1+%lo(fio_col)
	add r3, r3, fp
	stw r1+%lo(fio_col), r3
	jal r0, .LBB95_3
.LBB95_106:
	add r6, r11, r0
	jal r0, .LBB95_93
.LBB95_107:
	add r1, r0, r0
.LBB95_108:
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
.LBB95_109:
	add r1, r27, r3
	ldb r1, r1+0
	addi r1, r1, -58
	bgeu r1, lr, .LBB95_111
.LBB95_110:
	add r1, r0, r0
	jal r0, .LBB95_112
.LBB95_111:
	add r14, lr, r0
	jal r31, fio_number
	add lr, r14, r0
.LBB95_112:
	lui r3, %hi(fio_w)
	stw r3+%lo(fio_w), r1
	lui r14, %hi(fio_d)
	stw r14+%lo(fio_d), r0
	ldw r1, r13+%lo(fio_pos)
	add r3, r27, r1
	ldbu r3, r3+0
	addi r4, r0, 46
	bne r3, r4, .LBB95_114
.LBB95_113:
	addi r1, r1, 1
	stw r13+%lo(fio_pos), r1
	add r15, lr, r0
	jal r31, fio_number
	add lr, r15, r0
	stw r14+%lo(fio_d), r1
.LBB95_114:
	lui r14, %hi(fio_e)
	stw r14+%lo(fio_e), r0
	ldw r1, r13+%lo(fio_pos)
	add r3, r27, r1
	ldbu r3, r3+0
	ori  r3, r3, 32
	addi r4, r0, 101
	bne r3, r4, .LBB95_118
.LBB95_115:
	addi r3, r1, 1
	stw r13+%lo(fio_pos), r3
	add r3, r27, r3
	ldb r3, r3+0
	addi r3, r3, -58
	bgeu r3, lr, .LBB95_117
.LBB95_116:
	stw r13+%lo(fio_pos), r1
	jal r0, .LBB95_118
.LBB95_117:
	jal r31, fio_number
	stw r14+%lo(fio_e), r1
.LBB95_118:
	lui r1, %hi(fio_rep)
	stw r1+%lo(fio_rep), fp
	add r3, r11, r0
	jal r31, toupper
	lui r3, %hi(fio_desc)
	stw r3+%lo(fio_desc), r1
	addi r3, r0, 1
	stb r12+%lo(fio_since_data), r3
	jal r0, .LBB95_108
.LBB95_119:
	jal r31, fio_flush_line
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.87)
	addi r4, r4, %lo(.L.str.87)
	jal r0, .LBB95_121
.LBB95_120:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
.LBB95_121:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end95:
	.size	fio_next_desc, .Lfunc_end95-fio_next_desc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI95_0,@object
.LJTI95_0:
	.word	.LBB95_31
	.word	.LBB95_59
	.word	.LBB95_54
	.word	.LBB95_59
	.word	.LBB95_59
	.word	.LBB95_59
	.word	.LBB95_51
	.word	.LBB95_59
	.word	.LBB95_57
	.size	.LJTI95_0, 36
	.type	.LJTI95_1,@object
.LJTI95_1:
	.word	.LBB95_71
	.word	.LBB95_101
	.word	.LBB95_101
	.word	.LBB95_101
	.word	.LBB95_101
	.word	.LBB95_101
	.word	.LBB95_101
	.word	.LBB95_101
	.word	.LBB95_98
	.word	.LBB95_101
	.word	.LBB95_101
	.word	.LBB95_101
	.word	.LBB95_89
	.word	.LBB95_101
	.word	.LBB95_101
	.word	.LBB95_101
	.word	.LBB95_99
	.size	.LJTI95_1, 68
	.type	.LJTI95_2,@object
.LJTI95_2:
	.word	.LBB95_92
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_91
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_92
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_93
	.word	.LBB95_91
	.size	.LJTI95_2, 156
	.type	.LJTI95_3,@object
.LJTI95_3:
	.word	.LBB95_109
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_109
	.word	.LBB95_109
	.word	.LBB95_109
	.word	.LBB95_109
	.word	.LBB95_3
	.word	.LBB95_109
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_109
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_109
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_109
	.word	.LBB95_109
	.word	.LBB95_109
	.word	.LBB95_109
	.word	.LBB95_3
	.word	.LBB95_109
	.word	.LBB95_3
	.word	.LBB95_3
	.word	.LBB95_109
	.size	.LJTI95_3, 176
	.type	.LJTI95_4,@object
.LJTI95_4:
	.word	.LBB95_19
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_27
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_19
	.word	.LBB95_50
	.word	.LBB95_50
	.word	.LBB95_27
	.size	.LJTI95_4, 144
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
	beq r3, r0, .LBB96_9
.LBB96_1:
	lui r11, %hi(fio_irec)
	ldw r1, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r1, r4, .LBB96_18
.LBB96_2:
	lui r14, %hi(fio_ilen)
	ldw r6, r14+%lo(fio_ilen)
	bgt r5, r6, .LBB96_19
.LBB96_3:
	mul r1, r6, r1
	add r3, r3, r1
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	jal r31, memcpy
	ldw r3, r13+%lo(fio_len)
	ldw r1, r14+%lo(fio_ilen)
	ble r1, r3, .LBB96_6
.LBB96_4:
	ldw r4, r12+%lo(fio_ibuf)
	ldw r5, r11+%lo(fio_irec)
	mul r5, r5, r1
	sub r1, r1, r3
	add r3, r3, r5
	add r3, r4, r3
	addi r4, r0, 32
.LBB96_5:
	stb r3+0, r4
	addi r1, r1, -1
	addi r3, r3, 1
	bne r1, r0, .LBB96_5
.LBB96_6:
	ldw r1, r11+%lo(fio_irec)
	addi r1, r1, 1
	stw r11+%lo(fio_irec), r1
.LBB96_7:
	stw r13+%lo(fio_len), r0
.LBB96_8:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB96_9:
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
	bgtu r3, r14, .LBB96_15
.LBB96_10:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB96_15
.LBB96_11:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r12, r1+0
	bgt r5, r12, .LBB96_21
.LBB96_12:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	ldw r1, r13+%lo(fio_len)
	ble r12, r1, .LBB96_7
.LBB96_13:
	sub r14, r12, r1
	addi r12, r0, 32
.LBB96_14:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fputc
	addi r14, r14, -1
	bne r14, r0, .LBB96_14
	jal r0, .LBB96_7
.LBB96_15:
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
	bgtu r3, r14, .LBB96_8
.LBB96_16:
	addi r1, r1, -7
	addi r3, r0, -3
	bgtu r1, r3, .LBB96_8
.LBB96_17:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
	jal r0, .LBB96_8
.LBB96_18:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.88)
	addi r4, r4, %lo(.L.str.88)
	jal r0, .LBB96_20
.LBB96_19:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.89)
	addi r4, r4, %lo(.L.str.89)
.LBB96_20:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB96_21:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.90)
	addi r4, r4, %lo(.L.str.90)
	add r6, r12, r0
	jal r0, .LBB96_20
.Lfunc_end96:
	.size	fio_flush_line, .Lfunc_end96-fio_flush_line
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
	bne r3, r0, .LBB97_4
.LBB97_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB97_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB97_2
.LBB97_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB97_4:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_resolve
	add r13, r1, r0
	lui r1, %hi(stdout)
	ldw r1, r1+%lo(stdout)
	beq r13, r1, .LBB97_8
.LBB97_5:
	lui r1, %hi(stderr)
	ldw r1, r1+%lo(stderr)
	beq r13, r1, .LBB97_8
.LBB97_6:
	add r3, r13, r0
	jal r31, fflush
	addi r1, r11, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB97_8
.LBB97_7:
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
.LBB97_8:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end97:
	.size	f77_endfile, .Lfunc_end97-f77_endfile
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
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	add r5, r11, r0
	add r6, r12, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fprintf
.Lfunc_end98:
	.size	f77_pause, .Lfunc_end98-f77_pause
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
	add r14, r3, r0
	lui r15, %hi(fio_busy)
	ldbu r3, r15+%lo(fio_busy)
	addi r1, r0, 1
	beq r3, r1, .LBB99_19
.LBB99_1:
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	stb r15+%lo(fio_busy), r1
	lui r3, %hi(fio_uend_init)
	ldbu r4, r3+%lo(fio_uend_init)
	bne r4, r0, .LBB99_5
.LBB99_2:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
	addi r6, r0, -1
.LBB99_3:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r0, .LBB99_3
.LBB99_4:
	stb r3+%lo(fio_uend_init), r1
.LBB99_5:
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
	bgtu r3, r4, .LBB99_11
.LBB99_6:
	slli r16, r14, 2
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r16, r3
	stw r3+0, r0
	lui r4, %hi(fio_rec_next)
	ldw r3, r4+%lo(fio_rec_next)
	stw r4+%lo(fio_rec_next), r0
	addi r4, r14, -100
	addi r5, r0, -99
	bltu r4, r5, .LBB99_10
.LBB99_7:
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r16, r4
	ldw r4, r4+0
	beq r4, r0, .LBB99_10
.LBB99_8:
	ble r3, r0, .LBB99_20
.LBB99_9:
	addi r3, r3, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r4, r16, r4
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB99_10:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r16, r1
	ldw r1, r1+0
	jal r0, .LBB99_16
.LBB99_11:
	lui r4, %hi(fio_rec_next)
	ldw r3, r4+%lo(fio_rec_next)
	stw r4+%lo(fio_rec_next), r0
	addi r4, r14, -100
	addi r5, r0, -99
	bltu r4, r5, .LBB99_15
.LBB99_12:
	slli r4, r14, 2
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r4, r5
	ldw r5, r5+0
	beq r5, r0, .LBB99_15
.LBB99_13:
	ble r3, r0, .LBB99_20
.LBB99_14:
	addi r3, r3, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r4, r4, r5
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB99_15:
	add r1, r0, r0
.LBB99_16:
	lui r3, %hi(fio_bz)
	stw r3+%lo(fio_bz), r1
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, fio_rd_setup
	beq r1, r0, .LBB99_18
.LBB99_17:
	stb r15+%lo(fio_busy), r0
.LBB99_18:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB99_19:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	jal r0, .LBB99_21
.LBB99_20:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.64)
	addi r4, r4, %lo(.L.str.64)
.LBB99_21:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end99:
	.size	f77_rd_begin, .Lfunc_end99-f77_rd_begin
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
	bgt r1, r0, .LBB100_2
.LBB100_1:
	lui r1, %hi(fio_bz)
	stw r1+%lo(fio_bz), r0
.LBB100_2:
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
	lui r1, %hi(.L.str.7)
	addi r1, r1, %lo(.L.str.7)
	lui r16, %hi(fio_fmt)
	stw r16+%lo(fio_fmt), r1
	lui r14, %hi(fio_pos)
	stw r14+%lo(fio_pos), r0
	lui r15, %hi(fio_revert)
	addi r18, r0, -1
	stw r15+%lo(fio_revert), r18
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	beq r3, r0, .LBB100_4
.LBB100_3:
	jal r31, fio_fmt_use
	ble r1, r18, .LBB100_14
.LBB100_4:
	ldw r1, r17+%lo(fio_listed)
	bne r1, r0, .LBB100_10
.LBB100_5:
	ldw r3, r16+%lo(fio_fmt)
	ldw r1, r14+%lo(fio_pos)
	addi r4, r0, 32
.LBB100_6:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB100_8
.LBB100_7:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB100_6
.LBB100_8:
	addi r3, r0, 40
	bne r5, r3, .LBB100_10
.LBB100_9:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	stw r15+%lo(fio_revert), r1
.LBB100_10:
	jal r31, fio_getrec
	beq r1, r0, .LBB100_13
.LBB100_11:
	stb r13+%lo(fio_reading), r0
	andi r1, r11, 3
	bne r1, r0, .LBB100_16
.LBB100_12:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB100_13:
	add r12, r0, r0
	jal r0, .LBB100_16
.LBB100_14:
	stb r13+%lo(fio_reading), r0
	andi r1, r11, 2
	beq r1, r0, .LBB100_17
.LBB100_15:
	addi r12, r0, 2
.LBB100_16:
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
.LBB100_17:
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end100:
	.size	fio_rd_setup, .Lfunc_end100-fio_rd_setup
                                        # -- End function
	.globl	f77_rd_begin_int                # -- Begin function f77_rd_begin_int
	.p2align	2
	.type	f77_rd_begin_int,@function
f77_rd_begin_int:                       # @f77_rd_begin_int
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	lui r11, %hi(fio_busy)
	ldbu r9, r11+%lo(fio_busy)
	addi r1, r0, 1
	beq r9, r1, .LBB101_8
.LBB101_1:
	stb r11+%lo(fio_busy), r1
	lui r9, %hi(fio_uend_init)
	ldbu r10, r9+%lo(fio_uend_init)
	bne r10, r0, .LBB101_5
.LBB101_2:
	addi r10, r0, -400
	lui lr, %hi(fio_uend+400)
	addi lr, lr, %lo(fio_uend+400)
	addi r12, r0, -1
.LBB101_3:
	add r13, r10, lr
	stw r13+0, r12
	addi r10, r10, 4
	bne r10, r0, .LBB101_3
.LBB101_4:
	stb r9+%lo(fio_uend_init), r1
.LBB101_5:
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
	jal r31, fio_rd_setup
	beq r1, r0, .LBB101_7
.LBB101_6:
	stb r11+%lo(fio_busy), r0
.LBB101_7:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB101_8:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end101:
	.size	f77_rd_begin_int, .Lfunc_end101-f77_rd_begin_int
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
	beq r1, r0, .LBB102_5
.LBB102_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB102_60
.LBB102_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB102_60
.LBB102_3:
	addi r3, r0, 1
	bne r1, r3, .LBB102_27
.LBB102_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB102_60
.LBB102_5:
	lui r12, %hi(fio_desc)
	ldw r1, r12+%lo(fio_desc)
	lui r13, %hi(fio_rep)
	beq r1, r0, .LBB102_11
.LBB102_6:
	ldw r3, r13+%lo(fio_rep)
	ble r3, r0, .LBB102_11
.LBB102_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB102_12
.LBB102_8:
	beq r1, r0, .LBB102_60
.LBB102_9:
	addi r3, r0, 65
	bne r1, r3, .LBB102_30
.LBB102_10:
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
.LBB102_11:
	stw r12+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB102_8
.LBB102_12:
	addi r3, r0, 73
	beq r1, r3, .LBB102_31
.LBB102_13:
	addi r3, r0, 76
	bne r1, r3, .LBB102_30
.LBB102_14:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	addi r1, r0, 1
	lui r4, %hi(fio_rpos)
	blt r3, r1, .LBB102_44
.LBB102_15:
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
	jal r0, .LBB102_19
.LBB102_16:
	addi r19, r17, 1
	addi r21, sp, 20
	add r17, r21, r17
	stb r17+0, r20
	add r17, r19, r0
.LBB102_17:
	add r19, r14, r0
.LBB102_18:
	addi r18, r18, -1
	addi r5, r5, 1
	beq r18, r0, .LBB102_45
.LBB102_19:
	add r20, r8, r0
	bge r5, r6, .LBB102_21
.LBB102_20:
	add r20, r5, r9
	ldb r20, r20+0
.LBB102_21:
	beq r20, r8, .LBB102_23
.LBB102_22:
	bne r20, r15, .LBB102_26
.LBB102_23:
	beq r7, r0, .LBB102_18
.LBB102_24:
	beq r19, r0, .LBB102_18
.LBB102_25:
	add r20, r10, r0
	blt r17, lr, .LBB102_16
	jal r0, .LBB102_18
.LBB102_26:
	bgt r17, r16, .LBB102_17
	jal r0, .LBB102_16
.LBB102_27:
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
	bgtu r4, r5, .LBB102_61
.LBB102_28:
	slli r4, r4, 2
	lui r5, %hi(.LJTI102_0)
	addi r5, r5, %lo(.LJTI102_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB102_29:
	andi r3, r3, 255
	addi r4, r0, 46
	seq r3, r3, r4
	add r1, r3, r1
	ldbu r1, r1+0
	andi r1, r1, 223
	addi r3, r0, 84
	seq r1, r1, r3
	stw r11+0, r1
	jal r0, .LBB102_60
.LBB102_30:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.w.d r1, r4
	jal r0, .LBB102_56
.LBB102_31:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r3, r0, 1
	blt r1, r3, .LBB102_52
.LBB102_32:
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
	jal r0, .LBB102_36
.LBB102_33:
	addi r17, r15, 1
	addi r19, sp, 20
	add r15, r19, r15
	stb r15+0, r18
	add r15, r17, r0
.LBB102_34:
	add r17, r3, r0
.LBB102_35:
	addi r16, r16, -1
	addi r4, r4, 1
	beq r16, r0, .LBB102_53
.LBB102_36:
	add r18, r7, r0
	bge r4, r5, .LBB102_38
.LBB102_37:
	add r18, r4, r8
	ldb r18, r18+0
.LBB102_38:
	beq r18, r7, .LBB102_40
.LBB102_39:
	bne r18, lr, .LBB102_43
.LBB102_40:
	beq r6, r0, .LBB102_35
.LBB102_41:
	beq r17, r0, .LBB102_35
.LBB102_42:
	add r18, r9, r0
	blt r15, r10, .LBB102_33
	jal r0, .LBB102_35
.LBB102_43:
	bgt r15, r14, .LBB102_34
	jal r0, .LBB102_33
.LBB102_44:
	add r17, r0, r0
.LBB102_45:
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
	ble r3, r4, .LBB102_49
.LBB102_46:
	addi r4, r0, 84
	beq r3, r4, .LBB102_56
.LBB102_47:
	addi r4, r0, 116
	beq r3, r4, .LBB102_56
.LBB102_48:
	addi r1, r0, 102
	beq r3, r1, .LBB102_55
	jal r0, .LBB102_51
.LBB102_49:
	beq r3, r0, .LBB102_55
.LBB102_50:
	addi r1, r0, 70
	beq r3, r1, .LBB102_55
.LBB102_51:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB102_52:
	add r15, r0, r0
.LBB102_53:
	addi r3, sp, 20
	add r3, r3, r15
	stb r3+0, r0
	lui r3, %hi(fio_rpos)
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldbu r1, sp+20
	beq r1, r0, .LBB102_55
.LBB102_54:
	addi r3, sp, 20
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	jal r0, .LBB102_56
.LBB102_55:
	add r1, r0, r0
.LBB102_56:
	stw r11+0, r1
	ldw r1, r13+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB102_58
.LBB102_57:
	addi r1, r1, -1
	stw r13+%lo(fio_rep), r1
.LBB102_58:
	ldw r1, r13+%lo(fio_rep)
	bne r1, r0, .LBB102_60
.LBB102_59:
	stw r12+%lo(fio_desc), r0
.LBB102_60:
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
.LBB102_61:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	stw r11+0, r1
	jal r0, .LBB102_60
.Lfunc_end102:
	.size	f77_rd_i, .Lfunc_end102-f77_rd_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI102_0,@object
.LJTI102_0:
	.word	.LBB102_29
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_29
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_29
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_29
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_61
	.word	.LBB102_29
	.size	.LJTI102_0, 144
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
	blt r1, r20, .LBB103_2
.LBB103_1:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
	addi r11, r0, 2
	jal r0, .LBB103_5
.LBB103_2:
	lui r14, %hi(fio_l_pending)
	ldw r1, r14+%lo(fio_l_pending)
	ble r1, r0, .LBB103_6
.LBB103_3:
	addi r1, r1, -1
	stw r14+%lo(fio_l_pending), r1
.LBB103_4:
	add r11, r0, r0
.LBB103_5:
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
.LBB103_6:
	lui r15, %hi(fio_rpos)
	lui r17, %hi(fio_rlen)
	lui r16, %hi(fio_rec)
	addi r16, r16, %lo(fio_rec)
	addi r18, r0, 38
	lui r21, %hi(.LJTI103_0)
	addi r21, r21, %lo(.LJTI103_0)
	lui r12, %hi(fio_l_after)
	addi r11, r0, 2
	jal r0, .LBB103_8
.LBB103_7:
	jal r31, fio_getrec
	bne r1, r0, .LBB103_44
.LBB103_8:
	ldw r3, r15+%lo(fio_rpos)
	ldw r1, r17+%lo(fio_rlen)
	bge r3, r1, .LBB103_7
.LBB103_9:
	add r4, r3, r16
	ldbu r19, r4+0
	addi r4, r19, -9
	bgtu r4, r18, .LBB103_25
.LBB103_10:
	slli r4, r4, 2
	add r4, r21, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB103_11:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	jal r0, .LBB103_8
.LBB103_12:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	ldbu r1, r12+%lo(fio_l_after)
	bne r1, r20, .LBB103_5
.LBB103_13:
	stb r12+%lo(fio_l_after), r0
	jal r0, .LBB103_8
.LBB103_14:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	addi r13, r0, 126
	lui r11, %hi(fio_ltok)
	addi r11, r11, %lo(fio_ltok)
	add r14, r0, r0
	jal r0, .LBB103_16
.LBB103_15:
	jal r31, fio_getrec
	bne r1, r0, .LBB103_44
.LBB103_16:
	ldw r1, r15+%lo(fio_rpos)
	ldw r4, r17+%lo(fio_rlen)
	bge r1, r4, .LBB103_15
.LBB103_17:
	addi r5, r1, 1
	stw r15+%lo(fio_rpos), r5
	add r3, r1, r16
	ldbu r3, r3+0
	bne r3, r19, .LBB103_21
.LBB103_18:
	bge r5, r4, .LBB103_23
.LBB103_19:
	add r4, r5, r16
	ldbu r4, r4+0
	bne r4, r19, .LBB103_23
.LBB103_20:
	addi r1, r1, 2
	stw r15+%lo(fio_rpos), r1
.LBB103_21:
	bgtu r14, r13, .LBB103_16
.LBB103_22:
	addi r1, r14, 1
	add r4, r14, r11
	stb r4+0, r3
	add r14, r1, r0
	jal r0, .LBB103_16
.LBB103_23:
	add r1, r14, r11
	stb r1+0, r0
	addi r1, r0, 1
	stb r12+%lo(fio_l_after), r1
	jal r0, .LBB103_4
.LBB103_24:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	add r11, r20, r0
	jal r0, .LBB103_5
.LBB103_25:
	ldw r3, r15+%lo(fio_rpos)
	bge r3, r1, .LBB103_32
.LBB103_26:
	addi r5, r0, 126
	lui r6, %hi(fio_ltok)
	addi r6, r6, %lo(fio_ltok)
	lui r7, %hi(.LJTI103_1)
	addi r7, r7, %lo(.LJTI103_1)
	add r4, r0, r0
	jal r0, .LBB103_28
.LBB103_27:
	addi r3, r3, 1
	stw r15+%lo(fio_rpos), r3
	beq r1, r3, .LBB103_33
.LBB103_28:
	add r8, r3, r16
	ldbu r8, r8+0
	addi r9, r8, -9
	bgtu r9, r18, .LBB103_30
.LBB103_29:
	slli r9, r9, 2
	add r9, r7, r9
	ldw r9, r9+0
	jalr r0, r9, 0
.LBB103_30:
	bgt r4, r5, .LBB103_27
.LBB103_31:
	addi r9, r4, 1
	add r4, r4, r6
	stb r4+0, r8
	add r4, r9, r0
	jal r0, .LBB103_27
.LBB103_32:
	add r4, r0, r0
.LBB103_33:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	add r3, r4, r1
	stb r3+0, r0
	addi r3, r0, 1
	stb r12+%lo(fio_l_after), r3
	addi r4, r0, -11
	add r15, r0, r0
.LBB103_34:
	add r5, r15, r1
	ldb r5, r5+0
	addi r6, r5, -58
	addi r15, r15, 1
	bgtu r6, r4, .LBB103_34
.LBB103_35:
	beq r15, r3, .LBB103_4
.LBB103_36:
	addi r1, r0, 42
	bne r5, r1, .LBB103_4
.LBB103_37:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	addi r5, r0, 10
	add r3, r12, r0
	add r4, r0, r0
	jal r31, strtol
	beq r1, r0, .LBB103_46
.LBB103_38:
	add r12, r15, r12
	ldbu r15, r12+0
	beq r15, r0, .LBB103_42
.LBB103_39:
	addi r3, r0, 2
	blt r1, r3, .LBB103_41
.LBB103_40:
	addi r1, r1, -1
	stw r14+%lo(fio_l_pending), r1
.LBB103_41:
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	add r4, r12, r0
	jal r31, memmove
	jal r0, .LBB103_43
.LBB103_42:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
.LBB103_43:
	bne r15, r0, .LBB103_4
	jal r0, .LBB103_5
.LBB103_44:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
.LBB103_45:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB103_46:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.91)
	addi r4, r4, %lo(.L.str.91)
	jal r0, .LBB103_45
.Lfunc_end103:
	.size	fio_list_tok, .Lfunc_end103-fio_list_tok
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI103_0,@object
.LJTI103_0:
	.word	.LBB103_11
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_11
	.word	.LBB103_25
	.word	.LBB103_14
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_14
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_12
	.word	.LBB103_25
	.word	.LBB103_25
	.word	.LBB103_24
	.size	.LJTI103_0, 156
	.type	.LJTI103_1,@object
.LJTI103_1:
	.word	.LBB103_33
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_33
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_33
	.word	.LBB103_30
	.word	.LBB103_30
	.word	.LBB103_33
	.size	.LJTI103_1, 156
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
	beq r1, r0, .LBB104_5
.LBB104_1:
	lui r13, %hi(fio_ldone)
	ldbu r1, r13+%lo(fio_ldone)
	bne r1, r0, .LBB104_31
.LBB104_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB104_31
.LBB104_3:
	addi r14, r0, 1
	bne r1, r14, .LBB104_17
.LBB104_4:
	stb r13+%lo(fio_ldone), r14
	jal r0, .LBB104_31
.LBB104_5:
	lui r13, %hi(fio_desc)
	ldw r1, r13+%lo(fio_desc)
	lui r14, %hi(fio_rep)
	beq r1, r0, .LBB104_8
.LBB104_6:
	ldw r3, r14+%lo(fio_rep)
	ble r3, r0, .LBB104_8
.LBB104_7:
	bne r1, r0, .LBB104_9
	jal r0, .LBB104_31
.LBB104_8:
	stw r13+%lo(fio_desc), r0
	jal r31, fio_next_desc
	beq r1, r0, .LBB104_31
.LBB104_9:
	addi r3, r0, 65
	bne r1, r3, .LBB104_32
.LBB104_10:
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
	bge r1, r12, .LBB104_22
.LBB104_11:
	blt r12, r4, .LBB104_27
.LBB104_12:
	ldw r4, r3+%lo(fio_rpos)
	lui r5, %hi(fio_rlen)
	ldw r5, r5+%lo(fio_rlen)
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	add r8, r0, r0
	jal r0, .LBB104_14
.LBB104_13:
	add r10, r11, r8
	stb r10+0, r9
	addi r8, r8, 1
	beq r12, r8, .LBB104_27
.LBB104_14:
	add r9, r6, r0
	bge r8, r1, .LBB104_13
.LBB104_15:
	add r10, r4, r8
	add r9, r6, r0
	bge r10, r5, .LBB104_13
.LBB104_16:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB104_13
.LBB104_17:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB104_31
.LBB104_18:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB104_20
.LBB104_19:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB104_31
.LBB104_20:
	add r5, r3, r0
	bge r4, r1, .LBB104_19
.LBB104_21:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB104_19
.LBB104_22:
	blt r12, r4, .LBB104_27
.LBB104_23:
	ldw r5, r3+%lo(fio_rpos)
	lui r4, %hi(fio_rlen)
	ldw r4, r4+%lo(fio_rlen)
	sub r8, r0, r12
	add r5, r1, r5
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	jal r0, .LBB104_25
.LBB104_24:
	stb r11+0, r9
	addi r9, r8, 1
	sltu r10, r9, r8
	addi r11, r11, 1
	add r8, r9, r0
	bne r10, r0, .LBB104_27
.LBB104_25:
	add r10, r5, r8
	add r9, r6, r0
	bge r10, r4, .LBB104_24
.LBB104_26:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB104_24
.LBB104_27:
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldw r1, r14+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB104_29
.LBB104_28:
	addi r1, r1, -1
	stw r14+%lo(fio_rep), r1
.LBB104_29:
	ldw r1, r14+%lo(fio_rep)
	bne r1, r0, .LBB104_31
.LBB104_30:
	stw r13+%lo(fio_desc), r0
.LBB104_31:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB104_32:
	lui r3, %hi(stderr)
	ldw r3, r3+%lo(stderr)
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end104:
	.size	f77_rd_s, .Lfunc_end104-f77_rd_s
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_in_real
	.type	.LCPI105_0,@object
.LCPI105_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI105_0, 8
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
	blt r3, r5, .LBB105_13
.LBB105_1:
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
	jal r0, .LBB105_5
.LBB105_2:
	addi r18, r16, 1
	stw sp+16, r1
	add r1, sp, r0
	addi r1, r1, 2047
	addi r20, r1, 41
	ldw r1, sp+16
	add r16, r20, r16
	stb r16+0, r19
	add r16, r18, r0
.LBB105_3:
	add r18, r13, r0
.LBB105_4:
	addi r17, r17, -1
	addi r7, r7, 1
	beq r17, r0, .LBB105_14
.LBB105_5:
	add r19, r10, r0
	bge r7, r8, .LBB105_7
.LBB105_6:
	add r19, r7, lr
	ldb r19, r19+0
.LBB105_7:
	beq r19, r10, .LBB105_9
.LBB105_8:
	bne r19, r14, .LBB105_12
.LBB105_9:
	beq r9, r0, .LBB105_4
.LBB105_10:
	beq r18, r0, .LBB105_4
.LBB105_11:
	add r19, r11, r0
	blt r16, r12, .LBB105_2
	jal r0, .LBB105_4
.LBB105_12:
	bgt r16, r15, .LBB105_3
	jal r0, .LBB105_2
.LBB105_13:
	add r16, r0, r0
.LBB105_14:
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
	lui r8, %hi(.LCPI105_0)
	addi r8, r8, %lo(.LCPI105_0)
	ldw r7, r8+4
	ldw r6, r8+0
	beq r3, r0, .LBB105_67
.LBB105_15:
	addi lr, r0, 43
	beq r3, lr, .LBB105_18
.LBB105_16:
	addi r8, r0, 45
	bne r3, r8, .LBB105_19
.LBB105_17:
	addi r10, r0, 1
	add r8, r0, r0
	jal r0, .LBB105_21
.LBB105_18:
	add r10, r5, r0
	jal r0, .LBB105_20
.LBB105_19:
	add r10, r0, r0
.LBB105_20:
	add r8, r5, r0
.LBB105_21:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r3, r9, 41
	or  r3, r3, r10
	ldbu r19, r3+0
	beq r19, r0, .LBB105_42
.LBB105_22:
	addi r13, r0, 1
	addi r14, r0, 46
	addi r11, r0, 0
	addi r15, r0, -10
	addi r16, r0, 70
	lui r17, %hi(.LJTI105_0)
	addi r17, r17, %lo(.LJTI105_0)
	add r9, r13, r0
	add r12, r0, r0
	add r18, r0, r0
	add r3, r0, r0
.LBB105_23:
	andi r5, r19, 255
	bne r5, r14, .LBB105_25
.LBB105_24:
	add r18, r13, r0
	jal r0, .LBB105_34
.LBB105_25:
	slli r20, r19, 24
	srai r20, r20, 24
	addi r20, r20, -58
	bgeu r20, r15, .LBB105_31
.LBB105_26:
	addi r5, r5, -43
	bgtu r5, r16, .LBB105_40
.LBB105_27:
	slli r5, r5, 2
	add r5, r17, r5
	ldw r20, r5+0
	add r5, r0, r0
	add r19, r13, r0
	jalr r0, r20, 0
.LBB105_28:
	addi r5, r10, 1
	add r20, sp, r0
	addi r20, r20, 2047
	addi r19, r20, 41
	add r19, r19, r5
	ldbu r19, r19+0
	beq r19, lr, .LBB105_35
.LBB105_29:
	addi r20, r0, 45
	bne r19, r20, .LBB105_37
.LBB105_30:
	addi r9, r0, -1
	add r10, r5, r0
	jal r0, .LBB105_37
.LBB105_31:
	addi r5, r0, 1022
	bgt r3, r5, .LBB105_33
.LBB105_32:
	addi r5, r3, 1
	addi r20, sp, 1064
	add r3, r20, r3
	stb r3+0, r19
	add r3, r5, r0
.LBB105_33:
	sne r5, r18, r11
	add r12, r12, r5
.LBB105_34:
	add r5, r13, r0
	add r19, r13, r0
	bne r19, r0, .LBB105_39
	jal r0, .LBB105_41
.LBB105_35:
	add r10, r5, r0
	jal r0, .LBB105_37
.LBB105_36:
	addi r9, r0, -1
.LBB105_37:
	add r5, r0, r0
	add r19, r13, r0
.LBB105_38:
	beq r19, r0, .LBB105_41
.LBB105_39:
	addi r10, r10, 1
	add r20, sp, r0
	addi r20, r20, 2047
	addi r19, r20, 41
	add r19, r19, r10
	ldbu r19, r19+0
	sne r20, r19, r11
	and r20, r20, r5
	bne r20, r0, .LBB105_23
	jal r0, .LBB105_41
.LBB105_40:
	add r5, r13, r0
	add r19, r0, r0
	bne r19, r0, .LBB105_39
.LBB105_41:
	seq lr, r18, r11
	xor r4, r4, r12
	sub lr, r0, lr
	and r4, r4, lr
	xor r4, r12, r4
	beq r5, r0, .LBB105_43
	jal r0, .LBB105_47
.LBB105_42:
	add r3, r0, r0
	add r9, r5, r0
	bne r5, r0, .LBB105_47
.LBB105_43:
	add r11, sp, r0
	addi r11, r11, 2047
	addi lr, r11, 41
	add lr, lr, r10
	ldbu r12, lr+0
	beq r12, r0, .LBB105_47
.LBB105_44:
	add r11, sp, r0
	addi r11, r11, 2047
	addi lr, r11, 41
	add r10, r10, lr
	addi r10, r10, 1
	addi lr, r0, 9
	addi r11, r0, 10
	add r13, r0, r0
.LBB105_45:
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
	bne r12, r0, .LBB105_45
.LBB105_46:
	mul r10, r13, r9
	jal r0, .LBB105_48
.LBB105_47:
	add r10, r0, r0
.LBB105_48:
	addi r9, sp, 1064
	add r9, r9, r3
	stb r9+0, r0
	beq r3, r0, .LBB105_67
.LBB105_49:
	sub r9, r3, r4
	lui r6, %hi(fio_scale)
	ldw r6, r6+%lo(fio_scale)
	sub r5, r0, r5
	and r6, r6, r5
	addi lr, sp, 24
	add r5, r0, r0
	bne r8, r0, .LBB105_51
.LBB105_50:
	addi lr, r1, 1
	addi r1, r0, 45
	stb sp+24, r1
	addi r5, r0, 1
.LBB105_51:
	sub r6, r10, r6
	addi r7, r0, 1
	blt r9, r7, .LBB105_56
.LBB105_52:
	blt r3, r7, .LBB105_62
.LBB105_53:
	xor r1, r9, r3
	slt r4, r9, r3
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r3, r1
	addi r4, sp, 24
	or  r7, r4, r5
	add r4, r0, r0
.LBB105_54:
	addi r8, sp, 1064
	add r8, r8, r4
	ldbu r8, r8+0
	add r9, r7, r4
	stb r9+0, r8
	addi r4, r4, 1
	bne r1, r4, .LBB105_54
.LBB105_55:
	add r5, r5, r4
	jal r0, .LBB105_63
.LBB105_56:
	addi r1, sp, 24
	or  r1, r1, r5
	addi r8, r0, 48
	stb r1+0, r8
	addi r1, r5, 2
	addi r5, r0, 46
	stb lr+1, r5
	addi r5, r0, -1
	bgt r9, r5, .LBB105_59
.LBB105_57:
	sub r4, r4, r3
.LBB105_58:
	add r5, r1, r0
	addi r1, r1, 1
	addi r9, sp, 24
	add r5, r9, r5
	stb r5+0, r8
	addi r4, r4, -1
	bne r4, r0, .LBB105_58
.LBB105_59:
	blt r3, r7, .LBB105_66
.LBB105_60:
	addi r4, sp, 1064
.LBB105_61:
	add r5, r1, r0
	ldbu r7, r4+0
	addi r1, r1, 1
	addi r8, sp, 24
	add r5, r8, r5
	stb r5+0, r7
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r0, .LBB105_61
	jal r0, .LBB105_66
.LBB105_62:
	add r4, r0, r0
.LBB105_63:
	addi r1, sp, 24
	add r1, r1, r5
	addi r7, r0, 46
	stb r1+0, r7
	addi r1, r5, 1
	ble r3, r4, .LBB105_66
.LBB105_64:
	sub r3, r3, r4
	addi r5, sp, 1064
	add r4, r5, r4
.LBB105_65:
	ldbu r5, r4+0
	addi r7, sp, 24
	add r7, r7, r1
	stb r7+0, r5
	addi r1, r1, 1
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r0, .LBB105_65
.LBB105_66:
	addi r11, sp, 24
	add r3, r11, r1
	addi r4, r0, 1040
	sub r4, r4, r1
	lui r5, %hi(.L.str.93)
	addi r5, r5, %lo(.L.str.93)
	jal r31, snprintf
	add r3, r11, r0
	add r4, r0, r0
	jal r31, strtod
	add r6, r1, r0
	add r7, r2, r0
.LBB105_67:
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
.Lfunc_end105:
	.size	fio_in_real, .Lfunc_end105-fio_in_real
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI105_0,@object
.LJTI105_0:
	.word	.LBB105_38
	.word	.LBB105_40
	.word	.LBB105_36
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_28
	.word	.LBB105_28
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_28
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_28
	.word	.LBB105_28
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_40
	.word	.LBB105_28
	.size	.LJTI105_0, 284
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
	bne r1, r0, .LBB106_6
.LBB106_1:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB106_5
.LBB106_2:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB106_5
.LBB106_3:
	addi r3, r0, 65
	bne r1, r3, .LBB106_6
.LBB106_4:
	add r3, r11, r0
	add r4, r12, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_rd_s
.LBB106_5:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 65
	beq r1, r3, .LBB106_4
.LBB106_6:
	addi r13, r0, 2
	bne r12, r13, .LBB106_8
.LBB106_7:
	ldh r1, r11+0
	jal r0, .LBB106_9
.LBB106_8:
	ldbu r1, r11+0
.LBB106_9:
	stw sp+16, r1
	addi r3, sp, 16
	jal r31, f77_rd_i
	ldw r1, sp+16
	bne r12, r13, .LBB106_11
.LBB106_10:
	sth r11+0, r1
	jal r0, .LBB106_12
.LBB106_11:
	stb r11+0, r1
.LBB106_12:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end106:
	.size	f77_rd_in, .Lfunc_end106-f77_rd_in
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
	bne r1, r0, .LBB107_19
.LBB107_1:
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB107_18
.LBB107_2:
	ldw r5, r12+%lo(fio_rep)
	ble r5, r0, .LBB107_18
.LBB107_3:
	addi r5, r0, 65
	bne r1, r5, .LBB107_19
.LBB107_4:
	sth sp+28, r3
	lui r1, %hi(fio_w)
	ldw r5, r1+%lo(fio_w)
	addi r6, r0, 2
	bne r4, r6, .LBB107_20
.LBB107_5:
	addi r1, r0, 1
	beq r5, r1, .LBB107_9
.LBB107_6:
	addi r1, r0, 3
	blt r5, r1, .LBB107_8
.LBB107_7:
	lui r1, %hi(fio_col)
	ldw r3, r1+%lo(fio_col)
	add r3, r5, r3
	addi r3, r3, -2
	stw r1+%lo(fio_col), r3
.LBB107_8:
	add r1, r6, r0
.LBB107_9:
	lui r3, %hi(fio_len)
	ldw r7, r3+%lo(fio_len)
	lui r4, %hi(fio_col)
	ldw r10, r4+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r8, r0, 32
	add r9, r0, r0
	jal r0, .LBB107_12
.LBB107_10:
	add r10, lr, r0
.LBB107_11:
	addi r9, r9, 1
	beq r9, r1, .LBB107_29
.LBB107_12:
	addi lr, sp, 28
	bgt r10, r5, .LBB107_11
.LBB107_13:
	bge r7, r10, .LBB107_16
.LBB107_14:
	addi r13, r7, 1
	add r7, r7, r6
	stb r7+0, r8
	add r7, r13, r0
	bne r10, r13, .LBB107_14
.LBB107_15:
	stw r3+%lo(fio_len), r10
	add r7, r10, r0
.LBB107_16:
	add lr, lr, r9
	ldbu lr, lr+0
	add r13, r10, r6
	stb r13+0, lr
	addi lr, r10, 1
	stw r4+%lo(fio_col), lr
	blt r10, r7, .LBB107_10
.LBB107_17:
	stw r3+%lo(fio_len), lr
	add r7, lr, r0
	jal r0, .LBB107_10
.LBB107_18:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	add r14, r4, r0
	jal r31, fio_next_desc
	add r4, r14, r0
	add r3, r13, r0
	addi r5, r0, 65
	beq r1, r5, .LBB107_4
.LBB107_19:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_wr_i
.LBB107_20:
	lui r1, %hi(fio_col)
	blt r5, r6, .LBB107_22
.LBB107_21:
	ldw r4, r1+%lo(fio_col)
	add r4, r5, r4
	addi r4, r4, -1
	stw r1+%lo(fio_col), r4
.LBB107_22:
	ldw r4, r1+%lo(fio_col)
	addi r5, r0, 1022
	bgt r4, r5, .LBB107_29
.LBB107_23:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	bge r6, r4, .LBB107_27
.LBB107_24:
	addi r8, r0, 32
.LBB107_25:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r4, r9, .LBB107_25
.LBB107_26:
	stw r5+%lo(fio_len), r4
	add r6, r4, r0
.LBB107_27:
	add r7, r4, r7
	stb r7+0, r3
	addi r3, r4, 1
	stw r1+%lo(fio_col), r3
	blt r4, r6, .LBB107_29
.LBB107_28:
	stw r5+%lo(fio_len), r3
.LBB107_29:
	ldw r1, r12+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB107_31
.LBB107_30:
	addi r1, r1, -1
	stw r12+%lo(fio_rep), r1
.LBB107_31:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB107_33
.LBB107_32:
	stw r11+%lo(fio_desc), r0
.LBB107_33:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end107:
	.size	f77_wr_in, .Lfunc_end107-f77_wr_in
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_rd_d
	.type	.LCPI108_0,@object
.LCPI108_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI108_0, 8
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
	beq r1, r0, .LBB108_5
.LBB108_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB108_41
.LBB108_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB108_41
.LBB108_3:
	addi r3, r0, 1
	bne r1, r3, .LBB108_26
.LBB108_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB108_41
.LBB108_5:
	lui r12, %hi(fio_desc)
	ldw r1, r12+%lo(fio_desc)
	lui r13, %hi(fio_rep)
	beq r1, r0, .LBB108_11
.LBB108_6:
	ldw r3, r13+%lo(fio_rep)
	ble r3, r0, .LBB108_11
.LBB108_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB108_12
.LBB108_8:
	beq r1, r0, .LBB108_41
.LBB108_9:
	addi r3, r0, 65
	bne r1, r3, .LBB108_30
.LBB108_10:
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
.LBB108_11:
	stw r12+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB108_8
.LBB108_12:
	addi r3, r0, 73
	bne r1, r3, .LBB108_29
.LBB108_13:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	blt r1, r4, .LBB108_33
.LBB108_14:
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
	jal r0, .LBB108_18
.LBB108_15:
	addi r18, r16, 1
	addi r20, sp, 24
	add r16, r20, r16
	stb r16+0, r19
	add r16, r18, r0
.LBB108_16:
	add r18, r4, r0
.LBB108_17:
	addi r17, r17, -1
	addi r5, r5, 1
	beq r17, r0, .LBB108_34
.LBB108_18:
	add r19, r8, r0
	bge r5, r6, .LBB108_20
.LBB108_19:
	add r19, r5, r9
	ldb r19, r19+0
.LBB108_20:
	beq r19, r8, .LBB108_22
.LBB108_21:
	bne r19, r14, .LBB108_25
.LBB108_22:
	beq r7, r0, .LBB108_17
.LBB108_23:
	beq r18, r0, .LBB108_17
.LBB108_24:
	add r19, r10, r0
	blt r16, lr, .LBB108_15
	jal r0, .LBB108_17
.LBB108_25:
	ble r16, r15, .LBB108_15
	jal r0, .LBB108_16
.LBB108_26:
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB108_31
.LBB108_27:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB108_28:
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
	bne r5, r0, .LBB108_28
	jal r0, .LBB108_32
.LBB108_29:
	addi r3, r0, 76
	beq r1, r3, .LBB108_42
.LBB108_30:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	jal r0, .LBB108_37
.LBB108_31:
	add r6, r0, r0
.LBB108_32:
	addi r3, sp, 24
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
	jal r0, .LBB108_41
.LBB108_33:
	add r16, r0, r0
.LBB108_34:
	addi r4, sp, 24
	add r4, r4, r16
	stb r4+0, r0
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldbu r1, sp+24
	beq r1, r0, .LBB108_36
.LBB108_35:
	addi r3, sp, 24
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	fcvt.d.w r4, r1
	jal r0, .LBB108_37
.LBB108_36:
	lui r1, %hi(.LCPI108_0)
	addi r1, r1, %lo(.LCPI108_0)
	ldw r5, r1+4
	ldw r4, r1+0
.LBB108_37:
	stw r11+4, r5
	stw r11+0, r4
	ldw r1, r13+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB108_39
.LBB108_38:
	addi r1, r1, -1
	stw r13+%lo(fio_rep), r1
.LBB108_39:
	ldw r1, r13+%lo(fio_rep)
	bne r1, r0, .LBB108_41
.LBB108_40:
	stw r12+%lo(fio_desc), r0
.LBB108_41:
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
.LBB108_42:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end108:
	.size	f77_rd_d, .Lfunc_end108-f77_rd_d
                                        # -- End function
	.p2align	2                               # -- Begin function fio_list_real
	.type	fio_list_real,@function
fio_list_real:                          # @fio_list_real
# %bb.0:
	addi sp, sp, -144
	stw sp+0, lr
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB109_3
.LBB109_1:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB109_2:
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
	bne r5, r0, .LBB109_2
	jal r0, .LBB109_4
.LBB109_3:
	add r6, r0, r0
.LBB109_4:
	addi r3, sp, 16
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	ldw lr, sp+0
	addi sp, sp, 144
	jalr r0, r31, 0
.Lfunc_end109:
	.size	fio_list_real, .Lfunc_end109-fio_list_real
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rd_r
	.type	.LCPI110_0,@object
.LCPI110_0:
	.word	0x00000000                      # float 0
	.size	.LCPI110_0, 4
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
	beq r1, r0, .LBB110_5
.LBB110_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB110_41
.LBB110_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB110_41
.LBB110_3:
	addi r3, r0, 1
	bne r1, r3, .LBB110_26
.LBB110_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB110_41
.LBB110_5:
	lui r12, %hi(fio_desc)
	ldw r1, r12+%lo(fio_desc)
	lui r13, %hi(fio_rep)
	beq r1, r0, .LBB110_11
.LBB110_6:
	ldw r3, r13+%lo(fio_rep)
	ble r3, r0, .LBB110_11
.LBB110_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB110_12
.LBB110_8:
	beq r1, r0, .LBB110_41
.LBB110_9:
	addi r3, r0, 65
	bne r1, r3, .LBB110_30
.LBB110_10:
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
.LBB110_11:
	stw r12+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB110_8
.LBB110_12:
	addi r3, r0, 73
	bne r1, r3, .LBB110_29
.LBB110_13:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	blt r1, r4, .LBB110_33
.LBB110_14:
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
	jal r0, .LBB110_18
.LBB110_15:
	addi r18, r16, 1
	addi r20, sp, 24
	add r16, r20, r16
	stb r16+0, r19
	add r16, r18, r0
.LBB110_16:
	add r18, r4, r0
.LBB110_17:
	addi r17, r17, -1
	addi r5, r5, 1
	beq r17, r0, .LBB110_34
.LBB110_18:
	add r19, r8, r0
	bge r5, r6, .LBB110_20
.LBB110_19:
	add r19, r5, r9
	ldb r19, r19+0
.LBB110_20:
	beq r19, r8, .LBB110_22
.LBB110_21:
	bne r19, r14, .LBB110_25
.LBB110_22:
	beq r7, r0, .LBB110_17
.LBB110_23:
	beq r18, r0, .LBB110_17
.LBB110_24:
	add r19, r10, r0
	blt r16, lr, .LBB110_15
	jal r0, .LBB110_17
.LBB110_25:
	ble r16, r15, .LBB110_15
	jal r0, .LBB110_16
.LBB110_26:
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB110_31
.LBB110_27:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB110_28:
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
	bne r5, r0, .LBB110_28
	jal r0, .LBB110_32
.LBB110_29:
	addi r3, r0, 76
	beq r1, r3, .LBB110_42
.LBB110_30:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	jal r0, .LBB110_37
.LBB110_31:
	add r6, r0, r0
.LBB110_32:
	addi r3, sp, 24
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	jal r0, .LBB110_41
.LBB110_33:
	add r16, r0, r0
.LBB110_34:
	addi r4, sp, 24
	add r4, r4, r16
	stb r4+0, r0
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldbu r1, sp+24
	beq r1, r0, .LBB110_36
.LBB110_35:
	addi r3, sp, 24
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	fcvt.s.w r1, r1
	jal r0, .LBB110_37
.LBB110_36:
	lui r1, %hi(.LCPI110_0)
	ldw r1, r1+%lo(.LCPI110_0)
.LBB110_37:
	stw r11+0, r1
	ldw r1, r13+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB110_39
.LBB110_38:
	addi r1, r1, -1
	stw r13+%lo(fio_rep), r1
.LBB110_39:
	ldw r1, r13+%lo(fio_rep)
	bne r1, r0, .LBB110_41
.LBB110_40:
	stw r12+%lo(fio_desc), r0
.LBB110_41:
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
.LBB110_42:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end110:
	.size	f77_rd_r, .Lfunc_end110-f77_rd_r
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rd_c
	.type	.LCPI111_0,@object
.LCPI111_0:
	.word	0x00000000                      # float 0
	.size	.LCPI111_0, 4
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
	beq r1, r0, .LBB111_5
.LBB111_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB111_38
.LBB111_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB111_38
.LBB111_3:
	addi r14, r0, 1
	bne r1, r14, .LBB111_6
.LBB111_4:
	stb r12+%lo(fio_ldone), r14
	jal r0, .LBB111_38
.LBB111_5:
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
.LBB111_6:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	ldbu r1, r12+0
	addi r3, r0, 40
	bne r1, r3, .LBB111_12
.LBB111_7:
	lui r15, %hi(fio_ltok+1)
	addi r15, r15, %lo(fio_ltok+1)
	ldbu r4, r15+0
	beq r4, r0, .LBB111_15
.LBB111_8:
	lui r1, %hi(fio_ltok+2)
	addi r1, r1, %lo(fio_ltok+2)
	addi r3, r0, 126
	add r13, r0, r0
	jal r0, .LBB111_10
.LBB111_9:
	ldbu r4, r1+0
	addi r1, r1, 1
	beq r4, r0, .LBB111_16
.LBB111_10:
	bgt r13, r3, .LBB111_9
.LBB111_11:
	addi r5, r13, 1
	addi r6, sp, 20
	add r6, r6, r13
	stb r6+0, r4
	add r13, r5, r0
	jal r0, .LBB111_9
.LBB111_12:
	beq r1, r0, .LBB111_28
.LBB111_13:
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB111_14:
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
	bne r1, r0, .LBB111_14
	jal r0, .LBB111_29
.LBB111_15:
	add r13, r0, r0
.LBB111_16:
	lui r16, %hi(fio_rpos)
	lui r17, %hi(fio_rlen)
	addi r18, r0, 126
	lui r19, %hi(fio_rec)
	addi r19, r19, %lo(fio_rec)
	addi r20, r0, 41
.LBB111_17:
	addi r1, sp, 20
	add r21, r1, r13
	jal r0, .LBB111_19
.LBB111_18:
	jal r31, fio_getrec
	bne r1, r0, .LBB111_39
.LBB111_19:
	beq r13, r0, .LBB111_21
.LBB111_20:
	ldbu r1, r21+-1
	beq r1, r20, .LBB111_24
.LBB111_21:
	ldw r1, r16+%lo(fio_rpos)
	ldw r3, r17+%lo(fio_rlen)
	bge r1, r3, .LBB111_18
.LBB111_22:
	addi r3, r1, 1
	stw r16+%lo(fio_rpos), r3
	bgt r13, r18, .LBB111_19
.LBB111_23:
	add r1, r1, r19
	ldbu r1, r1+0
	addi r3, r13, 1
	addi r4, sp, 20
	add r4, r4, r13
	stb r4+0, r1
	add r13, r3, r0
	jal r0, .LBB111_17
.LBB111_24:
	stb r21+-1, r0
	addi r3, sp, 20
	addi r4, r0, 44
	jal r31, strchr
	beq r1, r0, .LBB111_30
.LBB111_25:
	add r13, r1, r0
	stb r1+0, r0
	addi r4, sp, 20
	add r3, r12, r0
	jal r31, strcpy
	ldbu r4, r12+0
	beq r4, r0, .LBB111_31
.LBB111_26:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB111_27:
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
	bne r4, r0, .LBB111_27
	jal r0, .LBB111_32
.LBB111_28:
	add r6, r0, r0
.LBB111_29:
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
	jal r0, .LBB111_38
.LBB111_30:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r4, sp, 20
	jal r31, strcpy
	jal r31, fio_list_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	lui r1, %hi(.LCPI111_0)
	ldw r1, r1+%lo(.LCPI111_0)
	jal r0, .LBB111_37
.LBB111_31:
	add r5, r0, r0
.LBB111_32:
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
	beq r4, r0, .LBB111_35
.LBB111_33:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB111_34:
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
	bne r4, r0, .LBB111_34
	jal r0, .LBB111_36
.LBB111_35:
	add r5, r0, r0
.LBB111_36:
	addi r3, sp, 148
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
.LBB111_37:
	stw r11+4, r1
	lui r1, %hi(fio_l_after)
	stb r1+%lo(fio_l_after), r14
.LBB111_38:
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
.LBB111_39:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end111:
	.size	f77_rd_c, .Lfunc_end111-f77_rd_c
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
	beq r1, r0, .LBB112_5
.LBB112_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB112_26
.LBB112_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB112_26
.LBB112_3:
	addi r3, r0, 1
	bne r1, r3, .LBB112_6
.LBB112_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB112_26
.LBB112_5:
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
.LBB112_6:
	lui r1, %hi(fio_ltok)
	ldbu r1, r1+%lo(fio_ltok)
	addi r3, r0, 40
	bne r1, r3, .LBB112_13
.LBB112_7:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	addi r4, r0, 41
	add r3, r13, r0
	jal r31, strchr
	beq r1, r0, .LBB112_9
.LBB112_8:
	stb r1+0, r0
.LBB112_9:
	lui r3, %hi(fio_ltok+1)
	addi r3, r3, %lo(fio_ltok+1)
	addi r4, r0, 44
	jal r31, strchr
	beq r1, r0, .LBB112_16
.LBB112_10:
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
	beq r4, r0, .LBB112_20
.LBB112_11:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB112_12:
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
	bne r4, r0, .LBB112_12
	jal r0, .LBB112_21
.LBB112_13:
	beq r1, r0, .LBB112_17
.LBB112_14:
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB112_15:
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
	bne r1, r0, .LBB112_15
	jal r0, .LBB112_18
.LBB112_16:
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
	jal r0, .LBB112_19
.LBB112_17:
	add r6, r0, r0
.LBB112_18:
	addi r3, sp, 16
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
.LBB112_19:
	stw r11+4, r2
	stw r11+0, r1
	stw r11+12, r0
	stw r11+8, r0
	jal r0, .LBB112_26
.LBB112_20:
	add r5, r0, r0
.LBB112_21:
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
	beq r4, r0, .LBB112_24
.LBB112_22:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB112_23:
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
	bne r4, r0, .LBB112_23
	jal r0, .LBB112_25
.LBB112_24:
	add r5, r0, r0
.LBB112_25:
	addi r3, sp, 16
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r11+12, r2
	stw r11+8, r1
.LBB112_26:
	ldw r14, sp+144
	ldw r13, sp+148
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jalr r0, r31, 0
.Lfunc_end112:
	.size	f77_rd_z, .Lfunc_end112-f77_rd_z
                                        # -- End function
	.globl	f77_rd_end                      # -- Begin function f77_rd_end
	.p2align	2
	.type	f77_rd_end,@function
f77_rd_end:                             # @f77_rd_end
# %bb.0:
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_reading)
	stb r1+%lo(fio_reading), r0
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	jalr r0, r31, 0
.Lfunc_end113:
	.size	f77_rd_end, .Lfunc_end113-f77_rd_end
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
	add r11, r6, r0
	lui r25, %hi(nml_unit)
	stw r25+%lo(nml_unit), r3
	lui r1, %hi(nml_reading)
	stw r1+%lo(nml_reading), r6
	lui r1, %hi(nml_flags)
	stw r1+%lo(nml_flags), r7
	lui r22, %hi(nml_status)
	stw r22+%lo(nml_status), r0
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
	addi r26, r0, 1
	lui r14, %hi(nml_grp)
	addi r14, r14, %lo(nml_grp)
	blt r5, r26, .LBB114_3
.LBB114_1:
	add r13, r4, r0
	add r15, r0, r0
.LBB114_2:
	add r1, r13, r15
	ldbu r3, r1+0
	jal r31, toupper
	add r3, r15, r14
	stb r3+0, r1
	addi r15, r15, 1
	bne r12, r15, .LBB114_2
.LBB114_3:
	add r1, r12, r14
	stb r1+0, r0
	lui r21, %hi(fio_busy)
	lui r1, %hi(fio_ibuf)
	stw sp+68, r1
	beq r11, r0, .LBB114_66
.LBB114_4:
	lui r1, %hi(nml_nasg)
	stw sp+44, r1
	stw r1+%lo(nml_nasg), r0
	lui r1, %hi(nml_nval)
	stw sp+52, r1
	stw r1+%lo(nml_nval), r0
	lui r3, %hi(nml_grp)
	addi r3, r3, %lo(nml_grp)
	stw sp+56, r3
	jal r31, strlen
	stw sp+60, r1
	addi r11, r1, 2
	lui r1, %hi(nml_tlen)
	stw sp+48, r1
	stw r1+%lo(nml_tlen), r0
	lui r3, %hi(.L.str.7)
	addi r3, r3, %lo(.L.str.7)
	add r4, r0, r0
	jal r31, nml_text_add
	addi r1, sp, 72
	stw sp+36, r11
	add r3, r1, r11
	stw sp+32, r3
	addi r1, r1, 2
	stw sp+40, r1
	lui r13, %hi(.L.str.94)
	addi r13, r13, %lo(.L.str.94)
	addi r11, r0, 3
	addi r15, r0, 1023
	lui r1, %hi(fio_reading)
	stw sp+64, r1
	addi r20, r0, 1022
	addi r23, r0, 32
	addi r24, r0, 2
                                        # implicit-def: $r30
	add r16, r0, r0
	add r27, r22, r0
.LBB114_5:
	ldw r3, r25+%lo(nml_unit)
	add r4, r13, r0
	add r5, r11, r0
	add r6, r11, r0
	jal r31, f77_rd_begin
	beq r1, r0, .LBB114_8
.LBB114_6:
	addi r14, r0, 1
	bne r1, r14, .LBB114_12
.LBB114_7:
	add fp, r1, r0
	add r14, r1, r0
	jal r0, .LBB114_62
.LBB114_8:
	addi r3, sp, 72
	add r4, r15, r0
	jal r31, f77_rd_s
	stb r21+%lo(fio_busy), r0
	ldw r1, sp+64
	stb r1+%lo(fio_reading), r0
	ldw r1, sp+68
	stw r1+%lo(fio_ibuf), r0
	add r1, r20, r0
.LBB114_9:
	addi r3, sp, 72
	add r3, r3, r1
	ldbu r3, r3+0
	bne r3, r23, .LBB114_14
.LBB114_10:
	addi r3, r1, -1
	addi r4, r1, 1
	add r1, r3, r0
	bgtu r4, r24, .LBB114_9
.LBB114_11:
	add r28, r26, r0
	jal r0, .LBB114_15
.LBB114_12:
	ldw r1, r22+%lo(nml_status)
	beq r1, r0, .LBB114_21
.LBB114_13:
	add fp, r0, r0
	jal r0, .LBB114_62
.LBB114_14:
	addi r28, r1, 1
.LBB114_15:
	add r22, r26, r0
	add r19, r26, r0
	bne r16, r0, .LBB114_41
.LBB114_16:
	blt r28, r24, .LBB114_22
.LBB114_17:
	addi r1, r28, -1
	add r14, r0, r0
.LBB114_18:
	addi r3, sp, 72
	add r3, r3, r14
	ldbu r3, r3+1
	bne r3, r23, .LBB114_23
.LBB114_19:
	addi r14, r14, 1
	bne r1, r14, .LBB114_18
.LBB114_20:
	add r22, r0, r0
	add r19, r28, r0
	jal r0, .LBB114_40
.LBB114_21:
	stw r22+%lo(nml_status), r24
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.95)
	addi r4, r4, %lo(.L.str.95)
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
	add fp, r0, r0
	jal r0, .LBB114_62
.LBB114_22:
	add r22, r0, r0
	add r19, r26, r0
	jal r0, .LBB114_40
.LBB114_23:
	addi r1, r0, 36
	beq r3, r1, .LBB114_25
.LBB114_24:
	addi r1, r0, 38
	bne r3, r1, .LBB114_38
.LBB114_25:
	addi r19, r14, 1
	addi r22, r0, 1
	ldw r1, sp+60
	blt r1, r22, .LBB114_30
.LBB114_26:
	ldw r1, sp+40
	add r12, r1, r14
	sub r1, r14, r28
	addi r16, r1, 2
	add r17, r0, r0
.LBB114_27:
	add r1, r16, r17
	beq r1, r0, .LBB114_39
.LBB114_28:
	add r1, r12, r17
	ldbu r3, r1+0
	jal r31, toupper
	ldw r3, sp+56
	add r3, r17, r3
	ldb r3, r3+0
	bne r1, r3, .LBB114_39
.LBB114_29:
	addi r17, r17, 1
	ldw r1, sp+60
	bne r1, r17, .LBB114_27
.LBB114_30:
	ldw r1, sp+36
	add r12, r1, r14
	bge r12, r28, .LBB114_36
.LBB114_31:
	ldw r1, sp+32
	add r1, r1, r14
	ldbu r16, r1+0
	add r3, r16, r0
	jal r31, isalnum
	bne r1, r0, .LBB114_39
.LBB114_32:
	addi r1, r16, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB114_39
.LBB114_33:
	addi r1, r0, 64
	beq r16, r1, .LBB114_39
.LBB114_34:
	addi r3, r0, 95
	add r22, r0, r0
	add r1, r0, r0
	add r14, r11, r0
	beq r16, r3, .LBB114_37
.LBB114_35:
	addi r22, r0, 1
.LBB114_36:
	add r19, r12, r0
	add r1, r22, r0
	add r14, r0, r0
.LBB114_37:
	bne r1, r0, .LBB114_41
	jal r0, .LBB114_59
.LBB114_38:
	addi r19, r14, 1
.LBB114_39:
	add r22, r0, r0
.LBB114_40:
	add r1, r0, r0
	add r14, r11, r0
	beq r1, r0, .LBB114_59
.LBB114_41:
	sub r16, r28, r19
	ble r28, r19, .LBB114_60
.LBB114_42:
	addi r1, sp, 72
	add r17, r1, r19
	add r18, r0, r0
	add r1, r0, r0
	jal r0, .LBB114_44
.LBB114_43:
	addi r18, r18, 1
	beq r16, r18, .LBB114_60
.LBB114_44:
	add r12, r17, r18
	ldb r3, r12+0
	beq r1, r0, .LBB114_46
.LBB114_45:
	seq r3, r1, r3
	sub r3, r0, r3
	and r3, r1, r3
	xor r1, r1, r3
	addi r14, r0, 13
	addi r3, r0, 13
	bne r14, r3, .LBB114_58
	jal r0, .LBB114_43
.LBB114_46:
	andi r1, r3, 255
	addi r14, r0, 13
	addi r4, r0, 34
	beq r1, r4, .LBB114_50
.LBB114_47:
	addi r4, r0, 39
	beq r1, r4, .LBB114_50
.LBB114_48:
	addi r4, r0, 47
	bne r1, r4, .LBB114_51
.LBB114_49:
	add r3, r17, r0
	add r4, r18, r0
	jal r31, nml_text_add
	addi r14, r0, 1
	add fp, r0, r0
	jal r0, .LBB114_57
.LBB114_50:
	add r1, r3, r0
	addi r3, r0, 13
	bne r14, r3, .LBB114_58
	jal r0, .LBB114_43
.LBB114_51:
	addi r1, r0, -3
	and r1, r3, r1
	addi r3, r0, 36
	bne r1, r3, .LBB114_56
.LBB114_52:
	add r1, r19, r18
	addi r1, r1, 3
	bge r1, r28, .LBB114_56
.LBB114_53:
	ldbu r3, r12+1
	jal r31, toupper
	addi r3, r0, 69
	bne r1, r3, .LBB114_56
.LBB114_54:
	ldbu r3, r12+2
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB114_56
.LBB114_55:
	ldbu r3, r12+3
	jal r31, toupper
	addi r3, r0, 68
	beq r1, r3, .LBB114_49
.LBB114_56:
	add r14, r0, r0
.LBB114_57:
	add r1, r0, r0
	addi r3, r0, 13
	beq r14, r3, .LBB114_43
.LBB114_58:
	beq r14, r0, .LBB114_43
.LBB114_59:
	add r16, r22, r0
	jal r0, .LBB114_61
.LBB114_60:
	addi r1, sp, 72
	add r3, r1, r19
	add r4, r16, r0
	jal r31, nml_text_add
	lui r3, %hi(.L.str.96)
	addi r3, r3, %lo(.L.str.96)
	addi r4, r0, 1
	jal r31, nml_text_add
	add r16, r22, r0
	add r14, r0, r0
.LBB114_61:
	add r22, r27, r0
.LBB114_62:
	beq r14, r0, .LBB114_5
.LBB114_63:
	addi r1, r0, 3
	beq r14, r1, .LBB114_5
.LBB114_64:
	beq fp, r0, .LBB114_72
.LBB114_65:
	addi r1, r0, 1
	stw r22+%lo(nml_status), r1
	jal r0, .LBB114_71
.LBB114_66:
	lui r1, 2
	addi r1, r1, 1568
	sth sp+72, r1
	addi r11, sp, 72
	addi r3, r11, 2
	lui r4, %hi(nml_grp)
	addi r4, r4, %lo(nml_grp)
	add r5, r12, r0
	jal r31, memcpy
	addi r12, r12, 2
	ldw r3, r25+%lo(nml_unit)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	add r3, r11, r0
	add r4, r12, r0
	jal r31, f77_wr_a
	stb r21+%lo(fio_busy), r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB114_70
.LBB114_67:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB114_69
.LBB114_68:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB114_70
.LBB114_69:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB114_70:
	jal r31, fio_flush_line
	ldw r1, sp+68
	stw r1+%lo(fio_ibuf), r0
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
.LBB114_71:
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
.LBB114_72:
	ldw r1, sp+44
	stw r1+%lo(nml_nasg), r0
	ldw r1, sp+52
	stw r1+%lo(nml_nval), r0
	ldw r21, sp+48
	ldw r1, r21+%lo(nml_tlen)
	addi r15, r0, 1
	blt r1, r15, .LBB114_71
.LBB114_73:
	ldw r1, r22+%lo(nml_status)
	bne r1, r0, .LBB114_71
.LBB114_74:
	lui r16, %hi(nml_text)
	addi r17, r0, 44
	lui r18, %hi(nml_cval)
	lui r25, %hi(nml_val)
	addi r26, r0, 12
	addi r1, r0, 0
	stw sp+68, r1
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+64, r1
	lui r1, %hi(.L.str.99)
	addi r1, r1, %lo(.L.str.99)
	stw sp+60, r1
	addi r13, r0, 95
	lui r28, %hi(nml_err+95)
	add r12, r15, r0
	add fp, r0, r0
	add r27, r0, r0
.LBB114_75:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r27
	ldbu r3, r1+0
	beq r3, r17, .LBB114_78
.LBB114_76:
	bne r3, r23, .LBB114_85
.LBB114_77:
	addi r11, r27, 1
	jal r0, .LBB114_198
.LBB114_78:
	beq r12, r0, .LBB114_84
.LBB114_79:
	beq fp, r0, .LBB114_84
.LBB114_80:
	ldw r1, sp+52
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB114_83
.LBB114_81:
	ldw r3, sp+68
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
	beq r1, r0, .LBB114_211
.LBB114_82:
	stw r25+%lo(nml_val), r1
.LBB114_83:
	ldw r1, r25+%lo(nml_val)
	ldw r5, sp+52
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
.LBB114_84:
	addi r11, r27, 1
	jal r0, .LBB114_197
.LBB114_85:
	jal r31, isalpha
                                        # implicit-def: $r11
	add r5, r15, r0
	beq r1, r0, .LBB114_106
.LBB114_86:
	ldw r1, r21+%lo(nml_tlen)
	add r19, r27, r0
	bge r27, r1, .LBB114_93
.LBB114_87:
	add r19, r27, r0
	jal r0, .LBB114_89
.LBB114_88:
	addi r19, r19, 1
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB114_93
.LBB114_89:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r19
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB114_88
.LBB114_90:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB114_88
.LBB114_91:
	beq r14, r13, .LBB114_88
.LBB114_92:
	addi r1, r0, 64
	beq r14, r1, .LBB114_88
.LBB114_93:
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB114_103
.LBB114_94:
	ldw r3, r16+%lo(nml_text)
.LBB114_95:
	add r4, r3, r19
	ldbu r4, r4+0
	bne r4, r23, .LBB114_98
.LBB114_96:
	addi r19, r19, 1
	bne r1, r19, .LBB114_95
.LBB114_97:
	add r4, r1, r0
	jal r0, .LBB114_104
.LBB114_98:
	addi r5, r0, 40
	bne r4, r5, .LBB114_103
.LBB114_99:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r19, r1, .LBB114_106
.LBB114_100:
	add r5, r3, r19
	ldbu r6, r5+0
	addi r4, r19, 1
	addi r7, r0, 41
	beq r6, r7, .LBB114_206
.LBB114_101:
	add r19, r4, r0
	blt r4, r1, .LBB114_100
.LBB114_102:
                                        # implicit-def: $r11
	add r5, r15, r0
	jal r0, .LBB114_106
.LBB114_103:
	add r4, r19, r0
.LBB114_104:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r4, r1, .LBB114_106
.LBB114_105:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r4
	ldbu r1, r1+0
	addi r3, r0, 61
	sne r5, r1, r3
	addi r11, r4, 1
.LBB114_106:
	beq r5, r0, .LBB114_116
.LBB114_107:
	beq fp, r0, .LBB114_163
.LBB114_108:
	ldw r1, r21+%lo(nml_tlen)
	add r11, r27, r0
	bge r27, r1, .LBB114_111
.LBB114_109:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB114_111
.LBB114_110:
	addi r11, r11, 1
	ldw r1, r21+%lo(nml_tlen)
	blt r11, r1, .LBB114_109
.LBB114_111:
	ble r11, r27, .LBB114_130
.LBB114_112:
	ldw r1, r21+%lo(nml_tlen)
	bge r11, r1, .LBB114_130
.LBB114_113:
	ldw r1, r16+%lo(nml_text)
	add r3, r1, r11
	ldbu r3, r3+0
	addi r4, r0, 42
	bne r3, r4, .LBB114_130
.LBB114_114:
	add r3, r1, r27
	jal r31, atoi
	ble r1, r0, .LBB114_203
.LBB114_115:
	add r14, r1, r0
	addi r27, r11, 1
	jal r0, .LBB114_131
.LBB114_116:
	ldw r1, sp+44
	ldw r4, r1+%lo(nml_nasg)
	lui r1, %hi(nml_casg)
	ldw r3, r1+%lo(nml_casg)
	lui r14, %hi(nml_asg)
	blt r4, r3, .LBB114_119
.LBB114_117:
	ldw r4, sp+68
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
	beq r1, r0, .LBB114_211
.LBB114_118:
	stw r14+%lo(nml_asg), r1
.LBB114_119:
	ldw r1, r14+%lo(nml_asg)
	ldw r5, sp+44
	ldw r3, r5+%lo(nml_nasg)
	addi r4, r3, 1
	stw r5+%lo(nml_nasg), r4
	addi r4, r0, 76
	mul r3, r3, r4
	add fp, r1, r3
	ldw r1, r21+%lo(nml_tlen)
	bge r27, r1, .LBB114_129
.LBB114_120:
	add r20, r0, r0
.LBB114_121:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r27
	add r1, r1, r20
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	addi r3, r0, 1
	beq r1, r0, .LBB114_125
.LBB114_122:
	beq r3, r0, .LBB114_166
.LBB114_123:
	addi r1, r0, 30
	bgtu r20, r1, .LBB114_166
.LBB114_124:
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
	blt r19, r1, .LBB114_121
	jal r0, .LBB114_167
.LBB114_125:
	addi r1, r14, -35
	addi r4, r0, 2
	bltu r1, r4, .LBB114_122
.LBB114_126:
	addi r1, r0, 64
	beq r14, r1, .LBB114_122
.LBB114_127:
	beq r14, r13, .LBB114_122
.LBB114_128:
	add r3, r0, r0
	jal r0, .LBB114_122
.LBB114_129:
	add r19, r27, r0
	add r20, r0, r0
	jal r0, .LBB114_168
.LBB114_130:
	add r14, r15, r0
.LBB114_131:
	add r12, r0, r0
	add r1, r0, r0
	add r11, r27, r0
	jal r0, .LBB114_136
.LBB114_132:
	add r1, r0, r0
.LBB114_133:
	add r4, r11, r0
.LBB114_134:
	addi r11, r4, 1
	add r3, r0, r0
.LBB114_135:
	bne r3, r0, .LBB114_159
.LBB114_136:
	ldw r3, r21+%lo(nml_tlen)
	bge r11, r3, .LBB114_159
.LBB114_137:
	ldw r6, r16+%lo(nml_text)
	add r4, r6, r11
	ldb r5, r4+0
	beq r1, r0, .LBB114_141
.LBB114_138:
	bne r1, r5, .LBB114_133
.LBB114_139:
	addi r4, r11, 1
	bge r4, r3, .LBB114_132
.LBB114_140:
	add r3, r6, r4
	ldb r3, r3+0
	bne r1, r3, .LBB114_132
	jal r0, .LBB114_134
.LBB114_141:
	andi r19, r5, 255
	addi r1, r19, -32
	addi r6, r0, 9
	bgtu r1, r6, .LBB114_151
.LBB114_142:
	slli r1, r1, 2
	lui r6, %hi(.LJTI114_0)
	addi r6, r6, %lo(.LJTI114_0)
	add r1, r6, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB114_143:
	addi r11, r11, 1
	add r3, r0, r0
	add r1, r5, r0
	ldw r21, sp+48
	jal r0, .LBB114_135
.LBB114_144:
	addi r1, r0, 2
	blt r11, r1, .LBB114_151
.LBB114_145:
	addi r20, r11, 1
	bge r20, r3, .LBB114_151
.LBB114_146:
	ldbu r3, r4+-1
	jal r31, toupper
	addi r1, r1, -70
	addi r3, r0, -2
	bltu r1, r3, .LBB114_151
.LBB114_147:
	ldw r1, r16+%lo(nml_text)
	addi r21, r11, -2
	add r1, r1, r21
	ldbu r3, r1+0
	jal r31, isdigit
	bne r1, r0, .LBB114_149
.LBB114_148:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r21
	ldbu r1, r1+0
	addi r3, r0, 46
	bne r1, r3, .LBB114_151
.LBB114_149:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB114_151
.LBB114_150:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	addi r3, r0, 43
	stb r1+0, r3
	jal r0, .LBB114_156
.LBB114_151:
	bgt r12, r0, .LBB114_156
.LBB114_152:
	addi r3, r0, 1
	ldw r21, sp+48
	beq r19, r23, .LBB114_158
.LBB114_153:
	beq r19, r17, .LBB114_158
	jal r0, .LBB114_157
.LBB114_154:
	addi r12, r12, 1
	jal r0, .LBB114_156
.LBB114_155:
	addi r12, r12, -1
.LBB114_156:
	ldw r21, sp+48
.LBB114_157:
	addi r11, r11, 1
	add r3, r0, r0
.LBB114_158:
	add r1, r0, r0
	jal r0, .LBB114_135
.LBB114_159:
	ldw r1, sp+52
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB114_162
.LBB114_160:
	ldw r3, sp+68
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
	beq r1, r0, .LBB114_211
.LBB114_161:
	stw r25+%lo(nml_val), r1
.LBB114_162:
	ldw r1, r25+%lo(nml_val)
	ldw r5, sp+52
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
	jal r0, .LBB114_199
.LBB114_163:
	ldw r1, r22+%lo(nml_status)
	bne r1, r0, .LBB114_165
.LBB114_164:
	stw r22+%lo(nml_status), r24
	ldw r3, sp+64
	ldw r4, sp+60
	add r5, r13, r0
	jal r31, strncpy
	stb r28+%lo(nml_err+95), r0
.LBB114_165:
	add r11, r27, r0
	add fp, r0, r0
	add r14, r15, r0
	jal r0, .LBB114_199
.LBB114_166:
	add r19, r27, r20
	jal r0, .LBB114_168
.LBB114_167:
	add r20, r3, r0
.LBB114_168:
	add r1, fp, r20
	stb r1+0, r0
	jal r0, .LBB114_170
.LBB114_169:
	addi r19, r19, 1
.LBB114_170:
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB114_175
.LBB114_171:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r19
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB114_169
.LBB114_172:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB114_169
.LBB114_173:
	beq r14, r13, .LBB114_169
.LBB114_174:
	addi r1, r0, 64
	beq r14, r1, .LBB114_169
.LBB114_175:
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB114_180
.LBB114_176:
	ldw r3, r16+%lo(nml_text)
.LBB114_177:
	add r4, r3, r19
	ldbu r4, r4+0
	bne r4, r23, .LBB114_180
.LBB114_178:
	addi r19, r19, 1
	bne r1, r19, .LBB114_177
.LBB114_179:
	add r19, r1, r0
.LBB114_180:
	stw fp+32, r0
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r19
	ldbu r1, r1+0
	addi r3, r0, 40
	bne r1, r3, .LBB114_196
.LBB114_181:
	addi r20, r19, 1
	addi r19, fp, 36
	jal r0, .LBB114_184
.LBB114_182:
	ldw r1, r22+%lo(nml_status)
	addi r14, r0, 1
	beq r1, r0, .LBB114_194
.LBB114_183:
	addi r1, r0, 11
	bne r14, r1, .LBB114_195
.LBB114_184:
	ldw r1, r16+%lo(nml_text)
	addi r20, r20, -1
.LBB114_185:
	add r3, r1, r20
	ldbu r3, r3+1
	addi r20, r20, 1
	beq r3, r23, .LBB114_185
.LBB114_186:
	add r3, r1, r20
	addi r4, sp, 72
	addi r14, r0, 10
	add r5, r14, r0
	jal r31, strtol
	ldw r4, sp+72
	ldw r3, r16+%lo(nml_text)
	add r5, r3, r20
	beq r5, r4, .LBB114_182
.LBB114_187:
	ldw r5, fp+32
	addi r6, r0, 6
	bgt r5, r6, .LBB114_189
.LBB114_188:
	addi r6, r5, 1
	stw fp+32, r6
	slli r5, r5, 2
	add r5, r19, r5
	stw r5+0, r1
.LBB114_189:
	sub r20, r4, r3
.LBB114_190:
	add r1, r3, r20
	ldbu r1, r1+0
	bne r1, r23, .LBB114_192
.LBB114_191:
	addi r20, r20, 1
	jal r0, .LBB114_190
.LBB114_192:
	bne r1, r17, .LBB114_183
.LBB114_193:
	addi r20, r20, 1
	addi r14, r0, 11
	jal r0, .LBB114_183
.LBB114_194:
	addi r1, r0, 2
	stw r22+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.98)
	addi r4, r4, %lo(.L.str.98)
	addi r5, r0, 95
	jal r31, strncpy
	stb r28+%lo(nml_err+95), r0
	jal r0, .LBB114_183
.LBB114_195:
	addi r1, r0, 10
	bne r14, r1, .LBB114_202
.LBB114_196:
	ldw r1, sp+52
	ldw r1, r1+%lo(nml_nval)
	stw fp+64, r1
	stw fp+68, r0
	stw fp+72, r0
.LBB114_197:
	add r12, r15, r0
.LBB114_198:
	add r14, r24, r0
.LBB114_199:
	ori  r1, r14, 2
	bne r1, r24, .LBB114_71
.LBB114_200:
	ldw r1, r21+%lo(nml_tlen)
	bge r11, r1, .LBB114_71
.LBB114_201:
	ldw r1, r22+%lo(nml_status)
	add r27, r11, r0
	beq r1, r0, .LBB114_75
	jal r0, .LBB114_71
.LBB114_202:
	add r11, r27, r0
	jal r0, .LBB114_199
.LBB114_203:
	ldw r1, r22+%lo(nml_status)
	bne r1, r0, .LBB114_205
.LBB114_204:
	addi r1, r0, 2
	stw r22+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
	addi r5, r0, 95
	jal r31, strncpy
	stb r28+%lo(nml_err+95), r0
.LBB114_205:
	add r11, r27, r0
	add r14, r15, r0
	jal r0, .LBB114_199
.LBB114_206:
	bge r4, r1, .LBB114_104
.LBB114_207:
	add r3, r0, r0
.LBB114_208:
	add r4, r5, r3
	ldbu r4, r4+1
	bne r4, r23, .LBB114_210
.LBB114_209:
	addi r3, r3, 1
	add r4, r19, r3
	addi r4, r4, 1
	blt r4, r1, .LBB114_208
	jal r0, .LBB114_104
.LBB114_210:
	add r3, r19, r3
	addi r4, r3, 1
	jal r0, .LBB114_104
.LBB114_211:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.97)
	addi r4, r4, %lo(.L.str.97)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end114:
	.size	f77_nml_begin, .Lfunc_end114-f77_nml_begin
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI114_0,@object
.LJTI114_0:
	.word	.LBB114_144
	.word	.LBB114_151
	.word	.LBB114_143
	.word	.LBB114_151
	.word	.LBB114_151
	.word	.LBB114_151
	.word	.LBB114_151
	.word	.LBB114_143
	.word	.LBB114_154
	.word	.LBB114_155
	.size	.LJTI114_0, 40
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
	bgt r5, r6, .LBB115_2
.LBB115_1:
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
.LBB115_2:
	jalr r0, r31, 0
.Lfunc_end115:
	.size	f77_nml_dim, .Lfunc_end115-f77_nml_dim
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
	add r25, r8, r0
	add r12, r7, r0
	add r27, r6, r0
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
	blt r4, r17, .LBB116_3
.LBB116_1:
	add r13, r16, r0
.LBB116_2:
	ldbu r3, r15+0
	jal r31, toupper
	stb r11+0, r1
	addi r13, r13, -1
	addi r11, r11, 1
	addi r15, r15, 1
	bne r13, r0, .LBB116_2
.LBB116_3:
	addi r1, sp, 248
	add r1, r1, r16
	stb r1+0, r0
	addi r1, r0, 6
	stw sp+108, r1
	beq r14, r1, .LBB116_6
.LBB116_4:
	addi r1, r0, 2
	beq r14, r1, .LBB116_7
.LBB116_5:
	bne r14, r17, .LBB116_8
.LBB116_6:
	addi r1, r0, 0
	sgt r1, r12, r1
	sub r1, r0, r1
	xori r3, r12, 4
	and r1, r3, r1
	xori r28, r1, 4
	jal r0, .LBB116_11
.LBB116_7:
	addi r28, r0, 4
	jal r0, .LBB116_11
.LBB116_8:
	addi r3, r14, -3
	bgeu r3, r1, .LBB116_10
.LBB116_9:
	addi r28, r0, 8
	jal r0, .LBB116_11
.LBB116_10:
	addi r1, r0, 5
	seq r1, r14, r1
	sub r1, r0, r1
	xori r3, r12, 16
	and r1, r3, r1
	xor r28, r12, r1
.LBB116_11:
	lui r1, %hi(nml_reading)
	ldw r1, r1+%lo(nml_reading)
	lui r3, %hi(nml_ndim)
	stw sp+72, r3
	lui fp, %hi(fio_busy)
	lui r3, %hi(fio_listed)
	stw sp+100, r3
	lui r3, %hi(fio_ibuf)
	stw sp+104, r3
	lui r3, %hi(fio_desc)
	stw sp+96, r3
	lui r3, %hi(fio_rep)
	stw sp+92, r3
	stw sp+84, r25
	beq r1, r0, .LBB116_100
.LBB116_12:
	stw sp+52, r17
	lui r24, %hi(nml_status)
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB116_156
.LBB116_13:
	lui r1, %hi(nml_nasg)
	stw sp+68, r1
	ldw r1, r1+%lo(nml_nasg)
	ldw r3, sp+52
	blt r1, r3, .LBB116_156
.LBB116_14:
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB116_156
.LBB116_15:
	addi r1, r0, 36
	stw sp+80, r1
	lui r1, %hi(nml_asg)
	stw sp+64, r1
	addi r1, r0, 76
	stw sp+60, r1
	addi r1, r0, 13
	stw sp+56, r1
	lui r1, %hi(nml_lo)
	addi r1, r1, %lo(nml_lo)
	stw sp+36, r1
	addi r16, r0, 2
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+32, r1
	lui r1, %hi(.L.str.21)
	addi r1, r1, %lo(.L.str.21)
	stw sp+28, r1
	addi r1, r0, 95
	stw sp+44, r1
	lui r1, %hi(nml_err+95)
	stw sp+88, r1
	lui r1, %hi(nml_ext)
	addi r1, r1, %lo(nml_ext)
	stw sp+48, r1
	addi r1, r0, 0
	stw sp+40, r1
	add r4, r0, r0
.LBB116_16:
	ldw r1, sp+64
	ldw r13, r1+%lo(nml_asg)
	ldw r1, sp+60
	stw sp+76, r4
	mul r1, r4, r1
	add r18, r13, r1
	addi r4, sp, 248
	add r3, r18, r0
	jal r31, strcmp
	ldw r11, sp+56
	beq r1, r0, .LBB116_20
.LBB116_17:
	addi r1, r0, 11
	ldw r4, sp+76
	beq r11, r1, .LBB116_156
.LBB116_18:
	addi r4, r4, 1
	ldw r1, sp+68
	ldw r1, r1+%lo(nml_nasg)
	bge r4, r1, .LBB116_156
.LBB116_19:
	ldw r1, r24+%lo(nml_status)
	ldw r3, sp+80
	addi r3, r3, 76
	stw sp+80, r3
	beq r1, r0, .LBB116_16
	jal r0, .LBB116_156
.LBB116_20:
	ldw r20, sp+52
	stw r18+72, r20
	ldw r1, r18+32
	blt r1, r20, .LBB116_31
.LBB116_21:
	ldw r3, sp+72
	ldw r3, r3+%lo(nml_ndim)
	bne r1, r3, .LBB116_32
.LBB116_22:
	ldw r1, r18+32
	blt r1, r20, .LBB116_35
.LBB116_23:
	ldw r1, sp+80
	add r11, r13, r1
	add r13, r0, r0
	add r17, r0, r0
	add r19, r20, r0
	add r15, r0, r0
	ldw r21, sp+36
	ldw r22, sp+32
	ldw r23, sp+28
	jal r0, .LBB116_26
.LBB116_24:
	mul r1, r1, r19
	add r15, r1, r15
	mul r19, r3, r19
	add r1, r20, r0
	beq r1, r0, .LBB116_36
.LBB116_25:
	addi r17, r17, 1
	ldw r1, r18+32
	addi r13, r13, 4
	bge r17, r1, .LBB116_36
.LBB116_26:
	add r1, r11, r13
	ldw r1, r1+0
	add r3, r13, r21
	ldw r3, r3+0
	sub r1, r1, r3
	blt r1, r0, .LBB116_28
.LBB116_27:
	ldw r3, sp+48
	add r3, r13, r3
	ldw r3, r3+0
	blt r1, r3, .LBB116_24
.LBB116_28:
	ldw r1, r24+%lo(nml_status)
	beq r1, r0, .LBB116_30
.LBB116_29:
	add r1, r0, r0
	bne r1, r0, .LBB116_25
	jal r0, .LBB116_36
.LBB116_30:
	stw r24+%lo(nml_status), r16
	add r3, r22, r0
	add r4, r23, r0
	ldw r5, sp+44
	jal r31, strncpy
	ldw r1, sp+88
	stb r1+%lo(nml_err+95), r0
	add r1, r0, r0
	bne r1, r0, .LBB116_25
	jal r0, .LBB116_36
.LBB116_31:
	add r15, r0, r0
	jal r0, .LBB116_37
.LBB116_32:
	ldw r1, r24+%lo(nml_status)
	addi r11, r0, 11
	bne r1, r0, .LBB116_34
.LBB116_33:
	stw r24+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+88
	stb r1+%lo(nml_err+95), r0
.LBB116_34:
	add r1, r0, r0
	add r15, r0, r0
	beq r1, r0, .LBB116_17
	jal r0, .LBB116_37
.LBB116_35:
	add r15, r0, r0
.LBB116_36:
	ldw r1, r24+%lo(nml_status)
	ldw r3, sp+40
	seq r1, r1, r3
	addi r3, r1, -1
	andi r11, r3, 11
	beq r1, r0, .LBB116_17
.LBB116_37:
	ldw r1, r18+68
	addi r23, r0, 1
	blt r1, r23, .LBB116_99
.LBB116_38:
	ldw r1, r24+%lo(nml_status)
	add r11, r0, r0
	bne r1, r0, .LBB116_17
.LBB116_39:
	ldw r22, r18+64
.LBB116_40:
	lui r13, %hi(nml_val)
	ldw r1, r13+%lo(nml_val)
	addi r3, r0, 12
	mul r17, r22, r3
	add r3, r1, r17
	ldw r1, r3+8
	blt r1, r23, .LBB116_97
.LBB116_41:
	bge r15, r25, .LBB116_94
.LBB116_42:
	xor r1, r15, r25
	sgt r4, r15, r25
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r25, r1
	sub r26, r1, r15
	mul r1, r28, r15
	add r11, r27, r1
	add r25, r0, r0
.LBB116_43:
	ldw r1, r3+4
	blt r1, r23, .LBB116_92
.LBB116_44:
	mul r4, r15, r28
	add r19, r27, r4
	lui r4, %hi(nml_text)
	ldw r4, r4+%lo(nml_text)
	ldw r3, r3+0
	add r4, r4, r3
	addi r3, r0, 7
	beq r14, r3, .LBB116_55
.LBB116_45:
	ldw r3, sp+108
	bne r14, r3, .LBB116_67
.LBB116_46:
	ldbu r3, r4+0
	addi r5, r0, 46
	seq r3, r3, r5
	ble r1, r3, .LBB116_49
.LBB116_47:
	add r20, r4, r3
	ldbu r3, r20+0
	jal r31, toupper
	addi r4, r0, 84
	add r3, r23, r0
	beq r1, r4, .LBB116_52
.LBB116_48:
	ldbu r3, r20+0
	jal r31, toupper
	addi r3, r0, 70
	beq r1, r3, .LBB116_51
.LBB116_49:
	ldw r1, r24+%lo(nml_status)
	add r3, r0, r0
	bne r1, r0, .LBB116_52
.LBB116_50:
	stw r24+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.104)
	addi r4, r4, %lo(.L.str.104)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+88
	stb r1+%lo(nml_err+95), r0
.LBB116_51:
	add r3, r0, r0
.LBB116_52:
	beq r12, r16, .LBB116_80
.LBB116_53:
	bne r12, r23, .LBB116_81
.LBB116_54:
	stb r19+0, r3
	jal r0, .LBB116_92
.LBB116_55:
	ldbu r3, r4+0
	addi r5, r0, 34
	beq r3, r5, .LBB116_57
.LBB116_56:
	addi r5, r0, 39
	bne r3, r5, .LBB116_82
.LBB116_57:
	add r5, r0, r0
	addi r6, r0, 1
	bne r1, r6, .LBB116_62
.LBB116_58:
	ble r12, r5, .LBB116_92
.LBB116_59:
	add r1, r11, r5
	sub r3, r12, r5
.LBB116_60:
	addi r4, r0, 32
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB116_60
	jal r0, .LBB116_92
.LBB116_61:
	addi r6, r6, 1
	bge r6, r1, .LBB116_58
.LBB116_62:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB116_65
.LBB116_63:
	addi r6, r6, 1
	bge r6, r1, .LBB116_58
.LBB116_64:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB116_58
.LBB116_65:
	bge r5, r12, .LBB116_61
.LBB116_66:
	add r7, r4, r6
	ldbu r7, r7+0
	addi r8, r5, 1
	add r5, r19, r5
	stb r5+0, r7
	add r5, r8, r0
	jal r0, .LBB116_61
.LBB116_67:
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
	ldbu r1, fp+%lo(fio_busy)
	beq r1, r23, .LBB116_157
.LBB116_68:
	stb fp+%lo(fio_busy), r23
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB116_72
.LBB116_69:
	addi r3, r0, -400
.LBB116_70:
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	add r4, r3, r4
	addi r5, r0, -1
	stw r4+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB116_70
.LBB116_71:
	stb r1+%lo(fio_uend_init), r23
.LBB116_72:
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
	lui r1, %hi(.L.str.7)
	addi r1, r1, %lo(.L.str.7)
	lui r4, %hi(fio_fmt)
	stw r4+%lo(fio_fmt), r1
	lui r1, %hi(fio_pos)
	stw r1+%lo(fio_pos), r0
	lui r1, %hi(fio_revert)
	stw r1+%lo(fio_revert), r3
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	jal r31, fio_getrec
	beq r1, r0, .LBB116_75
.LBB116_73:
	stb r20+%lo(fio_reading), r0
	stb fp+%lo(fio_busy), r0
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB116_92
.LBB116_74:
	stw r24+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.106)
	addi r4, r4, %lo(.L.str.106)
	jal r0, .LBB116_84
.LBB116_75:
	addi r1, r14, -1
	addi r3, r0, 3
	bgtu r1, r3, .LBB116_88
.LBB116_76:
	slli r1, r1, 2
	lui r3, %hi(.LJTI116_0)
	addi r3, r3, %lo(.LJTI116_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB116_77:
	stw sp+184, r0
	addi r3, sp, 184
	jal r31, f77_rd_i
	ldw r1, sp+184
	beq r12, r16, .LBB116_89
.LBB116_78:
	bne r12, r23, .LBB116_90
.LBB116_79:
	stb r19+0, r1
	jal r0, .LBB116_91
.LBB116_80:
	sth r19+0, r3
	jal r0, .LBB116_92
.LBB116_81:
	stw r19+0, r3
	jal r0, .LBB116_92
.LBB116_82:
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB116_92
.LBB116_83:
	stw r24+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.105)
	addi r4, r4, %lo(.L.str.105)
.LBB116_84:
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+88
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB116_92
.LBB116_85:
	add r3, r19, r0
	jal r31, f77_rd_d
	jal r0, .LBB116_91
.LBB116_86:
	add r3, r19, r0
	jal r31, f77_rd_c
	jal r0, .LBB116_91
.LBB116_87:
	add r3, r19, r0
	jal r31, f77_rd_r
	jal r0, .LBB116_91
.LBB116_88:
	add r3, r19, r0
	jal r31, f77_rd_z
	jal r0, .LBB116_91
.LBB116_89:
	sth r19+0, r1
	jal r0, .LBB116_91
.LBB116_90:
	stw r19+0, r1
.LBB116_91:
	stb fp+%lo(fio_busy), r0
	stb r20+%lo(fio_reading), r0
	ldw r1, sp+104
	stw r1+%lo(fio_ibuf), r0
.LBB116_92:
	addi r15, r15, 1
	addi r25, r25, 1
	ldw r1, r13+%lo(nml_val)
	add r3, r1, r17
	ldw r1, r3+8
	bge r25, r1, .LBB116_96
.LBB116_93:
	add r11, r11, r28
	bne r25, r26, .LBB116_43
.LBB116_94:
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB116_96
.LBB116_95:
	stw r24+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+88
	stb r1+%lo(nml_err+95), r0
.LBB116_96:
	ldw r25, sp+84
.LBB116_97:
	addi r22, r22, 1
	ldw r1, r18+64
	ldw r3, r18+68
	add r1, r3, r1
	bge r22, r1, .LBB116_99
.LBB116_98:
	ldw r1, r24+%lo(nml_status)
	add r11, r0, r0
	beq r1, r0, .LBB116_40
	jal r0, .LBB116_17
.LBB116_99:
	add r11, r0, r0
	jal r0, .LBB116_17
.LBB116_100:
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
	blt r1, r0, .LBB116_102
.LBB116_101:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	addi r3, r0, 44
	stb r1+0, r3
	add r3, r15, r0
	jal r31, nml_put
	addi r1, r0, -1
	stw r21+%lo(nml_npend), r1
.LBB116_102:
	addi r4, r16, 1
	addi r3, sp, 144
	jal r31, nml_put
	addi r16, r0, 1
	blt r25, r16, .LBB116_156
.LBB116_103:
	addi r18, r0, 5
	seq r3, r14, r18
	addi r1, r0, -2
	and r19, r14, r1
	stw sp+60, r3
	sub r1, r0, r3
	andi r1, r1, 16
	addi r1, r1, 33
	stw sp+52, r1
	addi r22, r0, 3
	addi r24, r0, 23
	addi r26, r0, 44
	addi r11, r0, -1
	addi r17, r0, 2
	addi r1, r0, 15
	stw sp+68, r1
	lui r1, %hi(.L.str.17)
	addi r1, r1, %lo(.L.str.17)
	stw sp+64, r1
	addi r1, r0, 11
	stw sp+88, r1
	add r13, r27, r0
	add r20, r0, r0
	stw sp+56, r19
	stw sp+48, r22
	stw sp+44, r24
	stw sp+76, r26
	stw sp+80, r11
	jal r0, .LBB116_107
.LBB116_104:
	addi r4, sp, 184
	add r3, r15, r0
	ldw r23, sp+68
.LBB116_105:
	add r5, r23, r0
	jal r31, memcpy
	stw r21+%lo(nml_npend), r23
.LBB116_106:
	addi r20, r20, 1
	add r13, r13, r28
	beq r20, r25, .LBB116_156
.LBB116_107:
	mul r1, r20, r28
	add r1, r27, r1
	beq r14, r22, .LBB116_115
.LBB116_108:
	beq r14, r17, .LBB116_113
.LBB116_109:
	bne r14, r16, .LBB116_118
.LBB116_110:
	beq r12, r17, .LBB116_128
.LBB116_111:
	addi r3, r0, 1
	bne r12, r3, .LBB116_129
.LBB116_112:
	ldbu r23, r1+0
	jal r0, .LBB116_130
.LBB116_113:
	ldw r1, r1+0
	fcvt.d.s r6, r1
	addi r3, sp, 184
	add r5, r6, r0
	add r6, r7, r0
	add r4, r0, r0
	jal r31, nml_real
	ldw r1, r21+%lo(nml_npend)
	blt r1, r0, .LBB116_104
.LBB116_114:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	stw r21+%lo(nml_npend), r11
	jal r0, .LBB116_104
.LBB116_115:
	ldw r6, r1+4
	ldw r5, r1+0
	addi r3, sp, 184
	add r4, r16, r0
	jal r31, nml_real
	ldw r1, r21+%lo(nml_npend)
	blt r1, r0, .LBB116_117
.LBB116_116:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	stw r21+%lo(nml_npend), r11
.LBB116_117:
	addi r4, sp, 184
	add r3, r15, r0
	add r5, r24, r0
	jal r31, memcpy
	stw r21+%lo(nml_npend), r24
	jal r0, .LBB116_106
.LBB116_118:
	addi r3, r0, 4
	bne r19, r3, .LBB116_122
.LBB116_119:
	bne r14, r18, .LBB116_137
.LBB116_120:
	ldw r7, r1+4
	ldw r6, r1+0
	add r11, r18, r0
	beq r14, r18, .LBB116_138
.LBB116_121:
	ldw r1, r1+4
	fcvt.d.s r18, r1
	jal r0, .LBB116_139
.LBB116_122:
	ldw r3, sp+108
	bne r14, r3, .LBB116_144
.LBB116_123:
	beq r12, r17, .LBB116_154
.LBB116_124:
	addi r3, r0, 1
	bne r12, r3, .LBB116_155
.LBB116_125:
	ldbu r11, r1+0
	ldw r1, r21+%lo(nml_npend)
	blt r1, r0, .LBB116_127
.LBB116_126:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	ldw r1, sp+80
	stw r21+%lo(nml_npend), r1
.LBB116_127:
	addi r1, r0, 0
	seq r1, r11, r1
	lui r3, %hi(.L.str.18)
	addi r3, r3, %lo(.L.str.18)
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r4, r3, r1
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r23, r0, 1
	jal r0, .LBB116_153
.LBB116_128:
	ldh r23, r1+0
	jal r0, .LBB116_130
.LBB116_129:
	ldw r23, r1+0
.LBB116_130:
	addi r3, sp, 184
	ldw r4, sp+88
	add r5, r16, r0
	ldw r6, sp+64
	add r7, r11, r0
	jal r31, f77_wr_begin_int
	add r3, r23, r0
	jal r31, f77_wr_i
	stb fp+%lo(fio_busy), r0
	ldw r1, sp+100
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB116_134
.LBB116_131:
	ldw r1, sp+96
	ldw r1, r1+%lo(fio_desc)
	beq r1, r0, .LBB116_133
.LBB116_132:
	ldw r1, sp+92
	ldw r1, r1+%lo(fio_rep)
	bgt r1, r0, .LBB116_134
.LBB116_133:
	lui r11, %hi(fio_draining)
	addi r1, r0, 1
	stb r11+%lo(fio_draining), r1
	ldw r1, sp+96
	stw r1+%lo(fio_desc), r0
	ldw r1, sp+92
	stw r1+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
	ldw r11, sp+80
.LBB116_134:
	jal r31, fio_flush_line
	ldw r1, sp+104
	stw r1+%lo(fio_ibuf), r0
	stb sp+195, r0
	ldw r1, r21+%lo(nml_npend)
	blt r1, r0, .LBB116_136
.LBB116_135:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	stw r21+%lo(nml_npend), r11
.LBB116_136:
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r4, sp, 184
	ldw r23, sp+88
	jal r0, .LBB116_105
.LBB116_137:
	ldw r3, r1+0
	fcvt.d.s r6, r3
	add r11, r18, r0
	bne r14, r18, .LBB116_121
.LBB116_138:
	ldw r19, r1+12
	ldw r18, r1+8
.LBB116_139:
	addi r1, r0, 40
	stb sp+280, r1
	addi r23, sp, 112
	add r3, r23, r0
	add r5, r6, r0
	add r6, r7, r0
	ldw r22, sp+60
	add r4, r22, r0
	jal r31, nml_real
	addi r24, sp, 280
	addi r25, r0, 1
	add r3, r24, r0
	add r4, r25, r0
	add r5, r23, r0
	jal r31, nml_trim_cat
	addi r26, r1, 1
	add r1, r24, r1
	ldw r3, sp+76
	stb r1+0, r3
	add r3, r23, r0
	add r5, r18, r0
	add r6, r19, r0
	add r4, r22, r0
	jal r31, nml_real
	add r3, r24, r0
	add r4, r26, r0
	add r5, r23, r0
	jal r31, nml_trim_cat
	addi r23, r1, 1
	add r1, r24, r1
	addi r3, r0, 41
	stb r1+0, r3
	ldw r18, sp+52
	sub r24, r18, r23
	blt r24, r25, .LBB116_141
.LBB116_140:
	addi r25, sp, 280
	add r3, r25, r24
	add r4, r25, r0
	add r5, r23, r0
	jal r31, memmove
	addi r4, r0, 32
	add r3, r25, r0
	add r5, r24, r0
	jal r31, memset
	add r23, r18, r0
.LBB116_141:
	ldw r25, sp+84
	add r18, r11, r0
	ldw r19, sp+56
	ldw r22, sp+48
	ldw r24, sp+44
	ldw r1, r21+%lo(nml_npend)
	ldw r26, sp+76
	ldw r11, sp+80
	blt r1, r0, .LBB116_143
.LBB116_142:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	stw r21+%lo(nml_npend), r11
.LBB116_143:
	addi r1, r0, 1022
	slt r1, r23, r1
	sub r1, r0, r1
	xori r3, r23, 1022
	and r1, r3, r1
	xori r23, r1, 1022
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r4, sp, 280
	jal r0, .LBB116_105
.LBB116_144:
	addi r1, r0, 39
	stb sp+280, r1
	add r11, r16, r0
	blt r12, r16, .LBB116_150
.LBB116_145:
	addi r11, r0, 1
	add r3, r0, r0
	jal r0, .LBB116_148
.LBB116_146:
	add r11, r4, r0
	addi r3, r3, 1
	bge r3, r12, .LBB116_150
.LBB116_147:
	addi r4, r0, 1021
	bge r11, r4, .LBB116_150
.LBB116_148:
	add r4, r13, r3
	ldbu r5, r4+0
	addi r4, r11, 1
	addi r6, sp, 280
	add r6, r6, r11
	stb r6+0, r5
	bne r5, r1, .LBB116_146
.LBB116_149:
	addi r11, r11, 2
	addi r5, sp, 280
	add r4, r5, r4
	stb r4+0, r1
	addi r3, r3, 1
	blt r3, r12, .LBB116_147
.LBB116_150:
	addi r3, sp, 280
	add r3, r3, r11
	stb r3+0, r1
	ldw r1, r21+%lo(nml_npend)
	blt r1, r0, .LBB116_152
.LBB116_151:
	addi r4, r1, 1
	stw r21+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	ldw r1, sp+80
	stw r21+%lo(nml_npend), r1
.LBB116_152:
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
.LBB116_153:
	add r5, r23, r0
	jal r31, memcpy
	stw r21+%lo(nml_npend), r23
	ldw r11, sp+80
	jal r0, .LBB116_106
.LBB116_154:
	ldh r11, r1+0
	ldw r1, r21+%lo(nml_npend)
	bge r1, r0, .LBB116_126
	jal r0, .LBB116_127
.LBB116_155:
	ldw r11, r1+0
	ldw r1, r21+%lo(nml_npend)
	bge r1, r0, .LBB116_126
	jal r0, .LBB116_127
.LBB116_156:
	ldw r1, sp+72
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
.LBB116_157:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end116:
	.size	f77_nml_item, .Lfunc_end116-f77_nml_item
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI116_0,@object
.LJTI116_0:
	.word	.LBB116_77
	.word	.LBB116_87
	.word	.LBB116_85
	.word	.LBB116_86
	.size	.LJTI116_0, 16
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
	blt r13, r1, .LBB117_7
.LBB117_1:
	add r1, r13, r4
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB117_7
.LBB117_2:
	add r16, r4, r0
	lui r1, %hi(nml_unit)
	ldw r3, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	lui r12, %hi(nml_line)
	addi r12, r12, %lo(nml_line)
	add r3, r12, r0
	add r4, r13, r0
	jal r31, f77_wr_a
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB117_6
.LBB117_3:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB117_5
.LBB117_4:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB117_6
.LBB117_5:
	lui r13, %hi(fio_draining)
	addi r4, r0, 1
	stb r13+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r13+%lo(fio_draining), r0
.LBB117_6:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	addi r1, r0, 32
	stb r12+0, r1
	addi r1, r0, 1
	stw r14+%lo(nml_len), r1
	add r4, r16, r0
.LBB117_7:
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
.Lfunc_end117:
	.size	nml_put, .Lfunc_end117-nml_put
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function nml_real
	.type	.LCPI118_0,@object
.LCPI118_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI118_0, 8
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
	lui r1, %hi(.LCPI118_0)
	addi r1, r1, %lo(.LCPI118_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r14, r6
	xori r1, r1, 1
	bne r1, r0, .LBB118_4
.LBB118_1:
	addi r4, r0, 32
	add r3, r11, r0
	add r5, r12, r0
	jal r31, memset
	addi r3, r11, 1
	lui r4, %hi(.L.str.101)
	addi r4, r4, %lo(.L.str.101)
	addi r5, r0, 3
	jal r31, memcpy
.LBB118_2:
	add r1, r11, r12
	stb r1+0, r0
.LBB118_3:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB118_4:
	beq r4, r0, .LBB118_13
.LBB118_5:
	lui r6, %hi(.L.str.102)
	addi r6, r6, %lo(.L.str.102)
	addi r13, r0, 1
	addi r7, r0, -1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, f77_wr_begin_int
	add r3, r14, r0
	add r4, r15, r0
	jal r31, f77_wr_d
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB118_9
.LBB118_6:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB118_8
.LBB118_7:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB118_9
.LBB118_8:
	lui r14, %hi(fio_draining)
	stb r14+%lo(fio_draining), r13
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r14+%lo(fio_draining), r0
.LBB118_9:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	add r1, r11, r12
	stb r1+0, r0
	addi r1, r0, 69
	addi r3, r0, 68
	jal r0, .LBB118_11
.LBB118_10:
	addi r12, r12, -1
	addi r11, r11, 1
	beq r12, r0, .LBB118_3
.LBB118_11:
	ldbu r4, r11+0
	bne r4, r1, .LBB118_10
.LBB118_12:
	stb r11+0, r3
	jal r0, .LBB118_10
.LBB118_13:
	fcvt.s.d r14, r14
	lui r6, %hi(.L.str.103)
	addi r6, r6, %lo(.L.str.103)
	addi r13, r0, 1
	addi r7, r0, -1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, f77_wr_begin_int
	add r3, r14, r0
	jal r31, f77_wr_r
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB118_17
.LBB118_14:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB118_16
.LBB118_15:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB118_17
.LBB118_16:
	lui r14, %hi(fio_draining)
	stb r14+%lo(fio_draining), r13
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r14+%lo(fio_draining), r0
.LBB118_17:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	jal r0, .LBB118_2
.Lfunc_end118:
	.size	nml_real, .Lfunc_end118-nml_real
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
	blt r13, r1, .LBB119_4
.LBB119_1:
	add r1, r14, r16
	ldbu r1, r1+0
	bne r1, r17, .LBB119_4
.LBB119_2:
	addi r16, r16, 1
	bne r13, r16, .LBB119_1
.LBB119_3:
	add r16, r13, r0
.LBB119_4:
	sub r1, r13, r16
	addi r3, r0, 3
	add r15, r14, r16
	blt r1, r3, .LBB119_6
.LBB119_5:
	lui r4, %hi(.L.str.101)
	addi r4, r4, %lo(.L.str.101)
	addi r5, r0, 3
	add r3, r15, r0
	jal r31, memcmp
	beq r1, r0, .LBB119_11
.LBB119_6:
	ble r13, r16, .LBB119_12
.LBB119_7:
	addi r1, r13, -1
	xor r3, r16, r1
	slt r4, r16, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	addi r3, r14, -1
.LBB119_8:
	add r4, r3, r13
	ldbu r4, r4+0
	bne r4, r17, .LBB119_12
.LBB119_9:
	addi r13, r13, -1
	bgt r13, r16, .LBB119_8
.LBB119_10:
	add r13, r1, r0
	jal r0, .LBB119_12
.LBB119_11:
	addi r1, r16, 3
	bne r1, r13, .LBB119_13
.LBB119_12:
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
.LBB119_13:
	add r1, r14, r1
	ldbu r1, r1+0
	bne r1, r17, .LBB119_6
	jal r0, .LBB119_12
.Lfunc_end119:
	.size	nml_trim_cat, .Lfunc_end119-nml_trim_cat
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
	lui r1, %hi(nml_reading)
	ldw r1, r1+%lo(nml_reading)
	beq r1, r0, .LBB120_6
.LBB120_1:
	lui r11, %hi(nml_status)
	ldw r1, r11+%lo(nml_status)
	beq r1, r0, .LBB120_19
.LBB120_2:
	ldw r1, r11+%lo(nml_status)
	addi r3, r0, 2
	beq r1, r3, .LBB120_23
.LBB120_3:
	addi r3, r0, 1
	bne r1, r3, .LBB120_24
.LBB120_4:
	lui r1, %hi(nml_flags)
	ldbu r1, r1+%lo(nml_flags)
	andi r4, r1, 1
	add r1, r3, r0
	bne r4, r0, .LBB120_24
.LBB120_5:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r1, %hi(nml_unit)
	ldw r5, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	lui r6, %hi(nml_grp)
	addi r6, r6, %lo(nml_grp)
	jal r0, .LBB120_28
.LBB120_6:
	lui r11, %hi(nml_npend)
	ldw r4, r11+%lo(nml_npend)
	blt r4, r0, .LBB120_8
.LBB120_7:
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	jal r31, nml_put
	addi r1, r0, -1
	stw r11+%lo(nml_npend), r1
.LBB120_8:
	lui r15, %hi(nml_len)
	ldw r11, r15+%lo(nml_len)
	addi r1, r0, 2
	lui r13, %hi(fio_busy)
	lui r14, %hi(fio_listed)
	lui r12, %hi(fio_ibuf)
	blt r11, r1, .LBB120_14
.LBB120_9:
	lui r1, %hi(nml_unit)
	ldw r3, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	lui r3, %hi(nml_line)
	addi r3, r3, %lo(nml_line)
	add r4, r11, r0
	jal r31, f77_wr_a
	stb r13+%lo(fio_busy), r0
	ldw r1, r14+%lo(fio_listed)
	bne r1, r0, .LBB120_13
.LBB120_10:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB120_12
.LBB120_11:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB120_13
.LBB120_12:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB120_13:
	jal r31, fio_flush_line
	stw r12+%lo(fio_ibuf), r0
.LBB120_14:
	lui r1, %hi(nml_line)
	addi r3, r0, 32
	stb r1+%lo(nml_line), r3
	addi r11, r0, 1
	stw r15+%lo(nml_len), r11
	lui r1, %hi(nml_unit)
	ldw r3, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	lui r3, %hi(.L.str.23)
	addi r3, r3, %lo(.L.str.23)
	addi r4, r0, 5
	jal r31, f77_wr_a
	stb r13+%lo(fio_busy), r0
	ldw r1, r14+%lo(fio_listed)
	bne r1, r0, .LBB120_18
.LBB120_15:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB120_17
.LBB120_16:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB120_18
.LBB120_17:
	lui r13, %hi(fio_draining)
	stb r13+%lo(fio_draining), r11
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r13+%lo(fio_draining), r0
.LBB120_18:
	jal r31, fio_flush_line
	stw r12+%lo(fio_ibuf), r0
	add r1, r0, r0
	jal r0, .LBB120_24
.LBB120_19:
	lui r1, %hi(nml_nasg)
	ldw r1, r1+%lo(nml_nasg)
	addi r3, r0, 1
	blt r1, r3, .LBB120_2
.LBB120_20:
	lui r3, %hi(nml_asg)
	ldw r5, r3+%lo(nml_asg)
.LBB120_21:
	ldw r3, r5+72
	beq r3, r0, .LBB120_25
.LBB120_22:
	addi r5, r5, 76
	addi r1, r1, -1
	bne r1, r0, .LBB120_21
	jal r0, .LBB120_2
.LBB120_23:
	lui r3, %hi(nml_flags)
	ldbu r3, r3+%lo(nml_flags)
	andi r3, r3, 2
	beq r3, r0, .LBB120_27
.LBB120_24:
	ldw r15, sp+124
	ldw r14, sp+128
	ldw r13, sp+132
	ldw r12, sp+136
	ldw r11, sp+140
	ldw lr, sp+0
	addi sp, sp, 144
	jalr r0, r31, 0
.LBB120_25:
	lui r4, %hi(.L.str.24)
	addi r4, r4, %lo(.L.str.24)
	addi r3, sp, 28
	jal r31, sprintf
	ldw r1, r11+%lo(nml_status)
	bne r1, r0, .LBB120_2
.LBB120_26:
	addi r1, r0, 2
	stw r11+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	addi r4, sp, 28
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB120_2
.LBB120_27:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r1, %hi(nml_unit)
	ldw r6, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.26)
	addi r4, r4, %lo(.L.str.26)
	lui r5, %hi(nml_grp)
	addi r5, r5, %lo(nml_grp)
	lui r7, %hi(nml_err)
	addi r7, r7, %lo(nml_err)
.LBB120_28:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end120:
	.size	f77_nml_end, .Lfunc_end120-f77_nml_end
                                        # -- End function
	.globl	f77_ipow                        # -- Begin function f77_ipow
	.p2align	2
	.type	f77_ipow,@function
f77_ipow:                               # @f77_ipow
# %bb.0:
	addi r1, r0, 1
	blt r4, r0, .LBB121_4
.LBB121_1:
	beq r4, r0, .LBB121_9
.LBB121_2:
	addi r5, r0, 0
.LBB121_3:
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
	bne r4, r0, .LBB121_3
	jal r0, .LBB121_9
.LBB121_4:
	beq r3, r1, .LBB121_7
.LBB121_5:
	addi r1, r0, -1
	bne r3, r1, .LBB121_8
.LBB121_6:
	andi r1, r4, 1
	addi r3, r0, 0
	seq r1, r1, r3
	addi r1, r1, -1
	ori  r1, r1, 1
	jal r0, .LBB121_9
.LBB121_7:
	add r1, r3, r0
	jal r0, .LBB121_9
.LBB121_8:
	add r1, r0, r0
.LBB121_9:
	jalr r0, r31, 0
.Lfunc_end121:
	.size	f77_ipow, .Lfunc_end121-f77_ipow
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_dpow_i
	.type	.LCPI122_0,@object
.LCPI122_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI122_0, 8
	.type	.LCPI122_1,@object
.LCPI122_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI122_1, 8
	.type	.LCPI122_2,@object
.LCPI122_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI122_2, 8
	.type	.LCPI122_3,@object
.LCPI122_3:
	.quad	0x7fefdcf158adbb99              # double 1.79E+308
	.size	.LCPI122_3, 8
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
	blt r5, r0, .LBB122_3
.LBB122_1:
	andi r3, r5, 1
	addi r1, r0, 0
	seq r3, r3, r1
	lui r4, %hi(.LCPI122_0)
	addi r4, r4, %lo(.LCPI122_0)
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
	beq r3, r0, .LBB122_16
.LBB122_2:
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
	bne r3, r0, .LBB122_2
	jal r0, .LBB122_16
.LBB122_3:
	sub r1, r0, r5
	lui r3, %hi(.LCPI122_0)
	addi r3, r3, %lo(.LCPI122_0)
	ldw r5, r3+4
	ldw r4, r3+0
	lui r3, %hi(.LCPI122_1)
	addi r3, r3, %lo(.LCPI122_1)
	ldw r9, r3+4
	ldw r8, r3+0
	lui r3, %hi(.LCPI122_3)
	addi r3, r3, %lo(.LCPI122_3)
	ldw r11, r3+4
	ldw r10, r3+0
	lui r3, %hi(.LCPI122_2)
	addi r3, r3, %lo(.LCPI122_2)
	ldw r15, r3+4
	ldw r14, r3+0
	add r16, r6, r0
	add r17, r7, r0
	add r3, r1, r0
	add r12, r4, r0
	add r13, r5, r0
	jal r0, .LBB122_5
.LBB122_4:
	fmul.d r16, r16, r16
.LBB122_5:
	andi lr, r3, 1
	fneg.d r18, r16
	beq lr, r0, .LBB122_10
.LBB122_6:
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
	fdiv.d r24, r10, r20
	fle.d lr, r22, r24
	bne lr, r0, .LBB122_9
.LBB122_7:
	fle.d lr, r20, r4
	bne lr, r0, .LBB122_9
.LBB122_8:
	fle.d lr, r20, r14
	xori lr, lr, 1
	fle.d r20, r22, r14
	xori r20, r20, 1
	or  lr, r20, lr
	bne lr, r0, .LBB122_13
.LBB122_9:
	fmul.d r12, r16, r12
.LBB122_10:
	srli r3, r3, 1
	beq r3, r0, .LBB122_15
.LBB122_11:
	flt.d lr, r16, r8
	xor r20, r19, r17
	sub lr, r0, lr
	and r20, r20, lr
	xor r21, r17, r20
	xor r18, r18, r16
	and lr, r18, lr
	xor r20, r16, lr
	fle.d lr, r20, r14
	bne lr, r0, .LBB122_4
.LBB122_12:
	fdiv.d r18, r10, r20
	fle.d lr, r20, r18
	xori lr, lr, 1
	beq lr, r0, .LBB122_4
.LBB122_13:
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
	beq r1, r0, .LBB122_16
.LBB122_14:
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
	bne r1, r0, .LBB122_14
	jal r0, .LBB122_16
.LBB122_15:
	fdiv.d r8, r4, r12
.LBB122_16:
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
.Lfunc_end122:
	.size	f77_dpow_i, .Lfunc_end122-f77_dpow_i
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rpow_i
	.type	.LCPI123_0,@object
.LCPI123_0:
	.word	0x3f800000                      # float 1
	.size	.LCPI123_0, 4
	.type	.LCPI123_1,@object
.LCPI123_1:
	.word	0x00000000                      # float 0
	.size	.LCPI123_1, 4
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0
	.type	.LCPI123_2,@object
.LCPI123_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI123_2, 8
	.type	.LCPI123_3,@object
.LCPI123_3:
	.quad	0x47eff933c78cdfad              # double 3.4E+38
	.size	.LCPI123_3, 8
	.type	.LCPI123_4,@object
.LCPI123_4:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI123_4, 8
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
	blt r4, r0, .LBB123_3
.LBB123_1:
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
	srli r4, r4, 1
	beq r4, r0, .LBB123_17
.LBB123_2:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	srli r4, r4, 1
	bne r4, r0, .LBB123_2
	jal r0, .LBB123_17
.LBB123_3:
	sub r4, r0, r4
	lui r1, %hi(.LCPI123_0)
	ldw r5, r1+%lo(.LCPI123_0)
	lui r6, %hi(.LCPI123_1)
	ldw r6, r6+%lo(.LCPI123_1)
	lui r7, %hi(.LCPI123_3)
	addi r7, r7, %lo(.LCPI123_3)
	ldw r9, r7+4
	ldw r8, r7+0
	lui r7, %hi(.LCPI123_2)
	addi r7, r7, %lo(.LCPI123_2)
	ldw r11, r7+4
	ldw r10, r7+0
	add r7, r3, r0
	add lr, r4, r0
	jal r0, .LBB123_5
.LBB123_4:
	fmul.s r7, r7, r7
.LBB123_5:
	andi r15, lr, 1
	fcvt.d.s r12, r7
	flt.s r14, r7, r6
	beq r15, r0, .LBB123_10
.LBB123_6:
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
	fdiv.d r20, r8, r16
	fle.d r15, r18, r20
	bne r15, r0, .LBB123_9
.LBB123_7:
	lui r15, %hi(.LCPI123_4)
	addi r15, r15, %lo(.LCPI123_4)
	ldw r21, r15+4
	ldw r20, r15+0
	fle.d r15, r16, r20
	bne r15, r0, .LBB123_9
.LBB123_8:
	fle.d r15, r16, r10
	xori r15, r15, 1
	fle.d r16, r18, r10
	xori r16, r16, 1
	or  r15, r16, r15
	bne r15, r0, .LBB123_13
.LBB123_9:
	fmul.s r5, r7, r5
.LBB123_10:
	srli lr, lr, 1
	beq lr, r0, .LBB123_16
.LBB123_11:
	fneg.d r16, r12
	xor r15, r17, r13
	sub r18, r0, r14
	and r14, r15, r18
	xor r15, r13, r14
	xor r16, r16, r12
	and r16, r16, r18
	xor r14, r12, r16
	fle.d r12, r14, r10
	bne r12, r0, .LBB123_4
.LBB123_12:
	fdiv.d r12, r8, r14
	fle.d r12, r14, r12
	xori r12, r12, 1
	beq r12, r0, .LBB123_4
.LBB123_13:
	ldw r1, r1+%lo(.LCPI123_0)
	fdiv.s r3, r1, r3
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
.LBB123_14:
	srli r4, r4, 1
	beq r4, r0, .LBB123_17
.LBB123_15:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	jal r0, .LBB123_14
.LBB123_16:
	ldw r1, r1+%lo(.LCPI123_0)
	fdiv.s r1, r1, r5
.LBB123_17:
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
.Lfunc_end123:
	.size	f77_rpow_i, .Lfunc_end123-f77_rpow_i
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
	bne r3, r0, .LBB124_4
.LBB124_1:
	addi r3, r0, -400
	lui lr, %hi(fio_uend+400)
	addi lr, lr, %lo(fio_uend+400)
	addi r12, r0, -1
.LBB124_2:
	add r13, r3, lr
	stw r13+0, r12
	addi r3, r3, 4
	bne r3, r0, .LBB124_2
.LBB124_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB124_4:
	ldw r14, sp+352
	addi r1, r11, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB124_21
.LBB124_5:
	addi r1, r11, -5
	addi r18, r0, 1
	bleu r1, r18, .LBB124_21
.LBB124_6:
	beq r8, r0, .LBB124_13
.LBB124_7:
	addi r1, r0, 1
	add r15, r0, r0
	blt r9, r1, .LBB124_14
.LBB124_8:
	addi r3, r0, 6
	lui lr, %hi(.L.str.28)
	addi lr, lr, %lo(.L.str.28)
	addi r12, r0, 26
	add r13, r0, r0
.LBB124_9:
	beq r13, r3, .LBB124_23
.LBB124_10:
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
	bne r16, r15, .LBB124_13
.LBB124_11:
	addi r13, r13, 1
	bne r9, r13, .LBB124_9
.LBB124_12:
	add lr, r9, r0
	beq lr, r3, .LBB124_24
.LBB124_13:
	add r15, r0, r0
.LBB124_14:
	add r16, r15, r0
	beq r10, r0, .LBB124_35
.LBB124_15:
	ldw r3, sp+336
	addi r1, r0, 1
	add r16, r15, r0
	blt r3, r1, .LBB124_35
.LBB124_16:
	addi r8, r0, 11
	lui r9, %hi(.L.str.29)
	addi r9, r9, %lo(.L.str.29)
	addi lr, r0, 26
	add r12, r0, r0
.LBB124_17:
	beq r12, r8, .LBB124_28
.LBB124_18:
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
	bne r16, r13, .LBB124_34
.LBB124_19:
	addi r12, r12, 1
	bne r3, r12, .LBB124_17
.LBB124_20:
	add r9, r3, r0
	beq r9, r8, .LBB124_29
	jal r0, .LBB124_34
.LBB124_21:
	bne r14, r0, .LBB124_44
.LBB124_22:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.27)
	addi r4, r4, %lo(.L.str.27)
	add r5, r11, r0
	jal r0, .LBB124_145
.LBB124_23:
	add lr, r3, r0
	bne lr, r3, .LBB124_13
.LBB124_24:
	addi r3, r0, 7
	add r15, r1, r0
	blt r9, r3, .LBB124_14
.LBB124_25:
	addi r9, r9, -7
	addi r1, r8, 6
	addi r3, r0, 32
	addi r8, r0, 1
.LBB124_26:
	ldbu lr, r1+0
	seq r15, lr, r3
	addi r12, r9, -1
	sltu r9, r12, r9
	bne r9, r8, .LBB124_14
.LBB124_27:
	addi r1, r1, 1
	add r9, r12, r0
	beq lr, r3, .LBB124_26
	jal r0, .LBB124_14
.LBB124_28:
	add r9, r8, r0
	bne r9, r8, .LBB124_34
.LBB124_29:
	addi r8, r0, 12
	blt r3, r8, .LBB124_33
.LBB124_30:
	addi r8, r10, 11
	addi r3, r3, -11
	addi r9, r0, 32
.LBB124_31:
	ldbu r10, r8+0
	bne r10, r9, .LBB124_34
.LBB124_32:
	addi r8, r8, 1
	addi r3, r3, -1
	bne r3, r0, .LBB124_31
.LBB124_33:
	add r16, r1, r0
	jal r0, .LBB124_35
.LBB124_34:
	add r16, r0, r0
.LBB124_35:
	ldw r17, sp+348
	addi r23, r0, 0
	sne r1, r6, r23
	sgt r3, r7, r23
	and r22, r1, r3
	bne r22, r18, .LBB124_41
.LBB124_36:
	addi r1, r0, 7
	lui r3, %hi(.L.str.30)
	addi r3, r3, %lo(.L.str.30)
	addi r8, r0, 26
	add r9, r0, r0
.LBB124_37:
	beq r9, r1, .LBB124_58
.LBB124_38:
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
	bne lr, r10, .LBB124_41
.LBB124_39:
	addi r9, r9, 1
	bne r7, r9, .LBB124_37
.LBB124_40:
	add r3, r7, r0
	beq r3, r1, .LBB124_59
.LBB124_41:
	add r24, r0, r0
.LBB124_42:
	sgt r1, r17, r23
	xori r3, r15, 1
	or  r1, r1, r3
	bne r1, r0, .LBB124_46
.LBB124_43:
	beq r14, r0, .LBB124_144
.LBB124_44:
	addi r1, r0, 2
.LBB124_45:
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
.LBB124_46:
	beq r4, r0, .LBB124_51
.LBB124_47:
	addi r19, r0, 1
	blt r5, r19, .LBB124_51
.LBB124_48:
	addi r1, r0, 256
	blt r5, r1, .LBB124_69
.LBB124_49:
	bne r14, r0, .LBB124_44
.LBB124_50:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
	jal r0, .LBB124_145
.LBB124_51:
	add r25, r6, r0
	add r26, r7, r0
	lui r5, %hi(.L.str.33)
	addi r5, r5, %lo(.L.str.33)
	addi r3, sp, 16
	addi r4, r0, 256
	add r6, r11, r0
	jal r31, snprintf
.LBB124_52:
	ldw r19, sp+344
	ldw r20, sp+340
	slli r13, r11, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r21, r13, r1
	ldw r1, r21+0
	beq r1, r0, .LBB124_55
.LBB124_53:
	slli r1, r11, 8
	lui r3, %hi(fio_ufname)
	addi r3, r3, %lo(fio_ufname)
	add r3, r1, r3
	addi r4, sp, 16
	jal r31, strcmp
	sne r1, r1, r23
	or  r1, r24, r1
	beq r1, r0, .LBB124_72
.LBB124_54:
	ldw r3, r21+0
	jal r31, fclose
	stw r21+0, r0
.LBB124_55:
	beq r24, r0, .LBB124_63
.LBB124_56:
	jal r31, tmpfile
	beq r1, r0, .LBB124_80
.LBB124_57:
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	jal r0, .LBB124_120
.LBB124_58:
	add r3, r1, r0
	bne r3, r1, .LBB124_41
.LBB124_59:
	addi r1, r0, 8
	blt r7, r1, .LBB124_79
.LBB124_60:
	addi r9, r7, -8
	addi r1, r6, 7
	addi r3, r0, 32
	addi r8, r0, 1
.LBB124_61:
	ldbu r10, r1+0
	seq r24, r10, r3
	addi lr, r9, -1
	sltu r9, lr, r9
	bne r9, r8, .LBB124_42
.LBB124_62:
	addi r1, r1, 1
	add r9, lr, r0
	beq r10, r3, .LBB124_61
	jal r0, .LBB124_42
.LBB124_63:
	beq r22, r0, .LBB124_97
.LBB124_64:
	addi r1, r0, 3
	lui r5, %hi(.L.str.36)
	addi r5, r5, %lo(.L.str.36)
	addi r6, r0, 26
	add r7, r0, r0
	add r4, r26, r0
	add r3, r25, r0
.LBB124_65:
	beq r7, r1, .LBB124_82
.LBB124_66:
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
	bne r9, r8, .LBB124_93
.LBB124_67:
	addi r7, r7, 1
	bne r4, r7, .LBB124_65
.LBB124_68:
	add r5, r4, r0
	jal r0, .LBB124_83
.LBB124_69:
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
.LBB124_70:
	add r5, r1, r12
	ldbu r6, r5+0
	bne r6, r3, .LBB124_52
.LBB124_71:
	add r4, r12, r0
	addi r12, r12, -1
	stb r5+0, r0
	bgt r4, r19, .LBB124_70
	jal r0, .LBB124_52
.LBB124_72:
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
	beq r20, r0, .LBB124_136
.LBB124_73:
	add r3, r0, r0
	blt r19, r18, .LBB124_137
.LBB124_74:
	addi r1, r0, 4
	lui r3, %hi(.L.str.34)
	addi r3, r3, %lo(.L.str.34)
	addi r4, r0, 26
	add r5, r0, r0
.LBB124_75:
	beq r5, r1, .LBB124_104
.LBB124_76:
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
	bne r7, r6, .LBB124_136
.LBB124_77:
	addi r5, r5, 1
	bne r19, r5, .LBB124_75
.LBB124_78:
	add r3, r19, r0
	jal r0, .LBB124_105
.LBB124_79:
	addi r24, r0, 1
	jal r0, .LBB124_42
.LBB124_80:
	bne r14, r0, .LBB124_44
.LBB124_81:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	jal r0, .LBB124_145
.LBB124_82:
	add r5, r1, r0
.LBB124_83:
	bne r5, r1, .LBB124_93
.LBB124_84:
	addi r1, r0, 4
	blt r4, r1, .LBB124_88
.LBB124_85:
	addi r1, r3, 3
	addi r5, r4, -3
	addi r6, r0, 32
.LBB124_86:
	ldbu r7, r1+0
	bne r7, r6, .LBB124_93
.LBB124_87:
	addi r1, r1, 1
	addi r5, r5, -1
	bne r5, r0, .LBB124_86
.LBB124_88:
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	addi r3, sp, 16
	jal r31, fopen
	bne r1, r0, .LBB124_90
.LBB124_89:
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	addi r3, sp, 16
	jal r31, fopen
.LBB124_90:
	addi r4, sp, 16
	bne r1, r0, .LBB124_120
.LBB124_91:
	bne r14, r0, .LBB124_44
.LBB124_92:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.39)
	addi r4, r4, %lo(.L.str.39)
	jal r0, .LBB124_140
.LBB124_93:
	lui r5, %hi(.L.str.40)
	addi r5, r5, %lo(.L.str.40)
	add r25, r3, r0
	add r26, r4, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB124_97
.LBB124_94:
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	addi r3, sp, 16
	jal r31, fopen
	beq r1, r0, .LBB124_141
.LBB124_95:
	add r3, r1, r0
	jal r31, fclose
	bne r14, r0, .LBB124_44
.LBB124_96:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	jal r0, .LBB124_140
.LBB124_97:
	beq r25, r0, .LBB124_117
.LBB124_98:
	addi r1, r0, 1
	blt r26, r1, .LBB124_117
.LBB124_99:
	addi r1, r0, 7
	lui r3, %hi(.L.str.44)
	addi r3, r3, %lo(.L.str.44)
	addi r4, r0, 26
	add r5, r0, r0
.LBB124_100:
	beq r5, r1, .LBB124_111
.LBB124_101:
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
	bne r7, r6, .LBB124_127
.LBB124_102:
	addi r5, r5, 1
	bne r26, r5, .LBB124_100
.LBB124_103:
	add r3, r26, r0
	jal r0, .LBB124_112
.LBB124_104:
	add r3, r1, r0
.LBB124_105:
	bne r3, r1, .LBB124_136
.LBB124_106:
	addi r1, r0, 5
	blt r19, r1, .LBB124_110
.LBB124_107:
	addi r1, r20, 4
	addi r3, r19, -4
	addi r4, r0, 32
.LBB124_108:
	ldbu r5, r1+0
	bne r5, r4, .LBB124_136
.LBB124_109:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB124_108
.LBB124_110:
	add r3, r18, r0
	jal r0, .LBB124_137
.LBB124_111:
	add r3, r1, r0
.LBB124_112:
	bne r3, r1, .LBB124_127
.LBB124_113:
	addi r1, r0, 8
	blt r26, r1, .LBB124_117
.LBB124_114:
	addi r1, r25, 7
	addi r3, r26, -7
	addi r4, r0, 32
.LBB124_115:
	ldbu r5, r1+0
	bne r5, r4, .LBB124_127
.LBB124_116:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB124_115
.LBB124_117:
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	addi r3, sp, 16
	jal r31, fopen
	bne r1, r0, .LBB124_119
.LBB124_118:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r3, sp, 16
	jal r31, fopen
.LBB124_119:
	addi r4, sp, 16
	beq r1, r0, .LBB124_138
.LBB124_120:
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
	beq r20, r0, .LBB124_136
.LBB124_121:
	addi r1, r0, 1
	add r3, r0, r0
	blt r19, r1, .LBB124_137
.LBB124_122:
	addi r3, r0, 4
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
	addi r5, r0, 26
	add r6, r0, r0
.LBB124_123:
	beq r6, r3, .LBB124_129
.LBB124_124:
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
	bne r8, r7, .LBB124_136
.LBB124_125:
	addi r6, r6, 1
	bne r19, r6, .LBB124_123
.LBB124_126:
	add r4, r19, r0
	jal r0, .LBB124_130
.LBB124_127:
	bne r14, r0, .LBB124_44
.LBB124_128:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.46)
	addi r4, r4, %lo(.L.str.46)
	jal r0, .LBB124_145
.LBB124_129:
	add r4, r3, r0
.LBB124_130:
	bne r4, r3, .LBB124_136
.LBB124_131:
	addi r3, r0, 5
	blt r19, r3, .LBB124_135
.LBB124_132:
	addi r3, r20, 4
	addi r4, r19, -4
	addi r5, r0, 32
.LBB124_133:
	ldbu r6, r3+0
	bne r6, r5, .LBB124_136
.LBB124_134:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB124_133
.LBB124_135:
	add r3, r1, r0
	jal r0, .LBB124_137
.LBB124_136:
	add r3, r0, r0
.LBB124_137:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r13, r1
	stw r1+0, r3
	add r1, r0, r0
	jal r0, .LBB124_45
.LBB124_138:
	bne r14, r0, .LBB124_44
.LBB124_139:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
.LBB124_140:
	addi r5, sp, 16
	jal r0, .LBB124_145
.LBB124_141:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r12, sp, 16
	add r3, r12, r0
	jal r31, fopen
	add r4, r12, r0
	bne r1, r0, .LBB124_120
.LBB124_142:
	bne r14, r0, .LBB124_44
.LBB124_143:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.43)
	addi r4, r4, %lo(.L.str.43)
	jal r0, .LBB124_140
.LBB124_144:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
.LBB124_145:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end124:
	.size	f77_open, .Lfunc_end124-f77_open
                                        # -- End function
	.p2align	2                               # -- Begin function fio_str_is
	.type	fio_str_is,@function
fio_str_is:                             # @fio_str_is
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	addi r1, r0, 1
	blt r4, r1, .LBB125_6
.LBB125_1:
	addi r7, r0, 26
	add r6, r0, r0
.LBB125_2:
	add r8, r5, r6
	ldb r8, r8+0
	beq r8, r0, .LBB125_7
.LBB125_3:
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
	bne r9, r8, .LBB125_8
.LBB125_4:
	addi r6, r6, 1
	bne r4, r6, .LBB125_2
.LBB125_5:
	add r6, r4, r0
	jal r0, .LBB125_7
.LBB125_6:
	add r6, r0, r0
.LBB125_7:
	add r5, r5, r6
	ldbu r5, r5+0
	beq r5, r0, .LBB125_10
.LBB125_8:
	add r1, r0, r0
.LBB125_9:
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.LBB125_10:
	ble r4, r6, .LBB125_9
.LBB125_11:
	add r3, r3, r6
	sub r4, r4, r6
	addi r5, r0, 32
.LBB125_12:
	ldbu r6, r3+0
	bne r6, r5, .LBB125_8
.LBB125_13:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB125_12
	jal r0, .LBB125_9
.Lfunc_end125:
	.size	fio_str_is, .Lfunc_end125-fio_str_is
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
	bgtu r1, r3, .LBB126_27
.LBB126_1:
	slli r11, r1, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r12, r11, r3
	ldw r3, r12+0
	beq r3, r0, .LBB126_27
.LBB126_2:
	addi r7, r0, 1
	beq r4, r0, .LBB126_10
.LBB126_3:
	addi r6, r0, 1
	add r13, r0, r0
	blt r5, r6, .LBB126_11
.LBB126_4:
	addi r7, r0, 6
	lui r8, %hi(.L.str.47)
	addi r8, r8, %lo(.L.str.47)
	addi r9, r0, 26
	add r10, r0, r0
.LBB126_5:
	beq r10, r7, .LBB126_28
.LBB126_6:
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
	bne r13, lr, .LBB126_9
.LBB126_7:
	addi r10, r10, 1
	bne r5, r10, .LBB126_5
.LBB126_8:
	add r8, r5, r0
	beq r8, r7, .LBB126_29
.LBB126_9:
	add r7, r0, r0
.LBB126_10:
	add r13, r0, r0
.LBB126_11:
	add r14, r1, r0
	bne r7, r0, .LBB126_24
.LBB126_12:
	addi r1, r0, 1
	blt r5, r1, .LBB126_34
.LBB126_13:
	addi r1, r0, 4
	lui r6, %hi(.L.str.48)
	addi r6, r6, %lo(.L.str.48)
	addi r7, r0, 26
	add r8, r0, r0
.LBB126_14:
	beq r8, r1, .LBB126_18
.LBB126_15:
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
	bne r10, r9, .LBB126_34
.LBB126_16:
	addi r8, r8, 1
	bne r5, r8, .LBB126_14
.LBB126_17:
	add r6, r5, r0
	jal r0, .LBB126_19
.LBB126_18:
	add r6, r1, r0
.LBB126_19:
	bne r6, r1, .LBB126_34
.LBB126_20:
	addi r1, r0, 5
	blt r5, r1, .LBB126_24
.LBB126_21:
	addi r1, r4, 4
	addi r4, r5, -4
	addi r5, r0, 32
.LBB126_22:
	ldbu r6, r1+0
	bne r6, r5, .LBB126_34
.LBB126_23:
	addi r1, r1, 1
	addi r4, r4, -1
	bne r4, r0, .LBB126_22
.LBB126_24:
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
	beq r13, r0, .LBB126_26
.LBB126_25:
	add r3, r11, r12
	jal r31, remove
.LBB126_26:
	add r1, r11, r12
	stb r1+0, r0
.LBB126_27:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB126_28:
	add r8, r7, r0
	bne r8, r7, .LBB126_9
.LBB126_29:
	addi r8, r0, 7
	add r7, r6, r0
	add r13, r6, r0
	blt r5, r8, .LBB126_11
.LBB126_30:
	addi r10, r5, -7
	addi r6, r4, 6
	addi r8, r0, 32
	addi r9, r0, 1
.LBB126_31:
	ldbu lr, r6+0
	seq r7, lr, r8
	addi r13, r10, -1
	sltu r10, r13, r10
	bne r10, r9, .LBB126_33
.LBB126_32:
	addi r6, r6, 1
	add r10, r13, r0
	add r13, r7, r0
	beq lr, r8, .LBB126_31
	jal r0, .LBB126_11
.LBB126_33:
	add r13, r7, r0
	jal r0, .LBB126_11
.LBB126_34:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.49)
	addi r4, r4, %lo(.L.str.49)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end126:
	.size	f77_close, .Lfunc_end126-f77_close
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
	bne r3, r0, .LBB127_4
.LBB127_1:
	addi r3, r0, -400
	lui r6, %hi(fio_uend+400)
	addi r6, r6, %lo(fio_uend+400)
	addi r7, r0, -1
.LBB127_2:
	add r8, r3, r6
	stw r8+0, r7
	addi r3, r3, 4
	bne r3, r0, .LBB127_2
.LBB127_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB127_4:
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
	beq r4, r0, .LBB127_21
.LBB127_5:
	addi r22, r0, 1
	blt r5, r22, .LBB127_21
.LBB127_6:
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
.LBB127_7:
	addi r4, sp, 28
	add r4, r4, r3
	ldbu r5, r4+0
	bne r5, r1, .LBB127_9
.LBB127_8:
	stb r4+0, r0
	addi r4, r3, -1
	addi r5, r3, 1
	add r3, r4, r0
	bgt r5, r22, .LBB127_7
.LBB127_9:
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	addi r12, sp, 28
	add r4, r12, r0
	jal r31, strcpy
	stw r21+%lo(inq_named), r22
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	add r3, r12, r0
	jal r31, fopen
	beq r1, r0, .LBB127_11
.LBB127_10:
	stb r23+%lo(inq_exist), r22
	add r3, r1, r0
	jal r31, fclose
.LBB127_11:
	lui r12, %hi(fio_ufname+256)
	addi r12, r12, %lo(fio_ufname+256)
	lui r13, %hi(fio_ufile+4)
	addi r13, r13, %lo(fio_ufile+4)
	addi r21, r0, 100
	jal r0, .LBB127_13
.LBB127_12:
	addi r22, r22, 1
	addi r12, r12, 256
	addi r13, r13, 4
	beq r22, r21, .LBB127_16
.LBB127_13:
	ldw r1, r13+0
	beq r1, r0, .LBB127_12
.LBB127_14:
	addi r4, sp, 28
	add r3, r12, r0
	jal r31, strcmp
	bne r1, r0, .LBB127_12
.LBB127_15:
	addi r1, r0, 1
	stb r19+%lo(inq_opened), r1
	stw r20+%lo(inq_unit), r22
.LBB127_16:
	ldbu r1, r19+%lo(inq_opened)
	beq r1, r0, .LBB127_26
.LBB127_17:
	ldw r3, r20+%lo(inq_unit)
	xor r3, r3, r11
	sub r1, r0, r1
	and r1, r3, r1
	xor r11, r11, r1
.LBB127_18:
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
	beq r4, r0, .LBB127_26
.LBB127_19:
	addi r4, r0, 1
	blt r3, r4, .LBB127_26
.LBB127_20:
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
	jal r0, .LBB127_26
.LBB127_21:
	addi r1, r0, 100
	bltu r11, r1, .LBB127_23
.LBB127_22:
	stb r23+%lo(inq_exist), r0
	jal r0, .LBB127_26
.LBB127_23:
	addi r1, r0, 1
	stb r23+%lo(inq_exist), r1
	stw r20+%lo(inq_unit), r11
	addi r3, r0, 6
	bgtu r11, r3, .LBB127_27
.LBB127_24:
	sll r4, r1, r11
	andi r4, r4, 97
	beq r4, r0, .LBB127_27
.LBB127_25:
	seq r3, r11, r3
	addi r4, r0, 5
	seq r4, r11, r4
	stb r19+%lo(inq_opened), r1
	lui r1, %hi(.L.str.52)
	addi r1, r1, %lo(.L.str.52)
	lui r5, %hi(.L.str.51)
	addi r5, r5, %lo(.L.str.51)
	xor r5, r5, r1
	sub r3, r0, r3
	and r3, r5, r3
	xor r1, r1, r3
	lui r3, %hi(.L.str.50)
	addi r3, r3, %lo(.L.str.50)
	xor r3, r3, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r4, r1, r3
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	jal r31, strcpy
	stw r21+%lo(inq_named), r0
.LBB127_26:
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
.LBB127_27:
	slli r3, r11, 2
	lui r4, %hi(fio_ufile)
	addi r4, r4, %lo(fio_ufile)
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r0, .LBB127_29
.LBB127_28:
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
	jal r0, .LBB127_18
.LBB127_29:
	stb r19+%lo(inq_opened), r0
	jal r0, .LBB127_26
.Lfunc_end127:
	.size	f77_inquire, .Lfunc_end127-f77_inquire
                                        # -- End function
	.globl	f77_inq                         # -- Begin function f77_inq
	.p2align	2
	.type	f77_inq,@function
f77_inq:                                # @f77_inq
# %bb.0:
	addi r1, r3, -1
	addi r3, r0, 13
	bgtu r1, r3, .LBB128_31
.LBB128_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r12, r5, r0
	add r11, r4, r0
	slli r1, r1, 2
	lui r3, %hi(.LJTI128_0)
	addi r3, r3, %lo(.LJTI128_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB128_2:
	lui r1, %hi(inq_exist)
	ldbu r1, r1+%lo(inq_exist)
	jal r0, .LBB128_29
.LBB128_3:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB128_30
.LBB128_4:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB128_30
.LBB128_5:
	lui r1, %hi(inq_recl)
	ldw r1, r1+%lo(inq_recl)
	jal r0, .LBB128_29
.LBB128_6:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r14, r0, 1
	bne r1, r14, .LBB128_30
.LBB128_7:
	lui r1, %hi(inq_named)
	ldw r1, r1+%lo(inq_named)
	beq r1, r0, .LBB128_30
.LBB128_8:
	lui r13, %hi(inq_name)
	addi r13, r13, %lo(inq_name)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB128_30
.LBB128_9:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB128_11
.LBB128_10:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB128_30
.LBB128_11:
	add r5, r3, r0
	bge r4, r1, .LBB128_10
.LBB128_12:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB128_10
.LBB128_13:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB128_30
.LBB128_14:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB128_30
.LBB128_15:
	lui r1, %hi(inq_nextrec)
	ldw r1, r1+%lo(inq_nextrec)
	jal r0, .LBB128_29
.LBB128_16:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	lui r5, %hi(.L.str.56)
	addi r5, r5, %lo(.L.str.56)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB128_30
.LBB128_17:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB128_19
.LBB128_18:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB128_30
.LBB128_19:
	add r5, r3, r0
	bge r4, r1, .LBB128_18
.LBB128_20:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB128_18
.LBB128_21:
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
	jal r0, .LBB128_29
.LBB128_22:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_named)
	ldw r3, r3+%lo(inq_named)
	sub r1, r0, r1
	and r1, r3, r1
	jal r0, .LBB128_29
.LBB128_23:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	lui r5, %hi(.L.str.55)
	addi r5, r5, %lo(.L.str.55)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB128_30
.LBB128_24:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB128_26
.LBB128_25:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB128_30
.LBB128_26:
	add r5, r3, r0
	bge r4, r1, .LBB128_25
.LBB128_27:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB128_25
.LBB128_28:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
.LBB128_29:
	stw r11+0, r1
.LBB128_30:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB128_31:
	jalr r0, r31, 0
.LBB128_32:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	lui r5, %hi(.L.str.54)
	addi r5, r5, %lo(.L.str.54)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB128_30
.LBB128_33:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB128_35
.LBB128_34:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB128_30
.LBB128_35:
	add r5, r3, r0
	bge r4, r1, .LBB128_34
.LBB128_36:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB128_34
.LBB128_37:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r5, %hi(inq_bz)
	ldw r5, r5+%lo(inq_bz)
	seq r4, r5, r4
	lui r5, %hi(.L.str.34)
	addi r5, r5, %lo(.L.str.34)
	lui r6, %hi(.L.str.58)
	addi r6, r6, %lo(.L.str.58)
	xor r6, r6, r5
	sub r4, r0, r4
	and r4, r6, r4
	xor r4, r5, r4
	lui r5, %hi(.L.str.53)
	addi r5, r5, %lo(.L.str.53)
	xor r4, r4, r5
	sub r3, r0, r3
	and r3, r4, r3
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r5, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB128_30
.LBB128_38:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB128_40
.LBB128_39:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB128_30
.LBB128_40:
	add r5, r3, r0
	bge r4, r1, .LBB128_39
.LBB128_41:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB128_39
.LBB128_42:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	lui r5, %hi(.L.str.56)
	addi r5, r5, %lo(.L.str.56)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB128_30
.LBB128_43:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB128_45
.LBB128_44:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB128_30
.LBB128_45:
	add r5, r3, r0
	bge r4, r1, .LBB128_44
.LBB128_46:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB128_44
.LBB128_47:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	lui r5, %hi(.L.str.55)
	addi r5, r5, %lo(.L.str.55)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB128_30
.LBB128_48:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB128_50
.LBB128_49:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB128_30
.LBB128_50:
	add r5, r3, r0
	bge r4, r1, .LBB128_49
.LBB128_51:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB128_49
.LBB128_52:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	lui r5, %hi(.L.str.57)
	addi r5, r5, %lo(.L.str.57)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB128_30
.LBB128_53:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB128_55
.LBB128_54:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB128_30
.LBB128_55:
	add r5, r3, r0
	bge r4, r1, .LBB128_54
.LBB128_56:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB128_54
.Lfunc_end128:
	.size	f77_inq, .Lfunc_end128-f77_inq
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI128_0,@object
.LJTI128_0:
	.word	.LBB128_2
	.word	.LBB128_28
	.word	.LBB128_21
	.word	.LBB128_22
	.word	.LBB128_6
	.word	.LBB128_32
	.word	.LBB128_42
	.word	.LBB128_23
	.word	.LBB128_52
	.word	.LBB128_16
	.word	.LBB128_47
	.word	.LBB128_3
	.word	.LBB128_13
	.word	.LBB128_37
	.size	.LJTI128_0, 56
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
	bne r3, r0, .LBB129_4
.LBB129_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB129_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB129_2
.LBB129_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB129_4:
	addi r1, r11, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB129_7
.LBB129_5:
	addi r1, r11, -5
	addi r3, r0, 1
	bleu r1, r3, .LBB129_7
.LBB129_6:
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
.LBB129_7:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.59)
	addi r4, r4, %lo(.L.str.59)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end129:
	.size	f77_rewind, .Lfunc_end129-f77_rewind
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
	beq r3, r16, .LBB130_187
.LBB130_1:
	add r13, r4, r0
	add r12, r3, r0
	lui lr, %hi(fio_ftext)
	addi r11, r0, -1
	stw lr+%lo(fio_ftext), r11
	addi r20, r0, 32
	add r1, r0, r0
	jal r0, .LBB130_3
.LBB130_2:
	add r1, r12, r28
	ldbu r3, r1+0
	addi r1, r28, 1
	bne r3, r20, .LBB130_5
.LBB130_3:
	add r28, r1, r0
	blt r13, r0, .LBB130_2
.LBB130_4:
	bltu r28, r13, .LBB130_2
.LBB130_5:
	lui r14, %hi(.L.str.68)
	addi r14, r14, %lo(.L.str.68)
	lui r15, %hi(fio_fmt_why)
	blt r13, r0, .LBB130_7
.LBB130_6:
	bgeu r28, r13, .LBB130_189
.LBB130_7:
	add r1, r12, r28
	ldbu r1, r1+0
	addi r3, r0, 40
	stw sp+36, r3
	bne r1, r3, .LBB130_189
.LBB130_8:
	addi r23, r0, 1022
	lui r19, %hi(.L.str.69)
	addi r19, r19, %lo(.L.str.69)
	addi r1, r0, 1
	stw sp+32, r1
	slli r1, r1, 2
	lui r24, %hi(.LJTI130_1)
	addi r24, r24, %lo(.LJTI130_1)
	add r22, r24, r1
	addi r26, r0, 39
	addi r9, r0, 246
	addi r10, r0, 41
	addi r25, r0, 0
	add r21, r0, r0
	add fp, r0, r0
	lui r18, %hi(fio_fbuf+1)
	addi r18, r18, %lo(fio_fbuf+1)
.LBB130_9:
	blt r13, r0, .LBB130_13
.LBB130_10:
	add r17, r0, r0
	bltu r28, r13, .LBB130_13
.LBB130_11:
	blt r21, r23, .LBB130_14
.LBB130_12:
	stw r15+%lo(fio_fmt_why), r19
	ldw r1, r22+0
	jalr r0, r1, 0
.LBB130_13:
	add r1, r12, r28
	ldbu r17, r1+0
	bge r21, r23, .LBB130_12
.LBB130_14:
	andi r27, r17, 255
	bgtu r27, r26, .LBB130_38
.LBB130_15:
	slli r1, r27, 2
	lui r3, %hi(.LJTI130_0)
	addi r3, r3, %lo(.LJTI130_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB130_16:
	add r1, r21, r0
	add r5, r26, r0
	jal r0, .LBB130_18
.LBB130_17:
	ldbu r5, r4+0
	add r1, r21, r0
.LBB130_18:
	add r3, r28, r0
.LBB130_19:
	add r4, r1, r16
	stb r4+0, r5
	addi r28, r3, 1
	blt r13, r0, .LBB130_21
.LBB130_20:
	add r4, r0, r0
	bgeu r28, r13, .LBB130_22
.LBB130_21:
	add r4, r12, r28
	ldbu r4, r4+0
.LBB130_22:
	addi r21, r1, 1
	andi r4, r4, 255
	beq r4, r0, .LBB130_35
.LBB130_23:
	addi r5, r0, 1020
	bge r1, r5, .LBB130_35
.LBB130_24:
	bne r4, r27, .LBB130_29
.LBB130_25:
	addi r28, r3, 2
	blt r13, r0, .LBB130_27
.LBB130_26:
	add r3, r0, r0
	bgeu r28, r13, .LBB130_28
.LBB130_27:
	add r3, r12, r28
	ldbu r3, r3+0
.LBB130_28:
	andi r3, r3, 255
	bne r3, r27, .LBB130_48
.LBB130_29:
	add r4, r12, r28
	blt r13, r0, .LBB130_31
.LBB130_30:
	bgeu r28, r13, .LBB130_33
.LBB130_31:
	ldbu r5, r4+0
	addi r3, r0, 39
	bne r5, r3, .LBB130_33
.LBB130_32:
	addi r21, r1, 2
	add r1, r1, r18
	stb r1+0, r3
.LBB130_33:
	blt r13, r0, .LBB130_17
.LBB130_34:
	add r1, r21, r0
	add r5, r0, r0
	add r3, r28, r0
	bgeu r28, r13, .LBB130_19
	jal r0, .LBB130_17
.LBB130_35:
	lui r1, %hi(.L.str.71)
	addi r1, r1, %lo(.L.str.71)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB130_36:
	addi r28, r28, 1
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB130_37:
	lui r1, %hi(.L.str.70)
	addi r1, r1, %lo(.L.str.70)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB130_38:
	addi r1, r17, -58
	andi r1, r1, 255
	bltu r1, r9, .LBB130_51
.LBB130_39:
	beq r21, r0, .LBB130_49
.LBB130_40:
	ldw r3, lr+%lo(fio_ftext)
	add r1, r0, r0
	beq r21, r3, .LBB130_50
.LBB130_41:
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
	blt r21, r3, .LBB130_43
.LBB130_42:
	lui r1, %hi(fio_fbuf-2)
	addi r1, r1, %lo(fio_fbuf-2)
	add r1, r21, r1
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 66
	sne r1, r1, r3
.LBB130_43:
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
	beq r14, r3, .LBB130_50
.LBB130_44:
	addi r1, r0, -4
	and r3, r14, r1
	addi r1, r0, 1
	addi r4, r0, 68
	beq r3, r4, .LBB130_50
.LBB130_45:
	addi r3, r14, -65
	addi r4, r0, 19
	bgtu r3, r4, .LBB130_47
.LBB130_46:
	lui r4, 165
	addi r4, r4, -1791
	srl r3, r4, r3
	andi r3, r3, 1
	bne r3, r0, .LBB130_50
.LBB130_47:
	addi r1, r0, 46
	seq r1, r14, r1
	jal r0, .LBB130_50
.LBB130_48:
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
.LBB130_49:
	add r1, r0, r0
.LBB130_50:
	beq r1, r0, .LBB130_55
.LBB130_51:
	addi r1, r21, 1
	add r3, r21, r16
	stb r3+0, r17
	addi r28, r28, 1
	ldw r3, sp+36
	seq r3, r27, r3
	add fp, fp, r3
	bne r27, r10, .LBB130_53
.LBB130_52:
	addi fp, fp, -1
	seq r3, fp, r25
	sub r3, r0, r3
	andi r5, r3, 4
	add r21, r1, r0
	jal r0, .LBB130_54
.LBB130_53:
	add r21, r1, r0
	add r5, r0, r0
.LBB130_54:
	slli r1, r5, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB130_55:
	add r1, r28, r0
	add r4, r0, r0
	jal r0, .LBB130_59
.LBB130_56:
	ldbu r6, r3+0
.LBB130_57:
	addi r3, r0, 10
	mul r3, r4, r3
	add r3, r3, r5
	addi r4, r3, -48
	addi r3, r21, 1
	add r5, r21, r16
	stb r5+0, r6
	add r21, r3, r0
.LBB130_58:
	addi r1, r1, 1
.LBB130_59:
	add r3, r12, r1
	blt r13, r0, .LBB130_61
.LBB130_60:
	bgeu r1, r13, .LBB130_62
.LBB130_61:
	ldbu r5, r3+0
	addi r5, r5, -58
	andi r5, r5, 255
	addi r6, r0, 245
	bgtu r5, r6, .LBB130_65
.LBB130_62:
	blt r13, r0, .LBB130_64
.LBB130_63:
	bgeu r1, r13, .LBB130_74
.LBB130_64:
	ldbu r5, r3+0
	bne r5, r20, .LBB130_74
.LBB130_65:
	blt r13, r0, .LBB130_67
.LBB130_66:
	bgeu r1, r13, .LBB130_68
.LBB130_67:
	ldbu r5, r3+0
	beq r5, r20, .LBB130_58
.LBB130_68:
	bge r21, r23, .LBB130_84
.LBB130_69:
	blt r13, r0, .LBB130_71
.LBB130_70:
	add r5, r0, r0
	bgeu r1, r13, .LBB130_72
.LBB130_71:
	ldbu r5, r3+0
.LBB130_72:
	blt r13, r0, .LBB130_56
.LBB130_73:
	add r6, r0, r0
	bgeu r1, r13, .LBB130_57
	jal r0, .LBB130_56
.LBB130_74:
	blt r13, r0, .LBB130_76
.LBB130_75:
	bgeu r1, r13, .LBB130_77
.LBB130_76:
	ldbu r5, r3+0
	addi r6, r0, 72
	beq r5, r6, .LBB130_80
.LBB130_77:
	addi r5, r0, 5
	blt r13, r0, .LBB130_79
.LBB130_78:
	bgeu r1, r13, .LBB130_83
.LBB130_79:
	ldbu r6, r3+0
	addi r7, r0, 104
	bne r6, r7, .LBB130_83
.LBB130_80:
	beq r4, r0, .LBB130_85
.LBB130_81:
	add r5, r4, r21
	addi r6, r0, 1022
	blt r5, r6, .LBB130_86
.LBB130_82:
	lui r3, %hi(.L.str.72)
	addi r3, r3, %lo(.L.str.72)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB130_83:
	add r28, r1, r0
	jal r0, .LBB130_54
.LBB130_84:
	lui r3, %hi(.L.str.72)
	addi r3, r3, %lo(.L.str.72)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	add r21, r23, r0
	jalr r0, r3, 0
.LBB130_85:
	lui r3, %hi(.L.str.73)
	addi r3, r3, %lo(.L.str.73)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB130_86:
	blt r13, r0, .LBB130_88
.LBB130_87:
	add r5, r0, r0
	bgeu r1, r13, .LBB130_89
.LBB130_88:
	ldbu r5, r3+0
.LBB130_89:
	add r6, r21, r16
	stb r6+0, r5
	addi r21, r21, 1
	addi r5, r0, 1
	blt r4, r5, .LBB130_99
.LBB130_90:
	addi r6, r4, 1
	add r4, r0, r0
	jal r0, .LBB130_93
.LBB130_91:
	ldbu r8, r7+1
.LBB130_92:
	add r7, r21, r4
	add r7, r7, r16
	stb r7+0, r8
	addi r6, r6, -1
	addi r4, r4, 1
	ble r6, r5, .LBB130_100
.LBB130_93:
	add r7, r1, r4
	addi r28, r7, 1
	blt r13, r0, .LBB130_95
.LBB130_94:
	bgeu r28, r13, .LBB130_98
.LBB130_95:
	add r7, r3, r4
	ldbu r8, r7+1
	beq r8, r0, .LBB130_98
.LBB130_96:
	blt r13, r0, .LBB130_91
.LBB130_97:
	add r8, r0, r0
	bgeu r28, r13, .LBB130_92
	jal r0, .LBB130_91
.LBB130_98:
	lui r1, %hi(.L.str.74)
	addi r1, r1, %lo(.L.str.74)
	stw r15+%lo(fio_fmt_why), r1
	add r21, r21, r4
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB130_99:
	addi r28, r1, 1
	jal r0, .LBB130_101
.LBB130_100:
	add r1, r1, r4
	addi r28, r1, 1
	add r21, r21, r4
.LBB130_101:
	stw lr+%lo(fio_ftext), r21
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB130_102:
	add r1, r21, r16
	stb r1+0, r0
	addi r14, r0, 1
	addi r19, r0, 58
	addi r24, r0, -1
	addi r21, r0, -10
	addi r22, r0, 9
	addi r23, r0, 10
	addi r25, r0, 72
	lui r17, %hi(.L.str.76)
	addi r17, r17, %lo(.L.str.76)
	addi r26, r0, 80
	addi r27, r0, 88
	addi r28, r0, 25
	addi r1, r0, -4
	stw sp+28, r1
	addi r1, r0, 68
	stw sp+32, r1
	addi r1, r0, 46
	stw sp+24, r1
	jal r0, .LBB130_104
.LBB130_103:
	addi r14, r14, 1
.LBB130_104:
	add r1, r14, r16
	ldbu r1, r1+0
	bgtu r1, r19, .LBB130_116
.LBB130_105:
	slli r3, r1, 2
	lui r4, %hi(.LJTI130_2)
	addi r4, r4, %lo(.LJTI130_2)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB130_106:
	addi fp, r14, 1
	add r1, r14, r18
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB130_112
.LBB130_107:
	addi r20, r0, -1
	jal r0, .LBB130_114
.LBB130_108:
	addi r14, r14, 1
.LBB130_109:
	add r1, r14, r0
	add r3, r14, r16
	ldbu r4, r3+0
	addi r14, r14, 1
	addi r3, r0, 39
	bne r4, r3, .LBB130_109
.LBB130_110:
	add r4, r14, r16
	ldbu r4, r4+0
	bne r4, r3, .LBB130_104
.LBB130_111:
	addi r14, r1, 2
	jal r0, .LBB130_109
.LBB130_112:
	add r20, r0, r0
	addi r1, r1, -48
	bgtu r1, r22, .LBB130_114
.LBB130_113:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB130_113
.LBB130_114:
	lui r14, %hi(.L.str.75)
	addi r14, r14, %lo(.L.str.75)
	blt r20, r0, .LBB130_189
.LBB130_115:
	add r1, fp, r16
	ldbu r3, r1+0
	jal r31, toupper
	beq r1, r26, .LBB130_122
	jal r0, .LBB130_189
.LBB130_116:
	slli r1, r1, 24
	srai r1, r1, 24
	addi r3, r1, -58
	bgeu r3, r21, .LBB130_118
.LBB130_117:
	add fp, r14, r0
	add r20, r24, r0
	jal r0, .LBB130_122
.LBB130_118:
	addi r1, r1, -48
	bleu r1, r22, .LBB130_120
.LBB130_119:
	add fp, r14, r0
	add r20, r0, r0
	jal r0, .LBB130_122
.LBB130_120:
	add r20, r0, r0
	add fp, r14, r0
.LBB130_121:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB130_121
.LBB130_122:
	add r14, fp, r16
	ldbu r3, r14+0
	jal r31, toupper
	bne r1, r25, .LBB130_125
.LBB130_123:
	blt r20, r0, .LBB130_188
.LBB130_124:
	add r1, fp, r20
	addi r14, r1, 1
	jal r0, .LBB130_104
.LBB130_125:
	blt r20, r0, .LBB130_128
.LBB130_126:
	ldw r3, sp+36
	bne r1, r3, .LBB130_128
.LBB130_127:
	addi r14, fp, 1
	jal r0, .LBB130_104
.LBB130_128:
	beq r1, r27, .LBB130_135
.LBB130_129:
	sgt r3, r20, r24
	seq r4, r1, r26
	and r3, r3, r4
	bne r3, r0, .LBB130_135
.LBB130_130:
	addi r3, r1, -65
	bgtu r3, r28, .LBB130_139
.LBB130_131:
	slli r3, r3, 2
	lui r4, %hi(.LJTI130_3)
	addi r4, r4, %lo(.LJTI130_3)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB130_132:
	addi r14, fp, 1
	add r1, fp, r18
.LBB130_133:
	ldb r1, r1+0
	addi r1, r1, -58
	bltu r1, r21, .LBB130_104
.LBB130_134:
	add r1, r14, r0
	addi r14, r14, 1
	add r1, r1, r18
	jal r0, .LBB130_133
.LBB130_135:
	addi r14, fp, 1
	jal r0, .LBB130_104
.LBB130_136:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 82
	beq r1, r3, .LBB130_138
.LBB130_137:
	addi r3, r0, 76
	bne r1, r3, .LBB130_159
.LBB130_138:
	addi r14, fp, 2
	jal r0, .LBB130_160
.LBB130_139:
	ldw r3, sp+28
	and r3, r1, r3
	ldw r4, sp+32
	beq r3, r4, .LBB130_142
.LBB130_140:
	addi r3, r0, 76
	beq r1, r3, .LBB130_142
.LBB130_141:
	addi r3, r0, 73
	bne r1, r3, .LBB130_207
.LBB130_142:
	add r3, fp, r18
	ldb r4, r3+0
	addi r3, r4, -58
	bltu r3, r21, .LBB130_202
.LBB130_143:
	addi r3, fp, 1
	addi r4, r4, -48
	bleu r4, r22, .LBB130_151
.LBB130_144:
	add r4, r0, r0
	jal r0, .LBB130_154
.LBB130_145:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 90
	beq r1, r3, .LBB130_147
.LBB130_146:
	addi r3, r0, 78
	bne r1, r3, .LBB130_205
.LBB130_147:
	addi r14, fp, 2
	jal r0, .LBB130_104
.LBB130_148:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 83
	beq r1, r3, .LBB130_150
.LBB130_149:
	addi r14, fp, 1
	bne r1, r26, .LBB130_104
.LBB130_150:
	addi r14, fp, 2
	jal r0, .LBB130_104
.LBB130_151:
	add r5, r0, r0
.LBB130_152:
	add r6, r3, r0
	mul r3, r5, r23
	add r5, r3, r4
	addi r3, r6, 1
	add r4, r6, r18
	ldb r4, r4+0
	addi r4, r4, -48
	bltu r4, r23, .LBB130_152
.LBB130_153:
	srli r4, r5, 31
.LBB130_154:
	bne r4, r0, .LBB130_202
.LBB130_155:
	add r4, r3, r16
	ldbu r4, r4+0
	ldw r5, sp+24
	bne r4, r5, .LBB130_158
.LBB130_156:
	addi r14, r3, 1
	add r3, r3, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB130_168
.LBB130_157:
	addi r3, r0, 1
	jal r0, .LBB130_173
.LBB130_158:
	add r14, r3, r0
	jal r0, .LBB130_174
.LBB130_159:
	addi r14, fp, 1
.LBB130_160:
	add r1, r14, r16
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB130_162
.LBB130_161:
	addi r1, r0, 1
	jal r0, .LBB130_167
.LBB130_162:
	addi r1, r1, -48
	bleu r1, r22, .LBB130_164
.LBB130_163:
	add r1, r0, r0
	jal r0, .LBB130_167
.LBB130_164:
	add r3, r0, r0
.LBB130_165:
	add r4, r14, r0
	mul r3, r3, r23
	add r3, r3, r1
	addi r14, r14, 1
	add r1, r4, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB130_165
.LBB130_166:
	srli r1, r3, 31
.LBB130_167:
	beq r1, r0, .LBB130_104
	jal r0, .LBB130_204
.LBB130_168:
	addi r3, r3, -48
	bleu r3, r22, .LBB130_170
.LBB130_169:
	add r3, r0, r0
	jal r0, .LBB130_173
.LBB130_170:
	add r4, r0, r0
.LBB130_171:
	add r5, r14, r0
	mul r4, r4, r23
	add r4, r4, r3
	addi r14, r14, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB130_171
.LBB130_172:
	srli r3, r4, 31
.LBB130_173:
	bne r3, r0, .LBB130_206
.LBB130_174:
	ldw r3, sp+32
	beq r1, r3, .LBB130_176
.LBB130_175:
	andi r1, r1, 77
	addi r3, r0, 69
	bne r1, r3, .LBB130_104
.LBB130_176:
	add r1, r14, r16
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 69
	bne r1, r3, .LBB130_104
.LBB130_177:
	addi r1, r14, 1
	add r3, r14, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB130_179
.LBB130_178:
	addi r3, r0, 1
	jal r0, .LBB130_184
.LBB130_179:
	addi r3, r3, -48
	bleu r3, r22, .LBB130_181
.LBB130_180:
	add r14, r1, r0
	add r3, r0, r0
	jal r0, .LBB130_185
.LBB130_181:
	add r4, r0, r0
.LBB130_182:
	add r5, r1, r0
	mul r1, r4, r23
	add r4, r1, r3
	addi r1, r5, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB130_182
.LBB130_183:
	srli r3, r4, 31
.LBB130_184:
	add r14, r1, r0
.LBB130_185:
	beq r3, r0, .LBB130_104
.LBB130_186:
	lui r14, %hi(.L.str.81)
	addi r14, r14, %lo(.L.str.81)
	jal r0, .LBB130_189
.LBB130_187:
	lui r1, %hi(fio_fmt)
	stw r1+%lo(fio_fmt), r16
	add r11, r0, r0
	jal r0, .LBB130_201
.LBB130_188:
	add r14, r17, r0
.LBB130_189:
	stw r15+%lo(fio_fmt_why), r14
.LBB130_190:
	addi r1, r0, 79
	addi r3, r0, 40
	addi r4, r0, 41
	add r6, r0, r0
	add r5, r0, r0
	jal r0, .LBB130_192
.LBB130_191:
	addi r6, r6, 1
.LBB130_192:
	blt r13, r0, .LBB130_194
.LBB130_193:
	add r7, r0, r0
	bgeu r6, r13, .LBB130_195
.LBB130_194:
	add r7, r12, r6
	ldbu r7, r7+0
.LBB130_195:
	andi r7, r7, 255
	beq r7, r0, .LBB130_200
.LBB130_196:
	bgtu r6, r1, .LBB130_200
.LBB130_197:
	seq r8, r7, r3
	add r5, r5, r8
	bne r7, r4, .LBB130_191
.LBB130_198:
	addi r5, r5, -1
	bne r5, r0, .LBB130_191
.LBB130_199:
	addi r6, r6, 1
.LBB130_200:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	ldw r5, r15+%lo(fio_fmt_why)
	lui r4, %hi(.L.str.85)
	addi r4, r4, %lo(.L.str.85)
	add r7, r12, r0
	jal r31, fprintf
.LBB130_201:
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
.LBB130_202:
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.79)
	addi r4, r4, %lo(.L.str.79)
	add r3, r14, r0
	add r5, r1, r0
.LBB130_203:
	jal r31, sprintf
	jal r0, .LBB130_189
.LBB130_204:
	lui r14, %hi(.L.str.77)
	addi r14, r14, %lo(.L.str.77)
	jal r0, .LBB130_189
.LBB130_205:
	lui r14, %hi(.L.str.78)
	addi r14, r14, %lo(.L.str.78)
	jal r0, .LBB130_189
.LBB130_206:
	lui r14, %hi(.L.str.80)
	addi r14, r14, %lo(.L.str.80)
	jal r0, .LBB130_189
.LBB130_207:
	blt r20, r0, .LBB130_210
.LBB130_208:
	addi r1, r1, -41
	addi r3, r0, 17
	bgtu r1, r3, .LBB130_211
.LBB130_209:
	slli r1, r1, 2
	lui r3, %hi(.Lswitch.table.fio_fmt_use)
	addi r3, r3, %lo(.Lswitch.table.fio_fmt_use)
	add r1, r1, r3
	ldw r14, r1+0
	jal r0, .LBB130_189
.LBB130_210:
	ldb r5, r14+0
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.84)
	addi r4, r4, %lo(.L.str.84)
	add r3, r14, r0
	jal r0, .LBB130_203
.LBB130_211:
	lui r14, %hi(.L.str.83)
	addi r14, r14, %lo(.L.str.83)
	jal r0, .LBB130_189
.LBB130_212:
.Lfunc_end130:
	.size	fio_fmt_use, .Lfunc_end130-fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI130_0,@object
.LJTI130_0:
	.word	.LBB130_37
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_36
	.word	.LBB130_38
	.word	.LBB130_16
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_38
	.word	.LBB130_16
	.size	.LJTI130_0, 160
	.type	.LJTI130_1,@object
.LJTI130_1:
	.word	.LBB130_9
	.word	.LBB130_190
	.word	.LBB130_212
	.word	.LBB130_212
	.word	.LBB130_102
	.word	.LBB130_9
	.size	.LJTI130_1, 24
	.type	.LJTI130_2,@object
.LJTI130_2:
	.word	.LBB130_187
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_108
	.word	.LBB130_103
	.word	.LBB130_103
	.word	.LBB130_116
	.word	.LBB130_106
	.word	.LBB130_103
	.word	.LBB130_106
	.word	.LBB130_116
	.word	.LBB130_103
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_116
	.word	.LBB130_103
	.size	.LJTI130_2, 236
	.type	.LJTI130_3,@object
.LJTI130_3:
	.word	.LBB130_132
	.word	.LBB130_145
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_132
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_148
	.word	.LBB130_136
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_139
	.word	.LBB130_132
	.size	.LJTI130_3, 104
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
	bleu r5, r6, .LBB131_2
.LBB131_1:
	add r1, r0, r0
	jal r0, .LBB131_5
.LBB131_2:
	addi r6, r1, 1
	addi r7, r0, 10
	add r1, r0, r0
.LBB131_3:
	mul r1, r1, r7
	addi r8, r4, 1
	add r1, r5, r1
	add r4, r6, r4
	ldb r4, r4+0
	addi r5, r4, -48
	add r4, r8, r0
	bltu r5, r7, .LBB131_3
.LBB131_4:
	stw r3+%lo(fio_pos), r8
.LBB131_5:
	jalr r0, r31, 0
.Lfunc_end131:
	.size	fio_number, .Lfunc_end131-fio_number
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
	beq r1, r0, .LBB132_3
.LBB132_1:
	lui r11, %hi(fio_irec)
	ldw r3, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r3, r4, .LBB132_28
.LBB132_2:
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
	jal r0, .LBB132_27
.LBB132_3:
	lui r12, %hi(fio_unit)
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	addi r13, r0, 98
	lui r11, %hi(fio_in)
	bgtu r3, r13, .LBB132_7
.LBB132_4:
	slli r1, r1, 2
	lui r14, %hi(fio_uend)
	addi r14, r14, %lo(fio_uend)
	add r1, r1, r14
	ldw r1, r1+0
	blt r1, r0, .LBB132_7
.LBB132_5:
	ldw r3, r11+%lo(fio_in)
	beq r3, r0, .LBB132_7
.LBB132_6:
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	add r4, r3, r14
	ldw r4, r4+0
	bge r1, r4, .LBB132_30
.LBB132_7:
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	bgtu r3, r13, .LBB132_11
.LBB132_8:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB132_11
.LBB132_9:
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
	beq r3, r0, .LBB132_29
.LBB132_10:
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB132_26
.LBB132_11:
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
.LBB132_12:
	add r18, r3, r0
	add r3, r11, r0
	jal r31, getc
	beq r1, r16, .LBB132_17
.LBB132_13:
	addi r3, r18, 1
	add r4, r18, r15
	stb r4+0, r1
	beq r1, r14, .LBB132_15
.LBB132_14:
	bltu r18, r17, .LBB132_12
.LBB132_15:
	beq r3, r0, .LBB132_18
.LBB132_16:
	add r1, r3, r15
	stb r1+0, r0
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB132_20
.LBB132_17:
	add r3, r18, r0
	bne r3, r0, .LBB132_16
.LBB132_18:
	ldw r1, r12+%lo(fio_unit)
	addi r4, r1, -1
	bgtu r4, r13, .LBB132_20
.LBB132_19:
	slli r1, r1, 2
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r1, r1, r4
	addi r4, r0, 1
	stw r1+0, r4
.LBB132_20:
	beq r3, r0, .LBB132_28
.LBB132_21:
	lui r1, %hi(fio_rlen)
	ldw r4, r1+%lo(fio_rlen)
	addi r3, r0, 1
	blt r4, r3, .LBB132_26
.LBB132_22:
	addi r4, r4, 1
	lui r5, %hi(fio_rec-2)
	addi r5, r5, %lo(fio_rec-2)
	addi r6, r0, 13
	jal r0, .LBB132_24
.LBB132_23:
	addi r7, r4, -2
	stw r1+%lo(fio_rlen), r7
	addi r4, r4, -1
	ble r4, r3, .LBB132_26
.LBB132_24:
	add r7, r4, r5
	ldbu r7, r7+0
	beq r7, r6, .LBB132_23
.LBB132_25:
	beq r7, r14, .LBB132_23
.LBB132_26:
	lui r1, %hi(fio_rpos)
	stw r1+%lo(fio_rpos), r0
.LBB132_27:
	add r1, r0, r0
	jal r0, .LBB132_29
.LBB132_28:
	addi r1, r0, 1
.LBB132_29:
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
.LBB132_30:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r3, r3, r1
	addi r1, r0, 1
	stw r3+0, r1
	jal r0, .LBB132_29
.Lfunc_end132:
	.size	fio_getrec, .Lfunc_end132-fio_getrec
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
	ble r1, r3, .LBB133_5
.LBB133_1:
	addi r4, r0, 0
	seq r4, r3, r4
	slli r3, r3, 1
	lui r5, 1
	xor r5, r3, r5
	sub r4, r0, r4
	and r4, r5, r4
	xor r3, r3, r4
.LBB133_2:
	add r13, r3, r0
	slli r3, r3, 1
	blt r13, r1, .LBB133_2
.LBB133_3:
	ldw r3, r15+%lo(nml_text)
	add r4, r13, r0
	jal r31, realloc
	beq r1, r0, .LBB133_6
.LBB133_4:
	stw r15+%lo(nml_text), r1
	stw r16+%lo(nml_tcap), r13
.LBB133_5:
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
.LBB133_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.97)
	addi r4, r4, %lo(.L.str.97)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end133:
	.size	nml_text_add, .Lfunc_end133-nml_text_add
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
	.type	fio_busy,@object                # @fio_busy
	.local	fio_busy
	.comm	fio_busy,1,4
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
	.type	fio_upos,@object                # @fio_upos
	.local	fio_upos
	.comm	fio_upos,4,4
	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"f77: unformatted READ past the end of the record\n"
	.size	.L.str.2, 50

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
	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"%12d"
	.size	.L.str.3, 5

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
	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"%u"
	.size	.L.str.4, 3

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"%s%0*d%s"
	.size	.L.str.5, 9

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"-"
	.size	.L.str.6, 2

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.zero	1
	.size	.L.str.7, 1

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"%s%s"
	.size	.L.str.8, 5

	.type	fio_fmt,@object                 # @fio_fmt
	.local	fio_fmt
	.comm	fio_fmt,4,4
	.type	fio_pos,@object                 # @fio_pos
	.local	fio_pos
	.comm	fio_pos,4,4
	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"%d"
	.size	.L.str.9, 3

	.type	fio_scale,@object               # @fio_scale
	.local	fio_scale
	.comm	fio_scale,4,4
	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"%%.%df"
	.size	.L.str.10, 7

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"."
	.size	.L.str.11, 2

	.type	fio_e,@object                   # @fio_e
	.local	fio_e
	.comm	fio_e,4,4
	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"%g"
	.size	.L.str.12, 3

	.type	fio_len,@object                 # @fio_len
	.local	fio_len
	.comm	fio_len,4,4
	.type	fio_line,@object                # @fio_line
	.local	fio_line
	.comm	fio_line,1024,1
	.type	fio_col,@object                 # @fio_col
	.local	fio_col
	.comm	fio_col,4,4
	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"f77: concatenation result of %d bytes is too long\n"
	.size	.L.str.13, 51

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
	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"PAUSE %.*s\n"
	.size	.L.str.14, 12

	.type	fio_in,@object                  # @fio_in
	.local	fio_in
	.comm	fio_in,4,4
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
	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"f77: %c editing does not match a numeric READ item\n"
	.size	.L.str.15, 52

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"f77: %c editing does not match a CHARACTER READ item\n"
	.size	.L.str.16, 54

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
	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"(I11)"
	.size	.L.str.17, 6

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"T"
	.size	.L.str.18, 2

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"F"
	.size	.L.str.19, 2

	.type	nml_asg,@object                 # @nml_asg
	.local	nml_asg
	.comm	nml_asg,4,4
	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"the subscripts do not match the array"
	.size	.L.str.20, 38

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"a subscript is out of range"
	.size	.L.str.21, 28

	.type	nml_val,@object                 # @nml_val
	.local	nml_val
	.comm	nml_val,4,4
	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"more values than the item has elements"
	.size	.L.str.22, 39

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	" &END"
	.size	.L.str.23, 6

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"%s is no item of the NAMELIST"
	.size	.L.str.24, 30

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"f77: end of file on unit %d looking for NAMELIST &%s\n"
	.size	.L.str.25, 54

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"f77: NAMELIST &%s on unit %d: %s\n"
	.size	.L.str.26, 34

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"f77: cannot OPEN unit %d\n"
	.size	.L.str.27, 26

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"DIRECT"
	.size	.L.str.28, 7

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"UNFORMATTED"
	.size	.L.str.29, 12

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"SCRATCH"
	.size	.L.str.30, 8

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"f77: OPEN ACCESS='DIRECT' needs RECL=\n"
	.size	.L.str.31, 39

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"f77: bad FILE= name in OPEN\n"
	.size	.L.str.32, 29

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"fort.%d"
	.size	.L.str.33, 8

	.type	fio_ufile,@object               # @fio_ufile
	.local	fio_ufile
	.comm	fio_ufile,400,4
	.type	fio_ufname,@object              # @fio_ufname
	.local	fio_ufname
	.comm	fio_ufname,25600,1
	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"ZERO"
	.size	.L.str.34, 5

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"f77: OPEN cannot create a scratch file\n"
	.size	.L.str.35, 40

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"OLD"
	.size	.L.str.36, 4

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"r+"
	.size	.L.str.37, 3

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"r"
	.size	.L.str.38, 2

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"f77: OPEN STATUS='OLD': no such file '%s'\n"
	.size	.L.str.39, 43

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"NEW"
	.size	.L.str.40, 4

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"f77: OPEN STATUS='NEW': '%s' already exists\n"
	.size	.L.str.41, 45

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"w+"
	.size	.L.str.42, 3

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"f77: OPEN cannot create '%s'\n"
	.size	.L.str.43, 30

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"UNKNOWN"
	.size	.L.str.44, 8

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"f77: OPEN cannot open '%s'\n"
	.size	.L.str.45, 28

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"f77: OPEN STATUS value is not supported\n"
	.size	.L.str.46, 41

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"DELETE"
	.size	.L.str.47, 7

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"KEEP"
	.size	.L.str.48, 5

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"f77: CLOSE STATUS value is not supported\n"
	.size	.L.str.49, 42

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
	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"stdin"
	.size	.L.str.50, 6

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"stdout"
	.size	.L.str.51, 7

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"stderr"
	.size	.L.str.52, 7

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"UNDEFINED"
	.size	.L.str.53, 10

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"SEQUENTIAL"
	.size	.L.str.54, 11

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"NO"
	.size	.L.str.55, 3

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"YES"
	.size	.L.str.56, 4

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"FORMATTED"
	.size	.L.str.57, 10

	.type	.L.str.58,@object               # @.str.58
.L.str.58:
	.asciz	"NULL"
	.size	.L.str.58, 5

	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"f77: REWIND: unit %d cannot be rewound\n"
	.size	.L.str.59, 40

	.type	.L.str.60,@object               # @.str.60
.L.str.60:
	.asciz	"f77: an I/O statement in a function referenced in another's I/O list\n"
	.size	.L.str.60, 70

	.type	fio_uend_init,@object           # @fio_uend_init
	.local	fio_uend_init
	.comm	fio_uend_init,1,4
	.type	.L.str.61,@object               # @.str.61
.L.str.61:
	.asciz	"f77: unit %d is not open for %s\n"
	.size	.L.str.61, 33

	.type	.L.str.62,@object               # @.str.62
.L.str.62:
	.asciz	"WRITE"
	.size	.L.str.62, 6

	.type	.L.str.63,@object               # @.str.63
.L.str.63:
	.asciz	"READ"
	.size	.L.str.63, 5

	.type	.L.str.64,@object               # @.str.64
.L.str.64:
	.asciz	"f77: a direct-access unit needs REC=\n"
	.size	.L.str.64, 38

	.type	.L.str.65,@object               # @.str.65
.L.str.65:
	.asciz	"f77: READ past the end of a direct-access file\n"
	.size	.L.str.65, 48

	.type	.L.str.66,@object               # @.str.66
.L.str.66:
	.asciz	"f77: end of file on READ\n"
	.size	.L.str.66, 26

	.type	.L.str.67,@object               # @.str.67
.L.str.67:
	.asciz	"f77: bad unformatted record\n"
	.size	.L.str.67, 29

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
	.type	.L.str.68,@object               # @.str.68
.L.str.68:
	.asciz	"a format begins with ("
	.size	.L.str.68, 23

	.type	fio_fmt_why,@object             # @fio_fmt_why
	.local	fio_fmt_why
	.comm	fio_fmt_why,4,4
	.type	.L.str.69,@object               # @.str.69
.L.str.69:
	.asciz	"the format is too long, or ends without its )"
	.size	.L.str.69, 46

	.type	.L.str.70,@object               # @.str.70
.L.str.70:
	.asciz	"the format ends without its )"
	.size	.L.str.70, 30

	.type	.L.str.71,@object               # @.str.71
.L.str.71:
	.asciz	"a quoted string runs past the end of the format"
	.size	.L.str.71, 48

	.type	.L.str.72,@object               # @.str.72
.L.str.72:
	.asciz	"the format is too long"
	.size	.L.str.72, 23

	.type	.L.str.73,@object               # @.str.73
.L.str.73:
	.asciz	"0H is no Hollerith string"
	.size	.L.str.73, 26

	.type	.L.str.74,@object               # @.str.74
.L.str.74:
	.asciz	"nH wants more characters than the format has"
	.size	.L.str.74, 45

	.type	fio_fmt_check.why,@object       # @fio_fmt_check.why
	.local	fio_fmt_check.why
	.comm	fio_fmt_check.why,64,1
	.type	.L.str.75,@object               # @.str.75
.L.str.75:
	.asciz	"a sign belongs to a scale factor, nP"
	.size	.L.str.75, 37

	.type	.L.str.76,@object               # @.str.76
.L.str.76:
	.asciz	"H needs its count, nH"
	.size	.L.str.76, 22

	.type	.L.str.77,@object               # @.str.77
.L.str.77:
	.asciz	"T, TL and TR take a column"
	.size	.L.str.77, 27

	.type	.L.str.78,@object               # @.str.78
.L.str.78:
	.asciz	"B is BN or BZ"
	.size	.L.str.78, 14

	.type	.L.str.79,@object               # @.str.79
.L.str.79:
	.asciz	"%c needs a width"
	.size	.L.str.79, 17

	.type	.L.str.80,@object               # @.str.80
.L.str.80:
	.asciz	"a . needs digits after it"
	.size	.L.str.80, 26

	.type	.L.str.81,@object               # @.str.81
.L.str.81:
	.asciz	"Ee needs its digits"
	.size	.L.str.81, 20

	.type	.L.str.82,@object               # @.str.82
.L.str.82:
	.asciz	"a count with no descriptor after it"
	.size	.L.str.82, 36

	.type	.L.str.83,@object               # @.str.83
.L.str.83:
	.asciz	"a count before something that is no edit descriptor"
	.size	.L.str.83, 52

	.type	.L.str.84,@object               # @.str.84
.L.str.84:
	.asciz	"%c is no edit descriptor"
	.size	.L.str.84, 25

	.type	.L.str.85,@object               # @.str.85
.L.str.85:
	.asciz	"f77: FORMAT: %s\n  in: %.*s\n"
	.size	.L.str.85, 28

	.type	.L.str.86,@object               # @.str.86
.L.str.86:
	.asciz	"%%.%dE"
	.size	.L.str.86, 7

	.type	fio_gcount,@object              # @fio_gcount
	.local	fio_gcount
	.comm	fio_gcount,64,4
	.type	fio_gstart,@object              # @fio_gstart
	.local	fio_gstart
	.comm	fio_gstart,64,4
	.type	.L.str.87,@object               # @.str.87
.L.str.87:
	.asciz	"f77: FORMAT: no data edit descriptor for the items left\n"
	.size	.L.str.87, 57

	.type	.L.str.88,@object               # @.str.88
.L.str.88:
	.asciz	"f77: WRITE past the end of an internal file\n"
	.size	.L.str.88, 45

	.type	.L.str.89,@object               # @.str.89
.L.str.89:
	.asciz	"f77: record longer than the internal file (%d > %d)\n"
	.size	.L.str.89, 53

	.type	.L.str.90,@object               # @.str.90
.L.str.90:
	.asciz	"f77: record longer than RECL (%d > %d)\n"
	.size	.L.str.90, 40

	.type	fio_l_nulls,@object             # @fio_l_nulls
	.local	fio_l_nulls
	.comm	fio_l_nulls,4,4
	.type	fio_l_pending,@object           # @fio_l_pending
	.local	fio_l_pending
	.comm	fio_l_pending,4,4
	.type	.L.str.91,@object               # @.str.91
.L.str.91:
	.asciz	"f77: zero repeat count in list-directed input\n"
	.size	.L.str.91, 47

	.type	.L.str.92,@object               # @.str.92
.L.str.92:
	.asciz	"f77: bad LOGICAL input field '%s'\n"
	.size	.L.str.92, 35

	.type	.L.str.93,@object               # @.str.93
.L.str.93:
	.asciz	"E%d"
	.size	.L.str.93, 4

	.type	.L.str.94,@object               # @.str.94
.L.str.94:
	.asciz	"(A)"
	.size	.L.str.94, 4

	.type	nml_tlen,@object                # @nml_tlen
	.local	nml_tlen
	.comm	nml_tlen,4,4
	.type	.L.str.95,@object               # @.str.95
.L.str.95:
	.asciz	"the record could not be read"
	.size	.L.str.95, 29

	.type	.L.str.96,@object               # @.str.96
.L.str.96:
	.asciz	" "
	.size	.L.str.96, 2

	.type	nml_tcap,@object                # @nml_tcap
	.local	nml_tcap
	.comm	nml_tcap,4,4
	.type	nml_text,@object                # @nml_text
	.local	nml_text
	.comm	nml_text,4,4
	.type	.L.str.97,@object               # @.str.97
.L.str.97:
	.asciz	"f77: out of storage for a NAMELIST\n"
	.size	.L.str.97, 36

	.type	.L.str.98,@object               # @.str.98
.L.str.98:
	.asciz	"a subscript is no integer"
	.size	.L.str.98, 26

	.type	.L.str.99,@object               # @.str.99
.L.str.99:
	.asciz	"a value before the first NAME="
	.size	.L.str.99, 31

	.type	.L.str.100,@object              # @.str.100
.L.str.100:
	.asciz	"a repeat count is not positive"
	.size	.L.str.100, 31

	.type	nml_cval,@object                # @nml_cval
	.local	nml_cval
	.comm	nml_cval,4,4
	.type	nml_casg,@object                # @nml_casg
	.local	nml_casg
	.comm	nml_casg,4,4
	.type	nml_pend,@object                # @nml_pend
	.local	nml_pend
	.comm	nml_pend,1024,1
	.type	.L.str.101,@object              # @.str.101
.L.str.101:
	.asciz	"0.0"
	.size	.L.str.101, 4

	.type	.L.str.102,@object              # @.str.102
.L.str.102:
	.asciz	"(G23.16)"
	.size	.L.str.102, 9

	.type	.L.str.103,@object              # @.str.103
.L.str.103:
	.asciz	"(G15.8)"
	.size	.L.str.103, 8

	.type	.L.str.104,@object              # @.str.104
.L.str.104:
	.asciz	"a LOGICAL value is not T or F"
	.size	.L.str.104, 30

	.type	.L.str.105,@object              # @.str.105
.L.str.105:
	.asciz	"a CHARACTER value is not in quotes"
	.size	.L.str.105, 35

	.type	.L.str.106,@object              # @.str.106
.L.str.106:
	.asciz	"a value could not be read"
	.size	.L.str.106, 26

	.type	.Lswitch.table.fio_fmt_use,@object # @switch.table.fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.Lswitch.table.fio_fmt_use:
	.word	.L.str.82
	.word	.L.str.83
	.word	.L.str.83
	.word	.L.str.82
	.word	.L.str.83
	.word	.L.str.83
	.word	.L.str.82
	.word	.L.str.83
	.word	.L.str.83
	.word	.L.str.83
	.word	.L.str.83
	.word	.L.str.83
	.word	.L.str.83
	.word	.L.str.83
	.word	.L.str.83
	.word	.L.str.83
	.word	.L.str.83
	.word	.L.str.82
	.size	.Lswitch.table.fio_fmt_use, 72

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git a06e1a6e35103a79b1ae3c6830426b277cc25690)"
	.section	".note.GNU-stack","",@progbits
