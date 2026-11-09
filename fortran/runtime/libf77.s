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
	lui r4, %hi(.L.str.76)
	addi r4, r4, %lo(.L.str.76)
	jal r0, .LBB1_24
.LBB1_22:
	addi r3, r0, 2
	jal r31, exit
.LBB1_23:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.80)
	addi r4, r4, %lo(.L.str.80)
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
	lui r5, %hi(.L.str.49)
	addi r5, r5, %lo(.L.str.49)
	addi r12, sp, 20
	addi r4, r0, 256
	add r3, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	add r3, r12, r0
	jal r31, fopen
	bne r1, r0, .LBB2_13
.LBB2_12:
	lui r4, %hi(.L.str.58)
	addi r4, r4, %lo(.L.str.58)
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
	lui r4, %hi(.L.str.78)
	addi r4, r4, %lo(.L.str.78)
	lui r5, %hi(.L.str.79)
	addi r5, r5, %lo(.L.str.79)
	xor r5, r5, r4
	sub r1, r0, r1
	and r1, r5, r1
	xor r6, r4, r1
	lui r4, %hi(.L.str.77)
	addi r4, r4, %lo(.L.str.77)
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
	lui r4, %hi(.L.str.76)
	addi r4, r4, %lo(.L.str.76)
	jal r0, .LBB3_14
.LBB3_13:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.80)
	addi r4, r4, %lo(.L.str.80)
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
	lui r4, %hi(.L.str.81)
	addi r4, r4, %lo(.L.str.81)
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
	lui r4, %hi(.L.str.76)
	addi r4, r4, %lo(.L.str.76)
	jal r0, .LBB6_35
.LBB6_32:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.82)
	addi r4, r4, %lo(.L.str.82)
	jal r0, .LBB6_35
.LBB6_33:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.80)
	addi r4, r4, %lo(.L.str.80)
	jal r0, .LBB6_35
.LBB6_34:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.83)
	addi r4, r4, %lo(.L.str.83)
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
	lui r4, %hi(.L.str.76)
	addi r4, r4, %lo(.L.str.76)
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
	.p2align	2                               # -- Begin function fio_plus
	.type	fio_plus,@function
fio_plus:                               # @fio_plus
# %bb.0:
	lui r1, %hi(fio_sp)
	ldbu r1, r1+%lo(fio_sp)
	addi r4, r0, 1
	bne r1, r4, .LBB12_6
.LBB12_1:
	ldbu r1, r3+0
	beq r1, r0, .LBB12_6
.LBB12_2:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	addi r13, r0, 43
	beq r1, r13, .LBB12_5
.LBB12_3:
	addi r4, r0, 45
	beq r1, r4, .LBB12_5
.LBB12_4:
	addi r11, r3, 1
	add r12, r3, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, memmove
	stb r12+0, r13
.LBB12_5:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB12_6:
	jalr r0, r31, 0
.Lfunc_end12:
	.size	fio_plus, .Lfunc_end12-fio_plus
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
	ble r12, r0, .LBB13_11
.LBB13_1:
	bge r12, r1, .LBB13_21
.LBB13_2:
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
	jal r0, .LBB13_5
.LBB13_3:
	add r10, r11, r0
.LBB13_4:
	addi r12, lr, -1
	ble lr, r5, .LBB13_33
.LBB13_5:
	add lr, r12, r0
	bgt r10, r4, .LBB13_4
.LBB13_6:
	bge r8, r10, .LBB13_9
.LBB13_7:
	addi r11, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, r11, r0
	bne r10, r11, .LBB13_7
.LBB13_8:
	stw r1+%lo(fio_len), r10
	add r8, r10, r0
.LBB13_9:
	add r11, r10, r6
	stb r11+0, r9
	addi r11, r10, 1
	stw r3+%lo(fio_col), r11
	blt r10, r8, .LBB13_3
.LBB13_10:
	stw r1+%lo(fio_len), r11
	add r8, r11, r0
	jal r0, .LBB13_3
.LBB13_11:
	ldbu r9, r11+0
	beq r9, r0, .LBB13_33
.LBB13_12:
	lui r1, %hi(fio_len)
	ldw r7, r1+%lo(fio_len)
	lui r3, %hi(fio_col)
	ldw r8, r3+%lo(fio_col)
	addi r4, r0, 1022
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	addi r6, r0, 32
	jal r0, .LBB13_15
.LBB13_13:
	add r8, r9, r0
.LBB13_14:
	addi r11, r11, 1
	ldbu r9, r11+0
	beq r9, r0, .LBB13_33
.LBB13_15:
	bgt r8, r4, .LBB13_14
.LBB13_16:
	bge r7, r8, .LBB13_19
.LBB13_17:
	addi r10, r7, 1
	add r7, r7, r5
	stb r7+0, r6
	add r7, r10, r0
	bne r8, r10, .LBB13_17
.LBB13_18:
	stw r1+%lo(fio_len), r8
	add r7, r8, r0
.LBB13_19:
	add r10, r8, r5
	stb r10+0, r9
	addi r9, r8, 1
	stw r3+%lo(fio_col), r9
	blt r8, r7, .LBB13_13
.LBB13_20:
	stw r1+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB13_13
.LBB13_21:
	sub r3, r12, r1
	addi r4, r0, 1
	lui r1, %hi(fio_col)
	blt r3, r4, .LBB13_23
.LBB13_22:
	ldw r4, r1+%lo(fio_col)
	add r3, r4, r3
	stw r1+%lo(fio_col), r3
.LBB13_23:
	ldbu r9, r11+0
	beq r9, r0, .LBB13_33
.LBB13_24:
	lui r3, %hi(fio_len)
	ldw r7, r3+%lo(fio_len)
	ldw r8, r1+%lo(fio_col)
	addi r4, r0, 1022
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	addi r6, r0, 32
	jal r0, .LBB13_27
.LBB13_25:
	add r8, r9, r0
.LBB13_26:
	addi r11, r11, 1
	ldbu r9, r11+0
	beq r9, r0, .LBB13_33
.LBB13_27:
	bgt r8, r4, .LBB13_26
.LBB13_28:
	bge r7, r8, .LBB13_31
.LBB13_29:
	addi r10, r7, 1
	add r7, r7, r5
	stb r7+0, r6
	add r7, r10, r0
	bne r8, r10, .LBB13_29
.LBB13_30:
	stw r3+%lo(fio_len), r8
	add r7, r8, r0
.LBB13_31:
	add r10, r8, r5
	stb r10+0, r9
	addi r9, r8, 1
	stw r1+%lo(fio_col), r9
	blt r8, r7, .LBB13_25
.LBB13_32:
	stw r3+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB13_25
.LBB13_33:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end13:
	.size	fio_field, .Lfunc_end13-fio_field
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_wr_d
	.type	.LCPI14_0,@object
.LCPI14_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI14_0, 8
	.type	.LCPI14_1,@object
.LCPI14_1:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI14_1, 8
	.type	.LCPI14_2,@object
.LCPI14_2:
	.quad	0x4024000000000000              # double 10
	.size	.LCPI14_2, 8
	.type	.LCPI14_3,@object
.LCPI14_3:
	.quad	0x3fb999999999999a              # double 0.10000000000000001
	.size	.LCPI14_3, 8
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
	beq r1, r0, .LBB14_2
.LBB14_1:
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
.LBB14_2:
	lui r15, %hi(fio_desc)
	ldw r1, r15+%lo(fio_desc)
	lui r16, %hi(fio_rep)
	beq r1, r0, .LBB14_4
.LBB14_3:
	ldw r3, r16+%lo(fio_rep)
	bgt r3, r0, .LBB14_5
.LBB14_4:
	stw r15+%lo(fio_desc), r0
	jal r31, fio_next_desc
.LBB14_5:
	beq r1, r0, .LBB14_79
.LBB14_6:
	addi r3, r0, 73
	beq r1, r3, .LBB14_15
.LBB14_7:
	addi r4, r0, 65
	bne r1, r4, .LBB14_10
.LBB14_8:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, r4, -1
	addi r5, r0, 7
	lui r1, %hi(fio_col)
	bgeu r3, r5, .LBB14_21
.LBB14_9:
	add r3, r4, r0
	jal r0, .LBB14_23
.LBB14_10:
	feq.d r4, r12, r12
	and r7, r4, r4
	xori r4, r7, 1
	lui r6, %hi(.LCPI14_0)
	addi r6, r6, %lo(.LCPI14_0)
	bne r4, r0, .LBB14_18
.LBB14_11:
	fsub.d r8, r12, r12
	ldw r5, r6+4
	ldw r4, r6+0
	feq.d r8, r8, r4
	beq r8, r0, .LBB14_18
.LBB14_12:
	addi r6, r0, 70
	beq r1, r6, .LBB14_40
.LBB14_13:
	addi r6, r0, 71
	beq r1, r6, .LBB14_32
.LBB14_14:
	bne r1, r3, .LBB14_50
.LBB14_15:
	fcvt.w.d r6, r12
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
	addi r3, sp, 44
	addi r4, r0, 64
.LBB14_16:
	jal r31, snprintf
.LBB14_17:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 44
	jal r0, .LBB14_74
.LBB14_18:
	lui r1, %hi(fio_w)
	ldw r11, r1+%lo(fio_w)
	beq r7, r0, .LBB14_71
.LBB14_19:
	ldw r5, r6+4
	ldw r4, r6+0
	flt.d r3, r12, r4
	lui r1, %hi(fio_sp)
	ldbu r4, r1+%lo(fio_sp)
	andi r1, r4, 1
	sub r1, r0, r1
	andi r1, r1, 43
	sub r5, r0, r3
	xori r6, r1, 45
	and r5, r6, r5
	xor r1, r1, r5
	or  r3, r3, r4
	andi r3, r3, 1
	sub r3, r11, r3
	addi r4, r0, 7
	sgt r3, r3, r4
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
	lui r5, %hi(.L.str.11)
	addi r5, r5, %lo(.L.str.11)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r4, r4, r3
	beq r1, r0, .LBB14_72
.LBB14_20:
	stb sp+44, r1
	addi r1, sp, 44
	addi r3, r1, 1
	jal r0, .LBB14_73
.LBB14_21:
	addi r3, r0, 8
	addi r5, r0, 9
	blt r4, r5, .LBB14_23
.LBB14_22:
	ldw r5, r1+%lo(fio_col)
	add r4, r4, r5
	addi r4, r4, -8
	stw r1+%lo(fio_col), r4
.LBB14_23:
	lui r4, %hi(fio_len)
	ldw r8, r4+%lo(fio_len)
	ldw r10, r1+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	add r9, r0, r0
	jal r0, .LBB14_26
.LBB14_24:
	add r10, lr, r0
.LBB14_25:
	addi r9, r9, 1
	beq r9, r3, .LBB14_75
.LBB14_26:
	addi lr, sp, 108
	bgt r10, r5, .LBB14_25
.LBB14_27:
	bge r8, r10, .LBB14_30
.LBB14_28:
	addi r11, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, r11, r0
	bne r10, r11, .LBB14_28
.LBB14_29:
	stw r4+%lo(fio_len), r10
	add r8, r10, r0
.LBB14_30:
	add lr, lr, r9
	ldbu lr, lr+0
	add r11, r10, r6
	stb r11+0, lr
	addi lr, r10, 1
	stw r1+%lo(fio_col), lr
	blt r10, r8, .LBB14_24
.LBB14_31:
	stw r4+%lo(fio_len), lr
	add r8, lr, r0
	jal r0, .LBB14_24
.LBB14_32:
	lui r1, %hi(fio_d)
	ldw r1, r1+%lo(fio_d)
	addi r17, r0, 1
	sgt r3, r1, r17
	sub r3, r0, r3
	xori r6, r1, 1
	and r3, r6, r3
	xori r10, r3, 1
	lui r6, %hi(.LCPI14_1)
	addi r6, r6, %lo(.LCPI14_1)
	ldw r9, r6+4
	ldw r8, r6+0
	lui lr, %hi(.LCPI14_2)
	addi lr, lr, %lo(.LCPI14_2)
	ldw r7, lr+4
	ldw r6, lr+0
	add r18, r8, r0
	add r19, r9, r0
.LBB14_33:
	fmul.d r18, r18, r6
	addi r10, r10, -1
	bne r10, r0, .LBB14_33
.LBB14_34:
	flt.d r10, r12, r4
	fneg.d r20, r12
	xor lr, r21, r13
	sub r14, r0, r10
	and r10, lr, r14
	xor r11, r13, r10
	xor lr, r20, r12
	and lr, lr, r14
	xor r10, r12, lr
	feq.d r4, r12, r4
	bne r4, r0, .LBB14_37
.LBB14_35:
	lui r4, %hi(.LCPI14_3)
	addi r4, r4, %lo(.LCPI14_3)
	ldw r21, r4+4
	ldw r20, r4+0
	fle.d r4, r20, r10
	xori r4, r4, 1
	bne r4, r0, .LBB14_38
.LBB14_36:
	flt.d r4, r10, r18
	xori r4, r4, 1
	bne r4, r0, .LBB14_38
.LBB14_37:
	add r17, r0, r0
.LBB14_38:
	xori r4, r3, 1
	beq r17, r0, .LBB14_53
.LBB14_39:
	addi r11, sp, 44
	addi r7, r0, 69
	add r3, r11, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fio_efmt
	add r3, r11, r0
	jal r31, fio_plus
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	add r3, r11, r0
	jal r31, fio_drop_zero
	jal r0, .LBB14_69
.LBB14_40:
	lui r1, %hi(fio_scale)
	ldw r3, r1+%lo(fio_scale)
	addi r1, r0, 1
	blt r3, r1, .LBB14_44
.LBB14_41:
	addi r3, r3, 1
	lui r6, %hi(.LCPI14_2)
	addi r6, r6, %lo(.LCPI14_2)
	ldw r5, r6+4
	ldw r4, r6+0
.LBB14_42:
	fmul.d r12, r12, r4
	addi r3, r3, -1
	bgt r3, r1, .LBB14_42
.LBB14_43:
	add r3, r0, r0
.LBB14_44:
	addi r4, r0, -1
	bgt r3, r4, .LBB14_47
.LBB14_45:
	lui r6, %hi(.LCPI14_2)
	addi r6, r6, %lo(.LCPI14_2)
	ldw r5, r6+4
	ldw r4, r6+0
.LBB14_46:
	fdiv.d r12, r12, r4
	addi r6, r3, 1
	sltu r7, r6, r3
	add r3, r6, r0
	bne r7, r1, .LBB14_46
.LBB14_47:
	lui r14, %hi(fio_d)
	ldw r6, r14+%lo(fio_d)
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
	ldw r1, r14+%lo(fio_d)
	bne r1, r0, .LBB14_49
.LBB14_48:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	addi r3, sp, 44
	jal r31, strcat
.LBB14_49:
	addi r11, sp, 44
	jal r0, .LBB14_52
.LBB14_50:
	addi r3, r0, -2
	and r3, r1, r3
	addi r4, r0, 68
	bne r3, r4, .LBB14_56
.LBB14_51:
	lui r3, %hi(fio_d)
	ldw r4, r3+%lo(fio_d)
	addi r11, sp, 44
	add r3, r11, r0
	add r5, r12, r0
	add r6, r13, r0
	add r7, r1, r0
	jal r31, fio_efmt
.LBB14_52:
	add r3, r11, r0
	jal r31, fio_plus
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	add r3, r11, r0
	jal r31, fio_drop_zero
	jal r0, .LBB14_17
.LBB14_53:
	lui r3, %hi(fio_e)
	ldw r5, r3+%lo(fio_e)
	addi r3, r0, 0
	sgt lr, r5, r3
	addi r5, r5, 2
	sub lr, r0, lr
	xori r5, r5, 4
	and r5, r5, lr
	xori r18, r5, 4
	lui r19, %hi(fio_w)
	ldw r5, r19+%lo(fio_w)
	slt lr, r5, r18
	sub lr, r0, lr
	and lr, r18, lr
	xor lr, r18, lr
	fle.d r8, r8, r10
	xori r9, r8, 1
	add r8, r0, r0
	bne r9, r0, .LBB14_64
.LBB14_54:
	fle.d r8, r6, r10
	xori r8, r8, 1
	beq r8, r0, .LBB14_60
.LBB14_55:
	addi r8, r0, -1
	jal r0, .LBB14_64
.LBB14_56:
	addi r3, r0, 339
	beq r1, r3, .LBB14_58
.LBB14_57:
	addi r3, r0, 334
	bne r1, r3, .LBB14_59
.LBB14_58:
	addi r3, r0, 334
	seq r7, r1, r3
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	addi r11, sp, 44
	add r3, r11, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fio_esn
	add r3, r11, r0
	jal r31, fio_plus
	jal r0, .LBB14_17
.LBB14_59:
	lui r5, %hi(.L.str.15)
	addi r5, r5, %lo(.L.str.15)
	addi r3, sp, 44
	addi r4, r0, 64
	add r6, r12, r0
	add r7, r13, r0
	jal r0, .LBB14_16
.LBB14_60:
	addi r8, r0, 1
.LBB14_61:
	add r9, r8, r0
	fdiv.d r10, r10, r6
	addi r8, r8, 1
	fle.d r14, r6, r10
	xori r14, r14, 1
	bne r14, r0, .LBB14_63
.LBB14_62:
	bgt r1, r9, .LBB14_61
.LBB14_63:
	sub r8, r0, r8
.LBB14_64:
	sub r11, r5, lr
	add r20, r8, r4
	sgt r1, r20, r3
	sub r1, r0, r1
	and r6, r20, r1
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
	bgt r20, r0, .LBB14_66
.LBB14_65:
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	addi r3, sp, 44
	jal r31, strcat
.LBB14_66:
	addi r12, sp, 44
	add r3, r12, r0
	jal r31, fio_plus
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fio_drop_zero
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fio_field
	ldw r1, r19+%lo(fio_w)
	slt r1, r1, r18
	sub r1, r0, r1
	and r1, r18, r1
	xor r3, r18, r1
	jal r31, fio_blanks
	ldw r1, r16+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB14_68
.LBB14_67:
	addi r1, r1, -1
	stw r16+%lo(fio_rep), r1
.LBB14_68:
	ldw r1, r16+%lo(fio_rep)
	beq r1, r0, .LBB14_70
.LBB14_69:
	bne r17, r0, .LBB14_17
	jal r0, .LBB14_79
.LBB14_70:
	stw r15+%lo(fio_desc), r0
	bne r17, r0, .LBB14_17
	jal r0, .LBB14_79
.LBB14_71:
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
.LBB14_72:
	addi r3, sp, 44
.LBB14_73:
	jal r31, strcpy
	addi r3, sp, 44
	add r4, r11, r0
.LBB14_74:
	jal r31, fio_field
.LBB14_75:
	ldw r1, r16+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB14_77
.LBB14_76:
	addi r1, r1, -1
	stw r16+%lo(fio_rep), r1
.LBB14_77:
	ldw r1, r16+%lo(fio_rep)
	bne r1, r0, .LBB14_79
.LBB14_78:
	stw r15+%lo(fio_desc), r0
.LBB14_79:
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
	.size	f77_wr_d, .Lfunc_end14-f77_wr_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_list_out_real
	.type	.LCPI15_0,@object
.LCPI15_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI15_0, 8
	.type	.LCPI15_2,@object
.LCPI15_2:
	.quad	0x3fb999999999999a              # double 0.10000000000000001
	.size	.LCPI15_2, 8
	.type	.LCPI15_3,@object
.LCPI15_3:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI15_3, 8
	.type	.LCPI15_4,@object
.LCPI15_4:
	.quad	0x4024000000000000              # double 10
	.size	.LCPI15_4, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI15_1,@object
.LCPI15_1:
	.quad	0x41cdcd6500000000              # double 1.0E+9
	.quad	0x4376345785d8a000              # double 1.0E+17
	.size	.LCPI15_1, 16
	.text
	.p2align	2
	.type	fio_list_out_real,@function
fio_list_out_real:                      # @fio_list_out_real
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
	stw sp+100, r21
	stw sp+96, r22
	add r13, r4, r0
	add r12, r3, r0
	lui r1, %hi(.LCPI15_0)
	addi r1, r1, %lo(.LCPI15_0)
	addi r3, r0, 0
	sne r16, r5, r3
	lui r14, %hi(fio_col)
	ldw r3, r14+%lo(fio_col)
	addi r15, r0, 1022
	bgt r3, r15, .LBB15_6
.LBB15_1:
	lui r4, %hi(fio_len)
	ldw r6, r4+%lo(fio_len)
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
	bge r6, r3, .LBB15_4
.LBB15_2:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r3, r9, .LBB15_2
.LBB15_3:
	stw r4+%lo(fio_len), r3
	add r6, r3, r0
.LBB15_4:
	add r7, r3, r7
	stb r7+0, r8
	addi r7, r3, 1
	stw r14+%lo(fio_col), r7
	blt r3, r6, .LBB15_6
.LBB15_5:
	stw r4+%lo(fio_len), r7
.LBB15_6:
	ldw r19, r1+4
	ldw r18, r1+0
	sub r17, r0, r16
	feq.d r1, r12, r12
	and r1, r1, r1
	xori r1, r1, 1
	bne r1, r0, .LBB15_26
.LBB15_7:
	fsub.d r6, r12, r12
	feq.d r3, r6, r18
	beq r3, r0, .LBB15_26
.LBB15_8:
	fneg.d r6, r12
	flt.d r1, r12, r18
	xor r3, r7, r13
	sub r1, r0, r1
	xor r4, r6, r12
	and r3, r3, r1
	and r1, r4, r1
	xor r7, r13, r3
	xor r6, r12, r1
	andi r20, r17, 24
	xori r21, r20, 9
	feq.d r1, r12, r18
	bne r1, r0, .LBB15_11
.LBB15_9:
	lui r1, %hi(.LCPI15_2)
	addi r1, r1, %lo(.LCPI15_2)
	ldw r9, r1+4
	ldw r8, r1+0
	fle.d r1, r8, r6
	xori r1, r1, 1
	bne r1, r0, .LBB15_29
.LBB15_10:
	andi r1, r17, 8
	lui r3, %hi(.LCPI15_1)
	addi r3, r3, %lo(.LCPI15_1)
	add r1, r3, r1
	ldw r9, r1+4
	ldw r8, r1+0
	flt.d r1, r6, r8
	xori r1, r1, 1
	bne r1, r0, .LBB15_29
.LBB15_11:
	addi r17, r0, 1
	lui r1, %hi(.LCPI15_3)
	addi r1, r1, %lo(.LCPI15_3)
	ldw r5, r1+4
	ldw r4, r1+0
	fle.d r1, r4, r6
	xori r3, r1, 1
	add r1, r17, r0
	bne r3, r0, .LBB15_14
.LBB15_12:
	lui r1, %hi(.LCPI15_4)
	addi r1, r1, %lo(.LCPI15_4)
	ldw r9, r1+4
	ldw r8, r1+0
	add r10, r6, r0
	add r11, r7, r0
	add r1, r0, r0
.LBB15_13:
	fdiv.d r10, r10, r8
	addi r1, r1, 1
	fle.d r3, r4, r10
	bne r3, r0, .LBB15_13
.LBB15_14:
	flt.d r3, r6, r4
	feq.d r4, r12, r18
	xori r4, r4, 1
	and r22, r4, r3
	sub r3, r0, r22
	and r3, r1, r3
	xor r1, r1, r3
	sub r6, r21, r1
	lui r5, %hi(.L.str.13)
	addi r5, r5, %lo(.L.str.13)
	addi r11, sp, 16
	addi r4, r0, 16
	add r3, r11, r0
	jal r31, snprintf
	addi r3, sp, 32
	addi r4, r0, 64
	add r5, r11, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	bne r22, r17, .LBB15_17
.LBB15_15:
	flt.d r11, r12, r18
	addi r3, sp, 32
	jal r31, strlen
	add r3, r11, r21
	addi r3, r3, 2
	bleu r1, r3, .LBB15_17
.LBB15_16:
	addi r1, sp, 32
	add r1, r1, r3
	stb r1+0, r0
.LBB15_17:
	addi r11, r16, 4
	xori r4, r20, 12
	addi r3, sp, 32
	jal r31, fio_field
	lui r1, %hi(fio_len)
	ldw r6, r1+%lo(fio_len)
	ldw r7, r14+%lo(fio_col)
	addi r3, r0, 1
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	addi r5, r0, 32
	jal r0, .LBB15_20
.LBB15_18:
	add r7, r9, r0
.LBB15_19:
	addi r11, r8, -1
	bleu r8, r3, .LBB15_28
.LBB15_20:
	add r8, r11, r0
	bgt r7, r15, .LBB15_19
.LBB15_21:
	bge r6, r7, .LBB15_24
.LBB15_22:
	addi r9, r6, 1
	add r6, r6, r4
	stb r6+0, r5
	add r6, r9, r0
	bne r7, r9, .LBB15_22
.LBB15_23:
	stw r1+%lo(fio_len), r7
	add r6, r7, r0
.LBB15_24:
	add r9, r7, r4
	stb r9+0, r5
	addi r9, r7, 1
	stw r14+%lo(fio_col), r9
	blt r7, r6, .LBB15_18
.LBB15_25:
	stw r1+%lo(fio_len), r9
	add r6, r9, r0
	jal r0, .LBB15_18
.LBB15_26:
	flt.d r3, r12, r18
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	lui r5, %hi(.L.str.102)
	addi r5, r5, %lo(.L.str.102)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.10)
	addi r4, r4, %lo(.L.str.10)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r3, r3, r1
.LBB15_27:
	andi r1, r17, 9
	addi r4, r1, 16
	jal r31, fio_field
.LBB15_28:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	ldw r22, sp+96
	ldw r21, sp+100
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
.LBB15_29:
	addi r6, r21, -1
	lui r1, %hi(.L.str.103)
	addi r1, r1, %lo(.L.str.103)
	addi r11, sp, 16
	addi r4, r0, 16
	add r3, r11, r0
	add r15, r5, r0
	add r5, r1, r0
	jal r31, snprintf
	addi r14, sp, 32
	addi r4, r0, 64
	add r3, r14, r0
	add r5, r11, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	addi r4, r0, 69
	add r3, r14, r0
	jal r31, strchr
	add r3, r14, r0
	beq r15, r0, .LBB15_27
.LBB15_30:
	beq r1, r0, .LBB15_27
.LBB15_31:
	addi r11, r1, 2
	add r3, r11, r0
	jal r31, strlen
	addi r3, sp, 32
	addi r4, r0, 2
	bgt r1, r4, .LBB15_27
.LBB15_32:
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
	addi r3, sp, 32
	jal r0, .LBB15_27
.Lfunc_end15:
	.size	fio_list_out_real, .Lfunc_end15-fio_list_out_real
                                        # -- End function
	.p2align	2                               # -- Begin function fio_drop_zero
	.type	fio_drop_zero,@function
fio_drop_zero:                          # @fio_drop_zero
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	add r12, r4, r0
	add r11, r3, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB16_11
.LBB16_1:
	addi r3, r12, 1
	bne r1, r3, .LBB16_11
.LBB16_2:
	ldbu r3, r11+0
	addi r4, r0, 43
	beq r3, r4, .LBB16_7
.LBB16_3:
	addi r4, r0, 45
	beq r3, r4, .LBB16_7
.LBB16_4:
	addi r4, r0, 48
	bne r3, r4, .LBB16_11
.LBB16_5:
	ldbu r3, r11+1
	addi r4, r0, 46
	bne r3, r4, .LBB16_11
.LBB16_6:
	addi r4, r11, 1
	add r3, r11, r0
	add r5, r1, r0
	jal r0, .LBB16_10
.LBB16_7:
	ldbu r3, r11+1
	addi r4, r0, 48
	bne r3, r4, .LBB16_11
.LBB16_8:
	ldbu r3, r11+2
	addi r4, r0, 46
	bne r3, r4, .LBB16_11
.LBB16_9:
	addi r3, r11, 1
	addi r4, r11, 2
	addi r5, r1, -1
.LBB16_10:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, memmove
.LBB16_11:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end16:
	.size	fio_drop_zero, .Lfunc_end16-fio_drop_zero
                                        # -- End function
	.p2align	2                               # -- Begin function fio_blanks
	.type	fio_blanks,@function
fio_blanks:                             # @fio_blanks
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	addi r1, r0, 1
	blt r3, r1, .LBB17_10
.LBB17_1:
	lui r4, %hi(fio_len)
	ldw r9, r4+%lo(fio_len)
	lui r5, %hi(fio_col)
	ldw r10, r5+%lo(fio_col)
	addi r6, r0, 1022
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
	jal r0, .LBB17_4
.LBB17_2:
	add r10, r3, r0
.LBB17_3:
	addi r3, lr, -1
	ble lr, r1, .LBB17_10
.LBB17_4:
	add lr, r3, r0
	bgt r10, r6, .LBB17_3
.LBB17_5:
	bge r9, r10, .LBB17_8
.LBB17_6:
	addi r3, r9, 1
	add r9, r9, r7
	stb r9+0, r8
	add r9, r3, r0
	bne r10, r3, .LBB17_6
.LBB17_7:
	stw r4+%lo(fio_len), r10
	add r9, r10, r0
.LBB17_8:
	add r3, r10, r7
	stb r3+0, r8
	addi r3, r10, 1
	stw r5+%lo(fio_col), r3
	blt r10, r9, .LBB17_2
.LBB17_9:
	stw r4+%lo(fio_len), r3
	add r9, r3, r0
	jal r0, .LBB17_2
.LBB17_10:
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end17:
	.size	fio_blanks, .Lfunc_end17-fio_blanks
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_efmt
	.type	.LCPI18_0,@object
.LCPI18_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI18_0, 8
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
	lui r5, %hi(.L.str.103)
	addi r5, r5, %lo(.L.str.103)
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
	ldbu r22, sp+32
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
	jal r0, .LBB18_3
.LBB18_1:
	addi r7, sp, 32
	add r7, r7, r15
	stb r7+40, r6
	addi r15, r15, 1
.LBB18_2:
	addi r3, r3, 1
.LBB18_3:
	ldbu r6, r3+-1
	bgt r6, r1, .LBB18_6
.LBB18_4:
	beq r6, r16, .LBB18_2
.LBB18_5:
	bne r6, r0, .LBB18_1
	jal r0, .LBB18_9
.LBB18_6:
	beq r6, r4, .LBB18_8
.LBB18_7:
	bne r6, r5, .LBB18_1
.LBB18_8:
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	addi r1, r1, 1
	jal r0, .LBB18_10
.LBB18_9:
	add r1, r17, r0
.LBB18_10:
	lui r3, %hi(.LCPI18_0)
	addi r3, r3, %lo(.LCPI18_0)
	ldw r5, r3+4
	ldw r4, r3+0
	feq.d r3, r12, r4
	sub r3, r0, r3
	and r3, r1, r3
	add r6, r14, r0
	bne r22, r21, .LBB18_12
.LBB18_11:
	addi r6, r14, 1
	stb r14+0, r21
.LBB18_12:
	xor r1, r1, r3
	blt r20, r17, .LBB18_17
.LBB18_13:
	blt r15, r17, .LBB18_22
.LBB18_14:
	xor r3, r19, r15
	slt r7, r19, r15
	sub r7, r0, r7
	and r3, r3, r7
	xor r3, r15, r3
	addi r7, sp, 32
	addi r8, r7, 40
	add r7, r0, r0
.LBB18_15:
	add r9, r6, r7
	add r10, r8, r7
	ldbu r10, r10+0
	stb r9+0, r10
	addi r7, r7, 1
	bne r3, r7, .LBB18_15
.LBB18_16:
	add r6, r6, r7
	jal r0, .LBB18_23
.LBB18_17:
	addi r8, r0, 48
	stb r6+0, r8
	addi r9, r6, 2
	stb r6+1, r16
	addi r7, r0, 55
	beq r20, r0, .LBB18_28
.LBB18_18:
	sub r3, r9, r14
	bgt r3, r7, .LBB18_28
.LBB18_19:
	sub r3, r6, r14
	addi lr, r3, 3
	addi r6, r0, -2
	addi r10, r0, 56
.LBB18_20:
	addi r3, r9, 1
	stb r9+0, r8
	bgt r20, r6, .LBB18_29
.LBB18_21:
	add r16, lr, r0
	addi r20, r20, 1
	addi lr, lr, 1
	add r9, r3, r0
	blt r16, r10, .LBB18_20
	jal r0, .LBB18_29
.LBB18_22:
	add r7, r0, r0
.LBB18_23:
	stb r6+0, r16
	addi r3, r6, 1
	bge r7, r15, .LBB18_35
.LBB18_24:
	sub r8, r3, r14
	addi r9, r0, 55
	bgt r8, r9, .LBB18_35
.LBB18_25:
	sub r6, r6, r14
	addi r9, r6, 2
	addi r6, sp, 32
	addi r6, r6, 40
	addi r8, r0, 56
.LBB18_26:
	add r10, r9, r0
	add r9, r6, r7
	ldbu r9, r9+0
	stb r3+0, r9
	addi r7, r7, 1
	addi r3, r3, 1
	bge r7, r15, .LBB18_35
.LBB18_27:
	addi r9, r10, 1
	blt r10, r8, .LBB18_26
	jal r0, .LBB18_35
.LBB18_28:
	add r3, r9, r0
.LBB18_29:
	blt r15, r17, .LBB18_35
.LBB18_30:
	sub r8, r3, r14
	bgt r8, r7, .LBB18_35
.LBB18_31:
	addi r6, sp, 32
	addi r6, r6, 40
	addi r7, r8, 1
	addi r8, r0, 56
	add r9, r0, r0
.LBB18_32:
	add r10, r3, r9
	add lr, r6, r9
	ldbu lr, lr+0
	stb r10+0, lr
	addi r10, r9, 1
	bge r10, r15, .LBB18_34
.LBB18_33:
	add lr, r7, r9
	add r9, r10, r0
	blt lr, r8, .LBB18_32
.LBB18_34:
	add r3, r3, r10
.LBB18_35:
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
	bne r4, r0, .LBB18_38
.LBB18_36:
	addi r1, r5, -100
	addi r8, r0, 900
	bgeu r1, r8, .LBB18_38
.LBB18_37:
	add r1, r3, r0
	jal r0, .LBB18_39
.LBB18_38:
	addi r1, r3, 1
	stb r3+0, r11
.LBB18_39:
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
.LBB18_40:
	add r10, r3, r0
	mulhu r3, r5, r7
	srli lr, r3, 3
	mul r3, lr, r8
	sub r3, r5, r3
	ori  r11, r3, 48
	addi r3, r10, 1
	addi r12, sp, 96
	add r12, r12, r10
	stb r12+0, r11
	bltu r5, r8, .LBB18_42
.LBB18_41:
	add r5, lr, r0
	bltu r10, r9, .LBB18_40
.LBB18_42:
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
	addi r5, sp, 96
	bge r3, r4, .LBB18_46
.LBB18_43:
	add r5, r5, r3
	addi r6, r0, 48
	add r7, r4, r0
.LBB18_44:
	stb r5+0, r6
	addi r5, r5, 1
	addi r7, r7, -1
	bne r3, r7, .LBB18_44
.LBB18_45:
	add r3, r4, r0
.LBB18_46:
	addi r1, r1, 1
	addi r4, sp, 96
	addi r4, r4, -1
.LBB18_47:
	add r5, r3, r0
	addi r3, r3, -1
	add r6, r4, r5
	ldbu r6, r6+0
	stb r1+0, r6
	addi r1, r1, 1
	bgt r5, r17, .LBB18_47
.LBB18_48:
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
.Lfunc_end18:
	.size	fio_efmt, .Lfunc_end18-fio_efmt
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_esn
	.type	.LCPI19_0,@object
.LCPI19_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI19_0, 8
	.text
	.p2align	2
	.type	fio_esn,@function
fio_esn:                                # @fio_esn
# %bb.0:
	addi sp, sp, -224
	stw sp+0, lr
	stw sp+220, r11
	stw sp+216, r12
	stw sp+212, r13
	stw sp+208, r14
	stw sp+204, r15
	stw sp+200, r16
	stw sp+196, r17
	stw sp+192, r18
	stw sp+188, r19
	stw sp+184, r20
	stw sp+180, r21
	stw sp+176, r22
	stw sp+172, r23
	stw sp+168, r24
	stw sp+164, r26
	stw sp+160, r27
	add r13, r7, r0
	add r15, r6, r0
	add r14, r5, r0
	add r12, r3, r0
	addi r19, r0, 0
	sgt r1, r4, r19
	sub r1, r0, r1
	and r1, r4, r1
	xori r3, r1, 30
	addi r4, r0, 30
	slt r1, r1, r4
	sub r1, r0, r1
	and r17, r3, r1
	lui r5, %hi(.L.str.104)
	addi r5, r5, %lo(.L.str.104)
	addi r16, sp, 80
	addi r4, r0, 64
	add r3, r16, r0
	add r6, r14, r0
	add r7, r15, r0
	jal r31, snprintf
	addi r11, r0, 69
	add r3, r16, r0
	add r4, r11, r0
	jal r31, strchr
	add r16, r0, r0
	lui r3, %hi(.LCPI19_0)
	addi r3, r3, %lo(.LCPI19_0)
	ldw r27, r3+4
	ldw r26, r3+0
	feq.d r3, r14, r26
	bne r3, r0, .LBB19_3
.LBB19_1:
	beq r1, r0, .LBB19_3
.LBB19_2:
	addi r3, r1, 1
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	add r16, r1, r0
.LBB19_3:
	xori r24, r17, 30
	lui r20, 699051
	beq r13, r0, .LBB19_6
.LBB19_4:
	addi r1, r0, -2
	blt r16, r0, .LBB19_7
.LBB19_5:
	addi r3, r20, -1365
	mulhu r3, r16, r3
	and r1, r3, r1
	srli r3, r3, 1
	add r1, r1, r3
	sub r1, r16, r1
	sub r1, r1, r16
	jal r0, .LBB19_8
.LBB19_6:
	addi r21, r0, 1
	jal r0, .LBB19_9
.LBB19_7:
	addi r3, r0, 2
	sub r3, r3, r16
	addi r4, r20, -1365
	mulhu r3, r3, r4
	and r1, r3, r1
	srli r3, r3, 1
	add r1, r1, r3
.LBB19_8:
	add r1, r16, r1
	addi r21, r1, 1
.LBB19_9:
	add r1, r24, r21
	addi r6, r1, -1
	lui r5, %hi(.L.str.103)
	addi r5, r5, %lo(.L.str.103)
	addi r17, sp, 64
	addi r4, r0, 16
	add r3, r17, r0
	jal r31, snprintf
	addi r18, sp, 80
	addi r4, r0, 64
	add r3, r18, r0
	add r5, r17, r0
	add r6, r14, r0
	add r7, r15, r0
	jal r31, snprintf
	ldbu r23, sp+80
	addi r22, r0, 45
	seq r1, r23, r22
	or  r1, r1, r18
	addi r3, r1, 1
	addi r18, r0, 46
	add r17, r0, r0
	jal r0, .LBB19_11
.LBB19_10:
	addi r3, r3, 1
.LBB19_11:
	ldbu r1, r3+-1
	beq r1, r0, .LBB19_17
.LBB19_12:
	beq r1, r11, .LBB19_16
.LBB19_13:
	beq r1, r18, .LBB19_10
.LBB19_14:
	bgt r17, r18, .LBB19_10
.LBB19_15:
	addi r4, r17, 1
	addi r5, sp, 16
	add r5, r5, r17
	stb r5+0, r1
	add r17, r4, r0
	jal r0, .LBB19_10
.LBB19_16:
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	jal r0, .LBB19_18
.LBB19_17:
	add r1, r0, r0
.LBB19_18:
	feq.d r3, r14, r26
	sub r3, r0, r3
	and r3, r1, r3
	xor r1, r1, r3
	beq r13, r0, .LBB19_28
.LBB19_19:
	beq r1, r16, .LBB19_26
.LBB19_20:
	blt r1, r0, .LBB19_22
.LBB19_21:
	addi r3, r20, -1365
	mulhu r3, r1, r3
	addi r4, r0, -2
	and r4, r3, r4
	srli r3, r3, 1
	add r3, r4, r3
	sub r3, r1, r3
	sub r3, r3, r1
	jal r0, .LBB19_23
.LBB19_22:
	addi r3, r0, 2
	sub r3, r3, r1
	addi r4, r20, -1365
	mulhu r3, r3, r4
	addi r4, r0, -2
	and r4, r3, r4
	srli r3, r3, 1
	add r3, r4, r3
.LBB19_23:
	add r3, r1, r3
	addi r21, r3, 1
	add r3, r21, r24
	xori r4, r3, 47
	addi r5, r0, 47
	slt r3, r3, r5
	sub r3, r0, r3
	and r3, r4, r3
	xori r3, r3, 47
	bge r17, r3, .LBB19_26
.LBB19_24:
	addi r4, r0, 48
.LBB19_25:
	add r5, r17, r0
	addi r17, r17, 1
	addi r6, sp, 16
	add r5, r6, r5
	stb r5+0, r4
	bne r3, r17, .LBB19_25
.LBB19_26:
	blt r1, r0, .LBB19_53
.LBB19_27:
	addi r3, r20, -1365
	mulhu r1, r1, r3
	addi r3, r0, -2
	and r3, r1, r3
	srli r1, r1, 1
	add r1, r3, r1
.LBB19_28:
	add r4, r12, r0
	bne r23, r22, .LBB19_30
.LBB19_29:
	addi r4, r12, 1
	stb r12+0, r22
.LBB19_30:
	xor r3, r21, r17
	slt r5, r21, r17
	sub r5, r0, r5
	and r3, r3, r5
	xor r5, r17, r3
	add r6, r0, r0
	addi r3, r0, 1
	blt r5, r3, .LBB19_33
.LBB19_31:
	add r7, r4, r6
	addi r8, sp, 16
	add r8, r8, r6
	ldbu r8, r8+0
	stb r7+0, r8
	addi r6, r6, 1
	bne r5, r6, .LBB19_31
.LBB19_32:
	add r4, r4, r6
.LBB19_33:
	stb r4+0, r18
	addi r5, r4, 1
	bge r6, r17, .LBB19_39
.LBB19_34:
	sub r7, r5, r12
	addi r8, r0, 55
	bgt r7, r8, .LBB19_39
.LBB19_35:
	sub r4, r4, r12
	addi r7, r4, 2
	addi r4, r0, 56
.LBB19_36:
	add r8, r7, r0
	addi r7, sp, 16
	add r7, r7, r6
	ldbu r7, r7+0
	stb r5+0, r7
	addi r6, r6, 1
	addi r5, r5, 1
	bge r6, r17, .LBB19_38
.LBB19_37:
	addi r7, r8, 1
	blt r8, r4, .LBB19_36
.LBB19_38:
	addi r4, r5, -1
.LBB19_39:
	srai r6, r1, 31
	xor r7, r1, r6
	sub r7, r7, r6
	lui r6, %hi(fio_e)
	ldw r6, r6+%lo(fio_e)
	addi r8, r0, 100
	sltu r8, r7, r8
	bne r6, r0, .LBB19_42
.LBB19_40:
	addi r9, r7, -100
	addi r10, r0, 900
	bgeu r9, r10, .LBB19_42
.LBB19_41:
	add r4, r5, r0
	jal r0, .LBB19_43
.LBB19_42:
	addi r4, r4, 2
	stb r5+0, r11
.LBB19_43:
	xori r5, r8, 3
	slt r1, r1, r19
	sub r1, r0, r1
	andi r1, r1, 6
	xori r1, r1, 43
	stb r4+0, r1
	lui r1, 838861
	addi r8, r1, -819
	addi r9, r0, 10
	addi r10, r0, 14
	add r1, r0, r0
.LBB19_44:
	add lr, r1, r0
	mulhu r1, r7, r8
	srli r11, r1, 3
	mul r1, r11, r9
	sub r1, r7, r1
	ori  r12, r1, 48
	addi r1, lr, 1
	addi r13, sp, 144
	add r13, r13, lr
	stb r13+0, r12
	bltu r7, r9, .LBB19_46
.LBB19_45:
	add r7, r11, r0
	bltu lr, r10, .LBB19_44
.LBB19_46:
	sgt r7, r6, r19
	addi r8, r0, 15
	slt r8, r6, r8
	sub r8, r0, r8
	xori r6, r6, 15
	and r6, r6, r8
	xor r6, r6, r5
	sub r7, r0, r7
	xori r6, r6, 15
	and r6, r6, r7
	xor r5, r5, r6
	addi r6, sp, 144
	bge r1, r5, .LBB19_50
.LBB19_47:
	add r6, r6, r1
	addi r7, r0, 48
	add r8, r5, r0
.LBB19_48:
	stb r6+0, r7
	addi r6, r6, 1
	addi r8, r8, -1
	bne r1, r8, .LBB19_48
.LBB19_49:
	add r1, r5, r0
.LBB19_50:
	addi r4, r4, 1
	addi r5, sp, 144
	addi r5, r5, -1
.LBB19_51:
	add r6, r1, r0
	addi r1, r1, -1
	add r7, r5, r6
	ldbu r7, r7+0
	stb r4+0, r7
	addi r4, r4, 1
	bgt r6, r3, .LBB19_51
.LBB19_52:
	stb r4+0, r0
	ldw r27, sp+160
	ldw r26, sp+164
	ldw r24, sp+168
	ldw r23, sp+172
	ldw r22, sp+176
	ldw r21, sp+180
	ldw r20, sp+184
	ldw r19, sp+188
	ldw r18, sp+192
	ldw r17, sp+196
	ldw r16, sp+200
	ldw r15, sp+204
	ldw r14, sp+208
	ldw r13, sp+212
	ldw r12, sp+216
	ldw r11, sp+220
	ldw lr, sp+0
	addi sp, sp, 224
	jalr r0, r31, 0
.LBB19_53:
	addi r3, r0, 2
	sub r1, r3, r1
	addi r3, r20, -1365
	mulhu r3, r1, r3
	addi r4, r0, -2
	and r4, r3, r4
	srli r3, r3, 1
	add r3, r4, r3
	sub r3, r1, r3
	sub r1, r3, r1
	add r4, r12, r0
	beq r23, r22, .LBB19_29
	jal r0, .LBB19_30
.Lfunc_end19:
	.size	fio_esn, .Lfunc_end19-fio_esn
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
	beq r1, r0, .LBB20_2
.LBB20_1:
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
.LBB20_2:
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB20_7
.LBB20_3:
	ldw r4, r12+%lo(fio_rep)
	ble r4, r0, .LBB20_7
.LBB20_4:
	addi r4, r0, 65
	bne r1, r4, .LBB20_8
.LBB20_5:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, r4, -1
	addi r5, r0, 3
	lui r1, %hi(fio_col)
	bgeu r3, r5, .LBB20_9
.LBB20_6:
	add r3, r4, r0
	jal r0, .LBB20_11
.LBB20_7:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	jal r31, fio_next_desc
	add r3, r13, r0
	addi r4, r0, 65
	beq r1, r4, .LBB20_5
.LBB20_8:
	fcvt.d.s r4, r3
	add r3, r4, r0
	add r4, r5, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_wr_d
.LBB20_9:
	addi r3, r0, 4
	addi r5, r0, 5
	blt r4, r5, .LBB20_11
.LBB20_10:
	ldw r5, r1+%lo(fio_col)
	add r4, r4, r5
	addi r4, r4, -4
	stw r1+%lo(fio_col), r4
.LBB20_11:
	lui r4, %hi(fio_len)
	ldw r8, r4+%lo(fio_len)
	ldw r10, r1+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	add r9, r0, r0
	jal r0, .LBB20_14
.LBB20_12:
	add r10, lr, r0
.LBB20_13:
	addi r9, r9, 1
	beq r9, r3, .LBB20_20
.LBB20_14:
	addi lr, sp, 16
	bgt r10, r5, .LBB20_13
.LBB20_15:
	bge r8, r10, .LBB20_18
.LBB20_16:
	addi r13, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, r13, r0
	bne r10, r13, .LBB20_16
.LBB20_17:
	stw r4+%lo(fio_len), r10
	add r8, r10, r0
.LBB20_18:
	add lr, lr, r9
	ldbu lr, lr+0
	add r13, r10, r6
	stb r13+0, lr
	addi lr, r10, 1
	stw r1+%lo(fio_col), lr
	blt r10, r8, .LBB20_12
.LBB20_19:
	stw r4+%lo(fio_len), lr
	add r8, lr, r0
	jal r0, .LBB20_12
.LBB20_20:
	ldw r1, r12+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB20_22
.LBB20_21:
	addi r1, r1, -1
	stw r12+%lo(fio_rep), r1
.LBB20_22:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB20_24
.LBB20_23:
	stw r11+%lo(fio_desc), r0
.LBB20_24:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end20:
	.size	f77_wr_r, .Lfunc_end20-f77_wr_r
                                        # -- End function
	.globl	f77_wr_c                        # -- Begin function f77_wr_c
	.p2align	2
	.type	f77_wr_c,@function
f77_wr_c:                               # @f77_wr_c
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	add r11, r4, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	fcvt.d.s r4, r3
	beq r1, r0, .LBB21_2
.LBB21_1:
	fcvt.d.s r6, r11
	add r3, r4, r0
	add r4, r5, r0
	add r5, r6, r0
	add r6, r7, r0
	add r7, r0, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fio_list_cplx
.LBB21_2:
	add r3, r4, r0
	add r4, r5, r0
	jal r31, f77_wr_d
	fcvt.d.s r4, r11
	add r3, r4, r0
	add r4, r5, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_wr_d
.Lfunc_end21:
	.size	f77_wr_c, .Lfunc_end21-f77_wr_c
                                        # -- End function
	.p2align	2                               # -- Begin function fio_list_cplx
	.type	fio_list_cplx,@function
fio_list_cplx:                          # @fio_list_cplx
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
	add r11, r7, r0
	add r12, r6, r0
	add r13, r5, r0
	lui r14, %hi(fio_col)
	ldw r1, r14+%lo(fio_col)
	addi r17, r0, 1022
	lui r15, %hi(fio_len)
	lui r16, %hi(fio_line)
	addi r16, r16, %lo(fio_line)
	bgt r1, r17, .LBB22_6
.LBB22_1:
	ldw r6, r15+%lo(fio_len)
	addi r5, r0, 32
	bge r6, r1, .LBB22_4
.LBB22_2:
	addi r7, r6, 1
	add r6, r6, r16
	stb r6+0, r5
	add r6, r7, r0
	bne r1, r7, .LBB22_2
.LBB22_3:
	stw r15+%lo(fio_len), r1
	add r6, r1, r0
.LBB22_4:
	add r7, r1, r16
	stb r7+0, r5
	addi r5, r1, 1
	stw r14+%lo(fio_col), r5
	blt r1, r6, .LBB22_6
.LBB22_5:
	stw r15+%lo(fio_len), r5
.LBB22_6:
	ldw r18, r15+%lo(fio_len)
	ldw r1, r14+%lo(fio_col)
	bgt r1, r17, .LBB22_13
.LBB22_7:
	add r5, r18, r0
	bge r18, r1, .LBB22_11
.LBB22_8:
	addi r5, r0, 32
	add r6, r18, r0
.LBB22_9:
	addi r7, r6, 1
	add r6, r6, r16
	stb r6+0, r5
	add r6, r7, r0
	bne r1, r7, .LBB22_9
.LBB22_10:
	stw r15+%lo(fio_len), r1
	add r5, r1, r0
.LBB22_11:
	add r6, r1, r16
	addi r7, r0, 40
	stb r6+0, r7
	addi r6, r1, 1
	stw r14+%lo(fio_col), r6
	blt r1, r5, .LBB22_13
.LBB22_12:
	stw r15+%lo(fio_len), r6
.LBB22_13:
	ldw r20, r15+%lo(fio_len)
	add r5, r11, r0
	jal r31, fio_list_out_real
	ldw r1, r15+%lo(fio_len)
	add r3, r20, r0
	bge r20, r1, .LBB22_18
.LBB22_14:
	addi r4, r0, 32
	add r3, r20, r0
.LBB22_15:
	add r5, r3, r16
	ldbu r5, r5+0
	bne r5, r4, .LBB22_18
.LBB22_16:
	addi r3, r3, 1
	bne r1, r3, .LBB22_15
.LBB22_17:
	add r3, r1, r0
.LBB22_18:
	ble r1, r3, .LBB22_23
.LBB22_19:
	lui r4, %hi(fio_line-1)
	addi r4, r4, %lo(fio_line-1)
	addi r5, r0, 32
.LBB22_20:
	add r6, r1, r4
	ldbu r6, r6+0
	bne r6, r5, .LBB22_23
.LBB22_21:
	addi r1, r1, -1
	bgt r1, r3, .LBB22_20
.LBB22_22:
	add r1, r3, r0
.LBB22_23:
	sub r4, r1, r3
	addi r19, r0, 1
	blt r4, r19, .LBB22_26
.LBB22_24:
	add r5, r20, r16
.LBB22_25:
	add r6, r3, r16
	ldbu r6, r6+0
	stb r5+0, r6
	addi r3, r3, 1
	addi r5, r5, 1
	bne r1, r3, .LBB22_25
.LBB22_26:
	add r1, r4, r20
	stw r15+%lo(fio_len), r1
	stw r14+%lo(fio_col), r1
	bgt r1, r17, .LBB22_28
.LBB22_27:
	add r3, r1, r16
	addi r4, r0, 44
	stb r3+0, r4
	addi r1, r1, 1
	stw r14+%lo(fio_col), r1
	stw r15+%lo(fio_len), r1
.LBB22_28:
	ldw r20, r15+%lo(fio_len)
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, fio_list_out_real
	ldw r1, r15+%lo(fio_len)
	add r3, r20, r0
	bge r20, r1, .LBB22_33
.LBB22_29:
	addi r4, r0, 32
	add r3, r20, r0
.LBB22_30:
	add r5, r3, r16
	ldbu r5, r5+0
	bne r5, r4, .LBB22_33
.LBB22_31:
	addi r3, r3, 1
	bne r1, r3, .LBB22_30
.LBB22_32:
	add r3, r1, r0
.LBB22_33:
	ble r1, r3, .LBB22_38
.LBB22_34:
	lui r4, %hi(fio_line-1)
	addi r4, r4, %lo(fio_line-1)
	addi r5, r0, 32
.LBB22_35:
	add r6, r1, r4
	ldbu r6, r6+0
	bne r6, r5, .LBB22_38
.LBB22_36:
	addi r1, r1, -1
	bgt r1, r3, .LBB22_35
.LBB22_37:
	add r1, r3, r0
.LBB22_38:
	sub r4, r1, r3
	blt r4, r19, .LBB22_41
.LBB22_39:
	add r5, r20, r16
.LBB22_40:
	add r6, r3, r16
	ldbu r6, r6+0
	stb r5+0, r6
	addi r3, r3, 1
	addi r5, r5, 1
	bne r1, r3, .LBB22_40
.LBB22_41:
	add r1, r4, r20
	stw r15+%lo(fio_len), r1
	stw r14+%lo(fio_col), r1
	bgt r1, r17, .LBB22_43
.LBB22_42:
	add r3, r1, r16
	addi r4, r0, 41
	stb r3+0, r4
	addi r1, r1, 1
	stw r14+%lo(fio_col), r1
	stw r15+%lo(fio_len), r1
.LBB22_43:
	ldw r1, r15+%lo(fio_len)
	sub r5, r1, r18
	addi r3, r0, 0
	seq r3, r11, r3
	sub r3, r0, r3
	andi r3, r3, 22
	xori r3, r3, 53
	sub r11, r3, r5
	blt r11, r19, .LBB22_46
.LBB22_44:
	add r1, r11, r1
	bgt r1, r17, .LBB22_46
.LBB22_45:
	add r12, r18, r16
	add r3, r12, r11
	add r4, r12, r0
	jal r31, memmove
	addi r4, r0, 32
	add r3, r12, r0
	add r5, r11, r0
	jal r31, memset
	ldw r1, r15+%lo(fio_len)
	add r1, r1, r11
	stw r15+%lo(fio_len), r1
.LBB22_46:
	ldw r1, r15+%lo(fio_len)
	stw r14+%lo(fio_col), r1
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
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
.Lfunc_end22:
	.size	fio_list_cplx, .Lfunc_end22-fio_list_cplx
                                        # -- End function
	.globl	f77_wr_z                        # -- Begin function f77_wr_z
	.p2align	2
	.type	f77_wr_z,@function
f77_wr_z:                               # @f77_wr_z
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	add r1, r3, r0
	ldw r4, r3+4
	ldw r3, r3+0
	ldw r11, r1+12
	ldw r12, r1+8
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB23_2
.LBB23_1:
	addi r7, r0, 1
	add r5, r12, r0
	add r6, r11, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fio_list_cplx
.LBB23_2:
	jal r31, f77_wr_d
	add r3, r12, r0
	add r4, r11, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_wr_d
.Lfunc_end23:
	.size	f77_wr_z, .Lfunc_end23-f77_wr_z
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
	beq r1, r0, .LBB24_20
.LBB24_1:
	lui r1, %hi(fio_first_item)
	ldbu r7, r1+%lo(fio_first_item)
	lui r5, %hi(fio_prev_char)
	lui r6, %hi(fio_col)
	bne r7, r0, .LBB24_3
.LBB24_2:
	ldbu r7, r5+%lo(fio_prev_char)
	andi r7, r7, 1
	bne r7, r0, .LBB24_9
.LBB24_3:
	ldw r7, r6+%lo(fio_col)
	addi r8, r0, 1022
	bgt r7, r8, .LBB24_9
.LBB24_4:
	lui r8, %hi(fio_len)
	ldw r9, r8+%lo(fio_len)
	lui r10, %hi(fio_line)
	addi r10, r10, %lo(fio_line)
	addi lr, r0, 32
	bge r9, r7, .LBB24_7
.LBB24_5:
	addi r11, r9, 1
	add r9, r9, r10
	stb r9+0, lr
	add r9, r11, r0
	bne r7, r11, .LBB24_5
.LBB24_6:
	stw r8+%lo(fio_len), r7
	add r9, r7, r0
.LBB24_7:
	add r10, r7, r10
	stb r10+0, lr
	addi r10, r7, 1
	stw r6+%lo(fio_col), r10
	blt r7, r9, .LBB24_9
.LBB24_8:
	stw r8+%lo(fio_len), r10
.LBB24_9:
	addi r7, r0, 1
	blt r4, r7, .LBB24_19
.LBB24_10:
	lui r8, %hi(fio_len)
	ldw r11, r8+%lo(fio_len)
	ldw r13, r6+%lo(fio_col)
	addi r9, r0, 1022
	lui r10, %hi(fio_line)
	addi r10, r10, %lo(fio_line)
	addi lr, r0, 32
	add r12, r0, r0
	jal r0, .LBB24_13
.LBB24_11:
	add r13, r14, r0
.LBB24_12:
	addi r12, r12, 1
	beq r12, r4, .LBB24_19
.LBB24_13:
	bgt r13, r9, .LBB24_12
.LBB24_14:
	add r14, r3, r12
	ldbu r14, r14+0
	bge r11, r13, .LBB24_17
.LBB24_15:
	addi r15, r11, 1
	add r11, r11, r10
	stb r11+0, lr
	add r11, r15, r0
	bne r13, r15, .LBB24_15
.LBB24_16:
	stw r8+%lo(fio_len), r13
	add r11, r13, r0
.LBB24_17:
	add r15, r13, r10
	stb r15+0, r14
	addi r14, r13, 1
	stw r6+%lo(fio_col), r14
	blt r13, r11, .LBB24_11
.LBB24_18:
	stw r8+%lo(fio_len), r14
	add r11, r14, r0
	jal r0, .LBB24_11
.LBB24_19:
	stb r1+%lo(fio_first_item), r0
	stb r5+%lo(fio_prev_char), r7
	jal r0, .LBB24_43
.LBB24_20:
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB24_22
.LBB24_21:
	ldw r1, r12+%lo(fio_rep)
	bgt r1, r0, .LBB24_23
.LBB24_22:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	add r14, r4, r0
	jal r31, fio_next_desc
	add r4, r14, r0
	add r3, r13, r0
	beq r1, r0, .LBB24_43
.LBB24_23:
	lui r1, %hi(fio_w)
	ldw r5, r1+%lo(fio_w)
	addi r1, r0, 1
	blt r5, r1, .LBB24_26
.LBB24_24:
	bge r5, r4, .LBB24_26
.LBB24_25:
	add r4, r5, r0
	jal r0, .LBB24_29
.LBB24_26:
	ble r5, r4, .LBB24_29
.LBB24_27:
	sub r5, r5, r4
	blt r5, r1, .LBB24_29
.LBB24_28:
	lui r6, %hi(fio_col)
	ldw r7, r6+%lo(fio_col)
	add r5, r7, r5
	stw r6+%lo(fio_col), r5
.LBB24_29:
	blt r4, r1, .LBB24_39
.LBB24_30:
	lui r5, %hi(fio_len)
	ldw r9, r5+%lo(fio_len)
	lui r6, %hi(fio_col)
	ldw r13, r6+%lo(fio_col)
	addi r7, r0, 1022
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	addi r10, r0, 32
	add lr, r0, r0
	jal r0, .LBB24_33
.LBB24_31:
	add r13, r14, r0
.LBB24_32:
	addi lr, lr, 1
	beq lr, r4, .LBB24_39
.LBB24_33:
	bgt r13, r7, .LBB24_32
.LBB24_34:
	add r14, r3, lr
	ldbu r14, r14+0
	bge r9, r13, .LBB24_37
.LBB24_35:
	addi r15, r9, 1
	add r9, r9, r8
	stb r9+0, r10
	add r9, r15, r0
	bne r13, r15, .LBB24_35
.LBB24_36:
	stw r5+%lo(fio_len), r13
	add r9, r13, r0
.LBB24_37:
	add r15, r13, r8
	stb r15+0, r14
	addi r14, r13, 1
	stw r6+%lo(fio_col), r14
	blt r13, r9, .LBB24_31
.LBB24_38:
	stw r5+%lo(fio_len), r14
	add r9, r14, r0
	jal r0, .LBB24_31
.LBB24_39:
	ldw r3, r12+%lo(fio_rep)
	blt r3, r1, .LBB24_41
.LBB24_40:
	addi r1, r3, -1
	stw r12+%lo(fio_rep), r1
.LBB24_41:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB24_43
.LBB24_42:
	stw r11+%lo(fio_desc), r0
.LBB24_43:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end24:
	.size	f77_wr_a, .Lfunc_end24-f77_wr_a
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
.Lfunc_end25:
	.size	f77_cabs, .Lfunc_end25-f77_cabs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cdiv
	.type	.LCPI26_0,@object
.LCPI26_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI26_0, 8
	.type	.LCPI26_1,@object
.LCPI26_1:
	.quad	0x43f0000000000000              # double 1.8446744073709552E+19
	.size	.LCPI26_1, 8
	.type	.LCPI26_2,@object
.LCPI26_2:
	.quad	0x3bf0000000000000              # double 5.4210108624275222E-20
	.size	.LCPI26_2, 8
	.type	.LCPI26_3,@object
.LCPI26_3:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI26_3, 8
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
	lui r1, %hi(.LCPI26_0)
	addi r1, r1, %lo(.LCPI26_0)
	ldw r5, r1+4
	ldw r4, r1+0
	lui r1, %hi(.LCPI26_1)
	addi r1, r1, %lo(.LCPI26_1)
	ldw r9, r1+4
	ldw r8, r1+0
	flt.d r1, r8, r6
	xori r3, r1, 1
	lui r1, %hi(.LCPI26_2)
	addi r1, r1, %lo(.LCPI26_2)
	bne r3, r0, .LBB26_2
.LBB26_1:
	ldw r19, r1+4
	ldw r18, r1+0
	fmul.d r14, r14, r18
	fmul.d r16, r16, r18
	fmul.d r6, r6, r18
	fmul.d r4, r4, r18
	flt.d r3, r8, r6
	bne r3, r0, .LBB26_1
.LBB26_2:
	lui r3, %hi(.LCPI26_3)
	addi r3, r3, %lo(.LCPI26_3)
	ldw r19, r3+4
	ldw r18, r3+0
	flt.d r3, r18, r6
	xori r3, r3, 1
	bne r3, r0, .LBB26_6
.LBB26_3:
	ldw r21, r1+4
	ldw r20, r1+0
	flt.d r1, r6, r20
	xori r1, r1, 1
	bne r1, r0, .LBB26_6
.LBB26_4:
	fmul.d r14, r14, r8
	fmul.d r16, r16, r8
	fmul.d r6, r6, r8
	fmul.d r4, r4, r8
	fle.d r1, r6, r18
	bne r1, r0, .LBB26_6
.LBB26_5:
	flt.d r1, r6, r20
	bne r1, r0, .LBB26_4
.LBB26_6:
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
.Lfunc_end26:
	.size	f77_cdiv, .Lfunc_end26-f77_cdiv
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpow_i
	.type	.LCPI27_0,@object
.LCPI27_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI27_0, 8
	.type	.LCPI27_1,@object
.LCPI27_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI27_1, 8
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
	lui r1, %hi(.LCPI27_0)
	addi r1, r1, %lo(.LCPI27_0)
	ldw r11, r1+4
	ldw r10, r1+0
	lui r1, %hi(.LCPI27_1)
	addi r1, r1, %lo(.LCPI27_1)
	ldw r9, r1+4
	ldw r8, r1+0
	beq r6, r0, .LBB27_7
.LBB27_1:
	srai r1, r6, 31
	xor r7, r6, r1
	sub r1, r7, r1
	fcvt.d.s r12, r5
	fcvt.d.s r4, r4
	jal r0, .LBB27_3
.LBB27_2:
	beq r1, r0, .LBB27_7
.LBB27_3:
	andi r7, r1, 1
	beq r7, r0, .LBB27_5
.LBB27_4:
	fmul.d r14, r4, r10
	fmul.d r16, r12, r8
	fsub.d r14, r14, r16
	fmul.d r10, r12, r10
	fmul.d r8, r4, r8
	fadd.d r8, r8, r10
	add r10, r14, r0
	add r11, r15, r0
.LBB27_5:
	srli r1, r1, 1
	beq r1, r0, .LBB27_2
.LBB27_6:
	fmul.d r14, r4, r4
	fmul.d r16, r12, r12
	fsub.d r14, r14, r16
	fadd.d r4, r4, r4
	fmul.d r12, r12, r4
	add r4, r14, r0
	add r5, r15, r0
	jal r0, .LBB27_2
.LBB27_7:
	addi r1, r0, -1
	bgt r6, r1, .LBB27_9
.LBB27_8:
	fmul.d r4, r10, r10
	fmul.d r6, r8, r8
	fadd.d r4, r6, r4
	fdiv.d r10, r10, r4
	fneg.d r6, r8
	fdiv.d r8, r6, r4
.LBB27_9:
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
.Lfunc_end27:
	.size	f77_cpow_i, .Lfunc_end27-f77_cpow_i
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
.Lfunc_end28:
	.size	f77_clog, .Lfunc_end28-f77_clog
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
.Lfunc_end29:
	.size	f77_cexp, .Lfunc_end29-f77_cexp
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_cpow_c
	.type	.LCPI30_0,@object
.LCPI30_0:
	.word	0x00000000                      # float 0
	.size	.LCPI30_0, 4
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
	lui r1, %hi(.LCPI30_0)
	ldw r1, r1+%lo(.LCPI30_0)
	feq.s r3, r4, r1
	xori r3, r3, 1
	bne r3, r0, .LBB30_3
.LBB30_1:
	feq.s r3, r5, r1
	xori r3, r3, 1
	bne r3, r0, .LBB30_3
.LBB30_2:
	stw r11+0, r0
	jal r0, .LBB30_4
.LBB30_3:
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
.LBB30_4:
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
.Lfunc_end30:
	.size	f77_cpow_c, .Lfunc_end30-f77_cpow_c
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_csqrt
	.type	.LCPI31_0,@object
.LCPI31_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI31_0, 8
	.type	.LCPI31_2,@object
.LCPI31_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI31_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI31_1,@object
.LCPI31_1:
	.word	0x00000000                      # float 0
	.size	.LCPI31_1, 4
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
	lui r1, %hi(.LCPI31_0)
	addi r1, r1, %lo(.LCPI31_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r16, r1, r0
	add r17, r2, r0
	lui r3, %hi(.LCPI31_1)
	ldw r1, r3+%lo(.LCPI31_1)
	fle.s r4, r1, r13
	xori r4, r4, 1
	beq r4, r0, .LBB31_4
.LBB31_1:
	ldw r13, r3+%lo(.LCPI31_1)
	lui r1, %hi(.LCPI31_2)
	addi r1, r1, %lo(.LCPI31_2)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r16, r4
	add r1, r13, r0
	bne r3, r0, .LBB31_3
.LBB31_2:
	add r3, r14, r0
	add r4, r15, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r16, r16
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
.LBB31_3:
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
	jal r0, .LBB31_6
.LBB31_4:
	fcvt.s.d r3, r16
	stw r11+0, r3
	lui r3, %hi(.LCPI31_2)
	addi r3, r3, %lo(.LCPI31_2)
	ldw r5, r3+4
	ldw r4, r3+0
	feq.d r3, r16, r4
	bne r3, r0, .LBB31_6
.LBB31_5:
	fadd.d r4, r16, r16
	fdiv.d r4, r14, r4
	fcvt.s.d r1, r4
.LBB31_6:
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
.Lfunc_end31:
	.size	f77_csqrt, .Lfunc_end31-f77_csqrt
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
.Lfunc_end32:
	.size	f77_csin, .Lfunc_end32-f77_csin
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
.Lfunc_end33:
	.size	f77_ccos, .Lfunc_end33-f77_ccos
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
	beq r1, r0, .LBB34_2
.LBB34_1:
	fdiv.d r6, r16, r18
	fmul.d r4, r16, r6
	fadd.d r8, r18, r4
	fmul.d r4, r12, r6
	fadd.d r4, r14, r4
	fdiv.d r4, r4, r8
	fmul.d r6, r14, r6
	fsub.d r6, r6, r12
	jal r0, .LBB34_3
.LBB34_2:
	fdiv.d r6, r18, r16
	fmul.d r4, r18, r6
	fadd.d r8, r16, r4
	fmul.d r4, r14, r6
	fadd.d r4, r12, r4
	fdiv.d r4, r4, r8
	fmul.d r6, r12, r6
	fsub.d r6, r14, r6
.LBB34_3:
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
.Lfunc_end34:
	.size	f77_zdiv, .Lfunc_end34-f77_zdiv
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zpow_i
	.type	.LCPI35_0,@object
.LCPI35_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI35_0, 8
	.type	.LCPI35_1,@object
.LCPI35_1:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI35_1, 8
	.type	.LCPI35_2,@object
.LCPI35_2:
	.quad	0xbff0000000000000              # double -1
	.size	.LCPI35_2, 8
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
	lui r1, %hi(.LCPI35_0)
	addi r1, r1, %lo(.LCPI35_0)
	ldw r13, r1+4
	ldw r12, r1+0
	lui r1, %hi(.LCPI35_1)
	addi r1, r1, %lo(.LCPI35_1)
	ldw r15, r1+4
	ldw r14, r1+0
	beq r5, r0, .LBB35_11
.LBB35_1:
	ldw r17, r4+4
	ldw r16, r4+0
	ldw r19, r4+12
	ldw r18, r4+8
	addi r1, r0, -1
	bgt r5, r1, .LBB35_6
.LBB35_2:
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
	beq r1, r0, .LBB35_4
.LBB35_3:
	fdiv.d r4, r16, r18
	fmul.d r6, r16, r4
	fadd.d r6, r18, r6
	fadd.d r8, r4, r12
	fdiv.d r16, r8, r6
	fmul.d r4, r4, r12
	lui r1, %hi(.LCPI35_2)
	addi r1, r1, %lo(.LCPI35_2)
	ldw r9, r1+4
	ldw r8, r1+0
	fadd.d r4, r4, r8
	jal r0, .LBB35_5
.LBB35_4:
	fdiv.d r4, r18, r16
	fmul.d r6, r18, r4
	fadd.d r6, r16, r6
	fmul.d r8, r4, r12
	fadd.d r8, r8, r14
	fdiv.d r16, r8, r6
	fsub.d r4, r12, r4
.LBB35_5:
	fdiv.d r18, r4, r6
	add r3, r11, r0
	add r5, r20, r0
.LBB35_6:
	andi r1, r5, 1
	bne r1, r0, .LBB35_8
.LBB35_7:
	add r6, r14, r0
	add r7, r15, r0
	srli r5, r5, 1
	bne r5, r0, .LBB35_9
	jal r0, .LBB35_10
.LBB35_8:
	fmul.d r6, r14, r16
	fmul.d r8, r12, r18
	fsub.d r6, r6, r8
	fmul.d r8, r14, r18
	fmul.d r10, r12, r16
	fadd.d r12, r10, r8
	srli r5, r5, 1
	beq r5, r0, .LBB35_10
.LBB35_9:
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
	beq r1, r0, .LBB35_7
	jal r0, .LBB35_8
.LBB35_10:
	add r14, r6, r0
	add r15, r7, r0
.LBB35_11:
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
.Lfunc_end35:
	.size	f77_zpow_i, .Lfunc_end35-f77_zpow_i
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zpow_z
	.type	.LCPI36_0,@object
.LCPI36_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI36_0, 8
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
	lui r1, %hi(.LCPI36_0)
	addi r1, r1, %lo(.LCPI36_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r8, r6
	xori r1, r1, 1
	bne r1, r0, .LBB36_3
.LBB36_1:
	ldw r15, r4+12
	ldw r14, r4+8
	feq.d r1, r14, r6
	xori r1, r1, 1
	bne r1, r0, .LBB36_3
.LBB36_2:
	stw r11+12, r0
	stw r11+8, r0
	stw r11+4, r0
	stw r11+0, r0
	jal r0, .LBB36_4
.LBB36_3:
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
.LBB36_4:
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
.Lfunc_end36:
	.size	f77_zpow_z, .Lfunc_end36-f77_zpow_z
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
.Lfunc_end37:
	.size	f77_zabs, .Lfunc_end37-f77_zabs
                                        # -- End function
	.globl	f77_allocate                    # -- Begin function f77_allocate
	.p2align	2
	.type	f77_allocate,@function
f77_allocate:                           # @f77_allocate
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	add r11, r5, r0
	beq r3, r0, .LBB38_5
.LBB38_1:
	beq r11, r0, .LBB38_9
.LBB38_2:
	add r1, r3, r0
	lui r3, 1
	addi r3, r3, 918
.LBB38_3:
	stw r11+0, r3
.LBB38_4:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB38_5:
	addi r1, r0, 1
	sgt r1, r4, r1
	sub r1, r0, r1
	add r12, r4, r0
	xori r3, r4, 1
	and r1, r3, r1
	xori r3, r1, 1
	jal r31, malloc
	beq r1, r0, .LBB38_7
.LBB38_6:
	add r3, r0, r0
	bne r11, r0, .LBB38_3
	jal r0, .LBB38_4
.LBB38_7:
	beq r11, r0, .LBB38_11
.LBB38_8:
	lui r1, 1
	addi r3, r1, 918
	add r1, r0, r0
	jal r0, .LBB38_3
.LBB38_9:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
.LBB38_10:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB38_11:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r5, r12, r0
	jal r0, .LBB38_10
.Lfunc_end38:
	.size	f77_allocate, .Lfunc_end38-f77_allocate
                                        # -- End function
	.globl	f77_deallocate                  # -- Begin function f77_deallocate
	.p2align	2
	.type	f77_deallocate,@function
f77_deallocate:                         # @f77_deallocate
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	add r11, r4, r0
	beq r3, r0, .LBB39_2
.LBB39_1:
	jal r31, free
	add r1, r0, r0
	bne r11, r0, .LBB39_4
	jal r0, .LBB39_5
.LBB39_2:
	beq r11, r0, .LBB39_6
.LBB39_3:
	addi r1, r0, 1
.LBB39_4:
	stw r11+0, r1
.LBB39_5:
	add r1, r0, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB39_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end39:
	.size	f77_deallocate, .Lfunc_end39-f77_deallocate
                                        # -- End function
	.globl	f77_dealloc_auto                # -- Begin function f77_dealloc_auto
	.p2align	2
	.type	f77_dealloc_auto,@function
f77_dealloc_auto:                       # @f77_dealloc_auto
# %bb.0:
	beq r3, r0, .LBB40_2
.LBB40_1:
	jal r0, free
.LBB40_2:
	jalr r0, r31, 0
.Lfunc_end40:
	.size	f77_dealloc_auto, .Lfunc_end40-f77_dealloc_auto
                                        # -- End function
	.globl	f77_dta_free                    # -- Begin function f77_dta_free
	.p2align	2
	.type	f77_dta_free,@function
f77_dta_free:                           # @f77_dta_free
# %bb.0:
	addi r1, r0, 1
	blt r4, r1, .LBB41_6
.LBB41_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	jal r0, .LBB41_3
.LBB41_2:
	addi r12, r12, -1
	add r13, r13, r11
	beq r12, r0, .LBB41_5
.LBB41_3:
	ldw r3, r13+0
	beq r3, r0, .LBB41_2
.LBB41_4:
	jal r31, free
	stw r13+0, r0
	jal r0, .LBB41_2
.LBB41_5:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB41_6:
	jalr r0, r31, 0
.Lfunc_end41:
	.size	f77_dta_free, .Lfunc_end41-f77_dta_free
                                        # -- End function
	.globl	f77_dta_dup                     # -- Begin function f77_dta_dup
	.p2align	2
	.type	f77_dta_dup,@function
f77_dta_dup:                            # @f77_dta_dup
# %bb.0:
	addi sp, sp, -96
	stw sp+0, lr
	stw sp+92, r11
	stw sp+88, r12
	stw sp+84, r13
	stw sp+80, r14
	stw sp+76, r15
	stw sp+72, r16
	stw sp+68, r17
	stw sp+64, r18
	stw sp+60, r19
	stw sp+56, r20
	ldw r1, r3+0
	beq r1, r0, .LBB42_20
.LBB42_1:
	add r13, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	addi r1, r0, 1
	add r15, r1, r0
	blt r4, r1, .LBB42_4
.LBB42_2:
	addi r3, r12, 4
	addi r15, r0, 1
	addi r4, r0, 0
	add r5, r11, r0
.LBB42_3:
	ldw r6, r3+0
	sgt r7, r6, r4
	mul r6, r6, r15
	sub r7, r0, r7
	and r15, r6, r7
	addi r5, r5, -1
	addi r3, r3, 8
	bne r5, r0, .LBB42_3
.LBB42_4:
	mul r3, r15, r13
	xori r4, r3, 1
	sgt r1, r3, r1
	sub r1, r0, r1
	and r1, r4, r1
	xori r3, r1, 1
	jal r31, malloc
	beq r1, r0, .LBB42_21
.LBB42_5:
	add r14, r1, r0
	addi r3, sp, 28
	addi r5, r0, 28
	add r4, r0, r0
	jal r31, memset
	addi r16, r0, 1
	blt r15, r16, .LBB42_17
.LBB42_6:
	addi r17, r12, 8
	addi r18, r12, 4
	addi r19, r0, 1
	add r20, r0, r0
	jal r0, .LBB42_8
.LBB42_7:
	addi r20, r20, 1
	beq r20, r15, .LBB42_17
.LBB42_8:
	blt r11, r19, .LBB42_12
.LBB42_9:
	addi r1, sp, 28
	add r3, r17, r0
	add r5, r11, r0
	add r4, r0, r0
.LBB42_10:
	ldw r6, r1+0
	ldw r7, r3+0
	mul r6, r7, r6
	add r4, r6, r4
	addi r5, r5, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r5, r0, .LBB42_10
.LBB42_11:
	mul r1, r4, r13
	jal r0, .LBB42_13
.LBB42_12:
	add r1, r0, r0
.LBB42_13:
	mul r3, r20, r13
	add r3, r14, r3
	ldw r4, r12+0
	add r4, r4, r1
	add r5, r13, r0
	jal r31, memcpy
	blt r11, r19, .LBB42_7
.LBB42_14:
	addi r1, sp, 28
	add r3, r18, r0
	add r4, r11, r0
.LBB42_15:
	ldw r5, r1+0
	addi r5, r5, 1
	stw r1+0, r5
	ldw r6, r3+0
	blt r5, r6, .LBB42_7
.LBB42_16:
	stw r1+0, r0
	addi r4, r4, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r4, r0, .LBB42_15
	jal r0, .LBB42_7
.LBB42_17:
	stw r12+0, r14
	blt r11, r16, .LBB42_20
.LBB42_18:
	addi r1, r12, 4
.LBB42_19:
	stw r1+4, r16
	ldw r3, r1+0
	mul r16, r3, r16
	addi r11, r11, -1
	addi r1, r1, 8
	bne r11, r0, .LBB42_19
.LBB42_20:
	ldw r20, sp+56
	ldw r19, sp+60
	ldw r18, sp+64
	ldw r17, sp+68
	ldw r16, sp+72
	ldw r15, sp+76
	ldw r14, sp+80
	ldw r13, sp+84
	ldw r12, sp+88
	ldw r11, sp+92
	ldw lr, sp+0
	addi sp, sp, 96
	jalr r0, r31, 0
.LBB42_21:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end42:
	.size	f77_dta_dup, .Lfunc_end42-f77_dta_dup
                                        # -- End function
	.globl	f77_as_pack                     # -- Begin function f77_as_pack
	.p2align	2
	.type	f77_as_pack,@function
f77_as_pack:                            # @f77_as_pack
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	addi r1, r0, 1
	blt r4, r1, .LBB43_9
.LBB43_1:
	addi r6, r3, 4
	addi r7, r0, 2
	add r8, r4, r0
	jal r0, .LBB43_3
.LBB43_2:
	mul r1, r9, r1
	addi r8, r8, -1
	addi r6, r6, 8
	beq r8, r0, .LBB43_9
.LBB43_3:
	ldw r9, r6+0
	blt r9, r7, .LBB43_2
.LBB43_4:
	ldw r10, r6+4
	beq r10, r1, .LBB43_2
.LBB43_5:
	add r11, r3, r0
	addi r3, r3, 4
	addi r1, r0, 1
	add r12, r4, r0
.LBB43_6:
	ldw r6, r3+0
	mul r1, r6, r1
	addi r4, r4, -1
	addi r3, r3, 8
	bne r4, r0, .LBB43_6
.LBB43_7:
	add r14, r5, r0
	mul r13, r1, r5
	xori r1, r13, 1
	addi r3, r0, 1
	sgt r3, r13, r3
	sub r3, r0, r3
	and r1, r1, r3
	xori r3, r1, 1
	jal r31, malloc
	beq r1, r0, .LBB43_11
.LBB43_8:
	addi r7, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r14, r0
	add r6, r1, r0
	add r11, r1, r0
	jal r31, f77_as_walk
	add r1, r11, r0
	jal r0, .LBB43_10
.LBB43_9:
	ldw r1, r3+0
.LBB43_10:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB43_11:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	add r5, r13, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end43:
	.size	f77_as_pack, .Lfunc_end43-f77_as_pack
                                        # -- End function
	.p2align	2                               # -- Begin function f77_as_walk
	.type	f77_as_walk,@function
f77_as_walk:                            # @f77_as_walk
# %bb.0:
	addi sp, sp, -96
	stw sp+0, lr
	stw sp+92, r11
	stw sp+88, r12
	stw sp+84, r13
	stw sp+80, r14
	stw sp+76, r15
	stw sp+72, r16
	stw sp+68, r17
	stw sp+64, r18
	stw sp+60, r19
	stw sp+56, r20
	add r11, r7, r0
	add r12, r6, r0
	add r13, r5, r0
	add r14, r4, r0
	addi r1, r0, 1
	addi r4, sp, 24
	add r15, r1, r0
	blt r14, r1, .LBB44_3
.LBB44_1:
	addi r5, r3, 4
	addi r15, r0, 1
	add r6, r14, r0
.LBB44_2:
	stw r4+0, r0
	ldw r7, r5+0
	mul r15, r7, r15
	addi r6, r6, -1
	addi r5, r5, 8
	addi r4, r4, 4
	bne r6, r0, .LBB44_2
.LBB44_3:
	blt r15, r1, .LBB44_18
.LBB44_4:
	ldw r16, r3+0
	addi r17, r3, 4
	addi r18, r3, 8
	addi r19, r0, 1
	add r20, r0, r0
	jal r0, .LBB44_6
.LBB44_5:
	addi r20, r20, 1
	beq r20, r15, .LBB44_18
.LBB44_6:
	addi r1, sp, 24
	blt r14, r19, .LBB44_10
.LBB44_7:
	add r3, r18, r0
	add r5, r14, r0
	add r4, r0, r0
.LBB44_8:
	ldw r6, r1+0
	ldw r7, r3+0
	mul r6, r7, r6
	add r4, r6, r4
	addi r5, r5, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r5, r0, .LBB44_8
.LBB44_9:
	mul r1, r4, r13
	jal r0, .LBB44_11
.LBB44_10:
	add r1, r0, r0
.LBB44_11:
	add r4, r16, r1
	mul r1, r20, r13
	beq r11, r0, .LBB44_13
.LBB44_12:
	add r3, r12, r1
	jal r0, .LBB44_14
.LBB44_13:
	add r1, r12, r1
	add r3, r4, r0
	add r4, r1, r0
.LBB44_14:
	add r5, r13, r0
	jal r31, memcpy
	blt r14, r19, .LBB44_5
.LBB44_15:
	addi r1, sp, 24
	add r3, r17, r0
	add r4, r14, r0
.LBB44_16:
	ldw r5, r1+0
	addi r5, r5, 1
	stw r1+0, r5
	ldw r6, r3+0
	blt r5, r6, .LBB44_5
.LBB44_17:
	stw r1+0, r0
	addi r4, r4, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r4, r0, .LBB44_16
	jal r0, .LBB44_5
.LBB44_18:
	ldw r20, sp+56
	ldw r19, sp+60
	ldw r18, sp+64
	ldw r17, sp+68
	ldw r16, sp+72
	ldw r15, sp+76
	ldw r14, sp+80
	ldw r13, sp+84
	ldw r12, sp+88
	ldw r11, sp+92
	ldw lr, sp+0
	addi sp, sp, 96
	jalr r0, r31, 0
.Lfunc_end44:
	.size	f77_as_walk, .Lfunc_end44-f77_as_walk
                                        # -- End function
	.globl	f77_vec_new                     # -- Begin function f77_vec_new
	.p2align	2
	.type	f77_vec_new,@function
f77_vec_new:                            # @f77_vec_new
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	addi r11, r0, 16
	add r3, r11, r0
	jal r31, malloc
	beq r1, r0, .LBB45_3
.LBB45_1:
	add r12, r1, r0
	addi r3, r0, 256
	jal r31, malloc
	stw r12+0, r1
	beq r1, r0, .LBB45_3
.LBB45_2:
	stw r12+4, r0
	stw r12+8, r11
	stw r12+12, r11
	add r1, r12, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB45_3:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.105)
	addi r4, r4, %lo(.L.str.105)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end45:
	.size	f77_vec_new, .Lfunc_end45-f77_vec_new
                                        # -- End function
	.globl	f77_vec_pushn                   # -- Begin function f77_vec_pushn
	.p2align	2
	.type	f77_vec_pushn,@function
f77_vec_pushn:                          # @f77_vec_pushn
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	add r11, r3, r0
	ldw r1, r3+4
	bne r1, r0, .LBB46_4
.LBB46_1:
	ldw r1, r11+12
	beq r5, r1, .LBB46_4
.LBB46_2:
	add r12, r4, r0
	ldw r3, r11+0
	ldw r1, r11+8
	addi r4, r0, 1
	sgt r4, r5, r4
	sub r4, r0, r4
	add r13, r5, r0
	xori r5, r5, 1
	and r4, r5, r4
	xori r4, r4, 1
	mul r4, r1, r4
	jal r31, realloc
	beq r1, r0, .LBB46_8
.LBB46_3:
	stw r11+0, r1
	stw r11+12, r13
	add r4, r12, r0
.LBB46_4:
	ldw r3, r11+4
	ldw r1, r11+8
	bne r3, r1, .LBB46_7
.LBB46_5:
	add r12, r4, r0
	ldw r3, r11+0
	ldw r4, r11+12
	addi r5, r0, 1
	sgt r5, r4, r5
	sub r5, r0, r5
	xori r4, r4, 1
	and r4, r4, r5
	xori r4, r4, 1
	mul r1, r1, r4
	slli r4, r1, 1
	jal r31, realloc
	beq r1, r0, .LBB46_8
.LBB46_6:
	stw r11+0, r1
	ldw r1, r11+8
	slli r1, r1, 1
	stw r11+8, r1
	add r4, r12, r0
.LBB46_7:
	ldw r1, r11+0
	ldw r3, r11+4
	ldw r5, r11+12
	mul r3, r5, r3
	add r3, r1, r3
	jal r31, memcpy
	ldw r1, r11+4
	addi r1, r1, 1
	stw r11+4, r1
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB46_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.105)
	addi r4, r4, %lo(.L.str.105)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end46:
	.size	f77_vec_pushn, .Lfunc_end46-f77_vec_pushn
                                        # -- End function
	.globl	f77_vec_push                    # -- Begin function f77_vec_push
	.p2align	2
	.type	f77_vec_push,@function
f77_vec_push:                           # @f77_vec_push
# %bb.0:
	addi r5, r0, 16
	jal r0, f77_vec_pushn
.Lfunc_end47:
	.size	f77_vec_push, .Lfunc_end47-f77_vec_push
                                        # -- End function
	.globl	f77_vec_count                   # -- Begin function f77_vec_count
	.p2align	2
	.type	f77_vec_count,@function
f77_vec_count:                          # @f77_vec_count
# %bb.0:
	ldw r1, r3+4
	jalr r0, r31, 0
.Lfunc_end48:
	.size	f77_vec_count, .Lfunc_end48-f77_vec_count
                                        # -- End function
	.globl	f77_vec_take                    # -- Begin function f77_vec_take
	.p2align	2
	.type	f77_vec_take,@function
f77_vec_take:                           # @f77_vec_take
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	ldw r11, r3+0
	jal r31, free
	add r1, r11, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end49:
	.size	f77_vec_take, .Lfunc_end49-f77_vec_take
                                        # -- End function
	.globl	f77_as_unpack                   # -- Begin function f77_as_unpack
	.p2align	2
	.type	f77_as_unpack,@function
f77_as_unpack:                          # @f77_as_unpack
# %bb.0:
	ldw r1, r3+0
	beq r6, r1, .LBB50_2
.LBB50_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	add r11, r6, r0
	add r7, r0, r0
	jal r31, f77_as_walk
	add r3, r11, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, free
.LBB50_2:
	jalr r0, r31, 0
.Lfunc_end50:
	.size	f77_as_unpack, .Lfunc_end50-f77_as_unpack
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpu_time_r
	.type	.LCPI51_0,@object
.LCPI51_0:
	.quad	0x412e848000000000              # double 1.0E+6
	.size	.LCPI51_0, 8
	.text
	.globl	f77_cpu_time_r
	.p2align	2
	.type	f77_cpu_time_r,@function
f77_cpu_time_r:                         # @f77_cpu_time_r
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	add r11, r3, r0
	jal r31, clock
	fcvt.d.wu r4, r1
	lui r1, %hi(.LCPI51_0)
	addi r1, r1, %lo(.LCPI51_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
	stw r11+0, r1
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end51:
	.size	f77_cpu_time_r, .Lfunc_end51-f77_cpu_time_r
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpu_time_d
	.type	.LCPI52_0,@object
.LCPI52_0:
	.quad	0x412e848000000000              # double 1.0E+6
	.size	.LCPI52_0, 8
	.text
	.globl	f77_cpu_time_d
	.p2align	2
	.type	f77_cpu_time_d,@function
f77_cpu_time_d:                         # @f77_cpu_time_d
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	add r11, r3, r0
	jal r31, clock
	fcvt.d.wu r4, r1
	lui r1, %hi(.LCPI52_0)
	addi r1, r1, %lo(.LCPI52_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fdiv.d r4, r4, r6
	stw r11+4, r5
	stw r11+0, r4
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end52:
	.size	f77_cpu_time_d, .Lfunc_end52-f77_cpu_time_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_fraction
	.type	.LCPI53_0,@object
.LCPI53_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI53_0, 8
	.text
	.globl	f77_fraction
	.p2align	2
	.type	f77_fraction,@function
f77_fraction:                           # @f77_fraction
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI53_0)
	addi r1, r1, %lo(.LCPI53_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r6, r4
	bne r1, r0, .LBB53_2
.LBB53_1:
	addi sp, sp, -32
	stw sp+0, lr
	addi r5, sp, 28
	add r3, r6, r0
	add r4, r7, r0
	jal r31, frexp
	add r4, r1, r0
	add r5, r2, r0
	ldw lr, sp+0
	addi sp, sp, 32
.LBB53_2:
	add r1, r4, r0
	add r2, r5, r0
	jalr r0, r31, 0
.Lfunc_end53:
	.size	f77_fraction, .Lfunc_end53-f77_fraction
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_set_exponent
	.type	.LCPI54_0,@object
.LCPI54_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI54_0, 8
	.text
	.globl	f77_set_exponent
	.p2align	2
	.type	f77_set_exponent,@function
f77_set_exponent:                       # @f77_set_exponent
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI54_0)
	addi r1, r1, %lo(.LCPI54_0)
	ldw r9, r1+4
	ldw r8, r1+0
	feq.d r1, r6, r8
	bne r1, r0, .LBB54_2
.LBB54_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	addi r1, sp, 24
	add r3, r6, r0
	add r4, r7, r0
	add r11, r5, r0
	add r5, r1, r0
	jal r31, frexp
	add r3, r1, r0
	add r4, r2, r0
	add r5, r11, r0
	jal r31, ldexp
	add r8, r1, r0
	add r9, r2, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB54_2:
	add r1, r8, r0
	add r2, r9, r0
	jalr r0, r31, 0
.Lfunc_end54:
	.size	f77_set_exponent, .Lfunc_end54-f77_set_exponent
                                        # -- End function
	.globl	f77_scale                       # -- Begin function f77_scale
	.p2align	2
	.type	f77_scale,@function
f77_scale:                              # @f77_scale
# %bb.0:
	add r9, r4, r0
	add r8, r3, r0
	beq r6, r0, .LBB55_2
.LBB55_1:
	add r3, r8, r0
	add r4, r9, r0
	jal r0, ldexp
.LBB55_2:
	addi sp, sp, -16
	stw sp+0, lr
	fcvt.s.d r3, r8
	add r4, r5, r0
	jal r31, ldexpf
	fcvt.d.s r4, r1
	add r1, r4, r0
	add r2, r5, r0
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end55:
	.size	f77_scale, .Lfunc_end55-f77_scale
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_nearest
	.type	.LCPI56_0,@object
.LCPI56_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI56_0, 8
	.type	.LCPI56_2,@object
.LCPI56_2:
	.word	0xff800000                      # float -Inf
	.word	0x7f800000                      # float +Inf
	.size	.LCPI56_2, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI56_1,@object
.LCPI56_1:
	.quad	0xfff0000000000000              # double -Inf
	.quad	0x7ff0000000000000              # double +Inf
	.size	.LCPI56_1, 16
	.text
	.globl	f77_nearest
	.p2align	2
	.type	f77_nearest,@function
f77_nearest:                            # @f77_nearest
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	add r11, r6, r0
	add r10, r5, r0
	add r9, r4, r0
	lui r1, %hi(.LCPI56_0)
	addi r1, r1, %lo(.LCPI56_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r10, r4
	xori r1, r1, 1
	beq r1, r0, .LBB56_4
.LBB56_1:
	add r8, r3, r0
	beq r7, r0, .LBB56_3
.LBB56_2:
	flt.d r1, r4, r10
	sub r1, r0, r1
	andi r1, r1, 8
	lui r3, %hi(.LCPI56_1)
	addi r3, r3, %lo(.LCPI56_1)
	add r1, r3, r1
	ldw r6, r1+4
	ldw r5, r1+0
	add r3, r8, r0
	add r4, r9, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, nextafter
.LBB56_3:
	fcvt.s.d r3, r8
	flt.d r1, r4, r10
	sub r1, r0, r1
	andi r1, r1, 4
	lui r4, %hi(.LCPI56_2)
	addi r4, r4, %lo(.LCPI56_2)
	add r1, r4, r1
	ldw r4, r1+0
	jal r31, nextafterf
	fcvt.d.s r4, r1
	add r1, r4, r0
	add r2, r5, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB56_4:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.21)
	addi r4, r4, %lo(.L.str.21)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end56:
	.size	f77_nearest, .Lfunc_end56-f77_nearest
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_spacing
	.type	.LCPI57_0,@object
.LCPI57_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI57_0, 8
	.type	.LCPI57_1,@object
.LCPI57_1:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI57_1, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI57_2,@object
.LCPI57_2:
	.quad	0x0010000000000000              # double 2.2250738585072014E-308
	.quad	0x3810000000000000              # double 1.1754943508222875E-38
	.size	.LCPI57_2, 16
	.text
	.globl	f77_spacing
	.p2align	2
	.type	f77_spacing,@function
f77_spacing:                            # @f77_spacing
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI57_0)
	addi r1, r1, %lo(.LCPI57_0)
	ldw r9, r1+4
	ldw r8, r1+0
	feq.d r1, r6, r8
	xori r1, r1, 1
	bne r1, r0, .LBB57_2
.LBB57_1:
	addi r1, r0, 0
	seq r1, r5, r1
	sub r1, r0, r1
	andi r1, r1, 8
	lui r3, %hi(.LCPI57_2)
	addi r3, r3, %lo(.LCPI57_2)
	add r1, r3, r1
	ldw r5, r1+4
	ldw r4, r1+0
	jal r0, .LBB57_6
.LBB57_2:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	addi r1, r0, 0
	seq r1, r5, r1
	sub r11, r0, r1
	andi r1, r11, 35
	addi r3, r0, -53
	xor r12, r1, r3
	addi r5, sp, 20
	add r3, r6, r0
	add r4, r7, r0
	jal r31, frexp
	ldw r1, sp+20
	add r5, r1, r12
	andi r1, r11, 896
	addi r1, r1, -1022
	bge r5, r1, .LBB57_4
.LBB57_3:
	andi r1, r11, 8
	lui r3, %hi(.LCPI57_2)
	addi r3, r3, %lo(.LCPI57_2)
	add r1, r3, r1
	ldw r5, r1+4
	ldw r4, r1+0
	jal r0, .LBB57_5
.LBB57_4:
	lui r1, %hi(.LCPI57_1)
	addi r1, r1, %lo(.LCPI57_1)
	ldw r4, r1+4
	ldw r3, r1+0
	jal r31, ldexp
	add r4, r1, r0
	add r5, r2, r0
.LBB57_5:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB57_6:
	add r1, r4, r0
	add r2, r5, r0
	jalr r0, r31, 0
.Lfunc_end57:
	.size	f77_spacing, .Lfunc_end57-f77_spacing
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_rrspacing
	.type	.LCPI58_0,@object
.LCPI58_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI58_0, 8
	.text
	.globl	f77_rrspacing
	.p2align	2
	.type	f77_rrspacing,@function
f77_rrspacing:                          # @f77_rrspacing
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	add r11, r5, r0
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI58_0)
	addi r1, r1, %lo(.LCPI58_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r6, r4
	bne r1, r0, .LBB58_2
.LBB58_1:
	addi r5, sp, 24
	add r3, r6, r0
	add r4, r7, r0
	jal r31, frexp
	add r3, r1, r0
	add r4, r2, r0
	jal r31, fabs
	addi r3, r0, 0
	seq r3, r11, r3
	sub r3, r0, r3
	andi r3, r3, 45
	xori r5, r3, 53
	add r3, r1, r0
	add r4, r2, r0
	jal r31, ldexp
	add r4, r1, r0
	add r5, r2, r0
.LBB58_2:
	add r1, r4, r0
	add r2, r5, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end58:
	.size	f77_rrspacing, .Lfunc_end58-f77_rrspacing
                                        # -- End function
	.globl	f77_modulo_i                    # -- Begin function f77_modulo_i
	.p2align	2
	.type	f77_modulo_i,@function
f77_modulo_i:                           # @f77_modulo_i
# %bb.0:
	beq r4, r0, .LBB59_2
.LBB59_1:
	rem r1, r3, r4
	addi r3, r0, 0
	seq r3, r1, r3
	xor r5, r1, r4
	srai r5, r5, 31
	and r4, r5, r4
	add r1, r4, r1
	sub r3, r0, r3
	and r3, r1, r3
	xor r1, r1, r3
	jalr r0, r31, 0
.LBB59_2:
	addi sp, sp, -16
	stw sp+0, lr
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end59:
	.size	f77_modulo_i, .Lfunc_end59-f77_modulo_i
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_modulo_d
	.type	.LCPI60_0,@object
.LCPI60_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI60_0, 8
	.type	.LCPI60_2,@object
.LCPI60_2:
	.quad	0xb690000000000000              # double -7.0064923216240854E-46
	.size	.LCPI60_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI60_1,@object
.LCPI60_1:
	.word	0x00000000                      # float 0
	.size	.LCPI60_1, 4
	.text
	.globl	f77_modulo_d
	.p2align	2
	.type	f77_modulo_d,@function
f77_modulo_d:                           # @f77_modulo_d
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	add r13, r6, r0
	add r12, r5, r0
	add r5, r4, r0
	add r4, r3, r0
	beq r7, r0, .LBB60_3
.LBB60_1:
	add r3, r4, r0
	add r4, r5, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fmod
	add r4, r1, r0
	add r5, r2, r0
	lui r1, %hi(.LCPI60_0)
	addi r1, r1, %lo(.LCPI60_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r4, r6
	bne r1, r0, .LBB60_6
.LBB60_2:
	flt.d r1, r4, r6
	flt.d r3, r12, r6
	xori r3, r3, 1
	xor r1, r3, r1
	fadd.d r6, r12, r4
	sub r1, r0, r1
	xor r3, r2, r7
	and r3, r3, r1
	xor r5, r7, r3
	xor r3, r4, r6
	and r1, r3, r1
	xor r4, r6, r1
	jal r0, .LBB60_6
.LBB60_3:
	fcvt.s.d r3, r4
	fcvt.s.d r11, r12
	add r4, r11, r0
	jal r31, fmodf
	lui r3, %hi(.LCPI60_1)
	ldw r3, r3+%lo(.LCPI60_1)
	feq.s r4, r1, r3
	bne r4, r0, .LBB60_5
.LBB60_4:
	flt.s r3, r1, r3
	lui r4, %hi(.LCPI60_2)
	addi r4, r4, %lo(.LCPI60_2)
	ldw r7, r4+4
	ldw r6, r4+0
	flt.d r4, r12, r6
	xori r4, r4, 1
	xor r3, r4, r3
	fadd.s r4, r1, r11
	sub r3, r0, r3
	xor r1, r1, r4
	and r1, r1, r3
	xor r1, r4, r1
.LBB60_5:
	fcvt.d.s r4, r1
.LBB60_6:
	add r1, r4, r0
	add r2, r5, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end60:
	.size	f77_modulo_d, .Lfunc_end60-f77_modulo_d
                                        # -- End function
	.globl	f77_date_and_time               # -- Begin function f77_date_and_time
	.p2align	2
	.type	f77_date_and_time,@function
f77_date_and_time:                      # @f77_date_and_time
# %bb.0:
	addi sp, sp, -96
	stw sp+0, lr
	stw sp+92, r11
	stw sp+88, r12
	stw sp+84, r13
	stw sp+80, r14
	stw sp+76, r15
	stw sp+72, r16
	stw sp+68, r17
	stw sp+64, r18
	add r11, r9, r0
	add r12, r8, r0
	add r13, r7, r0
	add r15, r6, r0
	add r16, r5, r0
	add r17, r4, r0
	add r18, r3, r0
	addi r3, sp, 52
	add r4, r0, r0
	jal r31, gettimeofday
	ldw r1, sp+52
	ldw r3, sp+56
	stw sp+48, r3
	stw sp+44, r1
	addi r3, sp, 44
	jal r31, gmtime
	add r14, r1, r0
	beq r18, r0, .LBB61_6
.LBB61_1:
	ldw r1, r14+20
	addi r6, r1, 1900
	ldw r1, r14+16
	addi r7, r1, 1
	ldw r8, r14+12
	lui r5, %hi(.L.str.23)
	addi r5, r5, %lo(.L.str.23)
	addi r3, sp, 28
	addi r4, r0, 16
	jal r31, snprintf
	addi r1, r0, 1
	blt r17, r1, .LBB61_6
.LBB61_2:
	addi r1, r0, 32
	addi r3, r0, 7
	add r4, r0, r0
	jal r0, .LBB61_4
.LBB61_3:
	add r6, r18, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r17, r4, .LBB61_6
.LBB61_4:
	add r5, r1, r0
	bgtu r4, r3, .LBB61_3
.LBB61_5:
	addi r5, sp, 28
	add r5, r5, r4
	ldbu r5, r5+0
	jal r0, .LBB61_3
.LBB61_6:
	lui r17, 67109
	beq r16, r0, .LBB61_12
.LBB61_7:
	ldw r6, r14+8
	ldw r7, r14+4
	ldw r8, r14+0
	ldw r1, sp+60
	addi r3, r17, -557
	mulh r1, r1, r3
	srli r3, r1, 31
	srai r1, r1, 6
	add r9, r1, r3
	lui r5, %hi(.L.str.24)
	addi r5, r5, %lo(.L.str.24)
	addi r3, sp, 28
	addi r4, r0, 16
	jal r31, snprintf
	addi r1, r0, 1
	blt r15, r1, .LBB61_12
.LBB61_8:
	addi r1, r0, 32
	addi r3, r0, 9
	add r4, r0, r0
	jal r0, .LBB61_10
.LBB61_9:
	add r6, r16, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r15, r4, .LBB61_12
.LBB61_10:
	add r5, r1, r0
	bgtu r4, r3, .LBB61_9
.LBB61_11:
	addi r5, sp, 28
	add r5, r5, r4
	ldbu r5, r5+0
	jal r0, .LBB61_9
.LBB61_12:
	beq r13, r0, .LBB61_18
.LBB61_13:
	addi r1, r0, 1
	blt r12, r1, .LBB61_18
.LBB61_14:
	addi r1, r0, 32
	addi r3, r0, 4
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	add r5, r0, r0
	jal r0, .LBB61_16
.LBB61_15:
	add r7, r13, r5
	stb r7+0, r6
	addi r5, r5, 1
	beq r12, r5, .LBB61_18
.LBB61_16:
	add r6, r1, r0
	bgtu r5, r3, .LBB61_15
.LBB61_17:
	add r6, r5, r4
	ldbu r6, r6+0
	jal r0, .LBB61_15
.LBB61_18:
	beq r11, r0, .LBB61_20
.LBB61_19:
	ldw r1, r14+20
	addi r1, r1, 1900
	stw r11+0, r1
	ldw r1, r14+16
	addi r1, r1, 1
	stw r11+4, r1
	ldw r1, r14+12
	stw r11+8, r1
	stw r11+12, r0
	ldw r1, r14+8
	stw r11+16, r1
	ldw r1, r14+4
	stw r11+20, r1
	ldw r1, r14+0
	stw r11+24, r1
	ldw r1, sp+60
	addi r3, r17, -557
	mulh r1, r1, r3
	srli r3, r1, 31
	srai r1, r1, 6
	add r1, r1, r3
	stw r11+28, r1
.LBB61_20:
	ldw r18, sp+64
	ldw r17, sp+68
	ldw r16, sp+72
	ldw r15, sp+76
	ldw r14, sp+80
	ldw r13, sp+84
	ldw r12, sp+88
	ldw r11, sp+92
	ldw lr, sp+0
	addi sp, sp, 96
	jalr r0, r31, 0
.Lfunc_end61:
	.size	f77_date_and_time, .Lfunc_end61-f77_date_and_time
                                        # -- End function
	.globl	f77_system_clock                # -- Begin function f77_system_clock
	.p2align	2
	.type	f77_system_clock,@function
f77_system_clock:                       # @f77_system_clock
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	addi r3, sp, 20
	add r4, r0, r0
	jal r31, gettimeofday
	lui r14, 524288
	beq r13, r0, .LBB62_2
.LBB62_1:
	ldw r1, sp+20
	ldw r3, sp+24
	addi r4, r0, 1000
	mul r3, r3, r4
	mulhu r5, r1, r4
	add r3, r5, r3
	mul r1, r1, r4
	ldw r4, sp+28
	lui r5, 67109
	addi r5, r5, -557
	mulh r4, r4, r5
	srli r5, r4, 31
	srai r4, r4, 6
	add r4, r4, r5
	srai r5, r4, 31
	add r5, r3, r5
	add r3, r1, r4
	sltu r1, r3, r1
	add r4, r5, r1
	addi r5, r14, -1
	add r6, r0, r0
	jal r31, __moddi3
	stw r13+0, r1
.LBB62_2:
	beq r12, r0, .LBB62_4
.LBB62_3:
	addi r1, r0, 1000
	stw r12+0, r1
.LBB62_4:
	beq r11, r0, .LBB62_6
.LBB62_5:
	addi r1, r14, -1
	stw r11+0, r1
.LBB62_6:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end62:
	.size	f77_system_clock, .Lfunc_end62-f77_system_clock
                                        # -- End function
	.globl	f77_random_seed                 # -- Begin function f77_random_seed
	.p2align	2
	.type	f77_random_seed,@function
f77_random_seed:                        # @f77_random_seed
# %bb.0:
	beq r3, r0, .LBB63_2
.LBB63_1:
	addi r1, r0, 4
	stw r3+0, r1
.LBB63_2:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	lui r11, %hi(f77_rng)
	addi r11, r11, %lo(f77_rng)
	addi r1, r0, 16
	beq r5, r0, .LBB63_5
.LBB63_3:
	add r6, r0, r0
.LBB63_4:
	add r7, r6, r11
	ldw r7, r7+0
	add r8, r5, r6
	stw r8+0, r7
	addi r6, r6, 4
	bne r6, r1, .LBB63_4
.LBB63_5:
	lui r12, %hi(f77_rng+4)
	lui r13, %hi(f77_rng+8)
	lui r14, %hi(f77_rng+12)
	beq r4, r0, .LBB63_10
.LBB63_6:
	add r6, r0, r0
.LBB63_7:
	add r7, r4, r6
	ldw r7, r7+0
	add r8, r6, r11
	stw r8+0, r7
	addi r6, r6, 4
	bne r6, r1, .LBB63_7
.LBB63_8:
	ldw r1, r11+0
	ldw r6, r12+%lo(f77_rng+4)
	or  r1, r6, r1
	ldw r6, r13+%lo(f77_rng+8)
	or  r1, r1, r6
	ldw r6, r14+%lo(f77_rng+12)
	or  r1, r1, r6
	bne r1, r0, .LBB63_10
.LBB63_9:
	addi r1, r0, 1
	stw r11+0, r1
.LBB63_10:
	bne r3, r0, .LBB63_15
.LBB63_11:
	bne r4, r0, .LBB63_15
.LBB63_12:
	bne r5, r0, .LBB63_15
.LBB63_13:
	addi r3, sp, 20
	add r4, r0, r0
	jal r31, gettimeofday
	ldw r1, sp+20
	ldw r3, r11+0
	xor r1, r3, r1
	stw r11+0, r1
	ldw r3, sp+28
	ldw r4, r12+%lo(f77_rng+4)
	xor r3, r4, r3
	stw r12+%lo(f77_rng+4), r3
	ldw r4, r13+%lo(f77_rng+8)
	ldw r5, r14+%lo(f77_rng+12)
	or  r4, r4, r5
	or  r1, r4, r1
	or  r1, r1, r3
	bne r1, r0, .LBB63_15
.LBB63_14:
	addi r1, r0, 1
	stw r11+0, r1
.LBB63_15:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end63:
	.size	f77_random_seed, .Lfunc_end63-f77_random_seed
                                        # -- End function
	.globl	f77_random_seed_d               # -- Begin function f77_random_seed_d
	.p2align	2
	.type	f77_random_seed_d,@function
f77_random_seed_d:                      # @f77_random_seed_d
# %bb.0:
	addi sp, sp, -64
	stw sp+0, lr
	stw sp+60, r11
	add r11, r5, r0
	beq r4, r0, .LBB64_2
.LBB64_1:
	ldw r1, r4+4
	addi r5, r0, 4
	blt r1, r5, .LBB64_12
.LBB64_2:
	beq r11, r0, .LBB64_4
.LBB64_3:
	ldw r1, r11+4
	addi r5, r0, 3
	ble r1, r5, .LBB64_12
.LBB64_4:
	beq r4, r0, .LBB64_7
.LBB64_5:
	ldw r1, r4+0
	ldw r4, r4+8
	slli r5, r4, 2
	addi r6, r0, 16
	add r7, r0, r0
.LBB64_6:
	ldw r8, r1+0
	addi r4, sp, 44
	add r9, r4, r7
	stw r9+0, r8
	addi r7, r7, 4
	add r1, r1, r5
	bne r7, r6, .LBB64_6
	jal r0, .LBB64_8
.LBB64_7:
	add r4, r0, r0
.LBB64_8:
	addi r1, r0, 0
	seq r1, r11, r1
	sub r1, r0, r1
	addi r5, sp, 28
	and r1, r5, r1
	xor r5, r5, r1
	jal r31, f77_random_seed
	beq r11, r0, .LBB64_11
.LBB64_9:
	addi r1, sp, 28
	addi r3, r0, 4
	add r4, r0, r0
.LBB64_10:
	ldw r5, r1+0
	ldw r6, r11+0
	ldw r7, r11+8
	mul r7, r7, r4
	slli r7, r7, 2
	add r6, r6, r7
	stw r6+0, r5
	addi r4, r4, 1
	addi r1, r1, 4
	bne r4, r3, .LBB64_10
.LBB64_11:
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.LBB64_12:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.26)
	addi r4, r4, %lo(.L.str.26)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end64:
	.size	f77_random_seed_d, .Lfunc_end64-f77_random_seed_d
                                        # -- End function
	.globl	f77_date_and_time_d             # -- Begin function f77_date_and_time_d
	.p2align	2
	.type	f77_date_and_time_d,@function
f77_date_and_time_d:                    # @f77_date_and_time_d
# %bb.0:
	addi sp, sp, -64
	stw sp+0, lr
	stw sp+60, r11
	stw sp+56, r12
	beq r9, r0, .LBB65_5
.LBB65_1:
	add r11, r9, r0
	ldw r1, r9+4
	addi r9, r0, 7
	ble r1, r9, .LBB65_6
.LBB65_2:
	addi r12, sp, 24
	add r9, r12, r0
	jal r31, f77_date_and_time
	addi r1, r0, 8
	add r3, r0, r0
.LBB65_3:
	ldw r4, r12+0
	ldw r5, r11+0
	ldw r6, r11+8
	mul r6, r6, r3
	slli r6, r6, 2
	add r5, r5, r6
	stw r5+0, r4
	addi r3, r3, 1
	addi r12, r12, 4
	bne r3, r1, .LBB65_3
.LBB65_4:
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.LBB65_5:
	add r9, r0, r0
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jal r0, f77_date_and_time
.LBB65_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.27)
	addi r4, r4, %lo(.L.str.27)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end65:
	.size	f77_date_and_time_d, .Lfunc_end65-f77_date_and_time_d
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_random_r
	.type	.LCPI66_0,@object
.LCPI66_0:
	.word	0x33800000                      # float 5.96046448E-8
	.size	.LCPI66_0, 4
	.text
	.globl	f77_random_r
	.p2align	2
	.type	f77_random_r,@function
f77_random_r:                           # @f77_random_r
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	lui r1, %hi(f77_rng+4)
	ldw r4, r1+%lo(f77_rng+4)
	lui r5, %hi(f77_rng)
	slli r6, r4, 2
	add r6, r6, r4
	slli r7, r4, 9
	srli r8, r6, 25
	slli r6, r6, 7
	or  r6, r6, r8
	slli r8, r6, 3
	add r6, r8, r6
	ldw r8, r5+%lo(f77_rng)
	lui r9, %hi(f77_rng+8)
	ldw r10, r9+%lo(f77_rng+8)
	xor r10, r10, r8
	lui lr, %hi(f77_rng+12)
	ldw r11, lr+%lo(f77_rng+12)
	xor r11, r11, r4
	xor r4, r10, r4
	stw r1+%lo(f77_rng+4), r4
	xor r1, r11, r8
	stw r5+%lo(f77_rng), r1
	xor r1, r10, r7
	stw r9+%lo(f77_rng+8), r1
	srli r1, r11, 21
	slli r4, r11, 11
	or  r1, r4, r1
	stw lr+%lo(f77_rng+12), r1
	srli r1, r6, 8
	fcvt.s.wu r1, r1
	lui r4, %hi(.LCPI66_0)
	ldw r4, r4+%lo(.LCPI66_0)
	fmul.s r1, r1, r4
	stw r3+0, r1
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end66:
	.size	f77_random_r, .Lfunc_end66-f77_random_r
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_random_d
	.type	.LCPI67_0,@object
.LCPI67_0:
	.quad	0x4190000000000000              # double 67108864
	.size	.LCPI67_0, 8
	.type	.LCPI67_1,@object
.LCPI67_1:
	.quad	0x3ca0000000000000              # double 1.1102230246251565E-16
	.size	.LCPI67_1, 8
	.text
	.globl	f77_random_d
	.p2align	2
	.type	f77_random_d,@function
f77_random_d:                           # @f77_random_d
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	lui r1, %hi(f77_rng+4)
	ldw r4, r1+%lo(f77_rng+4)
	lui r5, %hi(f77_rng)
	slli r6, r4, 2
	add r6, r6, r4
	slli r7, r4, 9
	srli r8, r6, 25
	slli r6, r6, 7
	or  r6, r6, r8
	slli r8, r6, 3
	add r6, r8, r6
	ldw r8, r5+%lo(f77_rng)
	lui r9, %hi(f77_rng+8)
	ldw r10, r9+%lo(f77_rng+8)
	xor r10, r10, r8
	lui lr, %hi(f77_rng+12)
	ldw r11, lr+%lo(f77_rng+12)
	xor r11, r11, r4
	xor r4, r10, r4
	xor r8, r11, r8
	xor r7, r10, r7
	srli r10, r11, 21
	slli r11, r11, 11
	or  r10, r11, r10
	srli r6, r6, 5
	slli r11, r4, 2
	add r11, r11, r4
	slli r12, r4, 9
	srli r13, r11, 25
	slli r11, r11, 7
	or  r11, r11, r13
	slli r13, r11, 3
	add r11, r13, r11
	xor r7, r8, r7
	xor r10, r10, r4
	xor r4, r7, r4
	stw r1+%lo(f77_rng+4), r4
	xor r1, r10, r8
	stw r5+%lo(f77_rng), r1
	xor r1, r7, r12
	stw r9+%lo(f77_rng+8), r1
	srli r1, r10, 21
	slli r4, r10, 11
	or  r1, r4, r1
	stw lr+%lo(f77_rng+12), r1
	srli r1, r11, 6
	fcvt.d.wu r4, r6
	lui r6, %hi(.LCPI67_0)
	addi r6, r6, %lo(.LCPI67_0)
	ldw r9, r6+4
	ldw r8, r6+0
	fmul.d r4, r4, r8
	fcvt.d.wu r6, r1
	fadd.d r4, r4, r6
	lui r1, %hi(.LCPI67_1)
	addi r1, r1, %lo(.LCPI67_1)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	stw r3+4, r5
	stw r3+0, r4
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end67:
	.size	f77_random_d, .Lfunc_end67-f77_random_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_random_desc
	.type	.LCPI68_0,@object
.LCPI68_0:
	.quad	0x4190000000000000              # double 67108864
	.size	.LCPI68_0, 8
	.type	.LCPI68_1,@object
.LCPI68_1:
	.quad	0x3ca0000000000000              # double 1.1102230246251565E-16
	.size	.LCPI68_1, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI68_2,@object
.LCPI68_2:
	.word	0x33800000                      # float 5.96046448E-8
	.size	.LCPI68_2, 4
	.text
	.globl	f77_random_desc
	.p2align	2
	.type	f77_random_desc,@function
f77_random_desc:                        # @f77_random_desc
# %bb.0:
	addi sp, sp, -96
	stw sp+0, lr
	stw sp+92, r11
	stw sp+88, r12
	stw sp+84, r13
	stw sp+80, r14
	stw sp+76, r15
	stw sp+72, r16
	stw sp+68, r17
	stw sp+64, r18
	stw sp+60, r19
	stw sp+56, r20
	stw sp+52, r21
	stw sp+48, r22
	stw sp+44, r23
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	addi r3, sp, 16
	addi r5, r0, 28
	add r4, r0, r0
	jal r31, memset
	addi r3, r0, 1
	add r1, r3, r0
	blt r12, r3, .LBB68_3
.LBB68_1:
	addi r4, r13, 4
	addi r1, r0, 1
	addi r5, r0, 0
	add r6, r12, r0
.LBB68_2:
	ldw r7, r4+0
	sgt r8, r7, r5
	mul r1, r7, r1
	sub r7, r0, r8
	and r1, r1, r7
	addi r6, r6, -1
	addi r4, r4, 8
	bne r6, r0, .LBB68_2
.LBB68_3:
	blt r1, r3, .LBB68_17
.LBB68_4:
	addi r4, r13, 4
	lui r3, %hi(f77_rng+4)
	ldw r6, r3+%lo(f77_rng+4)
	lui r5, %hi(f77_rng)
	ldw r8, r5+%lo(f77_rng)
	lui r7, %hi(f77_rng+8)
	ldw lr, r7+%lo(f77_rng+8)
	lui r9, %hi(f77_rng+12)
	ldw r15, r9+%lo(f77_rng+12)
	addi r10, r13, 8
	addi r14, r0, 1
	add r16, r0, r0
	jal r0, .LBB68_6
.LBB68_5:
	addi r16, r16, 1
	beq r16, r1, .LBB68_16
.LBB68_6:
	blt r12, r14, .LBB68_9
.LBB68_7:
	addi r18, sp, 16
	add r19, r10, r0
	add r20, r12, r0
	add r17, r0, r0
.LBB68_8:
	ldw r21, r18+0
	ldw r22, r19+0
	mul r21, r22, r21
	add r17, r21, r17
	addi r20, r20, -1
	addi r19, r19, 8
	addi r18, r18, 4
	bne r20, r0, .LBB68_8
	jal r0, .LBB68_10
.LBB68_9:
	add r17, r0, r0
.LBB68_10:
	slli r18, r6, 2
	add r18, r18, r6
	slli r19, r6, 9
	srli r20, r18, 25
	slli r18, r18, 7
	or  r18, r18, r20
	slli r20, r18, 3
	add r18, r20, r18
	xor lr, lr, r8
	xor r15, r15, r6
	xor r6, lr, r6
	xor r8, r15, r8
	xor lr, lr, r19
	srli r19, r15, 21
	slli r15, r15, 11
	or  r15, r15, r19
	beq r11, r0, .LBB68_12
.LBB68_11:
	srli r18, r18, 5
	slli r19, r6, 2
	add r19, r19, r6
	slli r20, r6, 9
	srli r21, r19, 25
	slli r19, r19, 7
	or  r19, r19, r21
	slli r21, r19, 3
	add r19, r21, r19
	xor lr, r8, lr
	xor r15, r15, r6
	xor r6, lr, r6
	xor r8, r15, r8
	xor lr, lr, r20
	srli r20, r15, 21
	slli r15, r15, 11
	or  r15, r15, r20
	srli r19, r19, 6
	fcvt.d.wu r20, r18
	lui r18, %hi(.LCPI68_0)
	addi r18, r18, %lo(.LCPI68_0)
	ldw r23, r18+4
	ldw r22, r18+0
	fmul.d r20, r20, r22
	fcvt.d.wu r18, r19
	fadd.d r18, r20, r18
	lui r20, %hi(.LCPI68_1)
	addi r20, r20, %lo(.LCPI68_1)
	ldw r23, r20+4
	ldw r22, r20+0
	fmul.d r18, r18, r22
	ldw r20, r13+0
	slli r17, r17, 3
	add r17, r20, r17
	stw r17+4, r19
	stw r17+0, r18
	bge r12, r14, .LBB68_13
	jal r0, .LBB68_5
.LBB68_12:
	srli r18, r18, 8
	fcvt.s.wu r18, r18
	lui r19, %hi(.LCPI68_2)
	ldw r19, r19+%lo(.LCPI68_2)
	fmul.s r18, r18, r19
	ldw r19, r13+0
	slli r17, r17, 2
	add r17, r19, r17
	stw r17+0, r18
	blt r12, r14, .LBB68_5
.LBB68_13:
	addi r17, sp, 16
	add r18, r4, r0
	add r19, r12, r0
.LBB68_14:
	ldw r20, r17+0
	addi r20, r20, 1
	stw r17+0, r20
	ldw r21, r18+0
	blt r20, r21, .LBB68_5
.LBB68_15:
	stw r17+0, r0
	addi r19, r19, -1
	addi r18, r18, 8
	addi r17, r17, 4
	bne r19, r0, .LBB68_14
	jal r0, .LBB68_5
.LBB68_16:
	stw r3+%lo(f77_rng+4), r6
	stw r5+%lo(f77_rng), r8
	stw r7+%lo(f77_rng+8), lr
	stw r9+%lo(f77_rng+12), r15
.LBB68_17:
	ldw r23, sp+44
	ldw r22, sp+48
	ldw r21, sp+52
	ldw r20, sp+56
	ldw r19, sp+60
	ldw r18, sp+64
	ldw r17, sp+68
	ldw r16, sp+72
	ldw r15, sp+76
	ldw r14, sp+80
	ldw r13, sp+84
	ldw r12, sp+88
	ldw r11, sp+92
	ldw lr, sp+0
	addi sp, sp, 96
	jalr r0, r31, 0
.Lfunc_end68:
	.size	f77_random_desc, .Lfunc_end68-f77_random_desc
                                        # -- End function
	.globl	f77_len_trim                    # -- Begin function f77_len_trim
	.p2align	2
	.type	f77_len_trim,@function
f77_len_trim:                           # @f77_len_trim
# %bb.0:
	srai r1, r4, 31
	and r1, r4, r1
	addi r5, r0, 1
	blt r4, r5, .LBB69_5
.LBB69_1:
	addi r3, r3, -1
	addi r5, r0, 32
	addi r6, r0, 2
.LBB69_2:
	add r7, r4, r0
	add r4, r3, r4
	ldbu r4, r4+0
	bne r4, r5, .LBB69_4
.LBB69_3:
	addi r4, r7, -1
	bge r7, r6, .LBB69_2
	jal r0, .LBB69_5
.LBB69_4:
	add r1, r7, r0
.LBB69_5:
	jalr r0, r31, 0
.Lfunc_end69:
	.size	f77_len_trim, .Lfunc_end69-f77_len_trim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_exponent
	.type	.LCPI70_0,@object
.LCPI70_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI70_0, 8
	.text
	.globl	f77_exponent
	.p2align	2
	.type	f77_exponent,@function
f77_exponent:                           # @f77_exponent
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI70_0)
	addi r1, r1, %lo(.LCPI70_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r6, r4
	add r1, r0, r0
	bne r3, r0, .LBB70_2
.LBB70_1:
	addi sp, sp, -32
	stw sp+0, lr
	addi r5, sp, 28
	add r3, r6, r0
	add r4, r7, r0
	jal r31, frexp
	ldw r1, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB70_2:
	jalr r0, r31, 0
.Lfunc_end70:
	.size	f77_exponent, .Lfunc_end70-f77_exponent
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zsqrt
	.type	.LCPI71_0,@object
.LCPI71_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI71_0, 8
	.type	.LCPI71_1,@object
.LCPI71_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI71_1, 8
	.text
	.globl	f77_zsqrt
	.p2align	2
	.type	f77_zsqrt,@function
f77_zsqrt:                              # @f77_zsqrt
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
	add r11, r3, r0
	ldw r19, r4+4
	ldw r18, r4+0
	ldw r13, r4+12
	ldw r12, r4+8
	add r3, r18, r0
	add r4, r19, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, hypot
	add r14, r1, r0
	add r15, r2, r0
	add r3, r18, r0
	add r4, r19, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r4, r14, r4
	lui r1, %hi(.LCPI71_0)
	addi r1, r1, %lo(.LCPI71_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r14, r1, r0
	add r15, r2, r0
	lui r1, %hi(.LCPI71_1)
	addi r1, r1, %lo(.LCPI71_1)
	ldw r17, r1+4
	ldw r16, r1+0
	fle.d r1, r16, r18
	xori r1, r1, 1
	beq r1, r0, .LBB71_4
.LBB71_1:
	feq.d r1, r14, r16
	add r4, r16, r0
	add r5, r17, r0
	bne r1, r0, .LBB71_3
.LBB71_2:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r14, r14
	fdiv.d r4, r4, r6
.LBB71_3:
	stw r11+4, r5
	stw r11+0, r4
	flt.d r1, r12, r16
	fneg.d r4, r14
	xor r3, r5, r15
	sub r1, r0, r1
	and r3, r3, r1
	xor r17, r15, r3
	xor r3, r4, r14
	and r1, r3, r1
	xor r16, r14, r1
	jal r0, .LBB71_6
.LBB71_4:
	stw r11+4, r15
	stw r11+0, r14
	feq.d r1, r14, r16
	bne r1, r0, .LBB71_6
.LBB71_5:
	fadd.d r4, r14, r14
	fdiv.d r16, r12, r4
.LBB71_6:
	stw r11+12, r17
	stw r11+8, r16
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
.Lfunc_end71:
	.size	f77_zsqrt, .Lfunc_end71-f77_zsqrt
                                        # -- End function
	.globl	f77_zexp                        # -- Begin function f77_zexp
	.p2align	2
	.type	f77_zexp,@function
f77_zexp:                               # @f77_zexp
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	add r11, r4, r0
	add r12, r3, r0
	ldw r4, r4+4
	ldw r3, r11+0
	jal r31, exp
	add r14, r1, r0
	add r15, r2, r0
	ldw r13, r11+12
	ldw r11, r11+8
	add r3, r11, r0
	add r4, r13, r0
	jal r31, cos
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r14, r4
	stw r12+4, r5
	stw r12+0, r4
	add r3, r11, r0
	add r4, r13, r0
	jal r31, sin
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r14, r4
	stw r12+12, r5
	stw r12+8, r4
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end72:
	.size	f77_zexp, .Lfunc_end72-f77_zexp
                                        # -- End function
	.globl	f77_zlog                        # -- Begin function f77_zlog
	.p2align	2
	.type	f77_zlog,@function
f77_zlog:                               # @f77_zlog
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	add r11, r3, r0
	ldw r12, r4+4
	ldw r13, r4+0
	ldw r14, r4+12
	ldw r15, r4+8
	add r3, r13, r0
	add r4, r12, r0
	add r5, r15, r0
	add r6, r14, r0
	jal r31, hypot
	add r3, r1, r0
	add r4, r2, r0
	jal r31, log
	stw r11+4, r2
	stw r11+0, r1
	add r3, r15, r0
	add r4, r14, r0
	add r5, r13, r0
	add r6, r12, r0
	jal r31, atan2
	stw r11+12, r2
	stw r11+8, r1
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end73:
	.size	f77_zlog, .Lfunc_end73-f77_zlog
                                        # -- End function
	.globl	f77_zsin                        # -- Begin function f77_zsin
	.p2align	2
	.type	f77_zsin,@function
f77_zsin:                               # @f77_zsin
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
	ldw r12, r4+4
	ldw r13, r4+0
	ldw r14, r4+12
	ldw r15, r4+8
	add r3, r13, r0
	add r4, r12, r0
	jal r31, sin
	add r16, r1, r0
	add r17, r2, r0
	add r3, r15, r0
	add r4, r14, r0
	jal r31, cosh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r16, r4
	stw r11+4, r5
	stw r11+0, r4
	add r3, r13, r0
	add r4, r12, r0
	jal r31, cos
	add r12, r1, r0
	add r13, r2, r0
	add r3, r15, r0
	add r4, r14, r0
	jal r31, sinh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r12, r4
	stw r11+12, r5
	stw r11+8, r4
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
.Lfunc_end74:
	.size	f77_zsin, .Lfunc_end74-f77_zsin
                                        # -- End function
	.globl	f77_zcos                        # -- Begin function f77_zcos
	.p2align	2
	.type	f77_zcos,@function
f77_zcos:                               # @f77_zcos
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
	ldw r12, r4+4
	ldw r13, r4+0
	ldw r14, r4+12
	ldw r15, r4+8
	add r3, r13, r0
	add r4, r12, r0
	jal r31, cos
	add r16, r1, r0
	add r17, r2, r0
	add r3, r15, r0
	add r4, r14, r0
	jal r31, cosh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r16, r4
	stw r11+4, r5
	stw r11+0, r4
	add r3, r13, r0
	add r4, r12, r0
	jal r31, sin
	add r4, r1, r0
	add r5, r2, r0
	fneg.d r12, r4
	add r3, r15, r0
	add r4, r14, r0
	jal r31, sinh
	add r4, r1, r0
	add r5, r2, r0
	fmul.d r4, r4, r12
	stw r11+12, r5
	stw r11+8, r4
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
.Lfunc_end75:
	.size	f77_zcos, .Lfunc_end75-f77_zcos
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
.Lfunc_end76:
	.size	f77i_iabs, .Lfunc_end76-f77i_iabs
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_abs
	.type	.LCPI77_0,@object
.LCPI77_0:
	.word	0x00000000                      # float 0
	.size	.LCPI77_0, 4
	.text
	.globl	f77i_abs
	.p2align	2
	.type	f77i_abs,@function
f77i_abs:                               # @f77i_abs
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI77_0)
	ldw r3, r3+%lo(.LCPI77_0)
	flt.s r3, r1, r3
	sub r3, r0, r3
	lui r4, 524288
	and r3, r3, r4
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end77:
	.size	f77i_abs, .Lfunc_end77-f77i_abs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dabs
	.type	.LCPI78_0,@object
.LCPI78_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI78_0, 8
	.text
	.globl	f77i_dabs
	.p2align	2
	.type	f77i_dabs,@function
f77i_dabs:                              # @f77i_dabs
# %bb.0:
	ldw r5, r3+4
	ldw r4, r3+0
	lui r1, %hi(.LCPI78_0)
	addi r1, r1, %lo(.LCPI78_0)
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
.Lfunc_end78:
	.size	f77i_dabs, .Lfunc_end78-f77i_dabs
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
.Lfunc_end79:
	.size	f77i_sqrt, .Lfunc_end79-f77i_sqrt
                                        # -- End function
	.globl	f77i_dsqrt                      # -- Begin function f77i_dsqrt
	.p2align	2
	.type	f77i_dsqrt,@function
f77i_dsqrt:                             # @f77i_dsqrt
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sqrt
.Lfunc_end80:
	.size	f77i_dsqrt, .Lfunc_end80-f77i_dsqrt
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
.Lfunc_end81:
	.size	f77i_exp, .Lfunc_end81-f77i_exp
                                        # -- End function
	.globl	f77i_dexp                       # -- Begin function f77i_dexp
	.p2align	2
	.type	f77i_dexp,@function
f77i_dexp:                              # @f77i_dexp
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, exp
.Lfunc_end82:
	.size	f77i_dexp, .Lfunc_end82-f77i_dexp
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
.Lfunc_end83:
	.size	f77i_alog, .Lfunc_end83-f77i_alog
                                        # -- End function
	.globl	f77i_dlog                       # -- Begin function f77i_dlog
	.p2align	2
	.type	f77i_dlog,@function
f77i_dlog:                              # @f77i_dlog
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log
.Lfunc_end84:
	.size	f77i_dlog, .Lfunc_end84-f77i_dlog
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
.Lfunc_end85:
	.size	f77i_alog10, .Lfunc_end85-f77i_alog10
                                        # -- End function
	.globl	f77i_dlog10                     # -- Begin function f77i_dlog10
	.p2align	2
	.type	f77i_dlog10,@function
f77i_dlog10:                            # @f77i_dlog10
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log10
.Lfunc_end86:
	.size	f77i_dlog10, .Lfunc_end86-f77i_dlog10
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
.Lfunc_end87:
	.size	f77i_sin, .Lfunc_end87-f77i_sin
                                        # -- End function
	.globl	f77i_dsin                       # -- Begin function f77i_dsin
	.p2align	2
	.type	f77i_dsin,@function
f77i_dsin:                              # @f77i_dsin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sin
.Lfunc_end88:
	.size	f77i_dsin, .Lfunc_end88-f77i_dsin
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
.Lfunc_end89:
	.size	f77i_cos, .Lfunc_end89-f77i_cos
                                        # -- End function
	.globl	f77i_dcos                       # -- Begin function f77i_dcos
	.p2align	2
	.type	f77i_dcos,@function
f77i_dcos:                              # @f77i_dcos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cos
.Lfunc_end90:
	.size	f77i_dcos, .Lfunc_end90-f77i_dcos
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
.Lfunc_end91:
	.size	f77i_tan, .Lfunc_end91-f77i_tan
                                        # -- End function
	.globl	f77i_dtan                       # -- Begin function f77i_dtan
	.p2align	2
	.type	f77i_dtan,@function
f77i_dtan:                              # @f77i_dtan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tan
.Lfunc_end92:
	.size	f77i_dtan, .Lfunc_end92-f77i_dtan
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
.Lfunc_end93:
	.size	f77i_asin, .Lfunc_end93-f77i_asin
                                        # -- End function
	.globl	f77i_dasin                      # -- Begin function f77i_dasin
	.p2align	2
	.type	f77i_dasin,@function
f77i_dasin:                             # @f77i_dasin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, asin
.Lfunc_end94:
	.size	f77i_dasin, .Lfunc_end94-f77i_dasin
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
.Lfunc_end95:
	.size	f77i_acos, .Lfunc_end95-f77i_acos
                                        # -- End function
	.globl	f77i_dacos                      # -- Begin function f77i_dacos
	.p2align	2
	.type	f77i_dacos,@function
f77i_dacos:                             # @f77i_dacos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, acos
.Lfunc_end96:
	.size	f77i_dacos, .Lfunc_end96-f77i_dacos
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
.Lfunc_end97:
	.size	f77i_atan, .Lfunc_end97-f77i_atan
                                        # -- End function
	.globl	f77i_datan                      # -- Begin function f77i_datan
	.p2align	2
	.type	f77i_datan,@function
f77i_datan:                             # @f77i_datan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, atan
.Lfunc_end98:
	.size	f77i_datan, .Lfunc_end98-f77i_datan
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
.Lfunc_end99:
	.size	f77i_atan2, .Lfunc_end99-f77i_atan2
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
.Lfunc_end100:
	.size	f77i_datan2, .Lfunc_end100-f77i_datan2
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
.Lfunc_end101:
	.size	f77i_sinh, .Lfunc_end101-f77i_sinh
                                        # -- End function
	.globl	f77i_dsinh                      # -- Begin function f77i_dsinh
	.p2align	2
	.type	f77i_dsinh,@function
f77i_dsinh:                             # @f77i_dsinh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sinh
.Lfunc_end102:
	.size	f77i_dsinh, .Lfunc_end102-f77i_dsinh
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
.Lfunc_end103:
	.size	f77i_cosh, .Lfunc_end103-f77i_cosh
                                        # -- End function
	.globl	f77i_dcosh                      # -- Begin function f77i_dcosh
	.p2align	2
	.type	f77i_dcosh,@function
f77i_dcosh:                             # @f77i_dcosh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cosh
.Lfunc_end104:
	.size	f77i_dcosh, .Lfunc_end104-f77i_dcosh
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
.Lfunc_end105:
	.size	f77i_tanh, .Lfunc_end105-f77i_tanh
                                        # -- End function
	.globl	f77i_dtanh                      # -- Begin function f77i_dtanh
	.p2align	2
	.type	f77i_dtanh,@function
f77i_dtanh:                             # @f77i_dtanh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tanh
.Lfunc_end106:
	.size	f77i_dtanh, .Lfunc_end106-f77i_dtanh
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
.Lfunc_end107:
	.size	f77i_aint, .Lfunc_end107-f77i_aint
                                        # -- End function
	.globl	f77i_dint                       # -- Begin function f77i_dint
	.p2align	2
	.type	f77i_dint,@function
f77i_dint:                              # @f77i_dint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, trunc
.Lfunc_end108:
	.size	f77i_dint, .Lfunc_end108-f77i_dint
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
.Lfunc_end109:
	.size	f77i_anint, .Lfunc_end109-f77i_anint
                                        # -- End function
	.globl	f77i_dnint                      # -- Begin function f77i_dnint
	.p2align	2
	.type	f77i_dnint,@function
f77i_dnint:                             # @f77i_dnint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, round
.Lfunc_end110:
	.size	f77i_dnint, .Lfunc_end110-f77i_dnint
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
.Lfunc_end111:
	.size	f77i_nint, .Lfunc_end111-f77i_nint
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
.Lfunc_end112:
	.size	f77i_idnint, .Lfunc_end112-f77i_idnint
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
.Lfunc_end113:
	.size	f77i_mod, .Lfunc_end113-f77i_mod
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
.Lfunc_end114:
	.size	f77i_amod, .Lfunc_end114-f77i_amod
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
.Lfunc_end115:
	.size	f77i_dmod, .Lfunc_end115-f77i_dmod
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
.Lfunc_end116:
	.size	f77i_isign, .Lfunc_end116-f77i_isign
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_sign
	.type	.LCPI117_0,@object
.LCPI117_0:
	.word	0x00000000                      # float 0
	.size	.LCPI117_0, 4
	.text
	.globl	f77i_sign
	.p2align	2
	.type	f77i_sign,@function
f77i_sign:                              # @f77i_sign
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI117_0)
	ldw r3, r3+%lo(.LCPI117_0)
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
.Lfunc_end117:
	.size	f77i_sign, .Lfunc_end117-f77i_sign
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dsign
	.type	.LCPI118_0,@object
.LCPI118_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI118_0, 8
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
	lui r1, %hi(.LCPI118_0)
	addi r1, r1, %lo(.LCPI118_0)
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
.Lfunc_end118:
	.size	f77i_dsign, .Lfunc_end118-f77i_dsign
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
.Lfunc_end119:
	.size	f77i_idim, .Lfunc_end119-f77i_idim
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
.Lfunc_end120:
	.size	f77i_dim, .Lfunc_end120-f77i_dim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_ddim
	.type	.LCPI121_0,@object
.LCPI121_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI121_0, 8
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
	lui r3, %hi(.LCPI121_0)
	addi r3, r3, %lo(.LCPI121_0)
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
.Lfunc_end121:
	.size	f77i_ddim, .Lfunc_end121-f77i_ddim
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
.Lfunc_end122:
	.size	f77i_dprod, .Lfunc_end122-f77i_dprod
                                        # -- End function
	.globl	f77i_len                        # -- Begin function f77i_len
	.p2align	2
	.type	f77i_len,@function
f77i_len:                               # @f77i_len
# %bb.0:
	add r1, r4, r0
	jalr r0, r31, 0
.Lfunc_end123:
	.size	f77i_len, .Lfunc_end123-f77i_len
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
	beq r6, r0, .LBB124_6
.LBB124_1:
	add r12, r6, r0
	bgt r6, r11, .LBB124_5
.LBB124_2:
	add r13, r4, r0
	add r14, r3, r0
	add r15, r0, r0
.LBB124_3:
	add r3, r14, r15
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcmp
	beq r1, r0, .LBB124_7
.LBB124_4:
	addi r15, r15, 1
	add r1, r12, r15
	ble r1, r11, .LBB124_3
.LBB124_5:
	add r1, r0, r0
	jal r0, .LBB124_8
.LBB124_6:
	addi r1, r0, 0
	sgt r1, r11, r1
	jal r0, .LBB124_8
.LBB124_7:
	addi r1, r15, 1
.LBB124_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end124:
	.size	f77i_index, .Lfunc_end124-f77i_index
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
	beq r6, r0, .LBB125_6
.LBB125_1:
	add r12, r6, r0
	bgt r6, r11, .LBB125_5
.LBB125_2:
	add r13, r5, r0
	add r14, r3, r0
	add r15, r0, r0
.LBB125_3:
	add r3, r14, r15
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcmp
	beq r1, r0, .LBB125_7
.LBB125_4:
	addi r15, r15, 1
	add r1, r12, r15
	ble r1, r11, .LBB125_3
.LBB125_5:
	add r1, r0, r0
	jal r0, .LBB125_8
.LBB125_6:
	addi r1, r0, 0
	sgt r1, r11, r1
	jal r0, .LBB125_8
.LBB125_7:
	addi r1, r15, 1
.LBB125_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end125:
	.size	f77_index, .Lfunc_end125-f77_index
                                        # -- End function
	.globl	f77i_aimag                      # -- Begin function f77i_aimag
	.p2align	2
	.type	f77i_aimag,@function
f77i_aimag:                             # @f77i_aimag
# %bb.0:
	ldw r1, r3+4
	jalr r0, r31, 0
.Lfunc_end126:
	.size	f77i_aimag, .Lfunc_end126-f77i_aimag
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
.Lfunc_end127:
	.size	f77i_cabs, .Lfunc_end127-f77i_cabs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_csqrt
	.type	.LCPI128_0,@object
.LCPI128_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI128_0, 8
	.type	.LCPI128_2,@object
.LCPI128_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI128_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI128_1,@object
.LCPI128_1:
	.word	0x00000000                      # float 0
	.size	.LCPI128_1, 4
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
	lui r1, %hi(.LCPI128_0)
	addi r1, r1, %lo(.LCPI128_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r14, r1, r0
	add r15, r2, r0
	lui r1, %hi(.LCPI128_1)
	ldw r3, r1+%lo(.LCPI128_1)
	fle.s r4, r3, r16
	xori r4, r4, 1
	beq r4, r0, .LBB128_4
.LBB128_1:
	ldw r16, r1+%lo(.LCPI128_1)
	lui r1, %hi(.LCPI128_2)
	addi r1, r1, %lo(.LCPI128_2)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r14, r4
	add r1, r16, r0
	bne r3, r0, .LBB128_3
.LBB128_2:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r14, r14
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
.LBB128_3:
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
	jal r0, .LBB128_6
.LBB128_4:
	fcvt.s.d r1, r14
	lui r4, %hi(.LCPI128_2)
	addi r4, r4, %lo(.LCPI128_2)
	ldw r7, r4+4
	ldw r6, r4+0
	feq.d r4, r14, r6
	bne r4, r0, .LBB128_6
.LBB128_5:
	fadd.d r4, r14, r14
	fdiv.d r4, r12, r4
	fcvt.s.d r3, r4
.LBB128_6:
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
.Lfunc_end128:
	.size	f77i_csqrt, .Lfunc_end128-f77i_csqrt
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
.Lfunc_end129:
	.size	f77i_cexp, .Lfunc_end129-f77i_cexp
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
.Lfunc_end130:
	.size	f77i_clog, .Lfunc_end130-f77i_clog
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
.Lfunc_end131:
	.size	f77i_csin, .Lfunc_end131-f77i_csin
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
.Lfunc_end132:
	.size	f77i_ccos, .Lfunc_end132-f77i_ccos
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
.Lfunc_end133:
	.size	f77i_conjg, .Lfunc_end133-f77i_conjg
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
	blt r14, r1, .LBB134_2
.LBB134_1:
	add r3, r12, r0
	add r4, r5, r0
	add r5, r14, r0
	jal r31, memmove
.LBB134_2:
	bge r13, r11, .LBB134_5
.LBB134_3:
	add r1, r12, r14
	sub r3, r11, r14
	addi r4, r0, 32
.LBB134_4:
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB134_4
.LBB134_5:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end134:
	.size	f77_strcpy, .Lfunc_end134-f77_strcpy
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
	blt r7, r1, .LBB135_8
.LBB135_1:
	addi r8, r0, 32
	add r9, r0, r0
                                        # implicit-def: $r1
.LBB135_2:
	add r10, r8, r0
	bge r9, r4, .LBB135_4
.LBB135_3:
	add r10, r3, r9
	ldbu r10, r10+0
.LBB135_4:
	add lr, r8, r0
	bge r9, r6, .LBB135_6
.LBB135_5:
	add lr, r5, r9
	ldbu lr, lr+0
.LBB135_6:
	seq r11, r10, lr
	sltu r12, r10, lr
	sub r12, r0, r12
	ori  r12, r12, 1
	xor r1, r1, r12
	sub r11, r0, r11
	and r1, r1, r11
	xor r1, r12, r1
	bne r10, lr, .LBB135_9
.LBB135_7:
	addi r9, r9, 1
	bne r7, r9, .LBB135_2
.LBB135_8:
	add r1, r0, r0
.LBB135_9:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end135:
	.size	f77_strcmp, .Lfunc_end135-f77_strcmp
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
	bgeu r5, r6, .LBB136_8
.LBB136_1:
	lui r3, %hi(fio_catpos)
	ldw r7, r3+%lo(fio_catpos)
	add r7, r7, r5
	blt r7, r6, .LBB136_3
.LBB136_2:
	stw r3+%lo(fio_catpos), r0
.LBB136_3:
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
	blt r1, r15, .LBB136_5
.LBB136_4:
	add r3, r13, r0
	add r5, r1, r0
	jal r31, memmove
.LBB136_5:
	blt r11, r15, .LBB136_7
.LBB136_6:
	add r3, r13, r14
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memmove
.LBB136_7:
	add r1, r13, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB136_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end136:
	.size	f77_cat, .Lfunc_end136-f77_cat
                                        # -- End function
	.globl	f77_repeat                      # -- Begin function f77_repeat
	.p2align	2
	.type	f77_repeat,@function
f77_repeat:                             # @f77_repeat
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	add r11, r5, r0
	addi r1, r0, -1
	ble r5, r1, .LBB137_8
.LBB137_1:
	add r12, r3, r0
	addi r1, r0, 0
	sgt r1, r4, r1
	sub r1, r0, r1
	and r13, r4, r1
	mul r5, r13, r11
	lui r1, 16
	addi r3, r1, 1
	bge r5, r3, .LBB137_9
.LBB137_2:
	lui r1, %hi(fio_catpos)
	ldw r4, r1+%lo(fio_catpos)
	add r4, r4, r5
	blt r4, r3, .LBB137_4
.LBB137_3:
	stw r1+%lo(fio_catpos), r0
.LBB137_4:
	ldw r3, r1+%lo(fio_catpos)
	lui r4, %hi(fio_catring)
	addi r4, r4, %lo(fio_catring)
	add r14, r3, r4
	addi r4, r5, 3
	lui r5, 524288
	addi r5, r5, -4
	and r4, r4, r5
	add r3, r3, r4
	stw r1+%lo(fio_catpos), r3
	beq r11, r0, .LBB137_7
.LBB137_5:
	add r15, r14, r0
.LBB137_6:
	add r3, r15, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, memmove
	addi r11, r11, -1
	add r15, r15, r13
	bne r11, r0, .LBB137_6
.LBB137_7:
	add r1, r14, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB137_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	add r5, r11, r0
	jal r0, .LBB137_10
.LBB137_9:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.106)
	addi r4, r4, %lo(.L.str.106)
.LBB137_10:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end137:
	.size	f77_repeat, .Lfunc_end137-f77_repeat
                                        # -- End function
	.globl	f77_adjust                      # -- Begin function f77_adjust
	.p2align	2
	.type	f77_adjust,@function
f77_adjust:                             # @f77_adjust
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r11, r4, r0
	add r13, r3, r0
	lui r1, 16
	addi r3, r1, 1
	bge r4, r3, .LBB138_18
.LBB138_1:
	lui r1, %hi(fio_catpos)
	ldw r4, r1+%lo(fio_catpos)
	add r4, r4, r11
	blt r4, r3, .LBB138_3
.LBB138_2:
	stw r1+%lo(fio_catpos), r0
.LBB138_3:
	ldw r3, r1+%lo(fio_catpos)
	lui r4, %hi(fio_catring)
	addi r4, r4, %lo(fio_catring)
	add r12, r3, r4
	addi r4, r11, 3
	addi r6, r0, -4
	and r4, r4, r6
	add r3, r3, r4
	stw r1+%lo(fio_catpos), r3
	addi r1, r0, 1
	beq r5, r0, .LBB138_10
.LBB138_4:
	add r14, r11, r0
	blt r11, r1, .LBB138_9
.LBB138_5:
	addi r3, r0, 32
	addi r4, r13, -1
	add r5, r11, r0
.LBB138_6:
	add r14, r5, r0
	add r5, r4, r5
	ldbu r5, r5+0
	bne r5, r3, .LBB138_9
.LBB138_7:
	addi r5, r14, -1
	bgt r14, r1, .LBB138_6
.LBB138_8:
	add r14, r0, r0
.LBB138_9:
	sub r5, r11, r14
	addi r4, r0, 32
	add r3, r12, r0
	jal r31, memset
	add r1, r12, r11
	sub r3, r1, r14
	add r4, r13, r0
	add r5, r14, r0
	jal r31, memmove
	jal r0, .LBB138_17
.LBB138_10:
	blt r11, r1, .LBB138_15
.LBB138_11:
	addi r1, r0, 32
	add r14, r0, r0
.LBB138_12:
	add r3, r13, r14
	ldbu r3, r3+0
	bne r3, r1, .LBB138_16
.LBB138_13:
	addi r14, r14, 1
	bne r11, r14, .LBB138_12
.LBB138_14:
	add r14, r11, r0
	jal r0, .LBB138_16
.LBB138_15:
	add r14, r0, r0
.LBB138_16:
	add r4, r13, r14
	sub r5, r11, r14
	add r3, r12, r0
	jal r31, memmove
	add r1, r12, r11
	sub r3, r1, r14
	addi r4, r0, 32
	add r5, r14, r0
	jal r31, memset
.LBB138_17:
	add r1, r12, r0
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB138_18:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.106)
	addi r4, r4, %lo(.L.str.106)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end138:
	.size	f77_adjust, .Lfunc_end138-f77_adjust
                                        # -- End function
	.globl	f77_scanset                     # -- Begin function f77_scanset
	.p2align	2
	.type	f77_scanset,@function
f77_scanset:                            # @f77_scanset
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
	addi r16, r0, 1
	blt r4, r16, .LBB139_7
.LBB139_1:
	add r11, r8, r0
	add r12, r6, r0
	add r13, r5, r0
	add r14, r3, r0
	addi r17, r4, -1
	addi r18, r0, 0
	seq r1, r7, r18
	sub r19, r0, r1
	addi r20, r0, -1
	add r21, r0, r0
                                        # implicit-def: $r15
.LBB139_2:
	xor r1, r21, r17
	and r1, r1, r19
	xor r22, r17, r1
	blt r12, r16, .LBB139_4
.LBB139_3:
	add r1, r14, r22
	ldb r4, r1+0
	add r3, r13, r0
	add r5, r12, r0
	jal r31, memchr
	sne r1, r1, r18
	jal r0, .LBB139_5
.LBB139_4:
	add r1, r0, r0
.LBB139_5:
	seq r3, r1, r11
	addi r4, r22, 1
	xor r5, r15, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r15, r4, r3
	bne r1, r11, .LBB139_8
.LBB139_6:
	addi r21, r21, 1
	addi r17, r17, -1
	bne r17, r20, .LBB139_2
.LBB139_7:
	add r15, r0, r0
.LBB139_8:
	add r1, r15, r0
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
.Lfunc_end139:
	.size	f77_scanset, .Lfunc_end139-f77_scanset
                                        # -- End function
	.globl	f77_index_back                  # -- Begin function f77_index_back
	.p2align	2
	.type	f77_index_back,@function
f77_index_back:                         # @f77_index_back
# %bb.0:
	ble r6, r0, .LBB140_6
.LBB140_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r11, r6, r0
	sub r1, r4, r6
	blt r1, r0, .LBB140_5
.LBB140_2:
	add r12, r5, r0
	add r13, r3, r0
.LBB140_3:
	add r14, r1, r0
	add r3, r13, r1
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memcmp
	beq r1, r0, .LBB140_7
.LBB140_4:
	addi r1, r14, -1
	bgt r14, r0, .LBB140_3
.LBB140_5:
	add r1, r0, r0
	jal r0, .LBB140_8
.LBB140_6:
	addi r1, r0, 0
	sgt r1, r4, r1
	sub r1, r0, r1
	and r1, r4, r1
	addi r1, r1, 1
	jal r0, .LBB140_9
.LBB140_7:
	addi r1, r14, 1
.LBB140_8:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB140_9:
	jalr r0, r31, 0
.Lfunc_end140:
	.size	f77_index_back, .Lfunc_end140-f77_index_back
                                        # -- End function
	.globl	f77_wr_l                        # -- Begin function f77_wr_l
	.p2align	2
	.type	f77_wr_l,@function
f77_wr_l:                               # @f77_wr_l
# %bb.0:
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB141_15
.LBB141_1:
	lui r4, %hi(fio_col)
	ldw r7, r4+%lo(fio_col)
	addi r6, r0, 1022
	lui r1, %hi(fio_len)
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	bgt r7, r6, .LBB141_7
.LBB141_2:
	ldw r9, r1+%lo(fio_len)
	addi r8, r0, 32
	bge r9, r7, .LBB141_5
.LBB141_3:
	addi r10, r9, 1
	add r9, r9, r5
	stb r9+0, r8
	add r9, r10, r0
	bne r7, r10, .LBB141_3
.LBB141_4:
	stw r1+%lo(fio_len), r7
	add r9, r7, r0
.LBB141_5:
	add r10, r7, r5
	stb r10+0, r8
	addi r8, r7, 1
	stw r4+%lo(fio_col), r8
	blt r7, r9, .LBB141_7
.LBB141_6:
	stw r1+%lo(fio_len), r8
.LBB141_7:
	ldw r7, r4+%lo(fio_col)
	bgt r7, r6, .LBB141_14
.LBB141_8:
	ldw r6, r1+%lo(fio_len)
	bge r6, r7, .LBB141_12
.LBB141_9:
	addi r8, r0, 32
.LBB141_10:
	addi r9, r6, 1
	add r6, r6, r5
	stb r6+0, r8
	add r6, r9, r0
	bne r7, r9, .LBB141_10
.LBB141_11:
	stw r1+%lo(fio_len), r7
	add r6, r7, r0
.LBB141_12:
	addi r8, r0, 0
	seq r3, r3, r8
	sub r3, r0, r3
	andi r3, r3, 18
	xori r3, r3, 84
	add r5, r7, r5
	stb r5+0, r3
	addi r3, r7, 1
	stw r4+%lo(fio_col), r3
	blt r7, r6, .LBB141_14
.LBB141_13:
	stw r1+%lo(fio_len), r3
.LBB141_14:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	jal r0, .LBB141_32
.LBB141_15:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB141_17
.LBB141_16:
	ldw r1, r12+%lo(fio_rep)
	bgt r1, r0, .LBB141_18
.LBB141_17:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	jal r31, fio_next_desc
	add r3, r13, r0
	beq r1, r0, .LBB141_31
.LBB141_18:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r4, r0, 2
	blt r1, r4, .LBB141_20
.LBB141_19:
	lui r4, %hi(fio_col)
	ldw r5, r4+%lo(fio_col)
	add r1, r1, r5
	addi r1, r1, -1
	stw r4+%lo(fio_col), r1
.LBB141_20:
	lui r4, %hi(fio_col)
	ldw r1, r4+%lo(fio_col)
	addi r5, r0, 1022
	bgt r1, r5, .LBB141_27
.LBB141_21:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	bge r6, r1, .LBB141_25
.LBB141_22:
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
.LBB141_23:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r1, r9, .LBB141_23
.LBB141_24:
	stw r5+%lo(fio_len), r1
	add r6, r1, r0
.LBB141_25:
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
	blt r1, r6, .LBB141_27
.LBB141_26:
	stw r5+%lo(fio_len), r3
.LBB141_27:
	ldw r1, r12+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB141_29
.LBB141_28:
	addi r1, r1, -1
	stw r12+%lo(fio_rep), r1
.LBB141_29:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB141_31
.LBB141_30:
	stw r11+%lo(fio_desc), r0
.LBB141_31:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB141_32:
	jalr r0, r31, 0
.Lfunc_end141:
	.size	f77_wr_l, .Lfunc_end141-f77_wr_l
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
	bne r1, r0, .LBB142_4
.LBB142_1:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB142_3
.LBB142_2:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB142_4
.LBB142_3:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB142_4:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end142:
	.size	f77_wr_end, .Lfunc_end142-f77_wr_end
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
	addi fp, r0, 1
	lui r15, %hi(fio_fmt)
	lui r14, %hi(fio_pos)
	addi r17, r0, 44
	lui r1, 24
	addi r18, r1, 1695
	addi r19, r0, 32
	lui r20, %hi(fio_gdepth)
	lui r21, %hi(fio_draining)
	lui r22, %hi(fio_revert)
	lui r13, %hi(fio_since_data)
	lui r23, %hi(fio_reading)
	addi r24, r0, -33
	addi r12, r0, 83
	addi r25, r0, 35
	lui r26, %hi(fio_sp)
	lui r27, %hi(.LJTI143_4)
	addi r27, r27, %lo(.LJTI143_4)
	addi r28, r0, 58
	addi r1, r0, 66
	stw sp+52, r1
	addi r1, r0, 11
	stw sp+44, r1
	addi r1, r0, 90
	stw sp+40, r1
	lui r1, %hi(fio_bz)
	stw sp+36, r1
	addi r1, r0, 8
	stw sp+48, r1
	add r16, fp, r0
	jal r0, .LBB143_5
.LBB143_1:
	addi r1, r4, 1
.LBB143_2:
	stw r14+%lo(fio_pos), r1
.LBB143_3:
	bgtu r16, r18, .LBB143_109
.LBB143_4:
	addi r16, r16, 1
.LBB143_5:
	ldw r3, r15+%lo(fio_fmt)
	ldw r4, r14+%lo(fio_pos)
	add r7, r3, r4
	ldb r6, r7+0
	andi r1, r6, 255
	beq r1, r17, .LBB143_1
.LBB143_6:
	beq r1, r19, .LBB143_1
.LBB143_7:
	bne r1, r0, .LBB143_16
.LBB143_8:
	ldw r1, r20+%lo(fio_gdepth)
	blt r1, fp, .LBB143_10
.LBB143_9:
	stw r20+%lo(fio_gdepth), r0
.LBB143_10:
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB143_109
.LBB143_11:
	ldw r4, r22+%lo(fio_revert)
	add r1, r0, r0
	blt r4, r0, .LBB143_110
.LBB143_12:
	ldbu r1, r13+%lo(fio_since_data)
	beq r1, r0, .LBB143_131
.LBB143_13:
	stb r13+%lo(fio_since_data), r0
	stw r14+%lo(fio_pos), r4
	add r1, r3, r4
	ldbu r1, r1+0
	beq r1, r0, .LBB143_109
.LBB143_14:
	ldbu r1, r23+%lo(fio_reading)
	bne r1, fp, .LBB143_88
.LBB143_15:
	jal r31, fio_getrec
	beq r1, r0, .LBB143_3
	jal r0, .LBB143_132
.LBB143_16:
	and r5, r6, r24
	bne r5, r12, .LBB143_20
.LBB143_17:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	add r1, r3, r1
	ldbu r1, r1+0
	addi r1, r1, -80
	bgtu r1, r25, .LBB143_50
.LBB143_18:
	slli r1, r1, 2
	add r1, r27, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB143_19:
	stb r26+%lo(fio_sp), fp
	jal r0, .LBB143_28
.LBB143_20:
	bne r6, r28, .LBB143_23
.LBB143_21:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB143_109
.LBB143_22:
	add r1, r0, r0
	bleu r16, r18, .LBB143_4
	jal r0, .LBB143_110
.LBB143_23:
	andi r5, r6, 223
	ldw r8, sp+52
	bne r5, r8, .LBB143_29
.LBB143_24:
	ldbu r5, r7+1
	addi r8, r5, -78
	andi r9, r8, 252
	slli r8, r8, 6
	srli r9, r9, 2
	or  r8, r8, r9
	andi r8, r8, 255
	ldw r9, sp+44
	bgtu r8, r9, .LBB143_29
.LBB143_25:
	sll r8, fp, r8
	andi r8, r8, 2313
	beq r8, r0, .LBB143_29
.LBB143_26:
	andi r1, r5, 223
	ldw r3, sp+40
	seq r1, r1, r3
	ldw r3, sp+36
	stw r3+%lo(fio_bz), r1
	jal r0, .LBB143_28
.LBB143_27:
	stb r26+%lo(fio_sp), r0
.LBB143_28:
	addi r1, r4, 2
	jal r0, .LBB143_2
.LBB143_29:
	addi r5, r1, -39
	lui r1, %hi(fio_line)
	addi r1, r1, %lo(fio_line)
	ldw r8, sp+48
	bgtu r5, r8, .LBB143_53
.LBB143_30:
	slli r5, r5, 2
	lui r8, %hi(.LJTI143_0)
	addi r8, r8, %lo(.LJTI143_0)
	add r5, r8, r5
	ldw r8, r5+0
	add r5, fp, r0
	jalr r0, r8, 0
.LBB143_31:
	addi r4, r4, 1
	stw r14+%lo(fio_pos), r4
	lui r5, %hi(fio_col)
	ldw r10, r5+%lo(fio_col)
	lui r6, %hi(fio_len)
	ldw r8, r6+%lo(fio_len)
	ldbu r7, r23+%lo(fio_reading)
	jal r0, .LBB143_35
.LBB143_32:
	addi lr, r0, 1
.LBB143_33:
	add r10, r9, r0
.LBB143_34:
	add r4, r4, lr
	stw r14+%lo(fio_pos), r4
.LBB143_35:
	add r9, r10, r0
	add r10, r3, r4
	ldbu r10, r10+0
	addi lr, r0, 39
	beq r10, lr, .LBB143_42
.LBB143_36:
	beq r10, r0, .LBB143_3
.LBB143_37:
	addi lr, r0, 1022
	sgt lr, r9, lr
	or  lr, r7, lr
	andi lr, lr, 1
	bne lr, r0, .LBB143_32
.LBB143_38:
	bge r8, r9, .LBB143_41
.LBB143_39:
	addi lr, r8, 1
	add r8, r8, r1
	stb r8+0, r19
	add r8, lr, r0
	bne r9, lr, .LBB143_39
.LBB143_40:
	stw r6+%lo(fio_len), r9
	add r8, r9, r0
.LBB143_41:
	add lr, r9, r1
	stb lr+0, r10
	addi r10, r9, 1
	stw r5+%lo(fio_col), r10
	addi lr, r0, 1
	blt r9, r8, .LBB143_34
	jal r0, .LBB143_49
.LBB143_42:
	addi r10, r4, 1
	add r11, r3, r10
	ldbu r11, r11+0
	bne r11, lr, .LBB143_82
.LBB143_43:
	addi r10, r0, 1022
	sgt r10, r9, r10
	or  r10, r7, r10
	andi r10, r10, 1
	beq r10, r0, .LBB143_45
.LBB143_44:
	addi lr, r0, 2
	jal r0, .LBB143_33
.LBB143_45:
	bge r8, r9, .LBB143_48
.LBB143_46:
	addi r10, r8, 1
	add r8, r8, r1
	stb r8+0, r19
	add r8, r10, r0
	bne r9, r10, .LBB143_46
.LBB143_47:
	stw r6+%lo(fio_len), r9
	add r8, r9, r0
.LBB143_48:
	add r10, r9, r1
	stb r10+0, lr
	addi r10, r9, 1
	stw r5+%lo(fio_col), r10
	addi lr, r0, 2
	blt r9, r8, .LBB143_34
.LBB143_49:
	stw r6+%lo(fio_len), r10
	add r8, r10, r0
	jal r0, .LBB143_34
.LBB143_50:
	stb r26+%lo(fio_sp), r0
	jal r0, .LBB143_3
.LBB143_51:
	ldb r5, r7+1
	addi r5, r5, -58
	addi r6, r0, -10
	bgeu r5, r6, .LBB143_58
.LBB143_52:
	addi r6, r0, 45
.LBB143_53:
	add r5, fp, r0
	jal r0, .LBB143_59
.LBB143_54:
	ldw r1, r20+%lo(fio_gdepth)
	beq r1, r0, .LBB143_83
.LBB143_55:
	addi r3, r4, 1
	stw r14+%lo(fio_pos), r3
	addi r3, r1, -1
	stw r20+%lo(fio_gdepth), r3
	slli r3, r3, 2
	lui r4, %hi(fio_gcount)
	addi r4, r4, %lo(fio_gcount)
	add r4, r3, r4
	ldw r5, r4+0
	addi r6, r5, -1
	stw r4+0, r6
	addi r4, r0, 2
	blt r5, r4, .LBB143_3
.LBB143_56:
	lui r4, %hi(fio_gstart)
	addi r4, r4, %lo(fio_gstart)
	add r3, r3, r4
	ldw r3, r3+0
	stw r14+%lo(fio_pos), r3
	stw r20+%lo(fio_gdepth), r1
	jal r0, .LBB143_3
.LBB143_57:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB143_87
.LBB143_58:
	addi r5, r4, 1
	stw r14+%lo(fio_pos), r5
	add r5, r3, r5
	ldb r6, r5+0
	add r5, r0, r0
.LBB143_59:
	addi r6, r6, -58
	addi r9, r0, -10
	add r11, fp, r0
	bltu r6, r9, .LBB143_65
.LBB143_60:
	ldw r6, r14+%lo(fio_pos)
	add r7, r3, r6
	ldb r7, r7+0
	addi r7, r7, -48
	addi r8, r0, 9
	bleu r7, r8, .LBB143_62
.LBB143_61:
	add r11, r0, r0
	jal r0, .LBB143_65
.LBB143_62:
	stw sp+32, r9
	addi r8, r3, 1
	add r11, r0, r0
.LBB143_63:
	addi r10, r0, 10
	mul lr, r11, r10
	addi r9, r6, 1
	add r11, lr, r7
	add r6, r8, r6
	ldb r6, r6+0
	addi r7, r6, -48
	add r6, r9, r0
	bltu r7, r10, .LBB143_63
.LBB143_64:
	stw r14+%lo(fio_pos), r9
	ldw r9, sp+32
.LBB143_65:
	ldw r6, r14+%lo(fio_pos)
	add r7, r3, r6
	ldbu r7, r7+0
	addi r8, r0, 40
	bne r7, r8, .LBB143_69
.LBB143_66:
	addi r3, r6, 1
	stw r14+%lo(fio_pos), r3
	ldw r1, r20+%lo(fio_gdepth)
	addi r5, r0, 15
	bgt r1, r5, .LBB143_3
.LBB143_67:
	slli r5, r1, 2
	lui r6, %hi(fio_gstart)
	addi r6, r6, %lo(fio_gstart)
	add r6, r5, r6
	stw r6+0, r3
	lui r3, %hi(fio_gcount)
	addi r3, r3, %lo(fio_gcount)
	add r3, r5, r3
	stw r3+0, r11
	addi r3, r1, 1
	stw r20+%lo(fio_gdepth), r3
	bne r1, r0, .LBB143_3
.LBB143_68:
	stw r22+%lo(fio_revert), r4
	jal r0, .LBB143_3
.LBB143_69:
	andi r4, r7, 223
	addi r4, r4, -72
	addi r8, r0, 16
	bgtu r4, r8, .LBB143_101
.LBB143_70:
	slli r4, r4, 2
	lui r8, %hi(.LJTI143_1)
	addi r8, r8, %lo(.LJTI143_1)
	add r4, r8, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB143_71:
	addi r4, r6, 1
	addi r5, r0, 1
	blt r11, r5, .LBB143_97
.LBB143_72:
	lui r6, %hi(fio_len)
	stw sp+32, r6
	ldw r9, r6+%lo(fio_len)
	lui r7, %hi(fio_col)
	ldw r10, r7+%lo(fio_col)
	ldbu r8, r23+%lo(fio_reading)
	jal r0, .LBB143_75
.LBB143_73:
	add r10, r11, r0
.LBB143_74:
	addi r11, lr, -1
	addi r4, r4, 1
	ble lr, r5, .LBB143_97
.LBB143_75:
	add lr, r11, r0
	add r11, r3, r4
	ldbu r11, r11+0
	beq r11, r0, .LBB143_97
.LBB143_76:
	addi r6, r0, 1022
	sgt r6, r10, r6
	or  r6, r8, r6
	andi r6, r6, 1
	bne r6, r0, .LBB143_74
.LBB143_77:
	bge r9, r10, .LBB143_80
.LBB143_78:
	addi r6, r9, 1
	add r9, r9, r1
	stb r9+0, r19
	add r9, r6, r0
	bne r10, r6, .LBB143_78
.LBB143_79:
	ldw r6, sp+32
	stw r6+%lo(fio_len), r10
	add r9, r10, r0
.LBB143_80:
	add r6, r10, r1
	stb r6+0, r11
	addi r11, r10, 1
	stw r7+%lo(fio_col), r11
	blt r10, r9, .LBB143_73
.LBB143_81:
	ldw r6, sp+32
	stw r6+%lo(fio_len), r11
	add r9, r11, r0
	jal r0, .LBB143_73
.LBB143_82:
	stw r14+%lo(fio_pos), r10
	jal r0, .LBB143_3
.LBB143_83:
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB143_109
.LBB143_84:
	ldw r4, r22+%lo(fio_revert)
	add r1, r0, r0
	blt r4, r0, .LBB143_110
.LBB143_85:
	ldbu r1, r13+%lo(fio_since_data)
	beq r1, r0, .LBB143_131
.LBB143_86:
	stb r13+%lo(fio_since_data), r0
	stw r14+%lo(fio_pos), r4
	add r1, r3, r4
	ldbu r1, r1+0
	beq r1, r0, .LBB143_109
.LBB143_87:
	ldbu r1, r23+%lo(fio_reading)
	addi r3, r0, 1
	beq r1, r3, .LBB143_15
.LBB143_88:
	jal r31, fio_flush_line
	jal r0, .LBB143_3
.LBB143_89:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	add r1, r3, r1
	ldbu r1, r1+0
	add r8, r0, r0
	addi r11, r0, 1
	addi r4, r1, -76
	addi r1, r0, 38
	bgtu r4, r1, .LBB143_108
.LBB143_90:
	addi r1, r0, -1
	slli r4, r4, 2
	lui r5, %hi(.LJTI143_2)
	addi r5, r5, %lo(.LJTI143_2)
	add r4, r5, r4
	ldw r4, r4+0
	add r7, r11, r0
	jalr r0, r4, 0
.LBB143_91:
	addi r1, r0, 1
.LBB143_92:
	addi r4, r6, 2
	stw r14+%lo(fio_pos), r4
	add r7, r0, r0
	add r8, r1, r0
.LBB143_93:
	ldw r1, r14+%lo(fio_pos)
	add r1, r3, r1
	ldb r1, r1+0
	addi r3, r1, -58
	add r1, r11, r0
	bltu r3, r9, .LBB143_95
.LBB143_94:
	stw sp+32, r7
	stw sp+28, r8
	jal r31, fio_number
	ldw r8, sp+28
	ldw r7, sp+32
.LBB143_95:
	ldbu r4, r23+%lo(fio_reading)
	mul r3, r1, r8
	addi r1, r1, -1
	bne r4, r11, .LBB143_105
.LBB143_96:
	lui r4, %hi(fio_rpos)
	ldw r5, r4+%lo(fio_rpos)
	add r3, r5, r3
	xor r1, r1, r3
	sub r5, r0, r7
	and r1, r1, r5
	xor r1, r3, r1
	addi r3, r0, 0
	sgt r3, r1, r3
	sub r3, r0, r3
	and r1, r1, r3
	stw r4+%lo(fio_rpos), r1
	jal r0, .LBB143_3
.LBB143_97:
	stw r14+%lo(fio_pos), r4
	jal r0, .LBB143_3
.LBB143_98:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	sub r1, r0, r11
	xor r3, r11, r1
	sub r4, r0, r5
	and r3, r3, r4
	xor r1, r1, r3
	lui r3, %hi(fio_scale)
	stw r3+%lo(fio_scale), r1
	jal r0, .LBB143_3
.LBB143_99:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	ldbu r3, r23+%lo(fio_reading)
	addi r1, r0, 1
	bne r3, r1, .LBB143_106
.LBB143_100:
	lui r1, %hi(fio_rpos)
	ldw r3, r1+%lo(fio_rpos)
	add r3, r3, r11
	stw r1+%lo(fio_rpos), r3
	jal r0, .LBB143_3
.LBB143_101:
	andi r1, r7, 222
	addi r4, r0, 68
	andi r8, r7, 255
	beq r1, r4, .LBB143_111
.LBB143_102:
	addi r1, r8, -65
	addi r4, r0, 43
	bgtu r1, r4, .LBB143_104
.LBB143_103:
	slli r1, r1, 2
	lui r4, %hi(.LJTI143_3)
	addi r4, r4, %lo(.LJTI143_3)
	add r1, r4, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB143_104:
	addi r1, r6, 1
	jal r0, .LBB143_2
.LBB143_105:
	lui r4, %hi(fio_col)
	ldw r5, r4+%lo(fio_col)
	add r3, r5, r3
	xor r1, r1, r3
	sub r5, r0, r7
	and r1, r1, r5
	xor r1, r3, r1
	addi r3, r0, 0
	sgt r3, r1, r3
	sub r3, r0, r3
	and r1, r1, r3
	stw r4+%lo(fio_col), r1
	jal r0, .LBB143_3
.LBB143_106:
	blt r11, r1, .LBB143_3
.LBB143_107:
	lui r1, %hi(fio_col)
	ldw r3, r1+%lo(fio_col)
	add r3, r3, r11
	stw r1+%lo(fio_col), r3
	jal r0, .LBB143_3
.LBB143_108:
	add r7, r11, r0
	jal r0, .LBB143_93
.LBB143_109:
	add r1, r0, r0
.LBB143_110:
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
.LBB143_111:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	andi r4, r7, 79
	addi r5, r0, 69
	add r18, r8, r0
	bne r4, r5, .LBB143_115
.LBB143_112:
	add r16, r9, r0
	add r1, r3, r1
	ldbu r3, r1+0
	jal r31, toupper
	beq r1, r12, .LBB143_114
.LBB143_113:
	ldw r1, r15+%lo(fio_fmt)
	ldw r3, r14+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB143_116
.LBB143_114:
	ldw r1, r15+%lo(fio_fmt)
	ldw r3, r14+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+0
	jal r31, toupper
	add r12, r1, r0
	ldw r1, r14+%lo(fio_pos)
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB143_117
.LBB143_115:
	add r12, r0, r0
	jal r0, .LBB143_118
.LBB143_116:
	add r12, r0, r0
.LBB143_117:
	add r9, r16, r0
	add r8, r18, r0
.LBB143_118:
	ldw r15, r15+%lo(fio_fmt)
	ldw r1, r14+%lo(fio_pos)
	add r1, r15, r1
	ldb r1, r1+0
	addi r1, r1, -58
	bgeu r1, r9, .LBB143_120
.LBB143_119:
	add r1, r0, r0
	jal r0, .LBB143_121
.LBB143_120:
	add r16, r9, r0
	jal r31, fio_number
	add r8, r18, r0
	add r9, r16, r0
.LBB143_121:
	lui r3, %hi(fio_w)
	stw r3+%lo(fio_w), r1
	lui r16, %hi(fio_d)
	stw r16+%lo(fio_d), r0
	ldw r1, r14+%lo(fio_pos)
	add r3, r15, r1
	ldbu r3, r3+0
	addi r4, r0, 46
	bne r3, r4, .LBB143_123
.LBB143_122:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	add r17, r9, r0
	jal r31, fio_number
	add r8, r18, r0
	add r9, r17, r0
	stw r16+%lo(fio_d), r1
.LBB143_123:
	lui r16, %hi(fio_e)
	stw r16+%lo(fio_e), r0
	ldw r1, r14+%lo(fio_pos)
	add r3, r15, r1
	ldbu r3, r3+0
	ori  r3, r3, 32
	addi r4, r0, 101
	bne r3, r4, .LBB143_127
.LBB143_124:
	addi r3, r1, 1
	stw r14+%lo(fio_pos), r3
	add r3, r15, r3
	ldb r3, r3+0
	addi r3, r3, -58
	bgeu r3, r9, .LBB143_126
.LBB143_125:
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB143_127
.LBB143_126:
	jal r31, fio_number
	add r8, r18, r0
	stw r16+%lo(fio_e), r1
.LBB143_127:
	lui r1, %hi(fio_rep)
	stw r1+%lo(fio_rep), r11
	beq r12, r0, .LBB143_129
.LBB143_128:
	addi r1, r12, 256
	jal r0, .LBB143_130
.LBB143_129:
	add r3, r8, r0
	jal r31, toupper
.LBB143_130:
	lui r3, %hi(fio_desc)
	stw r3+%lo(fio_desc), r1
	addi r3, r0, 1
	stb r13+%lo(fio_since_data), r3
	jal r0, .LBB143_110
.LBB143_131:
	jal r31, fio_flush_line
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.107)
	addi r4, r4, %lo(.L.str.107)
	jal r0, .LBB143_133
.LBB143_132:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.82)
	addi r4, r4, %lo(.L.str.82)
.LBB143_133:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end143:
	.size	fio_next_desc, .Lfunc_end143-fio_next_desc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI143_0,@object
.LJTI143_0:
	.word	.LBB143_31
	.word	.LBB143_59
	.word	.LBB143_54
	.word	.LBB143_59
	.word	.LBB143_59
	.word	.LBB143_59
	.word	.LBB143_51
	.word	.LBB143_59
	.word	.LBB143_57
	.size	.LJTI143_0, 36
	.type	.LJTI143_1,@object
.LJTI143_1:
	.word	.LBB143_71
	.word	.LBB143_101
	.word	.LBB143_101
	.word	.LBB143_101
	.word	.LBB143_101
	.word	.LBB143_101
	.word	.LBB143_101
	.word	.LBB143_101
	.word	.LBB143_98
	.word	.LBB143_101
	.word	.LBB143_101
	.word	.LBB143_101
	.word	.LBB143_89
	.word	.LBB143_101
	.word	.LBB143_101
	.word	.LBB143_101
	.word	.LBB143_99
	.size	.LJTI143_1, 68
	.type	.LJTI143_2,@object
.LJTI143_2:
	.word	.LBB143_92
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_91
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_92
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_93
	.word	.LBB143_91
	.size	.LJTI143_2, 156
	.type	.LJTI143_3,@object
.LJTI143_3:
	.word	.LBB143_111
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_111
	.word	.LBB143_111
	.word	.LBB143_104
	.word	.LBB143_111
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_111
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_111
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_111
	.word	.LBB143_111
	.word	.LBB143_104
	.word	.LBB143_111
	.word	.LBB143_104
	.word	.LBB143_104
	.word	.LBB143_111
	.size	.LJTI143_3, 176
	.type	.LJTI143_4,@object
.LJTI143_4:
	.word	.LBB143_19
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_27
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_19
	.word	.LBB143_50
	.word	.LBB143_50
	.word	.LBB143_27
	.size	.LJTI143_4, 144
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
	beq r3, r0, .LBB144_9
.LBB144_1:
	lui r11, %hi(fio_irec)
	ldw r1, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r1, r4, .LBB144_18
.LBB144_2:
	lui r14, %hi(fio_ilen)
	ldw r6, r14+%lo(fio_ilen)
	bgt r5, r6, .LBB144_19
.LBB144_3:
	mul r1, r6, r1
	add r3, r3, r1
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	jal r31, memcpy
	ldw r3, r13+%lo(fio_len)
	ldw r1, r14+%lo(fio_ilen)
	ble r1, r3, .LBB144_6
.LBB144_4:
	ldw r4, r12+%lo(fio_ibuf)
	ldw r5, r11+%lo(fio_irec)
	mul r5, r5, r1
	sub r1, r1, r3
	add r3, r3, r5
	add r3, r4, r3
	addi r4, r0, 32
.LBB144_5:
	stb r3+0, r4
	addi r1, r1, -1
	addi r3, r3, 1
	bne r1, r0, .LBB144_5
.LBB144_6:
	ldw r1, r11+%lo(fio_irec)
	addi r1, r1, 1
	stw r11+%lo(fio_irec), r1
.LBB144_7:
	stw r13+%lo(fio_len), r0
.LBB144_8:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB144_9:
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
	bgtu r3, r14, .LBB144_15
.LBB144_10:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB144_15
.LBB144_11:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r12, r1+0
	bgt r5, r12, .LBB144_21
.LBB144_12:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	ldw r1, r13+%lo(fio_len)
	ble r12, r1, .LBB144_7
.LBB144_13:
	sub r14, r12, r1
	addi r12, r0, 32
.LBB144_14:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fputc
	addi r14, r14, -1
	bne r14, r0, .LBB144_14
	jal r0, .LBB144_7
.LBB144_15:
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
	bgtu r3, r14, .LBB144_8
.LBB144_16:
	addi r1, r1, -7
	addi r3, r0, -3
	bgtu r1, r3, .LBB144_8
.LBB144_17:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
	jal r0, .LBB144_8
.LBB144_18:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.108)
	addi r4, r4, %lo(.L.str.108)
	jal r0, .LBB144_20
.LBB144_19:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.109)
	addi r4, r4, %lo(.L.str.109)
.LBB144_20:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB144_21:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.110)
	addi r4, r4, %lo(.L.str.110)
	add r6, r12, r0
	jal r0, .LBB144_20
.Lfunc_end144:
	.size	fio_flush_line, .Lfunc_end144-fio_flush_line
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
	bne r3, r0, .LBB145_4
.LBB145_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB145_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB145_2
.LBB145_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB145_4:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_resolve
	add r13, r1, r0
	lui r1, %hi(stdout)
	ldw r1, r1+%lo(stdout)
	beq r13, r1, .LBB145_8
.LBB145_5:
	lui r1, %hi(stderr)
	ldw r1, r1+%lo(stderr)
	beq r13, r1, .LBB145_8
.LBB145_6:
	add r3, r13, r0
	jal r31, fflush
	addi r1, r11, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB145_8
.LBB145_7:
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
.LBB145_8:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end145:
	.size	f77_endfile, .Lfunc_end145-f77_endfile
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
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	add r5, r11, r0
	add r6, r12, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fprintf
.Lfunc_end146:
	.size	f77_pause, .Lfunc_end146-f77_pause
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
	beq r3, r1, .LBB147_19
.LBB147_1:
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	stb r15+%lo(fio_busy), r1
	lui r3, %hi(fio_uend_init)
	ldbu r4, r3+%lo(fio_uend_init)
	bne r4, r0, .LBB147_5
.LBB147_2:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
	addi r6, r0, -1
.LBB147_3:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r0, .LBB147_3
.LBB147_4:
	stb r3+%lo(fio_uend_init), r1
.LBB147_5:
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
	bgtu r3, r4, .LBB147_11
.LBB147_6:
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
	bltu r4, r5, .LBB147_10
.LBB147_7:
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r16, r4
	ldw r4, r4+0
	beq r4, r0, .LBB147_10
.LBB147_8:
	ble r3, r0, .LBB147_20
.LBB147_9:
	addi r3, r3, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r4, r16, r4
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB147_10:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r16, r1
	ldw r1, r1+0
	jal r0, .LBB147_16
.LBB147_11:
	lui r4, %hi(fio_rec_next)
	ldw r3, r4+%lo(fio_rec_next)
	stw r4+%lo(fio_rec_next), r0
	addi r4, r14, -100
	addi r5, r0, -99
	bltu r4, r5, .LBB147_15
.LBB147_12:
	slli r4, r14, 2
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r4, r5
	ldw r5, r5+0
	beq r5, r0, .LBB147_15
.LBB147_13:
	ble r3, r0, .LBB147_20
.LBB147_14:
	addi r3, r3, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r4, r4, r5
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB147_15:
	add r1, r0, r0
.LBB147_16:
	lui r3, %hi(fio_bz)
	stw r3+%lo(fio_bz), r1
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, fio_rd_setup
	beq r1, r0, .LBB147_18
.LBB147_17:
	stb r15+%lo(fio_busy), r0
.LBB147_18:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB147_19:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.76)
	addi r4, r4, %lo(.L.str.76)
	jal r0, .LBB147_21
.LBB147_20:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.80)
	addi r4, r4, %lo(.L.str.80)
.LBB147_21:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end147:
	.size	f77_rd_begin, .Lfunc_end147-f77_rd_begin
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
	bgt r1, r0, .LBB148_2
.LBB148_1:
	lui r1, %hi(fio_bz)
	stw r1+%lo(fio_bz), r0
.LBB148_2:
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
	beq r3, r0, .LBB148_4
.LBB148_3:
	jal r31, fio_fmt_use
	ble r1, r18, .LBB148_14
.LBB148_4:
	ldw r1, r17+%lo(fio_listed)
	bne r1, r0, .LBB148_10
.LBB148_5:
	ldw r3, r16+%lo(fio_fmt)
	ldw r1, r14+%lo(fio_pos)
	addi r4, r0, 32
.LBB148_6:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB148_8
.LBB148_7:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB148_6
.LBB148_8:
	addi r3, r0, 40
	bne r5, r3, .LBB148_10
.LBB148_9:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	stw r15+%lo(fio_revert), r1
.LBB148_10:
	jal r31, fio_getrec
	beq r1, r0, .LBB148_13
.LBB148_11:
	stb r13+%lo(fio_reading), r0
	andi r1, r11, 3
	bne r1, r0, .LBB148_16
.LBB148_12:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.82)
	addi r4, r4, %lo(.L.str.82)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB148_13:
	add r12, r0, r0
	jal r0, .LBB148_16
.LBB148_14:
	stb r13+%lo(fio_reading), r0
	andi r1, r11, 2
	beq r1, r0, .LBB148_17
.LBB148_15:
	addi r12, r0, 2
.LBB148_16:
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
.LBB148_17:
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end148:
	.size	fio_rd_setup, .Lfunc_end148-fio_rd_setup
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
	beq r9, r1, .LBB149_8
.LBB149_1:
	stb r11+%lo(fio_busy), r1
	lui r9, %hi(fio_uend_init)
	ldbu r10, r9+%lo(fio_uend_init)
	bne r10, r0, .LBB149_5
.LBB149_2:
	addi r10, r0, -400
	lui lr, %hi(fio_uend+400)
	addi lr, lr, %lo(fio_uend+400)
	addi r12, r0, -1
.LBB149_3:
	add r13, r10, lr
	stw r13+0, r12
	addi r10, r10, 4
	bne r10, r0, .LBB149_3
.LBB149_4:
	stb r9+%lo(fio_uend_init), r1
.LBB149_5:
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
	beq r1, r0, .LBB149_7
.LBB149_6:
	stb r11+%lo(fio_busy), r0
.LBB149_7:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB149_8:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.76)
	addi r4, r4, %lo(.L.str.76)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end149:
	.size	f77_rd_begin_int, .Lfunc_end149-f77_rd_begin_int
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
	beq r1, r0, .LBB150_5
.LBB150_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB150_60
.LBB150_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB150_60
.LBB150_3:
	addi r3, r0, 1
	bne r1, r3, .LBB150_27
.LBB150_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB150_60
.LBB150_5:
	lui r12, %hi(fio_desc)
	ldw r1, r12+%lo(fio_desc)
	lui r13, %hi(fio_rep)
	beq r1, r0, .LBB150_11
.LBB150_6:
	ldw r3, r13+%lo(fio_rep)
	ble r3, r0, .LBB150_11
.LBB150_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB150_12
.LBB150_8:
	beq r1, r0, .LBB150_60
.LBB150_9:
	addi r3, r0, 65
	bne r1, r3, .LBB150_30
.LBB150_10:
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
.LBB150_11:
	stw r12+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB150_8
.LBB150_12:
	addi r3, r0, 73
	beq r1, r3, .LBB150_31
.LBB150_13:
	addi r3, r0, 76
	bne r1, r3, .LBB150_30
.LBB150_14:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	addi r1, r0, 1
	lui r4, %hi(fio_rpos)
	blt r3, r1, .LBB150_44
.LBB150_15:
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
	jal r0, .LBB150_19
.LBB150_16:
	addi r19, r17, 1
	addi r21, sp, 20
	add r17, r21, r17
	stb r17+0, r20
	add r17, r19, r0
.LBB150_17:
	add r19, r14, r0
.LBB150_18:
	addi r18, r18, -1
	addi r5, r5, 1
	beq r18, r0, .LBB150_45
.LBB150_19:
	add r20, r8, r0
	bge r5, r6, .LBB150_21
.LBB150_20:
	add r20, r5, r9
	ldb r20, r20+0
.LBB150_21:
	beq r20, r8, .LBB150_23
.LBB150_22:
	bne r20, r15, .LBB150_26
.LBB150_23:
	beq r7, r0, .LBB150_18
.LBB150_24:
	beq r19, r0, .LBB150_18
.LBB150_25:
	add r20, r10, r0
	blt r17, lr, .LBB150_16
	jal r0, .LBB150_18
.LBB150_26:
	bgt r17, r16, .LBB150_17
	jal r0, .LBB150_16
.LBB150_27:
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
	bgtu r4, r5, .LBB150_61
.LBB150_28:
	slli r4, r4, 2
	lui r5, %hi(.LJTI150_0)
	addi r5, r5, %lo(.LJTI150_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB150_29:
	andi r3, r3, 255
	addi r4, r0, 46
	seq r3, r3, r4
	add r1, r3, r1
	ldbu r1, r1+0
	andi r1, r1, 223
	addi r3, r0, 84
	seq r1, r1, r3
	stw r11+0, r1
	jal r0, .LBB150_60
.LBB150_30:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.w.d r1, r4
	jal r0, .LBB150_56
.LBB150_31:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r3, r0, 1
	blt r1, r3, .LBB150_52
.LBB150_32:
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
	jal r0, .LBB150_36
.LBB150_33:
	addi r17, r15, 1
	addi r19, sp, 20
	add r15, r19, r15
	stb r15+0, r18
	add r15, r17, r0
.LBB150_34:
	add r17, r3, r0
.LBB150_35:
	addi r16, r16, -1
	addi r4, r4, 1
	beq r16, r0, .LBB150_53
.LBB150_36:
	add r18, r7, r0
	bge r4, r5, .LBB150_38
.LBB150_37:
	add r18, r4, r8
	ldb r18, r18+0
.LBB150_38:
	beq r18, r7, .LBB150_40
.LBB150_39:
	bne r18, lr, .LBB150_43
.LBB150_40:
	beq r6, r0, .LBB150_35
.LBB150_41:
	beq r17, r0, .LBB150_35
.LBB150_42:
	add r18, r9, r0
	blt r15, r10, .LBB150_33
	jal r0, .LBB150_35
.LBB150_43:
	bgt r15, r14, .LBB150_34
	jal r0, .LBB150_33
.LBB150_44:
	add r17, r0, r0
.LBB150_45:
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
	ble r3, r4, .LBB150_49
.LBB150_46:
	addi r4, r0, 84
	beq r3, r4, .LBB150_56
.LBB150_47:
	addi r4, r0, 116
	beq r3, r4, .LBB150_56
.LBB150_48:
	addi r1, r0, 102
	beq r3, r1, .LBB150_55
	jal r0, .LBB150_51
.LBB150_49:
	beq r3, r0, .LBB150_55
.LBB150_50:
	addi r1, r0, 70
	beq r3, r1, .LBB150_55
.LBB150_51:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.112)
	addi r4, r4, %lo(.L.str.112)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB150_52:
	add r15, r0, r0
.LBB150_53:
	addi r3, sp, 20
	add r3, r3, r15
	stb r3+0, r0
	lui r3, %hi(fio_rpos)
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldbu r1, sp+20
	beq r1, r0, .LBB150_55
.LBB150_54:
	addi r3, sp, 20
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	jal r0, .LBB150_56
.LBB150_55:
	add r1, r0, r0
.LBB150_56:
	stw r11+0, r1
	ldw r1, r13+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB150_58
.LBB150_57:
	addi r1, r1, -1
	stw r13+%lo(fio_rep), r1
.LBB150_58:
	ldw r1, r13+%lo(fio_rep)
	bne r1, r0, .LBB150_60
.LBB150_59:
	stw r12+%lo(fio_desc), r0
.LBB150_60:
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
.LBB150_61:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	stw r11+0, r1
	jal r0, .LBB150_60
.Lfunc_end150:
	.size	f77_rd_i, .Lfunc_end150-f77_rd_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI150_0,@object
.LJTI150_0:
	.word	.LBB150_29
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_29
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_29
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_29
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_61
	.word	.LBB150_29
	.size	.LJTI150_0, 144
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
	blt r1, r20, .LBB151_2
.LBB151_1:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
	addi r11, r0, 2
	jal r0, .LBB151_5
.LBB151_2:
	lui r14, %hi(fio_l_pending)
	ldw r1, r14+%lo(fio_l_pending)
	ble r1, r0, .LBB151_6
.LBB151_3:
	addi r1, r1, -1
	stw r14+%lo(fio_l_pending), r1
.LBB151_4:
	add r11, r0, r0
.LBB151_5:
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
.LBB151_6:
	lui r15, %hi(fio_rpos)
	lui r17, %hi(fio_rlen)
	lui r16, %hi(fio_rec)
	addi r16, r16, %lo(fio_rec)
	addi r18, r0, 38
	lui r21, %hi(.LJTI151_0)
	addi r21, r21, %lo(.LJTI151_0)
	lui r12, %hi(fio_l_after)
	addi r11, r0, 2
	jal r0, .LBB151_8
.LBB151_7:
	jal r31, fio_getrec
	bne r1, r0, .LBB151_44
.LBB151_8:
	ldw r3, r15+%lo(fio_rpos)
	ldw r1, r17+%lo(fio_rlen)
	bge r3, r1, .LBB151_7
.LBB151_9:
	add r4, r3, r16
	ldbu r19, r4+0
	addi r4, r19, -9
	bgtu r4, r18, .LBB151_25
.LBB151_10:
	slli r4, r4, 2
	add r4, r21, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB151_11:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	jal r0, .LBB151_8
.LBB151_12:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	ldbu r1, r12+%lo(fio_l_after)
	bne r1, r20, .LBB151_5
.LBB151_13:
	stb r12+%lo(fio_l_after), r0
	jal r0, .LBB151_8
.LBB151_14:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	addi r13, r0, 126
	lui r11, %hi(fio_ltok)
	addi r11, r11, %lo(fio_ltok)
	add r14, r0, r0
	jal r0, .LBB151_16
.LBB151_15:
	jal r31, fio_getrec
	bne r1, r0, .LBB151_44
.LBB151_16:
	ldw r1, r15+%lo(fio_rpos)
	ldw r4, r17+%lo(fio_rlen)
	bge r1, r4, .LBB151_15
.LBB151_17:
	addi r5, r1, 1
	stw r15+%lo(fio_rpos), r5
	add r3, r1, r16
	ldbu r3, r3+0
	bne r3, r19, .LBB151_21
.LBB151_18:
	bge r5, r4, .LBB151_23
.LBB151_19:
	add r4, r5, r16
	ldbu r4, r4+0
	bne r4, r19, .LBB151_23
.LBB151_20:
	addi r1, r1, 2
	stw r15+%lo(fio_rpos), r1
.LBB151_21:
	bgtu r14, r13, .LBB151_16
.LBB151_22:
	addi r1, r14, 1
	add r4, r14, r11
	stb r4+0, r3
	add r14, r1, r0
	jal r0, .LBB151_16
.LBB151_23:
	add r1, r14, r11
	stb r1+0, r0
	addi r1, r0, 1
	stb r12+%lo(fio_l_after), r1
	jal r0, .LBB151_4
.LBB151_24:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	add r11, r20, r0
	jal r0, .LBB151_5
.LBB151_25:
	ldw r3, r15+%lo(fio_rpos)
	bge r3, r1, .LBB151_32
.LBB151_26:
	addi r5, r0, 126
	lui r6, %hi(fio_ltok)
	addi r6, r6, %lo(fio_ltok)
	lui r7, %hi(.LJTI151_1)
	addi r7, r7, %lo(.LJTI151_1)
	add r4, r0, r0
	jal r0, .LBB151_28
.LBB151_27:
	addi r3, r3, 1
	stw r15+%lo(fio_rpos), r3
	beq r1, r3, .LBB151_33
.LBB151_28:
	add r8, r3, r16
	ldbu r8, r8+0
	addi r9, r8, -9
	bgtu r9, r18, .LBB151_30
.LBB151_29:
	slli r9, r9, 2
	add r9, r7, r9
	ldw r9, r9+0
	jalr r0, r9, 0
.LBB151_30:
	bgt r4, r5, .LBB151_27
.LBB151_31:
	addi r9, r4, 1
	add r4, r4, r6
	stb r4+0, r8
	add r4, r9, r0
	jal r0, .LBB151_27
.LBB151_32:
	add r4, r0, r0
.LBB151_33:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	add r3, r4, r1
	stb r3+0, r0
	addi r3, r0, 1
	stb r12+%lo(fio_l_after), r3
	addi r4, r0, -11
	add r15, r0, r0
.LBB151_34:
	add r5, r15, r1
	ldb r5, r5+0
	addi r6, r5, -58
	addi r15, r15, 1
	bgtu r6, r4, .LBB151_34
.LBB151_35:
	beq r15, r3, .LBB151_4
.LBB151_36:
	addi r1, r0, 42
	bne r5, r1, .LBB151_4
.LBB151_37:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	addi r5, r0, 10
	add r3, r12, r0
	add r4, r0, r0
	jal r31, strtol
	beq r1, r0, .LBB151_46
.LBB151_38:
	add r12, r15, r12
	ldbu r15, r12+0
	beq r15, r0, .LBB151_42
.LBB151_39:
	addi r3, r0, 2
	blt r1, r3, .LBB151_41
.LBB151_40:
	addi r1, r1, -1
	stw r14+%lo(fio_l_pending), r1
.LBB151_41:
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	add r4, r12, r0
	jal r31, memmove
	jal r0, .LBB151_43
.LBB151_42:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
.LBB151_43:
	bne r15, r0, .LBB151_4
	jal r0, .LBB151_5
.LBB151_44:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.82)
	addi r4, r4, %lo(.L.str.82)
.LBB151_45:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB151_46:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.111)
	addi r4, r4, %lo(.L.str.111)
	jal r0, .LBB151_45
.Lfunc_end151:
	.size	fio_list_tok, .Lfunc_end151-fio_list_tok
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI151_0,@object
.LJTI151_0:
	.word	.LBB151_11
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_11
	.word	.LBB151_25
	.word	.LBB151_14
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_14
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_12
	.word	.LBB151_25
	.word	.LBB151_25
	.word	.LBB151_24
	.size	.LJTI151_0, 156
	.type	.LJTI151_1,@object
.LJTI151_1:
	.word	.LBB151_33
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_33
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_33
	.word	.LBB151_30
	.word	.LBB151_30
	.word	.LBB151_33
	.size	.LJTI151_1, 156
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
	beq r1, r0, .LBB152_5
.LBB152_1:
	lui r13, %hi(fio_ldone)
	ldbu r1, r13+%lo(fio_ldone)
	bne r1, r0, .LBB152_31
.LBB152_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB152_31
.LBB152_3:
	addi r14, r0, 1
	bne r1, r14, .LBB152_17
.LBB152_4:
	stb r13+%lo(fio_ldone), r14
	jal r0, .LBB152_31
.LBB152_5:
	lui r13, %hi(fio_desc)
	ldw r1, r13+%lo(fio_desc)
	lui r14, %hi(fio_rep)
	beq r1, r0, .LBB152_8
.LBB152_6:
	ldw r3, r14+%lo(fio_rep)
	ble r3, r0, .LBB152_8
.LBB152_7:
	bne r1, r0, .LBB152_9
	jal r0, .LBB152_31
.LBB152_8:
	stw r13+%lo(fio_desc), r0
	jal r31, fio_next_desc
	beq r1, r0, .LBB152_31
.LBB152_9:
	addi r3, r0, 65
	bne r1, r3, .LBB152_32
.LBB152_10:
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
	bge r1, r12, .LBB152_22
.LBB152_11:
	blt r12, r4, .LBB152_27
.LBB152_12:
	ldw r4, r3+%lo(fio_rpos)
	lui r5, %hi(fio_rlen)
	ldw r5, r5+%lo(fio_rlen)
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	add r8, r0, r0
	jal r0, .LBB152_14
.LBB152_13:
	add r10, r11, r8
	stb r10+0, r9
	addi r8, r8, 1
	beq r12, r8, .LBB152_27
.LBB152_14:
	add r9, r6, r0
	bge r8, r1, .LBB152_13
.LBB152_15:
	add r10, r4, r8
	add r9, r6, r0
	bge r10, r5, .LBB152_13
.LBB152_16:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB152_13
.LBB152_17:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB152_31
.LBB152_18:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB152_20
.LBB152_19:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB152_31
.LBB152_20:
	add r5, r3, r0
	bge r4, r1, .LBB152_19
.LBB152_21:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB152_19
.LBB152_22:
	blt r12, r4, .LBB152_27
.LBB152_23:
	ldw r5, r3+%lo(fio_rpos)
	lui r4, %hi(fio_rlen)
	ldw r4, r4+%lo(fio_rlen)
	sub r8, r0, r12
	add r5, r1, r5
	addi r6, r0, 32
	lui r7, %hi(fio_rec)
	addi r7, r7, %lo(fio_rec)
	jal r0, .LBB152_25
.LBB152_24:
	stb r11+0, r9
	addi r9, r8, 1
	sltu r10, r9, r8
	addi r11, r11, 1
	add r8, r9, r0
	bne r10, r0, .LBB152_27
.LBB152_25:
	add r10, r5, r8
	add r9, r6, r0
	bge r10, r4, .LBB152_24
.LBB152_26:
	add r9, r10, r7
	ldbu r9, r9+0
	jal r0, .LBB152_24
.LBB152_27:
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldw r1, r14+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB152_29
.LBB152_28:
	addi r1, r1, -1
	stw r14+%lo(fio_rep), r1
.LBB152_29:
	ldw r1, r14+%lo(fio_rep)
	bne r1, r0, .LBB152_31
.LBB152_30:
	stw r13+%lo(fio_desc), r0
.LBB152_31:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB152_32:
	lui r3, %hi(stderr)
	ldw r3, r3+%lo(stderr)
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end152:
	.size	f77_rd_s, .Lfunc_end152-f77_rd_s
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_in_real
	.type	.LCPI153_0,@object
.LCPI153_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI153_0, 8
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
	blt r3, r5, .LBB153_13
.LBB153_1:
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
	jal r0, .LBB153_5
.LBB153_2:
	addi r18, r16, 1
	stw sp+16, r1
	add r1, sp, r0
	addi r1, r1, 2047
	addi r20, r1, 41
	ldw r1, sp+16
	add r16, r20, r16
	stb r16+0, r19
	add r16, r18, r0
.LBB153_3:
	add r18, r13, r0
.LBB153_4:
	addi r17, r17, -1
	addi r7, r7, 1
	beq r17, r0, .LBB153_14
.LBB153_5:
	add r19, r10, r0
	bge r7, r8, .LBB153_7
.LBB153_6:
	add r19, r7, lr
	ldb r19, r19+0
.LBB153_7:
	beq r19, r10, .LBB153_9
.LBB153_8:
	bne r19, r14, .LBB153_12
.LBB153_9:
	beq r9, r0, .LBB153_4
.LBB153_10:
	beq r18, r0, .LBB153_4
.LBB153_11:
	add r19, r11, r0
	blt r16, r12, .LBB153_2
	jal r0, .LBB153_4
.LBB153_12:
	bgt r16, r15, .LBB153_3
	jal r0, .LBB153_2
.LBB153_13:
	add r16, r0, r0
.LBB153_14:
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
	lui r8, %hi(.LCPI153_0)
	addi r8, r8, %lo(.LCPI153_0)
	ldw r7, r8+4
	ldw r6, r8+0
	beq r3, r0, .LBB153_67
.LBB153_15:
	addi lr, r0, 43
	beq r3, lr, .LBB153_18
.LBB153_16:
	addi r8, r0, 45
	bne r3, r8, .LBB153_19
.LBB153_17:
	addi r10, r0, 1
	add r8, r0, r0
	jal r0, .LBB153_21
.LBB153_18:
	add r10, r5, r0
	jal r0, .LBB153_20
.LBB153_19:
	add r10, r0, r0
.LBB153_20:
	add r8, r5, r0
.LBB153_21:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r3, r9, 41
	or  r3, r3, r10
	ldbu r19, r3+0
	beq r19, r0, .LBB153_42
.LBB153_22:
	addi r13, r0, 1
	addi r14, r0, 46
	addi r11, r0, 0
	addi r15, r0, -10
	addi r16, r0, 70
	lui r17, %hi(.LJTI153_0)
	addi r17, r17, %lo(.LJTI153_0)
	add r9, r13, r0
	add r12, r0, r0
	add r18, r0, r0
	add r3, r0, r0
.LBB153_23:
	andi r5, r19, 255
	bne r5, r14, .LBB153_25
.LBB153_24:
	add r18, r13, r0
	jal r0, .LBB153_34
.LBB153_25:
	slli r20, r19, 24
	srai r20, r20, 24
	addi r20, r20, -58
	bgeu r20, r15, .LBB153_31
.LBB153_26:
	addi r5, r5, -43
	bgtu r5, r16, .LBB153_40
.LBB153_27:
	slli r5, r5, 2
	add r5, r17, r5
	ldw r20, r5+0
	add r5, r0, r0
	add r19, r13, r0
	jalr r0, r20, 0
.LBB153_28:
	addi r5, r10, 1
	add r20, sp, r0
	addi r20, r20, 2047
	addi r19, r20, 41
	add r19, r19, r5
	ldbu r19, r19+0
	beq r19, lr, .LBB153_35
.LBB153_29:
	addi r20, r0, 45
	bne r19, r20, .LBB153_37
.LBB153_30:
	addi r9, r0, -1
	add r10, r5, r0
	jal r0, .LBB153_37
.LBB153_31:
	addi r5, r0, 1022
	bgt r3, r5, .LBB153_33
.LBB153_32:
	addi r5, r3, 1
	addi r20, sp, 1064
	add r3, r20, r3
	stb r3+0, r19
	add r3, r5, r0
.LBB153_33:
	sne r5, r18, r11
	add r12, r12, r5
.LBB153_34:
	add r5, r13, r0
	add r19, r13, r0
	bne r19, r0, .LBB153_39
	jal r0, .LBB153_41
.LBB153_35:
	add r10, r5, r0
	jal r0, .LBB153_37
.LBB153_36:
	addi r9, r0, -1
.LBB153_37:
	add r5, r0, r0
	add r19, r13, r0
.LBB153_38:
	beq r19, r0, .LBB153_41
.LBB153_39:
	addi r10, r10, 1
	add r20, sp, r0
	addi r20, r20, 2047
	addi r19, r20, 41
	add r19, r19, r10
	ldbu r19, r19+0
	sne r20, r19, r11
	and r20, r20, r5
	bne r20, r0, .LBB153_23
	jal r0, .LBB153_41
.LBB153_40:
	add r5, r13, r0
	add r19, r0, r0
	bne r19, r0, .LBB153_39
.LBB153_41:
	seq lr, r18, r11
	xor r4, r4, r12
	sub lr, r0, lr
	and r4, r4, lr
	xor r4, r12, r4
	beq r5, r0, .LBB153_43
	jal r0, .LBB153_47
.LBB153_42:
	add r3, r0, r0
	add r9, r5, r0
	bne r5, r0, .LBB153_47
.LBB153_43:
	add r11, sp, r0
	addi r11, r11, 2047
	addi lr, r11, 41
	add lr, lr, r10
	ldbu r12, lr+0
	beq r12, r0, .LBB153_47
.LBB153_44:
	add r11, sp, r0
	addi r11, r11, 2047
	addi lr, r11, 41
	add r10, r10, lr
	addi r10, r10, 1
	addi lr, r0, 9
	addi r11, r0, 10
	add r13, r0, r0
.LBB153_45:
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
	bne r12, r0, .LBB153_45
.LBB153_46:
	mul r10, r13, r9
	jal r0, .LBB153_48
.LBB153_47:
	add r10, r0, r0
.LBB153_48:
	addi r9, sp, 1064
	add r9, r9, r3
	stb r9+0, r0
	beq r3, r0, .LBB153_67
.LBB153_49:
	sub r9, r3, r4
	lui r6, %hi(fio_scale)
	ldw r6, r6+%lo(fio_scale)
	sub r5, r0, r5
	and r6, r6, r5
	addi lr, sp, 24
	add r5, r0, r0
	bne r8, r0, .LBB153_51
.LBB153_50:
	addi lr, r1, 1
	addi r1, r0, 45
	stb sp+24, r1
	addi r5, r0, 1
.LBB153_51:
	sub r6, r10, r6
	addi r7, r0, 1
	blt r9, r7, .LBB153_56
.LBB153_52:
	blt r3, r7, .LBB153_62
.LBB153_53:
	xor r1, r9, r3
	slt r4, r9, r3
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r3, r1
	addi r4, sp, 24
	or  r7, r4, r5
	add r4, r0, r0
.LBB153_54:
	addi r8, sp, 1064
	add r8, r8, r4
	ldbu r8, r8+0
	add r9, r7, r4
	stb r9+0, r8
	addi r4, r4, 1
	bne r1, r4, .LBB153_54
.LBB153_55:
	add r5, r5, r4
	jal r0, .LBB153_63
.LBB153_56:
	addi r1, sp, 24
	or  r1, r1, r5
	addi r8, r0, 48
	stb r1+0, r8
	addi r1, r5, 2
	addi r5, r0, 46
	stb lr+1, r5
	addi r5, r0, -1
	bgt r9, r5, .LBB153_59
.LBB153_57:
	sub r4, r4, r3
.LBB153_58:
	add r5, r1, r0
	addi r1, r1, 1
	addi r9, sp, 24
	add r5, r9, r5
	stb r5+0, r8
	addi r4, r4, -1
	bne r4, r0, .LBB153_58
.LBB153_59:
	blt r3, r7, .LBB153_66
.LBB153_60:
	addi r4, sp, 1064
.LBB153_61:
	add r5, r1, r0
	ldbu r7, r4+0
	addi r1, r1, 1
	addi r8, sp, 24
	add r5, r8, r5
	stb r5+0, r7
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r0, .LBB153_61
	jal r0, .LBB153_66
.LBB153_62:
	add r4, r0, r0
.LBB153_63:
	addi r1, sp, 24
	add r1, r1, r5
	addi r7, r0, 46
	stb r1+0, r7
	addi r1, r5, 1
	ble r3, r4, .LBB153_66
.LBB153_64:
	sub r3, r3, r4
	addi r5, sp, 1064
	add r4, r5, r4
.LBB153_65:
	ldbu r5, r4+0
	addi r7, sp, 24
	add r7, r7, r1
	stb r7+0, r5
	addi r1, r1, 1
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r0, .LBB153_65
.LBB153_66:
	addi r11, sp, 24
	add r3, r11, r1
	addi r4, r0, 1040
	sub r4, r4, r1
	lui r5, %hi(.L.str.113)
	addi r5, r5, %lo(.L.str.113)
	jal r31, snprintf
	add r3, r11, r0
	add r4, r0, r0
	jal r31, strtod
	add r6, r1, r0
	add r7, r2, r0
.LBB153_67:
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
.Lfunc_end153:
	.size	fio_in_real, .Lfunc_end153-fio_in_real
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI153_0,@object
.LJTI153_0:
	.word	.LBB153_38
	.word	.LBB153_40
	.word	.LBB153_36
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_28
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_40
	.word	.LBB153_28
	.size	.LJTI153_0, 284
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
	bne r1, r0, .LBB154_6
.LBB154_1:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB154_5
.LBB154_2:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB154_5
.LBB154_3:
	addi r3, r0, 65
	bne r1, r3, .LBB154_6
.LBB154_4:
	add r3, r11, r0
	add r4, r12, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_rd_s
.LBB154_5:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 65
	beq r1, r3, .LBB154_4
.LBB154_6:
	addi r13, r0, 2
	bne r12, r13, .LBB154_8
.LBB154_7:
	ldh r1, r11+0
	jal r0, .LBB154_9
.LBB154_8:
	ldbu r1, r11+0
.LBB154_9:
	stw sp+16, r1
	addi r3, sp, 16
	jal r31, f77_rd_i
	ldw r1, sp+16
	bne r12, r13, .LBB154_11
.LBB154_10:
	sth r11+0, r1
	jal r0, .LBB154_12
.LBB154_11:
	stb r11+0, r1
.LBB154_12:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end154:
	.size	f77_rd_in, .Lfunc_end154-f77_rd_in
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
	bne r1, r0, .LBB155_19
.LBB155_1:
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB155_18
.LBB155_2:
	ldw r5, r12+%lo(fio_rep)
	ble r5, r0, .LBB155_18
.LBB155_3:
	addi r5, r0, 65
	bne r1, r5, .LBB155_19
.LBB155_4:
	sth sp+28, r3
	lui r1, %hi(fio_w)
	ldw r5, r1+%lo(fio_w)
	addi r6, r0, 2
	bne r4, r6, .LBB155_20
.LBB155_5:
	addi r1, r0, 1
	beq r5, r1, .LBB155_9
.LBB155_6:
	addi r1, r0, 3
	blt r5, r1, .LBB155_8
.LBB155_7:
	lui r1, %hi(fio_col)
	ldw r3, r1+%lo(fio_col)
	add r3, r5, r3
	addi r3, r3, -2
	stw r1+%lo(fio_col), r3
.LBB155_8:
	add r1, r6, r0
.LBB155_9:
	lui r3, %hi(fio_len)
	ldw r7, r3+%lo(fio_len)
	lui r4, %hi(fio_col)
	ldw r10, r4+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r8, r0, 32
	add r9, r0, r0
	jal r0, .LBB155_12
.LBB155_10:
	add r10, lr, r0
.LBB155_11:
	addi r9, r9, 1
	beq r9, r1, .LBB155_29
.LBB155_12:
	addi lr, sp, 28
	bgt r10, r5, .LBB155_11
.LBB155_13:
	bge r7, r10, .LBB155_16
.LBB155_14:
	addi r13, r7, 1
	add r7, r7, r6
	stb r7+0, r8
	add r7, r13, r0
	bne r10, r13, .LBB155_14
.LBB155_15:
	stw r3+%lo(fio_len), r10
	add r7, r10, r0
.LBB155_16:
	add lr, lr, r9
	ldbu lr, lr+0
	add r13, r10, r6
	stb r13+0, lr
	addi lr, r10, 1
	stw r4+%lo(fio_col), lr
	blt r10, r7, .LBB155_10
.LBB155_17:
	stw r3+%lo(fio_len), lr
	add r7, lr, r0
	jal r0, .LBB155_10
.LBB155_18:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	add r14, r4, r0
	jal r31, fio_next_desc
	add r4, r14, r0
	add r3, r13, r0
	addi r5, r0, 65
	beq r1, r5, .LBB155_4
.LBB155_19:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_wr_i
.LBB155_20:
	lui r1, %hi(fio_col)
	blt r5, r6, .LBB155_22
.LBB155_21:
	ldw r4, r1+%lo(fio_col)
	add r4, r5, r4
	addi r4, r4, -1
	stw r1+%lo(fio_col), r4
.LBB155_22:
	ldw r4, r1+%lo(fio_col)
	addi r5, r0, 1022
	bgt r4, r5, .LBB155_29
.LBB155_23:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	bge r6, r4, .LBB155_27
.LBB155_24:
	addi r8, r0, 32
.LBB155_25:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r4, r9, .LBB155_25
.LBB155_26:
	stw r5+%lo(fio_len), r4
	add r6, r4, r0
.LBB155_27:
	add r7, r4, r7
	stb r7+0, r3
	addi r3, r4, 1
	stw r1+%lo(fio_col), r3
	blt r4, r6, .LBB155_29
.LBB155_28:
	stw r5+%lo(fio_len), r3
.LBB155_29:
	ldw r1, r12+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB155_31
.LBB155_30:
	addi r1, r1, -1
	stw r12+%lo(fio_rep), r1
.LBB155_31:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB155_33
.LBB155_32:
	stw r11+%lo(fio_desc), r0
.LBB155_33:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end155:
	.size	f77_wr_in, .Lfunc_end155-f77_wr_in
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_rd_d
	.type	.LCPI156_0,@object
.LCPI156_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI156_0, 8
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
	beq r1, r0, .LBB156_5
.LBB156_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB156_41
.LBB156_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB156_41
.LBB156_3:
	addi r3, r0, 1
	bne r1, r3, .LBB156_26
.LBB156_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB156_41
.LBB156_5:
	lui r12, %hi(fio_desc)
	ldw r1, r12+%lo(fio_desc)
	lui r13, %hi(fio_rep)
	beq r1, r0, .LBB156_11
.LBB156_6:
	ldw r3, r13+%lo(fio_rep)
	ble r3, r0, .LBB156_11
.LBB156_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB156_12
.LBB156_8:
	beq r1, r0, .LBB156_41
.LBB156_9:
	addi r3, r0, 65
	bne r1, r3, .LBB156_30
.LBB156_10:
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
.LBB156_11:
	stw r12+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB156_8
.LBB156_12:
	addi r3, r0, 73
	bne r1, r3, .LBB156_29
.LBB156_13:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	blt r1, r4, .LBB156_33
.LBB156_14:
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
	jal r0, .LBB156_18
.LBB156_15:
	addi r18, r16, 1
	addi r20, sp, 24
	add r16, r20, r16
	stb r16+0, r19
	add r16, r18, r0
.LBB156_16:
	add r18, r4, r0
.LBB156_17:
	addi r17, r17, -1
	addi r5, r5, 1
	beq r17, r0, .LBB156_34
.LBB156_18:
	add r19, r8, r0
	bge r5, r6, .LBB156_20
.LBB156_19:
	add r19, r5, r9
	ldb r19, r19+0
.LBB156_20:
	beq r19, r8, .LBB156_22
.LBB156_21:
	bne r19, r14, .LBB156_25
.LBB156_22:
	beq r7, r0, .LBB156_17
.LBB156_23:
	beq r18, r0, .LBB156_17
.LBB156_24:
	add r19, r10, r0
	blt r16, lr, .LBB156_15
	jal r0, .LBB156_17
.LBB156_25:
	ble r16, r15, .LBB156_15
	jal r0, .LBB156_16
.LBB156_26:
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB156_31
.LBB156_27:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB156_28:
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
	bne r5, r0, .LBB156_28
	jal r0, .LBB156_32
.LBB156_29:
	addi r3, r0, 76
	beq r1, r3, .LBB156_42
.LBB156_30:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	jal r0, .LBB156_37
.LBB156_31:
	add r6, r0, r0
.LBB156_32:
	addi r3, sp, 24
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
	jal r0, .LBB156_41
.LBB156_33:
	add r16, r0, r0
.LBB156_34:
	addi r4, sp, 24
	add r4, r4, r16
	stb r4+0, r0
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldbu r1, sp+24
	beq r1, r0, .LBB156_36
.LBB156_35:
	addi r3, sp, 24
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	fcvt.d.w r4, r1
	jal r0, .LBB156_37
.LBB156_36:
	lui r1, %hi(.LCPI156_0)
	addi r1, r1, %lo(.LCPI156_0)
	ldw r5, r1+4
	ldw r4, r1+0
.LBB156_37:
	stw r11+4, r5
	stw r11+0, r4
	ldw r1, r13+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB156_39
.LBB156_38:
	addi r1, r1, -1
	stw r13+%lo(fio_rep), r1
.LBB156_39:
	ldw r1, r13+%lo(fio_rep)
	bne r1, r0, .LBB156_41
.LBB156_40:
	stw r12+%lo(fio_desc), r0
.LBB156_41:
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
.LBB156_42:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end156:
	.size	f77_rd_d, .Lfunc_end156-f77_rd_d
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rd_r
	.type	.LCPI157_0,@object
.LCPI157_0:
	.word	0x00000000                      # float 0
	.size	.LCPI157_0, 4
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
	beq r1, r0, .LBB157_5
.LBB157_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB157_41
.LBB157_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB157_41
.LBB157_3:
	addi r3, r0, 1
	bne r1, r3, .LBB157_26
.LBB157_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB157_41
.LBB157_5:
	lui r12, %hi(fio_desc)
	ldw r1, r12+%lo(fio_desc)
	lui r13, %hi(fio_rep)
	beq r1, r0, .LBB157_11
.LBB157_6:
	ldw r3, r13+%lo(fio_rep)
	ble r3, r0, .LBB157_11
.LBB157_7:
	addi r3, r0, 72
	bgt r1, r3, .LBB157_12
.LBB157_8:
	beq r1, r0, .LBB157_41
.LBB157_9:
	addi r3, r0, 65
	bne r1, r3, .LBB157_30
.LBB157_10:
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
.LBB157_11:
	stw r12+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB157_8
.LBB157_12:
	addi r3, r0, 73
	bne r1, r3, .LBB157_29
.LBB157_13:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r4, r0, 1
	lui r3, %hi(fio_rpos)
	blt r1, r4, .LBB157_33
.LBB157_14:
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
	jal r0, .LBB157_18
.LBB157_15:
	addi r18, r16, 1
	addi r20, sp, 24
	add r16, r20, r16
	stb r16+0, r19
	add r16, r18, r0
.LBB157_16:
	add r18, r4, r0
.LBB157_17:
	addi r17, r17, -1
	addi r5, r5, 1
	beq r17, r0, .LBB157_34
.LBB157_18:
	add r19, r8, r0
	bge r5, r6, .LBB157_20
.LBB157_19:
	add r19, r5, r9
	ldb r19, r19+0
.LBB157_20:
	beq r19, r8, .LBB157_22
.LBB157_21:
	bne r19, r14, .LBB157_25
.LBB157_22:
	beq r7, r0, .LBB157_17
.LBB157_23:
	beq r18, r0, .LBB157_17
.LBB157_24:
	add r19, r10, r0
	blt r16, lr, .LBB157_15
	jal r0, .LBB157_17
.LBB157_25:
	ble r16, r15, .LBB157_15
	jal r0, .LBB157_16
.LBB157_26:
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB157_31
.LBB157_27:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB157_28:
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
	bne r5, r0, .LBB157_28
	jal r0, .LBB157_32
.LBB157_29:
	addi r3, r0, 76
	beq r1, r3, .LBB157_42
.LBB157_30:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	jal r0, .LBB157_37
.LBB157_31:
	add r6, r0, r0
.LBB157_32:
	addi r3, sp, 24
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	jal r0, .LBB157_41
.LBB157_33:
	add r16, r0, r0
.LBB157_34:
	addi r4, sp, 24
	add r4, r4, r16
	stb r4+0, r0
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldbu r1, sp+24
	beq r1, r0, .LBB157_36
.LBB157_35:
	addi r3, sp, 24
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	fcvt.s.w r1, r1
	jal r0, .LBB157_37
.LBB157_36:
	lui r1, %hi(.LCPI157_0)
	ldw r1, r1+%lo(.LCPI157_0)
.LBB157_37:
	stw r11+0, r1
	ldw r1, r13+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB157_39
.LBB157_38:
	addi r1, r1, -1
	stw r13+%lo(fio_rep), r1
.LBB157_39:
	ldw r1, r13+%lo(fio_rep)
	bne r1, r0, .LBB157_41
.LBB157_40:
	stw r12+%lo(fio_desc), r0
.LBB157_41:
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
.LBB157_42:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end157:
	.size	f77_rd_r, .Lfunc_end157-f77_rd_r
                                        # -- End function
	.globl	f77_rd_c                        # -- Begin function f77_rd_c
	.p2align	2
	.type	f77_rd_c,@function
f77_rd_c:                               # @f77_rd_c
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB158_5
.LBB158_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB158_7
.LBB158_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB158_7
.LBB158_3:
	addi r3, r0, 1
	bne r1, r3, .LBB158_6
.LBB158_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB158_7
.LBB158_5:
	add r3, r11, r0
	jal r31, f77_rd_r
	addi r3, r11, 4
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_rd_r
.LBB158_6:
	addi r3, sp, 32
	addi r4, sp, 24
	jal r31, fio_list_cplx_in
	ldw r5, sp+36
	ldw r4, sp+32
	fcvt.s.d r1, r4
	stw r11+0, r1
	ldw r5, sp+28
	ldw r4, sp+24
	fcvt.s.d r1, r4
	stw r11+4, r1
.LBB158_7:
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end158:
	.size	f77_rd_c, .Lfunc_end158-f77_rd_c
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_list_cplx_in
	.type	.LCPI159_0,@object
.LCPI159_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI159_0, 8
	.text
	.p2align	2
	.type	fio_list_cplx_in,@function
fio_list_cplx_in:                       # @fio_list_cplx_in
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
	add r11, r4, r0
	add r12, r3, r0
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	ldbu r1, r13+0
	addi r3, r0, 40
	bne r1, r3, .LBB159_6
.LBB159_1:
	lui r15, %hi(fio_ltok+1)
	addi r15, r15, %lo(fio_ltok+1)
	ldbu r4, r15+0
	beq r4, r0, .LBB159_9
.LBB159_2:
	lui r1, %hi(fio_ltok+2)
	addi r1, r1, %lo(fio_ltok+2)
	addi r3, r0, 126
	add r14, r0, r0
	jal r0, .LBB159_4
.LBB159_3:
	ldbu r4, r1+0
	addi r1, r1, 1
	beq r4, r0, .LBB159_10
.LBB159_4:
	bgt r14, r3, .LBB159_3
.LBB159_5:
	addi r5, r14, 1
	addi r6, sp, 20
	add r6, r6, r14
	stb r6+0, r4
	add r14, r5, r0
	jal r0, .LBB159_3
.LBB159_6:
	beq r1, r0, .LBB159_22
.LBB159_7:
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB159_8:
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
	bne r1, r0, .LBB159_8
	jal r0, .LBB159_23
.LBB159_9:
	add r14, r0, r0
.LBB159_10:
	lui r16, %hi(fio_rpos)
	lui r17, %hi(fio_rlen)
	addi r18, r0, 126
	lui r19, %hi(fio_rec)
	addi r19, r19, %lo(fio_rec)
	addi r20, r0, 41
.LBB159_11:
	addi r1, sp, 20
	add r21, r1, r14
	jal r0, .LBB159_13
.LBB159_12:
	jal r31, fio_getrec
	bne r1, r0, .LBB159_37
.LBB159_13:
	beq r14, r0, .LBB159_15
.LBB159_14:
	ldbu r1, r21+-1
	beq r1, r20, .LBB159_18
.LBB159_15:
	ldw r1, r16+%lo(fio_rpos)
	ldw r3, r17+%lo(fio_rlen)
	bge r1, r3, .LBB159_12
.LBB159_16:
	addi r3, r1, 1
	stw r16+%lo(fio_rpos), r3
	bgt r14, r18, .LBB159_13
.LBB159_17:
	add r1, r1, r19
	ldbu r1, r1+0
	addi r3, r14, 1
	addi r4, sp, 20
	add r4, r4, r14
	stb r4+0, r1
	add r14, r3, r0
	jal r0, .LBB159_11
.LBB159_18:
	stb r21+-1, r0
	addi r3, sp, 20
	addi r4, r0, 44
	jal r31, strchr
	beq r1, r0, .LBB159_24
.LBB159_19:
	add r14, r1, r0
	stb r1+0, r0
	addi r4, sp, 20
	add r3, r13, r0
	jal r31, strcpy
	ldbu r4, r13+0
	beq r4, r0, .LBB159_27
.LBB159_20:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB159_21:
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
	bne r4, r0, .LBB159_21
	jal r0, .LBB159_28
.LBB159_22:
	add r6, r0, r0
.LBB159_23:
	addi r3, sp, 148
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r12+4, r2
	stw r12+0, r1
	stw r11+4, r0
	stw r11+0, r0
	jal r0, .LBB159_36
.LBB159_24:
	addi r4, sp, 20
	add r3, r13, r0
	jal r31, strcpy
	ldbu r4, r13+0
	beq r4, r0, .LBB159_33
.LBB159_25:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB159_26:
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
	bne r4, r0, .LBB159_26
	jal r0, .LBB159_34
.LBB159_27:
	add r5, r0, r0
.LBB159_28:
	addi r3, sp, 148
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r12+4, r2
	stw r12+0, r1
	addi r4, r14, 1
	add r3, r13, r0
	jal r31, strcpy
	ldbu r4, r13+0
	beq r4, r0, .LBB159_31
.LBB159_29:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB159_30:
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
	bne r4, r0, .LBB159_30
	jal r0, .LBB159_32
.LBB159_31:
	add r5, r0, r0
.LBB159_32:
	addi r3, sp, 148
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	jal r0, .LBB159_35
.LBB159_33:
	add r5, r0, r0
.LBB159_34:
	addi r3, sp, 148
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r12+4, r2
	stw r12+0, r1
	lui r1, %hi(.LCPI159_0)
	addi r1, r1, %lo(.LCPI159_0)
	ldw r5, r1+4
	ldw r4, r1+0
.LBB159_35:
	stw r11+4, r5
	stw r11+0, r4
	lui r1, %hi(fio_l_after)
	addi r3, r0, 1
	stb r1+%lo(fio_l_after), r3
.LBB159_36:
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
.LBB159_37:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.82)
	addi r4, r4, %lo(.L.str.82)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end159:
	.size	fio_list_cplx_in, .Lfunc_end159-fio_list_cplx_in
                                        # -- End function
	.globl	f77_rd_z                        # -- Begin function f77_rd_z
	.p2align	2
	.type	f77_rd_z,@function
f77_rd_z:                               # @f77_rd_z
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB160_5
.LBB160_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB160_7
.LBB160_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB160_7
.LBB160_3:
	addi r3, r0, 1
	bne r1, r3, .LBB160_6
.LBB160_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB160_7
.LBB160_5:
	add r3, r11, r0
	jal r31, f77_rd_d
	addi r3, r11, 8
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_rd_d
.LBB160_6:
	addi r3, sp, 32
	addi r4, sp, 24
	jal r31, fio_list_cplx_in
	ldw r1, sp+36
	ldw r3, sp+32
	stw r11+4, r1
	stw r11+0, r3
	ldw r1, sp+28
	ldw r3, sp+24
	stw r11+12, r1
	stw r11+8, r3
.LBB160_7:
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end160:
	.size	f77_rd_z, .Lfunc_end160-f77_rd_z
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
.Lfunc_end161:
	.size	f77_rd_end, .Lfunc_end161-f77_rd_end
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
	blt r5, r26, .LBB162_3
.LBB162_1:
	add r13, r4, r0
	add r15, r0, r0
.LBB162_2:
	add r1, r13, r15
	ldbu r3, r1+0
	jal r31, toupper
	add r3, r15, r14
	stb r3+0, r1
	addi r15, r15, 1
	bne r12, r15, .LBB162_2
.LBB162_3:
	add r1, r12, r14
	stb r1+0, r0
	lui r21, %hi(fio_busy)
	lui r1, %hi(fio_ibuf)
	stw sp+68, r1
	beq r11, r0, .LBB162_66
.LBB162_4:
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
	lui r13, %hi(.L.str.114)
	addi r13, r13, %lo(.L.str.114)
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
.LBB162_5:
	ldw r3, r25+%lo(nml_unit)
	add r4, r13, r0
	add r5, r11, r0
	add r6, r11, r0
	jal r31, f77_rd_begin
	beq r1, r0, .LBB162_8
.LBB162_6:
	addi r14, r0, 1
	bne r1, r14, .LBB162_12
.LBB162_7:
	add fp, r1, r0
	add r14, r1, r0
	jal r0, .LBB162_62
.LBB162_8:
	addi r3, sp, 72
	add r4, r15, r0
	jal r31, f77_rd_s
	stb r21+%lo(fio_busy), r0
	ldw r1, sp+64
	stb r1+%lo(fio_reading), r0
	ldw r1, sp+68
	stw r1+%lo(fio_ibuf), r0
	add r1, r20, r0
.LBB162_9:
	addi r3, sp, 72
	add r3, r3, r1
	ldbu r3, r3+0
	bne r3, r23, .LBB162_14
.LBB162_10:
	addi r3, r1, -1
	addi r4, r1, 1
	add r1, r3, r0
	bgtu r4, r24, .LBB162_9
.LBB162_11:
	add r28, r26, r0
	jal r0, .LBB162_15
.LBB162_12:
	ldw r1, r22+%lo(nml_status)
	beq r1, r0, .LBB162_21
.LBB162_13:
	add fp, r0, r0
	jal r0, .LBB162_62
.LBB162_14:
	addi r28, r1, 1
.LBB162_15:
	add r22, r26, r0
	add r19, r26, r0
	bne r16, r0, .LBB162_41
.LBB162_16:
	blt r28, r24, .LBB162_22
.LBB162_17:
	addi r1, r28, -1
	add r14, r0, r0
.LBB162_18:
	addi r3, sp, 72
	add r3, r3, r14
	ldbu r3, r3+1
	bne r3, r23, .LBB162_23
.LBB162_19:
	addi r14, r14, 1
	bne r1, r14, .LBB162_18
.LBB162_20:
	add r22, r0, r0
	add r19, r28, r0
	jal r0, .LBB162_40
.LBB162_21:
	stw r22+%lo(nml_status), r24
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.115)
	addi r4, r4, %lo(.L.str.115)
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
	add fp, r0, r0
	jal r0, .LBB162_62
.LBB162_22:
	add r22, r0, r0
	add r19, r26, r0
	jal r0, .LBB162_40
.LBB162_23:
	addi r1, r0, 36
	beq r3, r1, .LBB162_25
.LBB162_24:
	addi r1, r0, 38
	bne r3, r1, .LBB162_38
.LBB162_25:
	addi r19, r14, 1
	addi r22, r0, 1
	ldw r1, sp+60
	blt r1, r22, .LBB162_30
.LBB162_26:
	ldw r1, sp+40
	add r12, r1, r14
	sub r1, r14, r28
	addi r16, r1, 2
	add r17, r0, r0
.LBB162_27:
	add r1, r16, r17
	beq r1, r0, .LBB162_39
.LBB162_28:
	add r1, r12, r17
	ldbu r3, r1+0
	jal r31, toupper
	ldw r3, sp+56
	add r3, r17, r3
	ldb r3, r3+0
	bne r1, r3, .LBB162_39
.LBB162_29:
	addi r17, r17, 1
	ldw r1, sp+60
	bne r1, r17, .LBB162_27
.LBB162_30:
	ldw r1, sp+36
	add r12, r1, r14
	bge r12, r28, .LBB162_36
.LBB162_31:
	ldw r1, sp+32
	add r1, r1, r14
	ldbu r16, r1+0
	add r3, r16, r0
	jal r31, isalnum
	bne r1, r0, .LBB162_39
.LBB162_32:
	addi r1, r16, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB162_39
.LBB162_33:
	addi r1, r0, 64
	beq r16, r1, .LBB162_39
.LBB162_34:
	addi r3, r0, 95
	add r22, r0, r0
	add r1, r0, r0
	add r14, r11, r0
	beq r16, r3, .LBB162_37
.LBB162_35:
	addi r22, r0, 1
.LBB162_36:
	add r19, r12, r0
	add r1, r22, r0
	add r14, r0, r0
.LBB162_37:
	bne r1, r0, .LBB162_41
	jal r0, .LBB162_59
.LBB162_38:
	addi r19, r14, 1
.LBB162_39:
	add r22, r0, r0
.LBB162_40:
	add r1, r0, r0
	add r14, r11, r0
	beq r1, r0, .LBB162_59
.LBB162_41:
	sub r16, r28, r19
	ble r28, r19, .LBB162_60
.LBB162_42:
	addi r1, sp, 72
	add r17, r1, r19
	add r18, r0, r0
	add r1, r0, r0
	jal r0, .LBB162_44
.LBB162_43:
	addi r18, r18, 1
	beq r16, r18, .LBB162_60
.LBB162_44:
	add r12, r17, r18
	ldb r3, r12+0
	beq r1, r0, .LBB162_46
.LBB162_45:
	seq r3, r1, r3
	sub r3, r0, r3
	and r3, r1, r3
	xor r1, r1, r3
	addi r14, r0, 13
	addi r3, r0, 13
	bne r14, r3, .LBB162_58
	jal r0, .LBB162_43
.LBB162_46:
	andi r1, r3, 255
	addi r14, r0, 13
	addi r4, r0, 34
	beq r1, r4, .LBB162_50
.LBB162_47:
	addi r4, r0, 39
	beq r1, r4, .LBB162_50
.LBB162_48:
	addi r4, r0, 47
	bne r1, r4, .LBB162_51
.LBB162_49:
	add r3, r17, r0
	add r4, r18, r0
	jal r31, nml_text_add
	addi r14, r0, 1
	add fp, r0, r0
	jal r0, .LBB162_57
.LBB162_50:
	add r1, r3, r0
	addi r3, r0, 13
	bne r14, r3, .LBB162_58
	jal r0, .LBB162_43
.LBB162_51:
	addi r1, r0, -3
	and r1, r3, r1
	addi r3, r0, 36
	bne r1, r3, .LBB162_56
.LBB162_52:
	add r1, r19, r18
	addi r1, r1, 3
	bge r1, r28, .LBB162_56
.LBB162_53:
	ldbu r3, r12+1
	jal r31, toupper
	addi r3, r0, 69
	bne r1, r3, .LBB162_56
.LBB162_54:
	ldbu r3, r12+2
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB162_56
.LBB162_55:
	ldbu r3, r12+3
	jal r31, toupper
	addi r3, r0, 68
	beq r1, r3, .LBB162_49
.LBB162_56:
	add r14, r0, r0
.LBB162_57:
	add r1, r0, r0
	addi r3, r0, 13
	beq r14, r3, .LBB162_43
.LBB162_58:
	beq r14, r0, .LBB162_43
.LBB162_59:
	add r16, r22, r0
	jal r0, .LBB162_61
.LBB162_60:
	addi r1, sp, 72
	add r3, r1, r19
	add r4, r16, r0
	jal r31, nml_text_add
	lui r3, %hi(.L.str.116)
	addi r3, r3, %lo(.L.str.116)
	addi r4, r0, 1
	jal r31, nml_text_add
	add r16, r22, r0
	add r14, r0, r0
.LBB162_61:
	add r22, r27, r0
.LBB162_62:
	beq r14, r0, .LBB162_5
.LBB162_63:
	addi r1, r0, 3
	beq r14, r1, .LBB162_5
.LBB162_64:
	beq fp, r0, .LBB162_72
.LBB162_65:
	addi r1, r0, 1
	stw r22+%lo(nml_status), r1
	jal r0, .LBB162_71
.LBB162_66:
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
	lui r4, %hi(.L.str.114)
	addi r4, r4, %lo(.L.str.114)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	add r3, r11, r0
	add r4, r12, r0
	jal r31, f77_wr_a
	stb r21+%lo(fio_busy), r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB162_70
.LBB162_67:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB162_69
.LBB162_68:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB162_70
.LBB162_69:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB162_70:
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
.LBB162_71:
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
.LBB162_72:
	ldw r1, sp+44
	stw r1+%lo(nml_nasg), r0
	ldw r1, sp+52
	stw r1+%lo(nml_nval), r0
	ldw r21, sp+48
	ldw r1, r21+%lo(nml_tlen)
	addi r15, r0, 1
	blt r1, r15, .LBB162_71
.LBB162_73:
	ldw r1, r22+%lo(nml_status)
	bne r1, r0, .LBB162_71
.LBB162_74:
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
	lui r1, %hi(.L.str.119)
	addi r1, r1, %lo(.L.str.119)
	stw sp+60, r1
	addi r13, r0, 95
	lui r28, %hi(nml_err+95)
	add r12, r15, r0
	add fp, r0, r0
	add r27, r0, r0
.LBB162_75:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r27
	ldbu r3, r1+0
	beq r3, r17, .LBB162_78
.LBB162_76:
	bne r3, r23, .LBB162_85
.LBB162_77:
	addi r11, r27, 1
	jal r0, .LBB162_198
.LBB162_78:
	beq r12, r0, .LBB162_84
.LBB162_79:
	beq fp, r0, .LBB162_84
.LBB162_80:
	ldw r1, sp+52
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB162_83
.LBB162_81:
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
	beq r1, r0, .LBB162_211
.LBB162_82:
	stw r25+%lo(nml_val), r1
.LBB162_83:
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
.LBB162_84:
	addi r11, r27, 1
	jal r0, .LBB162_197
.LBB162_85:
	jal r31, isalpha
                                        # implicit-def: $r11
	add r5, r15, r0
	beq r1, r0, .LBB162_106
.LBB162_86:
	ldw r1, r21+%lo(nml_tlen)
	add r19, r27, r0
	bge r27, r1, .LBB162_93
.LBB162_87:
	add r19, r27, r0
	jal r0, .LBB162_89
.LBB162_88:
	addi r19, r19, 1
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB162_93
.LBB162_89:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r19
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB162_88
.LBB162_90:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB162_88
.LBB162_91:
	beq r14, r13, .LBB162_88
.LBB162_92:
	addi r1, r0, 64
	beq r14, r1, .LBB162_88
.LBB162_93:
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB162_103
.LBB162_94:
	ldw r3, r16+%lo(nml_text)
.LBB162_95:
	add r4, r3, r19
	ldbu r4, r4+0
	bne r4, r23, .LBB162_98
.LBB162_96:
	addi r19, r19, 1
	bne r1, r19, .LBB162_95
.LBB162_97:
	add r4, r1, r0
	jal r0, .LBB162_104
.LBB162_98:
	addi r5, r0, 40
	bne r4, r5, .LBB162_103
.LBB162_99:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r19, r1, .LBB162_106
.LBB162_100:
	add r5, r3, r19
	ldbu r6, r5+0
	addi r4, r19, 1
	addi r7, r0, 41
	beq r6, r7, .LBB162_206
.LBB162_101:
	add r19, r4, r0
	blt r4, r1, .LBB162_100
.LBB162_102:
                                        # implicit-def: $r11
	add r5, r15, r0
	jal r0, .LBB162_106
.LBB162_103:
	add r4, r19, r0
.LBB162_104:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r4, r1, .LBB162_106
.LBB162_105:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r4
	ldbu r1, r1+0
	addi r3, r0, 61
	sne r5, r1, r3
	addi r11, r4, 1
.LBB162_106:
	beq r5, r0, .LBB162_116
.LBB162_107:
	beq fp, r0, .LBB162_163
.LBB162_108:
	ldw r1, r21+%lo(nml_tlen)
	add r11, r27, r0
	bge r27, r1, .LBB162_111
.LBB162_109:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB162_111
.LBB162_110:
	addi r11, r11, 1
	ldw r1, r21+%lo(nml_tlen)
	blt r11, r1, .LBB162_109
.LBB162_111:
	ble r11, r27, .LBB162_130
.LBB162_112:
	ldw r1, r21+%lo(nml_tlen)
	bge r11, r1, .LBB162_130
.LBB162_113:
	ldw r1, r16+%lo(nml_text)
	add r3, r1, r11
	ldbu r3, r3+0
	addi r4, r0, 42
	bne r3, r4, .LBB162_130
.LBB162_114:
	add r3, r1, r27
	jal r31, atoi
	ble r1, r0, .LBB162_203
.LBB162_115:
	add r14, r1, r0
	addi r27, r11, 1
	jal r0, .LBB162_131
.LBB162_116:
	ldw r1, sp+44
	ldw r4, r1+%lo(nml_nasg)
	lui r1, %hi(nml_casg)
	ldw r3, r1+%lo(nml_casg)
	lui r14, %hi(nml_asg)
	blt r4, r3, .LBB162_119
.LBB162_117:
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
	beq r1, r0, .LBB162_211
.LBB162_118:
	stw r14+%lo(nml_asg), r1
.LBB162_119:
	ldw r1, r14+%lo(nml_asg)
	ldw r5, sp+44
	ldw r3, r5+%lo(nml_nasg)
	addi r4, r3, 1
	stw r5+%lo(nml_nasg), r4
	addi r4, r0, 76
	mul r3, r3, r4
	add fp, r1, r3
	ldw r1, r21+%lo(nml_tlen)
	bge r27, r1, .LBB162_129
.LBB162_120:
	add r20, r0, r0
.LBB162_121:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r27
	add r1, r1, r20
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	addi r3, r0, 1
	beq r1, r0, .LBB162_125
.LBB162_122:
	beq r3, r0, .LBB162_166
.LBB162_123:
	addi r1, r0, 30
	bgtu r20, r1, .LBB162_166
.LBB162_124:
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
	blt r19, r1, .LBB162_121
	jal r0, .LBB162_167
.LBB162_125:
	addi r1, r14, -35
	addi r4, r0, 2
	bltu r1, r4, .LBB162_122
.LBB162_126:
	addi r1, r0, 64
	beq r14, r1, .LBB162_122
.LBB162_127:
	beq r14, r13, .LBB162_122
.LBB162_128:
	add r3, r0, r0
	jal r0, .LBB162_122
.LBB162_129:
	add r19, r27, r0
	add r20, r0, r0
	jal r0, .LBB162_168
.LBB162_130:
	add r14, r15, r0
.LBB162_131:
	add r12, r0, r0
	add r1, r0, r0
	add r11, r27, r0
	jal r0, .LBB162_136
.LBB162_132:
	add r1, r0, r0
.LBB162_133:
	add r4, r11, r0
.LBB162_134:
	addi r11, r4, 1
	add r3, r0, r0
.LBB162_135:
	bne r3, r0, .LBB162_159
.LBB162_136:
	ldw r3, r21+%lo(nml_tlen)
	bge r11, r3, .LBB162_159
.LBB162_137:
	ldw r6, r16+%lo(nml_text)
	add r4, r6, r11
	ldb r5, r4+0
	beq r1, r0, .LBB162_141
.LBB162_138:
	bne r1, r5, .LBB162_133
.LBB162_139:
	addi r4, r11, 1
	bge r4, r3, .LBB162_132
.LBB162_140:
	add r3, r6, r4
	ldb r3, r3+0
	bne r1, r3, .LBB162_132
	jal r0, .LBB162_134
.LBB162_141:
	andi r19, r5, 255
	addi r1, r19, -32
	addi r6, r0, 9
	bgtu r1, r6, .LBB162_151
.LBB162_142:
	slli r1, r1, 2
	lui r6, %hi(.LJTI162_0)
	addi r6, r6, %lo(.LJTI162_0)
	add r1, r6, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB162_143:
	addi r11, r11, 1
	add r3, r0, r0
	add r1, r5, r0
	ldw r21, sp+48
	jal r0, .LBB162_135
.LBB162_144:
	addi r1, r0, 2
	blt r11, r1, .LBB162_151
.LBB162_145:
	addi r20, r11, 1
	bge r20, r3, .LBB162_151
.LBB162_146:
	ldbu r3, r4+-1
	jal r31, toupper
	addi r1, r1, -70
	addi r3, r0, -2
	bltu r1, r3, .LBB162_151
.LBB162_147:
	ldw r1, r16+%lo(nml_text)
	addi r21, r11, -2
	add r1, r1, r21
	ldbu r3, r1+0
	jal r31, isdigit
	bne r1, r0, .LBB162_149
.LBB162_148:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r21
	ldbu r1, r1+0
	addi r3, r0, 46
	bne r1, r3, .LBB162_151
.LBB162_149:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB162_151
.LBB162_150:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	addi r3, r0, 43
	stb r1+0, r3
	jal r0, .LBB162_156
.LBB162_151:
	bgt r12, r0, .LBB162_156
.LBB162_152:
	addi r3, r0, 1
	ldw r21, sp+48
	beq r19, r23, .LBB162_158
.LBB162_153:
	beq r19, r17, .LBB162_158
	jal r0, .LBB162_157
.LBB162_154:
	addi r12, r12, 1
	jal r0, .LBB162_156
.LBB162_155:
	addi r12, r12, -1
.LBB162_156:
	ldw r21, sp+48
.LBB162_157:
	addi r11, r11, 1
	add r3, r0, r0
.LBB162_158:
	add r1, r0, r0
	jal r0, .LBB162_135
.LBB162_159:
	ldw r1, sp+52
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB162_162
.LBB162_160:
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
	beq r1, r0, .LBB162_211
.LBB162_161:
	stw r25+%lo(nml_val), r1
.LBB162_162:
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
	jal r0, .LBB162_199
.LBB162_163:
	ldw r1, r22+%lo(nml_status)
	bne r1, r0, .LBB162_165
.LBB162_164:
	stw r22+%lo(nml_status), r24
	ldw r3, sp+64
	ldw r4, sp+60
	add r5, r13, r0
	jal r31, strncpy
	stb r28+%lo(nml_err+95), r0
.LBB162_165:
	add r11, r27, r0
	add fp, r0, r0
	add r14, r15, r0
	jal r0, .LBB162_199
.LBB162_166:
	add r19, r27, r20
	jal r0, .LBB162_168
.LBB162_167:
	add r20, r3, r0
.LBB162_168:
	add r1, fp, r20
	stb r1+0, r0
	jal r0, .LBB162_170
.LBB162_169:
	addi r19, r19, 1
.LBB162_170:
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB162_175
.LBB162_171:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r19
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB162_169
.LBB162_172:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB162_169
.LBB162_173:
	beq r14, r13, .LBB162_169
.LBB162_174:
	addi r1, r0, 64
	beq r14, r1, .LBB162_169
.LBB162_175:
	ldw r1, r21+%lo(nml_tlen)
	bge r19, r1, .LBB162_180
.LBB162_176:
	ldw r3, r16+%lo(nml_text)
.LBB162_177:
	add r4, r3, r19
	ldbu r4, r4+0
	bne r4, r23, .LBB162_180
.LBB162_178:
	addi r19, r19, 1
	bne r1, r19, .LBB162_177
.LBB162_179:
	add r19, r1, r0
.LBB162_180:
	stw fp+32, r0
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r19
	ldbu r1, r1+0
	addi r3, r0, 40
	bne r1, r3, .LBB162_196
.LBB162_181:
	addi r20, r19, 1
	addi r19, fp, 36
	jal r0, .LBB162_184
.LBB162_182:
	ldw r1, r22+%lo(nml_status)
	addi r14, r0, 1
	beq r1, r0, .LBB162_194
.LBB162_183:
	addi r1, r0, 11
	bne r14, r1, .LBB162_195
.LBB162_184:
	ldw r1, r16+%lo(nml_text)
	addi r20, r20, -1
.LBB162_185:
	add r3, r1, r20
	ldbu r3, r3+1
	addi r20, r20, 1
	beq r3, r23, .LBB162_185
.LBB162_186:
	add r3, r1, r20
	addi r4, sp, 72
	addi r14, r0, 10
	add r5, r14, r0
	jal r31, strtol
	ldw r4, sp+72
	ldw r3, r16+%lo(nml_text)
	add r5, r3, r20
	beq r5, r4, .LBB162_182
.LBB162_187:
	ldw r5, fp+32
	addi r6, r0, 6
	bgt r5, r6, .LBB162_189
.LBB162_188:
	addi r6, r5, 1
	stw fp+32, r6
	slli r5, r5, 2
	add r5, r19, r5
	stw r5+0, r1
.LBB162_189:
	sub r20, r4, r3
.LBB162_190:
	add r1, r3, r20
	ldbu r1, r1+0
	bne r1, r23, .LBB162_192
.LBB162_191:
	addi r20, r20, 1
	jal r0, .LBB162_190
.LBB162_192:
	bne r1, r17, .LBB162_183
.LBB162_193:
	addi r20, r20, 1
	addi r14, r0, 11
	jal r0, .LBB162_183
.LBB162_194:
	addi r1, r0, 2
	stw r22+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.118)
	addi r4, r4, %lo(.L.str.118)
	addi r5, r0, 95
	jal r31, strncpy
	stb r28+%lo(nml_err+95), r0
	jal r0, .LBB162_183
.LBB162_195:
	addi r1, r0, 10
	bne r14, r1, .LBB162_202
.LBB162_196:
	ldw r1, sp+52
	ldw r1, r1+%lo(nml_nval)
	stw fp+64, r1
	stw fp+68, r0
	stw fp+72, r0
.LBB162_197:
	add r12, r15, r0
.LBB162_198:
	add r14, r24, r0
.LBB162_199:
	ori  r1, r14, 2
	bne r1, r24, .LBB162_71
.LBB162_200:
	ldw r1, r21+%lo(nml_tlen)
	bge r11, r1, .LBB162_71
.LBB162_201:
	ldw r1, r22+%lo(nml_status)
	add r27, r11, r0
	beq r1, r0, .LBB162_75
	jal r0, .LBB162_71
.LBB162_202:
	add r11, r27, r0
	jal r0, .LBB162_199
.LBB162_203:
	ldw r1, r22+%lo(nml_status)
	bne r1, r0, .LBB162_205
.LBB162_204:
	addi r1, r0, 2
	stw r22+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.120)
	addi r4, r4, %lo(.L.str.120)
	addi r5, r0, 95
	jal r31, strncpy
	stb r28+%lo(nml_err+95), r0
.LBB162_205:
	add r11, r27, r0
	add r14, r15, r0
	jal r0, .LBB162_199
.LBB162_206:
	bge r4, r1, .LBB162_104
.LBB162_207:
	add r3, r0, r0
.LBB162_208:
	add r4, r5, r3
	ldbu r4, r4+1
	bne r4, r23, .LBB162_210
.LBB162_209:
	addi r3, r3, 1
	add r4, r19, r3
	addi r4, r4, 1
	blt r4, r1, .LBB162_208
	jal r0, .LBB162_104
.LBB162_210:
	add r3, r19, r3
	addi r4, r3, 1
	jal r0, .LBB162_104
.LBB162_211:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.117)
	addi r4, r4, %lo(.L.str.117)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end162:
	.size	f77_nml_begin, .Lfunc_end162-f77_nml_begin
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI162_0,@object
.LJTI162_0:
	.word	.LBB162_144
	.word	.LBB162_151
	.word	.LBB162_143
	.word	.LBB162_151
	.word	.LBB162_151
	.word	.LBB162_151
	.word	.LBB162_151
	.word	.LBB162_143
	.word	.LBB162_154
	.word	.LBB162_155
	.size	.LJTI162_0, 40
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
	bgt r5, r6, .LBB163_2
.LBB163_1:
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
.LBB163_2:
	jalr r0, r31, 0
.Lfunc_end163:
	.size	f77_nml_dim, .Lfunc_end163-f77_nml_dim
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
	add r21, r8, r0
	add r12, r7, r0
	add fp, r6, r0
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
	blt r4, r17, .LBB164_3
.LBB164_1:
	add r13, r16, r0
.LBB164_2:
	ldbu r3, r15+0
	jal r31, toupper
	stb r11+0, r1
	addi r13, r13, -1
	addi r11, r11, 1
	addi r15, r15, 1
	bne r13, r0, .LBB164_2
.LBB164_3:
	addi r1, sp, 248
	add r1, r1, r16
	stb r1+0, r0
	addi r1, r0, 6
	stw sp+108, r1
	beq r14, r1, .LBB164_6
.LBB164_4:
	addi r1, r0, 2
	beq r14, r1, .LBB164_7
.LBB164_5:
	bne r14, r17, .LBB164_8
.LBB164_6:
	addi r1, r0, 0
	sgt r1, r12, r1
	sub r1, r0, r1
	xori r3, r12, 4
	and r1, r3, r1
	xori r28, r1, 4
	jal r0, .LBB164_11
.LBB164_7:
	addi r28, r0, 4
	jal r0, .LBB164_11
.LBB164_8:
	addi r3, r14, -3
	bgeu r3, r1, .LBB164_10
.LBB164_9:
	addi r28, r0, 8
	jal r0, .LBB164_11
.LBB164_10:
	addi r1, r0, 5
	seq r1, r14, r1
	sub r1, r0, r1
	xori r3, r12, 16
	and r1, r3, r1
	xor r28, r12, r1
.LBB164_11:
	lui r1, %hi(nml_reading)
	ldw r1, r1+%lo(nml_reading)
	lui r3, %hi(nml_ndim)
	stw sp+68, r3
	lui r3, %hi(fio_busy)
	stw sp+96, r3
	lui r3, %hi(fio_listed)
	stw sp+100, r3
	lui r3, %hi(fio_ibuf)
	stw sp+104, r3
	lui r3, %hi(fio_desc)
	stw sp+92, r3
	lui r3, %hi(fio_rep)
	stw sp+88, r3
	stw sp+80, r21
	beq r1, r0, .LBB164_112
.LBB164_12:
	stw sp+48, r17
	lui r24, %hi(nml_status)
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB164_169
.LBB164_13:
	lui r1, %hi(nml_nasg)
	stw sp+64, r1
	ldw r1, r1+%lo(nml_nasg)
	ldw r3, sp+48
	blt r1, r3, .LBB164_169
.LBB164_14:
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB164_169
.LBB164_15:
	addi r1, r0, 36
	stw sp+76, r1
	lui r1, %hi(nml_asg)
	stw sp+60, r1
	addi r1, r0, 76
	stw sp+56, r1
	addi r1, r0, 13
	stw sp+52, r1
	lui r1, %hi(nml_lo)
	addi r1, r1, %lo(nml_lo)
	stw sp+28, r1
	addi r16, r0, 2
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+24, r1
	lui r1, %hi(.L.str.37)
	addi r1, r1, %lo(.L.str.37)
	stw sp+40, r1
	addi r1, r0, 95
	stw sp+36, r1
	lui r1, %hi(nml_err+95)
	stw sp+84, r1
	lui r1, %hi(nml_ext)
	addi r1, r1, %lo(nml_ext)
	stw sp+44, r1
	addi r1, r0, 0
	stw sp+32, r1
	add r4, r0, r0
.LBB164_16:
	ldw r1, sp+60
	ldw r13, r1+%lo(nml_asg)
	ldw r1, sp+56
	stw sp+72, r4
	mul r1, r4, r1
	add r18, r13, r1
	addi r4, sp, 248
	add r3, r18, r0
	jal r31, strcmp
	ldw r11, sp+52
	beq r1, r0, .LBB164_20
.LBB164_17:
	addi r1, r0, 11
	ldw r4, sp+72
	beq r11, r1, .LBB164_169
.LBB164_18:
	addi r4, r4, 1
	ldw r1, sp+64
	ldw r1, r1+%lo(nml_nasg)
	bge r4, r1, .LBB164_169
.LBB164_19:
	ldw r1, r24+%lo(nml_status)
	ldw r3, sp+76
	addi r3, r3, 76
	stw sp+76, r3
	beq r1, r0, .LBB164_16
	jal r0, .LBB164_169
.LBB164_20:
	ldw r20, sp+48
	stw r18+72, r20
	ldw r1, r18+32
	blt r1, r20, .LBB164_31
.LBB164_21:
	ldw r3, sp+68
	ldw r3, r3+%lo(nml_ndim)
	bne r1, r3, .LBB164_32
.LBB164_22:
	ldw r1, r18+32
	blt r1, r20, .LBB164_35
.LBB164_23:
	ldw r1, sp+76
	add r11, r13, r1
	add r13, r0, r0
	add r17, r0, r0
	add r19, r20, r0
	add r15, r0, r0
	ldw r22, sp+28
	ldw r23, sp+24
	jal r0, .LBB164_26
.LBB164_24:
	mul r1, r1, r19
	add r15, r1, r15
	mul r19, r3, r19
	add r1, r20, r0
	beq r1, r0, .LBB164_36
.LBB164_25:
	addi r17, r17, 1
	ldw r1, r18+32
	addi r13, r13, 4
	bge r17, r1, .LBB164_36
.LBB164_26:
	add r1, r11, r13
	ldw r1, r1+0
	add r3, r13, r22
	ldw r3, r3+0
	sub r1, r1, r3
	blt r1, r0, .LBB164_28
.LBB164_27:
	ldw r3, sp+44
	add r3, r13, r3
	ldw r3, r3+0
	blt r1, r3, .LBB164_24
.LBB164_28:
	ldw r1, r24+%lo(nml_status)
	beq r1, r0, .LBB164_30
.LBB164_29:
	add r1, r0, r0
	bne r1, r0, .LBB164_25
	jal r0, .LBB164_36
.LBB164_30:
	stw r24+%lo(nml_status), r16
	add r3, r23, r0
	ldw r4, sp+40
	ldw r5, sp+36
	jal r31, strncpy
	ldw r1, sp+84
	stb r1+%lo(nml_err+95), r0
	add r1, r0, r0
	bne r1, r0, .LBB164_25
	jal r0, .LBB164_36
.LBB164_31:
	add r15, r0, r0
	jal r0, .LBB164_37
.LBB164_32:
	ldw r1, r24+%lo(nml_status)
	addi r11, r0, 11
	bne r1, r0, .LBB164_34
.LBB164_33:
	stw r24+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.36)
	addi r4, r4, %lo(.L.str.36)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+84
	stb r1+%lo(nml_err+95), r0
.LBB164_34:
	add r1, r0, r0
	add r15, r0, r0
	beq r1, r0, .LBB164_17
	jal r0, .LBB164_37
.LBB164_35:
	add r15, r0, r0
.LBB164_36:
	ldw r1, r24+%lo(nml_status)
	ldw r3, sp+32
	seq r1, r1, r3
	addi r3, r1, -1
	andi r11, r3, 11
	beq r1, r0, .LBB164_17
.LBB164_37:
	ldw r1, r18+68
	addi r23, r0, 1
	blt r1, r23, .LBB164_111
.LBB164_38:
	ldw r1, r24+%lo(nml_status)
	add r11, r0, r0
	bne r1, r0, .LBB164_17
.LBB164_39:
	ldw r22, r18+64
.LBB164_40:
	lui r17, %hi(nml_val)
	ldw r1, r17+%lo(nml_val)
	addi r3, r0, 12
	mul r26, r22, r3
	add r3, r1, r26
	ldw r1, r3+8
	blt r1, r23, .LBB164_109
.LBB164_41:
	bge r15, r21, .LBB164_106
.LBB164_42:
	xor r1, r15, r21
	sgt r4, r15, r21
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r21, r1
	sub r27, r1, r15
	mul r1, r28, r15
	add r11, fp, r1
	add r25, r0, r0
.LBB164_43:
	ldw r1, r3+4
	blt r1, r23, .LBB164_101
.LBB164_44:
	mul r4, r15, r28
	add r19, fp, r4
	lui r4, %hi(nml_text)
	ldw r4, r4+%lo(nml_text)
	ldw r3, r3+0
	add r4, r4, r3
	addi r3, r0, 7
	beq r14, r3, .LBB164_55
.LBB164_45:
	ldw r3, sp+108
	bne r14, r3, .LBB164_67
.LBB164_46:
	ldbu r3, r4+0
	addi r5, r0, 46
	seq r3, r3, r5
	ble r1, r3, .LBB164_49
.LBB164_47:
	add r13, r4, r3
	ldbu r3, r13+0
	jal r31, toupper
	addi r4, r0, 84
	add r3, r23, r0
	beq r1, r4, .LBB164_52
.LBB164_48:
	ldbu r3, r13+0
	jal r31, toupper
	addi r3, r0, 70
	beq r1, r3, .LBB164_51
.LBB164_49:
	ldw r1, r24+%lo(nml_status)
	add r3, r0, r0
	bne r1, r0, .LBB164_52
.LBB164_50:
	stw r24+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.124)
	addi r4, r4, %lo(.L.str.124)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+84
	stb r1+%lo(nml_err+95), r0
.LBB164_51:
	add r3, r0, r0
.LBB164_52:
	beq r12, r16, .LBB164_80
.LBB164_53:
	bne r12, r23, .LBB164_81
.LBB164_54:
	stb r19+0, r3
	jal r0, .LBB164_101
.LBB164_55:
	ldbu r3, r4+0
	addi r5, r0, 34
	beq r3, r5, .LBB164_57
.LBB164_56:
	addi r5, r0, 39
	bne r3, r5, .LBB164_82
.LBB164_57:
	add r5, r0, r0
	addi r6, r0, 1
	bne r1, r6, .LBB164_62
.LBB164_58:
	ble r12, r5, .LBB164_101
.LBB164_59:
	add r1, r11, r5
	sub r3, r12, r5
.LBB164_60:
	addi r4, r0, 32
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB164_60
	jal r0, .LBB164_101
.LBB164_61:
	addi r6, r6, 1
	bge r6, r1, .LBB164_58
.LBB164_62:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB164_65
.LBB164_63:
	addi r6, r6, 1
	bge r6, r1, .LBB164_58
.LBB164_64:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB164_58
.LBB164_65:
	bge r5, r12, .LBB164_61
.LBB164_66:
	add r7, r4, r6
	ldbu r7, r7+0
	addi r8, r5, 1
	add r5, r19, r5
	stb r5+0, r7
	add r5, r8, r0
	jal r0, .LBB164_61
.LBB164_67:
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
	ldw r21, sp+96
	stb r1+0, r0
	ldbu r1, r21+%lo(fio_busy)
	beq r1, r23, .LBB164_170
.LBB164_68:
	stb r21+%lo(fio_busy), r23
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB164_72
.LBB164_69:
	addi r3, r0, -400
.LBB164_70:
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	add r4, r3, r4
	addi r5, r0, -1
	stw r4+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB164_70
.LBB164_71:
	stb r1+%lo(fio_uend_init), r23
.LBB164_72:
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
	lui r13, %hi(fio_reading)
	stb r13+%lo(fio_reading), r23
	lui r20, %hi(fio_ldone)
	stb r20+%lo(fio_ldone), r0
	lui r1, %hi(fio_l_pending)
	stw r1+%lo(fio_l_pending), r0
	lui r1, %hi(fio_gdepth)
	stw r1+%lo(fio_gdepth), r0
	lui r1, %hi(fio_scale)
	stw r1+%lo(fio_scale), r0
	ldw r1, sp+88
	stw r1+%lo(fio_rep), r0
	ldw r1, sp+92
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
	beq r1, r0, .LBB164_75
.LBB164_73:
	stb r13+%lo(fio_reading), r0
	stb r21+%lo(fio_busy), r0
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB164_101
.LBB164_74:
	stw r24+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.126)
	addi r4, r4, %lo(.L.str.126)
	jal r0, .LBB164_84
.LBB164_75:
	addi r1, r14, -1
	addi r3, r0, 3
	bgtu r1, r3, .LBB164_92
.LBB164_76:
	slli r1, r1, 2
	lui r3, %hi(.LJTI164_0)
	addi r3, r3, %lo(.LJTI164_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB164_77:
	stw sp+184, r0
	addi r3, sp, 184
	jal r31, f77_rd_i
	ldw r1, sp+184
	beq r12, r16, .LBB164_103
.LBB164_78:
	bne r12, r23, .LBB164_104
.LBB164_79:
	stb r19+0, r1
	jal r0, .LBB164_100
.LBB164_80:
	sth r19+0, r3
	jal r0, .LBB164_101
.LBB164_81:
	stw r19+0, r3
	jal r0, .LBB164_101
.LBB164_82:
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB164_101
.LBB164_83:
	stw r24+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.125)
	addi r4, r4, %lo(.L.str.125)
.LBB164_84:
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+84
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB164_101
.LBB164_85:
	add r3, r19, r0
	jal r0, .LBB164_99
.LBB164_86:
	ldw r1, sp+100
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB164_97
.LBB164_87:
	ldbu r1, r20+%lo(fio_ldone)
	bne r1, r0, .LBB164_100
.LBB164_88:
	jal r31, fio_list_tok
	beq r1, r16, .LBB164_100
.LBB164_89:
	beq r1, r23, .LBB164_96
.LBB164_90:
	addi r3, sp, 184
	addi r4, sp, 144
	jal r31, fio_list_cplx_in
	ldw r5, sp+188
	ldw r4, sp+184
	fcvt.s.d r1, r4
	stw r19+0, r1
	ldw r5, sp+148
	ldw r4, sp+144
	fcvt.s.d r1, r4
	stw r19+4, r1
	jal r0, .LBB164_100
.LBB164_91:
	add r3, r19, r0
	jal r31, f77_rd_r
	jal r0, .LBB164_100
.LBB164_92:
	ldw r1, sp+100
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB164_98
.LBB164_93:
	ldbu r1, r20+%lo(fio_ldone)
	bne r1, r0, .LBB164_100
.LBB164_94:
	jal r31, fio_list_tok
	beq r1, r16, .LBB164_100
.LBB164_95:
	bne r1, r23, .LBB164_105
.LBB164_96:
	stb r20+%lo(fio_ldone), r23
	jal r0, .LBB164_100
.LBB164_97:
	add r3, r19, r0
	jal r31, f77_rd_r
	addi r3, r19, 4
	jal r31, f77_rd_r
	jal r0, .LBB164_100
.LBB164_98:
	add r3, r19, r0
	jal r31, f77_rd_d
	addi r3, r19, 8
.LBB164_99:
	jal r31, f77_rd_d
.LBB164_100:
	stb r21+%lo(fio_busy), r0
	stb r13+%lo(fio_reading), r0
	ldw r1, sp+104
	stw r1+%lo(fio_ibuf), r0
.LBB164_101:
	addi r15, r15, 1
	addi r25, r25, 1
	ldw r1, r17+%lo(nml_val)
	add r3, r1, r26
	ldw r1, r3+8
	bge r25, r1, .LBB164_108
.LBB164_102:
	add r11, r11, r28
	bne r25, r27, .LBB164_43
	jal r0, .LBB164_106
.LBB164_103:
	sth r19+0, r1
	jal r0, .LBB164_100
.LBB164_104:
	stw r19+0, r1
	jal r0, .LBB164_100
.LBB164_105:
	addi r3, sp, 184
	addi r4, sp, 144
	jal r31, fio_list_cplx_in
	ldw r1, sp+188
	ldw r3, sp+184
	stw r19+4, r1
	stw r19+0, r3
	ldw r1, sp+148
	ldw r3, sp+144
	stw r19+12, r1
	stw r19+8, r3
	jal r0, .LBB164_100
.LBB164_106:
	ldw r1, r24+%lo(nml_status)
	bne r1, r0, .LBB164_108
.LBB164_107:
	stw r24+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+84
	stb r1+%lo(nml_err+95), r0
.LBB164_108:
	ldw r21, sp+80
.LBB164_109:
	addi r22, r22, 1
	ldw r1, r18+64
	ldw r3, r18+68
	add r1, r3, r1
	bge r22, r1, .LBB164_111
.LBB164_110:
	ldw r1, r24+%lo(nml_status)
	add r11, r0, r0
	beq r1, r0, .LBB164_40
	jal r0, .LBB164_17
.LBB164_111:
	add r11, r0, r0
	jal r0, .LBB164_17
.LBB164_112:
	addi r15, sp, 144
	addi r4, sp, 248
	add r3, r15, r0
	add r5, r16, r0
	jal r31, memcpy
	add r1, r15, r16
	addi r3, r0, 61
	stb r1+0, r3
	lui r11, %hi(nml_npend)
	ldw r1, r11+%lo(nml_npend)
	lui r15, %hi(nml_pend)
	addi r15, r15, %lo(nml_pend)
	blt r1, r0, .LBB164_114
.LBB164_113:
	addi r4, r1, 1
	stw r11+%lo(nml_npend), r4
	add r1, r1, r15
	addi r3, r0, 44
	stb r1+0, r3
	add r3, r15, r0
	jal r31, nml_put
	addi r1, r0, -1
	stw r11+%lo(nml_npend), r1
.LBB164_114:
	addi r4, r16, 1
	addi r3, sp, 144
	jal r31, nml_put
	addi r16, r0, 1
	blt r21, r16, .LBB164_169
.LBB164_115:
	addi r24, r0, 5
	seq r3, r14, r24
	addi r1, r0, -2
	and r22, r14, r1
	stw sp+56, r3
	sub r1, r0, r3
	andi r1, r1, 16
	addi r1, r1, 33
	stw sp+48, r1
	addi r27, r0, 3
	addi r25, r0, 23
	addi r26, r0, 44
	addi r19, r0, -1
	addi r17, r0, 2
	addi r1, r0, 15
	stw sp+72, r1
	lui r1, %hi(.L.str.33)
	addi r1, r1, %lo(.L.str.33)
	stw sp+64, r1
	addi r1, r0, 11
	stw sp+84, r1
	add r13, fp, r0
	add r18, r0, r0
	stw sp+60, r24
	stw sp+52, r22
	stw sp+44, r25
	stw sp+76, r26
	jal r0, .LBB164_119
.LBB164_116:
	addi r4, sp, 184
	add r3, r15, r0
	ldw r20, sp+72
.LBB164_117:
	add r5, r20, r0
	jal r31, memcpy
	stw r11+%lo(nml_npend), r20
.LBB164_118:
	addi r18, r18, 1
	add r13, r13, r28
	beq r18, r21, .LBB164_169
.LBB164_119:
	mul r1, r18, r28
	add r1, fp, r1
	beq r14, r27, .LBB164_127
.LBB164_120:
	beq r14, r17, .LBB164_125
.LBB164_121:
	bne r14, r16, .LBB164_130
.LBB164_122:
	beq r12, r17, .LBB164_140
.LBB164_123:
	addi r3, r0, 1
	bne r12, r3, .LBB164_141
.LBB164_124:
	ldbu r23, r1+0
	jal r0, .LBB164_142
.LBB164_125:
	ldw r1, r1+0
	fcvt.d.s r6, r1
	addi r3, sp, 184
	add r5, r6, r0
	add r6, r7, r0
	add r4, r0, r0
	jal r31, nml_real
	ldw r1, r11+%lo(nml_npend)
	blt r1, r0, .LBB164_116
.LBB164_126:
	addi r4, r1, 1
	stw r11+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	stw r11+%lo(nml_npend), r19
	jal r0, .LBB164_116
.LBB164_127:
	ldw r6, r1+4
	ldw r5, r1+0
	addi r3, sp, 184
	add r4, r16, r0
	jal r31, nml_real
	ldw r1, r11+%lo(nml_npend)
	blt r1, r0, .LBB164_129
.LBB164_128:
	addi r4, r1, 1
	stw r11+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	stw r11+%lo(nml_npend), r19
.LBB164_129:
	addi r4, sp, 184
	add r3, r15, r0
	add r5, r25, r0
	jal r31, memcpy
	stw r11+%lo(nml_npend), r25
	jal r0, .LBB164_118
.LBB164_130:
	addi r3, r0, 4
	bne r22, r3, .LBB164_134
.LBB164_131:
	bne r14, r24, .LBB164_149
.LBB164_132:
	ldw r7, r1+4
	ldw r6, r1+0
	beq r14, r24, .LBB164_150
.LBB164_133:
	ldw r1, r1+4
	fcvt.d.s r20, r1
	jal r0, .LBB164_151
.LBB164_134:
	ldw r3, sp+108
	bne r14, r3, .LBB164_156
.LBB164_135:
	beq r12, r17, .LBB164_167
.LBB164_136:
	addi r3, r0, 1
	bne r12, r3, .LBB164_168
.LBB164_137:
	ldbu r20, r1+0
	ldw r1, r11+%lo(nml_npend)
	blt r1, r0, .LBB164_139
.LBB164_138:
	addi r4, r1, 1
	stw r11+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	stw r11+%lo(nml_npend), r19
.LBB164_139:
	addi r1, r0, 0
	seq r1, r20, r1
	lui r3, %hi(.L.str.34)
	addi r3, r3, %lo(.L.str.34)
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r4, r3, r1
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r23, r0, 1
	jal r0, .LBB164_166
.LBB164_140:
	ldh r23, r1+0
	jal r0, .LBB164_142
.LBB164_141:
	ldw r23, r1+0
.LBB164_142:
	addi r3, sp, 184
	ldw r4, sp+84
	add r5, r16, r0
	ldw r6, sp+64
	add r7, r19, r0
	jal r31, f77_wr_begin_int
	add r3, r23, r0
	jal r31, f77_wr_i
	ldw r1, sp+96
	stb r1+%lo(fio_busy), r0
	ldw r1, sp+100
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB164_146
.LBB164_143:
	ldw r1, sp+92
	ldw r1, r1+%lo(fio_desc)
	beq r1, r0, .LBB164_145
.LBB164_144:
	ldw r1, sp+88
	ldw r1, r1+%lo(fio_rep)
	bgt r1, r0, .LBB164_146
.LBB164_145:
	lui r20, %hi(fio_draining)
	addi r1, r0, 1
	stb r20+%lo(fio_draining), r1
	ldw r1, sp+92
	stw r1+%lo(fio_desc), r0
	ldw r1, sp+88
	stw r1+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r20+%lo(fio_draining), r0
.LBB164_146:
	jal r31, fio_flush_line
	ldw r1, sp+104
	stw r1+%lo(fio_ibuf), r0
	stb sp+195, r0
	ldw r1, r11+%lo(nml_npend)
	blt r1, r0, .LBB164_148
.LBB164_147:
	addi r4, r1, 1
	stw r11+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	stw r11+%lo(nml_npend), r19
.LBB164_148:
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r4, sp, 184
	ldw r20, sp+84
	jal r0, .LBB164_117
.LBB164_149:
	ldw r3, r1+0
	fcvt.d.s r6, r3
	bne r14, r24, .LBB164_133
.LBB164_150:
	ldw r21, r1+12
	ldw r20, r1+8
.LBB164_151:
	addi r1, r0, 40
	stb sp+280, r1
	addi r23, sp, 112
	add r3, r23, r0
	add r5, r6, r0
	add r6, r7, r0
	ldw r22, sp+56
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
	add r5, r20, r0
	add r6, r21, r0
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
	ldw r20, sp+48
	sub r24, r20, r23
	blt r24, r25, .LBB164_153
.LBB164_152:
	addi r25, sp, 280
	add r3, r25, r24
	add r4, r25, r0
	add r5, r23, r0
	jal r31, memmove
	addi r4, r0, 32
	add r3, r25, r0
	add r5, r24, r0
	jal r31, memset
	add r23, r20, r0
.LBB164_153:
	ldw r21, sp+80
	ldw r22, sp+52
	ldw r25, sp+44
	ldw r1, r11+%lo(nml_npend)
	ldw r24, sp+60
	ldw r26, sp+76
	blt r1, r0, .LBB164_155
.LBB164_154:
	addi r4, r1, 1
	stw r11+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	stw r11+%lo(nml_npend), r19
.LBB164_155:
	addi r1, r0, 1022
	slt r1, r23, r1
	sub r1, r0, r1
	xori r3, r23, 1022
	and r1, r3, r1
	xori r23, r1, 1022
	jal r0, .LBB164_165
.LBB164_156:
	addi r1, r0, 39
	stb sp+280, r1
	add r23, r16, r0
	blt r12, r16, .LBB164_162
.LBB164_157:
	addi r23, r0, 1
	add r3, r0, r0
	jal r0, .LBB164_160
.LBB164_158:
	add r23, r4, r0
	addi r3, r3, 1
	bge r3, r12, .LBB164_162
.LBB164_159:
	addi r4, r0, 1021
	bge r23, r4, .LBB164_162
.LBB164_160:
	add r4, r13, r3
	ldbu r5, r4+0
	addi r4, r23, 1
	addi r6, sp, 280
	add r6, r6, r23
	stb r6+0, r5
	bne r5, r1, .LBB164_158
.LBB164_161:
	addi r23, r23, 2
	addi r5, sp, 280
	add r4, r5, r4
	stb r4+0, r1
	addi r3, r3, 1
	blt r3, r12, .LBB164_159
.LBB164_162:
	addi r3, sp, 280
	add r3, r3, r23
	stb r3+0, r1
	ldw r1, r11+%lo(nml_npend)
	blt r1, r0, .LBB164_164
.LBB164_163:
	addi r4, r1, 1
	stw r11+%lo(nml_npend), r4
	add r1, r1, r15
	stb r1+0, r26
	add r3, r15, r0
	jal r31, nml_put
	stw r11+%lo(nml_npend), r19
.LBB164_164:
	addi r1, r0, 1021
	slt r1, r23, r1
	sub r1, r0, r1
	xori r3, r23, 1021
	and r1, r3, r1
	xori r1, r1, 1021
	addi r23, r1, 1
.LBB164_165:
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	addi r4, sp, 280
.LBB164_166:
	add r5, r23, r0
	jal r31, memcpy
	stw r11+%lo(nml_npend), r23
	jal r0, .LBB164_118
.LBB164_167:
	ldh r20, r1+0
	ldw r1, r11+%lo(nml_npend)
	bge r1, r0, .LBB164_138
	jal r0, .LBB164_139
.LBB164_168:
	ldw r20, r1+0
	ldw r1, r11+%lo(nml_npend)
	bge r1, r0, .LBB164_138
	jal r0, .LBB164_139
.LBB164_169:
	ldw r1, sp+68
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
.LBB164_170:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.76)
	addi r4, r4, %lo(.L.str.76)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end164:
	.size	f77_nml_item, .Lfunc_end164-f77_nml_item
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI164_0,@object
.LJTI164_0:
	.word	.LBB164_77
	.word	.LBB164_91
	.word	.LBB164_85
	.word	.LBB164_86
	.size	.LJTI164_0, 16
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
	blt r13, r1, .LBB165_7
.LBB165_1:
	add r1, r13, r4
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB165_7
.LBB165_2:
	add r16, r4, r0
	lui r1, %hi(nml_unit)
	ldw r3, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.114)
	addi r4, r4, %lo(.L.str.114)
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
	bne r1, r0, .LBB165_6
.LBB165_3:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB165_5
.LBB165_4:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB165_6
.LBB165_5:
	lui r13, %hi(fio_draining)
	addi r4, r0, 1
	stb r13+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r13+%lo(fio_draining), r0
.LBB165_6:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	addi r1, r0, 32
	stb r12+0, r1
	addi r1, r0, 1
	stw r14+%lo(nml_len), r1
	add r4, r16, r0
.LBB165_7:
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
.Lfunc_end165:
	.size	nml_put, .Lfunc_end165-nml_put
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function nml_real
	.type	.LCPI166_0,@object
.LCPI166_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI166_0, 8
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
	lui r1, %hi(.LCPI166_0)
	addi r1, r1, %lo(.LCPI166_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r14, r6
	xori r1, r1, 1
	bne r1, r0, .LBB166_4
.LBB166_1:
	addi r4, r0, 32
	add r3, r11, r0
	add r5, r12, r0
	jal r31, memset
	addi r3, r11, 1
	lui r4, %hi(.L.str.121)
	addi r4, r4, %lo(.L.str.121)
	addi r5, r0, 3
	jal r31, memcpy
.LBB166_2:
	add r1, r11, r12
	stb r1+0, r0
.LBB166_3:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB166_4:
	beq r4, r0, .LBB166_13
.LBB166_5:
	lui r6, %hi(.L.str.122)
	addi r6, r6, %lo(.L.str.122)
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
	bne r1, r0, .LBB166_9
.LBB166_6:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB166_8
.LBB166_7:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB166_9
.LBB166_8:
	lui r14, %hi(fio_draining)
	stb r14+%lo(fio_draining), r13
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r14+%lo(fio_draining), r0
.LBB166_9:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	add r1, r11, r12
	stb r1+0, r0
	addi r1, r0, 69
	addi r3, r0, 68
	jal r0, .LBB166_11
.LBB166_10:
	addi r12, r12, -1
	addi r11, r11, 1
	beq r12, r0, .LBB166_3
.LBB166_11:
	ldbu r4, r11+0
	bne r4, r1, .LBB166_10
.LBB166_12:
	stb r11+0, r3
	jal r0, .LBB166_10
.LBB166_13:
	fcvt.s.d r14, r14
	lui r6, %hi(.L.str.123)
	addi r6, r6, %lo(.L.str.123)
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
	bne r1, r0, .LBB166_17
.LBB166_14:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB166_16
.LBB166_15:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB166_17
.LBB166_16:
	lui r14, %hi(fio_draining)
	stb r14+%lo(fio_draining), r13
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r14+%lo(fio_draining), r0
.LBB166_17:
	jal r31, fio_flush_line
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	jal r0, .LBB166_2
.Lfunc_end166:
	.size	nml_real, .Lfunc_end166-nml_real
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
	blt r13, r1, .LBB167_4
.LBB167_1:
	add r1, r14, r16
	ldbu r1, r1+0
	bne r1, r17, .LBB167_4
.LBB167_2:
	addi r16, r16, 1
	bne r13, r16, .LBB167_1
.LBB167_3:
	add r16, r13, r0
.LBB167_4:
	sub r1, r13, r16
	addi r3, r0, 3
	add r15, r14, r16
	blt r1, r3, .LBB167_6
.LBB167_5:
	lui r4, %hi(.L.str.121)
	addi r4, r4, %lo(.L.str.121)
	addi r5, r0, 3
	add r3, r15, r0
	jal r31, memcmp
	beq r1, r0, .LBB167_11
.LBB167_6:
	ble r13, r16, .LBB167_12
.LBB167_7:
	addi r1, r13, -1
	xor r3, r16, r1
	slt r4, r16, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	addi r3, r14, -1
.LBB167_8:
	add r4, r3, r13
	ldbu r4, r4+0
	bne r4, r17, .LBB167_12
.LBB167_9:
	addi r13, r13, -1
	bgt r13, r16, .LBB167_8
.LBB167_10:
	add r13, r1, r0
	jal r0, .LBB167_12
.LBB167_11:
	addi r1, r16, 3
	bne r1, r13, .LBB167_13
.LBB167_12:
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
.LBB167_13:
	add r1, r14, r1
	ldbu r1, r1+0
	bne r1, r17, .LBB167_6
	jal r0, .LBB167_12
.Lfunc_end167:
	.size	nml_trim_cat, .Lfunc_end167-nml_trim_cat
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
	beq r1, r0, .LBB168_6
.LBB168_1:
	lui r11, %hi(nml_status)
	ldw r1, r11+%lo(nml_status)
	beq r1, r0, .LBB168_19
.LBB168_2:
	ldw r1, r11+%lo(nml_status)
	addi r3, r0, 2
	beq r1, r3, .LBB168_23
.LBB168_3:
	addi r3, r0, 1
	bne r1, r3, .LBB168_24
.LBB168_4:
	lui r1, %hi(nml_flags)
	ldbu r1, r1+%lo(nml_flags)
	andi r4, r1, 1
	add r1, r3, r0
	bne r4, r0, .LBB168_24
.LBB168_5:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r1, %hi(nml_unit)
	ldw r5, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	lui r6, %hi(nml_grp)
	addi r6, r6, %lo(nml_grp)
	jal r0, .LBB168_28
.LBB168_6:
	lui r11, %hi(nml_npend)
	ldw r4, r11+%lo(nml_npend)
	blt r4, r0, .LBB168_8
.LBB168_7:
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	jal r31, nml_put
	addi r1, r0, -1
	stw r11+%lo(nml_npend), r1
.LBB168_8:
	lui r15, %hi(nml_len)
	ldw r11, r15+%lo(nml_len)
	addi r1, r0, 2
	lui r13, %hi(fio_busy)
	lui r14, %hi(fio_listed)
	lui r12, %hi(fio_ibuf)
	blt r11, r1, .LBB168_14
.LBB168_9:
	lui r1, %hi(nml_unit)
	ldw r3, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.114)
	addi r4, r4, %lo(.L.str.114)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	lui r3, %hi(nml_line)
	addi r3, r3, %lo(nml_line)
	add r4, r11, r0
	jal r31, f77_wr_a
	stb r13+%lo(fio_busy), r0
	ldw r1, r14+%lo(fio_listed)
	bne r1, r0, .LBB168_13
.LBB168_10:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB168_12
.LBB168_11:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB168_13
.LBB168_12:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB168_13:
	jal r31, fio_flush_line
	stw r12+%lo(fio_ibuf), r0
.LBB168_14:
	lui r1, %hi(nml_line)
	addi r3, r0, 32
	stb r1+%lo(nml_line), r3
	addi r11, r0, 1
	stw r15+%lo(nml_len), r11
	lui r1, %hi(nml_unit)
	ldw r3, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.114)
	addi r4, r4, %lo(.L.str.114)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	lui r3, %hi(.L.str.39)
	addi r3, r3, %lo(.L.str.39)
	addi r4, r0, 5
	jal r31, f77_wr_a
	stb r13+%lo(fio_busy), r0
	ldw r1, r14+%lo(fio_listed)
	bne r1, r0, .LBB168_18
.LBB168_15:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB168_17
.LBB168_16:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB168_18
.LBB168_17:
	lui r13, %hi(fio_draining)
	stb r13+%lo(fio_draining), r11
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r13+%lo(fio_draining), r0
.LBB168_18:
	jal r31, fio_flush_line
	stw r12+%lo(fio_ibuf), r0
	add r1, r0, r0
	jal r0, .LBB168_24
.LBB168_19:
	lui r1, %hi(nml_nasg)
	ldw r1, r1+%lo(nml_nasg)
	addi r3, r0, 1
	blt r1, r3, .LBB168_2
.LBB168_20:
	lui r3, %hi(nml_asg)
	ldw r5, r3+%lo(nml_asg)
.LBB168_21:
	ldw r3, r5+72
	beq r3, r0, .LBB168_25
.LBB168_22:
	addi r5, r5, 76
	addi r1, r1, -1
	bne r1, r0, .LBB168_21
	jal r0, .LBB168_2
.LBB168_23:
	lui r3, %hi(nml_flags)
	ldbu r3, r3+%lo(nml_flags)
	andi r3, r3, 2
	beq r3, r0, .LBB168_27
.LBB168_24:
	ldw r15, sp+124
	ldw r14, sp+128
	ldw r13, sp+132
	ldw r12, sp+136
	ldw r11, sp+140
	ldw lr, sp+0
	addi sp, sp, 144
	jalr r0, r31, 0
.LBB168_25:
	lui r4, %hi(.L.str.40)
	addi r4, r4, %lo(.L.str.40)
	addi r3, sp, 28
	jal r31, sprintf
	ldw r1, r11+%lo(nml_status)
	bne r1, r0, .LBB168_2
.LBB168_26:
	addi r1, r0, 2
	stw r11+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	addi r4, sp, 28
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB168_2
.LBB168_27:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r1, %hi(nml_unit)
	ldw r6, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	lui r5, %hi(nml_grp)
	addi r5, r5, %lo(nml_grp)
	lui r7, %hi(nml_err)
	addi r7, r7, %lo(nml_err)
.LBB168_28:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end168:
	.size	f77_nml_end, .Lfunc_end168-f77_nml_end
                                        # -- End function
	.globl	f77_ipow                        # -- Begin function f77_ipow
	.p2align	2
	.type	f77_ipow,@function
f77_ipow:                               # @f77_ipow
# %bb.0:
	addi r1, r0, 1
	blt r4, r0, .LBB169_4
.LBB169_1:
	beq r4, r0, .LBB169_9
.LBB169_2:
	addi r5, r0, 0
.LBB169_3:
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
	bne r4, r0, .LBB169_3
	jal r0, .LBB169_9
.LBB169_4:
	beq r3, r1, .LBB169_7
.LBB169_5:
	addi r1, r0, -1
	bne r3, r1, .LBB169_8
.LBB169_6:
	andi r1, r4, 1
	addi r3, r0, 0
	seq r1, r1, r3
	addi r1, r1, -1
	ori  r1, r1, 1
	jal r0, .LBB169_9
.LBB169_7:
	add r1, r3, r0
	jal r0, .LBB169_9
.LBB169_8:
	add r1, r0, r0
.LBB169_9:
	jalr r0, r31, 0
.Lfunc_end169:
	.size	f77_ipow, .Lfunc_end169-f77_ipow
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_dpow_i
	.type	.LCPI170_0,@object
.LCPI170_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI170_0, 8
	.type	.LCPI170_1,@object
.LCPI170_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI170_1, 8
	.type	.LCPI170_2,@object
.LCPI170_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI170_2, 8
	.type	.LCPI170_3,@object
.LCPI170_3:
	.quad	0x7fefdcf158adbb99              # double 1.79E+308
	.size	.LCPI170_3, 8
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
	blt r5, r0, .LBB170_3
.LBB170_1:
	andi r3, r5, 1
	addi r1, r0, 0
	seq r3, r3, r1
	lui r4, %hi(.LCPI170_0)
	addi r4, r4, %lo(.LCPI170_0)
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
	beq r3, r0, .LBB170_16
.LBB170_2:
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
	bne r3, r0, .LBB170_2
	jal r0, .LBB170_16
.LBB170_3:
	sub r1, r0, r5
	lui r3, %hi(.LCPI170_0)
	addi r3, r3, %lo(.LCPI170_0)
	ldw r5, r3+4
	ldw r4, r3+0
	lui r3, %hi(.LCPI170_1)
	addi r3, r3, %lo(.LCPI170_1)
	ldw r9, r3+4
	ldw r8, r3+0
	lui r3, %hi(.LCPI170_3)
	addi r3, r3, %lo(.LCPI170_3)
	ldw r11, r3+4
	ldw r10, r3+0
	lui r3, %hi(.LCPI170_2)
	addi r3, r3, %lo(.LCPI170_2)
	ldw r15, r3+4
	ldw r14, r3+0
	add r16, r6, r0
	add r17, r7, r0
	add r3, r1, r0
	add r12, r4, r0
	add r13, r5, r0
	jal r0, .LBB170_5
.LBB170_4:
	fmul.d r16, r16, r16
.LBB170_5:
	andi lr, r3, 1
	fneg.d r18, r16
	beq lr, r0, .LBB170_10
.LBB170_6:
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
	bne lr, r0, .LBB170_9
.LBB170_7:
	fle.d lr, r20, r4
	bne lr, r0, .LBB170_9
.LBB170_8:
	fle.d lr, r20, r14
	xori lr, lr, 1
	fle.d r20, r22, r14
	xori r20, r20, 1
	or  lr, r20, lr
	bne lr, r0, .LBB170_13
.LBB170_9:
	fmul.d r12, r16, r12
.LBB170_10:
	srli r3, r3, 1
	beq r3, r0, .LBB170_15
.LBB170_11:
	flt.d lr, r16, r8
	xor r20, r19, r17
	sub lr, r0, lr
	and r20, r20, lr
	xor r21, r17, r20
	xor r18, r18, r16
	and lr, r18, lr
	xor r20, r16, lr
	fle.d lr, r20, r14
	bne lr, r0, .LBB170_4
.LBB170_12:
	fdiv.d r18, r10, r20
	fle.d lr, r20, r18
	xori lr, lr, 1
	beq lr, r0, .LBB170_4
.LBB170_13:
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
	beq r1, r0, .LBB170_16
.LBB170_14:
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
	bne r1, r0, .LBB170_14
	jal r0, .LBB170_16
.LBB170_15:
	fdiv.d r8, r4, r12
.LBB170_16:
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
.Lfunc_end170:
	.size	f77_dpow_i, .Lfunc_end170-f77_dpow_i
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rpow_i
	.type	.LCPI171_0,@object
.LCPI171_0:
	.word	0x3f800000                      # float 1
	.size	.LCPI171_0, 4
	.type	.LCPI171_1,@object
.LCPI171_1:
	.word	0x00000000                      # float 0
	.size	.LCPI171_1, 4
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0
	.type	.LCPI171_2,@object
.LCPI171_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI171_2, 8
	.type	.LCPI171_3,@object
.LCPI171_3:
	.quad	0x47eff933c78cdfad              # double 3.4E+38
	.size	.LCPI171_3, 8
	.type	.LCPI171_4,@object
.LCPI171_4:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI171_4, 8
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
	blt r4, r0, .LBB171_3
.LBB171_1:
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
	srli r4, r4, 1
	beq r4, r0, .LBB171_17
.LBB171_2:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	srli r4, r4, 1
	bne r4, r0, .LBB171_2
	jal r0, .LBB171_17
.LBB171_3:
	sub r4, r0, r4
	lui r1, %hi(.LCPI171_0)
	ldw r5, r1+%lo(.LCPI171_0)
	lui r6, %hi(.LCPI171_1)
	ldw r6, r6+%lo(.LCPI171_1)
	lui r7, %hi(.LCPI171_3)
	addi r7, r7, %lo(.LCPI171_3)
	ldw r9, r7+4
	ldw r8, r7+0
	lui r7, %hi(.LCPI171_2)
	addi r7, r7, %lo(.LCPI171_2)
	ldw r11, r7+4
	ldw r10, r7+0
	add r7, r3, r0
	add lr, r4, r0
	jal r0, .LBB171_5
.LBB171_4:
	fmul.s r7, r7, r7
.LBB171_5:
	andi r15, lr, 1
	fcvt.d.s r12, r7
	flt.s r14, r7, r6
	beq r15, r0, .LBB171_10
.LBB171_6:
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
	bne r15, r0, .LBB171_9
.LBB171_7:
	lui r15, %hi(.LCPI171_4)
	addi r15, r15, %lo(.LCPI171_4)
	ldw r21, r15+4
	ldw r20, r15+0
	fle.d r15, r16, r20
	bne r15, r0, .LBB171_9
.LBB171_8:
	fle.d r15, r16, r10
	xori r15, r15, 1
	fle.d r16, r18, r10
	xori r16, r16, 1
	or  r15, r16, r15
	bne r15, r0, .LBB171_13
.LBB171_9:
	fmul.s r5, r7, r5
.LBB171_10:
	srli lr, lr, 1
	beq lr, r0, .LBB171_16
.LBB171_11:
	fneg.d r16, r12
	xor r15, r17, r13
	sub r18, r0, r14
	and r14, r15, r18
	xor r15, r13, r14
	xor r16, r16, r12
	and r16, r16, r18
	xor r14, r12, r16
	fle.d r12, r14, r10
	bne r12, r0, .LBB171_4
.LBB171_12:
	fdiv.d r12, r8, r14
	fle.d r12, r14, r12
	xori r12, r12, 1
	beq r12, r0, .LBB171_4
.LBB171_13:
	ldw r1, r1+%lo(.LCPI171_0)
	fdiv.s r3, r1, r3
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
.LBB171_14:
	srli r4, r4, 1
	beq r4, r0, .LBB171_17
.LBB171_15:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	jal r0, .LBB171_14
.LBB171_16:
	ldw r1, r1+%lo(.LCPI171_0)
	fdiv.s r1, r1, r5
.LBB171_17:
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
.Lfunc_end171:
	.size	f77_rpow_i, .Lfunc_end171-f77_rpow_i
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
	bne r3, r0, .LBB172_4
.LBB172_1:
	addi r3, r0, -400
	lui lr, %hi(fio_uend+400)
	addi lr, lr, %lo(fio_uend+400)
	addi r12, r0, -1
.LBB172_2:
	add r13, r3, lr
	stw r13+0, r12
	addi r3, r3, 4
	bne r3, r0, .LBB172_2
.LBB172_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB172_4:
	ldw r14, sp+352
	addi r1, r11, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB172_21
.LBB172_5:
	addi r1, r11, -5
	addi r18, r0, 1
	bleu r1, r18, .LBB172_21
.LBB172_6:
	beq r8, r0, .LBB172_13
.LBB172_7:
	addi r1, r0, 1
	add r15, r0, r0
	blt r9, r1, .LBB172_14
.LBB172_8:
	addi r3, r0, 6
	lui lr, %hi(.L.str.44)
	addi lr, lr, %lo(.L.str.44)
	addi r12, r0, 26
	add r13, r0, r0
.LBB172_9:
	beq r13, r3, .LBB172_23
.LBB172_10:
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
	bne r16, r15, .LBB172_13
.LBB172_11:
	addi r13, r13, 1
	bne r9, r13, .LBB172_9
.LBB172_12:
	add lr, r9, r0
	beq lr, r3, .LBB172_24
.LBB172_13:
	add r15, r0, r0
.LBB172_14:
	add r16, r15, r0
	beq r10, r0, .LBB172_35
.LBB172_15:
	ldw r3, sp+336
	addi r1, r0, 1
	add r16, r15, r0
	blt r3, r1, .LBB172_35
.LBB172_16:
	addi r8, r0, 11
	lui r9, %hi(.L.str.45)
	addi r9, r9, %lo(.L.str.45)
	addi lr, r0, 26
	add r12, r0, r0
.LBB172_17:
	beq r12, r8, .LBB172_28
.LBB172_18:
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
	bne r16, r13, .LBB172_34
.LBB172_19:
	addi r12, r12, 1
	bne r3, r12, .LBB172_17
.LBB172_20:
	add r9, r3, r0
	beq r9, r8, .LBB172_29
	jal r0, .LBB172_34
.LBB172_21:
	bne r14, r0, .LBB172_44
.LBB172_22:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.43)
	addi r4, r4, %lo(.L.str.43)
	add r5, r11, r0
	jal r0, .LBB172_145
.LBB172_23:
	add lr, r3, r0
	bne lr, r3, .LBB172_13
.LBB172_24:
	addi r3, r0, 7
	add r15, r1, r0
	blt r9, r3, .LBB172_14
.LBB172_25:
	addi r9, r9, -7
	addi r1, r8, 6
	addi r3, r0, 32
	addi r8, r0, 1
.LBB172_26:
	ldbu lr, r1+0
	seq r15, lr, r3
	addi r12, r9, -1
	sltu r9, r12, r9
	bne r9, r8, .LBB172_14
.LBB172_27:
	addi r1, r1, 1
	add r9, r12, r0
	beq lr, r3, .LBB172_26
	jal r0, .LBB172_14
.LBB172_28:
	add r9, r8, r0
	bne r9, r8, .LBB172_34
.LBB172_29:
	addi r8, r0, 12
	blt r3, r8, .LBB172_33
.LBB172_30:
	addi r8, r10, 11
	addi r3, r3, -11
	addi r9, r0, 32
.LBB172_31:
	ldbu r10, r8+0
	bne r10, r9, .LBB172_34
.LBB172_32:
	addi r8, r8, 1
	addi r3, r3, -1
	bne r3, r0, .LBB172_31
.LBB172_33:
	add r16, r1, r0
	jal r0, .LBB172_35
.LBB172_34:
	add r16, r0, r0
.LBB172_35:
	ldw r17, sp+348
	addi r23, r0, 0
	sne r1, r6, r23
	sgt r3, r7, r23
	and r22, r1, r3
	bne r22, r18, .LBB172_41
.LBB172_36:
	addi r1, r0, 7
	lui r3, %hi(.L.str.46)
	addi r3, r3, %lo(.L.str.46)
	addi r8, r0, 26
	add r9, r0, r0
.LBB172_37:
	beq r9, r1, .LBB172_58
.LBB172_38:
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
	bne lr, r10, .LBB172_41
.LBB172_39:
	addi r9, r9, 1
	bne r7, r9, .LBB172_37
.LBB172_40:
	add r3, r7, r0
	beq r3, r1, .LBB172_59
.LBB172_41:
	add r24, r0, r0
.LBB172_42:
	sgt r1, r17, r23
	xori r3, r15, 1
	or  r1, r1, r3
	bne r1, r0, .LBB172_46
.LBB172_43:
	beq r14, r0, .LBB172_144
.LBB172_44:
	addi r1, r0, 2
.LBB172_45:
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
.LBB172_46:
	beq r4, r0, .LBB172_51
.LBB172_47:
	addi r19, r0, 1
	blt r5, r19, .LBB172_51
.LBB172_48:
	addi r1, r0, 256
	blt r5, r1, .LBB172_69
.LBB172_49:
	bne r14, r0, .LBB172_44
.LBB172_50:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.48)
	addi r4, r4, %lo(.L.str.48)
	jal r0, .LBB172_145
.LBB172_51:
	add r25, r6, r0
	add r26, r7, r0
	lui r5, %hi(.L.str.49)
	addi r5, r5, %lo(.L.str.49)
	addi r3, sp, 16
	addi r4, r0, 256
	add r6, r11, r0
	jal r31, snprintf
.LBB172_52:
	ldw r19, sp+344
	ldw r20, sp+340
	slli r13, r11, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r21, r13, r1
	ldw r1, r21+0
	beq r1, r0, .LBB172_55
.LBB172_53:
	slli r1, r11, 8
	lui r3, %hi(fio_ufname)
	addi r3, r3, %lo(fio_ufname)
	add r3, r1, r3
	addi r4, sp, 16
	jal r31, strcmp
	sne r1, r1, r23
	or  r1, r24, r1
	beq r1, r0, .LBB172_72
.LBB172_54:
	ldw r3, r21+0
	jal r31, fclose
	stw r21+0, r0
.LBB172_55:
	beq r24, r0, .LBB172_63
.LBB172_56:
	jal r31, tmpfile
	beq r1, r0, .LBB172_80
.LBB172_57:
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	jal r0, .LBB172_120
.LBB172_58:
	add r3, r1, r0
	bne r3, r1, .LBB172_41
.LBB172_59:
	addi r1, r0, 8
	blt r7, r1, .LBB172_79
.LBB172_60:
	addi r9, r7, -8
	addi r1, r6, 7
	addi r3, r0, 32
	addi r8, r0, 1
.LBB172_61:
	ldbu r10, r1+0
	seq r24, r10, r3
	addi lr, r9, -1
	sltu r9, lr, r9
	bne r9, r8, .LBB172_42
.LBB172_62:
	addi r1, r1, 1
	add r9, lr, r0
	beq r10, r3, .LBB172_61
	jal r0, .LBB172_42
.LBB172_63:
	beq r22, r0, .LBB172_97
.LBB172_64:
	addi r1, r0, 3
	lui r5, %hi(.L.str.52)
	addi r5, r5, %lo(.L.str.52)
	addi r6, r0, 26
	add r7, r0, r0
	add r4, r26, r0
	add r3, r25, r0
.LBB172_65:
	beq r7, r1, .LBB172_82
.LBB172_66:
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
	bne r9, r8, .LBB172_93
.LBB172_67:
	addi r7, r7, 1
	bne r4, r7, .LBB172_65
.LBB172_68:
	add r5, r4, r0
	jal r0, .LBB172_83
.LBB172_69:
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
.LBB172_70:
	add r5, r1, r12
	ldbu r6, r5+0
	bne r6, r3, .LBB172_52
.LBB172_71:
	add r4, r12, r0
	addi r12, r12, -1
	stb r5+0, r0
	bgt r4, r19, .LBB172_70
	jal r0, .LBB172_52
.LBB172_72:
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
	beq r20, r0, .LBB172_136
.LBB172_73:
	add r3, r0, r0
	blt r19, r18, .LBB172_137
.LBB172_74:
	addi r1, r0, 4
	lui r3, %hi(.L.str.50)
	addi r3, r3, %lo(.L.str.50)
	addi r4, r0, 26
	add r5, r0, r0
.LBB172_75:
	beq r5, r1, .LBB172_104
.LBB172_76:
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
	bne r7, r6, .LBB172_136
.LBB172_77:
	addi r5, r5, 1
	bne r19, r5, .LBB172_75
.LBB172_78:
	add r3, r19, r0
	jal r0, .LBB172_105
.LBB172_79:
	addi r24, r0, 1
	jal r0, .LBB172_42
.LBB172_80:
	bne r14, r0, .LBB172_44
.LBB172_81:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.51)
	addi r4, r4, %lo(.L.str.51)
	jal r0, .LBB172_145
.LBB172_82:
	add r5, r1, r0
.LBB172_83:
	bne r5, r1, .LBB172_93
.LBB172_84:
	addi r1, r0, 4
	blt r4, r1, .LBB172_88
.LBB172_85:
	addi r1, r3, 3
	addi r5, r4, -3
	addi r6, r0, 32
.LBB172_86:
	ldbu r7, r1+0
	bne r7, r6, .LBB172_93
.LBB172_87:
	addi r1, r1, 1
	addi r5, r5, -1
	bne r5, r0, .LBB172_86
.LBB172_88:
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	addi r3, sp, 16
	jal r31, fopen
	bne r1, r0, .LBB172_90
.LBB172_89:
	lui r4, %hi(.L.str.54)
	addi r4, r4, %lo(.L.str.54)
	addi r3, sp, 16
	jal r31, fopen
.LBB172_90:
	addi r4, sp, 16
	bne r1, r0, .LBB172_120
.LBB172_91:
	bne r14, r0, .LBB172_44
.LBB172_92:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	jal r0, .LBB172_140
.LBB172_93:
	lui r5, %hi(.L.str.56)
	addi r5, r5, %lo(.L.str.56)
	add r25, r3, r0
	add r26, r4, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB172_97
.LBB172_94:
	lui r4, %hi(.L.str.54)
	addi r4, r4, %lo(.L.str.54)
	addi r3, sp, 16
	jal r31, fopen
	beq r1, r0, .LBB172_141
.LBB172_95:
	add r3, r1, r0
	jal r31, fclose
	bne r14, r0, .LBB172_44
.LBB172_96:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.57)
	addi r4, r4, %lo(.L.str.57)
	jal r0, .LBB172_140
.LBB172_97:
	beq r25, r0, .LBB172_117
.LBB172_98:
	addi r1, r0, 1
	blt r26, r1, .LBB172_117
.LBB172_99:
	addi r1, r0, 7
	lui r3, %hi(.L.str.60)
	addi r3, r3, %lo(.L.str.60)
	addi r4, r0, 26
	add r5, r0, r0
.LBB172_100:
	beq r5, r1, .LBB172_111
.LBB172_101:
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
	bne r7, r6, .LBB172_127
.LBB172_102:
	addi r5, r5, 1
	bne r26, r5, .LBB172_100
.LBB172_103:
	add r3, r26, r0
	jal r0, .LBB172_112
.LBB172_104:
	add r3, r1, r0
.LBB172_105:
	bne r3, r1, .LBB172_136
.LBB172_106:
	addi r1, r0, 5
	blt r19, r1, .LBB172_110
.LBB172_107:
	addi r1, r20, 4
	addi r3, r19, -4
	addi r4, r0, 32
.LBB172_108:
	ldbu r5, r1+0
	bne r5, r4, .LBB172_136
.LBB172_109:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB172_108
.LBB172_110:
	add r3, r18, r0
	jal r0, .LBB172_137
.LBB172_111:
	add r3, r1, r0
.LBB172_112:
	bne r3, r1, .LBB172_127
.LBB172_113:
	addi r1, r0, 8
	blt r26, r1, .LBB172_117
.LBB172_114:
	addi r1, r25, 7
	addi r3, r26, -7
	addi r4, r0, 32
.LBB172_115:
	ldbu r5, r1+0
	bne r5, r4, .LBB172_127
.LBB172_116:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB172_115
.LBB172_117:
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	addi r3, sp, 16
	jal r31, fopen
	bne r1, r0, .LBB172_119
.LBB172_118:
	lui r4, %hi(.L.str.58)
	addi r4, r4, %lo(.L.str.58)
	addi r3, sp, 16
	jal r31, fopen
.LBB172_119:
	addi r4, sp, 16
	beq r1, r0, .LBB172_138
.LBB172_120:
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
	beq r20, r0, .LBB172_136
.LBB172_121:
	addi r1, r0, 1
	add r3, r0, r0
	blt r19, r1, .LBB172_137
.LBB172_122:
	addi r3, r0, 4
	lui r4, %hi(.L.str.50)
	addi r4, r4, %lo(.L.str.50)
	addi r5, r0, 26
	add r6, r0, r0
.LBB172_123:
	beq r6, r3, .LBB172_129
.LBB172_124:
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
	bne r8, r7, .LBB172_136
.LBB172_125:
	addi r6, r6, 1
	bne r19, r6, .LBB172_123
.LBB172_126:
	add r4, r19, r0
	jal r0, .LBB172_130
.LBB172_127:
	bne r14, r0, .LBB172_44
.LBB172_128:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.62)
	addi r4, r4, %lo(.L.str.62)
	jal r0, .LBB172_145
.LBB172_129:
	add r4, r3, r0
.LBB172_130:
	bne r4, r3, .LBB172_136
.LBB172_131:
	addi r3, r0, 5
	blt r19, r3, .LBB172_135
.LBB172_132:
	addi r3, r20, 4
	addi r4, r19, -4
	addi r5, r0, 32
.LBB172_133:
	ldbu r6, r3+0
	bne r6, r5, .LBB172_136
.LBB172_134:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB172_133
.LBB172_135:
	add r3, r1, r0
	jal r0, .LBB172_137
.LBB172_136:
	add r3, r0, r0
.LBB172_137:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r13, r1
	stw r1+0, r3
	add r1, r0, r0
	jal r0, .LBB172_45
.LBB172_138:
	bne r14, r0, .LBB172_44
.LBB172_139:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.61)
	addi r4, r4, %lo(.L.str.61)
.LBB172_140:
	addi r5, sp, 16
	jal r0, .LBB172_145
.LBB172_141:
	lui r4, %hi(.L.str.58)
	addi r4, r4, %lo(.L.str.58)
	addi r12, sp, 16
	add r3, r12, r0
	jal r31, fopen
	add r4, r12, r0
	bne r1, r0, .LBB172_120
.LBB172_142:
	bne r14, r0, .LBB172_44
.LBB172_143:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.59)
	addi r4, r4, %lo(.L.str.59)
	jal r0, .LBB172_140
.LBB172_144:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
.LBB172_145:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end172:
	.size	f77_open, .Lfunc_end172-f77_open
                                        # -- End function
	.p2align	2                               # -- Begin function fio_str_is
	.type	fio_str_is,@function
fio_str_is:                             # @fio_str_is
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	addi r1, r0, 1
	blt r4, r1, .LBB173_6
.LBB173_1:
	addi r7, r0, 26
	add r6, r0, r0
.LBB173_2:
	add r8, r5, r6
	ldb r8, r8+0
	beq r8, r0, .LBB173_7
.LBB173_3:
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
	bne r9, r8, .LBB173_8
.LBB173_4:
	addi r6, r6, 1
	bne r4, r6, .LBB173_2
.LBB173_5:
	add r6, r4, r0
	jal r0, .LBB173_7
.LBB173_6:
	add r6, r0, r0
.LBB173_7:
	add r5, r5, r6
	ldbu r5, r5+0
	beq r5, r0, .LBB173_10
.LBB173_8:
	add r1, r0, r0
.LBB173_9:
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.LBB173_10:
	ble r4, r6, .LBB173_9
.LBB173_11:
	add r3, r3, r6
	sub r4, r4, r6
	addi r5, r0, 32
.LBB173_12:
	ldbu r6, r3+0
	bne r6, r5, .LBB173_8
.LBB173_13:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB173_12
	jal r0, .LBB173_9
.Lfunc_end173:
	.size	fio_str_is, .Lfunc_end173-fio_str_is
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
	bgtu r1, r3, .LBB174_27
.LBB174_1:
	slli r11, r1, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r12, r11, r3
	ldw r3, r12+0
	beq r3, r0, .LBB174_27
.LBB174_2:
	addi r7, r0, 1
	beq r4, r0, .LBB174_10
.LBB174_3:
	addi r6, r0, 1
	add r13, r0, r0
	blt r5, r6, .LBB174_11
.LBB174_4:
	addi r7, r0, 6
	lui r8, %hi(.L.str.63)
	addi r8, r8, %lo(.L.str.63)
	addi r9, r0, 26
	add r10, r0, r0
.LBB174_5:
	beq r10, r7, .LBB174_28
.LBB174_6:
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
	bne r13, lr, .LBB174_9
.LBB174_7:
	addi r10, r10, 1
	bne r5, r10, .LBB174_5
.LBB174_8:
	add r8, r5, r0
	beq r8, r7, .LBB174_29
.LBB174_9:
	add r7, r0, r0
.LBB174_10:
	add r13, r0, r0
.LBB174_11:
	add r14, r1, r0
	bne r7, r0, .LBB174_24
.LBB174_12:
	addi r1, r0, 1
	blt r5, r1, .LBB174_34
.LBB174_13:
	addi r1, r0, 4
	lui r6, %hi(.L.str.64)
	addi r6, r6, %lo(.L.str.64)
	addi r7, r0, 26
	add r8, r0, r0
.LBB174_14:
	beq r8, r1, .LBB174_18
.LBB174_15:
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
	bne r10, r9, .LBB174_34
.LBB174_16:
	addi r8, r8, 1
	bne r5, r8, .LBB174_14
.LBB174_17:
	add r6, r5, r0
	jal r0, .LBB174_19
.LBB174_18:
	add r6, r1, r0
.LBB174_19:
	bne r6, r1, .LBB174_34
.LBB174_20:
	addi r1, r0, 5
	blt r5, r1, .LBB174_24
.LBB174_21:
	addi r1, r4, 4
	addi r4, r5, -4
	addi r5, r0, 32
.LBB174_22:
	ldbu r6, r1+0
	bne r6, r5, .LBB174_34
.LBB174_23:
	addi r1, r1, 1
	addi r4, r4, -1
	bne r4, r0, .LBB174_22
.LBB174_24:
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
	beq r13, r0, .LBB174_26
.LBB174_25:
	add r3, r11, r12
	jal r31, remove
.LBB174_26:
	add r1, r11, r12
	stb r1+0, r0
.LBB174_27:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB174_28:
	add r8, r7, r0
	bne r8, r7, .LBB174_9
.LBB174_29:
	addi r8, r0, 7
	add r7, r6, r0
	add r13, r6, r0
	blt r5, r8, .LBB174_11
.LBB174_30:
	addi r10, r5, -7
	addi r6, r4, 6
	addi r8, r0, 32
	addi r9, r0, 1
.LBB174_31:
	ldbu lr, r6+0
	seq r7, lr, r8
	addi r13, r10, -1
	sltu r10, r13, r10
	bne r10, r9, .LBB174_33
.LBB174_32:
	addi r6, r6, 1
	add r10, r13, r0
	add r13, r7, r0
	beq lr, r8, .LBB174_31
	jal r0, .LBB174_11
.LBB174_33:
	add r13, r7, r0
	jal r0, .LBB174_11
.LBB174_34:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.65)
	addi r4, r4, %lo(.L.str.65)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end174:
	.size	f77_close, .Lfunc_end174-f77_close
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
	bne r3, r0, .LBB175_4
.LBB175_1:
	addi r3, r0, -400
	lui r6, %hi(fio_uend+400)
	addi r6, r6, %lo(fio_uend+400)
	addi r7, r0, -1
.LBB175_2:
	add r8, r3, r6
	stw r8+0, r7
	addi r3, r3, 4
	bne r3, r0, .LBB175_2
.LBB175_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB175_4:
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
	beq r4, r0, .LBB175_21
.LBB175_5:
	addi r22, r0, 1
	blt r5, r22, .LBB175_21
.LBB175_6:
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
.LBB175_7:
	addi r4, sp, 28
	add r4, r4, r3
	ldbu r5, r4+0
	bne r5, r1, .LBB175_9
.LBB175_8:
	stb r4+0, r0
	addi r4, r3, -1
	addi r5, r3, 1
	add r3, r4, r0
	bgt r5, r22, .LBB175_7
.LBB175_9:
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	addi r12, sp, 28
	add r4, r12, r0
	jal r31, strcpy
	stw r21+%lo(inq_named), r22
	lui r4, %hi(.L.str.54)
	addi r4, r4, %lo(.L.str.54)
	add r3, r12, r0
	jal r31, fopen
	beq r1, r0, .LBB175_11
.LBB175_10:
	stb r23+%lo(inq_exist), r22
	add r3, r1, r0
	jal r31, fclose
.LBB175_11:
	lui r12, %hi(fio_ufname+256)
	addi r12, r12, %lo(fio_ufname+256)
	lui r13, %hi(fio_ufile+4)
	addi r13, r13, %lo(fio_ufile+4)
	addi r21, r0, 100
	jal r0, .LBB175_13
.LBB175_12:
	addi r22, r22, 1
	addi r12, r12, 256
	addi r13, r13, 4
	beq r22, r21, .LBB175_16
.LBB175_13:
	ldw r1, r13+0
	beq r1, r0, .LBB175_12
.LBB175_14:
	addi r4, sp, 28
	add r3, r12, r0
	jal r31, strcmp
	bne r1, r0, .LBB175_12
.LBB175_15:
	addi r1, r0, 1
	stb r19+%lo(inq_opened), r1
	stw r20+%lo(inq_unit), r22
.LBB175_16:
	ldbu r1, r19+%lo(inq_opened)
	beq r1, r0, .LBB175_26
.LBB175_17:
	ldw r3, r20+%lo(inq_unit)
	xor r3, r3, r11
	sub r1, r0, r1
	and r1, r3, r1
	xor r11, r11, r1
.LBB175_18:
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
	beq r4, r0, .LBB175_26
.LBB175_19:
	addi r4, r0, 1
	blt r3, r4, .LBB175_26
.LBB175_20:
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
	jal r0, .LBB175_26
.LBB175_21:
	addi r1, r0, 100
	bltu r11, r1, .LBB175_23
.LBB175_22:
	stb r23+%lo(inq_exist), r0
	jal r0, .LBB175_26
.LBB175_23:
	addi r1, r0, 1
	stb r23+%lo(inq_exist), r1
	stw r20+%lo(inq_unit), r11
	addi r3, r0, 6
	bgtu r11, r3, .LBB175_27
.LBB175_24:
	sll r4, r1, r11
	andi r4, r4, 97
	beq r4, r0, .LBB175_27
.LBB175_25:
	seq r3, r11, r3
	addi r4, r0, 5
	seq r4, r11, r4
	stb r19+%lo(inq_opened), r1
	lui r1, %hi(.L.str.68)
	addi r1, r1, %lo(.L.str.68)
	lui r5, %hi(.L.str.67)
	addi r5, r5, %lo(.L.str.67)
	xor r5, r5, r1
	sub r3, r0, r3
	and r3, r5, r3
	xor r1, r1, r3
	lui r3, %hi(.L.str.66)
	addi r3, r3, %lo(.L.str.66)
	xor r3, r3, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r4, r1, r3
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	jal r31, strcpy
	stw r21+%lo(inq_named), r0
.LBB175_26:
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
.LBB175_27:
	slli r3, r11, 2
	lui r4, %hi(fio_ufile)
	addi r4, r4, %lo(fio_ufile)
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r0, .LBB175_29
.LBB175_28:
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
	jal r0, .LBB175_18
.LBB175_29:
	stb r19+%lo(inq_opened), r0
	jal r0, .LBB175_26
.Lfunc_end175:
	.size	f77_inquire, .Lfunc_end175-f77_inquire
                                        # -- End function
	.globl	f77_inq                         # -- Begin function f77_inq
	.p2align	2
	.type	f77_inq,@function
f77_inq:                                # @f77_inq
# %bb.0:
	addi r1, r3, -1
	addi r3, r0, 13
	bgtu r1, r3, .LBB176_31
.LBB176_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r12, r5, r0
	add r11, r4, r0
	slli r1, r1, 2
	lui r3, %hi(.LJTI176_0)
	addi r3, r3, %lo(.LJTI176_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB176_2:
	lui r1, %hi(inq_exist)
	ldbu r1, r1+%lo(inq_exist)
	jal r0, .LBB176_29
.LBB176_3:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB176_30
.LBB176_4:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB176_30
.LBB176_5:
	lui r1, %hi(inq_recl)
	ldw r1, r1+%lo(inq_recl)
	jal r0, .LBB176_29
.LBB176_6:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r14, r0, 1
	bne r1, r14, .LBB176_30
.LBB176_7:
	lui r1, %hi(inq_named)
	ldw r1, r1+%lo(inq_named)
	beq r1, r0, .LBB176_30
.LBB176_8:
	lui r13, %hi(inq_name)
	addi r13, r13, %lo(inq_name)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB176_30
.LBB176_9:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB176_11
.LBB176_10:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB176_30
.LBB176_11:
	add r5, r3, r0
	bge r4, r1, .LBB176_10
.LBB176_12:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB176_10
.LBB176_13:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB176_30
.LBB176_14:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB176_30
.LBB176_15:
	lui r1, %hi(inq_nextrec)
	ldw r1, r1+%lo(inq_nextrec)
	jal r0, .LBB176_29
.LBB176_16:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.71)
	addi r4, r4, %lo(.L.str.71)
	lui r5, %hi(.L.str.72)
	addi r5, r5, %lo(.L.str.72)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB176_30
.LBB176_17:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB176_19
.LBB176_18:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB176_30
.LBB176_19:
	add r5, r3, r0
	bge r4, r1, .LBB176_18
.LBB176_20:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB176_18
.LBB176_21:
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
	jal r0, .LBB176_29
.LBB176_22:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_named)
	ldw r3, r3+%lo(inq_named)
	sub r1, r0, r1
	and r1, r3, r1
	jal r0, .LBB176_29
.LBB176_23:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.72)
	addi r4, r4, %lo(.L.str.72)
	lui r5, %hi(.L.str.71)
	addi r5, r5, %lo(.L.str.71)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB176_30
.LBB176_24:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB176_26
.LBB176_25:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB176_30
.LBB176_26:
	add r5, r3, r0
	bge r4, r1, .LBB176_25
.LBB176_27:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB176_25
.LBB176_28:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
.LBB176_29:
	stw r11+0, r1
.LBB176_30:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB176_31:
	jalr r0, r31, 0
.LBB176_32:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	lui r5, %hi(.L.str.70)
	addi r5, r5, %lo(.L.str.70)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.69)
	addi r4, r4, %lo(.L.str.69)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB176_30
.LBB176_33:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB176_35
.LBB176_34:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB176_30
.LBB176_35:
	add r5, r3, r0
	bge r4, r1, .LBB176_34
.LBB176_36:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB176_34
.LBB176_37:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r5, %hi(inq_bz)
	ldw r5, r5+%lo(inq_bz)
	seq r4, r5, r4
	lui r5, %hi(.L.str.50)
	addi r5, r5, %lo(.L.str.50)
	lui r6, %hi(.L.str.74)
	addi r6, r6, %lo(.L.str.74)
	xor r6, r6, r5
	sub r4, r0, r4
	and r4, r6, r4
	xor r4, r5, r4
	lui r5, %hi(.L.str.69)
	addi r5, r5, %lo(.L.str.69)
	xor r4, r4, r5
	sub r3, r0, r3
	and r3, r4, r3
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r5, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB176_30
.LBB176_38:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB176_40
.LBB176_39:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB176_30
.LBB176_40:
	add r5, r3, r0
	bge r4, r1, .LBB176_39
.LBB176_41:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB176_39
.LBB176_42:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.71)
	addi r4, r4, %lo(.L.str.71)
	lui r5, %hi(.L.str.72)
	addi r5, r5, %lo(.L.str.72)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB176_30
.LBB176_43:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB176_45
.LBB176_44:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB176_30
.LBB176_45:
	add r5, r3, r0
	bge r4, r1, .LBB176_44
.LBB176_46:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB176_44
.LBB176_47:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.72)
	addi r4, r4, %lo(.L.str.72)
	lui r5, %hi(.L.str.71)
	addi r5, r5, %lo(.L.str.71)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB176_30
.LBB176_48:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB176_50
.LBB176_49:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB176_30
.LBB176_50:
	add r5, r3, r0
	bge r4, r1, .LBB176_49
.LBB176_51:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB176_49
.LBB176_52:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	lui r5, %hi(.L.str.73)
	addi r5, r5, %lo(.L.str.73)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.69)
	addi r4, r4, %lo(.L.str.69)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB176_30
.LBB176_53:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB176_55
.LBB176_54:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB176_30
.LBB176_55:
	add r5, r3, r0
	bge r4, r1, .LBB176_54
.LBB176_56:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB176_54
.Lfunc_end176:
	.size	f77_inq, .Lfunc_end176-f77_inq
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI176_0,@object
.LJTI176_0:
	.word	.LBB176_2
	.word	.LBB176_28
	.word	.LBB176_21
	.word	.LBB176_22
	.word	.LBB176_6
	.word	.LBB176_32
	.word	.LBB176_42
	.word	.LBB176_23
	.word	.LBB176_52
	.word	.LBB176_16
	.word	.LBB176_47
	.word	.LBB176_3
	.word	.LBB176_13
	.word	.LBB176_37
	.size	.LJTI176_0, 56
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
	bne r3, r0, .LBB177_4
.LBB177_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB177_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB177_2
.LBB177_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB177_4:
	addi r1, r11, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB177_7
.LBB177_5:
	addi r1, r11, -5
	addi r3, r0, 1
	bleu r1, r3, .LBB177_7
.LBB177_6:
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
.LBB177_7:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.75)
	addi r4, r4, %lo(.L.str.75)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end177:
	.size	f77_rewind, .Lfunc_end177-f77_rewind
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
	beq r3, r16, .LBB178_191
.LBB178_1:
	add r13, r4, r0
	add r12, r3, r0
	lui lr, %hi(fio_ftext)
	addi r11, r0, -1
	stw lr+%lo(fio_ftext), r11
	addi r20, r0, 32
	add r1, r0, r0
	jal r0, .LBB178_3
.LBB178_2:
	add r1, r12, r28
	ldbu r3, r1+0
	addi r1, r28, 1
	bne r3, r20, .LBB178_5
.LBB178_3:
	add r28, r1, r0
	blt r13, r0, .LBB178_2
.LBB178_4:
	bltu r28, r13, .LBB178_2
.LBB178_5:
	lui r14, %hi(.L.str.84)
	addi r14, r14, %lo(.L.str.84)
	lui r15, %hi(fio_fmt_why)
	blt r13, r0, .LBB178_7
.LBB178_6:
	bgeu r28, r13, .LBB178_193
.LBB178_7:
	add r1, r12, r28
	ldbu r1, r1+0
	addi r3, r0, 40
	stw sp+52, r3
	bne r1, r3, .LBB178_193
.LBB178_8:
	addi r23, r0, 1022
	lui r19, %hi(.L.str.85)
	addi r19, r19, %lo(.L.str.85)
	addi r1, r0, 1
	stw sp+48, r1
	slli r1, r1, 2
	lui r24, %hi(.LJTI178_1)
	addi r24, r24, %lo(.LJTI178_1)
	add r22, r24, r1
	addi r26, r0, 39
	addi r9, r0, 246
	addi r10, r0, 41
	addi r25, r0, 0
	add r21, r0, r0
	add fp, r0, r0
	lui r18, %hi(fio_fbuf+1)
	addi r18, r18, %lo(fio_fbuf+1)
.LBB178_9:
	blt r13, r0, .LBB178_13
.LBB178_10:
	add r17, r0, r0
	bltu r28, r13, .LBB178_13
.LBB178_11:
	blt r21, r23, .LBB178_14
.LBB178_12:
	stw r15+%lo(fio_fmt_why), r19
	ldw r1, r22+0
	jalr r0, r1, 0
.LBB178_13:
	add r1, r12, r28
	ldbu r17, r1+0
	bge r21, r23, .LBB178_12
.LBB178_14:
	andi r27, r17, 255
	bgtu r27, r26, .LBB178_38
.LBB178_15:
	slli r1, r27, 2
	lui r3, %hi(.LJTI178_0)
	addi r3, r3, %lo(.LJTI178_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB178_16:
	add r1, r21, r0
	add r5, r26, r0
	jal r0, .LBB178_18
.LBB178_17:
	ldbu r5, r4+0
	add r1, r21, r0
.LBB178_18:
	add r3, r28, r0
.LBB178_19:
	add r4, r1, r16
	stb r4+0, r5
	addi r28, r3, 1
	blt r13, r0, .LBB178_21
.LBB178_20:
	add r4, r0, r0
	bgeu r28, r13, .LBB178_22
.LBB178_21:
	add r4, r12, r28
	ldbu r4, r4+0
.LBB178_22:
	addi r21, r1, 1
	andi r4, r4, 255
	beq r4, r0, .LBB178_35
.LBB178_23:
	addi r5, r0, 1020
	bge r1, r5, .LBB178_35
.LBB178_24:
	bne r4, r27, .LBB178_29
.LBB178_25:
	addi r28, r3, 2
	blt r13, r0, .LBB178_27
.LBB178_26:
	add r3, r0, r0
	bgeu r28, r13, .LBB178_28
.LBB178_27:
	add r3, r12, r28
	ldbu r3, r3+0
.LBB178_28:
	andi r3, r3, 255
	bne r3, r27, .LBB178_48
.LBB178_29:
	add r4, r12, r28
	blt r13, r0, .LBB178_31
.LBB178_30:
	bgeu r28, r13, .LBB178_33
.LBB178_31:
	ldbu r5, r4+0
	addi r3, r0, 39
	bne r5, r3, .LBB178_33
.LBB178_32:
	addi r21, r1, 2
	add r1, r1, r18
	stb r1+0, r3
.LBB178_33:
	blt r13, r0, .LBB178_17
.LBB178_34:
	add r1, r21, r0
	add r5, r0, r0
	add r3, r28, r0
	bgeu r28, r13, .LBB178_19
	jal r0, .LBB178_17
.LBB178_35:
	lui r1, %hi(.L.str.87)
	addi r1, r1, %lo(.L.str.87)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB178_36:
	addi r28, r28, 1
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB178_37:
	lui r1, %hi(.L.str.86)
	addi r1, r1, %lo(.L.str.86)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB178_38:
	addi r1, r17, -58
	andi r1, r1, 255
	bltu r1, r9, .LBB178_51
.LBB178_39:
	beq r21, r0, .LBB178_49
.LBB178_40:
	ldw r3, lr+%lo(fio_ftext)
	add r1, r0, r0
	beq r21, r3, .LBB178_50
.LBB178_41:
	stw sp+36, fp
	stw sp+40, r25
	stw sp+44, r24
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
	ldw r1, sp+48
	blt r21, r3, .LBB178_43
.LBB178_42:
	lui r1, %hi(fio_fbuf-2)
	addi r1, r1, %lo(fio_fbuf-2)
	add r1, r21, r1
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 66
	sne r1, r1, r3
.LBB178_43:
	addi r3, r0, 90
	add r9, r11, r0
	add r11, fp, r0
	add r10, r19, r0
	add lr, r15, r0
	add r15, r22, r0
	add r19, r25, r0
	add r22, r24, r0
	ldw r24, sp+44
	ldw r25, sp+40
	ldw fp, sp+36
	beq r14, r3, .LBB178_50
.LBB178_44:
	addi r1, r0, -4
	and r3, r14, r1
	addi r1, r0, 1
	addi r4, r0, 68
	beq r3, r4, .LBB178_50
.LBB178_45:
	addi r3, r14, -65
	addi r4, r0, 19
	bgtu r3, r4, .LBB178_47
.LBB178_46:
	lui r4, 165
	addi r4, r4, -1791
	srl r3, r4, r3
	andi r3, r3, 1
	bne r3, r0, .LBB178_50
.LBB178_47:
	addi r1, r0, 46
	seq r1, r14, r1
	jal r0, .LBB178_50
.LBB178_48:
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
.LBB178_49:
	add r1, r0, r0
.LBB178_50:
	beq r1, r0, .LBB178_55
.LBB178_51:
	addi r1, r21, 1
	add r3, r21, r16
	stb r3+0, r17
	addi r28, r28, 1
	ldw r3, sp+52
	seq r3, r27, r3
	add fp, fp, r3
	bne r27, r10, .LBB178_53
.LBB178_52:
	addi fp, fp, -1
	seq r3, fp, r25
	sub r3, r0, r3
	andi r5, r3, 4
	add r21, r1, r0
	jal r0, .LBB178_54
.LBB178_53:
	add r21, r1, r0
	add r5, r0, r0
.LBB178_54:
	slli r1, r5, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB178_55:
	add r1, r28, r0
	add r4, r0, r0
	jal r0, .LBB178_59
.LBB178_56:
	ldbu r6, r3+0
.LBB178_57:
	addi r3, r0, 10
	mul r3, r4, r3
	add r3, r3, r5
	addi r4, r3, -48
	addi r3, r21, 1
	add r5, r21, r16
	stb r5+0, r6
	add r21, r3, r0
.LBB178_58:
	addi r1, r1, 1
.LBB178_59:
	add r3, r12, r1
	blt r13, r0, .LBB178_61
.LBB178_60:
	bgeu r1, r13, .LBB178_62
.LBB178_61:
	ldbu r5, r3+0
	addi r5, r5, -58
	andi r5, r5, 255
	addi r6, r0, 245
	bgtu r5, r6, .LBB178_65
.LBB178_62:
	blt r13, r0, .LBB178_64
.LBB178_63:
	bgeu r1, r13, .LBB178_74
.LBB178_64:
	ldbu r5, r3+0
	bne r5, r20, .LBB178_74
.LBB178_65:
	blt r13, r0, .LBB178_67
.LBB178_66:
	bgeu r1, r13, .LBB178_68
.LBB178_67:
	ldbu r5, r3+0
	beq r5, r20, .LBB178_58
.LBB178_68:
	bge r21, r23, .LBB178_84
.LBB178_69:
	blt r13, r0, .LBB178_71
.LBB178_70:
	add r5, r0, r0
	bgeu r1, r13, .LBB178_72
.LBB178_71:
	ldbu r5, r3+0
.LBB178_72:
	blt r13, r0, .LBB178_56
.LBB178_73:
	add r6, r0, r0
	bgeu r1, r13, .LBB178_57
	jal r0, .LBB178_56
.LBB178_74:
	blt r13, r0, .LBB178_76
.LBB178_75:
	bgeu r1, r13, .LBB178_77
.LBB178_76:
	ldbu r5, r3+0
	addi r6, r0, 72
	beq r5, r6, .LBB178_80
.LBB178_77:
	addi r5, r0, 5
	blt r13, r0, .LBB178_79
.LBB178_78:
	bgeu r1, r13, .LBB178_83
.LBB178_79:
	ldbu r6, r3+0
	addi r7, r0, 104
	bne r6, r7, .LBB178_83
.LBB178_80:
	beq r4, r0, .LBB178_85
.LBB178_81:
	add r5, r4, r21
	addi r6, r0, 1022
	blt r5, r6, .LBB178_86
.LBB178_82:
	lui r3, %hi(.L.str.88)
	addi r3, r3, %lo(.L.str.88)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB178_83:
	add r28, r1, r0
	jal r0, .LBB178_54
.LBB178_84:
	lui r3, %hi(.L.str.88)
	addi r3, r3, %lo(.L.str.88)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	add r21, r23, r0
	jalr r0, r3, 0
.LBB178_85:
	lui r3, %hi(.L.str.89)
	addi r3, r3, %lo(.L.str.89)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB178_86:
	blt r13, r0, .LBB178_88
.LBB178_87:
	add r5, r0, r0
	bgeu r1, r13, .LBB178_89
.LBB178_88:
	ldbu r5, r3+0
.LBB178_89:
	add r6, r21, r16
	stb r6+0, r5
	addi r21, r21, 1
	addi r5, r0, 1
	blt r4, r5, .LBB178_99
.LBB178_90:
	addi r6, r4, 1
	add r4, r0, r0
	jal r0, .LBB178_93
.LBB178_91:
	ldbu r8, r7+1
.LBB178_92:
	add r7, r21, r4
	add r7, r7, r16
	stb r7+0, r8
	addi r6, r6, -1
	addi r4, r4, 1
	ble r6, r5, .LBB178_100
.LBB178_93:
	add r7, r1, r4
	addi r28, r7, 1
	blt r13, r0, .LBB178_95
.LBB178_94:
	bgeu r28, r13, .LBB178_98
.LBB178_95:
	add r7, r3, r4
	ldbu r8, r7+1
	beq r8, r0, .LBB178_98
.LBB178_96:
	blt r13, r0, .LBB178_91
.LBB178_97:
	add r8, r0, r0
	bgeu r28, r13, .LBB178_92
	jal r0, .LBB178_91
.LBB178_98:
	lui r1, %hi(.L.str.90)
	addi r1, r1, %lo(.L.str.90)
	stw r15+%lo(fio_fmt_why), r1
	add r21, r21, r4
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB178_99:
	addi r28, r1, 1
	jal r0, .LBB178_101
.LBB178_100:
	add r1, r1, r4
	addi r28, r1, 1
	add r21, r21, r4
.LBB178_101:
	stw lr+%lo(fio_ftext), r21
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB178_102:
	add r1, r21, r16
	stb r1+0, r0
	addi r14, r0, 1
	addi r19, r0, 58
	addi r24, r0, -1
	addi r21, r0, -10
	addi r22, r0, 9
	addi r23, r0, 10
	addi r25, r0, 72
	lui r17, %hi(.L.str.92)
	addi r17, r17, %lo(.L.str.92)
	addi r26, r0, 80
	addi r27, r0, 88
	addi r28, r0, 25
	addi r1, r0, -4
	stw sp+36, r1
	addi r1, r0, 68
	stw sp+48, r1
	addi r1, r0, 69
	stw sp+40, r1
	addi r1, r0, 83
	stw sp+44, r1
	addi r1, r0, 78
	stw sp+32, r1
	jal r0, .LBB178_104
.LBB178_103:
	addi r14, r14, 1
.LBB178_104:
	add r1, r14, r16
	ldbu r1, r1+0
	bgtu r1, r19, .LBB178_116
.LBB178_105:
	slli r3, r1, 2
	lui r4, %hi(.LJTI178_2)
	addi r4, r4, %lo(.LJTI178_2)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB178_106:
	addi fp, r14, 1
	add r1, r14, r18
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB178_112
.LBB178_107:
	addi r20, r0, -1
	jal r0, .LBB178_114
.LBB178_108:
	addi r14, r14, 1
.LBB178_109:
	add r1, r14, r0
	add r3, r14, r16
	ldbu r4, r3+0
	addi r14, r14, 1
	addi r3, r0, 39
	bne r4, r3, .LBB178_109
.LBB178_110:
	add r4, r14, r16
	ldbu r4, r4+0
	bne r4, r3, .LBB178_104
.LBB178_111:
	addi r14, r1, 2
	jal r0, .LBB178_109
.LBB178_112:
	add r20, r0, r0
	addi r1, r1, -48
	bgtu r1, r22, .LBB178_114
.LBB178_113:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB178_113
.LBB178_114:
	lui r14, %hi(.L.str.91)
	addi r14, r14, %lo(.L.str.91)
	blt r20, r0, .LBB178_193
.LBB178_115:
	add r1, fp, r16
	ldbu r3, r1+0
	jal r31, toupper
	beq r1, r26, .LBB178_122
	jal r0, .LBB178_193
.LBB178_116:
	slli r1, r1, 24
	srai r1, r1, 24
	addi r3, r1, -58
	bgeu r3, r21, .LBB178_118
.LBB178_117:
	add fp, r14, r0
	add r20, r24, r0
	jal r0, .LBB178_122
.LBB178_118:
	addi r1, r1, -48
	bleu r1, r22, .LBB178_120
.LBB178_119:
	add fp, r14, r0
	add r20, r0, r0
	jal r0, .LBB178_122
.LBB178_120:
	add r20, r0, r0
	add fp, r14, r0
.LBB178_121:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB178_121
.LBB178_122:
	add r14, fp, r16
	ldbu r3, r14+0
	jal r31, toupper
	bne r1, r25, .LBB178_125
.LBB178_123:
	blt r20, r0, .LBB178_192
.LBB178_124:
	add r1, fp, r20
	addi r14, r1, 1
	jal r0, .LBB178_104
.LBB178_125:
	blt r20, r0, .LBB178_128
.LBB178_126:
	ldw r3, sp+52
	bne r1, r3, .LBB178_128
.LBB178_127:
	addi r14, fp, 1
	jal r0, .LBB178_104
.LBB178_128:
	beq r1, r27, .LBB178_135
.LBB178_129:
	sgt r3, r20, r24
	seq r4, r1, r26
	and r3, r3, r4
	bne r3, r0, .LBB178_135
.LBB178_130:
	addi r3, r1, -65
	bgtu r3, r28, .LBB178_139
.LBB178_131:
	slli r3, r3, 2
	lui r4, %hi(.LJTI178_3)
	addi r4, r4, %lo(.LJTI178_3)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB178_132:
	addi r14, fp, 1
	add r1, fp, r18
.LBB178_133:
	ldb r1, r1+0
	addi r1, r1, -58
	bltu r1, r21, .LBB178_104
.LBB178_134:
	add r1, r14, r0
	addi r14, r14, 1
	add r1, r1, r18
	jal r0, .LBB178_133
.LBB178_135:
	addi r14, fp, 1
	jal r0, .LBB178_104
.LBB178_136:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 82
	beq r1, r3, .LBB178_138
.LBB178_137:
	addi r3, r0, 76
	bne r1, r3, .LBB178_163
.LBB178_138:
	addi r14, fp, 2
	jal r0, .LBB178_164
.LBB178_139:
	ldw r3, sp+36
	and r3, r1, r3
	ldw r4, sp+48
	beq r3, r4, .LBB178_142
.LBB178_140:
	addi r3, r0, 76
	beq r1, r3, .LBB178_142
.LBB178_141:
	addi r3, r0, 73
	bne r1, r3, .LBB178_211
.LBB178_142:
	stw sp+28, r17
	addi r17, fp, 1
	ldw r3, sp+40
	bne r1, r3, .LBB178_146
.LBB178_143:
	add r20, r17, r16
	ldbu r3, r20+0
	add r14, r1, r0
	jal r31, toupper
	add r3, r1, r0
	add r1, r14, r0
	ldw r4, sp+44
	beq r3, r4, .LBB178_145
.LBB178_144:
	ldbu r3, r20+0
	jal r31, toupper
	add r3, r1, r0
	add r1, r14, r0
	ldw r4, sp+32
	bne r3, r4, .LBB178_146
.LBB178_145:
	addi r17, fp, 2
.LBB178_146:
	add r3, r17, r16
	ldb r3, r3+0
	addi r4, r3, -58
	bltu r4, r21, .LBB178_206
.LBB178_147:
	addi r3, r3, -48
	bleu r3, r22, .LBB178_155
.LBB178_148:
	add r3, r0, r0
	jal r0, .LBB178_158
.LBB178_149:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 90
	beq r1, r3, .LBB178_151
.LBB178_150:
	ldw r3, sp+32
	bne r1, r3, .LBB178_209
.LBB178_151:
	addi r14, fp, 2
	jal r0, .LBB178_104
.LBB178_152:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	ldw r3, sp+44
	beq r1, r3, .LBB178_154
.LBB178_153:
	addi r14, fp, 1
	bne r1, r26, .LBB178_104
.LBB178_154:
	addi r14, fp, 2
	jal r0, .LBB178_104
.LBB178_155:
	add r4, r0, r0
.LBB178_156:
	add r5, r17, r0
	mul r4, r4, r23
	add r4, r4, r3
	addi r17, r17, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB178_156
.LBB178_157:
	srli r3, r4, 31
.LBB178_158:
	bne r3, r0, .LBB178_206
.LBB178_159:
	add r3, r17, r16
	ldbu r3, r3+0
	addi r4, r0, 46
	bne r3, r4, .LBB178_162
.LBB178_160:
	addi r14, r17, 1
	add r3, r17, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB178_172
.LBB178_161:
	addi r3, r0, 1
	ldw r17, sp+28
	jal r0, .LBB178_177
.LBB178_162:
	add r14, r17, r0
	ldw r17, sp+28
	jal r0, .LBB178_178
.LBB178_163:
	addi r14, fp, 1
.LBB178_164:
	add r1, r14, r16
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB178_166
.LBB178_165:
	addi r1, r0, 1
	jal r0, .LBB178_171
.LBB178_166:
	addi r1, r1, -48
	bleu r1, r22, .LBB178_168
.LBB178_167:
	add r1, r0, r0
	jal r0, .LBB178_171
.LBB178_168:
	add r3, r0, r0
.LBB178_169:
	add r4, r14, r0
	mul r3, r3, r23
	add r3, r3, r1
	addi r14, r14, 1
	add r1, r4, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB178_169
.LBB178_170:
	srli r1, r3, 31
.LBB178_171:
	beq r1, r0, .LBB178_104
	jal r0, .LBB178_208
.LBB178_172:
	addi r3, r3, -48
	ldw r17, sp+28
	bleu r3, r22, .LBB178_174
.LBB178_173:
	add r3, r0, r0
	jal r0, .LBB178_177
.LBB178_174:
	add r4, r0, r0
.LBB178_175:
	add r5, r14, r0
	mul r4, r4, r23
	add r4, r4, r3
	addi r14, r14, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB178_175
.LBB178_176:
	srli r3, r4, 31
.LBB178_177:
	bne r3, r0, .LBB178_210
.LBB178_178:
	addi r3, r0, 71
	beq r1, r3, .LBB178_180
.LBB178_179:
	andi r1, r1, 78
	ldw r3, sp+48
	bne r1, r3, .LBB178_104
.LBB178_180:
	add r1, r14, r16
	ldbu r3, r1+0
	jal r31, toupper
	ldw r3, sp+40
	bne r1, r3, .LBB178_104
.LBB178_181:
	addi r1, r14, 1
	add r3, r14, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB178_183
.LBB178_182:
	addi r3, r0, 1
	jal r0, .LBB178_188
.LBB178_183:
	addi r3, r3, -48
	bleu r3, r22, .LBB178_185
.LBB178_184:
	add r14, r1, r0
	add r3, r0, r0
	jal r0, .LBB178_189
.LBB178_185:
	add r4, r0, r0
.LBB178_186:
	add r5, r1, r0
	mul r1, r4, r23
	add r4, r1, r3
	addi r1, r5, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB178_186
.LBB178_187:
	srli r3, r4, 31
.LBB178_188:
	add r14, r1, r0
.LBB178_189:
	beq r3, r0, .LBB178_104
.LBB178_190:
	lui r14, %hi(.L.str.97)
	addi r14, r14, %lo(.L.str.97)
	jal r0, .LBB178_193
.LBB178_191:
	lui r1, %hi(fio_fmt)
	stw r1+%lo(fio_fmt), r16
	add r11, r0, r0
	jal r0, .LBB178_205
.LBB178_192:
	add r14, r17, r0
.LBB178_193:
	stw r15+%lo(fio_fmt_why), r14
.LBB178_194:
	addi r1, r0, 79
	addi r3, r0, 40
	addi r4, r0, 41
	add r6, r0, r0
	add r5, r0, r0
	jal r0, .LBB178_196
.LBB178_195:
	addi r6, r6, 1
.LBB178_196:
	blt r13, r0, .LBB178_198
.LBB178_197:
	add r7, r0, r0
	bgeu r6, r13, .LBB178_199
.LBB178_198:
	add r7, r12, r6
	ldbu r7, r7+0
.LBB178_199:
	andi r7, r7, 255
	beq r7, r0, .LBB178_204
.LBB178_200:
	bgtu r6, r1, .LBB178_204
.LBB178_201:
	seq r8, r7, r3
	add r5, r5, r8
	bne r7, r4, .LBB178_195
.LBB178_202:
	addi r5, r5, -1
	bne r5, r0, .LBB178_195
.LBB178_203:
	addi r6, r6, 1
.LBB178_204:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	ldw r5, r15+%lo(fio_fmt_why)
	lui r4, %hi(.L.str.101)
	addi r4, r4, %lo(.L.str.101)
	add r7, r12, r0
	jal r31, fprintf
.LBB178_205:
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
.LBB178_206:
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.95)
	addi r4, r4, %lo(.L.str.95)
	add r3, r14, r0
	add r5, r1, r0
.LBB178_207:
	jal r31, sprintf
	jal r0, .LBB178_193
.LBB178_208:
	lui r14, %hi(.L.str.93)
	addi r14, r14, %lo(.L.str.93)
	jal r0, .LBB178_193
.LBB178_209:
	lui r14, %hi(.L.str.94)
	addi r14, r14, %lo(.L.str.94)
	jal r0, .LBB178_193
.LBB178_210:
	lui r14, %hi(.L.str.96)
	addi r14, r14, %lo(.L.str.96)
	jal r0, .LBB178_193
.LBB178_211:
	blt r20, r0, .LBB178_214
.LBB178_212:
	addi r1, r1, -41
	addi r3, r0, 17
	bgtu r1, r3, .LBB178_215
.LBB178_213:
	slli r1, r1, 2
	lui r3, %hi(.Lswitch.table.fio_fmt_use)
	addi r3, r3, %lo(.Lswitch.table.fio_fmt_use)
	add r1, r1, r3
	ldw r14, r1+0
	jal r0, .LBB178_193
.LBB178_214:
	ldb r5, r14+0
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
	add r3, r14, r0
	jal r0, .LBB178_207
.LBB178_215:
	lui r14, %hi(.L.str.99)
	addi r14, r14, %lo(.L.str.99)
	jal r0, .LBB178_193
.LBB178_216:
.Lfunc_end178:
	.size	fio_fmt_use, .Lfunc_end178-fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI178_0,@object
.LJTI178_0:
	.word	.LBB178_37
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_36
	.word	.LBB178_38
	.word	.LBB178_16
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_38
	.word	.LBB178_16
	.size	.LJTI178_0, 160
	.type	.LJTI178_1,@object
.LJTI178_1:
	.word	.LBB178_9
	.word	.LBB178_194
	.word	.LBB178_216
	.word	.LBB178_216
	.word	.LBB178_102
	.word	.LBB178_9
	.size	.LJTI178_1, 24
	.type	.LJTI178_2,@object
.LJTI178_2:
	.word	.LBB178_191
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_108
	.word	.LBB178_103
	.word	.LBB178_103
	.word	.LBB178_116
	.word	.LBB178_106
	.word	.LBB178_103
	.word	.LBB178_106
	.word	.LBB178_116
	.word	.LBB178_103
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_116
	.word	.LBB178_103
	.size	.LJTI178_2, 236
	.type	.LJTI178_3,@object
.LJTI178_3:
	.word	.LBB178_132
	.word	.LBB178_149
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_132
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_152
	.word	.LBB178_136
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_139
	.word	.LBB178_132
	.size	.LJTI178_3, 104
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
	bleu r5, r6, .LBB179_2
.LBB179_1:
	add r1, r0, r0
	jal r0, .LBB179_5
.LBB179_2:
	addi r6, r1, 1
	addi r7, r0, 10
	add r1, r0, r0
.LBB179_3:
	mul r1, r1, r7
	addi r8, r4, 1
	add r1, r5, r1
	add r4, r6, r4
	ldb r4, r4+0
	addi r5, r4, -48
	add r4, r8, r0
	bltu r5, r7, .LBB179_3
.LBB179_4:
	stw r3+%lo(fio_pos), r8
.LBB179_5:
	jalr r0, r31, 0
.Lfunc_end179:
	.size	fio_number, .Lfunc_end179-fio_number
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
	beq r1, r0, .LBB180_3
.LBB180_1:
	lui r11, %hi(fio_irec)
	ldw r3, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r3, r4, .LBB180_28
.LBB180_2:
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
	jal r0, .LBB180_27
.LBB180_3:
	lui r12, %hi(fio_unit)
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	addi r13, r0, 98
	lui r11, %hi(fio_in)
	bgtu r3, r13, .LBB180_7
.LBB180_4:
	slli r1, r1, 2
	lui r14, %hi(fio_uend)
	addi r14, r14, %lo(fio_uend)
	add r1, r1, r14
	ldw r1, r1+0
	blt r1, r0, .LBB180_7
.LBB180_5:
	ldw r3, r11+%lo(fio_in)
	beq r3, r0, .LBB180_7
.LBB180_6:
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	add r4, r3, r14
	ldw r4, r4+0
	bge r1, r4, .LBB180_30
.LBB180_7:
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	bgtu r3, r13, .LBB180_11
.LBB180_8:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB180_11
.LBB180_9:
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
	beq r3, r0, .LBB180_29
.LBB180_10:
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB180_26
.LBB180_11:
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
.LBB180_12:
	add r18, r3, r0
	add r3, r11, r0
	jal r31, getc
	beq r1, r16, .LBB180_17
.LBB180_13:
	addi r3, r18, 1
	add r4, r18, r15
	stb r4+0, r1
	beq r1, r14, .LBB180_15
.LBB180_14:
	bltu r18, r17, .LBB180_12
.LBB180_15:
	beq r3, r0, .LBB180_18
.LBB180_16:
	add r1, r3, r15
	stb r1+0, r0
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB180_20
.LBB180_17:
	add r3, r18, r0
	bne r3, r0, .LBB180_16
.LBB180_18:
	ldw r1, r12+%lo(fio_unit)
	addi r4, r1, -1
	bgtu r4, r13, .LBB180_20
.LBB180_19:
	slli r1, r1, 2
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r1, r1, r4
	addi r4, r0, 1
	stw r1+0, r4
.LBB180_20:
	beq r3, r0, .LBB180_28
.LBB180_21:
	lui r1, %hi(fio_rlen)
	ldw r4, r1+%lo(fio_rlen)
	addi r3, r0, 1
	blt r4, r3, .LBB180_26
.LBB180_22:
	addi r4, r4, 1
	lui r5, %hi(fio_rec-2)
	addi r5, r5, %lo(fio_rec-2)
	addi r6, r0, 13
	jal r0, .LBB180_24
.LBB180_23:
	addi r7, r4, -2
	stw r1+%lo(fio_rlen), r7
	addi r4, r4, -1
	ble r4, r3, .LBB180_26
.LBB180_24:
	add r7, r4, r5
	ldbu r7, r7+0
	beq r7, r6, .LBB180_23
.LBB180_25:
	beq r7, r14, .LBB180_23
.LBB180_26:
	lui r1, %hi(fio_rpos)
	stw r1+%lo(fio_rpos), r0
.LBB180_27:
	add r1, r0, r0
	jal r0, .LBB180_29
.LBB180_28:
	addi r1, r0, 1
.LBB180_29:
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
.LBB180_30:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r3, r3, r1
	addi r1, r0, 1
	stw r3+0, r1
	jal r0, .LBB180_29
.Lfunc_end180:
	.size	fio_getrec, .Lfunc_end180-fio_getrec
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
	ble r1, r3, .LBB181_5
.LBB181_1:
	addi r4, r0, 0
	seq r4, r3, r4
	slli r3, r3, 1
	lui r5, 1
	xor r5, r3, r5
	sub r4, r0, r4
	and r4, r5, r4
	xor r3, r3, r4
.LBB181_2:
	add r13, r3, r0
	slli r3, r3, 1
	blt r13, r1, .LBB181_2
.LBB181_3:
	ldw r3, r15+%lo(nml_text)
	add r4, r13, r0
	jal r31, realloc
	beq r1, r0, .LBB181_6
.LBB181_4:
	stw r15+%lo(nml_text), r1
	stw r16+%lo(nml_tcap), r13
.LBB181_5:
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
.LBB181_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.117)
	addi r4, r4, %lo(.L.str.117)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end181:
	.size	nml_text_add, .Lfunc_end181-nml_text_add
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

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"NaN"
	.size	.L.str.10, 4

	.type	fio_sp,@object                  # @fio_sp
	.local	fio_sp
	.comm	fio_sp,1,4
	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"Infinity"
	.size	.L.str.11, 9

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"Inf"
	.size	.L.str.12, 4

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

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"f77: ALLOCATE of an array already allocated\n"
	.size	.L.str.16, 45

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"f77: ALLOCATE: out of memory (%d bytes)\n"
	.size	.L.str.17, 41

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"f77: DEALLOCATE of an array not allocated\n"
	.size	.L.str.18, 43

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"f77: out of memory copying an ALLOCATABLE component\n"
	.size	.L.str.19, 53

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"f77: out of memory for an array argument (%d bytes)\n"
	.size	.L.str.20, 53

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"f77: NEAREST with S = 0\n"
	.size	.L.str.21, 25

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"f77: MODULO with P = 0\n"
	.size	.L.str.22, 24

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"%04d%02d%02d"
	.size	.L.str.23, 13

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"%02d%02d%02d.%03d"
	.size	.L.str.24, 18

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"+0000"
	.size	.L.str.25, 6

	.type	f77_rng,@object                 # @f77_rng
	.data
	.p2align	2, 0x0
f77_rng:
	.word	2654435769                      # 0x9e3779b9
	.word	608135816                       # 0x243f6a88
	.word	3084996962                      # 0xb7e15162
	.word	2242054355                      # 0x85a308d3
	.size	f77_rng, 16

	.type	.L.str.26,@object               # @.str.26
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.26:
	.asciz	"f77: RANDOM_SEED's PUT and GET have 4 elements at least\n"
	.size	.L.str.26, 57

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"f77: DATE_AND_TIME's VALUES has 8 elements at least\n"
	.size	.L.str.27, 53

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"f77: concatenation result of %d bytes is too long\n"
	.size	.L.str.28, 51

	.type	fio_catpos,@object              # @fio_catpos
	.local	fio_catpos
	.comm	fio_catpos,4,4
	.type	fio_catring,@object             # @fio_catring
	.local	fio_catring
	.comm	fio_catring,65536,1
	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"f77: REPEAT with NCOPIES %d, less than 0\n"
	.size	.L.str.29, 42

	.type	fio_desc,@object                # @fio_desc
	.local	fio_desc
	.comm	fio_desc,4,4
	.type	fio_rep,@object                 # @fio_rep
	.local	fio_rep
	.comm	fio_rep,4,4
	.type	fio_draining,@object            # @fio_draining
	.local	fio_draining
	.comm	fio_draining,1,4
	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"PAUSE %.*s\n"
	.size	.L.str.30, 12

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
	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"f77: %c editing does not match a numeric READ item\n"
	.size	.L.str.31, 52

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"f77: %c editing does not match a CHARACTER READ item\n"
	.size	.L.str.32, 54

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
	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"(I11)"
	.size	.L.str.33, 6

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"T"
	.size	.L.str.34, 2

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"F"
	.size	.L.str.35, 2

	.type	nml_asg,@object                 # @nml_asg
	.local	nml_asg
	.comm	nml_asg,4,4
	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"the subscripts do not match the array"
	.size	.L.str.36, 38

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"a subscript is out of range"
	.size	.L.str.37, 28

	.type	nml_val,@object                 # @nml_val
	.local	nml_val
	.comm	nml_val,4,4
	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"more values than the item has elements"
	.size	.L.str.38, 39

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	" &END"
	.size	.L.str.39, 6

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"%s is no item of the NAMELIST"
	.size	.L.str.40, 30

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"f77: end of file on unit %d looking for NAMELIST &%s\n"
	.size	.L.str.41, 54

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"f77: NAMELIST &%s on unit %d: %s\n"
	.size	.L.str.42, 34

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"f77: cannot OPEN unit %d\n"
	.size	.L.str.43, 26

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"DIRECT"
	.size	.L.str.44, 7

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"UNFORMATTED"
	.size	.L.str.45, 12

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"SCRATCH"
	.size	.L.str.46, 8

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"f77: OPEN ACCESS='DIRECT' needs RECL=\n"
	.size	.L.str.47, 39

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"f77: bad FILE= name in OPEN\n"
	.size	.L.str.48, 29

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"fort.%d"
	.size	.L.str.49, 8

	.type	fio_ufile,@object               # @fio_ufile
	.local	fio_ufile
	.comm	fio_ufile,400,4
	.type	fio_ufname,@object              # @fio_ufname
	.local	fio_ufname
	.comm	fio_ufname,25600,1
	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"ZERO"
	.size	.L.str.50, 5

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"f77: OPEN cannot create a scratch file\n"
	.size	.L.str.51, 40

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"OLD"
	.size	.L.str.52, 4

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"r+"
	.size	.L.str.53, 3

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"r"
	.size	.L.str.54, 2

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"f77: OPEN STATUS='OLD': no such file '%s'\n"
	.size	.L.str.55, 43

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"NEW"
	.size	.L.str.56, 4

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"f77: OPEN STATUS='NEW': '%s' already exists\n"
	.size	.L.str.57, 45

	.type	.L.str.58,@object               # @.str.58
.L.str.58:
	.asciz	"w+"
	.size	.L.str.58, 3

	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"f77: OPEN cannot create '%s'\n"
	.size	.L.str.59, 30

	.type	.L.str.60,@object               # @.str.60
.L.str.60:
	.asciz	"UNKNOWN"
	.size	.L.str.60, 8

	.type	.L.str.61,@object               # @.str.61
.L.str.61:
	.asciz	"f77: OPEN cannot open '%s'\n"
	.size	.L.str.61, 28

	.type	.L.str.62,@object               # @.str.62
.L.str.62:
	.asciz	"f77: OPEN STATUS value is not supported\n"
	.size	.L.str.62, 41

	.type	.L.str.63,@object               # @.str.63
.L.str.63:
	.asciz	"DELETE"
	.size	.L.str.63, 7

	.type	.L.str.64,@object               # @.str.64
.L.str.64:
	.asciz	"KEEP"
	.size	.L.str.64, 5

	.type	.L.str.65,@object               # @.str.65
.L.str.65:
	.asciz	"f77: CLOSE STATUS value is not supported\n"
	.size	.L.str.65, 42

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
	.type	.L.str.66,@object               # @.str.66
.L.str.66:
	.asciz	"stdin"
	.size	.L.str.66, 6

	.type	.L.str.67,@object               # @.str.67
.L.str.67:
	.asciz	"stdout"
	.size	.L.str.67, 7

	.type	.L.str.68,@object               # @.str.68
.L.str.68:
	.asciz	"stderr"
	.size	.L.str.68, 7

	.type	.L.str.69,@object               # @.str.69
.L.str.69:
	.asciz	"UNDEFINED"
	.size	.L.str.69, 10

	.type	.L.str.70,@object               # @.str.70
.L.str.70:
	.asciz	"SEQUENTIAL"
	.size	.L.str.70, 11

	.type	.L.str.71,@object               # @.str.71
.L.str.71:
	.asciz	"NO"
	.size	.L.str.71, 3

	.type	.L.str.72,@object               # @.str.72
.L.str.72:
	.asciz	"YES"
	.size	.L.str.72, 4

	.type	.L.str.73,@object               # @.str.73
.L.str.73:
	.asciz	"FORMATTED"
	.size	.L.str.73, 10

	.type	.L.str.74,@object               # @.str.74
.L.str.74:
	.asciz	"NULL"
	.size	.L.str.74, 5

	.type	.L.str.75,@object               # @.str.75
.L.str.75:
	.asciz	"f77: REWIND: unit %d cannot be rewound\n"
	.size	.L.str.75, 40

	.type	.L.str.76,@object               # @.str.76
.L.str.76:
	.asciz	"f77: an I/O statement in a function referenced in another's I/O list\n"
	.size	.L.str.76, 70

	.type	fio_uend_init,@object           # @fio_uend_init
	.local	fio_uend_init
	.comm	fio_uend_init,1,4
	.type	.L.str.77,@object               # @.str.77
.L.str.77:
	.asciz	"f77: unit %d is not open for %s\n"
	.size	.L.str.77, 33

	.type	.L.str.78,@object               # @.str.78
.L.str.78:
	.asciz	"WRITE"
	.size	.L.str.78, 6

	.type	.L.str.79,@object               # @.str.79
.L.str.79:
	.asciz	"READ"
	.size	.L.str.79, 5

	.type	.L.str.80,@object               # @.str.80
.L.str.80:
	.asciz	"f77: a direct-access unit needs REC=\n"
	.size	.L.str.80, 38

	.type	.L.str.81,@object               # @.str.81
.L.str.81:
	.asciz	"f77: READ past the end of a direct-access file\n"
	.size	.L.str.81, 48

	.type	.L.str.82,@object               # @.str.82
.L.str.82:
	.asciz	"f77: end of file on READ\n"
	.size	.L.str.82, 26

	.type	.L.str.83,@object               # @.str.83
.L.str.83:
	.asciz	"f77: bad unformatted record\n"
	.size	.L.str.83, 29

	.type	fio_len,@object                 # @fio_len
	.local	fio_len
	.comm	fio_len,4,4
	.type	fio_col,@object                 # @fio_col
	.local	fio_col
	.comm	fio_col,4,4
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
	.type	.L.str.84,@object               # @.str.84
.L.str.84:
	.asciz	"a format begins with ("
	.size	.L.str.84, 23

	.type	fio_fmt_why,@object             # @fio_fmt_why
	.local	fio_fmt_why
	.comm	fio_fmt_why,4,4
	.type	.L.str.85,@object               # @.str.85
.L.str.85:
	.asciz	"the format is too long, or ends without its )"
	.size	.L.str.85, 46

	.type	.L.str.86,@object               # @.str.86
.L.str.86:
	.asciz	"the format ends without its )"
	.size	.L.str.86, 30

	.type	.L.str.87,@object               # @.str.87
.L.str.87:
	.asciz	"a quoted string runs past the end of the format"
	.size	.L.str.87, 48

	.type	.L.str.88,@object               # @.str.88
.L.str.88:
	.asciz	"the format is too long"
	.size	.L.str.88, 23

	.type	.L.str.89,@object               # @.str.89
.L.str.89:
	.asciz	"0H is no Hollerith string"
	.size	.L.str.89, 26

	.type	.L.str.90,@object               # @.str.90
.L.str.90:
	.asciz	"nH wants more characters than the format has"
	.size	.L.str.90, 45

	.type	fio_fmt_check.why,@object       # @fio_fmt_check.why
	.local	fio_fmt_check.why
	.comm	fio_fmt_check.why,64,1
	.type	.L.str.91,@object               # @.str.91
.L.str.91:
	.asciz	"a sign belongs to a scale factor, nP"
	.size	.L.str.91, 37

	.type	.L.str.92,@object               # @.str.92
.L.str.92:
	.asciz	"H needs its count, nH"
	.size	.L.str.92, 22

	.type	.L.str.93,@object               # @.str.93
.L.str.93:
	.asciz	"T, TL and TR take a column"
	.size	.L.str.93, 27

	.type	.L.str.94,@object               # @.str.94
.L.str.94:
	.asciz	"B is BN or BZ"
	.size	.L.str.94, 14

	.type	.L.str.95,@object               # @.str.95
.L.str.95:
	.asciz	"%c needs a width"
	.size	.L.str.95, 17

	.type	.L.str.96,@object               # @.str.96
.L.str.96:
	.asciz	"a . needs digits after it"
	.size	.L.str.96, 26

	.type	.L.str.97,@object               # @.str.97
.L.str.97:
	.asciz	"Ee needs its digits"
	.size	.L.str.97, 20

	.type	.L.str.98,@object               # @.str.98
.L.str.98:
	.asciz	"a count with no descriptor after it"
	.size	.L.str.98, 36

	.type	.L.str.99,@object               # @.str.99
.L.str.99:
	.asciz	"a count before something that is no edit descriptor"
	.size	.L.str.99, 52

	.type	.L.str.100,@object              # @.str.100
.L.str.100:
	.asciz	"%c is no edit descriptor"
	.size	.L.str.100, 25

	.type	.L.str.101,@object              # @.str.101
.L.str.101:
	.asciz	"f77: FORMAT: %s\n  in: %.*s\n"
	.size	.L.str.101, 28

	.type	fio_line,@object                # @fio_line
	.local	fio_line
	.comm	fio_line,1024,1
	.type	.L.str.102,@object              # @.str.102
.L.str.102:
	.asciz	"-Infinity"
	.size	.L.str.102, 10

	.type	.L.str.103,@object              # @.str.103
.L.str.103:
	.asciz	"%%.%dE"
	.size	.L.str.103, 7

	.type	.L.str.104,@object              # @.str.104
.L.str.104:
	.asciz	"%.17E"
	.size	.L.str.104, 6

	.type	.L.str.105,@object              # @.str.105
.L.str.105:
	.asciz	"f77: out of memory for an array constructor\n"
	.size	.L.str.105, 45

	.type	.L.str.106,@object              # @.str.106
.L.str.106:
	.asciz	"f77: a CHARACTER result of %d bytes is too long\n"
	.size	.L.str.106, 49

	.type	fio_gcount,@object              # @fio_gcount
	.local	fio_gcount
	.comm	fio_gcount,64,4
	.type	fio_gstart,@object              # @fio_gstart
	.local	fio_gstart
	.comm	fio_gstart,64,4
	.type	.L.str.107,@object              # @.str.107
.L.str.107:
	.asciz	"f77: FORMAT: no data edit descriptor for the items left\n"
	.size	.L.str.107, 57

	.type	.L.str.108,@object              # @.str.108
.L.str.108:
	.asciz	"f77: WRITE past the end of an internal file\n"
	.size	.L.str.108, 45

	.type	.L.str.109,@object              # @.str.109
.L.str.109:
	.asciz	"f77: record longer than the internal file (%d > %d)\n"
	.size	.L.str.109, 53

	.type	.L.str.110,@object              # @.str.110
.L.str.110:
	.asciz	"f77: record longer than RECL (%d > %d)\n"
	.size	.L.str.110, 40

	.type	fio_l_nulls,@object             # @fio_l_nulls
	.local	fio_l_nulls
	.comm	fio_l_nulls,4,4
	.type	fio_l_after,@object             # @fio_l_after
	.local	fio_l_after
	.comm	fio_l_after,1,4
	.type	fio_l_pending,@object           # @fio_l_pending
	.local	fio_l_pending
	.comm	fio_l_pending,4,4
	.type	.L.str.111,@object              # @.str.111
.L.str.111:
	.asciz	"f77: zero repeat count in list-directed input\n"
	.size	.L.str.111, 47

	.type	.L.str.112,@object              # @.str.112
.L.str.112:
	.asciz	"f77: bad LOGICAL input field '%s'\n"
	.size	.L.str.112, 35

	.type	.L.str.113,@object              # @.str.113
.L.str.113:
	.asciz	"E%d"
	.size	.L.str.113, 4

	.type	.L.str.114,@object              # @.str.114
.L.str.114:
	.asciz	"(A)"
	.size	.L.str.114, 4

	.type	nml_tlen,@object                # @nml_tlen
	.local	nml_tlen
	.comm	nml_tlen,4,4
	.type	.L.str.115,@object              # @.str.115
.L.str.115:
	.asciz	"the record could not be read"
	.size	.L.str.115, 29

	.type	.L.str.116,@object              # @.str.116
.L.str.116:
	.asciz	" "
	.size	.L.str.116, 2

	.type	nml_tcap,@object                # @nml_tcap
	.local	nml_tcap
	.comm	nml_tcap,4,4
	.type	nml_text,@object                # @nml_text
	.local	nml_text
	.comm	nml_text,4,4
	.type	.L.str.117,@object              # @.str.117
.L.str.117:
	.asciz	"f77: out of storage for a NAMELIST\n"
	.size	.L.str.117, 36

	.type	.L.str.118,@object              # @.str.118
.L.str.118:
	.asciz	"a subscript is no integer"
	.size	.L.str.118, 26

	.type	.L.str.119,@object              # @.str.119
.L.str.119:
	.asciz	"a value before the first NAME="
	.size	.L.str.119, 31

	.type	.L.str.120,@object              # @.str.120
.L.str.120:
	.asciz	"a repeat count is not positive"
	.size	.L.str.120, 31

	.type	nml_cval,@object                # @nml_cval
	.local	nml_cval
	.comm	nml_cval,4,4
	.type	nml_casg,@object                # @nml_casg
	.local	nml_casg
	.comm	nml_casg,4,4
	.type	nml_pend,@object                # @nml_pend
	.local	nml_pend
	.comm	nml_pend,1024,1
	.type	.L.str.121,@object              # @.str.121
.L.str.121:
	.asciz	"0.0"
	.size	.L.str.121, 4

	.type	.L.str.122,@object              # @.str.122
.L.str.122:
	.asciz	"(G23.16)"
	.size	.L.str.122, 9

	.type	.L.str.123,@object              # @.str.123
.L.str.123:
	.asciz	"(G15.8)"
	.size	.L.str.123, 8

	.type	.L.str.124,@object              # @.str.124
.L.str.124:
	.asciz	"a LOGICAL value is not T or F"
	.size	.L.str.124, 30

	.type	.L.str.125,@object              # @.str.125
.L.str.125:
	.asciz	"a CHARACTER value is not in quotes"
	.size	.L.str.125, 35

	.type	.L.str.126,@object              # @.str.126
.L.str.126:
	.asciz	"a value could not be read"
	.size	.L.str.126, 26

	.type	.Lswitch.table.fio_fmt_use,@object # @switch.table.fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.Lswitch.table.fio_fmt_use:
	.word	.L.str.98
	.word	.L.str.99
	.word	.L.str.99
	.word	.L.str.98
	.word	.L.str.99
	.word	.L.str.99
	.word	.L.str.98
	.word	.L.str.99
	.word	.L.str.99
	.word	.L.str.99
	.word	.L.str.99
	.word	.L.str.99
	.word	.L.str.99
	.word	.L.str.99
	.word	.L.str.99
	.word	.L.str.99
	.word	.L.str.99
	.word	.L.str.98
	.size	.Lswitch.table.fio_fmt_use, 72

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git a06e1a6e35103a79b1ae3c6830426b277cc25690)"
	.section	".note.GNU-stack","",@progbits
