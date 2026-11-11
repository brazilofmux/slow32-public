	.file	"libf77.c"
	.text
	.globl	f77_io_adv                      # -- Begin function f77_io_adv
	.p2align	2
	.type	f77_io_adv,@function
f77_io_adv:                             # @f77_io_adv
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	add r11, r5, r0
	addi r12, r0, 1
	addi r5, r0, 32
	add r1, r0, r0
	blt r4, r12, .LBB0_4
.LBB0_1:
	add r6, r3, r1
	ldbu r6, r6+0
	bne r6, r5, .LBB0_4
.LBB0_2:
	addi r1, r1, 1
	bne r4, r1, .LBB0_1
.LBB0_3:
	add r1, r4, r0
.LBB0_4:
	ble r4, r1, .LBB0_9
.LBB0_5:
	addi r6, r3, -1
.LBB0_6:
	add r7, r6, r4
	ldbu r7, r7+0
	bne r7, r5, .LBB0_9
.LBB0_7:
	addi r4, r4, -1
	bgt r4, r1, .LBB0_6
.LBB0_8:
	add r4, r1, r0
.LBB0_9:
	sub r4, r4, r1
	addi r5, r0, 3
	beq r4, r5, .LBB0_13
.LBB0_10:
	addi r5, r0, 2
	bne r4, r5, .LBB0_18
.LBB0_11:
	add r13, r3, r1
	ldbu r3, r13+0
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB0_18
.LBB0_12:
	ldbu r3, r13+1
	jal r31, toupper
	addi r3, r0, 79
	beq r1, r3, .LBB0_17
	jal r0, .LBB0_18
.LBB0_13:
	add r12, r3, r1
	ldbu r3, r12+0
	jal r31, toupper
	addi r3, r0, 89
	bne r1, r3, .LBB0_18
.LBB0_14:
	ldbu r3, r12+1
	jal r31, toupper
	addi r3, r0, 69
	bne r1, r3, .LBB0_18
.LBB0_15:
	ldbu r3, r12+2
	jal r31, toupper
	addi r3, r0, 83
	bne r1, r3, .LBB0_18
.LBB0_16:
	add r12, r0, r0
.LBB0_17:
	lui r1, %hi(fio_noadv)
	stb r1+%lo(fio_noadv), r12
	lui r1, %hi(fio_sizep)
	stw r1+%lo(fio_sizep), r11
	lui r1, %hi(fio_eor)
	stb r1+%lo(fio_eor), r0
	lui r1, %hi(fio_size)
	stw r1+%lo(fio_size), r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB0_18:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end0:
	.size	f77_io_adv, .Lfunc_end0-f77_io_adv
                                        # -- End function
	.globl	f77_io_eor                      # -- Begin function f77_io_eor
	.p2align	2
	.type	f77_io_eor,@function
f77_io_eor:                             # @f77_io_eor
# %bb.0:
	lui r3, %hi(fio_eor)
	ldbu r1, r3+%lo(fio_eor)
	stb r3+%lo(fio_eor), r0
	jalr r0, r31, 0
.Lfunc_end1:
	.size	f77_io_eor, .Lfunc_end1-f77_io_eor
                                        # -- End function
	.globl	f77_set_rec                     # -- Begin function f77_set_rec
	.p2align	2
	.type	f77_set_rec,@function
f77_set_rec:                            # @f77_set_rec
# %bb.0:
	lui r1, %hi(fio_rec_next)
	stw r1+%lo(fio_rec_next), r3
	jalr r0, r31, 0
.Lfunc_end2:
	.size	f77_set_rec, .Lfunc_end2-f77_set_rec
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
	beq r4, r1, .LBB3_28
.LBB3_1:
	add r11, r5, r0
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
	addi r6, r14, -1
	addi r7, r0, 98
	slli r3, r14, 2
	bgtu r6, r7, .LBB3_7
.LBB3_6:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r4, r3, r4
	stw r4+0, r0
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r4, r3, r4
	stw r4+0, r0
.LBB3_7:
	addi r4, r14, -100
	addi r5, r0, -99
	bltu r4, r5, .LBB3_10
.LBB3_8:
	lui r8, %hi(fio_ufile)
	addi r8, r8, %lo(fio_ufile)
	add r8, r3, r8
	ldw r8, r8+0
	beq r8, r0, .LBB3_10
.LBB3_9:
	lui r8, %hi(fio_uaction)
	addi r8, r8, %lo(fio_uaction)
	add r8, r3, r8
	ldw r8, r8+0
	beq r8, r13, .LBB3_29
.LBB3_10:
	bgtu r6, r7, .LBB3_14
.LBB3_11:
	lui r6, %hi(fio_ufile)
	addi r6, r6, %lo(fio_ufile)
	add r6, r3, r6
	ldw r6, r6+0
	beq r6, r0, .LBB3_14
.LBB3_12:
	lui r6, %hi(fio_udirect)
	addi r6, r6, %lo(fio_udirect)
	add r6, r3, r6
	ldw r6, r6+0
	bne r6, r0, .LBB3_14
.LBB3_13:
	lui r6, %hi(fio_uwrote)
	addi r6, r6, %lo(fio_uwrote)
	add r6, r3, r6
	stw r6+0, r13
.LBB3_14:
	lui r7, %hi(fio_rec_next)
	ldw r6, r7+%lo(fio_rec_next)
	stw r7+%lo(fio_rec_next), r0
	bltu r4, r5, .LBB3_18
.LBB3_15:
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r3, r4
	ldw r4, r4+0
	beq r4, r0, .LBB3_18
.LBB3_16:
	ble r6, r0, .LBB3_31
.LBB3_17:
	addi r4, r6, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB3_18:
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
	beq r12, r0, .LBB3_25
.LBB3_19:
	stw r1+%lo(fio_listed), r0
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fio_fmt_use
	addi r3, r0, -1
	ble r1, r3, .LBB3_30
.LBB3_20:
	stw r14+%lo(fio_pos), r0
	stw r15+%lo(fio_revert), r3
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	lui r1, %hi(fio_fmt)
	ldw r3, r1+%lo(fio_fmt)
	addi r4, r0, 32
	add r1, r0, r0
.LBB3_21:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB3_23
.LBB3_22:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB3_21
.LBB3_23:
	addi r3, r0, 40
	bne r5, r3, .LBB3_27
.LBB3_24:
	addi r3, r1, 1
	add r1, r3, r0
	jal r0, .LBB3_26
.LBB3_25:
	stw r1+%lo(fio_listed), r13
	lui r1, %hi(.L.str.8)
	addi r1, r1, %lo(.L.str.8)
	lui r3, %hi(fio_fmt)
	stw r3+%lo(fio_fmt), r1
	addi r1, r0, -1
	add r3, r0, r0
.LBB3_26:
	stw r14+%lo(fio_pos), r3
	stw r15+%lo(fio_revert), r1
.LBB3_27:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB3_28:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r0, .LBB3_32
.LBB3_29:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.130)
	addi r4, r4, %lo(.L.str.130)
	lui r5, %hi(.L.str.131)
	addi r5, r5, %lo(.L.str.131)
	lui r7, %hi(.L.str.72)
	addi r7, r7, %lo(.L.str.72)
	add r6, r14, r0
	jal r31, fprintf
.LBB3_30:
	addi r3, r0, 2
	jal r31, exit
.LBB3_31:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.98)
	addi r4, r4, %lo(.L.str.98)
.LBB3_32:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end3:
	.size	f77_wr_begin, .Lfunc_end3-f77_wr_begin
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
	bgtu r3, r1, .LBB4_2
.LBB4_1:
	slli r1, r11, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r1, r1, r3
	ldw r1, r1+0
	bne r1, r0, .LBB4_16
.LBB4_2:
	beq r4, r0, .LBB4_7
.LBB4_3:
	beq r11, r0, .LBB4_15
.LBB4_4:
	addi r1, r0, 5
	beq r11, r1, .LBB4_6
.LBB4_5:
	addi r1, r0, 6
	bne r11, r1, .LBB4_10
.LBB4_6:
	lui r1, %hi(stdout)
	ldw r1, r1+%lo(stdout)
	jal r0, .LBB4_16
.LBB4_7:
	addi r1, r0, 5
	beq r11, r1, .LBB4_9
.LBB4_8:
	bne r11, r0, .LBB4_10
.LBB4_9:
	lui r1, %hi(stdin)
	ldw r1, r1+%lo(stdin)
	jal r0, .LBB4_16
.LBB4_10:
	add r13, r4, r0
	addi r1, r11, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB4_17
.LBB4_11:
	lui r5, %hi(.L.str.57)
	addi r5, r5, %lo(.L.str.57)
	addi r12, sp, 20
	addi r4, r0, 256
	add r3, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	lui r4, %hi(.L.str.61)
	addi r4, r4, %lo(.L.str.61)
	add r3, r12, r0
	jal r31, fopen
	bne r1, r0, .LBB4_13
.LBB4_12:
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	addi r3, sp, 20
	jal r31, fopen
.LBB4_13:
	beq r1, r0, .LBB4_17
.LBB4_14:
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
	jal r0, .LBB4_16
.LBB4_15:
	lui r1, %hi(stderr)
	ldw r1, r1+%lo(stderr)
.LBB4_16:
	ldw r13, sp+276
	ldw r12, sp+280
	ldw r11, sp+284
	ldw lr, sp+0
	addi sp, sp, 288
	jalr r0, r31, 0
.LBB4_17:
	addi r1, r0, 0
	seq r1, r13, r1
	lui r3, %hi(stderr)
	ldw r3, r3+%lo(stderr)
	lui r4, %hi(.L.str.73)
	addi r4, r4, %lo(.L.str.73)
	lui r5, %hi(.L.str.72)
	addi r5, r5, %lo(.L.str.72)
	xor r5, r5, r4
	sub r1, r0, r1
	and r1, r5, r1
	xor r6, r4, r1
	lui r4, %hi(.L.str.97)
	addi r4, r4, %lo(.L.str.97)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end4:
	.size	fio_resolve, .Lfunc_end4-fio_resolve
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
	beq r4, r1, .LBB5_19
.LBB5_1:
	stb r3+%lo(fio_busy), r1
	lui r3, %hi(fio_uend_init)
	ldbu r4, r3+%lo(fio_uend_init)
	bne r4, r0, .LBB5_5
.LBB5_2:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
	addi r6, r0, -1
.LBB5_3:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r0, .LBB5_3
.LBB5_4:
	stb r3+%lo(fio_uend_init), r1
.LBB5_5:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_resolve
	lui r3, %hi(fio_out)
	stw r3+%lo(fio_out), r1
	lui r3, %hi(fio_unit)
	stw r3+%lo(fio_unit), r11
	addi r6, r11, -1
	addi r7, r0, 98
	slli r3, r11, 2
	bgtu r6, r7, .LBB5_7
.LBB5_6:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r4, r3, r4
	stw r4+0, r12
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r4, r3, r4
	stw r4+0, r0
.LBB5_7:
	addi r4, r11, -100
	addi r5, r0, -99
	bltu r4, r5, .LBB5_10
.LBB5_8:
	lui r8, %hi(fio_ufile)
	addi r8, r8, %lo(fio_ufile)
	add r8, r3, r8
	ldw r8, r8+0
	beq r8, r0, .LBB5_10
.LBB5_9:
	lui r8, %hi(fio_uaction)
	addi r8, r8, %lo(fio_uaction)
	add r8, r3, r8
	ldw r8, r8+0
	beq r8, r12, .LBB5_20
.LBB5_10:
	bgtu r6, r7, .LBB5_14
.LBB5_11:
	lui r6, %hi(fio_ufile)
	addi r6, r6, %lo(fio_ufile)
	add r6, r3, r6
	ldw r6, r6+0
	beq r6, r0, .LBB5_14
.LBB5_12:
	lui r6, %hi(fio_udirect)
	addi r6, r6, %lo(fio_udirect)
	add r6, r3, r6
	ldw r6, r6+0
	bne r6, r0, .LBB5_14
.LBB5_13:
	lui r6, %hi(fio_uwrote)
	addi r6, r6, %lo(fio_uwrote)
	add r6, r3, r6
	stw r6+0, r12
.LBB5_14:
	lui r7, %hi(fio_rec_next)
	ldw r6, r7+%lo(fio_rec_next)
	stw r7+%lo(fio_rec_next), r0
	bltu r4, r5, .LBB5_18
.LBB5_15:
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r3, r4
	ldw r4, r4+0
	beq r4, r0, .LBB5_18
.LBB5_16:
	ble r6, r0, .LBB5_21
.LBB5_17:
	addi r4, r6, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB5_18:
	lui r1, %hi(fio_ulen)
	stw r1+%lo(fio_ulen), r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB5_19:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r0, .LBB5_22
.LBB5_20:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.130)
	addi r4, r4, %lo(.L.str.130)
	lui r5, %hi(.L.str.131)
	addi r5, r5, %lo(.L.str.131)
	lui r7, %hi(.L.str.72)
	addi r7, r7, %lo(.L.str.72)
	add r6, r11, r0
	jal r0, .LBB5_22
.LBB5_21:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.98)
	addi r4, r4, %lo(.L.str.98)
.LBB5_22:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end5:
	.size	f77_wu_begin, .Lfunc_end5-f77_wu_begin
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
	blt r4, r1, .LBB6_3
.LBB6_1:
	add r4, r3, r0
	lui r12, %hi(fio_ulen)
	ldw r1, r12+%lo(fio_ulen)
	add r3, r1, r11
	lui r5, 16
	addi r5, r5, 1
	bge r3, r5, .LBB6_4
.LBB6_2:
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	add r3, r1, r3
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r12+%lo(fio_ulen)
	add r1, r1, r11
	stw r12+%lo(fio_ulen), r1
.LBB6_3:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB6_4:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end6:
	.size	f77_wu, .Lfunc_end6-f77_wu
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
	bgtu r3, r17, .LBB7_5
.LBB7_1:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB7_5
.LBB7_2:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r12, r1+0
	lui r13, %hi(fio_ulen)
	ldw r5, r13+%lo(fio_ulen)
	bgt r5, r12, .LBB7_8
.LBB7_3:
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	ldw r1, r13+%lo(fio_ulen)
	bge r1, r12, .LBB7_7
.LBB7_4:
	add r3, r0, r0
	add r4, r11, r0
	jal r31, fputc
	ldw r1, r13+%lo(fio_ulen)
	addi r1, r1, 1
	stw r13+%lo(fio_ulen), r1
	blt r1, r12, .LBB7_4
	jal r0, .LBB7_7
.LBB7_5:
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
	bgtu r1, r17, .LBB7_7
.LBB7_6:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r16+%lo(fio_unit)
	slli r3, r3, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
.LBB7_7:
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
.LBB7_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end7:
	.size	f77_wu_end, .Lfunc_end7-f77_wu_end
                                        # -- End function
	.globl	f77_ru_begin                    # -- Begin function f77_ru_begin
	.p2align	2
	.type	f77_ru_begin,@function
f77_ru_begin:                           # @f77_ru_begin
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
	lui r15, %hi(fio_busy)
	ldbu r1, r15+%lo(fio_busy)
	addi r12, r0, 1
	beq r1, r12, .LBB8_32
.LBB8_1:
	add r11, r4, r0
	add r14, r3, r0
	stb r15+%lo(fio_busy), r12
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB8_5
.LBB8_2:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB8_3:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB8_3
.LBB8_4:
	stb r1+%lo(fio_uend_init), r12
.LBB8_5:
	add r3, r14, r0
	add r4, r0, r0
	jal r31, fio_resolve
	add r13, r1, r0
	addi r1, r14, -100
	addi r3, r0, -99
	slli r16, r14, 2
	bltu r1, r3, .LBB8_8
.LBB8_6:
	lui r4, %hi(fio_ufile)
	addi r4, r4, %lo(fio_ufile)
	add r4, r16, r4
	ldw r4, r4+0
	beq r4, r0, .LBB8_8
.LBB8_7:
	lui r4, %hi(fio_uaction)
	addi r4, r4, %lo(fio_uaction)
	add r4, r16, r4
	ldw r4, r4+0
	addi r5, r0, 2
	beq r4, r5, .LBB8_34
.LBB8_8:
	addi r17, r14, -1
	addi r18, r0, 98
	bgtu r17, r18, .LBB8_10
.LBB8_9:
	lui r4, %hi(fio_uwrote)
	addi r4, r4, %lo(fio_uwrote)
	add r4, r16, r4
	stw r4+0, r0
	lui r4, %hi(fio_in)
	stw r4+%lo(fio_in), r13
	lui r4, %hi(fio_unit)
	stw r4+%lo(fio_unit), r14
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r4, r16, r4
	addi r5, r0, 1
	stw r4+0, r5
	jal r0, .LBB8_11
.LBB8_10:
	lui r4, %hi(fio_in)
	stw r4+%lo(fio_in), r13
	lui r4, %hi(fio_unit)
	stw r4+%lo(fio_unit), r14
.LBB8_11:
	lui r5, %hi(fio_rec_next)
	ldw r4, r5+%lo(fio_rec_next)
	stw r5+%lo(fio_rec_next), r0
	bltu r1, r3, .LBB8_15
.LBB8_12:
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r16, r1
	ldw r1, r1+0
	beq r1, r0, .LBB8_15
.LBB8_13:
	ble r4, r0, .LBB8_35
.LBB8_14:
	addi r1, r4, -1
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r16, r3
	ldw r3, r3+0
	mul r4, r3, r1
	add r3, r13, r0
	add r5, r0, r0
	jal r31, fseek
.LBB8_15:
	lui r1, %hi(fio_upos)
	stw r1+%lo(fio_upos), r0
	bgtu r17, r18, .LBB8_20
.LBB8_16:
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r16, r1
	ldw r1, r1+0
	beq r1, r0, .LBB8_20
.LBB8_17:
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
	bne r1, r0, .LBB8_27
.LBB8_18:
	andi r1, r11, 2
	bne r1, r0, .LBB8_31
.LBB8_19:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.99)
	addi r4, r4, %lo(.L.str.99)
	jal r0, .LBB8_37
.LBB8_20:
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r14, r16, r1
	ldw r1, r14+0
	blt r1, r0, .LBB8_22
.LBB8_21:
	add r3, r13, r0
	jal r31, ftell
	ldw r3, r14+0
	bge r1, r3, .LBB8_26
.LBB8_22:
	addi r3, sp, 28
	addi r12, r0, 1
	addi r14, r0, 4
	add r4, r12, r0
	add r5, r14, r0
	add r6, r13, r0
	jal r31, fread
	bne r1, r14, .LBB8_26
.LBB8_23:
	addi r3, sp, 28
	addi r1, r3, 3
	ldbu r1, r1+0
	slli r1, r1, 24
	blt r1, r0, .LBB8_30
.LBB8_24:
	ldhu r4, sp+28
	addi r3, r3, 2
	ldbu r3, r3+0
	slli r3, r3, 16
	or  r3, r4, r3
	or  r5, r3, r1
	lui r1, 16
	addi r1, r1, 1
	bge r5, r1, .LBB8_30
.LBB8_25:
	lui r3, %hi(fio_urec)
	addi r3, r3, %lo(fio_urec)
	addi r11, r0, 1
	add r4, r11, r0
	add r6, r13, r0
	jal r31, fread
	lui r3, %hi(fio_ulen)
	stw r3+%lo(fio_ulen), r1
	addi r3, sp, 28
	addi r5, r0, 4
	add r4, r11, r0
	add r6, r13, r0
	jal r31, fread
	add r12, r0, r0
	bne r12, r0, .LBB8_28
	jal r0, .LBB8_29
.LBB8_26:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r16, r1
	stw r1+0, r12
	andi r1, r11, 3
	beq r1, r0, .LBB8_33
.LBB8_27:
	beq r12, r0, .LBB8_29
.LBB8_28:
	stb r15+%lo(fio_busy), r0
.LBB8_29:
	add r1, r12, r0
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
.LBB8_30:
	andi r1, r11, 2
	beq r1, r0, .LBB8_36
.LBB8_31:
	addi r12, r0, 2
	bne r12, r0, .LBB8_28
	jal r0, .LBB8_29
.LBB8_32:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r0, .LBB8_37
.LBB8_33:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
	jal r0, .LBB8_37
.LBB8_34:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.130)
	addi r4, r4, %lo(.L.str.130)
	lui r5, %hi(.L.str.132)
	addi r5, r5, %lo(.L.str.132)
	lui r7, %hi(.L.str.73)
	addi r7, r7, %lo(.L.str.73)
	add r6, r14, r0
	jal r0, .LBB8_37
.LBB8_35:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.98)
	addi r4, r4, %lo(.L.str.98)
	jal r0, .LBB8_37
.LBB8_36:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.101)
	addi r4, r4, %lo(.L.str.101)
.LBB8_37:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end8:
	.size	f77_ru_begin, .Lfunc_end8-f77_ru_begin
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
	blt r4, r1, .LBB9_3
.LBB9_1:
	add r11, r4, r0
	lui r12, %hi(fio_upos)
	ldw r1, r12+%lo(fio_upos)
	add r4, r1, r4
	lui r5, %hi(fio_ulen)
	ldw r5, r5+%lo(fio_ulen)
	bgt r4, r5, .LBB9_4
.LBB9_2:
	lui r4, %hi(fio_urec)
	addi r4, r4, %lo(fio_urec)
	add r4, r1, r4
	add r5, r11, r0
	jal r31, memcpy
	ldw r1, r12+%lo(fio_upos)
	add r1, r1, r11
	stw r12+%lo(fio_upos), r1
.LBB9_3:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB9_4:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.3)
	addi r4, r4, %lo(.L.str.3)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end9:
	.size	f77_ru, .Lfunc_end9-f77_ru
                                        # -- End function
	.globl	f77_ru_end                      # -- Begin function f77_ru_end
	.p2align	2
	.type	f77_ru_end,@function
f77_ru_end:                             # @f77_ru_end
# %bb.0:
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	jalr r0, r31, 0
.Lfunc_end10:
	.size	f77_ru_end, .Lfunc_end10-f77_ru_end
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
	addi r1, r3, -1
	addi r4, r0, 99
	sltu r1, r1, r4
	sub r1, r0, r1
	and r1, r3, r1
	slli r1, r1, 2
	lui r4, %hi(fio_pend)
	addi r4, r4, %lo(fio_pend)
	add r1, r1, r4
	stw r1+0, r0
	lui r1, %hi(fio_uend_init)
	ldbu r4, r1+%lo(fio_uend_init)
	bne r4, r0, .LBB11_4
.LBB11_1:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
	addi r6, r0, -1
.LBB11_2:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r0, .LBB11_2
.LBB11_3:
	addi r4, r0, 1
	stb r1+%lo(fio_uend_init), r4
.LBB11_4:
	addi r1, r3, -100
	addi r4, r0, -99
	bltu r1, r4, .LBB11_10
.LBB11_5:
	addi r1, r3, -5
	addi r4, r0, 2
	bltu r1, r4, .LBB11_10
.LBB11_6:
	addi r12, r0, 1
	add r13, r3, r0
	add r4, r12, r0
	jal r31, fio_resolve
	add r11, r1, r0
	slli r14, r13, 2
	lui r1, %hi(fio_uwrote)
	addi r1, r1, %lo(fio_uwrote)
	add r1, r14, r1
	ldw r3, r1+0
	beq r3, r0, .LBB11_8
.LBB11_7:
	stw r1+0, r0
	add r3, r11, r0
	jal r31, fflush
	add r3, r11, r0
	jal r31, fileno
	add r13, r1, r0
	add r3, r11, r0
	jal r31, ftell
	add r3, r13, r0
	add r4, r1, r0
	jal r31, ftruncate
.LBB11_8:
	add r3, r11, r0
	jal r31, fflush
	add r3, r11, r0
	jal r31, ftell
	lui r3, %hi(fio_uaeof)
	addi r3, r3, %lo(fio_uaeof)
	add r3, r14, r3
	ldw r4, r3+0
	beq r4, r0, .LBB11_11
.LBB11_9:
	stw r3+0, r0
.LBB11_10:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB11_11:
	blt r1, r12, .LBB11_10
.LBB11_12:
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r14, r3
	ldw r3, r3+0
	beq r3, r0, .LBB11_16
.LBB11_13:
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
	bne r1, r12, .LBB11_15
.LBB11_14:
	ldw r1, sp+24
	sub r1, r13, r1
	addi r4, r1, -8
.LBB11_15:
	add r3, r11, r0
	add r5, r0, r0
	jal r31, fseek
	jal r0, .LBB11_10
.LBB11_16:
	addi r13, r1, -1
	addi r15, r0, 10
.LBB11_17:
	blt r13, r12, .LBB11_19
.LBB11_18:
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
	bne r1, r15, .LBB11_17
	jal r0, .LBB11_20
.LBB11_19:
	add r13, r0, r0
.LBB11_20:
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
.Lfunc_end11:
	.size	f77_backspace, .Lfunc_end11-f77_backspace
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
	beq r9, r1, .LBB12_15
.LBB12_1:
	stb r8+%lo(fio_busy), r1
	lui r8, %hi(fio_uend_init)
	ldbu r9, r8+%lo(fio_uend_init)
	bne r9, r0, .LBB12_5
.LBB12_2:
	addi r9, r0, -400
	lui r10, %hi(fio_uend+400)
	addi r10, r10, %lo(fio_uend+400)
	addi lr, r0, -1
.LBB12_3:
	add r11, r9, r10
	stw r11+0, lr
	addi r9, r9, 4
	bne r9, r0, .LBB12_3
.LBB12_4:
	stb r8+%lo(fio_uend_init), r1
.LBB12_5:
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
	beq r6, r0, .LBB12_12
.LBB12_6:
	lui r1, %hi(fio_listed)
	stw r1+%lo(fio_listed), r0
	add r3, r6, r0
	add r4, r7, r0
	jal r31, fio_fmt_use
	ble r1, r11, .LBB12_16
.LBB12_7:
	stw r12+%lo(fio_pos), r0
	stw r13+%lo(fio_revert), r11
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	lui r1, %hi(fio_fmt)
	ldw r3, r1+%lo(fio_fmt)
	addi r4, r0, 32
	add r1, r0, r0
.LBB12_8:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB12_10
.LBB12_9:
	addi r1, r1, 1
	stw r12+%lo(fio_pos), r1
	jal r0, .LBB12_8
.LBB12_10:
	addi r3, r0, 40
	bne r5, r3, .LBB12_14
.LBB12_11:
	addi r1, r1, 1
	add r11, r1, r0
	jal r0, .LBB12_13
.LBB12_12:
	lui r3, %hi(fio_listed)
	stw r3+%lo(fio_listed), r1
	lui r1, %hi(.L.str.8)
	addi r1, r1, %lo(.L.str.8)
	lui r3, %hi(fio_fmt)
	stw r3+%lo(fio_fmt), r1
	add r1, r0, r0
.LBB12_13:
	stw r12+%lo(fio_pos), r1
	stw r13+%lo(fio_revert), r11
.LBB12_14:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB12_15:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r31, fprintf
.LBB12_16:
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end12:
	.size	f77_wr_begin_int, .Lfunc_end12-f77_wr_begin_int
                                        # -- End function
	.globl	f77_wr_i                        # -- Begin function f77_wr_i
	.p2align	2
	.type	f77_wr_i,@function
f77_wr_i:                               # @f77_wr_i
# %bb.0:
	addi sp, sp, -2048
	addi sp, sp, -48
	stw sp+0, lr
	addi r1, sp, 2047
	stw r1+45, r11
	stw r1+41, r12
	stw r1+37, r13
	stw r1+33, r14
	stw r1+29, r15
	add r11, r3, r0
	stw r1+25, r3
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB13_12
.LBB13_1:
	lui r5, %hi(.L.str.119)
	addi r5, r5, %lo(.L.str.119)
	addi r3, sp, 1048
	addi r12, r0, 32
	addi r6, r0, 12
	add r4, r12, r0
	add r7, r11, r0
	jal r31, snprintf
	ldbu r9, sp+1048
	beq r9, r0, .LBB13_11
.LBB13_2:
	lui r1, %hi(fio_len)
	ldw r7, r1+%lo(fio_len)
	lui r3, %hi(fio_col)
	ldw r8, r3+%lo(fio_col)
	addi r4, sp, 1048
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	jal r0, .LBB13_5
.LBB13_3:
	add r8, r9, r0
.LBB13_4:
	addi r4, r4, 1
	ldbu r9, r4+0
	beq r9, r0, .LBB13_11
.LBB13_5:
	bgt r8, r5, .LBB13_4
.LBB13_6:
	bge r7, r8, .LBB13_9
.LBB13_7:
	addi r10, r7, 1
	add r7, r7, r6
	stb r7+0, r12
	add r7, r10, r0
	bne r8, r10, .LBB13_7
.LBB13_8:
	stw r1+%lo(fio_len), r8
	add r7, r8, r0
.LBB13_9:
	add r10, r8, r6
	stb r10+0, r9
	addi r9, r8, 1
	stw r3+%lo(fio_col), r9
	blt r8, r7, .LBB13_3
.LBB13_10:
	stw r1+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB13_3
.LBB13_11:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	jal r0, .LBB13_56
.LBB13_12:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB13_16
.LBB13_13:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB13_15
.LBB13_14:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	bgt r4, r0, .LBB13_16
.LBB13_15:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
.LBB13_16:
	addi r3, r1, -65
	addi r4, r0, 25
	bgtu r3, r4, .LBB13_34
.LBB13_17:
	slli r3, r3, 2
	lui r4, %hi(.LJTI13_0)
	addi r4, r4, %lo(.LJTI13_0)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB13_18:
	addi r3, r0, 79
	seq r3, r1, r3
	addi r4, r0, 66
	seq r1, r1, r4
	sub r4, r0, r3
	andi r3, r4, 8
	xori r5, r3, 15
	xori r3, r3, 14
	sub r1, r0, r1
	and r3, r3, r1
	xor r3, r5, r3
	andi r4, r4, 7
	xori r5, r4, 4
	xori r4, r4, 5
	and r1, r4, r1
	xor r4, r5, r1
	lui r5, %hi(.L.str.4)
	addi r5, r5, %lo(.L.str.4)
	add r6, r11, r0
	add r1, r0, r0
.LBB13_19:
	add r7, r1, r0
	and r1, r6, r3
	add r1, r1, r5
	ldbu r8, r1+0
	addi r1, r7, 1
	addi r9, sp, 24
	add r7, r9, r7
	stb r7+0, r8
	srl r6, r6, r4
	bne r6, r0, .LBB13_19
.LBB13_20:
	lui r3, %hi(fio_d)
	ldw r3, r3+%lo(fio_d)
	addi r4, r0, 1023
	slt r4, r3, r4
	sub r4, r0, r4
	xori r5, r3, 1023
	and r4, r5, r4
	xori r4, r4, 1023
	bge r1, r4, .LBB13_23
.LBB13_21:
	addi r5, r0, 48
.LBB13_22:
	add r6, r1, r0
	addi r1, r1, 1
	addi r7, sp, 24
	add r6, r7, r6
	stb r6+0, r5
	bne r4, r1, .LBB13_22
.LBB13_23:
	addi r4, sp, 24
	addi r4, r4, -1
	addi r5, sp, 1048
	add r6, r1, r0
.LBB13_24:
	add r7, r4, r6
	ldbu r7, r7+0
	stb r5+0, r7
	addi r6, r6, -1
	addi r5, r5, 1
	bne r6, r0, .LBB13_24
.LBB13_25:
	addi r4, sp, 1048
	add r1, r4, r1
	stb r1+0, r0
	or  r1, r3, r11
	bne r1, r0, .LBB13_29
.LBB13_26:
	lui r1, %hi(fio_fmt)
	ldw r1, r1+%lo(fio_fmt)
	lui r3, %hi(fio_pos)
	ldw r3, r3+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+-1
	addi r4, r0, 48
	bne r3, r4, .LBB13_29
.LBB13_27:
	ldbu r1, r1+-2
	addi r3, r0, 46
	bne r1, r3, .LBB13_29
.LBB13_28:
	stb sp+1048, r0
.LBB13_29:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 1048
	jal r31, fio_field
.LBB13_30:
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	blt r3, r4, .LBB13_32
.LBB13_31:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB13_32:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB13_56
.LBB13_33:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
	jal r0, .LBB13_56
.LBB13_34:
	beq r1, r0, .LBB13_56
.LBB13_35:
	lui r15, %hi(fio_d)
	ldw r1, r15+%lo(fio_d)
	addi r14, r0, 1
	blt r1, r14, .LBB13_57
.LBB13_36:
	srai r1, r11, 31
	xor r3, r11, r1
	sub r6, r3, r1
	lui r5, %hi(.L.str.5)
	addi r5, r5, %lo(.L.str.5)
	addi r12, sp, 24
	addi r4, r0, 32
	add r3, r12, r0
	jal r31, snprintf
	add r3, r12, r0
	jal r31, strlen
	add r13, r1, r0
	addi r1, r0, 0
	slt r1, r11, r1
	lui r3, %hi(.L.str.8)
	addi r3, r3, %lo(.L.str.8)
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r11, r3, r1
	ldw r1, r15+%lo(fio_d)
	sgt r3, r1, r13
	sub r1, r1, r13
	sub r3, r0, r3
	and r7, r1, r3
	lui r5, %hi(.L.str.6)
	addi r5, r5, %lo(.L.str.6)
	addi r3, sp, 1048
	addi r4, r0, 1024
	add r6, r11, r0
	add r8, r0, r0
	add r9, r12, r0
	jal r31, snprintf
	ldw r1, r15+%lo(fio_d)
	blt r13, r1, .LBB13_63
.LBB13_37:
	lui r5, %hi(.L.str.9)
	addi r5, r5, %lo(.L.str.9)
	addi r3, sp, 1048
	addi r4, r0, 1024
	addi r7, sp, 24
	jal r0, .LBB13_62
.LBB13_38:
	lui r1, %hi(fio_w)
	ldw lr, r1+%lo(fio_w)
	addi r1, r0, 2
	blt lr, r1, .LBB13_48
.LBB13_39:
	lui r3, %hi(fio_len)
	ldw r8, r3+%lo(fio_len)
	lui r4, %hi(fio_col)
	ldw r9, r4+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	jal r0, .LBB13_42
.LBB13_40:
	add r9, lr, r0
.LBB13_41:
	addi lr, r10, -1
	ble r10, r1, .LBB13_48
.LBB13_42:
	add r10, lr, r0
	bgt r9, r5, .LBB13_41
.LBB13_43:
	bge r8, r9, .LBB13_46
.LBB13_44:
	addi lr, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, lr, r0
	bne r9, lr, .LBB13_44
.LBB13_45:
	stw r3+%lo(fio_len), r9
	add r8, r9, r0
.LBB13_46:
	add lr, r9, r6
	stb lr+0, r7
	addi lr, r9, 1
	stw r4+%lo(fio_col), lr
	blt r9, r8, .LBB13_40
.LBB13_47:
	stw r3+%lo(fio_len), lr
	add r8, lr, r0
	jal r0, .LBB13_40
.LBB13_48:
	lui r3, %hi(fio_col)
	ldw r1, r3+%lo(fio_col)
	addi r4, r0, 1022
	bgt r1, r4, .LBB13_30
.LBB13_49:
	lui r4, %hi(fio_len)
	ldw r5, r4+%lo(fio_len)
	bge r5, r1, .LBB13_53
.LBB13_50:
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
.LBB13_51:
	addi r8, r5, 1
	add r5, r5, r6
	stb r5+0, r7
	add r5, r8, r0
	bne r1, r8, .LBB13_51
.LBB13_52:
	stw r4+%lo(fio_len), r1
	add r5, r1, r0
.LBB13_53:
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
	blt r1, r5, .LBB13_30
.LBB13_54:
	stw r4+%lo(fio_len), r6
	jal r0, .LBB13_30
.LBB13_55:
	addi r1, sp, 2047
	addi r3, r1, 25
	addi r4, r0, 4
	jal r31, fio_hol_out
.LBB13_56:
	addi r1, sp, 2047
	ldw r15, r1+29
	ldw r14, r1+33
	ldw r13, r1+37
	ldw r12, r1+41
	ldw r11, r1+45
	ldw lr, sp+0
	addi sp, sp, 2047
	addi sp, sp, 49
	jalr r0, r31, 0
.LBB13_57:
	lui r1, %hi(fio_fmt)
	ldw r1, r1+%lo(fio_fmt)
	lui r3, %hi(fio_pos)
	ldw r3, r3+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+-1
	addi r4, r0, 48
	bne r3, r4, .LBB13_61
.LBB13_58:
	bne r11, r0, .LBB13_61
.LBB13_59:
	ldbu r1, r1+-2
	andi r1, r1, 255
	addi r3, r0, 46
	bne r1, r3, .LBB13_61
.LBB13_60:
	stb sp+1048, r0
	jal r0, .LBB13_63
.LBB13_61:
	lui r5, %hi(.L.str.10)
	addi r5, r5, %lo(.L.str.10)
	addi r3, sp, 1048
	addi r4, r0, 1024
.LBB13_62:
	add r6, r11, r0
	jal r31, snprintf
.LBB13_63:
	lui r1, %hi(fio_sp)
	ldbu r1, r1+%lo(fio_sp)
	bne r1, r14, .LBB13_68
.LBB13_64:
	ldbu r1, sp+1048
	beq r1, r0, .LBB13_68
.LBB13_65:
	addi r13, r0, 43
	beq r1, r13, .LBB13_68
.LBB13_66:
	addi r3, r0, 45
	beq r1, r3, .LBB13_68
.LBB13_67:
	addi r11, sp, 1048
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+1048, r13
.LBB13_68:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 1048
	jal r31, fio_field
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	bge r3, r14, .LBB13_31
	jal r0, .LBB13_32
.Lfunc_end13:
	.size	f77_wr_i, .Lfunc_end13-f77_wr_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI13_0,@object
.LJTI13_0:
	.word	.LBB13_55
	.word	.LBB13_18
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_38
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_18
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_35
	.word	.LBB13_18
	.size	.LJTI13_0, 104
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function fio_hol_out
	.type	fio_hol_out,@function
fio_hol_out:                            # @fio_hol_out
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	lui r1, %hi(fio_w)
	ldw r8, r1+%lo(fio_w)
	addi r1, r0, 1
	lui r5, %hi(fio_len)
	lui r6, %hi(fio_col)
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	blt r8, r1, .LBB14_2
.LBB14_1:
	blt r8, r4, .LBB14_14
.LBB14_2:
	ble r8, r4, .LBB14_13
.LBB14_3:
	sub r12, r8, r4
	blt r12, r1, .LBB14_13
.LBB14_4:
	ldw r10, r5+%lo(fio_len)
	ldw lr, r6+%lo(fio_col)
	addi r8, r0, 1022
	addi r9, r0, 32
	jal r0, .LBB14_7
.LBB14_5:
	add lr, r12, r0
.LBB14_6:
	addi r12, r11, -1
	ble r11, r1, .LBB14_13
.LBB14_7:
	add r11, r12, r0
	bgt lr, r8, .LBB14_6
.LBB14_8:
	bge r10, lr, .LBB14_11
.LBB14_9:
	addi r12, r10, 1
	add r10, r10, r7
	stb r10+0, r9
	add r10, r12, r0
	bne lr, r12, .LBB14_9
.LBB14_10:
	stw r5+%lo(fio_len), lr
	add r10, lr, r0
.LBB14_11:
	add r12, lr, r7
	stb r12+0, r9
	addi r12, lr, 1
	stw r6+%lo(fio_col), r12
	blt lr, r10, .LBB14_5
.LBB14_12:
	stw r5+%lo(fio_len), r12
	add r10, r12, r0
	jal r0, .LBB14_5
.LBB14_13:
	add r8, r4, r0
.LBB14_14:
	ldw lr, r5+%lo(fio_len)
	ldw r11, r6+%lo(fio_col)
	addi r4, r0, 1022
	addi r9, r0, 32
	add r10, r0, r0
	jal r0, .LBB14_17
.LBB14_15:
	add r11, r12, r0
.LBB14_16:
	addi r10, r10, 1
	beq r10, r8, .LBB14_23
.LBB14_17:
	bgt r11, r4, .LBB14_16
.LBB14_18:
	add r12, r3, r10
	ldbu r12, r12+0
	bge lr, r11, .LBB14_21
.LBB14_19:
	addi r13, lr, 1
	add lr, lr, r7
	stb lr+0, r9
	add lr, r13, r0
	bne r11, r13, .LBB14_19
.LBB14_20:
	stw r5+%lo(fio_len), r11
	add lr, r11, r0
.LBB14_21:
	add r13, r11, r7
	stb r13+0, r12
	addi r12, r11, 1
	stw r6+%lo(fio_col), r12
	blt r11, lr, .LBB14_15
.LBB14_22:
	stw r5+%lo(fio_len), r12
	add lr, r12, r0
	jal r0, .LBB14_15
.LBB14_23:
	lui r3, %hi(fio_rep)
	ldw r4, r3+%lo(fio_rep)
	blt r4, r1, .LBB14_25
.LBB14_24:
	addi r1, r4, -1
	stw r3+%lo(fio_rep), r1
.LBB14_25:
	ldw r1, r3+%lo(fio_rep)
	bne r1, r0, .LBB14_27
.LBB14_26:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB14_27:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end14:
	.size	fio_hol_out, .Lfunc_end14-fio_hol_out
                                        # -- End function
	.p2align	2                               # -- Begin function fio_blanks
	.type	fio_blanks,@function
fio_blanks:                             # @fio_blanks
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	addi r1, r0, 1
	blt r3, r1, .LBB15_10
.LBB15_1:
	lui r4, %hi(fio_len)
	ldw r9, r4+%lo(fio_len)
	lui r5, %hi(fio_col)
	ldw r10, r5+%lo(fio_col)
	addi r6, r0, 1022
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
	jal r0, .LBB15_4
.LBB15_2:
	add r10, r3, r0
.LBB15_3:
	addi r3, lr, -1
	ble lr, r1, .LBB15_10
.LBB15_4:
	add lr, r3, r0
	bgt r10, r6, .LBB15_3
.LBB15_5:
	bge r9, r10, .LBB15_8
.LBB15_6:
	addi r3, r9, 1
	add r9, r9, r7
	stb r9+0, r8
	add r9, r3, r0
	bne r10, r3, .LBB15_6
.LBB15_7:
	stw r4+%lo(fio_len), r10
	add r9, r10, r0
.LBB15_8:
	add r3, r10, r7
	stb r3+0, r8
	addi r3, r10, 1
	stw r5+%lo(fio_col), r3
	blt r10, r9, .LBB15_2
.LBB15_9:
	stw r4+%lo(fio_len), r3
	add r9, r3, r0
	jal r0, .LBB15_2
.LBB15_10:
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end15:
	.size	fio_blanks, .Lfunc_end15-fio_blanks
                                        # -- End function
	.p2align	2                               # -- Begin function fio_putc
	.type	fio_putc,@function
fio_putc:                               # @fio_putc
# %bb.0:
	lui r4, %hi(fio_col)
	ldw r1, r4+%lo(fio_col)
	addi r5, r0, 1022
	bgt r1, r5, .LBB16_7
.LBB16_1:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	bge r6, r1, .LBB16_5
.LBB16_2:
	addi r8, r0, 32
.LBB16_3:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r1, r9, .LBB16_3
.LBB16_4:
	stw r5+%lo(fio_len), r1
	add r6, r1, r0
.LBB16_5:
	add r7, r1, r7
	stb r7+0, r3
	addi r3, r1, 1
	stw r4+%lo(fio_col), r3
	blt r1, r6, .LBB16_7
.LBB16_6:
	stw r5+%lo(fio_len), r3
.LBB16_7:
	jalr r0, r31, 0
.Lfunc_end16:
	.size	fio_putc, .Lfunc_end16-fio_putc
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
	ble r12, r0, .LBB17_11
.LBB17_1:
	bge r12, r1, .LBB17_21
.LBB17_2:
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
	jal r0, .LBB17_5
.LBB17_3:
	add r10, r11, r0
.LBB17_4:
	addi r12, lr, -1
	ble lr, r5, .LBB17_41
.LBB17_5:
	add lr, r12, r0
	bgt r10, r4, .LBB17_4
.LBB17_6:
	bge r8, r10, .LBB17_9
.LBB17_7:
	addi r11, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, r11, r0
	bne r10, r11, .LBB17_7
.LBB17_8:
	stw r1+%lo(fio_len), r10
	add r8, r10, r0
.LBB17_9:
	add r11, r10, r6
	stb r11+0, r9
	addi r11, r10, 1
	stw r3+%lo(fio_col), r11
	blt r10, r8, .LBB17_3
.LBB17_10:
	stw r1+%lo(fio_len), r11
	add r8, r11, r0
	jal r0, .LBB17_3
.LBB17_11:
	ldbu r9, r11+0
	beq r9, r0, .LBB17_41
.LBB17_12:
	lui r1, %hi(fio_len)
	ldw r7, r1+%lo(fio_len)
	lui r3, %hi(fio_col)
	ldw r8, r3+%lo(fio_col)
	addi r4, r0, 1022
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	addi r6, r0, 32
	jal r0, .LBB17_15
.LBB17_13:
	add r8, r9, r0
.LBB17_14:
	addi r11, r11, 1
	ldbu r9, r11+0
	beq r9, r0, .LBB17_41
.LBB17_15:
	bgt r8, r4, .LBB17_14
.LBB17_16:
	bge r7, r8, .LBB17_19
.LBB17_17:
	addi r10, r7, 1
	add r7, r7, r5
	stb r7+0, r6
	add r7, r10, r0
	bne r8, r10, .LBB17_17
.LBB17_18:
	stw r1+%lo(fio_len), r8
	add r7, r8, r0
.LBB17_19:
	add r10, r8, r5
	stb r10+0, r9
	addi r9, r8, 1
	stw r3+%lo(fio_col), r9
	blt r8, r7, .LBB17_13
.LBB17_20:
	stw r1+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB17_13
.LBB17_21:
	sub lr, r12, r1
	addi r5, r0, 1
	lui r1, %hi(fio_len)
	lui r3, %hi(fio_col)
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	blt lr, r5, .LBB17_31
.LBB17_22:
	ldw r8, r1+%lo(fio_len)
	ldw r9, r3+%lo(fio_col)
	addi r6, r0, 1022
	addi r7, r0, 32
	jal r0, .LBB17_25
.LBB17_23:
	add r9, lr, r0
.LBB17_24:
	addi lr, r10, -1
	ble r10, r5, .LBB17_31
.LBB17_25:
	add r10, lr, r0
	bgt r9, r6, .LBB17_24
.LBB17_26:
	bge r8, r9, .LBB17_29
.LBB17_27:
	addi lr, r8, 1
	add r8, r8, r4
	stb r8+0, r7
	add r8, lr, r0
	bne r9, lr, .LBB17_27
.LBB17_28:
	stw r1+%lo(fio_len), r9
	add r8, r9, r0
.LBB17_29:
	add lr, r9, r4
	stb lr+0, r7
	addi lr, r9, 1
	stw r3+%lo(fio_col), lr
	blt r9, r8, .LBB17_23
.LBB17_30:
	stw r1+%lo(fio_len), lr
	add r8, lr, r0
	jal r0, .LBB17_23
.LBB17_31:
	ldbu r9, r11+0
	beq r9, r0, .LBB17_41
.LBB17_32:
	ldw r7, r1+%lo(fio_len)
	ldw r8, r3+%lo(fio_col)
	addi r5, r0, 1022
	addi r6, r0, 32
	jal r0, .LBB17_35
.LBB17_33:
	add r8, r9, r0
.LBB17_34:
	addi r11, r11, 1
	ldbu r9, r11+0
	beq r9, r0, .LBB17_41
.LBB17_35:
	bgt r8, r5, .LBB17_34
.LBB17_36:
	bge r7, r8, .LBB17_39
.LBB17_37:
	addi r10, r7, 1
	add r7, r7, r4
	stb r7+0, r6
	add r7, r10, r0
	bne r8, r10, .LBB17_37
.LBB17_38:
	stw r1+%lo(fio_len), r8
	add r7, r8, r0
.LBB17_39:
	add r10, r8, r4
	stb r10+0, r9
	addi r9, r8, 1
	stw r3+%lo(fio_col), r9
	blt r8, r7, .LBB17_33
.LBB17_40:
	stw r1+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB17_33
.LBB17_41:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end17:
	.size	fio_field, .Lfunc_end17-fio_field
                                        # -- End function
	.p2align	2                               # -- Begin function fio_plus
	.type	fio_plus,@function
fio_plus:                               # @fio_plus
# %bb.0:
	lui r1, %hi(fio_sp)
	ldbu r1, r1+%lo(fio_sp)
	addi r4, r0, 1
	bne r1, r4, .LBB18_6
.LBB18_1:
	ldbu r1, r3+0
	beq r1, r0, .LBB18_6
.LBB18_2:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	addi r13, r0, 43
	beq r1, r13, .LBB18_5
.LBB18_3:
	addi r4, r0, 45
	beq r1, r4, .LBB18_5
.LBB18_4:
	addi r11, r3, 1
	add r12, r3, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, memmove
	stb r12+0, r13
.LBB18_5:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB18_6:
	jalr r0, r31, 0
.Lfunc_end18:
	.size	fio_plus, .Lfunc_end18-fio_plus
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_wr_d
	.type	.LCPI19_0,@object
.LCPI19_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI19_0, 8
	.type	.LCPI19_1,@object
.LCPI19_1:
	.quad	0x4024000000000000              # double 10
	.size	.LCPI19_1, 8
	.text
	.globl	f77_wr_d
	.p2align	2
	.type	f77_wr_d,@function
f77_wr_d:                               # @f77_wr_d
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
	add r13, r4, r0
	add r12, r3, r0
	stw sp+1064, r13
	stw sp+1060, r3
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB19_2
.LBB19_1:
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r13, r0
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
	jal r0, fio_list_out_real
.LBB19_2:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB19_6
.LBB19_3:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB19_5
.LBB19_4:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	bgt r4, r0, .LBB19_6
.LBB19_5:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
.LBB19_6:
	beq r1, r0, .LBB19_70
.LBB19_7:
	addi r3, r0, 73
	beq r1, r3, .LBB19_14
.LBB19_8:
	addi r4, r0, 65
	bne r1, r4, .LBB19_10
.LBB19_9:
	addi r3, sp, 1060
	addi r4, r0, 8
	jal r31, fio_hol_out
	jal r0, .LBB19_70
.LBB19_10:
	feq.d r4, r12, r12
	and r7, r4, r4
	xori r4, r7, 1
	lui r6, %hi(.LCPI19_0)
	addi r6, r6, %lo(.LCPI19_0)
	bne r4, r0, .LBB19_17
.LBB19_11:
	fsub.d r8, r12, r12
	ldw r5, r6+4
	ldw r4, r6+0
	feq.d r8, r8, r4
	beq r8, r0, .LBB19_17
.LBB19_12:
	addi r6, r0, 70
	beq r1, r6, .LBB19_20
.LBB19_13:
	bne r1, r3, .LBB19_31
.LBB19_14:
	fcvt.w.d r6, r12
	lui r5, %hi(.L.str.10)
	addi r5, r5, %lo(.L.str.10)
	addi r3, sp, 36
	addi r4, r0, 1024
.LBB19_15:
	jal r31, snprintf
.LBB19_16:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 36
	jal r0, .LBB19_66
.LBB19_17:
	lui r1, %hi(fio_w)
	ldw r11, r1+%lo(fio_w)
	beq r7, r0, .LBB19_63
.LBB19_18:
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
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	lui r5, %hi(.L.str.12)
	addi r5, r5, %lo(.L.str.12)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r4, r4, r3
	beq r1, r0, .LBB19_64
.LBB19_19:
	stb sp+36, r1
	addi r1, sp, 36
	addi r3, r1, 1
	jal r0, .LBB19_65
.LBB19_20:
	lui r1, %hi(fio_scale)
	ldw r3, r1+%lo(fio_scale)
	addi r1, r0, 1
	blt r3, r1, .LBB19_24
.LBB19_21:
	addi r3, r3, 1
	lui r6, %hi(.LCPI19_1)
	addi r6, r6, %lo(.LCPI19_1)
	ldw r5, r6+4
	ldw r4, r6+0
.LBB19_22:
	fmul.d r12, r12, r4
	addi r3, r3, -1
	bgt r3, r1, .LBB19_22
.LBB19_23:
	add r3, r0, r0
.LBB19_24:
	addi r4, r0, -1
	bgt r3, r4, .LBB19_27
.LBB19_25:
	lui r6, %hi(.LCPI19_1)
	addi r6, r6, %lo(.LCPI19_1)
	ldw r5, r6+4
	ldw r4, r6+0
.LBB19_26:
	fdiv.d r12, r12, r4
	addi r6, r3, 1
	sltu r7, r6, r3
	add r3, r6, r0
	bne r7, r1, .LBB19_26
.LBB19_27:
	lui r14, %hi(fio_d)
	ldw r6, r14+%lo(fio_d)
	lui r5, %hi(.L.str.14)
	addi r5, r5, %lo(.L.str.14)
	addi r11, sp, 20
	addi r4, r0, 16
	add r3, r11, r0
	jal r31, snprintf
	addi r3, sp, 36
	addi r4, r0, 1024
	add r5, r11, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	ldw r1, r14+%lo(fio_d)
	bne r1, r0, .LBB19_29
.LBB19_28:
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	addi r3, sp, 36
	jal r31, strcat
.LBB19_29:
	addi r11, sp, 36
	add r3, r11, r0
	jal r31, fio_plus
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
.LBB19_30:
	add r3, r11, r0
	jal r31, fio_drop_zero
	jal r0, .LBB19_16
.LBB19_31:
	lui r15, %hi(fio_w)
	addi r3, r0, 71
	bne r1, r3, .LBB19_36
.LBB19_32:
	ldw r6, r15+%lo(fio_w)
	bne r6, r0, .LBB19_36
.LBB19_33:
	lui r6, %hi(fio_dgiven)
	ldbu r6, r6+%lo(fio_dgiven)
	andi r6, r6, 1
	bne r6, r0, .LBB19_36
.LBB19_34:
	lui r11, %hi(fio_single)
	ldbu r1, r11+%lo(fio_single)
	addi r3, r0, -1
	xor r1, r1, r3
	andi r7, r1, 1
	addi r3, sp, 36
	addi r4, r0, 1024
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fio_list_real_text
	bne r1, r0, .LBB19_16
.LBB19_35:
	lui r14, %hi(fio_scale)
	ldw r15, r14+%lo(fio_scale)
	lui r16, %hi(fio_e)
	ldw r17, r16+%lo(fio_e)
	stw r14+%lo(fio_scale), r0
	ldbu r1, r11+%lo(fio_single)
	sub r3, r0, r1
	addi r1, r1, -1
	andi r1, r1, 3
	stw r16+%lo(fio_e), r1
	andi r1, r3, 24
	xori r4, r1, 17
	addi r3, sp, 36
	addi r7, r0, 69
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fio_efmt
	stw r14+%lo(fio_scale), r15
	stw r16+%lo(fio_e), r17
	jal r0, .LBB19_16
.LBB19_36:
	beq r1, r3, .LBB19_39
.LBB19_37:
	addi r3, r0, -2
	and r3, r1, r3
	addi r4, r0, 68
	bne r3, r4, .LBB19_42
.LBB19_38:
	lui r3, %hi(fio_d)
	ldw r4, r3+%lo(fio_d)
	addi r11, sp, 36
	add r3, r11, r0
	add r5, r12, r0
	add r6, r13, r0
	add r7, r1, r0
	jal r31, fio_efmt
	add r3, r11, r0
	jal r31, fio_plus
	ldw r4, r15+%lo(fio_w)
	jal r0, .LBB19_30
.LBB19_39:
	lui r17, %hi(fio_d)
	ldw r3, r17+%lo(fio_d)
	addi r16, r0, 1
	sgt r1, r3, r16
	sub r1, r0, r1
	xori r6, r3, 1
	and r1, r6, r1
	xori r14, r1, 1
	addi r1, r0, -1
	beq r3, r0, .LBB19_47
.LBB19_40:
	feq.d r3, r12, r4
	beq r3, r0, .LBB19_45
.LBB19_41:
	add r16, r0, r0
	jal r0, .LBB19_47
.LBB19_42:
	addi r3, r0, 339
	beq r1, r3, .LBB19_44
.LBB19_43:
	addi r3, r0, 334
	bne r1, r3, .LBB19_62
.LBB19_44:
	addi r3, r0, 334
	seq r7, r1, r3
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	addi r11, sp, 36
	add r3, r11, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fio_esn
	add r3, r11, r0
	jal r31, fio_plus
	jal r0, .LBB19_16
.LBB19_45:
	flt.d r3, r12, r4
	fneg.d r6, r12
	xor r8, r7, r13
	sub r3, r0, r3
	and r8, r8, r3
	xor r9, r13, r8
	xor r6, r6, r12
	and r3, r6, r3
	xor r8, r12, r3
	fsub.d r6, r8, r8
	feq.d r3, r6, r4
	xori r3, r3, 1
	bne r3, r0, .LBB19_47
.LBB19_46:
	addi r6, r14, -1
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r11, sp, 36
	addi r4, r0, 48
	add r3, r11, r0
	add r7, r8, r0
	add r8, r9, r0
	jal r31, snprintf
	addi r4, r0, 69
	add r3, r11, r0
	jal r31, strchr
	addi r3, r1, 1
	jal r31, atoi
	addi r3, r0, -1
	slt r4, r1, r3
	sge r5, r1, r14
	or  r16, r4, r5
	xor r1, r1, r3
	sub r3, r0, r16
	or  r1, r3, r1
.LBB19_47:
	beq r16, r0, .LBB19_49
.LBB19_48:
	ldw r4, r17+%lo(fio_d)
	addi r11, sp, 36
	addi r7, r0, 69
	add r3, r11, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fio_efmt
	add r3, r11, r0
	jal r31, fio_plus
	ldw r4, r15+%lo(fio_w)
	add r3, r11, r0
	jal r31, fio_drop_zero
	jal r0, .LBB19_60
.LBB19_49:
	lui r3, %hi(fio_e)
	ldw r3, r3+%lo(fio_e)
	addi r17, r0, 0
	sgt r4, r3, r17
	addi r3, r3, 2
	sub r4, r0, r4
	xori r3, r3, 4
	and r3, r3, r4
	xori r18, r3, 4
	ldw r3, r15+%lo(fio_w)
	sgt r4, r3, r17
	sub r3, r3, r18
	sub r4, r0, r4
	and r11, r3, r4
	add r19, r1, r14
	sgt r1, r19, r17
	sub r1, r0, r1
	and r6, r19, r1
	lui r5, %hi(.L.str.14)
	addi r5, r5, %lo(.L.str.14)
	addi r14, sp, 20
	addi r4, r0, 16
	add r3, r14, r0
	jal r31, snprintf
	addi r3, sp, 36
	addi r4, r0, 1024
	add r5, r14, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	bgt r19, r0, .LBB19_51
.LBB19_50:
	lui r4, %hi(.L.str.15)
	addi r4, r4, %lo(.L.str.15)
	addi r3, sp, 36
	jal r31, strcat
.LBB19_51:
	addi r12, sp, 36
	add r3, r12, r0
	jal r31, fio_plus
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fio_drop_zero
	ldw r1, r15+%lo(fio_w)
	addi r12, r0, 1
	blt r1, r12, .LBB19_56
.LBB19_52:
	addi r3, sp, 36
	jal r31, strlen
	ble r1, r11, .LBB19_56
.LBB19_53:
	ldw r13, r15+%lo(fio_w)
	blt r13, r12, .LBB19_57
.LBB19_54:
	addi r11, r0, 42
.LBB19_55:
	add r3, r11, r0
	jal r31, fio_putc
	addi r13, r13, -1
	bne r13, r0, .LBB19_55
	jal r0, .LBB19_57
.LBB19_56:
	addi r3, sp, 36
	add r4, r11, r0
	jal r31, fio_field
	ldw r1, r15+%lo(fio_w)
	sgt r1, r1, r17
	sub r1, r0, r1
	and r3, r18, r1
	jal r31, fio_blanks
.LBB19_57:
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	blt r3, r12, .LBB19_59
.LBB19_58:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB19_59:
	ldw r1, r1+%lo(fio_rep)
	beq r1, r0, .LBB19_61
.LBB19_60:
	bne r16, r0, .LBB19_16
	jal r0, .LBB19_70
.LBB19_61:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
	bne r16, r0, .LBB19_16
	jal r0, .LBB19_70
.LBB19_62:
	lui r5, %hi(.L.str.17)
	addi r5, r5, %lo(.L.str.17)
	addi r3, sp, 36
	addi r4, r0, 1024
	add r6, r12, r0
	add r7, r13, r0
	jal r0, .LBB19_15
.LBB19_63:
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
.LBB19_64:
	addi r3, sp, 36
.LBB19_65:
	jal r31, strcpy
	addi r3, sp, 36
	add r4, r11, r0
.LBB19_66:
	jal r31, fio_field
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	blt r3, r4, .LBB19_68
.LBB19_67:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB19_68:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB19_70
.LBB19_69:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB19_70:
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
.Lfunc_end19:
	.size	f77_wr_d, .Lfunc_end19-f77_wr_d
                                        # -- End function
	.p2align	2                               # -- Begin function fio_list_out_real
	.type	fio_list_out_real,@function
fio_list_out_real:                      # @fio_list_out_real
# %bb.0:
	addi sp, sp, -112
	stw sp+0, lr
	stw sp+108, r11
	stw sp+104, r12
	stw sp+100, r13
	stw sp+96, r14
	stw sp+92, r15
	add r11, r5, r0
	add r6, r4, r0
	add r5, r3, r0
	lui r12, %hi(fio_col)
	ldw r1, r12+%lo(fio_col)
	addi r13, r0, 1022
	lui r14, %hi(fio_len)
	lui r15, %hi(fio_line)
	addi r15, r15, %lo(fio_line)
	bgt r1, r13, .LBB20_6
.LBB20_1:
	ldw r4, r14+%lo(fio_len)
	addi r3, r0, 32
	bge r4, r1, .LBB20_4
.LBB20_2:
	addi r7, r4, 1
	add r4, r4, r15
	stb r4+0, r3
	add r4, r7, r0
	bne r1, r7, .LBB20_2
.LBB20_3:
	stw r14+%lo(fio_len), r1
	add r4, r1, r0
.LBB20_4:
	add r7, r1, r15
	stb r7+0, r3
	addi r3, r1, 1
	stw r12+%lo(fio_col), r3
	blt r1, r4, .LBB20_6
.LBB20_5:
	stw r14+%lo(fio_len), r3
.LBB20_6:
	addi r3, sp, 28
	addi r4, r0, 64
	add r7, r11, r0
	jal r31, fio_list_real_text
	beq r1, r0, .LBB20_16
.LBB20_7:
	addi r1, r0, 0
	seq r1, r11, r1
	sub r1, r0, r1
	andi r3, r1, 1
	xori r11, r3, 5
	andi r1, r1, 24
	xori r4, r1, 20
	addi r3, sp, 28
	jal r31, fio_field
	ldw r4, r14+%lo(fio_len)
	ldw r5, r12+%lo(fio_col)
	addi r1, r0, 1
	addi r3, r0, 32
	jal r0, .LBB20_10
.LBB20_8:
	add r5, r7, r0
.LBB20_9:
	addi r11, r6, -1
	bleu r6, r1, .LBB20_17
.LBB20_10:
	add r6, r11, r0
	bgt r5, r13, .LBB20_9
.LBB20_11:
	bge r4, r5, .LBB20_14
.LBB20_12:
	addi r7, r4, 1
	add r4, r4, r15
	stb r4+0, r3
	add r4, r7, r0
	bne r5, r7, .LBB20_12
.LBB20_13:
	stw r14+%lo(fio_len), r5
	add r4, r5, r0
.LBB20_14:
	add r7, r5, r15
	stb r7+0, r3
	addi r7, r5, 1
	stw r12+%lo(fio_col), r7
	blt r5, r4, .LBB20_8
.LBB20_15:
	stw r14+%lo(fio_len), r7
	add r4, r7, r0
	jal r0, .LBB20_8
.LBB20_16:
	addi r1, r0, 0
	seq r1, r11, r1
	sub r1, r0, r1
	andi r1, r1, 9
	xori r4, r1, 25
	addi r3, sp, 28
	jal r31, fio_field
.LBB20_17:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	ldw r15, sp+92
	ldw r14, sp+96
	ldw r13, sp+100
	ldw r12, sp+104
	ldw r11, sp+108
	ldw lr, sp+0
	addi sp, sp, 112
	jalr r0, r31, 0
.Lfunc_end20:
	.size	fio_list_out_real, .Lfunc_end20-fio_list_out_real
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
	blt r12, r3, .LBB21_3
.LBB21_1:
	addi r3, r12, 1
	bne r1, r3, .LBB21_3
.LBB21_2:
	addi r5, r1, -1
	add r3, r11, r5
	ldb r3, r3+0
	addi r3, r3, -58
	addi r4, r0, -10
	bgeu r3, r4, .LBB21_4
.LBB21_3:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB21_4:
	ldbu r3, r11+0
	addi r4, r0, 43
	beq r3, r4, .LBB21_9
.LBB21_5:
	addi r4, r0, 45
	beq r3, r4, .LBB21_9
.LBB21_6:
	addi r4, r0, 48
	bne r3, r4, .LBB21_3
.LBB21_7:
	ldbu r3, r11+1
	addi r4, r0, 46
	bne r3, r4, .LBB21_3
.LBB21_8:
	addi r4, r11, 1
	add r3, r11, r0
	add r5, r1, r0
	jal r0, .LBB21_12
.LBB21_9:
	ldbu r1, r11+1
	addi r3, r0, 48
	bne r1, r3, .LBB21_3
.LBB21_10:
	ldbu r1, r11+2
	addi r3, r0, 46
	bne r1, r3, .LBB21_3
.LBB21_11:
	addi r3, r11, 1
	addi r4, r11, 2
.LBB21_12:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, memmove
.Lfunc_end21:
	.size	fio_drop_zero, .Lfunc_end21-fio_drop_zero
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_list_real_text
	.type	.LCPI22_0,@object
.LCPI22_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI22_0, 8
	.type	.LCPI22_2,@object
.LCPI22_2:
	.quad	0x3fb999999999999a              # double 0.10000000000000001
	.size	.LCPI22_2, 8
	.type	.LCPI22_3,@object
.LCPI22_3:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI22_3, 8
	.type	.LCPI22_4,@object
.LCPI22_4:
	.quad	0x4024000000000000              # double 10
	.size	.LCPI22_4, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI22_1,@object
.LCPI22_1:
	.quad	0x41cdcd6500000000              # double 1.0E+9
	.quad	0x4376345785d8a000              # double 1.0E+17
	.size	.LCPI22_1, 16
	.text
	.p2align	2
	.type	fio_list_real_text,@function
fio_list_real_text:                     # @fio_list_real_text
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
	add r13, r6, r0
	add r12, r5, r0
	add r11, r3, r0
	lui r1, %hi(.LCPI22_0)
	addi r1, r1, %lo(.LCPI22_0)
	ldw r19, r1+4
	ldw r18, r1+0
	flt.d r1, r12, r18
	sub r1, r0, r1
	feq.d r3, r12, r12
	and r3, r3, r3
	xori r3, r3, 1
	bne r3, r0, .LBB22_11
.LBB22_1:
	fsub.d r8, r12, r12
	feq.d r5, r8, r18
	beq r5, r0, .LBB22_11
.LBB22_2:
	fneg.d r14, r12
	xor r3, r15, r13
	and r3, r3, r1
	xor r9, r13, r3
	xor r3, r14, r12
	and r1, r3, r1
	xor r8, r12, r1
	addi r1, r0, 0
	sne r1, r7, r1
	sub r1, r0, r1
	andi r3, r1, 24
	xori r17, r3, 9
	feq.d r3, r12, r18
	bne r3, r0, .LBB22_5
.LBB22_3:
	lui r3, %hi(.LCPI22_2)
	addi r3, r3, %lo(.LCPI22_2)
	ldw r15, r3+4
	ldw r14, r3+0
	fle.d r3, r14, r8
	xori r3, r3, 1
	bne r3, r0, .LBB22_14
.LBB22_4:
	andi r1, r1, 8
	lui r3, %hi(.LCPI22_1)
	addi r3, r3, %lo(.LCPI22_1)
	add r1, r3, r1
	ldw r15, r1+4
	ldw r14, r1+0
	flt.d r1, r8, r14
	xori r1, r1, 1
	bne r1, r0, .LBB22_14
.LBB22_5:
	add r15, r4, r0
	addi r14, r0, 1
	lui r1, %hi(.LCPI22_3)
	addi r1, r1, %lo(.LCPI22_3)
	ldw r5, r1+4
	ldw r4, r1+0
	fle.d r1, r4, r8
	xori r3, r1, 1
	add r1, r14, r0
	bne r3, r0, .LBB22_8
.LBB22_6:
	lui r1, %hi(.LCPI22_4)
	addi r1, r1, %lo(.LCPI22_4)
	ldw r7, r1+4
	ldw r6, r1+0
	add r20, r8, r0
	add r21, r9, r0
	add r1, r0, r0
.LBB22_7:
	fdiv.d r20, r20, r6
	addi r1, r1, 1
	fle.d r3, r4, r20
	bne r3, r0, .LBB22_7
.LBB22_8:
	flt.d r3, r8, r4
	feq.d r4, r12, r18
	xori r4, r4, 1
	and r20, r4, r3
	sub r3, r0, r20
	and r3, r1, r3
	xor r1, r1, r3
	sub r6, r17, r1
	lui r5, %hi(.L.str.14)
	addi r5, r5, %lo(.L.str.14)
	addi r16, sp, 20
	addi r4, r0, 16
	add r3, r16, r0
	jal r31, snprintf
	add r3, r11, r0
	add r4, r15, r0
	add r5, r16, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	bne r20, r14, .LBB22_13
.LBB22_9:
	flt.d r12, r12, r18
	add r3, r11, r0
	jal r31, strlen
	add r3, r12, r17
	addi r3, r3, 2
	bleu r1, r3, .LBB22_13
.LBB22_10:
	add r1, r11, r3
	stb r1+0, r0
	jal r0, .LBB22_13
.LBB22_11:
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
	lui r5, %hi(.L.str.120)
	addi r5, r5, %lo(.L.str.120)
	xor r5, r5, r4
	and r1, r5, r1
	xor r1, r4, r1
	lui r4, %hi(.L.str.11)
	addi r4, r4, %lo(.L.str.11)
	xor r4, r4, r1
	sub r3, r0, r3
	and r3, r4, r3
	xor r4, r1, r3
	add r3, r11, r0
	jal r31, strcpy
.LBB22_12:
	add r14, r0, r0
.LBB22_13:
	add r1, r14, r0
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
.LBB22_14:
	addi r6, r17, -1
	lui r5, %hi(.L.str.121)
	addi r5, r5, %lo(.L.str.121)
	addi r14, sp, 20
	addi r1, r0, 16
	add r3, r14, r0
	add r15, r4, r0
	add r4, r1, r0
	add r16, r7, r0
	jal r31, snprintf
	add r3, r11, r0
	add r4, r15, r0
	add r5, r14, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	addi r4, r0, 69
	add r3, r11, r0
	jal r31, strchr
	beq r16, r0, .LBB22_12
.LBB22_15:
	add r14, r0, r0
	beq r1, r0, .LBB22_13
.LBB22_16:
	addi r11, r1, 2
	add r3, r11, r0
	jal r31, strlen
	addi r3, r0, 2
	bgt r1, r3, .LBB22_12
.LBB22_17:
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
	jal r0, .LBB22_12
.Lfunc_end22:
	.size	fio_list_real_text, .Lfunc_end22-fio_list_real_text
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_efmt
	.type	.LCPI23_0,@object
.LCPI23_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI23_0, 8
	.text
	.p2align	2
	.type	fio_efmt,@function
fio_efmt:                               # @fio_efmt
# %bb.0:
	addi sp, sp, -2048
	addi sp, sp, -96
	stw sp+0, lr
	addi r1, sp, 2047
	stw r1+93, r11
	stw r1+89, r12
	stw r1+85, r13
	stw r1+81, r14
	stw r1+77, r15
	stw r1+73, r16
	stw r1+69, r17
	stw r1+65, r18
	stw r1+61, r19
	stw r1+57, r20
	stw r1+53, r21
	add r11, r7, r0
	add r13, r6, r0
	add r12, r5, r0
	add r14, r3, r0
	lui r1, %hi(fio_scale)
	ldw r1, r1+%lo(fio_scale)
	bgt r4, r0, .LBB23_2
.LBB23_1:
	ble r1, r0, .LBB23_51
.LBB23_2:
	addi r18, r0, 0
	sgt r3, r4, r18
	sub r3, r0, r3
	and r3, r4, r3
	sub r4, r0, r3
	sgt r4, r1, r4
	addi r5, r3, 2
	slt r5, r1, r5
	sub r5, r0, r5
	and r1, r1, r5
	sub r4, r0, r4
	and r19, r1, r4
	sgt r1, r19, r18
	add r4, r19, r3
	xori r5, r4, 1
	addi r17, r0, 1
	sgt r4, r4, r17
	sub r4, r0, r4
	and r4, r5, r4
	xori r4, r4, 1
	addi r4, r4, -1
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r6, r4, r1
	lui r5, %hi(.L.str.121)
	addi r5, r5, %lo(.L.str.121)
	addi r15, sp, 20
	addi r4, r0, 16
	add r3, r15, r0
	jal r31, snprintf
	addi r16, sp, 1060
	addi r4, r0, 1024
	add r3, r16, r0
	add r5, r15, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	ldbu r21, sp+1060
	addi r20, r0, 45
	seq r1, r21, r20
	addi r3, r0, 43
	seq r3, r21, r3
	or  r1, r1, r3
	or  r1, r1, r16
	addi r3, r1, 1
	addi r1, r0, 69
	addi r4, r0, 101
	addi r16, r0, 46
	addi r5, r0, 1023
	add r15, r0, r0
	jal r0, .LBB23_4
.LBB23_3:
	addi r3, r3, 1
.LBB23_4:
	ldbu r6, r3+-1
	beq r6, r0, .LBB23_11
.LBB23_5:
	beq r6, r1, .LBB23_10
.LBB23_6:
	beq r6, r4, .LBB23_10
.LBB23_7:
	beq r6, r16, .LBB23_3
.LBB23_8:
	bgt r15, r5, .LBB23_3
.LBB23_9:
	addi r7, sp, 36
	add r7, r7, r15
	stb r7+0, r6
	addi r15, r15, 1
	jal r0, .LBB23_3
.LBB23_10:
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	addi r1, r1, 1
	jal r0, .LBB23_12
.LBB23_11:
	add r1, r17, r0
.LBB23_12:
	lui r3, %hi(.LCPI23_0)
	addi r3, r3, %lo(.LCPI23_0)
	ldw r5, r3+4
	ldw r4, r3+0
	feq.d r3, r12, r4
	sub r3, r0, r3
	and r3, r1, r3
	add r6, r14, r0
	bne r21, r20, .LBB23_14
.LBB23_13:
	addi r6, r14, 1
	stb r14+0, r20
.LBB23_14:
	xor r1, r1, r3
	blt r19, r17, .LBB23_19
.LBB23_15:
	blt r15, r17, .LBB23_24
.LBB23_16:
	xor r3, r19, r15
	slt r7, r19, r15
	sub r7, r0, r7
	and r3, r3, r7
	xor r3, r15, r3
	add r7, r0, r0
.LBB23_17:
	add r8, r6, r7
	addi r9, sp, 36
	add r9, r9, r7
	ldbu r9, r9+0
	stb r8+0, r9
	addi r7, r7, 1
	bne r3, r7, .LBB23_17
.LBB23_18:
	add r6, r6, r7
	jal r0, .LBB23_25
.LBB23_19:
	addi r8, r0, 48
	stb r6+0, r8
	addi r9, r6, 2
	stb r6+1, r16
	addi r7, r0, 1015
	beq r19, r0, .LBB23_30
.LBB23_20:
	sub r3, r9, r14
	bgt r3, r7, .LBB23_30
.LBB23_21:
	sub r3, r6, r14
	addi r16, r3, 3
	addi r6, r0, -2
	addi r10, r0, 1016
	add lr, r19, r0
.LBB23_22:
	addi r3, r9, 1
	stb r9+0, r8
	bgt lr, r6, .LBB23_31
.LBB23_23:
	add r20, r16, r0
	addi lr, lr, 1
	addi r16, r16, 1
	add r9, r3, r0
	blt r20, r10, .LBB23_22
	jal r0, .LBB23_31
.LBB23_24:
	add r7, r0, r0
.LBB23_25:
	stb r6+0, r16
	addi r3, r6, 1
	bge r7, r15, .LBB23_37
.LBB23_26:
	sub r8, r3, r14
	addi r9, r0, 1015
	bgt r8, r9, .LBB23_37
.LBB23_27:
	sub r6, r6, r14
	addi r8, r6, 2
	addi r6, r0, 1016
.LBB23_28:
	add r9, r8, r0
	addi r8, sp, 36
	add r8, r8, r7
	ldbu r8, r8+0
	stb r3+0, r8
	addi r7, r7, 1
	addi r3, r3, 1
	bge r7, r15, .LBB23_37
.LBB23_29:
	addi r8, r9, 1
	blt r9, r6, .LBB23_28
	jal r0, .LBB23_37
.LBB23_30:
	add r3, r9, r0
.LBB23_31:
	blt r15, r17, .LBB23_37
.LBB23_32:
	sub r6, r3, r14
	bgt r6, r7, .LBB23_37
.LBB23_33:
	addi r6, r6, 1
	addi r7, r0, 1016
	add r8, r0, r0
.LBB23_34:
	add r9, r3, r8
	addi r10, sp, 36
	add r10, r10, r8
	ldbu r10, r10+0
	stb r9+0, r10
	addi r9, r8, 1
	bge r9, r15, .LBB23_36
.LBB23_35:
	add r10, r6, r8
	add r8, r9, r0
	blt r10, r7, .LBB23_34
.LBB23_36:
	add r3, r3, r9
.LBB23_37:
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
	bne r4, r0, .LBB23_40
.LBB23_38:
	addi r1, r5, -100
	addi r8, r0, 900
	bgeu r1, r8, .LBB23_40
.LBB23_39:
	add r1, r3, r0
	jal r0, .LBB23_41
.LBB23_40:
	addi r1, r3, 1
	stb r3+0, r11
.LBB23_41:
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
.LBB23_42:
	add r10, r3, r0
	mulhu r3, r5, r7
	srli lr, r3, 3
	mul r3, lr, r8
	sub r3, r5, r3
	ori  r11, r3, 48
	addi r3, r10, 1
	addi r13, sp, 2047
	addi r12, r13, 37
	add r12, r12, r10
	stb r12+0, r11
	bltu r5, r8, .LBB23_44
.LBB23_43:
	add r5, lr, r0
	bltu r10, r9, .LBB23_42
.LBB23_44:
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
	addi r6, sp, 2047
	addi r5, r6, 37
	bge r3, r4, .LBB23_48
.LBB23_45:
	add r5, r5, r3
	addi r6, r0, 48
	add r7, r4, r0
.LBB23_46:
	stb r5+0, r6
	addi r5, r5, 1
	addi r7, r7, -1
	bne r3, r7, .LBB23_46
.LBB23_47:
	add r3, r4, r0
.LBB23_48:
	addi r1, r1, 1
	addi r5, sp, 2047
	addi r4, r5, 37
	addi r4, r4, -1
.LBB23_49:
	add r5, r3, r0
	addi r3, r3, -1
	add r6, r4, r5
	ldbu r6, r6+0
	stb r1+0, r6
	addi r1, r1, 1
	bgt r5, r17, .LBB23_49
.LBB23_50:
	stb r1+0, r0
	addi r1, sp, 2047
	ldw r21, r1+53
	ldw r20, r1+57
	ldw r19, r1+61
	ldw r18, r1+65
	ldw r17, r1+69
	ldw r16, r1+73
	ldw r15, r1+77
	ldw r14, r1+81
	ldw r13, r1+85
	ldw r12, r1+89
	ldw r11, r1+93
	ldw lr, sp+0
	addi sp, sp, 2047
	addi sp, sp, 97
	jalr r0, r31, 0
.LBB23_51:
	jal r31, fio_flush_line
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.122)
	addi r4, r4, %lo(.L.str.122)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end23:
	.size	fio_efmt, .Lfunc_end23-fio_efmt
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_esn
	.type	.LCPI24_0,@object
.LCPI24_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI24_0, 8
	.text
	.p2align	2
	.type	fio_esn,@function
fio_esn:                                # @fio_esn
# %bb.0:
	addi sp, sp, -2048
	addi sp, sp, -128
	stw sp+0, lr
	addi r1, sp, 2047
	stw r1+125, r11
	stw r1+121, r12
	stw r1+117, r13
	stw r1+113, r14
	stw r1+109, r15
	stw r1+105, r16
	stw r1+101, r17
	stw r1+97, r18
	stw r1+93, r19
	stw r1+89, r20
	stw r1+85, r21
	stw r1+81, r22
	stw r1+77, r23
	stw r1+73, r24
	stw r1+69, r26
	stw r1+65, r27
	add r13, r7, r0
	add r15, r6, r0
	add r14, r5, r0
	add r12, r3, r0
	addi r19, r0, 0
	sgt r1, r4, r19
	sub r1, r0, r1
	and r1, r4, r1
	xori r3, r1, 500
	addi r4, r0, 500
	slt r1, r1, r4
	sub r1, r0, r1
	and r17, r3, r1
	lui r5, %hi(.L.str.123)
	addi r5, r5, %lo(.L.str.123)
	addi r16, sp, 1072
	addi r4, r0, 1024
	add r3, r16, r0
	add r6, r14, r0
	add r7, r15, r0
	jal r31, snprintf
	addi r11, r0, 69
	add r3, r16, r0
	add r4, r11, r0
	jal r31, strchr
	add r16, r0, r0
	lui r3, %hi(.LCPI24_0)
	addi r3, r3, %lo(.LCPI24_0)
	ldw r27, r3+4
	ldw r26, r3+0
	feq.d r3, r14, r26
	bne r3, r0, .LBB24_3
.LBB24_1:
	beq r1, r0, .LBB24_3
.LBB24_2:
	addi r3, r1, 1
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	add r16, r1, r0
.LBB24_3:
	xori r24, r17, 500
	lui r20, 699051
	beq r13, r0, .LBB24_6
.LBB24_4:
	addi r1, r0, -2
	blt r16, r0, .LBB24_7
.LBB24_5:
	addi r3, r20, -1365
	mulhu r3, r16, r3
	and r1, r3, r1
	srli r3, r3, 1
	add r1, r1, r3
	sub r1, r16, r1
	sub r1, r1, r16
	jal r0, .LBB24_8
.LBB24_6:
	addi r21, r0, 1
	jal r0, .LBB24_9
.LBB24_7:
	addi r3, r0, 2
	sub r3, r3, r16
	addi r4, r20, -1365
	mulhu r3, r3, r4
	and r1, r3, r1
	srli r3, r3, 1
	add r1, r1, r3
.LBB24_8:
	add r1, r16, r1
	addi r21, r1, 1
.LBB24_9:
	add r1, r24, r21
	addi r6, r1, -1
	lui r5, %hi(.L.str.121)
	addi r5, r5, %lo(.L.str.121)
	addi r17, sp, 1056
	addi r4, r0, 16
	add r3, r17, r0
	jal r31, snprintf
	addi r18, sp, 1072
	addi r4, r0, 1024
	add r3, r18, r0
	add r5, r17, r0
	add r6, r14, r0
	add r7, r15, r0
	jal r31, snprintf
	ldbu r23, sp+1072
	addi r22, r0, 45
	seq r1, r23, r22
	or  r1, r1, r18
	addi r3, r1, 1
	addi r18, r0, 46
	addi r1, r0, 1022
	add r17, r0, r0
	jal r0, .LBB24_11
.LBB24_10:
	addi r3, r3, 1
.LBB24_11:
	ldbu r4, r3+-1
	beq r4, r0, .LBB24_17
.LBB24_12:
	beq r4, r11, .LBB24_16
.LBB24_13:
	beq r4, r18, .LBB24_10
.LBB24_14:
	bgt r17, r1, .LBB24_10
.LBB24_15:
	addi r5, r17, 1
	addi r6, sp, 32
	add r6, r6, r17
	stb r6+0, r4
	add r17, r5, r0
	jal r0, .LBB24_10
.LBB24_16:
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	jal r0, .LBB24_18
.LBB24_17:
	add r1, r0, r0
.LBB24_18:
	feq.d r3, r14, r26
	sub r3, r0, r3
	and r3, r1, r3
	xor r1, r1, r3
	beq r13, r0, .LBB24_29
.LBB24_19:
	beq r1, r16, .LBB24_27
.LBB24_20:
	blt r1, r0, .LBB24_22
.LBB24_21:
	addi r3, r20, -1365
	mulhu r3, r1, r3
	addi r4, r0, -2
	and r4, r3, r4
	srli r3, r3, 1
	add r3, r4, r3
	sub r3, r1, r3
	sub r3, r3, r1
	jal r0, .LBB24_23
.LBB24_22:
	addi r3, r0, 2
	sub r3, r3, r1
	addi r4, r20, -1365
	mulhu r3, r3, r4
	addi r4, r0, -2
	and r4, r3, r4
	srli r3, r3, 1
	add r3, r4, r3
.LBB24_23:
	add r3, r1, r3
	addi r21, r3, 1
	add r3, r21, r24
	xori r4, r3, 1023
	addi r5, r0, 1023
	slt r5, r3, r5
	sub r5, r0, r5
	and r4, r4, r5
	xori r4, r4, 1023
	bge r17, r4, .LBB24_26
.LBB24_24:
	addi r5, r0, 48
.LBB24_25:
	add r6, r17, r0
	addi r17, r17, 1
	addi r7, sp, 32
	add r6, r7, r6
	stb r6+0, r5
	bne r4, r17, .LBB24_25
.LBB24_26:
	xor r4, r17, r3
	slt r5, r17, r3
	sub r5, r0, r5
	and r4, r4, r5
	xor r17, r3, r4
.LBB24_27:
	blt r1, r0, .LBB24_54
.LBB24_28:
	addi r3, r20, -1365
	mulhu r1, r1, r3
	addi r3, r0, -2
	and r3, r1, r3
	srli r1, r1, 1
	add r1, r3, r1
.LBB24_29:
	add r4, r12, r0
	bne r23, r22, .LBB24_31
.LBB24_30:
	addi r4, r12, 1
	stb r12+0, r22
.LBB24_31:
	xor r3, r21, r17
	slt r5, r21, r17
	sub r5, r0, r5
	and r3, r3, r5
	xor r5, r17, r3
	add r6, r0, r0
	addi r3, r0, 1
	blt r5, r3, .LBB24_34
.LBB24_32:
	add r7, r4, r6
	addi r8, sp, 32
	add r8, r8, r6
	ldbu r8, r8+0
	stb r7+0, r8
	addi r6, r6, 1
	bne r5, r6, .LBB24_32
.LBB24_33:
	add r4, r4, r6
.LBB24_34:
	stb r4+0, r18
	addi r5, r4, 1
	bge r6, r17, .LBB24_40
.LBB24_35:
	sub r7, r5, r12
	addi r8, r0, 1015
	bgt r7, r8, .LBB24_40
.LBB24_36:
	sub r4, r4, r12
	addi r7, r4, 2
	addi r4, r0, 1016
.LBB24_37:
	add r8, r7, r0
	addi r7, sp, 32
	add r7, r7, r6
	ldbu r7, r7+0
	stb r5+0, r7
	addi r6, r6, 1
	addi r5, r5, 1
	bge r6, r17, .LBB24_39
.LBB24_38:
	addi r7, r8, 1
	blt r8, r4, .LBB24_37
.LBB24_39:
	addi r4, r5, -1
.LBB24_40:
	srai r6, r1, 31
	xor r7, r1, r6
	sub r7, r7, r6
	lui r6, %hi(fio_e)
	ldw r6, r6+%lo(fio_e)
	addi r8, r0, 100
	sltu r8, r7, r8
	bne r6, r0, .LBB24_43
.LBB24_41:
	addi r9, r7, -100
	addi r10, r0, 900
	bgeu r9, r10, .LBB24_43
.LBB24_42:
	add r4, r5, r0
	jal r0, .LBB24_44
.LBB24_43:
	addi r4, r4, 2
	stb r5+0, r11
.LBB24_44:
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
.LBB24_45:
	add lr, r1, r0
	mulhu r1, r7, r8
	srli r11, r1, 3
	mul r1, r11, r9
	sub r1, r7, r1
	ori  r12, r1, 48
	addi r1, lr, 1
	addi r14, sp, 2047
	addi r13, r14, 49
	add r13, r13, lr
	stb r13+0, r12
	bltu r7, r9, .LBB24_47
.LBB24_46:
	add r7, r11, r0
	bltu lr, r10, .LBB24_45
.LBB24_47:
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
	addi r7, sp, 2047
	addi r6, r7, 49
	bge r1, r5, .LBB24_51
.LBB24_48:
	add r6, r6, r1
	addi r7, r0, 48
	add r8, r5, r0
.LBB24_49:
	stb r6+0, r7
	addi r6, r6, 1
	addi r8, r8, -1
	bne r1, r8, .LBB24_49
.LBB24_50:
	add r1, r5, r0
.LBB24_51:
	addi r4, r4, 1
	addi r6, sp, 2047
	addi r5, r6, 49
	addi r5, r5, -1
.LBB24_52:
	add r6, r1, r0
	addi r1, r1, -1
	add r7, r5, r6
	ldbu r7, r7+0
	stb r4+0, r7
	addi r4, r4, 1
	bgt r6, r3, .LBB24_52
.LBB24_53:
	stb r4+0, r0
	addi r1, sp, 2047
	ldw r27, r1+65
	ldw r26, r1+69
	ldw r24, r1+73
	ldw r23, r1+77
	ldw r22, r1+81
	ldw r21, r1+85
	ldw r20, r1+89
	ldw r19, r1+93
	ldw r18, r1+97
	ldw r17, r1+101
	ldw r16, r1+105
	ldw r15, r1+109
	ldw r14, r1+113
	ldw r13, r1+117
	ldw r12, r1+121
	ldw r11, r1+125
	ldw lr, sp+0
	addi sp, sp, 2047
	addi sp, sp, 129
	jalr r0, r31, 0
.LBB24_54:
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
	beq r23, r22, .LBB24_30
	jal r0, .LBB24_31
.Lfunc_end24:
	.size	fio_esn, .Lfunc_end24-fio_esn
                                        # -- End function
	.globl	f77_wr_r                        # -- Begin function f77_wr_r
	.p2align	2
	.type	f77_wr_r,@function
f77_wr_r:                               # @f77_wr_r
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r3
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB25_2
.LBB25_1:
	fcvt.d.s r4, r3
	add r3, r4, r0
	add r4, r5, r0
	add r5, r0, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fio_list_out_real
.LBB25_2:
	lui r1, %hi(fio_eor)
	ldbu r4, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r4, r0, .LBB25_5
.LBB25_3:
	lui r4, %hi(fio_desc)
	ldw r1, r4+%lo(fio_desc)
	beq r1, r0, .LBB25_7
.LBB25_4:
	lui r5, %hi(fio_rep)
	ldw r5, r5+%lo(fio_rep)
	ble r5, r0, .LBB25_7
.LBB25_5:
	addi r4, r0, 65
	bne r1, r4, .LBB25_8
.LBB25_6:
	addi r3, sp, 24
	addi r4, r0, 4
	jal r31, fio_hol_out
	jal r0, .LBB25_9
.LBB25_7:
	stw r4+%lo(fio_desc), r0
	add r11, r3, r0
	jal r31, fio_next_desc
	add r3, r11, r0
	addi r4, r0, 65
	beq r1, r4, .LBB25_6
.LBB25_8:
	lui r11, %hi(fio_single)
	addi r1, r0, 1
	stb r11+%lo(fio_single), r1
	fcvt.d.s r4, r3
	add r3, r4, r0
	add r4, r5, r0
	jal r31, f77_wr_d
	stb r11+%lo(fio_single), r0
.LBB25_9:
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end25:
	.size	f77_wr_r, .Lfunc_end25-f77_wr_r
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
	beq r1, r0, .LBB26_2
.LBB26_1:
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
.LBB26_2:
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
.Lfunc_end26:
	.size	f77_wr_c, .Lfunc_end26-f77_wr_c
                                        # -- End function
	.p2align	2                               # -- Begin function fio_list_cplx
	.type	fio_list_cplx,@function
fio_list_cplx:                          # @fio_list_cplx
# %bb.0:
	addi sp, sp, -112
	stw sp+0, lr
	stw sp+108, r11
	stw sp+104, r12
	stw sp+100, r13
	stw sp+96, r14
	stw sp+92, r15
	stw sp+88, r16
	stw sp+84, r17
	stw sp+80, r18
	add r11, r7, r0
	add r12, r6, r0
	add r13, r5, r0
	add r6, r4, r0
	add r5, r3, r0
	lui r14, %hi(fio_col)
	ldw r1, r14+%lo(fio_col)
	addi r17, r0, 1022
	lui r15, %hi(fio_len)
	lui r16, %hi(fio_line)
	addi r16, r16, %lo(fio_line)
	bgt r1, r17, .LBB27_6
.LBB27_1:
	ldw r4, r15+%lo(fio_len)
	addi r3, r0, 32
	bge r4, r1, .LBB27_4
.LBB27_2:
	addi r7, r4, 1
	add r4, r4, r16
	stb r4+0, r3
	add r4, r7, r0
	bne r1, r7, .LBB27_2
.LBB27_3:
	stw r15+%lo(fio_len), r1
	add r4, r1, r0
.LBB27_4:
	add r7, r1, r16
	stb r7+0, r3
	addi r3, r1, 1
	stw r14+%lo(fio_col), r3
	blt r1, r4, .LBB27_6
.LBB27_5:
	stw r15+%lo(fio_len), r3
.LBB27_6:
	ldw r18, r15+%lo(fio_len)
	ldw r1, r14+%lo(fio_col)
	bgt r1, r17, .LBB27_13
.LBB27_7:
	add r3, r18, r0
	bge r18, r1, .LBB27_11
.LBB27_8:
	addi r3, r0, 32
	add r4, r18, r0
.LBB27_9:
	addi r7, r4, 1
	add r4, r4, r16
	stb r4+0, r3
	add r4, r7, r0
	bne r1, r7, .LBB27_9
.LBB27_10:
	stw r15+%lo(fio_len), r1
	add r3, r1, r0
.LBB27_11:
	add r4, r1, r16
	addi r7, r0, 40
	stb r4+0, r7
	addi r4, r1, 1
	stw r14+%lo(fio_col), r4
	blt r1, r3, .LBB27_13
.LBB27_12:
	stw r15+%lo(fio_len), r4
.LBB27_13:
	addi r3, sp, 16
	addi r4, r0, 64
	add r7, r11, r0
	jal r31, fio_list_real_text
	ldbu r6, sp+16
	beq r6, r0, .LBB27_23
.LBB27_14:
	ldw r4, r15+%lo(fio_len)
	ldw r5, r14+%lo(fio_col)
	addi r1, sp, 16
	addi r3, r0, 32
	jal r0, .LBB27_17
.LBB27_15:
	add r5, r6, r0
.LBB27_16:
	addi r1, r1, 1
	ldbu r6, r1+0
	beq r6, r0, .LBB27_23
.LBB27_17:
	bgt r5, r17, .LBB27_16
.LBB27_18:
	bge r4, r5, .LBB27_21
.LBB27_19:
	addi r7, r4, 1
	add r4, r4, r16
	stb r4+0, r3
	add r4, r7, r0
	bne r5, r7, .LBB27_19
.LBB27_20:
	stw r15+%lo(fio_len), r5
	add r4, r5, r0
.LBB27_21:
	add r7, r5, r16
	stb r7+0, r6
	addi r6, r5, 1
	stw r14+%lo(fio_col), r6
	blt r5, r4, .LBB27_15
.LBB27_22:
	stw r15+%lo(fio_len), r6
	add r4, r6, r0
	jal r0, .LBB27_15
.LBB27_23:
	ldw r1, r14+%lo(fio_col)
	bgt r1, r17, .LBB27_30
.LBB27_24:
	ldw r3, r15+%lo(fio_len)
	bge r3, r1, .LBB27_28
.LBB27_25:
	addi r4, r0, 32
.LBB27_26:
	addi r5, r3, 1
	add r3, r3, r16
	stb r3+0, r4
	add r3, r5, r0
	bne r1, r5, .LBB27_26
.LBB27_27:
	stw r15+%lo(fio_len), r1
	add r3, r1, r0
.LBB27_28:
	add r4, r1, r16
	addi r5, r0, 44
	stb r4+0, r5
	addi r4, r1, 1
	stw r14+%lo(fio_col), r4
	blt r1, r3, .LBB27_30
.LBB27_29:
	stw r15+%lo(fio_len), r4
.LBB27_30:
	addi r3, sp, 16
	addi r4, r0, 64
	add r5, r13, r0
	add r6, r12, r0
	add r7, r11, r0
	jal r31, fio_list_real_text
	ldbu r6, sp+16
	beq r6, r0, .LBB27_40
.LBB27_31:
	ldw r4, r15+%lo(fio_len)
	ldw r5, r14+%lo(fio_col)
	addi r1, sp, 16
	addi r3, r0, 32
	jal r0, .LBB27_34
.LBB27_32:
	add r5, r6, r0
.LBB27_33:
	addi r1, r1, 1
	ldbu r6, r1+0
	beq r6, r0, .LBB27_40
.LBB27_34:
	bgt r5, r17, .LBB27_33
.LBB27_35:
	bge r4, r5, .LBB27_38
.LBB27_36:
	addi r7, r4, 1
	add r4, r4, r16
	stb r4+0, r3
	add r4, r7, r0
	bne r5, r7, .LBB27_36
.LBB27_37:
	stw r15+%lo(fio_len), r5
	add r4, r5, r0
.LBB27_38:
	add r7, r5, r16
	stb r7+0, r6
	addi r6, r5, 1
	stw r14+%lo(fio_col), r6
	blt r5, r4, .LBB27_32
.LBB27_39:
	stw r15+%lo(fio_len), r6
	add r4, r6, r0
	jal r0, .LBB27_32
.LBB27_40:
	ldw r1, r14+%lo(fio_col)
	bgt r1, r17, .LBB27_47
.LBB27_41:
	ldw r3, r15+%lo(fio_len)
	bge r3, r1, .LBB27_45
.LBB27_42:
	addi r4, r0, 32
.LBB27_43:
	addi r5, r3, 1
	add r3, r3, r16
	stb r3+0, r4
	add r3, r5, r0
	bne r1, r5, .LBB27_43
.LBB27_44:
	stw r15+%lo(fio_len), r1
	add r3, r1, r0
.LBB27_45:
	add r4, r1, r16
	addi r5, r0, 41
	stb r4+0, r5
	addi r4, r1, 1
	stw r14+%lo(fio_col), r4
	blt r1, r3, .LBB27_47
.LBB27_46:
	stw r15+%lo(fio_len), r4
.LBB27_47:
	ldw r1, r15+%lo(fio_len)
	sub r5, r1, r18
	addi r3, r0, 0
	seq r3, r11, r3
	sub r3, r0, r3
	andi r3, r3, 22
	xori r3, r3, 53
	sub r11, r3, r5
	addi r3, r0, 1
	blt r11, r3, .LBB27_50
.LBB27_48:
	add r1, r11, r1
	bgt r1, r17, .LBB27_50
.LBB27_49:
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
.LBB27_50:
	ldw r1, r15+%lo(fio_len)
	stw r14+%lo(fio_col), r1
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	ldw r18, sp+80
	ldw r17, sp+84
	ldw r16, sp+88
	ldw r15, sp+92
	ldw r14, sp+96
	ldw r13, sp+100
	ldw r12, sp+104
	ldw r11, sp+108
	ldw lr, sp+0
	addi sp, sp, 112
	jalr r0, r31, 0
.Lfunc_end27:
	.size	fio_list_cplx, .Lfunc_end27-fio_list_cplx
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
	beq r1, r0, .LBB28_2
.LBB28_1:
	addi r7, r0, 1
	add r5, r12, r0
	add r6, r11, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fio_list_cplx
.LBB28_2:
	jal r31, f77_wr_d
	add r3, r12, r0
	add r4, r11, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_wr_d
.Lfunc_end28:
	.size	f77_wr_z, .Lfunc_end28-f77_wr_z
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
	beq r1, r0, .LBB29_4
.LBB29_1:
	lui r1, %hi(fio_unit)
	ldw r5, r1+%lo(fio_unit)
	addi r1, r5, -100
	addi r6, r0, -99
	bltu r1, r6, .LBB29_21
.LBB29_2:
	lui r1, %hi(fio_ibuf)
	ldw r6, r1+%lo(fio_ibuf)
	add r1, r0, r0
	bne r6, r0, .LBB29_22
.LBB29_3:
	slli r1, r5, 2
	lui r5, %hi(fio_udelim)
	addi r5, r5, %lo(fio_udelim)
	add r1, r1, r5
	ldw r1, r1+0
	jal r0, .LBB29_22
.LBB29_4:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB29_82
.LBB29_5:
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB29_7
.LBB29_6:
	ldw r1, r12+%lo(fio_rep)
	bgt r1, r0, .LBB29_8
.LBB29_7:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	add r14, r4, r0
	jal r31, fio_next_desc
	add r4, r14, r0
	add r3, r13, r0
	beq r1, r0, .LBB29_82
.LBB29_8:
	lui r1, %hi(fio_w)
	ldw r5, r1+%lo(fio_w)
	addi r1, r0, 1
	blt r5, r1, .LBB29_10
.LBB29_9:
	blt r5, r4, .LBB29_84
.LBB29_10:
	ble r5, r4, .LBB29_83
.LBB29_11:
	sub r14, r5, r4
	blt r14, r1, .LBB29_83
.LBB29_12:
	lui r5, %hi(fio_len)
	ldw r10, r5+%lo(fio_len)
	lui r6, %hi(fio_col)
	ldw lr, r6+%lo(fio_col)
	addi r7, r0, 1022
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	addi r9, r0, 32
	jal r0, .LBB29_15
.LBB29_13:
	add lr, r14, r0
.LBB29_14:
	addi r14, r13, -1
	ble r13, r1, .LBB29_83
.LBB29_15:
	add r13, r14, r0
	bgt lr, r7, .LBB29_14
.LBB29_16:
	bge r10, lr, .LBB29_19
.LBB29_17:
	addi r14, r10, 1
	add r10, r10, r8
	stb r10+0, r9
	add r10, r14, r0
	bne lr, r14, .LBB29_17
.LBB29_18:
	stw r5+%lo(fio_len), lr
	add r10, lr, r0
.LBB29_19:
	add r14, lr, r8
	stb r14+0, r9
	addi r14, lr, 1
	stw r6+%lo(fio_col), r14
	blt lr, r10, .LBB29_13
.LBB29_20:
	stw r5+%lo(fio_len), r14
	add r10, r14, r0
	jal r0, .LBB29_13
.LBB29_21:
	add r1, r0, r0
.LBB29_22:
	lui r5, %hi(fio_first_item)
	lui r6, %hi(fio_prev_char)
	beq r1, r0, .LBB29_62
.LBB29_23:
	lui r8, %hi(fio_col)
	ldw lr, r8+%lo(fio_col)
	addi r10, r0, 1022
	lui r7, %hi(fio_len)
	lui r9, %hi(fio_line)
	addi r9, r9, %lo(fio_line)
	bgt lr, r10, .LBB29_29
.LBB29_24:
	ldw r12, r7+%lo(fio_len)
	addi r11, r0, 32
	bge r12, lr, .LBB29_27
.LBB29_25:
	addi r13, r12, 1
	add r12, r12, r9
	stb r12+0, r11
	add r12, r13, r0
	bne lr, r13, .LBB29_25
.LBB29_26:
	stw r7+%lo(fio_len), lr
	add r12, lr, r0
.LBB29_27:
	add r13, lr, r9
	stb r13+0, r11
	addi r11, lr, 1
	stw r8+%lo(fio_col), r11
	blt lr, r12, .LBB29_29
.LBB29_28:
	stw r7+%lo(fio_len), r11
.LBB29_29:
	ldw lr, r8+%lo(fio_col)
	bgt lr, r10, .LBB29_36
.LBB29_30:
	ldw r11, r7+%lo(fio_len)
	bge r11, lr, .LBB29_34
.LBB29_31:
	addi r12, r0, 32
.LBB29_32:
	addi r13, r11, 1
	add r11, r11, r9
	stb r11+0, r12
	add r11, r13, r0
	bne lr, r13, .LBB29_32
.LBB29_33:
	stw r7+%lo(fio_len), lr
	add r11, lr, r0
.LBB29_34:
	add r12, lr, r9
	stb r12+0, r1
	addi r12, lr, 1
	stw r8+%lo(fio_col), r12
	blt lr, r11, .LBB29_36
.LBB29_35:
	stw r7+%lo(fio_len), r12
.LBB29_36:
	addi lr, r0, 1
	blt r4, lr, .LBB29_55
.LBB29_37:
	ldw r12, r7+%lo(fio_len)
	ldw r13, r8+%lo(fio_col)
	addi lr, r0, 32
	add r11, r0, r0
	jal r0, .LBB29_40
.LBB29_38:
	add r13, r15, r0
.LBB29_39:
	addi r11, r11, 1
	beq r11, r4, .LBB29_55
.LBB29_40:
	add r14, r3, r11
	ldb r15, r14+0
	bne r1, r15, .LBB29_54
.LBB29_41:
	bgt r13, r10, .LBB29_47
.LBB29_42:
	bge r12, r13, .LBB29_45
.LBB29_43:
	addi r15, r12, 1
	add r12, r12, r9
	stb r12+0, lr
	add r12, r15, r0
	bne r13, r15, .LBB29_43
.LBB29_44:
	stw r7+%lo(fio_len), r13
	add r12, r13, r0
.LBB29_45:
	add r15, r13, r9
	stb r15+0, r1
	addi r15, r13, 1
	stw r8+%lo(fio_col), r15
	blt r13, r12, .LBB29_48
.LBB29_46:
	stw r7+%lo(fio_len), r15
	add r12, r15, r0
	bgt r15, r10, .LBB29_38
	jal r0, .LBB29_49
.LBB29_47:
	add r15, r13, r0
.LBB29_48:
	bgt r15, r10, .LBB29_38
.LBB29_49:
	ldbu r13, r14+0
	bge r12, r15, .LBB29_52
.LBB29_50:
	addi r14, r12, 1
	add r12, r12, r9
	stb r12+0, lr
	add r12, r14, r0
	bne r15, r14, .LBB29_50
.LBB29_51:
	stw r7+%lo(fio_len), r15
	add r12, r15, r0
.LBB29_52:
	add r14, r15, r9
	stb r14+0, r13
	addi r13, r15, 1
	stw r8+%lo(fio_col), r13
	blt r15, r12, .LBB29_39
.LBB29_53:
	stw r7+%lo(fio_len), r13
	add r12, r13, r0
	jal r0, .LBB29_39
.LBB29_54:
	add r15, r13, r0
	bgt r15, r10, .LBB29_38
	jal r0, .LBB29_49
.LBB29_55:
	ldw r3, r8+%lo(fio_col)
	bgt r3, r10, .LBB29_81
.LBB29_56:
	ldw r4, r7+%lo(fio_len)
	bge r4, r3, .LBB29_60
.LBB29_57:
	addi r10, r0, 32
.LBB29_58:
	addi lr, r4, 1
	add r4, r4, r9
	stb r4+0, r10
	add r4, lr, r0
	bne r3, lr, .LBB29_58
.LBB29_59:
	stw r7+%lo(fio_len), r3
	add r4, r3, r0
.LBB29_60:
	add r9, r3, r9
	stb r9+0, r1
	addi r9, r3, 1
	stw r8+%lo(fio_col), r9
	blt r3, r4, .LBB29_81
.LBB29_61:
	stw r7+%lo(fio_len), r9
	jal r0, .LBB29_81
.LBB29_62:
	ldbu r7, r5+%lo(fio_first_item)
	bne r7, r0, .LBB29_64
.LBB29_63:
	ldbu r7, r6+%lo(fio_prev_char)
	andi r7, r7, 1
	bne r7, r0, .LBB29_71
.LBB29_64:
	lui r8, %hi(fio_col)
	ldw r7, r8+%lo(fio_col)
	addi r9, r0, 1022
	bgt r7, r9, .LBB29_71
.LBB29_65:
	lui r9, %hi(fio_len)
	ldw r10, r9+%lo(fio_len)
	bge r10, r7, .LBB29_69
.LBB29_66:
	lui lr, %hi(fio_line)
	addi lr, lr, %lo(fio_line)
	addi r11, r0, 32
.LBB29_67:
	addi r12, r10, 1
	add r10, r10, lr
	stb r10+0, r11
	add r10, r12, r0
	bne r7, r12, .LBB29_67
.LBB29_68:
	stw r9+%lo(fio_len), r7
	add r10, r7, r0
.LBB29_69:
	lui lr, %hi(fio_line)
	addi lr, lr, %lo(fio_line)
	add lr, r7, lr
	addi r11, r0, 32
	stb lr+0, r11
	addi lr, r7, 1
	stw r8+%lo(fio_col), lr
	blt r7, r10, .LBB29_71
.LBB29_70:
	stw r9+%lo(fio_len), lr
.LBB29_71:
	addi r7, r0, 1
	blt r4, r7, .LBB29_81
.LBB29_72:
	lui r7, %hi(fio_len)
	ldw lr, r7+%lo(fio_len)
	lui r8, %hi(fio_col)
	ldw r13, r8+%lo(fio_col)
	addi r9, r0, 1022
	lui r10, %hi(fio_line)
	addi r10, r10, %lo(fio_line)
	addi r11, r0, 32
	add r12, r0, r0
	jal r0, .LBB29_75
.LBB29_73:
	add r13, r14, r0
.LBB29_74:
	addi r12, r12, 1
	beq r12, r4, .LBB29_81
.LBB29_75:
	bgt r13, r9, .LBB29_74
.LBB29_76:
	add r14, r3, r12
	ldbu r14, r14+0
	bge lr, r13, .LBB29_79
.LBB29_77:
	addi r15, lr, 1
	add lr, lr, r10
	stb lr+0, r11
	add lr, r15, r0
	bne r13, r15, .LBB29_77
.LBB29_78:
	stw r7+%lo(fio_len), r13
	add lr, r13, r0
.LBB29_79:
	add r15, r13, r10
	stb r15+0, r14
	addi r14, r13, 1
	stw r8+%lo(fio_col), r14
	blt r13, lr, .LBB29_73
.LBB29_80:
	stw r7+%lo(fio_len), r14
	add lr, r14, r0
	jal r0, .LBB29_73
.LBB29_81:
	addi r3, r0, 0
	seq r1, r1, r3
	stb r5+%lo(fio_first_item), r0
	stb r6+%lo(fio_prev_char), r1
.LBB29_82:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB29_83:
	add r5, r4, r0
.LBB29_84:
	blt r5, r1, .LBB29_94
.LBB29_85:
	lui r4, %hi(fio_len)
	ldw r9, r4+%lo(fio_len)
	lui r6, %hi(fio_col)
	ldw r13, r6+%lo(fio_col)
	addi r7, r0, 1022
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	addi r10, r0, 32
	add lr, r0, r0
	jal r0, .LBB29_88
.LBB29_86:
	add r13, r14, r0
.LBB29_87:
	addi lr, lr, 1
	beq lr, r5, .LBB29_94
.LBB29_88:
	bgt r13, r7, .LBB29_87
.LBB29_89:
	add r14, r3, lr
	ldbu r14, r14+0
	bge r9, r13, .LBB29_92
.LBB29_90:
	addi r15, r9, 1
	add r9, r9, r8
	stb r9+0, r10
	add r9, r15, r0
	bne r13, r15, .LBB29_90
.LBB29_91:
	stw r4+%lo(fio_len), r13
	add r9, r13, r0
.LBB29_92:
	add r15, r13, r8
	stb r15+0, r14
	addi r14, r13, 1
	stw r6+%lo(fio_col), r14
	blt r13, r9, .LBB29_86
.LBB29_93:
	stw r4+%lo(fio_len), r14
	add r9, r14, r0
	jal r0, .LBB29_86
.LBB29_94:
	ldw r3, r12+%lo(fio_rep)
	blt r3, r1, .LBB29_96
.LBB29_95:
	addi r1, r3, -1
	stw r12+%lo(fio_rep), r1
.LBB29_96:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB29_82
.LBB29_97:
	stw r11+%lo(fio_desc), r0
	jal r0, .LBB29_82
.Lfunc_end29:
	.size	f77_wr_a, .Lfunc_end29-f77_wr_a
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
.Lfunc_end30:
	.size	f77_cabs, .Lfunc_end30-f77_cabs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cdiv
	.type	.LCPI31_0,@object
.LCPI31_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI31_0, 8
	.type	.LCPI31_1,@object
.LCPI31_1:
	.quad	0x43f0000000000000              # double 1.8446744073709552E+19
	.size	.LCPI31_1, 8
	.type	.LCPI31_2,@object
.LCPI31_2:
	.quad	0x3bf0000000000000              # double 5.4210108624275222E-20
	.size	.LCPI31_2, 8
	.type	.LCPI31_3,@object
.LCPI31_3:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI31_3, 8
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
	lui r1, %hi(.LCPI31_0)
	addi r1, r1, %lo(.LCPI31_0)
	ldw r5, r1+4
	ldw r4, r1+0
	lui r1, %hi(.LCPI31_1)
	addi r1, r1, %lo(.LCPI31_1)
	ldw r9, r1+4
	ldw r8, r1+0
	flt.d r1, r8, r6
	xori r3, r1, 1
	lui r1, %hi(.LCPI31_2)
	addi r1, r1, %lo(.LCPI31_2)
	bne r3, r0, .LBB31_2
.LBB31_1:
	ldw r19, r1+4
	ldw r18, r1+0
	fmul.d r14, r14, r18
	fmul.d r16, r16, r18
	fmul.d r6, r6, r18
	fmul.d r4, r4, r18
	flt.d r3, r8, r6
	bne r3, r0, .LBB31_1
.LBB31_2:
	lui r3, %hi(.LCPI31_3)
	addi r3, r3, %lo(.LCPI31_3)
	ldw r19, r3+4
	ldw r18, r3+0
	flt.d r3, r18, r6
	xori r3, r3, 1
	bne r3, r0, .LBB31_6
.LBB31_3:
	ldw r21, r1+4
	ldw r20, r1+0
	flt.d r1, r6, r20
	xori r1, r1, 1
	bne r1, r0, .LBB31_6
.LBB31_4:
	fmul.d r14, r14, r8
	fmul.d r16, r16, r8
	fmul.d r6, r6, r8
	fmul.d r4, r4, r8
	fle.d r1, r6, r18
	bne r1, r0, .LBB31_6
.LBB31_5:
	flt.d r1, r6, r20
	bne r1, r0, .LBB31_4
.LBB31_6:
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
.Lfunc_end31:
	.size	f77_cdiv, .Lfunc_end31-f77_cdiv
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpow_i
	.type	.LCPI32_0,@object
.LCPI32_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI32_0, 8
	.type	.LCPI32_1,@object
.LCPI32_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI32_1, 8
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
	lui r1, %hi(.LCPI32_0)
	addi r1, r1, %lo(.LCPI32_0)
	ldw r11, r1+4
	ldw r10, r1+0
	lui r1, %hi(.LCPI32_1)
	addi r1, r1, %lo(.LCPI32_1)
	ldw r9, r1+4
	ldw r8, r1+0
	beq r6, r0, .LBB32_7
.LBB32_1:
	srai r1, r6, 31
	xor r7, r6, r1
	sub r1, r7, r1
	fcvt.d.s r12, r5
	fcvt.d.s r4, r4
	jal r0, .LBB32_3
.LBB32_2:
	beq r1, r0, .LBB32_7
.LBB32_3:
	andi r7, r1, 1
	beq r7, r0, .LBB32_5
.LBB32_4:
	fmul.d r14, r4, r10
	fmul.d r16, r12, r8
	fsub.d r14, r14, r16
	fmul.d r10, r12, r10
	fmul.d r8, r4, r8
	fadd.d r8, r8, r10
	add r10, r14, r0
	add r11, r15, r0
.LBB32_5:
	srli r1, r1, 1
	beq r1, r0, .LBB32_2
.LBB32_6:
	fmul.d r14, r4, r4
	fmul.d r16, r12, r12
	fsub.d r14, r14, r16
	fadd.d r4, r4, r4
	fmul.d r12, r12, r4
	add r4, r14, r0
	add r5, r15, r0
	jal r0, .LBB32_2
.LBB32_7:
	addi r1, r0, -1
	bgt r6, r1, .LBB32_9
.LBB32_8:
	fmul.d r4, r10, r10
	fmul.d r6, r8, r8
	fadd.d r4, r6, r4
	fdiv.d r10, r10, r4
	fneg.d r6, r8
	fdiv.d r8, r6, r4
.LBB32_9:
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
.Lfunc_end32:
	.size	f77_cpow_i, .Lfunc_end32-f77_cpow_i
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
.Lfunc_end33:
	.size	f77_clog, .Lfunc_end33-f77_clog
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
.Lfunc_end34:
	.size	f77_cexp, .Lfunc_end34-f77_cexp
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_cpow_c
	.type	.LCPI35_0,@object
.LCPI35_0:
	.word	0x00000000                      # float 0
	.size	.LCPI35_0, 4
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
	lui r1, %hi(.LCPI35_0)
	ldw r1, r1+%lo(.LCPI35_0)
	feq.s r3, r4, r1
	xori r3, r3, 1
	bne r3, r0, .LBB35_3
.LBB35_1:
	feq.s r3, r5, r1
	xori r3, r3, 1
	bne r3, r0, .LBB35_3
.LBB35_2:
	stw r11+0, r0
	jal r0, .LBB35_4
.LBB35_3:
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
.LBB35_4:
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
.Lfunc_end35:
	.size	f77_cpow_c, .Lfunc_end35-f77_cpow_c
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_csqrt
	.type	.LCPI36_0,@object
.LCPI36_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI36_0, 8
	.type	.LCPI36_2,@object
.LCPI36_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI36_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI36_1,@object
.LCPI36_1:
	.word	0x00000000                      # float 0
	.size	.LCPI36_1, 4
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
	lui r1, %hi(.LCPI36_0)
	addi r1, r1, %lo(.LCPI36_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r16, r1, r0
	add r17, r2, r0
	lui r3, %hi(.LCPI36_1)
	ldw r1, r3+%lo(.LCPI36_1)
	fle.s r4, r1, r13
	xori r4, r4, 1
	beq r4, r0, .LBB36_4
.LBB36_1:
	ldw r13, r3+%lo(.LCPI36_1)
	lui r1, %hi(.LCPI36_2)
	addi r1, r1, %lo(.LCPI36_2)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r16, r4
	add r1, r13, r0
	bne r3, r0, .LBB36_3
.LBB36_2:
	add r3, r14, r0
	add r4, r15, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r16, r16
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
.LBB36_3:
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
	jal r0, .LBB36_6
.LBB36_4:
	fcvt.s.d r3, r16
	stw r11+0, r3
	lui r3, %hi(.LCPI36_2)
	addi r3, r3, %lo(.LCPI36_2)
	ldw r5, r3+4
	ldw r4, r3+0
	feq.d r3, r16, r4
	bne r3, r0, .LBB36_6
.LBB36_5:
	fadd.d r4, r16, r16
	fdiv.d r4, r14, r4
	fcvt.s.d r1, r4
.LBB36_6:
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
.Lfunc_end36:
	.size	f77_csqrt, .Lfunc_end36-f77_csqrt
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
.Lfunc_end37:
	.size	f77_csin, .Lfunc_end37-f77_csin
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
.Lfunc_end38:
	.size	f77_ccos, .Lfunc_end38-f77_ccos
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
	beq r1, r0, .LBB39_2
.LBB39_1:
	fdiv.d r6, r16, r18
	fmul.d r4, r16, r6
	fadd.d r8, r18, r4
	fmul.d r4, r12, r6
	fadd.d r4, r14, r4
	fdiv.d r4, r4, r8
	fmul.d r6, r14, r6
	fsub.d r6, r6, r12
	jal r0, .LBB39_3
.LBB39_2:
	fdiv.d r6, r18, r16
	fmul.d r4, r18, r6
	fadd.d r8, r16, r4
	fmul.d r4, r14, r6
	fadd.d r4, r12, r4
	fdiv.d r4, r4, r8
	fmul.d r6, r12, r6
	fsub.d r6, r14, r6
.LBB39_3:
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
.Lfunc_end39:
	.size	f77_zdiv, .Lfunc_end39-f77_zdiv
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zpow_i
	.type	.LCPI40_0,@object
.LCPI40_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI40_0, 8
	.type	.LCPI40_1,@object
.LCPI40_1:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI40_1, 8
	.type	.LCPI40_2,@object
.LCPI40_2:
	.quad	0xbff0000000000000              # double -1
	.size	.LCPI40_2, 8
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
	lui r1, %hi(.LCPI40_0)
	addi r1, r1, %lo(.LCPI40_0)
	ldw r13, r1+4
	ldw r12, r1+0
	lui r1, %hi(.LCPI40_1)
	addi r1, r1, %lo(.LCPI40_1)
	ldw r15, r1+4
	ldw r14, r1+0
	beq r5, r0, .LBB40_11
.LBB40_1:
	ldw r17, r4+4
	ldw r16, r4+0
	ldw r19, r4+12
	ldw r18, r4+8
	addi r1, r0, -1
	bgt r5, r1, .LBB40_6
.LBB40_2:
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
	beq r1, r0, .LBB40_4
.LBB40_3:
	fdiv.d r4, r16, r18
	fmul.d r6, r16, r4
	fadd.d r6, r18, r6
	fadd.d r8, r4, r12
	fdiv.d r16, r8, r6
	fmul.d r4, r4, r12
	lui r1, %hi(.LCPI40_2)
	addi r1, r1, %lo(.LCPI40_2)
	ldw r9, r1+4
	ldw r8, r1+0
	fadd.d r4, r4, r8
	jal r0, .LBB40_5
.LBB40_4:
	fdiv.d r4, r18, r16
	fmul.d r6, r18, r4
	fadd.d r6, r16, r6
	fmul.d r8, r4, r12
	fadd.d r8, r8, r14
	fdiv.d r16, r8, r6
	fsub.d r4, r12, r4
.LBB40_5:
	fdiv.d r18, r4, r6
	add r3, r11, r0
	add r5, r20, r0
.LBB40_6:
	andi r1, r5, 1
	bne r1, r0, .LBB40_8
.LBB40_7:
	add r6, r14, r0
	add r7, r15, r0
	srli r5, r5, 1
	bne r5, r0, .LBB40_9
	jal r0, .LBB40_10
.LBB40_8:
	fmul.d r6, r14, r16
	fmul.d r8, r12, r18
	fsub.d r6, r6, r8
	fmul.d r8, r14, r18
	fmul.d r10, r12, r16
	fadd.d r12, r10, r8
	srli r5, r5, 1
	beq r5, r0, .LBB40_10
.LBB40_9:
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
	beq r1, r0, .LBB40_7
	jal r0, .LBB40_8
.LBB40_10:
	add r14, r6, r0
	add r15, r7, r0
.LBB40_11:
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
.Lfunc_end40:
	.size	f77_zpow_i, .Lfunc_end40-f77_zpow_i
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zpow_z
	.type	.LCPI41_0,@object
.LCPI41_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI41_0, 8
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
	lui r1, %hi(.LCPI41_0)
	addi r1, r1, %lo(.LCPI41_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r8, r6
	xori r1, r1, 1
	bne r1, r0, .LBB41_3
.LBB41_1:
	ldw r15, r4+12
	ldw r14, r4+8
	feq.d r1, r14, r6
	xori r1, r1, 1
	bne r1, r0, .LBB41_3
.LBB41_2:
	stw r11+12, r0
	stw r11+8, r0
	stw r11+4, r0
	stw r11+0, r0
	jal r0, .LBB41_4
.LBB41_3:
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
.LBB41_4:
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
.Lfunc_end41:
	.size	f77_zpow_z, .Lfunc_end41-f77_zpow_z
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
.Lfunc_end42:
	.size	f77_zabs, .Lfunc_end42-f77_zabs
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
	add r1, r3, r0
	beq r5, r0, .LBB43_2
.LBB43_1:
	ldw r3, r5+0
	bne r3, r0, .LBB43_6
.LBB43_2:
	beq r1, r0, .LBB43_5
.LBB43_3:
	beq r5, r0, .LBB43_9
.LBB43_4:
	lui r3, 1
	addi r3, r3, 918
	stw r5+0, r3
	jal r0, .LBB43_6
.LBB43_5:
	add r12, r5, r0
	addi r1, r0, 1
	sgt r1, r4, r1
	sub r1, r0, r1
	add r11, r4, r0
	xori r3, r4, 1
	and r1, r3, r1
	xori r3, r1, 1
	jal r31, malloc
	beq r1, r0, .LBB43_7
.LBB43_6:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB43_7:
	add r1, r0, r0
	add r5, r12, r0
	bne r12, r0, .LBB43_4
.LBB43_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r5, r11, r0
	jal r0, .LBB43_10
.LBB43_9:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
.LBB43_10:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end43:
	.size	f77_allocate, .Lfunc_end43-f77_allocate
                                        # -- End function
	.globl	f77_deallocate                  # -- Begin function f77_deallocate
	.p2align	2
	.type	f77_deallocate,@function
f77_deallocate:                         # @f77_deallocate
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	beq r4, r0, .LBB44_2
.LBB44_1:
	ldw r1, r4+0
	bne r1, r0, .LBB44_7
.LBB44_2:
	beq r3, r0, .LBB44_4
.LBB44_3:
	jal r31, free
	jal r0, .LBB44_6
.LBB44_4:
	beq r4, r0, .LBB44_8
.LBB44_5:
	addi r1, r0, 1
	stw r4+0, r1
.LBB44_6:
	add r3, r0, r0
.LBB44_7:
	add r1, r3, r0
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.LBB44_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end44:
	.size	f77_deallocate, .Lfunc_end44-f77_deallocate
                                        # -- End function
	.globl	f77_need_allocated              # -- Begin function f77_need_allocated
	.p2align	2
	.type	f77_need_allocated,@function
f77_need_allocated:                     # @f77_need_allocated
# %bb.0:
	beq r3, r0, .LBB45_2
.LBB45_1:
	jalr r0, r31, 0
.LBB45_2:
	addi sp, sp, -16
	stw sp+0, lr
	add r6, r4, r0
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.21)
	addi r4, r4, %lo(.L.str.21)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end45:
	.size	f77_need_allocated, .Lfunc_end45-f77_need_allocated
                                        # -- End function
	.globl	f77_dealloc_auto                # -- Begin function f77_dealloc_auto
	.p2align	2
	.type	f77_dealloc_auto,@function
f77_dealloc_auto:                       # @f77_dealloc_auto
# %bb.0:
	beq r3, r0, .LBB46_2
.LBB46_1:
	jal r0, free
.LBB46_2:
	jalr r0, r31, 0
.Lfunc_end46:
	.size	f77_dealloc_auto, .Lfunc_end46-f77_dealloc_auto
                                        # -- End function
	.globl	f77_dfl_fill                    # -- Begin function f77_dfl_fill
	.p2align	2
	.type	f77_dfl_fill,@function
f77_dfl_fill:                           # @f77_dfl_fill
# %bb.0:
	beq r3, r0, .LBB47_8
.LBB47_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r11, r6, r0
	addi r1, r0, 1
	blt r6, r1, .LBB47_7
.LBB47_2:
	add r12, r5, r0
	add r13, r4, r0
	add r14, r3, r0
	jal r0, .LBB47_5
.LBB47_3:
	add r4, r0, r0
	add r5, r12, r0
	jal r31, memset
.LBB47_4:
	addi r11, r11, -1
	add r14, r14, r12
	beq r11, r0, .LBB47_7
.LBB47_5:
	add r3, r14, r0
	beq r13, r0, .LBB47_3
.LBB47_6:
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcpy
	jal r0, .LBB47_4
.LBB47_7:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB47_8:
	jalr r0, r31, 0
.Lfunc_end47:
	.size	f77_dfl_fill, .Lfunc_end47-f77_dfl_fill
                                        # -- End function
	.globl	f77_dta_free                    # -- Begin function f77_dta_free
	.p2align	2
	.type	f77_dta_free,@function
f77_dta_free:                           # @f77_dta_free
# %bb.0:
	addi r1, r0, 1
	blt r4, r1, .LBB48_6
.LBB48_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	jal r0, .LBB48_3
.LBB48_2:
	addi r12, r12, -1
	add r13, r13, r11
	beq r12, r0, .LBB48_5
.LBB48_3:
	ldw r3, r13+0
	beq r3, r0, .LBB48_2
.LBB48_4:
	jal r31, free
	stw r13+0, r0
	jal r0, .LBB48_2
.LBB48_5:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB48_6:
	jalr r0, r31, 0
.Lfunc_end48:
	.size	f77_dta_free, .Lfunc_end48-f77_dta_free
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
	beq r1, r0, .LBB49_20
.LBB49_1:
	add r13, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	addi r1, r0, 1
	add r15, r1, r0
	blt r4, r1, .LBB49_4
.LBB49_2:
	addi r3, r12, 4
	addi r15, r0, 1
	addi r4, r0, 0
	add r5, r11, r0
.LBB49_3:
	ldw r6, r3+0
	sgt r7, r6, r4
	mul r6, r6, r15
	sub r7, r0, r7
	and r15, r6, r7
	addi r5, r5, -1
	addi r3, r3, 8
	bne r5, r0, .LBB49_3
.LBB49_4:
	mul r3, r15, r13
	xori r4, r3, 1
	sgt r1, r3, r1
	sub r1, r0, r1
	and r1, r4, r1
	xori r3, r1, 1
	jal r31, malloc
	beq r1, r0, .LBB49_21
.LBB49_5:
	add r14, r1, r0
	addi r3, sp, 28
	addi r5, r0, 28
	add r4, r0, r0
	jal r31, memset
	addi r16, r0, 1
	blt r15, r16, .LBB49_17
.LBB49_6:
	addi r17, r12, 8
	addi r18, r12, 4
	addi r19, r0, 1
	add r20, r0, r0
	jal r0, .LBB49_8
.LBB49_7:
	addi r20, r20, 1
	beq r20, r15, .LBB49_17
.LBB49_8:
	blt r11, r19, .LBB49_12
.LBB49_9:
	addi r1, sp, 28
	add r3, r17, r0
	add r5, r11, r0
	add r4, r0, r0
.LBB49_10:
	ldw r6, r1+0
	ldw r7, r3+0
	mul r6, r7, r6
	add r4, r6, r4
	addi r5, r5, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r5, r0, .LBB49_10
.LBB49_11:
	mul r1, r4, r13
	jal r0, .LBB49_13
.LBB49_12:
	add r1, r0, r0
.LBB49_13:
	mul r3, r20, r13
	add r3, r14, r3
	ldw r4, r12+0
	add r4, r4, r1
	add r5, r13, r0
	jal r31, memcpy
	blt r11, r19, .LBB49_7
.LBB49_14:
	addi r1, sp, 28
	add r3, r18, r0
	add r4, r11, r0
.LBB49_15:
	ldw r5, r1+0
	addi r5, r5, 1
	stw r1+0, r5
	ldw r6, r3+0
	blt r5, r6, .LBB49_7
.LBB49_16:
	stw r1+0, r0
	addi r4, r4, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r4, r0, .LBB49_15
	jal r0, .LBB49_7
.LBB49_17:
	stw r12+0, r14
	blt r11, r16, .LBB49_20
.LBB49_18:
	addi r1, r12, 4
.LBB49_19:
	stw r1+4, r16
	ldw r3, r1+0
	mul r16, r3, r16
	addi r11, r11, -1
	addi r1, r1, 8
	bne r11, r0, .LBB49_19
.LBB49_20:
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
.LBB49_21:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end49:
	.size	f77_dta_dup, .Lfunc_end49-f77_dta_dup
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
	beq r3, r0, .LBB50_11
.LBB50_1:
	addi r1, r0, 1
	blt r4, r1, .LBB50_10
.LBB50_2:
	addi r6, r3, 4
	addi r7, r0, 2
	add r8, r4, r0
	jal r0, .LBB50_4
.LBB50_3:
	mul r1, r9, r1
	addi r8, r8, -1
	addi r6, r6, 8
	beq r8, r0, .LBB50_10
.LBB50_4:
	ldw r9, r6+0
	blt r9, r7, .LBB50_3
.LBB50_5:
	ldw r10, r6+4
	beq r10, r1, .LBB50_3
.LBB50_6:
	add r11, r3, r0
	addi r3, r3, 4
	addi r1, r0, 1
	add r12, r4, r0
.LBB50_7:
	ldw r6, r3+0
	mul r1, r6, r1
	addi r4, r4, -1
	addi r3, r3, 8
	bne r4, r0, .LBB50_7
.LBB50_8:
	add r14, r5, r0
	mul r13, r1, r5
	xori r1, r13, 1
	addi r3, r0, 1
	sgt r3, r13, r3
	sub r3, r0, r3
	and r1, r1, r3
	xori r3, r1, 1
	jal r31, malloc
	beq r1, r0, .LBB50_13
.LBB50_9:
	addi r7, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r14, r0
	add r6, r1, r0
	add r11, r1, r0
	jal r31, f77_as_walk
	add r1, r11, r0
	jal r0, .LBB50_12
.LBB50_10:
	ldw r1, r3+0
	jal r0, .LBB50_12
.LBB50_11:
	add r1, r0, r0
.LBB50_12:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB50_13:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	add r5, r13, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end50:
	.size	f77_as_pack, .Lfunc_end50-f77_as_pack
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
	blt r14, r1, .LBB51_3
.LBB51_1:
	addi r5, r3, 4
	addi r15, r0, 1
	add r6, r14, r0
.LBB51_2:
	stw r4+0, r0
	ldw r7, r5+0
	mul r15, r7, r15
	addi r6, r6, -1
	addi r5, r5, 8
	addi r4, r4, 4
	bne r6, r0, .LBB51_2
.LBB51_3:
	blt r15, r1, .LBB51_18
.LBB51_4:
	ldw r16, r3+0
	addi r17, r3, 4
	addi r18, r3, 8
	addi r19, r0, 1
	add r20, r0, r0
	jal r0, .LBB51_6
.LBB51_5:
	addi r20, r20, 1
	beq r20, r15, .LBB51_18
.LBB51_6:
	addi r1, sp, 24
	blt r14, r19, .LBB51_10
.LBB51_7:
	add r3, r18, r0
	add r5, r14, r0
	add r4, r0, r0
.LBB51_8:
	ldw r6, r1+0
	ldw r7, r3+0
	mul r6, r7, r6
	add r4, r6, r4
	addi r5, r5, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r5, r0, .LBB51_8
.LBB51_9:
	mul r1, r4, r13
	jal r0, .LBB51_11
.LBB51_10:
	add r1, r0, r0
.LBB51_11:
	add r4, r16, r1
	mul r1, r20, r13
	beq r11, r0, .LBB51_13
.LBB51_12:
	add r3, r12, r1
	jal r0, .LBB51_14
.LBB51_13:
	add r1, r12, r1
	add r3, r4, r0
	add r4, r1, r0
.LBB51_14:
	add r5, r13, r0
	jal r31, memcpy
	blt r14, r19, .LBB51_5
.LBB51_15:
	addi r1, sp, 24
	add r3, r17, r0
	add r4, r14, r0
.LBB51_16:
	ldw r5, r1+0
	addi r5, r5, 1
	stw r1+0, r5
	ldw r6, r3+0
	blt r5, r6, .LBB51_5
.LBB51_17:
	stw r1+0, r0
	addi r4, r4, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r4, r0, .LBB51_16
	jal r0, .LBB51_5
.LBB51_18:
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
.Lfunc_end51:
	.size	f77_as_walk, .Lfunc_end51-f77_as_walk
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
	beq r1, r0, .LBB52_3
.LBB52_1:
	add r12, r1, r0
	addi r3, r0, 256
	jal r31, malloc
	stw r12+0, r1
	beq r1, r0, .LBB52_3
.LBB52_2:
	stw r12+4, r0
	stw r12+8, r11
	stw r12+12, r11
	add r1, r12, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB52_3:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.124)
	addi r4, r4, %lo(.L.str.124)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end52:
	.size	f77_vec_new, .Lfunc_end52-f77_vec_new
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
	bne r1, r0, .LBB53_4
.LBB53_1:
	ldw r1, r11+12
	beq r5, r1, .LBB53_4
.LBB53_2:
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
	beq r1, r0, .LBB53_8
.LBB53_3:
	stw r11+0, r1
	stw r11+12, r13
	add r4, r12, r0
.LBB53_4:
	ldw r3, r11+4
	ldw r1, r11+8
	bne r3, r1, .LBB53_7
.LBB53_5:
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
	beq r1, r0, .LBB53_8
.LBB53_6:
	stw r11+0, r1
	ldw r1, r11+8
	slli r1, r1, 1
	stw r11+8, r1
	add r4, r12, r0
.LBB53_7:
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
.LBB53_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.124)
	addi r4, r4, %lo(.L.str.124)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end53:
	.size	f77_vec_pushn, .Lfunc_end53-f77_vec_pushn
                                        # -- End function
	.globl	f77_vec_push                    # -- Begin function f77_vec_push
	.p2align	2
	.type	f77_vec_push,@function
f77_vec_push:                           # @f77_vec_push
# %bb.0:
	addi r5, r0, 16
	jal r0, f77_vec_pushn
.Lfunc_end54:
	.size	f77_vec_push, .Lfunc_end54-f77_vec_push
                                        # -- End function
	.globl	f77_vec_count                   # -- Begin function f77_vec_count
	.p2align	2
	.type	f77_vec_count,@function
f77_vec_count:                          # @f77_vec_count
# %bb.0:
	ldw r1, r3+4
	jalr r0, r31, 0
.Lfunc_end55:
	.size	f77_vec_count, .Lfunc_end55-f77_vec_count
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
.Lfunc_end56:
	.size	f77_vec_take, .Lfunc_end56-f77_vec_take
                                        # -- End function
	.globl	f77_as_unpack                   # -- Begin function f77_as_unpack
	.p2align	2
	.type	f77_as_unpack,@function
f77_as_unpack:                          # @f77_as_unpack
# %bb.0:
	beq r3, r0, .LBB57_3
.LBB57_1:
	ldw r1, r3+0
	beq r6, r1, .LBB57_3
.LBB57_2:
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
.LBB57_3:
	jalr r0, r31, 0
.Lfunc_end57:
	.size	f77_as_unpack, .Lfunc_end57-f77_as_unpack
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpu_time_r
	.type	.LCPI58_0,@object
.LCPI58_0:
	.quad	0x412e848000000000              # double 1.0E+6
	.size	.LCPI58_0, 8
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
	lui r1, %hi(.LCPI58_0)
	addi r1, r1, %lo(.LCPI58_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
	stw r11+0, r1
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end58:
	.size	f77_cpu_time_r, .Lfunc_end58-f77_cpu_time_r
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpu_time_d
	.type	.LCPI59_0,@object
.LCPI59_0:
	.quad	0x412e848000000000              # double 1.0E+6
	.size	.LCPI59_0, 8
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
	lui r1, %hi(.LCPI59_0)
	addi r1, r1, %lo(.LCPI59_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fdiv.d r4, r4, r6
	stw r11+4, r5
	stw r11+0, r4
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end59:
	.size	f77_cpu_time_d, .Lfunc_end59-f77_cpu_time_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_fraction
	.type	.LCPI60_0,@object
.LCPI60_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI60_0, 8
	.text
	.globl	f77_fraction
	.p2align	2
	.type	f77_fraction,@function
f77_fraction:                           # @f77_fraction
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI60_0)
	addi r1, r1, %lo(.LCPI60_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r6, r4
	bne r1, r0, .LBB60_2
.LBB60_1:
	addi sp, sp, -32
	stw sp+0, lr
	addi r5, sp, 28
	add r3, r6, r0
	add r4, r7, r0
	jal r31, frexp
	add r6, r1, r0
	add r7, r2, r0
	ldw lr, sp+0
	addi sp, sp, 32
.LBB60_2:
	add r1, r6, r0
	add r2, r7, r0
	jalr r0, r31, 0
.Lfunc_end60:
	.size	f77_fraction, .Lfunc_end60-f77_fraction
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_set_exponent
	.type	.LCPI61_0,@object
.LCPI61_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI61_0, 8
	.text
	.globl	f77_set_exponent
	.p2align	2
	.type	f77_set_exponent,@function
f77_set_exponent:                       # @f77_set_exponent
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI61_0)
	addi r1, r1, %lo(.LCPI61_0)
	ldw r9, r1+4
	ldw r8, r1+0
	feq.d r1, r6, r8
	bne r1, r0, .LBB61_2
.LBB61_1:
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
	add r6, r1, r0
	add r7, r2, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB61_2:
	add r1, r6, r0
	add r2, r7, r0
	jalr r0, r31, 0
.Lfunc_end61:
	.size	f77_set_exponent, .Lfunc_end61-f77_set_exponent
                                        # -- End function
	.globl	f77_scale                       # -- Begin function f77_scale
	.p2align	2
	.type	f77_scale,@function
f77_scale:                              # @f77_scale
# %bb.0:
	add r9, r4, r0
	add r8, r3, r0
	beq r6, r0, .LBB62_2
.LBB62_1:
	add r3, r8, r0
	add r4, r9, r0
	jal r0, ldexp
.LBB62_2:
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
.Lfunc_end62:
	.size	f77_scale, .Lfunc_end62-f77_scale
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_nearest
	.type	.LCPI63_0,@object
.LCPI63_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI63_0, 8
	.type	.LCPI63_2,@object
.LCPI63_2:
	.word	0xff800000                      # float -Inf
	.word	0x7f800000                      # float +Inf
	.size	.LCPI63_2, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI63_1,@object
.LCPI63_1:
	.quad	0xfff0000000000000              # double -Inf
	.quad	0x7ff0000000000000              # double +Inf
	.size	.LCPI63_1, 16
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
	lui r1, %hi(.LCPI63_0)
	addi r1, r1, %lo(.LCPI63_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r10, r4
	xori r1, r1, 1
	beq r1, r0, .LBB63_4
.LBB63_1:
	add r8, r3, r0
	beq r7, r0, .LBB63_3
.LBB63_2:
	flt.d r1, r4, r10
	sub r1, r0, r1
	andi r1, r1, 8
	lui r3, %hi(.LCPI63_1)
	addi r3, r3, %lo(.LCPI63_1)
	add r1, r3, r1
	ldw r6, r1+4
	ldw r5, r1+0
	add r3, r8, r0
	add r4, r9, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, nextafter
.LBB63_3:
	fcvt.s.d r3, r8
	flt.d r1, r4, r10
	sub r1, r0, r1
	andi r1, r1, 4
	lui r4, %hi(.LCPI63_2)
	addi r4, r4, %lo(.LCPI63_2)
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
.LBB63_4:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.24)
	addi r4, r4, %lo(.L.str.24)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end63:
	.size	f77_nearest, .Lfunc_end63-f77_nearest
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_spacing
	.type	.LCPI64_0,@object
.LCPI64_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI64_0, 8
	.type	.LCPI64_1,@object
.LCPI64_1:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI64_1, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI64_2,@object
.LCPI64_2:
	.quad	0x0010000000000000              # double 2.2250738585072014E-308
	.quad	0x3810000000000000              # double 1.1754943508222875E-38
	.size	.LCPI64_2, 16
	.text
	.globl	f77_spacing
	.p2align	2
	.type	f77_spacing,@function
f77_spacing:                            # @f77_spacing
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI64_0)
	addi r1, r1, %lo(.LCPI64_0)
	ldw r9, r1+4
	ldw r8, r1+0
	feq.d r1, r6, r8
	xori r1, r1, 1
	bne r1, r0, .LBB64_2
.LBB64_1:
	addi r1, r0, 0
	seq r1, r5, r1
	sub r1, r0, r1
	andi r1, r1, 8
	lui r3, %hi(.LCPI64_2)
	addi r3, r3, %lo(.LCPI64_2)
	add r1, r3, r1
	ldw r5, r1+4
	ldw r4, r1+0
	jal r0, .LBB64_6
.LBB64_2:
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
	bge r5, r1, .LBB64_4
.LBB64_3:
	andi r1, r11, 8
	lui r3, %hi(.LCPI64_2)
	addi r3, r3, %lo(.LCPI64_2)
	add r1, r3, r1
	ldw r5, r1+4
	ldw r4, r1+0
	jal r0, .LBB64_5
.LBB64_4:
	lui r1, %hi(.LCPI64_1)
	addi r1, r1, %lo(.LCPI64_1)
	ldw r4, r1+4
	ldw r3, r1+0
	jal r31, ldexp
	add r4, r1, r0
	add r5, r2, r0
.LBB64_5:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB64_6:
	add r1, r4, r0
	add r2, r5, r0
	jalr r0, r31, 0
.Lfunc_end64:
	.size	f77_spacing, .Lfunc_end64-f77_spacing
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_rrspacing
	.type	.LCPI65_0,@object
.LCPI65_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI65_0, 8
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
	lui r1, %hi(.LCPI65_0)
	addi r1, r1, %lo(.LCPI65_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r6, r4
	bne r1, r0, .LBB65_2
.LBB65_1:
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
.LBB65_2:
	add r1, r4, r0
	add r2, r5, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end65:
	.size	f77_rrspacing, .Lfunc_end65-f77_rrspacing
                                        # -- End function
	.globl	f77_modulo_i                    # -- Begin function f77_modulo_i
	.p2align	2
	.type	f77_modulo_i,@function
f77_modulo_i:                           # @f77_modulo_i
# %bb.0:
	beq r4, r0, .LBB66_2
.LBB66_1:
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
.LBB66_2:
	addi sp, sp, -16
	stw sp+0, lr
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.25)
	addi r4, r4, %lo(.L.str.25)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end66:
	.size	f77_modulo_i, .Lfunc_end66-f77_modulo_i
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_modulo_d
	.type	.LCPI67_0,@object
.LCPI67_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI67_0, 8
	.type	.LCPI67_2,@object
.LCPI67_2:
	.quad	0xb690000000000000              # double -7.0064923216240854E-46
	.size	.LCPI67_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI67_1,@object
.LCPI67_1:
	.word	0x00000000                      # float 0
	.size	.LCPI67_1, 4
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
	beq r7, r0, .LBB67_3
.LBB67_1:
	add r3, r4, r0
	add r4, r5, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fmod
	add r4, r1, r0
	add r5, r2, r0
	lui r1, %hi(.LCPI67_0)
	addi r1, r1, %lo(.LCPI67_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r4, r6
	xori r1, r1, 1
	bne r1, r0, .LBB67_6
.LBB67_2:
	add r3, r6, r0
	add r4, r7, r0
	jal r0, .LBB67_5
.LBB67_3:
	fcvt.s.d r3, r4
	fcvt.s.d r11, r12
	add r4, r11, r0
	jal r31, fmodf
	lui r3, %hi(.LCPI67_1)
	ldw r3, r3+%lo(.LCPI67_1)
	feq.s r4, r1, r3
	xori r4, r4, 1
	bne r4, r0, .LBB67_7
.LBB67_4:
	lui r1, %hi(.LCPI67_0)
	addi r1, r1, %lo(.LCPI67_0)
	ldw r4, r1+4
	ldw r3, r1+0
.LBB67_5:
	add r5, r12, r0
	add r6, r13, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, copysign
.LBB67_6:
	flt.d r1, r4, r6
	flt.d r3, r12, r6
	xori r3, r3, 1
	xor r1, r3, r1
	fadd.d r8, r12, r4
	sub r1, r0, r1
	xor r3, r2, r9
	and r3, r3, r1
	xor r7, r9, r3
	xor r3, r4, r8
	and r1, r3, r1
	xor r6, r8, r1
	jal r0, .LBB67_8
.LBB67_7:
	flt.s r3, r1, r3
	lui r4, %hi(.LCPI67_2)
	addi r4, r4, %lo(.LCPI67_2)
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
	fcvt.d.s r6, r1
.LBB67_8:
	add r1, r6, r0
	add r2, r7, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end67:
	.size	f77_modulo_d, .Lfunc_end67-f77_modulo_d
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
	beq r18, r0, .LBB68_6
.LBB68_1:
	ldw r1, r14+20
	addi r6, r1, 1900
	ldw r1, r14+16
	addi r7, r1, 1
	ldw r8, r14+12
	lui r5, %hi(.L.str.26)
	addi r5, r5, %lo(.L.str.26)
	addi r3, sp, 28
	addi r4, r0, 16
	jal r31, snprintf
	addi r1, r0, 1
	blt r17, r1, .LBB68_6
.LBB68_2:
	addi r1, r0, 32
	addi r3, r0, 7
	add r4, r0, r0
	jal r0, .LBB68_4
.LBB68_3:
	add r6, r18, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r17, r4, .LBB68_6
.LBB68_4:
	add r5, r1, r0
	bgtu r4, r3, .LBB68_3
.LBB68_5:
	addi r5, sp, 28
	add r5, r5, r4
	ldbu r5, r5+0
	jal r0, .LBB68_3
.LBB68_6:
	lui r17, 67109
	beq r16, r0, .LBB68_12
.LBB68_7:
	ldw r6, r14+8
	ldw r7, r14+4
	ldw r8, r14+0
	ldw r1, sp+60
	addi r3, r17, -557
	mulh r1, r1, r3
	srli r3, r1, 31
	srai r1, r1, 6
	add r9, r1, r3
	lui r5, %hi(.L.str.27)
	addi r5, r5, %lo(.L.str.27)
	addi r3, sp, 28
	addi r4, r0, 16
	jal r31, snprintf
	addi r1, r0, 1
	blt r15, r1, .LBB68_12
.LBB68_8:
	addi r1, r0, 32
	addi r3, r0, 9
	add r4, r0, r0
	jal r0, .LBB68_10
.LBB68_9:
	add r6, r16, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r15, r4, .LBB68_12
.LBB68_10:
	add r5, r1, r0
	bgtu r4, r3, .LBB68_9
.LBB68_11:
	addi r5, sp, 28
	add r5, r5, r4
	ldbu r5, r5+0
	jal r0, .LBB68_9
.LBB68_12:
	beq r13, r0, .LBB68_18
.LBB68_13:
	addi r1, r0, 1
	blt r12, r1, .LBB68_18
.LBB68_14:
	addi r1, r0, 32
	addi r3, r0, 4
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	add r5, r0, r0
	jal r0, .LBB68_16
.LBB68_15:
	add r7, r13, r5
	stb r7+0, r6
	addi r5, r5, 1
	beq r12, r5, .LBB68_18
.LBB68_16:
	add r6, r1, r0
	bgtu r5, r3, .LBB68_15
.LBB68_17:
	add r6, r5, r4
	ldbu r6, r6+0
	jal r0, .LBB68_15
.LBB68_18:
	beq r11, r0, .LBB68_20
.LBB68_19:
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
.LBB68_20:
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
	.size	f77_date_and_time, .Lfunc_end68-f77_date_and_time
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
	beq r13, r0, .LBB69_2
.LBB69_1:
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
.LBB69_2:
	beq r12, r0, .LBB69_4
.LBB69_3:
	addi r1, r0, 1000
	stw r12+0, r1
.LBB69_4:
	beq r11, r0, .LBB69_6
.LBB69_5:
	addi r1, r14, -1
	stw r11+0, r1
.LBB69_6:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end69:
	.size	f77_system_clock, .Lfunc_end69-f77_system_clock
                                        # -- End function
	.globl	f77_random_seed                 # -- Begin function f77_random_seed
	.p2align	2
	.type	f77_random_seed,@function
f77_random_seed:                        # @f77_random_seed
# %bb.0:
	beq r3, r0, .LBB70_2
.LBB70_1:
	addi r1, r0, 4
	stw r3+0, r1
.LBB70_2:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	lui r11, %hi(f77_rng)
	addi r11, r11, %lo(f77_rng)
	addi r1, r0, 16
	beq r5, r0, .LBB70_5
.LBB70_3:
	add r6, r0, r0
.LBB70_4:
	add r7, r6, r11
	ldw r7, r7+0
	add r8, r5, r6
	stw r8+0, r7
	addi r6, r6, 4
	bne r6, r1, .LBB70_4
.LBB70_5:
	lui r12, %hi(f77_rng+4)
	lui r13, %hi(f77_rng+8)
	lui r14, %hi(f77_rng+12)
	beq r4, r0, .LBB70_10
.LBB70_6:
	add r6, r0, r0
.LBB70_7:
	add r7, r4, r6
	ldw r7, r7+0
	add r8, r6, r11
	stw r8+0, r7
	addi r6, r6, 4
	bne r6, r1, .LBB70_7
.LBB70_8:
	ldw r1, r11+0
	ldw r6, r12+%lo(f77_rng+4)
	or  r1, r6, r1
	ldw r6, r13+%lo(f77_rng+8)
	or  r1, r1, r6
	ldw r6, r14+%lo(f77_rng+12)
	or  r1, r1, r6
	bne r1, r0, .LBB70_10
.LBB70_9:
	addi r1, r0, 1
	stw r11+0, r1
.LBB70_10:
	bne r3, r0, .LBB70_15
.LBB70_11:
	bne r4, r0, .LBB70_15
.LBB70_12:
	bne r5, r0, .LBB70_15
.LBB70_13:
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
	bne r1, r0, .LBB70_15
.LBB70_14:
	addi r1, r0, 1
	stw r11+0, r1
.LBB70_15:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end70:
	.size	f77_random_seed, .Lfunc_end70-f77_random_seed
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
	beq r4, r0, .LBB71_2
.LBB71_1:
	ldw r1, r4+4
	addi r5, r0, 4
	blt r1, r5, .LBB71_12
.LBB71_2:
	beq r11, r0, .LBB71_4
.LBB71_3:
	ldw r1, r11+4
	addi r5, r0, 3
	ble r1, r5, .LBB71_12
.LBB71_4:
	beq r4, r0, .LBB71_7
.LBB71_5:
	ldw r1, r4+0
	ldw r4, r4+8
	slli r5, r4, 2
	addi r6, r0, 16
	add r7, r0, r0
.LBB71_6:
	ldw r8, r1+0
	addi r4, sp, 44
	add r9, r4, r7
	stw r9+0, r8
	addi r7, r7, 4
	add r1, r1, r5
	bne r7, r6, .LBB71_6
	jal r0, .LBB71_8
.LBB71_7:
	add r4, r0, r0
.LBB71_8:
	addi r1, r0, 0
	seq r1, r11, r1
	sub r1, r0, r1
	addi r5, sp, 28
	and r1, r5, r1
	xor r5, r5, r1
	jal r31, f77_random_seed
	beq r11, r0, .LBB71_11
.LBB71_9:
	addi r1, sp, 28
	addi r3, r0, 4
	add r4, r0, r0
.LBB71_10:
	ldw r5, r1+0
	ldw r6, r11+0
	ldw r7, r11+8
	mul r7, r7, r4
	slli r7, r7, 2
	add r6, r6, r7
	stw r6+0, r5
	addi r4, r4, 1
	addi r1, r1, 4
	bne r4, r3, .LBB71_10
.LBB71_11:
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.LBB71_12:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end71:
	.size	f77_random_seed_d, .Lfunc_end71-f77_random_seed_d
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
	beq r9, r0, .LBB72_5
.LBB72_1:
	add r11, r9, r0
	ldw r1, r9+4
	addi r9, r0, 7
	ble r1, r9, .LBB72_6
.LBB72_2:
	addi r12, sp, 24
	add r9, r12, r0
	jal r31, f77_date_and_time
	addi r1, r0, 8
	add r3, r0, r0
.LBB72_3:
	ldw r4, r12+0
	ldw r5, r11+0
	ldw r6, r11+8
	mul r6, r6, r3
	slli r6, r6, 2
	add r5, r5, r6
	stw r5+0, r4
	addi r3, r3, 1
	addi r12, r12, 4
	bne r3, r1, .LBB72_3
.LBB72_4:
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.LBB72_5:
	add r9, r0, r0
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jal r0, f77_date_and_time
.LBB72_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end72:
	.size	f77_date_and_time_d, .Lfunc_end72-f77_date_and_time_d
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_random_r
	.type	.LCPI73_0,@object
.LCPI73_0:
	.word	0x33800000                      # float 5.96046448E-8
	.size	.LCPI73_0, 4
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
	lui r4, %hi(.LCPI73_0)
	ldw r4, r4+%lo(.LCPI73_0)
	fmul.s r1, r1, r4
	stw r3+0, r1
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end73:
	.size	f77_random_r, .Lfunc_end73-f77_random_r
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_random_d
	.type	.LCPI74_0,@object
.LCPI74_0:
	.quad	0x4190000000000000              # double 67108864
	.size	.LCPI74_0, 8
	.type	.LCPI74_1,@object
.LCPI74_1:
	.quad	0x3ca0000000000000              # double 1.1102230246251565E-16
	.size	.LCPI74_1, 8
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
	lui r6, %hi(.LCPI74_0)
	addi r6, r6, %lo(.LCPI74_0)
	ldw r9, r6+4
	ldw r8, r6+0
	fmul.d r4, r4, r8
	fcvt.d.wu r6, r1
	fadd.d r4, r4, r6
	lui r1, %hi(.LCPI74_1)
	addi r1, r1, %lo(.LCPI74_1)
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
.Lfunc_end74:
	.size	f77_random_d, .Lfunc_end74-f77_random_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_random_desc
	.type	.LCPI75_0,@object
.LCPI75_0:
	.quad	0x4190000000000000              # double 67108864
	.size	.LCPI75_0, 8
	.type	.LCPI75_1,@object
.LCPI75_1:
	.quad	0x3ca0000000000000              # double 1.1102230246251565E-16
	.size	.LCPI75_1, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI75_2,@object
.LCPI75_2:
	.word	0x33800000                      # float 5.96046448E-8
	.size	.LCPI75_2, 4
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
	blt r12, r3, .LBB75_3
.LBB75_1:
	addi r4, r13, 4
	addi r1, r0, 1
	addi r5, r0, 0
	add r6, r12, r0
.LBB75_2:
	ldw r7, r4+0
	sgt r8, r7, r5
	mul r1, r7, r1
	sub r7, r0, r8
	and r1, r1, r7
	addi r6, r6, -1
	addi r4, r4, 8
	bne r6, r0, .LBB75_2
.LBB75_3:
	blt r1, r3, .LBB75_17
.LBB75_4:
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
	jal r0, .LBB75_6
.LBB75_5:
	addi r16, r16, 1
	beq r16, r1, .LBB75_16
.LBB75_6:
	blt r12, r14, .LBB75_9
.LBB75_7:
	addi r18, sp, 16
	add r19, r10, r0
	add r20, r12, r0
	add r17, r0, r0
.LBB75_8:
	ldw r21, r18+0
	ldw r22, r19+0
	mul r21, r22, r21
	add r17, r21, r17
	addi r20, r20, -1
	addi r19, r19, 8
	addi r18, r18, 4
	bne r20, r0, .LBB75_8
	jal r0, .LBB75_10
.LBB75_9:
	add r17, r0, r0
.LBB75_10:
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
	beq r11, r0, .LBB75_12
.LBB75_11:
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
	lui r18, %hi(.LCPI75_0)
	addi r18, r18, %lo(.LCPI75_0)
	ldw r23, r18+4
	ldw r22, r18+0
	fmul.d r20, r20, r22
	fcvt.d.wu r18, r19
	fadd.d r18, r20, r18
	lui r20, %hi(.LCPI75_1)
	addi r20, r20, %lo(.LCPI75_1)
	ldw r23, r20+4
	ldw r22, r20+0
	fmul.d r18, r18, r22
	ldw r20, r13+0
	slli r17, r17, 3
	add r17, r20, r17
	stw r17+4, r19
	stw r17+0, r18
	bge r12, r14, .LBB75_13
	jal r0, .LBB75_5
.LBB75_12:
	srli r18, r18, 8
	fcvt.s.wu r18, r18
	lui r19, %hi(.LCPI75_2)
	ldw r19, r19+%lo(.LCPI75_2)
	fmul.s r18, r18, r19
	ldw r19, r13+0
	slli r17, r17, 2
	add r17, r19, r17
	stw r17+0, r18
	blt r12, r14, .LBB75_5
.LBB75_13:
	addi r17, sp, 16
	add r18, r4, r0
	add r19, r12, r0
.LBB75_14:
	ldw r20, r17+0
	addi r20, r20, 1
	stw r17+0, r20
	ldw r21, r18+0
	blt r20, r21, .LBB75_5
.LBB75_15:
	stw r17+0, r0
	addi r19, r19, -1
	addi r18, r18, 8
	addi r17, r17, 4
	bne r19, r0, .LBB75_14
	jal r0, .LBB75_5
.LBB75_16:
	stw r3+%lo(f77_rng+4), r6
	stw r5+%lo(f77_rng), r8
	stw r7+%lo(f77_rng+8), lr
	stw r9+%lo(f77_rng+12), r15
.LBB75_17:
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
.Lfunc_end75:
	.size	f77_random_desc, .Lfunc_end75-f77_random_desc
                                        # -- End function
	.globl	f77_len_trim                    # -- Begin function f77_len_trim
	.p2align	2
	.type	f77_len_trim,@function
f77_len_trim:                           # @f77_len_trim
# %bb.0:
	srai r1, r4, 31
	and r1, r4, r1
	addi r5, r0, 1
	blt r4, r5, .LBB76_5
.LBB76_1:
	addi r3, r3, -1
	addi r5, r0, 32
	addi r6, r0, 2
.LBB76_2:
	add r7, r4, r0
	add r4, r3, r4
	ldbu r4, r4+0
	bne r4, r5, .LBB76_4
.LBB76_3:
	addi r4, r7, -1
	bge r7, r6, .LBB76_2
	jal r0, .LBB76_5
.LBB76_4:
	add r1, r7, r0
.LBB76_5:
	jalr r0, r31, 0
.Lfunc_end76:
	.size	f77_len_trim, .Lfunc_end76-f77_len_trim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_exponent
	.type	.LCPI77_0,@object
.LCPI77_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI77_0, 8
	.text
	.globl	f77_exponent
	.p2align	2
	.type	f77_exponent,@function
f77_exponent:                           # @f77_exponent
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI77_0)
	addi r1, r1, %lo(.LCPI77_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r6, r4
	add r1, r0, r0
	bne r3, r0, .LBB77_2
.LBB77_1:
	addi sp, sp, -32
	stw sp+0, lr
	addi r5, sp, 28
	add r3, r6, r0
	add r4, r7, r0
	jal r31, frexp
	ldw r1, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB77_2:
	jalr r0, r31, 0
.Lfunc_end77:
	.size	f77_exponent, .Lfunc_end77-f77_exponent
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zsqrt
	.type	.LCPI78_0,@object
.LCPI78_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI78_0, 8
	.type	.LCPI78_1,@object
.LCPI78_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI78_1, 8
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
	lui r1, %hi(.LCPI78_0)
	addi r1, r1, %lo(.LCPI78_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r14, r1, r0
	add r15, r2, r0
	lui r1, %hi(.LCPI78_1)
	addi r1, r1, %lo(.LCPI78_1)
	ldw r17, r1+4
	ldw r16, r1+0
	fle.d r1, r16, r18
	xori r1, r1, 1
	beq r1, r0, .LBB78_4
.LBB78_1:
	feq.d r1, r14, r16
	add r4, r16, r0
	add r5, r17, r0
	bne r1, r0, .LBB78_3
.LBB78_2:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r14, r14
	fdiv.d r4, r4, r6
.LBB78_3:
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
	jal r0, .LBB78_6
.LBB78_4:
	stw r11+4, r15
	stw r11+0, r14
	feq.d r1, r14, r16
	bne r1, r0, .LBB78_6
.LBB78_5:
	fadd.d r4, r14, r14
	fdiv.d r16, r12, r4
.LBB78_6:
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
.Lfunc_end78:
	.size	f77_zsqrt, .Lfunc_end78-f77_zsqrt
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
.Lfunc_end79:
	.size	f77_zexp, .Lfunc_end79-f77_zexp
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
.Lfunc_end80:
	.size	f77_zlog, .Lfunc_end80-f77_zlog
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
.Lfunc_end81:
	.size	f77_zsin, .Lfunc_end81-f77_zsin
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
.Lfunc_end82:
	.size	f77_zcos, .Lfunc_end82-f77_zcos
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
.Lfunc_end83:
	.size	f77i_iabs, .Lfunc_end83-f77i_iabs
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_abs
	.type	.LCPI84_0,@object
.LCPI84_0:
	.word	0x00000000                      # float 0
	.size	.LCPI84_0, 4
	.text
	.globl	f77i_abs
	.p2align	2
	.type	f77i_abs,@function
f77i_abs:                               # @f77i_abs
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI84_0)
	ldw r3, r3+%lo(.LCPI84_0)
	flt.s r3, r1, r3
	sub r3, r0, r3
	lui r4, 524288
	and r3, r3, r4
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end84:
	.size	f77i_abs, .Lfunc_end84-f77i_abs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dabs
	.type	.LCPI85_0,@object
.LCPI85_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI85_0, 8
	.text
	.globl	f77i_dabs
	.p2align	2
	.type	f77i_dabs,@function
f77i_dabs:                              # @f77i_dabs
# %bb.0:
	ldw r5, r3+4
	ldw r4, r3+0
	lui r1, %hi(.LCPI85_0)
	addi r1, r1, %lo(.LCPI85_0)
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
.Lfunc_end85:
	.size	f77i_dabs, .Lfunc_end85-f77i_dabs
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
.Lfunc_end86:
	.size	f77i_sqrt, .Lfunc_end86-f77i_sqrt
                                        # -- End function
	.globl	f77i_dsqrt                      # -- Begin function f77i_dsqrt
	.p2align	2
	.type	f77i_dsqrt,@function
f77i_dsqrt:                             # @f77i_dsqrt
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sqrt
.Lfunc_end87:
	.size	f77i_dsqrt, .Lfunc_end87-f77i_dsqrt
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
.Lfunc_end88:
	.size	f77i_exp, .Lfunc_end88-f77i_exp
                                        # -- End function
	.globl	f77i_dexp                       # -- Begin function f77i_dexp
	.p2align	2
	.type	f77i_dexp,@function
f77i_dexp:                              # @f77i_dexp
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, exp
.Lfunc_end89:
	.size	f77i_dexp, .Lfunc_end89-f77i_dexp
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
.Lfunc_end90:
	.size	f77i_alog, .Lfunc_end90-f77i_alog
                                        # -- End function
	.globl	f77i_dlog                       # -- Begin function f77i_dlog
	.p2align	2
	.type	f77i_dlog,@function
f77i_dlog:                              # @f77i_dlog
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log
.Lfunc_end91:
	.size	f77i_dlog, .Lfunc_end91-f77i_dlog
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
.Lfunc_end92:
	.size	f77i_alog10, .Lfunc_end92-f77i_alog10
                                        # -- End function
	.globl	f77i_dlog10                     # -- Begin function f77i_dlog10
	.p2align	2
	.type	f77i_dlog10,@function
f77i_dlog10:                            # @f77i_dlog10
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log10
.Lfunc_end93:
	.size	f77i_dlog10, .Lfunc_end93-f77i_dlog10
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
.Lfunc_end94:
	.size	f77i_sin, .Lfunc_end94-f77i_sin
                                        # -- End function
	.globl	f77i_dsin                       # -- Begin function f77i_dsin
	.p2align	2
	.type	f77i_dsin,@function
f77i_dsin:                              # @f77i_dsin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sin
.Lfunc_end95:
	.size	f77i_dsin, .Lfunc_end95-f77i_dsin
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
.Lfunc_end96:
	.size	f77i_cos, .Lfunc_end96-f77i_cos
                                        # -- End function
	.globl	f77i_dcos                       # -- Begin function f77i_dcos
	.p2align	2
	.type	f77i_dcos,@function
f77i_dcos:                              # @f77i_dcos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cos
.Lfunc_end97:
	.size	f77i_dcos, .Lfunc_end97-f77i_dcos
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
.Lfunc_end98:
	.size	f77i_tan, .Lfunc_end98-f77i_tan
                                        # -- End function
	.globl	f77i_dtan                       # -- Begin function f77i_dtan
	.p2align	2
	.type	f77i_dtan,@function
f77i_dtan:                              # @f77i_dtan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tan
.Lfunc_end99:
	.size	f77i_dtan, .Lfunc_end99-f77i_dtan
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
.Lfunc_end100:
	.size	f77i_asin, .Lfunc_end100-f77i_asin
                                        # -- End function
	.globl	f77i_dasin                      # -- Begin function f77i_dasin
	.p2align	2
	.type	f77i_dasin,@function
f77i_dasin:                             # @f77i_dasin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, asin
.Lfunc_end101:
	.size	f77i_dasin, .Lfunc_end101-f77i_dasin
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
.Lfunc_end102:
	.size	f77i_acos, .Lfunc_end102-f77i_acos
                                        # -- End function
	.globl	f77i_dacos                      # -- Begin function f77i_dacos
	.p2align	2
	.type	f77i_dacos,@function
f77i_dacos:                             # @f77i_dacos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, acos
.Lfunc_end103:
	.size	f77i_dacos, .Lfunc_end103-f77i_dacos
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
.Lfunc_end104:
	.size	f77i_atan, .Lfunc_end104-f77i_atan
                                        # -- End function
	.globl	f77i_datan                      # -- Begin function f77i_datan
	.p2align	2
	.type	f77i_datan,@function
f77i_datan:                             # @f77i_datan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, atan
.Lfunc_end105:
	.size	f77i_datan, .Lfunc_end105-f77i_datan
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
.Lfunc_end106:
	.size	f77i_atan2, .Lfunc_end106-f77i_atan2
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
.Lfunc_end107:
	.size	f77i_datan2, .Lfunc_end107-f77i_datan2
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
.Lfunc_end108:
	.size	f77i_sinh, .Lfunc_end108-f77i_sinh
                                        # -- End function
	.globl	f77i_dsinh                      # -- Begin function f77i_dsinh
	.p2align	2
	.type	f77i_dsinh,@function
f77i_dsinh:                             # @f77i_dsinh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sinh
.Lfunc_end109:
	.size	f77i_dsinh, .Lfunc_end109-f77i_dsinh
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
.Lfunc_end110:
	.size	f77i_cosh, .Lfunc_end110-f77i_cosh
                                        # -- End function
	.globl	f77i_dcosh                      # -- Begin function f77i_dcosh
	.p2align	2
	.type	f77i_dcosh,@function
f77i_dcosh:                             # @f77i_dcosh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cosh
.Lfunc_end111:
	.size	f77i_dcosh, .Lfunc_end111-f77i_dcosh
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
.Lfunc_end112:
	.size	f77i_tanh, .Lfunc_end112-f77i_tanh
                                        # -- End function
	.globl	f77i_dtanh                      # -- Begin function f77i_dtanh
	.p2align	2
	.type	f77i_dtanh,@function
f77i_dtanh:                             # @f77i_dtanh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tanh
.Lfunc_end113:
	.size	f77i_dtanh, .Lfunc_end113-f77i_dtanh
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
.Lfunc_end114:
	.size	f77i_aint, .Lfunc_end114-f77i_aint
                                        # -- End function
	.globl	f77i_dint                       # -- Begin function f77i_dint
	.p2align	2
	.type	f77i_dint,@function
f77i_dint:                              # @f77i_dint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, trunc
.Lfunc_end115:
	.size	f77i_dint, .Lfunc_end115-f77i_dint
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
.Lfunc_end116:
	.size	f77i_anint, .Lfunc_end116-f77i_anint
                                        # -- End function
	.globl	f77i_dnint                      # -- Begin function f77i_dnint
	.p2align	2
	.type	f77i_dnint,@function
f77i_dnint:                             # @f77i_dnint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, round
.Lfunc_end117:
	.size	f77i_dnint, .Lfunc_end117-f77i_dnint
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
.Lfunc_end118:
	.size	f77i_nint, .Lfunc_end118-f77i_nint
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
.Lfunc_end119:
	.size	f77i_idnint, .Lfunc_end119-f77i_idnint
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
.Lfunc_end120:
	.size	f77i_mod, .Lfunc_end120-f77i_mod
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
.Lfunc_end121:
	.size	f77i_amod, .Lfunc_end121-f77i_amod
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
.Lfunc_end122:
	.size	f77i_dmod, .Lfunc_end122-f77i_dmod
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
.Lfunc_end123:
	.size	f77i_isign, .Lfunc_end123-f77i_isign
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_sign
	.type	.LCPI124_0,@object
.LCPI124_0:
	.word	0x00000000                      # float 0
	.size	.LCPI124_0, 4
	.text
	.globl	f77i_sign
	.p2align	2
	.type	f77i_sign,@function
f77i_sign:                              # @f77i_sign
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI124_0)
	ldw r3, r3+%lo(.LCPI124_0)
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
.Lfunc_end124:
	.size	f77i_sign, .Lfunc_end124-f77i_sign
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dsign
	.type	.LCPI125_0,@object
.LCPI125_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI125_0, 8
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
	lui r1, %hi(.LCPI125_0)
	addi r1, r1, %lo(.LCPI125_0)
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
.Lfunc_end125:
	.size	f77i_dsign, .Lfunc_end125-f77i_dsign
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
.Lfunc_end126:
	.size	f77i_idim, .Lfunc_end126-f77i_idim
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
.Lfunc_end127:
	.size	f77i_dim, .Lfunc_end127-f77i_dim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_ddim
	.type	.LCPI128_0,@object
.LCPI128_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI128_0, 8
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
	lui r3, %hi(.LCPI128_0)
	addi r3, r3, %lo(.LCPI128_0)
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
.Lfunc_end128:
	.size	f77i_ddim, .Lfunc_end128-f77i_ddim
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
.Lfunc_end129:
	.size	f77i_dprod, .Lfunc_end129-f77i_dprod
                                        # -- End function
	.globl	f77i_len                        # -- Begin function f77i_len
	.p2align	2
	.type	f77i_len,@function
f77i_len:                               # @f77i_len
# %bb.0:
	add r1, r4, r0
	jalr r0, r31, 0
.Lfunc_end130:
	.size	f77i_len, .Lfunc_end130-f77i_len
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
	beq r6, r0, .LBB131_6
.LBB131_1:
	add r12, r6, r0
	bgt r6, r11, .LBB131_5
.LBB131_2:
	add r13, r4, r0
	add r14, r3, r0
	add r15, r0, r0
.LBB131_3:
	add r3, r14, r15
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcmp
	beq r1, r0, .LBB131_7
.LBB131_4:
	addi r15, r15, 1
	add r1, r12, r15
	ble r1, r11, .LBB131_3
.LBB131_5:
	add r1, r0, r0
	jal r0, .LBB131_8
.LBB131_6:
	addi r1, r0, 0
	sgt r1, r11, r1
	jal r0, .LBB131_8
.LBB131_7:
	addi r1, r15, 1
.LBB131_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end131:
	.size	f77i_index, .Lfunc_end131-f77i_index
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
	beq r6, r0, .LBB132_6
.LBB132_1:
	add r12, r6, r0
	bgt r6, r11, .LBB132_5
.LBB132_2:
	add r13, r5, r0
	add r14, r3, r0
	add r15, r0, r0
.LBB132_3:
	add r3, r14, r15
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcmp
	beq r1, r0, .LBB132_7
.LBB132_4:
	addi r15, r15, 1
	add r1, r12, r15
	ble r1, r11, .LBB132_3
.LBB132_5:
	add r1, r0, r0
	jal r0, .LBB132_8
.LBB132_6:
	addi r1, r0, 0
	sgt r1, r11, r1
	jal r0, .LBB132_8
.LBB132_7:
	addi r1, r15, 1
.LBB132_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end132:
	.size	f77_index, .Lfunc_end132-f77_index
                                        # -- End function
	.globl	f77i_aimag                      # -- Begin function f77i_aimag
	.p2align	2
	.type	f77i_aimag,@function
f77i_aimag:                             # @f77i_aimag
# %bb.0:
	ldw r1, r3+4
	jalr r0, r31, 0
.Lfunc_end133:
	.size	f77i_aimag, .Lfunc_end133-f77i_aimag
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
.Lfunc_end134:
	.size	f77i_cabs, .Lfunc_end134-f77i_cabs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_csqrt
	.type	.LCPI135_0,@object
.LCPI135_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI135_0, 8
	.type	.LCPI135_2,@object
.LCPI135_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI135_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI135_1,@object
.LCPI135_1:
	.word	0x00000000                      # float 0
	.size	.LCPI135_1, 4
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
	lui r1, %hi(.LCPI135_0)
	addi r1, r1, %lo(.LCPI135_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r14, r1, r0
	add r15, r2, r0
	lui r1, %hi(.LCPI135_1)
	ldw r3, r1+%lo(.LCPI135_1)
	fle.s r4, r3, r16
	xori r4, r4, 1
	beq r4, r0, .LBB135_4
.LBB135_1:
	ldw r16, r1+%lo(.LCPI135_1)
	lui r1, %hi(.LCPI135_2)
	addi r1, r1, %lo(.LCPI135_2)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r14, r4
	add r1, r16, r0
	bne r3, r0, .LBB135_3
.LBB135_2:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r14, r14
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
.LBB135_3:
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
	jal r0, .LBB135_6
.LBB135_4:
	fcvt.s.d r1, r14
	lui r4, %hi(.LCPI135_2)
	addi r4, r4, %lo(.LCPI135_2)
	ldw r7, r4+4
	ldw r6, r4+0
	feq.d r4, r14, r6
	bne r4, r0, .LBB135_6
.LBB135_5:
	fadd.d r4, r14, r14
	fdiv.d r4, r12, r4
	fcvt.s.d r3, r4
.LBB135_6:
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
.Lfunc_end135:
	.size	f77i_csqrt, .Lfunc_end135-f77i_csqrt
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
.Lfunc_end136:
	.size	f77i_cexp, .Lfunc_end136-f77i_cexp
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
.Lfunc_end137:
	.size	f77i_clog, .Lfunc_end137-f77i_clog
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
.Lfunc_end138:
	.size	f77i_csin, .Lfunc_end138-f77i_csin
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
.Lfunc_end139:
	.size	f77i_ccos, .Lfunc_end139-f77i_ccos
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
.Lfunc_end140:
	.size	f77i_conjg, .Lfunc_end140-f77i_conjg
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
	blt r14, r1, .LBB141_2
.LBB141_1:
	add r3, r12, r0
	add r4, r5, r0
	add r5, r14, r0
	jal r31, memmove
.LBB141_2:
	bge r13, r11, .LBB141_5
.LBB141_3:
	add r1, r12, r14
	sub r3, r11, r14
	addi r4, r0, 32
.LBB141_4:
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB141_4
.LBB141_5:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end141:
	.size	f77_strcpy, .Lfunc_end141-f77_strcpy
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
	blt r7, r1, .LBB142_8
.LBB142_1:
	addi r8, r0, 32
	add r9, r0, r0
                                        # implicit-def: $r1
.LBB142_2:
	add r10, r8, r0
	bge r9, r4, .LBB142_4
.LBB142_3:
	add r10, r3, r9
	ldbu r10, r10+0
.LBB142_4:
	add lr, r8, r0
	bge r9, r6, .LBB142_6
.LBB142_5:
	add lr, r5, r9
	ldbu lr, lr+0
.LBB142_6:
	seq r11, r10, lr
	sltu r12, r10, lr
	sub r12, r0, r12
	ori  r12, r12, 1
	xor r1, r1, r12
	sub r11, r0, r11
	and r1, r1, r11
	xor r1, r12, r1
	bne r10, lr, .LBB142_9
.LBB142_7:
	addi r9, r9, 1
	bne r7, r9, .LBB142_2
.LBB142_8:
	add r1, r0, r0
.LBB142_9:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end142:
	.size	f77_strcmp, .Lfunc_end142-f77_strcmp
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
	bgeu r5, r6, .LBB143_8
.LBB143_1:
	lui r3, %hi(fio_catpos)
	ldw r7, r3+%lo(fio_catpos)
	add r7, r7, r5
	blt r7, r6, .LBB143_3
.LBB143_2:
	stw r3+%lo(fio_catpos), r0
.LBB143_3:
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
	blt r1, r15, .LBB143_5
.LBB143_4:
	add r3, r13, r0
	add r5, r1, r0
	jal r31, memmove
.LBB143_5:
	blt r11, r15, .LBB143_7
.LBB143_6:
	add r3, r13, r14
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memmove
.LBB143_7:
	add r1, r13, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB143_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end143:
	.size	f77_cat, .Lfunc_end143-f77_cat
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
	ble r5, r1, .LBB144_8
.LBB144_1:
	add r12, r3, r0
	addi r1, r0, 0
	sgt r1, r4, r1
	sub r1, r0, r1
	and r13, r4, r1
	mul r5, r13, r11
	lui r1, 16
	addi r3, r1, 1
	bge r5, r3, .LBB144_9
.LBB144_2:
	lui r1, %hi(fio_catpos)
	ldw r4, r1+%lo(fio_catpos)
	add r4, r4, r5
	blt r4, r3, .LBB144_4
.LBB144_3:
	stw r1+%lo(fio_catpos), r0
.LBB144_4:
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
	beq r11, r0, .LBB144_7
.LBB144_5:
	add r15, r14, r0
.LBB144_6:
	add r3, r15, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, memmove
	addi r11, r11, -1
	add r15, r15, r13
	bne r11, r0, .LBB144_6
.LBB144_7:
	add r1, r14, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB144_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
	add r5, r11, r0
	jal r0, .LBB144_10
.LBB144_9:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.125)
	addi r4, r4, %lo(.L.str.125)
.LBB144_10:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end144:
	.size	f77_repeat, .Lfunc_end144-f77_repeat
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
	bge r4, r3, .LBB145_18
.LBB145_1:
	lui r1, %hi(fio_catpos)
	ldw r4, r1+%lo(fio_catpos)
	add r4, r4, r11
	blt r4, r3, .LBB145_3
.LBB145_2:
	stw r1+%lo(fio_catpos), r0
.LBB145_3:
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
	beq r5, r0, .LBB145_10
.LBB145_4:
	add r14, r11, r0
	blt r11, r1, .LBB145_9
.LBB145_5:
	addi r3, r0, 32
	addi r4, r13, -1
	add r5, r11, r0
.LBB145_6:
	add r14, r5, r0
	add r5, r4, r5
	ldbu r5, r5+0
	bne r5, r3, .LBB145_9
.LBB145_7:
	addi r5, r14, -1
	bgt r14, r1, .LBB145_6
.LBB145_8:
	add r14, r0, r0
.LBB145_9:
	sub r5, r11, r14
	addi r4, r0, 32
	add r3, r12, r0
	jal r31, memset
	add r1, r12, r11
	sub r3, r1, r14
	add r4, r13, r0
	add r5, r14, r0
	jal r31, memmove
	jal r0, .LBB145_17
.LBB145_10:
	blt r11, r1, .LBB145_15
.LBB145_11:
	addi r1, r0, 32
	add r14, r0, r0
.LBB145_12:
	add r3, r13, r14
	ldbu r3, r3+0
	bne r3, r1, .LBB145_16
.LBB145_13:
	addi r14, r14, 1
	bne r11, r14, .LBB145_12
.LBB145_14:
	add r14, r11, r0
	jal r0, .LBB145_16
.LBB145_15:
	add r14, r0, r0
.LBB145_16:
	add r4, r13, r14
	sub r5, r11, r14
	add r3, r12, r0
	jal r31, memmove
	add r1, r12, r11
	sub r3, r1, r14
	addi r4, r0, 32
	add r5, r14, r0
	jal r31, memset
.LBB145_17:
	add r1, r12, r0
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB145_18:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.125)
	addi r4, r4, %lo(.L.str.125)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end145:
	.size	f77_adjust, .Lfunc_end145-f77_adjust
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
	blt r4, r16, .LBB146_7
.LBB146_1:
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
.LBB146_2:
	xor r1, r21, r17
	and r1, r1, r19
	xor r22, r17, r1
	blt r12, r16, .LBB146_4
.LBB146_3:
	add r1, r14, r22
	ldb r4, r1+0
	add r3, r13, r0
	add r5, r12, r0
	jal r31, memchr
	sne r1, r1, r18
	jal r0, .LBB146_5
.LBB146_4:
	add r1, r0, r0
.LBB146_5:
	seq r3, r1, r11
	addi r4, r22, 1
	xor r5, r15, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r15, r4, r3
	bne r1, r11, .LBB146_8
.LBB146_6:
	addi r21, r21, 1
	addi r17, r17, -1
	bne r17, r20, .LBB146_2
.LBB146_7:
	add r15, r0, r0
.LBB146_8:
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
.Lfunc_end146:
	.size	f77_scanset, .Lfunc_end146-f77_scanset
                                        # -- End function
	.globl	f77_index_back                  # -- Begin function f77_index_back
	.p2align	2
	.type	f77_index_back,@function
f77_index_back:                         # @f77_index_back
# %bb.0:
	ble r6, r0, .LBB147_6
.LBB147_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r11, r6, r0
	sub r1, r4, r6
	blt r1, r0, .LBB147_5
.LBB147_2:
	add r12, r5, r0
	add r13, r3, r0
.LBB147_3:
	add r14, r1, r0
	add r3, r13, r1
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memcmp
	beq r1, r0, .LBB147_7
.LBB147_4:
	addi r1, r14, -1
	bgt r14, r0, .LBB147_3
.LBB147_5:
	add r1, r0, r0
	jal r0, .LBB147_8
.LBB147_6:
	addi r1, r0, 0
	sgt r1, r4, r1
	sub r1, r0, r1
	and r1, r4, r1
	addi r1, r1, 1
	jal r0, .LBB147_9
.LBB147_7:
	addi r1, r14, 1
.LBB147_8:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB147_9:
	jalr r0, r31, 0
.Lfunc_end147:
	.size	f77_index_back, .Lfunc_end147-f77_index_back
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
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB148_15
.LBB148_1:
	lui r1, %hi(fio_col)
	ldw r4, r1+%lo(fio_col)
	addi r5, r0, 1022
	bgt r4, r5, .LBB148_7
.LBB148_2:
	lui r6, %hi(fio_len)
	ldw r7, r6+%lo(fio_len)
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	addi r9, r0, 32
	bge r7, r4, .LBB148_5
.LBB148_3:
	addi r10, r7, 1
	add r7, r7, r8
	stb r7+0, r9
	add r7, r10, r0
	bne r4, r10, .LBB148_3
.LBB148_4:
	stw r6+%lo(fio_len), r4
	add r7, r4, r0
.LBB148_5:
	add r8, r4, r8
	stb r8+0, r9
	addi r8, r4, 1
	stw r1+%lo(fio_col), r8
	blt r4, r7, .LBB148_7
.LBB148_6:
	stw r6+%lo(fio_len), r8
.LBB148_7:
	ldw r4, r1+%lo(fio_col)
	bgt r4, r5, .LBB148_14
.LBB148_8:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	bge r6, r4, .LBB148_12
.LBB148_9:
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
.LBB148_10:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r4, r9, .LBB148_10
.LBB148_11:
	stw r5+%lo(fio_len), r4
	add r6, r4, r0
.LBB148_12:
	addi r7, r0, 0
	seq r3, r3, r7
	sub r3, r0, r3
	andi r3, r3, 18
	xori r3, r3, 84
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	add r7, r4, r7
	stb r7+0, r3
	addi r3, r4, 1
	stw r1+%lo(fio_col), r3
	blt r4, r6, .LBB148_14
.LBB148_13:
	stw r5+%lo(fio_len), r3
.LBB148_14:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	jal r0, .LBB148_40
.LBB148_15:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB148_40
.LBB148_16:
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB148_18
.LBB148_17:
	ldw r1, r12+%lo(fio_rep)
	bgt r1, r0, .LBB148_19
.LBB148_18:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	jal r31, fio_next_desc
	add r3, r13, r0
	beq r1, r0, .LBB148_40
.LBB148_19:
	lui r1, %hi(fio_w)
	ldw r13, r1+%lo(fio_w)
	addi r1, r0, 2
	blt r13, r1, .LBB148_29
.LBB148_20:
	lui r4, %hi(fio_len)
	ldw r9, r4+%lo(fio_len)
	lui r5, %hi(fio_col)
	ldw r10, r5+%lo(fio_col)
	addi r6, r0, 1022
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
	jal r0, .LBB148_23
.LBB148_21:
	add r10, r13, r0
.LBB148_22:
	addi r13, lr, -1
	ble lr, r1, .LBB148_29
.LBB148_23:
	add lr, r13, r0
	bgt r10, r6, .LBB148_22
.LBB148_24:
	bge r9, r10, .LBB148_27
.LBB148_25:
	addi r13, r9, 1
	add r9, r9, r7
	stb r9+0, r8
	add r9, r13, r0
	bne r10, r13, .LBB148_25
.LBB148_26:
	stw r4+%lo(fio_len), r10
	add r9, r10, r0
.LBB148_27:
	add r13, r10, r7
	stb r13+0, r8
	addi r13, r10, 1
	stw r5+%lo(fio_col), r13
	blt r10, r9, .LBB148_21
.LBB148_28:
	stw r4+%lo(fio_len), r13
	add r9, r13, r0
	jal r0, .LBB148_21
.LBB148_29:
	lui r4, %hi(fio_col)
	ldw r1, r4+%lo(fio_col)
	addi r5, r0, 1022
	bgt r1, r5, .LBB148_36
.LBB148_30:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	bge r6, r1, .LBB148_34
.LBB148_31:
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
.LBB148_32:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r1, r9, .LBB148_32
.LBB148_33:
	stw r5+%lo(fio_len), r1
	add r6, r1, r0
.LBB148_34:
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
	blt r1, r6, .LBB148_36
.LBB148_35:
	stw r5+%lo(fio_len), r3
.LBB148_36:
	ldw r1, r12+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB148_38
.LBB148_37:
	addi r1, r1, -1
	stw r12+%lo(fio_rep), r1
.LBB148_38:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB148_40
.LBB148_39:
	stw r11+%lo(fio_desc), r0
.LBB148_40:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end148:
	.size	f77_wr_l, .Lfunc_end148-f77_wr_l
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
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB149_4
.LBB149_1:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB149_3
.LBB149_2:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB149_4
.LBB149_3:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB149_4:
	lui r1, %hi(fio_noadv)
	ldbu r3, r1+%lo(fio_noadv)
	lui r11, %hi(fio_ibuf)
	ldw r4, r11+%lo(fio_ibuf)
	addi r5, r0, 0
	seq r4, r4, r5
	and r3, r3, r4
	lui r12, %hi(fio_open_rec)
	stw r12+%lo(fio_open_rec), r3
	stb r1+%lo(fio_noadv), r0
	jal r31, fio_flush_line
	stw r12+%lo(fio_open_rec), r0
	stw r11+%lo(fio_ibuf), r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end149:
	.size	f77_wr_end, .Lfunc_end149-f77_wr_end
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
	lui r27, %hi(.LJTI150_4)
	addi r27, r27, %lo(.LJTI150_4)
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
	jal r0, .LBB150_5
.LBB150_1:
	addi r1, r4, 1
.LBB150_2:
	stw r14+%lo(fio_pos), r1
.LBB150_3:
	bgtu r16, r18, .LBB150_109
.LBB150_4:
	addi r16, r16, 1
.LBB150_5:
	ldw r3, r15+%lo(fio_fmt)
	ldw r4, r14+%lo(fio_pos)
	add r7, r3, r4
	ldb r6, r7+0
	andi r1, r6, 255
	beq r1, r17, .LBB150_1
.LBB150_6:
	beq r1, r19, .LBB150_1
.LBB150_7:
	bne r1, r0, .LBB150_16
.LBB150_8:
	ldw r1, r20+%lo(fio_gdepth)
	blt r1, fp, .LBB150_10
.LBB150_9:
	stw r20+%lo(fio_gdepth), r0
.LBB150_10:
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB150_109
.LBB150_11:
	ldw r4, r22+%lo(fio_revert)
	add r1, r0, r0
	blt r4, r0, .LBB150_110
.LBB150_12:
	ldbu r1, r13+%lo(fio_since_data)
	beq r1, r0, .LBB150_131
.LBB150_13:
	stb r13+%lo(fio_since_data), r0
	stw r14+%lo(fio_pos), r4
	add r1, r3, r4
	ldbu r1, r1+0
	beq r1, r0, .LBB150_109
.LBB150_14:
	ldbu r1, r23+%lo(fio_reading)
	bne r1, fp, .LBB150_88
.LBB150_15:
	jal r31, fio_getrec
	beq r1, r0, .LBB150_3
	jal r0, .LBB150_132
.LBB150_16:
	and r5, r6, r24
	bne r5, r12, .LBB150_20
.LBB150_17:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	add r1, r3, r1
	ldbu r1, r1+0
	addi r1, r1, -80
	bgtu r1, r25, .LBB150_50
.LBB150_18:
	slli r1, r1, 2
	add r1, r27, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB150_19:
	stb r26+%lo(fio_sp), fp
	jal r0, .LBB150_28
.LBB150_20:
	bne r6, r28, .LBB150_23
.LBB150_21:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB150_109
.LBB150_22:
	add r1, r0, r0
	bleu r16, r18, .LBB150_4
	jal r0, .LBB150_110
.LBB150_23:
	andi r5, r6, 223
	ldw r8, sp+52
	bne r5, r8, .LBB150_29
.LBB150_24:
	ldbu r5, r7+1
	addi r8, r5, -78
	andi r9, r8, 252
	slli r8, r8, 6
	srli r9, r9, 2
	or  r8, r8, r9
	andi r8, r8, 255
	ldw r9, sp+44
	bgtu r8, r9, .LBB150_29
.LBB150_25:
	sll r8, fp, r8
	andi r8, r8, 2313
	beq r8, r0, .LBB150_29
.LBB150_26:
	andi r1, r5, 223
	ldw r3, sp+40
	seq r1, r1, r3
	ldw r3, sp+36
	stw r3+%lo(fio_bz), r1
	jal r0, .LBB150_28
.LBB150_27:
	stb r26+%lo(fio_sp), r0
.LBB150_28:
	addi r1, r4, 2
	jal r0, .LBB150_2
.LBB150_29:
	addi r5, r1, -39
	lui r1, %hi(fio_line)
	addi r1, r1, %lo(fio_line)
	ldw r8, sp+48
	bgtu r5, r8, .LBB150_53
.LBB150_30:
	slli r5, r5, 2
	lui r8, %hi(.LJTI150_0)
	addi r8, r8, %lo(.LJTI150_0)
	add r5, r8, r5
	ldw r8, r5+0
	add r5, fp, r0
	jalr r0, r8, 0
.LBB150_31:
	addi r4, r4, 1
	stw r14+%lo(fio_pos), r4
	lui r5, %hi(fio_col)
	ldw r10, r5+%lo(fio_col)
	lui r6, %hi(fio_len)
	ldw r8, r6+%lo(fio_len)
	ldbu r7, r23+%lo(fio_reading)
	jal r0, .LBB150_35
.LBB150_32:
	addi lr, r0, 1
.LBB150_33:
	add r10, r9, r0
.LBB150_34:
	add r4, r4, lr
	stw r14+%lo(fio_pos), r4
.LBB150_35:
	add r9, r10, r0
	add r10, r3, r4
	ldbu r10, r10+0
	addi lr, r0, 39
	beq r10, lr, .LBB150_42
.LBB150_36:
	beq r10, r0, .LBB150_3
.LBB150_37:
	addi lr, r0, 1022
	sgt lr, r9, lr
	or  lr, r7, lr
	andi lr, lr, 1
	bne lr, r0, .LBB150_32
.LBB150_38:
	bge r8, r9, .LBB150_41
.LBB150_39:
	addi lr, r8, 1
	add r8, r8, r1
	stb r8+0, r19
	add r8, lr, r0
	bne r9, lr, .LBB150_39
.LBB150_40:
	stw r6+%lo(fio_len), r9
	add r8, r9, r0
.LBB150_41:
	add lr, r9, r1
	stb lr+0, r10
	addi r10, r9, 1
	stw r5+%lo(fio_col), r10
	addi lr, r0, 1
	blt r9, r8, .LBB150_34
	jal r0, .LBB150_49
.LBB150_42:
	addi r10, r4, 1
	add r11, r3, r10
	ldbu r11, r11+0
	bne r11, lr, .LBB150_82
.LBB150_43:
	addi r10, r0, 1022
	sgt r10, r9, r10
	or  r10, r7, r10
	andi r10, r10, 1
	beq r10, r0, .LBB150_45
.LBB150_44:
	addi lr, r0, 2
	jal r0, .LBB150_33
.LBB150_45:
	bge r8, r9, .LBB150_48
.LBB150_46:
	addi r10, r8, 1
	add r8, r8, r1
	stb r8+0, r19
	add r8, r10, r0
	bne r9, r10, .LBB150_46
.LBB150_47:
	stw r6+%lo(fio_len), r9
	add r8, r9, r0
.LBB150_48:
	add r10, r9, r1
	stb r10+0, lr
	addi r10, r9, 1
	stw r5+%lo(fio_col), r10
	addi lr, r0, 2
	blt r9, r8, .LBB150_34
.LBB150_49:
	stw r6+%lo(fio_len), r10
	add r8, r10, r0
	jal r0, .LBB150_34
.LBB150_50:
	stb r26+%lo(fio_sp), r0
	jal r0, .LBB150_3
.LBB150_51:
	ldb r5, r7+1
	addi r5, r5, -58
	addi r6, r0, -10
	bgeu r5, r6, .LBB150_58
.LBB150_52:
	addi r6, r0, 45
.LBB150_53:
	add r5, fp, r0
	jal r0, .LBB150_59
.LBB150_54:
	ldw r1, r20+%lo(fio_gdepth)
	beq r1, r0, .LBB150_83
.LBB150_55:
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
	blt r5, r4, .LBB150_3
.LBB150_56:
	lui r4, %hi(fio_gstart)
	addi r4, r4, %lo(fio_gstart)
	add r3, r3, r4
	ldw r3, r3+0
	stw r14+%lo(fio_pos), r3
	stw r20+%lo(fio_gdepth), r1
	jal r0, .LBB150_3
.LBB150_57:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB150_87
.LBB150_58:
	addi r5, r4, 1
	stw r14+%lo(fio_pos), r5
	add r5, r3, r5
	ldb r6, r5+0
	add r5, r0, r0
.LBB150_59:
	addi r6, r6, -58
	addi r10, r0, -10
	add r11, fp, r0
	bltu r6, r10, .LBB150_65
.LBB150_60:
	ldw r6, r14+%lo(fio_pos)
	add r7, r3, r6
	ldb r7, r7+0
	addi r7, r7, -48
	addi r8, r0, 9
	bleu r7, r8, .LBB150_62
.LBB150_61:
	add r11, r0, r0
	jal r0, .LBB150_65
.LBB150_62:
	stw sp+32, r10
	addi r8, r3, 1
	add r11, r0, r0
.LBB150_63:
	addi r10, r0, 10
	mul lr, r11, r10
	addi r9, r6, 1
	add r11, lr, r7
	add r6, r8, r6
	ldb r6, r6+0
	addi r7, r6, -48
	add r6, r9, r0
	bltu r7, r10, .LBB150_63
.LBB150_64:
	stw r14+%lo(fio_pos), r9
	ldw r10, sp+32
.LBB150_65:
	ldw r6, r14+%lo(fio_pos)
	add r7, r3, r6
	ldbu r7, r7+0
	addi r8, r0, 40
	bne r7, r8, .LBB150_69
.LBB150_66:
	addi r3, r6, 1
	stw r14+%lo(fio_pos), r3
	ldw r1, r20+%lo(fio_gdepth)
	addi r5, r0, 15
	bgt r1, r5, .LBB150_3
.LBB150_67:
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
	bne r1, r0, .LBB150_3
.LBB150_68:
	stw r22+%lo(fio_revert), r4
	jal r0, .LBB150_3
.LBB150_69:
	andi r4, r7, 223
	addi r8, r4, -72
	addi r9, r0, 16
	bgtu r8, r9, .LBB150_101
.LBB150_70:
	slli r8, r8, 2
	lui r9, %hi(.LJTI150_1)
	addi r9, r9, %lo(.LJTI150_1)
	add r8, r9, r8
	ldw r8, r8+0
	jalr r0, r8, 0
.LBB150_71:
	addi r4, r6, 1
	addi r5, r0, 1
	blt r11, r5, .LBB150_97
.LBB150_72:
	lui r6, %hi(fio_len)
	stw sp+32, r6
	ldw r9, r6+%lo(fio_len)
	lui r7, %hi(fio_col)
	ldw r10, r7+%lo(fio_col)
	ldbu r8, r23+%lo(fio_reading)
	jal r0, .LBB150_75
.LBB150_73:
	add r10, r11, r0
.LBB150_74:
	addi r11, lr, -1
	addi r4, r4, 1
	ble lr, r5, .LBB150_97
.LBB150_75:
	add lr, r11, r0
	add r11, r3, r4
	ldbu r11, r11+0
	beq r11, r0, .LBB150_97
.LBB150_76:
	addi r6, r0, 1022
	sgt r6, r10, r6
	or  r6, r8, r6
	andi r6, r6, 1
	bne r6, r0, .LBB150_74
.LBB150_77:
	bge r9, r10, .LBB150_80
.LBB150_78:
	addi r6, r9, 1
	add r9, r9, r1
	stb r9+0, r19
	add r9, r6, r0
	bne r10, r6, .LBB150_78
.LBB150_79:
	ldw r6, sp+32
	stw r6+%lo(fio_len), r10
	add r9, r10, r0
.LBB150_80:
	add r6, r10, r1
	stb r6+0, r11
	addi r11, r10, 1
	stw r7+%lo(fio_col), r11
	blt r10, r9, .LBB150_73
.LBB150_81:
	ldw r6, sp+32
	stw r6+%lo(fio_len), r11
	add r9, r11, r0
	jal r0, .LBB150_73
.LBB150_82:
	stw r14+%lo(fio_pos), r10
	jal r0, .LBB150_3
.LBB150_83:
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB150_109
.LBB150_84:
	ldw r4, r22+%lo(fio_revert)
	add r1, r0, r0
	blt r4, r0, .LBB150_110
.LBB150_85:
	ldbu r1, r13+%lo(fio_since_data)
	beq r1, r0, .LBB150_131
.LBB150_86:
	stb r13+%lo(fio_since_data), r0
	stw r14+%lo(fio_pos), r4
	add r1, r3, r4
	ldbu r1, r1+0
	beq r1, r0, .LBB150_109
.LBB150_87:
	ldbu r1, r23+%lo(fio_reading)
	addi r3, r0, 1
	beq r1, r3, .LBB150_15
.LBB150_88:
	jal r31, fio_flush_line
	jal r0, .LBB150_3
.LBB150_89:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	add r1, r3, r1
	ldbu r1, r1+0
	add r8, r0, r0
	addi r11, r0, 1
	addi r4, r1, -76
	addi r1, r0, 38
	bgtu r4, r1, .LBB150_108
.LBB150_90:
	addi r1, r0, -1
	slli r4, r4, 2
	lui r5, %hi(.LJTI150_2)
	addi r5, r5, %lo(.LJTI150_2)
	add r4, r5, r4
	ldw r4, r4+0
	add r7, r11, r0
	jalr r0, r4, 0
.LBB150_91:
	addi r1, r0, 1
.LBB150_92:
	addi r4, r6, 2
	stw r14+%lo(fio_pos), r4
	add r7, r0, r0
	add r8, r1, r0
.LBB150_93:
	ldw r1, r14+%lo(fio_pos)
	add r1, r3, r1
	ldb r1, r1+0
	addi r3, r1, -58
	add r1, r11, r0
	bltu r3, r10, .LBB150_95
.LBB150_94:
	stw sp+32, r7
	stw sp+28, r8
	jal r31, fio_number
	ldw r8, sp+28
	ldw r7, sp+32
.LBB150_95:
	ldbu r4, r23+%lo(fio_reading)
	mul r3, r1, r8
	addi r1, r1, -1
	bne r4, r11, .LBB150_105
.LBB150_96:
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
	jal r0, .LBB150_3
.LBB150_97:
	stw r14+%lo(fio_pos), r4
	jal r0, .LBB150_3
.LBB150_98:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	sub r1, r0, r11
	xor r3, r11, r1
	sub r4, r0, r5
	and r3, r3, r4
	xor r1, r1, r3
	lui r3, %hi(fio_scale)
	stw r3+%lo(fio_scale), r1
	jal r0, .LBB150_3
.LBB150_99:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	ldbu r3, r23+%lo(fio_reading)
	addi r1, r0, 1
	bne r3, r1, .LBB150_106
.LBB150_100:
	lui r1, %hi(fio_rpos)
	ldw r3, r1+%lo(fio_rpos)
	add r3, r3, r11
	stw r1+%lo(fio_rpos), r3
	jal r0, .LBB150_3
.LBB150_101:
	andi r1, r7, 222
	addi r5, r0, 68
	andi r7, r7, 255
	beq r1, r5, .LBB150_111
.LBB150_102:
	addi r1, r7, -65
	addi r5, r0, 57
	bgtu r1, r5, .LBB150_104
.LBB150_103:
	slli r1, r1, 2
	lui r5, %hi(.LJTI150_3)
	addi r5, r5, %lo(.LJTI150_3)
	add r1, r5, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB150_104:
	addi r1, r6, 1
	jal r0, .LBB150_2
.LBB150_105:
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
	jal r0, .LBB150_3
.LBB150_106:
	blt r11, r1, .LBB150_3
.LBB150_107:
	lui r1, %hi(fio_col)
	ldw r3, r1+%lo(fio_col)
	add r3, r3, r11
	stw r1+%lo(fio_col), r3
	jal r0, .LBB150_3
.LBB150_108:
	add r7, r11, r0
	jal r0, .LBB150_93
.LBB150_109:
	add r1, r0, r0
.LBB150_110:
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
.LBB150_111:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	addi r5, r0, 69
	add r19, r7, r0
	bne r4, r5, .LBB150_115
.LBB150_112:
	add r16, r10, r0
	add r1, r3, r1
	ldbu r3, r1+0
	jal r31, toupper
	beq r1, r12, .LBB150_114
.LBB150_113:
	ldw r1, r15+%lo(fio_fmt)
	ldw r3, r14+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB150_116
.LBB150_114:
	ldw r1, r15+%lo(fio_fmt)
	ldw r3, r14+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+0
	jal r31, toupper
	add r12, r1, r0
	ldw r1, r14+%lo(fio_pos)
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB150_117
.LBB150_115:
	add r12, r0, r0
	jal r0, .LBB150_118
.LBB150_116:
	add r12, r0, r0
.LBB150_117:
	add r10, r16, r0
	add r7, r19, r0
.LBB150_118:
	ldw r15, r15+%lo(fio_fmt)
	ldw r1, r14+%lo(fio_pos)
	add r1, r15, r1
	ldb r1, r1+0
	addi r1, r1, -58
	bgeu r1, r10, .LBB150_120
.LBB150_119:
	add r1, r0, r0
	jal r0, .LBB150_121
.LBB150_120:
	add r16, r10, r0
	jal r31, fio_number
	add r7, r19, r0
	add r10, r16, r0
.LBB150_121:
	lui r3, %hi(fio_w)
	stw r3+%lo(fio_w), r1
	lui r16, %hi(fio_d)
	stw r16+%lo(fio_d), r0
	lui r17, %hi(fio_dgiven)
	stb r17+%lo(fio_dgiven), r0
	ldw r1, r14+%lo(fio_pos)
	add r3, r15, r1
	ldbu r3, r3+0
	addi r4, r0, 46
	bne r3, r4, .LBB150_123
.LBB150_122:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	add r18, r10, r0
	jal r31, fio_number
	add r7, r19, r0
	add r10, r18, r0
	stw r16+%lo(fio_d), r1
	addi r1, r0, 1
	stb r17+%lo(fio_dgiven), r1
.LBB150_123:
	lui r16, %hi(fio_e)
	stw r16+%lo(fio_e), r0
	ldw r1, r14+%lo(fio_pos)
	add r3, r15, r1
	ldbu r3, r3+0
	ori  r3, r3, 32
	addi r4, r0, 101
	bne r3, r4, .LBB150_127
.LBB150_124:
	addi r3, r1, 1
	stw r14+%lo(fio_pos), r3
	add r3, r15, r3
	ldb r3, r3+0
	addi r3, r3, -58
	bgeu r3, r10, .LBB150_126
.LBB150_125:
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB150_127
.LBB150_126:
	jal r31, fio_number
	add r7, r19, r0
	stw r16+%lo(fio_e), r1
.LBB150_127:
	lui r1, %hi(fio_rep)
	stw r1+%lo(fio_rep), r11
	beq r12, r0, .LBB150_129
.LBB150_128:
	addi r1, r12, 256
	jal r0, .LBB150_130
.LBB150_129:
	add r3, r7, r0
	jal r31, toupper
.LBB150_130:
	lui r3, %hi(fio_desc)
	stw r3+%lo(fio_desc), r1
	addi r3, r0, 1
	stb r13+%lo(fio_since_data), r3
	jal r0, .LBB150_110
.LBB150_131:
	jal r31, fio_flush_line
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.126)
	addi r4, r4, %lo(.L.str.126)
	jal r0, .LBB150_133
.LBB150_132:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
.LBB150_133:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end150:
	.size	fio_next_desc, .Lfunc_end150-fio_next_desc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI150_0,@object
.LJTI150_0:
	.word	.LBB150_31
	.word	.LBB150_59
	.word	.LBB150_54
	.word	.LBB150_59
	.word	.LBB150_59
	.word	.LBB150_59
	.word	.LBB150_51
	.word	.LBB150_59
	.word	.LBB150_57
	.size	.LJTI150_0, 36
	.type	.LJTI150_1,@object
.LJTI150_1:
	.word	.LBB150_71
	.word	.LBB150_101
	.word	.LBB150_101
	.word	.LBB150_101
	.word	.LBB150_101
	.word	.LBB150_101
	.word	.LBB150_101
	.word	.LBB150_101
	.word	.LBB150_98
	.word	.LBB150_101
	.word	.LBB150_101
	.word	.LBB150_101
	.word	.LBB150_89
	.word	.LBB150_101
	.word	.LBB150_101
	.word	.LBB150_101
	.word	.LBB150_99
	.size	.LJTI150_1, 68
	.type	.LJTI150_2,@object
.LJTI150_2:
	.word	.LBB150_92
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_91
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_92
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_93
	.word	.LBB150_91
	.size	.LJTI150_2, 156
	.type	.LJTI150_3,@object
.LJTI150_3:
	.word	.LBB150_111
	.word	.LBB150_111
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_111
	.word	.LBB150_111
	.word	.LBB150_104
	.word	.LBB150_111
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_111
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_111
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_111
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_111
	.word	.LBB150_111
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_111
	.word	.LBB150_111
	.word	.LBB150_104
	.word	.LBB150_111
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_111
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_111
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_104
	.word	.LBB150_111
	.size	.LJTI150_3, 232
	.type	.LJTI150_4,@object
.LJTI150_4:
	.word	.LBB150_19
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_27
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_19
	.word	.LBB150_50
	.word	.LBB150_50
	.word	.LBB150_27
	.size	.LJTI150_4, 144
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
	beq r3, r0, .LBB151_9
.LBB151_1:
	lui r11, %hi(fio_irec)
	ldw r1, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r1, r4, .LBB151_21
.LBB151_2:
	lui r14, %hi(fio_ilen)
	ldw r6, r14+%lo(fio_ilen)
	bgt r5, r6, .LBB151_22
.LBB151_3:
	mul r1, r6, r1
	add r3, r3, r1
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	jal r31, memcpy
	ldw r3, r13+%lo(fio_len)
	ldw r1, r14+%lo(fio_ilen)
	ble r1, r3, .LBB151_6
.LBB151_4:
	ldw r4, r12+%lo(fio_ibuf)
	ldw r5, r11+%lo(fio_irec)
	mul r5, r5, r1
	sub r1, r1, r3
	add r3, r3, r5
	add r3, r4, r3
	addi r4, r0, 32
.LBB151_5:
	stb r3+0, r4
	addi r1, r1, -1
	addi r3, r3, 1
	bne r1, r0, .LBB151_5
.LBB151_6:
	ldw r1, r11+%lo(fio_irec)
	addi r1, r1, 1
	stw r11+%lo(fio_irec), r1
.LBB151_7:
	stw r13+%lo(fio_len), r0
.LBB151_8:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB151_9:
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
	bgtu r3, r14, .LBB151_15
.LBB151_10:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB151_15
.LBB151_11:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r12, r1+0
	bgt r5, r12, .LBB151_24
.LBB151_12:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	ldw r1, r13+%lo(fio_len)
	ble r12, r1, .LBB151_7
.LBB151_13:
	sub r14, r12, r1
	addi r12, r0, 32
.LBB151_14:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fputc
	addi r14, r14, -1
	bne r14, r0, .LBB151_14
	jal r0, .LBB151_7
.LBB151_15:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	lui r1, %hi(fio_open_rec)
	ldw r3, r1+%lo(fio_open_rec)
	beq r3, r0, .LBB151_17
.LBB151_16:
	stw r1+%lo(fio_open_rec), r0
	jal r0, .LBB151_18
.LBB151_17:
	addi r3, r0, 10
	add r4, r11, r0
	jal r31, fputc
.LBB151_18:
	stw r13+%lo(fio_len), r0
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	bgtu r3, r14, .LBB151_8
.LBB151_19:
	addi r1, r1, -7
	addi r3, r0, -3
	bgtu r1, r3, .LBB151_8
.LBB151_20:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
	jal r0, .LBB151_8
.LBB151_21:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.127)
	addi r4, r4, %lo(.L.str.127)
	jal r0, .LBB151_23
.LBB151_22:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.128)
	addi r4, r4, %lo(.L.str.128)
.LBB151_23:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB151_24:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.129)
	addi r4, r4, %lo(.L.str.129)
	add r6, r12, r0
	jal r0, .LBB151_23
.Lfunc_end151:
	.size	fio_flush_line, .Lfunc_end151-fio_flush_line
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
	stw sp+16, r14
	add r13, r3, r0
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB152_4
.LBB152_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB152_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB152_2
.LBB152_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB152_4:
	addi r12, r0, 1
	add r3, r13, r0
	add r4, r12, r0
	jal r31, fio_resolve
	add r11, r1, r0
	lui r1, %hi(stdout)
	ldw r1, r1+%lo(stdout)
	beq r11, r1, .LBB152_14
.LBB152_5:
	lui r1, %hi(stderr)
	ldw r1, r1+%lo(stderr)
	beq r11, r1, .LBB152_14
.LBB152_6:
	addi r1, r13, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB152_15
.LBB152_7:
	slli r14, r13, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r1, r14, r1
	ldw r1, r1+0
	beq r1, r0, .LBB152_10
.LBB152_8:
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r14, r1
	ldw r1, r1+0
	bne r1, r0, .LBB152_10
.LBB152_9:
	lui r1, %hi(fio_uwrote)
	addi r1, r1, %lo(fio_uwrote)
	add r1, r14, r1
	stw r1+0, r12
.LBB152_10:
	addi r1, r13, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB152_13
.LBB152_11:
	lui r1, %hi(fio_uwrote)
	addi r1, r1, %lo(fio_uwrote)
	add r1, r14, r1
	ldw r3, r1+0
	beq r3, r0, .LBB152_13
.LBB152_12:
	stw r1+0, r0
	add r3, r11, r0
	jal r31, fflush
	add r3, r11, r0
	jal r31, fileno
	add r13, r1, r0
	add r3, r11, r0
	jal r31, ftell
	add r3, r13, r0
	add r4, r1, r0
	jal r31, ftruncate
.LBB152_13:
	add r3, r11, r0
	jal r31, fflush
	add r3, r11, r0
	jal r31, ftell
	lui r3, %hi(fio_uend)
	addi r3, r3, %lo(fio_uend)
	add r3, r14, r3
	stw r3+0, r1
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r14, r1
	stw r1+0, r12
.LBB152_14:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB152_15:
	addi r1, r13, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB152_18
.LBB152_16:
	slli r1, r13, 2
	lui r3, %hi(fio_uwrote)
	addi r3, r3, %lo(fio_uwrote)
	add r1, r1, r3
	ldw r3, r1+0
	beq r3, r0, .LBB152_18
.LBB152_17:
	stw r1+0, r0
	add r3, r11, r0
	jal r31, fflush
	add r3, r11, r0
	jal r31, fileno
	add r12, r1, r0
	add r3, r11, r0
	jal r31, ftell
	add r3, r12, r0
	add r4, r1, r0
	jal r31, ftruncate
.LBB152_18:
	add r3, r11, r0
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fflush
.Lfunc_end152:
	.size	f77_endfile, .Lfunc_end152-f77_endfile
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
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	add r5, r11, r0
	add r6, r12, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fprintf
.Lfunc_end153:
	.size	f77_pause, .Lfunc_end153-f77_pause
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
	add r14, r3, r0
	lui r15, %hi(fio_busy)
	ldbu r3, r15+%lo(fio_busy)
	addi r1, r0, 1
	beq r3, r1, .LBB154_22
.LBB154_1:
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	stb r15+%lo(fio_busy), r1
	lui r3, %hi(fio_uend_init)
	ldbu r4, r3+%lo(fio_uend_init)
	bne r4, r0, .LBB154_5
.LBB154_2:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
	addi r6, r0, -1
.LBB154_3:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r0, .LBB154_3
.LBB154_4:
	stb r3+%lo(fio_uend_init), r1
.LBB154_5:
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	add r3, r14, r0
	add r4, r0, r0
	jal r31, fio_resolve
	lui r3, %hi(fio_in)
	stw r3+%lo(fio_in), r1
	lui r3, %hi(fio_unit)
	stw r3+%lo(fio_unit), r14
	addi r3, r14, -100
	addi r4, r0, -99
	bltu r3, r4, .LBB154_8
.LBB154_6:
	slli r5, r14, 2
	lui r6, %hi(fio_ufile)
	addi r6, r6, %lo(fio_ufile)
	add r6, r5, r6
	ldw r6, r6+0
	beq r6, r0, .LBB154_8
.LBB154_7:
	lui r6, %hi(fio_uaction)
	addi r6, r6, %lo(fio_uaction)
	add r5, r5, r6
	ldw r5, r5+0
	addi r6, r0, 2
	beq r5, r6, .LBB154_23
.LBB154_8:
	addi r5, r14, -1
	addi r6, r0, 98
	bgtu r5, r6, .LBB154_14
.LBB154_9:
	slli r14, r14, 2
	lui r5, %hi(fio_uunf)
	addi r5, r5, %lo(fio_uunf)
	add r5, r14, r5
	stw r5+0, r0
	lui r5, %hi(fio_uwrote)
	addi r5, r5, %lo(fio_uwrote)
	add r5, r14, r5
	stw r5+0, r0
	lui r6, %hi(fio_rec_next)
	ldw r5, r6+%lo(fio_rec_next)
	stw r6+%lo(fio_rec_next), r0
	bltu r3, r4, .LBB154_13
.LBB154_10:
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r14, r3
	ldw r3, r3+0
	beq r3, r0, .LBB154_13
.LBB154_11:
	ble r5, r0, .LBB154_24
.LBB154_12:
	addi r3, r5, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r4, r14, r4
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB154_13:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r14, r1
	ldw r1, r1+0
	jal r0, .LBB154_19
.LBB154_14:
	lui r6, %hi(fio_rec_next)
	ldw r5, r6+%lo(fio_rec_next)
	stw r6+%lo(fio_rec_next), r0
	bltu r3, r4, .LBB154_18
.LBB154_15:
	slli r3, r14, 2
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r3, r4
	ldw r4, r4+0
	beq r4, r0, .LBB154_18
.LBB154_16:
	ble r5, r0, .LBB154_24
.LBB154_17:
	addi r4, r5, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB154_18:
	add r1, r0, r0
.LBB154_19:
	lui r3, %hi(fio_bz)
	stw r3+%lo(fio_bz), r1
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, fio_rd_setup
	beq r1, r0, .LBB154_21
.LBB154_20:
	stb r15+%lo(fio_busy), r0
.LBB154_21:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB154_22:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r0, .LBB154_25
.LBB154_23:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.130)
	addi r4, r4, %lo(.L.str.130)
	lui r5, %hi(.L.str.132)
	addi r5, r5, %lo(.L.str.132)
	lui r7, %hi(.L.str.73)
	addi r7, r7, %lo(.L.str.73)
	add r6, r14, r0
	jal r0, .LBB154_25
.LBB154_24:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.98)
	addi r4, r4, %lo(.L.str.98)
.LBB154_25:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end154:
	.size	f77_rd_begin, .Lfunc_end154-f77_rd_begin
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
	add r11, r5, r0
	lui r14, %hi(fio_unit)
	ldw r1, r14+%lo(fio_unit)
	bgt r1, r0, .LBB155_2
.LBB155_1:
	lui r1, %hi(fio_bz)
	stw r1+%lo(fio_bz), r0
.LBB155_2:
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
	lui r18, %hi(fio_listed)
	stw r18+%lo(fio_listed), r1
	lui r1, %hi(.L.str.8)
	addi r1, r1, %lo(.L.str.8)
	lui r17, %hi(fio_fmt)
	stw r17+%lo(fio_fmt), r1
	lui r15, %hi(fio_pos)
	stw r15+%lo(fio_pos), r0
	lui r16, %hi(fio_revert)
	addi r19, r0, -1
	stw r16+%lo(fio_revert), r19
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	beq r3, r0, .LBB155_4
.LBB155_3:
	jal r31, fio_fmt_use
	ble r1, r19, .LBB155_17
.LBB155_4:
	ldw r1, r18+%lo(fio_listed)
	bne r1, r0, .LBB155_10
.LBB155_5:
	ldw r3, r17+%lo(fio_fmt)
	ldw r1, r15+%lo(fio_pos)
	addi r4, r0, 32
.LBB155_6:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB155_8
.LBB155_7:
	addi r1, r1, 1
	stw r15+%lo(fio_pos), r1
	jal r0, .LBB155_6
.LBB155_8:
	addi r3, r0, 40
	bne r5, r3, .LBB155_10
.LBB155_9:
	addi r1, r1, 1
	stw r15+%lo(fio_pos), r1
	stw r16+%lo(fio_revert), r1
.LBB155_10:
	lui r1, %hi(fio_ibuf)
	ldw r1, r1+%lo(fio_ibuf)
	beq r1, r0, .LBB155_14
.LBB155_11:
	jal r31, fio_getrec
	beq r1, r0, .LBB155_16
.LBB155_12:
	stb r13+%lo(fio_reading), r0
	lui r1, %hi(fio_noadv)
	stb r1+%lo(fio_noadv), r0
	andi r1, r11, 3
	bne r1, r0, .LBB155_19
.LBB155_13:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB155_14:
	ldw r1, r14+%lo(fio_unit)
	addi r3, r1, -1
	addi r4, r0, 99
	sltu r3, r3, r4
	sub r3, r0, r3
	and r1, r1, r3
	slli r15, r1, 2
	lui r1, %hi(fio_pend)
	addi r1, r1, %lo(fio_pend)
	add r14, r15, r1
	ldw r1, r14+0
	beq r1, r0, .LBB155_11
.LBB155_15:
	lui r1, %hi(fio_psave)
	addi r1, r1, %lo(fio_psave)
	add r1, r15, r1
	ldw r4, r1+0
	lui r1, %hi(fio_plen)
	addi r1, r1, %lo(fio_plen)
	add r11, r15, r1
	ldw r5, r11+0
	lui r3, %hi(fio_rec)
	addi r3, r3, %lo(fio_rec)
	jal r31, memcpy
	ldw r1, r11+0
	lui r3, %hi(fio_rlen)
	stw r3+%lo(fio_rlen), r1
	lui r1, %hi(fio_ppos)
	addi r1, r1, %lo(fio_ppos)
	add r1, r15, r1
	ldw r1, r1+0
	lui r3, %hi(fio_rpos)
	stw r3+%lo(fio_rpos), r1
	stw r14+0, r0
.LBB155_16:
	add r12, r0, r0
	jal r0, .LBB155_19
.LBB155_17:
	stb r13+%lo(fio_reading), r0
	andi r1, r11, 2
	beq r1, r0, .LBB155_20
.LBB155_18:
	addi r12, r0, 2
.LBB155_19:
	add r1, r12, r0
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
.LBB155_20:
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end155:
	.size	fio_rd_setup, .Lfunc_end155-fio_rd_setup
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
	beq r9, r1, .LBB156_8
.LBB156_1:
	stb r11+%lo(fio_busy), r1
	lui r9, %hi(fio_uend_init)
	ldbu r10, r9+%lo(fio_uend_init)
	bne r10, r0, .LBB156_5
.LBB156_2:
	addi r10, r0, -400
	lui lr, %hi(fio_uend+400)
	addi lr, lr, %lo(fio_uend+400)
	addi r12, r0, -1
.LBB156_3:
	add r13, r10, lr
	stw r13+0, r12
	addi r10, r10, 4
	bne r10, r0, .LBB156_3
.LBB156_4:
	stb r9+%lo(fio_uend_init), r1
.LBB156_5:
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
	beq r1, r0, .LBB156_7
.LBB156_6:
	stb r11+%lo(fio_busy), r0
.LBB156_7:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB156_8:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end156:
	.size	f77_rd_begin_int, .Lfunc_end156-f77_rd_begin_int
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
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB157_5
.LBB157_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB157_39
.LBB157_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB157_39
.LBB157_3:
	addi r3, r0, 1
	bne r1, r3, .LBB157_17
.LBB157_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB157_39
.LBB157_5:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	lui r14, %hi(fio_rep)
	add r12, r0, r0
	bne r1, r0, .LBB157_9
.LBB157_6:
	lui r1, %hi(fio_desc)
	ldw r12, r1+%lo(fio_desc)
	beq r12, r0, .LBB157_8
.LBB157_7:
	ldw r3, r14+%lo(fio_rep)
	bgt r3, r0, .LBB157_9
.LBB157_8:
	stw r1+%lo(fio_desc), r0
	jal r31, fio_next_desc
	add r12, r1, r0
.LBB157_9:
	addi r1, r12, -65
	addi r3, r0, 25
	bgtu r1, r3, .LBB157_20
.LBB157_10:
	slli r1, r1, 2
	lui r3, %hi(.LJTI157_1)
	addi r3, r3, %lo(.LJTI157_1)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB157_11:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 28
	jal r31, fio_field_in
	ldbu r1, sp+28
	beq r1, r0, .LBB157_34
.LBB157_12:
	addi r3, r0, 79
	seq r3, r12, r3
	addi r4, r0, 66
	seq r4, r12, r4
	sub r3, r0, r3
	andi r5, r3, 24
	xori r5, r5, 16
	sub r4, r0, r4
	ori  r6, r5, 2
	and r6, r6, r4
	xor r13, r5, r6
	andi r3, r3, 7
	xori r5, r3, 4
	xori r3, r3, 5
	and r3, r3, r4
	xor r16, r5, r3
	addi r3, sp, 28
	addi r15, r3, 1
	addi r17, r0, 32
	addi r18, r0, 10
	addi r19, r0, 6
	add r12, r0, r0
	jal r0, .LBB157_14
.LBB157_13:
	ldbu r1, r15+0
	addi r15, r15, 1
	beq r1, r0, .LBB157_35
.LBB157_14:
	andi r3, r1, 255
	jal r31, toupper
	beq r1, r17, .LBB157_13
.LBB157_15:
	addi r3, r1, -48
	sltu r4, r3, r18
	addi r5, r1, -65
	sltu r5, r5, r19
	addi r1, r1, -55
	sub r5, r0, r5
	xori r1, r1, 99
	and r1, r1, r5
	xori r1, r1, 99
	xor r3, r3, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	bge r1, r13, .LBB157_43
.LBB157_16:
	sll r3, r12, r16
	add r12, r1, r3
	jal r0, .LBB157_13
.LBB157_17:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	ldbu r1, r12+0
	addi r3, r1, -46
	andi r4, r3, 254
	slli r3, r3, 7
	srli r4, r4, 1
	or  r3, r3, r4
	andi r3, r3, 255
	addi r4, r0, 35
	bgtu r3, r4, .LBB157_40
.LBB157_18:
	slli r3, r3, 2
	lui r4, %hi(.LJTI157_0)
	addi r4, r4, %lo(.LJTI157_0)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB157_19:
	andi r1, r1, 255
	addi r3, r0, 46
	seq r1, r1, r3
	add r1, r1, r12
	ldbu r1, r1+0
	andi r1, r1, 223
	addi r3, r0, 84
	seq r1, r1, r3
	stw r11+0, r1
	jal r0, .LBB157_39
.LBB157_20:
	beq r12, r0, .LBB157_39
.LBB157_21:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.w.d r12, r4
	jal r0, .LBB157_35
.LBB157_22:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r12, sp, 28
	add r3, r12, r0
	jal r31, fio_field_in
	ldbu r1, sp+28
	addi r3, r0, 46
	seq r1, r1, r3
	or  r1, r12, r1
	ldbu r1, r1+0
	addi r3, r0, 83
	ble r1, r3, .LBB157_32
.LBB157_23:
	addi r12, r0, 1
	addi r3, r0, 84
	beq r1, r3, .LBB157_35
.LBB157_24:
	addi r3, r0, 116
	beq r1, r3, .LBB157_35
.LBB157_25:
	addi r3, r0, 102
	beq r1, r3, .LBB157_34
	jal r0, .LBB157_46
.LBB157_26:
	addi r4, r0, 4
	add r3, r11, r0
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
.LBB157_27:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 28
	jal r31, fio_field_in
	ldbu r1, sp+28
	beq r1, r0, .LBB157_34
.LBB157_28:
	addi r13, sp, 28
	addi r4, sp, 24
	addi r5, r0, 10
	add r3, r13, r0
	jal r31, strtol
	add r12, r1, r0
	ldw r1, sp+24
	beq r1, r13, .LBB157_30
.LBB157_29:
	ldbu r1, r1+0
	beq r1, r0, .LBB157_35
.LBB157_30:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.134)
	addi r4, r4, %lo(.L.str.134)
.LBB157_31:
	addi r5, sp, 28
	jal r0, .LBB157_44
.LBB157_32:
	beq r1, r0, .LBB157_34
.LBB157_33:
	addi r3, r0, 70
	bne r1, r3, .LBB157_46
.LBB157_34:
	add r12, r0, r0
.LBB157_35:
	stw r11+0, r12
	ldw r1, r14+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB157_37
.LBB157_36:
	addi r1, r1, -1
	stw r14+%lo(fio_rep), r1
.LBB157_37:
	ldw r1, r14+%lo(fio_rep)
	bne r1, r0, .LBB157_39
.LBB157_38:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB157_39:
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
.LBB157_40:
	addi r4, sp, 28
	addi r5, r0, 10
	add r3, r12, r0
	jal r31, strtol
	ldw r3, sp+28
	beq r3, r12, .LBB157_45
.LBB157_41:
	ldbu r3, r3+0
	bne r3, r0, .LBB157_45
.LBB157_42:
	stw r11+0, r1
	jal r0, .LBB157_39
.LBB157_43:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	ldb r5, r15+-1
	lui r4, %hi(.L.str.136)
	addi r4, r4, %lo(.L.str.136)
.LBB157_44:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB157_45:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	jal r31, fio_bad_num
.LBB157_46:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.135)
	addi r4, r4, %lo(.L.str.135)
	jal r0, .LBB157_31
.Lfunc_end157:
	.size	f77_rd_i, .Lfunc_end157-f77_rd_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI157_0,@object
.LJTI157_0:
	.word	.LBB157_19
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_19
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_19
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_19
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_40
	.word	.LBB157_19
	.size	.LJTI157_0, 144
	.type	.LJTI157_1,@object
.LJTI157_1:
	.word	.LBB157_26
	.word	.LBB157_11
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_27
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_22
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_11
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_21
	.word	.LBB157_11
	.size	.LJTI157_1, 104
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
	blt r1, r20, .LBB158_2
.LBB158_1:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
	addi r11, r0, 2
	jal r0, .LBB158_5
.LBB158_2:
	lui r14, %hi(fio_l_pending)
	ldw r3, r14+%lo(fio_l_pending)
	lui r1, %hi(fio_l_rep)
	blt r3, r20, .LBB158_6
.LBB158_3:
	addi r3, r3, -1
	stw r14+%lo(fio_l_pending), r3
	addi r3, r0, 1
	stb r1+%lo(fio_l_rep), r3
.LBB158_4:
	add r11, r0, r0
.LBB158_5:
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
.LBB158_6:
	stb r1+%lo(fio_l_rep), r0
	lui r15, %hi(fio_rpos)
	lui r17, %hi(fio_rlen)
	lui r16, %hi(fio_rec)
	addi r16, r16, %lo(fio_rec)
	addi r18, r0, 38
	lui r21, %hi(.LJTI158_0)
	addi r21, r21, %lo(.LJTI158_0)
	lui r12, %hi(fio_l_after)
	addi r11, r0, 2
	jal r0, .LBB158_8
.LBB158_7:
	jal r31, fio_getrec
	bne r1, r0, .LBB158_44
.LBB158_8:
	ldw r3, r15+%lo(fio_rpos)
	ldw r1, r17+%lo(fio_rlen)
	bge r3, r1, .LBB158_7
.LBB158_9:
	add r4, r3, r16
	ldbu r19, r4+0
	addi r4, r19, -9
	bgtu r4, r18, .LBB158_25
.LBB158_10:
	slli r4, r4, 2
	add r4, r21, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB158_11:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	jal r0, .LBB158_8
.LBB158_12:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	ldbu r1, r12+%lo(fio_l_after)
	bne r1, r20, .LBB158_5
.LBB158_13:
	stb r12+%lo(fio_l_after), r0
	jal r0, .LBB158_8
.LBB158_14:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	addi r13, r0, 126
	lui r11, %hi(fio_ltok)
	addi r11, r11, %lo(fio_ltok)
	add r14, r0, r0
	jal r0, .LBB158_16
.LBB158_15:
	jal r31, fio_getrec
	bne r1, r0, .LBB158_44
.LBB158_16:
	ldw r1, r15+%lo(fio_rpos)
	ldw r4, r17+%lo(fio_rlen)
	bge r1, r4, .LBB158_15
.LBB158_17:
	addi r5, r1, 1
	stw r15+%lo(fio_rpos), r5
	add r3, r1, r16
	ldbu r3, r3+0
	bne r3, r19, .LBB158_21
.LBB158_18:
	bge r5, r4, .LBB158_23
.LBB158_19:
	add r4, r5, r16
	ldbu r4, r4+0
	bne r4, r19, .LBB158_23
.LBB158_20:
	addi r1, r1, 2
	stw r15+%lo(fio_rpos), r1
.LBB158_21:
	bgtu r14, r13, .LBB158_16
.LBB158_22:
	addi r1, r14, 1
	add r4, r14, r11
	stb r4+0, r3
	add r14, r1, r0
	jal r0, .LBB158_16
.LBB158_23:
	add r1, r14, r11
	stb r1+0, r0
	addi r1, r0, 1
	stb r12+%lo(fio_l_after), r1
	jal r0, .LBB158_4
.LBB158_24:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	add r11, r20, r0
	jal r0, .LBB158_5
.LBB158_25:
	ldw r3, r15+%lo(fio_rpos)
	bge r3, r1, .LBB158_32
.LBB158_26:
	addi r5, r0, 126
	lui r6, %hi(fio_ltok)
	addi r6, r6, %lo(fio_ltok)
	lui r7, %hi(.LJTI158_1)
	addi r7, r7, %lo(.LJTI158_1)
	add r4, r0, r0
	jal r0, .LBB158_28
.LBB158_27:
	addi r3, r3, 1
	stw r15+%lo(fio_rpos), r3
	beq r1, r3, .LBB158_33
.LBB158_28:
	add r8, r3, r16
	ldbu r8, r8+0
	addi r9, r8, -9
	bgtu r9, r18, .LBB158_30
.LBB158_29:
	slli r9, r9, 2
	add r9, r7, r9
	ldw r9, r9+0
	jalr r0, r9, 0
.LBB158_30:
	bgt r4, r5, .LBB158_27
.LBB158_31:
	addi r9, r4, 1
	add r4, r4, r6
	stb r4+0, r8
	add r4, r9, r0
	jal r0, .LBB158_27
.LBB158_32:
	add r4, r0, r0
.LBB158_33:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	add r3, r4, r1
	stb r3+0, r0
	addi r3, r0, 1
	stb r12+%lo(fio_l_after), r3
	addi r4, r0, -11
	add r15, r0, r0
.LBB158_34:
	add r5, r15, r1
	ldb r5, r5+0
	addi r6, r5, -58
	addi r15, r15, 1
	bgtu r6, r4, .LBB158_34
.LBB158_35:
	beq r15, r3, .LBB158_4
.LBB158_36:
	addi r1, r0, 42
	bne r5, r1, .LBB158_4
.LBB158_37:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	addi r5, r0, 10
	add r3, r12, r0
	add r4, r0, r0
	jal r31, strtol
	beq r1, r0, .LBB158_46
.LBB158_38:
	add r12, r15, r12
	ldbu r15, r12+0
	beq r15, r0, .LBB158_42
.LBB158_39:
	addi r3, r0, 2
	blt r1, r3, .LBB158_41
.LBB158_40:
	addi r1, r1, -1
	stw r14+%lo(fio_l_pending), r1
.LBB158_41:
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	add r4, r12, r0
	jal r31, memmove
	jal r0, .LBB158_43
.LBB158_42:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
.LBB158_43:
	bne r15, r0, .LBB158_4
	jal r0, .LBB158_5
.LBB158_44:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
.LBB158_45:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB158_46:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.133)
	addi r4, r4, %lo(.L.str.133)
	jal r0, .LBB158_45
.Lfunc_end158:
	.size	fio_list_tok, .Lfunc_end158-fio_list_tok
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI158_0,@object
.LJTI158_0:
	.word	.LBB158_11
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_11
	.word	.LBB158_25
	.word	.LBB158_14
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_14
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_12
	.word	.LBB158_25
	.word	.LBB158_25
	.word	.LBB158_24
	.size	.LJTI158_0, 156
	.type	.LJTI158_1,@object
.LJTI158_1:
	.word	.LBB158_33
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_33
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_33
	.word	.LBB158_30
	.word	.LBB158_30
	.word	.LBB158_33
	.size	.LJTI158_1, 156
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function fio_bad_num
	.type	fio_bad_num,@function
fio_bad_num:                            # @fio_bad_num
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	add r5, r3, r0
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.134)
	addi r4, r4, %lo(.L.str.134)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end159:
	.size	fio_bad_num, .Lfunc_end159-fio_bad_num
                                        # -- End function
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
	beq r1, r0, .LBB160_5
.LBB160_1:
	lui r13, %hi(fio_ldone)
	ldbu r1, r13+%lo(fio_ldone)
	bne r1, r0, .LBB160_36
.LBB160_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB160_36
.LBB160_3:
	addi r14, r0, 1
	bne r1, r14, .LBB160_17
.LBB160_4:
	stb r13+%lo(fio_ldone), r14
	jal r0, .LBB160_36
.LBB160_5:
	lui r13, %hi(fio_eor)
	ldbu r3, r13+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB160_8
.LBB160_6:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB160_22
.LBB160_7:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB160_22
.LBB160_8:
	beq r1, r0, .LBB160_36
.LBB160_9:
	addi r3, r0, 65
	bne r1, r3, .LBB160_37
.LBB160_10:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r4, r0, 0
	sgt r3, r1, r4
	xor r1, r1, r12
	sub r3, r0, r3
	and r1, r1, r3
	xor r1, r12, r1
	lui r3, %hi(fio_rpos)
	addi r5, r0, 1
	bge r1, r12, .LBB160_23
.LBB160_11:
	blt r12, r5, .LBB160_28
.LBB160_12:
	ldw r5, r3+%lo(fio_rpos)
	lui r6, %hi(fio_rlen)
	ldw r6, r6+%lo(fio_rlen)
	addi r7, r0, 32
	lui r8, %hi(fio_rec)
	addi r8, r8, %lo(fio_rec)
	add r9, r0, r0
	jal r0, .LBB160_14
.LBB160_13:
	add lr, r11, r9
	stb lr+0, r10
	addi r9, r9, 1
	beq r12, r9, .LBB160_28
.LBB160_14:
	add r10, r7, r0
	bge r9, r1, .LBB160_13
.LBB160_15:
	add lr, r5, r9
	add r10, r7, r0
	bge lr, r6, .LBB160_13
.LBB160_16:
	add r10, lr, r8
	ldbu r10, r10+0
	jal r0, .LBB160_13
.LBB160_17:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB160_36
.LBB160_18:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB160_20
.LBB160_19:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB160_36
.LBB160_20:
	add r5, r3, r0
	bge r4, r1, .LBB160_19
.LBB160_21:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB160_19
.LBB160_22:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	bne r1, r0, .LBB160_9
	jal r0, .LBB160_36
.LBB160_23:
	blt r12, r5, .LBB160_28
.LBB160_24:
	ldw r6, r3+%lo(fio_rpos)
	lui r5, %hi(fio_rlen)
	ldw r5, r5+%lo(fio_rlen)
	sub r9, r0, r12
	add r6, r1, r6
	addi r7, r0, 32
	lui r8, %hi(fio_rec)
	addi r8, r8, %lo(fio_rec)
	jal r0, .LBB160_26
.LBB160_25:
	stb r11+0, r10
	addi r10, r9, 1
	sltu lr, r10, r9
	addi r11, r11, 1
	add r9, r10, r0
	bne lr, r0, .LBB160_28
.LBB160_26:
	add lr, r6, r9
	add r10, r7, r0
	bge lr, r5, .LBB160_25
.LBB160_27:
	add r10, lr, r8
	ldbu r10, r10+0
	jal r0, .LBB160_25
.LBB160_28:
	lui r5, %hi(fio_noadv)
	ldbu r6, r5+%lo(fio_noadv)
	addi r5, r0, 1
	bne r6, r5, .LBB160_32
.LBB160_29:
	lui r6, %hi(fio_reading)
	ldbu r6, r6+%lo(fio_reading)
	andi r6, r6, 1
	beq r6, r0, .LBB160_32
.LBB160_30:
	lui r6, %hi(fio_rlen)
	ldw r6, r6+%lo(fio_rlen)
	ldw r7, r3+%lo(fio_rpos)
	sub r6, r6, r7
	sgt r4, r6, r4
	sub r4, r0, r4
	and r4, r6, r4
	xor r6, r1, r4
	slt r7, r1, r4
	sub r7, r0, r7
	and r6, r6, r7
	xor r6, r4, r6
	lui r7, %hi(fio_size)
	ldw r8, r7+%lo(fio_size)
	add r6, r6, r8
	stw r7+%lo(fio_size), r6
	ble r1, r4, .LBB160_32
.LBB160_31:
	stb r13+%lo(fio_eor), r5
.LBB160_32:
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	blt r3, r5, .LBB160_34
.LBB160_33:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB160_34:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB160_36
.LBB160_35:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB160_36:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB160_37:
	lui r3, %hi(stderr)
	ldw r3, r3+%lo(stderr)
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end160:
	.size	f77_rd_s, .Lfunc_end160-f77_rd_s
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_in_real
	.type	.LCPI161_0,@object
.LCPI161_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI161_0, 8
	.type	.LCPI161_1,@object
.LCPI161_1:
	.quad	0x7ff0000000000000              # double +Inf
	.size	.LCPI161_1, 8
	.type	.LCPI161_2,@object
.LCPI161_2:
	.quad	0xfff0000000000000              # double -Inf
	.size	.LCPI161_2, 8
	.text
	.p2align	2
	.type	fio_in_real,@function
fio_in_real:                            # @fio_in_real
# %bb.0:
	addi sp, sp, -2048
	addi sp, sp, -1104
	stw sp+0, lr
	addi r1, sp, 2047
	stw r1+1101, r11
	stw r1+1097, r12
	stw r1+1093, r13
	stw r1+1089, r14
	stw r1+1085, r15
	stw r1+1081, r16
	stw r1+1077, r17
	stw r1+1073, r18
	stw r1+1069, r19
	stw r1+1065, r20
	stw r1+1061, r21
	add r11, r4, r0
	add r4, r3, r0
	addi r3, r1, 37
	jal r31, fio_field_in
	addi r1, sp, 2047
	ldbu r3, r1+37
	addi r12, sp, 20
	lui r1, %hi(.LCPI161_0)
	addi r1, r1, %lo(.LCPI161_0)
	ldw r15, r1+4
	ldw r14, r1+0
	beq r3, r0, .LBB161_8
.LBB161_1:
	addi r4, sp, 2047
	addi r1, r4, 37
	addi r13, r0, 45
	beq r3, r13, .LBB161_9
.LBB161_2:
	lui r4, %hi(.LCPI161_1)
	addi r4, r4, %lo(.LCPI161_1)
	ldw r17, r4+4
	ldw r16, r4+0
	addi r4, r0, 43
	bne r3, r4, .LBB161_4
.LBB161_3:
	addi r3, sp, 2047
	addi r1, r3, 37
	addi r1, r1, 1
.LBB161_4:
	ldbu r3, r1+0
	beq r3, r0, .LBB161_10
.LBB161_5:
	addi r18, r1, 1
	addi r19, r0, 14
	add r1, r0, r0
.LBB161_6:
	add r20, r1, r0
	andi r3, r3, 255
	jal r31, toupper
	addi r3, sp, 1060
	add r3, r3, r20
	stb r3+0, r1
	addi r1, r20, 1
	add r3, r18, r20
	ldbu r3, r3+0
	beq r3, r0, .LBB161_11
.LBB161_7:
	bltu r20, r19, .LBB161_6
	jal r0, .LBB161_11
.LBB161_8:
	add r16, r14, r0
	add r17, r15, r0
	jal r0, .LBB161_71
.LBB161_9:
	addi r3, sp, 2047
	addi r1, r3, 37
	addi r1, r1, 1
	lui r3, %hi(.LCPI161_2)
	addi r3, r3, %lo(.LCPI161_2)
	ldw r17, r3+4
	ldw r16, r3+0
	ldbu r3, r1+0
	bne r3, r0, .LBB161_5
.LBB161_10:
	add r1, r0, r0
.LBB161_11:
	addi r3, sp, 1060
	add r1, r3, r1
	stb r1+0, r0
	lui r4, %hi(.L.str.138)
	addi r4, r4, %lo(.L.str.138)
	jal r31, strcmp
	beq r1, r0, .LBB161_18
.LBB161_12:
	lui r4, %hi(.L.str.139)
	addi r4, r4, %lo(.L.str.139)
	addi r3, sp, 1060
	jal r31, strcmp
	beq r1, r0, .LBB161_18
.LBB161_13:
	lui r4, %hi(.L.str.140)
	addi r4, r4, %lo(.L.str.140)
	addi r3, sp, 1060
	addi r5, r0, 3
	jal r31, strncmp
	addi r3, r0, 1
	beq r1, r0, .LBB161_15
.LBB161_14:
                                        # implicit-def: $r16_r17
	jal r0, .LBB161_19
.LBB161_15:
	ldbu r1, sp+1063
	addi r4, r0, 40
	beq r1, r4, .LBB161_17
.LBB161_16:
                                        # implicit-def: $r16_r17
	bne r1, r0, .LBB161_19
.LBB161_17:
	lui r3, %hi(.L.str.8)
	addi r3, r3, %lo(.L.str.8)
	jal r31, nan
	add r16, r1, r0
	add r17, r2, r0
.LBB161_18:
	add r3, r0, r0
.LBB161_19:
	beq r3, r0, .LBB161_71
.LBB161_20:
	addi r3, sp, 2047
	ldbu r1, r3+37
	addi r4, r0, 1
	addi r6, r0, 43
	beq r1, r6, .LBB161_23
.LBB161_21:
	bne r1, r13, .LBB161_24
.LBB161_22:
	addi r5, r0, 1
	add r3, r0, r0
	jal r0, .LBB161_26
.LBB161_23:
	add r5, r4, r0
	jal r0, .LBB161_25
.LBB161_24:
	add r5, r0, r0
.LBB161_25:
	add r3, r4, r0
.LBB161_26:
	addi r7, sp, 2047
	addi r1, r7, 37
	or  r1, r1, r5
	ldbu r20, r1+0
	beq r20, r0, .LBB161_45
.LBB161_27:
	addi r7, r0, 1
	addi r8, r0, 46
	addi r10, r0, -10
	addi lr, r0, 70
	lui r16, %hi(.LJTI161_0)
	addi r16, r16, %lo(.LJTI161_0)
	addi r17, r0, -1
	add r4, r7, r0
	add r9, r0, r0
	add r18, r0, r0
	add r1, r0, r0
.LBB161_28:
	andi r19, r20, 255
	bne r19, r8, .LBB161_30
.LBB161_29:
	add r18, r7, r0
	add r19, r7, r0
	jal r0, .LBB161_42
.LBB161_30:
	slli r21, r20, 24
	srai r21, r21, 24
	addi r21, r21, -58
	bgeu r21, r10, .LBB161_37
.LBB161_31:
	addi r19, r19, -43
	bgtu r19, lr, .LBB161_72
.LBB161_32:
	slli r19, r19, 2
	add r19, r16, r19
	ldw r20, r19+0
	add r19, r0, r0
	jalr r0, r20, 0
.LBB161_33:
	addi r19, r5, 1
	addi r21, sp, 2047
	addi r20, r21, 37
	add r20, r20, r19
	ldbu r20, r20+0
	beq r20, r6, .LBB161_40
.LBB161_34:
	bne r20, r13, .LBB161_41
.LBB161_35:
	add r5, r19, r0
.LBB161_36:
	add r19, r0, r0
	add r4, r17, r0
	jal r0, .LBB161_42
.LBB161_37:
	addi r19, r0, 1022
	bgt r1, r19, .LBB161_39
.LBB161_38:
	addi r19, r1, 1
	addi r21, sp, 1060
	add r1, r21, r1
	stb r1+0, r20
	add r1, r19, r0
.LBB161_39:
	addi r19, r0, 0
	sne r19, r18, r19
	add r9, r9, r19
	add r19, r7, r0
	jal r0, .LBB161_42
.LBB161_40:
	add r5, r19, r0
.LBB161_41:
	add r19, r0, r0
.LBB161_42:
	addi r5, r5, 1
	addi r21, sp, 2047
	addi r20, r21, 37
	add r20, r20, r5
	ldbu r20, r20+0
	beq r20, r0, .LBB161_44
.LBB161_43:
	bne r19, r0, .LBB161_28
.LBB161_44:
	addi r6, r0, 0
	seq r6, r18, r6
	xor r7, r11, r9
	sub r6, r0, r6
	and r6, r7, r6
	xor r11, r9, r6
	beq r19, r0, .LBB161_46
	jal r0, .LBB161_51
.LBB161_45:
	add r1, r0, r0
	add r19, r4, r0
	bne r19, r0, .LBB161_51
.LBB161_46:
	addi r7, sp, 2047
	addi r6, r7, 37
	add r6, r6, r5
	ldbu r6, r6+0
	beq r6, r0, .LBB161_51
.LBB161_47:
	addi r8, sp, 2047
	addi r7, r8, 37
	add r5, r5, r7
	addi r5, r5, 1
	addi r7, r0, 10
	add r8, r0, r0
.LBB161_48:
	slli r6, r6, 24
	srai r6, r6, 24
	addi r6, r6, -48
	bgeu r6, r7, .LBB161_72
.LBB161_49:
	mul r8, r8, r7
	add r8, r6, r8
	ldbu r6, r5+0
	addi r5, r5, 1
	bne r6, r0, .LBB161_48
.LBB161_50:
	mul r6, r8, r4
	jal r0, .LBB161_52
.LBB161_51:
	add r6, r0, r0
.LBB161_52:
	addi r4, sp, 1060
	add r4, r4, r1
	stb r4+0, r0
	add r16, r14, r0
	add r17, r15, r0
	beq r1, r0, .LBB161_71
.LBB161_53:
	sub r5, r1, r11
	lui r4, %hi(fio_scale)
	ldw r4, r4+%lo(fio_scale)
	sub r7, r0, r19
	and r7, r4, r7
	addi r8, sp, 20
	add r4, r0, r0
	bne r3, r0, .LBB161_55
.LBB161_54:
	addi r8, r12, 1
	stb sp+20, r13
	addi r4, r0, 1
.LBB161_55:
	sub r6, r6, r7
	addi r3, r0, 1
	blt r5, r3, .LBB161_60
.LBB161_56:
	blt r1, r3, .LBB161_66
.LBB161_57:
	xor r3, r5, r1
	slt r5, r5, r1
	sub r5, r0, r5
	and r3, r3, r5
	xor r5, r1, r3
	addi r3, sp, 20
	or  r7, r3, r4
	add r3, r0, r0
.LBB161_58:
	addi r8, sp, 1060
	add r8, r8, r3
	ldbu r8, r8+0
	add r9, r7, r3
	stb r9+0, r8
	addi r3, r3, 1
	bne r5, r3, .LBB161_58
.LBB161_59:
	add r4, r4, r3
	jal r0, .LBB161_67
.LBB161_60:
	addi r7, sp, 20
	or  r9, r7, r4
	addi r7, r0, 48
	stb r9+0, r7
	addi r4, r4, 2
	addi r9, r0, 46
	stb r8+1, r9
	addi r8, r0, -1
	bgt r5, r8, .LBB161_63
.LBB161_61:
	sub r5, r11, r1
.LBB161_62:
	add r8, r4, r0
	addi r4, r4, 1
	addi r9, sp, 20
	add r8, r9, r8
	stb r8+0, r7
	addi r5, r5, -1
	bne r5, r0, .LBB161_62
.LBB161_63:
	blt r1, r3, .LBB161_70
.LBB161_64:
	addi r3, sp, 1060
.LBB161_65:
	add r5, r4, r0
	ldbu r7, r3+0
	addi r4, r4, 1
	addi r8, sp, 20
	add r5, r8, r5
	stb r5+0, r7
	addi r1, r1, -1
	addi r3, r3, 1
	bne r1, r0, .LBB161_65
	jal r0, .LBB161_70
.LBB161_66:
	add r3, r0, r0
.LBB161_67:
	addi r5, sp, 20
	add r5, r5, r4
	addi r7, r0, 46
	stb r5+0, r7
	addi r4, r4, 1
	ble r1, r3, .LBB161_70
.LBB161_68:
	sub r1, r1, r3
	addi r5, sp, 1060
	add r3, r5, r3
.LBB161_69:
	ldbu r5, r3+0
	addi r7, sp, 20
	add r7, r7, r4
	stb r7+0, r5
	addi r4, r4, 1
	addi r1, r1, -1
	addi r3, r3, 1
	bne r1, r0, .LBB161_69
.LBB161_70:
	addi r11, sp, 20
	add r3, r11, r4
	addi r1, r0, 1040
	sub r4, r1, r4
	lui r5, %hi(.L.str.137)
	addi r5, r5, %lo(.L.str.137)
	jal r31, snprintf
	add r3, r11, r0
	add r4, r0, r0
	jal r31, strtod
	add r16, r1, r0
	add r17, r2, r0
.LBB161_71:
	add r1, r16, r0
	add r2, r17, r0
	addi r3, sp, 2047
	ldw r21, r3+1061
	ldw r20, r3+1065
	ldw r19, r3+1069
	ldw r18, r3+1073
	ldw r17, r3+1077
	ldw r16, r3+1081
	ldw r15, r3+1085
	ldw r14, r3+1089
	ldw r13, r3+1093
	ldw r12, r3+1097
	ldw r11, r3+1101
	ldw lr, sp+0
	addi sp, sp, 2047
	addi sp, sp, 1105
	jalr r0, r31, 0
.LBB161_72:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.134)
	addi r4, r4, %lo(.L.str.134)
	addi r1, sp, 2047
	addi r5, r1, 37
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end161:
	.size	fio_in_real, .Lfunc_end161-fio_in_real
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI161_0,@object
.LJTI161_0:
	.word	.LBB161_42
	.word	.LBB161_72
	.word	.LBB161_36
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_33
	.word	.LBB161_33
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_33
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_33
	.word	.LBB161_33
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_72
	.word	.LBB161_33
	.size	.LJTI161_0, 284
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
	bne r1, r0, .LBB162_7
.LBB162_1:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB162_4
.LBB162_2:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB162_6
.LBB162_3:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB162_6
.LBB162_4:
	addi r3, r0, 65
	bne r1, r3, .LBB162_7
.LBB162_5:
	add r3, r11, r0
	add r4, r12, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_rd_s
.LBB162_6:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 65
	beq r1, r3, .LBB162_5
.LBB162_7:
	addi r13, r0, 2
	bne r12, r13, .LBB162_9
.LBB162_8:
	ldh r1, r11+0
	jal r0, .LBB162_10
.LBB162_9:
	ldbu r1, r11+0
.LBB162_10:
	stw sp+16, r1
	addi r3, sp, 16
	jal r31, f77_rd_i
	ldw r1, sp+16
	bne r12, r13, .LBB162_12
.LBB162_11:
	sth r11+0, r1
	jal r0, .LBB162_13
.LBB162_12:
	stb r11+0, r1
.LBB162_13:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end162:
	.size	f77_rd_in, .Lfunc_end162-f77_rd_in
                                        # -- End function
	.globl	f77_wr_in                       # -- Begin function f77_wr_in
	.p2align	2
	.type	f77_wr_in,@function
f77_wr_in:                              # @f77_wr_in
# %bb.0:
	addi sp, sp, -64
	stw sp+0, lr
	stw sp+60, r11
	stw sp+56, r12
	stw sp+52, r13
	add r12, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	addi r11, r0, 2
	bne r4, r11, .LBB163_12
.LBB163_1:
	beq r1, r0, .LBB163_12
.LBB163_2:
	lui r5, %hi(.L.str.119)
	addi r5, r5, %lo(.L.str.119)
	addi r3, sp, 20
	addi r11, r0, 32
	addi r6, r0, 7
	add r4, r11, r0
	add r7, r12, r0
	jal r31, snprintf
	ldbu r9, sp+20
	beq r9, r0, .LBB163_19
.LBB163_3:
	lui r1, %hi(fio_len)
	ldw r7, r1+%lo(fio_len)
	lui r3, %hi(fio_col)
	ldw r8, r3+%lo(fio_col)
	addi r4, sp, 20
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	jal r0, .LBB163_6
.LBB163_4:
	add r8, r9, r0
.LBB163_5:
	addi r4, r4, 1
	ldbu r9, r4+0
	beq r9, r0, .LBB163_19
.LBB163_6:
	bgt r8, r5, .LBB163_5
.LBB163_7:
	bge r7, r8, .LBB163_10
.LBB163_8:
	addi r10, r7, 1
	add r7, r7, r6
	stb r7+0, r11
	add r7, r10, r0
	bne r8, r10, .LBB163_8
.LBB163_9:
	stw r1+%lo(fio_len), r8
	add r7, r8, r0
.LBB163_10:
	add r10, r8, r6
	stb r10+0, r9
	addi r9, r8, 1
	stw r3+%lo(fio_col), r9
	blt r8, r7, .LBB163_4
.LBB163_11:
	stw r1+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB163_4
.LBB163_12:
	bne r1, r0, .LBB163_22
.LBB163_13:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB163_16
.LBB163_14:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB163_21
.LBB163_15:
	lui r5, %hi(fio_rep)
	ldw r5, r5+%lo(fio_rep)
	ble r5, r0, .LBB163_21
.LBB163_16:
	addi r3, r0, 65
	bne r1, r3, .LBB163_22
.LBB163_17:
	sth sp+20, r12
	stb sp+16, r12
	bne r4, r11, .LBB163_23
.LBB163_18:
	addi r3, sp, 20
	addi r4, r0, 2
	jal r0, .LBB163_24
.LBB163_19:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
.LBB163_20:
	ldw r13, sp+52
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.LBB163_21:
	stw r3+%lo(fio_desc), r0
	add r13, r4, r0
	jal r31, fio_next_desc
	add r4, r13, r0
	addi r3, r0, 65
	beq r1, r3, .LBB163_17
.LBB163_22:
	add r3, r12, r0
	ldw r13, sp+52
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jal r0, f77_wr_i
.LBB163_23:
	addi r3, sp, 16
	addi r4, r0, 1
.LBB163_24:
	jal r31, fio_hol_out
	jal r0, .LBB163_20
.Lfunc_end163:
	.size	f77_wr_in, .Lfunc_end163-f77_wr_in
                                        # -- End function
	.globl	f77_rd_d                        # -- Begin function f77_rd_d
	.p2align	2
	.type	f77_rd_d,@function
f77_rd_d:                               # @f77_rd_d
# %bb.0:
	addi sp, sp, -1056
	stw sp+0, lr
	stw sp+1052, r11
	stw sp+1048, r12
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB164_5
.LBB164_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB164_27
.LBB164_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB164_27
.LBB164_3:
	addi r3, r0, 1
	bne r1, r3, .LBB164_12
.LBB164_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB164_27
.LBB164_5:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB164_8
.LBB164_6:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB164_13
.LBB164_7:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB164_13
.LBB164_8:
	addi r3, r0, 72
	bgt r1, r3, .LBB164_14
.LBB164_9:
	beq r1, r0, .LBB164_27
.LBB164_10:
	addi r3, r0, 65
	bne r1, r3, .LBB164_20
.LBB164_11:
	addi r4, r0, 8
	add r3, r11, r0
	ldw r12, sp+1048
	ldw r11, sp+1052
	ldw lr, sp+0
	addi sp, sp, 1056
	jal r0, f77_rd_s
.LBB164_12:
	jal r31, fio_list_real
	stw r11+4, r2
	stw r11+0, r1
	jal r0, .LBB164_27
.LBB164_13:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB164_9
.LBB164_14:
	addi r3, r0, 73
	bne r1, r3, .LBB164_19
.LBB164_15:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 24
	jal r31, fio_field_in
	ldbu r1, sp+24
	beq r1, r0, .LBB164_21
.LBB164_16:
	addi r12, sp, 24
	addi r4, sp, 20
	addi r5, r0, 10
	add r3, r12, r0
	jal r31, strtol
	ldw r3, sp+20
	beq r3, r12, .LBB164_18
.LBB164_17:
	ldbu r3, r3+0
	beq r3, r0, .LBB164_22
.LBB164_18:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.134)
	addi r4, r4, %lo(.L.str.134)
	addi r5, sp, 24
	jal r0, .LBB164_29
.LBB164_19:
	addi r3, r0, 76
	beq r1, r3, .LBB164_28
.LBB164_20:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	jal r0, .LBB164_23
.LBB164_21:
	add r1, r0, r0
.LBB164_22:
	fcvt.d.w r4, r1
.LBB164_23:
	stw r11+4, r5
	stw r11+0, r4
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	blt r3, r4, .LBB164_25
.LBB164_24:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB164_25:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB164_27
.LBB164_26:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB164_27:
	ldw r12, sp+1048
	ldw r11, sp+1052
	ldw lr, sp+0
	addi sp, sp, 1056
	jalr r0, r31, 0
.LBB164_28:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
	addi r5, r0, 76
.LBB164_29:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end164:
	.size	f77_rd_d, .Lfunc_end164-f77_rd_d
                                        # -- End function
	.p2align	2                               # -- Begin function fio_list_real
	.type	fio_list_real,@function
fio_list_real:                          # @fio_list_real
# %bb.0:
	addi sp, sp, -160
	stw sp+0, lr
	stw sp+156, r11
	stw sp+152, r12
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	addi r3, r0, 9
	addi r4, r0, 32
	addi r5, r0, 68
	addi r6, r0, 81
	add r7, r0, r0
	jal r0, .LBB165_2
.LBB165_1:
	addi r1, r1, 1
.LBB165_2:
	ldbu r8, r1+0
	beq r8, r3, .LBB165_1
.LBB165_3:
	beq r8, r4, .LBB165_1
.LBB165_4:
	beq r8, r0, .LBB165_6
.LBB165_5:
	andi r9, r8, 223
	seq r10, r9, r5
	seq r9, r9, r6
	sub r10, r0, r10
	sub r9, r0, r9
	xori lr, r8, 69
	and r9, lr, r9
	xor r8, r8, r9
	xori r9, r8, 69
	and r9, r9, r10
	xor r8, r8, r9
	addi r9, r7, 1
	addi r10, sp, 24
	add r7, r10, r7
	stb r7+0, r8
	add r7, r9, r0
	jal r0, .LBB165_1
.LBB165_6:
	addi r12, sp, 24
	add r1, r12, r7
	stb r1+0, r0
	addi r4, sp, 20
	add r3, r12, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	ldw r11, sp+20
	beq r11, r12, .LBB165_11
.LBB165_7:
	ldbu r1, r11+0
	addi r3, r0, 45
	beq r1, r3, .LBB165_9
.LBB165_8:
	addi r3, r0, 43
	bne r1, r3, .LBB165_11
.LBB165_9:
	ldb r1, r11+1
	addi r1, r1, -58
	addi r3, r0, -10
	bltu r1, r3, .LBB165_11
.LBB165_10:
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	ldw r1, sp+20
	addi r3, r0, 69
	stb r1+0, r3
	addi r3, sp, 24
	addi r4, sp, 20
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
.LBB165_11:
	ldw r1, sp+20
	addi r3, sp, 24
	beq r1, r3, .LBB165_14
.LBB165_12:
	ldbu r1, r1+0
	bne r1, r0, .LBB165_14
.LBB165_13:
	add r1, r4, r0
	add r2, r5, r0
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jalr r0, r31, 0
.LBB165_14:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.134)
	addi r4, r4, %lo(.L.str.134)
	lui r5, %hi(fio_ltok)
	addi r5, r5, %lo(fio_ltok)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end165:
	.size	fio_list_real, .Lfunc_end165-fio_list_real
                                        # -- End function
	.globl	f77_rd_r                        # -- Begin function f77_rd_r
	.p2align	2
	.type	f77_rd_r,@function
f77_rd_r:                               # @f77_rd_r
# %bb.0:
	addi sp, sp, -1056
	stw sp+0, lr
	stw sp+1052, r11
	stw sp+1048, r12
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB166_5
.LBB166_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB166_27
.LBB166_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB166_27
.LBB166_3:
	addi r3, r0, 1
	bne r1, r3, .LBB166_12
.LBB166_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB166_27
.LBB166_5:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB166_8
.LBB166_6:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB166_13
.LBB166_7:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB166_13
.LBB166_8:
	addi r3, r0, 72
	bgt r1, r3, .LBB166_14
.LBB166_9:
	beq r1, r0, .LBB166_27
.LBB166_10:
	addi r3, r0, 65
	bne r1, r3, .LBB166_20
.LBB166_11:
	addi r4, r0, 4
	add r3, r11, r0
	ldw r12, sp+1048
	ldw r11, sp+1052
	ldw lr, sp+0
	addi sp, sp, 1056
	jal r0, f77_rd_s
.LBB166_12:
	jal r31, fio_list_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
	jal r0, .LBB166_27
.LBB166_13:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB166_9
.LBB166_14:
	addi r3, r0, 73
	bne r1, r3, .LBB166_19
.LBB166_15:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 24
	jal r31, fio_field_in
	ldbu r1, sp+24
	beq r1, r0, .LBB166_21
.LBB166_16:
	addi r12, sp, 24
	addi r4, sp, 20
	addi r5, r0, 10
	add r3, r12, r0
	jal r31, strtol
	ldw r3, sp+20
	beq r3, r12, .LBB166_18
.LBB166_17:
	ldbu r3, r3+0
	beq r3, r0, .LBB166_22
.LBB166_18:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.134)
	addi r4, r4, %lo(.L.str.134)
	addi r5, sp, 24
	jal r0, .LBB166_29
.LBB166_19:
	addi r3, r0, 76
	beq r1, r3, .LBB166_28
.LBB166_20:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	jal r0, .LBB166_23
.LBB166_21:
	add r1, r0, r0
.LBB166_22:
	fcvt.s.w r1, r1
.LBB166_23:
	stw r11+0, r1
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	blt r3, r4, .LBB166_25
.LBB166_24:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB166_25:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB166_27
.LBB166_26:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB166_27:
	ldw r12, sp+1048
	ldw r11, sp+1052
	ldw lr, sp+0
	addi sp, sp, 1056
	jalr r0, r31, 0
.LBB166_28:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
	addi r5, r0, 76
.LBB166_29:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end166:
	.size	f77_rd_r, .Lfunc_end166-f77_rd_r
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
	beq r1, r0, .LBB167_5
.LBB167_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB167_7
.LBB167_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB167_7
.LBB167_3:
	addi r3, r0, 1
	bne r1, r3, .LBB167_6
.LBB167_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB167_7
.LBB167_5:
	add r3, r11, r0
	jal r31, f77_rd_r
	addi r3, r11, 4
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_rd_r
.LBB167_6:
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
.LBB167_7:
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end167:
	.size	f77_rd_c, .Lfunc_end167-f77_rd_c
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_list_cplx_in
	.type	.LCPI168_0,@object
.LCPI168_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI168_0, 8
	.text
	.p2align	2
	.type	fio_list_cplx_in,@function
fio_list_cplx_in:                       # @fio_list_cplx_in
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
	stw sp+164, r21
	stw sp+160, r22
	stw sp+156, r23
	add r11, r4, r0
	add r12, r3, r0
	lui r1, %hi(fio_l_rep)
	ldbu r1, r1+%lo(fio_l_rep)
	lui r16, %hi(fio_l_re+4)
	lui r17, %hi(fio_l_re)
	lui r14, %hi(fio_l_im+4)
	lui r15, %hi(fio_l_im)
	bne r1, r0, .LBB168_21
.LBB168_1:
	lui r1, %hi(fio_ltok)
	ldbu r1, r1+%lo(fio_ltok)
	addi r3, r0, 40
	bne r1, r3, .LBB168_7
.LBB168_2:
	lui r1, %hi(fio_ltok+1)
	ldbu r4, r1+%lo(fio_ltok+1)
	beq r4, r0, .LBB168_8
.LBB168_3:
	lui r1, %hi(fio_ltok+2)
	addi r1, r1, %lo(fio_ltok+2)
	addi r3, r0, 126
	add r13, r0, r0
	jal r0, .LBB168_5
.LBB168_4:
	ldbu r4, r1+0
	addi r1, r1, 1
	beq r4, r0, .LBB168_9
.LBB168_5:
	bgt r13, r3, .LBB168_4
.LBB168_6:
	addi r5, r13, 1
	addi r6, sp, 28
	add r6, r6, r13
	stb r6+0, r4
	add r13, r5, r0
	jal r0, .LBB168_4
.LBB168_7:
	jal r31, fio_list_real
	stw r17+%lo(fio_l_re), r1
	stw r16+%lo(fio_l_re+4), r2
	stw r14+%lo(fio_l_im+4), r0
	stw r15+%lo(fio_l_im), r0
	jal r0, .LBB168_21
.LBB168_8:
	add r13, r0, r0
.LBB168_9:
	lui r18, %hi(fio_rpos)
	lui r19, %hi(fio_rlen)
	addi r20, r0, 126
	lui r21, %hi(fio_rec)
	addi r21, r21, %lo(fio_rec)
	addi r22, r0, 41
.LBB168_10:
	addi r1, sp, 28
	add r23, r1, r13
	jal r0, .LBB168_12
.LBB168_11:
	jal r31, fio_getrec
	bne r1, r0, .LBB168_22
.LBB168_12:
	beq r13, r0, .LBB168_14
.LBB168_13:
	ldbu r1, r23+-1
	beq r1, r22, .LBB168_17
.LBB168_14:
	ldw r1, r18+%lo(fio_rpos)
	ldw r3, r19+%lo(fio_rlen)
	bge r1, r3, .LBB168_11
.LBB168_15:
	addi r3, r1, 1
	stw r18+%lo(fio_rpos), r3
	bgt r13, r20, .LBB168_12
.LBB168_16:
	add r1, r1, r21
	ldbu r1, r1+0
	addi r3, r13, 1
	addi r4, sp, 28
	add r4, r4, r13
	stb r4+0, r1
	add r13, r3, r0
	jal r0, .LBB168_10
.LBB168_17:
	stb r23+-1, r0
	addi r3, sp, 28
	addi r4, r0, 44
	jal r31, strchr
	beq r1, r0, .LBB168_19
.LBB168_18:
	stb r1+0, r0
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	addi r4, sp, 28
	add r3, r13, r0
	add r18, r1, r0
	jal r31, strcpy
	jal r31, fio_list_real
	stw r17+%lo(fio_l_re), r1
	stw r16+%lo(fio_l_re+4), r2
	addi r4, r18, 1
	add r3, r13, r0
	jal r31, strcpy
	jal r31, fio_list_real
	add r4, r1, r0
	add r5, r2, r0
	jal r0, .LBB168_20
.LBB168_19:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r4, sp, 28
	jal r31, strcpy
	jal r31, fio_list_real
	stw r17+%lo(fio_l_re), r1
	stw r16+%lo(fio_l_re+4), r2
	lui r1, %hi(.LCPI168_0)
	addi r1, r1, %lo(.LCPI168_0)
	ldw r5, r1+4
	ldw r4, r1+0
.LBB168_20:
	stw r15+%lo(fio_l_im), r4
	stw r14+%lo(fio_l_im+4), r5
	lui r1, %hi(fio_l_after)
	addi r3, r0, 1
	stb r1+%lo(fio_l_after), r3
.LBB168_21:
	ldw r1, r16+%lo(fio_l_re+4)
	ldw r3, r17+%lo(fio_l_re)
	stw r12+4, r1
	stw r12+0, r3
	ldw r1, r14+%lo(fio_l_im+4)
	ldw r3, r15+%lo(fio_l_im)
	stw r11+4, r1
	stw r11+0, r3
	ldw r23, sp+156
	ldw r22, sp+160
	ldw r21, sp+164
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
.LBB168_22:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.100)
	addi r4, r4, %lo(.L.str.100)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end168:
	.size	fio_list_cplx_in, .Lfunc_end168-fio_list_cplx_in
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
	beq r1, r0, .LBB169_5
.LBB169_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB169_7
.LBB169_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB169_7
.LBB169_3:
	addi r3, r0, 1
	bne r1, r3, .LBB169_6
.LBB169_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB169_7
.LBB169_5:
	add r3, r11, r0
	jal r31, f77_rd_d
	addi r3, r11, 8
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_rd_d
.LBB169_6:
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
.LBB169_7:
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end169:
	.size	f77_rd_z, .Lfunc_end169-f77_rd_z
                                        # -- End function
	.globl	f77_rd_end                      # -- Begin function f77_rd_end
	.p2align	2
	.type	f77_rd_end,@function
f77_rd_end:                             # @f77_rd_end
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	lui r11, %hi(fio_noadv)
	ldbu r1, r11+%lo(fio_noadv)
	addi r12, r0, 1
	bne r1, r12, .LBB170_8
.LBB170_1:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB170_5
.LBB170_2:
	lui r1, %hi(fio_unit)
	ldw r1, r1+%lo(fio_unit)
	addi r3, r1, -1
	addi r4, r0, 99
	sltu r3, r3, r4
	sub r3, r0, r3
	and r1, r1, r3
	slli r13, r1, 2
	lui r1, %hi(fio_psave)
	addi r1, r1, %lo(fio_psave)
	add r14, r13, r1
	ldw r1, r14+0
	bne r1, r0, .LBB170_4
.LBB170_3:
	addi r3, r0, 1024
	jal r31, malloc
	stw r14+0, r1
.LBB170_4:
	ldw r3, r14+0
	lui r14, %hi(fio_rlen)
	ldw r5, r14+%lo(fio_rlen)
	lui r4, %hi(fio_rec)
	addi r4, r4, %lo(fio_rec)
	jal r31, memcpy
	ldw r1, r14+%lo(fio_rlen)
	lui r3, %hi(fio_plen)
	addi r3, r3, %lo(fio_plen)
	add r3, r13, r3
	stw r3+0, r1
	lui r1, %hi(fio_rpos)
	ldw r1, r1+%lo(fio_rpos)
	lui r3, %hi(fio_ppos)
	addi r3, r3, %lo(fio_ppos)
	add r3, r13, r3
	stw r3+0, r1
	lui r1, %hi(fio_pend)
	addi r1, r1, %lo(fio_pend)
	add r1, r13, r1
	stw r1+0, r12
.LBB170_5:
	lui r1, %hi(fio_sizep)
	ldw r3, r1+%lo(fio_sizep)
	beq r3, r0, .LBB170_7
.LBB170_6:
	lui r4, %hi(fio_size)
	ldw r4, r4+%lo(fio_size)
	stw r3+0, r4
.LBB170_7:
	stb r11+%lo(fio_noadv), r0
	stw r1+%lo(fio_sizep), r0
.LBB170_8:
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_reading)
	stb r1+%lo(fio_reading), r0
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end170:
	.size	f77_rd_end, .Lfunc_end170-f77_rd_end
                                        # -- End function
	.globl	f77_nml_begin                   # -- Begin function f77_nml_begin
	.p2align	2
	.type	f77_nml_begin,@function
f77_nml_begin:                          # @f77_nml_begin
# %bb.0:
	addi sp, sp, -1184
	stw sp+0, lr
	stw sp+4, fp
	stw sp+1180, r11
	stw sp+1176, r12
	stw sp+1172, r13
	stw sp+1168, r14
	stw sp+1164, r15
	stw sp+1160, r16
	stw sp+1156, r17
	stw sp+1152, r18
	stw sp+1148, r19
	stw sp+1144, r20
	stw sp+1140, r21
	stw sp+1136, r22
	stw sp+1132, r23
	stw sp+1128, r24
	stw sp+1124, r25
	stw sp+1120, r26
	stw sp+1116, r27
	stw sp+1112, r28
	add r11, r6, r0
	lui r23, %hi(nml_unit)
	stw r23+%lo(nml_unit), r3
	lui r1, %hi(nml_reading)
	stw r1+%lo(nml_reading), r6
	lui r1, %hi(nml_flags)
	stw r1+%lo(nml_flags), r7
	lui r19, %hi(nml_status)
	stw r19+%lo(nml_status), r0
	lui r1, %hi(nml_ndim)
	stw r1+%lo(nml_ndim), r0
	lui r1, %hi(nml_err)
	stb r1+%lo(nml_err), r0
	addi r1, r0, 31
	slt r1, r5, r1
	sub r1, r0, r1
	xori r3, r5, 31
	and r1, r3, r1
	xori r13, r1, 31
	addi r24, r0, 1
	lui r14, %hi(nml_grp)
	addi r14, r14, %lo(nml_grp)
	blt r5, r24, .LBB171_3
.LBB171_1:
	add r12, r4, r0
	add r15, r0, r0
.LBB171_2:
	add r1, r12, r15
	ldbu r3, r1+0
	jal r31, toupper
	add r3, r15, r14
	stb r3+0, r1
	addi r15, r15, 1
	bne r13, r15, .LBB171_2
.LBB171_3:
	add r1, r13, r14
	stb r1+0, r0
	beq r11, r0, .LBB171_71
.LBB171_4:
	lui r1, %hi(nml_nasg)
	stw sp+40, r1
	stw r1+%lo(nml_nasg), r0
	lui r1, %hi(nml_nval)
	stw sp+48, r1
	stw r1+%lo(nml_nval), r0
	lui r11, %hi(nml_grp)
	addi r11, r11, %lo(nml_grp)
	add r3, r11, r0
	jal r31, strlen
	add r12, r1, r0
	addi r13, r1, 1
	lui r22, %hi(nml_tlen)
	stw r22+%lo(nml_tlen), r0
	lui r3, %hi(.L.str.8)
	addi r3, r3, %lo(.L.str.8)
	add r4, r0, r0
	jal r31, nml_text_add
	addi r1, sp, 84
	stw sp+36, r13
	add r1, r1, r13
	stw sp+32, r1
	lui r13, %hi(.L.str.141)
	addi r13, r13, %lo(.L.str.141)
	addi r14, r0, 3
	addi r15, r0, 1023
	lui r27, %hi(fio_noadv)
	lui r1, %hi(fio_unit)
	stw sp+72, r1
	addi r1, r0, 99
	stw sp+68, r1
	lui r1, %hi(fio_eor)
	stw sp+80, r1
	lui r20, %hi(fio_sizep)
	lui r1, %hi(fio_size)
	stw sp+76, r1
	lui r1, %hi(fio_psave)
	addi r1, r1, %lo(fio_psave)
	stw sp+64, r1
	addi r1, r0, 1024
	stw sp+44, r1
	lui r1, %hi(fio_rlen)
	stw sp+60, r1
	lui r1, %hi(fio_rec)
	addi r1, r1, %lo(fio_rec)
	stw sp+56, r1
	lui r1, %hi(fio_plen)
	addi r1, r1, %lo(fio_plen)
	stw sp+52, r1
                                        # implicit-def: $r16
	add r28, r0, r0
	add fp, r0, r0
.LBB171_5:
	ldw r3, r23+%lo(nml_unit)
	add r4, r13, r0
	add r5, r14, r0
	add r6, r14, r0
	jal r31, f77_rd_begin
	beq r1, r0, .LBB171_8
.LBB171_6:
	bne r1, r24, .LBB171_20
.LBB171_7:
	add r16, r1, r0
	jal r0, .LBB171_62
.LBB171_8:
	addi r3, sp, 84
	add r4, r15, r0
	jal r31, f77_rd_s
	ldbu r1, r27+%lo(fio_noadv)
	bne r1, r24, .LBB171_16
.LBB171_9:
	ldw r1, sp+80
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB171_13
.LBB171_10:
	ldw r1, sp+72
	ldw r1, r1+%lo(fio_unit)
	addi r3, r1, -1
	ldw r4, sp+68
	sltu r3, r3, r4
	sub r3, r0, r3
	and r1, r1, r3
	slli r17, r1, 2
	ldw r1, sp+64
	add r18, r17, r1
	ldw r1, r18+0
	bne r1, r0, .LBB171_12
.LBB171_11:
	ldw r3, sp+44
	jal r31, malloc
	stw r18+0, r1
.LBB171_12:
	ldw r3, r18+0
	ldw r18, sp+60
	ldw r5, r18+%lo(fio_rlen)
	ldw r4, sp+56
	jal r31, memcpy
	ldw r1, r18+%lo(fio_rlen)
	ldw r3, sp+52
	add r3, r17, r3
	stw r3+0, r1
	lui r1, %hi(fio_rpos)
	ldw r1, r1+%lo(fio_rpos)
	lui r3, %hi(fio_ppos)
	addi r3, r3, %lo(fio_ppos)
	add r3, r17, r3
	stw r3+0, r1
	lui r1, %hi(fio_pend)
	addi r1, r1, %lo(fio_pend)
	add r1, r17, r1
	addi r3, r0, 1
	stw r1+0, r3
.LBB171_13:
	ldw r1, r20+%lo(fio_sizep)
	beq r1, r0, .LBB171_15
.LBB171_14:
	ldw r3, sp+76
	ldw r3, r3+%lo(fio_size)
	stw r1+0, r3
.LBB171_15:
	stb r27+%lo(fio_noadv), r0
	stw r20+%lo(fio_sizep), r0
.LBB171_16:
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_reading)
	stb r1+%lo(fio_reading), r0
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	addi r3, r0, 1022
.LBB171_17:
	addi r1, sp, 84
	add r1, r1, r3
	ldbu r4, r1+0
	addi r1, r0, 32
	bne r4, r1, .LBB171_23
.LBB171_18:
	addi r4, r3, -1
	addi r5, r3, 1
	addi r6, r0, 2
	add r3, r4, r0
	bgtu r5, r6, .LBB171_17
.LBB171_19:
	addi r25, r0, 1
	jal r0, .LBB171_24
.LBB171_20:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB171_22
.LBB171_21:
	addi r1, r0, 2
	stw r19+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.142)
	addi r4, r4, %lo(.L.str.142)
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
.LBB171_22:
	add r16, r0, r0
	add r1, r24, r0
	jal r0, .LBB171_62
.LBB171_23:
	addi r25, r3, 1
.LBB171_24:
	addi r3, r0, 0
	seq r17, fp, r3
	beq r28, r0, .LBB171_26
.LBB171_25:
	addi r28, r0, 1
	jal r0, .LBB171_42
.LBB171_26:
	ldbu r3, sp+84
	andi r4, r3, 253
	addi r3, r0, 36
	seq r4, r4, r3
	sub r26, r0, r4
	and r4, r17, r26
	xor r17, r17, r4
	bge r17, r25, .LBB171_39
.LBB171_27:
	addi r4, sp, 84
	add r4, r4, r17
	ldbu r4, r4+0
	bne r4, r1, .LBB171_30
.LBB171_28:
	addi r17, r17, 1
	bne r25, r17, .LBB171_27
.LBB171_29:
	add r28, r0, r0
	add r17, r25, r0
	jal r0, .LBB171_40
.LBB171_30:
	beq r4, r3, .LBB171_32
.LBB171_31:
	addi r1, r0, 38
	bne r4, r1, .LBB171_39
.LBB171_32:
	addi r28, r0, 1
	blt r12, r28, .LBB171_37
.LBB171_33:
	addi r1, sp, 84
	add r18, r1, r17
	add r21, r0, r0
.LBB171_34:
	add r1, r17, r21
	addi r1, r1, 1
	bgeu r1, r25, .LBB171_39
.LBB171_35:
	add r1, r18, r21
	ldbu r3, r1+1
	jal r31, toupper
	add r3, r21, r11
	ldb r3, r3+0
	bne r1, r3, .LBB171_39
.LBB171_36:
	addi r21, r21, 1
	bne r12, r21, .LBB171_34
.LBB171_37:
	ldw r1, sp+36
	add r21, r1, r17
	bge r21, r25, .LBB171_68
.LBB171_38:
	ldw r1, sp+32
	add r1, r1, r17
	ldbu r18, r1+0
	add r3, r18, r0
	jal r31, isalnum
	beq r1, r0, .LBB171_64
.LBB171_39:
	add r28, r0, r0
.LBB171_40:
	add r3, r0, r0
	add r1, r14, r0
.LBB171_41:
	xori r4, fp, 1
	and r4, r4, r26
	xor fp, fp, r4
	beq r3, r0, .LBB171_62
.LBB171_42:
	bge r17, r25, .LBB171_61
.LBB171_43:
	addi r1, sp, 84
	add r18, r1, r17
	addi r26, r0, 1
	add r4, r0, r0
	add r21, r17, r0
	jal r0, .LBB171_45
.LBB171_44:
	addi r21, r21, 1
	addi r26, r26, 1
	add r4, r3, r0
	bge r21, r25, .LBB171_61
.LBB171_45:
	addi r1, sp, 84
	add r1, r1, r21
	ldb r3, r1+0
	addi r1, r0, 13
	beq r4, r0, .LBB171_47
.LBB171_46:
	seq r3, r4, r3
	sub r3, r0, r3
	and r3, r4, r3
	xor r3, r4, r3
	jal r0, .LBB171_58
.LBB171_47:
	andi r4, r3, 255
	addi r4, r4, -33
	addi r5, r0, 14
	bgtu r4, r5, .LBB171_50
.LBB171_48:
	slli r4, r4, 2
	lui r5, %hi(.LJTI171_0)
	addi r5, r5, %lo(.LJTI171_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB171_49:
	addi r1, r0, 11
	add r25, r21, r0
	jal r0, .LBB171_57
.LBB171_50:
	addi r1, r0, -3
	and r1, r3, r1
	addi r3, r0, 36
	bne r1, r3, .LBB171_56
.LBB171_51:
	addi r1, r21, 3
	bge r1, r25, .LBB171_56
.LBB171_52:
	addi r1, sp, 84
	add r1, r1, r21
	ldbu r3, r1+1
	jal r31, toupper
	addi r3, r0, 69
	bne r1, r3, .LBB171_56
.LBB171_53:
	addi r1, sp, 84
	add r1, r1, r21
	ldbu r3, r1+2
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB171_56
.LBB171_54:
	addi r1, sp, 84
	add r1, r1, r21
	ldbu r3, r1+3
	jal r31, toupper
	addi r3, r0, 68
	bne r1, r3, .LBB171_56
.LBB171_55:
	addi r4, r26, -1
	add r3, r18, r0
	jal r31, nml_text_add
	addi r1, r0, 1
	add r16, r0, r0
	jal r0, .LBB171_57
.LBB171_56:
	add r1, r0, r0
.LBB171_57:
	add r3, r0, r0
.LBB171_58:
	beq r1, r0, .LBB171_44
.LBB171_59:
	addi r4, r0, 13
	beq r1, r4, .LBB171_44
.LBB171_60:
	addi r3, r0, 11
	bne r1, r3, .LBB171_62
.LBB171_61:
	addi r1, sp, 84
	add r3, r1, r17
	sub r4, r25, r17
	jal r31, nml_text_add
	lui r3, %hi(.L.str.143)
	addi r3, r3, %lo(.L.str.143)
	addi r4, r0, 1
	jal r31, nml_text_add
	add r1, r0, r0
.LBB171_62:
	beq r1, r0, .LBB171_5
.LBB171_63:
	beq r1, r14, .LBB171_5
	jal r0, .LBB171_69
.LBB171_64:
	addi r1, r18, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB171_39
.LBB171_65:
	addi r1, r0, 64
	beq r18, r1, .LBB171_39
.LBB171_66:
	addi r4, r0, 95
	add r28, r0, r0
	add r3, r0, r0
	add r1, r14, r0
	beq r18, r4, .LBB171_41
.LBB171_67:
	addi r28, r0, 1
.LBB171_68:
	add r17, r21, r0
	add r3, r28, r0
	add r1, r0, r0
	jal r0, .LBB171_41
.LBB171_69:
	beq r16, r0, .LBB171_73
.LBB171_70:
	addi r1, r0, 1
	stw r19+%lo(nml_status), r1
	jal r0, .LBB171_72
.LBB171_71:
	lui r1, 2
	addi r1, r1, 1568
	sth sp+84, r1
	addi r11, sp, 84
	addi r3, r11, 2
	lui r4, %hi(nml_grp)
	addi r4, r4, %lo(nml_grp)
	add r5, r13, r0
	jal r31, memcpy
	addi r4, r13, 2
	add r3, r11, r0
	jal r31, nml_record
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
.LBB171_72:
	ldw r28, sp+1112
	ldw r27, sp+1116
	ldw r26, sp+1120
	ldw r25, sp+1124
	ldw r24, sp+1128
	ldw r23, sp+1132
	ldw r22, sp+1136
	ldw r21, sp+1140
	ldw r20, sp+1144
	ldw r19, sp+1148
	ldw r18, sp+1152
	ldw r17, sp+1156
	ldw r16, sp+1160
	ldw r15, sp+1164
	ldw r14, sp+1168
	ldw r13, sp+1172
	ldw r12, sp+1176
	ldw r11, sp+1180
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 1184
	jalr r0, r31, 0
.LBB171_73:
	ldw r1, sp+40
	stw r1+%lo(nml_nasg), r0
	ldw r1, sp+48
	stw r1+%lo(nml_nval), r0
	ldw r1, r22+%lo(nml_tlen)
	addi r15, r0, 1
	blt r1, r15, .LBB171_72
.LBB171_74:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB171_72
.LBB171_75:
	lui r16, %hi(nml_text)
	addi r17, r0, 44
	lui r18, %hi(nml_cval)
	lui r23, %hi(nml_val)
	addi r24, r0, 12
	addi r1, r0, 0
	stw sp+80, r1
	addi r25, r0, 2
	addi r27, r0, 32
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+72, r1
	lui r1, %hi(.L.str.147)
	addi r1, r1, %lo(.L.str.147)
	stw sp+68, r1
	addi r13, r0, 95
	lui r1, %hi(nml_err+95)
	stw sp+76, r1
	add r12, r15, r0
	add fp, r0, r0
	add r28, r0, r0
.LBB171_76:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r28
	ldbu r3, r1+0
	beq r3, r17, .LBB171_79
.LBB171_77:
	bne r3, r27, .LBB171_86
.LBB171_78:
	addi r11, r28, 1
	jal r0, .LBB171_209
.LBB171_79:
	beq r12, r0, .LBB171_85
.LBB171_80:
	beq fp, r0, .LBB171_85
.LBB171_81:
	ldw r1, sp+48
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB171_84
.LBB171_82:
	ldw r3, sp+80
	seq r3, r1, r3
	slli r1, r1, 1
	sub r3, r0, r3
	xori r4, r1, 64
	and r3, r4, r3
	xor r1, r1, r3
	stw r18+%lo(nml_cval), r1
	ldw r3, r23+%lo(nml_val)
	mul r4, r1, r24
	jal r31, realloc
	beq r1, r0, .LBB171_222
.LBB171_83:
	stw r23+%lo(nml_val), r1
.LBB171_84:
	ldw r1, r23+%lo(nml_val)
	ldw r5, sp+48
	ldw r3, r5+%lo(nml_nval)
	mul r4, r3, r24
	add r1, r1, r4
	stw r1+0, r28
	stw r1+4, r0
	stw r1+8, r15
	addi r1, r3, 1
	stw r5+%lo(nml_nval), r1
	ldw r1, fp+68
	addi r1, r1, 1
	stw fp+68, r1
.LBB171_85:
	addi r11, r28, 1
	jal r0, .LBB171_208
.LBB171_86:
	jal r31, isalpha
                                        # implicit-def: $r11
	add r5, r15, r0
	beq r1, r0, .LBB171_107
.LBB171_87:
	ldw r1, r22+%lo(nml_tlen)
	add r20, r28, r0
	bge r28, r1, .LBB171_94
.LBB171_88:
	add r20, r28, r0
	jal r0, .LBB171_90
.LBB171_89:
	addi r20, r20, 1
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB171_94
.LBB171_90:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB171_89
.LBB171_91:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB171_89
.LBB171_92:
	beq r14, r13, .LBB171_89
.LBB171_93:
	addi r1, r0, 64
	beq r14, r1, .LBB171_89
.LBB171_94:
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB171_104
.LBB171_95:
	ldw r3, r16+%lo(nml_text)
.LBB171_96:
	add r4, r3, r20
	ldbu r4, r4+0
	bne r4, r27, .LBB171_99
.LBB171_97:
	addi r20, r20, 1
	bne r1, r20, .LBB171_96
.LBB171_98:
	add r4, r1, r0
	jal r0, .LBB171_105
.LBB171_99:
	addi r5, r0, 40
	bne r4, r5, .LBB171_104
.LBB171_100:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r20, r1, .LBB171_107
.LBB171_101:
	add r5, r3, r20
	ldbu r6, r5+0
	addi r4, r20, 1
	addi r7, r0, 41
	beq r6, r7, .LBB171_217
.LBB171_102:
	add r20, r4, r0
	blt r4, r1, .LBB171_101
.LBB171_103:
                                        # implicit-def: $r11
	add r5, r15, r0
	jal r0, .LBB171_107
.LBB171_104:
	add r4, r20, r0
.LBB171_105:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r4, r1, .LBB171_107
.LBB171_106:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r4
	ldbu r1, r1+0
	addi r3, r0, 61
	sne r5, r1, r3
	addi r11, r4, 1
.LBB171_107:
	beq r5, r0, .LBB171_117
.LBB171_108:
	beq fp, r0, .LBB171_163
.LBB171_109:
	ldw r1, r22+%lo(nml_tlen)
	add r11, r28, r0
	bge r28, r1, .LBB171_112
.LBB171_110:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB171_112
.LBB171_111:
	addi r11, r11, 1
	ldw r1, r22+%lo(nml_tlen)
	blt r11, r1, .LBB171_110
.LBB171_112:
	ble r11, r28, .LBB171_131
.LBB171_113:
	ldw r1, r22+%lo(nml_tlen)
	bge r11, r1, .LBB171_131
.LBB171_114:
	ldw r1, r16+%lo(nml_text)
	add r3, r1, r11
	ldbu r3, r3+0
	addi r4, r0, 42
	bne r3, r4, .LBB171_131
.LBB171_115:
	add r3, r1, r28
	jal r31, atoi
	ble r1, r0, .LBB171_214
.LBB171_116:
	add r14, r1, r0
	addi r28, r11, 1
	jal r0, .LBB171_132
.LBB171_117:
	ldw r1, sp+40
	ldw r4, r1+%lo(nml_nasg)
	lui r1, %hi(nml_casg)
	ldw r3, r1+%lo(nml_casg)
	lui r14, %hi(nml_asg)
	blt r4, r3, .LBB171_120
.LBB171_118:
	ldw r4, sp+80
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
	beq r1, r0, .LBB171_222
.LBB171_119:
	stw r14+%lo(nml_asg), r1
.LBB171_120:
	ldw r1, r14+%lo(nml_asg)
	ldw r5, sp+40
	ldw r3, r5+%lo(nml_nasg)
	addi r4, r3, 1
	stw r5+%lo(nml_nasg), r4
	addi r4, r0, 76
	mul r3, r3, r4
	add fp, r1, r3
	ldw r1, r22+%lo(nml_tlen)
	bge r28, r1, .LBB171_130
.LBB171_121:
	add r21, r0, r0
.LBB171_122:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r28
	add r1, r1, r21
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	addi r3, r0, 1
	beq r1, r0, .LBB171_126
.LBB171_123:
	beq r3, r0, .LBB171_166
.LBB171_124:
	addi r1, r0, 30
	bgtu r21, r1, .LBB171_166
.LBB171_125:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r28
	add r1, r1, r21
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r21, 1
	add r4, fp, r21
	stb r4+0, r1
	ldw r1, r22+%lo(nml_tlen)
	add r20, r28, r3
	add r21, r3, r0
	blt r20, r1, .LBB171_122
	jal r0, .LBB171_167
.LBB171_126:
	addi r1, r14, -35
	addi r4, r0, 2
	bltu r1, r4, .LBB171_123
.LBB171_127:
	addi r1, r0, 64
	beq r14, r1, .LBB171_123
.LBB171_128:
	beq r14, r13, .LBB171_123
.LBB171_129:
	add r3, r0, r0
	jal r0, .LBB171_123
.LBB171_130:
	add r20, r28, r0
	add r21, r0, r0
	jal r0, .LBB171_168
.LBB171_131:
	add r14, r15, r0
.LBB171_132:
	add r12, r0, r0
	add r1, r0, r0
	add r11, r28, r0
	jal r0, .LBB171_137
.LBB171_133:
	add r1, r0, r0
.LBB171_134:
	add r4, r11, r0
.LBB171_135:
	addi r11, r4, 1
	add r3, r0, r0
.LBB171_136:
	bne r3, r0, .LBB171_159
.LBB171_137:
	ldw r3, r22+%lo(nml_tlen)
	bge r11, r3, .LBB171_159
.LBB171_138:
	ldw r6, r16+%lo(nml_text)
	add r4, r6, r11
	ldb r5, r4+0
	beq r1, r0, .LBB171_142
.LBB171_139:
	bne r1, r5, .LBB171_134
.LBB171_140:
	addi r4, r11, 1
	bge r4, r3, .LBB171_133
.LBB171_141:
	add r3, r6, r4
	ldb r3, r3+0
	bne r1, r3, .LBB171_133
	jal r0, .LBB171_135
.LBB171_142:
	andi r20, r5, 255
	addi r1, r20, -32
	addi r6, r0, 9
	bgtu r1, r6, .LBB171_152
.LBB171_143:
	slli r1, r1, 2
	lui r6, %hi(.LJTI171_1)
	addi r6, r6, %lo(.LJTI171_1)
	add r1, r6, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB171_144:
	addi r11, r11, 1
	add r3, r0, r0
	add r1, r5, r0
	jal r0, .LBB171_136
.LBB171_145:
	addi r1, r0, 2
	blt r11, r1, .LBB171_152
.LBB171_146:
	addi r21, r11, 1
	bge r21, r3, .LBB171_152
.LBB171_147:
	ldbu r3, r4+-1
	jal r31, toupper
	addi r1, r1, -70
	addi r3, r0, -2
	bltu r1, r3, .LBB171_152
.LBB171_148:
	ldw r1, r16+%lo(nml_text)
	addi r26, r11, -2
	add r1, r1, r26
	ldbu r3, r1+0
	jal r31, isdigit
	bne r1, r0, .LBB171_150
.LBB171_149:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r26
	ldbu r1, r1+0
	addi r3, r0, 46
	bne r1, r3, .LBB171_152
.LBB171_150:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r21
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB171_152
.LBB171_151:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	addi r3, r0, 43
	stb r1+0, r3
	jal r0, .LBB171_157
.LBB171_152:
	bgt r12, r0, .LBB171_157
.LBB171_153:
	addi r3, r0, 1
	beq r20, r27, .LBB171_158
.LBB171_154:
	beq r20, r17, .LBB171_158
	jal r0, .LBB171_157
.LBB171_155:
	addi r12, r12, 1
	jal r0, .LBB171_157
.LBB171_156:
	addi r12, r12, -1
.LBB171_157:
	addi r11, r11, 1
	add r3, r0, r0
.LBB171_158:
	add r1, r0, r0
	jal r0, .LBB171_136
.LBB171_159:
	ldw r1, sp+48
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB171_162
.LBB171_160:
	ldw r3, sp+80
	seq r3, r1, r3
	slli r1, r1, 1
	sub r3, r0, r3
	xori r4, r1, 64
	and r3, r4, r3
	xor r1, r1, r3
	stw r18+%lo(nml_cval), r1
	ldw r3, r23+%lo(nml_val)
	mul r4, r1, r24
	jal r31, realloc
	beq r1, r0, .LBB171_222
.LBB171_161:
	stw r23+%lo(nml_val), r1
.LBB171_162:
	ldw r1, r23+%lo(nml_val)
	ldw r5, sp+48
	ldw r3, r5+%lo(nml_nval)
	mul r4, r3, r24
	add r1, r1, r4
	stw r1+0, r28
	sub r4, r11, r28
	stw r1+4, r4
	stw r1+8, r14
	addi r1, r3, 1
	stw r5+%lo(nml_nval), r1
	ldw r1, fp+68
	addi r1, r1, 1
	stw fp+68, r1
	add r12, r0, r0
	add r26, r0, r0
	jal r0, .LBB171_210
.LBB171_163:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB171_165
.LBB171_164:
	stw r19+%lo(nml_status), r25
	ldw r3, sp+72
	ldw r4, sp+68
	add r5, r13, r0
	jal r31, strncpy
	ldw r1, sp+76
	stb r1+%lo(nml_err+95), r0
.LBB171_165:
	add r11, r28, r0
	add fp, r0, r0
	add r26, r15, r0
	jal r0, .LBB171_210
.LBB171_166:
	add r20, r28, r21
	jal r0, .LBB171_168
.LBB171_167:
	add r21, r3, r0
.LBB171_168:
	add r1, fp, r21
	stb r1+0, r0
	jal r0, .LBB171_170
.LBB171_169:
	addi r20, r20, 1
.LBB171_170:
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB171_175
.LBB171_171:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB171_169
.LBB171_172:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB171_169
.LBB171_173:
	beq r14, r13, .LBB171_169
.LBB171_174:
	addi r1, r0, 64
	beq r14, r1, .LBB171_169
.LBB171_175:
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB171_180
.LBB171_176:
	ldw r3, r16+%lo(nml_text)
.LBB171_177:
	add r4, r3, r20
	ldbu r4, r4+0
	bne r4, r27, .LBB171_180
.LBB171_178:
	addi r20, r20, 1
	bne r1, r20, .LBB171_177
.LBB171_179:
	add r20, r1, r0
.LBB171_180:
	stw fp+32, r0
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r1, r1+0
	addi r3, r0, 40
	bne r1, r3, .LBB171_207
.LBB171_181:
	addi r20, r20, 1
	addi r14, fp, 36
	jal r0, .LBB171_184
.LBB171_182:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r1, r1+0
	seq r1, r1, r17
	ori  r26, r1, 10
	add r20, r20, r1
.LBB171_183:
	addi r1, r0, 11
	bne r26, r1, .LBB171_206
.LBB171_184:
	ldw r1, r16+%lo(nml_text)
.LBB171_185:
	add r3, r1, r20
	ldbu r4, r3+0
	bne r4, r27, .LBB171_187
.LBB171_186:
	addi r20, r20, 1
	jal r0, .LBB171_185
.LBB171_187:
	addi r21, r0, 58
	bne r4, r21, .LBB171_189
.LBB171_188:
	stw sp+84, r3
	lui r1, 524288
	jal r0, .LBB171_190
.LBB171_189:
	addi r4, sp, 84
	addi r5, r0, 10
	jal r31, strtol
	ldw r3, sp+84
	ldw r4, r16+%lo(nml_text)
	add r4, r4, r20
	beq r4, r3, .LBB171_201
.LBB171_190:
	ldw r3, fp+32
	addi r4, r0, 6
	bgt r3, r4, .LBB171_192
.LBB171_191:
	addi r4, r3, 1
	stw fp+32, r4
	slli r3, r3, 2
	add r3, r14, r3
	stw r3+0, r1
.LBB171_192:
	ldw r3, sp+84
	ldw r1, r16+%lo(nml_text)
	sub r20, r3, r1
.LBB171_193:
	add r3, r1, r20
	ldbu r3, r3+0
	bne r3, r27, .LBB171_195
.LBB171_194:
	addi r20, r20, 1
	jal r0, .LBB171_193
.LBB171_195:
	beq r3, r21, .LBB171_197
	jal r0, .LBB171_182
.LBB171_196:
	addi r3, r3, 1
	addi r4, sp, 1108
	addi r5, r0, 10
	jal r31, strtol
	addi r26, r0, 1
	bne r1, r26, .LBB171_204
.LBB171_197:
	addi r20, r20, 1
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB171_182
.LBB171_198:
	ldw r1, r16+%lo(nml_text)
	add r3, r1, r20
	ldbu r1, r3+0
	beq r1, r21, .LBB171_196
.LBB171_199:
	addi r3, r0, 41
	beq r1, r3, .LBB171_182
.LBB171_200:
	bne r1, r17, .LBB171_197
	jal r0, .LBB171_182
.LBB171_201:
	ldw r1, r19+%lo(nml_status)
	addi r26, r0, 1
	bne r1, r0, .LBB171_183
.LBB171_202:
	addi r1, r0, 2
	stw r19+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.145)
	addi r4, r4, %lo(.L.str.145)
.LBB171_203:
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+76
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB171_183
.LBB171_204:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB171_183
.LBB171_205:
	addi r1, r0, 2
	stw r19+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.146)
	addi r4, r4, %lo(.L.str.146)
	jal r0, .LBB171_203
.LBB171_206:
	addi r1, r0, 10
	bne r26, r1, .LBB171_213
.LBB171_207:
	ldw r1, sp+48
	ldw r1, r1+%lo(nml_nval)
	stw fp+64, r1
	stw fp+68, r0
	stw fp+72, r0
.LBB171_208:
	add r12, r15, r0
.LBB171_209:
	add r26, r25, r0
.LBB171_210:
	ori  r1, r26, 2
	bne r1, r25, .LBB171_72
.LBB171_211:
	ldw r1, r22+%lo(nml_tlen)
	bge r11, r1, .LBB171_72
.LBB171_212:
	ldw r1, r19+%lo(nml_status)
	add r28, r11, r0
	beq r1, r0, .LBB171_76
	jal r0, .LBB171_72
.LBB171_213:
	add r11, r28, r0
	jal r0, .LBB171_210
.LBB171_214:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB171_216
.LBB171_215:
	addi r1, r0, 2
	stw r19+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.148)
	addi r4, r4, %lo(.L.str.148)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+76
	stb r1+%lo(nml_err+95), r0
.LBB171_216:
	add r11, r28, r0
	add r26, r15, r0
	jal r0, .LBB171_210
.LBB171_217:
	bge r4, r1, .LBB171_105
.LBB171_218:
	add r3, r0, r0
.LBB171_219:
	add r4, r5, r3
	ldbu r4, r4+1
	bne r4, r27, .LBB171_221
.LBB171_220:
	addi r3, r3, 1
	add r4, r20, r3
	addi r4, r4, 1
	blt r4, r1, .LBB171_219
	jal r0, .LBB171_105
.LBB171_221:
	add r3, r20, r3
	addi r4, r3, 1
	jal r0, .LBB171_105
.LBB171_222:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.144)
	addi r4, r4, %lo(.L.str.144)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end171:
	.size	f77_nml_begin, .Lfunc_end171-f77_nml_begin
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI171_0,@object
.LJTI171_0:
	.word	.LBB171_49
	.word	.LBB171_58
	.word	.LBB171_50
	.word	.LBB171_50
	.word	.LBB171_50
	.word	.LBB171_50
	.word	.LBB171_58
	.word	.LBB171_50
	.word	.LBB171_50
	.word	.LBB171_50
	.word	.LBB171_50
	.word	.LBB171_50
	.word	.LBB171_50
	.word	.LBB171_50
	.word	.LBB171_55
	.size	.LJTI171_0, 60
	.type	.LJTI171_1,@object
.LJTI171_1:
	.word	.LBB171_145
	.word	.LBB171_152
	.word	.LBB171_144
	.word	.LBB171_152
	.word	.LBB171_152
	.word	.LBB171_152
	.word	.LBB171_152
	.word	.LBB171_144
	.word	.LBB171_155
	.word	.LBB171_156
	.size	.LJTI171_1, 40
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function nml_record
	.type	nml_record,@function
nml_record:                             # @nml_record
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	add r11, r4, r0
	add r12, r3, r0
	lui r1, %hi(nml_unit)
	ldw r3, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.141)
	addi r4, r4, %lo(.L.str.141)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	add r3, r12, r0
	add r4, r11, r0
	jal r31, f77_wr_a
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB172_4
.LBB172_1:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB172_3
.LBB172_2:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB172_4
.LBB172_3:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB172_4:
	lui r1, %hi(fio_noadv)
	ldbu r3, r1+%lo(fio_noadv)
	lui r11, %hi(fio_ibuf)
	ldw r4, r11+%lo(fio_ibuf)
	addi r5, r0, 0
	seq r4, r4, r5
	and r3, r3, r4
	lui r12, %hi(fio_open_rec)
	stw r12+%lo(fio_open_rec), r3
	stb r1+%lo(fio_noadv), r0
	jal r31, fio_flush_line
	stw r12+%lo(fio_open_rec), r0
	stw r11+%lo(fio_ibuf), r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end172:
	.size	nml_record, .Lfunc_end172-nml_record
                                        # -- End function
	.globl	f77_nml_dim                     # -- Begin function f77_nml_dim
	.p2align	2
	.type	f77_nml_dim,@function
f77_nml_dim:                            # @f77_nml_dim
# %bb.0:
	lui r1, %hi(nml_ndim)
	ldw r5, r1+%lo(nml_ndim)
	addi r6, r0, 6
	bgt r5, r6, .LBB173_2
.LBB173_1:
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
.LBB173_2:
	jalr r0, r31, 0
.Lfunc_end173:
	.size	f77_nml_dim, .Lfunc_end173-f77_nml_dim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_nml_item
	.type	.LCPI174_0,@object
.LCPI174_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI174_0, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI174_1,@object
.LCPI174_1:
	.word	0x00000000                      # float 0
	.size	.LCPI174_1, 4
	.text
	.globl	f77_nml_item
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
	add r26, r8, r0
	add r12, r7, r0
	add r23, r6, r0
	add r14, r5, r0
	add r16, r3, r0
	addi r1, r0, 31
	slt r1, r4, r1
	sub r1, r0, r1
	xori r3, r4, 31
	and r1, r3, r1
	xori r15, r1, 31
	addi r17, r0, 1
	addi r11, sp, 248
	blt r4, r17, .LBB174_3
.LBB174_1:
	add r13, r15, r0
.LBB174_2:
	ldbu r3, r16+0
	jal r31, toupper
	stb r11+0, r1
	addi r13, r13, -1
	addi r11, r11, 1
	addi r16, r16, 1
	bne r13, r0, .LBB174_2
.LBB174_3:
	addi r1, sp, 248
	add r1, r1, r15
	stb r1+0, r0
	addi r1, r0, 6
	stw sp+108, r1
	beq r14, r1, .LBB174_6
.LBB174_4:
	addi r1, r0, 2
	beq r14, r1, .LBB174_7
.LBB174_5:
	bne r14, r17, .LBB174_8
.LBB174_6:
	addi r1, r0, 0
	sgt r1, r12, r1
	sub r1, r0, r1
	xori r3, r12, 4
	and r1, r3, r1
	xori fp, r1, 4
	jal r0, .LBB174_11
.LBB174_7:
	addi fp, r0, 4
	jal r0, .LBB174_11
.LBB174_8:
	addi r3, r14, -3
	bgeu r3, r1, .LBB174_10
.LBB174_9:
	addi fp, r0, 8
	jal r0, .LBB174_11
.LBB174_10:
	addi r1, r0, 5
	seq r1, r14, r1
	sub r1, r0, r1
	xori r3, r12, 16
	and r1, r3, r1
	xor fp, r12, r1
.LBB174_11:
	lui r1, %hi(nml_reading)
	ldw r1, r1+%lo(nml_reading)
	lui r3, %hi(nml_ndim)
	stw sp+72, r3
	lui r21, %hi(fio_busy)
	lui r3, %hi(fio_listed)
	stw sp+88, r3
	lui r3, %hi(fio_noadv)
	stw sp+100, r3
	lui r3, %hi(fio_ibuf)
	stw sp+104, r3
	stw sp+92, r26
	beq r1, r0, .LBB174_122
.LBB174_12:
	stw sp+52, r17
	lui r25, %hi(nml_status)
	ldw r1, r25+%lo(nml_status)
	bne r1, r0, .LBB174_185
.LBB174_13:
	lui r1, %hi(nml_nasg)
	stw sp+68, r1
	ldw r1, r1+%lo(nml_nasg)
	ldw r3, sp+52
	blt r1, r3, .LBB174_185
.LBB174_14:
	ldw r1, r25+%lo(nml_status)
	bne r1, r0, .LBB174_185
.LBB174_15:
	addi r1, r0, 36
	stw sp+80, r1
	lui r1, %hi(nml_asg)
	stw sp+64, r1
	addi r1, r0, 76
	stw sp+60, r1
	addi r1, r0, 13
	stw sp+56, r1
	lui r1, 524288
	stw sp+36, r1
	addi r16, r0, 2
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+32, r1
	lui r1, %hi(.L.str.40)
	addi r1, r1, %lo(.L.str.40)
	stw sp+44, r1
	addi r1, r0, 95
	stw sp+40, r1
	lui r1, %hi(nml_err+95)
	stw sp+96, r1
	lui r1, %hi(nml_ext)
	addi r1, r1, %lo(nml_ext)
	stw sp+48, r1
	add r4, r0, r0
.LBB174_16:
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
	beq r1, r0, .LBB174_20
.LBB174_17:
	addi r1, r0, 11
	ldw r4, sp+76
	beq r11, r1, .LBB174_185
.LBB174_18:
	addi r4, r4, 1
	ldw r1, sp+68
	ldw r1, r1+%lo(nml_nasg)
	bge r4, r1, .LBB174_185
.LBB174_19:
	ldw r1, r25+%lo(nml_status)
	ldw r3, sp+80
	addi r3, r3, 76
	stw sp+80, r3
	beq r1, r0, .LBB174_16
	jal r0, .LBB174_185
.LBB174_20:
	ldw r20, sp+52
	stw r18+72, r20
	ldw r1, r18+32
	blt r1, r20, .LBB174_33
.LBB174_21:
	ldw r3, sp+72
	ldw r3, r3+%lo(nml_ndim)
	bne r1, r3, .LBB174_34
.LBB174_22:
	ldw r1, r18+32
	blt r1, r20, .LBB174_37
.LBB174_23:
	ldw r1, sp+80
	add r11, r13, r1
	add r13, r0, r0
	add r17, r0, r0
	add r19, r20, r0
	add r15, r0, r0
	ldw r22, sp+36
	ldw r24, sp+32
	jal r0, .LBB174_26
.LBB174_24:
	mul r1, r1, r19
	add r15, r1, r15
	mul r19, r3, r19
	add r1, r20, r0
	beq r1, r0, .LBB174_38
.LBB174_25:
	addi r17, r17, 1
	ldw r1, r18+32
	addi r13, r13, 4
	bge r17, r1, .LBB174_38
.LBB174_26:
	add r1, r11, r13
	ldw r1, r1+0
	bne r1, r22, .LBB174_28
.LBB174_27:
	add r1, r0, r0
	bge r1, r0, .LBB174_29
	jal r0, .LBB174_30
.LBB174_28:
	lui r3, %hi(nml_lo)
	addi r3, r3, %lo(nml_lo)
	add r3, r13, r3
	ldw r3, r3+0
	sub r1, r1, r3
	blt r1, r0, .LBB174_30
.LBB174_29:
	ldw r3, sp+48
	add r3, r13, r3
	ldw r3, r3+0
	blt r1, r3, .LBB174_24
.LBB174_30:
	ldw r1, r25+%lo(nml_status)
	beq r1, r0, .LBB174_32
.LBB174_31:
	add r1, r0, r0
	bne r1, r0, .LBB174_25
	jal r0, .LBB174_38
.LBB174_32:
	stw r25+%lo(nml_status), r16
	add r3, r24, r0
	ldw r4, sp+44
	ldw r5, sp+40
	jal r31, strncpy
	ldw r1, sp+96
	stb r1+%lo(nml_err+95), r0
	add r1, r0, r0
	bne r1, r0, .LBB174_25
	jal r0, .LBB174_38
.LBB174_33:
	add r15, r0, r0
	jal r0, .LBB174_39
.LBB174_34:
	ldw r1, r25+%lo(nml_status)
	addi r11, r0, 11
	bne r1, r0, .LBB174_36
.LBB174_35:
	stw r25+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.39)
	addi r4, r4, %lo(.L.str.39)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+96
	stb r1+%lo(nml_err+95), r0
.LBB174_36:
	add r1, r0, r0
	add r15, r0, r0
	beq r1, r0, .LBB174_17
	jal r0, .LBB174_39
.LBB174_37:
	add r15, r0, r0
.LBB174_38:
	ldw r1, r25+%lo(nml_status)
	addi r3, r0, 0
	seq r1, r1, r3
	addi r3, r1, -1
	andi r11, r3, 11
	beq r1, r0, .LBB174_17
.LBB174_39:
	ldw r1, r18+68
	addi r24, r0, 1
	blt r1, r24, .LBB174_121
.LBB174_40:
	ldw r1, r25+%lo(nml_status)
	add r11, r0, r0
	bne r1, r0, .LBB174_17
.LBB174_41:
	ldw r17, r18+64
.LBB174_42:
	lui r22, %hi(nml_val)
	ldw r1, r22+%lo(nml_val)
	addi r3, r0, 12
	mul r28, r17, r3
	add r3, r1, r28
	ldw r1, r3+8
	blt r1, r24, .LBB174_119
.LBB174_43:
	bge r15, r26, .LBB174_116
.LBB174_44:
	xor r1, r15, r26
	sgt r4, r15, r26
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r26, r1
	sub r27, r1, r15
	mul r1, fp, r15
	add r11, r23, r1
	add r26, r0, r0
.LBB174_45:
	ldw r1, r3+4
	blt r1, r24, .LBB174_111
.LBB174_46:
	mul r4, r15, fp
	add r19, r23, r4
	lui r4, %hi(nml_text)
	ldw r4, r4+%lo(nml_text)
	ldw r3, r3+0
	add r4, r4, r3
	addi r3, r0, 7
	beq r14, r3, .LBB174_57
.LBB174_47:
	ldw r3, sp+108
	bne r14, r3, .LBB174_69
.LBB174_48:
	ldbu r3, r4+0
	addi r5, r0, 46
	seq r3, r3, r5
	ble r1, r3, .LBB174_51
.LBB174_49:
	add r13, r4, r3
	ldbu r3, r13+0
	jal r31, toupper
	addi r4, r0, 84
	add r3, r24, r0
	beq r1, r4, .LBB174_54
.LBB174_50:
	ldbu r3, r13+0
	jal r31, toupper
	addi r3, r0, 70
	beq r1, r3, .LBB174_53
.LBB174_51:
	ldw r1, r25+%lo(nml_status)
	add r3, r0, r0
	bne r1, r0, .LBB174_54
.LBB174_52:
	stw r25+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.152)
	addi r4, r4, %lo(.L.str.152)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+96
	stb r1+%lo(nml_err+95), r0
.LBB174_53:
	add r3, r0, r0
.LBB174_54:
	beq r12, r16, .LBB174_82
.LBB174_55:
	bne r12, r24, .LBB174_83
.LBB174_56:
	stb r19+0, r3
	jal r0, .LBB174_111
.LBB174_57:
	ldbu r3, r4+0
	addi r5, r0, 34
	beq r3, r5, .LBB174_59
.LBB174_58:
	addi r5, r0, 39
	bne r3, r5, .LBB174_84
.LBB174_59:
	add r5, r0, r0
	addi r6, r0, 1
	bne r1, r6, .LBB174_64
.LBB174_60:
	ble r12, r5, .LBB174_111
.LBB174_61:
	add r1, r11, r5
	sub r3, r12, r5
.LBB174_62:
	addi r4, r0, 32
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB174_62
	jal r0, .LBB174_111
.LBB174_63:
	addi r6, r6, 1
	bge r6, r1, .LBB174_60
.LBB174_64:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB174_67
.LBB174_65:
	addi r6, r6, 1
	bge r6, r1, .LBB174_60
.LBB174_66:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB174_60
.LBB174_67:
	bge r5, r12, .LBB174_63
.LBB174_68:
	add r7, r4, r6
	ldbu r7, r7+0
	addi r8, r5, 1
	add r5, r19, r5
	stb r5+0, r7
	add r5, r8, r0
	jal r0, .LBB174_63
.LBB174_69:
	addi r3, r0, 1023
	slt r3, r1, r3
	sub r3, r0, r3
	xori r1, r1, 1023
	and r1, r1, r3
	xori r20, r1, 1023
	add r13, r21, r0
	addi r21, sp, 280
	add r3, r21, r0
	add r5, r20, r0
	jal r31, memcpy
	add r1, r21, r20
	stb r1+0, r0
	ldbu r1, r13+%lo(fio_busy)
	beq r1, r24, .LBB174_186
.LBB174_70:
	add r21, r13, r0
	stb r13+%lo(fio_busy), r24
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB174_74
.LBB174_71:
	addi r3, r0, -400
.LBB174_72:
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	add r4, r3, r4
	addi r5, r0, -1
	stw r4+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB174_72
.LBB174_73:
	stb r1+%lo(fio_uend_init), r24
.LBB174_74:
	lui r1, %hi(stdin)
	ldw r1, r1+%lo(stdin)
	lui r3, %hi(fio_in)
	stw r3+%lo(fio_in), r1
	lui r13, %hi(fio_unit)
	addi r1, r0, -1
	stw r13+%lo(fio_unit), r1
	addi r1, sp, 280
	ldw r3, sp+104
	stw r3+%lo(fio_ibuf), r1
	lui r1, %hi(fio_ilen)
	stw r1+%lo(fio_ilen), r20
	lui r1, %hi(fio_inrec)
	stw r1+%lo(fio_inrec), r24
	lui r1, %hi(fio_irec)
	stw r1+%lo(fio_irec), r0
	addi r20, r0, 3
	add r3, r0, r0
	add r4, r0, r0
	add r5, r20, r0
	jal r31, fio_rd_setup
	beq r1, r0, .LBB174_77
.LBB174_75:
	stb r21+%lo(fio_busy), r0
	ldw r1, r25+%lo(nml_status)
	bne r1, r0, .LBB174_111
.LBB174_76:
	stw r25+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.154)
	addi r4, r4, %lo(.L.str.154)
	jal r0, .LBB174_86
.LBB174_77:
	addi r1, r14, -1
	bgtu r1, r20, .LBB174_94
.LBB174_78:
	slli r1, r1, 2
	lui r3, %hi(.LJTI174_0)
	addi r3, r3, %lo(.LJTI174_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB174_79:
	stw sp+184, r0
	addi r3, sp, 184
	jal r31, f77_rd_i
	ldw r1, sp+184
	beq r12, r16, .LBB174_113
.LBB174_80:
	bne r12, r24, .LBB174_114
.LBB174_81:
	stb r19+0, r1
	jal r0, .LBB174_102
.LBB174_82:
	sth r19+0, r3
	jal r0, .LBB174_111
.LBB174_83:
	stw r19+0, r3
	jal r0, .LBB174_111
.LBB174_84:
	ldw r1, r25+%lo(nml_status)
	bne r1, r0, .LBB174_111
.LBB174_85:
	stw r25+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.153)
	addi r4, r4, %lo(.L.str.153)
.LBB174_86:
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+96
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB174_111
.LBB174_87:
	add r3, r19, r0
	jal r0, .LBB174_101
.LBB174_88:
	ldw r1, sp+88
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB174_99
.LBB174_89:
	lui r20, %hi(fio_ldone)
	ldbu r1, r20+%lo(fio_ldone)
	bne r1, r0, .LBB174_102
.LBB174_90:
	jal r31, fio_list_tok
	beq r1, r16, .LBB174_102
.LBB174_91:
	beq r1, r24, .LBB174_98
.LBB174_92:
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
	jal r0, .LBB174_102
.LBB174_93:
	add r3, r19, r0
	jal r31, f77_rd_r
	jal r0, .LBB174_102
.LBB174_94:
	ldw r1, sp+88
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB174_100
.LBB174_95:
	lui r20, %hi(fio_ldone)
	ldbu r1, r20+%lo(fio_ldone)
	bne r1, r0, .LBB174_102
.LBB174_96:
	jal r31, fio_list_tok
	beq r1, r16, .LBB174_102
.LBB174_97:
	bne r1, r24, .LBB174_115
.LBB174_98:
	stb r20+%lo(fio_ldone), r24
	jal r0, .LBB174_102
.LBB174_99:
	add r3, r19, r0
	jal r31, f77_rd_r
	addi r3, r19, 4
	jal r31, f77_rd_r
	jal r0, .LBB174_102
.LBB174_100:
	add r3, r19, r0
	jal r31, f77_rd_d
	addi r3, r19, 8
.LBB174_101:
	jal r31, f77_rd_d
.LBB174_102:
	ldw r1, sp+100
	ldbu r1, r1+%lo(fio_noadv)
	bne r1, r24, .LBB174_110
.LBB174_103:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB174_107
.LBB174_104:
	ldw r1, r13+%lo(fio_unit)
	addi r3, r1, -1
	addi r4, r0, 99
	sltu r3, r3, r4
	sub r3, r0, r3
	and r1, r1, r3
	slli r13, r1, 2
	lui r1, %hi(fio_psave)
	addi r1, r1, %lo(fio_psave)
	add r19, r13, r1
	ldw r1, r19+0
	bne r1, r0, .LBB174_106
.LBB174_105:
	addi r3, r0, 1024
	jal r31, malloc
	stw r19+0, r1
.LBB174_106:
	ldw r3, r19+0
	lui r19, %hi(fio_rlen)
	ldw r5, r19+%lo(fio_rlen)
	lui r4, %hi(fio_rec)
	addi r4, r4, %lo(fio_rec)
	jal r31, memcpy
	ldw r1, r19+%lo(fio_rlen)
	lui r3, %hi(fio_plen)
	addi r3, r3, %lo(fio_plen)
	add r3, r13, r3
	stw r3+0, r1
	lui r1, %hi(fio_rpos)
	ldw r1, r1+%lo(fio_rpos)
	lui r3, %hi(fio_ppos)
	addi r3, r3, %lo(fio_ppos)
	add r3, r13, r3
	stw r3+0, r1
	lui r1, %hi(fio_pend)
	addi r1, r1, %lo(fio_pend)
	add r1, r13, r1
	stw r1+0, r24
.LBB174_107:
	lui r1, %hi(fio_sizep)
	ldw r3, r1+%lo(fio_sizep)
	beq r3, r0, .LBB174_109
.LBB174_108:
	lui r4, %hi(fio_size)
	ldw r4, r4+%lo(fio_size)
	stw r3+0, r4
.LBB174_109:
	ldw r3, sp+100
	stb r3+%lo(fio_noadv), r0
	stw r1+%lo(fio_sizep), r0
.LBB174_110:
	stb r21+%lo(fio_busy), r0
	lui r1, %hi(fio_reading)
	stb r1+%lo(fio_reading), r0
	ldw r1, sp+104
	stw r1+%lo(fio_ibuf), r0
.LBB174_111:
	addi r15, r15, 1
	addi r26, r26, 1
	ldw r1, r22+%lo(nml_val)
	add r3, r1, r28
	ldw r1, r3+8
	bge r26, r1, .LBB174_118
.LBB174_112:
	add r11, r11, fp
	bne r26, r27, .LBB174_45
	jal r0, .LBB174_116
.LBB174_113:
	sth r19+0, r1
	jal r0, .LBB174_102
.LBB174_114:
	stw r19+0, r1
	jal r0, .LBB174_102
.LBB174_115:
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
	jal r0, .LBB174_102
.LBB174_116:
	ldw r1, r25+%lo(nml_status)
	bne r1, r0, .LBB174_118
.LBB174_117:
	stw r25+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.41)
	addi r4, r4, %lo(.L.str.41)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+96
	stb r1+%lo(nml_err+95), r0
.LBB174_118:
	ldw r26, sp+92
.LBB174_119:
	addi r17, r17, 1
	ldw r1, r18+64
	ldw r3, r18+68
	add r1, r3, r1
	bge r17, r1, .LBB174_121
.LBB174_120:
	ldw r1, r25+%lo(nml_status)
	add r11, r0, r0
	beq r1, r0, .LBB174_42
	jal r0, .LBB174_17
.LBB174_121:
	add r11, r0, r0
	jal r0, .LBB174_17
.LBB174_122:
	addi r16, sp, 144
	addi r4, sp, 248
	add r3, r16, r0
	add r5, r15, r0
	jal r31, memcpy
	add r1, r16, r15
	addi r3, r0, 61
	stb r1+0, r3
	lui r11, %hi(nml_npend)
	ldw r1, r11+%lo(nml_npend)
	lui r13, %hi(nml_len)
	lui r16, %hi(nml_line)
	addi r16, r16, %lo(nml_line)
	blt r1, r0, .LBB174_127
.LBB174_123:
	addi r17, r1, 1
	stw r11+%lo(nml_npend), r17
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	add r1, r1, r3
	addi r3, r0, 44
	stb r1+0, r3
	ldw r4, r13+%lo(nml_len)
	addi r1, r0, 2
	blt r4, r1, .LBB174_126
.LBB174_124:
	add r1, r4, r17
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB174_126
.LBB174_125:
	add r3, r16, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r16+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
.LBB174_126:
	ldw r1, r13+%lo(nml_len)
	add r3, r1, r17
	addi r4, r0, 1024
	sgt r3, r3, r4
	sub r4, r4, r1
	xor r4, r4, r17
	sub r3, r0, r3
	and r3, r4, r3
	xor r17, r17, r3
	add r3, r1, r16
	lui r4, %hi(nml_pend)
	addi r4, r4, %lo(nml_pend)
	add r5, r17, r0
	jal r31, memcpy
	ldw r1, r13+%lo(nml_len)
	add r1, r17, r1
	stw r13+%lo(nml_len), r1
	addi r1, r0, -1
	stw r11+%lo(nml_npend), r1
.LBB174_127:
	add r22, r21, r0
	addi r15, r15, 1
	ldw r4, r13+%lo(nml_len)
	addi r11, r0, 2
	blt r4, r11, .LBB174_130
.LBB174_128:
	add r1, r4, r15
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB174_130
.LBB174_129:
	add r3, r16, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r16+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
.LBB174_130:
	ldw r1, r13+%lo(nml_len)
	add r3, r1, r15
	addi r4, r0, 1024
	sgt r3, r3, r4
	sub r4, r4, r1
	xor r4, r4, r15
	sub r3, r0, r3
	and r3, r4, r3
	xor r15, r15, r3
	add r3, r1, r16
	addi r4, sp, 144
	add r5, r15, r0
	jal r31, memcpy
	ldw r1, r13+%lo(nml_len)
	add r1, r15, r1
	stw r13+%lo(nml_len), r1
	addi r1, r0, 1
	blt r26, r1, .LBB174_185
.LBB174_131:
	addi r1, sp, 184
	addi r15, r1, 1
	addi r27, r0, 5
	seq r18, r14, r27
	addi r1, r0, -2
	and r1, r14, r1
	stw sp+96, r1
	sub r1, r0, r18
	andi r1, r1, 16
	addi r1, r1, 33
	stw sp+56, r1
	addi r28, r0, 3
	lui r1, %hi(.LCPI174_0)
	addi r1, r1, %lo(.LCPI174_0)
	ldw r5, r1+4
	ldw r4, r1+0
	stw sp+80, r4
	stw sp+84, r5
	addi r1, r0, 32
	stw sp+68, r1
	addi r19, r0, 23
	lui r1, %hi(.L.str.149)
	addi r1, r1, %lo(.L.str.149)
	stw sp+64, r1
	lui r1, %hi(.L.str.150)
	addi r1, r1, %lo(.L.str.150)
	stw sp+52, r1
	addi r21, r0, 1
	addi r1, r0, -1
	stw sp+48, r1
	lui r1, %hi(fio_desc)
	stw sp+76, r1
	add r13, r23, r0
	add r20, r0, r0
	stw sp+60, r27
	jal r0, .LBB174_136
.LBB174_132:
	lui r16, %hi(fio_draining)
	stb r16+%lo(fio_draining), r24
	ldw r3, sp+76
	stw r3+%lo(fio_desc), r0
	stw r1+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r16+%lo(fio_draining), r0
.LBB174_133:
	ldw r5, sp+100
	ldbu r1, r5+%lo(fio_noadv)
	ldw r17, sp+104
	ldw r3, r17+%lo(fio_ibuf)
	addi r4, r0, 0
	seq r3, r3, r4
	and r1, r1, r3
	lui r16, %hi(fio_open_rec)
	stw r16+%lo(fio_open_rec), r1
	stb r5+%lo(fio_noadv), r0
	jal r31, fio_flush_line
	stw r16+%lo(fio_open_rec), r0
	stw r17+%lo(fio_ibuf), r0
	stb sp+195, r0
	addi r3, sp, 184
	addi r4, r0, 11
.LBB174_134:
	jal r31, nml_value
.LBB174_135:
	addi r20, r20, 1
	add r13, r13, fp
	beq r20, r26, .LBB174_185
.LBB174_136:
	mul r1, r20, fp
	add r1, r23, r1
	beq r14, r28, .LBB174_144
.LBB174_137:
	beq r14, r11, .LBB174_142
.LBB174_138:
	bne r14, r21, .LBB174_147
.LBB174_139:
	beq r12, r11, .LBB174_165
.LBB174_140:
	addi r3, r0, 1
	bne r12, r3, .LBB174_166
.LBB174_141:
	ldbu r25, r1+0
	jal r0, .LBB174_167
.LBB174_142:
	ldw r1, r1+0
	lui r3, %hi(.LCPI174_1)
	ldw r3, r3+%lo(.LCPI174_1)
	feq.s r3, r1, r3
	xori r3, r3, 1
	bne r3, r0, .LBB174_155
.LBB174_143:
	addi r3, sp, 184
	addi r4, r0, 32
	addi r5, r0, 15
	jal r31, memset
	lui r4, %hi(.L.str.149)
	addi r4, r4, %lo(.L.str.149)
	addi r5, r0, 3
	add r3, r15, r0
	jal r31, memcpy
	stb sp+199, r0
	jal r0, .LBB174_156
.LBB174_144:
	ldw r17, r1+4
	ldw r16, r1+0
	ldw r4, sp+80
	ldw r5, sp+84
	feq.d r1, r16, r4
	addi r3, sp, 184
	xori r1, r1, 1
	bne r1, r0, .LBB174_157
.LBB174_145:
	ldw r4, sp+68
	add r5, r19, r0
	jal r31, memset
	add r3, r15, r0
	ldw r4, sp+64
	add r5, r28, r0
	jal r31, memcpy
	stb sp+207, r0
.LBB174_146:
	addi r3, sp, 184
	add r4, r19, r0
	jal r0, .LBB174_134
.LBB174_147:
	addi r3, r0, 4
	ldw r4, sp+96
	bne r4, r3, .LBB174_151
.LBB174_148:
	bne r14, r27, .LBB174_170
.LBB174_149:
	ldw r7, r1+4
	ldw r6, r1+0
	beq r14, r27, .LBB174_171
.LBB174_150:
	ldw r1, r1+4
	fcvt.d.s r16, r1
	jal r0, .LBB174_172
.LBB174_151:
	ldw r3, sp+108
	bne r14, r3, .LBB174_175
.LBB174_152:
	beq r12, r11, .LBB174_182
.LBB174_153:
	addi r3, r0, 1
	bne r12, r3, .LBB174_183
.LBB174_154:
	ldbu r1, r1+0
	jal r0, .LBB174_184
.LBB174_155:
	stw sp+280, r1
	lui r5, %hi(.L.str.151)
	addi r5, r5, %lo(.L.str.151)
	addi r3, sp, 184
	addi r4, r0, 15
	addi r6, r0, 2
	addi r7, sp, 280
	jal r31, nml_fmt
.LBB174_156:
	addi r3, sp, 184
	addi r4, r0, 15
	jal r0, .LBB174_134
.LBB174_157:
	add r4, r19, r0
	add r5, r21, r0
	ldw r6, sp+52
	ldw r7, sp+48
	jal r31, f77_wr_begin_int
	add r3, r16, r0
	add r4, r17, r0
	jal r31, f77_wr_d
	stb r22+%lo(fio_busy), r0
	ldw r1, sp+88
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB174_161
.LBB174_158:
	ldw r1, sp+76
	ldw r3, r1+%lo(fio_desc)
	lui r1, %hi(fio_rep)
	beq r3, r0, .LBB174_160
.LBB174_159:
	ldw r3, r1+%lo(fio_rep)
	bgt r3, r0, .LBB174_161
.LBB174_160:
	lui r16, %hi(fio_draining)
	addi r3, r0, 1
	stb r16+%lo(fio_draining), r3
	ldw r3, sp+76
	stw r3+%lo(fio_desc), r0
	stw r1+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r16+%lo(fio_draining), r0
.LBB174_161:
	ldw r5, sp+100
	ldbu r1, r5+%lo(fio_noadv)
	ldw r17, sp+104
	ldw r3, r17+%lo(fio_ibuf)
	addi r4, r0, 0
	seq r3, r3, r4
	and r1, r1, r3
	lui r16, %hi(fio_open_rec)
	stw r16+%lo(fio_open_rec), r1
	stb r5+%lo(fio_noadv), r0
	jal r31, fio_flush_line
	stw r16+%lo(fio_open_rec), r0
	stw r17+%lo(fio_ibuf), r0
	stb sp+207, r0
	add r1, r0, r0
	jal r0, .LBB174_163
.LBB174_162:
	addi r1, r1, 1
	beq r1, r19, .LBB174_146
.LBB174_163:
	addi r3, sp, 184
	add r3, r3, r1
	ldbu r4, r3+0
	addi r5, r0, 69
	bne r4, r5, .LBB174_162
.LBB174_164:
	addi r4, r0, 68
	stb r3+0, r4
	jal r0, .LBB174_162
.LBB174_165:
	ldh r25, r1+0
	jal r0, .LBB174_167
.LBB174_166:
	ldw r25, r1+0
.LBB174_167:
	lui r6, %hi(.L.str.36)
	addi r6, r6, %lo(.L.str.36)
	addi r3, sp, 184
	addi r4, r0, 11
	addi r24, r0, 1
	addi r7, r0, -1
	add r5, r24, r0
	jal r31, f77_wr_begin_int
	add r3, r25, r0
	jal r31, f77_wr_i
	stb r22+%lo(fio_busy), r0
	ldw r1, sp+88
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB174_133
.LBB174_168:
	ldw r1, sp+76
	ldw r3, r1+%lo(fio_desc)
	lui r1, %hi(fio_rep)
	beq r3, r0, .LBB174_132
.LBB174_169:
	ldw r3, r1+%lo(fio_rep)
	bgt r3, r0, .LBB174_133
	jal r0, .LBB174_132
.LBB174_170:
	ldw r3, r1+0
	fcvt.d.s r6, r3
	bne r14, r27, .LBB174_150
.LBB174_171:
	ldw r17, r1+12
	ldw r16, r1+8
.LBB174_172:
	addi r1, r0, 40
	stb sp+280, r1
	addi r24, sp, 112
	add r3, r24, r0
	add r5, r6, r0
	add r6, r7, r0
	add r4, r18, r0
	jal r31, nml_real
	addi r25, sp, 280
	addi r26, r0, 1
	add r3, r25, r0
	add r4, r26, r0
	add r5, r24, r0
	jal r31, nml_trim_cat
	addi r27, r1, 1
	add r1, r25, r1
	addi r3, r0, 44
	stb r1+0, r3
	add r3, r24, r0
	add r5, r16, r0
	add r6, r17, r0
	add r4, r18, r0
	jal r31, nml_real
	add r3, r25, r0
	add r4, r27, r0
	add r5, r24, r0
	jal r31, nml_trim_cat
	addi r5, r1, 1
	add r1, r25, r1
	addi r3, r0, 41
	stb r1+0, r3
	ldw r16, sp+56
	sub r24, r16, r5
	blt r24, r26, .LBB174_174
.LBB174_173:
	addi r25, sp, 280
	add r3, r25, r24
	add r4, r25, r0
	jal r31, memmove
	addi r4, r0, 32
	add r3, r25, r0
	add r5, r24, r0
	jal r31, memset
	add r5, r16, r0
.LBB174_174:
	ldw r26, sp+92
	addi r3, sp, 280
	add r4, r5, r0
	jal r31, nml_value
	ldw r27, sp+60
	jal r0, .LBB174_135
.LBB174_175:
	addi r1, r0, 39
	stb sp+280, r1
	add r5, r21, r0
	blt r12, r21, .LBB174_181
.LBB174_176:
	addi r5, r0, 1
	add r3, r0, r0
	jal r0, .LBB174_179
.LBB174_177:
	add r5, r4, r0
	addi r3, r3, 1
	bge r3, r12, .LBB174_181
.LBB174_178:
	addi r4, r0, 1021
	bge r5, r4, .LBB174_181
.LBB174_179:
	add r4, r13, r3
	ldbu r6, r4+0
	addi r4, r5, 1
	addi r7, sp, 280
	add r7, r7, r5
	stb r7+0, r6
	bne r6, r1, .LBB174_177
.LBB174_180:
	addi r5, r5, 2
	addi r6, sp, 280
	add r4, r6, r4
	stb r4+0, r1
	addi r3, r3, 1
	blt r3, r12, .LBB174_178
.LBB174_181:
	addi r4, r5, 1
	addi r3, sp, 280
	add r5, r3, r5
	stb r5+0, r1
	jal r0, .LBB174_134
.LBB174_182:
	ldh r1, r1+0
	jal r0, .LBB174_184
.LBB174_183:
	ldw r1, r1+0
.LBB174_184:
	addi r3, r0, 0
	seq r1, r1, r3
	lui r3, %hi(.L.str.37)
	addi r3, r3, %lo(.L.str.37)
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r3, r3, r1
	addi r4, r0, 1
	jal r0, .LBB174_134
.LBB174_185:
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
.LBB174_186:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end174:
	.size	f77_nml_item, .Lfunc_end174-f77_nml_item
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI174_0,@object
.LJTI174_0:
	.word	.LBB174_79
	.word	.LBB174_93
	.word	.LBB174_87
	.word	.LBB174_88
	.size	.LJTI174_0, 16
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function nml_fmt
	.type	nml_fmt,@function
nml_fmt:                                # @nml_fmt
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	stw sp+24, r16
	add r14, r7, r0
	add r15, r6, r0
	add r6, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	addi r13, r0, 1
	addi r7, r0, -1
	add r5, r13, r0
	jal r31, f77_wr_begin_int
	addi r1, r0, 2
	lui r16, %hi(fio_listed)
	beq r15, r1, .LBB175_3
.LBB175_1:
	bne r15, r13, .LBB175_5
.LBB175_2:
	ldw r3, r14+0
	jal r31, f77_wr_i
	jal r0, .LBB175_6
.LBB175_3:
	ldw r14, r14+0
	stw sp+20, r14
	ldw r1, r16+%lo(fio_listed)
	beq r1, r0, .LBB175_11
.LBB175_4:
	fcvt.d.s r4, r14
	add r3, r4, r0
	add r4, r5, r0
	add r5, r0, r0
	jal r31, fio_list_out_real
	jal r0, .LBB175_6
.LBB175_5:
	ldw r4, r14+4
	ldw r3, r14+0
	jal r31, f77_wr_d
.LBB175_6:
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	ldw r1, r16+%lo(fio_listed)
	bne r1, r0, .LBB175_10
.LBB175_7:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB175_9
.LBB175_8:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB175_10
.LBB175_9:
	lui r14, %hi(fio_draining)
	stb r14+%lo(fio_draining), r13
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r14+%lo(fio_draining), r0
.LBB175_10:
	lui r1, %hi(fio_noadv)
	ldbu r3, r1+%lo(fio_noadv)
	lui r13, %hi(fio_ibuf)
	ldw r4, r13+%lo(fio_ibuf)
	addi r5, r0, 0
	seq r4, r4, r5
	and r3, r3, r4
	lui r14, %hi(fio_open_rec)
	stw r14+%lo(fio_open_rec), r3
	stb r1+%lo(fio_noadv), r0
	jal r31, fio_flush_line
	stw r14+%lo(fio_open_rec), r0
	stw r13+%lo(fio_ibuf), r0
	add r1, r12, r11
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
.LBB175_11:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB175_15
.LBB175_12:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB175_14
.LBB175_13:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	bgt r4, r0, .LBB175_15
.LBB175_14:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
.LBB175_15:
	addi r3, r0, 65
	bne r1, r3, .LBB175_17
.LBB175_16:
	addi r3, sp, 20
	addi r4, r0, 4
	jal r31, fio_hol_out
	jal r0, .LBB175_6
.LBB175_17:
	lui r15, %hi(fio_single)
	stb r15+%lo(fio_single), r13
	fcvt.d.s r4, r14
	add r3, r4, r0
	add r4, r5, r0
	jal r31, f77_wr_d
	stb r15+%lo(fio_single), r0
	jal r0, .LBB175_6
.Lfunc_end175:
	.size	nml_fmt, .Lfunc_end175-nml_fmt
                                        # -- End function
	.p2align	2                               # -- Begin function nml_value
	.type	nml_value,@function
nml_value:                              # @nml_value
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
	lui r15, %hi(nml_npend)
	ldw r1, r15+%lo(nml_npend)
	blt r1, r0, .LBB176_5
.LBB176_1:
	addi r14, r1, 1
	stw r15+%lo(nml_npend), r14
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	add r1, r1, r3
	addi r3, r0, 44
	stb r1+0, r3
	lui r16, %hi(nml_len)
	ldw r4, r16+%lo(nml_len)
	addi r1, r0, 2
	lui r13, %hi(nml_line)
	addi r13, r13, %lo(nml_line)
	blt r4, r1, .LBB176_4
.LBB176_2:
	add r1, r4, r14
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB176_4
.LBB176_3:
	add r3, r13, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r13+0, r1
	addi r1, r0, 1
	stw r16+%lo(nml_len), r1
.LBB176_4:
	ldw r1, r16+%lo(nml_len)
	add r3, r1, r14
	addi r4, r0, 1024
	sgt r3, r3, r4
	sub r4, r4, r1
	xor r4, r4, r14
	sub r3, r0, r3
	and r3, r4, r3
	xor r14, r14, r3
	add r3, r1, r13
	lui r4, %hi(nml_pend)
	addi r4, r4, %lo(nml_pend)
	add r5, r14, r0
	jal r31, memcpy
	ldw r1, r16+%lo(nml_len)
	add r1, r14, r1
	stw r16+%lo(nml_len), r1
	addi r1, r0, -1
	stw r15+%lo(nml_npend), r1
.LBB176_5:
	addi r1, r0, 1022
	slt r1, r12, r1
	sub r1, r0, r1
	xori r3, r12, 1022
	and r1, r3, r1
	xori r12, r1, 1022
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	add r4, r11, r0
	add r5, r12, r0
	jal r31, memcpy
	stw r15+%lo(nml_npend), r12
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end176:
	.size	nml_value, .Lfunc_end176-nml_value
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function nml_real
	.type	.LCPI177_0,@object
.LCPI177_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI177_0, 8
	.text
	.p2align	2
	.type	nml_real,@function
nml_real:                               # @nml_real
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	add r7, r6, r0
	add r6, r5, r0
	add r11, r3, r0
	stw sp+36, r7
	stw sp+32, r5
	addi r1, r0, 0
	seq r1, r4, r1
	sub r1, r0, r1
	andi r1, r1, 24
	xori r12, r1, 23
	lui r1, %hi(.LCPI177_0)
	addi r1, r1, %lo(.LCPI177_0)
	ldw r9, r1+4
	ldw r8, r1+0
	feq.d r1, r6, r8
	xori r1, r1, 1
	bne r1, r0, .LBB177_3
.LBB177_1:
	addi r4, r0, 32
	add r3, r11, r0
	add r5, r12, r0
	jal r31, memset
	addi r3, r11, 1
	lui r4, %hi(.L.str.149)
	addi r4, r4, %lo(.L.str.149)
	addi r5, r0, 3
	jal r31, memcpy
	add r1, r11, r12
	stb r1+0, r0
.LBB177_2:
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB177_3:
	beq r4, r0, .LBB177_8
.LBB177_4:
	lui r5, %hi(.L.str.150)
	addi r5, r5, %lo(.L.str.150)
	addi r6, r0, 3
	addi r7, sp, 32
	add r3, r11, r0
	add r4, r12, r0
	jal r31, nml_fmt
	addi r1, r0, 69
	addi r3, r0, 68
	jal r0, .LBB177_6
.LBB177_5:
	addi r12, r12, -1
	addi r11, r11, 1
	beq r12, r0, .LBB177_2
.LBB177_6:
	ldbu r4, r11+0
	bne r4, r1, .LBB177_5
.LBB177_7:
	stb r11+0, r3
	jal r0, .LBB177_5
.LBB177_8:
	fcvt.s.d r1, r6
	stw sp+28, r1
	lui r5, %hi(.L.str.151)
	addi r5, r5, %lo(.L.str.151)
	addi r6, r0, 2
	addi r7, sp, 28
	add r3, r11, r0
	add r4, r12, r0
	jal r31, nml_fmt
	jal r0, .LBB177_2
.Lfunc_end177:
	.size	nml_real, .Lfunc_end177-nml_real
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
	blt r13, r1, .LBB178_4
.LBB178_1:
	add r1, r14, r16
	ldbu r1, r1+0
	bne r1, r17, .LBB178_4
.LBB178_2:
	addi r16, r16, 1
	bne r13, r16, .LBB178_1
.LBB178_3:
	add r16, r13, r0
.LBB178_4:
	sub r1, r13, r16
	addi r3, r0, 3
	add r15, r14, r16
	blt r1, r3, .LBB178_6
.LBB178_5:
	lui r4, %hi(.L.str.149)
	addi r4, r4, %lo(.L.str.149)
	addi r5, r0, 3
	add r3, r15, r0
	jal r31, memcmp
	beq r1, r0, .LBB178_11
.LBB178_6:
	ble r13, r16, .LBB178_12
.LBB178_7:
	addi r1, r13, -1
	xor r3, r16, r1
	slt r4, r16, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	addi r3, r14, -1
.LBB178_8:
	add r4, r3, r13
	ldbu r4, r4+0
	bne r4, r17, .LBB178_12
.LBB178_9:
	addi r13, r13, -1
	bgt r13, r16, .LBB178_8
.LBB178_10:
	add r13, r1, r0
	jal r0, .LBB178_12
.LBB178_11:
	addi r1, r16, 3
	bne r1, r13, .LBB178_13
.LBB178_12:
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
.LBB178_13:
	add r1, r14, r1
	ldbu r1, r1+0
	bne r1, r17, .LBB178_6
	jal r0, .LBB178_12
.Lfunc_end178:
	.size	nml_trim_cat, .Lfunc_end178-nml_trim_cat
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
	beq r1, r0, .LBB179_6
.LBB179_1:
	lui r11, %hi(nml_status)
	ldw r1, r11+%lo(nml_status)
	beq r1, r0, .LBB179_14
.LBB179_2:
	ldw r1, r11+%lo(nml_status)
	addi r3, r0, 2
	beq r1, r3, .LBB179_18
.LBB179_3:
	addi r3, r0, 1
	bne r1, r3, .LBB179_19
.LBB179_4:
	lui r1, %hi(nml_flags)
	ldbu r1, r1+%lo(nml_flags)
	andi r4, r1, 1
	add r1, r3, r0
	bne r4, r0, .LBB179_19
.LBB179_5:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r1, %hi(nml_unit)
	ldw r5, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	lui r6, %hi(nml_grp)
	addi r6, r6, %lo(nml_grp)
	jal r0, .LBB179_23
.LBB179_6:
	lui r14, %hi(nml_npend)
	ldw r12, r14+%lo(nml_npend)
	lui r13, %hi(nml_len)
	lui r11, %hi(nml_line)
	addi r11, r11, %lo(nml_line)
	blt r12, r0, .LBB179_11
.LBB179_7:
	ldw r4, r13+%lo(nml_len)
	addi r1, r0, 2
	blt r4, r1, .LBB179_10
.LBB179_8:
	add r1, r4, r12
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB179_10
.LBB179_9:
	add r3, r11, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r11+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
.LBB179_10:
	ldw r1, r13+%lo(nml_len)
	add r3, r1, r12
	addi r4, r0, 1024
	sgt r3, r3, r4
	sub r4, r4, r1
	xor r4, r4, r12
	sub r3, r0, r3
	and r3, r4, r3
	xor r12, r12, r3
	add r3, r1, r11
	lui r4, %hi(nml_pend)
	addi r4, r4, %lo(nml_pend)
	add r5, r12, r0
	jal r31, memcpy
	ldw r1, r13+%lo(nml_len)
	add r1, r12, r1
	stw r13+%lo(nml_len), r1
	addi r1, r0, -1
	stw r14+%lo(nml_npend), r1
.LBB179_11:
	ldw r4, r13+%lo(nml_len)
	addi r1, r0, 2
	blt r4, r1, .LBB179_13
.LBB179_12:
	lui r3, %hi(nml_line)
	addi r3, r3, %lo(nml_line)
	jal r31, nml_record
.LBB179_13:
	addi r1, r0, 32
	stb r11+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
	lui r3, %hi(.L.str.42)
	addi r3, r3, %lo(.L.str.42)
	addi r4, r0, 5
	jal r31, nml_record
	add r1, r0, r0
	jal r0, .LBB179_19
.LBB179_14:
	lui r1, %hi(nml_nasg)
	ldw r1, r1+%lo(nml_nasg)
	addi r3, r0, 1
	blt r1, r3, .LBB179_2
.LBB179_15:
	lui r3, %hi(nml_asg)
	ldw r5, r3+%lo(nml_asg)
.LBB179_16:
	ldw r3, r5+72
	beq r3, r0, .LBB179_20
.LBB179_17:
	addi r5, r5, 76
	addi r1, r1, -1
	bne r1, r0, .LBB179_16
	jal r0, .LBB179_2
.LBB179_18:
	lui r3, %hi(nml_flags)
	ldbu r3, r3+%lo(nml_flags)
	andi r3, r3, 2
	beq r3, r0, .LBB179_22
.LBB179_19:
	ldw r14, sp+112
	ldw r13, sp+116
	ldw r12, sp+120
	ldw r11, sp+124
	ldw lr, sp+0
	addi sp, sp, 128
	jalr r0, r31, 0
.LBB179_20:
	lui r4, %hi(.L.str.43)
	addi r4, r4, %lo(.L.str.43)
	addi r3, sp, 16
	jal r31, sprintf
	ldw r1, r11+%lo(nml_status)
	bne r1, r0, .LBB179_2
.LBB179_21:
	addi r1, r0, 2
	stw r11+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	addi r4, sp, 16
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB179_2
.LBB179_22:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r1, %hi(nml_unit)
	ldw r6, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	lui r5, %hi(nml_grp)
	addi r5, r5, %lo(nml_grp)
	lui r7, %hi(nml_err)
	addi r7, r7, %lo(nml_err)
.LBB179_23:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end179:
	.size	f77_nml_end, .Lfunc_end179-f77_nml_end
                                        # -- End function
	.globl	f77_ipow                        # -- Begin function f77_ipow
	.p2align	2
	.type	f77_ipow,@function
f77_ipow:                               # @f77_ipow
# %bb.0:
	addi r1, r0, 1
	blt r4, r0, .LBB180_4
.LBB180_1:
	beq r4, r0, .LBB180_9
.LBB180_2:
	addi r5, r0, 0
.LBB180_3:
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
	bne r4, r0, .LBB180_3
	jal r0, .LBB180_9
.LBB180_4:
	beq r3, r1, .LBB180_7
.LBB180_5:
	addi r1, r0, -1
	bne r3, r1, .LBB180_8
.LBB180_6:
	andi r1, r4, 1
	addi r3, r0, 0
	seq r1, r1, r3
	addi r1, r1, -1
	ori  r1, r1, 1
	jal r0, .LBB180_9
.LBB180_7:
	add r1, r3, r0
	jal r0, .LBB180_9
.LBB180_8:
	add r1, r0, r0
.LBB180_9:
	jalr r0, r31, 0
.Lfunc_end180:
	.size	f77_ipow, .Lfunc_end180-f77_ipow
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_dpow_i
	.type	.LCPI181_0,@object
.LCPI181_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI181_0, 8
	.type	.LCPI181_1,@object
.LCPI181_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI181_1, 8
	.type	.LCPI181_2,@object
.LCPI181_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI181_2, 8
	.type	.LCPI181_3,@object
.LCPI181_3:
	.quad	0x7fefdcf158adbb99              # double 1.79E+308
	.size	.LCPI181_3, 8
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
	blt r5, r0, .LBB181_3
.LBB181_1:
	andi r3, r5, 1
	addi r1, r0, 0
	seq r3, r3, r1
	lui r4, %hi(.LCPI181_0)
	addi r4, r4, %lo(.LCPI181_0)
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
	beq r3, r0, .LBB181_16
.LBB181_2:
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
	bne r3, r0, .LBB181_2
	jal r0, .LBB181_16
.LBB181_3:
	sub r1, r0, r5
	lui r3, %hi(.LCPI181_0)
	addi r3, r3, %lo(.LCPI181_0)
	ldw r5, r3+4
	ldw r4, r3+0
	lui r3, %hi(.LCPI181_1)
	addi r3, r3, %lo(.LCPI181_1)
	ldw r9, r3+4
	ldw r8, r3+0
	lui r3, %hi(.LCPI181_3)
	addi r3, r3, %lo(.LCPI181_3)
	ldw r11, r3+4
	ldw r10, r3+0
	lui r3, %hi(.LCPI181_2)
	addi r3, r3, %lo(.LCPI181_2)
	ldw r15, r3+4
	ldw r14, r3+0
	add r16, r6, r0
	add r17, r7, r0
	add r3, r1, r0
	add r12, r4, r0
	add r13, r5, r0
	jal r0, .LBB181_5
.LBB181_4:
	fmul.d r16, r16, r16
.LBB181_5:
	andi lr, r3, 1
	fneg.d r18, r16
	beq lr, r0, .LBB181_10
.LBB181_6:
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
	bne lr, r0, .LBB181_9
.LBB181_7:
	fle.d lr, r20, r4
	bne lr, r0, .LBB181_9
.LBB181_8:
	fle.d lr, r20, r14
	xori lr, lr, 1
	fle.d r20, r22, r14
	xori r20, r20, 1
	or  lr, r20, lr
	bne lr, r0, .LBB181_13
.LBB181_9:
	fmul.d r12, r16, r12
.LBB181_10:
	srli r3, r3, 1
	beq r3, r0, .LBB181_15
.LBB181_11:
	flt.d lr, r16, r8
	xor r20, r19, r17
	sub lr, r0, lr
	and r20, r20, lr
	xor r21, r17, r20
	xor r18, r18, r16
	and lr, r18, lr
	xor r20, r16, lr
	fle.d lr, r20, r14
	bne lr, r0, .LBB181_4
.LBB181_12:
	fdiv.d r18, r10, r20
	fle.d lr, r20, r18
	xori lr, lr, 1
	beq lr, r0, .LBB181_4
.LBB181_13:
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
	beq r1, r0, .LBB181_16
.LBB181_14:
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
	bne r1, r0, .LBB181_14
	jal r0, .LBB181_16
.LBB181_15:
	fdiv.d r8, r4, r12
.LBB181_16:
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
.Lfunc_end181:
	.size	f77_dpow_i, .Lfunc_end181-f77_dpow_i
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rpow_i
	.type	.LCPI182_0,@object
.LCPI182_0:
	.word	0x3f800000                      # float 1
	.size	.LCPI182_0, 4
	.type	.LCPI182_1,@object
.LCPI182_1:
	.word	0x00000000                      # float 0
	.size	.LCPI182_1, 4
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0
	.type	.LCPI182_2,@object
.LCPI182_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI182_2, 8
	.type	.LCPI182_3,@object
.LCPI182_3:
	.quad	0x47eff933c78cdfad              # double 3.4E+38
	.size	.LCPI182_3, 8
	.type	.LCPI182_4,@object
.LCPI182_4:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI182_4, 8
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
	blt r4, r0, .LBB182_3
.LBB182_1:
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
	srli r4, r4, 1
	beq r4, r0, .LBB182_17
.LBB182_2:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	srli r4, r4, 1
	bne r4, r0, .LBB182_2
	jal r0, .LBB182_17
.LBB182_3:
	sub r4, r0, r4
	lui r1, %hi(.LCPI182_0)
	ldw r5, r1+%lo(.LCPI182_0)
	lui r6, %hi(.LCPI182_1)
	ldw r6, r6+%lo(.LCPI182_1)
	lui r7, %hi(.LCPI182_3)
	addi r7, r7, %lo(.LCPI182_3)
	ldw r9, r7+4
	ldw r8, r7+0
	lui r7, %hi(.LCPI182_2)
	addi r7, r7, %lo(.LCPI182_2)
	ldw r11, r7+4
	ldw r10, r7+0
	add r7, r3, r0
	add lr, r4, r0
	jal r0, .LBB182_5
.LBB182_4:
	fmul.s r7, r7, r7
.LBB182_5:
	andi r15, lr, 1
	fcvt.d.s r12, r7
	flt.s r14, r7, r6
	beq r15, r0, .LBB182_10
.LBB182_6:
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
	bne r15, r0, .LBB182_9
.LBB182_7:
	lui r15, %hi(.LCPI182_4)
	addi r15, r15, %lo(.LCPI182_4)
	ldw r21, r15+4
	ldw r20, r15+0
	fle.d r15, r16, r20
	bne r15, r0, .LBB182_9
.LBB182_8:
	fle.d r15, r16, r10
	xori r15, r15, 1
	fle.d r16, r18, r10
	xori r16, r16, 1
	or  r15, r16, r15
	bne r15, r0, .LBB182_13
.LBB182_9:
	fmul.s r5, r7, r5
.LBB182_10:
	srli lr, lr, 1
	beq lr, r0, .LBB182_16
.LBB182_11:
	fneg.d r16, r12
	xor r15, r17, r13
	sub r18, r0, r14
	and r14, r15, r18
	xor r15, r13, r14
	xor r16, r16, r12
	and r16, r16, r18
	xor r14, r12, r16
	fle.d r12, r14, r10
	bne r12, r0, .LBB182_4
.LBB182_12:
	fdiv.d r12, r8, r14
	fle.d r12, r14, r12
	xori r12, r12, 1
	beq r12, r0, .LBB182_4
.LBB182_13:
	ldw r1, r1+%lo(.LCPI182_0)
	fdiv.s r3, r1, r3
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
.LBB182_14:
	srli r4, r4, 1
	beq r4, r0, .LBB182_17
.LBB182_15:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	jal r0, .LBB182_14
.LBB182_16:
	ldw r1, r1+%lo(.LCPI182_0)
	fdiv.s r1, r1, r5
.LBB182_17:
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
.Lfunc_end182:
	.size	f77_rpow_i, .Lfunc_end182-f77_rpow_i
                                        # -- End function
	.globl	f77_open_f90                    # -- Begin function f77_open_f90
	.p2align	2
	.type	f77_open_f90,@function
f77_open_f90:                           # @f77_open_f90
# %bb.0:
	lui r1, %hi(fio_o_pos)
	stw r1+%lo(fio_o_pos), r3
	lui r1, %hi(fio_o_poslen)
	stw r1+%lo(fio_o_poslen), r4
	lui r1, %hi(fio_o_act)
	stw r1+%lo(fio_o_act), r5
	lui r1, %hi(fio_o_actlen)
	stw r1+%lo(fio_o_actlen), r6
	lui r1, %hi(fio_o_pad)
	stw r1+%lo(fio_o_pad), r7
	lui r1, %hi(fio_o_padlen)
	stw r1+%lo(fio_o_padlen), r8
	lui r1, %hi(fio_o_delim)
	stw r1+%lo(fio_o_delim), r9
	lui r1, %hi(fio_o_delimlen)
	stw r1+%lo(fio_o_delimlen), r10
	jalr r0, r31, 0
.Lfunc_end183:
	.size	f77_open_f90, .Lfunc_end183-f77_open_f90
                                        # -- End function
	.globl	f77_open                        # -- Begin function f77_open
	.p2align	2
	.type	f77_open,@function
f77_open:                               # @f77_open
# %bb.0:
	addi sp, sp, -352
	stw sp+0, lr
	stw sp+348, r11
	stw sp+344, r12
	stw sp+340, r13
	stw sp+336, r14
	stw sp+332, r15
	stw sp+328, r16
	stw sp+324, r17
	stw sp+320, r18
	stw sp+316, r19
	stw sp+312, r20
	stw sp+308, r21
	stw sp+304, r22
	stw sp+300, r23
	stw sp+296, r24
	stw sp+292, r25
	stw sp+288, r26
	stw sp+284, r27
	stw sp+280, r28
	lui r1, %hi(fio_uend_init)
	ldbu lr, r1+%lo(fio_uend_init)
	bne lr, r0, .LBB184_4
.LBB184_1:
	addi lr, r0, -400
	lui r11, %hi(fio_uend+400)
	addi r11, r11, %lo(fio_uend+400)
	addi r12, r0, -1
.LBB184_2:
	add r13, lr, r11
	stw r13+0, r12
	addi lr, lr, 4
	bne lr, r0, .LBB184_2
.LBB184_3:
	addi lr, r0, 1
	stb r1+%lo(fio_uend_init), lr
.LBB184_4:
	ldw r15, sp+368
	addi r1, r3, -100
	addi lr, r0, -99
	bltu r1, lr, .LBB184_14
.LBB184_5:
	addi r1, r3, -5
	addi r11, r0, 1
	bleu r1, r11, .LBB184_14
.LBB184_6:
	ldw r1, sp+352
	beq r8, r0, .LBB184_16
.LBB184_7:
	addi r12, r0, 1
	add lr, r11, r0
	add r16, r0, r0
	blt r9, r12, .LBB184_23
.LBB184_8:
	addi lr, r0, 6
	lui r13, %hi(.L.str.47)
	addi r13, r13, %lo(.L.str.47)
	addi r14, r0, 26
	add r16, r0, r0
.LBB184_9:
	beq r16, lr, .LBB184_17
.LBB184_10:
	add r17, r16, r13
	ldb r17, r17+0
	add r18, r8, r16
	ldb r18, r18+0
	addi r19, r18, -97
	andi r19, r19, 255
	sltu r19, r19, r14
	addi r20, r18, -32
	xor r20, r20, r18
	sub r19, r0, r19
	and r19, r20, r19
	xor r18, r18, r19
	bne r18, r17, .LBB184_13
.LBB184_11:
	addi r16, r16, 1
	bne r9, r16, .LBB184_9
.LBB184_12:
	add r13, r9, r0
	beq r13, lr, .LBB184_18
.LBB184_13:
	add lr, r0, r0
	add r16, r0, r0
	jal r0, .LBB184_23
.LBB184_14:
	bne r15, r0, .LBB184_96
.LBB184_15:
	lui r1, %hi(stderr)
	ldw r1, r1+%lo(stderr)
	lui r4, %hi(.L.str.46)
	addi r4, r4, %lo(.L.str.46)
	add r5, r3, r0
	add r3, r1, r0
	jal r0, .LBB184_241
.LBB184_16:
	add lr, r11, r0
	add r16, r0, r0
	jal r0, .LBB184_23
.LBB184_17:
	add r13, lr, r0
	bne r13, lr, .LBB184_13
.LBB184_18:
	addi r13, r0, 7
	add lr, r12, r0
	add r16, r12, r0
	blt r9, r13, .LBB184_23
.LBB184_19:
	addi r17, r9, -7
	addi r12, r8, 6
	addi r13, r0, 32
	addi r14, r0, 1
.LBB184_20:
	ldbu r18, r12+0
	seq lr, r18, r13
	addi r16, r17, -1
	sltu r17, r16, r17
	bne r17, r14, .LBB184_22
.LBB184_21:
	addi r12, r12, 1
	add r17, r16, r0
	add r16, lr, r0
	beq r18, r13, .LBB184_20
	jal r0, .LBB184_23
.LBB184_22:
	add r16, lr, r0
.LBB184_23:
	addi r24, r0, 0
	seq r12, r10, r24
	slt r11, r1, r11
	or  r11, r12, r11
	add r17, r16, r0
	beq r11, r0, .LBB184_25
.LBB184_24:
	bne lr, r0, .LBB184_47
	jal r0, .LBB184_36
.LBB184_25:
	addi r12, r0, 11
	lui r13, %hi(.L.str.48)
	addi r13, r13, %lo(.L.str.48)
	addi r14, r0, 26
	add r17, r0, r0
.LBB184_26:
	beq r17, r12, .LBB184_30
.LBB184_27:
	add r18, r17, r13
	ldb r18, r18+0
	add r19, r10, r17
	ldb r19, r19+0
	addi r20, r19, -97
	andi r20, r20, 255
	sltu r20, r20, r14
	addi r21, r19, -32
	xor r21, r21, r19
	sub r20, r0, r20
	and r20, r21, r20
	xor r19, r19, r20
	bne r19, r18, .LBB184_35
.LBB184_28:
	addi r17, r17, 1
	bne r1, r17, .LBB184_26
.LBB184_29:
	add r13, r1, r0
	beq r13, r12, .LBB184_31
	jal r0, .LBB184_35
.LBB184_30:
	add r13, r12, r0
	bne r13, r12, .LBB184_35
.LBB184_31:
	addi r17, r0, 1
	addi r12, r0, 12
	blt r1, r12, .LBB184_24
.LBB184_32:
	addi r12, r10, 11
	addi r13, r1, -11
	addi r14, r0, 32
.LBB184_33:
	ldbu r18, r12+0
	bne r18, r14, .LBB184_35
.LBB184_34:
	addi r12, r12, 1
	addi r13, r13, -1
	bne r13, r0, .LBB184_33
	jal r0, .LBB184_24
.LBB184_35:
	add r17, r0, r0
	bne lr, r0, .LBB184_47
.LBB184_36:
	addi r12, r0, 1
	blt r9, r12, .LBB184_73
.LBB184_37:
	addi r12, r0, 10
	lui r13, %hi(.L.str.49)
	addi r13, r13, %lo(.L.str.49)
	addi r14, r0, 26
	add r18, r0, r0
.LBB184_38:
	beq r18, r12, .LBB184_42
.LBB184_39:
	add r19, r18, r13
	ldb r19, r19+0
	add r20, r8, r18
	ldb r20, r20+0
	addi r21, r20, -97
	andi r21, r21, 255
	sltu r21, r21, r14
	addi r22, r20, -32
	xor r22, r22, r20
	sub r21, r0, r21
	and r21, r22, r21
	xor r20, r20, r21
	bne r20, r19, .LBB184_73
.LBB184_40:
	addi r18, r18, 1
	bne r9, r18, .LBB184_38
.LBB184_41:
	add r13, r9, r0
	beq r13, r12, .LBB184_43
	jal r0, .LBB184_73
.LBB184_42:
	add r13, r12, r0
	bne r13, r12, .LBB184_73
.LBB184_43:
	addi r12, r0, 11
	blt r9, r12, .LBB184_47
.LBB184_44:
	addi r12, r8, 10
	addi r13, r9, -10
	addi r14, r0, 32
.LBB184_45:
	ldbu r18, r12+0
	bne r18, r14, .LBB184_73
.LBB184_46:
	addi r12, r12, 1
	addi r13, r13, -1
	bne r13, r0, .LBB184_45
.LBB184_47:
	or  r11, r11, r17
	beq r11, r0, .LBB184_58
.LBB184_48:
	ldw r19, sp+364
	sne r1, r6, r24
	sgt r8, r7, r24
	and r26, r1, r8
	addi r18, r0, 1
	bne r26, r18, .LBB184_54
.LBB184_49:
	addi r1, r0, 7
	lui r8, %hi(.L.str.54)
	addi r8, r8, %lo(.L.str.54)
	addi r9, r0, 26
	add r10, r0, r0
.LBB184_50:
	beq r10, r1, .LBB184_98
.LBB184_51:
	add lr, r10, r8
	ldb lr, lr+0
	add r11, r6, r10
	ldb r11, r11+0
	addi r12, r11, -97
	andi r12, r12, 255
	sltu r12, r12, r9
	addi r13, r11, -32
	xor r13, r13, r11
	sub r12, r0, r12
	and r12, r13, r12
	xor r11, r11, r12
	bne r11, lr, .LBB184_54
.LBB184_52:
	addi r10, r10, 1
	bne r7, r10, .LBB184_50
.LBB184_53:
	add r8, r7, r0
	beq r8, r1, .LBB184_99
.LBB184_54:
	add r21, r0, r0
.LBB184_55:
	sgt r1, r19, r24
	xori r8, r16, 1
	or  r1, r1, r8
	bne r1, r0, .LBB184_63
.LBB184_56:
	bne r15, r0, .LBB184_96
.LBB184_57:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	jal r0, .LBB184_241
.LBB184_58:
	addi r11, r0, 9
	lui r12, %hi(.L.str.50)
	addi r12, r12, %lo(.L.str.50)
	addi r13, r0, 26
	add r14, r0, r0
.LBB184_59:
	beq r14, r11, .LBB184_68
.LBB184_60:
	add r18, r14, r12
	ldb r18, r18+0
	add r19, r10, r14
	ldb r19, r19+0
	addi r20, r19, -97
	andi r20, r20, 255
	sltu r20, r20, r13
	addi r21, r19, -32
	xor r21, r21, r19
	sub r20, r0, r20
	and r20, r21, r20
	xor r19, r19, r20
	bne r19, r18, .LBB184_73
.LBB184_61:
	addi r14, r14, 1
	bne r1, r14, .LBB184_59
.LBB184_62:
	add r12, r1, r0
	bne r12, r11, .LBB184_73
	jal r0, .LBB184_69
.LBB184_63:
	beq r4, r0, .LBB184_81
.LBB184_64:
	addi r20, r0, 1
	blt r5, r20, .LBB184_81
.LBB184_65:
	addi r1, r0, 256
	blt r5, r1, .LBB184_109
.LBB184_66:
	bne r15, r0, .LBB184_96
.LBB184_67:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	jal r0, .LBB184_241
.LBB184_68:
	add r12, r11, r0
	bne r12, r11, .LBB184_73
.LBB184_69:
	addi r11, r0, 10
	blt r1, r11, .LBB184_48
.LBB184_70:
	addi r11, r10, 9
	addi r12, r1, -9
	addi r13, r0, 32
.LBB184_71:
	ldbu r14, r11+0
	bne r14, r13, .LBB184_73
.LBB184_72:
	addi r11, r11, 1
	addi r12, r12, -1
	bne r12, r0, .LBB184_71
	jal r0, .LBB184_48
.LBB184_73:
	beq lr, r0, .LBB184_75
.LBB184_74:
	add r4, r0, r0
	jal r0, .LBB184_95
.LBB184_75:
	addi r4, r0, 1
	blt r9, r4, .LBB184_95
.LBB184_76:
	addi r3, r0, 10
	lui r5, %hi(.L.str.49)
	addi r5, r5, %lo(.L.str.49)
	addi r6, r0, 26
	add r7, r0, r0
.LBB184_77:
	beq r7, r3, .LBB184_89
.LBB184_78:
	add lr, r7, r5
	ldb lr, lr+0
	add r11, r8, r7
	ldb r11, r11+0
	addi r12, r11, -97
	andi r12, r12, 255
	sltu r12, r12, r6
	addi r13, r11, -32
	xor r13, r13, r11
	sub r12, r0, r12
	and r12, r13, r12
	xor r11, r11, r12
	bne r11, lr, .LBB184_95
.LBB184_79:
	addi r7, r7, 1
	bne r9, r7, .LBB184_77
.LBB184_80:
	add r5, r9, r0
	jal r0, .LBB184_90
.LBB184_81:
	add r13, r6, r0
	add r14, r7, r0
	lui r5, %hi(.L.str.57)
	addi r5, r5, %lo(.L.str.57)
	addi r1, sp, 24
	addi r4, r0, 256
	add r6, r3, r0
	add r3, r1, r0
	add r22, r6, r0
	jal r31, snprintf
.LBB184_82:
	ldw r11, sp+360
	ldw r12, sp+356
	slli r20, r22, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r25, r20, r1
	ldw r1, r25+0
	slli r22, r22, 8
	lui r23, %hi(fio_ufname)
	addi r23, r23, %lo(fio_ufname)
	beq r1, r0, .LBB184_85
.LBB184_83:
	add r3, r22, r23
	addi r4, sp, 24
	jal r31, strcmp
	sne r1, r1, r24
	or  r1, r21, r1
	beq r1, r0, .LBB184_112
.LBB184_84:
	ldw r3, r25+0
	jal r31, fclose
	stw r25+0, r0
.LBB184_85:
	beq r21, r0, .LBB184_103
.LBB184_86:
	jal r31, tmpfile
	bne r1, r0, .LBB184_141
.LBB184_87:
	bne r15, r0, .LBB184_96
.LBB184_88:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.59)
	addi r4, r4, %lo(.L.str.59)
	jal r0, .LBB184_241
.LBB184_89:
	add r5, r3, r0
.LBB184_90:
	bne r5, r3, .LBB184_95
.LBB184_91:
	addi r3, r0, 11
	blt r9, r3, .LBB184_74
.LBB184_92:
	addi r7, r9, -11
	addi r3, r8, 10
	addi r5, r0, 32
.LBB184_93:
	add r6, r7, r0
	ldbu r7, r3+0
	sne r4, r7, r5
	bne r7, r5, .LBB184_95
.LBB184_94:
	addi r7, r6, -1
	addi r3, r3, 1
	bne r6, r0, .LBB184_93
.LBB184_95:
	beq r15, r0, .LBB184_240
.LBB184_96:
	addi r1, r0, 2
.LBB184_97:
	ldw r28, sp+280
	ldw r27, sp+284
	ldw r26, sp+288
	ldw r25, sp+292
	ldw r24, sp+296
	ldw r23, sp+300
	ldw r22, sp+304
	ldw r21, sp+308
	ldw r20, sp+312
	ldw r19, sp+316
	ldw r18, sp+320
	ldw r17, sp+324
	ldw r16, sp+328
	ldw r15, sp+332
	ldw r14, sp+336
	ldw r13, sp+340
	ldw r12, sp+344
	ldw r11, sp+348
	ldw lr, sp+0
	addi sp, sp, 352
	jalr r0, r31, 0
.LBB184_98:
	add r8, r1, r0
	bne r8, r1, .LBB184_54
.LBB184_99:
	addi r1, r0, 8
	blt r7, r1, .LBB184_115
.LBB184_100:
	addi r10, r7, -8
	addi r1, r6, 7
	addi r8, r0, 32
	addi r9, r0, 1
.LBB184_101:
	ldbu lr, r1+0
	seq r21, lr, r8
	addi r11, r10, -1
	sltu r10, r11, r10
	bne r10, r9, .LBB184_55
.LBB184_102:
	addi r1, r1, 1
	add r10, r11, r0
	beq lr, r8, .LBB184_101
	jal r0, .LBB184_55
.LBB184_103:
	beq r26, r0, .LBB184_135
.LBB184_104:
	addi r1, r0, 3
	lui r5, %hi(.L.str.60)
	addi r5, r5, %lo(.L.str.60)
	addi r6, r0, 26
	add r7, r0, r0
	add r4, r14, r0
	add r3, r13, r0
.LBB184_105:
	beq r7, r1, .LBB184_116
.LBB184_106:
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
	bne r9, r8, .LBB184_127
.LBB184_107:
	addi r7, r7, 1
	bne r4, r7, .LBB184_105
.LBB184_108:
	add r5, r4, r0
	jal r0, .LBB184_117
.LBB184_109:
	add r22, r3, r0
	add r13, r6, r0
	add r14, r7, r0
	addi r12, sp, 24
	add r3, r12, r0
	add r11, r5, r0
	jal r31, memcpy
	add r1, r12, r11
	stb r1+0, r0
	addi r1, r12, -1
	addi r3, r0, 32
.LBB184_110:
	add r5, r1, r11
	ldbu r6, r5+0
	bne r6, r3, .LBB184_82
.LBB184_111:
	add r4, r11, r0
	addi r11, r11, -1
	stb r5+0, r0
	bgt r4, r20, .LBB184_110
	jal r0, .LBB184_82
.LBB184_112:
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r20, r1
	stw r1+0, r16
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r20, r1
	stw r1+0, r17
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r20, r1
	stw r1+0, r19
	beq r12, r0, .LBB184_235
.LBB184_113:
	add r1, r0, r0
	blt r11, r18, .LBB184_236
.LBB184_114:
	lui r5, %hi(.L.str.58)
	addi r5, r5, %lo(.L.str.58)
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fio_str_is
	jal r0, .LBB184_236
.LBB184_115:
	addi r21, r0, 1
	jal r0, .LBB184_55
.LBB184_116:
	add r5, r1, r0
.LBB184_117:
	bne r5, r1, .LBB184_127
.LBB184_118:
	addi r1, r0, 4
	blt r4, r1, .LBB184_122
.LBB184_119:
	addi r1, r3, 3
	addi r5, r4, -3
	addi r6, r0, 32
.LBB184_120:
	ldbu r7, r1+0
	bne r7, r6, .LBB184_127
.LBB184_121:
	addi r1, r1, 1
	addi r5, r5, -1
	bne r5, r0, .LBB184_120
.LBB184_122:
	lui r4, %hi(.L.str.61)
	addi r4, r4, %lo(.L.str.61)
	addi r3, sp, 24
	jal r31, fopen
	bne r1, r0, .LBB184_124
.LBB184_123:
	lui r4, %hi(.L.str.62)
	addi r4, r4, %lo(.L.str.62)
	addi r3, sp, 24
	jal r31, fopen
.LBB184_124:
	bne r1, r0, .LBB184_141
.LBB184_125:
	bne r15, r0, .LBB184_96
.LBB184_126:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.63)
	addi r4, r4, %lo(.L.str.63)
	jal r0, .LBB184_174
.LBB184_127:
	lui r5, %hi(.L.str.64)
	addi r5, r5, %lo(.L.str.64)
	add r13, r3, r0
	add r14, r4, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB184_131
.LBB184_128:
	lui r4, %hi(.L.str.62)
	addi r4, r4, %lo(.L.str.62)
	addi r3, sp, 24
	jal r31, fopen
	beq r1, r0, .LBB184_132
.LBB184_129:
	add r3, r1, r0
	jal r31, fclose
	bne r15, r0, .LBB184_96
.LBB184_130:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.65)
	addi r4, r4, %lo(.L.str.65)
	jal r0, .LBB184_174
.LBB184_131:
	lui r5, %hi(.L.str.68)
	addi r5, r5, %lo(.L.str.68)
	add r3, r13, r0
	add r4, r14, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB184_135
.LBB184_132:
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	addi r3, sp, 24
	jal r31, fopen
	bne r1, r0, .LBB184_141
.LBB184_133:
	bne r15, r0, .LBB184_96
.LBB184_134:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.67)
	addi r4, r4, %lo(.L.str.67)
	jal r0, .LBB184_174
.LBB184_135:
	beq r13, r0, .LBB184_138
.LBB184_136:
	blt r14, r18, .LBB184_138
.LBB184_137:
	lui r5, %hi(.L.str.69)
	addi r5, r5, %lo(.L.str.69)
	add r3, r13, r0
	add r4, r14, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB184_237
.LBB184_138:
	lui r4, %hi(.L.str.61)
	addi r4, r4, %lo(.L.str.61)
	addi r3, sp, 24
	jal r31, fopen
	bne r1, r0, .LBB184_140
.LBB184_139:
	lui r4, %hi(.L.str.66)
	addi r4, r4, %lo(.L.str.66)
	addi r3, sp, 24
	jal r31, fopen
.LBB184_140:
	beq r1, r0, .LBB184_172
.LBB184_141:
	stw r25+0, r1
	lui r3, %hi(fio_udelim)
	addi r3, r3, %lo(fio_udelim)
	add r26, r20, r3
	stw r26+0, r0
	lui r3, %hi(fio_uaction)
	addi r3, r3, %lo(fio_uaction)
	add r13, r20, r3
	stw r13+0, r0
	lui r3, %hi(fio_uposn)
	addi r3, r3, %lo(fio_uposn)
	add r28, r20, r3
	stw r28+0, r0
	lui r25, %hi(fio_o_act)
	ldw r3, r25+%lo(fio_o_act)
	lui r27, %hi(fio_o_actlen)
	beq r3, r0, .LBB184_156
.LBB184_142:
	ldw r4, r27+%lo(fio_o_actlen)
	addi r5, r0, 1
	blt r4, r5, .LBB184_156
.LBB184_143:
	addi r6, r0, 4
	lui r7, %hi(.L.str.72)
	addi r7, r7, %lo(.L.str.72)
	addi r8, r0, 26
	add r9, r0, r0
.LBB184_144:
	beq r9, r6, .LBB184_148
.LBB184_145:
	add r10, r9, r7
	ldb r10, r10+0
	add lr, r3, r9
	ldb lr, lr+0
	addi r14, lr, -97
	andi r14, r14, 255
	sltu r14, r14, r8
	addi r15, lr, -32
	xor r15, r15, lr
	sub r14, r0, r14
	and r14, r15, r14
	xor lr, lr, r14
	bne lr, r10, .LBB184_154
.LBB184_146:
	addi r9, r9, 1
	bne r4, r9, .LBB184_144
.LBB184_147:
	add r7, r4, r0
	jal r0, .LBB184_149
.LBB184_148:
	add r7, r6, r0
.LBB184_149:
	bne r7, r6, .LBB184_154
.LBB184_150:
	addi r6, r0, 5
	blt r4, r6, .LBB184_155
.LBB184_151:
	addi r6, r3, 4
	addi r7, r4, -4
	addi r8, r0, 32
.LBB184_152:
	ldbu r9, r6+0
	bne r9, r8, .LBB184_154
.LBB184_153:
	addi r6, r6, 1
	addi r7, r7, -1
	bne r7, r0, .LBB184_152
	jal r0, .LBB184_155
.LBB184_154:
	lui r5, %hi(.L.str.73)
	addi r5, r5, %lo(.L.str.73)
	add r14, r1, r0
	jal r31, fio_str_is
	add r3, r1, r0
	add r1, r14, r0
	seq r3, r3, r24
	addi r3, r3, -1
	andi r5, r3, 2
.LBB184_155:
	stw r13+0, r5
.LBB184_156:
	lui r24, %hi(fio_o_pos)
	ldw r14, r24+%lo(fio_o_pos)
	beq r14, r0, .LBB184_176
.LBB184_157:
	lui r3, %hi(fio_o_poslen)
	ldw r15, r3+%lo(fio_o_poslen)
	addi r13, r0, 1
	blt r15, r13, .LBB184_176
.LBB184_158:
	addi r3, r0, 6
	lui r4, %hi(.L.str.74)
	addi r4, r4, %lo(.L.str.74)
	addi r5, r0, 26
	add r6, r0, r0
.LBB184_159:
	beq r6, r3, .LBB184_163
.LBB184_160:
	add r7, r6, r4
	ldb r7, r7+0
	add r8, r14, r6
	ldb r8, r8+0
	addi r9, r8, -97
	andi r9, r9, 255
	sltu r9, r9, r5
	addi r10, r8, -32
	xor r10, r10, r8
	sub r9, r0, r9
	and r9, r10, r9
	xor r8, r8, r9
	bne r8, r7, .LBB184_170
.LBB184_161:
	addi r6, r6, 1
	bne r15, r6, .LBB184_159
.LBB184_162:
	add r4, r15, r0
	jal r0, .LBB184_164
.LBB184_163:
	add r4, r3, r0
.LBB184_164:
	bne r4, r3, .LBB184_170
.LBB184_165:
	addi r3, r0, 7
	blt r15, r3, .LBB184_169
.LBB184_166:
	addi r3, r14, 6
	addi r4, r15, -6
	addi r5, r0, 32
.LBB184_167:
	ldbu r6, r3+0
	bne r6, r5, .LBB184_170
.LBB184_168:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB184_167
.LBB184_169:
	addi r13, r0, 2
	add r3, r1, r0
	add r4, r0, r0
	add r5, r13, r0
	jal r31, fseek
	jal r0, .LBB184_171
.LBB184_170:
	lui r5, %hi(.L.str.75)
	addi r5, r5, %lo(.L.str.75)
	add r3, r14, r0
	add r4, r15, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB184_175
.LBB184_171:
	stw r28+0, r13
	jal r0, .LBB184_176
.LBB184_172:
	bne r15, r0, .LBB184_96
.LBB184_173:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.70)
	addi r4, r4, %lo(.L.str.70)
.LBB184_174:
	addi r5, sp, 24
	jal r0, .LBB184_241
.LBB184_175:
	lui r5, %hi(.L.str.76)
	addi r5, r5, %lo(.L.str.76)
	add r3, r14, r0
	add r4, r15, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB184_242
.LBB184_176:
	ldw r13, r25+%lo(fio_o_act)
	beq r13, r0, .LBB184_191
.LBB184_177:
	ldw r14, r27+%lo(fio_o_actlen)
	addi r1, r0, 1
	blt r14, r1, .LBB184_191
.LBB184_178:
	addi r1, r0, 4
	lui r3, %hi(.L.str.72)
	addi r3, r3, %lo(.L.str.72)
	addi r4, r0, 26
	add r5, r0, r0
.LBB184_179:
	beq r5, r1, .LBB184_183
.LBB184_180:
	add r6, r5, r3
	ldb r6, r6+0
	add r7, r13, r5
	ldb r7, r7+0
	addi r8, r7, -97
	andi r8, r8, 255
	sltu r8, r8, r4
	addi r9, r7, -32
	xor r9, r9, r7
	sub r8, r0, r8
	and r8, r9, r8
	xor r7, r7, r8
	bne r7, r6, .LBB184_189
.LBB184_181:
	addi r5, r5, 1
	bne r14, r5, .LBB184_179
.LBB184_182:
	add r3, r14, r0
	jal r0, .LBB184_184
.LBB184_183:
	add r3, r1, r0
.LBB184_184:
	bne r3, r1, .LBB184_189
.LBB184_185:
	addi r1, r0, 5
	blt r14, r1, .LBB184_191
.LBB184_186:
	addi r1, r13, 4
	addi r3, r14, -4
	addi r4, r0, 32
.LBB184_187:
	ldbu r5, r1+0
	bne r5, r4, .LBB184_189
.LBB184_188:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB184_187
	jal r0, .LBB184_191
.LBB184_189:
	lui r5, %hi(.L.str.73)
	addi r5, r5, %lo(.L.str.73)
	add r3, r13, r0
	add r4, r14, r0
	jal r31, fio_str_is
	bne r1, r0, .LBB184_191
.LBB184_190:
	lui r5, %hi(.L.str.78)
	addi r5, r5, %lo(.L.str.78)
	add r3, r13, r0
	add r4, r14, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB184_243
.LBB184_191:
	lui r15, %hi(fio_o_pad)
	ldw r1, r15+%lo(fio_o_pad)
	beq r1, r0, .LBB184_204
.LBB184_192:
	lui r3, %hi(fio_o_padlen)
	ldw r3, r3+%lo(fio_o_padlen)
	addi r4, r0, 1
	blt r3, r4, .LBB184_204
.LBB184_193:
	addi r4, r0, 3
	lui r5, %hi(.L.str.80)
	addi r5, r5, %lo(.L.str.80)
	addi r6, r0, 26
	add r7, r0, r0
.LBB184_194:
	beq r7, r4, .LBB184_198
.LBB184_195:
	add r8, r7, r5
	ldb r8, r8+0
	add r9, r1, r7
	ldb r9, r9+0
	addi r10, r9, -97
	andi r10, r10, 255
	sltu r10, r10, r6
	addi lr, r9, -32
	xor lr, lr, r9
	sub r10, r0, r10
	and r10, lr, r10
	xor r9, r9, r10
	bne r9, r8, .LBB184_239
.LBB184_196:
	addi r7, r7, 1
	bne r3, r7, .LBB184_194
.LBB184_197:
	add r5, r3, r0
	jal r0, .LBB184_199
.LBB184_198:
	add r5, r4, r0
.LBB184_199:
	bne r5, r4, .LBB184_239
.LBB184_200:
	addi r4, r0, 4
	blt r3, r4, .LBB184_204
.LBB184_201:
	addi r1, r1, 3
	addi r3, r3, -3
	addi r4, r0, 32
.LBB184_202:
	ldbu r5, r1+0
	bne r5, r4, .LBB184_239
.LBB184_203:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB184_202
.LBB184_204:
	lui r27, %hi(fio_o_delim)
	ldw r13, r27+%lo(fio_o_delim)
	beq r13, r0, .LBB184_221
.LBB184_205:
	lui r1, %hi(fio_o_delimlen)
	ldw r14, r1+%lo(fio_o_delimlen)
	addi r1, r0, 1
	blt r14, r1, .LBB184_221
.LBB184_206:
	addi r1, r0, 10
	lui r3, %hi(.L.str.82)
	addi r3, r3, %lo(.L.str.82)
	addi r4, r0, 26
	add r5, r0, r0
.LBB184_207:
	beq r5, r1, .LBB184_211
.LBB184_208:
	add r6, r5, r3
	ldb r6, r6+0
	add r7, r13, r5
	ldb r7, r7+0
	addi r8, r7, -97
	andi r8, r8, 255
	sltu r8, r8, r4
	addi r9, r7, -32
	xor r9, r9, r7
	sub r8, r0, r8
	and r8, r9, r8
	xor r7, r7, r8
	bne r7, r6, .LBB184_217
.LBB184_209:
	addi r5, r5, 1
	bne r14, r5, .LBB184_207
.LBB184_210:
	add r3, r14, r0
	jal r0, .LBB184_212
.LBB184_211:
	add r3, r1, r0
.LBB184_212:
	bne r3, r1, .LBB184_217
.LBB184_213:
	addi r1, r0, 39
	addi r3, r0, 11
	blt r14, r3, .LBB184_219
.LBB184_214:
	addi r3, r13, 10
	addi r4, r14, -10
	addi r5, r0, 32
.LBB184_215:
	ldbu r6, r3+0
	bne r6, r5, .LBB184_217
.LBB184_216:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB184_215
	jal r0, .LBB184_219
.LBB184_217:
	lui r5, %hi(.L.str.83)
	addi r5, r5, %lo(.L.str.83)
	add r3, r13, r0
	add r4, r14, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB184_220
.LBB184_218:
	addi r1, r0, 34
.LBB184_219:
	stw r26+0, r1
	jal r0, .LBB184_221
.LBB184_220:
	lui r5, %hi(.L.str.84)
	addi r5, r5, %lo(.L.str.84)
	add r3, r13, r0
	add r4, r14, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB184_244
.LBB184_221:
	stw r27+%lo(fio_o_delim), r0
	stw r15+%lo(fio_o_pad), r0
	stw r25+%lo(fio_o_act), r0
	stw r24+%lo(fio_o_pos), r0
	add r3, r22, r23
	lui r1, %hi(.L.str.8)
	addi r1, r1, %lo(.L.str.8)
	addi r4, sp, 24
	xor r1, r1, r4
	sub r5, r0, r21
	and r1, r1, r5
	xor r4, r4, r1
	jal r31, strcpy
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r1, r20, r1
	addi r3, r0, -1
	stw r1+0, r3
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r20, r1
	stw r1+0, r16
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r20, r1
	stw r1+0, r17
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r20, r1
	stw r1+0, r19
	beq r12, r0, .LBB184_235
.LBB184_222:
	add r1, r0, r0
	blt r11, r18, .LBB184_236
.LBB184_223:
	addi r1, r0, 4
	lui r3, %hi(.L.str.58)
	addi r3, r3, %lo(.L.str.58)
	addi r4, r0, 26
	add r5, r0, r0
.LBB184_224:
	beq r5, r1, .LBB184_228
.LBB184_225:
	add r6, r5, r3
	ldb r6, r6+0
	add r7, r12, r5
	ldb r7, r7+0
	addi r8, r7, -97
	andi r8, r8, 255
	sltu r8, r8, r4
	addi r9, r7, -32
	xor r9, r9, r7
	sub r8, r0, r8
	and r8, r9, r8
	xor r7, r7, r8
	bne r7, r6, .LBB184_235
.LBB184_226:
	addi r5, r5, 1
	bne r11, r5, .LBB184_224
.LBB184_227:
	add r3, r11, r0
	jal r0, .LBB184_229
.LBB184_228:
	add r3, r1, r0
.LBB184_229:
	bne r3, r1, .LBB184_235
.LBB184_230:
	addi r1, r0, 5
	blt r11, r1, .LBB184_234
.LBB184_231:
	addi r1, r12, 4
	addi r3, r11, -4
	addi r4, r0, 32
.LBB184_232:
	ldbu r5, r1+0
	bne r5, r4, .LBB184_235
.LBB184_233:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB184_232
.LBB184_234:
	add r1, r18, r0
	jal r0, .LBB184_236
.LBB184_235:
	add r1, r0, r0
.LBB184_236:
	lui r3, %hi(fio_ubz)
	addi r3, r3, %lo(fio_ubz)
	add r3, r20, r3
	stw r3+0, r1
	add r1, r0, r0
	jal r0, .LBB184_97
.LBB184_237:
	bne r15, r0, .LBB184_96
.LBB184_238:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.71)
	addi r4, r4, %lo(.L.str.71)
	jal r0, .LBB184_241
.LBB184_239:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.81)
	addi r4, r4, %lo(.L.str.81)
	jal r0, .LBB184_241
.LBB184_240:
	lui r3, %hi(stderr)
	ldw r3, r3+%lo(stderr)
	lui r5, %hi(.L.str.53)
	addi r5, r5, %lo(.L.str.53)
	lui r6, %hi(.L.str.52)
	addi r6, r6, %lo(.L.str.52)
	xor r6, r6, r5
	sub r4, r0, r4
	and r6, r6, r4
	xor r5, r5, r6
	xor r6, r9, r1
	and r6, r6, r4
	xor r6, r1, r6
	xor r1, r8, r10
	and r1, r1, r4
	xor r7, r10, r1
	lui r4, %hi(.L.str.51)
	addi r4, r4, %lo(.L.str.51)
.LBB184_241:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB184_242:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.77)
	addi r4, r4, %lo(.L.str.77)
	jal r0, .LBB184_241
.LBB184_243:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.79)
	addi r4, r4, %lo(.L.str.79)
	jal r0, .LBB184_241
.LBB184_244:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.85)
	addi r4, r4, %lo(.L.str.85)
	jal r0, .LBB184_241
.Lfunc_end184:
	.size	f77_open, .Lfunc_end184-f77_open
                                        # -- End function
	.p2align	2                               # -- Begin function fio_str_is
	.type	fio_str_is,@function
fio_str_is:                             # @fio_str_is
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	addi r1, r0, 1
	blt r4, r1, .LBB185_6
.LBB185_1:
	addi r7, r0, 26
	add r6, r0, r0
.LBB185_2:
	add r8, r5, r6
	ldb r8, r8+0
	beq r8, r0, .LBB185_7
.LBB185_3:
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
	bne r9, r8, .LBB185_8
.LBB185_4:
	addi r6, r6, 1
	bne r4, r6, .LBB185_2
.LBB185_5:
	add r6, r4, r0
	jal r0, .LBB185_7
.LBB185_6:
	add r6, r0, r0
.LBB185_7:
	add r5, r5, r6
	ldbu r5, r5+0
	beq r5, r0, .LBB185_10
.LBB185_8:
	add r1, r0, r0
.LBB185_9:
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.LBB185_10:
	ble r4, r6, .LBB185_9
.LBB185_11:
	add r3, r3, r6
	sub r4, r4, r6
	addi r5, r0, 32
.LBB185_12:
	ldbu r6, r3+0
	bne r6, r5, .LBB185_8
.LBB185_13:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB185_12
	jal r0, .LBB185_9
.Lfunc_end185:
	.size	fio_str_is, .Lfunc_end185-fio_str_is
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
	add r6, r4, r0
	addi r4, r3, -1
	addi r1, r0, 99
	sltu r4, r4, r1
	sub r4, r0, r4
	and r4, r3, r4
	slli r4, r4, 2
	lui r7, %hi(fio_pend)
	addi r7, r7, %lo(fio_pend)
	add r4, r4, r7
	stw r4+0, r0
	add r13, r0, r0
	addi r7, r0, 1
	beq r6, r0, .LBB186_13
.LBB186_1:
	addi r4, r0, 1
	blt r5, r4, .LBB186_13
.LBB186_2:
	addi r7, r0, 6
	lui r8, %hi(.L.str.86)
	addi r8, r8, %lo(.L.str.86)
	addi r9, r0, 26
	add r10, r0, r0
.LBB186_3:
	beq r10, r7, .LBB186_8
.LBB186_4:
	add lr, r10, r8
	ldb lr, lr+0
	add r11, r6, r10
	ldb r11, r11+0
	addi r12, r11, -97
	andi r12, r12, 255
	sltu r12, r12, r9
	addi r13, r11, -32
	xor r13, r13, r11
	sub r12, r0, r12
	and r12, r13, r12
	xor r11, r11, r12
	bne r11, lr, .LBB186_7
.LBB186_5:
	addi r10, r10, 1
	bne r5, r10, .LBB186_3
.LBB186_6:
	add r8, r5, r0
	beq r8, r7, .LBB186_9
.LBB186_7:
	add r7, r0, r0
	add r13, r0, r0
	beq r7, r0, .LBB186_14
	jal r0, .LBB186_26
.LBB186_8:
	add r8, r7, r0
	bne r8, r7, .LBB186_7
.LBB186_9:
	addi r8, r0, 7
	add r7, r4, r0
	add r13, r4, r0
	blt r5, r8, .LBB186_13
.LBB186_10:
	addi r10, r5, -7
	addi r4, r6, 6
	addi r8, r0, 32
	addi r9, r0, 1
.LBB186_11:
	ldbu lr, r4+0
	seq r7, lr, r8
	addi r11, r10, -1
	sltu r10, r11, r10
	bne r10, r9, .LBB186_35
.LBB186_12:
	addi r4, r4, 1
	add r10, r11, r0
	add r13, r7, r0
	beq lr, r8, .LBB186_11
.LBB186_13:
	bne r7, r0, .LBB186_26
.LBB186_14:
	addi r4, r0, 1
	blt r5, r4, .LBB186_36
.LBB186_15:
	addi r4, r0, 4
	lui r7, %hi(.L.str.87)
	addi r7, r7, %lo(.L.str.87)
	addi r8, r0, 26
	add r9, r0, r0
.LBB186_16:
	beq r9, r4, .LBB186_20
.LBB186_17:
	add r10, r9, r7
	ldb r10, r10+0
	add lr, r6, r9
	ldb lr, lr+0
	addi r11, lr, -97
	andi r11, r11, 255
	sltu r11, r11, r8
	addi r12, lr, -32
	xor r12, r12, lr
	sub r11, r0, r11
	and r11, r12, r11
	xor lr, lr, r11
	bne lr, r10, .LBB186_36
.LBB186_18:
	addi r9, r9, 1
	bne r5, r9, .LBB186_16
.LBB186_19:
	add r7, r5, r0
	jal r0, .LBB186_21
.LBB186_20:
	add r7, r4, r0
.LBB186_21:
	bne r7, r4, .LBB186_36
.LBB186_22:
	addi r4, r0, 5
	blt r5, r4, .LBB186_26
.LBB186_23:
	addi r4, r6, 4
	addi r7, r5, -4
	addi r8, r0, 32
.LBB186_24:
	ldbu r9, r4+0
	bne r9, r8, .LBB186_36
.LBB186_25:
	addi r4, r4, 1
	addi r7, r7, -1
	bne r7, r0, .LBB186_24
.LBB186_26:
	bgtu r3, r1, .LBB186_34
.LBB186_27:
	slli r14, r3, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r15, r14, r1
	ldw r11, r15+0
	beq r11, r0, .LBB186_34
.LBB186_28:
	add r16, r3, r0
	beq r3, r0, .LBB186_31
.LBB186_29:
	lui r1, %hi(fio_uwrote)
	addi r1, r1, %lo(fio_uwrote)
	add r1, r14, r1
	ldw r3, r1+0
	beq r3, r0, .LBB186_31
.LBB186_30:
	stw r1+0, r0
	add r3, r11, r0
	jal r31, fflush
	add r3, r11, r0
	jal r31, fileno
	add r12, r1, r0
	add r3, r11, r0
	jal r31, ftell
	add r3, r12, r0
	add r4, r1, r0
	jal r31, ftruncate
.LBB186_31:
	ldw r3, r15+0
	jal r31, fclose
	stw r15+0, r0
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r1, r14, r1
	addi r3, r0, -1
	stw r1+0, r3
	slli r11, r16, 8
	lui r12, %hi(fio_ufname)
	addi r12, r12, %lo(fio_ufname)
	beq r13, r0, .LBB186_33
.LBB186_32:
	add r3, r11, r12
	jal r31, remove
.LBB186_33:
	add r1, r11, r12
	stb r1+0, r0
.LBB186_34:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB186_35:
	add r13, r7, r0
	beq r7, r0, .LBB186_14
	jal r0, .LBB186_26
.LBB186_36:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.88)
	addi r4, r4, %lo(.L.str.88)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end186:
	.size	f77_close, .Lfunc_end186-f77_close
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
	stw sp+276, r25
	stw sp+272, r26
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB187_4
.LBB187_1:
	addi r3, r0, -400
	lui r6, %hi(fio_uend+400)
	addi r6, r6, %lo(fio_uend+400)
	addi r7, r0, -1
.LBB187_2:
	add r8, r3, r6
	stw r8+0, r7
	addi r3, r3, 4
	bne r3, r0, .LBB187_2
.LBB187_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB187_4:
	lui r23, %hi(inq_unit)
	addi r1, r0, -1
	stw r23+%lo(inq_unit), r1
	lui r26, %hi(inq_exist)
	stb r26+%lo(inq_exist), r0
	lui r22, %hi(inq_opened)
	stb r22+%lo(inq_opened), r0
	lui r12, %hi(inq_name)
	addi r12, r12, %lo(inq_name)
	stb r12+0, r0
	lui r24, %hi(inq_named)
	stw r24+%lo(inq_named), r0
	lui r21, %hi(inq_direct)
	stw r21+%lo(inq_direct), r0
	lui r20, %hi(inq_unf)
	stw r20+%lo(inq_unf), r0
	lui r15, %hi(inq_recl)
	stw r15+%lo(inq_recl), r0
	lui r14, %hi(inq_nextrec)
	stw r14+%lo(inq_nextrec), r0
	lui r19, %hi(inq_bz)
	stw r19+%lo(inq_bz), r0
	lui r18, %hi(inq_action)
	stw r18+%lo(inq_action), r0
	lui r17, %hi(inq_posn)
	stw r17+%lo(inq_posn), r0
	lui r16, %hi(inq_delim)
	stw r16+%lo(inq_delim), r0
	beq r4, r0, .LBB187_21
.LBB187_5:
	addi r25, r0, 1
	blt r5, r25, .LBB187_21
.LBB187_6:
	addi r1, r0, 255
	slt r1, r5, r1
	sub r1, r0, r1
	xori r3, r5, 255
	and r1, r3, r1
	xori r12, r1, 255
	addi r13, sp, 16
	add r3, r13, r0
	add r5, r12, r0
	jal r31, memcpy
	add r1, r13, r12
	stb r1+0, r0
	addi r3, r12, -1
	addi r1, r0, 32
.LBB187_7:
	addi r4, sp, 16
	add r4, r4, r3
	ldbu r5, r4+0
	bne r5, r1, .LBB187_9
.LBB187_8:
	stb r4+0, r0
	addi r4, r3, -1
	addi r5, r3, 1
	add r3, r4, r0
	bgt r5, r25, .LBB187_7
.LBB187_9:
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	addi r12, sp, 16
	add r4, r12, r0
	jal r31, strcpy
	stw r24+%lo(inq_named), r25
	lui r4, %hi(.L.str.62)
	addi r4, r4, %lo(.L.str.62)
	add r3, r12, r0
	jal r31, fopen
	beq r1, r0, .LBB187_11
.LBB187_10:
	stb r26+%lo(inq_exist), r25
	add r3, r1, r0
	jal r31, fclose
.LBB187_11:
	lui r12, %hi(fio_ufname+256)
	addi r12, r12, %lo(fio_ufname+256)
	lui r13, %hi(fio_ufile+4)
	addi r13, r13, %lo(fio_ufile+4)
	addi r24, r0, 100
	jal r0, .LBB187_13
.LBB187_12:
	addi r25, r25, 1
	addi r12, r12, 256
	addi r13, r13, 4
	beq r25, r24, .LBB187_16
.LBB187_13:
	ldw r1, r13+0
	beq r1, r0, .LBB187_12
.LBB187_14:
	addi r4, sp, 16
	add r3, r12, r0
	jal r31, strcmp
	bne r1, r0, .LBB187_12
.LBB187_15:
	addi r1, r0, 1
	stb r22+%lo(inq_opened), r1
	stw r23+%lo(inq_unit), r25
.LBB187_16:
	ldbu r1, r22+%lo(inq_opened)
	beq r1, r0, .LBB187_26
.LBB187_17:
	ldw r3, r23+%lo(inq_unit)
	xor r3, r3, r11
	sub r1, r0, r1
	and r1, r3, r1
	xor r11, r11, r1
.LBB187_18:
	slli r1, r11, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r4, r3+0
	stw r21+%lo(inq_direct), r4
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r1, r3
	ldw r3, r3+0
	stw r20+%lo(inq_unf), r3
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r1, r3
	ldw r3, r3+0
	stw r15+%lo(inq_recl), r3
	lui r5, %hi(fio_ubz)
	addi r5, r5, %lo(fio_ubz)
	add r5, r1, r5
	ldw r5, r5+0
	stw r19+%lo(inq_bz), r5
	lui r5, %hi(fio_uaction)
	addi r5, r5, %lo(fio_uaction)
	add r5, r1, r5
	ldw r5, r5+0
	stw r18+%lo(inq_action), r5
	lui r5, %hi(fio_uposn)
	addi r5, r5, %lo(fio_uposn)
	add r5, r1, r5
	ldw r5, r5+0
	stw r17+%lo(inq_posn), r5
	lui r5, %hi(fio_udelim)
	addi r5, r5, %lo(fio_udelim)
	add r5, r1, r5
	ldw r5, r5+0
	stw r16+%lo(inq_delim), r5
	beq r4, r0, .LBB187_26
.LBB187_19:
	addi r4, r0, 1
	blt r3, r4, .LBB187_26
.LBB187_20:
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
	jal r0, .LBB187_26
.LBB187_21:
	addi r1, r0, 100
	bltu r11, r1, .LBB187_23
.LBB187_22:
	stb r26+%lo(inq_exist), r0
	jal r0, .LBB187_26
.LBB187_23:
	addi r1, r0, 1
	stb r26+%lo(inq_exist), r1
	stw r23+%lo(inq_unit), r11
	addi r3, r0, 6
	bgtu r11, r3, .LBB187_27
.LBB187_24:
	sll r4, r1, r11
	andi r4, r4, 97
	beq r4, r0, .LBB187_27
.LBB187_25:
	seq r3, r11, r3
	addi r4, r0, 5
	seq r4, r11, r4
	stb r22+%lo(inq_opened), r1
	lui r1, %hi(.L.str.91)
	addi r1, r1, %lo(.L.str.91)
	lui r5, %hi(.L.str.90)
	addi r5, r5, %lo(.L.str.90)
	xor r5, r5, r1
	sub r3, r0, r3
	and r3, r5, r3
	xor r1, r1, r3
	lui r3, %hi(.L.str.89)
	addi r3, r3, %lo(.L.str.89)
	xor r3, r3, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r4, r1, r3
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	jal r31, strcpy
	stw r24+%lo(inq_named), r0
.LBB187_26:
	add r1, r0, r0
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
.LBB187_27:
	slli r3, r11, 2
	lui r4, %hi(fio_ufile)
	addi r4, r4, %lo(fio_ufile)
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r0, .LBB187_29
.LBB187_28:
	stb r22+%lo(inq_opened), r1
	slli r1, r11, 8
	lui r3, %hi(fio_ufname)
	addi r3, r3, %lo(fio_ufname)
	add r4, r1, r3
	add r3, r12, r0
	jal r31, strcpy
	ldbu r1, r12+0
	addi r3, r0, 0
	sne r1, r1, r3
	stw r24+%lo(inq_named), r1
	jal r0, .LBB187_18
.LBB187_29:
	stb r22+%lo(inq_opened), r0
	jal r0, .LBB187_26
.Lfunc_end187:
	.size	f77_inquire, .Lfunc_end187-f77_inquire
                                        # -- End function
	.globl	f77_inq                         # -- Begin function f77_inq
	.p2align	2
	.type	f77_inq,@function
f77_inq:                                # @f77_inq
# %bb.0:
	addi r1, r3, -1
	addi r3, r0, 20
	bgtu r1, r3, .LBB188_43
.LBB188_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r12, r5, r0
	add r11, r4, r0
	slli r1, r1, 2
	lui r3, %hi(.LJTI188_0)
	addi r3, r3, %lo(.LJTI188_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB188_2:
	lui r1, %hi(inq_exist)
	ldbu r1, r1+%lo(inq_exist)
	jal r0, .LBB188_41
.LBB188_3:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_action)
	ldw r3, r3+%lo(inq_action)
	addi r4, r0, 2
	seq r3, r3, r4
	lui r4, %hi(.L.str.80)
	addi r4, r4, %lo(.L.str.80)
	lui r5, %hi(.L.str.93)
	addi r5, r5, %lo(.L.str.93)
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
	blt r12, r3, .LBB188_42
.LBB188_4:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_6
.LBB188_5:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_6:
	add r5, r3, r0
	bge r4, r1, .LBB188_5
.LBB188_7:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_5
.LBB188_8:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r14, r0, 1
	bne r1, r14, .LBB188_88
.LBB188_9:
	lui r1, %hi(inq_action)
	ldw r1, r1+%lo(inq_action)
	bne r1, r14, .LBB188_89
.LBB188_10:
	lui r13, %hi(.L.str.72)
	addi r13, r13, %lo(.L.str.72)
	jal r0, .LBB188_90
.LBB188_11:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB188_42
.LBB188_12:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB188_42
.LBB188_13:
	lui r1, %hi(inq_recl)
	ldw r1, r1+%lo(inq_recl)
	jal r0, .LBB188_41
.LBB188_14:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r14, r0, 1
	bne r1, r14, .LBB188_42
.LBB188_15:
	lui r1, %hi(inq_named)
	ldw r1, r1+%lo(inq_named)
	beq r1, r0, .LBB188_42
.LBB188_16:
	lui r13, %hi(inq_name)
	addi r13, r13, %lo(inq_name)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB188_42
.LBB188_17:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_19
.LBB188_18:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_19:
	add r5, r3, r0
	bge r4, r1, .LBB188_18
.LBB188_20:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB188_18
.LBB188_21:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB188_42
.LBB188_22:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB188_42
.LBB188_23:
	lui r1, %hi(inq_nextrec)
	ldw r1, r1+%lo(inq_nextrec)
	jal r0, .LBB188_41
.LBB188_24:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	lui r5, %hi(.L.str.80)
	addi r5, r5, %lo(.L.str.80)
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
	blt r12, r3, .LBB188_42
.LBB188_25:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_27
.LBB188_26:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_27:
	add r5, r3, r0
	bge r4, r1, .LBB188_26
.LBB188_28:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_26
.LBB188_29:
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
	jal r0, .LBB188_41
.LBB188_30:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r13, %hi(.L.str.92)
	addi r13, r13, %lo(.L.str.92)
	addi r14, r0, 1
	bne r1, r14, .LBB188_96
.LBB188_31:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	bne r1, r0, .LBB188_96
.LBB188_32:
	lui r1, %hi(inq_posn)
	ldw r1, r1+%lo(inq_posn)
	addi r3, r0, 2
	bne r1, r3, .LBB188_95
.LBB188_33:
	lui r13, %hi(.L.str.74)
	addi r13, r13, %lo(.L.str.74)
	jal r0, .LBB188_96
.LBB188_34:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_named)
	ldw r3, r3+%lo(inq_named)
	sub r1, r0, r1
	and r1, r3, r1
	jal r0, .LBB188_41
.LBB188_35:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.80)
	addi r4, r4, %lo(.L.str.80)
	lui r5, %hi(.L.str.93)
	addi r5, r5, %lo(.L.str.93)
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
	blt r12, r3, .LBB188_42
.LBB188_36:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_38
.LBB188_37:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_38:
	add r5, r3, r0
	bge r4, r1, .LBB188_37
.LBB188_39:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_37
.LBB188_40:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
.LBB188_41:
	stw r11+0, r1
.LBB188_42:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB188_43:
	jalr r0, r31, 0
.LBB188_44:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_action)
	ldw r3, r3+%lo(inq_action)
	addi r14, r0, 1
	seq r3, r3, r14
	lui r4, %hi(.L.str.80)
	addi r4, r4, %lo(.L.str.80)
	lui r5, %hi(.L.str.93)
	addi r5, r5, %lo(.L.str.93)
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
	blt r12, r14, .LBB188_42
.LBB188_45:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_47
.LBB188_46:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_47:
	add r5, r3, r0
	bge r4, r1, .LBB188_46
.LBB188_48:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_46
.LBB188_49:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	lui r5, %hi(.L.str.80)
	addi r5, r5, %lo(.L.str.80)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB188_42
.LBB188_50:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_52
.LBB188_51:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_52:
	add r5, r3, r0
	bge r4, r1, .LBB188_51
.LBB188_53:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_51
.LBB188_54:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
	lui r5, %hi(.L.str.49)
	addi r5, r5, %lo(.L.str.49)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB188_42
.LBB188_55:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_57
.LBB188_56:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_57:
	add r5, r3, r0
	bge r4, r1, .LBB188_56
.LBB188_58:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_56
.LBB188_59:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r5, %hi(inq_bz)
	ldw r5, r5+%lo(inq_bz)
	seq r4, r5, r4
	lui r5, %hi(.L.str.58)
	addi r5, r5, %lo(.L.str.58)
	lui r6, %hi(.L.str.94)
	addi r6, r6, %lo(.L.str.94)
	xor r6, r6, r5
	sub r4, r0, r4
	and r4, r6, r4
	xor r4, r5, r4
	lui r5, %hi(.L.str.92)
	addi r5, r5, %lo(.L.str.92)
	xor r4, r4, r5
	sub r3, r0, r3
	and r3, r4, r3
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r5, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB188_42
.LBB188_60:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_62
.LBB188_61:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_62:
	add r5, r3, r0
	bge r4, r1, .LBB188_61
.LBB188_63:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_61
.LBB188_64:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	lui r5, %hi(.L.str.80)
	addi r5, r5, %lo(.L.str.80)
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
	blt r12, r3, .LBB188_42
.LBB188_65:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_67
.LBB188_66:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_67:
	add r5, r3, r0
	bge r4, r1, .LBB188_66
.LBB188_68:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_66
.LBB188_69:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_action)
	ldw r3, r3+%lo(inq_action)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
	lui r5, %hi(.L.str.80)
	addi r5, r5, %lo(.L.str.80)
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
	blt r12, r3, .LBB188_42
.LBB188_70:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_72
.LBB188_71:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_72:
	add r5, r3, r0
	bge r4, r1, .LBB188_71
.LBB188_73:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_71
.LBB188_74:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.80)
	addi r4, r4, %lo(.L.str.80)
	lui r5, %hi(.L.str.93)
	addi r5, r5, %lo(.L.str.93)
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
	blt r12, r3, .LBB188_42
.LBB188_75:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_77
.LBB188_76:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_77:
	add r5, r3, r0
	bge r4, r1, .LBB188_76
.LBB188_78:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_76
.LBB188_79:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.48)
	addi r4, r4, %lo(.L.str.48)
	lui r5, %hi(.L.str.50)
	addi r5, r5, %lo(.L.str.50)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB188_42
.LBB188_80:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_82
.LBB188_81:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_82:
	add r5, r3, r0
	bge r4, r1, .LBB188_81
.LBB188_83:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_81
.LBB188_84:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r13, %hi(.L.str.92)
	addi r13, r13, %lo(.L.str.92)
	addi r14, r0, 1
	bne r1, r14, .LBB188_102
.LBB188_85:
	lui r1, %hi(inq_unf)
	ldw r1, r1+%lo(inq_unf)
	bne r1, r0, .LBB188_102
.LBB188_86:
	lui r1, %hi(inq_delim)
	ldw r1, r1+%lo(inq_delim)
	addi r3, r0, 39
	bne r1, r3, .LBB188_101
.LBB188_87:
	lui r13, %hi(.L.str.82)
	addi r13, r13, %lo(.L.str.82)
	jal r0, .LBB188_102
.LBB188_88:
	lui r13, %hi(.L.str.92)
	addi r13, r13, %lo(.L.str.92)
	jal r0, .LBB188_90
.LBB188_89:
	addi r3, r0, 2
	seq r1, r1, r3
	lui r3, %hi(.L.str.78)
	addi r3, r3, %lo(.L.str.78)
	lui r4, %hi(.L.str.73)
	addi r4, r4, %lo(.L.str.73)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r13, r3, r1
.LBB188_90:
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB188_42
.LBB188_91:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_93
.LBB188_92:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_93:
	add r5, r3, r0
	bge r4, r1, .LBB188_92
.LBB188_94:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_92
.LBB188_95:
	seq r1, r1, r14
	lui r3, %hi(.L.str.76)
	addi r3, r3, %lo(.L.str.76)
	lui r4, %hi(.L.str.75)
	addi r4, r4, %lo(.L.str.75)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r13, r3, r1
.LBB188_96:
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB188_42
.LBB188_97:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_99
.LBB188_98:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_99:
	add r5, r3, r0
	bge r4, r1, .LBB188_98
.LBB188_100:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_98
.LBB188_101:
	addi r3, r0, 34
	seq r1, r1, r3
	lui r3, %hi(.L.str.84)
	addi r3, r3, %lo(.L.str.84)
	lui r4, %hi(.L.str.83)
	addi r4, r4, %lo(.L.str.83)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r13, r3, r1
.LBB188_102:
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB188_42
.LBB188_103:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB188_105
.LBB188_104:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB188_42
.LBB188_105:
	add r5, r3, r0
	bge r4, r1, .LBB188_104
.LBB188_106:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB188_104
.Lfunc_end188:
	.size	f77_inq, .Lfunc_end188-f77_inq
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI188_0,@object
.LJTI188_0:
	.word	.LBB188_2
	.word	.LBB188_40
	.word	.LBB188_29
	.word	.LBB188_34
	.word	.LBB188_14
	.word	.LBB188_54
	.word	.LBB188_64
	.word	.LBB188_35
	.word	.LBB188_79
	.word	.LBB188_24
	.word	.LBB188_74
	.word	.LBB188_11
	.word	.LBB188_21
	.word	.LBB188_59
	.word	.LBB188_8
	.word	.LBB188_30
	.word	.LBB188_3
	.word	.LBB188_44
	.word	.LBB188_69
	.word	.LBB188_84
	.word	.LBB188_49
	.size	.LJTI188_0, 84
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
	addi r1, r3, -1
	addi r3, r0, 99
	sltu r1, r1, r3
	sub r1, r0, r1
	and r1, r11, r1
	slli r1, r1, 2
	lui r3, %hi(fio_pend)
	addi r3, r3, %lo(fio_pend)
	add r1, r1, r3
	stw r1+0, r0
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB189_4
.LBB189_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB189_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB189_2
.LBB189_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB189_4:
	addi r1, r11, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB189_9
.LBB189_5:
	addi r1, r11, -5
	addi r3, r0, 1
	bleu r1, r3, .LBB189_9
.LBB189_6:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, fio_resolve
	add r12, r1, r0
	slli r13, r11, 2
	lui r1, %hi(fio_uwrote)
	addi r1, r1, %lo(fio_uwrote)
	add r1, r13, r1
	ldw r3, r1+0
	beq r3, r0, .LBB189_8
.LBB189_7:
	stw r1+0, r0
	add r3, r12, r0
	jal r31, fflush
	add r3, r12, r0
	jal r31, fileno
	add r11, r1, r0
	add r3, r12, r0
	jal r31, ftell
	add r3, r11, r0
	add r4, r1, r0
	jal r31, ftruncate
.LBB189_8:
	add r3, r12, r0
	jal r31, fflush
	add r3, r12, r0
	add r4, r0, r0
	add r5, r0, r0
	jal r31, fseek
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r13, r1
	stw r1+0, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB189_9:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.95)
	addi r4, r4, %lo(.L.str.95)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end189:
	.size	f77_rewind, .Lfunc_end189-f77_rewind
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
	beq r3, r16, .LBB190_197
.LBB190_1:
	add r13, r4, r0
	add r12, r3, r0
	lui lr, %hi(fio_ftext)
	addi r11, r0, -1
	stw lr+%lo(fio_ftext), r11
	addi r20, r0, 32
	add r1, r0, r0
	jal r0, .LBB190_3
.LBB190_2:
	add r1, r12, r28
	ldbu r3, r1+0
	addi r1, r28, 1
	bne r3, r20, .LBB190_5
.LBB190_3:
	add r28, r1, r0
	blt r13, r0, .LBB190_2
.LBB190_4:
	bltu r28, r13, .LBB190_2
.LBB190_5:
	lui r14, %hi(.L.str.102)
	addi r14, r14, %lo(.L.str.102)
	lui r15, %hi(fio_fmt_why)
	blt r13, r0, .LBB190_7
.LBB190_6:
	bgeu r28, r13, .LBB190_199
.LBB190_7:
	add r1, r12, r28
	ldbu r1, r1+0
	addi r3, r0, 40
	stw sp+52, r3
	bne r1, r3, .LBB190_199
.LBB190_8:
	addi r23, r0, 1022
	lui r19, %hi(.L.str.103)
	addi r19, r19, %lo(.L.str.103)
	addi r1, r0, 1
	stw sp+48, r1
	slli r1, r1, 2
	lui r24, %hi(.LJTI190_1)
	addi r24, r24, %lo(.LJTI190_1)
	add r22, r24, r1
	addi r26, r0, 39
	addi r9, r0, 246
	addi r10, r0, 41
	addi r14, r0, 0
	add r21, r0, r0
	add r25, r0, r0
	lui r18, %hi(fio_fbuf+1)
	addi r18, r18, %lo(fio_fbuf+1)
.LBB190_9:
	blt r13, r0, .LBB190_13
.LBB190_10:
	add r17, r0, r0
	bltu r28, r13, .LBB190_13
.LBB190_11:
	blt r21, r23, .LBB190_14
.LBB190_12:
	stw r15+%lo(fio_fmt_why), r19
	ldw r1, r22+0
	jalr r0, r1, 0
.LBB190_13:
	add r1, r12, r28
	ldbu r17, r1+0
	bge r21, r23, .LBB190_12
.LBB190_14:
	andi r27, r17, 255
	bgtu r27, r26, .LBB190_38
.LBB190_15:
	slli r1, r27, 2
	lui r3, %hi(.LJTI190_0)
	addi r3, r3, %lo(.LJTI190_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB190_16:
	add r1, r21, r0
	add r5, r26, r0
	jal r0, .LBB190_18
.LBB190_17:
	ldbu r5, r4+0
	add r1, r21, r0
.LBB190_18:
	add r3, r28, r0
.LBB190_19:
	add r4, r1, r16
	stb r4+0, r5
	addi r28, r3, 1
	blt r13, r0, .LBB190_21
.LBB190_20:
	add r4, r0, r0
	bgeu r28, r13, .LBB190_22
.LBB190_21:
	add r4, r12, r28
	ldbu r4, r4+0
.LBB190_22:
	addi r21, r1, 1
	andi r4, r4, 255
	beq r4, r0, .LBB190_35
.LBB190_23:
	addi r5, r0, 1020
	bge r1, r5, .LBB190_35
.LBB190_24:
	bne r4, r27, .LBB190_29
.LBB190_25:
	addi r28, r3, 2
	blt r13, r0, .LBB190_27
.LBB190_26:
	add r3, r0, r0
	bgeu r28, r13, .LBB190_28
.LBB190_27:
	add r3, r12, r28
	ldbu r3, r3+0
.LBB190_28:
	andi r3, r3, 255
	bne r3, r27, .LBB190_46
.LBB190_29:
	add r4, r12, r28
	blt r13, r0, .LBB190_31
.LBB190_30:
	bgeu r28, r13, .LBB190_33
.LBB190_31:
	ldbu r5, r4+0
	addi r3, r0, 39
	bne r5, r3, .LBB190_33
.LBB190_32:
	addi r21, r1, 2
	add r1, r1, r18
	stb r1+0, r3
.LBB190_33:
	blt r13, r0, .LBB190_17
.LBB190_34:
	add r1, r21, r0
	add r5, r0, r0
	add r3, r28, r0
	bgeu r28, r13, .LBB190_19
	jal r0, .LBB190_17
.LBB190_35:
	lui r1, %hi(.L.str.105)
	addi r1, r1, %lo(.L.str.105)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB190_36:
	addi r28, r28, 1
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB190_37:
	lui r1, %hi(.L.str.104)
	addi r1, r1, %lo(.L.str.104)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB190_38:
	addi r1, r17, -58
	andi r1, r1, 255
	bltu r1, r9, .LBB190_49
.LBB190_39:
	beq r21, r0, .LBB190_47
.LBB190_40:
	ldw r3, lr+%lo(fio_ftext)
	add r1, r0, r0
	beq r21, r3, .LBB190_48
.LBB190_41:
	stw sp+40, r25
	stw sp+44, r14
	stw sp+36, r24
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
	add r8, r1, r0
	addi r3, r0, 2
	ldw r1, sp+48
	blt r21, r3, .LBB190_43
.LBB190_42:
	lui r1, %hi(fio_fbuf-2)
	addi r1, r1, %lo(fio_fbuf-2)
	add r1, r21, r1
	ldbu r3, r1+0
	add r14, r8, r0
	jal r31, toupper
	add r8, r14, r0
	addi r3, r0, 66
	sne r1, r1, r3
.LBB190_43:
	addi r3, r8, -65
	addi r4, r0, 25
	add r9, r11, r0
	add r11, fp, r0
	add r10, r19, r0
	add lr, r15, r0
	add r15, r22, r0
	add r19, r25, r0
	add r22, r24, r0
	ldw r24, sp+36
	ldw r14, sp+44
	ldw r25, sp+40
	bgtu r3, r4, .LBB190_84
.LBB190_44:
	addi r5, r0, 1
	sll r6, r5, r3
	lui r7, 165
	addi r7, r7, -1669
	and r6, r6, r7
	beq r6, r0, .LBB190_83
.LBB190_45:
	add r1, r5, r0
	jal r0, .LBB190_48
.LBB190_46:
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
.LBB190_47:
	add r1, r0, r0
.LBB190_48:
	beq r1, r0, .LBB190_53
.LBB190_49:
	addi r1, r21, 1
	add r3, r21, r16
	stb r3+0, r17
	addi r28, r28, 1
	ldw r3, sp+52
	seq r3, r27, r3
	add r25, r25, r3
	bne r27, r10, .LBB190_51
.LBB190_50:
	addi r25, r25, -1
	seq r3, r25, r14
	sub r3, r0, r3
	andi r5, r3, 4
	add r21, r1, r0
	jal r0, .LBB190_52
.LBB190_51:
	add r21, r1, r0
	add r5, r0, r0
.LBB190_52:
	slli r1, r5, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB190_53:
	add r1, r28, r0
	add r4, r0, r0
	jal r0, .LBB190_57
.LBB190_54:
	ldbu r6, r3+0
.LBB190_55:
	addi r3, r0, 10
	mul r3, r4, r3
	add r3, r3, r5
	addi r4, r3, -48
	addi r3, r21, 1
	add r5, r21, r16
	stb r5+0, r6
	add r21, r3, r0
.LBB190_56:
	addi r1, r1, 1
.LBB190_57:
	add r3, r12, r1
	blt r13, r0, .LBB190_59
.LBB190_58:
	bgeu r1, r13, .LBB190_60
.LBB190_59:
	ldbu r5, r3+0
	addi r5, r5, -58
	andi r5, r5, 255
	addi r6, r0, 245
	bgtu r5, r6, .LBB190_63
.LBB190_60:
	blt r13, r0, .LBB190_62
.LBB190_61:
	bgeu r1, r13, .LBB190_72
.LBB190_62:
	ldbu r5, r3+0
	bne r5, r20, .LBB190_72
.LBB190_63:
	blt r13, r0, .LBB190_65
.LBB190_64:
	bgeu r1, r13, .LBB190_66
.LBB190_65:
	ldbu r5, r3+0
	beq r5, r20, .LBB190_56
.LBB190_66:
	bge r21, r23, .LBB190_82
.LBB190_67:
	blt r13, r0, .LBB190_69
.LBB190_68:
	add r5, r0, r0
	bgeu r1, r13, .LBB190_70
.LBB190_69:
	ldbu r5, r3+0
.LBB190_70:
	blt r13, r0, .LBB190_54
.LBB190_71:
	add r6, r0, r0
	bgeu r1, r13, .LBB190_55
	jal r0, .LBB190_54
.LBB190_72:
	blt r13, r0, .LBB190_74
.LBB190_73:
	bgeu r1, r13, .LBB190_75
.LBB190_74:
	ldbu r5, r3+0
	addi r6, r0, 72
	beq r5, r6, .LBB190_78
.LBB190_75:
	addi r5, r0, 5
	blt r13, r0, .LBB190_77
.LBB190_76:
	bgeu r1, r13, .LBB190_81
.LBB190_77:
	ldbu r6, r3+0
	addi r7, r0, 104
	bne r6, r7, .LBB190_81
.LBB190_78:
	beq r4, r0, .LBB190_85
.LBB190_79:
	add r5, r4, r21
	addi r6, r0, 1022
	blt r5, r6, .LBB190_86
.LBB190_80:
	lui r3, %hi(.L.str.106)
	addi r3, r3, %lo(.L.str.106)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB190_81:
	add r28, r1, r0
	jal r0, .LBB190_52
.LBB190_82:
	lui r3, %hi(.L.str.106)
	addi r3, r3, %lo(.L.str.106)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	add r21, r23, r0
	jalr r0, r3, 0
.LBB190_83:
	beq r3, r4, .LBB190_48
.LBB190_84:
	addi r1, r0, 46
	seq r1, r8, r1
	jal r0, .LBB190_48
.LBB190_85:
	lui r3, %hi(.L.str.107)
	addi r3, r3, %lo(.L.str.107)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB190_86:
	blt r13, r0, .LBB190_88
.LBB190_87:
	add r5, r0, r0
	bgeu r1, r13, .LBB190_89
.LBB190_88:
	ldbu r5, r3+0
.LBB190_89:
	add r6, r21, r16
	stb r6+0, r5
	addi r21, r21, 1
	addi r5, r0, 1
	blt r4, r5, .LBB190_99
.LBB190_90:
	addi r6, r4, 1
	add r4, r0, r0
	jal r0, .LBB190_93
.LBB190_91:
	ldbu r8, r7+1
.LBB190_92:
	add r7, r21, r4
	add r7, r7, r16
	stb r7+0, r8
	addi r6, r6, -1
	addi r4, r4, 1
	ble r6, r5, .LBB190_100
.LBB190_93:
	add r7, r1, r4
	addi r28, r7, 1
	blt r13, r0, .LBB190_95
.LBB190_94:
	bgeu r28, r13, .LBB190_98
.LBB190_95:
	add r7, r3, r4
	ldbu r8, r7+1
	beq r8, r0, .LBB190_98
.LBB190_96:
	blt r13, r0, .LBB190_91
.LBB190_97:
	add r8, r0, r0
	bgeu r28, r13, .LBB190_92
	jal r0, .LBB190_91
.LBB190_98:
	lui r1, %hi(.L.str.108)
	addi r1, r1, %lo(.L.str.108)
	stw r15+%lo(fio_fmt_why), r1
	add r21, r21, r4
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB190_99:
	addi r28, r1, 1
	jal r0, .LBB190_101
.LBB190_100:
	add r1, r1, r4
	addi r28, r1, 1
	add r21, r21, r4
.LBB190_101:
	stw lr+%lo(fio_ftext), r21
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB190_102:
	add r1, r21, r16
	stb r1+0, r0
	addi r14, r0, 1
	addi r19, r0, 58
	addi r24, r0, -1
	addi r21, r0, -10
	addi r22, r0, 9
	addi r23, r0, 10
	addi r25, r0, 72
	lui r27, %hi(.L.str.110)
	addi r27, r27, %lo(.L.str.110)
	addi r26, r0, 80
	addi r28, r0, 88
	addi r1, r0, 25
	stw sp+48, r1
	addi r1, r0, -4
	stw sp+28, r1
	addi r1, r0, 68
	stw sp+44, r1
	addi r1, r0, 69
	stw sp+40, r1
	addi r1, r0, 83
	stw sp+36, r1
	addi r1, r0, 78
	stw sp+32, r1
	jal r0, .LBB190_104
.LBB190_103:
	addi r14, r14, 1
.LBB190_104:
	add r1, r14, r16
	ldbu r1, r1+0
	bgtu r1, r19, .LBB190_116
.LBB190_105:
	slli r3, r1, 2
	lui r4, %hi(.LJTI190_2)
	addi r4, r4, %lo(.LJTI190_2)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB190_106:
	addi fp, r14, 1
	add r1, r14, r18
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB190_112
.LBB190_107:
	addi r20, r0, -1
	jal r0, .LBB190_114
.LBB190_108:
	addi r14, r14, 1
.LBB190_109:
	add r1, r14, r0
	add r3, r14, r16
	ldbu r4, r3+0
	addi r14, r14, 1
	addi r3, r0, 39
	bne r4, r3, .LBB190_109
.LBB190_110:
	add r4, r14, r16
	ldbu r4, r4+0
	bne r4, r3, .LBB190_104
.LBB190_111:
	addi r14, r1, 2
	jal r0, .LBB190_109
.LBB190_112:
	add r20, r0, r0
	addi r1, r1, -48
	bgtu r1, r22, .LBB190_114
.LBB190_113:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB190_113
.LBB190_114:
	lui r14, %hi(.L.str.109)
	addi r14, r14, %lo(.L.str.109)
	blt r20, r0, .LBB190_199
.LBB190_115:
	add r1, fp, r16
	ldbu r3, r1+0
	jal r31, toupper
	beq r1, r26, .LBB190_122
	jal r0, .LBB190_199
.LBB190_116:
	slli r1, r1, 24
	srai r1, r1, 24
	addi r3, r1, -58
	bgeu r3, r21, .LBB190_118
.LBB190_117:
	add fp, r14, r0
	add r20, r24, r0
	jal r0, .LBB190_122
.LBB190_118:
	addi r1, r1, -48
	bleu r1, r22, .LBB190_120
.LBB190_119:
	add fp, r14, r0
	add r20, r0, r0
	jal r0, .LBB190_122
.LBB190_120:
	add r20, r0, r0
	add fp, r14, r0
.LBB190_121:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB190_121
.LBB190_122:
	add r14, fp, r16
	ldbu r3, r14+0
	jal r31, toupper
	bne r1, r25, .LBB190_125
.LBB190_123:
	blt r20, r0, .LBB190_198
.LBB190_124:
	add r1, fp, r20
	addi r14, r1, 1
	jal r0, .LBB190_104
.LBB190_125:
	blt r20, r0, .LBB190_128
.LBB190_126:
	ldw r3, sp+52
	bne r1, r3, .LBB190_128
.LBB190_127:
	addi r14, fp, 1
	jal r0, .LBB190_104
.LBB190_128:
	beq r1, r28, .LBB190_139
.LBB190_129:
	sgt r3, r20, r24
	seq r4, r1, r26
	and r3, r3, r4
	bne r3, r0, .LBB190_139
.LBB190_130:
	addi r17, r1, -65
	ldw r3, sp+48
	bgtu r17, r3, .LBB190_160
.LBB190_131:
	slli r3, r17, 2
	lui r4, %hi(.LJTI190_3)
	addi r4, r4, %lo(.LJTI190_3)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB190_132:
	addi r14, fp, 1
	add r3, fp, r18
	ldb r3, r3+0
	addi r3, r3, -58
	bltu r3, r21, .LBB190_134
.LBB190_133:
	add r3, r14, r0
	addi r14, r14, 1
	add r3, r3, r18
	ldb r3, r3+0
	addi r3, r3, -58
	addi r4, r0, -11
	bgtu r3, r4, .LBB190_133
.LBB190_134:
	addi r3, r0, 65
	beq r1, r3, .LBB190_104
.LBB190_135:
	add r1, r14, r16
	ldbu r1, r1+0
	addi r3, r0, 46
	bne r1, r3, .LBB190_104
.LBB190_136:
	addi r1, r14, 1
	add r3, r14, r18
	ldb r3, r3+0
	addi r3, r3, -58
	add r14, r1, r0
	bltu r3, r21, .LBB190_104
.LBB190_137:
	add r14, r1, r0
.LBB190_138:
	add r1, r14, r0
	addi r14, r14, 1
	add r1, r1, r18
	ldb r1, r1+0
	addi r1, r1, -58
	bgeu r1, r21, .LBB190_138
	jal r0, .LBB190_104
.LBB190_139:
	addi r14, fp, 1
	jal r0, .LBB190_104
.LBB190_140:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 82
	beq r1, r3, .LBB190_142
.LBB190_141:
	addi r3, r0, 76
	bne r1, r3, .LBB190_149
.LBB190_142:
	addi r14, fp, 2
	jal r0, .LBB190_150
.LBB190_143:
	stw sp+24, r24
	add r24, r28, r0
	add r28, r27, r0
	stw sp+20, r1
	add r27, fp, r18
	ldbu r3, r27+0
	jal r31, toupper
	ldw r3, sp+32
	beq r1, r3, .LBB190_145
.LBB190_144:
	ldbu r3, r27+0
	jal r31, toupper
	addi r3, r0, 90
	bne r1, r3, .LBB190_158
.LBB190_145:
	addi r14, fp, 2
	add r27, r28, r0
	add r28, r24, r0
	ldw r24, sp+24
	jal r0, .LBB190_104
.LBB190_146:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	ldw r3, sp+36
	beq r1, r3, .LBB190_148
.LBB190_147:
	addi r14, fp, 1
	bne r1, r26, .LBB190_104
.LBB190_148:
	addi r14, fp, 2
	jal r0, .LBB190_104
.LBB190_149:
	addi r14, fp, 1
.LBB190_150:
	add r1, r14, r16
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB190_152
.LBB190_151:
	addi r1, r0, 1
	jal r0, .LBB190_157
.LBB190_152:
	addi r1, r1, -48
	bleu r1, r22, .LBB190_154
.LBB190_153:
	add r1, r0, r0
	jal r0, .LBB190_157
.LBB190_154:
	add r3, r0, r0
.LBB190_155:
	add r4, r14, r0
	mul r3, r3, r23
	add r3, r3, r1
	addi r14, r14, 1
	add r1, r4, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB190_155
.LBB190_156:
	srli r1, r3, 31
.LBB190_157:
	beq r1, r0, .LBB190_104
	jal r0, .LBB190_214
.LBB190_158:
	ldw r1, sp+20
	add r27, r28, r0
	add r28, r24, r0
	ldw r24, sp+24
	ldw r3, sp+48
	bgtu r17, r3, .LBB190_160
.LBB190_159:
	addi r3, r0, 1
	sll r3, r3, r17
	lui r4, 8196
	addi r4, r4, 3
	and r3, r3, r4
	bne r3, r0, .LBB190_132
.LBB190_160:
	ldw r3, sp+28
	and r3, r1, r3
	ldw r4, sp+44
	beq r3, r4, .LBB190_163
.LBB190_161:
	addi r3, r0, 76
	beq r1, r3, .LBB190_163
.LBB190_162:
	addi r3, r0, 73
	bne r1, r3, .LBB190_216
.LBB190_163:
	addi r17, fp, 1
	ldw r3, sp+40
	bne r1, r3, .LBB190_167
.LBB190_164:
	add r20, r17, r16
	ldbu r3, r20+0
	add r14, r1, r0
	jal r31, toupper
	add r3, r1, r0
	add r1, r14, r0
	ldw r4, sp+36
	beq r3, r4, .LBB190_166
.LBB190_165:
	ldbu r3, r20+0
	jal r31, toupper
	add r3, r1, r0
	add r1, r14, r0
	ldw r4, sp+32
	bne r3, r4, .LBB190_167
.LBB190_166:
	addi r17, fp, 2
.LBB190_167:
	add r3, r17, r16
	ldb r3, r3+0
	addi r4, r3, -58
	bltu r4, r21, .LBB190_212
.LBB190_168:
	addi r3, r3, -48
	bleu r3, r22, .LBB190_170
.LBB190_169:
	add r3, r0, r0
	jal r0, .LBB190_173
.LBB190_170:
	add r4, r0, r0
.LBB190_171:
	add r5, r17, r0
	mul r4, r4, r23
	add r4, r4, r3
	addi r17, r17, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB190_171
.LBB190_172:
	srli r3, r4, 31
.LBB190_173:
	bne r3, r0, .LBB190_212
.LBB190_174:
	add r3, r17, r16
	ldbu r3, r3+0
	addi r4, r0, 46
	bne r3, r4, .LBB190_177
.LBB190_175:
	addi r14, r17, 1
	add r3, r17, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB190_178
.LBB190_176:
	addi r3, r0, 1
	jal r0, .LBB190_183
.LBB190_177:
	add r14, r17, r0
	jal r0, .LBB190_184
.LBB190_178:
	addi r3, r3, -48
	bleu r3, r22, .LBB190_180
.LBB190_179:
	add r3, r0, r0
	jal r0, .LBB190_183
.LBB190_180:
	add r4, r0, r0
.LBB190_181:
	add r5, r14, r0
	mul r4, r4, r23
	add r4, r4, r3
	addi r14, r14, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB190_181
.LBB190_182:
	srli r3, r4, 31
.LBB190_183:
	bne r3, r0, .LBB190_215
.LBB190_184:
	addi r3, r0, 71
	beq r1, r3, .LBB190_186
.LBB190_185:
	andi r1, r1, 78
	ldw r3, sp+44
	bne r1, r3, .LBB190_104
.LBB190_186:
	add r1, r14, r16
	ldbu r3, r1+0
	jal r31, toupper
	ldw r3, sp+40
	bne r1, r3, .LBB190_104
.LBB190_187:
	addi r1, r14, 1
	add r3, r14, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB190_189
.LBB190_188:
	addi r3, r0, 1
	jal r0, .LBB190_194
.LBB190_189:
	addi r3, r3, -48
	bleu r3, r22, .LBB190_191
.LBB190_190:
	add r14, r1, r0
	add r3, r0, r0
	jal r0, .LBB190_195
.LBB190_191:
	add r4, r0, r0
.LBB190_192:
	add r5, r1, r0
	mul r1, r4, r23
	add r4, r1, r3
	addi r1, r5, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB190_192
.LBB190_193:
	srli r3, r4, 31
.LBB190_194:
	add r14, r1, r0
.LBB190_195:
	beq r3, r0, .LBB190_104
.LBB190_196:
	lui r14, %hi(.L.str.114)
	addi r14, r14, %lo(.L.str.114)
	jal r0, .LBB190_199
.LBB190_197:
	lui r1, %hi(fio_fmt)
	stw r1+%lo(fio_fmt), r16
	add r11, r0, r0
	jal r0, .LBB190_211
.LBB190_198:
	add r14, r27, r0
.LBB190_199:
	stw r15+%lo(fio_fmt_why), r14
.LBB190_200:
	addi r1, r0, 79
	addi r3, r0, 40
	addi r4, r0, 41
	add r6, r0, r0
	add r5, r0, r0
	jal r0, .LBB190_202
.LBB190_201:
	addi r6, r6, 1
.LBB190_202:
	blt r13, r0, .LBB190_204
.LBB190_203:
	add r7, r0, r0
	bgeu r6, r13, .LBB190_205
.LBB190_204:
	add r7, r12, r6
	ldbu r7, r7+0
.LBB190_205:
	andi r7, r7, 255
	beq r7, r0, .LBB190_210
.LBB190_206:
	bgtu r6, r1, .LBB190_210
.LBB190_207:
	seq r8, r7, r3
	add r5, r5, r8
	bne r7, r4, .LBB190_201
.LBB190_208:
	addi r5, r5, -1
	bne r5, r0, .LBB190_201
.LBB190_209:
	addi r6, r6, 1
.LBB190_210:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	ldw r5, r15+%lo(fio_fmt_why)
	lui r4, %hi(.L.str.118)
	addi r4, r4, %lo(.L.str.118)
	add r7, r12, r0
	jal r31, fprintf
.LBB190_211:
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
.LBB190_212:
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.112)
	addi r4, r4, %lo(.L.str.112)
	add r3, r14, r0
	add r5, r1, r0
.LBB190_213:
	jal r31, sprintf
	jal r0, .LBB190_199
.LBB190_214:
	lui r14, %hi(.L.str.111)
	addi r14, r14, %lo(.L.str.111)
	jal r0, .LBB190_199
.LBB190_215:
	lui r14, %hi(.L.str.113)
	addi r14, r14, %lo(.L.str.113)
	jal r0, .LBB190_199
.LBB190_216:
	blt r20, r0, .LBB190_219
.LBB190_217:
	addi r1, r1, -41
	addi r3, r0, 17
	bgtu r1, r3, .LBB190_220
.LBB190_218:
	slli r1, r1, 2
	lui r3, %hi(.Lswitch.table.fio_fmt_use)
	addi r3, r3, %lo(.Lswitch.table.fio_fmt_use)
	add r1, r1, r3
	ldw r14, r1+0
	jal r0, .LBB190_199
.LBB190_219:
	ldb r5, r14+0
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.117)
	addi r4, r4, %lo(.L.str.117)
	add r3, r14, r0
	jal r0, .LBB190_213
.LBB190_220:
	lui r14, %hi(.L.str.116)
	addi r14, r14, %lo(.L.str.116)
	jal r0, .LBB190_199
.LBB190_221:
.Lfunc_end190:
	.size	fio_fmt_use, .Lfunc_end190-fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI190_0,@object
.LJTI190_0:
	.word	.LBB190_37
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_36
	.word	.LBB190_38
	.word	.LBB190_16
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_38
	.word	.LBB190_16
	.size	.LJTI190_0, 160
	.type	.LJTI190_1,@object
.LJTI190_1:
	.word	.LBB190_9
	.word	.LBB190_200
	.word	.LBB190_221
	.word	.LBB190_221
	.word	.LBB190_102
	.word	.LBB190_9
	.size	.LJTI190_1, 24
	.type	.LJTI190_2,@object
.LJTI190_2:
	.word	.LBB190_197
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_108
	.word	.LBB190_103
	.word	.LBB190_103
	.word	.LBB190_116
	.word	.LBB190_106
	.word	.LBB190_103
	.word	.LBB190_106
	.word	.LBB190_116
	.word	.LBB190_103
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_116
	.word	.LBB190_103
	.size	.LJTI190_2, 236
	.type	.LJTI190_3,@object
.LJTI190_3:
	.word	.LBB190_132
	.word	.LBB190_143
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_132
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_146
	.word	.LBB190_140
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_160
	.word	.LBB190_132
	.size	.LJTI190_3, 104
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
	bleu r5, r6, .LBB191_2
.LBB191_1:
	add r1, r0, r0
	jal r0, .LBB191_5
.LBB191_2:
	addi r6, r1, 1
	addi r7, r0, 10
	add r1, r0, r0
.LBB191_3:
	mul r1, r1, r7
	addi r8, r4, 1
	add r1, r5, r1
	add r4, r6, r4
	ldb r4, r4+0
	addi r5, r4, -48
	add r4, r8, r0
	bltu r5, r7, .LBB191_3
.LBB191_4:
	stw r3+%lo(fio_pos), r8
.LBB191_5:
	jalr r0, r31, 0
.Lfunc_end191:
	.size	fio_number, .Lfunc_end191-fio_number
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
	beq r1, r0, .LBB192_3
.LBB192_1:
	lui r11, %hi(fio_irec)
	ldw r3, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r3, r4, .LBB192_28
.LBB192_2:
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
	jal r0, .LBB192_27
.LBB192_3:
	lui r12, %hi(fio_unit)
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	addi r13, r0, 98
	lui r11, %hi(fio_in)
	bgtu r3, r13, .LBB192_7
.LBB192_4:
	slli r1, r1, 2
	lui r14, %hi(fio_uend)
	addi r14, r14, %lo(fio_uend)
	add r1, r1, r14
	ldw r1, r1+0
	blt r1, r0, .LBB192_7
.LBB192_5:
	ldw r3, r11+%lo(fio_in)
	beq r3, r0, .LBB192_7
.LBB192_6:
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	add r4, r3, r14
	ldw r4, r4+0
	bge r1, r4, .LBB192_30
.LBB192_7:
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	bgtu r3, r13, .LBB192_11
.LBB192_8:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB192_11
.LBB192_9:
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
	beq r3, r0, .LBB192_29
.LBB192_10:
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB192_26
.LBB192_11:
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
.LBB192_12:
	add r18, r3, r0
	add r3, r11, r0
	jal r31, getc
	beq r1, r16, .LBB192_17
.LBB192_13:
	addi r3, r18, 1
	add r4, r18, r15
	stb r4+0, r1
	beq r1, r14, .LBB192_15
.LBB192_14:
	bltu r18, r17, .LBB192_12
.LBB192_15:
	beq r3, r0, .LBB192_18
.LBB192_16:
	add r1, r3, r15
	stb r1+0, r0
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB192_20
.LBB192_17:
	add r3, r18, r0
	bne r3, r0, .LBB192_16
.LBB192_18:
	ldw r1, r12+%lo(fio_unit)
	addi r4, r1, -1
	bgtu r4, r13, .LBB192_20
.LBB192_19:
	slli r1, r1, 2
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r1, r1, r4
	addi r4, r0, 1
	stw r1+0, r4
.LBB192_20:
	beq r3, r0, .LBB192_28
.LBB192_21:
	lui r1, %hi(fio_rlen)
	ldw r4, r1+%lo(fio_rlen)
	addi r3, r0, 1
	blt r4, r3, .LBB192_26
.LBB192_22:
	addi r4, r4, 1
	lui r5, %hi(fio_rec-2)
	addi r5, r5, %lo(fio_rec-2)
	addi r6, r0, 13
	jal r0, .LBB192_24
.LBB192_23:
	addi r7, r4, -2
	stw r1+%lo(fio_rlen), r7
	addi r4, r4, -1
	ble r4, r3, .LBB192_26
.LBB192_24:
	add r7, r4, r5
	ldbu r7, r7+0
	beq r7, r6, .LBB192_23
.LBB192_25:
	beq r7, r14, .LBB192_23
.LBB192_26:
	lui r1, %hi(fio_rpos)
	stw r1+%lo(fio_rpos), r0
.LBB192_27:
	add r1, r0, r0
	jal r0, .LBB192_29
.LBB192_28:
	addi r1, r0, 1
.LBB192_29:
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
.LBB192_30:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r3, r3, r1
	addi r1, r0, 1
	stw r3+0, r1
	jal r0, .LBB192_29
.Lfunc_end192:
	.size	fio_getrec, .Lfunc_end192-fio_getrec
                                        # -- End function
	.p2align	2                               # -- Begin function fio_field_in
	.type	fio_field_in,@function
fio_field_in:                           # @fio_field_in
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
	addi r5, r0, 1
	lui r1, %hi(fio_rpos)
	lui r6, %hi(fio_rlen)
	blt r4, r5, .LBB193_14
.LBB193_1:
	ldw r7, r1+%lo(fio_rpos)
	ldw r9, r6+%lo(fio_rlen)
	lui r8, %hi(fio_bz)
	ldw r10, r8+%lo(fio_bz)
	addi lr, r0, 32
	lui r11, %hi(fio_rec)
	addi r11, r11, %lo(fio_rec)
	addi r12, r0, 9
	addi r13, r0, 48
	addi r14, r0, 1023
	addi r15, r0, 44
	addi r16, r0, 1022
	add r18, r0, r0
	add r8, r0, r0
	add r17, r0, r0
	jal r0, .LBB193_5
.LBB193_2:
	addi r18, r8, 1
	add r8, r3, r8
	stb r8+0, r19
	add r8, r18, r0
.LBB193_3:
	add r18, r5, r0
.LBB193_4:
	addi r17, r17, 1
	beq r4, r17, .LBB193_16
.LBB193_5:
	add r20, r7, r17
	add r19, lr, r0
	bge r20, r9, .LBB193_7
.LBB193_6:
	add r19, r20, r11
	ldb r19, r19+0
.LBB193_7:
	beq r19, r12, .LBB193_11
.LBB193_8:
	beq r19, lr, .LBB193_11
.LBB193_9:
	beq r19, r15, .LBB193_15
.LBB193_10:
	ble r8, r16, .LBB193_2
	jal r0, .LBB193_3
.LBB193_11:
	beq r10, r0, .LBB193_4
.LBB193_12:
	beq r18, r0, .LBB193_4
.LBB193_13:
	add r19, r13, r0
	blt r8, r14, .LBB193_2
	jal r0, .LBB193_4
.LBB193_14:
	add r8, r0, r0
	jal r0, .LBB193_16
.LBB193_15:
	addi r4, r17, 1
.LBB193_16:
	add r3, r3, r8
	stb r3+0, r0
	lui r3, %hi(fio_noadv)
	ldbu r3, r3+%lo(fio_noadv)
	bne r3, r5, .LBB193_20
.LBB193_17:
	lui r3, %hi(fio_reading)
	ldbu r3, r3+%lo(fio_reading)
	andi r3, r3, 1
	beq r3, r0, .LBB193_20
.LBB193_18:
	ldw r3, r6+%lo(fio_rlen)
	ldw r6, r1+%lo(fio_rpos)
	sub r3, r3, r6
	addi r6, r0, 0
	sgt r6, r3, r6
	sub r6, r0, r6
	and r3, r3, r6
	xor r6, r4, r3
	slt r7, r4, r3
	sub r7, r0, r7
	and r6, r6, r7
	xor r6, r3, r6
	lui r7, %hi(fio_size)
	ldw r8, r7+%lo(fio_size)
	add r6, r6, r8
	stw r7+%lo(fio_size), r6
	ble r4, r3, .LBB193_20
.LBB193_19:
	lui r3, %hi(fio_eor)
	stb r3+%lo(fio_eor), r5
.LBB193_20:
	ldw r3, r1+%lo(fio_rpos)
	add r3, r3, r4
	stw r1+%lo(fio_rpos), r3
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
.Lfunc_end193:
	.size	fio_field_in, .Lfunc_end193-fio_field_in
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
	ble r1, r3, .LBB194_5
.LBB194_1:
	addi r4, r0, 0
	seq r4, r3, r4
	slli r3, r3, 1
	lui r5, 1
	xor r5, r3, r5
	sub r4, r0, r4
	and r4, r5, r4
	xor r3, r3, r4
.LBB194_2:
	add r13, r3, r0
	slli r3, r3, 1
	blt r13, r1, .LBB194_2
.LBB194_3:
	ldw r3, r15+%lo(nml_text)
	add r4, r13, r0
	jal r31, realloc
	beq r1, r0, .LBB194_6
.LBB194_4:
	stw r15+%lo(nml_text), r1
	stw r16+%lo(nml_tcap), r13
.LBB194_5:
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
.LBB194_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.144)
	addi r4, r4, %lo(.L.str.144)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end194:
	.size	nml_text_add, .Lfunc_end194-nml_text_add
                                        # -- End function
	.type	f77_absent,@object              # @f77_absent
	.section	.rodata,"a",@progbits
	.globl	f77_absent
	.p2align	2, 0x0
f77_absent:
	.zero	16
	.size	f77_absent, 16

	.type	fio_noadv,@object               # @fio_noadv
	.local	fio_noadv
	.comm	fio_noadv,1,4
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"f77: ADVANCE= is 'YES' or 'NO'\n"
	.size	.L.str, 32

	.type	fio_sizep,@object               # @fio_sizep
	.local	fio_sizep
	.comm	fio_sizep,4,4
	.type	fio_eor,@object                 # @fio_eor
	.local	fio_eor
	.comm	fio_eor,1,4
	.type	fio_size,@object                # @fio_size
	.local	fio_size
	.comm	fio_size,4,4
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
	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"f77: unformatted record too long\n"
	.size	.L.str.1, 34

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
	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"f77: unformatted record longer than RECL\n"
	.size	.L.str.2, 42

	.type	fio_uend,@object                # @fio_uend
	.local	fio_uend
	.comm	fio_uend,400,4
	.type	fio_upos,@object                # @fio_upos
	.local	fio_upos
	.comm	fio_upos,4,4
	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"f77: unformatted READ past the end of the record\n"
	.size	.L.str.3, 50

	.type	fio_pend,@object                # @fio_pend
	.local	fio_pend
	.comm	fio_pend,400,4
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
	.type	fio_w,@object                   # @fio_w
	.local	fio_w
	.comm	fio_w,4,4
	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"0123456789ABCDEF"
	.size	.L.str.4, 17

	.type	fio_d,@object                   # @fio_d
	.local	fio_d
	.comm	fio_d,4,4
	.type	fio_fmt,@object                 # @fio_fmt
	.local	fio_fmt
	.comm	fio_fmt,4,4
	.type	fio_pos,@object                 # @fio_pos
	.local	fio_pos
	.comm	fio_pos,4,4
	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"%u"
	.size	.L.str.5, 3

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"%s%0*d%s"
	.size	.L.str.6, 9

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"-"
	.size	.L.str.7, 2

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.zero	1
	.size	.L.str.8, 1

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"%s%s"
	.size	.L.str.9, 5

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"%d"
	.size	.L.str.10, 3

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"NaN"
	.size	.L.str.11, 4

	.type	fio_sp,@object                  # @fio_sp
	.local	fio_sp
	.comm	fio_sp,1,4
	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"Infinity"
	.size	.L.str.12, 9

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"Inf"
	.size	.L.str.13, 4

	.type	fio_scale,@object               # @fio_scale
	.local	fio_scale
	.comm	fio_scale,4,4
	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"%%.%df"
	.size	.L.str.14, 7

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"."
	.size	.L.str.15, 2

	.type	fio_dgiven,@object              # @fio_dgiven
	.local	fio_dgiven
	.comm	fio_dgiven,1,4
	.type	fio_single,@object              # @fio_single
	.local	fio_single
	.comm	fio_single,1,4
	.type	fio_e,@object                   # @fio_e
	.local	fio_e
	.comm	fio_e,4,4
	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"%.*E"
	.size	.L.str.16, 5

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"%g"
	.size	.L.str.17, 3

	.type	fio_udelim,@object              # @fio_udelim
	.local	fio_udelim
	.comm	fio_udelim,400,4
	.type	fio_first_item,@object          # @fio_first_item
	.local	fio_first_item
	.comm	fio_first_item,1,4
	.type	fio_prev_char,@object           # @fio_prev_char
	.local	fio_prev_char
	.comm	fio_prev_char,1,4
	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"f77: ALLOCATE of an array already allocated\n"
	.size	.L.str.18, 45

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"f77: ALLOCATE: out of memory (%d bytes)\n"
	.size	.L.str.19, 41

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"f77: DEALLOCATE of an array not allocated\n"
	.size	.L.str.20, 43

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"f77: %.*s is not allocated: Fortran 95 does not allocate an array by assigning to it (Fortran 2003 does)\n"
	.size	.L.str.21, 106

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"f77: out of memory copying an ALLOCATABLE component\n"
	.size	.L.str.22, 53

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"f77: out of memory for an array argument (%d bytes)\n"
	.size	.L.str.23, 53

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"f77: NEAREST with S = 0\n"
	.size	.L.str.24, 25

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"f77: MODULO with P = 0\n"
	.size	.L.str.25, 24

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"%04d%02d%02d"
	.size	.L.str.26, 13

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"%02d%02d%02d.%03d"
	.size	.L.str.27, 18

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"+0000"
	.size	.L.str.28, 6

	.type	f77_rng,@object                 # @f77_rng
	.data
	.p2align	2, 0x0
f77_rng:
	.word	2654435769                      # 0x9e3779b9
	.word	608135816                       # 0x243f6a88
	.word	3084996962                      # 0xb7e15162
	.word	2242054355                      # 0x85a308d3
	.size	f77_rng, 16

	.type	.L.str.29,@object               # @.str.29
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.29:
	.asciz	"f77: RANDOM_SEED's PUT and GET have 4 elements at least\n"
	.size	.L.str.29, 57

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"f77: DATE_AND_TIME's VALUES has 8 elements at least\n"
	.size	.L.str.30, 53

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"f77: concatenation result of %d bytes is too long\n"
	.size	.L.str.31, 51

	.type	fio_catpos,@object              # @fio_catpos
	.local	fio_catpos
	.comm	fio_catpos,4,4
	.type	fio_catring,@object             # @fio_catring
	.local	fio_catring
	.comm	fio_catring,65536,1
	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"f77: REPEAT with NCOPIES %d, less than 0\n"
	.size	.L.str.32, 42

	.type	fio_desc,@object                # @fio_desc
	.local	fio_desc
	.comm	fio_desc,4,4
	.type	fio_rep,@object                 # @fio_rep
	.local	fio_rep
	.comm	fio_rep,4,4
	.type	fio_draining,@object            # @fio_draining
	.local	fio_draining
	.comm	fio_draining,1,4
	.type	fio_open_rec,@object            # @fio_open_rec
	.local	fio_open_rec
	.comm	fio_open_rec,4,4
	.type	fio_ufile,@object               # @fio_ufile
	.local	fio_ufile
	.comm	fio_ufile,400,4
	.type	fio_uwrote,@object              # @fio_uwrote
	.local	fio_uwrote
	.comm	fio_uwrote,400,4
	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"PAUSE %.*s\n"
	.size	.L.str.33, 12

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
	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"f77: %c editing does not match a numeric READ item\n"
	.size	.L.str.34, 52

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"f77: %c editing does not match a CHARACTER READ item\n"
	.size	.L.str.35, 54

	.type	fio_rpos,@object                # @fio_rpos
	.local	fio_rpos
	.comm	fio_rpos,4,4
	.type	fio_rlen,@object                # @fio_rlen
	.local	fio_rlen
	.comm	fio_rlen,4,4
	.type	fio_rec,@object                 # @fio_rec
	.local	fio_rec
	.comm	fio_rec,1024,1
	.type	fio_psave,@object               # @fio_psave
	.local	fio_psave
	.comm	fio_psave,400,4
	.type	fio_plen,@object                # @fio_plen
	.local	fio_plen
	.comm	fio_plen,400,4
	.type	fio_ppos,@object                # @fio_ppos
	.local	fio_ppos
	.comm	fio_ppos,400,4
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
	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"(I11)"
	.size	.L.str.36, 6

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"T"
	.size	.L.str.37, 2

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"F"
	.size	.L.str.38, 2

	.type	nml_asg,@object                 # @nml_asg
	.local	nml_asg
	.comm	nml_asg,4,4
	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"the subscripts do not match the array"
	.size	.L.str.39, 38

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"a subscript is out of range"
	.size	.L.str.40, 28

	.type	nml_val,@object                 # @nml_val
	.local	nml_val
	.comm	nml_val,4,4
	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"more values than the item has elements"
	.size	.L.str.41, 39

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	" &END"
	.size	.L.str.42, 6

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"%s is no item of the NAMELIST"
	.size	.L.str.43, 30

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"f77: end of file on unit %d looking for NAMELIST &%s\n"
	.size	.L.str.44, 54

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"f77: NAMELIST &%s on unit %d: %s\n"
	.size	.L.str.45, 34

	.type	fio_o_pos,@object               # @fio_o_pos
	.local	fio_o_pos
	.comm	fio_o_pos,4,4
	.type	fio_o_poslen,@object            # @fio_o_poslen
	.local	fio_o_poslen
	.comm	fio_o_poslen,4,4
	.type	fio_o_act,@object               # @fio_o_act
	.local	fio_o_act
	.comm	fio_o_act,4,4
	.type	fio_o_actlen,@object            # @fio_o_actlen
	.local	fio_o_actlen
	.comm	fio_o_actlen,4,4
	.type	fio_o_pad,@object               # @fio_o_pad
	.local	fio_o_pad
	.comm	fio_o_pad,4,4
	.type	fio_o_padlen,@object            # @fio_o_padlen
	.local	fio_o_padlen
	.comm	fio_o_padlen,4,4
	.type	fio_o_delim,@object             # @fio_o_delim
	.local	fio_o_delim
	.comm	fio_o_delim,4,4
	.type	fio_o_delimlen,@object          # @fio_o_delimlen
	.local	fio_o_delimlen
	.comm	fio_o_delimlen,4,4
	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"f77: cannot OPEN unit %d\n"
	.size	.L.str.46, 26

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"DIRECT"
	.size	.L.str.47, 7

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"UNFORMATTED"
	.size	.L.str.48, 12

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"SEQUENTIAL"
	.size	.L.str.49, 11

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"FORMATTED"
	.size	.L.str.50, 10

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"f77: OPEN %s='%.*s' is not supported\n"
	.size	.L.str.51, 38

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"ACCESS"
	.size	.L.str.52, 7

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"FORM"
	.size	.L.str.53, 5

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"SCRATCH"
	.size	.L.str.54, 8

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"f77: OPEN ACCESS='DIRECT' needs RECL=\n"
	.size	.L.str.55, 39

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"f77: bad FILE= name in OPEN\n"
	.size	.L.str.56, 29

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"fort.%d"
	.size	.L.str.57, 8

	.type	fio_ufname,@object              # @fio_ufname
	.local	fio_ufname
	.comm	fio_ufname,25600,1
	.type	.L.str.58,@object               # @.str.58
.L.str.58:
	.asciz	"ZERO"
	.size	.L.str.58, 5

	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"f77: OPEN cannot create a scratch file\n"
	.size	.L.str.59, 40

	.type	.L.str.60,@object               # @.str.60
.L.str.60:
	.asciz	"OLD"
	.size	.L.str.60, 4

	.type	.L.str.61,@object               # @.str.61
.L.str.61:
	.asciz	"r+"
	.size	.L.str.61, 3

	.type	.L.str.62,@object               # @.str.62
.L.str.62:
	.asciz	"r"
	.size	.L.str.62, 2

	.type	.L.str.63,@object               # @.str.63
.L.str.63:
	.asciz	"f77: OPEN STATUS='OLD': no such file '%s'\n"
	.size	.L.str.63, 43

	.type	.L.str.64,@object               # @.str.64
.L.str.64:
	.asciz	"NEW"
	.size	.L.str.64, 4

	.type	.L.str.65,@object               # @.str.65
.L.str.65:
	.asciz	"f77: OPEN STATUS='NEW': '%s' already exists\n"
	.size	.L.str.65, 45

	.type	.L.str.66,@object               # @.str.66
.L.str.66:
	.asciz	"w+"
	.size	.L.str.66, 3

	.type	.L.str.67,@object               # @.str.67
.L.str.67:
	.asciz	"f77: OPEN cannot create '%s'\n"
	.size	.L.str.67, 30

	.type	.L.str.68,@object               # @.str.68
.L.str.68:
	.asciz	"REPLACE"
	.size	.L.str.68, 8

	.type	.L.str.69,@object               # @.str.69
.L.str.69:
	.asciz	"UNKNOWN"
	.size	.L.str.69, 8

	.type	.L.str.70,@object               # @.str.70
.L.str.70:
	.asciz	"f77: OPEN cannot open '%s'\n"
	.size	.L.str.70, 28

	.type	.L.str.71,@object               # @.str.71
.L.str.71:
	.asciz	"f77: OPEN STATUS value is not supported\n"
	.size	.L.str.71, 41

	.type	fio_uaction,@object             # @fio_uaction
	.local	fio_uaction
	.comm	fio_uaction,400,4
	.type	fio_uposn,@object               # @fio_uposn
	.local	fio_uposn
	.comm	fio_uposn,400,4
	.type	.L.str.72,@object               # @.str.72
.L.str.72:
	.asciz	"READ"
	.size	.L.str.72, 5

	.type	.L.str.73,@object               # @.str.73
.L.str.73:
	.asciz	"WRITE"
	.size	.L.str.73, 6

	.type	.L.str.74,@object               # @.str.74
.L.str.74:
	.asciz	"APPEND"
	.size	.L.str.74, 7

	.type	.L.str.75,@object               # @.str.75
.L.str.75:
	.asciz	"REWIND"
	.size	.L.str.75, 7

	.type	.L.str.76,@object               # @.str.76
.L.str.76:
	.asciz	"ASIS"
	.size	.L.str.76, 5

	.type	.L.str.77,@object               # @.str.77
.L.str.77:
	.asciz	"f77: OPEN POSITION= is ASIS, REWIND or APPEND\n"
	.size	.L.str.77, 47

	.type	.L.str.78,@object               # @.str.78
.L.str.78:
	.asciz	"READWRITE"
	.size	.L.str.78, 10

	.type	.L.str.79,@object               # @.str.79
.L.str.79:
	.asciz	"f77: OPEN ACTION= is READ, WRITE or READWRITE\n"
	.size	.L.str.79, 47

	.type	.L.str.80,@object               # @.str.80
.L.str.80:
	.asciz	"YES"
	.size	.L.str.80, 4

	.type	.L.str.81,@object               # @.str.81
.L.str.81:
	.asciz	"f77: OPEN PAD='NO' is not supported\n"
	.size	.L.str.81, 37

	.type	.L.str.82,@object               # @.str.82
.L.str.82:
	.asciz	"APOSTROPHE"
	.size	.L.str.82, 11

	.type	.L.str.83,@object               # @.str.83
.L.str.83:
	.asciz	"QUOTE"
	.size	.L.str.83, 6

	.type	.L.str.84,@object               # @.str.84
.L.str.84:
	.asciz	"NONE"
	.size	.L.str.84, 5

	.type	.L.str.85,@object               # @.str.85
.L.str.85:
	.asciz	"f77: OPEN DELIM= is APOSTROPHE, QUOTE or NONE\n"
	.size	.L.str.85, 47

	.type	.L.str.86,@object               # @.str.86
.L.str.86:
	.asciz	"DELETE"
	.size	.L.str.86, 7

	.type	.L.str.87,@object               # @.str.87
.L.str.87:
	.asciz	"KEEP"
	.size	.L.str.87, 5

	.type	.L.str.88,@object               # @.str.88
.L.str.88:
	.asciz	"f77: CLOSE STATUS='%.*s' is not supported\n"
	.size	.L.str.88, 43

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
	.type	inq_action,@object              # @inq_action
	.local	inq_action
	.comm	inq_action,4,4
	.type	inq_posn,@object                # @inq_posn
	.local	inq_posn
	.comm	inq_posn,4,4
	.type	inq_delim,@object               # @inq_delim
	.local	inq_delim
	.comm	inq_delim,4,4
	.type	.L.str.89,@object               # @.str.89
.L.str.89:
	.asciz	"stdin"
	.size	.L.str.89, 6

	.type	.L.str.90,@object               # @.str.90
.L.str.90:
	.asciz	"stdout"
	.size	.L.str.90, 7

	.type	.L.str.91,@object               # @.str.91
.L.str.91:
	.asciz	"stderr"
	.size	.L.str.91, 7

	.type	.L.str.92,@object               # @.str.92
.L.str.92:
	.asciz	"UNDEFINED"
	.size	.L.str.92, 10

	.type	.L.str.93,@object               # @.str.93
.L.str.93:
	.asciz	"NO"
	.size	.L.str.93, 3

	.type	.L.str.94,@object               # @.str.94
.L.str.94:
	.asciz	"NULL"
	.size	.L.str.94, 5

	.type	.L.str.95,@object               # @.str.95
.L.str.95:
	.asciz	"f77: REWIND: unit %d cannot be rewound\n"
	.size	.L.str.95, 40

	.type	.L.str.96,@object               # @.str.96
.L.str.96:
	.asciz	"f77: an I/O statement in a function referenced in another's I/O list\n"
	.size	.L.str.96, 70

	.type	fio_uend_init,@object           # @fio_uend_init
	.local	fio_uend_init
	.comm	fio_uend_init,1,4
	.type	.L.str.97,@object               # @.str.97
.L.str.97:
	.asciz	"f77: unit %d is not open for %s\n"
	.size	.L.str.97, 33

	.type	.L.str.98,@object               # @.str.98
.L.str.98:
	.asciz	"f77: a direct-access unit needs REC=\n"
	.size	.L.str.98, 38

	.type	.L.str.99,@object               # @.str.99
.L.str.99:
	.asciz	"f77: READ past the end of a direct-access file\n"
	.size	.L.str.99, 48

	.type	.L.str.100,@object              # @.str.100
.L.str.100:
	.asciz	"f77: end of file on READ\n"
	.size	.L.str.100, 26

	.type	.L.str.101,@object              # @.str.101
.L.str.101:
	.asciz	"f77: bad unformatted record\n"
	.size	.L.str.101, 29

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
	.type	.L.str.102,@object              # @.str.102
.L.str.102:
	.asciz	"a format begins with ("
	.size	.L.str.102, 23

	.type	fio_fmt_why,@object             # @fio_fmt_why
	.local	fio_fmt_why
	.comm	fio_fmt_why,4,4
	.type	.L.str.103,@object              # @.str.103
.L.str.103:
	.asciz	"the format is too long, or ends without its )"
	.size	.L.str.103, 46

	.type	.L.str.104,@object              # @.str.104
.L.str.104:
	.asciz	"the format ends without its )"
	.size	.L.str.104, 30

	.type	.L.str.105,@object              # @.str.105
.L.str.105:
	.asciz	"a quoted string runs past the end of the format"
	.size	.L.str.105, 48

	.type	.L.str.106,@object              # @.str.106
.L.str.106:
	.asciz	"the format is too long"
	.size	.L.str.106, 23

	.type	.L.str.107,@object              # @.str.107
.L.str.107:
	.asciz	"0H is no Hollerith string"
	.size	.L.str.107, 26

	.type	.L.str.108,@object              # @.str.108
.L.str.108:
	.asciz	"nH wants more characters than the format has"
	.size	.L.str.108, 45

	.type	fio_fmt_check.why,@object       # @fio_fmt_check.why
	.local	fio_fmt_check.why
	.comm	fio_fmt_check.why,64,1
	.type	.L.str.109,@object              # @.str.109
.L.str.109:
	.asciz	"a sign belongs to a scale factor, nP"
	.size	.L.str.109, 37

	.type	.L.str.110,@object              # @.str.110
.L.str.110:
	.asciz	"H needs its count, nH"
	.size	.L.str.110, 22

	.type	.L.str.111,@object              # @.str.111
.L.str.111:
	.asciz	"T, TL and TR take a column"
	.size	.L.str.111, 27

	.type	.L.str.112,@object              # @.str.112
.L.str.112:
	.asciz	"%c needs a width"
	.size	.L.str.112, 17

	.type	.L.str.113,@object              # @.str.113
.L.str.113:
	.asciz	"a . needs digits after it"
	.size	.L.str.113, 26

	.type	.L.str.114,@object              # @.str.114
.L.str.114:
	.asciz	"Ee needs its digits"
	.size	.L.str.114, 20

	.type	.L.str.115,@object              # @.str.115
.L.str.115:
	.asciz	"a count with no descriptor after it"
	.size	.L.str.115, 36

	.type	.L.str.116,@object              # @.str.116
.L.str.116:
	.asciz	"a count before something that is no edit descriptor"
	.size	.L.str.116, 52

	.type	.L.str.117,@object              # @.str.117
.L.str.117:
	.asciz	"%c is no edit descriptor"
	.size	.L.str.117, 25

	.type	.L.str.118,@object              # @.str.118
.L.str.118:
	.asciz	"f77: FORMAT: %s\n  in: %.*s\n"
	.size	.L.str.118, 28

	.type	.L.str.119,@object              # @.str.119
.L.str.119:
	.asciz	"%*d"
	.size	.L.str.119, 4

	.type	fio_line,@object                # @fio_line
	.local	fio_line
	.comm	fio_line,1024,1
	.type	.L.str.120,@object              # @.str.120
.L.str.120:
	.asciz	"-Infinity"
	.size	.L.str.120, 10

	.type	.L.str.121,@object              # @.str.121
.L.str.121:
	.asciz	"%%.%dE"
	.size	.L.str.121, 7

	.type	.L.str.122,@object              # @.str.122
.L.str.122:
	.asciz	"f77: FORMAT: E, D and G with no digits (d = 0) need a positive scale factor\n"
	.size	.L.str.122, 77

	.type	.L.str.123,@object              # @.str.123
.L.str.123:
	.asciz	"%.17E"
	.size	.L.str.123, 6

	.type	.L.str.124,@object              # @.str.124
.L.str.124:
	.asciz	"f77: out of memory for an array constructor\n"
	.size	.L.str.124, 45

	.type	.L.str.125,@object              # @.str.125
.L.str.125:
	.asciz	"f77: a CHARACTER result of %d bytes is too long\n"
	.size	.L.str.125, 49

	.type	fio_gcount,@object              # @fio_gcount
	.local	fio_gcount
	.comm	fio_gcount,64,4
	.type	fio_gstart,@object              # @fio_gstart
	.local	fio_gstart
	.comm	fio_gstart,64,4
	.type	.L.str.126,@object              # @.str.126
.L.str.126:
	.asciz	"f77: FORMAT: no data edit descriptor for the items left\n"
	.size	.L.str.126, 57

	.type	.L.str.127,@object              # @.str.127
.L.str.127:
	.asciz	"f77: WRITE past the end of an internal file\n"
	.size	.L.str.127, 45

	.type	.L.str.128,@object              # @.str.128
.L.str.128:
	.asciz	"f77: record longer than the internal file (%d > %d)\n"
	.size	.L.str.128, 53

	.type	.L.str.129,@object              # @.str.129
.L.str.129:
	.asciz	"f77: record longer than RECL (%d > %d)\n"
	.size	.L.str.129, 40

	.type	.L.str.130,@object              # @.str.130
.L.str.130:
	.asciz	"f77: %s unit %d, opened with ACTION='%s'\n"
	.size	.L.str.130, 42

	.type	.L.str.131,@object              # @.str.131
.L.str.131:
	.asciz	"WRITE to"
	.size	.L.str.131, 9

	.type	.L.str.132,@object              # @.str.132
.L.str.132:
	.asciz	"READ from"
	.size	.L.str.132, 10

	.type	fio_l_nulls,@object             # @fio_l_nulls
	.local	fio_l_nulls
	.comm	fio_l_nulls,4,4
	.type	fio_l_after,@object             # @fio_l_after
	.local	fio_l_after
	.comm	fio_l_after,1,4
	.type	fio_l_pending,@object           # @fio_l_pending
	.local	fio_l_pending
	.comm	fio_l_pending,4,4
	.type	fio_l_rep,@object               # @fio_l_rep
	.local	fio_l_rep
	.comm	fio_l_rep,1,4
	.type	.L.str.133,@object              # @.str.133
.L.str.133:
	.asciz	"f77: zero repeat count in list-directed input\n"
	.size	.L.str.133, 47

	.type	.L.str.134,@object              # @.str.134
.L.str.134:
	.asciz	"f77: READ: '%s' is no number\n"
	.size	.L.str.134, 30

	.type	.L.str.135,@object              # @.str.135
.L.str.135:
	.asciz	"f77: bad LOGICAL input field '%s'\n"
	.size	.L.str.135, 35

	.type	.L.str.136,@object              # @.str.136
.L.str.136:
	.asciz	"f77: bad digit '%c' in a B, O or Z input field\n"
	.size	.L.str.136, 48

	.type	.L.str.137,@object              # @.str.137
.L.str.137:
	.asciz	"E%d"
	.size	.L.str.137, 4

	.type	.L.str.138,@object              # @.str.138
.L.str.138:
	.asciz	"INF"
	.size	.L.str.138, 4

	.type	.L.str.139,@object              # @.str.139
.L.str.139:
	.asciz	"INFINITY"
	.size	.L.str.139, 9

	.type	.L.str.140,@object              # @.str.140
.L.str.140:
	.asciz	"NAN"
	.size	.L.str.140, 4

	.type	fio_l_re,@object                # @fio_l_re
	.local	fio_l_re
	.comm	fio_l_re,8,4
	.type	fio_l_im,@object                # @fio_l_im
	.local	fio_l_im
	.comm	fio_l_im,8,4
	.type	.L.str.141,@object              # @.str.141
.L.str.141:
	.asciz	"(A)"
	.size	.L.str.141, 4

	.type	nml_tlen,@object                # @nml_tlen
	.local	nml_tlen
	.comm	nml_tlen,4,4
	.type	.L.str.142,@object              # @.str.142
.L.str.142:
	.asciz	"the record could not be read"
	.size	.L.str.142, 29

	.type	.L.str.143,@object              # @.str.143
.L.str.143:
	.asciz	" "
	.size	.L.str.143, 2

	.type	nml_tcap,@object                # @nml_tcap
	.local	nml_tcap
	.comm	nml_tcap,4,4
	.type	nml_text,@object                # @nml_text
	.local	nml_text
	.comm	nml_text,4,4
	.type	.L.str.144,@object              # @.str.144
.L.str.144:
	.asciz	"f77: out of storage for a NAMELIST\n"
	.size	.L.str.144, 36

	.type	.L.str.145,@object              # @.str.145
.L.str.145:
	.asciz	"a subscript is no integer"
	.size	.L.str.145, 26

	.type	.L.str.146,@object              # @.str.146
.L.str.146:
	.asciz	"a section with a stride other than 1 is not supported"
	.size	.L.str.146, 54

	.type	.L.str.147,@object              # @.str.147
.L.str.147:
	.asciz	"a value before the first NAME="
	.size	.L.str.147, 31

	.type	.L.str.148,@object              # @.str.148
.L.str.148:
	.asciz	"a repeat count is not positive"
	.size	.L.str.148, 31

	.type	nml_cval,@object                # @nml_cval
	.local	nml_cval
	.comm	nml_cval,4,4
	.type	nml_casg,@object                # @nml_casg
	.local	nml_casg
	.comm	nml_casg,4,4
	.type	nml_pend,@object                # @nml_pend
	.local	nml_pend
	.comm	nml_pend,1024,1
	.type	.L.str.149,@object              # @.str.149
.L.str.149:
	.asciz	"0.0"
	.size	.L.str.149, 4

	.type	.L.str.150,@object              # @.str.150
.L.str.150:
	.asciz	"(G23.16)"
	.size	.L.str.150, 9

	.type	.L.str.151,@object              # @.str.151
.L.str.151:
	.asciz	"(G15.8)"
	.size	.L.str.151, 8

	.type	.L.str.152,@object              # @.str.152
.L.str.152:
	.asciz	"a LOGICAL value is not T or F"
	.size	.L.str.152, 30

	.type	.L.str.153,@object              # @.str.153
.L.str.153:
	.asciz	"a CHARACTER value is not in quotes"
	.size	.L.str.153, 35

	.type	.L.str.154,@object              # @.str.154
.L.str.154:
	.asciz	"a value could not be read"
	.size	.L.str.154, 26

	.type	.Lswitch.table.fio_fmt_use,@object # @switch.table.fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.Lswitch.table.fio_fmt_use:
	.word	.L.str.115
	.word	.L.str.116
	.word	.L.str.116
	.word	.L.str.115
	.word	.L.str.116
	.word	.L.str.116
	.word	.L.str.115
	.word	.L.str.116
	.word	.L.str.116
	.word	.L.str.116
	.word	.L.str.116
	.word	.L.str.116
	.word	.L.str.116
	.word	.L.str.116
	.word	.L.str.116
	.word	.L.str.116
	.word	.L.str.116
	.word	.L.str.115
	.size	.Lswitch.table.fio_fmt_use, 72

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git 77f07727152e39f179dfe39786aa3bcae0860790)"
	.section	".note.GNU-stack","",@progbits
