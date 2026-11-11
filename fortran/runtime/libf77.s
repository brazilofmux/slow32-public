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
	beq r4, r1, .LBB3_23
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
	addi r4, r14, -1
	addi r5, r0, 98
	slli r3, r14, 2
	bgtu r4, r5, .LBB3_9
.LBB3_6:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r4, r3, r4
	stw r4+0, r0
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r4, r3, r4
	stw r4+0, r0
	lui r4, %hi(fio_ufile)
	addi r4, r4, %lo(fio_ufile)
	add r4, r3, r4
	ldw r4, r4+0
	beq r4, r0, .LBB3_9
.LBB3_7:
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r3, r4
	ldw r4, r4+0
	bne r4, r0, .LBB3_9
.LBB3_8:
	lui r4, %hi(fio_uwrote)
	addi r4, r4, %lo(fio_uwrote)
	add r4, r3, r4
	stw r4+0, r13
.LBB3_9:
	lui r5, %hi(fio_rec_next)
	ldw r4, r5+%lo(fio_rec_next)
	stw r5+%lo(fio_rec_next), r0
	addi r5, r14, -100
	addi r6, r0, -99
	bltu r5, r6, .LBB3_13
.LBB3_10:
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r3, r5
	ldw r5, r5+0
	beq r5, r0, .LBB3_13
.LBB3_11:
	ble r4, r0, .LBB3_25
.LBB3_12:
	addi r4, r4, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB3_13:
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
	beq r12, r0, .LBB3_20
.LBB3_14:
	stw r1+%lo(fio_listed), r0
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fio_fmt_use
	addi r3, r0, -1
	ble r1, r3, .LBB3_24
.LBB3_15:
	stw r14+%lo(fio_pos), r0
	stw r15+%lo(fio_revert), r3
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	lui r1, %hi(fio_fmt)
	ldw r3, r1+%lo(fio_fmt)
	addi r4, r0, 32
	add r1, r0, r0
.LBB3_16:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB3_18
.LBB3_17:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB3_16
.LBB3_18:
	addi r3, r0, 40
	bne r5, r3, .LBB3_22
.LBB3_19:
	addi r3, r1, 1
	add r1, r3, r0
	jal r0, .LBB3_21
.LBB3_20:
	stw r1+%lo(fio_listed), r13
	lui r1, %hi(.L.str.8)
	addi r1, r1, %lo(.L.str.8)
	lui r3, %hi(fio_fmt)
	stw r3+%lo(fio_fmt), r1
	addi r1, r0, -1
	add r3, r0, r0
.LBB3_21:
	stw r14+%lo(fio_pos), r3
	stw r15+%lo(fio_revert), r1
.LBB3_22:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB3_23:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	jal r0, .LBB3_26
.LBB3_24:
	addi r3, r0, 2
	jal r31, exit
.LBB3_25:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
.LBB3_26:
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
	lui r5, %hi(.L.str.51)
	addi r5, r5, %lo(.L.str.51)
	addi r12, sp, 20
	addi r4, r0, 256
	add r3, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	add r3, r12, r0
	jal r31, fopen
	bne r1, r0, .LBB4_13
.LBB4_12:
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
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
	lui r4, %hi(.L.str.67)
	addi r4, r4, %lo(.L.str.67)
	lui r5, %hi(.L.str.66)
	addi r5, r5, %lo(.L.str.66)
	xor r5, r5, r4
	sub r1, r0, r1
	and r1, r5, r1
	xor r6, r4, r1
	lui r4, %hi(.L.str.93)
	addi r4, r4, %lo(.L.str.93)
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
	beq r4, r1, .LBB5_14
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
	addi r4, r11, -1
	addi r5, r0, 98
	slli r3, r11, 2
	bgtu r4, r5, .LBB5_9
.LBB5_6:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r4, r3, r4
	stw r4+0, r12
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r4, r3, r4
	stw r4+0, r0
	lui r4, %hi(fio_ufile)
	addi r4, r4, %lo(fio_ufile)
	add r4, r3, r4
	ldw r4, r4+0
	beq r4, r0, .LBB5_9
.LBB5_7:
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r3, r4
	ldw r4, r4+0
	bne r4, r0, .LBB5_9
.LBB5_8:
	lui r4, %hi(fio_uwrote)
	addi r4, r4, %lo(fio_uwrote)
	add r4, r3, r4
	stw r4+0, r12
.LBB5_9:
	lui r5, %hi(fio_rec_next)
	ldw r4, r5+%lo(fio_rec_next)
	stw r5+%lo(fio_rec_next), r0
	addi r5, r11, -100
	addi r6, r0, -99
	bltu r5, r6, .LBB5_13
.LBB5_10:
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r3, r5
	ldw r5, r5+0
	beq r5, r0, .LBB5_13
.LBB5_11:
	ble r4, r0, .LBB5_15
.LBB5_12:
	addi r4, r4, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB5_13:
	lui r1, %hi(fio_ulen)
	stw r1+%lo(fio_ulen), r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB5_14:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	jal r0, .LBB5_16
.LBB5_15:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
.LBB5_16:
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
	beq r1, r12, .LBB8_29
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
	addi r17, r14, -1
	addi r18, r0, 98
	slli r16, r14, 2
	bgtu r17, r18, .LBB8_7
.LBB8_6:
	lui r1, %hi(fio_uwrote)
	addi r1, r1, %lo(fio_uwrote)
	add r1, r16, r1
	stw r1+0, r0
	lui r1, %hi(fio_in)
	stw r1+%lo(fio_in), r13
	lui r1, %hi(fio_unit)
	stw r1+%lo(fio_unit), r14
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r16, r1
	addi r3, r0, 1
	stw r1+0, r3
	jal r0, .LBB8_8
.LBB8_7:
	lui r1, %hi(fio_in)
	stw r1+%lo(fio_in), r13
	lui r1, %hi(fio_unit)
	stw r1+%lo(fio_unit), r14
.LBB8_8:
	lui r3, %hi(fio_rec_next)
	ldw r1, r3+%lo(fio_rec_next)
	stw r3+%lo(fio_rec_next), r0
	addi r3, r14, -100
	addi r4, r0, -99
	bltu r3, r4, .LBB8_12
.LBB8_9:
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r16, r3
	ldw r3, r3+0
	beq r3, r0, .LBB8_12
.LBB8_10:
	ble r1, r0, .LBB8_31
.LBB8_11:
	addi r1, r1, -1
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r16, r3
	ldw r3, r3+0
	mul r4, r3, r1
	add r3, r13, r0
	add r5, r0, r0
	jal r31, fseek
.LBB8_12:
	lui r1, %hi(fio_upos)
	stw r1+%lo(fio_upos), r0
	bgtu r17, r18, .LBB8_17
.LBB8_13:
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r16, r1
	ldw r1, r1+0
	beq r1, r0, .LBB8_17
.LBB8_14:
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
	bne r1, r0, .LBB8_24
.LBB8_15:
	andi r1, r11, 2
	bne r1, r0, .LBB8_28
.LBB8_16:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.95)
	addi r4, r4, %lo(.L.str.95)
	jal r0, .LBB8_33
.LBB8_17:
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r14, r16, r1
	ldw r1, r14+0
	blt r1, r0, .LBB8_19
.LBB8_18:
	add r3, r13, r0
	jal r31, ftell
	ldw r3, r14+0
	bge r1, r3, .LBB8_23
.LBB8_19:
	addi r3, sp, 28
	addi r12, r0, 1
	addi r14, r0, 4
	add r4, r12, r0
	add r5, r14, r0
	add r6, r13, r0
	jal r31, fread
	bne r1, r14, .LBB8_23
.LBB8_20:
	addi r3, sp, 28
	addi r1, r3, 3
	ldbu r1, r1+0
	slli r1, r1, 24
	blt r1, r0, .LBB8_27
.LBB8_21:
	ldhu r4, sp+28
	addi r3, r3, 2
	ldbu r3, r3+0
	slli r3, r3, 16
	or  r3, r4, r3
	or  r5, r3, r1
	lui r1, 16
	addi r1, r1, 1
	bge r5, r1, .LBB8_27
.LBB8_22:
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
	bne r12, r0, .LBB8_25
	jal r0, .LBB8_26
.LBB8_23:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r16, r1
	stw r1+0, r12
	andi r1, r11, 3
	beq r1, r0, .LBB8_30
.LBB8_24:
	beq r12, r0, .LBB8_26
.LBB8_25:
	stb r15+%lo(fio_busy), r0
.LBB8_26:
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
.LBB8_27:
	andi r1, r11, 2
	beq r1, r0, .LBB8_32
.LBB8_28:
	addi r12, r0, 2
	bne r12, r0, .LBB8_25
	jal r0, .LBB8_26
.LBB8_29:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	jal r0, .LBB8_33
.LBB8_30:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r0, .LBB8_33
.LBB8_31:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	jal r0, .LBB8_33
.LBB8_32:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.97)
	addi r4, r4, %lo(.L.str.97)
.LBB8_33:
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
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
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
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+45, r11
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+41, r12
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+37, r13
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+33, r14
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+29, r15
	add r11, r3, r0
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+25, r3
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB13_12
.LBB13_1:
	lui r5, %hi(.L.str.115)
	addi r5, r5, %lo(.L.str.115)
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
	add r1, sp, r0
	addi r1, r1, 2047
	addi r3, r1, 25
	addi r4, r0, 4
	jal r31, fio_hol_out
.LBB13_56:
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r15, r1+29
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r14, r1+33
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r13, r1+37
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r12, r1+41
	add r1, sp, r0
	addi r1, r1, 2047
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
	lui r5, %hi(.L.str.116)
	addi r5, r5, %lo(.L.str.116)
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
	lui r5, %hi(.L.str.117)
	addi r5, r5, %lo(.L.str.117)
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
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+93, r11
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+89, r12
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+85, r13
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+81, r14
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+77, r15
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+73, r16
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+69, r17
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+65, r18
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+61, r19
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+57, r20
	add r1, sp, r0
	addi r1, r1, 2047
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
	lui r5, %hi(.L.str.117)
	addi r5, r5, %lo(.L.str.117)
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
	add r13, sp, r0
	addi r13, r13, 2047
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
	add r6, sp, r0
	addi r6, r6, 2047
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
	add r5, sp, r0
	addi r5, r5, 2047
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
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r21, r1+53
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r20, r1+57
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r19, r1+61
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r18, r1+65
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r17, r1+69
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r16, r1+73
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r15, r1+77
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r14, r1+81
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r13, r1+85
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r12, r1+89
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r11, r1+93
	ldw lr, sp+0
	addi sp, sp, 2047
	addi sp, sp, 97
	jalr r0, r31, 0
.LBB23_51:
	jal r31, fio_flush_line
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.118)
	addi r4, r4, %lo(.L.str.118)
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
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+125, r11
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+121, r12
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+117, r13
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+113, r14
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+109, r15
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+105, r16
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+101, r17
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+97, r18
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+93, r19
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+89, r20
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+85, r21
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+81, r22
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+77, r23
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+73, r24
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+69, r26
	add r1, sp, r0
	addi r1, r1, 2047
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
	lui r5, %hi(.L.str.119)
	addi r5, r5, %lo(.L.str.119)
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
	lui r5, %hi(.L.str.117)
	addi r5, r5, %lo(.L.str.117)
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
	add r14, sp, r0
	addi r14, r14, 2047
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
	add r7, sp, r0
	addi r7, r7, 2047
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
	add r6, sp, r0
	addi r6, r6, 2047
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
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r27, r1+65
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r26, r1+69
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r24, r1+73
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r23, r1+77
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r22, r1+81
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r21, r1+85
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r20, r1+89
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r19, r1+93
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r18, r1+97
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r17, r1+101
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r16, r1+105
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r15, r1+109
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r14, r1+113
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r13, r1+117
	add r1, sp, r0
	addi r1, r1, 2047
	ldw r12, r1+121
	add r1, sp, r0
	addi r1, r1, 2047
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
	.globl	f77_dealloc_auto                # -- Begin function f77_dealloc_auto
	.p2align	2
	.type	f77_dealloc_auto,@function
f77_dealloc_auto:                       # @f77_dealloc_auto
# %bb.0:
	beq r3, r0, .LBB45_2
.LBB45_1:
	jal r0, free
.LBB45_2:
	jalr r0, r31, 0
.Lfunc_end45:
	.size	f77_dealloc_auto, .Lfunc_end45-f77_dealloc_auto
                                        # -- End function
	.globl	f77_dfl_fill                    # -- Begin function f77_dfl_fill
	.p2align	2
	.type	f77_dfl_fill,@function
f77_dfl_fill:                           # @f77_dfl_fill
# %bb.0:
	beq r3, r0, .LBB46_8
.LBB46_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r11, r6, r0
	addi r1, r0, 1
	blt r6, r1, .LBB46_7
.LBB46_2:
	add r12, r5, r0
	add r13, r4, r0
	add r14, r3, r0
	jal r0, .LBB46_5
.LBB46_3:
	add r4, r0, r0
	add r5, r12, r0
	jal r31, memset
.LBB46_4:
	addi r11, r11, -1
	add r14, r14, r12
	beq r11, r0, .LBB46_7
.LBB46_5:
	add r3, r14, r0
	beq r13, r0, .LBB46_3
.LBB46_6:
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcpy
	jal r0, .LBB46_4
.LBB46_7:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB46_8:
	jalr r0, r31, 0
.Lfunc_end46:
	.size	f77_dfl_fill, .Lfunc_end46-f77_dfl_fill
                                        # -- End function
	.globl	f77_dta_free                    # -- Begin function f77_dta_free
	.p2align	2
	.type	f77_dta_free,@function
f77_dta_free:                           # @f77_dta_free
# %bb.0:
	addi r1, r0, 1
	blt r4, r1, .LBB47_6
.LBB47_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	jal r0, .LBB47_3
.LBB47_2:
	addi r12, r12, -1
	add r13, r13, r11
	beq r12, r0, .LBB47_5
.LBB47_3:
	ldw r3, r13+0
	beq r3, r0, .LBB47_2
.LBB47_4:
	jal r31, free
	stw r13+0, r0
	jal r0, .LBB47_2
.LBB47_5:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB47_6:
	jalr r0, r31, 0
.Lfunc_end47:
	.size	f77_dta_free, .Lfunc_end47-f77_dta_free
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
	beq r1, r0, .LBB48_20
.LBB48_1:
	add r13, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	addi r1, r0, 1
	add r15, r1, r0
	blt r4, r1, .LBB48_4
.LBB48_2:
	addi r3, r12, 4
	addi r15, r0, 1
	addi r4, r0, 0
	add r5, r11, r0
.LBB48_3:
	ldw r6, r3+0
	sgt r7, r6, r4
	mul r6, r6, r15
	sub r7, r0, r7
	and r15, r6, r7
	addi r5, r5, -1
	addi r3, r3, 8
	bne r5, r0, .LBB48_3
.LBB48_4:
	mul r3, r15, r13
	xori r4, r3, 1
	sgt r1, r3, r1
	sub r1, r0, r1
	and r1, r4, r1
	xori r3, r1, 1
	jal r31, malloc
	beq r1, r0, .LBB48_21
.LBB48_5:
	add r14, r1, r0
	addi r3, sp, 28
	addi r5, r0, 28
	add r4, r0, r0
	jal r31, memset
	addi r16, r0, 1
	blt r15, r16, .LBB48_17
.LBB48_6:
	addi r17, r12, 8
	addi r18, r12, 4
	addi r19, r0, 1
	add r20, r0, r0
	jal r0, .LBB48_8
.LBB48_7:
	addi r20, r20, 1
	beq r20, r15, .LBB48_17
.LBB48_8:
	blt r11, r19, .LBB48_12
.LBB48_9:
	addi r1, sp, 28
	add r3, r17, r0
	add r5, r11, r0
	add r4, r0, r0
.LBB48_10:
	ldw r6, r1+0
	ldw r7, r3+0
	mul r6, r7, r6
	add r4, r6, r4
	addi r5, r5, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r5, r0, .LBB48_10
.LBB48_11:
	mul r1, r4, r13
	jal r0, .LBB48_13
.LBB48_12:
	add r1, r0, r0
.LBB48_13:
	mul r3, r20, r13
	add r3, r14, r3
	ldw r4, r12+0
	add r4, r4, r1
	add r5, r13, r0
	jal r31, memcpy
	blt r11, r19, .LBB48_7
.LBB48_14:
	addi r1, sp, 28
	add r3, r18, r0
	add r4, r11, r0
.LBB48_15:
	ldw r5, r1+0
	addi r5, r5, 1
	stw r1+0, r5
	ldw r6, r3+0
	blt r5, r6, .LBB48_7
.LBB48_16:
	stw r1+0, r0
	addi r4, r4, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r4, r0, .LBB48_15
	jal r0, .LBB48_7
.LBB48_17:
	stw r12+0, r14
	blt r11, r16, .LBB48_20
.LBB48_18:
	addi r1, r12, 4
.LBB48_19:
	stw r1+4, r16
	ldw r3, r1+0
	mul r16, r3, r16
	addi r11, r11, -1
	addi r1, r1, 8
	bne r11, r0, .LBB48_19
.LBB48_20:
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
.LBB48_21:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.21)
	addi r4, r4, %lo(.L.str.21)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end48:
	.size	f77_dta_dup, .Lfunc_end48-f77_dta_dup
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
	beq r3, r0, .LBB49_11
.LBB49_1:
	addi r1, r0, 1
	blt r4, r1, .LBB49_10
.LBB49_2:
	addi r6, r3, 4
	addi r7, r0, 2
	add r8, r4, r0
	jal r0, .LBB49_4
.LBB49_3:
	mul r1, r9, r1
	addi r8, r8, -1
	addi r6, r6, 8
	beq r8, r0, .LBB49_10
.LBB49_4:
	ldw r9, r6+0
	blt r9, r7, .LBB49_3
.LBB49_5:
	ldw r10, r6+4
	beq r10, r1, .LBB49_3
.LBB49_6:
	add r11, r3, r0
	addi r3, r3, 4
	addi r1, r0, 1
	add r12, r4, r0
.LBB49_7:
	ldw r6, r3+0
	mul r1, r6, r1
	addi r4, r4, -1
	addi r3, r3, 8
	bne r4, r0, .LBB49_7
.LBB49_8:
	add r14, r5, r0
	mul r13, r1, r5
	xori r1, r13, 1
	addi r3, r0, 1
	sgt r3, r13, r3
	sub r3, r0, r3
	and r1, r1, r3
	xori r3, r1, 1
	jal r31, malloc
	beq r1, r0, .LBB49_13
.LBB49_9:
	addi r7, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r14, r0
	add r6, r1, r0
	add r11, r1, r0
	jal r31, f77_as_walk
	add r1, r11, r0
	jal r0, .LBB49_12
.LBB49_10:
	ldw r1, r3+0
	jal r0, .LBB49_12
.LBB49_11:
	add r1, r0, r0
.LBB49_12:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB49_13:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	add r5, r13, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end49:
	.size	f77_as_pack, .Lfunc_end49-f77_as_pack
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
	blt r14, r1, .LBB50_3
.LBB50_1:
	addi r5, r3, 4
	addi r15, r0, 1
	add r6, r14, r0
.LBB50_2:
	stw r4+0, r0
	ldw r7, r5+0
	mul r15, r7, r15
	addi r6, r6, -1
	addi r5, r5, 8
	addi r4, r4, 4
	bne r6, r0, .LBB50_2
.LBB50_3:
	blt r15, r1, .LBB50_18
.LBB50_4:
	ldw r16, r3+0
	addi r17, r3, 4
	addi r18, r3, 8
	addi r19, r0, 1
	add r20, r0, r0
	jal r0, .LBB50_6
.LBB50_5:
	addi r20, r20, 1
	beq r20, r15, .LBB50_18
.LBB50_6:
	addi r1, sp, 24
	blt r14, r19, .LBB50_10
.LBB50_7:
	add r3, r18, r0
	add r5, r14, r0
	add r4, r0, r0
.LBB50_8:
	ldw r6, r1+0
	ldw r7, r3+0
	mul r6, r7, r6
	add r4, r6, r4
	addi r5, r5, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r5, r0, .LBB50_8
.LBB50_9:
	mul r1, r4, r13
	jal r0, .LBB50_11
.LBB50_10:
	add r1, r0, r0
.LBB50_11:
	add r4, r16, r1
	mul r1, r20, r13
	beq r11, r0, .LBB50_13
.LBB50_12:
	add r3, r12, r1
	jal r0, .LBB50_14
.LBB50_13:
	add r1, r12, r1
	add r3, r4, r0
	add r4, r1, r0
.LBB50_14:
	add r5, r13, r0
	jal r31, memcpy
	blt r14, r19, .LBB50_5
.LBB50_15:
	addi r1, sp, 24
	add r3, r17, r0
	add r4, r14, r0
.LBB50_16:
	ldw r5, r1+0
	addi r5, r5, 1
	stw r1+0, r5
	ldw r6, r3+0
	blt r5, r6, .LBB50_5
.LBB50_17:
	stw r1+0, r0
	addi r4, r4, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r4, r0, .LBB50_16
	jal r0, .LBB50_5
.LBB50_18:
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
.Lfunc_end50:
	.size	f77_as_walk, .Lfunc_end50-f77_as_walk
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
	beq r1, r0, .LBB51_3
.LBB51_1:
	add r12, r1, r0
	addi r3, r0, 256
	jal r31, malloc
	stw r12+0, r1
	beq r1, r0, .LBB51_3
.LBB51_2:
	stw r12+4, r0
	stw r12+8, r11
	stw r12+12, r11
	add r1, r12, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB51_3:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.120)
	addi r4, r4, %lo(.L.str.120)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end51:
	.size	f77_vec_new, .Lfunc_end51-f77_vec_new
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
	bne r1, r0, .LBB52_4
.LBB52_1:
	ldw r1, r11+12
	beq r5, r1, .LBB52_4
.LBB52_2:
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
	beq r1, r0, .LBB52_8
.LBB52_3:
	stw r11+0, r1
	stw r11+12, r13
	add r4, r12, r0
.LBB52_4:
	ldw r3, r11+4
	ldw r1, r11+8
	bne r3, r1, .LBB52_7
.LBB52_5:
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
	beq r1, r0, .LBB52_8
.LBB52_6:
	stw r11+0, r1
	ldw r1, r11+8
	slli r1, r1, 1
	stw r11+8, r1
	add r4, r12, r0
.LBB52_7:
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
.LBB52_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.120)
	addi r4, r4, %lo(.L.str.120)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end52:
	.size	f77_vec_pushn, .Lfunc_end52-f77_vec_pushn
                                        # -- End function
	.globl	f77_vec_push                    # -- Begin function f77_vec_push
	.p2align	2
	.type	f77_vec_push,@function
f77_vec_push:                           # @f77_vec_push
# %bb.0:
	addi r5, r0, 16
	jal r0, f77_vec_pushn
.Lfunc_end53:
	.size	f77_vec_push, .Lfunc_end53-f77_vec_push
                                        # -- End function
	.globl	f77_vec_count                   # -- Begin function f77_vec_count
	.p2align	2
	.type	f77_vec_count,@function
f77_vec_count:                          # @f77_vec_count
# %bb.0:
	ldw r1, r3+4
	jalr r0, r31, 0
.Lfunc_end54:
	.size	f77_vec_count, .Lfunc_end54-f77_vec_count
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
.Lfunc_end55:
	.size	f77_vec_take, .Lfunc_end55-f77_vec_take
                                        # -- End function
	.globl	f77_as_unpack                   # -- Begin function f77_as_unpack
	.p2align	2
	.type	f77_as_unpack,@function
f77_as_unpack:                          # @f77_as_unpack
# %bb.0:
	beq r3, r0, .LBB56_3
.LBB56_1:
	ldw r1, r3+0
	beq r6, r1, .LBB56_3
.LBB56_2:
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
.LBB56_3:
	jalr r0, r31, 0
.Lfunc_end56:
	.size	f77_as_unpack, .Lfunc_end56-f77_as_unpack
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpu_time_r
	.type	.LCPI57_0,@object
.LCPI57_0:
	.quad	0x412e848000000000              # double 1.0E+6
	.size	.LCPI57_0, 8
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
	lui r1, %hi(.LCPI57_0)
	addi r1, r1, %lo(.LCPI57_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
	stw r11+0, r1
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end57:
	.size	f77_cpu_time_r, .Lfunc_end57-f77_cpu_time_r
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpu_time_d
	.type	.LCPI58_0,@object
.LCPI58_0:
	.quad	0x412e848000000000              # double 1.0E+6
	.size	.LCPI58_0, 8
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
	lui r1, %hi(.LCPI58_0)
	addi r1, r1, %lo(.LCPI58_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fdiv.d r4, r4, r6
	stw r11+4, r5
	stw r11+0, r4
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end58:
	.size	f77_cpu_time_d, .Lfunc_end58-f77_cpu_time_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_fraction
	.type	.LCPI59_0,@object
.LCPI59_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI59_0, 8
	.text
	.globl	f77_fraction
	.p2align	2
	.type	f77_fraction,@function
f77_fraction:                           # @f77_fraction
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI59_0)
	addi r1, r1, %lo(.LCPI59_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r6, r4
	bne r1, r0, .LBB59_2
.LBB59_1:
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
.LBB59_2:
	add r1, r6, r0
	add r2, r7, r0
	jalr r0, r31, 0
.Lfunc_end59:
	.size	f77_fraction, .Lfunc_end59-f77_fraction
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_set_exponent
	.type	.LCPI60_0,@object
.LCPI60_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI60_0, 8
	.text
	.globl	f77_set_exponent
	.p2align	2
	.type	f77_set_exponent,@function
f77_set_exponent:                       # @f77_set_exponent
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI60_0)
	addi r1, r1, %lo(.LCPI60_0)
	ldw r9, r1+4
	ldw r8, r1+0
	feq.d r1, r6, r8
	bne r1, r0, .LBB60_2
.LBB60_1:
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
.LBB60_2:
	add r1, r6, r0
	add r2, r7, r0
	jalr r0, r31, 0
.Lfunc_end60:
	.size	f77_set_exponent, .Lfunc_end60-f77_set_exponent
                                        # -- End function
	.globl	f77_scale                       # -- Begin function f77_scale
	.p2align	2
	.type	f77_scale,@function
f77_scale:                              # @f77_scale
# %bb.0:
	add r9, r4, r0
	add r8, r3, r0
	beq r6, r0, .LBB61_2
.LBB61_1:
	add r3, r8, r0
	add r4, r9, r0
	jal r0, ldexp
.LBB61_2:
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
.Lfunc_end61:
	.size	f77_scale, .Lfunc_end61-f77_scale
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_nearest
	.type	.LCPI62_0,@object
.LCPI62_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI62_0, 8
	.type	.LCPI62_2,@object
.LCPI62_2:
	.word	0xff800000                      # float -Inf
	.word	0x7f800000                      # float +Inf
	.size	.LCPI62_2, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI62_1,@object
.LCPI62_1:
	.quad	0xfff0000000000000              # double -Inf
	.quad	0x7ff0000000000000              # double +Inf
	.size	.LCPI62_1, 16
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
	lui r1, %hi(.LCPI62_0)
	addi r1, r1, %lo(.LCPI62_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r10, r4
	xori r1, r1, 1
	beq r1, r0, .LBB62_4
.LBB62_1:
	add r8, r3, r0
	beq r7, r0, .LBB62_3
.LBB62_2:
	flt.d r1, r4, r10
	sub r1, r0, r1
	andi r1, r1, 8
	lui r3, %hi(.LCPI62_1)
	addi r3, r3, %lo(.LCPI62_1)
	add r1, r3, r1
	ldw r6, r1+4
	ldw r5, r1+0
	add r3, r8, r0
	add r4, r9, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, nextafter
.LBB62_3:
	fcvt.s.d r3, r8
	flt.d r1, r4, r10
	sub r1, r0, r1
	andi r1, r1, 4
	lui r4, %hi(.LCPI62_2)
	addi r4, r4, %lo(.LCPI62_2)
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
.LBB62_4:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end62:
	.size	f77_nearest, .Lfunc_end62-f77_nearest
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_spacing
	.type	.LCPI63_0,@object
.LCPI63_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI63_0, 8
	.type	.LCPI63_1,@object
.LCPI63_1:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI63_1, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI63_2,@object
.LCPI63_2:
	.quad	0x0010000000000000              # double 2.2250738585072014E-308
	.quad	0x3810000000000000              # double 1.1754943508222875E-38
	.size	.LCPI63_2, 16
	.text
	.globl	f77_spacing
	.p2align	2
	.type	f77_spacing,@function
f77_spacing:                            # @f77_spacing
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI63_0)
	addi r1, r1, %lo(.LCPI63_0)
	ldw r9, r1+4
	ldw r8, r1+0
	feq.d r1, r6, r8
	xori r1, r1, 1
	bne r1, r0, .LBB63_2
.LBB63_1:
	addi r1, r0, 0
	seq r1, r5, r1
	sub r1, r0, r1
	andi r1, r1, 8
	lui r3, %hi(.LCPI63_2)
	addi r3, r3, %lo(.LCPI63_2)
	add r1, r3, r1
	ldw r5, r1+4
	ldw r4, r1+0
	jal r0, .LBB63_6
.LBB63_2:
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
	bge r5, r1, .LBB63_4
.LBB63_3:
	andi r1, r11, 8
	lui r3, %hi(.LCPI63_2)
	addi r3, r3, %lo(.LCPI63_2)
	add r1, r3, r1
	ldw r5, r1+4
	ldw r4, r1+0
	jal r0, .LBB63_5
.LBB63_4:
	lui r1, %hi(.LCPI63_1)
	addi r1, r1, %lo(.LCPI63_1)
	ldw r4, r1+4
	ldw r3, r1+0
	jal r31, ldexp
	add r4, r1, r0
	add r5, r2, r0
.LBB63_5:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB63_6:
	add r1, r4, r0
	add r2, r5, r0
	jalr r0, r31, 0
.Lfunc_end63:
	.size	f77_spacing, .Lfunc_end63-f77_spacing
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_rrspacing
	.type	.LCPI64_0,@object
.LCPI64_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI64_0, 8
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
	lui r1, %hi(.LCPI64_0)
	addi r1, r1, %lo(.LCPI64_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r6, r4
	bne r1, r0, .LBB64_2
.LBB64_1:
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
.LBB64_2:
	add r1, r4, r0
	add r2, r5, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end64:
	.size	f77_rrspacing, .Lfunc_end64-f77_rrspacing
                                        # -- End function
	.globl	f77_modulo_i                    # -- Begin function f77_modulo_i
	.p2align	2
	.type	f77_modulo_i,@function
f77_modulo_i:                           # @f77_modulo_i
# %bb.0:
	beq r4, r0, .LBB65_2
.LBB65_1:
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
.LBB65_2:
	addi sp, sp, -16
	stw sp+0, lr
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.24)
	addi r4, r4, %lo(.L.str.24)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end65:
	.size	f77_modulo_i, .Lfunc_end65-f77_modulo_i
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_modulo_d
	.type	.LCPI66_0,@object
.LCPI66_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI66_0, 8
	.type	.LCPI66_2,@object
.LCPI66_2:
	.quad	0xb690000000000000              # double -7.0064923216240854E-46
	.size	.LCPI66_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI66_1,@object
.LCPI66_1:
	.word	0x00000000                      # float 0
	.size	.LCPI66_1, 4
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
	beq r7, r0, .LBB66_3
.LBB66_1:
	add r3, r4, r0
	add r4, r5, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fmod
	add r4, r1, r0
	add r5, r2, r0
	lui r1, %hi(.LCPI66_0)
	addi r1, r1, %lo(.LCPI66_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r4, r6
	xori r1, r1, 1
	bne r1, r0, .LBB66_6
.LBB66_2:
	add r3, r6, r0
	add r4, r7, r0
	jal r0, .LBB66_5
.LBB66_3:
	fcvt.s.d r3, r4
	fcvt.s.d r11, r12
	add r4, r11, r0
	jal r31, fmodf
	lui r3, %hi(.LCPI66_1)
	ldw r3, r3+%lo(.LCPI66_1)
	feq.s r4, r1, r3
	xori r4, r4, 1
	bne r4, r0, .LBB66_7
.LBB66_4:
	lui r1, %hi(.LCPI66_0)
	addi r1, r1, %lo(.LCPI66_0)
	ldw r4, r1+4
	ldw r3, r1+0
.LBB66_5:
	add r5, r12, r0
	add r6, r13, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, copysign
.LBB66_6:
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
	jal r0, .LBB66_8
.LBB66_7:
	flt.s r3, r1, r3
	lui r4, %hi(.LCPI66_2)
	addi r4, r4, %lo(.LCPI66_2)
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
.LBB66_8:
	add r1, r6, r0
	add r2, r7, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end66:
	.size	f77_modulo_d, .Lfunc_end66-f77_modulo_d
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
	beq r18, r0, .LBB67_6
.LBB67_1:
	ldw r1, r14+20
	addi r6, r1, 1900
	ldw r1, r14+16
	addi r7, r1, 1
	ldw r8, r14+12
	lui r5, %hi(.L.str.25)
	addi r5, r5, %lo(.L.str.25)
	addi r3, sp, 28
	addi r4, r0, 16
	jal r31, snprintf
	addi r1, r0, 1
	blt r17, r1, .LBB67_6
.LBB67_2:
	addi r1, r0, 32
	addi r3, r0, 7
	add r4, r0, r0
	jal r0, .LBB67_4
.LBB67_3:
	add r6, r18, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r17, r4, .LBB67_6
.LBB67_4:
	add r5, r1, r0
	bgtu r4, r3, .LBB67_3
.LBB67_5:
	addi r5, sp, 28
	add r5, r5, r4
	ldbu r5, r5+0
	jal r0, .LBB67_3
.LBB67_6:
	lui r17, 67109
	beq r16, r0, .LBB67_12
.LBB67_7:
	ldw r6, r14+8
	ldw r7, r14+4
	ldw r8, r14+0
	ldw r1, sp+60
	addi r3, r17, -557
	mulh r1, r1, r3
	srli r3, r1, 31
	srai r1, r1, 6
	add r9, r1, r3
	lui r5, %hi(.L.str.26)
	addi r5, r5, %lo(.L.str.26)
	addi r3, sp, 28
	addi r4, r0, 16
	jal r31, snprintf
	addi r1, r0, 1
	blt r15, r1, .LBB67_12
.LBB67_8:
	addi r1, r0, 32
	addi r3, r0, 9
	add r4, r0, r0
	jal r0, .LBB67_10
.LBB67_9:
	add r6, r16, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r15, r4, .LBB67_12
.LBB67_10:
	add r5, r1, r0
	bgtu r4, r3, .LBB67_9
.LBB67_11:
	addi r5, sp, 28
	add r5, r5, r4
	ldbu r5, r5+0
	jal r0, .LBB67_9
.LBB67_12:
	beq r13, r0, .LBB67_18
.LBB67_13:
	addi r1, r0, 1
	blt r12, r1, .LBB67_18
.LBB67_14:
	addi r1, r0, 32
	addi r3, r0, 4
	lui r4, %hi(.L.str.27)
	addi r4, r4, %lo(.L.str.27)
	add r5, r0, r0
	jal r0, .LBB67_16
.LBB67_15:
	add r7, r13, r5
	stb r7+0, r6
	addi r5, r5, 1
	beq r12, r5, .LBB67_18
.LBB67_16:
	add r6, r1, r0
	bgtu r5, r3, .LBB67_15
.LBB67_17:
	add r6, r5, r4
	ldbu r6, r6+0
	jal r0, .LBB67_15
.LBB67_18:
	beq r11, r0, .LBB67_20
.LBB67_19:
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
.LBB67_20:
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
.Lfunc_end67:
	.size	f77_date_and_time, .Lfunc_end67-f77_date_and_time
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
	beq r13, r0, .LBB68_2
.LBB68_1:
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
.LBB68_2:
	beq r12, r0, .LBB68_4
.LBB68_3:
	addi r1, r0, 1000
	stw r12+0, r1
.LBB68_4:
	beq r11, r0, .LBB68_6
.LBB68_5:
	addi r1, r14, -1
	stw r11+0, r1
.LBB68_6:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end68:
	.size	f77_system_clock, .Lfunc_end68-f77_system_clock
                                        # -- End function
	.globl	f77_random_seed                 # -- Begin function f77_random_seed
	.p2align	2
	.type	f77_random_seed,@function
f77_random_seed:                        # @f77_random_seed
# %bb.0:
	beq r3, r0, .LBB69_2
.LBB69_1:
	addi r1, r0, 4
	stw r3+0, r1
.LBB69_2:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	lui r11, %hi(f77_rng)
	addi r11, r11, %lo(f77_rng)
	addi r1, r0, 16
	beq r5, r0, .LBB69_5
.LBB69_3:
	add r6, r0, r0
.LBB69_4:
	add r7, r6, r11
	ldw r7, r7+0
	add r8, r5, r6
	stw r8+0, r7
	addi r6, r6, 4
	bne r6, r1, .LBB69_4
.LBB69_5:
	lui r12, %hi(f77_rng+4)
	lui r13, %hi(f77_rng+8)
	lui r14, %hi(f77_rng+12)
	beq r4, r0, .LBB69_10
.LBB69_6:
	add r6, r0, r0
.LBB69_7:
	add r7, r4, r6
	ldw r7, r7+0
	add r8, r6, r11
	stw r8+0, r7
	addi r6, r6, 4
	bne r6, r1, .LBB69_7
.LBB69_8:
	ldw r1, r11+0
	ldw r6, r12+%lo(f77_rng+4)
	or  r1, r6, r1
	ldw r6, r13+%lo(f77_rng+8)
	or  r1, r1, r6
	ldw r6, r14+%lo(f77_rng+12)
	or  r1, r1, r6
	bne r1, r0, .LBB69_10
.LBB69_9:
	addi r1, r0, 1
	stw r11+0, r1
.LBB69_10:
	bne r3, r0, .LBB69_15
.LBB69_11:
	bne r4, r0, .LBB69_15
.LBB69_12:
	bne r5, r0, .LBB69_15
.LBB69_13:
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
	bne r1, r0, .LBB69_15
.LBB69_14:
	addi r1, r0, 1
	stw r11+0, r1
.LBB69_15:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end69:
	.size	f77_random_seed, .Lfunc_end69-f77_random_seed
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
	beq r4, r0, .LBB70_2
.LBB70_1:
	ldw r1, r4+4
	addi r5, r0, 4
	blt r1, r5, .LBB70_12
.LBB70_2:
	beq r11, r0, .LBB70_4
.LBB70_3:
	ldw r1, r11+4
	addi r5, r0, 3
	ble r1, r5, .LBB70_12
.LBB70_4:
	beq r4, r0, .LBB70_7
.LBB70_5:
	ldw r1, r4+0
	ldw r4, r4+8
	slli r5, r4, 2
	addi r6, r0, 16
	add r7, r0, r0
.LBB70_6:
	ldw r8, r1+0
	addi r4, sp, 44
	add r9, r4, r7
	stw r9+0, r8
	addi r7, r7, 4
	add r1, r1, r5
	bne r7, r6, .LBB70_6
	jal r0, .LBB70_8
.LBB70_7:
	add r4, r0, r0
.LBB70_8:
	addi r1, r0, 0
	seq r1, r11, r1
	sub r1, r0, r1
	addi r5, sp, 28
	and r1, r5, r1
	xor r5, r5, r1
	jal r31, f77_random_seed
	beq r11, r0, .LBB70_11
.LBB70_9:
	addi r1, sp, 28
	addi r3, r0, 4
	add r4, r0, r0
.LBB70_10:
	ldw r5, r1+0
	ldw r6, r11+0
	ldw r7, r11+8
	mul r7, r7, r4
	slli r7, r7, 2
	add r6, r6, r7
	stw r6+0, r5
	addi r4, r4, 1
	addi r1, r1, 4
	bne r4, r3, .LBB70_10
.LBB70_11:
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.LBB70_12:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end70:
	.size	f77_random_seed_d, .Lfunc_end70-f77_random_seed_d
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
	beq r9, r0, .LBB71_5
.LBB71_1:
	add r11, r9, r0
	ldw r1, r9+4
	addi r9, r0, 7
	ble r1, r9, .LBB71_6
.LBB71_2:
	addi r12, sp, 24
	add r9, r12, r0
	jal r31, f77_date_and_time
	addi r1, r0, 8
	add r3, r0, r0
.LBB71_3:
	ldw r4, r12+0
	ldw r5, r11+0
	ldw r6, r11+8
	mul r6, r6, r3
	slli r6, r6, 2
	add r5, r5, r6
	stw r5+0, r4
	addi r3, r3, 1
	addi r12, r12, 4
	bne r3, r1, .LBB71_3
.LBB71_4:
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.LBB71_5:
	add r9, r0, r0
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jal r0, f77_date_and_time
.LBB71_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end71:
	.size	f77_date_and_time_d, .Lfunc_end71-f77_date_and_time_d
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_random_r
	.type	.LCPI72_0,@object
.LCPI72_0:
	.word	0x33800000                      # float 5.96046448E-8
	.size	.LCPI72_0, 4
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
	lui r4, %hi(.LCPI72_0)
	ldw r4, r4+%lo(.LCPI72_0)
	fmul.s r1, r1, r4
	stw r3+0, r1
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end72:
	.size	f77_random_r, .Lfunc_end72-f77_random_r
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_random_d
	.type	.LCPI73_0,@object
.LCPI73_0:
	.quad	0x4190000000000000              # double 67108864
	.size	.LCPI73_0, 8
	.type	.LCPI73_1,@object
.LCPI73_1:
	.quad	0x3ca0000000000000              # double 1.1102230246251565E-16
	.size	.LCPI73_1, 8
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
	lui r6, %hi(.LCPI73_0)
	addi r6, r6, %lo(.LCPI73_0)
	ldw r9, r6+4
	ldw r8, r6+0
	fmul.d r4, r4, r8
	fcvt.d.wu r6, r1
	fadd.d r4, r4, r6
	lui r1, %hi(.LCPI73_1)
	addi r1, r1, %lo(.LCPI73_1)
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
.Lfunc_end73:
	.size	f77_random_d, .Lfunc_end73-f77_random_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_random_desc
	.type	.LCPI74_0,@object
.LCPI74_0:
	.quad	0x4190000000000000              # double 67108864
	.size	.LCPI74_0, 8
	.type	.LCPI74_1,@object
.LCPI74_1:
	.quad	0x3ca0000000000000              # double 1.1102230246251565E-16
	.size	.LCPI74_1, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI74_2,@object
.LCPI74_2:
	.word	0x33800000                      # float 5.96046448E-8
	.size	.LCPI74_2, 4
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
	blt r12, r3, .LBB74_3
.LBB74_1:
	addi r4, r13, 4
	addi r1, r0, 1
	addi r5, r0, 0
	add r6, r12, r0
.LBB74_2:
	ldw r7, r4+0
	sgt r8, r7, r5
	mul r1, r7, r1
	sub r7, r0, r8
	and r1, r1, r7
	addi r6, r6, -1
	addi r4, r4, 8
	bne r6, r0, .LBB74_2
.LBB74_3:
	blt r1, r3, .LBB74_17
.LBB74_4:
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
	jal r0, .LBB74_6
.LBB74_5:
	addi r16, r16, 1
	beq r16, r1, .LBB74_16
.LBB74_6:
	blt r12, r14, .LBB74_9
.LBB74_7:
	addi r18, sp, 16
	add r19, r10, r0
	add r20, r12, r0
	add r17, r0, r0
.LBB74_8:
	ldw r21, r18+0
	ldw r22, r19+0
	mul r21, r22, r21
	add r17, r21, r17
	addi r20, r20, -1
	addi r19, r19, 8
	addi r18, r18, 4
	bne r20, r0, .LBB74_8
	jal r0, .LBB74_10
.LBB74_9:
	add r17, r0, r0
.LBB74_10:
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
	beq r11, r0, .LBB74_12
.LBB74_11:
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
	lui r18, %hi(.LCPI74_0)
	addi r18, r18, %lo(.LCPI74_0)
	ldw r23, r18+4
	ldw r22, r18+0
	fmul.d r20, r20, r22
	fcvt.d.wu r18, r19
	fadd.d r18, r20, r18
	lui r20, %hi(.LCPI74_1)
	addi r20, r20, %lo(.LCPI74_1)
	ldw r23, r20+4
	ldw r22, r20+0
	fmul.d r18, r18, r22
	ldw r20, r13+0
	slli r17, r17, 3
	add r17, r20, r17
	stw r17+4, r19
	stw r17+0, r18
	bge r12, r14, .LBB74_13
	jal r0, .LBB74_5
.LBB74_12:
	srli r18, r18, 8
	fcvt.s.wu r18, r18
	lui r19, %hi(.LCPI74_2)
	ldw r19, r19+%lo(.LCPI74_2)
	fmul.s r18, r18, r19
	ldw r19, r13+0
	slli r17, r17, 2
	add r17, r19, r17
	stw r17+0, r18
	blt r12, r14, .LBB74_5
.LBB74_13:
	addi r17, sp, 16
	add r18, r4, r0
	add r19, r12, r0
.LBB74_14:
	ldw r20, r17+0
	addi r20, r20, 1
	stw r17+0, r20
	ldw r21, r18+0
	blt r20, r21, .LBB74_5
.LBB74_15:
	stw r17+0, r0
	addi r19, r19, -1
	addi r18, r18, 8
	addi r17, r17, 4
	bne r19, r0, .LBB74_14
	jal r0, .LBB74_5
.LBB74_16:
	stw r3+%lo(f77_rng+4), r6
	stw r5+%lo(f77_rng), r8
	stw r7+%lo(f77_rng+8), lr
	stw r9+%lo(f77_rng+12), r15
.LBB74_17:
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
.Lfunc_end74:
	.size	f77_random_desc, .Lfunc_end74-f77_random_desc
                                        # -- End function
	.globl	f77_len_trim                    # -- Begin function f77_len_trim
	.p2align	2
	.type	f77_len_trim,@function
f77_len_trim:                           # @f77_len_trim
# %bb.0:
	srai r1, r4, 31
	and r1, r4, r1
	addi r5, r0, 1
	blt r4, r5, .LBB75_5
.LBB75_1:
	addi r3, r3, -1
	addi r5, r0, 32
	addi r6, r0, 2
.LBB75_2:
	add r7, r4, r0
	add r4, r3, r4
	ldbu r4, r4+0
	bne r4, r5, .LBB75_4
.LBB75_3:
	addi r4, r7, -1
	bge r7, r6, .LBB75_2
	jal r0, .LBB75_5
.LBB75_4:
	add r1, r7, r0
.LBB75_5:
	jalr r0, r31, 0
.Lfunc_end75:
	.size	f77_len_trim, .Lfunc_end75-f77_len_trim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_exponent
	.type	.LCPI76_0,@object
.LCPI76_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI76_0, 8
	.text
	.globl	f77_exponent
	.p2align	2
	.type	f77_exponent,@function
f77_exponent:                           # @f77_exponent
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI76_0)
	addi r1, r1, %lo(.LCPI76_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r6, r4
	add r1, r0, r0
	bne r3, r0, .LBB76_2
.LBB76_1:
	addi sp, sp, -32
	stw sp+0, lr
	addi r5, sp, 28
	add r3, r6, r0
	add r4, r7, r0
	jal r31, frexp
	ldw r1, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB76_2:
	jalr r0, r31, 0
.Lfunc_end76:
	.size	f77_exponent, .Lfunc_end76-f77_exponent
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zsqrt
	.type	.LCPI77_0,@object
.LCPI77_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI77_0, 8
	.type	.LCPI77_1,@object
.LCPI77_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI77_1, 8
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
	lui r1, %hi(.LCPI77_0)
	addi r1, r1, %lo(.LCPI77_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r14, r1, r0
	add r15, r2, r0
	lui r1, %hi(.LCPI77_1)
	addi r1, r1, %lo(.LCPI77_1)
	ldw r17, r1+4
	ldw r16, r1+0
	fle.d r1, r16, r18
	xori r1, r1, 1
	beq r1, r0, .LBB77_4
.LBB77_1:
	feq.d r1, r14, r16
	add r4, r16, r0
	add r5, r17, r0
	bne r1, r0, .LBB77_3
.LBB77_2:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r14, r14
	fdiv.d r4, r4, r6
.LBB77_3:
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
	jal r0, .LBB77_6
.LBB77_4:
	stw r11+4, r15
	stw r11+0, r14
	feq.d r1, r14, r16
	bne r1, r0, .LBB77_6
.LBB77_5:
	fadd.d r4, r14, r14
	fdiv.d r16, r12, r4
.LBB77_6:
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
.Lfunc_end77:
	.size	f77_zsqrt, .Lfunc_end77-f77_zsqrt
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
.Lfunc_end78:
	.size	f77_zexp, .Lfunc_end78-f77_zexp
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
.Lfunc_end79:
	.size	f77_zlog, .Lfunc_end79-f77_zlog
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
.Lfunc_end80:
	.size	f77_zsin, .Lfunc_end80-f77_zsin
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
.Lfunc_end81:
	.size	f77_zcos, .Lfunc_end81-f77_zcos
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
.Lfunc_end82:
	.size	f77i_iabs, .Lfunc_end82-f77i_iabs
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_abs
	.type	.LCPI83_0,@object
.LCPI83_0:
	.word	0x00000000                      # float 0
	.size	.LCPI83_0, 4
	.text
	.globl	f77i_abs
	.p2align	2
	.type	f77i_abs,@function
f77i_abs:                               # @f77i_abs
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI83_0)
	ldw r3, r3+%lo(.LCPI83_0)
	flt.s r3, r1, r3
	sub r3, r0, r3
	lui r4, 524288
	and r3, r3, r4
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end83:
	.size	f77i_abs, .Lfunc_end83-f77i_abs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dabs
	.type	.LCPI84_0,@object
.LCPI84_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI84_0, 8
	.text
	.globl	f77i_dabs
	.p2align	2
	.type	f77i_dabs,@function
f77i_dabs:                              # @f77i_dabs
# %bb.0:
	ldw r5, r3+4
	ldw r4, r3+0
	lui r1, %hi(.LCPI84_0)
	addi r1, r1, %lo(.LCPI84_0)
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
.Lfunc_end84:
	.size	f77i_dabs, .Lfunc_end84-f77i_dabs
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
.Lfunc_end85:
	.size	f77i_sqrt, .Lfunc_end85-f77i_sqrt
                                        # -- End function
	.globl	f77i_dsqrt                      # -- Begin function f77i_dsqrt
	.p2align	2
	.type	f77i_dsqrt,@function
f77i_dsqrt:                             # @f77i_dsqrt
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sqrt
.Lfunc_end86:
	.size	f77i_dsqrt, .Lfunc_end86-f77i_dsqrt
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
.Lfunc_end87:
	.size	f77i_exp, .Lfunc_end87-f77i_exp
                                        # -- End function
	.globl	f77i_dexp                       # -- Begin function f77i_dexp
	.p2align	2
	.type	f77i_dexp,@function
f77i_dexp:                              # @f77i_dexp
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, exp
.Lfunc_end88:
	.size	f77i_dexp, .Lfunc_end88-f77i_dexp
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
.Lfunc_end89:
	.size	f77i_alog, .Lfunc_end89-f77i_alog
                                        # -- End function
	.globl	f77i_dlog                       # -- Begin function f77i_dlog
	.p2align	2
	.type	f77i_dlog,@function
f77i_dlog:                              # @f77i_dlog
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log
.Lfunc_end90:
	.size	f77i_dlog, .Lfunc_end90-f77i_dlog
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
.Lfunc_end91:
	.size	f77i_alog10, .Lfunc_end91-f77i_alog10
                                        # -- End function
	.globl	f77i_dlog10                     # -- Begin function f77i_dlog10
	.p2align	2
	.type	f77i_dlog10,@function
f77i_dlog10:                            # @f77i_dlog10
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log10
.Lfunc_end92:
	.size	f77i_dlog10, .Lfunc_end92-f77i_dlog10
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
.Lfunc_end93:
	.size	f77i_sin, .Lfunc_end93-f77i_sin
                                        # -- End function
	.globl	f77i_dsin                       # -- Begin function f77i_dsin
	.p2align	2
	.type	f77i_dsin,@function
f77i_dsin:                              # @f77i_dsin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sin
.Lfunc_end94:
	.size	f77i_dsin, .Lfunc_end94-f77i_dsin
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
.Lfunc_end95:
	.size	f77i_cos, .Lfunc_end95-f77i_cos
                                        # -- End function
	.globl	f77i_dcos                       # -- Begin function f77i_dcos
	.p2align	2
	.type	f77i_dcos,@function
f77i_dcos:                              # @f77i_dcos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cos
.Lfunc_end96:
	.size	f77i_dcos, .Lfunc_end96-f77i_dcos
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
.Lfunc_end97:
	.size	f77i_tan, .Lfunc_end97-f77i_tan
                                        # -- End function
	.globl	f77i_dtan                       # -- Begin function f77i_dtan
	.p2align	2
	.type	f77i_dtan,@function
f77i_dtan:                              # @f77i_dtan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tan
.Lfunc_end98:
	.size	f77i_dtan, .Lfunc_end98-f77i_dtan
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
.Lfunc_end99:
	.size	f77i_asin, .Lfunc_end99-f77i_asin
                                        # -- End function
	.globl	f77i_dasin                      # -- Begin function f77i_dasin
	.p2align	2
	.type	f77i_dasin,@function
f77i_dasin:                             # @f77i_dasin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, asin
.Lfunc_end100:
	.size	f77i_dasin, .Lfunc_end100-f77i_dasin
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
.Lfunc_end101:
	.size	f77i_acos, .Lfunc_end101-f77i_acos
                                        # -- End function
	.globl	f77i_dacos                      # -- Begin function f77i_dacos
	.p2align	2
	.type	f77i_dacos,@function
f77i_dacos:                             # @f77i_dacos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, acos
.Lfunc_end102:
	.size	f77i_dacos, .Lfunc_end102-f77i_dacos
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
.Lfunc_end103:
	.size	f77i_atan, .Lfunc_end103-f77i_atan
                                        # -- End function
	.globl	f77i_datan                      # -- Begin function f77i_datan
	.p2align	2
	.type	f77i_datan,@function
f77i_datan:                             # @f77i_datan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, atan
.Lfunc_end104:
	.size	f77i_datan, .Lfunc_end104-f77i_datan
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
.Lfunc_end105:
	.size	f77i_atan2, .Lfunc_end105-f77i_atan2
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
.Lfunc_end106:
	.size	f77i_datan2, .Lfunc_end106-f77i_datan2
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
.Lfunc_end107:
	.size	f77i_sinh, .Lfunc_end107-f77i_sinh
                                        # -- End function
	.globl	f77i_dsinh                      # -- Begin function f77i_dsinh
	.p2align	2
	.type	f77i_dsinh,@function
f77i_dsinh:                             # @f77i_dsinh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sinh
.Lfunc_end108:
	.size	f77i_dsinh, .Lfunc_end108-f77i_dsinh
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
.Lfunc_end109:
	.size	f77i_cosh, .Lfunc_end109-f77i_cosh
                                        # -- End function
	.globl	f77i_dcosh                      # -- Begin function f77i_dcosh
	.p2align	2
	.type	f77i_dcosh,@function
f77i_dcosh:                             # @f77i_dcosh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cosh
.Lfunc_end110:
	.size	f77i_dcosh, .Lfunc_end110-f77i_dcosh
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
.Lfunc_end111:
	.size	f77i_tanh, .Lfunc_end111-f77i_tanh
                                        # -- End function
	.globl	f77i_dtanh                      # -- Begin function f77i_dtanh
	.p2align	2
	.type	f77i_dtanh,@function
f77i_dtanh:                             # @f77i_dtanh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tanh
.Lfunc_end112:
	.size	f77i_dtanh, .Lfunc_end112-f77i_dtanh
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
.Lfunc_end113:
	.size	f77i_aint, .Lfunc_end113-f77i_aint
                                        # -- End function
	.globl	f77i_dint                       # -- Begin function f77i_dint
	.p2align	2
	.type	f77i_dint,@function
f77i_dint:                              # @f77i_dint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, trunc
.Lfunc_end114:
	.size	f77i_dint, .Lfunc_end114-f77i_dint
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
.Lfunc_end115:
	.size	f77i_anint, .Lfunc_end115-f77i_anint
                                        # -- End function
	.globl	f77i_dnint                      # -- Begin function f77i_dnint
	.p2align	2
	.type	f77i_dnint,@function
f77i_dnint:                             # @f77i_dnint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, round
.Lfunc_end116:
	.size	f77i_dnint, .Lfunc_end116-f77i_dnint
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
.Lfunc_end117:
	.size	f77i_nint, .Lfunc_end117-f77i_nint
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
.Lfunc_end118:
	.size	f77i_idnint, .Lfunc_end118-f77i_idnint
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
.Lfunc_end119:
	.size	f77i_mod, .Lfunc_end119-f77i_mod
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
.Lfunc_end120:
	.size	f77i_amod, .Lfunc_end120-f77i_amod
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
.Lfunc_end121:
	.size	f77i_dmod, .Lfunc_end121-f77i_dmod
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
.Lfunc_end122:
	.size	f77i_isign, .Lfunc_end122-f77i_isign
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_sign
	.type	.LCPI123_0,@object
.LCPI123_0:
	.word	0x00000000                      # float 0
	.size	.LCPI123_0, 4
	.text
	.globl	f77i_sign
	.p2align	2
	.type	f77i_sign,@function
f77i_sign:                              # @f77i_sign
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI123_0)
	ldw r3, r3+%lo(.LCPI123_0)
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
.Lfunc_end123:
	.size	f77i_sign, .Lfunc_end123-f77i_sign
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dsign
	.type	.LCPI124_0,@object
.LCPI124_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI124_0, 8
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
	lui r1, %hi(.LCPI124_0)
	addi r1, r1, %lo(.LCPI124_0)
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
.Lfunc_end124:
	.size	f77i_dsign, .Lfunc_end124-f77i_dsign
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
.Lfunc_end125:
	.size	f77i_idim, .Lfunc_end125-f77i_idim
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
.Lfunc_end126:
	.size	f77i_dim, .Lfunc_end126-f77i_dim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_ddim
	.type	.LCPI127_0,@object
.LCPI127_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI127_0, 8
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
	lui r3, %hi(.LCPI127_0)
	addi r3, r3, %lo(.LCPI127_0)
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
.Lfunc_end127:
	.size	f77i_ddim, .Lfunc_end127-f77i_ddim
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
.Lfunc_end128:
	.size	f77i_dprod, .Lfunc_end128-f77i_dprod
                                        # -- End function
	.globl	f77i_len                        # -- Begin function f77i_len
	.p2align	2
	.type	f77i_len,@function
f77i_len:                               # @f77i_len
# %bb.0:
	add r1, r4, r0
	jalr r0, r31, 0
.Lfunc_end129:
	.size	f77i_len, .Lfunc_end129-f77i_len
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
	beq r6, r0, .LBB130_6
.LBB130_1:
	add r12, r6, r0
	bgt r6, r11, .LBB130_5
.LBB130_2:
	add r13, r4, r0
	add r14, r3, r0
	add r15, r0, r0
.LBB130_3:
	add r3, r14, r15
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcmp
	beq r1, r0, .LBB130_7
.LBB130_4:
	addi r15, r15, 1
	add r1, r12, r15
	ble r1, r11, .LBB130_3
.LBB130_5:
	add r1, r0, r0
	jal r0, .LBB130_8
.LBB130_6:
	addi r1, r0, 0
	sgt r1, r11, r1
	jal r0, .LBB130_8
.LBB130_7:
	addi r1, r15, 1
.LBB130_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end130:
	.size	f77i_index, .Lfunc_end130-f77i_index
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
	beq r6, r0, .LBB131_6
.LBB131_1:
	add r12, r6, r0
	bgt r6, r11, .LBB131_5
.LBB131_2:
	add r13, r5, r0
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
	.size	f77_index, .Lfunc_end131-f77_index
                                        # -- End function
	.globl	f77i_aimag                      # -- Begin function f77i_aimag
	.p2align	2
	.type	f77i_aimag,@function
f77i_aimag:                             # @f77i_aimag
# %bb.0:
	ldw r1, r3+4
	jalr r0, r31, 0
.Lfunc_end132:
	.size	f77i_aimag, .Lfunc_end132-f77i_aimag
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
.Lfunc_end133:
	.size	f77i_cabs, .Lfunc_end133-f77i_cabs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_csqrt
	.type	.LCPI134_0,@object
.LCPI134_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI134_0, 8
	.type	.LCPI134_2,@object
.LCPI134_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI134_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI134_1,@object
.LCPI134_1:
	.word	0x00000000                      # float 0
	.size	.LCPI134_1, 4
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
	lui r1, %hi(.LCPI134_0)
	addi r1, r1, %lo(.LCPI134_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r14, r1, r0
	add r15, r2, r0
	lui r1, %hi(.LCPI134_1)
	ldw r3, r1+%lo(.LCPI134_1)
	fle.s r4, r3, r16
	xori r4, r4, 1
	beq r4, r0, .LBB134_4
.LBB134_1:
	ldw r16, r1+%lo(.LCPI134_1)
	lui r1, %hi(.LCPI134_2)
	addi r1, r1, %lo(.LCPI134_2)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r14, r4
	add r1, r16, r0
	bne r3, r0, .LBB134_3
.LBB134_2:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r14, r14
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
.LBB134_3:
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
	jal r0, .LBB134_6
.LBB134_4:
	fcvt.s.d r1, r14
	lui r4, %hi(.LCPI134_2)
	addi r4, r4, %lo(.LCPI134_2)
	ldw r7, r4+4
	ldw r6, r4+0
	feq.d r4, r14, r6
	bne r4, r0, .LBB134_6
.LBB134_5:
	fadd.d r4, r14, r14
	fdiv.d r4, r12, r4
	fcvt.s.d r3, r4
.LBB134_6:
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
.Lfunc_end134:
	.size	f77i_csqrt, .Lfunc_end134-f77i_csqrt
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
.Lfunc_end135:
	.size	f77i_cexp, .Lfunc_end135-f77i_cexp
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
.Lfunc_end136:
	.size	f77i_clog, .Lfunc_end136-f77i_clog
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
.Lfunc_end137:
	.size	f77i_csin, .Lfunc_end137-f77i_csin
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
.Lfunc_end138:
	.size	f77i_ccos, .Lfunc_end138-f77i_ccos
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
.Lfunc_end139:
	.size	f77i_conjg, .Lfunc_end139-f77i_conjg
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
	blt r14, r1, .LBB140_2
.LBB140_1:
	add r3, r12, r0
	add r4, r5, r0
	add r5, r14, r0
	jal r31, memmove
.LBB140_2:
	bge r13, r11, .LBB140_5
.LBB140_3:
	add r1, r12, r14
	sub r3, r11, r14
	addi r4, r0, 32
.LBB140_4:
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB140_4
.LBB140_5:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end140:
	.size	f77_strcpy, .Lfunc_end140-f77_strcpy
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
	blt r7, r1, .LBB141_8
.LBB141_1:
	addi r8, r0, 32
	add r9, r0, r0
                                        # implicit-def: $r1
.LBB141_2:
	add r10, r8, r0
	bge r9, r4, .LBB141_4
.LBB141_3:
	add r10, r3, r9
	ldbu r10, r10+0
.LBB141_4:
	add lr, r8, r0
	bge r9, r6, .LBB141_6
.LBB141_5:
	add lr, r5, r9
	ldbu lr, lr+0
.LBB141_6:
	seq r11, r10, lr
	sltu r12, r10, lr
	sub r12, r0, r12
	ori  r12, r12, 1
	xor r1, r1, r12
	sub r11, r0, r11
	and r1, r1, r11
	xor r1, r12, r1
	bne r10, lr, .LBB141_9
.LBB141_7:
	addi r9, r9, 1
	bne r7, r9, .LBB141_2
.LBB141_8:
	add r1, r0, r0
.LBB141_9:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end141:
	.size	f77_strcmp, .Lfunc_end141-f77_strcmp
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
	bgeu r5, r6, .LBB142_8
.LBB142_1:
	lui r3, %hi(fio_catpos)
	ldw r7, r3+%lo(fio_catpos)
	add r7, r7, r5
	blt r7, r6, .LBB142_3
.LBB142_2:
	stw r3+%lo(fio_catpos), r0
.LBB142_3:
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
	blt r1, r15, .LBB142_5
.LBB142_4:
	add r3, r13, r0
	add r5, r1, r0
	jal r31, memmove
.LBB142_5:
	blt r11, r15, .LBB142_7
.LBB142_6:
	add r3, r13, r14
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memmove
.LBB142_7:
	add r1, r13, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB142_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end142:
	.size	f77_cat, .Lfunc_end142-f77_cat
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
	ble r5, r1, .LBB143_8
.LBB143_1:
	add r12, r3, r0
	addi r1, r0, 0
	sgt r1, r4, r1
	sub r1, r0, r1
	and r13, r4, r1
	mul r5, r13, r11
	lui r1, 16
	addi r3, r1, 1
	bge r5, r3, .LBB143_9
.LBB143_2:
	lui r1, %hi(fio_catpos)
	ldw r4, r1+%lo(fio_catpos)
	add r4, r4, r5
	blt r4, r3, .LBB143_4
.LBB143_3:
	stw r1+%lo(fio_catpos), r0
.LBB143_4:
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
	beq r11, r0, .LBB143_7
.LBB143_5:
	add r15, r14, r0
.LBB143_6:
	add r3, r15, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, memmove
	addi r11, r11, -1
	add r15, r15, r13
	bne r11, r0, .LBB143_6
.LBB143_7:
	add r1, r14, r0
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
	add r5, r11, r0
	jal r0, .LBB143_10
.LBB143_9:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.121)
	addi r4, r4, %lo(.L.str.121)
.LBB143_10:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end143:
	.size	f77_repeat, .Lfunc_end143-f77_repeat
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
	bge r4, r3, .LBB144_18
.LBB144_1:
	lui r1, %hi(fio_catpos)
	ldw r4, r1+%lo(fio_catpos)
	add r4, r4, r11
	blt r4, r3, .LBB144_3
.LBB144_2:
	stw r1+%lo(fio_catpos), r0
.LBB144_3:
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
	beq r5, r0, .LBB144_10
.LBB144_4:
	add r14, r11, r0
	blt r11, r1, .LBB144_9
.LBB144_5:
	addi r3, r0, 32
	addi r4, r13, -1
	add r5, r11, r0
.LBB144_6:
	add r14, r5, r0
	add r5, r4, r5
	ldbu r5, r5+0
	bne r5, r3, .LBB144_9
.LBB144_7:
	addi r5, r14, -1
	bgt r14, r1, .LBB144_6
.LBB144_8:
	add r14, r0, r0
.LBB144_9:
	sub r5, r11, r14
	addi r4, r0, 32
	add r3, r12, r0
	jal r31, memset
	add r1, r12, r11
	sub r3, r1, r14
	add r4, r13, r0
	add r5, r14, r0
	jal r31, memmove
	jal r0, .LBB144_17
.LBB144_10:
	blt r11, r1, .LBB144_15
.LBB144_11:
	addi r1, r0, 32
	add r14, r0, r0
.LBB144_12:
	add r3, r13, r14
	ldbu r3, r3+0
	bne r3, r1, .LBB144_16
.LBB144_13:
	addi r14, r14, 1
	bne r11, r14, .LBB144_12
.LBB144_14:
	add r14, r11, r0
	jal r0, .LBB144_16
.LBB144_15:
	add r14, r0, r0
.LBB144_16:
	add r4, r13, r14
	sub r5, r11, r14
	add r3, r12, r0
	jal r31, memmove
	add r1, r12, r11
	sub r3, r1, r14
	addi r4, r0, 32
	add r5, r14, r0
	jal r31, memset
.LBB144_17:
	add r1, r12, r0
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB144_18:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.121)
	addi r4, r4, %lo(.L.str.121)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end144:
	.size	f77_adjust, .Lfunc_end144-f77_adjust
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
	blt r4, r16, .LBB145_7
.LBB145_1:
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
.LBB145_2:
	xor r1, r21, r17
	and r1, r1, r19
	xor r22, r17, r1
	blt r12, r16, .LBB145_4
.LBB145_3:
	add r1, r14, r22
	ldb r4, r1+0
	add r3, r13, r0
	add r5, r12, r0
	jal r31, memchr
	sne r1, r1, r18
	jal r0, .LBB145_5
.LBB145_4:
	add r1, r0, r0
.LBB145_5:
	seq r3, r1, r11
	addi r4, r22, 1
	xor r5, r15, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r15, r4, r3
	bne r1, r11, .LBB145_8
.LBB145_6:
	addi r21, r21, 1
	addi r17, r17, -1
	bne r17, r20, .LBB145_2
.LBB145_7:
	add r15, r0, r0
.LBB145_8:
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
.Lfunc_end145:
	.size	f77_scanset, .Lfunc_end145-f77_scanset
                                        # -- End function
	.globl	f77_index_back                  # -- Begin function f77_index_back
	.p2align	2
	.type	f77_index_back,@function
f77_index_back:                         # @f77_index_back
# %bb.0:
	ble r6, r0, .LBB146_6
.LBB146_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r11, r6, r0
	sub r1, r4, r6
	blt r1, r0, .LBB146_5
.LBB146_2:
	add r12, r5, r0
	add r13, r3, r0
.LBB146_3:
	add r14, r1, r0
	add r3, r13, r1
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memcmp
	beq r1, r0, .LBB146_7
.LBB146_4:
	addi r1, r14, -1
	bgt r14, r0, .LBB146_3
.LBB146_5:
	add r1, r0, r0
	jal r0, .LBB146_8
.LBB146_6:
	addi r1, r0, 0
	sgt r1, r4, r1
	sub r1, r0, r1
	and r1, r4, r1
	addi r1, r1, 1
	jal r0, .LBB146_9
.LBB146_7:
	addi r1, r14, 1
.LBB146_8:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB146_9:
	jalr r0, r31, 0
.Lfunc_end146:
	.size	f77_index_back, .Lfunc_end146-f77_index_back
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
	beq r1, r0, .LBB147_15
.LBB147_1:
	lui r1, %hi(fio_col)
	ldw r4, r1+%lo(fio_col)
	addi r5, r0, 1022
	bgt r4, r5, .LBB147_7
.LBB147_2:
	lui r6, %hi(fio_len)
	ldw r7, r6+%lo(fio_len)
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	addi r9, r0, 32
	bge r7, r4, .LBB147_5
.LBB147_3:
	addi r10, r7, 1
	add r7, r7, r8
	stb r7+0, r9
	add r7, r10, r0
	bne r4, r10, .LBB147_3
.LBB147_4:
	stw r6+%lo(fio_len), r4
	add r7, r4, r0
.LBB147_5:
	add r8, r4, r8
	stb r8+0, r9
	addi r8, r4, 1
	stw r1+%lo(fio_col), r8
	blt r4, r7, .LBB147_7
.LBB147_6:
	stw r6+%lo(fio_len), r8
.LBB147_7:
	ldw r4, r1+%lo(fio_col)
	bgt r4, r5, .LBB147_14
.LBB147_8:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	bge r6, r4, .LBB147_12
.LBB147_9:
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
.LBB147_10:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r4, r9, .LBB147_10
.LBB147_11:
	stw r5+%lo(fio_len), r4
	add r6, r4, r0
.LBB147_12:
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
	blt r4, r6, .LBB147_14
.LBB147_13:
	stw r5+%lo(fio_len), r3
.LBB147_14:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	jal r0, .LBB147_40
.LBB147_15:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB147_40
.LBB147_16:
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB147_18
.LBB147_17:
	ldw r1, r12+%lo(fio_rep)
	bgt r1, r0, .LBB147_19
.LBB147_18:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	jal r31, fio_next_desc
	add r3, r13, r0
	beq r1, r0, .LBB147_40
.LBB147_19:
	lui r1, %hi(fio_w)
	ldw r13, r1+%lo(fio_w)
	addi r1, r0, 2
	blt r13, r1, .LBB147_29
.LBB147_20:
	lui r4, %hi(fio_len)
	ldw r9, r4+%lo(fio_len)
	lui r5, %hi(fio_col)
	ldw r10, r5+%lo(fio_col)
	addi r6, r0, 1022
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
	jal r0, .LBB147_23
.LBB147_21:
	add r10, r13, r0
.LBB147_22:
	addi r13, lr, -1
	ble lr, r1, .LBB147_29
.LBB147_23:
	add lr, r13, r0
	bgt r10, r6, .LBB147_22
.LBB147_24:
	bge r9, r10, .LBB147_27
.LBB147_25:
	addi r13, r9, 1
	add r9, r9, r7
	stb r9+0, r8
	add r9, r13, r0
	bne r10, r13, .LBB147_25
.LBB147_26:
	stw r4+%lo(fio_len), r10
	add r9, r10, r0
.LBB147_27:
	add r13, r10, r7
	stb r13+0, r8
	addi r13, r10, 1
	stw r5+%lo(fio_col), r13
	blt r10, r9, .LBB147_21
.LBB147_28:
	stw r4+%lo(fio_len), r13
	add r9, r13, r0
	jal r0, .LBB147_21
.LBB147_29:
	lui r4, %hi(fio_col)
	ldw r1, r4+%lo(fio_col)
	addi r5, r0, 1022
	bgt r1, r5, .LBB147_36
.LBB147_30:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	bge r6, r1, .LBB147_34
.LBB147_31:
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
.LBB147_32:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r1, r9, .LBB147_32
.LBB147_33:
	stw r5+%lo(fio_len), r1
	add r6, r1, r0
.LBB147_34:
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
	blt r1, r6, .LBB147_36
.LBB147_35:
	stw r5+%lo(fio_len), r3
.LBB147_36:
	ldw r1, r12+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB147_38
.LBB147_37:
	addi r1, r1, -1
	stw r12+%lo(fio_rep), r1
.LBB147_38:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB147_40
.LBB147_39:
	stw r11+%lo(fio_desc), r0
.LBB147_40:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end147:
	.size	f77_wr_l, .Lfunc_end147-f77_wr_l
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
	bne r1, r0, .LBB148_4
.LBB148_1:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB148_3
.LBB148_2:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB148_4
.LBB148_3:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB148_4:
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
.Lfunc_end148:
	.size	f77_wr_end, .Lfunc_end148-f77_wr_end
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
	lui r27, %hi(.LJTI149_4)
	addi r27, r27, %lo(.LJTI149_4)
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
	jal r0, .LBB149_5
.LBB149_1:
	addi r1, r4, 1
.LBB149_2:
	stw r14+%lo(fio_pos), r1
.LBB149_3:
	bgtu r16, r18, .LBB149_109
.LBB149_4:
	addi r16, r16, 1
.LBB149_5:
	ldw r3, r15+%lo(fio_fmt)
	ldw r4, r14+%lo(fio_pos)
	add r7, r3, r4
	ldb r6, r7+0
	andi r1, r6, 255
	beq r1, r17, .LBB149_1
.LBB149_6:
	beq r1, r19, .LBB149_1
.LBB149_7:
	bne r1, r0, .LBB149_16
.LBB149_8:
	ldw r1, r20+%lo(fio_gdepth)
	blt r1, fp, .LBB149_10
.LBB149_9:
	stw r20+%lo(fio_gdepth), r0
.LBB149_10:
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB149_109
.LBB149_11:
	ldw r4, r22+%lo(fio_revert)
	add r1, r0, r0
	blt r4, r0, .LBB149_110
.LBB149_12:
	ldbu r1, r13+%lo(fio_since_data)
	beq r1, r0, .LBB149_131
.LBB149_13:
	stb r13+%lo(fio_since_data), r0
	stw r14+%lo(fio_pos), r4
	add r1, r3, r4
	ldbu r1, r1+0
	beq r1, r0, .LBB149_109
.LBB149_14:
	ldbu r1, r23+%lo(fio_reading)
	bne r1, fp, .LBB149_88
.LBB149_15:
	jal r31, fio_getrec
	beq r1, r0, .LBB149_3
	jal r0, .LBB149_132
.LBB149_16:
	and r5, r6, r24
	bne r5, r12, .LBB149_20
.LBB149_17:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	add r1, r3, r1
	ldbu r1, r1+0
	addi r1, r1, -80
	bgtu r1, r25, .LBB149_50
.LBB149_18:
	slli r1, r1, 2
	add r1, r27, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB149_19:
	stb r26+%lo(fio_sp), fp
	jal r0, .LBB149_28
.LBB149_20:
	bne r6, r28, .LBB149_23
.LBB149_21:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB149_109
.LBB149_22:
	add r1, r0, r0
	bleu r16, r18, .LBB149_4
	jal r0, .LBB149_110
.LBB149_23:
	andi r5, r6, 223
	ldw r8, sp+52
	bne r5, r8, .LBB149_29
.LBB149_24:
	ldbu r5, r7+1
	addi r8, r5, -78
	andi r9, r8, 252
	slli r8, r8, 6
	srli r9, r9, 2
	or  r8, r8, r9
	andi r8, r8, 255
	ldw r9, sp+44
	bgtu r8, r9, .LBB149_29
.LBB149_25:
	sll r8, fp, r8
	andi r8, r8, 2313
	beq r8, r0, .LBB149_29
.LBB149_26:
	andi r1, r5, 223
	ldw r3, sp+40
	seq r1, r1, r3
	ldw r3, sp+36
	stw r3+%lo(fio_bz), r1
	jal r0, .LBB149_28
.LBB149_27:
	stb r26+%lo(fio_sp), r0
.LBB149_28:
	addi r1, r4, 2
	jal r0, .LBB149_2
.LBB149_29:
	addi r5, r1, -39
	lui r1, %hi(fio_line)
	addi r1, r1, %lo(fio_line)
	ldw r8, sp+48
	bgtu r5, r8, .LBB149_53
.LBB149_30:
	slli r5, r5, 2
	lui r8, %hi(.LJTI149_0)
	addi r8, r8, %lo(.LJTI149_0)
	add r5, r8, r5
	ldw r8, r5+0
	add r5, fp, r0
	jalr r0, r8, 0
.LBB149_31:
	addi r4, r4, 1
	stw r14+%lo(fio_pos), r4
	lui r5, %hi(fio_col)
	ldw r10, r5+%lo(fio_col)
	lui r6, %hi(fio_len)
	ldw r8, r6+%lo(fio_len)
	ldbu r7, r23+%lo(fio_reading)
	jal r0, .LBB149_35
.LBB149_32:
	addi lr, r0, 1
.LBB149_33:
	add r10, r9, r0
.LBB149_34:
	add r4, r4, lr
	stw r14+%lo(fio_pos), r4
.LBB149_35:
	add r9, r10, r0
	add r10, r3, r4
	ldbu r10, r10+0
	addi lr, r0, 39
	beq r10, lr, .LBB149_42
.LBB149_36:
	beq r10, r0, .LBB149_3
.LBB149_37:
	addi lr, r0, 1022
	sgt lr, r9, lr
	or  lr, r7, lr
	andi lr, lr, 1
	bne lr, r0, .LBB149_32
.LBB149_38:
	bge r8, r9, .LBB149_41
.LBB149_39:
	addi lr, r8, 1
	add r8, r8, r1
	stb r8+0, r19
	add r8, lr, r0
	bne r9, lr, .LBB149_39
.LBB149_40:
	stw r6+%lo(fio_len), r9
	add r8, r9, r0
.LBB149_41:
	add lr, r9, r1
	stb lr+0, r10
	addi r10, r9, 1
	stw r5+%lo(fio_col), r10
	addi lr, r0, 1
	blt r9, r8, .LBB149_34
	jal r0, .LBB149_49
.LBB149_42:
	addi r10, r4, 1
	add r11, r3, r10
	ldbu r11, r11+0
	bne r11, lr, .LBB149_82
.LBB149_43:
	addi r10, r0, 1022
	sgt r10, r9, r10
	or  r10, r7, r10
	andi r10, r10, 1
	beq r10, r0, .LBB149_45
.LBB149_44:
	addi lr, r0, 2
	jal r0, .LBB149_33
.LBB149_45:
	bge r8, r9, .LBB149_48
.LBB149_46:
	addi r10, r8, 1
	add r8, r8, r1
	stb r8+0, r19
	add r8, r10, r0
	bne r9, r10, .LBB149_46
.LBB149_47:
	stw r6+%lo(fio_len), r9
	add r8, r9, r0
.LBB149_48:
	add r10, r9, r1
	stb r10+0, lr
	addi r10, r9, 1
	stw r5+%lo(fio_col), r10
	addi lr, r0, 2
	blt r9, r8, .LBB149_34
.LBB149_49:
	stw r6+%lo(fio_len), r10
	add r8, r10, r0
	jal r0, .LBB149_34
.LBB149_50:
	stb r26+%lo(fio_sp), r0
	jal r0, .LBB149_3
.LBB149_51:
	ldb r5, r7+1
	addi r5, r5, -58
	addi r6, r0, -10
	bgeu r5, r6, .LBB149_58
.LBB149_52:
	addi r6, r0, 45
.LBB149_53:
	add r5, fp, r0
	jal r0, .LBB149_59
.LBB149_54:
	ldw r1, r20+%lo(fio_gdepth)
	beq r1, r0, .LBB149_83
.LBB149_55:
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
	blt r5, r4, .LBB149_3
.LBB149_56:
	lui r4, %hi(fio_gstart)
	addi r4, r4, %lo(fio_gstart)
	add r3, r3, r4
	ldw r3, r3+0
	stw r14+%lo(fio_pos), r3
	stw r20+%lo(fio_gdepth), r1
	jal r0, .LBB149_3
.LBB149_57:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB149_87
.LBB149_58:
	addi r5, r4, 1
	stw r14+%lo(fio_pos), r5
	add r5, r3, r5
	ldb r6, r5+0
	add r5, r0, r0
.LBB149_59:
	addi r6, r6, -58
	addi r10, r0, -10
	add r11, fp, r0
	bltu r6, r10, .LBB149_65
.LBB149_60:
	ldw r6, r14+%lo(fio_pos)
	add r7, r3, r6
	ldb r7, r7+0
	addi r7, r7, -48
	addi r8, r0, 9
	bleu r7, r8, .LBB149_62
.LBB149_61:
	add r11, r0, r0
	jal r0, .LBB149_65
.LBB149_62:
	stw sp+32, r10
	addi r8, r3, 1
	add r11, r0, r0
.LBB149_63:
	addi r10, r0, 10
	mul lr, r11, r10
	addi r9, r6, 1
	add r11, lr, r7
	add r6, r8, r6
	ldb r6, r6+0
	addi r7, r6, -48
	add r6, r9, r0
	bltu r7, r10, .LBB149_63
.LBB149_64:
	stw r14+%lo(fio_pos), r9
	ldw r10, sp+32
.LBB149_65:
	ldw r6, r14+%lo(fio_pos)
	add r7, r3, r6
	ldbu r7, r7+0
	addi r8, r0, 40
	bne r7, r8, .LBB149_69
.LBB149_66:
	addi r3, r6, 1
	stw r14+%lo(fio_pos), r3
	ldw r1, r20+%lo(fio_gdepth)
	addi r5, r0, 15
	bgt r1, r5, .LBB149_3
.LBB149_67:
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
	bne r1, r0, .LBB149_3
.LBB149_68:
	stw r22+%lo(fio_revert), r4
	jal r0, .LBB149_3
.LBB149_69:
	andi r4, r7, 223
	addi r8, r4, -72
	addi r9, r0, 16
	bgtu r8, r9, .LBB149_101
.LBB149_70:
	slli r8, r8, 2
	lui r9, %hi(.LJTI149_1)
	addi r9, r9, %lo(.LJTI149_1)
	add r8, r9, r8
	ldw r8, r8+0
	jalr r0, r8, 0
.LBB149_71:
	addi r4, r6, 1
	addi r5, r0, 1
	blt r11, r5, .LBB149_97
.LBB149_72:
	lui r6, %hi(fio_len)
	stw sp+32, r6
	ldw r9, r6+%lo(fio_len)
	lui r7, %hi(fio_col)
	ldw r10, r7+%lo(fio_col)
	ldbu r8, r23+%lo(fio_reading)
	jal r0, .LBB149_75
.LBB149_73:
	add r10, r11, r0
.LBB149_74:
	addi r11, lr, -1
	addi r4, r4, 1
	ble lr, r5, .LBB149_97
.LBB149_75:
	add lr, r11, r0
	add r11, r3, r4
	ldbu r11, r11+0
	beq r11, r0, .LBB149_97
.LBB149_76:
	addi r6, r0, 1022
	sgt r6, r10, r6
	or  r6, r8, r6
	andi r6, r6, 1
	bne r6, r0, .LBB149_74
.LBB149_77:
	bge r9, r10, .LBB149_80
.LBB149_78:
	addi r6, r9, 1
	add r9, r9, r1
	stb r9+0, r19
	add r9, r6, r0
	bne r10, r6, .LBB149_78
.LBB149_79:
	ldw r6, sp+32
	stw r6+%lo(fio_len), r10
	add r9, r10, r0
.LBB149_80:
	add r6, r10, r1
	stb r6+0, r11
	addi r11, r10, 1
	stw r7+%lo(fio_col), r11
	blt r10, r9, .LBB149_73
.LBB149_81:
	ldw r6, sp+32
	stw r6+%lo(fio_len), r11
	add r9, r11, r0
	jal r0, .LBB149_73
.LBB149_82:
	stw r14+%lo(fio_pos), r10
	jal r0, .LBB149_3
.LBB149_83:
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB149_109
.LBB149_84:
	ldw r4, r22+%lo(fio_revert)
	add r1, r0, r0
	blt r4, r0, .LBB149_110
.LBB149_85:
	ldbu r1, r13+%lo(fio_since_data)
	beq r1, r0, .LBB149_131
.LBB149_86:
	stb r13+%lo(fio_since_data), r0
	stw r14+%lo(fio_pos), r4
	add r1, r3, r4
	ldbu r1, r1+0
	beq r1, r0, .LBB149_109
.LBB149_87:
	ldbu r1, r23+%lo(fio_reading)
	addi r3, r0, 1
	beq r1, r3, .LBB149_15
.LBB149_88:
	jal r31, fio_flush_line
	jal r0, .LBB149_3
.LBB149_89:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	add r1, r3, r1
	ldbu r1, r1+0
	add r8, r0, r0
	addi r11, r0, 1
	addi r4, r1, -76
	addi r1, r0, 38
	bgtu r4, r1, .LBB149_108
.LBB149_90:
	addi r1, r0, -1
	slli r4, r4, 2
	lui r5, %hi(.LJTI149_2)
	addi r5, r5, %lo(.LJTI149_2)
	add r4, r5, r4
	ldw r4, r4+0
	add r7, r11, r0
	jalr r0, r4, 0
.LBB149_91:
	addi r1, r0, 1
.LBB149_92:
	addi r4, r6, 2
	stw r14+%lo(fio_pos), r4
	add r7, r0, r0
	add r8, r1, r0
.LBB149_93:
	ldw r1, r14+%lo(fio_pos)
	add r1, r3, r1
	ldb r1, r1+0
	addi r3, r1, -58
	add r1, r11, r0
	bltu r3, r10, .LBB149_95
.LBB149_94:
	stw sp+32, r7
	stw sp+28, r8
	jal r31, fio_number
	ldw r8, sp+28
	ldw r7, sp+32
.LBB149_95:
	ldbu r4, r23+%lo(fio_reading)
	mul r3, r1, r8
	addi r1, r1, -1
	bne r4, r11, .LBB149_105
.LBB149_96:
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
	jal r0, .LBB149_3
.LBB149_97:
	stw r14+%lo(fio_pos), r4
	jal r0, .LBB149_3
.LBB149_98:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	sub r1, r0, r11
	xor r3, r11, r1
	sub r4, r0, r5
	and r3, r3, r4
	xor r1, r1, r3
	lui r3, %hi(fio_scale)
	stw r3+%lo(fio_scale), r1
	jal r0, .LBB149_3
.LBB149_99:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	ldbu r3, r23+%lo(fio_reading)
	addi r1, r0, 1
	bne r3, r1, .LBB149_106
.LBB149_100:
	lui r1, %hi(fio_rpos)
	ldw r3, r1+%lo(fio_rpos)
	add r3, r3, r11
	stw r1+%lo(fio_rpos), r3
	jal r0, .LBB149_3
.LBB149_101:
	andi r1, r7, 222
	addi r5, r0, 68
	andi r7, r7, 255
	beq r1, r5, .LBB149_111
.LBB149_102:
	addi r1, r7, -65
	addi r5, r0, 57
	bgtu r1, r5, .LBB149_104
.LBB149_103:
	slli r1, r1, 2
	lui r5, %hi(.LJTI149_3)
	addi r5, r5, %lo(.LJTI149_3)
	add r1, r5, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB149_104:
	addi r1, r6, 1
	jal r0, .LBB149_2
.LBB149_105:
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
	jal r0, .LBB149_3
.LBB149_106:
	blt r11, r1, .LBB149_3
.LBB149_107:
	lui r1, %hi(fio_col)
	ldw r3, r1+%lo(fio_col)
	add r3, r3, r11
	stw r1+%lo(fio_col), r3
	jal r0, .LBB149_3
.LBB149_108:
	add r7, r11, r0
	jal r0, .LBB149_93
.LBB149_109:
	add r1, r0, r0
.LBB149_110:
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
.LBB149_111:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	addi r5, r0, 69
	add r19, r7, r0
	bne r4, r5, .LBB149_115
.LBB149_112:
	add r16, r10, r0
	add r1, r3, r1
	ldbu r3, r1+0
	jal r31, toupper
	beq r1, r12, .LBB149_114
.LBB149_113:
	ldw r1, r15+%lo(fio_fmt)
	ldw r3, r14+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB149_116
.LBB149_114:
	ldw r1, r15+%lo(fio_fmt)
	ldw r3, r14+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+0
	jal r31, toupper
	add r12, r1, r0
	ldw r1, r14+%lo(fio_pos)
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB149_117
.LBB149_115:
	add r12, r0, r0
	jal r0, .LBB149_118
.LBB149_116:
	add r12, r0, r0
.LBB149_117:
	add r10, r16, r0
	add r7, r19, r0
.LBB149_118:
	ldw r15, r15+%lo(fio_fmt)
	ldw r1, r14+%lo(fio_pos)
	add r1, r15, r1
	ldb r1, r1+0
	addi r1, r1, -58
	bgeu r1, r10, .LBB149_120
.LBB149_119:
	add r1, r0, r0
	jal r0, .LBB149_121
.LBB149_120:
	add r16, r10, r0
	jal r31, fio_number
	add r7, r19, r0
	add r10, r16, r0
.LBB149_121:
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
	bne r3, r4, .LBB149_123
.LBB149_122:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	add r18, r10, r0
	jal r31, fio_number
	add r7, r19, r0
	add r10, r18, r0
	stw r16+%lo(fio_d), r1
	addi r1, r0, 1
	stb r17+%lo(fio_dgiven), r1
.LBB149_123:
	lui r16, %hi(fio_e)
	stw r16+%lo(fio_e), r0
	ldw r1, r14+%lo(fio_pos)
	add r3, r15, r1
	ldbu r3, r3+0
	ori  r3, r3, 32
	addi r4, r0, 101
	bne r3, r4, .LBB149_127
.LBB149_124:
	addi r3, r1, 1
	stw r14+%lo(fio_pos), r3
	add r3, r15, r3
	ldb r3, r3+0
	addi r3, r3, -58
	bgeu r3, r10, .LBB149_126
.LBB149_125:
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB149_127
.LBB149_126:
	jal r31, fio_number
	add r7, r19, r0
	stw r16+%lo(fio_e), r1
.LBB149_127:
	lui r1, %hi(fio_rep)
	stw r1+%lo(fio_rep), r11
	beq r12, r0, .LBB149_129
.LBB149_128:
	addi r1, r12, 256
	jal r0, .LBB149_130
.LBB149_129:
	add r3, r7, r0
	jal r31, toupper
.LBB149_130:
	lui r3, %hi(fio_desc)
	stw r3+%lo(fio_desc), r1
	addi r3, r0, 1
	stb r13+%lo(fio_since_data), r3
	jal r0, .LBB149_110
.LBB149_131:
	jal r31, fio_flush_line
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.122)
	addi r4, r4, %lo(.L.str.122)
	jal r0, .LBB149_133
.LBB149_132:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
.LBB149_133:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end149:
	.size	fio_next_desc, .Lfunc_end149-fio_next_desc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI149_0,@object
.LJTI149_0:
	.word	.LBB149_31
	.word	.LBB149_59
	.word	.LBB149_54
	.word	.LBB149_59
	.word	.LBB149_59
	.word	.LBB149_59
	.word	.LBB149_51
	.word	.LBB149_59
	.word	.LBB149_57
	.size	.LJTI149_0, 36
	.type	.LJTI149_1,@object
.LJTI149_1:
	.word	.LBB149_71
	.word	.LBB149_101
	.word	.LBB149_101
	.word	.LBB149_101
	.word	.LBB149_101
	.word	.LBB149_101
	.word	.LBB149_101
	.word	.LBB149_101
	.word	.LBB149_98
	.word	.LBB149_101
	.word	.LBB149_101
	.word	.LBB149_101
	.word	.LBB149_89
	.word	.LBB149_101
	.word	.LBB149_101
	.word	.LBB149_101
	.word	.LBB149_99
	.size	.LJTI149_1, 68
	.type	.LJTI149_2,@object
.LJTI149_2:
	.word	.LBB149_92
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_91
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_92
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_93
	.word	.LBB149_91
	.size	.LJTI149_2, 156
	.type	.LJTI149_3,@object
.LJTI149_3:
	.word	.LBB149_111
	.word	.LBB149_111
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_111
	.word	.LBB149_111
	.word	.LBB149_104
	.word	.LBB149_111
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_111
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_111
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_111
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_111
	.word	.LBB149_111
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_111
	.word	.LBB149_111
	.word	.LBB149_104
	.word	.LBB149_111
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_111
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_111
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_104
	.word	.LBB149_111
	.size	.LJTI149_3, 232
	.type	.LJTI149_4,@object
.LJTI149_4:
	.word	.LBB149_19
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_27
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_19
	.word	.LBB149_50
	.word	.LBB149_50
	.word	.LBB149_27
	.size	.LJTI149_4, 144
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
	beq r3, r0, .LBB150_9
.LBB150_1:
	lui r11, %hi(fio_irec)
	ldw r1, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r1, r4, .LBB150_21
.LBB150_2:
	lui r14, %hi(fio_ilen)
	ldw r6, r14+%lo(fio_ilen)
	bgt r5, r6, .LBB150_22
.LBB150_3:
	mul r1, r6, r1
	add r3, r3, r1
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	jal r31, memcpy
	ldw r3, r13+%lo(fio_len)
	ldw r1, r14+%lo(fio_ilen)
	ble r1, r3, .LBB150_6
.LBB150_4:
	ldw r4, r12+%lo(fio_ibuf)
	ldw r5, r11+%lo(fio_irec)
	mul r5, r5, r1
	sub r1, r1, r3
	add r3, r3, r5
	add r3, r4, r3
	addi r4, r0, 32
.LBB150_5:
	stb r3+0, r4
	addi r1, r1, -1
	addi r3, r3, 1
	bne r1, r0, .LBB150_5
.LBB150_6:
	ldw r1, r11+%lo(fio_irec)
	addi r1, r1, 1
	stw r11+%lo(fio_irec), r1
.LBB150_7:
	stw r13+%lo(fio_len), r0
.LBB150_8:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB150_9:
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
	bgtu r3, r14, .LBB150_15
.LBB150_10:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB150_15
.LBB150_11:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r12, r1+0
	bgt r5, r12, .LBB150_24
.LBB150_12:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	ldw r1, r13+%lo(fio_len)
	ble r12, r1, .LBB150_7
.LBB150_13:
	sub r14, r12, r1
	addi r12, r0, 32
.LBB150_14:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fputc
	addi r14, r14, -1
	bne r14, r0, .LBB150_14
	jal r0, .LBB150_7
.LBB150_15:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	lui r1, %hi(fio_open_rec)
	ldw r3, r1+%lo(fio_open_rec)
	beq r3, r0, .LBB150_17
.LBB150_16:
	stw r1+%lo(fio_open_rec), r0
	jal r0, .LBB150_18
.LBB150_17:
	addi r3, r0, 10
	add r4, r11, r0
	jal r31, fputc
.LBB150_18:
	stw r13+%lo(fio_len), r0
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	bgtu r3, r14, .LBB150_8
.LBB150_19:
	addi r1, r1, -7
	addi r3, r0, -3
	bgtu r1, r3, .LBB150_8
.LBB150_20:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
	jal r0, .LBB150_8
.LBB150_21:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.123)
	addi r4, r4, %lo(.L.str.123)
	jal r0, .LBB150_23
.LBB150_22:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.124)
	addi r4, r4, %lo(.L.str.124)
.LBB150_23:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB150_24:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.125)
	addi r4, r4, %lo(.L.str.125)
	add r6, r12, r0
	jal r0, .LBB150_23
.Lfunc_end150:
	.size	fio_flush_line, .Lfunc_end150-fio_flush_line
                                        # -- End function
	.globl	f77_endfile                     # -- Begin function f77_endfile
	.p2align	2
	.type	f77_endfile,@function
f77_endfile:                            # @f77_endfile
# %bb.0:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	stw sp+28, r15
	add r11, r3, r0
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB151_4
.LBB151_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB151_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB151_2
.LBB151_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB151_4:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_resolve
	add r13, r1, r0
	lui r1, %hi(stdout)
	ldw r1, r1+%lo(stdout)
	beq r13, r1, .LBB151_11
.LBB151_5:
	lui r1, %hi(stderr)
	ldw r1, r1+%lo(stderr)
	beq r13, r1, .LBB151_11
.LBB151_6:
	addi r1, r11, -100
	addi r3, r0, -99
	slli r15, r11, 2
	bltu r1, r3, .LBB151_9
.LBB151_7:
	lui r1, %hi(fio_uwrote)
	addi r1, r1, %lo(fio_uwrote)
	add r1, r15, r1
	ldw r3, r1+0
	beq r3, r0, .LBB151_9
.LBB151_8:
	stw r1+0, r0
	add r3, r13, r0
	jal r31, fflush
	add r3, r13, r0
	jal r31, fileno
	add r14, r1, r0
	add r3, r13, r0
	jal r31, ftell
	add r3, r14, r0
	add r4, r1, r0
	jal r31, ftruncate
.LBB151_9:
	add r3, r13, r0
	jal r31, fflush
	addi r1, r11, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB151_11
.LBB151_10:
	add r3, r13, r0
	jal r31, ftell
	lui r3, %hi(fio_uend)
	addi r3, r3, %lo(fio_uend)
	add r3, r15, r3
	stw r3+0, r1
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r15, r1
	stw r1+0, r12
.LBB151_11:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end151:
	.size	f77_endfile, .Lfunc_end151-f77_endfile
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
	lui r4, %hi(.L.str.32)
	addi r4, r4, %lo(.L.str.32)
	add r5, r11, r0
	add r6, r12, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fprintf
.Lfunc_end152:
	.size	f77_pause, .Lfunc_end152-f77_pause
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
	beq r3, r1, .LBB153_19
.LBB153_1:
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	stb r15+%lo(fio_busy), r1
	lui r3, %hi(fio_uend_init)
	ldbu r4, r3+%lo(fio_uend_init)
	bne r4, r0, .LBB153_5
.LBB153_2:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
	addi r6, r0, -1
.LBB153_3:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r0, .LBB153_3
.LBB153_4:
	stb r3+%lo(fio_uend_init), r1
.LBB153_5:
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
	bgtu r3, r4, .LBB153_11
.LBB153_6:
	slli r16, r14, 2
	lui r3, %hi(fio_uunf)
	addi r3, r3, %lo(fio_uunf)
	add r3, r16, r3
	stw r3+0, r0
	lui r3, %hi(fio_uwrote)
	addi r3, r3, %lo(fio_uwrote)
	add r3, r16, r3
	stw r3+0, r0
	lui r4, %hi(fio_rec_next)
	ldw r3, r4+%lo(fio_rec_next)
	stw r4+%lo(fio_rec_next), r0
	addi r4, r14, -100
	addi r5, r0, -99
	bltu r4, r5, .LBB153_10
.LBB153_7:
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r16, r4
	ldw r4, r4+0
	beq r4, r0, .LBB153_10
.LBB153_8:
	ble r3, r0, .LBB153_20
.LBB153_9:
	addi r3, r3, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r4, r16, r4
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB153_10:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r16, r1
	ldw r1, r1+0
	jal r0, .LBB153_16
.LBB153_11:
	lui r4, %hi(fio_rec_next)
	ldw r3, r4+%lo(fio_rec_next)
	stw r4+%lo(fio_rec_next), r0
	addi r4, r14, -100
	addi r5, r0, -99
	bltu r4, r5, .LBB153_15
.LBB153_12:
	slli r4, r14, 2
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r4, r5
	ldw r5, r5+0
	beq r5, r0, .LBB153_15
.LBB153_13:
	ble r3, r0, .LBB153_20
.LBB153_14:
	addi r3, r3, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r4, r4, r5
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB153_15:
	add r1, r0, r0
.LBB153_16:
	lui r3, %hi(fio_bz)
	stw r3+%lo(fio_bz), r1
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, fio_rd_setup
	beq r1, r0, .LBB153_18
.LBB153_17:
	stb r15+%lo(fio_busy), r0
.LBB153_18:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB153_19:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	jal r0, .LBB153_21
.LBB153_20:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
.LBB153_21:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end153:
	.size	f77_rd_begin, .Lfunc_end153-f77_rd_begin
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
	bgt r1, r0, .LBB154_2
.LBB154_1:
	lui r1, %hi(fio_bz)
	stw r1+%lo(fio_bz), r0
.LBB154_2:
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
	beq r3, r0, .LBB154_4
.LBB154_3:
	jal r31, fio_fmt_use
	ble r1, r19, .LBB154_17
.LBB154_4:
	ldw r1, r18+%lo(fio_listed)
	bne r1, r0, .LBB154_10
.LBB154_5:
	ldw r3, r17+%lo(fio_fmt)
	ldw r1, r15+%lo(fio_pos)
	addi r4, r0, 32
.LBB154_6:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB154_8
.LBB154_7:
	addi r1, r1, 1
	stw r15+%lo(fio_pos), r1
	jal r0, .LBB154_6
.LBB154_8:
	addi r3, r0, 40
	bne r5, r3, .LBB154_10
.LBB154_9:
	addi r1, r1, 1
	stw r15+%lo(fio_pos), r1
	stw r16+%lo(fio_revert), r1
.LBB154_10:
	lui r1, %hi(fio_ibuf)
	ldw r1, r1+%lo(fio_ibuf)
	beq r1, r0, .LBB154_14
.LBB154_11:
	jal r31, fio_getrec
	beq r1, r0, .LBB154_16
.LBB154_12:
	stb r13+%lo(fio_reading), r0
	lui r1, %hi(fio_noadv)
	stb r1+%lo(fio_noadv), r0
	andi r1, r11, 3
	bne r1, r0, .LBB154_19
.LBB154_13:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB154_14:
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
	beq r1, r0, .LBB154_11
.LBB154_15:
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
.LBB154_16:
	add r12, r0, r0
	jal r0, .LBB154_19
.LBB154_17:
	stb r13+%lo(fio_reading), r0
	andi r1, r11, 2
	beq r1, r0, .LBB154_20
.LBB154_18:
	addi r12, r0, 2
.LBB154_19:
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
.LBB154_20:
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end154:
	.size	fio_rd_setup, .Lfunc_end154-fio_rd_setup
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
	beq r9, r1, .LBB155_8
.LBB155_1:
	stb r11+%lo(fio_busy), r1
	lui r9, %hi(fio_uend_init)
	ldbu r10, r9+%lo(fio_uend_init)
	bne r10, r0, .LBB155_5
.LBB155_2:
	addi r10, r0, -400
	lui lr, %hi(fio_uend+400)
	addi lr, lr, %lo(fio_uend+400)
	addi r12, r0, -1
.LBB155_3:
	add r13, r10, lr
	stw r13+0, r12
	addi r10, r10, 4
	bne r10, r0, .LBB155_3
.LBB155_4:
	stb r9+%lo(fio_uend_init), r1
.LBB155_5:
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
	beq r1, r0, .LBB155_7
.LBB155_6:
	stb r11+%lo(fio_busy), r0
.LBB155_7:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB155_8:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end155:
	.size	f77_rd_begin_int, .Lfunc_end155-f77_rd_begin_int
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
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB156_5
.LBB156_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB156_73
.LBB156_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB156_73
.LBB156_3:
	addi r3, r0, 1
	bne r1, r3, .LBB156_24
.LBB156_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB156_73
.LBB156_5:
	lui r14, %hi(fio_eor)
	ldbu r3, r14+%lo(fio_eor)
	lui r13, %hi(fio_rep)
	add r1, r0, r0
	bne r3, r0, .LBB156_9
.LBB156_6:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB156_8
.LBB156_7:
	ldw r4, r13+%lo(fio_rep)
	bgt r4, r0, .LBB156_9
.LBB156_8:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
.LBB156_9:
	addi r3, r1, -65
	addi r4, r0, 25
	bgtu r3, r4, .LBB156_27
.LBB156_10:
	slli r3, r3, 2
	lui r4, %hi(.LJTI156_1)
	addi r4, r4, %lo(.LJTI156_1)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB156_11:
	lui r3, %hi(fio_w)
	ldw r3, r3+%lo(fio_w)
	addi r4, r0, 1
	blt r3, r4, .LBB156_29
.LBB156_12:
	lui r5, %hi(fio_rpos)
	ldw r5, r5+%lo(fio_rpos)
	lui r6, %hi(fio_rlen)
	ldw r6, r6+%lo(fio_rlen)
	lui r7, %hi(fio_bz)
	ldw r7, r7+%lo(fio_bz)
	addi r8, r0, 32
	lui r9, %hi(fio_rec)
	addi r9, r9, %lo(fio_rec)
	addi r10, r0, 48
	addi lr, r0, 1023
	addi r12, r0, 9
	addi r15, r0, 1022
	add r17, r3, r0
	add r18, r0, r0
	add r16, r0, r0
	jal r0, .LBB156_16
.LBB156_13:
	addi r18, r16, 1
	addi r20, sp, 32
	add r16, r20, r16
	stb r16+0, r19
	add r16, r18, r0
.LBB156_14:
	add r18, r4, r0
.LBB156_15:
	addi r17, r17, -1
	addi r5, r5, 1
	beq r17, r0, .LBB156_30
.LBB156_16:
	add r19, r8, r0
	bge r5, r6, .LBB156_18
.LBB156_17:
	add r19, r5, r9
	ldb r19, r19+0
.LBB156_18:
	beq r19, r8, .LBB156_20
.LBB156_19:
	bne r19, r12, .LBB156_23
.LBB156_20:
	beq r7, r0, .LBB156_15
.LBB156_21:
	beq r18, r0, .LBB156_15
.LBB156_22:
	add r19, r10, r0
	blt r16, lr, .LBB156_13
	jal r0, .LBB156_15
.LBB156_23:
	bgt r16, r15, .LBB156_14
	jal r0, .LBB156_13
.LBB156_24:
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
	bgtu r4, r5, .LBB156_74
.LBB156_25:
	slli r4, r4, 2
	lui r5, %hi(.LJTI156_0)
	addi r5, r5, %lo(.LJTI156_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB156_26:
	andi r3, r3, 255
	addi r4, r0, 46
	seq r3, r3, r4
	add r1, r3, r1
	ldbu r1, r1+0
	andi r1, r1, 223
	addi r3, r0, 84
	seq r1, r1, r3
	stw r11+0, r1
	jal r0, .LBB156_73
.LBB156_27:
	beq r1, r0, .LBB156_73
.LBB156_28:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.w.d r12, r4
	jal r0, .LBB156_69
.LBB156_29:
	add r16, r0, r0
.LBB156_30:
	addi r5, sp, 32
	add r5, r5, r16
	stb r5+0, r0
	lui r5, %hi(fio_noadv)
	ldbu r5, r5+%lo(fio_noadv)
	bne r5, r4, .LBB156_34
.LBB156_31:
	lui r5, %hi(fio_reading)
	ldbu r5, r5+%lo(fio_reading)
	andi r5, r5, 1
	beq r5, r0, .LBB156_34
.LBB156_32:
	lui r5, %hi(fio_rlen)
	ldw r5, r5+%lo(fio_rlen)
	lui r6, %hi(fio_rpos)
	ldw r6, r6+%lo(fio_rpos)
	sub r5, r5, r6
	addi r6, r0, 0
	sgt r6, r5, r6
	sub r6, r0, r6
	and r5, r5, r6
	xor r6, r3, r5
	slt r7, r3, r5
	sub r7, r0, r7
	and r6, r6, r7
	xor r6, r5, r6
	lui r7, %hi(fio_size)
	ldw r8, r7+%lo(fio_size)
	add r6, r6, r8
	stw r7+%lo(fio_size), r6
	ble r3, r5, .LBB156_34
.LBB156_33:
	stb r14+%lo(fio_eor), r4
.LBB156_34:
	lui r4, %hi(fio_rpos)
	ldw r5, r4+%lo(fio_rpos)
	add r3, r5, r3
	stw r4+%lo(fio_rpos), r3
	ldbu r3, sp+32
	beq r3, r0, .LBB156_68
.LBB156_35:
	addi r4, r0, 79
	seq r4, r1, r4
	sub r4, r0, r4
	addi r5, r0, 66
	andi r6, r4, 24
	seq r1, r1, r5
	xori r5, r6, 16
	sub r1, r0, r1
	ori  r6, r5, 2
	and r6, r6, r1
	xor r14, r5, r6
	andi r4, r4, 7
	xori r5, r4, 4
	xori r4, r4, 5
	and r1, r4, r1
	xor r16, r5, r1
	addi r1, sp, 32
	addi r15, r1, 1
	addi r17, r0, 32
	addi r18, r0, 10
	addi r19, r0, 6
	add r12, r0, r0
	jal r0, .LBB156_38
.LBB156_36:
	sll r3, r12, r16
	add r12, r1, r3
.LBB156_37:
	ldbu r3, r15+0
	addi r15, r15, 1
	beq r3, r0, .LBB156_69
.LBB156_38:
	andi r3, r3, 255
	jal r31, toupper
	beq r1, r17, .LBB156_37
.LBB156_39:
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
	blt r1, r14, .LBB156_36
.LBB156_40:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	ldb r5, r15+-1
	lui r4, %hi(.L.str.128)
	addi r4, r4, %lo(.L.str.128)
.LBB156_41:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB156_42:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r12, r0, 1
	lui r3, %hi(fio_rpos)
	lui r4, %hi(fio_rlen)
	blt r1, r12, .LBB156_57
.LBB156_43:
	ldw r5, r3+%lo(fio_rpos)
	ldw r6, r4+%lo(fio_rlen)
	lui r7, %hi(fio_bz)
	ldw r7, r7+%lo(fio_bz)
	addi r8, r0, 32
	lui r9, %hi(fio_rec)
	addi r9, r9, %lo(fio_rec)
	addi r10, r0, 48
	addi lr, r0, 1023
	addi r15, r0, 1
	addi r16, r0, 9
	addi r17, r0, 1022
	add r19, r1, r0
	add r20, r0, r0
	add r18, r0, r0
	jal r0, .LBB156_47
.LBB156_44:
	addi r20, r18, 1
	addi r22, sp, 32
	add r18, r22, r18
	stb r18+0, r21
	add r18, r20, r0
.LBB156_45:
	add r20, r15, r0
.LBB156_46:
	addi r19, r19, -1
	addi r5, r5, 1
	beq r19, r0, .LBB156_58
.LBB156_47:
	add r21, r8, r0
	bge r5, r6, .LBB156_49
.LBB156_48:
	add r21, r5, r9
	ldb r21, r21+0
.LBB156_49:
	beq r21, r8, .LBB156_51
.LBB156_50:
	bne r21, r16, .LBB156_54
.LBB156_51:
	beq r7, r0, .LBB156_46
.LBB156_52:
	beq r20, r0, .LBB156_46
.LBB156_53:
	add r21, r10, r0
	blt r18, lr, .LBB156_44
	jal r0, .LBB156_46
.LBB156_54:
	bgt r18, r17, .LBB156_45
	jal r0, .LBB156_44
.LBB156_55:
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
.LBB156_56:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	jal r31, fio_in_int
	add r12, r1, r0
	jal r0, .LBB156_69
.LBB156_57:
	add r18, r0, r0
.LBB156_58:
	addi r5, sp, 32
	add r5, r5, r18
	stb r5+0, r0
	lui r5, %hi(fio_noadv)
	ldbu r5, r5+%lo(fio_noadv)
	bne r5, r12, .LBB156_62
.LBB156_59:
	lui r5, %hi(fio_reading)
	ldbu r5, r5+%lo(fio_reading)
	andi r5, r5, 1
	beq r5, r0, .LBB156_62
.LBB156_60:
	ldw r4, r4+%lo(fio_rlen)
	ldw r5, r3+%lo(fio_rpos)
	sub r4, r4, r5
	addi r5, r0, 0
	sgt r5, r4, r5
	sub r5, r0, r5
	and r4, r4, r5
	xor r5, r1, r4
	slt r6, r1, r4
	sub r6, r0, r6
	and r5, r5, r6
	xor r5, r4, r5
	lui r6, %hi(fio_size)
	ldw r7, r6+%lo(fio_size)
	add r5, r5, r7
	stw r6+%lo(fio_size), r5
	ble r1, r4, .LBB156_62
.LBB156_61:
	addi r4, r0, 1
	stb r14+%lo(fio_eor), r4
.LBB156_62:
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	ldbu r1, sp+32
	addi r3, r0, 46
	seq r1, r1, r3
	addi r3, sp, 32
	or  r1, r3, r1
	ldbu r1, r1+0
	addi r3, r0, 83
	ble r1, r3, .LBB156_66
.LBB156_63:
	addi r3, r0, 84
	beq r1, r3, .LBB156_69
.LBB156_64:
	addi r3, r0, 116
	beq r1, r3, .LBB156_69
.LBB156_65:
	addi r3, r0, 102
	beq r1, r3, .LBB156_68
	jal r0, .LBB156_75
.LBB156_66:
	beq r1, r0, .LBB156_68
.LBB156_67:
	addi r3, r0, 70
	bne r1, r3, .LBB156_75
.LBB156_68:
	add r12, r0, r0
.LBB156_69:
	stw r11+0, r12
	ldw r1, r13+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB156_71
.LBB156_70:
	addi r1, r1, -1
	stw r13+%lo(fio_rep), r1
.LBB156_71:
	ldw r1, r13+%lo(fio_rep)
	bne r1, r0, .LBB156_73
.LBB156_72:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB156_73:
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
.LBB156_74:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	stw r11+0, r1
	jal r0, .LBB156_73
.LBB156_75:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.127)
	addi r4, r4, %lo(.L.str.127)
	addi r5, sp, 32
	jal r0, .LBB156_41
.Lfunc_end156:
	.size	f77_rd_i, .Lfunc_end156-f77_rd_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI156_0,@object
.LJTI156_0:
	.word	.LBB156_26
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_26
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_26
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_26
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_74
	.word	.LBB156_26
	.size	.LJTI156_0, 144
	.type	.LJTI156_1,@object
.LJTI156_1:
	.word	.LBB156_55
	.word	.LBB156_11
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_56
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_42
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_11
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_28
	.word	.LBB156_11
	.size	.LJTI156_1, 104
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
	blt r1, r20, .LBB157_2
.LBB157_1:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
	addi r11, r0, 2
	jal r0, .LBB157_5
.LBB157_2:
	lui r14, %hi(fio_l_pending)
	ldw r1, r14+%lo(fio_l_pending)
	ble r1, r0, .LBB157_6
.LBB157_3:
	addi r1, r1, -1
	stw r14+%lo(fio_l_pending), r1
.LBB157_4:
	add r11, r0, r0
.LBB157_5:
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
.LBB157_6:
	lui r15, %hi(fio_rpos)
	lui r17, %hi(fio_rlen)
	lui r16, %hi(fio_rec)
	addi r16, r16, %lo(fio_rec)
	addi r18, r0, 38
	lui r21, %hi(.LJTI157_0)
	addi r21, r21, %lo(.LJTI157_0)
	lui r12, %hi(fio_l_after)
	addi r11, r0, 2
	jal r0, .LBB157_8
.LBB157_7:
	jal r31, fio_getrec
	bne r1, r0, .LBB157_44
.LBB157_8:
	ldw r3, r15+%lo(fio_rpos)
	ldw r1, r17+%lo(fio_rlen)
	bge r3, r1, .LBB157_7
.LBB157_9:
	add r4, r3, r16
	ldbu r19, r4+0
	addi r4, r19, -9
	bgtu r4, r18, .LBB157_25
.LBB157_10:
	slli r4, r4, 2
	add r4, r21, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB157_11:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	jal r0, .LBB157_8
.LBB157_12:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	ldbu r1, r12+%lo(fio_l_after)
	bne r1, r20, .LBB157_5
.LBB157_13:
	stb r12+%lo(fio_l_after), r0
	jal r0, .LBB157_8
.LBB157_14:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	addi r13, r0, 126
	lui r11, %hi(fio_ltok)
	addi r11, r11, %lo(fio_ltok)
	add r14, r0, r0
	jal r0, .LBB157_16
.LBB157_15:
	jal r31, fio_getrec
	bne r1, r0, .LBB157_44
.LBB157_16:
	ldw r1, r15+%lo(fio_rpos)
	ldw r4, r17+%lo(fio_rlen)
	bge r1, r4, .LBB157_15
.LBB157_17:
	addi r5, r1, 1
	stw r15+%lo(fio_rpos), r5
	add r3, r1, r16
	ldbu r3, r3+0
	bne r3, r19, .LBB157_21
.LBB157_18:
	bge r5, r4, .LBB157_23
.LBB157_19:
	add r4, r5, r16
	ldbu r4, r4+0
	bne r4, r19, .LBB157_23
.LBB157_20:
	addi r1, r1, 2
	stw r15+%lo(fio_rpos), r1
.LBB157_21:
	bgtu r14, r13, .LBB157_16
.LBB157_22:
	addi r1, r14, 1
	add r4, r14, r11
	stb r4+0, r3
	add r14, r1, r0
	jal r0, .LBB157_16
.LBB157_23:
	add r1, r14, r11
	stb r1+0, r0
	addi r1, r0, 1
	stb r12+%lo(fio_l_after), r1
	jal r0, .LBB157_4
.LBB157_24:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	add r11, r20, r0
	jal r0, .LBB157_5
.LBB157_25:
	ldw r3, r15+%lo(fio_rpos)
	bge r3, r1, .LBB157_32
.LBB157_26:
	addi r5, r0, 126
	lui r6, %hi(fio_ltok)
	addi r6, r6, %lo(fio_ltok)
	lui r7, %hi(.LJTI157_1)
	addi r7, r7, %lo(.LJTI157_1)
	add r4, r0, r0
	jal r0, .LBB157_28
.LBB157_27:
	addi r3, r3, 1
	stw r15+%lo(fio_rpos), r3
	beq r1, r3, .LBB157_33
.LBB157_28:
	add r8, r3, r16
	ldbu r8, r8+0
	addi r9, r8, -9
	bgtu r9, r18, .LBB157_30
.LBB157_29:
	slli r9, r9, 2
	add r9, r7, r9
	ldw r9, r9+0
	jalr r0, r9, 0
.LBB157_30:
	bgt r4, r5, .LBB157_27
.LBB157_31:
	addi r9, r4, 1
	add r4, r4, r6
	stb r4+0, r8
	add r4, r9, r0
	jal r0, .LBB157_27
.LBB157_32:
	add r4, r0, r0
.LBB157_33:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	add r3, r4, r1
	stb r3+0, r0
	addi r3, r0, 1
	stb r12+%lo(fio_l_after), r3
	addi r4, r0, -11
	add r15, r0, r0
.LBB157_34:
	add r5, r15, r1
	ldb r5, r5+0
	addi r6, r5, -58
	addi r15, r15, 1
	bgtu r6, r4, .LBB157_34
.LBB157_35:
	beq r15, r3, .LBB157_4
.LBB157_36:
	addi r1, r0, 42
	bne r5, r1, .LBB157_4
.LBB157_37:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	addi r5, r0, 10
	add r3, r12, r0
	add r4, r0, r0
	jal r31, strtol
	beq r1, r0, .LBB157_46
.LBB157_38:
	add r12, r15, r12
	ldbu r15, r12+0
	beq r15, r0, .LBB157_42
.LBB157_39:
	addi r3, r0, 2
	blt r1, r3, .LBB157_41
.LBB157_40:
	addi r1, r1, -1
	stw r14+%lo(fio_l_pending), r1
.LBB157_41:
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	add r4, r12, r0
	jal r31, memmove
	jal r0, .LBB157_43
.LBB157_42:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
.LBB157_43:
	bne r15, r0, .LBB157_4
	jal r0, .LBB157_5
.LBB157_44:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
.LBB157_45:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB157_46:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.126)
	addi r4, r4, %lo(.L.str.126)
	jal r0, .LBB157_45
.Lfunc_end157:
	.size	fio_list_tok, .Lfunc_end157-fio_list_tok
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI157_0,@object
.LJTI157_0:
	.word	.LBB157_11
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_11
	.word	.LBB157_25
	.word	.LBB157_14
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_14
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_12
	.word	.LBB157_25
	.word	.LBB157_25
	.word	.LBB157_24
	.size	.LJTI157_0, 156
	.type	.LJTI157_1,@object
.LJTI157_1:
	.word	.LBB157_33
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_33
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_33
	.word	.LBB157_30
	.word	.LBB157_30
	.word	.LBB157_33
	.size	.LJTI157_1, 156
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function fio_in_int
	.type	fio_in_int,@function
fio_in_int:                             # @fio_in_int
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
	addi r4, r0, 1
	lui r1, %hi(fio_rpos)
	lui r5, %hi(fio_rlen)
	blt r3, r4, .LBB158_13
.LBB158_1:
	ldw r6, r1+%lo(fio_rpos)
	ldw r7, r5+%lo(fio_rlen)
	lui r8, %hi(fio_bz)
	ldw r8, r8+%lo(fio_bz)
	addi r9, r0, 32
	lui r10, %hi(fio_rec)
	addi r10, r10, %lo(fio_rec)
	addi lr, r0, 48
	addi r11, r0, 1023
	addi r12, r0, 9
	addi r13, r0, 1022
	add r15, r3, r0
	add r16, r0, r0
	add r14, r0, r0
	jal r0, .LBB158_5
.LBB158_2:
	addi r16, r14, 1
	addi r18, sp, 32
	add r14, r18, r14
	stb r14+0, r17
	add r14, r16, r0
.LBB158_3:
	add r16, r4, r0
.LBB158_4:
	addi r15, r15, -1
	addi r6, r6, 1
	beq r15, r0, .LBB158_14
.LBB158_5:
	add r17, r9, r0
	bge r6, r7, .LBB158_7
.LBB158_6:
	add r17, r6, r10
	ldb r17, r17+0
.LBB158_7:
	beq r17, r9, .LBB158_9
.LBB158_8:
	bne r17, r12, .LBB158_12
.LBB158_9:
	beq r8, r0, .LBB158_4
.LBB158_10:
	beq r16, r0, .LBB158_4
.LBB158_11:
	add r17, lr, r0
	blt r14, r11, .LBB158_2
	jal r0, .LBB158_4
.LBB158_12:
	ble r14, r13, .LBB158_2
	jal r0, .LBB158_3
.LBB158_13:
	add r14, r0, r0
.LBB158_14:
	addi r6, sp, 32
	add r6, r6, r14
	stb r6+0, r0
	lui r6, %hi(fio_noadv)
	ldbu r6, r6+%lo(fio_noadv)
	bne r6, r4, .LBB158_18
.LBB158_15:
	lui r6, %hi(fio_reading)
	ldbu r6, r6+%lo(fio_reading)
	andi r6, r6, 1
	beq r6, r0, .LBB158_18
.LBB158_16:
	ldw r5, r5+%lo(fio_rlen)
	ldw r6, r1+%lo(fio_rpos)
	sub r5, r5, r6
	addi r6, r0, 0
	sgt r6, r5, r6
	sub r6, r0, r6
	and r5, r5, r6
	xor r6, r3, r5
	slt r7, r3, r5
	sub r7, r0, r7
	and r6, r6, r7
	xor r6, r5, r6
	lui r7, %hi(fio_size)
	ldw r8, r7+%lo(fio_size)
	add r6, r6, r8
	stw r7+%lo(fio_size), r6
	ble r3, r5, .LBB158_18
.LBB158_17:
	lui r5, %hi(fio_eor)
	stb r5+%lo(fio_eor), r4
.LBB158_18:
	ldw r4, r1+%lo(fio_rpos)
	add r3, r4, r3
	stw r1+%lo(fio_rpos), r3
	ldbu r1, sp+32
	beq r1, r0, .LBB158_20
.LBB158_19:
	addi r3, sp, 32
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	jal r0, .LBB158_21
.LBB158_20:
	add r1, r0, r0
.LBB158_21:
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
.Lfunc_end158:
	.size	fio_in_int, .Lfunc_end158-fio_in_int
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
	beq r1, r0, .LBB159_5
.LBB159_1:
	lui r13, %hi(fio_ldone)
	ldbu r1, r13+%lo(fio_ldone)
	bne r1, r0, .LBB159_36
.LBB159_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB159_36
.LBB159_3:
	addi r14, r0, 1
	bne r1, r14, .LBB159_17
.LBB159_4:
	stb r13+%lo(fio_ldone), r14
	jal r0, .LBB159_36
.LBB159_5:
	lui r13, %hi(fio_eor)
	ldbu r3, r13+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB159_8
.LBB159_6:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB159_22
.LBB159_7:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB159_22
.LBB159_8:
	beq r1, r0, .LBB159_36
.LBB159_9:
	addi r3, r0, 65
	bne r1, r3, .LBB159_37
.LBB159_10:
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
	bge r1, r12, .LBB159_23
.LBB159_11:
	blt r12, r5, .LBB159_28
.LBB159_12:
	ldw r5, r3+%lo(fio_rpos)
	lui r6, %hi(fio_rlen)
	ldw r6, r6+%lo(fio_rlen)
	addi r7, r0, 32
	lui r8, %hi(fio_rec)
	addi r8, r8, %lo(fio_rec)
	add r9, r0, r0
	jal r0, .LBB159_14
.LBB159_13:
	add lr, r11, r9
	stb lr+0, r10
	addi r9, r9, 1
	beq r12, r9, .LBB159_28
.LBB159_14:
	add r10, r7, r0
	bge r9, r1, .LBB159_13
.LBB159_15:
	add lr, r5, r9
	add r10, r7, r0
	bge lr, r6, .LBB159_13
.LBB159_16:
	add r10, lr, r8
	ldbu r10, r10+0
	jal r0, .LBB159_13
.LBB159_17:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB159_36
.LBB159_18:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB159_20
.LBB159_19:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB159_36
.LBB159_20:
	add r5, r3, r0
	bge r4, r1, .LBB159_19
.LBB159_21:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB159_19
.LBB159_22:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	bne r1, r0, .LBB159_9
	jal r0, .LBB159_36
.LBB159_23:
	blt r12, r5, .LBB159_28
.LBB159_24:
	ldw r6, r3+%lo(fio_rpos)
	lui r5, %hi(fio_rlen)
	ldw r5, r5+%lo(fio_rlen)
	sub r9, r0, r12
	add r6, r1, r6
	addi r7, r0, 32
	lui r8, %hi(fio_rec)
	addi r8, r8, %lo(fio_rec)
	jal r0, .LBB159_26
.LBB159_25:
	stb r11+0, r10
	addi r10, r9, 1
	sltu lr, r10, r9
	addi r11, r11, 1
	add r9, r10, r0
	bne lr, r0, .LBB159_28
.LBB159_26:
	add lr, r6, r9
	add r10, r7, r0
	bge lr, r5, .LBB159_25
.LBB159_27:
	add r10, lr, r8
	ldbu r10, r10+0
	jal r0, .LBB159_25
.LBB159_28:
	lui r5, %hi(fio_noadv)
	ldbu r6, r5+%lo(fio_noadv)
	addi r5, r0, 1
	bne r6, r5, .LBB159_32
.LBB159_29:
	lui r6, %hi(fio_reading)
	ldbu r6, r6+%lo(fio_reading)
	andi r6, r6, 1
	beq r6, r0, .LBB159_32
.LBB159_30:
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
	ble r1, r4, .LBB159_32
.LBB159_31:
	stb r13+%lo(fio_eor), r5
.LBB159_32:
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	blt r3, r5, .LBB159_34
.LBB159_33:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB159_34:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB159_36
.LBB159_35:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB159_36:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB159_37:
	lui r3, %hi(stderr)
	ldw r3, r3+%lo(stderr)
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end159:
	.size	f77_rd_s, .Lfunc_end159-f77_rd_s
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_in_real
	.type	.LCPI160_0,@object
.LCPI160_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI160_0, 8
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
	add r1, sp, r0
	addi r1, r1, 2047
	stw r1+1061, r21
	addi r1, sp, 20
	addi r5, r0, 1
	lui r6, %hi(fio_rpos)
	lui r7, %hi(fio_rlen)
	blt r3, r5, .LBB160_13
.LBB160_1:
	ldw r8, r6+%lo(fio_rpos)
	ldw r9, r7+%lo(fio_rlen)
	lui r10, %hi(fio_bz)
	ldw r10, r10+%lo(fio_bz)
	addi lr, r0, 32
	lui r11, %hi(fio_rec)
	addi r11, r11, %lo(fio_rec)
	addi r12, r0, 48
	addi r13, r0, 1023
	addi r14, r0, 1
	addi r15, r0, 9
	addi r16, r0, 1022
	add r18, r3, r0
	add r19, r0, r0
	add r17, r0, r0
	jal r0, .LBB160_5
.LBB160_2:
	addi r19, r17, 1
	stw sp+16, r1
	add r1, sp, r0
	addi r1, r1, 2047
	addi r21, r1, 37
	ldw r1, sp+16
	add r17, r21, r17
	stb r17+0, r20
	add r17, r19, r0
.LBB160_3:
	add r19, r14, r0
.LBB160_4:
	addi r18, r18, -1
	addi r8, r8, 1
	beq r18, r0, .LBB160_14
.LBB160_5:
	add r20, lr, r0
	bge r8, r9, .LBB160_7
.LBB160_6:
	add r20, r8, r11
	ldb r20, r20+0
.LBB160_7:
	beq r20, lr, .LBB160_9
.LBB160_8:
	bne r20, r15, .LBB160_12
.LBB160_9:
	beq r10, r0, .LBB160_4
.LBB160_10:
	beq r19, r0, .LBB160_4
.LBB160_11:
	add r20, r12, r0
	blt r17, r13, .LBB160_2
	jal r0, .LBB160_4
.LBB160_12:
	bgt r17, r16, .LBB160_3
	jal r0, .LBB160_2
.LBB160_13:
	add r17, r0, r0
.LBB160_14:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r8, r9, 37
	add r8, r8, r17
	stb r8+0, r0
	lui r8, %hi(fio_noadv)
	ldbu r8, r8+%lo(fio_noadv)
	bne r8, r5, .LBB160_18
.LBB160_15:
	lui r8, %hi(fio_reading)
	ldbu r8, r8+%lo(fio_reading)
	andi r8, r8, 1
	beq r8, r0, .LBB160_18
.LBB160_16:
	ldw r7, r7+%lo(fio_rlen)
	ldw r8, r6+%lo(fio_rpos)
	sub r7, r7, r8
	addi r8, r0, 0
	sgt r8, r7, r8
	sub r8, r0, r8
	and r7, r7, r8
	xor r8, r3, r7
	slt r9, r3, r7
	sub r9, r0, r9
	and r8, r8, r9
	xor r8, r7, r8
	lui r9, %hi(fio_size)
	ldw r10, r9+%lo(fio_size)
	add r8, r8, r10
	stw r9+%lo(fio_size), r8
	ble r3, r7, .LBB160_18
.LBB160_17:
	lui r7, %hi(fio_eor)
	addi r8, r0, 1
	stb r7+%lo(fio_eor), r8
.LBB160_18:
	ldw r7, r6+%lo(fio_rpos)
	add r3, r7, r3
	stw r6+%lo(fio_rpos), r3
	add r6, sp, r0
	addi r6, r6, 2047
	ldbu r3, r6+37
	lui r8, %hi(.LCPI160_0)
	addi r8, r8, %lo(.LCPI160_0)
	ldw r7, r8+4
	ldw r6, r8+0
	beq r3, r0, .LBB160_71
.LBB160_19:
	addi lr, r0, 43
	beq r3, lr, .LBB160_22
.LBB160_20:
	addi r8, r0, 45
	bne r3, r8, .LBB160_23
.LBB160_21:
	addi r10, r0, 1
	add r8, r0, r0
	jal r0, .LBB160_25
.LBB160_22:
	add r10, r5, r0
	jal r0, .LBB160_24
.LBB160_23:
	add r10, r0, r0
.LBB160_24:
	add r8, r5, r0
.LBB160_25:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r3, r9, 37
	or  r3, r3, r10
	ldbu r19, r3+0
	beq r19, r0, .LBB160_46
.LBB160_26:
	addi r13, r0, 1
	addi r14, r0, 46
	addi r11, r0, 0
	addi r15, r0, -10
	addi r16, r0, 70
	lui r17, %hi(.LJTI160_0)
	addi r17, r17, %lo(.LJTI160_0)
	add r9, r13, r0
	add r12, r0, r0
	add r18, r0, r0
	add r3, r0, r0
.LBB160_27:
	andi r5, r19, 255
	bne r5, r14, .LBB160_29
.LBB160_28:
	add r18, r13, r0
	jal r0, .LBB160_38
.LBB160_29:
	slli r20, r19, 24
	srai r20, r20, 24
	addi r20, r20, -58
	bgeu r20, r15, .LBB160_35
.LBB160_30:
	addi r5, r5, -43
	bgtu r5, r16, .LBB160_44
.LBB160_31:
	slli r5, r5, 2
	add r5, r17, r5
	ldw r20, r5+0
	add r5, r0, r0
	add r19, r13, r0
	jalr r0, r20, 0
.LBB160_32:
	addi r5, r10, 1
	add r20, sp, r0
	addi r20, r20, 2047
	addi r19, r20, 37
	add r19, r19, r5
	ldbu r19, r19+0
	beq r19, lr, .LBB160_39
.LBB160_33:
	addi r20, r0, 45
	bne r19, r20, .LBB160_41
.LBB160_34:
	addi r9, r0, -1
	add r10, r5, r0
	jal r0, .LBB160_41
.LBB160_35:
	addi r5, r0, 1022
	bgt r3, r5, .LBB160_37
.LBB160_36:
	addi r5, r3, 1
	addi r20, sp, 1060
	add r3, r20, r3
	stb r3+0, r19
	add r3, r5, r0
.LBB160_37:
	sne r5, r18, r11
	add r12, r12, r5
.LBB160_38:
	add r5, r13, r0
	add r19, r13, r0
	bne r19, r0, .LBB160_43
	jal r0, .LBB160_45
.LBB160_39:
	add r10, r5, r0
	jal r0, .LBB160_41
.LBB160_40:
	addi r9, r0, -1
.LBB160_41:
	add r5, r0, r0
	add r19, r13, r0
.LBB160_42:
	beq r19, r0, .LBB160_45
.LBB160_43:
	addi r10, r10, 1
	add r20, sp, r0
	addi r20, r20, 2047
	addi r19, r20, 37
	add r19, r19, r10
	ldbu r19, r19+0
	sne r20, r19, r11
	and r20, r20, r5
	bne r20, r0, .LBB160_27
	jal r0, .LBB160_45
.LBB160_44:
	add r5, r13, r0
	add r19, r0, r0
	bne r19, r0, .LBB160_43
.LBB160_45:
	seq lr, r18, r11
	xor r4, r4, r12
	sub lr, r0, lr
	and r4, r4, lr
	xor r4, r12, r4
	beq r5, r0, .LBB160_47
	jal r0, .LBB160_51
.LBB160_46:
	add r3, r0, r0
	add r9, r5, r0
	bne r5, r0, .LBB160_51
.LBB160_47:
	add r11, sp, r0
	addi r11, r11, 2047
	addi lr, r11, 37
	add lr, lr, r10
	ldbu r12, lr+0
	beq r12, r0, .LBB160_51
.LBB160_48:
	add r11, sp, r0
	addi r11, r11, 2047
	addi lr, r11, 37
	add r10, r10, lr
	addi r10, r10, 1
	addi lr, r0, 9
	addi r11, r0, 10
	add r13, r0, r0
.LBB160_49:
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
	bne r12, r0, .LBB160_49
.LBB160_50:
	mul r10, r13, r9
	jal r0, .LBB160_52
.LBB160_51:
	add r10, r0, r0
.LBB160_52:
	addi r9, sp, 1060
	add r9, r9, r3
	stb r9+0, r0
	beq r3, r0, .LBB160_71
.LBB160_53:
	sub r9, r3, r4
	lui r6, %hi(fio_scale)
	ldw r6, r6+%lo(fio_scale)
	sub r5, r0, r5
	and r6, r6, r5
	addi lr, sp, 20
	add r5, r0, r0
	bne r8, r0, .LBB160_55
.LBB160_54:
	addi lr, r1, 1
	addi r1, r0, 45
	stb sp+20, r1
	addi r5, r0, 1
.LBB160_55:
	sub r6, r10, r6
	addi r7, r0, 1
	blt r9, r7, .LBB160_60
.LBB160_56:
	blt r3, r7, .LBB160_66
.LBB160_57:
	xor r1, r9, r3
	slt r4, r9, r3
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r3, r1
	addi r4, sp, 20
	or  r7, r4, r5
	add r4, r0, r0
.LBB160_58:
	addi r8, sp, 1060
	add r8, r8, r4
	ldbu r8, r8+0
	add r9, r7, r4
	stb r9+0, r8
	addi r4, r4, 1
	bne r1, r4, .LBB160_58
.LBB160_59:
	add r5, r5, r4
	jal r0, .LBB160_67
.LBB160_60:
	addi r1, sp, 20
	or  r1, r1, r5
	addi r8, r0, 48
	stb r1+0, r8
	addi r1, r5, 2
	addi r5, r0, 46
	stb lr+1, r5
	addi r5, r0, -1
	bgt r9, r5, .LBB160_63
.LBB160_61:
	sub r4, r4, r3
.LBB160_62:
	add r5, r1, r0
	addi r1, r1, 1
	addi r9, sp, 20
	add r5, r9, r5
	stb r5+0, r8
	addi r4, r4, -1
	bne r4, r0, .LBB160_62
.LBB160_63:
	blt r3, r7, .LBB160_70
.LBB160_64:
	addi r4, sp, 1060
.LBB160_65:
	add r5, r1, r0
	ldbu r7, r4+0
	addi r1, r1, 1
	addi r8, sp, 20
	add r5, r8, r5
	stb r5+0, r7
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r0, .LBB160_65
	jal r0, .LBB160_70
.LBB160_66:
	add r4, r0, r0
.LBB160_67:
	addi r1, sp, 20
	add r1, r1, r5
	addi r7, r0, 46
	stb r1+0, r7
	addi r1, r5, 1
	ble r3, r4, .LBB160_70
.LBB160_68:
	sub r3, r3, r4
	addi r5, sp, 1060
	add r4, r5, r4
.LBB160_69:
	ldbu r5, r4+0
	addi r7, sp, 20
	add r7, r7, r1
	stb r7+0, r5
	addi r1, r1, 1
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r0, .LBB160_69
.LBB160_70:
	addi r11, sp, 20
	add r3, r11, r1
	addi r4, r0, 1040
	sub r4, r4, r1
	lui r5, %hi(.L.str.129)
	addi r5, r5, %lo(.L.str.129)
	jal r31, snprintf
	add r3, r11, r0
	add r4, r0, r0
	jal r31, strtod
	add r6, r1, r0
	add r7, r2, r0
.LBB160_71:
	add r1, r6, r0
	add r2, r7, r0
	add r3, sp, r0
	addi r3, r3, 2047
	ldw r21, r3+1061
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
.Lfunc_end160:
	.size	fio_in_real, .Lfunc_end160-fio_in_real
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI160_0,@object
.LJTI160_0:
	.word	.LBB160_42
	.word	.LBB160_44
	.word	.LBB160_40
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_32
	.word	.LBB160_32
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_32
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_32
	.word	.LBB160_32
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_44
	.word	.LBB160_32
	.size	.LJTI160_0, 284
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
	bne r1, r0, .LBB161_7
.LBB161_1:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB161_4
.LBB161_2:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB161_6
.LBB161_3:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB161_6
.LBB161_4:
	addi r3, r0, 65
	bne r1, r3, .LBB161_7
.LBB161_5:
	add r3, r11, r0
	add r4, r12, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_rd_s
.LBB161_6:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 65
	beq r1, r3, .LBB161_5
.LBB161_7:
	addi r13, r0, 2
	bne r12, r13, .LBB161_9
.LBB161_8:
	ldh r1, r11+0
	jal r0, .LBB161_10
.LBB161_9:
	ldbu r1, r11+0
.LBB161_10:
	stw sp+16, r1
	addi r3, sp, 16
	jal r31, f77_rd_i
	ldw r1, sp+16
	bne r12, r13, .LBB161_12
.LBB161_11:
	sth r11+0, r1
	jal r0, .LBB161_13
.LBB161_12:
	stb r11+0, r1
.LBB161_13:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end161:
	.size	f77_rd_in, .Lfunc_end161-f77_rd_in
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
	bne r4, r11, .LBB162_12
.LBB162_1:
	beq r1, r0, .LBB162_12
.LBB162_2:
	lui r5, %hi(.L.str.115)
	addi r5, r5, %lo(.L.str.115)
	addi r3, sp, 20
	addi r11, r0, 32
	addi r6, r0, 7
	add r4, r11, r0
	add r7, r12, r0
	jal r31, snprintf
	ldbu r9, sp+20
	beq r9, r0, .LBB162_19
.LBB162_3:
	lui r1, %hi(fio_len)
	ldw r7, r1+%lo(fio_len)
	lui r3, %hi(fio_col)
	ldw r8, r3+%lo(fio_col)
	addi r4, sp, 20
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	jal r0, .LBB162_6
.LBB162_4:
	add r8, r9, r0
.LBB162_5:
	addi r4, r4, 1
	ldbu r9, r4+0
	beq r9, r0, .LBB162_19
.LBB162_6:
	bgt r8, r5, .LBB162_5
.LBB162_7:
	bge r7, r8, .LBB162_10
.LBB162_8:
	addi r10, r7, 1
	add r7, r7, r6
	stb r7+0, r11
	add r7, r10, r0
	bne r8, r10, .LBB162_8
.LBB162_9:
	stw r1+%lo(fio_len), r8
	add r7, r8, r0
.LBB162_10:
	add r10, r8, r6
	stb r10+0, r9
	addi r9, r8, 1
	stw r3+%lo(fio_col), r9
	blt r8, r7, .LBB162_4
.LBB162_11:
	stw r1+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB162_4
.LBB162_12:
	bne r1, r0, .LBB162_22
.LBB162_13:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB162_16
.LBB162_14:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB162_21
.LBB162_15:
	lui r5, %hi(fio_rep)
	ldw r5, r5+%lo(fio_rep)
	ble r5, r0, .LBB162_21
.LBB162_16:
	addi r3, r0, 65
	bne r1, r3, .LBB162_22
.LBB162_17:
	sth sp+20, r12
	stb sp+16, r12
	bne r4, r11, .LBB162_23
.LBB162_18:
	addi r3, sp, 20
	addi r4, r0, 2
	jal r0, .LBB162_24
.LBB162_19:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
.LBB162_20:
	ldw r13, sp+52
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.LBB162_21:
	stw r3+%lo(fio_desc), r0
	add r13, r4, r0
	jal r31, fio_next_desc
	add r4, r13, r0
	addi r3, r0, 65
	beq r1, r3, .LBB162_17
.LBB162_22:
	add r3, r12, r0
	ldw r13, sp+52
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jal r0, f77_wr_i
.LBB162_23:
	addi r3, sp, 16
	addi r4, r0, 1
.LBB162_24:
	jal r31, fio_hol_out
	jal r0, .LBB162_20
.Lfunc_end162:
	.size	f77_wr_in, .Lfunc_end162-f77_wr_in
                                        # -- End function
	.globl	f77_rd_d                        # -- Begin function f77_rd_d
	.p2align	2
	.type	f77_rd_d,@function
f77_rd_d:                               # @f77_rd_d
# %bb.0:
	addi sp, sp, -160
	stw sp+0, lr
	stw sp+156, r11
	stw sp+152, r12
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB163_5
.LBB163_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB163_26
.LBB163_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB163_26
.LBB163_3:
	addi r3, r0, 1
	bne r1, r3, .LBB163_12
.LBB163_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB163_26
.LBB163_5:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB163_8
.LBB163_6:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB163_15
.LBB163_7:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB163_15
.LBB163_8:
	addi r3, r0, 72
	bgt r1, r3, .LBB163_16
.LBB163_9:
	beq r1, r0, .LBB163_26
.LBB163_10:
	addi r3, r0, 65
	bne r1, r3, .LBB163_19
.LBB163_11:
	addi r4, r0, 8
	add r3, r11, r0
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jal r0, f77_rd_s
.LBB163_12:
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB163_24
.LBB163_13:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB163_14:
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
	bne r5, r0, .LBB163_14
	jal r0, .LBB163_25
.LBB163_15:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB163_9
.LBB163_16:
	addi r3, r0, 73
	bne r1, r3, .LBB163_18
.LBB163_17:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	jal r31, fio_in_int
	fcvt.d.w r4, r1
	jal r0, .LBB163_20
.LBB163_18:
	addi r3, r0, 76
	beq r1, r3, .LBB163_27
.LBB163_19:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
.LBB163_20:
	stw r11+4, r5
	stw r11+0, r4
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	blt r3, r4, .LBB163_22
.LBB163_21:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB163_22:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB163_26
.LBB163_23:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
	jal r0, .LBB163_26
.LBB163_24:
	add r6, r0, r0
.LBB163_25:
	addi r3, sp, 24
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
.LBB163_26:
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jalr r0, r31, 0
.LBB163_27:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end163:
	.size	f77_rd_d, .Lfunc_end163-f77_rd_d
                                        # -- End function
	.globl	f77_rd_r                        # -- Begin function f77_rd_r
	.p2align	2
	.type	f77_rd_r,@function
f77_rd_r:                               # @f77_rd_r
# %bb.0:
	addi sp, sp, -160
	stw sp+0, lr
	stw sp+156, r11
	stw sp+152, r12
	add r11, r3, r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB164_5
.LBB164_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB164_26
.LBB164_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB164_26
.LBB164_3:
	addi r3, r0, 1
	bne r1, r3, .LBB164_12
.LBB164_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB164_26
.LBB164_5:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB164_8
.LBB164_6:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB164_15
.LBB164_7:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB164_15
.LBB164_8:
	addi r3, r0, 72
	bgt r1, r3, .LBB164_16
.LBB164_9:
	beq r1, r0, .LBB164_26
.LBB164_10:
	addi r3, r0, 65
	bne r1, r3, .LBB164_19
.LBB164_11:
	addi r4, r0, 4
	add r3, r11, r0
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jal r0, f77_rd_s
.LBB164_12:
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB164_24
.LBB164_13:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB164_14:
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
	bne r5, r0, .LBB164_14
	jal r0, .LBB164_25
.LBB164_15:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB164_9
.LBB164_16:
	addi r3, r0, 73
	bne r1, r3, .LBB164_18
.LBB164_17:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	jal r31, fio_in_int
	fcvt.s.w r1, r1
	jal r0, .LBB164_20
.LBB164_18:
	addi r3, r0, 76
	beq r1, r3, .LBB164_27
.LBB164_19:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
.LBB164_20:
	stw r11+0, r1
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	blt r3, r4, .LBB164_22
.LBB164_21:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB164_22:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB164_26
.LBB164_23:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
	jal r0, .LBB164_26
.LBB164_24:
	add r6, r0, r0
.LBB164_25:
	addi r3, sp, 24
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
.LBB164_26:
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jalr r0, r31, 0
.LBB164_27:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end164:
	.size	f77_rd_r, .Lfunc_end164-f77_rd_r
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
	beq r1, r0, .LBB165_5
.LBB165_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB165_7
.LBB165_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB165_7
.LBB165_3:
	addi r3, r0, 1
	bne r1, r3, .LBB165_6
.LBB165_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB165_7
.LBB165_5:
	add r3, r11, r0
	jal r31, f77_rd_r
	addi r3, r11, 4
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_rd_r
.LBB165_6:
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
.LBB165_7:
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end165:
	.size	f77_rd_c, .Lfunc_end165-f77_rd_c
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_list_cplx_in
	.type	.LCPI166_0,@object
.LCPI166_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI166_0, 8
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
	bne r1, r3, .LBB166_6
.LBB166_1:
	lui r15, %hi(fio_ltok+1)
	addi r15, r15, %lo(fio_ltok+1)
	ldbu r4, r15+0
	beq r4, r0, .LBB166_9
.LBB166_2:
	lui r1, %hi(fio_ltok+2)
	addi r1, r1, %lo(fio_ltok+2)
	addi r3, r0, 126
	add r14, r0, r0
	jal r0, .LBB166_4
.LBB166_3:
	ldbu r4, r1+0
	addi r1, r1, 1
	beq r4, r0, .LBB166_10
.LBB166_4:
	bgt r14, r3, .LBB166_3
.LBB166_5:
	addi r5, r14, 1
	addi r6, sp, 20
	add r6, r6, r14
	stb r6+0, r4
	add r14, r5, r0
	jal r0, .LBB166_3
.LBB166_6:
	beq r1, r0, .LBB166_22
.LBB166_7:
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB166_8:
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
	bne r1, r0, .LBB166_8
	jal r0, .LBB166_23
.LBB166_9:
	add r14, r0, r0
.LBB166_10:
	lui r16, %hi(fio_rpos)
	lui r17, %hi(fio_rlen)
	addi r18, r0, 126
	lui r19, %hi(fio_rec)
	addi r19, r19, %lo(fio_rec)
	addi r20, r0, 41
.LBB166_11:
	addi r1, sp, 20
	add r21, r1, r14
	jal r0, .LBB166_13
.LBB166_12:
	jal r31, fio_getrec
	bne r1, r0, .LBB166_37
.LBB166_13:
	beq r14, r0, .LBB166_15
.LBB166_14:
	ldbu r1, r21+-1
	beq r1, r20, .LBB166_18
.LBB166_15:
	ldw r1, r16+%lo(fio_rpos)
	ldw r3, r17+%lo(fio_rlen)
	bge r1, r3, .LBB166_12
.LBB166_16:
	addi r3, r1, 1
	stw r16+%lo(fio_rpos), r3
	bgt r14, r18, .LBB166_13
.LBB166_17:
	add r1, r1, r19
	ldbu r1, r1+0
	addi r3, r14, 1
	addi r4, sp, 20
	add r4, r4, r14
	stb r4+0, r1
	add r14, r3, r0
	jal r0, .LBB166_11
.LBB166_18:
	stb r21+-1, r0
	addi r3, sp, 20
	addi r4, r0, 44
	jal r31, strchr
	beq r1, r0, .LBB166_24
.LBB166_19:
	add r14, r1, r0
	stb r1+0, r0
	addi r4, sp, 20
	add r3, r13, r0
	jal r31, strcpy
	ldbu r4, r13+0
	beq r4, r0, .LBB166_27
.LBB166_20:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB166_21:
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
	bne r4, r0, .LBB166_21
	jal r0, .LBB166_28
.LBB166_22:
	add r6, r0, r0
.LBB166_23:
	addi r3, sp, 148
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r12+4, r2
	stw r12+0, r1
	stw r11+4, r0
	stw r11+0, r0
	jal r0, .LBB166_36
.LBB166_24:
	addi r4, sp, 20
	add r3, r13, r0
	jal r31, strcpy
	ldbu r4, r13+0
	beq r4, r0, .LBB166_33
.LBB166_25:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB166_26:
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
	bne r4, r0, .LBB166_26
	jal r0, .LBB166_34
.LBB166_27:
	add r5, r0, r0
.LBB166_28:
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
	beq r4, r0, .LBB166_31
.LBB166_29:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB166_30:
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
	bne r4, r0, .LBB166_30
	jal r0, .LBB166_32
.LBB166_31:
	add r5, r0, r0
.LBB166_32:
	addi r3, sp, 148
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	jal r0, .LBB166_35
.LBB166_33:
	add r5, r0, r0
.LBB166_34:
	addi r3, sp, 148
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r12+4, r2
	stw r12+0, r1
	lui r1, %hi(.LCPI166_0)
	addi r1, r1, %lo(.LCPI166_0)
	ldw r5, r1+4
	ldw r4, r1+0
.LBB166_35:
	stw r11+4, r5
	stw r11+0, r4
	lui r1, %hi(fio_l_after)
	addi r3, r0, 1
	stb r1+%lo(fio_l_after), r3
.LBB166_36:
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
.LBB166_37:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end166:
	.size	fio_list_cplx_in, .Lfunc_end166-fio_list_cplx_in
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
	jal r31, f77_rd_d
	addi r3, r11, 8
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_rd_d
.LBB167_6:
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
.LBB167_7:
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end167:
	.size	f77_rd_z, .Lfunc_end167-f77_rd_z
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
	bne r1, r12, .LBB168_8
.LBB168_1:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB168_5
.LBB168_2:
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
	bne r1, r0, .LBB168_4
.LBB168_3:
	addi r3, r0, 1024
	jal r31, malloc
	stw r14+0, r1
.LBB168_4:
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
.LBB168_5:
	lui r1, %hi(fio_sizep)
	ldw r3, r1+%lo(fio_sizep)
	beq r3, r0, .LBB168_7
.LBB168_6:
	lui r4, %hi(fio_size)
	ldw r4, r4+%lo(fio_size)
	stw r3+0, r4
.LBB168_7:
	stb r11+%lo(fio_noadv), r0
	stw r1+%lo(fio_sizep), r0
.LBB168_8:
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
.Lfunc_end168:
	.size	f77_rd_end, .Lfunc_end168-f77_rd_end
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
	blt r5, r24, .LBB169_3
.LBB169_1:
	add r12, r4, r0
	add r15, r0, r0
.LBB169_2:
	add r1, r12, r15
	ldbu r3, r1+0
	jal r31, toupper
	add r3, r15, r14
	stb r3+0, r1
	addi r15, r15, 1
	bne r13, r15, .LBB169_2
.LBB169_3:
	add r1, r13, r14
	stb r1+0, r0
	beq r11, r0, .LBB169_71
.LBB169_4:
	lui r1, %hi(nml_nasg)
	stw sp+28, r1
	stw r1+%lo(nml_nasg), r0
	lui r1, %hi(nml_nval)
	stw sp+36, r1
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
	addi r1, sp, 72
	stw sp+24, r13
	add r1, r1, r13
	stw sp+20, r1
	lui r13, %hi(.L.str.130)
	addi r13, r13, %lo(.L.str.130)
	addi r14, r0, 3
	addi r15, r0, 1023
	lui r27, %hi(fio_noadv)
	lui r1, %hi(fio_unit)
	stw sp+60, r1
	addi r1, r0, 99
	stw sp+56, r1
	lui r1, %hi(fio_eor)
	stw sp+68, r1
	lui r20, %hi(fio_sizep)
	lui r1, %hi(fio_size)
	stw sp+64, r1
	lui r1, %hi(fio_psave)
	addi r1, r1, %lo(fio_psave)
	stw sp+52, r1
	addi r1, r0, 1024
	stw sp+32, r1
	lui r1, %hi(fio_rlen)
	stw sp+48, r1
	lui r1, %hi(fio_rec)
	addi r1, r1, %lo(fio_rec)
	stw sp+44, r1
	lui r1, %hi(fio_plen)
	addi r1, r1, %lo(fio_plen)
	stw sp+40, r1
                                        # implicit-def: $r16
	add r28, r0, r0
	add fp, r0, r0
.LBB169_5:
	ldw r3, r23+%lo(nml_unit)
	add r4, r13, r0
	add r5, r14, r0
	add r6, r14, r0
	jal r31, f77_rd_begin
	beq r1, r0, .LBB169_8
.LBB169_6:
	bne r1, r24, .LBB169_20
.LBB169_7:
	add r16, r1, r0
	jal r0, .LBB169_62
.LBB169_8:
	addi r3, sp, 72
	add r4, r15, r0
	jal r31, f77_rd_s
	ldbu r1, r27+%lo(fio_noadv)
	bne r1, r24, .LBB169_16
.LBB169_9:
	ldw r1, sp+68
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB169_13
.LBB169_10:
	ldw r1, sp+60
	ldw r1, r1+%lo(fio_unit)
	addi r3, r1, -1
	ldw r4, sp+56
	sltu r3, r3, r4
	sub r3, r0, r3
	and r1, r1, r3
	slli r17, r1, 2
	ldw r1, sp+52
	add r18, r17, r1
	ldw r1, r18+0
	bne r1, r0, .LBB169_12
.LBB169_11:
	ldw r3, sp+32
	jal r31, malloc
	stw r18+0, r1
.LBB169_12:
	ldw r3, r18+0
	ldw r18, sp+48
	ldw r5, r18+%lo(fio_rlen)
	ldw r4, sp+44
	jal r31, memcpy
	ldw r1, r18+%lo(fio_rlen)
	ldw r3, sp+40
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
.LBB169_13:
	ldw r1, r20+%lo(fio_sizep)
	beq r1, r0, .LBB169_15
.LBB169_14:
	ldw r3, sp+64
	ldw r3, r3+%lo(fio_size)
	stw r1+0, r3
.LBB169_15:
	stb r27+%lo(fio_noadv), r0
	stw r20+%lo(fio_sizep), r0
.LBB169_16:
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_reading)
	stb r1+%lo(fio_reading), r0
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	addi r3, r0, 1022
.LBB169_17:
	addi r1, sp, 72
	add r1, r1, r3
	ldbu r4, r1+0
	addi r1, r0, 32
	bne r4, r1, .LBB169_23
.LBB169_18:
	addi r4, r3, -1
	addi r5, r3, 1
	addi r6, r0, 2
	add r3, r4, r0
	bgtu r5, r6, .LBB169_17
.LBB169_19:
	addi r25, r0, 1
	jal r0, .LBB169_24
.LBB169_20:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB169_22
.LBB169_21:
	addi r1, r0, 2
	stw r19+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.131)
	addi r4, r4, %lo(.L.str.131)
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
.LBB169_22:
	add r16, r0, r0
	add r1, r24, r0
	jal r0, .LBB169_62
.LBB169_23:
	addi r25, r3, 1
.LBB169_24:
	addi r3, r0, 0
	seq r17, fp, r3
	beq r28, r0, .LBB169_26
.LBB169_25:
	addi r28, r0, 1
	jal r0, .LBB169_42
.LBB169_26:
	ldbu r3, sp+72
	andi r4, r3, 253
	addi r3, r0, 36
	seq r4, r4, r3
	sub r26, r0, r4
	and r4, r17, r26
	xor r17, r17, r4
	bge r17, r25, .LBB169_39
.LBB169_27:
	addi r4, sp, 72
	add r4, r4, r17
	ldbu r4, r4+0
	bne r4, r1, .LBB169_30
.LBB169_28:
	addi r17, r17, 1
	bne r25, r17, .LBB169_27
.LBB169_29:
	add r28, r0, r0
	add r17, r25, r0
	jal r0, .LBB169_40
.LBB169_30:
	beq r4, r3, .LBB169_32
.LBB169_31:
	addi r1, r0, 38
	bne r4, r1, .LBB169_39
.LBB169_32:
	addi r28, r0, 1
	blt r12, r28, .LBB169_37
.LBB169_33:
	addi r1, sp, 72
	add r18, r1, r17
	add r21, r0, r0
.LBB169_34:
	add r1, r17, r21
	addi r1, r1, 1
	bgeu r1, r25, .LBB169_39
.LBB169_35:
	add r1, r18, r21
	ldbu r3, r1+1
	jal r31, toupper
	add r3, r21, r11
	ldb r3, r3+0
	bne r1, r3, .LBB169_39
.LBB169_36:
	addi r21, r21, 1
	bne r12, r21, .LBB169_34
.LBB169_37:
	ldw r1, sp+24
	add r21, r1, r17
	bge r21, r25, .LBB169_68
.LBB169_38:
	ldw r1, sp+20
	add r1, r1, r17
	ldbu r18, r1+0
	add r3, r18, r0
	jal r31, isalnum
	beq r1, r0, .LBB169_64
.LBB169_39:
	add r28, r0, r0
.LBB169_40:
	add r3, r0, r0
	add r1, r14, r0
.LBB169_41:
	xori r4, fp, 1
	and r4, r4, r26
	xor fp, fp, r4
	beq r3, r0, .LBB169_62
.LBB169_42:
	bge r17, r25, .LBB169_61
.LBB169_43:
	addi r1, sp, 72
	add r18, r1, r17
	addi r26, r0, 1
	add r4, r0, r0
	add r21, r17, r0
	jal r0, .LBB169_45
.LBB169_44:
	addi r21, r21, 1
	addi r26, r26, 1
	add r4, r3, r0
	bge r21, r25, .LBB169_61
.LBB169_45:
	addi r1, sp, 72
	add r1, r1, r21
	ldb r3, r1+0
	addi r1, r0, 13
	beq r4, r0, .LBB169_47
.LBB169_46:
	seq r3, r4, r3
	sub r3, r0, r3
	and r3, r4, r3
	xor r3, r4, r3
	jal r0, .LBB169_58
.LBB169_47:
	andi r4, r3, 255
	addi r4, r4, -33
	addi r5, r0, 14
	bgtu r4, r5, .LBB169_50
.LBB169_48:
	slli r4, r4, 2
	lui r5, %hi(.LJTI169_0)
	addi r5, r5, %lo(.LJTI169_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB169_49:
	addi r1, r0, 11
	add r25, r21, r0
	jal r0, .LBB169_57
.LBB169_50:
	addi r1, r0, -3
	and r1, r3, r1
	addi r3, r0, 36
	bne r1, r3, .LBB169_56
.LBB169_51:
	addi r1, r21, 3
	bge r1, r25, .LBB169_56
.LBB169_52:
	addi r1, sp, 72
	add r1, r1, r21
	ldbu r3, r1+1
	jal r31, toupper
	addi r3, r0, 69
	bne r1, r3, .LBB169_56
.LBB169_53:
	addi r1, sp, 72
	add r1, r1, r21
	ldbu r3, r1+2
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB169_56
.LBB169_54:
	addi r1, sp, 72
	add r1, r1, r21
	ldbu r3, r1+3
	jal r31, toupper
	addi r3, r0, 68
	bne r1, r3, .LBB169_56
.LBB169_55:
	addi r4, r26, -1
	add r3, r18, r0
	jal r31, nml_text_add
	addi r1, r0, 1
	add r16, r0, r0
	jal r0, .LBB169_57
.LBB169_56:
	add r1, r0, r0
.LBB169_57:
	add r3, r0, r0
.LBB169_58:
	beq r1, r0, .LBB169_44
.LBB169_59:
	addi r4, r0, 13
	beq r1, r4, .LBB169_44
.LBB169_60:
	addi r3, r0, 11
	bne r1, r3, .LBB169_62
.LBB169_61:
	addi r1, sp, 72
	add r3, r1, r17
	sub r4, r25, r17
	jal r31, nml_text_add
	lui r3, %hi(.L.str.132)
	addi r3, r3, %lo(.L.str.132)
	addi r4, r0, 1
	jal r31, nml_text_add
	add r1, r0, r0
.LBB169_62:
	beq r1, r0, .LBB169_5
.LBB169_63:
	beq r1, r14, .LBB169_5
	jal r0, .LBB169_69
.LBB169_64:
	addi r1, r18, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB169_39
.LBB169_65:
	addi r1, r0, 64
	beq r18, r1, .LBB169_39
.LBB169_66:
	addi r4, r0, 95
	add r28, r0, r0
	add r3, r0, r0
	add r1, r14, r0
	beq r18, r4, .LBB169_41
.LBB169_67:
	addi r28, r0, 1
.LBB169_68:
	add r17, r21, r0
	add r3, r28, r0
	add r1, r0, r0
	jal r0, .LBB169_41
.LBB169_69:
	beq r16, r0, .LBB169_73
.LBB169_70:
	addi r1, r0, 1
	stw r19+%lo(nml_status), r1
	jal r0, .LBB169_72
.LBB169_71:
	lui r1, 2
	addi r1, r1, 1568
	sth sp+72, r1
	addi r11, sp, 72
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
.LBB169_72:
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
.LBB169_73:
	ldw r1, sp+28
	stw r1+%lo(nml_nasg), r0
	ldw r1, sp+36
	stw r1+%lo(nml_nval), r0
	ldw r1, r22+%lo(nml_tlen)
	addi r15, r0, 1
	blt r1, r15, .LBB169_72
.LBB169_74:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB169_72
.LBB169_75:
	lui r16, %hi(nml_text)
	addi r17, r0, 44
	lui r18, %hi(nml_cval)
	lui r23, %hi(nml_val)
	addi r24, r0, 12
	addi r1, r0, 0
	stw sp+64, r1
	addi r26, r0, 2
	addi r27, r0, 32
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+60, r1
	lui r1, %hi(.L.str.135)
	addi r1, r1, %lo(.L.str.135)
	stw sp+56, r1
	addi r13, r0, 95
	lui r1, %hi(nml_err+95)
	stw sp+68, r1
	add r12, r15, r0
	add fp, r0, r0
	add r25, r0, r0
.LBB169_76:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r25
	ldbu r3, r1+0
	beq r3, r17, .LBB169_79
.LBB169_77:
	bne r3, r27, .LBB169_86
.LBB169_78:
	addi r11, r25, 1
	jal r0, .LBB169_198
.LBB169_79:
	beq r12, r0, .LBB169_85
.LBB169_80:
	beq fp, r0, .LBB169_85
.LBB169_81:
	ldw r1, sp+36
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB169_84
.LBB169_82:
	ldw r3, sp+64
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
	beq r1, r0, .LBB169_211
.LBB169_83:
	stw r23+%lo(nml_val), r1
.LBB169_84:
	ldw r1, r23+%lo(nml_val)
	ldw r5, sp+36
	ldw r3, r5+%lo(nml_nval)
	mul r4, r3, r24
	add r1, r1, r4
	stw r1+0, r25
	stw r1+4, r0
	stw r1+8, r15
	addi r1, r3, 1
	stw r5+%lo(nml_nval), r1
	ldw r1, fp+68
	addi r1, r1, 1
	stw fp+68, r1
.LBB169_85:
	addi r11, r25, 1
	jal r0, .LBB169_197
.LBB169_86:
	jal r31, isalpha
                                        # implicit-def: $r11
	add r5, r15, r0
	beq r1, r0, .LBB169_107
.LBB169_87:
	ldw r1, r22+%lo(nml_tlen)
	add r20, r25, r0
	bge r25, r1, .LBB169_94
.LBB169_88:
	add r20, r25, r0
	jal r0, .LBB169_90
.LBB169_89:
	addi r20, r20, 1
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB169_94
.LBB169_90:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB169_89
.LBB169_91:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB169_89
.LBB169_92:
	beq r14, r13, .LBB169_89
.LBB169_93:
	addi r1, r0, 64
	beq r14, r1, .LBB169_89
.LBB169_94:
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB169_104
.LBB169_95:
	ldw r3, r16+%lo(nml_text)
.LBB169_96:
	add r4, r3, r20
	ldbu r4, r4+0
	bne r4, r27, .LBB169_99
.LBB169_97:
	addi r20, r20, 1
	bne r1, r20, .LBB169_96
.LBB169_98:
	add r4, r1, r0
	jal r0, .LBB169_105
.LBB169_99:
	addi r5, r0, 40
	bne r4, r5, .LBB169_104
.LBB169_100:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r20, r1, .LBB169_107
.LBB169_101:
	add r5, r3, r20
	ldbu r6, r5+0
	addi r4, r20, 1
	addi r7, r0, 41
	beq r6, r7, .LBB169_206
.LBB169_102:
	add r20, r4, r0
	blt r4, r1, .LBB169_101
.LBB169_103:
                                        # implicit-def: $r11
	add r5, r15, r0
	jal r0, .LBB169_107
.LBB169_104:
	add r4, r20, r0
.LBB169_105:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r4, r1, .LBB169_107
.LBB169_106:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r4
	ldbu r1, r1+0
	addi r3, r0, 61
	sne r5, r1, r3
	addi r11, r4, 1
.LBB169_107:
	beq r5, r0, .LBB169_117
.LBB169_108:
	beq fp, r0, .LBB169_163
.LBB169_109:
	ldw r1, r22+%lo(nml_tlen)
	add r11, r25, r0
	bge r25, r1, .LBB169_112
.LBB169_110:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB169_112
.LBB169_111:
	addi r11, r11, 1
	ldw r1, r22+%lo(nml_tlen)
	blt r11, r1, .LBB169_110
.LBB169_112:
	ble r11, r25, .LBB169_131
.LBB169_113:
	ldw r1, r22+%lo(nml_tlen)
	bge r11, r1, .LBB169_131
.LBB169_114:
	ldw r1, r16+%lo(nml_text)
	add r3, r1, r11
	ldbu r3, r3+0
	addi r4, r0, 42
	bne r3, r4, .LBB169_131
.LBB169_115:
	add r3, r1, r25
	jal r31, atoi
	ble r1, r0, .LBB169_203
.LBB169_116:
	add r14, r1, r0
	addi r25, r11, 1
	jal r0, .LBB169_132
.LBB169_117:
	ldw r1, sp+28
	ldw r4, r1+%lo(nml_nasg)
	lui r1, %hi(nml_casg)
	ldw r3, r1+%lo(nml_casg)
	lui r14, %hi(nml_asg)
	blt r4, r3, .LBB169_120
.LBB169_118:
	ldw r4, sp+64
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
	beq r1, r0, .LBB169_211
.LBB169_119:
	stw r14+%lo(nml_asg), r1
.LBB169_120:
	ldw r1, r14+%lo(nml_asg)
	ldw r5, sp+28
	ldw r3, r5+%lo(nml_nasg)
	addi r4, r3, 1
	stw r5+%lo(nml_nasg), r4
	addi r4, r0, 76
	mul r3, r3, r4
	add fp, r1, r3
	ldw r1, r22+%lo(nml_tlen)
	bge r25, r1, .LBB169_130
.LBB169_121:
	add r21, r0, r0
.LBB169_122:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r25
	add r1, r1, r21
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	addi r3, r0, 1
	beq r1, r0, .LBB169_126
.LBB169_123:
	beq r3, r0, .LBB169_166
.LBB169_124:
	addi r1, r0, 30
	bgtu r21, r1, .LBB169_166
.LBB169_125:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r25
	add r1, r1, r21
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r21, 1
	add r4, fp, r21
	stb r4+0, r1
	ldw r1, r22+%lo(nml_tlen)
	add r20, r25, r3
	add r21, r3, r0
	blt r20, r1, .LBB169_122
	jal r0, .LBB169_167
.LBB169_126:
	addi r1, r14, -35
	addi r4, r0, 2
	bltu r1, r4, .LBB169_123
.LBB169_127:
	addi r1, r0, 64
	beq r14, r1, .LBB169_123
.LBB169_128:
	beq r14, r13, .LBB169_123
.LBB169_129:
	add r3, r0, r0
	jal r0, .LBB169_123
.LBB169_130:
	add r20, r25, r0
	add r21, r0, r0
	jal r0, .LBB169_168
.LBB169_131:
	add r14, r15, r0
.LBB169_132:
	add r12, r0, r0
	add r1, r0, r0
	add r11, r25, r0
	jal r0, .LBB169_137
.LBB169_133:
	add r1, r0, r0
.LBB169_134:
	add r4, r11, r0
.LBB169_135:
	addi r11, r4, 1
	add r3, r0, r0
.LBB169_136:
	bne r3, r0, .LBB169_159
.LBB169_137:
	ldw r3, r22+%lo(nml_tlen)
	bge r11, r3, .LBB169_159
.LBB169_138:
	ldw r6, r16+%lo(nml_text)
	add r4, r6, r11
	ldb r5, r4+0
	beq r1, r0, .LBB169_142
.LBB169_139:
	bne r1, r5, .LBB169_134
.LBB169_140:
	addi r4, r11, 1
	bge r4, r3, .LBB169_133
.LBB169_141:
	add r3, r6, r4
	ldb r3, r3+0
	bne r1, r3, .LBB169_133
	jal r0, .LBB169_135
.LBB169_142:
	andi r20, r5, 255
	addi r1, r20, -32
	addi r6, r0, 9
	bgtu r1, r6, .LBB169_152
.LBB169_143:
	slli r1, r1, 2
	lui r6, %hi(.LJTI169_1)
	addi r6, r6, %lo(.LJTI169_1)
	add r1, r6, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB169_144:
	addi r11, r11, 1
	add r3, r0, r0
	add r1, r5, r0
	jal r0, .LBB169_136
.LBB169_145:
	addi r1, r0, 2
	blt r11, r1, .LBB169_152
.LBB169_146:
	addi r21, r11, 1
	bge r21, r3, .LBB169_152
.LBB169_147:
	ldbu r3, r4+-1
	jal r31, toupper
	addi r1, r1, -70
	addi r3, r0, -2
	bltu r1, r3, .LBB169_152
.LBB169_148:
	ldw r1, r16+%lo(nml_text)
	addi r28, r11, -2
	add r1, r1, r28
	ldbu r3, r1+0
	jal r31, isdigit
	bne r1, r0, .LBB169_150
.LBB169_149:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r28
	ldbu r1, r1+0
	addi r3, r0, 46
	bne r1, r3, .LBB169_152
.LBB169_150:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r21
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB169_152
.LBB169_151:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	addi r3, r0, 43
	stb r1+0, r3
	jal r0, .LBB169_157
.LBB169_152:
	bgt r12, r0, .LBB169_157
.LBB169_153:
	addi r3, r0, 1
	beq r20, r27, .LBB169_158
.LBB169_154:
	beq r20, r17, .LBB169_158
	jal r0, .LBB169_157
.LBB169_155:
	addi r12, r12, 1
	jal r0, .LBB169_157
.LBB169_156:
	addi r12, r12, -1
.LBB169_157:
	addi r11, r11, 1
	add r3, r0, r0
.LBB169_158:
	add r1, r0, r0
	jal r0, .LBB169_136
.LBB169_159:
	ldw r1, sp+36
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB169_162
.LBB169_160:
	ldw r3, sp+64
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
	beq r1, r0, .LBB169_211
.LBB169_161:
	stw r23+%lo(nml_val), r1
.LBB169_162:
	ldw r1, r23+%lo(nml_val)
	ldw r5, sp+36
	ldw r3, r5+%lo(nml_nval)
	mul r4, r3, r24
	add r1, r1, r4
	stw r1+0, r25
	sub r4, r11, r25
	stw r1+4, r4
	stw r1+8, r14
	addi r1, r3, 1
	stw r5+%lo(nml_nval), r1
	ldw r1, fp+68
	addi r1, r1, 1
	stw fp+68, r1
	add r12, r0, r0
	add r14, r0, r0
	jal r0, .LBB169_199
.LBB169_163:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB169_165
.LBB169_164:
	stw r19+%lo(nml_status), r26
	ldw r3, sp+60
	ldw r4, sp+56
	add r5, r13, r0
	jal r31, strncpy
	ldw r1, sp+68
	stb r1+%lo(nml_err+95), r0
.LBB169_165:
	add r11, r25, r0
	add fp, r0, r0
	add r14, r15, r0
	jal r0, .LBB169_199
.LBB169_166:
	add r20, r25, r21
	jal r0, .LBB169_168
.LBB169_167:
	add r21, r3, r0
.LBB169_168:
	add r1, fp, r21
	stb r1+0, r0
	jal r0, .LBB169_170
.LBB169_169:
	addi r20, r20, 1
.LBB169_170:
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB169_175
.LBB169_171:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB169_169
.LBB169_172:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB169_169
.LBB169_173:
	beq r14, r13, .LBB169_169
.LBB169_174:
	addi r1, r0, 64
	beq r14, r1, .LBB169_169
.LBB169_175:
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB169_180
.LBB169_176:
	ldw r3, r16+%lo(nml_text)
.LBB169_177:
	add r4, r3, r20
	ldbu r4, r4+0
	bne r4, r27, .LBB169_180
.LBB169_178:
	addi r20, r20, 1
	bne r1, r20, .LBB169_177
.LBB169_179:
	add r20, r1, r0
.LBB169_180:
	stw fp+32, r0
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r1, r1+0
	addi r3, r0, 40
	bne r1, r3, .LBB169_196
.LBB169_181:
	addi r21, r20, 1
	addi r20, fp, 36
	jal r0, .LBB169_184
.LBB169_182:
	ldw r1, r19+%lo(nml_status)
	addi r14, r0, 1
	beq r1, r0, .LBB169_194
.LBB169_183:
	addi r1, r0, 11
	bne r14, r1, .LBB169_195
.LBB169_184:
	ldw r1, r16+%lo(nml_text)
	addi r21, r21, -1
.LBB169_185:
	add r3, r1, r21
	ldbu r3, r3+1
	addi r21, r21, 1
	beq r3, r27, .LBB169_185
.LBB169_186:
	add r3, r1, r21
	addi r4, sp, 72
	addi r14, r0, 10
	add r5, r14, r0
	jal r31, strtol
	ldw r4, sp+72
	ldw r3, r16+%lo(nml_text)
	add r5, r3, r21
	beq r5, r4, .LBB169_182
.LBB169_187:
	ldw r5, fp+32
	addi r6, r0, 6
	bgt r5, r6, .LBB169_189
.LBB169_188:
	addi r6, r5, 1
	stw fp+32, r6
	slli r5, r5, 2
	add r5, r20, r5
	stw r5+0, r1
.LBB169_189:
	sub r21, r4, r3
.LBB169_190:
	add r1, r3, r21
	ldbu r1, r1+0
	bne r1, r27, .LBB169_192
.LBB169_191:
	addi r21, r21, 1
	jal r0, .LBB169_190
.LBB169_192:
	bne r1, r17, .LBB169_183
.LBB169_193:
	addi r21, r21, 1
	addi r14, r0, 11
	jal r0, .LBB169_183
.LBB169_194:
	addi r1, r0, 2
	stw r19+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.134)
	addi r4, r4, %lo(.L.str.134)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+68
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB169_183
.LBB169_195:
	addi r1, r0, 10
	bne r14, r1, .LBB169_202
.LBB169_196:
	ldw r1, sp+36
	ldw r1, r1+%lo(nml_nval)
	stw fp+64, r1
	stw fp+68, r0
	stw fp+72, r0
.LBB169_197:
	add r12, r15, r0
.LBB169_198:
	add r14, r26, r0
.LBB169_199:
	ori  r1, r14, 2
	bne r1, r26, .LBB169_72
.LBB169_200:
	ldw r1, r22+%lo(nml_tlen)
	bge r11, r1, .LBB169_72
.LBB169_201:
	ldw r1, r19+%lo(nml_status)
	add r25, r11, r0
	beq r1, r0, .LBB169_76
	jal r0, .LBB169_72
.LBB169_202:
	add r11, r25, r0
	jal r0, .LBB169_199
.LBB169_203:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB169_205
.LBB169_204:
	addi r1, r0, 2
	stw r19+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.136)
	addi r4, r4, %lo(.L.str.136)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+68
	stb r1+%lo(nml_err+95), r0
.LBB169_205:
	add r11, r25, r0
	add r14, r15, r0
	jal r0, .LBB169_199
.LBB169_206:
	bge r4, r1, .LBB169_105
.LBB169_207:
	add r3, r0, r0
.LBB169_208:
	add r4, r5, r3
	ldbu r4, r4+1
	bne r4, r27, .LBB169_210
.LBB169_209:
	addi r3, r3, 1
	add r4, r20, r3
	addi r4, r4, 1
	blt r4, r1, .LBB169_208
	jal r0, .LBB169_105
.LBB169_210:
	add r3, r20, r3
	addi r4, r3, 1
	jal r0, .LBB169_105
.LBB169_211:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.133)
	addi r4, r4, %lo(.L.str.133)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end169:
	.size	f77_nml_begin, .Lfunc_end169-f77_nml_begin
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI169_0,@object
.LJTI169_0:
	.word	.LBB169_49
	.word	.LBB169_58
	.word	.LBB169_50
	.word	.LBB169_50
	.word	.LBB169_50
	.word	.LBB169_50
	.word	.LBB169_58
	.word	.LBB169_50
	.word	.LBB169_50
	.word	.LBB169_50
	.word	.LBB169_50
	.word	.LBB169_50
	.word	.LBB169_50
	.word	.LBB169_50
	.word	.LBB169_55
	.size	.LJTI169_0, 60
	.type	.LJTI169_1,@object
.LJTI169_1:
	.word	.LBB169_145
	.word	.LBB169_152
	.word	.LBB169_144
	.word	.LBB169_152
	.word	.LBB169_152
	.word	.LBB169_152
	.word	.LBB169_152
	.word	.LBB169_144
	.word	.LBB169_155
	.word	.LBB169_156
	.size	.LJTI169_1, 40
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
	lui r4, %hi(.L.str.130)
	addi r4, r4, %lo(.L.str.130)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	add r3, r12, r0
	add r4, r11, r0
	jal r31, f77_wr_a
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB170_4
.LBB170_1:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB170_3
.LBB170_2:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB170_4
.LBB170_3:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB170_4:
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
.Lfunc_end170:
	.size	nml_record, .Lfunc_end170-nml_record
                                        # -- End function
	.globl	f77_nml_dim                     # -- Begin function f77_nml_dim
	.p2align	2
	.type	f77_nml_dim,@function
f77_nml_dim:                            # @f77_nml_dim
# %bb.0:
	lui r1, %hi(nml_ndim)
	ldw r5, r1+%lo(nml_ndim)
	addi r6, r0, 6
	bgt r5, r6, .LBB171_2
.LBB171_1:
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
.LBB171_2:
	jalr r0, r31, 0
.Lfunc_end171:
	.size	f77_nml_dim, .Lfunc_end171-f77_nml_dim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_nml_item
	.type	.LCPI172_0,@object
.LCPI172_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI172_0, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI172_1,@object
.LCPI172_1:
	.word	0x00000000                      # float 0
	.size	.LCPI172_1, 4
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
	blt r4, r17, .LBB172_3
.LBB172_1:
	add r13, r15, r0
.LBB172_2:
	ldbu r3, r16+0
	jal r31, toupper
	stb r11+0, r1
	addi r13, r13, -1
	addi r11, r11, 1
	addi r16, r16, 1
	bne r13, r0, .LBB172_2
.LBB172_3:
	addi r1, sp, 248
	add r1, r1, r15
	stb r1+0, r0
	addi r1, r0, 6
	stw sp+108, r1
	beq r14, r1, .LBB172_6
.LBB172_4:
	addi r1, r0, 2
	beq r14, r1, .LBB172_7
.LBB172_5:
	bne r14, r17, .LBB172_8
.LBB172_6:
	addi r1, r0, 0
	sgt r1, r12, r1
	sub r1, r0, r1
	xori r3, r12, 4
	and r1, r3, r1
	xori fp, r1, 4
	jal r0, .LBB172_11
.LBB172_7:
	addi fp, r0, 4
	jal r0, .LBB172_11
.LBB172_8:
	addi r3, r14, -3
	bgeu r3, r1, .LBB172_10
.LBB172_9:
	addi fp, r0, 8
	jal r0, .LBB172_11
.LBB172_10:
	addi r1, r0, 5
	seq r1, r14, r1
	sub r1, r0, r1
	xori r3, r12, 16
	and r1, r3, r1
	xor fp, r12, r1
.LBB172_11:
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
	beq r1, r0, .LBB172_120
.LBB172_12:
	stw sp+52, r17
	lui r25, %hi(nml_status)
	ldw r1, r25+%lo(nml_status)
	bne r1, r0, .LBB172_183
.LBB172_13:
	lui r1, %hi(nml_nasg)
	stw sp+68, r1
	ldw r1, r1+%lo(nml_nasg)
	ldw r3, sp+52
	blt r1, r3, .LBB172_183
.LBB172_14:
	ldw r1, r25+%lo(nml_status)
	bne r1, r0, .LBB172_183
.LBB172_15:
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
	stw sp+32, r1
	addi r16, r0, 2
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+28, r1
	lui r1, %hi(.L.str.39)
	addi r1, r1, %lo(.L.str.39)
	stw sp+44, r1
	addi r1, r0, 95
	stw sp+40, r1
	lui r1, %hi(nml_err+95)
	stw sp+96, r1
	lui r1, %hi(nml_ext)
	addi r1, r1, %lo(nml_ext)
	stw sp+48, r1
	addi r1, r0, 0
	stw sp+36, r1
	add r4, r0, r0
.LBB172_16:
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
	beq r1, r0, .LBB172_20
.LBB172_17:
	addi r1, r0, 11
	ldw r4, sp+76
	beq r11, r1, .LBB172_183
.LBB172_18:
	addi r4, r4, 1
	ldw r1, sp+68
	ldw r1, r1+%lo(nml_nasg)
	bge r4, r1, .LBB172_183
.LBB172_19:
	ldw r1, r25+%lo(nml_status)
	ldw r3, sp+80
	addi r3, r3, 76
	stw sp+80, r3
	beq r1, r0, .LBB172_16
	jal r0, .LBB172_183
.LBB172_20:
	ldw r20, sp+52
	stw r18+72, r20
	ldw r1, r18+32
	blt r1, r20, .LBB172_31
.LBB172_21:
	ldw r3, sp+72
	ldw r3, r3+%lo(nml_ndim)
	bne r1, r3, .LBB172_32
.LBB172_22:
	ldw r1, r18+32
	blt r1, r20, .LBB172_35
.LBB172_23:
	ldw r1, sp+80
	add r11, r13, r1
	add r13, r0, r0
	add r17, r0, r0
	add r19, r20, r0
	add r15, r0, r0
	ldw r22, sp+32
	ldw r24, sp+28
	jal r0, .LBB172_26
.LBB172_24:
	mul r1, r1, r19
	add r15, r1, r15
	mul r19, r3, r19
	add r1, r20, r0
	beq r1, r0, .LBB172_36
.LBB172_25:
	addi r17, r17, 1
	ldw r1, r18+32
	addi r13, r13, 4
	bge r17, r1, .LBB172_36
.LBB172_26:
	add r1, r11, r13
	ldw r1, r1+0
	add r3, r13, r22
	ldw r3, r3+0
	sub r1, r1, r3
	blt r1, r0, .LBB172_28
.LBB172_27:
	ldw r3, sp+48
	add r3, r13, r3
	ldw r3, r3+0
	blt r1, r3, .LBB172_24
.LBB172_28:
	ldw r1, r25+%lo(nml_status)
	beq r1, r0, .LBB172_30
.LBB172_29:
	add r1, r0, r0
	bne r1, r0, .LBB172_25
	jal r0, .LBB172_36
.LBB172_30:
	stw r25+%lo(nml_status), r16
	add r3, r24, r0
	ldw r4, sp+44
	ldw r5, sp+40
	jal r31, strncpy
	ldw r1, sp+96
	stb r1+%lo(nml_err+95), r0
	add r1, r0, r0
	bne r1, r0, .LBB172_25
	jal r0, .LBB172_36
.LBB172_31:
	add r15, r0, r0
	jal r0, .LBB172_37
.LBB172_32:
	ldw r1, r25+%lo(nml_status)
	addi r11, r0, 11
	bne r1, r0, .LBB172_34
.LBB172_33:
	stw r25+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+96
	stb r1+%lo(nml_err+95), r0
.LBB172_34:
	add r1, r0, r0
	add r15, r0, r0
	beq r1, r0, .LBB172_17
	jal r0, .LBB172_37
.LBB172_35:
	add r15, r0, r0
.LBB172_36:
	ldw r1, r25+%lo(nml_status)
	ldw r3, sp+36
	seq r1, r1, r3
	addi r3, r1, -1
	andi r11, r3, 11
	beq r1, r0, .LBB172_17
.LBB172_37:
	ldw r1, r18+68
	addi r24, r0, 1
	blt r1, r24, .LBB172_119
.LBB172_38:
	ldw r1, r25+%lo(nml_status)
	add r11, r0, r0
	bne r1, r0, .LBB172_17
.LBB172_39:
	ldw r17, r18+64
.LBB172_40:
	lui r22, %hi(nml_val)
	ldw r1, r22+%lo(nml_val)
	addi r3, r0, 12
	mul r28, r17, r3
	add r3, r1, r28
	ldw r1, r3+8
	blt r1, r24, .LBB172_117
.LBB172_41:
	bge r15, r26, .LBB172_114
.LBB172_42:
	xor r1, r15, r26
	sgt r4, r15, r26
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r26, r1
	sub r27, r1, r15
	mul r1, fp, r15
	add r11, r23, r1
	add r26, r0, r0
.LBB172_43:
	ldw r1, r3+4
	blt r1, r24, .LBB172_109
.LBB172_44:
	mul r4, r15, fp
	add r19, r23, r4
	lui r4, %hi(nml_text)
	ldw r4, r4+%lo(nml_text)
	ldw r3, r3+0
	add r4, r4, r3
	addi r3, r0, 7
	beq r14, r3, .LBB172_55
.LBB172_45:
	ldw r3, sp+108
	bne r14, r3, .LBB172_67
.LBB172_46:
	ldbu r3, r4+0
	addi r5, r0, 46
	seq r3, r3, r5
	ble r1, r3, .LBB172_49
.LBB172_47:
	add r13, r4, r3
	ldbu r3, r13+0
	jal r31, toupper
	addi r4, r0, 84
	add r3, r24, r0
	beq r1, r4, .LBB172_52
.LBB172_48:
	ldbu r3, r13+0
	jal r31, toupper
	addi r3, r0, 70
	beq r1, r3, .LBB172_51
.LBB172_49:
	ldw r1, r25+%lo(nml_status)
	add r3, r0, r0
	bne r1, r0, .LBB172_52
.LBB172_50:
	stw r25+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.140)
	addi r4, r4, %lo(.L.str.140)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+96
	stb r1+%lo(nml_err+95), r0
.LBB172_51:
	add r3, r0, r0
.LBB172_52:
	beq r12, r16, .LBB172_80
.LBB172_53:
	bne r12, r24, .LBB172_81
.LBB172_54:
	stb r19+0, r3
	jal r0, .LBB172_109
.LBB172_55:
	ldbu r3, r4+0
	addi r5, r0, 34
	beq r3, r5, .LBB172_57
.LBB172_56:
	addi r5, r0, 39
	bne r3, r5, .LBB172_82
.LBB172_57:
	add r5, r0, r0
	addi r6, r0, 1
	bne r1, r6, .LBB172_62
.LBB172_58:
	ble r12, r5, .LBB172_109
.LBB172_59:
	add r1, r11, r5
	sub r3, r12, r5
.LBB172_60:
	addi r4, r0, 32
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB172_60
	jal r0, .LBB172_109
.LBB172_61:
	addi r6, r6, 1
	bge r6, r1, .LBB172_58
.LBB172_62:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB172_65
.LBB172_63:
	addi r6, r6, 1
	bge r6, r1, .LBB172_58
.LBB172_64:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB172_58
.LBB172_65:
	bge r5, r12, .LBB172_61
.LBB172_66:
	add r7, r4, r6
	ldbu r7, r7+0
	addi r8, r5, 1
	add r5, r19, r5
	stb r5+0, r7
	add r5, r8, r0
	jal r0, .LBB172_61
.LBB172_67:
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
	beq r1, r24, .LBB172_184
.LBB172_68:
	add r21, r13, r0
	stb r13+%lo(fio_busy), r24
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB172_72
.LBB172_69:
	addi r3, r0, -400
.LBB172_70:
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	add r4, r3, r4
	addi r5, r0, -1
	stw r4+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB172_70
.LBB172_71:
	stb r1+%lo(fio_uend_init), r24
.LBB172_72:
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
	beq r1, r0, .LBB172_75
.LBB172_73:
	stb r21+%lo(fio_busy), r0
	ldw r1, r25+%lo(nml_status)
	bne r1, r0, .LBB172_109
.LBB172_74:
	stw r25+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.142)
	addi r4, r4, %lo(.L.str.142)
	jal r0, .LBB172_84
.LBB172_75:
	addi r1, r14, -1
	bgtu r1, r20, .LBB172_92
.LBB172_76:
	slli r1, r1, 2
	lui r3, %hi(.LJTI172_0)
	addi r3, r3, %lo(.LJTI172_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB172_77:
	stw sp+184, r0
	addi r3, sp, 184
	jal r31, f77_rd_i
	ldw r1, sp+184
	beq r12, r16, .LBB172_111
.LBB172_78:
	bne r12, r24, .LBB172_112
.LBB172_79:
	stb r19+0, r1
	jal r0, .LBB172_100
.LBB172_80:
	sth r19+0, r3
	jal r0, .LBB172_109
.LBB172_81:
	stw r19+0, r3
	jal r0, .LBB172_109
.LBB172_82:
	ldw r1, r25+%lo(nml_status)
	bne r1, r0, .LBB172_109
.LBB172_83:
	stw r25+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.141)
	addi r4, r4, %lo(.L.str.141)
.LBB172_84:
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+96
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB172_109
.LBB172_85:
	add r3, r19, r0
	jal r0, .LBB172_99
.LBB172_86:
	ldw r1, sp+88
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB172_97
.LBB172_87:
	lui r20, %hi(fio_ldone)
	ldbu r1, r20+%lo(fio_ldone)
	bne r1, r0, .LBB172_100
.LBB172_88:
	jal r31, fio_list_tok
	beq r1, r16, .LBB172_100
.LBB172_89:
	beq r1, r24, .LBB172_96
.LBB172_90:
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
	jal r0, .LBB172_100
.LBB172_91:
	add r3, r19, r0
	jal r31, f77_rd_r
	jal r0, .LBB172_100
.LBB172_92:
	ldw r1, sp+88
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB172_98
.LBB172_93:
	lui r20, %hi(fio_ldone)
	ldbu r1, r20+%lo(fio_ldone)
	bne r1, r0, .LBB172_100
.LBB172_94:
	jal r31, fio_list_tok
	beq r1, r16, .LBB172_100
.LBB172_95:
	bne r1, r24, .LBB172_113
.LBB172_96:
	stb r20+%lo(fio_ldone), r24
	jal r0, .LBB172_100
.LBB172_97:
	add r3, r19, r0
	jal r31, f77_rd_r
	addi r3, r19, 4
	jal r31, f77_rd_r
	jal r0, .LBB172_100
.LBB172_98:
	add r3, r19, r0
	jal r31, f77_rd_d
	addi r3, r19, 8
.LBB172_99:
	jal r31, f77_rd_d
.LBB172_100:
	ldw r1, sp+100
	ldbu r1, r1+%lo(fio_noadv)
	bne r1, r24, .LBB172_108
.LBB172_101:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB172_105
.LBB172_102:
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
	bne r1, r0, .LBB172_104
.LBB172_103:
	addi r3, r0, 1024
	jal r31, malloc
	stw r19+0, r1
.LBB172_104:
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
.LBB172_105:
	lui r1, %hi(fio_sizep)
	ldw r3, r1+%lo(fio_sizep)
	beq r3, r0, .LBB172_107
.LBB172_106:
	lui r4, %hi(fio_size)
	ldw r4, r4+%lo(fio_size)
	stw r3+0, r4
.LBB172_107:
	ldw r3, sp+100
	stb r3+%lo(fio_noadv), r0
	stw r1+%lo(fio_sizep), r0
.LBB172_108:
	stb r21+%lo(fio_busy), r0
	lui r1, %hi(fio_reading)
	stb r1+%lo(fio_reading), r0
	ldw r1, sp+104
	stw r1+%lo(fio_ibuf), r0
.LBB172_109:
	addi r15, r15, 1
	addi r26, r26, 1
	ldw r1, r22+%lo(nml_val)
	add r3, r1, r28
	ldw r1, r3+8
	bge r26, r1, .LBB172_116
.LBB172_110:
	add r11, r11, fp
	bne r26, r27, .LBB172_43
	jal r0, .LBB172_114
.LBB172_111:
	sth r19+0, r1
	jal r0, .LBB172_100
.LBB172_112:
	stw r19+0, r1
	jal r0, .LBB172_100
.LBB172_113:
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
	jal r0, .LBB172_100
.LBB172_114:
	ldw r1, r25+%lo(nml_status)
	bne r1, r0, .LBB172_116
.LBB172_115:
	stw r25+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.40)
	addi r4, r4, %lo(.L.str.40)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+96
	stb r1+%lo(nml_err+95), r0
.LBB172_116:
	ldw r26, sp+92
.LBB172_117:
	addi r17, r17, 1
	ldw r1, r18+64
	ldw r3, r18+68
	add r1, r3, r1
	bge r17, r1, .LBB172_119
.LBB172_118:
	ldw r1, r25+%lo(nml_status)
	add r11, r0, r0
	beq r1, r0, .LBB172_40
	jal r0, .LBB172_17
.LBB172_119:
	add r11, r0, r0
	jal r0, .LBB172_17
.LBB172_120:
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
	blt r1, r0, .LBB172_125
.LBB172_121:
	addi r17, r1, 1
	stw r11+%lo(nml_npend), r17
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	add r1, r1, r3
	addi r3, r0, 44
	stb r1+0, r3
	ldw r4, r13+%lo(nml_len)
	addi r1, r0, 2
	blt r4, r1, .LBB172_124
.LBB172_122:
	add r1, r4, r17
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB172_124
.LBB172_123:
	add r3, r16, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r16+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
.LBB172_124:
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
.LBB172_125:
	add r22, r21, r0
	addi r15, r15, 1
	ldw r4, r13+%lo(nml_len)
	addi r11, r0, 2
	blt r4, r11, .LBB172_128
.LBB172_126:
	add r1, r4, r15
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB172_128
.LBB172_127:
	add r3, r16, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r16+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
.LBB172_128:
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
	blt r26, r1, .LBB172_183
.LBB172_129:
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
	lui r1, %hi(.LCPI172_0)
	addi r1, r1, %lo(.LCPI172_0)
	ldw r5, r1+4
	ldw r4, r1+0
	stw sp+80, r4
	stw sp+84, r5
	addi r1, r0, 32
	stw sp+68, r1
	addi r19, r0, 23
	lui r1, %hi(.L.str.137)
	addi r1, r1, %lo(.L.str.137)
	stw sp+64, r1
	lui r1, %hi(.L.str.138)
	addi r1, r1, %lo(.L.str.138)
	stw sp+52, r1
	addi r21, r0, 1
	addi r1, r0, -1
	stw sp+48, r1
	lui r1, %hi(fio_desc)
	stw sp+76, r1
	add r13, r23, r0
	add r20, r0, r0
	stw sp+60, r27
	jal r0, .LBB172_134
.LBB172_130:
	lui r16, %hi(fio_draining)
	stb r16+%lo(fio_draining), r24
	ldw r3, sp+76
	stw r3+%lo(fio_desc), r0
	stw r1+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r16+%lo(fio_draining), r0
.LBB172_131:
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
.LBB172_132:
	jal r31, nml_value
.LBB172_133:
	addi r20, r20, 1
	add r13, r13, fp
	beq r20, r26, .LBB172_183
.LBB172_134:
	mul r1, r20, fp
	add r1, r23, r1
	beq r14, r28, .LBB172_142
.LBB172_135:
	beq r14, r11, .LBB172_140
.LBB172_136:
	bne r14, r21, .LBB172_145
.LBB172_137:
	beq r12, r11, .LBB172_163
.LBB172_138:
	addi r3, r0, 1
	bne r12, r3, .LBB172_164
.LBB172_139:
	ldbu r25, r1+0
	jal r0, .LBB172_165
.LBB172_140:
	ldw r1, r1+0
	lui r3, %hi(.LCPI172_1)
	ldw r3, r3+%lo(.LCPI172_1)
	feq.s r3, r1, r3
	xori r3, r3, 1
	bne r3, r0, .LBB172_153
.LBB172_141:
	addi r3, sp, 184
	addi r4, r0, 32
	addi r5, r0, 15
	jal r31, memset
	lui r4, %hi(.L.str.137)
	addi r4, r4, %lo(.L.str.137)
	addi r5, r0, 3
	add r3, r15, r0
	jal r31, memcpy
	stb sp+199, r0
	jal r0, .LBB172_154
.LBB172_142:
	ldw r17, r1+4
	ldw r16, r1+0
	ldw r4, sp+80
	ldw r5, sp+84
	feq.d r1, r16, r4
	addi r3, sp, 184
	xori r1, r1, 1
	bne r1, r0, .LBB172_155
.LBB172_143:
	ldw r4, sp+68
	add r5, r19, r0
	jal r31, memset
	add r3, r15, r0
	ldw r4, sp+64
	add r5, r28, r0
	jal r31, memcpy
	stb sp+207, r0
.LBB172_144:
	addi r3, sp, 184
	add r4, r19, r0
	jal r0, .LBB172_132
.LBB172_145:
	addi r3, r0, 4
	ldw r4, sp+96
	bne r4, r3, .LBB172_149
.LBB172_146:
	bne r14, r27, .LBB172_168
.LBB172_147:
	ldw r7, r1+4
	ldw r6, r1+0
	beq r14, r27, .LBB172_169
.LBB172_148:
	ldw r1, r1+4
	fcvt.d.s r16, r1
	jal r0, .LBB172_170
.LBB172_149:
	ldw r3, sp+108
	bne r14, r3, .LBB172_173
.LBB172_150:
	beq r12, r11, .LBB172_180
.LBB172_151:
	addi r3, r0, 1
	bne r12, r3, .LBB172_181
.LBB172_152:
	ldbu r1, r1+0
	jal r0, .LBB172_182
.LBB172_153:
	stw sp+280, r1
	lui r5, %hi(.L.str.139)
	addi r5, r5, %lo(.L.str.139)
	addi r3, sp, 184
	addi r4, r0, 15
	addi r6, r0, 2
	addi r7, sp, 280
	jal r31, nml_fmt
.LBB172_154:
	addi r3, sp, 184
	addi r4, r0, 15
	jal r0, .LBB172_132
.LBB172_155:
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
	bne r1, r0, .LBB172_159
.LBB172_156:
	ldw r1, sp+76
	ldw r3, r1+%lo(fio_desc)
	lui r1, %hi(fio_rep)
	beq r3, r0, .LBB172_158
.LBB172_157:
	ldw r3, r1+%lo(fio_rep)
	bgt r3, r0, .LBB172_159
.LBB172_158:
	lui r16, %hi(fio_draining)
	addi r3, r0, 1
	stb r16+%lo(fio_draining), r3
	ldw r3, sp+76
	stw r3+%lo(fio_desc), r0
	stw r1+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r16+%lo(fio_draining), r0
.LBB172_159:
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
	jal r0, .LBB172_161
.LBB172_160:
	addi r1, r1, 1
	beq r1, r19, .LBB172_144
.LBB172_161:
	addi r3, sp, 184
	add r3, r3, r1
	ldbu r4, r3+0
	addi r5, r0, 69
	bne r4, r5, .LBB172_160
.LBB172_162:
	addi r4, r0, 68
	stb r3+0, r4
	jal r0, .LBB172_160
.LBB172_163:
	ldh r25, r1+0
	jal r0, .LBB172_165
.LBB172_164:
	ldw r25, r1+0
.LBB172_165:
	lui r6, %hi(.L.str.35)
	addi r6, r6, %lo(.L.str.35)
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
	bne r1, r0, .LBB172_131
.LBB172_166:
	ldw r1, sp+76
	ldw r3, r1+%lo(fio_desc)
	lui r1, %hi(fio_rep)
	beq r3, r0, .LBB172_130
.LBB172_167:
	ldw r3, r1+%lo(fio_rep)
	bgt r3, r0, .LBB172_131
	jal r0, .LBB172_130
.LBB172_168:
	ldw r3, r1+0
	fcvt.d.s r6, r3
	bne r14, r27, .LBB172_148
.LBB172_169:
	ldw r17, r1+12
	ldw r16, r1+8
.LBB172_170:
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
	blt r24, r26, .LBB172_172
.LBB172_171:
	addi r25, sp, 280
	add r3, r25, r24
	add r4, r25, r0
	jal r31, memmove
	addi r4, r0, 32
	add r3, r25, r0
	add r5, r24, r0
	jal r31, memset
	add r5, r16, r0
.LBB172_172:
	ldw r26, sp+92
	addi r3, sp, 280
	add r4, r5, r0
	jal r31, nml_value
	ldw r27, sp+60
	jal r0, .LBB172_133
.LBB172_173:
	addi r1, r0, 39
	stb sp+280, r1
	add r5, r21, r0
	blt r12, r21, .LBB172_179
.LBB172_174:
	addi r5, r0, 1
	add r3, r0, r0
	jal r0, .LBB172_177
.LBB172_175:
	add r5, r4, r0
	addi r3, r3, 1
	bge r3, r12, .LBB172_179
.LBB172_176:
	addi r4, r0, 1021
	bge r5, r4, .LBB172_179
.LBB172_177:
	add r4, r13, r3
	ldbu r6, r4+0
	addi r4, r5, 1
	addi r7, sp, 280
	add r7, r7, r5
	stb r7+0, r6
	bne r6, r1, .LBB172_175
.LBB172_178:
	addi r5, r5, 2
	addi r6, sp, 280
	add r4, r6, r4
	stb r4+0, r1
	addi r3, r3, 1
	blt r3, r12, .LBB172_176
.LBB172_179:
	addi r4, r5, 1
	addi r3, sp, 280
	add r5, r3, r5
	stb r5+0, r1
	jal r0, .LBB172_132
.LBB172_180:
	ldh r1, r1+0
	jal r0, .LBB172_182
.LBB172_181:
	ldw r1, r1+0
.LBB172_182:
	addi r3, r0, 0
	seq r1, r1, r3
	lui r3, %hi(.L.str.36)
	addi r3, r3, %lo(.L.str.36)
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r3, r3, r1
	addi r4, r0, 1
	jal r0, .LBB172_132
.LBB172_183:
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
.LBB172_184:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end172:
	.size	f77_nml_item, .Lfunc_end172-f77_nml_item
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI172_0,@object
.LJTI172_0:
	.word	.LBB172_77
	.word	.LBB172_91
	.word	.LBB172_85
	.word	.LBB172_86
	.size	.LJTI172_0, 16
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
	beq r15, r1, .LBB173_3
.LBB173_1:
	bne r15, r13, .LBB173_5
.LBB173_2:
	ldw r3, r14+0
	jal r31, f77_wr_i
	jal r0, .LBB173_6
.LBB173_3:
	ldw r14, r14+0
	stw sp+20, r14
	ldw r1, r16+%lo(fio_listed)
	beq r1, r0, .LBB173_11
.LBB173_4:
	fcvt.d.s r4, r14
	add r3, r4, r0
	add r4, r5, r0
	add r5, r0, r0
	jal r31, fio_list_out_real
	jal r0, .LBB173_6
.LBB173_5:
	ldw r4, r14+4
	ldw r3, r14+0
	jal r31, f77_wr_d
.LBB173_6:
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	ldw r1, r16+%lo(fio_listed)
	bne r1, r0, .LBB173_10
.LBB173_7:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB173_9
.LBB173_8:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB173_10
.LBB173_9:
	lui r14, %hi(fio_draining)
	stb r14+%lo(fio_draining), r13
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r14+%lo(fio_draining), r0
.LBB173_10:
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
.LBB173_11:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB173_15
.LBB173_12:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB173_14
.LBB173_13:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	bgt r4, r0, .LBB173_15
.LBB173_14:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
.LBB173_15:
	addi r3, r0, 65
	bne r1, r3, .LBB173_17
.LBB173_16:
	addi r3, sp, 20
	addi r4, r0, 4
	jal r31, fio_hol_out
	jal r0, .LBB173_6
.LBB173_17:
	lui r15, %hi(fio_single)
	stb r15+%lo(fio_single), r13
	fcvt.d.s r4, r14
	add r3, r4, r0
	add r4, r5, r0
	jal r31, f77_wr_d
	stb r15+%lo(fio_single), r0
	jal r0, .LBB173_6
.Lfunc_end173:
	.size	nml_fmt, .Lfunc_end173-nml_fmt
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
	blt r1, r0, .LBB174_5
.LBB174_1:
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
	blt r4, r1, .LBB174_4
.LBB174_2:
	add r1, r4, r14
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB174_4
.LBB174_3:
	add r3, r13, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r13+0, r1
	addi r1, r0, 1
	stw r16+%lo(nml_len), r1
.LBB174_4:
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
.LBB174_5:
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
.Lfunc_end174:
	.size	nml_value, .Lfunc_end174-nml_value
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function nml_real
	.type	.LCPI175_0,@object
.LCPI175_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI175_0, 8
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
	lui r1, %hi(.LCPI175_0)
	addi r1, r1, %lo(.LCPI175_0)
	ldw r9, r1+4
	ldw r8, r1+0
	feq.d r1, r6, r8
	xori r1, r1, 1
	bne r1, r0, .LBB175_3
.LBB175_1:
	addi r4, r0, 32
	add r3, r11, r0
	add r5, r12, r0
	jal r31, memset
	addi r3, r11, 1
	lui r4, %hi(.L.str.137)
	addi r4, r4, %lo(.L.str.137)
	addi r5, r0, 3
	jal r31, memcpy
	add r1, r11, r12
	stb r1+0, r0
.LBB175_2:
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB175_3:
	beq r4, r0, .LBB175_8
.LBB175_4:
	lui r5, %hi(.L.str.138)
	addi r5, r5, %lo(.L.str.138)
	addi r6, r0, 3
	addi r7, sp, 32
	add r3, r11, r0
	add r4, r12, r0
	jal r31, nml_fmt
	addi r1, r0, 69
	addi r3, r0, 68
	jal r0, .LBB175_6
.LBB175_5:
	addi r12, r12, -1
	addi r11, r11, 1
	beq r12, r0, .LBB175_2
.LBB175_6:
	ldbu r4, r11+0
	bne r4, r1, .LBB175_5
.LBB175_7:
	stb r11+0, r3
	jal r0, .LBB175_5
.LBB175_8:
	fcvt.s.d r1, r6
	stw sp+28, r1
	lui r5, %hi(.L.str.139)
	addi r5, r5, %lo(.L.str.139)
	addi r6, r0, 2
	addi r7, sp, 28
	add r3, r11, r0
	add r4, r12, r0
	jal r31, nml_fmt
	jal r0, .LBB175_2
.Lfunc_end175:
	.size	nml_real, .Lfunc_end175-nml_real
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
	blt r13, r1, .LBB176_4
.LBB176_1:
	add r1, r14, r16
	ldbu r1, r1+0
	bne r1, r17, .LBB176_4
.LBB176_2:
	addi r16, r16, 1
	bne r13, r16, .LBB176_1
.LBB176_3:
	add r16, r13, r0
.LBB176_4:
	sub r1, r13, r16
	addi r3, r0, 3
	add r15, r14, r16
	blt r1, r3, .LBB176_6
.LBB176_5:
	lui r4, %hi(.L.str.137)
	addi r4, r4, %lo(.L.str.137)
	addi r5, r0, 3
	add r3, r15, r0
	jal r31, memcmp
	beq r1, r0, .LBB176_11
.LBB176_6:
	ble r13, r16, .LBB176_12
.LBB176_7:
	addi r1, r13, -1
	xor r3, r16, r1
	slt r4, r16, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	addi r3, r14, -1
.LBB176_8:
	add r4, r3, r13
	ldbu r4, r4+0
	bne r4, r17, .LBB176_12
.LBB176_9:
	addi r13, r13, -1
	bgt r13, r16, .LBB176_8
.LBB176_10:
	add r13, r1, r0
	jal r0, .LBB176_12
.LBB176_11:
	addi r1, r16, 3
	bne r1, r13, .LBB176_13
.LBB176_12:
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
.LBB176_13:
	add r1, r14, r1
	ldbu r1, r1+0
	bne r1, r17, .LBB176_6
	jal r0, .LBB176_12
.Lfunc_end176:
	.size	nml_trim_cat, .Lfunc_end176-nml_trim_cat
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
	beq r1, r0, .LBB177_6
.LBB177_1:
	lui r11, %hi(nml_status)
	ldw r1, r11+%lo(nml_status)
	beq r1, r0, .LBB177_14
.LBB177_2:
	ldw r1, r11+%lo(nml_status)
	addi r3, r0, 2
	beq r1, r3, .LBB177_18
.LBB177_3:
	addi r3, r0, 1
	bne r1, r3, .LBB177_19
.LBB177_4:
	lui r1, %hi(nml_flags)
	ldbu r1, r1+%lo(nml_flags)
	andi r4, r1, 1
	add r1, r3, r0
	bne r4, r0, .LBB177_19
.LBB177_5:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r1, %hi(nml_unit)
	ldw r5, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.43)
	addi r4, r4, %lo(.L.str.43)
	lui r6, %hi(nml_grp)
	addi r6, r6, %lo(nml_grp)
	jal r0, .LBB177_23
.LBB177_6:
	lui r14, %hi(nml_npend)
	ldw r12, r14+%lo(nml_npend)
	lui r13, %hi(nml_len)
	lui r11, %hi(nml_line)
	addi r11, r11, %lo(nml_line)
	blt r12, r0, .LBB177_11
.LBB177_7:
	ldw r4, r13+%lo(nml_len)
	addi r1, r0, 2
	blt r4, r1, .LBB177_10
.LBB177_8:
	add r1, r4, r12
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB177_10
.LBB177_9:
	add r3, r11, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r11+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
.LBB177_10:
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
.LBB177_11:
	ldw r4, r13+%lo(nml_len)
	addi r1, r0, 2
	blt r4, r1, .LBB177_13
.LBB177_12:
	lui r3, %hi(nml_line)
	addi r3, r3, %lo(nml_line)
	jal r31, nml_record
.LBB177_13:
	addi r1, r0, 32
	stb r11+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
	lui r3, %hi(.L.str.41)
	addi r3, r3, %lo(.L.str.41)
	addi r4, r0, 5
	jal r31, nml_record
	add r1, r0, r0
	jal r0, .LBB177_19
.LBB177_14:
	lui r1, %hi(nml_nasg)
	ldw r1, r1+%lo(nml_nasg)
	addi r3, r0, 1
	blt r1, r3, .LBB177_2
.LBB177_15:
	lui r3, %hi(nml_asg)
	ldw r5, r3+%lo(nml_asg)
.LBB177_16:
	ldw r3, r5+72
	beq r3, r0, .LBB177_20
.LBB177_17:
	addi r5, r5, 76
	addi r1, r1, -1
	bne r1, r0, .LBB177_16
	jal r0, .LBB177_2
.LBB177_18:
	lui r3, %hi(nml_flags)
	ldbu r3, r3+%lo(nml_flags)
	andi r3, r3, 2
	beq r3, r0, .LBB177_22
.LBB177_19:
	ldw r14, sp+112
	ldw r13, sp+116
	ldw r12, sp+120
	ldw r11, sp+124
	ldw lr, sp+0
	addi sp, sp, 128
	jalr r0, r31, 0
.LBB177_20:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r3, sp, 16
	jal r31, sprintf
	ldw r1, r11+%lo(nml_status)
	bne r1, r0, .LBB177_2
.LBB177_21:
	addi r1, r0, 2
	stw r11+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	addi r4, sp, 16
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB177_2
.LBB177_22:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r1, %hi(nml_unit)
	ldw r6, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	lui r5, %hi(nml_grp)
	addi r5, r5, %lo(nml_grp)
	lui r7, %hi(nml_err)
	addi r7, r7, %lo(nml_err)
.LBB177_23:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end177:
	.size	f77_nml_end, .Lfunc_end177-f77_nml_end
                                        # -- End function
	.globl	f77_ipow                        # -- Begin function f77_ipow
	.p2align	2
	.type	f77_ipow,@function
f77_ipow:                               # @f77_ipow
# %bb.0:
	addi r1, r0, 1
	blt r4, r0, .LBB178_4
.LBB178_1:
	beq r4, r0, .LBB178_9
.LBB178_2:
	addi r5, r0, 0
.LBB178_3:
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
	bne r4, r0, .LBB178_3
	jal r0, .LBB178_9
.LBB178_4:
	beq r3, r1, .LBB178_7
.LBB178_5:
	addi r1, r0, -1
	bne r3, r1, .LBB178_8
.LBB178_6:
	andi r1, r4, 1
	addi r3, r0, 0
	seq r1, r1, r3
	addi r1, r1, -1
	ori  r1, r1, 1
	jal r0, .LBB178_9
.LBB178_7:
	add r1, r3, r0
	jal r0, .LBB178_9
.LBB178_8:
	add r1, r0, r0
.LBB178_9:
	jalr r0, r31, 0
.Lfunc_end178:
	.size	f77_ipow, .Lfunc_end178-f77_ipow
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_dpow_i
	.type	.LCPI179_0,@object
.LCPI179_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI179_0, 8
	.type	.LCPI179_1,@object
.LCPI179_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI179_1, 8
	.type	.LCPI179_2,@object
.LCPI179_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI179_2, 8
	.type	.LCPI179_3,@object
.LCPI179_3:
	.quad	0x7fefdcf158adbb99              # double 1.79E+308
	.size	.LCPI179_3, 8
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
	blt r5, r0, .LBB179_3
.LBB179_1:
	andi r3, r5, 1
	addi r1, r0, 0
	seq r3, r3, r1
	lui r4, %hi(.LCPI179_0)
	addi r4, r4, %lo(.LCPI179_0)
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
	beq r3, r0, .LBB179_16
.LBB179_2:
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
	bne r3, r0, .LBB179_2
	jal r0, .LBB179_16
.LBB179_3:
	sub r1, r0, r5
	lui r3, %hi(.LCPI179_0)
	addi r3, r3, %lo(.LCPI179_0)
	ldw r5, r3+4
	ldw r4, r3+0
	lui r3, %hi(.LCPI179_1)
	addi r3, r3, %lo(.LCPI179_1)
	ldw r9, r3+4
	ldw r8, r3+0
	lui r3, %hi(.LCPI179_3)
	addi r3, r3, %lo(.LCPI179_3)
	ldw r11, r3+4
	ldw r10, r3+0
	lui r3, %hi(.LCPI179_2)
	addi r3, r3, %lo(.LCPI179_2)
	ldw r15, r3+4
	ldw r14, r3+0
	add r16, r6, r0
	add r17, r7, r0
	add r3, r1, r0
	add r12, r4, r0
	add r13, r5, r0
	jal r0, .LBB179_5
.LBB179_4:
	fmul.d r16, r16, r16
.LBB179_5:
	andi lr, r3, 1
	fneg.d r18, r16
	beq lr, r0, .LBB179_10
.LBB179_6:
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
	bne lr, r0, .LBB179_9
.LBB179_7:
	fle.d lr, r20, r4
	bne lr, r0, .LBB179_9
.LBB179_8:
	fle.d lr, r20, r14
	xori lr, lr, 1
	fle.d r20, r22, r14
	xori r20, r20, 1
	or  lr, r20, lr
	bne lr, r0, .LBB179_13
.LBB179_9:
	fmul.d r12, r16, r12
.LBB179_10:
	srli r3, r3, 1
	beq r3, r0, .LBB179_15
.LBB179_11:
	flt.d lr, r16, r8
	xor r20, r19, r17
	sub lr, r0, lr
	and r20, r20, lr
	xor r21, r17, r20
	xor r18, r18, r16
	and lr, r18, lr
	xor r20, r16, lr
	fle.d lr, r20, r14
	bne lr, r0, .LBB179_4
.LBB179_12:
	fdiv.d r18, r10, r20
	fle.d lr, r20, r18
	xori lr, lr, 1
	beq lr, r0, .LBB179_4
.LBB179_13:
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
	beq r1, r0, .LBB179_16
.LBB179_14:
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
	bne r1, r0, .LBB179_14
	jal r0, .LBB179_16
.LBB179_15:
	fdiv.d r8, r4, r12
.LBB179_16:
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
.Lfunc_end179:
	.size	f77_dpow_i, .Lfunc_end179-f77_dpow_i
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rpow_i
	.type	.LCPI180_0,@object
.LCPI180_0:
	.word	0x3f800000                      # float 1
	.size	.LCPI180_0, 4
	.type	.LCPI180_1,@object
.LCPI180_1:
	.word	0x00000000                      # float 0
	.size	.LCPI180_1, 4
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0
	.type	.LCPI180_2,@object
.LCPI180_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI180_2, 8
	.type	.LCPI180_3,@object
.LCPI180_3:
	.quad	0x47eff933c78cdfad              # double 3.4E+38
	.size	.LCPI180_3, 8
	.type	.LCPI180_4,@object
.LCPI180_4:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI180_4, 8
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
	blt r4, r0, .LBB180_3
.LBB180_1:
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
	srli r4, r4, 1
	beq r4, r0, .LBB180_17
.LBB180_2:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	srli r4, r4, 1
	bne r4, r0, .LBB180_2
	jal r0, .LBB180_17
.LBB180_3:
	sub r4, r0, r4
	lui r1, %hi(.LCPI180_0)
	ldw r5, r1+%lo(.LCPI180_0)
	lui r6, %hi(.LCPI180_1)
	ldw r6, r6+%lo(.LCPI180_1)
	lui r7, %hi(.LCPI180_3)
	addi r7, r7, %lo(.LCPI180_3)
	ldw r9, r7+4
	ldw r8, r7+0
	lui r7, %hi(.LCPI180_2)
	addi r7, r7, %lo(.LCPI180_2)
	ldw r11, r7+4
	ldw r10, r7+0
	add r7, r3, r0
	add lr, r4, r0
	jal r0, .LBB180_5
.LBB180_4:
	fmul.s r7, r7, r7
.LBB180_5:
	andi r15, lr, 1
	fcvt.d.s r12, r7
	flt.s r14, r7, r6
	beq r15, r0, .LBB180_10
.LBB180_6:
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
	bne r15, r0, .LBB180_9
.LBB180_7:
	lui r15, %hi(.LCPI180_4)
	addi r15, r15, %lo(.LCPI180_4)
	ldw r21, r15+4
	ldw r20, r15+0
	fle.d r15, r16, r20
	bne r15, r0, .LBB180_9
.LBB180_8:
	fle.d r15, r16, r10
	xori r15, r15, 1
	fle.d r16, r18, r10
	xori r16, r16, 1
	or  r15, r16, r15
	bne r15, r0, .LBB180_13
.LBB180_9:
	fmul.s r5, r7, r5
.LBB180_10:
	srli lr, lr, 1
	beq lr, r0, .LBB180_16
.LBB180_11:
	fneg.d r16, r12
	xor r15, r17, r13
	sub r18, r0, r14
	and r14, r15, r18
	xor r15, r13, r14
	xor r16, r16, r12
	and r16, r16, r18
	xor r14, r12, r16
	fle.d r12, r14, r10
	bne r12, r0, .LBB180_4
.LBB180_12:
	fdiv.d r12, r8, r14
	fle.d r12, r14, r12
	xori r12, r12, 1
	beq r12, r0, .LBB180_4
.LBB180_13:
	ldw r1, r1+%lo(.LCPI180_0)
	fdiv.s r3, r1, r3
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
.LBB180_14:
	srli r4, r4, 1
	beq r4, r0, .LBB180_17
.LBB180_15:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	jal r0, .LBB180_14
.LBB180_16:
	ldw r1, r1+%lo(.LCPI180_0)
	fdiv.s r1, r1, r5
.LBB180_17:
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
.Lfunc_end180:
	.size	f77_rpow_i, .Lfunc_end180-f77_rpow_i
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
.Lfunc_end181:
	.size	f77_open_f90, .Lfunc_end181-f77_open_f90
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
	lui r1, %hi(fio_uend_init)
	ldbu lr, r1+%lo(fio_uend_init)
	bne lr, r0, .LBB182_4
.LBB182_1:
	addi lr, r0, -400
	lui r11, %hi(fio_uend+400)
	addi r11, r11, %lo(fio_uend+400)
	addi r12, r0, -1
.LBB182_2:
	add r13, lr, r11
	stw r13+0, r12
	addi lr, lr, 4
	bne lr, r0, .LBB182_2
.LBB182_3:
	addi lr, r0, 1
	stb r1+%lo(fio_uend_init), lr
.LBB182_4:
	ldw r23, sp+368
	addi r1, r3, -100
	addi lr, r0, -99
	bltu r1, lr, .LBB182_21
.LBB182_5:
	addi r1, r3, -5
	addi r25, r0, 1
	bleu r1, r25, .LBB182_21
.LBB182_6:
	beq r8, r0, .LBB182_13
.LBB182_7:
	addi r1, r0, 1
	add r15, r0, r0
	blt r9, r1, .LBB182_14
.LBB182_8:
	addi lr, r0, 6
	lui r11, %hi(.L.str.46)
	addi r11, r11, %lo(.L.str.46)
	addi r12, r0, 26
	add r13, r0, r0
.LBB182_9:
	beq r13, lr, .LBB182_23
.LBB182_10:
	add r14, r13, r11
	ldb r14, r14+0
	add r15, r8, r13
	ldb r15, r15+0
	addi r16, r15, -97
	andi r16, r16, 255
	sltu r16, r16, r12
	addi r17, r15, -32
	xor r17, r17, r15
	sub r16, r0, r16
	and r16, r17, r16
	xor r15, r15, r16
	bne r15, r14, .LBB182_13
.LBB182_11:
	addi r13, r13, 1
	bne r9, r13, .LBB182_9
.LBB182_12:
	add r11, r9, r0
	beq r11, lr, .LBB182_24
.LBB182_13:
	add r15, r0, r0
.LBB182_14:
	add r16, r15, r0
	beq r10, r0, .LBB182_35
.LBB182_15:
	ldw r8, sp+352
	addi r1, r0, 1
	add r16, r15, r0
	blt r8, r1, .LBB182_35
.LBB182_16:
	addi r9, r0, 11
	lui lr, %hi(.L.str.47)
	addi lr, lr, %lo(.L.str.47)
	addi r11, r0, 26
	add r12, r0, r0
.LBB182_17:
	beq r12, r9, .LBB182_28
.LBB182_18:
	add r13, r12, lr
	ldb r13, r13+0
	add r14, r10, r12
	ldb r14, r14+0
	addi r16, r14, -97
	andi r16, r16, 255
	sltu r16, r16, r11
	addi r17, r14, -32
	xor r17, r17, r14
	sub r16, r0, r16
	and r16, r17, r16
	xor r14, r14, r16
	bne r14, r13, .LBB182_34
.LBB182_19:
	addi r12, r12, 1
	bne r8, r12, .LBB182_17
.LBB182_20:
	add lr, r8, r0
	beq lr, r9, .LBB182_29
	jal r0, .LBB182_34
.LBB182_21:
	bne r23, r0, .LBB182_44
.LBB182_22:
	lui r1, %hi(stderr)
	ldw r1, r1+%lo(stderr)
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	add r5, r3, r0
	add r3, r1, r0
	jal r0, .LBB182_212
.LBB182_23:
	add r11, lr, r0
	bne r11, lr, .LBB182_13
.LBB182_24:
	addi lr, r0, 7
	add r15, r1, r0
	blt r9, lr, .LBB182_14
.LBB182_25:
	addi lr, r9, -7
	addi r1, r8, 6
	addi r8, r0, 32
	addi r9, r0, 1
.LBB182_26:
	ldbu r11, r1+0
	seq r15, r11, r8
	addi r12, lr, -1
	sltu lr, r12, lr
	bne lr, r9, .LBB182_14
.LBB182_27:
	addi r1, r1, 1
	add lr, r12, r0
	beq r11, r8, .LBB182_26
	jal r0, .LBB182_14
.LBB182_28:
	add lr, r9, r0
	bne lr, r9, .LBB182_34
.LBB182_29:
	addi r9, r0, 12
	blt r8, r9, .LBB182_33
.LBB182_30:
	addi r9, r10, 11
	addi r8, r8, -11
	addi r10, r0, 32
.LBB182_31:
	ldbu lr, r9+0
	bne lr, r10, .LBB182_34
.LBB182_32:
	addi r9, r9, 1
	addi r8, r8, -1
	bne r8, r0, .LBB182_31
.LBB182_33:
	add r16, r1, r0
	jal r0, .LBB182_35
.LBB182_34:
	add r16, r0, r0
.LBB182_35:
	ldw r17, sp+364
	addi r24, r0, 0
	sne r1, r6, r24
	sgt r8, r7, r24
	and r26, r1, r8
	bne r26, r25, .LBB182_41
.LBB182_36:
	addi r1, r0, 7
	lui r8, %hi(.L.str.48)
	addi r8, r8, %lo(.L.str.48)
	addi r9, r0, 26
	add r10, r0, r0
.LBB182_37:
	beq r10, r1, .LBB182_59
.LBB182_38:
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
	bne r11, lr, .LBB182_41
.LBB182_39:
	addi r10, r10, 1
	bne r7, r10, .LBB182_37
.LBB182_40:
	add r8, r7, r0
	beq r8, r1, .LBB182_60
.LBB182_41:
	add r20, r0, r0
.LBB182_42:
	sgt r1, r17, r24
	xori r8, r15, 1
	or  r1, r1, r8
	bne r1, r0, .LBB182_46
.LBB182_43:
	beq r23, r0, .LBB182_213
.LBB182_44:
	addi r1, r0, 2
.LBB182_45:
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
.LBB182_46:
	beq r4, r0, .LBB182_51
.LBB182_47:
	addi r18, r0, 1
	blt r5, r18, .LBB182_51
.LBB182_48:
	addi r1, r0, 256
	blt r5, r1, .LBB182_70
.LBB182_49:
	bne r23, r0, .LBB182_44
.LBB182_50:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.50)
	addi r4, r4, %lo(.L.str.50)
	jal r0, .LBB182_212
.LBB182_51:
	add r11, r6, r0
	add r12, r7, r0
	lui r5, %hi(.L.str.51)
	addi r5, r5, %lo(.L.str.51)
	addi r1, sp, 28
	addi r4, r0, 256
	add r6, r3, r0
	add r3, r1, r0
	add r21, r6, r0
	jal r31, snprintf
.LBB182_52:
	ldw r18, sp+360
	ldw r19, sp+356
	slli r14, r21, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r13, r14, r1
	ldw r1, r13+0
	slli r21, r21, 8
	lui r22, %hi(fio_ufname)
	addi r22, r22, %lo(fio_ufname)
	beq r1, r0, .LBB182_55
.LBB182_53:
	add r3, r21, r22
	addi r4, sp, 28
	jal r31, strcmp
	sne r1, r1, r24
	or  r1, r20, r1
	beq r1, r0, .LBB182_73
.LBB182_54:
	ldw r3, r13+0
	jal r31, fclose
	stw r13+0, r0
.LBB182_55:
	beq r20, r0, .LBB182_64
.LBB182_56:
	jal r31, tmpfile
	bne r1, r0, .LBB182_113
.LBB182_57:
	bne r23, r0, .LBB182_44
.LBB182_58:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	jal r0, .LBB182_212
.LBB182_59:
	add r8, r1, r0
	bne r8, r1, .LBB182_41
.LBB182_60:
	addi r1, r0, 8
	blt r7, r1, .LBB182_80
.LBB182_61:
	addi r10, r7, -8
	addi r1, r6, 7
	addi r8, r0, 32
	addi r9, r0, 1
.LBB182_62:
	ldbu lr, r1+0
	seq r20, lr, r8
	addi r11, r10, -1
	sltu r10, r11, r10
	bne r10, r9, .LBB182_42
.LBB182_63:
	addi r1, r1, 1
	add r10, r11, r0
	beq lr, r8, .LBB182_62
	jal r0, .LBB182_42
.LBB182_64:
	beq r26, r0, .LBB182_107
.LBB182_65:
	addi r1, r0, 3
	lui r5, %hi(.L.str.54)
	addi r5, r5, %lo(.L.str.54)
	addi r6, r0, 26
	add r7, r0, r0
	add r4, r12, r0
	add r3, r11, r0
.LBB182_66:
	beq r7, r1, .LBB182_81
.LBB182_67:
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
	bne r9, r8, .LBB182_92
.LBB182_68:
	addi r7, r7, 1
	bne r4, r7, .LBB182_66
.LBB182_69:
	add r5, r4, r0
	jal r0, .LBB182_82
.LBB182_70:
	add r21, r3, r0
	add r11, r6, r0
	add r12, r7, r0
	addi r14, sp, 28
	add r3, r14, r0
	add r13, r5, r0
	jal r31, memcpy
	add r1, r14, r13
	stb r1+0, r0
	addi r1, r14, -1
	addi r3, r0, 32
.LBB182_71:
	add r5, r1, r13
	ldbu r6, r5+0
	bne r6, r3, .LBB182_52
.LBB182_72:
	add r4, r13, r0
	addi r13, r13, -1
	stb r5+0, r0
	bgt r4, r18, .LBB182_71
	jal r0, .LBB182_52
.LBB182_73:
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r14, r1
	stw r1+0, r15
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r14, r1
	stw r1+0, r16
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r14, r1
	stw r1+0, r17
	beq r19, r0, .LBB182_207
.LBB182_74:
	add r3, r0, r0
	blt r18, r25, .LBB182_208
.LBB182_75:
	addi r1, r0, 4
	lui r3, %hi(.L.str.52)
	addi r3, r3, %lo(.L.str.52)
	addi r4, r0, 26
	add r5, r0, r0
.LBB182_76:
	beq r5, r1, .LBB182_96
.LBB182_77:
	add r6, r5, r3
	ldb r6, r6+0
	add r7, r19, r5
	ldb r7, r7+0
	addi r8, r7, -97
	andi r8, r8, 255
	sltu r8, r8, r4
	addi r9, r7, -32
	xor r9, r9, r7
	sub r8, r0, r8
	and r8, r9, r8
	xor r7, r7, r8
	bne r7, r6, .LBB182_207
.LBB182_78:
	addi r5, r5, 1
	bne r18, r5, .LBB182_76
.LBB182_79:
	add r3, r18, r0
	jal r0, .LBB182_97
.LBB182_80:
	addi r20, r0, 1
	jal r0, .LBB182_42
.LBB182_81:
	add r5, r1, r0
.LBB182_82:
	bne r5, r1, .LBB182_92
.LBB182_83:
	addi r1, r0, 4
	blt r4, r1, .LBB182_87
.LBB182_84:
	addi r1, r3, 3
	addi r5, r4, -3
	addi r6, r0, 32
.LBB182_85:
	ldbu r7, r1+0
	bne r7, r6, .LBB182_92
.LBB182_86:
	addi r1, r1, 1
	addi r5, r5, -1
	bne r5, r0, .LBB182_85
.LBB182_87:
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	addi r3, sp, 28
	jal r31, fopen
	bne r1, r0, .LBB182_89
.LBB182_88:
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	addi r3, sp, 28
	jal r31, fopen
.LBB182_89:
	bne r1, r0, .LBB182_113
.LBB182_90:
	bne r23, r0, .LBB182_44
.LBB182_91:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.57)
	addi r4, r4, %lo(.L.str.57)
	jal r0, .LBB182_146
.LBB182_92:
	lui r5, %hi(.L.str.58)
	addi r5, r5, %lo(.L.str.58)
	add r11, r3, r0
	add r12, r4, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB182_103
.LBB182_93:
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	addi r3, sp, 28
	jal r31, fopen
	beq r1, r0, .LBB182_104
.LBB182_94:
	add r3, r1, r0
	jal r31, fclose
	bne r23, r0, .LBB182_44
.LBB182_95:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.59)
	addi r4, r4, %lo(.L.str.59)
	jal r0, .LBB182_146
.LBB182_96:
	add r3, r1, r0
.LBB182_97:
	bne r3, r1, .LBB182_207
.LBB182_98:
	addi r1, r0, 5
	blt r18, r1, .LBB182_102
.LBB182_99:
	addi r1, r19, 4
	addi r3, r18, -4
	addi r4, r0, 32
.LBB182_100:
	ldbu r5, r1+0
	bne r5, r4, .LBB182_207
.LBB182_101:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB182_100
.LBB182_102:
	add r3, r25, r0
	jal r0, .LBB182_208
.LBB182_103:
	lui r5, %hi(.L.str.62)
	addi r5, r5, %lo(.L.str.62)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB182_107
.LBB182_104:
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	addi r3, sp, 28
	jal r31, fopen
	bne r1, r0, .LBB182_113
.LBB182_105:
	bne r23, r0, .LBB182_44
.LBB182_106:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.61)
	addi r4, r4, %lo(.L.str.61)
	jal r0, .LBB182_146
.LBB182_107:
	beq r11, r0, .LBB182_110
.LBB182_108:
	addi r1, r0, 1
	blt r12, r1, .LBB182_110
.LBB182_109:
	lui r5, %hi(.L.str.63)
	addi r5, r5, %lo(.L.str.63)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB182_209
.LBB182_110:
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	addi r3, sp, 28
	jal r31, fopen
	bne r1, r0, .LBB182_112
.LBB182_111:
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	addi r3, sp, 28
	jal r31, fopen
.LBB182_112:
	beq r1, r0, .LBB182_144
.LBB182_113:
	stw r13+0, r1
	lui r3, %hi(fio_udelim)
	addi r3, r3, %lo(fio_udelim)
	add r25, r14, r3
	stw r25+0, r0
	lui r3, %hi(fio_uaction)
	addi r3, r3, %lo(fio_uaction)
	add r11, r14, r3
	stw r11+0, r0
	lui r3, %hi(fio_uposn)
	addi r3, r3, %lo(fio_uposn)
	add r27, r14, r3
	stw r27+0, r0
	lui r23, %hi(fio_o_act)
	ldw r3, r23+%lo(fio_o_act)
	lui r26, %hi(fio_o_actlen)
	beq r3, r0, .LBB182_128
.LBB182_114:
	ldw r4, r26+%lo(fio_o_actlen)
	addi r5, r0, 1
	blt r4, r5, .LBB182_128
.LBB182_115:
	addi r6, r0, 4
	lui r7, %hi(.L.str.66)
	addi r7, r7, %lo(.L.str.66)
	addi r8, r0, 26
	add r9, r0, r0
.LBB182_116:
	beq r9, r6, .LBB182_120
.LBB182_117:
	add r10, r9, r7
	ldb r10, r10+0
	add lr, r3, r9
	ldb lr, lr+0
	addi r12, lr, -97
	andi r12, r12, 255
	sltu r12, r12, r8
	addi r13, lr, -32
	xor r13, r13, lr
	sub r12, r0, r12
	and r12, r13, r12
	xor lr, lr, r12
	bne lr, r10, .LBB182_126
.LBB182_118:
	addi r9, r9, 1
	bne r4, r9, .LBB182_116
.LBB182_119:
	add r7, r4, r0
	jal r0, .LBB182_121
.LBB182_120:
	add r7, r6, r0
.LBB182_121:
	bne r7, r6, .LBB182_126
.LBB182_122:
	addi r6, r0, 5
	blt r4, r6, .LBB182_127
.LBB182_123:
	addi r6, r3, 4
	addi r7, r4, -4
	addi r8, r0, 32
.LBB182_124:
	ldbu r9, r6+0
	bne r9, r8, .LBB182_126
.LBB182_125:
	addi r6, r6, 1
	addi r7, r7, -1
	bne r7, r0, .LBB182_124
	jal r0, .LBB182_127
.LBB182_126:
	lui r5, %hi(.L.str.67)
	addi r5, r5, %lo(.L.str.67)
	add r12, r1, r0
	jal r31, fio_str_is
	add r3, r1, r0
	add r1, r12, r0
	seq r3, r3, r24
	addi r3, r3, -1
	andi r5, r3, 2
.LBB182_127:
	stw r11+0, r5
.LBB182_128:
	lui r24, %hi(fio_o_pos)
	ldw r12, r24+%lo(fio_o_pos)
	beq r12, r0, .LBB182_148
.LBB182_129:
	lui r3, %hi(fio_o_poslen)
	ldw r13, r3+%lo(fio_o_poslen)
	addi r11, r0, 1
	blt r13, r11, .LBB182_148
.LBB182_130:
	addi r3, r0, 6
	lui r4, %hi(.L.str.68)
	addi r4, r4, %lo(.L.str.68)
	addi r5, r0, 26
	add r6, r0, r0
.LBB182_131:
	beq r6, r3, .LBB182_135
.LBB182_132:
	add r7, r6, r4
	ldb r7, r7+0
	add r8, r12, r6
	ldb r8, r8+0
	addi r9, r8, -97
	andi r9, r9, 255
	sltu r9, r9, r5
	addi r10, r8, -32
	xor r10, r10, r8
	sub r9, r0, r9
	and r9, r10, r9
	xor r8, r8, r9
	bne r8, r7, .LBB182_142
.LBB182_133:
	addi r6, r6, 1
	bne r13, r6, .LBB182_131
.LBB182_134:
	add r4, r13, r0
	jal r0, .LBB182_136
.LBB182_135:
	add r4, r3, r0
.LBB182_136:
	bne r4, r3, .LBB182_142
.LBB182_137:
	addi r3, r0, 7
	blt r13, r3, .LBB182_141
.LBB182_138:
	addi r3, r12, 6
	addi r4, r13, -6
	addi r5, r0, 32
.LBB182_139:
	ldbu r6, r3+0
	bne r6, r5, .LBB182_142
.LBB182_140:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB182_139
.LBB182_141:
	addi r11, r0, 2
	add r3, r1, r0
	add r4, r0, r0
	add r5, r11, r0
	jal r31, fseek
	jal r0, .LBB182_143
.LBB182_142:
	lui r5, %hi(.L.str.69)
	addi r5, r5, %lo(.L.str.69)
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB182_147
.LBB182_143:
	stw r27+0, r11
	jal r0, .LBB182_148
.LBB182_144:
	bne r23, r0, .LBB182_44
.LBB182_145:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.64)
	addi r4, r4, %lo(.L.str.64)
.LBB182_146:
	addi r5, sp, 28
	jal r0, .LBB182_212
.LBB182_147:
	lui r5, %hi(.L.str.70)
	addi r5, r5, %lo(.L.str.70)
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB182_214
.LBB182_148:
	ldw r11, r23+%lo(fio_o_act)
	beq r11, r0, .LBB182_163
.LBB182_149:
	ldw r12, r26+%lo(fio_o_actlen)
	addi r1, r0, 1
	blt r12, r1, .LBB182_163
.LBB182_150:
	addi r1, r0, 4
	lui r3, %hi(.L.str.66)
	addi r3, r3, %lo(.L.str.66)
	addi r4, r0, 26
	add r5, r0, r0
.LBB182_151:
	beq r5, r1, .LBB182_155
.LBB182_152:
	add r6, r5, r3
	ldb r6, r6+0
	add r7, r11, r5
	ldb r7, r7+0
	addi r8, r7, -97
	andi r8, r8, 255
	sltu r8, r8, r4
	addi r9, r7, -32
	xor r9, r9, r7
	sub r8, r0, r8
	and r8, r9, r8
	xor r7, r7, r8
	bne r7, r6, .LBB182_161
.LBB182_153:
	addi r5, r5, 1
	bne r12, r5, .LBB182_151
.LBB182_154:
	add r3, r12, r0
	jal r0, .LBB182_156
.LBB182_155:
	add r3, r1, r0
.LBB182_156:
	bne r3, r1, .LBB182_161
.LBB182_157:
	addi r1, r0, 5
	blt r12, r1, .LBB182_163
.LBB182_158:
	addi r1, r11, 4
	addi r3, r12, -4
	addi r4, r0, 32
.LBB182_159:
	ldbu r5, r1+0
	bne r5, r4, .LBB182_161
.LBB182_160:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB182_159
	jal r0, .LBB182_163
.LBB182_161:
	lui r5, %hi(.L.str.67)
	addi r5, r5, %lo(.L.str.67)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	bne r1, r0, .LBB182_163
.LBB182_162:
	lui r5, %hi(.L.str.72)
	addi r5, r5, %lo(.L.str.72)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB182_215
.LBB182_163:
	lui r13, %hi(fio_o_pad)
	ldw r1, r13+%lo(fio_o_pad)
	beq r1, r0, .LBB182_176
.LBB182_164:
	lui r3, %hi(fio_o_padlen)
	ldw r3, r3+%lo(fio_o_padlen)
	addi r4, r0, 1
	blt r3, r4, .LBB182_176
.LBB182_165:
	addi r4, r0, 3
	lui r5, %hi(.L.str.74)
	addi r5, r5, %lo(.L.str.74)
	addi r6, r0, 26
	add r7, r0, r0
.LBB182_166:
	beq r7, r4, .LBB182_170
.LBB182_167:
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
	bne r9, r8, .LBB182_211
.LBB182_168:
	addi r7, r7, 1
	bne r3, r7, .LBB182_166
.LBB182_169:
	add r5, r3, r0
	jal r0, .LBB182_171
.LBB182_170:
	add r5, r4, r0
.LBB182_171:
	bne r5, r4, .LBB182_211
.LBB182_172:
	addi r4, r0, 4
	blt r3, r4, .LBB182_176
.LBB182_173:
	addi r1, r1, 3
	addi r3, r3, -3
	addi r4, r0, 32
.LBB182_174:
	ldbu r5, r1+0
	bne r5, r4, .LBB182_211
.LBB182_175:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB182_174
.LBB182_176:
	lui r26, %hi(fio_o_delim)
	ldw r11, r26+%lo(fio_o_delim)
	beq r11, r0, .LBB182_193
.LBB182_177:
	lui r1, %hi(fio_o_delimlen)
	ldw r12, r1+%lo(fio_o_delimlen)
	addi r1, r0, 1
	blt r12, r1, .LBB182_193
.LBB182_178:
	addi r1, r0, 10
	lui r3, %hi(.L.str.76)
	addi r3, r3, %lo(.L.str.76)
	addi r4, r0, 26
	add r5, r0, r0
.LBB182_179:
	beq r5, r1, .LBB182_183
.LBB182_180:
	add r6, r5, r3
	ldb r6, r6+0
	add r7, r11, r5
	ldb r7, r7+0
	addi r8, r7, -97
	andi r8, r8, 255
	sltu r8, r8, r4
	addi r9, r7, -32
	xor r9, r9, r7
	sub r8, r0, r8
	and r8, r9, r8
	xor r7, r7, r8
	bne r7, r6, .LBB182_189
.LBB182_181:
	addi r5, r5, 1
	bne r12, r5, .LBB182_179
.LBB182_182:
	add r3, r12, r0
	jal r0, .LBB182_184
.LBB182_183:
	add r3, r1, r0
.LBB182_184:
	bne r3, r1, .LBB182_189
.LBB182_185:
	addi r1, r0, 39
	addi r3, r0, 11
	blt r12, r3, .LBB182_191
.LBB182_186:
	addi r3, r11, 10
	addi r4, r12, -10
	addi r5, r0, 32
.LBB182_187:
	ldbu r6, r3+0
	bne r6, r5, .LBB182_189
.LBB182_188:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB182_187
	jal r0, .LBB182_191
.LBB182_189:
	lui r5, %hi(.L.str.77)
	addi r5, r5, %lo(.L.str.77)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB182_192
.LBB182_190:
	addi r1, r0, 34
.LBB182_191:
	stw r25+0, r1
	jal r0, .LBB182_193
.LBB182_192:
	lui r5, %hi(.L.str.78)
	addi r5, r5, %lo(.L.str.78)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB182_216
.LBB182_193:
	stw r26+%lo(fio_o_delim), r0
	stw r13+%lo(fio_o_pad), r0
	stw r23+%lo(fio_o_act), r0
	stw r24+%lo(fio_o_pos), r0
	add r3, r21, r22
	lui r1, %hi(.L.str.8)
	addi r1, r1, %lo(.L.str.8)
	addi r4, sp, 28
	xor r1, r1, r4
	sub r5, r0, r20
	and r1, r1, r5
	xor r4, r4, r1
	jal r31, strcpy
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r1, r14, r1
	addi r3, r0, -1
	stw r1+0, r3
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r14, r1
	stw r1+0, r15
	lui r1, %hi(fio_uunf)
	addi r1, r1, %lo(fio_uunf)
	add r1, r14, r1
	stw r1+0, r16
	lui r1, %hi(fio_urecl)
	addi r1, r1, %lo(fio_urecl)
	add r1, r14, r1
	stw r1+0, r17
	beq r19, r0, .LBB182_207
.LBB182_194:
	addi r1, r0, 1
	add r3, r0, r0
	blt r18, r1, .LBB182_208
.LBB182_195:
	addi r3, r0, 4
	lui r4, %hi(.L.str.52)
	addi r4, r4, %lo(.L.str.52)
	addi r5, r0, 26
	add r6, r0, r0
.LBB182_196:
	beq r6, r3, .LBB182_200
.LBB182_197:
	add r7, r6, r4
	ldb r7, r7+0
	add r8, r19, r6
	ldb r8, r8+0
	addi r9, r8, -97
	andi r9, r9, 255
	sltu r9, r9, r5
	addi r10, r8, -32
	xor r10, r10, r8
	sub r9, r0, r9
	and r9, r10, r9
	xor r8, r8, r9
	bne r8, r7, .LBB182_207
.LBB182_198:
	addi r6, r6, 1
	bne r18, r6, .LBB182_196
.LBB182_199:
	add r4, r18, r0
	jal r0, .LBB182_201
.LBB182_200:
	add r4, r3, r0
.LBB182_201:
	bne r4, r3, .LBB182_207
.LBB182_202:
	addi r3, r0, 5
	blt r18, r3, .LBB182_206
.LBB182_203:
	addi r3, r19, 4
	addi r4, r18, -4
	addi r5, r0, 32
.LBB182_204:
	ldbu r6, r3+0
	bne r6, r5, .LBB182_207
.LBB182_205:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB182_204
.LBB182_206:
	add r3, r1, r0
	jal r0, .LBB182_208
.LBB182_207:
	add r3, r0, r0
.LBB182_208:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r14, r1
	stw r1+0, r3
	add r1, r0, r0
	jal r0, .LBB182_45
.LBB182_209:
	bne r23, r0, .LBB182_44
.LBB182_210:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.65)
	addi r4, r4, %lo(.L.str.65)
	jal r0, .LBB182_212
.LBB182_211:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.75)
	addi r4, r4, %lo(.L.str.75)
.LBB182_212:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB182_213:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.49)
	addi r4, r4, %lo(.L.str.49)
	jal r0, .LBB182_212
.LBB182_214:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.71)
	addi r4, r4, %lo(.L.str.71)
	jal r0, .LBB182_212
.LBB182_215:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.73)
	addi r4, r4, %lo(.L.str.73)
	jal r0, .LBB182_212
.LBB182_216:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.79)
	addi r4, r4, %lo(.L.str.79)
	jal r0, .LBB182_212
.Lfunc_end182:
	.size	f77_open, .Lfunc_end182-f77_open
                                        # -- End function
	.p2align	2                               # -- Begin function fio_str_is
	.type	fio_str_is,@function
fio_str_is:                             # @fio_str_is
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	addi r1, r0, 1
	blt r4, r1, .LBB183_6
.LBB183_1:
	addi r7, r0, 26
	add r6, r0, r0
.LBB183_2:
	add r8, r5, r6
	ldb r8, r8+0
	beq r8, r0, .LBB183_7
.LBB183_3:
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
	bne r9, r8, .LBB183_8
.LBB183_4:
	addi r6, r6, 1
	bne r4, r6, .LBB183_2
.LBB183_5:
	add r6, r4, r0
	jal r0, .LBB183_7
.LBB183_6:
	add r6, r0, r0
.LBB183_7:
	add r5, r5, r6
	ldbu r5, r5+0
	beq r5, r0, .LBB183_10
.LBB183_8:
	add r1, r0, r0
.LBB183_9:
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.LBB183_10:
	ble r4, r6, .LBB183_9
.LBB183_11:
	add r3, r3, r6
	sub r4, r4, r6
	addi r5, r0, 32
.LBB183_12:
	ldbu r6, r3+0
	bne r6, r5, .LBB183_8
.LBB183_13:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB183_12
	jal r0, .LBB183_9
.Lfunc_end183:
	.size	fio_str_is, .Lfunc_end183-fio_str_is
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
	addi r1, r3, -1
	addi r6, r0, 99
	sltu r1, r1, r6
	sub r1, r0, r1
	and r1, r3, r1
	slli r1, r1, 2
	lui r7, %hi(fio_pend)
	addi r7, r7, %lo(fio_pend)
	add r1, r1, r7
	stw r1+0, r0
	bgtu r3, r6, .LBB184_30
.LBB184_1:
	slli r13, r3, 2
	lui r1, %hi(fio_ufile)
	addi r1, r1, %lo(fio_ufile)
	add r14, r13, r1
	ldw r11, r14+0
	beq r11, r0, .LBB184_30
.LBB184_2:
	addi r6, r0, 1
	beq r4, r0, .LBB184_10
.LBB184_3:
	addi r1, r0, 1
	add r15, r0, r0
	blt r5, r1, .LBB184_11
.LBB184_4:
	addi r6, r0, 6
	lui r7, %hi(.L.str.80)
	addi r7, r7, %lo(.L.str.80)
	addi r8, r0, 26
	add r9, r0, r0
.LBB184_5:
	beq r9, r6, .LBB184_31
.LBB184_6:
	add r10, r9, r7
	ldb r10, r10+0
	add lr, r4, r9
	ldb lr, lr+0
	addi r12, lr, -97
	andi r12, r12, 255
	sltu r12, r12, r8
	addi r15, lr, -32
	xor r15, r15, lr
	sub r12, r0, r12
	and r12, r15, r12
	xor lr, lr, r12
	bne lr, r10, .LBB184_9
.LBB184_7:
	addi r9, r9, 1
	bne r5, r9, .LBB184_5
.LBB184_8:
	add r7, r5, r0
	beq r7, r6, .LBB184_32
.LBB184_9:
	add r6, r0, r0
.LBB184_10:
	add r15, r0, r0
.LBB184_11:
	bne r6, r0, .LBB184_24
.LBB184_12:
	addi r1, r0, 1
	blt r5, r1, .LBB184_37
.LBB184_13:
	addi r1, r0, 4
	lui r6, %hi(.L.str.81)
	addi r6, r6, %lo(.L.str.81)
	addi r7, r0, 26
	add r8, r0, r0
.LBB184_14:
	beq r8, r1, .LBB184_18
.LBB184_15:
	add r9, r8, r6
	ldb r9, r9+0
	add r10, r4, r8
	ldb r10, r10+0
	addi lr, r10, -97
	andi lr, lr, 255
	sltu lr, lr, r7
	addi r12, r10, -32
	xor r12, r12, r10
	sub lr, r0, lr
	and lr, r12, lr
	xor r10, r10, lr
	bne r10, r9, .LBB184_37
.LBB184_16:
	addi r8, r8, 1
	bne r5, r8, .LBB184_14
.LBB184_17:
	add r6, r5, r0
	jal r0, .LBB184_19
.LBB184_18:
	add r6, r1, r0
.LBB184_19:
	bne r6, r1, .LBB184_37
.LBB184_20:
	addi r1, r0, 5
	blt r5, r1, .LBB184_24
.LBB184_21:
	addi r1, r4, 4
	addi r4, r5, -4
	addi r5, r0, 32
.LBB184_22:
	ldbu r6, r1+0
	bne r6, r5, .LBB184_37
.LBB184_23:
	addi r1, r1, 1
	addi r4, r4, -1
	bne r4, r0, .LBB184_22
.LBB184_24:
	add r16, r3, r0
	beq r3, r0, .LBB184_27
.LBB184_25:
	lui r1, %hi(fio_uwrote)
	addi r1, r1, %lo(fio_uwrote)
	add r1, r13, r1
	ldw r3, r1+0
	beq r3, r0, .LBB184_27
.LBB184_26:
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
.LBB184_27:
	ldw r3, r14+0
	jal r31, fclose
	stw r14+0, r0
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r1, r13, r1
	addi r3, r0, -1
	stw r1+0, r3
	slli r11, r16, 8
	lui r12, %hi(fio_ufname)
	addi r12, r12, %lo(fio_ufname)
	beq r15, r0, .LBB184_29
.LBB184_28:
	add r3, r11, r12
	jal r31, remove
.LBB184_29:
	add r1, r11, r12
	stb r1+0, r0
.LBB184_30:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB184_31:
	add r7, r6, r0
	bne r7, r6, .LBB184_9
.LBB184_32:
	addi r7, r0, 7
	add r6, r1, r0
	add r15, r1, r0
	blt r5, r7, .LBB184_11
.LBB184_33:
	addi r9, r5, -7
	addi r1, r4, 6
	addi r7, r0, 32
	addi r8, r0, 1
.LBB184_34:
	ldbu r10, r1+0
	seq r6, r10, r7
	addi lr, r9, -1
	sltu r9, lr, r9
	bne r9, r8, .LBB184_36
.LBB184_35:
	addi r1, r1, 1
	add r9, lr, r0
	add r15, r6, r0
	beq r10, r7, .LBB184_34
	jal r0, .LBB184_11
.LBB184_36:
	add r15, r6, r0
	jal r0, .LBB184_11
.LBB184_37:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.82)
	addi r4, r4, %lo(.L.str.82)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end184:
	.size	f77_close, .Lfunc_end184-f77_close
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
	bne r3, r0, .LBB185_4
.LBB185_1:
	addi r3, r0, -400
	lui r6, %hi(fio_uend+400)
	addi r6, r6, %lo(fio_uend+400)
	addi r7, r0, -1
.LBB185_2:
	add r8, r3, r6
	stw r8+0, r7
	addi r3, r3, 4
	bne r3, r0, .LBB185_2
.LBB185_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB185_4:
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
	beq r4, r0, .LBB185_21
.LBB185_5:
	addi r25, r0, 1
	blt r5, r25, .LBB185_21
.LBB185_6:
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
.LBB185_7:
	addi r4, sp, 16
	add r4, r4, r3
	ldbu r5, r4+0
	bne r5, r1, .LBB185_9
.LBB185_8:
	stb r4+0, r0
	addi r4, r3, -1
	addi r5, r3, 1
	add r3, r4, r0
	bgt r5, r25, .LBB185_7
.LBB185_9:
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	addi r12, sp, 16
	add r4, r12, r0
	jal r31, strcpy
	stw r24+%lo(inq_named), r25
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	add r3, r12, r0
	jal r31, fopen
	beq r1, r0, .LBB185_11
.LBB185_10:
	stb r26+%lo(inq_exist), r25
	add r3, r1, r0
	jal r31, fclose
.LBB185_11:
	lui r12, %hi(fio_ufname+256)
	addi r12, r12, %lo(fio_ufname+256)
	lui r13, %hi(fio_ufile+4)
	addi r13, r13, %lo(fio_ufile+4)
	addi r24, r0, 100
	jal r0, .LBB185_13
.LBB185_12:
	addi r25, r25, 1
	addi r12, r12, 256
	addi r13, r13, 4
	beq r25, r24, .LBB185_16
.LBB185_13:
	ldw r1, r13+0
	beq r1, r0, .LBB185_12
.LBB185_14:
	addi r4, sp, 16
	add r3, r12, r0
	jal r31, strcmp
	bne r1, r0, .LBB185_12
.LBB185_15:
	addi r1, r0, 1
	stb r22+%lo(inq_opened), r1
	stw r23+%lo(inq_unit), r25
.LBB185_16:
	ldbu r1, r22+%lo(inq_opened)
	beq r1, r0, .LBB185_26
.LBB185_17:
	ldw r3, r23+%lo(inq_unit)
	xor r3, r3, r11
	sub r1, r0, r1
	and r1, r3, r1
	xor r11, r11, r1
.LBB185_18:
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
	beq r4, r0, .LBB185_26
.LBB185_19:
	addi r4, r0, 1
	blt r3, r4, .LBB185_26
.LBB185_20:
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
	jal r0, .LBB185_26
.LBB185_21:
	addi r1, r0, 100
	bltu r11, r1, .LBB185_23
.LBB185_22:
	stb r26+%lo(inq_exist), r0
	jal r0, .LBB185_26
.LBB185_23:
	addi r1, r0, 1
	stb r26+%lo(inq_exist), r1
	stw r23+%lo(inq_unit), r11
	addi r3, r0, 6
	bgtu r11, r3, .LBB185_27
.LBB185_24:
	sll r4, r1, r11
	andi r4, r4, 97
	beq r4, r0, .LBB185_27
.LBB185_25:
	seq r3, r11, r3
	addi r4, r0, 5
	seq r4, r11, r4
	stb r22+%lo(inq_opened), r1
	lui r1, %hi(.L.str.85)
	addi r1, r1, %lo(.L.str.85)
	lui r5, %hi(.L.str.84)
	addi r5, r5, %lo(.L.str.84)
	xor r5, r5, r1
	sub r3, r0, r3
	and r3, r5, r3
	xor r1, r1, r3
	lui r3, %hi(.L.str.83)
	addi r3, r3, %lo(.L.str.83)
	xor r3, r3, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r4, r1, r3
	lui r3, %hi(inq_name)
	addi r3, r3, %lo(inq_name)
	jal r31, strcpy
	stw r24+%lo(inq_named), r0
.LBB185_26:
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
.LBB185_27:
	slli r3, r11, 2
	lui r4, %hi(fio_ufile)
	addi r4, r4, %lo(fio_ufile)
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r0, .LBB185_29
.LBB185_28:
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
	jal r0, .LBB185_18
.LBB185_29:
	stb r22+%lo(inq_opened), r0
	jal r0, .LBB185_26
.Lfunc_end185:
	.size	f77_inquire, .Lfunc_end185-f77_inquire
                                        # -- End function
	.globl	f77_inq                         # -- Begin function f77_inq
	.p2align	2
	.type	f77_inq,@function
f77_inq:                                # @f77_inq
# %bb.0:
	addi r1, r3, -1
	addi r3, r0, 20
	bgtu r1, r3, .LBB186_43
.LBB186_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r12, r5, r0
	add r11, r4, r0
	slli r1, r1, 2
	lui r3, %hi(.LJTI186_0)
	addi r3, r3, %lo(.LJTI186_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB186_2:
	lui r1, %hi(inq_exist)
	ldbu r1, r1+%lo(inq_exist)
	jal r0, .LBB186_41
.LBB186_3:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_action)
	ldw r3, r3+%lo(inq_action)
	addi r4, r0, 2
	seq r3, r3, r4
	lui r4, %hi(.L.str.74)
	addi r4, r4, %lo(.L.str.74)
	lui r5, %hi(.L.str.88)
	addi r5, r5, %lo(.L.str.88)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.63)
	addi r4, r4, %lo(.L.str.63)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB186_42
.LBB186_4:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_6
.LBB186_5:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_6:
	add r5, r3, r0
	bge r4, r1, .LBB186_5
.LBB186_7:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_5
.LBB186_8:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r14, r0, 1
	bne r1, r14, .LBB186_88
.LBB186_9:
	lui r1, %hi(inq_action)
	ldw r1, r1+%lo(inq_action)
	bne r1, r14, .LBB186_89
.LBB186_10:
	lui r13, %hi(.L.str.66)
	addi r13, r13, %lo(.L.str.66)
	jal r0, .LBB186_90
.LBB186_11:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB186_42
.LBB186_12:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB186_42
.LBB186_13:
	lui r1, %hi(inq_recl)
	ldw r1, r1+%lo(inq_recl)
	jal r0, .LBB186_41
.LBB186_14:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r14, r0, 1
	bne r1, r14, .LBB186_42
.LBB186_15:
	lui r1, %hi(inq_named)
	ldw r1, r1+%lo(inq_named)
	beq r1, r0, .LBB186_42
.LBB186_16:
	lui r13, %hi(inq_name)
	addi r13, r13, %lo(inq_name)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB186_42
.LBB186_17:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_19
.LBB186_18:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_19:
	add r5, r3, r0
	bge r4, r1, .LBB186_18
.LBB186_20:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB186_18
.LBB186_21:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB186_42
.LBB186_22:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB186_42
.LBB186_23:
	lui r1, %hi(inq_nextrec)
	ldw r1, r1+%lo(inq_nextrec)
	jal r0, .LBB186_41
.LBB186_24:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.88)
	addi r4, r4, %lo(.L.str.88)
	lui r5, %hi(.L.str.74)
	addi r5, r5, %lo(.L.str.74)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.63)
	addi r4, r4, %lo(.L.str.63)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB186_42
.LBB186_25:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_27
.LBB186_26:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_27:
	add r5, r3, r0
	bge r4, r1, .LBB186_26
.LBB186_28:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_26
.LBB186_29:
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
	jal r0, .LBB186_41
.LBB186_30:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r13, %hi(.L.str.86)
	addi r13, r13, %lo(.L.str.86)
	addi r14, r0, 1
	bne r1, r14, .LBB186_96
.LBB186_31:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	bne r1, r0, .LBB186_96
.LBB186_32:
	lui r1, %hi(inq_posn)
	ldw r1, r1+%lo(inq_posn)
	addi r3, r0, 2
	bne r1, r3, .LBB186_95
.LBB186_33:
	lui r13, %hi(.L.str.68)
	addi r13, r13, %lo(.L.str.68)
	jal r0, .LBB186_96
.LBB186_34:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_named)
	ldw r3, r3+%lo(inq_named)
	sub r1, r0, r1
	and r1, r3, r1
	jal r0, .LBB186_41
.LBB186_35:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.74)
	addi r4, r4, %lo(.L.str.74)
	lui r5, %hi(.L.str.88)
	addi r5, r5, %lo(.L.str.88)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.63)
	addi r4, r4, %lo(.L.str.63)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB186_42
.LBB186_36:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_38
.LBB186_37:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_38:
	add r5, r3, r0
	bge r4, r1, .LBB186_37
.LBB186_39:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_37
.LBB186_40:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
.LBB186_41:
	stw r11+0, r1
.LBB186_42:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB186_43:
	jalr r0, r31, 0
.LBB186_44:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_action)
	ldw r3, r3+%lo(inq_action)
	addi r14, r0, 1
	seq r3, r3, r14
	lui r4, %hi(.L.str.74)
	addi r4, r4, %lo(.L.str.74)
	lui r5, %hi(.L.str.88)
	addi r5, r5, %lo(.L.str.88)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.63)
	addi r4, r4, %lo(.L.str.63)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB186_42
.LBB186_45:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_47
.LBB186_46:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_47:
	add r5, r3, r0
	bge r4, r1, .LBB186_46
.LBB186_48:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_46
.LBB186_49:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.86)
	addi r4, r4, %lo(.L.str.86)
	lui r5, %hi(.L.str.74)
	addi r5, r5, %lo(.L.str.74)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB186_42
.LBB186_50:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_52
.LBB186_51:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_52:
	add r5, r3, r0
	bge r4, r1, .LBB186_51
.LBB186_53:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_51
.LBB186_54:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.46)
	addi r4, r4, %lo(.L.str.46)
	lui r5, %hi(.L.str.87)
	addi r5, r5, %lo(.L.str.87)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.86)
	addi r4, r4, %lo(.L.str.86)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB186_42
.LBB186_55:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_57
.LBB186_56:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_57:
	add r5, r3, r0
	bge r4, r1, .LBB186_56
.LBB186_58:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_56
.LBB186_59:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r5, %hi(inq_bz)
	ldw r5, r5+%lo(inq_bz)
	seq r4, r5, r4
	lui r5, %hi(.L.str.52)
	addi r5, r5, %lo(.L.str.52)
	lui r6, %hi(.L.str.90)
	addi r6, r6, %lo(.L.str.90)
	xor r6, r6, r5
	sub r4, r0, r4
	and r4, r6, r4
	xor r4, r5, r4
	lui r5, %hi(.L.str.86)
	addi r5, r5, %lo(.L.str.86)
	xor r4, r4, r5
	sub r3, r0, r3
	and r3, r4, r3
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r5, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB186_42
.LBB186_60:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_62
.LBB186_61:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_62:
	add r5, r3, r0
	bge r4, r1, .LBB186_61
.LBB186_63:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_61
.LBB186_64:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_direct)
	ldw r3, r3+%lo(inq_direct)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.88)
	addi r4, r4, %lo(.L.str.88)
	lui r5, %hi(.L.str.74)
	addi r5, r5, %lo(.L.str.74)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.63)
	addi r4, r4, %lo(.L.str.63)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB186_42
.LBB186_65:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_67
.LBB186_66:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_67:
	add r5, r3, r0
	bge r4, r1, .LBB186_66
.LBB186_68:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_66
.LBB186_69:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_action)
	ldw r3, r3+%lo(inq_action)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.88)
	addi r4, r4, %lo(.L.str.88)
	lui r5, %hi(.L.str.74)
	addi r5, r5, %lo(.L.str.74)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.63)
	addi r4, r4, %lo(.L.str.63)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB186_42
.LBB186_70:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_72
.LBB186_71:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_72:
	add r5, r3, r0
	bge r4, r1, .LBB186_71
.LBB186_73:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_71
.LBB186_74:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.74)
	addi r4, r4, %lo(.L.str.74)
	lui r5, %hi(.L.str.88)
	addi r5, r5, %lo(.L.str.88)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.63)
	addi r4, r4, %lo(.L.str.63)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB186_42
.LBB186_75:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_77
.LBB186_76:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_77:
	add r5, r3, r0
	bge r4, r1, .LBB186_76
.LBB186_78:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_76
.LBB186_79:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_unf)
	ldw r3, r3+%lo(inq_unf)
	addi r4, r0, 0
	seq r3, r3, r4
	lui r4, %hi(.L.str.47)
	addi r4, r4, %lo(.L.str.47)
	lui r5, %hi(.L.str.89)
	addi r5, r5, %lo(.L.str.89)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.86)
	addi r4, r4, %lo(.L.str.86)
	xor r3, r3, r4
	sub r1, r0, r1
	and r1, r3, r1
	xor r13, r4, r1
	add r3, r13, r0
	jal r31, strlen
	addi r3, r0, 1
	blt r12, r3, .LBB186_42
.LBB186_80:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_82
.LBB186_81:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_82:
	add r5, r3, r0
	bge r4, r1, .LBB186_81
.LBB186_83:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_81
.LBB186_84:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r13, %hi(.L.str.86)
	addi r13, r13, %lo(.L.str.86)
	addi r14, r0, 1
	bne r1, r14, .LBB186_102
.LBB186_85:
	lui r1, %hi(inq_unf)
	ldw r1, r1+%lo(inq_unf)
	bne r1, r0, .LBB186_102
.LBB186_86:
	lui r1, %hi(inq_delim)
	ldw r1, r1+%lo(inq_delim)
	addi r3, r0, 39
	bne r1, r3, .LBB186_101
.LBB186_87:
	lui r13, %hi(.L.str.76)
	addi r13, r13, %lo(.L.str.76)
	jal r0, .LBB186_102
.LBB186_88:
	lui r13, %hi(.L.str.86)
	addi r13, r13, %lo(.L.str.86)
	jal r0, .LBB186_90
.LBB186_89:
	addi r3, r0, 2
	seq r1, r1, r3
	lui r3, %hi(.L.str.72)
	addi r3, r3, %lo(.L.str.72)
	lui r4, %hi(.L.str.67)
	addi r4, r4, %lo(.L.str.67)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r13, r3, r1
.LBB186_90:
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB186_42
.LBB186_91:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_93
.LBB186_92:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_93:
	add r5, r3, r0
	bge r4, r1, .LBB186_92
.LBB186_94:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_92
.LBB186_95:
	seq r1, r1, r14
	lui r3, %hi(.L.str.70)
	addi r3, r3, %lo(.L.str.70)
	lui r4, %hi(.L.str.69)
	addi r4, r4, %lo(.L.str.69)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r13, r3, r1
.LBB186_96:
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB186_42
.LBB186_97:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_99
.LBB186_98:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_99:
	add r5, r3, r0
	bge r4, r1, .LBB186_98
.LBB186_100:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_98
.LBB186_101:
	addi r3, r0, 34
	seq r1, r1, r3
	lui r3, %hi(.L.str.78)
	addi r3, r3, %lo(.L.str.78)
	lui r4, %hi(.L.str.77)
	addi r4, r4, %lo(.L.str.77)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r13, r3, r1
.LBB186_102:
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB186_42
.LBB186_103:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB186_105
.LBB186_104:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB186_42
.LBB186_105:
	add r5, r3, r0
	bge r4, r1, .LBB186_104
.LBB186_106:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB186_104
.Lfunc_end186:
	.size	f77_inq, .Lfunc_end186-f77_inq
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI186_0,@object
.LJTI186_0:
	.word	.LBB186_2
	.word	.LBB186_40
	.word	.LBB186_29
	.word	.LBB186_34
	.word	.LBB186_14
	.word	.LBB186_54
	.word	.LBB186_64
	.word	.LBB186_35
	.word	.LBB186_79
	.word	.LBB186_24
	.word	.LBB186_74
	.word	.LBB186_11
	.word	.LBB186_21
	.word	.LBB186_59
	.word	.LBB186_8
	.word	.LBB186_30
	.word	.LBB186_3
	.word	.LBB186_44
	.word	.LBB186_69
	.word	.LBB186_84
	.word	.LBB186_49
	.size	.LJTI186_0, 84
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
	bne r3, r0, .LBB187_4
.LBB187_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB187_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB187_2
.LBB187_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB187_4:
	addi r1, r11, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB187_9
.LBB187_5:
	addi r1, r11, -5
	addi r3, r0, 1
	bleu r1, r3, .LBB187_9
.LBB187_6:
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, fio_resolve
	add r12, r1, r0
	slli r13, r11, 2
	lui r1, %hi(fio_uwrote)
	addi r1, r1, %lo(fio_uwrote)
	add r1, r13, r1
	ldw r3, r1+0
	beq r3, r0, .LBB187_8
.LBB187_7:
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
.LBB187_8:
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
.LBB187_9:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.91)
	addi r4, r4, %lo(.L.str.91)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end187:
	.size	f77_rewind, .Lfunc_end187-f77_rewind
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
	beq r3, r16, .LBB188_197
.LBB188_1:
	add r13, r4, r0
	add r12, r3, r0
	lui lr, %hi(fio_ftext)
	addi r11, r0, -1
	stw lr+%lo(fio_ftext), r11
	addi r20, r0, 32
	add r1, r0, r0
	jal r0, .LBB188_3
.LBB188_2:
	add r1, r12, r28
	ldbu r3, r1+0
	addi r1, r28, 1
	bne r3, r20, .LBB188_5
.LBB188_3:
	add r28, r1, r0
	blt r13, r0, .LBB188_2
.LBB188_4:
	bltu r28, r13, .LBB188_2
.LBB188_5:
	lui r14, %hi(.L.str.98)
	addi r14, r14, %lo(.L.str.98)
	lui r15, %hi(fio_fmt_why)
	blt r13, r0, .LBB188_7
.LBB188_6:
	bgeu r28, r13, .LBB188_199
.LBB188_7:
	add r1, r12, r28
	ldbu r1, r1+0
	addi r3, r0, 40
	stw sp+52, r3
	bne r1, r3, .LBB188_199
.LBB188_8:
	addi r23, r0, 1022
	lui r19, %hi(.L.str.99)
	addi r19, r19, %lo(.L.str.99)
	addi r1, r0, 1
	stw sp+48, r1
	slli r1, r1, 2
	lui r24, %hi(.LJTI188_1)
	addi r24, r24, %lo(.LJTI188_1)
	add r22, r24, r1
	addi r26, r0, 39
	addi r9, r0, 246
	addi r10, r0, 41
	addi r14, r0, 0
	add r21, r0, r0
	add r25, r0, r0
	lui r18, %hi(fio_fbuf+1)
	addi r18, r18, %lo(fio_fbuf+1)
.LBB188_9:
	blt r13, r0, .LBB188_13
.LBB188_10:
	add r17, r0, r0
	bltu r28, r13, .LBB188_13
.LBB188_11:
	blt r21, r23, .LBB188_14
.LBB188_12:
	stw r15+%lo(fio_fmt_why), r19
	ldw r1, r22+0
	jalr r0, r1, 0
.LBB188_13:
	add r1, r12, r28
	ldbu r17, r1+0
	bge r21, r23, .LBB188_12
.LBB188_14:
	andi r27, r17, 255
	bgtu r27, r26, .LBB188_38
.LBB188_15:
	slli r1, r27, 2
	lui r3, %hi(.LJTI188_0)
	addi r3, r3, %lo(.LJTI188_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB188_16:
	add r1, r21, r0
	add r5, r26, r0
	jal r0, .LBB188_18
.LBB188_17:
	ldbu r5, r4+0
	add r1, r21, r0
.LBB188_18:
	add r3, r28, r0
.LBB188_19:
	add r4, r1, r16
	stb r4+0, r5
	addi r28, r3, 1
	blt r13, r0, .LBB188_21
.LBB188_20:
	add r4, r0, r0
	bgeu r28, r13, .LBB188_22
.LBB188_21:
	add r4, r12, r28
	ldbu r4, r4+0
.LBB188_22:
	addi r21, r1, 1
	andi r4, r4, 255
	beq r4, r0, .LBB188_35
.LBB188_23:
	addi r5, r0, 1020
	bge r1, r5, .LBB188_35
.LBB188_24:
	bne r4, r27, .LBB188_29
.LBB188_25:
	addi r28, r3, 2
	blt r13, r0, .LBB188_27
.LBB188_26:
	add r3, r0, r0
	bgeu r28, r13, .LBB188_28
.LBB188_27:
	add r3, r12, r28
	ldbu r3, r3+0
.LBB188_28:
	andi r3, r3, 255
	bne r3, r27, .LBB188_46
.LBB188_29:
	add r4, r12, r28
	blt r13, r0, .LBB188_31
.LBB188_30:
	bgeu r28, r13, .LBB188_33
.LBB188_31:
	ldbu r5, r4+0
	addi r3, r0, 39
	bne r5, r3, .LBB188_33
.LBB188_32:
	addi r21, r1, 2
	add r1, r1, r18
	stb r1+0, r3
.LBB188_33:
	blt r13, r0, .LBB188_17
.LBB188_34:
	add r1, r21, r0
	add r5, r0, r0
	add r3, r28, r0
	bgeu r28, r13, .LBB188_19
	jal r0, .LBB188_17
.LBB188_35:
	lui r1, %hi(.L.str.101)
	addi r1, r1, %lo(.L.str.101)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB188_36:
	addi r28, r28, 1
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB188_37:
	lui r1, %hi(.L.str.100)
	addi r1, r1, %lo(.L.str.100)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB188_38:
	addi r1, r17, -58
	andi r1, r1, 255
	bltu r1, r9, .LBB188_49
.LBB188_39:
	beq r21, r0, .LBB188_47
.LBB188_40:
	ldw r3, lr+%lo(fio_ftext)
	add r1, r0, r0
	beq r21, r3, .LBB188_48
.LBB188_41:
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
	blt r21, r3, .LBB188_43
.LBB188_42:
	lui r1, %hi(fio_fbuf-2)
	addi r1, r1, %lo(fio_fbuf-2)
	add r1, r21, r1
	ldbu r3, r1+0
	add r14, r8, r0
	jal r31, toupper
	add r8, r14, r0
	addi r3, r0, 66
	sne r1, r1, r3
.LBB188_43:
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
	bgtu r3, r4, .LBB188_84
.LBB188_44:
	addi r5, r0, 1
	sll r6, r5, r3
	lui r7, 165
	addi r7, r7, -1669
	and r6, r6, r7
	beq r6, r0, .LBB188_83
.LBB188_45:
	add r1, r5, r0
	jal r0, .LBB188_48
.LBB188_46:
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
.LBB188_47:
	add r1, r0, r0
.LBB188_48:
	beq r1, r0, .LBB188_53
.LBB188_49:
	addi r1, r21, 1
	add r3, r21, r16
	stb r3+0, r17
	addi r28, r28, 1
	ldw r3, sp+52
	seq r3, r27, r3
	add r25, r25, r3
	bne r27, r10, .LBB188_51
.LBB188_50:
	addi r25, r25, -1
	seq r3, r25, r14
	sub r3, r0, r3
	andi r5, r3, 4
	add r21, r1, r0
	jal r0, .LBB188_52
.LBB188_51:
	add r21, r1, r0
	add r5, r0, r0
.LBB188_52:
	slli r1, r5, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB188_53:
	add r1, r28, r0
	add r4, r0, r0
	jal r0, .LBB188_57
.LBB188_54:
	ldbu r6, r3+0
.LBB188_55:
	addi r3, r0, 10
	mul r3, r4, r3
	add r3, r3, r5
	addi r4, r3, -48
	addi r3, r21, 1
	add r5, r21, r16
	stb r5+0, r6
	add r21, r3, r0
.LBB188_56:
	addi r1, r1, 1
.LBB188_57:
	add r3, r12, r1
	blt r13, r0, .LBB188_59
.LBB188_58:
	bgeu r1, r13, .LBB188_60
.LBB188_59:
	ldbu r5, r3+0
	addi r5, r5, -58
	andi r5, r5, 255
	addi r6, r0, 245
	bgtu r5, r6, .LBB188_63
.LBB188_60:
	blt r13, r0, .LBB188_62
.LBB188_61:
	bgeu r1, r13, .LBB188_72
.LBB188_62:
	ldbu r5, r3+0
	bne r5, r20, .LBB188_72
.LBB188_63:
	blt r13, r0, .LBB188_65
.LBB188_64:
	bgeu r1, r13, .LBB188_66
.LBB188_65:
	ldbu r5, r3+0
	beq r5, r20, .LBB188_56
.LBB188_66:
	bge r21, r23, .LBB188_82
.LBB188_67:
	blt r13, r0, .LBB188_69
.LBB188_68:
	add r5, r0, r0
	bgeu r1, r13, .LBB188_70
.LBB188_69:
	ldbu r5, r3+0
.LBB188_70:
	blt r13, r0, .LBB188_54
.LBB188_71:
	add r6, r0, r0
	bgeu r1, r13, .LBB188_55
	jal r0, .LBB188_54
.LBB188_72:
	blt r13, r0, .LBB188_74
.LBB188_73:
	bgeu r1, r13, .LBB188_75
.LBB188_74:
	ldbu r5, r3+0
	addi r6, r0, 72
	beq r5, r6, .LBB188_78
.LBB188_75:
	addi r5, r0, 5
	blt r13, r0, .LBB188_77
.LBB188_76:
	bgeu r1, r13, .LBB188_81
.LBB188_77:
	ldbu r6, r3+0
	addi r7, r0, 104
	bne r6, r7, .LBB188_81
.LBB188_78:
	beq r4, r0, .LBB188_85
.LBB188_79:
	add r5, r4, r21
	addi r6, r0, 1022
	blt r5, r6, .LBB188_86
.LBB188_80:
	lui r3, %hi(.L.str.102)
	addi r3, r3, %lo(.L.str.102)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB188_81:
	add r28, r1, r0
	jal r0, .LBB188_52
.LBB188_82:
	lui r3, %hi(.L.str.102)
	addi r3, r3, %lo(.L.str.102)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	add r21, r23, r0
	jalr r0, r3, 0
.LBB188_83:
	beq r3, r4, .LBB188_48
.LBB188_84:
	addi r1, r0, 46
	seq r1, r8, r1
	jal r0, .LBB188_48
.LBB188_85:
	lui r3, %hi(.L.str.103)
	addi r3, r3, %lo(.L.str.103)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB188_86:
	blt r13, r0, .LBB188_88
.LBB188_87:
	add r5, r0, r0
	bgeu r1, r13, .LBB188_89
.LBB188_88:
	ldbu r5, r3+0
.LBB188_89:
	add r6, r21, r16
	stb r6+0, r5
	addi r21, r21, 1
	addi r5, r0, 1
	blt r4, r5, .LBB188_99
.LBB188_90:
	addi r6, r4, 1
	add r4, r0, r0
	jal r0, .LBB188_93
.LBB188_91:
	ldbu r8, r7+1
.LBB188_92:
	add r7, r21, r4
	add r7, r7, r16
	stb r7+0, r8
	addi r6, r6, -1
	addi r4, r4, 1
	ble r6, r5, .LBB188_100
.LBB188_93:
	add r7, r1, r4
	addi r28, r7, 1
	blt r13, r0, .LBB188_95
.LBB188_94:
	bgeu r28, r13, .LBB188_98
.LBB188_95:
	add r7, r3, r4
	ldbu r8, r7+1
	beq r8, r0, .LBB188_98
.LBB188_96:
	blt r13, r0, .LBB188_91
.LBB188_97:
	add r8, r0, r0
	bgeu r28, r13, .LBB188_92
	jal r0, .LBB188_91
.LBB188_98:
	lui r1, %hi(.L.str.104)
	addi r1, r1, %lo(.L.str.104)
	stw r15+%lo(fio_fmt_why), r1
	add r21, r21, r4
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB188_99:
	addi r28, r1, 1
	jal r0, .LBB188_101
.LBB188_100:
	add r1, r1, r4
	addi r28, r1, 1
	add r21, r21, r4
.LBB188_101:
	stw lr+%lo(fio_ftext), r21
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB188_102:
	add r1, r21, r16
	stb r1+0, r0
	addi r14, r0, 1
	addi r19, r0, 58
	addi r24, r0, -1
	addi r21, r0, -10
	addi r22, r0, 9
	addi r23, r0, 10
	addi r25, r0, 72
	lui r27, %hi(.L.str.106)
	addi r27, r27, %lo(.L.str.106)
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
	jal r0, .LBB188_104
.LBB188_103:
	addi r14, r14, 1
.LBB188_104:
	add r1, r14, r16
	ldbu r1, r1+0
	bgtu r1, r19, .LBB188_116
.LBB188_105:
	slli r3, r1, 2
	lui r4, %hi(.LJTI188_2)
	addi r4, r4, %lo(.LJTI188_2)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB188_106:
	addi fp, r14, 1
	add r1, r14, r18
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB188_112
.LBB188_107:
	addi r20, r0, -1
	jal r0, .LBB188_114
.LBB188_108:
	addi r14, r14, 1
.LBB188_109:
	add r1, r14, r0
	add r3, r14, r16
	ldbu r4, r3+0
	addi r14, r14, 1
	addi r3, r0, 39
	bne r4, r3, .LBB188_109
.LBB188_110:
	add r4, r14, r16
	ldbu r4, r4+0
	bne r4, r3, .LBB188_104
.LBB188_111:
	addi r14, r1, 2
	jal r0, .LBB188_109
.LBB188_112:
	add r20, r0, r0
	addi r1, r1, -48
	bgtu r1, r22, .LBB188_114
.LBB188_113:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB188_113
.LBB188_114:
	lui r14, %hi(.L.str.105)
	addi r14, r14, %lo(.L.str.105)
	blt r20, r0, .LBB188_199
.LBB188_115:
	add r1, fp, r16
	ldbu r3, r1+0
	jal r31, toupper
	beq r1, r26, .LBB188_122
	jal r0, .LBB188_199
.LBB188_116:
	slli r1, r1, 24
	srai r1, r1, 24
	addi r3, r1, -58
	bgeu r3, r21, .LBB188_118
.LBB188_117:
	add fp, r14, r0
	add r20, r24, r0
	jal r0, .LBB188_122
.LBB188_118:
	addi r1, r1, -48
	bleu r1, r22, .LBB188_120
.LBB188_119:
	add fp, r14, r0
	add r20, r0, r0
	jal r0, .LBB188_122
.LBB188_120:
	add r20, r0, r0
	add fp, r14, r0
.LBB188_121:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB188_121
.LBB188_122:
	add r14, fp, r16
	ldbu r3, r14+0
	jal r31, toupper
	bne r1, r25, .LBB188_125
.LBB188_123:
	blt r20, r0, .LBB188_198
.LBB188_124:
	add r1, fp, r20
	addi r14, r1, 1
	jal r0, .LBB188_104
.LBB188_125:
	blt r20, r0, .LBB188_128
.LBB188_126:
	ldw r3, sp+52
	bne r1, r3, .LBB188_128
.LBB188_127:
	addi r14, fp, 1
	jal r0, .LBB188_104
.LBB188_128:
	beq r1, r28, .LBB188_139
.LBB188_129:
	sgt r3, r20, r24
	seq r4, r1, r26
	and r3, r3, r4
	bne r3, r0, .LBB188_139
.LBB188_130:
	addi r17, r1, -65
	ldw r3, sp+48
	bgtu r17, r3, .LBB188_160
.LBB188_131:
	slli r3, r17, 2
	lui r4, %hi(.LJTI188_3)
	addi r4, r4, %lo(.LJTI188_3)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB188_132:
	addi r14, fp, 1
	add r3, fp, r18
	ldb r3, r3+0
	addi r3, r3, -58
	bltu r3, r21, .LBB188_134
.LBB188_133:
	add r3, r14, r0
	addi r14, r14, 1
	add r3, r3, r18
	ldb r3, r3+0
	addi r3, r3, -58
	addi r4, r0, -11
	bgtu r3, r4, .LBB188_133
.LBB188_134:
	addi r3, r0, 65
	beq r1, r3, .LBB188_104
.LBB188_135:
	add r1, r14, r16
	ldbu r1, r1+0
	addi r3, r0, 46
	bne r1, r3, .LBB188_104
.LBB188_136:
	addi r1, r14, 1
	add r3, r14, r18
	ldb r3, r3+0
	addi r3, r3, -58
	add r14, r1, r0
	bltu r3, r21, .LBB188_104
.LBB188_137:
	add r14, r1, r0
.LBB188_138:
	add r1, r14, r0
	addi r14, r14, 1
	add r1, r1, r18
	ldb r1, r1+0
	addi r1, r1, -58
	bgeu r1, r21, .LBB188_138
	jal r0, .LBB188_104
.LBB188_139:
	addi r14, fp, 1
	jal r0, .LBB188_104
.LBB188_140:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 82
	beq r1, r3, .LBB188_142
.LBB188_141:
	addi r3, r0, 76
	bne r1, r3, .LBB188_149
.LBB188_142:
	addi r14, fp, 2
	jal r0, .LBB188_150
.LBB188_143:
	stw sp+24, r24
	add r24, r28, r0
	add r28, r27, r0
	stw sp+20, r1
	add r27, fp, r18
	ldbu r3, r27+0
	jal r31, toupper
	ldw r3, sp+32
	beq r1, r3, .LBB188_145
.LBB188_144:
	ldbu r3, r27+0
	jal r31, toupper
	addi r3, r0, 90
	bne r1, r3, .LBB188_158
.LBB188_145:
	addi r14, fp, 2
	add r27, r28, r0
	add r28, r24, r0
	ldw r24, sp+24
	jal r0, .LBB188_104
.LBB188_146:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	ldw r3, sp+36
	beq r1, r3, .LBB188_148
.LBB188_147:
	addi r14, fp, 1
	bne r1, r26, .LBB188_104
.LBB188_148:
	addi r14, fp, 2
	jal r0, .LBB188_104
.LBB188_149:
	addi r14, fp, 1
.LBB188_150:
	add r1, r14, r16
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB188_152
.LBB188_151:
	addi r1, r0, 1
	jal r0, .LBB188_157
.LBB188_152:
	addi r1, r1, -48
	bleu r1, r22, .LBB188_154
.LBB188_153:
	add r1, r0, r0
	jal r0, .LBB188_157
.LBB188_154:
	add r3, r0, r0
.LBB188_155:
	add r4, r14, r0
	mul r3, r3, r23
	add r3, r3, r1
	addi r14, r14, 1
	add r1, r4, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB188_155
.LBB188_156:
	srli r1, r3, 31
.LBB188_157:
	beq r1, r0, .LBB188_104
	jal r0, .LBB188_214
.LBB188_158:
	ldw r1, sp+20
	add r27, r28, r0
	add r28, r24, r0
	ldw r24, sp+24
	ldw r3, sp+48
	bgtu r17, r3, .LBB188_160
.LBB188_159:
	addi r3, r0, 1
	sll r3, r3, r17
	lui r4, 8196
	addi r4, r4, 3
	and r3, r3, r4
	bne r3, r0, .LBB188_132
.LBB188_160:
	ldw r3, sp+28
	and r3, r1, r3
	ldw r4, sp+44
	beq r3, r4, .LBB188_163
.LBB188_161:
	addi r3, r0, 76
	beq r1, r3, .LBB188_163
.LBB188_162:
	addi r3, r0, 73
	bne r1, r3, .LBB188_216
.LBB188_163:
	addi r17, fp, 1
	ldw r3, sp+40
	bne r1, r3, .LBB188_167
.LBB188_164:
	add r20, r17, r16
	ldbu r3, r20+0
	add r14, r1, r0
	jal r31, toupper
	add r3, r1, r0
	add r1, r14, r0
	ldw r4, sp+36
	beq r3, r4, .LBB188_166
.LBB188_165:
	ldbu r3, r20+0
	jal r31, toupper
	add r3, r1, r0
	add r1, r14, r0
	ldw r4, sp+32
	bne r3, r4, .LBB188_167
.LBB188_166:
	addi r17, fp, 2
.LBB188_167:
	add r3, r17, r16
	ldb r3, r3+0
	addi r4, r3, -58
	bltu r4, r21, .LBB188_212
.LBB188_168:
	addi r3, r3, -48
	bleu r3, r22, .LBB188_170
.LBB188_169:
	add r3, r0, r0
	jal r0, .LBB188_173
.LBB188_170:
	add r4, r0, r0
.LBB188_171:
	add r5, r17, r0
	mul r4, r4, r23
	add r4, r4, r3
	addi r17, r17, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB188_171
.LBB188_172:
	srli r3, r4, 31
.LBB188_173:
	bne r3, r0, .LBB188_212
.LBB188_174:
	add r3, r17, r16
	ldbu r3, r3+0
	addi r4, r0, 46
	bne r3, r4, .LBB188_177
.LBB188_175:
	addi r14, r17, 1
	add r3, r17, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB188_178
.LBB188_176:
	addi r3, r0, 1
	jal r0, .LBB188_183
.LBB188_177:
	add r14, r17, r0
	jal r0, .LBB188_184
.LBB188_178:
	addi r3, r3, -48
	bleu r3, r22, .LBB188_180
.LBB188_179:
	add r3, r0, r0
	jal r0, .LBB188_183
.LBB188_180:
	add r4, r0, r0
.LBB188_181:
	add r5, r14, r0
	mul r4, r4, r23
	add r4, r4, r3
	addi r14, r14, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB188_181
.LBB188_182:
	srli r3, r4, 31
.LBB188_183:
	bne r3, r0, .LBB188_215
.LBB188_184:
	addi r3, r0, 71
	beq r1, r3, .LBB188_186
.LBB188_185:
	andi r1, r1, 78
	ldw r3, sp+44
	bne r1, r3, .LBB188_104
.LBB188_186:
	add r1, r14, r16
	ldbu r3, r1+0
	jal r31, toupper
	ldw r3, sp+40
	bne r1, r3, .LBB188_104
.LBB188_187:
	addi r1, r14, 1
	add r3, r14, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB188_189
.LBB188_188:
	addi r3, r0, 1
	jal r0, .LBB188_194
.LBB188_189:
	addi r3, r3, -48
	bleu r3, r22, .LBB188_191
.LBB188_190:
	add r14, r1, r0
	add r3, r0, r0
	jal r0, .LBB188_195
.LBB188_191:
	add r4, r0, r0
.LBB188_192:
	add r5, r1, r0
	mul r1, r4, r23
	add r4, r1, r3
	addi r1, r5, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB188_192
.LBB188_193:
	srli r3, r4, 31
.LBB188_194:
	add r14, r1, r0
.LBB188_195:
	beq r3, r0, .LBB188_104
.LBB188_196:
	lui r14, %hi(.L.str.110)
	addi r14, r14, %lo(.L.str.110)
	jal r0, .LBB188_199
.LBB188_197:
	lui r1, %hi(fio_fmt)
	stw r1+%lo(fio_fmt), r16
	add r11, r0, r0
	jal r0, .LBB188_211
.LBB188_198:
	add r14, r27, r0
.LBB188_199:
	stw r15+%lo(fio_fmt_why), r14
.LBB188_200:
	addi r1, r0, 79
	addi r3, r0, 40
	addi r4, r0, 41
	add r6, r0, r0
	add r5, r0, r0
	jal r0, .LBB188_202
.LBB188_201:
	addi r6, r6, 1
.LBB188_202:
	blt r13, r0, .LBB188_204
.LBB188_203:
	add r7, r0, r0
	bgeu r6, r13, .LBB188_205
.LBB188_204:
	add r7, r12, r6
	ldbu r7, r7+0
.LBB188_205:
	andi r7, r7, 255
	beq r7, r0, .LBB188_210
.LBB188_206:
	bgtu r6, r1, .LBB188_210
.LBB188_207:
	seq r8, r7, r3
	add r5, r5, r8
	bne r7, r4, .LBB188_201
.LBB188_208:
	addi r5, r5, -1
	bne r5, r0, .LBB188_201
.LBB188_209:
	addi r6, r6, 1
.LBB188_210:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	ldw r5, r15+%lo(fio_fmt_why)
	lui r4, %hi(.L.str.114)
	addi r4, r4, %lo(.L.str.114)
	add r7, r12, r0
	jal r31, fprintf
.LBB188_211:
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
.LBB188_212:
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.108)
	addi r4, r4, %lo(.L.str.108)
	add r3, r14, r0
	add r5, r1, r0
.LBB188_213:
	jal r31, sprintf
	jal r0, .LBB188_199
.LBB188_214:
	lui r14, %hi(.L.str.107)
	addi r14, r14, %lo(.L.str.107)
	jal r0, .LBB188_199
.LBB188_215:
	lui r14, %hi(.L.str.109)
	addi r14, r14, %lo(.L.str.109)
	jal r0, .LBB188_199
.LBB188_216:
	blt r20, r0, .LBB188_219
.LBB188_217:
	addi r1, r1, -41
	addi r3, r0, 17
	bgtu r1, r3, .LBB188_220
.LBB188_218:
	slli r1, r1, 2
	lui r3, %hi(.Lswitch.table.fio_fmt_use)
	addi r3, r3, %lo(.Lswitch.table.fio_fmt_use)
	add r1, r1, r3
	ldw r14, r1+0
	jal r0, .LBB188_199
.LBB188_219:
	ldb r5, r14+0
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.113)
	addi r4, r4, %lo(.L.str.113)
	add r3, r14, r0
	jal r0, .LBB188_213
.LBB188_220:
	lui r14, %hi(.L.str.112)
	addi r14, r14, %lo(.L.str.112)
	jal r0, .LBB188_199
.LBB188_221:
.Lfunc_end188:
	.size	fio_fmt_use, .Lfunc_end188-fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI188_0,@object
.LJTI188_0:
	.word	.LBB188_37
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_36
	.word	.LBB188_38
	.word	.LBB188_16
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_38
	.word	.LBB188_16
	.size	.LJTI188_0, 160
	.type	.LJTI188_1,@object
.LJTI188_1:
	.word	.LBB188_9
	.word	.LBB188_200
	.word	.LBB188_221
	.word	.LBB188_221
	.word	.LBB188_102
	.word	.LBB188_9
	.size	.LJTI188_1, 24
	.type	.LJTI188_2,@object
.LJTI188_2:
	.word	.LBB188_197
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_108
	.word	.LBB188_103
	.word	.LBB188_103
	.word	.LBB188_116
	.word	.LBB188_106
	.word	.LBB188_103
	.word	.LBB188_106
	.word	.LBB188_116
	.word	.LBB188_103
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_116
	.word	.LBB188_103
	.size	.LJTI188_2, 236
	.type	.LJTI188_3,@object
.LJTI188_3:
	.word	.LBB188_132
	.word	.LBB188_143
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_132
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_146
	.word	.LBB188_140
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_160
	.word	.LBB188_132
	.size	.LJTI188_3, 104
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
	bleu r5, r6, .LBB189_2
.LBB189_1:
	add r1, r0, r0
	jal r0, .LBB189_5
.LBB189_2:
	addi r6, r1, 1
	addi r7, r0, 10
	add r1, r0, r0
.LBB189_3:
	mul r1, r1, r7
	addi r8, r4, 1
	add r1, r5, r1
	add r4, r6, r4
	ldb r4, r4+0
	addi r5, r4, -48
	add r4, r8, r0
	bltu r5, r7, .LBB189_3
.LBB189_4:
	stw r3+%lo(fio_pos), r8
.LBB189_5:
	jalr r0, r31, 0
.Lfunc_end189:
	.size	fio_number, .Lfunc_end189-fio_number
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
	beq r1, r0, .LBB190_3
.LBB190_1:
	lui r11, %hi(fio_irec)
	ldw r3, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r3, r4, .LBB190_28
.LBB190_2:
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
	jal r0, .LBB190_27
.LBB190_3:
	lui r12, %hi(fio_unit)
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	addi r13, r0, 98
	lui r11, %hi(fio_in)
	bgtu r3, r13, .LBB190_7
.LBB190_4:
	slli r1, r1, 2
	lui r14, %hi(fio_uend)
	addi r14, r14, %lo(fio_uend)
	add r1, r1, r14
	ldw r1, r1+0
	blt r1, r0, .LBB190_7
.LBB190_5:
	ldw r3, r11+%lo(fio_in)
	beq r3, r0, .LBB190_7
.LBB190_6:
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	add r4, r3, r14
	ldw r4, r4+0
	bge r1, r4, .LBB190_30
.LBB190_7:
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	bgtu r3, r13, .LBB190_11
.LBB190_8:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB190_11
.LBB190_9:
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
	beq r3, r0, .LBB190_29
.LBB190_10:
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB190_26
.LBB190_11:
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
.LBB190_12:
	add r18, r3, r0
	add r3, r11, r0
	jal r31, getc
	beq r1, r16, .LBB190_17
.LBB190_13:
	addi r3, r18, 1
	add r4, r18, r15
	stb r4+0, r1
	beq r1, r14, .LBB190_15
.LBB190_14:
	bltu r18, r17, .LBB190_12
.LBB190_15:
	beq r3, r0, .LBB190_18
.LBB190_16:
	add r1, r3, r15
	stb r1+0, r0
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB190_20
.LBB190_17:
	add r3, r18, r0
	bne r3, r0, .LBB190_16
.LBB190_18:
	ldw r1, r12+%lo(fio_unit)
	addi r4, r1, -1
	bgtu r4, r13, .LBB190_20
.LBB190_19:
	slli r1, r1, 2
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r1, r1, r4
	addi r4, r0, 1
	stw r1+0, r4
.LBB190_20:
	beq r3, r0, .LBB190_28
.LBB190_21:
	lui r1, %hi(fio_rlen)
	ldw r4, r1+%lo(fio_rlen)
	addi r3, r0, 1
	blt r4, r3, .LBB190_26
.LBB190_22:
	addi r4, r4, 1
	lui r5, %hi(fio_rec-2)
	addi r5, r5, %lo(fio_rec-2)
	addi r6, r0, 13
	jal r0, .LBB190_24
.LBB190_23:
	addi r7, r4, -2
	stw r1+%lo(fio_rlen), r7
	addi r4, r4, -1
	ble r4, r3, .LBB190_26
.LBB190_24:
	add r7, r4, r5
	ldbu r7, r7+0
	beq r7, r6, .LBB190_23
.LBB190_25:
	beq r7, r14, .LBB190_23
.LBB190_26:
	lui r1, %hi(fio_rpos)
	stw r1+%lo(fio_rpos), r0
.LBB190_27:
	add r1, r0, r0
	jal r0, .LBB190_29
.LBB190_28:
	addi r1, r0, 1
.LBB190_29:
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
.LBB190_30:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r3, r3, r1
	addi r1, r0, 1
	stw r3+0, r1
	jal r0, .LBB190_29
.Lfunc_end190:
	.size	fio_getrec, .Lfunc_end190-fio_getrec
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
	ble r1, r3, .LBB191_5
.LBB191_1:
	addi r4, r0, 0
	seq r4, r3, r4
	slli r3, r3, 1
	lui r5, 1
	xor r5, r3, r5
	sub r4, r0, r4
	and r4, r5, r4
	xor r3, r3, r4
.LBB191_2:
	add r13, r3, r0
	slli r3, r3, 1
	blt r13, r1, .LBB191_2
.LBB191_3:
	ldw r3, r15+%lo(nml_text)
	add r4, r13, r0
	jal r31, realloc
	beq r1, r0, .LBB191_6
.LBB191_4:
	stw r15+%lo(nml_text), r1
	stw r16+%lo(nml_tcap), r13
.LBB191_5:
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
.LBB191_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.133)
	addi r4, r4, %lo(.L.str.133)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end191:
	.size	nml_text_add, .Lfunc_end191-nml_text_add
                                        # -- End function
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
	.asciz	"f77: out of memory copying an ALLOCATABLE component\n"
	.size	.L.str.21, 53

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"f77: out of memory for an array argument (%d bytes)\n"
	.size	.L.str.22, 53

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"f77: NEAREST with S = 0\n"
	.size	.L.str.23, 25

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"f77: MODULO with P = 0\n"
	.size	.L.str.24, 24

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"%04d%02d%02d"
	.size	.L.str.25, 13

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"%02d%02d%02d.%03d"
	.size	.L.str.26, 18

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"+0000"
	.size	.L.str.27, 6

	.type	f77_rng,@object                 # @f77_rng
	.data
	.p2align	2, 0x0
f77_rng:
	.word	2654435769                      # 0x9e3779b9
	.word	608135816                       # 0x243f6a88
	.word	3084996962                      # 0xb7e15162
	.word	2242054355                      # 0x85a308d3
	.size	f77_rng, 16

	.type	.L.str.28,@object               # @.str.28
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.28:
	.asciz	"f77: RANDOM_SEED's PUT and GET have 4 elements at least\n"
	.size	.L.str.28, 57

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"f77: DATE_AND_TIME's VALUES has 8 elements at least\n"
	.size	.L.str.29, 53

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"f77: concatenation result of %d bytes is too long\n"
	.size	.L.str.30, 51

	.type	fio_catpos,@object              # @fio_catpos
	.local	fio_catpos
	.comm	fio_catpos,4,4
	.type	fio_catring,@object             # @fio_catring
	.local	fio_catring
	.comm	fio_catring,65536,1
	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"f77: REPEAT with NCOPIES %d, less than 0\n"
	.size	.L.str.31, 42

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
	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"PAUSE %.*s\n"
	.size	.L.str.32, 12

	.type	fio_in,@object                  # @fio_in
	.local	fio_in
	.comm	fio_in,4,4
	.type	fio_uwrote,@object              # @fio_uwrote
	.local	fio_uwrote
	.comm	fio_uwrote,400,4
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
	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"f77: %c editing does not match a numeric READ item\n"
	.size	.L.str.33, 52

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"f77: %c editing does not match a CHARACTER READ item\n"
	.size	.L.str.34, 54

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
	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"(I11)"
	.size	.L.str.35, 6

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"T"
	.size	.L.str.36, 2

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"F"
	.size	.L.str.37, 2

	.type	nml_asg,@object                 # @nml_asg
	.local	nml_asg
	.comm	nml_asg,4,4
	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"the subscripts do not match the array"
	.size	.L.str.38, 38

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"a subscript is out of range"
	.size	.L.str.39, 28

	.type	nml_val,@object                 # @nml_val
	.local	nml_val
	.comm	nml_val,4,4
	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	"more values than the item has elements"
	.size	.L.str.40, 39

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	" &END"
	.size	.L.str.41, 6

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"%s is no item of the NAMELIST"
	.size	.L.str.42, 30

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"f77: end of file on unit %d looking for NAMELIST &%s\n"
	.size	.L.str.43, 54

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"f77: NAMELIST &%s on unit %d: %s\n"
	.size	.L.str.44, 34

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
	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"f77: cannot OPEN unit %d\n"
	.size	.L.str.45, 26

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"DIRECT"
	.size	.L.str.46, 7

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"UNFORMATTED"
	.size	.L.str.47, 12

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"SCRATCH"
	.size	.L.str.48, 8

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"f77: OPEN ACCESS='DIRECT' needs RECL=\n"
	.size	.L.str.49, 39

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"f77: bad FILE= name in OPEN\n"
	.size	.L.str.50, 29

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"fort.%d"
	.size	.L.str.51, 8

	.type	fio_ufile,@object               # @fio_ufile
	.local	fio_ufile
	.comm	fio_ufile,400,4
	.type	fio_ufname,@object              # @fio_ufname
	.local	fio_ufname
	.comm	fio_ufname,25600,1
	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"ZERO"
	.size	.L.str.52, 5

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"f77: OPEN cannot create a scratch file\n"
	.size	.L.str.53, 40

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"OLD"
	.size	.L.str.54, 4

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"r+"
	.size	.L.str.55, 3

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"r"
	.size	.L.str.56, 2

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"f77: OPEN STATUS='OLD': no such file '%s'\n"
	.size	.L.str.57, 43

	.type	.L.str.58,@object               # @.str.58
.L.str.58:
	.asciz	"NEW"
	.size	.L.str.58, 4

	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"f77: OPEN STATUS='NEW': '%s' already exists\n"
	.size	.L.str.59, 45

	.type	.L.str.60,@object               # @.str.60
.L.str.60:
	.asciz	"w+"
	.size	.L.str.60, 3

	.type	.L.str.61,@object               # @.str.61
.L.str.61:
	.asciz	"f77: OPEN cannot create '%s'\n"
	.size	.L.str.61, 30

	.type	.L.str.62,@object               # @.str.62
.L.str.62:
	.asciz	"REPLACE"
	.size	.L.str.62, 8

	.type	.L.str.63,@object               # @.str.63
.L.str.63:
	.asciz	"UNKNOWN"
	.size	.L.str.63, 8

	.type	.L.str.64,@object               # @.str.64
.L.str.64:
	.asciz	"f77: OPEN cannot open '%s'\n"
	.size	.L.str.64, 28

	.type	.L.str.65,@object               # @.str.65
.L.str.65:
	.asciz	"f77: OPEN STATUS value is not supported\n"
	.size	.L.str.65, 41

	.type	fio_uaction,@object             # @fio_uaction
	.local	fio_uaction
	.comm	fio_uaction,400,4
	.type	fio_uposn,@object               # @fio_uposn
	.local	fio_uposn
	.comm	fio_uposn,400,4
	.type	.L.str.66,@object               # @.str.66
.L.str.66:
	.asciz	"READ"
	.size	.L.str.66, 5

	.type	.L.str.67,@object               # @.str.67
.L.str.67:
	.asciz	"WRITE"
	.size	.L.str.67, 6

	.type	.L.str.68,@object               # @.str.68
.L.str.68:
	.asciz	"APPEND"
	.size	.L.str.68, 7

	.type	.L.str.69,@object               # @.str.69
.L.str.69:
	.asciz	"REWIND"
	.size	.L.str.69, 7

	.type	.L.str.70,@object               # @.str.70
.L.str.70:
	.asciz	"ASIS"
	.size	.L.str.70, 5

	.type	.L.str.71,@object               # @.str.71
.L.str.71:
	.asciz	"f77: OPEN POSITION= is ASIS, REWIND or APPEND\n"
	.size	.L.str.71, 47

	.type	.L.str.72,@object               # @.str.72
.L.str.72:
	.asciz	"READWRITE"
	.size	.L.str.72, 10

	.type	.L.str.73,@object               # @.str.73
.L.str.73:
	.asciz	"f77: OPEN ACTION= is READ, WRITE or READWRITE\n"
	.size	.L.str.73, 47

	.type	.L.str.74,@object               # @.str.74
.L.str.74:
	.asciz	"YES"
	.size	.L.str.74, 4

	.type	.L.str.75,@object               # @.str.75
.L.str.75:
	.asciz	"f77: OPEN PAD='NO' is not supported\n"
	.size	.L.str.75, 37

	.type	.L.str.76,@object               # @.str.76
.L.str.76:
	.asciz	"APOSTROPHE"
	.size	.L.str.76, 11

	.type	.L.str.77,@object               # @.str.77
.L.str.77:
	.asciz	"QUOTE"
	.size	.L.str.77, 6

	.type	.L.str.78,@object               # @.str.78
.L.str.78:
	.asciz	"NONE"
	.size	.L.str.78, 5

	.type	.L.str.79,@object               # @.str.79
.L.str.79:
	.asciz	"f77: OPEN DELIM= is APOSTROPHE, QUOTE or NONE\n"
	.size	.L.str.79, 47

	.type	.L.str.80,@object               # @.str.80
.L.str.80:
	.asciz	"DELETE"
	.size	.L.str.80, 7

	.type	.L.str.81,@object               # @.str.81
.L.str.81:
	.asciz	"KEEP"
	.size	.L.str.81, 5

	.type	.L.str.82,@object               # @.str.82
.L.str.82:
	.asciz	"f77: CLOSE STATUS value is not supported\n"
	.size	.L.str.82, 42

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
	.type	.L.str.83,@object               # @.str.83
.L.str.83:
	.asciz	"stdin"
	.size	.L.str.83, 6

	.type	.L.str.84,@object               # @.str.84
.L.str.84:
	.asciz	"stdout"
	.size	.L.str.84, 7

	.type	.L.str.85,@object               # @.str.85
.L.str.85:
	.asciz	"stderr"
	.size	.L.str.85, 7

	.type	.L.str.86,@object               # @.str.86
.L.str.86:
	.asciz	"UNDEFINED"
	.size	.L.str.86, 10

	.type	.L.str.87,@object               # @.str.87
.L.str.87:
	.asciz	"SEQUENTIAL"
	.size	.L.str.87, 11

	.type	.L.str.88,@object               # @.str.88
.L.str.88:
	.asciz	"NO"
	.size	.L.str.88, 3

	.type	.L.str.89,@object               # @.str.89
.L.str.89:
	.asciz	"FORMATTED"
	.size	.L.str.89, 10

	.type	.L.str.90,@object               # @.str.90
.L.str.90:
	.asciz	"NULL"
	.size	.L.str.90, 5

	.type	.L.str.91,@object               # @.str.91
.L.str.91:
	.asciz	"f77: REWIND: unit %d cannot be rewound\n"
	.size	.L.str.91, 40

	.type	.L.str.92,@object               # @.str.92
.L.str.92:
	.asciz	"f77: an I/O statement in a function referenced in another's I/O list\n"
	.size	.L.str.92, 70

	.type	fio_uend_init,@object           # @fio_uend_init
	.local	fio_uend_init
	.comm	fio_uend_init,1,4
	.type	.L.str.93,@object               # @.str.93
.L.str.93:
	.asciz	"f77: unit %d is not open for %s\n"
	.size	.L.str.93, 33

	.type	.L.str.94,@object               # @.str.94
.L.str.94:
	.asciz	"f77: a direct-access unit needs REC=\n"
	.size	.L.str.94, 38

	.type	.L.str.95,@object               # @.str.95
.L.str.95:
	.asciz	"f77: READ past the end of a direct-access file\n"
	.size	.L.str.95, 48

	.type	.L.str.96,@object               # @.str.96
.L.str.96:
	.asciz	"f77: end of file on READ\n"
	.size	.L.str.96, 26

	.type	.L.str.97,@object               # @.str.97
.L.str.97:
	.asciz	"f77: bad unformatted record\n"
	.size	.L.str.97, 29

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
	.type	.L.str.98,@object               # @.str.98
.L.str.98:
	.asciz	"a format begins with ("
	.size	.L.str.98, 23

	.type	fio_fmt_why,@object             # @fio_fmt_why
	.local	fio_fmt_why
	.comm	fio_fmt_why,4,4
	.type	.L.str.99,@object               # @.str.99
.L.str.99:
	.asciz	"the format is too long, or ends without its )"
	.size	.L.str.99, 46

	.type	.L.str.100,@object              # @.str.100
.L.str.100:
	.asciz	"the format ends without its )"
	.size	.L.str.100, 30

	.type	.L.str.101,@object              # @.str.101
.L.str.101:
	.asciz	"a quoted string runs past the end of the format"
	.size	.L.str.101, 48

	.type	.L.str.102,@object              # @.str.102
.L.str.102:
	.asciz	"the format is too long"
	.size	.L.str.102, 23

	.type	.L.str.103,@object              # @.str.103
.L.str.103:
	.asciz	"0H is no Hollerith string"
	.size	.L.str.103, 26

	.type	.L.str.104,@object              # @.str.104
.L.str.104:
	.asciz	"nH wants more characters than the format has"
	.size	.L.str.104, 45

	.type	fio_fmt_check.why,@object       # @fio_fmt_check.why
	.local	fio_fmt_check.why
	.comm	fio_fmt_check.why,64,1
	.type	.L.str.105,@object              # @.str.105
.L.str.105:
	.asciz	"a sign belongs to a scale factor, nP"
	.size	.L.str.105, 37

	.type	.L.str.106,@object              # @.str.106
.L.str.106:
	.asciz	"H needs its count, nH"
	.size	.L.str.106, 22

	.type	.L.str.107,@object              # @.str.107
.L.str.107:
	.asciz	"T, TL and TR take a column"
	.size	.L.str.107, 27

	.type	.L.str.108,@object              # @.str.108
.L.str.108:
	.asciz	"%c needs a width"
	.size	.L.str.108, 17

	.type	.L.str.109,@object              # @.str.109
.L.str.109:
	.asciz	"a . needs digits after it"
	.size	.L.str.109, 26

	.type	.L.str.110,@object              # @.str.110
.L.str.110:
	.asciz	"Ee needs its digits"
	.size	.L.str.110, 20

	.type	.L.str.111,@object              # @.str.111
.L.str.111:
	.asciz	"a count with no descriptor after it"
	.size	.L.str.111, 36

	.type	.L.str.112,@object              # @.str.112
.L.str.112:
	.asciz	"a count before something that is no edit descriptor"
	.size	.L.str.112, 52

	.type	.L.str.113,@object              # @.str.113
.L.str.113:
	.asciz	"%c is no edit descriptor"
	.size	.L.str.113, 25

	.type	.L.str.114,@object              # @.str.114
.L.str.114:
	.asciz	"f77: FORMAT: %s\n  in: %.*s\n"
	.size	.L.str.114, 28

	.type	.L.str.115,@object              # @.str.115
.L.str.115:
	.asciz	"%*d"
	.size	.L.str.115, 4

	.type	fio_line,@object                # @fio_line
	.local	fio_line
	.comm	fio_line,1024,1
	.type	.L.str.116,@object              # @.str.116
.L.str.116:
	.asciz	"-Infinity"
	.size	.L.str.116, 10

	.type	.L.str.117,@object              # @.str.117
.L.str.117:
	.asciz	"%%.%dE"
	.size	.L.str.117, 7

	.type	.L.str.118,@object              # @.str.118
.L.str.118:
	.asciz	"f77: FORMAT: E, D and G with no digits (d = 0) need a positive scale factor\n"
	.size	.L.str.118, 77

	.type	.L.str.119,@object              # @.str.119
.L.str.119:
	.asciz	"%.17E"
	.size	.L.str.119, 6

	.type	.L.str.120,@object              # @.str.120
.L.str.120:
	.asciz	"f77: out of memory for an array constructor\n"
	.size	.L.str.120, 45

	.type	.L.str.121,@object              # @.str.121
.L.str.121:
	.asciz	"f77: a CHARACTER result of %d bytes is too long\n"
	.size	.L.str.121, 49

	.type	fio_gcount,@object              # @fio_gcount
	.local	fio_gcount
	.comm	fio_gcount,64,4
	.type	fio_gstart,@object              # @fio_gstart
	.local	fio_gstart
	.comm	fio_gstart,64,4
	.type	.L.str.122,@object              # @.str.122
.L.str.122:
	.asciz	"f77: FORMAT: no data edit descriptor for the items left\n"
	.size	.L.str.122, 57

	.type	.L.str.123,@object              # @.str.123
.L.str.123:
	.asciz	"f77: WRITE past the end of an internal file\n"
	.size	.L.str.123, 45

	.type	.L.str.124,@object              # @.str.124
.L.str.124:
	.asciz	"f77: record longer than the internal file (%d > %d)\n"
	.size	.L.str.124, 53

	.type	.L.str.125,@object              # @.str.125
.L.str.125:
	.asciz	"f77: record longer than RECL (%d > %d)\n"
	.size	.L.str.125, 40

	.type	fio_l_nulls,@object             # @fio_l_nulls
	.local	fio_l_nulls
	.comm	fio_l_nulls,4,4
	.type	fio_l_after,@object             # @fio_l_after
	.local	fio_l_after
	.comm	fio_l_after,1,4
	.type	fio_l_pending,@object           # @fio_l_pending
	.local	fio_l_pending
	.comm	fio_l_pending,4,4
	.type	.L.str.126,@object              # @.str.126
.L.str.126:
	.asciz	"f77: zero repeat count in list-directed input\n"
	.size	.L.str.126, 47

	.type	.L.str.127,@object              # @.str.127
.L.str.127:
	.asciz	"f77: bad LOGICAL input field '%s'\n"
	.size	.L.str.127, 35

	.type	.L.str.128,@object              # @.str.128
.L.str.128:
	.asciz	"f77: bad digit '%c' in a B, O or Z input field\n"
	.size	.L.str.128, 48

	.type	.L.str.129,@object              # @.str.129
.L.str.129:
	.asciz	"E%d"
	.size	.L.str.129, 4

	.type	.L.str.130,@object              # @.str.130
.L.str.130:
	.asciz	"(A)"
	.size	.L.str.130, 4

	.type	nml_tlen,@object                # @nml_tlen
	.local	nml_tlen
	.comm	nml_tlen,4,4
	.type	.L.str.131,@object              # @.str.131
.L.str.131:
	.asciz	"the record could not be read"
	.size	.L.str.131, 29

	.type	.L.str.132,@object              # @.str.132
.L.str.132:
	.asciz	" "
	.size	.L.str.132, 2

	.type	nml_tcap,@object                # @nml_tcap
	.local	nml_tcap
	.comm	nml_tcap,4,4
	.type	nml_text,@object                # @nml_text
	.local	nml_text
	.comm	nml_text,4,4
	.type	.L.str.133,@object              # @.str.133
.L.str.133:
	.asciz	"f77: out of storage for a NAMELIST\n"
	.size	.L.str.133, 36

	.type	.L.str.134,@object              # @.str.134
.L.str.134:
	.asciz	"a subscript is no integer"
	.size	.L.str.134, 26

	.type	.L.str.135,@object              # @.str.135
.L.str.135:
	.asciz	"a value before the first NAME="
	.size	.L.str.135, 31

	.type	.L.str.136,@object              # @.str.136
.L.str.136:
	.asciz	"a repeat count is not positive"
	.size	.L.str.136, 31

	.type	nml_cval,@object                # @nml_cval
	.local	nml_cval
	.comm	nml_cval,4,4
	.type	nml_casg,@object                # @nml_casg
	.local	nml_casg
	.comm	nml_casg,4,4
	.type	nml_pend,@object                # @nml_pend
	.local	nml_pend
	.comm	nml_pend,1024,1
	.type	.L.str.137,@object              # @.str.137
.L.str.137:
	.asciz	"0.0"
	.size	.L.str.137, 4

	.type	.L.str.138,@object              # @.str.138
.L.str.138:
	.asciz	"(G23.16)"
	.size	.L.str.138, 9

	.type	.L.str.139,@object              # @.str.139
.L.str.139:
	.asciz	"(G15.8)"
	.size	.L.str.139, 8

	.type	.L.str.140,@object              # @.str.140
.L.str.140:
	.asciz	"a LOGICAL value is not T or F"
	.size	.L.str.140, 30

	.type	.L.str.141,@object              # @.str.141
.L.str.141:
	.asciz	"a CHARACTER value is not in quotes"
	.size	.L.str.141, 35

	.type	.L.str.142,@object              # @.str.142
.L.str.142:
	.asciz	"a value could not be read"
	.size	.L.str.142, 26

	.type	.Lswitch.table.fio_fmt_use,@object # @switch.table.fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.Lswitch.table.fio_fmt_use:
	.word	.L.str.111
	.word	.L.str.112
	.word	.L.str.112
	.word	.L.str.111
	.word	.L.str.112
	.word	.L.str.112
	.word	.L.str.111
	.word	.L.str.112
	.word	.L.str.112
	.word	.L.str.112
	.word	.L.str.112
	.word	.L.str.112
	.word	.L.str.112
	.word	.L.str.112
	.word	.L.str.112
	.word	.L.str.112
	.word	.L.str.112
	.word	.L.str.111
	.size	.Lswitch.table.fio_fmt_use, 72

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git a06e1a6e35103a79b1ae3c6830426b277cc25690)"
	.section	".note.GNU-stack","",@progbits
