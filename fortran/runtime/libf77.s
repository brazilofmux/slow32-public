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
	beq r4, r1, .LBB3_21
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
	bgtu r4, r5, .LBB3_7
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
	lui r5, %hi(fio_rec_next)
	ldw r4, r5+%lo(fio_rec_next)
	stw r5+%lo(fio_rec_next), r0
	addi r5, r14, -100
	addi r6, r0, -99
	bltu r5, r6, .LBB3_11
.LBB3_8:
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r3, r5
	ldw r5, r5+0
	beq r5, r0, .LBB3_11
.LBB3_9:
	ble r4, r0, .LBB3_23
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
	beq r12, r0, .LBB3_18
.LBB3_12:
	stw r1+%lo(fio_listed), r0
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fio_fmt_use
	addi r3, r0, -1
	ble r1, r3, .LBB3_22
.LBB3_13:
	stw r14+%lo(fio_pos), r0
	stw r15+%lo(fio_revert), r3
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	lui r1, %hi(fio_fmt)
	ldw r3, r1+%lo(fio_fmt)
	addi r4, r0, 32
	add r1, r0, r0
.LBB3_14:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB3_16
.LBB3_15:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB3_14
.LBB3_16:
	addi r3, r0, 40
	bne r5, r3, .LBB3_20
.LBB3_17:
	addi r3, r1, 1
	add r1, r3, r0
	jal r0, .LBB3_19
.LBB3_18:
	stw r1+%lo(fio_listed), r13
	lui r1, %hi(.L.str.9)
	addi r1, r1, %lo(.L.str.9)
	lui r3, %hi(fio_fmt)
	stw r3+%lo(fio_fmt), r1
	addi r1, r0, -1
	add r3, r0, r0
.LBB3_19:
	stw r14+%lo(fio_pos), r3
	stw r15+%lo(fio_revert), r1
.LBB3_20:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB3_21:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	jal r0, .LBB3_24
.LBB3_22:
	addi r3, r0, 2
	jal r31, exit
.LBB3_23:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
.LBB3_24:
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
	beq r4, r1, .LBB5_12
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
	bgtu r4, r5, .LBB5_7
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
	lui r5, %hi(fio_rec_next)
	ldw r4, r5+%lo(fio_rec_next)
	stw r5+%lo(fio_rec_next), r0
	addi r5, r11, -100
	addi r6, r0, -99
	bltu r5, r6, .LBB5_11
.LBB5_8:
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r3, r5
	ldw r5, r5+0
	beq r5, r0, .LBB5_11
.LBB5_9:
	ble r4, r0, .LBB5_13
.LBB5_10:
	addi r4, r4, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r3, r3, r5
	ldw r3, r3+0
	mul r4, r3, r4
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB5_11:
	lui r1, %hi(fio_ulen)
	stw r1+%lo(fio_ulen), r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB5_12:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	jal r0, .LBB5_14
.LBB5_13:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
.LBB5_14:
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
	beq r1, r12, .LBB8_31
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
	lui r1, %hi(fio_in)
	stw r1+%lo(fio_in), r13
	lui r1, %hi(fio_unit)
	stw r1+%lo(fio_unit), r14
	addi r1, r14, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB8_14
.LBB8_6:
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
	bltu r3, r4, .LBB8_10
.LBB8_7:
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r16, r3
	ldw r3, r3+0
	beq r3, r0, .LBB8_10
.LBB8_8:
	ble r1, r0, .LBB8_33
.LBB8_9:
	addi r1, r1, -1
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r3, r16, r3
	ldw r3, r3+0
	mul r4, r3, r1
	add r3, r13, r0
	add r5, r0, r0
	jal r31, fseek
.LBB8_10:
	lui r1, %hi(fio_upos)
	stw r1+%lo(fio_upos), r0
	lui r1, %hi(fio_udirect)
	addi r1, r1, %lo(fio_udirect)
	add r1, r16, r1
	ldw r1, r1+0
	beq r1, r0, .LBB8_19
.LBB8_11:
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
	bne r1, r0, .LBB8_26
.LBB8_12:
	andi r1, r11, 2
	bne r1, r0, .LBB8_30
.LBB8_13:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.95)
	addi r4, r4, %lo(.L.str.95)
	jal r0, .LBB8_35
.LBB8_14:
	lui r3, %hi(fio_rec_next)
	ldw r1, r3+%lo(fio_rec_next)
	stw r3+%lo(fio_rec_next), r0
	addi r3, r14, -100
	addi r4, r0, -99
	bltu r3, r4, .LBB8_18
.LBB8_15:
	slli r3, r14, 2
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r3, r4
	ldw r4, r4+0
	beq r4, r0, .LBB8_18
.LBB8_16:
	ble r1, r0, .LBB8_33
.LBB8_17:
	addi r1, r1, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r3, r3, r4
	ldw r3, r3+0
	mul r4, r3, r1
	add r3, r13, r0
	add r5, r0, r0
	jal r31, fseek
.LBB8_18:
	lui r1, %hi(fio_upos)
	stw r1+%lo(fio_upos), r0
.LBB8_19:
	slli r16, r14, 2
	lui r1, %hi(fio_uend)
	addi r1, r1, %lo(fio_uend)
	add r14, r16, r1
	ldw r1, r14+0
	blt r1, r0, .LBB8_21
.LBB8_20:
	add r3, r13, r0
	jal r31, ftell
	ldw r3, r14+0
	bge r1, r3, .LBB8_25
.LBB8_21:
	addi r3, sp, 20
	addi r12, r0, 1
	addi r14, r0, 4
	add r4, r12, r0
	add r5, r14, r0
	add r6, r13, r0
	jal r31, fread
	bne r1, r14, .LBB8_25
.LBB8_22:
	addi r3, sp, 20
	addi r1, r3, 3
	ldbu r1, r1+0
	slli r1, r1, 24
	blt r1, r0, .LBB8_29
.LBB8_23:
	ldhu r4, sp+20
	addi r3, r3, 2
	ldbu r3, r3+0
	slli r3, r3, 16
	or  r3, r4, r3
	or  r5, r3, r1
	lui r1, 16
	addi r1, r1, 1
	bge r5, r1, .LBB8_29
.LBB8_24:
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
	bne r12, r0, .LBB8_27
	jal r0, .LBB8_28
.LBB8_25:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r1, r16, r1
	stw r1+0, r12
	andi r1, r11, 3
	beq r1, r0, .LBB8_32
.LBB8_26:
	beq r12, r0, .LBB8_28
.LBB8_27:
	stb r15+%lo(fio_busy), r0
.LBB8_28:
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
.LBB8_29:
	andi r1, r11, 2
	beq r1, r0, .LBB8_34
.LBB8_30:
	addi r12, r0, 2
	bne r12, r0, .LBB8_27
	jal r0, .LBB8_28
.LBB8_31:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	jal r0, .LBB8_35
.LBB8_32:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r0, .LBB8_35
.LBB8_33:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
	jal r0, .LBB8_35
.LBB8_34:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.97)
	addi r4, r4, %lo(.L.str.97)
.LBB8_35:
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
	bltu r1, r4, .LBB11_8
.LBB11_5:
	addi r1, r3, -5
	addi r4, r0, 2
	bltu r1, r4, .LBB11_8
.LBB11_6:
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
	beq r5, r0, .LBB11_9
.LBB11_7:
	stw r4+0, r0
.LBB11_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB11_9:
	blt r1, r12, .LBB11_8
.LBB11_10:
	lui r4, %hi(fio_uunf)
	addi r4, r4, %lo(fio_uunf)
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r0, .LBB11_14
.LBB11_11:
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
	bne r1, r12, .LBB11_13
.LBB11_12:
	ldw r1, sp+24
	sub r1, r13, r1
	addi r4, r1, -8
.LBB11_13:
	add r3, r11, r0
	add r5, r0, r0
	jal r31, fseek
	jal r0, .LBB11_8
.LBB11_14:
	addi r13, r1, -1
	addi r15, r0, 10
.LBB11_15:
	blt r13, r12, .LBB11_17
.LBB11_16:
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
	bne r1, r15, .LBB11_15
	jal r0, .LBB11_18
.LBB11_17:
	add r13, r0, r0
.LBB11_18:
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
	lui r1, %hi(.L.str.9)
	addi r1, r1, %lo(.L.str.9)
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
	addi sp, sp, -128
	stw sp+0, lr
	stw sp+124, r11
	stw sp+120, r12
	stw sp+116, r13
	stw sp+112, r14
	stw sp+108, r15
	stw sp+104, r16
	add r11, r3, r0
	stw sp+100, r3
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB13_12
.LBB13_1:
	lui r5, %hi(.L.str.4)
	addi r5, r5, %lo(.L.str.4)
	addi r3, sp, 68
	addi r12, r0, 32
	add r4, r12, r0
	add r6, r11, r0
	jal r31, snprintf
	ldbu r9, sp+68
	beq r9, r0, .LBB13_11
.LBB13_2:
	lui r1, %hi(fio_len)
	ldw r7, r1+%lo(fio_len)
	lui r3, %hi(fio_col)
	ldw r8, r3+%lo(fio_col)
	addi r4, sp, 68
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
	jal r0, .LBB13_35
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
	sub r3, r0, r3
	andi r4, r3, 8
	xori r5, r4, 15
	xori r4, r4, 14
	sub r6, r0, r1
	and r1, r4, r6
	xor r1, r5, r1
	andi r3, r3, 7
	xori r4, r3, 4
	xori r3, r3, 5
	and r3, r3, r6
	xor r4, r4, r3
	lui r5, %hi(.L.str.5)
	addi r5, r5, %lo(.L.str.5)
	add r6, r11, r0
	add r3, r0, r0
.LBB13_19:
	add r7, r3, r0
	and r3, r6, r1
	add r3, r3, r5
	ldbu r8, r3+0
	addi r3, r7, 1
	addi r9, sp, 28
	add r7, r9, r7
	stb r7+0, r8
	srl r6, r6, r4
	bne r6, r0, .LBB13_19
.LBB13_20:
	lui r1, %hi(fio_d)
	ldw r1, r1+%lo(fio_d)
	addi r5, sp, 28
	add r4, r3, r0
	bge r3, r1, .LBB13_24
.LBB13_21:
	add r4, r5, r3
	addi r5, r0, 48
	add r6, r1, r0
.LBB13_22:
	stb r4+0, r5
	addi r4, r4, 1
	addi r6, r6, -1
	bne r3, r6, .LBB13_22
.LBB13_23:
	add r4, r1, r0
.LBB13_24:
	xor r5, r1, r3
	sgt r6, r1, r3
	sub r6, r0, r6
	and r5, r5, r6
	xor r5, r3, r5
	xori r3, r5, 1
	addi r12, r0, 1
	sgtu r6, r5, r12
	sub r6, r0, r6
	and r3, r3, r6
	xori r3, r3, 1
	addi r6, sp, 28
	add r5, r6, r5
	addi r5, r5, -1
	add r6, r0, r0
.LBB13_25:
	ldbu r7, r5+0
	addi r8, sp, 68
	add r8, r8, r6
	stb r8+0, r7
	addi r6, r6, 1
	addi r5, r5, -1
	bne r3, r6, .LBB13_25
.LBB13_26:
	addi r3, sp, 68
	add r3, r3, r4
	stb r3+0, r0
	or  r1, r1, r11
	bne r1, r0, .LBB13_30
.LBB13_27:
	lui r1, %hi(fio_fmt)
	ldw r1, r1+%lo(fio_fmt)
	lui r3, %hi(fio_pos)
	ldw r3, r3+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+-1
	addi r4, r0, 48
	bne r3, r4, .LBB13_30
.LBB13_28:
	ldbu r1, r1+-2
	addi r3, r0, 46
	bne r1, r3, .LBB13_30
.LBB13_29:
	stb sp+68, r0
.LBB13_30:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 68
	jal r31, fio_field
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	blt r3, r12, .LBB13_32
.LBB13_31:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB13_32:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB13_35
.LBB13_33:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
	jal r0, .LBB13_35
.LBB13_34:
	bne r1, r0, .LBB13_36
.LBB13_35:
	ldw r16, sp+104
	ldw r15, sp+108
	ldw r14, sp+112
	ldw r13, sp+116
	ldw r12, sp+120
	ldw r11, sp+124
	ldw lr, sp+0
	addi sp, sp, 128
	jalr r0, r31, 0
.LBB13_36:
	lui r16, %hi(fio_d)
	ldw r1, r16+%lo(fio_d)
	addi r15, r0, 1
	blt r1, r15, .LBB13_62
.LBB13_37:
	srai r1, r11, 31
	xor r3, r11, r1
	sub r6, r3, r1
	lui r5, %hi(.L.str.6)
	addi r5, r5, %lo(.L.str.6)
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
	lui r3, %hi(.L.str.9)
	addi r3, r3, %lo(.L.str.9)
	lui r4, %hi(.L.str.8)
	addi r4, r4, %lo(.L.str.8)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r11, r3, r1
	ldw r1, r16+%lo(fio_d)
	sgt r3, r1, r14
	sub r1, r1, r14
	sub r3, r0, r3
	and r7, r1, r3
	lui r5, %hi(.L.str.7)
	addi r5, r5, %lo(.L.str.7)
	addi r3, sp, 68
	add r4, r13, r0
	add r6, r11, r0
	add r8, r0, r0
	add r9, r12, r0
	jal r31, snprintf
	ldw r1, r16+%lo(fio_d)
	blt r14, r1, .LBB13_68
.LBB13_38:
	lui r5, %hi(.L.str.10)
	addi r5, r5, %lo(.L.str.10)
	addi r3, sp, 68
	addi r4, r0, 32
	addi r7, sp, 28
	jal r0, .LBB13_67
.LBB13_39:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r3, r0, 2
	blt r1, r3, .LBB13_41
.LBB13_40:
	lui r3, %hi(fio_col)
	ldw r4, r3+%lo(fio_col)
	add r1, r1, r4
	addi r1, r1, -1
	stw r3+%lo(fio_col), r1
.LBB13_41:
	lui r3, %hi(fio_col)
	ldw r1, r3+%lo(fio_col)
	addi r4, r0, 1022
	bgt r1, r4, .LBB13_48
.LBB13_42:
	lui r4, %hi(fio_len)
	ldw r5, r4+%lo(fio_len)
	bge r5, r1, .LBB13_46
.LBB13_43:
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
.LBB13_44:
	addi r8, r5, 1
	add r5, r5, r6
	stb r5+0, r7
	add r5, r8, r0
	bne r1, r8, .LBB13_44
.LBB13_45:
	stw r4+%lo(fio_len), r1
	add r5, r1, r0
.LBB13_46:
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
	blt r1, r5, .LBB13_48
.LBB13_47:
	stw r4+%lo(fio_len), r6
.LBB13_48:
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	bge r3, r4, .LBB13_31
	jal r0, .LBB13_32
.LBB13_49:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	addi r1, r3, -1
	addi r4, r0, 3
	bgeu r1, r4, .LBB13_51
.LBB13_50:
	add r1, r3, r0
	jal r0, .LBB13_53
.LBB13_51:
	addi r1, r0, 4
	addi r4, r0, 5
	blt r3, r4, .LBB13_53
.LBB13_52:
	lui r4, %hi(fio_col)
	ldw r5, r4+%lo(fio_col)
	add r3, r3, r5
	addi r3, r3, -4
	stw r4+%lo(fio_col), r3
.LBB13_53:
	lui r3, %hi(fio_len)
	ldw r7, r3+%lo(fio_len)
	lui r4, %hi(fio_col)
	ldw r10, r4+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r8, r0, 32
	add r9, r0, r0
	jal r0, .LBB13_56
.LBB13_54:
	add r10, lr, r0
.LBB13_55:
	addi r9, r9, 1
	beq r9, r1, .LBB13_48
.LBB13_56:
	addi lr, sp, 100
	bgt r10, r5, .LBB13_55
.LBB13_57:
	bge r7, r10, .LBB13_60
.LBB13_58:
	addi r11, r7, 1
	add r7, r7, r6
	stb r7+0, r8
	add r7, r11, r0
	bne r10, r11, .LBB13_58
.LBB13_59:
	stw r3+%lo(fio_len), r10
	add r7, r10, r0
.LBB13_60:
	add lr, lr, r9
	ldbu lr, lr+0
	add r11, r10, r6
	stb r11+0, lr
	addi lr, r10, 1
	stw r4+%lo(fio_col), lr
	blt r10, r7, .LBB13_54
.LBB13_61:
	stw r3+%lo(fio_len), lr
	add r7, lr, r0
	jal r0, .LBB13_54
.LBB13_62:
	lui r1, %hi(fio_fmt)
	ldw r1, r1+%lo(fio_fmt)
	lui r3, %hi(fio_pos)
	ldw r3, r3+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+-1
	addi r4, r0, 48
	bne r3, r4, .LBB13_66
.LBB13_63:
	bne r11, r0, .LBB13_66
.LBB13_64:
	ldbu r1, r1+-2
	andi r1, r1, 255
	addi r3, r0, 46
	bne r1, r3, .LBB13_66
.LBB13_65:
	stb sp+68, r0
	jal r0, .LBB13_68
.LBB13_66:
	lui r5, %hi(.L.str.11)
	addi r5, r5, %lo(.L.str.11)
	addi r3, sp, 68
	addi r4, r0, 32
.LBB13_67:
	add r6, r11, r0
	jal r31, snprintf
.LBB13_68:
	lui r1, %hi(fio_sp)
	ldbu r1, r1+%lo(fio_sp)
	bne r1, r15, .LBB13_73
.LBB13_69:
	ldbu r1, sp+68
	beq r1, r0, .LBB13_73
.LBB13_70:
	addi r13, r0, 43
	beq r1, r13, .LBB13_73
.LBB13_71:
	addi r3, r0, 45
	beq r1, r3, .LBB13_73
.LBB13_72:
	addi r11, sp, 68
	addi r12, r11, 1
	add r3, r11, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r12, r0
	add r4, r11, r0
	jal r31, memmove
	stb sp+68, r13
.LBB13_73:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 68
	jal r31, fio_field
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	bge r3, r15, .LBB13_31
	jal r0, .LBB13_32
.Lfunc_end13:
	.size	f77_wr_i, .Lfunc_end13-f77_wr_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI13_0,@object
.LJTI13_0:
	.word	.LBB13_49
	.word	.LBB13_18
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_39
	.word	.LBB13_36
	.word	.LBB13_36
	.word	.LBB13_18
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
	.word	.LBB13_18
	.size	.LJTI13_0, 104
                                        # -- End function
	.text
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
	ble r12, r0, .LBB14_11
.LBB14_1:
	bge r12, r1, .LBB14_21
.LBB14_2:
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
	jal r0, .LBB14_5
.LBB14_3:
	add r10, r11, r0
.LBB14_4:
	addi r12, lr, -1
	ble lr, r5, .LBB14_33
.LBB14_5:
	add lr, r12, r0
	bgt r10, r4, .LBB14_4
.LBB14_6:
	bge r8, r10, .LBB14_9
.LBB14_7:
	addi r11, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, r11, r0
	bne r10, r11, .LBB14_7
.LBB14_8:
	stw r1+%lo(fio_len), r10
	add r8, r10, r0
.LBB14_9:
	add r11, r10, r6
	stb r11+0, r9
	addi r11, r10, 1
	stw r3+%lo(fio_col), r11
	blt r10, r8, .LBB14_3
.LBB14_10:
	stw r1+%lo(fio_len), r11
	add r8, r11, r0
	jal r0, .LBB14_3
.LBB14_11:
	ldbu r9, r11+0
	beq r9, r0, .LBB14_33
.LBB14_12:
	lui r1, %hi(fio_len)
	ldw r7, r1+%lo(fio_len)
	lui r3, %hi(fio_col)
	ldw r8, r3+%lo(fio_col)
	addi r4, r0, 1022
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	addi r6, r0, 32
	jal r0, .LBB14_15
.LBB14_13:
	add r8, r9, r0
.LBB14_14:
	addi r11, r11, 1
	ldbu r9, r11+0
	beq r9, r0, .LBB14_33
.LBB14_15:
	bgt r8, r4, .LBB14_14
.LBB14_16:
	bge r7, r8, .LBB14_19
.LBB14_17:
	addi r10, r7, 1
	add r7, r7, r5
	stb r7+0, r6
	add r7, r10, r0
	bne r8, r10, .LBB14_17
.LBB14_18:
	stw r1+%lo(fio_len), r8
	add r7, r8, r0
.LBB14_19:
	add r10, r8, r5
	stb r10+0, r9
	addi r9, r8, 1
	stw r3+%lo(fio_col), r9
	blt r8, r7, .LBB14_13
.LBB14_20:
	stw r1+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB14_13
.LBB14_21:
	sub r3, r12, r1
	addi r4, r0, 1
	lui r1, %hi(fio_col)
	blt r3, r4, .LBB14_23
.LBB14_22:
	ldw r4, r1+%lo(fio_col)
	add r3, r4, r3
	stw r1+%lo(fio_col), r3
.LBB14_23:
	ldbu r9, r11+0
	beq r9, r0, .LBB14_33
.LBB14_24:
	lui r3, %hi(fio_len)
	ldw r7, r3+%lo(fio_len)
	ldw r8, r1+%lo(fio_col)
	addi r4, r0, 1022
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	addi r6, r0, 32
	jal r0, .LBB14_27
.LBB14_25:
	add r8, r9, r0
.LBB14_26:
	addi r11, r11, 1
	ldbu r9, r11+0
	beq r9, r0, .LBB14_33
.LBB14_27:
	bgt r8, r4, .LBB14_26
.LBB14_28:
	bge r7, r8, .LBB14_31
.LBB14_29:
	addi r10, r7, 1
	add r7, r7, r5
	stb r7+0, r6
	add r7, r10, r0
	bne r8, r10, .LBB14_29
.LBB14_30:
	stw r3+%lo(fio_len), r8
	add r7, r8, r0
.LBB14_31:
	add r10, r8, r5
	stb r10+0, r9
	addi r9, r8, 1
	stw r1+%lo(fio_col), r9
	blt r8, r7, .LBB14_25
.LBB14_32:
	stw r3+%lo(fio_len), r9
	add r7, r9, r0
	jal r0, .LBB14_25
.LBB14_33:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end14:
	.size	fio_field, .Lfunc_end14-fio_field
                                        # -- End function
	.p2align	2                               # -- Begin function fio_plus
	.type	fio_plus,@function
fio_plus:                               # @fio_plus
# %bb.0:
	lui r1, %hi(fio_sp)
	ldbu r1, r1+%lo(fio_sp)
	addi r4, r0, 1
	bne r1, r4, .LBB15_6
.LBB15_1:
	ldbu r1, r3+0
	beq r1, r0, .LBB15_6
.LBB15_2:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	addi r13, r0, 43
	beq r1, r13, .LBB15_5
.LBB15_3:
	addi r4, r0, 45
	beq r1, r4, .LBB15_5
.LBB15_4:
	addi r11, r3, 1
	add r12, r3, r0
	jal r31, strlen
	addi r5, r1, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, memmove
	stb r12+0, r13
.LBB15_5:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB15_6:
	jalr r0, r31, 0
.Lfunc_end15:
	.size	fio_plus, .Lfunc_end15-fio_plus
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_wr_d
	.type	.LCPI16_0,@object
.LCPI16_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI16_0, 8
	.type	.LCPI16_1,@object
.LCPI16_1:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI16_1, 8
	.type	.LCPI16_2,@object
.LCPI16_2:
	.quad	0x4024000000000000              # double 10
	.size	.LCPI16_2, 8
	.type	.LCPI16_3,@object
.LCPI16_3:
	.quad	0x3fb999999999999a              # double 0.10000000000000001
	.size	.LCPI16_3, 8
	.text
	.globl	f77_wr_d
	.p2align	2
	.type	f77_wr_d,@function
f77_wr_d:                               # @f77_wr_d
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
	add r13, r4, r0
	add r12, r3, r0
	stw sp+104, r13
	stw sp+100, r3
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB16_2
.LBB16_1:
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r13, r0
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
	jal r0, fio_list_out_real
.LBB16_2:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB16_6
.LBB16_3:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB16_5
.LBB16_4:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	bgt r4, r0, .LBB16_6
.LBB16_5:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
.LBB16_6:
	beq r1, r0, .LBB16_80
.LBB16_7:
	addi r3, r0, 73
	beq r1, r3, .LBB16_16
.LBB16_8:
	addi r4, r0, 65
	bne r1, r4, .LBB16_11
.LBB16_9:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, r4, -1
	addi r5, r0, 7
	lui r1, %hi(fio_col)
	bgeu r3, r5, .LBB16_22
.LBB16_10:
	add r3, r4, r0
	jal r0, .LBB16_24
.LBB16_11:
	feq.d r4, r12, r12
	and r7, r4, r4
	xori r4, r7, 1
	lui r6, %hi(.LCPI16_0)
	addi r6, r6, %lo(.LCPI16_0)
	bne r4, r0, .LBB16_19
.LBB16_12:
	fsub.d r8, r12, r12
	ldw r5, r6+4
	ldw r4, r6+0
	feq.d r8, r8, r4
	beq r8, r0, .LBB16_19
.LBB16_13:
	addi r6, r0, 70
	beq r1, r6, .LBB16_41
.LBB16_14:
	addi r6, r0, 71
	beq r1, r6, .LBB16_33
.LBB16_15:
	bne r1, r3, .LBB16_51
.LBB16_16:
	fcvt.w.d r6, r12
	lui r5, %hi(.L.str.11)
	addi r5, r5, %lo(.L.str.11)
	addi r3, sp, 36
	addi r4, r0, 64
.LBB16_17:
	jal r31, snprintf
.LBB16_18:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, sp, 36
	jal r0, .LBB16_75
.LBB16_19:
	lui r1, %hi(fio_w)
	ldw r11, r1+%lo(fio_w)
	beq r7, r0, .LBB16_72
.LBB16_20:
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
	lui r4, %hi(.L.str.14)
	addi r4, r4, %lo(.L.str.14)
	lui r5, %hi(.L.str.13)
	addi r5, r5, %lo(.L.str.13)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r4, r4, r3
	beq r1, r0, .LBB16_73
.LBB16_21:
	stb sp+36, r1
	addi r1, sp, 36
	addi r3, r1, 1
	jal r0, .LBB16_74
.LBB16_22:
	addi r3, r0, 8
	addi r5, r0, 9
	blt r4, r5, .LBB16_24
.LBB16_23:
	ldw r5, r1+%lo(fio_col)
	add r4, r4, r5
	addi r4, r4, -8
	stw r1+%lo(fio_col), r4
.LBB16_24:
	lui r4, %hi(fio_len)
	ldw r8, r4+%lo(fio_len)
	ldw r10, r1+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	add r9, r0, r0
	jal r0, .LBB16_27
.LBB16_25:
	add r10, lr, r0
.LBB16_26:
	addi r9, r9, 1
	beq r9, r3, .LBB16_76
.LBB16_27:
	addi lr, sp, 100
	bgt r10, r5, .LBB16_26
.LBB16_28:
	bge r8, r10, .LBB16_31
.LBB16_29:
	addi r11, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, r11, r0
	bne r10, r11, .LBB16_29
.LBB16_30:
	stw r4+%lo(fio_len), r10
	add r8, r10, r0
.LBB16_31:
	add lr, lr, r9
	ldbu lr, lr+0
	add r11, r10, r6
	stb r11+0, lr
	addi lr, r10, 1
	stw r1+%lo(fio_col), lr
	blt r10, r8, .LBB16_25
.LBB16_32:
	stw r4+%lo(fio_len), lr
	add r8, lr, r0
	jal r0, .LBB16_25
.LBB16_33:
	lui r1, %hi(fio_d)
	ldw r1, r1+%lo(fio_d)
	addi r15, r0, 1
	sgt r3, r1, r15
	sub r3, r0, r3
	xori r6, r1, 1
	and r3, r6, r3
	xori r10, r3, 1
	lui r6, %hi(.LCPI16_1)
	addi r6, r6, %lo(.LCPI16_1)
	ldw r9, r6+4
	ldw r8, r6+0
	lui lr, %hi(.LCPI16_2)
	addi lr, lr, %lo(.LCPI16_2)
	ldw r7, lr+4
	ldw r6, lr+0
	add r16, r8, r0
	add r17, r9, r0
.LBB16_34:
	fmul.d r16, r16, r6
	addi r10, r10, -1
	bne r10, r0, .LBB16_34
.LBB16_35:
	flt.d r10, r12, r4
	fneg.d r18, r12
	xor lr, r19, r13
	sub r14, r0, r10
	and r10, lr, r14
	xor r11, r13, r10
	xor lr, r18, r12
	and lr, lr, r14
	xor r10, r12, lr
	feq.d r4, r12, r4
	bne r4, r0, .LBB16_38
.LBB16_36:
	lui r4, %hi(.LCPI16_3)
	addi r4, r4, %lo(.LCPI16_3)
	ldw r19, r4+4
	ldw r18, r4+0
	fle.d r4, r18, r10
	xori r4, r4, 1
	bne r4, r0, .LBB16_39
.LBB16_37:
	flt.d r4, r10, r16
	xori r4, r4, 1
	bne r4, r0, .LBB16_39
.LBB16_38:
	add r15, r0, r0
.LBB16_39:
	xori r4, r3, 1
	beq r15, r0, .LBB16_54
.LBB16_40:
	addi r11, sp, 36
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
	jal r0, .LBB16_70
.LBB16_41:
	lui r1, %hi(fio_scale)
	ldw r3, r1+%lo(fio_scale)
	addi r1, r0, 1
	blt r3, r1, .LBB16_45
.LBB16_42:
	addi r3, r3, 1
	lui r6, %hi(.LCPI16_2)
	addi r6, r6, %lo(.LCPI16_2)
	ldw r5, r6+4
	ldw r4, r6+0
.LBB16_43:
	fmul.d r12, r12, r4
	addi r3, r3, -1
	bgt r3, r1, .LBB16_43
.LBB16_44:
	add r3, r0, r0
.LBB16_45:
	addi r4, r0, -1
	bgt r3, r4, .LBB16_48
.LBB16_46:
	lui r6, %hi(.LCPI16_2)
	addi r6, r6, %lo(.LCPI16_2)
	ldw r5, r6+4
	ldw r4, r6+0
.LBB16_47:
	fdiv.d r12, r12, r4
	addi r6, r3, 1
	sltu r7, r6, r3
	add r3, r6, r0
	bne r7, r1, .LBB16_47
.LBB16_48:
	lui r14, %hi(fio_d)
	ldw r6, r14+%lo(fio_d)
	lui r5, %hi(.L.str.15)
	addi r5, r5, %lo(.L.str.15)
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
	ldw r1, r14+%lo(fio_d)
	bne r1, r0, .LBB16_50
.LBB16_49:
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	addi r3, sp, 36
	jal r31, strcat
.LBB16_50:
	addi r11, sp, 36
	jal r0, .LBB16_53
.LBB16_51:
	addi r3, r0, -2
	and r3, r1, r3
	addi r4, r0, 68
	bne r3, r4, .LBB16_57
.LBB16_52:
	lui r3, %hi(fio_d)
	ldw r4, r3+%lo(fio_d)
	addi r11, sp, 36
	add r3, r11, r0
	add r5, r12, r0
	add r6, r13, r0
	add r7, r1, r0
	jal r31, fio_efmt
.LBB16_53:
	add r3, r11, r0
	jal r31, fio_plus
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	add r3, r11, r0
	jal r31, fio_drop_zero
	jal r0, .LBB16_18
.LBB16_54:
	lui r3, %hi(fio_e)
	ldw r5, r3+%lo(fio_e)
	addi r3, r0, 0
	sgt lr, r5, r3
	addi r5, r5, 2
	sub lr, r0, lr
	xori r5, r5, 4
	and r5, r5, lr
	xori r16, r5, 4
	lui r17, %hi(fio_w)
	ldw r5, r17+%lo(fio_w)
	slt lr, r5, r16
	sub lr, r0, lr
	and lr, r16, lr
	xor lr, r16, lr
	fle.d r8, r8, r10
	xori r9, r8, 1
	add r8, r0, r0
	bne r9, r0, .LBB16_65
.LBB16_55:
	fle.d r8, r6, r10
	xori r8, r8, 1
	beq r8, r0, .LBB16_61
.LBB16_56:
	addi r8, r0, -1
	jal r0, .LBB16_65
.LBB16_57:
	addi r3, r0, 339
	beq r1, r3, .LBB16_59
.LBB16_58:
	addi r3, r0, 334
	bne r1, r3, .LBB16_60
.LBB16_59:
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
	jal r0, .LBB16_18
.LBB16_60:
	lui r5, %hi(.L.str.17)
	addi r5, r5, %lo(.L.str.17)
	addi r3, sp, 36
	addi r4, r0, 64
	add r6, r12, r0
	add r7, r13, r0
	jal r0, .LBB16_17
.LBB16_61:
	addi r8, r0, 1
.LBB16_62:
	add r9, r8, r0
	fdiv.d r10, r10, r6
	addi r8, r8, 1
	fle.d r14, r6, r10
	xori r14, r14, 1
	bne r14, r0, .LBB16_64
.LBB16_63:
	bgt r1, r9, .LBB16_62
.LBB16_64:
	sub r8, r0, r8
.LBB16_65:
	sub r11, r5, lr
	add r18, r8, r4
	sgt r1, r18, r3
	sub r1, r0, r1
	and r6, r18, r1
	lui r5, %hi(.L.str.15)
	addi r5, r5, %lo(.L.str.15)
	addi r14, sp, 20
	addi r4, r0, 16
	add r3, r14, r0
	jal r31, snprintf
	addi r3, sp, 36
	addi r4, r0, 64
	add r5, r14, r0
	add r6, r12, r0
	add r7, r13, r0
	jal r31, snprintf
	bgt r18, r0, .LBB16_67
.LBB16_66:
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	addi r3, sp, 36
	jal r31, strcat
.LBB16_67:
	addi r12, sp, 36
	add r3, r12, r0
	jal r31, fio_plus
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fio_drop_zero
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fio_field
	ldw r1, r17+%lo(fio_w)
	slt r1, r1, r16
	sub r1, r0, r1
	and r1, r16, r1
	xor r3, r16, r1
	jal r31, fio_blanks
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	blt r3, r4, .LBB16_69
.LBB16_68:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB16_69:
	ldw r1, r1+%lo(fio_rep)
	beq r1, r0, .LBB16_71
.LBB16_70:
	bne r15, r0, .LBB16_18
	jal r0, .LBB16_80
.LBB16_71:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
	bne r15, r0, .LBB16_18
	jal r0, .LBB16_80
.LBB16_72:
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
.LBB16_73:
	addi r3, sp, 36
.LBB16_74:
	jal r31, strcpy
	addi r3, sp, 36
	add r4, r11, r0
.LBB16_75:
	jal r31, fio_field
.LBB16_76:
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	blt r3, r4, .LBB16_78
.LBB16_77:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB16_78:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB16_80
.LBB16_79:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB16_80:
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
.Lfunc_end16:
	.size	f77_wr_d, .Lfunc_end16-f77_wr_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_list_out_real
	.type	.LCPI17_0,@object
.LCPI17_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI17_0, 8
	.type	.LCPI17_2,@object
.LCPI17_2:
	.quad	0x3fb999999999999a              # double 0.10000000000000001
	.size	.LCPI17_2, 8
	.type	.LCPI17_3,@object
.LCPI17_3:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI17_3, 8
	.type	.LCPI17_4,@object
.LCPI17_4:
	.quad	0x4024000000000000              # double 10
	.size	.LCPI17_4, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI17_1,@object
.LCPI17_1:
	.quad	0x41cdcd6500000000              # double 1.0E+9
	.quad	0x4376345785d8a000              # double 1.0E+17
	.size	.LCPI17_1, 16
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
	lui r1, %hi(.LCPI17_0)
	addi r1, r1, %lo(.LCPI17_0)
	addi r3, r0, 0
	sne r16, r5, r3
	lui r14, %hi(fio_col)
	ldw r3, r14+%lo(fio_col)
	addi r15, r0, 1022
	bgt r3, r15, .LBB17_6
.LBB17_1:
	lui r4, %hi(fio_len)
	ldw r6, r4+%lo(fio_len)
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
	bge r6, r3, .LBB17_4
.LBB17_2:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r3, r9, .LBB17_2
.LBB17_3:
	stw r4+%lo(fio_len), r3
	add r6, r3, r0
.LBB17_4:
	add r7, r3, r7
	stb r7+0, r8
	addi r7, r3, 1
	stw r14+%lo(fio_col), r7
	blt r3, r6, .LBB17_6
.LBB17_5:
	stw r4+%lo(fio_len), r7
.LBB17_6:
	ldw r19, r1+4
	ldw r18, r1+0
	sub r17, r0, r16
	feq.d r1, r12, r12
	and r1, r1, r1
	xori r1, r1, 1
	bne r1, r0, .LBB17_26
.LBB17_7:
	fsub.d r6, r12, r12
	feq.d r3, r6, r18
	beq r3, r0, .LBB17_26
.LBB17_8:
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
	bne r1, r0, .LBB17_11
.LBB17_9:
	lui r1, %hi(.LCPI17_2)
	addi r1, r1, %lo(.LCPI17_2)
	ldw r9, r1+4
	ldw r8, r1+0
	fle.d r1, r8, r6
	xori r1, r1, 1
	bne r1, r0, .LBB17_29
.LBB17_10:
	andi r1, r17, 8
	lui r3, %hi(.LCPI17_1)
	addi r3, r3, %lo(.LCPI17_1)
	add r1, r3, r1
	ldw r9, r1+4
	ldw r8, r1+0
	flt.d r1, r6, r8
	xori r1, r1, 1
	bne r1, r0, .LBB17_29
.LBB17_11:
	addi r17, r0, 1
	lui r1, %hi(.LCPI17_3)
	addi r1, r1, %lo(.LCPI17_3)
	ldw r5, r1+4
	ldw r4, r1+0
	fle.d r1, r4, r6
	xori r3, r1, 1
	add r1, r17, r0
	bne r3, r0, .LBB17_14
.LBB17_12:
	lui r1, %hi(.LCPI17_4)
	addi r1, r1, %lo(.LCPI17_4)
	ldw r9, r1+4
	ldw r8, r1+0
	add r10, r6, r0
	add r11, r7, r0
	add r1, r0, r0
.LBB17_13:
	fdiv.d r10, r10, r8
	addi r1, r1, 1
	fle.d r3, r4, r10
	bne r3, r0, .LBB17_13
.LBB17_14:
	flt.d r3, r6, r4
	feq.d r4, r12, r18
	xori r4, r4, 1
	and r22, r4, r3
	sub r3, r0, r22
	and r3, r1, r3
	xor r1, r1, r3
	sub r6, r21, r1
	lui r5, %hi(.L.str.15)
	addi r5, r5, %lo(.L.str.15)
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
	bne r22, r17, .LBB17_17
.LBB17_15:
	flt.d r11, r12, r18
	addi r3, sp, 32
	jal r31, strlen
	add r3, r11, r21
	addi r3, r3, 2
	bleu r1, r3, .LBB17_17
.LBB17_16:
	addi r1, sp, 32
	add r1, r1, r3
	stb r1+0, r0
.LBB17_17:
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
	jal r0, .LBB17_20
.LBB17_18:
	add r7, r9, r0
.LBB17_19:
	addi r11, r8, -1
	bleu r8, r3, .LBB17_28
.LBB17_20:
	add r8, r11, r0
	bgt r7, r15, .LBB17_19
.LBB17_21:
	bge r6, r7, .LBB17_24
.LBB17_22:
	addi r9, r6, 1
	add r6, r6, r4
	stb r6+0, r5
	add r6, r9, r0
	bne r7, r9, .LBB17_22
.LBB17_23:
	stw r1+%lo(fio_len), r7
	add r6, r7, r0
.LBB17_24:
	add r9, r7, r4
	stb r9+0, r5
	addi r9, r7, 1
	stw r14+%lo(fio_col), r9
	blt r7, r6, .LBB17_18
.LBB17_25:
	stw r1+%lo(fio_len), r9
	add r6, r9, r0
	jal r0, .LBB17_18
.LBB17_26:
	flt.d r3, r12, r18
	lui r4, %hi(.L.str.13)
	addi r4, r4, %lo(.L.str.13)
	lui r5, %hi(.L.str.115)
	addi r5, r5, %lo(.L.str.115)
	xor r5, r5, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r3, r4, r3
	lui r4, %hi(.L.str.12)
	addi r4, r4, %lo(.L.str.12)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r3, r3, r1
.LBB17_27:
	andi r1, r17, 9
	addi r4, r1, 16
	jal r31, fio_field
.LBB17_28:
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
.LBB17_29:
	addi r6, r21, -1
	lui r1, %hi(.L.str.116)
	addi r1, r1, %lo(.L.str.116)
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
	beq r15, r0, .LBB17_27
.LBB17_30:
	beq r1, r0, .LBB17_27
.LBB17_31:
	addi r11, r1, 2
	add r3, r11, r0
	jal r31, strlen
	addi r3, sp, 32
	addi r4, r0, 2
	bgt r1, r4, .LBB17_27
.LBB17_32:
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
	jal r0, .LBB17_27
.Lfunc_end17:
	.size	fio_list_out_real, .Lfunc_end17-fio_list_out_real
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
	blt r12, r3, .LBB18_11
.LBB18_1:
	addi r3, r12, 1
	bne r1, r3, .LBB18_11
.LBB18_2:
	ldbu r3, r11+0
	addi r4, r0, 43
	beq r3, r4, .LBB18_7
.LBB18_3:
	addi r4, r0, 45
	beq r3, r4, .LBB18_7
.LBB18_4:
	addi r4, r0, 48
	bne r3, r4, .LBB18_11
.LBB18_5:
	ldbu r3, r11+1
	addi r4, r0, 46
	bne r3, r4, .LBB18_11
.LBB18_6:
	addi r4, r11, 1
	add r3, r11, r0
	add r5, r1, r0
	jal r0, .LBB18_10
.LBB18_7:
	ldbu r3, r11+1
	addi r4, r0, 48
	bne r3, r4, .LBB18_11
.LBB18_8:
	ldbu r3, r11+2
	addi r4, r0, 46
	bne r3, r4, .LBB18_11
.LBB18_9:
	addi r3, r11, 1
	addi r4, r11, 2
	addi r5, r1, -1
.LBB18_10:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, memmove
.LBB18_11:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end18:
	.size	fio_drop_zero, .Lfunc_end18-fio_drop_zero
                                        # -- End function
	.p2align	2                               # -- Begin function fio_blanks
	.type	fio_blanks,@function
fio_blanks:                             # @fio_blanks
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	addi r1, r0, 1
	blt r3, r1, .LBB19_10
.LBB19_1:
	lui r4, %hi(fio_len)
	ldw r9, r4+%lo(fio_len)
	lui r5, %hi(fio_col)
	ldw r10, r5+%lo(fio_col)
	addi r6, r0, 1022
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
	jal r0, .LBB19_4
.LBB19_2:
	add r10, r3, r0
.LBB19_3:
	addi r3, lr, -1
	ble lr, r1, .LBB19_10
.LBB19_4:
	add lr, r3, r0
	bgt r10, r6, .LBB19_3
.LBB19_5:
	bge r9, r10, .LBB19_8
.LBB19_6:
	addi r3, r9, 1
	add r9, r9, r7
	stb r9+0, r8
	add r9, r3, r0
	bne r10, r3, .LBB19_6
.LBB19_7:
	stw r4+%lo(fio_len), r10
	add r9, r10, r0
.LBB19_8:
	add r3, r10, r7
	stb r3+0, r8
	addi r3, r10, 1
	stw r5+%lo(fio_col), r3
	blt r10, r9, .LBB19_2
.LBB19_9:
	stw r4+%lo(fio_len), r3
	add r9, r3, r0
	jal r0, .LBB19_2
.LBB19_10:
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.Lfunc_end19:
	.size	fio_blanks, .Lfunc_end19-fio_blanks
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_efmt
	.type	.LCPI20_0,@object
.LCPI20_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI20_0, 8
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
	lui r5, %hi(.L.str.116)
	addi r5, r5, %lo(.L.str.116)
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
	jal r0, .LBB20_3
.LBB20_1:
	addi r7, sp, 32
	add r7, r7, r15
	stb r7+40, r6
	addi r15, r15, 1
.LBB20_2:
	addi r3, r3, 1
.LBB20_3:
	ldbu r6, r3+-1
	bgt r6, r1, .LBB20_6
.LBB20_4:
	beq r6, r16, .LBB20_2
.LBB20_5:
	bne r6, r0, .LBB20_1
	jal r0, .LBB20_9
.LBB20_6:
	beq r6, r4, .LBB20_8
.LBB20_7:
	bne r6, r5, .LBB20_1
.LBB20_8:
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	addi r1, r1, 1
	jal r0, .LBB20_10
.LBB20_9:
	add r1, r17, r0
.LBB20_10:
	lui r3, %hi(.LCPI20_0)
	addi r3, r3, %lo(.LCPI20_0)
	ldw r5, r3+4
	ldw r4, r3+0
	feq.d r3, r12, r4
	sub r3, r0, r3
	and r3, r1, r3
	add r6, r14, r0
	bne r22, r21, .LBB20_12
.LBB20_11:
	addi r6, r14, 1
	stb r14+0, r21
.LBB20_12:
	xor r1, r1, r3
	blt r20, r17, .LBB20_17
.LBB20_13:
	blt r15, r17, .LBB20_22
.LBB20_14:
	xor r3, r19, r15
	slt r7, r19, r15
	sub r7, r0, r7
	and r3, r3, r7
	xor r3, r15, r3
	addi r7, sp, 32
	addi r8, r7, 40
	add r7, r0, r0
.LBB20_15:
	add r9, r6, r7
	add r10, r8, r7
	ldbu r10, r10+0
	stb r9+0, r10
	addi r7, r7, 1
	bne r3, r7, .LBB20_15
.LBB20_16:
	add r6, r6, r7
	jal r0, .LBB20_23
.LBB20_17:
	addi r8, r0, 48
	stb r6+0, r8
	addi r9, r6, 2
	stb r6+1, r16
	addi r7, r0, 55
	beq r20, r0, .LBB20_28
.LBB20_18:
	sub r3, r9, r14
	bgt r3, r7, .LBB20_28
.LBB20_19:
	sub r3, r6, r14
	addi lr, r3, 3
	addi r6, r0, -2
	addi r10, r0, 56
.LBB20_20:
	addi r3, r9, 1
	stb r9+0, r8
	bgt r20, r6, .LBB20_29
.LBB20_21:
	add r16, lr, r0
	addi r20, r20, 1
	addi lr, lr, 1
	add r9, r3, r0
	blt r16, r10, .LBB20_20
	jal r0, .LBB20_29
.LBB20_22:
	add r7, r0, r0
.LBB20_23:
	stb r6+0, r16
	addi r3, r6, 1
	bge r7, r15, .LBB20_35
.LBB20_24:
	sub r8, r3, r14
	addi r9, r0, 55
	bgt r8, r9, .LBB20_35
.LBB20_25:
	sub r6, r6, r14
	addi r9, r6, 2
	addi r6, sp, 32
	addi r6, r6, 40
	addi r8, r0, 56
.LBB20_26:
	add r10, r9, r0
	add r9, r6, r7
	ldbu r9, r9+0
	stb r3+0, r9
	addi r7, r7, 1
	addi r3, r3, 1
	bge r7, r15, .LBB20_35
.LBB20_27:
	addi r9, r10, 1
	blt r10, r8, .LBB20_26
	jal r0, .LBB20_35
.LBB20_28:
	add r3, r9, r0
.LBB20_29:
	blt r15, r17, .LBB20_35
.LBB20_30:
	sub r8, r3, r14
	bgt r8, r7, .LBB20_35
.LBB20_31:
	addi r6, sp, 32
	addi r6, r6, 40
	addi r7, r8, 1
	addi r8, r0, 56
	add r9, r0, r0
.LBB20_32:
	add r10, r3, r9
	add lr, r6, r9
	ldbu lr, lr+0
	stb r10+0, lr
	addi r10, r9, 1
	bge r10, r15, .LBB20_34
.LBB20_33:
	add lr, r7, r9
	add r9, r10, r0
	blt lr, r8, .LBB20_32
.LBB20_34:
	add r3, r3, r10
.LBB20_35:
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
	bne r4, r0, .LBB20_38
.LBB20_36:
	addi r1, r5, -100
	addi r8, r0, 900
	bgeu r1, r8, .LBB20_38
.LBB20_37:
	add r1, r3, r0
	jal r0, .LBB20_39
.LBB20_38:
	addi r1, r3, 1
	stb r3+0, r11
.LBB20_39:
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
.LBB20_40:
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
	bltu r5, r8, .LBB20_42
.LBB20_41:
	add r5, lr, r0
	bltu r10, r9, .LBB20_40
.LBB20_42:
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
	bge r3, r4, .LBB20_46
.LBB20_43:
	add r5, r5, r3
	addi r6, r0, 48
	add r7, r4, r0
.LBB20_44:
	stb r5+0, r6
	addi r5, r5, 1
	addi r7, r7, -1
	bne r3, r7, .LBB20_44
.LBB20_45:
	add r3, r4, r0
.LBB20_46:
	addi r1, r1, 1
	addi r4, sp, 96
	addi r4, r4, -1
.LBB20_47:
	add r5, r3, r0
	addi r3, r3, -1
	add r6, r4, r5
	ldbu r6, r6+0
	stb r1+0, r6
	addi r1, r1, 1
	bgt r5, r17, .LBB20_47
.LBB20_48:
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
.Lfunc_end20:
	.size	fio_efmt, .Lfunc_end20-fio_efmt
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_esn
	.type	.LCPI21_0,@object
.LCPI21_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI21_0, 8
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
	lui r5, %hi(.L.str.117)
	addi r5, r5, %lo(.L.str.117)
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
	lui r3, %hi(.LCPI21_0)
	addi r3, r3, %lo(.LCPI21_0)
	ldw r27, r3+4
	ldw r26, r3+0
	feq.d r3, r14, r26
	bne r3, r0, .LBB21_3
.LBB21_1:
	beq r1, r0, .LBB21_3
.LBB21_2:
	addi r3, r1, 1
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	add r16, r1, r0
.LBB21_3:
	xori r24, r17, 30
	lui r20, 699051
	beq r13, r0, .LBB21_6
.LBB21_4:
	addi r1, r0, -2
	blt r16, r0, .LBB21_7
.LBB21_5:
	addi r3, r20, -1365
	mulhu r3, r16, r3
	and r1, r3, r1
	srli r3, r3, 1
	add r1, r1, r3
	sub r1, r16, r1
	sub r1, r1, r16
	jal r0, .LBB21_8
.LBB21_6:
	addi r21, r0, 1
	jal r0, .LBB21_9
.LBB21_7:
	addi r3, r0, 2
	sub r3, r3, r16
	addi r4, r20, -1365
	mulhu r3, r3, r4
	and r1, r3, r1
	srli r3, r3, 1
	add r1, r1, r3
.LBB21_8:
	add r1, r16, r1
	addi r21, r1, 1
.LBB21_9:
	add r1, r24, r21
	addi r6, r1, -1
	lui r5, %hi(.L.str.116)
	addi r5, r5, %lo(.L.str.116)
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
	jal r0, .LBB21_11
.LBB21_10:
	addi r3, r3, 1
.LBB21_11:
	ldbu r1, r3+-1
	beq r1, r0, .LBB21_17
.LBB21_12:
	beq r1, r11, .LBB21_16
.LBB21_13:
	beq r1, r18, .LBB21_10
.LBB21_14:
	bgt r17, r18, .LBB21_10
.LBB21_15:
	addi r4, r17, 1
	addi r5, sp, 16
	add r5, r5, r17
	stb r5+0, r1
	add r17, r4, r0
	jal r0, .LBB21_10
.LBB21_16:
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	jal r0, .LBB21_18
.LBB21_17:
	add r1, r0, r0
.LBB21_18:
	feq.d r3, r14, r26
	sub r3, r0, r3
	and r3, r1, r3
	xor r1, r1, r3
	beq r13, r0, .LBB21_28
.LBB21_19:
	beq r1, r16, .LBB21_26
.LBB21_20:
	blt r1, r0, .LBB21_22
.LBB21_21:
	addi r3, r20, -1365
	mulhu r3, r1, r3
	addi r4, r0, -2
	and r4, r3, r4
	srli r3, r3, 1
	add r3, r4, r3
	sub r3, r1, r3
	sub r3, r3, r1
	jal r0, .LBB21_23
.LBB21_22:
	addi r3, r0, 2
	sub r3, r3, r1
	addi r4, r20, -1365
	mulhu r3, r3, r4
	addi r4, r0, -2
	and r4, r3, r4
	srli r3, r3, 1
	add r3, r4, r3
.LBB21_23:
	add r3, r1, r3
	addi r21, r3, 1
	add r3, r21, r24
	xori r4, r3, 47
	addi r5, r0, 47
	slt r3, r3, r5
	sub r3, r0, r3
	and r3, r4, r3
	xori r3, r3, 47
	bge r17, r3, .LBB21_26
.LBB21_24:
	addi r4, r0, 48
.LBB21_25:
	add r5, r17, r0
	addi r17, r17, 1
	addi r6, sp, 16
	add r5, r6, r5
	stb r5+0, r4
	bne r3, r17, .LBB21_25
.LBB21_26:
	blt r1, r0, .LBB21_53
.LBB21_27:
	addi r3, r20, -1365
	mulhu r1, r1, r3
	addi r3, r0, -2
	and r3, r1, r3
	srli r1, r1, 1
	add r1, r3, r1
.LBB21_28:
	add r4, r12, r0
	bne r23, r22, .LBB21_30
.LBB21_29:
	addi r4, r12, 1
	stb r12+0, r22
.LBB21_30:
	xor r3, r21, r17
	slt r5, r21, r17
	sub r5, r0, r5
	and r3, r3, r5
	xor r5, r17, r3
	add r6, r0, r0
	addi r3, r0, 1
	blt r5, r3, .LBB21_33
.LBB21_31:
	add r7, r4, r6
	addi r8, sp, 16
	add r8, r8, r6
	ldbu r8, r8+0
	stb r7+0, r8
	addi r6, r6, 1
	bne r5, r6, .LBB21_31
.LBB21_32:
	add r4, r4, r6
.LBB21_33:
	stb r4+0, r18
	addi r5, r4, 1
	bge r6, r17, .LBB21_39
.LBB21_34:
	sub r7, r5, r12
	addi r8, r0, 55
	bgt r7, r8, .LBB21_39
.LBB21_35:
	sub r4, r4, r12
	addi r7, r4, 2
	addi r4, r0, 56
.LBB21_36:
	add r8, r7, r0
	addi r7, sp, 16
	add r7, r7, r6
	ldbu r7, r7+0
	stb r5+0, r7
	addi r6, r6, 1
	addi r5, r5, 1
	bge r6, r17, .LBB21_38
.LBB21_37:
	addi r7, r8, 1
	blt r8, r4, .LBB21_36
.LBB21_38:
	addi r4, r5, -1
.LBB21_39:
	srai r6, r1, 31
	xor r7, r1, r6
	sub r7, r7, r6
	lui r6, %hi(fio_e)
	ldw r6, r6+%lo(fio_e)
	addi r8, r0, 100
	sltu r8, r7, r8
	bne r6, r0, .LBB21_42
.LBB21_40:
	addi r9, r7, -100
	addi r10, r0, 900
	bgeu r9, r10, .LBB21_42
.LBB21_41:
	add r4, r5, r0
	jal r0, .LBB21_43
.LBB21_42:
	addi r4, r4, 2
	stb r5+0, r11
.LBB21_43:
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
.LBB21_44:
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
	bltu r7, r9, .LBB21_46
.LBB21_45:
	add r7, r11, r0
	bltu lr, r10, .LBB21_44
.LBB21_46:
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
	bge r1, r5, .LBB21_50
.LBB21_47:
	add r6, r6, r1
	addi r7, r0, 48
	add r8, r5, r0
.LBB21_48:
	stb r6+0, r7
	addi r6, r6, 1
	addi r8, r8, -1
	bne r1, r8, .LBB21_48
.LBB21_49:
	add r1, r5, r0
.LBB21_50:
	addi r4, r4, 1
	addi r5, sp, 144
	addi r5, r5, -1
.LBB21_51:
	add r6, r1, r0
	addi r1, r1, -1
	add r7, r5, r6
	ldbu r7, r7+0
	stb r4+0, r7
	addi r4, r4, 1
	bgt r6, r3, .LBB21_51
.LBB21_52:
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
.LBB21_53:
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
	beq r23, r22, .LBB21_29
	jal r0, .LBB21_30
.Lfunc_end21:
	.size	fio_esn, .Lfunc_end21-fio_esn
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
	stw sp+20, r3
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB22_2
.LBB22_1:
	fcvt.d.s r4, r3
	add r3, r4, r0
	add r4, r5, r0
	add r5, r0, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fio_list_out_real
.LBB22_2:
	lui r1, %hi(fio_eor)
	ldbu r4, r1+%lo(fio_eor)
	lui r11, %hi(fio_rep)
	add r1, r0, r0
	bne r4, r0, .LBB22_5
.LBB22_3:
	lui r4, %hi(fio_desc)
	ldw r1, r4+%lo(fio_desc)
	beq r1, r0, .LBB22_8
.LBB22_4:
	ldw r5, r11+%lo(fio_rep)
	ble r5, r0, .LBB22_8
.LBB22_5:
	addi r4, r0, 65
	bne r1, r4, .LBB22_9
.LBB22_6:
	lui r1, %hi(fio_w)
	ldw r4, r1+%lo(fio_w)
	addi r3, r4, -1
	addi r5, r0, 3
	lui r1, %hi(fio_col)
	bgeu r3, r5, .LBB22_10
.LBB22_7:
	add r3, r4, r0
	jal r0, .LBB22_12
.LBB22_8:
	stw r4+%lo(fio_desc), r0
	add r12, r3, r0
	jal r31, fio_next_desc
	add r3, r12, r0
	addi r4, r0, 65
	beq r1, r4, .LBB22_6
.LBB22_9:
	fcvt.d.s r4, r3
	add r3, r4, r0
	add r4, r5, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_wr_d
.LBB22_10:
	addi r3, r0, 4
	addi r5, r0, 5
	blt r4, r5, .LBB22_12
.LBB22_11:
	ldw r5, r1+%lo(fio_col)
	add r4, r4, r5
	addi r4, r4, -4
	stw r1+%lo(fio_col), r4
.LBB22_12:
	lui r4, %hi(fio_len)
	ldw r8, r4+%lo(fio_len)
	ldw r10, r1+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r7, r0, 32
	add r9, r0, r0
	jal r0, .LBB22_15
.LBB22_13:
	add r10, lr, r0
.LBB22_14:
	addi r9, r9, 1
	beq r9, r3, .LBB22_21
.LBB22_15:
	addi lr, sp, 20
	bgt r10, r5, .LBB22_14
.LBB22_16:
	bge r8, r10, .LBB22_19
.LBB22_17:
	addi r12, r8, 1
	add r8, r8, r6
	stb r8+0, r7
	add r8, r12, r0
	bne r10, r12, .LBB22_17
.LBB22_18:
	stw r4+%lo(fio_len), r10
	add r8, r10, r0
.LBB22_19:
	add lr, lr, r9
	ldbu lr, lr+0
	add r12, r10, r6
	stb r12+0, lr
	addi lr, r10, 1
	stw r1+%lo(fio_col), lr
	blt r10, r8, .LBB22_13
.LBB22_20:
	stw r4+%lo(fio_len), lr
	add r8, lr, r0
	jal r0, .LBB22_13
.LBB22_21:
	ldw r1, r11+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB22_23
.LBB22_22:
	addi r1, r1, -1
	stw r11+%lo(fio_rep), r1
.LBB22_23:
	ldw r1, r11+%lo(fio_rep)
	bne r1, r0, .LBB22_25
.LBB22_24:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB22_25:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end22:
	.size	f77_wr_r, .Lfunc_end22-f77_wr_r
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
	beq r1, r0, .LBB23_2
.LBB23_1:
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
.LBB23_2:
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
.Lfunc_end23:
	.size	f77_wr_c, .Lfunc_end23-f77_wr_c
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
	bgt r1, r17, .LBB24_6
.LBB24_1:
	ldw r6, r15+%lo(fio_len)
	addi r5, r0, 32
	bge r6, r1, .LBB24_4
.LBB24_2:
	addi r7, r6, 1
	add r6, r6, r16
	stb r6+0, r5
	add r6, r7, r0
	bne r1, r7, .LBB24_2
.LBB24_3:
	stw r15+%lo(fio_len), r1
	add r6, r1, r0
.LBB24_4:
	add r7, r1, r16
	stb r7+0, r5
	addi r5, r1, 1
	stw r14+%lo(fio_col), r5
	blt r1, r6, .LBB24_6
.LBB24_5:
	stw r15+%lo(fio_len), r5
.LBB24_6:
	ldw r18, r15+%lo(fio_len)
	ldw r1, r14+%lo(fio_col)
	bgt r1, r17, .LBB24_13
.LBB24_7:
	add r5, r18, r0
	bge r18, r1, .LBB24_11
.LBB24_8:
	addi r5, r0, 32
	add r6, r18, r0
.LBB24_9:
	addi r7, r6, 1
	add r6, r6, r16
	stb r6+0, r5
	add r6, r7, r0
	bne r1, r7, .LBB24_9
.LBB24_10:
	stw r15+%lo(fio_len), r1
	add r5, r1, r0
.LBB24_11:
	add r6, r1, r16
	addi r7, r0, 40
	stb r6+0, r7
	addi r6, r1, 1
	stw r14+%lo(fio_col), r6
	blt r1, r5, .LBB24_13
.LBB24_12:
	stw r15+%lo(fio_len), r6
.LBB24_13:
	ldw r20, r15+%lo(fio_len)
	add r5, r11, r0
	jal r31, fio_list_out_real
	ldw r1, r15+%lo(fio_len)
	add r3, r20, r0
	bge r20, r1, .LBB24_18
.LBB24_14:
	addi r4, r0, 32
	add r3, r20, r0
.LBB24_15:
	add r5, r3, r16
	ldbu r5, r5+0
	bne r5, r4, .LBB24_18
.LBB24_16:
	addi r3, r3, 1
	bne r1, r3, .LBB24_15
.LBB24_17:
	add r3, r1, r0
.LBB24_18:
	ble r1, r3, .LBB24_23
.LBB24_19:
	lui r4, %hi(fio_line-1)
	addi r4, r4, %lo(fio_line-1)
	addi r5, r0, 32
.LBB24_20:
	add r6, r1, r4
	ldbu r6, r6+0
	bne r6, r5, .LBB24_23
.LBB24_21:
	addi r1, r1, -1
	bgt r1, r3, .LBB24_20
.LBB24_22:
	add r1, r3, r0
.LBB24_23:
	sub r4, r1, r3
	addi r19, r0, 1
	blt r4, r19, .LBB24_26
.LBB24_24:
	add r5, r20, r16
.LBB24_25:
	add r6, r3, r16
	ldbu r6, r6+0
	stb r5+0, r6
	addi r3, r3, 1
	addi r5, r5, 1
	bne r1, r3, .LBB24_25
.LBB24_26:
	add r1, r4, r20
	stw r15+%lo(fio_len), r1
	stw r14+%lo(fio_col), r1
	bgt r1, r17, .LBB24_28
.LBB24_27:
	add r3, r1, r16
	addi r4, r0, 44
	stb r3+0, r4
	addi r1, r1, 1
	stw r14+%lo(fio_col), r1
	stw r15+%lo(fio_len), r1
.LBB24_28:
	ldw r20, r15+%lo(fio_len)
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, fio_list_out_real
	ldw r1, r15+%lo(fio_len)
	add r3, r20, r0
	bge r20, r1, .LBB24_33
.LBB24_29:
	addi r4, r0, 32
	add r3, r20, r0
.LBB24_30:
	add r5, r3, r16
	ldbu r5, r5+0
	bne r5, r4, .LBB24_33
.LBB24_31:
	addi r3, r3, 1
	bne r1, r3, .LBB24_30
.LBB24_32:
	add r3, r1, r0
.LBB24_33:
	ble r1, r3, .LBB24_38
.LBB24_34:
	lui r4, %hi(fio_line-1)
	addi r4, r4, %lo(fio_line-1)
	addi r5, r0, 32
.LBB24_35:
	add r6, r1, r4
	ldbu r6, r6+0
	bne r6, r5, .LBB24_38
.LBB24_36:
	addi r1, r1, -1
	bgt r1, r3, .LBB24_35
.LBB24_37:
	add r1, r3, r0
.LBB24_38:
	sub r4, r1, r3
	blt r4, r19, .LBB24_41
.LBB24_39:
	add r5, r20, r16
.LBB24_40:
	add r6, r3, r16
	ldbu r6, r6+0
	stb r5+0, r6
	addi r3, r3, 1
	addi r5, r5, 1
	bne r1, r3, .LBB24_40
.LBB24_41:
	add r1, r4, r20
	stw r15+%lo(fio_len), r1
	stw r14+%lo(fio_col), r1
	bgt r1, r17, .LBB24_43
.LBB24_42:
	add r3, r1, r16
	addi r4, r0, 41
	stb r3+0, r4
	addi r1, r1, 1
	stw r14+%lo(fio_col), r1
	stw r15+%lo(fio_len), r1
.LBB24_43:
	ldw r1, r15+%lo(fio_len)
	sub r5, r1, r18
	addi r3, r0, 0
	seq r3, r11, r3
	sub r3, r0, r3
	andi r3, r3, 22
	xori r3, r3, 53
	sub r11, r3, r5
	blt r11, r19, .LBB24_46
.LBB24_44:
	add r1, r11, r1
	bgt r1, r17, .LBB24_46
.LBB24_45:
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
.LBB24_46:
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
.Lfunc_end24:
	.size	fio_list_cplx, .Lfunc_end24-fio_list_cplx
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
	beq r1, r0, .LBB25_2
.LBB25_1:
	addi r7, r0, 1
	add r5, r12, r0
	add r6, r11, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, fio_list_cplx
.LBB25_2:
	jal r31, f77_wr_d
	add r3, r12, r0
	add r4, r11, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_wr_d
.Lfunc_end25:
	.size	f77_wr_z, .Lfunc_end25-f77_wr_z
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
	beq r1, r0, .LBB26_4
.LBB26_1:
	lui r1, %hi(fio_unit)
	ldw r5, r1+%lo(fio_unit)
	addi r1, r5, -100
	addi r6, r0, -99
	bltu r1, r6, .LBB26_11
.LBB26_2:
	lui r1, %hi(fio_ibuf)
	ldw r6, r1+%lo(fio_ibuf)
	add r1, r0, r0
	bne r6, r0, .LBB26_12
.LBB26_3:
	slli r1, r5, 2
	lui r5, %hi(fio_udelim)
	addi r5, r5, %lo(fio_udelim)
	add r1, r1, r5
	ldw r1, r1+0
	jal r0, .LBB26_12
.LBB26_4:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB26_72
.LBB26_5:
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB26_7
.LBB26_6:
	ldw r1, r12+%lo(fio_rep)
	bgt r1, r0, .LBB26_8
.LBB26_7:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	add r14, r4, r0
	jal r31, fio_next_desc
	add r4, r14, r0
	add r3, r13, r0
	beq r1, r0, .LBB26_72
.LBB26_8:
	lui r1, %hi(fio_w)
	ldw r5, r1+%lo(fio_w)
	addi r1, r0, 1
	blt r5, r1, .LBB26_73
.LBB26_9:
	bge r5, r4, .LBB26_73
.LBB26_10:
	add r4, r5, r0
	jal r0, .LBB26_76
.LBB26_11:
	add r1, r0, r0
.LBB26_12:
	lui r5, %hi(fio_first_item)
	lui r6, %hi(fio_prev_char)
	beq r1, r0, .LBB26_52
.LBB26_13:
	lui r8, %hi(fio_col)
	ldw lr, r8+%lo(fio_col)
	addi r10, r0, 1022
	lui r7, %hi(fio_len)
	lui r9, %hi(fio_line)
	addi r9, r9, %lo(fio_line)
	bgt lr, r10, .LBB26_19
.LBB26_14:
	ldw r12, r7+%lo(fio_len)
	addi r11, r0, 32
	bge r12, lr, .LBB26_17
.LBB26_15:
	addi r13, r12, 1
	add r12, r12, r9
	stb r12+0, r11
	add r12, r13, r0
	bne lr, r13, .LBB26_15
.LBB26_16:
	stw r7+%lo(fio_len), lr
	add r12, lr, r0
.LBB26_17:
	add r13, lr, r9
	stb r13+0, r11
	addi r11, lr, 1
	stw r8+%lo(fio_col), r11
	blt lr, r12, .LBB26_19
.LBB26_18:
	stw r7+%lo(fio_len), r11
.LBB26_19:
	ldw lr, r8+%lo(fio_col)
	bgt lr, r10, .LBB26_26
.LBB26_20:
	ldw r11, r7+%lo(fio_len)
	bge r11, lr, .LBB26_24
.LBB26_21:
	addi r12, r0, 32
.LBB26_22:
	addi r13, r11, 1
	add r11, r11, r9
	stb r11+0, r12
	add r11, r13, r0
	bne lr, r13, .LBB26_22
.LBB26_23:
	stw r7+%lo(fio_len), lr
	add r11, lr, r0
.LBB26_24:
	add r12, lr, r9
	stb r12+0, r1
	addi r12, lr, 1
	stw r8+%lo(fio_col), r12
	blt lr, r11, .LBB26_26
.LBB26_25:
	stw r7+%lo(fio_len), r12
.LBB26_26:
	addi lr, r0, 1
	blt r4, lr, .LBB26_45
.LBB26_27:
	ldw r12, r7+%lo(fio_len)
	ldw r13, r8+%lo(fio_col)
	addi lr, r0, 32
	add r11, r0, r0
	jal r0, .LBB26_30
.LBB26_28:
	add r13, r15, r0
.LBB26_29:
	addi r11, r11, 1
	beq r11, r4, .LBB26_45
.LBB26_30:
	add r14, r3, r11
	ldb r15, r14+0
	bne r1, r15, .LBB26_44
.LBB26_31:
	bgt r13, r10, .LBB26_37
.LBB26_32:
	bge r12, r13, .LBB26_35
.LBB26_33:
	addi r15, r12, 1
	add r12, r12, r9
	stb r12+0, lr
	add r12, r15, r0
	bne r13, r15, .LBB26_33
.LBB26_34:
	stw r7+%lo(fio_len), r13
	add r12, r13, r0
.LBB26_35:
	add r15, r13, r9
	stb r15+0, r1
	addi r15, r13, 1
	stw r8+%lo(fio_col), r15
	blt r13, r12, .LBB26_38
.LBB26_36:
	stw r7+%lo(fio_len), r15
	add r12, r15, r0
	bgt r15, r10, .LBB26_28
	jal r0, .LBB26_39
.LBB26_37:
	add r15, r13, r0
.LBB26_38:
	bgt r15, r10, .LBB26_28
.LBB26_39:
	ldbu r13, r14+0
	bge r12, r15, .LBB26_42
.LBB26_40:
	addi r14, r12, 1
	add r12, r12, r9
	stb r12+0, lr
	add r12, r14, r0
	bne r15, r14, .LBB26_40
.LBB26_41:
	stw r7+%lo(fio_len), r15
	add r12, r15, r0
.LBB26_42:
	add r14, r15, r9
	stb r14+0, r13
	addi r13, r15, 1
	stw r8+%lo(fio_col), r13
	blt r15, r12, .LBB26_29
.LBB26_43:
	stw r7+%lo(fio_len), r13
	add r12, r13, r0
	jal r0, .LBB26_29
.LBB26_44:
	add r15, r13, r0
	bgt r15, r10, .LBB26_28
	jal r0, .LBB26_39
.LBB26_45:
	ldw r3, r8+%lo(fio_col)
	bgt r3, r10, .LBB26_71
.LBB26_46:
	ldw r4, r7+%lo(fio_len)
	bge r4, r3, .LBB26_50
.LBB26_47:
	addi r10, r0, 32
.LBB26_48:
	addi lr, r4, 1
	add r4, r4, r9
	stb r4+0, r10
	add r4, lr, r0
	bne r3, lr, .LBB26_48
.LBB26_49:
	stw r7+%lo(fio_len), r3
	add r4, r3, r0
.LBB26_50:
	add r9, r3, r9
	stb r9+0, r1
	addi r9, r3, 1
	stw r8+%lo(fio_col), r9
	blt r3, r4, .LBB26_71
.LBB26_51:
	stw r7+%lo(fio_len), r9
	jal r0, .LBB26_71
.LBB26_52:
	ldbu r7, r5+%lo(fio_first_item)
	bne r7, r0, .LBB26_54
.LBB26_53:
	ldbu r7, r6+%lo(fio_prev_char)
	andi r7, r7, 1
	bne r7, r0, .LBB26_61
.LBB26_54:
	lui r8, %hi(fio_col)
	ldw r7, r8+%lo(fio_col)
	addi r9, r0, 1022
	bgt r7, r9, .LBB26_61
.LBB26_55:
	lui r9, %hi(fio_len)
	ldw r10, r9+%lo(fio_len)
	bge r10, r7, .LBB26_59
.LBB26_56:
	lui lr, %hi(fio_line)
	addi lr, lr, %lo(fio_line)
	addi r11, r0, 32
.LBB26_57:
	addi r12, r10, 1
	add r10, r10, lr
	stb r10+0, r11
	add r10, r12, r0
	bne r7, r12, .LBB26_57
.LBB26_58:
	stw r9+%lo(fio_len), r7
	add r10, r7, r0
.LBB26_59:
	lui lr, %hi(fio_line)
	addi lr, lr, %lo(fio_line)
	add lr, r7, lr
	addi r11, r0, 32
	stb lr+0, r11
	addi lr, r7, 1
	stw r8+%lo(fio_col), lr
	blt r7, r10, .LBB26_61
.LBB26_60:
	stw r9+%lo(fio_len), lr
.LBB26_61:
	addi r7, r0, 1
	blt r4, r7, .LBB26_71
.LBB26_62:
	lui r7, %hi(fio_len)
	ldw lr, r7+%lo(fio_len)
	lui r8, %hi(fio_col)
	ldw r13, r8+%lo(fio_col)
	addi r9, r0, 1022
	lui r10, %hi(fio_line)
	addi r10, r10, %lo(fio_line)
	addi r11, r0, 32
	add r12, r0, r0
	jal r0, .LBB26_65
.LBB26_63:
	add r13, r14, r0
.LBB26_64:
	addi r12, r12, 1
	beq r12, r4, .LBB26_71
.LBB26_65:
	bgt r13, r9, .LBB26_64
.LBB26_66:
	add r14, r3, r12
	ldbu r14, r14+0
	bge lr, r13, .LBB26_69
.LBB26_67:
	addi r15, lr, 1
	add lr, lr, r10
	stb lr+0, r11
	add lr, r15, r0
	bne r13, r15, .LBB26_67
.LBB26_68:
	stw r7+%lo(fio_len), r13
	add lr, r13, r0
.LBB26_69:
	add r15, r13, r10
	stb r15+0, r14
	addi r14, r13, 1
	stw r8+%lo(fio_col), r14
	blt r13, lr, .LBB26_63
.LBB26_70:
	stw r7+%lo(fio_len), r14
	add lr, r14, r0
	jal r0, .LBB26_63
.LBB26_71:
	addi r3, r0, 0
	seq r1, r1, r3
	stb r5+%lo(fio_first_item), r0
	stb r6+%lo(fio_prev_char), r1
.LBB26_72:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB26_73:
	ble r5, r4, .LBB26_76
.LBB26_74:
	sub r5, r5, r4
	blt r5, r1, .LBB26_76
.LBB26_75:
	lui r6, %hi(fio_col)
	ldw r7, r6+%lo(fio_col)
	add r5, r7, r5
	stw r6+%lo(fio_col), r5
.LBB26_76:
	blt r4, r1, .LBB26_86
.LBB26_77:
	lui r5, %hi(fio_len)
	ldw r9, r5+%lo(fio_len)
	lui r6, %hi(fio_col)
	ldw r13, r6+%lo(fio_col)
	addi r7, r0, 1022
	lui r8, %hi(fio_line)
	addi r8, r8, %lo(fio_line)
	addi r10, r0, 32
	add lr, r0, r0
	jal r0, .LBB26_80
.LBB26_78:
	add r13, r14, r0
.LBB26_79:
	addi lr, lr, 1
	beq lr, r4, .LBB26_86
.LBB26_80:
	bgt r13, r7, .LBB26_79
.LBB26_81:
	add r14, r3, lr
	ldbu r14, r14+0
	bge r9, r13, .LBB26_84
.LBB26_82:
	addi r15, r9, 1
	add r9, r9, r8
	stb r9+0, r10
	add r9, r15, r0
	bne r13, r15, .LBB26_82
.LBB26_83:
	stw r5+%lo(fio_len), r13
	add r9, r13, r0
.LBB26_84:
	add r15, r13, r8
	stb r15+0, r14
	addi r14, r13, 1
	stw r6+%lo(fio_col), r14
	blt r13, r9, .LBB26_78
.LBB26_85:
	stw r5+%lo(fio_len), r14
	add r9, r14, r0
	jal r0, .LBB26_78
.LBB26_86:
	ldw r3, r12+%lo(fio_rep)
	blt r3, r1, .LBB26_88
.LBB26_87:
	addi r1, r3, -1
	stw r12+%lo(fio_rep), r1
.LBB26_88:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB26_72
.LBB26_89:
	stw r11+%lo(fio_desc), r0
	jal r0, .LBB26_72
.Lfunc_end26:
	.size	f77_wr_a, .Lfunc_end26-f77_wr_a
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
.Lfunc_end27:
	.size	f77_cabs, .Lfunc_end27-f77_cabs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cdiv
	.type	.LCPI28_0,@object
.LCPI28_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI28_0, 8
	.type	.LCPI28_1,@object
.LCPI28_1:
	.quad	0x43f0000000000000              # double 1.8446744073709552E+19
	.size	.LCPI28_1, 8
	.type	.LCPI28_2,@object
.LCPI28_2:
	.quad	0x3bf0000000000000              # double 5.4210108624275222E-20
	.size	.LCPI28_2, 8
	.type	.LCPI28_3,@object
.LCPI28_3:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI28_3, 8
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
	lui r1, %hi(.LCPI28_0)
	addi r1, r1, %lo(.LCPI28_0)
	ldw r5, r1+4
	ldw r4, r1+0
	lui r1, %hi(.LCPI28_1)
	addi r1, r1, %lo(.LCPI28_1)
	ldw r9, r1+4
	ldw r8, r1+0
	flt.d r1, r8, r6
	xori r3, r1, 1
	lui r1, %hi(.LCPI28_2)
	addi r1, r1, %lo(.LCPI28_2)
	bne r3, r0, .LBB28_2
.LBB28_1:
	ldw r19, r1+4
	ldw r18, r1+0
	fmul.d r14, r14, r18
	fmul.d r16, r16, r18
	fmul.d r6, r6, r18
	fmul.d r4, r4, r18
	flt.d r3, r8, r6
	bne r3, r0, .LBB28_1
.LBB28_2:
	lui r3, %hi(.LCPI28_3)
	addi r3, r3, %lo(.LCPI28_3)
	ldw r19, r3+4
	ldw r18, r3+0
	flt.d r3, r18, r6
	xori r3, r3, 1
	bne r3, r0, .LBB28_6
.LBB28_3:
	ldw r21, r1+4
	ldw r20, r1+0
	flt.d r1, r6, r20
	xori r1, r1, 1
	bne r1, r0, .LBB28_6
.LBB28_4:
	fmul.d r14, r14, r8
	fmul.d r16, r16, r8
	fmul.d r6, r6, r8
	fmul.d r4, r4, r8
	fle.d r1, r6, r18
	bne r1, r0, .LBB28_6
.LBB28_5:
	flt.d r1, r6, r20
	bne r1, r0, .LBB28_4
.LBB28_6:
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
.Lfunc_end28:
	.size	f77_cdiv, .Lfunc_end28-f77_cdiv
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpow_i
	.type	.LCPI29_0,@object
.LCPI29_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI29_0, 8
	.type	.LCPI29_1,@object
.LCPI29_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI29_1, 8
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
	lui r1, %hi(.LCPI29_0)
	addi r1, r1, %lo(.LCPI29_0)
	ldw r11, r1+4
	ldw r10, r1+0
	lui r1, %hi(.LCPI29_1)
	addi r1, r1, %lo(.LCPI29_1)
	ldw r9, r1+4
	ldw r8, r1+0
	beq r6, r0, .LBB29_7
.LBB29_1:
	srai r1, r6, 31
	xor r7, r6, r1
	sub r1, r7, r1
	fcvt.d.s r12, r5
	fcvt.d.s r4, r4
	jal r0, .LBB29_3
.LBB29_2:
	beq r1, r0, .LBB29_7
.LBB29_3:
	andi r7, r1, 1
	beq r7, r0, .LBB29_5
.LBB29_4:
	fmul.d r14, r4, r10
	fmul.d r16, r12, r8
	fsub.d r14, r14, r16
	fmul.d r10, r12, r10
	fmul.d r8, r4, r8
	fadd.d r8, r8, r10
	add r10, r14, r0
	add r11, r15, r0
.LBB29_5:
	srli r1, r1, 1
	beq r1, r0, .LBB29_2
.LBB29_6:
	fmul.d r14, r4, r4
	fmul.d r16, r12, r12
	fsub.d r14, r14, r16
	fadd.d r4, r4, r4
	fmul.d r12, r12, r4
	add r4, r14, r0
	add r5, r15, r0
	jal r0, .LBB29_2
.LBB29_7:
	addi r1, r0, -1
	bgt r6, r1, .LBB29_9
.LBB29_8:
	fmul.d r4, r10, r10
	fmul.d r6, r8, r8
	fadd.d r4, r6, r4
	fdiv.d r10, r10, r4
	fneg.d r6, r8
	fdiv.d r8, r6, r4
.LBB29_9:
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
.Lfunc_end29:
	.size	f77_cpow_i, .Lfunc_end29-f77_cpow_i
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
.Lfunc_end30:
	.size	f77_clog, .Lfunc_end30-f77_clog
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
.Lfunc_end31:
	.size	f77_cexp, .Lfunc_end31-f77_cexp
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_cpow_c
	.type	.LCPI32_0,@object
.LCPI32_0:
	.word	0x00000000                      # float 0
	.size	.LCPI32_0, 4
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
	lui r1, %hi(.LCPI32_0)
	ldw r1, r1+%lo(.LCPI32_0)
	feq.s r3, r4, r1
	xori r3, r3, 1
	bne r3, r0, .LBB32_3
.LBB32_1:
	feq.s r3, r5, r1
	xori r3, r3, 1
	bne r3, r0, .LBB32_3
.LBB32_2:
	stw r11+0, r0
	jal r0, .LBB32_4
.LBB32_3:
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
.LBB32_4:
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
.Lfunc_end32:
	.size	f77_cpow_c, .Lfunc_end32-f77_cpow_c
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_csqrt
	.type	.LCPI33_0,@object
.LCPI33_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI33_0, 8
	.type	.LCPI33_2,@object
.LCPI33_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI33_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI33_1,@object
.LCPI33_1:
	.word	0x00000000                      # float 0
	.size	.LCPI33_1, 4
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
	lui r1, %hi(.LCPI33_0)
	addi r1, r1, %lo(.LCPI33_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r16, r1, r0
	add r17, r2, r0
	lui r3, %hi(.LCPI33_1)
	ldw r1, r3+%lo(.LCPI33_1)
	fle.s r4, r1, r13
	xori r4, r4, 1
	beq r4, r0, .LBB33_4
.LBB33_1:
	ldw r13, r3+%lo(.LCPI33_1)
	lui r1, %hi(.LCPI33_2)
	addi r1, r1, %lo(.LCPI33_2)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r16, r4
	add r1, r13, r0
	bne r3, r0, .LBB33_3
.LBB33_2:
	add r3, r14, r0
	add r4, r15, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r16, r16
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
.LBB33_3:
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
	jal r0, .LBB33_6
.LBB33_4:
	fcvt.s.d r3, r16
	stw r11+0, r3
	lui r3, %hi(.LCPI33_2)
	addi r3, r3, %lo(.LCPI33_2)
	ldw r5, r3+4
	ldw r4, r3+0
	feq.d r3, r16, r4
	bne r3, r0, .LBB33_6
.LBB33_5:
	fadd.d r4, r16, r16
	fdiv.d r4, r14, r4
	fcvt.s.d r1, r4
.LBB33_6:
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
.Lfunc_end33:
	.size	f77_csqrt, .Lfunc_end33-f77_csqrt
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
.Lfunc_end34:
	.size	f77_csin, .Lfunc_end34-f77_csin
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
.Lfunc_end35:
	.size	f77_ccos, .Lfunc_end35-f77_ccos
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
	beq r1, r0, .LBB36_2
.LBB36_1:
	fdiv.d r6, r16, r18
	fmul.d r4, r16, r6
	fadd.d r8, r18, r4
	fmul.d r4, r12, r6
	fadd.d r4, r14, r4
	fdiv.d r4, r4, r8
	fmul.d r6, r14, r6
	fsub.d r6, r6, r12
	jal r0, .LBB36_3
.LBB36_2:
	fdiv.d r6, r18, r16
	fmul.d r4, r18, r6
	fadd.d r8, r16, r4
	fmul.d r4, r14, r6
	fadd.d r4, r12, r4
	fdiv.d r4, r4, r8
	fmul.d r6, r12, r6
	fsub.d r6, r14, r6
.LBB36_3:
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
.Lfunc_end36:
	.size	f77_zdiv, .Lfunc_end36-f77_zdiv
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zpow_i
	.type	.LCPI37_0,@object
.LCPI37_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI37_0, 8
	.type	.LCPI37_1,@object
.LCPI37_1:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI37_1, 8
	.type	.LCPI37_2,@object
.LCPI37_2:
	.quad	0xbff0000000000000              # double -1
	.size	.LCPI37_2, 8
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
	lui r1, %hi(.LCPI37_0)
	addi r1, r1, %lo(.LCPI37_0)
	ldw r13, r1+4
	ldw r12, r1+0
	lui r1, %hi(.LCPI37_1)
	addi r1, r1, %lo(.LCPI37_1)
	ldw r15, r1+4
	ldw r14, r1+0
	beq r5, r0, .LBB37_11
.LBB37_1:
	ldw r17, r4+4
	ldw r16, r4+0
	ldw r19, r4+12
	ldw r18, r4+8
	addi r1, r0, -1
	bgt r5, r1, .LBB37_6
.LBB37_2:
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
	beq r1, r0, .LBB37_4
.LBB37_3:
	fdiv.d r4, r16, r18
	fmul.d r6, r16, r4
	fadd.d r6, r18, r6
	fadd.d r8, r4, r12
	fdiv.d r16, r8, r6
	fmul.d r4, r4, r12
	lui r1, %hi(.LCPI37_2)
	addi r1, r1, %lo(.LCPI37_2)
	ldw r9, r1+4
	ldw r8, r1+0
	fadd.d r4, r4, r8
	jal r0, .LBB37_5
.LBB37_4:
	fdiv.d r4, r18, r16
	fmul.d r6, r18, r4
	fadd.d r6, r16, r6
	fmul.d r8, r4, r12
	fadd.d r8, r8, r14
	fdiv.d r16, r8, r6
	fsub.d r4, r12, r4
.LBB37_5:
	fdiv.d r18, r4, r6
	add r3, r11, r0
	add r5, r20, r0
.LBB37_6:
	andi r1, r5, 1
	bne r1, r0, .LBB37_8
.LBB37_7:
	add r6, r14, r0
	add r7, r15, r0
	srli r5, r5, 1
	bne r5, r0, .LBB37_9
	jal r0, .LBB37_10
.LBB37_8:
	fmul.d r6, r14, r16
	fmul.d r8, r12, r18
	fsub.d r6, r6, r8
	fmul.d r8, r14, r18
	fmul.d r10, r12, r16
	fadd.d r12, r10, r8
	srli r5, r5, 1
	beq r5, r0, .LBB37_10
.LBB37_9:
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
	beq r1, r0, .LBB37_7
	jal r0, .LBB37_8
.LBB37_10:
	add r14, r6, r0
	add r15, r7, r0
.LBB37_11:
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
.Lfunc_end37:
	.size	f77_zpow_i, .Lfunc_end37-f77_zpow_i
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zpow_z
	.type	.LCPI38_0,@object
.LCPI38_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI38_0, 8
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
	lui r1, %hi(.LCPI38_0)
	addi r1, r1, %lo(.LCPI38_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r8, r6
	xori r1, r1, 1
	bne r1, r0, .LBB38_3
.LBB38_1:
	ldw r15, r4+12
	ldw r14, r4+8
	feq.d r1, r14, r6
	xori r1, r1, 1
	bne r1, r0, .LBB38_3
.LBB38_2:
	stw r11+12, r0
	stw r11+8, r0
	stw r11+4, r0
	stw r11+0, r0
	jal r0, .LBB38_4
.LBB38_3:
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
.LBB38_4:
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
	.size	f77_zpow_z, .Lfunc_end38-f77_zpow_z
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
.Lfunc_end39:
	.size	f77_zabs, .Lfunc_end39-f77_zabs
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
	beq r3, r0, .LBB40_5
.LBB40_1:
	beq r11, r0, .LBB40_9
.LBB40_2:
	add r1, r3, r0
	lui r3, 1
	addi r3, r3, 918
.LBB40_3:
	stw r11+0, r3
.LBB40_4:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB40_5:
	addi r1, r0, 1
	sgt r1, r4, r1
	sub r1, r0, r1
	add r12, r4, r0
	xori r3, r4, 1
	and r1, r3, r1
	xori r3, r1, 1
	jal r31, malloc
	beq r1, r0, .LBB40_7
.LBB40_6:
	add r3, r0, r0
	bne r11, r0, .LBB40_3
	jal r0, .LBB40_4
.LBB40_7:
	beq r11, r0, .LBB40_11
.LBB40_8:
	lui r1, 1
	addi r3, r1, 918
	add r1, r0, r0
	jal r0, .LBB40_3
.LBB40_9:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
.LBB40_10:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB40_11:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r5, r12, r0
	jal r0, .LBB40_10
.Lfunc_end40:
	.size	f77_allocate, .Lfunc_end40-f77_allocate
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
	beq r3, r0, .LBB41_2
.LBB41_1:
	jal r31, free
	add r1, r0, r0
	bne r11, r0, .LBB41_4
	jal r0, .LBB41_5
.LBB41_2:
	beq r11, r0, .LBB41_6
.LBB41_3:
	addi r1, r0, 1
.LBB41_4:
	stw r11+0, r1
.LBB41_5:
	add r1, r0, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB41_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end41:
	.size	f77_deallocate, .Lfunc_end41-f77_deallocate
                                        # -- End function
	.globl	f77_dealloc_auto                # -- Begin function f77_dealloc_auto
	.p2align	2
	.type	f77_dealloc_auto,@function
f77_dealloc_auto:                       # @f77_dealloc_auto
# %bb.0:
	beq r3, r0, .LBB42_2
.LBB42_1:
	jal r0, free
.LBB42_2:
	jalr r0, r31, 0
.Lfunc_end42:
	.size	f77_dealloc_auto, .Lfunc_end42-f77_dealloc_auto
                                        # -- End function
	.globl	f77_dfl_fill                    # -- Begin function f77_dfl_fill
	.p2align	2
	.type	f77_dfl_fill,@function
f77_dfl_fill:                           # @f77_dfl_fill
# %bb.0:
	beq r3, r0, .LBB43_8
.LBB43_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r11, r6, r0
	addi r1, r0, 1
	blt r6, r1, .LBB43_7
.LBB43_2:
	add r12, r5, r0
	add r13, r4, r0
	add r14, r3, r0
	jal r0, .LBB43_5
.LBB43_3:
	add r4, r0, r0
	add r5, r12, r0
	jal r31, memset
.LBB43_4:
	addi r11, r11, -1
	add r14, r14, r12
	beq r11, r0, .LBB43_7
.LBB43_5:
	add r3, r14, r0
	beq r13, r0, .LBB43_3
.LBB43_6:
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcpy
	jal r0, .LBB43_4
.LBB43_7:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB43_8:
	jalr r0, r31, 0
.Lfunc_end43:
	.size	f77_dfl_fill, .Lfunc_end43-f77_dfl_fill
                                        # -- End function
	.globl	f77_dta_free                    # -- Begin function f77_dta_free
	.p2align	2
	.type	f77_dta_free,@function
f77_dta_free:                           # @f77_dta_free
# %bb.0:
	addi r1, r0, 1
	blt r4, r1, .LBB44_6
.LBB44_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	add r11, r5, r0
	add r12, r4, r0
	add r13, r3, r0
	jal r0, .LBB44_3
.LBB44_2:
	addi r12, r12, -1
	add r13, r13, r11
	beq r12, r0, .LBB44_5
.LBB44_3:
	ldw r3, r13+0
	beq r3, r0, .LBB44_2
.LBB44_4:
	jal r31, free
	stw r13+0, r0
	jal r0, .LBB44_2
.LBB44_5:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB44_6:
	jalr r0, r31, 0
.Lfunc_end44:
	.size	f77_dta_free, .Lfunc_end44-f77_dta_free
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
	beq r1, r0, .LBB45_20
.LBB45_1:
	add r13, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	addi r1, r0, 1
	add r15, r1, r0
	blt r4, r1, .LBB45_4
.LBB45_2:
	addi r3, r12, 4
	addi r15, r0, 1
	addi r4, r0, 0
	add r5, r11, r0
.LBB45_3:
	ldw r6, r3+0
	sgt r7, r6, r4
	mul r6, r6, r15
	sub r7, r0, r7
	and r15, r6, r7
	addi r5, r5, -1
	addi r3, r3, 8
	bne r5, r0, .LBB45_3
.LBB45_4:
	mul r3, r15, r13
	xori r4, r3, 1
	sgt r1, r3, r1
	sub r1, r0, r1
	and r1, r4, r1
	xori r3, r1, 1
	jal r31, malloc
	beq r1, r0, .LBB45_21
.LBB45_5:
	add r14, r1, r0
	addi r3, sp, 28
	addi r5, r0, 28
	add r4, r0, r0
	jal r31, memset
	addi r16, r0, 1
	blt r15, r16, .LBB45_17
.LBB45_6:
	addi r17, r12, 8
	addi r18, r12, 4
	addi r19, r0, 1
	add r20, r0, r0
	jal r0, .LBB45_8
.LBB45_7:
	addi r20, r20, 1
	beq r20, r15, .LBB45_17
.LBB45_8:
	blt r11, r19, .LBB45_12
.LBB45_9:
	addi r1, sp, 28
	add r3, r17, r0
	add r5, r11, r0
	add r4, r0, r0
.LBB45_10:
	ldw r6, r1+0
	ldw r7, r3+0
	mul r6, r7, r6
	add r4, r6, r4
	addi r5, r5, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r5, r0, .LBB45_10
.LBB45_11:
	mul r1, r4, r13
	jal r0, .LBB45_13
.LBB45_12:
	add r1, r0, r0
.LBB45_13:
	mul r3, r20, r13
	add r3, r14, r3
	ldw r4, r12+0
	add r4, r4, r1
	add r5, r13, r0
	jal r31, memcpy
	blt r11, r19, .LBB45_7
.LBB45_14:
	addi r1, sp, 28
	add r3, r18, r0
	add r4, r11, r0
.LBB45_15:
	ldw r5, r1+0
	addi r5, r5, 1
	stw r1+0, r5
	ldw r6, r3+0
	blt r5, r6, .LBB45_7
.LBB45_16:
	stw r1+0, r0
	addi r4, r4, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r4, r0, .LBB45_15
	jal r0, .LBB45_7
.LBB45_17:
	stw r12+0, r14
	blt r11, r16, .LBB45_20
.LBB45_18:
	addi r1, r12, 4
.LBB45_19:
	stw r1+4, r16
	ldw r3, r1+0
	mul r16, r3, r16
	addi r11, r11, -1
	addi r1, r1, 8
	bne r11, r0, .LBB45_19
.LBB45_20:
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
.LBB45_21:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.21)
	addi r4, r4, %lo(.L.str.21)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end45:
	.size	f77_dta_dup, .Lfunc_end45-f77_dta_dup
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
	blt r4, r1, .LBB46_9
.LBB46_1:
	addi r6, r3, 4
	addi r7, r0, 2
	add r8, r4, r0
	jal r0, .LBB46_3
.LBB46_2:
	mul r1, r9, r1
	addi r8, r8, -1
	addi r6, r6, 8
	beq r8, r0, .LBB46_9
.LBB46_3:
	ldw r9, r6+0
	blt r9, r7, .LBB46_2
.LBB46_4:
	ldw r10, r6+4
	beq r10, r1, .LBB46_2
.LBB46_5:
	add r11, r3, r0
	addi r3, r3, 4
	addi r1, r0, 1
	add r12, r4, r0
.LBB46_6:
	ldw r6, r3+0
	mul r1, r6, r1
	addi r4, r4, -1
	addi r3, r3, 8
	bne r4, r0, .LBB46_6
.LBB46_7:
	add r14, r5, r0
	mul r13, r1, r5
	xori r1, r13, 1
	addi r3, r0, 1
	sgt r3, r13, r3
	sub r3, r0, r3
	and r1, r1, r3
	xori r3, r1, 1
	jal r31, malloc
	beq r1, r0, .LBB46_11
.LBB46_8:
	addi r7, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r14, r0
	add r6, r1, r0
	add r11, r1, r0
	jal r31, f77_as_walk
	add r1, r11, r0
	jal r0, .LBB46_10
.LBB46_9:
	ldw r1, r3+0
.LBB46_10:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB46_11:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	add r5, r13, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end46:
	.size	f77_as_pack, .Lfunc_end46-f77_as_pack
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
	blt r14, r1, .LBB47_3
.LBB47_1:
	addi r5, r3, 4
	addi r15, r0, 1
	add r6, r14, r0
.LBB47_2:
	stw r4+0, r0
	ldw r7, r5+0
	mul r15, r7, r15
	addi r6, r6, -1
	addi r5, r5, 8
	addi r4, r4, 4
	bne r6, r0, .LBB47_2
.LBB47_3:
	blt r15, r1, .LBB47_18
.LBB47_4:
	ldw r16, r3+0
	addi r17, r3, 4
	addi r18, r3, 8
	addi r19, r0, 1
	add r20, r0, r0
	jal r0, .LBB47_6
.LBB47_5:
	addi r20, r20, 1
	beq r20, r15, .LBB47_18
.LBB47_6:
	addi r1, sp, 24
	blt r14, r19, .LBB47_10
.LBB47_7:
	add r3, r18, r0
	add r5, r14, r0
	add r4, r0, r0
.LBB47_8:
	ldw r6, r1+0
	ldw r7, r3+0
	mul r6, r7, r6
	add r4, r6, r4
	addi r5, r5, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r5, r0, .LBB47_8
.LBB47_9:
	mul r1, r4, r13
	jal r0, .LBB47_11
.LBB47_10:
	add r1, r0, r0
.LBB47_11:
	add r4, r16, r1
	mul r1, r20, r13
	beq r11, r0, .LBB47_13
.LBB47_12:
	add r3, r12, r1
	jal r0, .LBB47_14
.LBB47_13:
	add r1, r12, r1
	add r3, r4, r0
	add r4, r1, r0
.LBB47_14:
	add r5, r13, r0
	jal r31, memcpy
	blt r14, r19, .LBB47_5
.LBB47_15:
	addi r1, sp, 24
	add r3, r17, r0
	add r4, r14, r0
.LBB47_16:
	ldw r5, r1+0
	addi r5, r5, 1
	stw r1+0, r5
	ldw r6, r3+0
	blt r5, r6, .LBB47_5
.LBB47_17:
	stw r1+0, r0
	addi r4, r4, -1
	addi r3, r3, 8
	addi r1, r1, 4
	bne r4, r0, .LBB47_16
	jal r0, .LBB47_5
.LBB47_18:
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
.Lfunc_end47:
	.size	f77_as_walk, .Lfunc_end47-f77_as_walk
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
	beq r1, r0, .LBB48_3
.LBB48_1:
	add r12, r1, r0
	addi r3, r0, 256
	jal r31, malloc
	stw r12+0, r1
	beq r1, r0, .LBB48_3
.LBB48_2:
	stw r12+4, r0
	stw r12+8, r11
	stw r12+12, r11
	add r1, r12, r0
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB48_3:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.118)
	addi r4, r4, %lo(.L.str.118)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end48:
	.size	f77_vec_new, .Lfunc_end48-f77_vec_new
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
	bne r1, r0, .LBB49_4
.LBB49_1:
	ldw r1, r11+12
	beq r5, r1, .LBB49_4
.LBB49_2:
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
	beq r1, r0, .LBB49_8
.LBB49_3:
	stw r11+0, r1
	stw r11+12, r13
	add r4, r12, r0
.LBB49_4:
	ldw r3, r11+4
	ldw r1, r11+8
	bne r3, r1, .LBB49_7
.LBB49_5:
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
	beq r1, r0, .LBB49_8
.LBB49_6:
	stw r11+0, r1
	ldw r1, r11+8
	slli r1, r1, 1
	stw r11+8, r1
	add r4, r12, r0
.LBB49_7:
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
.LBB49_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.118)
	addi r4, r4, %lo(.L.str.118)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end49:
	.size	f77_vec_pushn, .Lfunc_end49-f77_vec_pushn
                                        # -- End function
	.globl	f77_vec_push                    # -- Begin function f77_vec_push
	.p2align	2
	.type	f77_vec_push,@function
f77_vec_push:                           # @f77_vec_push
# %bb.0:
	addi r5, r0, 16
	jal r0, f77_vec_pushn
.Lfunc_end50:
	.size	f77_vec_push, .Lfunc_end50-f77_vec_push
                                        # -- End function
	.globl	f77_vec_count                   # -- Begin function f77_vec_count
	.p2align	2
	.type	f77_vec_count,@function
f77_vec_count:                          # @f77_vec_count
# %bb.0:
	ldw r1, r3+4
	jalr r0, r31, 0
.Lfunc_end51:
	.size	f77_vec_count, .Lfunc_end51-f77_vec_count
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
.Lfunc_end52:
	.size	f77_vec_take, .Lfunc_end52-f77_vec_take
                                        # -- End function
	.globl	f77_as_unpack                   # -- Begin function f77_as_unpack
	.p2align	2
	.type	f77_as_unpack,@function
f77_as_unpack:                          # @f77_as_unpack
# %bb.0:
	ldw r1, r3+0
	beq r6, r1, .LBB53_2
.LBB53_1:
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
.LBB53_2:
	jalr r0, r31, 0
.Lfunc_end53:
	.size	f77_as_unpack, .Lfunc_end53-f77_as_unpack
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpu_time_r
	.type	.LCPI54_0,@object
.LCPI54_0:
	.quad	0x412e848000000000              # double 1.0E+6
	.size	.LCPI54_0, 8
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
	lui r1, %hi(.LCPI54_0)
	addi r1, r1, %lo(.LCPI54_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
	stw r11+0, r1
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end54:
	.size	f77_cpu_time_r, .Lfunc_end54-f77_cpu_time_r
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_cpu_time_d
	.type	.LCPI55_0,@object
.LCPI55_0:
	.quad	0x412e848000000000              # double 1.0E+6
	.size	.LCPI55_0, 8
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
	lui r1, %hi(.LCPI55_0)
	addi r1, r1, %lo(.LCPI55_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fdiv.d r4, r4, r6
	stw r11+4, r5
	stw r11+0, r4
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end55:
	.size	f77_cpu_time_d, .Lfunc_end55-f77_cpu_time_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_fraction
	.type	.LCPI56_0,@object
.LCPI56_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI56_0, 8
	.text
	.globl	f77_fraction
	.p2align	2
	.type	f77_fraction,@function
f77_fraction:                           # @f77_fraction
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI56_0)
	addi r1, r1, %lo(.LCPI56_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r6, r4
	bne r1, r0, .LBB56_2
.LBB56_1:
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
.LBB56_2:
	add r1, r4, r0
	add r2, r5, r0
	jalr r0, r31, 0
.Lfunc_end56:
	.size	f77_fraction, .Lfunc_end56-f77_fraction
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_set_exponent
	.type	.LCPI57_0,@object
.LCPI57_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI57_0, 8
	.text
	.globl	f77_set_exponent
	.p2align	2
	.type	f77_set_exponent,@function
f77_set_exponent:                       # @f77_set_exponent
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI57_0)
	addi r1, r1, %lo(.LCPI57_0)
	ldw r9, r1+4
	ldw r8, r1+0
	feq.d r1, r6, r8
	bne r1, r0, .LBB57_2
.LBB57_1:
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
.LBB57_2:
	add r1, r8, r0
	add r2, r9, r0
	jalr r0, r31, 0
.Lfunc_end57:
	.size	f77_set_exponent, .Lfunc_end57-f77_set_exponent
                                        # -- End function
	.globl	f77_scale                       # -- Begin function f77_scale
	.p2align	2
	.type	f77_scale,@function
f77_scale:                              # @f77_scale
# %bb.0:
	add r9, r4, r0
	add r8, r3, r0
	beq r6, r0, .LBB58_2
.LBB58_1:
	add r3, r8, r0
	add r4, r9, r0
	jal r0, ldexp
.LBB58_2:
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
.Lfunc_end58:
	.size	f77_scale, .Lfunc_end58-f77_scale
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_nearest
	.type	.LCPI59_0,@object
.LCPI59_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI59_0, 8
	.type	.LCPI59_2,@object
.LCPI59_2:
	.word	0xff800000                      # float -Inf
	.word	0x7f800000                      # float +Inf
	.size	.LCPI59_2, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI59_1,@object
.LCPI59_1:
	.quad	0xfff0000000000000              # double -Inf
	.quad	0x7ff0000000000000              # double +Inf
	.size	.LCPI59_1, 16
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
	lui r1, %hi(.LCPI59_0)
	addi r1, r1, %lo(.LCPI59_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r10, r4
	xori r1, r1, 1
	beq r1, r0, .LBB59_4
.LBB59_1:
	add r8, r3, r0
	beq r7, r0, .LBB59_3
.LBB59_2:
	flt.d r1, r4, r10
	sub r1, r0, r1
	andi r1, r1, 8
	lui r3, %hi(.LCPI59_1)
	addi r3, r3, %lo(.LCPI59_1)
	add r1, r3, r1
	ldw r6, r1+4
	ldw r5, r1+0
	add r3, r8, r0
	add r4, r9, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, nextafter
.LBB59_3:
	fcvt.s.d r3, r8
	flt.d r1, r4, r10
	sub r1, r0, r1
	andi r1, r1, 4
	lui r4, %hi(.LCPI59_2)
	addi r4, r4, %lo(.LCPI59_2)
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
.LBB59_4:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end59:
	.size	f77_nearest, .Lfunc_end59-f77_nearest
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_spacing
	.type	.LCPI60_0,@object
.LCPI60_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI60_0, 8
	.type	.LCPI60_1,@object
.LCPI60_1:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI60_1, 8
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	2, 0x0
	.type	.LCPI60_2,@object
.LCPI60_2:
	.quad	0x0010000000000000              # double 2.2250738585072014E-308
	.quad	0x3810000000000000              # double 1.1754943508222875E-38
	.size	.LCPI60_2, 16
	.text
	.globl	f77_spacing
	.p2align	2
	.type	f77_spacing,@function
f77_spacing:                            # @f77_spacing
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI60_0)
	addi r1, r1, %lo(.LCPI60_0)
	ldw r9, r1+4
	ldw r8, r1+0
	feq.d r1, r6, r8
	xori r1, r1, 1
	bne r1, r0, .LBB60_2
.LBB60_1:
	addi r1, r0, 0
	seq r1, r5, r1
	sub r1, r0, r1
	andi r1, r1, 8
	lui r3, %hi(.LCPI60_2)
	addi r3, r3, %lo(.LCPI60_2)
	add r1, r3, r1
	ldw r5, r1+4
	ldw r4, r1+0
	jal r0, .LBB60_6
.LBB60_2:
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
	bge r5, r1, .LBB60_4
.LBB60_3:
	andi r1, r11, 8
	lui r3, %hi(.LCPI60_2)
	addi r3, r3, %lo(.LCPI60_2)
	add r1, r3, r1
	ldw r5, r1+4
	ldw r4, r1+0
	jal r0, .LBB60_5
.LBB60_4:
	lui r1, %hi(.LCPI60_1)
	addi r1, r1, %lo(.LCPI60_1)
	ldw r4, r1+4
	ldw r3, r1+0
	jal r31, ldexp
	add r4, r1, r0
	add r5, r2, r0
.LBB60_5:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB60_6:
	add r1, r4, r0
	add r2, r5, r0
	jalr r0, r31, 0
.Lfunc_end60:
	.size	f77_spacing, .Lfunc_end60-f77_spacing
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_rrspacing
	.type	.LCPI61_0,@object
.LCPI61_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI61_0, 8
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
	lui r1, %hi(.LCPI61_0)
	addi r1, r1, %lo(.LCPI61_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r1, r6, r4
	bne r1, r0, .LBB61_2
.LBB61_1:
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
.LBB61_2:
	add r1, r4, r0
	add r2, r5, r0
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end61:
	.size	f77_rrspacing, .Lfunc_end61-f77_rrspacing
                                        # -- End function
	.globl	f77_modulo_i                    # -- Begin function f77_modulo_i
	.p2align	2
	.type	f77_modulo_i,@function
f77_modulo_i:                           # @f77_modulo_i
# %bb.0:
	beq r4, r0, .LBB62_2
.LBB62_1:
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
.LBB62_2:
	addi sp, sp, -16
	stw sp+0, lr
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.24)
	addi r4, r4, %lo(.L.str.24)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end62:
	.size	f77_modulo_i, .Lfunc_end62-f77_modulo_i
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_modulo_d
	.type	.LCPI63_0,@object
.LCPI63_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI63_0, 8
	.type	.LCPI63_2,@object
.LCPI63_2:
	.quad	0xb690000000000000              # double -7.0064923216240854E-46
	.size	.LCPI63_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI63_1,@object
.LCPI63_1:
	.word	0x00000000                      # float 0
	.size	.LCPI63_1, 4
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
	beq r7, r0, .LBB63_3
.LBB63_1:
	add r3, r4, r0
	add r4, r5, r0
	add r5, r12, r0
	add r6, r13, r0
	jal r31, fmod
	add r4, r1, r0
	add r5, r2, r0
	lui r1, %hi(.LCPI63_0)
	addi r1, r1, %lo(.LCPI63_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r4, r6
	bne r1, r0, .LBB63_6
.LBB63_2:
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
	jal r0, .LBB63_6
.LBB63_3:
	fcvt.s.d r3, r4
	fcvt.s.d r11, r12
	add r4, r11, r0
	jal r31, fmodf
	lui r3, %hi(.LCPI63_1)
	ldw r3, r3+%lo(.LCPI63_1)
	feq.s r4, r1, r3
	bne r4, r0, .LBB63_5
.LBB63_4:
	flt.s r3, r1, r3
	lui r4, %hi(.LCPI63_2)
	addi r4, r4, %lo(.LCPI63_2)
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
.LBB63_5:
	fcvt.d.s r4, r1
.LBB63_6:
	add r1, r4, r0
	add r2, r5, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end63:
	.size	f77_modulo_d, .Lfunc_end63-f77_modulo_d
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
	beq r18, r0, .LBB64_6
.LBB64_1:
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
	blt r17, r1, .LBB64_6
.LBB64_2:
	addi r1, r0, 32
	addi r3, r0, 7
	add r4, r0, r0
	jal r0, .LBB64_4
.LBB64_3:
	add r6, r18, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r17, r4, .LBB64_6
.LBB64_4:
	add r5, r1, r0
	bgtu r4, r3, .LBB64_3
.LBB64_5:
	addi r5, sp, 28
	add r5, r5, r4
	ldbu r5, r5+0
	jal r0, .LBB64_3
.LBB64_6:
	lui r17, 67109
	beq r16, r0, .LBB64_12
.LBB64_7:
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
	blt r15, r1, .LBB64_12
.LBB64_8:
	addi r1, r0, 32
	addi r3, r0, 9
	add r4, r0, r0
	jal r0, .LBB64_10
.LBB64_9:
	add r6, r16, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r15, r4, .LBB64_12
.LBB64_10:
	add r5, r1, r0
	bgtu r4, r3, .LBB64_9
.LBB64_11:
	addi r5, sp, 28
	add r5, r5, r4
	ldbu r5, r5+0
	jal r0, .LBB64_9
.LBB64_12:
	beq r13, r0, .LBB64_18
.LBB64_13:
	addi r1, r0, 1
	blt r12, r1, .LBB64_18
.LBB64_14:
	addi r1, r0, 32
	addi r3, r0, 4
	lui r4, %hi(.L.str.27)
	addi r4, r4, %lo(.L.str.27)
	add r5, r0, r0
	jal r0, .LBB64_16
.LBB64_15:
	add r7, r13, r5
	stb r7+0, r6
	addi r5, r5, 1
	beq r12, r5, .LBB64_18
.LBB64_16:
	add r6, r1, r0
	bgtu r5, r3, .LBB64_15
.LBB64_17:
	add r6, r5, r4
	ldbu r6, r6+0
	jal r0, .LBB64_15
.LBB64_18:
	beq r11, r0, .LBB64_20
.LBB64_19:
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
.LBB64_20:
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
.Lfunc_end64:
	.size	f77_date_and_time, .Lfunc_end64-f77_date_and_time
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
	beq r13, r0, .LBB65_2
.LBB65_1:
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
.LBB65_2:
	beq r12, r0, .LBB65_4
.LBB65_3:
	addi r1, r0, 1000
	stw r12+0, r1
.LBB65_4:
	beq r11, r0, .LBB65_6
.LBB65_5:
	addi r1, r14, -1
	stw r11+0, r1
.LBB65_6:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end65:
	.size	f77_system_clock, .Lfunc_end65-f77_system_clock
                                        # -- End function
	.globl	f77_random_seed                 # -- Begin function f77_random_seed
	.p2align	2
	.type	f77_random_seed,@function
f77_random_seed:                        # @f77_random_seed
# %bb.0:
	beq r3, r0, .LBB66_2
.LBB66_1:
	addi r1, r0, 4
	stw r3+0, r1
.LBB66_2:
	addi sp, sp, -48
	stw sp+0, lr
	stw sp+44, r11
	stw sp+40, r12
	stw sp+36, r13
	stw sp+32, r14
	lui r11, %hi(f77_rng)
	addi r11, r11, %lo(f77_rng)
	addi r1, r0, 16
	beq r5, r0, .LBB66_5
.LBB66_3:
	add r6, r0, r0
.LBB66_4:
	add r7, r6, r11
	ldw r7, r7+0
	add r8, r5, r6
	stw r8+0, r7
	addi r6, r6, 4
	bne r6, r1, .LBB66_4
.LBB66_5:
	lui r12, %hi(f77_rng+4)
	lui r13, %hi(f77_rng+8)
	lui r14, %hi(f77_rng+12)
	beq r4, r0, .LBB66_10
.LBB66_6:
	add r6, r0, r0
.LBB66_7:
	add r7, r4, r6
	ldw r7, r7+0
	add r8, r6, r11
	stw r8+0, r7
	addi r6, r6, 4
	bne r6, r1, .LBB66_7
.LBB66_8:
	ldw r1, r11+0
	ldw r6, r12+%lo(f77_rng+4)
	or  r1, r6, r1
	ldw r6, r13+%lo(f77_rng+8)
	or  r1, r1, r6
	ldw r6, r14+%lo(f77_rng+12)
	or  r1, r1, r6
	bne r1, r0, .LBB66_10
.LBB66_9:
	addi r1, r0, 1
	stw r11+0, r1
.LBB66_10:
	bne r3, r0, .LBB66_15
.LBB66_11:
	bne r4, r0, .LBB66_15
.LBB66_12:
	bne r5, r0, .LBB66_15
.LBB66_13:
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
	bne r1, r0, .LBB66_15
.LBB66_14:
	addi r1, r0, 1
	stw r11+0, r1
.LBB66_15:
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end66:
	.size	f77_random_seed, .Lfunc_end66-f77_random_seed
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
	beq r4, r0, .LBB67_2
.LBB67_1:
	ldw r1, r4+4
	addi r5, r0, 4
	blt r1, r5, .LBB67_12
.LBB67_2:
	beq r11, r0, .LBB67_4
.LBB67_3:
	ldw r1, r11+4
	addi r5, r0, 3
	ble r1, r5, .LBB67_12
.LBB67_4:
	beq r4, r0, .LBB67_7
.LBB67_5:
	ldw r1, r4+0
	ldw r4, r4+8
	slli r5, r4, 2
	addi r6, r0, 16
	add r7, r0, r0
.LBB67_6:
	ldw r8, r1+0
	addi r4, sp, 44
	add r9, r4, r7
	stw r9+0, r8
	addi r7, r7, 4
	add r1, r1, r5
	bne r7, r6, .LBB67_6
	jal r0, .LBB67_8
.LBB67_7:
	add r4, r0, r0
.LBB67_8:
	addi r1, r0, 0
	seq r1, r11, r1
	sub r1, r0, r1
	addi r5, sp, 28
	and r1, r5, r1
	xor r5, r5, r1
	jal r31, f77_random_seed
	beq r11, r0, .LBB67_11
.LBB67_9:
	addi r1, sp, 28
	addi r3, r0, 4
	add r4, r0, r0
.LBB67_10:
	ldw r5, r1+0
	ldw r6, r11+0
	ldw r7, r11+8
	mul r7, r7, r4
	slli r7, r7, 2
	add r6, r6, r7
	stw r6+0, r5
	addi r4, r4, 1
	addi r1, r1, 4
	bne r4, r3, .LBB67_10
.LBB67_11:
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.LBB67_12:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.28)
	addi r4, r4, %lo(.L.str.28)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end67:
	.size	f77_random_seed_d, .Lfunc_end67-f77_random_seed_d
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
	beq r9, r0, .LBB68_5
.LBB68_1:
	add r11, r9, r0
	ldw r1, r9+4
	addi r9, r0, 7
	ble r1, r9, .LBB68_6
.LBB68_2:
	addi r12, sp, 24
	add r9, r12, r0
	jal r31, f77_date_and_time
	addi r1, r0, 8
	add r3, r0, r0
.LBB68_3:
	ldw r4, r12+0
	ldw r5, r11+0
	ldw r6, r11+8
	mul r6, r6, r3
	slli r6, r6, 2
	add r5, r5, r6
	stw r5+0, r4
	addi r3, r3, 1
	addi r12, r12, 4
	bne r3, r1, .LBB68_3
.LBB68_4:
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.LBB68_5:
	add r9, r0, r0
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jal r0, f77_date_and_time
.LBB68_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.29)
	addi r4, r4, %lo(.L.str.29)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end68:
	.size	f77_date_and_time_d, .Lfunc_end68-f77_date_and_time_d
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_random_r
	.type	.LCPI69_0,@object
.LCPI69_0:
	.word	0x33800000                      # float 5.96046448E-8
	.size	.LCPI69_0, 4
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
	lui r4, %hi(.LCPI69_0)
	ldw r4, r4+%lo(.LCPI69_0)
	fmul.s r1, r1, r4
	stw r3+0, r1
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end69:
	.size	f77_random_r, .Lfunc_end69-f77_random_r
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_random_d
	.type	.LCPI70_0,@object
.LCPI70_0:
	.quad	0x4190000000000000              # double 67108864
	.size	.LCPI70_0, 8
	.type	.LCPI70_1,@object
.LCPI70_1:
	.quad	0x3ca0000000000000              # double 1.1102230246251565E-16
	.size	.LCPI70_1, 8
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
	lui r6, %hi(.LCPI70_0)
	addi r6, r6, %lo(.LCPI70_0)
	ldw r9, r6+4
	ldw r8, r6+0
	fmul.d r4, r4, r8
	fcvt.d.wu r6, r1
	fadd.d r4, r4, r6
	lui r1, %hi(.LCPI70_1)
	addi r1, r1, %lo(.LCPI70_1)
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
.Lfunc_end70:
	.size	f77_random_d, .Lfunc_end70-f77_random_d
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_random_desc
	.type	.LCPI71_0,@object
.LCPI71_0:
	.quad	0x4190000000000000              # double 67108864
	.size	.LCPI71_0, 8
	.type	.LCPI71_1,@object
.LCPI71_1:
	.quad	0x3ca0000000000000              # double 1.1102230246251565E-16
	.size	.LCPI71_1, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI71_2,@object
.LCPI71_2:
	.word	0x33800000                      # float 5.96046448E-8
	.size	.LCPI71_2, 4
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
	blt r12, r3, .LBB71_3
.LBB71_1:
	addi r4, r13, 4
	addi r1, r0, 1
	addi r5, r0, 0
	add r6, r12, r0
.LBB71_2:
	ldw r7, r4+0
	sgt r8, r7, r5
	mul r1, r7, r1
	sub r7, r0, r8
	and r1, r1, r7
	addi r6, r6, -1
	addi r4, r4, 8
	bne r6, r0, .LBB71_2
.LBB71_3:
	blt r1, r3, .LBB71_17
.LBB71_4:
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
	jal r0, .LBB71_6
.LBB71_5:
	addi r16, r16, 1
	beq r16, r1, .LBB71_16
.LBB71_6:
	blt r12, r14, .LBB71_9
.LBB71_7:
	addi r18, sp, 16
	add r19, r10, r0
	add r20, r12, r0
	add r17, r0, r0
.LBB71_8:
	ldw r21, r18+0
	ldw r22, r19+0
	mul r21, r22, r21
	add r17, r21, r17
	addi r20, r20, -1
	addi r19, r19, 8
	addi r18, r18, 4
	bne r20, r0, .LBB71_8
	jal r0, .LBB71_10
.LBB71_9:
	add r17, r0, r0
.LBB71_10:
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
	beq r11, r0, .LBB71_12
.LBB71_11:
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
	lui r18, %hi(.LCPI71_0)
	addi r18, r18, %lo(.LCPI71_0)
	ldw r23, r18+4
	ldw r22, r18+0
	fmul.d r20, r20, r22
	fcvt.d.wu r18, r19
	fadd.d r18, r20, r18
	lui r20, %hi(.LCPI71_1)
	addi r20, r20, %lo(.LCPI71_1)
	ldw r23, r20+4
	ldw r22, r20+0
	fmul.d r18, r18, r22
	ldw r20, r13+0
	slli r17, r17, 3
	add r17, r20, r17
	stw r17+4, r19
	stw r17+0, r18
	bge r12, r14, .LBB71_13
	jal r0, .LBB71_5
.LBB71_12:
	srli r18, r18, 8
	fcvt.s.wu r18, r18
	lui r19, %hi(.LCPI71_2)
	ldw r19, r19+%lo(.LCPI71_2)
	fmul.s r18, r18, r19
	ldw r19, r13+0
	slli r17, r17, 2
	add r17, r19, r17
	stw r17+0, r18
	blt r12, r14, .LBB71_5
.LBB71_13:
	addi r17, sp, 16
	add r18, r4, r0
	add r19, r12, r0
.LBB71_14:
	ldw r20, r17+0
	addi r20, r20, 1
	stw r17+0, r20
	ldw r21, r18+0
	blt r20, r21, .LBB71_5
.LBB71_15:
	stw r17+0, r0
	addi r19, r19, -1
	addi r18, r18, 8
	addi r17, r17, 4
	bne r19, r0, .LBB71_14
	jal r0, .LBB71_5
.LBB71_16:
	stw r3+%lo(f77_rng+4), r6
	stw r5+%lo(f77_rng), r8
	stw r7+%lo(f77_rng+8), lr
	stw r9+%lo(f77_rng+12), r15
.LBB71_17:
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
.Lfunc_end71:
	.size	f77_random_desc, .Lfunc_end71-f77_random_desc
                                        # -- End function
	.globl	f77_len_trim                    # -- Begin function f77_len_trim
	.p2align	2
	.type	f77_len_trim,@function
f77_len_trim:                           # @f77_len_trim
# %bb.0:
	srai r1, r4, 31
	and r1, r4, r1
	addi r5, r0, 1
	blt r4, r5, .LBB72_5
.LBB72_1:
	addi r3, r3, -1
	addi r5, r0, 32
	addi r6, r0, 2
.LBB72_2:
	add r7, r4, r0
	add r4, r3, r4
	ldbu r4, r4+0
	bne r4, r5, .LBB72_4
.LBB72_3:
	addi r4, r7, -1
	bge r7, r6, .LBB72_2
	jal r0, .LBB72_5
.LBB72_4:
	add r1, r7, r0
.LBB72_5:
	jalr r0, r31, 0
.Lfunc_end72:
	.size	f77_len_trim, .Lfunc_end72-f77_len_trim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_exponent
	.type	.LCPI73_0,@object
.LCPI73_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI73_0, 8
	.text
	.globl	f77_exponent
	.p2align	2
	.type	f77_exponent,@function
f77_exponent:                           # @f77_exponent
# %bb.0:
	add r7, r4, r0
	add r6, r3, r0
	lui r1, %hi(.LCPI73_0)
	addi r1, r1, %lo(.LCPI73_0)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r6, r4
	add r1, r0, r0
	bne r3, r0, .LBB73_2
.LBB73_1:
	addi sp, sp, -32
	stw sp+0, lr
	addi r5, sp, 28
	add r3, r6, r0
	add r4, r7, r0
	jal r31, frexp
	ldw r1, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB73_2:
	jalr r0, r31, 0
.Lfunc_end73:
	.size	f77_exponent, .Lfunc_end73-f77_exponent
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_zsqrt
	.type	.LCPI74_0,@object
.LCPI74_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI74_0, 8
	.type	.LCPI74_1,@object
.LCPI74_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI74_1, 8
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
	lui r1, %hi(.LCPI74_0)
	addi r1, r1, %lo(.LCPI74_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r14, r1, r0
	add r15, r2, r0
	lui r1, %hi(.LCPI74_1)
	addi r1, r1, %lo(.LCPI74_1)
	ldw r17, r1+4
	ldw r16, r1+0
	fle.d r1, r16, r18
	xori r1, r1, 1
	beq r1, r0, .LBB74_4
.LBB74_1:
	feq.d r1, r14, r16
	add r4, r16, r0
	add r5, r17, r0
	bne r1, r0, .LBB74_3
.LBB74_2:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r14, r14
	fdiv.d r4, r4, r6
.LBB74_3:
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
	jal r0, .LBB74_6
.LBB74_4:
	stw r11+4, r15
	stw r11+0, r14
	feq.d r1, r14, r16
	bne r1, r0, .LBB74_6
.LBB74_5:
	fadd.d r4, r14, r14
	fdiv.d r16, r12, r4
.LBB74_6:
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
.Lfunc_end74:
	.size	f77_zsqrt, .Lfunc_end74-f77_zsqrt
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
.Lfunc_end75:
	.size	f77_zexp, .Lfunc_end75-f77_zexp
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
.Lfunc_end76:
	.size	f77_zlog, .Lfunc_end76-f77_zlog
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
.Lfunc_end77:
	.size	f77_zsin, .Lfunc_end77-f77_zsin
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
.Lfunc_end78:
	.size	f77_zcos, .Lfunc_end78-f77_zcos
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
.Lfunc_end79:
	.size	f77i_iabs, .Lfunc_end79-f77i_iabs
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_abs
	.type	.LCPI80_0,@object
.LCPI80_0:
	.word	0x00000000                      # float 0
	.size	.LCPI80_0, 4
	.text
	.globl	f77i_abs
	.p2align	2
	.type	f77i_abs,@function
f77i_abs:                               # @f77i_abs
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI80_0)
	ldw r3, r3+%lo(.LCPI80_0)
	flt.s r3, r1, r3
	sub r3, r0, r3
	lui r4, 524288
	and r3, r3, r4
	xor r1, r1, r3
	jalr r0, r31, 0
.Lfunc_end80:
	.size	f77i_abs, .Lfunc_end80-f77i_abs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dabs
	.type	.LCPI81_0,@object
.LCPI81_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI81_0, 8
	.text
	.globl	f77i_dabs
	.p2align	2
	.type	f77i_dabs,@function
f77i_dabs:                              # @f77i_dabs
# %bb.0:
	ldw r5, r3+4
	ldw r4, r3+0
	lui r1, %hi(.LCPI81_0)
	addi r1, r1, %lo(.LCPI81_0)
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
.Lfunc_end81:
	.size	f77i_dabs, .Lfunc_end81-f77i_dabs
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
.Lfunc_end82:
	.size	f77i_sqrt, .Lfunc_end82-f77i_sqrt
                                        # -- End function
	.globl	f77i_dsqrt                      # -- Begin function f77i_dsqrt
	.p2align	2
	.type	f77i_dsqrt,@function
f77i_dsqrt:                             # @f77i_dsqrt
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sqrt
.Lfunc_end83:
	.size	f77i_dsqrt, .Lfunc_end83-f77i_dsqrt
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
.Lfunc_end84:
	.size	f77i_exp, .Lfunc_end84-f77i_exp
                                        # -- End function
	.globl	f77i_dexp                       # -- Begin function f77i_dexp
	.p2align	2
	.type	f77i_dexp,@function
f77i_dexp:                              # @f77i_dexp
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, exp
.Lfunc_end85:
	.size	f77i_dexp, .Lfunc_end85-f77i_dexp
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
.Lfunc_end86:
	.size	f77i_alog, .Lfunc_end86-f77i_alog
                                        # -- End function
	.globl	f77i_dlog                       # -- Begin function f77i_dlog
	.p2align	2
	.type	f77i_dlog,@function
f77i_dlog:                              # @f77i_dlog
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log
.Lfunc_end87:
	.size	f77i_dlog, .Lfunc_end87-f77i_dlog
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
.Lfunc_end88:
	.size	f77i_alog10, .Lfunc_end88-f77i_alog10
                                        # -- End function
	.globl	f77i_dlog10                     # -- Begin function f77i_dlog10
	.p2align	2
	.type	f77i_dlog10,@function
f77i_dlog10:                            # @f77i_dlog10
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, log10
.Lfunc_end89:
	.size	f77i_dlog10, .Lfunc_end89-f77i_dlog10
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
.Lfunc_end90:
	.size	f77i_sin, .Lfunc_end90-f77i_sin
                                        # -- End function
	.globl	f77i_dsin                       # -- Begin function f77i_dsin
	.p2align	2
	.type	f77i_dsin,@function
f77i_dsin:                              # @f77i_dsin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sin
.Lfunc_end91:
	.size	f77i_dsin, .Lfunc_end91-f77i_dsin
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
.Lfunc_end92:
	.size	f77i_cos, .Lfunc_end92-f77i_cos
                                        # -- End function
	.globl	f77i_dcos                       # -- Begin function f77i_dcos
	.p2align	2
	.type	f77i_dcos,@function
f77i_dcos:                              # @f77i_dcos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cos
.Lfunc_end93:
	.size	f77i_dcos, .Lfunc_end93-f77i_dcos
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
.Lfunc_end94:
	.size	f77i_tan, .Lfunc_end94-f77i_tan
                                        # -- End function
	.globl	f77i_dtan                       # -- Begin function f77i_dtan
	.p2align	2
	.type	f77i_dtan,@function
f77i_dtan:                              # @f77i_dtan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tan
.Lfunc_end95:
	.size	f77i_dtan, .Lfunc_end95-f77i_dtan
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
.Lfunc_end96:
	.size	f77i_asin, .Lfunc_end96-f77i_asin
                                        # -- End function
	.globl	f77i_dasin                      # -- Begin function f77i_dasin
	.p2align	2
	.type	f77i_dasin,@function
f77i_dasin:                             # @f77i_dasin
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, asin
.Lfunc_end97:
	.size	f77i_dasin, .Lfunc_end97-f77i_dasin
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
.Lfunc_end98:
	.size	f77i_acos, .Lfunc_end98-f77i_acos
                                        # -- End function
	.globl	f77i_dacos                      # -- Begin function f77i_dacos
	.p2align	2
	.type	f77i_dacos,@function
f77i_dacos:                             # @f77i_dacos
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, acos
.Lfunc_end99:
	.size	f77i_dacos, .Lfunc_end99-f77i_dacos
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
.Lfunc_end100:
	.size	f77i_atan, .Lfunc_end100-f77i_atan
                                        # -- End function
	.globl	f77i_datan                      # -- Begin function f77i_datan
	.p2align	2
	.type	f77i_datan,@function
f77i_datan:                             # @f77i_datan
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, atan
.Lfunc_end101:
	.size	f77i_datan, .Lfunc_end101-f77i_datan
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
.Lfunc_end102:
	.size	f77i_atan2, .Lfunc_end102-f77i_atan2
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
.Lfunc_end103:
	.size	f77i_datan2, .Lfunc_end103-f77i_datan2
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
.Lfunc_end104:
	.size	f77i_sinh, .Lfunc_end104-f77i_sinh
                                        # -- End function
	.globl	f77i_dsinh                      # -- Begin function f77i_dsinh
	.p2align	2
	.type	f77i_dsinh,@function
f77i_dsinh:                             # @f77i_dsinh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, sinh
.Lfunc_end105:
	.size	f77i_dsinh, .Lfunc_end105-f77i_dsinh
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
.Lfunc_end106:
	.size	f77i_cosh, .Lfunc_end106-f77i_cosh
                                        # -- End function
	.globl	f77i_dcosh                      # -- Begin function f77i_dcosh
	.p2align	2
	.type	f77i_dcosh,@function
f77i_dcosh:                             # @f77i_dcosh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, cosh
.Lfunc_end107:
	.size	f77i_dcosh, .Lfunc_end107-f77i_dcosh
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
.Lfunc_end108:
	.size	f77i_tanh, .Lfunc_end108-f77i_tanh
                                        # -- End function
	.globl	f77i_dtanh                      # -- Begin function f77i_dtanh
	.p2align	2
	.type	f77i_dtanh,@function
f77i_dtanh:                             # @f77i_dtanh
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, tanh
.Lfunc_end109:
	.size	f77i_dtanh, .Lfunc_end109-f77i_dtanh
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
.Lfunc_end110:
	.size	f77i_aint, .Lfunc_end110-f77i_aint
                                        # -- End function
	.globl	f77i_dint                       # -- Begin function f77i_dint
	.p2align	2
	.type	f77i_dint,@function
f77i_dint:                              # @f77i_dint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, trunc
.Lfunc_end111:
	.size	f77i_dint, .Lfunc_end111-f77i_dint
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
.Lfunc_end112:
	.size	f77i_anint, .Lfunc_end112-f77i_anint
                                        # -- End function
	.globl	f77i_dnint                      # -- Begin function f77i_dnint
	.p2align	2
	.type	f77i_dnint,@function
f77i_dnint:                             # @f77i_dnint
# %bb.0:
	ldw r4, r3+4
	ldw r3, r3+0
	jal r0, round
.Lfunc_end113:
	.size	f77i_dnint, .Lfunc_end113-f77i_dnint
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
.Lfunc_end114:
	.size	f77i_nint, .Lfunc_end114-f77i_nint
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
.Lfunc_end115:
	.size	f77i_idnint, .Lfunc_end115-f77i_idnint
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
.Lfunc_end116:
	.size	f77i_mod, .Lfunc_end116-f77i_mod
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
.Lfunc_end117:
	.size	f77i_amod, .Lfunc_end117-f77i_amod
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
.Lfunc_end118:
	.size	f77i_dmod, .Lfunc_end118-f77i_dmod
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
.Lfunc_end119:
	.size	f77i_isign, .Lfunc_end119-f77i_isign
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77i_sign
	.type	.LCPI120_0,@object
.LCPI120_0:
	.word	0x00000000                      # float 0
	.size	.LCPI120_0, 4
	.text
	.globl	f77i_sign
	.p2align	2
	.type	f77i_sign,@function
f77i_sign:                              # @f77i_sign
# %bb.0:
	ldw r1, r3+0
	lui r3, %hi(.LCPI120_0)
	ldw r3, r3+%lo(.LCPI120_0)
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
.Lfunc_end120:
	.size	f77i_sign, .Lfunc_end120-f77i_sign
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_dsign
	.type	.LCPI121_0,@object
.LCPI121_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI121_0, 8
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
	lui r1, %hi(.LCPI121_0)
	addi r1, r1, %lo(.LCPI121_0)
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
.Lfunc_end121:
	.size	f77i_dsign, .Lfunc_end121-f77i_dsign
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
.Lfunc_end122:
	.size	f77i_idim, .Lfunc_end122-f77i_idim
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
.Lfunc_end123:
	.size	f77i_dim, .Lfunc_end123-f77i_dim
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_ddim
	.type	.LCPI124_0,@object
.LCPI124_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI124_0, 8
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
	lui r3, %hi(.LCPI124_0)
	addi r3, r3, %lo(.LCPI124_0)
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
.Lfunc_end124:
	.size	f77i_ddim, .Lfunc_end124-f77i_ddim
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
.Lfunc_end125:
	.size	f77i_dprod, .Lfunc_end125-f77i_dprod
                                        # -- End function
	.globl	f77i_len                        # -- Begin function f77i_len
	.p2align	2
	.type	f77i_len,@function
f77i_len:                               # @f77i_len
# %bb.0:
	add r1, r4, r0
	jalr r0, r31, 0
.Lfunc_end126:
	.size	f77i_len, .Lfunc_end126-f77i_len
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
	beq r6, r0, .LBB127_6
.LBB127_1:
	add r12, r6, r0
	bgt r6, r11, .LBB127_5
.LBB127_2:
	add r13, r4, r0
	add r14, r3, r0
	add r15, r0, r0
.LBB127_3:
	add r3, r14, r15
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcmp
	beq r1, r0, .LBB127_7
.LBB127_4:
	addi r15, r15, 1
	add r1, r12, r15
	ble r1, r11, .LBB127_3
.LBB127_5:
	add r1, r0, r0
	jal r0, .LBB127_8
.LBB127_6:
	addi r1, r0, 0
	sgt r1, r11, r1
	jal r0, .LBB127_8
.LBB127_7:
	addi r1, r15, 1
.LBB127_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end127:
	.size	f77i_index, .Lfunc_end127-f77i_index
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
	beq r6, r0, .LBB128_6
.LBB128_1:
	add r12, r6, r0
	bgt r6, r11, .LBB128_5
.LBB128_2:
	add r13, r5, r0
	add r14, r3, r0
	add r15, r0, r0
.LBB128_3:
	add r3, r14, r15
	add r4, r13, r0
	add r5, r12, r0
	jal r31, memcmp
	beq r1, r0, .LBB128_7
.LBB128_4:
	addi r15, r15, 1
	add r1, r12, r15
	ble r1, r11, .LBB128_3
.LBB128_5:
	add r1, r0, r0
	jal r0, .LBB128_8
.LBB128_6:
	addi r1, r0, 0
	sgt r1, r11, r1
	jal r0, .LBB128_8
.LBB128_7:
	addi r1, r15, 1
.LBB128_8:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end128:
	.size	f77_index, .Lfunc_end128-f77_index
                                        # -- End function
	.globl	f77i_aimag                      # -- Begin function f77i_aimag
	.p2align	2
	.type	f77i_aimag,@function
f77i_aimag:                             # @f77i_aimag
# %bb.0:
	ldw r1, r3+4
	jalr r0, r31, 0
.Lfunc_end129:
	.size	f77i_aimag, .Lfunc_end129-f77i_aimag
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
.Lfunc_end130:
	.size	f77i_cabs, .Lfunc_end130-f77i_cabs
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77i_csqrt
	.type	.LCPI131_0,@object
.LCPI131_0:
	.quad	0x3fe0000000000000              # double 0.5
	.size	.LCPI131_0, 8
	.type	.LCPI131_2,@object
.LCPI131_2:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI131_2, 8
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
	.type	.LCPI131_1,@object
.LCPI131_1:
	.word	0x00000000                      # float 0
	.size	.LCPI131_1, 4
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
	lui r1, %hi(.LCPI131_0)
	addi r1, r1, %lo(.LCPI131_0)
	ldw r7, r1+4
	ldw r6, r1+0
	fmul.d r4, r4, r6
	add r3, r4, r0
	add r4, r5, r0
	jal r31, sqrt
	add r14, r1, r0
	add r15, r2, r0
	lui r1, %hi(.LCPI131_1)
	ldw r3, r1+%lo(.LCPI131_1)
	fle.s r4, r3, r16
	xori r4, r4, 1
	beq r4, r0, .LBB131_4
.LBB131_1:
	ldw r16, r1+%lo(.LCPI131_1)
	lui r1, %hi(.LCPI131_2)
	addi r1, r1, %lo(.LCPI131_2)
	ldw r5, r1+4
	ldw r4, r1+0
	feq.d r3, r14, r4
	add r1, r16, r0
	bne r3, r0, .LBB131_3
.LBB131_2:
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fabs
	add r4, r1, r0
	add r5, r2, r0
	fadd.d r6, r14, r14
	fdiv.d r4, r4, r6
	fcvt.s.d r1, r4
.LBB131_3:
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
	jal r0, .LBB131_6
.LBB131_4:
	fcvt.s.d r1, r14
	lui r4, %hi(.LCPI131_2)
	addi r4, r4, %lo(.LCPI131_2)
	ldw r7, r4+4
	ldw r6, r4+0
	feq.d r4, r14, r6
	bne r4, r0, .LBB131_6
.LBB131_5:
	fadd.d r4, r14, r14
	fdiv.d r4, r12, r4
	fcvt.s.d r3, r4
.LBB131_6:
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
.Lfunc_end131:
	.size	f77i_csqrt, .Lfunc_end131-f77i_csqrt
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
.Lfunc_end132:
	.size	f77i_cexp, .Lfunc_end132-f77i_cexp
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
.Lfunc_end133:
	.size	f77i_clog, .Lfunc_end133-f77i_clog
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
.Lfunc_end134:
	.size	f77i_csin, .Lfunc_end134-f77i_csin
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
.Lfunc_end135:
	.size	f77i_ccos, .Lfunc_end135-f77i_ccos
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
.Lfunc_end136:
	.size	f77i_conjg, .Lfunc_end136-f77i_conjg
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
	blt r14, r1, .LBB137_2
.LBB137_1:
	add r3, r12, r0
	add r4, r5, r0
	add r5, r14, r0
	jal r31, memmove
.LBB137_2:
	bge r13, r11, .LBB137_5
.LBB137_3:
	add r1, r12, r14
	sub r3, r11, r14
	addi r4, r0, 32
.LBB137_4:
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB137_4
.LBB137_5:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end137:
	.size	f77_strcpy, .Lfunc_end137-f77_strcpy
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
	blt r7, r1, .LBB138_8
.LBB138_1:
	addi r8, r0, 32
	add r9, r0, r0
                                        # implicit-def: $r1
.LBB138_2:
	add r10, r8, r0
	bge r9, r4, .LBB138_4
.LBB138_3:
	add r10, r3, r9
	ldbu r10, r10+0
.LBB138_4:
	add lr, r8, r0
	bge r9, r6, .LBB138_6
.LBB138_5:
	add lr, r5, r9
	ldbu lr, lr+0
.LBB138_6:
	seq r11, r10, lr
	sltu r12, r10, lr
	sub r12, r0, r12
	ori  r12, r12, 1
	xor r1, r1, r12
	sub r11, r0, r11
	and r1, r1, r11
	xor r1, r12, r1
	bne r10, lr, .LBB138_9
.LBB138_7:
	addi r9, r9, 1
	bne r7, r9, .LBB138_2
.LBB138_8:
	add r1, r0, r0
.LBB138_9:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end138:
	.size	f77_strcmp, .Lfunc_end138-f77_strcmp
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
	bgeu r5, r6, .LBB139_8
.LBB139_1:
	lui r3, %hi(fio_catpos)
	ldw r7, r3+%lo(fio_catpos)
	add r7, r7, r5
	blt r7, r6, .LBB139_3
.LBB139_2:
	stw r3+%lo(fio_catpos), r0
.LBB139_3:
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
	blt r1, r15, .LBB139_5
.LBB139_4:
	add r3, r13, r0
	add r5, r1, r0
	jal r31, memmove
.LBB139_5:
	blt r11, r15, .LBB139_7
.LBB139_6:
	add r3, r13, r14
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memmove
.LBB139_7:
	add r1, r13, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB139_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end139:
	.size	f77_cat, .Lfunc_end139-f77_cat
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
	ble r5, r1, .LBB140_8
.LBB140_1:
	add r12, r3, r0
	addi r1, r0, 0
	sgt r1, r4, r1
	sub r1, r0, r1
	and r13, r4, r1
	mul r5, r13, r11
	lui r1, 16
	addi r3, r1, 1
	bge r5, r3, .LBB140_9
.LBB140_2:
	lui r1, %hi(fio_catpos)
	ldw r4, r1+%lo(fio_catpos)
	add r4, r4, r5
	blt r4, r3, .LBB140_4
.LBB140_3:
	stw r1+%lo(fio_catpos), r0
.LBB140_4:
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
	beq r11, r0, .LBB140_7
.LBB140_5:
	add r15, r14, r0
.LBB140_6:
	add r3, r15, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, memmove
	addi r11, r11, -1
	add r15, r15, r13
	bne r11, r0, .LBB140_6
.LBB140_7:
	add r1, r14, r0
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB140_8:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.31)
	addi r4, r4, %lo(.L.str.31)
	add r5, r11, r0
	jal r0, .LBB140_10
.LBB140_9:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.119)
	addi r4, r4, %lo(.L.str.119)
.LBB140_10:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end140:
	.size	f77_repeat, .Lfunc_end140-f77_repeat
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
	bge r4, r3, .LBB141_18
.LBB141_1:
	lui r1, %hi(fio_catpos)
	ldw r4, r1+%lo(fio_catpos)
	add r4, r4, r11
	blt r4, r3, .LBB141_3
.LBB141_2:
	stw r1+%lo(fio_catpos), r0
.LBB141_3:
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
	beq r5, r0, .LBB141_10
.LBB141_4:
	add r14, r11, r0
	blt r11, r1, .LBB141_9
.LBB141_5:
	addi r3, r0, 32
	addi r4, r13, -1
	add r5, r11, r0
.LBB141_6:
	add r14, r5, r0
	add r5, r4, r5
	ldbu r5, r5+0
	bne r5, r3, .LBB141_9
.LBB141_7:
	addi r5, r14, -1
	bgt r14, r1, .LBB141_6
.LBB141_8:
	add r14, r0, r0
.LBB141_9:
	sub r5, r11, r14
	addi r4, r0, 32
	add r3, r12, r0
	jal r31, memset
	add r1, r12, r11
	sub r3, r1, r14
	add r4, r13, r0
	add r5, r14, r0
	jal r31, memmove
	jal r0, .LBB141_17
.LBB141_10:
	blt r11, r1, .LBB141_15
.LBB141_11:
	addi r1, r0, 32
	add r14, r0, r0
.LBB141_12:
	add r3, r13, r14
	ldbu r3, r3+0
	bne r3, r1, .LBB141_16
.LBB141_13:
	addi r14, r14, 1
	bne r11, r14, .LBB141_12
.LBB141_14:
	add r14, r11, r0
	jal r0, .LBB141_16
.LBB141_15:
	add r14, r0, r0
.LBB141_16:
	add r4, r13, r14
	sub r5, r11, r14
	add r3, r12, r0
	jal r31, memmove
	add r1, r12, r11
	sub r3, r1, r14
	addi r4, r0, 32
	add r5, r14, r0
	jal r31, memset
.LBB141_17:
	add r1, r12, r0
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB141_18:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.119)
	addi r4, r4, %lo(.L.str.119)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end141:
	.size	f77_adjust, .Lfunc_end141-f77_adjust
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
	blt r4, r16, .LBB142_7
.LBB142_1:
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
.LBB142_2:
	xor r1, r21, r17
	and r1, r1, r19
	xor r22, r17, r1
	blt r12, r16, .LBB142_4
.LBB142_3:
	add r1, r14, r22
	ldb r4, r1+0
	add r3, r13, r0
	add r5, r12, r0
	jal r31, memchr
	sne r1, r1, r18
	jal r0, .LBB142_5
.LBB142_4:
	add r1, r0, r0
.LBB142_5:
	seq r3, r1, r11
	addi r4, r22, 1
	xor r5, r15, r4
	sub r3, r0, r3
	and r3, r5, r3
	xor r15, r4, r3
	bne r1, r11, .LBB142_8
.LBB142_6:
	addi r21, r21, 1
	addi r17, r17, -1
	bne r17, r20, .LBB142_2
.LBB142_7:
	add r15, r0, r0
.LBB142_8:
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
.Lfunc_end142:
	.size	f77_scanset, .Lfunc_end142-f77_scanset
                                        # -- End function
	.globl	f77_index_back                  # -- Begin function f77_index_back
	.p2align	2
	.type	f77_index_back,@function
f77_index_back:                         # @f77_index_back
# %bb.0:
	ble r6, r0, .LBB143_6
.LBB143_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r11, r6, r0
	sub r1, r4, r6
	blt r1, r0, .LBB143_5
.LBB143_2:
	add r12, r5, r0
	add r13, r3, r0
.LBB143_3:
	add r14, r1, r0
	add r3, r13, r1
	add r4, r12, r0
	add r5, r11, r0
	jal r31, memcmp
	beq r1, r0, .LBB143_7
.LBB143_4:
	addi r1, r14, -1
	bgt r14, r0, .LBB143_3
.LBB143_5:
	add r1, r0, r0
	jal r0, .LBB143_8
.LBB143_6:
	addi r1, r0, 0
	sgt r1, r4, r1
	sub r1, r0, r1
	and r1, r4, r1
	addi r1, r1, 1
	jal r0, .LBB143_9
.LBB143_7:
	addi r1, r14, 1
.LBB143_8:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB143_9:
	jalr r0, r31, 0
.Lfunc_end143:
	.size	f77_index_back, .Lfunc_end143-f77_index_back
                                        # -- End function
	.globl	f77_wr_l                        # -- Begin function f77_wr_l
	.p2align	2
	.type	f77_wr_l,@function
f77_wr_l:                               # @f77_wr_l
# %bb.0:
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB144_15
.LBB144_1:
	lui r4, %hi(fio_col)
	ldw r7, r4+%lo(fio_col)
	addi r6, r0, 1022
	lui r1, %hi(fio_len)
	lui r5, %hi(fio_line)
	addi r5, r5, %lo(fio_line)
	bgt r7, r6, .LBB144_7
.LBB144_2:
	ldw r9, r1+%lo(fio_len)
	addi r8, r0, 32
	bge r9, r7, .LBB144_5
.LBB144_3:
	addi r10, r9, 1
	add r9, r9, r5
	stb r9+0, r8
	add r9, r10, r0
	bne r7, r10, .LBB144_3
.LBB144_4:
	stw r1+%lo(fio_len), r7
	add r9, r7, r0
.LBB144_5:
	add r10, r7, r5
	stb r10+0, r8
	addi r8, r7, 1
	stw r4+%lo(fio_col), r8
	blt r7, r9, .LBB144_7
.LBB144_6:
	stw r1+%lo(fio_len), r8
.LBB144_7:
	ldw r7, r4+%lo(fio_col)
	bgt r7, r6, .LBB144_14
.LBB144_8:
	ldw r6, r1+%lo(fio_len)
	bge r6, r7, .LBB144_12
.LBB144_9:
	addi r8, r0, 32
.LBB144_10:
	addi r9, r6, 1
	add r6, r6, r5
	stb r6+0, r8
	add r6, r9, r0
	bne r7, r9, .LBB144_10
.LBB144_11:
	stw r1+%lo(fio_len), r7
	add r6, r7, r0
.LBB144_12:
	addi r8, r0, 0
	seq r3, r3, r8
	sub r3, r0, r3
	andi r3, r3, 18
	xori r3, r3, 84
	add r5, r7, r5
	stb r5+0, r3
	addi r3, r7, 1
	stw r4+%lo(fio_col), r3
	blt r7, r6, .LBB144_14
.LBB144_13:
	stw r1+%lo(fio_len), r3
.LBB144_14:
	lui r1, %hi(fio_first_item)
	stb r1+%lo(fio_first_item), r0
	lui r1, %hi(fio_prev_char)
	stb r1+%lo(fio_prev_char), r0
	jal r0, .LBB144_33
.LBB144_15:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB144_33
.LBB144_16:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	lui r11, %hi(fio_desc)
	ldw r1, r11+%lo(fio_desc)
	lui r12, %hi(fio_rep)
	beq r1, r0, .LBB144_18
.LBB144_17:
	ldw r1, r12+%lo(fio_rep)
	bgt r1, r0, .LBB144_19
.LBB144_18:
	stw r11+%lo(fio_desc), r0
	add r13, r3, r0
	jal r31, fio_next_desc
	add r3, r13, r0
	beq r1, r0, .LBB144_32
.LBB144_19:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r4, r0, 2
	blt r1, r4, .LBB144_21
.LBB144_20:
	lui r4, %hi(fio_col)
	ldw r5, r4+%lo(fio_col)
	add r1, r1, r5
	addi r1, r1, -1
	stw r4+%lo(fio_col), r1
.LBB144_21:
	lui r4, %hi(fio_col)
	ldw r1, r4+%lo(fio_col)
	addi r5, r0, 1022
	bgt r1, r5, .LBB144_28
.LBB144_22:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	bge r6, r1, .LBB144_26
.LBB144_23:
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	addi r8, r0, 32
.LBB144_24:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r1, r9, .LBB144_24
.LBB144_25:
	stw r5+%lo(fio_len), r1
	add r6, r1, r0
.LBB144_26:
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
	blt r1, r6, .LBB144_28
.LBB144_27:
	stw r5+%lo(fio_len), r3
.LBB144_28:
	ldw r1, r12+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB144_30
.LBB144_29:
	addi r1, r1, -1
	stw r12+%lo(fio_rep), r1
.LBB144_30:
	ldw r1, r12+%lo(fio_rep)
	bne r1, r0, .LBB144_32
.LBB144_31:
	stw r11+%lo(fio_desc), r0
.LBB144_32:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB144_33:
	jalr r0, r31, 0
.Lfunc_end144:
	.size	f77_wr_l, .Lfunc_end144-f77_wr_l
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
	bne r1, r0, .LBB145_4
.LBB145_1:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB145_3
.LBB145_2:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB145_4
.LBB145_3:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB145_4:
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
.Lfunc_end145:
	.size	f77_wr_end, .Lfunc_end145-f77_wr_end
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
	lui r27, %hi(.LJTI146_4)
	addi r27, r27, %lo(.LJTI146_4)
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
	jal r0, .LBB146_5
.LBB146_1:
	addi r1, r4, 1
.LBB146_2:
	stw r14+%lo(fio_pos), r1
.LBB146_3:
	bgtu r16, r18, .LBB146_109
.LBB146_4:
	addi r16, r16, 1
.LBB146_5:
	ldw r3, r15+%lo(fio_fmt)
	ldw r4, r14+%lo(fio_pos)
	add r7, r3, r4
	ldb r6, r7+0
	andi r1, r6, 255
	beq r1, r17, .LBB146_1
.LBB146_6:
	beq r1, r19, .LBB146_1
.LBB146_7:
	bne r1, r0, .LBB146_16
.LBB146_8:
	ldw r1, r20+%lo(fio_gdepth)
	blt r1, fp, .LBB146_10
.LBB146_9:
	stw r20+%lo(fio_gdepth), r0
.LBB146_10:
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB146_109
.LBB146_11:
	ldw r4, r22+%lo(fio_revert)
	add r1, r0, r0
	blt r4, r0, .LBB146_110
.LBB146_12:
	ldbu r1, r13+%lo(fio_since_data)
	beq r1, r0, .LBB146_131
.LBB146_13:
	stb r13+%lo(fio_since_data), r0
	stw r14+%lo(fio_pos), r4
	add r1, r3, r4
	ldbu r1, r1+0
	beq r1, r0, .LBB146_109
.LBB146_14:
	ldbu r1, r23+%lo(fio_reading)
	bne r1, fp, .LBB146_88
.LBB146_15:
	jal r31, fio_getrec
	beq r1, r0, .LBB146_3
	jal r0, .LBB146_132
.LBB146_16:
	and r5, r6, r24
	bne r5, r12, .LBB146_20
.LBB146_17:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	add r1, r3, r1
	ldbu r1, r1+0
	addi r1, r1, -80
	bgtu r1, r25, .LBB146_50
.LBB146_18:
	slli r1, r1, 2
	add r1, r27, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB146_19:
	stb r26+%lo(fio_sp), fp
	jal r0, .LBB146_28
.LBB146_20:
	bne r6, r28, .LBB146_23
.LBB146_21:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB146_109
.LBB146_22:
	add r1, r0, r0
	bleu r16, r18, .LBB146_4
	jal r0, .LBB146_110
.LBB146_23:
	andi r5, r6, 223
	ldw r8, sp+52
	bne r5, r8, .LBB146_29
.LBB146_24:
	ldbu r5, r7+1
	addi r8, r5, -78
	andi r9, r8, 252
	slli r8, r8, 6
	srli r9, r9, 2
	or  r8, r8, r9
	andi r8, r8, 255
	ldw r9, sp+44
	bgtu r8, r9, .LBB146_29
.LBB146_25:
	sll r8, fp, r8
	andi r8, r8, 2313
	beq r8, r0, .LBB146_29
.LBB146_26:
	andi r1, r5, 223
	ldw r3, sp+40
	seq r1, r1, r3
	ldw r3, sp+36
	stw r3+%lo(fio_bz), r1
	jal r0, .LBB146_28
.LBB146_27:
	stb r26+%lo(fio_sp), r0
.LBB146_28:
	addi r1, r4, 2
	jal r0, .LBB146_2
.LBB146_29:
	addi r5, r1, -39
	lui r1, %hi(fio_line)
	addi r1, r1, %lo(fio_line)
	ldw r8, sp+48
	bgtu r5, r8, .LBB146_53
.LBB146_30:
	slli r5, r5, 2
	lui r8, %hi(.LJTI146_0)
	addi r8, r8, %lo(.LJTI146_0)
	add r5, r8, r5
	ldw r8, r5+0
	add r5, fp, r0
	jalr r0, r8, 0
.LBB146_31:
	addi r4, r4, 1
	stw r14+%lo(fio_pos), r4
	lui r5, %hi(fio_col)
	ldw r10, r5+%lo(fio_col)
	lui r6, %hi(fio_len)
	ldw r8, r6+%lo(fio_len)
	ldbu r7, r23+%lo(fio_reading)
	jal r0, .LBB146_35
.LBB146_32:
	addi lr, r0, 1
.LBB146_33:
	add r10, r9, r0
.LBB146_34:
	add r4, r4, lr
	stw r14+%lo(fio_pos), r4
.LBB146_35:
	add r9, r10, r0
	add r10, r3, r4
	ldbu r10, r10+0
	addi lr, r0, 39
	beq r10, lr, .LBB146_42
.LBB146_36:
	beq r10, r0, .LBB146_3
.LBB146_37:
	addi lr, r0, 1022
	sgt lr, r9, lr
	or  lr, r7, lr
	andi lr, lr, 1
	bne lr, r0, .LBB146_32
.LBB146_38:
	bge r8, r9, .LBB146_41
.LBB146_39:
	addi lr, r8, 1
	add r8, r8, r1
	stb r8+0, r19
	add r8, lr, r0
	bne r9, lr, .LBB146_39
.LBB146_40:
	stw r6+%lo(fio_len), r9
	add r8, r9, r0
.LBB146_41:
	add lr, r9, r1
	stb lr+0, r10
	addi r10, r9, 1
	stw r5+%lo(fio_col), r10
	addi lr, r0, 1
	blt r9, r8, .LBB146_34
	jal r0, .LBB146_49
.LBB146_42:
	addi r10, r4, 1
	add r11, r3, r10
	ldbu r11, r11+0
	bne r11, lr, .LBB146_82
.LBB146_43:
	addi r10, r0, 1022
	sgt r10, r9, r10
	or  r10, r7, r10
	andi r10, r10, 1
	beq r10, r0, .LBB146_45
.LBB146_44:
	addi lr, r0, 2
	jal r0, .LBB146_33
.LBB146_45:
	bge r8, r9, .LBB146_48
.LBB146_46:
	addi r10, r8, 1
	add r8, r8, r1
	stb r8+0, r19
	add r8, r10, r0
	bne r9, r10, .LBB146_46
.LBB146_47:
	stw r6+%lo(fio_len), r9
	add r8, r9, r0
.LBB146_48:
	add r10, r9, r1
	stb r10+0, lr
	addi r10, r9, 1
	stw r5+%lo(fio_col), r10
	addi lr, r0, 2
	blt r9, r8, .LBB146_34
.LBB146_49:
	stw r6+%lo(fio_len), r10
	add r8, r10, r0
	jal r0, .LBB146_34
.LBB146_50:
	stb r26+%lo(fio_sp), r0
	jal r0, .LBB146_3
.LBB146_51:
	ldb r5, r7+1
	addi r5, r5, -58
	addi r6, r0, -10
	bgeu r5, r6, .LBB146_58
.LBB146_52:
	addi r6, r0, 45
.LBB146_53:
	add r5, fp, r0
	jal r0, .LBB146_59
.LBB146_54:
	ldw r1, r20+%lo(fio_gdepth)
	beq r1, r0, .LBB146_83
.LBB146_55:
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
	blt r5, r4, .LBB146_3
.LBB146_56:
	lui r4, %hi(fio_gstart)
	addi r4, r4, %lo(fio_gstart)
	add r3, r3, r4
	ldw r3, r3+0
	stw r14+%lo(fio_pos), r3
	stw r20+%lo(fio_gdepth), r1
	jal r0, .LBB146_3
.LBB146_57:
	addi r1, r4, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB146_87
.LBB146_58:
	addi r5, r4, 1
	stw r14+%lo(fio_pos), r5
	add r5, r3, r5
	ldb r6, r5+0
	add r5, r0, r0
.LBB146_59:
	addi r6, r6, -58
	addi r10, r0, -10
	add r11, fp, r0
	bltu r6, r10, .LBB146_65
.LBB146_60:
	ldw r6, r14+%lo(fio_pos)
	add r7, r3, r6
	ldb r7, r7+0
	addi r7, r7, -48
	addi r8, r0, 9
	bleu r7, r8, .LBB146_62
.LBB146_61:
	add r11, r0, r0
	jal r0, .LBB146_65
.LBB146_62:
	stw sp+32, r10
	addi r8, r3, 1
	add r11, r0, r0
.LBB146_63:
	addi r10, r0, 10
	mul lr, r11, r10
	addi r9, r6, 1
	add r11, lr, r7
	add r6, r8, r6
	ldb r6, r6+0
	addi r7, r6, -48
	add r6, r9, r0
	bltu r7, r10, .LBB146_63
.LBB146_64:
	stw r14+%lo(fio_pos), r9
	ldw r10, sp+32
.LBB146_65:
	ldw r6, r14+%lo(fio_pos)
	add r7, r3, r6
	ldbu r7, r7+0
	addi r8, r0, 40
	bne r7, r8, .LBB146_69
.LBB146_66:
	addi r3, r6, 1
	stw r14+%lo(fio_pos), r3
	ldw r1, r20+%lo(fio_gdepth)
	addi r5, r0, 15
	bgt r1, r5, .LBB146_3
.LBB146_67:
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
	bne r1, r0, .LBB146_3
.LBB146_68:
	stw r22+%lo(fio_revert), r4
	jal r0, .LBB146_3
.LBB146_69:
	andi r4, r7, 223
	addi r8, r4, -72
	addi r9, r0, 16
	bgtu r8, r9, .LBB146_101
.LBB146_70:
	slli r8, r8, 2
	lui r9, %hi(.LJTI146_1)
	addi r9, r9, %lo(.LJTI146_1)
	add r8, r9, r8
	ldw r8, r8+0
	jalr r0, r8, 0
.LBB146_71:
	addi r4, r6, 1
	addi r5, r0, 1
	blt r11, r5, .LBB146_97
.LBB146_72:
	lui r6, %hi(fio_len)
	stw sp+32, r6
	ldw r9, r6+%lo(fio_len)
	lui r7, %hi(fio_col)
	ldw r10, r7+%lo(fio_col)
	ldbu r8, r23+%lo(fio_reading)
	jal r0, .LBB146_75
.LBB146_73:
	add r10, r11, r0
.LBB146_74:
	addi r11, lr, -1
	addi r4, r4, 1
	ble lr, r5, .LBB146_97
.LBB146_75:
	add lr, r11, r0
	add r11, r3, r4
	ldbu r11, r11+0
	beq r11, r0, .LBB146_97
.LBB146_76:
	addi r6, r0, 1022
	sgt r6, r10, r6
	or  r6, r8, r6
	andi r6, r6, 1
	bne r6, r0, .LBB146_74
.LBB146_77:
	bge r9, r10, .LBB146_80
.LBB146_78:
	addi r6, r9, 1
	add r9, r9, r1
	stb r9+0, r19
	add r9, r6, r0
	bne r10, r6, .LBB146_78
.LBB146_79:
	ldw r6, sp+32
	stw r6+%lo(fio_len), r10
	add r9, r10, r0
.LBB146_80:
	add r6, r10, r1
	stb r6+0, r11
	addi r11, r10, 1
	stw r7+%lo(fio_col), r11
	blt r10, r9, .LBB146_73
.LBB146_81:
	ldw r6, sp+32
	stw r6+%lo(fio_len), r11
	add r9, r11, r0
	jal r0, .LBB146_73
.LBB146_82:
	stw r14+%lo(fio_pos), r10
	jal r0, .LBB146_3
.LBB146_83:
	ldbu r1, r21+%lo(fio_draining)
	bne r1, r0, .LBB146_109
.LBB146_84:
	ldw r4, r22+%lo(fio_revert)
	add r1, r0, r0
	blt r4, r0, .LBB146_110
.LBB146_85:
	ldbu r1, r13+%lo(fio_since_data)
	beq r1, r0, .LBB146_131
.LBB146_86:
	stb r13+%lo(fio_since_data), r0
	stw r14+%lo(fio_pos), r4
	add r1, r3, r4
	ldbu r1, r1+0
	beq r1, r0, .LBB146_109
.LBB146_87:
	ldbu r1, r23+%lo(fio_reading)
	addi r3, r0, 1
	beq r1, r3, .LBB146_15
.LBB146_88:
	jal r31, fio_flush_line
	jal r0, .LBB146_3
.LBB146_89:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	add r1, r3, r1
	ldbu r1, r1+0
	add r8, r0, r0
	addi r11, r0, 1
	addi r4, r1, -76
	addi r1, r0, 38
	bgtu r4, r1, .LBB146_108
.LBB146_90:
	addi r1, r0, -1
	slli r4, r4, 2
	lui r5, %hi(.LJTI146_2)
	addi r5, r5, %lo(.LJTI146_2)
	add r4, r5, r4
	ldw r4, r4+0
	add r7, r11, r0
	jalr r0, r4, 0
.LBB146_91:
	addi r1, r0, 1
.LBB146_92:
	addi r4, r6, 2
	stw r14+%lo(fio_pos), r4
	add r7, r0, r0
	add r8, r1, r0
.LBB146_93:
	ldw r1, r14+%lo(fio_pos)
	add r1, r3, r1
	ldb r1, r1+0
	addi r3, r1, -58
	add r1, r11, r0
	bltu r3, r10, .LBB146_95
.LBB146_94:
	stw sp+32, r7
	stw sp+28, r8
	jal r31, fio_number
	ldw r8, sp+28
	ldw r7, sp+32
.LBB146_95:
	ldbu r4, r23+%lo(fio_reading)
	mul r3, r1, r8
	addi r1, r1, -1
	bne r4, r11, .LBB146_105
.LBB146_96:
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
	jal r0, .LBB146_3
.LBB146_97:
	stw r14+%lo(fio_pos), r4
	jal r0, .LBB146_3
.LBB146_98:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	sub r1, r0, r11
	xor r3, r11, r1
	sub r4, r0, r5
	and r3, r3, r4
	xor r1, r1, r3
	lui r3, %hi(fio_scale)
	stw r3+%lo(fio_scale), r1
	jal r0, .LBB146_3
.LBB146_99:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	ldbu r3, r23+%lo(fio_reading)
	addi r1, r0, 1
	bne r3, r1, .LBB146_106
.LBB146_100:
	lui r1, %hi(fio_rpos)
	ldw r3, r1+%lo(fio_rpos)
	add r3, r3, r11
	stw r1+%lo(fio_rpos), r3
	jal r0, .LBB146_3
.LBB146_101:
	andi r1, r7, 222
	addi r5, r0, 68
	andi r7, r7, 255
	beq r1, r5, .LBB146_111
.LBB146_102:
	addi r1, r7, -65
	addi r5, r0, 57
	bgtu r1, r5, .LBB146_104
.LBB146_103:
	slli r1, r1, 2
	lui r5, %hi(.LJTI146_3)
	addi r5, r5, %lo(.LJTI146_3)
	add r1, r5, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB146_104:
	addi r1, r6, 1
	jal r0, .LBB146_2
.LBB146_105:
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
	jal r0, .LBB146_3
.LBB146_106:
	blt r11, r1, .LBB146_3
.LBB146_107:
	lui r1, %hi(fio_col)
	ldw r3, r1+%lo(fio_col)
	add r3, r3, r11
	stw r1+%lo(fio_col), r3
	jal r0, .LBB146_3
.LBB146_108:
	add r7, r11, r0
	jal r0, .LBB146_93
.LBB146_109:
	add r1, r0, r0
.LBB146_110:
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
.LBB146_111:
	addi r1, r6, 1
	stw r14+%lo(fio_pos), r1
	addi r5, r0, 69
	add r18, r7, r0
	bne r4, r5, .LBB146_115
.LBB146_112:
	add r16, r10, r0
	add r1, r3, r1
	ldbu r3, r1+0
	jal r31, toupper
	beq r1, r12, .LBB146_114
.LBB146_113:
	ldw r1, r15+%lo(fio_fmt)
	ldw r3, r14+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB146_116
.LBB146_114:
	ldw r1, r15+%lo(fio_fmt)
	ldw r3, r14+%lo(fio_pos)
	add r1, r1, r3
	ldbu r3, r1+0
	jal r31, toupper
	add r12, r1, r0
	ldw r1, r14+%lo(fio_pos)
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB146_117
.LBB146_115:
	add r12, r0, r0
	jal r0, .LBB146_118
.LBB146_116:
	add r12, r0, r0
.LBB146_117:
	add r10, r16, r0
	add r7, r18, r0
.LBB146_118:
	ldw r15, r15+%lo(fio_fmt)
	ldw r1, r14+%lo(fio_pos)
	add r1, r15, r1
	ldb r1, r1+0
	addi r1, r1, -58
	bgeu r1, r10, .LBB146_120
.LBB146_119:
	add r1, r0, r0
	jal r0, .LBB146_121
.LBB146_120:
	add r16, r10, r0
	jal r31, fio_number
	add r7, r18, r0
	add r10, r16, r0
.LBB146_121:
	lui r3, %hi(fio_w)
	stw r3+%lo(fio_w), r1
	lui r16, %hi(fio_d)
	stw r16+%lo(fio_d), r0
	ldw r1, r14+%lo(fio_pos)
	add r3, r15, r1
	ldbu r3, r3+0
	addi r4, r0, 46
	bne r3, r4, .LBB146_123
.LBB146_122:
	addi r1, r1, 1
	stw r14+%lo(fio_pos), r1
	add r17, r10, r0
	jal r31, fio_number
	add r7, r18, r0
	add r10, r17, r0
	stw r16+%lo(fio_d), r1
.LBB146_123:
	lui r16, %hi(fio_e)
	stw r16+%lo(fio_e), r0
	ldw r1, r14+%lo(fio_pos)
	add r3, r15, r1
	ldbu r3, r3+0
	ori  r3, r3, 32
	addi r4, r0, 101
	bne r3, r4, .LBB146_127
.LBB146_124:
	addi r3, r1, 1
	stw r14+%lo(fio_pos), r3
	add r3, r15, r3
	ldb r3, r3+0
	addi r3, r3, -58
	bgeu r3, r10, .LBB146_126
.LBB146_125:
	stw r14+%lo(fio_pos), r1
	jal r0, .LBB146_127
.LBB146_126:
	jal r31, fio_number
	add r7, r18, r0
	stw r16+%lo(fio_e), r1
.LBB146_127:
	lui r1, %hi(fio_rep)
	stw r1+%lo(fio_rep), r11
	beq r12, r0, .LBB146_129
.LBB146_128:
	addi r1, r12, 256
	jal r0, .LBB146_130
.LBB146_129:
	add r3, r7, r0
	jal r31, toupper
.LBB146_130:
	lui r3, %hi(fio_desc)
	stw r3+%lo(fio_desc), r1
	addi r3, r0, 1
	stb r13+%lo(fio_since_data), r3
	jal r0, .LBB146_110
.LBB146_131:
	jal r31, fio_flush_line
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.120)
	addi r4, r4, %lo(.L.str.120)
	jal r0, .LBB146_133
.LBB146_132:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
.LBB146_133:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end146:
	.size	fio_next_desc, .Lfunc_end146-fio_next_desc
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI146_0,@object
.LJTI146_0:
	.word	.LBB146_31
	.word	.LBB146_59
	.word	.LBB146_54
	.word	.LBB146_59
	.word	.LBB146_59
	.word	.LBB146_59
	.word	.LBB146_51
	.word	.LBB146_59
	.word	.LBB146_57
	.size	.LJTI146_0, 36
	.type	.LJTI146_1,@object
.LJTI146_1:
	.word	.LBB146_71
	.word	.LBB146_101
	.word	.LBB146_101
	.word	.LBB146_101
	.word	.LBB146_101
	.word	.LBB146_101
	.word	.LBB146_101
	.word	.LBB146_101
	.word	.LBB146_98
	.word	.LBB146_101
	.word	.LBB146_101
	.word	.LBB146_101
	.word	.LBB146_89
	.word	.LBB146_101
	.word	.LBB146_101
	.word	.LBB146_101
	.word	.LBB146_99
	.size	.LJTI146_1, 68
	.type	.LJTI146_2,@object
.LJTI146_2:
	.word	.LBB146_92
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_91
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_92
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_93
	.word	.LBB146_91
	.size	.LJTI146_2, 156
	.type	.LJTI146_3,@object
.LJTI146_3:
	.word	.LBB146_111
	.word	.LBB146_111
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_111
	.word	.LBB146_111
	.word	.LBB146_104
	.word	.LBB146_111
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_111
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_111
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_111
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_111
	.word	.LBB146_111
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_111
	.word	.LBB146_111
	.word	.LBB146_104
	.word	.LBB146_111
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_111
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_111
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_104
	.word	.LBB146_111
	.size	.LJTI146_3, 232
	.type	.LJTI146_4,@object
.LJTI146_4:
	.word	.LBB146_19
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_27
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_19
	.word	.LBB146_50
	.word	.LBB146_50
	.word	.LBB146_27
	.size	.LJTI146_4, 144
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
	beq r3, r0, .LBB147_9
.LBB147_1:
	lui r11, %hi(fio_irec)
	ldw r1, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r1, r4, .LBB147_21
.LBB147_2:
	lui r14, %hi(fio_ilen)
	ldw r6, r14+%lo(fio_ilen)
	bgt r5, r6, .LBB147_22
.LBB147_3:
	mul r1, r6, r1
	add r3, r3, r1
	lui r4, %hi(fio_line)
	addi r4, r4, %lo(fio_line)
	jal r31, memcpy
	ldw r3, r13+%lo(fio_len)
	ldw r1, r14+%lo(fio_ilen)
	ble r1, r3, .LBB147_6
.LBB147_4:
	ldw r4, r12+%lo(fio_ibuf)
	ldw r5, r11+%lo(fio_irec)
	mul r5, r5, r1
	sub r1, r1, r3
	add r3, r3, r5
	add r3, r4, r3
	addi r4, r0, 32
.LBB147_5:
	stb r3+0, r4
	addi r1, r1, -1
	addi r3, r3, 1
	bne r1, r0, .LBB147_5
.LBB147_6:
	ldw r1, r11+%lo(fio_irec)
	addi r1, r1, 1
	stw r11+%lo(fio_irec), r1
.LBB147_7:
	stw r13+%lo(fio_len), r0
.LBB147_8:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB147_9:
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
	bgtu r3, r14, .LBB147_15
.LBB147_10:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB147_15
.LBB147_11:
	lui r3, %hi(fio_urecl)
	addi r3, r3, %lo(fio_urecl)
	add r1, r1, r3
	ldw r12, r1+0
	bgt r5, r12, .LBB147_24
.LBB147_12:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	ldw r1, r13+%lo(fio_len)
	ble r12, r1, .LBB147_7
.LBB147_13:
	sub r14, r12, r1
	addi r12, r0, 32
.LBB147_14:
	add r3, r12, r0
	add r4, r11, r0
	jal r31, fputc
	addi r14, r14, -1
	bne r14, r0, .LBB147_14
	jal r0, .LBB147_7
.LBB147_15:
	lui r3, %hi(fio_line)
	addi r3, r3, %lo(fio_line)
	addi r4, r0, 1
	add r6, r11, r0
	jal r31, fwrite
	lui r1, %hi(fio_open_rec)
	ldw r3, r1+%lo(fio_open_rec)
	beq r3, r0, .LBB147_17
.LBB147_16:
	stw r1+%lo(fio_open_rec), r0
	jal r0, .LBB147_18
.LBB147_17:
	addi r3, r0, 10
	add r4, r11, r0
	jal r31, fputc
.LBB147_18:
	stw r13+%lo(fio_len), r0
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	bgtu r3, r14, .LBB147_8
.LBB147_19:
	addi r1, r1, -7
	addi r3, r0, -3
	bgtu r1, r3, .LBB147_8
.LBB147_20:
	add r3, r11, r0
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	lui r4, %hi(fio_uend)
	addi r4, r4, %lo(fio_uend)
	add r3, r3, r4
	stw r3+0, r1
	jal r0, .LBB147_8
.LBB147_21:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.121)
	addi r4, r4, %lo(.L.str.121)
	jal r0, .LBB147_23
.LBB147_22:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.122)
	addi r4, r4, %lo(.L.str.122)
.LBB147_23:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB147_24:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.123)
	addi r4, r4, %lo(.L.str.123)
	add r6, r12, r0
	jal r0, .LBB147_23
.Lfunc_end147:
	.size	fio_flush_line, .Lfunc_end147-fio_flush_line
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
	bne r3, r0, .LBB148_4
.LBB148_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB148_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB148_2
.LBB148_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB148_4:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_resolve
	add r13, r1, r0
	lui r1, %hi(stdout)
	ldw r1, r1+%lo(stdout)
	beq r13, r1, .LBB148_8
.LBB148_5:
	lui r1, %hi(stderr)
	ldw r1, r1+%lo(stderr)
	beq r13, r1, .LBB148_8
.LBB148_6:
	add r3, r13, r0
	jal r31, fflush
	addi r1, r11, -1
	addi r3, r0, 98
	bgtu r1, r3, .LBB148_8
.LBB148_7:
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
.LBB148_8:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end148:
	.size	f77_endfile, .Lfunc_end148-f77_endfile
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
.Lfunc_end149:
	.size	f77_pause, .Lfunc_end149-f77_pause
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
	beq r3, r1, .LBB150_19
.LBB150_1:
	add r11, r6, r0
	add r12, r5, r0
	add r13, r4, r0
	stb r15+%lo(fio_busy), r1
	lui r3, %hi(fio_uend_init)
	ldbu r4, r3+%lo(fio_uend_init)
	bne r4, r0, .LBB150_5
.LBB150_2:
	addi r4, r0, -400
	lui r5, %hi(fio_uend+400)
	addi r5, r5, %lo(fio_uend+400)
	addi r6, r0, -1
.LBB150_3:
	add r7, r4, r5
	stw r7+0, r6
	addi r4, r4, 4
	bne r4, r0, .LBB150_3
.LBB150_4:
	stb r3+%lo(fio_uend_init), r1
.LBB150_5:
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
	bgtu r3, r4, .LBB150_11
.LBB150_6:
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
	bltu r4, r5, .LBB150_10
.LBB150_7:
	lui r4, %hi(fio_udirect)
	addi r4, r4, %lo(fio_udirect)
	add r4, r16, r4
	ldw r4, r4+0
	beq r4, r0, .LBB150_10
.LBB150_8:
	ble r3, r0, .LBB150_20
.LBB150_9:
	addi r3, r3, -1
	lui r4, %hi(fio_urecl)
	addi r4, r4, %lo(fio_urecl)
	add r4, r16, r4
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB150_10:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r16, r1
	ldw r1, r1+0
	jal r0, .LBB150_16
.LBB150_11:
	lui r4, %hi(fio_rec_next)
	ldw r3, r4+%lo(fio_rec_next)
	stw r4+%lo(fio_rec_next), r0
	addi r4, r14, -100
	addi r5, r0, -99
	bltu r4, r5, .LBB150_15
.LBB150_12:
	slli r4, r14, 2
	lui r5, %hi(fio_udirect)
	addi r5, r5, %lo(fio_udirect)
	add r5, r4, r5
	ldw r5, r5+0
	beq r5, r0, .LBB150_15
.LBB150_13:
	ble r3, r0, .LBB150_20
.LBB150_14:
	addi r3, r3, -1
	lui r5, %hi(fio_urecl)
	addi r5, r5, %lo(fio_urecl)
	add r4, r4, r5
	ldw r4, r4+0
	mul r4, r4, r3
	add r3, r1, r0
	add r5, r0, r0
	jal r31, fseek
.LBB150_15:
	add r1, r0, r0
.LBB150_16:
	lui r3, %hi(fio_bz)
	stw r3+%lo(fio_bz), r1
	add r3, r13, r0
	add r4, r12, r0
	add r5, r11, r0
	jal r31, fio_rd_setup
	beq r1, r0, .LBB150_18
.LBB150_17:
	stb r15+%lo(fio_busy), r0
.LBB150_18:
	ldw r16, sp+24
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB150_19:
	lui r1, %hi(stdout)
	ldw r3, r1+%lo(stdout)
	jal r31, fflush
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.92)
	addi r4, r4, %lo(.L.str.92)
	jal r0, .LBB150_21
.LBB150_20:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.94)
	addi r4, r4, %lo(.L.str.94)
.LBB150_21:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end150:
	.size	f77_rd_begin, .Lfunc_end150-f77_rd_begin
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
	bgt r1, r0, .LBB151_2
.LBB151_1:
	lui r1, %hi(fio_bz)
	stw r1+%lo(fio_bz), r0
.LBB151_2:
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
	lui r1, %hi(.L.str.9)
	addi r1, r1, %lo(.L.str.9)
	lui r17, %hi(fio_fmt)
	stw r17+%lo(fio_fmt), r1
	lui r15, %hi(fio_pos)
	stw r15+%lo(fio_pos), r0
	lui r16, %hi(fio_revert)
	addi r19, r0, -1
	stw r16+%lo(fio_revert), r19
	lui r1, %hi(fio_since_data)
	stb r1+%lo(fio_since_data), r0
	beq r3, r0, .LBB151_4
.LBB151_3:
	jal r31, fio_fmt_use
	ble r1, r19, .LBB151_17
.LBB151_4:
	ldw r1, r18+%lo(fio_listed)
	bne r1, r0, .LBB151_10
.LBB151_5:
	ldw r3, r17+%lo(fio_fmt)
	ldw r1, r15+%lo(fio_pos)
	addi r4, r0, 32
.LBB151_6:
	add r5, r3, r1
	ldbu r5, r5+0
	bne r5, r4, .LBB151_8
.LBB151_7:
	addi r1, r1, 1
	stw r15+%lo(fio_pos), r1
	jal r0, .LBB151_6
.LBB151_8:
	addi r3, r0, 40
	bne r5, r3, .LBB151_10
.LBB151_9:
	addi r1, r1, 1
	stw r15+%lo(fio_pos), r1
	stw r16+%lo(fio_revert), r1
.LBB151_10:
	lui r1, %hi(fio_ibuf)
	ldw r1, r1+%lo(fio_ibuf)
	beq r1, r0, .LBB151_14
.LBB151_11:
	jal r31, fio_getrec
	beq r1, r0, .LBB151_16
.LBB151_12:
	stb r13+%lo(fio_reading), r0
	lui r1, %hi(fio_noadv)
	stb r1+%lo(fio_noadv), r0
	andi r1, r11, 3
	bne r1, r0, .LBB151_19
.LBB151_13:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB151_14:
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
	beq r1, r0, .LBB151_11
.LBB151_15:
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
.LBB151_16:
	add r12, r0, r0
	jal r0, .LBB151_19
.LBB151_17:
	stb r13+%lo(fio_reading), r0
	andi r1, r11, 2
	beq r1, r0, .LBB151_20
.LBB151_18:
	addi r12, r0, 2
.LBB151_19:
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
.LBB151_20:
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end151:
	.size	fio_rd_setup, .Lfunc_end151-fio_rd_setup
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
	beq r9, r1, .LBB152_8
.LBB152_1:
	stb r11+%lo(fio_busy), r1
	lui r9, %hi(fio_uend_init)
	ldbu r10, r9+%lo(fio_uend_init)
	bne r10, r0, .LBB152_5
.LBB152_2:
	addi r10, r0, -400
	lui lr, %hi(fio_uend+400)
	addi lr, lr, %lo(fio_uend+400)
	addi r12, r0, -1
.LBB152_3:
	add r13, r10, lr
	stw r13+0, r12
	addi r10, r10, 4
	bne r10, r0, .LBB152_3
.LBB152_4:
	stb r9+%lo(fio_uend_init), r1
.LBB152_5:
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
	beq r1, r0, .LBB152_7
.LBB152_6:
	stb r11+%lo(fio_busy), r0
.LBB152_7:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB152_8:
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
.Lfunc_end152:
	.size	f77_rd_begin_int, .Lfunc_end152-f77_rd_begin_int
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
	beq r1, r0, .LBB153_5
.LBB153_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB153_73
.LBB153_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB153_73
.LBB153_3:
	addi r3, r0, 1
	bne r1, r3, .LBB153_24
.LBB153_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB153_73
.LBB153_5:
	lui r14, %hi(fio_eor)
	ldbu r3, r14+%lo(fio_eor)
	lui r13, %hi(fio_rep)
	add r1, r0, r0
	bne r3, r0, .LBB153_9
.LBB153_6:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB153_8
.LBB153_7:
	ldw r4, r13+%lo(fio_rep)
	bgt r4, r0, .LBB153_9
.LBB153_8:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
.LBB153_9:
	addi r3, r1, -65
	addi r4, r0, 25
	bgtu r3, r4, .LBB153_27
.LBB153_10:
	slli r3, r3, 2
	lui r4, %hi(.LJTI153_1)
	addi r4, r4, %lo(.LJTI153_1)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB153_11:
	lui r3, %hi(fio_w)
	ldw r3, r3+%lo(fio_w)
	addi r4, r0, 1
	blt r3, r4, .LBB153_29
.LBB153_12:
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
	jal r0, .LBB153_16
.LBB153_13:
	addi r18, r16, 1
	addi r20, sp, 32
	add r16, r20, r16
	stb r16+0, r19
	add r16, r18, r0
.LBB153_14:
	add r18, r4, r0
.LBB153_15:
	addi r17, r17, -1
	addi r5, r5, 1
	beq r17, r0, .LBB153_30
.LBB153_16:
	add r19, r8, r0
	bge r5, r6, .LBB153_18
.LBB153_17:
	add r19, r5, r9
	ldb r19, r19+0
.LBB153_18:
	beq r19, r8, .LBB153_20
.LBB153_19:
	bne r19, r12, .LBB153_23
.LBB153_20:
	beq r7, r0, .LBB153_15
.LBB153_21:
	beq r18, r0, .LBB153_15
.LBB153_22:
	add r19, r10, r0
	blt r16, lr, .LBB153_13
	jal r0, .LBB153_15
.LBB153_23:
	bgt r16, r15, .LBB153_14
	jal r0, .LBB153_13
.LBB153_24:
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
	bgtu r4, r5, .LBB153_74
.LBB153_25:
	slli r4, r4, 2
	lui r5, %hi(.LJTI153_0)
	addi r5, r5, %lo(.LJTI153_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB153_26:
	andi r3, r3, 255
	addi r4, r0, 46
	seq r3, r3, r4
	add r1, r3, r1
	ldbu r1, r1+0
	andi r1, r1, 223
	addi r3, r0, 84
	seq r1, r1, r3
	stw r11+0, r1
	jal r0, .LBB153_73
.LBB153_27:
	beq r1, r0, .LBB153_73
.LBB153_28:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.w.d r12, r4
	jal r0, .LBB153_69
.LBB153_29:
	add r16, r0, r0
.LBB153_30:
	addi r5, sp, 32
	add r5, r5, r16
	stb r5+0, r0
	lui r5, %hi(fio_noadv)
	ldbu r5, r5+%lo(fio_noadv)
	bne r5, r4, .LBB153_34
.LBB153_31:
	lui r5, %hi(fio_reading)
	ldbu r5, r5+%lo(fio_reading)
	andi r5, r5, 1
	beq r5, r0, .LBB153_34
.LBB153_32:
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
	ble r3, r5, .LBB153_34
.LBB153_33:
	stb r14+%lo(fio_eor), r4
.LBB153_34:
	lui r4, %hi(fio_rpos)
	ldw r5, r4+%lo(fio_rpos)
	add r3, r5, r3
	stw r4+%lo(fio_rpos), r3
	ldbu r3, sp+32
	beq r3, r0, .LBB153_68
.LBB153_35:
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
	jal r0, .LBB153_38
.LBB153_36:
	sll r3, r12, r16
	add r12, r1, r3
.LBB153_37:
	ldbu r3, r15+0
	addi r15, r15, 1
	beq r3, r0, .LBB153_69
.LBB153_38:
	andi r3, r3, 255
	jal r31, toupper
	beq r1, r17, .LBB153_37
.LBB153_39:
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
	blt r1, r14, .LBB153_36
.LBB153_40:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	ldb r5, r15+-1
	lui r4, %hi(.L.str.126)
	addi r4, r4, %lo(.L.str.126)
.LBB153_41:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB153_42:
	lui r1, %hi(fio_w)
	ldw r1, r1+%lo(fio_w)
	addi r12, r0, 1
	lui r3, %hi(fio_rpos)
	lui r4, %hi(fio_rlen)
	blt r1, r12, .LBB153_57
.LBB153_43:
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
	jal r0, .LBB153_47
.LBB153_44:
	addi r20, r18, 1
	addi r22, sp, 32
	add r18, r22, r18
	stb r18+0, r21
	add r18, r20, r0
.LBB153_45:
	add r20, r15, r0
.LBB153_46:
	addi r19, r19, -1
	addi r5, r5, 1
	beq r19, r0, .LBB153_58
.LBB153_47:
	add r21, r8, r0
	bge r5, r6, .LBB153_49
.LBB153_48:
	add r21, r5, r9
	ldb r21, r21+0
.LBB153_49:
	beq r21, r8, .LBB153_51
.LBB153_50:
	bne r21, r16, .LBB153_54
.LBB153_51:
	beq r7, r0, .LBB153_46
.LBB153_52:
	beq r20, r0, .LBB153_46
.LBB153_53:
	add r21, r10, r0
	blt r18, lr, .LBB153_44
	jal r0, .LBB153_46
.LBB153_54:
	bgt r18, r17, .LBB153_45
	jal r0, .LBB153_44
.LBB153_55:
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
.LBB153_56:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	jal r31, fio_in_int
	add r12, r1, r0
	jal r0, .LBB153_69
.LBB153_57:
	add r18, r0, r0
.LBB153_58:
	addi r5, sp, 32
	add r5, r5, r18
	stb r5+0, r0
	lui r5, %hi(fio_noadv)
	ldbu r5, r5+%lo(fio_noadv)
	bne r5, r12, .LBB153_62
.LBB153_59:
	lui r5, %hi(fio_reading)
	ldbu r5, r5+%lo(fio_reading)
	andi r5, r5, 1
	beq r5, r0, .LBB153_62
.LBB153_60:
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
	ble r1, r4, .LBB153_62
.LBB153_61:
	addi r4, r0, 1
	stb r14+%lo(fio_eor), r4
.LBB153_62:
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
	ble r1, r3, .LBB153_66
.LBB153_63:
	addi r3, r0, 84
	beq r1, r3, .LBB153_69
.LBB153_64:
	addi r3, r0, 116
	beq r1, r3, .LBB153_69
.LBB153_65:
	addi r3, r0, 102
	beq r1, r3, .LBB153_68
	jal r0, .LBB153_75
.LBB153_66:
	beq r1, r0, .LBB153_68
.LBB153_67:
	addi r3, r0, 70
	bne r1, r3, .LBB153_75
.LBB153_68:
	add r12, r0, r0
.LBB153_69:
	stw r11+0, r12
	ldw r1, r13+%lo(fio_rep)
	addi r3, r0, 1
	blt r1, r3, .LBB153_71
.LBB153_70:
	addi r1, r1, -1
	stw r13+%lo(fio_rep), r1
.LBB153_71:
	ldw r1, r13+%lo(fio_rep)
	bne r1, r0, .LBB153_73
.LBB153_72:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB153_73:
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
.LBB153_74:
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	stw r11+0, r1
	jal r0, .LBB153_73
.LBB153_75:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.125)
	addi r4, r4, %lo(.L.str.125)
	addi r5, sp, 32
	jal r0, .LBB153_41
.Lfunc_end153:
	.size	f77_rd_i, .Lfunc_end153-f77_rd_i
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI153_0,@object
.LJTI153_0:
	.word	.LBB153_26
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_26
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_26
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_26
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_74
	.word	.LBB153_26
	.size	.LJTI153_0, 144
	.type	.LJTI153_1,@object
.LJTI153_1:
	.word	.LBB153_55
	.word	.LBB153_11
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_56
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_42
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_11
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_28
	.word	.LBB153_11
	.size	.LJTI153_1, 104
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
	blt r1, r20, .LBB154_2
.LBB154_1:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
	addi r11, r0, 2
	jal r0, .LBB154_5
.LBB154_2:
	lui r14, %hi(fio_l_pending)
	ldw r1, r14+%lo(fio_l_pending)
	ble r1, r0, .LBB154_6
.LBB154_3:
	addi r1, r1, -1
	stw r14+%lo(fio_l_pending), r1
.LBB154_4:
	add r11, r0, r0
.LBB154_5:
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
.LBB154_6:
	lui r15, %hi(fio_rpos)
	lui r17, %hi(fio_rlen)
	lui r16, %hi(fio_rec)
	addi r16, r16, %lo(fio_rec)
	addi r18, r0, 38
	lui r21, %hi(.LJTI154_0)
	addi r21, r21, %lo(.LJTI154_0)
	lui r12, %hi(fio_l_after)
	addi r11, r0, 2
	jal r0, .LBB154_8
.LBB154_7:
	jal r31, fio_getrec
	bne r1, r0, .LBB154_44
.LBB154_8:
	ldw r3, r15+%lo(fio_rpos)
	ldw r1, r17+%lo(fio_rlen)
	bge r3, r1, .LBB154_7
.LBB154_9:
	add r4, r3, r16
	ldbu r19, r4+0
	addi r4, r19, -9
	bgtu r4, r18, .LBB154_25
.LBB154_10:
	slli r4, r4, 2
	add r4, r21, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB154_11:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	jal r0, .LBB154_8
.LBB154_12:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	ldbu r1, r12+%lo(fio_l_after)
	bne r1, r20, .LBB154_5
.LBB154_13:
	stb r12+%lo(fio_l_after), r0
	jal r0, .LBB154_8
.LBB154_14:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	addi r13, r0, 126
	lui r11, %hi(fio_ltok)
	addi r11, r11, %lo(fio_ltok)
	add r14, r0, r0
	jal r0, .LBB154_16
.LBB154_15:
	jal r31, fio_getrec
	bne r1, r0, .LBB154_44
.LBB154_16:
	ldw r1, r15+%lo(fio_rpos)
	ldw r4, r17+%lo(fio_rlen)
	bge r1, r4, .LBB154_15
.LBB154_17:
	addi r5, r1, 1
	stw r15+%lo(fio_rpos), r5
	add r3, r1, r16
	ldbu r3, r3+0
	bne r3, r19, .LBB154_21
.LBB154_18:
	bge r5, r4, .LBB154_23
.LBB154_19:
	add r4, r5, r16
	ldbu r4, r4+0
	bne r4, r19, .LBB154_23
.LBB154_20:
	addi r1, r1, 2
	stw r15+%lo(fio_rpos), r1
.LBB154_21:
	bgtu r14, r13, .LBB154_16
.LBB154_22:
	addi r1, r14, 1
	add r4, r14, r11
	stb r4+0, r3
	add r14, r1, r0
	jal r0, .LBB154_16
.LBB154_23:
	add r1, r14, r11
	stb r1+0, r0
	addi r1, r0, 1
	stb r12+%lo(fio_l_after), r1
	jal r0, .LBB154_4
.LBB154_24:
	addi r1, r3, 1
	stw r15+%lo(fio_rpos), r1
	add r11, r20, r0
	jal r0, .LBB154_5
.LBB154_25:
	ldw r3, r15+%lo(fio_rpos)
	bge r3, r1, .LBB154_32
.LBB154_26:
	addi r5, r0, 126
	lui r6, %hi(fio_ltok)
	addi r6, r6, %lo(fio_ltok)
	lui r7, %hi(.LJTI154_1)
	addi r7, r7, %lo(.LJTI154_1)
	add r4, r0, r0
	jal r0, .LBB154_28
.LBB154_27:
	addi r3, r3, 1
	stw r15+%lo(fio_rpos), r3
	beq r1, r3, .LBB154_33
.LBB154_28:
	add r8, r3, r16
	ldbu r8, r8+0
	addi r9, r8, -9
	bgtu r9, r18, .LBB154_30
.LBB154_29:
	slli r9, r9, 2
	add r9, r7, r9
	ldw r9, r9+0
	jalr r0, r9, 0
.LBB154_30:
	bgt r4, r5, .LBB154_27
.LBB154_31:
	addi r9, r4, 1
	add r4, r4, r6
	stb r4+0, r8
	add r4, r9, r0
	jal r0, .LBB154_27
.LBB154_32:
	add r4, r0, r0
.LBB154_33:
	lui r1, %hi(fio_ltok)
	addi r1, r1, %lo(fio_ltok)
	add r3, r4, r1
	stb r3+0, r0
	addi r3, r0, 1
	stb r12+%lo(fio_l_after), r3
	addi r4, r0, -11
	add r15, r0, r0
.LBB154_34:
	add r5, r15, r1
	ldb r5, r5+0
	addi r6, r5, -58
	addi r15, r15, 1
	bgtu r6, r4, .LBB154_34
.LBB154_35:
	beq r15, r3, .LBB154_4
.LBB154_36:
	addi r1, r0, 42
	bne r5, r1, .LBB154_4
.LBB154_37:
	lui r12, %hi(fio_ltok)
	addi r12, r12, %lo(fio_ltok)
	addi r5, r0, 10
	add r3, r12, r0
	add r4, r0, r0
	jal r31, strtol
	beq r1, r0, .LBB154_46
.LBB154_38:
	add r12, r15, r12
	ldbu r15, r12+0
	beq r15, r0, .LBB154_42
.LBB154_39:
	addi r3, r0, 2
	blt r1, r3, .LBB154_41
.LBB154_40:
	addi r1, r1, -1
	stw r14+%lo(fio_l_pending), r1
.LBB154_41:
	add r3, r12, r0
	jal r31, strlen
	addi r5, r1, 1
	lui r3, %hi(fio_ltok)
	addi r3, r3, %lo(fio_ltok)
	add r4, r12, r0
	jal r31, memmove
	jal r0, .LBB154_43
.LBB154_42:
	addi r1, r1, -1
	stw r13+%lo(fio_l_nulls), r1
.LBB154_43:
	bne r15, r0, .LBB154_4
	jal r0, .LBB154_5
.LBB154_44:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
.LBB154_45:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB154_46:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.124)
	addi r4, r4, %lo(.L.str.124)
	jal r0, .LBB154_45
.Lfunc_end154:
	.size	fio_list_tok, .Lfunc_end154-fio_list_tok
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI154_0,@object
.LJTI154_0:
	.word	.LBB154_11
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_11
	.word	.LBB154_25
	.word	.LBB154_14
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_14
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_12
	.word	.LBB154_25
	.word	.LBB154_25
	.word	.LBB154_24
	.size	.LJTI154_0, 156
	.type	.LJTI154_1,@object
.LJTI154_1:
	.word	.LBB154_33
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_33
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_33
	.word	.LBB154_30
	.word	.LBB154_30
	.word	.LBB154_33
	.size	.LJTI154_1, 156
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
	blt r3, r4, .LBB155_13
.LBB155_1:
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
	jal r0, .LBB155_5
.LBB155_2:
	addi r16, r14, 1
	addi r18, sp, 32
	add r14, r18, r14
	stb r14+0, r17
	add r14, r16, r0
.LBB155_3:
	add r16, r4, r0
.LBB155_4:
	addi r15, r15, -1
	addi r6, r6, 1
	beq r15, r0, .LBB155_14
.LBB155_5:
	add r17, r9, r0
	bge r6, r7, .LBB155_7
.LBB155_6:
	add r17, r6, r10
	ldb r17, r17+0
.LBB155_7:
	beq r17, r9, .LBB155_9
.LBB155_8:
	bne r17, r12, .LBB155_12
.LBB155_9:
	beq r8, r0, .LBB155_4
.LBB155_10:
	beq r16, r0, .LBB155_4
.LBB155_11:
	add r17, lr, r0
	blt r14, r11, .LBB155_2
	jal r0, .LBB155_4
.LBB155_12:
	ble r14, r13, .LBB155_2
	jal r0, .LBB155_3
.LBB155_13:
	add r14, r0, r0
.LBB155_14:
	addi r6, sp, 32
	add r6, r6, r14
	stb r6+0, r0
	lui r6, %hi(fio_noadv)
	ldbu r6, r6+%lo(fio_noadv)
	bne r6, r4, .LBB155_18
.LBB155_15:
	lui r6, %hi(fio_reading)
	ldbu r6, r6+%lo(fio_reading)
	andi r6, r6, 1
	beq r6, r0, .LBB155_18
.LBB155_16:
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
	ble r3, r5, .LBB155_18
.LBB155_17:
	lui r5, %hi(fio_eor)
	stb r5+%lo(fio_eor), r4
.LBB155_18:
	ldw r4, r1+%lo(fio_rpos)
	add r3, r4, r3
	stw r1+%lo(fio_rpos), r3
	ldbu r1, sp+32
	beq r1, r0, .LBB155_20
.LBB155_19:
	addi r3, sp, 32
	addi r5, r0, 10
	add r4, r0, r0
	jal r31, strtol
	jal r0, .LBB155_21
.LBB155_20:
	add r1, r0, r0
.LBB155_21:
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
.Lfunc_end155:
	.size	fio_in_int, .Lfunc_end155-fio_in_int
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
	beq r1, r0, .LBB156_5
.LBB156_1:
	lui r13, %hi(fio_ldone)
	ldbu r1, r13+%lo(fio_ldone)
	bne r1, r0, .LBB156_36
.LBB156_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB156_36
.LBB156_3:
	addi r14, r0, 1
	bne r1, r14, .LBB156_17
.LBB156_4:
	stb r13+%lo(fio_ldone), r14
	jal r0, .LBB156_36
.LBB156_5:
	lui r13, %hi(fio_eor)
	ldbu r3, r13+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB156_8
.LBB156_6:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB156_22
.LBB156_7:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB156_22
.LBB156_8:
	beq r1, r0, .LBB156_36
.LBB156_9:
	addi r3, r0, 65
	bne r1, r3, .LBB156_37
.LBB156_10:
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
	bge r1, r12, .LBB156_23
.LBB156_11:
	blt r12, r5, .LBB156_28
.LBB156_12:
	ldw r5, r3+%lo(fio_rpos)
	lui r6, %hi(fio_rlen)
	ldw r6, r6+%lo(fio_rlen)
	addi r7, r0, 32
	lui r8, %hi(fio_rec)
	addi r8, r8, %lo(fio_rec)
	add r9, r0, r0
	jal r0, .LBB156_14
.LBB156_13:
	add lr, r11, r9
	stb lr+0, r10
	addi r9, r9, 1
	beq r12, r9, .LBB156_28
.LBB156_14:
	add r10, r7, r0
	bge r9, r1, .LBB156_13
.LBB156_15:
	add lr, r5, r9
	add r10, r7, r0
	bge lr, r6, .LBB156_13
.LBB156_16:
	add r10, lr, r8
	ldbu r10, r10+0
	jal r0, .LBB156_13
.LBB156_17:
	lui r13, %hi(fio_ltok)
	addi r13, r13, %lo(fio_ltok)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB156_36
.LBB156_18:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB156_20
.LBB156_19:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB156_36
.LBB156_20:
	add r5, r3, r0
	bge r4, r1, .LBB156_19
.LBB156_21:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB156_19
.LBB156_22:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	bne r1, r0, .LBB156_9
	jal r0, .LBB156_36
.LBB156_23:
	blt r12, r5, .LBB156_28
.LBB156_24:
	ldw r6, r3+%lo(fio_rpos)
	lui r5, %hi(fio_rlen)
	ldw r5, r5+%lo(fio_rlen)
	sub r9, r0, r12
	add r6, r1, r6
	addi r7, r0, 32
	lui r8, %hi(fio_rec)
	addi r8, r8, %lo(fio_rec)
	jal r0, .LBB156_26
.LBB156_25:
	stb r11+0, r10
	addi r10, r9, 1
	sltu lr, r10, r9
	addi r11, r11, 1
	add r9, r10, r0
	bne lr, r0, .LBB156_28
.LBB156_26:
	add lr, r6, r9
	add r10, r7, r0
	bge lr, r5, .LBB156_25
.LBB156_27:
	add r10, lr, r8
	ldbu r10, r10+0
	jal r0, .LBB156_25
.LBB156_28:
	lui r5, %hi(fio_noadv)
	ldbu r6, r5+%lo(fio_noadv)
	addi r5, r0, 1
	bne r6, r5, .LBB156_32
.LBB156_29:
	lui r6, %hi(fio_reading)
	ldbu r6, r6+%lo(fio_reading)
	andi r6, r6, 1
	beq r6, r0, .LBB156_32
.LBB156_30:
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
	ble r1, r4, .LBB156_32
.LBB156_31:
	stb r13+%lo(fio_eor), r5
.LBB156_32:
	ldw r4, r3+%lo(fio_rpos)
	add r1, r4, r1
	stw r3+%lo(fio_rpos), r1
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	blt r3, r5, .LBB156_34
.LBB156_33:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB156_34:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB156_36
.LBB156_35:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB156_36:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.LBB156_37:
	lui r3, %hi(stderr)
	ldw r3, r3+%lo(stderr)
	lui r4, %hi(.L.str.34)
	addi r4, r4, %lo(.L.str.34)
	add r5, r1, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end156:
	.size	f77_rd_s, .Lfunc_end156-f77_rd_s
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_in_real
	.type	.LCPI157_0,@object
.LCPI157_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI157_0, 8
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
	blt r3, r5, .LBB157_13
.LBB157_1:
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
	jal r0, .LBB157_5
.LBB157_2:
	addi r19, r17, 1
	stw sp+16, r1
	add r1, sp, r0
	addi r1, r1, 2047
	addi r21, r1, 37
	ldw r1, sp+16
	add r17, r21, r17
	stb r17+0, r20
	add r17, r19, r0
.LBB157_3:
	add r19, r14, r0
.LBB157_4:
	addi r18, r18, -1
	addi r8, r8, 1
	beq r18, r0, .LBB157_14
.LBB157_5:
	add r20, lr, r0
	bge r8, r9, .LBB157_7
.LBB157_6:
	add r20, r8, r11
	ldb r20, r20+0
.LBB157_7:
	beq r20, lr, .LBB157_9
.LBB157_8:
	bne r20, r15, .LBB157_12
.LBB157_9:
	beq r10, r0, .LBB157_4
.LBB157_10:
	beq r19, r0, .LBB157_4
.LBB157_11:
	add r20, r12, r0
	blt r17, r13, .LBB157_2
	jal r0, .LBB157_4
.LBB157_12:
	bgt r17, r16, .LBB157_3
	jal r0, .LBB157_2
.LBB157_13:
	add r17, r0, r0
.LBB157_14:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r8, r9, 37
	add r8, r8, r17
	stb r8+0, r0
	lui r8, %hi(fio_noadv)
	ldbu r8, r8+%lo(fio_noadv)
	bne r8, r5, .LBB157_18
.LBB157_15:
	lui r8, %hi(fio_reading)
	ldbu r8, r8+%lo(fio_reading)
	andi r8, r8, 1
	beq r8, r0, .LBB157_18
.LBB157_16:
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
	ble r3, r7, .LBB157_18
.LBB157_17:
	lui r7, %hi(fio_eor)
	addi r8, r0, 1
	stb r7+%lo(fio_eor), r8
.LBB157_18:
	ldw r7, r6+%lo(fio_rpos)
	add r3, r7, r3
	stw r6+%lo(fio_rpos), r3
	add r6, sp, r0
	addi r6, r6, 2047
	ldbu r3, r6+37
	lui r8, %hi(.LCPI157_0)
	addi r8, r8, %lo(.LCPI157_0)
	ldw r7, r8+4
	ldw r6, r8+0
	beq r3, r0, .LBB157_71
.LBB157_19:
	addi lr, r0, 43
	beq r3, lr, .LBB157_22
.LBB157_20:
	addi r8, r0, 45
	bne r3, r8, .LBB157_23
.LBB157_21:
	addi r10, r0, 1
	add r8, r0, r0
	jal r0, .LBB157_25
.LBB157_22:
	add r10, r5, r0
	jal r0, .LBB157_24
.LBB157_23:
	add r10, r0, r0
.LBB157_24:
	add r8, r5, r0
.LBB157_25:
	add r9, sp, r0
	addi r9, r9, 2047
	addi r3, r9, 37
	or  r3, r3, r10
	ldbu r19, r3+0
	beq r19, r0, .LBB157_46
.LBB157_26:
	addi r13, r0, 1
	addi r14, r0, 46
	addi r11, r0, 0
	addi r15, r0, -10
	addi r16, r0, 70
	lui r17, %hi(.LJTI157_0)
	addi r17, r17, %lo(.LJTI157_0)
	add r9, r13, r0
	add r12, r0, r0
	add r18, r0, r0
	add r3, r0, r0
.LBB157_27:
	andi r5, r19, 255
	bne r5, r14, .LBB157_29
.LBB157_28:
	add r18, r13, r0
	jal r0, .LBB157_38
.LBB157_29:
	slli r20, r19, 24
	srai r20, r20, 24
	addi r20, r20, -58
	bgeu r20, r15, .LBB157_35
.LBB157_30:
	addi r5, r5, -43
	bgtu r5, r16, .LBB157_44
.LBB157_31:
	slli r5, r5, 2
	add r5, r17, r5
	ldw r20, r5+0
	add r5, r0, r0
	add r19, r13, r0
	jalr r0, r20, 0
.LBB157_32:
	addi r5, r10, 1
	add r20, sp, r0
	addi r20, r20, 2047
	addi r19, r20, 37
	add r19, r19, r5
	ldbu r19, r19+0
	beq r19, lr, .LBB157_39
.LBB157_33:
	addi r20, r0, 45
	bne r19, r20, .LBB157_41
.LBB157_34:
	addi r9, r0, -1
	add r10, r5, r0
	jal r0, .LBB157_41
.LBB157_35:
	addi r5, r0, 1022
	bgt r3, r5, .LBB157_37
.LBB157_36:
	addi r5, r3, 1
	addi r20, sp, 1060
	add r3, r20, r3
	stb r3+0, r19
	add r3, r5, r0
.LBB157_37:
	sne r5, r18, r11
	add r12, r12, r5
.LBB157_38:
	add r5, r13, r0
	add r19, r13, r0
	bne r19, r0, .LBB157_43
	jal r0, .LBB157_45
.LBB157_39:
	add r10, r5, r0
	jal r0, .LBB157_41
.LBB157_40:
	addi r9, r0, -1
.LBB157_41:
	add r5, r0, r0
	add r19, r13, r0
.LBB157_42:
	beq r19, r0, .LBB157_45
.LBB157_43:
	addi r10, r10, 1
	add r20, sp, r0
	addi r20, r20, 2047
	addi r19, r20, 37
	add r19, r19, r10
	ldbu r19, r19+0
	sne r20, r19, r11
	and r20, r20, r5
	bne r20, r0, .LBB157_27
	jal r0, .LBB157_45
.LBB157_44:
	add r5, r13, r0
	add r19, r0, r0
	bne r19, r0, .LBB157_43
.LBB157_45:
	seq lr, r18, r11
	xor r4, r4, r12
	sub lr, r0, lr
	and r4, r4, lr
	xor r4, r12, r4
	beq r5, r0, .LBB157_47
	jal r0, .LBB157_51
.LBB157_46:
	add r3, r0, r0
	add r9, r5, r0
	bne r5, r0, .LBB157_51
.LBB157_47:
	add r11, sp, r0
	addi r11, r11, 2047
	addi lr, r11, 37
	add lr, lr, r10
	ldbu r12, lr+0
	beq r12, r0, .LBB157_51
.LBB157_48:
	add r11, sp, r0
	addi r11, r11, 2047
	addi lr, r11, 37
	add r10, r10, lr
	addi r10, r10, 1
	addi lr, r0, 9
	addi r11, r0, 10
	add r13, r0, r0
.LBB157_49:
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
	bne r12, r0, .LBB157_49
.LBB157_50:
	mul r10, r13, r9
	jal r0, .LBB157_52
.LBB157_51:
	add r10, r0, r0
.LBB157_52:
	addi r9, sp, 1060
	add r9, r9, r3
	stb r9+0, r0
	beq r3, r0, .LBB157_71
.LBB157_53:
	sub r9, r3, r4
	lui r6, %hi(fio_scale)
	ldw r6, r6+%lo(fio_scale)
	sub r5, r0, r5
	and r6, r6, r5
	addi lr, sp, 20
	add r5, r0, r0
	bne r8, r0, .LBB157_55
.LBB157_54:
	addi lr, r1, 1
	addi r1, r0, 45
	stb sp+20, r1
	addi r5, r0, 1
.LBB157_55:
	sub r6, r10, r6
	addi r7, r0, 1
	blt r9, r7, .LBB157_60
.LBB157_56:
	blt r3, r7, .LBB157_66
.LBB157_57:
	xor r1, r9, r3
	slt r4, r9, r3
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r3, r1
	addi r4, sp, 20
	or  r7, r4, r5
	add r4, r0, r0
.LBB157_58:
	addi r8, sp, 1060
	add r8, r8, r4
	ldbu r8, r8+0
	add r9, r7, r4
	stb r9+0, r8
	addi r4, r4, 1
	bne r1, r4, .LBB157_58
.LBB157_59:
	add r5, r5, r4
	jal r0, .LBB157_67
.LBB157_60:
	addi r1, sp, 20
	or  r1, r1, r5
	addi r8, r0, 48
	stb r1+0, r8
	addi r1, r5, 2
	addi r5, r0, 46
	stb lr+1, r5
	addi r5, r0, -1
	bgt r9, r5, .LBB157_63
.LBB157_61:
	sub r4, r4, r3
.LBB157_62:
	add r5, r1, r0
	addi r1, r1, 1
	addi r9, sp, 20
	add r5, r9, r5
	stb r5+0, r8
	addi r4, r4, -1
	bne r4, r0, .LBB157_62
.LBB157_63:
	blt r3, r7, .LBB157_70
.LBB157_64:
	addi r4, sp, 1060
.LBB157_65:
	add r5, r1, r0
	ldbu r7, r4+0
	addi r1, r1, 1
	addi r8, sp, 20
	add r5, r8, r5
	stb r5+0, r7
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r0, .LBB157_65
	jal r0, .LBB157_70
.LBB157_66:
	add r4, r0, r0
.LBB157_67:
	addi r1, sp, 20
	add r1, r1, r5
	addi r7, r0, 46
	stb r1+0, r7
	addi r1, r5, 1
	ble r3, r4, .LBB157_70
.LBB157_68:
	sub r3, r3, r4
	addi r5, sp, 1060
	add r4, r5, r4
.LBB157_69:
	ldbu r5, r4+0
	addi r7, sp, 20
	add r7, r7, r1
	stb r7+0, r5
	addi r1, r1, 1
	addi r3, r3, -1
	addi r4, r4, 1
	bne r3, r0, .LBB157_69
.LBB157_70:
	addi r11, sp, 20
	add r3, r11, r1
	addi r4, r0, 1040
	sub r4, r4, r1
	lui r5, %hi(.L.str.127)
	addi r5, r5, %lo(.L.str.127)
	jal r31, snprintf
	add r3, r11, r0
	add r4, r0, r0
	jal r31, strtod
	add r6, r1, r0
	add r7, r2, r0
.LBB157_71:
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
.Lfunc_end157:
	.size	fio_in_real, .Lfunc_end157-fio_in_real
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI157_0,@object
.LJTI157_0:
	.word	.LBB157_42
	.word	.LBB157_44
	.word	.LBB157_40
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_32
	.word	.LBB157_32
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_32
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_32
	.word	.LBB157_32
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_44
	.word	.LBB157_32
	.size	.LJTI157_0, 284
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
	bne r1, r0, .LBB158_7
.LBB158_1:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB158_4
.LBB158_2:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB158_6
.LBB158_3:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB158_6
.LBB158_4:
	addi r3, r0, 65
	bne r1, r3, .LBB158_7
.LBB158_5:
	add r3, r11, r0
	add r4, r12, r0
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_rd_s
.LBB158_6:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 65
	beq r1, r3, .LBB158_5
.LBB158_7:
	addi r13, r0, 2
	bne r12, r13, .LBB158_9
.LBB158_8:
	ldh r1, r11+0
	jal r0, .LBB158_10
.LBB158_9:
	ldbu r1, r11+0
.LBB158_10:
	stw sp+16, r1
	addi r3, sp, 16
	jal r31, f77_rd_i
	ldw r1, sp+16
	bne r12, r13, .LBB158_12
.LBB158_11:
	sth r11+0, r1
	jal r0, .LBB158_13
.LBB158_12:
	stb r11+0, r1
.LBB158_13:
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end158:
	.size	f77_rd_in, .Lfunc_end158-f77_rd_in
                                        # -- End function
	.globl	f77_wr_in                       # -- Begin function f77_wr_in
	.p2align	2
	.type	f77_wr_in,@function
f77_wr_in:                              # @f77_wr_in
# %bb.0:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB159_20
.LBB159_1:
	lui r1, %hi(fio_eor)
	ldbu r5, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r5, r0, .LBB159_4
.LBB159_2:
	lui r5, %hi(fio_desc)
	ldw r1, r5+%lo(fio_desc)
	beq r1, r0, .LBB159_19
.LBB159_3:
	lui r6, %hi(fio_rep)
	ldw r6, r6+%lo(fio_rep)
	ble r6, r0, .LBB159_19
.LBB159_4:
	addi r5, r0, 65
	bne r1, r5, .LBB159_20
.LBB159_5:
	sth sp+20, r3
	lui r1, %hi(fio_w)
	ldw r5, r1+%lo(fio_w)
	addi r6, r0, 2
	bne r4, r6, .LBB159_21
.LBB159_6:
	addi r1, r0, 1
	beq r5, r1, .LBB159_10
.LBB159_7:
	addi r1, r0, 3
	blt r5, r1, .LBB159_9
.LBB159_8:
	lui r1, %hi(fio_col)
	ldw r3, r1+%lo(fio_col)
	add r3, r5, r3
	addi r3, r3, -2
	stw r1+%lo(fio_col), r3
.LBB159_9:
	add r1, r6, r0
.LBB159_10:
	lui r3, %hi(fio_len)
	ldw r7, r3+%lo(fio_len)
	lui r4, %hi(fio_col)
	ldw r10, r4+%lo(fio_col)
	addi r5, r0, 1022
	lui r6, %hi(fio_line)
	addi r6, r6, %lo(fio_line)
	addi r8, r0, 32
	add r9, r0, r0
	jal r0, .LBB159_13
.LBB159_11:
	add r10, lr, r0
.LBB159_12:
	addi r9, r9, 1
	beq r9, r1, .LBB159_30
.LBB159_13:
	addi lr, sp, 20
	bgt r10, r5, .LBB159_12
.LBB159_14:
	bge r7, r10, .LBB159_17
.LBB159_15:
	addi r11, r7, 1
	add r7, r7, r6
	stb r7+0, r8
	add r7, r11, r0
	bne r10, r11, .LBB159_15
.LBB159_16:
	stw r3+%lo(fio_len), r10
	add r7, r10, r0
.LBB159_17:
	add lr, lr, r9
	ldbu lr, lr+0
	add r11, r10, r6
	stb r11+0, lr
	addi lr, r10, 1
	stw r4+%lo(fio_col), lr
	blt r10, r7, .LBB159_11
.LBB159_18:
	stw r3+%lo(fio_len), lr
	add r7, lr, r0
	jal r0, .LBB159_11
.LBB159_19:
	stw r5+%lo(fio_desc), r0
	add r11, r3, r0
	add r12, r4, r0
	jal r31, fio_next_desc
	add r4, r12, r0
	add r3, r11, r0
	addi r5, r0, 65
	beq r1, r5, .LBB159_5
.LBB159_20:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jal r0, f77_wr_i
.LBB159_21:
	lui r1, %hi(fio_col)
	blt r5, r6, .LBB159_23
.LBB159_22:
	ldw r4, r1+%lo(fio_col)
	add r4, r5, r4
	addi r4, r4, -1
	stw r1+%lo(fio_col), r4
.LBB159_23:
	ldw r4, r1+%lo(fio_col)
	addi r5, r0, 1022
	bgt r4, r5, .LBB159_30
.LBB159_24:
	lui r5, %hi(fio_len)
	ldw r6, r5+%lo(fio_len)
	lui r7, %hi(fio_line)
	addi r7, r7, %lo(fio_line)
	bge r6, r4, .LBB159_28
.LBB159_25:
	addi r8, r0, 32
.LBB159_26:
	addi r9, r6, 1
	add r6, r6, r7
	stb r6+0, r8
	add r6, r9, r0
	bne r4, r9, .LBB159_26
.LBB159_27:
	stw r5+%lo(fio_len), r4
	add r6, r4, r0
.LBB159_28:
	add r7, r4, r7
	stb r7+0, r3
	addi r3, r4, 1
	stw r1+%lo(fio_col), r3
	blt r4, r6, .LBB159_30
.LBB159_29:
	stw r5+%lo(fio_len), r3
.LBB159_30:
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	blt r3, r4, .LBB159_32
.LBB159_31:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB159_32:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB159_34
.LBB159_33:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
.LBB159_34:
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
	jalr r0, r31, 0
.Lfunc_end159:
	.size	f77_wr_in, .Lfunc_end159-f77_wr_in
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
	beq r1, r0, .LBB160_5
.LBB160_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB160_26
.LBB160_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB160_26
.LBB160_3:
	addi r3, r0, 1
	bne r1, r3, .LBB160_12
.LBB160_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB160_26
.LBB160_5:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB160_8
.LBB160_6:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB160_15
.LBB160_7:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB160_15
.LBB160_8:
	addi r3, r0, 72
	bgt r1, r3, .LBB160_16
.LBB160_9:
	beq r1, r0, .LBB160_26
.LBB160_10:
	addi r3, r0, 65
	bne r1, r3, .LBB160_19
.LBB160_11:
	addi r4, r0, 8
	add r3, r11, r0
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jal r0, f77_rd_s
.LBB160_12:
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB160_24
.LBB160_13:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB160_14:
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
	bne r5, r0, .LBB160_14
	jal r0, .LBB160_25
.LBB160_15:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB160_9
.LBB160_16:
	addi r3, r0, 73
	bne r1, r3, .LBB160_18
.LBB160_17:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	jal r31, fio_in_int
	fcvt.d.w r4, r1
	jal r0, .LBB160_20
.LBB160_18:
	addi r3, r0, 76
	beq r1, r3, .LBB160_27
.LBB160_19:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
.LBB160_20:
	stw r11+4, r5
	stw r11+0, r4
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	blt r3, r4, .LBB160_22
.LBB160_21:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB160_22:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB160_26
.LBB160_23:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
	jal r0, .LBB160_26
.LBB160_24:
	add r6, r0, r0
.LBB160_25:
	addi r3, sp, 24
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r11+4, r2
	stw r11+0, r1
.LBB160_26:
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jalr r0, r31, 0
.LBB160_27:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end160:
	.size	f77_rd_d, .Lfunc_end160-f77_rd_d
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
	beq r1, r0, .LBB161_5
.LBB161_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB161_26
.LBB161_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB161_26
.LBB161_3:
	addi r3, r0, 1
	bne r1, r3, .LBB161_12
.LBB161_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB161_26
.LBB161_5:
	lui r1, %hi(fio_eor)
	ldbu r3, r1+%lo(fio_eor)
	add r1, r0, r0
	bne r3, r0, .LBB161_8
.LBB161_6:
	lui r3, %hi(fio_desc)
	ldw r1, r3+%lo(fio_desc)
	beq r1, r0, .LBB161_15
.LBB161_7:
	lui r4, %hi(fio_rep)
	ldw r4, r4+%lo(fio_rep)
	ble r4, r0, .LBB161_15
.LBB161_8:
	addi r3, r0, 72
	bgt r1, r3, .LBB161_16
.LBB161_9:
	beq r1, r0, .LBB161_26
.LBB161_10:
	addi r3, r0, 65
	bne r1, r3, .LBB161_19
.LBB161_11:
	addi r4, r0, 4
	add r3, r11, r0
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jal r0, f77_rd_s
.LBB161_12:
	lui r1, %hi(fio_ltok)
	ldbu r5, r1+%lo(fio_ltok)
	beq r5, r0, .LBB161_24
.LBB161_13:
	addi r1, r0, 68
	addi r3, r0, 81
	lui r4, %hi(fio_ltok+1)
	addi r4, r4, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB161_14:
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
	bne r5, r0, .LBB161_14
	jal r0, .LBB161_25
.LBB161_15:
	stw r3+%lo(fio_desc), r0
	jal r31, fio_next_desc
	addi r3, r0, 72
	ble r1, r3, .LBB161_9
.LBB161_16:
	addi r3, r0, 73
	bne r1, r3, .LBB161_18
.LBB161_17:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	jal r31, fio_in_int
	fcvt.s.w r1, r1
	jal r0, .LBB161_20
.LBB161_18:
	addi r3, r0, 76
	beq r1, r3, .LBB161_27
.LBB161_19:
	lui r1, %hi(fio_w)
	ldw r3, r1+%lo(fio_w)
	lui r1, %hi(fio_d)
	ldw r4, r1+%lo(fio_d)
	jal r31, fio_in_real
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
.LBB161_20:
	stw r11+0, r1
	lui r1, %hi(fio_rep)
	ldw r3, r1+%lo(fio_rep)
	addi r4, r0, 1
	blt r3, r4, .LBB161_22
.LBB161_21:
	addi r3, r3, -1
	stw r1+%lo(fio_rep), r3
.LBB161_22:
	ldw r1, r1+%lo(fio_rep)
	bne r1, r0, .LBB161_26
.LBB161_23:
	lui r1, %hi(fio_desc)
	stw r1+%lo(fio_desc), r0
	jal r0, .LBB161_26
.LBB161_24:
	add r6, r0, r0
.LBB161_25:
	addi r3, sp, 24
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	fcvt.s.d r1, r4
	stw r11+0, r1
.LBB161_26:
	ldw r12, sp+152
	ldw r11, sp+156
	ldw lr, sp+0
	addi sp, sp, 160
	jalr r0, r31, 0
.LBB161_27:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	addi r5, r0, 76
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end161:
	.size	f77_rd_r, .Lfunc_end161-f77_rd_r
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
	beq r1, r0, .LBB162_5
.LBB162_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB162_7
.LBB162_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB162_7
.LBB162_3:
	addi r3, r0, 1
	bne r1, r3, .LBB162_6
.LBB162_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB162_7
.LBB162_5:
	add r3, r11, r0
	jal r31, f77_rd_r
	addi r3, r11, 4
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_rd_r
.LBB162_6:
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
.LBB162_7:
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end162:
	.size	f77_rd_c, .Lfunc_end162-f77_rd_c
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function fio_list_cplx_in
	.type	.LCPI163_0,@object
.LCPI163_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI163_0, 8
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
	bne r1, r3, .LBB163_6
.LBB163_1:
	lui r15, %hi(fio_ltok+1)
	addi r15, r15, %lo(fio_ltok+1)
	ldbu r4, r15+0
	beq r4, r0, .LBB163_9
.LBB163_2:
	lui r1, %hi(fio_ltok+2)
	addi r1, r1, %lo(fio_ltok+2)
	addi r3, r0, 126
	add r14, r0, r0
	jal r0, .LBB163_4
.LBB163_3:
	ldbu r4, r1+0
	addi r1, r1, 1
	beq r4, r0, .LBB163_10
.LBB163_4:
	bgt r14, r3, .LBB163_3
.LBB163_5:
	addi r5, r14, 1
	addi r6, sp, 20
	add r6, r6, r14
	stb r6+0, r4
	add r14, r5, r0
	jal r0, .LBB163_3
.LBB163_6:
	beq r1, r0, .LBB163_22
.LBB163_7:
	addi r3, r0, 68
	addi r4, r0, 81
	lui r5, %hi(fio_ltok+1)
	addi r5, r5, %lo(fio_ltok+1)
	add r6, r0, r0
.LBB163_8:
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
	bne r1, r0, .LBB163_8
	jal r0, .LBB163_23
.LBB163_9:
	add r14, r0, r0
.LBB163_10:
	lui r16, %hi(fio_rpos)
	lui r17, %hi(fio_rlen)
	addi r18, r0, 126
	lui r19, %hi(fio_rec)
	addi r19, r19, %lo(fio_rec)
	addi r20, r0, 41
.LBB163_11:
	addi r1, sp, 20
	add r21, r1, r14
	jal r0, .LBB163_13
.LBB163_12:
	jal r31, fio_getrec
	bne r1, r0, .LBB163_37
.LBB163_13:
	beq r14, r0, .LBB163_15
.LBB163_14:
	ldbu r1, r21+-1
	beq r1, r20, .LBB163_18
.LBB163_15:
	ldw r1, r16+%lo(fio_rpos)
	ldw r3, r17+%lo(fio_rlen)
	bge r1, r3, .LBB163_12
.LBB163_16:
	addi r3, r1, 1
	stw r16+%lo(fio_rpos), r3
	bgt r14, r18, .LBB163_13
.LBB163_17:
	add r1, r1, r19
	ldbu r1, r1+0
	addi r3, r14, 1
	addi r4, sp, 20
	add r4, r4, r14
	stb r4+0, r1
	add r14, r3, r0
	jal r0, .LBB163_11
.LBB163_18:
	stb r21+-1, r0
	addi r3, sp, 20
	addi r4, r0, 44
	jal r31, strchr
	beq r1, r0, .LBB163_24
.LBB163_19:
	add r14, r1, r0
	stb r1+0, r0
	addi r4, sp, 20
	add r3, r13, r0
	jal r31, strcpy
	ldbu r4, r13+0
	beq r4, r0, .LBB163_27
.LBB163_20:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB163_21:
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
	bne r4, r0, .LBB163_21
	jal r0, .LBB163_28
.LBB163_22:
	add r6, r0, r0
.LBB163_23:
	addi r3, sp, 148
	add r1, r3, r6
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r12+4, r2
	stw r12+0, r1
	stw r11+4, r0
	stw r11+0, r0
	jal r0, .LBB163_36
.LBB163_24:
	addi r4, sp, 20
	add r3, r13, r0
	jal r31, strcpy
	ldbu r4, r13+0
	beq r4, r0, .LBB163_33
.LBB163_25:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB163_26:
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
	bne r4, r0, .LBB163_26
	jal r0, .LBB163_34
.LBB163_27:
	add r5, r0, r0
.LBB163_28:
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
	beq r4, r0, .LBB163_31
.LBB163_29:
	addi r1, r0, 68
	addi r3, r0, 81
	add r5, r0, r0
.LBB163_30:
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
	bne r4, r0, .LBB163_30
	jal r0, .LBB163_32
.LBB163_31:
	add r5, r0, r0
.LBB163_32:
	addi r3, sp, 148
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	add r4, r1, r0
	add r5, r2, r0
	jal r0, .LBB163_35
.LBB163_33:
	add r5, r0, r0
.LBB163_34:
	addi r3, sp, 148
	add r1, r3, r5
	stb r1+0, r0
	add r4, r0, r0
	jal r31, strtod
	stw r12+4, r2
	stw r12+0, r1
	lui r1, %hi(.LCPI163_0)
	addi r1, r1, %lo(.LCPI163_0)
	ldw r5, r1+4
	ldw r4, r1+0
.LBB163_35:
	stw r11+4, r5
	stw r11+0, r4
	lui r1, %hi(fio_l_after)
	addi r3, r0, 1
	stb r1+%lo(fio_l_after), r3
.LBB163_36:
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
.LBB163_37:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.96)
	addi r4, r4, %lo(.L.str.96)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end163:
	.size	fio_list_cplx_in, .Lfunc_end163-fio_list_cplx_in
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
	beq r1, r0, .LBB164_5
.LBB164_1:
	lui r12, %hi(fio_ldone)
	ldbu r1, r12+%lo(fio_ldone)
	bne r1, r0, .LBB164_7
.LBB164_2:
	jal r31, fio_list_tok
	addi r3, r0, 2
	beq r1, r3, .LBB164_7
.LBB164_3:
	addi r3, r0, 1
	bne r1, r3, .LBB164_6
.LBB164_4:
	stb r12+%lo(fio_ldone), r3
	jal r0, .LBB164_7
.LBB164_5:
	add r3, r11, r0
	jal r31, f77_rd_d
	addi r3, r11, 8
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jal r0, f77_rd_d
.LBB164_6:
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
.LBB164_7:
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.Lfunc_end164:
	.size	f77_rd_z, .Lfunc_end164-f77_rd_z
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
	bne r1, r12, .LBB165_8
.LBB165_1:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB165_5
.LBB165_2:
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
	bne r1, r0, .LBB165_4
.LBB165_3:
	addi r3, r0, 1024
	jal r31, malloc
	stw r14+0, r1
.LBB165_4:
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
.LBB165_5:
	lui r1, %hi(fio_sizep)
	ldw r3, r1+%lo(fio_sizep)
	beq r3, r0, .LBB165_7
.LBB165_6:
	lui r4, %hi(fio_size)
	ldw r4, r4+%lo(fio_size)
	stw r3+0, r4
.LBB165_7:
	stb r11+%lo(fio_noadv), r0
	stw r1+%lo(fio_sizep), r0
.LBB165_8:
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
.Lfunc_end165:
	.size	f77_rd_end, .Lfunc_end165-f77_rd_end
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
	blt r5, r24, .LBB166_3
.LBB166_1:
	add r12, r4, r0
	add r15, r0, r0
.LBB166_2:
	add r1, r12, r15
	ldbu r3, r1+0
	jal r31, toupper
	add r3, r15, r14
	stb r3+0, r1
	addi r15, r15, 1
	bne r13, r15, .LBB166_2
.LBB166_3:
	add r1, r13, r14
	stb r1+0, r0
	beq r11, r0, .LBB166_71
.LBB166_4:
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
	lui r3, %hi(.L.str.9)
	addi r3, r3, %lo(.L.str.9)
	add r4, r0, r0
	jal r31, nml_text_add
	addi r1, sp, 72
	stw sp+24, r13
	add r1, r1, r13
	stw sp+20, r1
	lui r13, %hi(.L.str.128)
	addi r13, r13, %lo(.L.str.128)
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
.LBB166_5:
	ldw r3, r23+%lo(nml_unit)
	add r4, r13, r0
	add r5, r14, r0
	add r6, r14, r0
	jal r31, f77_rd_begin
	beq r1, r0, .LBB166_8
.LBB166_6:
	bne r1, r24, .LBB166_20
.LBB166_7:
	add r16, r1, r0
	jal r0, .LBB166_62
.LBB166_8:
	addi r3, sp, 72
	add r4, r15, r0
	jal r31, f77_rd_s
	ldbu r1, r27+%lo(fio_noadv)
	bne r1, r24, .LBB166_16
.LBB166_9:
	ldw r1, sp+68
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB166_13
.LBB166_10:
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
	bne r1, r0, .LBB166_12
.LBB166_11:
	ldw r3, sp+32
	jal r31, malloc
	stw r18+0, r1
.LBB166_12:
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
.LBB166_13:
	ldw r1, r20+%lo(fio_sizep)
	beq r1, r0, .LBB166_15
.LBB166_14:
	ldw r3, sp+64
	ldw r3, r3+%lo(fio_size)
	stw r1+0, r3
.LBB166_15:
	stb r27+%lo(fio_noadv), r0
	stw r20+%lo(fio_sizep), r0
.LBB166_16:
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_reading)
	stb r1+%lo(fio_reading), r0
	lui r1, %hi(fio_ibuf)
	stw r1+%lo(fio_ibuf), r0
	addi r3, r0, 1022
.LBB166_17:
	addi r1, sp, 72
	add r1, r1, r3
	ldbu r4, r1+0
	addi r1, r0, 32
	bne r4, r1, .LBB166_23
.LBB166_18:
	addi r4, r3, -1
	addi r5, r3, 1
	addi r6, r0, 2
	add r3, r4, r0
	bgtu r5, r6, .LBB166_17
.LBB166_19:
	addi r25, r0, 1
	jal r0, .LBB166_24
.LBB166_20:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB166_22
.LBB166_21:
	addi r1, r0, 2
	stw r19+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.129)
	addi r4, r4, %lo(.L.str.129)
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
.LBB166_22:
	add r16, r0, r0
	add r1, r24, r0
	jal r0, .LBB166_62
.LBB166_23:
	addi r25, r3, 1
.LBB166_24:
	addi r3, r0, 0
	seq r17, fp, r3
	beq r28, r0, .LBB166_26
.LBB166_25:
	addi r28, r0, 1
	jal r0, .LBB166_42
.LBB166_26:
	ldbu r3, sp+72
	andi r4, r3, 253
	addi r3, r0, 36
	seq r4, r4, r3
	sub r26, r0, r4
	and r4, r17, r26
	xor r17, r17, r4
	bge r17, r25, .LBB166_39
.LBB166_27:
	addi r4, sp, 72
	add r4, r4, r17
	ldbu r4, r4+0
	bne r4, r1, .LBB166_30
.LBB166_28:
	addi r17, r17, 1
	bne r25, r17, .LBB166_27
.LBB166_29:
	add r28, r0, r0
	add r17, r25, r0
	jal r0, .LBB166_40
.LBB166_30:
	beq r4, r3, .LBB166_32
.LBB166_31:
	addi r1, r0, 38
	bne r4, r1, .LBB166_39
.LBB166_32:
	addi r28, r0, 1
	blt r12, r28, .LBB166_37
.LBB166_33:
	addi r1, sp, 72
	add r18, r1, r17
	add r21, r0, r0
.LBB166_34:
	add r1, r17, r21
	addi r1, r1, 1
	bgeu r1, r25, .LBB166_39
.LBB166_35:
	add r1, r18, r21
	ldbu r3, r1+1
	jal r31, toupper
	add r3, r21, r11
	ldb r3, r3+0
	bne r1, r3, .LBB166_39
.LBB166_36:
	addi r21, r21, 1
	bne r12, r21, .LBB166_34
.LBB166_37:
	ldw r1, sp+24
	add r21, r1, r17
	bge r21, r25, .LBB166_68
.LBB166_38:
	ldw r1, sp+20
	add r1, r1, r17
	ldbu r18, r1+0
	add r3, r18, r0
	jal r31, isalnum
	beq r1, r0, .LBB166_64
.LBB166_39:
	add r28, r0, r0
.LBB166_40:
	add r3, r0, r0
	add r1, r14, r0
.LBB166_41:
	xori r4, fp, 1
	and r4, r4, r26
	xor fp, fp, r4
	beq r3, r0, .LBB166_62
.LBB166_42:
	bge r17, r25, .LBB166_61
.LBB166_43:
	addi r1, sp, 72
	add r18, r1, r17
	addi r26, r0, 1
	add r4, r0, r0
	add r21, r17, r0
	jal r0, .LBB166_45
.LBB166_44:
	addi r21, r21, 1
	addi r26, r26, 1
	add r4, r3, r0
	bge r21, r25, .LBB166_61
.LBB166_45:
	addi r1, sp, 72
	add r1, r1, r21
	ldb r3, r1+0
	addi r1, r0, 13
	beq r4, r0, .LBB166_47
.LBB166_46:
	seq r3, r4, r3
	sub r3, r0, r3
	and r3, r4, r3
	xor r3, r4, r3
	jal r0, .LBB166_58
.LBB166_47:
	andi r4, r3, 255
	addi r4, r4, -33
	addi r5, r0, 14
	bgtu r4, r5, .LBB166_50
.LBB166_48:
	slli r4, r4, 2
	lui r5, %hi(.LJTI166_0)
	addi r5, r5, %lo(.LJTI166_0)
	add r4, r5, r4
	ldw r4, r4+0
	jalr r0, r4, 0
.LBB166_49:
	addi r1, r0, 11
	add r25, r21, r0
	jal r0, .LBB166_57
.LBB166_50:
	addi r1, r0, -3
	and r1, r3, r1
	addi r3, r0, 36
	bne r1, r3, .LBB166_56
.LBB166_51:
	addi r1, r21, 3
	bge r1, r25, .LBB166_56
.LBB166_52:
	addi r1, sp, 72
	add r1, r1, r21
	ldbu r3, r1+1
	jal r31, toupper
	addi r3, r0, 69
	bne r1, r3, .LBB166_56
.LBB166_53:
	addi r1, sp, 72
	add r1, r1, r21
	ldbu r3, r1+2
	jal r31, toupper
	addi r3, r0, 78
	bne r1, r3, .LBB166_56
.LBB166_54:
	addi r1, sp, 72
	add r1, r1, r21
	ldbu r3, r1+3
	jal r31, toupper
	addi r3, r0, 68
	bne r1, r3, .LBB166_56
.LBB166_55:
	addi r4, r26, -1
	add r3, r18, r0
	jal r31, nml_text_add
	addi r1, r0, 1
	add r16, r0, r0
	jal r0, .LBB166_57
.LBB166_56:
	add r1, r0, r0
.LBB166_57:
	add r3, r0, r0
.LBB166_58:
	beq r1, r0, .LBB166_44
.LBB166_59:
	addi r4, r0, 13
	beq r1, r4, .LBB166_44
.LBB166_60:
	addi r3, r0, 11
	bne r1, r3, .LBB166_62
.LBB166_61:
	addi r1, sp, 72
	add r3, r1, r17
	sub r4, r25, r17
	jal r31, nml_text_add
	lui r3, %hi(.L.str.130)
	addi r3, r3, %lo(.L.str.130)
	addi r4, r0, 1
	jal r31, nml_text_add
	add r1, r0, r0
.LBB166_62:
	beq r1, r0, .LBB166_5
.LBB166_63:
	beq r1, r14, .LBB166_5
	jal r0, .LBB166_69
.LBB166_64:
	addi r1, r18, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB166_39
.LBB166_65:
	addi r1, r0, 64
	beq r18, r1, .LBB166_39
.LBB166_66:
	addi r4, r0, 95
	add r28, r0, r0
	add r3, r0, r0
	add r1, r14, r0
	beq r18, r4, .LBB166_41
.LBB166_67:
	addi r28, r0, 1
.LBB166_68:
	add r17, r21, r0
	add r3, r28, r0
	add r1, r0, r0
	jal r0, .LBB166_41
.LBB166_69:
	beq r16, r0, .LBB166_73
.LBB166_70:
	addi r1, r0, 1
	stw r19+%lo(nml_status), r1
	jal r0, .LBB166_72
.LBB166_71:
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
.LBB166_72:
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
.LBB166_73:
	ldw r1, sp+28
	stw r1+%lo(nml_nasg), r0
	ldw r1, sp+36
	stw r1+%lo(nml_nval), r0
	ldw r1, r22+%lo(nml_tlen)
	addi r15, r0, 1
	blt r1, r15, .LBB166_72
.LBB166_74:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB166_72
.LBB166_75:
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
	lui r1, %hi(.L.str.133)
	addi r1, r1, %lo(.L.str.133)
	stw sp+56, r1
	addi r13, r0, 95
	lui r1, %hi(nml_err+95)
	stw sp+68, r1
	add r12, r15, r0
	add fp, r0, r0
	add r25, r0, r0
.LBB166_76:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r25
	ldbu r3, r1+0
	beq r3, r17, .LBB166_79
.LBB166_77:
	bne r3, r27, .LBB166_86
.LBB166_78:
	addi r11, r25, 1
	jal r0, .LBB166_198
.LBB166_79:
	beq r12, r0, .LBB166_85
.LBB166_80:
	beq fp, r0, .LBB166_85
.LBB166_81:
	ldw r1, sp+36
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB166_84
.LBB166_82:
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
	beq r1, r0, .LBB166_211
.LBB166_83:
	stw r23+%lo(nml_val), r1
.LBB166_84:
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
.LBB166_85:
	addi r11, r25, 1
	jal r0, .LBB166_197
.LBB166_86:
	jal r31, isalpha
                                        # implicit-def: $r11
	add r5, r15, r0
	beq r1, r0, .LBB166_107
.LBB166_87:
	ldw r1, r22+%lo(nml_tlen)
	add r20, r25, r0
	bge r25, r1, .LBB166_94
.LBB166_88:
	add r20, r25, r0
	jal r0, .LBB166_90
.LBB166_89:
	addi r20, r20, 1
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB166_94
.LBB166_90:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB166_89
.LBB166_91:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB166_89
.LBB166_92:
	beq r14, r13, .LBB166_89
.LBB166_93:
	addi r1, r0, 64
	beq r14, r1, .LBB166_89
.LBB166_94:
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB166_104
.LBB166_95:
	ldw r3, r16+%lo(nml_text)
.LBB166_96:
	add r4, r3, r20
	ldbu r4, r4+0
	bne r4, r27, .LBB166_99
.LBB166_97:
	addi r20, r20, 1
	bne r1, r20, .LBB166_96
.LBB166_98:
	add r4, r1, r0
	jal r0, .LBB166_105
.LBB166_99:
	addi r5, r0, 40
	bne r4, r5, .LBB166_104
.LBB166_100:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r20, r1, .LBB166_107
.LBB166_101:
	add r5, r3, r20
	ldbu r6, r5+0
	addi r4, r20, 1
	addi r7, r0, 41
	beq r6, r7, .LBB166_206
.LBB166_102:
	add r20, r4, r0
	blt r4, r1, .LBB166_101
.LBB166_103:
                                        # implicit-def: $r11
	add r5, r15, r0
	jal r0, .LBB166_107
.LBB166_104:
	add r4, r20, r0
.LBB166_105:
                                        # implicit-def: $r11
	add r5, r15, r0
	bge r4, r1, .LBB166_107
.LBB166_106:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r4
	ldbu r1, r1+0
	addi r3, r0, 61
	sne r5, r1, r3
	addi r11, r4, 1
.LBB166_107:
	beq r5, r0, .LBB166_117
.LBB166_108:
	beq fp, r0, .LBB166_163
.LBB166_109:
	ldw r1, r22+%lo(nml_tlen)
	add r11, r25, r0
	bge r25, r1, .LBB166_112
.LBB166_110:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB166_112
.LBB166_111:
	addi r11, r11, 1
	ldw r1, r22+%lo(nml_tlen)
	blt r11, r1, .LBB166_110
.LBB166_112:
	ble r11, r25, .LBB166_131
.LBB166_113:
	ldw r1, r22+%lo(nml_tlen)
	bge r11, r1, .LBB166_131
.LBB166_114:
	ldw r1, r16+%lo(nml_text)
	add r3, r1, r11
	ldbu r3, r3+0
	addi r4, r0, 42
	bne r3, r4, .LBB166_131
.LBB166_115:
	add r3, r1, r25
	jal r31, atoi
	ble r1, r0, .LBB166_203
.LBB166_116:
	add r14, r1, r0
	addi r25, r11, 1
	jal r0, .LBB166_132
.LBB166_117:
	ldw r1, sp+28
	ldw r4, r1+%lo(nml_nasg)
	lui r1, %hi(nml_casg)
	ldw r3, r1+%lo(nml_casg)
	lui r14, %hi(nml_asg)
	blt r4, r3, .LBB166_120
.LBB166_118:
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
	beq r1, r0, .LBB166_211
.LBB166_119:
	stw r14+%lo(nml_asg), r1
.LBB166_120:
	ldw r1, r14+%lo(nml_asg)
	ldw r5, sp+28
	ldw r3, r5+%lo(nml_nasg)
	addi r4, r3, 1
	stw r5+%lo(nml_nasg), r4
	addi r4, r0, 76
	mul r3, r3, r4
	add fp, r1, r3
	ldw r1, r22+%lo(nml_tlen)
	bge r25, r1, .LBB166_130
.LBB166_121:
	add r21, r0, r0
.LBB166_122:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r25
	add r1, r1, r21
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	addi r3, r0, 1
	beq r1, r0, .LBB166_126
.LBB166_123:
	beq r3, r0, .LBB166_166
.LBB166_124:
	addi r1, r0, 30
	bgtu r21, r1, .LBB166_166
.LBB166_125:
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
	blt r20, r1, .LBB166_122
	jal r0, .LBB166_167
.LBB166_126:
	addi r1, r14, -35
	addi r4, r0, 2
	bltu r1, r4, .LBB166_123
.LBB166_127:
	addi r1, r0, 64
	beq r14, r1, .LBB166_123
.LBB166_128:
	beq r14, r13, .LBB166_123
.LBB166_129:
	add r3, r0, r0
	jal r0, .LBB166_123
.LBB166_130:
	add r20, r25, r0
	add r21, r0, r0
	jal r0, .LBB166_168
.LBB166_131:
	add r14, r15, r0
.LBB166_132:
	add r12, r0, r0
	add r1, r0, r0
	add r11, r25, r0
	jal r0, .LBB166_137
.LBB166_133:
	add r1, r0, r0
.LBB166_134:
	add r4, r11, r0
.LBB166_135:
	addi r11, r4, 1
	add r3, r0, r0
.LBB166_136:
	bne r3, r0, .LBB166_159
.LBB166_137:
	ldw r3, r22+%lo(nml_tlen)
	bge r11, r3, .LBB166_159
.LBB166_138:
	ldw r6, r16+%lo(nml_text)
	add r4, r6, r11
	ldb r5, r4+0
	beq r1, r0, .LBB166_142
.LBB166_139:
	bne r1, r5, .LBB166_134
.LBB166_140:
	addi r4, r11, 1
	bge r4, r3, .LBB166_133
.LBB166_141:
	add r3, r6, r4
	ldb r3, r3+0
	bne r1, r3, .LBB166_133
	jal r0, .LBB166_135
.LBB166_142:
	andi r20, r5, 255
	addi r1, r20, -32
	addi r6, r0, 9
	bgtu r1, r6, .LBB166_152
.LBB166_143:
	slli r1, r1, 2
	lui r6, %hi(.LJTI166_1)
	addi r6, r6, %lo(.LJTI166_1)
	add r1, r6, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB166_144:
	addi r11, r11, 1
	add r3, r0, r0
	add r1, r5, r0
	jal r0, .LBB166_136
.LBB166_145:
	addi r1, r0, 2
	blt r11, r1, .LBB166_152
.LBB166_146:
	addi r21, r11, 1
	bge r21, r3, .LBB166_152
.LBB166_147:
	ldbu r3, r4+-1
	jal r31, toupper
	addi r1, r1, -70
	addi r3, r0, -2
	bltu r1, r3, .LBB166_152
.LBB166_148:
	ldw r1, r16+%lo(nml_text)
	addi r28, r11, -2
	add r1, r1, r28
	ldbu r3, r1+0
	jal r31, isdigit
	bne r1, r0, .LBB166_150
.LBB166_149:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r28
	ldbu r1, r1+0
	addi r3, r0, 46
	bne r1, r3, .LBB166_152
.LBB166_150:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r21
	ldbu r3, r1+0
	jal r31, isdigit
	beq r1, r0, .LBB166_152
.LBB166_151:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r11
	addi r3, r0, 43
	stb r1+0, r3
	jal r0, .LBB166_157
.LBB166_152:
	bgt r12, r0, .LBB166_157
.LBB166_153:
	addi r3, r0, 1
	beq r20, r27, .LBB166_158
.LBB166_154:
	beq r20, r17, .LBB166_158
	jal r0, .LBB166_157
.LBB166_155:
	addi r12, r12, 1
	jal r0, .LBB166_157
.LBB166_156:
	addi r12, r12, -1
.LBB166_157:
	addi r11, r11, 1
	add r3, r0, r0
.LBB166_158:
	add r1, r0, r0
	jal r0, .LBB166_136
.LBB166_159:
	ldw r1, sp+36
	ldw r3, r1+%lo(nml_nval)
	ldw r1, r18+%lo(nml_cval)
	blt r3, r1, .LBB166_162
.LBB166_160:
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
	beq r1, r0, .LBB166_211
.LBB166_161:
	stw r23+%lo(nml_val), r1
.LBB166_162:
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
	jal r0, .LBB166_199
.LBB166_163:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB166_165
.LBB166_164:
	stw r19+%lo(nml_status), r26
	ldw r3, sp+60
	ldw r4, sp+56
	add r5, r13, r0
	jal r31, strncpy
	ldw r1, sp+68
	stb r1+%lo(nml_err+95), r0
.LBB166_165:
	add r11, r25, r0
	add fp, r0, r0
	add r14, r15, r0
	jal r0, .LBB166_199
.LBB166_166:
	add r20, r25, r21
	jal r0, .LBB166_168
.LBB166_167:
	add r21, r3, r0
.LBB166_168:
	add r1, fp, r21
	stb r1+0, r0
	jal r0, .LBB166_170
.LBB166_169:
	addi r20, r20, 1
.LBB166_170:
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB166_175
.LBB166_171:
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r14, r1+0
	add r3, r14, r0
	jal r31, isalnum
	bne r1, r0, .LBB166_169
.LBB166_172:
	addi r1, r14, -35
	addi r3, r0, 2
	bltu r1, r3, .LBB166_169
.LBB166_173:
	beq r14, r13, .LBB166_169
.LBB166_174:
	addi r1, r0, 64
	beq r14, r1, .LBB166_169
.LBB166_175:
	ldw r1, r22+%lo(nml_tlen)
	bge r20, r1, .LBB166_180
.LBB166_176:
	ldw r3, r16+%lo(nml_text)
.LBB166_177:
	add r4, r3, r20
	ldbu r4, r4+0
	bne r4, r27, .LBB166_180
.LBB166_178:
	addi r20, r20, 1
	bne r1, r20, .LBB166_177
.LBB166_179:
	add r20, r1, r0
.LBB166_180:
	stw fp+32, r0
	ldw r1, r16+%lo(nml_text)
	add r1, r1, r20
	ldbu r1, r1+0
	addi r3, r0, 40
	bne r1, r3, .LBB166_196
.LBB166_181:
	addi r21, r20, 1
	addi r20, fp, 36
	jal r0, .LBB166_184
.LBB166_182:
	ldw r1, r19+%lo(nml_status)
	addi r14, r0, 1
	beq r1, r0, .LBB166_194
.LBB166_183:
	addi r1, r0, 11
	bne r14, r1, .LBB166_195
.LBB166_184:
	ldw r1, r16+%lo(nml_text)
	addi r21, r21, -1
.LBB166_185:
	add r3, r1, r21
	ldbu r3, r3+1
	addi r21, r21, 1
	beq r3, r27, .LBB166_185
.LBB166_186:
	add r3, r1, r21
	addi r4, sp, 72
	addi r14, r0, 10
	add r5, r14, r0
	jal r31, strtol
	ldw r4, sp+72
	ldw r3, r16+%lo(nml_text)
	add r5, r3, r21
	beq r5, r4, .LBB166_182
.LBB166_187:
	ldw r5, fp+32
	addi r6, r0, 6
	bgt r5, r6, .LBB166_189
.LBB166_188:
	addi r6, r5, 1
	stw fp+32, r6
	slli r5, r5, 2
	add r5, r20, r5
	stw r5+0, r1
.LBB166_189:
	sub r21, r4, r3
.LBB166_190:
	add r1, r3, r21
	ldbu r1, r1+0
	bne r1, r27, .LBB166_192
.LBB166_191:
	addi r21, r21, 1
	jal r0, .LBB166_190
.LBB166_192:
	bne r1, r17, .LBB166_183
.LBB166_193:
	addi r21, r21, 1
	addi r14, r0, 11
	jal r0, .LBB166_183
.LBB166_194:
	addi r1, r0, 2
	stw r19+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.132)
	addi r4, r4, %lo(.L.str.132)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+68
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB166_183
.LBB166_195:
	addi r1, r0, 10
	bne r14, r1, .LBB166_202
.LBB166_196:
	ldw r1, sp+36
	ldw r1, r1+%lo(nml_nval)
	stw fp+64, r1
	stw fp+68, r0
	stw fp+72, r0
.LBB166_197:
	add r12, r15, r0
.LBB166_198:
	add r14, r26, r0
.LBB166_199:
	ori  r1, r14, 2
	bne r1, r26, .LBB166_72
.LBB166_200:
	ldw r1, r22+%lo(nml_tlen)
	bge r11, r1, .LBB166_72
.LBB166_201:
	ldw r1, r19+%lo(nml_status)
	add r25, r11, r0
	beq r1, r0, .LBB166_76
	jal r0, .LBB166_72
.LBB166_202:
	add r11, r25, r0
	jal r0, .LBB166_199
.LBB166_203:
	ldw r1, r19+%lo(nml_status)
	bne r1, r0, .LBB166_205
.LBB166_204:
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
.LBB166_205:
	add r11, r25, r0
	add r14, r15, r0
	jal r0, .LBB166_199
.LBB166_206:
	bge r4, r1, .LBB166_105
.LBB166_207:
	add r3, r0, r0
.LBB166_208:
	add r4, r5, r3
	ldbu r4, r4+1
	bne r4, r27, .LBB166_210
.LBB166_209:
	addi r3, r3, 1
	add r4, r20, r3
	addi r4, r4, 1
	blt r4, r1, .LBB166_208
	jal r0, .LBB166_105
.LBB166_210:
	add r3, r20, r3
	addi r4, r3, 1
	jal r0, .LBB166_105
.LBB166_211:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.131)
	addi r4, r4, %lo(.L.str.131)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end166:
	.size	f77_nml_begin, .Lfunc_end166-f77_nml_begin
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI166_0,@object
.LJTI166_0:
	.word	.LBB166_49
	.word	.LBB166_58
	.word	.LBB166_50
	.word	.LBB166_50
	.word	.LBB166_50
	.word	.LBB166_50
	.word	.LBB166_58
	.word	.LBB166_50
	.word	.LBB166_50
	.word	.LBB166_50
	.word	.LBB166_50
	.word	.LBB166_50
	.word	.LBB166_50
	.word	.LBB166_50
	.word	.LBB166_55
	.size	.LJTI166_0, 60
	.type	.LJTI166_1,@object
.LJTI166_1:
	.word	.LBB166_145
	.word	.LBB166_152
	.word	.LBB166_144
	.word	.LBB166_152
	.word	.LBB166_152
	.word	.LBB166_152
	.word	.LBB166_152
	.word	.LBB166_144
	.word	.LBB166_155
	.word	.LBB166_156
	.size	.LJTI166_1, 40
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
	lui r4, %hi(.L.str.128)
	addi r4, r4, %lo(.L.str.128)
	addi r5, r0, 3
	jal r31, f77_wr_begin
	add r3, r12, r0
	add r4, r11, r0
	jal r31, f77_wr_a
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB167_4
.LBB167_1:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB167_3
.LBB167_2:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB167_4
.LBB167_3:
	lui r11, %hi(fio_draining)
	addi r4, r0, 1
	stb r11+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r11+%lo(fio_draining), r0
.LBB167_4:
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
.Lfunc_end167:
	.size	nml_record, .Lfunc_end167-nml_record
                                        # -- End function
	.globl	f77_nml_dim                     # -- Begin function f77_nml_dim
	.p2align	2
	.type	f77_nml_dim,@function
f77_nml_dim:                            # @f77_nml_dim
# %bb.0:
	lui r1, %hi(nml_ndim)
	ldw r5, r1+%lo(nml_ndim)
	addi r6, r0, 6
	bgt r5, r6, .LBB168_2
.LBB168_1:
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
.LBB168_2:
	jalr r0, r31, 0
.Lfunc_end168:
	.size	f77_nml_dim, .Lfunc_end168-f77_nml_dim
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
	add r24, r8, r0
	add r12, r7, r0
	add fp, r6, r0
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
	blt r4, r17, .LBB169_3
.LBB169_1:
	add r13, r15, r0
.LBB169_2:
	ldbu r3, r16+0
	jal r31, toupper
	stb r11+0, r1
	addi r13, r13, -1
	addi r11, r11, 1
	addi r16, r16, 1
	bne r13, r0, .LBB169_2
.LBB169_3:
	addi r1, sp, 248
	add r1, r1, r15
	stb r1+0, r0
	addi r1, r0, 6
	stw sp+108, r1
	beq r14, r1, .LBB169_6
.LBB169_4:
	addi r1, r0, 2
	beq r14, r1, .LBB169_7
.LBB169_5:
	bne r14, r17, .LBB169_8
.LBB169_6:
	addi r1, r0, 0
	sgt r1, r12, r1
	sub r1, r0, r1
	xori r3, r12, 4
	and r1, r3, r1
	xori r27, r1, 4
	jal r0, .LBB169_11
.LBB169_7:
	addi r27, r0, 4
	jal r0, .LBB169_11
.LBB169_8:
	addi r3, r14, -3
	bgeu r3, r1, .LBB169_10
.LBB169_9:
	addi r27, r0, 8
	jal r0, .LBB169_11
.LBB169_10:
	addi r1, r0, 5
	seq r1, r14, r1
	sub r1, r0, r1
	xori r3, r12, 16
	and r1, r3, r1
	xor r27, r12, r1
.LBB169_11:
	lui r1, %hi(nml_reading)
	ldw r1, r1+%lo(nml_reading)
	lui r3, %hi(nml_ndim)
	stw sp+76, r3
	lui r21, %hi(fio_busy)
	lui r3, %hi(fio_listed)
	stw sp+88, r3
	lui r3, %hi(fio_noadv)
	stw sp+96, r3
	lui r3, %hi(fio_ibuf)
	stw sp+104, r3
	stw sp+92, r24
	beq r1, r0, .LBB169_120
.LBB169_12:
	stw sp+56, r17
	lui r23, %hi(nml_status)
	ldw r1, r23+%lo(nml_status)
	bne r1, r0, .LBB169_170
.LBB169_13:
	lui r1, %hi(nml_nasg)
	stw sp+72, r1
	ldw r1, r1+%lo(nml_nasg)
	ldw r3, sp+56
	blt r1, r3, .LBB169_170
.LBB169_14:
	ldw r1, r23+%lo(nml_status)
	bne r1, r0, .LBB169_170
.LBB169_15:
	addi r1, r0, 36
	stw sp+84, r1
	lui r1, %hi(nml_asg)
	stw sp+68, r1
	addi r1, r0, 76
	stw sp+64, r1
	addi r1, r0, 13
	stw sp+60, r1
	lui r1, %hi(nml_lo)
	addi r1, r1, %lo(nml_lo)
	stw sp+32, r1
	addi r16, r0, 2
	lui r1, %hi(nml_err)
	addi r1, r1, %lo(nml_err)
	stw sp+48, r1
	lui r1, %hi(.L.str.39)
	addi r1, r1, %lo(.L.str.39)
	stw sp+44, r1
	addi r1, r0, 95
	stw sp+40, r1
	lui r1, %hi(nml_err+95)
	stw sp+100, r1
	lui r1, %hi(nml_ext)
	addi r1, r1, %lo(nml_ext)
	stw sp+52, r1
	addi r1, r0, 0
	stw sp+36, r1
	add r4, r0, r0
.LBB169_16:
	ldw r1, sp+68
	ldw r13, r1+%lo(nml_asg)
	ldw r1, sp+64
	stw sp+80, r4
	mul r1, r4, r1
	add r18, r13, r1
	addi r4, sp, 248
	add r3, r18, r0
	jal r31, strcmp
	ldw r11, sp+60
	beq r1, r0, .LBB169_20
.LBB169_17:
	addi r1, r0, 11
	ldw r4, sp+80
	beq r11, r1, .LBB169_170
.LBB169_18:
	addi r4, r4, 1
	ldw r1, sp+72
	ldw r1, r1+%lo(nml_nasg)
	bge r4, r1, .LBB169_170
.LBB169_19:
	ldw r1, r23+%lo(nml_status)
	ldw r3, sp+84
	addi r3, r3, 76
	stw sp+84, r3
	beq r1, r0, .LBB169_16
	jal r0, .LBB169_170
.LBB169_20:
	ldw r20, sp+56
	stw r18+72, r20
	ldw r1, r18+32
	blt r1, r20, .LBB169_31
.LBB169_21:
	ldw r3, sp+76
	ldw r3, r3+%lo(nml_ndim)
	bne r1, r3, .LBB169_32
.LBB169_22:
	ldw r1, r18+32
	blt r1, r20, .LBB169_35
.LBB169_23:
	ldw r1, sp+84
	add r11, r13, r1
	add r13, r0, r0
	add r17, r0, r0
	add r19, r20, r0
	add r15, r0, r0
	ldw r22, sp+32
	jal r0, .LBB169_26
.LBB169_24:
	mul r1, r1, r19
	add r15, r1, r15
	mul r19, r3, r19
	add r1, r20, r0
	beq r1, r0, .LBB169_36
.LBB169_25:
	addi r17, r17, 1
	ldw r1, r18+32
	addi r13, r13, 4
	bge r17, r1, .LBB169_36
.LBB169_26:
	add r1, r11, r13
	ldw r1, r1+0
	add r3, r13, r22
	ldw r3, r3+0
	sub r1, r1, r3
	blt r1, r0, .LBB169_28
.LBB169_27:
	ldw r3, sp+52
	add r3, r13, r3
	ldw r3, r3+0
	blt r1, r3, .LBB169_24
.LBB169_28:
	ldw r1, r23+%lo(nml_status)
	beq r1, r0, .LBB169_30
.LBB169_29:
	add r1, r0, r0
	bne r1, r0, .LBB169_25
	jal r0, .LBB169_36
.LBB169_30:
	stw r23+%lo(nml_status), r16
	ldw r3, sp+48
	ldw r4, sp+44
	ldw r5, sp+40
	jal r31, strncpy
	ldw r1, sp+100
	stb r1+%lo(nml_err+95), r0
	add r1, r0, r0
	bne r1, r0, .LBB169_25
	jal r0, .LBB169_36
.LBB169_31:
	add r15, r0, r0
	jal r0, .LBB169_37
.LBB169_32:
	ldw r1, r23+%lo(nml_status)
	addi r11, r0, 11
	bne r1, r0, .LBB169_34
.LBB169_33:
	stw r23+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.38)
	addi r4, r4, %lo(.L.str.38)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+100
	stb r1+%lo(nml_err+95), r0
.LBB169_34:
	add r1, r0, r0
	add r15, r0, r0
	beq r1, r0, .LBB169_17
	jal r0, .LBB169_37
.LBB169_35:
	add r15, r0, r0
.LBB169_36:
	ldw r1, r23+%lo(nml_status)
	ldw r3, sp+36
	seq r1, r1, r3
	addi r3, r1, -1
	andi r11, r3, 11
	beq r1, r0, .LBB169_17
.LBB169_37:
	ldw r1, r18+68
	addi r22, r0, 1
	blt r1, r22, .LBB169_119
.LBB169_38:
	ldw r1, r23+%lo(nml_status)
	add r11, r0, r0
	bne r1, r0, .LBB169_17
.LBB169_39:
	ldw r17, r18+64
.LBB169_40:
	lui r28, %hi(nml_val)
	ldw r1, r28+%lo(nml_val)
	addi r3, r0, 12
	mul r25, r17, r3
	add r3, r1, r25
	ldw r1, r3+8
	blt r1, r22, .LBB169_117
.LBB169_41:
	bge r15, r24, .LBB169_114
.LBB169_42:
	xor r1, r15, r24
	sgt r4, r15, r24
	sub r4, r0, r4
	and r1, r1, r4
	xor r1, r24, r1
	sub r26, r1, r15
	mul r1, r27, r15
	add r11, fp, r1
	add r24, r0, r0
.LBB169_43:
	ldw r1, r3+4
	blt r1, r22, .LBB169_109
.LBB169_44:
	mul r4, r15, r27
	add r19, fp, r4
	lui r4, %hi(nml_text)
	ldw r4, r4+%lo(nml_text)
	ldw r3, r3+0
	add r4, r4, r3
	addi r3, r0, 7
	beq r14, r3, .LBB169_55
.LBB169_45:
	ldw r3, sp+108
	bne r14, r3, .LBB169_67
.LBB169_46:
	ldbu r3, r4+0
	addi r5, r0, 46
	seq r3, r3, r5
	ble r1, r3, .LBB169_49
.LBB169_47:
	add r13, r4, r3
	ldbu r3, r13+0
	jal r31, toupper
	addi r4, r0, 84
	add r3, r22, r0
	beq r1, r4, .LBB169_52
.LBB169_48:
	ldbu r3, r13+0
	jal r31, toupper
	addi r3, r0, 70
	beq r1, r3, .LBB169_51
.LBB169_49:
	ldw r1, r23+%lo(nml_status)
	add r3, r0, r0
	bne r1, r0, .LBB169_52
.LBB169_50:
	stw r23+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.138)
	addi r4, r4, %lo(.L.str.138)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+100
	stb r1+%lo(nml_err+95), r0
.LBB169_51:
	add r3, r0, r0
.LBB169_52:
	beq r12, r16, .LBB169_80
.LBB169_53:
	bne r12, r22, .LBB169_81
.LBB169_54:
	stb r19+0, r3
	jal r0, .LBB169_109
.LBB169_55:
	ldbu r3, r4+0
	addi r5, r0, 34
	beq r3, r5, .LBB169_57
.LBB169_56:
	addi r5, r0, 39
	bne r3, r5, .LBB169_82
.LBB169_57:
	add r5, r0, r0
	addi r6, r0, 1
	bne r1, r6, .LBB169_62
.LBB169_58:
	ble r12, r5, .LBB169_109
.LBB169_59:
	add r1, r11, r5
	sub r3, r12, r5
.LBB169_60:
	addi r4, r0, 32
	stb r1+0, r4
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB169_60
	jal r0, .LBB169_109
.LBB169_61:
	addi r6, r6, 1
	bge r6, r1, .LBB169_58
.LBB169_62:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB169_65
.LBB169_63:
	addi r6, r6, 1
	bge r6, r1, .LBB169_58
.LBB169_64:
	add r7, r4, r6
	ldbu r7, r7+0
	bne r7, r3, .LBB169_58
.LBB169_65:
	bge r5, r12, .LBB169_61
.LBB169_66:
	add r7, r4, r6
	ldbu r7, r7+0
	addi r8, r5, 1
	add r5, r19, r5
	stb r5+0, r7
	add r5, r8, r0
	jal r0, .LBB169_61
.LBB169_67:
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
	beq r1, r22, .LBB169_171
.LBB169_68:
	add r21, r13, r0
	stb r13+%lo(fio_busy), r22
	lui r1, %hi(fio_uend_init)
	ldbu r3, r1+%lo(fio_uend_init)
	bne r3, r0, .LBB169_72
.LBB169_69:
	addi r3, r0, -400
.LBB169_70:
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	add r4, r3, r4
	addi r5, r0, -1
	stw r4+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB169_70
.LBB169_71:
	stb r1+%lo(fio_uend_init), r22
.LBB169_72:
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
	stw r1+%lo(fio_inrec), r22
	lui r1, %hi(fio_irec)
	stw r1+%lo(fio_irec), r0
	addi r20, r0, 3
	add r3, r0, r0
	add r4, r0, r0
	add r5, r20, r0
	jal r31, fio_rd_setup
	beq r1, r0, .LBB169_75
.LBB169_73:
	stb r21+%lo(fio_busy), r0
	ldw r1, r23+%lo(nml_status)
	bne r1, r0, .LBB169_109
.LBB169_74:
	stw r23+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.140)
	addi r4, r4, %lo(.L.str.140)
	jal r0, .LBB169_84
.LBB169_75:
	addi r1, r14, -1
	bgtu r1, r20, .LBB169_92
.LBB169_76:
	slli r1, r1, 2
	lui r3, %hi(.LJTI169_0)
	addi r3, r3, %lo(.LJTI169_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB169_77:
	stw sp+184, r0
	addi r3, sp, 184
	jal r31, f77_rd_i
	ldw r1, sp+184
	beq r12, r16, .LBB169_111
.LBB169_78:
	bne r12, r22, .LBB169_112
.LBB169_79:
	stb r19+0, r1
	jal r0, .LBB169_100
.LBB169_80:
	sth r19+0, r3
	jal r0, .LBB169_109
.LBB169_81:
	stw r19+0, r3
	jal r0, .LBB169_109
.LBB169_82:
	ldw r1, r23+%lo(nml_status)
	bne r1, r0, .LBB169_109
.LBB169_83:
	stw r23+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.139)
	addi r4, r4, %lo(.L.str.139)
.LBB169_84:
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+100
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB169_109
.LBB169_85:
	add r3, r19, r0
	jal r0, .LBB169_99
.LBB169_86:
	ldw r1, sp+88
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB169_97
.LBB169_87:
	lui r20, %hi(fio_ldone)
	ldbu r1, r20+%lo(fio_ldone)
	bne r1, r0, .LBB169_100
.LBB169_88:
	jal r31, fio_list_tok
	beq r1, r16, .LBB169_100
.LBB169_89:
	beq r1, r22, .LBB169_96
.LBB169_90:
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
	jal r0, .LBB169_100
.LBB169_91:
	add r3, r19, r0
	jal r31, f77_rd_r
	jal r0, .LBB169_100
.LBB169_92:
	ldw r1, sp+88
	ldw r1, r1+%lo(fio_listed)
	beq r1, r0, .LBB169_98
.LBB169_93:
	lui r20, %hi(fio_ldone)
	ldbu r1, r20+%lo(fio_ldone)
	bne r1, r0, .LBB169_100
.LBB169_94:
	jal r31, fio_list_tok
	beq r1, r16, .LBB169_100
.LBB169_95:
	bne r1, r22, .LBB169_113
.LBB169_96:
	stb r20+%lo(fio_ldone), r22
	jal r0, .LBB169_100
.LBB169_97:
	add r3, r19, r0
	jal r31, f77_rd_r
	addi r3, r19, 4
	jal r31, f77_rd_r
	jal r0, .LBB169_100
.LBB169_98:
	add r3, r19, r0
	jal r31, f77_rd_d
	addi r3, r19, 8
.LBB169_99:
	jal r31, f77_rd_d
.LBB169_100:
	ldw r1, sp+96
	ldbu r1, r1+%lo(fio_noadv)
	bne r1, r22, .LBB169_108
.LBB169_101:
	lui r1, %hi(fio_eor)
	ldbu r1, r1+%lo(fio_eor)
	bne r1, r0, .LBB169_105
.LBB169_102:
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
	bne r1, r0, .LBB169_104
.LBB169_103:
	addi r3, r0, 1024
	jal r31, malloc
	stw r19+0, r1
.LBB169_104:
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
	stw r1+0, r22
.LBB169_105:
	lui r1, %hi(fio_sizep)
	ldw r3, r1+%lo(fio_sizep)
	beq r3, r0, .LBB169_107
.LBB169_106:
	lui r4, %hi(fio_size)
	ldw r4, r4+%lo(fio_size)
	stw r3+0, r4
.LBB169_107:
	ldw r3, sp+96
	stb r3+%lo(fio_noadv), r0
	stw r1+%lo(fio_sizep), r0
.LBB169_108:
	stb r21+%lo(fio_busy), r0
	lui r1, %hi(fio_reading)
	stb r1+%lo(fio_reading), r0
	ldw r1, sp+104
	stw r1+%lo(fio_ibuf), r0
.LBB169_109:
	addi r15, r15, 1
	addi r24, r24, 1
	ldw r1, r28+%lo(nml_val)
	add r3, r1, r25
	ldw r1, r3+8
	bge r24, r1, .LBB169_116
.LBB169_110:
	add r11, r11, r27
	bne r24, r26, .LBB169_43
	jal r0, .LBB169_114
.LBB169_111:
	sth r19+0, r1
	jal r0, .LBB169_100
.LBB169_112:
	stw r19+0, r1
	jal r0, .LBB169_100
.LBB169_113:
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
	jal r0, .LBB169_100
.LBB169_114:
	ldw r1, r23+%lo(nml_status)
	bne r1, r0, .LBB169_116
.LBB169_115:
	stw r23+%lo(nml_status), r16
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	lui r4, %hi(.L.str.40)
	addi r4, r4, %lo(.L.str.40)
	addi r5, r0, 95
	jal r31, strncpy
	ldw r1, sp+100
	stb r1+%lo(nml_err+95), r0
.LBB169_116:
	ldw r24, sp+92
.LBB169_117:
	addi r17, r17, 1
	ldw r1, r18+64
	ldw r3, r18+68
	add r1, r3, r1
	bge r17, r1, .LBB169_119
.LBB169_118:
	ldw r1, r23+%lo(nml_status)
	add r11, r0, r0
	beq r1, r0, .LBB169_40
	jal r0, .LBB169_17
.LBB169_119:
	add r11, r0, r0
	jal r0, .LBB169_17
.LBB169_120:
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
	blt r1, r0, .LBB169_125
.LBB169_121:
	addi r17, r1, 1
	stw r11+%lo(nml_npend), r17
	lui r3, %hi(nml_pend)
	addi r3, r3, %lo(nml_pend)
	add r1, r1, r3
	addi r3, r0, 44
	stb r1+0, r3
	ldw r4, r13+%lo(nml_len)
	addi r1, r0, 2
	blt r4, r1, .LBB169_124
.LBB169_122:
	add r1, r4, r17
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB169_124
.LBB169_123:
	add r3, r16, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r16+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
.LBB169_124:
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
.LBB169_125:
	stw sp+100, r21
	addi r15, r15, 1
	ldw r4, r13+%lo(nml_len)
	addi r11, r0, 2
	blt r4, r11, .LBB169_128
.LBB169_126:
	add r1, r4, r15
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB169_128
.LBB169_127:
	add r3, r16, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r16+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
.LBB169_128:
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
	addi r15, r0, 1
	blt r24, r15, .LBB169_170
.LBB169_129:
	addi r26, r0, 5
	seq r16, r14, r26
	addi r1, r0, -2
	and r23, r14, r1
	sub r1, r0, r16
	andi r1, r1, 16
	addi r1, r1, 33
	stw sp+68, r1
	addi r28, r0, 3
	addi r25, r0, 23
	addi r1, r0, 15
	stw sp+84, r1
	lui r1, %hi(.L.str.35)
	addi r1, r1, %lo(.L.str.35)
	stw sp+80, r1
	addi r20, r0, 11
	addi r21, r0, -1
	add r13, fp, r0
	add r17, r0, r0
	stw sp+72, r23
	stw sp+64, r25
	jal r0, .LBB169_133
.LBB169_130:
	ldw r1, r1+0
	fcvt.d.s r6, r1
	addi r22, sp, 184
	add r3, r22, r0
	add r5, r6, r0
	add r6, r7, r0
	add r4, r0, r0
	jal r31, nml_real
	add r3, r22, r0
	ldw r4, sp+84
.LBB169_131:
	jal r31, nml_value
.LBB169_132:
	addi r17, r17, 1
	add r13, r13, r27
	beq r17, r24, .LBB169_170
.LBB169_133:
	mul r1, r17, r27
	add r1, fp, r1
	beq r14, r28, .LBB169_139
.LBB169_134:
	beq r14, r11, .LBB169_130
.LBB169_135:
	bne r14, r15, .LBB169_140
.LBB169_136:
	beq r12, r11, .LBB169_148
.LBB169_137:
	addi r3, r0, 1
	bne r12, r3, .LBB169_149
.LBB169_138:
	ldbu r22, r1+0
	jal r0, .LBB169_150
.LBB169_139:
	ldw r6, r1+4
	ldw r5, r1+0
	addi r22, sp, 184
	add r3, r22, r0
	add r4, r15, r0
	jal r31, nml_real
	add r3, r22, r0
	add r4, r25, r0
	jal r0, .LBB169_131
.LBB169_140:
	addi r3, r0, 4
	bne r23, r3, .LBB169_144
.LBB169_141:
	bne r14, r26, .LBB169_155
.LBB169_142:
	ldw r7, r1+4
	ldw r6, r1+0
	beq r14, r26, .LBB169_156
.LBB169_143:
	ldw r1, r1+4
	fcvt.d.s r18, r1
	jal r0, .LBB169_157
.LBB169_144:
	ldw r3, sp+108
	bne r14, r3, .LBB169_160
.LBB169_145:
	beq r12, r11, .LBB169_167
.LBB169_146:
	addi r3, r0, 1
	bne r12, r3, .LBB169_168
.LBB169_147:
	ldbu r1, r1+0
	jal r0, .LBB169_169
.LBB169_148:
	ldh r22, r1+0
	jal r0, .LBB169_150
.LBB169_149:
	ldw r22, r1+0
.LBB169_150:
	addi r3, sp, 184
	add r4, r20, r0
	add r5, r15, r0
	ldw r6, sp+80
	add r7, r21, r0
	jal r31, f77_wr_begin_int
	add r3, r22, r0
	jal r31, f77_wr_i
	ldw r1, sp+100
	stb r1+%lo(fio_busy), r0
	ldw r1, sp+88
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB169_154
.LBB169_151:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB169_153
.LBB169_152:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB169_154
.LBB169_153:
	lui r18, %hi(fio_draining)
	addi r4, r0, 1
	stb r18+%lo(fio_draining), r4
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r18+%lo(fio_draining), r0
.LBB169_154:
	ldw r5, sp+96
	ldbu r1, r5+%lo(fio_noadv)
	ldw r19, sp+104
	ldw r3, r19+%lo(fio_ibuf)
	addi r4, r0, 0
	seq r3, r3, r4
	and r1, r1, r3
	lui r18, %hi(fio_open_rec)
	stw r18+%lo(fio_open_rec), r1
	stb r5+%lo(fio_noadv), r0
	jal r31, fio_flush_line
	stw r18+%lo(fio_open_rec), r0
	stw r19+%lo(fio_ibuf), r0
	stb sp+195, r0
	addi r3, sp, 184
	addi r4, r0, 11
	jal r0, .LBB169_131
.LBB169_155:
	ldw r3, r1+0
	fcvt.d.s r6, r3
	bne r14, r26, .LBB169_143
.LBB169_156:
	ldw r19, r1+12
	ldw r18, r1+8
.LBB169_157:
	addi r1, r0, 40
	stb sp+280, r1
	addi r22, sp, 112
	add r3, r22, r0
	add r5, r6, r0
	add r6, r7, r0
	add r4, r16, r0
	jal r31, nml_real
	addi r23, sp, 280
	addi r24, r0, 1
	add r3, r23, r0
	add r4, r24, r0
	add r5, r22, r0
	jal r31, nml_trim_cat
	addi r25, r1, 1
	add r1, r23, r1
	addi r3, r0, 44
	stb r1+0, r3
	add r3, r22, r0
	add r5, r18, r0
	add r6, r19, r0
	add r4, r16, r0
	jal r31, nml_real
	add r3, r23, r0
	add r4, r25, r0
	add r5, r22, r0
	jal r31, nml_trim_cat
	addi r5, r1, 1
	add r1, r23, r1
	addi r3, r0, 41
	stb r1+0, r3
	ldw r18, sp+68
	sub r22, r18, r5
	blt r22, r24, .LBB169_159
.LBB169_158:
	addi r23, sp, 280
	add r3, r23, r22
	add r4, r23, r0
	jal r31, memmove
	addi r4, r0, 32
	add r3, r23, r0
	add r5, r22, r0
	jal r31, memset
	add r5, r18, r0
.LBB169_159:
	ldw r23, sp+72
	ldw r25, sp+64
	addi r3, sp, 280
	add r4, r5, r0
	jal r31, nml_value
	ldw r24, sp+92
	jal r0, .LBB169_132
.LBB169_160:
	addi r1, r0, 39
	stb sp+280, r1
	add r5, r15, r0
	blt r12, r15, .LBB169_166
.LBB169_161:
	addi r5, r0, 1
	add r3, r0, r0
	jal r0, .LBB169_164
.LBB169_162:
	add r5, r4, r0
	addi r3, r3, 1
	bge r3, r12, .LBB169_166
.LBB169_163:
	addi r4, r0, 1021
	bge r5, r4, .LBB169_166
.LBB169_164:
	add r4, r13, r3
	ldbu r6, r4+0
	addi r4, r5, 1
	addi r7, sp, 280
	add r7, r7, r5
	stb r7+0, r6
	bne r6, r1, .LBB169_162
.LBB169_165:
	addi r5, r5, 2
	addi r6, sp, 280
	add r4, r6, r4
	stb r4+0, r1
	addi r3, r3, 1
	blt r3, r12, .LBB169_163
.LBB169_166:
	addi r4, r5, 1
	addi r3, sp, 280
	add r5, r3, r5
	stb r5+0, r1
	jal r0, .LBB169_131
.LBB169_167:
	ldh r1, r1+0
	jal r0, .LBB169_169
.LBB169_168:
	ldw r1, r1+0
.LBB169_169:
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
	jal r0, .LBB169_131
.LBB169_170:
	ldw r1, sp+76
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
.LBB169_171:
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
.Lfunc_end169:
	.size	f77_nml_item, .Lfunc_end169-f77_nml_item
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI169_0,@object
.LJTI169_0:
	.word	.LBB169_77
	.word	.LBB169_91
	.word	.LBB169_85
	.word	.LBB169_86
	.size	.LJTI169_0, 16
                                        # -- End function
	.text
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
	blt r1, r0, .LBB170_5
.LBB170_1:
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
	blt r4, r1, .LBB170_4
.LBB170_2:
	add r1, r4, r14
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB170_4
.LBB170_3:
	add r3, r13, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r13+0, r1
	addi r1, r0, 1
	stw r16+%lo(nml_len), r1
.LBB170_4:
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
.LBB170_5:
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
.Lfunc_end170:
	.size	nml_value, .Lfunc_end170-nml_value
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function nml_real
	.type	.LCPI171_0,@object
.LCPI171_0:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI171_0, 8
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
	lui r1, %hi(.LCPI171_0)
	addi r1, r1, %lo(.LCPI171_0)
	ldw r7, r1+4
	ldw r6, r1+0
	feq.d r1, r16, r6
	xori r1, r1, 1
	bne r1, r0, .LBB171_4
.LBB171_1:
	addi r4, r0, 32
	add r3, r11, r0
	add r5, r12, r0
	jal r31, memset
	addi r3, r11, 1
	lui r4, %hi(.L.str.135)
	addi r4, r4, %lo(.L.str.135)
	addi r5, r0, 3
	jal r31, memcpy
.LBB171_2:
	add r1, r11, r12
	stb r1+0, r0
.LBB171_3:
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
.LBB171_4:
	beq r4, r0, .LBB171_13
.LBB171_5:
	lui r6, %hi(.L.str.136)
	addi r6, r6, %lo(.L.str.136)
	addi r13, r0, 1
	addi r7, r0, -1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, f77_wr_begin_int
	add r3, r16, r0
	add r4, r17, r0
	jal r31, f77_wr_d
	lui r1, %hi(fio_busy)
	stb r1+%lo(fio_busy), r0
	lui r1, %hi(fio_listed)
	ldw r1, r1+%lo(fio_listed)
	bne r1, r0, .LBB171_9
.LBB171_6:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB171_8
.LBB171_7:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB171_9
.LBB171_8:
	lui r14, %hi(fio_draining)
	stb r14+%lo(fio_draining), r13
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r14+%lo(fio_draining), r0
.LBB171_9:
	lui r1, %hi(fio_noadv)
	ldbu r3, r1+%lo(fio_noadv)
	lui r13, %hi(fio_ibuf)
	ldw r4, r13+%lo(fio_ibuf)
	seq r4, r4, r15
	and r3, r3, r4
	lui r14, %hi(fio_open_rec)
	stw r14+%lo(fio_open_rec), r3
	stb r1+%lo(fio_noadv), r0
	jal r31, fio_flush_line
	stw r14+%lo(fio_open_rec), r0
	stw r13+%lo(fio_ibuf), r0
	add r1, r11, r12
	stb r1+0, r0
	addi r1, r0, 69
	addi r3, r0, 68
	jal r0, .LBB171_11
.LBB171_10:
	addi r12, r12, -1
	addi r11, r11, 1
	beq r12, r0, .LBB171_3
.LBB171_11:
	ldbu r4, r11+0
	bne r4, r1, .LBB171_10
.LBB171_12:
	stb r11+0, r3
	jal r0, .LBB171_10
.LBB171_13:
	fcvt.s.d r14, r16
	lui r6, %hi(.L.str.137)
	addi r6, r6, %lo(.L.str.137)
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
	bne r1, r0, .LBB171_17
.LBB171_14:
	lui r1, %hi(fio_desc)
	ldw r4, r1+%lo(fio_desc)
	lui r3, %hi(fio_rep)
	beq r4, r0, .LBB171_16
.LBB171_15:
	ldw r4, r3+%lo(fio_rep)
	bgt r4, r0, .LBB171_17
.LBB171_16:
	lui r14, %hi(fio_draining)
	stb r14+%lo(fio_draining), r13
	stw r1+%lo(fio_desc), r0
	stw r3+%lo(fio_rep), r0
	jal r31, fio_next_desc
	stb r14+%lo(fio_draining), r0
.LBB171_17:
	lui r1, %hi(fio_noadv)
	ldbu r3, r1+%lo(fio_noadv)
	lui r13, %hi(fio_ibuf)
	ldw r4, r13+%lo(fio_ibuf)
	seq r4, r4, r15
	and r3, r3, r4
	lui r14, %hi(fio_open_rec)
	stw r14+%lo(fio_open_rec), r3
	stb r1+%lo(fio_noadv), r0
	jal r31, fio_flush_line
	stw r14+%lo(fio_open_rec), r0
	stw r13+%lo(fio_ibuf), r0
	jal r0, .LBB171_2
.Lfunc_end171:
	.size	nml_real, .Lfunc_end171-nml_real
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
	blt r13, r1, .LBB172_4
.LBB172_1:
	add r1, r14, r16
	ldbu r1, r1+0
	bne r1, r17, .LBB172_4
.LBB172_2:
	addi r16, r16, 1
	bne r13, r16, .LBB172_1
.LBB172_3:
	add r16, r13, r0
.LBB172_4:
	sub r1, r13, r16
	addi r3, r0, 3
	add r15, r14, r16
	blt r1, r3, .LBB172_6
.LBB172_5:
	lui r4, %hi(.L.str.135)
	addi r4, r4, %lo(.L.str.135)
	addi r5, r0, 3
	add r3, r15, r0
	jal r31, memcmp
	beq r1, r0, .LBB172_11
.LBB172_6:
	ble r13, r16, .LBB172_12
.LBB172_7:
	addi r1, r13, -1
	xor r3, r16, r1
	slt r4, r16, r1
	sub r4, r0, r4
	and r3, r3, r4
	xor r1, r1, r3
	addi r3, r14, -1
.LBB172_8:
	add r4, r3, r13
	ldbu r4, r4+0
	bne r4, r17, .LBB172_12
.LBB172_9:
	addi r13, r13, -1
	bgt r13, r16, .LBB172_8
.LBB172_10:
	add r13, r1, r0
	jal r0, .LBB172_12
.LBB172_11:
	addi r1, r16, 3
	bne r1, r13, .LBB172_13
.LBB172_12:
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
.LBB172_13:
	add r1, r14, r1
	ldbu r1, r1+0
	bne r1, r17, .LBB172_6
	jal r0, .LBB172_12
.Lfunc_end172:
	.size	nml_trim_cat, .Lfunc_end172-nml_trim_cat
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
	beq r1, r0, .LBB173_6
.LBB173_1:
	lui r11, %hi(nml_status)
	ldw r1, r11+%lo(nml_status)
	beq r1, r0, .LBB173_14
.LBB173_2:
	ldw r1, r11+%lo(nml_status)
	addi r3, r0, 2
	beq r1, r3, .LBB173_18
.LBB173_3:
	addi r3, r0, 1
	bne r1, r3, .LBB173_19
.LBB173_4:
	lui r1, %hi(nml_flags)
	ldbu r1, r1+%lo(nml_flags)
	andi r4, r1, 1
	add r1, r3, r0
	bne r4, r0, .LBB173_19
.LBB173_5:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r1, %hi(nml_unit)
	ldw r5, r1+%lo(nml_unit)
	lui r4, %hi(.L.str.43)
	addi r4, r4, %lo(.L.str.43)
	lui r6, %hi(nml_grp)
	addi r6, r6, %lo(nml_grp)
	jal r0, .LBB173_23
.LBB173_6:
	lui r14, %hi(nml_npend)
	ldw r12, r14+%lo(nml_npend)
	lui r13, %hi(nml_len)
	lui r11, %hi(nml_line)
	addi r11, r11, %lo(nml_line)
	blt r12, r0, .LBB173_11
.LBB173_7:
	ldw r4, r13+%lo(nml_len)
	addi r1, r0, 2
	blt r4, r1, .LBB173_10
.LBB173_8:
	add r1, r4, r12
	lui r3, %hi(nml_width)
	ldbu r3, r3+%lo(nml_width)
	sub r3, r0, r3
	andi r3, r3, 133
	ble r1, r3, .LBB173_10
.LBB173_9:
	add r3, r11, r0
	jal r31, nml_record
	addi r1, r0, 32
	stb r11+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
.LBB173_10:
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
.LBB173_11:
	ldw r4, r13+%lo(nml_len)
	addi r1, r0, 2
	blt r4, r1, .LBB173_13
.LBB173_12:
	lui r3, %hi(nml_line)
	addi r3, r3, %lo(nml_line)
	jal r31, nml_record
.LBB173_13:
	addi r1, r0, 32
	stb r11+0, r1
	addi r1, r0, 1
	stw r13+%lo(nml_len), r1
	lui r3, %hi(.L.str.41)
	addi r3, r3, %lo(.L.str.41)
	addi r4, r0, 5
	jal r31, nml_record
	add r1, r0, r0
	jal r0, .LBB173_19
.LBB173_14:
	lui r1, %hi(nml_nasg)
	ldw r1, r1+%lo(nml_nasg)
	addi r3, r0, 1
	blt r1, r3, .LBB173_2
.LBB173_15:
	lui r3, %hi(nml_asg)
	ldw r5, r3+%lo(nml_asg)
.LBB173_16:
	ldw r3, r5+72
	beq r3, r0, .LBB173_20
.LBB173_17:
	addi r5, r5, 76
	addi r1, r1, -1
	bne r1, r0, .LBB173_16
	jal r0, .LBB173_2
.LBB173_18:
	lui r3, %hi(nml_flags)
	ldbu r3, r3+%lo(nml_flags)
	andi r3, r3, 2
	beq r3, r0, .LBB173_22
.LBB173_19:
	ldw r14, sp+112
	ldw r13, sp+116
	ldw r12, sp+120
	ldw r11, sp+124
	ldw lr, sp+0
	addi sp, sp, 128
	jalr r0, r31, 0
.LBB173_20:
	lui r4, %hi(.L.str.42)
	addi r4, r4, %lo(.L.str.42)
	addi r3, sp, 16
	jal r31, sprintf
	ldw r1, r11+%lo(nml_status)
	bne r1, r0, .LBB173_2
.LBB173_21:
	addi r1, r0, 2
	stw r11+%lo(nml_status), r1
	lui r3, %hi(nml_err)
	addi r3, r3, %lo(nml_err)
	addi r4, sp, 16
	addi r5, r0, 95
	jal r31, strncpy
	lui r1, %hi(nml_err+95)
	stb r1+%lo(nml_err+95), r0
	jal r0, .LBB173_2
.LBB173_22:
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
.LBB173_23:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end173:
	.size	f77_nml_end, .Lfunc_end173-f77_nml_end
                                        # -- End function
	.globl	f77_ipow                        # -- Begin function f77_ipow
	.p2align	2
	.type	f77_ipow,@function
f77_ipow:                               # @f77_ipow
# %bb.0:
	addi r1, r0, 1
	blt r4, r0, .LBB174_4
.LBB174_1:
	beq r4, r0, .LBB174_9
.LBB174_2:
	addi r5, r0, 0
.LBB174_3:
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
	bne r4, r0, .LBB174_3
	jal r0, .LBB174_9
.LBB174_4:
	beq r3, r1, .LBB174_7
.LBB174_5:
	addi r1, r0, -1
	bne r3, r1, .LBB174_8
.LBB174_6:
	andi r1, r4, 1
	addi r3, r0, 0
	seq r1, r1, r3
	addi r1, r1, -1
	ori  r1, r1, 1
	jal r0, .LBB174_9
.LBB174_7:
	add r1, r3, r0
	jal r0, .LBB174_9
.LBB174_8:
	add r1, r0, r0
.LBB174_9:
	jalr r0, r31, 0
.Lfunc_end174:
	.size	f77_ipow, .Lfunc_end174-f77_ipow
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function f77_dpow_i
	.type	.LCPI175_0,@object
.LCPI175_0:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI175_0, 8
	.type	.LCPI175_1,@object
.LCPI175_1:
	.quad	0x0000000000000000              # double 0
	.size	.LCPI175_1, 8
	.type	.LCPI175_2,@object
.LCPI175_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI175_2, 8
	.type	.LCPI175_3,@object
.LCPI175_3:
	.quad	0x7fefdcf158adbb99              # double 1.79E+308
	.size	.LCPI175_3, 8
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
	blt r5, r0, .LBB175_3
.LBB175_1:
	andi r3, r5, 1
	addi r1, r0, 0
	seq r3, r3, r1
	lui r4, %hi(.LCPI175_0)
	addi r4, r4, %lo(.LCPI175_0)
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
	beq r3, r0, .LBB175_16
.LBB175_2:
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
	bne r3, r0, .LBB175_2
	jal r0, .LBB175_16
.LBB175_3:
	sub r1, r0, r5
	lui r3, %hi(.LCPI175_0)
	addi r3, r3, %lo(.LCPI175_0)
	ldw r5, r3+4
	ldw r4, r3+0
	lui r3, %hi(.LCPI175_1)
	addi r3, r3, %lo(.LCPI175_1)
	ldw r9, r3+4
	ldw r8, r3+0
	lui r3, %hi(.LCPI175_3)
	addi r3, r3, %lo(.LCPI175_3)
	ldw r11, r3+4
	ldw r10, r3+0
	lui r3, %hi(.LCPI175_2)
	addi r3, r3, %lo(.LCPI175_2)
	ldw r15, r3+4
	ldw r14, r3+0
	add r16, r6, r0
	add r17, r7, r0
	add r3, r1, r0
	add r12, r4, r0
	add r13, r5, r0
	jal r0, .LBB175_5
.LBB175_4:
	fmul.d r16, r16, r16
.LBB175_5:
	andi lr, r3, 1
	fneg.d r18, r16
	beq lr, r0, .LBB175_10
.LBB175_6:
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
	bne lr, r0, .LBB175_9
.LBB175_7:
	fle.d lr, r20, r4
	bne lr, r0, .LBB175_9
.LBB175_8:
	fle.d lr, r20, r14
	xori lr, lr, 1
	fle.d r20, r22, r14
	xori r20, r20, 1
	or  lr, r20, lr
	bne lr, r0, .LBB175_13
.LBB175_9:
	fmul.d r12, r16, r12
.LBB175_10:
	srli r3, r3, 1
	beq r3, r0, .LBB175_15
.LBB175_11:
	flt.d lr, r16, r8
	xor r20, r19, r17
	sub lr, r0, lr
	and r20, r20, lr
	xor r21, r17, r20
	xor r18, r18, r16
	and lr, r18, lr
	xor r20, r16, lr
	fle.d lr, r20, r14
	bne lr, r0, .LBB175_4
.LBB175_12:
	fdiv.d r18, r10, r20
	fle.d lr, r20, r18
	xori lr, lr, 1
	beq lr, r0, .LBB175_4
.LBB175_13:
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
	beq r1, r0, .LBB175_16
.LBB175_14:
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
	bne r1, r0, .LBB175_14
	jal r0, .LBB175_16
.LBB175_15:
	fdiv.d r8, r4, r12
.LBB175_16:
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
.Lfunc_end175:
	.size	f77_dpow_i, .Lfunc_end175-f77_dpow_i
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function f77_rpow_i
	.type	.LCPI176_0,@object
.LCPI176_0:
	.word	0x3f800000                      # float 1
	.size	.LCPI176_0, 4
	.type	.LCPI176_1,@object
.LCPI176_1:
	.word	0x00000000                      # float 0
	.size	.LCPI176_1, 4
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0
	.type	.LCPI176_2,@object
.LCPI176_2:
	.quad	0x43abc16d674ec800              # double 1.0E+18
	.size	.LCPI176_2, 8
	.type	.LCPI176_3,@object
.LCPI176_3:
	.quad	0x47eff933c78cdfad              # double 3.4E+38
	.size	.LCPI176_3, 8
	.type	.LCPI176_4,@object
.LCPI176_4:
	.quad	0x3ff0000000000000              # double 1
	.size	.LCPI176_4, 8
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
	blt r4, r0, .LBB176_3
.LBB176_1:
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
	srli r4, r4, 1
	beq r4, r0, .LBB176_17
.LBB176_2:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	srli r4, r4, 1
	bne r4, r0, .LBB176_2
	jal r0, .LBB176_17
.LBB176_3:
	sub r4, r0, r4
	lui r1, %hi(.LCPI176_0)
	ldw r5, r1+%lo(.LCPI176_0)
	lui r6, %hi(.LCPI176_1)
	ldw r6, r6+%lo(.LCPI176_1)
	lui r7, %hi(.LCPI176_3)
	addi r7, r7, %lo(.LCPI176_3)
	ldw r9, r7+4
	ldw r8, r7+0
	lui r7, %hi(.LCPI176_2)
	addi r7, r7, %lo(.LCPI176_2)
	ldw r11, r7+4
	ldw r10, r7+0
	add r7, r3, r0
	add lr, r4, r0
	jal r0, .LBB176_5
.LBB176_4:
	fmul.s r7, r7, r7
.LBB176_5:
	andi r15, lr, 1
	fcvt.d.s r12, r7
	flt.s r14, r7, r6
	beq r15, r0, .LBB176_10
.LBB176_6:
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
	bne r15, r0, .LBB176_9
.LBB176_7:
	lui r15, %hi(.LCPI176_4)
	addi r15, r15, %lo(.LCPI176_4)
	ldw r21, r15+4
	ldw r20, r15+0
	fle.d r15, r16, r20
	bne r15, r0, .LBB176_9
.LBB176_8:
	fle.d r15, r16, r10
	xori r15, r15, 1
	fle.d r16, r18, r10
	xori r16, r16, 1
	or  r15, r16, r15
	bne r15, r0, .LBB176_13
.LBB176_9:
	fmul.s r5, r7, r5
.LBB176_10:
	srli lr, lr, 1
	beq lr, r0, .LBB176_16
.LBB176_11:
	fneg.d r16, r12
	xor r15, r17, r13
	sub r18, r0, r14
	and r14, r15, r18
	xor r15, r13, r14
	xor r16, r16, r12
	and r16, r16, r18
	xor r14, r12, r16
	fle.d r12, r14, r10
	bne r12, r0, .LBB176_4
.LBB176_12:
	fdiv.d r12, r8, r14
	fle.d r12, r14, r12
	xori r12, r12, 1
	beq r12, r0, .LBB176_4
.LBB176_13:
	ldw r1, r1+%lo(.LCPI176_0)
	fdiv.s r3, r1, r3
	andi r1, r4, 1
	addi r5, r0, 0
	seq r1, r1, r5
	lui r6, 260096
	xor r6, r3, r6
	sub r1, r0, r1
	and r1, r6, r1
	xor r1, r3, r1
.LBB176_14:
	srli r4, r4, 1
	beq r4, r0, .LBB176_17
.LBB176_15:
	fmul.s r3, r3, r3
	andi r6, r4, 1
	seq r6, r6, r5
	fmul.s r7, r3, r1
	xor r1, r1, r7
	sub r6, r0, r6
	and r1, r1, r6
	xor r1, r7, r1
	jal r0, .LBB176_14
.LBB176_16:
	ldw r1, r1+%lo(.LCPI176_0)
	fdiv.s r1, r1, r5
.LBB176_17:
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
.Lfunc_end176:
	.size	f77_rpow_i, .Lfunc_end176-f77_rpow_i
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
.Lfunc_end177:
	.size	f77_open_f90, .Lfunc_end177-f77_open_f90
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
	bne lr, r0, .LBB178_4
.LBB178_1:
	addi lr, r0, -400
	lui r11, %hi(fio_uend+400)
	addi r11, r11, %lo(fio_uend+400)
	addi r12, r0, -1
.LBB178_2:
	add r13, lr, r11
	stw r13+0, r12
	addi lr, lr, 4
	bne lr, r0, .LBB178_2
.LBB178_3:
	addi lr, r0, 1
	stb r1+%lo(fio_uend_init), lr
.LBB178_4:
	ldw r23, sp+368
	addi r1, r3, -100
	addi lr, r0, -99
	bltu r1, lr, .LBB178_21
.LBB178_5:
	addi r1, r3, -5
	addi r25, r0, 1
	bleu r1, r25, .LBB178_21
.LBB178_6:
	beq r8, r0, .LBB178_13
.LBB178_7:
	addi r1, r0, 1
	add r15, r0, r0
	blt r9, r1, .LBB178_14
.LBB178_8:
	addi lr, r0, 6
	lui r11, %hi(.L.str.46)
	addi r11, r11, %lo(.L.str.46)
	addi r12, r0, 26
	add r13, r0, r0
.LBB178_9:
	beq r13, lr, .LBB178_23
.LBB178_10:
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
	bne r15, r14, .LBB178_13
.LBB178_11:
	addi r13, r13, 1
	bne r9, r13, .LBB178_9
.LBB178_12:
	add r11, r9, r0
	beq r11, lr, .LBB178_24
.LBB178_13:
	add r15, r0, r0
.LBB178_14:
	add r16, r15, r0
	beq r10, r0, .LBB178_35
.LBB178_15:
	ldw r8, sp+352
	addi r1, r0, 1
	add r16, r15, r0
	blt r8, r1, .LBB178_35
.LBB178_16:
	addi r9, r0, 11
	lui lr, %hi(.L.str.47)
	addi lr, lr, %lo(.L.str.47)
	addi r11, r0, 26
	add r12, r0, r0
.LBB178_17:
	beq r12, r9, .LBB178_28
.LBB178_18:
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
	bne r14, r13, .LBB178_34
.LBB178_19:
	addi r12, r12, 1
	bne r8, r12, .LBB178_17
.LBB178_20:
	add lr, r8, r0
	beq lr, r9, .LBB178_29
	jal r0, .LBB178_34
.LBB178_21:
	bne r23, r0, .LBB178_44
.LBB178_22:
	lui r1, %hi(stderr)
	ldw r1, r1+%lo(stderr)
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	add r5, r3, r0
	add r3, r1, r0
	jal r0, .LBB178_212
.LBB178_23:
	add r11, lr, r0
	bne r11, lr, .LBB178_13
.LBB178_24:
	addi lr, r0, 7
	add r15, r1, r0
	blt r9, lr, .LBB178_14
.LBB178_25:
	addi lr, r9, -7
	addi r1, r8, 6
	addi r8, r0, 32
	addi r9, r0, 1
.LBB178_26:
	ldbu r11, r1+0
	seq r15, r11, r8
	addi r12, lr, -1
	sltu lr, r12, lr
	bne lr, r9, .LBB178_14
.LBB178_27:
	addi r1, r1, 1
	add lr, r12, r0
	beq r11, r8, .LBB178_26
	jal r0, .LBB178_14
.LBB178_28:
	add lr, r9, r0
	bne lr, r9, .LBB178_34
.LBB178_29:
	addi r9, r0, 12
	blt r8, r9, .LBB178_33
.LBB178_30:
	addi r9, r10, 11
	addi r8, r8, -11
	addi r10, r0, 32
.LBB178_31:
	ldbu lr, r9+0
	bne lr, r10, .LBB178_34
.LBB178_32:
	addi r9, r9, 1
	addi r8, r8, -1
	bne r8, r0, .LBB178_31
.LBB178_33:
	add r16, r1, r0
	jal r0, .LBB178_35
.LBB178_34:
	add r16, r0, r0
.LBB178_35:
	ldw r17, sp+364
	addi r24, r0, 0
	sne r1, r6, r24
	sgt r8, r7, r24
	and r26, r1, r8
	bne r26, r25, .LBB178_41
.LBB178_36:
	addi r1, r0, 7
	lui r8, %hi(.L.str.48)
	addi r8, r8, %lo(.L.str.48)
	addi r9, r0, 26
	add r10, r0, r0
.LBB178_37:
	beq r10, r1, .LBB178_59
.LBB178_38:
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
	bne r11, lr, .LBB178_41
.LBB178_39:
	addi r10, r10, 1
	bne r7, r10, .LBB178_37
.LBB178_40:
	add r8, r7, r0
	beq r8, r1, .LBB178_60
.LBB178_41:
	add r20, r0, r0
.LBB178_42:
	sgt r1, r17, r24
	xori r8, r15, 1
	or  r1, r1, r8
	bne r1, r0, .LBB178_46
.LBB178_43:
	beq r23, r0, .LBB178_213
.LBB178_44:
	addi r1, r0, 2
.LBB178_45:
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
.LBB178_46:
	beq r4, r0, .LBB178_51
.LBB178_47:
	addi r18, r0, 1
	blt r5, r18, .LBB178_51
.LBB178_48:
	addi r1, r0, 256
	blt r5, r1, .LBB178_70
.LBB178_49:
	bne r23, r0, .LBB178_44
.LBB178_50:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.50)
	addi r4, r4, %lo(.L.str.50)
	jal r0, .LBB178_212
.LBB178_51:
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
.LBB178_52:
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
	beq r1, r0, .LBB178_55
.LBB178_53:
	add r3, r21, r22
	addi r4, sp, 28
	jal r31, strcmp
	sne r1, r1, r24
	or  r1, r20, r1
	beq r1, r0, .LBB178_73
.LBB178_54:
	ldw r3, r13+0
	jal r31, fclose
	stw r13+0, r0
.LBB178_55:
	beq r20, r0, .LBB178_64
.LBB178_56:
	jal r31, tmpfile
	bne r1, r0, .LBB178_113
.LBB178_57:
	bne r23, r0, .LBB178_44
.LBB178_58:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.53)
	addi r4, r4, %lo(.L.str.53)
	jal r0, .LBB178_212
.LBB178_59:
	add r8, r1, r0
	bne r8, r1, .LBB178_41
.LBB178_60:
	addi r1, r0, 8
	blt r7, r1, .LBB178_80
.LBB178_61:
	addi r10, r7, -8
	addi r1, r6, 7
	addi r8, r0, 32
	addi r9, r0, 1
.LBB178_62:
	ldbu lr, r1+0
	seq r20, lr, r8
	addi r11, r10, -1
	sltu r10, r11, r10
	bne r10, r9, .LBB178_42
.LBB178_63:
	addi r1, r1, 1
	add r10, r11, r0
	beq lr, r8, .LBB178_62
	jal r0, .LBB178_42
.LBB178_64:
	beq r26, r0, .LBB178_107
.LBB178_65:
	addi r1, r0, 3
	lui r5, %hi(.L.str.54)
	addi r5, r5, %lo(.L.str.54)
	addi r6, r0, 26
	add r7, r0, r0
	add r4, r12, r0
	add r3, r11, r0
.LBB178_66:
	beq r7, r1, .LBB178_81
.LBB178_67:
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
	bne r9, r8, .LBB178_92
.LBB178_68:
	addi r7, r7, 1
	bne r4, r7, .LBB178_66
.LBB178_69:
	add r5, r4, r0
	jal r0, .LBB178_82
.LBB178_70:
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
.LBB178_71:
	add r5, r1, r13
	ldbu r6, r5+0
	bne r6, r3, .LBB178_52
.LBB178_72:
	add r4, r13, r0
	addi r13, r13, -1
	stb r5+0, r0
	bgt r4, r18, .LBB178_71
	jal r0, .LBB178_52
.LBB178_73:
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
	beq r19, r0, .LBB178_207
.LBB178_74:
	add r3, r0, r0
	blt r18, r25, .LBB178_208
.LBB178_75:
	addi r1, r0, 4
	lui r3, %hi(.L.str.52)
	addi r3, r3, %lo(.L.str.52)
	addi r4, r0, 26
	add r5, r0, r0
.LBB178_76:
	beq r5, r1, .LBB178_96
.LBB178_77:
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
	bne r7, r6, .LBB178_207
.LBB178_78:
	addi r5, r5, 1
	bne r18, r5, .LBB178_76
.LBB178_79:
	add r3, r18, r0
	jal r0, .LBB178_97
.LBB178_80:
	addi r20, r0, 1
	jal r0, .LBB178_42
.LBB178_81:
	add r5, r1, r0
.LBB178_82:
	bne r5, r1, .LBB178_92
.LBB178_83:
	addi r1, r0, 4
	blt r4, r1, .LBB178_87
.LBB178_84:
	addi r1, r3, 3
	addi r5, r4, -3
	addi r6, r0, 32
.LBB178_85:
	ldbu r7, r1+0
	bne r7, r6, .LBB178_92
.LBB178_86:
	addi r1, r1, 1
	addi r5, r5, -1
	bne r5, r0, .LBB178_85
.LBB178_87:
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	addi r3, sp, 28
	jal r31, fopen
	bne r1, r0, .LBB178_89
.LBB178_88:
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	addi r3, sp, 28
	jal r31, fopen
.LBB178_89:
	bne r1, r0, .LBB178_113
.LBB178_90:
	bne r23, r0, .LBB178_44
.LBB178_91:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.57)
	addi r4, r4, %lo(.L.str.57)
	jal r0, .LBB178_146
.LBB178_92:
	lui r5, %hi(.L.str.58)
	addi r5, r5, %lo(.L.str.58)
	add r11, r3, r0
	add r12, r4, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB178_103
.LBB178_93:
	lui r4, %hi(.L.str.56)
	addi r4, r4, %lo(.L.str.56)
	addi r3, sp, 28
	jal r31, fopen
	beq r1, r0, .LBB178_104
.LBB178_94:
	add r3, r1, r0
	jal r31, fclose
	bne r23, r0, .LBB178_44
.LBB178_95:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.59)
	addi r4, r4, %lo(.L.str.59)
	jal r0, .LBB178_146
.LBB178_96:
	add r3, r1, r0
.LBB178_97:
	bne r3, r1, .LBB178_207
.LBB178_98:
	addi r1, r0, 5
	blt r18, r1, .LBB178_102
.LBB178_99:
	addi r1, r19, 4
	addi r3, r18, -4
	addi r4, r0, 32
.LBB178_100:
	ldbu r5, r1+0
	bne r5, r4, .LBB178_207
.LBB178_101:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB178_100
.LBB178_102:
	add r3, r25, r0
	jal r0, .LBB178_208
.LBB178_103:
	lui r5, %hi(.L.str.62)
	addi r5, r5, %lo(.L.str.62)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB178_107
.LBB178_104:
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	addi r3, sp, 28
	jal r31, fopen
	bne r1, r0, .LBB178_113
.LBB178_105:
	bne r23, r0, .LBB178_44
.LBB178_106:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.61)
	addi r4, r4, %lo(.L.str.61)
	jal r0, .LBB178_146
.LBB178_107:
	beq r11, r0, .LBB178_110
.LBB178_108:
	addi r1, r0, 1
	blt r12, r1, .LBB178_110
.LBB178_109:
	lui r5, %hi(.L.str.63)
	addi r5, r5, %lo(.L.str.63)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB178_209
.LBB178_110:
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	addi r3, sp, 28
	jal r31, fopen
	bne r1, r0, .LBB178_112
.LBB178_111:
	lui r4, %hi(.L.str.60)
	addi r4, r4, %lo(.L.str.60)
	addi r3, sp, 28
	jal r31, fopen
.LBB178_112:
	beq r1, r0, .LBB178_144
.LBB178_113:
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
	beq r3, r0, .LBB178_128
.LBB178_114:
	ldw r4, r26+%lo(fio_o_actlen)
	addi r5, r0, 1
	blt r4, r5, .LBB178_128
.LBB178_115:
	addi r6, r0, 4
	lui r7, %hi(.L.str.66)
	addi r7, r7, %lo(.L.str.66)
	addi r8, r0, 26
	add r9, r0, r0
.LBB178_116:
	beq r9, r6, .LBB178_120
.LBB178_117:
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
	bne lr, r10, .LBB178_126
.LBB178_118:
	addi r9, r9, 1
	bne r4, r9, .LBB178_116
.LBB178_119:
	add r7, r4, r0
	jal r0, .LBB178_121
.LBB178_120:
	add r7, r6, r0
.LBB178_121:
	bne r7, r6, .LBB178_126
.LBB178_122:
	addi r6, r0, 5
	blt r4, r6, .LBB178_127
.LBB178_123:
	addi r6, r3, 4
	addi r7, r4, -4
	addi r8, r0, 32
.LBB178_124:
	ldbu r9, r6+0
	bne r9, r8, .LBB178_126
.LBB178_125:
	addi r6, r6, 1
	addi r7, r7, -1
	bne r7, r0, .LBB178_124
	jal r0, .LBB178_127
.LBB178_126:
	lui r5, %hi(.L.str.67)
	addi r5, r5, %lo(.L.str.67)
	add r12, r1, r0
	jal r31, fio_str_is
	add r3, r1, r0
	add r1, r12, r0
	seq r3, r3, r24
	addi r3, r3, -1
	andi r5, r3, 2
.LBB178_127:
	stw r11+0, r5
.LBB178_128:
	lui r24, %hi(fio_o_pos)
	ldw r12, r24+%lo(fio_o_pos)
	beq r12, r0, .LBB178_148
.LBB178_129:
	lui r3, %hi(fio_o_poslen)
	ldw r13, r3+%lo(fio_o_poslen)
	addi r11, r0, 1
	blt r13, r11, .LBB178_148
.LBB178_130:
	addi r3, r0, 6
	lui r4, %hi(.L.str.68)
	addi r4, r4, %lo(.L.str.68)
	addi r5, r0, 26
	add r6, r0, r0
.LBB178_131:
	beq r6, r3, .LBB178_135
.LBB178_132:
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
	bne r8, r7, .LBB178_142
.LBB178_133:
	addi r6, r6, 1
	bne r13, r6, .LBB178_131
.LBB178_134:
	add r4, r13, r0
	jal r0, .LBB178_136
.LBB178_135:
	add r4, r3, r0
.LBB178_136:
	bne r4, r3, .LBB178_142
.LBB178_137:
	addi r3, r0, 7
	blt r13, r3, .LBB178_141
.LBB178_138:
	addi r3, r12, 6
	addi r4, r13, -6
	addi r5, r0, 32
.LBB178_139:
	ldbu r6, r3+0
	bne r6, r5, .LBB178_142
.LBB178_140:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB178_139
.LBB178_141:
	addi r11, r0, 2
	add r3, r1, r0
	add r4, r0, r0
	add r5, r11, r0
	jal r31, fseek
	jal r0, .LBB178_143
.LBB178_142:
	lui r5, %hi(.L.str.69)
	addi r5, r5, %lo(.L.str.69)
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB178_147
.LBB178_143:
	stw r27+0, r11
	jal r0, .LBB178_148
.LBB178_144:
	bne r23, r0, .LBB178_44
.LBB178_145:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.64)
	addi r4, r4, %lo(.L.str.64)
.LBB178_146:
	addi r5, sp, 28
	jal r0, .LBB178_212
.LBB178_147:
	lui r5, %hi(.L.str.70)
	addi r5, r5, %lo(.L.str.70)
	add r3, r12, r0
	add r4, r13, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB178_214
.LBB178_148:
	ldw r11, r23+%lo(fio_o_act)
	beq r11, r0, .LBB178_163
.LBB178_149:
	ldw r12, r26+%lo(fio_o_actlen)
	addi r1, r0, 1
	blt r12, r1, .LBB178_163
.LBB178_150:
	addi r1, r0, 4
	lui r3, %hi(.L.str.66)
	addi r3, r3, %lo(.L.str.66)
	addi r4, r0, 26
	add r5, r0, r0
.LBB178_151:
	beq r5, r1, .LBB178_155
.LBB178_152:
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
	bne r7, r6, .LBB178_161
.LBB178_153:
	addi r5, r5, 1
	bne r12, r5, .LBB178_151
.LBB178_154:
	add r3, r12, r0
	jal r0, .LBB178_156
.LBB178_155:
	add r3, r1, r0
.LBB178_156:
	bne r3, r1, .LBB178_161
.LBB178_157:
	addi r1, r0, 5
	blt r12, r1, .LBB178_163
.LBB178_158:
	addi r1, r11, 4
	addi r3, r12, -4
	addi r4, r0, 32
.LBB178_159:
	ldbu r5, r1+0
	bne r5, r4, .LBB178_161
.LBB178_160:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB178_159
	jal r0, .LBB178_163
.LBB178_161:
	lui r5, %hi(.L.str.67)
	addi r5, r5, %lo(.L.str.67)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	bne r1, r0, .LBB178_163
.LBB178_162:
	lui r5, %hi(.L.str.72)
	addi r5, r5, %lo(.L.str.72)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB178_215
.LBB178_163:
	lui r13, %hi(fio_o_pad)
	ldw r1, r13+%lo(fio_o_pad)
	beq r1, r0, .LBB178_176
.LBB178_164:
	lui r3, %hi(fio_o_padlen)
	ldw r3, r3+%lo(fio_o_padlen)
	addi r4, r0, 1
	blt r3, r4, .LBB178_176
.LBB178_165:
	addi r4, r0, 3
	lui r5, %hi(.L.str.74)
	addi r5, r5, %lo(.L.str.74)
	addi r6, r0, 26
	add r7, r0, r0
.LBB178_166:
	beq r7, r4, .LBB178_170
.LBB178_167:
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
	bne r9, r8, .LBB178_211
.LBB178_168:
	addi r7, r7, 1
	bne r3, r7, .LBB178_166
.LBB178_169:
	add r5, r3, r0
	jal r0, .LBB178_171
.LBB178_170:
	add r5, r4, r0
.LBB178_171:
	bne r5, r4, .LBB178_211
.LBB178_172:
	addi r4, r0, 4
	blt r3, r4, .LBB178_176
.LBB178_173:
	addi r1, r1, 3
	addi r3, r3, -3
	addi r4, r0, 32
.LBB178_174:
	ldbu r5, r1+0
	bne r5, r4, .LBB178_211
.LBB178_175:
	addi r1, r1, 1
	addi r3, r3, -1
	bne r3, r0, .LBB178_174
.LBB178_176:
	lui r26, %hi(fio_o_delim)
	ldw r11, r26+%lo(fio_o_delim)
	beq r11, r0, .LBB178_193
.LBB178_177:
	lui r1, %hi(fio_o_delimlen)
	ldw r12, r1+%lo(fio_o_delimlen)
	addi r1, r0, 1
	blt r12, r1, .LBB178_193
.LBB178_178:
	addi r1, r0, 10
	lui r3, %hi(.L.str.76)
	addi r3, r3, %lo(.L.str.76)
	addi r4, r0, 26
	add r5, r0, r0
.LBB178_179:
	beq r5, r1, .LBB178_183
.LBB178_180:
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
	bne r7, r6, .LBB178_189
.LBB178_181:
	addi r5, r5, 1
	bne r12, r5, .LBB178_179
.LBB178_182:
	add r3, r12, r0
	jal r0, .LBB178_184
.LBB178_183:
	add r3, r1, r0
.LBB178_184:
	bne r3, r1, .LBB178_189
.LBB178_185:
	addi r1, r0, 39
	addi r3, r0, 11
	blt r12, r3, .LBB178_191
.LBB178_186:
	addi r3, r11, 10
	addi r4, r12, -10
	addi r5, r0, 32
.LBB178_187:
	ldbu r6, r3+0
	bne r6, r5, .LBB178_189
.LBB178_188:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB178_187
	jal r0, .LBB178_191
.LBB178_189:
	lui r5, %hi(.L.str.77)
	addi r5, r5, %lo(.L.str.77)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB178_192
.LBB178_190:
	addi r1, r0, 34
.LBB178_191:
	stw r25+0, r1
	jal r0, .LBB178_193
.LBB178_192:
	lui r5, %hi(.L.str.78)
	addi r5, r5, %lo(.L.str.78)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, fio_str_is
	beq r1, r0, .LBB178_216
.LBB178_193:
	stw r26+%lo(fio_o_delim), r0
	stw r13+%lo(fio_o_pad), r0
	stw r23+%lo(fio_o_act), r0
	stw r24+%lo(fio_o_pos), r0
	add r3, r21, r22
	lui r1, %hi(.L.str.9)
	addi r1, r1, %lo(.L.str.9)
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
	beq r19, r0, .LBB178_207
.LBB178_194:
	addi r1, r0, 1
	add r3, r0, r0
	blt r18, r1, .LBB178_208
.LBB178_195:
	addi r3, r0, 4
	lui r4, %hi(.L.str.52)
	addi r4, r4, %lo(.L.str.52)
	addi r5, r0, 26
	add r6, r0, r0
.LBB178_196:
	beq r6, r3, .LBB178_200
.LBB178_197:
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
	bne r8, r7, .LBB178_207
.LBB178_198:
	addi r6, r6, 1
	bne r18, r6, .LBB178_196
.LBB178_199:
	add r4, r18, r0
	jal r0, .LBB178_201
.LBB178_200:
	add r4, r3, r0
.LBB178_201:
	bne r4, r3, .LBB178_207
.LBB178_202:
	addi r3, r0, 5
	blt r18, r3, .LBB178_206
.LBB178_203:
	addi r3, r19, 4
	addi r4, r18, -4
	addi r5, r0, 32
.LBB178_204:
	ldbu r6, r3+0
	bne r6, r5, .LBB178_207
.LBB178_205:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB178_204
.LBB178_206:
	add r3, r1, r0
	jal r0, .LBB178_208
.LBB178_207:
	add r3, r0, r0
.LBB178_208:
	lui r1, %hi(fio_ubz)
	addi r1, r1, %lo(fio_ubz)
	add r1, r14, r1
	stw r1+0, r3
	add r1, r0, r0
	jal r0, .LBB178_45
.LBB178_209:
	bne r23, r0, .LBB178_44
.LBB178_210:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.65)
	addi r4, r4, %lo(.L.str.65)
	jal r0, .LBB178_212
.LBB178_211:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.75)
	addi r4, r4, %lo(.L.str.75)
.LBB178_212:
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.LBB178_213:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.49)
	addi r4, r4, %lo(.L.str.49)
	jal r0, .LBB178_212
.LBB178_214:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.71)
	addi r4, r4, %lo(.L.str.71)
	jal r0, .LBB178_212
.LBB178_215:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.73)
	addi r4, r4, %lo(.L.str.73)
	jal r0, .LBB178_212
.LBB178_216:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.79)
	addi r4, r4, %lo(.L.str.79)
	jal r0, .LBB178_212
.Lfunc_end178:
	.size	f77_open, .Lfunc_end178-f77_open
                                        # -- End function
	.p2align	2                               # -- Begin function fio_str_is
	.type	fio_str_is,@function
fio_str_is:                             # @fio_str_is
# %bb.0:
	addi sp, sp, -16
	stw sp+0, lr
	addi r1, r0, 1
	blt r4, r1, .LBB179_6
.LBB179_1:
	addi r7, r0, 26
	add r6, r0, r0
.LBB179_2:
	add r8, r5, r6
	ldb r8, r8+0
	beq r8, r0, .LBB179_7
.LBB179_3:
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
	bne r9, r8, .LBB179_8
.LBB179_4:
	addi r6, r6, 1
	bne r4, r6, .LBB179_2
.LBB179_5:
	add r6, r4, r0
	jal r0, .LBB179_7
.LBB179_6:
	add r6, r0, r0
.LBB179_7:
	add r5, r5, r6
	ldbu r5, r5+0
	beq r5, r0, .LBB179_10
.LBB179_8:
	add r1, r0, r0
.LBB179_9:
	ldw lr, sp+0
	addi sp, sp, 16
	jalr r0, r31, 0
.LBB179_10:
	ble r4, r6, .LBB179_9
.LBB179_11:
	add r3, r3, r6
	sub r4, r4, r6
	addi r5, r0, 32
.LBB179_12:
	ldbu r6, r3+0
	bne r6, r5, .LBB179_8
.LBB179_13:
	addi r3, r3, 1
	addi r4, r4, -1
	bne r4, r0, .LBB179_12
	jal r0, .LBB179_9
.Lfunc_end179:
	.size	fio_str_is, .Lfunc_end179-fio_str_is
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
	addi r3, r3, -1
	addi r6, r0, 99
	sltu r3, r3, r6
	sub r3, r0, r3
	and r3, r1, r3
	slli r3, r3, 2
	lui r7, %hi(fio_pend)
	addi r7, r7, %lo(fio_pend)
	add r3, r3, r7
	stw r3+0, r0
	bgtu r1, r6, .LBB180_27
.LBB180_1:
	slli r11, r1, 2
	lui r3, %hi(fio_ufile)
	addi r3, r3, %lo(fio_ufile)
	add r12, r11, r3
	ldw r3, r12+0
	beq r3, r0, .LBB180_27
.LBB180_2:
	addi r7, r0, 1
	beq r4, r0, .LBB180_10
.LBB180_3:
	addi r6, r0, 1
	add r13, r0, r0
	blt r5, r6, .LBB180_11
.LBB180_4:
	addi r7, r0, 6
	lui r8, %hi(.L.str.80)
	addi r8, r8, %lo(.L.str.80)
	addi r9, r0, 26
	add r10, r0, r0
.LBB180_5:
	beq r10, r7, .LBB180_28
.LBB180_6:
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
	bne r13, lr, .LBB180_9
.LBB180_7:
	addi r10, r10, 1
	bne r5, r10, .LBB180_5
.LBB180_8:
	add r8, r5, r0
	beq r8, r7, .LBB180_29
.LBB180_9:
	add r7, r0, r0
.LBB180_10:
	add r13, r0, r0
.LBB180_11:
	add r14, r1, r0
	bne r7, r0, .LBB180_24
.LBB180_12:
	addi r1, r0, 1
	blt r5, r1, .LBB180_34
.LBB180_13:
	addi r1, r0, 4
	lui r6, %hi(.L.str.81)
	addi r6, r6, %lo(.L.str.81)
	addi r7, r0, 26
	add r8, r0, r0
.LBB180_14:
	beq r8, r1, .LBB180_18
.LBB180_15:
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
	bne r10, r9, .LBB180_34
.LBB180_16:
	addi r8, r8, 1
	bne r5, r8, .LBB180_14
.LBB180_17:
	add r6, r5, r0
	jal r0, .LBB180_19
.LBB180_18:
	add r6, r1, r0
.LBB180_19:
	bne r6, r1, .LBB180_34
.LBB180_20:
	addi r1, r0, 5
	blt r5, r1, .LBB180_24
.LBB180_21:
	addi r1, r4, 4
	addi r4, r5, -4
	addi r5, r0, 32
.LBB180_22:
	ldbu r6, r1+0
	bne r6, r5, .LBB180_34
.LBB180_23:
	addi r1, r1, 1
	addi r4, r4, -1
	bne r4, r0, .LBB180_22
.LBB180_24:
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
	beq r13, r0, .LBB180_26
.LBB180_25:
	add r3, r11, r12
	jal r31, remove
.LBB180_26:
	add r1, r11, r12
	stb r1+0, r0
.LBB180_27:
	ldw r15, sp+28
	ldw r14, sp+32
	ldw r13, sp+36
	ldw r12, sp+40
	ldw r11, sp+44
	ldw lr, sp+0
	addi sp, sp, 48
	jalr r0, r31, 0
.LBB180_28:
	add r8, r7, r0
	bne r8, r7, .LBB180_9
.LBB180_29:
	addi r8, r0, 7
	add r7, r6, r0
	add r13, r6, r0
	blt r5, r8, .LBB180_11
.LBB180_30:
	addi r10, r5, -7
	addi r6, r4, 6
	addi r8, r0, 32
	addi r9, r0, 1
.LBB180_31:
	ldbu lr, r6+0
	seq r7, lr, r8
	addi r13, r10, -1
	sltu r10, r13, r10
	bne r10, r9, .LBB180_33
.LBB180_32:
	addi r6, r6, 1
	add r10, r13, r0
	add r13, r7, r0
	beq lr, r8, .LBB180_31
	jal r0, .LBB180_11
.LBB180_33:
	add r13, r7, r0
	jal r0, .LBB180_11
.LBB180_34:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.82)
	addi r4, r4, %lo(.L.str.82)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end180:
	.size	f77_close, .Lfunc_end180-f77_close
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
	bne r3, r0, .LBB181_4
.LBB181_1:
	addi r3, r0, -400
	lui r6, %hi(fio_uend+400)
	addi r6, r6, %lo(fio_uend+400)
	addi r7, r0, -1
.LBB181_2:
	add r8, r3, r6
	stw r8+0, r7
	addi r3, r3, 4
	bne r3, r0, .LBB181_2
.LBB181_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB181_4:
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
	beq r4, r0, .LBB181_21
.LBB181_5:
	addi r25, r0, 1
	blt r5, r25, .LBB181_21
.LBB181_6:
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
.LBB181_7:
	addi r4, sp, 16
	add r4, r4, r3
	ldbu r5, r4+0
	bne r5, r1, .LBB181_9
.LBB181_8:
	stb r4+0, r0
	addi r4, r3, -1
	addi r5, r3, 1
	add r3, r4, r0
	bgt r5, r25, .LBB181_7
.LBB181_9:
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
	beq r1, r0, .LBB181_11
.LBB181_10:
	stb r26+%lo(inq_exist), r25
	add r3, r1, r0
	jal r31, fclose
.LBB181_11:
	lui r12, %hi(fio_ufname+256)
	addi r12, r12, %lo(fio_ufname+256)
	lui r13, %hi(fio_ufile+4)
	addi r13, r13, %lo(fio_ufile+4)
	addi r24, r0, 100
	jal r0, .LBB181_13
.LBB181_12:
	addi r25, r25, 1
	addi r12, r12, 256
	addi r13, r13, 4
	beq r25, r24, .LBB181_16
.LBB181_13:
	ldw r1, r13+0
	beq r1, r0, .LBB181_12
.LBB181_14:
	addi r4, sp, 16
	add r3, r12, r0
	jal r31, strcmp
	bne r1, r0, .LBB181_12
.LBB181_15:
	addi r1, r0, 1
	stb r22+%lo(inq_opened), r1
	stw r23+%lo(inq_unit), r25
.LBB181_16:
	ldbu r1, r22+%lo(inq_opened)
	beq r1, r0, .LBB181_26
.LBB181_17:
	ldw r3, r23+%lo(inq_unit)
	xor r3, r3, r11
	sub r1, r0, r1
	and r1, r3, r1
	xor r11, r11, r1
.LBB181_18:
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
	beq r4, r0, .LBB181_26
.LBB181_19:
	addi r4, r0, 1
	blt r3, r4, .LBB181_26
.LBB181_20:
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
	jal r0, .LBB181_26
.LBB181_21:
	addi r1, r0, 100
	bltu r11, r1, .LBB181_23
.LBB181_22:
	stb r26+%lo(inq_exist), r0
	jal r0, .LBB181_26
.LBB181_23:
	addi r1, r0, 1
	stb r26+%lo(inq_exist), r1
	stw r23+%lo(inq_unit), r11
	addi r3, r0, 6
	bgtu r11, r3, .LBB181_27
.LBB181_24:
	sll r4, r1, r11
	andi r4, r4, 97
	beq r4, r0, .LBB181_27
.LBB181_25:
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
.LBB181_26:
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
.LBB181_27:
	slli r3, r11, 2
	lui r4, %hi(fio_ufile)
	addi r4, r4, %lo(fio_ufile)
	add r3, r3, r4
	ldw r3, r3+0
	beq r3, r0, .LBB181_29
.LBB181_28:
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
	jal r0, .LBB181_18
.LBB181_29:
	stb r22+%lo(inq_opened), r0
	jal r0, .LBB181_26
.Lfunc_end181:
	.size	f77_inquire, .Lfunc_end181-f77_inquire
                                        # -- End function
	.globl	f77_inq                         # -- Begin function f77_inq
	.p2align	2
	.type	f77_inq,@function
f77_inq:                                # @f77_inq
# %bb.0:
	addi r1, r3, -1
	addi r3, r0, 20
	bgtu r1, r3, .LBB182_43
.LBB182_1:
	addi sp, sp, -32
	stw sp+0, lr
	stw sp+28, r11
	stw sp+24, r12
	stw sp+20, r13
	stw sp+16, r14
	add r12, r5, r0
	add r11, r4, r0
	slli r1, r1, 2
	lui r3, %hi(.LJTI182_0)
	addi r3, r3, %lo(.LJTI182_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB182_2:
	lui r1, %hi(inq_exist)
	ldbu r1, r1+%lo(inq_exist)
	jal r0, .LBB182_41
.LBB182_3:
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
	blt r12, r3, .LBB182_42
.LBB182_4:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_6
.LBB182_5:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_6:
	add r5, r3, r0
	bge r4, r1, .LBB182_5
.LBB182_7:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_5
.LBB182_8:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r14, r0, 1
	bne r1, r14, .LBB182_88
.LBB182_9:
	lui r1, %hi(inq_action)
	ldw r1, r1+%lo(inq_action)
	bne r1, r14, .LBB182_89
.LBB182_10:
	lui r13, %hi(.L.str.66)
	addi r13, r13, %lo(.L.str.66)
	jal r0, .LBB182_90
.LBB182_11:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB182_42
.LBB182_12:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB182_42
.LBB182_13:
	lui r1, %hi(inq_recl)
	ldw r1, r1+%lo(inq_recl)
	jal r0, .LBB182_41
.LBB182_14:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r14, r0, 1
	bne r1, r14, .LBB182_42
.LBB182_15:
	lui r1, %hi(inq_named)
	ldw r1, r1+%lo(inq_named)
	beq r1, r0, .LBB182_42
.LBB182_16:
	lui r13, %hi(inq_name)
	addi r13, r13, %lo(inq_name)
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB182_42
.LBB182_17:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_19
.LBB182_18:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_19:
	add r5, r3, r0
	bge r4, r1, .LBB182_18
.LBB182_20:
	add r5, r4, r13
	ldbu r5, r5+0
	jal r0, .LBB182_18
.LBB182_21:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	addi r3, r0, 1
	bne r1, r3, .LBB182_42
.LBB182_22:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	beq r1, r0, .LBB182_42
.LBB182_23:
	lui r1, %hi(inq_nextrec)
	ldw r1, r1+%lo(inq_nextrec)
	jal r0, .LBB182_41
.LBB182_24:
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
	blt r12, r3, .LBB182_42
.LBB182_25:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_27
.LBB182_26:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_27:
	add r5, r3, r0
	bge r4, r1, .LBB182_26
.LBB182_28:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_26
.LBB182_29:
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
	jal r0, .LBB182_41
.LBB182_30:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r13, %hi(.L.str.86)
	addi r13, r13, %lo(.L.str.86)
	addi r14, r0, 1
	bne r1, r14, .LBB182_96
.LBB182_31:
	lui r1, %hi(inq_direct)
	ldw r1, r1+%lo(inq_direct)
	bne r1, r0, .LBB182_96
.LBB182_32:
	lui r1, %hi(inq_posn)
	ldw r1, r1+%lo(inq_posn)
	addi r3, r0, 2
	bne r1, r3, .LBB182_95
.LBB182_33:
	lui r13, %hi(.L.str.68)
	addi r13, r13, %lo(.L.str.68)
	jal r0, .LBB182_96
.LBB182_34:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r3, %hi(inq_named)
	ldw r3, r3+%lo(inq_named)
	sub r1, r0, r1
	and r1, r3, r1
	jal r0, .LBB182_41
.LBB182_35:
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
	blt r12, r3, .LBB182_42
.LBB182_36:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_38
.LBB182_37:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_38:
	add r5, r3, r0
	bge r4, r1, .LBB182_37
.LBB182_39:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_37
.LBB182_40:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
.LBB182_41:
	stw r11+0, r1
.LBB182_42:
	ldw r14, sp+16
	ldw r13, sp+20
	ldw r12, sp+24
	ldw r11, sp+28
	ldw lr, sp+0
	addi sp, sp, 32
.LBB182_43:
	jalr r0, r31, 0
.LBB182_44:
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
	blt r12, r14, .LBB182_42
.LBB182_45:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_47
.LBB182_46:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_47:
	add r5, r3, r0
	bge r4, r1, .LBB182_46
.LBB182_48:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_46
.LBB182_49:
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
	blt r12, r3, .LBB182_42
.LBB182_50:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_52
.LBB182_51:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_52:
	add r5, r3, r0
	bge r4, r1, .LBB182_51
.LBB182_53:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_51
.LBB182_54:
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
	blt r12, r3, .LBB182_42
.LBB182_55:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_57
.LBB182_56:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_57:
	add r5, r3, r0
	bge r4, r1, .LBB182_56
.LBB182_58:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_56
.LBB182_59:
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
	blt r12, r3, .LBB182_42
.LBB182_60:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_62
.LBB182_61:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_62:
	add r5, r3, r0
	bge r4, r1, .LBB182_61
.LBB182_63:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_61
.LBB182_64:
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
	blt r12, r3, .LBB182_42
.LBB182_65:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_67
.LBB182_66:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_67:
	add r5, r3, r0
	bge r4, r1, .LBB182_66
.LBB182_68:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_66
.LBB182_69:
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
	blt r12, r3, .LBB182_42
.LBB182_70:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_72
.LBB182_71:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_72:
	add r5, r3, r0
	bge r4, r1, .LBB182_71
.LBB182_73:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_71
.LBB182_74:
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
	blt r12, r3, .LBB182_42
.LBB182_75:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_77
.LBB182_76:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_77:
	add r5, r3, r0
	bge r4, r1, .LBB182_76
.LBB182_78:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_76
.LBB182_79:
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
	blt r12, r3, .LBB182_42
.LBB182_80:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_82
.LBB182_81:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_82:
	add r5, r3, r0
	bge r4, r1, .LBB182_81
.LBB182_83:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_81
.LBB182_84:
	lui r1, %hi(inq_opened)
	ldbu r1, r1+%lo(inq_opened)
	lui r13, %hi(.L.str.86)
	addi r13, r13, %lo(.L.str.86)
	addi r14, r0, 1
	bne r1, r14, .LBB182_102
.LBB182_85:
	lui r1, %hi(inq_unf)
	ldw r1, r1+%lo(inq_unf)
	bne r1, r0, .LBB182_102
.LBB182_86:
	lui r1, %hi(inq_delim)
	ldw r1, r1+%lo(inq_delim)
	addi r3, r0, 39
	bne r1, r3, .LBB182_101
.LBB182_87:
	lui r13, %hi(.L.str.76)
	addi r13, r13, %lo(.L.str.76)
	jal r0, .LBB182_102
.LBB182_88:
	lui r13, %hi(.L.str.86)
	addi r13, r13, %lo(.L.str.86)
	jal r0, .LBB182_90
.LBB182_89:
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
.LBB182_90:
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB182_42
.LBB182_91:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_93
.LBB182_92:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_93:
	add r5, r3, r0
	bge r4, r1, .LBB182_92
.LBB182_94:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_92
.LBB182_95:
	seq r1, r1, r14
	lui r3, %hi(.L.str.70)
	addi r3, r3, %lo(.L.str.70)
	lui r4, %hi(.L.str.69)
	addi r4, r4, %lo(.L.str.69)
	xor r4, r4, r3
	sub r1, r0, r1
	and r1, r4, r1
	xor r13, r3, r1
.LBB182_96:
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB182_42
.LBB182_97:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_99
.LBB182_98:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_99:
	add r5, r3, r0
	bge r4, r1, .LBB182_98
.LBB182_100:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_98
.LBB182_101:
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
.LBB182_102:
	add r3, r13, r0
	jal r31, strlen
	blt r12, r14, .LBB182_42
.LBB182_103:
	addi r3, r0, 32
	add r4, r0, r0
	jal r0, .LBB182_105
.LBB182_104:
	add r6, r11, r4
	stb r6+0, r5
	addi r4, r4, 1
	beq r12, r4, .LBB182_42
.LBB182_105:
	add r5, r3, r0
	bge r4, r1, .LBB182_104
.LBB182_106:
	add r5, r13, r4
	ldbu r5, r5+0
	jal r0, .LBB182_104
.Lfunc_end182:
	.size	f77_inq, .Lfunc_end182-f77_inq
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI182_0,@object
.LJTI182_0:
	.word	.LBB182_2
	.word	.LBB182_40
	.word	.LBB182_29
	.word	.LBB182_34
	.word	.LBB182_14
	.word	.LBB182_54
	.word	.LBB182_64
	.word	.LBB182_35
	.word	.LBB182_79
	.word	.LBB182_24
	.word	.LBB182_74
	.word	.LBB182_11
	.word	.LBB182_21
	.word	.LBB182_59
	.word	.LBB182_8
	.word	.LBB182_30
	.word	.LBB182_3
	.word	.LBB182_44
	.word	.LBB182_69
	.word	.LBB182_84
	.word	.LBB182_49
	.size	.LJTI182_0, 84
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
	bne r3, r0, .LBB183_4
.LBB183_1:
	addi r3, r0, -400
	lui r4, %hi(fio_uend+400)
	addi r4, r4, %lo(fio_uend+400)
	addi r5, r0, -1
.LBB183_2:
	add r6, r3, r4
	stw r6+0, r5
	addi r3, r3, 4
	bne r3, r0, .LBB183_2
.LBB183_3:
	addi r3, r0, 1
	stb r1+%lo(fio_uend_init), r3
.LBB183_4:
	addi r1, r11, -100
	addi r3, r0, -99
	bltu r1, r3, .LBB183_7
.LBB183_5:
	addi r1, r11, -5
	addi r3, r0, 1
	bleu r1, r3, .LBB183_7
.LBB183_6:
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
.LBB183_7:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.91)
	addi r4, r4, %lo(.L.str.91)
	add r5, r11, r0
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end183:
	.size	f77_rewind, .Lfunc_end183-f77_rewind
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
	beq r3, r16, .LBB184_197
.LBB184_1:
	add r13, r4, r0
	add r12, r3, r0
	lui lr, %hi(fio_ftext)
	addi r11, r0, -1
	stw lr+%lo(fio_ftext), r11
	addi r20, r0, 32
	add r1, r0, r0
	jal r0, .LBB184_3
.LBB184_2:
	add r1, r12, r28
	ldbu r3, r1+0
	addi r1, r28, 1
	bne r3, r20, .LBB184_5
.LBB184_3:
	add r28, r1, r0
	blt r13, r0, .LBB184_2
.LBB184_4:
	bltu r28, r13, .LBB184_2
.LBB184_5:
	lui r14, %hi(.L.str.98)
	addi r14, r14, %lo(.L.str.98)
	lui r15, %hi(fio_fmt_why)
	blt r13, r0, .LBB184_7
.LBB184_6:
	bgeu r28, r13, .LBB184_199
.LBB184_7:
	add r1, r12, r28
	ldbu r1, r1+0
	addi r3, r0, 40
	stw sp+52, r3
	bne r1, r3, .LBB184_199
.LBB184_8:
	addi r23, r0, 1022
	lui r19, %hi(.L.str.99)
	addi r19, r19, %lo(.L.str.99)
	addi r1, r0, 1
	stw sp+48, r1
	slli r1, r1, 2
	lui r24, %hi(.LJTI184_1)
	addi r24, r24, %lo(.LJTI184_1)
	add r22, r24, r1
	addi r26, r0, 39
	addi r9, r0, 246
	addi r10, r0, 41
	addi r14, r0, 0
	add r21, r0, r0
	add r25, r0, r0
	lui r18, %hi(fio_fbuf+1)
	addi r18, r18, %lo(fio_fbuf+1)
.LBB184_9:
	blt r13, r0, .LBB184_13
.LBB184_10:
	add r17, r0, r0
	bltu r28, r13, .LBB184_13
.LBB184_11:
	blt r21, r23, .LBB184_14
.LBB184_12:
	stw r15+%lo(fio_fmt_why), r19
	ldw r1, r22+0
	jalr r0, r1, 0
.LBB184_13:
	add r1, r12, r28
	ldbu r17, r1+0
	bge r21, r23, .LBB184_12
.LBB184_14:
	andi r27, r17, 255
	bgtu r27, r26, .LBB184_38
.LBB184_15:
	slli r1, r27, 2
	lui r3, %hi(.LJTI184_0)
	addi r3, r3, %lo(.LJTI184_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB184_16:
	add r1, r21, r0
	add r5, r26, r0
	jal r0, .LBB184_18
.LBB184_17:
	ldbu r5, r4+0
	add r1, r21, r0
.LBB184_18:
	add r3, r28, r0
.LBB184_19:
	add r4, r1, r16
	stb r4+0, r5
	addi r28, r3, 1
	blt r13, r0, .LBB184_21
.LBB184_20:
	add r4, r0, r0
	bgeu r28, r13, .LBB184_22
.LBB184_21:
	add r4, r12, r28
	ldbu r4, r4+0
.LBB184_22:
	addi r21, r1, 1
	andi r4, r4, 255
	beq r4, r0, .LBB184_35
.LBB184_23:
	addi r5, r0, 1020
	bge r1, r5, .LBB184_35
.LBB184_24:
	bne r4, r27, .LBB184_29
.LBB184_25:
	addi r28, r3, 2
	blt r13, r0, .LBB184_27
.LBB184_26:
	add r3, r0, r0
	bgeu r28, r13, .LBB184_28
.LBB184_27:
	add r3, r12, r28
	ldbu r3, r3+0
.LBB184_28:
	andi r3, r3, 255
	bne r3, r27, .LBB184_46
.LBB184_29:
	add r4, r12, r28
	blt r13, r0, .LBB184_31
.LBB184_30:
	bgeu r28, r13, .LBB184_33
.LBB184_31:
	ldbu r5, r4+0
	addi r3, r0, 39
	bne r5, r3, .LBB184_33
.LBB184_32:
	addi r21, r1, 2
	add r1, r1, r18
	stb r1+0, r3
.LBB184_33:
	blt r13, r0, .LBB184_17
.LBB184_34:
	add r1, r21, r0
	add r5, r0, r0
	add r3, r28, r0
	bgeu r28, r13, .LBB184_19
	jal r0, .LBB184_17
.LBB184_35:
	lui r1, %hi(.L.str.101)
	addi r1, r1, %lo(.L.str.101)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB184_36:
	addi r28, r28, 1
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB184_37:
	lui r1, %hi(.L.str.100)
	addi r1, r1, %lo(.L.str.100)
	stw r15+%lo(fio_fmt_why), r1
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB184_38:
	addi r1, r17, -58
	andi r1, r1, 255
	bltu r1, r9, .LBB184_49
.LBB184_39:
	beq r21, r0, .LBB184_47
.LBB184_40:
	ldw r3, lr+%lo(fio_ftext)
	add r1, r0, r0
	beq r21, r3, .LBB184_48
.LBB184_41:
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
	blt r21, r3, .LBB184_43
.LBB184_42:
	lui r1, %hi(fio_fbuf-2)
	addi r1, r1, %lo(fio_fbuf-2)
	add r1, r21, r1
	ldbu r3, r1+0
	add r14, r8, r0
	jal r31, toupper
	add r8, r14, r0
	addi r3, r0, 66
	sne r1, r1, r3
.LBB184_43:
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
	bgtu r3, r4, .LBB184_84
.LBB184_44:
	addi r5, r0, 1
	sll r6, r5, r3
	lui r7, 165
	addi r7, r7, -1669
	and r6, r6, r7
	beq r6, r0, .LBB184_83
.LBB184_45:
	add r1, r5, r0
	jal r0, .LBB184_48
.LBB184_46:
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
.LBB184_47:
	add r1, r0, r0
.LBB184_48:
	beq r1, r0, .LBB184_53
.LBB184_49:
	addi r1, r21, 1
	add r3, r21, r16
	stb r3+0, r17
	addi r28, r28, 1
	ldw r3, sp+52
	seq r3, r27, r3
	add r25, r25, r3
	bne r27, r10, .LBB184_51
.LBB184_50:
	addi r25, r25, -1
	seq r3, r25, r14
	sub r3, r0, r3
	andi r5, r3, 4
	add r21, r1, r0
	jal r0, .LBB184_52
.LBB184_51:
	add r21, r1, r0
	add r5, r0, r0
.LBB184_52:
	slli r1, r5, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB184_53:
	add r1, r28, r0
	add r4, r0, r0
	jal r0, .LBB184_57
.LBB184_54:
	ldbu r6, r3+0
.LBB184_55:
	addi r3, r0, 10
	mul r3, r4, r3
	add r3, r3, r5
	addi r4, r3, -48
	addi r3, r21, 1
	add r5, r21, r16
	stb r5+0, r6
	add r21, r3, r0
.LBB184_56:
	addi r1, r1, 1
.LBB184_57:
	add r3, r12, r1
	blt r13, r0, .LBB184_59
.LBB184_58:
	bgeu r1, r13, .LBB184_60
.LBB184_59:
	ldbu r5, r3+0
	addi r5, r5, -58
	andi r5, r5, 255
	addi r6, r0, 245
	bgtu r5, r6, .LBB184_63
.LBB184_60:
	blt r13, r0, .LBB184_62
.LBB184_61:
	bgeu r1, r13, .LBB184_72
.LBB184_62:
	ldbu r5, r3+0
	bne r5, r20, .LBB184_72
.LBB184_63:
	blt r13, r0, .LBB184_65
.LBB184_64:
	bgeu r1, r13, .LBB184_66
.LBB184_65:
	ldbu r5, r3+0
	beq r5, r20, .LBB184_56
.LBB184_66:
	bge r21, r23, .LBB184_82
.LBB184_67:
	blt r13, r0, .LBB184_69
.LBB184_68:
	add r5, r0, r0
	bgeu r1, r13, .LBB184_70
.LBB184_69:
	ldbu r5, r3+0
.LBB184_70:
	blt r13, r0, .LBB184_54
.LBB184_71:
	add r6, r0, r0
	bgeu r1, r13, .LBB184_55
	jal r0, .LBB184_54
.LBB184_72:
	blt r13, r0, .LBB184_74
.LBB184_73:
	bgeu r1, r13, .LBB184_75
.LBB184_74:
	ldbu r5, r3+0
	addi r6, r0, 72
	beq r5, r6, .LBB184_78
.LBB184_75:
	addi r5, r0, 5
	blt r13, r0, .LBB184_77
.LBB184_76:
	bgeu r1, r13, .LBB184_81
.LBB184_77:
	ldbu r6, r3+0
	addi r7, r0, 104
	bne r6, r7, .LBB184_81
.LBB184_78:
	beq r4, r0, .LBB184_85
.LBB184_79:
	add r5, r4, r21
	addi r6, r0, 1022
	blt r5, r6, .LBB184_86
.LBB184_80:
	lui r3, %hi(.L.str.102)
	addi r3, r3, %lo(.L.str.102)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB184_81:
	add r28, r1, r0
	jal r0, .LBB184_52
.LBB184_82:
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
.LBB184_83:
	beq r3, r4, .LBB184_48
.LBB184_84:
	addi r1, r0, 46
	seq r1, r8, r1
	jal r0, .LBB184_48
.LBB184_85:
	lui r3, %hi(.L.str.103)
	addi r3, r3, %lo(.L.str.103)
	stw r15+%lo(fio_fmt_why), r3
	addi r3, r0, 1
	slli r3, r3, 2
	add r3, r24, r3
	ldw r3, r3+0
	add r28, r1, r0
	jalr r0, r3, 0
.LBB184_86:
	blt r13, r0, .LBB184_88
.LBB184_87:
	add r5, r0, r0
	bgeu r1, r13, .LBB184_89
.LBB184_88:
	ldbu r5, r3+0
.LBB184_89:
	add r6, r21, r16
	stb r6+0, r5
	addi r21, r21, 1
	addi r5, r0, 1
	blt r4, r5, .LBB184_99
.LBB184_90:
	addi r6, r4, 1
	add r4, r0, r0
	jal r0, .LBB184_93
.LBB184_91:
	ldbu r8, r7+1
.LBB184_92:
	add r7, r21, r4
	add r7, r7, r16
	stb r7+0, r8
	addi r6, r6, -1
	addi r4, r4, 1
	ble r6, r5, .LBB184_100
.LBB184_93:
	add r7, r1, r4
	addi r28, r7, 1
	blt r13, r0, .LBB184_95
.LBB184_94:
	bgeu r28, r13, .LBB184_98
.LBB184_95:
	add r7, r3, r4
	ldbu r8, r7+1
	beq r8, r0, .LBB184_98
.LBB184_96:
	blt r13, r0, .LBB184_91
.LBB184_97:
	add r8, r0, r0
	bgeu r28, r13, .LBB184_92
	jal r0, .LBB184_91
.LBB184_98:
	lui r1, %hi(.L.str.104)
	addi r1, r1, %lo(.L.str.104)
	stw r15+%lo(fio_fmt_why), r1
	add r21, r21, r4
	addi r1, r0, 1
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB184_99:
	addi r28, r1, 1
	jal r0, .LBB184_101
.LBB184_100:
	add r1, r1, r4
	addi r28, r1, 1
	add r21, r21, r4
.LBB184_101:
	stw lr+%lo(fio_ftext), r21
	addi r1, r0, 5
	slli r1, r1, 2
	add r1, r24, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB184_102:
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
	jal r0, .LBB184_104
.LBB184_103:
	addi r14, r14, 1
.LBB184_104:
	add r1, r14, r16
	ldbu r1, r1+0
	bgtu r1, r19, .LBB184_116
.LBB184_105:
	slli r3, r1, 2
	lui r4, %hi(.LJTI184_2)
	addi r4, r4, %lo(.LJTI184_2)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB184_106:
	addi fp, r14, 1
	add r1, r14, r18
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB184_112
.LBB184_107:
	addi r20, r0, -1
	jal r0, .LBB184_114
.LBB184_108:
	addi r14, r14, 1
.LBB184_109:
	add r1, r14, r0
	add r3, r14, r16
	ldbu r4, r3+0
	addi r14, r14, 1
	addi r3, r0, 39
	bne r4, r3, .LBB184_109
.LBB184_110:
	add r4, r14, r16
	ldbu r4, r4+0
	bne r4, r3, .LBB184_104
.LBB184_111:
	addi r14, r1, 2
	jal r0, .LBB184_109
.LBB184_112:
	add r20, r0, r0
	addi r1, r1, -48
	bgtu r1, r22, .LBB184_114
.LBB184_113:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB184_113
.LBB184_114:
	lui r14, %hi(.L.str.105)
	addi r14, r14, %lo(.L.str.105)
	blt r20, r0, .LBB184_199
.LBB184_115:
	add r1, fp, r16
	ldbu r3, r1+0
	jal r31, toupper
	beq r1, r26, .LBB184_122
	jal r0, .LBB184_199
.LBB184_116:
	slli r1, r1, 24
	srai r1, r1, 24
	addi r3, r1, -58
	bgeu r3, r21, .LBB184_118
.LBB184_117:
	add fp, r14, r0
	add r20, r24, r0
	jal r0, .LBB184_122
.LBB184_118:
	addi r1, r1, -48
	bleu r1, r22, .LBB184_120
.LBB184_119:
	add fp, r14, r0
	add r20, r0, r0
	jal r0, .LBB184_122
.LBB184_120:
	add r20, r0, r0
	add fp, r14, r0
.LBB184_121:
	add r3, fp, r0
	mul r4, r20, r23
	add r20, r4, r1
	addi fp, fp, 1
	add r1, r3, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB184_121
.LBB184_122:
	add r14, fp, r16
	ldbu r3, r14+0
	jal r31, toupper
	bne r1, r25, .LBB184_125
.LBB184_123:
	blt r20, r0, .LBB184_198
.LBB184_124:
	add r1, fp, r20
	addi r14, r1, 1
	jal r0, .LBB184_104
.LBB184_125:
	blt r20, r0, .LBB184_128
.LBB184_126:
	ldw r3, sp+52
	bne r1, r3, .LBB184_128
.LBB184_127:
	addi r14, fp, 1
	jal r0, .LBB184_104
.LBB184_128:
	beq r1, r28, .LBB184_139
.LBB184_129:
	sgt r3, r20, r24
	seq r4, r1, r26
	and r3, r3, r4
	bne r3, r0, .LBB184_139
.LBB184_130:
	addi r17, r1, -65
	ldw r3, sp+48
	bgtu r17, r3, .LBB184_160
.LBB184_131:
	slli r3, r17, 2
	lui r4, %hi(.LJTI184_3)
	addi r4, r4, %lo(.LJTI184_3)
	add r3, r4, r3
	ldw r3, r3+0
	jalr r0, r3, 0
.LBB184_132:
	addi r14, fp, 1
	add r3, fp, r18
	ldb r3, r3+0
	addi r3, r3, -58
	bltu r3, r21, .LBB184_134
.LBB184_133:
	add r3, r14, r0
	addi r14, r14, 1
	add r3, r3, r18
	ldb r3, r3+0
	addi r3, r3, -58
	addi r4, r0, -11
	bgtu r3, r4, .LBB184_133
.LBB184_134:
	addi r3, r0, 65
	beq r1, r3, .LBB184_104
.LBB184_135:
	add r1, r14, r16
	ldbu r1, r1+0
	addi r3, r0, 46
	bne r1, r3, .LBB184_104
.LBB184_136:
	addi r1, r14, 1
	add r3, r14, r18
	ldb r3, r3+0
	addi r3, r3, -58
	add r14, r1, r0
	bltu r3, r21, .LBB184_104
.LBB184_137:
	add r14, r1, r0
.LBB184_138:
	add r1, r14, r0
	addi r14, r14, 1
	add r1, r1, r18
	ldb r1, r1+0
	addi r1, r1, -58
	bgeu r1, r21, .LBB184_138
	jal r0, .LBB184_104
.LBB184_139:
	addi r14, fp, 1
	jal r0, .LBB184_104
.LBB184_140:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	addi r3, r0, 82
	beq r1, r3, .LBB184_142
.LBB184_141:
	addi r3, r0, 76
	bne r1, r3, .LBB184_149
.LBB184_142:
	addi r14, fp, 2
	jal r0, .LBB184_150
.LBB184_143:
	stw sp+24, r24
	add r24, r28, r0
	add r28, r27, r0
	stw sp+20, r1
	add r27, fp, r18
	ldbu r3, r27+0
	jal r31, toupper
	ldw r3, sp+32
	beq r1, r3, .LBB184_145
.LBB184_144:
	ldbu r3, r27+0
	jal r31, toupper
	addi r3, r0, 90
	bne r1, r3, .LBB184_158
.LBB184_145:
	addi r14, fp, 2
	add r27, r28, r0
	add r28, r24, r0
	ldw r24, sp+24
	jal r0, .LBB184_104
.LBB184_146:
	add r1, fp, r18
	ldbu r3, r1+0
	jal r31, toupper
	ldw r3, sp+36
	beq r1, r3, .LBB184_148
.LBB184_147:
	addi r14, fp, 1
	bne r1, r26, .LBB184_104
.LBB184_148:
	addi r14, fp, 2
	jal r0, .LBB184_104
.LBB184_149:
	addi r14, fp, 1
.LBB184_150:
	add r1, r14, r16
	ldb r1, r1+0
	addi r3, r1, -58
	bgeu r3, r21, .LBB184_152
.LBB184_151:
	addi r1, r0, 1
	jal r0, .LBB184_157
.LBB184_152:
	addi r1, r1, -48
	bleu r1, r22, .LBB184_154
.LBB184_153:
	add r1, r0, r0
	jal r0, .LBB184_157
.LBB184_154:
	add r3, r0, r0
.LBB184_155:
	add r4, r14, r0
	mul r3, r3, r23
	add r3, r3, r1
	addi r14, r14, 1
	add r1, r4, r18
	ldb r1, r1+0
	addi r1, r1, -48
	bltu r1, r23, .LBB184_155
.LBB184_156:
	srli r1, r3, 31
.LBB184_157:
	beq r1, r0, .LBB184_104
	jal r0, .LBB184_214
.LBB184_158:
	ldw r1, sp+20
	add r27, r28, r0
	add r28, r24, r0
	ldw r24, sp+24
	ldw r3, sp+48
	bgtu r17, r3, .LBB184_160
.LBB184_159:
	addi r3, r0, 1
	sll r3, r3, r17
	lui r4, 8196
	addi r4, r4, 3
	and r3, r3, r4
	bne r3, r0, .LBB184_132
.LBB184_160:
	ldw r3, sp+28
	and r3, r1, r3
	ldw r4, sp+44
	beq r3, r4, .LBB184_163
.LBB184_161:
	addi r3, r0, 76
	beq r1, r3, .LBB184_163
.LBB184_162:
	addi r3, r0, 73
	bne r1, r3, .LBB184_216
.LBB184_163:
	addi r17, fp, 1
	ldw r3, sp+40
	bne r1, r3, .LBB184_167
.LBB184_164:
	add r20, r17, r16
	ldbu r3, r20+0
	add r14, r1, r0
	jal r31, toupper
	add r3, r1, r0
	add r1, r14, r0
	ldw r4, sp+36
	beq r3, r4, .LBB184_166
.LBB184_165:
	ldbu r3, r20+0
	jal r31, toupper
	add r3, r1, r0
	add r1, r14, r0
	ldw r4, sp+32
	bne r3, r4, .LBB184_167
.LBB184_166:
	addi r17, fp, 2
.LBB184_167:
	add r3, r17, r16
	ldb r3, r3+0
	addi r4, r3, -58
	bltu r4, r21, .LBB184_212
.LBB184_168:
	addi r3, r3, -48
	bleu r3, r22, .LBB184_170
.LBB184_169:
	add r3, r0, r0
	jal r0, .LBB184_173
.LBB184_170:
	add r4, r0, r0
.LBB184_171:
	add r5, r17, r0
	mul r4, r4, r23
	add r4, r4, r3
	addi r17, r17, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB184_171
.LBB184_172:
	srli r3, r4, 31
.LBB184_173:
	bne r3, r0, .LBB184_212
.LBB184_174:
	add r3, r17, r16
	ldbu r3, r3+0
	addi r4, r0, 46
	bne r3, r4, .LBB184_177
.LBB184_175:
	addi r14, r17, 1
	add r3, r17, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB184_178
.LBB184_176:
	addi r3, r0, 1
	jal r0, .LBB184_183
.LBB184_177:
	add r14, r17, r0
	jal r0, .LBB184_184
.LBB184_178:
	addi r3, r3, -48
	bleu r3, r22, .LBB184_180
.LBB184_179:
	add r3, r0, r0
	jal r0, .LBB184_183
.LBB184_180:
	add r4, r0, r0
.LBB184_181:
	add r5, r14, r0
	mul r4, r4, r23
	add r4, r4, r3
	addi r14, r14, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB184_181
.LBB184_182:
	srli r3, r4, 31
.LBB184_183:
	bne r3, r0, .LBB184_215
.LBB184_184:
	addi r3, r0, 71
	beq r1, r3, .LBB184_186
.LBB184_185:
	andi r1, r1, 78
	ldw r3, sp+44
	bne r1, r3, .LBB184_104
.LBB184_186:
	add r1, r14, r16
	ldbu r3, r1+0
	jal r31, toupper
	ldw r3, sp+40
	bne r1, r3, .LBB184_104
.LBB184_187:
	addi r1, r14, 1
	add r3, r14, r18
	ldb r3, r3+0
	addi r4, r3, -58
	bgeu r4, r21, .LBB184_189
.LBB184_188:
	addi r3, r0, 1
	jal r0, .LBB184_194
.LBB184_189:
	addi r3, r3, -48
	bleu r3, r22, .LBB184_191
.LBB184_190:
	add r14, r1, r0
	add r3, r0, r0
	jal r0, .LBB184_195
.LBB184_191:
	add r4, r0, r0
.LBB184_192:
	add r5, r1, r0
	mul r1, r4, r23
	add r4, r1, r3
	addi r1, r5, 1
	add r3, r5, r18
	ldb r3, r3+0
	addi r3, r3, -48
	bltu r3, r23, .LBB184_192
.LBB184_193:
	srli r3, r4, 31
.LBB184_194:
	add r14, r1, r0
.LBB184_195:
	beq r3, r0, .LBB184_104
.LBB184_196:
	lui r14, %hi(.L.str.110)
	addi r14, r14, %lo(.L.str.110)
	jal r0, .LBB184_199
.LBB184_197:
	lui r1, %hi(fio_fmt)
	stw r1+%lo(fio_fmt), r16
	add r11, r0, r0
	jal r0, .LBB184_211
.LBB184_198:
	add r14, r27, r0
.LBB184_199:
	stw r15+%lo(fio_fmt_why), r14
.LBB184_200:
	addi r1, r0, 79
	addi r3, r0, 40
	addi r4, r0, 41
	add r6, r0, r0
	add r5, r0, r0
	jal r0, .LBB184_202
.LBB184_201:
	addi r6, r6, 1
.LBB184_202:
	blt r13, r0, .LBB184_204
.LBB184_203:
	add r7, r0, r0
	bgeu r6, r13, .LBB184_205
.LBB184_204:
	add r7, r12, r6
	ldbu r7, r7+0
.LBB184_205:
	andi r7, r7, 255
	beq r7, r0, .LBB184_210
.LBB184_206:
	bgtu r6, r1, .LBB184_210
.LBB184_207:
	seq r8, r7, r3
	add r5, r5, r8
	bne r7, r4, .LBB184_201
.LBB184_208:
	addi r5, r5, -1
	bne r5, r0, .LBB184_201
.LBB184_209:
	addi r6, r6, 1
.LBB184_210:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	ldw r5, r15+%lo(fio_fmt_why)
	lui r4, %hi(.L.str.114)
	addi r4, r4, %lo(.L.str.114)
	add r7, r12, r0
	jal r31, fprintf
.LBB184_211:
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
.LBB184_212:
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.108)
	addi r4, r4, %lo(.L.str.108)
	add r3, r14, r0
	add r5, r1, r0
.LBB184_213:
	jal r31, sprintf
	jal r0, .LBB184_199
.LBB184_214:
	lui r14, %hi(.L.str.107)
	addi r14, r14, %lo(.L.str.107)
	jal r0, .LBB184_199
.LBB184_215:
	lui r14, %hi(.L.str.109)
	addi r14, r14, %lo(.L.str.109)
	jal r0, .LBB184_199
.LBB184_216:
	blt r20, r0, .LBB184_219
.LBB184_217:
	addi r1, r1, -41
	addi r3, r0, 17
	bgtu r1, r3, .LBB184_220
.LBB184_218:
	slli r1, r1, 2
	lui r3, %hi(.Lswitch.table.fio_fmt_use)
	addi r3, r3, %lo(.Lswitch.table.fio_fmt_use)
	add r1, r1, r3
	ldw r14, r1+0
	jal r0, .LBB184_199
.LBB184_219:
	ldb r5, r14+0
	lui r14, %hi(fio_fmt_check.why)
	addi r14, r14, %lo(fio_fmt_check.why)
	lui r4, %hi(.L.str.113)
	addi r4, r4, %lo(.L.str.113)
	add r3, r14, r0
	jal r0, .LBB184_213
.LBB184_220:
	lui r14, %hi(.L.str.112)
	addi r14, r14, %lo(.L.str.112)
	jal r0, .LBB184_199
.LBB184_221:
.Lfunc_end184:
	.size	fio_fmt_use, .Lfunc_end184-fio_fmt_use
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
	.type	.LJTI184_0,@object
.LJTI184_0:
	.word	.LBB184_37
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_36
	.word	.LBB184_38
	.word	.LBB184_16
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_38
	.word	.LBB184_16
	.size	.LJTI184_0, 160
	.type	.LJTI184_1,@object
.LJTI184_1:
	.word	.LBB184_9
	.word	.LBB184_200
	.word	.LBB184_221
	.word	.LBB184_221
	.word	.LBB184_102
	.word	.LBB184_9
	.size	.LJTI184_1, 24
	.type	.LJTI184_2,@object
.LJTI184_2:
	.word	.LBB184_197
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_108
	.word	.LBB184_103
	.word	.LBB184_103
	.word	.LBB184_116
	.word	.LBB184_106
	.word	.LBB184_103
	.word	.LBB184_106
	.word	.LBB184_116
	.word	.LBB184_103
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_116
	.word	.LBB184_103
	.size	.LJTI184_2, 236
	.type	.LJTI184_3,@object
.LJTI184_3:
	.word	.LBB184_132
	.word	.LBB184_143
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_132
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_146
	.word	.LBB184_140
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_160
	.word	.LBB184_132
	.size	.LJTI184_3, 104
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
	bleu r5, r6, .LBB185_2
.LBB185_1:
	add r1, r0, r0
	jal r0, .LBB185_5
.LBB185_2:
	addi r6, r1, 1
	addi r7, r0, 10
	add r1, r0, r0
.LBB185_3:
	mul r1, r1, r7
	addi r8, r4, 1
	add r1, r5, r1
	add r4, r6, r4
	ldb r4, r4+0
	addi r5, r4, -48
	add r4, r8, r0
	bltu r5, r7, .LBB185_3
.LBB185_4:
	stw r3+%lo(fio_pos), r8
.LBB185_5:
	jalr r0, r31, 0
.Lfunc_end185:
	.size	fio_number, .Lfunc_end185-fio_number
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
	beq r1, r0, .LBB186_3
.LBB186_1:
	lui r11, %hi(fio_irec)
	ldw r3, r11+%lo(fio_irec)
	lui r4, %hi(fio_inrec)
	ldw r4, r4+%lo(fio_inrec)
	bge r3, r4, .LBB186_28
.LBB186_2:
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
	jal r0, .LBB186_27
.LBB186_3:
	lui r12, %hi(fio_unit)
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	addi r13, r0, 98
	lui r11, %hi(fio_in)
	bgtu r3, r13, .LBB186_7
.LBB186_4:
	slli r1, r1, 2
	lui r14, %hi(fio_uend)
	addi r14, r14, %lo(fio_uend)
	add r1, r1, r14
	ldw r1, r1+0
	blt r1, r0, .LBB186_7
.LBB186_5:
	ldw r3, r11+%lo(fio_in)
	beq r3, r0, .LBB186_7
.LBB186_6:
	jal r31, ftell
	ldw r3, r12+%lo(fio_unit)
	slli r3, r3, 2
	add r4, r3, r14
	ldw r4, r4+0
	bge r1, r4, .LBB186_30
.LBB186_7:
	ldw r1, r12+%lo(fio_unit)
	addi r3, r1, -1
	bgtu r3, r13, .LBB186_11
.LBB186_8:
	slli r1, r1, 2
	lui r3, %hi(fio_udirect)
	addi r3, r3, %lo(fio_udirect)
	add r3, r1, r3
	ldw r3, r3+0
	beq r3, r0, .LBB186_11
.LBB186_9:
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
	beq r3, r0, .LBB186_29
.LBB186_10:
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB186_26
.LBB186_11:
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
.LBB186_12:
	add r18, r3, r0
	add r3, r11, r0
	jal r31, getc
	beq r1, r16, .LBB186_17
.LBB186_13:
	addi r3, r18, 1
	add r4, r18, r15
	stb r4+0, r1
	beq r1, r14, .LBB186_15
.LBB186_14:
	bltu r18, r17, .LBB186_12
.LBB186_15:
	beq r3, r0, .LBB186_18
.LBB186_16:
	add r1, r3, r15
	stb r1+0, r0
	lui r1, %hi(fio_rlen)
	stw r1+%lo(fio_rlen), r3
	jal r0, .LBB186_20
.LBB186_17:
	add r3, r18, r0
	bne r3, r0, .LBB186_16
.LBB186_18:
	ldw r1, r12+%lo(fio_unit)
	addi r4, r1, -1
	bgtu r4, r13, .LBB186_20
.LBB186_19:
	slli r1, r1, 2
	lui r4, %hi(fio_uaeof)
	addi r4, r4, %lo(fio_uaeof)
	add r1, r1, r4
	addi r4, r0, 1
	stw r1+0, r4
.LBB186_20:
	beq r3, r0, .LBB186_28
.LBB186_21:
	lui r1, %hi(fio_rlen)
	ldw r4, r1+%lo(fio_rlen)
	addi r3, r0, 1
	blt r4, r3, .LBB186_26
.LBB186_22:
	addi r4, r4, 1
	lui r5, %hi(fio_rec-2)
	addi r5, r5, %lo(fio_rec-2)
	addi r6, r0, 13
	jal r0, .LBB186_24
.LBB186_23:
	addi r7, r4, -2
	stw r1+%lo(fio_rlen), r7
	addi r4, r4, -1
	ble r4, r3, .LBB186_26
.LBB186_24:
	add r7, r4, r5
	ldbu r7, r7+0
	beq r7, r6, .LBB186_23
.LBB186_25:
	beq r7, r14, .LBB186_23
.LBB186_26:
	lui r1, %hi(fio_rpos)
	stw r1+%lo(fio_rpos), r0
.LBB186_27:
	add r1, r0, r0
	jal r0, .LBB186_29
.LBB186_28:
	addi r1, r0, 1
.LBB186_29:
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
.LBB186_30:
	lui r1, %hi(fio_uaeof)
	addi r1, r1, %lo(fio_uaeof)
	add r3, r3, r1
	addi r1, r0, 1
	stw r3+0, r1
	jal r0, .LBB186_29
.Lfunc_end186:
	.size	fio_getrec, .Lfunc_end186-fio_getrec
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
	ble r1, r3, .LBB187_5
.LBB187_1:
	addi r4, r0, 0
	seq r4, r3, r4
	slli r3, r3, 1
	lui r5, 1
	xor r5, r3, r5
	sub r4, r0, r4
	and r4, r5, r4
	xor r3, r3, r4
.LBB187_2:
	add r13, r3, r0
	slli r3, r3, 1
	blt r13, r1, .LBB187_2
.LBB187_3:
	ldw r3, r15+%lo(nml_text)
	add r4, r13, r0
	jal r31, realloc
	beq r1, r0, .LBB187_6
.LBB187_4:
	stw r15+%lo(nml_text), r1
	stw r16+%lo(nml_tcap), r13
.LBB187_5:
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
.LBB187_6:
	lui r1, %hi(stderr)
	ldw r3, r1+%lo(stderr)
	lui r4, %hi(.L.str.131)
	addi r4, r4, %lo(.L.str.131)
	jal r31, fprintf
	addi r3, r0, 2
	jal r31, exit
.Lfunc_end187:
	.size	nml_text_add, .Lfunc_end187-nml_text_add
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
	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"%12d"
	.size	.L.str.4, 5

	.type	fio_first_item,@object          # @fio_first_item
	.local	fio_first_item
	.comm	fio_first_item,1,4
	.type	fio_prev_char,@object           # @fio_prev_char
	.local	fio_prev_char
	.comm	fio_prev_char,1,4
	.type	fio_w,@object                   # @fio_w
	.local	fio_w
	.comm	fio_w,4,4
	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"0123456789ABCDEF"
	.size	.L.str.5, 17

	.type	fio_d,@object                   # @fio_d
	.local	fio_d
	.comm	fio_d,4,4
	.type	fio_fmt,@object                 # @fio_fmt
	.local	fio_fmt
	.comm	fio_fmt,4,4
	.type	fio_pos,@object                 # @fio_pos
	.local	fio_pos
	.comm	fio_pos,4,4
	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"%u"
	.size	.L.str.6, 3

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"%s%0*d%s"
	.size	.L.str.7, 9

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"-"
	.size	.L.str.8, 2

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.zero	1
	.size	.L.str.9, 1

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"%s%s"
	.size	.L.str.10, 5

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"%d"
	.size	.L.str.11, 3

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"NaN"
	.size	.L.str.12, 4

	.type	fio_sp,@object                  # @fio_sp
	.local	fio_sp
	.comm	fio_sp,1,4
	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"Infinity"
	.size	.L.str.13, 9

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"Inf"
	.size	.L.str.14, 4

	.type	fio_scale,@object               # @fio_scale
	.local	fio_scale
	.comm	fio_scale,4,4
	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"%%.%df"
	.size	.L.str.15, 7

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"."
	.size	.L.str.16, 2

	.type	fio_e,@object                   # @fio_e
	.local	fio_e
	.comm	fio_e,4,4
	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"%g"
	.size	.L.str.17, 3

	.type	fio_udelim,@object              # @fio_udelim
	.local	fio_udelim
	.comm	fio_udelim,400,4
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

	.type	fio_line,@object                # @fio_line
	.local	fio_line
	.comm	fio_line,1024,1
	.type	.L.str.115,@object              # @.str.115
.L.str.115:
	.asciz	"-Infinity"
	.size	.L.str.115, 10

	.type	.L.str.116,@object              # @.str.116
.L.str.116:
	.asciz	"%%.%dE"
	.size	.L.str.116, 7

	.type	.L.str.117,@object              # @.str.117
.L.str.117:
	.asciz	"%.17E"
	.size	.L.str.117, 6

	.type	.L.str.118,@object              # @.str.118
.L.str.118:
	.asciz	"f77: out of memory for an array constructor\n"
	.size	.L.str.118, 45

	.type	.L.str.119,@object              # @.str.119
.L.str.119:
	.asciz	"f77: a CHARACTER result of %d bytes is too long\n"
	.size	.L.str.119, 49

	.type	fio_gcount,@object              # @fio_gcount
	.local	fio_gcount
	.comm	fio_gcount,64,4
	.type	fio_gstart,@object              # @fio_gstart
	.local	fio_gstart
	.comm	fio_gstart,64,4
	.type	.L.str.120,@object              # @.str.120
.L.str.120:
	.asciz	"f77: FORMAT: no data edit descriptor for the items left\n"
	.size	.L.str.120, 57

	.type	.L.str.121,@object              # @.str.121
.L.str.121:
	.asciz	"f77: WRITE past the end of an internal file\n"
	.size	.L.str.121, 45

	.type	.L.str.122,@object              # @.str.122
.L.str.122:
	.asciz	"f77: record longer than the internal file (%d > %d)\n"
	.size	.L.str.122, 53

	.type	.L.str.123,@object              # @.str.123
.L.str.123:
	.asciz	"f77: record longer than RECL (%d > %d)\n"
	.size	.L.str.123, 40

	.type	fio_l_nulls,@object             # @fio_l_nulls
	.local	fio_l_nulls
	.comm	fio_l_nulls,4,4
	.type	fio_l_after,@object             # @fio_l_after
	.local	fio_l_after
	.comm	fio_l_after,1,4
	.type	fio_l_pending,@object           # @fio_l_pending
	.local	fio_l_pending
	.comm	fio_l_pending,4,4
	.type	.L.str.124,@object              # @.str.124
.L.str.124:
	.asciz	"f77: zero repeat count in list-directed input\n"
	.size	.L.str.124, 47

	.type	.L.str.125,@object              # @.str.125
.L.str.125:
	.asciz	"f77: bad LOGICAL input field '%s'\n"
	.size	.L.str.125, 35

	.type	.L.str.126,@object              # @.str.126
.L.str.126:
	.asciz	"f77: bad digit '%c' in a B, O or Z input field\n"
	.size	.L.str.126, 48

	.type	.L.str.127,@object              # @.str.127
.L.str.127:
	.asciz	"E%d"
	.size	.L.str.127, 4

	.type	.L.str.128,@object              # @.str.128
.L.str.128:
	.asciz	"(A)"
	.size	.L.str.128, 4

	.type	nml_tlen,@object                # @nml_tlen
	.local	nml_tlen
	.comm	nml_tlen,4,4
	.type	.L.str.129,@object              # @.str.129
.L.str.129:
	.asciz	"the record could not be read"
	.size	.L.str.129, 29

	.type	.L.str.130,@object              # @.str.130
.L.str.130:
	.asciz	" "
	.size	.L.str.130, 2

	.type	nml_tcap,@object                # @nml_tcap
	.local	nml_tcap
	.comm	nml_tcap,4,4
	.type	nml_text,@object                # @nml_text
	.local	nml_text
	.comm	nml_text,4,4
	.type	.L.str.131,@object              # @.str.131
.L.str.131:
	.asciz	"f77: out of storage for a NAMELIST\n"
	.size	.L.str.131, 36

	.type	.L.str.132,@object              # @.str.132
.L.str.132:
	.asciz	"a subscript is no integer"
	.size	.L.str.132, 26

	.type	.L.str.133,@object              # @.str.133
.L.str.133:
	.asciz	"a value before the first NAME="
	.size	.L.str.133, 31

	.type	.L.str.134,@object              # @.str.134
.L.str.134:
	.asciz	"a repeat count is not positive"
	.size	.L.str.134, 31

	.type	nml_cval,@object                # @nml_cval
	.local	nml_cval
	.comm	nml_cval,4,4
	.type	nml_casg,@object                # @nml_casg
	.local	nml_casg
	.comm	nml_casg,4,4
	.type	nml_pend,@object                # @nml_pend
	.local	nml_pend
	.comm	nml_pend,1024,1
	.type	.L.str.135,@object              # @.str.135
.L.str.135:
	.asciz	"0.0"
	.size	.L.str.135, 4

	.type	.L.str.136,@object              # @.str.136
.L.str.136:
	.asciz	"(G23.16)"
	.size	.L.str.136, 9

	.type	.L.str.137,@object              # @.str.137
.L.str.137:
	.asciz	"(G15.8)"
	.size	.L.str.137, 8

	.type	.L.str.138,@object              # @.str.138
.L.str.138:
	.asciz	"a LOGICAL value is not T or F"
	.size	.L.str.138, 30

	.type	.L.str.139,@object              # @.str.139
.L.str.139:
	.asciz	"a CHARACTER value is not in quotes"
	.size	.L.str.139, 35

	.type	.L.str.140,@object              # @.str.140
.L.str.140:
	.asciz	"a value could not be read"
	.size	.L.str.140, 26

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
