	.file	"liolib.c"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	2, 0x0                          # -- Begin function luaopen_io
.LCPI0_0:
	.quad	0x407f800000000000              # double 504
	.text
	.globl	luaopen_io
	.p2align	2
	.type	luaopen_io,@function
luaopen_io:                             # @luaopen_io
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 56
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, r19
	stw fp+-40, r20
	stw fp+-44, lr
	add r11, r3, r0
	lui r1, %hi(.LCPI0_0)
	addi r1, r1, %lo(.LCPI0_0)
	ldw r6, r1+4
	ldw r5, r1+0
	addi r4, r0, 72
	jal r31, luaL_checkversion_
	addi r12, r0, 0
	addi r5, r0, 11
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_createtable
	lui r4, %hi(iolib)
	addi r4, r4, %lo(iolib)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_setfuncs
	lui r13, %hi(.L.str.16)
	addi r13, r13, %lo(.L.str.16)
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_newmetatable
	lui r4, %hi(metameth)
	addi r4, r4, %lo(metameth)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_setfuncs
	addi r5, r0, 7
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_createtable
	lui r4, %hi(meth)
	addi r4, r4, %lo(meth)
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_setfuncs
	lui r5, %hi(.L.str.40)
	addi r5, r5, %lo(.L.str.40)
	addi r14, r0, -2
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_setfield
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_settop
	lui r1, %hi(stdin)
	addi r1, r1, %lo(stdin)
	ldw r17, r1+0
	addi r15, r0, 8
	add r3, r11, r0
	add r4, r15, r0
	add r5, r12, r0
	jal r31, lua_newuserdatauv
	add r16, r1, r0
	stw r1+4, r12
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_setmetatable
	stw r16+0, r17
	lui r19, %hi(io_noclose)
	addi r19, r19, %lo(io_noclose)
	stw r16+4, r19
	addi r16, r0, -1
	add r3, r11, r0
	add r4, r16, r0
	jal r31, lua_pushvalue
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	lui r1, 1048572
	addi r17, r1, 384
	add r3, r11, r0
	add r4, r17, r0
	jal r31, lua_setfield
	lui r5, %hi(.L.str.1)
	addi r5, r5, %lo(.L.str.1)
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_setfield
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r20, r1+0
	add r3, r11, r0
	add r4, r15, r0
	add r5, r12, r0
	jal r31, lua_newuserdatauv
	add r18, r1, r0
	stw r1+4, r12
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_setmetatable
	stw r18+0, r20
	stw r18+4, r19
	add r3, r11, r0
	add r4, r16, r0
	jal r31, lua_pushvalue
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	add r3, r11, r0
	add r4, r17, r0
	jal r31, lua_setfield
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_setfield
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r16, r1+0
	add r3, r11, r0
	add r4, r15, r0
	add r5, r12, r0
	jal r31, lua_newuserdatauv
	add r15, r1, r0
	stw r1+4, r12
	add r3, r11, r0
	add r4, r13, r0
	jal r31, luaL_setmetatable
	stw r15+0, r16
	stw r15+4, r19
	lui r5, %hi(.L.str.4)
	addi r5, r5, %lo(.L.str.4)
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_setfield
	addi r1, r0, 1
	ldw lr, fp+-44
	ldw r20, fp+-40
	ldw r19, fp+-36
	ldw r18, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end0:
	.size	luaopen_io, .Lfunc_end0-luaopen_io
                                        # -- End function
	.p2align	2                               # -- Begin function io_close
	.type	io_close,@function
io_close:                               # @io_close
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	addi r4, r0, 1
	jal r31, lua_type
	addi r3, r0, -1
	bne r1, r3, .LBB1_2
.LBB1_1:
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	lui r1, 1048572
	addi r4, r1, 384
	add r3, r11, r0
	jal r31, lua_getfield
.LBB1_2:
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_checkudata
	ldw r1, r1+4
	addi r12, r0, 0
	beq r1, r12, .LBB1_4
.LBB1_3:
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_checkudata
	ldw r3, r1+4
	addi r4, fp, -16
	stw r4+0, r3
	stw r1+4, r12
	ldw r1, r4+0
	add r3, r11, r0
	jalr lr, r1, 0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB1_4:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB1_3
.Lfunc_end1:
	.size	io_close, .Lfunc_end1-io_close
                                        # -- End function
	.p2align	2                               # -- Begin function io_flush
	.type	io_flush,@function
io_flush:                               # @io_flush
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	lui r1, 1048572
	addi r4, r1, 384
	jal r31, lua_getfield
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_touserdata
	ldw r3, r1+4
	addi r12, r0, 0
	beq r3, r12, .LBB2_2
.LBB2_1:
	ldw r3, r1+0
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	stw r1+0, r12
	jal r31, fflush
	seq r4, r1, r12
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_fileresult
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB2_2:
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	lui r5, %hi(.L.str.2+4)
	addi r5, r5, %lo(.L.str.2+4)
	add r3, r11, r0
	add r13, r1, r0
	jal r31, luaL_error
	add r1, r13, r0
	jal r0, .LBB2_1
.Lfunc_end2:
	.size	io_flush, .Lfunc_end2-io_flush
                                        # -- End function
	.p2align	2                               # -- Begin function io_input
	.type	io_input,@function
io_input:                               # @io_input
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	lui r5, %hi(.L.str.19)
	addi r5, r5, %lo(.L.str.19)
	jal r31, g_iofile
	addi r1, r0, 1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end3:
	.size	io_input, .Lfunc_end3-io_input
                                        # -- End function
	.p2align	2                               # -- Begin function io_lines
	.type	io_lines,@function
io_lines:                               # @io_lines
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, lr
	add r11, r3, r0
	addi r4, r0, 1
	jal r31, lua_type
	addi r3, r0, -1
	bne r1, r3, .LBB4_2
.LBB4_1:
	add r3, r11, r0
	jal r31, lua_pushnil
.LBB4_2:
	addi r12, r0, 1
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_type
	add r13, r1, r0
	addi r14, r0, 0
	beq r1, r14, .LBB4_5
.LBB4_3:
	addi r15, r0, 1
	add r3, r11, r0
	add r4, r15, r0
	add r5, r14, r0
	jal r31, luaL_checklstring
	add r16, r1, r0
	addi r4, r0, 8
	add r3, r11, r0
	add r5, r14, r0
	jal r31, lua_newuserdatauv
	add r17, r1, r0
	stw r1+4, r14
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	add r3, r11, r0
	jal r31, luaL_setmetatable
	stw r17+0, r14
	lui r1, %hi(io_fclose)
	addi r1, r1, %lo(io_fclose)
	stw r17+4, r1
	lui r4, %hi(.L.str.19)
	addi r4, r4, %lo(.L.str.19)
	add r3, r16, r0
	jal r31, fopen
	stw r17+0, r1
	beq r1, r14, .LBB4_9
.LBB4_4:
	addi r4, r0, -1
	add r3, r11, r0
	add r5, r15, r0
	jal r31, lua_copy
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	jal r0, .LBB4_6
.LBB4_5:
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	lui r1, 1048572
	addi r4, r1, 384
	add r3, r11, r0
	jal r31, lua_getfield
	addi r4, r0, -1
	addi r15, r0, 1
	add r3, r11, r0
	add r5, r15, r0
	jal r31, lua_copy
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	add r3, r11, r0
	add r4, r15, r0
	jal r31, luaL_checkudata
	ldw r1, r1+4
	add r15, r14, r0
	beq r1, r14, .LBB4_10
.LBB4_6:
	add r3, r11, r0
	add r4, r15, r0
	jal r31, aux_lines
	beq r13, r14, .LBB4_8
.LBB4_7:
	add r3, r11, r0
	jal r31, lua_pushnil
	add r3, r11, r0
	jal r31, lua_pushnil
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_pushvalue
	addi r12, r0, 4
.LBB4_8:
	add r1, r12, r0
	ldw lr, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB4_9:
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	ldw r3, r1+0
	jal r31, strerror
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	add r3, r11, r0
	add r5, r16, r0
	add r6, r1, r0
	jal r31, luaL_error
	jal r0, .LBB4_4
.LBB4_10:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r11, r0
	jal r31, luaL_error
	add r15, r14, r0
	jal r0, .LBB4_6
.Lfunc_end4:
	.size	io_lines, .Lfunc_end4-io_lines
                                        # -- End function
	.p2align	2                               # -- Begin function io_open
	.type	io_open,@function
io_open:                                # @io_open
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 56
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, lr
	add r11, r3, r0
	addi r12, r0, 1
	addi r14, r0, 0
	add r4, r12, r0
	add r5, r14, r0
	jal r31, luaL_checklstring
	add r13, r1, r0
	lui r5, %hi(.L.str.19)
	addi r5, r5, %lo(.L.str.19)
	addi r4, r0, 2
	add r3, r11, r0
	add r6, r14, r0
	jal r31, luaL_optlstring
	add r15, r1, r0
	addi r4, r0, 8
	add r3, r11, r0
	add r5, r14, r0
	jal r31, lua_newuserdatauv
	add r16, r1, r0
	stw r1+4, r14
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	add r3, r11, r0
	jal r31, luaL_setmetatable
	stw r16+0, r14
	lui r1, %hi(io_fclose)
	addi r1, r1, %lo(io_fclose)
	stw r16+4, r1
	ldb r4, r15+0
	beq r4, r14, .LBB5_3
.LBB5_1:
	lui r3, %hi(.L.str.32)
	addi r3, r3, %lo(.L.str.32)
	jal r31, strchr
	beq r1, r14, .LBB5_3
.LBB5_2:
	addi r1, r15, 1
	ldbu r3, r15+1
	addi r4, r0, 43
	seq r3, r3, r4
	addi r4, r15, 2
	xor r4, r4, r1
	sub r3, r14, r3
	and r3, r4, r3
	xor r17, r1, r3
	lui r4, %hi(.L.str.33)
	addi r4, r4, %lo(.L.str.33)
	add r3, r17, r0
	jal r31, strspn
	add r18, r1, r0
	add r3, r17, r0
	jal r31, strlen
	beq r18, r1, .LBB5_4
.LBB5_3:
	lui r5, %hi(.L.str.31)
	addi r5, r5, %lo(.L.str.31)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_argerror
.LBB5_4:
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	stw r1+0, r14
	add r3, r13, r0
	add r4, r15, r0
	jal r31, fopen
	stw r16+0, r1
	bne r1, r14, .LBB5_6
.LBB5_5:
	addi r4, r0, 0
	add r3, r11, r0
	add r5, r13, r0
	jal r31, luaL_fileresult
	add r12, r1, r0
.LBB5_6:
	add r1, r12, r0
	ldw lr, fp+-36
	ldw r18, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end5:
	.size	io_open, .Lfunc_end5-io_open
                                        # -- End function
	.p2align	2                               # -- Begin function io_output
	.type	io_output,@function
io_output:                              # @io_output
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	lui r4, %hi(.L.str.2)
	addi r4, r4, %lo(.L.str.2)
	lui r5, %hi(.L.str.34)
	addi r5, r5, %lo(.L.str.34)
	jal r31, g_iofile
	addi r1, r0, 1
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end6:
	.size	io_output, .Lfunc_end6-io_output
                                        # -- End function
	.p2align	2                               # -- Begin function io_popen
	.type	io_popen,@function
io_popen:                               # @io_popen
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, lr
	add r11, r3, r0
	addi r4, r0, 1
	addi r12, r0, 0
	add r5, r12, r0
	jal r31, luaL_checklstring
	add r13, r1, r0
	lui r5, %hi(.L.str.19)
	addi r5, r5, %lo(.L.str.19)
	addi r4, r0, 2
	add r3, r11, r0
	add r6, r12, r0
	jal r31, luaL_optlstring
	add r15, r1, r0
	addi r4, r0, 8
	add r3, r11, r0
	add r5, r12, r0
	jal r31, lua_newuserdatauv
	add r14, r1, r0
	stw r1+4, r12
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	add r3, r11, r0
	jal r31, luaL_setmetatable
	ldbu r1, r15+0
	addi r3, r0, 119
	beq r1, r3, .LBB7_2
.LBB7_1:
	addi r3, r0, 114
	bne r1, r3, .LBB7_3
.LBB7_2:
	ldbu r1, r15+1
	beq r1, r12, .LBB7_4
.LBB7_3:
	lui r5, %hi(.L.str.31)
	addi r5, r5, %lo(.L.str.31)
	addi r4, r0, 2
	add r3, r11, r0
	jal r31, luaL_argerror
.LBB7_4:
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	stw r1+0, r12
	lui r4, %hi(.L.str.35)
	addi r4, r4, %lo(.L.str.35)
	add r3, r11, r0
	jal r31, luaL_error
	stw r14+0, r12
	lui r1, %hi(io_pclose)
	addi r1, r1, %lo(io_pclose)
	stw r14+4, r1
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	jal r31, luaL_fileresult
	ldw lr, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.Lfunc_end7:
	.size	io_popen, .Lfunc_end7-io_popen
                                        # -- End function
	.p2align	2                               # -- Begin function io_read
	.type	io_read,@function
io_read:                                # @io_read
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	lui r5, %hi(.L.str)
	addi r5, r5, %lo(.L.str)
	lui r1, 1048572
	addi r4, r1, 384
	jal r31, lua_getfield
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_touserdata
	ldw r3, r1+4
	addi r4, r0, 0
	beq r3, r4, .LBB8_2
.LBB8_1:
	ldw r4, r1+0
	addi r5, r0, 1
	add r3, r11, r0
	jal r31, g_read
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB8_2:
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	lui r5, %hi(.L.str+4)
	addi r5, r5, %lo(.L.str+4)
	add r3, r11, r0
	add r12, r1, r0
	jal r31, luaL_error
	add r1, r12, r0
	jal r0, .LBB8_1
.Lfunc_end8:
	.size	io_read, .Lfunc_end8-io_read
                                        # -- End function
	.p2align	2                               # -- Begin function io_tmpfile
	.type	io_tmpfile,@function
io_tmpfile:                             # @io_tmpfile
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, lr
	add r11, r3, r0
	addi r4, r0, 8
	addi r12, r0, 0
	add r5, r12, r0
	jal r31, lua_newuserdatauv
	add r13, r1, r0
	stw r1+4, r12
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	add r3, r11, r0
	jal r31, luaL_setmetatable
	stw r13+0, r12
	lui r1, %hi(io_fclose)
	addi r1, r1, %lo(io_fclose)
	stw r13+4, r1
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	stw r1+0, r12
	jal r31, tmpfile
	stw r13+0, r1
	beq r1, r12, .LBB9_2
.LBB9_1:
	addi r1, r0, 1
	jal r0, .LBB9_3
.LBB9_2:
	addi r4, r0, 0
	add r3, r11, r0
	add r5, r4, r0
	jal r31, luaL_fileresult
.LBB9_3:
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end9:
	.size	io_tmpfile, .Lfunc_end9-io_tmpfile
                                        # -- End function
	.p2align	2                               # -- Begin function io_type
	.type	io_type,@function
io_type:                                # @io_type
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	addi r12, r0, 1
	add r4, r12, r0
	jal r31, luaL_checkany
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	add r3, r11, r0
	add r4, r12, r0
	jal r31, luaL_testudata
	addi r3, r0, 0
	beq r1, r3, .LBB10_3
.LBB10_1:
	ldw r1, r1+4
	beq r1, r3, .LBB10_4
.LBB10_2:
	lui r4, %hi(.L.str.37)
	addi r4, r4, %lo(.L.str.37)
	jal r0, .LBB10_5
.LBB10_3:
	add r3, r11, r0
	jal r31, lua_pushnil
	jal r0, .LBB10_6
.LBB10_4:
	lui r4, %hi(.L.str.36)
	addi r4, r4, %lo(.L.str.36)
.LBB10_5:
	add r3, r11, r0
	jal r31, lua_pushstring
.LBB10_6:
	addi r1, r0, 1
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end10:
	.size	io_type, .Lfunc_end10-io_type
                                        # -- End function
	.p2align	2                               # -- Begin function io_write
	.type	io_write,@function
io_write:                               # @io_write
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	lui r1, 1048572
	addi r4, r1, 384
	jal r31, lua_getfield
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_touserdata
	ldw r3, r1+4
	addi r4, r0, 0
	beq r3, r4, .LBB11_2
.LBB11_1:
	ldw r4, r1+0
	addi r5, r0, 1
	add r3, r11, r0
	jal r31, g_write
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB11_2:
	lui r4, %hi(.L.str.18)
	addi r4, r4, %lo(.L.str.18)
	lui r5, %hi(.L.str.2+4)
	addi r5, r5, %lo(.L.str.2+4)
	add r3, r11, r0
	add r12, r1, r0
	jal r31, luaL_error
	add r1, r12, r0
	jal r0, .LBB11_1
.Lfunc_end11:
	.size	io_write, .Lfunc_end11-io_write
                                        # -- End function
	.p2align	2                               # -- Begin function f_close
	.type	f_close,@function
f_close:                                # @f_close
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	jal r31, luaL_checkudata
	ldw r1, r1+4
	addi r12, r0, 0
	beq r1, r12, .LBB12_2
.LBB12_1:
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_checkudata
	ldw r3, r1+4
	addi r4, fp, -16
	stw r4+0, r3
	stw r1+4, r12
	ldw r1, r4+0
	add r3, r11, r0
	jalr lr, r1, 0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB12_2:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB12_1
.Lfunc_end12:
	.size	f_close, .Lfunc_end12-f_close
                                        # -- End function
	.p2align	2                               # -- Begin function g_iofile
	.type	g_iofile,@function
g_iofile:                               # @g_iofile
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, lr
	add r14, r5, r0
	add r11, r4, r0
	add r12, r3, r0
	addi r13, r0, 1
	add r4, r13, r0
	jal r31, lua_type
	lui r17, 1048572
	blt r1, r13, .LBB13_7
.LBB13_1:
	addi r4, r0, 1
	addi r13, r0, 0
	add r3, r12, r0
	add r5, r13, r0
	jal r31, lua_tolstring
	beq r1, r13, .LBB13_4
.LBB13_2:
	add r15, r1, r0
	addi r4, r0, 8
	add r3, r12, r0
	add r5, r13, r0
	jal r31, lua_newuserdatauv
	add r16, r1, r0
	stw r1+4, r13
	lui r4, %hi(.L.str.16)
	addi r4, r4, %lo(.L.str.16)
	add r3, r12, r0
	jal r31, luaL_setmetatable
	stw r16+0, r13
	lui r1, %hi(io_fclose)
	addi r1, r1, %lo(io_fclose)
	stw r16+4, r1
	add r3, r15, r0
	add r4, r14, r0
	jal r31, fopen
	stw r16+0, r1
	bne r1, r13, .LBB13_6
.LBB13_3:
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	ldw r3, r1+0
	jal r31, strerror
	lui r4, %hi(.L.str.20)
	addi r4, r4, %lo(.L.str.20)
	add r3, r12, r0
	add r5, r15, r0
	add r6, r1, r0
	jal r31, luaL_error
	jal r0, .LBB13_6
.LBB13_4:
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	add r3, r12, r0
	jal r31, luaL_checkudata
	ldw r1, r1+4
	beq r1, r13, .LBB13_8
.LBB13_5:
	addi r4, r0, 1
	add r3, r12, r0
	jal r31, lua_pushvalue
.LBB13_6:
	addi r4, r17, 384
	add r3, r12, r0
	add r5, r11, r0
	jal r31, lua_setfield
.LBB13_7:
	addi r4, r17, 384
	add r3, r12, r0
	add r5, r11, r0
	jal r31, lua_getfield
	ldw lr, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB13_8:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r12, r0
	jal r31, luaL_error
	jal r0, .LBB13_5
.Lfunc_end13:
	.size	g_iofile, .Lfunc_end13-g_iofile
                                        # -- End function
	.p2align	2                               # -- Begin function io_fclose
	.type	io_fclose,@function
io_fclose:                              # @io_fclose
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	jal r31, luaL_checkudata
	lui r3, %hi(errno)
	addi r3, r3, %lo(errno)
	addi r12, r0, 0
	stw r3+0, r12
	ldw r3, r1+0
	jal r31, fclose
	seq r4, r1, r12
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_fileresult
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end14:
	.size	io_fclose, .Lfunc_end14-io_fclose
                                        # -- End function
	.p2align	2                               # -- Begin function aux_lines
	.type	aux_lines,@function
aux_lines:                              # @aux_lines
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, lr
	add r13, r4, r0
	add r11, r3, r0
	jal r31, lua_gettop
	add r12, r1, r0
	addi r1, r0, 252
	bge r12, r1, .LBB15_2
.LBB15_1:
	addi r14, r12, -1
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_pushvalue
	add r3, r11, r0
	add r4, r14, r0
	jal r31, lua_pushinteger
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_pushboolean
	addi r4, r0, 2
	addi r5, r0, 3
	add r3, r11, r0
	jal r31, lua_rotate
	addi r5, r12, 2
	lui r4, %hi(io_readline)
	addi r4, r4, %lo(io_readline)
	add r3, r11, r0
	jal r31, lua_pushcclosure
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB15_2:
	lui r5, %hi(.L.str.21)
	addi r5, r5, %lo(.L.str.21)
	addi r4, r0, 252
	add r3, r11, r0
	jal r31, luaL_argerror
	jal r0, .LBB15_1
.Lfunc_end15:
	.size	aux_lines, .Lfunc_end15-aux_lines
                                        # -- End function
	.p2align	2                               # -- Begin function io_readline
	.type	io_readline,@function
io_readline:                            # @io_readline
# %bb.0:
	addi sp, sp, -56
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 56
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, lr
	add r11, r3, r0
	lui r17, 1048572
	addi r12, r17, 383
	add r4, r12, r0
	jal r31, lua_touserdata
	add r14, r1, r0
	addi r4, r17, 382
	addi r15, r0, 0
	add r3, r11, r0
	add r5, r15, r0
	jal r31, lua_tointegerx
	add r16, r1, r0
	ldw r1, r14+4
	beq r1, r15, .LBB16_6
.LBB16_1:
	addi r13, r0, 1
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_settop
	lui r5, %hi(.L.str.21)
	addi r5, r5, %lo(.L.str.21)
	add r3, r11, r0
	add r4, r16, r0
	jal r31, luaL_checkstack
	blt r16, r13, .LBB16_4
.LBB16_2:
	sub r16, r15, r16
	addi r18, r17, 380
.LBB16_3:
	add r4, r15, r18
	add r3, r11, r0
	jal r31, lua_pushvalue
	addi r15, r15, -1
	bne r16, r15, .LBB16_3
.LBB16_4:
	ldw r4, r14+0
	addi r14, r0, 2
	add r3, r11, r0
	add r5, r14, r0
	jal r31, g_read
	add r15, r1, r0
	addi r16, r0, 0
	sub r4, r16, r1
	add r3, r11, r0
	jal r31, lua_toboolean
	beq r1, r16, .LBB16_7
.LBB16_5:
	add r1, r15, r0
	jal r0, .LBB16_11
.LBB16_6:
	lui r4, %hi(.L.str.22)
	addi r4, r4, %lo(.L.str.22)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB16_11
.LBB16_7:
	blt r15, r14, .LBB16_9
.LBB16_8:
	sub r4, r13, r15
	addi r5, r0, 0
	add r3, r11, r0
	jal r31, lua_tolstring
	lui r4, %hi(.L.str.23)
	addi r4, r4, %lo(.L.str.23)
	add r3, r11, r0
	add r5, r1, r0
	jal r31, luaL_error
	jal r0, .LBB16_11
.LBB16_9:
	addi r4, r17, 381
	add r3, r11, r0
	add r13, r16, r0
	jal r31, lua_toboolean
	add r3, r1, r0
	add r1, r16, r0
	beq r3, r13, .LBB16_11
.LBB16_10:
	addi r13, r0, 0
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_settop
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_pushvalue
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_checkudata
	ldw r3, r1+4
	addi r4, fp, -40
	stw r4+0, r3
	stw r1+4, r13
	ldw r1, r4+0
	add r3, r11, r0
	jalr lr, r1, 0
	add r1, r13, r0
.LBB16_11:
	ldw lr, fp+-36
	ldw r18, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 56
	jalr r0, r31, 0
.Lfunc_end16:
	.size	io_readline, .Lfunc_end16-io_readline
                                        # -- End function
	.p2align	2                               # -- Begin function g_read
	.type	g_read,@function
g_read:                                 # @g_read
# %bb.0:
	addi sp, sp, -648
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 648
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, r19
	stw fp+-40, r20
	stw fp+-44, r21
	stw fp+-48, r22
	stw fp+-52, r23
	stw fp+-56, r24
	stw fp+-60, r25
	stw fp+-64, r26
	stw fp+-68, r27
	stw fp+-72, r28
	stw fp+-76, lr
	add r27, r5, r0
	add r13, r4, r0
	add r12, r3, r0
	jal r31, lua_gettop
	add r14, r1, r0
	add r3, r13, r0
	jal r31, clearerr
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	addi r15, r0, 0
	stw r1+0, r15
	addi r16, r0, 1
	bne r14, r16, .LBB17_4
.LBB17_1:
	addi r5, r0, 1
	add r3, r12, r0
	add r4, r13, r0
	jal r31, read_line
	addi r14, r27, 1
.LBB17_2:
	add r11, r1, r0
	add r3, r13, r0
	jal r31, ferror
	addi r3, r0, 0
	beq r1, r3, .LBB17_70
.LBB17_3:
	addi r4, r0, 0
	add r3, r12, r0
	add r5, r4, r0
	jal r31, luaL_fileresult
	jal r0, .LBB17_73
.LBB17_4:
	addi r4, r14, 19
	lui r5, %hi(.L.str.21)
	addi r5, r5, %lo(.L.str.21)
	add r3, r12, r0
	jal r31, luaL_checkstack
	addi r25, r14, -2
	addi r17, fp, -604
	addi r18, r17, 12
	addi r24, r0, 3
	lui r28, %hi(.L.str.25)
	addi r28, r28, %lo(.L.str.25)
	addi r11, r0, -1
	addi r19, r0, 42
	addi r20, r0, 34
	lui r1, %hi(.L.str.24)
	addi r1, r1, %lo(.L.str.24)
	stw fp+-628, r1
	lui r22, %hi(.LJTI17_0)
	addi r22, r22, %lo(.LJTI17_0)
	addi r21, r0, 512
                                        # implicit-def: $r1
                                        # kill: killed $r1
	add r14, r27, r0
	stw fp+-636, r28
	stw fp+-616, r11
	stw fp+-620, r19
	stw fp+-624, r20
	stw fp+-632, r22
.LBB17_5:
	add r3, r12, r0
	add r4, r14, r0
	jal r31, lua_type
	add r3, r12, r0
	add r4, r14, r0
	bne r1, r24, .LBB17_8
.LBB17_6:
	jal r31, luaL_checkinteger
	beq r1, r15, .LBB17_11
.LBB17_7:
	add r23, r1, r0
	add r3, r12, r0
	add r4, r17, r0
	jal r31, luaL_buffinit
	add r3, r17, r0
	add r4, r23, r0
	jal r31, luaL_prepbuffsize
	add r3, r1, r0
	add r4, r16, r0
	add r5, r23, r0
	add r6, r13, r0
	jal r31, fread
	add r23, r1, r0
	ldw r1, r17+8
	add r1, r1, r23
	stw r17+8, r1
	add r3, r17, r0
	jal r31, luaL_pushresult
	sne r1, r23, r15
	jal r0, .LBB17_65
.LBB17_8:
	add r5, r15, r0
	jal r31, luaL_checklstring
	ldbu r3, r1+0
	seq r3, r3, r19
	add r1, r1, r3
	ldbu r1, r1+0
	addi r1, r1, -76
	bgtu r1, r20, .LBB17_58
.LBB17_9:
	slli r1, r1, 2
	add r1, r22, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB17_10:
	add r3, r12, r0
	add r4, r13, r0
	add r5, r15, r0
	jal r31, read_line
	add r23, r16, r0
	bne r23, r15, .LBB17_65
	jal r0, .LBB17_74
.LBB17_11:
	add r3, r13, r0
	jal r31, getc
	add r23, r1, r0
	add r3, r1, r0
	add r4, r13, r0
	jal r31, ungetc
	add r3, r12, r0
	add r4, r28, r0
	jal r31, lua_pushstring
	sne r1, r23, r11
	jal r0, .LBB17_65
.LBB17_12:
	addi r23, r0, 1
	add r3, r12, r0
	add r4, r13, r0
	add r5, r23, r0
	jal r31, read_line
	bne r23, r15, .LBB17_65
	jal r0, .LBB17_74
.LBB17_13:
	add r3, r12, r0
	add r4, r17, r0
	jal r31, luaL_buffinit
.LBB17_14:
	add r3, r17, r0
	add r4, r21, r0
	jal r31, luaL_prepbuffsize
	add r3, r1, r0
	add r4, r16, r0
	add r5, r21, r0
	add r6, r13, r0
	jal r31, fread
	ldw r3, r17+8
	add r3, r3, r1
	stw r17+8, r3
	beq r1, r21, .LBB17_14
.LBB17_15:
	add r3, r17, r0
	jal r31, luaL_pushresult
	add r1, r16, r0
	add r23, r16, r0
	bne r23, r15, .LBB17_65
	jal r0, .LBB17_74
.LBB17_16:
	stw r17+0, r13
	addi r22, r0, 0
	stw r17+8, r22
.LBB17_17:
	ldw r3, r17+0
	jal r31, getc
	stw r17+4, r1
	add r3, r1, r0
	jal r31, isspace
	bne r1, r22, .LBB17_17
.LBB17_18:
	ldw r1, r17+4
	addi r3, r0, 45
	stw fp+-612, r3
	beq r1, r3, .LBB17_20
.LBB17_19:
	addi r3, r0, 43
	bne r1, r3, .LBB17_22
.LBB17_20:
	ldw r3, r17+8
	addi r4, r0, 200
	bge r3, r4, .LBB17_59
.LBB17_21:
	addi r4, r3, 1
	stw r17+8, r4
	add r3, r18, r3
	stb r3+0, r1
	ldw r3, r17+0
	jal r31, getc
	stw r17+4, r1
.LBB17_22:
	ldw r3, r17+4
	addi r23, r0, 1
	lui r20, %hi(.L.str.30)
	addi r20, r20, %lo(.L.str.30)
	addi r1, r0, 48
	addi r11, r0, 200
	bne r3, r1, .LBB17_28
.LBB17_23:
	ldw r3, r17+8
	bge r3, r11, .LBB17_60
.LBB17_24:
	addi r4, r3, 1
	stw r17+8, r4
	add r3, r18, r3
	stb r3+0, r1
	ldw r3, r17+0
	jal r31, getc
	stw r17+4, r1
	addi r22, r0, 1
	ori  r3, r1, 32
	addi r4, r0, 120
	bne r3, r4, .LBB17_27
.LBB17_25:
	ldw r3, r17+8
	bge r3, r11, .LBB17_68
.LBB17_26:
	addi r4, r3, 1
	stw r17+8, r4
	add r3, r18, r3
	stb r3+0, r1
	ldw r3, r17+0
	jal r31, getc
	stw r17+4, r1
	addi r22, r0, 0
.LBB17_27:
	addi r1, r0, 0
	sub r1, r1, r22
	lui r3, %hi(.L.str.29)
	addi r3, r3, %lo(.L.str.29)
	lui r4, %hi(.L.str.30)
	addi r4, r4, %lo(.L.str.30)
	xor r4, r4, r3
	and r1, r4, r1
	xor r20, r1, r3
	add r23, r22, r0
.LBB17_28:
	ldw r3, r17+4
	addi r19, r0, 0
	beq r23, r19, .LBB17_30
.LBB17_29:
	jal r31, isdigit
	addi r3, r0, 0
	bne r1, r3, .LBB17_31
	jal r0, .LBB17_35
.LBB17_30:
	jal r31, isxdigit
	beq r1, r19, .LBB17_35
.LBB17_31:
	ldw r26, r17+8
	bge r26, r11, .LBB17_34
.LBB17_32:
	ldw r1, r17+4
	addi r3, r26, 1
	stw r17+8, r3
	add r3, r18, r26
	stb r3+0, r1
	ldw r3, r17+0
	jal r31, getc
	stw r17+4, r1
	addi r1, r0, 199
	bgt r26, r1, .LBB17_35
.LBB17_33:
	addi r22, r22, 1
	jal r0, .LBB17_28
.LBB17_34:
	stb r17+12, r19
	addi r1, r0, 199
	ble r26, r1, .LBB17_33
.LBB17_35:
	ldw r3, r17+4
	addi r1, r0, 46
	bne r3, r1, .LBB17_46
.LBB17_36:
	ldw r3, r17+8
	bge r3, r11, .LBB17_61
.LBB17_37:
	add r28, r27, r0
	addi r4, r3, 1
	stw r17+8, r4
	add r3, r18, r3
	stb r3+0, r1
	ldw r3, r17+0
	jal r31, getc
	stw r17+4, r1
.LBB17_38:
	ldw r3, r17+4
	addi r26, r0, 0
	beq r23, r26, .LBB17_40
.LBB17_39:
	jal r31, isdigit
	bne r1, r26, .LBB17_41
	jal r0, .LBB17_45
.LBB17_40:
	jal r31, isxdigit
	beq r1, r26, .LBB17_45
.LBB17_41:
	ldw r27, r17+8
	bge r27, r11, .LBB17_44
.LBB17_42:
	ldw r1, r17+4
	addi r3, r27, 1
	stw r17+8, r3
	add r3, r18, r27
	stb r3+0, r1
	ldw r3, r17+0
	jal r31, getc
	stw r17+4, r1
	addi r1, r0, 199
	bgt r27, r1, .LBB17_45
.LBB17_43:
	addi r22, r22, 1
	jal r0, .LBB17_38
.LBB17_44:
	stb r17+12, r26
	addi r1, r0, 199
	ble r27, r1, .LBB17_43
.LBB17_45:
	add r27, r28, r0
	ldw r28, fp+-636
.LBB17_46:
	addi r23, r0, 1
	blt r22, r23, .LBB17_63
.LBB17_47:
	ldw r1, r17+4
	ldb r3, r20+0
	beq r1, r3, .LBB17_49
.LBB17_48:
	ldb r3, r20+1
	bne r1, r3, .LBB17_63
.LBB17_49:
	ldw r3, r17+8
	bge r3, r11, .LBB17_62
.LBB17_50:
	addi r4, r3, 1
	stw r17+8, r4
	add r3, r18, r3
	stb r3+0, r1
	ldw r3, r17+0
	jal r31, getc
	stw r17+4, r1
	ldw r3, fp+-612
	beq r1, r3, .LBB17_52
.LBB17_51:
	addi r3, r0, 43
	bne r1, r3, .LBB17_54
.LBB17_52:
	ldw r3, r17+8
	bge r3, r11, .LBB17_69
.LBB17_53:
	addi r4, r3, 1
	stw r17+8, r4
	add r3, r18, r3
	stb r3+0, r1
	ldw r3, r17+0
	jal r31, getc
	stw r17+4, r1
.LBB17_54:
	ldw r3, r17+4
	jal r31, isdigit
	addi r3, r0, 0
	beq r1, r3, .LBB17_63
.LBB17_55:
	ldw r20, r17+8
	bge r20, r11, .LBB17_57
.LBB17_56:
	ldw r1, r17+4
	addi r3, r20, 1
	stw r17+8, r3
	add r3, r18, r20
	stb r3+0, r1
	ldw r3, r17+0
	jal r31, getc
	stw r17+4, r1
	blt r20, r11, .LBB17_54
	jal r0, .LBB17_63
.LBB17_57:
	stb r17+12, r3
	blt r20, r11, .LBB17_54
	jal r0, .LBB17_63
.LBB17_58:
	add r3, r12, r0
	add r4, r14, r0
	ldw r5, fp+-628
	jal r31, luaL_argerror
	stw fp+-608, r1
	add r1, r16, r0
	add r23, r15, r0
	bne r23, r15, .LBB17_65
	jal r0, .LBB17_74
.LBB17_59:
	addi r1, r0, 0
	stb r17+12, r1
	jal r0, .LBB17_22
.LBB17_60:
	addi r22, r0, 0
	stb r17+12, r22
	jal r0, .LBB17_28
.LBB17_61:
	addi r1, r0, 0
	stb r17+12, r1
	addi r23, r0, 1
	bge r22, r23, .LBB17_47
	jal r0, .LBB17_63
.LBB17_62:
	addi r1, r0, 0
	stb r17+12, r1
.LBB17_63:
	ldw r3, r17+4
	ldw r4, r17+0
	jal r31, ungetc
	ldw r1, r17+8
	add r1, r18, r1
	stb r1+0, r19
	add r3, r12, r0
	add r4, r18, r0
	jal r31, lua_stringtonumber
	add r3, r1, r0
	add r1, r23, r0
	beq r3, r19, .LBB17_67
.LBB17_64:
	ldw r11, fp+-616
	ldw r19, fp+-620
	ldw r20, fp+-624
	ldw r22, fp+-632
	beq r23, r15, .LBB17_74
.LBB17_65:
	addi r14, r14, 1
	addi r3, r25, -1
	sltu r4, r3, r25
	bne r4, r16, .LBB17_2
.LBB17_66:
	add r25, r3, r0
	bne r1, r15, .LBB17_5
	jal r0, .LBB17_2
.LBB17_67:
	add r3, r12, r0
	jal r31, lua_pushnil
	add r1, r19, r0
	jal r0, .LBB17_64
.LBB17_68:
	addi r1, r0, 0
	stb r17+12, r1
	jal r0, .LBB17_27
.LBB17_69:
	addi r1, r0, 0
	stb r17+12, r1
	jal r0, .LBB17_54
.LBB17_70:
	bne r11, r3, .LBB17_72
.LBB17_71:
	addi r4, r0, -2
	add r3, r12, r0
	jal r31, lua_settop
	add r3, r12, r0
	jal r31, lua_pushnil
.LBB17_72:
	sub r1, r14, r27
.LBB17_73:
	stw fp+-608, r1
.LBB17_74:
	ldw r1, fp+-608
	ldw lr, fp+-76
	ldw r28, fp+-72
	ldw r27, fp+-68
	ldw r26, fp+-64
	ldw r25, fp+-60
	ldw r24, fp+-56
	ldw r23, fp+-52
	ldw r22, fp+-48
	ldw r21, fp+-44
	ldw r20, fp+-40
	ldw r19, fp+-36
	ldw r18, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 648
	jalr r0, r31, 0
.Lfunc_end17:
	.size	g_read, .Lfunc_end17-g_read
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI17_0:
	.word	.LBB17_10
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_13
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_58
	.word	.LBB17_12
	.word	.LBB17_58
	.word	.LBB17_16
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function read_line
	.type	read_line,@function
read_line:                              # @read_line
# %bb.0:
	addi sp, sp, -584
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 584
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, r19
	stw fp+-40, r20
	stw fp+-44, lr
	add r13, r5, r0
	add r14, r4, r0
	add r11, r3, r0
	addi r12, fp, -572
	add r4, r12, r0
	jal r31, luaL_buffinit
	addi r15, r0, 512
	addi r18, r0, 0
	addi r19, r0, -1
	addi r17, r0, 10
.LBB18_1:
	add r3, r12, r0
	add r4, r15, r0
	jal r31, luaL_prepbuffsize
	add r16, r1, r0
	add r20, r18, r0
.LBB18_2:
	add r3, r14, r0
	jal r31, getc
	beq r1, r19, .LBB18_6
.LBB18_3:
	beq r1, r17, .LBB18_6
.LBB18_4:
	addi r3, r20, 1
	add r4, r16, r20
	stb r4+0, r1
	add r20, r3, r0
	bne r3, r15, .LBB18_2
.LBB18_5:
	add r20, r15, r0
.LBB18_6:
	ldw r3, r12+8
	add r3, r3, r20
	stw r12+8, r3
	beq r1, r17, .LBB18_8
.LBB18_7:
	bne r1, r19, .LBB18_1
.LBB18_8:
	addi r14, r0, 0
	bne r13, r14, .LBB18_13
.LBB18_9:
	bne r1, r17, .LBB18_13
.LBB18_10:
	ldw r4, r12+4
	bltu r3, r4, .LBB18_12
.LBB18_11:
	addi r3, fp, -572
	addi r4, r0, 1
	add r13, r1, r0
	jal r31, luaL_prepbuffsize
	add r1, r13, r0
.LBB18_12:
	ldw r3, r12+0
	ldw r4, r12+8
	addi r5, r4, 1
	stw r12+8, r5
	add r3, r3, r4
	stb r3+0, r17
.LBB18_13:
	addi r3, fp, -572
	add r12, r1, r0
	jal r31, luaL_pushresult
	bne r12, r17, .LBB18_15
.LBB18_14:
	addi r1, r0, 1
	jal r0, .LBB18_16
.LBB18_15:
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, lua_rawlen
	sne r1, r1, r14
.LBB18_16:
	ldw lr, fp+-44
	ldw r20, fp+-40
	ldw r19, fp+-36
	ldw r18, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 584
	jalr r0, r31, 0
.Lfunc_end18:
	.size	read_line, .Lfunc_end18-read_line
                                        # -- End function
	.p2align	2                               # -- Begin function io_pclose
	.type	io_pclose,@function
io_pclose:                              # @io_pclose
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	jal r31, luaL_checkudata
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	addi r3, r0, 0
	stw r1+0, r3
	addi r4, r0, -1
	add r3, r11, r0
	jal r31, luaL_execresult
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end19:
	.size	io_pclose, .Lfunc_end19-io_pclose
                                        # -- End function
	.p2align	2                               # -- Begin function g_write
	.type	g_write,@function
g_write:                                # @g_write
# %bb.0:
	addi sp, sp, -72
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 72
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, r19
	stw fp+-40, r20
	stw fp+-44, r21
	stw fp+-48, lr
	add r12, r5, r0
	add r13, r4, r0
	add r11, r3, r0
	jal r31, lua_gettop
	add r14, r1, r0
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	addi r15, r0, 0
	stw r1+0, r15
	beq r14, r12, .LBB20_11
.LBB20_1:
	addi r16, r0, 1
	addi r20, r0, 3
	lui r17, %hi(.L.str.39)
	addi r17, r17, %lo(.L.str.39)
	lui r18, %hi(.L.str.38)
	addi r18, r18, %lo(.L.str.38)
	addi r19, fp, -52
	add r21, r16, r0
	jal r0, .LBB20_3
.LBB20_2:
	add r21, r3, r0
	addi r12, r12, 1
	beq r14, r12, .LBB20_10
.LBB20_3:
	add r3, r11, r0
	add r4, r12, r0
	jal r31, lua_type
	add r3, r11, r0
	add r4, r12, r0
	bne r1, r20, .LBB20_6
.LBB20_4:
	jal r31, lua_isinteger
	add r3, r11, r0
	add r4, r12, r0
	add r5, r15, r0
	beq r1, r15, .LBB20_8
.LBB20_5:
	jal r31, lua_tointegerx
	add r3, r13, r0
	add r4, r18, r0
	add r5, r1, r0
	jal r0, .LBB20_9
.LBB20_6:
	add r5, r19, r0
	jal r31, luaL_checklstring
	add r3, r15, r0
	beq r21, r15, .LBB20_2
.LBB20_7:
	ldw r5, r19+0
	add r3, r1, r0
	add r4, r16, r0
	add r6, r13, r0
	jal r31, fwrite
	ldw r3, r19+0
	seq r3, r1, r3
	jal r0, .LBB20_2
.LBB20_8:
	jal r31, lua_tonumberx
	add r3, r13, r0
	add r4, r17, r0
	add r5, r1, r0
	add r6, r2, r0
.LBB20_9:
	jal r31, fprintf
	sne r3, r21, r15
	sgt r1, r1, r15
	and r21, r3, r1
	addi r12, r12, 1
	bne r14, r12, .LBB20_3
.LBB20_10:
	addi r1, r0, 0
	beq r21, r1, .LBB20_13
.LBB20_11:
	addi r1, r0, 1
.LBB20_12:
	ldw lr, fp+-48
	ldw r21, fp+-44
	ldw r20, fp+-40
	ldw r19, fp+-36
	ldw r18, fp+-32
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB20_13:
	addi r4, r0, 0
	add r3, r11, r0
	add r5, r4, r0
	jal r31, luaL_fileresult
	jal r0, .LBB20_12
.Lfunc_end20:
	.size	g_write, .Lfunc_end20-g_write
                                        # -- End function
	.p2align	2                               # -- Begin function f_gc
	.type	f_gc,@function
f_gc:                                   # @f_gc
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	jal r31, luaL_checkudata
	ldw r3, r1+4
	addi r12, r0, 0
	beq r3, r12, .LBB21_3
.LBB21_1:
	ldw r1, r1+0
	beq r1, r12, .LBB21_3
.LBB21_2:
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, luaL_checkudata
	ldw r3, r1+4
	addi r4, fp, -16
	stw r4+0, r3
	stw r1+4, r12
	ldw r1, r4+0
	add r3, r11, r0
	jalr lr, r1, 0
.LBB21_3:
	addi r1, r0, 0
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end21:
	.size	f_gc, .Lfunc_end21-f_gc
                                        # -- End function
	.p2align	2                               # -- Begin function f_tostring
	.type	f_tostring,@function
f_tostring:                             # @f_tostring
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	jal r31, luaL_checkudata
	ldw r3, r1+4
	addi r4, r0, 0
	beq r3, r4, .LBB22_2
.LBB22_1:
	ldw r5, r1+0
	lui r4, %hi(.L.str.45)
	addi r4, r4, %lo(.L.str.45)
	add r3, r11, r0
	jal r31, lua_pushfstring
	jal r0, .LBB22_3
.LBB22_2:
	lui r4, %hi(.L.str.44)
	addi r4, r4, %lo(.L.str.44)
	add r3, r11, r0
	jal r31, lua_pushstring
.LBB22_3:
	addi r1, r0, 1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end22:
	.size	f_tostring, .Lfunc_end22-f_tostring
                                        # -- End function
	.p2align	2                               # -- Begin function f_read
	.type	f_read,@function
f_read:                                 # @f_read
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	jal r31, luaL_checkudata
	ldw r3, r1+4
	addi r4, r0, 0
	beq r3, r4, .LBB23_2
.LBB23_1:
	ldw r4, r1+0
	addi r5, r0, 2
	add r3, r11, r0
	jal r31, g_read
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB23_2:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r11, r0
	add r12, r1, r0
	jal r31, luaL_error
	add r1, r12, r0
	jal r0, .LBB23_1
.Lfunc_end23:
	.size	f_read, .Lfunc_end23-f_read
                                        # -- End function
	.p2align	2                               # -- Begin function f_write
	.type	f_write,@function
f_write:                                # @f_write
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	jal r31, luaL_checkudata
	ldw r3, r1+4
	addi r4, r0, 0
	beq r3, r4, .LBB24_2
.LBB24_1:
	ldw r12, r1+0
	addi r4, r0, 1
	add r3, r11, r0
	jal r31, lua_pushvalue
	addi r5, r0, 2
	add r3, r11, r0
	add r4, r12, r0
	jal r31, g_write
	ldw lr, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB24_2:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r11, r0
	add r12, r1, r0
	jal r31, luaL_error
	add r1, r12, r0
	jal r0, .LBB24_1
.Lfunc_end24:
	.size	f_write, .Lfunc_end24-f_write
                                        # -- End function
	.p2align	2                               # -- Begin function f_lines
	.type	f_lines,@function
f_lines:                                # @f_lines
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	jal r31, luaL_checkudata
	ldw r1, r1+4
	addi r3, r0, 0
	beq r1, r3, .LBB25_2
.LBB25_1:
	addi r4, r0, 0
	add r3, r11, r0
	jal r31, aux_lines
	addi r1, r0, 1
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB25_2:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r11, r0
	jal r31, luaL_error
	jal r0, .LBB25_1
.Lfunc_end25:
	.size	f_lines, .Lfunc_end25-f_lines
                                        # -- End function
	.p2align	2                               # -- Begin function f_flush
	.type	f_flush,@function
f_flush:                                # @f_flush
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	jal r31, luaL_checkudata
	ldw r3, r1+4
	addi r12, r0, 0
	beq r3, r12, .LBB26_2
.LBB26_1:
	ldw r3, r1+0
	lui r1, %hi(errno)
	addi r1, r1, %lo(errno)
	stw r1+0, r12
	jal r31, fflush
	seq r4, r1, r12
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_fileresult
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.LBB26_2:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r11, r0
	add r13, r1, r0
	jal r31, luaL_error
	add r1, r13, r0
	jal r0, .LBB26_1
.Lfunc_end26:
	.size	f_flush, .Lfunc_end26-f_flush
                                        # -- End function
	.p2align	2                               # -- Begin function f_seek
	.type	f_seek,@function
f_seek:                                 # @f_seek
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, lr
	add r12, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r11, r0, 1
	add r4, r11, r0
	jal r31, luaL_checkudata
	ldw r3, r1+4
	addi r13, r0, 0
	beq r3, r13, .LBB27_4
.LBB27_1:
	ldw r14, r1+0
	lui r5, %hi(.L.str.49)
	addi r5, r5, %lo(.L.str.49)
	lui r6, %hi(f_seek.modenames)
	addi r6, r6, %lo(f_seek.modenames)
	addi r4, r0, 2
	add r3, r12, r0
	jal r31, luaL_checkoption
	add r15, r1, r0
	addi r4, r0, 3
	add r3, r12, r0
	add r5, r13, r0
	jal r31, luaL_optinteger
	lui r3, %hi(errno)
	addi r3, r3, %lo(errno)
	stw r3+0, r13
	slli r3, r15, 2
	lui r4, %hi(f_seek.mode)
	addi r4, r4, %lo(f_seek.mode)
	add r3, r3, r4
	ldw r5, r3+0
	add r3, r14, r0
	add r4, r1, r0
	jal r31, fseek
	bne r1, r13, .LBB27_5
.LBB27_2:
	add r3, r14, r0
	jal r31, ftell
	add r3, r12, r0
	add r4, r1, r0
	jal r31, lua_pushinteger
.LBB27_3:
	add r1, r11, r0
	ldw lr, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB27_4:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r12, r0
	add r14, r1, r0
	jal r31, luaL_error
	add r1, r14, r0
	jal r0, .LBB27_1
.LBB27_5:
	addi r4, r0, 0
	add r3, r12, r0
	add r5, r4, r0
	jal r31, luaL_fileresult
	add r11, r1, r0
	jal r0, .LBB27_3
.Lfunc_end27:
	.size	f_seek, .Lfunc_end27-f_seek
                                        # -- End function
	.p2align	2                               # -- Begin function f_setvbuf
	.type	f_setvbuf,@function
f_setvbuf:                              # @f_setvbuf
# %bb.0:
	addi sp, sp, -40
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 40
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	jal r31, luaL_checkudata
	ldw r3, r1+4
	addi r12, r0, 0
	beq r3, r12, .LBB28_2
.LBB28_1:
	ldw r13, r1+0
	lui r6, %hi(f_setvbuf.modenames)
	addi r6, r6, %lo(f_setvbuf.modenames)
	addi r4, r0, 2
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_checkoption
	add r14, r1, r0
	addi r4, r0, 3
	addi r5, r0, 512
	add r3, r11, r0
	jal r31, luaL_optinteger
	lui r3, %hi(errno)
	addi r3, r3, %lo(errno)
	stw r3+0, r12
	slli r3, r14, 2
	lui r4, %hi(f_setvbuf.mode)
	addi r4, r4, %lo(f_setvbuf.mode)
	add r3, r3, r4
	ldw r5, r3+0
	add r3, r13, r0
	add r4, r12, r0
	add r6, r1, r0
	jal r31, setvbuf
	seq r4, r1, r12
	add r3, r11, r0
	add r5, r12, r0
	jal r31, luaL_fileresult
	ldw lr, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 40
	jalr r0, r31, 0
.LBB28_2:
	lui r4, %hi(.L.str.17)
	addi r4, r4, %lo(.L.str.17)
	add r3, r11, r0
	add r13, r1, r0
	jal r31, luaL_error
	add r1, r13, r0
	jal r0, .LBB28_1
.Lfunc_end28:
	.size	f_setvbuf, .Lfunc_end28-f_setvbuf
                                        # -- End function
	.p2align	2                               # -- Begin function io_noclose
	.type	io_noclose,@function
io_noclose:                             # @io_noclose
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	lui r5, %hi(.L.str.16)
	addi r5, r5, %lo(.L.str.16)
	addi r4, r0, 1
	jal r31, luaL_checkudata
	lui r3, %hi(io_noclose)
	addi r3, r3, %lo(io_noclose)
	stw r1+4, r3
	add r3, r11, r0
	jal r31, lua_pushnil
	lui r4, %hi(.L.str.55)
	addi r4, r4, %lo(.L.str.55)
	add r3, r11, r0
	jal r31, lua_pushstring
	addi r1, r0, 2
	ldw lr, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end29:
	.size	io_noclose, .Lfunc_end29-io_noclose
                                        # -- End function
	.type	iolib,@object                   # @iolib
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
iolib:
	.word	.L.str.5
	.word	io_close
	.word	.L.str.6
	.word	io_flush
	.word	.L.str.7
	.word	io_input
	.word	.L.str.8
	.word	io_lines
	.word	.L.str.9
	.word	io_open
	.word	.L.str.10
	.word	io_output
	.word	.L.str.11
	.word	io_popen
	.word	.L.str.12
	.word	io_read
	.word	.L.str.13
	.word	io_tmpfile
	.word	.L.str.14
	.word	io_type
	.word	.L.str.15
	.word	io_write
	.zero	8
	.size	iolib, 96

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"_IO_input"
	.size	.L.str, 10

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"stdin"
	.size	.L.str.1, 6

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"_IO_output"
	.size	.L.str.2, 11

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"stdout"
	.size	.L.str.3, 7

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"stderr"
	.size	.L.str.4, 7

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"close"
	.size	.L.str.5, 6

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"flush"
	.size	.L.str.6, 6

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"input"
	.size	.L.str.7, 6

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"lines"
	.size	.L.str.8, 6

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"open"
	.size	.L.str.9, 5

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"output"
	.size	.L.str.10, 7

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"popen"
	.size	.L.str.11, 6

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"read"
	.size	.L.str.12, 5

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"tmpfile"
	.size	.L.str.13, 8

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"type"
	.size	.L.str.14, 5

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"write"
	.size	.L.str.15, 6

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"FILE*"
	.size	.L.str.16, 6

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"attempt to use a closed file"
	.size	.L.str.17, 29

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"default %s file is closed"
	.size	.L.str.18, 26

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"r"
	.size	.L.str.19, 2

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"cannot open file '%s' (%s)"
	.size	.L.str.20, 27

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"too many arguments"
	.size	.L.str.21, 19

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"file is already closed"
	.size	.L.str.22, 23

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"%s"
	.size	.L.str.23, 3

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"invalid format"
	.size	.L.str.24, 15

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.zero	1
	.size	.L.str.25, 1

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"pP"
	.size	.L.str.29, 3

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"eE"
	.size	.L.str.30, 3

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"invalid mode"
	.size	.L.str.31, 13

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"rwa"
	.size	.L.str.32, 4

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"b"
	.size	.L.str.33, 2

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"w"
	.size	.L.str.34, 2

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"'popen' not supported"
	.size	.L.str.35, 22

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"closed file"
	.size	.L.str.36, 12

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"file"
	.size	.L.str.37, 5

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"%ld"
	.size	.L.str.38, 4

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"%.14g"
	.size	.L.str.39, 6

	.type	metameth,@object                # @metameth
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
metameth:
	.word	.L.str.40
	.word	0
	.word	.L.str.41
	.word	f_gc
	.word	.L.str.42
	.word	f_gc
	.word	.L.str.43
	.word	f_tostring
	.zero	8
	.size	metameth, 40

	.type	meth,@object                    # @meth
	.p2align	2, 0x0
meth:
	.word	.L.str.12
	.word	f_read
	.word	.L.str.15
	.word	f_write
	.word	.L.str.8
	.word	f_lines
	.word	.L.str.6
	.word	f_flush
	.word	.L.str.46
	.word	f_seek
	.word	.L.str.5
	.word	f_close
	.word	.L.str.47
	.word	f_setvbuf
	.zero	8
	.size	meth, 64

	.type	.L.str.40,@object               # @.str.40
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.40:
	.asciz	"__index"
	.size	.L.str.40, 8

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"__gc"
	.size	.L.str.41, 5

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"__close"
	.size	.L.str.42, 8

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"__tostring"
	.size	.L.str.43, 11

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"file (closed)"
	.size	.L.str.44, 14

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"file (%p)"
	.size	.L.str.45, 10

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"seek"
	.size	.L.str.46, 5

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"setvbuf"
	.size	.L.str.47, 8

	.type	f_seek.mode,@object             # @f_seek.mode
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
f_seek.mode:
	.word	0                               # 0x0
	.word	1                               # 0x1
	.word	2                               # 0x2
	.size	f_seek.mode, 12

	.type	f_seek.modenames,@object        # @f_seek.modenames
	.p2align	2, 0x0
f_seek.modenames:
	.word	.L.str.48
	.word	.L.str.49
	.word	.L.str.50
	.word	0
	.size	f_seek.modenames, 16

	.type	.L.str.48,@object               # @.str.48
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.48:
	.asciz	"set"
	.size	.L.str.48, 4

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"cur"
	.size	.L.str.49, 4

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"end"
	.size	.L.str.50, 4

	.type	f_setvbuf.mode,@object          # @f_setvbuf.mode
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
f_setvbuf.mode:
	.word	2                               # 0x2
	.word	0                               # 0x0
	.word	1                               # 0x1
	.size	f_setvbuf.mode, 12

	.type	f_setvbuf.modenames,@object     # @f_setvbuf.modenames
	.p2align	2, 0x0
f_setvbuf.modenames:
	.word	.L.str.52
	.word	.L.str.53
	.word	.L.str.54
	.word	0
	.size	f_setvbuf.modenames, 16

	.type	.L.str.52,@object               # @.str.52
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.52:
	.asciz	"no"
	.size	.L.str.52, 3

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"full"
	.size	.L.str.53, 5

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"line"
	.size	.L.str.54, 5

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"cannot close standard file"
	.size	.L.str.55, 27

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
