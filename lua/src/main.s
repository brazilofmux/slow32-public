	.file	"main.c"
	.text
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
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
	stw fp+-48, r22
	stw fp+-52, lr
	jal r31, luaL_newstate
	addi r18, r0, 0
	beq r1, r18, .LBB0_11
.LBB0_1:
	add r11, r1, r0
	lui r14, 1
	add r3, r14, r0
	jal r31, malloc
	beq r1, r18, .LBB0_12
.LBB0_2:
	add r16, r1, r0
	jal r31, getchar
	addi r19, r0, 0
	addi r20, r0, -1
	beq r1, r20, .LBB0_13
.LBB0_3:
	add r15, r1, r0
	addi r21, r0, 0
	add r22, r21, r0
	jal r0, .LBB0_6
.LBB0_4:
	add r12, r16, r0
.LBB0_5:
	add r1, r12, r22
	stb r1+0, r15
	jal r31, getchar
	add r15, r1, r0
	add r16, r12, r0
	add r22, r13, r0
	beq r1, r20, .LBB0_14
.LBB0_6:
	addi r13, r22, 1
	bltu r13, r14, .LBB0_4
.LBB0_7:
	slli r14, r14, 1
	add r3, r16, r0
	add r4, r14, r0
	jal r31, realloc
	add r17, r1, r0
	add r12, r1, r0
	beq r1, r21, .LBB0_9
.LBB0_8:
	bne r17, r21, .LBB0_5
	jal r0, .LBB0_10
.LBB0_9:
	add r3, r16, r0
	jal r31, free
	add r12, r16, r0
	bne r17, r21, .LBB0_5
.LBB0_10:
	add r13, r21, r0
	add r12, r21, r0
	jal r0, .LBB0_15
.LBB0_11:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str)
	addi r4, r4, %lo(.L.str)
	jal r31, fprintf
	addi r1, r0, 1
	jal r0, .LBB0_38
.LBB0_12:
	add r13, r18, r0
	add r12, r18, r0
	jal r0, .LBB0_15
.LBB0_13:
	add r13, r19, r0
	add r12, r16, r0
.LBB0_14:
	add r1, r12, r13
	stb r1+0, r19
.LBB0_15:
	add r3, r11, r0
	jal r31, luaL_openlibs
	beq r12, r18, .LBB0_24
.LBB0_16:
	addi r19, r0, 0
	add r15, r19, r0
	beq r13, r19, .LBB0_37
.LBB0_17:
	addi r15, r0, 10
	add r3, r12, r0
	add r4, r15, r0
	jal r31, strchr
	addi r14, r0, 0
	beq r1, r14, .LBB0_19
.LBB0_18:
	addi r4, r0, 10
	add r3, r12, r0
	jal r31, strchr
	add r3, r1, r0
	add r1, r12, r13
	addi r4, r1, -1
	add r1, r14, r0
	bne r3, r4, .LBB0_30
.LBB0_19:
	addi r17, r13, 16
	add r3, r17, r0
	jal r31, malloc
	addi r20, r0, 0
	addi r18, r0, 1
	beq r1, r20, .LBB0_29
.LBB0_20:
	add r16, r1, r0
	addi r1, r0, 13
	addi r21, r0, 0
	add r6, r13, r0
	jal r0, .LBB0_22
.LBB0_21:
	addi r6, r6, -1
	beq r6, r21, .LBB0_25
.LBB0_22:
	add r3, r12, r6
	ldbu r3, r3+-1
	beq r3, r1, .LBB0_21
.LBB0_23:
	beq r3, r15, .LBB0_21
	jal r0, .LBB0_26
.LBB0_24:
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.1)
	addi r4, r4, %lo(.L.str.1)
	jal r31, fprintf
	add r3, r11, r0
	jal r31, lua_close
	addi r1, r0, 1
	jal r0, .LBB0_38
.LBB0_25:
	add r6, r21, r0
.LBB0_26:
	lui r5, %hi(.L.str.2)
	addi r5, r5, %lo(.L.str.2)
	add r3, r16, r0
	add r4, r17, r0
	add r7, r12, r0
	jal r31, snprintf
	add r3, r11, r0
	add r4, r16, r0
	jal r31, luaL_loadstring
	bne r1, r21, .LBB0_28
.LBB0_27:
	addi r5, r0, -1
	addi r17, r0, 0
	add r3, r11, r0
	add r4, r17, r0
	add r6, r17, r0
	add r7, r17, r0
	add r8, r17, r0
	jal r31, lua_pcallk
	beq r1, r17, .LBB0_39
.LBB0_28:
	add r15, r1, r0
	add r3, r16, r0
	jal r31, free
	addi r16, r0, 0
	add r3, r11, r0
	add r4, r16, r0
	jal r31, lua_settop
	add r1, r16, r0
	add r17, r18, r0
	bne r17, r20, .LBB0_30
	jal r0, .LBB0_36
.LBB0_29:
	add r17, r18, r0
	add r15, r20, r0
	add r1, r20, r0
	beq r17, r20, .LBB0_36
.LBB0_30:
	add r16, r1, r0
	lui r6, %hi(.L.str.5)
	addi r6, r6, %lo(.L.str.5)
	add r3, r11, r0
	add r4, r12, r0
	add r5, r13, r0
	add r7, r14, r0
	jal r31, luaL_loadbufferx
	add r15, r1, r0
	bne r1, r14, .LBB0_32
.LBB0_31:
	addi r4, r0, 0
	add r3, r11, r0
	add r5, r4, r0
	add r6, r4, r0
	add r7, r4, r0
	add r8, r4, r0
	jal r31, lua_pcallk
	add r15, r1, r0
.LBB0_32:
	addi r1, r0, 0
	addi r20, r0, 1
	beq r15, r1, .LBB0_34
.LBB0_33:
	addi r4, r0, -1
	addi r13, r0, 0
	add r3, r11, r0
	add r5, r13, r0
	jal r31, lua_tolstring
	seq r3, r1, r13
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	xor r4, r1, r4
	sub r3, r13, r3
	and r3, r4, r3
	xor r5, r1, r3
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	jal r31, fprintf
	addi r4, r0, -2
	add r3, r11, r0
	jal r31, lua_settop
	jal r0, .LBB0_35
.LBB0_34:
	add r15, r1, r0
.LBB0_35:
	add r1, r16, r0
.LBB0_36:
	addi r3, r0, 0
	beq r20, r3, .LBB0_38
.LBB0_37:
	add r3, r12, r0
	jal r31, free
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r3, r1+0
	jal r31, fflush
	add r3, r11, r0
	jal r31, lua_close
	sne r1, r15, r19
.LBB0_38:
	ldw lr, fp+-52
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
	addi sp, sp, 72
	jalr r0, r31, 0
.LBB0_39:
	add r3, r11, r0
	jal r31, lua_gettop
	add r18, r1, r0
	addi r1, r0, 1
	add r15, r17, r0
	blt r18, r1, .LBB0_41
.LBB0_40:
	lui r5, %hi(.L.str.3)
	addi r5, r5, %lo(.L.str.3)
	addi r4, r0, 20
	add r3, r11, r0
	jal r31, luaL_checkstack
	lui r4, %hi(.L.str.4)
	addi r4, r4, %lo(.L.str.4)
	add r3, r11, r0
	jal r31, lua_getglobal
	addi r4, r0, 1
	add r3, r11, r0
	add r5, r4, r0
	jal r31, lua_rotate
	addi r5, r0, 0
	add r3, r11, r0
	add r4, r18, r0
	add r6, r5, r0
	add r7, r5, r0
	add r8, r5, r0
	jal r31, lua_pcallk
	add r15, r1, r0
	add r3, r11, r0
	add r4, r1, r0
	jal r31, report
.LBB0_41:
	add r3, r16, r0
	jal r31, free
	add r3, r12, r0
	jal r31, free
	lui r1, %hi(stdout)
	addi r1, r1, %lo(stdout)
	ldw r3, r1+0
	jal r31, fflush
	add r3, r11, r0
	jal r31, lua_close
	sne r1, r15, r17
	bne r17, r20, .LBB0_30
	jal r0, .LBB0_36
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
                                        # -- End function
	.p2align	2                               # -- Begin function report
	.type	report,@function
report:                                 # @report
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
	add r11, r4, r0
	addi r12, r0, 0
	beq r4, r12, .LBB1_2
.LBB1_1:
	addi r4, r0, -1
	add r13, r3, r0
	add r5, r12, r0
	jal r31, lua_tolstring
	seq r3, r1, r12
	lui r4, %hi(.L.str.6)
	addi r4, r4, %lo(.L.str.6)
	xor r4, r1, r4
	sub r3, r12, r3
	and r3, r4, r3
	xor r5, r1, r3
	lui r1, %hi(stderr)
	addi r1, r1, %lo(stderr)
	ldw r3, r1+0
	lui r4, %hi(.L.str.7)
	addi r4, r4, %lo(.L.str.7)
	jal r31, fprintf
	addi r4, r0, -2
	add r3, r13, r0
	jal r31, lua_settop
.LBB1_2:
	add r1, r11, r0
	ldw lr, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end1:
	.size	report, .Lfunc_end1-report
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"cannot create Lua state: not enough memory\n"
	.size	.L.str, 44

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"out of memory reading input\n"
	.size	.L.str.1, 29

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"return %.*s"
	.size	.L.str.2, 12

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"too many results"
	.size	.L.str.3, 17

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"print"
	.size	.L.str.4, 6

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"=stdin"
	.size	.L.str.5, 7

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"(error object is not a string)"
	.size	.L.str.6, 31

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"%s\n"
	.size	.L.str.7, 4

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
