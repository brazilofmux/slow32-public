	.file	"linit.c"
	.text
	.globl	luaL_openlibs                   # -- Begin function luaL_openlibs
	.p2align	2
	.type	luaL_openlibs,@function
luaL_openlibs:                          # @luaL_openlibs
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
	lui r14, %hi(loadedlibs)
	addi r14, r14, %lo(loadedlibs)
	lui r5, %hi(luaopen_base)
	addi r5, r5, %lo(luaopen_base)
	addi r12, r0, 1
	addi r13, r0, -2
	addi r15, r0, 0
.LBB0_1:
	ldw r4, r14+0
	add r3, r11, r0
	add r6, r12, r0
	jal r31, luaL_requiref
	add r3, r11, r0
	add r4, r13, r0
	jal r31, lua_settop
	addi r1, r14, 8
	ldw r5, r14+12
	add r14, r1, r0
	bne r5, r15, .LBB0_1
.LBB0_2:
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
.Lfunc_end0:
	.size	luaL_openlibs, .Lfunc_end0-luaL_openlibs
                                        # -- End function
	.type	loadedlibs,@object              # @loadedlibs
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
loadedlibs:
	.word	.L.str
	.word	luaopen_base
	.word	.L.str.1
	.word	luaopen_package
	.word	.L.str.2
	.word	luaopen_coroutine
	.word	.L.str.3
	.word	luaopen_table
	.word	.L.str.4
	.word	luaopen_io
	.word	.L.str.5
	.word	luaopen_os
	.word	.L.str.6
	.word	luaopen_string
	.word	.L.str.7
	.word	luaopen_math
	.word	.L.str.8
	.word	luaopen_utf8
	.word	.L.str.9
	.word	luaopen_debug
	.zero	8
	.size	loadedlibs, 88

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"_G"
	.size	.L.str, 3

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"package"
	.size	.L.str.1, 8

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"coroutine"
	.size	.L.str.2, 10

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"table"
	.size	.L.str.3, 6

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"io"
	.size	.L.str.4, 3

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"os"
	.size	.L.str.5, 3

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"string"
	.size	.L.str.6, 7

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"math"
	.size	.L.str.7, 5

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"utf8"
	.size	.L.str.8, 5

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"debug"
	.size	.L.str.9, 6

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
