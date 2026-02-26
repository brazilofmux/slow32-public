	.file	"12_exception_basic.cpp"
	.section	.text.unlikely.,"ax",@progbits
	.globl	_Z9throw_inti                   # -- Begin function _Z9throw_inti
	.p2align	2
	.type	_Z9throw_inti,@function
_Z9throw_inti:                          # @_Z9throw_inti
	.cfi_startproc
# %bb.0:
	addi sp, sp, -24
	.cfi_def_cfa_offset 24
	stw sp+4, fp
	.cfi_offset fp, -20
	stw sp+0, lr
	.cfi_offset lr, -24
	add fp, sp, r0
	addi fp, fp, 24
	.cfi_def_cfa fp, 0
	stw fp+-4, r11
	stw fp+-8, lr
	add r11, r3, r0
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	add r4, r11, r0
	jal r31, printf
	addi r3, r0, 4
	jal r31, __cxa_allocate_exception
	stw r1+0, r11
	lui r4, %hi(_ZTIi)
	addi r4, r4, %lo(_ZTIi)
	addi r5, r0, 0
	add r3, r1, r0
	jal r31, __cxa_throw
.Lfunc_end0:
	.size	_Z9throw_inti, .Lfunc_end0-_Z9throw_inti
	.cfi_endproc
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
.Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 0, __gxx_personality_v0
	.cfi_lsda 0, .Lexception0
# %bb.0:
	addi sp, sp, -24
	.cfi_def_cfa_offset 24
	stw sp+4, fp
	.cfi_offset fp, -20
	stw sp+0, lr
	.cfi_offset lr, -24
	add fp, sp, r0
	addi fp, fp, 24
	.cfi_def_cfa fp, 0
	stw fp+-4, lr
	lui r3, %hi(.Lstr)
	addi r3, r3, %lo(.Lstr)
	jal r31, puts
.Ltmp0:                                 # EH_LABEL
	addi r3, r0, 42
	jal r31, _Z9throw_inti
.Ltmp1:                                 # EH_LABEL
.LBB1_1:
.LBB1_2:
.Ltmp2:                                 # EH_LABEL
	jal r31, __cxa_begin_catch
	ldw r4, r1+0
	lui r3, %hi(.L.str.3)
	addi r3, r3, %lo(.L.str.3)
	jal r31, printf
	jal r31, __cxa_end_catch
	lui r3, %hi(.Lstr.8)
	addi r3, r3, %lo(.Lstr.8)
	jal r31, puts
	addi r3, r0, 4
	jal r31, __cxa_allocate_exception
	addi r3, r0, 99
	stw r1+0, r3
.Ltmp3:                                 # EH_LABEL
	lui r4, %hi(_ZTIi)
	addi r4, r4, %lo(_ZTIi)
	addi r5, r0, 0
	add r3, r1, r0
	jal r31, __cxa_throw
.Ltmp4:                                 # EH_LABEL
.LBB1_3:
.LBB1_4:
.Ltmp5:                                 # EH_LABEL
	jal r31, __cxa_begin_catch
	lui r3, %hi(.Lstr.9)
	addi r3, r3, %lo(.Lstr.9)
	jal r31, puts
	jal r31, __cxa_end_catch
	lui r3, %hi(.Lstr.10)
	addi r3, r3, %lo(.Lstr.10)
	jal r31, puts
	addi r1, r0, 0
	ldw lr, fp+-4
	.cfi_def_cfa sp, 24
	ldw lr, sp+0
	.cfi_restore lr
	ldw fp, sp+4
	.cfi_restore fp
	addi sp, sp, 24
	.cfi_def_cfa_offset 0
	jalr r0, r31, 0
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2, 0x0
GCC_except_table1:
.Lexception0:
	.byte	255                             # @LPStart Encoding = omit
	.byte	0                               # @TType Encoding = absptr
	.uleb128 .Lttbase0-.Lttbaseref0
.Lttbaseref0:
	.byte	1                               # Call site Encoding = uleb128
	.uleb128 .Lcst_end0-.Lcst_begin0
.Lcst_begin0:
	.uleb128 .Ltmp0-.Lfunc_begin0           # >> Call Site 1 <<
	.uleb128 .Ltmp1-.Ltmp0                  #   Call between .Ltmp0 and .Ltmp1
	.uleb128 .Ltmp2-.Lfunc_begin0           #     jumps to .Ltmp2
	.byte	1                               #   On action: 1
	.uleb128 .Ltmp1-.Lfunc_begin0           # >> Call Site 2 <<
	.uleb128 .Ltmp3-.Ltmp1                  #   Call between .Ltmp1 and .Ltmp3
	.byte	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
	.uleb128 .Ltmp3-.Lfunc_begin0           # >> Call Site 3 <<
	.uleb128 .Ltmp4-.Ltmp3                  #   Call between .Ltmp3 and .Ltmp4
	.uleb128 .Ltmp5-.Lfunc_begin0           #     jumps to .Ltmp5
	.byte	3                               #   On action: 2
	.uleb128 .Ltmp4-.Lfunc_begin0           # >> Call Site 4 <<
	.uleb128 .Lfunc_end1-.Ltmp4             #   Call between .Ltmp4 and .Lfunc_end1
	.byte	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
.Lcst_end0:
	.byte	1                               # >> Action Record 1 <<
                                        #   Catch TypeInfo 1
	.byte	0                               #   No further actions
	.byte	2                               # >> Action Record 2 <<
                                        #   Catch TypeInfo 2
	.byte	0                               #   No further actions
	.p2align	2, 0x0
                                        # >> Catch TypeInfos <<
	.word	0                               # TypeInfo 2
	.word	_ZTIi                           # TypeInfo 1
.Lttbase0:
	.p2align	2, 0x0
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Throwing %d\n"
	.size	.L.str, 13

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"Caught int: %d\n"
	.size	.L.str.3, 16

	.type	.Lstr,@object                   # @str
	.section	.rodata.str1.4,"aMS",@progbits,1
	.p2align	2, 0x0
.Lstr:
	.asciz	"Exception test start"
	.size	.Lstr, 21

	.type	.Lstr.8,@object                 # @str.8
	.p2align	2, 0x0
.Lstr.8:
	.asciz	"After first catch"
	.size	.Lstr.8, 18

	.type	.Lstr.9,@object                 # @str.9
	.p2align	2, 0x0
.Lstr.9:
	.asciz	"Caught with catch-all"
	.size	.Lstr.9, 22

	.type	.Lstr.10,@object                # @str.10
	.p2align	2, 0x0
.Lstr.10:
	.asciz	"Exception test done"
	.size	.Lstr.10, 20

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 4f33d807e4104ffdac97c1cac88ef88896c0638e)"
	.section	".note.GNU-stack","",@progbits
