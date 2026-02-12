	.file	"print.c"
	.text
	.globl	lisp_print                      # -- Begin function lisp_print
	.p2align	2
	.type	lisp_print,@function
lisp_print:                             # @lisp_print
# %bb.0:
	addi sp, sp, -24
	stw sp+4, fp
	stw sp+0, lr
	add fp, sp, r0
	addi fp, fp, 24
	stw fp+-4, lr
	addi r4, r0, 0
	jal r31, print_val
	ldw lr, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
	addi sp, sp, 24
	jalr r0, r31, 0
.Lfunc_end0:
	.size	lisp_print, .Lfunc_end0-lisp_print
                                        # -- End function
	.p2align	2                               # -- Begin function print_val
	.type	print_val,@function
print_val:                              # @print_val
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
	addi r1, r0, 1001
	blt r4, r1, .LBB1_2
.LBB1_1:
	lui r3, %hi(.L.str)
	addi r3, r3, %lo(.L.str)
	jal r0, .LBB1_17
.LBB1_2:
	add r11, r3, r0
	addi r16, r0, 0
	beq r3, r16, .LBB1_15
.LBB1_3:
	andi r1, r11, 1
	bne r1, r16, .LBB1_16
.LBB1_4:
	ldw r1, r11+0
	addi r3, r0, 4
	bgtu r1, r3, .LBB1_34
.LBB1_5:
	slli r1, r1, 2
	lui r3, %hi(.LJTI1_0)
	addi r3, r3, %lo(.LJTI1_0)
	add r1, r3, r1
	ldw r1, r1+0
	jalr r0, r1, 0
.LBB1_6:
	addi r3, r0, 40
	add r12, r4, r0
	jal r31, putchar
	ldw r3, r11+12
	addi r12, r12, 1
	add r4, r12, r0
	jal r31, print_val
	ldw r11, r11+16
	addi r14, r0, 0
	beq r11, r14, .LBB1_12
.LBB1_7:
	andi r1, r11, 1
	bne r1, r14, .LBB1_12
.LBB1_8:
	addi r13, r0, 32
.LBB1_9:
	ldw r1, r11+0
	bne r1, r14, .LBB1_12
.LBB1_10:
	add r3, r13, r0
	jal r31, putchar
	ldw r3, r11+12
	add r4, r12, r0
	jal r31, print_val
	ldw r11, r11+16
	beq r11, r14, .LBB1_12
.LBB1_11:
	andi r1, r11, 1
	beq r1, r14, .LBB1_9
.LBB1_12:
	beq r11, r14, .LBB1_14
.LBB1_13:
	lui r3, %hi(.L.str.7)
	addi r3, r3, %lo(.L.str.7)
	jal r31, printf
	add r3, r11, r0
	add r4, r12, r0
	jal r31, print_val
.LBB1_14:
	addi r3, r0, 41
	jal r31, putchar
	jal r0, .LBB1_18
.LBB1_15:
	lui r3, %hi(.L.str.1)
	addi r3, r3, %lo(.L.str.1)
	jal r0, .LBB1_17
.LBB1_16:
	srai r4, r11, 1
	lui r3, %hi(.L.str.2)
	addi r3, r3, %lo(.L.str.2)
.LBB1_17:
	jal r31, printf
.LBB1_18:
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
.LBB1_19:
	ldw r4, r11+12
	lui r3, %hi(.L.str.9)
	addi r3, r3, %lo(.L.str.9)
	jal r0, .LBB1_17
.LBB1_20:
	addi r12, r0, 34
	add r3, r12, r0
	jal r31, putchar
	ldw r1, r11+16
	addi r3, r0, 1
	blt r1, r3, .LBB1_31
.LBB1_21:
	addi r17, r0, 10
	lui r13, %hi(.L.str.6)
	addi r13, r13, %lo(.L.str.6)
	addi r18, r0, 92
	lui r14, %hi(.L.str.5)
	addi r14, r14, %lo(.L.str.5)
	lui r15, %hi(.L.str.4)
	addi r15, r15, %lo(.L.str.4)
	jal r0, .LBB1_25
.LBB1_22:
	add r3, r14, r0
.LBB1_23:
	jal r31, printf
.LBB1_24:
	addi r16, r16, 1
	ldw r1, r11+16
	bge r16, r1, .LBB1_31
.LBB1_25:
	ldw r1, r11+12
	add r1, r1, r16
	ldbu r1, r1+0
	beq r1, r17, .LBB1_29
.LBB1_26:
	beq r1, r18, .LBB1_22
.LBB1_27:
	bne r1, r12, .LBB1_30
.LBB1_28:
	add r3, r15, r0
	jal r0, .LBB1_23
.LBB1_29:
	add r3, r13, r0
	jal r0, .LBB1_23
.LBB1_30:
	slli r1, r1, 24
	srai r3, r1, 24
	jal r31, putchar
	jal r0, .LBB1_24
.LBB1_31:
	addi r3, r0, 34
	jal r31, putchar
	jal r0, .LBB1_18
.LBB1_32:
	lui r3, %hi(.L.str.8)
	addi r3, r3, %lo(.L.str.8)
	jal r0, .LBB1_17
.LBB1_33:
	ldw r4, r11+12
	lui r3, %hi(.L.str.3)
	addi r3, r3, %lo(.L.str.3)
	jal r0, .LBB1_17
.LBB1_34:
	lui r3, %hi(.L.str.10)
	addi r3, r3, %lo(.L.str.10)
	jal r0, .LBB1_17
.Lfunc_end1:
	.size	print_val, .Lfunc_end1-print_val
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI1_0:
	.word	.LBB1_6
	.word	.LBB1_33
	.word	.LBB1_20
	.word	.LBB1_32
	.word	.LBB1_19
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"..."
	.size	.L.str, 4

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"()"
	.size	.L.str.1, 3

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"%d"
	.size	.L.str.2, 3

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"%s"
	.size	.L.str.3, 3

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"\\\""
	.size	.L.str.4, 3

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"\\\\"
	.size	.L.str.5, 3

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"\\n"
	.size	.L.str.6, 3

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	" . "
	.size	.L.str.7, 4

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"#<lambda>"
	.size	.L.str.8, 10

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"#<builtin %s>"
	.size	.L.str.9, 14

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"#<unknown>"
	.size	.L.str.10, 11

	.ident	"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 0c27e7716b1b351bd93e1a7d5c7965bde4656ae9)"
	.section	".note.GNU-stack","",@progbits
