	.file	"vfont.c"
	.text
	.globl	vfont_text                      # -- Begin function vfont_text
	.p2align	2
	.type	vfont_text,@function
vfont_text:                             # @vfont_text
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
	ldbu r14, r8+0
	beq r14, r0, .LBB0_11
.LBB0_1:
	slli r1, r7, 3
	lui r9, %hi(glyphs)
	addi r9, r9, %lo(glyphs)
	addi r10, r0, 0
	lui lr, 1
	addi lr, lr, -1
	lui r11, 65536
	lui r12, 131072
	jal r0, .LBB0_3
.LBB0_2:
	addi r8, r13, 1
	add r5, r5, r1
	ldbu r14, r13+1
	beq r14, r0, .LBB0_11
.LBB0_3:
	add r13, r8, r0
	andi r8, r14, 255
	add r15, r0, r0
.LBB0_4:
	add r14, r15, r9
	ldbu r16, r14+0
	beq r16, r8, .LBB0_7
.LBB0_5:
	addi r15, r15, 26
	addi r14, r0, 650
	bne r15, r14, .LBB0_4
.LBB0_6:
	add r14, r0, r0
.LBB0_7:
	beq r14, r0, .LBB0_2
.LBB0_8:
	ldbu r8, r14+1
	beq r8, r0, .LBB0_2
.LBB0_9:
	addi r14, r14, 2
.LBB0_10:
	ldbu r15, r14+0
	mul r15, r7, r15
	add r15, r15, r5
	sgt r16, r15, r10
	sub r16, r0, r16
	and r15, r15, r16
	xori r16, r15, 4095
	slt r15, r15, lr
	sub r15, r0, r15
	and r15, r16, r15
	xori r15, r15, 4095
	ldbu r16, r14+1
	mul r16, r7, r16
	add r16, r16, r6
	sgt r17, r16, r10
	sub r17, r0, r17
	and r16, r16, r17
	xori r17, r16, 4095
	slt r16, r16, lr
	sub r16, r0, r16
	and r16, r17, r16
	xori r16, r16, 4095
	ldbu r17, r14+2
	mul r17, r7, r17
	add r17, r17, r5
	sgt r18, r17, r10
	sub r18, r0, r18
	and r17, r17, r18
	xori r18, r17, 4095
	slt r17, r17, lr
	sub r17, r0, r17
	and r17, r18, r17
	xori r17, r17, 4095
	ldbu r18, r14+3
	mul r18, r7, r18
	add r18, r18, r6
	sgt r19, r18, r10
	sub r19, r0, r19
	and r18, r18, r19
	xori r19, r18, 4095
	slt r18, r18, lr
	sub r18, r0, r18
	and r18, r19, r18
	xori r18, r18, 4095
	slli r15, r15, 16
	slli r16, r16, 4
	or  r15, r16, r15
	or  r15, r15, r11
	ldw r16, r4+0
	addi r19, r16, 1
	stw r4+0, r19
	slli r16, r16, 2
	add r16, r3, r16
	stw r16+0, r15
	slli r15, r17, 16
	slli r16, r18, 4
	or  r15, r16, r15
	or  r15, r15, r12
	ldw r16, r4+0
	addi r17, r16, 1
	stw r4+0, r17
	slli r16, r16, 2
	add r16, r3, r16
	stw r16+0, r15
	addi r8, r8, -1
	addi r14, r14, 4
	bne r8, r0, .LBB0_10
	jal r0, .LBB0_2
.LBB0_11:
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
.Lfunc_end0:
	.size	vfont_text, .Lfunc_end0-vfont_text
                                        # -- End function
	.globl	vfont_uint                      # -- Begin function vfont_uint
	.p2align	2
	.type	vfont_uint,@function
vfont_uint:                             # @vfont_uint
# %bb.0:
	addi sp, sp, -64
	stw sp+0, lr
	stw sp+60, r11
	stw sp+56, r12
	stw sp+52, r13
	stw sp+48, r14
	stw sp+44, r15
	stw sp+40, r16
	stb sp+39, r0
	lui r1, 838861
	addi r1, r1, -819
	addi r10, r0, 10
	addi lr, r0, 9
	addi r11, r0, 1
	add r12, r0, r0
.LBB1_1:
	add r14, r9, r12
	mulhu r13, r8, r1
	srli r13, r13, 3
	mul r15, r13, r10
	sub r15, r8, r15
	ori  r15, r15, 48
	addi r16, sp, 28
	add r16, r16, r12
	stb r16+10, r15
	sgtu r8, r8, lr
	sgt r14, r14, r11
	or  r8, r8, r14
	addi r14, r12, -1
	bne r8, r11, .LBB1_3
.LBB1_2:
	addi r15, r12, 11
	add r12, r14, r0
	add r8, r13, r0
	bgtu r15, r11, .LBB1_1
.LBB1_3:
	addi r1, sp, 28
	add r1, r1, r14
	addi r8, r1, 11
	jal r31, vfont_text
	ldw r16, sp+40
	ldw r15, sp+44
	ldw r14, sp+48
	ldw r13, sp+52
	ldw r12, sp+56
	ldw r11, sp+60
	ldw lr, sp+0
	addi sp, sp, 64
	jalr r0, r31, 0
.Lfunc_end1:
	.size	vfont_uint, .Lfunc_end1-vfont_uint
                                        # -- End function
	.type	glyphs,@object                  # @glyphs
	.section	.rodata,"a",@progbits
glyphs:
	.byte	48                              # 0x30
	.byte	4                               # 0x4
	.asciz	"\000\000\006"
	.ascii	"\006\000\006\n"
	.ascii	"\006\n\000\n"
	.asciz	"\000\n\000"
	.zero	4
	.zero	4
	.byte	49                              # 0x31
	.byte	1                               # 0x1
	.ascii	"\003\000\003\n"
	.zero	4
	.zero	4
	.zero	4
	.zero	4
	.zero	4
	.byte	50                              # 0x32
	.byte	5                               # 0x5
	.ascii	"\000\n\006\n"
	.ascii	"\006\n\006\005"
	.ascii	"\006\005\000\005"
	.asciz	"\000\005\000"
	.asciz	"\000\000\006"
	.zero	4
	.byte	51                              # 0x33
	.byte	4                               # 0x4
	.ascii	"\000\n\006\n"
	.asciz	"\006\n\006"
	.asciz	"\006\000\000"
	.ascii	"\000\005\006\005"
	.zero	4
	.zero	4
	.byte	52                              # 0x34
	.byte	3                               # 0x3
	.ascii	"\000\n\000\005"
	.ascii	"\000\005\006\005"
	.asciz	"\006\n\006"
	.zero	4
	.zero	4
	.zero	4
	.byte	53                              # 0x35
	.byte	5                               # 0x5
	.ascii	"\006\n\000\n"
	.ascii	"\000\n\000\005"
	.ascii	"\000\005\006\005"
	.asciz	"\006\005\006"
	.asciz	"\006\000\000"
	.zero	4
	.byte	54                              # 0x36
	.byte	5                               # 0x5
	.ascii	"\006\n\000\n"
	.asciz	"\000\n\000"
	.asciz	"\000\000\006"
	.ascii	"\006\000\006\005"
	.ascii	"\006\005\000\005"
	.zero	4
	.byte	55                              # 0x37
	.byte	2                               # 0x2
	.ascii	"\000\n\006\n"
	.asciz	"\006\n\002"
	.zero	4
	.zero	4
	.zero	4
	.zero	4
	.byte	56                              # 0x38
	.byte	5                               # 0x5
	.asciz	"\000\000\006"
	.ascii	"\006\000\006\n"
	.ascii	"\006\n\000\n"
	.asciz	"\000\n\000"
	.ascii	"\000\005\006\005"
	.zero	4
	.byte	57                              # 0x39
	.byte	4                               # 0x4
	.ascii	"\006\000\006\n"
	.ascii	"\006\n\000\n"
	.ascii	"\000\n\000\005"
	.ascii	"\000\005\006\005"
	.zero	4
	.zero	4
	.byte	65                              # 0x41
	.byte	5                               # 0x5
	.ascii	"\000\000\000\007"
	.ascii	"\000\007\003\n"
	.ascii	"\003\n\006\007"
	.asciz	"\006\007\006"
	.ascii	"\000\004\006\004"
	.zero	4
	.byte	67                              # 0x43
	.byte	3                               # 0x3
	.ascii	"\006\n\000\n"
	.asciz	"\000\n\000"
	.asciz	"\000\000\006"
	.zero	4
	.zero	4
	.zero	4
	.byte	68                              # 0x44
	.byte	6                               # 0x6
	.ascii	"\000\000\000\n"
	.ascii	"\000\n\004\n"
	.ascii	"\004\n\006\b"
	.ascii	"\006\b\006\002"
	.asciz	"\006\002\004"
	.asciz	"\004\000\000"
	.byte	69                              # 0x45
	.byte	4                               # 0x4
	.asciz	"\006\000\000"
	.ascii	"\000\000\000\n"
	.ascii	"\000\n\006\n"
	.ascii	"\000\005\004\005"
	.zero	4
	.zero	4
	.byte	71                              # 0x47
	.byte	5                               # 0x5
	.ascii	"\006\n\000\n"
	.asciz	"\000\n\000"
	.asciz	"\000\000\006"
	.ascii	"\006\000\006\004"
	.ascii	"\006\004\003\004"
	.zero	4
	.byte	72                              # 0x48
	.byte	3                               # 0x3
	.ascii	"\000\000\000\n"
	.ascii	"\006\000\006\n"
	.ascii	"\000\005\006\005"
	.zero	4
	.zero	4
	.zero	4
	.byte	73                              # 0x49
	.byte	3                               # 0x3
	.ascii	"\003\000\003\n"
	.asciz	"\000\000\006"
	.ascii	"\000\n\006\n"
	.zero	4
	.zero	4
	.zero	4
	.byte	77                              # 0x4d
	.byte	4                               # 0x4
	.ascii	"\000\000\000\n"
	.ascii	"\000\n\003\006"
	.ascii	"\003\006\006\n"
	.asciz	"\006\n\006"
	.zero	4
	.zero	4
	.byte	79                              # 0x4f
	.byte	4                               # 0x4
	.asciz	"\000\000\006"
	.ascii	"\006\000\006\n"
	.ascii	"\006\n\000\n"
	.asciz	"\000\n\000"
	.zero	4
	.zero	4
	.byte	80                              # 0x50
	.byte	4                               # 0x4
	.ascii	"\000\000\000\n"
	.ascii	"\000\n\006\n"
	.ascii	"\006\n\006\005"
	.ascii	"\006\005\000\005"
	.zero	4
	.zero	4
	.byte	82                              # 0x52
	.byte	5                               # 0x5
	.ascii	"\000\000\000\n"
	.ascii	"\000\n\006\n"
	.ascii	"\006\n\006\005"
	.ascii	"\006\005\000\005"
	.asciz	"\003\005\006"
	.zero	4
	.byte	83                              # 0x53
	.byte	5                               # 0x5
	.ascii	"\006\n\000\n"
	.ascii	"\000\n\000\005"
	.ascii	"\000\005\006\005"
	.asciz	"\006\005\006"
	.asciz	"\006\000\000"
	.zero	4
	.byte	84                              # 0x54
	.byte	2                               # 0x2
	.ascii	"\000\n\006\n"
	.asciz	"\003\n\003"
	.zero	4
	.zero	4
	.zero	4
	.zero	4
	.byte	85                              # 0x55
	.byte	3                               # 0x3
	.asciz	"\000\n\000"
	.asciz	"\000\000\006"
	.ascii	"\006\000\006\n"
	.zero	4
	.zero	4
	.zero	4
	.byte	86                              # 0x56
	.byte	2                               # 0x2
	.asciz	"\000\n\003"
	.ascii	"\003\000\006\n"
	.zero	4
	.zero	4
	.zero	4
	.zero	4
	.size	glyphs, 650

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git 77f07727152e39f179dfe39786aa3bcae0860790)"
	.section	".note.GNU-stack","",@progbits
