	.file	"vfont.c"
	.text
	.globl	vfont_text                      # -- Begin function vfont_text
	.p2align	2
	.type	vfont_text,@function
vfont_text:                             # @vfont_text
# %bb.0:
	addi sp, sp, -64
	stw sp+0, lr
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 64
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	stw fp+-32, r18
	stw fp+-36, r19
	ldbu r14, r8+0
	addi r1, r0, 0
	beq r14, r1, .LBB0_11
.LBB0_1:
	slli r9, r7, 3
	lui r10, %hi(glyphs)
	addi r10, r10, %lo(glyphs)
	lui lr, 1
	addi lr, lr, -1
	lui r11, 65536
	lui r12, 131072
	jal r0, .LBB0_3
.LBB0_2:
	addi r8, r13, 1
	add r5, r5, r9
	ldbu r14, r13+1
	beq r14, r1, .LBB0_11
.LBB0_3:
	add r13, r8, r0
	andi r8, r14, 255
	add r15, r1, r0
.LBB0_4:
	add r14, r15, r10
	ldbu r16, r14+0
	beq r16, r8, .LBB0_7
.LBB0_5:
	addi r15, r15, 26
	addi r14, r0, 650
	bne r15, r14, .LBB0_4
.LBB0_6:
	addi r14, r0, 0
.LBB0_7:
	beq r14, r1, .LBB0_2
.LBB0_8:
	ldbu r8, r14+1
	beq r8, r1, .LBB0_2
.LBB0_9:
	addi r14, r14, 2
.LBB0_10:
	ldbu r15, r14+0
	mul r15, r7, r15
	add r15, r15, r5
	sgt r16, r15, r1
	sub r16, r1, r16
	and r15, r15, r16
	slt r16, r15, lr
	sub r16, r1, r16
	xori r15, r15, 4095
	and r15, r15, r16
	xori r15, r15, 4095
	ldbu r16, r14+1
	mul r16, r7, r16
	add r16, r16, r6
	sgt r17, r16, r1
	sub r17, r1, r17
	and r16, r16, r17
	slt r17, r16, lr
	sub r17, r1, r17
	xori r16, r16, 4095
	and r16, r16, r17
	xori r16, r16, 4095
	ldbu r17, r14+2
	mul r17, r7, r17
	add r17, r17, r5
	sgt r18, r17, r1
	sub r18, r1, r18
	and r17, r17, r18
	slt r18, r17, lr
	sub r18, r1, r18
	xori r17, r17, 4095
	and r17, r17, r18
	xori r17, r17, 4095
	ldbu r18, r14+3
	mul r18, r7, r18
	add r18, r18, r6
	sgt r19, r18, r1
	sub r19, r1, r19
	and r18, r18, r19
	slt r19, r18, lr
	sub r19, r1, r19
	xori r18, r18, 4095
	and r18, r18, r19
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
	bne r8, r1, .LBB0_10
	jal r0, .LBB0_2
.LBB0_11:
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
	stw sp+4, fp
	add fp, sp, r0
	addi fp, fp, 64
	stw fp+-4, r11
	stw fp+-8, r12
	stw fp+-12, r13
	stw fp+-16, r14
	stw fp+-20, r15
	stw fp+-24, r16
	stw fp+-28, r17
	addi r1, r0, 0
	stb fp+-29, r1
	lui r10, 838861
	addi r10, r10, -819
	addi r11, r0, 10
	addi lr, fp, -40
	addi r12, r0, 9
	addi r13, r0, 1
.LBB1_1:
	add r15, r9, r1
	mulhu r14, r8, r10
	srli r14, r14, 3
	mul r16, r14, r11
	sub r16, r8, r16
	ori  r16, r16, 48
	add r17, lr, r1
	stb r17+10, r16
	sgtu r8, r8, r12
	sgt r15, r15, r13
	or  r8, r8, r15
	addi r15, r1, -1
	bne r8, r13, .LBB1_3
.LBB1_2:
	addi r16, r1, 11
	add r1, r15, r0
	add r8, r14, r0
	bgtu r16, r13, .LBB1_1
.LBB1_3:
	add r1, lr, r15
	addi r8, r1, 11
	jal r31, vfont_text
	ldw r17, fp+-28
	ldw r16, fp+-24
	ldw r15, fp+-20
	ldw r14, fp+-16
	ldw r13, fp+-12
	ldw r12, fp+-8
	ldw r11, fp+-4
	ldw lr, sp+0
	ldw fp, sp+4
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

	.ident	"clang version 24.0.0git (https://github.com/llvm/llvm-project.git e34f541beea69553ff1fd655361b4faa1e656dc2)"
	.section	".note.GNU-stack","",@progbits
