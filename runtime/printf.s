# SLOW-32 Assembly (v3 with correct PHI handling)

# Function: vprintf
.global vprintf
vprintf:
    addi sp, sp, -784
    stw sp+0, lr
    stw sp+4, fp
    add fp, sp, r0
.L_0x627a4b5a1170:
    addi r2, fp, 8
    stw fp+160, r2  # spill
    addi r2, fp, 20
    stw fp+164, r2  # spill
    addi r2, fp, 32
    stw fp+168, r2  # spill
    addi r2, fp, 44
    stw fp+172, r2  # spill
    ldw r2, fp+172  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+188, r2  # spill
    ldw r2, fp+168  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+196, r2  # spill
    ldw r2, fp+164  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+204, r2  # spill
    ldw r2, fp+160  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+208, r2  # spill
    add r12, r3, r0  # PHI move
    addi r13, r0, 0
    # PHI const
    ldw r14, fp+180  # PHI reload
    beq r0, r0, .L_0x627a4b5a1cc0
.L_0x627a4b5a1cc0:
    add r15, r12, r0  # PHI move
    add r16, r13, r0  # PHI move
    beq r0, r0, .L_0x627a4b5a25c0
.L_0x627a4b5a25c0:
    ldbu r2, r15+0
    stw fp+216, r2  # spill
.L_0x627a4b5a2700:
    ldw r2, fp+216  # reload
    andi r2, r2, 255
    stw fp+220, r2  # spill
    ldw r3, fp+220  # reload
    jal putchar
    addi r11, r0, 1
    add r2, r16, r11
    stw fp+224, r2  # spill
    add r2, r15, r0
    stw fp+228, r2  # spill
    ldw r15, fp+228  # PHI reload
    ldw r16, fp+224  # PHI reload
    beq r0, r0, .L_0x627a4b5a25c0
.L_0x627a4b5a3c70:
    add r2, r15, r0
    stw fp+232, r2  # spill
    ldw r2, fp+232  # reload
    ldbu r2, r2+0
    stw fp+236, r2  # spill
.L_0x627a4b5a49e0:
    add r2, r14, r0
    stw fp+240, r2  # spill
    ldw r2, r14+0
    stw fp+244, r2  # spill
    addi r3, r0, 12
    ldw r4, fp+160  # reload
    jal llvm.lifetime.start.p0
    ldw r2, fp+244  # reload
    slt r2, r2, r0
    stw fp+248, r2  # spill
    ldw r2, fp+248  # reload
    beq r2, r0, .L_0x627a4b5a49e0_to_0x627a4b5a5810
    beq r0, r0, .L_0x627a4b5a5770
.L_0x627a4b5a49e0_to_0x627a4b5a5810:  # Critical edge split
    ldw r17, fp+244  # PHI reload
    ldw r18, fp+160  # PHI reload
    beq r0, r0, .L_0x627a4b5a5810
.L_0x627a4b5a5770:
    addi r2, r0, 45
    ldw r2, fp+160  # reload
    stb r2+0, r2
    ldw r11, fp+244  # reload
    sub r2, r0, r11
    stw fp+252, r2  # spill
    ldw r17, fp+252  # PHI reload
    ldw r18, fp+208  # PHI reload
    beq r0, r0, .L_0x627a4b5a5810
.L_0x627a4b5a5810:
    seq r2, r17, r0
    stw fp+256, r2  # spill
    ldw r2, fp+256  # reload
    beq r2, r0, .L_0x627a4b5a5810_to_0x627a4b5a5c50
    beq r0, r0, .L_0x627a4b5a5bf0
.L_0x627a4b5a5810_to_0x627a4b5a5c50:  # Critical edge split
    add r19, r18, r0  # PHI move
    add r20, r17, r0  # PHI move
    beq r0, r0, .L_0x627a4b5a5c50
.L_0x627a4b5a5bf0:
    add r2, r18, r0
    stw fp+260, r2  # spill
    addi r2, r0, 48
    stb r18+0, r2
    ldw r2, fp+260  # reload
    stb r2+0, r0
    addi r21, r0, 1
    # PHI const
    beq r0, r0, .L_0x627a4b5a5f70
.L_0x627a4b5a5c50:
    ldw r2, fp+264  # reload
    addi r11, r0, 10
    ldw r2, fp+268  # reload
    addi r11, r0, 10
    mul r2, r2, r11
    stw fp+272, r2  # spill
    ldw r2, fp+264  # reload
    ldw r11, fp+272  # reload
    sub r2, r2, r11
    stw fp+276, r2  # spill
    ldw r2, fp+276  # reload
    andi r2, r2, 255
    stw fp+280, r2  # spill
    ldw r2, fp+280  # reload
    addi r11, r0, 48
    or r2, r2, r11
    stw fp+284, r2  # spill
    add r2, r19, r0
    stw fp+288, r2  # spill
    ldw r2, fp+284  # reload
    stb r19+0, r2
    addi r11, r0, 10
    sltu r2, r20, r11
    stw fp+292, r2  # spill
    ldw r2, fp+292  # reload
    beq r2, r0, .L_0x627a4b5a5c50_to_0x627a4b5a5c50
    beq r0, r0, .L_0x627a4b5a6f00
.L_0x627a4b5a5c50_to_0x627a4b5a5c50:  # Critical edge split
    ldw r19, fp+288  # PHI reload
    ldw r20, fp+268  # PHI reload
    beq r0, r0, .L_0x627a4b5a5c50
.L_0x627a4b5a6f00:
    ldw r2, fp+288  # reload
    stb r2+0, r0
    sgtu r2, r19, r18
    stw fp+296, r2  # spill
    ldw r2, fp+296  # reload
    beq r2, r0, .L_0x627a4b5a72a0
    beq r0, r0, .L_0x627a4b5a6f00_to_0x627a4b5a7240
.L_0x627a4b5a6f00_to_0x627a4b5a7240:  # Critical edge split
    add r22, r18, r0  # PHI move
    add r23, r19, r0  # PHI move
    beq r0, r0, .L_0x627a4b5a7240
.L_0x627a4b5a7240:
    ldbu r2, r23+0
    stw fp+300, r2  # spill
    ldbu r2, r22+0
    stw fp+304, r2  # spill
    ldw r2, fp+304  # reload
    stb r23+0, r2
    ldw r2, fp+300  # reload
    stb r22+0, r2
    add r2, r22, r0
    stw fp+308, r2  # spill
    add r2, r23, r0
    stw fp+312, r2  # spill
    ldw r2, fp+308  # reload
    ldw r11, fp+312  # reload
    sltu r2, r2, r11
    stw fp+316, r2  # spill
    ldw r2, fp+316  # reload
    beq r2, r0, .L_0x627a4b5a72a0
    beq r0, r0, .L_0x627a4b5a7240_to_0x627a4b5a7240
.L_0x627a4b5a7240_to_0x627a4b5a7240:  # Critical edge split
    ldw r22, fp+308  # PHI reload
    ldw r23, fp+312  # PHI reload
    beq r0, r0, .L_0x627a4b5a7240
.L_0x627a4b5a72a0:
    ldw r2, fp+320  # reload
    ldw r11, fp+324  # reload
    sub r2, r2, r11
    stw fp+328, r2  # spill
    ldw r21, fp+328  # PHI reload
    beq r0, r0, .L_0x627a4b5a5f70
.L_0x627a4b5a5f70:
    add r2, r18, r0
    stw fp+332, r2  # spill
    ldw r2, fp+332  # reload
    ldw r11, fp+160  # reload
    seq r2, r2, r11
    stw fp+336, r2  # spill
    ldw r2, fp+336  # reload
    beq r2, r0, .L_0x627a4b5a7f60
    beq r0, r0, .L_0x627a4b5a5f70_to_0x627a4b5a7f00
.L_0x627a4b5a5f70_to_0x627a4b5a7f00:  # Critical edge split
    add r24, r16, r0  # PHI move
    beq r0, r0, .L_0x627a4b5a7f00
.L_0x627a4b5a7f60:
    ldw r2, fp+340  # reload
    ldw r11, fp+212  # reload
    sub r2, r2, r11
    stw fp+344, r2  # spill
    ldw r3, fp+344  # reload
    addi r4, r0, 1
    jal llvm.umax.i32
    stw fp+348, r1  # spill
    add r25, r16, r0  # PHI move
    addi r26, r0, 0
    # PHI const
    beq r0, r0, .L_0x627a4b5a8380
.L_0x627a4b5a7f00:
    addi r3, r0, 12
    ldw r4, fp+160  # reload
    jal llvm.lifetime.end.p0
    add r27, r24, r0  # PHI move
    ldw r28, fp+240  # PHI reload
    stw fp+56, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a1dc0
.L_0x627a4b5a8380:
    ldw r2, fp+160  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r26
    stw fp+352, r2  # spill
    ldw r2, fp+352  # reload
    ldbu r2, r2+0
    stw fp+356, r2  # spill
    ldw r2, fp+356  # reload
    andi r2, r2, 255
    stw fp+360, r2  # spill
    ldw r3, fp+360  # reload
    jal putchar
    addi r11, r0, 1
    add r2, r25, r11
    stw fp+364, r2  # spill
    addi r11, r0, 1
    add r2, r26, r11
    stw fp+368, r2  # spill
    ldw r2, fp+368  # reload
    ldw r11, fp+348  # reload
    seq r2, r2, r11
    stw fp+372, r2  # spill
    ldw r2, fp+372  # reload
    beq r2, r0, .L_0x627a4b5a8380_to_0x627a4b5a8380
    beq r0, r0, .L_0x627a4b5a8380_to_0x627a4b5a7f00
.L_0x627a4b5a8380_to_0x627a4b5a7f00:  # Critical edge split
    ldw r24, fp+364  # PHI reload
    beq r0, r0, .L_0x627a4b5a7f00
.L_0x627a4b5a8380_to_0x627a4b5a8380:  # Critical edge split
    ldw r25, fp+364  # PHI reload
    ldw r26, fp+368  # PHI reload
    beq r0, r0, .L_0x627a4b5a8380
.L_0x627a4b5a4ab0:
    add r2, r14, r0
    stw fp+376, r2  # spill
    ldw r2, r14+0
    stw fp+380, r2  # spill
    addi r3, r0, 11
    ldw r4, fp+164  # reload
    jal llvm.lifetime.start.p0
    ldw r2, fp+380  # reload
    seq r2, r2, r0
    stw fp+384, r2  # spill
    ldw r2, fp+384  # reload
    beq r2, r0, .L_0x627a4b5a4ab0_to_0x627a4b5a98c0
    beq r0, r0, .L_0x627a4b5a9860
.L_0x627a4b5a4ab0_to_0x627a4b5a98c0:  # Critical edge split
    ldw r28, fp+164  # PHI reload
    stw fp+60, r28  # PHI spill
    ldw r28, fp+380  # PHI reload
    stw fp+64, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a98c0
.L_0x627a4b5a9860:
    addi r2, r0, 48
    ldw r2, fp+164  # reload
    stb r2+0, r2
    ldw r2, fp+204  # reload
    stb r2+0, r0
    addi r28, r0, 1
    # PHI const
    stw fp+68, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a9b50
.L_0x627a4b5a98c0:
    ldw r2, fp+388  # reload
    addi r11, r0, 10
    ldw r2, fp+392  # reload
    addi r11, r0, 10
    mul r2, r2, r11
    stw fp+396, r2  # spill
    ldw r2, fp+388  # reload
    ldw r11, fp+396  # reload
    sub r2, r2, r11
    stw fp+400, r2  # spill
    ldw r2, fp+400  # reload
    andi r2, r2, 255
    stw fp+404, r2  # spill
    ldw r2, fp+404  # reload
    addi r11, r0, 48
    or r2, r2, r11
    stw fp+408, r2  # spill
    ldw r2, fp+60  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+412, r2  # spill
    ldw r2, fp+408  # reload
    ldw r2, fp+60  # reload
    stb r2+0, r2
    ldw r2, fp+64  # reload
    addi r11, r0, 10
    sltu r2, r2, r11
    stw fp+416, r2  # spill
    ldw r2, fp+416  # reload
    beq r2, r0, .L_0x627a4b5a98c0_to_0x627a4b5a98c0
    beq r0, r0, .L_0x627a4b5a67c0
.L_0x627a4b5a98c0_to_0x627a4b5a98c0:  # Critical edge split
    ldw r28, fp+412  # PHI reload
    stw fp+60, r28  # PHI spill
    ldw r28, fp+392  # PHI reload
    stw fp+64, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a98c0
.L_0x627a4b5a67c0:
    ldw r2, fp+412  # reload
    stb r2+0, r0
    ldw r2, fp+60  # reload
    ldw r11, fp+164  # reload
    sgtu r2, r2, r11
    stw fp+420, r2  # spill
    ldw r2, fp+420  # reload
    beq r2, r0, .L_0x627a4b5a6a60
    beq r0, r0, .L_0x627a4b5a67c0_to_0x627a4b5a6a00
.L_0x627a4b5a67c0_to_0x627a4b5a6a00:  # Critical edge split
    ldw r28, fp+164  # PHI reload
    stw fp+72, r28  # PHI spill
    ldw r28, fp+60  # PHI reload
    stw fp+76, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a6a00
.L_0x627a4b5a6a00:
    ldw r2, fp+76  # reload
    ldbu r2, r2+0
    stw fp+424, r2  # spill
    ldw r2, fp+72  # reload
    ldbu r2, r2+0
    stw fp+428, r2  # spill
    ldw r2, fp+428  # reload
    ldw r2, fp+76  # reload
    stb r2+0, r2
    ldw r2, fp+424  # reload
    ldw r2, fp+72  # reload
    stb r2+0, r2
    ldw r2, fp+72  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+432, r2  # spill
    ldw r2, fp+76  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+436, r2  # spill
    ldw r2, fp+432  # reload
    ldw r11, fp+436  # reload
    sltu r2, r2, r11
    stw fp+440, r2  # spill
    ldw r2, fp+440  # reload
    beq r2, r0, .L_0x627a4b5a6a60
    beq r0, r0, .L_0x627a4b5a6a00_to_0x627a4b5a6a00
.L_0x627a4b5a6a00_to_0x627a4b5a6a00:  # Critical edge split
    ldw r28, fp+432  # PHI reload
    stw fp+72, r28  # PHI spill
    ldw r28, fp+436  # PHI reload
    stw fp+76, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a6a00
.L_0x627a4b5a6a60:
    ldw r2, fp+444  # reload
    ldw r11, fp+200  # reload
    sub r2, r2, r11
    stw fp+448, r2  # spill
    ldw r28, fp+448  # PHI reload
    stw fp+68, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a9b50
.L_0x627a4b5a9b50:
    ldw r3, fp+68  # reload
    addi r4, r0, 1
    jal llvm.umax.i32
    stw fp+452, r1  # spill
    stw fp+80, r16  # PHI spill
    addi r28, r0, 0
    # PHI const
    stw fp+84, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5ac480
.L_0x627a4b5ac550:
    addi r3, r0, 11
    ldw r4, fp+164  # reload
    jal llvm.lifetime.end.p0
    ldw r27, fp+468  # PHI reload
    ldw r28, fp+376  # PHI reload
    stw fp+56, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a1dc0
.L_0x627a4b5ac480:
    ldw r2, fp+164  # reload
    add r12, r2, r0  # save ptr
    ldw r2, fp+84  # reload
    add r2, r12, r2
    stw fp+456, r2  # spill
    ldw r2, fp+456  # reload
    ldbu r2, r2+0
    stw fp+460, r2  # spill
    ldw r2, fp+460  # reload
    andi r2, r2, 255
    stw fp+464, r2  # spill
    ldw r3, fp+464  # reload
    jal putchar
    ldw r2, fp+80  # reload
    addi r11, r0, 1
    add r2, r2, r11
    stw fp+468, r2  # spill
    ldw r2, fp+84  # reload
    addi r11, r0, 1
    add r2, r2, r11
    stw fp+472, r2  # spill
    ldw r2, fp+472  # reload
    ldw r11, fp+452  # reload
    seq r2, r2, r11
    stw fp+476, r2  # spill
    ldw r2, fp+476  # reload
    beq r2, r0, .L_0x627a4b5ac480_to_0x627a4b5ac480
    beq r0, r0, .L_0x627a4b5ac550
.L_0x627a4b5ac480_to_0x627a4b5ac480:  # Critical edge split
    ldw r28, fp+468  # PHI reload
    stw fp+80, r28  # PHI spill
    ldw r28, fp+472  # PHI reload
    stw fp+84, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5ac480
.L_0x627a4b5a4b80:
    add r2, r14, r0
    stw fp+480, r2  # spill
    ldw r2, r14+0
    stw fp+484, r2  # spill
    addi r3, r0, 9
    ldw r4, fp+168  # reload
    jal llvm.lifetime.start.p0
    ldw r2, fp+236  # reload
    addi r11, r0, 88
    seq r2, r2, r11
    stw fp+488, r2  # spill
    ldw r2, fp+484  # reload
    seq r2, r2, r0
    stw fp+496, r2  # spill
    ldw r2, fp+496  # reload
    beq r2, r0, .L_0x627a4b5a4b80_to_0x627a4b5ad300
    beq r0, r0, .L_0x627a4b5ad2a0
.L_0x627a4b5a4b80_to_0x627a4b5ad300:  # Critical edge split
    ldw r28, fp+484  # PHI reload
    stw fp+88, r28  # PHI spill
    ldw r28, fp+168  # PHI reload
    stw fp+92, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5ad300
.L_0x627a4b5ad2a0:
    addi r2, r0, 48
    ldw r2, fp+168  # reload
    stb r2+0, r2
    ldw r2, fp+196  # reload
    stb r2+0, r0
    addi r28, r0, 1
    # PHI const
    stw fp+96, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5ad550
.L_0x627a4b5ad300:
    ldw r2, fp+88  # reload
    addi r11, r0, 15
    and r2, r2, r11
    stw fp+500, r2  # spill
    ldw r2, fp+492  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+504, r2  # spill
    ldw r2, fp+504  # reload
    ldbu r2, r2+0
    stw fp+508, r2  # spill
    ldw r2, fp+92  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+512, r2  # spill
    ldw r2, fp+508  # reload
    ldw r2, fp+92  # reload
    stb r2+0, r2
    ldw r2, fp+88  # reload
    addi r11, r0, 4
    srl r2, r2, r11
    stw fp+516, r2  # spill
    ldw r2, fp+88  # reload
    addi r11, r0, 16
    sltu r2, r2, r11
    stw fp+520, r2  # spill
    ldw r2, fp+520  # reload
    beq r2, r0, .L_0x627a4b5ad300_to_0x627a4b5ad300
    beq r0, r0, .L_0x627a4b5adc20
.L_0x627a4b5ad300_to_0x627a4b5ad300:  # Critical edge split
    ldw r28, fp+516  # PHI reload
    stw fp+88, r28  # PHI spill
    ldw r28, fp+512  # PHI reload
    stw fp+92, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5ad300
.L_0x627a4b5adc20:
    ldw r2, fp+512  # reload
    stb r2+0, r0
    ldw r2, fp+92  # reload
    ldw r11, fp+168  # reload
    sgtu r2, r2, r11
    stw fp+524, r2  # spill
    ldw r2, fp+524  # reload
    beq r2, r0, .L_0x627a4b5adfc0
    beq r0, r0, .L_0x627a4b5adc20_to_0x627a4b5adf60
.L_0x627a4b5adc20_to_0x627a4b5adf60:  # Critical edge split
    ldw r28, fp+168  # PHI reload
    stw fp+100, r28  # PHI spill
    ldw r28, fp+92  # PHI reload
    stw fp+104, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5adf60
.L_0x627a4b5adf60:
    ldw r2, fp+104  # reload
    ldbu r2, r2+0
    stw fp+528, r2  # spill
    ldw r2, fp+100  # reload
    ldbu r2, r2+0
    stw fp+532, r2  # spill
    ldw r2, fp+532  # reload
    ldw r2, fp+104  # reload
    stb r2+0, r2
    ldw r2, fp+528  # reload
    ldw r2, fp+100  # reload
    stb r2+0, r2
    ldw r2, fp+100  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+536, r2  # spill
    ldw r2, fp+104  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+540, r2  # spill
    ldw r2, fp+536  # reload
    ldw r11, fp+540  # reload
    sltu r2, r2, r11
    stw fp+544, r2  # spill
    ldw r2, fp+544  # reload
    beq r2, r0, .L_0x627a4b5adfc0
    beq r0, r0, .L_0x627a4b5adf60_to_0x627a4b5adf60
.L_0x627a4b5adf60_to_0x627a4b5adf60:  # Critical edge split
    ldw r28, fp+536  # PHI reload
    stw fp+100, r28  # PHI spill
    ldw r28, fp+540  # PHI reload
    stw fp+104, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5adf60
.L_0x627a4b5adfc0:
    ldw r2, fp+548  # reload
    ldw r11, fp+192  # reload
    sub r2, r2, r11
    stw fp+552, r2  # spill
    ldw r28, fp+552  # PHI reload
    stw fp+96, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5ad550
.L_0x627a4b5ad550:
    addi r28, r0, 0
    # PHI const
    stw fp+108, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5ae9f0
.L_0x627a4b5aeac0:
    ldw r2, fp+96  # reload
    add r2, r2, r16
    stw fp+556, r2  # spill
    addi r3, r0, 9
    ldw r4, fp+168  # reload
    jal llvm.lifetime.end.p0
    ldw r27, fp+556  # PHI reload
    ldw r28, fp+480  # PHI reload
    stw fp+56, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a1dc0
.L_0x627a4b5ae9f0:
    ldw r2, fp+168  # reload
    add r12, r2, r0  # save ptr
    ldw r2, fp+108  # reload
    add r2, r12, r2
    stw fp+560, r2  # spill
    ldw r2, fp+560  # reload
    ldbu r2, r2+0
    stw fp+564, r2  # spill
    ldw r2, fp+564  # reload
    andi r2, r2, 255
    stw fp+568, r2  # spill
    ldw r3, fp+568  # reload
    jal putchar
    ldw r2, fp+108  # reload
    addi r11, r0, 1
    add r2, r2, r11
    stw fp+572, r2  # spill
    ldw r2, fp+572  # reload
    ldw r11, fp+96  # reload
    seq r2, r2, r11
    stw fp+576, r2  # spill
    ldw r2, fp+576  # reload
    beq r2, r0, .L_0x627a4b5ae9f0_to_0x627a4b5ae9f0
    beq r0, r0, .L_0x627a4b5aeac0
.L_0x627a4b5ae9f0_to_0x627a4b5ae9f0:  # Critical edge split
    ldw r28, fp+572  # PHI reload
    stw fp+108, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5ae9f0
.L_0x627a4b5a4c80:
    add r2, r14, r0
    stw fp+580, r2  # spill
    ldw r2, r14+0
    stw fp+584, r2  # spill
    addi r3, r0, 48
    jal putchar
    addi r3, r0, 120
    jal putchar
    addi r11, r0, 2
    add r2, r16, r11
    stw fp+588, r2  # spill
    addi r3, r0, 9
    ldw r4, fp+172  # reload
    jal llvm.lifetime.start.p0
    ldw r2, fp+584  # reload
    ldw r11, fp+772  # reload
    seq r2, r2, r11
    stw fp+592, r2  # spill
    ldw r2, fp+592  # reload
    beq r2, r0, .L_0x627a4b5afaa0
    beq r0, r0, .L_0x627a4b5afa00
.L_0x627a4b5afaa0:
    ldw r28, fp+596  # PHI reload
    stw fp+112, r28  # PHI spill
    ldw r28, fp+172  # PHI reload
    stw fp+116, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5afc60
.L_0x627a4b5afa00:
    addi r2, r0, 48
    ldw r2, fp+172  # reload
    stb r2+0, r2
    ldw r2, fp+188  # reload
    stb r2+0, r0
    addi r28, r0, 1
    # PHI const
    stw fp+120, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5afe70
.L_0x627a4b5afc60:
    ldw r2, fp+112  # reload
    addi r11, r0, 15
    and r2, r2, r11
    stw fp+600, r2  # spill
    lui r2, %hi(.L_runtime_printf_str_2)
    addi r2, r2, %lo(.L_runtime_printf_str_2)
    stw fp+776, r2  # spill
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+604, r2  # spill
    ldw r2, fp+604  # reload
    ldbu r2, r2+0
    stw fp+608, r2  # spill
    ldw r2, fp+116  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+612, r2  # spill
    ldw r2, fp+608  # reload
    ldw r2, fp+116  # reload
    stb r2+0, r2
    ldw r2, fp+112  # reload
    addi r11, r0, 4
    srl r2, r2, r11
    stw fp+616, r2  # spill
    ldw r2, fp+112  # reload
    addi r11, r0, 16
    sltu r2, r2, r11
    stw fp+620, r2  # spill
    ldw r2, fp+620  # reload
    beq r2, r0, .L_0x627a4b5afc60_to_0x627a4b5afc60
    beq r0, r0, .L_0x627a4b5a33c0
.L_0x627a4b5afc60_to_0x627a4b5afc60:  # Critical edge split
    ldw r28, fp+616  # PHI reload
    stw fp+112, r28  # PHI spill
    ldw r28, fp+612  # PHI reload
    stw fp+116, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5afc60
.L_0x627a4b5a33c0:
    ldw r2, fp+612  # reload
    stb r2+0, r0
    ldw r2, fp+116  # reload
    ldw r11, fp+172  # reload
    sgtu r2, r2, r11
    stw fp+624, r2  # spill
    ldw r2, fp+624  # reload
    beq r2, r0, .L_0x627a4b5b17c0
    beq r0, r0, .L_0x627a4b5a33c0_to_0x627a4b5b1760
.L_0x627a4b5a33c0_to_0x627a4b5b1760:  # Critical edge split
    ldw r28, fp+172  # PHI reload
    stw fp+124, r28  # PHI spill
    ldw r28, fp+116  # PHI reload
    stw fp+128, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b1760
.L_0x627a4b5b1760:
    ldw r2, fp+128  # reload
    ldbu r2, r2+0
    stw fp+628, r2  # spill
    ldw r2, fp+124  # reload
    ldbu r2, r2+0
    stw fp+632, r2  # spill
    ldw r2, fp+632  # reload
    ldw r2, fp+128  # reload
    stb r2+0, r2
    ldw r2, fp+628  # reload
    ldw r2, fp+124  # reload
    stb r2+0, r2
    ldw r2, fp+124  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+636, r2  # spill
    ldw r2, fp+128  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+640, r2  # spill
    ldw r2, fp+636  # reload
    ldw r11, fp+640  # reload
    sltu r2, r2, r11
    stw fp+644, r2  # spill
    ldw r2, fp+644  # reload
    beq r2, r0, .L_0x627a4b5b17c0
    beq r0, r0, .L_0x627a4b5b1760_to_0x627a4b5b1760
.L_0x627a4b5b1760_to_0x627a4b5b1760:  # Critical edge split
    ldw r28, fp+636  # PHI reload
    stw fp+124, r28  # PHI spill
    ldw r28, fp+640  # PHI reload
    stw fp+128, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b1760
.L_0x627a4b5b17c0:
    ldw r2, fp+648  # reload
    ldw r11, fp+184  # reload
    sub r2, r2, r11
    stw fp+652, r2  # spill
    ldw r28, fp+652  # PHI reload
    stw fp+120, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5afe70
.L_0x627a4b5afe70:
    ldw r2, fp+120  # reload
    addi r11, r0, 8
    sltu r2, r2, r11
    stw fp+656, r2  # spill
    ldw r2, fp+656  # reload
    beq r2, r0, .L_0x627a4b5afe70_to_0x627a4b5aa5c0
    beq r0, r0, .L_0x627a4b5afe70_to_0x627a4b5aa560
.L_0x627a4b5afe70_to_0x627a4b5aa560:  # Critical edge split
    ldw r28, fp+120  # PHI reload
    stw fp+132, r28  # PHI spill
    ldw r28, fp+588  # PHI reload
    stw fp+136, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5aa560
.L_0x627a4b5afe70_to_0x627a4b5aa5c0:  # Critical edge split
    ldw r28, fp+588  # PHI reload
    stw fp+140, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5aa5c0
.L_0x627a4b5aa5c0:
    addi r28, r0, 0
    # PHI const
    stw fp+144, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5aa790
.L_0x627a4b5aa560:
    addi r3, r0, 48
    jal putchar
    ldw r2, fp+136  # reload
    addi r11, r0, 1
    add r2, r2, r11
    stw fp+660, r2  # spill
    ldw r2, fp+132  # reload
    addi r11, r0, 1
    add r2, r2, r11
    stw fp+664, r2  # spill
    ldw r2, fp+664  # reload
    addi r11, r0, 8
    seq r2, r2, r11
    stw fp+668, r2  # spill
    ldw r2, fp+668  # reload
    beq r2, r0, .L_0x627a4b5aa560_to_0x627a4b5aa560
    beq r0, r0, .L_0x627a4b5aa560_to_0x627a4b5aa5c0
.L_0x627a4b5aa560_to_0x627a4b5aa5c0:  # Critical edge split
    ldw r28, fp+660  # PHI reload
    stw fp+140, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5aa5c0
.L_0x627a4b5aa560_to_0x627a4b5aa560:  # Critical edge split
    ldw r28, fp+664  # PHI reload
    stw fp+132, r28  # PHI spill
    ldw r28, fp+660  # PHI reload
    stw fp+136, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5aa560
.L_0x627a4b5aad80:
    ldw r2, fp+140  # reload
    ldw r11, fp+120  # reload
    add r2, r2, r11
    stw fp+672, r2  # spill
    addi r3, r0, 9
    ldw r4, fp+172  # reload
    jal llvm.lifetime.end.p0
    ldw r27, fp+672  # PHI reload
    ldw r28, fp+580  # PHI reload
    stw fp+56, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a1dc0
.L_0x627a4b5aa790:
    ldw r2, fp+172  # reload
    add r12, r2, r0  # save ptr
    ldw r2, fp+144  # reload
    add r2, r12, r2
    stw fp+676, r2  # spill
    ldw r2, fp+676  # reload
    ldbu r2, r2+0
    stw fp+680, r2  # spill
    ldw r2, fp+680  # reload
    andi r2, r2, 255
    stw fp+684, r2  # spill
    ldw r3, fp+684  # reload
    jal putchar
    ldw r2, fp+144  # reload
    addi r11, r0, 1
    add r2, r2, r11
    stw fp+688, r2  # spill
    ldw r2, fp+688  # reload
    ldw r11, fp+120  # reload
    seq r2, r2, r11
    stw fp+692, r2  # spill
    ldw r2, fp+692  # reload
    beq r2, r0, .L_0x627a4b5aa790_to_0x627a4b5aa790
    beq r0, r0, .L_0x627a4b5aad80
.L_0x627a4b5aa790_to_0x627a4b5aa790:  # Critical edge split
    ldw r28, fp+688  # PHI reload
    stw fp+144, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5aa790
.L_0x627a4b5a4d50:
    add r2, r14, r0
    stw fp+696, r2  # spill
    ldw r2, r14+0
    stw fp+700, r2  # spill
    ldw r3, fp+700  # reload
    jal putchar
    addi r11, r0, 1
    add r2, r16, r11
    stw fp+704, r2  # spill
    ldw r27, fp+704  # PHI reload
    ldw r28, fp+696  # PHI reload
    stw fp+56, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a1dc0
.L_0x627a4b5a4e20:
    add r2, r14, r0
    stw fp+708, r2  # spill
    ldw r2, r14+0
    stw fp+712, r2  # spill
    ldw r2, fp+712  # reload
    ldw r11, fp+772  # reload
    seq r2, r2, r11
    stw fp+716, r2  # spill
    ldw r2, fp+720  # reload
    ldbu r2, r2+0
    stw fp+724, r2  # spill
    ldw r2, fp+724  # reload
    seq r2, r2, r0
    stw fp+728, r2  # spill
    ldw r2, fp+728  # reload
    beq r2, r0, .L_0x627a4b5a4e20_to_0x627a4b5b5050
    beq r0, r0, .L_0x627a4b5a4e20_to_0x627a4b5a1dc0
.L_0x627a4b5a4e20_to_0x627a4b5a1dc0:  # Critical edge split
    add r27, r16, r0  # PHI move
    ldw r28, fp+708  # PHI reload
    stw fp+56, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a1dc0
.L_0x627a4b5a4e20_to_0x627a4b5b5050:  # Critical edge split
    ldw r28, fp+724  # PHI reload
    stw fp+148, r28  # PHI spill
    ldw r28, fp+720  # PHI reload
    stw fp+152, r28  # PHI spill
    stw fp+156, r16  # PHI spill
    beq r0, r0, .L_0x627a4b5b5050
.L_0x627a4b5b5050:
    ldw r2, fp+148  # reload
    andi r2, r2, 255
    stw fp+732, r2  # spill
    ldw r3, fp+732  # reload
    jal putchar
    ldw r2, fp+152  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+736, r2  # spill
    ldw r2, fp+156  # reload
    addi r11, r0, 1
    add r2, r2, r11
    stw fp+740, r2  # spill
    ldw r2, fp+736  # reload
    ldbu r2, r2+0
    stw fp+744, r2  # spill
    ldw r2, fp+744  # reload
    seq r2, r2, r0
    stw fp+748, r2  # spill
    ldw r2, fp+748  # reload
    beq r2, r0, .L_0x627a4b5b5050_to_0x627a4b5b5050
    beq r0, r0, .L_0x627a4b5b5050_to_0x627a4b5a1dc0
.L_0x627a4b5b5050_to_0x627a4b5a1dc0:  # Critical edge split
    ldw r27, fp+740  # PHI reload
    ldw r28, fp+708  # PHI reload
    stw fp+56, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5a1dc0
.L_0x627a4b5b5050_to_0x627a4b5b5050:  # Critical edge split
    ldw r28, fp+744  # PHI reload
    stw fp+148, r28  # PHI spill
    ldw r28, fp+736  # PHI reload
    stw fp+152, r28  # PHI spill
    ldw r28, fp+740  # PHI reload
    stw fp+156, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b5050
.L_0x627a4b5a4ec0:
    addi r3, r0, 37
    jal putchar
    addi r11, r0, 1
    add r2, r16, r11
    stw fp+752, r2  # spill
    ldw r27, fp+752  # PHI reload
    stw fp+56, r14  # PHI spill
    beq r0, r0, .L_0x627a4b5a1dc0
.L_0x627a4b5a4980:
    addi r3, r0, 37
    jal putchar
    ldw r2, fp+232  # reload
    ldbu r2, r2+0
    stw fp+756, r2  # spill
    ldw r2, fp+756  # reload
    andi r2, r2, 255
    stw fp+760, r2  # spill
    ldw r3, fp+760  # reload
    jal putchar
    addi r11, r0, 2
    add r2, r16, r11
    stw fp+764, r2  # spill
    ldw r27, fp+764  # PHI reload
    stw fp+56, r14  # PHI spill
    beq r0, r0, .L_0x627a4b5a1dc0
.L_0x627a4b5a1dc0:
    add r2, r15, r0
    stw fp+768, r2  # spill
    ldw r12, fp+768  # PHI reload
    add r13, r27, r0  # PHI move
    ldw r14, fp+56  # PHI reload
    beq r0, r0, .L_0x627a4b5a1cc0
.L_0x627a4b5a3590:
    add r1, r16, r0
    add sp, fp, r0
    ldw lr, sp+0
    ldw fp, sp+4
    addi sp, sp, 784
    jalr r0, lr, 0

# Function: printf
.global printf
printf:
    addi sp, sp, -16
    stw sp+0, lr
    stw sp+4, fp
    add fp, sp, r0
.L_0x627a4b5a3f30:
    addi r12, fp, 8
    addi r3, r0, 4
    add r4, r12, r0
    jal llvm.lifetime.start.p0
    addi r11, fp, 16  # ap = entry SP
    stw r12+0, r11
    ldw r13, r12+0
    add r4, r14, r0
    jal vprintf
    add r15, r1, r0
    addi r3, r0, 4
    add r4, r12, r0
    jal llvm.lifetime.end.p0
    add r1, r15, r0
    add sp, fp, r0
    ldw lr, sp+0
    ldw fp, sp+4
    addi sp, sp, 16
    jalr r0, lr, 0

# Function: vsprintf
.global vsprintf
vsprintf:
    addi sp, sp, -640
    stw sp+0, lr
    stw sp+4, fp
    add fp, sp, r0
.L_0x627a4b5b2d80:
    addi r2, fp, 8
    stw fp+116, r2  # spill
    addi r2, fp, 20
    stw fp+120, r2  # spill
    addi r2, fp, 32
    stw fp+124, r2  # spill
    addi r2, fp, 44
    stw fp+128, r2  # spill
    ldw r2, fp+128  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+144, r2  # spill
    ldw r2, fp+124  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+152, r2  # spill
    ldw r2, fp+120  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+160, r2  # spill
    ldw r2, fp+116  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+164, r2  # spill
    add r12, r4, r0  # PHI move
    add r13, r3, r0  # PHI move
    ldw r14, fp+136  # PHI reload
    beq r0, r0, .L_0x627a4b5b69f0
.L_0x627a4b5b69f0:
    add r2, r13, r0
    stw fp+172, r2  # spill
    ldw r15, fp+172  # PHI reload
    add r16, r12, r0  # PHI move
    add r17, r13, r0  # PHI move
    beq r0, r0, .L_0x627a4b5b6e30
.L_0x627a4b5b6e30:
    ldbu r2, r16+0
    stw fp+176, r2  # spill
.L_0x627a4b5b6f30:
    add r2, r16, r0
    stw fp+180, r2  # spill
    add r2, r17, r0
    stw fp+184, r2  # spill
    ldw r2, fp+176  # reload
    stb r17+0, r2
    add r2, r15, r0
    stw fp+188, r2  # spill
    ldw r15, fp+188  # PHI reload
    ldw r16, fp+180  # PHI reload
    ldw r17, fp+184  # PHI reload
    beq r0, r0, .L_0x627a4b5b6e30
.L_0x627a4b5b7410:
    add r2, r16, r0
    stw fp+192, r2  # spill
    ldw r2, fp+192  # reload
    ldbu r2, r2+0
    stw fp+196, r2  # spill
.L_0x627a4b5a2bb0:
    add r2, r14, r0
    stw fp+200, r2  # spill
    ldw r2, r14+0
    stw fp+204, r2  # spill
    addi r3, r0, 12
    ldw r4, fp+116  # reload
    jal llvm.lifetime.start.p0
    ldw r2, fp+204  # reload
    slt r2, r2, r0
    stw fp+208, r2  # spill
    ldw r2, fp+208  # reload
    beq r2, r0, .L_0x627a4b5a2bb0_to_0x627a4b5b8b10
    beq r0, r0, .L_0x627a4b5b8ab0
.L_0x627a4b5a2bb0_to_0x627a4b5b8b10:  # Critical edge split
    ldw r18, fp+204  # PHI reload
    ldw r19, fp+116  # PHI reload
    beq r0, r0, .L_0x627a4b5b8b10
.L_0x627a4b5b8ab0:
    addi r2, r0, 45
    ldw r2, fp+116  # reload
    stb r2+0, r2
    ldw r11, fp+204  # reload
    sub r2, r0, r11
    stw fp+212, r2  # spill
    ldw r18, fp+212  # PHI reload
    ldw r19, fp+164  # PHI reload
    beq r0, r0, .L_0x627a4b5b8b10
.L_0x627a4b5b8b10:
    seq r2, r18, r0
    stw fp+216, r2  # spill
    ldw r2, fp+216  # reload
    beq r2, r0, .L_0x627a4b5b8b10_to_0x627a4b5b9070
    beq r0, r0, .L_0x627a4b5b9010
.L_0x627a4b5b8b10_to_0x627a4b5b9070:  # Critical edge split
    add r20, r19, r0  # PHI move
    add r21, r18, r0  # PHI move
    beq r0, r0, .L_0x627a4b5b9070
.L_0x627a4b5b9010:
    add r2, r19, r0
    stw fp+220, r2  # spill
    addi r2, r0, 48
    stb r19+0, r2
    ldw r2, fp+220  # reload
    stb r2+0, r0
    addi r22, r0, 1
    # PHI const
    beq r0, r0, .L_0x627a4b5b9360
.L_0x627a4b5b9070:
    ldw r2, fp+224  # reload
    addi r11, r0, 10
    ldw r2, fp+228  # reload
    addi r11, r0, 10
    mul r2, r2, r11
    stw fp+232, r2  # spill
    ldw r2, fp+224  # reload
    ldw r11, fp+232  # reload
    sub r2, r2, r11
    stw fp+236, r2  # spill
    ldw r2, fp+236  # reload
    andi r2, r2, 255
    stw fp+240, r2  # spill
    ldw r2, fp+240  # reload
    addi r11, r0, 48
    or r2, r2, r11
    stw fp+244, r2  # spill
    add r2, r20, r0
    stw fp+248, r2  # spill
    ldw r2, fp+244  # reload
    stb r20+0, r2
    addi r11, r0, 10
    sltu r2, r21, r11
    stw fp+252, r2  # spill
    ldw r2, fp+252  # reload
    beq r2, r0, .L_0x627a4b5b9070_to_0x627a4b5b9070
    beq r0, r0, .L_0x627a4b5ba2f0
.L_0x627a4b5b9070_to_0x627a4b5b9070:  # Critical edge split
    ldw r20, fp+248  # PHI reload
    ldw r21, fp+228  # PHI reload
    beq r0, r0, .L_0x627a4b5b9070
.L_0x627a4b5ba2f0:
    ldw r2, fp+248  # reload
    stb r2+0, r0
    sgtu r2, r20, r19
    stw fp+256, r2  # spill
    ldw r2, fp+256  # reload
    beq r2, r0, .L_0x627a4b5ba590
    beq r0, r0, .L_0x627a4b5ba2f0_to_0x627a4b5ba530
.L_0x627a4b5ba2f0_to_0x627a4b5ba530:  # Critical edge split
    add r23, r19, r0  # PHI move
    add r24, r20, r0  # PHI move
    beq r0, r0, .L_0x627a4b5ba530
.L_0x627a4b5ba530:
    ldbu r2, r24+0
    stw fp+260, r2  # spill
    ldbu r2, r23+0
    stw fp+264, r2  # spill
    ldw r2, fp+264  # reload
    stb r24+0, r2
    ldw r2, fp+260  # reload
    stb r23+0, r2
    add r2, r23, r0
    stw fp+268, r2  # spill
    add r2, r24, r0
    stw fp+272, r2  # spill
    ldw r2, fp+268  # reload
    ldw r11, fp+272  # reload
    sltu r2, r2, r11
    stw fp+276, r2  # spill
    ldw r2, fp+276  # reload
    beq r2, r0, .L_0x627a4b5ba590
    beq r0, r0, .L_0x627a4b5ba530_to_0x627a4b5ba530
.L_0x627a4b5ba530_to_0x627a4b5ba530:  # Critical edge split
    ldw r23, fp+268  # PHI reload
    ldw r24, fp+272  # PHI reload
    beq r0, r0, .L_0x627a4b5ba530
.L_0x627a4b5ba590:
    ldw r2, fp+280  # reload
    ldw r11, fp+284  # reload
    sub r2, r2, r11
    stw fp+288, r2  # spill
    ldw r22, fp+288  # PHI reload
    beq r0, r0, .L_0x627a4b5b9360
.L_0x627a4b5b9360:
    add r2, r19, r0
    stw fp+292, r2  # spill
    ldw r2, fp+296  # reload
    ldw r11, fp+168  # reload
    sub r2, r2, r11
    stw fp+300, r2  # spill
    add r3, r17, r0
    ldw r4, fp+116  # reload
    ldw r5, fp+300  # reload
    add r6, r0, r0
    jal llvm.memcpy.p0.p0.i32
    add r2, r17, r0
    stw fp+304, r2  # spill
    addi r3, r0, 12
    ldw r4, fp+116  # reload
    jal llvm.lifetime.end.p0
    ldw r25, fp+304  # PHI reload
    ldw r26, fp+200  # PHI reload
    beq r0, r0, .L_0x627a4b5b6ac0
.L_0x627a4b5a2c10:
    add r2, r14, r0
    stw fp+308, r2  # spill
    ldw r2, r14+0
    stw fp+312, r2  # spill
    addi r3, r0, 11
    ldw r4, fp+120  # reload
    jal llvm.lifetime.start.p0
    ldw r2, fp+312  # reload
    seq r2, r2, r0
    stw fp+316, r2  # spill
    ldw r2, fp+316  # reload
    beq r2, r0, .L_0x627a4b5a2c10_to_0x627a4b5bb980
    beq r0, r0, .L_0x627a4b5bb920
.L_0x627a4b5a2c10_to_0x627a4b5bb980:  # Critical edge split
    ldw r27, fp+120  # PHI reload
    ldw r28, fp+312  # PHI reload
    stw fp+56, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5bb980
.L_0x627a4b5bb920:
    addi r2, r0, 48
    ldw r2, fp+120  # reload
    stb r2+0, r2
    ldw r2, fp+160  # reload
    stb r2+0, r0
    addi r28, r0, 1
    # PHI const
    stw fp+60, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5bbbd0
.L_0x627a4b5bb980:
    ldw r2, fp+320  # reload
    addi r11, r0, 10
    ldw r2, fp+324  # reload
    addi r11, r0, 10
    mul r2, r2, r11
    stw fp+328, r2  # spill
    ldw r2, fp+320  # reload
    ldw r11, fp+328  # reload
    sub r2, r2, r11
    stw fp+332, r2  # spill
    ldw r2, fp+332  # reload
    andi r2, r2, 255
    stw fp+336, r2  # spill
    ldw r2, fp+336  # reload
    addi r11, r0, 48
    or r2, r2, r11
    stw fp+340, r2  # spill
    add r2, r27, r0
    stw fp+344, r2  # spill
    ldw r2, fp+340  # reload
    stb r27+0, r2
    ldw r2, fp+56  # reload
    addi r11, r0, 10
    sltu r2, r2, r11
    stw fp+348, r2  # spill
    ldw r2, fp+348  # reload
    beq r2, r0, .L_0x627a4b5bb980_to_0x627a4b5bb980
    beq r0, r0, .L_0x627a4b5bc310
.L_0x627a4b5bb980_to_0x627a4b5bb980:  # Critical edge split
    ldw r27, fp+344  # PHI reload
    ldw r28, fp+324  # PHI reload
    stw fp+56, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5bb980
.L_0x627a4b5bc310:
    ldw r2, fp+344  # reload
    stb r2+0, r0
    ldw r11, fp+120  # reload
    sgtu r2, r27, r11
    stw fp+352, r2  # spill
    ldw r2, fp+352  # reload
    beq r2, r0, .L_0x627a4b5b9490
    beq r0, r0, .L_0x627a4b5bc310_to_0x627a4b5b9430
.L_0x627a4b5bc310_to_0x627a4b5b9430:  # Critical edge split
    ldw r28, fp+120  # PHI reload
    stw fp+64, r28  # PHI spill
    stw fp+68, r27  # PHI spill
    beq r0, r0, .L_0x627a4b5b9430
.L_0x627a4b5b9430:
    ldw r2, fp+68  # reload
    ldbu r2, r2+0
    stw fp+356, r2  # spill
    ldw r2, fp+64  # reload
    ldbu r2, r2+0
    stw fp+360, r2  # spill
    ldw r2, fp+360  # reload
    ldw r2, fp+68  # reload
    stb r2+0, r2
    ldw r2, fp+356  # reload
    ldw r2, fp+64  # reload
    stb r2+0, r2
    ldw r2, fp+64  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+364, r2  # spill
    ldw r2, fp+68  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+368, r2  # spill
    ldw r2, fp+364  # reload
    ldw r11, fp+368  # reload
    sltu r2, r2, r11
    stw fp+372, r2  # spill
    ldw r2, fp+372  # reload
    beq r2, r0, .L_0x627a4b5b9490
    beq r0, r0, .L_0x627a4b5b9430_to_0x627a4b5b9430
.L_0x627a4b5b9430_to_0x627a4b5b9430:  # Critical edge split
    ldw r28, fp+364  # PHI reload
    stw fp+64, r28  # PHI spill
    ldw r28, fp+368  # PHI reload
    stw fp+68, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b9430
.L_0x627a4b5b9490:
    ldw r2, fp+376  # reload
    ldw r11, fp+156  # reload
    sub r2, r2, r11
    stw fp+380, r2  # spill
    ldw r28, fp+380  # PHI reload
    stw fp+60, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5bbbd0
.L_0x627a4b5bbbd0:
    add r3, r17, r0
    ldw r4, fp+120  # reload
    ldw r5, fp+60  # reload
    add r6, r0, r0
    jal llvm.memcpy.p0.p0.i32
    add r2, r17, r0
    stw fp+384, r2  # spill
    addi r3, r0, 11
    ldw r4, fp+120  # reload
    jal llvm.lifetime.end.p0
    ldw r25, fp+384  # PHI reload
    ldw r26, fp+308  # PHI reload
    beq r0, r0, .L_0x627a4b5b6ac0
.L_0x627a4b5a2cb0:
    add r2, r14, r0
    stw fp+388, r2  # spill
    ldw r2, r14+0
    stw fp+392, r2  # spill
    addi r3, r0, 9
    ldw r4, fp+124  # reload
    jal llvm.lifetime.start.p0
    ldw r2, fp+196  # reload
    addi r11, r0, 88
    seq r2, r2, r11
    stw fp+396, r2  # spill
    ldw r2, fp+392  # reload
    seq r2, r2, r0
    stw fp+404, r2  # spill
    ldw r2, fp+404  # reload
    beq r2, r0, .L_0x627a4b5a2cb0_to_0x627a4b5bdf40
    beq r0, r0, .L_0x627a4b5bdee0
.L_0x627a4b5a2cb0_to_0x627a4b5bdf40:  # Critical edge split
    ldw r28, fp+392  # PHI reload
    stw fp+72, r28  # PHI spill
    ldw r28, fp+124  # PHI reload
    stw fp+76, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5bdf40
.L_0x627a4b5bdee0:
    addi r2, r0, 48
    ldw r2, fp+124  # reload
    stb r2+0, r2
    ldw r2, fp+152  # reload
    stb r2+0, r0
    addi r28, r0, 1
    # PHI const
    stw fp+80, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b02b0
.L_0x627a4b5bdf40:
    ldw r2, fp+72  # reload
    addi r11, r0, 15
    and r2, r2, r11
    stw fp+408, r2  # spill
    ldw r2, fp+400  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+412, r2  # spill
    ldw r2, fp+412  # reload
    ldbu r2, r2+0
    stw fp+416, r2  # spill
    ldw r2, fp+76  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+420, r2  # spill
    ldw r2, fp+416  # reload
    ldw r2, fp+76  # reload
    stb r2+0, r2
    ldw r2, fp+72  # reload
    addi r11, r0, 4
    srl r2, r2, r11
    stw fp+424, r2  # spill
    ldw r2, fp+72  # reload
    addi r11, r0, 16
    sltu r2, r2, r11
    stw fp+428, r2  # spill
    ldw r2, fp+428  # reload
    beq r2, r0, .L_0x627a4b5bdf40_to_0x627a4b5bdf40
    beq r0, r0, .L_0x627a4b5b0910
.L_0x627a4b5bdf40_to_0x627a4b5bdf40:  # Critical edge split
    ldw r28, fp+424  # PHI reload
    stw fp+72, r28  # PHI spill
    ldw r28, fp+420  # PHI reload
    stw fp+76, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5bdf40
.L_0x627a4b5b0910:
    ldw r2, fp+420  # reload
    stb r2+0, r0
    ldw r2, fp+76  # reload
    ldw r11, fp+124  # reload
    sgtu r2, r2, r11
    stw fp+432, r2  # spill
    ldw r2, fp+432  # reload
    beq r2, r0, .L_0x627a4b5b0bb0
    beq r0, r0, .L_0x627a4b5b0910_to_0x627a4b5b0b50
.L_0x627a4b5b0910_to_0x627a4b5b0b50:  # Critical edge split
    ldw r28, fp+124  # PHI reload
    stw fp+84, r28  # PHI spill
    ldw r28, fp+76  # PHI reload
    stw fp+88, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b0b50
.L_0x627a4b5b0b50:
    ldw r2, fp+88  # reload
    ldbu r2, r2+0
    stw fp+436, r2  # spill
    ldw r2, fp+84  # reload
    ldbu r2, r2+0
    stw fp+440, r2  # spill
    ldw r2, fp+440  # reload
    ldw r2, fp+88  # reload
    stb r2+0, r2
    ldw r2, fp+436  # reload
    ldw r2, fp+84  # reload
    stb r2+0, r2
    ldw r2, fp+84  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+444, r2  # spill
    ldw r2, fp+88  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+448, r2  # spill
    ldw r2, fp+444  # reload
    ldw r11, fp+448  # reload
    sltu r2, r2, r11
    stw fp+452, r2  # spill
    ldw r2, fp+452  # reload
    beq r2, r0, .L_0x627a4b5b0bb0
    beq r0, r0, .L_0x627a4b5b0b50_to_0x627a4b5b0b50
.L_0x627a4b5b0b50_to_0x627a4b5b0b50:  # Critical edge split
    ldw r28, fp+444  # PHI reload
    stw fp+84, r28  # PHI spill
    ldw r28, fp+448  # PHI reload
    stw fp+88, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b0b50
.L_0x627a4b5b0bb0:
    ldw r2, fp+456  # reload
    ldw r11, fp+148  # reload
    sub r2, r2, r11
    stw fp+460, r2  # spill
    ldw r28, fp+460  # PHI reload
    stw fp+80, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b02b0
.L_0x627a4b5b02b0:
    add r3, r17, r0
    ldw r4, fp+124  # reload
    ldw r5, fp+80  # reload
    add r6, r0, r0
    jal llvm.memcpy.p0.p0.i32
    add r2, r17, r0
    stw fp+464, r2  # spill
    addi r3, r0, 9
    ldw r4, fp+124  # reload
    jal llvm.lifetime.end.p0
    ldw r25, fp+464  # PHI reload
    ldw r26, fp+388  # PHI reload
    beq r0, r0, .L_0x627a4b5b6ac0
.L_0x627a4b5a2d50:
    add r2, r14, r0
    stw fp+468, r2  # spill
    ldw r2, r14+0
    stw fp+472, r2  # spill
    add r2, r17, r0
    stw fp+476, r2  # spill
    addi r2, r0, 48
    stb r17+0, r2
    add r2, r17, r0
    stw fp+480, r2  # spill
    addi r2, r0, 120
    ldw r2, fp+476  # reload
    stb r2+0, r2
    addi r3, r0, 9
    ldw r4, fp+128  # reload
    jal llvm.lifetime.start.p0
    ldw r2, fp+472  # reload
    ldw r11, fp+640  # reload
    seq r2, r2, r11
    stw fp+484, r2  # spill
    ldw r2, fp+484  # reload
    beq r2, r0, .L_0x627a4b5b8270
    beq r0, r0, .L_0x627a4b5b8210
.L_0x627a4b5b8270:
    ldw r28, fp+488  # PHI reload
    stw fp+92, r28  # PHI spill
    ldw r28, fp+128  # PHI reload
    stw fp+96, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b83f0
.L_0x627a4b5b8210:
    addi r2, r0, 48
    ldw r2, fp+128  # reload
    stb r2+0, r2
    ldw r2, fp+144  # reload
    stb r2+0, r0
    addi r28, r0, 1
    # PHI const
    stw fp+100, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b8600
.L_0x627a4b5b83f0:
    ldw r2, fp+92  # reload
    addi r11, r0, 15
    and r2, r2, r11
    stw fp+492, r2  # spill
    lui r2, %hi(.L_runtime_printf_str_2)
    addi r2, r2, %lo(.L_runtime_printf_str_2)
    stw fp+644, r2  # spill
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+496, r2  # spill
    ldw r2, fp+496  # reload
    ldbu r2, r2+0
    stw fp+500, r2  # spill
    ldw r2, fp+96  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+504, r2  # spill
    ldw r2, fp+500  # reload
    ldw r2, fp+96  # reload
    stb r2+0, r2
    ldw r2, fp+92  # reload
    addi r11, r0, 4
    srl r2, r2, r11
    stw fp+508, r2  # spill
    ldw r2, fp+92  # reload
    addi r11, r0, 16
    sltu r2, r2, r11
    stw fp+512, r2  # spill
    ldw r2, fp+512  # reload
    beq r2, r0, .L_0x627a4b5b83f0_to_0x627a4b5b83f0
    beq r0, r0, .L_0x627a4b5c1bb0
.L_0x627a4b5b83f0_to_0x627a4b5b83f0:  # Critical edge split
    ldw r28, fp+508  # PHI reload
    stw fp+92, r28  # PHI spill
    ldw r28, fp+504  # PHI reload
    stw fp+96, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b83f0
.L_0x627a4b5c1bb0:
    ldw r2, fp+504  # reload
    stb r2+0, r0
    ldw r2, fp+96  # reload
    ldw r11, fp+128  # reload
    sgtu r2, r2, r11
    stw fp+516, r2  # spill
    ldw r2, fp+516  # reload
    beq r2, r0, .L_0x627a4b5c1e50
    beq r0, r0, .L_0x627a4b5c1bb0_to_0x627a4b5c1df0
.L_0x627a4b5c1bb0_to_0x627a4b5c1df0:  # Critical edge split
    ldw r28, fp+128  # PHI reload
    stw fp+104, r28  # PHI spill
    ldw r28, fp+96  # PHI reload
    stw fp+108, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5c1df0
.L_0x627a4b5c1df0:
    ldw r2, fp+108  # reload
    ldbu r2, r2+0
    stw fp+520, r2  # spill
    ldw r2, fp+104  # reload
    ldbu r2, r2+0
    stw fp+524, r2  # spill
    ldw r2, fp+524  # reload
    ldw r2, fp+108  # reload
    stb r2+0, r2
    ldw r2, fp+520  # reload
    ldw r2, fp+104  # reload
    stb r2+0, r2
    ldw r2, fp+104  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+528, r2  # spill
    ldw r2, fp+108  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+532, r2  # spill
    ldw r2, fp+528  # reload
    ldw r11, fp+532  # reload
    sltu r2, r2, r11
    stw fp+536, r2  # spill
    ldw r2, fp+536  # reload
    beq r2, r0, .L_0x627a4b5c1e50
    beq r0, r0, .L_0x627a4b5c1df0_to_0x627a4b5c1df0
.L_0x627a4b5c1df0_to_0x627a4b5c1df0:  # Critical edge split
    ldw r28, fp+528  # PHI reload
    stw fp+104, r28  # PHI spill
    ldw r28, fp+532  # PHI reload
    stw fp+108, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5c1df0
.L_0x627a4b5c1e50:
    ldw r2, fp+540  # reload
    ldw r11, fp+140  # reload
    sub r2, r2, r11
    stw fp+544, r2  # spill
    ldw r28, fp+544  # PHI reload
    stw fp+100, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5b8600
.L_0x627a4b5b8600:
    ldw r2, fp+100  # reload
    addi r11, r0, 8
    sltu r2, r2, r11
    stw fp+548, r2  # spill
    ldw r2, fp+548  # reload
    beq r2, r0, .L_0x627a4b5b8600_to_0x627a4b5c28c0
    beq r0, r0, .L_0x627a4b5c2860
.L_0x627a4b5b8600_to_0x627a4b5c28c0:  # Critical edge split
    ldw r28, fp+480  # PHI reload
    stw fp+112, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5c28c0
.L_0x627a4b5c2860:
    addi r2, r0, 8
    ldw r11, fp+100  # reload
    sub r2, r2, r11
    stw fp+552, r2  # spill
    ldw r3, fp+480  # reload
    addi r4, r0, 48
    ldw r5, fp+552  # reload
    add r6, r0, r0
    jal llvm.memset.p0.i32
    ldw r11, fp+100  # reload
    sub r2, r0, r11
    stw fp+556, r2  # spill
    add r2, r15, r0
    stw fp+560, r2  # spill
    ldw r28, fp+560  # PHI reload
    stw fp+112, r28  # PHI spill
    beq r0, r0, .L_0x627a4b5c28c0
.L_0x627a4b5c28c0:
    ldw r3, fp+112  # reload
    ldw r4, fp+128  # reload
    ldw r5, fp+100  # reload
    add r6, r0, r0
    jal llvm.memcpy.p0.p0.i32
    ldw r2, fp+112  # reload
    add r12, r2, r0  # save ptr
    add r2, r12, r0
    stw fp+564, r2  # spill
    addi r3, r0, 9
    ldw r4, fp+128  # reload
    jal llvm.lifetime.end.p0
    ldw r25, fp+564  # PHI reload
    ldw r26, fp+468  # PHI reload
    beq r0, r0, .L_0x627a4b5b6ac0
.L_0x627a4b5a2df0:
    add r2, r14, r0
    stw fp+568, r2  # spill
    ldw r2, r14+0
    stw fp+572, r2  # spill
    ldw r2, fp+572  # reload
    andi r2, r2, 255
    stw fp+576, r2  # spill
    add r2, r17, r0
    stw fp+580, r2  # spill
    ldw r2, fp+576  # reload
    stb r17+0, r2
    ldw r25, fp+580  # PHI reload
    ldw r26, fp+568  # PHI reload
    beq r0, r0, .L_0x627a4b5b6ac0
.L_0x627a4b5a2e90:
    add r2, r14, r0
    stw fp+584, r2  # spill
    ldw r2, r14+0
    stw fp+588, r2  # spill
    ldw r2, fp+588  # reload
    ldw r11, fp+640  # reload
    seq r2, r2, r11
    stw fp+592, r2  # spill
    ldw r3, fp+596  # reload
    jal strlen
    stw fp+600, r1  # spill
    add r3, r17, r0
    ldw r4, fp+596  # reload
    ldw r5, fp+600  # reload
    add r6, r0, r0
    jal llvm.memcpy.p0.p0.i32
    add r2, r17, r0
    stw fp+604, r2  # spill
    ldw r25, fp+604  # PHI reload
    ldw r26, fp+584  # PHI reload
    beq r0, r0, .L_0x627a4b5b6ac0
.L_0x627a4b5a2f30:
    add r2, r17, r0
    stw fp+608, r2  # spill
    addi r2, r0, 37
    stb r17+0, r2
    ldw r25, fp+608  # PHI reload
    add r26, r14, r0  # PHI move
    beq r0, r0, .L_0x627a4b5b6ac0
.L_0x627a4b5a2b50:
    add r2, r17, r0
    stw fp+612, r2  # spill
    addi r2, r0, 37
    stb r17+0, r2
    ldw r2, fp+192  # reload
    ldbu r2, r2+0
    stw fp+616, r2  # spill
    add r2, r17, r0
    stw fp+620, r2  # spill
    ldw r2, fp+616  # reload
    ldw r2, fp+612  # reload
    stb r2+0, r2
    ldw r25, fp+620  # PHI reload
    add r26, r14, r0  # PHI move
    beq r0, r0, .L_0x627a4b5b6ac0
.L_0x627a4b5b6ac0:
    add r2, r16, r0
    stw fp+624, r2  # spill
    ldw r12, fp+624  # PHI reload
    add r13, r25, r0  # PHI move
    add r14, r26, r0  # PHI move
    beq r0, r0, .L_0x627a4b5b69f0
.L_0x627a4b5b7370:
    stb r17+0, r0
    ldw r2, fp+628  # reload
    ldw r11, fp+632  # reload
    sub r2, r2, r11
    stw fp+636, r2  # spill
    ldw r2, fp+636  # reload
    add r1, r2, r0
    add sp, fp, r0
    ldw lr, sp+0
    ldw fp, sp+4
    addi sp, sp, 640
    jalr r0, lr, 0

# Function: sprintf
.global sprintf
sprintf:
    addi sp, sp, -16
    stw sp+0, lr
    stw sp+4, fp
    add fp, sp, r0
.L_0x627a4b5c41c0:
    addi r12, fp, 8
    addi r3, r0, 4
    add r4, r12, r0
    jal llvm.lifetime.start.p0
    addi r11, fp, 16  # ap = entry SP
    stw r12+0, r11
    ldw r13, r12+0
    add r5, r14, r0
    jal vsprintf
    add r15, r1, r0
    addi r3, r0, 4
    add r4, r12, r0
    jal llvm.lifetime.end.p0
    add r1, r15, r0
    add sp, fp, r0
    ldw lr, sp+0
    ldw fp, sp+4
    addi sp, sp, 16
    jalr r0, lr, 0


.rodata
.L_runtime_printf_str_0:
.string "(null)"
.L_runtime_printf_str_1:
.string "0123456789ABCDEF"
.L_runtime_printf_str_2:
.string "0123456789abcdef"
