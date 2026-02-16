# SLOW-32 Minimal MMIO Runtime for Selfhost Bootstrap
#
# This file provides the minimal runtime functions needed to link kernel.s32x
# without depending on host-built .s32o files.
#
# Functions provided:
#   - yield, putchar (system primitives)
#   - s32_mmio_request (core MMIO protocol)
#   - getchar, open, close, read, write, lseek, fstat, rename, unlink, usleep
#   - strlen, memset, memcpy, memmove
#   - __slow32_start (minimal C runtime entry)
#
# Assembled from compiler output of runtime/*.c using the SLOW-32 LLVM backend.
# No hand-written assembly - extracted and adapted for Forth assembler compatibility.

.text

# ============================================================================
# System Primitives
# ============================================================================

.global yield
yield:
    yield r0, r0, 0
    jalr r0, lr, 0

.global putchar
putchar:
    add r1, r3, r0
    debug r3
    jalr r0, lr, 0

# ============================================================================
# Core MMIO Request Protocol
# ============================================================================

.global s32_mmio_request
s32_mmio_request:
    addi sp, sp, -56
    stw sp, fp, 4
    stw sp, lr, 0
    add fp, sp, r0
    addi fp, fp, 56
    stw fp, r11, -4
    stw fp, r12, -8
    stw fp, r13, -12
    stw fp, r14, -16
    stw fp, r15, -20
    stw fp, r16, -24
    stw fp, r17, -28
    stw fp, r18, -32
    stw fp, lr, -36
    add r11, r6, r0
    add r12, r5, r0
    add r13, r4, r0
    add r14, r3, r0
    lui r15, %hi(__mmio_base)
    addi r15, r15, %lo(__mmio_base)
    ldw r17, r15, 0
    addi r1, r17, 1
    andi r16, r1, 255
    lui r18, %hi(__mmio_base+4)
    addi r18, r18, %lo(__mmio_base+4)
    ldw r1, r18, 0
    bne r16, r1, .Lmmio_req_ready
.Lmmio_req_wait1:
    jal yield
    ldw r1, r18, 0
    beq r16, r1, .Lmmio_req_wait1
.Lmmio_req_ready:
    slli r1, r17, 4
    lui r3, %hi(__mmio_base+4096)
    addi r3, r3, %lo(__mmio_base+4096)
    add r3, r1, r3
    stw r3, r14, 0
    lui r3, %hi(__mmio_base+4100)
    addi r3, r3, %lo(__mmio_base+4100)
    add r3, r1, r3
    stw r3, r13, 0
    lui r3, %hi(__mmio_base+4104)
    addi r3, r3, %lo(__mmio_base+4104)
    add r3, r1, r3
    stw r3, r12, 0
    lui r3, %hi(__mmio_base+4108)
    addi r3, r3, %lo(__mmio_base+4108)
    add r1, r1, r3
    stw r1, r11, 0
    stw r15, r16, 0
    lui r12, %hi(__mmio_base+8192)
    addi r12, r12, %lo(__mmio_base+8192)
    ldw r1, r12, 0
    lui r11, %hi(__mmio_base+8196)
    addi r11, r11, %lo(__mmio_base+8196)
    ldw r3, r11, 0
    bne r1, r3, .Lmmio_resp_ready
.Lmmio_req_wait2:
    jal yield
    ldw r1, r12, 0
    ldw r3, r11, 0
    beq r1, r3, .Lmmio_req_wait2
.Lmmio_resp_ready:
    slli r1, r3, 4
    lui r4, %hi(__mmio_base+12300)
    addi r4, r4, %lo(__mmio_base+12300)
    add r1, r1, r4
    ldw r1, r1, 0
    addi r3, r3, 1
    andi r3, r3, 255
    stw r11, r3, 0
    addi r3, r0, -2
    beq r1, r3, .Lmmio_err_io
    addi r3, r0, -1
    bne r1, r3, .Lmmio_done
    addi r3, r0, 5
    jal r0, .Lmmio_set_errno
.Lmmio_err_io:
    addi r3, r0, 4
.Lmmio_set_errno:
    lui r4, %hi(errno)
    addi r4, r4, %lo(errno)
    stw r4, r3, 0
.Lmmio_done:
    ldw lr, fp, -36
    ldw r18, fp, -32
    ldw r17, fp, -28
    ldw r16, fp, -24
    ldw r15, fp, -20
    ldw r14, fp, -16
    ldw r13, fp, -12
    ldw r12, fp, -8
    ldw r11, fp, -4
    ldw lr, sp, 0
    ldw fp, sp, 4
    addi sp, sp, 56
    jalr r0, lr, 0

# ============================================================================
# Character I/O
# ============================================================================

.global getchar
getchar:
    addi sp, sp, -24
    stw sp, fp, 4
    stw sp, lr, 0
    add fp, sp, r0
    addi fp, fp, 24
    stw fp, lr, -4
    addi r3, r0, 2
    addi r4, r0, 0
    add r5, r4, r0
    add r6, r4, r0
    jal s32_mmio_request
    add r3, r1, r0
    addi r1, r0, -1
    beq r3, r1, .Lgetchar_done
    lui r1, %hi(__mmio_base+16384)
    addi r1, r1, %lo(__mmio_base+16384)
    ldbu r1, r1, 0
.Lgetchar_done:
    ldw lr, fp, -4
    ldw lr, sp, 0
    ldw fp, sp, 4
    addi sp, sp, 24
    jalr r0, lr, 0

# ============================================================================
# File Operations
# ============================================================================

.global open
open:
    addi sp, sp, -56
    stw sp, fp, 4
    stw sp, lr, 0
    add fp, sp, r0
    addi fp, fp, 56
    stw fp, r11, -28
    stw fp, r12, -32
    stw fp, r13, -36
    stw fp, r14, -40
    stw fp, lr, -44
    add r12, r3, r0
    addi r1, fp, -24
    stw r1, r10, 20
    stw r1, r9, 16
    stw r1, r8, 12
    stw r1, r7, 8
    addi r3, r1, 4
    stw r3, r6, 0
    stw r1, r5, 0
    addi r13, r0, -1
    addi r1, r0, 0
    beq r12, r1, .Lopen_fail
    add r11, r4, r0
    add r3, r12, r0
    jal strlen
    addi r14, r1, 1
    lui r1, 12
    bgtu r14, r1, .Lopen_fail
    lui r3, %hi(__mmio_base+16384)
    addi r3, r3, %lo(__mmio_base+16384)
    add r4, r12, r0
    add r5, r14, r0
    jal memcpy
    addi r3, r0, 5
    addi r5, r0, 0
    add r4, r14, r0
    add r6, r11, r0
    jal s32_mmio_request
    add r13, r1, r0
.Lopen_fail:
    add r1, r13, r0
    ldw lr, fp, -44
    ldw r14, fp, -40
    ldw r13, fp, -36
    ldw r12, fp, -32
    ldw r11, fp, -28
    ldw lr, sp, 0
    ldw fp, sp, 4
    addi sp, sp, 56
    jalr r0, lr, 0

.global close
close:
    addi sp, sp, -24
    stw sp, fp, 4
    stw sp, lr, 0
    add fp, sp, r0
    addi fp, fp, 24
    stw fp, lr, -4
    add r6, r3, r0
    addi r3, r0, 6
    addi r4, r0, 0
    add r5, r4, r0
    jal s32_mmio_request
    ldw lr, fp, -4
    ldw lr, sp, 0
    ldw fp, sp, 4
    addi sp, sp, 24
    jalr r0, lr, 0

.global read
read:
    addi sp, sp, -56
    stw sp, fp, 4
    stw sp, lr, 0
    add fp, sp, r0
    addi fp, fp, 56
    stw fp, r11, -4
    stw fp, r12, -8
    stw fp, r13, -12
    stw fp, r14, -16
    stw fp, r15, -20
    stw fp, r16, -24
    stw fp, r17, -28
    stw fp, r18, -32
    stw fp, r19, -36
    stw fp, r20, -40
    stw fp, r21, -44
    stw fp, lr, -48
    addi r11, r0, -1
    addi r1, r0, 0
    blt r3, r1, .Lread_done
    add r13, r4, r0
    beq r4, r1, .Lread_done
    add r12, r5, r0
    add r11, r1, r0
    beq r5, r1, .Lread_done
    add r14, r3, r0
    addi r15, r0, 0
    lui r20, 12
    addi r16, r0, 4
    addi r21, r0, -2
    lui r17, %hi(__mmio_base+16384)
    addi r17, r17, %lo(__mmio_base+16384)
    add r11, r15, r0
.Lread_loop:
    bleu r12, r11, .Lread_done
    sub r1, r12, r11
    xor r3, r1, r20
    sltu r1, r1, r20
    sub r1, r15, r1
    and r1, r3, r1
    xor r18, r1, r20
    add r3, r16, r0
    add r4, r18, r0
    add r5, r15, r0
    add r6, r14, r0
    jal s32_mmio_request
    bgeu r1, r21, .Lread_err
    add r19, r1, r0
    beq r1, r15, .Lread_done
    add r3, r13, r11
    add r4, r17, r0
    add r5, r19, r0
    jal memcpy
    add r11, r19, r11
    bgeu r19, r18, .Lread_loop
    jal r0, .Lread_done
.Lread_err:
    addi r1, r0, 0
    seq r3, r11, r1
    sub r1, r1, r3
    or  r11, r1, r11
.Lread_done:
    add r1, r11, r0
    ldw lr, fp, -48
    ldw r21, fp, -44
    ldw r20, fp, -40
    ldw r19, fp, -36
    ldw r18, fp, -32
    ldw r17, fp, -28
    ldw r16, fp, -24
    ldw r15, fp, -20
    ldw r14, fp, -16
    ldw r13, fp, -12
    ldw r12, fp, -8
    ldw r11, fp, -4
    ldw lr, sp, 0
    ldw fp, sp, 4
    addi sp, sp, 56
    jalr r0, lr, 0

.global write
write:
    addi sp, sp, -56
    stw sp, fp, 4
    stw sp, lr, 0
    add fp, sp, r0
    addi fp, fp, 56
    stw fp, r11, -4
    stw fp, r12, -8
    stw fp, r13, -12
    stw fp, r14, -16
    stw fp, r15, -20
    stw fp, r16, -24
    stw fp, r17, -28
    stw fp, r18, -32
    stw fp, r19, -36
    stw fp, r20, -40
    stw fp, lr, -44
    addi r11, r0, -1
    addi r1, r0, 0
    blt r3, r1, .Lwrite_done
    add r13, r4, r0
    beq r4, r1, .Lwrite_done
    add r12, r5, r0
    add r11, r1, r0
    beq r5, r1, .Lwrite_done
    add r14, r3, r0
    addi r15, r0, 0
    lui r19, 12
    lui r16, %hi(__mmio_base+16384)
    addi r16, r16, %lo(__mmio_base+16384)
    addi r17, r0, 3
    addi r20, r0, -2
    add r11, r15, r0
.Lwrite_loop:
    bleu r12, r11, .Lwrite_done
    sub r1, r12, r11
    xor r3, r1, r19
    sltu r1, r1, r19
    sub r1, r15, r1
    and r1, r3, r1
    xor r18, r1, r19
    add r4, r13, r11
    add r3, r16, r0
    add r5, r18, r0
    jal memcpy
    add r3, r17, r0
    add r4, r18, r0
    add r5, r15, r0
    add r6, r14, r0
    jal s32_mmio_request
    bgeu r1, r20, .Lwrite_err
    add r11, r1, r11
    bgeu r1, r18, .Lwrite_loop
    jal r0, .Lwrite_done
.Lwrite_err:
    addi r1, r0, 0
    seq r3, r11, r1
    sub r1, r1, r3
    or  r11, r1, r11
.Lwrite_done:
    add r1, r11, r0
    ldw lr, fp, -44
    ldw r20, fp, -40
    ldw r19, fp, -36
    ldw r18, fp, -32
    ldw r17, fp, -28
    ldw r16, fp, -24
    ldw r15, fp, -20
    ldw r14, fp, -16
    ldw r13, fp, -12
    ldw r12, fp, -8
    ldw r11, fp, -4
    ldw lr, sp, 0
    ldw fp, sp, 4
    addi sp, sp, 56
    jalr r0, lr, 0

.global lseek
lseek:
    addi sp, sp, -24
    stw sp, fp, 4
    stw sp, lr, 0
    add fp, sp, r0
    addi fp, fp, 24
    stw fp, r11, -4
    stw fp, lr, -8
    addi r11, r0, 0
    blt r3, r11, .Llseek_fail
    add r6, r3, r0
    lui r1, %hi(__mmio_base+16384)
    addi r1, r1, %lo(__mmio_base+16384)
    stb r1, r5, 0
    lui r1, %hi(__mmio_base+16388)
    addi r1, r1, %lo(__mmio_base+16388)
    stw r1, r4, 0
    addi r3, r0, 7
    addi r4, r0, 8
    add r5, r11, r0
    jal s32_mmio_request
    addi r3, r0, -1
    xor r4, r1, r3
    sgt r1, r1, r3
    sub r1, r11, r1
    and r1, r4, r1
    xor r1, r1, r3
    jal r0, .Llseek_done
.Llseek_fail:
    addi r1, r0, -1
.Llseek_done:
    ldw lr, fp, -8
    ldw r11, fp, -4
    ldw lr, sp, 0
    ldw fp, sp, 4
    addi sp, sp, 24
    jalr r0, lr, 0

.global fstat
fstat:
    addi sp, sp, -88
    stw sp, fp, 4
    stw sp, lr, 0
    add fp, sp, r0
    addi fp, fp, 88
    stw fp, r11, -4
    stw fp, r12, -8
    stw fp, r13, -12
    stw fp, r14, -16
    stw fp, r15, -20
    stw fp, r16, -24
    stw fp, r17, -28
    stw fp, r18, -32
    stw fp, r19, -36
    stw fp, r20, -40
    stw fp, r21, -44
    stw fp, r22, -48
    stw fp, r23, -52
    stw fp, r24, -56
    stw fp, r25, -60
    stw fp, r26, -64
    stw fp, r27, -68
    stw fp, lr, -72
    addi r12, r0, -1
    addi r11, r0, 0
    blt r3, r11, .Lfstat_done
    beq r4, r11, .Lfstat_done
    add r6, r3, r0
    add r13, r4, r0
    addi r3, r0, 10
    add r4, r11, r0
    add r5, r11, r0
    jal s32_mmio_request
    bne r1, r11, .Lfstat_done
    lui r1, %hi(__mmio_base+16388)
    addi r1, r1, %lo(__mmio_base+16388)
    ldw r1, r1, 0
    lui r3, %hi(__mmio_base+16384)
    addi r3, r3, %lo(__mmio_base+16384)
    ldw r3, r3, 0
    lui r4, %hi(__mmio_base+16396)
    addi r4, r4, %lo(__mmio_base+16396)
    ldw r4, r4, 0
    lui r5, %hi(__mmio_base+16392)
    addi r5, r5, %lo(__mmio_base+16392)
    ldw r5, r5, 0
    lui r6, %hi(__mmio_base+16400)
    addi r6, r6, %lo(__mmio_base+16400)
    ldw r6, r6, 0
    lui r7, %hi(__mmio_base+16404)
    addi r7, r7, %lo(__mmio_base+16404)
    ldw r7, r7, 0
    lui r8, %hi(__mmio_base+16408)
    addi r8, r8, %lo(__mmio_base+16408)
    ldw r8, r8, 0
    lui r9, %hi(__mmio_base+16412)
    addi r9, r9, %lo(__mmio_base+16412)
    ldw r9, r9, 0
    lui r10, %hi(__mmio_base+16420)
    addi r10, r10, %lo(__mmio_base+16420)
    ldw r10, r10, 0
    lui r12, %hi(__mmio_base+16416)
    addi r12, r12, %lo(__mmio_base+16416)
    ldw r12, r12, 0
    lui r14, %hi(__mmio_base+16428)
    addi r14, r14, %lo(__mmio_base+16428)
    ldw r14, r14, 0
    lui r15, %hi(__mmio_base+16424)
    addi r15, r15, %lo(__mmio_base+16424)
    ldw r15, r15, 0
    lui r16, %hi(__mmio_base+16432)
    addi r16, r16, %lo(__mmio_base+16432)
    ldw r16, r16, 0
    stw r13, r3, 0
    stw r13, r1, 4
    stw r13, r5, 8
    stw r13, r4, 12
    stw r13, r6, 16
    stw r13, r7, 20
    stw r13, r8, 24
    stw r13, r9, 28
    stw r13, r12, 32
    stw r13, r10, 36
    stw r13, r15, 40
    stw r13, r14, 44
    stw r13, r16, 48
    add r12, r11, r0
.Lfstat_done:
    add r1, r12, r0
    ldw lr, fp, -72
    ldw r27, fp, -68
    ldw r26, fp, -64
    ldw r25, fp, -60
    ldw r24, fp, -56
    ldw r23, fp, -52
    ldw r22, fp, -48
    ldw r21, fp, -44
    ldw r20, fp, -40
    ldw r19, fp, -36
    ldw r18, fp, -32
    ldw r17, fp, -28
    ldw r16, fp, -24
    ldw r15, fp, -20
    ldw r14, fp, -16
    ldw r13, fp, -12
    ldw r12, fp, -8
    ldw r11, fp, -4
    ldw lr, sp, 0
    ldw fp, sp, 4
    addi sp, sp, 88
    jalr r0, lr, 0

.global unlink
unlink:
    addi sp, sp, -24
    stw sp, fp, 4
    stw sp, lr, 0
    add fp, sp, r0
    addi fp, fp, 24
    stw fp, r11, -4
    stw fp, r12, -8
    stw fp, r13, -12
    stw fp, lr, -16
    addi r12, r0, -1
    addi r11, r0, 0
    beq r3, r11, .Lunlink_done
    add r13, r3, r0
    jal strlen
    lui r3, 12
    addi r3, r3, -1
    bgtu r1, r3, .Lunlink_done
    addi r12, r1, 1
    lui r3, %hi(__mmio_base+16384)
    addi r3, r3, %lo(__mmio_base+16384)
    add r4, r13, r0
    add r5, r12, r0
    jal memcpy
    addi r3, r0, 32
    add r4, r12, r0
    add r5, r11, r0
    add r6, r11, r0
    jal s32_mmio_request
    sne r1, r1, r11
    sub r12, r11, r1
.Lunlink_done:
    add r1, r12, r0
    ldw lr, fp, -16
    ldw r13, fp, -12
    ldw r12, fp, -8
    ldw r11, fp, -4
    ldw lr, sp, 0
    ldw fp, sp, 4
    addi sp, sp, 24
    jalr r0, lr, 0

.global rename
rename:
    addi sp, sp, -40
    stw sp, fp, 4
    stw sp, lr, 0
    add fp, sp, r0
    addi fp, fp, 40
    stw fp, r11, -4
    stw fp, r12, -8
    stw fp, r13, -12
    stw fp, r14, -16
    stw fp, r15, -20
    stw fp, r16, -24
    stw fp, r17, -28
    stw fp, lr, -32
    addi r12, r0, -1
    addi r11, r0, 0
    beq r3, r11, .Lrename_done
    add r13, r4, r0
    beq r4, r11, .Lrename_done
    add r15, r3, r0
    jal strlen
    addi r14, r1, 1
    sltu r16, r14, r1
    add r3, r13, r0
    jal strlen
    bne r16, r11, .Lrename_done
    addi r17, r1, 1
    sltu r1, r17, r1
    bne r1, r11, .Lrename_done
    add r16, r17, r14
    lui r1, 12
    bgtu r16, r1, .Lrename_done
    lui r12, %hi(__mmio_base+16384)
    addi r12, r12, %lo(__mmio_base+16384)
    add r3, r12, r0
    add r4, r15, r0
    add r5, r14, r0
    jal memcpy
    add r3, r14, r12
    add r4, r13, r0
    add r5, r17, r0
    jal memcpy
    addi r3, r0, 33
    add r4, r16, r0
    add r5, r11, r0
    add r6, r14, r0
    jal s32_mmio_request
    sne r1, r1, r11
    sub r12, r11, r1
.Lrename_done:
    add r1, r12, r0
    ldw lr, fp, -32
    ldw r17, fp, -28
    ldw r16, fp, -24
    ldw r15, fp, -20
    ldw r14, fp, -16
    ldw r13, fp, -12
    ldw r12, fp, -8
    ldw r11, fp, -4
    ldw lr, sp, 0
    ldw fp, sp, 4
    addi sp, sp, 40
    jalr r0, lr, 0

.global usleep
usleep:
    addi sp, sp, -24
    stw sp, fp, 4
    stw sp, lr, 0
    add fp, sp, r0
    addi fp, fp, 24
    stw fp, r11, -4
    stw fp, lr, -8
    # Convert microseconds to seconds and nanoseconds
    # sec = usec / 1000000, nsec = (usec % 1000000) * 1000
    lui r1, 274878
    addi r1, r1, -381
    mulhu r1, r3, r1
    srli r1, r1, 18
    lui r4, 244
    addi r4, r4, 576
    mul r4, r1, r4
    sub r3, r3, r4
    addi r4, r0, 1000
    mul r3, r3, r4
    lui r4, %hi(__mmio_base+16384)
    addi r4, r4, %lo(__mmio_base+16384)
    stw r4, r1, 0
    addi r11, r0, 0
    lui r1, %hi(__mmio_base+16388)
    addi r1, r1, %lo(__mmio_base+16388)
    stw r1, r11, 0
    lui r1, %hi(__mmio_base+16392)
    addi r1, r1, %lo(__mmio_base+16392)
    stw r1, r3, 0
    lui r1, %hi(__mmio_base+16396)
    addi r1, r1, %lo(__mmio_base+16396)
    stw r1, r11, 0
    addi r3, r0, 11
    addi r4, r0, 16
    add r5, r11, r0
    add r6, r11, r0
    jal s32_mmio_request
    add r1, r11, r0
    ldw lr, fp, -8
    ldw r11, fp, -4
    ldw lr, sp, 0
    ldw fp, sp, 4
    addi sp, sp, 24
    jalr r0, lr, 0

# ============================================================================
# String Operations
# ============================================================================

.global strlen
strlen:
    ldbu r4, r3, 0
    addi r1, r0, 0
    beq r4, r1, .Lstrlen_done
    addi r3, r3, 1
    addi r4, r0, 0
    add r1, r4, r0
.Lstrlen_loop:
    add r5, r3, r1
    addi r1, r1, 1
    ldbu r5, r5, 0
    bne r5, r4, .Lstrlen_loop
.Lstrlen_done:
    jalr r0, lr, 0

.global memset
memset:
    add r1, r3, r0
    addi r3, r0, 0
    beq r5, r3, .Lmemset_done
    add r6, r1, r0
.Lmemset_loop:
    addi r5, r5, -1
    addi r7, r6, 1
    stb r6, r4, 0
    add r6, r7, r0
    bne r5, r3, .Lmemset_loop
.Lmemset_done:
    jalr r0, lr, 0

.global memcpy
memcpy:
    add r1, r3, r0
    addi r3, r0, 0
    beq r5, r3, .Lmemcpy_done
    add r6, r1, r0
.Lmemcpy_loop:
    addi r5, r5, -1
    addi r7, r4, 1
    ldbu r4, r4, 0
    addi r8, r6, 1
    stb r6, r4, 0
    add r4, r7, r0
    add r6, r8, r0
    bne r5, r3, .Lmemcpy_loop
.Lmemcpy_done:
    jalr r0, lr, 0

.global memmove
memmove:
    add r1, r3, r0
    bgeu r3, r4, .Lmemmove_fwd
    # Forward copy (src < dest could overlap)
    addi r3, r0, 0
    beq r5, r3, .Lmemmove_done
    add r6, r1, r0
.Lmemmove_fwd_loop:
    addi r5, r5, -1
    addi r7, r4, 1
    ldbu r4, r4, 0
    addi r8, r6, 1
    stb r6, r4, 0
    add r4, r7, r0
    add r6, r8, r0
    bne r5, r3, .Lmemmove_fwd_loop
    jal r0, .Lmemmove_done
.Lmemmove_fwd:
    bleu r1, r4, .Lmemmove_done
    # Backward copy (for overlapping regions where dest > src)
    addi r3, r0, 0
    beq r5, r3, .Lmemmove_done
    addi r4, r4, -1
    addi r6, r1, -1
.Lmemmove_bwd_loop:
    addi r7, r5, -1
    add r8, r4, r5
    ldbu r8, r8, 0
    add r5, r6, r5
    stb r5, r8, 0
    add r5, r7, r0
    bne r7, r3, .Lmemmove_bwd_loop
.Lmemmove_done:
    jalr r0, lr, 0

# ============================================================================
# C Runtime Entry
# ============================================================================

.global __slow32_start
__slow32_start:
    addi sp, sp, -16
    stw sp, lr, 0
    stw sp, fp, 4
    add fp, sp, r0
    # Call main(0, NULL)
    addi r3, r0, 0
    addi r4, r0, 0
    call main
    # Exit with return value
    add r3, r1, r0
    jal exit

.global exit
exit:
    halt

# ============================================================================
# BSS Variables
# ============================================================================

.bss
.global errno
errno:
    .space 4
