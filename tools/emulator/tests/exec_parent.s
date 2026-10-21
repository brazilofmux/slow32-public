# Issue MMIO EXEC of a dash-named sibling `-t.s32x`, then HALT.
# HALT is a service point, so the request runs before the parent stops.
.global _start
.text
_start:
    lui     r10, %hi(__mmio_base)
    addi    r10, r10, %lo(__mmio_base)

    lui     r11, %hi(__mmio_base+16384)
    addi    r11, r11, %lo(__mmio_base+16384)

    # path "-t.s32x\0" (8 bytes) at data-buffer offset 0
    addi    r1, r0, 45      # '-'
    stb     r11+0, r1
    addi    r1, r0, 116     # 't'
    stb     r11+1, r1
    addi    r1, r0, 46      # '.'
    stb     r11+2, r1
    addi    r1, r0, 115     # 's'
    stb     r11+3, r1
    addi    r1, r0, 51      # '3'
    stb     r11+4, r1
    addi    r1, r0, 50      # '2'
    stb     r11+5, r1
    addi    r1, r0, 120     # 'x'
    stb     r11+6, r1
    stb     r11+7, r0

    lui     r12, %hi(__mmio_base+4096)
    addi    r12, r12, %lo(__mmio_base+4096)
    addi    r1, r0, 16      # OP_EXEC
    stw     r12+0, r1
    addi    r1, r0, 8       # length
    stw     r12+4, r1
    stw     r12+8, r0       # offset 0
    addi    r1, r0, -1      # inherit host stdio
    stw     r12+12, r1

    addi    r1, r0, 1
    stw     r10+0, r1       # REQ_HEAD = 1

    yield   r0, r0, 0
    halt

# Exceed the linker's 4KB compact-layout threshold so MMIO sits
# below the default high stack rather than overlapping it.
.bss
    .space 8192
