# SLOW-32 Minimal C Runtime Startup for Selfhost Stage02+
#
# Simplified version that:
#   - Assumes stack pointer initialized from .s32x header
#   - Zeros the .bss section
#   - Invokes __slow32_start (provided by libc/start.c)
#
# This copy lives with the stage02 toolchain so later stages no longer
# depend on stage01 sources once the C cycle begins.

.text
.global _start

_start:
    # Stack pointer was primed by the loader. Reserve a little scratch.
    addi sp, sp, -16

    # Zero the BSS section.
    lui  r3, %hi(__bss_start)
    addi r3, r3, %lo(__bss_start)
    add  r4, r0, r0
    lui  r5, %hi(__bss_end)
    addi r5, r5, %lo(__bss_end)
    sub  r5, r5, r3
    beq  r5, r0, .L_bss_done
    # Address-forming call: a jal reaches +/-1MB, and crt0 sits at 0
    # while libc may follow a 1.2MB library (SQLite built by stage08).
    lui  r1, %hi(memset)
    addi r1, r1, %lo(memset)
    jalr r31, r1, 0
.L_bss_done:

    # Clear frame pointer and tail-call into libc startup.
    add  fp, r0, r0
    lui  r1, %hi(__slow32_start)
    addi r1, r1, %lo(__slow32_start)
    jalr r31, r1, 0

    # __slow32_start should never return, but halt defensively.
    halt
