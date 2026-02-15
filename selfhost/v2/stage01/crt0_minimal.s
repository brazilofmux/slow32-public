# SLOW-32 Minimal C Runtime Startup for Selfhost Bootstrap
#
# Simplified version that:
#   - Sets up stack (already done by loader from .s32x header)
#   - Zeros BSS section
#   - Calls __slow32_start
#
# This version avoids complex dependencies.

.text
.global _start

_start:
    # Stack pointer is already initialized by the loader from .s32x header
    # Reserve space for potential use
    addi sp, sp, -16

    # Zero .bss section
    lui  r3, %hi(__bss_start)
    addi r3, r3, %lo(__bss_start)
    add  r4, r0, r0                # r4 = 0 (fill value)
    lui  r5, %hi(__bss_end)
    addi r5, r5, %lo(__bss_end)
    sub  r5, r5, r3                # r5 = length
    beq  r5, r0, .L_bss_done
    call memset
.L_bss_done:

    # Clear frame pointer
    add  fp, r0, r0

    # Call runtime entry point
    call __slow32_start

    # Fallback halt if __slow32_start ever returns
    halt
