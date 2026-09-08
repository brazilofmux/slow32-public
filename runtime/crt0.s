# SLOW-32 C runtime startup code
# Sets up stack, calls main, then halts

.global _start

_start:
    # Stack pointer is already initialized by the loader from .s32x header
    # Just reserve space for main's return
    addi sp, sp, -16

    # Zero .bss via memset (fast on all emulators: dbt/QEMU intercept memset)
    lui  r3, %hi(__bss_start)
    addi r3, r3, %lo(__bss_start)  # r3 = dest (arg 1)
    add  r4, r0, r0                # r4 = 0 (arg 2)
    lui  r5, %hi(__bss_end)
    addi r5, r5, %lo(__bss_end)
    sub  r5, r5, r3                # r5 = length (arg 3)
    beq  r5, r0, .L_bss_done
    # Address-forming call: a jal reaches +/-1MB, and crt0 sits at 0
    # while libc may follow a 1.2MB library (SQLite built by stage08).
    lui  r1, %hi(memset)
    addi r1, r1, %lo(memset)
    jalr r31, r1, 0
.L_bss_done:
    
    # Clear frame pointer
    add  fp, r0, r0
    
    # Call runtime entry point that fetches argc/argv and calls main
    lui  r1, %hi(__slow32_start)
    addi r1, r1, %lo(__slow32_start)
    jalr r31, r1, 0
    
    # Fallback halt if __slow32_start ever returns
    halt
