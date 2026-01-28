# SLOW-32 C runtime startup code
# Sets up stack, calls main, then halts

.global _start

_start:
    # Stack pointer is already initialized by the loader from .s32x header
    # Just reserve space for main's return
    addi sp, sp, -16

    # Zero .bss (embedded convention: data already loaded, bss cleared)
    lui  r1, %hi(__bss_start)
    addi r1, r1, %lo(__bss_start)  # r1 = bss cursor
    lui  r2, %hi(__bss_end)
    addi r2, r2, %lo(__bss_end)    # r2 = bss end
.L_bss_clear:
    beq  r1, r2, .L_bss_done
    # Use legacy form (base, data, imm) to avoid 0(base) ambiguity in assembler.
    stb  r1, r0, 0
    addi r1, r1, 1
    jal  r0, .L_bss_clear
.L_bss_done:
    
    # Clear frame pointer
    add  fp, r0, r0
    
    # Call runtime entry point that fetches argc/argv and calls main
    jal  __slow32_start
    
    # Fallback halt if __slow32_start ever returns
    halt
