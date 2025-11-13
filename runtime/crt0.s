# SLOW-32 C runtime startup code
# Sets up stack, calls main, then halts

.global _start

_start:
    # Stack pointer is already initialized by the loader from .s32x header
    # Just reserve space for main's return
    addi sp, sp, -16
    
    # Clear frame pointer
    add  fp, r0, r0
    
    # Call runtime entry point that fetches argc/argv and calls main
    jal  __slow32_start
    
    # Fallback halt if __slow32_start ever returns
    halt
