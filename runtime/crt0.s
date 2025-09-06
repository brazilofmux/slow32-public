# SLOW-32 C runtime startup code
# Sets up stack, calls main, then halts

.global _start

_start:
    # Initialize stack pointer (256MB - 16 bytes)
    # Note: lui loads into bits [31:12], so 0x10000 becomes 0x10000000
    lui  sp, 0x10000    # sp = 0x10000000 (256MB)
    addi sp, sp, -16    # sp = 0x0FFFFFF0
    
    # Clear frame pointer
    add  fp, r0, r0
    
    # Call main
    jal  main
    
    # Exit - just halt after main returns
    halt