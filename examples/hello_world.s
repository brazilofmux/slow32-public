# hello_world.s - Basic SLOW-32 program
# Prints "Hello, World!" and exits

        .section .text
        .globl _start
_start:
        # Print "Hello, World!\n"
        addi r1, r0, 72    # 'H'
        debug r1
        addi r1, r0, 101   # 'e'
        debug r1
        addi r1, r0, 108   # 'l'
        debug r1
        addi r1, r0, 108   # 'l'
        debug r1
        addi r1, r0, 111   # 'o'
        debug r1
        addi r1, r0, 44    # ','
        debug r1
        addi r1, r0, 32    # ' '
        debug r1
        addi r1, r0, 87    # 'W'
        debug r1
        addi r1, r0, 111   # 'o'
        debug r1
        addi r1, r0, 114   # 'r'
        debug r1
        addi r1, r0, 108   # 'l'
        debug r1
        addi r1, r0, 100   # 'd'
        debug r1
        addi r1, r0, 33    # '!'
        debug r1
        addi r1, r0, 10    # '\n'
        debug r1
        
        # Exit with code 0
        addi r1, r0, 0
        halt