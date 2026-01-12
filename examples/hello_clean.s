# Clean Hello World example using standardized directives
# Uses RISC-V/GNU assembler conventions

.global _start

.data
    # Define the hello message using .byte directive
    msg:
        .byte 72, 101, 108, 108, 111, 44, 32   # "Hello, "
        .byte 87, 111, 114, 108, 100, 33        # "World!"
        .byte 10, 0                             # newline and null terminator

.text
_start:
    # Initialize stack pointer
    lui sp, 0x1000
    addi sp, sp, -16
    
    # Load data segment base address
    lui r10, 0x100      # r10 = 0x100000 (data segment base)
    add r11, r10, r0    # r11 = current character pointer
    
print_loop:
    # Load current character
    ldbu r12, r11+0
    
    # Check for null terminator
    seq r13, r12, r0
    bne r13, r0, done
    
    # Print character
    debug r12
    
    # Move to next character
    addi r11, r11, 1
    beq r0, r0, print_loop
    
done:
    halt