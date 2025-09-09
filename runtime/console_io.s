# Console I/O functions using MMIO ring buffers
# Provides: putchar, getchar, puts, gets

.text
.global putchar
.global getchar
.global puts
.global gets
.global print_string
.global print_hex

# MMIO addresses
.equ MMIO_BASE,     0x10000000
.equ REQ_HEAD,      0x10000000
.equ REQ_TAIL,      0x10000004
.equ REQ_RING,      0x10001000
.equ RESP_HEAD,     0x10002000
.equ RESP_TAIL,     0x10002004
.equ RESP_RING,     0x10003000
.equ DATA_BUFFER,   0x10004000

# Ring parameters
.equ RING_SIZE,     256
.equ DESC_SIZE,     16

# I/O opcodes
.equ IO_OP_PUTCHAR, 0x01
.equ IO_OP_GETCHAR, 0x02
.equ IO_OP_WRITE,   0x03
.equ IO_OP_READ,    0x04
.equ IO_OP_FLUSH,   0x0B

# putchar(char c) - Output a single character
# Arguments: r3 = character to output
# Returns: r1 = 0 on success, -1 on error
putchar:
    # Save registers
    ADDI sp, sp, -16
    STW sp, r30, 0
    STW sp, r31, 4
    ADDI r30, sp, 16
    
    # Load MMIO base
    LUI r5, 0x10000     # r5 = MMIO_BASE
    
    # Store character in data buffer at offset 0
    STW r5, r3, 0x4000  # DATA_BUFFER[0] = char
    
    # Get current request head
    LDW r6, r5, 0       # r6 = req_head
    
    # Calculate descriptor address: REQ_RING + (head * DESC_SIZE)
    SLLI r7, r6, 4      # r7 = head * 16
    ADDI r8, r5, 0x1000 # r8 = REQ_RING base
    ADD r8, r8, r7      # r8 = descriptor address
    
    # Build descriptor
    ADDI r9, r0, 1      # IO_OP_PUTCHAR = 0x01
    STW r8, r9, 0       # desc.opcode = IO_OP_PUTCHAR
    ADDI r9, r0, 1
    STW r8, r9, 4       # desc.length = 1
    STW r8, r0, 8       # desc.offset = 0
    STW r8, r0, 12      # desc.status = 0
    
    # Increment head
    ADDI r6, r6, 1
    ANDI r6, r6, 255    # head = (head + 1) % 256
    STW r5, r6, 0       # Update REQ_HEAD
    
    # Trigger MMIO processing
    YIELD r0, r0, r0
    
    # Wait for response
    LDW r10, r5, 0x2004 # r10 = resp_tail
putchar_wait:
    LDW r11, r5, 0x2000 # r11 = resp_head
    BEQ r11, r10, putchar_wait  # Wait while resp_head == resp_tail
    
    # Read response descriptor
    SLLI r7, r10, 4     # r7 = tail * 16
    ADDI r8, r5, 0x3000 # r8 = RESP_RING base
    ADD r8, r8, r7      # r8 = response descriptor
    LDW r1, r8, 12      # r1 = status
    
    # Update response tail
    ADDI r10, r10, 1
    ANDI r10, r10, 255  # tail = (tail + 1) % 256
    STW r5, r10, 0x2004 # Update RESP_TAIL
    
    # Restore registers
    LDW r30, sp, 0
    LDW r31, sp, 4
    ADDI sp, sp, 16
    JALR r0, r31, 0

# getchar() - Read a single character
# Returns: r1 = character read, or -1 on EOF
getchar:
    # Save registers
    ADDI sp, sp, -16
    STW sp, r30, 0
    STW sp, r31, 4
    ADDI r30, sp, 16
    
    # Load MMIO base
    LUI r5, 0x10000     # r5 = MMIO_BASE
    
    # Get current request head
    LDW r6, r5, 0       # r6 = req_head
    
    # Calculate descriptor address
    SLLI r7, r6, 4      # r7 = head * 16
    ADDI r8, r5, 0x1000 # r8 = REQ_RING base
    ADD r8, r8, r7      # r8 = descriptor address
    
    # Build descriptor
    ADDI r9, r0, 2      # IO_OP_GETCHAR = 0x02
    STW r8, r9, 0       # desc.opcode = IO_OP_GETCHAR
    STW r8, r0, 4       # desc.length = 0
    STW r8, r0, 8       # desc.offset = 0
    STW r8, r0, 12      # desc.status = 0
    
    # Increment head
    ADDI r6, r6, 1
    ANDI r6, r6, 255    # head = (head + 1) % 256
    STW r5, r6, 0       # Update REQ_HEAD
    
    # Trigger MMIO processing
    YIELD r0, r0, r0
    
    # Wait for response
    LDW r10, r5, 0x2004 # r10 = resp_tail
getchar_wait:
    LDW r11, r5, 0x2000 # r11 = resp_head
    BEQ r11, r10, getchar_wait  # Wait while resp_head == resp_tail
    
    # Read response descriptor
    SLLI r7, r10, 4     # r7 = tail * 16
    ADDI r8, r5, 0x3000 # r8 = RESP_RING base
    ADD r8, r8, r7      # r8 = response descriptor
    LDW r12, r8, 4      # r12 = length
    LDW r13, r8, 12     # r13 = status
    
    # Check if we got a character
    BEQ r12, r0, getchar_eof
    
    # Read the character from data buffer
    LDW r1, r5, 0x4000  # r1 = DATA_BUFFER[0]
    ANDI r1, r1, 0xFF   # Mask to byte
    JAL r0, getchar_done
    
getchar_eof:
    ADDI r1, r0, -1     # Return -1 for EOF
    
getchar_done:
    # Update response tail
    ADDI r10, r10, 1
    ANDI r10, r10, 255  # tail = (tail + 1) % 256
    STW r5, r10, 0x2004 # Update RESP_TAIL
    
    # Restore registers
    LDW r30, sp, 0
    LDW r31, sp, 4
    ADDI sp, sp, 16
    JALR r0, r31, 0

# puts(const char *str) - Output a null-terminated string
# Arguments: r3 = pointer to string
# Returns: r1 = number of characters written
puts:
    # Save registers
    ADDI sp, sp, -20
    STW sp, r30, 0
    STW sp, r31, 4
    STW sp, r20, 8      # Save string pointer
    STW sp, r21, 12     # Save counter
    ADDI r30, sp, 20
    
    ADD r20, r3, r0         # r20 = string pointer
    ADDI r21, r0, 0     # r21 = character counter
    
puts_loop:
    LDB r3, r20, 0      # Load byte
    BEQ r3, r0, puts_newline  # If null, add newline and exit
    
    # Call putchar
    JAL putchar
    
    ADDI r20, r20, 1    # Increment pointer
    ADDI r21, r21, 1    # Increment counter
    JAL r0, puts_loop
    
puts_newline:
    # Output newline
    ADDI r3, r0, 10     # '\n'
    JAL putchar
    ADDI r21, r21, 1    # Count the newline
    
    ADD r1, r21, r0         # Return character count
    
    # Restore registers
    LDW r30, sp, 0
    LDW r31, sp, 4
    LDW r20, sp, 8
    LDW r21, sp, 12
    ADDI sp, sp, 20
    JALR r0, r31, 0

# print_string(const char *str) - Output string without newline
# Arguments: r3 = pointer to string
# Returns: r1 = number of characters written
print_string:
    # Save registers
    ADDI sp, sp, -20
    STW sp, r30, 0
    STW sp, r31, 4
    STW sp, r20, 8      # Save string pointer
    STW sp, r21, 12     # Save counter
    ADDI r30, sp, 20
    
    ADD r20, r3, r0         # r20 = string pointer
    ADDI r21, r0, 0     # r21 = character counter
    
print_loop:
    LDB r3, r20, 0      # Load byte
    BEQ r3, r0, print_done  # If null, exit
    
    # Call putchar
    JAL putchar
    
    ADDI r20, r20, 1    # Increment pointer
    ADDI r21, r21, 1    # Increment counter
    JAL r0, print_loop
    
print_done:
    ADD r1, r21, r0         # Return character count
    
    # Restore registers
    LDW r30, sp, 0
    LDW r31, sp, 4
    LDW r20, sp, 8
    LDW r21, sp, 12
    ADDI sp, sp, 20
    JALR r0, r31, 0

# print_hex(uint32_t value) - Print value as hexadecimal
# Arguments: r3 = value to print
# Returns: nothing
print_hex:
    # Save registers
    ADDI sp, sp, -24
    STW sp, r30, 0
    STW sp, r31, 4
    STW sp, r20, 8      # Save value
    STW sp, r21, 12     # Save digit counter
    ADDI r30, sp, 24
    
    ADD r20, r3, r0         # r20 = value
    
    # Print "0x" prefix
    ADDI r3, r0, 48     # '0'
    JAL putchar
    ADDI r3, r0, 120    # 'x'
    JAL putchar
    
    # Print 8 hex digits
    ADDI r21, r0, 8     # 8 digits
    
hex_loop:
    # Extract top 4 bits
    SRLI r3, r20, 28
    
    # Convert to hex character
    SLTI r4, r3, 10     # r4 = (digit < 10)
    BEQ r4, r0, hex_letter
    
    # It's 0-9
    ADDI r3, r3, 48     # '0' + digit
    JAL r0, hex_print
    
hex_letter:
    # It's A-F
    ADDI r3, r3, 55     # 'A' + (digit - 10)
    
hex_print:
    JAL putchar
    
    # Shift value left by 4
    SLLI r20, r20, 4
    
    # Decrement counter
    ADDI r21, r21, -1
    BNE r21, r0, hex_loop
    
    # Restore registers
    LDW r30, sp, 0
    LDW r31, sp, 4
    LDW r20, sp, 8
    LDW r21, sp, 12
    ADDI sp, sp, 24
    JALR r0, r31, 0