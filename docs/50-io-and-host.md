# SLOW-32 I/O and Host Interfaces

## Overview

SLOW-32 currently provides minimal I/O capabilities, prioritizing simplicity over functionality. The primary I/O mechanism is the DEBUG instruction, with more sophisticated interfaces planned for future versions.

## Current I/O Support

### DEBUG Instruction

The DEBUG instruction is the sole I/O mechanism in SLOW-32 v1.0:

```asm
debug rs    # Output value in rs to debug console
```

#### Character Output
```asm
# Print single character
li r3, 'A'
debug r3    # Outputs 'A'

# Print newline
li r3, '\n'
debug r3
```

#### String Output
```asm
.section .rodata
message:
    .asciz "Hello, World!\n"

.section .text
print_string:
    # r3 = string pointer
    ldbu r2, r3+0
    beq r2, r0, print_done
    debug r2
    addi r3, r3, 1
    jal r0, print_string
print_done:
    jalr r0, r31, 0
```

#### Numeric Output

Since DEBUG only outputs characters, numbers must be converted:

```asm
# Print single digit (0-9)
print_digit:
    # r3 = digit value (0-9)
    addi r3, r3, '0'    # Convert to ASCII
    debug r3
    jalr r0, r31, 0

# Print hex digit (0-F)
print_hex_digit:
    # r3 = value (0-15)
    slti r2, r3, 10
    beq r2, r0, print_letter
    addi r3, r3, '0'
    jal r0, print_it
print_letter:
    addi r3, r3, 'A'-10
print_it:
    debug r3
    jalr r0, r31, 0
```

### Program Termination

Programs communicate with the host via return value:

```asm
# Return success (0)
li r1, 0
halt

# Return error code
li r1, 1    # Or any error code
halt
```

The emulator sets the shell exit code to r1's value:
```bash
$SLOW32_HOME/bin/slow32 program.s32x
echo $?    # Prints r1 value at halt
```

## Memory-Mapped I/O (MMIO) Support

### MMIO Console Interface

**Status**: Implemented in v2.0

SLOW-32 now includes full MMIO support for console I/O operations. The implementation features a ring buffer architecture for efficient bidirectional communication.

Memory map:
```
0x10000000 - 0x1FFFFFFF    MMIO region (256MB)
  0x10000000 - Console output (write-only)
  0x10000004 - Console input (read-only) 
  0x10000008 - Console status (read-only)
  0x10001000 - Timer (reserved for future)
  0x10002000 - Interrupt controller (reserved for future)
```

Example usage:
```asm
# Write character to console
li r3, 0x10000000
li r4, 'A'
stw r4, r3+0

# Read character with status check
li r3, 0x10000008  # Status register
li r4, 0x10000004  # Input register
wait_input:
    ldw r5, r3+0    # Check status
    beq r5, r0, wait_input
    ldw r6, r4+0    # Read character
```

### MMIO Implementation Features

- **Ring buffer architecture** for efficient data transfer
- **YIELD instruction support** for cooperative I/O waiting
- **Automatic heap allocation** in the linker
- **Console I/O with status checking**
- **Configurable via linker flags**

To enable MMIO in your programs:
```bash
# Link with MMIO support
./linker/s32-ld -o program.s32x --enable-mmio runtime/crt0.s32o program.s32o
```

### TRAP Instruction

**Status**: Under consideration

System call interface via TRAP:
```asm
# Proposed system calls
li r3, 1        # Syscall: write
li r4, 1        # File descriptor: stdout
li r5, buffer   # Buffer address
li r6, length   # Length
trap            # Invoke system call
```

### Host File I/O

**Status**: Under consideration

File operations through MMIO or TRAP:
- Open/close files
- Read/write data
- Seek position
- Get file info

## Debug Console Implementation

### Emulator Handling

The emulator processes DEBUG as:
```c
case OP_DEBUG:
    value = registers[rs];
    if (value >= 0x20 && value <= 0x7E) {
        putchar(value);  // Printable ASCII
    } else if (value == '\n' || value == '\t') {
        putchar(value);  // Whitespace
    }
    // Ignore other values
    break;
```

### Buffering

Output is line-buffered by default:
- Characters accumulate until newline
- Flush on newline or program exit
- Force flush with fflush (C runtime)

## C Runtime Support

### Character Output

```c
void debug_putchar(int c) {
    asm volatile("debug %0" : : "r"(c));
}

void debug_puts(const char* str) {
    while (*str) {
        debug_putchar(*str++);
    }
}
```

### Formatted Output

Simple printf implementation (no floating-point):

```c
void debug_printf(const char* fmt, ...) {
    // Supports %d, %x, %s, %c
    // See runtime/printf.c for implementation
}
```

## Performance Considerations

### DEBUG Instruction Overhead

- Reference emulator: ~10 cycles per character
- Fast emulator: ~3 cycles per character
- Actual hardware: Would depend on implementation

### Optimization Tips

1. **Buffer output**: Minimize DEBUG calls
2. **Batch operations**: Print strings, not characters
3. **Avoid in loops**: Move DEBUG outside hot paths
4. **Use fast emulator**: For I/O-heavy programs

## Examples

### Hello World
```asm
.section .rodata
msg:    .asciz "Hello, World!\n"

.section .text
.globl _start
_start:
    li r3, msg
print:
    ldbu r2, r3+0
    beq r2, r0, done
    debug r2
    addi r3, r3, 1
    jal r0, print
done:
    li r1, 0
    halt
```

### Print Number
```asm
# Print 32-bit number in hex
print_hex:
    # r3 = value to print
    li r4, 8        # 8 hex digits
hex_loop:
    srli r5, r3, 28     # Get top nibble
    andi r5, r5, 0xF
    # Convert to ASCII
    slti r6, r5, 10
    beq r6, r0, letter
    addi r5, r5, '0'
    jal r0, output
letter:
    addi r5, r5, 'A'-10
output:
    debug r5
    slli r3, r3, 4      # Shift left
    addi r4, r4, -1
    bne r4, r0, hex_loop
    jalr r0, r31, 0
```

### Error Messages
```asm
.section .rodata
error_msg:  .asciz "Error: Division by zero\n"

.section .text
error_handler:
    li r3, error_msg
    jal r31, print_string
    li r1, 1        # Error code
    halt
```

## Testing I/O

### Unit Tests
```bash
# Test character output
echo 'A' > expected.txt
$SLOW32_HOME/bin/slow32 test_char.s32x > actual.txt
diff expected.txt actual.txt

# Test string output
echo "Hello, World!" > expected.txt
$SLOW32_HOME/bin/slow32 test_string.s32x > actual.txt
diff expected.txt actual.txt
```

### Interactive Testing
```bash
# Run with output visible
$SLOW32_HOME/bin/slow32 interactive.s32x

# Capture output
$SLOW32_HOME/bin/slow32 program.s32x > output.txt

# Discard output
$SLOW32_HOME/bin/slow32 program.s32x > /dev/null
```

## Limitations

### Current Limitations

1. **Output only** - No input mechanism
2. **Character-based** - No binary I/O
3. **No formatting** - Must convert manually
4. **No file I/O** - Console only
5. **No buffering control** - Line buffered
6. **ASCII only** - No Unicode support

### Workarounds

**Need input?**
- Embed test data in program
- Use different binaries for different inputs
- Wait for MMIO support

**Need file I/O?**
- Use shell redirection for output
- Embed data as arrays
- Wait for TRAP interface

**Need binary output?**
- Convert to hex strings
- Use memory dump from debugger
- Post-process text output

## Future Enhancements

### Phase 1: Extended MMIO (v2.1)
- Timer support
- Interrupt controller
- Additional device interfaces

### Phase 2: TRAP System Calls (v2.2)
- File operations
- Process control
- Time/date access

### Phase 3: Device Emulation (v3.0)
- Block devices
- Network interfaces
- Graphics framebuffer

## Best Practices

1. **Minimize I/O in performance code**
2. **Use helper functions for common patterns**
3. **Test with both emulators**
4. **Document I/O expectations**
5. **Plan for future interfaces**

## See Also

- [Emulators](40-emulators.md) - Emulator details
- [Examples](../examples/) - I/O examples
- [Roadmap](90-roadmap.md) - Planned features
- [Known Limitations](91-known-limitations.md) - Current constraints