# MMIO Console I/O

The SLOW-32 MMIO system provides console I/O capabilities through a ring buffer interface, replacing the limited DEBUG instruction with full input/output functionality.

## Overview

Console I/O is implemented via MMIO ring buffers at address `0x10000000`. Programs must be linked with the `--mmio` flag to enable MMIO support.

## Memory Map

| Address Range | Description |
|--------------|-------------|
| 0x10000000 | Request queue head (guest writes) |
| 0x10000004 | Request queue tail (host updates) |
| 0x10001000 | Request ring buffer (256 descriptors) |
| 0x10002000 | Response queue head (host writes) |
| 0x10002004 | Response queue tail (guest updates) |
| 0x10003000 | Response ring buffer (256 descriptors) |
| 0x10004000 | Data buffer (56KB) |

## I/O Operations

### Supported Operations
- `IO_OP_PUTCHAR` (0x01) - Output single character
- `IO_OP_GETCHAR` (0x02) - Input single character
- `IO_OP_WRITE` (0x03) - Write buffer to file descriptor
- `IO_OP_READ` (0x04) - Read from file descriptor
- `IO_OP_FLUSH` (0x0B) - Flush output buffers

### Descriptor Format
Each descriptor is 16 bytes:
```c
typedef struct {
    uint32_t opcode;    // Operation type
    uint32_t length;    // Data length
    uint32_t offset;    // Offset in data buffer
    uint32_t status;    // fd for requests, result for responses
} io_descriptor_t;
```

## Usage Example

### Assembly (Direct MMIO)
```assembly
# Output 'H' using MMIO
LUI r5, 0x10000     # MMIO base
LUI r7, 0x10004     # Data buffer base
ADDI r6, r0, 72     # 'H'
STW r7, r6, 0       # Store character in data buffer

# Build descriptor
LUI r8, 0x10001     # Request ring base
ADDI r9, r0, 1      # IO_OP_PUTCHAR
STW r8, r9, 0       # opcode
ADDI r9, r0, 1
STW r8, r9, 4       # length = 1
STW r8, r0, 8       # offset = 0
STW r8, r0, 12      # status = 0

# Update request head
ADDI r6, r0, 1
STW r5, r6, 0       # REQ_HEAD = 1

# Process MMIO
YIELD r0, r0, r0    # Trigger processing
```

### Using Console I/O Library
A console I/O library (`runtime/console_io.s`) provides convenient functions:

```assembly
.text
.global _start

_start:
    # Print string
    LUI r3, %hi(message)
    ADDI r3, r3, %lo(message)
    JAL puts            # Print with newline
    
    # Print single character
    ADDI r3, r0, 65     # 'A'
    JAL putchar
    
    # Read character
    JAL getchar
    # r1 = character or -1 for EOF
    
    HALT

.data
message:
    .asciz "Hello, MMIO World!"
```

## Building and Linking

### Assemble Console I/O Library
```bash
./assembler/slow32asm runtime/console_io.s runtime/console_io.o23s
```

### Link with MMIO Support
```bash
# Link with --mmio flag to enable MMIO
./linker/s32-ld --mmio 64k -o program.s32x program.o23s console_io.o23s
```

### Without MMIO
Programs linked without `--mmio` will get memory protection faults when accessing MMIO addresses.

## Implementation Details

### Processing Model
- MMIO operations are processed only on YIELD instructions
- Guest writes requests to ring buffer
- YIELD triggers host processing
- Host processes all pending requests
- Responses are written back to response ring

### Ring Buffer Management
- Ring size: 256 descriptors
- Head incremented by producer
- Tail incremented by consumer
- Full/empty detection via head/tail comparison

### Data Buffer
- 56KB shared data area
- Offsets specified in descriptors
- Used for both input and output data

## Performance Considerations

- Batch multiple operations before YIELD for efficiency
- YIELD has minimal overhead when no requests pending
- Ring buffers avoid memory copies
- Direct memory mapping for zero-copy I/O

## Migration from DEBUG Instruction

Replace DEBUG instruction usage:

**Before (DEBUG):**
```assembly
ADDI r3, r0, 65  # 'A'
DEBUG r3, r0, r0
```

**After (MMIO):**
```assembly
ADDI r3, r0, 65  # 'A'
JAL putchar      # Using console_io library
```

## Example Programs

- `tests/test_mmio_putchar.s` - Basic MMIO putchar test
- `tests/test_console_hello.s` - Hello world using console I/O library
- `runtime/console_io.s` - Console I/O function library

## Notes

- MMIO must be enabled at link time with `--mmio` flag
- YIELD instruction is required to process I/O operations
- Console I/O is unbuffered (immediate output)
- Input is line-buffered (waits for newline)