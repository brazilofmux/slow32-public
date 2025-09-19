# SLOW-32 MMIO Device Framework Design

## Overview
Replace the DEBUG instruction hack with proper memory-mapped I/O devices, enabling real stdin/stdout/stderr and eventually files.

## Memory Map
```
0x10000000 - 0x10000FFF : System Control Block
0x10001000 - 0x10001FFF : Console Device  
0x10002000 - 0x10002FFF : Timer Device (future)
0x10003000 - 0x10003FFF : Block Device (future)
```

## Console Device (Priority 1)

### Registers (at 0x10001000)
```c
0x00: CONSOLE_STATUS   // bit 0: TX ready, bit 1: RX available
0x04: CONSOLE_DATA     // Read: get char, Write: put char  
0x08: CONSOLE_CONTROL  // bit 0: enable interrupts (future)
```

### Operation
- **Write**: Check TX ready bit, write to DATA register
- **Read**: Check RX available bit, read from DATA register
- **Polling mode** initially (no interrupts)

### Emulator Implementation
```c
// In memory_manager.c or new mmio.c
typedef struct {
    uint32_t status;
    uint32_t data;
    uint32_t control;
} console_device_t;

// Hook into mm_read/mm_write
if (addr >= MMIO_BASE && addr < MMIO_BASE + mmio_size) {
    return handle_mmio(addr, value, is_write);
}
```

## System Calls via MMIO

### System Call Interface (at 0x10000000)
```c
0x00: SYSCALL_NUM      // Write syscall number
0x04: SYSCALL_ARG1     // First argument
0x08: SYSCALL_ARG2     // Second argument
0x0C: SYSCALL_ARG3     // Third argument
0x10: SYSCALL_RESULT   // Read result
0x14: SYSCALL_ERRNO    // Read errno
0x18: SYSCALL_TRIGGER  // Write any value to execute syscall
```

### Syscall Numbers
```c
#define SYS_EXIT   0
#define SYS_OPEN   1
#define SYS_CLOSE  2
#define SYS_READ   3
#define SYS_WRITE  4
#define SYS_LSEEK  5
#define SYS_BRK    6  // For malloc
```

## Implementation Plan

### Phase 1: Console Device
1. Add MMIO region to memory_manager
2. Implement console device registers
3. Create `putchar()` and `getchar()` using MMIO
4. Replace DEBUG instruction usage with console writes

### Phase 2: System Calls
1. Implement syscall interface via MMIO
2. Add write() system call first (for printf)
3. Add read() for input
4. Add brk() for malloc

### Phase 3: stdio Library
1. Implement proper FILE structure
2. Build fopen/fclose/fread/fwrite on syscalls
3. Update printf to use FILE* stdout
4. Add scanf, gets, puts

## Benefits
- **Real I/O**: Not just debug hacks
- **Extensible**: Easy to add devices
- **Standard**: Follows typical embedded patterns
- **Foundation**: Enables real stdlib

## Example Usage

### Assembly
```asm
    # Write 'A' to console
    lui r1, 0x10001          # Console base
    lw r2, 0(r1)            # Read status
    andi r2, r2, 1          # Check TX ready
    beq r2, r0, -8          # Wait if not ready
    addi r3, r0, 65         # 'A'
    sw r3, 4(r1)            # Write to data register
```

### C (with new stdio)
```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n");  // Uses MMIO console
    
    char name[100];
    printf("Enter name: ");
    scanf("%s", name);          // Real input!
    printf("Hello, %s!\n", name);
    
    return 0;
}
```

## Testing Strategy
1. Start with simple putchar via MMIO
2. Verify DEBUG instruction still works (compatibility)
3. Build up to full printf
4. Add input capabilities
5. Test with interactive programs

This transforms SLOW-32 from a toy to a real system!