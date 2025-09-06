# SLOW-32 ABI Reference

## Register Usage Convention

| Register | Name | Purpose | Preserved |
|----------|------|---------|-----------|
| r0 | zero | Hardwired zero | - |
| r1 | rv | Return value | No |
| r2 | t0 | Temporary | No |
| r3-r10 | a0-a7 | Function arguments | No |
| r11-r28 | s0-s17 | Saved registers | Yes |
| r29 | sp | Stack pointer | Yes |
| r30 | fp | Frame pointer | Yes |
| r31 | lr | Link register | No* |

*Link register is saved by callee if function makes calls

## Calling Convention

### Function Arguments
- **First 8 arguments**: Passed in registers r3-r10 (a0-a7)
- **Additional arguments**: Pushed on stack in reverse order (rightmost first)
- **Stack arguments**: Word-aligned, accessed at sp+0, sp+4, etc.

### Return Values
- **32-bit values**: Returned in r1
- **64-bit values**: Returned in r1:r2 (low:high)
- **Larger structures**: Caller allocates space, passes pointer in r3

### Stack Frame Layout

```
High Address
+----------------+
| Argument n     |  sp + 4*(n-8)   [if > 8 args]
| ...            |
| Argument 9     |  sp + 4
| Argument 8     |  sp + 0
+----------------+ <- SP on entry
| Return address |  sp - 4         [if saved]
| Frame pointer  |  sp - 8         [if saved]
| Saved s0-s17   |  sp - 12...     [as needed]
| Local vars     |
+----------------+ <- SP after prologue
Low Address
```

### Function Prologue/Epilogue

Standard prologue:
```asm
function:
    addi sp, sp, -frame_size    # Allocate stack frame
    stw r31, sp+offset1          # Save return address (if needed)
    stw r30, sp+offset2          # Save frame pointer (if needed)
    stw r11, sp+offset3          # Save s0 (if used)
    # ... save other used s-registers
    addi r30, sp, frame_size     # Set frame pointer (optional)
```

Standard epilogue:
```asm
    # ... restore saved registers in reverse order
    ldw r11, sp+offset3          # Restore s0
    ldw r30, sp+offset2          # Restore frame pointer
    ldw r31, sp+offset1          # Restore return address
    addi sp, sp, frame_size      # Deallocate frame
    jalr r0, r31, 0              # Return
```

### Leaf Function Optimization

Leaf functions (no calls) can skip saving r31:
```asm
leaf_function:
    # Use r31 freely - no need to save
    # ... function body ...
    jalr r0, r31, 0              # Return directly
```

## Varargs Functions

Varargs functions must:
1. Save all argument registers (r3-r10) to stack
2. Create a contiguous argument area
3. Use va_list pointer to traverse arguments

```asm
varargs_func:
    addi sp, sp, -40    # Space for r3-r10 + alignment
    stw r3, sp+0        # Save all argument registers
    stw r4, sp+4
    stw r5, sp+8
    stw r6, sp+12
    stw r7, sp+16
    stw r8, sp+20
    stw r9, sp+24
    stw r10, sp+28
    # va_list starts at sp+0
```

## 64-bit Operations

### 64-bit Addition
```asm
# Add r3:r4 + r5:r6 -> r1:r2
add r1, r3, r5      # Add low parts
sltu r2, r1, r3     # Carry = (sum < operand)
add r2, r4, r6      # Add high parts
add r2, r2, r2      # Add carry
```

### 64-bit Subtraction
```asm
# Sub r3:r4 - r5:r6 -> r1:r2
sltu r2, r3, r5     # Borrow = (minuend < subtrahend)
sub r1, r3, r5      # Subtract low parts
sub r2, r4, r6      # Subtract high parts
sub r2, r2, r2      # Subtract borrow
```

### 64-bit Shifts
The compiler generates optimized sequences for all shift amounts:
- **Shifts < 32**: Use two shifts with OR combination
- **Shifts = 32**: Simple register move
- **Shifts > 32**: Single shift of high/low part

## Structure Passing

### Small Structures (≤ 8 words)
- Passed in available argument registers
- Packed sequentially, no padding between arguments

### Large Structures
- Caller allocates space
- Passes pointer to structure in argument register
- Callee accesses via pointer

## Alignment Requirements

| Type | Size | Alignment |
|------|------|-----------|
| char | 1 | 1 |
| short | 2 | 2 |
| int | 4 | 4 |
| long | 4 | 4 |
| long long | 8 | 4 |
| pointer | 4 | 4 |
| float | 4 | 4 |
| double | 8 | 4 |

## Stack Alignment
- Stack pointer must be 4-byte aligned at all times
- Functions maintain 8-byte alignment for performance
- Leaf functions may use 4-byte alignment

## Thread-Local Storage
Not supported (single-threaded architecture)

## Dynamic Linking
Not supported (static linking only)

## Intrinsic Functions

The runtime provides these intrinsics:

| Function | Purpose | Registers |
|----------|---------|-----------|
| memcpy | Copy memory | r3=dest, r4=src, r5=size |
| memset | Fill memory | r3=dest, r4=value, r5=size |
| __muldi3 | 64-bit multiply | r3:r4 * r5:r6 -> r1:r2 |
| __divdi3 | 64-bit divide | r3:r4 / r5:r6 -> r1:r2 |
| __moddi3 | 64-bit modulo | r3:r4 % r5:r6 -> r1:r2 |

## System Calls
Currently none. Use DEBUG instruction for output.

## See Also
- [Programmer's Guide](20-programmers-guide.md) - Detailed programming information
- [Instruction Set](25-instruction-set.md) - Complete ISA reference
- [Examples](../examples/) - ABI usage examples