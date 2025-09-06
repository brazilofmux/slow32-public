# SLOW-32 Relocations Reference

## Overview

Relocations allow the assembler to generate object files with symbolic references that the linker resolves to final addresses. SLOW-32 supports four relocation types designed for simplicity and efficiency.

## Relocation Types

### R_SLOW32_ABS32 (Type 1)
**Purpose:** 32-bit absolute address relocation  
**Usage:** Global variables, function addresses, jump tables  
**Format:** Replace 32-bit field with symbol's absolute address  

```asm
    .section .data
addr:
    .word function_name    # Generates R_SLOW32_ABS32 relocation

    .section .text
    ldw r3, addr           # Load function address
    jalr r31, r3, 0        # Call via register
```

### R_SLOW32_PC32 (Type 2)
**Purpose:** 32-bit PC-relative relocation  
**Usage:** Branches and calls within ±2GB range  
**Format:** Replace with (symbol - PC)  

```asm
    jal r31, function      # Generates R_SLOW32_PC32
    beq r3, r4, label      # Generates R_SLOW32_PC32
```

### R_SLOW32_HI20 (Type 3)
**Purpose:** Upper 20 bits of 32-bit address  
**Usage:** First half of LUI+ADDI constant materialization  
**Format:** Adjusted for sign-extension of paired LO12  

```asm
    lui r3, %hi(symbol)    # Generates R_SLOW32_HI20
```

**Critical:** The HI20 value is adjusted when bit 11 of the address is 1:
- If bit 11 = 1, HI20 = (address >> 12) + 1
- If bit 11 = 0, HI20 = (address >> 12)

This compensates for ADDI's sign-extension of the LO12 value.

### R_SLOW32_LO12 (Type 4)
**Purpose:** Lower 12 bits of 32-bit address  
**Usage:** Second half of LUI+ADDI constant materialization  
**Format:** Sign-extended 12-bit immediate  

```asm
    addi r3, r3, %lo(symbol)  # Generates R_SLOW32_LO12
```

## Usage Patterns

### Loading 32-bit Address

```asm
# Load address of global variable
    lui r3, %hi(global_var)       # R_SLOW32_HI20
    addi r3, r3, %lo(global_var)  # R_SLOW32_LO12
    ldw r4, r3+0                   # Load value
```

### Direct Function Call

```asm
# Near call (within ±2GB)
    jal r31, function              # R_SLOW32_PC32
```

### Indirect Function Call

```asm
# Far call or computed call
    lui r3, %hi(function)          # R_SLOW32_HI20
    addi r3, r3, %lo(function)     # R_SLOW32_LO12
    jalr r31, r3, 0
```

### Jump Table

```asm
    .section .rodata
jump_table:
    .word case_0                   # R_SLOW32_ABS32
    .word case_1                   # R_SLOW32_ABS32
    .word case_2                   # R_SLOW32_ABS32

    .section .text
    # r3 contains index
    slli r3, r3, 2                 # Index * 4
    lui r4, %hi(jump_table)        # R_SLOW32_HI20
    addi r4, r4, %lo(jump_table)   # R_SLOW32_LO12
    add r4, r4, r3
    ldw r4, r4+0
    jalr r0, r4, 0                 # Jump to case
```

## Object File Format

Relocations in .s32o files:

```
Relocation Entry (16 bytes):
+0x00: uint32_t offset      # Offset in section
+0x04: uint32_t type        # Relocation type (1-4)
+0x08: uint32_t symbol      # Symbol index
+0x0C: uint32_t addend      # Constant to add
```

## Linker Processing

The linker processes relocations in this order:

1. **Symbol Resolution**: Assign addresses to all symbols
2. **Relocation Application**: For each relocation:
   - Read the relocation entry
   - Calculate the final value based on type
   - Write the value to the specified offset

### Calculation Formulas

```c
switch (relocation.type) {
    case R_SLOW32_ABS32:
        value = symbol_addr + addend;
        write32(offset, value);
        break;
        
    case R_SLOW32_PC32:
        value = symbol_addr - offset + addend;
        write32(offset, value);
        break;
        
    case R_SLOW32_HI20:
        addr = symbol_addr + addend;
        if (addr & 0x800) addr += 0x1000;  // Adjust for sign
        value = (addr >> 12) & 0xFFFFF;
        write20(offset, value);
        break;
        
    case R_SLOW32_LO12:
        addr = symbol_addr + addend;
        value = addr & 0xFFF;
        if (value & 0x800) value |= 0xFFFFF000;  // Sign extend
        write12(offset, value);
        break;
}
```

## Limitations

1. **No Dynamic Relocations**: All relocations resolved at link time
2. **No Thread-Local**: No TLS relocations
3. **No PLT/GOT**: No dynamic linking support
4. **32-bit Only**: No 64-bit address relocations

## Assembly Directives

### Symbol Definition
```asm
.globl symbol      # Make symbol visible to linker
.local symbol      # Keep symbol local to file
symbol:            # Define symbol at current address
```

### Relocation Generation
```asm
.word symbol       # Generate R_SLOW32_ABS32
.word symbol-.     # PC-relative (assembler may optimize)
lui r, %hi(sym)    # Generate R_SLOW32_HI20
addi r, r, %lo(sym) # Generate R_SLOW32_LO12
```

## Examples

### External Function Call
```asm
    .section .text
    .globl main
main:
    # Call external printf
    lui r3, %hi(printf)
    addi r3, r3, %lo(printf)
    lui r4, %hi(format_string)
    addi r4, r4, %lo(format_string)
    jalr r31, r3, 0
    
    .section .rodata
format_string:
    .asciz "Hello, World!\n"
```

### Global Variable Access
```asm
    .section .data
    .globl counter
counter:
    .word 0
    
    .section .text
increment:
    lui r3, %hi(counter)
    addi r3, r3, %lo(counter)
    ldw r4, r3+0
    addi r4, r4, 1
    stw r4, r3+0
    jalr r0, r31, 0
```

## Troubleshooting

### Common Issues

**Problem**: "Undefined symbol" error  
**Solution**: Ensure symbol is marked `.globl` in defining file

**Problem**: Wrong address after relocation  
**Solution**: Check HI20/LO12 adjustment for sign extension

**Problem**: "Relocation truncated to fit"  
**Solution**: Value too large for relocation type

## See Also

- [Instruction Set](25-instruction-set.md) - Constant materialization
- [Toolchain Overview](15-toolchain-overview.md) - Assembler and linker
- [Object Format](30-object-format.md) - File structure details