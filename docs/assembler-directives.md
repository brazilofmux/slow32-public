# SLOW32 Assembler Directives and Pseudo-Instructions

## New Directives

The SLOW32 assembler now supports a comprehensive set of directives for easier assembly programming:

### Data Definition
- `.zero N` / `.space N` - Reserve N bytes of zeros
- `.long` - Alias for `.word` (emit 32-bit values)
- `.quad` - Emit 64-bit values (as two 32-bit words, little-endian)

### Section Control
- `.text` - Switch to code section
- `.data` - Switch to data section
- `.bss` - Switch to BSS (uninitialized data) section

### Symbol Definition
- `.equ symbol, value` / `.set symbol, value` - Define a constant symbol
- `.comm symbol, size[, alignment]` - Declare common symbol in BSS

## Pseudo-Instructions

These pseudo-instructions expand to one or more real SLOW32 instructions, making assembly programming more convenient:

### Basic Operations
- `li rd, imm` - Load immediate
  - Small values (-2048 to 2047): expands to `ADDI rd, r0, imm`
  - Large values: expands to `LUI rd, upper` + `ORI rd, rd, lower`
- `mv rd, rs` - Move register (expands to `ADD rd, rs, r0`)
- `nop` - No operation (expands to `ADD r0, r0, r0`)

### Control Flow
- `ret` - Return from function (expands to `JR r31`)
- `j label` - Unconditional jump (expands to `JAL r0, label`)
- `call label` - Function call (expands to `JAL r31, label`)

### Arithmetic/Logic
- `not rd, rs` - Bitwise NOT (expands to `XORI rd, rs, -1`)
- `neg rd, rs` - Negate (expands to `SUB rd, r0, rs`)
- `seqz rd, rs` - Set if zero (expands to `SEQ rd, rs, r0`)
- `snez rd, rs` - Set if not zero (expands to `SNE rd, rs, r0`)

### Address Loading
- `la rd, symbol` - Load address of symbol
  - Expands to `LUI rd, %hi(symbol)` + `ORI rd, rd, %lo(symbol)`
  - Properly handles relocations for linking

## Usage Examples

```asm
.data
message:    .asciz "Hello, World!"
buffer:     .space 256       # Reserve 256 bytes
counter:    .long 0x12345678 # 32-bit value
bignum:     .quad 0x123456789ABCDEF0  # 64-bit value

.text
.global main
main:
    # Load immediate values
    li r1, 42          # Small immediate
    li r2, 0x12345678  # Large immediate (uses LUI+ORI)
    
    # Load address of data
    la r3, message     # Load address of message string
    
    # Function call
    call print_string  # Call function (saves return in r31)
    
    # Simple operations
    mv r4, r1         # Copy r1 to r4
    not r5, r2        # Bitwise NOT of r2
    neg r6, r1        # Negate r1
    
    # Return
    li r1, 0          # Return value
    ret               # Return to caller

.bss
.comm global_buffer, 1024, 16  # 1KB buffer, 16-byte aligned

.equ MAX_SIZE, 256              # Define constant
.set MAGIC, 0xDEADBEEF         # Another way to define constant
```

## Design Philosophy

These additions make hand-written assembly more convenient while keeping the assembler simple. The pseudo-instructions handle common patterns that would otherwise require multiple instructions, reducing code size and improving readability.

For complex metaprogramming needs, high-level languages remain the preferred choice. The assembler focuses on being a straightforward tool that handles the essential features needed for system programming and compiler output.