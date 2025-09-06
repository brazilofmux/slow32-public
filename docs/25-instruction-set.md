# SLOW-32 Instruction Set Reference

## Revision History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| TBD | TBD | - | Floating-point instructions (planned) |
| TBD | TBD | - | Full 64-bit MUL/DIV support (in progress) |
| 1.0.0 | 2025-09-05 | S. Dennis | Initial public release, partial i64 support |
| 0.9.0 | 2025-09-04 | S. Dennis | Jump tables, debug features |
| 0.8.0 | 2025-09-03 | S. Dennis | Critical fix: 32-bit constants (LUI+ADDI) |
| 0.7.0 | 2025-08-31 | S. Dennis | Varargs, memcpy/memset |
| 0.5.0 | 2025-08-28 | S. Dennis | Base ISA defined |

## Overview
This document tracks the implementation status of SLOW-32 instructions across all toolchain components:
- **Emulator**: Executes instructions
- **Assembler**: Converts assembly to machine code
- **Compiler**: Generates assembly from LLVM IR

## Status Legend
- ✅ Fully implemented and tested
- ⚠️ Partially implemented or untested
- ❌ Not implemented
- ➖ Not applicable

## Instruction Format

SLOW-32 uses 32-bit fixed-width instructions with several formats:

### R-Type (Register)
```
[31:26] opcode | [25:21] rd | [20:16] rs1 | [15:11] rs2 | [10:0] unused
```

### I-Type (Immediate)
```
[31:26] opcode | [25:21] rd | [20:16] rs1 | [15:0] imm16
```

### B-Type (Branch)
```
[31:26] opcode | [25:21] rs1 | [20:16] rs2 | [15:0] offset
```

### J-Type (Jump)
```
[31:26] opcode | [25:21] rd | [20:0] offset
```

## Core Arithmetic Instructions

| Instruction | Format | Description | Emulator | Assembler | Compiler | Notes |
|------------|--------|-------------|----------|-----------|----------|-------|
| ADD rd, rs1, rs2 | R | rd = rs1 + rs2 | ✅ | ✅ | ✅ | |
| ADDI rd, rs1, imm | I | rd = rs1 + imm | ✅ | ✅ | ✅ | imm: -2048 to 2047 |
| SUB rd, rs1, rs2 | R | rd = rs1 - rs2 | ✅ | ✅ | ✅ | |
| MUL rd, rs1, rs2 | R | rd = rs1 * rs2 | ✅ | ✅ | ✅ | 32 cycles |
| DIV rd, rs1, rs2 | R | rd = rs1 / rs2 | ✅ | ✅ | ✅ | 64 cycles, signed |
| REM rd, rs1, rs2 | R | rd = rs1 % rs2 | ✅ | ✅ | ✅ | 64 cycles, signed |

## Logical Instructions

| Instruction | Format | Description | Emulator | Assembler | Compiler | Notes |
|------------|--------|-------------|----------|-----------|----------|-------|
| AND rd, rs1, rs2 | R | rd = rs1 & rs2 | ✅ | ✅ | ✅ | |
| ANDI rd, rs1, imm | I | rd = rs1 & imm | ✅ | ✅ | ⚠️ | Compiler rarely uses |
| OR rd, rs1, rs2 | R | rd = rs1 \| rs2 | ✅ | ✅ | ✅ | |
| ORI rd, rs1, imm | I | rd = rs1 \| imm | ✅ | ✅ | ✅ | Used for large constants |
| XOR rd, rs1, rs2 | R | rd = rs1 ^ rs2 | ✅ | ✅ | ✅ | |
| XORI rd, rs1, imm | I | rd = rs1 ^ imm | ✅ | ✅ | ⚠️ | Compiler rarely uses |

## Shift Instructions

| Instruction | Format | Description | Emulator | Assembler | Compiler | Notes |
|------------|--------|-------------|----------|-----------|----------|-------|
| SLL rd, rs1, rs2 | R | rd = rs1 << rs2 | ✅ | ✅ | ✅ | Logical left shift |
| SLLI rd, rs1, imm | I | rd = rs1 << imm | ✅ | ✅ | ✅ | |
| SRL rd, rs1, rs2 | R | rd = rs1 >> rs2 | ✅ | ✅ | ✅ | Logical right shift |
| SRLI rd, rs1, imm | I | rd = rs1 >> imm | ✅ | ✅ | ⚠️ | |
| SRA rd, rs1, rs2 | R | rd = rs1 >>> rs2 | ✅ | ✅ | ✅ | Arithmetic right shift |
| SRAI rd, rs1, imm | I | rd = rs1 >>> imm | ✅ | ✅ | ⚠️ | |

## Comparison Instructions

| Instruction | Format | Description | Emulator | Assembler | Compiler | Notes |
|------------|--------|-------------|----------|-----------|----------|-------|
| SLT rd, rs1, rs2 | R | rd = (rs1 < rs2) ? 1 : 0 | ✅ | ✅ | ✅ | Signed |
| SLTI rd, rs1, imm | I | rd = (rs1 < imm) ? 1 : 0 | ✅ | ✅ | ⚠️ | Signed |
| SLTU rd, rs1, rs2 | R | rd = (rs1 < rs2) ? 1 : 0 | ✅ | ✅ | ✅ | Unsigned |
| SLTIU rd, rs1, imm | I | rd = (rs1 < imm) ? 1 : 0 | ✅ | ✅ | ⚠️ | Unsigned |
| SEQ rd, rs1, rs2 | R | rd = (rs1 == rs2) ? 1 : 0 | ✅ | ✅ | ✅ | Set equal |
| SNE rd, rs1, rs2 | R | rd = (rs1 != rs2) ? 1 : 0 | ✅ | ✅ | ✅ | Set not equal |
| SGT rd, rs1, rs2 | R | rd = (rs1 > rs2) ? 1 : 0 | ✅ | ✅ | ✅ | Signed |
| SGE rd, rs1, rs2 | R | rd = (rs1 >= rs2) ? 1 : 0 | ✅ | ✅ | ✅ | Signed |
| SGTU rd, rs1, rs2 | R | rd = (rs1 > rs2) ? 1 : 0 | ✅ | ✅ | ✅ | Unsigned |
| SGEU rd, rs1, rs2 | R | rd = (rs1 >= rs2) ? 1 : 0 | ✅ | ✅ | ✅ | Unsigned |
| SLE rd, rs1, rs2 | R | rd = (rs1 <= rs2) ? 1 : 0 | ✅ | ✅ | ✅ | Signed |
| SLEU rd, rs1, rs2 | R | rd = (rs1 <= rs2) ? 1 : 0 | ✅ | ✅ | ✅ | Unsigned |

## Branch Instructions

| Instruction | Format | Description | Emulator | Assembler | Compiler | Notes |
|------------|--------|-------------|----------|-----------|----------|-------|
| BEQ rs1, rs2, offset | B | if (rs1 == rs2) PC += offset | ✅ | ✅ | ✅ | |
| BNE rs1, rs2, offset | B | if (rs1 != rs2) PC += offset | ✅ | ✅ | ✅ | |
| BLT rs1, rs2, offset | B | if (rs1 < rs2) PC += offset | ✅ | ✅ | ⚠️ | Signed |
| BGE rs1, rs2, offset | B | if (rs1 >= rs2) PC += offset | ✅ | ✅ | ⚠️ | Signed |
| BLTU rs1, rs2, offset | B | if (rs1 < rs2) PC += offset | ✅ | ✅ | ⚠️ | Unsigned |
| BGEU rs1, rs2, offset | B | if (rs1 >= rs2) PC += offset | ✅ | ✅ | ⚠️ | Unsigned |

## Jump Instructions

| Instruction | Format | Description | Emulator | Assembler | Compiler | Notes |
|------------|--------|-------------|----------|-----------|----------|-------|
| JAL rd, offset | J | rd = PC + 4; PC = offset | ✅ | ✅ | ✅ | Function calls |
| JALR rd, rs1, imm | I | rd = PC + 4; PC = rs1 + imm | ✅ | ✅ | ✅ | Returns/indirect |

## Memory Instructions

| Instruction | Format | Description | Emulator | Assembler | Compiler | Notes |
|------------|--------|-------------|----------|-----------|----------|-------|
| LDW rd, rs1+imm | I | rd = mem[rs1 + imm] | ✅ | ✅ | ✅ | Load word (32-bit) |
| LDH rd, rs1+imm | I | rd = mem[rs1 + imm] | ✅ | ✅ | ⚠️ | Load halfword (16-bit) signed |
| LDHU rd, rs1+imm | I | rd = mem[rs1 + imm] | ✅ | ✅ | ✅ | Load halfword unsigned |
| LDB rd, rs1+imm | I | rd = mem[rs1 + imm] | ✅ | ✅ | ⚠️ | Load byte (8-bit) signed |
| LDBU rd, rs1+imm | I | rd = mem[rs1 + imm] | ✅ | ✅ | ✅ | Load byte unsigned |
| STW rs1+imm, rs2 | I | mem[rs1 + imm] = rs2 | ✅ | ✅ | ✅ | Store word |
| STH rs1+imm, rs2 | I | mem[rs1 + imm] = rs2 | ✅ | ✅ | ⚠️ | Store halfword |
| STB rs1+imm, rs2 | I | mem[rs1 + imm] = rs2 | ✅ | ✅ | ⚠️ | Store byte |

## Special Instructions

| Instruction | Format | Description | Emulator | Assembler | Compiler | Notes |
|------------|--------|-------------|----------|-----------|----------|-------|
| LUI rd, imm | I | rd = imm << 12 | ✅ | ✅ | ✅ | Load upper immediate |
| NOP | - | No operation | ✅ | ✅ | ➖ | Encoded as ADD r0, r0, r0 |
| HALT | - | Stop execution | ✅ | ✅ | ✅ | End program |
| DEBUG rs1 | - | Output character | ✅ | ✅ | ⚠️ | Outputs rs1 as char |
| YIELD | - | Waste cycles | ✅ | ✅ | ❌ | For timing |

## Pseudo-Instructions (Not Yet Implemented)

These are common pseudo-instructions that would be useful but are not yet implemented in slow32asm. Use the expanded forms directly:

| Instruction | Expansion | Description |
|------------|-----------|-------------|
| li rd, imm | lui+ori or addi | Load immediate - use LUI+ORI for large values, ADDI for small |
| mv rd, rs | add rd, rs, r0 | Move register - use ADD with r0 |
| jr rs | jalr r0, rs, 0 | Jump register - use JALR with r0 as link |
| ret | jalr r0, lr, 0 | Return from function - use JALR |

## Register Conventions

| Register | Name | Purpose | Saved |
|----------|------|---------|-------|
| r0 | zero | Always 0 | - |
| r1 | rv | Return value | No |
| r2 | t0 | Temporary | No |
| r3-r10 | a0-a7 | Arguments | No |
| r11-r28 | s0-s17 | Saved registers | Yes |
| r29 | sp | Stack pointer | Yes |
| r30 | fp | Frame pointer | Yes |
| r31 | lr | Link register | Yes |

## Implementation Notes

### Compiler Issues
1. **JALR format**: Compiler v3 tried `jalr r0, lr` but needs third parameter: `jalr r0, lr, 0`
2. **Immediate instructions**: Many immediate variants (SLTI, ANDI, etc.) not used by compiler
3. **Branch instructions**: Compiler prefers comparison + BEQ/BNE over direct branches

### Missing Features
1. **Atomic operations**: No atomic instructions for threading
2. **Floating point**: No FP support
3. **SIMD**: No vector instructions
4. **Privileged**: No system/trap instructions

### PHI-Related Issues
The compiler generates PHI moves using basic ADD instructions. Critical edge splitting is needed for correct placement.

## Memory Model

- **Code Segment**: 0x00000000 - 0x000FFFFF (1MB, execute-only)
- **Data Segment**: 0x00100000 - 0x0FFFFFFF (255MB, read/write)
- **Stack**: Starts at 0x0FFFFFF0, grows down
- **W^X Protection**: Code is execute-only, data is read/write only

## Performance Characteristics

- **Memory ops**: 3 cycles
- **Multiply**: 32 cycles
- **Divide/Rem**: 64 cycles
- **All others**: 1 cycle
- **YIELD**: Variable cycle waste

## Verification Status

### Working Examples
- Basic arithmetic
- Function calls
- String operations
- Simple loops (with PHI issues)

### Known Issues
- PHI moves placement (critical edges)
- Some immediate instructions untested
- Branch variants underutilized