# SLOW-32 Instruction Set Reference

## Overview
This document tracks the implementation status of SLOW-32 instructions across all toolchain components. SLOW-32 is a 32-bit RISC ISA inspired by RISC-V, featuring 32 general-purpose registers and fixed-width 32-bit instructions.

## Instruction Format

SLOW-32 instructions are 32 bits wide, little-endian. The opcode is always in bits [6:0].

### R-Type (Register)
Used for register-register arithmetic, logical, and comparison operations.
```
[31:25] funct7 | [24:20] rs2 | [19:15] rs1 | [14:12] funct3 | [11:7] rd | [6:0] opcode
```
*Note: In the current implementation, funct3 and funct7 are largely unused as the 7-bit opcode uniquely identifies the instruction.*

### I-Type (Immediate)
Used for register-immediate operations, loads, and JALR.
```
[31:20] imm[11:0] | [19:15] rs1 | [14:12] funct3 | [11:7] rd | [6:0] opcode
```

### S-Type (Store)
Used for store instructions.
```
[31:25] imm[11:5] | [24:20] rs2 | [19:15] rs1 | [14:12] funct3 | [11:7] imm[4:0] | [6:0] opcode
```

### B-Type (Branch)
Used for conditional branch instructions. Immediates are encoded with a 13-bit signed offset (multiples of 2).
```
[31] imm[12] | [30:25] imm[10:5] | [24:20] rs2 | [19:15] rs1 | [14:12] funct3 | [11:8] imm[4:1] | [7] imm[11] | [6:0] opcode
```

### U-Type (Upper Immediate)
Used for LUI.
```
[31:12] imm[31:12] | [11:7] rd | [6:0] opcode
```

### J-Type (Jump)
Used for JAL. Immediates are encoded with a 21-bit signed offset (multiples of 2).
```
[31] imm[20] | [30:21] imm[10:1] | [20] imm[11] | [19:12] imm[19:12] | [11:7] rd | [6:0] opcode
```

---

## Core Arithmetic Instructions

| Instruction | Opcode | Format | Description | Status | Notes |
|------------|--------|--------|-------------|--------|-------|
| ADD rd, rs1, rs2 | 0x00 | R | rd = rs1 + rs2 | ✅ | |
| SUB rd, rs1, rs2 | 0x01 | R | rd = rs1 - rs2 | ✅ | |
| MUL rd, rs1, rs2 | 0x0A | R | rd = rs1 * rs2 | ✅ | 32 cycles |
| MULH rd, rs1, rs2| 0x0B | R | rd = (rs1 * rs2) >> 32 | ✅ | Unsigned high part |
| DIV rd, rs1, rs2 | 0x0C | R | rd = rs1 / rs2 | ✅ | 64 cycles, signed |
| REM rd, rs1, rs2 | 0x0D | R | rd = rs1 % rs2 | ✅ | 64 cycles, signed |
| ADDI rd, rs1, imm| 0x10 | I | rd = rs1 + imm | ✅ | 12-bit signed imm |

## Logical Instructions

| Instruction | Opcode | Format | Description | Status | Notes |
|------------|--------|--------|-------------|--------|-------|
| XOR rd, rs1, rs2 | 0x02 | R | rd = rs1 ^ rs2 | ✅ | |
| OR rd, rs1, rs2  | 0x03 | R | rd = rs1 \| rs2 | ✅ | |
| AND rd, rs1, rs2 | 0x04 | R | rd = rs1 & rs2 | ✅ | |
| ORI rd, rs1, imm | 0x11 | I | rd = rs1 \| imm | ✅ | |
| ANDI rd, rs1, imm| 0x12 | I | rd = rs1 & imm | ✅ | |
| XORI rd, rs1, imm| 0x1E | I | rd = rs1 ^ imm | ✅ | |

## Shift Instructions

| Instruction | Opcode | Format | Description | Status | Notes |
|------------|--------|--------|-------------|--------|-------|
| SLL rd, rs1, rs2 | 0x05 | R | rd = rs1 << rs2 | ✅ | Logical left shift |
| SRL rd, rs1, rs2 | 0x06 | R | rd = rs1 >> rs2 | ✅ | Logical right shift |
| SRA rd, rs1, rs2 | 0x07 | R | rd = rs1 >>> rs2 | ✅ | Arithmetic right shift |
| SLLI rd, rs1, imm| 0x13 | I | rd = rs1 << imm | ✅ | |
| SRLI rd, rs1, imm| 0x14 | I | rd = rs1 >> imm | ✅ | |
| SRAI rd, rs1, imm| 0x15 | I | rd = rs1 >>> imm | ✅ | |

## Comparison Instructions

| Instruction | Opcode | Format | Description | Status | Notes |
|------------|--------|--------|-------------|--------|-------|
| SLT rd, rs1, rs2 | 0x08 | R | rd = (rs1 < rs2) ? 1 : 0 | ✅ | Signed |
| SLTU rd, rs1, rs2| 0x09 | R | rd = (rs1 < rs2) ? 1 : 0 | ✅ | Unsigned |
| SEQ rd, rs1, rs2 | 0x0E | R | rd = (rs1 == rs2) ? 1 : 0 | ✅ | Set equal |
| SNE rd, rs1, rs2 | 0x0F | R | rd = (rs1 != rs2) ? 1 : 0 | ✅ | Set not equal |
| SLTI rd, rs1, imm| 0x16 | I | rd = (rs1 < imm) ? 1 : 0 | ✅ | Signed |
| SLTIU rd, rs1, imm| 0x17 | I | rd = (rs1 < imm) ? 1 : 0 | ✅ | Unsigned |
| SGT rd, rs1, rs2 | 0x18 | R | rd = (rs1 > rs2) ? 1 : 0 | ✅ | Signed |
| SGTU rd, rs1, rs2| 0x19 | R | rd = (rs1 > rs2) ? 1 : 0 | ✅ | Unsigned |
| SLE rd, rs1, rs2 | 0x1A | R | rd = (rs1 <= rs2) ? 1 : 0 | ✅ | Signed |
| SLEU rd, rs1, rs2| 0x1B | R | rd = (rs1 <= rs2) ? 1 : 0 | ✅ | Unsigned |
| SGE rd, rs1, rs2 | 0x1C | R | rd = (rs1 >= rs2) ? 1 : 0 | ✅ | Signed |
| SGEU rd, rs1, rs2| 0x1D | R | rd = (rs1 >= rs2) ? 1 : 0 | ✅ | Unsigned |

## Branch Instructions

| Instruction | Opcode | Format | Description | Status | Notes |
|------------|--------|--------|-------------|--------|-------|
| BEQ rs1, rs2, imm | 0x48 | B | if (rs1 == rs2) PC += imm | ✅ | |
| BNE rs1, rs2, imm | 0x49 | B | if (rs1 != rs2) PC += imm | ✅ | |
| BLT rs1, rs2, imm | 0x4A | B | if (rs1 < rs2) PC += imm | ✅ | Signed |
| BGE rs1, rs2, imm | 0x4B | B | if (rs1 >= rs2) PC += imm | ✅ | Signed |
| BLTU rs1, rs2, imm| 0x4C | B | if (rs1 < rs2) PC += imm | ✅ | Unsigned |
| BGEU rs1, rs2, imm| 0x4D | B | if (rs1 >= rs2) PC += imm | ✅ | Unsigned |

## Jump Instructions

| Instruction | Opcode | Format | Description | Status | Notes |
|------------|--------|--------|-------------|--------|-------|
| JAL rd, imm      | 0x40 | J | rd = PC + 4; PC += imm | ✅ | |
| JALR rd, rs1, imm | 0x41 | I | rd = PC + 4; PC = rs1 + imm | ✅ | |

## Memory Instructions

| Instruction | Opcode | Format | Description | Status | Notes |
|------------|--------|--------|-------------|--------|-------|
| LDB rd, rs1+imm  | 0x30 | I | rd = sext(mem8[rs1+imm]) | ✅ | |
| LDH rd, rs1+imm  | 0x31 | I | rd = sext(mem16[rs1+imm]) | ✅ | |
| LDW rd, rs1+imm  | 0x32 | I | rd = mem32[rs1+imm] | ✅ | |
| LDBU rd, rs1+imm | 0x33 | I | rd = zext(mem8[rs1+imm]) | ✅ | |
| LDHU rd, rs1+imm | 0x34 | I | rd = zext(mem16[rs1+imm]) | ✅ | |
| STB rs1+imm, rs2 | 0x38 | S | mem8[rs1+imm] = rs2 | ✅ | |
| STH rs1+imm, rs2 | 0x39 | S | mem16[rs1+imm] = rs2 | ✅ | |
| STW rs1+imm, rs2 | 0x3A | S | mem32[rs1+imm] = rs2 | ✅ | |

## Special Instructions

| Instruction | Opcode | Format | Description | Status | Notes |
|------------|--------|--------|-------------|--------|-------|
| LUI rd, imm   | 0x20 | U | rd = imm << 12 | ✅ | |
| ASSERT_EQ rs1, rs2 | 0x3F | R | Trap if rs1 != rs2 | ✅ | Testing only |
| NOP           | 0x50 | - | No operation | ✅ | |
| YIELD         | 0x51 | R | Host interface yield | ✅ | |
| DEBUG rs1     | 0x52 | R | Output char in rs1 | ✅ | |
| HALT          | 0x7F | - | Stop execution | ✅ | |

---

## Pseudo-Instructions

The SLOW-32 assembler supports several pseudo-instructions that expand into one or more native instructions. All immediate operands in pseudo-instructions (and native instructions) support complex constant expressions (e.g., `(10 * 4) + 5`).

| Instruction | Expansion | Description |
|------------|-----------|-------------|
| li rd, imm | addi or lui+ori | Load 32-bit immediate |
| la rd, sym | lui + ori | Load address of symbol |
| mv rd, rs  | add rd, rs, r0 | Move register |
| not rd, rs | xori rd, rs, -1 | Bitwise NOT |
| neg rd, rs | sub rd, r0, rs | Negate |
| seqz rd, rs| seq rd, rs, r0 | Set if equal to zero |
| snez rd, rs| sne rd, rs, r0 | Set if not equal to zero |
| j imm/lbl  | jal r0, imm | Unconditional jump |
| jal lbl    | jal r31, lbl | Jump and link (call) shorthand |
| ret        | jalr r0, lr, 0 | Return from function |
| call lbl   | jal r31, lbl | Call function |
| nop        | add r0, r0, r0 | No operation |
| bgt rs1, rs2, imm | blt rs2, rs1, imm | Branch if greater than |
| ble rs1, rs2, imm | bge rs2, rs1, imm | Branch if less or equal |
| bgtu rs1, rs2, imm| bltu rs2, rs1, imm| Branch if greater than (U) |
| bleu rs1, rs2, imm| bgeu rs2, rs1, imm| Branch if less or equal (U) |

---

## Assembler Directives

The SLOW-32 assembler recognizes a small set of GNU-ish directives for section
selection, data definition, and symbol control.

| Directive | Description |
|-----------|-------------|
| .text / .code | Switch to code section |
| .data | Switch to initialized data section |
| .bss | Switch to BSS section |
| .rodata | Switch to read-only data section |
| .section <name> | Honors .rodata* and .init_array; others ignored |
| .global / .globl <sym> | Mark symbol global |
| .word <expr>[, ...] | Emit 32-bit words (supports symbol relocations) |
| .half <expr>[, ...] | Emit 16-bit halfwords |
| .byte <expr|string>[, ...] | Emit bytes or string literals |
| .string / .asciz "..." | Emit NUL-terminated strings |
| .ascii "..." | Emit non-terminated strings |
| .zero / .space <n> | Emit n zero bytes |
| .long <expr>[, ...] | Alias for .word |
| .quad <expr>[, ...] | Emit 64-bit values (little-endian pair of words) |
| .align <pow2> | Align to 2^pow2 boundary (default 2 = 4 bytes) |
| .equ / .set <sym>, <expr> | Define absolute symbol constant |
| .comm <sym>, <size>[, <align>] | Reserve common symbol in BSS |

---

## Register Conventions (ABI)

| Register | Name | Alias | Purpose | Saved |
|----------|------|-------|---------|-------|
| r0 | zero | - | Hardwired Zero | - |
| r1 | rv | - | Return Value | No |
| r2 | t0 | - | Temporary | No |
| r3-r10 | a0-a7 | - | Function Arguments | No |
| r11-r28 | s0-s17 | - | Saved Registers | Yes |
| r29 | sp | - | Stack Pointer | Yes |
| r30 | fp | - | Frame Pointer | Yes |
| r31 | lr | - | Link Register | Yes |

*Note: Assembler supports `zero`, `sp`, `fp`, and `lr` aliases. The compiler uses full ABI aliases.*

---

## Memory Model

- **Code**: `0x00000000 - 0x000FFFFF` (1MB, Execute-only)
- **Data**: `0x00100000 - 0x0FFFFFFF` (255MB, Read/Write)
- **MMIO**: `0x10000000+`
- **Stack**: Starts at `0x0FFFFFF0`, grows downward.
- **W^X Protection**: Enforced via memory manager.

## Performance

- **Standard**: 1 cycle per instruction.
- **Memory**: 3 cycles.
- **Multiply**: 32 cycles.
- **Divide/Rem**: 64 cycles.
