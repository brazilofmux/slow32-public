# SLOW-32 ISA Encoding Quick Reference

This document covers the instruction encoding needed for hand-assembly of SLOW-32 programs. All instructions are 32 bits wide, little-endian.

Stage00 decoder model (matches `selfhost/v2/stage00/s32-emu.c`):
- Opcode is always bits `[6:0]` and uniquely selects the instruction.
- `funct3`/`funct7` bit positions exist in the bit layout, but are not used to distinguish operations in the selfhost ISA subset.

## Register Map

| Reg | ABI Name | Purpose | Caller-Saved |
|-----|----------|---------|:------------:|
| r0 | zero | Hardwired zero | - |
| r1 | rv | Return value | Yes |
| r2 | t0 | Temporary | Yes |
| r3-r10 | a0-a7 | Arguments | Yes |
| r11-r28 | s0-s17 | Saved registers | No |
| r29 | sp | Stack pointer | No |
| r30 | fp | Frame pointer | No |
| r31 | lr | Link register | No |

## Instruction Formats

### R-Type (Register-Register)

```
 31       25 24   20 19   15 14  12 11    7 6     0
┌──────────┬───────┬───────┬──────┬───────┬───────┐
│  funct7  │  rs2  │  rs1  │  f3  │  rd   │opcode │
└──────────┴───────┴───────┴──────┴───────┴───────┘
     7         5       5      3       5       7
```

Encoding: `opcode | (rd << 7) | (rs1 << 15) | (rs2 << 20)`

### I-Type (Immediate)

```
 31                20 19   15 14  12 11    7 6     0
┌───────────────────┬───────┬──────┬───────┬───────┐
│    imm[11:0]      │  rs1  │  f3  │  rd   │opcode │
└───────────────────┴───────┴──────┴───────┴───────┘
        12              5      3       5       7
```

Encoding: `opcode | (rd << 7) | (rs1 << 15) | ((imm & 0xFFF) << 20)`

### S-Type (Store)

```
 31       25 24   20 19   15 14  12 11    7 6     0
┌──────────┬───────┬───────┬──────┬───────┬───────┐
│imm[11:5] │  rs2  │  rs1  │  f3  │im[4:0]│opcode │
└──────────┴───────┴───────┴──────┴───────┴───────┘
     7         5       5      3       5       7
```

Encoding:
```
opcode | ((imm & 0x1F) << 7) | (rs1 << 15) | (rs2 << 20) | (((imm >> 5) & 0x7F) << 25)
```

Note: `rs1` = base address, `rs2` = value to store.

### B-Type (Branch)

```
 31  30     25 24   20 19   15 14  12 11   8  7  6     0
┌──┬────────┬───────┬───────┬──────┬──────┬──┬───────┐
│12│ 10:5   │  rs2  │  rs1  │  f3  │ 4:1  │11│opcode │
└──┴────────┴───────┴───────┴──────┴──────┴──┴───────┘
 1     6        5       5      3      4     1    7
```

Encoding:
```
opcode
| (((imm >> 11) & 1) << 7)
| (((imm >> 1) & 0xF) << 8)
| (rs1 << 15)
| (rs2 << 20)
| (((imm >> 5) & 0x3F) << 25)
| (((imm >> 12) & 1) << 31)
```

The offset is PC-relative and in units of bytes (not halfwords). Range: -4096 to +4094.

### U-Type (Upper Immediate)

```
 31                      12 11    7 6     0
┌─────────────────────────┬───────┬───────┐
│       imm[31:12]        │  rd   │opcode │
└─────────────────────────┴───────┴───────┘
           20                 5       7
```

Encoding: `opcode | (rd << 7) | ((imm & 0xFFFFF) << 12)`

### J-Type (Jump)

```
 31  30       21 20  19       12 11    7 6     0
┌──┬──────────┬──┬────────────┬───────┬───────┐
│20│  10:1    │11│   19:12    │  rd   │opcode │
└──┴──────────┴──┴────────────┴───────┴───────┘
 1     10      1       8         5       7
```

Encoding:
```
opcode
| (rd << 7)
| (((imm >> 12) & 0xFF) << 12)
| (((imm >> 11) & 1) << 20)
| (((imm >> 1) & 0x3FF) << 21)
| (((imm >> 20) & 1) << 31)
```

Range: -1048576 to +1048574 (PC-relative, byte units).

## Bootstrap Core Instruction Set (34 Instructions)

These are sufficient for a complete bootstrap toolchain. The Stage00 emulator implements additional opcodes beyond this core (listed below).

### Arithmetic (R-Type)

| Mnemonic | Opcode | Operation |
|----------|:------:|-----------|
| `ADD rd, rs1, rs2` | 0x00 | rd = rs1 + rs2 |
| `SUB rd, rs1, rs2` | 0x01 | rd = rs1 - rs2 |
| `MUL rd, rs1, rs2` | 0x0A | rd = (rs1 * rs2) & 0xFFFFFFFF |

### Logical (R-Type)

| Mnemonic | Opcode | Operation |
|----------|:------:|-----------|
| `XOR rd, rs1, rs2` | 0x02 | rd = rs1 ^ rs2 |
| `OR rd, rs1, rs2` | 0x03 | rd = rs1 \| rs2 |
| `AND rd, rs1, rs2` | 0x04 | rd = rs1 & rs2 |
| `SLL rd, rs1, rs2` | 0x05 | rd = rs1 << (rs2 & 31) |
| `SRL rd, rs1, rs2` | 0x06 | rd = rs1 >>> (rs2 & 31) |
| `SRA rd, rs1, rs2` | 0x07 | rd = rs1 >> (rs2 & 31) (arithmetic) |

### Comparison (R-Type)

| Mnemonic | Opcode | Operation |
|----------|:------:|-----------|
| `SLT rd, rs1, rs2` | 0x08 | rd = (rs1 < rs2) ? 1 : 0 (signed) |
| `SLTU rd, rs1, rs2` | 0x09 | rd = (rs1 < rs2) ? 1 : 0 (unsigned) |
| `SEQ rd, rs1, rs2` | 0x0E | rd = (rs1 == rs2) ? 1 : 0 |
| `SNE rd, rs1, rs2` | 0x0F | rd = (rs1 != rs2) ? 1 : 0 |
| `SGT rd, rs1, rs2` | 0x18 | rd = (rs1 > rs2) ? 1 : 0 (signed) |
| `SGTU rd, rs1, rs2` | 0x19 | rd = (rs1 > rs2) ? 1 : 0 (unsigned) |

### Immediate (I-Type)

| Mnemonic | Opcode | Immediate | Operation |
|----------|:------:|:---------:|-----------|
| `ADDI rd, rs1, imm` | 0x10 | Sign-ext 12-bit | rd = rs1 + sext(imm) |
| `ORI rd, rs1, imm` | 0x11 | Zero-ext 12-bit | rd = rs1 \| zext(imm) |
| `ANDI rd, rs1, imm` | 0x12 | Zero-ext 12-bit | rd = rs1 & zext(imm) |
| `SLLI rd, rs1, imm` | 0x13 | Zero-ext (shamt) | rd = rs1 << imm[4:0] |
| `SRLI rd, rs1, imm` | 0x14 | Zero-ext (shamt) | rd = rs1 >>> imm[4:0] |
| `SRAI rd, rs1, imm` | 0x15 | Zero-ext (shamt) | rd = rs1 >> imm[4:0] |
| `XORI rd, rs1, imm` | 0x1E | Zero-ext 12-bit | rd = rs1 ^ zext(imm) |

### Upper Immediate (U-Type)

| Mnemonic | Opcode | Operation |
|----------|:------:|-----------|
| `LUI rd, imm` | 0x20 | rd = imm << 12 |

### Memory (I-Type loads, S-Type stores)

| Mnemonic | Opcode | Format | Operation |
|----------|:------:|:------:|-----------|
| `LDB rd, rs1, imm` | 0x30 | I | rd = sext(mem8[rs1 + sext(imm)]) |
| `LDW rd, rs1, imm` | 0x32 | I | rd = mem32[rs1 + sext(imm)] |
| `LDBU rd, rs1, imm` | 0x33 | I | rd = zext(mem8[rs1 + sext(imm)]) |
| `STB rs1, rs2, imm` | 0x38 | S | mem8[rs1 + sext(imm)] = rs2[7:0] |
| `STW rs1, rs2, imm` | 0x3A | S | mem32[rs1 + sext(imm)] = rs2 |

### Control Flow

| Mnemonic | Opcode | Format | Operation |
|----------|:------:|:------:|-----------|
| `JAL rd, offset` | 0x40 | J | rd = PC + 4; PC += offset |
| `JALR rd, rs1, imm` | 0x41 | I | rd = PC + 4; PC = rs1 + sext(imm) |
| `BEQ rs1, rs2, offset` | 0x48 | B | if (rs1 == rs2) PC += offset |
| `BNE rs1, rs2, offset` | 0x49 | B | if (rs1 != rs2) PC += offset |
| `BLT rs1, rs2, offset` | 0x4A | B | if (rs1 < rs2) PC += offset (signed) |
| `BGE rs1, rs2, offset` | 0x4B | B | if (rs1 >= rs2) PC += offset (signed) |

### System

| Mnemonic | Opcode | Format | Operation |
|----------|:------:|:------:|-----------|
| `DEBUG rs1` | 0x52 | R | Output character in rs1 |
| `HALT` | 0x7F | - | Stop execution |

## Additional Opcodes Implemented By Stage00 Emulator

These opcodes are implemented in `selfhost/v2/stage00/s32-emu.c` even though they are not required for the minimal 34-instruction bootstrap core.

### Arithmetic / Comparison Extensions

| Mnemonic | Opcode | Operation |
|----------|:------:|-----------|
| `MULH rd, rs1, rs2` | 0x0B | high 32 bits of signed multiply |
| `DIV rd, rs1, rs2` | 0x0C | signed divide |
| `REM rd, rs1, rs2` | 0x0D | signed remainder |
| `SLTI rd, rs1, imm` | 0x16 | signed less-than immediate |
| `SLTIU rd, rs1, imm` | 0x17 | unsigned less-than immediate |
| `SLE rd, rs1, rs2` | 0x1A | signed less-or-equal |
| `SLEU rd, rs1, rs2` | 0x1B | unsigned less-or-equal |
| `SGE rd, rs1, rs2` | 0x1C | signed greater-or-equal |
| `SGEU rd, rs1, rs2` | 0x1D | unsigned greater-or-equal |
| `MULHU rd, rs1, rs2` | 0x1F | high 32 bits of unsigned multiply |

### Memory / Control Extensions

| Mnemonic | Opcode | Format | Operation |
|----------|:------:|:------:|-----------|
| `LDH rd, rs1, imm` | 0x31 | I | rd = sext(mem16[rs1 + sext(imm)]) |
| `LDHU rd, rs1, imm` | 0x34 | I | rd = zext(mem16[rs1 + sext(imm)]) |
| `STH rs1, rs2, imm` | 0x39 | S | mem16[rs1 + sext(imm)] = rs2[15:0] |
| `BLTU rs1, rs2, offset` | 0x4C | B | if (rs1 < rs2) PC += offset (unsigned) |
| `BGEU rs1, rs2, offset` | 0x4D | B | if (rs1 >= rs2) PC += offset (unsigned) |
| `NOP` | 0x50 | - | no side effects |
| `YIELD` | 0x51 | - | process MMIO request/response rings |

## Immediate Encoding Gotchas

### Sign-Extension vs Zero-Extension

**Sign-extended** (12-bit value fills upper 20 bits with sign bit):

- ADDI, SLTI, SLTIU
- All loads (LDB, LDH, LDW, LDBU, LDHU)
- All stores (STB, STH, STW) — the immediate offset
- JALR

**Zero-extended** (12-bit value, upper 20 bits are zero):

- ORI, ANDI, XORI
- SLLI, SRLI, SRAI

This means `ADDI r1, r0, -1` produces `0xFFFFFFFF` (sign-extended), but `ORI r1, r0, 0xFFF` produces `0x00000FFF` (zero-extended).

### Loading 32-Bit Constants (LUI + ADDI)

To load an arbitrary 32-bit value into a register:

```
LUI  rd, upper20
ADDI rd, rd, lower12
```

Where:
```
upper20 = (value + 0x800) >> 12   (compensate for ADDI sign-extension)
lower12 = value & 0xFFF
```

The `+0x800` compensation is needed because ADDI sign-extends its immediate. When bit 11 of `lower12` is set, ADDI effectively subtracts from the upper bits, so we add 1 to compensate.

**Warning:** Do NOT use ORI instead of ADDI for the lower 12 bits. ORI zero-extends, so the sign-extension compensation produces the wrong result when bit 11 is set.

### Example: Loading 0x12345678

```
upper20 = (0x12345678 + 0x800) >> 12 = 0x12345
lower12 = 0x678
```

Since bit 11 of 0x678 is set:
```
upper20 = (0x12345678 + 0x800) >> 12 = 0x12346  (compensated!)
lower12 = 0x678
```

```
LUI  r1, 0x12346        → r1 = 0x12346000
ADDI r1, r1, 0x678      → 0x678 sign-extends to 0xFFFFF678
                         → r1 = 0x12346000 + 0xFFFFF678 = 0x12345678 ✓
```

## Worked Examples

### NOP (No Operation)

`ADD r0, r0, r0` — writes to r0 (discarded), no side effects.

```
opcode=0x00, rd=0, rs1=0, rs2=0
= 0x00 | (0 << 7) | (0 << 15) | (0 << 20)
= 0x00000000
```

### `ADDI r29, r29, -4` (Decrement SP by 4)

```
opcode=0x10, rd=29, rs1=29, imm=-4 (0xFFC as 12-bit)
= 0x10 | (29 << 7) | (29 << 15) | (0xFFC << 20)
= 0x10 | 0xE80 | 0xE8000 | 0xFFC00000
= 0xFFC_E8E90   → 0xFFCE8E90
```

### `STW r29, r3, 0` (Store r3 at [SP+0])

```
opcode=0x3A, rd_field=imm[4:0]=0, rs1=29, rs2=3, imm[11:5]=0
= 0x3A | (0 << 7) | (29 << 15) | (3 << 20) | (0 << 25)
= 0x3A | 0 | 0xE8000 | 0x300000 | 0
= 0x003E803A
```

### `JAL r31, offset` (Function call)

For offset = +100 (0x64):

```
opcode=0x40, rd=31
imm_20    = (0x64 >> 20) & 1 = 0
imm_19_12 = (0x64 >> 12) & 0xFF = 0
imm_11    = (0x64 >> 11) & 1 = 0
imm_10_1  = (0x64 >> 1) & 0x3FF = 0x32

= 0x40 | (31 << 7) | (0 << 12) | (0 << 20) | (0x32 << 21) | (0 << 31)
= 0x40 | 0xF80 | 0 | 0 | 0x6400000 | 0
= 0x06400FC0
```

### `BEQ r3, r0, -8` (Branch back 8 bytes if r3 == 0)

For offset = -8 (0xFFFFFFF8 as signed):

```
opcode=0x48, rs1=3, rs2=0
imm_12   = ((-8) >> 12) & 1 = 1 (sign bit)
imm_11   = ((-8) >> 11) & 1 = 1
imm_10_5 = ((-8) >> 5) & 0x3F = 0x3F
imm_4_1  = ((-8) >> 1) & 0xF = 0xC

= 0x48 | (1 << 7) | (0xC << 8) | (3 << 15) | (0 << 20) | (0x3F << 25) | (1 << 31)
= 0x48 | 0x80 | 0xC00 | 0x18000 | 0 | 0x7E000000 | 0x80000000
= 0xFE018CC8
```

## Common Pseudo-Instructions

| Pseudo | Expansion |
|--------|-----------|
| `mv rd, rs` | `ADDI rd, rs, 0` |
| `li rd, small` | `ADDI rd, r0, small` (if -2048 <= small <= 2047) |
| `li rd, large` | `LUI rd, upper` + `ADDI rd, rd, lower` |
| `la rd, symbol` | `LUI rd, %hi(symbol)` + `ADDI rd, rd, %lo(symbol)` |
| `j offset` | `JAL r0, offset` |
| `jal target` | `JAL r31, target` (shorthand) |
| `jr rs` | `JALR r0, rs, 0` |
| `ret` | `JALR r0, r31, 0` |
| `call target` | `LUI r2, %hi(target)` + `JALR r31, r2, %lo(target)` |
| `nop` | `ADD r0, r0, r0` |

## Performance (Cycle Costs)

| Operation | Cycles |
|-----------|:------:|
| Most instructions | 1 |
| Loads / Stores | 3 |
| MUL / MULH | 32 |
| DIV / REM | 64 |

## Memory Layout

```
0x00000000 - 0x000FFFFF   Code region (1 MB, execute-only with W^X)
0x00100000 - 0x0FFFFFFF   Data region (read/write)
     Stack starts at 0x0FFFFFF0, grows downward
     Heap follows BSS, grows upward
0x10000000+                MMIO region (if enabled)
```
