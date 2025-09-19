# FIX PLAN: Implement RISC-V-Style 32-bit Constant Loading

## Decision: RISC-V Approach

Use RISC-V's proven method:
1. LUI loads 20-bit immediate into bits [31:12]
2. ADDI adds 12-bit sign-extended immediate to complete the 32-bit value
3. When bit 11 of the lower part is set, increment the LUI value to compensate for sign extension

## Example

To load 0xDEADBEEF:
```
Original value: 0xDEADBEEF
Upper 20 bits: 0xDEADB
Lower 12 bits: 0xEEF

Since bit 11 of 0xEEF is set (0xEEF = 0b111011101111):
- ADDI will sign-extend 0xEEF to 0xFFFFFEEF
- So we need LUI to load 0xDEADC (0xDEADB + 1)

Result:
  lui r15, 0xDEADC     # Loads 0xDEADC000
  addi r15, r15, 0xEEF # Adds 0xFFFFFEEF (sign-extended)
  # r15 = 0xDEADC000 + 0xFFFFFEEF = 0xDEADBEEF ✓
```

## Changes Required

### 1. LLVM Backend (/ztank/secret/sdennis/llvm-project/llvm/lib/Target/SLOW32/)

**SLOW32ISelDAGToDAG.cpp - materializeImmediate()**
- Currently generates LUI (16-bit) + ORI
- Change to generate LUI (20-bit) + ADDI with proper adjustment
- When lower 12 bits have bit 11 set, increment upper 20 bits

**SLOW32InstrInfo.td**
- Ensure LUI handles 20-bit immediates correctly
- Use ADDI instead of ORI for constant materialization
- May need custom pattern for immediate loading

### 2. Assembler (/ztank/secret/sdennis/slow-32/assembler/slow32asm.c)

**encode_u() function**
- Currently: `((imm << 16) & 0xFFFF0000)` - WRONG!
- Change to: `(imm & 0xFFFFF) << 12` - Put 20 bits in [31:12]

**LUI parsing**
- Accept 20-bit immediates (0-0xFFFFF)
- Remove the 16-bit shift hack

### 3. Emulator - NO CHANGES NEEDED
- Already correctly implements RISC-V style:
  - LUI: `inst.imm = raw & 0xFFFFF000;`
  - Puts immediate directly in register
- Just need to verify ADDI sign-extends correctly (it should)

### 4. Test Cases

Create test that loads multiple constants:
- 0xDEADBEEF (tests bit 11 adjustment)
- 0x12345678 (no adjustment needed)
- 0x80000000 (sign bit)
- 0xFFFFFFFF (all ones)
- 0x00000001 (small positive)
- Small negative values

## Verification Steps

1. Fix LLVM backend first
2. Fix assembler to match
3. Test with emulator (should already work)
4. Run comprehensive constant loading tests
5. Run full regression suite

## Order of Implementation

1. **First**: Assembler fix (smallest change)
   - Fix encode_u() to properly encode 20 bits

2. **Second**: LLVM backend fix  
   - Update materializeImmediate() in SLOW32ISelDAGToDAG.cpp
   - Change from LUI+ORI to LUI+ADDI

3. **Third**: Comprehensive testing
   - Verify all 32-bit constants can be loaded
   - Run regression tests

This is the path forward. It's proven to work (RISC-V uses it successfully).