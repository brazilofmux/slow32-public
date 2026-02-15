# Stage 0: Minimal Bootstrap Emulator

> **V2 cross-reference:** This is **Stage 0** in [BOOTSTRAP-V2.md](BOOTSTRAP-V2.md) (Layer A: Foundation).

## Goal

A minimal SLOW-32 emulator capable of running the bootstrap Forth kernel and, ultimately, the self-hosted toolchain. Small enough to audit by hand (currently ~780 lines of C in `selfhost/stage0/s32-emu.c`).

## What It Must Do

1. Load a .s32x executable (64-byte header + sections)
2. Execute 34 essential instructions
3. Handle 4 legacy MMIO operations (PUTCHAR, GETCHAR, BRK, EXIT)
4. Provide a flat memory space with code/data/stack regions

## .s32x Loader

The executable header is 64 bytes (`s32x_header_t` from `common/s32_formats.h`):

```
Offset  Size  Field          Description
0x00    4     magic          0x53333258 ("S32X" little-endian)
0x04    2     version        Format version (1)
0x06    1     endian         0x01 = little-endian
0x07    1     machine        0x32 = SLOW-32
0x08    4     entry          Entry point address
0x0C    4     nsections      Number of sections
0x10    4     sec_offset     Offset to section table
0x14    4     str_offset     Offset to string table
0x18    4     str_size       String table size
0x1C    4     flags          Executable flags
0x20    4     code_limit     End of code region
0x24    4     rodata_limit   End of read-only region
0x28    4     data_limit     End of data region
0x2C    4     stack_base     Initial stack pointer
0x30    4     mem_size       Total memory to allocate
0x34    4     heap_base      Start of heap
0x38    4     stack_end      Bottom of stack
0x3C    4     mmio_base      MMIO region base (0 if not used)
```

Each section entry is 28 bytes (`s32x_section_t`):

```
Offset  Size  Field          Description
0x00    4     name_offset    Index into string table
0x04    4     type           Section type (CODE=1, DATA=2, BSS=3, RODATA=4)
0x08    4     vaddr          Virtual address to load at
0x0C    4     offset         File offset to data
0x10    4     size           Size in file
0x14    4     mem_size       Size in memory (may be larger for BSS)
0x18    4     flags          Section flags
```

**Loading procedure:**

1. Read 64-byte header, verify magic = 0x53333258
2. Allocate `header.mem_size` bytes of memory (zeroed)
3. Seek to `header.sec_offset`, read each section
4. For non-BSS sections: seek to `section.offset`, read `section.size` bytes into `memory[section.vaddr]`
5. BSS sections: already zeroed by allocation
6. Set PC = `header.entry`, SP = `header.stack_base`, FP = `header.stack_base`

## Required Instructions (34)

### Arithmetic (3)

- `ADD` (0x00) — R-type: rd = rs1 + rs2
- `SUB` (0x01) — R-type: rd = rs1 - rs2
- `MUL` (0x0A) — R-type: rd = (rs1 * rs2) & 0xFFFFFFFF

### Logical (6)

- `XOR` (0x02) — R-type: rd = rs1 ^ rs2
- `OR` (0x03) — R-type: rd = rs1 | rs2
- `AND` (0x04) — R-type: rd = rs1 & rs2
- `SLL` (0x05) — R-type: rd = rs1 << (rs2 & 31)
- `SRL` (0x06) — R-type: rd = rs1 >>> (rs2 & 31)
- `SRA` (0x07) — R-type: rd = (int32_t)rs1 >> (rs2 & 31)

### Comparison (6)

- `SLT` (0x08) — R-type: rd = ((int32_t)rs1 < (int32_t)rs2) ? 1 : 0
- `SLTU` (0x09) — R-type: rd = (rs1 < rs2) ? 1 : 0 (unsigned)
- `SEQ` (0x0E) — R-type: rd = (rs1 == rs2) ? 1 : 0
- `SNE` (0x0F) — R-type: rd = (rs1 != rs2) ? 1 : 0
- `SGT` (0x18) — R-type: rd = ((int32_t)rs1 > (int32_t)rs2) ? 1 : 0
- `SGTU` (0x19) — R-type: rd = (rs1 > rs2) ? 1 : 0 (unsigned)

### Immediate (7)

- `ADDI` (0x10) — I-type: rd = rs1 + sext(imm12)
- `ORI` (0x11) — I-type: rd = rs1 | zext(imm12)
- `ANDI` (0x12) — I-type: rd = rs1 & zext(imm12)
- `SLLI` (0x13) — I-type: rd = rs1 << imm[4:0]
- `SRLI` (0x14) — I-type: rd = rs1 >>> imm[4:0]
- `SRAI` (0x15) — I-type: rd = (int32_t)rs1 >> imm[4:0]
- `XORI` (0x1E) — I-type: rd = rs1 ^ zext(imm12)

### Upper Immediate (1)

- `LUI` (0x20) — U-type: rd = imm << 12

### Memory (5)

- `LDB` (0x30) — I-type: rd = sext(mem8[rs1 + sext(imm)])
- `LDW` (0x32) — I-type: rd = mem32[rs1 + sext(imm)]
- `LDBU` (0x33) — I-type: rd = zext(mem8[rs1 + sext(imm)])
- `STB` (0x38) — S-type: mem8[rs1 + sext(imm)] = rs2[7:0]
- `STW` (0x3A) — S-type: mem32[rs1 + sext(imm)] = rs2

### Control (6)

- `JAL` (0x40) — J-type: rd = PC + 4; PC += sext(imm)
- `JALR` (0x41) — I-type: rd = PC + 4; PC = rs1 + sext(imm)
- `BEQ` (0x48) — B-type: if (rs1 == rs2) PC += sext(imm)
- `BNE` (0x49) — B-type: if (rs1 != rs2) PC += sext(imm)
- `BLT` (0x4A) — B-type: if ((int32_t)rs1 < (int32_t)rs2) PC += sext(imm)
- `BGE` (0x4B) — B-type: if ((int32_t)rs1 >= (int32_t)rs2) PC += sext(imm)

## I/O: Legacy MMIO Operations

The minimal emulator needs only the DEBUG instruction for character output and four legacy MMIO operations. The existing emulators treat these as special stores to the MMIO region:

| Op | Code | Mechanism | Behavior |
|----|:----:|-----------|----------|
| PUTCHAR | 0x01 | MMIO write | Write byte to stdout |
| GETCHAR | 0x02 | MMIO write + read | Read byte from stdin (blocking), return in response |
| BRK | 0x08 | MMIO write | No-op (heap is statically allocated) |
| EXIT | 0x09 | MMIO write | Terminate with exit code from data field |

For the simplest possible emulator, the `DEBUG` instruction (opcode 0x52) can serve as the sole output mechanism — it writes the character in `rs1` directly to the console. The Forth kernel uses `DEBUG` for all output.

For running the C toolchain (which uses file I/O), the full MMIO ring buffer protocol is needed (OPEN, READ, WRITE, CLOSE, SEEK, etc.). This can be added later as a ~200-line extension.

## Memory Model

```
0x00000000 ┬─────────────────────┐
           │  Code (.text)       │  Execute-only (W^X)
           │  Read-only (.rodata)│
code_limit ├─────────────────────┤
           │  Data (.data)       │  Read-write
           │  BSS (.bss)         │  (zeroed)
data_limit ├─────────────────────┤
           │  Heap               │  Grows upward
heap_base  │  ...                │
           │                     │
           │  (free space)       │
           │                     │
stack_end  │  ...                │  Stack grows downward
           │  Stack              │
stack_base ├─────────────────────┤
           │  (guard page)       │
0x0FFFFFF0 └─────────────────────┘
```

## Implementation Sketch

```c
// Core decode-execute loop (surrounded by loader/MMIO support)
uint8_t *mem;           // flat memory array
uint32_t reg[32];       // register file (reg[0] always 0)
uint32_t pc;            // program counter

void run(void) {
    for (;;) {
        uint32_t insn = *(uint32_t *)(mem + pc);
        uint32_t op   = insn & 0x7F;
        uint32_t rd   = (insn >> 7) & 0x1F;
        uint32_t rs1  = (insn >> 15) & 0x1F;
        uint32_t rs2  = (insn >> 20) & 0x1F;
        int32_t  imm_i = (int32_t)insn >> 20;  // sign-extended I-type
        // ... decode other immediate forms as needed

        switch (op) {
        case 0x00: reg[rd] = reg[rs1] + reg[rs2]; break;  // ADD
        case 0x01: reg[rd] = reg[rs1] - reg[rs2]; break;  // SUB
        // ... 32 more cases ...
        case 0x7F: return;  // HALT
        }

        reg[0] = 0;  // r0 is always zero
        pc += 4;      // advance (branches override this)
    }
}
```

## What Can Be Deferred

- **Half-word operations** (LDH, LDHU, STH) — not needed by Forth kernel
- **DIV/REM** — the C compiler uses libcalls (`__divsi3` etc.)
- **MULH/MULHU** — only needed for 64-bit multiply
- **Floating point** — not used by any bootstrap stage
- **BLTU/BGEU** — can work around with SLTU + BNE
- **W^X enforcement** — useful but not required for correctness
- **Full MMIO ring protocol** — needed only for stage 5 (C toolchain with file I/O)

## Verification

The bootstrap emulator should produce identical output to `tools/emulator/slow32` for any program using only the 34 essential instructions. Test with:

- The Forth kernel: `forth/kernel.s32x` with basic prelude
- Simple regression tests that don't use floating point or MMIO file I/O

## Reference

The existing reference emulator is `tools/emulator/slow32.c` (~1,200 lines including MMIO, tracing, watchpoints, and all 121 instructions). The bootstrap emulator strips this down to essentials.

## Current Checkpoint

As of 2026-02-15:

- `selfhost/stage0/s32-emu.c` is the active minimal emulator implementation.
- It is sufficient to run the Forth kernel and Stage 0-4 bootstrap workflows.
- Full-emulator capabilities (`tools/emulator/slow32`, `slow32-fast`, DBT, QEMU variants) remain outside Stage 0 scope.
