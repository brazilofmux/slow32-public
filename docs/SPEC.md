# SLOW-32 Machine Specification

This document is the specification of the SLOW-32 machine: the
instruction set, the executable file format (`.s32x`), how an executable
is loaded, the memory it may touch, how it stops, and the host interface
through which it does I/O. It is meant to be complete enough on its own
that someone holding only this document, without the source tree or the
toolchain, can build a machine that runs existing SLOW-32 executables
correctly.

The machine is frozen. Where the reference implementation behaves in a
way that looks accidental, this document specifies that behaviour as it
is, marked **Quirk**, because existing executables depend on it. Where
behaviour is genuinely not defined (the engines differ, or it was never
pinned down), that is said plainly, marked **Unspecified** (section 8
also says **Implementation-defined**, meaning the same), and programs
must not depend on it.

Everything else under `docs/` is rationale, design history, or
documentation of the toolchain. Where another document disagrees with
this one, this one is right. The object file format (`.s32o`), archives,
the assembler and the calling convention are toolchain matters, not part
of the machine, and are described in `file-formats.md` and
`CALLING_CONVENTION.md`.

The reference implementation is `tools/emulator/slow32` with
`tools/emulator/mmio_ring.c` and `common/mmio_ring_layout.h`. Four other
engines (`slow32-fast`, `slow32-dbt`, QEMU's `qemu-system-slow32`, and
the self-hosted `stage00` emulator) are checked against it.

Contents:

1. Conventions
2. Registers and state
3. Instruction encoding
4. Instructions
5. The executable format
6. Loading and the memory map
7. Execution, faults and termination
8. Host interface (MMIO)
9. Conformance

---

## 1. Conventions

- All multi-byte quantities, in memory, in instructions and in files, are
  **little-endian**.
- A *word* is 32 bits, a *half* 16 bits, a *byte* 8 bits.
- `x[a:b]` is bits a down to b of x, inclusive; bit 0 is least
  significant.
- `sext(v, n)` sign-extends the n-bit value v to 32 bits; `zext(v, n)`
  zero-extends it.
- Arithmetic on register values is modulo 2^32 unless said otherwise.
  "Signed" means two's complement.
- Hex numbers are written `0x...`.

## 2. Registers and state

The machine has 32 general-purpose 32-bit registers `r0`–`r31` and a
32-bit program counter `PC`. There are no other architectural registers:
no condition codes, no separate floating-point registers, no status or
control registers.

- `r0` always reads as 0. A write to `r0` is discarded.
- All other registers are identical as far as the machine is concerned.
  Their conventional roles (r1–r2 return values, r3–r10 arguments, r29
  stack pointer, r30 frame pointer, r31 link register) belong to the
  calling convention, with one exception the machine itself relies on:
  **r1 holds the exit status** when a program halts (section 7).

Floating-point values live in the general registers:

- A single-precision value (IEEE 754 binary32) occupies one register.
- A double-precision value (binary64) occupies a register **pair**
  `(rN, rN+1)`: `rN` holds bits 31:0, `rN+1` holds bits 63:32. In every
  instruction that reads or writes a double, N must be **even** and less
  than 31. Odd N, or N = 31, is **Unspecified** (the reference
  interpreter faults; other engines may not).
- 64-bit integers handled by the float/int64 conversions use the same
  pairing: low word in `rN`, high word in `rN+1`.

## 3. Instruction encoding

Every instruction is one 32-bit word, little-endian, at a 4-byte-aligned
address. Bits 6:0 are the opcode; the opcode alone identifies the
instruction. Fields not used by an instruction are ignored (in
particular bits 14:12, which RISC-V calls funct3, and bits 31:25 in
register-register instructions, are not decoded). Six formats share
field positions:

```
R  [31:25] -        [24:20] rs2  [19:15] rs1  [14:12] -  [11:7] rd        [6:0] opcode
I  [31:20] imm[11:0]             [19:15] rs1  [14:12] -  [11:7] rd        [6:0] opcode
S  [31:25] imm[11:5] [24:20] rs2 [19:15] rs1  [14:12] -  [11:7] imm[4:0]  [6:0] opcode
B  [31] imm[12] [30:25] imm[10:5] [24:20] rs2 [19:15] rs1 [14:12] - [11:8] imm[4:1] [7] imm[11] [6:0] opcode
U  [31:12] imm[31:12]                                    [11:7] rd        [6:0] opcode
J  [31] imm[20] [30:21] imm[10:1] [20] imm[11] [19:12] imm[19:12] [11:7] rd [6:0] opcode
```

The immediate of each format, as a 32-bit value:

| Format | Immediate |
|---|---|
| I, signed | `sext(instr[31:20], 12)` |
| I, unsigned | `zext(instr[31:20], 12)` |
| S | `sext(instr[31:25] << 5 \| instr[11:7], 12)` |
| B | `sext(instr[31]<<12 \| instr[7]<<11 \| instr[30:25]<<5 \| instr[11:8]<<1, 13)` (bit 0 is 0) |
| U | `instr & 0xFFFFF000` |
| J | `sext(instr[31]<<20 \| instr[19:12]<<12 \| instr[20]<<11 \| instr[30:21]<<1, 21)` (bit 0 is 0) |

Which I-format instructions sign-extend and which zero-extend is part of
each instruction's definition below. **Quirk:** ORI, ANDI, XORI and
SLTIU **zero-extend** their 12-bit immediate; ADDI, SLTI, the shifts,
the loads and JALR sign-extend. So `ori rd, rs, 0x800` sets bit 11 only,
`andi rd, rs, 0xFFF` keeps the low 12 bits, and there is no one-instruction
bitwise NOT.

## 4. Instructions

In the tables, `R[x]` is the value of register x, `M8/M16/M32[a]` the
byte, half or word at address a, and `imm` the immediate as the format
defines it. "Next" is the address of the following instruction, PC + 4.

### 4.1 Integer register-register (format R)

| Op | Mnemonic | Operation |
|---|---|---|
| 0x00 | ADD | R[rd] = R[rs1] + R[rs2] |
| 0x01 | SUB | R[rd] = R[rs1] − R[rs2] |
| 0x02 | XOR | R[rd] = R[rs1] ^ R[rs2] |
| 0x03 | OR | R[rd] = R[rs1] \| R[rs2] |
| 0x04 | AND | R[rd] = R[rs1] & R[rs2] |
| 0x05 | SLL | R[rd] = R[rs1] << (R[rs2] & 31) |
| 0x06 | SRL | R[rd] = R[rs1] >> (R[rs2] & 31), logical |
| 0x07 | SRA | R[rd] = R[rs1] >> (R[rs2] & 31), arithmetic |
| 0x08 | SLT | R[rd] = (signed R[rs1] < signed R[rs2]) ? 1 : 0 |
| 0x09 | SLTU | R[rd] = (R[rs1] < R[rs2]) ? 1 : 0, unsigned |
| 0x0A | MUL | R[rd] = low 32 bits of R[rs1] × R[rs2] |
| 0x0B | MULH | R[rd] = high 32 bits of the 64-bit product, both operands signed |
| 0x1F | MULHU | R[rd] = high 32 bits of the 64-bit product, both operands unsigned |
| 0x0C | DIV | R[rd] = signed R[rs1] ÷ signed R[rs2], truncated toward zero |
| 0x0D | REM | R[rd] = signed remainder; its sign is that of R[rs1] |
| 0x0E | SEQ | R[rd] = (R[rs1] == R[rs2]) ? 1 : 0 |
| 0x0F | SNE | R[rd] = (R[rs1] != R[rs2]) ? 1 : 0 |
| 0x18 | SGT | signed > |
| 0x19 | SGTU | unsigned > |
| 0x1A | SLE | signed ≤ |
| 0x1B | SLEU | unsigned ≤ |
| 0x1C | SGE | signed ≥ |
| 0x1D | SGEU | unsigned ≥ |

Division never traps:

| Case | DIV | REM |
|---|---|---|
| divisor 0 | 0xFFFFFFFF | R[rs1] |
| 0x80000000 ÷ 0xFFFFFFFF (−2^31 ÷ −1) | 0x80000000 | 0 |

There is no unsigned divide instruction; the toolchain calls a library
routine.

### 4.2 Integer register-immediate (format I)

| Op | Mnemonic | Immediate | Operation |
|---|---|---|---|
| 0x10 | ADDI | signed | R[rd] = R[rs1] + imm |
| 0x11 | ORI | **unsigned** | R[rd] = R[rs1] \| imm |
| 0x12 | ANDI | **unsigned** | R[rd] = R[rs1] & imm |
| 0x1E | XORI | **unsigned** | R[rd] = R[rs1] ^ imm |
| 0x13 | SLLI | signed | R[rd] = R[rs1] << (imm & 31) |
| 0x14 | SRLI | signed | R[rd] = R[rs1] >> (imm & 31), logical |
| 0x15 | SRAI | signed | R[rd] = R[rs1] >> (imm & 31), arithmetic |
| 0x16 | SLTI | signed | R[rd] = (signed R[rs1] < signed imm) ? 1 : 0 |
| 0x17 | SLTIU | **unsigned** | R[rd] = (R[rs1] < imm) ? 1 : 0, unsigned compare |

### 4.3 Upper immediate (format U)

| Op | Mnemonic | Operation |
|---|---|---|
| 0x20 | LUI | R[rd] = imm (that is, instr[31:12] << 12) |

There is no AUIPC. A 32-bit constant or address is built with
`lui` + `ori` (whose zero-extended immediate makes this work without
the carry adjustment RISC-V needs) or `lui` + `addi`.

### 4.4 Loads (format I) and stores (format S)

The effective address is `R[rs1] + imm` with a signed immediate.

| Op | Mnemonic | Operation |
|---|---|---|
| 0x30 | LDB | R[rd] = sext(M8[a], 8) |
| 0x31 | LDH | R[rd] = sext(M16[a], 16) |
| 0x32 | LDW | R[rd] = M32[a] |
| 0x33 | LDBU | R[rd] = zext(M8[a], 8) |
| 0x34 | LDHU | R[rd] = zext(M16[a], 16) |
| 0x38 | STB | M8[a] = R[rs2] & 0xFF |
| 0x39 | STH | M16[a] = R[rs2] & 0xFFFF |
| 0x3A | STW | M32[a] = R[rs2] |

**Unaligned access is permitted.** A word or half at any address
accesses exactly the bytes named, and is not an error. (Aligning data is
a performance matter only; nothing may depend on it.)

An access is valid only if every byte it touches lies in one mapped
region with the needed permission (section 6). Otherwise it faults
(section 7).

### 4.5 Control transfer

| Op | Mnemonic | Format | Operation |
|---|---|---|---|
| 0x48 | BEQ | B | if R[rs1] == R[rs2]: PC = PC + 4 + imm |
| 0x49 | BNE | B | if R[rs1] != R[rs2]: PC = PC + 4 + imm |
| 0x4A | BLT | B | if signed R[rs1] < signed R[rs2]: PC = PC + 4 + imm |
| 0x4B | BGE | B | if signed R[rs1] ≥ signed R[rs2]: PC = PC + 4 + imm |
| 0x4C | BLTU | B | if R[rs1] < R[rs2] unsigned: PC = PC + 4 + imm |
| 0x4D | BGEU | B | if R[rs1] ≥ R[rs2] unsigned: PC = PC + 4 + imm |
| 0x40 | JAL | J | R[rd] = PC + 4; PC = PC + imm |
| 0x41 | JALR | I (signed) | t = (R[rs1] + imm) & ~1; R[rd] = PC + 4; PC = t |

**Quirk: conditional branches are relative to the next instruction
(PC + 4); JAL is relative to the JAL itself (PC).** A branch with
immediate 0 falls through whether taken or not; a JAL with immediate 0
is an infinite loop. Both are as the toolchain emits them, and every
existing executable depends on this.

JALR computes its target from R[rs1] before writing R[rd], so
`jalr rX, rX, 0` jumps to the old value of rX. It clears bit 0 of the
target and nothing else; a target that is not a multiple of 4 is
**Unspecified** (the toolchain never produces one).

The conventional forms: a call is `jal r31, f`; a return is
`jalr r0, r31, 0`; a jump is `jal r0, label`; a tail call through a
register is `jalr r0, rX, 0`.

### 4.6 System

| Op | Mnemonic | Format | Operation |
|---|---|---|---|
| 0x50 | NOP | — | nothing |
| 0x51 | YIELD | — | a service point for the host interface (section 8); otherwise nothing |
| 0x52 | DEBUG | R (rs1 only) | write the byte R[rs1] & 0xFF to the host's standard output |
| 0x3F | ASSERT_EQ | R (rs1, rs2) | if R[rs1] != R[rs2]: stop with a fault (section 7) |
| 0x7F | HALT | — | a final service point, then stop (section 7) |

DEBUG output is unbuffered as far as the program can tell: it appears in
order with everything else the program writes to standard output.

Every opcode not listed in this section is **illegal**: executing it
faults. (Note that 0x00 is ADD, so a word of zeros is
`add r0, r0, r0`, a no-op, not a fault.)

### 4.7 Floating point

Floating-point values are IEEE 754 binary32 and binary64, held in the
general registers as section 2 describes. Arithmetic rounds to nearest,
ties to even; there is no other rounding mode, no exception flags, and
no traps. Comparisons involving a NaN return 0. Subnormal numbers are
supported (no flush to zero).

**Unspecified:** the bit pattern of a NaN produced by an operation
(sign and payload differ between host processors).

All are format R. "f" is a single in R[x]; "d" a double in the pair
(R[x], R[x+1]). Unary operations ignore rs2.

| Op | Mnemonic | Operation |
|---|---|---|
| 0x53 | FADD.S | f[rd] = f[rs1] + f[rs2] |
| 0x54 | FSUB.S | f[rd] = f[rs1] − f[rs2] |
| 0x55 | FMUL.S | f[rd] = f[rs1] × f[rs2] |
| 0x56 | FDIV.S | f[rd] = f[rs1] ÷ f[rs2] |
| 0x57 | FSQRT.S | f[rd] = √f[rs1] |
| 0x58 | FEQ.S | R[rd] = (f[rs1] == f[rs2]) ? 1 : 0 |
| 0x59 | FLT.S | R[rd] = (f[rs1] < f[rs2]) ? 1 : 0 |
| 0x5A | FLE.S | R[rd] = (f[rs1] ≤ f[rs2]) ? 1 : 0 |
| 0x5B | FCVT.W.S | R[rd] = f[rs1] converted to signed 32-bit, truncating |
| 0x5C | FCVT.WU.S | R[rd] = f[rs1] converted to unsigned 32-bit, truncating |
| 0x5D | FCVT.S.W | f[rd] = signed R[rs1] converted, rounded |
| 0x5E | FCVT.S.WU | f[rd] = unsigned R[rs1] converted, rounded |
| 0x5F | FNEG.S | R[rd] = R[rs1] ^ 0x80000000 |
| 0x60 | FABS.S | R[rd] = R[rs1] & 0x7FFFFFFF |
| 0x61 | FADD.D | d[rd] = d[rs1] + d[rs2] |
| 0x62 | FSUB.D | d[rd] = d[rs1] − d[rs2] |
| 0x63 | FMUL.D | d[rd] = d[rs1] × d[rs2] |
| 0x64 | FDIV.D | d[rd] = d[rs1] ÷ d[rs2] |
| 0x65 | FSQRT.D | d[rd] = √d[rs1] |
| 0x66 | FEQ.D | R[rd] = (d[rs1] == d[rs2]) ? 1 : 0 |
| 0x67 | FLT.D | R[rd] = (d[rs1] < d[rs2]) ? 1 : 0 |
| 0x68 | FLE.D | R[rd] = (d[rs1] ≤ d[rs2]) ? 1 : 0 |
| 0x69 | FCVT.W.D | R[rd] = d[rs1] to signed 32-bit, truncating |
| 0x6A | FCVT.WU.D | R[rd] = d[rs1] to unsigned 32-bit, truncating |
| 0x6B | FCVT.D.W | d[rd] = signed R[rs1] (exact) |
| 0x6C | FCVT.D.WU | d[rd] = unsigned R[rs1] (exact) |
| 0x6D | FCVT.D.S | d[rd] = f[rs1] (exact) |
| 0x6E | FCVT.S.D | f[rd] = d[rs1], rounded |
| 0x6F | FNEG.D | R[rd] = R[rs1]; R[rd+1] = R[rs1+1] ^ 0x80000000 |
| 0x70 | FABS.D | R[rd] = R[rs1]; R[rd+1] = R[rs1+1] & 0x7FFFFFFF |
| 0x71 | FCVT.L.S | (R[rd], R[rd+1]) = f[rs1] to signed 64-bit, truncating |
| 0x72 | FCVT.LU.S | (R[rd], R[rd+1]) = f[rs1] to unsigned 64-bit, truncating |
| 0x73 | FCVT.S.L | f[rd] = signed 64-bit (R[rs1], R[rs1+1]), rounded |
| 0x74 | FCVT.S.LU | f[rd] = unsigned 64-bit (R[rs1], R[rs1+1]), rounded |
| 0x75 | FCVT.L.D | (R[rd], R[rd+1]) = d[rs1] to signed 64-bit, truncating |
| 0x76 | FCVT.LU.D | (R[rd], R[rd+1]) = d[rs1] to unsigned 64-bit, truncating |
| 0x77 | FCVT.D.L | d[rd] = signed 64-bit (R[rs1], R[rs1+1]), rounded |
| 0x78 | FCVT.D.LU | d[rd] = unsigned 64-bit (R[rs1], R[rs1+1]), rounded |

Float-to-integer conversions truncate toward zero. **Unspecified:** the
result when the value is a NaN or does not fit the destination type
(the engines follow their host processors, which disagree). The
toolchain's runtime does not rely on it.

Operations that write a single register (compares, conversions to a
32-bit integer or to a single) may name any rd, odd or even.

There are no instructions for remainder, min/max, fused multiply-add,
or the transcendental functions; the toolchain implements them in
software.

---

## 5. The executable format (`.s32x`)

An executable is a header, a section table, a string table, and section
data, at offsets the header and the table give. All fields are
little-endian.

### 5.1 Header (64 bytes, at file offset 0)

| Offset | Size | Field | Meaning |
|---|---|---|---|
| 0x00 | 4 | magic | 0x53333258 (the bytes `X23S`) |
| 0x04 | 2 | version | 1 |
| 0x06 | 1 | endian | 1 (little) |
| 0x07 | 1 | machine | 0x32 |
| 0x08 | 4 | entry | initial PC |
| 0x0C | 4 | nsections | number of section table entries |
| 0x10 | 4 | sec_offset | file offset of the section table |
| 0x14 | 4 | str_offset | file offset of the string table |
| 0x18 | 4 | str_size | size of the string table in bytes |
| 0x1C | 4 | flags | see below |
| 0x20 | 4 | code_limit | end of the code region (exclusive) |
| 0x24 | 4 | rodata_limit | end of the read-only region (exclusive) |
| 0x28 | 4 | data_limit | end of initialized data and BSS (exclusive) |
| 0x2C | 4 | stack_base | initial stack pointer |
| 0x30 | 4 | mem_size | size of the address space; 0x10000000 |
| 0x34 | 4 | heap_base | start of the heap, for the runtime |
| 0x38 | 4 | stack_end | lowest address of the stack region |
| 0x3C | 4 | mmio_base | base of the host-interface window, if flag MMIO |

Flags:

| Bit | Name | Meaning |
|---|---|---|
| 0x0001 | W^X | code and read-only data are not writable. Set by the linker in every executable. |
| 0x0080 | MMIO | the executable uses the host interface; `mmio_base` is valid |

Other flag bits (0x0002 EVT, 0x0004 TSR, 0x0008 DEBUG, 0x0010 STRIPPED,
0x0020 PIC, 0x0040 COMPRESSED) are not used by any executable and must
be zero; a machine may refuse an executable that sets one.

A machine must refuse a file whose magic, version or machine field is
wrong, and one whose `mem_size` exceeds 0x10000000.

### 5.2 Section table (28 bytes per entry)

| Offset | Size | Field |
|---|---|---|
| 0x00 | 4 | name_offset (into the string table) |
| 0x04 | 4 | type |
| 0x08 | 4 | vaddr: load address |
| 0x0C | 4 | offset: file offset of the data |
| 0x10 | 4 | size: bytes in the file |
| 0x14 | 4 | mem_size: bytes in memory |
| 0x18 | 4 | flags: 1 exec, 2 write, 4 read, 8 alloc |

Types: 1 code, 2 data, 3 BSS, 4 read-only data, 0x21 symbol table,
0x22 symbol string table, 0x30 `.eh_frame`, 0x31 `.eh_frame_hdr`,
0x32 `.gcc_except_table`. The type and flags are informative: what
loading does depends only on `mem_size`, `size`, `offset` and `vaddr`
(section 6.1).

The string table holds NUL-terminated section names. The symbol table
sections (`mem_size` 0) are for tools; their format is that of the
object file (`file-formats.md`) and a machine ignores them.

### 5.3 What the linker produces

Every existing executable has this shape (values from a typical one):

- Code from 0 to `code_limit`, a multiple of 4 KB (0x11000). Entry 0.
- Read-only data (`.rodata`, `.eh_frame`, `.eh_frame_hdr`) from
  `code_limit` to `rodata_limit`, a multiple of 4 KB (0x18000).
- Data and BSS from `rodata_limit` to `data_limit` (0x18E24, not
  rounded).
- `heap_base` = `data_limit` rounded up to 4 KB (0x19000).
- `stack_base` 0x0FFFFFF0; `stack_end` 0x0FFEFFF0 (a 64 KB stack by
  default; the linker can make it larger).
- With MMIO, `mmio_base` = the end of the heap, a 64 KB window, and the
  heap ends at `mmio_base`. Without MMIO, `mmio_base` is 0.

---

## 6. Loading and the memory map

### 6.1 Loading

1. Validate the header (5.1).
2. Set up the regions of 6.2, all zero-filled.
3. For each section with `mem_size` > 0: if `size` > 0 and `offset` > 0,
   copy `size` bytes from the file at `offset` to memory at `vaddr`. The
   remaining `mem_size − size` bytes stay zero. A section that does not
   fit inside the regions of 6.2 makes the file invalid.
4. Apply protection (6.3).
5. Set every register to 0, then R[29] (the stack pointer) =
   `stack_base`. PC = `entry`; an entry at or beyond `code_limit` makes
   the file invalid.
6. If flag MMIO is set, map the host-interface window at `mmio_base`
   (section 8) and give the host the program's arguments.

Nothing is passed in registers. Arguments and environment come through
the host interface (section 8).

### 6.2 Regions

The address space is 0 to 0x0FFFFFFF (`mem_size`). These regions are
mapped:

| Region | From | To (exclusive) | Access |
|---|---|---|---|
| code | 0 | `code_limit` | execute, read |
| read-only data | `code_limit` | `rodata_limit` | read |
| data, BSS and heap | `rodata_limit` | `mmio_base` if MMIO and `mmio_base` < `stack_end`, otherwise `stack_end` (never below `data_limit`) | read, write |
| host window | `mmio_base` | `mmio_base` + 0x10000 | read, write (section 8); only with flag MMIO |
| stack | `stack_end` | `stack_base` + 16 | read, write |

Data, BSS and heap are one region, so an access may straddle
`data_limit` or `heap_base`. (Before 2026-10-03 the reference mapped the
heap as a separate region from `data_limit` rounded up to 4 KB, leaving
the bytes in between unmapped.)

Any address outside these regions, including every address at or above
`mem_size`, is unmapped. An access to an unmapped address faults.
**Unspecified:** whether an access in a gap *between* regions faults;
the reference interpreter faults, a faster engine may not check. Programs
must not touch the gaps. Accesses at or above `mem_size` must fault in
every engine.

### 6.3 Protection

- **Fetch:** an instruction may be fetched only from `[0, code_limit)`.
  Fetching outside it faults.
- **Write:** with flag W^X (always set), writes to `[0, rodata_limit)`
  fault.
- **Read:** the code region is readable. (Earlier documents called code
  "execute-only"; no engine enforces that for loads, and no executable
  relies on either behaviour. Programs should not read their own code.)

---

## 7. Execution, faults and termination

The machine executes one instruction at a time: fetch at PC, execute,
then PC = the next instruction or the transfer target. There are no
interrupts, exceptions delivered to the program, or privilege levels.

### 7.1 Stopping

A program stops in one of three ways:

1. **HALT.** The host interface is serviced one last time (section 8),
   then the machine stops. The exit status is R[1].
2. **Exit request.** A program using the host interface calls OP_EXIT
   (section 8); the host stops the machine at that service point. The
   exit status is the value the request carried, which the host also
   writes into R[1].
3. **Fault.** See 7.2.

The host process's exit code is the exit status, truncated by the host
operating system as usual (the low 8 bits on POSIX).

The runtime library's `_exit` is `R[1] = status; HALT`, and its `exit`
(with MMIO) sends OP_EXIT. A signal raised with its default action
halts with 128 + the signal number, so `abort` halts with 134 (SIGABRT
is 6). These are runtime conventions, not machine rules.

### 7.2 Faults

A fault happens on: an access to an unmapped address or one without the
needed permission (6.2, 6.3), a fetch outside the code region, an
illegal opcode, a failed ASSERT_EQ, or (in the reference interpreter) an
invalid double-precision register. The faulting instruction has no
effect. The machine stops, and the host writes a diagnostic to its
standard error.

The exit status after a fault is 128 plus the number of the POSIX signal a
native process would have died of: **139** (SIGSEGV) for a memory, fetch or
protection fault, **132** (SIGILL) for an illegal opcode or an invalid
double-precision register, **134** (SIGABRT) for a failed ASSERT_EQ. (Before
2026-10-03 the engines exited with whatever R[1] held, so a fault could
exit 0.) A test harness that sees 132, 134 or 139 must decide from the
diagnostic whether the program faulted or the host itself died of that
signal.

**Unspecified:** the wording of the diagnostic. The reference
interpreter's forms are `Memory fault: Failed to read N bytes at 0xADDR
(...)` and `Memory fault: Failed to write N bytes at 0xADDR (...)`; the
test harness compares only the address.

Output the program wrote before the fault (by DEBUG or through the host
interface) is kept.

### 7.3 The host's own output

The reference interpreter prints banner and statistics lines on standard
output unless run with `-q` (`Starting execution`, `Program halted.`,
`Instructions executed: ...`, `HALT at PC=...` and similar). They are
the host's, not the program's; a conforming machine need not print them,
and the test harness removes them before comparing.

---

## 8. Host interface (MMIO)

An executable with flag MMIO does all its I/O through a 64 KB window of
shared memory at `mmio_base`: it writes request descriptors and data,
executes YIELD, and reads the host's responses.  This section specifies
that protocol, every operation's request and response byte by byte, and
the two negotiated services (terminal and graphics).

Conventions for this section:

- All multi-byte values are **little-endian**. "u32" = unsigned 32-bit,
  "i32" = signed 32-bit two's complement, "u64" = unsigned 64-bit stored as
  low word then high word.
- "The host does X" is normative. **Quirk:** marks reference behaviour that
  looks unintended but is specified as-is because the machine is frozen.
  **Implementation-defined** means existing binaries cannot depend on it.
- `CAP` = 49152 (0xC000), the data-buffer capacity.
- `off` in request tables is `request.offset`, which must be below CAP (see §8.2.4).


### 8.0 Two output paths that are not MMIO

1. **DEBUG instruction.** `DEBUG rs1` writes the low 8 bits of `rs1` as one
   byte to the host's standard output and flushes it. Available whether or not
   the executable has an MMIO window. (The DEBUG-only C library,
   `libc_debug`, implements `putchar` this way.)
2. **HALT instruction.** Ends execution (§8.3).

Everything else (files, stdin, arguments, environment, time, sockets,
terminal, graphics) goes through the MMIO window.


### 8.1 Window layout

#### 8.1.1 Presence and base address

An executable has an MMIO window iff bit `0x0080` (`S32X_FLAG_MMIO`) is set
in the `.s32x` header `flags` word (header offset `0x1C`). The window's guest
base address is the header word `mmio_base` (header offset `0x3C`). The
linker places it after the heap, 4 KB aligned; **it is not a fixed address**
(it is not `0x10000000`). Guest code finds it through the linker symbol
`__mmio_base` (absolute, = header `mmio_base`); `__mmio_end` =
`__mmio_base + SIZE` where SIZE is the linker's `--mmio` argument.

The host window is always **64 KB (0x10000 bytes)** starting at `mmio_base`,
regardless of the `--mmio` SIZE recorded by the linker. (A binary linked with
`--mmio` smaller than 64K: implementation-defined; all existing binaries use
`--mmio 64K`.) A `mmio_base` of 0 with the flag set: implementation-defined
(the reference interpreter treats base 0 as "no window").

If the flag is clear there is no window; guest accesses to that range are
ordinary memory accesses (normally a memory fault). All window state is zero
when the program starts.

#### 8.1.2 Map (offsets relative to `mmio_base`)

| Offset | Size | Name | Written by | Meaning |
|---|---|---|---|---|
| `0x0000` | u32 | `REQ_HEAD` | guest | index of the next request slot the guest will fill |
| `0x0004` | u32 | `REQ_TAIL` | host | index of the next request the host will consume |
| `0x0010` | u32 | `DPC_HEAD` | host | index of the next DPC slot the host will fill |
| `0x0014` | u32 | `DPC_TAIL` | guest | index of the next DPC entry the guest will consume |
| `0x0800`–`0x0BFF` | 64 × 16 B | DPC ring | host | asynchronous completions (§8.12) |
| `0x1000`–`0x1FFF` | 256 × 16 B | request ring | guest | request descriptors |
| `0x2000` | u32 | `RESP_HEAD` | host | index of the next response slot the host will fill |
| `0x2004` | u32 | `RESP_TAIL` | guest | index of the next response the guest will consume |
| `0x3000`–`0x3FFF` | 256 × 16 B | response ring | host | response descriptors |
| `0x4000`–`0xFFFF` | 48 KB | data buffer | both | bulk arguments and results |

Every other offset in `0x0000`–`0x3FFF` is unused: the guest must not rely on
it (the reference interpreter returns 0 on reads and discards writes; the
flat-memory engines treat it as RAM).

#### 8.1.3 Access widths

Head/tail registers and ring descriptors are accessed as aligned 32-bit
words. The data buffer may be accessed with byte, halfword or word loads and
stores. A sub-word store to a register or descriptor word is a
read-modify-write of the containing word. The guest must not write the DPC
ring or `DPC_HEAD`, `REQ_TAIL`, `RESP_HEAD` (implementation-defined: the
reference interpreter discards guest writes to the DPC ring but accepts
writes to the host-owned indices).


### 8.2 Descriptors and rings

#### 8.2.1 Descriptor

Every ring entry is 16 bytes, four u32 words:

| Word | Byte | Name | In a request | In a response |
|---|---|---|---|---|
| 0 | +0 | `opcode` | operation | copy of the request's opcode |
| 1 | +4 | `length` | primary byte count (per opcode) | result byte count, or errno when `status` = ERR (§8.4) |
| 2 | +8 | `offset` | data-buffer offset (or, per opcode, a guest address / generation number) | copy of the request's `offset` |
| 3 | +12 | `status` | fd / flags / argument (per opcode) | result (per opcode) |

Response `length` is 0 unless the opcode table says otherwise. The host never
modifies request descriptors.

#### 8.2.2 Indices

Head and tail values are **entry indices**, not byte offsets: request and
response rings modulo 256, DPC ring modulo 64. Entry `i` of a ring is at
`ring_base + 16*i`. A ring is empty when `head == tail` and full when
`(head + 1) mod N == tail`, so at most N−1 entries are outstanding.

The guest must store index values already reduced (0..255, 0..63).
(Implementation-defined: the reference interpreter reduces a stored value
mod N; the flat-memory engines use it as stored.)

#### 8.2.3 Protocol

Guest, to issue a request:

1. Write the payload into the data buffer.
2. Wait while the request ring is full (executing YIELD, re-reading `REQ_TAIL`).
3. Write the four words of request-ring entry `REQ_HEAD`.
4. Store `REQ_HEAD = (REQ_HEAD + 1) mod 256`.
5. Execute YIELD (§8.3) until `RESP_HEAD != RESP_TAIL`.
6. Read response entry `RESP_TAIL`, then store `RESP_TAIL = (RESP_TAIL + 1) mod 256`.
7. Read results from the data buffer.

Host, at each service point: for every request from `REQ_TAIL` up to (not
including) `REQ_HEAD`, in order: perform it, write the response into entry
`RESP_HEAD` and advance `RESP_HEAD`; then advance `REQ_TAIL`.

A request is taken only when the response ring has room for its response;
while the response ring is full, requests stay queued for a later service
point. (The guest libraries never have more than one request outstanding.)

Several requests may be queued before one YIELD (they are performed in
order). Each request sees the data buffer as left by the requests before it
in the same batch. The data buffer is a single shared scratch area: the
guest libraries put every payload at offset 0 and copy results out before
issuing the next request.

#### 8.2.4 The `offset` word

For every fixed opcode that uses the data buffer, an `offset` ≥ 49152 fails
with `EINVAL` before anything else is done; then the opcode table's range
checks apply. (Before 2026-10-03 the reference reduced it mod 49152.)

Exceptions where `offset` is not a data-buffer offset: `READ_DIRECT` and
`POST_READ` (a guest address), tube `PRESENT` (a generation number).


### 8.3 Service points and exit

#### 8.3.1 When requests are serviced

The host services the window **only** when the guest executes `YIELD` or
`HALT`. A store to `REQ_HEAD` does nothing by itself. A service point is:

1. deliver due timers and ready posted reads to the DPC ring (§8.12);
2. perform every queued request, in order (§8.2.3);
3. deliver due timers and ready posted reads again (so a timer that expired
   during a `SLEEP` is queued before the guest resumes).

The guest is stopped for the whole service point; the host may read and write
guest memory freely during it (`READ_DIRECT`, `POST_READ`, tube `PRESENT`)
and nowhere else. A YIELD in an executable without a window does nothing.

#### 8.3.2 Ending the program

There are two ways, and existing binaries use both:

- **`EXIT` request (opcode 0x09).** The host stores the request's `status`
  word into guest register **r1** and stops executing instructions after the
  current YIELD/HALT completes. A response `{0x09, 0, offset, status}` is
  written. Requests queued after the EXIT in the same batch are not
  performed. The `runtime/` C library's `exit()`/`_exit()` issue EXIT
  (after running atexit handlers and flushing streams) and then loop on
  YIELD.
- **`HALT` instruction.** The host first performs a service point (so queued
  requests, including an EXIT, are completed), then stops. Executables without
  a window and the self-hosted (stage08) libc end this way, with the exit
  status placed in r1 beforehand (stage08's `exit(n)` does `r1 = n; HALT`).

In both cases the **exit status of the program is the value of r1 when
execution stops**. The host process exits with that value; on a POSIX host
the observable process exit code is therefore `r1 & 0xFF`. Guest conventions:
`main`'s return value; `128 + signal` for a default signal action and for
`abort()` (128+6).

On stopping, the host releases all negotiated services (§8.13: terminal mode
restored; tube port file removed) and closes everything.

A fault ends with the fault status of 7.2 (139, 132 or 134), not r1. A
stop the host imposes itself (the reference interpreter's `-c` cycle
limit, a debugger) ends with r1. What is printed in either case is host
diagnostics (§8.16), not guest output.


### 8.4 Status and errno

#### 8.4.1 Response status values

| `status` | Name | Meaning |
|---|---|---|
| `0xFFFFFFFF` | `ERR` | failure; response `length` = errno (§8.4.2) |
| `0xFFFFFFFE` | `EINTR` | interrupted (only `SLEEP`); results still valid |
| `0xFFFFFFFD` | `EOF` | end of directory (`READDIR`), end of input (term `READ_KEY`/`READ_CHAR`) |
| anything else | | success; meaning per opcode (0 = OK, byte count, fd, …) |

The host puts errno `e` in `length` only if `0 < e < 4096`; otherwise it uses
`EIO`. Guest library behaviour (for reference): `runtime/` sets C `errno` to
`length` on ERR (to `EIO` if `length` is 0 or ≥ 4096) and to `EINTR` on
`0xFFFFFFFE`; success leaves `errno` alone. The stage01/stage08 assembly
helper sets `errno = EIO` on any ERR and `EINTR` on `0xFFFFFFFE`, ignoring
`length`.

**Quirk:** `GETCHAR` at end of input returns status `0xFFFFFFFF` with
length 0 (it is the C `EOF` value, not `STATUS_EOF`); existing binaries test
for exactly that value, so it stays. The request helper turns length 0 into
`EIO`; `runtime/`'s `getchar()` restores `errno` at end of input (since
2026-10-03; binaries linked before keep `EIO`).

Failures report the cause's errno. `EINVAL` is reserved for a malformed
request (a bad length or offset, an unknown flag or whence); a request the
host's policy refuses is `EPERM` (8.13.2).

#### 8.4.2 Errno numbering

Errno values in responses use **Linux numbering** (the generic/x86 table
below). The guest C libraries hard-code these numbers (`runtime/include/errno.h`,
`selfhost/stage08/include/errno.h`).

A host on another operating system translates its errno values to these
numbers (on macOS, for instance, `EAGAIN` is 35 and `ENOTEMPTY` 66; the guest
must see 11 and 39). A host error with no entry in the table is reported as
`EIO`. (The reference host does this through `common/s32_errno.h`; before
2026-10-03 it passed a non-Linux host's numbers through untranslated.)

Errno values the host produces itself: `EIO`, `ENOENT`, `EBADF`, `ENOMEM`,
`EINVAL`, `EMFILE`, `EAGAIN`, `EPROTONOSUPPORT`, `EAFNOSUPPORT`. Any other
value comes from the underlying host operation (open, read, write, close,
ftruncate, unlink, rename, mkdir, rmdir, access, chdir, opendir, closedir,
socket, bind, listen, connect, accept, shutdown, getsockname, fork, waitpid,
stat) and can be any errno that operation defines.

| No. | Name | No. | Name | No. | Name |
|---|---|---|---|---|---|
| 1 | EPERM | 20 | ENOTDIR | 92 | ENOPROTOOPT |
| 2 | ENOENT | 21 | EISDIR | 93 | EPROTONOSUPPORT |
| 3 | ESRCH | 22 | EINVAL | 94 | ESOCKTNOSUPPORT |
| 4 | EINTR | 23 | ENFILE | 95 | EOPNOTSUPP |
| 5 | EIO | 24 | EMFILE | 97 | EAFNOSUPPORT |
| 6 | ENXIO | 25 | ENOTTY | 98 | EADDRINUSE |
| 7 | E2BIG | 26 | ETXTBSY | 99 | EADDRNOTAVAIL |
| 8 | ENOEXEC | 27 | EFBIG | 100 | ENETDOWN |
| 9 | EBADF | 28 | ENOSPC | 101 | ENETUNREACH |
| 10 | ECHILD | 29 | ESPIPE | 102 | ENETRESET |
| 11 | EAGAIN (=EWOULDBLOCK) | 30 | EROFS | 103 | ECONNABORTED |
| 12 | ENOMEM | 31 | EMLINK | 104 | ECONNRESET |
| 13 | EACCES | 32 | EPIPE | 105 | ENOBUFS |
| 14 | EFAULT | 33 | EDOM | 106 | EISCONN |
| 15 | ENOTBLK | 34 | ERANGE | 107 | ENOTCONN |
| 16 | EBUSY | 35 | EDEADLK | 108 | ESHUTDOWN |
| 17 | EEXIST | 36 | ENAMETOOLONG | 110 | ETIMEDOUT |
| 18 | EXDEV | 38 | ENOSYS | 111 | ECONNREFUSED |
| 19 | ENODEV | 39 | ENOTEMPTY | 112 | EHOSTDOWN |
| | | 40 | ELOOP | 113 | EHOSTUNREACH |
| | | 75 | EOVERFLOW | 114 | EALREADY |
| | | 84 | EILSEQ | 115 | EINPROGRESS |
| | | 88 | ENOTSOCK | 116 | ESTALE |
| | | 89 | EDESTADDRREQ | 122 | EDQUOT |
| | | 90 | EMSGSIZE | 125 | ECANCELED |
| | | 91 | EPROTOTYPE | | |


### 8.5 File descriptors

The host keeps a table of 128 guest descriptors (0..127). Each slot is free,
a byte stream (file, pipe, terminal, socket) or a directory stream. At start:

| Guest fd | Host object |
|---|---|
| 0 | host standard input (not owned) |
| 1 | host standard output (not owned) |
| 2 | host standard error (not owned) |
| 3..127 | free |

`OPEN`, `SOCKET`, `ACCEPT` and `OPENDIR` take the **lowest free** slot.
Files, sockets and directories share one numbering. `CLOSE` of 0, 1 or 2
frees the slot without closing the host stream; the next `OPEN` may then
return 0, 1 or 2, and `WRITE`/`READ` on that number reach the new file.
`PUTCHAR`, `GETCHAR`, `FLUSH`, term output and `DEBUG` always use the host's
standard streams, whatever the table says.

Standard input is also reachable through `GETCHAR` and the term service. See
§8.16 (`S32_STDIN_PREFIX`) for a host option that prepends a file to it.

`GETCHAR`, `READ` on fd 0 and the term service's key reads all read the
host's standard input descriptor directly, one ordered byte stream, so a
program may mix them. (Before 2026-10-03 GETCHAR read through a host-side
buffered stream that could read ahead of the others.)


### 8.6 Core I/O (0x00–0x10)

#### 0x00 NOP
Request ignored. Response `status` 0.

#### 0x01 PUTCHAR
| | |
|---|---|
| Request | `length`, `status` ignored |
| Data in | 1 byte at `off` |
| Action | write the byte to host standard output, flush |
| Response | `status` 0 |
| Errors | none |

(No current guest library uses PUTCHAR; `runtime/` writes stdout with `WRITE`.)

#### 0x02 GETCHAR
| | |
|---|---|
| Request | `length`, `status` ignored |
| Action | read one byte from the host's standard input descriptor (§8.5) |
| Response | byte read: `status` 0, `length` 1; end of input or error: `status` `0xFFFFFFFF`, `length` 0 |
| Data out | the byte at `off` |

#### 0x03 WRITE, 0x43 SEND
| | |
|---|---|
| Request | `status` = guest fd, `length` = byte count n |
| Data in | n bytes at `off` |
| Action | one host `write(2)` of `min(n, CAP − off)` bytes |
| Response | `status` = `length` = bytes written (may be short) |
| Errors | fd not a byte stream: `EBADF`; n > CAP: `EINVAL`; write failure: errno |

A zero-length WRITE on an open fd succeeds with 0. Writing to a pipe or
socket whose reader has gone fails with `EPIPE`; the host ignores
`SIGPIPE`.

#### 0x04 READ, 0x44 RECV
| | |
|---|---|
| Request | `status` = guest fd, `length` = max bytes n |
| Action | one host `read(2)` of up to `min(n, CAP − off)` bytes (blocking) |
| Response | `status` = `length` = bytes read; **0 = end of file** |
| Data out | the bytes at `off` |
| Errors | `EBADF` (fd not a byte stream), `EINVAL` (n > CAP), read errno. n = 0 on an open fd: 0. |

#### 0x0C READ_DIRECT (optional)
| | |
|---|---|
| Request | `status` = guest fd, `length` = n, `offset` = **guest address** A |
| Action | one host `read(2)` of up to n bytes directly into guest memory `[A, A+n)` |
| Response | `status` = `length` = bytes read (0 = EOF); n = 0: `status` 0 without any check |
| Errors | `EINVAL` (host does not support it, fd invalid, range outside guest memory), read errno |

Support is optional. The reference interpreter and `slow32-fast` always fail
it with `EINVAL`; the DBT supports it. `runtime/` stdio tries READ_DIRECT
once and, after the first ERR, uses `READ` for the rest of the run, so both
behaviours are compatible with existing binaries. Implementation-defined:
the DBT does not refuse a destination inside the code or read-only segments;
no binary targets one.

#### 0x05 OPEN
| | |
|---|---|
| Request | `status` = SLOW-32 open flags (below), `length` = bytes of path including its NUL |
| Data in | path at `off`, `length` bytes; the host appends a NUL, so the guest's own NUL is optional |
| Action | host `open(path, flags, 0644)` (mode only with CREAT; subject to the host umask) |
| Response | `status` = new guest fd |
| Errors | `EINVAL` (`length` = 0, `length` > CAP, `off + length` > CAP, a flag bit outside `0x1F`); `EMFILE` (no free slot); open errno |

SLOW-32 open flags (`runtime/include/fcntl.h`):

| Bit | Name | Host meaning |
|---|---|---|
| `0x01` | READ (`O_RDONLY`) | |
| `0x02` | WRITE (`O_WRONLY`); `0x03` = `O_RDWR` | WRITE without READ = write-only; WRITE+READ = read-write; neither = read-only |
| `0x04` | APPEND | `O_APPEND` |
| `0x08` | CREAT | `O_CREAT`, mode 0644 |
| `0x10` | TRUNC | `O_TRUNC` |

There is no EXCL. `fopen` maps `"r"` → 0x01, `"w"` → 0x1A, `"a"` → 0x0E,
and `"+"` adds 0x03.


#### 0x06 CLOSE
| | |
|---|---|
| Request | `status` = guest fd |
| Action | close the host descriptor if the host opened it (not for 0–2); free the slot; then complete any posted read on that fd with 0 bytes (§8.12) |
| Response | `status` 0 |
| Errors | `EBADF` (fd ≥ 128, slot free, or slot is a directory stream); host close errno (slot is freed anyway) |

#### 0x07 SEEK
| | |
|---|---|
| Request | `status` = guest fd, `length` ≥ 8 |
| Data in | byte `off+0` = whence (0 SET, 1 CUR, 2 END); bytes `off+1..3` ignored; i32 at `off+4` = distance |
| Action | host `lseek(fd, distance, whence)` |
| Response | `status` = new position, truncated to 32 bits |
| Errors | `EBADF` (fd), `EINVAL` (`length` < 8, `off` > CAP−8, whence > 2), lseek errno (`ESPIPE` on a pipe) |

Positions ≥ 2³¹ appear negative to the guest library, which
reports them as errors.

#### 0x0D FTRUNCATE
| | |
|---|---|
| Request | `status` = guest fd, `length` ≥ 4 |
| Data in | u32 new length at `off` |
| Response | `status` 0 |
| Errors | `EBADF`, `EINVAL` (`length` < 4, `off` > CAP−4), ftruncate errno |

#### 0x0A STAT (and fstat)
| | |
|---|---|
| Request | path form: `status` = `0xFFFFFFFF`, `length` = path bytes incl. NUL; fd form: `status` = guest fd, `length` ignored |
| Data in | path form: path at `off`, `length` bytes; the host appends a NUL |
| Response | `status` 0, `length` 112 |
| Data out | `stat_result` (§8.6.1) at `off` (overwrites the path) |
| Errors | `EINVAL` (`CAP − off` < 112, path `length` = 0 or > `CAP − off`); fd form: `EBADF` if the guest fd is not open; stat/fstat errno (a missing file is `ENOENT`) |

The fd form stats the object the guest fd refers to (a byte stream or a
directory stream), through the descriptor table of 8.5. (Before 2026-10-03
the reference host called `fstat` on the host descriptor with the guest's
number, which was right only while the two numberings coincided, and
reported every failure as `EINVAL`.)

##### 8.6.1 `stat_result` (112 bytes, packed)

| Offset | Type | Field |
|---|---|---|
| 0 | u64 | st_dev |
| 8 | u64 | st_ino |
| 16 | u32 | st_mode (host `S_IF*` type bits and permission bits; the POSIX octal values `0170000` mask etc.) |
| 20 | u32 | st_nlink |
| 24 | u32 | st_uid |
| 28 | u32 | st_gid |
| 32 | u64 | st_rdev |
| 40 | u64 | st_size (negative clamped to 0) |
| 48 | u64 | st_blksize |
| 56 | u64 | st_blocks (512-byte units) |
| 64 | u64 | st_atime seconds |
| 72 | u32 | st_atime nanoseconds |
| 76 | u32 | 0 (pad) |
| 80 | u64 | st_mtime seconds |
| 88 | u32 | st_mtime nanoseconds |
| 92 | u32 | 0 |
| 96 | u64 | st_ctime seconds |
| 104 | u32 | st_ctime nanoseconds |
| 108 | u32 | 0 |

`st_dev`, `st_ino`, `st_uid`, `st_gid` are host values. The `S_IFMT` type
encoding is the traditional Unix one (`0100000` regular, `0040000`
directory, `0120000` symlink, `0020000` char device, `0010000` FIFO,
`0140000` socket, `0060000` block).

#### 0x0B FLUSH
Request words ignored (the guest sends an fd in `status`; the host does not
use it). Flushes the host's buffered standard output and standard error.
Response `status` 0. Never fails. (WRITE is unbuffered on the host, so this
matters only for host-buffered paths.)

#### 0x09 EXIT
See §8.3.2. `status` = exit status. Response `{0x09, 0, offset, status}`.
Never fails, never policy-gated.

#### 0x10 EXEC
Runs another SLOW-32 executable in a child host process and waits for it.

| | |
|---|---|
| Request | `length` = payload bytes (1..4095); `status` = `0xFFFFFFFF` to inherit the host's standard streams, or a guest fd whose host descriptor becomes the child's stdin, stdout and stderr |
| Data in | at `off`: `path\0arg1\0arg2\0…` (the child's argv[0] is `path`; arg1… are argv[1]…); the payload need not end in NUL |
| Action | child = the same emulator (resolved path of the host's own argv[0], else env `S32_EMU`), run as `emu -q [--deny LIST] [--allow LIST] path arg1 …` with the parent's policy lists; host blocks in waitpid |
| Response | `status` = child's exit code (0–255); 255 if the child died by a signal; 127 if the child could not be started or the given fd was not open |
| Errors | `EINVAL` (`length` 0 or ≥ 4096, `off + length` > CAP, path empty, path not ending in `.s32x` (case-insensitive)); stat errno (no such file); `EACCES` (exists but is not a regular file); `ENOENT` (emulator path unknown); fork/waitpid errno |

At most 11 arguments after the path are used; an empty string ends the list
early. The child inherits the host environment and the current directory (as
changed by `CHDIR`).


### 8.7 Filesystem metadata (0x20–0x2B)

For opcodes marked "path": `length` = path bytes, path at `off`; requires
`1 ≤ length ≤ CAP` and `off + length ≤ CAP` (else `EINVAL`); the host
appends a NUL after `length` bytes (so the NUL may or may not be included).
Success: `status` 0, `length` 0. Failure: host errno, except where noted.

| Opcode | Name | Request | Notes |
|---|---|---|---|
| 0x20 | UNLINK | path | host `unlink` |
| 0x21 | RENAME | `length` = total bytes; data `old\0new\0`; `status` = bytes of `old` **including** its NUL (= offset of `new`) | `EINVAL` if `status` = 0 or ≥ `length`. Host replaces byte `status−1` with NUL; `new` runs to the end of the payload. |
| 0x22 | MKDIR | path; `status` = mode | mode 0 means 0755; subject to umask |
| 0x23 | RMDIR | path | |
| 0x24 | LSTAT | path; `status` ignored (guests send `0xFFFFFFFF`) | Like STAT but does not follow a final symlink. Requires `CAP − off ≥ 112`. Result: `status` 0, `length` 112, `stat_result` at `off`. Failure: lstat errno (a missing file is `ENOENT`). |
| 0x25 | ACCESS | path; `status` = mode: 0 F_OK, 1 X_OK, 2 W_OK, 4 R_OK (OR-able) | host `access` |
| 0x26 | CHDIR | path | changes the host process's directory (affects all later relative paths, EXEC) |
| 0x27 | GETCWD | `length` = buffer size (1..CAP) | Writes the NUL-terminated directory at `off`, at most `min(length, CAP−off)` bytes. Response `status` = `length` = string length **including** NUL. Failure: getcwd errno (`ERANGE` if too small). |
| 0x28 | OPENDIR | path | Response `status` = guest fd of a directory stream (§8.5). `EMFILE` if no slot; opendir errno (or `ENOENT`). |
| 0x29 | READDIR | `status` = directory fd | Requires `off ≤ CAP − 272`. Entry: `status` 0, `length` 272, `dirent` at `off`. End: `status` `0xFFFFFFFD`, `length` 0. Not a directory stream → `EBADF`; read error → its errno. Entries include `.` and `..`, in host order. |
| 0x2A | CLOSEDIR | `status` = directory fd | `EBADF` if not a directory stream. Frees the slot. |
| 0x2B | REWINDDIR | `status` = directory fd | `EBADF` if not a directory stream. |

A directory fd is not a byte stream: READ/WRITE on it give `EBADF`, CLOSE
gives `EINVAL`.

##### 8.7.1 `dirent` (272 bytes, packed)

| Offset | Type | Field |
|---|---|---|
| 0 | u64 | d_ino (host value) |
| 8 | u32 | d_type: 0 unknown, 1 FIFO, 2 char dev, 4 directory, 6 block dev, 8 regular, 10 symlink, 12 socket |
| 12 | u32 | d_namlen (bytes, excluding NUL, at most 255) |
| 16 | 256 bytes | d_name, NUL-terminated, zero-filled after the NUL |


### 8.8 Time (0x30–0x35)

##### 8.8.1 `timepair64` (16 bytes)
| Offset | Type | Field |
|---|---|---|
| 0 | u32 | seconds, low word |
| 4 | u32 | seconds, high word |
| 8 | u32 | nanoseconds (0..999 999 999) |
| 12 | u32 | reserved (host writes 0) |

##### 8.8.2 `tzinfo` (16 bytes)
| Offset | Type | Field |
|---|---|---|
| 0 | i32 | seconds east of UTC (negative west) |
| 4 | u32 | 1 if daylight saving in effect, else 0 |
| 8 | 8 bytes | abbreviation, NUL-terminated (≤ 7 chars, zero-filled) |

| Opcode | Name | Request | Response / data | Errors |
|---|---|---|---|---|
| 0x30 | GETTIME | `length` ≥ 16 | host wall clock (UTC, POSIX epoch; negative clamped to 0) as `timepair64` at `off`; `status` 0, `length` 16 | `EINVAL` (`length` < 16, `off` > CAP−16, clock failure) |
| 0x31 | SLEEP | `length` ≥ 16; `timepair64` interval at `off` | Blocks the whole machine for the interval. Done: zeros written at `off`, `status` 0, `length` 16. Interrupted by a host signal: remaining time at `off`, `status` `0xFFFFFFFE`, `length` 16 | `EINVAL` (`length`, `off`, nanoseconds ≥ 10⁹, other host failure) |
| 0x35 | GETTZ | `length` ≥ 16; `timepair64` (a UTC instant) at `off` | `tzinfo` for that instant in the host's local time zone written over it at `off`; `status` 0, `length` 16. If no abbreviation is available, `"UTC"` | `EINVAL` (`length`, `off`) |
| 0x32 | TIMER_START | §8.12 | | |
| 0x33 | TIMER_CANCEL | §8.12 | | |
| 0x34 | POLL | §8.12 | | |

The local time zone is the host's (normally the host `TZ` setting).
`GETTIME`/`SLEEP` use the host's real clocks and are not deterministic.


### 8.9 Sockets (0x40–0x48)

IPv4 TCP only.

##### 8.9.1 `sockaddr` (8 bytes, packed) — not the POSIX layout
| Offset | Type | Field |
|---|---|---|
| 0 | u32 | IPv4 address as a number, little-endian (127.0.0.1 = `0x7F000001`) |
| 4 | u16 | port as a number, little-endian |
| 6 | u16 | family, must be 2 |

| Opcode | Name | Request | Response | Errors |
|---|---|---|---|---|
| 0x40 | SOCKET | `status` = family \| type<<8 \| protocol<<16 (bits 0–7, 8–15, 16–23) | `status` = new guest fd (a blocking TCP socket) | family ≠ 2: `EAFNOSUPPORT`; type ≠ 1 (stream): `EPROTONOSUPPORT`; protocol not 0 or 6: `EPROTONOSUPPORT`; no slot: `EMFILE`; socket errno |
| 0x46 | BIND | `status` = fd, `length` ≥ 8, `sockaddr` at `off` | `status` 0. The host sets `SO_REUSEADDR` first. | `EBADF`; `EINVAL` (`length` < 8, `off` > CAP−8); family ≠ 2: `EAFNOSUPPORT`; bind errno |
| 0x47 | LISTEN | `status` = fd, `length` = backlog | `status` 0. Backlog ≤ 0 (as i32) → 8; > 128 → 128. | `EBADF`, listen errno |
| 0x41 | CONNECT | as BIND | `status` 0 (blocks until connected) | as BIND, connect errno |
| 0x42 | ACCEPT | `status` = listening fd | blocks; `status` = new guest fd, `length` 8, peer `sockaddr` at `off` (not written if `off` > CAP−8) | `EBADF`, accept errno, `EMFILE` |
| 0x45 | SHUTDOWN | `status` = fd, `length` = how: 0 read, 1 write, 2 both | `status` 0 | `EBADF`, `EINVAL` (how > 2), shutdown errno |
| 0x48 | GETSOCKNAME | `status` = fd | `status` 0, `length` 8, local `sockaddr` at `off` (not written if `off` > CAP−8) | `EBADF`, getsockname errno |
| 0x43 | SEND | identical to WRITE (§8.6) | | |
| 0x44 | RECV | identical to READ (§8.6) | | |

Socket fds are closed with CLOSE. There is no DNS, UDP or Unix-domain
support. The guest library converts POSIX `sockaddr_in` (network byte order)
to and from §8.9.1.


### 8.10 Host environment (0x60–0x64)

##### 8.10.1 Info struct (16 bytes), used by ARGS_INFO and ENVP_INFO
| Offset | Type | Field |
|---|---|---|
| 0 | u32 | count (argc / envc) |
| 4 | u32 | total bytes of the blob |
| 8 | u32 | flags (0) |
| 12 | u32 | reserved (0) |

The **argument blob** is the guest's argv strings, each followed by NUL,
concatenated. argv[0] is the executable's path exactly as given to the host
on its command line; argv[1…] are the guest arguments. (Reference host: in
`emu [opts] prog.s32x [--] args…` the first `--` right after the program path
is removed; the emulator's own options are not passed.) The host refuses to
start a program whose blob would exceed 64 KB. The **environment blob** is
the host process's environment, `NAME=VALUE\0` per variable, in host order;
if it would exceed 128 KB the guest gets an empty environment.

| Opcode | Name | Request | Response / data | Errors |
|---|---|---|---|---|
| 0x60 | ARGS_INFO | `length` ≥ 16 | info struct at `off`; `status` 0, `length` 16 | `EINVAL` (`length` < 16, `off` > CAP−16) |
| 0x61 | ARGS_DATA | `length` = bytes wanted (≤ CAP); **`status` = byte offset into the blob** | copies `min(length, total − status)` blob bytes starting at blob offset `status` to `off`; `status` 0, `length` = bytes copied. `length` 0: `status` 0 with no checks | `EINVAL` (`length` > CAP, `off + length` > CAP, `status` > total) |
| 0x62 | ENVP_INFO | as ARGS_INFO | | |
| 0x63 | ENVP_DATA | as ARGS_DATA, over the environment blob | | |
| 0x64 | GETENV | `length` = bytes of name (NUL optional); name at `off` | value (not NUL-terminated) written at `off`, truncated to `CAP − off`; `status` = `length` = value bytes (0 for an empty value) | `EINVAL` (`length` 0 or > CAP, `off + length` > CAP); `ENOENT` (variable not set) |

GETENV looks the name up in the host's live environment (the same contents
as the environment blob). Guests fetch blobs in chunks of at most CAP
(the runtime) or 32 KB (stage08) bytes.


### 8.11 Unknown opcodes

Any opcode not listed in §8.6–§8.10, §8.12, §8.13 and not inside an active
negotiated range (§8.13) gets `status` ERR, `length` = `EINVAL`. This includes
0x08 (once BRK), 0x0F, 0x11–0x1F, 0x2C–0x2F, 0x36–0x3F, 0x49–0x5F,
0x65–0x7F, unallocated 0x80–0xEF, 0xF5–0xFF.


### 8.12 Timers, posted reads, POLL and the DPC ring

#### 8.12.1 DPC ring

Host → guest queue of 64 descriptors at window offset `0x0800`, head at
`0x0010` (host advances), tail at `0x0014` (guest advances), indices mod 64,
full at 63 entries. The host writes entries only at service points (§8.3.1),
inside POLL, and inside CLOSE. The guest consumes an entry by reading it and
storing `DPC_TAIL = (DPC_TAIL + 1) mod 64`; it may do so at any time.

| Kind | Entry {opcode, length, offset, status} |
|---|---|
| timer fired | `{0x32, 0, timer id, cookie}` |
| posted read completed | `{0x0E, bytes read, destination guest address, cookie}` |
| fd readiness (POLL) | `{0x34, 0, guest fd, why}` with why bits: `1` IN (readable or at end of file), `2` HUP, `4` ERR, `8` NVAL (not an open guest fd) |

#### 8.12.2 Timers

At most 8 one-shot timers (ids 0..7) are armed at once.

**0x32 TIMER_START.** Request: `length` ≥ 16, `timepair64` interval at `off`,
`status` = cookie. The host arms the lowest free id with deadline
= now + interval on a monotonic clock. Response `status` = id.
Errors: `EINVAL` (`length` < 16, `off` > CAP−16, nanoseconds ≥ 10⁹);
`EAGAIN` (all 8 armed).

**0x33 TIMER_CANCEL.** Request `status` = id. Disarms it; a cancelled timer
never queues. Response `status` 0. Error `EINVAL` (id ≥ 8 or not armed —
including one that already fired).

Delivery: at each delivery step every armed timer whose deadline has passed
is queued, earliest deadline first; its id becomes free when its entry is
queued. If the ring is full the timer stays armed and is queued at a later
delivery step.

#### 8.12.3 0x0E POST_READ

A read whose completion arrives as a DPC entry, not in the response.

| | |
|---|---|
| Request | `status` = guest fd, `length` = max bytes n (> 0), `offset` = **guest address** D of the destination |
| Data in | u32 cookie at data-buffer offset **0** (always 0, not `offset`) |
| Response | `status` 0 = accepted |
| Errors | `EBADF` (fd not a byte stream), `EINVAL` (n = 0, D below the end of the code segment), `EAGAIN` (see below) |

If the fd is readable now (host poll reports readable, hang-up or error) the
host completes it immediately: it reads without blocking, in a loop, until n
bytes, end of file, a short read, would-block, or an error, writing into guest
memory at D; then queues `{0x0E, total, D, cookie}`. Errors are not reported;
`total` is what was read before them (possibly 0). If the DPC ring is full at
that moment: `EAGAIN`.

Otherwise the read is kept pending in one of 8 slots (`EAGAIN` if a pending
read already has destination D, or no slot is free) and completed the same
way at the first delivery step (§8.3.1, POLL, CLOSE) at which the fd is
readable and the ring has room. A pending read whose fd was closed completes
with 0 bytes.

#### 8.12.4 0x34 POLL

Sleeps until the DPC ring is non-empty.

| | |
|---|---|
| Request | `length` = 4 × k, k ≤ 8; k u32 guest fds at `off` (k = 0: no fds) |
| Response | `status` = number of entries now in the DPC ring |
| Errors | `EINVAL` (`length` not a multiple of 4, k > 8, `off` > CAP − `length`); `EAGAIN` when the ring is empty and nothing can arrive (no armed timer, no pending post, k = 0) |

Algorithm:

1. For each named fd that is not an open byte stream, queue `{0x34, 0, fd, 8}`
   (dropped silently if the ring is full).
2. Deliver timers and posts.
3. While the ring is empty: if no timer is armed, no post pending and k = 0,
   fail with `EAGAIN`. Otherwise wait (host `poll`) on the fds of pending posts
   and the named fds for readability, with a timeout until the earliest timer
   deadline (none if no timer; plain sleep if there are no fds). Then, for each
   named fd **not** owned by a pending post, queue `{0x34, 0, fd, why}` if
   `why` ≠ 0. Then deliver timers and posts.
4. Respond with the ring's entry count.

A named fd that a pending POST_READ is reading never produces a readiness
entry; the post completion is the wake-up.


### 8.13 Service negotiation (0xF0–0xF4)

Negotiated services are reached through opcodes 0x80–0xEF that the host
allocates. Two services exist: `"term"` (15 opcodes, version 1) and `"tube"`
(16 opcodes, version 1).

#### 8.13.1 Wire format (as implemented)

All five opcodes: request `length` = bytes of the service name including NUL
(1..32; the host appends a NUL, so the guest's own NUL is optional), name at `off`,
`off + length ≤ CAP`, else `EINVAL`. The request `status` word is **ignored**
(guests send 0). Results are returned **in the data buffer at `off`**; the
response `status` is 0 except as noted.

**0xF0 SVC_REQUEST.** The host checks, in this order:

| Check | Data at `off` | Response `length` |
|---|---|---|
| policy denies the name | u32 `1` (DENIED) | 4 |
| a session with that name is active | u32 `3` (CONFLICT) | 4 |
| name is not a built-in service | u32 `2` (UNKNOWN) | 4 |
| 16 sessions are active | u32 `4` (LIMIT) | 4 |
| no free range of opcode-count opcodes below 0xF0 | u32 `4` (LIMIT) | 4 |
| otherwise: grant | u32 ×4: `[0 (OK), base, opcode_count, version]` | 16 |

The **host picks the base**: the lowest base from 0x80 whose range
`base .. base+count−1` overlaps no active session's; a released range is
free again. Opcodes in the range then route to the session (sub-opcode =
opcode − base). Code 5 (VERSION_ERR) is defined but never produced.
(Before 2026-10-03 a request for an active service granted a second
session, and ranges and session slots were never reused.)

**0xF1 SVC_RELEASE.** Destroys the oldest active session with that name
(service cleanup runs); its opcodes then fail with `EINVAL`.
Response `status` 0; `ENOENT` if none active.

**0xF2 SVC_QUERY.** Writes u32 at `off`: 2 UNKNOWN if not built in, else 1
DENIED if policy denies, else 0 OK. Response `length` 4. Does not consider
active sessions.

**0xF3 SVC_LIST.** No name needed (`length` not checked). Writes
`"term\0tube\0"` at `off` (names that do not fit are left out); response
`length` = bytes written (10). Policy is not applied.

**0xF4 SVC_VERSION.** Response `status` = 1 (protocol version), `length` 0.

#### 8.13.2 Policy

The host holds a deny list and an allow list of names (host options, §8.16).
A name is denied if it is on the deny list; otherwise, if the allow list is
non-empty, allowed only if on it; otherwise allowed. By default everything is
allowed.

Fixed opcodes are subject to the same policy under these names; a denied
request fails with `EPERM`:

| Name | Opcodes |
|---|---|
| `fs` | 0x03–0x07 (but WRITE and READ on fds 0–2 are never gated), 0x0A, 0x0C, 0x0D, 0x0E, 0x20–0x2B |
| `time` | 0x30–0x3F |
| `exec` | 0x10 |
| `net` | 0x40–0x4F |
| `env` | 0x62–0x6F (the environment; not the arguments) |
| (never gated) | 0x00, 0x01, 0x02, 0x09, 0x0B, 0x60, 0x61, 0xF0–0xF4, negotiated opcodes |

The standard streams and the command line belong to the program, so
denying `fs` or `env` leaves `printf` and `argv` working. A non-empty allow
list denies every name not on it, including the fixed ones.


### 8.14 Term service (`"term"`, 15 opcodes)

Sub-opcodes (opcode = base + n). Requests carry arguments in `status`
unless noted; responses are `status` 0, `length` 0 unless noted.

| n | Name | Request | Effect / response |
|---|---|---|---|
| 0 | SET_MODE | `status` ≠ 0 raw, 0 cooked | `EINVAL` if host stdin is not a terminal. Raw: from the termios saved at session creation, clear ECHO, ICANON, ISIG, IEXTEN, IXON, ICRNL, BRKINT, INPCK, ISTRIP, OPOST; VMIN 1, VTIME 0; applied with discard of pending input. Cooked: restore the saved termios (also with discard). |
| 1 | GET_SIZE | `off` ≤ CAP−8 | Host terminal size now (of host stdout; 24×80 if unavailable): u32 rows, u32 cols at `off`; `length` 8. `EINVAL` if `off` > CAP−8. |
| 2 | MOVE_CURSOR | `status` = row<<16 \| col, **1-based**, 16 bits each | Outside an update: emits `ESC [ row ; col H` (decimal, unclamped). Shadow cursor := (row−1, col−1). |
| 3 | CLEAR | `status` = 0 screen, 1 to end of line, 2 to end of screen (other = 0) | Outside an update emits `ESC[2J ESC[H` / `ESC[K` / `ESC[J`; inside, records it (§8.14.3). Shadow clear (§8.14.2). |
| 4 | SET_ATTR | `status` = SGR number | Outside an update emits `ESC [ status m` (decimal). Current attribute := status (low 8 bits kept in cells). Guests use 0 normal, 1 bold, 7 reverse. |
| 5 | READ_KEY | | Blocks for one byte of input (§8.14.4). `status` = byte, `length` 1, byte also at `off`. End of input: `status` `0xFFFFFFFD`. |
| 6 | KEY_AVAIL | | `status` 1 if a byte can be read without blocking (a pushed-back byte, unread prefix bytes, or host stdin polls readable), else 0. Never blocks. |
| 7 | SET_COLOR | `status` = fg<<8 \| bg (8 bits each; ANSI 0–7) | Outside an update emits `ESC [ 3fg ; 4bg m` (decimal, unclamped). Current colours := fg, bg. |
| 8 | PUTC | `status` low 8 bits = one byte | Outside an update: the byte to stdout. Always fed to the shadow (§8.14.2). |
| 9 | PUTS | `length` = n, bytes at `off` | `min(n, CAP−off)` bytes; outside an update written to stdout; each fed to the shadow. |
| 10 | SAVE_SCREEN | | Pushes shadow cells, cursor, attribute, colours (stack depth 8; `EINVAL` when full). No output. |
| 11 | RESTORE_SCREEN | | Pops and repaints (§8.14.3). Inside an update it restores the shadow only and emits nothing; END_UPDATE paints the difference. `EINVAL` if the stack is empty. |
| 12 | BEGIN_UPDATE | | Snapshots the shadow, cursor, attribute and colours; starts an update (output suppressed). `EINVAL` if already in one. |
| 13 | END_UPDATE | | Ends the update and paints the difference (§8.14.3). `EINVAL` if not in one. |
| 14 | READ_CHAR | | Blocks for one UTF-8 character (§8.14.4). `status` = code point, `length` 0. End of input: `status` `0xFFFFFFFD`. |

Unused sub-opcodes cannot occur (the range is exactly 15). All term output
goes to host standard output and is flushed after every request; `ESC` is
byte 0x1B. Session cleanup (release or exit) restores the saved termios if
raw mode is on; it emits nothing.

#### 8.14.1 The shadow model

The session keeps a **shadow screen**: R × C cells, where R, C are the host
terminal size at SVC_REQUEST time, each clamped to 256 (24 × 80 if the size is
unavailable or 0). The shadow never scrolls and never resizes. Each cell
holds: a base code point; up to 7 further code points ("marks") of its
grapheme cluster; attribute (8 bits); fg, bg (8 bits each); and a width kind:
NARROW, WIDE (first of two cells), or TAIL (second cell of a WIDE). Initial
cell: `' '`, no marks, attr 0, fg 7, bg 0, NARROW. Initial cursor (0,0),
attr 0, fg 7, bg 0. A "blank" cell is exactly that initial value. Cells
outside 0..R−1 × 0..C−1 do not exist; writes to them are discarded, but the
cursor still moves (it may go past the last row or be negative).

The session also keeps a UTF-8 decoder (persisting across PUTC/PUTS calls)
and the cluster being built plus the position of the last cell written
("last cell"; initially none).

#### 8.14.2 Feeding output to the shadow

**Decoding.** Bytes are decoded as strict UTF-8: well-formed sequences per
Unicode §8.3.9 (no overlongs, no surrogates, nothing above U+10FFFF); each
maximal subpart of an ill-formed sequence becomes one U+FFFD, and the byte
that cut a sequence short is decoded again as the start of the next.

**Code point dispatch.**
- U+000A: cluster reset; row += 1; col := 0.
- U+000D: cluster reset; col := 0.
- U+0009: cluster reset; col := (col + 8) rounded down to a multiple of 8;
  if col ≥ C: col := 0, row += 1.
- Other U+0000–U+001F and U+007F: ignored (they still reach the terminal
  outside an update). Note: the bytes of an escape sequence after the ESC are
  printable and enter the shadow as characters.
- Anything else: a character, below.

"Cluster reset" (also done by MOVE_CURSOR and CLEAR) discards the cluster
being built and forgets the last cell.

**Clusters and widths.** Extended grapheme clusters follow UAX #29 for
Unicode 16.0, rules GB3–GB13 evaluated in that order, **without GB9c**, with
the cluster state carried code point by code point (GB11 tracks
"ExtPict Extend* ZWJ"; GB12/13 pair regional indicators only when adjacent).
Code point width: 0 for General_Category Mn, Me, Cf; otherwise 2 for
East_Asian_Width W or F; otherwise 1 (Unicode 16.0 data; unlisted code points
1). Cluster width: 2 if it starts with a regional indicator and has ≥ 2 code
points; else 2 if it contains U+FE0F and its widest code point is < 2; else
the widest code point's width. A cluster is "lone" if its first code point
has Grapheme_Cluster_Break Extend or ZWJ.

**Character algorithm** (cp is the code point; step the cluster state with
cp; brk = cp starts a new cluster; w = width of the current cluster):

```
if not brk, or (w == 0 and cluster not lone):
    if no last cell: discard cp; done
    append cp to the last cell's marks (dropped if 7 already)
    if not brk and w == 2 and last cell is NARROW and last_col+1 < C:
        blank_half(last_row, last_col+1)
        cell(last_row, last_col+1) := copy of last cell with ch ' ', no marks, kind TAIL
        last cell kind := WIDE
        if cursor == (last_row, last_col+1): col += 1; if col >= C: col := 0, row += 1
    done
base := cp
if cluster lone: base := ' ', w := 1            (cp becomes mark[0])
if w == 2 and col+1 >= C: col := 0; row += 1    (wrap before a wide char)
for k in 0..w-1: blank_half(row, col+k)
last cell := (row, col)  (none if that cell does not exist)
for k in 0..w-1 (existing cells only):
    cell(row, col+k) := { ch: base if k==0 else ' ', marks: [cp] if k==0 and base != cp else [],
                          kind: NARROW if w==1, else WIDE (k==0) / TAIL (k==1),
                          attr, fg, bg := current }
col += w; if col >= C: col := 0; row += 1
```

`blank_half(r, c)`: if cell (r,c) is TAIL, the cell to its left becomes
`' '`, no marks, NARROW (attributes kept); if it is WIDE, the cell to its right
does likewise.

**Shadow clear.** Cluster reset, then: mode 1 blanks from the cursor to the
end of its row; mode 2 from the cursor to the end of the screen; mode 0 (and
any other value) blanks everything and moves the cursor to (0,0). Ranges are
computed on the linear index `row*C + col` and clipped to the screen. If the
first blanked cell is a TAIL, its WIDE head is blanked too. (Blank = the
initial cell value, so attr/fg/bg reset to 0/7/0.)

#### 8.14.3 Repaint algorithms (exact output bytes)

Notation: `CUP(r,c)` = `ESC [ r ; c H` with decimal 1-based numbers;
`SGR(a)` = `ESC [ a m`; `COL(f,b)` = `ESC [ 3f ; 4b m` (f, b decimal);
`EMIT(cell)` = nothing for a TAIL, else the UTF-8 of the base code point
followed by the UTF-8 of each mark (surrogates/out-of-range as U+FFFD).

**Recorded clears.** During an update each CLEAR is recorded as
(mode, cursor row, cursor col at that moment), up to 8; a 9th collapses the
record to a single full clear (mode 0), after which recording continues.

**END_UPDATE:**
```
(oa, of, ob) := attr, fg, bg at BEGIN_UPDATE; (orow, ocol) := cursor at BEGIN_UPDATE
for each recorded clear (m, r, c) in order:
    if m == 1 or m == 2: output CUP(r+1, c+1) then "ESC[K" (m=1) or "ESC[J" (m=2);
                         blank the snapshot from index r*C+c to the end of row r (m=1) or screen (m=2);
                         (orow, ocol) := (r, c)
    else:                output "ESC[2J" "ESC[H"; blank the whole snapshot; (orow, ocol) := (0, 0)
for r in 0..R-1:
  c := 0
  while c < C:
    cur := cell(r,c); prev := snapshot(r,c)
    if cur equals prev in ch, all 7 marks, attr, fg, bg and kind: c += 1; continue
    if cur is TAIL:
        if c == 0: c += 1; continue
        c := c-1; cur := cell(r,c)              (repaint the whole wide character)
    if (r,c) != (orow,ocol): output CUP(r+1,c+1); (orow,ocol) := (r,c)
    if cur.attr != oa: output SGR(cur.attr); oa := cur.attr; if cur.attr == 0: (of, ob) := (7, 0)
    if (cur.fg, cur.bg) != (of, ob): output COL(cur.fg, cur.bg); (of, ob) := (cur.fg, cur.bg)
    output EMIT(cur)
    ocol += 2 if cur is WIDE else 1
    if cur is WIDE: c += 1
    if ocol >= C: ocol := 0; orow += 1
    c += 1
if current attr != oa: output SGR(current attr)
if (current fg, bg) != (of, ob): output COL(current fg, current bg)
output CUP(cursor row+1, cursor col+1); flush
```

**RESTORE_SCREEN** outside an update (S = popped save; PR = min(S.R, R), PC = min(S.C, C)). Inside an update only the `shadow :=` and `cursor, attr, fg, bg :=` steps happen, with no output:
```
output "ESC[0m" "ESC[2J" "ESC[H"
(pa, pf, pb) := (0, 7, 0)
for r in 0..PR-1:
    output CUP(r+1, 1); lw := -1
    for c in 0..PC-1:
        x := S.cell(r,c)
        if x.ch == ' ' and no marks and x.attr == 0 and x.fg == 7 and x.bg == 0: continue
        if c != lw+1: output CUP(r+1, c+1)
        if x.attr != pa: output SGR(x.attr); pa := x.attr
        if (x.fg, x.bg) != (pf, pb): output COL(x.fg, x.bg); (pf, pb) := (x.fg, x.bg)
        if x is TAIL: continue
        output EMIT(x); lw := c+1 if x is WIDE else c
shadow := S cells (if sizes differ: blank shadow, then copy the PR×PC overlap)
cursor, attr, fg, bg := S's
output SGR(attr), COL(fg, bg), CUP(row+1, col+1); flush
```
Note RESTORE does not reset `pf, pb` on `SGR(0)`, unlike END_UPDATE.

What is left as presentation: how the physical terminal renders these bytes,
and the divergence between shadow and terminal that follows from it (the
shadow does not scroll, does not interpret escape sequences the guest writes
itself, treats LF as CR+LF even when raw mode has turned off output
processing, and ignores that `SGR(0)` outside an update resets the terminal's
colours). The algorithms above are complete for the bytes the host emits;
the only data not reproduced here are the Unicode 16.0 property tables named
in §8.14.2.

#### 8.14.4 Keyboard input

Key bytes come, in order of preference: a byte pushed back by READ_CHAR; the
stdin prefix file (§8.16); host stdin read one byte at a time with `read(2)`.
READ_CHAR decodes as §8.14.2: it reads bytes until a code point completes; if a
byte cuts a sequence short it returns U+FFFD and keeps that byte for the next
read; at end of input inside a sequence it returns U+FFFD; at end of input
otherwise `0xFFFFFFFD`. The term service's input is separate from tube key
events (§8.15).


### 8.15 Tube service (`"tube"`, 16 opcodes)

`docs/TUBE.md` (v0.2) is accurate for the guest-visible surface except for
the corrections below; read it with them applied.

1. **Negotiation.** As §8.13: the reply is the 16-byte blob at `off`;
   `opcode_count` 16. A second SVC_REQUEST for `"tube"` while one is active
   is CONFLICT, which enforces TUBE.md's "one tube session per guest".
2. **INFO bit 8** ("viewer attached") is also set whenever the
   `S32_TUBE_DUMP` journal directory is active. STATUS bit 31 reflects a real
   viewer connection only.
3. **OPEN does not read guest memory.** It only records the addresses; an
   unreadable address fails the next PRESENT, not OPEN (§8.1 "Guest-memory walk"
   says otherwise).
4. **fb OPEN limits:** width 16..640, height 16..480, format 1, palette
   address 4-byte aligned; the pixel address is not alignment-checked.
5. **ppu PRESENT reads fixed-size tables:** register block 64 bytes, the
   **whole** pattern table (1024 tiles × 32 = 32768 bytes) every time,
   nametable `nt_w × nt_h × 2` bytes, palettes 512 bytes, OAM 1024 bytes. All
   must be readable (not in the code segment) or PRESENT fails with `EINVAL`.
   `nt_w`/`nt_h` of 0 or > 128 → `EINVAL`.
6. **ppu background blend:** a background tile pixel with value ≠ 0 blends
   over `bg_color` with `a = palette.alpha` (no multiplier); sprites use
   `a = (sprite.alpha × palette.alpha) / 255`. Both use
   `out = (src × a + dst × (255 − a)) / 255` per channel, integer.
7. **CLOSE also empties the key queue**; the `S32_TUBE_KEYS` file is
   (re)loaded at every successful OPEN.
8. **Headless behaviour:** OPEN always tries to listen on 127.0.0.1 port 0
   and writes the port file (`tube.port`, or `S32_TUBE_PORT`) when listening
   succeeds, viewer or not; a port file is therefore normally written even in
   a headless run. If the port file cannot be created, the listener is closed
   (no viewer possible); OPEN still succeeds.
9. **Additional errno:** PRESENT and OPEN can fail with `ENOMEM`.
10. Every tube opcode, including reserved ones, first services the viewer
    socket (accept, receive).
11. The frame counter (STATUS bits 23:0) counts successful PRESENTs over the
    whole session and is not reset by CLOSE.
12. Viewer side (not guest-visible): a received frame whose `length` field is
    < 4 or > 16 drops the viewer; `KEYE` frames with `length` ≥ 8 carry one
    event, extra bytes ignored.


### 8.16 Host options (not guest protocol)

These change what a guest observes but are chosen by whoever runs the host.

| Option | Effect |
|---|---|
| `--deny LIST`, `--allow LIST` (before the program path) | §8.13.2 policy; comma-separated names, up to 16 each. Passed on to EXEC children. |
| `-q` | suppress the host's banner/statistics (EXEC children always get it) |
| `S32_STDIN_PREFIX=FILE` | bytes of FILE are served first on host stdin (READ on host fd 0, READ_DIRECT, GETCHAR, term key reads, POST_READ data, KEY_AVAIL, POLL readiness of fd 0), then real stdin |
| `S32_FAULT=OP:N:ERR[,…]` (≤ 16) | fail the Nth request of kind OP (OPEN, CLOSE, READ, WRITE, SEEK, STAT, FLUSH, READ_DIRECT, FTRUNCATE, UNLINK, RENAME, ACCESS, LSTAT) with errno ERR (number or name); N 0 = every one; requests on fds 0–2 are not counted |
| `S32_MMIO_TRACE` | request tracing on host stderr |
| `S32_EMU` | emulator used by EXEC if the host's own path is unknown |
| `S32_TUBE_KEYS`, `S32_TUBE_DUMP`, `S32_TUBE_DUMP_FULL`, `S32_TUBE_PORT` | tube key injection, headless journal, port-file path (`docs/TUBE.md` sections 2, 6, 7) |
| `TZ`, umask, current directory, environment | host process state seen through GETTZ, OPEN/MKDIR modes, relative paths, ENVP/GETENV |

Host diagnostics are not guest output and a host need not reproduce them.
The reference hosts print a banner and statistics on standard output unless
`-q` (the reference interpreter's `HALT at PC=…` line among them).

---

## 9. Conformance

A machine conforms if, for every executable, it produces the same
standard output and standard error bytes and the same exit status as the
reference, except where this document says **Unspecified**, and except
for the host's own lines (7.3) and the wording of fault diagnostics
(7.2).

The tree checks this in two ways: `regression/run-differential.sh` runs
104 executables on every engine and compares them with the reference
(`SLOW32_FAST=<your engine>` substitutes an engine), and
`regression/run-kit-differential.sh` does the same for a corpus built by
the self-hosted compiler, whose instruction choices differ.

---

## Appendix A. Known errors in the older documents

Statements in other documents under `docs/` that contradict this
specification, found while writing it (2026-10-03).  Each of those
documents carries a note pointing here.

Specific statements in the existing documents that are wrong (as of this
writing).

**docs/mmio/ring-design.md**
- "Memory Layout": all addresses are printed absolute at `0x10000000`. The
  window is at the header's `mmio_base` (placed by the linker after the heap);
  offsets are relative to it.
- "Memory Layout" and "Implementation Notes": data buffer "56KB" and
  "8KB for rings". It is 48 KB at 0x4000; rings and registers occupy
  0x0000–0x3FFF (16 KB).
- The layout omits the DPC registers (0x10, 0x14) and DPC ring (0x800).
- "Linux Syscall Correspondence" table: errors are not returned as `-errno`;
  they are `status = 0xFFFFFFFF` with `length = errno`. PUTCHAR always writes
  host stdout and returns 0. GETCHAR returns `status` 0, `length` 1 (EOF:
  `status 0xFFFFFFFF`, `length` 0). OPEN takes SLOW-32 flags (0x01 READ, 0x02
  WRITE, 0x04 APPEND, 0x08 CREAT, 0x10 TRUNC), not host `O_*`. SEEK's payload
  is whence byte at +0 and i32 distance at +4. FLUSH ignores the fd and only
  flushes host stdio; it never fails. TIMER_START/CANCEL/POLL are implemented
  (DPC ring), not "future"/"HP ring". Sockets do not "follow Linux socket
  argument ordering"; they use packed formats (status = family|type<<8|proto<<16,
  8-byte sockaddr).
- The note "Until errno plumbing lands …" is obsolete.
- "Opcodes" lists `OP_BRK 0x08`; it is removed and returns `EINVAL`.
- "Operation Flow" and both examples omit YIELD: the host services requests
  only at YIELD/HALT, so the example wait loops spin forever.

**docs/mmio/opcode-map.md**
- Range table, `0x80–0xEF`: "Guest-picked bases" — the host picks bases,
  the lowest free range from 0x80.
- Range table, `0x60–0x7F` "Host environment": only 0x60–0x64 exist; the
  `env` policy name covers 0x62–0x6F only (ARGS_INFO/ARGS_DATA are never
  gated).
- GETTIME, SLEEP, STAT: "errors clear `resp.length`" — on ERR `length` is the
  errno.
- SLEEP: "`length` must be 16" — any `length` ≥ 16 is accepted. "We still lack
  a global `errno`" is obsolete.
- STAT: omits the errors (8.6: `EBADF` for a closed fd, otherwise the stat
  errno).
- EXEC: omits the `.s32x` suffix requirement, the 4095-byte payload limit,
  the 11-argument limit, and the results 127 (could not start) and 255
  (signal).
- POLL row: NVAL entries are dropped if the ring is full (not stated).

**docs/SERVICE_NEGOTIATION.md**
- Design principle 5 "Guest-driven addressing", the SVC_REQUEST flow diagram
  (`word3 = 0x80 (desired base)`), "Negotiation Format (Resolved)" and the
  "Service addressing" decision: the request `status` is ignored and the host
  chooses the base.
- The response is not in descriptor words (`word1 = opcode_count, word2 =
  version, word3 = SVC status`); it is a blob in the data buffer
  `[result, base, count, version]` (16 bytes on grant, 4 bytes otherwise) with
  descriptor `status` 0.
- "opcode_count (e.g. 8 — so term is 0x80-0x87)": term has 15 opcodes, tube 16.
- Response codes: SVC_OK is not "at requested opcode base"; SVC_VERSION_ERR
  is never returned.
- "Versioning (Resolved)": the guest cannot request a minimum version.
- "Host Policy": `--sandbox`, `--sandbox-off` and the policy file do not
  exist; options must precede the program path; there is no default-deny
  mode.
- "Session Lifecycle": cleanup does not flush output buffers; term cleanup
  only restores termios.
- Term opcode table: MOVE_CURSOR packs `row<<16 | col`, 1-based;
  SET_COLOR packs `fg<<8 | bg`; SET_ATTR is a raw SGR number; READ_KEY/
  READ_CHAR return `0xFFFFFFFD` at end of input. The "Guest Library" listing
  does not match `runtime/include/term.h` (`term_clear(int mode)`,
  `term_set_attr`, no `term_clear_eol`/`term_bold`/`term_reverse`).

**docs/MMIO_CONSOLE.md**
- "Overview" and "Memory Map": `0x10000000` absolute; the window is at
  `mmio_base`.
- "Memory Map" and "Data Buffer": 56KB — it is 48 KB.
- The assembly example uses absolute addresses (`LUI r5, 0x10000`), which are
  right only for a binary whose `mmio_base` happens to be `0x10000000`.
- `runtime/console_io.s`, `tests/test_mmio_putchar.s`,
  `tests/test_console_hello.s` do not exist; `.o23s` is not the object
  extension (`.s32o`).
- "Input is line-buffered (waits for newline)" depends on the host terminal
  mode, not on SLOW-32.

**docs/MMIO_STATUS.md**
- "Memory Layout: MMIO at `0x10000000`" — the linker places the window after
  the heap (4 KB aligned) and records it in the header.
- "Queue Contract Snapshot": the host does not return "negative errno codes";
  guests do not "TRAP after enqueuing work" (there is no TRAP; they YIELD).
- `tests/test_mmio_simple.s` does not exist.

**docs/TUBE.md**
- §1 "Guest-memory walk": OPEN does not read guest RAM.
- §2 Errors: "`PRESENT` / `OPEN` address unmapped" — only PRESENT can fail
  that way; ENOMEM is also possible.
- §2 INFO: bit 8 is also set by `S32_TUBE_DUMP`.
- §2 Errors, last paragraph: "Headless … no `tube.port` is written" — the
  port file is written whenever listening succeeds, headless or not.
- §2 Input events: CLOSE empties the queue; the key file is reloaded at each
  OPEN.
- §4: width/height minimum is 16 (not stated); palette address must be
  4-aligned.
- §5: PRESENT always reads the full 32 KB pattern table; background tiles
  blend with `a = palette.alpha`.
