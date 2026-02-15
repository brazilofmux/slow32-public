# Stage 2: Forth-Hosted SLOW-32 Assembler

> **V2 cross-reference:** This is **Stage 1** in [BOOTSTRAP-V2.md](BOOTSTRAP-V2.md) (Layer B: Forth Self-Hosting).

## Goal

A SLOW-32 assembler written in Forth, loaded as a prelude file into the stage 1 kernel. It reads `.s` assembly source and produces `.s32o` object files (or `.s32x` executables directly for simplicity).

## Design

Classic Forth assembler pattern: each mnemonic is a Forth word that encodes an instruction and writes it to the output buffer.

### Architecture

```
Source text (stdin or file)
    │
    ▼
┌─────────────┐
│ Line reader  │  Read lines, strip comments
└─────┬───────┘
      │
      ▼
┌─────────────┐
│ Tokenizer    │  Split into mnemonic + operands
└─────┬───────┘
      │
      ▼
┌─────────────┐
│ Assembler    │  Encode instruction, handle labels
└─────┬───────┘
      │
      ▼
┌─────────────────┐
│ .s32o/.s32x emit │  Write binary output
└─────────────────┘
```

### Two-Pass Assembly

**Pass 1:** Scan for labels, record their addresses. Build a symbol table.

**Pass 2:** Emit instructions, resolving label references to actual offsets or relocation entries.

Alternatively, a single-pass assembler with backpatching: emit placeholder values for forward references, record fixup locations, patch after the end of input.

## Instruction Encoding Words

Each mnemonic becomes a Forth word. The assembler defines encoding helpers:

```forth
\ Encoding helpers
: R-TYPE ( op rd rs1 rs2 -- )
    20 LSHIFT SWAP 15 LSHIFT OR SWAP 7 LSHIFT OR OR
    HERE @ W, ;    \ emit 32-bit word to code buffer

: I-TYPE ( op rd rs1 imm -- )
    $FFF AND 20 LSHIFT SWAP 15 LSHIFT OR SWAP 7 LSHIFT OR OR
    HERE @ W, ;

: S-TYPE ( op rs1 rs2 imm -- )
    DUP $1F AND 7 LSHIFT          \ imm[4:0] << 7
    SWAP 5 RSHIFT $7F AND 25 LSHIFT OR  \ imm[11:5] << 25
    SWAP 20 LSHIFT OR             \ rs2 << 20
    SWAP 15 LSHIFT OR             \ rs1 << 15
    OR                            \ opcode
    HERE @ W, ;
```

Then instruction words are trivial:

```forth
: ADD,  ( rd rs1 rs2 -- )  $00 -ROT R-TYPE ;
: SUB,  ( rd rs1 rs2 -- )  $01 -ROT R-TYPE ;
: ADDI, ( rd rs1 imm -- )  $10 -ROT I-TYPE ;
: LUI,  ( rd imm -- )      \ U-type encoding
    $FFFFF AND 12 LSHIFT SWAP 7 LSHIFT OR $20 OR
    HERE @ W, ;
\ ... etc for all ~40 mnemonics
```

## Features Required

### Labels

```forth
VARIABLE label-count
CREATE label-names 256 CELLS ALLOT   \ array of name pointers
CREATE label-addrs 256 CELLS ALLOT   \ array of addresses

: LABEL: ( "name" -- )
    PARSE-WORD label-names label-count @ CELLS + !
    HERE @ label-addrs label-count @ CELLS + !
    1 label-count +! ;

: LABEL> ( "name" -- addr )
    PARSE-WORD                 \ get name to look up
    label-count @ 0 DO
        DUP label-names I CELLS + @ COMPARE 0= IF
            DROP label-addrs I CELLS + @
            UNLOOP EXIT
        THEN
    LOOP
    DROP -1 ;                  \ not found
```

### Directives

| Directive | Purpose |
|-----------|---------|
| `.text` | Switch to code section |
| `.data` | Switch to data section |
| `.bss` | Switch to BSS section |
| `.word VALUE` | Emit 32-bit value |
| `.byte VALUE` | Emit 8-bit value |
| `.asciz "string"` | Emit NUL-terminated string |
| `.ascii "string"` | Emit string without NUL |
| `.space N` | Reserve N bytes (BSS or zero-fill) |
| `.align N` | Align to N-byte boundary |
| `.global NAME` | Mark symbol as global |

### Pseudo-Instructions

| Pseudo | Expansion |
|--------|-----------|
| `mv rd, rs` | `ADDI rd, rs, 0` |
| `li rd, value` | `LUI + ADDI` (or just `ADDI` if small) |
| `la rd, label` | `LUI + ADDI` with relocations |
| `j label` | `JAL r0, label` |
| `jal label` | `JAL r31, label` |
| `jr rs` | `JALR r0, rs, 0` |
| `ret` | `JALR r0, r31, 0` |
| `call label` | `LUI r2, %hi` + `JALR r31, r2, %lo` |
| `nop` | `ADD r0, r0, 0` |

### Register Parsing

```forth
: PARSE-REG ( -- regnum )
    PARSE-WORD              \ get "r0", "r1", ..., "r31", "sp", "fp", "lr"
    \ Handle aliases first
    2DUP S" sp" COMPARE 0= IF 2DROP 29 EXIT THEN
    2DUP S" fp" COMPARE 0= IF 2DROP 30 EXIT THEN
    2DUP S" lr" COMPARE 0= IF 2DROP 31 EXIT THEN
    \ Parse "rN"
    DROP 1+ NUMBER           \ skip 'r', parse number
    DROP ;                   \ drop flag, keep number
```

## Output Format

### Simple Path: Direct .s32x Output

For the initial bootstrap, skip the object file format and emit .s32x directly:

1. Assemble all code to a buffer, tracking section sizes
2. Write the 64-byte header
3. Write the section table
4. Write section data

This avoids implementing relocations for single-file programs.

### Full Path: .s32o Object Files

For multi-file assembly (needed for linking libc), emit proper .s32o files:

1. Emit sections with placeholder relocation targets
2. Build symbol table (local + global symbols)
3. Build relocation table (HI20, LO12, BRANCH, JAL types)
4. Write .s32o header, section table, symbol table, relocation table, string table

The relocation types match those in `common/s32_formats.h`:

| Type | Code | Description |
|------|:----:|-------------|
| REL_32 | 0x01 | Direct 32-bit reference |
| REL_HI20 | 0x02 | Upper 20 bits (for LUI) |
| REL_LO12 | 0x03 | Lower 12 bits (for ADDI/LD/ST) |
| REL_BRANCH | 0x04 | 13-bit PC-relative branch |
| REL_JAL | 0x05 | 21-bit PC-relative jump |

## Size Estimate

| Component | Forth Lines |
|-----------|:-----------:|
| Encoding helpers (R/I/S/B/U/J) | ~40 |
| Instruction words (~40 mnemonics) | ~80 |
| Register parser | ~20 |
| Label management | ~40 |
| Directive handling | ~50 |
| Line reader + tokenizer | ~40 |
| .s32x output | ~50 |
| .s32o output (optional) | ~80 |
| **Total** | **~320-400** |

## Testing

1. Assemble a trivial "Hello, World" program and verify output matches hand-assembled binary
2. Assemble the Forth kernel source (`forth/kernel.s`) — output should match the cross-assembled binary
3. Assemble `crt0.s` — verify it produces a working startup sequence
4. Round-trip test: assemble a program, disassemble it (stage 2 output vs reference disassembler)

## Dependencies

- **Stage 1**: Forth kernel with file I/O (for writing output files)
- **ISA-ENCODING.md**: Reference for instruction encoding

## What Stage 2 Provides to Stage 3

The assembler enables:

- Building `.s32o` object files from assembly source
- Assembling the runtime (`crt0.s`, libc assembly stubs)
- Assembling output from the stage 4 C compiler
- Rebuilding the Forth kernel itself (self-test)
