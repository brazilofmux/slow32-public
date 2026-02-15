# Stage 3: Forth-Hosted Minimal Linker

> **V2 cross-reference:** This is **Stage 3** in [BOOTSTRAP-V2.md](BOOTSTRAP-V2.md) (Layer B: Forth Self-Hosting). The Forth archiver (previously bundled here) is now **Stage 2**.

## Goal

A minimal linker written in Forth that takes `.s32o` object files, resolves symbols, applies relocations, and emits a `.s32x` executable. This replaces the cross-compiled `s32-ld` for bootstrap purposes.

## Current Checkpoint

As of 2026-02-15:

- `selfhost/stage3/link.fth` is the active Stage 3 (V2) linker.
- `selfhost/stage3/ar.fth` is split as the Stage 2 (V2) archiver and is no longer bundled here.
- Linker behavior is stable for Stage 4 regression and validation pipelines.

## What the Linker Does

1. **Load object files** — read `.s32o` headers, sections, symbols, relocations
2. **Merge sections** — combine `.text` from all objects into one `.text`, same for `.data`, `.bss`, `.rodata`
3. **Resolve symbols** — match undefined references to global definitions
4. **Apply relocations** — patch instruction fields with resolved addresses
5. **Emit executable** — write `.s32x` with proper header and memory layout
6. **Inject linker symbols** — `__bss_start`, `__bss_end`, `__heap_start`, etc.

## Input Format: .s32o

Object file structure (from `common/s32_formats.h`):

```
s32o_header_t (40 bytes)
├── magic: 0x5333324F ("S32O")
├── nsections, nsymbols
├── sec_offset, sym_offset, str_offset, str_size
│
Section table: s32o_section_t[nsections] (32 bytes each)
├── name_offset, type, flags, size, offset, align
├── nrelocs, reloc_offset
│
Symbol table: s32o_symbol_t[nsymbols] (16 bytes each)
├── name_offset, value, section, type, binding, size
│
Relocation entries: s32o_reloc_t[nrelocs] (16 bytes each)
├── offset, symbol, type, addend
│
String table: raw bytes (NUL-separated names)
Section data: raw bytes
```

## Output Format: .s32x

Executable structure:

```
s32x_header_t (64 bytes)
├── magic: 0x53333258 ("S32X")
├── entry, nsections, sec_offset
├── code_limit, data_limit, stack_base, mem_size, heap_base
│
Section table: s32x_section_t[nsections] (28 bytes each)
├── name_offset, type, vaddr, offset, size, mem_size, flags
│
String table
Section data
```

## Relocation Types

| Type | Code | Instruction | Field | Computation |
|------|:----:|-------------|-------|-------------|
| REL_32 | 0x01 | `.word` | Full 32-bit | S + A |
| REL_HI20 | 0x02 | LUI | bits [31:12] | ((S + A + 0x800) >> 12) & 0xFFFFF |
| REL_LO12 | 0x03 | ADDI/LD/ST | bits [31:20] or split | (S + A) & 0xFFF |
| REL_BRANCH | 0x04 | BEQ/BNE/... | B-type imm | S + A - P |
| REL_JAL | 0x05 | JAL | J-type imm | S + A - P |

Where S = symbol value, A = addend, P = relocation site address.

### HI20 Relocation Detail

The `+0x800` in HI20 compensates for ADDI sign-extension. When bit 11 of the lower 12 bits is set, ADDI effectively subtracts 0x1000 from the upper bits. Adding 0x800 before shifting rounds up to compensate.

### LO12 for Store Instructions

S-type stores split the immediate across two bit fields ([31:25] and [11:7]). The linker must detect whether the target instruction is an I-type (ADDI, loads) or S-type (stores) and patch the appropriate fields.

Detection: check `opcode & 0x7F`:

- 0x38, 0x39, 0x3A → S-type (STB, STH, STW)
- Everything else with LO12 → I-type

## Memory Layout

The linker assigns virtual addresses:

```
.text   → 0x00000000   (entry point)
.rodata → after .text, aligned
.data   → 0x00100000   (start of writable region)
.bss    → after .data, aligned
heap    → after .bss, aligned to 16 bytes
stack   → 0x0FFFFFF0, grows down to stack_end
```

The 0x00100000 boundary enforces W^X: code below is execute-only, data above is read-write.

## Linker-Provided Symbols

These synthetic symbols are injected so the runtime can find memory regions:

| Symbol | Value | Used By |
|--------|-------|---------|
| `__bss_start` | Start of BSS | crt0 (zeroing) |
| `__bss_end` | End of BSS | crt0 (zeroing) |
| `__heap_start` | Start of heap | malloc |
| `__heap_end` | End of heap | malloc |
| `__stack_top` | Initial SP | crt0 |
| `__mmio_base` | MMIO region base | libc I/O |

## Implementation Sketch

```forth
\ Linker state
VARIABLE num-objects
VARIABLE num-combined-sections
CREATE combined-text 1024 CELLS ALLOT   \ merged .text buffer
CREATE combined-data 1024 CELLS ALLOT   \ merged .data buffer
\ ... symbol table, relocation list

: LOAD-OBJECT ( filename-addr filename-len -- )
    \ Open file, read s32o header, verify magic
    \ Read sections, symbols, relocations, strings
    \ Merge each section into combined buffers
    \ Add symbols to global symbol table
    ;

: RESOLVE-SYMBOLS ( -- )
    \ For each undefined symbol, find matching global
    \ Report errors for unresolved symbols
    ;

: APPLY-RELOCATIONS ( -- )
    \ For each relocation entry:
    \   Look up symbol value
    \   Compute target value based on relocation type
    \   Patch instruction in combined section buffer
    ;

: EMIT-S32X ( filename-addr filename-len -- )
    \ Write s32x header
    \ Write section table
    \ Write string table
    \ Write section data
    ;

: LINK ( -- )
    \ Parse command line: object files and output name
    \ Load each object file
    RESOLVE-SYMBOLS
    APPLY-RELOCATIONS
    EMIT-S32X
    ;
```

## Size Estimate

| Component | Forth Lines (est.) |
|-----------|:-------------------:|
| Object/archive loading | ~250 |
| Merge + layout | ~200 |
| Symbol resolution | ~200 |
| Relocation application | ~250 |
| Emitter + diagnostics | ~200 |
| **Total (`link.fth`)** | **~1,100** |

## Archive Support (Optional)

The real linker supports `.s32a` archive files (collections of `.s32o` with an index). For bootstrap, this can be deferred — link individual `.s32o` files directly. Archive support adds ~100 lines.

## Testing

1. Link a single-file "Hello, World" object → verify output runs correctly
2. Link `crt0.s32o` + `program.s32o` + libc objects → verify multi-file linking
3. Compare output to reference linker: `s32-ld` output should produce identical executables for the same inputs
4. Verify relocation correctness: programs using global variables, function calls across files, and string literals

## Dependencies

- **Stage 1**: Forth kernel with file I/O
- **Stage 2**: Assembler (produces `.s32o` files to link)
- **s32_formats.h**: Binary format specification (reimplemented in Forth constants)

## What Stage 3 Provides to Stage 4

With assembler + linker, the bootstrap can:

- Build complete programs from assembly source
- Link multiple object files with proper symbol resolution
- Produce working `.s32x` executables
- The C compiler (stage 4) only needs to emit assembly text — the assembler and linker handle everything else
