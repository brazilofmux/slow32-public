# Cross-Toolchain: SLOW-32 → x86-64

*A complete native toolchain — compiler, archiver, linker — running on SLOW-32, targeting x86-64 Linux.*

## Motivation

The cross-compiler (cc-x64) currently produces standalone ELF executables directly — no linking step. Every function the program calls must be defined in the same source file. This works for small self-contained programs but breaks for anything that uses libc or is split across files.

The fix: a proper compilation pipeline.

```
cc-x64  input.c  -o output.o        # compile C → relocatable ELF object
ar-x64  rcs  lib.a  a.o b.o c.o     # archive objects into library
ld-x64  -o binary  crt0.o main.o lib.a   # link into static ELF executable
```

All three tools are SLOW-32 programs (`.s32x` binaries), run under any SLOW-32 emulator. They target x86-64 Linux. The result is a fully static ELF binary with no host dependencies.

### Why Not a Single Tool?

A monolithic tool (compile+link in one pass) limits reuse:

- Can't compile libc once and reuse it across programs
- Can't split large programs across files
- Can't let users replace libc with their own
- Can't archive and distribute prebuilt libraries

A proper pipeline costs ~2000 extra lines (linker + archiver) but makes the cross-toolchain genuinely useful rather than a demo.

## Architecture Overview

```
Source Files          Object Files              Executable
                                               
  foo.c ──→ cc-x64 ──→ foo.o ──┐              
  bar.c ──→ cc-x64 ──→ bar.o ──┤              
                                ├──→ ld-x64 ──→ program (ELF64)
  libc_x64.c ──→ cc-x64 ──→ libc.o ──→ ar-x64 ──→ libc_x64.a ──┘
  crt0_x64.c ──→ cc-x64 ──→ crt0.o ──────────────────────────────┘
```

### Existing Infrastructure

| Component | Status | Notes |
|-----------|--------|-------|
| x64_encode.h | Done | x86-64 instruction encoder (MOV, ADD, CALL, JMP, etc.) |
| elf_writer.h | Done | ELF64 executable writer (headers, segments, sections) |
| codegen_x64.h | Done | Tree-walk code generator with call/data patching |
| libc_x64.h | Partial | Syscall wrappers, malloc, string functions |
| cc-x64.c | Done | Driver, compiles and produces ELF executable |
| SLOW-32 linker (s32-ld) | Done | Reference for relocation processing, archive handling |

### What Needs to Change

1. **cc-x64**: Emit `.o` (relocatable ELF) instead of final executable. Unresolved symbols become relocations, not NOPs.
2. **ld-x64**: New tool. Reads `.o` and `.a` files, resolves symbols, applies relocations, writes final ELF.
3. **ar-x64**: New tool. Bundles `.o` files with a symbol index.
4. **libc_x64**: Compiled as a library (`libc_x64.a`), not `#include`d.
5. **crt0**: Separate object providing `_start` entry point.

## Object File Format

We use **standard ELF64 relocatable** format (not a custom format). This lets us verify objects with `readelf` on the host, which is invaluable for debugging.

### ELF64 Relocatable Object Layout

```
ELF Header (64 bytes)
  e_type    = ET_REL (1)
  e_machine = EM_X86_64 (62)

Section Header Table (at e_shoff)
  [0]  SHT_NULL
  [1]  .text       SHT_PROGBITS   SHF_ALLOC|SHF_EXECINSTR
  [2]  .rodata     SHT_PROGBITS   SHF_ALLOC
  [3]  .data       SHT_PROGBITS   SHF_ALLOC|SHF_WRITE
  [4]  .bss        SHT_NOBITS     SHF_ALLOC|SHF_WRITE
  [5]  .rela.text  SHT_RELA       sh_link→symtab, sh_info→.text
  [6]  .rela.data  SHT_RELA       sh_link→symtab, sh_info→.data
  [7]  .symtab     SHT_SYMTAB     sh_link→strtab
  [8]  .strtab     SHT_STRTAB
  [9]  .shstrtab   SHT_STRTAB

String Table (.strtab)
  Function names, global names, external references

Symbol Table (.symtab)
  STT_FUNC   — each compiled function (STB_GLOBAL)
  STT_OBJECT — each global variable (STB_GLOBAL)
  STT_NOTYPE — each external reference (STB_GLOBAL, st_shndx=SHN_UNDEF)
```

### Why Standard ELF, Not Custom

The SLOW-32 toolchain uses `.s32o` (custom format) because SLOW-32 is a custom architecture. For x86-64, standard ELF gives us:

- `readelf -a foo.o` works immediately for debugging
- `objdump -d foo.o` disassembles our code
- Format is well-documented (no ambiguity)
- Potential future interop with host-compiled objects (advanced, not required)

The cost is that ELF is more complex to emit — section headers, string tables, symbol info fields — but the stage07 compiler already handles ELF headers for executables. Extending to relocatable is incremental.

### ELF Constants

```c
/* e_type */
#define ET_REL   1    /* Relocatable object */
#define ET_EXEC  2    /* Executable (already used) */

/* Section header types */
#define SHT_NULL     0
#define SHT_PROGBITS 1
#define SHT_SYMTAB   2
#define SHT_STRTAB   3
#define SHT_RELA     4    /* Relocations with addend */
#define SHT_NOBITS   8    /* BSS */

/* Section flags */
#define SHF_WRITE     0x1
#define SHF_ALLOC     0x2
#define SHF_EXECINSTR 0x4

/* Symbol binding */
#define STB_LOCAL  0
#define STB_GLOBAL 1
#define STB_WEAK   2

/* Symbol type */
#define STT_NOTYPE  0
#define STT_OBJECT  1
#define STT_FUNC    2
#define STT_SECTION 3

/* Special section indices */
#define SHN_UNDEF  0
#define SHN_ABS    0xFFF1
```

## Relocation Types

The compiler generates a small set of x86-64 instruction patterns. Each pattern that references an external symbol needs a relocation type.

### Instruction Patterns and Their Relocations

| Pattern | Encoding | Relocation | When Used |
|---------|----------|------------|-----------|
| `CALL func` | `E8 rel32` | R_X86_64_PC32 | Direct function call |
| `MOVABS r64, addr` | `48 B8 imm64` | R_X86_64_64 | Function pointer, absolute address |
| `MOV [rip+disp], ...` | `48 89 05 rel32` | R_X86_64_PC32 | Global variable store (future) |
| `LEA r64, [rip+disp]` | `48 8D 05 rel32` | R_X86_64_PC32 | Address of global/string (future) |
| `.quad addr` | 8-byte data | R_X86_64_64 | Pointer in .data section |

### Relocation Constants

```c
#define R_X86_64_NONE   0    /* No relocation */
#define R_X86_64_64     1    /* 64-bit absolute: S + A */
#define R_X86_64_PC32   2    /* 32-bit PC-relative: S + A - P */
#define R_X86_64_32S   11    /* 32-bit signed absolute: S + A (future) */
```

Where: S = symbol value, A = addend, P = place (address of relocation site).

### Rela Entry (24 bytes)

```c
typedef struct {
    uint64_t r_offset;    /* Offset in section */
    uint64_t r_info;      /* Symbol index (32 hi) + type (32 lo) */
    int64_t  r_addend;    /* Constant addend */
} Elf64_Rela;

/* r_info encoding */
#define ELF64_R_SYM(i)   ((i) >> 32)
#define ELF64_R_TYPE(i)   ((i) & 0xFFFFFFFF)
#define ELF64_R_INFO(s,t) (((uint64_t)(s) << 32) | (uint64_t)(t))
```

**Implementation note**: The stage07 compiler uses 32-bit `int`. We emit r_info as two 32-bit writes: symbol index at offset+4 (hi), type at offset+0 (lo). Same for r_offset and r_addend — always emit as lo/hi pairs.

### Relocation Application (Linker)

For each relocation entry in `.rela.text` or `.rela.data`:

```
R_X86_64_PC32:
    target = symbol_vaddr + addend - reloc_vaddr
    patch 4 bytes at reloc_offset with (int32_t)target

R_X86_64_64:
    target = symbol_vaddr + addend
    patch 8 bytes at reloc_offset with (int64_t)target
```

The addend for CALL instructions is typically `-4` (because PC-relative is measured from the end of the instruction, i.e., after the 4-byte displacement field).

## Compiler Changes (cc-x64)

### Current Flow

```
parse → codegen → resolve_calls → resolve_relocations → elf_build → elf_write_file
```

`resolve_calls()` matches call patches against `cg_func_name[]`. Unresolved calls get NOP'd.
`resolve_relocations()` patches absolute addresses for strings and globals.

### New Flow

```
parse → codegen → emit_object_file
```

No resolution step. Instead:

1. Every call patch (`cg_cpatch_*`) becomes an `R_X86_64_PC32` rela entry
2. Every data relocation (`cg_dreloc_*`) becomes an `R_X86_64_64` rela entry
3. Every compiled function becomes a `STT_FUNC` symbol (section .text, `STB_GLOBAL`)
4. Every called-but-undefined function becomes an `SHN_UNDEF` symbol
5. Every global variable becomes a `STT_OBJECT` symbol (section .data or .bss)

The existing `cg_func_name[]`/`cg_func_off[]` arrays already track defined functions. The `cg_cpatch_name[]`/`cg_cpatch_off[]` arrays track call sites. These map directly to symbols and relocations.

### New Function: `emit_object_file()`

Replaces the current `gen_program()` tail (resolve + elf_build + elf_write):

```
emit_object_file(filename):
    1. Collect sections: .text (x64_buf), .rodata (cg_str_*), .data (cg_data), .bss (cg_bss_len)
    2. Build symbol table:
       - For each cg_func_name[i]: add FUNC symbol at cg_func_off[i] in .text
       - For each cg_glob_name[i]: add OBJECT symbol in .data or .bss
       - For each unresolved cg_cpatch_name[i]: add UNDEF symbol (dedup by name)
    3. Build .rela.text:
       - For each cg_cpatch_off[i]: add R_X86_64_PC32 rela (addend = -4)
       - For each code-section cg_dreloc_off[i]: add R_X86_64_64 rela
    4. Build .rela.data:
       - For each data-section cg_dreloc_off[i]: add R_X86_64_64 rela
    5. Build section header string table (.shstrtab)
    6. Build string table (.strtab)
    7. Emit ELF header (ET_REL) + section headers + all data
    8. Write to file
```

### Backward Compatibility

The compiler gains a `-c` flag:
- `cc-x64 -c input.c -o output.o` — emit relocatable object
- `cc-x64 input.c -o output` — compile + link in one shot (legacy behavior, calls internal linker or errors)

Initially, only `-c` mode is needed. The one-shot mode can be restored later by invoking the linker internally.

## Linker (ld-x64)

### Interface

```
ld-x64 [-o output] [-e entry] input1.o input2.o lib.a ...
```

- `-o output`: Output ELF executable (default: `a.out`)
- `-e entry`: Entry point symbol (default: `_start`)
- Input: `.o` files and `.a` archives, processed left to right

### Algorithm

```
1. LOAD INPUTS
   For each .o file:
     - Parse ELF header, section headers, symbol table, rela sections
     - Add all GLOBAL symbols to global symbol table
     - Track undefined symbols
   For each .a file:
     - Parse archive index
     - Iteratively load members that satisfy undefined symbols (fixed-point)

2. CHECK UNDEFINED SYMBOLS
   If any symbol remains undefined after archive scanning → error

3. ASSIGN ADDRESSES
   Merge sections by name:
     - All .text sections → combined .text at 0x401000
     - All .rodata sections → combined .rodata at next page after .text
     - All .data sections → combined .data at next page after .rodata
     - All .bss sections → combined .bss immediately after .data
   Each input section gets a base offset within the combined section.
   Update all symbol values: sym.value += section_base_offset

4. APPLY RELOCATIONS
   For each rela entry in each input object:
     - Look up target symbol → resolved address
     - Compute value based on relocation type
     - Patch bytes in the combined section buffer

5. WRITE EXECUTABLE
   Use elf_writer.h (or equivalent) to emit:
     - ELF header (ET_EXEC, entry = resolved _start address)
     - Program headers (PT_LOAD for text, rodata, data+bss)
     - Section data (.text, .rodata, .data)
   BSS is not written (memsz > filesz tells kernel to zero-fill)
```

### Memory Layout

Same as current elf_writer.h:

```
0x400000:    ELF headers + program headers
0x401000:    .text  (R+X)
0x4xxxxx:    .rodata (R)   — page-aligned after .text
0x4xxxxx:    .data  (R+W)  — page-aligned after .rodata
             .bss   (R+W)  — immediately after .data (zero-filled)
```

### Symbol Resolution

Simple global namespace (no namespaces, no versioning):

1. Scan all input objects, collect defined and undefined symbols
2. For archives, iteratively pull in members that define needed symbols
3. Duplicate strong definitions → error
4. Weak definitions → overridden by strong; if no strong, use weak value (or 0 if only declarations)

### Data Structures

The linker needs (all as flat arrays, stage07-compatible):

```c
#define LD_MAX_OBJECTS   256
#define LD_MAX_SYMBOLS   8192
#define LD_MAX_SECTIONS  1024
#define LD_MAX_RELOCS    32768

/* Per-input-object tracking */
static int ld_obj_text_base[LD_MAX_OBJECTS];   /* offset of this obj's .text in merged .text */
static int ld_obj_rodata_base[LD_MAX_OBJECTS];
static int ld_obj_data_base[LD_MAX_OBJECTS];
static int ld_obj_bss_base[LD_MAX_OBJECTS];

/* Global symbol table */
static char *ld_sym_name[LD_MAX_SYMBOLS];
static int   ld_sym_value[LD_MAX_SYMBOLS];     /* resolved virtual address */
static int   ld_sym_section[LD_MAX_SYMBOLS];   /* 0=undef, 1=text, 2=rodata, 3=data, 4=bss */
static int   ld_sym_binding[LD_MAX_SYMBOLS];   /* local/global/weak */
static int   ld_nsyms;
```

## Archive Format

Use **standard Unix ar** format. This means host `ar t lib.a` and `readelf` can inspect our archives. The format is trivially simple:

```
!<arch>\n                          (8-byte magic)

/               0           0     0     0       size    `\n
<symbol index>                                          (System V format)

filename.o/     timestamp   uid   gid   mode    size    `\n
<ELF object data, padded to 2-byte boundary>

filename2.o/    timestamp   uid   gid   mode    size    `\n
<ELF object data, padded to 2-byte boundary>
...
```

### Symbol Index (/ member)

The first member is named `/` and contains a symbol index:

```
4-byte big-endian: number of symbols (N)
N * 4-byte big-endian: file offset of member defining each symbol
N * NUL-terminated strings: symbol names
```

### ar-x64 Interface

```
ar-x64 rcs output.a input1.o input2.o ...
```

Only `rcs` mode (replace, create, symbol index). That's all we need.

### Implementation

~200-300 lines. Read each input `.o`, extract global symbols from its `.symtab`, build the index, write the archive.

## C Runtime (crt0 + libc)

### crt0_x64.c

Minimal startup. Compiled to `crt0.o`, linked first in every executable.

```c
/* Provides _start, calls main, calls exit */
void _start(void) {
    /* Linux puts argc at [rsp], argv at [rsp+8], envp after argv's NULL */
    /* This is implemented in inline assembly or as hand-coded bytes */
}
```

The current codegen emits `_start` as inline machine code. With the linker, `_start` becomes a normal function in `crt0.o`. The crt0 source:

```c
extern int main(int argc, char **argv);

/* These are syscall-based, defined in libc */
extern void exit(int status);

void _start(void) {
    /* argc, argv, envp come from the kernel on the stack */
    /* Implemented as a small assembly stub or compiler intrinsic */
    int argc;
    char **argv;
    /* ... extract from stack ... */
    int ret;
    ret = main(argc, argv);
    exit(ret);
}
```

**Pragmatic approach**: The codegen can emit `_start` as a special function that uses hand-coded bytes (the current approach), compiled into `crt0.o`. This avoids needing inline assembly support in the compiler.

### libc_x64.a

`libc_x64.h` already exists with syscall wrappers and a basic `malloc`. It needs to be restructured as compilable C source files:

```
libc_x64/
  syscalls.c    — sys_write, sys_read, sys_open, sys_close, sys_lseek, sys_brk, sys_mmap, sys_exit
  malloc.c      — malloc, free, calloc, realloc (brk-based)
  string.c      — strlen, strcmp, strncmp, strcpy, strncpy, memcpy, memset, memcmp, strdup
  stdio.c       — write, read, open, close, lseek (POSIX wrappers around sys_*)
  printf.c      — printf, fprintf, sprintf, snprintf (subset: %d %u %x %s %c %p %ld %lu %lx)
  ctype.c       — isdigit, isspace, isalpha, toupper, tolower
  exit.c        — exit, abort
  env.c         — getenv
```

Each file compiles to a separate `.o`, then `ar-x64 rcs libc_x64.a *.o`. The linker's archive handling pulls in only the members actually needed.

**Key constraint**: All source must compile under stage07's C subset. No `long long` keyword (use `int` — 64-bit on x86-64 target? No — the stage07 compiler treats `int` as 32-bit. We need to handle this carefully). No complex preprocessor. No VLAs. No compound literals.

### Integer Size Question

The stage07 compiler's `int` is 32 bits. On x86-64, `int` is 32 bits and pointers are 64 bits. The current codegen uses `MOVABS` (64-bit immediate) for addresses but 32-bit arithmetic for `int`.

For the linker and libc, this means:
- Pointer variables need 64-bit storage (8 bytes on stack)
- `int` arithmetic is 32-bit (MOV r32 zero-extends to 64 on x86-64)
- `malloc` returns a pointer — must be 64-bit capable
- Array indexing with int index needs sign-extension to 64-bit for address calculation

The current codegen already handles this: all locals are 8 bytes on the stack, pointers use 64-bit registers. This continues to work in the separate-compilation model.

## Implementation Plan

### Phase 1: Object File Emission (cc-x64 -c)

**Goal**: `cc-x64 -c input.c -o output.o` produces a valid ELF64 relocatable object.

**Changes to codegen_x64.h**:
- New function `emit_object_file()` replacing resolve+elf_build+elf_write
- Build ELF section headers (10 sections: null, .text, .rodata, .data, .bss, .rela.text, .rela.data, .symtab, .strtab, .shstrtab)
- Convert `cg_cpatch_*` arrays to `.rela.text` entries
- Convert `cg_dreloc_*` arrays to `.rela.text` and `.rela.data` entries
- Build `.symtab` from `cg_func_*` and `cg_glob_*` arrays + undefined externals

**New file**: `obj_writer.h` — ELF64 relocatable writer (complement to `elf_writer.h` which writes executables)

**Validation**: `readelf -a output.o` should show correct sections, symbols, relocations.

**Estimated size**: ~400-600 lines (obj_writer.h)

### Phase 2: Linker (ld-x64)

**Goal**: `ld-x64 -o output crt0.o main.o libc_x64.a` produces a working ELF executable.

**New file**: `ld-x64.c` — linker driver

**Components**:
1. ELF64 relocatable reader (~300 lines)
2. Archive reader (~200 lines)
3. Symbol table + resolution (~200 lines)
4. Section merging + address assignment (~200 lines)
5. Relocation application (~150 lines)
6. ELF executable writer (reuse elf_writer.h concepts, ~200 lines)

**Estimated size**: ~1200-1500 lines

**Validation**: Link a trivial program (main calls write via syscall), run it.

### Phase 3: Archiver (ar-x64)

**Goal**: `ar-x64 rcs libc_x64.a obj1.o obj2.o ...`

**New file**: `ar-x64.c`

**Estimated size**: ~200-300 lines

**Validation**: `ar t libc_x64.a` on host shows members. Linker can consume the archive.

### Phase 4: libc + crt0

**Goal**: Compile libc_x64 sources into `libc_x64.a`, compile crt0 into `crt0.o`.

**Split libc_x64.h** into separate `.c` files (see list above).

**Compile each**: `cc-x64 -c syscalls.c -o syscalls.o` (etc.)

**Archive**: `ar-x64 rcs libc_x64.a syscalls.o malloc.o string.o ...`

**Validation**: Compile+link a program that calls `printf`, `malloc`, `strlen`. Run it.

### Phase 5: Compile s32-emu-x64.c

**Goal**: The original motivating use case. Compile the SLOW-32 emulator to a native x86-64 binary.

```
cc-x64 -c s32-emu-x64.c -o s32-emu-x64.o
ld-x64 -o s32-emu-x64 crt0.o s32-emu-x64.o libc_x64.a
```

**Validation**: Run a simple SLOW-32 program on the cross-compiled emulator. Compare output.

### Phase 6: Self-Compilation

Compile cc-x64.c itself into a native binary. Then use that native binary to compile s32-emu-x64.c without needing a SLOW-32 emulator.

## File Inventory

### New Files

```
selfhost/stage07-cross/
  obj_writer.h        — ELF64 relocatable object writer
  ld-x64.c            — Linker driver
  ar-x64.c            — Archiver
  libc_x64/
    syscalls.c         — Linux syscall wrappers
    malloc.c           — Memory allocator
    string.c           — String functions
    stdio.c            — POSIX I/O wrappers
    printf.c           — Formatted output
    ctype.c            — Character classification
    exit.c             — Process exit
    env.c              — Environment access
  crt0_x64.c           — C runtime startup
```

### Modified Files

```
selfhost/stage07-cross/
  cc-x64.c             — Add -c flag handling
  codegen_x64.h        — Add emit_object_file(), keep emit_executable() for reference
  elf_writer.h         — Minor: extract shared ELF constants to elf_common.h
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| ELF relocatable format complexity | High | Use readelf for validation at every step |
| 64-bit arithmetic in 32-bit compiler | Medium | Emit lo/hi pairs for all 64-bit ELF fields (already done in elf_writer.h) |
| Stage07 C subset limitations | Medium | Keep libc source simple; avoid features the compiler doesn't support |
| Large symbol tables | Low | Flat arrays with generous limits; linker for small programs first |
| Archive format parsing | Low | Standard ar format is well-documented and simple |

## Testing Strategy

Each phase has concrete validation:

1. **Object files**: `readelf -a` + `objdump -d` on host
2. **Linker**: Link trivial test → run → check exit code
3. **Archiver**: `ar t` on host + linker can consume
4. **libc**: Compile test programs calling each libc function
5. **Emulator**: Cross-compiled emulator matches reference emulator output
6. **Self-compilation**: `cc-x64_native(cc-x64.c) == cc-x64_emu(cc-x64.c)` (bit-for-bit match = fixed point)

## Appendix: ELF64 Structure Sizes

For reference when computing offsets:

| Structure | Size (bytes) |
|-----------|-------------|
| Elf64_Ehdr (ELF header) | 64 |
| Elf64_Phdr (program header) | 56 |
| Elf64_Shdr (section header) | 64 |
| Elf64_Sym (symbol entry) | 24 |
| Elf64_Rela (relocation+addend) | 24 |

All ELF64 fields that are addresses or offsets are 8 bytes (uint64_t). In our 32-bit compiler, we emit these as lo32/hi32 pairs (hi32 is always 0 for addresses below 4GB).
