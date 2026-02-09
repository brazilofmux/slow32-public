# SLOW-32 Linker — Issues & Recommendations

This document tracks bugs, architectural limitations, and potential improvements identified during the code review of the SLOW-32 linker (`s32-ld.c`).

## Critical Bugs & Safety Issues

### 1. Buffer Overflows in Global Tables
The linker uses multiple fixed-size arrays for its core data structures:
- `input_files[MAX_INPUT_FILES]` (100)
- `sections[MAX_SECTIONS]` (1000)
- `symbols[MAX_SYMBOLS]` (32000)
- `relocations[MAX_RELOCATIONS]` (32000)
- **Problem**: There are insufficient bounds checks when adding to these tables (especially in `load_object_file`, `merge_sections`, and `build_symbol_table`). Exceeding these limits will cause memory corruption or crashes.
- **Recommendation**: Switch to dynamically allocated arrays that grow as needed, or at a minimum, add strict `if (count >= MAX) { error(); }` checks to every insertion point.

### 2. `strncpy` and `strcpy` Safety
Multiple call sites use `strncpy` or `strcpy` into fixed-size buffers:
- `symbol_entry_t.name` (256 bytes)
- `combined_section_t.name` (64 bytes)
- **Problem**: `strncpy(dest, src, sizeof(dest)-1)` does not guarantee null-termination if the source is too long. `strcpy` is inherently unsafe.
- **Recommendation**: Use a helper that ensures null-termination or use `snprintf(dest, sizeof(dest), "%s", src)`.

### 3. Memory Leaks in `load_archive_file`
When an archive is processed, several large buffers are allocated:
- `strings = malloc(hdr.str_size)`
- `symbols = malloc(hdr.nsymbols * ...)`
- `members = malloc(hdr.nmembers * ...)`
- **Problem**: These are not freed in the success path, only in one specific error branch. Every linked library causes a significant leak.
- **Recommendation**: Ensure `free()` is called for all three buffers before the function returns `true`.

### 4. Quadratic Complexity in Symbol/String Lookups
The linker performs multiple linear scans:
- `add_string`: Linear scan of the string table for every insertion.
- `build_symbol_table`: Linear scan of `ld->symbols` for every symbol in every input file.
- `collect_relocations`: Linear scan of `ld->symbols` for every relocation.
- **Problem**: Link time will grow quadratically ($O(N^2)$) with the number of symbols and relocations. Large projects (like those with C++ or complex libraries) will be extremely slow to link.
- **Recommendation**: Use hash tables to index the string table and the global symbol table.

### 5. `sym_strtab` Stack Overflow in `write_executable` (Resolved)
`write_executable` allocated `char sym_strtab[STRING_TABLE_SIZE]` (256KB) on the stack.
- **Status**: Fixed. Now heap-allocated with `malloc`/`free`.

---

## Architectural Limitations

### 6. Minimal Alignment Support
The linker assumes 4-byte alignment for most things but does not strictly honor the `align` field of input sections in all layout paths.
- **Problem**: If an object file requests 16-byte alignment for a specific data structure (e.g., SIMD-like structures or page-aligned buffers), the linker might misalign it.
- **Recommendation**: Rigorously use `(current + align - 1) & ~(align - 1)` in `merge_sections` and `layout_sections`.

### 7. Fixed Stack Base
`DEFAULT_STACK_BASE` is hardcoded at `0x0FFFFFF0`.
- **Problem**: This limits total memory usage and doesn't account for the actually available RAM in the emulator or the MMIO region.
- **Recommendation**: Allow the stack base to be configurable or calculate it based on total memory.

### 8. `PCREL_LO12` Implementation is Incomplete
The code for `S32O_REL_PCREL_LO12` (lines 1548+) contains a comment "may need adjustment" and uses the symbol value directly.
- **Problem**: PC-relative loads/stores in RISC-V/SLOW-32 are usually paired with an `AUIPC` at a specific address. The `LO12` part must be relative to the *address of the AUIPC*, not the current PC of the load/store.
- **Recommendation**: Implement proper pairing logic where `LO12` relocations find their associated `HI20` parent to calculate the correct delta.

---

## Quality of Life

### 9. Vague Error Reporting
Many errors (like "Relocation offset out of bounds") do not print the filename or section where the error occurred.
- **Recommendation**: Include context (file, section, symbol name) in all error messages.

### 10. `strip_symbols` implementation
The `-s` flag is only partially implemented. It prevents global symbols from being written to the output, but the symbol table logic still spends time processing them.
- **Recommendation**: Optimize the symbol table pass to ignore non-essential symbols earlier when stripping is enabled.
