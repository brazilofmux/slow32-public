# SLOW-32 Assembler — Issues & Recommendations

This document tracks bugs, architectural limitations, and potential improvements identified during the code review of the SLOW-32 assembler (`slow32asm.c`).

## Critical Bugs & Safety Issues

### 1. Buffer Overflows in Symbol and Relocation Tables (Resolved)
The assembler uses fixed-size arrays for symbols and relocations.
- **Status**: Fixed. Symbol table in `write_object_file` is now heap-allocated via `malloc` and has bounds checks on `num_symbols` in both symbol-adding loops. Relocation table already had a bounds check in `add_relocation`.

### 2. `strncpy` Null-Termination Risks (Partially Resolved)
Multiple call sites use `strncpy` or `strcpy` with lengths exactly matching the destination buffer size.
- **Status**: Partially fixed in `af5e7d8`. Added explicit null-termination to `add_relocation`.
- **Problem**: Other sites like `add_label` and `instruction_t` assignments still use `strcpy` or potentially unsafe `strncpy`.
- **Mitigating factor**: The scanner limits identifiers to 63 chars, so in practice overflow cannot occur from source code input.
- **Recommendation**: Use a safer wrapper or manually ensure null-termination for all remaining sites.

### 3. Redundant and Duplicate Section Logic (Resolved)
The handling of `.text`, `.data`, and `.bss` was duplicated in `assemble_line`.
- **Status**: Fixed. Removed dead-code duplicate handlers that were unreachable in the `else if` chain.

### 4. Token Buffer Overflow
The `token_t.sval` buffer is fixed at 256 bytes.
- **Problem**: Since `MAX_LINE` is 65536, a very long string literal or an exceptionally long identifier will overflow the `sval` buffer in `scanner_next`.
- **Mitigating factor**: `scanner_next` already limits identifiers to 63 chars and strings to 254 chars. The 256-byte buffer is safe in practice.
- **Recommendation**: Low priority. Add explicit bounds checking during scanning if needed.

### 5. `symbols` array stack overflow (Resolved)
In `write_object_file`, `symbols` was declared as `s32o_symbol_t symbols[MAX_LABELS]`.
- **Status**: Fixed. Now heap-allocated with `malloc` and freed before return.

---

## Architectural Limitations

### 6. Silent Failure on Unknown Directives
The `assemble_line` function returns `true` at the end of the directive handling block regardless of whether the directive was recognized.
- **Problem**: Unrecognized directives are silently ignored.
- **Note**: This is actually **correct behavior** — LLVM emits many metadata directives (`.cfi_startproc`, `.type`, `.size`, `.addrsig`, etc.) that the assembler correctly ignores. Making these errors would break all LLVM-compiled files.

### 7. Branch/JAL Range Check uses `exit(1)` (Resolved)
When a branch was out of range, the code called `exit(1)` immediately.
- **Status**: Fixed. Range errors now set an error flag and break out of the label-resolution loop. The assembler exits cleanly with proper error messages.

### 8. Limited Relocation Support for `.word`
The `.word` and `.long` directives do not support `%hi()` or `%lo()` modifiers.
- **Problem**: Users cannot manually construct split-address data words.
- **Note**: Nobody has needed this. `.word symbol+offset` works for data references, and `%hi`/`%lo` work on instructions.

---

## Performance & Quality

### 9. O(N^2) String Table Deduplication
`add_string` performs a linear search through the string table for every new string added.
- **Problem**: For large projects with many symbols, assembly time will grow quadratically.
- **Note**: Not a bottleneck in practice. Even the largest runtime .s files assemble instantly.

### 10. Inefficient Instruction Buffer Growth
`ensure_instruction_capacity` is called for every single instruction or data byte.
- **Note**: Non-issue. The hot path is `if (needed <= capacity) return;` which is O(1). Geometric growth amortizes realloc cost.

---

## Additional Fixes

### 11. `.long` Used Stale Codepath (Resolved)
The `.long` handler duplicated `.word` logic but used the old `parse_immediate_or_symbol_ex` API.
- **Problem**: `.long (sym1 - sym2)` silently produced garbage while `.word (sym1 - sym2)` worked correctly.
- **Status**: Fixed. `.long` is now handled by the same codepath as `.word`, supporting label-diffs and all other `.word` features. The unused `parse_immediate_or_symbol_ex` function was removed.
