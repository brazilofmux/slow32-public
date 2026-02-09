# SLOW-32 Forth — Issues & Recommendations

This document tracks bugs, architectural limitations, and potential improvements identified during the code review of the Stage 4 Forth kernel.

## Critical Bugs & Standard Violations

### 1. `EVALUATE` Overwrites `TIB` (Resolved)
The `EVALUATE` primitive previously copied input strings into the `tib` buffer.
- **Status**: Fixed in `86b3f41`. Implemented `SOURCE` as a pointer/length pair (`var_source_ptr`, `var_source_len`). `EVALUATE` now saves the caller's source state on the return stack and restores it after interpretation.

### 2. `S"` in Interpretation Mode (kernel.s:2815)
The `S"` word is marked `IMMEDIATE` but the implementation explicitly does nothing if `STATE` is zero.
- **Problem**: In interpretation mode, `S" hello"` should parse "hello" and leave `( c-addr u )` on the stack. Currently, it just exits, leaving the string in the input stream where the interpreter will try to execute it as a word.
- **Recommendation**: Implement interpretation-mode parsing for `S"` (copying the string to a transient buffer like `PAD`).

### 3. Missing Dictionary Bounds Checks (Resolved)
Primitives that advanced `HERE` lacked overflow protection.
- **Status**: Fixed in `86b3f41`. Added checks against `user_dictionary_end` in `COMMA`, `ALLOT`, `C,`, and word definition headers. Overflow now triggers a system reset via `ABORT`.

### 4. `PAD` Buffer Conflict
- **Problem**: The `pad` label in `kernel.s` (used by `WORD` to store the parsed name) is at the end of the user dictionary. However, the `PAD` word in `prelude.fth` is defined as `HERE 128 +`. 
- **Conflict**: If the dictionary grows, `PAD` (word) moves and may collide with other data. `WORD` returns an address that is far away from the user's `PAD`.
- **Recommendation**: Unify the definition of `PAD`. Standard Forth `WORD` buffer is usually separate from `PAD`.

### 5. Brittle Branch Offsets in `COLD_START` (Resolved)
The boot code used hardcoded numeric offsets for `0BRANCH` and `BRANCH`.
- **Status**: Fixed in `6e0b3ba`. Added label-difference support to the assembler and updated `kernel.s` to use computed offsets (e.g., `target - .Lhere`).

---

## Performance & Optimization Opportunities

### 6. Inefficient `MOVE` and `SEARCH`
These were implemented as byte-by-byte loops in `prelude.fth`.
- **Status**: `MOVE` fixed in `221eb58`. Now an assembly primitive that calls `memmove()`, allowing fast emulators to use host-native memory operations.
- **Opportunity**: Implement `SEARCH` as an assembly primitive or using an optimized algorithm.

### 7. Native `MULHU` for `UM*` (kernel.s:3460)
The `UM*` implementation manually converts a signed `MULH` result to unsigned.
- **Opportunity**: The SLOW-32 ISA provides a native `MULHU` instruction (opcode 0x1F) which performs unsigned 32x32->hi-32 multiply directly.

### 8. `FIND` Efficiency
The `FIND` word performs a linear search through a linked list of word headers.
- **Note**: While standard for Stage 4, as the dictionary grows (prelude + user code), lookups will slow down significantly. 
- **Future**: Consider a hashed dictionary or a more efficient search structure for Stage 5.

---

## Usability & Diagnostics

### 9. Vague Error Messages (Resolved)
When a word was not found, the interpreter printed only `?`.
- **Status**: Fixed in `ba29655`. The interpreter now prints the offending word name (e.g., `XYZZY ?`) before the newline.

### 10. Stack Size Limits
Data and Return stacks are currently 1024 bytes (256 cells) each.
- **Note**: This is sufficient for the prelude, but deeply recursive Forth code or large `DO` loop nesting may hit these limits quickly.
- **Recommendation**: Consider increasing to 4KB or 8KB if memory permits.
