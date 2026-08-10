# SLOW-32 Forth — Issues & Recommendations

This document tracks bugs, architectural limitations, and potential improvements identified during the code review of the Stage 4 Forth kernel.

## Critical Bugs & Standard Violations

### 1. `EVALUATE` Overwrites `TIB` (Resolved)
The `EVALUATE` primitive previously copied input strings into the `tib` buffer.
- **Status**: Fixed in `86b3f41`. Implemented `SOURCE` as a pointer/length pair (`var_source_ptr`, `var_source_len`). `EVALUATE` now saves the caller's source state on the return stack and restores it after interpretation.

### 2. `S"` in Interpretation Mode (Resolved)
- **Status**: Implemented in `kernel.s` (`squote_interpret`): when `STATE=0`,
  `S"` parses into alternating transient buffers (`squote_ibuf0` /
  `squote_ibuf1`) and pushes `( c-addr u )`. Covered by
  `tests/test-squote-interp.fth`.

### 3. Missing Dictionary Bounds Checks (Resolved)
Primitives that advanced `HERE` lacked overflow protection.
- **Status**: Fixed in `86b3f41`. Added checks against `user_dictionary_end` in `COMMA`, `ALLOT`, `C,`, and word definition headers. Overflow now triggers a system reset via `ABORT`.

### 4. `PAD` Buffer Conflict (Resolved)
- **Problem**: A single fixed `pad` BSS buffer was shared by `WORD`/`: `/`CREATE`
  (counted names), pictured numeric output (`<#`/`HOLD`/`#>`), and `.`
  conversion, while the `PAD` word returned `HERE 128 +`.
- **Status**: Split into dedicated buffers:
  - `word_buf` (256 B) — parse / define name buffer; length capped at 255
    for `WORD`, 127 for dictionary headers (IMMEDIATE uses length bit 7)
  - `pno_buf` (128 B) — pictured numeric output and `.`
  - `PAD` remains `HERE 128 +` (ANS transient region above the dictionary)
- Covered by `tests/test-pad-isolation.fth`.

### 5. Brittle Branch Offsets in `COLD_START` (Resolved)
The boot code used hardcoded numeric offsets for `0BRANCH` and `BRANCH`.
- **Status**: Fixed in `6e0b3ba`. Added label-difference support to the assembler and updated `kernel.s` to use computed offsets (e.g., `target - .Lhere`).

---

## Performance & Optimization Opportunities

### 6. Inefficient `MOVE` and `SEARCH` (Resolved)
These were implemented as byte-by-byte loops in `prelude.fth`.
- **Status**: `MOVE` is a kernel primitive calling `memmove()`. `SEARCH` is now
  a kernel primitive using host `memcmp()` over candidate windows (empty
  needle / not-found semantics preserved). Removed the Forth `(STREQ)` helper.

### 7. Native `MULHU` for `UM*` (Resolved)
- **Status**: `UM*` already uses `mulhu` (unsigned high multiply). `M*` continues
  to use signed `mulh`. No further change required.

### 8. `FIND` Efficiency
The `FIND` word performs a linear search through a linked list of word headers.
- **Note**: While standard for Stage 4, as the dictionary grows (prelude + user code), lookups will slow down significantly.
- **Future**: Consider a hashed dictionary or a more efficient search structure for Stage 5. Not blocking; stage01 tools remain usable.

---

## Usability & Diagnostics

### 9. Vague Error Messages (Resolved)
When a word was not found, the interpreter printed only `?`.
- **Status**: Fixed in `ba29655`. The interpreter now prints the offending word name (e.g., `XYZZY ?`) before the newline.

### 10. Stack Size Limits (Resolved)
- **Status**: Data and return stacks are **8192 bytes (2048 cells)** each
  (was already 4 KB; raised further for deep recursion / nested `DO`).

---

## Hardening (this pass)

| Area | Change |
|------|--------|
| `HOLD` | Aborts if the pictured buffer underflows past `pno_buf` |
| `PICK` | Aborts if the index would read past `dstack_top` |
| `WORD` / `:` / `CREATE` | Length caps prevent writing past `word_buf` / 7-bit header length |
| Buffer split | Parse, pictured numbers, and user `PAD` no longer alias |
