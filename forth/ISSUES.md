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
    for `WORD`, 127 for dictionary headers (IMMEDIATE uses length bit 7;
    31 since 2026-10-10, see "Closed the same day" below)
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

### 8. `FIND` Efficiency (Resolved 2026-10-10)
The `FIND` word performs a linear search through a linked list of word headers.

- **Note**: While standard for Stage 4, as the dictionary grows (prelude + user code), lookups will slow down significantly.
- **Future**: Consider a hashed dictionary or a more efficient search structure for Stage 5. Not blocking; stage01 tools remain usable.  (Done 2026-10-10: the `dict_find` cache, see "Closed the same day" below.)

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

## Forth 2012 pass (2026-10-10)

The gforth-shipped Forth 2012 suite (`tests/run-forth2012.sh`) was run
against the kernel for the first time: 136 wrong results and 105 undefined
words in coretest alone at the start, 0 and 0 at the end across all eight
files (residue: gforth's `ROOT` wordlist, floating point).  README.md
"Forth 2012 conformance" lists the behaviour that changed.  Defects it
exposed, all resolved in this pass:

| Item | Defect |
|------|--------|
| 11 | Comparisons returned 1 for true; `SEARCH` too |
| 12 | `HERE` pushed the address of the pointer variable; `PAD` (`HERE 128 +`) overlapped `interp_saved_ip`, and `PAD 5 BLANK` took the interpreter down once the prelude had grown |
| 13 | `FIND` returned `0 0` on failure and `xt 1` for every hit |
| 14 | `WORD` ignored its delimiter, uppercased, and shared `(WORD)`'s buffer; tabs did not delimit; `>IN` was left on the delimiter |
| 15 | `NUMBER` accepted characters below `0` as negative digits, so `.(`'s own `( "text)" -- )` comment compiled |
| 16 | `'` `[']` `POSTPONE` left 0 for an undefined name; `IS` then wrote through address 8 |
| 17 | `:` was IMMEDIATE (a defining word could not contain `:`), and no smudge: a definition found itself, an aborted one stayed callable (the fault at PC 0xff0e8e90 seen all over) |
| 18 | `WHILE` left (dest orig): a second `WHILE` took the first's patch cell as the loop start |
| 19 | `IF`..`+LOOP` `DOES>` did nothing when STATE was 0, so `[ POSTPONE-X ]` compiled nothing and `pWEIRD:` lost its `DOES>` |
| 20 | `.` printed `/` for `HEX -1`; `UM/MOD` dropped the remainder's 33rd bit; `*/MOD` floored while `/` truncated |
| 21 | `FILL` with a zero count filled 4 GB (`0 DO`) |
| 22 | `CATCH` did not save the input source: an `EVALUATE` that threw left the interpreter reading the evaluated string |
| 23 | `REFILL` set neither `#TIB` nor `>IN`; no `[IF] [ELSE] [THEN]`; no double literals; no `ENVIRONMENT?` of use (an older one at the end of the prelude shadowed the new one for a while); no `DEFER!` `DEFER@` `DU<` `D2*` `D2/` `M*/` |
| 24 | `POSTPONE S"` and `ABORT"` under `[ ]` interpreted the string (STATE-smart `S"`): `(S")` added |

Found on the way, in selfhost: cc.fth's `STRUCT-SIZE` called `TYPE-SIZE`
and `ALIGN-UP` before they were defined.  The old interpreter printed
`TYPE-SIZE ?` and carried on compiling the rest of the line, so the two
calls were simply missing from the compiled word (selfhost ISSUES-85).

Closed the same day, in a second step:

- Compile-only is a header bit (bit 5 of the length byte, `COMPILE-ONLY`
  sets it), checked by the text interpreter on a direct interpretation, as
  the standard's is -- so `[ POSTPONE-+LOOP ]` runs while a bare `IF` at the
  prompt is `IF is compile-only` and `-14 THROW`.  The "no definition open"
  heuristic is gone.  The bit cost the name-length cap: 31 characters, the
  standard's minimum; nothing in the tree had a longer name, and a longer
  one is kept and looked up as its first 31.
- Item 8: `FIND` and `SEARCH-WORDLIST` share `dict_find`, a direct-mapped
  1024-entry cache {hash, wid, header, epoch} in front of the linked list
  (FNV-1a over the folded name).  Hits are verified by name; misses are
  cached with the epoch and every new header bumps it; `MARKER` clears the
  table (`DICT-CACHE-CLEAR`) because it moves DP back.  Loading cc.fth:
  112M -> 39M instructions.  Compiling stage02's cc-min-pass1.c with cc.fth:
  826M -> 753M, output byte-identical -- the compiler's own work dominates
  that run, so this is a load-time win mostly.

Still open: ttester.fs' no-FP-stack branch names `0E FDROP F@ FCONF=`; the
gate stubs them.  Floating point is not in this kernel's scope.
