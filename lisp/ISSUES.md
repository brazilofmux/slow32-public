# Lisp Interpreter Issues

## 1) ~~GC Root Stack Overflow~~ — FIXED
- File: `lisp/src/heap.h`, `lisp/src/heap.c`
- Fixed: `PUSH_ROOT` now calls `push_root_checked()` which fatals on overflow.

## 2) ~~GC Mark Stack Overflow~~ — FIXED
- File: `lisp/src/heap.c`
- Fixed: `mark_push` now fatals on overflow instead of silently dropping reachable objects.

## 3) ~~`number->string` UB on `INT_MIN`~~ — FIXED
- File: `lisp/src/builtin.c`
- Fixed: Conversion uses unsigned logic (`-(n+1) + 1u`) to safely handle `INT_MIN`.

## 4) ~~`env_lookup` Error Path Assumes Symbol Type~~ — FIXED
- File: `lisp/src/env.c`
- Fixed: Guarded with `IS_PTR` + type check; falls back to generic message for non-symbol values.

## 5) ~~Fixed-Size Buffers in Reader~~ — FIXED
- File: `lisp/src/reader.c`
- Fixed: `read_string` uses dynamic reallocation; `read_symbol_or_number` reports error on overflow.

## 6) ~~Symbol Table Capacity~~ — FIXED
- File: `lisp/src/heap.c`
- Fixed: `MAX_SYMBOLS` increased to 1024.

## 7) Reader Dotted Pair Naming Constraint (Low)
- Problem: `.` is not allowed within symbols, preventing naming conventions like `pkg.func`.
- Status: PENDING.

## 8) Architectural Strengths (Observation)
- **Iterative Mark Phase:** The mark phase is iterative rather than recursive, preventing host C stack issues during GC.
- **Comprehensive TCO:** Explicit `goto tail` in `eval` for control structures and lambda applications allows for infinite tail recursion.
- **Tagged Value System:** Efficient 31-bit fixnum representation using bit 0 tagging.

## 9) Optimization & Expansion Opportunities
- **Bytecode VM:** Compiling to stack-based bytecode would likely improve performance over the recursive-descent `eval`.
- **Heap Growth:** Replacing the current `malloc`-per-object strategy with a block-based allocator would reduce overhead.
- **Library Prelude:** Implementing a `prelude.scm` for high-level functions (`filter`, `fold`, etc.) would improve usability.
