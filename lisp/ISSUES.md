# Lisp Interpreter Issues

## 1) GC Root Stack Overflow (High) — RESOLVED
- File: `lisp/src/heap.h`, `lisp/src/heap.c`
- Problem: `PUSH_ROOT(v)` incremented `root_sp` with no bounds check.
- Fix: `PUSH_ROOT` now calls `push_root_checked()` which fatals on overflow.

## 2) GC Mark Stack Overflow Drops Reachable Objects (High) — RESOLVED
- File: `lisp/src/heap.c`
- Problem: `mark_push` silently ignored pushes when `MARK_STACK_SIZE` was exceeded.
- Fix: `mark_push` now fatals on overflow instead of silently dropping reachable objects.

## 3) `number->string` Undefined Behavior on `INT_MIN` (Medium) — RESOLVED
- File: `lisp/src/builtin.c`
- Problem: Conversion path negated signed `int` (`n = -n`) for negative values.
- Fix: Convert through unsigned math (`-(n+1) + 1u`). Regression test `16-minint` added.

## 4) `env_lookup` Error Path Assumes Symbol Type (Low)
- File: `lisp/src/env.c`
- Problem: Unbound-variable message dereferences `AS_OBJ(sym)->symbol.name` without validating type.
- Risk: Malformed call paths can crash while reporting the original error.
- Suggested fix: Guard with symbol type check and use generic message for non-symbol values.

## Regression Tests To Add
- `number->string` with minimum integer value.
- Deep recursion / nested call stress test to exercise root stack and mark stack limits.

---

## February 2026 Review Findings

### 5) Fixed-Size Buffers in Reader (Medium) — RESOLVED
- `read_string` now uses a dynamically-grown malloc'd buffer (starts at 256, doubles as needed).
- `read_symbol_or_number` now errors on symbols exceeding 255 chars instead of silently truncating.

### 6) Symbol Table Capacity (Low)
- `MAX_SYMBOLS` is currently set to 512. Larger programs or extensive libraries may reach this limit, causing an "out of memory" or "too many symbols" error.

### 7) Reader Dotted Pair Naming Constraint (Low)
- `.` is not allowed within symbols, preventing naming conventions like `pkg.func`.

### 8) Architectural Strengths (Observation)
- **Iterative Mark Phase:** The mark phase is iterative rather than recursive, preventing host C stack issues during GC.
- **Comprehensive TCO:** Explicit `goto tail` in `eval` for control structures and lambda applications allows for infinite tail recursion.
- **Tagged Value System:** Efficient 31-bit fixnum representation using bit 0 tagging.

### 9) Optimization & Expansion Opportunities
- **Bytecode VM:** Compiling to stack-based bytecode would likely improve performance over the recursive-descent `eval`.
- **Heap Growth:** Replacing the current `malloc`-per-object strategy with a block-based allocator would reduce overhead.
- **Library Prelude:** Implementing a `prelude.scm` for high-level functions (`filter`, `fold`, etc.) would improve usability.
