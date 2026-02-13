# Lua Port Issues

Review scope: `c1c8e8600bd024df6650c6624aad65a36d805b99..HEAD`

## 1) Uninitialized Exit Status on Empty stdin (High) — FIXED
- File: `lua/src/main.c`
- Fix: Initialize `status = LUA_OK` at declaration. Empty stdin regression test added.

## 2) Silent Failure in Expression Print Path (Medium) — FIXED
- File: `lua/src/main.c`
- Fix: Check `lua_pcall` return from `print`, route through `report`, return `EXIT_FAILURE` on error.

---

## February 2026 Additional Review Findings

### 3) Minimal `strftime` Implementation (Medium)
- File: `runtime/time_stubs.c`
- Problem: `strftime` only supports a small subset of format specifiers.
- Risk: `os.date()` will return incomplete or incorrect strings for standard specifiers like `%b`, `%B`, or `%p`.
- Suggested fix: Expand specifier support to cover full ISO C / POSIX standard.

### 4) `ungetc` is a no-op Stub (Medium)
- File: `runtime/stdio.c`
- Problem: `ungetc` just returns the character without pushing it back into the stream buffer.
- Risk: Scripts performing lookahead character parsing via `io.read()` will lose characters or read them out of order.
- Suggested fix: Implement a 1-character pushback buffer in the `FILE` structure.

### 5) Soft-Float Bottleneck (Low / Performance)
- Observation: SLOW-32 relies on software emulation for all `double` operations.
- Risk: Lua scripts performing intensive mathematics will be significantly slower than logic-heavy scripts.
- Note: This is a hardware limitation, but worth tracking if critical scripts require optimization (e.g., fixed-point math).

### 6) Memory Fragmentation Risk (Low)
- Observation: Lua's incremental GC frequently calls `malloc`/`free`.
- Risk: Potential for heap exhaustion due to fragmentation in `runtime/malloc.c` over long periods.
- Suggested fix: Monitor heap state or consider a block-based arena for Lua's custom allocator.

## Test Gaps
- ~~Add regression for empty stdin to ensure deterministic `EXIT_SUCCESS`.~~ DONE
- Add regression for single-line expression mode where `print` fails (e.g. override `print`) and assert non-zero exit.
