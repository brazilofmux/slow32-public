# Lisp Interpreter Issues

## 1) GC Root Stack Overflow (High)
- File: `lisp/src/heap.h`
- Problem: `PUSH_ROOT(v)` increments `root_sp` with no bounds check.
- Risk: Deep recursion or nested evaluation can write past `root_stack`, causing memory corruption.
- Suggested fix: Replace macro-only push with a checked helper (`push_root_checked`) that errors hard on overflow.

## 2) GC Mark Stack Overflow Drops Reachable Objects (High)
- File: `lisp/src/heap.c`
- Problem: `mark_push` silently ignores pushes when `MARK_STACK_SIZE` is exceeded.
- Risk: Reachable objects can remain unmarked and be swept, leading to use-after-free/corruption.
- Suggested fix: Treat overflow as fatal GC error, or make mark stack dynamically grow.

## 3) `number->string` Undefined Behavior on `INT_MIN` (Medium)
- File: `lisp/src/builtin.c`
- Problem: Conversion path negates signed `int` (`n = -n`) for negative values.
- Risk: `INT_MIN` cannot be represented as positive `int`, causing signed overflow (UB).
- Suggested fix: Convert through unsigned math or `long long`, and add regression for minimum integer.

## 4) `env_lookup` Error Path Assumes Symbol Type (Low)
- File: `lisp/src/env.c`
- Problem: Unbound-variable message dereferences `AS_OBJ(sym)->symbol.name` without validating type.
- Risk: Malformed call paths can crash while reporting the original error.
- Suggested fix: Guard with symbol type check and use generic message for non-symbol values.

## Regression Tests To Add
- `number->string` with minimum integer value.
- Deep recursion / nested call stress test to exercise root stack and mark stack limits.
