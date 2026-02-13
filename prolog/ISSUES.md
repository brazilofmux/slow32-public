# Prolog Interpreter Issues

Review date: February 12, 2026.

Current test status from `prolog/tests/run-tests.sh`: 9/15 passing.
Failing tests: `cut`, `findall`, `hanoi`, `lists`, `recursion`, `term-manip`.

## 1) `findall/3` stores invalid term pointers (High)
- File: `prolog/src/builtin.c`
- Problem: `findall/3` stores copied templates in `results[256]`, then restores `hp = saved_hp` before building the output list.
- Risk: Stored result terms can point into reclaimed working heap space, causing corrupted structures and non-deterministic output.
- Evidence: `findall` test currently produces giant malformed nested output.
- Suggested fix: Persist each captured template into stable storage (code heap or dedicated findall heap/list) before heap rewind.

## 2) Recursion/backtracking regression in solver path (High)
- Files: `prolog/src/engine.c`, `prolog/src/builtin.c`
- Problem: Recursive workloads currently fail (`lists`, `recursion`, `hanoi`), returning `false.` where success is expected.
- Risk: Core Prolog behavior for recursive rules is broken.
- Suggested fix: Audit continuation and choice-point restore logic, then add targeted regression tests for recursive conjunction/backtracking patterns.

## 3) Hardcoded arity buffers still unsafe (High)
- Files: `prolog/src/engine.c`, `prolog/src/term.c`, `prolog/src/builtin.c`, `prolog/src/parser.c`
- Problem: Multiple sites use `term_t args[32]` while still constructing terms with larger arity.
- Risk: Uninitialized arguments and memory safety bugs for arity > 32.
- Suggested fix: Allocate argument vectors based on runtime arity, or reject oversized arity uniformly before term construction.

## 4) Unification has no occurs-check (High)
- File: `prolog/src/unify.c`
- Problem: `unify()` binds variables without checking whether the variable appears inside the target term (`X = f(X)`).
- Risk: Cyclic terms can trigger non-termination or printer blowups.
- Suggested fix: Add `occurs_check(var_id, term)` before variable binding.

## 5) `copy_term` can corrupt `var_count` on overflow path (Medium)
- File: `prolog/src/engine.c`
- Problem: `copy_term()` increments `var_count` before bounds failure returns.
- Risk: State drift after overflow errors; later allocations can behave unpredictably.
- Suggested fix: Check capacity before incrementing `var_count`.

## 6) Variable safety checks are incomplete (Medium)
- File: `prolog/src/engine.c`
- Problem: `deref()` validates only `id < MAX_VARS`; `bind()` does not validate `var_id`.
- Risk: Malformed terms can read/write outside active variable range.
- Suggested fix: Validate against both `MAX_VARS` and current `var_count` in `deref()` and `bind()`.

## 7) Negation snapshots truncate engine state at 64 entries (Medium)
- File: `prolog/src/engine.c`
- Problem: `\+/1` saves only first 64 goals and 64 choice points, but restores `goal_sp`/`choice_top` to full previous counts.
- Risk: State corruption when negation is used with deeper stacks.
- Suggested fix: Save full stacks (bounded by compile-time limits), or reject with explicit error when limits are exceeded.

## 8) `findall/3` hard limit of 256 results (Medium)
- File: `prolog/src/builtin.c`
- Problem: `term_t results[256]` limits collected answers.
- Risk: Silent truncation of valid result sets.
- Suggested fix: Build result list incrementally on heap or use growable storage.

## 9) Atom store overflow aliases atom 0 (Low)
- File: `prolog/src/term.c`
- Problem: `store_string()` returns `atom_names[0]` when full.
- Risk: Distinct atoms collapse to the same name, causing misleading behavior.
- Suggested fix: Hard fail on atom store exhaustion instead of alias fallback.

## 10) Deep recursion still relies on C stack (Low)
- Files: `prolog/src/parser.c`, `prolog/src/builtin.c`, `prolog/src/print.c`, `prolog/src/term.c`, `prolog/src/engine.c`
- Problem: Parser, arithmetic evaluator, term copy/persist, and writer still recurse on host C stack.
- Risk: Stack overflow with deep terms/programs.
- Suggested fix: Move critical paths (`copy_term`, `persist_term`, writer) to explicit iterative stacks.
