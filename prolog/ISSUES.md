# Prolog Interpreter Issues

Review date: February 12, 2026.

Current test status: 16/16 passing (Stabilized core + arity guards).

## 1) `findall/3` and `\+/1` Corrupt Engine State (Resolved 2026-08 Pack E)
- **Problem:** Nested solve loops overwrote `goal_stack` / `choices` without
  fully preserving parent content (`\+` only saved 64 entries; `findall`
  restored pointers only).
- **Status:** Fixed. `engine_snapshot_stacks` / `engine_restore_stacks`
  heap-copy the full parent stacks around nested search. Regression:
  `tests/nested-isolation.pl` (70× `true` then findall / `\+`).

## 2) OOB Read/Write in Term Copying (Resolved)
- **Previous Problem:** `persist_term()` and `copy_term_impl()` used fixed-size `args[32]` buffers and could call constructors with larger arity.
- **Fix:** Added explicit `PROLOG_MAX_ARITY` guards in parser, constructors, `copy_term_impl()`, `persist_term()`, `functor/3`, and `=../2`.
- **Regression:** `tests/arity-limits.*` covers runtime arity overflow paths (`functor/3`, `=../2`).

## 3) Unification has no occurs-check (High)
- **Problem:** `unify()` binds variables without checking whether the variable appears inside the target term (`X = f(X)`).
- **Risk:** Cyclic terms can break traversal/printing and cause non-termination in recursive walkers.
- **Suggested Fix:** Add `occurs_check(var_id, term)` before variable binding.

## 4) Hardcoded Arity Limit in Parser (Low / Design Limit)
- **Current Behavior:** Parser enforces `PROLOG_MAX_ARITY` (currently 32) and reports a clear error when exceeded.
- **Risk:** High-arity source terms are rejected by design.
- **Suggested Fix (optional):** Raise `PROLOG_MAX_ARITY` (e.g., 255) or move to growable argument vectors.

## 5) Atom Store Overflow Aliases Atom 0 (Resolved)
- **Previous Problem:** `store_string()` returned `atom_names[0]` fallback when full.
- **Fix:** Overflow now sets error and fails allocation path instead of aliasing atom identity.

## 6) Deep recursion still relies on C stack (Low)
- **Problem:** Parser, arithmetic evaluator, term copy/persist, and writer still recurse on host C stack.
- **Risk:** Stack overflow with deep terms/programs.
- **Suggested Fix:** Move recursive paths to explicit iterative stacks.

## 7) `findall/3` Hard-Limits Results to 256 (Medium)
- **Problem:** `findall/3` stores matches in a fixed-size `persisted[256]` array.
- **Risk:** Result sets larger than 256 are silently truncated.
- **Suggested Fix:** Build the output list incrementally on heap/code-heap, or use growable temporary storage.

---

## February 2026 Additional Review Findings

### 8) `persist_term` Recursion Depth Risk (Opportunity)
- **Problem:** `persist_term` in `term.c` recurses for every argument of a compound term.
- **Risk:** Deeply nested terms can drive C stack growth in conversion paths.
- **Suggested Fix:** Optional iterative rewrite with an explicit term stack.

### 9) `atom_intern` Search Performance (Opportunity)
- **Problem:** `atom_intern` uses a linear search over `atom_names`.
- **Risk:** Performance will degrade as the number of atoms grows toward `MAX_ATOMS (2048)`.
- **Suggested Fix:** Implement a simple hash table for atom lookups.

---

## Completed / Fixed Items

- **~~findall/3 stored invalid term pointers~~** — FIXED: Now uses `persist_term()` to move results to code heap.
- **~~Recursion/backtracking regression~~** — FIXED: Solver stabilized, all 15 regression tests pass.
- **~~copy_term could corrupt var_count~~** — FIXED: Added bounds checks before increment.
- **~~Variable safety checks incomplete~~** — FIXED: `deref()` and `bind()` now validate against `var_count`.
- **~~OOB in term copying for arity > 32~~** — FIXED: Global arity guards now prevent unsafe construction/copy.
- **~~Atom store overflow alias fallback~~** — FIXED: overflow now errors instead of aliasing atom 0.
