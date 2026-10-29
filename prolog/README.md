# prolog — a small Prolog interpreter for SLOW-32

A Prolog interpreter in about 2,900 lines of C (`src/`), built with
clang for SLOW-32 and run on the emulators. It is a core of
Edinburgh (DEC-10) Prolog, not ISO Prolog: clauses, unification,
backtracking, cut, `;`, `->`, `\+`, `assert`/`retract`, `findall/3` and
term inspection, with SWI-Prolog's top-level conventions (`X = 3`,
`true.`, `false.`) and a few SWI names (`writeln/1`, `is_list/1`,
`succ/2`, `plus/3`). There are no exceptions, no floats, no `op/3` and
no list library. Terms are tagged 32-bit words; integers are 30-bit.

```bash
./build.sh                 # clang + llc from ~/llvm-project/build/bin; OPT=-O2 ./build.sh to change -O1
./tests/run-tests.sh       # EMU=... to use another engine (default slow32-fast)
```

`build.sh` compiles each `src/*.c`, assembles and links `prolog.s32x`
with `libc_mmio`, a 64K MMIO window, a 128K stack and 5M of data.
`prolog.s32x` is tracked, so rebuilding changes a tracked file.

## Running

The interpreter reads one program from standard input, clauses and
queries interleaved, and has no prompt and no `consult/1`:

```bash
../tools/emulator/slow32-fast prolog.s32x < program.pl
echo '?- write(hello), nl.' | ../tools/emulator/slow32-fast prolog.s32x
```

```prolog
max(X, Y, X) :- X >= Y, !.
max(_, Y, Y).

?- max(3, 5, M), write(M), nl.
```

prints

```
5
M = 5
true.
```

A query (`?- G.`) reports its first solution only: the named bindings,
then `true.`, or `false.`. A directive (`:- G.`) runs `G` and prints
`Error: directive failed` if it fails. Anything else is added to the
database. `halt/0` and `halt/1` exit; end of input also ends the run.

## Tests

`tests/run-tests.sh` pipes each `tests/*.pl` through the emulator and
compares with `tests/*.expected`. Result on 2026-10-29: **17/17
passed**, both with the tracked `prolog.s32x` and with a fresh
`./build.sh`. The expected files are the interpreter's own output, not
an external oracle.

## Builtins

- Control: `true`, `fail`, `,`, `;`, `->` (with and without else), `\+`, `!`
- Unification and comparison: `=`, `\=`, `==`, `\==`
- Arithmetic: `is`, `=:=`, `=\=`, `<`, `>`, `=<`, `>=`; evaluable
  `+ - * // mod min max abs` and unary `-`
- Types: `atom/1`, `integer/1`, `number/1`, `var/1`, `nonvar/1`,
  `compound/1`, `is_list/1`
- Terms: `functor/3`, `arg/3`, `=../2`, `copy_term/2`
- Atoms: `atom_length/2`, `atom_chars/2`, `char_code/2`,
  `number_chars/2`, `atom_concat/3` (both arguments bound)
- Database: `assert/1`, `asserta/1`, `assertz/1`, `retract/1`
- All-solutions: `findall/3`
- Output: `write/1`, `writeln/1`, `write_canonical/1`, `nl/0`
- `succ/2`, `plus/3` (first two bound), `halt/0`, `halt/1`

## What it does not do

Things a user will trip over, found by running the interpreter:

- **Cut is not local to its clause.** `!` cuts every choice point back
  to the start of the query, including ones created before the call.
  `b(1). b(2). c(1). c(2). once_b(X) :- b(X), !.` then
  `?- c(Y), once_b(X), Y > 1.` answers `false.`.
- **`f (...)` is read as `f(...)`.** The reader does not tell a prefix
  operator followed by a space and `(` from a functor call, so
  `?- (A ; B).` is a syntax error and `\+ (A, B)` is a call to `\+/2`,
  which fails. Inside a clause body after `:-` the parentheses work;
  at the top of a query, write `?- true, (A ; B).`.
- No `/` (use `//`), no `-` prefix operator on expressions
  (`X is - 3` is an error; `-3` and `-(3)` work), no floats, no
  bignums: integers wrap at 30 bits (`536870911 + 1` is `-536870912`).
- No `call/N`, `catch/3`, `throw/1`, `op/3`, `read/1`, `atom_codes/2`,
  `between/3`, `length/2`, `append/3`, `member/2` or other list
  predicates; define the list predicates yourself (`tests/lists.pl`
  does). A bound variable can be called as a goal (`G = write(hi), G`).
- An undefined predicate fails silently; there is no existence error.
- `"text"` is read as an atom, not a code list. Quoted atoms take `''`
  but no backslash escapes.
- No occurs check: `X = f(X)` succeeds and prints a truncated cyclic
  term (prolog ISSUES-3).
- Fixed limits: arity 32, 2,048 atoms, 2,048 choice points, 256
  variables per clause, `findall/3` keeps at most 256 results and drops
  the rest (prolog ISSUES-4, ISSUES-7).
- A syntax error resynchronises badly: the rest of the line can be
  read as a further clause and give a second error.

## Pointers

- [ISSUES.md](ISSUES.md) — open items (occurs check, findall limit,
  C-stack recursion, atom lookup) and fixed ones.
- [../docs/AUDIT-2026-08.md](../docs/AUDIT-2026-08.md) section 7.6 — the
  August 2026 review that led to the nested-query fix (prolog ISSUES-1).
