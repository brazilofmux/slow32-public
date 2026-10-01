# User-defined functions — and the C bridge

## Stage B: user-defined functions under `-std=2002` (2026-09-28)

The ruling below (2026-08-30) was about majesty, and it stands for
majesty: its corpus is COBOL 85. The compiler now implements COBOL 2002
user-defined functions anyway, for the standard's sake, as Stage B's
second module (docs/standards.md; cobol ISSUES-50). Under `-std=85`,
the default, `FUNCTION-ID` and `REPOSITORY` are still refused.

**Accepted.** `FUNCTION-ID. name.` with `PROCEDURE DIVISION USING ...
RETURNING item` and `END FUNCTION name`; parameters BY REFERENCE (the
default); the RETURNING item a level 01 or 77 in LINKAGE (2023 14.2.2
rule 5). `REPOSITORY. FUNCTION name ...` lets a program invoke `name(args)`
without the word FUNCTION; `FUNCTION ALL INTRINSIC` (or `FUNCTION name
... INTRINSIC`) does the same for intrinsics. A function is always
recursive: LOCAL-STORAGE, LINKAGE and the rest are per activation, as
for a RECURSIVE program (ISSUES-49).

**Arguments** (2023 8.4.3.2.4 rule 5). An identifier the program could
store into goes BY REFERENCE and must be described as the parameter is
(14.8.2.3): PICTURE, USAGE, SIGN, JUSTIFIED and BLANK WHEN ZERO, the
pictures compared as analysed. One extension: two's-complement binary
integers of the same size and signedness conform whatever the spelling
(`PIC S9(8) COMP-5` and `SIGNED-INT`), as GnuCOBOL takes them and as
majesty's `holidays` needs. A literal, an arithmetic expression or
another function's result goes BY CONTENT, into a copy described as the
parameter, converted as COMPUTE would (14.8.2.3.3 rule 2a) -- GnuCOBOL 4
gets this wrong for expressions and negative or short literals
(docs/oracles.md).

**Where the call happens.** Outside a condition, where the operand is
parsed: the compiler emits in execution order. Inside a condition, the
calls are kept with the condition and made each time it is evaluated,
so `PERFORM UNTIL f(x) > 3` and a `WHEN` call it every time. VARYING's
BY and an AFTER's FROM are evaluated at every step and reset, not where
they are parsed; a function there is refused until they are deferred
the same way. The rule is item identification's (2023 14.6.4): the
identifiers in a statement are evaluated left to right as the first
operation of its execution, function evaluation and subscript
evaluation among the steps. So a statement's calls are made once,
first, in the order written -- wherever they stand: an operand, a
reference modifier's start or length, an expression's operand, another
call's argument (that one first). Each call's code is placed before the
statement's own, whatever the verb had already emitted when it read the
call (`DISPLAY "a" F(X)` calls F before it shows "a"). "Unless
otherwise specified", the rule says, and receiving items are: a MOVE's
receiver is identified immediately before the move to it, an arithmetic
statement's as each is accessed, READ INTO's after the record is read.
A function in such a receiver's subscript is called there, once, and
sees what the statement has stored so far: `MOVE 2 TO N T(F(N))` moves
to T(2). A compile-time guard stops any result from a scan reaching
code without its call. An EVALUATE subject
is evaluated once, at the beginning (14.9.13.4 rule 3), so its calls
are made there and not again for each WHEN (docs/plans/
frontend-pass.md).

**The external repository.** A caller needs the function's RETURNING
description to build its result, and a separately compiled function is
not in its source. Compiling a function writes `name.s32fn` beside the
output: the RETURNING item and each parameter, one line each. A caller
finds a function defined earlier in its own source first, then a
`.s32fn` beside its output, beside its source, or on `-I`. `s32-cobc
-fnsig` writes the signatures and nothing else, skipping every
procedure body so it needs no other signature; `compile.sh -std=2002`
runs it over all its inputs first, so the order of the files does not
matter. Call sites stay compile-time specialized: the result is an
ordinary item of known description, not a runtime-typed one.

**Calling sequence.** The C ABI of `CALL`: the arguments' addresses in
r3 onward, then the address of the caller's result temporary, which
becomes the function's RETURNING item. The temporary is LOCAL-STORAGE
in a program that can be re-entered (the same call site in two
activations), static otherwise.

**Not implemented yet, refused with a message:** prototypes (`IS
PROTOTYPE`), function pointers, `AS literal`, BY VALUE parameters of a
function. (A program's BY VALUE and OPTIONAL parameters, OMITTED
arguments and PROCEDURE DIVISION RETURNING are implemented:
docs/conformance/call.md; ANY LENGTH parameters, of functions and
programs, are too: docs/conformance/data-division.md.)

**Real code.** `tests/majesty-functions.sh` takes majesty's date family
as it was written in COBOL 2002 -- twelve functions across seven files,
from majesty's history, unchanged -- and builds it with `-std=2002` and
with GnuCOBOL: jerm's 400,001 lines and the gltrans trio over 3,000
synthetic transactions print the same bytes.

## The C bridge (2026-08-30) — and why majesty's functions became CALLs


**Finding, 2026-08-30:** the corpus reaches C through COBOL 2002
user-defined functions, not `CALL`. gl030 does `move
c_lineartofielded(ltf_lineardate) to result`, where `c_lineartofielded`
is a `FUNCTION-ID` program in `clinkages.cbl` that in turn `CALL`s
`'du_lineartofielded'` (C, `~/majesty/src/c/dateutil.c`). menu does
the same through `taskdt()`. Earlier drafts of these specs said
"`CALL` of `c_lineartofielded`" — wrong on both counts.

**Ruling, same day:** the compiler does **not** grow `FUNCTION-ID`.
**The corpus is rewritten to COBOL 85**, and the compiler stays at
X3.23-1985 + the 1989 intrinsic amendment + the implementor modules
already named. The 2002 features in majesty were written to give
GnuCOBOL 2002 coverage at a time when that mattered; it no longer
does, and this compiler exists to retire GnuCOBOL, not to inherit
its dialect.

## What the rewrite is

Every user function becomes a subprogram, every invocation becomes a
`CALL`, and `REPOSITORY` disappears. It is mechanical and it is
semantics-preserving:

| today (2002 / GnuCOBOL) | after (1985) |
|---|---|
| `function-id. c_lineartofielded.` | `program-id. c_lineartofielded.` |
| `procedure division using x returning result.` | `procedure division using x result.` |
| `end function c_lineartofielded.` | `end program c_lineartofielded.` |
| `move c_lineartofielded(ltf_lineardate) to result` | `call 'c_lineartofielded' using ltf_lineardate result` |
| `move taskdt() to option` | `call 'taskdt' using option` |
| `repository. function c_lineartofielded / function all intrinsic.` | *(deleted)* — intrinsics are invoked as `FUNCTION name(…)` under the 1989 amendment and need no repository |
| `if is-valid then` | `if is-valid` |

A `RETURNING` group that was returned by value is now a `USING`
argument filled in place. Same bytes, same effect. A former function's
`WORKING-STORAGE` is now a subprogram's — static across calls — which
is what 85 says and what every one of these routines is already safe
under (taskdt rebuilds everything from `CURRENT-DATE` on each call).
The "activation semantics" question this document used to carry is
gone.

### Landed: ~/majesty commit `1da955d` (2026-08-29)

Done in one scripted pass over 18 files, wider than the v1 path: all
seven C-bridge functions in `clinkages.cbl` (`c_isvaliddate`,
`c_fieldedtolinear`, `c_lineartofielded`, `c_newyear`, `c_yearend`,
`c_dayofweek`, `c_kdayonorbefore`) plus `taskdt`, and every caller
(`gl024`, `gl030`, `gl034`, `gl036`, `gl038`, `gl040`, `gl042`,
`gl043`, `holidays`, `jerm2`, `ldgltrans`, `menu`, `today`, `w001`,
and the retired `gl015`/`gl016`). Every `REPOSITORY` that named only
converted functions is gone, including its `FUNCTION ALL INTRINSIC`
(the compiler flagged nothing, so no bare intrinsic remained beyond
taskdt's two `length(`). The inner `call 'du_*' using by value ...
returning ...` **stays** -- see below.

Verified the way this document asked: GnuCOBOL build clean of
warnings; `batch.sh` unchanged; all 12 `reports_cobol/*.prn`
byte-identical to a baseline taken from the untouched source minutes
earlier; `run_tests.sh` verdicts identical to that baseline. So the
oracle `.prn` files this compiler is gated on are now produced from
1985 source, and stage 6 is open.

**The one non-mechanical finding, worth keeping:** `MOVE f(x) TO y`
and `CALL 'f' USING x y` differ when `y` is *smaller* than the
function's `RETURNING` item. `c_lineartofielded` returns an 11-byte
group (five `SYNC` shorts and a flag); `taskdt` and `today` received
it into the 10-byte `fielded-date`. The `MOVE` truncated the flag
silently; a by-reference `CALL` would have written it one byte past
the receiver. Both sites now call into a correctly-shaped temporary
and `MOVE` the group part across. Every other receiver was checked
against its function's `RETURNING` item and matched in size and
layout. Any future rewrite of this kind has to make that check --
GnuCOBOL will not, and neither will this compiler, because
by-reference `CALL` is *defined* to overlay whatever it is handed.
`jerm2`'s `if c_isvaliddate(...) = 0` needed a `pic x` temporary for
the same reason `fielded_to_linear`'s sites do (below); the odd
comparison itself was left exactly as it was.

### What is still 2002, and why it waited

The pure-COBOL date family -- `fielded_to_linear.cbl`,
`linear_to_fielded.cbl`, `isvaliddate.cbl`, `isleapyear.cbl`,
`floor-div.cbl`, `floor-divmod.cbl`, `holidays.cbl`'s inner units and
their callers (`jerm`, `exgltrans`) -- still uses `FUNCTION-ID`.
Those invocations sit inside arithmetic and conditions
(`add floor-div(fdm-x, c4) to gtl-linear`,
`if isleapyear(gtl-year) = 'Y'`,
`subtract 584389 from gregorian_to_linear(year, month, dom) giving
linear`, `move linear_to_gregorian(584389 + linear) to fielded` with an
expression argument), so each needs a temporary and a `CALL` hoisted
ahead of the statement, and none of them is on `batch.sh`, so the
byte-identical check does not reach them. Separate pass, own test
plan, after v1 -- see [plan.md](plan.md).

## What stays, as the C-ABI implementor module

Talking to C at all needs things 1985 does not have, and those stay —
they were already in the dialect as the C-ABI bucket:

- `USAGE COMP-5`, `SIGNED-INT`, `SIGNED-SHORT`, `UNSIGNED-SHORT`,
  `BINARY-CHAR [UNSIGNED]`, `POINTER`, `SYNC` reproducing a C struct
- `CALL … USING BY VALUE item` — a scalar in an argument register,
  `signed-short`/`unsigned-short` widened to a word, which is what
  `du_isvaliddate(int, int, int)` expects
- `CALL … USING BY REFERENCE item` — the 85 default, named explicitly
- `CALL … RETURNING item` — `r1` into a `signed-int`

`BY VALUE` and `RETURNING` on `CALL` are 2002 too, but they are the
seam to C rather than COBOL logic, and the alternative — rewriting
`dateutil.c` to take every argument by pointer — moves the
non-standardness into a C file majesty shares with the C++ stack.
Confined to `clinkages.cbl` they cost the compiler one clause each.

SLOW-32 ABI: `r3`–`r10` arguments, `r1` return. `dateutil.c` is
portable C (`stdio.h`, `stdbool.h`, `limits.h`, `memory.h`, one
`memset`; all four headers exist in `~/slow-32/runtime/include/`).
Compile it with the SLOW-32 clang and link it into every program that
reaches a `du_*` name. `rs.c`, `csvgen.c`, `csvparser.c`, `crc.c` wait
for a program that calls them.

## Why the rewrite is cheap: the conventions were never COBOL's

GnuCOBOL transpiles to C, so its `CALL`, its `FUNCTION-ID` and its
`RETURNING` are all C calls wearing COBOL syntax. Nothing in majesty
depends on any *COBOL* calling convention — there was none. That is
exactly why every 2002 form above collapses to `CALL … USING` with no
semantic loss, and why the only place C's convention has to survive
is the two-line seam in `clinkages.cbl`. The COBOL-to-COBOL
convention this compiler uses is its own choice; see
[lowering.md](lowering.md), "whose convention?".

## Linking shape

One `.s32x` per `PROGRAM-ID`, as cobc370 makes one load module per
program. Each links the subprograms its `CALL` literals name —
`gl030` links `clinkages`; `menu` links `taskdt`, which links
`clinkages` — plus the SLOW-32 build of `dateutil.c`. GnuCOBOL's
single `MAJESTY.so` holding every program is a `cobcrun -M` packaging
choice, not a language fact; see
[majesty-corpus.md](majesty-corpus.md).
