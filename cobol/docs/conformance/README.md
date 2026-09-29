# Conformance, rule by rule

Each page takes one section of ISO/IEC 1989:2023 (and, where the
statement is in COBOL 85, X3.23-1985) and gives every syntax rule and
general rule a disposition:

| mark | meaning |
|---|---|
| **test** | a program in `tests/` exercises the rule, named |
| **refused** | a `tests/bad/` program shows the violation refused, with a message citing the rule |
| **n/a** | the rule is about a feature ruled out (object orientation, ...), with the ruling |
| **gap** | not implemented; the refusal names it; recorded in docs/refusals.md |
| **ruling** | the text leaves it to the implementor, or is unclear; the choice made and why |

The rules are paraphrased, never quoted: the 2023 text is licensed and
stays out of the tree (docs/standards.md). Section and rule numbers are
enough to find them.

A page is done when every rule has a mark. Sweeping a section finds
three kinds of thing, and each is fixed and tested before the page is
written: rules the compiler does not enforce (the CCVS suite tests what
must be accepted, not what must be refused), behaviour that differs from
the text, and misleading messages. docs/refusals.md, "What follows",
began this; ISSUES-96 on record the sweeps.

| section | page | swept |
|---|---|---|
| 14.9.14 EXIT | [exit.md](exit.md) | 2026-09-28 |
| 13.18.40 PICTURE, 13.18.8 BLANK WHEN ZERO | [picture.md](picture.md) | 2026-09-29 |
| 13.18.60 USAGE (the rest) | [usage.md](usage.md) | 2026-09-29 |
| 14.2, 14.9.4 CALL parameters | [call.md](call.md) | 2026-09-29 |
| 14.7.7, ADD SUBTRACT MULTIPLY DIVIDE COMPUTE | [arithmetic.md](arithmetic.md) | 2026-09-29 |
| 12.4.5, 13.4.5, RECORD, LINAGE: files | [files.md](files.md) | 2026-09-29 |
| 14.9.6/.10/.27/.30/.35/.41/.51 the I-O statements | [io-statements.md](io-statements.md) | 2026-09-29 |
| 14.9.20 INITIALIZE | [initialize.md](initialize.md) | 2026-09-29 |
| 14.9.25 MOVE | [move.md](move.md) | 2026-09-29 |
| 14.9.40, .24, .32, .34 SORT, MERGE, RELEASE, RETURN | [sort.md](sort.md) | 2026-09-29 |
| 13.18.32, .33, .52, .55 JUSTIFIED, level-number, SIGN, SYNCHRONIZED | [clauses.md](clauses.md) | 2026-09-29 |
| 13.18.38 OCCURS | [occurs.md](occurs.md) | 2026-09-29 |
| 13.18.44 REDEFINES | [redefines.md](redefines.md) | 2026-09-29 |
| 13.18.45 RENAMES | [renames.md](renames.md) | 2026-09-29 |
| 13.18.63 VALUE | [value.md](value.md) | 2026-09-29 |
| 14.9.22 INSPECT, 14.9.43 STRING, 14.9.48 UNSTRING | [string.md](string.md) | 2026-09-29 |
| 13.18.29 GROUP-USAGE, 13.18.60 USAGE BIT/NATIONAL, 13.18.40 PICTURE 1/N, 8.3.3.4-5 | [national-boolean.md](national-boolean.md) | 2026-09-29 |
| 14.9.28 PERFORM | [perform.md](perform.md) | 2026-09-28 |
| 14.9.29 RAISE | [raise.md](raise.md) | 2026-09-28 |
| 7.3.25 TURN | [turn.md](turn.md) | 2026-09-28 |
| 14.9.49 USE | [use.md](use.md) | 2026-09-28 |
