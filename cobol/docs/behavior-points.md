# Behavior points

A behavior point is a construct whose treatment depends on which
standard year, or which dialect, a program was written for. Each one has
a stable id. The id appears here, in the compiler's message, and in the
tests, so the three stay in step.

The rule for the compiler: every site that meets a point calls
`bp(point, line)` in `src/s32-cobc.c`, and the policy (silent, warn, and
later refuse) lives in that one function and its table, keyed by `-std`
and the warning flags. A site never decides for itself. That is what
lets a new switch be one table change instead of a hunt through 9,500
lines.

Plan and reasoning: [standards.md](standards.md).

## Switches

| switch | today |
|---|---|
| `-std=85` (also `-std=cobol85`) | the default and the only standard implemented: X3.23-1985 and the X3.23a-1989 intrinsic functions |
| `-std=74` | refused: 74 programs compile as 85, and `-warn-74` flags where their meaning changed; full COBOL 74 is cobc370's job |
| `-std=2002`, any other | refused as not implemented; COBOL 2002 is Stage B of standards.md |
| `-warn-74` | warns at every class M, O and N point below; never changes the output |

Without `-warn-74` the compiler is silent at every point, and its
assembler is byte-identical to what it was before the registry existed
(checked across the whole suite, 113 programs, when this landed).

## Class M — COBOL 85 changed the meaning

The 85 meaning is applied. A 74 program compiles cleanly here and
computes something different, which is why these are the points that
matter most.

| id | construct | COBOL 85 (applied) | COBOL 74 | detected when |
|---|---|---|---|---|
| BP-M1 | `PERFORM VARYING ... AFTER` | the outer item is augmented, then the inner one is reset from its FROM | the inner item is reset first, from the outer item's old value | an AFTER item's FROM is an outer VARYING item of the same statement |
| BP-M2 | a receiving group holding an `OCCURS DEPENDING ON` table | the group's maximum length | its current length | the whole group (no subscript, no reference modification) receives a MOVE, or anything lowered as one (`READ ... INTO`) |

BP-M1 is detected by the same symbol only. An outer item reached
through `REDEFINES` or `RENAMES` would change the bounds just the same
and is not flagged yet.

## Class O — obsolete in COBOL 85, deleted in COBOL 2002

Accepted under `-std=85`, as the 1985 text requires. They are what a
74-era program most often carries, and none survives into 2002, so
`-warn-74` names them to encourage the update.

| id | construct | effect here |
|---|---|---|
| BP-O1 | `ALTER` (and the bare `GO TO` it rewrites) | implemented |
| BP-O2 | comment-entries: `AUTHOR.`, `INSTALLATION.`, `DATE-WRITTEN.`, `DATE-COMPILED.`, `SECURITY.` (and `REMARKS.`) | comments |
| BP-O3 | `STOP literal` | the literal is displayed and the run goes on |
| BP-O4 | `OPEN ... REVERSED` | implemented |
| BP-O5 | `MEMORY SIZE` | no effect |
| BP-O6 | `LABEL RECORDS` | no effect |
| BP-O7 | `VALUE OF` | no effect |
| BP-O8 | `DATA RECORDS` | no effect |

Already refused rather than accepted, so not points: section segment
numbers (Segmentation), and `ENTER` with the Communication module.

## Class N — a word COBOL 85 reserved, used as a name

A reserved word is never a user-defined word (X3.23-1985), and the
compiler refuses one wherever a program names a data item, index, file,
paragraph or section (cobol ISSUES-43; the list is GnuCOBOL's
`-std=cobol85`, 348 words). The exception is a word the 1985 text newly
reserved that 74-era programs really use as a name. They are accepted,
and `-warn-74` names them so the program can be updated.

| id | construct | accepted words | why |
|---|---|---|---|
| BP-N1 | a newly reserved word naming a data item (or other user-defined word) | `CLASS`, `OTHER`, `TRUE`, `FALSE`, `ANY` | the Open Systems suite's payroll programs name items `CLASS` (PAACEMP, PACHKTBL, PAMANCHK, PAPRECHK) and `OTHER` (PAACEMP); `TRUE`, `FALSE`, `ANY` were taken with `OTHER` in 91e6807f |

The set is data-driven, not the full list of words 85 added: every
corpus was surveyed (majesty, the Open Systems suite, CCVS-85, the
tests) and these are the only reserved words any of them uses as a
name. A word joins the set when a real program needs it, as a dialect
does.

## Class E — extensions already taken (registered, not yet enforced)

Implementor and dialect features accepted under `-std=85`. They are
recorded so the day a stricter switch arrives (`-std=2002`, or a
pedantic 85), deciding each one is a row here, not an archaeology dig.
They do not call `bp()` yet.

| construct | source | in a later standard |
|---|---|---|
| free-format source | GnuCOBOL / majesty | COBOL 2002 |
| `SCREEN SECTION` | Micro Focus | COBOL 2002 |
| positioned `DISPLAY` / `ACCEPT` (`LINE`, `POSITION`, `AT rrcc`) | RM/COBOL | no |
| the device word in `ASSIGN` (`RANDOM`, `PRINT`, `DISK`) | RM/COBOL | no |
| `STOP RUN identifier` / `RETURNING n` | RM/COBOL, GnuCOBOL | `RETURNING` form in 2002 |
| `COMP-1` as a binary integer with a PICTURE | RM/COBOL | no (IBM means hex float) |
| `USAGE POINTER` | GnuCOBOL / majesty | COBOL 2002 |
| `COMP-5`, `BINARY-CHAR` and kin | GnuCOBOL / majesty | partly, 2002 |
| `CALL ... BY VALUE ... RETURNING` to C | C-ABI implementor module | `BY VALUE` / `RETURNING` in 2002 |

The "later standard" column comes from general knowledge of the 2002
text and has not been checked line by line; standards.md schedules that
survey before Stage B.

## Adding a point

1. Pick the next id in its class and add it to the enum and `g_bp[]`
   table in `src/s32-cobc.c`, with a message that says what changed and
   what to do instead.
2. Call `bp()` at the site. Put the call before any early return, so a
   path that returns first (BP-M2 sits above the ODO-source path in
   `emit_move` for that reason) still reports.
3. Add a row here.
4. Extend `tests/warn/every-point.cbl` and its `.expected`. Gate 4 of
   `tests/run-tests.sh` requires the exact set of ids under `-warn-74`
   and no stderr at all without it; `tests/warn/clean-85.cbl` holds the
   near-misses that must stay silent.

When this landed, Gate 4 was mutation-tested both ways: a compiler with
BP-M2's call removed fails on the id set, and one that warns without the
flag fails the silence check.

## Not yet checked

- CCVS-85 is the heaviest user of class O constructs. Its corpus is not
  on the Lenovo, so "CCVS output unchanged by the registry" rests on the
  byte-identical suite, not on a CCVS run. Run `tests/ccvs-run.sh`
  where the corpus lives.

## Audit: the Open Systems suite (2026-09-27)

All 229 sources of the 1978-83 RM/COBOL suite (`~/open`), compiled
under `-warn-74`. 217 compile; the other 12 fail identically without
the registry (eleven need `TG*SORT` copybooks that live outside their
module's directory, and `pa/SPACHKR` is a copybook fragment, not a
program).

| id | occurrences | programs |
|---|---|---|
| BP-M1 | 0 | 0 |
| BP-M2 | 0 | 0 |
| BP-O1 `ALTER` | 253 | 11 |
| BP-O2 comment-entries | 423 | 217 |
| BP-O6 `LABEL RECORDS` | 399 | 214 |
| the rest | 0 | 0 |

**No program meets a class M point**, so nothing in the suite computes
differently under the 85 rules it is compiled with. What it carries
instead is class O, and in the proportions to expect of 74-era code:
comment-entries and `LABEL RECORDS` nearly everywhere, and `ALTER`
concentrated in 11 programs. BP-M1 is still matched by symbol only, so
a bound reached through `REDEFINES` would not have been seen.
