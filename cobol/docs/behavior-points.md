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
| `-warn-extensions` | warns at every class E point that calls `bp()` (an extension to the standard the program is compiled for); never changes the output |

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
| BP-M2 | a receiving group holding an `OCCURS DEPENDING ON` table whose DEPENDING ON item is inside it | the group's maximum length (X3.23-1985 XVII-54, change 8) | its current length | the whole group (no subscript, no reference modification) receives a MOVE, or anything lowered as one (`READ ... INTO`) |

BP-M1 is detected by the same symbol only. An outer item reached
through `REDEFINES` or `RENAMES` would change the bounds just the same
and is not flagged yet.

## Class O — obsolete in COBOL 85, deleted by the next revision

Accepted under `-std=85`, as the 1985 text requires: a conforming
implementation must support its obsolete elements. They are what a
74-era program most often carries, so `-warn-74` names them to
encourage the update.

Checked 2026-09-28 against the texts. Each row is an item of the 1985
text's Obsolete Language Element List (FIPS PUB 21-2, XVII-81 ff; the
item number is in the construct column). That list says obsolete
elements "will be deleted from the next revision", and ISO/IEC
1989:2002 did so: its Annex F.1 item 1 (page 810) names all eighteen
items as removed. One part survived a revision longer. 2002 removed
the Debug module but kept debugging lines and the `DEBUGGING MODE`
phrase as obsolete (its G.2 item 2), and ISO/IEC 1989:2014 removed
those (Annex E.2 item 19, page 891), so BP-O12 says 2014. None of the
eighteen appears in ISO/IEC 1989:2023.

| id | construct | effect here |
|---|---|---|
| BP-O1 | `ALTER` (item 10), and the bare `GO TO` it rewrites (item 14) | implemented |
| BP-O2 | comment-entries: `AUTHOR.`, `INSTALLATION.`, `DATE-WRITTEN.`, `DATE-COMPILED.`, `SECURITY.` (item 3), and `REMARKS.` (a 74-era paragraph taken with them) | comments |
| BP-O3 | `STOP literal` (item 16) | the literal is displayed and the run goes on |
| BP-O4 | `OPEN ... REVERSED` (item 15) | implemented |
| BP-O5 | `MEMORY SIZE` (item 4) | no effect |
| BP-O6 | `LABEL RECORDS` (item 7) | no effect |
| BP-O7 | `VALUE OF` (item 8) | no effect |
| BP-O8 | `DATA RECORDS` (item 9) | no effect |
| BP-O9 | `MOVE ALL` a literal of more than one character to a numeric or numeric-edited item (item 2) | implemented: the literal repeated to the item's character positions, then moved as an unsigned integer (IV-11; the text's example on XVII-82, `ALL "123"` to `99V99` giving 31.00) |
| BP-O10 | `RERUN` (item 5) | no effect |
| BP-O11 | `MULTIPLE FILE TAPE` (item 6) | no effect |
| BP-O12 | debugging lines, `D` in column 7, and `WITH DEBUGGING MODE` (the Debug module, item 18) | comments without the clause; compiled with it (VI-10, rules 4-5) |

Already refused rather than accepted, so not points: section segment
numbers (Segmentation, item 17), `ENTER` (item 13) with the
Communication module (items 11 and 12), and `USE FOR DEBUGGING` (the
rest of the Debug module, item 18), refused with a message naming it.
Item 1, double character substitution, does not arise on an ASCII
machine. That accounts for all eighteen items of the list.

## Class N — a word COBOL 85 reserved, used as a name

A reserved word is never a user-defined word (X3.23-1985), and the
compiler refuses one wherever a program names a data item, index, file,
paragraph or section, or a SPECIAL-NAMES class, alphabet, symbolic
character or mnemonic (cobol ISSUES-43; the list is GnuCOBOL's
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

## Class E — extensions to the standard a program is compiled for

Implementor and dialect features this compiler accepts. Each calls
`bp()` and warns under `-warn-extensions` (2026-09-29), for the edition
where it is an extension: a point marked 85 is standard COBOL 2002 and
warns only under -std=85. Never changes the output.

| id | construct | source | an extension under | in ISO/IEC 1989:2023 |
|---|---|---|---|---|
| BP-E1 | `RETURN-CODE` | IBM, Micro Focus, GnuCOBOL | 85, 2002 | no: a special register of those dialects; the standard returns a value through `PROCEDURE DIVISION RETURNING` |
| BP-E2 | `GOBACK` | IBM / majesty (docs/dialect.md) | 85 | yes, from 2002 (14.8.17; 2023 14.9.18) |
| BP-E3 | `COMP-3`, `COMP-5`, `COMP-1` (RM/COBOL's binary integer with a PICTURE), `COMP-4` (BINARY), `COMP-X` (MF's unsigned big-endian binary, PIC 9(n) or X(n)), `COMP-6` (unsigned packed decimal, no sign nibble; signed, COMP-3) | IBM, GnuCOBOL / majesty, RM/COBOL, Micro Focus | 85, 2002 | no; the standard's are `PACKED-DECIMAL` and `BINARY` (and the floating types, not `COMP-1`) |
| BP-E4 | `SIGNED-INT`, `UNSIGNED-INT`, `SIGNED-SHORT`, `UNSIGNED-SHORT` | GnuCOBOL / majesty | 85, 2002 | no; `BINARY-LONG`, `BINARY-SHORT` [SIGNED / UNSIGNED] |
| BP-E5 | `BINARY-CHAR`, `BINARY-SHORT`, `BINARY-LONG`, `POINTER` | GnuCOBOL / majesty | 85 | yes, from 2002 |
| BP-E6 | `STOP RUN identifier` / `RETURNING n` | RM/COBOL, GnuCOBOL | 85, 2002 | no: the standard form is `STOP RUN WITH {ERROR / NORMAL} STATUS [identifier / literal]` (14.9.42) |
| BP-E7 | positioned `DISPLAY` / `ACCEPT` (`LINE`, `POSITION`, `AT`) | RM/COBOL, Micro Focus | 85, 2002 | no: RM's `LINE ... POSITION` form does not appear (`AT rrcc` not checked) |
| BP-E8 | hexadecimal literals `X"..."` | the Stage 1 extension majesty uses | 85 | yes, from 2002 |
| BP-E9 | `CALL ... BY VALUE`, `CALL ... RETURNING` (the seam to C) | C-ABI implementor module | 85 | yes, from 2002 |
| BP-E10 | `SCREEN SECTION` | Micro Focus | 85 | yes, from 2002 |
| BP-E11 | free-form source | GnuCOBOL / majesty | 85 | yes, free-form reference format (2002 6.3) |
| BP-E12 | `ORGANIZATION LINE SEQUENTIAL` | Micro Focus, GnuCOBOL | 85, 2002 | yes, but only from 2023 |
| BP-E13 | `_` in a user-defined word | GnuCOBOL / majesty | 85, 2002 | no: letters, digits and hyphens |
| BP-E14 | an ADD, SUBTRACT, MULTIPLY or DIVIDE whose composite of operands is 19-31 digits | majesty's dist01 (one SUBTRACT, 15 integer digits and 4 decimals) | 85 | 2002 allows 31 (14.7.7 rule 2); 1985 says 18. Taken, since the values fit the 18-digit arithmetic; past 31 refused in both editions |
| BP-E15 | INITIALIZE of an item that is or contains an OCCURS DEPENDING ON table | majesty's gl008, gl034, gl040 (and tests/free/odo, their shape) | 85 | X3.23-1985 INITIALIZE syntax rule 4 forbids it; 2002 allows it (2023 14.9.20.4 GR 8). Taken, the table unrolled to its maximum as before |
| BP-E16 | a RECORD KEY or ALTERNATE RECORD KEY that is not alphanumeric or national | the Open Systems suite (13 files) | 85, 2002 | both editions' key rule 2 asks for alphanumeric; taken, the key ordered by its bytes, as GnuCOBOL does |
| BP-E17 | a FILE STATUS item that is not alphanumeric (PIC 99) | none in the corpora | 85, 2002 | FILE STATUS rule 2 asks for two alphanumeric characters; a two-digit numeric one is taken |
| BP-E18 | READ without AT END, or a keyed statement without INVALID KEY, and no USE procedure for the file | the Open Systems suite (8) | 85 | X3.23-1985 requires the phrase then (READ rule 2 and its keyed siblings); the condition goes to the FILE STATUS, or stops the run |
| BP-E19 | RESERVE, BLOCK CONTAINS or RECORD CONTAINS on a LINE SEQUENTIAL file | majesty's jerm (RECORD CONTAINS) | 2002 | 2023 excludes them (12.4.5.2 rule 12, 13.4.5.3 rule 4); taken, with no effect on the lines |
| BP-E20 | a literal of more than 160 character positions | the NIST SQL suite (yts750, 255 positions) | 85, 2002 | 1985 and 2002 allow 1 through 160; 2014 and 2023 allow 8,191. Taken up to 8,191; past that refused |
| BP-E21 | EXIT PROGRAM followed by more statements in its sentence | the NIST SQL suite (dml116s: EXIT PROGRAM, STOP RUN) | 85 | X3.23-1985 EXIT PROGRAM syntax rule 1 makes it the last; 2002 dropped the rule. Taken, run as 2002 runs it |
| BP-E22 | a separator comma or semicolon with no space after it (`"...",SQL-COD`) | the NIST SQL suite (yts775) | 85, 2002 | the standard's separator comma is followed by a space; taken as a separator. A comma between digits is still the decimal point under DECIMAL-POINT IS COMMA |
| BP-E23 | a condition-name on a group holding items of a usage other than DISPLAY, or JUSTIFIED or SYNCHRONIZED ones | majesty (`88 ... VALUE HIGH-VALUES` end-of-file flags over packed fields: crgltrans, the sgltrans copybook) | 85, 2002 | X3.23-1985 VI-21 general rule 2c and 2023 13.16.3 rule 24c and d keep it off; taken, the group compared as its bytes (ISSUES-118) |

Not points, recorded here:

| construct | note |
|---|---|
| the device word in `ASSIGN` (`RANDOM`, `PRINT`, `DISK`) | not an extension: `ASSIGN TO device-name` is standard syntax in 1985 and 2023 (12.4.5), and device-names are implementor-defined; RM's names are this implementation's choice |
| a BY REFERENCE function argument described differently from its parameter but stored identically (`PIC S9(8) COMP-5` and `SIGNED-INT`) | GnuCOBOL / majesty's `holidays`; 14.8.2.3 requires the same USAGE and PICTURE; taken for two's-complement binary integers of equal size and signedness only (docs/functions.md) |

Checked 2026-09-28 against ISO/IEC 1989:2023 (a licensed copy, held
outside the tree; clause numbers are 2023's). The column used to say
"COBOL 2002" from general knowledge. Re-checked against ISO/IEC
1989:2002 the same day, once it was held: every answer is the same for
2002 (free-form reference format 6.3, `STOP RUN WITH ... STATUS`
14.8.38, `BINARY-CHAR`, `POINTER`, `BY VALUE`, the floating types, no
`COMP-5` or `COMP-1`), except `LINE SEQUENTIAL`, which 2002 does not
have and 2023 does.

**What 2023 marks archaic or obsolete** (its Annex F), for the day a
2023 switch needs points of its own: archaic, `EXIT PROGRAM` and `NEXT
SENTENCE`; obsolete, `MOVE ALL "digits"` to an integer item, and the
fixed-form continuation indicator (a hyphen in column 7), on which every
fixed-format program in the preserved corpora relies. 2023 also removed
`CLOSE ... WITH LOCK` and its status 38, both implemented here, and
with them (Annex E.2 item 1) the continuation of a word across fixed-form
lines, `CALL ... ON OVERFLOW`, and a figurative constant moved to a
numeric or numeric-edited item -- except `ALL` with a literal of digits
to an integer item, which 2023 keeps as obsolete (BP-O9's case, now only for
integers). docs/standards.md, "Later revisions", has the survey.

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

Re-run 2026-09-28 when BP-O9 to BP-O12 were added (ISSUES-47): none of
the four occurs anywhere in the suite -- no `MOVE ALL` to a numeric
item, no `RERUN` or `MULTIPLE FILE TAPE`, no debugging line. By then
227 of the 228 programs compiled (only `in/CPINVBIL`, whose `sexinv`
copybook is not in the tree, did not), so the older counts above have
grown with them: BP-O2 443, BP-O6 412, BP-N1 5.

Under -std=2002 the class O points BP-O1 to BP-O11, except BP-O9, are
errors, not warnings: COBOL 2002 deleted those elements (ISO/IEC
1989:2002 F.1), so a 2002 program cannot use them (cobol ISSUES-115).
BP-O9 stays a warning because 2023 permits an ALL literal of digits to
an integer item again.

