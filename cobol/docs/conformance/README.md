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

[coverage.md](coverage.md) is the matrix across the whole standard: every
element of ISO/IEC 1989:2023 with syntax or general rules, swept or not,
and for intrinsic functions and unswept statements, the compiler's own
answer as to whether they are implemented.  It is generated, by section
number (and the COBOL keyword, where the element is one), never the
standard's titles or text:

    python3 gen-coverage.py > coverage.md     # needs mutool and the PDF

| section | page | swept |
|---|---|---|
| 14.9.1 ACCEPT, 14.9.11 DISPLAY, 14.9.17 GO TO, ALTER; 2002 F.1 | [accept.md](accept.md) | 2026-09-29 |
| 14.9.13 EVALUATE, 14.9.19 IF | [evaluate.md](evaluate.md) | 2026-09-29 |
| 14.9.14 EXIT | [exit.md](exit.md) | 2026-09-28 |
| 13.18.40 PICTURE, 13.18.8 BLANK WHEN ZERO | [picture.md](picture.md) | 2026-09-29 |
| 13.18.60 USAGE (the rest); 8.5.2.15, 8.4.3.13 program-pointers, 14.9.39 format 9 (2026-10-06) | [usage.md](usage.md) | 2026-09-29 |
| 14.2, 14.9.4 CALL parameters, 14.8.2, 14.8.3; 11.5 FUNCTION-ID, 11.10 PROGRAM-ID, 12.3.8 REPOSITORY (prototypes, AS literal, 2026-10-06) | [call.md](call.md) | 2026-09-29 |
| 14.7.7, ADD SUBTRACT MULTIPLY DIVIDE COMPUTE | [arithmetic.md](arithmetic.md) | 2026-09-29 |
| 8.8.1 arithmetic expressions, native arithmetic | [expressions.md](expressions.md) | 2026-09-30 |
| 8.8.4.2 relation conditions, abbreviated relations; 8.7.5, 8.8.4.3-8.8.4.11 the other conditions (2026-10-07) | [conditions.md](conditions.md) | 2026-09-30 |
| 8.3.3.3 numeric literals, fixed-point and floating-point; the token scanner (lex.rl); 8.1.3 and Annex B, extended letters in user-defined words | [lexical.md](lexical.md) | 2026-10-07 |
| 11.9, 11.9.5, 11.9.6, 11.9.7, 11.9.8, 11.9.9, 11.9.10, 11.9.11 the OPTIONS paragraph | [options.md](options.md) | 2026-10-06 |
| 8.4.3.3 reference-modification | [refmod.md](refmod.md) | 2026-10-06 |
| 10.6, 10.7 the compilation group and end markers; 12.3.5, 12.3.6, 12.4.4, 12.4.6 SOURCE-/OBJECT-COMPUTER, I-O-CONTROL; 13.4.6 SD, 13.18.5 BASED, 13.18.57-58 TYPE/TYPEDEF; 14.7.4 ROUNDED, 14.9.3 ALLOCATE, 14.9.15 FREE, 14.9.47 UNLOCK | [environment.md](environment.md) | 2026-10-07 |
| 8.3.3.2, 8.3.3.6 literals and figurative constants; 8.4.2.2 qualification, 8.4.2.3 subscripts, 8.4.3.1-2, .10-.12, .14-.15 identifiers, 8.4.4 condition-name | [identifiers.md](identifiers.md) | 2026-10-07 |
| 12.4.5, 13.4.5, RECORD, LINAGE: files | [files.md](files.md) | 2026-09-29 |
| 14.9.6/.10/.27/.30/.35/.41/.51 the I-O statements | [io-statements.md](io-statements.md) | 2026-09-29 |
| 9.1.15, 9.1.16, 12.4.5.9, 12.4.5.15 file sharing and record locking; 14.7.9 RETRY; the LOCK phrases of OPEN, READ, WRITE, REWRITE | [locking.md](locking.md) | 2026-10-07 |
| 14.9.20 INITIALIZE | [initialize.md](initialize.md) | 2026-09-29 |
| 14.9.25 MOVE | [move.md](move.md) | 2026-09-29 |
| 14.9.37 SEARCH | [search.md](search.md) | 2026-09-29 |
| Report Writer: RD, report groups, GENERATE, INITIATE, TERMINATE, SUPPRESS (X3.23-1985 XIII) | [reportwriter.md](reportwriter.md) | 2026-09-30 |
| 14.9.42 STOP, 14.9.18 GOBACK, 14.9.9 CONTINUE, 14.9.5 CANCEL | [control.md](control.md) | 2026-09-30 |
| 15 intrinsic functions | [functions.md](functions.md) | 2026-09-30 |
| 7.2 COPY and REPLACE (text manipulation) | [copy.md](copy.md) | 2026-09-30 |
| 14.9.39 SET (formats 1-4) | [set.md](set.md) | 2026-09-30 |
| 14.9.40, .24, .32, .34 SORT, MERGE, RELEASE, RETURN | [sort.md](sort.md) | 2026-09-29 |
| 13.18.32, .33, .52, .55 JUSTIFIED, level-number, SIGN, SYNCHRONIZED | [clauses.md](clauses.md) | 2026-09-29 |
| 13.18.38 OCCURS | [occurs.md](occurs.md) | 2026-09-29 |
| 13.2, 13.5, 13.6, 13.7, 13.10, 13.11, 13.13, 13.16, 13.18.13, .20, .22, .27 the sections, the data description entry, CODE-SET, FILLER, EXTERNAL, GLOBAL; 13.18.49 SAME AS, 13.18.1 ALIGNED (2026-10-06) | [data-division.md](data-division.md) | 2026-09-30 |
| 13.18.44 REDEFINES | [redefines.md](redefines.md) | 2026-09-29 |
| 13.18.45 RENAMES | [renames.md](renames.md) | 2026-09-29 |
| 13.18.63 VALUE | [value.md](value.md) | 2026-09-29 |
| 14.9.22 INSPECT, 14.9.43 STRING, 14.9.48 UNSTRING | [string.md](string.md) | 2026-09-29 |
| 13.18.29 GROUP-USAGE, 13.18.60 USAGE BIT/NATIONAL, 13.18.40 PICTURE 1/N, 8.3.3.4-5 | [national-boolean.md](national-boolean.md) | 2026-09-29 |
| 14.9.28 PERFORM | [perform.md](perform.md) | 2026-09-28 |
| 13.17, 13.18.3, .4, .6, .7, .9, .14, .21, .23, .25, .26, .30, .35, .36, .47, .48, .50, .56, .59, .61 the screen section and its clauses; 14.9.1, 14.9.11 of a screen | [screen.md](screen.md) | 2026-10-06 |
| 14.9.29 RAISE | [raise.md](raise.md) | 2026-09-28 |
| 14.9.33 RESUME | [resume.md](resume.md) | 2026-10-07 |
| 7.3.25 TURN | [turn.md](turn.md) | 2026-09-28 |
| 14.6.13.1.6 Table 13, every exception-name: raised where, or why not | [exceptions.md](exceptions.md) | 2026-10-06 |
| 7.3.5, 7.3.6, 7.3.7, 7.3.8, 7.3.9, 7.3.11, 7.3.13, 7.3.16, 7.3.17, 7.3.18, 7.3.19, 7.3.21 conditional compilation: DEFINE, EVALUATE, IF; CALL-CONVENTION, LEAP-SECOND, LISTING, PAGE; PROPAGATE; 7.3.23 REF-MOD-ZERO-LENGTH; 7.3.10, 7.3.12, 7.3.15, 7.3.20, 7.3.22 COBOL-WORDS, DISPLAY, FLAG-14, POP, PUSH | [directives.md](directives.md) | 2026-10-07 |
| 14.9.49 USE | [use.md](use.md) | 2026-09-28 |
| 2014 Annex E.2 items 1-29, E.3 items 7, 18, 19: the edition's substantive changes, under -std=2002 and -std=2014 | [edition-2014.md](edition-2014.md) | 2026-10-07 |
| 2023 Annex E.2 items 1 and 21 (the removals, class R points), 11.9.10 OPTIONS INITIALIZE, 13.18.55 SYNCHRONIZED on a group, the 2023 statements, functions and directives (pointers); `-std=2023` | [edition-2023.md](edition-2023.md) | 2026-10-07 |
| 2023 Annex E.2 items 2-30: the 2014-to-2023 behaviour changes, one row each under -std=2014 and -std=2023 | [edition-2023-audit.md](edition-2023-audit.md) | 2026-10-07 |
