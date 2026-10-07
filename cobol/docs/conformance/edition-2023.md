# The 2023 edition: `-std=2023`, its removals (2023 Annex E.2 items 1 and 21), 11.9.10 OPTIONS INITIALIZE, 13.18.55 SYNCHRONIZED on a group

Started 2026-10-07 (docs/plans/standard-queue.md item 30). `-std=2023`
takes everything `-std=2014` does (docs/standards.md) plus the 2023
additions as they land, and refuses what 2023 removed. The rest of
2023's Annex E.2 (items 2-30) is queue item 36; the 2023 statements,
functions and directives are items 31-34.

## E.2 item 1 and item 21: the removals

Each is the language through 2014 and taken under `-std=85`, `-std=2002`
and `-std=2014` as a class R behaviour point (docs/behavior-points.md:
silent, no flag warns of a standard form); under `-std=2023` the point
is refused, the message naming `-std=2014`. Tests 2014/removed2023 (no
oracle: GnuCOBOL 4 has no EXIT FUNCTION), fixed/wordcont (GnuCOBOL
agrees); one bad test per point.

| removed | here through 2014 | under -std=2023 |
|---|---|---|
| a figurative constant (SPACE, QUOTE, HIGH-VALUE, LOW-VALUE, ALL literal) moved to a numeric or numeric-edited item; ALL digits to an integer item kept, obsolete (item 1; QUOTE was item 21's) | **refused** under every edition already, by ruling (move.md rule 5); ALL digits to an integer is BP-O9 | the same |
| the continuation of a COBOL word across fixed-form lines (item 1) | BP-R1: the halves joined (reader.h); a literal continued is still the language | **refused**: bad/std2023-word-continuation |
| CALL ... ON OVERFLOW (item 1) | BP-R2: as ON EXCEPTION | **refused**: bad/std2023-call-overflow |
| COPY REPLACING operands that are not pseudo-text (item 1) | BP-R4: a word, an identifier or a literal taken as pseudo-text of one text-word (copy.md) | **refused**: bad/std2023-copy-word |
| EXIT METHOD, EXIT FUNCTION (item 1) | BP-R5: EXIT FUNCTION a function's GOBACK, implemented with the point (2002 14.8.14 format 2 had it; it met a parse error before); EXIT METHOD is object orientation's | **refused**: bad/std2023-exit-function |
| CLOSE ... WITH LOCK and I-O status 38 (item 1) | BP-R3: taken (io-statements.md) | **refused**: bad/std2023-close-lock |
| FLAG-85, FLAG-NATIVE-ARITHMETIC (item 21) | not implemented under any edition (directives.md) | the same |
| ARITHMETIC IS STANDARD (item 21) | **refused** naming NATIVE (options.md) | the same |

## The small 2023 statements (queue item 31, 2026-10-07)

Each under `-std=2023`, refused naming the edition under 2002 and 2014
(bad/std2002-* and bad/std2014-*); the rows are on the statements'
pages. Tests 2023/stmts2023 (no oracle: GnuCOBOL 4 has none of these
but a DELETE FILE spelt its own way), 2023/inspback (no oracle).

| statement | where |
|---|---|
| XOR / EXCLUSIVE-OR (8.7.6, 8.8.4.9, 8.8.4.13) | conditions.md: desugared to AND, OR and NOT |
| INSPECT BACKWARD (14.9.22.4 rule 3) | string.md: the scan from the right |
| DELETE FILE [OVERRIDE] (14.9.10 format 2) | io-statements.md: `cob_delete_file` |
| WRITE with both BEFORE and AFTER ADVANCING (14.9.51) | io-statements.md; a LINAGE-file defect found with it |
| CONTINUE AFTER expression SECONDS (14.9.9), EC-CONTINUE-LESS-THAN-ZERO | control.md, exceptions.md |
| GOBACK WITH {ERROR / NORMAL} STATUS (14.9.18 GR 3) | control.md: STOP RUN's status phrase when no caller controls the program |
| USAGE PACKED-DECIMAL WITH NO SIGN (13.18.60 GR 25) | usage.md: the standard's COMP-6 |

## The 2023 functions (queue item 33, 2026-10-07)

BASECONVERT, CONCAT, CONVERT, FIND-STRING, MODULE-NAME,
SMALLEST-ALGEBRAIC, SUBSTITUTE and EXCEPTION-FILE(-N)'s file-name
argument: functions.md "The 2023 functions" has the rows; tests
2023/fn2023 and 2023/excfile (no oracle).

## 11.9.10 OPTIONS INITIALIZE

The clause names the fill byte every item without a VALUE in the named
sections starts with: options.md has the rows (implemented 2026-10-07,
2023/optinit).

## 13.18.55 SYNCHRONIZED on a group item

2023 lets the clause stand on a group (SR 1), as if written on each
elementary item below it (GR 1): clauses.md has the row (implemented
2026-10-07; under the earlier editions a group's SYNCHRONIZED stays
refused, as their rule 1 has it).
