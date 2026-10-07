# ACCEPT, DISPLAY, GO TO, ALTER; the elements COBOL 2002 deleted

Swept 2026-09-29 (ISSUES-115). X3.23-1985: 6.5 ACCEPT, 6.12 DISPLAY,
6.15 GO TO, 6.7 ALTER. 2023: 14.9.1, 14.9.11, 14.9.17; ISO/IEC 1989:2002
F.1 for what 2002 deleted.

## ACCEPT and DISPLAY

| rule | paraphrase | disposition |
|---|---|---|
| ACCEPT 1, DISPLAY 1 (2023) | no index, pointer, strongly-typed group | **refused** (the usage and typedef sweeps) |
| ACCEPT 2, DISPLAY 2 (both) | a device is a mnemonic-name of SPECIAL-NAMES | **refused** with the rule: bad/accept-rules -- an undeclared name said "is not implemented" |
| ACCEPT 3 (2023; 85 GR 6, the MOVE rules) | DATE, DAY, TIME, DAY-OF-WEEK not into an alphabetic or boolean item | **refused**: bad/accept-rules -- accepted before |
| DATE YYYYMMDD, DAY YYYYDDD (2002) | the four-digit year | **new**: 2002/acceptyyyy, the oracle agrees (the clock pinned by COB_CURRENT_DATE) -- was a parse error; under -std=85 "is COBOL 2002" |
| ACCEPT 5, DISPLAY format 2 | LINE and COLUMN unsigned integers | **refused** (the screen work) |
| ACCEPT GR 1 (2023 14.9.1.4) | the data to the receiving operand by its size | **test**: 2002/acceptrm (GnuCOBOL agrees) -- every FROM form passed the whole item's descriptor with a reference-modified receiver until 2026-10-07, so `ACCEPT X(2:2) FROM TIME` wrote four digits (found by standard-queue item 24; `accept_desc`, display.h) |

## GO TO and ALTER

| rule | paraphrase | disposition |
|---|---|---|
| GO TO 1 (both) | DEPENDING ON an integer item | **refused** |
| GO TO 2 (both) | an unconditional GO TO is the last statement of its sequence | **refused**: bad/goto-alter -- a statement after it (never reached) was accepted |
| GO TO 3 (2023) | not in a WHEN of an exception-checking PERFORM | **refused**: bad/std2002-goto-when -- accepted before (only FINALLY was checked) |
| ALTER 1 (85) | the paragraph ALTER names holds one sentence, a GO TO without DEPENDING | **refused**: bad/goto-alter -- any paragraph was accepted |
| ALTER 2 (85) | the new target a paragraph or section | **refused** |
| GO TO without a procedure-name | only in a paragraph ALTER names | **refused** |

## The elements COBOL 2002 deleted

2002 F.1 lists the 1985 obsolete elements it removed: ALTER, comment-
entries (AUTHOR, INSTALLATION, DATE-WRITTEN, DATE-COMPILED, SECURITY),
STOP literal, OPEN REVERSED, MEMORY SIZE, LABEL RECORDS, VALUE OF, DATA
RECORDS, RERUN, MULTIPLE FILE TAPE, ENTER, GO TO without a
procedure-name. They were behavior points BP-O1 to BP-O11, warned about
under -warn-74 and accepted in every edition. Under -std=2002 they are
now refused (bad/std2002-deleted); under -std=85 nothing changes. Not
BP-O9 (an ALL literal of digits to an integer item): 2023 14.9.25.3
rule 5 permits it again, as an obsolete feature. ENTER is refused in
both editions (the Communication module is out).
