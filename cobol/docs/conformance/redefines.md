# REDEFINES: 13.18.44

Swept 2026-09-29 (ISSUES-105), prompted by GnuCOBOL warning on a test
this compiler had accepted (a REDEFINES of an item with OCCURS).
X3.23-1985: 5.10, the REDEFINES clause (VI-37..38). 2023: 13.18.44.

The redefined item used to be found by searching back for any earlier
entry with that name and level, which let a redefinition stand anywhere
after it. It is now the entry just before this one at its level (or,
when that entry is itself a redefinition, the entry it redefines), with
the checks that need sizes made after layout. Only entries with a
REDEFINES clause are checked: a file's records, and SAME RECORD AREA,
share storage through the same mechanism without one.

| rule | paraphrase | disposition |
|---|---|---|
| 2023 2 (85: 2) | the same level; not 66 or 88 | **refused**, with both levels named (the message named the wrong item before) |
| 2023 3 (85: 3) | not on a level 01 entry in the FILE SECTION | **refused**: bad/redefines-fd -- accepted before this sweep |
| 2023 4, 10 (85: 10, 11) | nothing at a lower level between, no intervening storage | **refused**: bad/redefines-rules -- accepted before |
| 2023 5 (85: 5) | data-name-2 has no OCCURS (it may be inside a table); neither side includes an OCCURS DEPENDING ON table | **refused**: bad/redefines-occurs, and an ODO table in the redefinition -- both accepted before; an ODO in the original was refused already (by 13.18.38.3 rule 22) |
| 2023 6 (85: 7) | data-name-2 not qualified | **refused** ("unexpected 'of'") |
| 2023 7 (85: 8) | several redefinitions all name the original | **refused**: bad/redefines-rules -- accepted before |
| 2023 8 (85: 6) | no larger than data-name-2 unless that is a level 01 item and not EXTERNAL | **refused**: bad/redefines-larger -- accepted before |
| 2023 9 (85: 9) | no VALUE in the entry or below it, but at level 88 | **refused**: bad/redefines-value -- accepted before |
| 2023 11 | data-name-2 may itself be under a redefinition | **accepted** |
| 2023 12, 14 | no pointer (object, message-tag) item either side; no strongly-typed group | **refused**: bad/std2002-redefines-pointer (accepted before), bad/std2002-strong-redefines |
| 2023 13, 16, 17 | CONSTANT RECORD, ANY LENGTH, variable-length groups | **n/a**: 2014's |
| 2023 15 | the same alignment | holds: a redefinition starts where data-name-2 does |
| GR 1 | storage from the first bit of data-name-2 | **test**: the bit sweep's tests (bad/std2002-bit-redef-byte for the refused case) |

None of the new refusals fires in CCVS-85, the Open Systems suite or
majesty; all their programs compile to the same code as before.
