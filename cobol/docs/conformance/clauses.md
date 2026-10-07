# JUSTIFIED, SIGN, SYNCHRONIZED, level-numbers

Swept 2026-09-29 (ISSUES-109). X3.23-1985: 5.6 JUSTIFIED, 5.7
level-number, 5.12 SIGN, 5.13 SYNCHRONIZED. 2023: 13.18.32, 13.18.33,
13.18.52, 13.18.55. The checks that need the tree are made after
layout, in the same per-entry pass as OCCURS (occurs.md).

| rule | paraphrase | disposition |
|---|---|---|
| JUSTIFIED 1 (both) | elementary items only | **refused**: bad/clause-rules -- a group was accepted before this sweep |
| JUSTIFIED 3 (85: 3) | not numeric, not edited (2023: alphabetic, alphanumeric, boolean, national) | **refused**: numeric already; an edited item (bad/clause-rules) was accepted before |
| JUSTIFIED 4 (85) | not an index data item | **refused** (usage.md) |
| SIGN 1 (85) | a signed numeric entry, or a group with at least one below it | **refused** under -std=85: a group with none (bad/clause-rules) was accepted before; 2023 allows SIGN on any alphanumeric group |
| SIGN 1-2 | an elementary item signed, usage display (or national) | **refused** ("SIGN applies to a signed numeric DISPLAY item") |
| SIGN 3 | a file with CODE-SET: signed numeric items SIGN SEPARATE | **refused** (bad/codeset-sign, the EBCDIC work) |
| SYNCHRONIZED 1 | elementary items only, in 85, 2002 and 2014; a group is COBOL 2023 (13.18.55.3 rule 1, E.3.2 item 6; 2023 GR 1: as if on each elementary item below it) | **refused** under -std=85 (bad/clause-rules), -std=2002 and -std=2014 (bad/std2002-group-sync, bad/std2014-sync-group); **implemented** under -std=2023 (queue item 30, 2026-10-07): the group's SYNC put on every elementary item under it before the record is laid out (layout.h). **Test**: 2023/optinit (a group of 12 bytes against the same unsynchronized group's 8) |
| level-number | 1-49, 66, 77, 88; 1-9 as 01-09; 77 not in the FILE SECTION | **refused** outside those; accepted: one digit |

CCVS-85, the Open Systems suite and majesty trip none of the new rules.
