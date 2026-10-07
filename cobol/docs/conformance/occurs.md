# OCCURS: 13.18.38

Swept 2026-09-29 (ISSUES-108). X3.23-1985: 5.8, the OCCURS clause
(VI-25..28). 2023: 13.18.38, formats 1 (fixed) and 2 (DEPENDING ON);
formats 3 (report STEP) and 4 (dynamic capacity) are 2014's.

The KEY names were kept only for SEARCH ALL's binary search and never
resolved; a pass after layout (occurs_rules, recovering per entry) now
resolves and checks them. A qualified key (KEY IS k OF t) used to be
read as three keys.

| rule | paraphrase | disposition |
|---|---|---|
| 2023 1a (85: 1a) | not at level 01, 66, 77, 88 | **refused** |
| 2023 1b (85: 1b) | no OCCURS DEPENDING ON table below a table | **refused**: bad/occurs-rules -- accepted before this sweep |
| 2023 2, 5 (85: 4) | data-names not subscripted | **refused** ("unexpected '('") |
| 2023 3 (85: 3) | the first key is the entry or below it; later keys below it | **refused**: bad/occurs-rules -- a key outside the table, or not declared at all, was accepted before |
| 2023 4 (85: 12) | no OCCURS between a key and the table | **refused**: bad/occurs-rules -- accepted before |
| 2023 6 (85: 11) | a key has no OCCURS unless it is the table's own entry | **refused**: bad/occurs-rules -- accepted before |
| 2023 7 (85: 13, an index-name is not data) | an index-name only as a subscript, in PERFORM and SEARCH VARYING, SET, a relation | **refused**: bad/index-name-operand (ADD, DISPLAY, INITIALIZE) -- accepted before; our own 2002/bitarray2 DISPLAYed one and now SETs an integer from it |
| 2023 8 | no boolean or pointer key | **refused** -- accepted before |
| 2023 10 | at most seven subscripts | **refused** ("too many OCCURS levels") |
| 2023 16 (85: 5) | 0 <= minimum < maximum | **refused**: bad/occurs-rules (3 TO 3) -- accepted before; a count below 1 in format 1 refused already |
| 2023 17 (85: 6) | the DEPENDING ON item an integer | **refused** |
| 2023 20 (85: 7) | the DEPENDING ON item not inside the table or after it in its record | **refused** |
| 2023 19, 23, 33 | no format 2 (DEPENDING), no dynamic-capacity table in a CONSTANT RECORD | **refused**: bad/std2014-constrec-odo (2026-10-07, queue item 25); dynamic tables are not implemented |
| 2023 22 (85: 10) | the table followed in its record only by its subordinates | **refused** (since ISSUES-95) |
| 2023 24 | TO and DEPENDING together | **refused** ("OCCURS m TO n needs DEPENDING ON") |

CCVS-85, the Open Systems suite and majesty trip none of the new rules
(majesty's SEARCH ALL tables, with keys and INDEXED BY, resolve).
