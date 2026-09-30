# The file control entry and the FD: 12.4.5, 13.4.5, 13.18.10/34/43

Swept 2026-09-29 (ISSUES-110). X3.23-1985: the Sequential, Relative and
Indexed I-O modules' file control entries (ACCESS MODE, ALTERNATE
RECORD KEY, FILE STATUS, RECORD KEY, RELATIVE KEY) and file description
entries (3.2 FD, 3.3 BLOCK CONTAINS, 3.5 DATA RECORDS, 3.7 LINAGE, 3.8
RECORD). 2023: 12.4.5 and its clauses, 13.4.5, 13.4.6, 13.18.10,
13.18.34, 13.18.43.

## The file control entry

| rule | paraphrase | disposition |
|---|---|---|
| 2023 2-3 | one SELECT per file, and an FD or SD for it | **refused** ("SELECTed twice", "has no FD") |
| 2023 12.4.5.5.2 rule 2 (85: the sequential format) | no ACCESS RANDOM or DYNAMIC for a sequential file | **refused**: bad/select-access -- accepted before this sweep |
| 2023 8 (85: the indexed format) | RECORD KEY only for an indexed file | **refused**: bad/select-record-key -- accepted before |
| 2023 10 | RELATIVE KEY with random or dynamic access | **refused** |
| RECORD KEY 2 (85: 2) | the key alphanumeric (or national) in the file's record, no OCCURS | not in a table, in the record: **refused**. A numeric key: **extension** BP-E16, taken and ordered by its bytes -- the Open Systems suite has 13 |
| ALTERNATE RECORD KEY 4 (85: 4) | no alternate key beginning where the prime key or another alternate does | **refused**: bad/select-alternate-key -- accepted before |
| FILE STATUS 1-2 (85: 2) | two alphanumeric characters, not in a table, not in the FILE SECTION | table and FILE SECTION: **refused**, bad/select-file-status -- accepted before; a numeric PIC 99: **extension** BP-E17 |
| RELATIVE KEY 1-3 | an unsigned integer without P, outside the file's record | **refused** |
| 2023 12 | no RESERVE for LINE SEQUENTIAL | **extension** under -std=2002, BP-E19 (warn/ext-std2002-lineseq); it was refused by ISSUES-110 and broke majesty's jerm (RECORD CONTAINS), found by tests/majesty-functions.sh in ISSUES-116. Under -std=85 LINE SEQUENTIAL is itself an extension (BP-E12) |

## The file description entry

| rule | paraphrase | disposition |
|---|---|---|
| 85 FD rule 3; 2023 13.4.5.3 rule 3 | record descriptions follow (2023: or a RECORD clause, with READ INTO and WRITE FILE ... FROM) | **refused** under -std=85 with the rule (bad/fd-no-record; it said "file 'f' has no FD"); under -std=2002 **not implemented**, said so |
| one FD per file | | **refused**: bad/fd-twice -- accepted before |
| 2023 13.4.5.3 rule 4 | LINE SEQUENTIAL takes no BLOCK or RECORD CONTAINS | **extension** under -std=2002, BP-E19, as RESERVE above |
| DATA RECORDS 1 (85) | the names are the FD's own 01 records | **refused**: bad/fd-data-records -- accepted before (the clause is obsolete, BP-O8) |
| RECORD 1 (85: 1) | no record longer than RECORD CONTAINS | **refused**; a longer RECORD CONTAINS is taken as the record area, as GnuCOBOL does (majesty's sglentry) |
| RECORD 4 (85: 2) | m TO n: no record shorter than m | **refused**: bad/fd-record-min -- accepted before |
| RECORD 5, 9 (85: 3) | n above m | **refused**: bad/fd-record-range -- a reversed range gave a confused size message before |
| RECORD 6 (85: 4) | DEPENDING ON an elementary unsigned integer in WORKING-STORAGE, LOCAL-STORAGE or LINKAGE | **refused**: bad/fd-depending-in-record (an item of the record itself was accepted); a signed item too. A LINKAGE or LOCAL-STORAGE item is refused as an implementation limit, as before |
| LINAGE 1-2 (85: 1) | the data-names elementary unsigned integers, not in a table | **refused**: bad/fd-linage-signed -- signed and table items were accepted before |
| LINAGE 3 (85: 3) | the footing within the page body | **refused**: bad/fd-linage-footing -- accepted before |
| BLOCK CONTAINS | no syntax rules in either edition | accepted, a hint with no meaning on a byte stream |

CCVS-85, the Open Systems suite and majesty trip none of the refusals;
the Open Systems suite relies on BP-E16.
