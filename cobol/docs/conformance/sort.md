# SORT, MERGE, RELEASE, RETURN: 14.9.40, 14.9.24, 14.9.32, 14.9.34

Swept 2026-09-29 (ISSUES-112). X3.23-1985: the Sort-Merge module's SORT,
MERGE, RELEASE and RETURN. 2023: 14.9.40 (formats 1, file, and 2,
table), 14.9.24, 14.9.32, 14.9.34. The file form's general rules are
exercised by CCVS-85's ST programs and free/sort*; this sweep went after
the syntax rules and implemented the table form.

## SORT and MERGE of files

| rule | paraphrase | disposition |
|---|---|---|
| SORT 1, MERGE 1 (2023 SORT 3, MERGE 1) | not inside an input or output procedure, nor a declarative | **refused**: bad/sort-rules -- accepted before this sweep. Checked when the procedure division is done, over the procedures' paragraph ranges (THRU a section included) |
| SORT 2, MERGE 2 | file-name-1 is an SD | **refused** (the message cited nothing and spoke of the table form; now the rule) |
| SORT 4, MERGE 4 (2023 6, 4) | keys in the SD's records, not in a table; not boolean or pointer | table: **refused** with the rule (it asked for a subscript before); boolean, pointer: **refused**, accepted before |
| SORT 3, MERGE 3 (2023 5, 3) | USING records no longer than the SD's | **refused**: bad/sort-rules -- accepted before; our own free/sortfile had a 30-byte input and a 29-byte SD, now 30 |
| SORT 10, MERGE 11 (2023 11, 12) | the SD's record no longer than a GIVING file's | **refused** for a fixed-length GIVING file; a LINE SEQUENTIAL or variable one is taken |
| SORT 8, MERGE 10 (2023 9, 10) | an indexed GIVING file: the first key ascending, in its RECORD KEY's place | **refused** -- accepted before |
| MERGE 7 | no file named twice | **refused**: bad/sort-rules -- accepted before |
| 2023 SORT 12, MERGE 13 | a relative or indexed USING file in sequential or dynamic access | **refused** -- accepted before |
| SORT 6, MERGE 5 | USING and GIVING files are not SDs | **refused** |
| RELEASE 1, RETURN 1 | an SD's record; an SD | **refused** |
| RETURN 2 (2023) | INTO with several records: all alphanumeric | holds (one record here) |

## SORT of a table (COBOL 2002)

New in this sweep; it was refused ("the file must be described by an
SD"). `SORT table [ON {ASCENDING|DESCENDING} KEY k ...]... [WITH
DUPLICATES [IN ORDER]] [COLLATING SEQUENCE alphabet]`: the occurrences
(the OCCURS DEPENDING ON count, or all of them) put in order in place.
Each gets the file sort's normalized key with its occurrence number
trailing; the keys are merge-sorted with memcmp and the occurrences moved
once (cob_sort_table).

| rule | paraphrase | disposition |
|---|---|---|
| 13 | the table is an entry with OCCURS | **refused** otherwise: bad/std2002-sort-table. A table inside another table: **not implemented** |
| 14a-e | keys the entry or inside it, unsubscripted, not in a nested table, not boolean or pointer | **refused**: bad/std2002-sort-table |
| 15 | no KEY phrase only if the OCCURS clause has KEYs; then those | **test**: 2002/sorttable (own key); **refused** otherwise |
| GR 3c | WITH DUPLICATES: equal keys keep their order | **test**: 2002/sortdups -- no oracle: GnuCOBOL's table SORT reorders equal keys, with the phrase or without |
| GR 4 | without DUPLICATES the order of equal keys is undefined | stable anyway |
| the rest | ascending, descending, several keys, signed numeric, ODO count | **test**: 2002/sorttable, the oracle agrees |

CCVS-85, the Open Systems suite and majesty trip none of the refusals.
