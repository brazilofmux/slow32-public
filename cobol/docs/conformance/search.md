# SEARCH: 14.9.37

Swept 2026-09-29 (ISSUES-113). X3.23-1985: the Table Handling module's
SEARCH statement. 2023: 14.9.37. SEARCH ALL has been a binary search
over the table's KEYs since ISSUES-97; its WHEN was taken in any form
and a form the binary search could not use fell back to a scan. The
format allows only one form, and it is now enforced.

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 2023 1-2 (85: 1) | the table named without subscripts or reference modification, with INDEXED BY | **refused** |
| 2023 3 | a table inside another: the outer subscripts from the WHEN | **test**: CCVS-85 NC233A, NC237A, NC238A |
| 2023 4 (85: 5) | NEXT SENTENCE not with END-SEARCH | **refused**: bad/search-rules -- accepted before this sweep |
| 2023 5 (85: 2) | VARYING an index or an integer item | **refused** otherwise |
| 2023 7 (85: 1) | SEARCH ALL: the OCCURS clause has KEY | **refused**: bad/search-rules -- was scanned |
| format 2 | one WHEN; data-name = value, or a condition-name, joined by AND; the key first | **refused**: bad/search-rules -- OR, NOT, other relations, the value first and several WHENs were accepted and scanned |
| 2023 8 (85: 4) | each data-name a KEY of the table, subscripted at the table's level by its first index, without + or - | **refused** -- a non-key, another index and an index + n were accepted and scanned |
| 2023 9 (85: 4) | condition-names of one value, for KEYs | **refused** (one of several values becomes an OR) |
| 2023 10 (85: 4) | the values neither KEYs of the table nor subscripted by its first index | **refused** -- accepted before |
| 2023 11 (85: 4) | the keys used a leading run of the KEY list | **refused** -- accepted and scanned before |
| 2023 13 | no zero-length literals | **refused** (2014's literal) |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 3a | a serial search starts at the index's current value | **test**: free/searchrules (the oracle agrees) |
| 3b1, 3c | VARYING an index, another table's index: stepped with the search index | **test**: free/searchrules |
| 3b2 | VARYING an integer item: incremented by one with each step, from its own value | **test**: free/searchvary -- no oracle: GnuCOBOL sets it from the index |
| 4 | an index outside the table at the start: unsuccessful, AT END; EC-RANGE-SEARCH-INDEX | **test**: free/searchvary (no oracle: GnuCOBOL searches from the first occurrence), 2002/ecsearchidx -- the condition was registered and never raised |
| 4 | the first WHEN that holds | **test**: free/searchrules |
| SEARCH ALL | two keys, a descending key, a condition-name, an ODO table searched as far as its count | **test**: free/searchrules, 2002/searchall |

CCVS-85, the Open Systems suite and majesty are unaffected; the first cut
of the key-subscript rule refused CCVS-85's nested tables and was
corrected before commit.
