# RENAMES: 13.18.45

Swept 2026-09-29 (ISSUES-106). X3.23-1985: 5.11, the RENAMES clause
(VI-39..40). 2023: 13.18.45.

| rule | paraphrase | disposition |
|---|---|---|
| 2023 2 (85: 2) | the RENAMES entries follow the last entry of their record | **refused**: a name from another record is not found under the record the entry follows |
| 2023 3 (85: 3) | data-name-1 qualified only by its record; no OCCURS on data-name-2 or -3, nor a table above them | **refused**: "has OCCURS or lies in a table"; free/renames qualifies by the record |
| 2023 4 (85: 4) | the same record; not the same data-name | **refused**: bad/renames-same (THRU naming data-name-2 again) -- accepted before this sweep |
| 2023 5 (85: 4) | not level 01, 66, 77 or 88 | **refused**: bad/renames-level -- was "'g' is not declared under 'g'" for the record itself and a 77; now the rule |
| 2023 7 | not subscripted | **refused** (a table item is refused by rule 3 already) |
| 2023 8 (85: 6) | nothing in the range a pointer, strongly typed, or an ODO table | **refused**: the endpoints, and now any strongly-typed item inside the range; an ODO table ends its record (13.18.38.3 rule 22), so it can only be an endpoint |
| 2023 10 | whole bytes | **refused** when a bit item starts or ends the range inside a byte -- not checked before |
| 2023 11 (85: 8) | data-name-3 begins no earlier than data-name-2 and ends after it | **refused**: bad/renames-range (data-name-3 inside data-name-2, or ending before it) -- only "ends before data-name-2 begins" was refused before. **test**: free/renames2 (the oracle agrees), free/renames3: data-name-2 inside data-name-3 is legal by the text; no oracle, GnuCOBOL refuses a THRU item declared before data-name-2 |
| GR | an elementary data-name-2 alone is an alias of its description; anything else a group | **test**: free/renames (the oracle agrees) |
| 2023 6 | CONSTANT RECORD | **n/a**: 2014's |

Nothing in CCVS-85, the Open Systems suite or majesty is affected.
