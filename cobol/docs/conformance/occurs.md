# OCCURS: 13.18.38

Swept 2026-09-29 (ISSUES-108). X3.23-1985: 5.8, the OCCURS clause
(VI-25..28). 2023: 13.18.38, formats 1 (fixed) and 2 (DEPENDING ON);
format 3 (report STEP) came with the Report Writer's 2002 additions
(queue item 37), format 4 (dynamic capacity, 2014) on 2026-10-08
(queue item 44; its rows are the second table below).

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
| 2023 19, 23, 33 | no format 2 (DEPENDING), no dynamic-capacity table in a CONSTANT RECORD | **refused**: bad/std2014-constrec-odo (2026-10-07, queue item 25), bad/std2014-dyn-constrec |
| 2023 22 (85: 10) | the table followed in its record only by its subordinates | **refused** (since ISSUES-95) |
| 2023 24 | TO and DEPENDING together | **refused** ("OCCURS m TO n needs DEPENDING ON") |

CCVS-85, the Open Systems suite and majesty trip none of the new rules
(majesty's SEARCH ALL tables, with keys and INDEXED BY, resolve).

## Format 4: dynamic-capacity tables (2014; 2023 13.18.38, 8.5.1.9, 14.9.39 format 14)

Implemented 2026-10-08 (queue item 44). The table's entry is an 8-byte
slot in its record -- the elements' address and the current capacity
(libcob `cob_dyn`) -- and the elements live on the heap, one after
another; `cob_dyn_elem` finds or makes an element, so every reference
to one goes through a call, and the HIR islands and the census leave
such items alone. The capacity item is a numeric item laid over the
slot's second word. Items after the table keep their place (8.5.1.11.2).
The implementor's maximum capacity is 16,777,215 occurrences (A.3 item
60). Tests 2014/dyntable, 2014/dyntable2 (no oracle: neither GnuCOBOL 4
nor gcobol has OCCURS DYNAMIC); the bad tests below.

| rule | paraphrase | disposition |
|---|---|---|
| 13.18.38.2 format 4 | OCCURS DYNAMIC [CAPACITY IN name] [FROM n] [TO m] [INITIALIZED], with KEY and INDEXED BY | **implemented** under -std=2014; **refused** under -std=2002 naming the switch (bad/std2002-occurs-dynamic) |
| 8.5.1.9.1 | not in the FILE SECTION | **refused**: bad/std2014-dyn-file |
| 8.5.1.9.1 | nested in any combination | **gap** in this stage: a dynamic table inside a table, or inside a dynamic table, is refused (bad/std2014-dyn-nested, -dyn-in-dyn); a fixed table inside the element is fine (dyntable2's scores) |
| 8.5.1.9.2 | an element as a sending item: as a fixed table of the current capacity | **implemented**: past the capacity, EC-BOUND-SUBSCRIPT (dyntable's last line, fatal); unchecked, a note on stderr and a stand-in element of spaces |
| 8.5.1.9.3 | a receiving item past the capacity: the elements up to it are made | **implemented** for every receiving operand the statements name (MOVE, the arithmetic verbs, STRING INTO, ACCEPT, SET, INITIALIZE ...; dyntable, dyntable2); a CALL BY REFERENCE argument is an address, not a store |
| 8.5.1.9.4; 14.9.39 format 14 | SET capacity TO / UP BY / DOWN BY; below the minimum, the minimum; the higher occurrences deleted | **implemented** (set.md) |
| 8.5.1.9.5 | INITIALIZED: new elements as INITIALIZE WITH FILLER ALL TO VALUE THEN TO DEFAULT | **implemented**: an element's initial state is an image the compiler builds; without INITIALIZED the text leaves them undefined -- here the categories' defaults (spaces, zero) |
| 8.5.1.9.6 (1) | EC-BOUND-OVERFLOW, nonfatal, when the expected capacity is first exceeded | **implemented**: dyntable (store 6, then 7 raises nothing) |
| 8.5.1.9.6 (2) | EC-BOUND-TABLE-LIMIT, fatal, past the maximum capacity | **implemented**: raised when checked; fatal in the runtime otherwise; not driven by a test (16 million elements) |
| 8.5.1.9.1; 13.18.63.4 rule 16 | the initial capacity: FROM, or the expected capacity when a VALUE clause reaches the elements | **implemented**: dyntable's `t` starts at 5 (its items have VALUEs), `e` at its FROM 2 |
| 13.18.38.3 rule 28 | FROM nonnegative, TO greater | **refused**: bad/std2014-dyn-from-to |
| 13.18.38.3 rule 29 | FROM and TO within the implementor's maximum | **refused** (16,777,215) |
| 13.18.38.3 rules 30-32 | the CAPACITY item not defined elsewhere, not subscripted, not a receiving operand but for SET | **refused**: bad/std2014-dyn-cap-dup, -dyn-cap-receive (no subscripts: it is not a table) |
| 13.18.44.3 rule 17 | neither REDEFINES side holds a dynamic table | **refused**: bad/std2014-dyn-redefines |
| 8.5.1.12, 14.6.9 | a variable-length group moved or compared whole, with a compatible group | **gap** in this stage: **refused** (bad/std2014-dyn-group-move, -dyn-group-recv); BY REFERENCE it passes (dyntable2's `g`) |
| 14.9.20.4 rule 10 | INITIALIZE of a group holding the table: every element up to the capacity, the capacity unchanged | **implemented**: without phrases the categories' defaults, under ALL TO VALUE the VALUEs (dyntable); REPLACING and a category's VALUE over such a group **refused** in this stage (bad/std2014-dyn-init-replacing) |
| 14.9.37, 14.9.40, 8.4.2.3.3 | SEARCH, SEARCH ALL, a table SORT and the ALL subscript run to the current capacity | **implemented**: dyntable |
| 14.9.5.4 rule 2 | CANCEL: the initial state | **implemented**: the elements given up, the capacity the initial one (dyntable2's dyn2own) |
| 13.18.38.4 rule 15 | the capacity item is numeric | **implemented**: unsigned, four bytes, DISPLAYed and compared as any integer |
| -- | a dynamic table in LINKAGE, LOCAL-STORAGE, a BASED record | LINKAGE **test** (dyntable2: the slot is the caller's); LOCAL-STORAGE and BASED take the same path, untested; a LOCAL-STORAGE table's elements are not reclaimed when the activation ends (**gap**) |
