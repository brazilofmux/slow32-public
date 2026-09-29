# INITIALIZE: 14.9.20

Swept 2026-09-29 (ISSUES-104). X3.23-1985: 6.17, the INITIALIZE
statement. 2023: 14.9.20. The 1985 statement was implemented at Stage
45 (dialect.md); this sweep implemented COBOL 2002's phrases -- WITH
FILLER, `{ALL | category} TO VALUE`, THEN REPLACING, THEN TO DEFAULT --
and checked the syntax rules of both editions.

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 2023 1 | identifier-1 of a class INITIALIZE can set | **refused**: an index-name (bad/initialize-operands; accepted before this sweep; since ISSUES-108 by the general index-name rule, occurs.md); a USAGE INDEX item (usage.md) |
| 2023 3-4 (85: 2) | each REPLACING value a valid MOVE (SET, for the pointer categories) to its category | **refused** through the MOVE rules; DATA-POINTER takes a pointer item or NULL. FUNCTION-POINTER, PROGRAM-POINTER, MESSAGE-TAG and OBJECT-REFERENCE: no such items exist here, so the words are refused as not implemented |
| 85: 4 | no OCCURS DEPENDING ON in identifier-1 or below it | **extension** under -std=85: BP-E15 (majesty's gl008, gl034, gl040 INITIALIZE such tables); 2023 GR 8 allows it |
| 2023 5 (85: 6) | no RENAMES item | **refused**: bad/initialize-operands -- accepted before |
| 2023 6 (85: 3) | a category once in REPLACING | **refused**: bad/initialize-operands -- accepted before |
| the phrases | COBOL 2002 | **refused** under -std=85 ("is COBOL 2002; compile with -std=2002") |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 3 | several identifiers: as if one INITIALIZE each, in order | **test**: 2002/init2002 (elem) |
| 4, 8 | a series of implicit MOVEs (SETs for pointers) to elementary items, in the order of definition, every occurrence | **test**: 2002/init2002, 2002/init2002cat |
| 5a | excluded: FILLERs unless WITH FILLER, REDEFINES items below the receiver, index items | **test**: 2002/init2002 (filler, fil+val; the REDEFINES item keeps its bytes) |
| 5c1 | under the VALUE phrase an item is a receiver only if its category is the one named (or ALL) and it has a VALUE clause (a pointer always) | **test**: 2002/init2002cat -- no oracle: GnuCOBOL restores every VALUE whatever the category named |
| 5c2-4 | else the REPLACING category; else DEFAULT; else only when neither VALUE nor REPLACING is given | **test**: 2002/init2002 (val+def, rep+def, all), 2002/init2002cat (num+rep, alnum+def) |
| 6a | the VALUE clause's value; NULL for a pointer | **test**: 2002/init2002cat (pointer) -- GnuCOBOL leaves the pointer alone |
| 6c | the defaults: SPACES for the alphabetic, alphanumeric and national categories and their edited forms, ZEROES for numeric, numeric-edited and boolean, NULL for a pointer | **test**: 2002/init2002, 2002/init2002cat (default) |
| ALLOCATE GR 7 | ALLOCATE data-name INITIALIZED is INITIALIZE WITH FILLER ALL TO VALUE THEN TO DEFAULT | **test**: 2002/allocinit, the oracle agrees -- refused as not implemented before |
| 7, 10 | dynamic-length items and dynamic-capacity tables | **n/a**: 2014's |

The 1985 forms (no phrase, or REPLACING alone) keep their own code path
and are byte-identical to before; the 2002 phrases take a new walk
(init_walk) that decides per elementary item. A reference-modified
identifier-1 with the 2002 phrases is refused as not implemented.
An OCCURS DEPENDING ON table below identifier-1 is unrolled to its
maximum, as the 1985 path does; GR 8's "the rules of the OCCURS clause
for a receiving item" is not tested beyond that.

## Found by this sweep

The 2002 phrases were refused as not implemented; a RENAMES item, an
index-name and a repeated REPLACING category were accepted. GnuCOBOL
agrees on every case except the category-restricted VALUE phrase and the
pointer, where it departs from GR 5c1 and 6a1.
