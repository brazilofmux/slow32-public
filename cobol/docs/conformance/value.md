# VALUE: 13.18.63

Swept 2026-09-29 (ISSUES-107). X3.23-1985: 5.15, the VALUE clause
(VI-47..49), and its condition-name rules. 2023: 13.18.63, formats 1
(an item's initial value) and 3 (condition-names). Format 2 (table
values), 4 and 5 are 2014's.

The rules that need the whole description are checked in one pass
after layout (value_rules), each entry on its own so that a mistake is
reported and the next one checked (ISSUES-41). An item's own literal
that is too long, or whose integer part does not fit, is still reported
where its image is built, with the messages it always had.

## Format 1

| rule | paraphrase | disposition |
|---|---|---|
| 2023 1 | not a strongly-typed group | **refused** (the typedef sweep) |
| 2023 2 (85: 3, GR 1a) | a numeric item: numeric literals (or ZERO) within the PICTURE, no nonzero digit lost | **refused**: bad/value-rules -- VALUE SPACES and a lost decimal digit (9(2) VALUE 1.5) were accepted before; an integer part too large was refused already. **taken**: a nonnumeric literal of digits, which CCVS-85 gives numeric items (NC107A, NC108M) |
| 2023 3 (85: 2) | a signed literal only for a signed item | **refused**: bad/value-rules -- the sign was dropped silently before |
| 2023 4 (85: 3, GR 1b) | alphanumeric categories: nonnumeric literals, no longer than the item; a group's literal no longer than the group | **refused**; the group case (bad/value-rules) was accepted before and truncated |
| 2023 5, 10 | national and boolean literals for those categories | **refused** (the national and boolean sweeps) |
| 2023 6 | a numeric literal for a numeric-edited item, edited as a MOVE would, no digit or sign truncated; ZERO the literal zero (E.2 item 28; E.3.3 item 43 dates both to 2023 -- 2002 and 2014 wanted the literal written edited, their rule 8) | **implemented** under -std=2023 (standard-queue item 32, 2026-10-07): the runtime's editing kernel (libcob/kern.h, host-compiled as the DBT compiles it) edits the literal into the image, so a VALUE and the MOVE of the same literal give the same bytes. **Test**: 2023/numedvalue (each VALUE beside its MOVE; no oracle: GnuCOBOL 4 refuses the literal). **Refused**: digits, decimals or a sign that would be truncated (bad/std2023-numed-value-digits, -decimals, -sign); more than 18 digits, a floating-point picture, a CURRENCY ... PICTURE SYMBOL string (not implemented, write it edited); under -std=2002 and -std=2014 as 2023's (bad/std2014-numed-value), under -std=85 by GR 1b (bad/edited-value). A numeric item with BLANK WHEN ZERO is numeric-edited (13.18.8.4 rule 2): its zero VALUE is spaces under every edition, as GnuCOBOL has it |
| 2023 7 | an alphanumeric or national literal for a numeric-edited item conforms to the picture, the editing symbols in it (E.2 items 27, 29) | **implemented** under -std=2023: the literal de-edited and edited again by the kernel must give itself back (bad/std2023-numed-value-literal: `"012.50"` for ZZ9.99); all spaces is zero under BLANK WHEN ZERO; a national literal, a floating-point picture and a currency string are not checked. Under the earlier editions the literal is stored as written (2002 rule 8, the programmer's responsibility) |
| 2023 9 | not for the pointer usages | **refused**: USAGE POINTER and INDEX take no VALUE clause |
| 2023 12 (85: 5.15.6(2)) | none in or below a REDEFINES entry | **refused** (redefines.md) |
| 2023 13 (85: 5.15.6(3)) | a group's VALUE: nothing below it has one | **refused**: bad/value-rules -- accepted before |
| 2023 14 (85: 5.15.6(4)) | nothing below a group with a VALUE is JUSTIFIED, SYNCHRONIZED or other than DISPLAY | **refused**: bad/value-rules -- accepted before |
| 85: 5.15.6(1) | in the FILE and LINKAGE SECTIONs only condition-names take a VALUE | **refused** under -std=85 (bad/value-rules); 2023 allows it, the value taking effect through INITIALIZE (GR 2-3) |

## Format 3, condition-names

| rule | paraphrase | disposition |
|---|---|---|
| 2 -- 4 applied to the conditional variable | the literals suit the variable's category and size | **refused**: bad/value-rules -- a nonnumeric literal for a numeric variable, a value past its PICTURE, a literal longer than an alphanumeric variable were accepted before |
| 26 (85: condition-name rule 2) | THRU runs from the lower value to the higher | **refused**: bad/value-rules -- accepted before; alphanumeric ranges are compared only under the native collating sequence |
| 27 | the FALSE literal is none of the values, nor in a range | **refused**: bad/std2002-value-false |
| 29 | no THRU for a boolean variable | **refused** (the boolean sweep) |
| FALSE phrase, SET ... TO FALSE | [WHEN SET TO] FALSE IS literal-4; SET moves it to the variable | **test**: 2002/condfalse, the oracle agrees -- refused as not in COBOL 85 before, under -std=2002 too. SET TO FALSE of a condition without the phrase is refused (14.9.39.3 rule 7) |

None of the new refusals fires in CCVS-85, the Open Systems suite or
majesty.
