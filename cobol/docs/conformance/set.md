# SET: 14.9.39

Swept 2026-09-30. X3.23-1985: 6.23 (VI-127..VI-129). 2002: 14.8.35.
2023: 14.9.39. Formats 1-4 are the ones in 1985 and in real programs:
index assignment, index arithmetic, switches and condition-names. The
pointer formats are 2002's and were swept with them (pointerset,
addressof); format 15, SET CONTENT OF (2014), is below; the object,
locale, dynamic-length and message-tag formats belong to features ruled
out or not yet taken.

CCVS-85 tests all four formats (the NC SET, SEARCH and switch
programs), and its totals held when the rules below were enforced, so
every refusal here was something the compiler accepted that no
conforming program writes. GnuCOBOL accepts nearly all of them (it
allows `SET n TO 5`, `SET ixd TO 3` and `SET n UP BY 1`), so the text
decides. Micro Focus's SET page agrees with 1985 on formats 1 and 2.

Four of this suite's own tests leaned on the old laxness and were
rewritten: `free/notrunc` (`SET u5 DOWN BY 1` on a COMP-5 item, now
SUBTRACT), `2002/movecorr` (an index data item set from a literal),
`2002/wide4` (an integer item set from a 31-digit one) and the
exception-sites gate (`SET ix TO n`, `ix` an index data item).

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1985 SR 2; 2023 SR 1 | identifier-1 is an index data item or an integer item | **refused**: bad/set-recv-decimal (a `9v9` receiver); an alphanumeric one likewise |
| 1985 SR 2; 2023 SR 2 | identifier-2 is an index data item (1985: or an integer item) | **refused**: bad/set-index-from-decimal (1985). Under 2002 a numeric item is arithmetic-expression-1, allowed |
| 2023 SR 3; 1985 GR 3b | an index data item takes an index-name or another index data item, never a literal or expression | **refused**: bad/set-ixd-from-literal, bad/std2002-set-ixd-expr |
| 2023 SR 4; 1985 GR 3c | an integer item takes only an index-name | **refused**: bad/set-int-from-literal, bad/std2002-set-int-from-item |
| 1985 SR 3 | identifier-3 (UP/DOWN BY) is an integer item | **refused**: bad/set-updown-decimal |
| 1985 SR 4 | integer-1 is positive | **refused**: bad/set-index-zero, under `-std=85` only (2023's index range reaches below 1, OCCURS GR 2). Nothing in CCVS, majesty or this suite sets an index to zero |
| format 2 | the receiver of UP BY / DOWN BY is an index-name | **refused**: bad/set-updown-int-recv. 1985, 2023 and Micro Focus agree; GnuCOBOL accepts an integer item |
| 2002 format 1 and 2 | arithmetic-expression-1 and -2 | **test**: 2002/setexpr. Under `-std=85` the expression is refused, naming 2002 (bad/set-expr-85). GnuCOBOL takes an identifier or an integer only, so there is no oracle |
| SR 5 | the mnemonic-name is a switch's | **test**: CCVS NC174A; any other word takes format 1's route and is refused there |
| SR 6 | the condition-name has a conditional variable | **refused**: "'x' is not a condition-name" |
| SR 7 | TO FALSE needs the FALSE phrase in the VALUE clause | **refused**: "its VALUE clause has no FALSE phrase"; **test**: 2002/condfalse |

## Format 15, SET CONTENT OF (2014; 2023 14.9.39 format 15; queue item 21, 2026-10-07)

| rule | paraphrase | disposition |
|---|---|---|
| SR 31 | FARTHEST-FROM-ZERO and NEAREST-TO-ZERO of a numeric item; SIGN required where the two extremes differ in magnitude | **refused**: bad/std2014-content-sign-required (a COMP-5 item: -32768 against +32767); a DISPLAY, packed or COMP item is symmetric and takes no SIGN |
| SR 32 | FLOAT-INFINITY and the NaNs of a standard floating-point item | **refused**: bad/std2014-content-float-only; FLOAT-SHORT, -LONG and COMP-2 taken (the conditions' ruling, conditions.md) |
| GR 32, 36 | the value farthest from zero, or the nonzero one nearest, the item permits; IN-ARITHMETIC-RANGE: the arithmetic's own where that is closer (farther); the sign by SIGN, positive otherwise | **test**: 2014/floatcontent -- 999.99, 9999 COMP, +99999 COMP-3, 99900 for 9(3)PP, +32767 / -32768 COMP-5, 3.4028235E38 and 1.4E-45 binary32, 1.797E308 and 4.94E-324 binary64, binary128's 1.189E4932 and 6.475E-4966, 9.999999999999999E384 and 1E-398 decimal64, decimal128's; **ruling**: IN-ARITHMETIC-RANGE changes nothing here (docs/usage.md) |
| GR 33-35 | a canonical infinity, quiet NaN or signaling NaN of the item's format, the payload the implementor's (zero), the sign by SIGN | **test**: the bytes of binary128's infinity; each format's NaNs read back by the class conditions, DISPLAYed as Inf and NaN with the sign |
| 2002 | SET CONTENT OF under -std=2002 | **refused** as 2014's: bad/std2002-set-content |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| GR 1 | index-names belong to the table whose INDEXED BY names them | **test**: 2002/setformats |
| GR 2 | the sending value is taken once, at the start; each receiver is identified just before it changes | **test**: 2002/setexpr (`SET i1 j1 UP BY FUNCTION MAX(n, 4)` and `SET i1 j1 TO -1 + n * 3`: the expression is evaluated once, into a word) |
| GR 2a1 | an arithmetic-expression-1 that is not an integer: EC-BOUND-SUBSCRIPT, the receivers unchanged | **test**: 2002/setexpr (2.5 under checking: the declarative sees both indexes as they were). Unchecked, the fraction is dropped (**ruling**: no exception condition is checked, so the text asks nothing of the result) |
| GR 2a1b, 2a2a, 2a3b, 4a | a value outside the index's range: EC-RANGE-INDEX | **ruling**: an index here is a signed 32-bit occurrence number, so its range is the word. That covers the 1-n .. 2n minimum OCCURS GR 2 requires. The condition cannot arise short of wrapping the word |
| GR 2a2, 2a3 | from an item or an index-name of another table, the occurrence number carried over; of the same table, unchanged | **test**: 2002/setformats (`SET i2 TO i1` across tables of 5 and 9) |
| GR 2b | an index data item takes the index's content unchanged | **test**: 2002/setformats (`SET ixd TO i1`, `SET j1 TO ixd`), 2002/movecorr |
| GR 2c | an integer item gets the occurrence number | **test**: 2002/setformats, 2002/wide4 |
| GR 3 | an arithmetic-expression-2 that is not an integer: EC-BOUND-SUBSCRIPT | as GR 2a1: the same code path |
| GR 4 | UP BY / DOWN BY moves each index by the value | **test**: 2002/setformats, 2002/setexpr |
| GR 5 | a switch set ON or OFF, which its condition-names then report | **test**: CCVS NC174A, NC135A |
| GR 6 | TO TRUE: the first literal of the VALUE clause, placed by the VALUE clause's rules | **test**: free/setcond. Fixed in this sweep: for an edited item given an alphanumeric literal, VALUE places the characters as written (13.18.63.3 rules 4 and 7-8), but SET moved them with editing, so `SET c TO TRUE` left `c` false. GnuCOBOL still does that (docs/oracles.md). A group conditional variable over a table takes its length by OCCURS; this compiler lays such a group out at its maximum, which is that length |
| GR 7 | TO FALSE: the FALSE phrase's literal, likewise | **test**: 2002/condfalse, 2002/setformats |
| GR 8 | several condition-names: each in turn, left to right | **test**: 2002/setformats (`SET s-no s2-on TO TRUE`) |

Found on the way: a MOVE to a reference-modified edited item edited
the value, though a reference-modified item is alphanumeric
(8.4.2.4.3). SET TO TRUE's fix depends on that, and the MOVE is fixed
with it.
