# 13.18.40 PICTURE clause (and 13.18.8 BLANK WHEN ZERO)

Swept 2026-09-29 (ISSUES-96). X3.23-1985: VI-29..43 (PICTURE: syntax
rules 1-7, the categories, the editing rules, the precedence chart on
VI-37) and VI-22 (BLANK WHEN ZERO). The boolean and national rules were
swept with the national and boolean data ([national-boolean.md](national-boolean.md)).

Before this sweep the PICTURE analyser (src/picture.c) took the symbols'
meaning but checked almost none of the order and combination rules:
`99CRCR`, `9V9V9`, `S9S9`, `Z*9`, `+99CR`, `9+9`, `9$9`, `++$$9`,
`ZZ.Z9` and `.$$` all compiled. It now checks the syntax rules one by
one (the messages cite them) and then every ordered pair of symbols
against Table 10, the precedence chart, read off the 2023 PDF by the
position of each mark.

**Differential.** Every picture of one to three symbols over `9 Z * + -
$ . V P S B 0 / , CR DB`, and 3,000 random ones of four to seven -- 7,368
in all -- compiled by this compiler and by GnuCOBOL 4 (-std=cobol85,
which implements the chart). They agree on 7,313. Of the 55 that
differ, each was checked against the text and in each the text is with
this compiler:

- 22 GnuCOBOL accepts: `++Z` and `-0-/BZ0` (two strings, rule 27),
  `.$$` and `V++` (a floating string wholly right of the point, rule
  29), `+P` (no digit symbol, rule 12a);
- 33 GnuCOBOL refuses: `9P+`, `ZP$`, `9PCR` (a trailing P before a
  trailing sign or currency symbol -- Table 10 allows it), `$P9`, `+P9`
  (a leading P after a fixed sign or currency symbol -- allowed),
  `$++`, `$--` (a fixed currency symbol before a floating sign --
  allowed), and `$Z`, `$ZZ`, `$**`, `+$Z` (GnuCOBOL takes the leading
  `$` for a trailing one when no 9 follows).

`tests/pic-differential.sh` reruns it (it needs the GnuCOBOL podman
image, a few minutes) and fails if the count of disagreements moves
from 55; `tests/pictures.txt` keeps 42 of the cases in the harness.

## Syntax rules (format 1)

| rule | paraphrase | disposition |
|---|---|---|
| 1 | elementary items only | **refused**: "'x' is a group and cannot have a PICTURE" |
| 2 | an allowable combination (13.18.40.6) | **refused**: every pair against Table 10 -- bad/pic-precedence; not checked before this sweep |
| 3 | letters in either case | **test**: throughout (the harness writes lowercase) |
| 4 | at most 63 characters (2023); 50 in 2002; 30 in 1985 | **refused**: bad/pic-too-long -- no limit was applied before this sweep |
| 5 | PIC is PICTURE | **test**: throughout |
| 6 | a repeat count is an unsigned nonzero integer | **refused**: bad/pic-zero-count (national-boolean sweep). A constant-name as the count is 2014's: **n/a** |
| 7 | a picture ending in `,` or `.` ends the entry | **test**: the scanner takes `, ` and `. ` as separators |
| 8-12 (FOR, EDITING) | extended sign control, the EDITING phrase | **n/a**: 2014's |
| 12a | at least one of A, X, Z, 9, * (N, 1), or two of +, -, cs | **refused**: bad/pic-no-digit-symbol -- `P` alone, `+P`, accepted before |
| 12b | CR, DB, S, V, `.` each once | **refused**: bad/pic-crdb-twice; V and `.`: pictures.txt |
| 13 | DECIMAL-POINT IS COMMA swaps `,` and `.` | **test**: free/dpcomma; the swap is applied before the analysis |
| 14 | 1-31 digit positions (2023); 18 in 1985 | 18 **refused** by name under -std=85; under -std=2002 19-31 **implemented** (docs/wide.md, 2002/wide1) and 32 **refused** (bad/std2002-wide-limits) |
| 15 | floating-point edited | **n/a**: 2014's |
| 16 | P one run at the leftmost or rightmost digit positions | **refused**: pictures.txt (`9P9`, `P9P`) |
| 17 | P and `.` exclude each other | **refused**: bad/pic-p-and-point |
| 18 | S first | **refused**: bad/pic-s-not-first |
| 19 | V next to the P run | **refused**: pictures.txt (`PPV99`, `99VPP`) |
| 20 | V and `.` exclude each other | **refused**: pictures.txt (`9V.9`) |
| 21 | Z and * exclude each other | **refused**: bad/pic-z-and-star |
| 22 | S and * not with BLANK WHEN ZERO | **refused**: bad/bwz-sign, bwz-star (85: PICTURE rule 7 for *; S by the category) |
| 23 | +, -, CR, DB exclude each other | **refused**: bad/pic-sign-exclusive |
| 24 | fixed insertion: one currency symbol, one sign | **refused**: by rules 23, 26 and the chart |
| 25 | a fixed + or - at an end | **refused**: bad/pic-fixed-sign-middle |
| 26 | a fixed currency symbol at an end, beside a sign | **refused**: bad/pic-currency-middle |
| 27 | one floating or zero-suppression string | **refused**: bad/pic-two-floating |
| 28 | one currency symbol character in a floating string | **n/a**: one currency symbol per program here (PICTURE SYMBOL is not taken) |
| 29 | a floating string starts left of the point | **refused**: bad/pic-float-after-point |
| 30 | no A or X with USAGE NATIONAL | **refused** (national-boolean sweep) |
| 31 | S not with NO SIGN | **n/a**: 2014's |
| 32-37 (format 2) | locale-based editing | **n/a**: the locale functions are out of scope by ruling |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 1-2 | national positions under usage national | **test**: 2002/natedit |
| 3-11 | the categories | **test**: tests/pictures.txt (the categories, sizes, digits and scale of 100 pictures) |
| 13 | numeric-edited: the symbols allowed; S is not one | **refused**: pictures.txt; `S` with an editing symbol names 13.18.40.4 rule 13 |
| editing (13.18.40.5) | simple, special, fixed and floating insertion; zero suppression and replacement; a Z past the point takes every digit | **test**: free/picedit (the oracle agrees), free/picmix, CCVS NC editing programs; **refused**: bad/pic-z-past-point, bad/pic-9-before-z |
| 15 | VALIDATE | **n/a**: out of scope by ruling |

## 13.18.8 BLANK WHEN ZERO

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | numeric-edited, or numeric without S (85 rule 1: numeric or numeric-edited) | **refused**: bad/bwz-alnum, bwz-sign -- neither checked before this sweep |
| SR 2 | usage display or national | **refused**: bad/bwz-comp -- not checked before |
| GR 1-2 | spaces for zero; a numeric item becomes numeric-edited | **test**: free/picedit |
| GR 3 | a sending item of all spaces is zero | **test**: CCVS NC |

The same checks apply to report group and screen fields.

## Found by this sweep

The precedence chart and syntax rules 2, 4, 12, 16-29 (all but 16's
two-sided case) were not enforced, nor BLANK WHEN ZERO's two rules; a
picture could be any length. CCVS-85, the Open Systems suite (229
programs byte-identical under -std=85) and majesty are unaffected: no
picture they hold breaks a rule.
