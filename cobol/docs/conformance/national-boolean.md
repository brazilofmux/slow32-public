# National and boolean data: 13.18.29 GROUP-USAGE, 13.18.60 USAGE (BIT, NATIONAL), 13.18.40 PICTURE (1, N), 8.3.3.4-5 literals

Swept 2026-09-29 (ISSUES-96). All COBOL 2002: under -std=85 every one of
these is refused as 2002's (bad/national-85, boolean-85, groupusage-85).
Only the rules about national and boolean data are taken here; the rest
of USAGE and PICTURE is a later sweep. The 2002 text was consulted for
the literal limits, which 2014 and 2023 changed.

## 13.18.29 GROUP-USAGE

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | only on a group that is not strongly typed, not variable-length | **refused**: bad/std2002-natgroup-elem (an elementary item), bad/std2002-groupusage-strong -- a strongly-typed group was accepted before this sweep. Variable-length groups are 2014's: **n/a** |
| SR 2 | BIT: no USAGE on the subject; subordinates usage bit and boolean; subordinate groups GROUP-USAGE BIT | **refused**: bad/std2002-groupusage-bit, std2002-bit-pic-x; a GROUP-USAGE NATIONAL group inside a bit group: bad/std2002-groupusage-mixed (was refused, but blamed an item below it under rule 3) |
| SR 3 | NATIONAL: no USAGE on the subject; subordinates usage national; signed numerics SIGN SEPARATE; subordinate groups GROUP-USAGE NATIONAL | **refused**: bad/std2002-natgroup-usage, -natgroup-alnum, -natgroup-sign; a subordinate group with its own USAGE: bad/std2002-groupusage-subusage (was refused naming the wrong group and rule); a bit group inside a national group likewise now names rule 3 |
| GR 1 | a bit group is a boolean item PIC 1(m), its items laid out by 8.5.1.6.3 | **test**: 2002/boolbit, bitredef; with OCCURS, a bit item of m bits whose occurrences follow at the next bit, its items reached through its subscript: 2002/bitoccurs (2026-10-06, standard-queue item 12) |
| GR 2 | a national group is a national item PIC N(m) | **test**: 2002/natgroup |
| GR 3 | otherwise a group is alphanumeric | **test**: throughout |

## 13.18.60 USAGE -- the BIT and NATIONAL rules

| rule | paraphrase | disposition |
|---|---|---|
| SR 5 | USAGE BIT only with a boolean PICTURE | **refused**: bad/std2002-bit-pic-x (the message now cites the rule) |
| SR 7 | a report group item takes only DISPLAY or NATIONAL | **refused**: bad/std2002-rw-usage (was "expected 'display', found 'comp'") |
| SR 12 | USAGE NATIONAL with a boolean, national, national-edited, numeric or numeric-edited PICTURE | **refused**: bad/std2002-nat-usage-x, -natgroup-alnum, -nat-rw-usage-x; **test**: 2002/natnum. The message cited "13.18.66", the 2002 numbering; it and the tests now say 13.18.60 |
| SR 13 | no USAGE anywhere: N implies NATIONAL, else DISPLAY | **test**: 2002/national |
| SR 17 | a screen item takes only DISPLAY or NATIONAL | **refused**: bad/std2002-screen-usage. A USAGE clause in a screen entry was not parsed at all ("unexpected 'usage'"); DISPLAY and NATIONAL are taken now, a group's reaching its children. USAGE NATIONAL on a screen item whose PICTURE is not N is **gap**, refused by name |
| SR 20 | a PICTURE with N takes only USAGE NATIONAL | **refused**: bad/std2002-picn-display, -picn-group-display -- PIC N USAGE DISPLAY, its own or its group's, was accepted before this sweep; in a report group and a screen entry too |
| GR 1 | a group's USAGE applies to its elementary items | **test**: 2002/natnum; bad/std2002-picn-group-display |
| GR 5 | USAGE BIT: bits, aligned by 8.5.1.6.3 | **test**: 2002/boolbit, bitarray, bitredef |
| GR 8 | USAGE NATIONAL: the implementor's national set, character-aligned | **ruling**: UTF-16 big-endian, two bytes a character (docs/national.md, the encoding rulings) |

## 13.18.40 PICTURE -- the symbols 1 and N

| rule | paraphrase | disposition |
|---|---|---|
| SR 6 | a repeat count is an unsigned nonzero integer (85 VI-30 general rule 7) | **refused**: bad/pic-zero-count. Refused before, as "not valid at character 1"; now named |
| SR 30 | no A or X with USAGE NATIONAL | **refused**: bad/std2002-nat-usage-x |
| GR 1, 2 | under usage national each position, and each insertion character, is national | **test**: 2002/natedit |
| GR 3, 4 | the categories; the size counts 1 and N positions | **test**: 2002/boolean, national |
| GR 8 | boolean: only 1 | **refused**: bad/std2002-pic-bool-mixed (was "not valid at character 1") |
| GR 9, 10 | national: only N; national-edited: N with B, 0 or / (or character-1, the EDITING phrase, which is 2014's) | **refused**: bad/std2002-pic-nat-mixed; **test**: 2002/natedit |
| symbols | 1 is a boolean position (a bit, a character or a national character); N a national character position | **test**: 2002/boolean (PIC 1 USAGE NATIONAL among them), boolbit, natnum |

## 8.3.3.4 Boolean literals, 8.3.3.5 National literals

| rule | paraphrase | disposition |
|---|---|---|
| length | 2002: more than zero and at most 160 positions (8.3.1.2.3.2 and .4.2 rule 1); 2014 and 2023 allow zero and 8,191 | **refused**: bad/std2002-empty-boolean, bad/std2002-national-literal-161 -- a literal of any length was accepted before this sweep, the alphanumeric one too (bad/literal-161; 85's limit is the same 160) |
| B SR 2 | only 0 and 1 | **refused**: bad/std2002-bool-literal-digit |
| BX SR 3; GR 5-6 | hexadecimal digits, each four boolean positions | **test**: 2002/natlitquote |
| N SR 2 | a source character with a national correspondence | **ruling**: the source is UTF-8, every character has one; invalid UTF-8 is refused (docs/national.md) |
| N SR 3 | a doubled quotation symbol is one | **test**: 2002/natlitquote |
| NX SR 4-5 | hexadecimal digits, the implementor's number per character | **ruling**: four, a UTF-16 code unit; **test**: 2002/natlitquote; other counts refused ("NX needs four hexadecimal digits") |
| GR | class and category boolean or national; the run-time value | **test**: 2002/natlitquote, national, boolean |

## The leftovers closed with standard-queue item 12 (2026-10-06)

Test 2002/bitoccurs and 2002/posbits (no oracle: GnuCOBOL 4 has no
USAGE BIT).

| what | disposition |
|---|---|
| OCCURS on a bit group (13.18.29.4 rule 1b, 8.5.1.6.3) | **test**: 2002/bitoccurs -- an item has one bit dimension (its own occurrences', or an occurring bit group's above it: `bitdim`, `bitdim_stride`); a bit table inside an occurring bit group, two such dimensions, is refused |
| an arithmetic-expression subscript of a bit item | **test**: 2002/bitoccurs (`on-f(i + 1)`, `arr(i + k)`, `more(k + 2)`) |
| OCCURS DEPENDING ON a bit array (13.18.38) | **test**: 2002/bitoccurs: the group's length is the bytes its bits span (`cob_odo_length_bits`) |
| a bit item's part, or a bit array's element, in a screen item; a bit item in a positioned DISPLAY or ACCEPT | **test**: 2002/posbits: a boolean field (PICTURE 1) of 0 and 1 characters, moved to and from the bits; a positioned DISPLAY of a whole bit item shows its positions (it showed the item's bytes); a part at a computed position in a SCREEN SECTION item is still refused |
| BY CONTENT of a bit item's part | **test**: 2002/refmodbit (item 10); a part of computed length is still refused |
| a character item redefining a bit item that starts inside a byte (13.18.44.4 rule 1) | **ruling**: refused (bad/std2002-bit-redef-byte). Storage association starts at the redefined item's first bit, and items of every other usage are byte-addressed here: a character item at a bit position would need every reference to it shifted. A bit item over a byte item, and a bit item over a bit item, take the rule |

## Rulings recorded

- A hexadecimal alphanumeric literal (`X"..."`) is accepted under
  -std=85, though it is 2002's: an extension since Stage 1, which
  majesty uses. The 160-position limit applies to it as to any
  alphanumeric literal.

## Found by this sweep

GROUP-USAGE on a strongly-typed group, PIC N with USAGE DISPLAY (its
own or its group's; in the data division, a report group or a screen),
and literals past 160 positions were accepted. USAGE in a screen entry
was not parsed. Mixed GROUP-USAGE nesting and a subordinate group's
USAGE were refused under the wrong rule, naming the wrong item. The
USAGE rules were cited by their 2002 number. Pictures mixing 1 or N
with other symbols, and zero repeat counts, were refused as "not valid
at character 1". GnuCOBOL 4's national support is unfinished (it
DISPLAYs UTF-16 and misreads BX literals), so the national tests have
no oracle.

CCVS-85, the Open Systems suite (229 programs byte-identical under
-std=85) and majesty are unaffected.
