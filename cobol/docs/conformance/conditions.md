# Conditions: 8.8.4.2 relations; 8.7.5, 8.8.4.3-8.8.4.11 the other simple conditions, negated and combined

Swept 2026-09-30 (relations); 2026-10-07 the rest (queue item 19b, below). X3.23-1985: 6.3.1.1 (VI-54..VI-56). 2023: 8.8.4.2.
CCVS-85 tests comparison at length: the NC relation programs, and the
IF and EVALUATE programs. So the probes went after what it does not
test: what must be refused, and the edges of the comparison rules.
Every probe gives the same answer with and without `-fno-hot-arith`,
which means the in-line compares agree with `cob_cmp` (the both-paths
gate checks that on every run).

## 8.8.4.2.1 general

| rule | paraphrase | disposition |
|---|---|---|
| the defined comparisons | numeric, alphabetic, alphanumeric, boolean and national pairs; a numeric integer with text; the alphanumeric-class mixtures; indexes; message-tag, object and pointer; strongly-typed groups of one type; variable-length groups | the rows below; message-tag and object are **n/a** (object orientation ruled out); variable-length groups are **n/a** (2014's dynamic-length items) |
| groups, alphabetic | a group compares as an alphanumeric item, an alphabetic one likewise | **test**: free/relrules (the group's part); CCVS |
| NOTE | a numeric-edited item compares as its characters, so equal values in different pictures differ | **test**: free/relrules (`zz9.99` and `z9.999` holding 1.5 are not equal) |
| operands | at least one operand is not a literal | **refused**: bad/rel-literals (`1 = 1`; `"a" = "b"` and `ZERO = 0` likewise). All three were accepted before this sweep, as GnuCOBOL accepts them; no program in any corpus writes one. An abbreviated relation and EVALUATE's own comparisons are not written relations and are not checked. A function reference is not a literal even where the compiler evaluates it (`FUNCTION LENGTH("ABC") = 4`, CCVS IF115A and IF402M: the first cut of this check refused them). This suite's own free/ebcdic compared two literals, and now compares items |

## 8.8.4.2.3 syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | strongly-typed groups: both of one type | **refused**: bad/std2002-strong-compare |
| 2, 3 | identifiers and literals of the classes that compare | **refused** where a class has no comparison: the boolean and pointer rules below |
| 4 | a strongly-typed group holding boolean, message-tag, object or pointer items: EQUAL and NOT EQUAL only | **refused**: "a strongly-typed group holding a boolean item is compared by EQUAL or NOT EQUAL only" |
| 5 | pointers compare with pointers of their category, EQUAL or NOT EQUAL | **refused**: bad/std2002-pointer-cmp-num; **test**: 2002/pointerset, 2002/ecptrnull |

## Comparisons

| rule | paraphrase | disposition |
|---|---|---|
| 8.8.4.2.4 numeric | by algebraic value, whatever the usage; the length of literals and expressions not significant; zero a unique value whatever its sign | **test**: free/relrules (a DISPLAY and a packed negative zero against positive zero and ZERO; COMP against DISPLAY with decimals; signed against unsigned), CCVS NC |
| 8.8.4.2.5 numeric beside text | the numeric operand an integer literal or an integer item of usage display or national, compared as its digits moved to text of their length | **test**: free/relrules (`9(3)` 42 equals `"042"`, and a longer item padded with spaces; `042 < "1"` as characters); **refused**: bad/rel-noninteger (a noninteger item or literal), bad/rel-usage (a COMP item), bad/rel-expr (an expression). All were accepted before this sweep. 1985's wording is "the same usage", 2023's "display or national". A figurative constant is exempt (**ruling**): `IF amount = SPACES` is common in real code, and ZERO and SPACE fit either class |
| 8.8.4.2.6, .9 alphanumeric beside national; national | via a temporary national item | **test** and **refused**: national-boolean.md |
| 8.8.4.2.7 alphanumeric | by the program collating sequence; equal lengths position by position; the shorter padded with spaces; locale-based comparison under a locale collating sequence | **test**: free/relrules (padding), free/cmpalnum, free/ebcdic and free/classcond (an ALPHABET as the collating sequence); locale-based comparison is a **gap**: a locale is named only by SPECIAL-NAMES LOCALE, which is refused ("SPECIAL-NAMES clause 'locale' is not implemented yet") |
| 8.8.4.2.8 boolean | by boolean value; the shorter padded with zeros | **test** and **refused**: national-boolean.md |
| 8.8.4.2.12 strongly-typed groups | as alphanumeric, same type only | **refused** as rule 1; **test**: 2002/strongtype |
| 8.8.4.2.13 indexes | index-name with index-name by occurrence number; with an item or literal, the occurrence number against the value | **test**: fixed/index-name-operand, CCVS NC (SEARCH and SET programs) |
| 8.8.4.2.16 pointers | equal when they address the same storage | **test**: 2002/pointerset |

## Abbreviated combined relation conditions (8.8.4.12 in 2023; 6.3.3 in 1985)

**test**: free/relrules checks the forms that are easy to get wrong, and
the oracle agrees on each:
- `a > 6 and < 8`: the subject carried over;
- `a = 1 or 7`: subject and operator carried over;
- `not a = 1 or 2`: NOT applies to the first relation only;
- `a not = 1 and 2`: NOT belongs to the operator, so it is carried over
  with it.

CCVS NC's abbreviated programs cover the rest.

## 8.7.5, 8.8.4.3, 8.8.4.4, 8.8.4.5, 8.8.4.6, 8.8.4.7, 8.8.4.8, 8.8.4.9, 8.8.4.10, 8.8.4.11: the other simple conditions, negated and combined conditions (2026-10-07, item 19b)

2023: 8.7.5 relational operators, 8.8.4.3 boolean, 8.8.4.4 class,
8.8.4.5 condition-name, 8.8.4.6 switch-status, 8.8.4.7 sign, 8.8.4.8
omitted-argument, 8.8.4.9-11 complex conditions. Test 2002/condsweep
(no oracle: GnuCOBOL 4 has no alphabet-name class condition and tests
a floating-point item's sign by value). The sweep found six unenforced
rules, a crash (a class condition on a reference-modified item, in a
unit whose first descriptor it was: an evaluation-order fault reading
the descriptor table before the call that allocated it) and two
behaviours short of the text (NUMERIC of a truncating binary beyond
its PICTURE was true; a floating-point item's bare sign test went by
value, so -0.0 was not NEGATIVE), all fixed.

| rule | paraphrase | disposition |
|---|---|---|
| 8.7.5 | the simple relational operators and the extended ones (>=, NOT <, <=, NOT >, NOT =) | **test**: CCVS NC; the abbreviations above |
| 8.8.4.3 SR 1 | a boolean condition's expression references boolean items of length 1 | **refused**: "a boolean condition takes a boolean item of one position" (national-boolean.md) |
| 8.8.4.3 GR 1-2 | true when the value is 1; NOT reverses | **test**: `IF b1`, `IF NOT b1` |
| 8.8.4.4 SR 1 | not of class index, message-tag, object or pointer, a strongly-typed group or a variable-length group | **refused**: bad/std2002-class-pointer -- accepted before this sweep; a strongly-typed group was refused already; an index item and a group over an OCCURS DEPENDING ON table likewise now; objects and message-tags **n/a** |
| 8.8.4.4 SR 2 | not an alphabet of a locale | locales are item 45; the alphabet forms taken (NATIVE, STANDARD-1, EBCDIC, literals) name characters |
| 8.8.4.4 SR 3 | the character tests (alphabet-name, ALPHABETIC, -LOWER, -UPPER, BOOLEAN, class-name) of a DISPLAY or NATIONAL item, or an alphanumeric or national function | **refused**: bad/std2002-class-alpha-packed -- accepted before (COMP-3 IS ALPHABETIC); **test**: `FUNCTION UPPER-CASE(lo) IS ALPHABETIC-UPPER`, `FUNCTION TRIM(t) IS NUMERIC` -- a function was refused before this sweep |
| 8.8.4.4 SR 4 | ALPHABETIC, -LOWER, -UPPER, class-name not of a boolean, numeric or numeric-edited item | **refused**: bad/std2002-class-alpha-numeric -- accepted before this sweep |
| 8.8.4.4 SR 5 | BOOLEAN not of a numeric or numeric-edited item | **refused**: bad/std2002-class-boolean-edited (the numeric case was refused already) |
| 8.8.4.4 SR 6-7 | FARTHEST-FROM-ZERO, IN-ARITHMETIC-RANGE, NEAREST-TO-ZERO of a numeric item; FLOAT-INFINITY, FLOAT-NOT-A-NUMBER(-QUIET, -SIGNALING) of a standard floating-point item | **implemented** under -std=2014 (queue item 21, 2026-10-07): 2014/floatcontent (no oracle: GnuCOBOL 4 has none). **Refused**: bad/std2014-class-numeric-only, -class-float-only; under -std=2002 as 2014's (bad/std2002-float-condition). **Ruling**: FLOAT-SHORT, -LONG and COMP-2 take the floating-point conditions too, being IEEE here (docs/usage.md) |
| 8.8.4.4 SR 8 | NUMERIC of a DISPLAY or NATIONAL item, or a numeric one | **refused** of a bit item ("NUMERIC tests a DISPLAY or NATIONAL item, or a numeric one") |
| 8.8.4.4 GR 1-2 | a zero-length item false; NOT reverses | zero-length items are 2014's; **test** |
| 8.8.4.4 GR 3a | alphabet-name: every character one of the alphabet's | **test**: `x(1:2) IS alf`, `x IS NOT alf` with `ALPHABET alf IS "A" THRU "Z"` -- accepted as a class by name only now (an alphabet's characters are kept beside its ranks) |
| 8.8.4.4 GR 3b-d | ALPHABETIC: letters of either case and space; -LOWER, -UPPER; a locale's LC_CTYPE when one is in effect | **test**; locales item 45 |
| 8.8.4.4 GR 3e-f | BOOLEAN: every position 0 or 1; class-name: the class's characters | **test**: `bl IS BOOLEAN`, `x(4:2) IS hexdig` |
| 8.8.4.4 GR 3g-m | FARTHEST-FROM-ZERO and NEAREST-TO-ZERO: the item's extreme values, either sign; the IEEE specials by their representations; IN-ARITHMETIC-RANGE: within the mode of arithmetic's intermediates | **test**: 2014/floatcontent -- DISPLAY, packed, COMP and COMP-5 items by their PICTUREs and bytes (a two's-complement item's negative extreme one farther), every float format's largest finite and smallest subnormal; infinities and quiet and signaling NaNs of each format, with their sign; **ruling**: NATIVE's intermediates (38 digits, doubles, floating decimals of any exponent) hold every finite value, so IN-ARITHMETIC-RANGE is true of each and false of an infinity or NaN (docs/usage.md) |
| 8.8.4.4 GR 3n.1a | NUMERIC of a DISPLAY numeric: digits, and the sign's presence as described | **test**: `nd` (S9(3), -12) numeric, "-1c" in it not |
| 8.8.4.4 GR 3n.1b | a standard floating-point item: finite | **test**: `fl IS NUMERIC` (-0.0); infinities and NaNs are item 21's |
| 8.8.4.4 GR 3n.1c | other usages: a valid representation, and within the PICTURE's range | **test**: a COMP 9(2) holding X"FFFF" is **not** NUMERIC -- was true before this sweep; packed: each nibble a digit, the last a sign; **ruling**: COMP-5 and COMP-X hold their bytes' full range (docs/usage.md, BP-E3), so they are NUMERIC whatever the PICTURE |
| 8.8.4.4 GR 3n.2 | NUMERIC of a non-numeric item: digits only | **test**: `x(4:2)`, `FUNCTION TRIM(t)` |
| 8.8.4.5 GR 1-3 | a condition-name's ranges inclusive, its values compared as relations are; true when one matches | **test**: `mid-k` (10 THRU 19 25), `odd-k`; CCVS NC |
| 8.8.4.6 | the switch's on or off status as SPECIAL-NAMES named it | **test**: `sw1-off`, `SET sw1 TO ON`, `sw1-on`; CCVS SW-1 |
| 8.8.4.7 SR 1 | format 1: an arithmetic expression, or a single item of a usage other than standard floating-point | **test**: `nd + 20 IS POSITIVE`; **refused** of an alphanumeric item ("a sign condition needs a numeric operand") |
| 8.8.4.7 SR 2 | format 2: a standard floating-point item, bare, not in parentheses | **test**: `fl IS NEGATIVE` |
| 8.8.4.7 GR 1 | format 1 by the value: POSITIVE > 0, NEGATIVE < 0, ZERO = 0 | **test**: `(fl) IS NEGATIVE` false for -0.0 |
| 8.8.4.7 GR 2 | format 2 by the IEEE sign bit, whatever the value (-0.0 NEGATIVE, -INF, a NaN with the sign); ZERO when the value is zero of either sign | **test**: -0.0 NEGATIVE and ZERO, not POSITIVE -- went by value before this sweep |
| 8.8.4.8 SR 1, GR 1-2 | IS OMITTED of a formal parameter: OMITTED written, a trailing argument left off, or the caller's own omitted parameter passed on | call.md (2002/fnproto, omitted); **refused** of an item that is not a parameter ("IS OMITTED tests a level 01 or 77 LINKAGE item") |
| 8.8.4.9 | AND, OR, NOT; EXCLUSIVE-OR and XOR | **test**; XOR is 2023's, **gap** by name (docs/refusals.md) |
| 8.8.4.10 | NOT reverses; parentheses leave it | **test**: `NOT (a = 1 AND b = 1)` |
| 8.8.4.11 precedence | NOT, AND, (XOR), OR; parentheses alter it | **test**: `a = 1 OR NOT b = 1 AND a = 0` true, `(a = 1 OR NOT b = 1) AND a = 0` false |
| 8.8.4.11 table 5 | the permitted neighbours: OR NOT yes, NOT OR and NOT AND no, NOT ( yes, NOT NOT no; parentheses paired | **refused**: bad/std2002-cond-not-not, -cond-not-or -- NOT NOT was accepted before this sweep, NOT OR met "'not' is not a COBOL verb" |
