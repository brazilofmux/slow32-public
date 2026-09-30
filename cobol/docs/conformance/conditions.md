# Relation conditions: 8.8.4.2

Swept 2026-09-30. X3.23-1985: 6.3.1.1 (VI-54..VI-56). 2023: 8.8.4.2.
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
