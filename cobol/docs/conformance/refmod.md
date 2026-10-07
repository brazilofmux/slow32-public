# 8.4.3.3 Reference-modification

Swept 2026-10-06 (docs/plans/standard-queue.md item 10), when the last
holes inside the implemented statements were closed. X3.23-1985: the
1989 amendment's reference modification (X3.23a); 2002: 8.4.2.4; 2023:
8.4.3.3. The national, boolean and bit forms were swept with the
national and boolean data (ISSUES-67, -76, -82); EC-BOUND-REF-MOD with
the exception module (docs/conformance/exceptions.md).

How a part of computed length is carried: its start and length are
evaluated where the statement evaluates its operands (`emit_rm_start_len`,
args.h), the bytes counted by the runtime (`cob_refmod_len`), and a
descriptor of that size made for the operation (`cob_refmod_desc`). A
screen item over such a part has a writable descriptor of its own that
the statement fills (`sfield_part`, `emit_dynpart_len`); a positioned
DISPLAY or ACCEPT of one has its width stored by the statement
(`COB_SR_DYNLEN`; under SIZE, the part's length in the value word,
`COB_SR_DYNSIZE`).

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | the item: boolean, national, alphanumeric (elementary or group), alphabetic, the edited categories and DISPLAY/NATIONAL numerics outside a strongly-typed group, a group that is neither strongly typed nor variable-length | **test**: 2002/refmodusage, natrefmod; **refused**: bad/std2002-strong-refmod, bad/refmod-arith (the part is not numeric); a binary or packed numeric item: "'x' is not reference-modified" |
| 2 | a function-identifier: an alphanumeric, boolean or national function | **refused**: bad/fn-refmod-numeric |
| 3 | not a reference-modified identifier again | **refused**: the parser takes one modification |
| 4 | leftmost-position and length are arithmetic expressions | **test**: 2002/refmodneg (an expression's value, not its intermediate results'; GnuCOBOL wraps an unsigned BINARY operand: docs/oracles.md), free/refmodsub, free/subrefmod |
| 5 | allowed wherever an identifier of class alphanumeric, boolean or national is | **test**: throughout; the operands that take no reference modification are each refused by their own rule (STRING's receiver, 14.9.43.3 rule 4: bad/string-refmod-receiver) |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 1-4 | positions: boolean, alphanumeric or national; a DISPLAY item's alphanumeric positions, a NATIONAL item's national ones, numbered from 1 | **test**: 2002/refmodusage, natrefmod |
| 5 | the unique data item: a subset of the item, from leftmost-position for length positions (bits of a bit item), to the end without a length; non-integer, zero or outside: EC-BOUND-REF-MOD | **test**: 2002/ecrefmod, fnrmpast, fnrmzero; free/lenrefmod; **refused** when the literal positions lie outside: bad/std2002-nat-refmod, bad/std2002-bitelem-refmod-past. A length of zero under REF-MOD-ZERO-LENGTH (2023): the section below |
| 6 | an elementary item without JUSTIFIED, of the item's class and usage: an edited item's part alphanumeric (national), a numeric item's part alphanumeric (national under usage NATIONAL), a bit item's boolean | **test**: 2002/refmodusage; INITIALIZE of a part takes that category (2002/refmodrest); a national sender to a numeric item's part is refused as to any alphanumeric receiver (Table 16) |
| 7 | in a function-identifier, the function's result is the item (the positions of a run-time-length result) | **test**: 2002/refmodrest (`FUNCTION UPPER-CASE (s)(i:n + 1)`), free/fnrefmod, 2002/fnrmpast |

## 8.5.4 Zero-length items, 8.3.3 zero-length literals, 7.3.23 REF-MOD-ZERO-LENGTH (2014/2023)

Implemented 2026-10-07 (standard-queue item 24), under -std=2014. Tests
2014/zerolen (GnuCOBOL 4 as the oracle where it agrees: it takes `""`
as one SPACE, FUNCTION LENGTH of a computed zero-length part as the
whole item's, and a zero-length STRING or UNSTRING delimiter as
matching at every position -- docs/oracles.md), 2014/zerolen2 (no
oracle: a written `(3:0)`, national and boolean parts, the class
conditions, the directive OFF). Sixteen bad tests, one per prohibition
below.

How: a literal with nothing between its delimiters is a token of length
zero (tokenizer.h; refused as 2014's under -std=85 and -std=2002), and
flows as any literal does -- `lit_label` of no bytes, a descriptor of
size zero. `>>REF-MOD-ZERO-LENGTH` is read with the directives and left
in the stream for the parser as `>>TURN` is (copy.h, control.h
`apply_turn`), so it is positional: it sets `g_refmod_zero` for the
statements after it. A reference modification parsed while it is on is
marked `rm_zero`; its length, written 0 or computed, then means zero,
not "to the end" (operand.h `parse_ref`, which supplies the omitted
length itself for such a part), and the part goes through its own
runtime entries, `cob_refmod_desc_z`, `cob_refmod_len_z`,
`cob_refmod_len_chk_z` and `cob_bound_refmod_z` (libcob.c), which take
0 as a length and check start within the item and start + length - 1
within it (8.4.3.3.3 rule 5c). Such a part is kept off the in-line
island (lower.h), which assumes a positive length. Everything else is
what a descriptor of size zero does: MOVE from one fills the receiver
with spaces (zeros for a boolean), MOVE to one stores nothing, two of
them compare equal and one compares as spaces against a longer operand,
DISPLAY transfers nothing, INSPECT finds nothing, STRING skips it as a
source, UNSTRING ends at once from it, and a zero-length delimiter is
ignored (the runtime's loops over its length). The class conditions
return false for a size of zero (`cob_class`, `cob_class_bytes`,
`cob_class_2014`, `cob_class_user`).

| rule | paraphrase | disposition |
|---|---|---|
| 8.3.3.4.2 GR 3-5, 8.3.3.5.4 GR 3, .4.4 GR 4, .5.3 GR 4, 7 | `""`, `X""`, `N""`, `NX""`, `B""`, `BX""` are zero-length literals | **test**: 2014/zerolen (`""`), zerolen2 (`N""`, `B""`); **refused** under -std=2002: bad/std2002-zero-literal, bad/empty-literal (-std=85), bad/std2002-empty-boolean |
| 8.3.3.6.3 SR 2 | ALL literal-1 not zero-length | **refused**: "ALL of a zero-length literal" (the figurative-constant sweep) |
| 8.4.3.3.3 GR 5c, 7.3.23.3 GR 1 | length zero allowed only under REF-MOD-ZERO-LENGTH ON; otherwise EC-BOUND-REF-MOD | **test**: 2014/zerolen2 (ON then OFF, the exception under OFF with checking on); **refused**: bad/std2014-refmod-zero-written (a written 0 without the directive), bad/std2002-refmod-zero-directive (the directive is 2023's, taken under -std=2014), bad/std2014-refmod-zero-arg (ON or OFF) |
| 8.4.3.3.3 GR 5b | leftmost-position still 1 to the item's positions | **test**: `x(5:0)` of X(5); `x(6:0)` is refused as before |
| 8.5.4 | a zero-length item: a literal, a part under the directive (items 8-9); an ODO group of zero occurrences, ANY LENGTH, DYNAMIC LENGTH, a zero-length record, a function's zero-length result, a group of only dynamic-capacity tables (items 1-7) | **test**: items 8 and 9 here; a function's result (item 6): TRIM of spaces, zerolen; items 1-5 and 7 are each their own feature's (OCCURS DEPENDING ON of zero: occurs.md; ANY LENGTH: data-division.md; DYNAMIC LENGTH and dynamic-capacity tables: queue items 31-32; a zero-length record: files.md) |
| 8.8.4.2 | two zero-length operands are equal; one against a longer operand is padded | **test**: zerolen, zerolen2 (alphanumeric, national, boolean) |
| 8.8.4.4.4 GR 1 | a class condition of a zero-length item is false | **test**: zerolen2 (ALPHABETIC, NUMERIC, NOT ALPHABETIC-UPPER, BOOLEAN) |
| 14.9.25.4 GR 2-3 | a zero-length alphanumeric or national literal moved is SPACE, a boolean one ZERO; a zero-length receiver is unchanged; a zero-length sending item as the literal | **test**: zerolen, zerolen2; **refused**: bad/std2014-zero-move-numeric (SPACE does not go to a numeric item, 14.9.25.3 rule 5) |
| 14.9.11.4 GR 1 | DISPLAY transfers nothing for a zero-length operand | **test**: zerolen (`"[" "" "]"`) |
| 14.9.1.4 GR 1 | ACCEPT into a zero-length item: the data ignored | **test**: by hand (ACCEPT x(1:n) FROM COMMAND-LINE); the receiver's own size through `accept_desc` (display.h), which also mends ACCEPT into any part -- below |
| 14.9.22.3 SR 3, 14.9.22.4 GR 2 | INSPECT's literals not zero-length; a zero-length inspected item: nothing | **test**: zerolen; **refused**: bad/std2014-zero-inspect |
| 14.9.43.3 SR 3, 14.9.43.4 | STRING's delimiter literal not zero-length; a zero-length source ignored, a zero-length delimiter item as SIZE (14.9.43.4 rule 3c) | **test**: zerolen; **refused**: bad/std2014-zero-delimiter |
| 14.9.48.3 SR 1, 14.9.48.4 GR 2 | UNSTRING's delimiter literals not zero-length; a zero-length sender ends the statement (GR 2), a zero-length delimiter item ignored (GR 9) | **test**: zerolen; **refused**: bad/std2014-zero-unstring |
| 14.9.4.3 SR 2, 14.9.5.3 SR 2 | CALL and CANCEL literal-1 not zero-length | **refused**: bad/std2014-zero-call, -zero-cancel |
| 14.9.42.3 SR 4 (14.9.18.3 SR 8) | STOP RUN WITH ... STATUS literal not zero-length (GOBACK's the same; GOBACK WITH STATUS is not implemented) | **refused**: bad/std2014-zero-stop-status |
| 14.9.37.3 SR 13 | SEARCH ALL's WHEN values not zero-length literals | **refused**: bad/std2014-zero-search |
| 13.16.3 SR 9, 13.15.3 SR 14, 13.17.3 SR 10 | a VALUE literal implies a PICTURE only when it is not zero-length | **refused**: bad/std2014-zero-value-nopic (then rule 8: no PICTURE) |
| 13.18.25.3 SR 5 | a screen FROM literal not zero-length | **refused** by rule |
| 12.4.5.2 SR 4 | ASSIGN TO literal not zero-length | **refused**: bad/std2014-zero-assign |
| 11.5.3, 11.10.3 SR 1, 13.18.22.3 SR 3 | FUNCTION-ID AS, PROGRAM-ID AS, EXTERNAL AS not zero-length | **refused** by rule (before this item) |
| 15.59.3, 15.63.3 SR 3, 15.71.3, 15.72.3 SR 2, 15.66.3 SR 3 | MAX, MIN, ORD-MAX, ORD-MIN and NATIONAL-OF arguments not zero-length literals | **refused**: bad/std2014-zero-max, -zero-national-of |
| 15.19.3, 15.85.3, 15.87.3 | CONVERT, STANDARD-COMPARE, SUBSTITUTE arguments not zero-length | **n/a**: the functions are not implemented (functions.md) |
| 14.9.23.3 SR 2, 17 | INVOKE | **n/a**: object orientation is out of scope |
| 14.9.32.3 SR 4 | RELEASE FROM literal not zero-length | **n/a**: RELEASE FROM takes an identifier here (14.9.32 format: 2023 admits a literal; not implemented) |
| 13.18.62.3 SR 2 | VALIDATE-STATUS literal not zero-length | **n/a**: validation is not implemented |

Found on the way: every ACCEPT ... FROM form passed the whole item's
descriptor with a reference-modified receiver, so `ACCEPT X(2:2) FROM
TIME` wrote four digits from position 2 and FROM COMMAND-LINE the
item's full width. Fixed with `accept_desc` (display.h): the part's
descriptor, as DISPLAY and MOVE have always had it. Test 2002/acceptrm
(GnuCOBOL agrees).

## The leftovers closed with item 10 (2026-10-06)

Each was refused by name until then (docs/refusals.md section 2).
Test 2002/refmodrest (GnuCOBOL 4 agrees but for FUNCTION LENGTH and
BYTE-LENGTH of a part of computed length, which it takes as the whole
item's: docs/oracles.md), 2002/refmodbit, free/posrefmod2 (no oracle:
screens).

| what | disposition |
|---|---|
| LENGTH OF, FUNCTION LENGTH and BYTE-LENGTH of a part of computed length | **test**: the part's length, counted at run time |
| UPPER-CASE, LOWER-CASE, REVERSE, TRIM and the other string functions of such a part | **test**: a result of run-time length, as a function result of run-time length is (the runtime records the length) |
| a function's reference modification with an expression length | **test**: `(i:n + 1)` and `(2:n + 1)` -- the computed route was taken for every non-literal form already; the message named a case that could not arise |
| INITIALIZE of a part with the 2002 phrases | **test**: WITH FILLER, REPLACING, TO DEFAULT: the part as an elementary item of its category with no VALUE clause (14.9.20.4 rules 2-5) |
| MOVE's general rule 1 for a sender of computed length whose length names an item a receiver changes | **test**: the bytes and the length copied first (`move_needs_temp` 3); `MOVE t(n2 + 1:n2) TO r2 grp r1` and `MOVE s(1:k) TO k y` |
| BY CONTENT of a bit item's part | **test**: 2002/refmodbit: the bits moved to a boolean record on a byte boundary, the copy passed; a bit part of computed length is still refused |
| a part of computed length in a screen item (FROM, USING, TO), in a positioned ACCEPT, and under SIZE in a positioned DISPLAY | **test**: free/posrefmod2 |
| a reference-modified numeric receiver of national data | the part is alphanumeric (rule 6), and national to alphanumeric is no MOVE: refused by Table 16, as it was; the "not implemented" message after it could not be reached |
