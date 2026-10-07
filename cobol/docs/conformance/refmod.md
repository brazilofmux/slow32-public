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
| 5 | the unique data item: a subset of the item, from leftmost-position for length positions (bits of a bit item), to the end without a length; non-integer, zero or outside: EC-BOUND-REF-MOD | **test**: 2002/ecrefmod, fnrmpast, fnrmzero; free/lenrefmod; **refused** when the literal positions lie outside: bad/std2002-nat-refmod, bad/std2002-bitelem-refmod-past. REF-MOD-ZERO-LENGTH (2023) is standard-queue item 34 |
| 6 | an elementary item without JUSTIFIED, of the item's class and usage: an edited item's part alphanumeric (national), a numeric item's part alphanumeric (national under usage NATIONAL), a bit item's boolean | **test**: 2002/refmodusage; INITIALIZE of a part takes that category (2002/refmodrest); a national sender to a numeric item's part is refused as to any alphanumeric receiver (Table 16) |
| 7 | in a function-identifier, the function's result is the item (the positions of a run-time-length result) | **test**: 2002/refmodrest (`FUNCTION UPPER-CASE (s)(i:n + 1)`), free/fnrefmod, 2002/fnrmpast |

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
