# 14.9.25 MOVE statement

Swept 2026-09-29 (ISSUES-96). X3.23-1985: VI-103 (MOVE, formats 1-2;
syntax rules 1-4, general rules 1-6 with the table of legal moves in
general rule 3). CCVS-85 exercises MOVE throughout, NC101A-NC105A and
NC114M-NC124A at length -- all 348 programs match GnuCOBOL's tally.

Table 16 was checked cell by cell: a program for each of the 90
sender/receiver category pairs, compiled under -std=2002, compared
with the table. Eleven cells the table forbids were accepted before
this sweep; all 90 now agree.

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | no index, message-tag, object or pointer operands (85 rule 4: no index data item) | **refused**: bad/move-index, bad/std2002-move-pointer -- both were accepted before this sweep. Message-tag and object: **n/a** (object orientation is out of scope, docs/refusals.md) |
| 2 | a strongly-typed group receives only a group of its own type | **refused**: bad/std2002-strong-move-group, -strong-move-other |
| 3, 4 | the sending and receiving operands | definitions |
| 5 | no alphanumeric figurative constant to a numeric or numeric-edited item, except ALL digits (or a digit symbolic character) to an integer, an obsolete feature | **refused** under -std=2002: bad/std2002-move-highvalue-num, -move-all-edited; the exception: 2002/movecorr -- none checked before this sweep. Under -std=85 only SPACE is refused (85 general rule 3a; the others are alphanumeric, which 85 allows): bad/move-space-to-num |
| 6 | ZERO not to an alphabetic item (85 general rule 3b) | **refused**: bad/move-zero-to-alpha -- accepted before this sweep |
| 7 | a figurative constant that is not boolean not to a boolean item | **refused**: bad/std2002-bool-space |
| 8 | a binary-char, -short, -long or -double sender goes only to a numeric or numeric-edited item | **refused**: bad/std2002-move-binchar-alnum -- accepted before this sweep. BINARY-DOUBLE likewise (2002/bindouble; implemented in ISSUES-117) |
| 9 | variable-length groups must be compatible (8.5.1.12) | **n/a** for -std=2002: variable-length groups and dynamic-capacity tables are COBOL 2014's. A group over an OCCURS DEPENDING ON table is moved by the 85 rules: free/odomove |
| 10 | Table 16 decides every other move | **refused**: all 90 cells agree. New refusals: bad/move-alpha-to-num (alphabetic, and likewise alphanumeric-edited, to numeric or numeric-edited: 85 rule 3a), bad/move-edited-to-alpha (numeric-edited, integer or noninteger to alphabetic: 85 rule 3b), bad/std2002-move-nonint-alnum, bad/std2002-move-natedited-num. Already refused: bad/std2002-nat-to-alnum, -nat-noninteger, -bool-to-num, -bool-from-num |
| 85 3c | a noninteger numeric item to an alphanumeric one (2023 Table 16 the same) | **ruling**: accepted under -std=85, because NIST NC105A, NC114M and NC124A require it (the user's ruling of 2026-08-31: the NIST cases outrank the text; free/numalnum). Refused under -std=2002 by Table 16. A noninteger *literal* is refused in both |
| 85 3d | at level 1 a numeric-edited item is not moved to a numeric item | **n/a**: this compiler is level 2; de-editing is general rule 5 |
| 11 | CORR is CORRESPONDING | **test**: 2002/movecorr |
| 12 | CORRESPONDING operands are groups, not reference-modified | **refused**: "CORRESPONDING: no reference modification on a group" |
| 13 | the corresponding items, by 14.7.6 | see general rule 11 |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | each receiver in order, identified just before its move; the sender (its subscripts, reference modifier, length, or function) identified once, before the first | **test**: free/moveonce -- a subscript, a reference modifier's start, an OCCURS DEPENDING ON group's length and a function were all evaluated again for each receiver before this sweep, so `MOVE te (b) TO b, ce (b)` stored the wrong element. The compiler now copies the sender (or the DEPENDING ON item) first when a receiver ahead of the last shares storage with what identifies it, and evaluates a function once. A reference modifier whose *length* is an expression over such an item is **gap**, refused by name: bad/move-refmod-len-recv. GnuCOBOL gets the ODO case wrong (docs/oracles.md) |
| 2 | a zero-length alphanumeric or national literal is SPACE | **n/a**: zero-length literals are COBOL 2014's. 85 has 1 to 160 characters and 2002 more than zero (8.3.1.2.1.2 rule 1); both were accepted before this sweep and are now **refused**: bad/empty-literal, bad/std2002-empty-boolean |
| 3 | a zero-length boolean literal is ZERO | **n/a**, as rule 2 |
| 4 | an elementary move; a bit group or national group counts as elementary; any other move is alphanumeric, no conversion | **test**: free/moverules (a group to a numeric item), 2002/natgroup, 2002/boolbit, CCVS NC105A |
| 5 | de-editing only from a numeric-edited sender to a numeric or numeric-edited receiver | **test**: free/moverules |
| 6 | conversion and editing; the national conversion and EC-DATA-CONVERSION | **test**: 2002/natconv, national |
| 6a | an alphanumeric-class receiver: alignment, padding; the operational sign not moved, a separate one not counted; P positions count as zeros | **test**: free/moverules (signed, separate), free/picmix (P) |
| 6b | the same item as sender and receiver: undefined when edited; a variable-length one through a temporary | **n/a**: undefined; variable-length items are 2014's |
| 6c | the same usage: the bytes unchanged (and endianness) | **test**: free/identmove. Endianness phrases are 2014's: **n/a** |
| 6d1 | a numeric-edited sender de-edited; invalid numeric content sets EC-DATA-INCOMPATIBLE | de-editing **test**: free/moverules. EC-DATA-INCOMPATIBLE is **gap**: it is in the exception table but no statement raises it yet (docs/refusals.md, the rest of Table 13) |
| 6d2 | a numeric value: the sign kept for a signed receiver, the absolute value for an unsigned one; float usages | **test**: CCVS NC1xx, free/moverules. Float: **n/a** until -std=2014 (the ruling of 2026-09-28) |
| 6d3 | an alphanumeric or national sender as an unsigned integer: its digits (the rightmost 31), a figurative constant replicated to the receiver's digits, a literal's characters (rightmost 31) | **test**: free/moverules (rightmost digits, ALL to an integer). Past 18 digits the receiver takes the rightmost integer digits it holds (docs/wide.md phase 1; 2002/wide1) |
| 6d4 | floating-point receivers | **n/a** until -std=2014 |
| 6d5 | a value too far from zero for the receiver's usage is EC-DATA-OVERFLOW; too near is zero | **n/a**: those are the floating-point usages' |
| 7 | the category of literals and figurative constants (Table 17) | **test**: free/moverules (ZERO to numeric-edited), 2002/natconv, 2002/boolean |
| 8 | dynamic-length items | **n/a**: 2014's |
| 9 | variable-length groups | **n/a**: 2014's (syntax rule 9) |
| 10 | overlapping operands (14.6.10) | **n/a**: undefined |
| 11 | CORRESPONDING by 14.7.6; subscripts evaluated at the start | **test**: 2002/movecorr. 14.7.6 rule 2 (a pair whose MOVE would be invalid does not correspond; 85 6.4.3 rule 2 the same) and rule 4 (index and pointer items do not correspond) were not honoured before this sweep: an invalid pair was moved, and after this sweep's refusals would have stopped the compile. GnuCOBOL refuses such a statement (docs/oracles.md) |

## Found by this sweep

Syntax rules 1, 5, 6, 8 and 10 (eleven Table 16 cells) were not
enforced. General rule 1 was not honoured: the sender was identified
again for each receiver. CORRESPONDING moved pairs that do not
correspond. Zero-length literals, 2014's, were accepted in both
editions. Numeric items of 19 to 31 digits under -std=2002 were refused
with the 1985 limit's message; they are a gap now named.

The sweep also turned up a use-after-free in the compiler: a record
made in the PROCEDURE DIVISION (a function's result, a BY CONTENT copy)
could grow the symbol table under the Sym pointers a statement held
(ISSUES-96). The table no longer moves.

CCVS-85, the Open Systems suite (229 programs byte-identical under
-std=85) and majesty are unaffected.
