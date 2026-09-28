# Boolean data

COBOL 2002's BOOLEAN module, landing in parts (docs/standards.md, Stage B).
Citations are to ISO/IEC 1989:2023.

## Representation

A boolean item is PICTURE 1, one boolean position per symbol. Its usage
decides the storage (Annex D.10):

- USAGE DISPLAY (the default): one character a position, `0` or `1`.
- USAGE NATIONAL: one national character a position, U+0030 or U+0031,
  as numeric USAGE NATIONAL is its DISPLAY form widened
  (docs/national.md).
- USAGE BIT: bits, packed (ISSUES-78, below).

Literals: `B"0101"`, and `BX"5"`, each hexadecimal digit four positions
(8.3.3.4). The compiler holds both as characters 0 and 1.

## What part one covers (ISSUES-76)

- VALUE: a boolean literal or ZERO, aligned left and zero-filled.
- MOVE, by the 14.9.25 table: a boolean receiver takes a boolean,
  alphanumeric or national sender, aligned left and zero-filled or
  truncated on the right (14.6.8.6); a boolean sender goes to an
  alphanumeric, national or group receiver as its characters. Numeric,
  numeric-edited and alphabetic items are no boolean's partners either
  way; a figurative constant other than ZERO is no boolean value (rule
  7). Characters that are not 0 or 1, moved from alphanumeric or
  national data, stay as they are and fail the BOOLEAN class test (the
  text calls such content incompatible data, 14.6.13.2).
- Comparison: boolean with boolean only, the shorter operand extended
  on the right with zeros (8.8.4.2.8).
- The simple boolean condition `IF flag`, for one boolean position
  (8.8.4.3), and `NOT flag`.
- The class test `IS BOOLEAN` (8.8.4.4).
- INITIALIZE: boolean zeros, and REPLACING BOOLEAN DATA BY.
- Reference modification (the part is boolean), LENGTH in positions.
- FUNCTION BOOLEAN-OF-INTEGER (15.13) and INTEGER-OF-BOOLEAN (15.45).
  BOOLEAN-OF-INTEGER with a length item returns a result whose length
  is known at run time (the ISSUES-64 machinery).

## Expressions (ISSUES-77)

`B-NOT`, `B-AND`, `B-XOR`, `B-OR` and the shifts `B-SHIFT-L`, `-R`,
`-LC`, `-RC` (8.8.2), in COMPUTE's format 2 (14.9.8) and in conditions.
Precedence is B-NOT, B-AND, B-XOR, B-OR, left to right; a shift takes
the precedence of the operation before it, or B-AND's (rule 7b), and its
count is an integer literal or item. A binary operation extends the
shorter operand with zeros on the right; a shift keeps its operand's
length (rules 8, 9). COMPUTE stores the value in each receiver by the
MOVE rules. In a condition an expression compares with a boolean
operand, and alone it is a simple boolean condition when every operand
is one position. ALL literal operands are not implemented.

## USAGE BIT and bit groups (ISSUES-78)

Bit items that follow one another at a level take the next bit position;
any other item, and a bit item after one, starts on the next byte, the
unused bits of the last byte being implicit filler (8.5.1.6.3). A level
01 or 77 bit item starts on a byte. Within a byte the first bit is the
most significant. `GROUP-USAGE BIT` makes a group one boolean item of
all its bits; its subordinate groups are bit groups and its elementary
items USAGE BIT, by implication or written (13.18.29.3 rule 2).

A bit item's descriptor holds its bit count and its first bit's place,
and libcob reads it as the DISPLAY form and writes back only its own
bits, so every boolean operation takes bits as it takes characters.

Not implemented, refused by name: OCCURS, REDEFINES, SYNCHRONIZED and
reference modification of a bit item or bit group; a VALUE on a bit
group; bit items in INSPECT, STRING and UNSTRING, whose runtimes work on
characters.

## Not yet

Reference modification of a USAGE NATIONAL or USAGE BIT item; ALL in a
boolean expression; the items listed under USAGE BIT.

## Oracle

None. GnuCOBOL 4.0-early-dev, measured on 2002/boolean: it compiles
PICTURE 1 and the literals, but has no simple boolean condition (`IF
f`), no BOOLEAN class test, no INITIALIZE REPLACING BOOLEAN, and
refuses MOVE ALL B"10". Tests are reviewed against the text.
