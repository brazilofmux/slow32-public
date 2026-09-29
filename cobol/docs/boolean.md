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
is one position. An ALL literal takes the length of the operand it
meets (ISSUES-83); it is not both operands of an operation (rule 4), a
shift's first operand (rule 5), or a COMPUTE's whole expression
(14.9.8.3 rule 3).

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

Reference modification of a bit item or bit group counts bits (8.4.3.3.4
rule 5a): its part starts at any bit and may cross a byte, at literal
(ISSUES-82) or computed (ISSUES-84) positions.

A bit item may OCCUR (ISSUES-84): its occurrences follow one another bit
by bit, and a subscript picks bits (i - 1) * bits + 1 onward, found at
run time when the subscript is an item. VALUE applies to each
occurrence. INDEXED BY works on a bit array, so do SET, SEARCH and
index-name subscripts. An element may be reference-modified, its
positions counting bits within the element (8.4.3.3.4 rule 5a; ISSUES-93):
the subscript and the start may each be literal or computed. The
element's start in the array, (i - 1) * bits + start, is worked out on
the numeric stack.

SYNCHRONIZED on a bit item or bit group is the implementor's to place
(8.5.1.6.3). Here the item starts at a byte, and whatever follows it
starts at the next byte (ISSUES-93).

A VALUE on a bit group (GROUP-USAGE BIT) is a boolean literal, ZERO or
ALL B"...". It is laid over the group's bits from the first, aligned
left and zero-filled. Without GROUP-USAGE BIT a group of bit items is an
alphanumeric group (13.18.29.4 rule 3), and a boolean VALUE on it is
refused: it would store the literal's characters, not its bits.

REDEFINES (ISSUES-85) starts at the first bit of the redefined item
(13.18.44.4 rule 1): a bit item over a character item starts at its
first bit, a bit item over a bit item at that item's bit, and a
character item may redefine a bit item that starts a byte.

Not implemented, refused by name: OCCURS DEPENDING ON on a bit array,
OCCURS on a bit group, and a character item redefining a bit item that
starts inside a byte. The ALIGNED clause (13.18.1) is not implemented.

After the Stage B review (ISSUES-94): INITIALIZE sets bit items by MOVE,
so the bits beside them keep their values; boolean relations are EQUAL
and NOT EQUAL only (8.8.4.2.2); an ALL literal beside a run-time length
is repeated at run time; a bit item passed BY REFERENCE starts a byte
(14.9.4.3 rule 6); a group moved to or from a bit group copies bytes
(14.9.25.4 rule 4); a character REDEFINES ends a run of bits.

Refused by the standard, not a gap: bit items in INSPECT, STRING and
UNSTRING, which take items of usage display or national (14.9.22.3
rules 1-2, 14.9.43.3 rule 1, 14.9.48.3 rules 2 and 4) (ISSUES-86).

## Not yet

The items listed under USAGE BIT. (B-NOT of an ALL literal is an ALL
literal with each position inverted, ISSUES-93.)

## Oracle

None. GnuCOBOL 4.0-early-dev, measured on 2002/boolean: it compiles
PICTURE 1 and the literals, but has no simple boolean condition (`IF
f`), no BOOLEAN class test, no INITIALIZE REPLACING BOOLEAN, and
refuses MOVE ALL B"10". Tests are reviewed against the text.
