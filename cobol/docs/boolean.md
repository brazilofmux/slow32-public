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
- USAGE BIT: bits. Not implemented yet; refused by name.

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

## Not yet

Boolean expressions (B-AND, B-OR, B-XOR, B-NOT, the shifts) and COMPUTE
of a boolean; USAGE BIT and GROUP-USAGE BIT; reference modification of
a USAGE NATIONAL boolean or numeric item.

## Oracle

None. GnuCOBOL 4.0-early-dev, measured on 2002/boolean: it compiles
PICTURE 1 and the literals, but has no simple boolean condition (`IF
f`), no BOOLEAN class test, no INITIALIZE REPLACING BOOLEAN, and
refuses MOVE ALL B"10". Tests are reviewed against the text.
