# National data

COBOL 2002's national category, under `-std=2002` (docs/standards.md,
Stage B). Parts landed so far are listed in ISSUES.md 62 onward.

## Representation (the user's rulings, 2026-09-28)

- **A national character is one UTF-16 code unit, stored big-endian**,
  two bytes. The text makes a national character position one UTF-16
  code unit and gives surrogate pairs and composite sequences no
  special handling (2023 8.5.1.4, Limitations of character handling);
  the byte order is the implementor's.
  Big-endian is IBM Enterprise COBOL's, so national data from z/OS
  reads unchanged, and a dump reads in character order.
- **Alphanumeric text is UTF-8 when it becomes national** -- in a MOVE,
  a comparison, NATIONAL-OF. Source files, the terminal and modern data
  are UTF-8; the text allows this as "mixed alphanumeric and national
  data". DISPLAY of a national item or literal writes UTF-8.
- **A byte that is not UTF-8 is malformed data, not another encoding**
  (ruled 2026-09-28, ISSUES-63; a first cut read it as Latin-1, and a
  byte string cannot be both). It becomes U+FFFD, and with checking on,
  the MOVE raises EC-DATA-CONVERSION (14.9.25 general rule 6). A national
  literal in the source must be UTF-8 text; anything else is a compile
  error. Legacy 8-bit or EBCDIC data is converted once, explicitly, where
  it comes in -- never guessed at byte by byte.

**The principle** (the user's framing): these are the choices of an
implementation on the non-IBM side of the fence. IBM keeps alphanumeric
in a single-byte code page and adds UTF-8 as a separate usage (`USAGE
UTF-8`, `U"..."` literals, Enterprise COBOL 6.x); here the source,
the terminal and alphanumeric data are UTF-8, and national data is
IBM's UTF-16BE so it interchanges unchanged. Surrogates follow the text
and IBM alike: a character above U+FFFF is a surrogate pair, two PIC N
positions, four bytes, and not splitting one is the programmer's care.

## What part one covers (ISSUES-62)

`PICTURE N`, `USAGE NATIONAL` with it, national literals `N"..."` (the
source's UTF-8) and `NX"..."` (hexadecimal code units), VALUE (a
national literal or a figurative constant only, 13.18.63 syntax rule
5), MOVE into a national item from national, alphanumeric, numeric and
figurative sources (JUSTIFIED, ALL), comparisons of national operands
with anything (a figurative beside a national operand is national:
HIGH-VALUE is U+FFFF), FUNCTION LENGTH in characters and BYTE-LENGTH in
bytes, INITIALIZE with REPLACING NATIONAL.

A national item is not MOVEd to an alphanumeric or numeric one (14.9.25;
DISPLAY-OF converts); a group receives its bytes (general rule 4).

Refused until later parts: ACCEPT of a national item at a screen
position; numeric and edited national pictures; GROUP-USAGE BIT (the
BOOLEAN module); national fields in Report Writer and SCREEN SECTION.

## What part two covers (ISSUES-64)

FUNCTION NATIONAL-OF (15.66), DISPLAY-OF (15.26) and CHAR-NATIONAL
(15.16). NATIONAL-OF decodes its alphanumeric argument as UTF-8,
DISPLAY-OF encodes its national argument as UTF-8, and a surrogate pair
becomes one four-byte sequence and back. What does not convert -- a
byte that begins no UTF-8 sequence, a lone surrogate -- becomes the
substitution character (argument-2: one national character for
NATIONAL-OF, one alphanumeric character for DISPLAY-OF), or U+FFFD
without one; then, with EC-DATA-CONVERSION checked, the statement
raises it when it completes (15.66.4 rule 3, 15.26.4 rule 3), not in
the middle of its operands. CHAR-NATIONAL(k) is code unit k-1, as
CHAR(k) is byte k-1.

The result of NATIONAL-OF and DISPLAY-OF is as long as the conversion
makes it, known only at run time: `café` is five bytes and four
national characters. libcob keeps the length of the last one evaluated,
and the compiler takes it from there -- for the source of a MOVE or a
comparison, a DISPLAY, and FUNCTION LENGTH (characters) and BYTE-LENGTH
(bytes). The compile-time bound is 2 bytes per argument byte
(NATIONAL-OF) or 3 per national character (DISPLAY-OF); a result that
could exceed 8190 bytes is refused. Reference modification of such a
result is refused for now.

## Reference modification (ISSUES-67)

`n(start:length)` on a national item counts character positions, two
bytes each, and the part is national (2023 8.4.2.4). Literal positions
become byte offsets at compile time; computed ones are doubled where
they are evaluated. The runtime's descriptor for a computed part reads
the item's category and counts in characters. EC-BOUND-REF-MOD checks
character positions, and so do the compile-time range checks: `n(4:2)`
on a PIC N(4) is refused, though its eight bytes would hold two more.
A position is a code unit, so a part can split a surrogate pair, as the
text's one-position-per-code-unit rule implies.

## INSPECT (ISSUES-68)

INSPECT of a national item (2023 14.9.22) scans character positions,
two bytes each: TALLYING counts characters, CHARACTERS takes one at a
time, and a pattern matches only at a character boundary, so no match
straddles two characters. Every operand is national (syntax rule 4):
N literals, national items, and figurative constants, each one
national character (rule 3). An alphanumeric operand beside a national
item is refused, as is a national operand beside an item that is not
national. The runtime takes the character width from the inspected
item's descriptor; a reference-modified national item passes a
national one.

## STRING and UNSTRING (ISSUES-69)

A national STRING receiver, or UNSTRING source, makes the statement
national: every operand is national (2023 14.9.43.3 rule 1, 14.9.48.3
rule 3), a figurative constant is one national character, and
positions are characters -- POINTER, COUNT IN and TALLYING IN count
them, and a delimiter matches only at a character boundary. A numeric
UNSTRING receiver of national data would have to be USAGE NATIONAL
(rule 4), which is not implemented, so it is refused.

## ACCEPT (ISSUES-70)

ACCEPT into a national item -- from standard input, the console, the
command line, an argument, DATE, DAY, TIME or DAY-OF-WEEK -- moves the
text as UTF-8, so the item is truncated or padded by character. A byte
that begins no UTF-8 character becomes U+FFFD and, with checking on,
EC-DATA-CONVERSION, as for a MOVE. At end of input the item is left as
it was, and nothing is raised.

## National groups (ISSUES-71)

`GROUP-USAGE IS NATIONAL` (2023 13.18.29) makes a group a national
group: treated as one national item described PICTURE N(m), for MOVE,
comparison, DISPLAY, LENGTH, reference modification, INSPECT, STRING and
UNSTRING, and with a national literal as its VALUE. INITIALIZE and MOVE
CORRESPONDING process it as a group (14.9.20.4 rule 1, the MOVE
statement's note 5); an alphanumeric group receives its bytes. Its
subordinate groups are national groups, and every elementary item under
it must be PICTURE N; a signed numeric one would need SIGN SEPARATE and
USAGE NATIONAL, which waits for numeric national. The subject is a
group, with no USAGE clause of its own (syntax rules 1 and 3).

## Case (ISSUES-66)

UPPER-CASE and LOWER-CASE take national arguments and return national
results (15.78, 15.52), and treat alphanumeric text as UTF-8. The
mappings are Unicode's simple, one-to-one ones from UnicodeData.txt,
which is what 2002 Annex D note 1 advises. With no locale the result
has the argument's length (E.13.2.4), so there is no full case mapping:
sharp s stays, final sigma becomes capital sigma, and U+0130 becomes i.
A supplementary letter (a surrogate pair) maps as one character and
stays two positions. In alphanumeric text, a letter is mapped only when
its other case takes the same number of UTF-8 bytes (e-acute does;
dotless i, two bytes against I's one, does not), and a byte that begins
no UTF-8 character is left alone. For ASCII text nothing changes.

## Oracle

None. GnuCOBOL 4.0-early-dev marks its national data unfinished, and
measured: it stores `N"é"` as the two UTF-8 bytes each widened, pads
with single-byte spaces, moves alphanumeric to national without
conversion, and has no DISPLAY-OF. Tests are reviewed against the text,
with IBM's Enterprise COBOL 6.5 Language Reference
(~/Documents/manuals) as a second reading.
