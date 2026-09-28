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

Refused until later parts: STRING, UNSTRING, INSPECT, ACCEPT and
reference modification of national items; numeric and edited national
pictures; national groups (GROUP-USAGE NATIONAL); national fields in
Report Writer and SCREEN SECTION; the NATIONAL-OF, DISPLAY-OF and
CHAR-NATIONAL functions.

## Oracle

None. GnuCOBOL 4.0-early-dev marks its national data unfinished, and
measured: it stores `N"é"` as the two UTF-8 bytes each widened, pads
with single-byte spaces, moves alphanumeric to national without
conversion, and has no DISPLAY-OF. Tests are reviewed against the text,
with IBM's Enterprise COBOL 6.5 Language Reference
(~/Documents/manuals) as a second reading.
