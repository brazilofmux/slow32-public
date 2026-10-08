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

Refused: GROUP-USAGE BIT (the BOOLEAN module).

## National text in columns (ISSUES-92)

A report line and a screen are character cells, and national text goes
into them by display width, as a terminal shows it. The text is split
into grapheme clusters (UAX #29: a letter and its combining marks, an
emoji ZWJ sequence, a flag's two regional indicators), and each cluster
takes its display width in columns: two for an East Asian wide or
full-width character or an emoji, one for most, and a cluster carrying
U+FE0F or a flag is two (tinymux's policy). A mark with nothing before
it sits over a space. The model is `common/s32utf.h`, with libutf's
Unicode 16.0 tables, shared by the term service, the runtime and the
compiler, so a screen, a report line and the compiler agree (ISSUES-94).
A cluster is a span of the item's own code units, so laying text out or
editing it on a screen never drops any of it.

- **A field of n national character positions is n columns.** Its text
  is laid out left to right. A character that would cross the field's
  last column is dropped with everything after it, and spaces stand
  instead. So what follows the field lands in the same column whatever
  the text: PIC N(6) holds three CJK characters, or six Latin ones.
- **A national VALUE with no PICTURE** in a report takes as many
  positions as it has code units or columns, whichever is more, so all
  of it shows.
- **Positions still count code units.** LENGTH, reference modification
  and CHAR/ORD are unchanged. Only the laying out is visual.
- **Report Writer:** PIC N and national-edited fields, USAGE NATIONAL on
  a numeric or numeric-edited PICTURE, a national VALUE, an alphanumeric
  SOURCE into a national field, and a numeric USAGE NATIONAL SOURCE.
- **SCREEN SECTION:** PIC N fields for FROM, TO and USING, and a
  national VALUE. The same holds for positioned DISPLAY of a national
  item or literal and positioned ACCEPT of a national item. Input is
  UTF-8, edited a cluster at a time: a combining mark joins the
  character before the cursor. A character that would take more columns
  than the field has, or more code units than the item has, is refused
  with the field's beep.
- **Refused, as MOVE refuses them (14.9.25):** a national SOURCE, FROM
  or VALUE into a field that is not national, and a national field's
  input into an item that is not national.

Once a program has painted with positioned DISPLAY/ACCEPT, the plain
DISPLAY that follows moves its column by display width too.
Alphanumeric fields are unchanged: an alphanumeric position is a byte,
so a PIC X field of UTF-8 text takes as many positions as it has bytes,
and shows narrower than that on the terminal.

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

## Numeric USAGE NATIONAL (ISSUES-72)

A numeric or numeric-edited picture with USAGE NATIONAL (2023 13.18.60
rule 12) -- written on the item, on a group above it, or implied by a
national group -- is stored as its DISPLAY form with each character one
UTF-16BE code unit: digits U+0030..U+0039, a separate sign U+002B or
U+002D. The text leaves an unseparated sign to the implementor
(13.18.52.4 rule 4), and here it is the DISPLAY overpunch widened
(`p`..`y` for a negative last digit), so the two usages differ only in
width. libcob narrows such an operand to a scratch DISPLAY copy for
every numeric primitive (reading, arithmetic, comparison, class tests,
editing, DISPLAY) and widens a receiver back, so everything DISPLAY
numeric does, national numeric does. LENGTH counts characters, and
INSPECT scans them.

The MOVE table (14.9.25) is by category: a national sender to a numeric
or numeric-edited receiver is valid, and a numeric noninteger to a
national receiver is not. A signed numeric item in a national group
needs SIGN SEPARATE (13.18.29.3 rule 3); A and X pictures take no
USAGE NATIONAL (rule 12).

## National-edited (ISSUES-73)

A picture of N with the insertion symbols B, 0 and / (2023 13.18.40) is
national-edited: class national, so it compares, displays, is
inspected and sends as its characters, insertions included. As a MOVE
receiver its N positions are filled left to right, and B, 0 and / put a
national space, zero and stroke in theirs. A figurative constant or ALL
literal is expanded to the item's length and edited too. It is not a
STRING or UNSTRING receiver (14.9.43.3 rule 5, 14.9.48.3 rule 4).

## Files (ISSUES-74)

A record sequential, relative or indexed file holds a national
record's bytes, UTF-16 big-endian -- the interchange form -- and a
national RECORD KEY orders by code unit, which is its byte order.

A line sequential file of national records is UTF-8 text, as every
text file here is. The text leaves the character set of a line
sequential file to the implementor (2023 12.4.5.10 general rule 2) and allows
for national records (14.9.30 rule 15, 14.9.51 rules 21-23):

- WRITE encodes the record as UTF-8, trailing national spaces dropped.
  A lone surrogate has no UTF-8 form: the WRITE fails with status 71.
- READ decodes a line into national characters, padded with national
  spaces. A byte that begins no UTF-8 character becomes U+FFFD and the
  status is 09; a line of more characters than the record holds is
  truncated, 04.

UTF-16 in a text file would also carry a 0A byte inside a character
(U+0A00, U+010A ...), which a line reader takes for the end of the
line. A file's records are all national or all alphanumeric; a mix is
refused. An alphanumeric record holding national fields is bytes, as
any alphanumeric record is.

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

## Audit (2026-10-08)

The user's concern, after the standard queue: national is easy to say
and hard to get right -- the intersection of display width, UTF-8 code
points, UTF-16 code units with surrogates, COBOL's own size, space and
length, and truncation that does not break the Unicode. libutf is
trusted; the suspects are our use of it and the UTF-16BE path.

The audit is `tests/gen/gen-national.py` with `tests/gen/run-ref.sh`
(tests/gen/README.md): a generator whose character pool spans every
intersection, with the text's code-unit model written out beside it as
the reference (no oracle has UTF-16 national data). Some 400 programs of 40
to 60 statements agreed with the model byte for byte, over 400 of their
reference lines parting a surrogate pair. Found, and fixed the same day:

- **ORD, REVERSE, NUMVAL, NUMVAL-C, NUMVAL-F, TEST-NUMVAL(-C, -F) on a
  national argument.** The 1989 table's functions accepted a national
  argument (15.3 rule 2 admits one) and read its bytes: ORD gave the
  high byte plus one, REVERSE reversed the bytes and called the result
  alphanumeric, the NUMVAL family read zero from the UTF-16 digits and
  TEST-NUMVAL said 1. Now ORD is the code unit plus one (the national
  collating sequence, 15.70), REVERSE returns national (15.79), and the
  NUMVAL family reads the text narrowed a character to a byte, so a
  position it reports is the same position. Test: 2014/natfuncs2.
- **REVERSE keeps a surrogate pair in its order.** The text counts the
  pair as two positions; reversing them would make two lone surrogates
  of one character, and no conforming program can tell the two readings
  apart except by taking the result to pieces. The generator's model
  says the same.

Confirmed right, by the generator or by hand: truncation and padding by
positions in MOVE, STRING, UNSTRING and ACCEPT; reference modification
of a lone surrogate (U+FFFD on DISPLAY); UPPER-CASE and LOWER-CASE of a
supplementary letter (Deseret) as one character; TRIM to a zero-length
result; the report column rule with a wide character that would cross
the field's last column, and with a parted pair (U+FFFD, one column);
line sequential WRITE of a parted pair (status 71) and READ of a
supplementary character; record sequential round trip.

Left as the text has it, the user's to change: **truncation by code
unit parts a surrogate pair** (8.5.1.4: a position is a code unit, and
MOVE truncates by positions). `MOVE N"ab😀" TO PIC N(3)` leaves `ab` and
a lone high surrogate, which displays as U+FFFD and which a line
sequential WRITE refuses. Dropping the whole pair instead would keep
the Unicode whole at the cost of a position of space, and would be a
documented deviation. Until that ruling, the generator counts these
cases and the runtime does what the text says.

## Oracle

None. GnuCOBOL 4.0-early-dev marks its national data unfinished, and
measured: it stores `N"é"` as the two UTF-8 bytes each widened, pads
with single-byte spaces, moves alphanumeric to national without
conversion, and has no DISPLAY-OF. Tests are reviewed against the text,
with IBM's Enterprise COBOL 6.5 Language Reference
(~/Documents/manuals) as a second reading.
