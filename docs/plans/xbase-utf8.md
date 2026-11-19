# xBase and UTF-8

Status: built, 2026-11-19 -- every step below.  Collation by language
(libutf's UCA) is the one piece left, and only if wanted.

xbase (and xbc, whose programs link the interpreter's sources as
`libxbase.s32a`, so one change serves both) treats text as bytes, one
byte to a character, as dBASE III, Clipper 5.2 and FoxPro 2.6 did.  This
page is what it would take to let a program work in UTF-8, the way
Harbour does, without disturbing the byte-exact behaviour the oracles
hold it to.

## What happens today

A run of the interpreter under `slow32-dbt` with a terminal, 2026-11-19:

    @ 1,1 TO 3,8          ->  DA C4 C4 C4 C4 C4 C4 BF ...   (CP437 bytes, raw)
    @ 5,1 SAY "café"      ->  63 61 66 C3 A9                 (UTF-8 through)
    ? LEN("café")         ->  5
    ? UPPER("café")       ->  CAFé

The emulator's terminal service already reads its output as UTF-8
(`common/s32utf.h`: clusters, double-width cells, U+FFFD for a byte that
is not UTF-8), and so does every terminal it runs in.  So a box drawn
with `@ ... TO`, a frame from `DISPBOX`, `CHR(196)`, or any text from an
old table with accented letters arrives as invalid UTF-8 and is shown as
U+FFFD.  Box drawing on a terminal is wrong now, before any of the rest
of this page.

## The model: Harbour's

- Text stays bytes.  A setting names the codepage the program's strings
  are in: CP437 by default (what the interpreter means today), or UTF-8.
- With UTF-8, the string functions count characters (code points, as
  Harbour's `UTF8EX` does), and byte versions stay available.
- The terminal is always UTF-8.  The codepage is translated at the
  terminal's edge, both ways: output into UTF-8, keys back into the
  program's codepage.
- A table carries its own codepage; the RDD translates between it and
  the program's on read and write.
- Off by default, so dBASE III PLUS, FoxPro 2.6 and Harbour's byte mode
  keep holding the interpreter to what they print, byte for byte.

## What the survey found, by area

**Terminal edge** (`screen.c`: `term_puts`, `term_putc`, the shadow
screen; `read_dbase_key`).  Every character reaches the terminal through
two functions, and every key arrives through one.  The shadow screen
(SAVESCREEN / RESTSCREEN) is one byte per cell plus an attribute, which
is Clipper's screen-string format.  Translating at `term_puts` /
`term_putc` leaves the shadow, the screen strings and everything above
them as they are.  Keys: `getsys.c:982` and `memoedit.c:477` take any key
32-255 as one character, so a typed `é` (C3 A9) is two characters today.

**String functions** (`func.c`, `clipfn.c`).  Those that count or cut:
LEN, SUBSTR, LEFT, RIGHT, AT, RAT, STUFF, PADL / PADR / PADC, TRIM /
RTRIM / LTRIM / ALLTRIM, REPLICATE, SPACE, STRTRAN, CHRTRAN, OCCURS,
TRANSFORM, MEMOLINE / MLCOUNT / MLPOS / MLCTOPOS / MPOSTOLC, WORD.  Case
and class: UPPER, LOWER, PROPER, ISALPHA, ISUPPER, ISLOWER.  (The
survey found that `=`, `==`, `<`, `>`, ASCAN, ASORT and LOOKUP() ignored
case, which dBASE, FoxPro and Clipper do not; they compare bytes now,
xbase ISSUES, 2026-11-19.  UTF-8 in byte order is code-point order, so
comparison needs nothing more under the setting.)  CHR
and ASC need a ruling (below).  SOUNDEX and DIFFERENCE are ASCII
algorithms and stay so.

**Screen layout** (`screen.c`, `getsys.c` 2,316 lines, `memoedit.c`,
`browse.c`, `menu.c`, `report.c`, `label.c`).  Columns are counted as
`strlen` throughout (about 90 calls across those files).  With UTF-8 a
string's width is its display width: a CJK character takes two columns,
a combining mark none.  The GET editor's cursor and picture positions
are byte offsets into the buffer; they become character offsets, and
the picture template's positions character positions.

**Tables** (`dbf.c`, `field.c`).  `dbf_set_field_raw` cuts a value at
the field's length in bytes (`dbf.c:553`), which can split a UTF-8
character.  Field lengths stay bytes, as in Harbour; the cut moves back
to a character boundary and the rest is padded with spaces.  The header's
codepage byte (offset 29, the language driver id FoxPro 2.5 and dBASE IV
write: 0x01 CP437, 0x02 CP850, 0x03 Windows 1252) is neither read nor
written.

**Indexes** (`index.c`).  Keys compare with `memcmp`.  UTF-8 in byte
order is code-point order, so a UTF-8 index sorts consistently as it is
and NDX stays byte-for-byte with dBASE.  The one defect is the same as
in tables: a key cut at `key_len` can split a character.  Language-aware
order is libutf's collation (53 CLDR locales) as an option for later;
an index built that way would have to say so in its header.

**Lexer** (`lex.c`).  Identifiers stay ASCII, as in Clipper.  String
literals and comments pass through as bytes, which is all UTF-8 needs.

## What exists to build it from

- **libutf** (`~/utf`, vendored the way `runtime/sync-libutf.sh` and
  `cobol/libcob/sync-libutf.sh` do it): `co_left`, `co_right`, `co_mid`,
  `co_pos`, `co_lpos`, `co_trim`, `co_center`, `co_ljust`, `co_rjust`,
  `co_repeat`, `co_reverse`, `co_visible_length`; the `tr_toupper` /
  `tr_tolower` / `tr_totitle` tables; `co_console_width`,
  `co_visual_width`, `co_cluster_width`; `utf_grapheme_next`; NFC;
  collation; and UTF-8 to CP437 / Latin-1 / Latin-2 / ASCII
  approximation, for writing into an old codepage.  The `co_*` functions
  treat TinyMUX's colour code points (in the Private Use Area) as having
  no width; whether that wants a switch for xbase is the first thing to
  look at.
- **TinyMUX** (`mux/include/stringutil.h`): `cp437_utf8[256]` and
  `latin1_utf8[256]`, the exact reading direction, and `utf8_FirstByte`
  with the backing-up-to-a-boundary logic a field cut needs.
- **tapemgr** (`tools/mktables.py`): a mapping file of one line per byte
  through libutf's DFA builder (`~/utf/gen/integers.cpp`) gives both
  directions for any single-byte codepage, with a round-trip check.
  CP850 (most of the non-US dBASE and Clipper world) is not in libutf
  or TinyMUX; it is one mapping file this way, and belongs in `~/utf/gen`
  beside CP437.

## Oracles

- **Harbour**, `slow32:harbour-oracle`: probes that select `UTF8EX`
  show one worked answer for the string functions and the screen.  It is
  a convenience, not the authority: the Clipper guides are silent on
  UTF-8, so where Harbour's answer is wrong (by Unicode, or by what the
  byte-mode products did) the ruling is ours and is written down here.
- **FoxPro 2.6 for Windows** (`xbase/tests/foxpro/foxrun.py`): FoxPro 2.5
  brought codepage marks in the table header, CPCURRENT(), CPDBF() and
  CPCONVERT().  It answers what a table's codepage byte means and what a
  translation on read or write produces.  VFP6 the same, later.
- dBASE III PLUS, as now, for everything with the setting off.

## Rulings (user, 2026-11-19)

1. **The setting.**  `SET CODEPAGE TO "UTF8"` (and `"CP437"`), which can
   change mid-run, readable as `SET(_SET_CODEPAGE)` in xbc, with an
   environment default; FoxPro's names for asking: CPCURRENT(), CPDBF().
2. **A write a table's codepage cannot hold** is approximated quietly
   (libutf's tables: a curly quote becomes a straight one), as FoxPro's
   conversions never stopped on a character.
3. **CHR() and ASC() in UTF-8 mode** are code points: CHR(233) is e-acute.
   (Bytes would make broken strings; CP437 numbering keeps a DOS habit in
   the mode meant to leave it.  Old programs stay in CP437 mode.)
4. **What counts as a character**, by job: code points for the string
   functions, grapheme clusters for the GET cursor, display columns for
   layout.
5. **SAVESCREEN in UTF-8 mode** is fixed-size wider cells: a code point and
   the attribute at a known offset.
6. **Source files** are judged per file: valid UTF-8 is UTF-8, anything
   else CP437.  A literal means the characters the file holds.
7. **A new table in UTF-8 mode** leaves the header's codepage byte 0 ("not
   stated") and is taken as UTF-8 by a program in UTF-8 mode.  FoxPro 2.6
   does the same with an unmarked table: read raw, in its own codepage
   (checked 2026-11-19, step 4).

## Order of work

0. **The terminal edge -- built 2026-11-19.**  Not in xbase's ~400
   console writes but in the machine: the term service has a charset
   (version 4, SET_CHARSET; docs/SPEC.md 8.14.7, `common/s32cp437.h`), as
   a real terminal does.  xbase declares CP437 when it takes the terminal
   (screen.c); the terminal shows each byte as its CP437 character, and
   gives keys back as CP437 bytes.  Output to a file or pipe stays bytes.
   Program text (codepage.c) is held in the program's codepage: a file or
   a typed line that is valid UTF-8 is converted on the way in (ruling 6),
   so `"café"` in a UTF-8 .PRG is four characters and shows as itself.
   xbc compiles from UTF-8 and emits each literal past ASCII as
   `cp_literal("...")`, converted at run time to the codepage then in
   force.  Test: test_codepage (xbase), feature-term-charset (every
   engine).
1. **libutf -- not needed yet.**  Case and character classes come from
   the C library (`<wctype.h>`, both libraries, Unicode's tables in a
   UTF-8 locale): the setting switches LC_CTYPE to C.UTF-8.  libutf is
   wanted for the approximations into an old codepage (step 4) and for
   collation; CP850 goes into `~/utf/gen` then.
2. **The setting and the string functions -- built 2026-11-19.**
   `SET CODEPAGE TO "UTF8"` / `"CP437"`, `SET(_SET_CODEPAGE)` (100, past
   Clipper's 38, in xbc's set.ch), XBASE_CODEPAGE for the start.  A switch
   converts every loaded program's text from the file's own (each line past
   ASCII keeps it), so a UTF-8 program that switches on its first line has
   lost nothing to the CP437 load; it also sets LC_CTYPE and the terminal's
   charset.  LEN, SUBSTR, LEFT, RIGHT, AT, RAT, STUFF, PADL/R/C, CHRTRAN
   count characters (one code path: in CP437 a character is a byte);
   UPPER, LOWER, PROPER, ISALPHA, ISUPPER, ISLOWER are Unicode's; CHR and
   ASC are code points.  TRIM, $, STRTRAN, OCCURS, REPLICATE need nothing.
   Harbour's UTF8EX agrees but for LEN of a literal (2 for "é": a
   compile-time byte count, not followed) and CHR/ASC (bytes: ruling 3).
   Tests: test_codepage_utf8 (xbase), utf8mode (xbc).  TRANSFORM and the
   MEMO* line functions count columns: step 3.
3. **The screen -- keys, the shadow and the GETs built 2026-11-19.**
   Keys (ruling (a)): a typed UTF-8 character is read whole and comes from
   INKEY() as its code point, plus 65536 past U+00FF (Alt-A stays 286);
   KEYCHAR(n) gives a key's character, "" for none; KEYBOARD stuffs
   characters.  The shadow's cells hold code points and know the two
   halves of a double-width character; output advances by display width;
   box frames split by character, @ TO draws CP437's rules as code points.
   SAVESCREEN in UTF-8 is four bytes a cell (ruling 5), read with the byte
   functions BLEN, BSUBSTR, BASC, BCHR.  Both GET editors (the Get object,
   the interpreter's READ) edit characters: a field is so many characters,
   the cursor stands at the width of those before it, a picture takes
   Unicode's letters (A, !, @!).  MEMOEDIT and BROWSE's cell editor edit
   characters too, and paint by width; ACHOICE and the DEFINE POPUP/PAD
   menus measure by width; TRANSFORM's template is by character; MEMOLINE,
   MLCOUNT wrap at a width in columns, and MLPOS, MLCTOPOS, MPOSTOLC count
   characters.  The soft return stays the byte 141: HARDCR, MEMOTRAN and
   MEMOEDIT take it only on a character's boundary, so the 8D inside
   c-caron (C4 8D) is left alone.  Tests: test_get_utf8, test_screen_utf8,
   test_codepage_utf8.  A field's width stays in characters (a CJK
   character takes two columns of it); a GET on a table field is still the
   field's bytes wide until step 4.
4. **Tables -- built 2026-11-19.**  A table's codepage is its header's mark
   (byte 29: 0x01 CP437, 0x02 CP850, 0x03 Windows 1252); none, or one not
   known, is the program's.  dbf_get_field_raw and the memo reader convert
   into the program's codepage, dbf_set_field_raw and the memo writer into
   the table's -- a character it lacks as the nearest (libutf's CP437 and
   Latin-1 approximation DFAs, vendored by xbase/sync-libutf.sh; CP850 and
   1252 exactly from src/cptables.h, gen-cptables.py), '?' at the last --
   and a UTF-8 field is cut before a character that would not fit, never
   through it.  field_format_char pads and cuts in characters; the table
   settles the bytes.  A memo's soft return is taken on a character's first
   byte only.  CPCURRENT() is 437 or 65001, CPDBF() the mark or 0.  FoxPro
   2.6 with CODEPAGE=1252 agrees: a CP437-marked table's "M\x81ller" reads
   as 1252's "Müller", its ü is written back as 0x81, an unmarked table is
   read raw.  (Without CODEPAGE= FoxPro's CPCURRENT() is 0 and it converts
   nothing; foxrun has no way to give it a CONFIG.FPW yet.)  Test:
   test_codepage_tables (fixtures in xbase/tests/codepage, mkcptables.py).
   Record-to-record copies (COPY TO, PACK) move bytes as they are.
5. **Index keys -- built 2026-11-19.**  A character key is held in its
   table's codepage, as dBASE reads the file's keys: index_build converts
   each key before it is measured and stored, and every key the program
   hands in (INSERT, REMOVE, SEEK, the UNIQUE check) passes through
   key_pad, which converts, pads, and cuts a UTF-8 key before a character
   that would not fit.  A SEEK's prefix is its converted text's length.
   The index knows its table's mark from the table (index_build, and SET
   INDEX / USE ... INDEX opening the file).  UTF-8 keys in byte order sort
   by code point.  Test: test_codepage_tables (a CP437-marked table indexed
   and sought in UTF-8 mode: the file holds "M\x81ller").  ndxfuzz 20 of
   20 with dBASE III PLUS.  Collation by language (libutf's UCA, 53 CLDR
   locales) is a separate option, not built; an index built that way would
   have to say so in its header.
