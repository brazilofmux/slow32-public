# Screen input: a picture-driven field editor

Status: step 0 done (2026-10-04); steps 1-6 to do.  The user accepted
the recommendations under "Decisions" on 2026-10-04.

A screen is the first thing a person sees of a COBOL program, and the
keystroke behaviour of ACCEPT is the part of it the language says least
about.  Today's editor (`cob_screen_accept` in libcob.c) is small and
correct for what it does: numeric fields are edited as a value and
re-rendered through the PICTURE after every key; everything else is an
overwrite buffer.  It does not check a key against the picture position
it lands on, has no insert mode, and several standard clauses that
touch input are absent.  This plan replaces it with an editor that is
driven by the PICTURE.

## Authority

The user's ruling (2026-10-04): the standard and Micro Focus are
trusted here; GnuCOBOL's screen work is in progress and is a
cross-check at most.

1. **ISO/IEC 1989:2023** for what is required: 9.2 (screens, CRT
   status, cursor, cursor locator, current screen item), 14.9.1 (ACCEPT,
   format 3 and its general rules 13-25), and the screen clauses in
   13.18 (AUTO, FULL, REQUIRED, SECURE, JUSTIFIED, BLANK WHEN ZERO,
   SIGN, PICTURE).
2. **Micro Focus's enhanced ACCEPT/DISPLAY ("ADIS") in its default
   configuration** for every choice the standard leaves to the
   implementor.  Source: the Visual COBOL 8.0 reference (kept outside
   the tree; paraphrased here, never quoted).  ADIS is configurable
   through some thirty options and a key-mapping utility; **we take its
   defaults as the one behaviour and build no configuration layer**
   (the user's earlier ruling on Enter: no ADIS-style configurability).
3. **Our own choice**, marked as such, where the Micro Focus reference
   is silent.  It is silent more often than expected for numeric-edited
   pictures: floating insertion, check protection, CR/DB, how each sign
   form is redrawn, and what Backspace does left of the decimal point.
4. GnuCOBOL: read for its key list and status codes only.  By a reading
   of its source (trunk, Dec 2025) its editor works on the item's raw
   characters and validates on leaving a field; implied decimals, signed
   non-edited items and left-to-right entry into zero-suppressed fields
   do not come out right, and its own changelog calls its numeric entry
   its own invention.  Nothing in this plan follows it.

### A Micro Focus we can run

`tests/mfcheck.sh` already compiles and runs programs under Microsoft
COBOL 5, a licensed Micro Focus compiler of 1993 with ADIS, on the
user's DOS translator (~/x86, "dos monster").  **It can be driven**
(tried 2026-10-04): the translator takes keys from piped stdin (escape
sequences become the extended keys) and `-D FILE` writes the text
screen at exit.  A one-field program, linked with
`T1+ADIS+ADISINIT+ADISKEY`, replayed one key-prefix at a time,
reproduces the reference's `ZZZ99.99` example exactly:

| Keys | Field on screen |
|---|---|
| (none) | `___00.00` |
| `1` | `___10.00` |
| `1 2` | `___12.00` |
| `1 2 3` | `__123.00` |
| `1 2 3 4` | `_1234.00` |
| ... Backspace | `__123.00` |
| ... `5` | `_1235.00` |
| ... `6` | `12356.00` |
| ... `7` | `12356.70` |
| ... `8` Enter | item holds 12356.78 |
| `1 2 3 . 4 5` Enter | item holds 123.45 |

(The underscores are its prompt character in the suppressed positions.)

So Micro Focus is an executable oracle here, not only a document.  The
rig is built (step 0, done 2026-10-04):

- the DOS translator has a **key trace** (`dos-monster -K FILE`): each
  scripted key is released only when the program is idle waiting for
  it, and the text screen and the cursor (cell and shape) are written
  before each; no Ctrl-Z is fed at the end of the script (ADIS reads
  Ctrl-Z as "clear to end of field", which spoiled the first dumps).
  It also learned Shift-Tab (`ESC [ Z`).  A run of ten keys takes a
  third of a second;
- `tests/adischeck.sh PICTURE KEYS` writes the one-field program,
  compiles and links it with ADIS, runs it under the trace and prints
  the field and cursor after each key;
- **docs/adis-observed.md** is the record: some seventy runs over the
  picture shapes and keys this plan names, and what they show.

What it showed that the reference does not say, and that the design
below must follow:

- **Numeric fields are edited as their edited image**, a digit position
  at a time, not as a value.  The cursor starts on the first position
  that is not suppressed; a digit there overtypes and moves right; on
  the point a digit is inserted before it.  So `ZZ9.99`-shaped pictures
  (one `9` before the point: the usual shape) come out calculator-style,
  while `ZZZ99.99` and `9(5)` are typed left to right: **`5` Enter in
  `ZZZ99.99` stores 50.00**, and only the decimal point key aligns.
- Left and Right move among the digits, and a digit typed there
  overtypes that position.
- Sign keys, Backspace, Delete, clear and undo all have definite
  behaviour (the record has each).
- Esc and function keys do nothing by default in ADIS; the standard has
  them end the ACCEPT, and that is what we keep.
- One oddity not to copy: in a picture with no point, the image while
  typing is drawn one position to the left with a prompt character in
  the last position.

## What the standard fixes

- Input fields are the elementary screen items with TO or USING, in the
  order they are declared (9.2.1).  That is the order for next-field and
  previous-field.
- Initial values: FROM, USING or VALUE; otherwise spaces for the
  alphanumeric and national categories and ZEROS for numeric and
  numeric-edited (14.9.1.4 rule 13).  *Today a numeric TO field starts
  as spaces until the first key.*
- The cursor starts at the first input field, or where the CURSOR
  clause's item says when that is inside an input field; it is visible
  and marks where input will go; the cursor locator is set when the
  ACCEPT ends (9.2.4, 9.2.5, rules 18 and 23).  *The CURSOR clause is
  not implemented.*
- What is keyed must be consistent with the PICTURE.  A numeric field's
  content must be acceptable to NUMVAL, a numeric-edited field's to
  NUMVAL-C, and the transfer to the item is defined as that conversion;
  anything else is a MOVE (rules 20 and 22).
- When the check is made, and whether bad data is refused or left in
  place, is the implementor's.  If it is left in place the ACCEPT fails:
  CRT status 8001, EC-DATA-INCOMPATIBLE, and only the consistent fields
  are transferred (rule 21).  **We refuse at the keystroke, so 8001
  cannot arise from typing** (the standard's note allows exactly this).
- Endings: 0000 for Enter or a full AUTO field with no next field; 1xxx
  for a function key; 2xxx for a context-dependent one; 8000 when no
  input item is on the screen; 9xxx ours (9.2.3).  A function key or an
  EC-SCREEN condition takes ON EXCEPTION (rules 24-25).  *8000 and ON
  EXCEPTION on a screen ACCEPT are not implemented.*
- REQUIRED and FULL refuse the terminator key and any cursor move out
  of the field until satisfied; they do nothing until the cursor has
  entered the field, and a function key bypasses them (13.18.47,
  13.18.26).  *Today Up and Shift-Tab leave without the check, and FULL
  is not applied to numeric fields.*

## What Micro Focus's defaults add

Paraphrased from the reference; the option numbers are ADIS's.

**Keys** (default mapping; ours will be fixed at this):

| Key | Function |
|---|---|
| Enter | end the ACCEPT |
| Tab / Shift-Tab | next / previous field |
| Left / Right | one position; at a field's edge, on into the neighbouring field |
| Up / Down | the nearest input position above / below |
| Home | first field of the screen |
| End | end of the line of a multi-line field, then of the field, then the last field |
| Backspace | undo the last character (see below) |
| Delete | delete under the cursor, closing up |
| Insert | toggle insert / replace (text fields only) |
| Ctrl-X / Ctrl-Z | clear field / clear to end of field |
| Ctrl-A | undo: the field as it was when the cursor entered it |
| Ctrl-O / Ctrl-R / Ctrl-Y / Ctrl-F | insert a space; restore a deleted character; retype; change case |
| Ctrl-End / Ctrl-Home | clear to end of screen / clear screen |
| F1.. / Esc | user function keys: end the ACCEPT with a status |

**Text fields** (alphanumeric, alphabetic, alphanumeric-edited):
cursor at the start; a key overtypes and advances, or inserts in insert
mode with what falls off the end kept in an overflow buffer; at the
last position the cursor stays and the last character may be overtyped;
an alphabetic field takes only letters and space.  Deleted characters
go to a restore buffer that Backspace (in replace mode) and Ctrl-R give
back.  JUSTIFIED RIGHT is applied when the ACCEPT ends, not while
typing.  PROMPT shows a fill character for the trailing blanks and the
cursor does not go past the data.  SECURE shows nothing (the cursor
moves); an asterisk per key is an option.  An alphanumeric-edited
picture is treated as X(n): its insertion positions are ordinary.

**Numeric fields, "fixed format"** (the default for numeric-edited
pictures up to 32 characters): the field is re-edited through its
picture after every key; only digits, sign and the decimal point are
taken; insertion characters are skipped by the cursor; no insert mode.

- *No zero suppression* (`9(5)`, `999.99`): typed left to right like
  text; pressing the decimal point right-aligns what has been typed.
  `9(5)`: `1` `2` `4` shows 12400; Backspace 12000; `.` gives 00012.
- *Zero suppression* (`ZZZ99.99`): the cursor starts at the first
  position that is not suppressed (on the point if all are); digits
  advance to the point, then insert before it, pushing left, until the
  integer part is full; then the cursor jumps into the fraction.  The
  decimal point key goes to the fraction at any time.
- A non-edited `9(m)V9(n)` is taken as two adjacent fields, integer and
  fraction; the V has no screen position.
- `+` and `-` set the sign wherever the cursor is.
- FULL on a suppressed field means zero, or no digit still suppressed.
  BLANK WHEN ZERO applies only while the field is not the current one.

**Not taken** (they are other ADIS configurations, or other dialects):
free-format numeric entry with SPACE-FILL, ZERO-FILL, LEFT/RIGHT-JUSTIFY
and TRAILING-SIGN; RM-style entry and MODE IS BLOCK; group-item ACCEPT
as a row of fields; the indicator line; tab stops; mouse.  Each is a
candidate for `-dialect=mf` if a program needs it, not before.

## What we have today

- **The terminal service** (`runtime/term.h`, host side in
  `tools/emulator/mmio_ring.c` and the QEMU port; normative text in
  docs/SPEC.md 8.13-8.14): position, clear, one attribute per cell,
  8+8 colours, raw key bytes, a zero-timeout "key ready", UTF-8 read,
  save/restore, and batched updates that repaint only changed cells.
  A timed key wait (`term_wait_key`) exists, built in the guest on the
  timer service.  **Absent: cursor visibility and shape**, combined
  attributes (bold and reverse together), resize notice, mouse.
- **Keys** are decoded in libcob (`scr_key`).  Known faults: a lone ESC
  is told from a sequence by a zero-timeout test, so a sequence split
  across reads reads as Escape; ESC followed by a letter (Alt-x) is
  taken as Escape, which abandons the ACCEPT, then the letter; modified
  keys (`ESC [ 1 ; 5 C`) are not parsed and their tail is typed into
  the field.
- **What the compiler tells the runtime about a field**: category,
  digits, scale, signed, BLANK WHEN ZERO, and the flattened picture
  only when it is edited.  Not passed: the per-position classes of a
  plain picture (`XXA99` loses its shape), JUSTIFIED (the SCREEN
  SECTION does not parse it), the sign's position.
- **Latent defects found while reading** (to be fixed in step 1
  whatever else is decided):
  - a refused first key in a numeric field (a point on an integer
    picture, a sign on an unsigned one) has already cleared the value:
    the screen shows the old number, zero is stored;
  - the digit buffers hold 20 digits; -std=2002 allows 31;
  - under DECIMAL-POINT IS COMMA the point's column is looked for as a
    period;
  - a positioned ACCEPT of a COMP or packed item takes its storage size
    as its width;
  - plain `ACCEPT identifier` after the terminal is in raw mode reads
    with fgets: on a real terminal Enter sends CR and the line never
    ends;
  - stale comments (UNDERLINE "painted plain"; "end of input submits").

## Design

### One editor core, testable on the host

The editor becomes a state machine with no terminal in it:

```
field model  +  editor state  +  key function   ->   new state, paint requests, verdict
```

- **Field model**, built once per field from what the compiler emits: an
  array of positions, each with a role (text position of class X, A or
  9; digit; suppressible digit; floating position; the point; a sign
  position; fixed insertion; national cluster cell) and the field's
  flags.  For numeric pictures also the integer and fraction digit
  counts and where each digit shows.
- **Editor state**: text buffer or digit buffers, cursor, insert mode,
  the entry snapshot (for undo), the restore buffer.
- **Key functions** are an enum that mirrors the table above; the key
  decoder turns bytes into them.  The mapping is one static table.
- **Output** is a list of "repaint these columns / move the cursor /
  beep"; the terminal layer applies it.

The core is one source file compiled into libcob and, unchanged, into a
host test program (as `pictest`, `bt_test` and `wide_test` already
are).  Its tests are tables:

```
PIC ZZZ99.99, value 0:  "1"  -> "   10.00" cursor 6
                        "2"  -> "   12.00" cursor 6 ...
```

Hundreds of such rows cost nothing to run and are where the behaviour
is pinned.  The documented Micro Focus examples go in first; whatever
MS COBOL 5 can be made to show goes in beside them.

### Numeric fields: digits in picture positions

The oracle settled this.  The state is the string of integer digits and
the string of fraction digits *as they stand in the picture's digit
positions* (so `5` typed on the first `9` of `ZZZ99.99` is the tens
digit), a sign, and a cursor that is on a digit position or on the
point.  After every key the image is produced by the ordinary editing
code from that state -- zero suppression, floating insertion, check
protection, commas, CR/DB cost the editor nothing -- and the cursor's
column is found from the picture.  The transfer at the end is the
value those digits spell: the standard's NUMVAL/NUMVAL-C by
construction.

The per-key rules are the ones in docs/adis-observed.md.  They are
pinned by a **differential test against the oracle**: the host test
program and `adischeck.sh` are run on the same picture and keys --
the recorded batteries first, then random key strings over a list of
picture shapes -- and the field and cursor after every key must match.
The deliberate differences (function keys end the ACCEPT; the no-point
drawing oddity; anything decided below) are listed in the test.

### Compiler side

- Emit a per-position mask for every screen field's picture (today only
  edited pictures carry their pattern).
- SCREEN SECTION: JUSTIFIED, SIGN.  SPECIAL-NAMES: CURSOR IS.
- ACCEPT screen: ON EXCEPTION / NOT ON EXCEPTION; status 8000.
- The slot record has free bits for the new flags; no layout change is
  expected.

### Terminal side

- Key decoding moves behind one function with a proper CSI parser
  (parameters and modifiers), a timed wait after ESC (`term_wait_key`,
  no wire change), Alt-key recognised and ignored, unknown sequences
  swallowed whole.
- **Cursor shape and visibility need a new term opcode** (the one wire
  change in this plan): docs/SPEC.md 8.13-8.14, both host
  implementations, the guest library, a regression test for the
  four-engine differential, and a clean-room rerun.  `term_init` must
  start reading the granted opcode count so a new guest on an old host
  does not call past the range.  It is used for the insert/replace
  indication (bar versus block), which Micro Focus shows as text on an
  indicator line.  It can be the last step; the editor works without it.
- Plain console ACCEPT while the terminal is up goes through the editor
  as a one-field text ACCEPT at the cursor.

### Tests and the existing expected files

Every screen test's `.expected` is the raw ANSI stream, so any change
to when the cursor is positioned rewrites all eighteen.  Two measures:

- the editor's behaviour is pinned by the host table tests, not by
  streams;
- the harness gains a rendered-screen comparison (the final 24x80 text,
  as `~/acas/build/screen.py` does for ACAS) for tests about what the
  user sees; the raw stream stays for the few tests about the stream
  itself (repaint economy, attributes).

The streams are regenerated once, in a commit of their own, each read
against its rendered screen before it is accepted.

## Steps

0. **The oracle rig.**  DONE: the translator's key trace,
   `tests/adischeck.sh`, docs/adis-observed.md.
1. **Keys and defects.**  The key decoder; the latent defects above; no
   intended change of behaviour.  Gates as usual.
2. **The core, text fields.**  The state machine and its host tests;
   the mask from the compiler; text fields to Micro Focus's defaults
   (class check, insert mode, Delete/clear/undo, restore buffer, PROMPT,
   end-of-field behaviour); the rendered-screen test mode; streams
   regenerated.
3. **Numeric fields.**  Fixed-format behaviour by picture shape; sign;
   Delete, clear and undo; FULL for suppressed fields; BLANK WHEN ZERO
   while current; the silent cases settled by what MS COBOL 5 does,
   and written down.
4. **The standard's remainder.**  CURSOR IS and the cursor locator;
   JUSTIFIED and SIGN in the SCREEN SECTION; initial ZEROS; 8000; ON
   EXCEPTION; REQUIRED/FULL exactly as 13.18.47 and 13.18.26 have them;
   field-to-field movement with Left/Right/Up/Down as in the table.
5. **Cursor opcode.**  The term service change, spec first, then the
   insert-mode cursor.
6. **Only on demand, behind `-dialect=mf`:** free-format entry and its
   fill/justify phrases, TIMEOUT, UPPER/LOWER, the three-byte CRT
   status, group-item ACCEPT.

Each step ends green on all COBOL gates (and, for step 5, the platform
gates and the clean room).  docs/screen.md is rewritten at step 3 to
describe what was built; behavior-points.md gains a point for each
choice that is ours and not the standard's or Micro Focus's.

## Decisions (accepted 2026-10-04: the recommendations stand)

- **DECIDE 1: plain numeric fields.**  Micro Focus types `9(5)` left to
  right and right-aligns on the decimal point key (12400, then `.`
  gives 00012).  Today every numeric field is calculator-style (digits
  push in from the right).  Following Micro Focus changes how `99` and
  `9(5)` fields feel and two existing tests.  Recommendation: follow
  Micro Focus; it is the ruling, and it is what ACAS-era operators
  expect.  *The oracle sharpened what this means: `5` Enter stores
  50000 in `9(5)` and 50.00 in `ZZZ99.99`; pictures with a single `9`
  before the point are unaffected.*
- **DECIDE 2: alphanumeric-edited pictures (`XX/XX/XXXX`).**  Micro
  Focus treats them as X(n): the slashes can be typed over.  The
  standard says entered data shall be consistent with the PICTURE.
  Recommendation: protect the insertion positions and skip them (the
  standard's reading, and what a date field wants), recorded as a point
  where we are stricter than Micro Focus.
- **DECIDE 3: Home.**  Micro Focus's Home goes to the first field of
  the screen, not the start of the field (it has a start-of-field
  function with no default key).  Recommendation: follow it, with
  Ctrl-A (undo) and Shift-Tab covering "back to the start".
- **DECIDE 4: SECURE.**  Micro Focus's default shows nothing as you
  type; we show an asterisk per character today (its option 2, and the
  common expectation).  Recommendation: keep the asterisks, recorded as
  our choice.
- **DECIDE 5: the wire change.**  A cursor shape/visibility opcode in
  the term service, with the spec and clean-room work it brings, for the
  insert-mode indication.  Recommendation: yes, as step 5.
