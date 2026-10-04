# SCREEN SECTION

ISO COBOL 85 has no SCREEN SECTION. Micro Focus did, RM did, GnuCOBOL
does, and majesty already writes it (`usescreen.cbl`, `menu.cbl`,
`taskdt.cbl`). It is an implementor module of the same rank as LINE
SEQUENTIAL: documented, tested, not pretended to be X3.23-1985.

Retiring GnuCOBOL without it leaves the interactive programs behind.

## What it is

A data description of a CRT, compiled the same way Report Writer
compiles a page and PICTURE compiles a field — a table of slots, then
a small engine.

```
SCREEN SECTION.
01  screen-name.
    05  BLANK SCREEN.
    05  LINE i COLUMN j VALUE '…' [HIGHLIGHT|UNDERLINE|…].
    05  LINE i COLUMN j PIC … FROM id.
    05  LINE i COLUMN j PIC … TO id.
    05  LINE i COLUMN j PIC … USING id [AUTO].
```

`DISPLAY screen-name` paints. `ACCEPT screen-name` runs the focus
loop. `usescreen.cbl` also `CALL 'CBL_GET_SCR_SIZE'`.

Bindings:

- `FROM` — output only
- `TO` — input only
- `USING` — both (dBase `@ SAY GET` of one variable)
- `VALUE` — literal, output only

## Runtime

SLOW-32 already has `runtime/include/term.h`: raw mode, size, cursor,
clear, attributes, color, getkey, kbhit, save/restore screen,
begin/end buffered update. Nano and dBase speak it. SCREEN SECTION
paints through that service, not a private curses.

dBase Stage 4 (`dbase/docs/STAGE4-terminal-ui.md`) is the ACCEPT
loop we would otherwise invent:

- GET buffer list (row, col, picture, width, display buffer, edit
  buffer)
- READ: show all, focus first, keystrokes honour the picture,
  Tab / AUTO advance, Escape abandons, a save key commits

Compile the SCREEN SECTION into that list. `DISPLAY` is "paint every
slot." `ACCEPT` is the READ loop. `BLANK SCREEN` is `term_clear(0)`
at the start of that DISPLAY/ACCEPT.

`CBL_GET_SCR_SIZE` is `term_get_size`.

`HIGHLIGHT` is `term_set_attr(1)` (bold). `UNDERLINE` is an attribute
the term service may not yet name; if `term.h` cannot express it,
v1 paints without underline and the gap is listed, rather than
faking a private escape. Reverse is `term_set_attr(7)`.

## State-machine compiling

This is the third of the three machines in [architecture.md](architecture.md).
Ragel earns a keep on field input: `9`, `A`, `X`, edited numerics
(`$zz9.99-` in `usescreen.cbl`) are regular languages. The picture
scanner already exists in spirit in cobc370's `picture.rl`; SCREEN
ACCEPT needs the *input* direction (what keystrokes a picture
allows), which `ED` never did.

Do not share Report Writer's engine. Paper and focus are different
extra state. Do share PICTURE analysis for the field's category and
edit description, and share `term.h` with nano and dBase.

## v1 screens

`usescreen.cbl`: `BLANK SCREEN`, `LINE`/`COLUMN`, `VALUE`, `PIC x(6) TO`,
`PIC zz9 FROM`, `PIC $zz9.99- BLANK WHEN ZERO FROM`, `DISPLAY` then
`ACCEPT` then `DISPLAY`.

`menu.cbl`: several `01` screens, `UNDERLINE`, `HIGHLIGHT`, `PIC xx
USING option AUTO`, switching screens on a two-character command via
nested `EVALUATE` inside `PERFORM WITH TEST AFTER`.

`taskdt.cbl`, which menu's `DT` command `CALL`s (a user function
until the rewrite),
carries a **third** screen, `date-page`: `FROM todays-date`, an item
the function builds at run time with `STRING`, `INSPECT` and
reference modification from `FUNCTION CURRENT-DATE`. So "the two
screen programs" are three screens and most of Nucleus Level 2. See
[plan.md](plan.md) Stage 9 and [functions.md](functions.md).

`usescreen.cbl` also `MOVE`s `amount-in PIC X(6)` to `amount PIC
S9(3)V99 COMP-5` after `ACCEPT` — the alphanumeric-to-numeric cell
in [lowering.md](lowering.md).

If `UNDERLINE` is missing from `term.h` when this is implemented,
menu still has to be usable; the words remain visible without the
attribute.

## The eventual target (the user's RM and Micro Focus experience)

Recorded 2026-08-29 so it outlives the conversation. What a COBOL
screen does on those systems, and what this one should grow into:

- **TAB order**: fields taken in declaration order; TAB and Enter move
  to the next.
- **Enter as submit** on the last field (or a commit key).
  *Superseded 2026-10-04 (the user's ruling, ACAS): Enter submits from
  any field, as the standard has it (2023 9.2.3, CRT status 0000) and as
  GnuCOBOL and Micro Focus do; Tab moves.  See "Order" below.*
- **In-place editing**: numeric fields anchored on the decimal point,
  or right-aligned; text fields left-aligned; typing edits the value
  where it sits rather than clearing it.
- **AUTO**: some fields advance by themselves when full.
- **SECURE**: some fields echo `*` (passwords).
- Field look: underline, or more commonly **reverse video**.

That set is reachable with dBase today, but dBase makes the program
manage it by hand; the point of the SCREEN SECTION is that the
compiler does. (dBase note: the user's 1986 teacher's-pet code still
runs on `dbase/`.)

## As built (Stage 8)

`libcob` compiles each `01` into a slot table (`cob_screen` /
`cob_scr_field` in `cobrt.h`): kind (VALUE / FROM / TO / USING), LINE,
COLUMN, width, the literal or the item with its descriptor and the
slot's PICTURE descriptor, attribute flags. `DISPLAY screen` paints
every slot inside `term_begin_update` / `term_end_update` (so the
emulator emits only changed cells); `ACCEPT screen` paints, then runs
the focus loop over the TO and USING slots in order: printable keys
overwrite and advance, Backspace erases, TAB goes to the next field,
Enter submits from any field, Escape or end of input ends
the ACCEPT, AUTO advances when the field fills; every input field's
text is then `MOVE`d into its item through the ordinary conversion
matrix -- which is where usescreen's `PIC X(6)` to `COMP-5` lands,
now parsed as GnuCOBOL does (blanks, sign, digits, point). HIGHLIGHT
is bold, REVERSE-VIDEO reverse; **UNDERLINE is painted plain** because
`term.h` has no such attribute yet. `CBL_GET_SCR_SIZE` is
`term_get_size`. The main wrapper leaves through `cob_stop_run`, which
restores the terminal.

## As built (Stage 58, 2026-08-30): the target above, reached

The focus loop (`cob_screen_accept`) now does what the RM/Micro Focus
notes ask, with GnuCOBOL nowhere in reach (its screens need a tty):

- **Keys.** The terminal's bytes, with the ANSI cursor sequences folded
  into codes (`scr_key`): arrows, Home/End, Delete, Shift-Tab (`ESC [
  Z`; `ESC TAB` on terminals without one). A lone Escape is told from
  a sequence by `term_kbhit`.
- **Order.** Fields in declaration order (AUTO's "next input field
  declared"). Tab and Down go to the next, wrapping; Up and Shift-Tab
  go back. **Enter submits from any field** (2023 9.2.3: status 0000 is
  "the operator pressing the enter key"; GnuCOBOL and Micro Focus alike)
  -- before 2026-10-04 it moved to the next field and submitted only on
  the last, which ACAS's screens do not expect. REQUIRED and FULL hold
  the field the cursor is in against Enter as against Tab (2023
  13.18.47.4 rule 3, 13.18.26.4 rule 3). Escape abandons: no item is changed. End of input
  (a `.keys` file running out) ends the run with a message and exit
  status 2: a program that re-prompts would otherwise loop.
- **Text fields** are edited where they sit: the buffer starts as the
  item's rendering (`USING`) or blanks (`TO`); typing overwrites at the
  cursor and moves right (stays on the last column when full; `AUTO`
  leaves); Left/Right/Home/End move; Backspace and Delete take a
  character out and close the gap. `SECURE` echoes `*` for every
  non-blank character. `FULL` refuses to leave a field neither empty
  nor full; `REQUIRED` refuses to leave an empty one (a bell).
- **Numeric fields** (a slot whose PICTURE is numeric or numeric-edited)
  (superseded 2026-10-04: see "Numeric fields through the editor core"
  below) were edited on the point: digits typed before the point shift into the
  integer part, `.` (or `,` under `DECIMAL-POINT IS COMMA`) moves to the
  fraction, which fills left to right; `-` and `+` set the sign when the
  picture has one; Backspace takes the last digit back; Home clears.
  After every key the value is rendered through the slot's picture and
  repainted (`Z9.99` shows ` 9.75`), the cursor standing at the point,
  or after the last fraction digit, or on the last column of an integer
  field. The first digit typed into a field replaces its value (Enter
  alone keeps it); `AUTO` leaves when the fraction, or an integer
  field, is full. The commit is `cob_put_num` at the picture's scale,
  so a `99` slot on a `9(4)` item and a `Z9.99` slot on a `S9(3)V99`
  item both land right. `REQUIRED` on a numeric field wants a non-zero
  value.
- **Look.** `UNDERLINE` is `term_set_attr(4)`, `LOWLIGHT` 2 -- the
  emulator's term service passes any SGR code through, so `term.h` grew
  only a comment. `FOREGROUND-COLOR`/`BACKGROUND-COLOR n` use COBOL's
  numbering (0 black, 1 blue, 2 green, 3 cyan, 4 red, 5 magenta, 6
  yellow, 7 white), mapped to ANSI's; painted and reset after the
  field. `BLINK`, `BELL`, `ERASE` are accepted and do nothing.
- **Placement.** `LINE PLUS n` is the previous slot's line plus n;
  `COLUMN PLUS n` counts from the position after the previous slot, as
  GnuCOBOL does; a slot without `LINE` takes the previous slot's line,
  without `COLUMN` the position after it.

- **`CRT STATUS`** (Stage 59). `SPECIAL-NAMES. CRT STATUS IS item.`
  puts the ACCEPT's ending in the item, in GnuCOBOL's numbering: 0000
  an ordinary ending (Enter, or the last AUTO field filling), 1001-1012
  a function key (F1-F4 arrive as `ESC O P..S` or `ESC [ 11~..14~`,
  F5-F12 as `ESC [ 15~..24~`), 2001/2002 Page Up and Page Down, 2005
  Escape. A function key or page key ends the ACCEPT with the fields
  committed; Escape still abandons. A numeric item takes the number, a
  three-byte alphanumeric GnuCOBOL's packed form, anything else the
  four digits as text. The escape reader keeps one byte of pushback,
  so a lone Escape followed by typed text is told from a sequence.

- **Nested groups** (Stage 60). An entry with a name and no PICTURE
  or VALUE is a group: its look (attributes, colours) composes over
  the enclosing group's and reaches its children -- the input-only
  clauses (`AUTO`, `SECURE`, `REQUIRED`, `FULL`) only the fields that
  take input -- and its `LINE`/`COLUMN` anchor its first child. A
  *named* group is a screen of its own for `DISPLAY` and `ACCEPT`: the
  compiler emits a second `cob_screen` record whose field pointer
  lands mid-table, so the runtime paints or focuses just that window,
  and the group's own slot count bounds the loop. The runtime needed
  nothing.

- **Subscripted, LINKAGE and EXTERNAL slot items** (Stage 61). A
  slot's reference is recorded as tokens (Report Writer's SOURCE
  trick, since OCCURS dimensions do not exist yet when the Screen
  Section parses) and resolved at first use. A literal subscript
  folds into the slot's static address; a runtime subscript, a
  LINKAGE item or an EXTERNAL one makes the slot *dynamic*: its image
  points at a .data cell, flagged in the kind byte's high bit, and
  every ACCEPT or DISPLAY of the window computes the reference's
  address afresh (from the reference parsed at first use) and stores
  it first -- so `PIC X(4) USING
  CELL(I)` follows I from one ACCEPT to the next. A contained program
  may own screens now (the screen table gained a per-unit base, like
  the symbol table's); screens are per-unit, not GLOBAL. Reference
  modification in a slot stays refused.

The module is complete against the RM/Micro Focus target and
GnuCOBOL's clause set; only `BLINK`, `BELL` and `ERASE EOL/EOS` are
accepted without effect.

Testing: `tests/free/screen3.cbl` pins the four endings (Enter, F3,
Page Down, Escape after a typed character); `tests/free/screen2.cbl` drives the rest from `screen2.keys`
(arrows, Shift-Tab, a REQUIRED refusal, SECURE stars, both numeric
shapes) and its ANSI stream is pinned, reviewed by hand like
`screen.cbl`'s. majesty's `menu.s32x` walks MAIN, DAILY, the DATE PAGE
and back on `dwdtmmlo`.

Testing: `tests/free/screen.cbl` is driven by `screen.keys` on the
emulator's stdin and its expected output is the ANSI stream, reviewed
by hand -- GnuCOBOL's screens need a real tty, so this is the one
test class without an oracle run (`no oracle` in the source tells
the harness).

## RM/COBOL's positioned DISPLAY / ACCEPT (2026-09-05)

The Open Systems suite (`~/open`, 1978-83 RM/COBOL) never had a SCREEN
SECTION: every screen is painted with `DISPLAY x LINE n, POSITION m, ERASE
EOS, HIGH, SIZE k` and read with `ACCEPT x LINE n, POSITION m, PROMPT,
UPDATE, NO BEEP`, plus the 1983 `DISPLAY x AT rrcc [WITH ERASE EOS]`
spelling (GitHub #32, #33). Each such statement becomes a screen of its own,
one slot per operand, on this runtime; the slot record grew by a word for
it (`ext`, `prompt`; 32 bytes on the guest, `SCRF_SIZE` in the compiler):

- `COB_SX_POS`: LINE 0 is the line after the last positioned statement,
  POSITION 0 is column 1 (a SCREEN SECTION slot keeps its numbers);
  `COB_SX_CONT`: a second operand follows the first on its line;
- `COB_SX_ERASE_EOS` / `_EOL` / `_ALL` (`ERASE`, `ERASE SCREEN`): cleared
  before painting, from the slot's position;
- `COB_SX_PROMPT`: an input slot shows `prompt` (`_`, or `PROMPT "c"`)
  where it holds a space; `UPDATE` makes the slot USING;
- `COB_SX_NOBEEP`: no bell on a rejected key;
- HIGH / LOW / REVERSE are the HIGHLIGHT / LOWLIGHT / REVERSE-VIDEO flags.

LINE, POSITION and AT given as identifiers are stored into the slot by the
statement before the call (`cob_scr_at` splits rrcc). Once a positioned
statement has painted, a plain DISPLAY is positioned too, on the next line
at column 1 (RM's rule); a SCREEN SECTION program keeps its stdout stream.
ECHO, OFF, TAB, CONVERT, BLINK, BEEP, UNIT and CONTROL are accepted and
ignored. tests/fixed/rmscreen pins the stream.

## National fields (2026-09-28, ISSUES-92)

A PIC N(n) field is n columns, and its text is laid out by display
width. A CJK character takes two columns, and a combining mark rides
with the character before it. The rule is docs/national.md's, "National
text in columns".

Keys reach the focus loop as characters: `scr_key` decodes UTF-8, and a
byte that is not UTF-8 stands for U+FFFD. The cursor and function-key
codes moved above Unicode's range (0x110001 on) to make room.
Alphanumeric fields still take only printable ASCII, as before.

A national field is edited a cluster at a time, overwriting as a text
field does. The cursor moves by the columns each cluster takes. A
character that would not fit the field's columns or its item's code
units is refused with the beep. The terminal under it is the
Unicode-aware term service (docs/SERVICE_NEGOTIATION.md).

## Text fields through the editor core (2026-10-04)

Step 2 of docs/plans/screen-input.md.  A text field (alphanumeric,
alphabetic, alphanumeric-edited; national fields keep the cluster editor
above, numeric fields theirs until step 3) is now edited by
`libcob/scredit.h`: a state machine with no terminal in it, compiled
into libcob and into a host test (`tests/scredit_test.c`).  Its
behaviour is Micro Focus's ADIS in its default configuration, pinned
key for key against Microsoft COBOL 5 by `tests/scredit-differential.sh`
(1,258 of 1,260 random key strings agree; docs/adis-observed.md).

- **Keys.** A character overtypes and the cursor moves right; Insert
  toggles insert mode (a character pushes the rest right; what falls
  off the end is kept and comes back when a Delete makes room).
  Backspace in replace mode puts back what was overtyped; Delete closes
  up.  Ctrl-X clears the field, Ctrl-Z from the cursor on, Ctrl-A puts
  the field back as it was when the cursor entered it, Ctrl-O inserts a
  space, Ctrl-R re-inserts the last deleted character, Ctrl-F changes a
  letter's case.
- **The cursor** does not go past the end of the data: Right there goes
  to the next field, Left at the first position to the previous one (at
  the end of its data), End to the end of the data and from there to the
  last field, Home to the first field of the screen.  On the last
  position of a full field it stays, and the next character overtypes.
- **The picture is checked at each key** (2023 14.9.1.4 rule 20): a
  `PIC A` position takes a letter or a space, a `9` position in a text
  picture a digit; a refused character beeps.  The compiler passes each
  input field's picture a symbol a column.
- **Insertion characters are protected**: in `XX/XX/XXXX` the slashes
  stay and the cursor skips them, so `04102026` is the date.  This is
  the one place the editor is deliberately stricter than ADIS, which
  treats such a picture as X(n).
- **Leaving a field** by any cursor key is subject to REQUIRED and FULL
  (2023 13.18.47.4 rule 3, 13.18.26.4 rule 3), not only by Tab and Enter.
- The prompt character shows after the data only where the PROMPT
  phrase asks for it (a positioned ACCEPT's); a SCREEN SECTION field
  shows spaces, as before.  SECURE shows an asterisk a character.

A numeric field's Home no longer zeroes it (Home is the first field);
Ctrl-X does.

## Numeric fields through the editor core (2026-10-04)

Step 3 of the same plan; it replaces the "edited on the point"
description under Stage 58 above.  A numeric or numeric-edited field is
edited as digits standing in the picture's digit positions, by the
`sn_*` half of `libcob/scredit.h`.  After every key the field is the
picture's ordinary editing of those digits, so commas, floating signs
and check protection move as the number grows.

**A number is keyed as it is written** ("natural entry").  The core also
has the fixed-position style of the 1993 Micro Focus runtime that was
observed while building it -- the adding machine's, where `5` Enter in
`ZZZ99.99` is 50.00 and only the point key aligns -- and that style is
kept and tested (`tests/scrnum.txt`, `tests/scredit-differential.sh -N`,
docs/adis-observed.md) because it is where the rules about pictures were
learned.  The runtime does not use it: a positioned ACCEPT of a number
has always been keyed naturally here, the programs of the Open Systems
corpus are driven that way, and nothing since the adding machine expects
otherwise.

- **Entering a field** puts the cursor on the point (the last column of
  a picture with no fraction).  The first digit, point or Backspace
  replaces the value the field held; a cursor key first, and the value
  is edited instead.  Enter alone keeps it.
- **Digits** enter at the point and push the others left: `5` is 5.00 in
  `ZZZ99.99`, `42` is 42 in `9(5)`.  When the integer part is full the
  cursor goes into the fraction, across an assumed point too: `12345`
  in `9(3)V99` is 123.45.  A full picture with no fraction refuses the
  next digit.
- **The point key** (`,` under DECIMAL-POINT IS COMMA) goes to the
  fraction, which fills left to right; where there is no fraction it is
  refused and the field is as it was.
- **On a digit**, reached by Left or Right, a digit overtypes and the
  cursor moves right.
- **Keys**: `-` and `+` set the sign where the picture has one (an
  edited sign, or `S`), from anywhere in the field and without
  replacing the value; Backspace takes the last integer digit out at
  the point, or zeroes the digit to the left elsewhere; Delete closes up
  from the left; Ctrl-X zeroes the field, Ctrl-Z from the cursor on,
  Ctrl-A puts back the value the field was entered with.  Left, Right
  and End move within the field and then to the neighbour.
- **While the cursor is in the field** the point and the fraction show
  though the value is zero, a zero that has been keyed shows though the
  picture would suppress it, and BLANK WHEN ZERO waits; when the field
  is left it is the picture's editing of the value.
- **AUTO** leaves when the last fraction digit, or the last digit of a
  picture with none, has been typed.  **REQUIRED** wants a value that
  is not zero.
- **The value** passes between the item, the core and the picture as a
  DISPLAY number through `cob_move`: eighteen digits, any scale (a P
  picture is edited as its digit positions), any kind of item.

## The standard's remainder (2026-10-04)

Step 4 of docs/plans/screen-input.md.

- **`CURSOR IS data-name`** (SPECIAL-NAMES, 2023 12.3.7): the cursor
  locator, six digits, line then column.  Before an ACCEPT of a screen
  it says where the cursor starts, when that is inside an input field;
  afterwards it holds where the cursor stood when the terminating key
  was pressed.
- **`SIGN LEADING | TRAILING [SEPARATE]`** and **`JUSTIFIED`** on a
  screen item.  A separate sign has its own column and is set by the
  `-` and `+` keys from anywhere in the field.  A justified text field
  is keyed from the left like any other and moves right when left.
- **A numeric TO field** shows zeros through its picture when the
  ACCEPT starts.
- **Endings.**  Enter, or the last AUTO field filling: 0000.  A
  function key: 1xxx or 2xxx.  A screen with no input item: 8000, with
  no wait.  `ON EXCEPTION` takes everything but 0000; `NOT ON
  EXCEPTION` takes 0000.
- **FULL** on a numeric field wants zero, or every digit position of
  the picture in use.
