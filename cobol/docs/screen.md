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
