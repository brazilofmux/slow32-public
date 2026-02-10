# Stage 4: Terminal UI

## Goal

Implement full-screen data entry with `@...SAY...GET`, the `READ`
command, PICTURE formatting, BROWSE mode, and screen management.
This is what makes dBase III an interactive application rather than
just a command-line tool.

## Prerequisites

- Stage 3 complete (variables, SET commands)
- `term` service operational (raw mode, cursor, colors)

## Screen Commands

| Command | Syntax | Description |
|---------|--------|-------------|
| @ SAY | `@ <row>,<col> SAY <expr>` | Output at position |
| @ GET | `@ <row>,<col> GET <var> [PICTURE <mask>] [RANGE <low>,<high>] [VALID <expr>]` | Input field at position |
| @ SAY GET | `@ <r>,<c> SAY <e> GET <v> [PICTURE <m>]` | Combined |
| READ | `READ` | Activate all pending GETs |
| CLEAR | `CLEAR` | Clear screen |
| @ CLEAR | `@ <r1>,<c1> CLEAR [TO <r2>,<c2>]` | Clear region |
| @ TO | `@ <r1>,<c1> TO <r2>,<c2> [DOUBLE]` | Draw box |
| EDIT | `EDIT [<recno>]` | Full-screen record editor |
| BROWSE | `BROWSE` | Spreadsheet-style view |

## PICTURE Clauses

Formatting masks for GET fields:

| Symbol | Meaning |
|--------|---------|
| `9` | Digit only |
| `#` | Digit, space, or sign |
| `A` | Alpha only |
| `N` | Alpha or digit |
| `X` | Any character |
| `!` | Force uppercase |
| `.` | Decimal point |
| `,` | Thousands separator |
| `$` | Dollar sign fill |
| `*` | Asterisk fill |

Function symbols (prefix with `@`):
- `@!` — force uppercase
- `@R` — template (literal chars inserted)
- `@S<n>` — horizontal scroll width
- `@Z` — display blanks if zero

## GET/READ Implementation

```
GET buffer list (max 64 active GETs):
  - row, col position
  - variable reference
  - picture mask
  - field width
  - display buffer (formatted)
  - edit buffer (raw value)

READ loop:
  1. Display all GETs with current values
  2. Position cursor at first GET
  3. Handle keystrokes:
     - Printable chars: insert/overwrite with PICTURE validation
     - Enter/Tab: accept, move to next GET
     - Shift-Tab: move to previous GET
     - Up/Down arrow: move between GETs
     - Escape: abandon all changes
     - Ctrl-W / PgDn: save all and exit READ
  4. On exit: write values back to variables/fields
```

## BROWSE Implementation

Spreadsheet-style record viewer/editor:
- Field names as column headers
- Records as rows
- Cursor navigation (arrows, PgUp/PgDn, Home/End)
- In-place field editing
- Status bar showing record number, deleted status

## Color Support

Using `term_set_color()` and `term_set_attr()`:
- `SET COLOR TO <standard>[,<enhanced>[,<border>]]`
- Color codes: W(hite), R(ed), G(reen), B(lue), etc.
- `+` for bright, `*` for blink

## Source Files

```
src/
  screen.c        # @...SAY, CLEAR, box drawing
  screen.h
  get.c           # GET/READ engine + PICTURE formatting
  get.h
  browse.c        # BROWSE command
  browse.h
  color.c         # Color management
  color.h
```

## Success Criteria

- `@ 5,10 SAY "Name:" GET mName PICTURE "@!"` works
- READ navigates between GETs with Tab/Enter/arrows
- PICTURE masks validate and format input
- BROWSE shows records in a scrollable grid
- Box drawing with `@ 1,1 TO 20,60 DOUBLE`
- Color support: `SET COLOR TO W+/B,GR+/R`
