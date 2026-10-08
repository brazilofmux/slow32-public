# The screen section: 13.9, 13.17, the screen clauses, ACCEPT and DISPLAY of a screen

Swept 2026-10-06 (cobol ISSUES-125). ISO/IEC 1989:2023: 13.17 the
screen description entry; the screen clauses 13.18.3 AUTO, .4
BACKGROUND-COLOR, .6 BELL, .7 BLANK, .9 BLINK, .14 COLUMN (format 2),
.21 ERASE, .23 FOREGROUND-COLOR, .25 FROM, .26 FULL, .30 HIGHLIGHT, .35
LINE (format 2), .36 LOWLIGHT, .47 REQUIRED, .48 REVERSE-VIDEO, .50
SECURE, .56 TO, .59 UNDERLINE, .61 USING; the screen rules of the
shared clauses (JUSTIFIED, SIGN, OCCURS, USAGE, VALUE); 14.9.1 and
14.9.11 format 2/3 (ACCEPT and DISPLAY of a screen); 12.3.7's CURSOR and
CRT STATUS rules; 9.2. The key-by-key behaviour of input, which the
standard leaves to the implementor, is docs/screen.md and
docs/plans/screen-input.md.

Found by this sweep and fixed before it was written: PLUS or MINUS on
the first item, a clause written twice, HIGHLIGHT with LOWLIGHT,
JUSTIFIED and BLANK WHEN ZERO on a group, a reserved word as a screen
name, LINE without COLUMN following the item before, ACCEPT of an output-only screen, OCCURS with absolute LINE and
COLUMN, FROM with TO silently dropping the FROM -- all accepted before;
SECURE, REQUIRED and FULL on an output item refused though the standard
allows them; the 01 taking no attributes; BELL and BLINK read and
dropped.

## 13.17 The screen description entry

| rule | paraphrase | disposition |
|---|---|---|
| format 1/2 | a screen entry names itself first, or is FILLER; the rest in any order | **refused** if the name comes later: the name is read only after the level-number |
| SR 1 (13.17.3) | each clause once; HIGHLIGHT and LOWLIGHT one element | **refused**: bad/screen-dup-clause, bad/screen-hilo -- both accepted before, the last one winning |
| SR 1 | one of FROM, TO, USING, VALUE (FROM with TO is one element) | **refused**: a second one; FROM with TO **implemented** 2026-10-07 (item 38): shown from the FROM item, keyed into the TO item -- two slots at one place, the TO's initial content its FROM's (free/scrfromto, 2002/screenmore); **refused**: bad/std2002-screen-fromto-lit (FROM literal with TO) |
| SR 2 | GLOBAL only on a named 01 | **test**: 2002/screenglobal (a contained program DISPLAYs the containing program's screen; implemented 2026-10-07: its references are bound in the declaring program before its procedure division); **refused**: bad/std2002-screen-global-item (on a subordinate entry); a GLOBAL screen's item with a run-time address is **refused** as not implemented |
| SR 3, 5 | level 1-48 for a group, 1-49 for an elementary item | **refused**: level 50 ("bad level 50 in a screen"); a group at 49 is refused by its child's level |
| SR 7 | an elementary item has PICTURE with FROM/TO/USING, or a VALUE, BLANK, ERASE or BELL | **test**: free/scrattr (BELL alone, VALUE alone), free/screrase; a PICTURE with neither is **refused** unless -dialect=gnucobol (BP-G4) |
| SR 7, VALUE SR 15 | PICTURE with a numeric VALUE | **ruling**: refused. 13.17.3 rule 7 names "a PICTURE and a VALUE with a numeric literal", 13.18.63.3 rule 15 allows only an alphanumeric or national literal in the screen section; the narrower rule is kept |
| SR 8 | FULL and JUSTIFIED not together | **refused**: bad/screen-full-just |
| SR 9 | LOCALE with SIGN | **n/a**: PICTURE LOCALE is not implemented |
| SR 10 | no PICTURE with an alphanumeric, boolean or national VALUE: the PICTURE implied | **test**: free/screen, 2002/natscreen |
| SR 10 | not a zero-length literal | **refused** by rule under -std=2014 (bad/std2014-zero-value-nopic is the data-division form); under -std=2002 the literal itself |
| GR 1 (13.17.4) | a clause at the lowest level wins | **test**: free/scr01color (colours), free/scrattr |
| GR 2 | HIGHLIGHT against LOWLIGHT at different levels: the lower wins | **ruling**: both flags are kept and HIGHLIGHT is painted; one attribute is painted per field (below) |
| format 1 | JUSTIFIED, BLANK WHEN ZERO not on a group | **refused**: bad/screen-group-just -- accepted and ignored before |
| format 1 | ERASE, BLANK LINE not on a group | **ruling**: ERASE on a group is taken, applied from its first field, as ACAS writes it on its screens' 01 (free/screrase); BLANK LINE **implemented** 2026-10-07 (2002/screenmore) |
| format 1 | the 01 takes the group clauses | **test**: free/scrattr (AUTO, REQUIRED on the 01), free/scr01color -- only BLANK SCREEN, ERASE and the colours were taken before |
| names | a screen-name is a user-defined word | **refused**: bad/screen-reserved-name -- MOVE and GLOBAL were taken as names |

## The screen clauses

| clause, rule | paraphrase | disposition |
|---|---|---|
| AUTO GR 1-2 | at a group, reaches the input items; ignored on an output item | **test**: free/scrattr, free/screen |
| AUTO GR 4-5 | the last character typed moves on; the last field ends the ACCEPT, status 0000 | **test**: free/posauto, free/scrnumed |
| BACKGROUND/FOREGROUND SR 1 | identifier-1, an unsigned integer item | **test**: 2002/screenmore (FOREGROUND-COLOR from an item, read when the statement runs; implemented 2026-10-07); with OCCURS **refused** as not implemented |
| BACKGROUND/FOREGROUND SR 2 | integer 0-7 | **refused** |
| BACKGROUND/FOREGROUND GR 2-4 | the colour; a group's reaches its items; out of range is the implementor's | **test**: free/poscolor, free/scr01color |
| BELL GR 1 | the tone once at the start of a DISPLAY, however many entries; not for ACCEPT | **test**: free/scrattr (one BEL for DISPLAY and ACCEPT) -- read and dropped before |
| BELL GR 2 | at a group, reaches its items | **test**: free/scrattr (inherited through the 01's rsv bits; implementation shared with BLINK) |
| BLANK GR 1 | BLANK LINE clears the line first | **test**: 2002/screenmore (the line cleared before the field on a DISPLAY; ignored on an ACCEPT, rule 5) |
| BLANK GR 2-4 | BLANK SCREEN clears and homes; with a colour, sets the screen's default colours | **test**: free/screen2 (clear), 2002/screenmore (BACKGROUND-COLOR on the BLANK SCREEN entry: the clear and every field without a colour of its own take it; implemented 2026-10-07) |
| BLANK GR 5 | BLANK ignored during an ACCEPT | **test**: free/scrrulings (ruled 2026-10-07, standard-queue item 50: the ACCEPT paints without clearing, the DISPLAY after it clears). GnuCOBOL and Micro Focus clear on the ACCEPT too, kept under -dialect=gnucobol and -dialect=mf (free/gnu-scrrulings; libcob's blank_screen 2) -- ACAS ACCEPTs screens it never DISPLAYed |
| BLINK GR 1-2 | the characters blink | **test**: free/scrattr (SGR 5) -- read and dropped before |
| COLUMN SR 12 | identifier-1 | **test**: 2002/screenmore (COLUMN from an item, PLUS and MINUS from the slot before computed when the statement runs); **refused**: bad/std2002-screen-line-ident-signed (a signed item) |
| COLUMN SR 13, LINE SR 13 | no PLUS or MINUS on the first item | **refused**: bad/screen-plus-first -- accepted before |
| COLUMN, LINE: MINUS | MINUS | **test**: 2002/screenmore (COLUMN MINUS 3: that many back from the slot before's last column; LINE MINUS likewise); **refused**: bad/std2002-screen-col-minus-first |
| COLUMN GR 13-14 | the column within the screen record | **test**: free/screen |
| COLUMN GR 15 | PLUS n from the end of the item before: PLUS 1 is immediately after | **test**: free/scrrulings (ruled 2026-10-07, item 50: PLUS n leaves n - 1 blank columns); GnuCOBOL and Micro Focus count PLUS n as n columns beyond the end, kept under their switches (free/gnu-scrrulings); **refused**: PLUS 0 |
| COLUMN GR 16-17, LINE GR 13 | COLUMN 1 when only LINE is given; neither: the line before, immediately after the item before | **test**: free/scrlinecol -- LINE alone on the line of the item before used to follow it, not start at column 1 |
| COLUMN GR 18-19, LINE GR 14 | column 0, or past the terminal: EC-SCREEN-STARTING-COLUMN / -LINE-NUMBER, the item left out | **test**: 2002/screenmore (a field at line 90 left out, DISPLAY's ON EXCEPTION taken; implemented 2026-10-07: libcob notes the conditions (scr_field_out), the compiler raises those checked after the statement); column 0 paints at column 1 and is noted |
| ERASE SR 1-2 | EOL, EOS | **test**: free/screrase |
| ERASE GR 1-2 | clears from the item's position on DISPLAY; ignored on ACCEPT | **test**: free/screrase (positioned ACCEPT keeps its ERASE: BP-E7) |
| FROM SR 2 | a MOVE-compatible sender | **test**: free/screen; category clashes go through the MOVE rules at run time |
| FROM SR 3 | under OCCURS, the identifier unsubscripted | **test**: 2002/screenmore (USING elem OCCURS 3: each occurrence its element, the OCCURS supplying the last subscript; implemented 2026-10-07); **refused**: bad/std2002-screen-occurs-noelem (not a table element) |
| FROM SR 5 | not a zero-length literal | **refused** by rule (13.18.25.3 rule 5) |
| FROM literal | FROM literal-1 | **test**: free/scrpicval (alphanumeric), 2002/screenmore (a numeric literal through a numeric and a numeric-edited PICTURE: 7.5 as `  7.50`, 42 as `042`; implemented 2026-10-07); **refused**: bad/std2002-screen-from-numlit-x (a numeric literal with PICTURE X) |
| FULL GR 1 | at a group, reaches the items that are not JUSTIFIED | **test**: free/scrclauses |
| FULL GR 3a-b | text: all spaces, or the first and last positions filled | **test**: scredit_test (se_full_ok), free/scrclauses |
| FULL GR 3c | numeric: zero, or no suppressed digit position | **test**: free/scrclauses (12 refused, 123 taken) |
| FULL GR 3d | boolean | **gap**: no boolean screen input |
| FULL GR 2, 5, 6 | only once the cursor has been in it; not for a function key; with REQUIRED: filled | **test**: free/scrkeys, free/screen2 |
| HIGHLIGHT, LOWLIGHT, REVERSE-VIDEO, UNDERLINE | each attribute; at a group, reaches the items | **test**: free/screen4, free/scrattr. One attribute is painted per field, by rank (REVERSE-VIDEO, UNDERLINE, HIGHLIGHT, LOWLIGHT, BLINK): the term service's cell carries one attribute, so the clauses written together (13.17.2 allows it) cannot all show -- a **gap** of the service, not of the compiler |
| JUSTIFIED (screen) | the data to the right | **test**: free/scrclauses (on leaving the field); national: **gap** |
| LINE SR 12 | identifier-1 | **test**: 2002/screenmore (LINE from an item; a LINE PLUS integer after it computed when the statement runs) |
| LINE GR 11-13 | the line within the screen record; omitted: the line before | **test**: free/screen2 |
| OCCURS SR 11-12 | format 1, no phrases; two dimensions at most | **test**: free/scroccurs (one dimension, VALUE items) |
| OCCURS SR 13 | OCCURS over FROM, TO or USING: a matching table | **test**: 2002/screenmore (implemented 2026-10-07); OCCURS on a group stays a **gap**, as does an identifier for LINE, COLUMN or a colour under OCCURS |
| OCCURS SR 14-15 | with LINE and COLUMN, PLUS or MINUS in one of them | **refused**: bad/screen-occurs-absolute -- accepted before |
| OCCURS GR 5-6 | each occurrence placed by the same clauses | **test**: free/scroccurs |
| REQUIRED GR 1-7 | input only; not until entered; refuses every move out; nonzero / non-space; not for a function key; group | **test**: free/screen2, free/scrattr; on an output item it is allowed and does nothing (it was refused) |
| SECURE GR 1-3 | input only; the keyed data not shown | **test**: free/screen2. **ruling**: an asterisk is shown for each character (docs/plans/screen-input.md, decision 4), and a SECURE field painted by DISPLAY shows asterisks too |
| SECURE GR 4 | whether the cursor moves | **ruling**: it moves |
| SIGN (screen) | SIGN on a signed numeric item; SEPARATE takes a column | **test**: free/scrclauses; **refused** otherwise: bad/screen-sign |
| TO SR 1-2, USING SR 1-3 | MOVE-compatible both ways; no variable-length group | **test**: free/screen, free/scrnumed |
| TO, USING SR 3-4 | under OCCURS | **test**: 2002/screenmore (with FROM SR 3) |
| USAGE SR 17, 20 | DISPLAY or NATIONAL only; PICTURE N with NATIONAL | **refused**; NATIONAL on a picture that is not N is a **gap** |
| VALUE SR 15 | an alphanumeric or national literal, not a figurative constant | **refused**, with the rule (the message used to say "a nonnumeric literal") |

## ACCEPT and DISPLAY of a screen (14.9.1 format 3, 14.9.11 format 2)

| rule | paraphrase | disposition |
|---|---|---|
| ACCEPT SR 4 | a screen with FROM or VALUE items and no TO or USING is not ACCEPTed | **refused**: bad/accept-output-screen -- it compiled and ended 8000 |
| ACCEPT SR 5, DISPLAY SR 3 | LINE and COLUMN phrases: unsigned integers | **test**: 2002/screenmore (`DISPLAY scr AT LINE 2 COLUMN 5`: every field offset; implemented 2026-10-07, literals or items, AT rrcc too), 2002/screenglobal |
| ACCEPT GR 13 | initial values: FROM, USING, VALUE; otherwise spaces, or zeros for numeric | **test**: free/scrclauses (a numeric TO field shows zeros) |
| ACCEPT GR 15-17 | the screen record at line 1 column 1 unless placed | **test**: free/screen |
| ACCEPT GR 18 | the cursor at the CURSOR locator if it is in an input field, else the first field | **test**: free/scrcursor |
| ACCEPT GR 19 | attributes while editing | **test**: free/screen4 |
| ACCEPT GR 20-21 | keyed data consistent with the PICTURE; the implementor chooses when, and whether to refuse | **ruling**: refused at the keystroke, so 8001 cannot arise (docs/screen.md); scredit_test, scrnum_test |
| ACCEPT GR 22 | the transfer: NUMVAL for numeric, NUMVAL-C for numeric-edited, MOVE otherwise; overlapping fields EC-SCREEN-FIELD-OVERLAP | **test**: free/scrnumed, free/screen2; the overlap condition on an ACCEPT is raised when checked (2026-10-07, with DISPLAY GR 13) |
| ACCEPT GR 23, 12.3.7 GR 15 | the CURSOR item set to where the cursor stood | **test**: free/scrcursor |
| ACCEPT GR 24-25 | ON EXCEPTION for a function key, an unsuccessful ACCEPT or EC-SCREEN; NOT ON EXCEPTION otherwise | **test**: free/scrcursor (0000, 1002, 8000); the EC-SCREEN conditions an ACCEPT's painting finds are raised when checked (2026-10-07) |
| DISPLAY GR 13 | the items in order; overlap: EC-SCREEN-FIELD-OVERLAP, the later wins | **test**: free/screen; 2002/screenmore (two fields on one cell: ON EXCEPTION taken, the later painted; the cells each statement paints are kept, libcob scr_claim) |
| DISPLAY format 2 | ON EXCEPTION / NOT ON EXCEPTION | **test**: 2002/screenmore (implemented 2026-10-07: taken for a condition the screen raised, EC-SCREEN-STARTING-COLUMN, -LINE-NUMBER, -FIELD-OVERLAP) |
| DISPLAY GR 18-19 | EC-SCREEN conditions make it unsuccessful | **test**: 2002/screenmore; EC-SCREEN-ITEM-TRUNCATED is a **gap** (nothing is truncated here) |

## SPECIAL-NAMES: CURSOR and CRT STATUS (12.3.7)

| rule | paraphrase | disposition |
|---|---|---|
| SR 29 | CURSOR: six digits, or two three-digit items, in working- or local-storage | **refused** for any other size: bad/cursor-item; **test**: free/scrcursor |
| SR 30 | CRT STATUS: alphanumeric, four characters | **test**: free/scrrulings (PIC X(4); ruled 2026-10-07, item 50); **refused**: bad/crt-status-numeric (PIC 9(4), what GnuCOBOL's and Micro Focus's programs declare, taken under -dialect=gnucobol and -dialect=mf: free/gnu-scrrulings; BP-G2's implicit COB-CRT-STATUS is one), bad/crt-status-three and gnu-crt-status-three (Micro Focus's three-byte item, under -dialect=mf only) |
| GR 16, 9.2.3 | the CRT status values 0000, 1xxx, 2xxx, 8000, 8001, 9xxx | **test**: free/scrcursor, free/gnu-crtstatus; 8001 cannot arise (above) |

## Open, from this sweep

Most of the gaps of the sweep were closed 2026-10-07 (standard-queue
item 38: identifiers for LINE, COLUMN and the colours, MINUS, the AT
phrase on ACCEPT and DISPLAY of a screen, DISPLAY's ON EXCEPTION and the
EC-SCREEN conditions, FROM with TO, a FROM numeric literal, BLANK LINE,
BLANK SCREEN's default colours, OCCURS over FROM, TO and USING items,
GLOBAL, SET ... ATTRIBUTE). Still open: OCCURS on a group, and an
identifier for LINE, COLUMN or a colour under OCCURS; combined display
attributes (one per cell in the term service); boolean screen input;
JUSTIFIED and USAGE NATIONAL on national pictures; EC-SCREEN-ITEM-
TRUNCATED; a GLOBAL screen's item with a run-time address.

## SET screen-name ATTRIBUTE (14.9.39 format 6)

| rule | paraphrase | disposition |
|---|---|---|
| format 6 | BELL, BLINK, HIGHLIGHT, LOWLIGHT, REVERSE-VIDEO, UNDERLINE, each ON or OFF, of a screen-name or a group | **test**: 2002/screenglobal (UNDERLINE OFF HIGHLIGHT ON before the second DISPLAY; implemented 2026-10-07: every slot's bits changed in place) |
| SR 15, 16 | an attribute once; not HIGHLIGHT with LOWLIGHT | **refused**: bad/std2002-screen-set-attr-twice, -hl |

The three rulings of 2026-10-07 (standard-queue item 50) -- BLANK SCREEN
during an ACCEPT (13.18.7.3 rule 5), the PLUS column count (13.18.14.4
rule 15), the CRT STATUS item (12.3.7.3 rule 30) -- follow the text by
default, the dialects' behaviour under -dialect=gnucobol and -dialect=mf
(docs/behavior-points.md, "dialect behaviours"); the screen tests that
relied on the old count or clear were re-recorded (screen2-5, scrcursor,
screen, natscreen: a DISPLAY's clear now comes from the DISPLAY alone).
