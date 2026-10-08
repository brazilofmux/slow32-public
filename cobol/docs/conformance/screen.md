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
| SR 1 | one of FROM, TO, USING, VALUE (FROM with TO is one element) | **refused**: a second one; FROM with TO is a **gap** (bad/screen-from-to) -- it compiled as TO alone |
| SR 2 | GLOBAL only on a named 01 | **gap**: GLOBAL on a screen is refused as not implemented |
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
| format 1 | ERASE, BLANK LINE not on a group | **ruling**: ERASE on a group is taken, applied from its first field, as ACAS writes it on its screens' 01 (free/screrase); BLANK LINE is a **gap** everywhere |
| format 1 | the 01 takes the group clauses | **test**: free/scrattr (AUTO, REQUIRED on the 01), free/scr01color -- only BLANK SCREEN, ERASE and the colours were taken before |
| names | a screen-name is a user-defined word | **refused**: bad/screen-reserved-name -- MOVE and GLOBAL were taken as names |

## The screen clauses

| clause, rule | paraphrase | disposition |
|---|---|---|
| AUTO GR 1-2 | at a group, reaches the input items; ignored on an output item | **test**: free/scrattr, free/screen |
| AUTO GR 4-5 | the last character typed moves on; the last field ends the ACCEPT, status 0000 | **test**: free/posauto, free/scrnumed |
| BACKGROUND/FOREGROUND SR 1 | identifier-1, an unsigned integer item | **gap**: only an integer is taken |
| BACKGROUND/FOREGROUND SR 2 | integer 0-7 | **refused** |
| BACKGROUND/FOREGROUND GR 2-4 | the colour; a group's reaches its items; out of range is the implementor's | **test**: free/poscolor, free/scr01color |
| BELL GR 1 | the tone once at the start of a DISPLAY, however many entries; not for ACCEPT | **test**: free/scrattr (one BEL for DISPLAY and ACCEPT) -- read and dropped before |
| BELL GR 2 | at a group, reaches its items | **test**: free/scrattr (inherited through the 01's rsv bits; implementation shared with BLINK) |
| BLANK GR 1 | BLANK LINE clears the line first | **gap**: BLANK LINE is refused as not implemented |
| BLANK GR 2-4 | BLANK SCREEN clears and homes; with a colour, sets the screen's default colours | **test** (clear): free/screen2; the default-colour part is a **gap** |
| BLANK GR 5 | BLANK ignored during an ACCEPT | **test**: free/scrrulings (ruled 2026-10-07, standard-queue item 50: the ACCEPT paints without clearing, the DISPLAY after it clears). GnuCOBOL and Micro Focus clear on the ACCEPT too, kept under -dialect=gnucobol and -dialect=mf (free/gnu-scrrulings; libcob's blank_screen 2) -- ACAS ACCEPTs screens it never DISPLAYed |
| BLINK GR 1-2 | the characters blink | **test**: free/scrattr (SGR 5) -- read and dropped before |
| COLUMN SR 12 | identifier-1 | **gap**: only an integer is taken |
| COLUMN SR 13, LINE SR 13 | no PLUS or MINUS on the first item | **refused**: bad/screen-plus-first -- accepted before |
| COLUMN, LINE: MINUS | MINUS | **gap**: "expected a number" |
| COLUMN GR 13-14 | the column within the screen record | **test**: free/screen |
| COLUMN GR 15 | PLUS n from the end of the item before: PLUS 1 is immediately after | **test**: free/scrrulings (ruled 2026-10-07, item 50: PLUS n leaves n - 1 blank columns); GnuCOBOL and Micro Focus count PLUS n as n columns beyond the end, kept under their switches (free/gnu-scrrulings); **refused**: PLUS 0 |
| COLUMN GR 16-17, LINE GR 13 | COLUMN 1 when only LINE is given; neither: the line before, immediately after the item before | **test**: free/scrlinecol -- LINE alone on the line of the item before used to follow it, not start at column 1 |
| COLUMN GR 18-19, LINE GR 14 | column 0, or past the terminal: EC-SCREEN-STARTING-COLUMN / -LINE-NUMBER, the item left out | **gap**: no EC-SCREEN condition is raised; column 0 paints at column 1 |
| ERASE SR 1-2 | EOL, EOS | **test**: free/screrase |
| ERASE GR 1-2 | clears from the item's position on DISPLAY; ignored on ACCEPT | **test**: free/screrase (positioned ACCEPT keeps its ERASE: BP-E7) |
| FROM SR 2 | a MOVE-compatible sender | **test**: free/screen; category clashes go through the MOVE rules at run time |
| FROM SR 3 | under OCCURS, the identifier unsubscripted | **gap**: OCCURS on a FROM, TO or USING item is not implemented |
| FROM SR 5 | not a zero-length literal | **refused** by rule (13.18.25.3 rule 5) |
| FROM literal | FROM literal-1 | **test**: free/scrpicval (alphanumeric); a numeric literal is a **gap** |
| FULL GR 1 | at a group, reaches the items that are not JUSTIFIED | **test**: free/scrclauses |
| FULL GR 3a-b | text: all spaces, or the first and last positions filled | **test**: scredit_test (se_full_ok), free/scrclauses |
| FULL GR 3c | numeric: zero, or no suppressed digit position | **test**: free/scrclauses (12 refused, 123 taken) |
| FULL GR 3d | boolean | **gap**: no boolean screen input |
| FULL GR 2, 5, 6 | only once the cursor has been in it; not for a function key; with REQUIRED: filled | **test**: free/scrkeys, free/screen2 |
| HIGHLIGHT, LOWLIGHT, REVERSE-VIDEO, UNDERLINE | each attribute; at a group, reaches the items | **test**: free/screen4, free/scrattr. One attribute is painted per field (REVERSE-VIDEO, then UNDERLINE, HIGHLIGHT, LOWLIGHT, BLINK): the term service's shadow keeps one per cell -- a **gap** for combinations |
| JUSTIFIED (screen) | the data to the right | **test**: free/scrclauses (on leaving the field); national: **gap** |
| LINE SR 12 | identifier-1 | **gap**: only an integer is taken |
| LINE GR 11-13 | the line within the screen record; omitted: the line before | **test**: free/screen2 |
| OCCURS SR 11-12 | format 1, no phrases; two dimensions at most | **test**: free/scroccurs (one dimension, VALUE items) |
| OCCURS SR 13 | OCCURS over FROM, TO or USING: a matching table | **gap**: refused as not implemented; OCCURS on a group likewise |
| OCCURS SR 14-15 | with LINE and COLUMN, PLUS or MINUS in one of them | **refused**: bad/screen-occurs-absolute -- accepted before |
| OCCURS GR 5-6 | each occurrence placed by the same clauses | **test**: free/scroccurs |
| REQUIRED GR 1-7 | input only; not until entered; refuses every move out; nonzero / non-space; not for a function key; group | **test**: free/screen2, free/scrattr; on an output item it is allowed and does nothing (it was refused) |
| SECURE GR 1-3 | input only; the keyed data not shown | **test**: free/screen2. **ruling**: an asterisk is shown for each character (docs/plans/screen-input.md, decision 4), and a SECURE field painted by DISPLAY shows asterisks too |
| SECURE GR 4 | whether the cursor moves | **ruling**: it moves |
| SIGN (screen) | SIGN on a signed numeric item; SEPARATE takes a column | **test**: free/scrclauses; **refused** otherwise: bad/screen-sign |
| TO SR 1-2, USING SR 1-3 | MOVE-compatible both ways; no variable-length group | **test**: free/screen, free/scrnumed |
| TO, USING SR 3-4 | under OCCURS | **gap** (with OCCURS SR 13) |
| USAGE SR 17, 20 | DISPLAY or NATIONAL only; PICTURE N with NATIONAL | **refused**; NATIONAL on a picture that is not N is a **gap** |
| VALUE SR 15 | an alphanumeric or national literal, not a figurative constant | **refused**, with the rule (the message used to say "a nonnumeric literal") |

## ACCEPT and DISPLAY of a screen (14.9.1 format 3, 14.9.11 format 2)

| rule | paraphrase | disposition |
|---|---|---|
| ACCEPT SR 4 | a screen with FROM or VALUE items and no TO or USING is not ACCEPTed | **refused**: bad/accept-output-screen -- it compiled and ended 8000 |
| ACCEPT SR 5, DISPLAY SR 3 | LINE and COLUMN phrases: unsigned integers | **gap**: the screen record placed elsewhere than line 1, column 1 is refused (AT 0101 is taken) |
| ACCEPT GR 13 | initial values: FROM, USING, VALUE; otherwise spaces, or zeros for numeric | **test**: free/scrclauses (a numeric TO field shows zeros) |
| ACCEPT GR 15-17 | the screen record at line 1 column 1 unless placed | **test**: free/screen |
| ACCEPT GR 18 | the cursor at the CURSOR locator if it is in an input field, else the first field | **test**: free/scrcursor |
| ACCEPT GR 19 | attributes while editing | **test**: free/screen4 |
| ACCEPT GR 20-21 | keyed data consistent with the PICTURE; the implementor chooses when, and whether to refuse | **ruling**: refused at the keystroke, so 8001 cannot arise (docs/screen.md); scredit_test, scrnum_test |
| ACCEPT GR 22 | the transfer: NUMVAL for numeric, NUMVAL-C for numeric-edited, MOVE otherwise; overlapping fields EC-SCREEN-FIELD-OVERLAP | **test**: free/scrnumed, free/screen2; the overlap condition is a **gap** |
| ACCEPT GR 23, 12.3.7 GR 15 | the CURSOR item set to where the cursor stood | **test**: free/scrcursor |
| ACCEPT GR 24-25 | ON EXCEPTION for a function key, an unsuccessful ACCEPT or EC-SCREEN; NOT ON EXCEPTION otherwise | **test**: free/scrcursor (0000, 1002, 8000); EC-SCREEN is a **gap** |
| DISPLAY GR 13 | the items in order; overlap: EC-SCREEN-FIELD-OVERLAP, the later wins | **test**: free/screen; the condition is a **gap** |
| DISPLAY format 2 | ON EXCEPTION / NOT ON EXCEPTION | **gap**: refused by the parser (the phrases are not read) |
| DISPLAY GR 18-19 | EC-SCREEN conditions make it unsuccessful | **gap** |

## SPECIAL-NAMES: CURSOR and CRT STATUS (12.3.7)

| rule | paraphrase | disposition |
|---|---|---|
| SR 29 | CURSOR: six digits, or two three-digit items, in working- or local-storage | **refused** for any other size: bad/cursor-item; **test**: free/scrcursor |
| SR 30 | CRT STATUS: alphanumeric, four characters | **test**: free/scrrulings (PIC X(4); ruled 2026-10-07, item 50); **refused**: bad/crt-status-numeric (PIC 9(4), what GnuCOBOL's and Micro Focus's programs declare, taken under -dialect=gnucobol and -dialect=mf: free/gnu-scrrulings; BP-G2's implicit COB-CRT-STATUS is one), bad/crt-status-three and gnu-crt-status-three (Micro Focus's three-byte item, under -dialect=mf only) |
| GR 16, 9.2.3 | the CRT status values 0000, 1xxx, 2xxx, 8000, 8001, 9xxx | **test**: free/scrcursor, free/gnu-crtstatus; 8001 cannot arise (above) |

## Open, from this sweep

The gaps above, in one list: GLOBAL on a screen; colours, LINE and
COLUMN from an identifier; MINUS; LINE and COLUMN phrases on ACCEPT and
DISPLAY of a screen; ON EXCEPTION on DISPLAY of a screen; OCCURS on a
group and over FROM, TO or USING items; FROM with TO in one entry; a
FROM numeric literal; BLANK LINE; BLANK SCREEN's default colours; the
EC-SCREEN conditions; combined display attributes; boolean screen input;
JUSTIFIED and USAGE NATIONAL on national pictures.

The three rulings of 2026-10-07 (standard-queue item 50) -- BLANK SCREEN
during an ACCEPT (13.18.7.3 rule 5), the PLUS column count (13.18.14.4
rule 15), the CRT STATUS item (12.3.7.3 rule 30) -- follow the text by
default, the dialects' behaviour under -dialect=gnucobol and -dialect=mf
(docs/behavior-points.md, "dialect behaviours"); the screen tests that
relied on the old count or clear were re-recorded (screen2-5, scrcursor,
screen, natscreen: a DISPLAY's clear now comes from the DISPLAY alone).
