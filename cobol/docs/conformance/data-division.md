# The DATA DIVISION: its sections and the data description entry

Swept 2026-09-30. X3.23-1985: IV-34 (the sections' order), VI-18 to
VI-23 (WORKING-STORAGE, the data description entry, FILLER), X-19 to
X-26 (LINKAGE, the entry in the Inter-Program Communication module,
EXTERNAL, GLOBAL, the procedure division header), VII-24 CODE-SET (3.4,
Sequential I-O). 2023: 13.2, 13.5, 13.6, 13.7, 13.10, 13.11, 13.13,
13.16, 13.18.13, 13.18.20, 13.18.22, 13.18.27.

Rules that other pages sweep are named there: level-numbers, JUSTIFIED,
SIGN and SYNCHRONIZED (clauses.md), PICTURE and BLANK WHEN ZERO's own
rules (picture.md), USAGE (usage.md), OCCURS (occurs.md), REDEFINES
(redefines.md), RENAMES (renames.md), VALUE (value.md), and TYPEDEF and
TYPE (docs/typedef.md).

## The sections

| rule | paraphrase | disposition |
|---|---|---|
| 85 IV-34; 2023 13.2.1 | the sections in order (FILE, WORKING-STORAGE, LOCAL-STORAGE, LINKAGE, REPORT, SCREEN), each at most once | **refused**: bad/section-order, bad/section-twice -- both were accepted before this sweep |
| 85 VI-18 (5.2); 2023 13.5.3 rule 1 | WORKING-STORAGE in a program (and OO definitions) | **test**: every program; the OO placements **n/a** (object orientation is ruled out, docs/refusals.md) |
| 2023 13.5.4 rules 1-3 | static data, or initial data in an INITIAL program; initialized by VALUE | **test**: free/nested (an INITIAL program's fresh data) and the VALUE tests |
| 85 VI-19 (5.2.4) | an item without VALUE starts undefined | **ruling**: WORKING-STORAGE starts as each item's INITIALIZE would leave it (spaces, zeros), as GnuCOBOL does; a program that reads one before setting it gets that, not garbage |
| 2023 13.6.3 rule 1, 13.6.4 rules 1-2 | LOCAL-STORAGE: automatic data, fresh in each activation (2002) | **test**: 2002/localfresh, 2002/recfact; refused under -std=85 ("the LOCAL-STORAGE SECTION is COBOL 2002") |
| 85 X-19 (4.1); 2023 13.7.3 rule 1 | LINKAGE describes what a caller passes | **test**: fixed/callsub and every subprogram |
| 85 X-25 and X-26 rule 4; 2023 13.7.3 rule 4 | a LINKAGE item is referenced only as, under, or redefining a USING operand | **refused** under -std=85: bad/linkage-ref (accepted before this sweep). **ruling** under -std=2002: accepted, since SET ADDRESS OF gives a LINKAGE record its storage there, the way IBM programs use it |
| 2002 and 2023 13.7.3 rule 5 | a function's formal parameter is not a receiving operand | **gap**: not refused yet (ISSUES-118); receiving operands have no common point to check |
| 85 VALUE rule 5.15.6(1); 2023 13.7.4 rule 5 | VALUE in LINKAGE: 85 only on a level 88; 2002 on, applied only by INITIALIZE | **refused** under -std=85 (bad/value-rules); accepted under -std=2002 |
| 2023 13.10 | the constant entry (01 name CONSTANT AS ...) | **gap**: bad/std2002-constant names it -- it was "unexpected 'constant'" |
| 2023 13.11 | a record description is an 01 entry and its subordinates | **test**: everywhere |
| 85 VI-18 (5.2.1); 2023 13.13, 13.16.3 rule 2 | a level 77 entry has a data-name and a PICTURE (or USAGE INDEX) | **refused**: bad/entry-rules (the name; accepted before this sweep), bad/no-picture (the PICTURE) |

## The data description entry (85 VI-20 to VI-21; 2023 13.16)

| rule | paraphrase | disposition |
|---|---|---|
| 85 SR1; 2023 rule 1 | level-numbers | **refused** (clauses.md) |
| 85 SR2; 2023 rule 4 | the data-name or FILLER first, REDEFINES immediately after it, the rest in any order | **refused**: bad/entry-rules -- REDEFINES after another clause was accepted |
| 2002 13.13.2 and 2023 rule 4 | TYPEDEF immediately after the data-name | **refused**: bad/std2002-typedef-place (accepted before this sweep) |
| 85 SR3; 2023 rule 8 | a PICTURE for every elementary item but an index item and a RENAMES subject; none for those (or the native usages) | **refused**: bad/no-picture, and "USAGE INDEX takes no PICTURE" (usage.md) |
| 2023 rule 9 (2002 13.13.2 rule 14) | a PICTURE implied by an alphanumeric, boolean or national VALUE literal | **gap**: bad/std2002-implied-pic names it |
| 2023 rule 10 | no VALUE on index, pointer, object items | **refused** ("a USAGE INDEX item takes no VALUE clause") |
| 85 GR1; 2023 rule 11 | PICTURE, JUSTIFIED, BLANK WHEN ZERO (85: and SYNCHRONIZED) only for an elementary item | **refused**: bad/group-picture, bad/entry-rules (BLANK WHEN ZERO on a group, accepted before this sweep); JUSTIFIED and SYNCHRONIZED in clauses.md |
| 2023 rules 3, 6, 13 | CONSTANT RECORD | **n/a** under -std=2002: COBOL 2014, refused naming it |
| 2023 rule 12 | SAME AS | **gap**: refused naming it (2002) |
| 2023 rules 14, 15 | TYPE and TYPEDEF combinations | docs/typedef.md |
| 2023 rule 16 | BASED: level 01 or 77, in WORKING-STORAGE, LOCAL-STORAGE or LINKAGE | **refused** for the level; BASED in LOCAL-STORAGE is a **gap** |
| 2023 rules 17, 18 | ANY LENGTH; DYNAMIC LENGTH | **gap** (ANY LENGTH, 2002) and **n/a** (DYNAMIC LENGTH, 2014): both refused naming them |
| 2023 rule 19 | LOCALE in PICTURE | **gap**: refused naming it |
| 2023 rules 20, 21 | PRESENT WHEN, PROPERTY | PRESENT WHEN is Report Writer's (reportwriter.md); PROPERTY **n/a** (object orientation) |
| 85 SR4; 2023 rule 22 | THRU and THROUGH | **test**: free/renames, the 88 VALUE tests |
| 85 GR2; 2023 rules 23, 24 | condition-name entries follow their item | **test**: everywhere |
| 85 GR2a; 2023 rule 24a | not on another 88 | **ruling**: consecutive 88s all belong to the item before them, which is what a second 88 means |
| 85 GR2b; 2023 rule 24b | not on a level 66 entry | **refused**: bad/condname-66 -- before this sweep the message was a VALUE length complaint |
| 85 GR2c; 2023 rule 24c, d | not on a group holding items of another usage than DISPLAY, or JUSTIFIED or SYNCHRONIZED ones | **ruling**: taken, as behavior point BP-E23 (docs/behavior-points.md). Majesty's GnuCOBOL-era records carried an `88 ... VALUE HIGH-VALUES` end-of-file flag over packed fields, and refusing it broke two of its programs; by the user's ruling the point stays, warned under -warn-extensions, and majesty was changed to conform (majesty 5f9cc24) |
| 85 GR2d; 2023 rule 24e | not on an index or pointer item | **refused** (bad/index-88, bad/std2002-pointer-88) |
| 2023 rule 24f, g | not with ANY LENGTH; not on a strongly typed group | ANY LENGTH a **gap**; strong types in docs/typedef.md |
| 2023 rule 24h | not on a variable-length group | **ruling**: 2023 only (not in 2002's list); accepted under -std=2002 |
| 85 GR3 | several 01s under one FD redefine one area | **test**: free/vrec |
| 2023 GR1, 2 | groups under a bit or national group take its GROUP-USAGE | docs/national.md, docs/boolean.md |
| 2023 GR3, 4 | format 3 names conditions; format 4 is VALIDATE's | format 3 **test** (the condition tests); format 4 **gap** (VALIDATE is not implemented) |

## FILLER and the entry name (85 VI-23; 2023 13.18.20)

| rule | paraphrase | disposition |
|---|---|---|
| 85 SR1 | the name first in the entry | **refused** (as above) |
| 85 GR1 | no name means FILLER | **test**: free/corr, free/initrep |
| 85 GR2 | FILLER is never referenced; it may name a conditional variable | **refused** ("'filler' is not declared"); the 88 under FILLER **test** |

## EXTERNAL (85 X-21, X-23; 2023 13.16.3 rules 5, 7, 13.18.22)

| rule | paraphrase | disposition |
|---|---|---|
| 85 IPC SR2, EXTERNAL SR1; 2023 13.18.22.3 rule 1 | a level 01 entry in WORKING-STORAGE (or an FD) | **refused**: bad/external-rules -- a subordinate or LINKAGE entry was accepted |
| 85 EXTERNAL SR2; 2023 rule 2 | each EXTERNAL name described once in a program | **refused**: bad/external-rules |
| 85 EXTERNAL SR3 | no VALUE in or under an EXTERNAL record except on its 88s | **refused** under -std=85: bad/external-rules; 2002 lets INITIALIZE apply one (2023 13.18.63) |
| 2023 rule 3 | EXTERNAL AS literal | **gap**: bad/std2002-external-as names it |
| 85 IPC SR3; 2023 13.16.3 rule 5 | not with REDEFINES (2002 on: nor BASED, TYPEDEF) | **refused**: bad/external-rules (REDEFINES), bad/std2002-external-based (accepted before this sweep) |
| 85 IPC SR5; 2023 13.16.3 rule 7 | a data-name, not FILLER | **refused**: bad/external-rules |
| 85 GR1-4 | one record per name across the run unit; a file connector | **test**: free/external, free/faultwrite |

## GLOBAL (85 X-21, X-24; 2023 13.18.27)

| rule | paraphrase | disposition |
|---|---|---|
| 85 SR1, IPC SR4; 2023 rule 1 | a level 01 entry (85: in the FILE or WORKING-STORAGE SECTION; 2002 on: LOCAL-STORAGE and LINKAGE too), an FD, an RD | **refused**: bad/global-rules -- a subordinate, a 77 or a LINKAGE entry was accepted |
| 85 SR2 | no two GLOBAL items with one name in a DATA DIVISION | **refused** under -std=85: bad/global-rules |
| 85 SR3; 2023 rule 2 | no GLOBAL on files in a SAME RECORD AREA, nor their records | **refused**: bad/global-sra (accepted before this sweep) |
| 85 IPC SR5; 2023 13.16.3 rule 7 | a data-name, not FILLER | **refused**: bad/global-rules |
| 85 GR1-3; 2023 rule 3 | contained programs see a global name, its subordinates and 88s | **test**: free/nested, free/nestuse |

## CODE-SET (85 3.4, Sequential I-O; 2023 13.18.13)

| rule | paraphrase | disposition |
|---|---|---|
| 85 SR1; 2023 rule 3a | the file's data is all DISPLAY, signed items SIGN SEPARATE | **refused**: bad/codeset-comp, bad/codeset-sign |
| 85 SR2 | the alphabet is not given by literals | **refused**: bad/codeset-literal |
| 2023 rules 1-2, 3b | the alphanumeric and national alphabets | **test**: the CODE-SET (EBCDIC) tests; FOR NATIONAL a **gap** |
| line sequential | CODE-SET on a LINE SEQUENTIAL file | **refused**: bad/codeset-lineseq |

CCVS-85, the Open Systems suite and majesty trip none of the refusals
added here. Majesty tripped GR2c, which is why it is a behavior point;
its copybooks now conform, and -warn-extensions is silent on them.
