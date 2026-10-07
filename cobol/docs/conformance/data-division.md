# The DATA DIVISION: its sections and the data description entry

Swept 2026-09-30. X3.23-1985: IV-34 (the sections' order), VI-18 to
VI-23 (WORKING-STORAGE, the data description entry, FILLER), X-19 to
X-26 (LINKAGE, the entry in the Inter-Program Communication module,
EXTERNAL, GLOBAL, the procedure division header), VII-24 CODE-SET (3.4,
Sequential I-O). 2023: 13.2, 13.5, 13.6, 13.7, 13.10, 13.11, 13.13,
13.16, 13.18.2, 13.18.13, 13.18.20, 13.18.22, 13.18.27.

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
| 2023 13.10.2 | 01 name CONSTANT [IS GLOBAL] AS ... (2002 13.9; implemented 2026-10-01, ISSUES-120) | **test**: 2002/constent, 2002/constdup; refused under -std=85 (bad/constant-85). Without AS, GnuCOBOL's form, it is taken as BP-E25 (warn/ext-std2002-constant-noas) |
| 2023 13.10.3 rule 1; 13.10.4 rules 1-2 | a single numeric literal is a literal; the name stands for it, of its class and category | **test**: 2002/constent (`-2.5` with its scale, a string, a hexadecimal literal). Done as the rule words it: the rest of the program's tokens have the name replaced by the literal |
| 2023 13.10.3 rule 2 | the name wherever a literal of its class may be; an integer one as a PICTURE repetition | **test**: 2002/constent (VALUE, OCCURS, PICTURE, MOVE, DISPLAY, a relation, a subscript); **refused**: bad/std2002-constant-pic (not a positive integer). The compiler-directive exception is **gap** with the directives' compilation variables |
| 2023 13.10.3 rule 3 | data-name's subscripts are literals | **test**: 2002/constdup (none, an element: every occurrence has one size); a data-name subscript is refused naming the rule. GnuCOBOL gives the whole table either way (docs/oracles.md) |
| 2023 13.10.3 rules 4-5 | no circular definition | **ruling**: none can arise -- the name is replaced only after its own entry, and a LENGTH OF value only after the DATA DIVISION is laid out; a LENGTH OF constant used inside that DATA DIVISION is **gap** (bad/std2002-constant-length-early), a contained program's use is filled (2002/constdup) |
| 2023 13.10.3 rule 6 | no figurative constant | **refused**: bad/std2002-constant-figurative |
| 2023 13.10.3 rule 7; 7.3.6; 13.10.4 rules 3-4 | arithmetic-expression-1 at compile time, an integer | **test**: 2002/constent (`2 * (max-keys + 1) - 4`); **refused**: bad/std2002-constant-expr (7 / 2). **ruling** (7.3.6.2 rule 2 and 7.3.6.3 rule 2 leave it to the implementor): exact fractions over literals of at most 18 digits, refused past that; exponentiation refused (rule 1a) |
| 2023 13.10.3 rule 8 | FROM compilation-variable-name | **gap**: refused naming >>DEFINE, which is not implemented |
| 2023 13.10.3 rule 9 | the same name again only with the same specification | **test**: 2002/constdup; **refused**: bad/std2002-constant. A data item of the name is refused too (bad/std2002-constant-dataname) |
| 2023 13.10.3 rules 10, 12 | not ANY LENGTH, not dynamic-length | **n/a**: a LENGTH OF constant takes an item described before the DATA DIVISION ends, and an ANY LENGTH item's length is its argument's, at run time -- **gap** as a LENGTH OF operand; dynamic length is 2014, refused where declared |
| 2023 13.10.3 rule 11 | constant entries in the REPORT and SCREEN SECTIONs | **gap**: those sections' parsers do not take one yet |
| 2023 13.10.4 rules 5-6 | BYTE-LENGTH OF bytes, LENGTH OF characters, the maximum for an occurs-depending group | **test**: 2002/constent, 2002/constdup (a national item: 3 and 6) |
| 2023 13.11 | a record description is an 01 entry and its subordinates | **test**: everywhere |
| 85 VI-18 (5.2.1); 2023 13.13, 13.16.3 rule 2 | a level 77 entry has a data-name and a PICTURE (or USAGE INDEX) | **refused**: bad/entry-rules (the name; accepted before this sweep), bad/no-picture (the PICTURE) |

## The data description entry (85 VI-20 to VI-21; 2023 13.16)

| rule | paraphrase | disposition |
|---|---|---|
| 85 SR1; 2023 rule 1 | level-numbers | **refused** (clauses.md) |
| 85 SR2; 2023 rule 4 | the data-name or FILLER first, REDEFINES immediately after it, the rest in any order | **refused**: bad/entry-rules -- REDEFINES after another clause was accepted |
| 2002 13.13.2 and 2023 rule 4 | TYPEDEF immediately after the data-name | **refused**: bad/std2002-typedef-place (accepted before this sweep) |
| 85 SR3; 2023 rule 8 | a PICTURE for every elementary item but an index item and a RENAMES subject; none for those (or the native usages) | **refused**: bad/no-picture, and "USAGE INDEX takes no PICTURE" (usage.md) |
| 2023 rule 9 (2002 13.13.2 rule 14) | a PICTURE implied by an alphanumeric, boolean or national VALUE literal | **test**: 2002/impliedpic (X(n), 1(n), N(n); implemented 2026-10-06, standard-queue item 11; no oracle: GnuCOBOL 4 requires the PICTURE) |
| 2023 rule 10 | no VALUE on index, pointer, object items | **refused** ("a USAGE INDEX item takes no VALUE clause") |
| 85 GR1; 2023 rule 11 | PICTURE, JUSTIFIED, BLANK WHEN ZERO (85: and SYNCHRONIZED) only for an elementary item | **refused**: bad/group-picture, bad/entry-rules (BLANK WHEN ZERO on a group, accepted before this sweep); JUSTIFIED and SYNCHRONIZED in clauses.md |
| 2023 rules 3, 6, 13 | CONSTANT RECORD: not with REDEFINES; level 01 only; not with ANY LENGTH, BASED, BLANK WHEN ZERO, DYNAMIC LENGTH, SYNCHRONIZED, TYPEDEF or the validation clauses, in it or under it; with EXTERNAL a strongly typed TYPE | **implemented** under -std=2014 (standard-queue item 25, 2026-10-07): the section below. **Refused**: bad/std2014-constrec-redefines, -level, -sync, -blank, -external (not implemented: the strongly typed TYPE); BASED by its own level rule; TYPEDEF in a type declaration; DYNAMIC LENGTH and validation are not implemented |
| 2023 rule 12 | SAME AS | **test**: 2002/sameas (the 13.18.49 table below; implemented 2026-10-06, standard-queue item 11; GnuCOBOL 4 agrees) |
| 2023 rules 14, 15 | TYPE and TYPEDEF combinations | docs/typedef.md |
| 2023 rule 16 | BASED: level 01 or 77, in WORKING-STORAGE, LOCAL-STORAGE or LINKAGE | **refused** for the level; **test**: 2002/basedlocal (in LOCAL-STORAGE, its pointer NULL at each activation, 13.18.5.4 rule 2 and 8.6.5; implemented 2026-10-06; GnuCOBOL 4 keeps the outer activation's address: docs/oracles.md) |
| 2023 rules 17, 18 | ANY LENGTH; DYNAMIC LENGTH | ANY LENGTH implemented 2026-10-01 (13.18.2 below); **n/a** (DYNAMIC LENGTH, 2014): refused naming it |
| 2023 rule 19 | LOCALE in PICTURE | **gap**: refused naming it |
| 2023 rules 20, 21 | PRESENT WHEN, PROPERTY | PRESENT WHEN is Report Writer's (reportwriter.md); PROPERTY **n/a** (object orientation) |
| 85 SR4; 2023 rule 22 | THRU and THROUGH | **test**: free/renames, the 88 VALUE tests |
| 85 GR2; 2023 rules 23, 24 | condition-name entries follow their item | **test**: everywhere |
| 85 GR2a; 2023 rule 24a | not on another 88 | **ruling**: consecutive 88s all belong to the item before them, which is what a second 88 means |
| 85 GR2b; 2023 rule 24b | not on a level 66 entry | **refused**: bad/condname-66 -- before this sweep the message was a VALUE length complaint |
| 85 GR2c; 2023 rule 24c, d | not on a group holding items of another usage than DISPLAY, or JUSTIFIED or SYNCHRONIZED ones | **ruling**: taken, as behavior point BP-E23 (docs/behavior-points.md). Majesty's GnuCOBOL-era records carried an `88 ... VALUE HIGH-VALUES` end-of-file flag over packed fields, and refusing it broke two of its programs; by the user's ruling the point stays, warned under -warn-extensions, and majesty was changed to conform (majesty 1dfaa92) |
| 85 GR2d; 2023 rule 24e | not on an index or pointer item | **refused** (bad/index-88, bad/std2002-pointer-88) |
| 2023 rule 24f, g | not with ANY LENGTH; not on a strongly typed group | **refused**: bad/std2002-anylen-88; strong types in docs/typedef.md |
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
| 2023 rule 3 | EXTERNAL AS literal | **test**: 2002/externalas (the externalized name the storage is shared under, GR 5; implemented 2026-10-06; GnuCOBOL 4 agrees); **refused**: a zero-length or non-alphanumeric literal |
| 85 IPC SR3; 2023 13.16.3 rule 5 | not with REDEFINES (2002 on: nor BASED, TYPEDEF) | **refused**: bad/external-rules (REDEFINES), bad/std2002-external-based (accepted before this sweep) |
| 85 IPC SR5; 2023 13.16.3 rule 7 | a data-name, not FILLER | **refused**: bad/external-rules |
| 85 GR1-4 | one record per name across the run unit; a file connector | **test**: free/external, free/faultwrite |
| 2023 13.18.22.4 rule 6; 14.8.4.3 | the descriptions of one external record alike across the run unit: the externalized name, the VALUE, the number of bytes | **test**: 2023/extconform (alike, EC-EXTERNAL checked in both programs, nothing raised), 2023/extformat (two bytes longer: EC-EXTERNAL-FORMAT-CONFLICT, fatal). Implemented 2026-10-07 (queue item 35): with the condition checked in a program (14.8.4.1: a >>TURN before its PROCEDURE DIVISION), its description -- the size and the VALUE text -- is compared at entry with the first program's (cob_ext_sig); unchecked, the storage grows to the larger as before. A complete REDEFINES of the record is not part of the description |
| 2023 14.8.4.2; E.2 items 12, 24 | an external file's FILE STATUS, RELATIVE KEY and LINAGE items are external items, the same storage in every program | **test**: 2023/extconform (both in the external record), 2023/extdatamis (a FILE STATUS of the sub's own: EC-EXTERNAL-DATA-MISMATCH, fatal); a RELATIVE KEY, LINAGE item or RECORD VARYING DEPENDING ON item in an EXTERNAL, LINKAGE or LOCAL-STORAGE record is taken now, its address bound at entry (they were refused; a BASED one still is) |
| 2023 12.4.5.3 rule 1; 14.8.4.4 | the file control entries of one external file alike: OPTIONAL, ASSIGN, RECORD DELIMITER, RESERVE, organization, access, COLLATING SEQUENCE, RELATIVE KEY, FILE STATUS | **test**: 2023/extfilemis (another access mode: EC-EXTERNAL-FILE-MISMATCH, fatal); compared as a signature of OPTIONAL, ASSIGN, organization, access, and whether RELATIVE KEY and FILE STATUS are written; **gap**: RECORD DELIMITER, RESERVE and COLLATING SEQUENCE are not kept per file |

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

## 13.18.2 ANY LENGTH (2002; implemented 2026-10-01, ISSUES-120)

The caller leaves each argument's length in bytes beside the argument
count (cob_call_lens, a CALL or a user function's invocation compiled
-std=2002); the called program puts it in the parameter's descriptor,
its own and writable, at entry, saved and restored with the
activation's words when it recurses.  Every reference to the item is
then a reference modification to its end, its length the descriptor's
at run time.

| rule | paraphrase | disposition |
|---|---|---|
| 13.18.2.3 rule 1 | PICTURE one X, N or 1 | **test**: 2002/anylen (X), 2002/anylennat (N); **refused**: bad/std2002-anylen-pic. PICTURE 1 is a **gap**, refused naming it |
| rule 2 | an elementary level 1 LINKAGE entry of a function or a contained program | **test**: 2002/anylen, 2002/anylenfn; **refused**: bad/std2002-anylen-ws; under -std=85, bad/anylen-85. In an outermost program it is taken as BP-E28 (warn/ext-std2002-anylen-outer), as Micro Focus and GnuCOBOL take it: this compiler's CALL carries the lengths |
| rules 3-4 | a BY REFERENCE parameter (or a program's returning item) | **refused**: bad/std2002-anylen-notparam, bad/std2002-anylen-value (refused there by BY VALUE's own rule first). As a program's RETURNING item, **gap** |
| GR 1b | the argument's length: n repetitions of the symbol | **test**: 2002/anylen (identical to GnuCOBOL: two lengths, passed on, MOVE into it, refmods, INSPECT, compare), 2002/anylenfn (an item, a literal, a recursive function, each activation its own length; no oracle: GnuCOBOL dies with SIGSEGV), 2002/anylennat |
| GR 1a | a zero-length argument: a zero-length item | **gap**: an argument of length zero reaches the reference modification check |
| (ours) | a caller compiled -std=85, or C, passes no lengths | the run stops, naming the program, rather than guess (cob_anylen_missing) |

## 13.15 Screen description entry: PICTURE with VALUE (2026-10-01)

| rule | paraphrase | disposition |
|---|---|---|
| 2002 13.15.2 rule 7; GR 3 | an elementary screen item: PICTURE with FROM, TO or USING; PICTURE with a numeric VALUE; a VALUE literal, its PICTURE "may be omitted" (so it may be written) | **test**: free/scrpicval -- the literal in a field of the picture's size, padded with spaces, or cut on the right with a warning. It was "a VALUE slot takes no PICTURE" (ISSUES-120). A numeric VALUE with a numeric PICTURE is a **gap**, refused naming it |
| 2002 13.15.1 (source-destination clauses) | FROM literal-1 | **test**: free/scrpicval -- the literal through the entry's PICTURE, as PICTURE with VALUE is; **refused** without a PICTURE: bad/screen-from-lit-nopic. It was "expected a data-name" (abrignoli_COBSOFT's `pic x(01) from "-"`). A numeric literal is a **gap** |


## 13.18.15 CONSTANT RECORD (implemented 2026-10-07, standard-queue item 25)

A structured constant (2014; 2023 13.18.15, D.21): a level 01 record of
the WORKING-STORAGE or LOCAL-STORAGE SECTION whose content is its
initial state for good. Under -std=2014. Test 2014/constrec (no oracle:
GnuCOBOL 4 does not take the clause).

How: the record is laid out as any other and emitted into `.rodata`
(driver.h), where the emulators fault a store; CANCEL and an INITIAL
program's re-entry skip it (nothing to restore); one in LOCAL-STORAGE
is static all the same (8.6.4: "always in initial state"), not copied
per activation. Each statement that stores into an operand refuses an
item whose record carries the clause (`no_constrec_recv`, operand.h):
MOVE and whatever moves (READ and RETURN INTO, MOVE CORRESPONDING's
pairs), the arithmetic statements and COMPUTE, SET (an index, a pointer,
a condition-name TO TRUE), ACCEPT, STRING INTO and POINTER, UNSTRING's
receivers, INSPECT REPLACING, CONVERTING and the TALLYING counter,
INITIALIZE, PERFORM and SEARCH VARYING, CALL and ALLOCATE RETURNING, a
screen item's TO and USING. What gets past them -- a store through a
pointer SET to its address, a called program writing a BY REFERENCE
argument, an EXEC SQL INTO -- is the memory fault; the text (D.21)
prohibits those "indirect means" without a rule a compiler could check.

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | WORKING-STORAGE or LOCAL-STORAGE only | **refused**: bad/std2014-constrec-linkage |
| SR 2 | neither the record nor an item in it a receiving operand | **refused**: bad/std2014-constrec-receiver, -set-true, -initialize, -string, -inspect, -perform; the rest by the same check |
| GR 1 | the content: as INITIALIZE WITH FILLER ALL TO VALUE THEN TO DEFAULT would leave it | **test**: 2014/constrec (the D.21 example: a binary item zero, DISPLAY items spaces, VALUEs kept, ALL "Q", a REDEFINES inside, a VALUE under OCCURS) |
| 11.9.10.4 GR 7 | not re-initialized with the program | **test**: 2014/constrec (a LOCAL-STORAGE one across two calls) -- it cannot change, so there is nothing to do |
| 13.18.38.3 SR 19, 23, 33 | no OCCURS DEPENDING ON, no dynamic-capacity table under it | **refused**: bad/std2014-constrec-odo; dynamic tables are not implemented |
| 13.18.44.3 SR 13 | nothing REDEFINES it | **refused**: bad/std2014-constrec-redefined |
| 13.18.45.3 SR 6 | no RENAMES into it | **refused**: bad/std2014-constrec-renames |
| 13.18.49.3 SR 10 | no SAME AS it | **refused**: bad/std2014-constrec-same-as |
| 13.18.60.3 SR 4 | no INDEX, POINTER, PROGRAM-POINTER, FUNCTION-POINTER item in it | **refused** by the layout check (a pointer below level 01 is refused by 13.18.60.3 rule 14 first) |
| 8.4.3.11 SR 3 (ADDRESS OF) | not of a CONSTANT RECORD item | **ruling**: taken -- ADDRESS OF a constant's item is a pointer to read-only storage, a store through it faults; refusing it would also refuse the reading uses the text allows for a pointer |
| 14.9.1 ... (the statements' own rules) | 14.9.20.3 INITIALIZE rule 1, 14.9.39.3 SET rules, 14.9.25.3 MOVE: not a receiving item | by SR 2's check |

## 13.18.49 SAME AS (implemented 2026-10-06, standard-queue item 11)

Expanded over the tokens as TYPE is (src/cobc/typedef.h,
`same_emit_entry`): the referenced entry's clauses in place of the
clause, less CONSTANT RECORD, EXTERNAL, GLOBAL, REDEFINES and SELECT
WHEN, its subordinates following with their levels adjusted. Test:
2002/sameas (GnuCOBOL 4 agrees).

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | data-name-1 not subject to an OCCURS clause | **refused**: bad/std2002-sameas-occurs |
| SR 2 | the entry not followed by a subordinate or level 88 entry | **refused**: bad/std2002-sameas-sub |
| SR 3 | no SAME AS back to the subject or its groups | the reference is to an entry before the subject, so it cannot name the subject; the expansion carries no SAME AS clause |
| SR 4 | no TYPE back to the subject's record | **n/a**: TYPE clauses are expanded before (type_emit_entry), so the referenced entry holds none |
| SR 5 | data-name-1's own description without OCCURS (its subordinates may have one) | **refused**: "its description has an OCCURS clause"; **test**: 2002/sameas (addr's subordinates under r-addr, itself under OCCURS) |
| SR 6 | no OBJECT REFERENCE under a FILE SECTION subject | **n/a**: object orientation |
| SR 7 | an elementary item, or a level 1 group, of the file, working-storage, local-storage or linkage section | **refused**: bad/std2002-sameas-level |
| SR 8 | a level 77 subject takes an elementary item | **refused**: "a level 77 item takes an elementary item's description"; **test**: 2002/sameas (`77 total same as amount`) |
| SR 9 | no GROUP-USAGE, SIGN or USAGE on a group above the subject | **gap**: not checked |
| SR 10 | no CONSTANT RECORD on data-name-1 | **refused**: bad/std2014-constrec-same-as (2026-10-07) |
| GR 1 | as though coded in place, less the excluded clauses | **test**: 2002/sameas |
| GR 2 | a group: the same subordinates, levels adjusted, past 49 allowed | **test**: 2002/sameas (`48 d3 same as pr`, its subordinates at 50 and 51 -- an expansion's level past 49 is taken by the parser from an expansion only, kept clear of 66, 77 and 88; a TYPE expanding past 49 likewise, 13.18.57.4 rule 2c) |
| GR 3-5 | a USAGE, GROUP-USAGE or SIGN clause of a group above data-name-1 carries to the subject | **gap**: not carried (the referenced entry is a level 1 item or an elementary one, whose own clauses are what it has) |

## 13.18.1 ALIGNED (implemented 2026-10-06, standard-queue item 11)

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | only for a bit group item or an elementary bit data item | **refused**: bad/std2002-aligned-nonbit |
| GR 1, 3 | the item at the first bit of the next byte; without ALIGNED, 8.5.1.6.3's packing | **test**: 2002/aligned (no oracle: USAGE BIT) |
| GR 2 | each occurrence of an aligned bit array on a byte | **test**: 2002/aligned (`d` occurs 3, bytes 2-4; `bit_stride`) |

## 13.4.5.3 rule 3: an FD with no record description entry (implemented 2026-10-06)

A FILLER record of the RECORD clause's size stands for the area
(layout.h, `finish_data_division`). Test: 2002/fdnorec (no oracle:
GnuCOBOL 4 requires a record description).

| rule | paraphrase | disposition |
|---|---|---|
| 3a | a RECORD clause | **refused**: bad/std2002-fd-norec-nosize |
| 3b | WRITE FILE file-name FROM and REWRITE FILE file-name FROM (14.9.51 and 14.9.35 format 2's FILE phrase, rule 7: FROM required) | **test**: 2002/fdnorec; **refused**: "WRITE FILE f takes a FROM phrase" |
| 3c | READ ... INTO | **refused**: bad/std2002-fd-norec-read |
