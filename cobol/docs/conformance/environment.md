# The compilation group, end markers, SOURCE-/OBJECT-COMPUTER, I-O-CONTROL; SD, BASED, TYPE, TYPEDEF, ROUNDED, ALLOCATE, FREE, UNLOCK

Swept 2026-10-07 (docs/plans/standard-queue.md items 19c, 19d, 19e).
2023: 10.6, 10.7; 12.3.5, 12.3.6, 12.4.4, 12.4.6 (12.4.6.3, 12.4.6.4);
13.4.6, 13.18.5, 13.18.57, 13.18.58; 14.7.4, 14.9.3, 14.9.15, 14.9.47.
11.5 FUNCTION-ID and 11.10 PROGRAM-ID were swept with the CALL family
(call.md, item 8); 12.3.7 SPECIAL-NAMES and 12.3.8 REPOSITORY in
clauses.md and call.md; 12.4.5 in files.md. The object-oriented
paragraphs of clause 11 (11.3 CLASS-ID, 11.4 FACTORY, 11.6 INTERFACE-ID,
11.7 METHOD-ID, 11.8 OBJECT) are **n/a** by ruling (docs/standards.md).

Test 2002/envsweep (no oracle: GnuCOBOL 4 has no UNLOCK status and
refuses a SAME SORT AREA differently). The sweep found seven
unenforced rules and two gaps -- a contained program did not inherit
its container's PROGRAM COLLATING SEQUENCE (12.3.6.4 rule 1), and
UNLOCK set no I-O status (14.9.47.4 rules 2-3) -- all fixed.

## 10.6 The compilation group, 10.7 end markers

| rule | paraphrase | disposition |
|---|---|---|
| 10.6.2 SR 1 | prototypes precede every other source unit | **refused**: bad/std2002-proto-after-def -- accepted before this sweep |
| 10.6.2 SR 2-3 | a definition and a prototype of one name: the same signature | **refused**: call.md (bad/std2002-proto-mismatch, -pgproto-mismatch) |
| 10.6.2 SR 4a | no ARITHMETIC clause in a prototype's OPTIONS | **refused** ("a prototype's OPTIONS paragraph has no ARITHMETIC clause") |
| 10.6.2 SR 4b-d | no OBJECT-COMPUTER, no INPUT-OUTPUT SECTION; SPECIAL-NAMES with ALPHABET, CURRENCY, DECIMAL-POINT, LOCALE, SYMBOLIC CHARACTERS only | **refused**: bad/std2002-proto-io-section; the others by the same checks -- all accepted before this sweep |
| 10.6.2 SR 4e | a prototype's DATA DIVISION: the LINKAGE SECTION only | **refused**: bad/std2002-proto-working-storage -- accepted before |
| 10.6.2 SR 4f | a prototype's PROCEDURE DIVISION: its header only | **refused**: call.md (bad/std2002-proto-body) |
| 10.6.2 SR 5, GR 1 | directives anywhere; a prototype's compilation feeds the repository | directives.md; call.md (the .s32fn / .s32pg signature files) |
| 10.7.3 SR 1 | an end marker on every unit that contains, is contained in, or precedes another | **refused**: bad/std2002-end-program-missing (a containing program without END PROGRAM: the next program was read as contained and the outer one went unmarked -- accepted before this sweep); a contained program without one, and a following program without one, were refused already |
| 10.7.3 SR 2, 7-9 | the name after END PROGRAM / END FUNCTION is the unit's | **refused**: "END PROGRAM names 'main2' but the program is 'main1'", likewise END FUNCTION; a bare `END PROGRAM.` is RM/COBOL's and taken (the Open Systems corpus) |
| 10.7.3 SR 3 | a contained program's END PROGRAM precedes its container's | **refused**: "a contained program needs its END PROGRAM" |
| 10.7.3 SR 4-6 | CLASS, METHOD, INTERFACE end markers | **n/a** |

## 11.3 CLASS-ID, 11.4 FACTORY, 11.6 INTERFACE-ID, 11.7 METHOD-ID, 11.8 OBJECT

| rule | paraphrase | disposition |
|---|---|---|
| 11.3, 11.4, 11.6, 11.7, 11.8 | the object-oriented source units' identification paragraphs | **n/a**: object orientation is out of scope by ruling (docs/standards.md, "Deferred"); CLASS-ID, INTERFACE-ID and METHOD-ID are refused where they are met |

## 12.3.5 SOURCE-COMPUTER, 12.3.6 OBJECT-COMPUTER, 12.4.4 FILE-CONTROL

| rule | paraphrase | disposition |
|---|---|---|
| 12.3.5 SR 1, GR 1-3 | the paragraph with or without a computer-name; the compiling machine either way; applies to contained units | **test**: `SOURCE-COMPUTER. slow-32.` and bare; WITH DEBUGGING MODE is BP-O12 (docs/behavior-points.md) |
| 12.3.6 SR 1 | PROGRAM COLLATING SEQUENCE names an alphanumeric alphabet | **refused**: "is not an ALPHABET of SPECIAL-NAMES"; a second alphabet-name (the national one, FOR NATIONAL) **gap**, refused by name (national collating sequences) |
| 12.3.6 SR 2-3 | national alphabets; locale-names of SPECIAL-NAMES | **gap**: item 45 (locales) |
| 12.3.6 SR 4, GR 2-4 | the paragraph bare, or with a computer-name of the implementor's meaning; MEMORY SIZE, SEGMENT-LIMIT read past | **test**; BP-O5 for MEMORY SIZE (obsolete), refused under -std=2002 |
| 12.3.6 GR 1 | the clauses apply to contained units | **test**: `inner` compares by its container's sequence -- did not before this sweep (the contained unit started from the native sequence; the alphabet is carried down as its first, HIGH-VALUE and LOW-VALUE with it; its own OBJECT-COMPUTER clause replaces it) |
| 12.3.6 GR 5-8 | CHARACTER CLASSIFICATION: a locale's LC_CTYPE for the class conditions and UPPER-CASE / LOWER-CASE | **gap**: item 45, refused by name (bad/std2002-objcomp-classification -- accepted silently before this sweep, the probe note of the queue) |
| 12.3.6 GR 9-10 | the alphanumeric program collating sequence; native when none | **test**: `"A" > "Z"` under `ALPHABET rev IS "Z" THRU "A"`; CCVS NC (collating programs) |
| 12.3.6 GR 11 | decides relation conditions, condition-name conditions, report CONTROL breaks | **test**: a relation and a level-88 range under the sequence; reportwriter.md |
| 12.3.6 GR 12-13 | effective from the unit's start; SORT and MERGE keys unless the statement's own COLLATING SEQUENCE | sort.md |
| 12.4.4 | FILE-CONTROL: file control entries | files.md (12.4.5); FILE-CONTROL without the header is BP-D1 |

## 12.4.6 I-O-CONTROL: 12.4.6.3 APPLY COMMIT, 12.4.6.4 SAME

| rule | paraphrase | disposition |
|---|---|---|
| 12.4.6.2 | APPLY COMMIT and SAME clauses in any order, the paragraph's periods | **test** (three SAME clauses); RERUN and MULTIPLE FILE TAPE are BP-O10 and BP-O11 |
| 12.4.6.3 | APPLY COMMIT ON files and items: COMMIT and ROLLBACK's scope | **gap**: 2023's transaction facility, refused by name (bad/std2002-apply-commit) |
| 12.4.6.4 SR 1 | SORT and SORT-MERGE equivalent | **test** |
| 12.4.6.4 SR 2 | the files of this program's FILE-CONTROL | **refused**: bad/std2002-same-unknown -- an unknown name ended the list silently before this sweep |
| 12.4.6.4 SR 3 | not an EXTERNAL file | **refused** ("is an EXTERNAL file") |
| 12.4.6.4 SR 4 | any organizations and access modes together | **test** |
| 12.4.6.4 SR 5 | a report file in a SAME AREA clause only | **refused** ("is a report file, which goes in a SAME AREA clause only") |
| 12.4.6.4 SR 6 | a sort file not in a SAME AREA clause | **refused**: bad/std2002-same-area-sortfile -- accepted before this sweep |
| 12.4.6.4 SR 7 | a file in at most one SAME AREA and one SAME RECORD AREA clause | **refused**: bad/std2002-same-twice -- accepted before |
| 12.4.6.4 SR 8 | a SAME SORT AREA clause names a sort or merge file | **refused**: bad/std2002-same-sort-no-sortfile -- accepted before |
| 12.4.6.4 SR 9-10 | a SAME AREA set that overlaps a SAME RECORD AREA or SAME SORT AREA set lies within it | **refused**: bad/std2002-same-subset -- accepted before |
| 12.4.6.4 SR 11 | not with APPLY COMMIT | n/a while APPLY COMMIT is a gap |
| 12.4.6.4 GR 1 | SAME AREA: the files share their storage, one open at a time | **ruling**: accepted as the hint it is -- storage is not scarce here and each file keeps its own; the one-open-at-a-time restriction is not enforced |
| 12.4.6.4 GR 2 | SAME RECORD AREA: one record area, the record of the file last read or being written | **test**: `r2` shows what `r1` was written with (implemented long since: CCVS SQ, majesty) |
| 12.4.6.4 GR 3-5 | commit and rollback; SAME SORT AREA: the sort's storage reused, the other files not open during the sort | GR 3 with APPLY COMMIT; **ruling**: SAME SORT AREA accepted as a hint, the sort's work space is its own (xsort.h) |

## 13.4.6 The sort-merge file description entry (SD)

| rule | paraphrase | disposition |
|---|---|---|
| 13.4.6.2 | SD file-name with a RECORD clause only | **refused** for the other FD clauses (sort.md) |
| SR 1 | a file control entry for it | **refused**: "SD wk has no SELECT" |
| SR 2 | one or more record description entries | **refused**: bad/std2002-sd-no-record -- met an internal error before this sweep |
| SR 3 | not in an input-output statement | **refused**: bad/std2002-sd-open (OPEN, CLOSE, READ, WRITE, REWRITE, DELETE, START, UNLOCK of a sort file: SORT, MERGE, RELEASE and RETURN name it) -- OPEN was accepted and failed at run time before this sweep |
| SR 4 | its records only after FROM or INTO in an input-output statement | sort.md (RELEASE FROM, RETURN INTO) |
| GR 1 | sizes in bytes | **test** |

## 13.18.5 BASED

| rule | paraphrase | disposition |
|---|---|---|
| SR 1-2 | not an object; not a dynamic-length item or variable-length group | objects **n/a**; dynamic-length items are 2014's (item 25); a variable-length group (OCCURS DEPENDING ON inside) is taken, ALLOCATE giving the maximum length (14.9.3.4 rule 3) -- the **ruling** below |
| GR 1-2 | a template over an implicit data-pointer, NULL at first, set by SET and ALLOCATE | **test**: 2002/basedlocal, 2002/pointerset; set.md |
| GR 3-4 | referenced while NULL, EC-DATA-PTR-NULL; while not an address, EC-BOUND-PTR | exceptions.md (EC-DATA-PTR-NULL raised with checking on; EC-BOUND-PTR is not raised, the ruling of item 9) |

Ruling (2026-10-07): 13.18.5.3 rule 2 keeps variable-length groups
out of BASED, while 14.9.3.4 rule 3 says what ALLOCATE does for a
based item with an OCCURS DEPENDING ON entry under it (the maximum
length). The text contradicts itself; the ALLOCATE rule is the
useful one and is followed.

## 13.18.57 TYPE, 13.18.58 TYPEDEF

docs/typedef.md has the design (an expansion over the tokens) and the
strong-type rules (ISSUES-80); data-division.md the TYPEDEF placement
rules. Here, the clauses' own rules:

| rule | paraphrase | disposition |
|---|---|---|
| 13.18.57 SR 1 | no SAME AS under the type that refers back to the subject | the type is expanded before SAME AS is, and a type's SAME AS names an item before the type (13.18.49) |
| 13.18.57 SR 2 | an entry with TYPE is not followed by a subordinate or level 88 entry | **refused**: bad/std2002-type-followed-sub -- accepted before this sweep |
| 13.18.57 SR 3-4 | a strong type's subject neither renamed nor redefined | docs/typedef.md (**refused**: bad/std2002-strong-*) |
| 13.18.57 SR 5 | no group above the subject has GROUP-USAGE, SIGN or USAGE | **refused**: bad/std2002-type-under-usage -- accepted before |
| 13.18.57 SR 6-7 | a strong type at level 1 or inside a strong type; a level 77 takes an elementary type | **refused** (both before this sweep) |
| 13.18.57 SR 8 | no object references in a FILE SECTION type | **n/a** |
| 13.18.57 SR 9-16 | format 2, the report group's TYPE | reportwriter.md |
| 13.18.57 GR 1-2 | as though the description were written in place, less the level, name, alignment, GLOBAL, SELECT WHEN and TYPEDEF; a group's subordinates with levels adjusted, past 49 allowed, aligned as a level 1 | **test**: 2002/typedecl, 2002/strongtype, 2002/sameas (docs/typedef.md) |
| 13.18.57 GR 3 | the subject's own VALUE is its initial value; the type's implicit PICTURE comes along | **test**: `01 q TYPE pt VALUE "123ab"` shows 123ab, `01 r TYPE pt` the type's own 005ab -- the subject's VALUE was refused before this sweep ("has a VALUE clause inside the group, which has one"); the type's subordinate VALUEs are left out when the subject has one |
| 13.18.57 GR 4 | the subject's BASED over the type's | docs/typedef.md |
| 13.18.58 SR 1 | STRONG not on an elementary type | **refused** ("TYPEDEF STRONG: 'et' is not a group") |
| 13.18.58 SR 2 | no TYPE in the type that refers to itself | **refused** ("is not a type declared before this entry") |
| 13.18.58 SR 3 | TYPEDEF with EXTERNAL at level 1 | **refused** ("a type declaration here is a level 01 or 77 entry") |
| 13.18.58 GR 1-3 | a type declaration: its subordinates' names reachable only through items of the type; no storage; GLOBAL on the type-name's scope | docs/typedef.md; **test** |

## 14.7.4 ROUNDED

| rule | paraphrase | disposition |
|---|---|---|
| general | truncation after decimal-point alignment; P positions round at the rightmost stored digit | **test**: `9(3)PP` from 12345 gives 12300, from 12350 gives 12400 (the sweep's probe) |
| GR 1-2 | no MODE: the OPTIONS paragraph's DEFAULT ROUNDED, else NEAREST-AWAY-FROM-ZERO; no ROUNDED: TRUNCATION | **test**; options.md (DEFAULT ROUNDED, item 16) |
| GR 3-6, 8-10 | AWAY-FROM-ZERO, NEAREST-AWAY-FROM-ZERO, NEAREST-EVEN, NEAREST-TOWARD-ZERO, TOWARD-GREATER, TOWARD-LESSER, TRUNCATION | **test**: 2002/envsweep (each mode on 1.25, 1.21, -1.21, 1.29); arithmetic.md |
| GR 7 | PROHIBITED: not exactly representable, EC-SIZE-TRUNCATION, the size error condition, the receiver unchanged | **test**: `ON SIZE ERROR` taken, the receiver unchanged |

## 14.9.3 ALLOCATE, 14.9.15 FREE

| rule | paraphrase | disposition |
|---|---|---|
| 14.9.3 SR 1-3 | data-name-1 BASED; RETURNING required without it; data-name-2 a data-pointer | **refused** (each with its rule cited; probes al2, al3, al4 of the sweep) |
| 14.9.3 SR 4-5 | a restricted or strongly-typed pointer restricted to the type | usage.md (restricted pointers) |
| 14.9.3 GR 1-2 | a number of bytes, rounded up; 0 or less, NULL | **test**: `n * 2.5 CHARACTERS` (25), `0 CHARACTERS` gives NULL |
| 14.9.3 GR 3-5 | the item's size (the maximum with an OCCURS DEPENDING ON); the address set; none available, NULL and EC-STORAGE-NOT-AVAIL | **test**; exceptions.md |
| 14.9.3 GR 6-7 | INITIALIZED: binary zeros for characters; for an item, INITIALIZE ... WITH FILLER ALL TO VALUE THEN TO DEFAULT | **test**: `ALLOCATE b INITIALIZED` shows spaces in `bx`; initialize.md |
| 14.9.3 GR 8-9 | without INITIALIZED: the OPTIONS INITIALIZE clause's fill, else undefined (pointers in an item NULL) | **ruling**: zero-filled (calloc); the OPTIONS INITIALIZE clause is 2023's (item 30) |
| 14.9.3 GR 10 | until FREE or the end of the run unit | **test** |
| 14.9.15 SR 1 | a data-pointer | **refused** ("FREE 'x': a data-pointer item"); `FREE ADDRESS OF b` refused as a receiving ADDRESS OF (8.4.3.11 rule 5): SET a pointer first |
| 14.9.15 GR 1a-c | allocated storage released and the pointer NULL; NULL does nothing; otherwise EC-STORAGE-NOT-ALLOC | **test**: `FREE p` then `p = NULL`, a second FREE harmless; exceptions.md |
| 14.9.15 GR 2 | several operands: one FREE each, in order | **test**: `FREE p q` (probe al1) |

## 14.9.47 UNLOCK

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | not a sort or merge file | **refused**: bad/std2002-unlock-sortfile -- accepted before this sweep |
| SR 2 | not a file under APPLY COMMIT | n/a while APPLY COMMIT is a gap |
| GR 1 | the file's record locks released, locks or none | **test**: nothing is locked here; with locks, 2002/locking (locking.md) -- another connector reads the record UNLOCK freed |
| GR 2-3 | the file open; the I-O status set | **test**: 00 open, **47** not open -- no status was set before this sweep (RM/COBOL's statement, read past) |
