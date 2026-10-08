# What the compiler refuses, and why

A refusal says one of four things, and the message should say which:

1. **The standard forbids it.** The message cites the rule and reads
   as an error in the program ("... shall not be reference-modified
   (2023 14.9.43.3 rule 4)"), not as a promise.
2. **A gap in an edition this compiler targets** (-std=85 or
   -std=2002). The message says "not implemented".
3. **Out of scope, by ruling.** Object orientation, the Communication
   module, the Debug module and VALIDATE. The message names the ruling.
   (The functions of later editions were here until 2026-10-06; the
   owner ruled then that everything standard is in scope, so they are
   class 2 gaps, queued in docs/plans/standard-queue.md.)
4. **An implementation limit.** A fixed capacity that a program could
   in principle exceed.

Surveyed 2026-09-28 against X3.23-1985 (FIPS 21-2) and ISO/IEC
1989:2023; section 2 checked again against the compiler's messages on
2026-10-06. Line numbers drift; the messages are the index.

## 1. Forbidden by the standard

All three below fixed 2026-09-28 (ISSUES-95), each with a refusal test.

Mislabelled "not implemented", to be reworded with the citation:

| refusal | rule |
|---|---|
| a reference-modified STRING receiver | 85 VI-131 STRING syntax rule 3; 2023 14.9.43.3 rule 4 |
| items after an OCCURS DEPENDING ON table in its record ("variable-location items") | 85 OCCURS format 2 syntax rule 10; 2023 13.18.38.3 rule 22. Refused today only when a MOVE or operand uses the group; the declaration itself is the error |

Accepted but forbidden, a refusal missing:

| construct | rule |
|---|---|
| UNSTRING with a reference-modified sending item, under -std=85 | 85 UNSTRING syntax rule 7 (2023 dropped the rule) |

This second table is the finding that matters. CCVS-85 tests what a
compiler must accept; it barely tests what it must reject. So nothing
has ever checked the syntax rules this compiler does not enforce. One
turned up in a spot check; a rule-by-rule sweep is the way to find the
rest (see "What follows" below).

## 2. Gaps in a targeted edition

COBOL 85 (Stage A was declared complete; these are in the 85 text) --
all closed 2026-09-28 (ISSUES-94, -95):

- the Report Writer CODE clause, and with it REPORTS ARE with several
  reports to one file and INITIATE/TERMINATE of several reports, which
  were missing too;
- INITIALIZE of a reference-modified item;
- BY CONTENT of a reference-modified item;
- a REPORT SECTION in a contained program.

COBOL 2002/2023 (Stage B):

- compiler directives other than >>SOURCE, >>TURN, >>DEFINE, >>IF,
  >>EVALUATE, >>CALL-CONVENTION, >>LEAP-SECOND, >>LISTING, >>PAGE,
  >>PROPAGATE (the last eight since 2026-10-06, docs/conformance/directives.md)
  and >>REF-MOD-ZERO-LENGTH (2026-10-07, queue item 24, under -std=2014):
  that is 2023's COBOL-WORDS, DISPLAY, FLAG-14, PUSH and POP; and boolean
  expressions in a directive;
- exceptions: the conditions docs/conformance/exceptions.md marks
  **gap** -- EC-RANGE-INVALID, EC-I-O-EOP and -LINAGE, EC-FLOW-GLOBAL-*,
  EC-SCREEN-*, and those of features not built (USE AFTER EXCEPTION
  CONDITION ... FILE and WHEN EXCEPTION with a file-name or open mode
  since 2026-10-06, fourteen more conditions raised the same day; ON EXCEPTION on ACCEPT FROM
  ARGUMENT-NUMBER, ARGUMENT-VALUE and COMMAND-LINE is not a gap of the
  standard: those sources are X/Open's, in no ISO edition, and stay
  refused until a program needs them; EC-DATA-INCOMPATIBLE is raised wherever
  numeric or boolean content is sent since ISSUES-101 and -103; ON
  EXCEPTION on an ACCEPT of a screen and on a positioned ACCEPT since
  ISSUES-125, and on ACCEPT FROM ENVIRONMENT since BP-E31);
- BY CONTENT of a bit item's part of computed length (a part of
  literal length is passed since 2026-10-06, docs/conformance/refmod.md);
- a user-defined function in the places listed by g_ufn_forbid;
- in the screen section (the list is docs/conformance/screen.md's
  "Open"): GLOBAL; colours, LINE and COLUMN from an identifier; MINUS;
  LINE and COLUMN phrases on ACCEPT and DISPLAY of a screen; ON
  EXCEPTION on DISPLAY of a screen; OCCURS on a group or over FROM, TO,
  USING items; FROM with TO in one entry; BLANK LINE; JUSTIFIED and
  USAGE NATIONAL on national pictures; EC-SCREEN (above).  (CURSOR IS, SIGN, JUSTIFIED, status 8000 and ON
  EXCEPTION landed 2026-10-04, ISSUES-125.)  ACCEPT FROM the remaining
  sources; the SPECIAL-NAMES clauses not yet taken;
- bits: a bit table inside an occurring bit group (two bit
  dimensions); a bit item's part at a computed position in a SCREEN
  SECTION item (docs/conformance/national-boolean.md);
- a currency symbol from outside the COBOL character repertoire in a
  PICTURE (2014 E.3 item 18): the picture scanner works in bytes, and a
  multi-byte UTF-8 symbol is refused as "one character"; WITH PICTURE
  SYMBOL gives such a currency its string (edition-2014.md);
- USAGE NATIONAL on a screen item whose PICTURE is not N
  (docs/conformance/national-boolean.md).
- the 2002-2023 constructs that used to meet a parse error and are now
  refused by name (docs/plans/standard-queue.md item 1; tests/bad/
  std2002-*): SUPPRESS WHEN,
  FORMAT and SELECT WHEN, USAGE
  MESSAGE-TAG,
  OCCURS DYNAMIC, SET
  LOCALE / ATTRIBUTE, ALPHABET FOR and IS LOCALE,
  and the LOCALE function phrase.

## 3. Out of scope, by ruling

- Object orientation: RAISE of an exception object, USE AFTER EXCEPTION
  OBJECT, REPOSITORY class entries (docs/standards.md, "Deferred").
- The Communication module (ENTER, SEND, RECEIVE, ...).
- USE FOR DEBUGGING and the >>D indicator (the Debug module, obsolete
  in 85 and removed in 2014).
- VALIDATE (docs/standards.md).

In scope since 2026-10-06 (the owner: "everything standard holds"):
the COBOL 2014 and 2023 intrinsic functions and locale support, once
listed here. The 2014 date and time functions came 2026-10-07 (queue
item 27), the 2023 functions the same day (item 33); the locale
functions are still refused as not implemented and queued (item 45).

Two ruled 2026-09-28:

- **ALPHABET ... IS EBCDIC: implemented 2026-09-29** (ISSUES-100; code page 037). An alphabet is a
  collating sequence and a CODE-SET translation, not the machine's
  code. A mainframe program with PROGRAM COLLATING SEQUENCE IS EBCDIC
  sorts and compares in EBCDIC order on any machine, and honoring that
  is what makes it correct. README ruling 5 ("No EBCDIC on this ISA")
  stays about data formats. Moves to class 2.
- **Floating-point USAGE.** FLOAT-SHORT, FLOAT-LONG and FLOAT-EXTENDED
  are COBOL 2002 (its USAGE clause), and -std=2002 takes them. The
  IEEE usages FLOAT-BINARY-32/64/128 and FLOAT-DECIMAL-16/34 are COBOL
  2014: the first piece of the -std=2014 switch, which took them
  2026-10-07 (docs/plans/standard-queue.md item 20; docs/usage.md) --
  binary32 and binary64 on SLOW-32's hardware, the three others in
  software. COMP-1 and COMP-2 (IBM hexadecimal float) stay out.
  (Corrected 2026-10-06: this entry called all six 2014.)

Ruled 2026-10-07 (queue item 17): **READ PREVIOUS of a sequential file
of variable-length records** is refused (bad/std2002-readprev-varying).
The record before the current one has no fixed place to step back to;
2023 14.9.30 asks for it of every sequential file, and fixed-length
records get it (docs/conformance/io-statements.md). START LAST of such
a file walks its records and is taken.

## 4. Implementation limits

- a national-edited PICTURE longer than PIC_MAXPAT - 1 characters;
- more than 16 CALL arguments, more than 32 USING items (stack
  arguments beyond the eighth; the standard sets no limit).

## Dead code

- `"suppress"` in the verb refusal list: removed (ISSUES-95).

## What follows

Classes 1 and 4 are small and fixed directly. Class 2 is ranked when
the conformance matrix is built: each Stage B module's syntax and
general rules, taken from the 2023 text one by one, each with a
positive test, a refusal test, or a written reason. That sweep is also
what finds the missing refusals of class 1.
