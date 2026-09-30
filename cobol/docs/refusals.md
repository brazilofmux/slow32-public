# What the compiler refuses, and why

A refusal says one of four things, and the message should say which:

1. **The standard forbids it.** The message cites the rule and reads
   as an error in the program ("... shall not be reference-modified
   (2023 14.9.43.3 rule 4)"), not as a promise.
2. **A gap in an edition this compiler targets** (-std=85 or
   -std=2002). The message says "not implemented".
3. **Out of scope, by ruling.** Object orientation, the Communication
   module, the Debug module, VALIDATE, and the functions of later
   editions. The message names the ruling or the edition.
4. **An implementation limit.** A fixed capacity that a program could
   in principle exceed.

Surveyed 2026-09-28 against X3.23-1985 (FIPS 21-2) and ISO/IEC
1989:2023. Line numbers drift; the messages are the index.

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
- BY CONTENT of a reference-modified item (a bit part still refused);
- a REPORT SECTION in a contained program.

COBOL 2002/2023 (Stage B):

- the rest of the CALL family: function and program prototypes,
  FUNCTION-ID and REPOSITORY `AS literal`, BY VALUE parameters of a
  function, ANY LENGTH (BY VALUE, OPTIONAL, OMITTED, program RETURNING
  and stack arguments are implemented: docs/conformance/call.md);
- compiler directives other than >>SOURCE and >>TURN (>>DEFINE, >>IF,
  >>EVALUATE, ...);
- exceptions: USE AFTER EXCEPTION CONDITION ... FILE, WHEN EXCEPTION
  with a file-name or open mode, ACCEPT ... ON EXCEPTION, the rest of
  Table 13's conditions (EC-DATA-INCOMPATIBLE is raised wherever
  numeric or boolean content is sent since ISSUES-101 and -103);
- a MOVE sender reference-modified with a computed length over an item
  that a receiver before the last changes (general rule 1 needs a
  snapshot of run-time length; docs/conformance/move.md);
- reference modification in a screen item and in positioned DISPLAY
  and ACCEPT; a reference-modified numeric receiver of national data;
- LENGTH OF, BYTE-LENGTH and other functions of a reference
  modification with a variable length; a function's reference
  modification with an expression length;
- a TYPE that expands past level 49 (2023 13.18.57.4 rule 2c allows
  it);
- a user-defined function in the places listed by g_ufn_forbid;
- BLANK LINE in the screen section; ACCEPT FROM the remaining sources;
  the SPECIAL-NAMES clauses not yet taken;
- bits: OCCURS DEPENDING ON on a bit array, OCCURS on a bit group, a
  character item redefining a bit item that starts mid-byte;
- EXIT PROGRAM RAISING and GOBACK RAISING: propagating an exception to
  the caller (docs/conformance/exit.md);
- intrinsic functions in arithmetic of more than 18 digits, BINARY-DOUBLE
  and the other phase-3 cases of docs/wide.md (items, literals, MOVE,
  relations, DISPLAY and ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE of 19
  to 31 digits are implemented, ISSUES-117);
- RESUME (optional since 2014);
- USAGE NATIONAL on a screen item whose PICTURE is not N
  (docs/conformance/national-boolean.md).
- a BASED entry in LOCAL-STORAGE and EC-BOUND-PTR
  (docs/conformance/usage.md; ALLOCATE ... INITIALIZED of a based
  record is implemented since ISSUES-104);
- INITIALIZE of a reference-modified item with the COBOL 2002 phrases
  (WITH FILLER, TO VALUE, TO DEFAULT; the phrases themselves are
  implemented, docs/conformance/initialize.md);
- READ PREVIOUS of a sequential file (2002; io-statements.md);
- USAGE BINARY-DOUBLE, whose range needs 19 digits (docs/wide.md
  phase 3).

## 3. Out of scope, by ruling

- Object orientation: RAISE of an exception object, USE AFTER EXCEPTION
  OBJECT, REPOSITORY class entries (docs/standards.md, "Deferred").
- The Communication module (ENTER, SEND, RECEIVE, ...).
- USE FOR DEBUGGING and the >>D indicator (the Debug module, obsolete
  in 85 and removed in 2014).
- VALIDATE (docs/standards.md).
- The COBOL 2014 and 2023 intrinsic functions, and the locale
  functions.

Two ruled 2026-09-28:

- **ALPHABET ... IS EBCDIC: implemented 2026-09-29** (ISSUES-100; code page 037). An alphabet is a
  collating sequence and a CODE-SET translation, not the machine's
  code. A mainframe program with PROGRAM COLLATING SEQUENCE IS EBCDIC
  sorts and compares in EBCDIC order on any machine, and honoring that
  is what makes it correct. README ruling 5 ("No EBCDIC on this ISA")
  stays about data formats. Moves to class 2.
- **Floating-point USAGE: COBOL 2014, under -std=2014.** FLOAT-SHORT,
  FLOAT-LONG, FLOAT-EXTENDED and FLOAT-BINARY-32/64/128 are IEEE, which
  SLOW-32 has in hardware; they may land early as the first piece of a
  2014 switch. COMP-1 and COMP-2 (IBM hexadecimal float) stay out.

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
