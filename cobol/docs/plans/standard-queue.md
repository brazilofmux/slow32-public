# The road to ISO/IEC 1989:2023 -- a work queue for s32-cobc

Drafted 2026-10-06 for the owner's review; nothing in it is started. Items are taken one at a
time and checked off here; the order may change as the coverage matrix fills in.

## Scope and method

**Target text.** ISO/IEC 1989:2023, the current edition, minus what the owner has ruled out (section
"Out by ruling"). The compiler's model stays one compiler with a `-std` switch: today `-std=85`
(default) and `-std=2002` exist (`src/cobc/driver.h`; any other value is refused). This queue
assumes two more rows, `-std=2014` and `-std=2023`, each accepting the one before plus its own
additions, and each the place where a later edition's *removals* go behind the switch.

**Editions.** "Ed." below is the edition that introduced the construct, checked by searching the
2002, 2014 and 2023 texts, and Annex E of 2014 (changes from 2002) and of 2023 (changes from 2014).
Optional elements are 2023 A.4; processor-dependent ones 2023 A.3. Section numbers are 2023's unless
marked. Rules are paraphrased; nothing is quoted.

**How the gaps were found.** (1) refusals.md section 2 and every "not implemented" message in
`src/cobc/*.h`; (2) coverage.md's 28 not-implemented and 127 unswept elements; (3) over 100 probe
programs compiled with `out/s32-cobc -free -std=2002` (binary of 2026-10-04, newer than every
compiler source) in the scratchpad. "Refused: ..." quotes the compiler's own message, abbreviated. A
probe proves acceptance or refusal only, not correct run-time behaviour, except where "run" is said.

**JSON and XML.** Neither JSON nor XML appears anywhere in the 2002, 2014 or 2023 texts. `JSON
GENERATE`/`PARSE` and `XML GENERATE`/`PARSE` are IBM Enterprise COBOL extensions, not standard
COBOL; neither is queued.

**Sizes.** S = a day or so with tests; M = several sessions; L = a module of its own, with a design
note first.

## Tier 1 -- finish what is started, and make the gap list honest

**1. Name every gap.** Ed. 2002-2023; S; deps none. DONE 2026-10-06: each construct below is
refused by name, with a test in tests/bad (std2002-*), and so are ALPHABET FOR/IS LOCALE,
CURRENCY with PICTURE SYMBOL, CONSTANT FROM, the float conditions (INFINITY ...), ADDRESS OF
FUNCTION/PROGRAM, SET TO ENTRY, and OPEN f SHARING. Not changed, being unverified or not what the
probe meant: COMMIT after DISPLAY on the next line (DISPLAY takes COMMIT as an operand), SORT of a
table with no KEY data-name, PROGRAM COLLATING SEQUENCE NATIVE. SHARING and LOCK MODE in SELECT
stay accepted and ignored (GitHub #34).
- Today: about thirty constructs meet a bare parse error rather than a "not implemented" refusal,
  against refusals.md's own rule. Verified: `START f FIRST` ("'first' is not a COBOL verb"), `START
  ... WITH LENGTH`, `OPEN I-O SHARING ...`, `READ ... WITH LOCK`, `RETRY`, `WRITE FILE`, `REWRITE
  FILE`, `ASSIGN USING`, FD `FORMAT`, ALTERNATE KEY `SUPPRESS WHEN`, `PROCEDURE DIVISION RAISING`,
  `SET ... ATTRIBUTE`, `SET CONTENT OF`, `SET LOCALE`, `USAGE PROGRAM-POINTER` / `FUNCTION-POINTER`
  / `MESSAGE-TAG`, `ALIGNED`, the `OPTIONS` paragraph, floating-point literals (`1.5E+3`),
  floating-point numeric-edited PICTUREs, `ANYCASE`, `XOR` / `EXCLUSIVE-OR`, `INSPECT BACKWARD`,
  `DELETE FILE`, `WRITE` with both BEFORE and AFTER, `OCCURS DYNAMIC CAPACITY`, `PACKED-DECIMAL NO
  SIGN`. `START` on a sequential file is refused with a message that reads as a rule ("START needs
  an INDEXED or RELATIVE file") though 2002 14.8.37 allows it with FIRST/LAST.
- Why first: every later item lands as "refused by name, then implemented"; this makes the start
  state true and refusals.md complete.

**2. Correct the coverage matrix.** Ed. --; S; deps none. DONE 2026-10-06: the eight contradictions
below corrected in the docs and two compiler messages; group SYNCHRONIZED refused under -std=2002;
the matrix's keyword and ancestor credits fixed (Report Writer's re-keying stays item 19f).
- Today: `gen-coverage.py` credits by keyword and by "and its clauses". False credits found: 11.9.10
  (the OPTIONS paragraph's INITIALIZE clause) is credited to initialize.md, but the OPTIONS
  paragraph is not parsed at all; 12.4.5.9 LOCK MODE and 12.4.5.15 SHARING are credited to files.md,
  which has no row for either (the clauses parse; the statements that give them meaning do not). The
  Report Writer clauses (13.18.12/14/16/28/35/ 37/39/46/53/54/57) show unswept because
  reportwriter.md is keyed to 85 numbering. 14.9.31 RECEIVE and 14.9.38 SEND are labelled "the
  Communication module" -- in 2023 they are the new asynchronous messaging facility (E.3.2 item 1;
  A.3 item 4), not the module removed in 2014.
- Why here: the matrix is how this queue gets re-ranked; it must not over-report.

**3. Exception propagation.** 14.9.14/14.9.18 RAISING, 14.2 header RAISING, 7.3.21 PROPAGATE; Ed.
2002; M; deps none. DONE 2026-10-06 (exit.md, control.md, call.md, directives.md; test
2002/ecraising + lib/ecpropagate.cbl): EXIT PROGRAM / GOBACK RAISING exception-name and LAST, the
header's RAISING list of EC-USER names, EC-RAISING-NOT-SPECIFIED, >>PROPAGATE ON/OFF. The
callee leaves the name with libcob; every CALL and function invocation at -std=2002 asks for it
against the caller's enabled names and dispatches it as a raise of its own. RAISING an object
reference stays with object orientation (item 51).
- Today: refused -- "EXIT PROGRAM RAISING is not implemented yet", "GOBACK RAISING ...",
  ">>propagate is not implemented yet"; `PROCEDURE DIVISION RAISING ec-size` is a bare parse error.
- Why here: the last structural piece of the exception module, whose core landed (ISSUES-53 onward).

**4. The rest of Table 13.** 14.6.13.1.6, Table 13; Ed. 2002 (+2014/2023 names); M; deps 3 for
EC-RAISING-* (met). AUDITED 2026-10-06: docs/conformance/exceptions.md has every level-3 name's row.
Raised then: EC-OVERFLOW-STRING/-UNSTRING, EC-RANGE-SEARCH-NO-MATCH, EC-FLOW-RELEASE/-RETURN/
-REPORT, the five EC-SORT-MERGE-* that can arise, EC-REPORT-ACTIVE/-INACTIVE/-FILE-MODE/
-NOT-TERMINATED. Still open here (the page's **gap** rows): EC-RANGE-INVALID, EC-I-O-EOP/
-EOP-OVERFLOW/-LINAGE, EC-FLOW-GLOBAL-EXIT/-GOBACK, EC-RANGE-INSPECT-SIZE at run time,
EC-REPORT-PAGE-LIMIT at run time; the others wait on their features.
- Today: 119 level-3 names are in `g_ec[]` (control.h). Verified by run: a STRING that overflows
  under `>>TURN EC-ALL CHECKING ON` does not raise EC-OVERFLOW-STRING. A name-search heuristic
  suggests EC-OVERFLOW-*, EC-RANGE-INSPECT-SIZE, EC-FLOW-GLOBAL-*, EC-FLOW-RELEASE/RETURN,
  EC-PROGRAM-ARG-MISMATCH, EC-SIZE-UNDERFLOW, EC-REPORT-* and EC-SCREEN-* are never raised
  (unverified: the I-O names are raised from status classes, so the heuristic undercounts). First
  step is an audit table, one row per name: raised where, tested by what, or why it cannot arise.
- Why here: standards.md lists it as the module's open remainder.

**5. Exception leftovers.** 14.9.49, 14.9.28 WHEN, 14.9.1; Ed. 2002/2023; S; deps none. DONE
2026-10-06: USE AFTER EC ... FILE and WHEN EXCEPTION file-name / open mode (2002/usefile,
2002/ecpfile). The third piece was not standard: ACCEPT FROM ARGUMENT-NUMBER, ARGUMENT-VALUE and
COMMAND-LINE are X/Open's and appear in no ISO edition, so their ON EXCEPTION stays refused.
- Today: "USE AFTER EXCEPTION CONDITION ... FILE is not implemented yet"; "WHEN EXCEPTION with a
  file-name or an open mode is not implemented yet"; `ACCEPT ... FROM ARGUMENT-VALUE ON EXCEPTION`
  refused ("ACCEPT ... ON EXCEPTION is not implemented").
- Why here: small, and closes the forms real programs write first.

**6. Conditional compilation.** 7.3.5-7.3.8, 7.3.11 DEFINE, 7.3.13 EVALUATE, 7.3.16 IF; Ed. 2002; M;
deps none. DONE 2026-10-06 (docs/conformance/directives.md): all three, CONSTANT FROM, PARAMETER
from -D; compile-time boolean expressions remain a gap.
- Today: ">>define / >>if is not implemented yet"; the constant entry's FROM form waits on it ("FROM
  compilation-variable-name needs >>DEFINE").
- Why here: the most-used 2002 directive family, and a prerequisite for compiling real 2002+ sources
  that select variants at compile time.

**7. The small 2002 directives.** 7.3.9 CALL-CONVENTION, 7.3.17 LEAP-SECOND, 7.3.18 LISTING, 7.3.19
PAGE; Ed. 2002; S; deps 6 (shared directive parser). DONE 2026-10-06 (directives.md): each read
and checked, none with an effect here (no listing; POSIX time has no leap second; COBOL the one
call convention).
- Today: each refused by the generic directive message. LISTING and PAGE can be accepted without
  effect (no listing is produced); LEAP-SECOND needs a decision about the time service.
- Why here: cheap once 6 exists.

**8. The rest of the CALL family.** 14.9.4, 11.5, 12.3.8, 14.8; Ed. 2002; M; deps none. DONE
2026-10-06 (call.md; tests 2002/fnproto, pgproto, fnvarying): function and program prototypes
(IS PROTOTYPE, the definition checked against it), FUNCTION-ID / PROGRAM-ID / REPOSITORY `AS
literal`, REPOSITORY PROGRAM, CALL format 2 (prototype-name, AS prototype-name, AS NESTED) with
14.8.2's conversion of BY CONTENT / BY VALUE arguments and expressions, BY VALUE and OPTIONAL
parameters of a function, OMITTED arguments, sixteen function parameters (the result's address
through cob_call_retaddr, signature file version 2), a user function in VARYING's subscript, FROM
and BY. Left: NESTED to a program defined later in the group passes as format 1 (call.md's gap).
- Today: "function prototypes (IS PROTOTYPE) are not implemented yet", "FUNCTION-ID ... AS literal",
  "REPOSITORY FUNCTION ... AS literal", "a BY VALUE parameter of a function", "OMITTED arguments
  (OPTIONAL parameters) are not implemented yet" (for functions; CALL has them), "a user-defined
  function in ..." (g_ufn_forbid), and a limit of 7 USING items in a function (not listed in
  refusals.md section 4).
- Why here: finishes the user-defined-function module already in use by majesty's date family.

**9. Program pointers.** 8.5.2.15, 13.18.60, 14.9.39 format 9; Ed. 2002; S-M; deps 8 for
prototype-typed pointers. DONE 2026-10-06 (usage.md's last table; test 2002/pgpointer): USAGE
PROGRAM-POINTER [TO prototype], ADDRESS OF PROGRAM (literal, item, prototype-name; NULL and
EC-PROGRAM-NOT-FOUND when not here), SET format 9 with the category and restriction rules, CALL
through the pointer (EC-PROGRAM-PTR-NULL, ON EXCEPTION; a restricted one's arguments converted by
the prototype), relations, INITIALIZE. SET ... TO ENTRY stays refused as IBM's, naming the standard's
form; CALL through a data-pointer, accepted before, is refused.
- Today: `USAGE PROGRAM-POINTER` refused ("unexpected 'program-pointer'"), `SET p TO ENTRY "x"`
  refused. CALL through a data pointer is accepted. INITIALIZE names the category but says "no such
  items exist here".
- Why here: a 2002 data category, required, and small.

**10. Reference-modification leftovers.** 8.4.3.3, 14.9.25 GR 1, 14.9.20; Ed. 2002; M; deps none.
DONE 2026-10-06 (docs/conformance/refmod.md, the first sweep of 8.4.3.3; tests 2002/refmodrest,
refmodbit, free/posrefmod2): every hole in the "Today" list closed -- MOVE's rule 1 snapshot, the
screen and positioned forms, LENGTH OF and the string functions of a computed part, INITIALIZE of a
part with the 2002 phrases, BY CONTENT of a bit part; two of the messages named cases that could not
arise (a function's refmod with an expression length, a national sender to a numeric part).
- Today (refusals.md section 2, verified by message): MOVE with a computed-length sender a receiver
  changes; computed length in a screen item and positioned ACCEPT; LENGTH OF / BYTE-LENGTH / other
  functions of a variable-length refmod; a function's refmod with an expression length; INITIALIZE
  of a refmod with the 2002 phrases; refmod numeric receiver of national data.
- Why here: each is a hole inside an implemented statement.

**11. Data division leftovers.** 13.16 rule 9, 13.18.49, 13.18.1, 13.18.22, 13.10, 13.18.57,
13.18.5, 13.4.5; Ed. 2002; M (splittable); deps 6 for constant FROM. DONE 2026-10-06
(data-division.md's new tables; tests 2002/sameas, impliedpic, aligned, externalas, basedlocal,
fdnorec): the implied PICTURE, SAME AS (expanded over the tokens as TYPE is), ALIGNED (with
occurrences on bytes), EXTERNAL AS, a TYPE or SAME AS expanding past level 49, BASED in
LOCAL-STORAGE (NULL at each activation), an FD without a record description with WRITE FILE /
REWRITE FILE ... FROM and READ INTO. The GLOBAL constant entry was already there (item 6).
EC-BOUND-PTR is a ruling: not raised, the machine faults instead.
- Today: implied PICTURE from a VALUE ("not implemented"); `SAME AS` ("not implemented"); `ALIGNED`
  (bare parse error); `EXTERNAL AS literal`; a `GLOBAL` constant entry ("the constant entry ... is
  not implemented"); a TYPE expanding past level 49; a BASED entry in LOCAL-STORAGE and
  EC-BOUND-PTR; an FD with no record description under -std=2002.
- Why here: each is a single clause in a module that otherwise works.

**12. Bit-data leftovers.** 13.18.38, 13.18.44; Ed. 2002; S-M; deps none. DONE 2026-10-06
(national-boolean.md's leftovers table; tests 2002/bitoccurs, posbits): OCCURS on a bit group (one
bit dimension per item: its own or an occurring bit group's above it), expression subscripts of bit
items, OCCURS DEPENDING ON a bit array, bit parts and elements in screen items and positioned
DISPLAY/ACCEPT; BY CONTENT of a bit part came with item 10. A character item redefining a bit item
that starts inside a byte is a ruling (refused). Still refused: a bit table inside an occurring bit
group; a bit part at a computed position in a SCREEN SECTION item.
- Today: "OCCURS DEPENDING ON a USAGE BIT item", "OCCURS on a bit group", "a character item at a bit
  position", "an arithmetic-expression subscript of a bit data item", "BY CONTENT of a
  reference-modified bit item", "a bit item's part in a screen item" -- all "not implemented".
- Why here: finishes BOOLEAN part three.

**13. 31-digit floating intrinsics.** 15.x; Ed. 2002 (31 digits); M; deps none. DONE 2026-10-06
(docs/wide.md's last section; test 2002/widefloatfn): the queue's "Today" was already stale -- phase
3 (ISSUES-117) had put them on the wide stack -- and what remained was exactness: SIN/COS/TAN reduce
by 2 pi in decimal, double-to-wide is exact, wide-to-double is one rounding. Results are 15
significant digits by ruling; the soft libm's last digit is runtime ISSUES-29.
- Today: SQRT, LOG, trigonometric, MEAN, MEDIAN, VARIANCE, STANDARD-DEVIATION, ANNUITY,
  PRESENT-VALUE refuse arguments or results past 18 digits (docs/wide.md).
- Why here: the rest of 31-digit COBOL landed (ISSUES-117).

**14. CURRENCY with PICTURE SYMBOL; ANYCASE.** 12.3.7, 15.68, 15.94; Ed. 2002; S; deps none. DONE
2026-10-06 (picture.md, functions.md; tests 2002/currencystr, numvalanycase): the currency string
in the editor's kernel (kern.h, after the PICTURE's NUL, its length in the locale word -- the DBT
rebuilt on the new tag), fixed and floating insertion, de-editing, the wide path, NUMVAL-C's
default; ANYCASE on NUMVAL-C and TEST-NUMVAL-C. Ruling: one currency symbol per source unit (rule
21 allows several).
- Today: `CURRENCY SIGN IS "EUR" WITH PICTURE SYMBOL "$"` refused ("the literal is one character",
  then "SPECIAL-NAMES clause 'with'"); `NUMVAL-C(a "EUR" ANYCASE)` refused ("'anycase' is not
  declared").
- Why here: small, and the first thing a European-currency program hits.

## Tier 2 -- whole 2002 features still missing (required, not optional)

**15. Floating-point literals and numeric-edited items.** 8.3.3.3, 13.18.40 (floating-point
numeric-edited), 14.6.8.3-4; Ed. 2002; M; deps none. DONE 2026-10-06 (lexical.md, picture.md;
tests 2002/floatlit, fpedited): the lexer is Ragel -G2 (src/lex.rl, one grammar read by both the
text-word scanner and the tokenizer -- the whole suite and the gates byte-identical through the
change), floating-point literals written as the fixed-point value they are worth, floating-point
numeric-edited pictures edited in the kernel (and read back), the item going the wide way in
arithmetic.
- Ruled 2026-10-06: the lexer moves to Ragel -G2 as part of this item, as picture.rl did for
  PICTURE. One token grammar (8.3: words, literals and their prefixes, separators, the period
  rule, floating-point literals) generating both the text-word scanner of copy.h (tw_lex) and the
  token scanner of tokenizer.h, which today duplicate each other by hand. Outside the machine, as
  now: reference format and continuation (reader.h), PICTURE strings (context after PIC), EXEC SQL
  text, and DECIMAL-POINT IS COMMA's swap. The gates are the net.
- Today: `FLOAT-SHORT` / `FLOAT-LONG` / `FLOAT-EXTENDED` are accepted under -std=2002, but `1.5E+3`
  is refused ("a period must be followed by a space") and `PIC +9.9(5)E+99` is refused ("not valid
  at character 8").
- Why here: the 2002 float usages are half there; this is the other half, and item 20 (IEEE usages)
  builds on it.

**16. The OPTIONS paragraph.** 11.9, 11.9.5 ARITHMETIC (NATIVE), 11.9.7 ENTRY-CONVENTION; Ed. 2002;
S-M; deps none. DONE 2026-10-06 (options.md; test 2002/optionspara): the paragraph with every
clause parsed -- ARITHMETIC IS NATIVE, ENTRY-CONVENTION IS COBOL and DEFAULT ROUNDED MODE (item
22's first half, inherited by contained programs) taken; STANDARD by name; STANDARD-BINARY/
-DECIMAL, FLOAT-BINARY/-DECIMAL, INTERMEDIATE ROUNDING and INITIALIZE refused by name for items
22, 20, 22 and 30.
- Today: "unexpected 'options' in the IDENTIFICATION DIVISION". The 2014 and 2023 clauses (items 22,
  30) and 2023's INITIALIZE clause hang off it. ARITHMETIC IS STANDARD was made obsolete by 2014 and
  removed by 2023: take NATIVE, refuse STANDARD by name.
- Why here: a 2002 paragraph that later editions keep extending.

**17. START and READ positioning.** 14.9.41 (FIRST, LAST, WITH LENGTH, sequential files), 14.9.30
PREVIOUS; Ed. 2002; S-M; deps 1. DONE 2026-10-07 (io-statements.md; tests 2002/startfirst, startseq):
START FIRST and LAST of indexed (by the prime key, which becomes the key of reference), relative
(the first or last existing record) and sequential files (by position, variable-length records
walked); WITH LENGTH arithmetic-expression, the leading characters of an indexed file's key, 23
outside 1 to the key's length; READ PREVIOUS of a sequential file of fixed-length records, 46
either way past the end. Ruled: READ PREVIOUS of variable-length sequential records refused (no
fixed place to step back to); LINE SEQUENTIAL stays out of both.
- Today: START FIRST/LAST and WITH LENGTH are bare parse errors; START on a sequential file refused;
  READ PREVIOUS of a sequential (not line sequential) file "not implemented" (refusals.md section
  2).
- Why here: required 2002 I-O; the indexed runtime already has the positioning machinery for READ
  PREVIOUS.

**18. Dynamic file assignment.** 9.1.21, 12.4.5 ASSIGN USING; Ed. 2002; S; deps none. DONE
2026-10-07 (files.md; tests 2002/assignusing, assignusing2): ASSIGN USING data-name, the item's
content at OPEN, SORT or MERGE naming the file, 31 when it holds spaces; ASSIGN TO literal USING
data-name, the literal until the item holds a name (ruling). The item must be declared,
alphanumeric and outside the file's record.
- Today: `ASSIGN USING k` refused ("unexpected 'k' in SELECT f"). `-dialect=mf` already takes an
  undeclared data-name in ASSIGN TO (BP-D6); the standard form is the same run-time path.
- Why here: required, small, and the run-time path exists.

**19. Conformance sweeps of 2002/85 material not yet swept.** M each; deps none. Each is a page in
docs/conformance/ with every rule marked:
- 19a. 8.3-8.4: literals, figurative constants, qualification, subscripts, identifiers, refmod,
  LINAGE-COUNTER, report counters, condition-names (coverage.md: 22 unswept elements in clause 8).
  DONE 2026-10-07 (identifiers.md; test 2002/identifiers, twelve bad tests): eight unenforced
  rules refused by name (ALL ZERO as a numeric literal, ALL of a figurative, an index-name on
  another table, ALL and two-integer index subscripts, a function-identifier receiving, FUNCTION
  omitted without a REPOSITORY, OMITTED to an intrinsic, MOVE NULL to an alphanumeric item,
  LINAGE-COUNTER and LINE-COUNTER receiving) and one leniency closed (an unqualified paragraph-
  name held by two sections, from outside both). The OO formats n/a by ruling.
- 19b. 8.8.4.3-8.8.4.11 and 8.7.5: class, condition-name, switch, sign, omitted-argument, negated
  and combined conditions. DONE 2026-10-07 (conditions.md; test 2002/condsweep, six bad tests):
  class-condition rules 1, 3, 4, 5 enforced; a class condition of a function's result and by an
  alphabet-name taken; NUMERIC of a truncating binary within its PICTURE; a float's bare sign test
  by its sign bit; NOT NOT / NOT OR / NOT AND refused; a crash fixed (class test of a part, the
  unit's first descriptor).
- 19c. 10.6, 10.7, 11.5, 11.10: compilation group, end markers, FUNCTION-ID, PROGRAM-ID. DONE
  2026-10-07 (environment.md; 11.5 and 11.10 were call.md's since item 8): prototypes first,
  the prototype restrictions 4a-4e, a containing program's END PROGRAM required.
- 19d. 12.3.5-12.3.8, 12.4.4, 12.4.6: SOURCE-/OBJECT-COMPUTER, SPECIAL-NAMES, REPOSITORY,
  FILE-CONTROL, I-O-CONTROL (SAME). Probe note: `OBJECT-COMPUTER ... CHARACTER CLASSIFICATION IS
  LOCALE` is accepted silently (unverified whether parsed or skipped). DONE 2026-10-07
  (environment.md): it was skipped; refused by name now, with FOR NATIONAL and APPLY COMMIT;
  the SAME clauses' rules 2-10 enforced; a contained program inherits the PROGRAM COLLATING
  SEQUENCE (it did not).
- 19e. 13.4.6 SD, 13.18.5 BASED, 13.18.57-58 TYPE/TYPEDEF, 14.7.4 ROUNDED, 14.9.3 ALLOCATE, 14.9.15
  FREE, 14.9.47 UNLOCK (the last three implemented but unswept). DONE 2026-10-07
  (environment.md): a sort file in no I-O statement, an SD with a record; TYPE rules 2 and 5
  and the subject's own VALUE (GR 3); UNLOCK's I-O status, not of a sort file.
- 19f. Re-key reportwriter.md to 2023 numbers (13.8, 13.14, 13.15 and the RW clauses), so the matrix
  credits what was swept. DONE 2026-10-07: the headings name the 2023 sections (and screen.md 13.9).
- Why here: CCVS tests acceptance, not refusal; every sweep so far found unenforced rules. Cheap,
  and it feeds tier 1.

## Tier 3 -- COBOL 2014 additions (introduce `-std=2014`)

**20. `-std=2014` and the IEEE usages.** 13.18.60 FLOAT-BINARY-32/64/128, FLOAT-DECIMAL-16/34,
11.9.8-9 FLOAT-BINARY/FLOAT-DECIMAL clauses; Ed. 2014; binary M, decimal L; deps 15, 16. DONE
2026-10-07 (usage.md, options.md, docs/usage.md; tests 2014/floatdec, floatbin; libcob/ieee.h with
tests/ieee_test.c + ieee_vectors.py): the switch, with tests/2014; the five usages, binary32/64 on
the hardware's floats, binary128, decimal64 and decimal128 in software on the wide stack's new
floating mode; both encodings, both byte orders, the OPTIONS defaults; TRIM and ROUNDED MODE
unwarned under 2014.
- Today: "USAGE float-binary-64 is COBOL 2014 (ISO/IEC 60559 formats); not implemented". The owner's
  plan (refusals.md section 3) makes these the first piece of a 2014 switch. Binary32/64 are SLOW-32
  hardware; binary128 and both decimal formats need a software implementation. The switch should
  also take over the 2014 points taken early as class E (BP-E27 TRIM, BP-E29 ROUNDED MODE, and `<>`,
  accepted today under -std=2002).
- Why here: the owner's own chosen entry point to 2014.

**21. Float class, sign and content.** 8.8.4.3, 8.8.4.7, 14.9.39 format 15 (`SET CONTENT OF`); Ed.
2014; M; deps 20. DONE 2026-10-07 (conditions.md, set.md, docs/usage.md; test 2014/floatcontent):
the seven class conditions and SET CONTENT OF's five values with SIGN, over every numeric usage and
every float format; the sign conditions were 19b's. Ruled: IN-ARITHMETIC-RANGE is a no-op here;
FLOAT-SHORT/-LONG/COMP-2 take the floating-point forms.
- Today: `IF a IS INFINITY` and `SET CONTENT OF a TO FARTHEST-FROM-ZERO` are bare parse errors.
- Why here: the operations that make the IEEE usages usable.

**22. Rounding options.** 11.9.6 DEFAULT ROUNDED, 11.9.11 INTERMEDIATE ROUNDING; Ed. 2014; S-M; deps
16. DONE 2026-10-07 (options.md, docs/wide.md; test 2014/iround): INTERMEDIATE ROUNDING's four
modes applied wherever the stacks shed digits, the unit's own by its activation descriptor,
its arithmetic by the stack paths; DEFAULT ROUNDED was item 16's.
- Today: DEFAULT ROUNDED came with item 16 (2026-10-06); INTERMEDIATE ROUNDING refused by name;
  `ROUNDED MODE IS` on a statement already works (BP-E29). A.3 makes both clauses
  processor-dependent.
- Why here: the per-statement half exists.

**23. EVALUATE partial expressions.** 14.9.13; Ed. 2014; S; deps none. DONE 2026-10-07 (evaluate.md;
test 2014/partialwhen, GnuCOBOL agrees): the subject supplied to the condition parser as the first
simple condition's left operand, so relations, class and sign forms and their abbreviations follow.
- Today: "a partial expression as a WHEN object (COBOL 2014) is not implemented".
- Why here: common in modern code, isolated.

**24. Zero-length items.** 8.3.3 (zero-length literals), 8.5.4, 7.3.23 REF-MOD-ZERO-LENGTH (2023);
Ed. 2014/2023; M; deps 6 for the directive. DONE 2026-10-07 (refmod.md "8.5.4 Zero-length items";
tests 2014/zerolen (GnuCOBOL takes "" as one SPACE and a zero-length delimiter as matching
everywhere: docs/oracles.md), 2014/zerolen2): the literals under -std=2014, the directive as a
positional one, a part of length zero through its own runtime entries (`cob_refmod_desc_z` ...),
the text's prohibitions each refused by rule (16 bad tests), class conditions of a zero-length
item false. Found on the way: every ACCEPT ... FROM into a reference-modified item wrote the whole
item's length at the part's address (2002/acceptrm).
- Today: "a zero-length alphanumeric literal is COBOL 2014".
- Why here: 2014 introduced zero-length literals; 2023 completes them for reference modification.

**25. Structured constants.** 13.18.15 CONSTANT RECORD; Ed. 2014; M; deps none (2023 E.2 item 10
ties EXTERNAL use to strong typing). DONE 2026-10-07 (data-division.md "13.18.15 CONSTANT RECORD";
test 2014/constrec, no oracle: GnuCOBOL 4 does not take the clause; 17 bad tests): the record laid
out in .rodata, where a store faults; the receiving uses refused at each storing statement; the
clause rules and those of OCCURS, REDEFINES, RENAMES, SAME AS, TYPEDEF and USAGE that name it;
EXTERNAL with it (a strongly typed TYPE) not implemented.
- Today: "the CONSTANT RECORD clause is COBOL 2014, beyond -std=2002".

**26. Function pointers.** 8.5.2.7, 8.4.3.12 (ADDRESS OF FUNCTION), 14.9.39 format 8; Ed. 2014; M;
deps 8 (prototypes), 9. DONE 2026-10-07 (usage.md "FUNCTION-POINTER" rows; test 2014/fnpointer, no
oracle: GnuCOBOL 4 has none; 11 bad tests): USAGE FUNCTION-POINTER TO prototype, ADDRESS OF
FUNCTION by a prototype (linked by name) or an identifier (a function registry every function joins
at start-up, NULL and EC-FUNCTION-NOT-FOUND when the name is not there), SET format 8 with rule 20
by signature, the invocation pointer(arguments) through r12 with EC-FUNCTION-PTR-NULL, INITIALIZE's
FUNCTION-POINTER category, the pointer relations.
- Today: `USAGE FUNCTION-POINTER` and `ADDRESS OF FUNCTION` are bare parse errors.

**27. The 2014 date-and-time functions.** 15.17, 15.38-15.41, 15.48, 15.79, 15.80, 15.92; Ed. 2014;
M; deps none. DONE 2026-10-07 (functions.md "The 2014 international date and time functions";
tests 2014/dtformat (GnuCOBOL agrees but for the basic fractional separator: docs/oracles.md),
2014/dtformat2 (national, the comma, the clock's fraction, EC-ARGUMENT-FUNCTION); nine bad
tests): the formats of 15.3.1-15.3.3 in one shared parser (libcob/dtfmt.h), rendering and
scanning in libcob, TEST's position of the first error by the text's examples. Found on the way:
a >>TURN after a 45296,5 under DECIMAL-POINT IS COMMA applied one statement late (2002/turncomma).
- Today: each "FUNCTION ... is COBOL 2014; not implemented". In scope: the owner ruled on
  2026-10-06 that everything standard holds (refusals.md section 3 amended).

**28. Table SORT completion.** 14.9.40 (ALL subscript, nested tables); Ed. 2014; S-M; deps none. DONE
2026-10-07 (sort.md rules 13-14; tests 2002/sortnested (GnuCOBOL agrees), 2002/sortall (no oracle:
GnuCOBOL refuses ALL), bad/std2002-sort-nested): the table written with the outer subscripts, its
own omitted or ALL; the selected table sorted in place through the same cob_sort_table.
- Today: "a table SORT of a table inside another table is not implemented".

**29. 2014 behaviour-change audit.** 2014 E.2 items 1-29; Ed. 2014; S-M. One row per item: does
-std=2002 already behave the 2014 way, and does it need a behavior point. Includes PICTURE length 63
(E.3 19) and currency symbols outside the repertoire (E.3 7, 18) -- both unverified. DONE 2026-10-07
(docs/conformance/edition-2014.md, one row each; no behaviour point needed, every one is the
edition's rule). Changed under -std=2014: CLOSE NO REWIND 07 (7), hex currency symbol refused (10),
ALL INTRINSIC reserves the function names (13, and 2002's names under both editions), debugging
lines / DEBUGGING MODE / PADDING refused (19), the 2014 reserved words (24), PICTURE 63 (E.3 19);
E.3 18 (a currency symbol outside the repertoire) stays a gap. Found on the way: a float into a
floating-point numeric-edited item lost its exponent; a 1.0E-200 literal overran the tokenizer.

## Tier 4 -- COBOL 2023 additions (introduce `-std=2023`)

**30. `-std=2023` and its removals.** 2023 E.2 item 1, item 21; S-M; deps 20. The removals go behind
the switch as behavior points (already on behavior-points.md's watch list): word continuation in
fixed form, CALL ON OVERFLOW, CLOSE WITH LOCK and status 38, figurative constants to numeric items
(except ALL digit-literal to integer, BP-O9), non-pseudo- text COPY REPLACING operands, EXIT
FUNCTION/METHOD, QUOTE to numeric. The OPTIONS INITIALIZE clause (11.9.10) lands here too. Also:
SYNCHRONIZED on a group is a 2023 addition (E.3.2 item 6; 2002 13.16.53 SR 1 says elementary only)
but -std=2002 accepts it today. DONE 2026-10-07 (docs/conformance/edition-2023.md; behavior-points.md
class R; tests 2023/optinit, 2014/removed2023, fixed/wordcont; eight bad tests): -std=2023; BP-R1 to
BP-R5 taken through 2014 and refused under 2023 (EXIT FUNCTION implemented with its point; the
figurative-constant moves were refused under every edition already, by ruling); OPTIONS
INITIALIZE; SYNCHRONIZED on a group as 2023 has it (the note above was stale: -std=2002 refused it).

**31. Small 2023 statements.** S each; deps 30. DONE 2026-10-07 (edition-2023.md "The small 2023
statements" and the statements' pages; tests 2023/stmts2023, 2023/inspback; eleven bad tests): all
seven below. Found on the way: a LINAGE file's WRITE BEFORE n then WRITE AFTER m lost a line.
- XOR / EXCLUSIVE-OR logical operator (8.7.6) -- bare parse error today.
- INSPECT BACKWARD (14.9.22) -- bare parse error today.
- DELETE FILE (14.9.10) -- bare parse error today.
- WRITE with both BEFORE and AFTER ADVANCING (14.9.51) -- refused today.
- CONTINUE AFTER n SECONDS, EC-CONTINUE-* (14.9.9) -- "COBOL 2023 (14.9.9); not implemented".
- GOBACK WITH status (14.9.18) -- "COBOL 2023 (14.9.18); not implemented".
- USAGE PACKED-DECIMAL NO SIGN (13.18.60) -- bare parse error today.

**32. VALUE rules for numeric-edited items.** 13.18.63 rule 6; 2023 E.2 items 27-29, E.3.3 item 43;
S; deps 30. DONE 2026-10-07 (value.md rules 6-7; test 2023/numedvalue, no oracle; six bad tests): the
kernel compiled into the compiler edits the literal; ZERO the literal zero; an alphanumeric literal
checked to be the picture edited; all under -std=2023, as E.3.3 item 43 dates them.
- Today: "a numeric VALUE for the numeric-edited item ... is not implemented; write it edited".

**33. The 2023 functions.** 15.12 BASECONVERT, 15.18 CONCAT, 15.19 CONVERT, 15.37 FIND-STRING, 15.65
MODULE-NAME, 15.83 SMALLEST-ALGEBRAIC, 15.87 SUBSTITUTE, and EXCEPTION-FILE(-N)'s optional argument;
M; deps 30; in scope (see item 27). **DONE 2026-10-07**: all eight under -std=2023
(conformance/functions.md "The 2023 functions"; tests 2023/fn2023, 2023/excfile; 23 bad tests).
MODULE-NAME's activation stack is the runtime's (cob_act_enter), a contained program part of its
outermost program's module; SMALLEST-ALGEBRAIC folds at compile time; the string functions give
run-time-length results as TRIM does.
- Was: each "FUNCTION ... is COBOL 2023; not implemented".

**34. 2023 directives.** 7.3.12 DISPLAY, 7.3.15 FLAG-14, 7.3.20 POP, 7.3.22 PUSH, 7.3.23
REF-MOD-ZERO-LENGTH (with 24), 7.3.10 COBOL-WORDS; S each except COBOL-WORDS (M: it edits the
reserved-word table); deps 6.

**35. EXTERNAL conformance checking.** EC-EXTERNAL-*; 2023 E.2 items 9, 10, 12, 24; M; deps 4.
- Today: names are in the table; none raised (heuristic, unverified).

**36. 2023 behaviour-change audit.** 2023 E.2 items 2-30; S-M. Notable: item 19 (INVALID KEY and
other I-O exceptions now reach USE declaratives when no phrase is given) may already differ from
-std=2002 behaviour; items 15-18 (I-O status 04, 07, 0x, 37); item 20 (MERGE in an output
procedure); item 26 (transfer-of-control checks); item 30 (WRITE end-of-page). Item 22 (READ
PREVIOUS after OPEN) is already the implemented behaviour (ISSUES-116).

## Tier 5 -- optional (2023 A.4) or processor-dependent (2023 A.3)

The standard lets a conforming implementation omit all of these. The first two finish modules that
are already mostly built.

**37. Report Writer, 2002 additions.** A.4.11; 13.18.41 PRESENT WHEN, 13.18.14 COLUMN
PLUS/LEFT/RIGHT/CENTER and several numbers, 13.18.64 VARYING, OCCURS in report groups; Ed. 2002; L;
deps 19f.
- Today: "... is COBOL 2002's Report Writer; not implemented (the 1985 module is)". Recorded in
  conformance/reportwriter.md but missing from refusals.md section 2.

**38. Screen section leftovers.** A.4.2; 13.17, 14.9.39 format 6; Ed. 2002; M; deps none.
- Swept 2026-10-06: docs/conformance/screen.md, whose "Open" list is this item -- GLOBAL; colours,
  LINE and COLUMN from identifiers; MINUS; LINE/COLUMN phrases on ACCEPT and DISPLAY of a screen;
  DISPLAY ... ON EXCEPTION; OCCURS on groups and over FROM/TO/USING; FROM with TO; a FROM numeric
  literal; BLANK LINE; BLANK SCREEN's default colours; EC-SCREEN; combined attributes; national
  JUSTIFIED and USAGE NATIONAL; plus `SET ... ATTRIBUTE` (a bare parse error). Three rulings are
  open there: BLANK SCREEN during ACCEPT, the PLUS column count, the 9(4) CRT STATUS item.

**39. File sharing and record locking.** A.4.7; 9.1.15-16, 12.4.5.9, 12.4.5.15, 14.7.9 RETRY, OPEN
SHARING, READ/WRITE WITH [NO] LOCK, EC-I-O-FILE-SHARING; Ed. 2002; L; deps 1.
- Today: LOCK MODE and SHARING clauses parse; every statement-level form is a bare parse error. One
  run unit per engine makes the semantics mostly local, but the statuses and RETRY need defining.

**40. WRITE FILE and REWRITE FILE.** A.4.13; 14.9.35, 14.9.51; Ed. 2002; S; deps 11 (FD without
record description).
- Today: bare parse error.

**41. FORMAT and SELECT WHEN.** A.4.8; 13.18.24, 13.18.51; Ed. 2002; M.
- Today: "unexpected 'format' in FD".

**42. RESUME.** A.4.12; 14.9.33; Ed. 2002; S-M; deps 3.
- Today: "RESUME is not implemented (COBOL 2014 made it optional)".

**43. Dynamic-length items.** A.4.5; 13.18.19, SPECIAL-NAMES DYNAMIC LENGTH STRUCTURE, SET format
16; Ed. 2014; L.
- Today: "the DYNAMIC LENGTH clause is COBOL 2014, beyond -std=2002".

**44. Dynamic-capacity tables.** A.4.4; 13.18.38 format 4, SET format 14,
EC-BOUND-OVERFLOW/-SET/-TABLE-LIMIT, EC-FLOW-SEARCH; Ed. 2014; L.
- Today: "expected a count after OCCURS" (bare).

**45. Locale support and STANDARD-COMPARE.** A.4.9; 15.51-15.54, 15.85, LOCALE on UPPER-/LOWER-CASE
and TEST-NUMVAL-C, SET formats 11-12, SPECIAL-NAMES LOCALE, PICTURE locale format, CHARACTER
CLASSIFICATION; Ed. 2002; L. Today: functions refused naming "locale support" or "ISO/IEC 14651";
SPECIAL-NAMES LOCALE and SET LOCALE refused. In scope (ruled 2026-10-06; see item 27).

**46. Commit and rollback.** A.4.3; 9.1.18, 12.4.6.3 APPLY COMMIT, 14.9.7, 14.9.36,
EC-FLOW-*-COMMIT/ROLLBACK; Ed. 2023; L.
- Today: "COMMIT is COBOL 2023; not implemented".

**47. Extended letters.** A.4.6; 8.1.3; Ed. 2002; S-M. Unverified what the tokenizer accepts in
user-defined words today (national literals and UTF-8 data work; names were not probed).

**48. Asynchronous messaging.** A.3 item 4; 14.9.31 RECEIVE, 14.9.38 SEND, USAGE MESSAGE-TAG, SET
format 17, EC-MCS-*; Ed. 2023; L.
- Today: refused as "the Communication module" (mislabelled, item 2). Processor-dependent: needs a
  run-unit-to-run-unit channel the MMIO rings do not have; owner's call.

**49. Standard arithmetic modes.** A.3 items 1-3; 11.9.5 STANDARD-DECIMAL / STANDARD-BINARY; Ed.
2014; L; deps 16, 20. STANDARD-BINARY is obsolete in 2023 (F.2 item 3) and, by the text, no provider
has it: candidate for the same treatment as VALIDATE.

**50. The screen behaviours that differ from the text** (docs/conformance/screen.md's three
rulings). Ed. 2002; S each; deps none. By the owner's rule (everything standard holds) the text is
the target: BLANK SCREEN ignored during an ACCEPT (13.18.7.4 rule 5); COLUMN PLUS 1 immediately
after the item before (13.18.14.4 rule 15; today one column further, as GnuCOBOL counts); CRT
STATUS an alphanumeric item of four characters (12.3.7.3 rule 30; today PIC 9(4) and Micro
Focus's three bytes are taken too). Each changes what existing programs see, so each lands with the
old behaviour kept under -dialect=gnucobol or -dialect=mf where a program needs it, as ACAS does.

## Tier 6 -- object orientation, last

**51. Object orientation.** 9.3, 11.3-11.8, 14.9.21 INVOKE and the rest of the OO module, RAISE of
an exception object, USE AFTER EXCEPTION OBJECT, REPOSITORY class entries; Ed. 2002; L (a design
note first). Required by the text: 2023 A.4.10 makes only multiple inheritance and parametric
polymorphism optional. Ruled 2026-10-06: deferred, not excluded -- it comes after everything above,
so that the standard is implemented as far as it can be. Its runtime (object references, dispatch,
storage) lives in libcob above SLOW-32, as the rest does.

## Out by ruling

| element | ruling |
|---|---|
| VALIDATE and its clauses (CLASS, DEFAULT, DESTINATION, INVALID, VALIDATE-STATUS, VALUE format 5) | not built: obsolete in 2023 (F.2 item 5), optional since 2014 (A.4.13 of 2014) |
| The Communication module (ENTER, 85-style SEND/RECEIVE, CD) | out; removed by 2014 (E.2 item 19) |
| The Debug module (USE FOR DEBUGGING, debugging lines, `>>D`) | out; removed by 2014 (E.2 item 19) |
| COMP-1 / COMP-2 as IBM hexadecimal float | out (they are taken as IEEE under BP-E3); the standard's FLOAT-* usages are in scope (items 15, 20) |
| Micro Focus screen extras; vendor extensions generally | not the target; dialect forms only behind `-dialect=` when a program needs them |
| JSON / XML GENERATE and PARSE | not in any edition (see Scope) |

Not queued because the standard itself removed them: PADDING CHARACTER (2014), FLAG-85,
FLAG-NATIVE-ARITHMETIC and ARITHMETIC IS STANDARD (2023), and the 2023 removals of item 30 (kept for
85/2002 programs, refused under -std=2023). FLAG-02 is obsolete in 2023 (F.2 item 1):
accept-and-ignore at most.

## Checked and already done

Verified by probe or by tests/2002 unless noted: RECURSIVE and LOCAL-STORAGE; FUNCTION-ID,
REPOSITORY, FUNCTION ALL INTRINSIC; free form, `>>SOURCE`, floating continuation; `>>TURN`, RAISE,
USE AFTER EXCEPTION, EXCEPTION-STATUS/-LOCATION/-FILE, SET LAST EXCEPTION; the 2023
exception-checking PERFORM; EXIT PERFORM [CYCLE] / PARAGRAPH / SECTION and PERFORM UNTIL EXIT
(2023); NATIONAL throughout; BOOLEAN, B-XOR, and the 2023 shifts B-SHIFT-L/-RC (accepted under
-std=2002); USAGE BIT; TYPEDEF and STRONG; ALLOCATE, FREE, BASED, ADDRESS OF, data pointers; ANY
LENGTH; `01 name CONSTANT AS`; 31-digit arithmetic; ROUNDED MODE (2014, BP-E29); TRIM with 2023's
characters (BP-E27); `<>` (2014); `&` concatenation; 63-character words (2023); STOP RUN WITH
STATUS; FLOAT-SHORT/-LONG/-EXTENDED (2002); READ PREVIOUS for indexed and relative; split keys
(SOURCE); ALPHABET ... EBCDIC; LINE SEQUENTIAL (2023); INITIALIZE's 2002 phrases; ACCEPT FROM DATE
YYYYMMDD, DAY YYYYDDD, ARGUMENT-NUMBER, COMMAND-LINE; SPECIAL-NAMES SYMBOLIC CHARACTERS, CRT STATUS,
CURSOR; EC-I-O-WARNING (2023; six tests); the 1985 Report Writer; SCREEN SECTION's core
(ISSUES-125).

## Contradictions with the existing docs

1. refusals.md section 3 says FLOAT-SHORT/-LONG/-EXTENDED are COBOL 2014; they are 2002 (2002 USAGE
   clause) and -std=2002 accepts them. Only FLOAT-BINARY-n and FLOAT-DECIMAL-n are 2014.
2. standards.md says 2023's optional list "adds VALIDATE"; VALIDATE was already optional in 2014
   (2014 A.4.13). 2023 added Commit and Rollback and dropped ARITHMETIC IS STANDARD.
3. standards.md says the OO deferral "now has the standard's own blessing"; 2014 A.4.9 and 2023
   A.4.10 make only multiple inheritance and parametric polymorphism optional.
4. standards.md and the VALIDATE refusal message attribute to Annex E the statement that no provider
   has implemented VALIDATE; 2023 Annex E does not mention VALIDATE. The statement is F.2 item 5 (D.22
   does carry the "obsolete" note, so that half of the citation stands).
5. conformance/clauses.md says 2002 allows SYNCHRONIZED on a group; 2002 13.16.53 SR 1 says
   elementary only. Group SYNC is 2023 (E.3.2 item 6).
6. coverage.md credits 11.9.10, 12.4.5.9 and 12.4.5.15 as swept (item 2) and labels 2023
   SEND/RECEIVE as the Communication module.
7. refusals.md section 2 omits the 2002 Report Writer additions (in conformance/reportwriter.md),
   and every bare-parse-error gap of item 1.
8. refusals.md section 3 ruled the 2014/2023 functions and the locale functions out. Resolved
   2026-10-06: the owner ruled everything standard in scope, and section 3 was amended.
