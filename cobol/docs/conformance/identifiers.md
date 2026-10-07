# Literals, figurative constants, qualification, subscripts, identifiers: 8.3.3.2, 8.3.3.6, 8.4.2.2, 8.4.2.3, 8.4.3.1, 8.4.3.2, 8.4.3.10, 8.4.3.11, 8.4.3.12, 8.4.3.14, 8.4.3.15, 8.4.4

Swept 2026-10-07 (docs/plans/standard-queue.md item 19a). 2023: 8.3.3.2
alphanumeric literals, 8.3.3.6 figurative constant values, 8.4.2.2
qualification, 8.4.2.3 subscripts, 8.4.3.1 identifier, 8.4.3.2
function-identifier, 8.4.3.10 NULL, 8.4.3.11 data-address-identifier,
8.4.3.12 function-address-identifier, 8.4.3.14 LINAGE-COUNTER, 8.4.3.15
report counters, 8.4.4 condition-name. The object-oriented identifier
formats (8.4.3.4 inline method invocation, 8.4.3.5 object-view, 8.4.3.6
EXCEPTION-OBJECT, 8.4.3.7 NULL object reference, 8.4.3.8 SELF and SUPER,
8.4.3.9 object property) are **n/a** by ruling (docs/standards.md,
"Deferred": object orientation). Swept elsewhere: 8.3.3.3 numeric
literals (lexical.md), 8.3.3.4-5 boolean and national literals
(national-boolean.md), 8.4.3.3 reference-modification (refmod.md),
8.4.3.13 program-address-identifier (usage.md).

How: each rule probed with a small program; the dispositions below.
The sweep found eight unenforced rules, every one now refused by name,
and one leniency GnuCOBOL does not share (a paragraph-name shared by
two sections, referenced from outside both, was resolved to the first).
Test 2002/identifiers exercises the behaviours; GnuCOBOL agrees.

## 8.3.3.2 Alphanumeric literals

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | at most 8,191 character positions | **refused**: "this alphanumeric literal has 8192 positions"; 8,191 accepted |
| SR 2 | any character of the source character set | UTF-8 source, the literal's bytes as written (docs/dialect.md: columns count characters) |
| SR 3-4 | two of the opening quotation symbol stand for one | **test**: `"a""b'c"`, `'a''b"c'` |
| SR 5-6 | X"..." of hexadecimal digits, two to a character | **refused**: "bad hexadecimal digit", "needs an even number of digits" |
| GR 1-2 | the delimiters are not part of the value; class and category alphanumeric | **test** |
| GR 3 | no character: a zero-length literal (2014) | **test** under -std=2014: 2014/zerolen (refmod.md "8.5.4 Zero-length items"); **refused** under -std=2002 as 2014's: bad/std2002-zero-literal |
| GR 4-5 | X"" zero-length (2014); each pair one character | **test**: `X""` under -std=2014 as GR 3; `X"414243"` |

## 8.3.3.6 Figurative constant values

| rule | paraphrase | disposition |
|---|---|---|
| SR 1a | where a numeric literal is required, only ZERO, and without ALL | **refused**: bad/std2002-fig-all-zero-arith (`ADD ALL ZERO`) -- accepted before this sweep; `ADD SPACES` was refused already |
| SR 1b | not where a rule prohibits a figurative constant | each statement's own rule (MOVE: move.md) |
| SR 2 | ALL literal-1: an alphanumeric, boolean or national literal, not a figurative constant, not zero-length | **refused**: bad/std2002-fig-all-of-fig (`ALL ALL "a"`) -- was a parse error; `ALL ""` under -std=2014 likewise |
| SR 3 | ALL literal-1 longer than one character not with a numeric or numeric-edited item | **refused** by MOVE (14.9.25.3 rule 5), except an ALL literal of digits to an integer item, which 2023's MOVE keeps as obsolete: BP-O9, a warning (docs/behavior-points.md) |
| SR 4 | ALL symbolic-character from SPECIAL-NAMES | **test**: `ALL DASH` |
| GR 1 | national where national is required; ZERO, SPACE, QUOTE are '0', space, '"' | national-boolean.md; **test** |
| GR 2 | with a fixed-length item, repeated and cut to its length, before JUSTIFIED | **test**: `ALL "ab"` to X(5) gives ababa |
| GR 3 | where no length is given: one character in a concatenation expression or for any form but ALL literal; ALL literal's own length | **test**: DISPLAY SPACE one space, `"a" & QUOTE & "b"`, `ALL "xy"` two characters |
| GR 4-5 | ZERO numeric or character by context; SPACE | **test** |
| GR 6-7 | HIGH-VALUE and LOW-VALUE: the highest and lowest of the collating sequence in effect, compile-time in SPECIAL-NAMES | the program collating sequence's (free/ebcdic, CCVS); a locale's is item 45 |
| GR 8 | QUOTE the quotation mark; never a literal's delimiter | **test**; the lexer takes only `"` and `'` as delimiters |
| GR 9-10 | ALL literal repeated; ALL symbolic-character | **test** |

## 8.4.2.2 Qualification

| rule | paraphrase | disposition |
|---|---|---|
| general 1 | no qualification when the spelling is unique | **test** |
| general 2-3 | unique within a REDEFINES or VARYING clause | **test**: `z REDEFINES y` in two groups |
| general 4 | a name under an unreferenced TYPEDEF needs no qualification | usage.md (TYPEDEF) |
| general 5 | a data-name in a clause of an entry under the same group: implicit qualifiers | OCCURS DEPENDING ON, KEY IS, INDEXED BY resolve within the entry's group (occurs.md) |
| general 6 | a paragraph-name needs no qualification from within its section | **test** (`GO TO p2` inside s2); **refused** from outside every section holding it: bad/std2002-para-ambiguous -- resolved to the first before this sweep; GnuCOBOL refuses it too |
| SR 1 | a non-unique name qualified until unambiguous | **refused**: "'a' is ambiguous; qualify it with OF/IN" |
| SR 2-3 | qualification allowed where not needed, any sufficient set; IN = OF | **test** |
| SR 4 | each qualifier a level the item is under, in order of inclusion; a level-88's hierarchy includes its variable | **refused**: "'c' is not declared under 'g1'" for `c OF g1 OF b`; **test**: `w-odd OF w OF e OF t` |
| SR 5-6 | a condition-name by its variable; an index-name by its table | **test** |
| SR 7 | no duplicate paragraph-name within a section | **refused**: "declared twice in the same section" |
| SR 8 | LINAGE-COUNTER qualified when more than one LINAGE file | **refused**: "LINAGE-COUNTER is ambiguous: say LINAGE-COUNTER OF file-name"; **test**: two LINAGE files, each counter qualified |
| SR 9-10 | LINE-COUNTER, PAGE-COUNTER qualified when more than one RD; in the report section implicitly the report's own | **refused**: "PAGE-COUNTER is ambiguous"; **test**: `PAGE-COUNTER OF r1` |

## 8.4.2.3 Subscripts

| rule | paraphrase | disposition |
|---|---|---|
| SR 2 | a subscript only on an item under an OCCURS | **refused**: "is not a table item and takes no subscript" |
| SR 3 | as many subscripts as OCCURS clauses, at most seven, outermost first | **refused**: "needs 2 subscripts, 1 given"; MAXDIM 7 |
| SR 4 | an index-name of the table's own hierarchy | **refused**: bad/std2002-sub-index-other-table -- accepted before this sweep (the index of a sibling table addressed by its value) |
| SR 5 | the unsubscripted references: SEARCH's subject, REDEFINES, KEY IS, SORT's table and keys, screen FROM/TO/USING, SUM addends | each statement's own parse: search.md, sort.md, screen.md, reportwriter.md |
| SR 6 | ALL only as an intrinsic function's argument or SORT's rightmost | **refused** elsewhere: bad/std2002-sub-all-outside-fn -- was "'all' is not declared"; functions.md (`FUNCTION SUM(v(ALL))`) |
| SR 7 | not ALL on a condition-name | **refused** (the same message) |
| SR 8 | no sum counter, LINE-COUNTER or PAGE-COUNTER as a subscript in the report section | a report field's subscripts are literal or an item's (reportwriter.md); the counters are refused there as SOURCE operands only (8.4.3.15 rule 1) |
| GR 1a | ALL: every occurrence | functions.md |
| GR 1b | an arithmetic expression: its value; not an integer, EC-BOUND-SUBSCRIPT | **test**: `v (i * 2)`, `v (6 / 2)`; a non-integer value with checking on raises EC-BOUND-SUBSCRIPT (exceptions.md); an item that is not an integer item is **refused** at compile time ("must be an integer item"), the 85 rule kept |
| GR 1c | index-name plus or minus an integer: the occurrence number so modified | **test**: `v (ix + 1)`, `v (ix - 1)`; **refused**: two integers (bad/std2002-sub-index-two-adj -- was an error naming DISPLAY) |
| GR 2 | 1 to the OCCURS count, else EC-BOUND-SUBSCRIPT | **refused** for a literal outside the table ("subscript 4 is outside OCCURS 3"); exceptions.md for the run-time check |

## 8.4.3.1 Identifier

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | recursive: wherever an identifier may be written, any format | **test**: `v (function abs(n))`, `function upper-case(x)(2:3)` (fn7 in the sweep; refmod.md) |
| SR 2-4, 9-12 | the formats defined by their sections | this page, refmod.md, usage.md |
| SR 5-8 | the object formats | **n/a** |
| GR 1 | the order of application: the name with its subscripts first, ADDRESS OF to the right, arguments, then the reference modifier | **test**: `ADDRESS OF x`, a function's result reference-modified |

## 8.4.3.2 Function-identifier

Swept with the CALL family (call.md: prototypes, OMITTED, BY VALUE,
14.8.2-3) and functions.md (the intrinsics). Here, the rules of the
identifier itself:

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | not a receiving operand | **refused**: bad/std2002-fn-receiving -- was "'function' is not declared" |
| SR 2 | FUNCTION may be omitted when the REPOSITORY names the function (or ALL INTRINSIC), or for a prototype or pointer; required otherwise | **test**: `REPOSITORY. FUNCTION UPPER-CASE INTRINSIC` then `upper-case(x)` (fn3 in the sweep; 2002/fnproto); **refused** without: bad/std2002-fn-no-function-word -- was "'upper-case' is not declared" |
| SR 3-4 | a prototype of the REPOSITORY or the containing definition; a function-pointer item | call.md; a function-pointer item: usage.md "function-pointer-name (arguments)" (2026-10-07, queue item 26; 2014/fnpointer) |
| SR 5 | a function-pointer needs the parentheses | **refused**: bad/std2014-fnptr-no-parens |
| SR 6 | a left parenthesis after the name always opens the arguments | **test**: `FUNCTION RANDOM (A) B` takes A; `FUNCTION RANDOM ()`, `(FUNCTION RANDOM) (A)` (fn7 in the sweep) |
| SR 7 | OMITTED not with an intrinsic | **refused**: bad/std2002-fn-omitted-intrinsic -- was "'omitted' is not declared" |
| SR 8 | an argument an identifier, literal, boolean or arithmetic expression; the counts and classes by clause 15 or 14.8.2 | functions.md, call.md |
| SR 9-10 | OMITTED only for an OPTIONAL parameter; BY VALUE arguments numeric, object or pointer | **refused**: bad/std2002-fn-omitted-notopt; call.md |
| SR 11 | a numeric function not where an integer operand is required | **refused** as an integer argument of a function ("FUNCTION SQRT is a numeric function, not an integer function", functions.md); as a subscript it is an arithmetic expression, GR 1b of 8.4.2.3 deciding at run time |
| SR 12 | an integer function other than integer ABS not where an unsigned integer is required | the unsigned-integer contexts (OCCURS counts, PICTURE repeats, a literal subscript) take literals only |
| SR 13-14 | conformance of a prototype's arguments; a bit argument BY REFERENCE byte-aligned and literally located | call.md; national-boolean.md |
| GR 1 | a temporary item, described by the intrinsic's definition or the prototype's RETURNING | functions.md, call.md |
| GR 2 | arguments evaluated left to right, each may be a function | **test**: `FUNCTION MAX(FUNCTION RANDOM(1) 2)` |
| GR 3-5 | the function located by its prototype or pointer; BY REFERENCE, CONTENT, VALUE assumed by the parameter and the argument's kind | call.md |
| GR 6 | evaluation: arguments first; EC-FUNCTION-NOT-FOUND, EC-PROGRAM-RESOURCES, EC-FUNCTION-PTR-NULL; the declarative or WHEN | exceptions.md; EC-FUNCTION-PTR-NULL is item 26 |
| GR 7-8 | OMITTED or a trailing argument left off: the omitted-argument condition true; such a parameter referenced otherwise, EC-FUNCTION-ARG-OMITTED | call.md (2002/fnproto: `IS OMITTED`); the exception is raised with checking on (exceptions.md) |

## 8.4.3.4, 8.4.3.5, 8.4.3.6, 8.4.3.7, 8.4.3.8, 8.4.3.9: the object-oriented identifier formats

| rule | paraphrase | disposition |
|---|---|---|
| 8.4.3.4 | inline method invocation `identifier::"method"(...)` | **n/a**: object orientation is out of scope by ruling (docs/standards.md, "Deferred"); the `::` operator is refused where it is met |
| 8.4.3.5 | object-view `AS class-name` | **n/a** |
| 8.4.3.6 | EXCEPTION-OBJECT | **n/a**: exceptions.md (RAISE of an exception object refused by name) |
| 8.4.3.7 | the NULL object reference | **n/a**; NULL the address is 8.4.3.10 below |
| 8.4.3.8 | SELF and SUPER | **n/a** |
| 8.4.3.9 | object property `property-name OF identifier` | **n/a** |

## 8.4.3.10 NULL

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | only with an item of class pointer or message-tag | **refused** with an alphanumeric receiver: bad/std2002-move-null-alnum -- accepted before this sweep; a pointer takes NULL by SET (set.md), MOVE keeps pointers out (14.9.25.3 rule 1) |
| GR 1-3 | the null data, function and program address: a value no item, function or program has | **test**: `SET p TO NULL`, `p = NULL`; usage.md (program-pointers; function-pointers, 2014/fnpointer) |
| GR 4 | the null message-tag | **n/a**: message tags are the Communication module's successor, out of scope (docs/standards.md) |

## 8.4.3.11 Data-address-identifier (ADDRESS OF)

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | an item of the FILE, WORKING-STORAGE, LOCAL-STORAGE or LINKAGE SECTION | **test**: 2002/pointerset, 2002/basedlocal |
| SR 2 | not an object reference nor an elementary item of a strongly-typed group | **refused**: "an item inside a strongly-typed group (2023 8.4.3.11 rule 2)" (bad/std2002-address-strong-elem); objects **n/a** |
| SR 3 | not a CONSTANT RECORD item | **ruling** (2026-10-07, queue item 25): taken; the storage is read-only and a store through the pointer faults (data-division.md "13.18.15 CONSTANT RECORD") |
| SR 4 | a bit item byte-aligned, its subscripts and start literal | **refused**: "a bit item not on a byte, or located at run time" (national-boolean.md) |
| SR 5 | not a receiving operand | **refused**: `SET ADDRESS OF x TO p` for an item that is not BASED or a LINKAGE record (14.9.39.3 rule 18, set.md); `MOVE ADDRESS OF` refused |
| SR 6 | not a dynamic-length item or a dynamic-capacity table's element | both 2014's (queue items 25, 24) |
| GR 1-2 | a data-pointer with the item's address; restricted to the type of a strongly-typed group | **test**: `p = ADDRESS OF x`; usage.md (restricted pointers) |

## 8.4.3.12 Function-address-identifier (ADDRESS OF FUNCTION)

Implemented 2026-10-07 (queue item 26), under -std=2014: the rows
"ADDRESS OF FUNCTION" and "SET format 8" in usage.md. Test 2014/fnpointer.

| rule | paraphrase | disposition |
|---|---|---|
| SR 1-2 | identifier-1 alphanumeric or national; the prototype one of the REPOSITORY | **refused**: bad/std2014-fnptr-address-item; a word that is neither is "not declared" |
| SR 3 | not a receiving operand | **refused**: bad/std2014-fnptr-receiver |
| GR 1-4 | the function's address, by the item's content (8.3.2.2: the externalized name) or the prototype's; restricted to the prototype; not found, NULL and EC-FUNCTION-NOT-FOUND | **test**: 2014/fnpointer (usage.md) |

## 8.4.3.14 LINAGE-COUNTER

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | only in PROCEDURE DIVISION statements | **refused** in a VALUE clause ("expected a literal after VALUE") |
| SR 2 | not a receiving operand | **refused**: bad/std2002-linage-counter-receiver (MOVE, arithmetic, SET, ACCEPT) -- accepted before this sweep |
| SR 3 | qualified as 8.4.2.2 rule 8 | above |
| GR 1 | an unsigned integer as large as the page | **test**: a four-byte cell of the file; `MOVE LINAGE-COUNTER OF pa TO ctr` |
| GR 2 | its values: 13.18.34 rule 7 | files.md (LINAGE): 1 after OPEN, the lines advanced added at each WRITE (the test shows 4 after three writes) |

## 8.4.3.15 Report counters

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | in the REPORT SECTION only in SOURCE; in the PROCEDURE DIVISION wherever an integer item may be | **test**: `SOURCE PAGE-COUNTER`, `SOURCE LINE-COUNTER`, `MOVE 7 TO PAGE-COUNTER`, `ADD 1 TO PAGE-COUNTER OF r1`, `MOVE LINE-COUNTER TO ctr`; **refused** as a SUM addend ("SUM belongs in a CONTROL FOOTING group") and a VALUE |
| SR 2 | qualification: 8.4.2.2 rules 9-10 | above |
| SR 3 | LINE-COUNTER not a receiving operand | **refused**: bad/std2002-line-counter-receiver -- accepted before this sweep |
| GR 1 | unsigned integers kept per report | **test**: four-byte cells of the report block (DISPLAY shows them at ten digits, the cell's PICTURE) |
| GR 2-3 | PAGE-COUNTER 1 at INITIATE, +1 at each page advance, reset by NEXT GROUP ... RESET; LINE-COUNTER 0 at INITIATE, 0 at each page advance | reportwriter.md (CCVS RW programs; the test's report shows 01/07/08 after the program's own MOVE and ADD) |
| GR 4-5 | LINE-COUNTER the line printed; unchanged by a dummy group or SUPPRESS | reportwriter.md |

## 8.4.4 Condition-name

| rule | paraphrase | disposition |
|---|---|---|
| general | level 88 under a conditional variable: a subset of its values; SET makes it true; a SPECIAL-NAMES switch's on or off status | **test**: `w-odd`; set.md (SET TO TRUE / FALSE); CCVS SW-1 (switch conditions, 8.8.4.6) |
| SR 1 | format 1 names a switch of SPECIAL-NAMES | the switch names of docs/conformance/clauses.md (SPECIAL-NAMES) |
| SR 2 | format 2 qualified and subscripted as 8.4.2.2-3 | **test**: `w-odd OF w OF e OF t (ix + 1)`, `w-odd (ix)` |
