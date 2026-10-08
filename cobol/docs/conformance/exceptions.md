# Exception conditions: 14.6.13 and Table 13

Swept 2026-10-06 (docs/plans/standard-queue.md item 4). ISO/IEC 1989:2023
14.6.13.1.6, Table 13: every level-3 exception-name, its category
(Fatal, NF nonfatal, Imp implementor-defined), where this compiler and
runtime raise it, the test that shows it, or why it does not arise. The
condition each name stands for is the table's own text, cited by
name and not carried here. The
level-1 and level-2 names (EC-ALL, EC-SIZE, ...) are groups and are
covered by the `>>TURN`, `USE` and `RAISE` pages. The mechanism itself
-- checking turned on by >>TURN or an exception-checking PERFORM, the
declarative chosen, fatal conditions ending the run -- is in
docs/conformance/turn.md, use.md, raise.md and perform.md.

Raised since this sweep: EC-OVERFLOW-STRING and -UNSTRING,
EC-RANGE-SEARCH-NO-MATCH, EC-FLOW-RELEASE and -RETURN, EC-SORT-MERGE-
ACTIVE, -FILE-OPEN, -RETURN and -SEQUENCE, EC-REPORT-ACTIVE, -INACTIVE,
-FILE-MODE and -NOT-TERMINATED, EC-FLOW-REPORT. Each is asked of the
runtime only when its checking is on (`emit_ec_query`), so a program
that turns nothing on runs as before; a fatal one found with checking
off still stops the run with the runtime's own message, as it did.

Counting: 133 level-3 names. 37 raised and shown by a test; 7 refused at
compile time instead, where the text's run-time condition is decided by
the source; 22 the implementor's (`-IMP`), which this implementation
never defines; 43 belong to features not built (object orientation,
VALIDATE, messaging, locale, commit, prototypes and
pointers, IEEE float, the screen and 2002 Report Writer leftovers), each
with its queue item; 4 rulings; 20 gaps, named in their rows, which
queue item 4 continues with.

## EC-ARGUMENT

| name | cat | disposition |
|---|---|---|
| EC-ARGUMENT-FUNCTION | Fatal | **test**: 2002/fnargbad, 2002/fnreturn, ecsites/argfn |
| EC-ARGUMENT-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |

## EC-BOUND

| name | cat | disposition |
|---|---|---|
| EC-BOUND-FUNC-RET-VALUE | NF | **ruling**: cannot arise -- a function's temporary is sized to its result (docs/functions.md) |
| EC-BOUND-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-BOUND-ODO | Fatal | **test**: 2002/ecodo |
| EC-BOUND-OVERFLOW | NF | a dynamic-capacity table's expected capacity first exceeded by a store (occurs.md; 2014/dyntable) |
| EC-BOUND-PTR | Fatal | **gap**: queue item 11 |
| EC-BOUND-REF-MOD | Fatal | **test**: 2002/ecrefmod, 2002/fnrmpast, 2002/fnrmzero |
| EC-BOUND-SET | NF | SET of a dynamic-capacity table's capacity past its expected capacity (set.md; 2014/dyntable) |
| EC-BOUND-SUBSCRIPT | Fatal | **test**: 2002/ecbound, 2002/ecperform, 2002/ecpfatal |
| EC-BOUND-TABLE-LIMIT | Fatal | a dynamic-capacity table past the implementor's maximum, 16,777,215 occurrences, or past the storage there is; raised when checked, fatal in the runtime otherwise; no test drives it |

## EC-CONTINUE

| name | cat | disposition |
|---|---|---|
| EC-CONTINUE-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-CONTINUE-LESS-THAN-ZERO | NF | **raised** by CONTINUE AFTER a negative value, under -std=2023 (queue item 31, 2026-10-07): 2023/stmts2023 |

## EC-DATA

| name | cat | disposition |
|---|---|---|
| EC-DATA-CONVERSION | NF | **test**: 2002/nataccept, 2002/natconv, 2002/natfuncs |
| EC-DATA-INCOMPATIBLE | Fatal | **test**: 2002/ecincompat, 2002/ecincompat2, 2002/userfnrecvec |
| EC-DATA-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-DATA-NOT-FINITE | Fatal | **gap**: the IEEE usages (queue items 15, 20-21) |
| EC-DATA-OVERFLOW | Fatal | **gap**: as EC-DATA-NOT-FINITE |
| EC-DATA-PTR-NULL | Fatal | **test**: 2002/ecptrnull |

## EC-EXTERNAL

| name | cat | disposition |
|---|---|---|
| EC-EXTERNAL-DATA-MISMATCH | Fatal | **test**: 2023/extdatamis (the sub's FILE STATUS its own item, not the shared one); the FILE STATUS, RELATIVE KEY and LINAGE items of an external file the same storage in every program (14.8.4.2; implemented 2026-10-07, queue item 35; data-division.md EXTERNAL) |
| EC-EXTERNAL-FILE-MISMATCH | Fatal | **test**: 2023/extfilemis (another access mode); the SELECT entries of an external file alike (12.4.5.3 rule 1) |
| EC-EXTERNAL-FORMAT-CONFLICT | Fatal | **test**: 2023/extformat (a longer record; a declarative takes the condition, then the run unit ends); the descriptions of an external record alike (13.18.22.4 rule 6) |
| EC-EXTERNAL-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |

## EC-FLOW

| name | cat | disposition |
|---|---|---|
| EC-FLOW-APPLY-COMMIT | Fatal | **n/a**: commit and rollback (queue item 46) |
| EC-FLOW-COMMIT | Fatal | **n/a**: queue item 46 |
| EC-FLOW-GLOBAL-EXIT | Fatal | **refused** at compile time: EXIT PROGRAM in a declarative whose USE is GLOBAL (14.9.14.3 rule 2); the run-time case -- the declarative invoked for a contained program -- cannot arise, as the statement is refused in it |
| EC-FLOW-GLOBAL-GOBACK | Fatal | **refused** at compile time: GOBACK in a GLOBAL declarative (14.9.18.3 rule 1), likewise |
| EC-FLOW-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-FLOW-RELEASE | Fatal | **test**: 2002/ecsort |
| EC-FLOW-REPORT | Fatal | **test**: 2002/ecreport |
| EC-FLOW-RETURN | Fatal | **test**: 2002/ecsort |
| EC-FLOW-ROLLBACK | Fatal | **n/a**: queue item 46 |
| EC-FLOW-SEARCH | Fatal | SET of a capacity during a SEARCH of its table: **refused** at compile time when written inside the SEARCH (bad/std2014-dyn-set-in-search); not kept at run time (set.md) |
| EC-FLOW-USE | Fatal | **test**: 2002/ecflowuse |

## EC-FUNCTION

| name | cat | disposition |
|---|---|---|
| EC-FUNCTION-ARG-OMITTED | Fatal | **n/a**: OMITTED arguments of a user-defined function (queue item 8) |
| EC-FUNCTION-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-FUNCTION-NOT-FOUND | Fatal | **n/a**: function prototypes and pointers (queue items 8, 26) |
| EC-FUNCTION-PTR-INVALID | Fatal | **n/a**: function pointers (queue item 26) |
| EC-FUNCTION-PTR-NULL | Fatal | **n/a**: function pointers (queue item 26) |

## EC-I-O

| name | cat | disposition |
|---|---|---|
| EC-I-O-AT-END | NF | **test**: 2002/ecio, 2002/ecpreview, 2002/ecturnfile |
| EC-I-O-EOP | NF | **gap**: the end-of-page condition is the WRITE's EOP phrase only (queue item 4, next) |
| EC-I-O-EOP-OVERFLOW | NF | **gap**: as EC-I-O-EOP |
| EC-I-O-FILE-SHARING | NF | raised from I-O status class 6: **61** at an OPEN Table 19 refuses beside another connector of the run unit, **62** at DELETE FILE of a file open through another (locking.md; 2002/locking, 2023/lockdel) |
| EC-I-O-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-I-O-INVALID-KEY | NF | **test**: 2002/usefile |
| EC-I-O-LINAGE | Fatal | **gap**: a LINAGE value out of range is not detected (queue item 4, next) |
| EC-I-O-LOGIC-ERROR | Fatal | raised from I-O status class 4; **gap**: no test drives that class (2002/ecio drives 1, 3 and the warning) |
| EC-I-O-PERMANENT-ERROR | Fatal | **test**: 2002/ecio |
| EC-I-O-RECORD-CONTENT | Fatal | raised from I-O status class 7; **gap**: no test drives that class |
| EC-I-O-RECORD-OPERATION | NF | raised from I-O status class 5: **51** a record locked by another connector of the run unit, **53**/**54** the lock limits (locking.md; 2002/locking) |
| EC-I-O-WARNING | NF | **test**: 2002/ecio, 2002/ecturn, 2002/seqbyteec |

## EC-LOCALE

| name | cat | disposition |
|---|---|---|
| EC-LOCALE-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-LOCALE-INCOMPATIBLE | Fatal | **n/a**: locale support (queue item 45) |
| EC-LOCALE-INVALID | Fatal | **n/a**: locale support (queue item 45) |
| EC-LOCALE-INVALID-PTR | Fatal | **n/a**: locale support (queue item 45) |
| EC-LOCALE-MISSING | Fatal | **n/a**: locale support (queue item 45) |
| EC-LOCALE-SIZE | Fatal | **n/a**: locale support (queue item 45) |

## EC-MCS

| name | cat | disposition |
|---|---|---|
| EC-MCS-ABNORMAL-TERMINATION | NF | **n/a**: asynchronous messaging (queue item 48) |
| EC-MCS-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-MCS-INVALID-TAG | NF | **n/a**: asynchronous messaging (queue item 48) |
| EC-MCS-MESSAGE-LENGTH | NF | **n/a**: asynchronous messaging (queue item 48) |
| EC-MCS-NO-REQUESTER | NF | **n/a**: asynchronous messaging (queue item 48) |
| EC-MCS-NO-SERVER | NF | **n/a**: asynchronous messaging (queue item 48) |
| EC-MCS-NORMAL-TERMINATION | NF | **n/a**: asynchronous messaging (queue item 48) |
| EC-MCS-REQUESTOR-FAILED | NF | **n/a**: asynchronous messaging (queue item 48) |

## EC-OO

| name | cat | disposition |
|---|---|---|
| EC-OO-ARG-OMITTED | Fatal | **n/a**: object orientation (queue item 51) |
| EC-OO-CONFORMANCE | Fatal | **n/a**: object orientation (queue item 51) |
| EC-OO-EXCEPTION | Fatal | **n/a**: object orientation (queue item 51) |
| EC-OO-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-OO-METHOD | Fatal | **n/a**: object orientation (queue item 51) |
| EC-OO-NULL | Fatal | **n/a**: object orientation (queue item 51) |
| EC-OO-RESOURCE | Fatal | **n/a**: object orientation (queue item 51) |
| EC-OO-UNIVERSAL | Fatal | **n/a**: object orientation (queue item 51) |

## EC-ORDER

| name | cat | disposition |
|---|---|---|
| EC-ORDER-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-ORDER-NOT-SUPPORTED | Fatal | **n/a**: locale support and STANDARD-COMPARE (queue item 45) |

## EC-OVERFLOW

| name | cat | disposition |
|---|---|---|
| EC-OVERFLOW-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-OVERFLOW-STRING | NF | **test**: 2002/ecovfstr |
| EC-OVERFLOW-UNSTRING | NF | **test**: 2002/ecovfstr |

## EC-PROGRAM

| name | cat | disposition |
|---|---|---|
| EC-PROGRAM-ARG-MISMATCH | Fatal | **n/a**: program prototypes (queue item 8); without one the arguments are not checked |
| EC-PROGRAM-ARG-OMITTED | Fatal | **test**: 2002/ecargomit |
| EC-PROGRAM-CANCEL-ACTIVE | Fatal | **test**: 2002/cancelactive |
| EC-PROGRAM-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-PROGRAM-NOT-FOUND | Fatal | **test**: 2002/ecpgm; ADDRESS OF PROGRAM of a program not here, checked, run by hand (2002/pgpointer) |
| EC-PROGRAM-PTR-NULL | Fatal | **test**: 2002/pgpointer's NULL CALL, checked, run by hand (docs/conformance/usage.md) |
| EC-PROGRAM-RECURSIVE-CALL | Fatal | **test**: 2002/ecrecur, 2002/recnot |
| EC-PROGRAM-RESOURCES | Fatal | **ruling**: cannot arise -- every program is linked into the one executable |

## EC-RAISING

| name | cat | disposition |
|---|---|---|
| EC-RAISING-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-RAISING-NOT-SPECIFIED | Fatal | **test**: 2002/ecraising (case nspec, run by hand): GOBACK RAISING LAST EXCEPTION of an EC-USER name the header's RAISING does not list |

## EC-RANGE

| name | cat | disposition |
|---|---|---|
| EC-RANGE-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-RANGE-INDEX | Fatal | **ruling**: no range is enforced on an index value; an element outside the table is EC-BOUND-SUBSCRIPT when used |
| EC-RANGE-INSPECT-SIZE | Fatal | **refused** at compile time where the sizes are known (INSPECT REPLACING operands of unequal size); a run-time size (a reference-modified operand of computed length) is a **gap** |
| EC-RANGE-INVALID | NF | **gap**: EVALUATE ... THROUGH with its ends reversed is an empty range, no condition (queue item 4, next) |
| EC-RANGE-PERFORM-VARYING | Fatal | **test**: 2002/perfvary |
| EC-RANGE-PTR | Fatal | **gap**: pointer SET UP/DOWN is unchecked (with EC-BOUND-PTR, queue item 11) |
| EC-RANGE-SEARCH-INDEX | NF | **test**: 2002/ecsearchidx |
| EC-RANGE-SEARCH-NO-MATCH | NF | **test**: 2002/ecovfstr |

## EC-REPORT

| name | cat | disposition |
|---|---|---|
| EC-REPORT-ACTIVE | Fatal | **test**: 2002/ecreport |
| EC-REPORT-COLUMN-OVERLAP | NF | **refused** at compile time: overlapping items in a report line (docs/conformance/reportwriter.md) |
| EC-REPORT-FILE-MODE | Fatal | **test**: 2002/ecreport |
| EC-REPORT-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-REPORT-INACTIVE | Fatal | **test**: 2002/ecreport |
| EC-REPORT-LINE-OVERLAP | NF | **refused** at compile time: overlapping lines in a report group |
| EC-REPORT-NOT-TERMINATED | NF | **test**: 2002/ecreport |
| EC-REPORT-PAGE-LIMIT | NF | **refused** at compile time for absolute lines; a group taller than its region at run time is a **gap** |
| EC-REPORT-PAGE-WIDTH | NF | **refused** at compile time: an item past the page width |
| EC-REPORT-SUM-SIZE | Fatal | **gap**: a sum counter overflow is truncated silently (queue item 37, with the 2002 Report Writer) |
| EC-REPORT-VARYING | Fatal | **n/a**: VARYING in a report group is the 2002 Report Writer (queue item 37) |

## EC-SCREEN

| name | cat | disposition |
|---|---|---|
| EC-SCREEN-FIELD-OVERLAP | NF | **gap** (docs/conformance/screen.md, queue item 38) |
| EC-SCREEN-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-SCREEN-ITEM-TRUNCATED | NF | **gap** (screen.md) |
| EC-SCREEN-LINE-NUMBER | NF | **gap** (screen.md) |
| EC-SCREEN-STARTING-COLUMN | NF | **gap** (screen.md) |

## EC-SIZE

| name | cat | disposition |
|---|---|---|
| EC-SIZE-ADDRESS | Fatal | **gap**: pointer arithmetic is unchecked (queue item 11) |
| EC-SIZE-EXPONENTIATION | Fatal | **test**: 2002/ecsizeexp |
| EC-SIZE-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-SIZE-OVERFLOW | Fatal | **test**: 2002/ecovfl, 2002/ecsizeexp, 2002/ecraise |
| EC-SIZE-TRUNCATION | Fatal | **test**: 2002/ecsize |
| EC-SIZE-UNDERFLOW | Fatal | **gap**: floating-point underflow (queue items 15, 20-21) |
| EC-SIZE-ZERO-DIVIDE | Fatal | **test**: 2002/eczdiv |

## EC-SORT-MERGE

| name | cat | disposition |
|---|---|---|
| EC-SORT-MERGE-ACTIVE | Fatal | raised before a SORT or MERGE while one is under way; **gap**: no test drives it (a nested SORT needs an INPUT PROCEDURE that sorts) |
| EC-SORT-MERGE-FILE-OPEN | Fatal | **test**: 2002/ecsort |
| EC-SORT-MERGE-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-SORT-MERGE-RELEASE | Fatal | **ruling**: cannot arise -- the released record is the SD's own record area, always its full size |
| EC-SORT-MERGE-RETURN | Fatal | **test**: 2002/ecsort |
| EC-SORT-MERGE-SEQUENCE | Fatal | **test**: 2002/ecsort |

## EC-STORAGE

| name | cat | disposition |
|---|---|---|
| EC-STORAGE-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-STORAGE-NOT-ALLOC | NF | **test**: 2002/allocfree |
| EC-STORAGE-NOT-AVAIL | NF | raised when ALLOCATE gets no storage (no test can ask for more than the heap gives deterministically); and by SET SIZE OF a dynamic-length item to a negative size or one past its LIMIT (2014/dynlen2, through a declarative) |

## EC-VALIDATE

| name | cat | disposition |
|---|---|---|
| EC-VALIDATE-CONTENT | NF | **n/a**: VALIDATE, not built by ruling |
| EC-VALIDATE-FORMAT | NF | **n/a**: VALIDATE, not built by ruling |
| EC-VALIDATE-IMP | Imp | **ruling**: no implementor-defined condition is defined here, so it never arises |
| EC-VALIDATE-RELATION | NF | **n/a**: VALIDATE, not built by ruling |
| EC-VALIDATE-VARYING | Fatal | **n/a**: VALIDATE, not built by ruling |
