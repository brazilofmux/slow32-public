# The census: which items stand alone

The end of the performance plan (`performance.md`, stage 4) is stage08's
HIR: its allocator and its passes in place of the emitter's habits.  A
back end of that kind is only as good as what it is told, and what
COBOL has to tell it is in the DATA DIVISION.  A record laid out byte
by byte, redefined, moved as a whole, handed to another program, is
memory and stays memory.  But an item that is reached only by its own
name has no layout anyone can observe: the DATA DIVISION says where it
lies and how it is encoded, and nothing in the program can tell
whether that was obeyed.  Such an item's representation is the
compiler's to choose -- a word, in a register -- and a 77 level is only
the plainest hint of it.

The census counts them.  It is the first step of that road and changes
no code: every program compiles to the same assembly with it and
without it (`tests/asm-snapshot.sh` over 1,506 programs, the same to
the byte).

## What standing alone means

An elementary item stands alone when the statements that name it are
the only way its bytes are reached:

- no group over it is named by a statement (a MOVE of the group, a
  WRITE FROM it, a comparison of it, a CALL passing it);
- no redefinition or renaming over its bytes is named.  A REDEFINES
  that is written and never used does not count: `B REDEFINES A` with
  only B named leaves B alone;
- its storage is the program's own: not a file's record, not LINKAGE,
  EXTERNAL or BASED, not an ANY LENGTH item, not a GLOBAL item that a
  contained program names;
- its address is given to nobody and kept by nobody: not a CALL
  argument BY REFERENCE or a RETURNING item (BY CONTENT and BY VALUE
  give a copy), not the operand of ADDRESS OF, not a host variable or
  a field of the SQLCA, not a FILE STATUS, RELATIVE KEY, ASSIGN,
  record DEPENDING ON, LINAGE or CRT STATUS item, not a report's
  CONTROL, not a screen item's FROM, TO or USING.

Two more things are noted without deciding anything: a numeric item
that is reference-modified or class-tested has its bytes looked at, so
its encoding is pinned though its place is not; and the object of an
OCCURS DEPENDING ON is read by the code of every reference to its
table, which is a use like any other.

A group named only by INITIALIZE or SEARCH is counted apart ("group,
item by item").  INITIALIZE is defined as a MOVE to each elementary
item, and SEARCH names the table and looks at what its WHEN phrases
name; neither looks at the group's bytes as a whole.

## How it is taken

`S32_CENSUS_DIR=dir` in the environment of any compile.  The compiler
writes `dir/<source>.<hash>.census`: one line per elementary item of
each program unit.

- `sym_lookup` (symtab.h) counts every name a statement resolves, with
  the statement's verb; a statement parsed twice is counted once (by
  token).
- `parse_ref` (operand.h) is where an identifier is parsed, and adds
  what only it knows: reference modification, subscripts.  A name
  resolved for a statement by any other path, and not one of the few
  that are known (SEARCH's table, a table SORT's, CRT STATUS, a host
  variable), is marked `other` and the item is refused: what the census
  has no rule for it does not guess at.  No corpus has one.
- CALL's arguments, ADDRESS OF, a class condition, a user function's
  arguments say what they are where they are compiled.
- At the unit's end (`census_unit`, census.h) the clauses of the other
  divisions are added, a condition-name's uses are given to its item,
  and every elementary item is tested against every named item of the
  same record: is that one a group over this one, or do its bytes --
  every occurrence, for an item in a table -- meet this one's.

The line holds the facts, not the verdict: what the item is, how it was
named, by which statements, and by which statements the groups over it
and its aliases were named.  `tests/census.py` draws the verdict, so
the reasons can be weighed differently without building the compiler
again.  `tests/census.sh` runs every corpus; the harness's gate 1g
holds two programs written to carry every shape that decides it.

## What it found (2026-10-02)

1,434 programs, 107,274 elementary items.  Items not in a table first;
"refs" is how many times statements name the items, which is what the
generated code is made of.  Shares are of the items that are named at
all -- 65,181 of the 100,450 are not: FILLER with a VALUE, the fields of
print lines and copybook records the program never touches by name.

| corpus | programs | named items | alone | refs | to items alone |
|---|---:|---:|---:|---:|---:|
| majesty | 57 | 1,186 | 32.8% | 3,861 | 55.3% |
| Open Systems | 227 | 9,393 | 34.3% | 35,423 | 54.6% |
| CCVS-85 | 365 | 14,902 | 43.4% | 92,448 | 46.2% |
| X-COBOL | 434 | 7,810 | 33.5% + 7.6% | 28,786 | 43.7% + 9.6% |
| the harness | 351 | 1,978 | 68.6% | 9,489 | 77.5% |
| all | 1,434 | 35,269 | 39.8% + 1.7% | 170,007 | 49.5% + 1.6% |

(The second figure is "group, item by item"; INITIALIZE of a group is
an X-COBOL habit and nearly absent elsewhere.)

Why the others do not, by references, all corpora:

| | items | refs |
|---|---:|---:|
| a group over it is named | 30.6% | 25.6% |
| storage not the program's (file records 6,126 items, LINKAGE 1,488) | 20.7% | 17.1% |
| address given or kept (CALL 656, runtime 497, SQL 347) | 4.1% | 4.2% |
| a redefinition or renaming is named | 3.1% | 2.0% |

- **Half of what the code names stands alone**, in the working corpora
  as in the test suites.  In majesty it is 55% of the references, and
  in `csv2fw` -- 60% of the batch's time -- 52 of the 65 named items
  and 500 of the 528 references.
- **REDEFINES is the small reason.**  The large one is the group: a
  record built field by field in WORKING-STORAGE and then moved,
  written or displayed whole.  The statements that name the groups are
  MOVE (10,546 items), IF (1,876, nearly all CCVS-85), WRITE (1,115),
  DISPLAY (744), CALL (516).  Those items have a layout that is
  observed, and they stay memory.
- **What stands alone is mostly numbers.**  Of the references to items
  alone: unsigned DISPLAY integers of nine digits or fewer 33%,
  alphanumeric items 17% and single characters 9%, DISPLAY with decimal
  places 10%, binary integers of a word or less 8%, packed integers 7%
  (Open Systems: 30%) and packed with decimal places 4%, signed or
  long DISPLAY integers 7%.  In majesty: DISPLAY integers 23%, binary
  21%, packed decimal 20%, single characters 17%.
- **Their bytes are almost never looked at.**  Of some 9,800 numeric
  items standing alone, 42 are reference-modified or class-tested.
- **Tables stand alone less, but the busy ones do.**  Items under an
  OCCURS: 19.9% alone, 31.2% of the references; in majesty 39% of the
  items and 80% of the references (csv2fw's tables).  A table whose
  rows are never moved whole could be an array of words.
- **Index-names** (861, 4,773 references) are the compiler's own cells
  already and are counted apart.

An item with no VALUE is not a question: this compiler starts an
unvalued numeric item at zero and an alphanumeric one at spaces
already, so a chosen layout starts where the written one did.

## What it does not say

- The counts are of the source, not of the run: a reference in a loop
  counts once.  The profile says where the time is (`docs/performance.md`);
  the census says what could be done there.
- A named group refuses every item under it, whatever the statement
  did.  `MOVE SPACES TO group` writes the items and looks at none of
  them, and could be given to the items one by one as INITIALIZE is;
  the census does not yet tell a group sent from a group received, so
  the "group" share is an upper bound on what a finer reading would
  leave.
- It is taken a compilation unit at a time, and that is enough: what
  another unit can reach, it reaches through an address that was given
  to it, and those are counted.
- LOCAL-STORAGE items are sorted like WORKING-STORAGE ones.  Standing
  alone, they are the easiest case of all: an activation's own value.

## Step 2: numbers written the machine's way (2026-10-02)

A number that stands alone, and whose every use is a use of its
number, is no longer stored as its entry says.  A DISPLAY or COMP-3
item of eighteen digits or fewer, with decimal places or without,
signed or not, becomes a binary one of the size a COMP item of its
picture has -- two bytes to four digits, four to nine, eight to
eighteen -- in the first bytes of the place it had; a COMP item keeps
its bytes and loses its byte order.  Its picture still says how many
digits it holds and where its point is: a store truncates and rounds
to them and a size error is raised by them, as before.  The record
keeps its length and every other item its place.  `-fno-native-items`
(or `S32_NATIVE_ITEMS=0` in the environment) leaves every item as
written.

An item stays in its place when its own bytes are enough.  A DISPLAY
item's nearly always are; a packed item of five digits has three bytes
where four are wanted, and one of ten to thirteen digits has six or
seven where eight are.  Those leave their records: the unit gets one
record of the compiler's making, each such item a cell in it at the
alignment of its size, and the item's old place is a hole nothing
looks at -- nothing can, since no group over it is named.  That is
the transformation in full: the DATA DIVISION as written, and another
that behaves the same.

### What must be true of the item

Standing alone is not enough.  `MOVE SPACES TO WS-N` is written, and a
blank field moved to an item of the same picture arrives blank and is
printed blank (the bytes are copied as they are: GitHub #27).  A
program can see how an item is written through any statement that
takes its bytes.  So:

- **Every use is of its number.**  The item's address is formed in one
  place in the compiler (`emit_item_addr`).  What follows decides: a
  load or a store of its value in line (`emit_load_int`,
  `emit_store_int`, the in-line decimal add), or a call of a runtime
  routine read for this and known to take the item by its own
  descriptor as a number -- `cob_push`, `cob_load_int`, `cob_get_num`,
  the stores, `cob_display_field`, and `cob_move` where a number comes
  out or goes in.  Anything else -- another routine, bytes copied or
  compared in line, the statement ending with the address unused --
  **pins** the item to the form it was written in.  What is not known
  to be a use of the number is taken for a use of the bytes; a use
  nobody thought of costs a speed-up, not a wrong answer.
- **Every name owes an address.**  `FUNCTION LENGTH (X)` forms no
  address and depends on how X is written.  A statement that names an
  item more often than it forms its address pins it; and asking for an
  item's length in bytes pins it wherever that is asked.
- **Where the rules want DISPLAY, it stays DISPLAY.**  STRING's and
  INSPECT's operands, UNSTRING's receivers, an item compared with a
  nonnumeric operand or filled by a figurative constant or ALL literal.
- **Items copied or compared byte for byte are written one way.**  A
  MOVE between two items of one description copies the bytes, and a
  relation between them compares the bytes; both take the machine's
  form or neither does, so one that must stay as written keeps its
  partners with it.
- **It starts as a number.**  Its own VALUE is a numeric literal or
  ZERO, or it has none; no group over it has a VALUE; it redefines
  nothing (an item laid over an alphanumeric one starts as spaces);
  its condition-names' values are numbers.
- Its sign, if it has one, is on its last digit (no SIGN LEADING or
  SEPARATE), and its picture has no P.
- An element of a table is taken where it is, every occurrence in its
  own bytes (the table's stride is its entries', not the item's size),
  when the table's length does not vary and nothing names the table as
  a whole (SEARCH, SORT) or a row of it.
- Not LOCAL-STORAGE, not GLOBAL: not yet.

### How the compiler comes to know before it writes

The verdict needs the whole PROCEDURE DIVISION and the first
statement's code needs the verdict, and the emitter writes code as it
reads.  So the program is read twice: once by a child of the compiler
(`fork`), which compiles it as written with the census on and sends
back the items that may change, and once by the compiler itself, with
those items changed before any statement is compiled
(`src/cobc/native.h`).  The child is the whole compiler with nothing
to undo.  When a unit is a tree before it is code, the tree will be
asked instead and this goes.  A program with errors is compiled as
written, so its messages are about what was written.

### What it bought

7,609 items in the corpora are written the machine's way: 19.8% of the
named items, 25.6% of the references -- 4,468 DISPLAY integers, 590
DISPLAY numbers with decimal places, 1,463 packed items, 1,088 COMP
items.

| | as written | the machine's way | |
|---|---:|---:|---:|
| kmove | 381 ms | 271 ms | -29% |
| karith | 419 ms | 336 ms | -20% |
| kseq | 205 ms | 167 ms | -18% |
| kedit | 573 ms | 484 ms | -16% |
| kstring | 258 ms | 233 ms | -10% |
| kreport | 321 ms | 305 ms | -5% |
| ksearch | 258 ms | 249 ms | -3% |
| kidx | 393 ms | 390 ms | -1% |
| ksort | 490 ms | 489 ms | 0% |
| csv2fw | 279 ms | 255 ms | -9% |
| majesty's batch, 28 runs | 621 ms | 583 ms | -6% |

Integers alone gave the kernels 0.3 to 3.8% (stage 3 had already put
the hot ones in registers); the rest is decimals.  karith's items are
COMP-3 and signed DISPLAY with decimal places, every one standing
alone: 11.16 G instructions as written, 8.31 G now.

majesty's batch moves less than the kernels, and the census says why.
Its amounts are packed items of eleven digits, six bytes where a
binary item of eleven digits wants eight: in place, 46 of its items
changed.  Moved out of their records, with a report's SOURCE taken for
the use of a number that it is (the Report Writer moves it to the
print line), and with tables, 118 do: 19.4% of its references.  The
rest are held by partners (392 references), named groups, and storage
that is LINKAGE or a record's.  csv2fw's gain is its tables: the COMP
table its hot loop walks is no longer swapped at each access (3.955 G
instructions to 3.794 G).  What is left of the batch's time is
csv2fw's text and each short run's translation.

What keeps a number of these kinds as written, all corpora, by
references: storage not the program's 10,963; a named group 9,176
(tables' rows among them); a partner that must stay as written 7,868;
a use of its bytes 3,857; an address given away 3,123; a used
redefinition 1,937; a kind not taken (SIGN LEADING or SEPARATE, P,
more than eighteen digits) 1,375.

The arithmetic itself is what is left in karith: 4,155 instructions a
pass for seven statements -- 327 of generated code, a 152-instruction
division routine, and the rest inside the runtime's get and put
kernels (which the DBT runs natively, so they cost less than they
count).  Equivalent C is a few dozen instructions and some 64-bit
divisions.  That is the lowering's work, not the data's.

### What the tests for it found

- **An item laid over another starts as the other was left.**  `01 X
  PIC X.  01 N REDEFINES X PIC 9.` begins as a space.  Every gate
  passed with N changed; the generator's first forty programs did not.
- **DIVIDE ... GIVING an operand, with REMAINDER** (a defect of the
  compiler, not of this step).  The numeric stack's path stored the
  quotient and then worked the remainder out from the operands again
  -- one of which was now the quotient: `DIVIDE D INTO 10 GIVING D
  REMAINDER R` with D = 4 left R = 0.  The register path, which the
  same statement takes once its items are binary, left 2, and the two
  compiles of one program disagreed.  The operand's value is now kept
  before the quotient is stored (`tests/free/divremgiving`; GnuCOBOL
  agrees with every line).
- **DECIMAL-POINT IS COMMA and DISPLAY of a packed or binary number**
  (a defect of the runtime).  It showed a period where a DISPLAY item
  shows the comma (`tests/free/dpcommausage`; GnuCOBOL shows the
  comma).
- **`ADD P TO P R`** (a defect of the compiler).  The in-line decimal
  add read its operand again for each receiver, so R got P as the
  statement had just left it; every other path adds P as it was, which
  is what the text says of a statement with several results
  (X3.23-1985 6.4.6).  The harness's own `free/hotdec` held the wrong
  line -- GnuCOBOL's, which reads the operand again too
  (`docs/oracles.md`).  With the operand among several receivers the
  in-line add now leaves the statement to the path that keeps the
  value.

Each of the three defects was one statement compiled two ways by the
same compiler, with the two disagreeing: the kind of fault a compiler
with several paths for one statement has, and that changing how the
operands are written brings out.

### What checks it

- The same compiler with `-fno-native-items` is the oracle.
  `tests/gen/gen-native.py` writes items of every kind taken, alone
  and not, used as numbers and as bytes in every way above, and prints
  them; 300 programs agree (harness gen/native runs 60), and so do
  forty programs of each of the other eleven generators.
- `tests/census_test.c` (harness gate 1h): the rule about addresses,
  on events written for it -- the register used in between, the
  address formed twice, a routine taking another register, code cut
  away, a name with no address.  38 checks; of seventeen mutants of the
  rule fifteen fail it, and the other two change nothing an item can
  show.
- Mutants of the verdict: see cobol ISSUES-123.
- With the switch off, 1,506 programs compile to the assembly they did
  before any of this.

## Step 3: PERFORM classified (2026-10-03)

The same instrument, pointed at the PROCEDURE DIVISION.  A paragraph is
a label; whether it may be a procedure depends on everything that names
it, so the compiler writes the facts (`src/cobc/pcensus.h`: each
paragraph and how its code ends -- falling through, a jump, STOP RUN;
each out-of-line PERFORM with its range and form; each GO TO with its
source, DEPENDING, ALTER and EXEC SQL WHENEVER included; the declarative
sections) and `tests/performs.py` draws the verdicts.  Nothing changes
in the code.

A performed range (first paragraph to THRU paragraph, or a section) is a
**procedure** when it is entered at its top by PERFORM and by nothing
else: no GO TO from outside it reaches a paragraph of it, no GO TO
inside it leaves it, the paragraph before it does not fall into it from
code that runs in line, it is not the program's entry, and no other
range partly overlaps it.  Whether code runs in line is a fixed point
over the GO TOs: the entry does; a GO TO's target does when the GO TO
comes from in-line code or from outside every range both are in (a GO
TO into a range's own exit paragraph from inside it keeps control in the
range); the paragraph after one that runs in line and falls through
does.

| | ranges | procedures | PERFORMs | to procedures |
|---|---:|---:|---:|---:|
| majesty | 239 | 92.9% | 446 | 94.2% |
| CCVS-85 | 4,141 | 94.4% | 39,596 | 98.4% |
| X-COBOL | 2,259 | 70.6% | 5,721 | 76.6% |
| Open Systems | 1,517 | 33.9% | 3,341 | 37.0% |
| all | 8,237 | 76.6% | 49,272 | 91.6% |

- **PERFORM is a call, nine times in ten.**  In the working corpora and
  in CCVS the ranges named are procedures; a GO TO within one (13.9% of
  ranges, 4.0% of PERFORMs) is a branch inside the procedure, not a
  reason against it.
- **What refuses the rest, by ranges:** fallen into 13.8% (X-COBOL's
  share is 28%: a mainline that ends without STOP RUN or GOBACK and
  runs on into its own subroutines), GO TO out 7.4%, GO TO in 6.0%,
  nested in another range 5.7%, overlapping 1.1%.
- **The Open Systems suite is the other COBOL.**  RM/COBOL-74 style:
  8,253 GO TOs against 3,341 PERFORMs; 253 ALTERs; a third of its
  ranges are jumped into and a third jumped out of.  It is the corpus
  that keeps the general mechanism honest -- a lowering that could only
  do procedures would do a third of it.
- **GO TO itself:** 26,815 in all; 19,421 from the mainline (the
  program's own control flow, not a procedure's), 5,642 within every
  range they are in, 1,086 leaving a range, 389 DEPENDING, 266 ALTER.
- **Paragraphs:** 46,565; 12.3% are reached by nothing -- after a STOP
  RUN, or a GO TO chain's leftovers.

What the lowering takes from this: a procedure range becomes a
function-like block with one entry and one return; a range that is
fallen into or jumped into is a block whose paragraphs are labels of
the unit's one function with the PERFORM return stack kept as it is
(the `switch` on the entering PERFORM's number); a GO TO out of a range
abandons frames exactly as the runtime does today.  Nothing here needs
a construct HIR lacks.

## What follows

Staged; each step is measured before the next is begun.

1. **The census** -- this.
2. **A representation of its own for items standing alone, in the
   emitter as it is.**  Done for numbers to eighteen digits, in place
   or in a cell outside the record (above).  What is left of it, by
   what the measurement says:
   - *LOCAL-STORAGE*, items under a group named only by INITIALIZE,
     and a table's rows named only to be initialized or searched.
   - *Partners.*  After storage that is not the program's, the
     largest reason a number stays as written is an item of the same
     picture it is copied to or from.
     Whether a copy between two numeric items of one description must
     carry bytes that are not a number is a ruling, not an analysis
     (IBM's NUMPROC(PFD) is the same question).
3. **PERFORM classified.**  Done as a census (above): 91.6% of
   PERFORMs name a procedure; the facts per range are in the
   `.perform` files for the lowering to read.
4. **Lowering to HIR.**  Standing-alone items become HIR's own values
   (a non-escaping slot is promoted to SSA there already); the rest
   stay loads and stores at known addresses.  PERFORM needs nothing new
   of HIR: a unit is one function, a paragraph a label, and a
   paragraph's exit a `switch` on the number of the PERFORM that
   entered it -- which HIR lowers to a jump table.  The same shape can
   be written in C, so nothing here is asked of the self-hosting
   compiler that C could not ask of it.

On the HIR sources: the Fortran front keeps its own copy of the
headers (`fortran/src/hir*.h`), and COBOL is expected to do the same.
The self-hosting tree is its own universe: it may lend its source, and
it must never depend on anything built elsewhere in the tree.  Whether
COBOL's copy stays a copy or becomes a fork is left to be decided by
what the work needs.
