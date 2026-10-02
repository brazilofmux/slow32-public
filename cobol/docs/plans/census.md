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

## What follows

Staged; each step is measured before the next is begun.

1. **The census** -- this.
2. **A representation of its own for items standing alone, in the
   emitter as it is.**  An unsigned DISPLAY integer or a COMP item kept
   as a native word in memory: no decode, no byte swap, no encode; the
   written form is made only where a statement wants bytes (DISPLAY,
   STRING, a MOVE to an alphanumeric item).  The analysis of stage 3
   (`loopreg.h`) already holds such values in registers across a loop
   and a unit; with a native cell under it, its stores are one word.
   This is the step that says what the transformation is worth before
   any back end is changed.
3. **PERFORM classified.**  Which paragraphs are entered only by
   PERFORM, from where, and whether a range is ever fallen into or left
   by GO TO: the facts that let a paragraph be a procedure, or a block
   with a known set of returns, instead of an address in a cell.
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
