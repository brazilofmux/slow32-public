# Performance: the plan

The goal is a compiler of high quality: compliant, and fast.  The scope
is broad and nothing is ruled out, but it is staged, and each stage is
measured before it is begun and after it is done.  In the short term
the work goes where the time is.

`docs/performance.md` is the log: what was measured, what was changed,
what it bought.  This is the plan it is measured against.

## Rules

- **Measure first.**  `bench/prof.sh` says where a program's
  instructions go; `slow32-dbt` gives the wall time, and `-H` what the
  hooks cover.  A profile by routine name alone misleads (an edit
  routine at 68% of a profile was inside a hook): the two are read
  together.
- **Correctness is not traded.**  Every gate stands as it is.  A change
  that leaves the code alone is checked by `tests/asm-snapshot.sh` and
  `tests/asm-equiv.py`; one that changes it is checked by running:
  `tests/gen/run-self.sh` (generated programs through the compiler
  before and after), Gate 7 against GnuCOBOL, CCVS-85 before and after.
- **The reference is the guest.**  A hook is an accelerator, exact to
  the byte, never a dependency (`docs/dbt-hooks.md`); every engine runs
  the same program to the same output.
- **Nothing is frozen.**  Nothing is released, so a runtime entry point
  the compiler no longer calls is removed, not kept.

## Where the time is (2026-10-01)

Two measurements.  The kernels of `bench/vs` and jerm, below; and the
batch they stand in for, which is one program more than any other
(csv2fw: byte files, positions, one wide statement) and reads
differently.  The batch decides what is next; the kernels say what a
change did to everything else.

Under the DBT, over the nine kernels and jerm: generated code is a
fifth to a quarter of the wall time where arithmetic and moves dominate
(all of it only in a search loop); the largest share is inside the
hooks -- the numeric fetches and stores themselves; libcob code still
translated is large where the work is strings (INSPECT, UNSTRING), the
wide stack, sorting and the index files; a user function's activation
is an eighth of jerm.  The table is in `docs/performance.md`.

## Stages

### 1. Opportunistic: where the profile points

One routine or one code shape at a time, each a contained change.

- Division stops at the digits the receivers keep (done).
- Moves whose lengths the compiler can count are copies (done).
- Checked 64-bit arithmetic, for COMPUTE (done): a statement the
  pictures cannot prove is computed in 64 bits, each operation's inputs
  tested, the wide stack's code behind the tests (ksort 972 ms -> 475).
  Not yet: MULTIPLY ... GIVING and the other arithmetic verbs' own
  paths; a division inside such a statement; a SIZE ERROR phrase.
- A subscript's or a reference modification's expression in registers;
  a DISPLAY subscript's digits in line (done: csv2fw 1.20 s -> 0.90).

From the batch's own profile (csv2fw is 60% of it;
`docs/performance.md`, "the batch itself"), done:

- `v = v * 10 + FUNCTION NUMVAL(one character)`: a digit as a leaf of
  the checked arithmetic.
- The byte files: READ of a fixed-length sequential record out of the
  runtime's block buffer; READ and WRITE as small entries.
- Out-of-line PERFORM: a cell for each paragraph makes the push and the
  exit constant, and an exit nobody waits on costs no call.

- FUNCTION MIN and MAX of integers as nodes of the register trees.
- A reference-modified alphanumeric move with a computed length: the
  alphanumeric move itself, no descriptors.
- A position whose operand is itself subscripted: computed before the
  reference's own offset begins, kept in the frame.
- A numeric literal moved to an item: its bytes, from the store's
  kernel compiled into the compiler.

csv2fw 1.20 s -> 0.42 with all of it.  Next:

- fwrite and fread of a few bytes in the C library (runtime/): a short
  path when the buffer has room.  It is every program's library, so it
  is its own step, with the platform's gates.
- PERFORM in line: the push and the exit are constant now but still two
  calls.
- A class test of one character (`x(i:1) IS NUMERIC`) in line.
- The generated code: a COMP item's load is twelve instructions (it is
  big-endian); a statement fetches what the statement before stored.
  Stage 3 and stage 4, below.
- The SEARCH loops: the serial step and the binary probe.
- A user function's activation (cob_act_enter, cob_act_leave).
- INSPECT and UNSTRING in the runtime; the wide stack's MOD.

Dropped: DIVIDE ... GIVING ... REMAINDER in registers.  On binary items
it already is; no workload has it on packed or DISPLAY ones.

### 2. Operation-level hooks

Today's hooks are a fetch and a store: an operand in, a result out, one
crossing each.  A statement of two operands and a result crosses three
times, and the crossings and the conversions are where the largest
share of the time now is.  The next hooks are whole operations, in the
same mechanism (kernels in `libcob/kern.h`, compiled into libcob and
into the DBT; a content tag; a thunk that declines):

- a division kernel (ndiv_core's, its scale returned through a pointer,
  not a global);
- a numeric move, item to item: one crossing for the fetch and store;
- add and subtract an item into an item, with the store's options;
- a numeric compare of two items;
- later, as the profile says: the wide stack's kernels, the string
  statements' (their state made the caller's, so a kernel can be pure).

Each needs the kernel differential (`tests/kern-differential.sh`) and
a run on both hosts.  A kernel changes the tag: libcob and the DBT are
rebuilt together.

### 3. Fewer fetches and stores: values across statements

A value an earlier statement left in hand, used again without fetching
it; a store that a later store makes dead.  This is data flow, and it
turns on what may overlap what: REDEFINES, a group over its items,
reference modification, LINKAGE and EXTERNAL items, a CALL BY
REFERENCE.  Staged:

1. the overlap relation itself, as a tested piece (two references:
   never, always, or perhaps the same storage);
2. within a run of statements with no label between them: a store
   followed by a fetch of the same item;
3. a loop's items: the VARYING item and what the body only reads, kept
   in registers across the body, stored at its exits.

The front-end pass left statements as code in Blocks, not as trees.
Stage 3 wants trees: a statement's operands and effect kept until its
code is made.  That is its first piece of work, verb by verb, as the
pass went.

### 4. The generated code itself

A COMP item is big-endian: each access is a byte swap, a truncating
rem and a byte-wise store.  Register allocation is by hand, statement
by statement.  If stage 3's values want an allocator and the usual
passes, stage08's HIR has them (the Fortran front already lowers to
it); lowering COBOL's binary arithmetic and control flow to HIR is the
alternative to growing an optimizer here, to be decided when stage 3
is measured.

## Not performance, kept beside it

The compliance queue is unchanged by this plan: COBOL 74 behavior
points from the Open Systems corpus and the 1985 text's list of
substantive changes; the table-handling generator; the DATA DIVISION
sweep.
