# Step 4: lowering to HIR

Begun 2026-10-03, after the census (`census.md`) had given the compiler
numbers written the machine's way and a reading of which PERFORMs are
procedures.  This is the plan for the first part of step 4: what is
lowered, into what, and how it sits inside the text emitter.

## The shape

The emitter writes assembly a statement at a time into an in-memory
list of lines (`emit.h`); phrases are read into blocks of lines and
placed (`Block`, `block_cut`, `block_put`).  The lowering keeps that:

- A statement the lowering takes emits **one placeholder line**,
  `\tisland <n>`, in place of its code, and records the statement as a
  node of its own (`LStmt`, in `src/cobc/lower.h`): the receivers, the
  value tree with each node's scale and bound, the condition, the body.
  The line is an instruction nobody lists, so every reading of the text
  that follows registers (`loopreg.h`) treats it as "anything may have
  happened" -- which is right: the island reads and writes the items it
  names behind the text's back.
- An IF whose condition is numeric and whose branches are nothing but
  placeholders becomes one placeholder; an in-line PERFORM VARYING or
  UNTIL whose body is nothing but placeholders likewise.  Anything else
  is laid out by the text emitter as before, placeholders inside it.
- The statement's text is written too, after the placeholder, and cut
  away with the node when the statement is over (`lw_stmt_text`, from
  `parse_statement`); a folded IF or PERFORM writes its text over its
  branches' kept text.  So every run of placeholders can still become
  text.
- Before anything reads the code as code -- `loopreg`'s regions at an
  outermost in-line PERFORM's end, and its unit-wide pass -- each
  maximal run of placeholders is resolved (`lw_resolve`): a run that
  pays becomes an **island**, one HIR function compiled through
  stage08's pipeline -- SSA, the optimizer, LICM, BURG, the register
  allocator -- its text kept until the unit has returned (`lw_flush`)
  and reached by `jal r31, .LislN` where the run was; a run that does
  not pay becomes its statements' own text.  The island saves what it
  uses (r11-r28 are callee-saved in HIR's ABI), so the text around it
  loses nothing it held.  A placeholder left in the text while loopreg
  read it cost csv2fw 6%: an instruction nobody lists makes it forget
  what the registers hold.
- A run pays when a loop is among its statements, or one of them is
  heavy -- decimals, an eight-byte item, ROUNDED, a quotient with
  decimals: what the text emitter's word path (`hx_*`) does not take and
  sends through the runtime -- or there are `S32_HIR_MIN` (default 4) of
  them.  (Measured before MULH: a product past a word cost a call then;
  it is two instructions now, and the criterion may be loosened again.)  A statement alone over integers is already in place in the
  text, a product past a word included (the word path tests for
  overflow; the island calls for 64 bits), and an island of it costs
  the call: kmove +0.3% with every run an island, ksearch +5% from one
  MOD; kedit -14% from its COMPUTE and ADD alone.

Inside an island every native item it names is an **alloca**: loaded
from its storage on entry, stored back on exit if written.  SSA
construction promotes the allocas, so across a lowered loop the items
are registers and nothing else -- the thing the census was for.

## What is lowered (milestone 1: karith's loop)

Statements over native items, numeric items in storage (fetched and
stored by the runtime: `cob_get_num`, `cob_put_num_x`, the edited
forms; a MOVE between two of one descriptor is a byte copy), numeric
literals and ZERO:

- COMPUTE, ADD (TO, GIVING), SUBTRACT (FROM, GIVING), MULTIPLY (BY,
  GIVING), DIVIDE (INTO, BY, GIVING; REMAINDER later), MOVE numeric to
  numeric, with ROUNDED; no SIZE ERROR, no ROUNDED MODE, no EC checks on.
- IF with relation conditions on numbers, AND/OR/NOT.
- FUNCTION MOD, REM, INTEGER, INTEGER-PART, ABS, MAX, MIN, as the
  register trees take them.
- Alphanumeric MOVE and equality compare of lengths the compiler knows
  (`lw_bytes_ref_ok`): subscripts literal or an integer item, reference
  modification with literal positions, literals and SPACE/ZERO; the
  receiver takes the sender's first bytes and spaces, a compare xor-s
  chunks or calls memcmp; two items the text compares bytewise
  (`cmp_is_bytewise`) are compared so, any relation.  Not ordering of
  other bytes, not a collating sequence, not JUSTIFIED, edited, national
  or bit items.
- DISPLAY of literals and items to the console, ADVANCING or not (a
  native item is stored to its storage before the call).
- In-line PERFORM VARYING (AFTER too; TEST AFTER with one level) and
  PERFORM UNTIL.

## What the arithmetic must equal

The store is `cob_k_put_scale` + the binary store (`libcob/kern.h`),
written out: align the scale (truncating, or ROUNDED: |r| >= 5*10^(m-1)
adds the dividend's sign), scaling up drops digits that cannot survive,
truncate the magnitude to the picture's digits, an unsigned receiver
takes the magnitude.  Every step is skipped where the bound proves it
does nothing.  A division at the top is the stack's `cob_xdivn` rule
written out: the quotient is produced to `want = min(max(sa, sb) + 6, 18,
need)` fraction digits, at least 9, unless it reaches 17 digits first --
so the quotient's scale is fixed at compile time exactly when the bound
of `|a| * 10^e` is below 10^17, and the statement is refused otherwise.
A zero divisor leaves the receivers alone.

The bounds are the ones `arith_reg.h` computes for the decimal register
path (`dx_check`: `g_dsc`, `g_dbd`); the lowering reads them off the
same `HNode` tree.  A node whose bound is below 2^31 is computed in a
word; the rest in word pairs, as stage08 lowers `long long` (add with
SLTU carry; div and rem through `__divdi3`, `__moddi3`) -- except the
product, which is MUL and MULH of two words, or MULHU and two MULs of
pairs: the HIR copy gained the two kinds (its first divergence from
selfhost, listed in `src/hir/hir.h`), where stage08 calls `__muldi3`.

## Switches and checks

- `-fno-hir`, `S32_HIR=0`: the lowering off, the text emitter alone.
  `S32_HIR_TRACE=1`: each statement taken or refused, and why; each run
  made an island or kept as text.  `S32_HIR_MIN=n`: the run length that
  pays on its own (0: every run).  `S32_HIR_ONLY=a[-b]`: islands only
  of the runs beginning on those source lines -- bisect a differing
  program by line range down to the one island.
- Content a picture does not describe -- characters in a COMP item after
  a group MOVE or READ INTO -- is nobody's promise: the text's word path
  and an island may make different numbers of it (both compute from the
  picture's bound).  The generators keep their numeric items numeric.
- The on/off differential is the gate: `tests/gen/gen-native.py` with
  `-fno-hir` against the default, the harness with `S32_HIR=0`, and the
  kernel and majesty numbers before and after.

## Measured (2026-10-03, first day)

karith's loop is one island: 8.31 G instructions -> 2.35 G (-72%),
0.32 s -> 0.155 under the DBT; what is left is the 64-bit division
routines, which the DBT runs natively.  With items in storage, the
functions and the byte moves and compares taken, kmove's and kedit's
loops are whole islands: kmove -42% (0.28 -> 0.12 s), kedit -26%,
kseq -26%, kstring -6%, kreport -4%, ksort -3%, ksearch and kidx about
even (their loops hold SEARCH, READ and PERFORM of paragraphs);
csv2fw and majesty's batch within their noise.

## What follows

Where the real programs' time is -- csv2fw's byte loop, the report
programs' READ loops -- the loops hold READ, PERFORM of paragraphs,
EVALUATE, alphanumeric MOVEs and compares, which no island takes; those
need the plan's next milestone, procedures as functions and the unit as
one HIR function, with the text emitter's verbs lowered one by one.
Nearer: ordering compares of other bytes; a checked word path for a product past a
word (the text's hx mode 2), so a MOD of one need not be a 64-bit
remainder -- the integer functions (MOD, REM, INTEGER, ABS, MAX,
MIN) the register trees already take; non-native numeric operands
fetched by `cob_get_num` inside an island; DISPLAY of a native item (the
item stored before the call); then procedures as functions (the PERFORM
census) and the whole unit.
