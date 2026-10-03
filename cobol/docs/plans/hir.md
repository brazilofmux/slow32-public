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
  heavy -- decimals, a value past a word, a division, ROUNDED: what the
  text emitter sends through the runtime's fetch and store -- or there
  are `S32_HIR_MIN` (default 4) of them.  A statement alone over
  integers in a word is already in place in the text, and an island of
  it costs the call: kmove +0.3% with every run an island, kedit -14%
  from its COMPUTE and ADD alone.

Inside an island every native item it names is an **alloca**: loaded
from its storage on entry, stored back on exit if written.  SSA
construction promotes the allocas, so across a lowered loop the items
are registers and nothing else -- the thing the census was for.

## What is lowered (milestone 1: karith's loop)

Statements over native items, numeric literals and ZERO only:

- COMPUTE, ADD (TO, GIVING), SUBTRACT (FROM, GIVING), MULTIPLY (BY,
  GIVING), DIVIDE (INTO, BY, GIVING; REMAINDER later), MOVE numeric to
  numeric, with ROUNDED; no SIZE ERROR, no ROUNDED MODE, no EC checks on.
- IF with relation conditions on numbers, AND/OR/NOT.
- In-line PERFORM VARYING (one level, TEST BEFORE or AFTER) and PERFORM
  UNTIL.

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
SLTU carry; mul, div and rem through `__muldi3`, `__divdi3`, `__moddi3`).

## Switches and checks

- `-fno-hir`, `S32_HIR=0`: the lowering off, the text emitter alone.
  `S32_HIR_TRACE=1`: each statement taken or refused, and why; each run
  made an island or kept as text.  `S32_HIR_MIN=n`: the run length that
  pays on its own (0: every run).
- The on/off differential is the gate: `tests/gen/gen-native.py` with
  `-fno-hir` against the default, the harness with `S32_HIR=0`, and the
  kernel and majesty numbers before and after.

## Measured (2026-10-03, first day)

karith's loop is one island: 8.31 G instructions -> 2.35 G (-72%),
0.32 s -> 0.155 under the DBT; what is left is the 64-bit division
routines, which the DBT runs natively.  kedit -14% (two heavy
statements); the other kernels and csv2fw unchanged to the instruction
(their loops hold statements the islands do not take yet); majesty's
batch within its noise.

## What follows

The integer functions (MOD, REM, INTEGER, ABS, MAX,
MIN) the register trees already take; non-native numeric operands
fetched by `cob_get_num` inside an island; DISPLAY of a native item (the
item stored before the call); then procedures as functions (the PERFORM
census) and the whole unit.
