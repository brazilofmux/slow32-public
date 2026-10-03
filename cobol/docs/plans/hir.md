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
  made an island or kept as text, and why a run with text nodes was
  refused (no loop, or none of its own; text more than a third; no
  native item; a PERFORM that may not come back).
  `S32_HIR_DUMP=.LislN`: that island's HIR as lowered and after the
  optimizer.  `S32_HIR_INLINE=0` (no PERFORM inlined) or `=line` (that
  line's alone).  The trace also says, per text node, how many of the
  island's items it syncs.  `S32_HIR_MIN=n`: the run length that
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

## Milestone 2: text statements in islands; paragraphs as islands

What keeps the real programs' loops in the text is not arithmetic but
READ, WRITE, PERFORM of a paragraph, GENERATE, CALL -- verbs with text
emitters that took months to get right and will not be rewritten as HIR
one by one.  So an island takes them as they are: a **text statement**
is a node whose code is the lines the text emitter wrote for it (kept
already, `lw_stmt_text`), emitted in place as an opaque call.  What that
needs, and where each piece sits:

- *The frame.*  The text's code addresses its scratch at `sp+8..sp+136`
  (`SLOT`, `SLOT_A..C`; emit.h FRAME).  An island with text statements
  reserves the bottom FRAME bytes of its own frame for them: the
  backend's frame grows by `hcg_frame_reserve` and every slot offset
  moves up (hcg_slot_off goes through hcg_frame) -- one line in the
  copy.  Statements that touch the unit's own slots (SLOT_PBASE, SLOT_ACT,
  the saved registers) are not taken: GOBACK, EXIT PROGRAM, STOP RUN.
- *The registers.*  Text code uses r1-r13, r30, r31 as scratch, never
  r14-r28 (loopreg's r14-r17 only through marks, which are stripped from
  a text node).  So a text node is a call that also clobbers r11-r13 and
  r30: the allocator's callee pool starts at r14 in such an island
  (`ra_callee_skip`), r30 stays out, and HI_CALL's crossing rules do the
  rest.  The node is an HI_CALL of a name the emitter recognises
  (`.Ltext<k>`): no marshalling, no jal, the lines instead.
- *Control.*  A text node may not leave: every jump and branch in its
  lines targets a label of its own, except PERFORM of a range the PERFORM
  census calls a procedure (entered at its top by PERFORM alone, no GO
  TO in or out, not fallen into, not overlapping) -- which returns to the
  line after its jump.  The census is taken in the compiler now
  (`pc_is_procedure`), from the same records `tests/performs.py` reads,
  and known only when the unit is parsed: a run with such a node waits
  for the unit's end to be resolved.  GO TO, EXIT PERFORM, NEXT SENTENCE,
  an EC raise that branches to a handler, keep a run in the text.
- *The items.*  A text node may read or write any item: the native items
  an island holds are stored to their storage before it and loaded again
  after it.  (The census could say which items a paragraph names; that
  is the refinement.)
- *Paragraphs.*  A paragraph that is a procedure and whose statements an
  island takes becomes one island, called from where the paragraph's
  code was; a PERFORM of it from another island is then a direct call,
  without the perform stack.
- *What pays.*  A loop of text statements alone is the text's; an
  island is formed where the loop's own control, its IFs and arithmetic
  are HIR's and the text nodes are a minority -- measured, as before.

### Measured (2026-10-03): the first half, and what it says

Text nodes are in: all of the above but the paragraphs, gated by the
twelve generators on and off.  The measurement that mattered: the real
programs' loops are `PERFORM paragraph UNTIL`, so their runs are in a
loop that is not their own.  Counting them as loops (the census records
whether a PERFORM iterates, `lw_para_looped` follows it transitively)
made csv2fw 23 islands and **+5%** slower.  Three of the causes were the
island's codegen, fixed and kept for every island -- a DISPLAY integer in
storage fetched by `cob_get_num` where the text decodes it in line
(`lw_dec_load`), AND/OR evaluating both sides where the text
short-circuits (`lw_cond_br`), a literal's bytes loaded where an `xori`
immediate serves, and `x == 0` as `xori; seq` in the backend copy; the
truncation guard also takes |v| no more when the value cannot be negative.
That brought csv2fw to **+0.2%**: at par.  What remains is the shape
itself: a run that does not hold its loop loads its items at entry, syncs
them round each text node and stores them at exit on every pass; the text
does none of that.  So such runs stay text (the trace says so), and the
kernels took the fixes: kmove -42 -> -50%, kseq -27 -> -32%, kstring -5
-> -8%, kreport -2 -> -4.6%.

The conclusion for the second half: the island must hold the whole
paragraph -- one entry, one exit, the items in registers across every
statement between, the syncs only round the text nodes that name them --
and, further on, the PERFORM UNTIL that drives it, as a loop in HIR
calling the paragraph's function.  GO TO within the paragraph's range
would be a branch inside the island; the PERFORM census already says
which paragraphs are procedures.

### The second half (2026-10-03): a PERFORM as its paragraph's nodes

Done not as a function per paragraph but by **inlining**: a text node
that is a plain `PERFORM range` (the verb itself, by the statement's own
parse), whose paragraphs hold nothing but nodes between their label and
their end mark (`#@E`), and which comes back, is emitted as the
paragraphs' own nodes in its place (`lw_inline_performs`, at the unit's
end before the runs are resolved).  No perform stack, no entry and exit
of a function, the items in registers straight through; the paragraph's
own code stays for whoever else reaches it, and a text node emitted in
two places gets fresh labels at each (`lw_relabel`).  Recursion is
refused by reachability; `S32_HIR_INLINE=0` or `=line` bisects.

What it needed, each measured on csv2fw's per-byte path: reference
modification with a computed start (one unsigned compare, the runtime's
check on the failing branch), subscripted storage items and native
tables' elements as values and receivers, the inline DISPLAY/packed
store -- and above all **syncing only what a text node can touch**.  A
native item is reached by its label or not at all (the census admits
nothing else), so a node stores and reloads an item only if its lines
name the item's record, or a range its own code performs does, found
transitively through the ranges' text, the islands they call and their
placeholders' nodes.  Without that, inlining made csv2fw 5% slower (13
items synced round every text node, every byte); with it, csv2fw is
3.794 -> 3.628 G instructions (-4.4%, 0.25 -> 0.23 s), and the kernels
moved again (kmove -62%, kseq -53%, kstring -14%, kreport -9%).

Two lessons the generators taught, both about the text: a text node's
lines may use r14-r28 (loopreg rewrote the in-line loop before the
statement's text was cut) and so clobber the island's live values --
hidden while every item was reloaded after every node; and the text's
own hot word path was wrong for MOD/REM over a wide dividend into a
wrapping receiver, where the island was right (it takes the checked
word mode now).  A differential that breaks is a question for the
oracle, not a verdict against the new side.

Next on this path: EVALUATE as an IF chain (parse-byte's two EVALUATEs
are text nodes with PERFORMs inside, so they still sync every item each
byte); then the footprint by item per paragraph.

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
