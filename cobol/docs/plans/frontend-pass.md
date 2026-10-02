# The front-end pass: parse first, emit second

s32-cobc emits code as it parses.  That was the right shape for a
compiler that had to exist, and it still is for most statements, but it
keeps producing one class of bug: code whose order is the order the
tokens were read in, not the order the values are needed in.

- A reference modifier's computed start addressed its own subscripted
  operands through r11, the register the outer reference's offset was
  accumulating in (ISSUES 119).
- Arithmetic-expression subscripts (2002) had to be evaluated and
  parked on the numeric stack before the address register starts, last
  subscript first, so they come off in order.
- UPON SYSERR is found by looking ahead before any operand is written.
- A user-defined function met while scanning ahead makes no call, so
  everything that scans ahead must later re-read the same tokens to get
  the call made (`ftemp_scan`, `opnd_scanned`).

Each was fixed with a hand-built ordering.  The general fix is to parse
a whole construct into a tree and let the emitter choose the order.

## What re-parses today

The compiler remembers expressions as token ranges and parses them
again, with emission on, when it wants the code:

| Where | Range | Re-read by |
|---|---|---|
| expression subscript (2002) | `Ref.sub[k].x0..x1` | `emit_expr_pos_push` (twice: once to learn its width, once to emit) |
| computed refmod start, length | `Ref.rm_s0..rm_s1`, `rm_l0..rm_l1` | the same |
| computed refmod of a function | `Opnd.fs0..fs1`, `fl0..fl1` | the same |
| expression operand (conditions, function and CALL arguments) | `Opnd.e_start..e_end` (O_EXPR) | `emit_expr_tokens`; and the register paths' own parsers (`hn_arg`) |
| boolean expression | `Opnd.e_start..e_end` (O_BEXPR) | `parse_bexpr` |
| COMPUTE | the statement's own tokens | a width scan, `hx_expr`, `dx_expr`, then `parse_expr` for real |
| a MOVE sender's refmod, a SEARCH ALL argument | the ranges above | token scans for names (`move_needs_temp`, `sa_uses_index`) |
| ADD/SUBTRACT/MULTIPLY/DIVIDE GIVING | the operands, scanned ahead | parsed again where the code goes |
| SEARCH's WHEN and AT END bodies | statement lists | `parse_statements` once to find their end, again to emit (step 4: blocks) |
| a SCREEN SECTION entry's reference, a report's SOURCE and CODE, positioned LINE/POSITION/AT identifiers | token positions | at every ACCEPT, DISPLAY, GENERATE (step 4: parsed once, at first use, and kept) |

## Steps

Each step is a refactor: `tests/asm-snapshot.sh` before and after
(1477 programs: the harness, Open Systems, majesty, CCVS-85, X-COBOL),
and the two snapshots diff empty -- or every difference is accounted
for in the commit -- plus all the usual gates.

1. **Expression trees for the stored ranges** (done 2026-10-01, ISSUES
   121).  An `Expr` (operator,
   two children, or a leaf `Opnd`) is built by the scan that today only
   records the range; `emit_expr` walks it.  Subscripts, refmod start
   and length, function refmods and O_EXPR operands carry an `Expr *`;
   `emit_expr_tokens` and the third parse in `emit_expr_pos_push` go
   away.  A user-defined function inside one keeps its call with the
   leaf, made when the leaf is emitted.  The name scans become walks
   over resolved operands.
2. **The register paths over trees** (done 2026-10-01, ISSUES 121).
   `hx_expr` and `dx_expr` read an
   `Expr` instead of tokens (their `HNode` is already the same shape),
   and COMPUTE parses its expression once: width, register tree and
   stack code all from one tree.  O_EXPR's token range goes.
3. **Boolean expressions** the same way, and O_BEXPR's range goes
   (done 2026-10-01, ISSUES 121).
4. **Statements.**  A statement parses to a node -- receivers, senders,
   phrases, its nested statements -- and is emitted from that.  This is
   where evaluation order becomes the emitter's decision: identify each
   operand once (MOVE general rule 1), make user-function calls before
   the statement uses their results, evaluate subscripts before any
   register they could disturb is live.  Verb by verb, simplest first;
   a verb not yet converted keeps parse-and-emit.
   - ADD, SUBTRACT, MULTIPLY, DIVIDE (done 2026-10-01, ISSUES 121):
     an `Arith` node, user-function calls first; their SIZE ERROR
     phrases' statements are still parsed where their code goes, until
     nested statements are nodes too.
   - References in the REPORT and SCREEN sections, and positioned
     DISPLAY/ACCEPT's LINE, POSITION and AT identifiers (done
     2026-10-01): parsed once, at first use, and kept.
   - IF (done 2026-10-01): the condition and both branches read whole,
     each branch a Block, then laid out (emit_if).  Labels, literals and
     descriptors are allocated in a different order, so from here a
     step's check is `tests/asm-equiv.py` over the two snapshots: the
     same code, labels renamed in order of appearance and data in any
     order.
   - With the branches known before the IF's code, a branch that is
     one jump -- GO TO, NEXT SENTENCE -- becomes the condition's own
     branch to its target, and an empty THEN (CONTINUE) a branch round
     the ELSE: 5301 of the corpus's 5821 branch-over-jump shapes gone,
     about 7700 instructions.  The rest are phrases (AT END GO TO,
     INVALID KEY GO TO), for when the phrases are nodes.  A change of
     code, not a refactor: checked by running the corpus (harness,
     majesty, the papers, CCVS-85 before and after).
   - The conditional phrases (done 2026-10-01): [NOT] ON SIZE ERROR
     read with its statement before any code (SizePh; the four verbs
     and COMPUTE), and every phrase pair -- SIZE ERROR, AT END, INVALID
     KEY, AT END-OF-PAGE, ON EXCEPTION, ON OVERFLOW -- laid out by one
     emitter (emit_phrases) from Blocks: a phrase that is one jump is
     the status test's own branch, and an ON phrase with no NOT after it
     no longer jumps past nothing.  Checked by running: CCVS-85 before
     and after, and tests/gen/run-self.sh (new): generated programs
     through the compiler before the change and after, the outputs the
     same bytes; mutation-tested (a wrong branch sense in either status
     test shows in 100 and 62 of 100 programs).  tests/gen/gen-flow.py
     (new) generates the control flow it runs, and is in Gate 7 against
     GnuCOBOL, which found an oracle defect (free/callexc).
   - EVALUATE (done 2026-10-01): its WHEN phrases read whole -- each
     one's objects (with the code reading them makes), its test, its
     statements as a Block -- then laid out: a body that is one jump is
     its test's own branch, the last body and one ending in a jump need
     no jump to the end.  Its subjects are evaluated once, at the
     beginning (2023 14.9.13.4 rule 3).
   - Calls once, first, in the order written (2023 14.6.4, item
     identification): a statement is its user function calls, then its
     code.  Each call's code is cut out of the stream as it is made and
     placed before the statement's (parse_statement, stmt_call_cut), so
     this holds for every verb, whatever it had emitted when the call
     was read.  The operands are then plain items, their code free to
     be made any number of times, and the register paths take
     statements they had to refuse.
   - Receivers at access: 14.6.4 is "unless otherwise specified", and
     it is, for receiving items -- a MOVE's immediately before the move
     to it, an arithmetic statement's as each is accessed, a DIVIDE's
     dividend and REMAINDER, READ and RETURN INTO after the record is
     read, SET's immediately before each is changed.  Those are read as
     scans and their calls made in place where the item is stored
     (recv_calls), on the stack's stores.  STRING's and UNSTRING's
     identifiers are evaluated once, before the statement (X3.23-1985
     XVII-68, substantive changes 33 and 34; 2023 has no rule of their
     own, so 14.6.4), which calls-first already is.  A PERFORM VARYING
     item's subscripting is evaluated each time it is set or augmented:
     a user function there is refused, as BY's is.
   - EVALUATE's subject, once: an arithmetic expression or a numeric
     function is evaluated at the beginning and its value kept
     (cob_nsave) for every WHEN to compare against (cob_npush_saved);
     it was evaluated again for each WHEN, twice for a THRU.  Any other
     function's result is kept with its run-time length (cob_fn_keep)
     and made the result just evaluated again (cob_fn_kept).
   - PERFORM (done 2026-10-01): its phrases, then an inline body's
     statements as a Block, read before the loop's code (Body.blk;
     the exception-checking PERFORM is still its own path).  Then the
     loops laid out with the test at the bottom -- UNTIL, VARYING at
     every level, TIMES: one jump in, and each iteration is the body
     and the test's own branch back; an instruction less per iteration,
     the static size the same.  Openings it leaves: a loop item kept in
     a register across the body (a COMP item's every access is a byte
     swap, a truncating rem and a byte-wise store), and the out-of-line
     PERFORM's cob_perform_push / cob_perform_exit calls.
   - INSPECT (done 2026-10-01): the one verb that truly interleaved --
     it told the runtime of its item, then read and registered each
     phrase.  Read whole now (InspPh, InspRange), its calls made first,
     then the runtime's sequence emitted.  STRING, UNSTRING and CALL
     already read their operands before any code; their OVERFLOW
     phrases are Blocks through emit_phrases now, which lays a
     two-valued status out as one test with the phrases its arms.
   - SEARCH (done 2026-10-01): its AT END and WHEN bodies are parsed
     once, where they are written, and their code cut out as a `Block`
     and put after the loop -- a nested statement list as a node of
     already-made code, which serves until every verb is a node.

## Where it stands (2026-10-01)

Done: steps 1 to 3, and step 4 for every verb that has operands or
nested statements.  What still reads the source more than once, and
stays:

- Decisions.  A recursive-descent parser looks ahead to choose a
  production, and here a few of those looks are dry parses that emit
  nothing and are thrown away: is a subscript an expression
  (sub_is_expr), is SET's operand one (set_at_expr), does a condition's
  operand begin a boolean expression, is an EVALUATE subject a
  condition.  Nothing is kept from them and no call is made in them.
- The exception-checking PERFORM (2002).  It has no operands, and its
  code is its source order -- the statements, the WHEN handlers, FINALLY.
  Its one look ahead (ecp_scan) reads the WHEN phrases' exception-names,
  which must be on before the statements above them are compiled.
- ADDRESS OF in a CALL argument makes its pointer record where it is
  read; a call read after it still goes first.

The two gaps left at first are closed: an alphanumeric, national or
boolean function as an EVALUATE subject is evaluated once, its result
and run-time length kept (cob_fn_keep, cob_fn_kept); and with
EC-DATA-INCOMPATIBLE checking on, a receiver whose subscript has a call
to make is checked where it is accessed, not before the statement
(recv_access).

Openings the nodes leave, none taken: a loop item kept in a register
across the body; the out-of-line PERFORM's cob_perform_push and
cob_perform_exit calls; statement nodes proper (the Blocks hold code,
not trees), which an optimizer would want and nothing else has needed.

Nothing here is an optimizer.  An SSA layer was considered and set aside
(2026-10-01): most COBOL time is in libcob, little COBOL data can live in
SSA values, and speed is not the complaint.  If it ever is, stage08's HIR
is the place to lower to, not a new layer here.
