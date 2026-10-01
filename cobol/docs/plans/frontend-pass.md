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
| SEARCH's WHEN and AT END bodies | statement lists | `parse_statements` once to find their end, again to emit |
| a SCREEN SECTION entry's reference | its token position | `emit_screen_dyn_fill`, at every ACCEPT and DISPLAY |

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
2. **The register paths over trees.**  `hx_expr` and `dx_expr` read an
   `Expr` instead of tokens (their `HNode` is already the same shape),
   and COMPUTE parses its expression once: width, register tree and
   stack code all from one tree.  O_EXPR's token range goes.
3. **Boolean expressions** the same way, and O_BEXPR's range goes.
4. **Statements.**  A statement parses to a node -- receivers, senders,
   phrases, its nested statements -- and is emitted from that.  This is
   where evaluation order becomes the emitter's decision: identify each
   operand once (MOVE general rule 1), make user-function calls before
   the statement uses their results, evaluate subscripts before any
   register they could disturb is live.  Verb by verb, simplest first;
   a verb not yet converted keeps parse-and-emit.

Nothing here is an optimizer.  An SSA layer was considered and set aside
(2026-10-01): most COBOL time is in libcob, little COBOL data can live in
SSA values, and speed is not the complaint.  If it ever is, stage08's HIR
is the place to lower to, not a new layer here.
