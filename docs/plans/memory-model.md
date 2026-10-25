# One Memory Model for stage08

Started 2026-10-25, the evening the crypto round closed at 1.1x to
1.3x of clang's executed instructions.  The round's last lesson named
this document: a pointer-offset alias rule written for one pass
(dead-store elimination) was reused by another (store-to-load
forwarding), and the second use was wrong in a way neither pass could
see -- it held Argon2's rows in registers that then spilled, and the
x86-64 DBT ran 12% slower (33874324).  The fix was a flag telling one
function to answer differently depending on who asked.  That flag is
the symptom this plan removes.

## What is wrong

stage08's optimizer has the shape of a real compiler -- SSA, a pass
list, BURG selection, iterated-coalescing allocation -- and each pass is
short enough to read top to bottom.  What it lacks is the facilities the
passes should share.  Five places each answer "what memory does this
address name" on their own:

- `ho_addr_resolve` / `ho_may_alias` (hir_opt.h): forwarding and DSE,
  one block at a time;
- `ssa_sr_root` (hir_ssa.h): the alloca an address derives from, for
  the late scalar replacement;
- `ho_ds_root` (hir_opt.h): the same question, for dead slots;
- the escape scan in `ssa_find_promo` (hir_ssa.h): which allocas have
  only load/store uses, for mem2reg;
- `h_ld_ro` (hir_licm.h): a front end's promise that a load is from
  read-only memory, the only way a load is hoisted today.

And five more each walk an instruction's operand references by hand
(src1, src2-if-a-reference, call arguments, phi arguments), with their
own list of which kinds carry an argument list.  Two of those lists
disagree.

## The steps

Each step is a commit with the full gate run.  A step that changes no
answer is gated by byte-identical output: the SQLite build and the five
crypto benchmarks must assemble to the same bytes before and after.

1. **One walk, one resolver.**  `hi_nrefs`/`hi_ref` in hir.h: the
   operand references of an instruction.  `hir_alias.h`: `al_resolve`
   (frame slot, global, pointer plus offset, or unknown), `al_may_alias`,
   and `al_roots` (the alloca each address derives from, with its
   offset).  Every pass above calls these; none keeps its own copy.
   No answer changes.  The gate is the byte-identity one.

2. **Memory state across blocks** -- measured and set aside (see the
   log).  Carrying the "last known value" table across blocks as a
   dataflow (meet by intersection, empty over a back edge) is sound and
   was built; what it forwards is worth having only when the forwarded
   value can be held in a register across the join, and whether it can
   is the allocator's question (step 5).  Forwarding only values already
   live at the join costs nothing and gains nothing measurable.  The
   test (`test_mem_join`) stays as the correctness line.

2a. **`volatile` as a barrier.**  The parser drops the qualifier.  Carry
   it to the HIR (a type flag on the access), and have forwarding, DSE,
   dead slots and LICM treat a volatile access as a call does: nothing
   known across it, never forwarded, never removed, never hoisted.  A
   prerequisite for step 3, and a correctness fix on its own.

3. **Loads out of loops.**  A load whose address is invariant and whose
   bytes no store in the loop body may touch is invariant.  LICM hoists
   it to the preheader.  `h_ld_ro` becomes one way to establish that
   and stops being the only one.

4. **Stores dead across blocks.**  A store is dead when every path from
   it to the function's exit overwrites its bytes before any may-read.
   Today a store is dead only within its block.

5. **Then, measured on both DBTs:** whether forwarding through pointer
   offsets (the half of 7c16eb9f that was undone) pays once the
   allocator knows which values it would hold.  That is the allocator's
   question -- live-range splitting -- and it is a separate front, as is
   lowering 64-bit operations late.  Neither starts under this plan.

## Rules

- Executed-instruction counts are not the measure.  Every step that
  changes code is timed under slow32-dbt on arm64 and on kagura's
  x86-64, and its frame traffic (`(ldw|stw) .., r29`) is compared.
- The five differentials are the correctness gate: run-tests with the
  fixed point, SQLite acceptance, libc, stage07, FP.  An alias bug is
  wrong code, and only they see it.
- The Fortran and COBOL copies of these headers are not touched; the
  user's ruling of 2026-10-25 keeps this on the C compiler.
- No new pass may answer a memory question its own way.  If
  `hir_alias.h` cannot answer it, the facility grows.
- `volatile` never reaches the HIR: the parser reads the qualifier and
  drops it (found at step 2).  The within-block forwarding has always
  ignored it, and the kit's own MMIO accesses go through calls, which
  are barriers.  Step 3 must not start until a volatile access is a
  barrier in the IR: a hoisted poll of a device register would never
  see it change.

## Log

- 2026-10-25: step 1 (cce2a178).  Byte-identical output.
- 2026-10-25: step 2, measured.  The full join meet: SQLite 281,696 ->
  279,246 static, 2,471,316 -> 2,457,872 executed (-0.5%), PBKDF2
  -0.1%; but sqlite3VdbeExec, the hottest function, +65 instructions
  and +152 frame accesses, and a 200,000-row SQLite workload slower
  under the DBT: arm64 1.60 -> 1.63 s, x86-64 12.77 -> 13.5 s, for
  0.1% fewer instructions executed.  The dispatch block's loads were
  forwarded into the 190 cases and held in registers across the
  dispatch.  Straight-line-only (one predecessor with one successor):
  -36 static, nothing else.  Only values already live at the load: -22
  static, VdbeExec identical, times identical.  Set aside; the
  allocator decides this one.  Not committed; the pass as built is in
  this session's record.
- 2026-10-25: step 2a.  `volatile` reaches the code: the parser keeps
  the qualifier as two bits (before the first star: the object, or the
  pointee when stars follow; after one: the pointer itself), a
  declaration or cast puts them on the nodes that name the object
  (is_vol) or point to it (is_volptr), the lowering marks a volatile
  lvalue's address (h_voladdr, carried by ADDI), and hi_emit marks each
  LOAD and STORE through one (h_vol).  Forwarding, DSE, dead slots,
  mem2reg, the late scalar replacement, DCE and LICM treat the access
  as a barrier.  The five crypto benchmarks are byte-identical; SQLite
  changes in dekkerMul2, kahanBabuskaNeumaierStep and
  sqlite3_test_control, the three places it says volatile, and the
  200,000-row run is 1.5% slower under the x86-64 DBT (unchanged on
  arm64): sum() calls the two Kahan-Babuska helpers once per row, and
  their doubles now stay in memory as written.  Found on the way: two
  reads of one plain global in an expression are two loads, because
  each access builds its own GADDR and forwarding matches by address
  value -- a candidate for the alias facility to answer.
