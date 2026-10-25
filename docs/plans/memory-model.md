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

2b. **Addresses compared as locations.**  Forwarding keyed its table by
   address value; two namings of one global in a statement each build
   a GADDR, so both loaded.  `al_same_loc` compares two addresses as
   what they name (object and offset), and the table uses it.

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
   Today a store is dead only within its block.  Done: `al_escapes`
   (a frame slot whose address never left is invisible to calls and
   dead at return), and `ho_dse_dead_after` walks the successors.

5. **Then, measured on both DBTs:** whether forwarding through pointer
   offsets (the half of 7c16eb9f that was undone) pays once the
   allocator knows which values it would hold.  That is the allocator's
   question -- live-range splitting -- and it is a separate front, as is
   lowering 64-bit operations late.  Neither starts under this plan.

## Rules

- Executed-instruction counts are not the measure, and neither is one
  DBT's wall time by itself.  Every step that changes code is judged on
  guest work first -- instructions executed, frame traffic (`(ldw|stw)
  .., r29`), the hottest superblocks and their counts (`slow32-dbt -s`)
  -- and then timed under slow32-dbt on arm64 and on kagura's x86-64.
  A wall-time move with the guest work unchanged is the DBT's to
  explain (tools/dbt/ISSUES.md 28: a two-load reorder in one block of
  sqlite3VdbeExec cost 15% more dispatcher entries on x86-64 and 6-8%
  of the run, with arm64 unmoved); one with the guest work changed is
  the compiler's (33874324).
- The five differentials are the correctness gate: run-tests with the
  fixed point, SQLite acceptance, libc, stage07, FP.  An alias bug is
  wrong code, and only they see it.
- Measure with the machine quiet.  A kit batch or a gate running
  alongside moved the arm64 DBT time 5% and once produced an
  instruction count a third of the true one; neither reproduced.
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
- 2026-10-25: step 3.  A load in a loop body whose address is invariant,
  with no call in the body and no store in the body that may touch its
  bytes (al_may_alias, words off one pointer apart), is hoisted to the
  preheader.  Through a pointer it moves only when the body makes it on
  every trip (header block, or dominates the latch in a loop that exits
  only there), since the preheader's copy could fault where the loop's
  would not have run; a frame slot or global cannot fault.  LICM's
  dominance rule still declines anything inside an if, loads included.
  Benchmarks byte-identical (their loop loads are table lookups with
  moving addresses); SQLite 281,656 -> 281,623 static, frame accesses
  34,634 -> 34,606, the 200,000-row run 12.75-13.07 -> 12.54-12.82 s
  under the x86-64 DBT, unchanged on arm64.  The compiler's object
  crossed stage07's linker input buffer (1 MB, FILE_BUFSZ) by 1,108
  bytes; the buffer is 4 MB now, as stage08's own linker's has been.
- 2026-10-25: step 2b.  `al_same_loc`; forwarding's table matches by
  location.  Benchmarks byte-identical; SQLite 281,623 -> 281,594
  static in ten functions (a global read twice in one statement), frame
  accesses unchanged, instructions executed on the 200,000-row run
  6,834,216,050 -> 6,834,216,032, hottest superblocks identical;
  arm64 1.57-1.60 -> 1.56-1.58 s; x86-64 12.58-12.82 -> 13.67-13.87 s
  from 15% more dispatcher entries -- the DBT's (ISSUES 28 there), and
  the origin of the rule above.
- 2026-10-25: step 4.  Stores dead across blocks: `al_escapes` in
  hir_alias.h, `ho_dse_dead_after` in hir_opt.h, bounded to 48 blocks.
  The first version judged a block with no successor dead without
  reading its terminator, so every store before a `return` went -- the
  kit libc lost `srand`'s seed, `atexit`'s handler, all of `s_init`;
  the smoke test caught it, and the libc comparison showed the shape
  at once.  Fixed, the libc is byte-identical, the benchmarks too;
  SQLite 281,594 -> 281,573 static in three functions (varint buffers),
  6,834,216,032 -> 6,832,736,152 executed, arm64 unchanged; x86-64
  13.7 -> 12.7-12.9 s as the dispatcher entries fell back to 112.7 M
  (ISSUES 28: the anomaly moves with unrelated code shape).  A second
  wrong version followed: the walk marked the store's own block seen
  after scanning only below the store, so a loop's back edge into it
  never saw the loads above -- `log`'s `term` slot, loaded at the top
  and stored at the bottom of one block, lost its store; the libc
  differential's math_decl caught it.  And the pass exposed selfhost
  ISSUES-93: a slot reached only through address steps that BURG folds
  looked unused to the frame classifier, and the leaf went frameless.
  Both fixed.  What this pass finds in real code is small -- the scalar replacement and
  mem2reg take most slots first -- but DSE now reads the CFG and the
  escape set through the facility, which is what step 5 will want.
- 2026-10-25, evening: before step 5, the measure the rules ask for --
  a per-function profile of the 200,000-row SQLite run (the reference
  emulator's `-p` histogram, mapped through the object symbol tables
  with every function label made global; `scratchpad/ra/pcprof2.py`'s
  method, worth a tool).  It said the run was 6.72 G instructions
  under stage08 against 3.42 G under clang, and that the difference
  was not the allocator: `sqlite3VdbeRecordCompareWithSkip` 413 M
  against 50 M, `sqlite3VdbeSerialGet` 247 M against 35 M,
  `sqlite3VdbeRealValue` and `fabs` where clang's build had none.
  The sorter was comparing integer keys as generic records because the
  integer column had become REAL: `typeof(5*7919)` was `real`, and the
  run's output differed from clang's, which the acceptance SQL (no
  integer product in it) had never asked.  A parser rule -- unsigned
  int with long long typed unsigned -- sent SQLite's overflow test
  through `__udivdi3` (selfhost ISSUES-95).  Fixed, with the acceptance
  SQL extended.  Found on the way, each measured and left for later:
  (a) `sqlite3VdbeExec`'s 184-case switch is a compare tree, not a jump
  table, because `case OP_OpenRead:` sits inside `OP_ReopenIdx`'s block
  and the Duff check refuses any case nested in a statement; a case in
  a plain block only needs the fall-through trampoline (hot: the tree
  costs ~25 instructions per opcode dispatched against 6);
  (b) 13.0 M `memcpy` calls against clang's 4.3 M: struct copies over
  32 bytes and constant-size `memcpy` through pointers go out of line
  (928 M instructions in the kit's memcpy);
  (c) `sqlite3GetToken` keeps two scalar locals in the frame: one
  alloca holds both (ADDI +4 off it), mem2reg refuses the step and the
  late scalar replacement does not take it either -- 116 frame reloads
  against clang's 2;
  (d) `sqlite3VdbeRecordCompareWithSkip` saves and restores 20
  callee-saved registers on every call (3.9 M calls; its prologue and
  epilogue are its two hottest runs) because values live across cold
  calls take callee-saved colours -- the allocator question for step 5
  is this one, not forwarding: save around the cold call instead;
  (e) `nVmStep`, a 64-bit counter in `sqlite3VdbeExec`'s loop, lives
  in memory (7 instructions per opcode against clang's 4 in registers).
  Also fixed tonight: a phi whose argument has no value on an edge (a
  loop-body variable at loop entry) sent the whole edge through the
  push/pop path -- 28 instructions per record compared, 115 M in this
  run.
- 2026-10-25, later: follow-up (a).  A case label inside a plain block
  no longer refuses the jump table (selfhost ISSUES-96); sqlite3VdbeExec
  dispatches through a table.  Heavy run 3.91 G -> 3.71 G, static
  281,124 -> 280,587, arm64 DBT 0.95 -> 0.87 s, output unchanged.
- 2026-10-25, later: follow-up (b).  Constant-size memcpy through
  pointers and struct copies to 64 bytes inline (selfhost ISSUES-97).
  Heavy run 3.71 G -> 3.65 G, static 280,587 -> 280,911; the arm64 DBT
  sweep over DBT_LAYOUT_PAD put both builds at 0.91-0.96 s with the
  gate running alongside -- re-time quiet, and on kagura.
- 2026-10-25, later: follow-ups (c) and (e) turned out to be one
  thing: 8-byte locals.  GetToken's `i64 i` was refused by the pair
  splitter's cap of eight hi-half access sites (selfhost ISSUES-98,
  raised to 256: heavy run unchanged to within 200 instructions, pb10
  frame reloads 54 -> 53), and then by its `i++`, whose old-value load
  is typed as the whole 8 bytes.  Typing that load as a word (the
  `hl_rmw_lo_ty` patch, kept in the session's scratchpad and
  reproduced below) promotes every i64 in sqlite3VdbeExec too, and
  under spill-everywhere that costs more than the frame: VdbeExec
  respills 54 -> 102, frame reloads 1,340 -> 1,840, heavy run 3.647 G
  -> 3.714 G, GetToken 116 -> 7.  Set aside; it is the first thing to
  re-measure once step 5 (the allocator) can spill around cold regions
  instead of everywhere.  The patch: in hl_expr's compound-assignment
  and ND_POST_INC/DEC paths, `old_val = hi_emit(HI_LOAD, n->ty, addr,
  ...)` becomes `hl_rmw_lo_ty(n->ty)`, which is TY_INT for long long
  (not S12CC_X64_HOST) and double (not S12CC_NATIVE_F64).
- 2026-10-25, end of day.  Follow-up (d) is moot as stated: clang's
  sqlite3VdbeRecordCompareWithSkip also saves 20 callee-saved registers
  (478 instructions to stage08's 475); the 413 M the function cost was
  the comparator choice, gone with ISSUES-95.  The x86-64 side of the
  rule, from Kagura (Xeon 8259CL, idle, batch-38 cc = bca7cde7): stage08
  8.61-8.68 s, clang 6.91-6.96 s, 1.25x on 1.084x the instructions
  (3,707,551,448 against 3,421,617,042, outputs byte-identical); `-p`
  says dispatch is ~40% of either build and the kit binary takes none
  of the DBT's native div/mod hooks (tools/dbt/ISSUES.md 29).  arm64
  0.87-0.92 s.  What remains of the 1.084x, by function: VdbeExec
  1,179 M against 991 M (1.19x); insertCellFast + fillInCell +
  allocateSpace 144 M that clang inlines into sqlite3BtreeInsert (ours
  65 M + 144 M against its 115 M -- single-site statics refused into a
  caller over S12CC_INLINE_HUGE = 500 nodes); sqlite3BtreeTableMoveto
  1.28x; vdbeSorterMerge 1.57x; our memcpy 261 M against clang's 644 M
  and our division helpers 98 M against its 207 M run the other way.
  Inliner budgets 16-20 are within 0.1% of each other on this run; a
  reading of 1.05 G at budget 16, taken while kit batch 39 ran
  alongside, did not reproduce -- measure with the machine quiet, the
  DBT timings said the same.
- 2026-10-25, night: step 5 begins where the dispatch loop's profile
  pointed.  VdbeExec's hot block stored four words to the frame on every
  opcode: a loop-carried variable assigned in two arms and read in two
  others is P = phi(init, M) at the header and M = phi(P, P, ..., X) at
  the join; P, live across the whole loop, is spilled, M keeps a
  register, and the back edge stores M into P's slot every iteration --
  before SSA the variable sat in its slot and the pass-through arms
  touched nothing.  `ra_phi_webs` (hir_regalloc.h) runs once colouring
  is final: for a spilled phi P, an argument phi M that does not
  interfere with P and whose every non-phi reference is none gives up
  its register and takes P's slot, so P := M and M := P are memory-to-
  memory in one slot (no-ops to the emitter) and only the assigning
  edges store.  Three things the first versions got wrong, each caught
  by the heavy run's output: (1) the emitter's "slot written and read on
  one edge" hazard compared values, not slots -- compares slots now;
  (2) the reference walk used hi_ref and missed a compare fused into a
  BRC (a NOP whose operands the branch re-reads) -- it uses lv_uses,
  the liveness builder's own enumeration; (3) two members of one web
  were not checked against each other -- M1 live while M2's assigning
  edge stored into the slot (sqlite3WhereCodeOneLoopStart, the
  index-range aggregate came out wrong; that query shape is in
  acceptance.sql now, `indexed by` so the planner takes the index at
  300 rows).  Numbers: 153 webs in SQLite, 12 in VdbeExec; VdbeExec
  frame reloads 1,340 -> 802, stores -> 324; heavy run 3.647 -> 3.565 G
  (-2.3%), static 280,891 -> 280,559; arm64 DBT 0.85-0.86 -> 0.84 s,
  quiet machine, interleaved.  test_phi_web: 33 loop-carried variables
  and one assigned in a single arm; the harness requires a PHIWEB line.
