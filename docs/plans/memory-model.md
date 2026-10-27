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
- 2026-10-25, night: the RMW word-typed load re-measured on top of the
  phi webs, as the plan said to.  Still worse: VdbeExec respills 54 ->
  102, frame reloads 802 -> 1,293, stores 324 -> 735, heavy run 3.565 G
  -> 3.609 G (+1.2%); GetToken 116 -> 7 as before.  The webs took the
  pass-through stores out, but promoting every i64 in a 10,000-
  instruction function still puts more live values on the table than
  spill-everywhere can hold.  Set aside again; the next allocator piece
  is the spill *placement* -- a value defined in the hot loop head and
  used in a cold arm should be stored where it is used, not at every
  definition -- and that is what will let this land.

## Step 5b: spill placement (designed 2026-10-25 night; 1 and 2 landed 2026-10-26, 3 and 4 built and opt-in; 4 found two liveness fixes that landed -- see the log below)

What the profile says after the webs, in sqlite3VdbeExec's 146 M frame
instructions per 200,000-row run: 38% is `nVmStep`, a `u64` counter in
memory (ld, ld, add, sltu, add, st, st per opcode); the rest is spread
over 70 slots, none above 7 M.  The counter is in memory because the
pair splitter refuses its `++` (ISSUES-98's set-aside half); promoting
it with the RMW patch promotes every case-local `i64` with it, and the
allocator then spills worse: 102 victims against 54, and the new ones
are not the transients -- they are `addi`/`add` values defined in the
header and used in 140 arms (reloaded 140 times), header phis with
hundreds of uses, and `load` #16 from the entry block used in nine
arms.  Spill-everywhere makes a transient pressure peak in one arm
cost a long-lived value its register in every arm.

The piece, in order of return for effort, each measured on the heavy
run before the next:

1. **Rematerialize address arithmetic.**  `hi_inst_remat` covers
   constants and global addresses.  An `addi`/`add` of a register-
   resident value is one instruction to recompute, the same as a
   reload, and needs no store and no slot: when such a value is a
   respill victim, mark it remat (recompute at each use) instead of
   mem_forced, provided its operand is live at every use (it is, when
   the operand is a header value).  VdbeExec's #163/#166 (142 and 143
   uses) are this shape.
2. **Reload once per block.**  A mem_forced value used several times in
   one block reloads at each use; one reload at the first use into a
   caller-saved register, kept to the last use, needs the emitter to
   know the register is free -- the allocator must own the reload as a
   value.  Mechanism: for each victim, per block with uses, a new
   `LOAD slot` instruction at the first use whose users are that
   block's uses; a short node for the next colouring round (if it
   fails, it falls back to mem_forced at use).  Static same-block
   repeats were 6% of all reloads; in VdbeExec the arms are longer.
3. **Store at the use region, not the definition**, for a value defined
   in a hot block and used in cold arms: the per-iteration store is the
   last of the hot-block frame traffic.  Equivalent to keeping the
   value in a register through the hot block and spilling it on entry
   to the arms that use it -- live-range splitting at the arm entry,
   with the register freed inside the arm.  Needs 2's reload values
   plus a store placed on the arm's entry edge; the webs' slot-sharing
   argument (one slot, stores only where the variable changes) is the
   model.

Then re-measure the RMW patch; it lands when 1-3 hold the line on
VdbeExec.  Rule as always: heavy-run instructions, frame traffic and
the hottest blocks first, then DBT time on both machines.

### Log

- 2026-10-25, night: the x86-64 starting line, from Kagura (Xeon
  8259CL, idle, three runs, batch-40 cc): 8.54-8.58 s on heavy.sql,
  outputs byte-identical to clang's; batch 38 was 8.61-8.68, clang
  6.91-6.96.
- 2026-10-25, night: **5b.1 landed.**  `ra_remat_op` (hir.h): a respill
  victim that is an ADDI with an i12 immediate of a register-allocated
  value is recomputed at each use instead of stored and reloaded --
  no node, no slot, nothing at the definition; `hcg_into` emits `addi
  reg, src, imm` at the use (or the use folds it into a displacement
  and emits nothing).  The liveness builder reads the operand where the
  victim was read (`lv_remat_src` in `lv_uses` and the phi-argument
  walk), and the fixpoint is re-solved from the old sets after each
  round that made remat victims (`lv_solve`, factored out of
  lv_prepare; a non-convergence sends the round's victims to memory
  instead).  The operand's interval end is widened to the victim's for
  the slot sharing, should a later round spill the operand.  Exact
  liveness only; pairs stay in memory.  One wrong version: the flag was
  cleared with the allocator's state, after liveness had already read
  it, so a function inherited the previous function's flags
  (test_respill's blam read through round16's ADDI and faulted); it is
  cleared in hir_reset now.  Numbers: SQLite 222 remat victims (85
  recomputed, the rest fold), static frame reloads 5,556 -> 5,524,
  stores 1,017 -> 818; VdbeExec 802 -> 800 and 324 -> 316 (its 54
  victims: 18 phis, 15 constants, 8 ADDIs, 5 loads, 3 calls, 5 other);
  heavy run 3,564,780,038 -> 3,561,562,038 (-0.09%), output identical;
  arm64 DBT 0.95-0.99 -> 0.97-0.98 s, unchanged.  Small on its own, as
  the design said: the shape it is for is the one the RMW patch
  creates.  Gates: run-tests 142/142 with the fixed point, SQLite
  acceptance identical, libc 27/0, stage07 5/5, FP 288 bit-identical.
- 2026-10-26: **5b.2 landed** -- one reload per block.  Once the first
  colouring has settled, every spilled scalar gets, per block and per
  run of reads with no call between them, a COPY of itself hung before
  its first reader (the pre-instruction chain the call splitter uses,
  `ra_insert_reloads`), and the run's reads name the COPY; a second
  colouring places those (short, never across a call, so r3-r10), and
  a COPY it cannot place is deleted by the respill (reads back to the
  slot, `ra_delete_reload`), never spilled.  The emitter loads a COPY
  of a spilled value straight into its register (one ldw, not ldw+addi
  -- call-split copies of spilled values gained the same).  Two kinds
  of read are served: operand slots, and the base a load's or store's
  address chain folds down to (`rl_fold_sub`: the emitter's two fold
  sites take the base from it; `ra_codegen_fold_base` reports it to
  liveness).  Reads through an ADDI or COPY operand are left alone --
  the emitter's folds walk through those from other blocks, and a
  reload must stay block-local; 239 of the remaining 541 same-block
  repeat reloads in SQLite are materialized address ADDIs of this
  shape, the next piece's material.  Also: `ra_build_wuses` now counts
  the fold-base reads (a reload serving only such reads had weight 0
  and was always the cheapest victim); that alone moved spill choices
  across SQLite -- static reloads 5,524 -> 4,662 but the heavy run
  +0.12% -- and with the reloads on top: reloads 4,501 (-18.5%),
  stores 818 -> 810, VdbeExec reloads 800 -> 679, same-block repeats
  1,086 -> 541, heavy run 3,561,562,038 -> 3,561,020,599 (-0.015%),
  output identical, arm64 DBT 0.86-0.87 s both.  Two wrong versions on
  the way, both caught by the heavy run: the fold table was reset after
  liveness had read it (strcmp's old pointer looked dead at its step;
  a stale entry named instruction 0 as a base), and an access with a
  folding address chain was served twice -- its ADDI operand AND the
  chain's base -- so the emitter, folding from the reload COPY of the
  ADDI, dropped the displacement (sqlite3Insert read p->f28 at p+0;
  found by splicing the 36 differing functions between the good and
  bad builds, scratchpad `splice.py` now carries jump tables).  The
  address operand of a folding access is not a read.  Test
  test_blk_reload (gather: 58 -> 29 frame reloads; the harness
  requires a RELOAD line).  Gates as above, host frontend OK.
  Verdict: a static win, dynamically neutral on this workload -- the
  hot blocks of VdbeExec do not re-read spilled values within a block;
  the machinery (reload values the allocator owns) is what piece 3
  needs.
- 2026-10-26: the RMW patch re-measured on 5b.1+5b.2: heavy run
  3,561,020,599 -> 3,599,919,103 (+1.1%), all of it sqlite3VdbeExec,
  all of it the dispatch block: nVmStep's two words become a spilled
  pair and the block goes from 10 instructions to 13, plus 3 in the
  latch (the lo word reloaded, incremented, reloaded again for the
  carry compare, the hi word stored; the latch recomputes lo+1 and
  stores it, and reloads hi).  12.82 M opcodes x 3 = the 38.9 M.
- 2026-10-26: **5b.3 built, measured, parked (opt-in `HIR_REGION=1`).**
  Regional spill (`ra_region_split`): when a node fails to colour and
  the victim is live across the region the failure is in, the victim
  goes to memory in that region only -- a COPY at the region entry (the
  top-of-block list), reads inside the region and phi arguments on its
  exit edges name the copy (the phi copy reloads), and the value keeps
  its register elsewhere.  Region = the smallest dominator subtree
  holding the failing node's live range; only inside loops, never a
  loop header, same loop as the definition, one split per value per
  respill round, the value not live into any exit target except as a
  phi argument; liveness and everything downstream rebuilt after a
  round that split.  Correct (heavy run output identical in every
  configuration, five differentials green) but it does not pay:
  alone, SQLite static reloads 4,501 -> 4,519, stores 810 -> 876, 54
  regions (11 in VdbeExec), heavy run 3,560,324,456 (-0.02%); with the
  RMW patch on top, 3,689,777,782 against 3,599,919,103 without it --
  VdbeExec's respill ran 57 rounds instead of 23 (161 victims, 57
  pinned constants unpinned, 53 phis) and its frame stores went 698 ->
  1,326.  And the value it was built for is not reachable by this
  shape: nVmStep's new lo word is defined in the header and live
  THROUGH every arm into the join (its only use after the arms is the
  back-edge phi copy), so every arm's exit fails the "not live into the
  exit target" test.  Serving it needs the reload at the region exit
  as a value -- a phi at the join (and, for a loop-invariant, a header
  phi) with the region copy as its argument from the region's exits,
  and the uses after the join renamed -- the webs' P/M shape built on
  demand.  That is the next design, not a tuning of this one.  The
  first version (region = dominator child of the DEFINING block, any
  depth) split 1,889 values, most in straight-line code or the whole
  switch dispatch, and tripled the stores.
  Two wrong-code defects found on the way, both older than this piece
  and both landed: (1) a rematerialized ADDI (5b.1) as a parallel-copy
  source -- a phi argument on an edge, a call argument -- reads its
  base's register AT THE COPY, but the phi-copy fast path and the
  argument marshaller classed it "constant-like" and materialized it
  whenever convenient: test_phi_web's loop-entry copies wrote `step :=
  0` into the register still holding `seed` before `h := seed + 7` read
  it.  `hcg_remat_reg_src`: such a source is sequenced as a move from
  the base's register and emitted as `addi dst, base, imm`; a remat
  ADDI of a spilled base counts the base's slot in the slot-hazard
  check; and the respill no longer remats the operand of a remat
  victim (one immediate, not a chain).  Live in kit batches 41 and 42.
  (2) The allocator's per-value state (ra_mem_forced, ra_web_of, ...)
  is reset for [0, h_ninst) at the start of hir_regalloc; a copy
  appended later (5b.2's block reloads too) inherited the previous
  function's entry -- a stale ra_web_of handed test_phi_web's region
  copy another value's slot.  ra_new_split_copy clears them.  Also:
  the -d dump carries phi arguments and a block table (dominator,
  depth, successors, top-of-block list), and `HIR_REGION_FN`/`_SKIP`/
  `_MAX` bisect region candidates within one function.  The harness
  compiles test_phi_web a second time with HIR_REGION=1 and requires a
  REGION line and the right answer.  Default build: SQLite static
  reloads 4,501 -> 4,503, stores 810 -> 824 (the remat-chain rule),
  heavy run 3,561,020,599 -> 3,560,217,280, output identical.
- 2026-10-26: Kagura on batch 43 (`50b7a3b7`): comparison 1033/0,
  heavy.sql 3,560,240,662 under slow32-fast (the usual 23k offset from
  the shell objects), output byte-identical to clang's; idle three-run
  x86-64 DBT 8.48-8.55 s (batch 40 8.54-8.58, clang 6.91-6.96).  This is
  the baseline the join-phi design is measured against.
- 2026-10-26: **5b.4, the join form, built; it found two allocator
  bugs that landed, and stays opt-in itself.**  `ra_region_split` no
  longer refuses a value live out of the region: the variable has two
  definitions (v at d, the copy m at e) and gets a phi at every block
  of e's iterated dominance frontier where v is live in (`ssa_df`, built
  with the SSA and valid since nothing after it alters the CFG), with m
  as the argument from the region's exits and the reaching definition
  (nearest join, m, or v up the dominator tree, `ra_region_reach`) from
  everywhere else; every read of v is renamed to what reaches it.  A
  region inside a loop whose value comes from before it gets its join
  at the loop header -- the header phi the design asked for -- and the
  loop's reads name that.  test_join_split (a step counter stepped at
  the loop top and read after the loop, a table pointer from before it,
  two arms holding eight temporaries across an indirect call) shows
  both shapes, the joins coalesced into the value's register.  Rules
  added on the way: a value that is the operand of a rematerialized
  ADDI is not split and an ADDI of a split value is not rematerialized
  (the recompute reads the operand's register at the ADDI's users, past
  the joins); a failing node live into e, or a phi at e, is not served
  (the copy at e's top reads v while it is live, and the next round
  would split v there again -- wherePathSolver split one ADDI four
  times, each copy a copy of the last); the region must not run on
  every iteration of its loop (`ra_region_every_iter`); and the victim
  must hold a colour this round, so the joins can inherit it.
  The first SQLite build with it answered the join query "no query
  solution": wherePathSolver's `mxChoice`, a phi whose only use in the
  loop is a compare fused into a BRC, read back 0.  Function splice,
  then per-candidate `HIR_REGION_MAX`, then a `-t -r` trace of a
  300-row reproducer replayed against the asm listing (scratchpad
  calls.py/norm.py: calls, branches and memory accesses by value, the
  two builds diffed) found the slot: **ra_compute_ends saw nothing of a
  compare already NOPed by ra_extend_fused_cmp** when intervals were
  recomputed after appended copies (call splits, block reloads, region
  copies), so the operands got no cross-block extension and the phi
  looked dead from the branch to the latch, where the region copy's
  store shared its slot.  Older than this step (5b.2's recompute had it;
  test_phi_web under HIR_LINEAR_LIVE=1 failed on it, and passes now);
  fixed by reading the fused compare's operands in ra_compute_ends,
  both passes.  Second find, from a REGION-LEAK diagnostic for the
  repeated splits: **lv_uses listed the address operand of a folding
  load or store as a read** although the emitter folds through it to
  the chain's base and never touches its register; an address ADDI
  therefore stayed live through every access that folded past it.
  Fixed -- a folding access reads only its base, as 5b.2 already said
  for the reload rewiring -- and that alone, regions off, is the
  step's result: SQLite static frame reloads 4,503 -> 3,013, stores 824
  -> 516, callee-save traffic 20,401 -> 18,051, heavy run
  3,560,217,280 -> 3,471,233,866 (-2.5%), output identical; gates:
  run-tests 147/147 with the fixed point, SQLite acceptance, libc 27/0,
  stage07, host frontend, FP 288 bit-identical.
  The join form itself, on top of that: 209 regions / 559 joins,
  3,510,626,177 (+1.1%); with the colour rule 160 / 524,
  3,555,723,405 (+2.4%).  All of the loss is sqlite3VdbeExec (the
  per-function profile has everything else within noise): a join at the
  dispatch loop's merge blocks -- where every loop-carried value is
  live -- is spilled, and then the value's reads past the join reload
  where v had a register, and the latch pays memory-to-memory copies
  into the header phis the webs used to tie (5 frame instructions per
  opcode).  Opt-in (`HIR_REGION=1`, `HIR_REGION_ANY=1` lifts the
  every-iteration and colour rules, `HIR_REGION_SAME=1` the parked
  form's same-depth rule) until a cost model says where a join will
  keep a register.  arm64 DBT on heavy.sql, quiet, interleaved three
  runs: kit batch 43 0.87-0.88 s, this default 0.82-0.83 s.
- 2026-10-26: the RMW patch re-measured on the new default (fold-aware
  liveness): 3,471,233,866 -> 3,669,514,044 (+5.7%), regions on
  3,607,573,905 (+3.9%, 130 regions / 391 joins -- the join form does
  reach the shape it was built for, 62 M of the 198 M back, and not the
  rest).  Still set aside.  Without the patch the loop counter stays a
  memory pair at 7 instructions per opcode; with it, the promoted pair
  and the other case-local i64s cost more in the arms than the
  dispatch block saves, until the joins at the dispatch loop's merge
  blocks can keep a register.
- 2026-10-26: Kagura on batch 44 (`9ca39c80`): comparison 1033/0,
  heavy.sql output byte-identical to clang's, slow32-fast count
  3,471,242,483 (-2.50% from batch 43's 3,560,240,662 there; 8,617
  above the Mac's 3,471,233,866 where the offset was 23,382 at batch 43
  -- so part of the offset is compiled code the two builds do not
  share: the Mac's runs reuse the shell and VFS objects built by the
  tree's compiler at gate time and the tree's libc archive, kagura
  rebuilds all of it with the kit); idle three-run x86-64 DBT 8.35,
  8.35, 8.51 s (batch 43 8.48-8.55, clang 6.91-6.96).  Baseline for
  step 5b.5.

## Step 5b.5: the join cost model (designed and measured 2026-10-26; does not pay, opt-in; found the same-block web bug)

What the colour-rule build (`HIR_REGION=1`, the 5b.4 rules) says,
from its `-d` dump of SQLite: 162 joins, of which 111 took their
value's register, 34 another register (a move on each pass-through
edge) and 17 were spilled -- nearly all of the 17 where the value
itself was spilled in a later round.  So the joins keep a register:
the colour rule and the phi-move bias answer that question, no pin is
needed.  The loss (+84 M, all of it sqlite3VdbeExec) is the split
itself.  Its 43 splits land on eight entry blocks -- 13 on one arm, 9
and 9 on two others -- and 36 of the 43 victims are header phis.  The
arm with 13 is entered 1.29 M times in 12.8 M opcodes, one opcode in
ten, where the static view (one target of a 186-way jump table) says
one in 186.  Each split stores at the entry and reloads at every exit
the value is live into, whether or not the arm reads the value; the
global spill the respill would otherwise have chosen pays only where
the value is read, and these victims were the cheapest precisely
because they are read little.  **A region split pays for liveness; a
global spill pays for uses.**  Thirteen values stored and reloaded on
one opcode in ten is the 84 M.

The model.  One static block frequency f, and per failing node the
cheapest of three actions over all its neighbours -- not, as now, the
cheapest neighbour by weighted uses and then whatever action it
admits:

- global spill  G(v) = f(def) + Σ f(B) over the blocks B that read v
  (one reload per block, 5b.2's rule).  For a phi the "store at the
  definition" is the back-edge copy, which the webs often make a
  no-op: count f(def)/2.
- region split  S(v, R) = f(e) + Σ f(x) over the exit edges x -> t with
  v live into t + Σ f(B) over the blocks in R that read v.  (A move on
  each pass-through edge when the join does not coalesce -- 34 of 162
  -- is not known at decision time; left out, to be checked against
  the latch afterwards.)
- rematerialize  M(v) = Σ f(B) over the blocks that read v, for an
  ADDI that qualifies: no store, no slot.

The frequency: forward over the forward edges in reverse postorder,
entry 1; a BRC gives each successor half, except that an edge leaving
a loop gets 1/L; a loop header multiplies by L (8) per depth; a
jump table gives EACH target half -- pessimistic by design, because
any one arm may dominate and VdbeExec's does, and a profile is what we
do not have (slow32 -p could feed one later).  With that, an arm's
split costs about f(dispatch), and the split is chosen only when the
value is read on the hot path at least that often.  Checked against
the shapes at hand: the promoted loop counter `lo' = lo + 1` under the
RMW patch, G = 1 (store at the def) + 2 (the header's carry compare,
the back-edge copy) = 3 against S = 0.5 + 0.5 = 1 -> split, which is
the point of the step; a loop-invariant read in two arms, G = 0 + 1
against S = 1 -> no split; a header phi read only inside the arm, G =
0.5 + 0.5 against S = 1 + 0.5 -> no split; the 13-split arm, whose
victims are read little, keeps at most the one or two that are read
in the dispatch block.

f also replaces the depth weights in `ra_build_wuses` (today a read in
a cold arm weighs as much as one in the dispatch block, 10^depth
either way), which changes every respill decision on its own.  So the
order of work: (1) f and the weights, regions off, measured alone
(`HIR_RA_FREQ=1`, then default if it holds); (2) the G/S/M choice in
gc_respill with regions on, measured; the targets are the 13-split
arm receiving 0-2 splits, the heavy run at or under 3,471 M with
regions on, and then the RMW patch: the dispatch block keeps `lo'` in
a register and only the cold arms store it.  (3) If the latch shows
pass-through moves, add their term to S.  Rules as always: heavy-run
instructions and the hottest blocks first, then DBT time on both
machines, five differentials green, the machine quiet.

### Log

- 2026-10-26: **built and measured; it does not pay, and it found a
  wrong-code bug that landed.**  `ra_build_bfreq` (the frequency, with
  the share rules selectable: `HIR_RA_FREQ=1` even shares, `=2` a
  jump table half per target, `=3` that and whole branches, `=4` even
  table and whole branches, `=5` the depth weights themselves),
  `ra_region_plan`/`ra_region_commit` (the split's checks apart from
  its mutation), `ra_region_cost` (S), and in gc_respill the cheapest
  action over the four cheapest neighbours when the frequency is on
  with `HIR_REGION=1`.  Heavy run, output identical in every case,
  default 3,471 M:

  | weighting | regions off | regions on (regions/joins) |
  |---|---|---|
  | 1 even shares | 3,561 M (+2.6%) | 3,527 M |
  | 2 table halves | 3,512 M (+1.2%) | 3,528 M (151/426) |
  | 3 table halves, whole branches | 3,552 M (+2.3%) | 3,482 M (+0.3%, 42/75) |
  | 4 even table, whole branches | 3,568 M (+2.8%) | -- |
  | 5 depth weights, S/G in depth units | = default | 3,530 M (111/345) |

  Every frequency variant chooses respill victims worse than the depth
  weights: an even jump-table share makes an arm-only value look free
  to spill and VdbeExec's hot arms pay; halving the branches
  undercounts reads deep in a loop body.  The depth weights cannot
  reject a split (an arm weighs as the dispatch block, so S looks
  cheap: mode 5 still put 15 splits on the busiest arm), and the
  frequency that rejects best (3) chooses victims worst.  The model's
  other error, visible in the mode-5 splits: a header phi's G counts a
  store at the definition and ten per read, where the webs make the
  store free and the reads sit in arms entered one time in 186 -- G
  is overstated for exactly the victims that should be spilled
  globally.  What would settle it is a profile, which this platform
  does not have (slow32 -p could feed one; that is a different step).
  Default unchanged, the machinery opt-in.
  The bug: modes 3 and 4 produced a SQLite that failed its first
  statement.  Splice to wherePathSolver, a three-row reproducer, the
  trace replay (regb/calls.py, addresses normalized for the data
  segment's shift): two different pointers stored into one frame slot
  back to back on the loop-entry edge.  **ra_phi_webs webbed two phis of
  the same block** -- the aFrom/aTo swap pair, both spilled under those
  weights -- because its interference test asks whether each is live
  into the other's block, and a phi is never live into its own: both
  took one slot and the entry copies stored both into it.  Latent
  since step 5 (any two spilled phis of one block with the right use
  shape); a phi of p's own block is refused now, for the argument and
  for web members.  The harness runs test_phi_web under HIR_RA_FREQ=3
  as well.  SQLite's default code is byte-identical with the fix.
- 2026-10-26: Kagura on batch 45 (`fb3dea7b`): comparison 1033/0; every
  kit tool rebuilds byte-identical to batch 44, and so does its SQLite
  build -- the default code is unchanged, as claimed, so no count or
  timing was re-run.
- 2026-10-27: the Fortran re-sync (fortran/src's copied backend brought
  level with stage08) found a step-5b.1 cost on its first measurement:
  LINPACK's DGEFA got 806,400 instructions slower.  A rematerialized
  ADDI (`LDA+1`) was chosen while its operand had a register; a later
  respill round put `LDA` in memory, and every use became a reload and
  an add where the ADDI's own slot is one load.  Now a value sent to
  memory takes any rematerialized ADDI of it along (stored once at its
  definition).  LINPACK 509,008,169 -> 508,104,169 (0.01% under the
  pre-re-sync compiler), mandel unchanged, SQLite heavy 3,471,219,082
  -> 3,471,219,117 (+35), output identical.
- 2026-10-27: Kagura on batch 46 (`694d26f2`): comparison 1033/0;
  heavy.sql 3,471,242,518, exactly the Mac's +35 over batch 45
  (3,471,242,483 there), output byte-identical to clang's.

## Step 5b.6: profile-guided allocation (built and measured 2026-10-27; opt-in)

The question 5b.5 left: with real block frequencies instead of static
guesses, do the regional splits and the RMW patch pay?  Built: block
labels behind `HIR_PGO_LABELS=1` (`__pgo_<function>_<block>` at every
emitted block, no code changed -- SQLite's labelled build is the
default one plus 45,576 labels), `selfhost/stage08/pgo-profile.sh`
(link map + `slow32 -p` histogram -> `function block count`), and
`HIR_PGO=<file>` (`ra_pgo_fill`: ra_bfreq from the counts, normalized
to the function's hottest block = 4096, unexecuted blocks 1, functions
the training run never entered keep the defaults; mode 6 of
ra_freq_on).  Trained on heavy.sql (12,208 blocks ran, 740 functions);
measured on heavy.sql and on a second workload it never saw
(scratchpad other.sql: 60,000-row ledger, LIKE, upper(), DELETE,
group_concat DISTINCT, a self join).  Outputs identical throughout.

| build | heavy | other |
|---|---|---|
| default | 3,471,217,446 | 2,115,739,729 |
| profile, cost as before | +0.02% | +0.02% |
| profile, cost as before, regions | +2.9% | +2.3% |
| profile, cost = weighted uses | **-0.19%** | **-0.13%** |
| that, with regions | -0.07% | +0.31% |
| RMW patch alone | +6.0% | +3.9% |
| RMW, its own profile | +7.0% | +5.0% |
| RMW, its own profile, regions | +2.8% | +1.7% |

What it found: the respill's cost divides weighted uses by the
failing neighbours and by the lifetime.  With depth weights every value
in a loop weighs about the same and the divisors only break ties; with
measured counts (a factor of 4,096 apart) they made the hottest,
longest-lived values look cheapest -- the regional build sent
sqlite3VdbeExec's opcode pointer to memory, along with join phis
carrying ~625,000 of weight.  In profile mode the cost is now the
weighted uses alone (`HIR_PGO_COST=1` keeps the neighbour divisor, =3
both, for measuring).  That is the -0.19%, and it carries over to the
workload the profile never saw, so it is not training on the test.

Verdict: measured frequencies are worth a little, and do not rescue
the regional splits or the RMW patch.  The RMW patch's cost is not
where the allocator puts values: promoting every case-local i64 in a
10,000-instruction function puts more on the table than 19 callee-
saved registers hold, whatever the weights.  Landed opt-in; nothing
changes by default.  The harness compiles test_phi_web with a profile
made by this pipeline (tests/test_phi_web.pgo) and requires `PGO
fn=run`.  A profile fits builds that allocate differently, not ones
that lower differently: the RMW build numbers 46 blocks differently and
needed its own.
