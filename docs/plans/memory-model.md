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

2. **Memory state across blocks.**  Forwarding and load-load
   elimination work on one block's "last known value" table today.
   Carry the table across blocks as a dataflow: what is known at a
   block's entry is what every predecessor knows at its exit, meeting
   by intersection; a back edge starts empty.  This makes `x = *p; if
   (c) ...; y = *p` one load without a store in between, and it is the
   foundation for step 3.

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

## Log

- 2026-10-25: step 1.
