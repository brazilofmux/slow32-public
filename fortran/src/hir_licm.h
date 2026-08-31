/* COPIED from selfhost/src/hir_licm.h at 849dd791.
 * fortran/ is self-contained by ruling: selfhost must be free to evolve
 * without breaking f77.  Do NOT symlink this back.  Re-sync deliberately,
 * and record the new vintage here.
 */
/* hir_licm.h -- Loop-Invariant Code Motion for s12cc
 *
 * Hoists pure computations out of loops.  Runs after hir_opt.h,
 * before hir_regalloc.h.  Requires dominator tree from hir_ssa.h.
 *
 * Design: Clone invariant instructions at h_ninst (appended), NOP
 * the originals in the loop body, rewrite all references globally.
 * Hoisted clones are linked into per-block lists (licm_head/licm_next)
 * and emitted by codegen/regalloc before the block's regular instructions.
 *
 * What gets hoisted: pure non-faulting arithmetic (ADD..SGEU excluding
 * DIV/REM), NEG, NOT, BNOT, ADDI, COPY.  An instruction is invariant
 * if all operands are defined outside the loop, rematerializable, or
 * themselves loop-invariant (iterative marking to fixpoint).
 *
 * Where: to ssa_idom[loop_header] (the preheader).  Safe for pure ops.
 */

/* --- Per-block hoisted instruction lists (append order: defs before uses) --- */
static int licm_head[HIR_MAX_BLOCK];  /* first hoisted inst, -1 = none */
static int licm_tail[HIR_MAX_BLOCK];  /* last hoisted inst, for O(1) append */
static int licm_next[HIR_MAX_INST];   /* next hoisted inst, -1 = end */

/* --- Old-to-clone mapping --- */
static int licm_map[HIR_MAX_INST];    /* old index -> clone index, -1 = not hoisted */

/* Blocks belonging to ANY natural loop in this function — persists
 * after hir_licm (ssa_vis is per-loop scratch).  Consumed by
 * hcg_mark_loop_consts. */
static int licm_in_any_loop[HIR_MAX_BLOCK];
/* DIVERGENCE (f77, port upstream): nesting depth per block -- each
 * natural-loop body found below increments its blocks, so an inner
 * loop's blocks count every enclosing loop.  Consumed by the spill
 * cost in hir_regalloc: a value used in an inner loop is far more
 * expensive to spill than its static use count says. */
static int licm_depth[HIR_MAX_BLOCK];

/* --- Stats --- */
static int licm_stat_hoisted;

/* --- Live-range splitting around loops (DIVERGENCE f77, port
 * upstream candidate) ---------------------------------------------
 *
 * A value LIVE THROUGH a loop but unused inside it still occupies a
 * register for the whole loop under linear-interval liveness -- and
 * under pressure it is exactly the register an inner-loop phi
 * needed.  Split it: store it to a fresh frame slot in the
 * preheader, end its register life there, and reload it at the top
 * of each post-loop block that uses it.  Cost: one store per loop
 * entry plus one load per use block; benefit: a callee-saved
 * register freed for the entire loop.
 *
 * split_head[b] is a TOP-of-block list (chained through licm_next,
 * memberships are disjoint from licm_head's): unlike LICM's hoists,
 * which run after a block's body, reloads must run before the uses
 * in their block.  Consumed by ra_order, hcg_block and the layout
 * size estimate; a block carrying reloads must never be forwarded.
 *
 * Safety: candidates are defined in a block dominating the header,
 * have NO use in the body, at least one use in a block dominated by
 * the preheader, and no use anywhere else (a use neither clearly
 * before nor clearly after the loop disqualifies).  CALL/CALLHI
 * values are excluded (their result-pair linkage is positional). */
static int split_head[HIR_MAX_BLOCK];
static int split_frame;
static int licm_stat_split;
static char sp_inbody[HIR_MAX_INST];
static char sp_post[HIR_MAX_INST];
static char sp_bad[HIR_MAX_INST];

/* --- Scratch for loop body --- */
#define LICM_MAX_BODY 2048
static int licm_body[LICM_MAX_BODY];

/* --- Helpers --- */

/* Does block a dominate block b?  Walk ssa_idom[] chain from b. */
static int licm_dominates(int a, int b) {
    int depth;
    depth = 0;
    while (b >= 0 && depth < HIR_MAX_BLOCK) {
        if (b == a) return 1;
        if (b == ssa_idom[b]) return 0;  /* root */
        b = ssa_idom[b];
        depth = depth + 1;
    }
    return 0;
}

/* Is instruction defined inside the loop body? */
static int licm_in_loop(int inst) {
    if (inst < 0) return 0;
    if (h_blk[inst] < 0 || h_blk[inst] >= bb_nblk) return 0;
    return ssa_vis[h_blk[inst]];
}

/* Is an operand "loop-invariant"?  True if:
 * - negative (no operand)
 * - defined outside the loop
 * - rematerializable (ICONST, ALLOCA, etc.)
 * - already marked as invariant (ho_use[inst] == 1) */
static int licm_operand_ok(int inst) {
    if (inst < 0) return 1;
    if (!licm_in_loop(inst)) return 1;
    if (hi_is_remat(h_kind[inst])) return 1;
    if (ho_use[inst]) return 1;
    return 0;
}

/* ----------------------------------------------------------------
 * Find natural loops and hoist invariants
 * ---------------------------------------------------------------- */

/* Compute loop body via backward BFS from latch to header.
 * Returns body count in licm_body[]. Sets ssa_vis[] for body blocks. */
static int licm_find_body(int header, int latch) {
    int n;
    int wh;
    int wt;
    int b;
    int j;
    int p;

    /* Clear vis */
    b = 0;
    while (b < bb_nblk) {
        ssa_vis[b] = 0;
        b = b + 1;
    }

    /* Header is always in the loop */
    ssa_vis[header] = 1;
    licm_body[0] = header;
    n = 1;

    /* If latch != header, seed BFS with latch */
    if (latch != header) {
        ssa_vis[latch] = 1;
        if (n < LICM_MAX_BODY) {
            licm_body[n] = latch;
            n = n + 1;
        }
    }

    /* BFS backwards from latch through predecessors.
     * Start from latch (index 1), not header (index 0) — going backwards
     * from header would escape the loop.
     * For single-block loops (latch == header), body = {header}, no BFS. */
    if (latch == header) return n;
    wh = 1;
    wt = n;

    while (wh < wt) {
        b = licm_body[wh];
        wh = wh + 1;

        j = 0;
        while (j < ssa_npred[b]) {
            p = ssa_pred[ssa_pbase[b] + j];
            if (p >= 0 && p < bb_nblk && !ssa_vis[p]) {
                ssa_vis[p] = 1;
                if (wt < LICM_MAX_BODY) {
                    licm_body[wt] = p;
                    wt = wt + 1;
                }
            }
            j = j + 1;
        }
    }

    return wt;
}

/* Mark loop-invariant instructions.  Iterates to fixpoint.
 * Uses ho_use[] as the invariant flag (0=not invariant, 1=invariant).
 * ssa_vis[] must be set for loop body blocks. */
static void licm_mark(int body_count) {
    int changed;
    int bi;
    int b;
    int i;
    int k;

    /* Clear marking for all instructions in loop body */
    bi = 0;
    while (bi < body_count) {
        b = licm_body[bi];
        i = bb_start[b];
        while (i < bb_end[b]) {
            ho_use[i] = 0;
            i = i + 1;
        }
        bi = bi + 1;
    }

    /* Iterate to fixpoint */
    changed = 1;
    while (changed) {
        changed = 0;

        bi = 0;
        while (bi < body_count) {
            b = licm_body[bi];
            i = bb_start[b];
            while (i < bb_end[b]) {
                k = h_kind[i];
                /* DIVERGENCE (f77): a LOAD carrying the frontend's
                 * read-only assertion (h_ld_ro) hoists like a pure
                 * instruction once its address is invariant.  The
                 * speculation is safe: the address is a dummy
                 * argument the caller already dereferenced. */
                if (!ho_use[i] &&
                    (hi_is_pure(k) || (k == HI_LOAD && h_ld_ro[i]))) {
                    /* Check all operands */
                    if (licm_operand_ok(h_src1[i]) &&
                        (ho_src2_is_ref(k) ? licm_operand_ok(h_src2[i]) : 1)) {
                        ho_use[i] = 1;
                        changed = 1;
                    }
                }
                i = i + 1;
            }
            bi = bi + 1;
        }
    }
}

/* Clone an instruction at h_ninst.  Returns clone index. */
static int licm_clone(int orig) {
    int cl;
    int s1;
    int s2;

    cl = h_ninst;
    if (cl >= HIR_MAX_INST) return -1;

    h_kind[cl] = h_kind[orig];
    h_ty[cl] = h_ty[orig];

    /* Remap operands through licm_map (already-hoisted deps) */
    s1 = h_src1[orig];
    if (s1 >= 0 && licm_map[s1] >= 0) s1 = licm_map[s1];
    h_src1[cl] = s1;

    s2 = h_src2[orig];
    if (s2 >= 0 && ho_src2_is_ref(h_kind[orig]) && licm_map[s2] >= 0) {
        s2 = licm_map[s2];
    }
    h_src2[cl] = s2;

    h_val[cl] = h_val[orig];
    h_name[cl] = h_name[orig];
    h_ld_ro[cl] = h_ld_ro[orig];   /* direct writes bypass hi_emit's clear */
    h_cbase[cl] = -1;
    h_pbase[cl] = -1;
    h_pcnt[cl] = 0;

    /* Clones are never themselves hoisted; mark so rewrite skips them */
    licm_map[cl] = -1;

    h_ninst = h_ninst + 1;

    return cl;
}

/* Hoist invariants for one loop.  Body is in licm_body[], count is body_count.
 * ssa_vis[] set for body blocks.  ho_use[] marks invariants. */
static void licm_hoist(int header, int body_count) {
    int target;
    int ri;
    int b;
    int i;
    int cl;

    target = ssa_idom[header];
    if (target < 0 || target >= bb_nblk) return;

    /* Process body blocks in RPO order (defs before uses) */
    ri = 0;
    while (ri < ssa_rpo_cnt) {
        b = ssa_rpo_ord[ri];
        if (b >= 0 && b < bb_nblk && ssa_vis[b]) {
            i = bb_start[b];
            while (i < bb_end[b]) {
                if (ho_use[i]) {
                    cl = licm_clone(i);
                    if (cl < 0) {
                        ri = ssa_rpo_cnt;  /* abort */
                        break;
                    }
                    /* Set block of clone to target (preheader) */
                    h_blk[cl] = target;

                    /* Append to target's hoisted list (defs before uses) */
                    licm_next[cl] = -1;
                    if (licm_tail[target] >= 0) {
                        licm_next[licm_tail[target]] = cl;
                    } else {
                        licm_head[target] = cl;
                    }
                    licm_tail[target] = cl;

                    /* NOP the original */
                    h_kind[i] = HI_NOP;

                    /* Record mapping */
                    licm_map[i] = cl;
                    licm_stat_hoisted = licm_stat_hoisted + 1;
                }
                i = i + 1;
            }
        }
        ri = ri + 1;
    }
}

/* Global rewrite: replace all references to hoisted originals with clones. */
static void licm_rewrite(void) {
    int i;
    int k;
    int j;
    int a;

    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP) { i = i + 1; continue; }

        /* src1 */
        if (h_src1[i] >= 0 && licm_map[h_src1[i]] >= 0) {
            h_src1[i] = licm_map[h_src1[i]];
        }

        /* src2 (instruction ref) */
        if (h_src2[i] >= 0 && ho_src2_is_ref(k) && licm_map[h_src2[i]] >= 0) {
            h_src2[i] = licm_map[h_src2[i]];
        }

        /* Call arguments */
        if ((k == HI_CALL || k == HI_CALLP ||
             k == HI_A64_DBT_TRAMPOLINE || k == HI_X64_DBT_TRAMPOLINE) &&
            h_cbase[i] >= 0) {
            j = 0;
            while (j < h_val[i]) {
                a = h_carg[h_cbase[i] + j];
                if (a >= 0 && licm_map[a] >= 0) {
                    h_carg[h_cbase[i] + j] = licm_map[a];
                }
                j = j + 1;
            }
        }

        /* PHI arguments */
        if (k == HI_PHI && h_pbase[i] >= 0) {
            j = 0;
            while (j < h_pcnt[i]) {
                a = h_pval[h_pbase[i] + j];
                if (a >= 0 && licm_map[a] >= 0) {
                    h_pval[h_pbase[i] + j] = licm_map[a];
                }
                j = j + 1;
            }
        }

        i = i + 1;
    }
}

/* ----------------------------------------------------------------
 * Main entry point
 * ---------------------------------------------------------------- */

/* Classify one use of value v occurring in block ub, for licm_split. */
static void sp_use(int v, int ub, int pre, int header) {
    if (v < 0) return;
    if (ub < 0 || ub >= bb_nblk) { sp_bad[v] = 1; return; }
    if (ssa_vis[ub]) { sp_inbody[v] = 1; return; }
    if (ub == pre || licm_dominates(ub, header)) return;   /* pre-loop */
    if (licm_dominates(pre, ub)) { sp_post[v] = 1; return; }
    sp_bad[v] = 1;
}

static int sp_new_inst(int kind, int ty, int s1, int s2, int val, int blk) {
    int cl;
    cl = h_ninst;
    if (cl >= HIR_MAX_INST) return -1;
    h_kind[cl] = kind;
    h_ty[cl] = ty;
    h_src1[cl] = s1;
    h_src2[cl] = s2;
    h_val[cl] = val;
    h_name[cl] = 0;
    h_blk[cl] = blk;
    h_cbase[cl] = -1;
    h_pbase[cl] = -1;
    h_pcnt[cl] = 0;
    h_ld_ro[cl] = 0;
    h_no_remat[cl] = 0;
    licm_map[cl] = -1;
    licm_next[cl] = -1;
    h_ninst = h_ninst + 1;
    return cl;
}

static void licm_split(int header) {
    int pre;
    int i;
    int j;
    int k;
    int v;
    int lim;
    int nsplit;
    int a_inst;
    int s_inst;
    int lb[8];
    int li[8];
    int nlb;

    pre = ssa_idom[header];
    if (pre < 0 || pre >= bb_nblk || ssa_vis[pre]) return;

    lim = h_ninst;
    i = 0;
    while (i < lim) {
        sp_inbody[i] = 0;
        sp_post[i] = 0;
        sp_bad[i] = 0;
        i = i + 1;
    }

    /* Classify every use */
    i = 0;
    while (i < lim) {
        k = h_kind[i];
        if (k == HI_NOP) { i = i + 1; continue; }
        if (k == HI_PHI && h_pbase[i] >= 0) {
            j = 0;
            while (j < h_pcnt[i]) {
                sp_use(h_pval[h_pbase[i] + j], h_pblk[h_pbase[i] + j],
                       pre, header);
                j = j + 1;
            }
        } else {
            if (h_src1[i] >= 0) sp_use(h_src1[i], h_blk[i], pre, header);
            if (h_src2[i] >= 0 && ho_src2_is_ref(k))
                sp_use(h_src2[i], h_blk[i], pre, header);
            if ((k == HI_CALL || k == HI_CALLP) && h_cbase[i] >= 0) {
                j = 0;
                while (j < h_val[i]) {
                    sp_use(h_carg[h_cbase[i] + j], h_blk[i], pre, header);
                    j = j + 1;
                }
            }
        }
        i = i + 1;
    }

    nsplit = 0;
    v = 0;
    while (v < lim && nsplit < 8) {
        k = h_kind[v];
        if (k == HI_NOP || !hi_has_value(k) || hi_inst_remat(v) ||
            k == HI_CALL || k == HI_CALLHI ||
            sp_inbody[v] || sp_bad[v] || !sp_post[v] ||
            h_blk[v] < 0 || h_blk[v] >= bb_nblk || ssa_vis[h_blk[v]] ||
            !licm_dominates(h_blk[v], header)) { v = v + 1; continue; }

        if (h_ninst + 2 >= HIR_MAX_INST) return;

        split_frame = split_frame + 4;
        a_inst = sp_new_inst(HI_ALLOCA, TY_INT, -1, -1, 0 - split_frame, pre);
        s_inst = sp_new_inst(HI_STORE, TY_INT, a_inst, v, 0, pre);
        if (a_inst < 0 || s_inst < 0) return;

        /* The store runs in the preheader, after any hoisted clones
         * (which may themselves read v). */
        if (licm_tail[pre] >= 0) licm_next[licm_tail[pre]] = s_inst;
        else licm_head[pre] = s_inst;
        licm_tail[pre] = s_inst;

        /* Rewrite each post-loop use through a per-block reload. */
        nlb = 0;
        i = 0;
        while (i < lim) {
            k = h_kind[i];
            if (k == HI_NOP) { i = i + 1; continue; }
            if (k == HI_PHI && h_pbase[i] >= 0) {
                j = 0;
                while (j < h_pcnt[i]) {
                    if (h_pval[h_pbase[i] + j] == v) {
                        int ub;
                        ub = h_pblk[h_pbase[i] + j];
                        if (ub >= 0 && ub < bb_nblk && !ssa_vis[ub] &&
                            ub != pre && !licm_dominates(ub, header) &&
                            licm_dominates(pre, ub)) {
                            int ld;
                            int x;
                            ld = -1;
                            x = 0;
                            while (x < nlb) { if (lb[x] == ub) { ld = li[x]; break; } x = x + 1; }
                            if (ld < 0 && nlb < 8) {
                                ld = sp_new_inst(HI_LOAD, TY_INT, a_inst, -1, 0, ub);
                                if (ld < 0) return;
                                licm_next[ld] = split_head[ub];
                                split_head[ub] = ld;
                                lb[nlb] = ub; li[nlb] = ld; nlb = nlb + 1;
                            }
                            if (ld >= 0) h_pval[h_pbase[i] + j] = ld;
                        }
                    }
                    j = j + 1;
                }
                i = i + 1;
                continue;
            }
            {
                int ub;
                int ld;
                int x;
                int refs;
                ub = h_blk[i];
                refs = 0;
                if (h_src1[i] == v) refs = 1;
                if (h_src2[i] == v && ho_src2_is_ref(k)) refs = 1;
                if ((k == HI_CALL || k == HI_CALLP) && h_cbase[i] >= 0) {
                    j = 0;
                    while (j < h_val[i]) {
                        if (h_carg[h_cbase[i] + j] == v) refs = 1;
                        j = j + 1;
                    }
                }
                if (!refs || ub < 0 || ub >= bb_nblk || ssa_vis[ub] ||
                    ub == pre || licm_dominates(ub, header) ||
                    !licm_dominates(pre, ub)) {
                    i = i + 1;
                    continue;
                }
                ld = -1;
                x = 0;
                while (x < nlb) { if (lb[x] == ub) { ld = li[x]; break; } x = x + 1; }
                if (ld < 0 && nlb < 8) {
                    ld = sp_new_inst(HI_LOAD, TY_INT, a_inst, -1, 0, ub);
                    if (ld < 0) return;
                    licm_next[ld] = split_head[ub];
                    split_head[ub] = ld;
                    lb[nlb] = ub; li[nlb] = ld; nlb = nlb + 1;
                }
                if (ld >= 0) {
                    if (h_src1[i] == v) h_src1[i] = ld;
                    if (h_src2[i] == v && ho_src2_is_ref(k)) h_src2[i] = ld;
                    if ((k == HI_CALL || k == HI_CALLP) && h_cbase[i] >= 0) {
                        j = 0;
                        while (j < h_val[i]) {
                            if (h_carg[h_cbase[i] + j] == v)
                                h_carg[h_cbase[i] + j] = ld;
                            j = j + 1;
                        }
                    }
                }
            }
            i = i + 1;
        }
        licm_stat_split = licm_stat_split + 1;
        nsplit = nsplit + 1;
        v = v + 1;
    }
}

/* --- Derived induction variables (DIVERGENCE f77, port upstream) ---
 *
 * Strength reduction.  For a loop phi iv with a single back edge and
 * an invariant step, an in-body SLL(iv, k) / MUL(iv, m) / ADD(iv, m)
 * with invariant k/m is itself an induction variable: give it its
 * own phi (init computed once in the preheader, its own increment in
 * the latch) and rewrite the uses.  Run twice, so ADD(base, p) over
 * a first-round derived p becomes a POINTER induction variable --
 * the clang shape: no shift, no add, just a bumped address.  ADDI
 * candidates are deliberately excluded: trailing displacement ADDIs
 * fold into load/store offsets, and deriving them would unfold that.
 * Original IVs whose only remaining use is their own increment are
 * swept afterwards. */
#define SR_MAX_IV 16
static int sr_phi[SR_MAX_IV];
static int sr_init[SR_MAX_IV];
static int sr_step[SR_MAX_IV];      /* step VALUE (inst id) */
static int sr_inc[SR_MAX_IV];
static int sr_niv;

static int sr_arg_of(int phi, int blk) {
    int j;
    j = 0;
    while (j < h_pcnt[phi]) {
        if (h_pblk[h_pbase[phi] + j] == blk) return h_pval[h_pbase[phi] + j];
        j = j + 1;
    }
    return -1;
}

static int sr_invariant(int v) {
    if (v < 0) return 0;
    if (hi_is_remat(h_kind[v])) return 1;
    if (h_blk[v] < 0 || h_blk[v] >= bb_nblk) return 0;
    return !ssa_vis[h_blk[v]];
}

/* Is value x one of the recorded IVs?  Returns record index or -1. */
static int sr_find(int x) {
    int i;
    i = 0;
    while (i < sr_niv) {
        if (sr_phi[i] == x) return i;
        i = i + 1;
    }
    return -1;
}

/* Resolve a reference one hop through licm_map: LICM NOPs hoisted
 * originals immediately but defers the global reference rewrite to
 * the end of hir_licm, so at strength-reduction time users of a
 * hoisted computation still name the ORIGINAL.  (This cost a day-one
 * bug: deriving from a clone rewrote zero references, and the
 * deferred rewrite then pointed users at the clone we had NOPped.) */
static int sr_res(int x) {
    if (x >= 0 && licm_map[x] >= 0) return licm_map[x];
    return x;
}

/* All uses of j inside the loop body (and none in phi pools outside)? */
static int sr_uses_local(int j) {
    int i;
    int k;
    int x;
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP || i == j) { i = i + 1; continue; }
        x = 0;
        if (sr_res(h_src1[i]) == j) x = 1;
        if (sr_res(h_src2[i]) == j && ho_src2_is_ref(k)) x = 1;
        if ((k == HI_CALL || k == HI_CALLP) && h_cbase[i] >= 0) {
            int q;
            q = 0;
            while (q < h_val[i]) {
                if (sr_res(h_carg[h_cbase[i] + q]) == j) x = 1;
                q = q + 1;
            }
        }
        if (k == HI_PHI && h_pbase[i] >= 0) {
            int q;
            q = 0;
            while (q < h_pcnt[i]) {
                if (sr_res(h_pval[h_pbase[i] + q]) == j) return 0;
                q = q + 1;
            }
        }
        if (x) {
            if (h_blk[i] < 0 || h_blk[i] >= bb_nblk || !ssa_vis[h_blk[i]])
                return 0;
        }
        i = i + 1;
    }
    return 1;
}

/* Count references to `old` that sr_rewrite would see (operands,
 * call args, phi pool), resolving one licm_map hop like the rewrite
 * does.  A candidate with zero VISIBLE references must not be
 * derived: either it is dead (nothing to gain) or it has a user
 * behind an aliasing form this pass does not know, and retiring it
 * would hand that user a NOP. */
static int sr_count_refs(int old) {
    int i;
    int k;
    int q;
    int n;
    n = 0;
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP) { i = i + 1; continue; }
        if (sr_res(h_src1[i]) == old) n = n + 1;
        if (sr_res(h_src2[i]) == old && ho_src2_is_ref(k)) n = n + 1;
        if ((k == HI_CALL || k == HI_CALLP) && h_cbase[i] >= 0) {
            q = 0;
            while (q < h_val[i]) {
                if (sr_res(h_carg[h_cbase[i] + q]) == old) n = n + 1;
                q = q + 1;
            }
        }
        if (k == HI_PHI && h_pbase[i] >= 0) {
            q = 0;
            while (q < h_pcnt[i]) {
                if (sr_res(h_pval[h_pbase[i] + q]) == old) n = n + 1;
                q = q + 1;
            }
        }
        i = i + 1;
    }
    return n;
}

/* Rewrite every reference to old with nw. */
static int sr_rewrite_n;
static void sr_rewrite(int old, int nw) {
    int i;
    int k;
    int q;
    sr_rewrite_n = 0;
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP || i == nw) { i = i + 1; continue; }
        if (sr_res(h_src1[i]) == old) { h_src1[i] = nw; sr_rewrite_n = sr_rewrite_n + 1; }
        if (sr_res(h_src2[i]) == old && ho_src2_is_ref(k)) { h_src2[i] = nw; sr_rewrite_n = sr_rewrite_n + 1; }
        if ((k == HI_CALL || k == HI_CALLP) && h_cbase[i] >= 0) {
            q = 0;
            while (q < h_val[i]) {
                if (sr_res(h_carg[h_cbase[i] + q]) == old) {
                    h_carg[h_cbase[i] + q] = nw;
                    sr_rewrite_n = sr_rewrite_n + 1;
                }
                q = q + 1;
            }
        }
        if (k == HI_PHI && h_pbase[i] >= 0) {
            q = 0;
            while (q < h_pcnt[i]) {
                if (sr_res(h_pval[h_pbase[i] + q]) == old) {
                    h_pval[h_pbase[i] + q] = nw;
                    sr_rewrite_n = sr_rewrite_n + 1;
                }
                q = q + 1;
            }
        }
        i = i + 1;
    }
}

static int licm_stat_sr;

static void licm_strred(int header, int latch) {
    int pre;
    int phi;
    int i;
    int rec;
    int round;
    int nderived;

    pre = ssa_idom[header];
    if (pre < 0 || pre >= bb_nblk || ssa_vis[pre]) return;

    /* Base IVs: two-arg phis of the header whose latch arg is an
     * ADD/ADDI of the phi by an invariant step. */
    sr_niv = 0;
    phi = ssa_phi_head[header];
    while (phi >= 0 && sr_niv < SR_MAX_IV) {
        if (h_kind[phi] == HI_PHI && h_pcnt[phi] == 2) {
            int a_l;
            int a_0;
            a_l = sr_arg_of(phi, latch);
            a_0 = -1;
            i = 0;
            while (i < 2) {
                if (h_pblk[h_pbase[phi] + i] != latch)
                    a_0 = h_pval[h_pbase[phi] + i];
                i = i + 1;
            }
            if (a_l >= 0 && a_0 >= 0) {
                int stepv;
                stepv = -1;
                if (h_kind[a_l] == HI_ADDI && h_src1[a_l] == phi) {
                    /* materialize the constant step as an ICONST */
                    if (h_ninst < HIR_MAX_INST) {
                        stepv = sp_new_inst(HI_ICONST, TY_INT, -1, -1,
                                            h_val[a_l], pre);
                    }
                } else if (h_kind[a_l] == HI_ADD) {
                    if (h_src1[a_l] == phi &&
                        sr_invariant(sr_res(h_src2[a_l])))
                        stepv = sr_res(h_src2[a_l]);
                    else if (h_src2[a_l] == phi &&
                             sr_invariant(sr_res(h_src1[a_l])))
                        stepv = sr_res(h_src1[a_l]);
                }
                if (stepv >= 0 && ssa_vis[h_blk[a_l]]) {
                    sr_phi[sr_niv] = phi;
                    sr_init[sr_niv] = sr_res(a_0);
                    sr_step[sr_niv] = stepv;
                    sr_inc[sr_niv] = a_l;
                    sr_niv = sr_niv + 1;
                }
            }
        }
        phi = ssa_phi_next[phi];
    }
    if (sr_niv == 0) return;

    nderived = 0;
    round = 0;
    while (round < 2) {
        int lim;
        lim = h_ninst;
        i = 0;
        while (i < lim && nderived < 6) {
            int k;
            int r;
            int other;
            int knd;
            k = h_kind[i];
            r = -1;
            other = -1;
            knd = 0;
            if (h_blk[i] >= 0 && h_blk[i] < bb_nblk && ssa_vis[h_blk[i]]) {
                int o1;
                int o2;
                o1 = sr_res(h_src1[i]);
                o2 = sr_res(h_src2[i]);
                if (k == HI_SLL && o2 >= 0 &&
                    h_kind[o2] == HI_ICONST &&
                    h_val[o2] >= 1 && h_val[o2] <= 20) {
                    r = sr_find(o1);
                    other = o2;
                    knd = 1;
                } else if (k == HI_MUL) {
                    if (sr_find(o1) >= 0 && sr_invariant(o2)) {
                        r = sr_find(o1); other = o2; knd = 2;
                    } else if (sr_find(o2) >= 0 && sr_invariant(o1)) {
                        r = sr_find(o2); other = o1; knd = 2;
                    }
                } else if (k == HI_ADD) {
                    if (sr_find(o1) >= 0 && sr_invariant(o2)) {
                        r = sr_find(o1); other = o2; knd = 3;
                    } else if (sr_find(o2) >= 0 && sr_invariant(o1)) {
                        r = sr_find(o2); other = o1; knd = 3;
                    }
                }
            }
            if (r >= 0 && other >= 0 && getenv("F77_SR_DEBUG") &&
                !sr_uses_local(i))
                fprintf(stderr, "SR reject cand=%d kind=%d (nonlocal use)\n", i, k);
            if (r >= 0 && other >= 0 && sr_uses_local(i) &&
                sr_count_refs(i) > 0 &&
                sr_niv < SR_MAX_IV && h_ninst + 4 < HIR_MAX_INST) {
                int ninit;
                int nstep;
                int nphi;
                int nnext;
                int okind;
                okind = (knd == 1) ? HI_SLL : ((knd == 2) ? HI_MUL : HI_ADD);
                /* init = op(iv.init, other), in the preheader */
                ninit = sp_new_inst(okind, h_ty[i], sr_init[r], other, 0, pre);
                if (licm_tail[pre] >= 0) licm_next[licm_tail[pre]] = ninit;
                else licm_head[pre] = ninit;
                licm_tail[pre] = ninit;
                /* step' = step<<k / step*m / step (for ADD) */
                if (knd == 3) {
                    nstep = sr_step[r];
                } else {
                    nstep = sp_new_inst(okind, TY_INT, sr_step[r], other, 0, pre);
                    licm_next[licm_tail[pre]] = nstep;
                    licm_tail[pre] = nstep;
                }
                /* the new phi, in the header's list */
                nphi = sp_new_inst(HI_PHI, h_ty[i], -1, -1, 0, header);
                if (h_nparg + 2 > HIR_MAX_PARG) return;
                h_pbase[nphi] = h_nparg;
                h_pcnt[nphi] = 2;
                {
                    int q;
                    q = 0;
                    while (q < 2) {
                        h_pblk[h_nparg] = h_pblk[h_pbase[sr_phi[r]] + q];
                        h_pval[h_nparg] =
                            (h_pblk[h_nparg] == latch) ? -1 : ninit;
                        h_nparg = h_nparg + 1;
                        q = q + 1;
                    }
                }
                ssa_phi_next[nphi] = ssa_phi_head[header];
                ssa_phi_head[header] = nphi;
                /* increment in the latch */
                nnext = sp_new_inst(HI_ADD, h_ty[i], nphi, nstep, 0, latch);
                if (licm_tail[latch] >= 0) licm_next[licm_tail[latch]] = nnext;
                else licm_head[latch] = nnext;
                licm_tail[latch] = nnext;
                {
                    int q;
                    q = 0;
                    while (q < 2) {
                        if (h_pblk[h_pbase[nphi] + q] == latch)
                            h_pval[h_pbase[nphi] + q] = nnext;
                        q = q + 1;
                    }
                }
                if (getenv("F77_SR_DEBUG"))
                    fprintf(stderr, "SR hdr=%d latch=%d pre=%d: cand=%d kind=%d iv=%d init=%d step=%d -> ninit=%d nstep=%d nphi=%d nnext=%d\n",
                            header, latch, pre, i, k, sr_phi[r], sr_init[r], sr_step[r], ninit, nstep, nphi, nnext);
                /* rewrite and retire the original computation */
                sr_rewrite(i, nphi);
                if (getenv("F77_SR_DEBUG"))
                    fprintf(stderr, "SR rewrote %d refs of %d\n", sr_rewrite_n, i);
                h_kind[i] = HI_NOP;
                h_src1[i] = -1;
                h_src2[i] = -1;
                /* register as an IV for round 2 */
                sr_phi[sr_niv] = nphi;
                sr_init[sr_niv] = ninit;
                sr_step[sr_niv] = nstep;
                sr_inc[sr_niv] = nnext;
                sr_niv = sr_niv + 1;
                nderived = nderived + 1;
                licm_stat_sr = licm_stat_sr + 1;
            }
            i = i + 1;
        }
        round = round + 1;
    }

    /* Sweep original IVs whose only remaining use is their own
     * increment (the subscripts were their only consumers). */
    i = 0;
    while (i < sr_niv) {
        int phi2;
        int inc2;
        int j;
        int k;
        int used;
        phi2 = sr_phi[i];
        inc2 = sr_inc[i];
        if (h_kind[phi2] != HI_PHI) { i = i + 1; continue; }
        used = 0;
        j = 0;
        while (j < h_ninst && !used) {
            k = h_kind[j];
            if (k == HI_NOP || j == inc2) { j = j + 1; continue; }
            if (h_src1[j] == phi2 || h_src1[j] == inc2) used = 1;
            if (!used && ho_src2_is_ref(k) &&
                (h_src2[j] == phi2 || h_src2[j] == inc2)) used = 1;
            if (!used && (k == HI_CALL || k == HI_CALLP) && h_cbase[j] >= 0) {
                int q;
                q = 0;
                while (q < h_val[j]) {
                    if (h_carg[h_cbase[j] + q] == phi2 ||
                        h_carg[h_cbase[j] + q] == inc2) used = 1;
                    q = q + 1;
                }
            }
            if (!used && k == HI_PHI && j != phi2 && h_pbase[j] >= 0) {
                int q;
                q = 0;
                while (q < h_pcnt[j]) {
                    if (h_pval[h_pbase[j] + q] == phi2 ||
                        h_pval[h_pbase[j] + q] == inc2) used = 1;
                    q = q + 1;
                }
            }
            j = j + 1;
        }
        if (getenv("F77_SR_DEBUG"))
            fprintf(stderr, "SWEEP hdr=%d rec=%d phi=%d inc=%d used=%d\n",
                    header, i, phi2, inc2, used);
        if (!used) {
            /* unlink the phi from the header's list, NOP both */
            int pp;
            if (ssa_phi_head[header] == phi2) {
                ssa_phi_head[header] = ssa_phi_next[phi2];
            } else {
                pp = ssa_phi_head[header];
                while (pp >= 0 && ssa_phi_next[pp] != phi2) pp = ssa_phi_next[pp];
                if (pp >= 0) ssa_phi_next[pp] = ssa_phi_next[phi2];
            }
            h_kind[phi2] = HI_NOP;
            h_pcnt[phi2] = 0;
            h_kind[inc2] = HI_NOP;
            h_src1[inc2] = -1;
            h_src2[inc2] = -1;
        }
        i = i + 1;
    }
}

static void hir_licm(void) {
    int b;
    int i;
    int si;
    int s;
    int body_count;

    /* Initialize (licm_stat_hoisted is cumulative across functions) */
    b = 0;
    while (b < bb_nblk) {
        licm_head[b] = -1;
        licm_tail[b] = -1;
        licm_in_any_loop[b] = 0;
        licm_depth[b] = 0;
        split_head[b] = -1;
        b = b + 1;
    }
    /* Frame high-water for split slots: below every existing ALLOCA,
     * and never above fp-8 -- the frontend reserves the top 8 bytes
     * of the frame for the saved r31/r30 (f77_frame starts at 8; the
     * same ps_stack = 8 convention as the C compiler).  In a function
     * whose allocas were all promoted away, the scan alone sees
     * nothing and the first slot would land ON the saved return
     * address (SCALE in slice9: J overwrote r31's save; RETURN
     * jumped to J). */
    split_frame = 8;
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_ALLOCA && h_val[i] < 0 &&
            0 - h_val[i] > split_frame) split_frame = 0 - h_val[i];
        i = i + 1;
    }
    i = 0;
    while (i < h_ninst) {
        licm_map[i] = -1;
        licm_next[i] = -1;
        ho_use[i] = 0;  /* clear stale use counts before reusing as invariant flag */
        i = i + 1;
    }

    /* Find natural loops: scan for back-edges (b -> s where s dominates b) */
    b = 0;
    while (b < bb_nblk) {
        /* Check each successor */
        si = 0;
        while (si < ssa_nsucc[b]) {
            s = ssa_succ[ssa_soff[b] + si];
            if (s >= 0 && s < bb_nblk && licm_dominates(s, b)) {
                /* Back-edge: b -> s, s is loop header */
                body_count = licm_find_body(s, b);
                {
                    int lb;
                    lb = 0;
                    while (lb < bb_nblk) {
                        if (ssa_vis[lb]) {
                            licm_in_any_loop[lb] = 1;
                            licm_depth[lb] = licm_depth[lb] + 1;
                        }
                        lb = lb + 1;
                    }
                }
                licm_mark(body_count);
                licm_hoist(s, body_count);
                licm_split(s);
                licm_strred(s, b);
            }
            si = si + 1;
        }
        b = b + 1;
    }

    /* Global rewrite of all references */
    if (licm_stat_hoisted > 0) {
        licm_rewrite();
    }

}
