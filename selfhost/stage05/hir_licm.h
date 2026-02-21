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

/* --- Stats --- */
static int licm_stat_hoisted;

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
                if (!ho_use[i] && hi_is_pure(k)) {
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
        if ((k == HI_CALL || k == HI_CALLP) && h_cbase[i] >= 0) {
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
        b = b + 1;
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
            s = ssa_succ[b * 2 + si];
            if (s >= 0 && s < bb_nblk && licm_dominates(s, b)) {
                /* Back-edge: b -> s, s is loop header */
                body_count = licm_find_body(s, b);
                licm_mark(body_count);
                licm_hoist(s, body_count);
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
