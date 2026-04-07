/* hir_ssa.h -- SSA construction for s12cc
 *
 * Phase B: CFG, RPO, dominator tree, DF, phi insertion, renaming.
 * Promotes scalar local ALLOCAs (not params, arrays, structs) to SSA.
 */

/* --- CFG arrays --- */
#define SSA_SUCC_SZ 4096
static int ssa_succ[SSA_SUCC_SZ];
static int ssa_nsucc[HIR_MAX_BLOCK];

#define SSA_MAX_PRED 8192
static int ssa_pred[SSA_MAX_PRED];
static int ssa_pbase[HIR_MAX_BLOCK];
static int ssa_npred[HIR_MAX_BLOCK];

/* --- RPO --- */
static int ssa_rpo[HIR_MAX_BLOCK];
static int ssa_rpo_ord[HIR_MAX_BLOCK];
static int ssa_rpo_cnt;

/* --- Dominators --- */
static int ssa_idom[HIR_MAX_BLOCK];

/* --- Dominance frontier (fixed width per block) --- */
#define SSA_DF_W 32
#define SSA_DF_SZ 65536
static int ssa_df[SSA_DF_SZ];
static int ssa_dfc[HIR_MAX_BLOCK];

/* --- Promotable variables --- */
#define SSA_MAX_PROMO 256
static int ssa_promo[SSA_MAX_PROMO];
static int ssa_npromo;
static int ssa_phi_base; /* h_ninst before phi insertion */

/* --- PHI optimization (linked lists per block) --- */
static int ssa_phi_head[HIR_MAX_BLOCK];
static int ssa_phi_next[HIR_MAX_INST];

/* hl_nparams defined in hir_lower.h (shared) */

/* --- Dom tree children (flat) --- */
#define SSA_DTC_MAX 8192
static int ssa_dtc[SSA_DTC_MAX];
static int ssa_dtc_base[HIR_MAX_BLOCK];
static int ssa_dtc_cnt[HIR_MAX_BLOCK];

/* --- Rename: per-variable value stack --- */
#define SSA_VD 64
#define SSA_VSTK_SZ 16384
static int ssa_vstk[SSA_VSTK_SZ];
static int ssa_vtop[SSA_MAX_PROMO];

/* --- Rename: DFS stack --- */
#define SSA_DFS 2048
static int ssa_rblk[SSA_DFS];
static int ssa_rci[SSA_DFS];
static int ssa_rmark[SSA_DFS];

/* --- Rename: push log for backtracking --- */
#define SSA_PLOG 8192
static int ssa_plog[SSA_PLOG];
static int ssa_plog_n;

/* --- Temp arrays (reused across passes) --- */
static int ssa_vis[HIR_MAX_BLOCK];
static int ssa_post[HIR_MAX_BLOCK];
static int ssa_tmp1[HIR_MAX_BLOCK];  /* cnt / wl */
static int ssa_tmp2[HIR_MAX_BLOCK];  /* has_phi */
static int ssa_tmp3[HIR_MAX_BLOCK];  /* in_wl */

/* ------------------------------------------------------------ */

static int ssa_find_promo_idx(int inst) {
    int i;
    if (inst < 0) return -1;
    if (h_kind[inst] != HI_ALLOCA) return -1;
    i = 0;
    while (i < ssa_npromo) {
        if (ssa_promo[i] == inst) return i;
        i = i + 1;
    }
    return -1;
}

static int ssa_phi_find_arg(int phi, int pred) {
    int base;
    int cnt;
    int j;
    base = h_pbase[phi];
    cnt = h_pcnt[phi];
    j = 0;
    while (j < cnt) {
        if (h_pblk[base + j] == pred) return h_pval[base + j];
        j = j + 1;
    }
    return -1;
}

/* ------------------------------------------------------------ */
/* Step 1: Build CFG successor/predecessor lists                */
/* ------------------------------------------------------------ */

static void ssa_build_cfg(void) {
    int b;
    int term;
    int k;
    int total;
    int i;
    int s;
    int pb;

    b = 0;
    while (b < bb_nblk) {
        ssa_nsucc[b] = 0;
        ssa_npred[b] = 0;
        b = b + 1;
    }

    /* Build successors from terminators.
       Find the FIRST terminator in each block (dead code after
       goto/break/return may leave non-terminator instructions at
       the end of a block). */
    b = 0;
    while (b < bb_nblk) {
        if (bb_start[b] >= bb_end[b]) { b = b + 1; continue; }
        term = bb_start[b];
        while (term < bb_end[b]) {
            k = h_kind[term];
            if (k == HI_BR || k == HI_BRC || k == HI_RET) break;
            term = term + 1;
        }
        if (term < bb_end[b]) {
            k = h_kind[term];
            if (k == HI_BR) {
                ssa_succ[b * 2] = h_val[term];
                ssa_nsucc[b] = 1;
            } else if (k == HI_BRC) {
                ssa_succ[b * 2] = h_src2[term];
                ssa_succ[b * 2 + 1] = h_val[term];
                ssa_nsucc[b] = 2;
            }
        }
        b = b + 1;
    }

    /* Count predecessors (with bounds validation on targets) */
    b = 0;
    while (b < bb_nblk) {
        i = 0;
        while (i < ssa_nsucc[b]) {
            s = ssa_succ[b * 2 + i];
            if (s >= 0 && s < bb_nblk) {
                ssa_npred[s] = ssa_npred[s] + 1;
                i = i + 1;
            } else {
                /* Invalid target — remove this edge by shifting second edge if it exists */
                if (i == 0 && ssa_nsucc[b] == 2) {
                    ssa_succ[b * 2] = ssa_succ[b * 2 + 1];
                }
                ssa_nsucc[b] = ssa_nsucc[b] - 1;
                /* Don't increment i — retry current slot which now has the next edge */
            }
        }
        b = b + 1;
    }

    /* Compute pred bases */
    total = 0;
    b = 0;
    while (b < bb_nblk) {
        ssa_pbase[b] = total;
        total = total + ssa_npred[b];
        ssa_npred[b] = 0;
        b = b + 1;
    }
    if (total > SSA_MAX_PRED) {
        fdputs("s12cc: too many CFG edges\n", 2);
        exit(1);
    }

    /* Fill predecessors (with bounds validation) */
    b = 0;
    while (b < bb_nblk) {
        i = 0;
        while (i < ssa_nsucc[b]) {
            s = ssa_succ[b * 2 + i];
            if (s >= 0 && s < bb_nblk) {
                pb = ssa_pbase[s] + ssa_npred[s];
                ssa_pred[pb] = b;
                ssa_npred[s] = ssa_npred[s] + 1;
            }
            i = i + 1;
        }
        b = b + 1;
    }
}

/* ------------------------------------------------------------ */
/* Step 2: Compute RPO via iterative DFS                        */
/* ------------------------------------------------------------ */

static void ssa_compute_rpo(void) {
    int top;
    int b;
    int i;
    int s;
    int post_n;

    b = 0;
    while (b < bb_nblk) {
        ssa_vis[b] = 0;
        ssa_rpo[b] = -1;
        b = b + 1;
    }
    post_n = 0;

    top = 0;
    ssa_rblk[0] = 0;
    ssa_rci[0] = 0;
    ssa_vis[0] = 1;

    while (top >= 0) {
        b = ssa_rblk[top];
        i = ssa_rci[top];
        if (i < ssa_nsucc[b]) {
            ssa_rci[top] = i + 1;
            s = ssa_succ[b * 2 + i];
            if (!ssa_vis[s]) {
                ssa_vis[s] = 1;
                top = top + 1;
                if (top >= SSA_DFS) {
                    fdputs("s12cc: RPO DFS stack overflow\n", 2);
                    exit(1);
                }
                ssa_rblk[top] = s;
                ssa_rci[top] = 0;
            }
        } else {
            ssa_post[post_n] = b;
            post_n = post_n + 1;
            top = top - 1;
        }
    }

    /* Reverse post-order = reversed post_order */
    ssa_rpo_cnt = post_n;
    i = 0;
    while (i < post_n) {
        ssa_rpo_ord[i] = ssa_post[post_n - 1 - i];
        ssa_rpo[ssa_rpo_ord[i]] = i;
        i = i + 1;
    }
}

/* ------------------------------------------------------------ */
/* Step 3: Compute immediate dominators (Cooper-Harvey-Kennedy)  */
/* ------------------------------------------------------------ */

static int ssa_intersect(int b1, int b2) {
    int guard;
    guard = 0;
    while (b1 != b2) {
        while (ssa_rpo[b1] > ssa_rpo[b2]) {
            if (b1 == ssa_idom[b1]) return b1; /* at root */
            b1 = ssa_idom[b1];
        }
        while (ssa_rpo[b2] > ssa_rpo[b1]) {
            if (b2 == ssa_idom[b2]) return b2; /* at root */
            b2 = ssa_idom[b2];
        }
        guard = guard + 1;
        if (guard > bb_nblk) return 0; /* bail to entry */
    }
    return b1;
}

static void ssa_compute_idom(void) {
    int changed;
    int ri;
    int b;
    int j;
    int p;
    int new_idom;

    b = 0;
    while (b < bb_nblk) {
        ssa_idom[b] = -1;
        b = b + 1;
    }
    ssa_idom[0] = 0;

    changed = 1;
    while (changed) {
        changed = 0;
        ri = 1;
        while (ri < ssa_rpo_cnt) {
            b = ssa_rpo_ord[ri];
            new_idom = -1;
            j = 0;
            while (j < ssa_npred[b]) {
                p = ssa_pred[ssa_pbase[b] + j];
                if (ssa_idom[p] >= 0) {
                    if (new_idom < 0) {
                        new_idom = p;
                    } else {
                        new_idom = ssa_intersect(new_idom, p);
                    }
                }
                j = j + 1;
            }
            if (new_idom >= 0 && ssa_idom[b] != new_idom) {
                ssa_idom[b] = new_idom;
                changed = 1;
            }
            ri = ri + 1;
        }
    }
}

/* ------------------------------------------------------------ */
/* Step 4: Compute dominance frontiers                           */
/* ------------------------------------------------------------ */

static void ssa_compute_df(void) {
    int b;
    int j;
    int p;
    int runner;
    int base;
    int di;
    int dup;

    b = 0;
    while (b < bb_nblk) {
        ssa_dfc[b] = 0;
        b = b + 1;
    }

    b = 0;
    while (b < bb_nblk) {
        if (ssa_npred[b] < 2) { b = b + 1; continue; }
        if (ssa_idom[b] < 0) { b = b + 1; continue; }
        j = 0;
        while (j < ssa_npred[b]) {
            p = ssa_pred[ssa_pbase[b] + j];
            runner = p;
            while (runner >= 0 && runner != ssa_idom[b]) {
                /* Add b to DF(runner), check dups */
                base = runner * SSA_DF_W;
                dup = 0;
                di = 0;
                while (di < ssa_dfc[runner]) {
                    if (ssa_df[base + di] == b) { dup = 1; }
                    di = di + 1;
                }
                if (!dup && ssa_dfc[runner] < SSA_DF_W) {
                    ssa_df[base + ssa_dfc[runner]] = b;
                    ssa_dfc[runner] = ssa_dfc[runner] + 1;
                }
                /* Stop at root (self-dominating entry node) */
                if (runner == ssa_idom[runner]) break;
                runner = ssa_idom[runner];
            }
            j = j + 1;
        }
        b = b + 1;
    }
}

/* ------------------------------------------------------------ */
/* Step 5: Find promotable ALLOCAs                               */
/* ------------------------------------------------------------ */

static void ssa_find_promo(void) {
    int i;
    int j;
    int k;
    int ai;
    int a;
    int off;
    int ok[HL_MAX_ALLOCA];

    /* Mark all allocas as potentially promotable */
    i = 0;
    while (i < hl_nalloca) {
        ok[i] = 1;
        i = i + 1;
    }

    /* Scan instructions for non-promotable uses */
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        /* src1: only OK as addr in LOAD or STORE */
        if (h_src1[i] >= 0 && h_kind[h_src1[i]] == HI_ALLOCA) {
            if (k != HI_LOAD && k != HI_STORE) {
                ai = ssa_find_promo_idx(h_src1[i]);
                if (ai < 0) {
                    /* Not yet in promo list, find by scanning hl_ainst */
                    j = 0;
                    while (j < hl_nalloca) {
                        if (hl_ainst[j] == h_src1[i]) { ok[j] = 0; }
                        j = j + 1;
                    }
                }
            }
        }
        /* src2: any alloca as src2 means address-taken (STORE val) */
        if (h_src2[i] >= 0 && h_kind[h_src2[i]] == HI_ALLOCA) {
            j = 0;
            while (j < hl_nalloca) {
                if (hl_ainst[j] == h_src2[i]) { ok[j] = 0; }
                j = j + 1;
            }
        }
        /* Call arguments */
        if (k == HI_CALL || k == HI_CALLP) {
            j = 0;
            while (j < h_val[i]) {
                a = h_carg[h_cbase[i] + j];
                if (a >= 0 && h_kind[a] == HI_ALLOCA) {
                    ai = 0;
                    while (ai < hl_nalloca) {
                        if (hl_ainst[ai] == a) { ok[ai] = 0; }
                        ai = ai + 1;
                    }
                }
                j = j + 1;
            }
        }
        i = i + 1;
    }

    /* Collect promotable ALLOCAs */
    ssa_npromo = 0;
    i = 0;
    while (i < hl_nalloca) {
        if (ok[i] && ssa_npromo < SSA_MAX_PROMO) {
            ssa_promo[ssa_npromo] = hl_ainst[i];
            ssa_npromo = ssa_npromo + 1;
        }
        i = i + 1;
    }
}

/* ------------------------------------------------------------ */
/* Step 6: Build dominator tree children lists                   */
/* ------------------------------------------------------------ */

static void ssa_build_dtree(void) {
    int b;
    int total;
    int p;

    /* Use ssa_tmp1 as cnt[] (static, avoids large stack frame) */
    b = 0;
    while (b < bb_nblk) {
        ssa_tmp1[b] = 0;
        b = b + 1;
    }

    b = 1;
    while (b < bb_nblk) {
        if (ssa_idom[b] >= 0 && ssa_idom[b] != b) {
            ssa_tmp1[ssa_idom[b]] = ssa_tmp1[ssa_idom[b]] + 1;
        }
        b = b + 1;
    }

    total = 0;
    b = 0;
    while (b < bb_nblk) {
        ssa_dtc_base[b] = total;
        ssa_dtc_cnt[b] = 0;
        total = total + ssa_tmp1[b];
        b = b + 1;
    }
    if (total > SSA_DTC_MAX) {
        fdputs("s12cc: too many domtree children\n", 2);
        exit(1);
    }

    b = 1;
    while (b < bb_nblk) {
        if (ssa_idom[b] >= 0 && ssa_idom[b] != b) {
            p = ssa_idom[b];
            ssa_dtc[ssa_dtc_base[p] + ssa_dtc_cnt[p]] = b;
            ssa_dtc_cnt[p] = ssa_dtc_cnt[p] + 1;
        }
        b = b + 1;
    }
}

/* ------------------------------------------------------------ */
/* Step 7: Insert phi nodes                                      */
/* ------------------------------------------------------------ */

static void ssa_insert_phis(void) {
    int v;
    int i;
    int b;
    int d;
    int di;
    int phi;
    int ty;
    int j;
    int wh;
    int wt;

    ssa_phi_base = h_ninst;

    v = 0;
    while (v < ssa_npromo) {
        ty = h_ty[ssa_promo[v]];

        /* ssa_tmp2 = has_phi, ssa_tmp3 = in_wl, ssa_tmp1 = wl */
        b = 0;
        while (b < bb_nblk) {
            ssa_tmp2[b] = 0;
            ssa_tmp3[b] = 0;
            b = b + 1;
        }

        /* Worklist: blocks with stores to this var */
        wh = 0;
        wt = 0;
        i = 0;
        while (i < ssa_phi_base) {
            if (h_kind[i] == HI_STORE && h_src1[i] == ssa_promo[v]) {
                b = h_blk[i];
                if (!ssa_tmp3[b]) {
                    ssa_tmp3[b] = 1;
                    ssa_tmp1[wt] = b;
                    wt = wt + 1;
                }
            }
            i = i + 1;
        }

        while (wh < wt) {
            b = ssa_tmp1[wh];
            wh = wh + 1;
            di = 0;
            while (di < ssa_dfc[b]) {
                d = ssa_df[b * SSA_DF_W + di];
                if (!ssa_tmp2[d] && ssa_rpo[d] >= 0) {
                    ssa_tmp2[d] = 1;
                    /* Emit PHI, pre-allocate arg slots */
                    phi = hi_emit(HI_PHI, ty, -1, -1, v, NULL);
                    h_blk[phi] = d;
                    h_pbase[phi] = h_nparg;
                    h_pcnt[phi] = ssa_npred[d];
                    j = 0;
                    while (j < ssa_npred[d]) {
                        if (h_nparg >= HIR_MAX_PARG) {
                            fdputs("s12cc: too many phi args\n", 2);
                            exit(1);
                        }
                        h_pblk[h_nparg] = ssa_pred[ssa_pbase[d] + j];
                        h_pval[h_nparg] = -1;
                        h_nparg = h_nparg + 1;
                        j = j + 1;
                    }
                    if (!ssa_tmp3[d]) {
                        ssa_tmp3[d] = 1;
                        ssa_tmp1[wt] = d;
                        wt = wt + 1;
                    }
                }
                di = di + 1;
            }
        }
        v = v + 1;
    }

    /* Build PHI lists per block for fast lookup in ssa_rename */
    b = 0;
    while (b < bb_nblk) { ssa_phi_head[b] = -1; b = b + 1; }
    i = ssa_phi_base;
    while (i < h_ninst) {
        if (h_kind[i] == HI_PHI) {
            b = h_blk[i];
            ssa_phi_next[i] = ssa_phi_head[b];
            ssa_phi_head[b] = i;
        }
        i = i + 1;
    }
}

/* ------------------------------------------------------------ */
/* Step 8: Rename variables (iterative DFS on domtree)           */
/* ------------------------------------------------------------ */

static void ssa_vpush(int v, int val) {
    if (ssa_vtop[v] + 1 >= SSA_VD) {
        fdputs("s12cc: SSA var stack overflow\n", 2);
        exit(1);
    }
    ssa_vtop[v] = ssa_vtop[v] + 1;
    ssa_vstk[v * SSA_VD + ssa_vtop[v]] = val;
    if (ssa_plog_n >= SSA_PLOG) {
        fdputs("s12cc: SSA push log overflow\n", 2);
        exit(1);
    }
    ssa_plog[ssa_plog_n] = v;
    ssa_plog_n = ssa_plog_n + 1;
}

static int ssa_vcur(int v) {
    if (ssa_vtop[v] < 0) return -1;
    return ssa_vstk[v * SSA_VD + ssa_vtop[v]];
}

static void ssa_rename(void) {
    int top;
    int b;
    int ci;
    int nch;
    int child;
    int i;
    int k;
    int v;
    int val;
    int si;
    int s;
    int j;
    int phi_v;

    /* Init variable stacks: all start with undef (-1) */
    v = 0;
    while (v < ssa_npromo) {
        ssa_vtop[v] = 0;
        ssa_vstk[v * SSA_VD] = -1;
        v = v + 1;
    }
    ssa_plog_n = 0;

    /* DFS from entry block */
    top = 0;
    ssa_rblk[0] = 0;
    ssa_rci[0] = -1;
    ssa_rmark[0] = 0;

    while (top >= 0) {
        b = ssa_rblk[top];
        ci = ssa_rci[top];

        if (ci == -1) {
            /* First visit: process block b */
            ssa_rmark[top] = ssa_plog_n;

            /* Process PHI nodes in block b (they define the variable) */
            i = ssa_phi_head[b];
            while (i >= 0) {
                phi_v = h_val[i];
                ssa_vpush(phi_v, i);
                i = ssa_phi_next[i];
            }

            /* Process regular instructions (stop at first terminator
               to avoid processing dead code after goto/break/return) */
            i = bb_start[b];
            while (i < bb_end[b]) {
                k = h_kind[i];
                if (k == HI_LOAD) {
                    v = ssa_find_promo_idx(h_src1[i]);
                    if (v >= 0) {
                        val = ssa_vcur(v);
                        h_kind[i] = HI_COPY;
                        h_src1[i] = val;
                    }
                } else if (k == HI_STORE) {
                    v = ssa_find_promo_idx(h_src1[i]);
                    if (v >= 0) {
                        ssa_vpush(v, h_src2[i]);
                        h_kind[i] = HI_NOP;
                    }
                }
                if (k == HI_BR || k == HI_BRC || k == HI_RET) {
                    i = i + 1;
                    break;
                }
                i = i + 1;
            }

            /* Fill phi args in successors */
            si = 0;
            while (si < ssa_nsucc[b]) {
                s = ssa_succ[b * 2 + si];
                i = ssa_phi_head[s];
                while (i >= 0) {
                    phi_v = h_val[i];
                    val = ssa_vcur(phi_v);
                    /* Find this pred's slot in phi args */
                    j = 0;
                    while (j < h_pcnt[i]) {
                        if (h_pblk[h_pbase[i] + j] == b) {
                            h_pval[h_pbase[i] + j] = val;
                        }
                        j = j + 1;
                    }
                    i = ssa_phi_next[i];
                }
                si = si + 1;
            }

            ssa_rci[top] = 0;
        } else {
            nch = ssa_dtc_cnt[b];
            if (ci < nch) {
                child = ssa_dtc[ssa_dtc_base[b] + ci];
                ssa_rci[top] = ci + 1;
                top = top + 1;
                if (top >= SSA_DFS) {
                    fdputs("s12cc: SSA DFS stack overflow\n", 2);
                    exit(1);
                }
                ssa_rblk[top] = child;
                ssa_rci[top] = -1;
            } else {
                /* Backtrack: pop variable stacks */
                while (ssa_plog_n > ssa_rmark[top]) {
                    ssa_plog_n = ssa_plog_n - 1;
                    v = ssa_plog[ssa_plog_n];
                    ssa_vtop[v] = ssa_vtop[v] - 1;
                }
                top = top - 1;
            }
        }
    }
}

/* ------------------------------------------------------------ */
/* Main entry point                                              */
/* ------------------------------------------------------------ */

static void hir_ssa_construct(void) {
    int b;

    /* Always init phi linked lists (codegen uses them even with 0 promos) */
    b = 0;
    while (b < bb_nblk) { ssa_phi_head[b] = -1; b = b + 1; }

    if (bb_nblk < 1) {
        ssa_phi_base = h_ninst;
        return;
    }

    ssa_build_cfg();
    ssa_compute_rpo();
    ssa_compute_idom();
    ssa_compute_df();
    ssa_find_promo();

    if (ssa_npromo == 0) {
        ssa_phi_base = h_ninst;
        return;
    }

    ssa_build_dtree();
    /* Detach current block so hi_emit during phi insertion
       doesn't corrupt bb_end of the last lowered block */
    hl_cur_blk = -1;
    ssa_insert_phis();
    ssa_rename();
}
