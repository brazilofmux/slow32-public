/* hir_regalloc_x64.h -- Linear scan register allocation for x86-64
 *
 * Forked from stage07/hir_regalloc.h (SLOW-32 target).
 * Allocates callee-saved registers RBX, R12-R15 (5 registers).
 * RAX, RCX, RDX are reserved as scratch for the codegen.
 *
 * Reference: Poletto-Sarkar "Linear Scan Register Allocation" (1999).
 *
 * Input:  HIR after SSA construction and optimization.
 * Output: ra_reg[]   — physical x64 register per inst (-1 = spilled/remat)
 *         ra_spill_off[] — stack offset per inst (negative from fp, 0 = none)
 *         ra_ncsave/ra_csave_reg[]/ra_csave_off[] — callee-save info
 */

#ifndef HIR_REGALLOC_X64_H
#define HIR_REGALLOC_X64_H

/* --- Configuration --- */
#define RA_NPHY 5  /* RBX, R12, R13, R14, R15 */

/* Mapping: slot index → x64 physical register encoding */
static int ra_x64_phys[RA_NPHY];

/* Reverse mapping: x64 register encoding → slot index (-1 = not allocatable) */
#define RA_X64_REVERSE_SIZE 16
static int ra_x64_slot[RA_X64_REVERSE_SIZE];

static void ra_init_x64_regs(void) {
    int i;
    ra_x64_phys[0] = X64_RBX;
    ra_x64_phys[1] = X64_R12;
    ra_x64_phys[2] = X64_R13;
    ra_x64_phys[3] = X64_R14;
    ra_x64_phys[4] = X64_R15;
    i = 0;
    while (i < RA_X64_REVERSE_SIZE) {
        ra_x64_slot[i] = -1;
        i = i + 1;
    }
    i = 0;
    while (i < RA_NPHY) {
        ra_x64_slot[ra_x64_phys[i]] = i;
        i = i + 1;
    }
}

/* --- Output arrays --- */
static int ra_reg[HIR_MAX_INST];    /* x64 physical register, -1 = spilled/remat */
static int ra_spill_off[HIR_MAX_INST]; /* spill slot (negative fp offset), 0 = none */

/* --- Live interval arrays --- */
static int ra_pos[HIR_MAX_INST];    /* linearized position */
static int ra_iend[HIR_MAX_INST];   /* last use position (end of live interval) */

/* --- Linearized order --- */
static int ra_order[HIR_MAX_INST];  /* instruction indices in position order */
static int ra_norder;

/* --- Active set (intervals currently occupying registers) --- */
static int ra_act[RA_NPHY];         /* inst index for each active slot */
static int ra_nact;

/* --- Free register stack --- */
static int ra_fstk[RA_NPHY];        /* stack of free x64 register numbers */
static int ra_nfree;

/* --- Callee-save tracking --- */
static int ra_used[RA_NPHY];        /* 1 if slot was assigned */
static int ra_csave_reg[RA_NPHY];   /* x64 physical register number */
static int ra_csave_off[RA_NPHY];   /* fp offset for save slot */
static int ra_ncsave;               /* count of registers to save */
static int ra_stat_spills;          /* cumulative spill count across all functions */

/* =================================================================
 * Step 1: Compute linearized positions (RPO block order)
 * ================================================================= */

static void ra_compute_pos(void) {
    int ri;
    int b;
    int i;
    int pos;
    int phi;
    int term;
    int tk;

    /* Init all positions to -1 (unreachable) */
    i = 0;
    while (i < h_ninst) {
        ra_pos[i] = -1;
        i = i + 1;
    }

    pos = 0;
    ra_norder = 0;

    /* Walk blocks in RPO order */
    ri = 0;
    while (ri < ssa_rpo_cnt) {
        b = ssa_rpo_ord[ri];

        /* PHIs for this block (defined at block start) */
        phi = ssa_phi_head[b];
        while (phi >= 0) {
            if (h_kind[phi] == HI_PHI) {
                ra_pos[phi] = pos;
                ra_order[ra_norder] = phi;
                ra_norder = ra_norder + 1;
                pos = pos + 1;
            }
            phi = ssa_phi_next[phi];
        }

        /* Find terminator (last BR/BRC/RET) in this block */
        term = -1;
        i = bb_end[b] - 1;
        while (i >= bb_start[b]) {
            tk = h_kind[i];
            if (tk == HI_BR || tk == HI_BRC || tk == HI_RET) {
                term = i;
                break;
            }
            if (tk != HI_NOP) break;
            i = i - 1;
        }

        /* LICM-hoisted instructions are emitted at block top by hx_gen_func(),
         * so linearization must match that order. */
        i = licm_head[b];
        while (i >= 0) {
            if (h_kind[i] != HI_NOP) {
                ra_pos[i] = pos;
                ra_order[ra_norder] = i;
                ra_norder = ra_norder + 1;
                pos = pos + 1;
            }
            i = licm_next[i];
        }

        /* Regular instructions up to (not including) the terminator */
        i = bb_start[b];
        while (i < bb_end[b]) {
            if (i == term) break;
            if (h_kind[i] != HI_NOP) {
                ra_pos[i] = pos;
                ra_order[ra_norder] = i;
                ra_norder = ra_norder + 1;
                pos = pos + 1;
            }
            i = i + 1;
        }

        /* The terminator itself */
        if (term >= 0 && h_kind[term] != HI_NOP) {
            ra_pos[term] = pos;
            ra_order[ra_norder] = term;
            ra_norder = ra_norder + 1;
            pos = pos + 1;
        }

        ri = ri + 1;
    }
}

/* =================================================================
 * Step 2: Compute live interval endpoints
 * ================================================================= */

static void ra_extend(int inst, int use_pos) {
    if (inst >= 0 && ra_pos[inst] >= 0 && use_pos > ra_iend[inst]) {
        ra_iend[inst] = use_pos;
    }
}

/* --- Cross-block liveness propagation --- */

static int ra_blk_last[HIR_MAX_BLOCK];
static int ra_bvis[HIR_MAX_BLOCK];
static int ra_bwl[HIR_MAX_BLOCK];

static void ra_backprop(int val, int use_blk) {
    int def_blk;
    int wh;
    int wt;
    int b;
    int j;
    int p;

    def_blk = h_blk[val];
    if (def_blk < 0 || use_blk < 0) return;
    if (def_blk == use_blk) return;

    /* Clear visited */
    b = 0;
    while (b < bb_nblk) {
        ra_bvis[b] = 0;
        b = b + 1;
    }

    /* Start BFS from use_blk */
    ra_bvis[use_blk] = 1;
    ra_extend(val, ra_blk_last[use_blk]);
    ra_bwl[0] = use_blk;
    wh = 0;
    wt = 1;

    while (wh < wt) {
        b = ra_bwl[wh];
        wh = wh + 1;

        j = 0;
        while (j < ssa_npred[b]) {
            p = ssa_pred[ssa_pbase[b] + j];
            if (p >= 0 && p < bb_nblk && !ra_bvis[p]) {
                ra_bvis[p] = 1;
                ra_extend(val, ra_blk_last[p]);
                if (p != def_blk && wt < HIR_MAX_BLOCK) {
                    ra_bwl[wt] = p;
                    wt = wt + 1;
                }
            }
            j = j + 1;
        }
    }
}

static void ra_compute_ends(void) {
    int i;
    int j;
    int inst;
    int k;
    int p;
    int a;
    int pred;
    int term;
    int src;
    int b;

    /* Initialize end = start (def point) */
    i = 0;
    while (i < h_ninst) {
        ra_iend[i] = ra_pos[i];
        i = i + 1;
    }

    /* Compute last position in each block */
    b = 0;
    while (b < bb_nblk) {
        ra_blk_last[b] = -1;
        b = b + 1;
    }
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        b = h_blk[inst];
        if (b >= 0 && b < bb_nblk) {
            ra_blk_last[b] = ra_pos[inst];
        }
        i = i + 1;
    }

    /* Scan all instructions for uses */
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];
        p = ra_pos[inst];

        /* src1 */
        ra_extend(h_src1[inst], p);

        /* src2 (instruction ref for binops and STORE) */
        if (h_src2[inst] >= 0 && ho_src2_is_ref(k)) {
            ra_extend(h_src2[inst], p);
        }

        /* Call arguments */
        if ((k == HI_CALL || k == HI_CALLP) && h_cbase[inst] >= 0) {
            j = 0;
            while (j < h_val[inst]) {
                ra_extend(h_carg[h_cbase[inst] + j], p);
                j = j + 1;
            }
        }

        /* PHI arguments: used at end of predecessor block */
        if (k == HI_PHI && h_pbase[inst] >= 0) {
            j = 0;
            while (j < h_pcnt[inst]) {
                a = h_pval[h_pbase[inst] + j];
                pred = h_pblk[h_pbase[inst] + j];
                if (a >= 0 && ra_pos[a] >= 0 && pred >= 0 && pred < bb_nblk) {
                    term = bb_end[pred] - 1;
                    while (term >= bb_start[pred] && h_kind[term] == HI_NOP) {
                        term = term - 1;
                    }
                    if (term >= bb_start[pred] && ra_pos[term] >= 0) {
                        ra_extend(a, ra_pos[term]);
                    }
                }
                j = j + 1;
            }
        }

        i = i + 1;
    }

    /* Cross-block liveness propagation */
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];

        src = h_src1[inst];
        if (src >= 0 && ra_pos[src] >= 0 && h_blk[src] != h_blk[inst]) {
            ra_backprop(src, h_blk[inst]);
        }

        if (h_src2[inst] >= 0 && ho_src2_is_ref(k)) {
            src = h_src2[inst];
            if (ra_pos[src] >= 0 && h_blk[src] != h_blk[inst]) {
                ra_backprop(src, h_blk[inst]);
            }
        }

        if ((k == HI_CALL || k == HI_CALLP) && h_cbase[inst] >= 0) {
            j = 0;
            while (j < h_val[inst]) {
                src = h_carg[h_cbase[inst] + j];
                if (src >= 0 && ra_pos[src] >= 0 && h_blk[src] != h_blk[inst]) {
                    ra_backprop(src, h_blk[inst]);
                }
                j = j + 1;
            }
        }

        if (k == HI_PHI && h_pbase[inst] >= 0) {
            j = 0;
            while (j < h_pcnt[inst]) {
                a = h_pval[h_pbase[inst] + j];
                pred = h_pblk[h_pbase[inst] + j];
                if (a >= 0 && ra_pos[a] >= 0 && pred >= 0 && pred < bb_nblk &&
                    h_blk[a] != pred) {
                    ra_backprop(a, pred);
                }
                j = j + 1;
            }
        }

        i = i + 1;
    }
}

/* =================================================================
 * Step 3: Linear scan allocation
 * ================================================================= */

static void ra_expire(int p) {
    int i;
    int j;
    int inst;
    int reg;
    int slot;

    i = 0;
    while (i < ra_nact) {
        inst = ra_act[i];
        if (ra_iend[inst] < p) {
            reg = ra_reg[inst];
            ra_fstk[ra_nfree] = reg;
            ra_nfree = ra_nfree + 1;
            j = i;
            while (j < ra_nact - 1) {
                ra_act[j] = ra_act[j + 1];
                j = j + 1;
            }
            ra_nact = ra_nact - 1;
        } else {
            i = i + 1;
        }
    }
}

static void ra_linear_scan(void) {
    int i;
    int inst;
    int k;
    int reg;
    int best;
    int best_end;
    int spill_inst;
    int j;
    int slot;

    /* Initialize free register pool */
    ra_nfree = RA_NPHY;
    i = 0;
    while (i < RA_NPHY) {
        ra_fstk[i] = ra_x64_phys[RA_NPHY - 1 - i];
        i = i + 1;
    }
    ra_nact = 0;

    /* Clear used tracking */
    i = 0;
    while (i < RA_NPHY) {
        ra_used[i] = 0;
        i = i + 1;
    }

    /* Init all to unallocated */
    i = 0;
    while (i < h_ninst) {
        ra_reg[i] = -1;
        i = i + 1;
    }

    /* Process instructions in linear order */
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];

        if (!hi_has_value(k) || hi_is_remat(k) || k == HI_NOP) {
            i = i + 1;
            continue;
        }

        ra_expire(ra_pos[inst]);

        if (ra_nfree > 0) {
            ra_nfree = ra_nfree - 1;
            reg = ra_fstk[ra_nfree];
            ra_reg[inst] = reg;
            slot = ra_x64_slot[reg];
            if (slot >= 0) ra_used[slot] = 1;
            ra_act[ra_nact] = inst;
            ra_nact = ra_nact + 1;
        } else {
            best = 0;
            best_end = ra_iend[ra_act[0]];
            j = 1;
            while (j < ra_nact) {
                if (ra_iend[ra_act[j]] > best_end) {
                    best = j;
                    best_end = ra_iend[ra_act[j]];
                }
                j = j + 1;
            }

            if (best_end > ra_iend[inst]) {
                spill_inst = ra_act[best];
                reg = ra_reg[spill_inst];
                ra_reg[spill_inst] = -1;
                ra_reg[inst] = reg;
                ra_act[best] = inst;
            }
        }

        i = i + 1;
    }
}

/* =================================================================
 * Step 4: Assign spill slots and callee-save slots
 * ================================================================= */

static void ra_assign_spills(void) {
    int i;
    int r;

    i = 0;
    while (i < h_ninst) {
        ra_spill_off[i] = 0;
        i = i + 1;
    }

    /* Assign 8-byte spill slots for non-allocated value-producing instructions */
    i = 0;
    while (i < h_ninst) {
        if (ra_reg[i] < 0 && h_kind[i] != HI_NOP &&
            hi_has_value(h_kind[i]) && !hi_is_remat(h_kind[i])) {
            hl_temp_stack = hl_temp_stack + 8;
            ra_spill_off[i] = 0 - hl_temp_stack;
            ra_stat_spills = ra_stat_spills + 1;
        }
        i = i + 1;
    }

    /* Assign 8-byte callee-save slots */
    ra_ncsave = 0;
    r = 0;
    while (r < RA_NPHY) {
        if (ra_used[r]) {
            hl_temp_stack = hl_temp_stack + 8;
            ra_csave_reg[ra_ncsave] = ra_x64_phys[r];
            ra_csave_off[ra_ncsave] = 0 - hl_temp_stack;
            ra_ncsave = ra_ncsave + 1;
        }
        r = r + 1;
    }
}

/* =================================================================
 * Main entry point (no compare-branch fusion for x64 yet)
 * ================================================================= */

static void hir_regalloc(void) {
    ra_init_x64_regs();
    ra_compute_pos();
    ra_compute_ends();
    ra_linear_scan();
    ra_assign_spills();
}

#endif /* HIR_REGALLOC_X64_H */
