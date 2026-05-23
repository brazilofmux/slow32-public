/* hir_regalloc.h -- IRC graph-coloring register allocation for s12cc (SLOW-32)
 *
 * Iterated Register Coalescing (George-Appel 1996) for the SLOW-32 selfhost
 * compiler.
 *
 * Current production: 18 callee-saved registers (r11..r28) only.
 * Work in progress on this branch: full classification so the allocator can
 * also use the 8 caller-saved argument registers (r3..r10) for values that
 * do not cross calls — matching the official LLVM SLOW32 backend ABI.
 *
 * No x86-64 clobber constraints, no SIB, no SysV ABI PARAM precoloring.
 * The linear-scan implementation has been replaced by the IRC core below
 * (including the edge-transfer fix, GC_MAX_EDGE overflow guard with force_spill,
 * call-crossing divergence blocking in coalesce, and non-crossing spill bias
 * from the x64/a64 robustness work — e4681a1d / b6f0832f and siblings).
 * Same live-interval linearization, fusion extension, spill assignment, and
 * diagnostic interfaces are preserved.
 *
 * Reference: George-Appel "Iterated Register Coalescing" (1996).
 *
 * Input:  HIR after SSA construction, optimization, and LICM.
 * Output: ra_reg[]   — physical register (r11..r28) per inst, -1 = spilled/remat
 *         ra_spill_off[] — stack offset per inst (negative from fp, 0 = none)
 *         ra_ncsave/ra_csave_reg[]/ra_csave_off[] — callee-save info
 */

/* --- Configuration (SLOW-32) --- */
#define RA_NPHY      18   /* r11..r28 */
#define RA_FIRST_REG 11   /* lowest allocatable register */
#define RA_NCALLEE   18   /* all allocatable registers are callee-saved */

/* --- Full register classification (hard item — disabled until wired) ---
 * Goal: match the official LLVM SLOW32 ABI so short-lived values can use
 * the 8 caller-saved argument registers (r3-r10) instead of forcing every
 * live value into a callee-saved slot.
 *
 *   RA_NCALLEE   = 18  (r11..r28)  — must be saved/restored across calls
 *   RA_NCALLER   =  8  (r3..r10)   — clobbered by calls, cheap for temps
 *   RA_NPHY_ACTIVE = 18 + ra_caller_saved_enabled_count (0 during bring-up)
 *   RA_NPHY_TOTAL  = 26            — maximum pool size
 *
 * The knob ra_caller_saved_enabled_count starts at 0 so the tree remains
 * bit-for-bit identical to the committed 9117dec5 baseline.
 */
#define RA_NCALLER   8
/* Stage06 selfhost parser only accepts a bare integer literal as an array
 * dimension (parser.h:1496/1918 — no parenthesized expressions).  Hard-code
 * the sum so `static int ra_phys_reg[RA_NPHY_TOTAL]` below parses under the
 * bootstrap compiler.  Keep in sync with RA_NCALLEE + RA_NCALLER above. */
#define RA_NPHY_TOTAL 26   /* RA_NCALLEE (18) + RA_NCALLER (8) */

/* Knob: how many caller-saved registers the allocator may use right now.
 * 0 = current production behavior (18 callee only).
 * 8 = full pool (r3-r10 + r11-r28) for values that do not cross calls.
 */
static int ra_caller_saved_enabled_count = 8;  /* 0 = baseline (18 callee-saved only). 8 = enable r3-r10 for non-call-crossing values. */

/* Cross-call liveness tracking — must be declared extremely early because
 * ra_prefers_caller_for_inst() (and other early helpers) reference it in
 * this single translation unit. */
static int ra_crosses_call[HIR_MAX_INST];

/* Physical register table and classification (populated by ra_init_phys_regs).
 * Index 0..17  → r11..r28 (callee)
 * Index 18..25 → r3..r10  (caller)
 */
static int ra_phys_reg[RA_NPHY_TOTAL];
static int ra_is_callee[RA_NPHY_TOTAL];

/* Initialize the physical register map and callee/caller classification.
 * Called once at startup of the allocator (or lazily on first use).
 * This is the single source of truth for "which physicals are callee-saved".
 */
static void ra_init_phys_regs(void) {
    static int inited = 0;
    int i;

    if (inited) return;
    inited = 1;

    /* Callee-saved pool: r11..r28 (indices 0..17) */
    for (i = 0; i < RA_NCALLEE; i = i + 1) {
        ra_phys_reg[i] = RA_FIRST_REG + i;   /* 11 .. 28 */
        ra_is_callee[i] = 1;
    }

    /* Caller-saved pool: r3..r10 (indices 18..25) */
    for (i = 0; i < RA_NCALLER; i = i + 1) {
        ra_phys_reg[RA_NCALLEE + i] = 3 + i; /* 3 .. 10 */
        ra_is_callee[RA_NCALLEE + i] = 0;
    }
}

/* Query helpers (safe even when knob == 0) */
static int ra_num_callee_saved(void) {
    return RA_NCALLEE;
}

static int ra_num_caller_saved_enabled(void) {
    return ra_caller_saved_enabled_count;
}

static int ra_num_active_slots(void) {
    return RA_NCALLEE + ra_caller_saved_enabled_count;
}

static int ra_phys_is_callee_saved(int phys) {
    int i;
    ra_init_phys_regs();
    i = 0;
    while (i < RA_NPHY_TOTAL) {
        if (ra_phys_reg[i] == phys) return ra_is_callee[i];
        i = i + 1;
    }
    return 1; /* conservative: unknown phys → treat as callee */
}

static int ra_phys_is_caller_saved(int phys) {
    return !ra_phys_is_callee_saved(phys);
}

/* Map an allocator "slot" (0..active-1) to its physical register number.
 * When knob==0 this is exactly the old 0..17 → r11..r28 mapping.
 */
static int ra_get_phys(int slot) {
    ra_init_phys_regs();
    if (slot < 0 || slot >= ra_num_active_slots()) return -1;
    return ra_phys_reg[slot];
}

/* Returns true if the given HIR opcode can usefully reuse the color of
 * its src1 operand for the result.  Covers the main destructive integer
 * ops plus the dominant immediate form (ADDI).
 */
static int ra_can_reuse_src1(int k) {
    return (k >= HI_ADD && k <= HI_SRL) || k == HI_ADDI;
}

/* Returns true if we are allowed to give this instruction a caller-saved
 * register (r3-r10) on this run.
 *
 * With ra_caller_saved_enabled_count=8 (the default), this returns true for
 * values whose live range does not cross any call (computed by the active-set
 * walk in ra_mark_call_crossing).  Such values may be allocated from the
 * cheap r3-r10 pool instead of forcing everything into r11-r28.
 */
static int ra_prefers_caller_for_inst(int inst) {
    if (ra_caller_saved_enabled_count == 0) return 0;
    if (inst < 0 || inst >= h_ninst) return 0;

    /* Real call-crossing data is now available (ra_mark_call_crossing ran).
     * Values whose live range does *not* cross any call are allowed to use
     * the cheap caller-saved registers (r3-r10).  Values that cross calls
     * must stay in the callee-saved pool (r11-r28). */
    return !ra_crosses_call[inst];
}

/* Returns the allocator color (slot 0..active-1) whose physical register
 * matches the ABI incoming register for this HI_PARAM (i.e. r3 + h_val[inst]),
 * if that register is currently part of the enabled pool *and* the value is
 * allowed to live there under the current crossing classification.
 *
 * When gc_select takes this color, the HI_PARAM emission in hir_codegen
 * produces `addi rd, rd, 0`, which hcg_mov already elides.  Net effect:
 * zero-copy parameter entry for the common non-call-crossing / leaf case.
 *
 * Crossing params will normally not get their preferred (caller) reg because
 * they are forced into the callee range; they pay the expected move into a
 * callee-saved register. */
static int ra_param_preferred_color(int inst) {
    int phys, slot, nactive;

    if (inst < 0 || inst >= h_ninst) return -1;
    if (h_kind[inst] != HI_PARAM) return -1;
    /* Stack-passed args (phys_idx >= 8) have no incoming register: the
     * codegen materialises them with a fp-relative LOAD, so the param
     * can be placed in any free color the allocator chooses. */
    if (h_val[inst] >= 8) return -1;

    phys = 3 + h_val[inst];
    ra_init_phys_regs();
    nactive = ra_num_active_slots();

    slot = 0;
    while (slot < nactive) {
        if (ra_phys_reg[slot] == phys) {
            if (ra_is_callee[slot]) {
                /* Callee slot — always legal for any value */
                return slot;
            }
            /* Caller slot — only if this value is allowed in the caller pool */
            if (ra_prefers_caller_for_inst(inst)) return slot;
            return -1;
        }
        slot = slot + 1;
    }
    return -1;
}

/* --- Output arrays --- */
static int ra_reg[HIR_MAX_INST];    /* physical register, -1 = spilled/remat */
static int ra_spill_off[HIR_MAX_INST]; /* spill slot (negative fp offset), 0 = none */

/* --- Live interval arrays --- */
static int ra_pos[HIR_MAX_INST];    /* linearized position */
static int ra_iend[HIR_MAX_INST];   /* last use position (end of live interval) */

/* --- Linearized order --- */
static int ra_order[HIR_MAX_INST];  /* instruction indices in position order */
static int ra_norder;

/* --- Callee-save tracking --- */
static int ra_used[RA_NPHY_TOTAL];  /* 1 if register (slot) was assigned; sized for full pool */
static int ra_csave_reg[RA_NPHY];   /* physical register number (still only callee-saved get slots) */
static int ra_csave_off[RA_NPHY];   /* fp offset for save slot */
static int ra_ncsave;               /* count of registers to save */
static int ra_csave_bytes;          /* bytes of frame charged for csave slots; tail of hl_temp_stack
                                     * after ra_assign_spills.  Read by leaf-frame reclaim in
                                     * hir_codegen instead of recomputing ra_ncsave*4 inline. */
static int ra_stat_spills;          /* cumulative spill count across all functions */
static int ra_stat_caller_used;     /* how many values got caller-saved registers */
static int ra_stat_callee_used;     /* how many values got callee-saved registers */
static int ra_stat_param_preferred; /* how many HI_PARAM nodes got their ABI incoming reg as color (zero-copy entry) */
static int ra_stat_operand_reuse;   /* binary ops whose result reused an operand's physical register
                                     * (src1 for destructive ops, or src1/src2 for commutative ops) */
static int ra_stat_src2_reuse;      /* subset of operand_reuse where src2 was the winning operand
                                     * (only meaningful for commutative ops where reordering paid off) */
static int ra_stat_secondary_reuse; /* times the secondary/smart operand choice actually supplied the color */
static int ra_stat_imm_base_reuse;  /* HI_ADDI results that reused their base register's color */

/* (ra_crosses_* arrays moved to the very top of the classification section
 * for single-TU declaration ordering.) */

/* =================================================================
 * IRC data structures (Chunk 3 — dead code until wiring step)
 * ================================================================= */

#define GC_MAX_NODE  4096
#define GC_MAX_EDGE  262144   /* lowered for selfhost toolchain (BSS + assembler limits) */
#define GC_MAX_MOVE  8192

static int gc_nnode;
static int gc_inst[GC_MAX_NODE];
static int gc_node[HIR_MAX_INST];

static int gc_adj_head[GC_MAX_NODE];
static int gc_adj_peer[GC_MAX_EDGE];
static int gc_adj_next[GC_MAX_EDGE];
static int gc_nedge;

static int gc_degree[GC_MAX_NODE];

static int gc_mv_a[GC_MAX_MOVE];
static int gc_mv_b[GC_MAX_MOVE];
static int gc_nmove;

#define GC_MV_WORKLIST    0
#define GC_MV_ACTIVE      1
#define GC_MV_COALESCED   2
#define GC_MV_FROZEN      3
#define GC_MV_CONSTRAINED 4
static int gc_mv_status[GC_MAX_MOVE];

#define GC_MAX_NMLIST 16384   /* lowered for selfhost */
static int gc_nmlist_mv[GC_MAX_NMLIST];
static int gc_nmlist_next[GC_MAX_NMLIST];
static int gc_nmlist_head[GC_MAX_NODE];
static int gc_nnmlist;

#define GC_WL_SIMPLIFY  0
#define GC_WL_FREEZE    1
#define GC_WL_SPILL     2
#define GC_WL_COALESCED 3
#define GC_WL_SELECT    4
#define GC_WL_COLORED   5
static int gc_wl[GC_MAX_NODE];

static int gc_sel_stk[GC_MAX_NODE];
static int gc_nsel;

static int gc_alias[GC_MAX_NODE];
static int gc_color[GC_MAX_NODE];
static int gc_force_spill[GC_MAX_NODE];

/* ra_crosses_call is the only live cross tracking for SLOW-32 (caller-saved classification).
 * The x64 clobber-position lists and ra_mark_clobbers were dead after the IRC port and have
 * been removed.  See gc_add_edge for the GC_MAX_EDGE overflow guard (force_spill). */

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

        /* LICM-hoisted instructions (after regular, before terminator) */
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

/* --- Cross-block liveness propagation ---
 *
 * When a value is defined in one block and used in another, we must
 * extend its live interval through ALL blocks on any path from the
 * def block to the use block.  This is critical for loops: if a value
 * is defined before a loop and used inside it, the back-edge creates
 * a path that keeps the value alive through the entire loop body.
 *
 * Algorithm: BFS backward through CFG predecessors from the use block
 * to the def block, extending the value's interval to the end of every
 * intermediate block (and the use block and def block themselves).
 */

static int ra_blk_last[HIR_MAX_BLOCK]; /* last position in each block */
static int ra_bvis[HIR_MAX_BLOCK];     /* visited flags for BFS */
static int ra_bwl[HIR_MAX_BLOCK];      /* BFS worklist */

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

    /* Start BFS from use_blk — extend val to end of use_blk */
    ra_bvis[use_blk] = 1;
    ra_extend(val, ra_blk_last[use_blk]);
    ra_bwl[0] = use_blk;
    wh = 0;
    wt = 1;

    while (wh < wt) {
        b = ra_bwl[wh];
        wh = wh + 1;

        /* Walk predecessors of b */
        j = 0;
        while (j < ssa_npred[b]) {
            p = ssa_pred[ssa_pbase[b] + j];
            if (p >= 0 && p < bb_nblk && !ra_bvis[p]) {
                ra_bvis[p] = 1;
                ra_extend(val, ra_blk_last[p]);
                if (p != def_blk && wt < HIR_MAX_BLOCK) {
                    /* Continue BFS past intermediate blocks, stop at def */
                    ra_bwl[wt] = p;
                    wt = wt + 1;
                }
            }
            j = j + 1;
        }
    }
}

static int ra_is_i12(int v) {
    return (v >= -2048 && v <= 2047);
}

static int ra_codegen_fold_base(int inst, int *base_out) {
    int k;
    int s1;
    int pat;
    int lnt;
    int chain;
    int ck;
    int chlim;
    int off;

    k = h_kind[inst];
    s1 = h_src1[inst];
    pat = bg_sel[inst];
    lnt = -1;
    if (pat >= 0) {
        lnt = bg_plnt[pat];
    } else if (s1 >= 0 && h_kind[s1] == HI_ALLOCA) {
        lnt = BG_FADDR;
    }

    if (k == HI_LOAD || k == HI_STORE) {
        if (lnt == BG_FADDR || lnt == BG_SADDR) return 0;

        chain = s1;
        off = 0;
        chlim = 0;
        while (chain >= 0 && chlim < 64) {
            ck = h_kind[chain];
            if (ck == HI_COPY) {
                chain = h_src1[chain];
                chlim = chlim + 1;
                continue;
            }
            if (ck == HI_ADDI) {
                off = off + h_val[chain];
                chain = h_src1[chain];
                chlim = chlim + 1;
                continue;
            }
            break;
        }
        if (chain >= 0 && chain != s1 && ra_is_i12(off)) {
            *base_out = chain;
            return 1;
        }
        return 0;
    }

    if (k == HI_ADDI) {
        if (lnt == BG_FADDR) return 0;

        chain = s1;
        chlim = 0;
        while (chain >= 0 && h_kind[chain] == HI_ADDI && chlim < 64) {
            chain = h_src1[chain];
            chlim = chlim + 1;
        }
        if (chain >= 0 && chain != s1) {
            *base_out = chain;
            return 1;
        }
    }

    return 0;
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

    /* Compute last position in each block (for cross-block extension) */
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
            ra_blk_last[b] = ra_pos[inst];  /* last one wins */
        }
        i = i + 1;
    }

    /* Scan all instructions for uses */
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];
        p = ra_pos[inst];

        /* src1 is always an instruction reference when >= 0 */
        ra_extend(h_src1[inst], p);

        /* src2 is an instruction ref for binops and STORE */
        if (h_src2[inst] >= 0 && ho_src2_is_ref(k)) {
            ra_extend(h_src2[inst], p);
        }

        /* Codegen fold-through: codegen may emit a load/store/addi
         * using a folded chain's base register rather than s1's
         * register.  Extend exactly the base that codegen will read.
         *
         * Without this, n's interval (the deepest base) ends at its
         * last *direct* use, but a later folded LOAD that reaches
         * through an ADDI(n,52) still emits `ldw rd, r_n, 52` —
         * reading a clobbered register.  See Issue #31. */
        if (ra_codegen_fold_base(inst, &src)) {
            ra_extend(src, p);
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
                    /* Use point is at the terminator of the predecessor.
                     * Find the last non-NOP instruction in pred block. */
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

    /* Cross-block liveness propagation: for each operand used in a
     * different block than its definition, backward-propagate through
     * CFG predecessors to extend the live interval through all
     * intermediate blocks.  This handles loops correctly because the
     * BFS follows back-edges. */
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];

        /* src1 */
        src = h_src1[inst];
        if (src >= 0 && ra_pos[src] >= 0 && h_blk[src] != h_blk[inst]) {
            ra_backprop(src, h_blk[inst]);
        }

        /* src2 */
        if (h_src2[inst] >= 0 && ho_src2_is_ref(k)) {
            src = h_src2[inst];
            if (ra_pos[src] >= 0 && h_blk[src] != h_blk[inst]) {
                ra_backprop(src, h_blk[inst]);
            }
        }

        /* Codegen fold-through: mirror the forward extend above. */
        if (ra_codegen_fold_base(inst, &src) && ra_pos[src] >= 0 &&
            h_blk[src] != h_blk[inst]) {
            ra_backprop(src, h_blk[inst]);
        }

        /* Call arguments */
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

        /* PHI arguments: used at end of predecessor block */
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
 * Step 3: Spill / callee-save slot assignment
 * (Used by the IRC allocator)
 * ================================================================= */

static void ra_assign_spills(void) {
    int i;
    int r;

    /* Initialize all spill offsets to 0 */
    i = 0;
    while (i < h_ninst) {
        ra_spill_off[i] = 0;
        i = i + 1;
    }

    /* Assign spill slots for non-allocated, non-remat value-producing instructions */
    i = 0;
    while (i < h_ninst) {
        if (ra_reg[i] < 0 && h_kind[i] != HI_NOP &&
            hi_has_value(h_kind[i]) && !hi_is_remat(h_kind[i])) {
            hl_temp_stack = hl_temp_stack + 4;
            ra_spill_off[i] = 0 - hl_temp_stack;
            ra_stat_spills = ra_stat_spills + 1;
        }
        i = i + 1;
    }

    /* Assign callee-save slots.
     * Only colors 0 .. RA_NCALLEE-1 (r11..r28) ever need to be saved/restored.
     * Caller-saved colors (when enabled) are deliberately excluded — that is
     * the whole point of the classification work.
     *
     * Track the bytes we charge here in ra_csave_bytes so the leaf-frame
     * reclaim in hir_codegen can subtract them by name instead of assuming
     * the layout (csave block is the strict tail of hl_temp_stack here). */
    ra_ncsave = 0;
    ra_csave_bytes = 0;
    r = 0;
    while (r < RA_NCALLEE) {
        if (ra_used[r]) {
            hl_temp_stack = hl_temp_stack + 4;
            ra_csave_bytes = ra_csave_bytes + 4;
            ra_csave_reg[ra_ncsave] = ra_get_phys(r);  /* r11..r28 */
            ra_csave_off[ra_ncsave] = 0 - hl_temp_stack;
            ra_ncsave = ra_ncsave + 1;
        }
        r = r + 1;
    }
}

/* =================================================================
 * Step 4b: Extend live ranges for compare-and-branch fusion
 *
 * For each BRC that fuses a comparison, extend the comparison's
 * operands to the BRC position so they remain in registers.
 * Then NOP the comparison so regalloc doesn't allocate for it.
 * ================================================================= */

static void ra_extend_fused_cmp(void) {
    int i;
    int cmp;
    int ca;
    int cb;
    int brc_pos;
    int c;
    int lim;

    i = 0;
    while (i < h_ninst) {
        cmp = hcg_brc_fuse[i];
        if (cmp < 0) { i = i + 1; continue; }

        brc_pos = ra_pos[i];
        if (brc_pos < 0) {
            hcg_brc_fuse[i] = -1;
            i = i + 1;
            continue;
        }

        ca = h_src1[cmp];
        cb = h_src2[cmp];

        /* Extend comparison operand live ranges to BRC position */
        ra_extend(ca, brc_pos);
        ra_extend(cb, brc_pos);

        /* NOP the comparison so regalloc skips it */
        h_kind[cmp] = HI_NOP;

        /* NOP any intermediate COPYs between BRC and the comparison */
        c = h_src1[i];
        lim = 0;
        while (c >= 0 && c != cmp && lim < 64) {
            if (hcg_cmp_fused[c]) h_kind[c] = HI_NOP;
            c = h_src1[c];
            lim = lim + 1;
        }

        i = i + 1;
    }
}

/* =================================================================
 * Main entry point
 * ================================================================= */

/* =================================================================
 * IRC small helpers (Chunk 3 — dead code)
 * ================================================================= */

static int gc_has_edge(int u, int v) {
    int e;
    e = gc_adj_head[u];
    while (e >= 0) {
        if (gc_adj_peer[e] == v) return 1;
        e = gc_adj_next[e];
    }
    return 0;
}

static void gc_add_edge(int u, int v) {
    int e1, e2;
    if (u == v) return;
    if (gc_has_edge(u, v)) return;
    if (gc_nedge + 2 > GC_MAX_EDGE) {
        /* Can't record this interference — mark both nodes as force-spill
         * so they get spill slots instead of potentially sharing a color
         * with the unrecorded interferer.  Without this guard a silent drop
         * here can cause multiple call arguments (or other live values) to
         * be assigned the same physical register, leading to corruption
         * before the call.  See b6f0832f (IRC graph-edge overflow fix). */
        if (u >= 0 && u < GC_MAX_NODE) gc_force_spill[u] = 1;
        if (v >= 0 && v < GC_MAX_NODE) gc_force_spill[v] = 1;
        return;
    }
    e1 = gc_nedge;
    gc_adj_peer[e1] = v;
    gc_adj_next[e1] = gc_adj_head[u];
    gc_adj_head[u] = e1;
    gc_nedge = gc_nedge + 1;

    e2 = gc_nedge;
    gc_adj_peer[e2] = u;
    gc_adj_next[e2] = gc_adj_head[v];
    gc_adj_head[v] = e2;
    gc_nedge = gc_nedge + 1;

    gc_degree[u] = gc_degree[u] + 1;
    gc_degree[v] = gc_degree[v] + 1;
}

static void gc_add_node_move(int node, int mv) {
    int idx;
    if (gc_nnmlist >= GC_MAX_NMLIST) return;
    idx = gc_nnmlist;
    gc_nnmlist = gc_nnmlist + 1;
    gc_nmlist_mv[idx] = mv;
    gc_nmlist_next[idx] = gc_nmlist_head[node];
    gc_nmlist_head[node] = idx;
}

static int gc_move_related(int n) {
    int e, mv, st;
    e = gc_nmlist_head[n];
    while (e >= 0) {
        mv = gc_nmlist_mv[e];
        st = gc_mv_status[mv];
        if (st == GC_MV_WORKLIST || st == GC_MV_ACTIVE) return 1;
        e = gc_nmlist_next[e];
    }
    return 0;
}

static int gc_get_alias(int n) {
    int lim;
    lim = 0;
    while (gc_alias[n] != n && lim < GC_MAX_NODE) {
        n = gc_alias[n];
        lim = lim + 1;
    }
    return n;
}

/* Forward prototype so callers earlier in the TU see the static declaration.
 * Fixes the "static declaration follows non-static" error on some gcc versions
 * (Alpine, certain Debian builds) when building the single-TU s12cc.c. */
static int gc_k(int n);

static void gc_dec_degree(int n) {
    int k;
    gc_degree[n] = gc_degree[n] - 1;
    k = gc_k(n);
    if (gc_degree[n] == k - 1) {
        if (gc_move_related(n)) gc_wl[n] = GC_WL_FREEZE;
        else gc_wl[n] = GC_WL_SIMPLIFY;
    }
}

static int gc_k(int n) {
    (void)n;
    /* Dynamic K: the number of colors (registers) currently available.
     * When ra_caller_saved_enabled_count == 0 this is still 18 (identical
     * to the committed baseline).  When the knob is raised, short-lived
     * values will see K=26 and the allocator can use r3-r10. */
    return ra_num_active_slots();
}

/* =================================================================
 * IRC algorithmic core (Chunk 3 — dead code, C89 declaration style)
 * ================================================================= */

static void gc_add_move(int a, int b) {
    int mv;
    if (gc_nmove >= GC_MAX_MOVE) return;
    mv = gc_nmove;
    gc_mv_a[mv] = a;
    gc_mv_b[mv] = b;
    gc_mv_status[mv] = GC_MV_WORKLIST;
    gc_nmove = gc_nmove + 1;
    gc_add_node_move(a, mv);
    gc_add_node_move(b, mv);
}

static void gc_build(void) {
    int i, j, inst, k, n, ni, aj, nact, p;
    int act[GC_MAX_NODE];

    gc_nnode = 0;
    gc_nedge = 0;
    gc_nmove = 0;
    gc_nnmlist = 0;
    gc_nsel = 0;

    i = 0;
    while (i < h_ninst) { gc_node[i] = -1; i = i + 1; }
    i = 0;
    while (i < GC_MAX_NODE) { gc_force_spill[i] = 0; i = i + 1; }

    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];
        if (hi_has_value(k) && !hi_is_remat(k) && k != HI_NOP) {
            if (gc_nnode < GC_MAX_NODE) {
                n = gc_nnode;
                gc_inst[n] = inst;
                gc_node[inst] = n;
                gc_adj_head[n] = -1;
                gc_degree[n] = 0;
                gc_alias[n] = n;
                gc_color[n] = -1;
                gc_nmlist_head[n] = -1;
                gc_nnode = gc_nnode + 1;
            }
        }
        i = i + 1;
    }

    nact = 0;
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        p = ra_pos[inst];
        ni = gc_node[inst];

        j = 0;
        while (j < nact) {
            aj = act[j];
            if (ra_iend[gc_inst[aj]] < p) {
                nact = nact - 1;
                act[j] = act[nact];
            } else {
                j = j + 1;
            }
        }

        if (ni >= 0) {
            j = 0;
            while (j < nact) {
                gc_add_edge(ni, act[j]);
                j = j + 1;
            }
            act[nact] = ni;
            nact = nact + 1;
        }
        i = i + 1;
    }
}

static void gc_find_moves(void) {
    int i, inst, k, s1, nd, ns1, j, a, na;

    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];
        nd = gc_node[inst];
        if (nd < 0) { i = i + 1; continue; }

        if (k == HI_COPY) {
            s1 = h_src1[inst];
            if (s1 >= 0) {
                ns1 = gc_node[s1];
                if (ns1 >= 0) gc_add_move(nd, ns1);
            }
        }
        if (k >= HI_ADD && k <= HI_SRL) {
            s1 = h_src1[inst];
            if (s1 >= 0) {
                ns1 = gc_node[s1];
                if (ns1 >= 0) gc_add_move(nd, ns1);
            }
        }
        if (k == HI_PHI && h_pbase[inst] >= 0) {
            j = 0;
            while (j < h_pcnt[inst]) {
                a = h_pval[h_pbase[inst] + j];
                if (a >= 0) {
                    na = gc_node[a];
                    if (na >= 0) gc_add_move(nd, na);
                }
                j = j + 1;
            }
        }
        i = i + 1;
    }
}

static void gc_make_worklists(void) {
    int n, k;
    n = 0;
    while (n < gc_nnode) {
        k = gc_k(n);
        if (gc_degree[n] >= k) gc_wl[n] = GC_WL_SPILL;
        else if (gc_move_related(n)) gc_wl[n] = GC_WL_FREEZE;
        else gc_wl[n] = GC_WL_SIMPLIFY;
        n = n + 1;
    }
}

static int gc_simplify(void) {
    int n, e, peer;
    n = 0;
    while (n < gc_nnode) {
        if (gc_wl[n] == GC_WL_SIMPLIFY) {
            gc_wl[n] = GC_WL_SELECT;
            gc_sel_stk[gc_nsel] = n;
            gc_nsel = gc_nsel + 1;
            e = gc_adj_head[n];
            while (e >= 0) {
                peer = gc_adj_peer[e];
                if (gc_wl[peer] != GC_WL_SELECT && gc_wl[peer] != GC_WL_COALESCED) {
                    gc_dec_degree(peer);
                }
                e = gc_adj_next[e];
            }
            return 1;
        }
        n = n + 1;
    }
    return 0;
}

static int gc_george(int u, int v) {
    int e, t, kk;
    e = gc_adj_head[v];
    while (e >= 0) {
        t = gc_adj_peer[e];
        if (gc_wl[t] != GC_WL_SELECT && gc_wl[t] != GC_WL_COALESCED) {
            kk = gc_k(t);
            if (gc_degree[t] >= kk && !gc_has_edge(t, u)) return 0;
        }
        e = gc_adj_next[e];
    }
    return 1;
}

static int gc_briggs(int u, int v) {
    int count, kk, e, t;
    count = 0;
    kk = gc_k(u);
    if (gc_k(v) < kk) kk = gc_k(v);

    e = gc_adj_head[u];
    while (e >= 0) {
        t = gc_adj_peer[e];
        if (gc_wl[t] != GC_WL_SELECT && gc_wl[t] != GC_WL_COALESCED && t != v) {
            if (gc_degree[t] >= kk) count = count + 1;
        }
        e = gc_adj_next[e];
    }
    e = gc_adj_head[v];
    while (e >= 0) {
        t = gc_adj_peer[e];
        if (gc_wl[t] != GC_WL_SELECT && gc_wl[t] != GC_WL_COALESCED && t != u) {
            if (gc_degree[t] >= kk && !gc_has_edge(t, u)) count = count + 1;
        }
        e = gc_adj_next[e];
    }
    return count < kk;
}

static void gc_combine(int u, int v) {
    int e, t;
    gc_wl[v] = GC_WL_COALESCED;
    gc_alias[v] = u;

    e = gc_nmlist_head[v];
    while (e >= 0) {
        gc_add_node_move(u, gc_nmlist_mv[e]);
        e = gc_nmlist_next[e];
    }

    /* Add edges: for each neighbor t of v, add edge(t, u).
     *
     * Always transfer the edge — even when t is on the select stack —
     * because t's color is consulted later via u's adjacency when u
     * itself is colored.  Without the transfer u's adj loses the
     * "must not pick t's color" constraint and can clash with t.
     *
     * Latent under traditional reverse-of-push pop order (high index
     * first, so u colors before t): the asymmetric edge still on t's
     * side via the alias was sufficient.  The two-pass select that
     * colors PARAMs first (or any ordering change) surfaces the bug.
     * This fix (e4681a1d) was ported into the SLOW-32 selfhost IRC. */
    e = gc_adj_head[v];
    while (e >= 0) {
        t = gc_adj_peer[e];
        if (gc_wl[t] != GC_WL_COALESCED) {
            gc_add_edge(t, u);
            if (gc_wl[t] != GC_WL_SELECT) gc_dec_degree(t);
        }
        e = gc_adj_next[e];
    }

    if (gc_degree[u] >= gc_k(u) && gc_wl[u] == GC_WL_FREEZE) {
        gc_wl[u] = GC_WL_SPILL;
    }
}

static int gc_coalesce(void) {
    int mv, u, v, tmp;
    mv = 0;
    while (mv < gc_nmove) {
        if (gc_mv_status[mv] != GC_MV_WORKLIST) { mv = mv + 1; continue; }

        u = gc_get_alias(gc_mv_a[mv]);
        v = gc_get_alias(gc_mv_b[mv]);

        if (ra_crosses_call[gc_inst[v]] && !ra_crosses_call[gc_inst[u]]) {
            tmp = u; u = v; v = tmp;
        }

        if (u == v) {
            gc_mv_status[mv] = GC_MV_COALESCED;
            if (!gc_move_related(u) && gc_degree[u] < gc_k(u) && gc_wl[u] == GC_WL_FREEZE) {
                gc_wl[u] = GC_WL_SIMPLIFY;
            }
            return 1;
        }
        if (gc_has_edge(u, v)) {
            gc_mv_status[mv] = GC_MV_CONSTRAINED;
            if (!gc_move_related(u) && gc_degree[u] < gc_k(u) && gc_wl[u] == GC_WL_FREEZE)
                gc_wl[u] = GC_WL_SIMPLIFY;
            if (!gc_move_related(v) && gc_degree[v] < gc_k(v) && gc_wl[v] == GC_WL_FREEZE)
                gc_wl[v] = GC_WL_SIMPLIFY;
            return 1;
        }
        if (ra_crosses_call[gc_inst[u]] != ra_crosses_call[gc_inst[v]]) {
            gc_mv_status[mv] = GC_MV_CONSTRAINED;
            return 1;
        }
        if (gc_george(u, v) || gc_briggs(u, v)) {
            gc_mv_status[mv] = GC_MV_COALESCED;
            gc_combine(u, v);
            if (!gc_move_related(u) && gc_degree[u] < gc_k(u)) {
                if (gc_wl[u] == GC_WL_FREEZE) gc_wl[u] = GC_WL_SIMPLIFY;
            }
            return 1;
        }
        gc_mv_status[mv] = GC_MV_ACTIVE;
        return 1;
    }
    return 0;
}

static int gc_freeze(void) {
    int n, e, mv, other;
    n = 0;
    while (n < gc_nnode) {
        if (gc_wl[n] == GC_WL_FREEZE) {
            gc_wl[n] = GC_WL_SIMPLIFY;
            e = gc_nmlist_head[n];
            while (e >= 0) {
                mv = gc_nmlist_mv[e];
                if (gc_mv_status[mv] == GC_MV_WORKLIST || gc_mv_status[mv] == GC_MV_ACTIVE) {
                    gc_mv_status[mv] = GC_MV_FROZEN;
                    other = gc_get_alias(gc_mv_a[mv]);
                    if (other == n) other = gc_get_alias(gc_mv_b[mv]);
                    if (!gc_move_related(other) && gc_degree[other] < gc_k(other) &&
                        gc_wl[other] == GC_WL_FREEZE) {
                        gc_wl[other] = GC_WL_SIMPLIFY;
                    }
                }
                e = gc_nmlist_next[e];
            }
            return 1;
        }
        n = n + 1;
    }
    return 0;
}

static int gc_select_spill(void) {
    int n, best, best_cost, cost, inst;
    best = -1;
    best_cost = 0x7FFFFFFF;

    n = 0;
    while (n < gc_nnode) {
        if (gc_wl[n] == GC_WL_SPILL) {
            inst = gc_inst[n];
            /* cost = uses * 100 / (degree + 1).  Lower = cheaper to spill.
             * Non-call-crossing values get a +50 penalty in the cost (making
             * them *less* likely to be chosen for spill) because they have
             * access to the larger color pool (caller + callee when knob=8)
             * and are therefore easier to color successfully if left in simplify. */
            cost = (bg_uses[inst] * 100) / (gc_degree[n] + 1);
            if (!ra_crosses_call[inst]) cost = cost + 50;
            if (cost < best_cost) {
                best_cost = cost;
                best = n;
            }
        }
        n = n + 1;
    }
    if (best < 0) return 0;

    /* Optimistic spill: move the chosen node back to simplify and let the
     * main IRC loop try to color it later (after more coalescing/simplification).
     * Its moves are frozen so we stop trying to coalesce them. */
    gc_wl[best] = GC_WL_SIMPLIFY;

    {
        int e, mv, other;
        e = gc_nmlist_head[best];
        while (e >= 0) {
            mv = gc_nmlist_mv[e];
            if (gc_mv_status[mv] == GC_MV_WORKLIST || gc_mv_status[mv] == GC_MV_ACTIVE) {
                gc_mv_status[mv] = GC_MV_FROZEN;
                other = gc_get_alias(gc_mv_a[mv]);
                if (other == best) other = gc_get_alias(gc_mv_b[mv]);
                if (!gc_move_related(other) && gc_degree[other] < gc_k(other) &&
                    gc_wl[other] == GC_WL_FREEZE) {
                    gc_wl[other] = GC_WL_SIMPLIFY;
                }
            }
            e = gc_nmlist_next[e];
        }
    }
    return 1;
}

static void gc_irc(void) {
    int progress;
    gc_make_worklists();
    progress = 1;
    while (progress) {
        progress = 0;
        if (gc_simplify()) { progress = 1; continue; }
        if (gc_coalesce()) { progress = 1; continue; }
        if (gc_freeze())   { progress = 1; continue; }
        if (gc_select_spill()) { progress = 1; continue; }
    }
    /* When the loop terminates we have either colored everything or
     * the remaining spill worklist nodes will be forced to spill in
     * gc_select / gc_writeback via gc_force_spill or degree >= K. */
}

static void gc_select(void) {
    int i, n, inst, e, peer, pa, pc, c;
    int used[RA_NPHY_TOTAL];
    int maxc;
    int pass;

    /* Two-pass color: PARAMs first, then everything else.
     *
     * Why: gc_simplify pushes nodes in gc_node-INDEX order (low index
     * first), so the select-stack pops them with high index first and
     * low index last.  PARAM nodes are typically created early (low
     * gc_node index) in hir_lower and therefore pop LAST under the
     * natural single-pass order — by which point temporaries have
     * already taken slots in the cheap r3-r10 caller pool.
     *
     * This defeats the PARAM preferred-color bias we just added.
     *
     * Fix: walk the select stack twice.  First pass colors only
     * HI_PARAM nodes (seeding their exact ABI registers r3+N when
     * possible).  Second pass colors the rest.  The used[] mask from
     * the first pass naturally protects the preferred registers for
     * the second pass.
     *
     * Pop direction (high index first) is unchanged within each pass.
     * This is the same technique used in the x64 and a64 backends
     * after the edge-transfer robustness work.
     */
    pass = 0;
    while (pass < 2) {

        i = gc_nsel - 1;
        while (i >= 0) {
            n = gc_sel_stk[i];
            inst = gc_inst[n];
            maxc = ra_num_active_slots();

            int k = h_kind[inst];

            /* Skip nodes that do not belong in this pass */
            if (pass == 0 && h_kind[inst] != HI_PARAM) { i = i - 1; continue; }
            if (pass == 1 && h_kind[inst] == HI_PARAM) { i = i - 1; continue; }

            /* Zero only the active portion of the used[] mask */
            c = 0;
            while (c < maxc) { used[c] = 0; c = c + 1; }

            e = gc_adj_head[n];
            while (e >= 0) {
                peer = gc_adj_peer[e];
                pa = gc_get_alias(peer);
                pc = gc_color[pa];
                if (pc >= 0 && pc < maxc) used[pc] = 1;
                e = gc_adj_next[e];
            }

            gc_color[n] = -1;

            /* Src1 reuse for destructive binary ops.
             * For ADD, SUB, AND, OR, XOR, shifts, etc. the result can
             * usually live in the same physical register as the first
             * operand.  Trying src1's color first (if free) reduces
             * unnecessary copies in expressions.
             */
            if (gc_color[n] < 0) {
                if (ra_can_reuse_src1(k)) {
                    int s1 = h_src1[inst];
                    if (s1 >= 0) {
                        int s1_node = gc_node[s1];
                        if (s1_node >= 0) {
                            int col = gc_color[s1_node];
                            if (col >= 0 && col < maxc && !used[col]) {
                                gc_color[n] = col;
                                ra_stat_operand_reuse = ra_stat_operand_reuse + 1;
                                if (k == HI_ADDI) {
                                    ra_stat_imm_base_reuse = ra_stat_imm_base_reuse + 1;
                                }
                            }
                        }
                    }
                }

                /* Secondary operand preference for commutative ops.
                 * Consider both src1 and src2 and pick the one with the
                 * lowest physical register number (cheap tie-breaker that
                 * tends to keep lower caller registers free).
                 * This runs for all values, not just caller-saved.
                 */
                if (gc_color[n] < 0 &&
                    (k == HI_ADD || k == HI_AND || k == HI_OR || k == HI_XOR))
                {
                    int best_col = -1;
                    int best_phys = 999;
                    int best_is_src2 = 0;

                    // consider src1
                    int s1 = h_src1[inst];
                    if (s1 >= 0) {
                        int s1_node = gc_node[s1];
                        if (s1_node >= 0) {
                            int col = gc_color[s1_node];
                            if (col >= 0 && col < maxc && !used[col]) {
                                int phys = ra_get_phys(col);
                                if (phys < best_phys) {
                                    best_col = col;
                                    best_phys = phys;
                                    best_is_src2 = 0;
                                }
                            }
                        }
                    }

                    // consider src2 (may displace src1 if its phys is lower)
                    int s2 = h_src2[inst];
                    if (s2 >= 0) {
                        int s2_node = gc_node[s2];
                        if (s2_node >= 0) {
                            int col = gc_color[s2_node];
                            if (col >= 0 && col < maxc && !used[col]) {
                                int phys = ra_get_phys(col);
                                if (phys < best_phys) {
                                    best_col = col;
                                    best_phys = phys;
                                    best_is_src2 = 1;
                                }
                            }
                        }
                    }

                    if (best_col != -1) {
                        gc_color[n] = best_col;
                        ra_stat_operand_reuse = ra_stat_operand_reuse + 1;
                        if (best_is_src2) {
                            ra_stat_src2_reuse = ra_stat_src2_reuse + 1;
                        }
                        ra_stat_secondary_reuse = ra_stat_secondary_reuse + 1;
                    }
                }
            }

            /* Two-phase color selection (classification hook) + PARAM bias.
             *
             * Because of the outer two-pass, when we reach a non-PARAM
             * in pass 1 the preferred ABI registers for PARAMs are already
             * marked used, so temporaries naturally avoid stealing them.
             */
            if (ra_prefers_caller_for_inst(inst)) {
                /* Give operand reuse (src1/src2) higher priority in the cheap
                 * caller-saved pool.  When a value is allowed to live in r3-r10,
                 * we first try to keep it there by reusing an operand (with the
                 * lowest-physical bias).  Only if that fails do we fall back to
                 * the PARAM ABI preference.
                 */
                int best_col = -1;
                int best_phys = 999;
                int best_is_src2 = 0;

                if (ra_can_reuse_src1(k)) {
                    int s1 = h_src1[inst];
                    if (s1 >= 0) {
                        int s1_node = gc_node[s1];
                        if (s1_node >= 0) {
                            int col = gc_color[s1_node];
                            if (col >= RA_NCALLEE && col < maxc && !used[col]) {
                                int phys = ra_get_phys(col);
                                if (phys < best_phys) {
                                    best_col = col;
                                    best_phys = phys;
                                    best_is_src2 = 0;
                                }
                            }
                        }
                    }
                }

                if ((k == HI_ADD || k == HI_AND || k == HI_OR || k == HI_XOR)) {
                    int s2 = h_src2[inst];
                    if (s2 >= 0) {
                        int s2_node = gc_node[s2];
                        if (s2_node >= 0) {
                            int col = gc_color[s2_node];
                            if (col >= RA_NCALLEE && col < maxc && !used[col]) {
                                int phys = ra_get_phys(col);
                                if (phys < best_phys) {
                                    best_col = col;
                                    best_phys = phys;
                                    best_is_src2 = 1;
                                }
                            }
                        }
                    }
                }

                if (best_col != -1) {
                    gc_color[n] = best_col;
                    ra_stat_operand_reuse = ra_stat_operand_reuse + 1;
                    if (best_is_src2) {
                        ra_stat_src2_reuse = ra_stat_src2_reuse + 1;
                    }
                    ra_stat_secondary_reuse = ra_stat_secondary_reuse + 1;
                    if (k == HI_ADDI) {
                        /* ADDI is non-commutative — best_is_src2 is always 0 here,
                         * so this fires iff the ADDI base register was actually reused. */
                        ra_stat_imm_base_reuse = ra_stat_imm_base_reuse + 1;
                    }
                }

                /* If operand reuse didn't find anything, fall back to PARAM
                 * preferred color (still in the cheap pool).
                 */
                if (gc_color[n] < 0) {
                    int pref = ra_param_preferred_color(inst);
                    if (pref >= RA_NCALLEE && pref < maxc && !used[pref]) {
                        gc_color[n] = pref;
                        ra_stat_param_preferred = ra_stat_param_preferred + 1;
                    }
                }

                /* Final fallback: generic first-free in caller range,
                 * but with a bias toward operand colors when possible.
                 * This gives the hints one last chance even if the early
                 * strong attempts didn't find a completely free slot.
                 */
                if (gc_color[n] < 0) {
                    /* First, try to pick a free caller color that belongs to
                     * an operand we can reuse (src1, and src2 for commutative).
                     * This is the "biased selection" pass.
                     */
                    int biased = -1;
                    int biased_is_src2 = 0;
                    if (ra_can_reuse_src1(k)) {
                        int s1 = h_src1[inst];
                        if (s1 >= 0) {
                            int s1_node = gc_node[s1];
                            if (s1_node >= 0) {
                                int col = gc_color[s1_node];
                                if (col >= RA_NCALLEE && col < maxc && !used[col]) {
                                    biased = col;
                                    biased_is_src2 = 0;
                                }
                            }
                        }
                    }
                    if (biased < 0 &&
                        (k == HI_ADD || k == HI_AND || k == HI_OR || k == HI_XOR))
                    {
                        int s2 = h_src2[inst];
                        if (s2 >= 0) {
                            int s2_node = gc_node[s2];
                            if (s2_node >= 0) {
                                int col = gc_color[s2_node];
                                if (col >= RA_NCALLEE && col < maxc && !used[col]) {
                                    biased = col;
                                    biased_is_src2 = 1;
                                }
                            }
                        }
                    }

                    if (biased >= 0) {
                        gc_color[n] = biased;
                        ra_stat_operand_reuse = ra_stat_operand_reuse + 1;
                        if (biased_is_src2) {
                            ra_stat_src2_reuse = ra_stat_src2_reuse + 1;
                        }
                        ra_stat_secondary_reuse = ra_stat_secondary_reuse + 1;
                    } else {
                        /* No biased color available — fall back to plain first-free */
                        c = RA_NCALLEE;
                        while (c < maxc) {
                            if (!used[c]) { gc_color[n] = c; break; }
                            c = c + 1;
                        }
                    }
                }
            }

            if (gc_color[n] < 0) {
                int pref = ra_param_preferred_color(inst);
                if (pref >= 0 && pref < RA_NCALLEE && !used[pref]) {
                    gc_color[n] = pref;
                    ra_stat_param_preferred = ra_stat_param_preferred + 1;
                } else {
                    c = 0;
                    while (c < RA_NCALLEE) {
                        if (!used[c]) { gc_color[n] = c; break; }
                        c = c + 1;
                    }
                }
            }

            if (gc_color[n] < 0) gc_wl[n] = GC_WL_SPILL;

            i = i - 1;
        }
        pass = pass + 1;
    }
}

static void gc_writeback(void) {
    int i, n, inst, c, phys;

    i = 0;
    while (i < RA_NPHY_TOTAL) { ra_used[i] = 0; i = i + 1; }
    i = 0;
    while (i < h_ninst) { ra_reg[i] = -1; i = i + 1; }

    n = 0;
    while (n < gc_nnode) {
        inst = gc_inst[n];
        c = gc_color[n];
        if (gc_force_spill[n]) c = -1;

        if (c >= 0) {
            /* Color-to-physical mapping now goes through the classification table.
             * When caller-saved colors (18+) are handed out, ra_get_phys(c) will
             * return r3..r10 instead of the old linear r11.. formula. */
            phys = ra_get_phys(c);
            ra_reg[inst] = phys;
            if (phys >= 0) ra_used[c] = 1;

            /* Record class usage for diagnostics */
            if (c >= RA_NCALLEE)
                ra_stat_caller_used = ra_stat_caller_used + 1;
            else
                ra_stat_callee_used = ra_stat_callee_used + 1;
        }
        n = n + 1;
    }
}

static void gc_alloc(void) {
    gc_build();
    gc_find_moves();
    gc_irc();
    gc_select();
    gc_writeback();
}

static void ra_mark_call_crossing(void) {
    int i, j, inst, k, p, nact;
    int act[HIR_MAX_INST];   /* active instruction indices (the values themselves) */

    /* Clear */
    i = 0;
    while (i < h_ninst) { ra_crosses_call[i] = 0; i = i + 1; }

    nact = 0;
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        p = ra_pos[inst];
        k = h_kind[inst];

        /* Expire anything whose live range ended before this position */
        j = 0;
        while (j < nact) {
            int v = act[j];
            if (ra_iend[v] < p) {
                nact = nact - 1;
                act[j] = act[nact];
            } else {
                j = j + 1;
            }
        }

        /* If this program point is a call, every value still live here
         * crosses the call site and must not be allocated to a caller-saved
         * register (r3-r10 are clobbered by the call). */
        if (k == HI_CALL || k == HI_CALLP || k == HI_CALLHI ||
            k == HI_A64_DBT_TRAMPOLINE || k == HI_X64_DBT_TRAMPOLINE) {

            j = 0;
            while (j < nact) {
                int v = act[j];
                if (v >= 0 && v < h_ninst) {
                    ra_crosses_call[v] = 1;
                }
                j = j + 1;
            }
        }

        /* If this instruction produces a tracked value, add it to the active set.
         * We use the same predicate that gc_build uses. */
        if (hi_has_value(k) && !hi_is_remat(k) && k != HI_NOP) {
            if (nact < HIR_MAX_INST) {
                act[nact] = inst;
                nact = nact + 1;
            }
        }

        i = i + 1;
    }
}

static void hir_regalloc(void) {
    /* SLOW-32 IRC path (George-Appel Iterated Register Coalescing) */
    ra_init_phys_regs();   /* populates classification tables (safe, knob==0 today) */
    ra_compute_pos();
    ra_compute_ends();
    ra_extend_fused_cmp();
    ra_mark_call_crossing();
    /* (ra_mark_clobbers removed — x64 RCX/RDX clobber arrays were never populated or used for SLOW-32) */

    ra_stat_caller_used = 0;
    ra_stat_callee_used = 0;
    ra_stat_param_preferred = 0;
    ra_stat_operand_reuse = 0;
    ra_stat_src2_reuse = 0;
    ra_stat_secondary_reuse = 0;
    ra_stat_imm_base_reuse = 0;

    gc_alloc();
    ra_assign_spills();
}

/* =================================================================
 * Diagnostic dump — enabled by `-d` on the s12cc command line.
 * Emits one line per non-NOP HIR instruction (after regalloc) so two
 * compiler binaries running on the same source can be diff'd to find
 * which inst's live interval / register assignment diverges.
 * ================================================================= */

static void ra_dump_signed(int v) {
    if (v < 0) {
        fdputc(45, 2);   /* '-' */
        fdputuint(2, 0 - v);
    } else {
        fdputuint(2, v);
    }
}

static void ra_dump_intervals(char *fname) {
    int i;

    fdputs("DUMP fn=", 2);
    fdputs(fname, 2);
    fdputs(" ninst=", 2);
    fdputuint(2, h_ninst);
    fdputs(" norder=", 2);
    fdputuint(2, ra_norder);
    fdputc(10, 2);

    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_NOP) { i = i + 1; continue; }
        fdputs("DUMP i=", 2);
        fdputuint(2, i);
        fdputs(" k=", 2);
        fdputs(bg_op_name(h_kind[i]), 2);
        fdputs(" b=", 2);
        ra_dump_signed(h_blk[i]);
        fdputs(" p=", 2);
        ra_dump_signed(ra_pos[i]);
        fdputs(" e=", 2);
        ra_dump_signed(ra_iend[i]);
        fdputs(" r=", 2);
        ra_dump_signed(ra_reg[i]);
        fdputs(" s=", 2);
        ra_dump_signed(ra_spill_off[i]);
        fdputs(" v=", 2);
        ra_dump_signed(h_val[i]);
        fdputs(" s1=", 2);
        ra_dump_signed(h_src1[i]);
        fdputs(" s2=", 2);
        ra_dump_signed(h_src2[i]);
        fdputc(10, 2);
        i = i + 1;
    }
}
