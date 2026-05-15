/* hir_regalloc_a64.h -- IRC graph-coloring register allocation for AArch64
 *
 * Adapted from stage07-cross/hir_regalloc_x64.h.  Allocates 26 registers:
 *
 *   Callee-saved (slots 0..9):  X19..X28
 *   Caller-saved (slots 10..25): X0..X15
 *
 * X16 / X17 (IP0 / IP1, "intra-procedure call scratch") are reserved
 * as codegen scratch — always available for spill/reload, materialise,
 * and address temporaries.  This mirrors x86-64's use of RAX, but uses
 * AArch64's officially-designated scratch pair.
 *
 * X18 (Linux platform register), X29 (FP), X30 (LR), X31 (SP/XZR) are
 * not allocatable.  X8 is allocatable here even though the syscall
 * builtin uses it directly (the builtin sequence kills caller-saved
 * regs at its boundary, which is what regalloc expects of any call).
 *
 * AArch64 has no register clobbers from variable shifts (LSLV/LSRV/
 * ASRV use general regs) or DIV/REM (UDIV/SDIV use general regs), so
 * the cx/dx clobber tracking is degenerate (always zero).  We keep
 * the arrays present so the reused IRC algorithm doesn't need editing.
 *
 * Reference: Poletto-Sarkar "Linear Scan Register Allocation" (1999),
 * with Appel/George IRC-style coalescing layered on top.
 *
 * Input:  HIR after SSA construction and optimization.
 * Output: ra_reg[]      — physical AArch64 register per inst (-1 = spilled/remat)
 *         ra_spill_off[] — spill-slot fp offset per inst (0 = none)
 *         ra_ncsave/ra_csave_reg[]/ra_csave_off[] — callee-save info
 */

#ifndef HIR_REGALLOC_A64_H
#define HIR_REGALLOC_A64_H

/* Side arrays (defined in hir_codegen_a64.h, forward-declared here so
 * liveness analysis can track extra operands once peepholes land).
 * Currently unused on AArch64 (no SIB equivalent yet) but the regalloc
 * source treats them as inputs. */
static int hx_sib_index[HIR_MAX_INST];     /* SIB index for stores (unused) */
static int hx_alu_sib_idx[HIR_MAX_INST];   /* SIB index for ALU+SIB (unused) */

/* --- Configuration ---
 *
 * Two register classes: X (integer/pointer) and V (SIMD/FP).  IRC
 * coloring is unified — slot indices live in one space, but each node
 * has a class and only same-class nodes interfere (different physical
 * register files don't conflict).  ra_writeback dispatches on class to
 * pick the right physical-register table.
 *
 * X class: 26 slots = X19..X28 (callee-saved, 0..9) + X0..X15
 *          (caller-saved, 10..25).
 * V class: 24 slots = V0..V7 + V16..V31, all caller-saved.  V8..V15 are
 *          AAPCS callee-saved (lower 64 bits) but we don't allocate
 *          them yet — saving FP callee-saves in the prologue/epilogue
 *          is deferred to a follow-up session.  Call-crossing V values
 *          therefore must spill (gc_k returns 0 for them). */
#define RA_NPHY     26   /* X class total slots */
#define RA_NCALLEE  10   /* X class callee-saved slots (0..9) */
#define RA_NPHY_V   22   /* V class total slots (V0..V7 + V16..V29) */
#define RA_NCALLEE_V 0   /* V class callee-saved slots (none allocated yet) */
#define RA_CLASS_X  0
#define RA_CLASS_V  1

/* The CX/DX clobber slot constants are unused on AArch64 (always-zero
 * arrays below), but referenced by the shared IRC algorithm.  Wire them
 * to invalid slot indices so any accidental hit is harmless. */
#define RA_SLOT_RCX (-1)
#define RA_SLOT_RDX (-1)

/* X class: slot index → X-register encoding. */
static int ra_a64_phys[RA_NPHY];
static int ra_a64_is_callee[RA_NPHY];

/* V class: slot index → V-register encoding (0..31, same numerical
 * range as X but different physical file). */
static int ra_a64_phys_v[RA_NPHY_V];
static int ra_a64_is_callee_v[RA_NPHY_V];

/* Reverse mappings: register encoding → slot index (-1 = not allocatable). */
#define RA_A64_REVERSE_SIZE 32
static int ra_a64_slot[RA_A64_REVERSE_SIZE];     /* X-reg → X-slot */
static int ra_a64_slot_v[RA_A64_REVERSE_SIZE];   /* V-reg → V-slot */

/* Class of each HIR value's destination register.  Filled in gc_build
 * (size matches the gc_node[] index space — RA_CLASS_X for nodes that
 * produce ints/ptrs, RA_CLASS_V for nodes that produce float/double).
 * Cross-class interference edges are skipped during graph build. */
static int gc_class[HIR_MAX_INST];

/* Forward decl (defined below; used by gc_build before its definition). */
static int ra_class_of(int inst);

/* Call-crossing flag: 1 if value's live interval spans a CALL. */
static int ra_crosses_call[HIR_MAX_INST];

/* Always-zero on AArch64 — kept so the shared IRC code can read them. */
static int ra_crosses_cx_clobber[HIR_MAX_INST];
static int ra_crosses_dx_clobber[HIR_MAX_INST];

static void ra_init_a64_regs(void) {
    int i;
    /* Callee-saved (slots 0..9): X19..X28 */
    ra_a64_phys[0] = A64_X19;  ra_a64_is_callee[0] = 1;
    ra_a64_phys[1] = A64_X20;  ra_a64_is_callee[1] = 1;
    ra_a64_phys[2] = A64_X21;  ra_a64_is_callee[2] = 1;
    ra_a64_phys[3] = A64_X22;  ra_a64_is_callee[3] = 1;
    ra_a64_phys[4] = A64_X23;  ra_a64_is_callee[4] = 1;
    ra_a64_phys[5] = A64_X24;  ra_a64_is_callee[5] = 1;
    ra_a64_phys[6] = A64_X25;  ra_a64_is_callee[6] = 1;
    ra_a64_phys[7] = A64_X26;  ra_a64_is_callee[7] = 1;
    ra_a64_phys[8] = A64_X27;  ra_a64_is_callee[8] = 1;
    ra_a64_phys[9] = A64_X28;  ra_a64_is_callee[9] = 1;
    /* Caller-saved (slots 10..25): X0..X15 */
    ra_a64_phys[10] = A64_X0;   ra_a64_is_callee[10] = 0;
    ra_a64_phys[11] = A64_X1;   ra_a64_is_callee[11] = 0;
    ra_a64_phys[12] = A64_X2;   ra_a64_is_callee[12] = 0;
    ra_a64_phys[13] = A64_X3;   ra_a64_is_callee[13] = 0;
    ra_a64_phys[14] = A64_X4;   ra_a64_is_callee[14] = 0;
    ra_a64_phys[15] = A64_X5;   ra_a64_is_callee[15] = 0;
    ra_a64_phys[16] = A64_X6;   ra_a64_is_callee[16] = 0;
    ra_a64_phys[17] = A64_X7;   ra_a64_is_callee[17] = 0;
    ra_a64_phys[18] = A64_X8;   ra_a64_is_callee[18] = 0;
    ra_a64_phys[19] = A64_X9;   ra_a64_is_callee[19] = 0;
    ra_a64_phys[20] = A64_X10;  ra_a64_is_callee[20] = 0;
    ra_a64_phys[21] = A64_X11;  ra_a64_is_callee[21] = 0;
    ra_a64_phys[22] = A64_X12;  ra_a64_is_callee[22] = 0;
    ra_a64_phys[23] = A64_X13;  ra_a64_is_callee[23] = 0;
    ra_a64_phys[24] = A64_X14;  ra_a64_is_callee[24] = 0;
    ra_a64_phys[25] = A64_X15;  ra_a64_is_callee[25] = 0;

    i = 0;
    while (i < RA_A64_REVERSE_SIZE) {
        ra_a64_slot[i] = -1;
        ra_a64_slot_v[i] = -1;
        i = i + 1;
    }

    /* V class: V0..V7 (caller-saved arg regs) + V16..V31 (caller-saved). */
    ra_a64_phys_v[0]  = A64_V0;   ra_a64_is_callee_v[0]  = 0;
    ra_a64_phys_v[1]  = A64_V1;   ra_a64_is_callee_v[1]  = 0;
    ra_a64_phys_v[2]  = A64_V2;   ra_a64_is_callee_v[2]  = 0;
    ra_a64_phys_v[3]  = A64_V3;   ra_a64_is_callee_v[3]  = 0;
    ra_a64_phys_v[4]  = A64_V4;   ra_a64_is_callee_v[4]  = 0;
    ra_a64_phys_v[5]  = A64_V5;   ra_a64_is_callee_v[5]  = 0;
    ra_a64_phys_v[6]  = A64_V6;   ra_a64_is_callee_v[6]  = 0;
    ra_a64_phys_v[7]  = A64_V7;   ra_a64_is_callee_v[7]  = 0;
    ra_a64_phys_v[8]  = A64_V16;  ra_a64_is_callee_v[8]  = 0;
    ra_a64_phys_v[9]  = A64_V17;  ra_a64_is_callee_v[9]  = 0;
    ra_a64_phys_v[10] = A64_V18;  ra_a64_is_callee_v[10] = 0;
    ra_a64_phys_v[11] = A64_V19;  ra_a64_is_callee_v[11] = 0;
    ra_a64_phys_v[12] = A64_V20;  ra_a64_is_callee_v[12] = 0;
    ra_a64_phys_v[13] = A64_V21;  ra_a64_is_callee_v[13] = 0;
    ra_a64_phys_v[14] = A64_V22;  ra_a64_is_callee_v[14] = 0;
    ra_a64_phys_v[15] = A64_V23;  ra_a64_is_callee_v[15] = 0;
    ra_a64_phys_v[16] = A64_V24;  ra_a64_is_callee_v[16] = 0;
    ra_a64_phys_v[17] = A64_V25;  ra_a64_is_callee_v[17] = 0;
    ra_a64_phys_v[18] = A64_V26;  ra_a64_is_callee_v[18] = 0;
    ra_a64_phys_v[19] = A64_V27;  ra_a64_is_callee_v[19] = 0;
    ra_a64_phys_v[20] = A64_V28;  ra_a64_is_callee_v[20] = 0;
    ra_a64_phys_v[21] = A64_V29;  ra_a64_is_callee_v[21] = 0;
    /* V30/V31 reserved as HX_SCRATCH_V1/V2 — see hir_codegen_a64.h. */

    i = 0;
    while (i < RA_NPHY_V) {
        ra_a64_slot_v[ra_a64_phys_v[i]] = i;
        i = i + 1;
    }

    i = 0;
    while (i < RA_NPHY) {
        ra_a64_slot[ra_a64_phys[i]] = i;
        i = i + 1;
    }
}

/* --- Output arrays --- */
static int ra_reg[HIR_MAX_INST];    /* a64 physical register, -1 = spilled/remat */
static int ra_spill_off[HIR_MAX_INST]; /* spill slot (negative fp offset), 0 = none */

/* --- Live interval arrays --- */
static int ra_pos[HIR_MAX_INST];    /* linearized position */
static int ra_iend[HIR_MAX_INST];   /* last use position (end of live interval) */

/* --- Linearized order --- */
static int ra_order[HIR_MAX_INST];  /* instruction indices in position order */
static int ra_norder;

/* --- Graph-coloring register allocator (IRC) --- */

/* Limits */
#define GC_MAX_NODE  4096
#define GC_MAX_EDGE  524288
#define GC_MAX_MOVE  8192

/* Node data */
static int gc_nnode;                    /* number of nodes */
static int gc_inst[GC_MAX_NODE];        /* node → HIR inst index */
static int gc_node[HIR_MAX_INST];       /* HIR inst → node index (-1 = none) */

/* Adjacency lists (CSR-like linked list in flat edge pool) */
static int gc_adj_head[GC_MAX_NODE];    /* first edge index for node, -1 = none */
static int gc_adj_peer[GC_MAX_EDGE];    /* peer node of this edge */
static int gc_adj_next[GC_MAX_EDGE];    /* next edge in node's adj list */
static int gc_nedge;

/* Node degree (current, decremented during simplify) */
static int gc_degree[GC_MAX_NODE];

/* Move list */
static int gc_mv_a[GC_MAX_MOVE];       /* move: node a */
static int gc_mv_b[GC_MAX_MOVE];       /* move: node b */
static int gc_nmove;

/* Move worklists: status per move */
#define GC_MV_WORKLIST  0
#define GC_MV_ACTIVE    1
#define GC_MV_COALESCED 2
#define GC_MV_FROZEN    3
#define GC_MV_CONSTRAINED 4
static int gc_mv_status[GC_MAX_MOVE];

/* Per-node move list (linked list of move indices) */
#define GC_MAX_NMLIST 32768
static int gc_nmlist_mv[GC_MAX_NMLIST]; /* move index */
static int gc_nmlist_next[GC_MAX_NMLIST]; /* next in list */
static int gc_nmlist_head[GC_MAX_NODE]; /* first entry, -1 = none */
static int gc_nnmlist;

/* Node worklists */
#define GC_WL_SIMPLIFY  0
#define GC_WL_FREEZE    1
#define GC_WL_SPILL     2
#define GC_WL_COALESCED 3
#define GC_WL_SELECT    4  /* on select stack */
#define GC_WL_COLORED   5
static int gc_wl[GC_MAX_NODE];         /* worklist membership */

/* Select stack */
static int gc_sel_stk[GC_MAX_NODE];
static int gc_nsel;

/* Coalescing: alias[n] = merged-into node, or n itself */
static int gc_alias[GC_MAX_NODE];

/* Coloring result: color per node (slot index 0..RA_NPHY-1), -1 = spilled */
static int gc_color[GC_MAX_NODE];

/* --- Callee-save tracking --- */
static int ra_used[RA_NPHY];        /* 1 if slot was assigned */
static int ra_csave_reg[RA_NPHY];   /* a64 physical register number */
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

        /* LICM-hoisted clones split: those with no body-local dep go
         * at block top (so the body can use them); those that reference
         * a value defined in this block's body go just before the
         * terminator (so they see the body's defs).  Mirrors the x64
         * fix in b6f0832 -- this split must match hx_gen_func()'s
         * emission order exactly. */
        i = licm_head[b];
        while (i >= 0) {
            int s1 = h_src1[i];
            int s2 = h_src2[i];
            int dep_local = 0;
            if (s1 >= 0 && h_blk[s1] == b &&
                h_kind[s1] != HI_PARAM && h_kind[s1] != HI_PHI &&
                !hi_is_remat(h_kind[s1])) dep_local = 1;
            if (s2 >= 0 && ho_src2_is_ref(h_kind[i]) && h_blk[s2] == b &&
                h_kind[s2] != HI_PARAM && h_kind[s2] != HI_PHI &&
                !hi_is_remat(h_kind[s2])) dep_local = 1;
            if (!dep_local && h_kind[i] != HI_NOP) {
                ra_pos[i] = pos;
                ra_order[ra_norder] = i;
                ra_norder = ra_norder + 1;
                pos = pos + 1;
            }
            i = licm_next[i];
        }

        /* Regular block body up to (not including) the terminator */
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

        /* Body-dep hoisted clones, between body and terminator */
        i = licm_head[b];
        while (i >= 0) {
            int s1 = h_src1[i];
            int s2 = h_src2[i];
            int dep_local = 0;
            if (s1 >= 0 && h_blk[s1] == b &&
                h_kind[s1] != HI_PARAM && h_kind[s1] != HI_PHI &&
                !hi_is_remat(h_kind[s1])) dep_local = 1;
            if (s2 >= 0 && ho_src2_is_ref(h_kind[i]) && h_blk[s2] == b &&
                h_kind[s2] != HI_PARAM && h_kind[s2] != HI_PHI &&
                !hi_is_remat(h_kind[s2])) dep_local = 1;
            if (dep_local && h_kind[i] != HI_NOP) {
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

    /* Start BFS from use_blk.  Extending to last(use_blk) is conservative —
     * for non-phi values used in a loop, this captures the live-out-via-
     * back-edge case that this single-pass backprop cannot otherwise
     * compute.  Phi-result/phi-arg interference that this overshoot
     * introduces is removed after gc_build by gc_drop_phi_edges(), which
     * implements virtual out-of-SSA coalescing. */
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

        /* src2 (instruction ref for binops, STORE, and SIB-folded LOAD) */
        if (h_src2[inst] >= 0 && (ho_src2_is_ref(k) || k == HI_LOAD)) {
            ra_extend(h_src2[inst], p);
        }

        /* SIB index for stores and ALU+SIB (stored in side arrays) */
        if (k == HI_STORE && hx_sib_index[inst] >= 0) {
            ra_extend(hx_sib_index[inst], p);
        }
        if (hx_alu_sib_idx[inst] >= 0) {
            ra_extend(hx_alu_sib_idx[inst], p);
        }

        /* Call arguments */
        if ((k == HI_CALL || k == HI_CALLP || k == HI_A64_DBT_TRAMPOLINE) && h_cbase[inst] >= 0) {
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

        if (h_src2[inst] >= 0 && (ho_src2_is_ref(k) || k == HI_LOAD)) {
            src = h_src2[inst];
            if (ra_pos[src] >= 0 && h_blk[src] != h_blk[inst]) {
                ra_backprop(src, h_blk[inst]);
            }
        }

        if (k == HI_STORE && hx_sib_index[inst] >= 0) {
            src = hx_sib_index[inst];
            if (ra_pos[src] >= 0 && h_blk[src] != h_blk[inst]) {
                ra_backprop(src, h_blk[inst]);
            }
        }

        if (hx_alu_sib_idx[inst] >= 0) {
            src = hx_alu_sib_idx[inst];
            if (ra_pos[src] >= 0 && h_blk[src] != h_blk[inst]) {
                ra_backprop(src, h_blk[inst]);
            }
        }

        if ((k == HI_CALL || k == HI_CALLP || k == HI_A64_DBT_TRAMPOLINE) && h_cbase[inst] >= 0) {
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
 * Step 3: Graph-coloring register allocation (IRC)
 *
 * Iterated Register Coalescing (George-Appel, 1996).
 * Replaces linear scan with global interference graph coloring.
 * ================================================================= */

/* AArch64 AAPCS64 arg registers — 8 each for X (X0..X7) and V (V0..V7).
 * Sites that on x86-64 checked "pidx < 6" use the RA_NARG_REGS macro
 * here.  V-class hint lookup uses ra_v_arg_regs in parallel with X. */
#define RA_NARG_REGS 8
static int ra_arg_regs[RA_NARG_REGS];
static int ra_v_arg_regs[RA_NARG_REGS];

static void ra_init_arg_regs(void) {
    ra_arg_regs[0] = A64_X0;
    ra_arg_regs[1] = A64_X1;
    ra_arg_regs[2] = A64_X2;
    ra_arg_regs[3] = A64_X3;
    ra_arg_regs[4] = A64_X4;
    ra_arg_regs[5] = A64_X5;
    ra_arg_regs[6] = A64_X6;
    ra_arg_regs[7] = A64_X7;
    ra_v_arg_regs[0] = A64_V0;
    ra_v_arg_regs[1] = A64_V1;
    ra_v_arg_regs[2] = A64_V2;
    ra_v_arg_regs[3] = A64_V3;
    ra_v_arg_regs[4] = A64_V4;
    ra_v_arg_regs[5] = A64_V5;
    ra_v_arg_regs[6] = A64_V6;
    ra_v_arg_regs[7] = A64_V7;
}

/* --- Adjacency helpers --- */

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
    if (u == v) return;
    if (gc_has_edge(u, v)) return;
    if (gc_nedge + 2 > GC_MAX_EDGE) return;

    /* u → v */
    gc_adj_peer[gc_nedge] = v;
    gc_adj_next[gc_nedge] = gc_adj_head[u];
    gc_adj_head[u] = gc_nedge;
    gc_nedge = gc_nedge + 1;

    /* v → u */
    gc_adj_peer[gc_nedge] = u;
    gc_adj_next[gc_nedge] = gc_adj_head[v];
    gc_adj_head[v] = gc_nedge;
    gc_nedge = gc_nedge + 1;

    gc_degree[u] = gc_degree[u] + 1;
    gc_degree[v] = gc_degree[v] + 1;
}

static void gc_add_inst_edge(int a, int b) {
    int na;
    int nb;
    if (a < 0 || b < 0 || a == b) return;
    na = gc_node[a];
    nb = gc_node[b];
    if (na < 0 || nb < 0 || na == nb) return;
    if (gc_class[na] != gc_class[nb]) return;
    gc_add_edge(na, nb);
}

/* --- Move-list helpers --- */

static void gc_add_node_move(int node, int mv) {
    int idx;
    if (gc_nnmlist >= GC_MAX_NMLIST) return;
    idx = gc_nnmlist;
    gc_nnmlist = gc_nnmlist + 1;
    gc_nmlist_mv[idx] = mv;
    gc_nmlist_next[idx] = gc_nmlist_head[node];
    gc_nmlist_head[node] = idx;
}

/* Does node have any active/worklist moves? */
static int gc_move_related(int n) {
    int e;
    int mv;
    int st;
    e = gc_nmlist_head[n];
    while (e >= 0) {
        mv = gc_nmlist_mv[e];
        st = gc_mv_status[mv];
        if (st == GC_MV_WORKLIST || st == GC_MV_ACTIVE) return 1;
        e = gc_nmlist_next[e];
    }
    return 0;
}

/* Get alias (chase coalesce chain) */
static int gc_get_alias(int n) {
    int lim;
    lim = 0;
    while (gc_alias[n] != n && lim < GC_MAX_NODE) {
        n = gc_alias[n];
        lim = lim + 1;
    }
    return n;
}

/* --- Build interference graph --- */

static void gc_build(void) {
    int i;
    int j;
    int inst;
    int k;
    int n;
    int ai;
    int aj;
    int ni;
    int nj;

    /* Initialize */
    gc_nnode = 0;
    gc_nedge = 0;
    gc_nmove = 0;
    gc_nnmlist = 0;
    gc_nsel = 0;

    i = 0;
    while (i < h_ninst) {
        gc_node[i] = -1;
        i = i + 1;
    }

    /* Create nodes for value-producing, non-remat instructions */
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];
        if (hi_has_value(k) && !hi_is_remat(k) && k != HI_NOP) {
            if (gc_nnode < GC_MAX_NODE) {
                n = gc_nnode;
                gc_inst[n] = inst;
                gc_node[inst] = n;
                gc_class[n] = ra_class_of(inst);
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

    /* Build edges: sweep in linear order, maintaining active set.
     * Two values interfere if their live ranges overlap. */
    {
        int act[GC_MAX_NODE];
        int nact;
        int p;
        int aend;

        nact = 0;
        i = 0;
        while (i < ra_norder) {
            inst = ra_order[i];
            p = ra_pos[inst];
            ni = gc_node[inst];

            /* Expire dead intervals.  Use kill-use semantics: a value
             * whose last use is at pos p dies before any new definition
             * at the same pos, so phi-results and destructive-ALU dsts
             * do not interfere with their last-used sources.  This is
             * what makes phi/destructive-ALU coalescing actually fire. */
            j = 0;
            while (j < nact) {
                aj = act[j];
                if (ra_iend[gc_inst[aj]] <= p) {
                    nact = nact - 1;
                    act[j] = act[nact];
                } else {
                    j = j + 1;
                }
            }

            /* Add interference edges between new node and all active.
             * Skip edges between different classes — X-class and V-class
             * values use different physical register files and never
             * conflict for coloring purposes. */
            if (ni >= 0) {
                j = 0;
                while (j < nact) {
                    if (gc_class[ni] == gc_class[act[j]]) {
                        gc_add_edge(ni, act[j]);
                    }
                    j = j + 1;
                }
                act[nact] = ni;
                nact = nact + 1;
            }

            i = i + 1;
        }
    }

    /* Values consumed by the same instruction are simultaneously live at
     * that instruction even if all of them die there.  The interval sweep
     * above uses kill-at-use semantics so a destination can reuse its last
     * source register, but that also means source/source conflicts at the
     * same position need explicit edges.  This matters most for calls:
     * two call arguments cannot share one physical register before the
     * argument marshal runs. */
    {
        int ops[40];
        int nops;
        int base;
        int nargs;
        int o;
        int p;
        int src;

        i = 0;
        while (i < ra_norder) {
            inst = ra_order[i];
            k = h_kind[inst];
            nops = 0;

            if (h_src1[inst] >= 0) {
                ops[nops] = h_src1[inst];
                nops = nops + 1;
            }
            if (h_src2[inst] >= 0 && (ho_src2_is_ref(k) || k == HI_LOAD)) {
                ops[nops] = h_src2[inst];
                nops = nops + 1;
            }
            if (k == HI_STORE && hx_sib_index[inst] >= 0) {
                ops[nops] = hx_sib_index[inst];
                nops = nops + 1;
            }
            if (hx_alu_sib_idx[inst] >= 0) {
                ops[nops] = hx_alu_sib_idx[inst];
                nops = nops + 1;
            }
            if ((k == HI_CALL || k == HI_CALLP || k == HI_A64_DBT_TRAMPOLINE) &&
                h_cbase[inst] >= 0) {
                base = h_cbase[inst];
                nargs = h_val[inst];
                o = 0;
                while (o < nargs && nops < 40) {
                    src = h_carg[base + o];
                    if (src >= 0) {
                        ops[nops] = src;
                        nops = nops + 1;
                    }
                    o = o + 1;
                }
            }

            o = 0;
            while (o < nops) {
                p = o + 1;
                while (p < nops) {
                    gc_add_inst_edge(ops[o], ops[p]);
                    p = p + 1;
                }
                o = o + 1;
            }

            i = i + 1;
        }
    }

    /* PARAM nodes are linearized as ordinary HIR definitions, but their
     * incoming ABI register values all exist simultaneously at function
     * entry and the prologue materializes them as one parallel copy.
     * Make same-class PARAMs interfere so the allocator cannot assign two
     * distinct incoming args to one home register. */
    {
        int params[32];
        int nparams;
        int a;
        int b;

        nparams = 0;
        i = 0;
        while (i < ra_norder && nparams < 32) {
            inst = ra_order[i];
            if (h_kind[inst] == HI_PARAM) {
                params[nparams] = inst;
                nparams = nparams + 1;
            }
            i = i + 1;
        }

        a = 0;
        while (a < nparams) {
            b = a + 1;
            while (b < nparams) {
                gc_add_inst_edge(params[a], params[b]);
                b = b + 1;
            }
            a = a + 1;
        }
    }
}

/* --- Virtual out-of-SSA: drop phi-result/phi-arg interference edges ---
 *
 * The interference graph built above is over-conservative: by extending
 * each value's live range to the end of its use-block (needed for
 * loop-carried non-phi values like a function argument used inside a
 * loop body), it makes a phi-result and its back-edge phi-arg appear to
 * overlap inside the loop body, even though out-of-SSA semantics treat
 * them as the same physical name linked by an implicit copy at the
 * predecessor terminator.  The IRC coalescer then refuses to merge
 * them, leaving an explicit `mov phi_reg, new_reg` at every back-edge.
 *
 * This pass restores the standard "phi result and phi args don't
 * interfere with each other" rule by deleting just those edges.  Other
 * interference relationships (e.g., a phi-arg vs. an unrelated value
 * computed in the same block) are preserved.  When the IRC later
 * considers the move-edge between phi-result and phi-arg, only the
 * remaining structural constraints (Briggs/George) gate the merge.
 */

static void gc_remove_edge(int u, int v) {
    int e;
    int prev;
    int peer;
    int found;

    if (u < 0 || v < 0 || u == v) return;

    /* Remove v from u's adjacency list */
    found = 0;
    prev = -1;
    e = gc_adj_head[u];
    while (e >= 0) {
        peer = gc_adj_peer[e];
        if (peer == v) {
            if (prev < 0) gc_adj_head[u] = gc_adj_next[e];
            else gc_adj_next[prev] = gc_adj_next[e];
            found = 1;
            break;
        }
        prev = e;
        e = gc_adj_next[e];
    }
    if (!found) return;

    /* Remove u from v's adjacency list (must succeed if the above did) */
    prev = -1;
    e = gc_adj_head[v];
    while (e >= 0) {
        peer = gc_adj_peer[e];
        if (peer == u) {
            if (prev < 0) gc_adj_head[v] = gc_adj_next[e];
            else gc_adj_next[prev] = gc_adj_next[e];
            break;
        }
        prev = e;
        e = gc_adj_next[e];
    }

    if (gc_degree[u] > 0) gc_degree[u] = gc_degree[u] - 1;
    if (gc_degree[v] > 0) gc_degree[v] = gc_degree[v] - 1;
}

/* Per-inst flag: 1 if the inst's value is consumed by any non-PHI
 * user (src1/src2 of an ALU/LOAD/STORE/cmp/etc., a call arg, or a
 * SIB-index slot).  PHI-arg uses are not counted — those become
 * implicit copies at the predecessor terminator after out-of-SSA, so
 * they're consistent with phi-result/phi-arg coalescing. */
static int gc_has_nonphi_user[HIR_MAX_INST];

static void gc_compute_nonphi_users(void) {
    int i;
    int j;
    int inst;
    int k;
    int s;

    i = 0;
    while (i < h_ninst) { gc_has_nonphi_user[i] = 0; i = i + 1; }

    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];
        if (k == HI_PHI) { i = i + 1; continue; }

        s = h_src1[inst];
        if (s >= 0) gc_has_nonphi_user[s] = 1;

        s = h_src2[inst];
        if (s >= 0 && (ho_src2_is_ref(k) || k == HI_LOAD)) {
            gc_has_nonphi_user[s] = 1;
        }

        if ((k == HI_CALL || k == HI_CALLP || k == HI_A64_DBT_TRAMPOLINE) &&
            h_cbase[inst] >= 0) {
            j = 0;
            while (j < h_val[inst]) {
                s = h_carg[h_cbase[inst] + j];
                if (s >= 0) gc_has_nonphi_user[s] = 1;
                j = j + 1;
            }
        }

        if (k == HI_STORE && hx_sib_index[inst] >= 0) {
            gc_has_nonphi_user[hx_sib_index[inst]] = 1;
        }
        if (hx_alu_sib_idx[inst] >= 0) {
            gc_has_nonphi_user[hx_alu_sib_idx[inst]] = 1;
        }

        i = i + 1;
    }
}

static void gc_drop_phi_edges(void) {
    int i;
    int j;
    int k;
    int phi_n;
    int arg;
    int arg_n;

    gc_compute_nonphi_users();

    i = 0;
    while (i < ra_norder) {
        int inst;
        inst = ra_order[i];
        k = h_kind[inst];
        if (k == HI_PHI && h_pbase[inst] >= 0) {
            phi_n = gc_node[inst];
            if (phi_n >= 0) {
                j = 0;
                while (j < h_pcnt[inst]) {
                    arg = h_pval[h_pbase[inst] + j];
                    if (arg >= 0) {
                        /* Don't drop edges to phi-args that are PARAMs:
                         * coalescing a PARAM with a phi-result drifts it
                         * out of its ABI register, and the prologue's
                         * sequential `mov dst,arg_reg` materialization
                         * doesn't handle the resulting parallel-move
                         * problem (e.g., PARAM 0 placed in X1 clobbers
                         * PARAM 1's incoming value before it's read). */
                        if (h_kind[arg] == HI_PARAM) { j = j + 1; continue; }
                        /* Don't drop edges to phi-args that have a
                         * non-PHI user.  Those uses keep `arg` live
                         * past the implicit-copy point at the
                         * predecessor terminator, so `arg` and the
                         * phi-result genuinely interfere — coalescing
                         * them would fold a still-live value into the
                         * phi-result's storage and corrupt later reads.
                         * (Bug found via strtok_r in libc_a64: `start =
                         * s; while (...) s = s + 1; return start;`
                         * coalesced `start`'s value with the loop
                         * phi.) */
                        if (gc_has_nonphi_user[arg]) { j = j + 1; continue; }
                        arg_n = gc_node[arg];
                        if (arg_n >= 0) gc_remove_edge(phi_n, arg_n);
                    }
                    j = j + 1;
                }
            }
        }
        i = i + 1;
    }
}

/* --- Find coalesce candidates (moves) --- */

static void gc_add_move(int na, int nb) {
    int mv;
    if (na < 0 || nb < 0 || na == nb) return;
    if (gc_nmove >= GC_MAX_MOVE) return;
    mv = gc_nmove;
    gc_nmove = gc_nmove + 1;
    gc_mv_a[mv] = na;
    gc_mv_b[mv] = nb;
    gc_mv_status[mv] = GC_MV_WORKLIST;
    gc_add_node_move(na, mv);
    gc_add_node_move(nb, mv);
}

static void gc_find_moves(void) {
    int i;
    int inst;
    int k;
    int s1;
    int s2;
    int nd;
    int ns1;
    int ns2;
    int j;
    int a;
    int na;
    int pidx;

    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];
        nd = gc_node[inst];
        if (nd < 0) { i = i + 1; continue; }

        /* COPY: direct coalesce candidate */
        if (k == HI_COPY) {
            s1 = h_src1[inst];
            if (s1 >= 0) {
                ns1 = gc_node[s1];
                if (ns1 >= 0) gc_add_move(nd, ns1);
            }
        }

        /* Destructive ALU: src1 wants same register as dst.  Includes
         * HI_ADDI (add-immediate) so `p = p + offset` and friends emit
         * `add Wd, Wd, #imm` without a follow-up MOV. */
        if ((k >= HI_ADD && k <= HI_SRL) || k == HI_ADDI) {
            s1 = h_src1[inst];
            if (s1 >= 0) {
                ns1 = gc_node[s1];
                if (ns1 >= 0) gc_add_move(nd, ns1);
            }
        }

        /* PHI: each argument wants same register as PHI result */
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

/* --- IRC main loop --- */

/* Class of an HIR value's destination register: X (int/ptr) or V (FP).
 * For value-producing ops, the class follows ty_is_fp(h_ty[inst]) with
 * a few corrections: HI_FCVT_FtoI / HI_FEQ / HI_FLT / HI_FLE produce
 * an int from FP source (X class), and HI_FCVT_ItoF produces FP from
 * int (V class).  Non-FP-related ops are always X. */
static int ra_class_of(int inst) {
    int kk;
    if (inst < 0) return RA_CLASS_X;
    kk = h_kind[inst];
    if (kk == HI_FCVT_FtoI) return RA_CLASS_X;
    if (kk == HI_FEQ || kk == HI_FLT || kk == HI_FLE) return RA_CLASS_X;
    if (kk >= HI_FADD && kk <= HI_FNEG) return RA_CLASS_V;
    if (kk == HI_FCVT_ItoF) return RA_CLASS_V;
    if (kk == HI_FCVT_FtoD || kk == HI_FCVT_DtoF) return RA_CLASS_V;
    if (kk == HI_FCONST) return RA_CLASS_V;
    if (kk == HI_FSQRT) return RA_CLASS_V;
    /* Address-producing ops produce pointers (X-class) regardless of
     * the storage type they refer to.  HI_ALLOCA / HI_GADDR / HI_SADDR /
     * HI_FADDR / HI_GETFP have h_ty set to the underlying storage type
     * (e.g. TY_FLOAT for `float f`), so the type-based catch-all below
     * would mis-route them to V class. */
    if (kk == HI_ALLOCA) return RA_CLASS_X;
    if (kk == HI_GADDR || kk == HI_SADDR || kk == HI_FADDR) return RA_CLASS_X;
    if (kk == HI_GETFP) return RA_CLASS_X;
    /* Catch-all: route by destination type so PARAM/COPY/PHI/LOAD of
     * float/double end up V-class. */
    if (ty_is_fp(h_ty[inst])) return RA_CLASS_V;
    return RA_CLASS_X;
}

/* Number of available colors for a node.  Per class, with the X-class
 * special case for call-crossing (must use callee-saved).  V class has
 * no allocated callee-saved slots yet, so a call-crossing V value gets
 * gc_k=0 and is forced to spill — see RA_NCALLEE_V comment. */
static int gc_k(int n) {
    int inst;
    int k;
    int cls;
    inst = gc_inst[n];
    cls = gc_class[n];
    if (cls == RA_CLASS_V) {
        if (ra_crosses_call[inst]) return RA_NCALLEE_V;
        return RA_NPHY_V;
    }
    if (ra_crosses_call[inst]) return RA_NCALLEE;
    k = RA_NPHY;
    if (ra_crosses_cx_clobber[inst]) k = k - 1;
    if (ra_crosses_dx_clobber[inst]) k = k - 1;
    return k;
}

static void gc_make_worklists(void) {
    int n;
    int k;
    n = 0;
    while (n < gc_nnode) {
        k = gc_k(n);
        if (gc_degree[n] >= k) {
            gc_wl[n] = GC_WL_SPILL;
        } else if (gc_move_related(n)) {
            gc_wl[n] = GC_WL_FREEZE;
        } else {
            gc_wl[n] = GC_WL_SIMPLIFY;
        }
        n = n + 1;
    }
}

/* Enable moves for neighbors of node n */
static void gc_enable_moves(int n) {
    int e;
    int mv;
    int peer;
    int me;

    /* Enable moves for n itself */
    me = gc_nmlist_head[n];
    while (me >= 0) {
        mv = gc_nmlist_mv[me];
        if (gc_mv_status[mv] == GC_MV_ACTIVE) {
            gc_mv_status[mv] = GC_MV_WORKLIST;
        }
        me = gc_nmlist_next[me];
    }

    /* Enable moves for neighbors */
    e = gc_adj_head[n];
    while (e >= 0) {
        peer = gc_adj_peer[e];
        if (gc_wl[peer] != GC_WL_SELECT && gc_wl[peer] != GC_WL_COALESCED) {
            me = gc_nmlist_head[peer];
            while (me >= 0) {
                mv = gc_nmlist_mv[me];
                if (gc_mv_status[mv] == GC_MV_ACTIVE) {
                    gc_mv_status[mv] = GC_MV_WORKLIST;
                }
                me = gc_nmlist_next[me];
            }
        }
        e = gc_adj_next[e];
    }
}

/* Decrement degree; if it drops below K, move from spill to simplify/freeze */
static void gc_dec_degree(int n) {
    int d;
    int k;
    d = gc_degree[n];
    gc_degree[n] = d - 1;
    k = gc_k(n);
    if (d == k) {
        /* Degree just dropped below K */
        gc_enable_moves(n);
        if (gc_wl[n] == GC_WL_SPILL) {
            if (gc_move_related(n)) {
                gc_wl[n] = GC_WL_FREEZE;
            } else {
                gc_wl[n] = GC_WL_SIMPLIFY;
            }
        }
    }
}

/* Simplify: remove a low-degree non-move-related node */
static int gc_simplify(void) {
    int n;
    int e;
    int peer;
    n = 0;
    while (n < gc_nnode) {
        if (gc_wl[n] == GC_WL_SIMPLIFY) {
            gc_wl[n] = GC_WL_SELECT;
            gc_sel_stk[gc_nsel] = n;
            gc_nsel = gc_nsel + 1;

            /* Decrement neighbors' degrees */
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

/* George test: can we coalesce u and v?
 * For every neighbor t of v: t already interferes with u, or degree(t) < K */
static int gc_george(int u, int v) {
    int e;
    int t;
    int k;
    e = gc_adj_head[v];
    while (e >= 0) {
        t = gc_adj_peer[e];
        if (gc_wl[t] != GC_WL_SELECT && gc_wl[t] != GC_WL_COALESCED) {
            k = gc_k(t);
            if (gc_degree[t] >= k && !gc_has_edge(t, u)) {
                return 0;
            }
        }
        e = gc_adj_next[e];
    }
    return 1;
}

/* Briggs test: will the merged node have fewer than K high-degree neighbors? */
static int gc_briggs(int u, int v) {
    int count;
    int k;
    int e;
    int t;
    int already;

    k = gc_k(u);  /* conservative: use smaller K */
    if (gc_k(v) < k) k = gc_k(v);

    count = 0;
    /* Count high-degree neighbors of u */
    e = gc_adj_head[u];
    while (e >= 0) {
        t = gc_adj_peer[e];
        if (gc_wl[t] != GC_WL_SELECT && gc_wl[t] != GC_WL_COALESCED && t != v) {
            if (gc_degree[t] >= k) count = count + 1;
        }
        e = gc_adj_next[e];
    }
    /* Count high-degree neighbors of v not already counted */
    e = gc_adj_head[v];
    while (e >= 0) {
        t = gc_adj_peer[e];
        if (gc_wl[t] != GC_WL_SELECT && gc_wl[t] != GC_WL_COALESCED && t != u) {
            if (gc_degree[t] >= k && !gc_has_edge(t, u)) {
                count = count + 1;
            }
        }
        e = gc_adj_next[e];
    }
    return count < k;
}

/* Combine two nodes: merge v into u */
static void gc_combine(int u, int v) {
    int e;
    int t;

    gc_wl[v] = GC_WL_COALESCED;
    gc_alias[v] = u;

    /* Transfer v's move list to u */
    e = gc_nmlist_head[v];
    while (e >= 0) {
        gc_add_node_move(u, gc_nmlist_mv[e]);
        e = gc_nmlist_next[e];
    }

    /* Add edges: for each neighbor t of v, add edge(t, u).
     *
     * Always transfer the edge — even when t is already on the select
     * stack — because t's color will be looked up later via u's adj
     * list when u is colored.  Without the transfer, u's adj loses the
     * constraint that "u must not pick t's color" and u may pick a
     * color identical to t's.
     *
     * Latent in original IRC: traditional select order pops u BEFORE
     * t (high-index first), so u's color was already decided when t
     * was colored, and the asymmetric edge (still on t's side)
     * sufficed.  Surfaced once two-pass select started coloring some
     * nodes (PARAMs) before u.
     *
     * Degree update is still gated on t being in the active graph —
     * select/coalesced nodes have no live degree to manage. */
    e = gc_adj_head[v];
    while (e >= 0) {
        t = gc_adj_peer[e];
        if (gc_wl[t] != GC_WL_COALESCED) {
            gc_add_edge(t, u);
            if (gc_wl[t] != GC_WL_SELECT) {
                gc_dec_degree(t);
            }
        }
        e = gc_adj_next[e];
    }

    /* If u's degree now >= K and it was in freeze, move to spill */
    if (gc_degree[u] >= gc_k(u) && gc_wl[u] == GC_WL_FREEZE) {
        gc_wl[u] = GC_WL_SPILL;
    }
}

/* Coalesce: try to coalesce one move */
static int gc_coalesce(void) {
    int mv;
    int u;
    int v;
    int tmp;

    mv = 0;
    while (mv < gc_nmove) {
        if (gc_mv_status[mv] != GC_MV_WORKLIST) { mv = mv + 1; continue; }

        u = gc_get_alias(gc_mv_a[mv]);
        v = gc_get_alias(gc_mv_b[mv]);

        /* Ensure call-crossing node is u (the survivor) if applicable,
         * to preserve callee-saved constraint */
        if (ra_crosses_call[gc_inst[v]] && !ra_crosses_call[gc_inst[u]]) {
            tmp = u; u = v; v = tmp;
        }

        if (u == v) {
            /* Already coalesced */
            gc_mv_status[mv] = GC_MV_COALESCED;
            if (!gc_move_related(u) && gc_degree[u] < gc_k(u) && gc_wl[u] == GC_WL_FREEZE) {
                gc_wl[u] = GC_WL_SIMPLIFY;
            }
            return 1;
        }

        if (gc_has_edge(u, v)) {
            /* Constrained */
            gc_mv_status[mv] = GC_MV_CONSTRAINED;
            if (!gc_move_related(u) && gc_degree[u] < gc_k(u) && gc_wl[u] == GC_WL_FREEZE) {
                gc_wl[u] = GC_WL_SIMPLIFY;
            }
            if (!gc_move_related(v) && gc_degree[v] < gc_k(v) && gc_wl[v] == GC_WL_FREEZE) {
                gc_wl[v] = GC_WL_SIMPLIFY;
            }
            return 1;
        }

        /* Call-crossing constraint: don't coalesce if it would merge a
         * call-crossing node with one that can use caller-saved regs */
        if (ra_crosses_call[gc_inst[u]] != ra_crosses_call[gc_inst[v]]) {
            gc_mv_status[mv] = GC_MV_CONSTRAINED;
            return 1;
        }

        /* Try George or Briggs */
        if (gc_george(u, v) || gc_briggs(u, v)) {
            gc_mv_status[mv] = GC_MV_COALESCED;
            gc_combine(u, v);
            if (!gc_move_related(u) && gc_degree[u] < gc_k(u)) {
                if (gc_wl[u] == GC_WL_FREEZE) gc_wl[u] = GC_WL_SIMPLIFY;
            }
            return 1;
        }

        /* Cannot coalesce yet — make active */
        gc_mv_status[mv] = GC_MV_ACTIVE;
        return 1;
    }
    return 0;
}

/* Freeze: give up on coalescing a low-degree move-related node */
static int gc_freeze(void) {
    int n;
    int e;
    int mv;

    n = 0;
    while (n < gc_nnode) {
        if (gc_wl[n] == GC_WL_FREEZE) {
            gc_wl[n] = GC_WL_SIMPLIFY;
            /* Freeze all moves involving n */
            e = gc_nmlist_head[n];
            while (e >= 0) {
                mv = gc_nmlist_mv[e];
                if (gc_mv_status[mv] == GC_MV_WORKLIST || gc_mv_status[mv] == GC_MV_ACTIVE) {
                    int other;
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

/* Select spill: pick highest-degree node from spill worklist.
 * Uses cost heuristic: spill the node with lowest uses/degree. */
static int gc_select_spill(void) {
    int n;
    int best;
    int best_cost;
    int cost;
    int inst;

    best = -1;
    best_cost = 0x7FFFFFFF;

    n = 0;
    while (n < gc_nnode) {
        if (gc_wl[n] == GC_WL_SPILL) {
            inst = gc_inst[n];
            /* cost = uses * 100 / (degree + 1).  Lower = cheaper to spill. */
            cost = bg_uses[inst] * 100;
            cost = cost / (gc_degree[n] + 1);
            /* Prefer spilling non-call-crossing (they have more color options) */
            if (!ra_crosses_call[inst]) cost = cost + 50;
            if (cost < best_cost) {
                best_cost = cost;
                best = n;
            }
        }
        n = n + 1;
    }

    if (best < 0) return 0;

    /* Optimistic spill: move to simplify and try to color later */
    gc_wl[best] = GC_WL_SIMPLIFY;
    /* Freeze its moves */
    {
        int e;
        int mv;
        int other;
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

/* IRC main loop */
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
}

/* --- Select: pop stack and assign colors --- */

static void gc_select(void) {
    int i;
    int n;
    int inst;
    int k;
    int e;
    int peer;
    int pa;
    int pc;
    int used[RA_NPHY];
    int c;
    int hint;
    int pidx;
    int s1;
    int ns1;

    ra_init_arg_regs();

    /* Two-pass color: PARAMs first, then everything else.
     *
     * Why: gc_simplify pushes nodes in gc_node-INDEX order (low index
     * first), so the select-stack pops them with high index first and
     * low index last.  PARAM nodes are typically created early (low
     * gc_node index) and therefore pop LAST under the natural order —
     * by which point intermediate values have already taken slots
     * X0/X1/..., forcing each PARAM into a non-ABI register and adding
     * cross-MOV instructions in the prologue's parallel-copy phase.
     *
     * Fix: walk the stack twice.  First pass colors only PARAMs (via
     * the existing PARAM-hint code, which now finds X0/X1/... free);
     * second pass colors everything else, with `used` pre-seeded by
     * PARAM colors so intermediates naturally avoid clobbering them.
     * Pop direction within each pass is unchanged (high index first).
     *
     * a64 doesn't have x64's need_temp / 8-reg-homing cascade — the
     * prologue already does proper parallel-copy with cycle handling —
     * so the win here is just eliminating the wasted MOV pairs, not a
     * structural overhaul.  Same fix shape, smaller magnitude. */
    int pass;
    pass = 0;
    while (pass < 2) {

    /* Pop select stack in reverse */
    i = gc_nsel - 1;
    while (i >= 0) {
        int cls;
        int n_total;
        int n_callee;
        int param_x_class;

        n = gc_sel_stk[i];
        inst = gc_inst[n];
        k = gc_k(n);
        cls = gc_class[n];

        if (pass == 0 && h_kind[inst] != HI_PARAM) { i = i - 1; continue; }
        if (pass == 1 && h_kind[inst] == HI_PARAM) { i = i - 1; continue; }

        /* Per-class allocation bounds. */
        if (cls == RA_CLASS_V) {
            n_total  = RA_NPHY_V;
            n_callee = RA_NCALLEE_V;
        } else {
            n_total  = RA_NPHY;
            n_callee = RA_NCALLEE;
        }
        /* PARAM hints: X class uses ra_arg_regs / ra_a64_slot,
         * V class uses ra_v_arg_regs / ra_a64_slot_v.  Same shape; the
         * dispatch below selects based on `cls`. */
        param_x_class = (cls == RA_CLASS_X);

        /* Mark colors used by live neighbors.  All neighbours are same
         * class (cross-class edges were skipped in gc_build), so their
         * colors are valid slot indices in this class's color space. */
        c = 0;
        while (c < n_total) {
            used[c] = 0;
            c = c + 1;
        }

        e = gc_adj_head[n];
        while (e >= 0) {
            peer = gc_adj_peer[e];
            pa = gc_get_alias(peer);
            pc = gc_color[pa];
            if (pc >= 0 && pc < n_total) {
                used[pc] = 1;
            }
            e = gc_adj_next[e];
        }

        /* AArch64: no CX/DX-style clobber constraints — UDIV/SDIV and
         * LSLV/LSRV/ASRV use general registers, no architectural clobber. */

        /* Constraint: call-crossing → must use callee-saved.  V class
         * has zero allocated callee-saved (RA_NCALLEE_V == 0) so the
         * loop below produces no color and the value is forced to
         * spill via ra_assign_spills. */
        if (ra_crosses_call[inst]) {
            /* Try PARAM hint first */
            hint = -1;
            if (h_kind[inst] == HI_PARAM) {
                pidx = h_val[inst];
                if (pidx >= 0 && pidx < RA_NARG_REGS) {
                    if (param_x_class)
                        c = ra_a64_slot[ra_arg_regs[pidx]];
                    else
                        c = ra_a64_slot_v[ra_v_arg_regs[pidx]];
                    if (c >= 0 && c < n_callee && !used[c]) hint = c;
                }
            }
            if (hint >= 0) {
                gc_color[n] = hint;
            } else {
                gc_color[n] = -1;
                c = 0;
                while (c < n_callee) {
                    if (!used[c]) {
                        gc_color[n] = c;
                        break;
                    }
                    c = c + 1;
                }
            }
        } else {
            /* Non-call-crossing: try hints, then prefer caller-saved */
            hint = -1;

            /* PARAM hint: try ABI register */
            if (h_kind[inst] == HI_PARAM) {
                pidx = h_val[inst];
                if (pidx >= 0 && pidx < RA_NARG_REGS) {
                    if (param_x_class)
                        c = ra_a64_slot[ra_arg_regs[pidx]];
                    else
                        c = ra_a64_slot_v[ra_v_arg_regs[pidx]];
                    if (c >= 0 && !used[c]) hint = c;
                }
            }

            /* ALU hint: try src1's color for destructive ops (incl. ADDI) */
            if (hint < 0 && ((h_kind[inst] >= HI_ADD && h_kind[inst] <= HI_SRL) ||
                             h_kind[inst] == HI_ADDI)) {
                s1 = h_src1[inst];
                if (s1 >= 0) {
                    ns1 = gc_node[s1];
                    if (ns1 >= 0) {
                        c = gc_color[gc_get_alias(ns1)];
                        if (c >= 0 && !used[c]) hint = c;
                    }
                }
            }

            /* COPY hint: try source's color */
            if (hint < 0 && h_kind[inst] == HI_COPY) {
                s1 = h_src1[inst];
                if (s1 >= 0) {
                    ns1 = gc_node[s1];
                    if (ns1 >= 0) {
                        c = gc_color[gc_get_alias(ns1)];
                        if (c >= 0 && !used[c]) hint = c;
                    }
                }
            }

            if (hint >= 0) {
                gc_color[n] = hint;
            } else {
                gc_color[n] = -1;

                /* Biased coloring: for non-PARAM nodes, avoid colors wanted
                 * by uncolored PARAM neighbours.  AArch64 has no REX-prefix
                 * encoding penalty, so we always apply the bias when we can
                 * (the x64 sibling has a non-REX-availability override). */
                {
                    int avoid[RA_NPHY];   /* sized for X class; V class is smaller */
                    int ac;
                    int ae;
                    int ap;
                    int aa;
                    int apidx;
                    int awant;

                    ac = 0;
                    while (ac < n_total) { avoid[ac] = 0; ac = ac + 1; }

                    /* PARAM-avoidance bias is X-class only (uses
                     * ra_arg_regs which are X-form). */
                    if (param_x_class && h_kind[inst] != HI_PARAM) {
                        ae = gc_adj_head[n];
                        while (ae >= 0) {
                            ap = gc_adj_peer[ae];
                            aa = gc_get_alias(ap);
                            if (gc_color[aa] < 0 && h_kind[gc_inst[aa]] == HI_PARAM) {
                                apidx = h_val[gc_inst[aa]];
                                if (apidx >= 0 && apidx < RA_NARG_REGS) {
                                    awant = ra_a64_slot[ra_arg_regs[apidx]];
                                    if (awant >= 0 && awant < n_total)
                                        avoid[awant] = 1;
                                }
                            }
                            ae = gc_adj_next[ae];
                        }
                    }

                    /* Prefer caller-saved (slots n_callee..), avoiding bias */
                    c = n_callee;
                    while (c < n_total) {
                        if (!used[c] && !avoid[c]) { gc_color[n] = c; break; }
                        c = c + 1;
                    }
                    /* Fallback: caller-saved even if PARAM-wanted */
                    if (gc_color[n] < 0) {
                        c = n_callee;
                        while (c < n_total) {
                            if (!used[c]) { gc_color[n] = c; break; }
                            c = c + 1;
                        }
                    }
                    /* Fallback: callee-saved */
                    if (gc_color[n] < 0) {
                        c = 0;
                        while (c < n_callee) {
                            if (!used[c]) { gc_color[n] = c; break; }
                            c = c + 1;
                        }
                    }
                }
            }
        }

        i = i - 1;
    }

    pass = pass + 1;
    } /* end two-pass color */

    /* Propagate colors to coalesced nodes */
    i = 0;
    while (i < gc_nnode) {
        if (gc_wl[i] == GC_WL_COALESCED) {
            gc_color[i] = gc_color[gc_get_alias(i)];
        }
        i = i + 1;
    }

    /* Post-coloring PARAM fix-up: if a PARAM didn't get its ABI register,
     * and no neighbor has that color, take it to avoid prologue moves.
     *
     * Iterate until no progress: a PARAM blocked by another PARAM may
     * become unblocked once that other PARAM moves to its own want.
     * Without iteration, h_add-shaped functions (p0 holds X9/wants X0,
     * p1 holds X0/wants X1) cannot reach their natural homes in a
     * single visiting order — the chain has to unwind tail-first. */
    {
        int n;
        int inst;
        int pidx;
        int want;
        int old_c;
        int conflict;
        int ei;
        int peer;
        int pa;
        int moved;
        int passes;

        ra_init_arg_regs();
        passes = 0;
        moved = 1;
        while (moved && passes < 8) {
            moved = 0;
            passes = passes + 1;
            n = 0;
            while (n < gc_nnode) {
                inst = gc_inst[n];
                if (h_kind[inst] == HI_PARAM && gc_color[n] >= 0 && !ra_crosses_call[inst]) {
                    pidx = h_val[inst];
                    if (pidx >= 0 && pidx < RA_NARG_REGS) {
                        if (gc_class[n] == RA_CLASS_V)
                            want = ra_a64_slot_v[ra_v_arg_regs[pidx]];
                        else
                            want = ra_a64_slot[ra_arg_regs[pidx]];
                        old_c = gc_color[n];
                        if (want >= 0 && old_c != want) {
                            conflict = 0;
                            ei = gc_adj_head[n];
                            while (ei >= 0) {
                                peer = gc_adj_peer[ei];
                                pa = gc_get_alias(peer);
                                if (gc_color[pa] == want) {
                                    conflict = 1;
                                    break;
                                }
                                ei = gc_adj_next[ei];
                            }
                            if (!conflict) {
                                gc_color[n] = want;
                                moved = 1;
                            }
                        }
                    }
                }
                n = n + 1;
            }
        }
    }
}

/* --- Writeback: populate ra_reg[] and ra_used[] --- */

static void gc_writeback(void) {
    int i;
    int n;
    int inst;
    int c;
    int slot;

    (void)slot;

    /* Clear used tracking.  ra_used[] tracks X-class slots — used by
     * the prologue to decide which X callee-saves need spilling.  V
     * class doesn't allocate callee-saves yet, so no parallel tracker. */
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

    /* Write colors to ra_reg[].  Dispatch on class for the physical-
     * register table; ra_used[] only tracks X-class for now. */
    n = 0;
    while (n < gc_nnode) {
        inst = gc_inst[n];
        c = gc_color[n];
        if (c >= 0) {
            if (gc_class[n] == RA_CLASS_V) {
                ra_reg[inst] = ra_a64_phys_v[c];
                /* (No ra_used_v[] yet — V callee-saves not allocated.) */
            } else {
                ra_reg[inst] = ra_a64_phys[c];
                ra_used[c] = 1;
            }
        }
        n = n + 1;
    }
}

/* Add interference edges for HI_LOAD pair candidates.  For each pair
 * (A, B) of HI_LOADs in the same block whose addresses are
 * HI_ADDI(base, c_a) and HI_ADDI(base, c_b) with |c_b - c_a| matching
 * the access size, force distinct colors via interference edges:
 *
 *   - A vs B (LDP rt1 != rt2)
 *   - A vs base, B vs base (LDP rt != Xn)
 *   - B vs every value-producing inst in (A, B) (so B's color isn't
 *     coalesced with anything live in that range; this is what lets
 *     the codegen pair them at A's emission point without clobbering
 *     a live value).
 *
 * Stops scanning at HI_STORE / HI_CALL / HI_CALLP (alias / clobber
 * barriers).  Only triggers for integer types; FP loads use V-class
 * regs and have their own pairing path.
 *
 * Called after gc_build so the graph is allocated. */
static void hx_pair_load_interference(void) {
    int a; int b;
    int base_a; int disp_a;
    int wide_a; int step;
    int scan_lim;
    int na; int nb; int n_base;

    a = 0;
    while (a < h_ninst) {
        if (h_kind[a] != HI_LOAD) { a = a + 1; continue; }
        if (ty_is_fp(h_ty[a]))    { a = a + 1; continue; }
        na = gc_node[a];
        if (na < 0) { a = a + 1; continue; }
        if (h_src1[a] < 0 || h_kind[h_src1[a]] != HI_ADDI) { a = a + 1; continue; }
        base_a = h_src1[h_src1[a]];
        disp_a = h_val[h_src1[a]];
        if (base_a < 0) { a = a + 1; continue; }
        n_base = gc_node[base_a];
        wide_a = ty_is_ptr(h_ty[a]) || ty_is_llong(h_ty[a]);
        step = wide_a ? 8 : 4;

        /* Bound forward search: pairs more than ~32 insts apart almost
         * never survive to codegen (some intervening op will alias).
         * Linear scan keeps the pass O(n × K) with small K. */
        scan_lim = a + 32;
        if (scan_lim > h_ninst) scan_lim = h_ninst;
        b = a + 1;
        while (b < scan_lim && h_blk[b] == h_blk[a]) {
            int kb;
            kb = h_kind[b];
            if (kb == HI_NOP || kb == HI_PHI) { b = b + 1; continue; }
            if (kb == HI_STORE || kb == HI_CALL || kb == HI_CALLP) break;
            if (kb == HI_LOAD && !ty_is_fp(h_ty[b])
                    && (ty_is_ptr(h_ty[b]) || ty_is_llong(h_ty[b])) == wide_a
                    && h_src1[b] >= 0 && h_kind[h_src1[b]] == HI_ADDI
                    && h_src1[h_src1[b]] == base_a) {
                int disp_b;
                int diff;
                int min_off;
                disp_b = h_val[h_src1[b]];
                diff = disp_b - disp_a;
                if (diff == step || diff == -step) {
                    if (diff > 0) min_off = disp_a;
                    else          min_off = disp_b;
                    if ((wide_a && min_off >= -512 && min_off <= 504
                                && (min_off & 7) == 0)
                       || (!wide_a && min_off >= -256 && min_off <= 252
                                && (min_off & 3) == 0)) {
                        int x;
                        nb = gc_node[b];
                        if (nb >= 0) {
                            gc_add_edge(na, nb);
                            if (n_base >= 0) {
                                gc_add_edge(na, n_base);
                                gc_add_edge(nb, n_base);
                            }
                            x = a + 1;
                            while (x < b) {
                                int nx;
                                nx = gc_node[x];
                                if (nx >= 0) gc_add_edge(nb, nx);
                                x = x + 1;
                            }
                        }
                        break;
                    }
                }
            }
            b = b + 1;
        }
        a = a + 1;
    }
}

/* Top-level graph-coloring allocation */
static void gc_alloc(void) {
    gc_build();
    gc_drop_phi_edges();
    hx_pair_load_interference();
    gc_find_moves();
    gc_irc();
    gc_select();
    gc_writeback();
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

    /* Assign 8-byte spill slots for non-allocated value-producing instructions.
     * hl_temp_stack and ra_spill_off are in slow32 frontend units (4 bytes);
     * cg_a64_offset doubles them to 8-byte AArch64 byte offsets, so a 4-unit
     * increment yields one 8-byte spill slot. */
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

    /* Assign 8-byte callee-save slots (only for callee-saved registers).
     * Same convention as spill slots: increment hl_temp_stack by 4 slow32
     * units per 8-byte AArch64 slot (cg_a64_offset doubles it). */
    ra_ncsave = 0;
    r = 0;
    while (r < RA_NPHY) {
        if (ra_used[r] && ra_a64_is_callee[r]) {
            hl_temp_stack = hl_temp_stack + 4;
            ra_csave_reg[ra_ncsave] = ra_a64_phys[r];
            ra_csave_off[ra_ncsave] = 0 - hl_temp_stack;
            ra_ncsave = ra_ncsave + 1;
        }
        r = r + 1;
    }
}

/* =================================================================
 * Call-crossing detection
 *
 * Mark values whose live intervals span a CALL/CALLP instruction.
 * These must be allocated to callee-saved registers only.
 * ================================================================= */

#define RA_MAX_CALLS 512
static int ra_call_positions[RA_MAX_CALLS];
static int ra_ncalls;

static void ra_mark_call_crossing(void) {
    int i;
    int inst;
    int k;
    int j;
    int cp;

    /* Collect CALL/CALLP positions */
    ra_ncalls = 0;
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];
        if ((k == HI_CALL || k == HI_CALLP || k == HI_A64_DBT_TRAMPOLINE) && ra_ncalls < RA_MAX_CALLS) {
            ra_call_positions[ra_ncalls] = ra_pos[inst];
            ra_ncalls = ra_ncalls + 1;
        }
        i = i + 1;
    }

    /* For each value, check if any call falls within its live interval */
    i = 0;
    while (i < h_ninst) {
        ra_crosses_call[i] = 0;
        if (ra_pos[i] >= 0 && ra_iend[i] > ra_pos[i]) {
            j = 0;
            while (j < ra_ncalls) {
                cp = ra_call_positions[j];
                if (cp > ra_pos[i] && cp <= ra_iend[i]) {
                    ra_crosses_call[i] = 1;
                    break;
                }
                j = j + 1;
            }
        }
        i = i + 1;
    }
}

/* =================================================================
 * CX/DX clobber detection
 *
 * Mark values whose live intervals span an instruction that
 * architecturally clobbers RCX (variable shifts, DIV/REM) or
 * RDX (DIV/REM).  These values cannot be allocated to the
 * corresponding register.
 * ================================================================= */

#define RA_MAX_CLOBBERS 1024
static int ra_cx_clobber_positions[RA_MAX_CLOBBERS];
static int ra_dx_clobber_positions[RA_MAX_CLOBBERS];
static int ra_ncx_clobbers;
static int ra_ndx_clobbers;

static void ra_mark_clobbers(void) {
    int i;
    /* AArch64 has no architectural register clobbers from variable shifts
     * or DIV/REM (UDIV/SDIV and LSLV/LSRV/ASRV all use general regs).  The
     * arrays are kept for IRC algorithm compatibility but are always zero. */
    ra_ncx_clobbers = 0;
    ra_ndx_clobbers = 0;
    i = 0;
    while (i < h_ninst) {
        ra_crosses_cx_clobber[i] = 0;
        ra_crosses_dx_clobber[i] = 0;
        i = i + 1;
    }
}

/* =================================================================
 * Compare-branch fusion
 *
 * Identifies BRC instructions whose condition is a single-use
 * comparison (SEQ..SGEU).  The comparison is NOPed and its operand
 * live ranges extended to the BRC, so the codegen can emit
 * CMP + Jcc directly instead of SETcc + MOVZX + TEST + Jcc.
 * ================================================================= */

static int hx_brc_fuse[HIR_MAX_INST];   /* BRC idx → fused comparison idx, -1 = none */
static int hx_cmp_fused[HIR_MAX_INST];  /* 1 if comparison is fused into a BRC */
static int hx_cmp_kind[HIR_MAX_INST];   /* saved comparison kind (before NOP) */
static int hx_use_count[HIR_MAX_INST];  /* use count per instruction */

/* BIC / ORN / EON peephole.  When set, the AND/OR/XOR at this index
 * had one operand of the form HI_BNOT(y) with single use; the codegen
 * emits the corresponding negated-Rm instruction.  Identification
 * runs post-BURG, pre-regalloc (so regalloc sees y's live range
 * extended to the binop's use site).  Side effects of the pass:
 *   - The HI_BNOT instruction is NOPed.
 *   - The binop's src2 is rewritten to y (BNOT's child).  If the
 *     BNOT was on src1, src1 and src2 are swapped first (commutative). */
static int hx_bic_fuse[HIR_MAX_INST];   /* 1 = emit BIC/ORN/EON */

static int hx_is_cmp(int k) {
    if (k >= HI_SEQ && k <= HI_SGEU) return 1;
    if (k == HI_FEQ || k == HI_FLT || k == HI_FLE) return 1;
    return 0;
}

static void hx_identify_fusions(void) {
    int i;
    int k;
    int root;
    int rk;
    int lim;
    int ok;

    /* Initialize fusion arrays (use counts come from BURG's bg_uses[]) */
    i = 0;
    while (i < h_ninst) {
        hx_brc_fuse[i] = -1;
        hx_cmp_fused[i] = 0;
        i = i + 1;
    }

    /* Identify fusable BRC instructions */
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k != HI_BRC) { i = i + 1; continue; }

        ok = 1;

        /* Chase COPY chain on s1 to find root comparison */
        root = h_src1[i];
        lim = 0;
        while (root >= 0 && h_kind[root] == HI_COPY && lim < 64) {
            if (bg_uses[root] != 1 || h_blk[root] != h_blk[i]) {
                ok = 0;
                break;
            }
            root = h_src1[root];
            lim = lim + 1;
        }
        if (!ok || root < 0) { i = i + 1; continue; }

        rk = h_kind[root];
        if (!hx_is_cmp(rk)) { i = i + 1; continue; }
        if (bg_uses[root] != 1) { i = i + 1; continue; }
        if (h_blk[root] != h_blk[i]) { i = i + 1; continue; }

        /* Reject if comparison operands are NOPed */
        if (h_src1[root] >= 0 && h_kind[h_src1[root]] == HI_NOP) {
            i = i + 1; continue;
        }
        if (h_src2[root] >= 0 && h_kind[h_src2[root]] == HI_NOP) {
            i = i + 1; continue;
        }

        hx_brc_fuse[i] = root;
        hx_cmp_fused[root] = 1;
        hx_cmp_kind[root] = rk;

        /* Mark intermediate COPYs for suppression */
        {
            int c;
            c = h_src1[i];
            lim = 0;
            while (c >= 0 && c != root && h_kind[c] == HI_COPY && lim < 64) {
                hx_cmp_fused[c] = 1;
                c = h_src1[c];
                lim = lim + 1;
            }
        }

        i = i + 1;
    }
}

/* Identify BIC/ORN/EON peephole opportunities post-BURG, pre-regalloc.
 * For HI_AND/OR/XOR(x, BNOT(y)) where the BNOT has exactly one use and
 * lives in the same block, rewrite the binop to use y directly, NOP
 * the BNOT, and mark hx_bic_fuse[binop]=1 so codegen emits BIC/ORN/EON.
 * Commutative case (BNOT on src1) gets a swap first. */
static void hx_identify_bic_peephole(void) {
    int i;
    int k;

    i = 0;
    while (i < h_ninst) {
        hx_bic_fuse[i] = 0;
        i = i + 1;
    }

    i = 0;
    while (i < h_ninst) {
        int s1; int s2;
        int bnot_idx;
        int y;
        k = h_kind[i];
        if (k != HI_AND && k != HI_OR && k != HI_XOR) { i = i + 1; continue; }
        s1 = h_src1[i];
        s2 = h_src2[i];
        bnot_idx = -1;
        if (s2 >= 0 && h_kind[s2] == HI_BNOT
            && bg_uses[s2] == 1 && h_blk[s2] == h_blk[i]) {
            bnot_idx = s2;
        } else if (s1 >= 0 && h_kind[s1] == HI_BNOT
                && bg_uses[s1] == 1 && h_blk[s1] == h_blk[i]) {
            bnot_idx = s1;
            /* Swap so BNOT is on the right (BIC's Rm is the negated). */
            h_src1[i] = s2;
            h_src2[i] = s1;
            s1 = s2;
            s2 = bnot_idx;
        }
        if (bnot_idx < 0) { i = i + 1; continue; }
        y = h_src1[bnot_idx];
        if (y < 0) { i = i + 1; continue; }
        /* Rewrite: binop(x, BNOT(y)) → binop(x, y) with bic flag. */
        h_src2[i] = y;
        hx_bic_fuse[i] = 1;
        h_kind[bnot_idx] = HI_NOP;
        i = i + 1;
    }
}

/* Extend live ranges for fused comparisons and NOP them. */
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
        cmp = hx_brc_fuse[i];
        if (cmp < 0) { i = i + 1; continue; }

        brc_pos = ra_pos[i];
        if (brc_pos < 0) {
            hx_brc_fuse[i] = -1;
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

        /* NOP intermediate COPYs */
        c = h_src1[i];
        lim = 0;
        while (c >= 0 && c != cmp && lim < 64) {
            if (hx_cmp_fused[c]) h_kind[c] = HI_NOP;
            c = h_src1[c];
            lim = lim + 1;
        }

        i = i + 1;
    }
}


/* =================================================================
 * Invariant checks
 *
 * These run after regalloc with CC_A64_VERIFY=1 in the environment.
 * They catch the bug classes that have caused recent silent miscompiles
 * on cc-a64:
 *   - use-before-def in linearized order (LICM ordering, scheduler)
 *   - register-class collision: an interference edge with same physical
 *     register on both endpoints (silent IRC graph dropout, the
 *     gc_drop_phi_edges over-aggressive coalescing class)
 *   - PHI/CFG inconsistency: PHI's pred list out of sync with ssa_pred
 *   - duplicate spill slot for distinct, simultaneously-live SSA values
 * On any failure they print a diagnostic to stderr and exit(1) -- it's
 * better to fail loudly at compile time than silently miscompile.
 *
 * Mirrors the cc-x64 ra_verify (CC_X64_VERIFY) introduced in 5647d971.
 * Adapted for a64: HI_A64_DBT_TRAMPOLINE replaces HI_X64_DBT_TRAMPOLINE
 * in the call-arg use-before-def scan.
 * ================================================================= */

static void ra_verify(char *fn_name) {
    int i;
    int j;
    int inst;
    int p;
    int s;
    int err;
    int k;

    err = 0;

    /* (1) Use-before-def: every operand reference must have been
     * linearized at a position strictly less than this instruction's,
     * unless it's a PHI (live at block start), a PARAM (live at function
     * entry), or a rematerializable value (regenerated on demand). */
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        p = ra_pos[inst];
        k = h_kind[inst];

        s = h_src1[inst];
        if (s >= 0 && ra_pos[s] >= 0 && h_kind[s] != HI_PHI &&
            h_kind[s] != HI_PARAM &&
            !hi_is_remat(h_kind[s]) && ra_pos[s] >= p) {
            fdputs("RA_VERIFY: use-before-def in ", 2);
            fdputs(fn_name, 2);
            fdputs(": inst=", 2); fdputuint(2, inst);
            fdputs(" k=", 2); fdputuint(2, k);
            fdputs(" blk=", 2); fdputuint(2, h_blk[inst]);
            fdputs(" pos=", 2); fdputuint(2, p);
            fdputs(" src1=", 2); fdputuint(2, s);
            fdputs(" src1_k=", 2); fdputuint(2, h_kind[s]);
            fdputs(" src1_blk=", 2); fdputuint(2, h_blk[s]);
            fdputs(" src1_pos=", 2); fdputuint(2, ra_pos[s]);
            fdputs("\n", 2);
            err = err + 1;
        }
        if (h_src2[inst] >= 0 && (ho_src2_is_ref(k) || k == HI_LOAD)) {
            s = h_src2[inst];
            if (s >= 0 && ra_pos[s] >= 0 && h_kind[s] != HI_PHI &&
                h_kind[s] != HI_PARAM &&
                !hi_is_remat(h_kind[s]) && ra_pos[s] >= p) {
                fdputs("RA_VERIFY: src2 use-before-def in ", 2);
                fdputs(fn_name, 2);
                fdputs(": inst=", 2); fdputuint(2, inst);
                fdputs(" pos=", 2); fdputuint(2, p);
                fdputs(" src2=", 2); fdputuint(2, s);
                fdputs(" src2_pos=", 2); fdputuint(2, ra_pos[s]);
                fdputs("\n", 2);
                err = err + 1;
            }
        }
        if (k == HI_CALL || k == HI_CALLP || k == HI_A64_DBT_TRAMPOLINE) {
            if (h_cbase[inst] >= 0) {
                j = 0;
                while (j < h_val[inst]) {
                    s = h_carg[h_cbase[inst] + j];
                    if (s >= 0 && ra_pos[s] >= 0 && h_kind[s] != HI_PHI &&
                        h_kind[s] != HI_PARAM &&
                        !hi_is_remat(h_kind[s]) && ra_pos[s] >= p) {
                        fdputs("RA_VERIFY: carg use-before-def in ", 2);
                        fdputs(fn_name, 2);
                        fdputs(": call=", 2); fdputuint(2, inst);
                        fdputs(" pos=", 2); fdputuint(2, p);
                        fdputs(" carg[", 2); fdputuint(2, j);
                        fdputs("]=", 2); fdputuint(2, s);
                        fdputs(" pos=", 2); fdputuint(2, ra_pos[s]);
                        fdputs("\n", 2);
                        err = err + 1;
                    }
                    j = j + 1;
                }
            }
        }
        i = i + 1;
    }

    /* (2) Same-color interfering pair: two values with an interference
     * edge that nevertheless share a physical register.  This catches
     * the IRC-edge-overflow / over-aggressive-coalesce class of bug at
     * its source -- including the gc_drop_phi_edges regression that
     * previously corrupted strtok_r when a phi-arg with non-PHI uses
     * was coalesced with its phi-result. */
    i = 0;
    while (i < gc_nnode) {
        int e = gc_adj_head[i];
        int my_color = gc_color[gc_get_alias(i)];
        int my_inst = gc_inst[i];
        int my_reg = my_inst >= 0 ? ra_reg[my_inst] : -1;
        if (my_reg >= 0) {
            while (e >= 0) {
                int peer = gc_adj_peer[e];
                int pa = gc_get_alias(peer);
                int peer_inst = gc_inst[pa];
                int peer_reg = peer_inst >= 0 ? ra_reg[peer_inst] : -1;
                if (peer_reg == my_reg && peer != i) {
                    fdputs("RA_VERIFY: interfering pair shares reg in ", 2);
                    fdputs(fn_name, 2);
                    fdputs(": node=", 2); fdputuint(2, i);
                    fdputs(" peer=", 2); fdputuint(2, peer);
                    fdputs(" reg=", 2); fdputuint(2, my_reg);
                    fdputs(" my_color=", 2);
                    if (my_color >= 0) fdputuint(2, my_color); else fdputs("-", 2);
                    fdputs("\n", 2);
                    err = err + 1;
                    break;  /* one diagnostic per node is enough */
                }
                e = gc_adj_next[e];
            }
        }
        i = i + 1;
    }

    /* (3a) PHI/CFG consistency: every PHI's pred list (h_pblk[]) must
     * exactly match the block's current predecessor set (ssa_pred[]).
     * If a PHI lists a pred that isn't in ssa_pred[], the parallel-copy
     * for that edge will be emitted with arg=-1 -> hx_mat zeroes the
     * scratch register and the PHI's spill slot gets 0 from a phantom
     * edge.  If a PHI is missing a pred that DOES exist, the pred's
     * branch will emit a copy that finds no slot to write to, leaving
     * the PHI's slot uninitialised on that edge. */
    {
        int p_inst;
        int p_blk;
        int p_base;
        int p_cnt;
        int pred_idx;
        int pred_blk;
        int phi_pred_blk;
        int found;
        p_inst = 0;
        while (p_inst < h_ninst) {
            if (h_kind[p_inst] == HI_PHI) {
                p_blk = h_blk[p_inst];
                p_base = h_pbase[p_inst];
                p_cnt = h_pcnt[p_inst];
                /* Every actual predecessor of the block must appear in PHI's list. */
                pred_idx = 0;
                while (pred_idx < ssa_npred[p_blk]) {
                    pred_blk = ssa_pred[ssa_pbase[p_blk] + pred_idx];
                    found = 0;
                    j = 0;
                    while (j < p_cnt) {
                        phi_pred_blk = h_pblk[p_base + j];
                        if (phi_pred_blk == pred_blk) { found = 1; break; }
                        j = j + 1;
                    }
                    if (!found) {
                        fdputs("RA_VERIFY: PHI missing pred in ", 2);
                        fdputs(fn_name, 2);
                        fdputs(": phi=", 2); fdputuint(2, p_inst);
                        fdputs(" blk=", 2); fdputuint(2, p_blk);
                        fdputs(" missing pred_blk=", 2); fdputuint(2, pred_blk);
                        fdputs(" (cfg has ", 2); fdputuint(2, ssa_npred[p_blk]);
                        fdputs(" preds, phi has ", 2); fdputuint(2, p_cnt);
                        fdputs(")\n", 2);
                        err = err + 1;
                    }
                    pred_idx = pred_idx + 1;
                }
                /* Every PHI-listed pred must exist in current CFG (no stale entries). */
                j = 0;
                while (j < p_cnt) {
                    phi_pred_blk = h_pblk[p_base + j];
                    found = 0;
                    pred_idx = 0;
                    while (pred_idx < ssa_npred[p_blk]) {
                        pred_blk = ssa_pred[ssa_pbase[p_blk] + pred_idx];
                        if (pred_blk == phi_pred_blk) { found = 1; break; }
                        pred_idx = pred_idx + 1;
                    }
                    if (!found) {
                        fdputs("RA_VERIFY: PHI stale pred in ", 2);
                        fdputs(fn_name, 2);
                        fdputs(": phi=", 2); fdputuint(2, p_inst);
                        fdputs(" blk=", 2); fdputuint(2, p_blk);
                        fdputs(" stale pred_blk=", 2); fdputuint(2, phi_pred_blk);
                        fdputs("\n", 2);
                        err = err + 1;
                    }
                    j = j + 1;
                }
            }
            p_inst = p_inst + 1;
        }
    }

    /* (2b) PHI-coalesce soundness: gc_drop_phi_edges removes the
     * interference edge between a phi-result and each phi-arg before
     * regalloc runs, so by the time check (2) above looks at the IG,
     * the over-aggressive-drop class of bug has already vanished from
     * the graph.  Catch it directly: for every PHI whose result was
     * coalesced with a phi-arg (same physical register), assert the
     * arg has no non-PHI use at a position >= the phi-result's def.
     * Such a use would keep the arg live past the implicit copy at
     * the predecessor terminator, so coalescing the two corrupts the
     * post-phi reads -- exactly the strtok_r-class bug. */
    {
        int p_inst;
        int phi_reg;
        int phi_pos;
        int arg;
        int xi;
        int xk;
        int unsafe;
        p_inst = 0;
        while (p_inst < h_ninst) {
            if (h_kind[p_inst] == HI_PHI && h_pbase[p_inst] >= 0 &&
                ra_reg[p_inst] >= 0) {
                phi_reg = ra_reg[p_inst];
                phi_pos = ra_pos[p_inst];
                j = 0;
                while (j < h_pcnt[p_inst]) {
                    arg = h_pval[h_pbase[p_inst] + j];
                    /* Skip back-edge args: those whose def follows the
                     * phi in linearized order live entirely inside the
                     * loop body and can legitimately share the phi's
                     * register (their uses are all on the path back to
                     * the phi via the back-edge).  Only forward args
                     * (arg defined before phi) risk the strtok_r-class
                     * miscompile. */
                    if (arg >= 0 && arg != p_inst && ra_reg[arg] == phi_reg &&
                        ra_pos[arg] >= 0 && ra_pos[arg] < phi_pos) {
                        /* Coalesced.  Walk all insts looking for a
                         * non-PHI user of `arg` at position >= phi_pos. */
                        unsafe = -1;
                        xi = 0;
                        while (xi < h_ninst) {
                            xk = h_kind[xi];
                            if (xk == HI_PHI || xk == HI_NOP || ra_pos[xi] < phi_pos) {
                                xi = xi + 1; continue;
                            }
                            if (h_src1[xi] == arg) { unsafe = xi; break; }
                            if (h_src2[xi] == arg &&
                                (ho_src2_is_ref(xk) || xk == HI_LOAD)) {
                                unsafe = xi; break;
                            }
                            if ((xk == HI_CALL || xk == HI_CALLP ||
                                 xk == HI_A64_DBT_TRAMPOLINE) &&
                                h_cbase[xi] >= 0) {
                                int ai = 0;
                                int matched = 0;
                                while (ai < h_val[xi]) {
                                    if (h_carg[h_cbase[xi] + ai] == arg) {
                                        matched = 1; break;
                                    }
                                    ai = ai + 1;
                                }
                                if (matched) { unsafe = xi; break; }
                            }
                            xi = xi + 1;
                        }
                        if (unsafe >= 0) {
                            fdputs("RA_VERIFY: unsafe phi coalesce in ", 2);
                            fdputs(fn_name, 2);
                            fdputs(": phi=", 2); fdputuint(2, p_inst);
                            fdputs(" arg=", 2); fdputuint(2, arg);
                            fdputs(" reg=", 2); fdputuint(2, phi_reg);
                            fdputs(" use_inst=", 2); fdputuint(2, unsafe);
                            fdputs(" use_pos=", 2); fdputuint(2, ra_pos[unsafe]);
                            fdputs(" phi_pos=", 2); fdputuint(2, phi_pos);
                            fdputs("\n", 2);
                            err = err + 1;
                        }
                    }
                    j = j + 1;
                }
            }
            p_inst = p_inst + 1;
        }
    }

    /* (3) Spill-slot collision: two distinct value-producing insts
     * sharing the same negative-fp offset.  Today this should never
     * happen because ra_assign_spills hands out a fresh slot per inst,
     * but assert it -- if a future change adds slot reuse, it must
     * preserve non-overlapping intervals. */
    i = 0;
    while (i < h_ninst) {
        if (ra_spill_off[i] != 0) {
            j = i + 1;
            while (j < h_ninst) {
                if (ra_spill_off[j] == ra_spill_off[i]) {
                    fdputs("RA_VERIFY: spill-slot collision in ", 2);
                    fdputs(fn_name, 2);
                    fdputs(": inst=", 2); fdputuint(2, i);
                    fdputs(" inst2=", 2); fdputuint(2, j);
                    fdputs(" off=(neg)\n", 2);
                    err = err + 1;
                }
                j = j + 1;
            }
        }
        i = i + 1;
    }

    if (err > 0) {
        fdputs("RA_VERIFY: ", 2);
        fdputuint(2, err);
        fdputs(" failure(s) in ", 2);
        fdputs(fn_name, 2);
        fdputs(" -- aborting compile\n", 2);
        exit(1);
    }
}

/* =================================================================
 * Main entry point
 * ================================================================= */

static void hir_regalloc(void) {
    ra_init_a64_regs();
    ra_compute_pos();
    ra_compute_ends();
    ra_extend_fused_cmp();
    ra_mark_call_crossing();
    ra_mark_clobbers();
    gc_alloc();
    ra_assign_spills();
}

#endif /* HIR_REGALLOC_A64_H */
