/* hir_regalloc_x64.h -- IRC graph-coloring register allocation for x86-64
 *
 * Forked from stage07/hir_regalloc.h (SLOW-32 target).
 * Allocates 13 registers: callee-saved RBX, R12-R15 (5) plus
 * caller-saved RSI, RDI, RCX, RDX, R8-R11 (8).
 * RAX is reserved as scratch for the codegen.
 * RCX/RDX are allocatable but constrained at clobber points
 * (DIV/REM, variable shifts).
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

/* Side arrays (defined in hir_codegen_x64.h, forward-declared here
 * so liveness analysis can track extra operands). */
static int hx_sib_index[HIR_MAX_INST];     /* SIB index for stores */
static int hx_alu_sib_idx[HIR_MAX_INST];   /* SIB index for ALU+SIB ops */

/* --- Configuration --- */
#define RA_NPHY 13  /* callee: RBX, R12-R15; caller: RSI, RDI, RCX, RDX, R8-R11 */
#define RA_SLOT_RCX 7
#define RA_SLOT_RDX 8
#define RA_NCALLEE  5  /* number of callee-saved slots (0..4) */

/* Mapping: slot index → x64 physical register encoding */
static int ra_x64_phys[RA_NPHY];

/* Per-slot: 1 = callee-saved, 0 = caller-saved */
static int ra_x64_is_callee[RA_NPHY];

/* Reverse mapping: x64 register encoding → slot index (-1 = not allocatable) */
#define RA_X64_REVERSE_SIZE 16
static int ra_x64_slot[RA_X64_REVERSE_SIZE];

/* Call-crossing flag: 1 if value's live interval spans a CALL */
static int ra_crosses_call[HIR_MAX_INST];

/* Clobber flags: 1 if value's live interval spans an instruction that
 * architecturally clobbers RCX (variable shifts, DIV/REM) or RDX (DIV/REM).
 * Values with these flags cannot be allocated to the corresponding register. */
static int ra_crosses_cx_clobber[HIR_MAX_INST];
static int ra_crosses_dx_clobber[HIR_MAX_INST];

static void ra_init_x64_regs(void) {
    int i;
    /* Callee-saved registers (slots 0-4) */
    ra_x64_phys[0] = X64_RBX;  ra_x64_is_callee[0] = 1;
    ra_x64_phys[1] = X64_R12;  ra_x64_is_callee[1] = 1;
    ra_x64_phys[2] = X64_R13;  ra_x64_is_callee[2] = 1;
    ra_x64_phys[3] = X64_R14;  ra_x64_is_callee[3] = 1;
    ra_x64_phys[4] = X64_R15;  ra_x64_is_callee[4] = 1;
    /* Caller-saved registers (slots 5-12): non-REX first for smaller code */
    ra_x64_phys[5]  = X64_RSI;  ra_x64_is_callee[5]  = 0;
    ra_x64_phys[6]  = X64_RDI;  ra_x64_is_callee[6]  = 0;
    ra_x64_phys[7]  = X64_RCX;  ra_x64_is_callee[7]  = 0;  /* RA_SLOT_RCX */
    ra_x64_phys[8]  = X64_RDX;  ra_x64_is_callee[8]  = 0;  /* RA_SLOT_RDX */
    ra_x64_phys[9]  = X64_R8;   ra_x64_is_callee[9]  = 0;
    ra_x64_phys[10] = X64_R9;   ra_x64_is_callee[10] = 0;
    ra_x64_phys[11] = X64_R10;  ra_x64_is_callee[11] = 0;
    ra_x64_phys[12] = X64_R11;  ra_x64_is_callee[12] = 0;
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

/* --- Graph-coloring register allocator (IRC) --- */

/* Limits */
#define GC_MAX_NODE  4096
#define GC_MAX_EDGE  131072
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
 * Step 3: Graph-coloring register allocation (IRC)
 *
 * Iterated Register Coalescing (George-Appel, 1996).
 * Replaces linear scan with global interference graph coloring.
 * ================================================================= */

/* SysV ABI arg registers (mirrors hx_arg_reg[] from codegen) */
static int ra_arg_regs[6];

static void ra_init_arg_regs(void) {
    ra_arg_regs[0] = X64_RDI;
    ra_arg_regs[1] = X64_RSI;
    ra_arg_regs[2] = X64_RDX;
    ra_arg_regs[3] = X64_RCX;
    ra_arg_regs[4] = X64_R8;
    ra_arg_regs[5] = X64_R9;
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

            /* Expire dead intervals */
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

            /* Add interference edges between new node and all active */
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

        /* Destructive ALU: src1 wants same register as dst */
        if (k >= HI_ADD && k <= HI_SRL) {
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

/* Number of available colors for a node.  Call-crossing nodes can only
 * use callee-saved registers (5).  Others use all 13, minus any
 * CX/DX clobber constraints. */
static int gc_k(int n) {
    int inst;
    int k;
    inst = gc_inst[n];
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

    /* Add edges: for each neighbor t of v, add edge(t, u) */
    e = gc_adj_head[v];
    while (e >= 0) {
        t = gc_adj_peer[e];
        if (gc_wl[t] != GC_WL_SELECT && gc_wl[t] != GC_WL_COALESCED) {
            gc_add_edge(t, u);
            gc_dec_degree(t);
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

    /* Pop select stack in reverse */
    i = gc_nsel - 1;
    while (i >= 0) {
        n = gc_sel_stk[i];
        inst = gc_inst[n];
        k = gc_k(n);

        /* Mark colors used by live neighbors */
        c = 0;
        while (c < RA_NPHY) {
            used[c] = 0;
            c = c + 1;
        }

        e = gc_adj_head[n];
        while (e >= 0) {
            peer = gc_adj_peer[e];
            pa = gc_get_alias(peer);
            pc = gc_color[pa];
            if (pc >= 0) {
                used[pc] = 1;
            }
            e = gc_adj_next[e];
        }

        /* Also mark CX/DX clobber-constrained slots as used */
        if (ra_crosses_cx_clobber[inst]) used[RA_SLOT_RCX] = 1;
        if (ra_crosses_dx_clobber[inst]) used[RA_SLOT_RDX] = 1;
        /* Folded ALU+SIB uses RCX/RDX as address/load temporaries in codegen,
         * so the destination cannot live in either register. */
        if (hx_alu_sib_idx[inst] >= 0 &&
            (h_kind[inst] == HI_ADD || h_kind[inst] == HI_AND ||
             h_kind[inst] == HI_OR  || h_kind[inst] == HI_XOR)) {
            used[RA_SLOT_RCX] = 1;
            used[RA_SLOT_RDX] = 1;
        }
        /* Variable shifts consume CL for the count, so the result cannot
         * be allocated to RCX. Immediate shifts are fine. */
        if ((h_kind[inst] == HI_SLL || h_kind[inst] == HI_SRA ||
             h_kind[inst] == HI_SRL) &&
            (h_src2[inst] < 0 || h_kind[h_src2[inst]] != HI_ICONST)) {
            used[RA_SLOT_RCX] = 1;
        }

        /* Constraint: call-crossing → must use callee-saved (slots 0-4) */
        if (ra_crosses_call[inst]) {
            /* Try PARAM hint first */
            hint = -1;
            if (h_kind[inst] == HI_PARAM) {
                pidx = h_val[inst];
                if (pidx >= 0 && pidx < 6) {
                    c = ra_x64_slot[ra_arg_regs[pidx]];
                    if (c >= 0 && c < RA_NCALLEE && !used[c]) hint = c;
                }
            }
            if (hint >= 0) {
                gc_color[n] = hint;
            } else {
                gc_color[n] = -1;
                c = 0;
                while (c < RA_NCALLEE) {
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
                if (pidx >= 0 && pidx < 6) {
                    c = ra_x64_slot[ra_arg_regs[pidx]];
                    if (c >= 0 && !used[c]) hint = c;
                }
            }

            /* ALU hint: try src1's color for destructive ops */
            if (hint < 0 && h_kind[inst] >= HI_ADD && h_kind[inst] <= HI_SRL) {
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
                 * by uncolored PARAM neighbors — but only if doing so doesn't
                 * force all temps into REX-prefix registers (R8-R11).
                 * Count available non-REX, non-avoided caller-saved slots;
                 * if >= 1 remains, apply bias. */
                {
                    int avoid[RA_NPHY];
                    int ac;
                    int ae;
                    int ap;
                    int aa;
                    int apidx;
                    int awant;
                    int non_rex_avail;

                    ac = 0;
                    while (ac < RA_NPHY) {
                        avoid[ac] = 0;
                        ac = ac + 1;
                    }

                    if (h_kind[inst] != HI_PARAM) {
                        ae = gc_adj_head[n];
                        while (ae >= 0) {
                            ap = gc_adj_peer[ae];
                            aa = gc_get_alias(ap);
                            if (gc_color[aa] < 0 && h_kind[gc_inst[aa]] == HI_PARAM) {
                                apidx = h_val[gc_inst[aa]];
                                if (apidx >= 0 && apidx < 6) {
                                    awant = ra_x64_slot[ra_arg_regs[apidx]];
                                    if (awant >= 0) avoid[awant] = 1;
                                }
                            }
                            ae = gc_adj_next[ae];
                        }

                        /* Check if bias leaves any non-REX caller-saved option.
                         * Non-REX caller-saved: RSI(5), RDI(6), RCX(7), RDX(8). */
                        non_rex_avail = 0;
                        if (!used[5] && !avoid[5]) non_rex_avail = non_rex_avail + 1;
                        if (!used[6] && !avoid[6]) non_rex_avail = non_rex_avail + 1;
                        if (!used[7] && !avoid[7]) non_rex_avail = non_rex_avail + 1;
                        if (!used[8] && !avoid[8]) non_rex_avail = non_rex_avail + 1;
                        if (non_rex_avail == 0) {
                            /* Bias would force all temps to REX regs — disable */
                            ac = 0;
                            while (ac < RA_NPHY) {
                                avoid[ac] = 0;
                                ac = ac + 1;
                            }
                        }
                    }

                    /* Prefer caller-saved, avoiding PARAM-wanted colors */
                    c = 5;
                    while (c < RA_NPHY) {
                        if (!used[c] && !avoid[c]) {
                            gc_color[n] = c;
                            break;
                        }
                        c = c + 1;
                    }
                    /* Fallback: caller-saved even if PARAM-wanted */
                    if (gc_color[n] < 0) {
                        c = 5;
                        while (c < RA_NPHY) {
                            if (!used[c]) {
                                gc_color[n] = c;
                                break;
                            }
                            c = c + 1;
                        }
                    }
                    /* Fallback: callee-saved */
                    if (gc_color[n] < 0) {
                        c = 0;
                        while (c < RA_NCALLEE) {
                            if (!used[c]) {
                                gc_color[n] = c;
                                break;
                            }
                            c = c + 1;
                        }
                    }
                }
            }
        }

        i = i - 1;
    }

    /* Propagate colors to coalesced nodes */
    i = 0;
    while (i < gc_nnode) {
        if (gc_wl[i] == GC_WL_COALESCED) {
            gc_color[i] = gc_color[gc_get_alias(i)];
        }
        i = i + 1;
    }

    /* Post-coloring PARAM fix-up: if a PARAM didn't get its ABI register,
     * and no neighbor has that color, take it to avoid prologue moves. */
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

        ra_init_arg_regs();
        n = 0;
        while (n < gc_nnode) {
            inst = gc_inst[n];
            if (h_kind[inst] == HI_PARAM && gc_color[n] >= 0 && !ra_crosses_call[inst]) {
                pidx = h_val[inst];
                if (pidx >= 0 && pidx < 6) {
                    want = ra_x64_slot[ra_arg_regs[pidx]];
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
                        }
                    }
                }
            }
            n = n + 1;
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

    /* Write colors to ra_reg[] */
    n = 0;
    while (n < gc_nnode) {
        inst = gc_inst[n];
        c = gc_color[n];
        if (c >= 0) {
            ra_reg[inst] = ra_x64_phys[c];
            ra_used[c] = 1;
        }
        n = n + 1;
    }
}

/* Top-level graph-coloring allocation */
static void gc_alloc(void) {
    gc_build();
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

    /* Assign 8-byte callee-save slots (only for callee-saved registers) */
    ra_ncsave = 0;
    r = 0;
    while (r < RA_NPHY) {
        if (ra_used[r] && ra_x64_is_callee[r]) {
            hl_temp_stack = hl_temp_stack + 8;
            ra_csave_reg[ra_ncsave] = ra_x64_phys[r];
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
        if ((k == HI_CALL || k == HI_CALLP) && ra_ncalls < RA_MAX_CALLS) {
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
    int inst;
    int k;
    int j;
    int cp;
    int s2_inst;

    /* Collect clobber positions */
    ra_ncx_clobbers = 0;
    ra_ndx_clobbers = 0;
    i = 0;
    while (i < ra_norder) {
        inst = ra_order[i];
        k = h_kind[inst];
        /* DIV/REM clobbers both RCX (divisor) and RDX (remainder/sign-ext) */
        if (k == HI_DIV || k == HI_REM) {
            if (ra_ncx_clobbers < RA_MAX_CLOBBERS) {
                ra_cx_clobber_positions[ra_ncx_clobbers] = ra_pos[inst];
                ra_ncx_clobbers = ra_ncx_clobbers + 1;
            }
            if (ra_ndx_clobbers < RA_MAX_CLOBBERS) {
                ra_dx_clobber_positions[ra_ndx_clobbers] = ra_pos[inst];
                ra_ndx_clobbers = ra_ndx_clobbers + 1;
            }
        }
        /* Variable shifts clobber RCX (shift count in CL) */
        if (k == HI_SLL || k == HI_SRA || k == HI_SRL) {
            s2_inst = h_src2[inst];
            /* Only variable shifts (not immediate) clobber CL */
            if (s2_inst < 0 || h_kind[s2_inst] != HI_ICONST) {
                if (ra_ncx_clobbers < RA_MAX_CLOBBERS) {
                    ra_cx_clobber_positions[ra_ncx_clobbers] = ra_pos[inst];
                    ra_ncx_clobbers = ra_ncx_clobbers + 1;
                }
            }
        }
        i = i + 1;
    }

    /* For each value, check if any CX clobber falls within its live interval */
    i = 0;
    while (i < h_ninst) {
        ra_crosses_cx_clobber[i] = 0;
        ra_crosses_dx_clobber[i] = 0;
        if (ra_pos[i] >= 0 && ra_iend[i] > ra_pos[i]) {
            j = 0;
            while (j < ra_ncx_clobbers) {
                cp = ra_cx_clobber_positions[j];
                if (cp > ra_pos[i] && cp < ra_iend[i]) {
                    ra_crosses_cx_clobber[i] = 1;
                    break;
                }
                j = j + 1;
            }
            j = 0;
            while (j < ra_ndx_clobbers) {
                cp = ra_dx_clobber_positions[j];
                if (cp > ra_pos[i] && cp < ra_iend[i]) {
                    ra_crosses_dx_clobber[i] = 1;
                    break;
                }
                j = j + 1;
            }
        }
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

static int hx_is_cmp(int k) {
    return (k >= HI_SEQ && k <= HI_SGEU);
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
 * Main entry point
 * ================================================================= */

static void hir_regalloc(void) {
    ra_init_x64_regs();
    ra_compute_pos();
    ra_compute_ends();
    ra_extend_fused_cmp();
    ra_mark_call_crossing();
    ra_mark_clobbers();
    gc_alloc();
    ra_assign_spills();
}

#endif /* HIR_REGALLOC_X64_H */
