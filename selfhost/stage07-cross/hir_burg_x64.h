/* hir_burg_x64.h -- BURG-style instruction selection for x86-64
 *
 * Bottom-up rewrite grammar: table-driven pattern matcher.
 * Labels each HIR instruction with minimum-cost tiling, then
 * selects patterns top-down. Codegen dispatches on bg_sel[].
 *
 * Forked from stage07/hir_burg.h (SLOW-32 target).
 *
 * Nonterminals:
 *   BG_REG   = value in a register
 *   BG_IMM   = compile-time 32-bit constant (not yet materialized)
 *   BG_MEM   = frame-relative address [RBP + disp32]
 *   BG_SADDR = symbol address (needs relocation)
 *   BG_STMT  = side-effect (no value produced)
 */

#ifndef HIR_BURG_X64_H
#define HIR_BURG_X64_H

#define BG_REG    0
#define BG_IMM    1
#define BG_MEM    2   /* frame-relative [RBP+disp], replaces SLOW-32's BG_FADDR */
#define BG_SADDR  3
#define BG_STMT   4
#define BG_NNT    5
#define BG_INF    999999

/* --- Pattern table --- */
#define BG_MAX_PAT 128

static int bg_pnt[BG_MAX_PAT];     /* result nonterminal */
static int bg_pop[BG_MAX_PAT];     /* HIR operator */
static int bg_plnt[BG_MAX_PAT];    /* left child required NT (-1 = leaf) */
static int bg_prnt[BG_MAX_PAT];    /* right child required NT (-1 = none) */
static int bg_pcost[BG_MAX_PAT];   /* base cost */
static int bg_npat;

/* Per-operator index */
#define BG_MAX_OP 55
#define BG_OP_SZ 56
static int bg_ofirst[BG_OP_SZ];
static int bg_ocount[BG_OP_SZ];
static int bg_sorted[BG_MAX_PAT];

/* Chain rules */
#define BG_MAX_CHAIN 8
static int bg_cfrom[BG_MAX_CHAIN];
static int bg_cto[BG_MAX_CHAIN];
static int bg_ccost[BG_MAX_CHAIN];
static int bg_nchain;

/* Per-instruction labeling */
#define BG_MAX_INST 8192
#define BG_COST_SZ 40960
static int bg_cost[BG_COST_SZ];
static int bg_rule[BG_COST_SZ];

/* Selection output */
static int bg_sel[HIR_MAX_INST];
static int bg_fold[HIR_MAX_INST];
static int bg_uses[HIR_MAX_INST];
static int bg_foff[HIR_MAX_INST];  /* precomputed x64 frame offset (via cg_x64_offset) */
static int bg_ssym[HIR_MAX_INST];  /* base symbol inst for SADDR chain */
static int bg_soff[HIR_MAX_INST];  /* accumulated offset for SADDR chain */

/* Stats */
static int bg_stat_folded;
static int bg_stat_burg_skipped;

/* Init flag */
static int bg_inited;

/* --- Registration --- */

static int bg_add_pat(int nt, int op, int lnt, int rnt, int cost) {
    int idx;
    idx = bg_npat;
    if (idx >= BG_MAX_PAT) return -1;
    bg_pnt[idx] = nt;
    bg_pop[idx] = op;
    bg_plnt[idx] = lnt;
    bg_prnt[idx] = rnt;
    bg_pcost[idx] = cost;
    bg_npat = bg_npat + 1;
    return idx;
}

static void bg_add_chain(int from, int to, int cost) {
    int idx;
    idx = bg_nchain;
    if (idx >= BG_MAX_CHAIN) return;
    bg_cfrom[idx] = from;
    bg_cto[idx] = to;
    bg_ccost[idx] = cost;
    bg_nchain = bg_nchain + 1;
}

/* --- x86-64 pattern initialization --- */

static void bg_init(void) {
    bg_npat = 0;
    bg_nchain = 0;

    /* Leaves */
    bg_add_pat(BG_IMM,   HI_ICONST, -1, -1, 0);
    bg_add_pat(BG_REG,   HI_ICONST, -1, -1, 1);
    bg_add_pat(BG_MEM,   HI_ALLOCA, -1, -1, 0);  /* [RBP + offset] */
    bg_add_pat(BG_REG,   HI_ALLOCA, -1, -1, 1);  /* lea r, [RBP + offset] */
    bg_add_pat(BG_SADDR, HI_GADDR,  -1, -1, 0);
    bg_add_pat(BG_REG,   HI_GADDR,  -1, -1, 2);
    bg_add_pat(BG_SADDR, HI_SADDR,  -1, -1, 0);
    bg_add_pat(BG_REG,   HI_SADDR,  -1, -1, 2);
    bg_add_pat(BG_SADDR, HI_FADDR,  -1, -1, 0);
    bg_add_pat(BG_REG,   HI_FADDR,  -1, -1, 2);
    bg_add_pat(BG_REG,   HI_PARAM,  -1, -1, 0);  /* 0: already in register from regalloc */
    bg_add_pat(BG_REG,   HI_GETFP,  -1, -1, 1);

    /* Addressing: LOAD */
    bg_add_pat(BG_REG,  HI_LOAD, BG_MEM,   -1, 1);  /* mov r, [rbp+disp] */
    bg_add_pat(BG_REG,  HI_LOAD, BG_SADDR, -1, 2);  /* movabs+load */
    bg_add_pat(BG_REG,  HI_LOAD, BG_REG,   -1, 1);  /* mov r, [r] */

    /* Addressing: STORE */
    bg_add_pat(BG_STMT, HI_STORE, BG_MEM,   BG_REG, 1);  /* mov [rbp+disp], r */
    bg_add_pat(BG_STMT, HI_STORE, BG_SADDR, BG_REG, 2);  /* movabs+store */
    bg_add_pat(BG_STMT, HI_STORE, BG_REG,   BG_REG, 1);  /* mov [r], r */

    /* ADDI: offset accumulation and materialization */
    bg_add_pat(BG_IMM,   HI_ADDI, BG_IMM,   -1, 0);  /* constant fold */
    bg_add_pat(BG_MEM,   HI_ADDI, BG_MEM,   -1, 0);  /* accumulate displacement */
    bg_add_pat(BG_SADDR, HI_ADDI, BG_SADDR, -1, 0);  /* accumulate symbol offset */
    bg_add_pat(BG_REG,   HI_ADDI, BG_MEM,   -1, 1);  /* lea r, [rbp+disp] */
    bg_add_pat(BG_REG,   HI_ADDI, BG_REG,   -1, 1);  /* add r, imm */

    /* Binary with immediate operand (x64 supports 32-bit imm for most ALU) */
    bg_add_pat(BG_REG, HI_ADD,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_ADD,  BG_IMM, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SUB,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_MUL,  BG_REG, BG_IMM, 1);  /* imul r, r, imm */
    bg_add_pat(BG_REG, HI_MUL,  BG_IMM, BG_REG, 1);  /* imul r, r, imm (commutative) */
    bg_add_pat(BG_REG, HI_AND,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_AND,  BG_IMM, BG_REG, 1);
    bg_add_pat(BG_REG, HI_OR,   BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_OR,   BG_IMM, BG_REG, 1);
    bg_add_pat(BG_REG, HI_XOR,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_XOR,  BG_IMM, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SLL,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_SRL,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_SRA,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_SEQ,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_SEQ,  BG_IMM, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SNE,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_SNE,  BG_IMM, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SLT,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_SLTU, BG_REG, BG_IMM, 1);

    /* Binary register-register */
    bg_add_pat(BG_REG, HI_ADD,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SUB,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_MUL,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_DIV,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_REM,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_AND,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_OR,   BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_XOR,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SLL,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SRA,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SRL,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SEQ,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SNE,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SLT,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SGT,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SLE,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SGE,  BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SLTU, BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SGTU, BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SLEU, BG_REG, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SGEU, BG_REG, BG_REG, 1);

    /* Unary */
    bg_add_pat(BG_REG, HI_NEG,  BG_REG, -1, 1);
    bg_add_pat(BG_REG, HI_NOT,  BG_REG, -1, 1);
    bg_add_pat(BG_REG, HI_BNOT, BG_REG, -1, 1);  /* x64 has NOT r (1 instr) */
    bg_add_pat(BG_IMM, HI_COPY, BG_IMM, -1, 0);
    bg_add_pat(BG_REG, HI_COPY, BG_REG, -1, 0);

    /* Control flow */
    bg_add_pat(BG_STMT, HI_BR,  -1, -1, 1);
    bg_add_pat(BG_STMT, HI_BRC, BG_REG, -1, 1);
    bg_add_pat(BG_STMT, HI_RET, -1, -1, 1);

    /* Calls */
    bg_add_pat(BG_REG, HI_CALL,  -1, -1, 3);
    bg_add_pat(BG_REG, HI_CALLP, BG_REG, -1, 3);

    /* PHI */
    bg_add_pat(BG_REG, HI_PHI, -1, -1, 0);

    /* Chain rules */
    bg_add_chain(BG_IMM,   BG_REG, 1);   /* mov r, imm */
    bg_add_chain(BG_MEM,   BG_REG, 1);   /* lea r, [rbp+disp] */
    bg_add_chain(BG_SADDR, BG_REG, 2);   /* movabs r, sym */
}

/* --- Build per-operator index --- */

static void bg_build_index(void) {
    int op;
    int i;
    int j;

    op = 0;
    while (op <= BG_MAX_OP) {
        bg_ocount[op] = 0;
        op = op + 1;
    }

    i = 0;
    while (i < bg_npat) {
        op = bg_pop[i];
        if (op >= 0 && op <= BG_MAX_OP) {
            bg_ocount[op] = bg_ocount[op] + 1;
        }
        i = i + 1;
    }

    bg_ofirst[0] = 0;
    op = 1;
    while (op <= BG_MAX_OP) {
        bg_ofirst[op] = bg_ofirst[op - 1] + bg_ocount[op - 1];
        op = op + 1;
    }

    op = 0;
    while (op <= BG_MAX_OP) {
        bg_ocount[op] = 0;
        op = op + 1;
    }

    i = 0;
    while (i < bg_npat) {
        op = bg_pop[i];
        if (op >= 0 && op <= BG_MAX_OP) {
            j = bg_ofirst[op] + bg_ocount[op];
            bg_sorted[j] = i;
            bg_ocount[op] = bg_ocount[op] + 1;
        }
        i = i + 1;
    }
}

/* --- Precompute frame-relative and symbol-relative offsets ---
 *
 * bg_foff[] stores the x64-scaled frame offset for ALLOCA/ADDI chains.
 * Uses cg_x64_offset() for the SLOW-32→x64 offset scaling.
 * (cg_x64_offset is defined in codegen_x64.h, included after this file.) */
static int cg_x64_offset(int slow32_offset);

static int bg_is_sym(int k) {
    return k == HI_GADDR || k == HI_SADDR || k == HI_FADDR;
}

static void bg_compute_foff(void) {
    int i;
    int k;
    int s1;
    int sk;

    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        /* Frame offsets (x64-scaled) */
        if (k == HI_ALLOCA) {
            bg_foff[i] = cg_x64_offset(h_val[i]);
        } else if (k == HI_ADDI) {
            s1 = h_src1[i];
            sk = -1;
            if (s1 >= 0) sk = h_kind[s1];
            if (sk == HI_ALLOCA || sk == HI_ADDI) {
                /* Accumulate: parent foff + ADDI offset (scaled for pointers) */
                bg_foff[i] = bg_foff[s1] + h_val[i];
            } else {
                bg_foff[i] = 0;
            }
        } else {
            bg_foff[i] = 0;
        }
        /* Symbol offsets for SADDR chains */
        if (bg_is_sym(k)) {
            bg_ssym[i] = i;
            bg_soff[i] = 0;
        } else if (k == HI_ADDI) {
            s1 = h_src1[i];
            if (s1 >= 0 && bg_ssym[s1] >= 0) {
                bg_ssym[i] = bg_ssym[s1];
                bg_soff[i] = bg_soff[s1] + h_val[i];
            } else {
                bg_ssym[i] = -1;
                bg_soff[i] = 0;
            }
        } else {
            bg_ssym[i] = -1;
            bg_soff[i] = 0;
        }
        i = i + 1;
    }
}

/* --- Use counting --- */

static void bg_count_uses(void) {
    int i;
    int j;
    int k;
    int base;
    int cnt;

    i = 0;
    while (i < h_ninst) {
        bg_uses[i] = 0;
        i = i + 1;
    }

    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP) { i = i + 1; continue; }

        if (h_src1[i] >= 0) bg_uses[h_src1[i]] = bg_uses[h_src1[i]] + 1;
        if (h_src2[i] >= 0 && ho_src2_is_ref(k)) bg_uses[h_src2[i]] = bg_uses[h_src2[i]] + 1;

        if (k == HI_CALL || k == HI_CALLP) {
            base = h_cbase[i];
            cnt = h_val[i];
            j = 0;
            while (j < cnt) {
                if (h_carg[base + j] >= 0)
                    bg_uses[h_carg[base + j]] = bg_uses[h_carg[base + j]] + 1;
                j = j + 1;
            }
        }

        if (k == HI_PHI) {
            base = h_pbase[i];
            cnt = h_pcnt[i];
            j = 0;
            while (j < cnt) {
                if (h_pval[base + j] >= 0)
                    bg_uses[h_pval[base + j]] = bg_uses[h_pval[base + j]] + 1;
                j = j + 1;
            }
        }

        i = i + 1;
    }
}

/* --- Bottom-up labeling --- */

static void bg_label(void) {
    int i;
    int k;
    int pi;
    int pat;
    int lnt;
    int rnt;
    int cost;
    int ci;
    int s1;
    int s2;
    int base_idx;
    int dst;

    /* Initialize all costs to INF */
    i = 0;
    while (i < h_ninst * BG_NNT) {
        bg_cost[i] = BG_INF;
        bg_rule[i] = -1;
        i = i + 1;
    }

    /* Label each instruction */
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP) { i = i + 1; continue; }
        if (k < 0 || k > BG_MAX_OP) { i = i + 1; continue; }

        s1 = h_src1[i];
        s2 = h_src2[i];

        /* Try all patterns for this operator */
        pi = 0;
        while (pi < bg_ocount[k]) {
            pat = bg_sorted[bg_ofirst[k] + pi];
            lnt = bg_plnt[pat];
            rnt = bg_prnt[pat];
            cost = bg_pcost[pat];

            /* x64 guard: MEM patterns require valid frame offset chain.
             * On x64, 32-bit displacement always fits, so no range check
             * needed for ALLOCA→MEM.  For ADDI(MEM)→MEM, the accumulated
             * offset must be in 32-bit range (always true in practice). */

            /* Check left child */
            if (lnt >= 0) {
                if (s1 < 0 || bg_cost[s1 * BG_NNT + lnt] >= BG_INF) {
                    pi = pi + 1;
                    continue;
                }
                cost = cost + bg_cost[s1 * BG_NNT + lnt];
            }

            /* Check right child */
            if (rnt >= 0) {
                if (s2 < 0 || bg_cost[s2 * BG_NNT + rnt] >= BG_INF) {
                    pi = pi + 1;
                    continue;
                }
                cost = cost + bg_cost[s2 * BG_NNT + rnt];
            }

            /* Update if better */
            dst = i * BG_NNT + bg_pnt[pat];
            if (cost < bg_cost[dst]) {
                bg_cost[dst] = cost;
                bg_rule[dst] = pat;
            }

            pi = pi + 1;
        }

        /* Apply chain rules */
        ci = 0;
        while (ci < bg_nchain) {
            base_idx = i * BG_NNT + bg_cfrom[ci];
            if (bg_cost[base_idx] < BG_INF) {
                cost = bg_cost[base_idx] + bg_ccost[ci];
                dst = i * BG_NNT + bg_cto[ci];
                if (cost < bg_cost[dst]) {
                    bg_cost[dst] = cost;
                    bg_rule[dst] = -(ci + 2);
                }
            }
            ci = ci + 1;
        }

        i = i + 1;
    }
}

/* --- Top-down selection --- */

static void bg_select(void) {
    int i;
    int k;
    int nt;
    int pat;
    int lnt;
    int rnt;
    int s1;
    int s2;

    /* Phase 1: Select patterns */
    i = 0;
    while (i < h_ninst) {
        bg_sel[i] = -1;
        bg_fold[i] = 0;
        i = i + 1;
    }

    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP) { i = i + 1; continue; }

        if (!hi_has_value(k)) {
            nt = BG_STMT;
        } else {
            nt = BG_REG;
        }

        bg_sel[i] = bg_rule[i * BG_NNT + nt];
        i = i + 1;
    }

    /* Phase 2: Fold single-use children into parents */
    i = 0;
    while (i < h_ninst) {
        pat = bg_sel[i];
        if (pat < 0) { i = i + 1; continue; }

        lnt = bg_plnt[pat];
        s1 = h_src1[i];

        /* Fold left child if: non-REG NT, single use, same block, viable */
        if (lnt >= 0 && lnt != BG_REG && s1 >= 0 && !bg_fold[s1]) {
            if (!hi_is_remat(h_kind[s1])
                && bg_uses[s1] == 1
                && h_blk[s1] == h_blk[i]
                && h_kind[s1] != HI_NOP
                && bg_cost[s1 * BG_NNT + lnt] < BG_INF) {
                bg_fold[s1] = 1;
                bg_sel[s1] = -1;
                h_kind[s1] = HI_NOP;
                bg_stat_folded = bg_stat_folded + 1;
            }
        }

        rnt = bg_prnt[pat];
        s2 = h_src2[i];

        /* Fold right child (rare) */
        if (rnt >= 0 && rnt != BG_REG && s2 >= 0 && !bg_fold[s2]) {
            if (!hi_is_remat(h_kind[s2])
                && bg_uses[s2] == 1
                && h_blk[s2] == h_blk[i]
                && h_kind[s2] != HI_NOP
                && bg_cost[s2 * BG_NNT + rnt] < BG_INF) {
                bg_fold[s2] = 1;
                bg_sel[s2] = -1;
                h_kind[s2] = HI_NOP;
                bg_stat_folded = bg_stat_folded + 1;
            }
        }

        i = i + 1;
    }
}

/* --- Entry point --- */

static void hir_burg(void) {
    int i;

    if (!bg_inited) {
        bg_init();
        bg_build_index();
        bg_inited = 1;
    }

    /* Always precompute foff (before any NOP changes from folding) */
    bg_compute_foff();

    /* Skip BURG for very large functions */
    if (h_ninst > BG_MAX_INST) {
        i = 0;
        while (i < h_ninst) {
            bg_sel[i] = -1;
            bg_fold[i] = 0;
            i = i + 1;
        }
        bg_stat_burg_skipped = bg_stat_burg_skipped + 1;
        return;
    }

    bg_label();
    bg_count_uses();
    bg_select();
}

#endif /* HIR_BURG_X64_H */
