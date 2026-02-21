/* hir_burg.h -- BURG-style instruction selection for s12cc
 *
 * Bottom-up rewrite grammar: table-driven pattern matcher.
 * Labels each HIR instruction with minimum-cost tiling, then
 * selects patterns top-down. Codegen dispatches on bg_sel[].
 *
 * Runs BEFORE regalloc (between hir_licm and hir_regalloc).
 *
 * Nonterminals:
 *   BG_REG   = value in a register
 *   BG_IMM   = compile-time constant (not yet materialized)
 *   BG_FADDR = frame-relative address (r30 + small offset)
 *   BG_SADDR = symbol address (needs lui)
 *   BG_STMT  = side-effect (no value produced)
 */

#define BG_REG    0
#define BG_IMM    1
#define BG_FADDR  2
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

/* Per-operator index (41 ops + 1 sentinel = 42) */
#define BG_MAX_OP 41
#define BG_OP_SZ 42
static int bg_ofirst[BG_OP_SZ];
static int bg_ocount[BG_OP_SZ];
static int bg_sorted[BG_MAX_PAT];

/* Chain rules */
#define BG_MAX_CHAIN 8
static int bg_cfrom[BG_MAX_CHAIN];
static int bg_cto[BG_MAX_CHAIN];
static int bg_ccost[BG_MAX_CHAIN];
static int bg_nchain;

/* Per-instruction labeling: 8192 * 5 = 40960 entries.
 * Covers functions up to 8192 HIR instructions.
 * Functions exceeding this skip BURG (codegen falls back). */
#define BG_MAX_INST 8192
#define BG_COST_SZ 40960
static int bg_cost[BG_COST_SZ];
static int bg_rule[BG_COST_SZ];

/* Selection output */
static int bg_sel[HIR_MAX_INST];

/* Stats */
static int bg_stat_folded;

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

/* --- Pattern initialization --- */

static void bg_init(void) {
    bg_npat = 0;
    bg_nchain = 0;

    /* Leaves */
    bg_add_pat(BG_IMM,   HI_ICONST, -1, -1, 0);
    bg_add_pat(BG_REG,   HI_ICONST, -1, -1, 1);
    bg_add_pat(BG_FADDR, HI_ALLOCA, -1, -1, 0);
    bg_add_pat(BG_REG,   HI_ALLOCA, -1, -1, 1);
    bg_add_pat(BG_SADDR, HI_GADDR,  -1, -1, 0);
    bg_add_pat(BG_REG,   HI_GADDR,  -1, -1, 2);
    bg_add_pat(BG_SADDR, HI_SADDR,  -1, -1, 0);
    bg_add_pat(BG_REG,   HI_SADDR,  -1, -1, 2);
    bg_add_pat(BG_SADDR, HI_FADDR,  -1, -1, 0);
    bg_add_pat(BG_REG,   HI_FADDR,  -1, -1, 2);
    bg_add_pat(BG_REG,   HI_PARAM,  -1, -1, 1);

    /* Addressing: LOAD */
    bg_add_pat(BG_REG,  HI_LOAD, BG_FADDR, -1, 1);
    bg_add_pat(BG_REG,  HI_LOAD, BG_SADDR, -1, 2);
    bg_add_pat(BG_REG,  HI_LOAD, BG_REG,   -1, 1);

    /* Addressing: STORE */
    bg_add_pat(BG_STMT, HI_STORE, BG_FADDR, BG_REG, 1);
    bg_add_pat(BG_STMT, HI_STORE, BG_SADDR, BG_REG, 2);
    bg_add_pat(BG_STMT, HI_STORE, BG_REG,   BG_REG, 1);

    /* ADDI */
    bg_add_pat(BG_FADDR, HI_ADDI, BG_FADDR, -1, 0);
    bg_add_pat(BG_REG,   HI_ADDI, BG_FADDR, -1, 1);
    bg_add_pat(BG_REG,   HI_ADDI, BG_REG,   -1, 1);

    /* Binary arithmetic/logic/comparison */
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
    bg_add_pat(BG_REG, HI_BNOT, BG_REG, -1, 2);
    bg_add_pat(BG_REG, HI_COPY, BG_REG, -1, 0);

    /* Control flow */
    bg_add_pat(BG_STMT, HI_BR,  -1, -1, 1);
    bg_add_pat(BG_STMT, HI_BRC, BG_REG, -1, 2);
    bg_add_pat(BG_STMT, HI_RET, -1, -1, 1);

    /* Calls */
    bg_add_pat(BG_REG, HI_CALL,  -1, -1, 3);
    bg_add_pat(BG_REG, HI_CALLP, BG_REG, -1, 3);

    /* PHI */
    bg_add_pat(BG_REG, HI_PHI, -1, -1, 0);

    /* Chain rules */
    bg_add_chain(BG_IMM,   BG_REG, 1);
    bg_add_chain(BG_FADDR, BG_REG, 1);
    bg_add_chain(BG_SADDR, BG_REG, 2);
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

/* --- Helpers --- */

static int bg_faddr_ok(int idx) {
    int off;
    off = h_val[idx];
    return (off >= -2048 && off <= 2047);
}

static int bg_faddr_offset(int idx) {
    int k;
    k = h_kind[idx];
    if (k == HI_ALLOCA) return h_val[idx];
    if (k == HI_ADDI) {
        return h_val[idx] + bg_faddr_offset(h_src1[idx]);
    }
    return 0;
}

static int bg_addi_faddr_ok(int idx) {
    int off;
    off = bg_faddr_offset(idx);
    return (off >= -2048 && off <= 2047);
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

            /* Guard: faddr -> ALLOCA requires small offset */
            if (k == HI_ALLOCA && bg_pnt[pat] == BG_FADDR) {
                if (!bg_faddr_ok(i)) { pi = pi + 1; continue; }
            }

            /* Guard: faddr -> ADDI(faddr) requires combined offset fits */
            if (k == HI_ADDI && bg_pnt[pat] == BG_FADDR) {
                if (!bg_addi_faddr_ok(i)) { pi = pi + 1; continue; }
            }

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
                    bg_rule[dst] = -(ci + 1);
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

    i = 0;
    while (i < h_ninst) {
        bg_sel[i] = -1;
        i = i + 1;
    }

    bg_stat_folded = 0;

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
}

/* --- Entry point --- */

static void hir_burg(void) {
    int i;

    if (!bg_inited) {
        bg_init();
        bg_build_index();
        bg_inited = 1;
    }

    /* Skip BURG for functions with too many instructions */
    if (h_ninst > BG_MAX_INST) {
        /* Set bg_sel to -1: codegen falls back to h_kind dispatch */
        i = 0;
        while (i < h_ninst) {
            bg_sel[i] = -1;
            i = i + 1;
        }
        return;
    }

    bg_label();
    bg_select();
}
