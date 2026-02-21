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

/* Folding */
static int bg_fold[HIR_MAX_INST];  /* 1 = folded into parent */
static int bg_uses[HIR_MAX_INST];  /* use count per value */
static int bg_foff[HIR_MAX_INST];  /* precomputed faddr offset */

/* Stats */
static int bg_stat_folded;
static int bg_stat_cand;
static int bg_stat_rej_uses;
static int bg_stat_rej_remat;
static int bg_stat_rej_blk;
static int bg_stat_rej_cost;
static int bg_stat_sel_total;
static int bg_stat_sel_chain;
static int bg_stat_sel_imm;
static int bg_stat_sel_faddr;
static int bg_stat_sel_saddr;
static int bg_stat_sel_op[BG_OP_SZ];
static int bg_stat_sel_pat[BG_MAX_PAT];
static int bg_stat_iconst_total;
static int bg_stat_iconst_imm_only;
static int bg_stat_iconst_nonimm;
static int bg_stat_iconst_unused;
static int bg_iconst_seen_use[HIR_MAX_INST];
static int bg_iconst_seen_nonimm[HIR_MAX_INST];
static int bg_stat_iconst_use_op[BG_OP_SZ];
static int bg_stat_iconst_nonimm_use_op[BG_OP_SZ];

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
    bg_add_pat(BG_REG, HI_ADD,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_ADD,  BG_IMM, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SUB,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_AND,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_AND,  BG_IMM, BG_REG, 1);
    bg_add_pat(BG_REG, HI_OR,   BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_OR,   BG_IMM, BG_REG, 1);
    bg_add_pat(BG_REG, HI_XOR,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_XOR,  BG_IMM, BG_REG, 1);
    bg_add_pat(BG_REG, HI_SLL,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_SRL,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_SRA,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_SLT,  BG_REG, BG_IMM, 1);
    bg_add_pat(BG_REG, HI_SLTU, BG_REG, BG_IMM, 1);
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
    bg_add_pat(BG_IMM, HI_COPY, BG_IMM, -1, 0);
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

static int bg_use_is_imm_capable(int user, int pos) {
    int k;
    k = h_kind[user];

    if (k == HI_ADD) return 1;                  /* add r,r,imm (both sides) */
    if (k == HI_SUB && pos == 2) return 1;      /* sub r,imm via addi -imm */
    if ((k == HI_AND || k == HI_OR || k == HI_XOR)) return 1;
    if ((k == HI_SLL || k == HI_SRL || k == HI_SRA) && pos == 2) return 1;
    if ((k == HI_SLT || k == HI_SLTU) && pos == 2) return 1;
    return 0;
}

static void bg_profile_iconst_consumers(void) {
    int i;
    int j;
    int k;
    int base;
    int cnt;
    int v;

    i = 0;
    while (i < h_ninst) {
        bg_iconst_seen_use[i] = 0;
        bg_iconst_seen_nonimm[i] = 0;
        i = i + 1;
    }
    i = 0;
    while (i <= BG_MAX_OP) {
        bg_stat_iconst_use_op[i] = 0;
        bg_stat_iconst_nonimm_use_op[i] = 0;
        i = i + 1;
    }

    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k != HI_NOP) {
            v = h_src1[i];
            if (v >= 0 && h_kind[v] == HI_ICONST) {
                bg_iconst_seen_use[v] = 1;
                if (k >= 0 && k <= BG_MAX_OP) bg_stat_iconst_use_op[k] = bg_stat_iconst_use_op[k] + 1;
                if (!bg_use_is_imm_capable(i, 1)) {
                    bg_iconst_seen_nonimm[v] = 1;
                    if (k >= 0 && k <= BG_MAX_OP) {
                        bg_stat_iconst_nonimm_use_op[k] = bg_stat_iconst_nonimm_use_op[k] + 1;
                    }
                }
            }
            v = h_src2[i];
            if (v >= 0 && h_kind[v] == HI_ICONST) {
                bg_iconst_seen_use[v] = 1;
                if (k >= 0 && k <= BG_MAX_OP) bg_stat_iconst_use_op[k] = bg_stat_iconst_use_op[k] + 1;
                if (!bg_use_is_imm_capable(i, 2)) {
                    bg_iconst_seen_nonimm[v] = 1;
                    if (k >= 0 && k <= BG_MAX_OP) {
                        bg_stat_iconst_nonimm_use_op[k] = bg_stat_iconst_nonimm_use_op[k] + 1;
                    }
                }
            }

            if (k == HI_CALL || k == HI_CALLP) {
                base = h_cbase[i];
                cnt = h_val[i];
                j = 0;
                while (j < cnt) {
                    v = h_carg[base + j];
                    if (v >= 0 && h_kind[v] == HI_ICONST) {
                        bg_iconst_seen_use[v] = 1;
                        bg_iconst_seen_nonimm[v] = 1;
                        bg_stat_iconst_use_op[k] = bg_stat_iconst_use_op[k] + 1;
                        bg_stat_iconst_nonimm_use_op[k] = bg_stat_iconst_nonimm_use_op[k] + 1;
                    }
                    j = j + 1;
                }
            }

            if (k == HI_PHI) {
                base = h_pbase[i];
                cnt = h_pcnt[i];
                j = 0;
                while (j < cnt) {
                    v = h_pval[base + j];
                    if (v >= 0 && h_kind[v] == HI_ICONST) {
                        bg_iconst_seen_use[v] = 1;
                        bg_iconst_seen_nonimm[v] = 1;
                        bg_stat_iconst_use_op[k] = bg_stat_iconst_use_op[k] + 1;
                        bg_stat_iconst_nonimm_use_op[k] = bg_stat_iconst_nonimm_use_op[k] + 1;
                    }
                    j = j + 1;
                }
            }
        }
        i = i + 1;
    }

    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_ICONST) {
            bg_stat_iconst_total = bg_stat_iconst_total + 1;
            if (!bg_iconst_seen_use[i]) {
                bg_stat_iconst_unused = bg_stat_iconst_unused + 1;
            } else if (!bg_iconst_seen_nonimm[i]) {
                bg_stat_iconst_imm_only = bg_stat_iconst_imm_only + 1;
            } else {
                bg_stat_iconst_nonimm = bg_stat_iconst_nonimm + 1;
            }
        }
        i = i + 1;
    }
}

static char *bg_nt_name(int nt) {
    if (nt == BG_REG) return "reg";
    if (nt == BG_IMM) return "imm";
    if (nt == BG_FADDR) return "faddr";
    if (nt == BG_SADDR) return "saddr";
    if (nt == BG_STMT) return "stmt";
    return "?";
}

static char *bg_op_name(int op) {
    if (op == HI_NOP) return "nop";
    if (op == HI_ICONST) return "iconst";
    if (op == HI_PARAM) return "param";
    if (op == HI_ADD) return "add";
    if (op == HI_SUB) return "sub";
    if (op == HI_MUL) return "mul";
    if (op == HI_DIV) return "div";
    if (op == HI_REM) return "rem";
    if (op == HI_AND) return "and";
    if (op == HI_OR) return "or";
    if (op == HI_XOR) return "xor";
    if (op == HI_SLL) return "sll";
    if (op == HI_SRA) return "sra";
    if (op == HI_SRL) return "srl";
    if (op == HI_SEQ) return "seq";
    if (op == HI_SNE) return "sne";
    if (op == HI_SLT) return "slt";
    if (op == HI_SGT) return "sgt";
    if (op == HI_SLE) return "sle";
    if (op == HI_SGE) return "sge";
    if (op == HI_SLTU) return "sltu";
    if (op == HI_SGTU) return "sgtu";
    if (op == HI_SLEU) return "sleu";
    if (op == HI_SGEU) return "sgeu";
    if (op == HI_NEG) return "neg";
    if (op == HI_NOT) return "not";
    if (op == HI_BNOT) return "bnot";
    if (op == HI_LOAD) return "load";
    if (op == HI_STORE) return "store";
    if (op == HI_ALLOCA) return "alloca";
    if (op == HI_GADDR) return "gaddr";
    if (op == HI_SADDR) return "saddr";
    if (op == HI_FADDR) return "faddr";
    if (op == HI_BR) return "br";
    if (op == HI_BRC) return "brc";
    if (op == HI_RET) return "ret";
    if (op == HI_CALL) return "call";
    if (op == HI_CALLP) return "callp";
    if (op == HI_PHI) return "phi";
    if (op == HI_COPY) return "copy";
    if (op == HI_ADDI) return "addi";
    return "?";
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
        if (h_src2[i] >= 0) bg_uses[h_src2[i]] = bg_uses[h_src2[i]] + 1;

        /* CALL/CALLP args */
        if (k == HI_CALL || k == HI_CALLP) {
            base = h_cbase[i];
            cnt = h_val[i];
            j = 0;
            while (j < cnt) {
                if (h_carg[base + j] >= 0) {
                    bg_uses[h_carg[base + j]] = bg_uses[h_carg[base + j]] + 1;
                }
                j = j + 1;
            }
        }

        /* PHI args */
        if (k == HI_PHI) {
            base = h_pbase[i];
            cnt = h_pcnt[i];
            j = 0;
            while (j < cnt) {
                if (h_pval[base + j] >= 0) {
                    bg_uses[h_pval[base + j]] = bg_uses[h_pval[base + j]] + 1;
                }
                j = j + 1;
            }
        }

        i = i + 1;
    }
}

/* --- Precompute frame-relative offsets --- */

static void bg_compute_foff(void) {
    int i;
    int k;
    int s1;
    int sk;

    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_ALLOCA) {
            bg_foff[i] = h_val[i];
        } else if (k == HI_ADDI) {
            s1 = h_src1[i];
            sk = -1;
            if (s1 >= 0) sk = h_kind[s1];
            if (sk == HI_ALLOCA || sk == HI_ADDI) {
                bg_foff[i] = bg_foff[s1] + h_val[i];
            } else {
                bg_foff[i] = 0;
            }
        } else {
            bg_foff[i] = 0;
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
                    /* Chain encodes as <= -2; -1 means no rule. */
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

    /* Phase 1: Init and select all patterns */
    i = 0;
    while (i < h_ninst) {
        bg_sel[i] = -1;
        bg_fold[i] = 0;
        i = i + 1;
    }

    /* Note: stats accumulate across all functions (not reset here) */

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
        if (bg_sel[i] >= 0) {
            pat = bg_sel[i];
            k = bg_pop[pat];
            bg_stat_sel_total = bg_stat_sel_total + 1;
            if (k >= 0 && k <= BG_MAX_OP) {
                bg_stat_sel_op[k] = bg_stat_sel_op[k] + 1;
            }
            if (pat >= 0 && pat < BG_MAX_PAT) {
                bg_stat_sel_pat[pat] = bg_stat_sel_pat[pat] + 1;
            }
            lnt = bg_plnt[pat];
            rnt = bg_prnt[pat];
            if (lnt == BG_IMM || rnt == BG_IMM) bg_stat_sel_imm = bg_stat_sel_imm + 1;
            if (lnt == BG_FADDR || rnt == BG_FADDR) bg_stat_sel_faddr = bg_stat_sel_faddr + 1;
            if (lnt == BG_SADDR || rnt == BG_SADDR) bg_stat_sel_saddr = bg_stat_sel_saddr + 1;
        } else if (bg_sel[i] <= -2) {
            bg_stat_sel_chain = bg_stat_sel_chain + 1;
        }
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
            bg_stat_cand = bg_stat_cand + 1;
            if (hi_is_remat(h_kind[s1])) {
                bg_stat_rej_remat = bg_stat_rej_remat + 1;
            } else if (bg_uses[s1] != 1) {
                bg_stat_rej_uses = bg_stat_rej_uses + 1;
            } else if (h_blk[s1] != h_blk[i]) {
                bg_stat_rej_blk = bg_stat_rej_blk + 1;
            } else if (bg_cost[s1 * BG_NNT + lnt] >= BG_INF) {
                bg_stat_rej_cost = bg_stat_rej_cost + 1;
            } else if (h_kind[s1] != HI_NOP) {
                bg_fold[s1] = 1;
                bg_sel[s1] = -1;
                h_kind[s1] = HI_NOP;
                bg_stat_folded = bg_stat_folded + 1;
            }
        }

        rnt = bg_prnt[pat];
        s2 = h_src2[i];

        /* Fold right child (rare, but check for completeness) */
        if (rnt >= 0 && rnt != BG_REG && s2 >= 0 && !bg_fold[s2]) {
            if (bg_uses[s2] == 1
                && !hi_is_remat(h_kind[s2])
                && h_kind[s2] != HI_NOP
                && h_blk[s2] == h_blk[i]
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

    /* Always precompute foff (needed by codegen even in fallback) */
    bg_compute_foff();

    /* Skip BURG for functions with too many instructions */
    if (h_ninst > BG_MAX_INST) {
        /* Set bg_sel to -1: codegen falls back to h_kind dispatch */
        i = 0;
        while (i < h_ninst) {
            bg_sel[i] = -1;
            bg_fold[i] = 0;
            i = i + 1;
        }
        return;
    }

    bg_label();
    bg_count_uses();
    bg_profile_iconst_consumers();
    /* bg_compute_foff already called above (before any NOP changes) */
    bg_select();  /* Phase 2 of select may NOP folded instructions */
}
