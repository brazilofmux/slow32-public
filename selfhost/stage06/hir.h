/* hir.h -- High-level IR for s12cc
 *
 * Parallel-array instruction representation.
 * Each instruction slot is identified by its index.
 * Instructions that produce values use their index as the value number.
 */

/* --- Instruction kinds --- */
#define HI_NOP       0
#define HI_ICONST    1
#define HI_PARAM     2
#define HI_ADD       3
#define HI_SUB       4
#define HI_MUL       5
#define HI_DIV       6
#define HI_REM       7
#define HI_AND       8
#define HI_OR        9
#define HI_XOR      10
#define HI_SLL      11
#define HI_SRA      12
#define HI_SRL      13
#define HI_SEQ      14
#define HI_SNE      15
#define HI_SLT      16
#define HI_SGT      17
#define HI_SLE      18
#define HI_SGE      19
#define HI_SLTU     20
#define HI_SGTU     21
#define HI_SLEU     22
#define HI_SGEU     23
#define HI_NEG      24
#define HI_NOT      25
#define HI_BNOT     26
#define HI_LOAD     27
#define HI_STORE    28
#define HI_ALLOCA   29
#define HI_GADDR    30
#define HI_SADDR    31
#define HI_FADDR    32
#define HI_BR       33
#define HI_BRC      34
#define HI_RET      35
#define HI_CALL     36
#define HI_CALLP    37
#define HI_PHI      38
#define HI_COPY     39
#define HI_ADDI     40
#define HI_GETFP    41

/* --- Limits --- */
#define HIR_MAX_INST   16384
#define HIR_MAX_BLOCK  2048
#define HIR_MAX_CARG   4096
#define HIR_MAX_PARG   8192

/* --- Instruction parallel arrays --- */
static int   h_kind[HIR_MAX_INST];
static int   h_ty[HIR_MAX_INST];
static int   h_src1[HIR_MAX_INST];
static int   h_src2[HIR_MAX_INST];
static int   h_val[HIR_MAX_INST];
static char *h_name[HIR_MAX_INST];
static int   h_blk[HIR_MAX_INST];
static int   h_ninst;

/* Call arguments (flat array, indexed per-call) */
static int   h_carg[HIR_MAX_CARG];
static int   h_cbase[HIR_MAX_INST];
static int   h_ncarg;

/* Phi arguments (Phase B) */
static int   h_pblk[HIR_MAX_PARG];
static int   h_pval[HIR_MAX_PARG];
static int   h_pbase[HIR_MAX_INST];
static int   h_pcnt[HIR_MAX_INST];
static int   h_nparg;

/* Basic blocks */
static int   bb_start[HIR_MAX_BLOCK];
static int   bb_end[HIR_MAX_BLOCK];
static int   bb_nblk;

/* Current block being built (used by lowering) */
static int   hl_cur_blk;

/* --- Functions --- */

static void hir_reset(void) {
    h_ninst = 0;
    h_ncarg = 0;
    h_nparg = 0;
    bb_nblk = 0;
    hl_cur_blk = -1;
}

static int hir_new_block(void) {
    int b;
    b = bb_nblk;
    if (b >= HIR_MAX_BLOCK) {
        fputs("s12cc: too many HIR blocks\n", stderr);
        exit(1);
    }
    bb_start[b] = -1;  /* set lazily when block becomes current */
    bb_end[b] = -1;
    bb_nblk = bb_nblk + 1;
    return b;
}

/* Switch current block; fix up start/end to current h_ninst */
static void hl_switch_block(int blk) {
    hl_cur_blk = blk;
    bb_start[blk] = h_ninst;
    bb_end[blk] = h_ninst;
}

static int hi_emit(int kind, int ty, int s1, int s2, int val, char *name) {
    int idx;
    idx = h_ninst;
    if (idx >= HIR_MAX_INST) {
        fputs("s12cc: too many HIR instructions\n", stderr);
        exit(1);
    }
    h_kind[idx] = kind;
    h_ty[idx] = ty;
    h_src1[idx] = s1;
    h_src2[idx] = s2;
    h_val[idx] = val;
    h_name[idx] = name;
    h_blk[idx] = hl_cur_blk;
    h_cbase[idx] = -1;
    h_pbase[idx] = -1;
    h_pcnt[idx] = 0;
    h_ninst = h_ninst + 1;
    if (hl_cur_blk >= 0) {
        bb_end[hl_cur_blk] = h_ninst;
    }
    return idx;
}

/* Does an instruction produce a value? */
static int hi_has_value(int kind) {
    if (kind == HI_NOP) return 0;
    if (kind == HI_STORE) return 0;
    if (kind == HI_BR) return 0;
    if (kind == HI_BRC) return 0;
    if (kind == HI_RET) return 0;
    return 1;
}

/* Can a value be rematerialized (no spill needed)? */
static int hi_is_remat(int kind) {
    if (kind == HI_ICONST) return 1;
    if (kind == HI_ALLOCA) return 1;
    if (kind == HI_GADDR) return 1;
    if (kind == HI_SADDR) return 1;
    if (kind == HI_FADDR) return 1;
    if (kind == HI_GETFP) return 1;
    return 0;
}

/* Is this instruction kind safe to hoist/CSE? Pure, non-faulting. */
static int hi_is_pure(int k) {
    /* Binary arithmetic/logic/comparison, excluding DIV/REM */
    if (k >= HI_ADD && k <= HI_SGEU) {
        if (k == HI_DIV || k == HI_REM) return 0;
        return 1;
    }
    if (k == HI_NEG) return 1;
    if (k == HI_NOT) return 1;
    if (k == HI_BNOT) return 1;
    if (k == HI_ADDI) return 1;
    if (k == HI_COPY) return 1;
    if (k == HI_ICONST) return 1;
    if (k == HI_PARAM) return 1;
    if (k == HI_ALLOCA) return 1;
    if (k == HI_GADDR) return 1;
    if (k == HI_SADDR) return 1;
    if (k == HI_FADDR) return 1;
    if (k == HI_GETFP) return 1;
    return 0;
}

/* Is the current block terminated? */
static int hl_terminated(void) {
    int last;
    if (hl_cur_blk < 0) return 1;
    if (bb_start[hl_cur_blk] >= bb_end[hl_cur_blk]) return 0;
    last = bb_end[hl_cur_blk] - 1;
    if (h_kind[last] == HI_BR) return 1;
    if (h_kind[last] == HI_BRC) return 1;
    if (h_kind[last] == HI_RET) return 1;
    return 0;
}
