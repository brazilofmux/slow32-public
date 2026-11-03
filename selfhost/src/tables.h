/* tables.h -- tables that grow
 *
 * A compiler with fixed tables stops at the first program bigger than
 * whoever sized them guessed, and the ones whose adds are not checked
 * (or are checked and then carry on) overwrite their neighbours first:
 * a struct-member pool filled to its end made SQLite's shell report
 * "cannot open include: pwd.h", and a full function table dropped
 * declarations, so calls in s12cc.c came out undeclared.
 *
 * Parallel tables -- several arrays indexed alike, each entry of each
 * one esz bytes -- are registered under a kind (tab_add) and grown
 * together (tab_grow), so none is ever shorter than another.  A kind's
 * count of entries in use is its owner's; the registry keeps the
 * tables and, through the owner's cap variable, how many each holds.
 * New entries are zero, as the static arrays' were.  The kinds are
 * listed here, each with the name S12CC_TABDBG and the out-of-memory
 * message give it. */

#define TAB_HIR_BLK   0    /* hir.h: block-indexed, hir_blk_cap */
#define TAB_HIR_INST  1    /* hir.h: instruction-indexed, hir_inst_cap */
#define TAB_HIR_PARG  2    /* hir.h: phi arguments, hir_parg_cap */
#define TAB_HIR_EDGE  3    /* hir_regalloc.h: interference edges */
#define TAB_PS_FUNCS  4    /* parser.h: the functions declared, ps_nfuncs */
#define TAB_PP_EXP    5    /* pp.h: one macro's replacement text, pp_exp */
#define TAB_SSA_SUCC  6    /* hir_ssa.h: CFG successor lists */
#define TAB_SSA_PRED  7    /* hir_ssa.h: CFG predecessor lists */
#define TAB_SSA_DTC   8    /* hir_ssa.h: dominator-tree children */
#define TAB_SSA_PLOG  9    /* hir_ssa.h: rename's push log */
#define TAB_HIR_CARG 10    /* hir.h: call arguments, flat (h_carg, h_ncarg) */
#define TAB_HL_ALLOCA 11   /* hir_lower.h: the function's allocas, hl_nalloca */
#define TAB_HL_CASE  12    /* hir_lower.h: case labels of the open switches */
#define TAB_HL_GOTO  13    /* hir_lower.h: goto labels' blocks, hl_ngoto */
#define TAB_PS_LABELS 14   /* parser.h: a function's labels, ps_nlabels */
#define TAB_SSA_PROMO 15   /* hir_ssa.h: the variables promoted, ssa_npromo */
#define TAB_HCG_PHI  16    /* hir_codegen.h: one edge's phi copies */
#define TAB_PP_IF    17    /* pp.h: the open #if levels, pp_dep */
#define TAB_PP_MK    18    /* pp.h: their copies at the parser's lexer marks */
#define TAB_PP_BODY  19    /* pp.h: the #define body being read */
#define TAB_PP_SIG   20    /* pp.h: a definition's signature, for redefinition */
#define TAB_PP_PARMS 21    /* pp.h: a function-like macro's parameter names */
#define TAB_HL_PARAM 22    /* hir.h: a function's incoming parameter slots */

#define TAB_TABLES 512
static char **tab_ptr[TAB_TABLES];
static int tab_esz[TAB_TABLES];
static int tab_kind[TAB_TABLES];
static int tab_n;

static char *tab_kind_name(int kind) {
    if (kind == TAB_HIR_BLK) return "blk";
    if (kind == TAB_HIR_INST) return "inst";
    if (kind == TAB_HIR_PARG) return "parg";
    if (kind == TAB_HIR_EDGE) return "edge";
    if (kind == TAB_PS_FUNCS) return "function";
    if (kind == TAB_PP_EXP) return "macro expansion";
    if (kind == TAB_SSA_SUCC) return "CFG successor";
    if (kind == TAB_SSA_PRED) return "CFG predecessor";
    if (kind == TAB_SSA_DTC) return "dominator tree";
    if (kind == TAB_SSA_PLOG) return "SSA push log";
    if (kind == TAB_HIR_CARG) return "call argument";
    if (kind == TAB_HL_ALLOCA) return "alloca";
    if (kind == TAB_HL_CASE) return "case label";
    if (kind == TAB_HL_GOTO) return "goto label";
    if (kind == TAB_PS_LABELS) return "label";
    if (kind == TAB_SSA_PROMO) return "promoted variable";
    if (kind == TAB_HCG_PHI) return "phi copy";
    if (kind == TAB_PP_IF) return "#if level";
    if (kind == TAB_PP_MK) return "#if level copy";
    if (kind == TAB_PP_BODY) return "#define body";
    if (kind == TAB_PP_SIG) return "macro signature";
    if (kind == TAB_PP_PARMS) return "macro parameter";
    if (kind == TAB_HL_PARAM) return "parameter slot";
    return "?";
}

/* p's table from oldcap entries of esz bytes to cap, the new ones zero */
static char *tab_resize(char *p, int esz, int oldcap, int cap) {
    char *q;
    int i;
    q = (char *)realloc(p, (unsigned long)cap * (unsigned long)esz);
    if (!q) {
        fdputs("s12cc: out of memory (tables)\n", 2);
        exit(1);
    }
    i = oldcap * esz;
    while (i < cap * esz) { q[i] = 0; i = i + 1; }
    return q;
}

/* register *p as one of kind's tables, cap entries already (0: none yet) */
static void tab_add(char **p, int esz, int kind, int cap) {
    if (tab_n >= TAB_TABLES) {
        fdputs("s12cc: too many tables (TAB_TABLES)\n", 2);
        exit(1);
    }
    tab_ptr[tab_n] = p;
    tab_esz[tab_n] = esz;
    tab_kind[tab_n] = kind;
    tab_n = tab_n + 1;
    if (cap > 0) *p = tab_resize(*p, esz, 0, cap);
}

/* every table of the kind from oldcap entries to cap; 0, all left at
 * oldcap, when the heap has not that much (the ones already grown are
 * shrunk back, which realloc always can) */
static int tab_try(int kind, int oldcap, int cap) {
    int i;
    int j;
    char *q;
    i = 0;
    while (i < tab_n) {
        if (tab_kind[i] == kind) {
            q = (char *)realloc(*tab_ptr[i], (unsigned long)cap * (unsigned long)tab_esz[i]);
            if (!q) {
                j = 0;
                while (j < i) {
                    if (tab_kind[j] == kind)
                        *tab_ptr[j] = (char *)realloc(*tab_ptr[j], (unsigned long)oldcap * (unsigned long)tab_esz[j]);
                    j = j + 1;
                }
                return 0;
            }
            j = oldcap * tab_esz[i];
            while (j < cap * tab_esz[i]) { q[j] = 0; j = j + 1; }
            *tab_ptr[i] = q;
        }
        i = i + 1;
    }
    return 1;
}

/* every table of the kind to need entries at least, from *capp (first,
 * when there are none yet): half as much again each time (the HIR
 * instruction tables are ~300 bytes an instruction, so doubling past
 * 262144 asks the heap for 160MB), and when that is more than the heap
 * has, halfway back toward need, and so on to need exactly -- so a
 * program near the heap's end still grows in steps, not an entry at a
 * time */
static void tab_grow(int kind, int *capp, int first, int need) {
    int cap;
    cap = *capp ? *capp : first;
    while (cap < need) cap = cap + cap / 2;
    if (cap == *capp) return;
    if (getenv("S12CC_TABDBG")) { fdputs("grow ", 2); fdputs(tab_kind_name(kind), 2); fdputs(" ", 2); fdputuint(2, (unsigned)cap); fdputs("\n", 2); }   /* S12CC_TABDBG: each growth, on stderr */
    while (!tab_try(kind, *capp, cap)) {
        if (cap == need) {
            fdputs("s12cc: out of memory (", 2);
            fdputs(tab_kind_name(kind), 2);
            fdputs(" tables)\n", 2);
            exit(1);
        }
        cap = need + (cap - need) / 2;
        if (getenv("S12CC_TABDBG")) { fdputs("  (backing off to) ", 2); fdputuint(2, (unsigned)cap); fdputs("\n", 2); }
    }
    *capp = cap;
}

/* A kind with one table, *p, of esz-byte entries: room for need of them
 * (registered at the first call; first entries the first time). */
static void tab_room1(char **p, int esz, int kind, int *capp, int first, int need) {
    if (need <= *capp) return;
    if (*capp == 0) tab_add(p, esz, kind, 0);
    tab_grow(kind, capp, first, need);
}

/* A block of nt int arrays of cap entries each, one after another, that
 * starts in its user's frame and moves to the heap when it fills: the
 * block twice as big, the first n entries of each array kept, the old
 * block freed when it was the heap's (heap != 0).  For lists that are
 * short nearly always and must not be limited -- a macro's arguments,
 * a function's parameters (C11 5.2.4.1: 127 of each at least). */
static int *tab_iblk_grow(int *old, int nt, int cap, int n, int heap) {
    int *nb;
    int t;
    int k;
    nb = (int *)malloc((unsigned long)(nt * cap * 2) * sizeof(int));
    if (!nb) {
        fdputs("s12cc: out of memory (argument lists)\n", 2);
        exit(1);
    }
    t = 0;
    while (t < nt) {
        k = 0;
        while (k < n) { nb[t * cap * 2 + k] = old[t * cap + k]; k = k + 1; }
        t = t + 1;
    }
    if (heap) free(old);
    return nb;
}
