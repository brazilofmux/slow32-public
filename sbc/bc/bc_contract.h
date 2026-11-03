/* bc_contract.h -- what the copied stage08 backend (hir/) reads of a
 * front end, supplied for s32-bc.  Taken from fortran/src/f77_contract.h,
 * which explains each group: diagnostics, the type encoding and its
 * predicates, the alloca registry the SSA promoter scans, the C
 * inliner's inert hooks, and the global-data tables gen_data() emits.
 * BASIC's values cross to the backend as INTEGER/LONG -> TY_INT, SINGLE
 * -> TY_FLOAT, DOUBLE -> two TY_INT words; strings are bc_rt.c's.
 */
#ifndef BC_CONTRACT_H
#define BC_CONTRACT_H

/* --- 1. diagnostic output ------------------------------------------ */

static void fdputs(char *s, int fd) { write(fd, s, strlen(s)); }
static void fdputc(int c, int fd) { char b; b = (char)c; write(fd, &b, 1); }
static void fdputuint(int fd, unsigned int v) {
    char b[16];
    int n;
    n = 0;
    if (v == 0) { fdputc('0', fd); return; }
    while (v > 0) { b[n] = (char)('0' + (v % 10)); v = v / 10; n = n + 1; }
    while (n > 0) { n = n - 1; fdputc(b[n], fd); }
}

/* The backend's tables grow through the copied registry (tables.h). */
#include "hir/tables.h"

/* --- 2. type encoding (bit-identical to selfhost/src/ast.h) --------- */

#define TY_INT    0
#define TY_CHAR   1
#define TY_SHORT  2
#define TY_VOID   3
#define TY_LLONG  4
#define TY_FLOAT  5
#define TY_DOUBLE 6
#define TY_I128   7
#define TY_STRUCT_BASE 8
#define TY_PTR       256
#define TY_UNSIGNED  0x4000
#define TY_BASE_MASK 0x00FF
#define TY_PTR_MASK  0x3F00

static int ty_ptr_size = 4;   /* SLOW-32 is the only target, by ruling */

static int ty_is_llong(int ty) {
    return !(ty & TY_PTR_MASK) && (ty & TY_BASE_MASK) == TY_LLONG;
}
static int ty_is_float(int ty) {
    return !(ty & TY_PTR_MASK) && (ty & TY_BASE_MASK) == TY_FLOAT;
}
static int ty_is_double(int ty) {
    return !(ty & TY_PTR_MASK) && (ty & TY_BASE_MASK) == TY_DOUBLE;
}
static int ty_is_fp(int ty) {
    return ty_is_float(ty) || ty_is_double(ty);
}
static int ty_is_ptr(int ty) { return (ty & TY_PTR_MASK) != 0; }
/* Fortran 77 has no derived types; the shared ssa pair-split pass
 * asks, so answer no. */
static int ty_is_struct(int ty) { (void)ty; return 0; }

static int ty_size(int ty) {
    if (ty & TY_PTR_MASK) return ty_ptr_size;
    if ((ty & TY_BASE_MASK) == TY_DOUBLE) return 8;
    if ((ty & TY_BASE_MASK) == TY_LLONG) return 8;
    if ((ty & TY_BASE_MASK) == TY_FLOAT) return 4;
    if ((ty & TY_BASE_MASK) == TY_CHAR) return 1;
    if ((ty & TY_BASE_MASK) == TY_SHORT) return 2;
    if ((ty & TY_BASE_MASK) == TY_VOID) return 1;
    return 4;
}

/* --- 3. alloca registry scanned by the SSA promoter ----------------- */

/* As selfhost/src/hir_lower.h has them, grown (tables.h, TAB_HL_ALLOCA):
 * the SSA pass registers its own per-alloca table under the same kind.
 * They were a fixed 4096, and five of the writers did not check. */
static int *hl_aoff;    /* frame offset of each ALLOCA */
static int *hl_aslot;   /* the C parser's slot id; always 0 here (keyed by
                         * offset) -- it only matters to the SSA pass in a
                         * function that calls setjmp, which no Fortran does */
static int *hl_ainst;   /* HIR index of each ALLOCA */
static int hl_nalloca;
static int hl_alloca_cap;

/* room for n more allocas (hir_lower.h's, verbatim) */
static void hl_alloca_room(int n) {
    if (hl_nalloca + n <= hl_alloca_cap) return;
    if (hl_alloca_cap == 0) {
        tab_add((char **)&hl_aoff, (int)sizeof(int), TAB_HL_ALLOCA, 0);
        tab_add((char **)&hl_aslot, (int)sizeof(int), TAB_HL_ALLOCA, 0);
        tab_add((char **)&hl_ainst, (int)sizeof(int), TAB_HL_ALLOCA, 0);
    }
    tab_grow(TAB_HL_ALLOCA, &hl_alloca_cap, 1024, hl_nalloca + n);
}

/* register ALLOCA inst at frame offset off */
static void bc_alloca_add(int inst, int off) {
    hl_alloca_room(1);
    hl_ainst[hl_nalloca] = inst;
    hl_aoff[hl_nalloca] = off;
    hl_aslot[hl_nalloca] = 0;
    hl_nalloca = hl_nalloca + 1;
}
static int hl_temp_stack;            /* bytes of compiler temporaries */
static int hl_nparams;               /* flat incoming-parameter count */

/* --- 3b. the C inliner's hooks (selfhost hir_lower.h) ---------------- */
/* gen_program() sets these up from S12CC_INLINE* and calls
 * hl_inl_prepare() before the first unit.  Fortran inlines at the
 * front end (f77_parse.h splices units, F77_INLINE_MAX), so the hooks
 * are inert here: the budget stays whatever the environment says and
 * nothing reads it. */
static Node *hl_prog;
static int   hl_inline_max;
static int   hl_inl_move_max;
static int   hl_inl_huge;
static int   hl_stat_inlined;
static int   hl_stat_inl_sel;
static int   hl_stat_inl_direct;
static int   hl_inl_tiny;            /* S12CC_INLINE_TINY, the tiny-body budget */
static int   hl_unroll_budget;       /* S12CC_UNROLL, the C unroller's budget */
static int   hl_peel_budget;         /* S12CC_PEEL, set by gen_program */
static void  hl_inl_prepare(Node *prog) { (void)prog; }

/* --- 4. global data tables walked by gen_data() --------------------- */

#define P_MAX_GLOBALS 8192
#define PS_MAX_INIT_POOL 262144
#define PS_MAX_INIT_RELOCS 8192

static char *ps_gname[P_MAX_GLOBALS];
static int   ps_gtype[P_MAX_GLOBALS];
static int   ps_gsize[P_MAX_GLOBALS];
static int   ps_ginit[P_MAX_GLOBALS];
static int   ps_ginit_hi[P_MAX_GLOBALS];
static int   ps_gstr[P_MAX_GLOBALS];
static int   ps_glocal[P_MAX_GLOBALS];
static int   ps_gextern[P_MAX_GLOBALS];
static int   ps_galign[P_MAX_GLOBALS];   /* C's _Alignas on a global: 0, none */
static int   ps_gconst[P_MAX_GLOBALS];   /* a const object, for .rodata: 0, none
                                          * yet (Fortran could mark PARAMETER
                                          * arrays and the like) */
static int   ps_nglobals;

static unsigned char ps_ginit_pool[PS_MAX_INIT_POOL];
static int ps_ginit_start[P_MAX_GLOBALS];
static int ps_ginit_count[P_MAX_GLOBALS];
static int ps_ginit_pool_len;

#define GIRELOC_STRING 0
#define GIRELOC_GLOBAL 1
#define GIRELOC_SYMBOL 2
static int   ps_girel_start[P_MAX_GLOBALS];
static int   ps_girel_count[P_MAX_GLOBALS];
static int   ps_girel_off[PS_MAX_INIT_RELOCS];
static int   ps_girel_kind[PS_MAX_INIT_RELOCS];
static int   ps_girel_idx[PS_MAX_INIT_RELOCS];
static int   ps_girel_size[PS_MAX_INIT_RELOCS];
static int   ps_girel_add[PS_MAX_INIT_RELOCS];
static char *ps_girel_name[PS_MAX_INIT_RELOCS];

/* String pool: Fortran character constants (Hollerith and '...' alike)
 * land here and are emitted as .L.str labels by gen_data(). */
#define LEX_STRPOOL_MAX 262144
#define LEX_MAX_STRINGS 8192
static char lex_strpool[LEX_STRPOOL_MAX];
static int  lex_str_off[LEX_MAX_STRINGS];
static int  lex_str_len[LEX_MAX_STRINGS];
static int  lex_str_count;
static int  lex_strpool_len;
static char lex_str_used[LEX_MAX_STRINGS];   /* set by the backend as it refers to one; only those are emitted */
static int  lex_str_width[LEX_MAX_STRINGS];  /* C's wide literals: 0, all narrow */

/* Intern a string constant; returns its pool index. */
static int bc_intern_str(char *s, int len) {
    int idx;
    int i;
    idx = lex_str_count;
    lex_str_off[idx] = lex_strpool_len;
    lex_str_len[idx] = len;
    i = 0;
    while (i < len) {
        lex_strpool[lex_strpool_len] = s[i];
        lex_strpool_len = lex_strpool_len + 1;
        i = i + 1;
    }
    lex_strpool[lex_strpool_len] = 0;
    lex_strpool_len = lex_strpool_len + 1;
    lex_str_count = idx + 1;
    return idx;
}

/* Label counter shared with the backend's branch emission (same shape
 * as selfhost's parser.h: a monotonic counter behind an allocator). */
static int cg_lbl;

static int cg_label(void) {
    int l;
    l = cg_lbl;
    cg_lbl = cg_lbl + 1;
    return l;
}

/* Defined by hir_regalloc.h / the Fortran lowering respectively; the
 * backend calls both before their definitions appear. */
static void ra_dump_signed(int v);
static void hl_func(Node *fn);

/* ra_dump_signed is defined by hir_regalloc.h itself. */

#endif
