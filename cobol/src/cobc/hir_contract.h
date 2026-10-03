/* s32-cobc: the front-end side of the copied SLOW-32 HIR backend
 * (src/hir/, copied from selfhost at the vintage each file records).
 * A part of one translation unit, included by s32-cobc.c in order.
 *
 * The backend is stage08's: SSA construction, the optimizer, LICM, BURG
 * selection, graph-colouring register allocation and SLOW-32 emission.
 * It is language-neutral but for a few dozen names the C compiler's
 * front end happened to define, and this file supplies them, on the
 * model of fortran/src/f77_contract.h:
 *
 *   1. fd* diagnostic output
 *   2. the type encoding and ty_* predicates (bit-identical to
 *      selfhost/src/ast.h, so the backend's pair handling, comparison
 *      lowering and ABI are exactly stage08's)
 *   3. the alloca registry the SSA promoter scans
 *   4. the global-data tables gen_data() walks -- empty here: COBOL's
 *      data is laid out by layout.h and emitted by the text emitter;
 *      an island reaches it through HI_GADDR of the record's label
 *   5. the Node a function is handed in as, and the inliner's knobs
 *      (never used: an island is one function, compiled on its own)
 *
 * Why a copy and not a symlink (docs/plans/census.md): selfhost must be
 * free to change its HIR without breaking s32-cobc, and selfhost never
 * depends on anything built elsewhere in the tree.  Fortran made the
 * same choice.  Re-sync deliberately, recording the vintage; the copy's
 * divergences are listed at the top of hir/hir.h.
 *
 * What the backend writes (cg_out) is a function's text with 4-space
 * indents; hir_take_text() moves it into g_asm one line at a time with
 * the indent made a tab, so relax_branches counts each instruction as
 * the 4 bytes it is.  The backend spells li/la out and emits no
 * pseudo-instructions, so that count is exact.  Labels come from
 * new_label() through cg_label(), one numbering with the rest of the
 * unit's text. */

#include <fcntl.h>   /* the backend's dead-static DCE reopens its output; it never runs here */

/* --- 1. diagnostic output ------------------------------------------ */

static void fdputs(char *s, int fd) { if (write(fd, s, strlen(s)) < 0) {} }
static void fdputc(int c, int fd) { char b = (char)c; if (write(fd, &b, 1) < 0) {} }
static void fdputuint(int fd, unsigned int v)
{
    char b[16]; int n = 0;
    if (v == 0) { fdputc('0', fd); return; }
    while (v > 0) { b[n++] = (char)('0' + v % 10); v /= 10; }
    while (n > 0) fdputc(b[--n], fd);
}

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

static int ty_ptr_size = 4;

static int ty_is_llong(int ty)  { return !(ty & TY_PTR_MASK) && (ty & TY_BASE_MASK) == TY_LLONG; }
static int ty_is_float(int ty)  { return !(ty & TY_PTR_MASK) && (ty & TY_BASE_MASK) == TY_FLOAT; }
static int ty_is_double(int ty) { return !(ty & TY_PTR_MASK) && (ty & TY_BASE_MASK) == TY_DOUBLE; }
static int ty_is_fp(int ty)     { return ty_is_float(ty) || ty_is_double(ty); }
static int ty_is_ptr(int ty)    { return (ty & TY_PTR_MASK) != 0; }
static int ty_is_struct(int ty) { (void)ty; return 0; }   /* no derived types reach the backend */

static int ty_size(int ty)
{
    if (ty & TY_PTR_MASK) return ty_ptr_size;
    switch (ty & TY_BASE_MASK) {
    case TY_DOUBLE: case TY_LLONG: return 8;
    case TY_CHAR: case TY_VOID: return 1;
    case TY_SHORT: return 2;
    default: return 4;
    }
}

/* --- 3. alloca registry scanned by the SSA promoter ----------------- */

#define HL_MAX_ALLOCA 4096
static int hl_ainst[HL_MAX_ALLOCA];  /* HIR index of each ALLOCA */
static int hl_aoff[HL_MAX_ALLOCA];   /* frame offset of each ALLOCA */
static int hl_aslot[HL_MAX_ALLOCA];  /* the declaration's slot id; 0 = a temporary (stage08's setjmp rule asks) */
static int hl_nalloca;
static int hl_temp_stack;            /* bytes of compiler temporaries */
static int hl_nparams;               /* flat incoming-parameter count */

/* --- 4. global data tables walked by gen_data() -- all empty --------- */

#define P_MAX_GLOBALS 16
#define PS_MAX_INIT_POOL 16
#define PS_MAX_INIT_RELOCS 16

static char *ps_gname[P_MAX_GLOBALS];
static int   ps_gtype[P_MAX_GLOBALS];
static int   ps_gsize[P_MAX_GLOBALS];
static int   ps_ginit[P_MAX_GLOBALS];
static int   ps_ginit_hi[P_MAX_GLOBALS];
static int   ps_gstr[P_MAX_GLOBALS];
static int   ps_glocal[P_MAX_GLOBALS];
static int   ps_gextern[P_MAX_GLOBALS];
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

#define LEX_STRPOOL_MAX 16
#define LEX_MAX_STRINGS 16
static char lex_strpool[LEX_STRPOOL_MAX];
static int  lex_str_off[LEX_MAX_STRINGS];
static int  lex_str_len[LEX_MAX_STRINGS];
static int  lex_str_count;
static int  lex_strpool_len;

/* the backend's labels are the unit's labels */
static int cg_label(void) { return new_label(); }

/* --- 5. the function node, and what the inliner would read ---------- */

typedef struct Node {
    char        *name;         /* the island's label */
    int          locals_size;  /* frame bytes for its named locals (allocas) */
    int          nparams;      /* 0: an island takes nothing */
    int          is_varargs;   /* 0 */
    int          is_static;    /* 1: local to the unit's text */
    struct Node *next;
    struct Node *body;
} Node;

static Node *hl_prog;
static int hl_stat_inlined, hl_stat_inl_sel, hl_stat_inl_direct;
static int hl_inline_max, hl_inl_move_max, hl_inl_huge;
static void hl_inl_prepare(Node *prog) { (void)prog; }

static int s12cc_dump_intervals;     /* the backend's -dintervals; never set here */

/* Defined by hir_regalloc.h and by lower.h respectively; the backend
 * calls both before their definitions appear. */
static void ra_dump_signed(int v);
static void hl_func(Node *fn);
