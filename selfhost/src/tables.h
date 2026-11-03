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
#define TAB_PP_DEFS  23    /* pp.h: the macros defined, pp_ndefs */
#define TAB_CT_PLIST 24    /* ctype.h: parameter-type lists */
#define TAB_CT_REC   25    /* ctype.h: C type records, ct_n */
#define TAB_ST_MEMB  26    /* ast.h, struct member */
#define TAB_PS_TYPEDEFS 27    /* parser.h, typedef */
#define TAB_PS_CONSTS 28    /* parser.h, enum constant */
#define TAB_PS_GIREL 29    /* parser.h, initializer relocation */
#define TAB_HL_INLSYM 30    /* hir_lower.h, inline candidate */
#define TAB_PS_LOCALS 31    /* parser.h, local */
#define TAB_PS_GLOBALS 32    /* parser.h, global */
#define TAB_PS_FPARAM 33    /* parser.h, parameter type */
#define TAB_LEX_STR  34    /* c_lexer.rl, string literal */
#define TAB_LEX_POOL 35    /* c_lexer.rl, the string-literal pool's bytes */
#define TAB_PS_STRCAT 39   /* parser.h, the adjacent literals being joined */
#define TAB_PP_FILES 36    /* pp.h, source file */
#define TAB_PP_DIS   37    /* pp.h, nested macro expansion */
#define TAB_PP_ONCE  38    /* pp.h, #pragma once file */

#define TAB_TABLES 512
#define TAB_KIND_MAX 128   /* kinds are numbered below this */
#define TAB_HL_LOOP  40    /* hir_lower.h, loop nesting */
#define TAB_HL_SWNEST 41    /* hir_lower.h, switch nesting */
#define TAB_PS_KR    42    /* parser.h, K&R parameter */
#define TAB_PS_VGOTO 43    /* parser.h, goto by a VLA */
#define TAB_PS_VD    44    /* parser.h, VLA object */
#define TAB_PS_VC    45    /* parser.h, VLA count */
#define TAB_AST_SUBS 46    /* ast.h, array shape */
#define TAB_PS_VPEND 47    /* parser.h, VLA count assignments pending */
#define TAB_PS_VPP   48    /* parser.h, a parameter's VLA counts */
#define TAB_GC_NODE  49    /* hir_regalloc.h, interference-graph node */
#define TAB_GC_MOVE  50    /* hir_regalloc.h, move */
#define TAB_GC_NML   51    /* hir_regalloc.h, node-move list entry */
#define TAB_CG_FN    52    /* hir_codegen.h, function emitted (dead-static DCE) */
#define TAB_CG_REF   53    /* hir_codegen.h, name referenced from data */
#define TAB_CG_EDGE  54    /* hir_codegen.h, name referenced from a function */
#define TAB_PP_INC   55    /* pp.h, #include nesting level */
#define TAB_PP_IDIR  56    /* pp.h, -I directory */
#define TAB_HL_CTEMP 57    /* hir_lower.h, complex temporary */
#define TAB_LEX_SRC  58    /* c_lexer.rl, the translation unit's text (lex_src) */
#define TAB_PP_PRE   59    /* pp.h, pre-expanded macro arguments */
#define TAB_ST       60    /* ast.h, structure or union */
#define TAB_BG_COST  61    /* hir_burg.h, instruction selection's costs and rules */
#define TAB_CG_JT    62    /* hir_codegen.h, jump table */
#define TAB_CG_JTENT 63    /* hir_codegen.h, jump-table entry */

/* each kind's owner, by its cap variable: a kind number given twice
 * (TAB_PP_FILES and TAB_PS_STRCAT were both 36 once, so the file names
 * were reallocated at the joined literals' size) is caught at the
 * second owner's first growth */
static int *tab_kind_capp[TAB_KIND_MAX];
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
    if (kind == TAB_PP_DEFS) return "#define";
    if (kind == TAB_CT_PLIST) return "parameter type list";
    if (kind == TAB_CT_REC) return "C type";
    if (kind == TAB_ST_MEMB) return "struct member";
    if (kind == TAB_PS_TYPEDEFS) return "typedef";
    if (kind == TAB_PS_CONSTS) return "enum constant";
    if (kind == TAB_PS_GIREL) return "initializer relocation";
    if (kind == TAB_HL_INLSYM) return "inline candidate";
    if (kind == TAB_PS_LOCALS) return "local";
    if (kind == TAB_PS_GLOBALS) return "global";
    if (kind == TAB_PS_FPARAM) return "parameter type";
    if (kind == TAB_LEX_STR) return "string literal";
    if (kind == TAB_LEX_POOL) return "string literal bytes";
    if (kind == TAB_PS_STRCAT) return "adjacent string literal";
    if (kind == TAB_PP_FILES) return "source file";
    if (kind == TAB_PP_DIS) return "nested macro expansion";
    if (kind == TAB_PP_ONCE) return "#pragma once file";
    if (kind == TAB_HL_LOOP) return "loop nesting";
    if (kind == TAB_HL_SWNEST) return "switch nesting";
    if (kind == TAB_PS_KR) return "K&R parameter";
    if (kind == TAB_PS_VGOTO) return "goto by a VLA";
    if (kind == TAB_PS_VD) return "VLA object";
    if (kind == TAB_PS_VC) return "VLA count";
    if (kind == TAB_AST_SUBS) return "array shape";
    if (kind == TAB_PS_VPEND) return "VLA count assignment";
    if (kind == TAB_PS_VPP) return "VLA parameter count";
    if (kind == TAB_GC_NODE) return "interference node";
    if (kind == TAB_GC_MOVE) return "move";
    if (kind == TAB_GC_NML) return "node-move list";
    if (kind == TAB_CG_FN) return "function emitted";
    if (kind == TAB_CG_REF) return "data reference";
    if (kind == TAB_CG_EDGE) return "function reference";
    if (kind == TAB_PP_INC) return "#include nesting";
    if (kind == TAB_PP_IDIR) return "-I directory";
    if (kind == TAB_HL_CTEMP) return "complex temporary";
    if (kind == TAB_LEX_SRC) return "source text";
    if (kind == TAB_PP_PRE) return "pre-expanded argument";
    if (kind == TAB_ST) return "structure";
    if (kind == TAB_BG_COST) return "selection cost";
    if (kind == TAB_CG_JT) return "jump table";
    if (kind == TAB_CG_JTENT) return "jump-table entry";
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
    if (kind < 0 || kind >= TAB_KIND_MAX ||
        (tab_kind_capp[kind] != 0 && tab_kind_capp[kind] != capp)) {
        fdputs("s12cc: table kind ", 2);
        fdputuint(2, (unsigned)kind);
        fdputs(" has two owners (tables.h)\n", 2);
        exit(1);
    }
    tab_kind_capp[kind] = capp;
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

/* A name index over a table of names (char **, entries 0..count-1, each
 * set once and never changed): lookups as a scan from the last entry to
 * the first would make -- the highest index with the name -- in a hash
 * of newest-first chains.  It catches up on new entries at each lookup,
 * and lets entries go when count goes down (a block's names, from the
 * top: the newest of each chain).  The scans were quadratic in a
 * translation unit's globals, constants, typedefs and functions. */
struct tab_nix {
    int *head;          /* bucket -> entry, -1 none */
    int cap;            /* buckets, a power of two */
    int *next;          /* entry -> next entry in its chain */
    int ncap;           /* entries next has room for */
    int hashed;         /* entries 0..hashed-1 are in */
};

static unsigned tab_nix_hash(char *s) {
    unsigned h;
    h = 2166136261u;
    while (*s) {
        h = (h ^ (unsigned)(*s & 255)) * 16777619u;
        s = s + 1;
    }
    return h;
}

static void tab_nix_add(struct tab_nix *x, char **names, int k) {
    int b;
    b = (int)(tab_nix_hash(names[k]) & (unsigned)(x->cap - 1));
    x->next[k] = x->head[b];
    x->head[b] = k;
}

static void tab_nix_sync(struct tab_nix *x, char **names, int count) {
    int k;
    int b;
    while (x->hashed > count) {         /* the top entries went away */
        k = x->hashed - 1;
        b = (int)(tab_nix_hash(names[k]) & (unsigned)(x->cap - 1));
        x->head[b] = x->next[k];
        x->hashed = k;
    }
    if (count > x->ncap) {
        x->next = (int *)realloc(x->next, (unsigned long)(2 * count) * sizeof(int));
        if (!x->next) {
            fdputs("s12cc: out of memory (name index)\n", 2);
            exit(1);
        }
        x->ncap = 2 * count;
    }
    if (count > x->cap) {               /* at most one entry a bucket, on average */
        k = x->cap ? x->cap : 1024;
        while (k < count) k = k * 2;
        k = k * 2;
        if (x->head) free(x->head);
        x->head = (int *)malloc((unsigned long)k * sizeof(int));
        if (!x->head) {
            fdputs("s12cc: out of memory (name index)\n", 2);
            exit(1);
        }
        x->cap = k;
        k = 0;
        while (k < x->cap) { x->head[k] = -1; k = k + 1; }
        k = 0;
        while (k < x->hashed) { tab_nix_add(x, names, k); k = k + 1; }
    }
    while (x->hashed < count) {
        tab_nix_add(x, names, x->hashed);
        x->hashed = x->hashed + 1;
    }
}

/* the highest index whose name is `name`, or -1 */
static int tab_nix_find(struct tab_nix *x, char **names, int count, char *name) {
    int k;
    if (count <= 0) return -1;
    tab_nix_sync(x, names, count);
    k = x->head[(int)(tab_nix_hash(name) & (unsigned)(x->cap - 1))];
    while (k >= 0) {
        if (strcmp(names[k], name) == 0) return k;
        k = x->next[k];
    }
    return -1;
}
