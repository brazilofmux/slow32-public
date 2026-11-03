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
#define TAB_KINDS     4

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
