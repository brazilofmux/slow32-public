/* s32-cobc: symbol table.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ====================================================================== */
/* Symbol table -- the IR                                                  */
/* ====================================================================== */

enum {
    U_DISPLAY, U_BINARY, U_PACKED, U_COMP5,
    U_SINT, U_UINT, U_SSHORT, U_USHORT, U_BCHAR, U_UBCHAR, U_POINTER, U_INDEX,
    U_NATIONAL,                     /* numeric and numeric-edited USAGE NATIONAL (cobol ISSUES-72); PIC N keeps U_DISPLAY */
    U_BIT,                          /* boolean USAGE BIT: bits, packed (cobol ISSUES-78) */
    U_SDBL, U_UDBL,                 /* BINARY-DOUBLE [UNSIGNED]: eight bytes, 19 (20) digits -- the wide path (docs/wide.md) */
    U_FLOAT                         /* COMP-1 (no PICTURE), COMP-2, FLOAT-SHORT/-LONG: IEEE, size 4 or 8 (docs/usage.md) */
};

static const char *usage_name(int u)
{
    static const char *n[] = { "display", "comp", "comp-3", "comp-5", "signed-int",
        "unsigned-int", "signed-short", "unsigned-short", "binary-char",
        "binary-char unsigned", "pointer", "index", "national", "bit", "binary-double", "binary-double unsigned", "float" };
    return n[u];
}

enum { UV_NONE, UV_COMPX, UV_NOSIGN,     /* Sym.uvar (docs/usage.md) */
       UV_COMP1,                        /* COMP-1, until sym_finish sees whether a PICTURE came: RM's binary, or a float */
       UV_FSHORT, UV_FLONG };           /* U_FLOAT: four bytes or eight */
static int g_comp1 = -1;                /* -fcomp1=binary (1) | float (0); -1: by its PICTURE */

static int usage_is_native(int u)
{
    return u == U_SINT || u == U_UINT || u == U_SSHORT || u == U_USHORT ||
           u == U_BCHAR || u == U_UBCHAR || u == U_POINTER || u == U_INDEX || u == U_SDBL || u == U_UDBL;
}

#define MAXDIM 7
#define MAXCV 32

typedef struct Sym {
    char name[64];
    int  level, line, is_filler;
    int  parent, child, sibling;    /* tree, as indices; -1 = none */
    int  record;                    /* the 01/77 (or index) owning the storage */
    int  usage, has_usage, has_pic;
    int  uvar;                          /* UV_*: a usage's variant -- COMP-X (U_COMP5), unsigned COMP-6 (U_PACKED) */
    int  compx_x;                       /* COMP-X described with a PICTURE of X's: no digit limit */
    char pic[PIC_MAXPAT];
    PicInfo pi;
    int  is_group, is_cond, is_index;
    int  type_lvl1;                 /* expanded from a group TYPE: aligned as a level 1 item (13.18.57.4 rule 2d) */
    int  size;                      /* one occurrence */
    int  offset;                    /* from the start of the record */
    int  occurs;                    /* 0 = no OCCURS; with DEPENDING ON, the maximum */
    int  nokey; char okey[8][64]; unsigned char okey_desc[8];   /* OCCURS ASCENDING/DESCENDING KEY: names in order, 1 = descending (SEARCH ALL) */
    int  odo_min; char odo_dep[64]; struct Sym *odo_dep_sym;   /* OCCURS m TO n DEPENDING ON */
    int  idx1;                      /* the table's first INDEXED BY item, or -1 */
    int  ix_table;                  /* an index item: the table it indexes */
    int  lin_file;                  /* LINAGE-COUNTER of file lin_file (a cell in its cob_file), -1 otherwise */
    int  rep_ctr;                   /* LINE-COUNTER / PAGE-COUNTER of report rep_ctr (a cell in its cob_report), -1 otherwise */
    int  redefines;                 /* sym index, -1 */
    int  redef_clause;              /* ... from a REDEFINES clause, not a file's records sharing storage */
    int  sync, just, blank_zero;
    int  sign_lead, sign_sep;        /* SIGN IS LEADING/TRAILING [SEPARATE] */
    int  ndims, dim_count[MAXDIM], dim_stride[MAXDIM];
    /* VALUE (elementary or group) */
    Tok *value_tok; int value_all, value_fig;
    /* level 88 */
    int  ncv; Tok *cv_lo[MAXCV], *cv_hi[MAXCV];
    Tok *cv_false;                  /* level 88 ... [WHEN SET TO] FALSE IS literal-4 (COBOL 2002) */
    unsigned cv_all;                 /* bit i: value i is ALL literal */
    int  fd;                        /* file index for an 01 under an FD, else -1 */
    int  is_linkage;                /* a LINKAGE SECTION record: storage is the caller's */
    int  is_based;                  /* a BASED entry: reached through a cell SET ADDRESS OF fills, NULL at first (2002 8.6.4) */
    int  param_opt;                 /* a PROCEDURE DIVISION USING OPTIONAL parameter: its cell may be NULL (omitted) */
    int  any_len;                   /* ANY LENGTH (2002; 2023 13.18.2): its size the argument's, in a writable descriptor */
    int  split_key;                 /* a Micro Focus split key (BP-D2): its slot in the record area's tail */
    int  is_rc;                     /* RETURN-CODE: storage in libcob (cob_return_code), none of the unit's */
    int  is_local;                  /* a LOCAL-STORAGE record: storage is the activation's (COBOL 2002) */
    int  is_ftemp;                  /* a user function's result or BY CONTENT argument, made by the compiler */
    int  nat_usage;                 /* USAGE NATIONAL was written (cobol ISSUES-62) */
    int  natgroup;                  /* GROUP-USAGE NATIONAL, written or inherited: a national group (cobol ISSUES-71) */
    int  bitgroup;                  /* GROUP-USAGE BIT, written (2) or inherited (1): a bit group (cobol ISSUES-78) */
    int  bits, bitoff;              /* USAGE BIT and bit groups: boolean positions, and the first bit's place in the first byte */
    int  strong;                    /* a strongly-typed group: its type key + 1 (cobol ISSUES-80) */
    int  standin;                   /* the FILLER PIC X put in place of an entry refused with an error */
    int  in_natgroup;               /* an elementary item of a national group */
    int  ftemp_scan;                /* ... made while scanning ahead (no code): must never be emitted */
    int  is_global;                 /* GLOBAL (or under a GLOBAL item / a GLOBAL FD): contained programs see it */
    int  is_external;               /* EXTERNAL record (or a record of an EXTERNAL FD): storage shared by name, through a cell */
    int  is_rename;                 /* level 66: another name for a range of the record, resolved after layout */
    char rn_a[64], rn_b[64]; char rn_aq[8][64], rn_bq[8][64]; int rn_naq, rn_nbq;
    /* records */
    unsigned char *image; int image_size;
    char label[48];
    /* descriptor */
    int  desc_id;                   /* -1 until emitted */
} Sym;

/* COMP, BINARY (and RM's COMP-1) are big-endian, as IBM, Micro Focus and
 * GnuCOBOL store them, so a record written by any of them reads here
 * (docs/usage.md); -fbinary-byteorder=native keeps SLOW-32's little-endian
 * order.  COMP-5, the native usages and RETURN-CODE (a C int the run unit
 * shares) are always the machine's order. */
static int g_bin_native;
static int sym_be(const Sym *s) { return (s->usage == U_BINARY && !s->is_rc && !g_bin_native) || s->uvar == UV_COMPX; }   /* COMP-X always (MF) */
static int sym_in_strong(const Sym *s);
static Sym *odo_table_for(Sym *s);
static void value_rules(void);
static void occurs_rules(void);
static Sym *odo_table_below(Sym *s);

static Sym *g_sym;
static int g_nsym, g_scap;

/* Program units share one symbol, file and paragraph table; a unit's own
 * entries begin at the bases.  A contained program (COBOL 85 nesting)
 * pushes the containing unit's state on g_ustack and starts its bases at
 * the current ends; on END PROGRAM the tables are cut back and the
 * containing unit resumes.  Name lookup falls through the stack to the
 * ancestors' GLOBAL items and files. */
static int g_sym_base, g_file_base, g_para_base;
static int g_unit_counter;          /* units so far in this source file, for label spaces */
static int g_unit_parent1[4096];    /* a contained unit's container + 1; 0 at the top */
typedef struct UnitSave UnitSave;
static UnitSave *g_ustack[8]; static int g_udepth;

/* The symbol table never moves.  Sym pointers live the whole compile --
 * in every Ref a statement holds while it parses, in odo_dep_sym, in a
 * file's keys -- and records are still made in the PROCEDURE DIVISION
 * (ftemp_new: a function's result, a BY CONTENT copy, a MOVE's sender).
 * A realloc under them was a use-after-free (a COMPUTE whose function
 * call grew the table, cobol ISSUES-96).  So the address space is
 * reserved once and committed as the table grows; untouched pages cost
 * nothing. */
enum { SYM_RESERVE = 1 << 20 };
static Sym *sym_new(void)
{
    if (g_nsym == g_scap) {
        if (!g_sym) {
            int fl = MAP_PRIVATE | MAP_ANON;
#ifdef MAP_NORESERVE
            fl |= MAP_NORESERVE;
#endif
            void *m = mmap(NULL, (size_t)SYM_RESERVE * sizeof *g_sym, PROT_NONE, fl, -1, 0);
            if (m == MAP_FAILED) { fprintf(stderr, "s32-cobc: cannot reserve the symbol table\n"); exit(1); }
            g_sym = m;
        }
        if (g_scap == SYM_RESERVE) { fprintf(stderr, "s32-cobc: more than %d data items\n", SYM_RESERVE); exit(1); }
        int n = g_scap ? 2 * g_scap : 128;
        if (n > SYM_RESERVE) n = SYM_RESERVE;
        if (mprotect(g_sym, (size_t)n * sizeof *g_sym, PROT_READ | PROT_WRITE)) { fprintf(stderr, "s32-cobc: cannot grow the symbol table\n"); exit(1); }
        g_scap = n;
    }
    Sym *s = &g_sym[g_nsym++];
    memset(s, 0, sizeof *s);
    s->parent = s->child = s->sibling = s->redefines = -1;
    s->desc_id = -1; s->fd = -1; s->idx1 = -1; s->ix_table = -1; s->lin_file = -1; s->rep_ctr = -1;
    return s;
}

static int sym_idx(Sym *s) { return (int)(s - g_sym); }
/* a record reached through a cell holding its address, not by its label:
 * LINKAGE (the caller's), LOCAL-STORAGE (the activation's), EXTERNAL */
static int rec_indirect(const Sym *rec) { return rec->is_linkage || rec->is_local || rec->is_external || rec->is_based; }
static const char *indirect_kind(const Sym *rec) { return rec->is_based ? "BASED" : rec->is_linkage ? "LINKAGE" : rec->is_local ? "LOCAL-STORAGE" : "EXTERNAL"; }

/* name [OF|IN qualifier]...: the unique item that matches */
static void unit_range(int level, int *from, int *to);   /* an ancestor's symbol range */
static const char *file_name_of(int fd);                /* the FD's file-name (File is declared below) */

static char g_poison[64][64]; static int g_npoison;   /* names whose uses fail quietly: data entries dropped after an error, a refused module's registers */

static int g_lk_check;              /* -std=85, in a PROCEDURE DIVISION after its USING: references to LINKAGE are checked */
static int g_lk_using[32], g_lk_nusing;   /* that division's USING records, by index */
static int g_in_proc;               /* (defined with the PROCEDURE DIVISION's state) */
static void lk_reference(const Sym *s, int line);
static Sym *sym_lookup(const char *name, char **quals, int nq, int line)
{
    Sym *found = NULL; int nfound = 0;
    int from = g_sym_base, to = g_nsym, global_only = 0;
    for (int level = g_udepth; level >= 0 && !nfound; level--) {
        if (level < g_udepth) { unit_range(level, &from, &to); global_only = 1; }
        for (int i = from; i < to; i++) {
            Sym *s = &g_sym[i];
            if (s->is_filler || strcmp(s->name, name)) continue;
            if (global_only && !s->is_global) continue;
            int ok = 1, at = i;
            for (int q = 0; q < nq && ok; q++) {
                int hit = -1;
                for (int p = g_sym[at].parent; p >= 0; p = g_sym[p].parent)
                    if (!strcmp(g_sym[p].name, quals[q])) { hit = p; break; }
                if (hit < 0) {
                    /* the outermost qualifier may be the file whose FD holds the record (SQ207M: PRINT-REC IN PRINT-FILE) */
                    int r = at; while (g_sym[r].parent >= 0) r = g_sym[r].parent;
                    if (g_sym[r].fd >= 0 && !strcmp(file_name_of(g_sym[r].fd), quals[q]) && q == nq - 1) hit = r;
                }
                if (hit < 0) ok = 0; else at = hit;
            }
            if (ok) { found = s; nfound++; }
        }
    }
    if (!nfound) {
        /* a data entry dropped after an error: its uses fail quietly,
         * rather than once each */
        for (int i = 0; i < g_npoison; i++)
            if (!strcmp(g_poison[i], name) && g_recover) longjmp(*g_recover, 1);
        if (nq) die_at(line, "'%s' is not declared under '%s'", name, quals[0]);
        die_at(line, "'%s' is not declared", name);
    }
    if (nfound > 1) die_at(line, "'%s' is ambiguous; qualify it with OF/IN", name);
    if (g_lk_check && g_in_proc && found->is_linkage) lk_reference(found, line);
    return found;
}

/* X3.23-1985 X-25 and X-26, procedure division header rule 4: an item
 * of the LINKAGE SECTION is referenced only as, or under, a USING
 * operand, or a REDEFINES or RENAMES of one (and its 88s and indexes).
 * Under -std=2002 SET ADDRESS OF gives such an item its storage, so the
 * rule is left to 2002's own reading (docs/conformance/data-division.md). */
static void lk_reference(const Sym *s, int line)
{
    int r = sym_idx((Sym *)s);
    while (g_sym[r].parent >= 0) r = g_sym[r].parent;
    if (g_sym[r].level == 66) return;
    for (int k = 0; k < g_lk_nusing; k++)
        if (r == g_lk_using[k] || g_sym[r].redefines == g_lk_using[k]) return;
    die_at(line, "'%s' is in the LINKAGE SECTION but not a USING operand, nor under or redefining one (X3.23-1985 X-25, procedure division header rule 4)",
           s->name);
}

static Sym *sym_lookup_quiet(const char *name)
{
    for (int i = g_sym_base; i < g_nsym; i++)
        if (!g_sym[i].is_filler && !strcmp(g_sym[i].name, name)) return &g_sym[i];
    for (int level = g_udepth - 1; level >= 0; level--) {
        int from, to; unit_range(level, &from, &to);
        for (int i = from; i < to; i++)
            if (g_sym[i].is_global && !g_sym[i].is_filler && !strcmp(g_sym[i].name, name)) return &g_sym[i];
    }
    return NULL;
}

static char g_progid[64], g_progid_orig[64];    /* the program-name, and as written */

/* ---- files: SELECT + FD ------------------------------------------------ */

typedef struct {
    char name[64], oname[64];        /* the file-name, and as written (EXCEPTION-FILE) */
    int  line, org, access, optional;
    Tok *assign_lit;                 /* ASSIGN TO literal ... */
    char assign_name[64];            /* ... or to a data-name */
    char status_name[64], key_name[64], report_name[64];
    char (*report_more)[64]; int nreport_more;    /* REPORTS ARE: the names after the first */
    char status_qual[64];            /* FILE STATUS name OF group */
    char relkey_name[64];            /* RELATIVE KEY IS data-name */
    char key_qual[64];               /* RECORD KEY IS name IN group */
    int  linage;                     /* FD LINAGE: lin_lit/lin_name for LINES, FOOTING, TOP, BOTTOM */
    long lin_lit[4]; char lin_name[4][64]; Sym *lin_sym[4];
    int  lin_counter_sym;            /* the LINAGE-COUNTER item, or -1 */
    struct { char name[64]; char qual[64]; Sym *sym; int dups; char (*split)[64]; int nsplit, split_mf; } alt[16]; int nalt;   /* ALTERNATE RECORD KEY ... [WITH DUPLICATES] */
    char (*ksplit)[64]; int nksplit, ksplit_mf;   /* RECORD KEY IS record-key-name SOURCE IS data-name ... (2002), or = (Micro Focus, BP-D2) */
    int *splitw; int nsplitw;        /* the runtime's split-key table (cob_file.split) */
    Sym *assign_sym, *status_sym, *key_sym, *relkey_sym;
    int  use_para;                   /* DECLARATIVES: the USE section for this file, 0 none */
    int  rec;                        /* sym index of the first 01, -1 */
    int  recsize;
    int  org_given;                  /* an ORGANIZATION clause was written */
    int  varying;                    /* mode V: RECORDING MODE V, RECORD CONTAINS m TO n, VARYING, unequal 01s */
    int  minlen, maxlen;             /* from RECORD CONTAINS / VARYING; 0 = unset */
    char dep_name[64]; Sym *dep_sym; /* RECORD IS VARYING ... DEPENDING ON */
    int  unit;                       /* the program unit that declares it (its image is .Lf<unit>_<index>) */
    int  global;                     /* FD ... GLOBAL: contained programs may use it */
    int  external;                   /* FD ... EXTERNAL: one file connector for every program naming it */
    int  codeset;                    /* FD CODE-SET: 1 + the alphabet index of a non-native code set, 0 native */
    int  fd_line;                    /* the line of its FD (or SD) entry, 0 before one is seen */
    int  block_given, rc_given, rc_varying_from, reserve_given;   /* BLOCK CONTAINS, RECORD CONTAINS written; RECORD VARYING FROM written */
    char data_rec[8][64]; int ndata_rec;           /* DATA RECORDS names (85 3.5), checked against the 01s */
    int  codeset_line;
} File;

static File *g_files; static int g_nfile, g_fcap;
static const char *file_name_of(int fd) { return g_files[fd].name; }
static int g_cur_fd = -1;            /* the FD whose 01s are being parsed */

static void unit_file_range(int level, int *from, int *to);

static File *file_find(const char *name)
{
    for (int i = g_file_base; i < g_nfile; i++) if (!strcmp(g_files[i].name, name)) return &g_files[i];
    for (int level = g_udepth - 1; level >= 0; level--) {
        int from, to; unit_file_range(level, &from, &to);
        for (int i = from; i < to; i++) if (g_files[i].global && !strcmp(g_files[i].name, name)) return &g_files[i];
    }
    return NULL;
}

/* ---- reports: RD and its groups --------------------------------------- */

typedef struct {
    int column, line;
    int has_pic; char pic[PIC_MAXPAT]; PicInfo pi;
    int has_source; int source_tp;      /* token position of the SOURCE reference, parsed at the first GENERATE */
    struct Ref_ *source;                /* ... and kept, parsed */
    Tok *value;
    int just, blank_zero;
    int usage_nat;                      /* USAGE NATIONAL on a numeric or numeric-edited PICTURE */
    char ename[64];                     /* the entry's data-name (a SUM entry's names its counter) */
    int gi;                             /* GROUP INDICATE */
    int has_sum, nsum, sum_tp[8];       /* SUM operands: token positions, resolved when the report is first used */
    int nupon, upon_tp[4];              /* UPON detail-names */
    int reset_tp, reset_final;          /* RESET ON: a control's token position, or FINAL */
    int ctr_sym;                        /* the sum counter item (0 = none) */
    int sum_sym[8], sum_is_ctr[8];      /* resolved operands */
    int upon_g[4];                      /* resolved UPON detail groups (indexes into r->g) */
    int reset_lvl;                      /* resolved: 0 FINAL, 1..nctl; the own CF's level by default */
    int sign_lead, sign_sep;            /* SIGN IS LEADING/TRAILING SEPARATE */
} RField;

typedef struct {
    int abs, plus, line, np;            /* np: LINE ... NEXT PAGE */
    RField *f; int nf, fcap;
} RLine;

enum { RG_PAGE_HEADING, RG_DETAIL, RG_PAGE_FOOTING,
       RG_REPORT_HEADING, RG_REPORT_FOOTING, RG_CONTROL_HEADING, RG_CONTROL_FOOTING };

typedef struct {
    char name[64];
    int type, line;
    int ctl_tp;                         /* CH/CF: the control reference's token position (0: FINAL) */
    int ctl_level;                      /* resolved: 0 FINAL, 1 most major .. nctl; -1 not a control group */
    int next_kind, next_n;              /* NEXT GROUP: 0 none, 1 integer, 2 PLUS, 3 NEXT PAGE */
    int use_sec;                        /* USE BEFORE REPORTING: the DECLARATIVES section, -1 none */
    RLine *l; int nl, lcap;
} RGroup;

typedef struct {
    char name[64];
    int line, file;                  /* the FD whose REPORT IS names it */
    int page_limit, heading, first_detail, last_detail, footing;
    int lc_sym, pc_sym;              /* the synthetic LINE-COUNTER and PAGE-COUNTER items */
    int nctl, ctl_final;             /* CONTROL: levels 1 (most major) .. nctl; FINAL besides */
    int ctl_sym[8], ctl_clone[8];    /* each control item and its prior-value clone (X3.23 VIII 2.21.4(13)) */
    int ctl_held[8];                 /* where the new value waits while the item holds the prior one */
    int resolved;                    /* SUM operands, UPON, RESET, CH/CF levels resolved at first use */
    RGroup *g; int ng, gcap;
    Tok *code_lit; int code_tp;      /* CODE: the literal, or the identifier's token position (0 none) */
    struct Ref_ *code_ref;           /* ... the identifier, parsed at first use and kept */
} Report;

/* the report state block's cells past the ones with symbols (cobrt.h) */
#define RW_OFF_FIRST_GEN 40
#define RW_OFF_BRK       44
#define RW_OFF_SUPPRESS  56
#define RW_OFF_GI        60

static Report *g_reports; static int g_nreport, g_rcap;
static int g_report_base;           /* the unit's first report: a contained program's follow its container's (ISSUES-94) */

/* ---- screens: SCREEN SECTION 01s as slot tables ------------------------ */

typedef struct {
    int kind, flags, line, col, width, srcline, fg, bg;
    Tok *value;
    int has_pic; char pic[PIC_MAXPAT]; PicInfo pi; int blank_zero;
    Sym *item;
    int ref_tp, dyn;            /* the reference's token position; dyn: its address is computed at ACCEPT/DISPLAY */
    struct Ref_ *ref;           /* ... the reference, parsed: at the statement, or at first use (sfield_resolve) */
    long stat_off;              /* static references (literal subscripts included): the resolved offset */
int ext, prompt;            /* positioned DISPLAY/ACCEPT: COB_SX_* bits, the PROMPT character */
int natlit;                 /* a VALUE slot's literal is national: its columns are its display width */
struct Ref_ *line_r, *col_r, *at_r;   /* LINE / POSITION / AT given as identifiers, stored at run time */
int idesc;                  /* the item a reference-modified part: its descriptor + 1 (the part's, not the item's) */
int from_lit;               /* FROM literal-1: a VALUE slot that must have its PICTURE */
} SField;

typedef struct { char name[64]; int first, count; } SGroup;   /* a named nested group: a window into the slot table */

typedef struct {
    char name[64];
    int line, blank_screen;
    SField *f; int nf, fcap;
    SGroup *sub; int nsub, subcap;
} Screen;

static Screen *g_screens; static int g_nscreen, g_scrcap;
static int g_screen_base;   /* the unit's screens start here (contained programs append) */

static Screen *screen_find(const char *name)
{
    for (int i = g_screen_base; i < g_nscreen; i++) if (!strcmp(g_screens[i].name, name)) return &g_screens[i];
    return NULL;
}

/* the record label for a screen-name or a named group inside one; the
 * window's slot range comes back for the dynamic-address fill */
static Screen *screen_ref(const char *name, char *lab, size_t n, int *first, int *count)
{
    Screen *sc = screen_find(name);
    if (sc) { snprintf(lab, n, ".Lscr%d_%d", g_unit, (int)(sc - g_screens)); *first = 0; *count = sc->nf; return sc; }
    for (int i = g_screen_base; i < g_nscreen; i++)
        for (int j = 0; j < g_screens[i].nsub; j++)
            if (!strcmp(g_screens[i].sub[j].name, name)) {
                snprintf(lab, n, ".Lscrg%d_%d_%d", g_unit, i, j);
                *first = g_screens[i].sub[j].first; *count = g_screens[i].sub[j].count;
                return &g_screens[i];
            }
    return NULL;
}

static void emit_screen_dyn_fill(Screen *sc, int first, int count);   /* below, after parse_ref exists */

static Report *report_find(const char *name)
{
    for (int i = g_report_base; i < g_nreport; i++) if (!strcmp(g_reports[i].name, name)) return &g_reports[i];
    return NULL;
}

static File *file_of_record(Sym *s, int line)
{
    if (s->level != 1 || s->fd < 0) die_at(line, "'%s' is not a record of a file", s->name);
    return &g_files[s->fd];
}

/* ====================================================================== */
