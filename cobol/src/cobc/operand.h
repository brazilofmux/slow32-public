/* s32-cobc: operands and addresses.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ====================================================================== */
/* Operands and addresses                                                  */
/* ====================================================================== */

/* an arithmetic expression, parsed once (docs/plans/frontend-pass.md):
 * a leaf operand, or an operator over its subtrees.  parse_expr builds
 * one whether or not it emits; emit_expr walks it, where the code is
 * wanted.  Shared, never changed once built: a leaf is copied before
 * anything is done to it. */
typedef struct Expr {
    char op;                    /* 0 a leaf; + - * / and '^' (a power); 'n' negation */
    struct Expr *l, *r;
    struct Opnd_ *o;            /* a leaf's operand, as the scan parsed it */
    int tp;                     /* a leaf's first token */
    int wide, flt;              /* scan_expr's: an operand past 18 digits, a float, inside it */
} Expr;

typedef struct Ref_ {
    Sym *sym;
    int nsub;
    struct { Sym *sym; long lit; long adj; Expr *x; } sub[MAXDIM];   /* sym == NULL: literal; &g_subx: the expression x */
    int line;
    int rm;                         /* reference modification item(start:len) */
    long rm_start, rm_len;          /* literal values, or 0 when an expression / omitted */
    int rm_nat;                     /* a national item's: start and length count characters, two bytes each */
    int rm_bit;                     /* a USAGE BIT item's or bit group's: they count bits (cobol ISSUES-82) */
    int bitsub;                     /* a bit array's element: 1 + the subscript that picks it, as a bit position (cobol ISSUES-84) */
    long bitu_start;                /* ... and the start within the element: 1 without a reference modification, 0 computed (cobol ISSUES-93) */
    int user_rm;                    /* the program wrote a reference modification (rm is also set for a bit-array element) */
    Expr *rm_sx, *rm_lx;            /* the start and length when expressions (rm_lx NULL: no length expression) */
    int rm_odo; Sym *odo_dep; int odo_base, odo_elem;   /* a whole group over an ODO table, sent at its current length */
    int lw_lenx, lw_startx;         /* lower.h: a computed length's, a computed start's node + 1 when an island takes it, else 0 */
} Ref;
static void emit_refmod_check(const Ref *r, long len, int slot);
/* the stand-in subscript symbol of an arithmetic-expression subscript
 * (2002 8.4.1.2): a group, so nothing takes it for an integer item */
static Sym g_subx = { .is_group = 1, .record = -1, .redefines = -1, .parent = -1, .name = "(an expression)" };

static Expr *parse_expr(void);
static Expr *scan_expr(void);
static void ucall_make(struct Opnd_ *o);
static void expr_calls(Expr *e);
static void emit_expr(Expr *e);
static void emit_ucalls(int from, int to);
static int g_nucall;                    /* user-function calls recorded (cobol ISSUES-50) */
static const char *g_ufn_forbid;        /* where a user function may not appear yet, or NULL */
static int ec_size_on(void);
static void emit_ec_size(void);
static int ec_on_name(const char *name);
struct Sym;
static struct Sym *odo_table_for(struct Sym *s);
static void emit_ec_raise(int i);
static void emit_ec_query(const char *name, const char *fn, int want);
static void emit_report_addr(const char *reg, Report *r);
static int ec_find(const char *w, int line);
static char g_cur_stmt[16];              /* the statement being compiled, for EXCEPTION-STATEMENT */
static const Tok *g_stmt_tok;            /* its first token: the line EXCEPTION-LOCATION names */
static const char *g_ec_file;            /* EC-I-O being raised: the file-name as written, for EXCEPTION-FILE */
static int g_ec_fidx = -1;               /* ... and its file index, for a TURN WITH LOCATION for that file */
static int g_recursive, g_std, g_cond_depth;   /* defined below */
static int g_fnsig_only;                /* -fnsig: write the functions' .s32fn files, compile nothing */
static void skip_unit_body(void);
static int g_is_function;           /* FUNCTION-ID: a user-defined function (COBOL 2002; always recursive) */
static int g_main_done;             /* the executable's main program has been emitted */
static Sym *g_returning;            /* the function's RETURNING item */

/* A user-defined function's signature (docs/functions.md, Stage B): its
 * RETURNING item and parameters, as descriptions a caller can rebuild.
 * Known from a definition earlier in the source, or from the external
 * repository -- a name.s32fn file the function's own compile wrote. */
typedef struct { int group, size, usage, has_pic, just, bwz, sign_lead, sign_sep; char pic[PIC_MAXPAT]; } FDesc;
typedef struct { char name[64], link[128]; int nparam; FDesc param[8], ret; } FnSig;
static FnSig g_fnsig[128]; static int g_nfnsig;
/* the unit's REPOSITORY: functions named there are invoked without FUNCTION */
static char g_repo_fn[32][64]; static int g_nrepo_fn;
static int g_repo_all_intrinsic;    /* FUNCTION ALL INTRINSIC */

enum { O_REF, O_STR, O_NUM, O_FIG, O_ALL, O_EXPR, O_FUNC, O_BEXPR, O_ADDR };   /* O_ADDR: ADDRESS OF ref, a data-address identifier */   /* O_BEXPR: a boolean expression, bx, fsize its widest operand */

/* a boolean expression (2023 8.8.2), parsed once: an operand, or an
 * operator over one operand (B-NOT; a shift, with its count) or two */
typedef struct BExpr {
    int op;                     /* 0 an operand; BO_AND ... BO_SRC */
    struct BExpr *l, *r;
    struct Opnd_ *o;            /* an operand; a shift's count */
} BExpr;

/* the trees' nodes: never freed, the compiler being a run that ends */
static void *ex_alloc(size_t n)
{
    static char *p; static size_t left;
    n = (n + 15) & ~(size_t)15;
    if (n > left) { left = n > 65536 ? n : 65536; p = xmalloc(left); }
    void *q = p; p += n; left -= n;
    memset(q, 0, n);
    return q;
}

typedef struct Opnd_ {
    int kind;
    int wide;           /* O_EXPR: an operand past 18 digits inside (docs/wide.md) */
    int folded;         /* O_NUM: a function or LENGTH OF the compiler evaluated -- written as a reference, not a literal */
    int flt;            /* O_EXPR: a floating-point item inside (docs/usage.md) */
    Ref ref;
    Tok *tok;           /* O_STR / O_FIG / O_ALL's literal */
    NumLit num;         /* O_NUM */
    int line;
    Expr *ex;           /* O_EXPR: the expression */
    BExpr *bx;          /* O_BEXPR: the expression */
    int fn; struct Opnd_ *farg, *farg2; int fsize;   /* O_FUNC: intrinsic, its argument(s), result width */
    int ffull, frm;                          /* O_FUNC reference-modified: the width evaluated, the offset taken */
    int fvar, fnat, fbool;                   /* O_FUNC: length known only at run time (fsize its maximum); a national, a boolean result */
    Tok *ftrim;                              /* FN_TRIM: the characters to delete, NULL for a space */
    int fwasvar;                             /* O_FUNC: a fixed part cut from a run-time-length result (cobol ISSUES-88) */
    Expr *fsx, *flx; int flen;               /* O_FUNC reference-modified at computed positions: the start and length
                                              * (flx NULL: flen, or to the end when 0) (cobol ISSUES-91) */
    int fnid, fkind, fscale;                 /* O_FUNC, 1989 amendment: cob_fn id, argument shape, result scale */
    struct Opnd_ **fargs; int nfargs;        /* its argument list (an ALL-subscript table arg has all_sub set) */
    unsigned all_sub;                        /* O_REF: the subscript positions written ALL (bit k: subscript k+1) -- every element, looped over at emission */
    int fsaved;                              /* O_FUNC evaluated already: 1 + the label of its result's copy (MOVE, general rule 1) */
    int fwnum;                               /* O_FUNC: an exact numeric function on the wide stack, its result described at run time (docs/wide.md) */
    const char *fname;                       /* O_FUNC: the intrinsic's name, for messages */
    Sym *fkept;                              /* O_FUNC evaluated already: the record holding its result and its
                                              * run-time length (an EVALUATE subject; cob_fn_keep, cob_fn_kept) */
    Sym *nsave;                              /* O_EXPR evaluated already: the record holding its value (an EVALUATE
                                              * subject, evaluated once; cob_nsave, cob_npush_saved) */
    struct UCall_ *uc;                       /* a user function's result met while scanning ahead: the call,
                                              * made when an expression holding it is emitted (ucall_make) */
} Opnd;
typedef int (*SymVisit)(const Sym *s, const void *cx);
static int expr_names(const Expr *e, SymVisit f, const void *cx);
static int opnd_names(const Opnd *o, SymVisit f, const void *cx);
static int opnds_wide(const Opnd *ops, int n);
static int refs_wide(const Ref *rs, int nr);
static int opnd_is_national(const Opnd *o);
static int opnd_is_boolean(const Opnd *o);
static int ref_is_national(const Ref *r);
static int ref_static_len(const Ref *r);
static void nat_fig_opnd(Opnd *o, int nbytes);
static void emit_incompat(const Opnd *o);
static int g_incompat_push;         /* test each operand as an expression or a function pushes it */
static void emit_incompat_sym(Sym *s, int line);

/* national: an elementary PIC N item, or a national group, which is
 * treated as one (2023 13.18.29.4 rule 2b) */
static int sym_is_national(const Sym *s) { return s->natgroup || (!s->is_group && s->pi.category == PIC_NATIONAL); }
static int sym_is_boolean(const Sym *s) { return s->bitgroup || (!s->is_group && s->pi.category == PIC_BOOLEAN); }
/* does a group hold a boolean item anywhere below it */
static int sym_strong_has_boolean(Sym *g)
{
    for (int c = g->child; c >= 0; c = g_sym[c].sibling) {
        Sym *k = &g_sym[c];
        if (sym_is_boolean(k) || (k->is_group && sym_strong_has_boolean(k))) return 1;
    }
    return 0;
}
/* a strong group's elementary items, in order, as (offset in the group,
 * descriptor) words; an OCCURS repeats its entries; returns the count */
static int sym_desc(Sym *s);
static int strong_table_at(Sym *g, Sym *s, int base)
{
    int n = 0, times = s->occurs ? s->occurs : 1;
    for (int k = 0; k < times; k++) {
        int b = base + k * s->size;
        if (!s->is_group) { emit("\t.word %d, .Ld%d", b + s->offset - g->offset, sym_desc(s)); n++; continue; }
        for (int c = s->child; c >= 0; c = g_sym[c].sibling)
            if (!g_sym[c].is_cond && !g_sym[c].is_rename && g_sym[c].redefines < 0) n += strong_table_at(g, &g_sym[c], b);
    }
    return n;
}
static int strong_table(Sym *g, int base)
{
    int n = 0;
    for (int c = g->child; c >= 0; c = g_sym[c].sibling)
        if (!g_sym[c].is_cond && !g_sym[c].is_rename && g_sym[c].redefines < 0) n += strong_table_at(g, &g_sym[c], base);
    return n;
}

/* the descriptor of a reference-modified part with literal positions
 * (2023 8.4.3.3.4 rule 6): national for a national item or a numeric
 * USAGE NATIONAL one, boolean (in the item's usage) for a boolean one,
 * alphanumeric otherwise */
static int bool_desc(int len);
/* a bit array as one boolean item of all its bits, the base its
 * elements are reference-modified out of at run time */
static int bitarray_desc(Sym *s)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_BOOLEAN; d.usage = COB_U_BIT; d.size = bit_total(s); d.scale = (signed char)s->bitoff;
    return desc_add(&d);
}

static int part_desc(const Ref *r)
{
    Sym *s = r->sym; int len = (int)r->rm_len;
    if (r->rm_bit) {
        Desc d; memset(&d, 0, sizeof d);
        d.cat = COB_BOOLEAN; d.usage = COB_U_BIT; d.size = len; d.scale = (signed char)((s->bitoff + r->rm_start - 1) % 8);
        return desc_add(&d);
    }
    if (sym_is_boolean(s)) {
        if (!r->rm_nat) return bool_desc(len);
        Desc d; memset(&d, 0, sizeof d);
        d.cat = COB_BOOLEAN; d.usage = COB_U_NATIONAL; d.size = 2 * len;
        return desc_add(&d);
    }
    return r->rm_nat ? nat_desc(2 * len) : str_desc(len);
}

/* in a strongly-typed group: the group itself or anything under one */
static int sym_in_strong(const Sym *s)
{
    for (; ; s = &g_sym[s->parent]) { if (s->strong) return 1; if (s->parent < 0) return 0; }
}

static int is_int_item(Sym *s)
{
    return is_numeric_sym(s) && s->pi.scale == 0;
}

/* COMP-5 and the C-ABI types keep the binary field's capacity rather than
 * the picture's digit count (COB_F_NOTRUNC in the descriptor) */
static int sym_notrunc(Sym *s) { return s->usage == U_COMP5 || usage_is_native(s->usage); }

/* a "hot" integer: binary, at most 4 bytes, no scale */
static int is_hot_int(Sym *s)
{
    if (s->is_group || s->pi.category != PIC_NUMERIC || s->pi.scale != 0) return 0;
    if (s->usage == U_DISPLAY || s->usage == U_PACKED || s->usage == U_NATIONAL || s->usage == U_FLOAT) return 0;
    return s->size == 1 || s->size == 2 || s->size == 4;    /* not COMP-X's three bytes */
}

/* is the subscript at the cursor more than COBOL 85's forms -- integer,
 * data-name or index-name, data-name +/- integer -- can say: does an
 * arithmetic expression from here run further than they would, or is
 * the data-name itself subscripted (X-COBOL's E(9 - I), K(I - N, 1)) */
static int sub_is_expr(void)
{
    int i = g_tp, simple;
    if (g_tok[i].kind == T_NUM) simple = i + 1;
    else if (g_tok[i].kind == T_WORD) {
        /* FUNCTION name ...: a function-identifier is an expression */
        if (!strcmp(g_tok[i].s, "function") && g_tok[i + 1].kind == T_WORD && !sym_lookup_quiet("function")) return 1;
        i++;
        while ((is_word(&g_tok[i], "of") || is_word(&g_tok[i], "in")) && g_tok[i + 1].kind == T_WORD) i += 2;
        if (g_tok[i].kind == T_LP && !g_tok[i].after_comma) return 1;
        if (g_tok[i].kind == T_OP && (!strcmp(g_tok[i].s, "+") || !strcmp(g_tok[i].s, "-")) && g_tok[i + 1].kind == T_NUM) i += 2;
        simple = i;
    } else return g_tok[i].kind == T_LP || (g_tok[i].kind == T_OP && (!strcmp(g_tok[i].s, "+") || !strcmp(g_tok[i].s, "-")));
    /* only an arithmetic operator after the 85 form carries it further;
     * anything else -- ')', the next subscript -- ends it there, and the
     * expression parser is not asked (it refuses an index-name, which
     * is a subscript) */
    Tok *nx = &g_tok[simple];
    if (nx->kind != T_OP || !(!strcmp(nx->s, "+") || !strcmp(nx->s, "-") || !strcmp(nx->s, "*") ||
                              !strcmp(nx->s, "/") || !strcmp(nx->s, "**"))) return 0;
    int save = g_tp;
    g_noemit++; parse_expr(); g_noemit--;
    int end = g_tp; g_tp = save;
    return end > simple;
}

/* identifier [OF|IN qualifier]... [( subscripts )] */
/* a bit array's element, subscripted: its bits are picked out of the
 * array as a reference modification does -- the element's bits, (i - 1)
 * * bits + 1 onward -- whatever builds the Ref (parse_ref, INITIALIZE's
 * walk; cobol ISSUES-84, -94 B2).  A reference modification the program
 * wrote counts bits within the element (8.4.3.3.4 rule 5a), its bounds
 * checked against the element's bits already. */
static void ref_resolve_bits(Ref *r)
{
    if (r->sym->is_group || r->sym->usage != U_BIT || !r->sym->occurs || r->nsub != r->sym->ndims || !r->nsub) return;
    int k = r->nsub - 1;
    if (r->rm) r->bitu_start = r->rm_start;
    else { r->rm = 1; r->rm_len = r->sym->bits; r->rm_lx = NULL; r->bitu_start = 1; }
    r->rm_bit = 1; r->bitsub = r->nsub;
    r->rm_start = !r->sub[k].sym && r->bitu_start ? (r->sub[k].lit - 1) * r->sym->bits + r->bitu_start : 0;
}

/* a bit data item passed BY REFERENCE starts a byte, with only literal
 * subscripts and a literal leftmost position (2023 14.9.4.3 rule 6;
 * cobol ISSUES-94 B6): the callee gets a byte address */
static void bit_arg_check(const Ref *r)
{
    for (int i = 0; i < r->nsub; i++)
        if (r->sub[i].sym) die_at(r->line, "'%s' is a bit data item passed BY REFERENCE: its subscripts must be literals (2023 14.9.4.3 rule 6)", r->sym->name);
    if (r->rm && !r->rm_start) die_at(r->line, "'%s' is a bit data item passed BY REFERENCE: its leftmost position must be a literal (2023 14.9.4.3 rule 6)", r->sym->name);
    long first = r->sym->bitoff + (r->rm ? r->rm_start - 1 : 0);
    if (first % 8) die_at(r->line, "'%s' is a bit data item passed BY REFERENCE and does not start a byte (its bit %ld; 2023 14.9.4.3 rule 6)", r->sym->name, first % 8 + 1);
}

static int g_fn_depth;              /* parsing a function-identifier's arguments (13.18.60.3 rules 8-10) */
static int g_in_proc;               /* in a PROCEDURE DIVISION's statements */
static int g_cond_depth;
/* an index data item is referenced only in SEARCH, SET, a relation
 * condition, a function argument or a USING phrase (2023 13.18.60.3
 * rule 10; X3.23-1985 USAGE syntax rule 5); a pointer only in CALL,
 * INITIALIZE, SET, a relation condition, a function argument or a
 * procedure division header (rules 8-9) */
static void index_ref_check(const Ref *r)
{
    const Sym *x = r->sym;
    if (g_in_proc && x->is_index && !g_cond_depth && !g_fn_depth && g_cur_stmt[0]) {
        /* an index-name: a subscript, PERFORM and SEARCH VARYING, SET, a
         * relation (2023 13.18.38.3 rule 7; X3.23-1985 OCCURS rule 13:
         * an index-name is not data) */
        static const char *in_ok[] = { "SET", "SEARCH", "PERFORM", "EVALUATE", "MOVE", NULL };
        for (int i = 0; in_ok[i]; i++) if (!strcmp(g_cur_stmt, in_ok[i])) return;
        die_at(r->line, "the index-name '%s' is not an operand of %s; SET it, or SET an integer item from it (%s)", x->name, g_cur_stmt,
               g_std < 2002 ? "X3.23-1985 OCCURS syntax rule 13" : "2023 13.18.38.3 rule 7");
    }
    if (!g_in_proc || x->is_group || x->is_index || (x->usage != U_INDEX && x->usage != U_POINTER)) return;
    if (g_cond_depth || g_fn_depth || !g_cur_stmt[0]) return;
    /* MOVE says so itself, pointing at SET (move_invalid) */
    static const char *ix_ok[] = { "SET", "SEARCH", "CALL", "EVALUATE", "MOVE", NULL };
    static const char *pt_ok[] = { "SET", "CALL", "INITIALIZE", "EVALUATE", "MOVE", "ALLOCATE", "FREE", NULL };
    const char *const *ok = x->usage == U_INDEX ? ix_ok : pt_ok;
    for (int i = 0; ok[i]; i++) if (!strcmp(g_cur_stmt, ok[i])) return;
    die_at(r->line, "the %s item '%s' is not an operand of %s (%s)", x->usage == U_INDEX ? "USAGE INDEX" : "USAGE POINTER", x->name, g_cur_stmt,
           x->usage == U_INDEX ? (g_std < 2002 ? "X3.23-1985 USAGE syntax rule 5" : "2023 13.18.60.3 rule 10") : "2023 13.18.60.3 rules 8-9");
}

static void parse_ref_1(Ref *r);
/* an identifier: parsed, and counted by the census */
static void parse_ref(Ref *r)
{
    if (!g_cen_on) { parse_ref_1(r); return; }
    unsigned ctx = g_cen_ctx;
    g_cen_ctx = 0; g_cen_in_ref++;
    parse_ref_1(r);
    g_cen_in_ref--; g_cen_ctx = ctx;
    if (r->user_rm) cen_flag(r->sym, CEN_RM);
    if (!g_in_proc) { cen_flag(r->sym, CEN_DD); if (r->sym >= g_sym && r->sym < g_sym + g_nsym) cen_of(r->sym)->refs++; }
    if (ctx) cen_flag(r->sym, ctx & ~(unsigned)CEN_PLAIN);
    for (int k = 0; k < r->nsub; k++) if (r->sub[k].sym) cen_flag(r->sub[k].sym, CEN_SUB);
}

static void parse_ref_1(Ref *r)
{
    memset(r, 0, sizeof *r);
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a data-name, found %s", tok_desc(t));
    if (!strcmp(t->s, "return-code") && !sym_lookup_quiet("return-code")) {
        /* RETURN-CODE (IBM, Micro Focus): a signed binary word the run unit
         * shares, cob_return_code in libcob; a CALL sets it from what the
         * callee returns, and STOP RUN exits with it */
        bp(BP_E1_RETURN_CODE, t->line);
        Sym *rc = NULL;
        for (int i = g_sym_base; i < g_nsym; i++) if (g_sym[i].is_rc) rc = &g_sym[i];
        if (!rc) {
            rc = sym_new();
            int idx = sym_idx(rc);
            /* PIC S9(9) BINARY: a native signed word, shown as nine digits, as GnuCOBOL declares
             * it (IBM's is S9(4) BINARY) */
            rc->level = 1; rc->line = t->line; rc->is_rc = 1; rc->usage = U_BINARY; rc->has_usage = 1;
            snprintf(rc->name, sizeof rc->name, "return-code");
            rc->has_pic = 1; snprintf(rc->pic, sizeof rc->pic, "s9(9)");
            if (pic_analyse(rc->pic, &rc->pi) < 0) die_at(t->line, "internal: RETURN-CODE picture");
            sym_finish(rc);
            rc->record = idx; rc->desc_id = -1;
            snprintf(rc->label, sizeof rc->label, "cob_return_code");
        }
        r->sym = rc; r->line = t->line; advance();
        return;
    }
    if (!strcmp(t->s, "address") && is_word(peek(1), "of") && !sym_lookup_quiet("address"))
        die_at(t->line, "ADDRESS OF is a sending operand of SET or CALL, or a relation's operand; not here (2023 8.4.3.11 rule 5)");
    r->line = t->line;
    if (!strcmp(t->s, "line-counter") || !strcmp(t->s, "page-counter")) {
        /* the report's counters: cells of its block, four-byte unsigned */
        int which = t->s[0] == 'p';
        advance();
        Report *rp = NULL;
        if (accept_word("of") || accept_word("in")) {
            if (cur()->kind != T_WORD || !report_find(cur()->s)) die_at(t->line, "%s-COUNTER OF needs a report-name", which ? "PAGE" : "LINE");
            rp = report_find(cur()->s); advance();
        } else {
            if (g_nreport - g_report_base != 1) die_at(t->line, g_nreport > g_report_base ? "%s-COUNTER is ambiguous: say %s-COUNTER OF report-name" : "%s-COUNTER: there is no RD", which ? "PAGE" : "LINE", which ? "PAGE" : "LINE");
            rp = &g_reports[g_report_base];
        }
        r->sym = &g_sym[which ? rp->pc_sym : rp->lc_sym];
        return;
    }
    if (!strcmp(t->s, "linage-counter")) {
        /* LINAGE-COUNTER [OF|IN file-name]: the cell of that file, or of the one LINAGE file */
        advance();
        File *lf = NULL;
        if (accept_word("of") || accept_word("in")) {
            if (cur()->kind != T_WORD || !file_find(cur()->s)) die_at(t->line, "LINAGE-COUNTER OF needs a file-name");
            lf = file_find(cur()->s); advance();
            if (!lf->linage) die_at(t->line, "file '%s' has no LINAGE clause", lf->name);
        } else {
            int n = 0;
            for (int i = g_file_base; i < g_nfile; i++) if (g_files[i].linage) { lf = &g_files[i]; n++; }
            if (!lf) die_at(t->line, "LINAGE-COUNTER: no file has a LINAGE clause");
            if (n > 1) die_at(t->line, "LINAGE-COUNTER is ambiguous: say LINAGE-COUNTER OF file-name");
        }
        r->sym = &g_sym[lf->lin_counter_sym];
        return;
    }
    char *name = t->s; advance();
    char *quals[64]; int nq = 0;                    /* NC207A qualifies 48 deep */
    while (at_word("of") || at_word("in")) {
        advance();
        if (cur()->kind != T_WORD) die_at(cur()->line, "expected a data-name after OF/IN");
        if (nq < 64) quals[nq++] = cur()->s; else die_at(cur()->line, "more than 64 qualifiers");
        advance();
    }
    r->sym = sym_lookup(name, quals, nq, t->line);
    /* an unsubscripted item's parenthesis holding a ':' is a reference
     * modification, not a subscript list */
    int lead_rm = 0;
    if (cur()->kind == T_LP && !cur()->after_comma && r->sym->ndims == 0) {
        int depth = 0;
        for (int i = g_tp; i < g_ntok; i++) {
            if (g_tok[i].kind == T_LP) depth++;
            else if (g_tok[i].kind == T_RP) { if (--depth == 0) break; }
            else if (g_tok[i].kind == T_COLON && depth == 1) { lead_rm = 1; break; }
            else if (g_tok[i].kind == T_PERIOD) break;
        }
    }
    if (cur()->kind == T_LP && !cur()->after_comma && !lead_rm) {   /* MAX(B, (C + 1) / 2): the comma detaches the paren */
        advance();
        for (;;) {
            if (r->nsub >= MAXDIM) die_at(cur()->line, "too many subscripts");
            Tok *st = cur();
            if (sub_is_expr()) {
                if (g_std < 2002) die_at(st->line, "an arithmetic-expression subscript is COBOL 2002; compile with -std=2002");
                /* an arithmetic expression (2002 8.4.1.2.1): evaluated
                 * when the reference is, before its address is formed */
                r->sub[r->nsub].sym = &g_subx; r->sub[r->nsub].lit = 0; r->sub[r->nsub].adj = 0;
                r->sub[r->nsub].x = scan_expr();
            } else if (st->kind == T_NUM) {
                NumLit n; numlit_parse(st, &n);
                if (!numlit_is_int(&n) || n.neg) die_at(st->line, "a subscript must be a positive integer");
                r->sub[r->nsub].lit = numlit_int(&n);
                advance();
            } else if (st->kind == T_WORD) {
                char *sname = st->s; advance();
                char *sq[64]; int snq = 0;                 /* NC246A qualifies a subscript 18 deep */
                while (at_word("of") || at_word("in")) {
                    advance();
                    if (cur()->kind != T_WORD) die_at(cur()->line, "expected a qualifier after OF/IN");
                    if (snq == 64) die_at(cur()->line, "more than 64 qualifiers on a subscript");
                    sq[snq++] = cur()->s; advance();
                }
                Sym *ss = sym_lookup(sname, sq, snq, st->line);
                if (!is_int_item(ss)) die_at(st->line, "the subscript '%s' must be an integer item", ss->name);
                if (ss->ndims) die_at(st->line, "a subscript cannot itself be subscripted in COBOL 85");
                r->sub[r->nsub].sym = ss;
                if (at_op("+") || at_op("-")) {
                    int neg = at_op("-"); advance();
                    if (cur()->kind != T_NUM) die_at(cur()->line, "expected an integer after '%s' in a subscript", neg ? "-" : "+");
                    NumLit n; numlit_parse(cur(), &n);
                    r->sub[r->nsub].adj = neg ? -numlit_int(&n) : numlit_int(&n);
                    advance();
                }
            } else die_at(st->line, "expected a subscript, found %s", tok_desc(st));
            r->nsub++;
            if (cur()->kind == T_RP) { advance(); break; }
        }
    }
    /* item(start:len) -- after the subscripts, or alone on an unsubscripted
     * item: the parenthesis holds a ':' at depth one */
    int is_rm = 0;
    if (cur()->kind == T_LP) {
        int depth = 0;
        for (int i = g_tp; i < g_ntok; i++) {
            if (g_tok[i].kind == T_LP) depth++;
            else if (g_tok[i].kind == T_RP) { if (--depth == 0) break; }
            else if (g_tok[i].kind == T_COLON && depth == 1) { is_rm = 1; break; }
            else if (g_tok[i].kind == T_PERIOD) break;
        }
    }
    if (is_rm) {
        if (r->sym->is_cond) die_at(r->line, "a condition-name cannot be reference-modified");
        advance();
        r->rm = 1; r->rm_lx = NULL; r->user_rm = 1;
        if (r->sym->strong || (sym_in_strong(r->sym) && (is_numeric_sym(r->sym) || r->sym->pi.edited)))
            die_at(r->line, "'%s' is %s and is not reference-modified (2023 8.4.2.4)", r->sym->name,
                   r->sym->strong ? "a strongly-typed group" : "a numeric or edited item in a strongly-typed group");
        /* 2023 8.4.3.3.4: a USAGE NATIONAL item counts characters, its part
         * national, or boolean for a boolean item (rule 6); a USAGE BIT item
         * or bit group counts bits (rule 5a) */
        r->rm_nat = sym_is_national(r->sym) || (!r->sym->is_group && r->sym->usage == U_NATIONAL);
        r->rm_bit = (!r->sym->is_group && r->sym->usage == U_BIT) || r->sym->bitgroup;   /* 2023 8.4.2.4: character positions; a national group as elementary */
        if (cur()->kind == T_NUM && peek(1)->kind == T_COLON) {
            NumLit n; numlit_parse(cur(), &n);
            if (!numlit_is_int(&n) || n.neg || numlit_int(&n) < 1) die_at(cur()->line, "the start of a reference modification must be a positive integer");
            r->rm_start = (long)numlit_int(&n); advance();
        } else {
            r->rm_sx = scan_expr();
        }
        if (cur()->kind != T_COLON) die_at(cur()->line, "expected ':' in the reference modification");
        advance();
        if (cur()->kind == T_RP) { /* (start:) runs to the end */ }
        else if (cur()->kind == T_NUM && peek(1)->kind == T_RP) {
            NumLit n; numlit_parse(cur(), &n);
            if (!numlit_is_int(&n) || n.neg || numlit_int(&n) < 1) die_at(cur()->line, "the length of a reference modification must be a positive integer");
            r->rm_len = (long)numlit_int(&n); advance();
        } else {
            r->rm_lx = scan_expr();
        }
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the reference modification");
        advance();
        long chars = r->rm_bit ? r->sym->bits : r->rm_nat ? r->sym->size / 2 : r->sym->size;
        if (!r->sym->any_len) {                     /* its length is the argument's, known at run time */
            if (r->rm_start && r->rm_start > chars) die_at(r->line, "reference modification starts past the end of '%s'", r->sym->name);
            if (r->rm_start && r->rm_len && r->rm_start - 1 + r->rm_len > chars) die_at(r->line, "reference modification runs past the end of '%s'", r->sym->name);
            if (r->rm_start && !r->rm_len && !r->rm_lx) r->rm_len = chars - r->rm_start + 1;
        }
    }
    if (r->sym->split_key && strcmp(g_cur_stmt, "READ") && strcmp(g_cur_stmt, "START"))
        die_at(r->line, "'%s' is a record-key-name (a split key): READ and START name it, and no other statement (2002 14.8.29, 14.8.37; Micro Focus SELECT rule 22)", r->sym->name);
    if (r->sym->any_len && !r->rm) {
        /* the whole ANY LENGTH item: (1:), to the end its descriptor gives
         * at run time, so every statement takes its length as it takes a
         * computed reference modification's */
        r->rm = 1; r->rm_start = 1; r->rm_len = 0; r->rm_lx = NULL;
        r->rm_nat = r->sym->pi.category == PIC_NATIONAL;
    }
    for (int i = 0; i < r->nsub; i++)
        if (r->sub[i].sym == &g_subx && (r->sym->usage == U_BIT || r->sym->bitgroup))
            die_at(r->line, "'%s': an arithmetic-expression subscript of a bit data item is not implemented", r->sym->name);
    ref_resolve_bits(r);
    if (r->nsub != r->sym->ndims) {
        if (r->sym->ndims == 0) die_at(r->line, "'%s' is not a table item and takes no subscript", r->sym->name);
        die_at(r->line, "'%s' needs %d subscript%s, %d given", r->sym->name, r->sym->ndims,
               r->sym->ndims == 1 ? "" : "s", r->nsub);
    }
    for (int i = 0; i < r->nsub; i++)
        if (!r->sub[i].sym && (r->sub[i].lit < 1 || r->sub[i].lit > r->sym->dim_count[i]))
            die_at(r->line, "subscript %ld is outside OCCURS %d of '%s'", r->sub[i].lit, r->sym->dim_count[i], r->sym->name);
    index_ref_check(r);
}
