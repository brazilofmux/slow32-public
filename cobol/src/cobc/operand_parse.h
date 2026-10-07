/* s32-cobc: operand parsing, intrinsic functions, references.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

enum { FN_UPPER, FN_LOWER, FN_CURDATE, FN_INTDATE, FN_DATEINT, FN_DAYINT, FN_INTDAY, FN_EXCSTATUS, FN_EXCSTMT,
       FN_NATOF, FN_DISPOF, FN_CHARNAT, FN_VARLEN, FN_EXCFILE, FN_EXCLOC, FN_BOOLOFINT, FN_INTOFBOOL, FN_RMLEN, FN_TRIM };
/* the calendar functions (1989 addendum) take an integer and give one back;
 * the runtime renders the result as numeric DISPLAY digits in its buffer */
static int fn_is_numeric(int fn) { return (fn >= FN_INTDATE && fn <= FN_INTDAY) || fn == FN_VARLEN || fn == FN_INTOFBOOL || fn == FN_RMLEN; }
static int num_desc(int digits);
/* a run-time integer result (LENGTH of a run-time length, INTEGER-OF-
 * BOOLEAN): DISPLAYed as its value, no leading zeros, as a compile-time
 * LENGTH is and as GnuCOBOL shows integer functions */
static int fn_num_desc(const Opnd *o)
{
    int d = num_desc(o->fsize);
    if (o->fn != FN_VARLEN && o->fn != FN_RMLEN && o->fn != FN_INTOFBOOL) return d;
    Desc x = g_desc[d]; x.flags |= COB_F_INTFN;
    return desc_add(&x);
}
static const char *fn_runtime_name(int fn)
{
    switch (fn) {
    case FN_INTDATE: return "cob_fn_integer_of_date";
    case FN_DATEINT: return "cob_fn_date_of_integer";
    case FN_DAYINT:  return "cob_fn_day_of_integer";
    default:         return "cob_fn_integer_of_day";
    }
}
static int opnd_size(Opnd *o);

static void numlit_from_int(NumLit *n, long v)
{
    memset(n, 0, sizeof *n);
    char b[24]; snprintf(b, sizeof b, "%ld", v < 0 ? -v : v);
    n->neg = v < 0; n->ndigits = (int)strlen(b); memcpy(n->digits, b, n->ndigits);
}

static int has_odo(Sym *s);
static Sym *odo_table_below(Sym *s);

/* an operand that is a whole group over an OCCURS DEPENDING ON table
 * (no subscript, no reference modification) has the group's current
 * length wherever it is sent -- MOVE, STRING, UNSTRING, INSPECT, a
 * comparison, DISPLAY: it becomes (1:length) computed at run time.
 * A receiving group is decided in emit_move: the current length too when
 * the DEPENDING ON item is outside it, the maximum when it is inside. */
/* the group's length over its ODO table: the fixed part and the element,
 * in bytes -- or, the table a bit array, in bits from the group's first
 * byte (cob_odo_length_bits rounds the total up to bytes) */
static void odo_ref_lengths(Ref *r, const Sym *g, const Sym *tbl)
{
    if (!tbl->is_group && tbl->usage == U_BIT) {
        r->odo_base = (tbl->offset - g->offset) * 8 + tbl->bitoff; r->odo_elem = bit_stride(tbl); r->odo_bits = tbl->bits;
    } else { r->odo_base = g->size - tbl->occurs * tbl->size; r->odo_elem = tbl->size; r->odo_bits = 0; }
}
static void operand_odo_length(Opnd *o)
{
    if (o->kind != O_REF || o->ref.rm || o->ref.nsub) return;
    Sym *g = o->ref.sym;
    if (!g->is_group || !has_odo(g)) return;
    Sym *tbl = odo_table_below(g);
    if (!tbl || !tbl->odo_dep_sym) return;
    for (Sym *k = tbl; k != g; k = &g_sym[k->parent])
        if (k->sibling >= 0)
            die_at(o->line, "'%s': items follow its OCCURS DEPENDING ON table (variable-location items are not implemented)", g->name);
    o->ref.rm = 1; o->ref.rm_start = 1; o->ref.rm_len = 0; o->ref.rm_lx = NULL;
    o->ref.rm_odo = 1; o->ref.odo_dep = tbl->odo_dep_sym;
    odo_ref_lengths(&o->ref, g, tbl);
}

static void parse_operand_raw(Opnd *o);
/* FUNCTION name [(args)] (leftmost:[length]) -- a reference modification
 * of an alphanumeric function's result (X3.23a-1989, the reference-
 * modifier format; cobol ISSUES-54).  Literal positions: the function is
 * evaluated at its full width and the operand is the part. */
static void function_refmod(Opnd *o)
{
    if (o->kind != O_FUNC || cur()->kind != T_LP) return;
    int d = 0, colon = 0;
    for (int k = g_tp; k < g_ntok && g_tok[k].kind != T_EOF; k++) {
        if (g_tok[k].kind == T_LP) d++;
        else if (g_tok[k].kind == T_RP) { if (--d == 0) break; }
        else if (g_tok[k].kind == T_COLON && d == 1) { colon = 1; break; }
    }
    if (!colon) return;
    int line = cur()->line;
    int numeric = o->fn == -1 ? o->fscale >= 0 : fn_is_numeric(o->fn);
    if (numeric) die_at(line, "a numeric function cannot be reference-modified (2023 8.4.3.3.3 rule 2)");
    /* positions are characters: two bytes each in a national result; a
     * result of run-time length is bounded by its maximum here (cobol
     * ISSUES-88) */
    int unit = o->fnat ? 2 : 1, chars = o->fsize / unit;
    advance();
    if (cur()->kind != T_NUM || peek(1)->kind != T_COLON || !(peek(2)->kind == T_RP || (peek(2)->kind == T_NUM && peek(3)->kind == T_RP))) {
        /* a computed start or length: evaluated after the function, the
         * part's place and length found at run time (cobol ISSUES-91) */
        o->fsx = scan_expr();
        if (cur()->kind != T_COLON) die_at(cur()->line, "expected ':' in the reference modification");
        advance();
        o->flx = NULL; o->flen = 0;
        if (cur()->kind != T_RP) o->flx = scan_expr();
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the reference modification");
        advance();
        if (!o->ffull) o->ffull = o->fsize;
        o->fwasvar = o->fvar;                   /* the whole result's length: the runtime's, or ffull */
        o->fvar = 1;                            /* the part's length is known at run time */
        o->frm = -1;                            /* marks the computed form */
        return;
    }
    NumLit a; numlit_parse(cur(), &a);
    long start = numlit_is_int(&a) && !a.neg ? (long)numlit_int(&a) : 0;
    if (start < 1 || start > chars) die_at(line, "the reference modification starts outside the function's %d characters", chars);
    advance(); advance();
    long len = chars - start + 1; int given = 0;
    if (cur()->kind != T_RP) {
        if (cur()->kind != T_NUM || peek(1)->kind != T_RP)
            die_at(line, "internal: a function's reference modification of literal start and non-literal length took the literal route");
        NumLit b; numlit_parse(cur(), &b);
        len = numlit_is_int(&b) && !b.neg ? (long)numlit_int(&b) : 0;
        if (len < 1 || start + len - 1 > chars) die_at(line, "the reference modification runs outside the function's %d characters", chars);
        given = 1;
        advance();
    }
    advance();
    if (!o->ffull) o->ffull = o->fsize;
    o->frm += ((int)start - 1) * unit; o->fsize = (int)len * unit;
    /* a run-time-length result: with a length, the part is fixed; to its
     * end, the part's length is the result's less the start (at run time) */
    if (o->fvar && given) { o->fvar = 0; o->fwasvar = 1; }
}
static void parse_operand(Opnd *o) { parse_operand_raw(o); function_refmod(o); operand_odo_length(o); }

/* the 1989 amendment's functions: argument shapes FK_NUMS (a list of
 * numerics onto the stack), FK_INT (one integer by value), FK_ALNUM
 * (one string), FK_NONE.  Results: numeric digit strings (scale 0 or
 * 9), or a string buffer. */
enum { FK_NUMS, FK_INT, FK_ALNUM, FK_NONE, FK_ALNUMS };
static const struct { const char *name; int id, kind, scale, minargs, maxargs, fsize, std; } g_fn89[] = {
    { "max", COB_FN_MAX, FK_NUMS, 9, 1, 99, 19, 85 },
    { "min", COB_FN_MIN, FK_NUMS, 9, 1, 99, 19, 85 },
    { "ord-max", COB_FN_ORD_MAX, FK_NUMS, 0, 1, 99, 19, 85 },
    { "ord-min", COB_FN_ORD_MIN, FK_NUMS, 0, 1, 99, 19, 85 },
    { "sum", COB_FN_SUM, FK_NUMS, 9, 1, 99, 19, 85 },
    { "range", COB_FN_RANGE, FK_NUMS, 9, 1, 99, 19, 85 },
    { "midrange", COB_FN_MIDRANGE, FK_NUMS, 9, 1, 99, 19, 85 },
    { "mean", COB_FN_MEAN, FK_NUMS, 9, 1, 99, 19, 85 },
    { "median", COB_FN_MEDIAN, FK_NUMS, 9, 1, 99, 19, 85 },
    { "variance", COB_FN_VARIANCE, FK_NUMS, 9, 1, 99, 19, 85 },
    { "standard-deviation", COB_FN_STDDEV, FK_NUMS, 9, 1, 99, 19, 85 },
    { "mod", COB_FN_MOD, FK_NUMS, 0, 2, 2, 19, 85 },
    { "rem", COB_FN_REM, FK_NUMS, 9, 2, 2, 19, 85 },
    { "integer", COB_FN_INTEGER, FK_NUMS, 0, 1, 1, 19, 85 },
    { "integer-part", COB_FN_INTEGER_PART, FK_NUMS, 0, 1, 1, 19, 85 },
    { "factorial", COB_FN_FACTORIAL, FK_NUMS, 0, 1, 1, 19, 85 },
    { "sqrt", COB_FN_SQRT, FK_NUMS, 9, 1, 1, 19, 85 },
    { "log", COB_FN_LOG, FK_NUMS, 9, 1, 1, 19, 85 },
    { "log10", COB_FN_LOG10, FK_NUMS, 9, 1, 1, 19, 85 },
    { "sin", COB_FN_SIN, FK_NUMS, 9, 1, 1, 19, 85 },
    { "cos", COB_FN_COS, FK_NUMS, 9, 1, 1, 19, 85 },
    { "tan", COB_FN_TAN, FK_NUMS, 9, 1, 1, 19, 85 },
    { "asin", COB_FN_ASIN, FK_NUMS, 9, 1, 1, 19, 85 },
    { "acos", COB_FN_ACOS, FK_NUMS, 9, 1, 1, 19, 85 },
    { "atan", COB_FN_ATAN, FK_NUMS, 9, 1, 1, 19, 85 },
    { "annuity", COB_FN_ANNUITY, FK_NUMS, 9, 2, 2, 19, 85 },
    { "present-value", COB_FN_PRESENT_VALUE, FK_NUMS, 9, 2, 99, 19, 85 },
    { "random", COB_FN_RANDOM, FK_NUMS, 9, 0, 1, 19, 85 },
    { "char", -2, FK_INT, -1, 1, 1, 1, 85 },
    { "ord", -3, FK_ALNUM, 0, 1, 1, 19, 85 },
    { "reverse", -4, FK_ALNUM, -1, 1, 1, 0, 85 },
    { "numval", -5, FK_ALNUM, 9, 1, 1, 19, 85 },
    { "numval-c", -6, FK_ALNUM, 9, 1, 2, 19, 85 },
    /* COBOL 2002 (15.x; cobol ISSUES-52), under -std=2002 */
    { "abs", COB_FN_ABS, FK_NUMS, 9, 1, 1, 19, 2002 },
    { "exp", COB_FN_EXP, FK_NUMS, 9, 1, 1, 19, 2002 },
    { "exp10", COB_FN_EXP10, FK_NUMS, 9, 1, 1, 19, 2002 },
    { "pi", COB_FN_PI, FK_NUMS, 9, 0, 0, 19, 2002 },
    { "e", COB_FN_E, FK_NUMS, 9, 0, 0, 19, 2002 },
    { "sign", COB_FN_SIGN, FK_NUMS, 0, 1, 1, 19, 2002 },
    { "fraction-part", COB_FN_FRACTION_PART, FK_NUMS, 9, 1, 1, 19, 2002 },
    { "year-to-yyyy", COB_FN_YEAR_TO_YYYY, FK_NUMS, 0, 1, 3, 19, 2002 },
    { "date-to-yyyymmdd", COB_FN_DATE_TO_YYYYMMDD, FK_NUMS, 0, 1, 3, 19, 2002 },
    { "day-to-yyyyddd", COB_FN_DAY_TO_YYYYDDD, FK_NUMS, 0, 1, 3, 19, 2002 },
    { "test-date-yyyymmdd", COB_FN_TEST_DATE_YYYYMMDD, FK_NUMS, 0, 1, 1, 19, 2002 },
    { "test-day-yyyyddd", COB_FN_TEST_DAY_YYYYDDD, FK_NUMS, 0, 1, 1, 19, 2002 },
    { "numval-f", -7, FK_ALNUM, 9, 1, 1, 19, 2002 },
    { "test-numval", -8, FK_ALNUM, 0, 1, 1, 19, 2002 },
    { "test-numval-c", -9, FK_ALNUM, 0, 1, 2, 19, 2002 },
    { "test-numval-f", -10, FK_ALNUM, 0, 1, 1, 19, 2002 },
    { NULL, 0, 0, 0, 0, 0, 0, 0 }
};

static void parse_operand(Opnd *o);
static Opnd expr_opnd(void);
static Opnd expr_opnd_after(const Opnd *first, int start);
static int at_arith_op(void);
static int opnd_is_national(const Opnd *o);
static int opnd_is_boolean(const Opnd *o);

/* a function's result: numeric, and an integer (X3.23a-1989 23; 2023
 * 15.2): the table's scale 0 or 9, or a calendar or length function */
static int fn_is_numeric(int fn);
static int opnd_fn_numeric(const Opnd *o) { return o->fn < 0 ? o->fscale >= 0 : fn_is_numeric(o->fn); }
static int opnd_fn_integer(const Opnd *o) { return o->fn < 0 ? o->fscale == 0 : fn_is_numeric(o->fn); }
/* a function computed in double, FACTORIAL, PI or E: its value as wide
 * as it is, so a statement that uses it computes on the wide stack */
static int fn_inexact(const Opnd *o)
{
    if (o->kind != O_FUNC || o->fn != -1 || o->fkind != FK_NUMS) return 0;
    switch (o->fnid) {
    case COB_FN_MEAN: case COB_FN_MEDIAN: case COB_FN_VARIANCE: case COB_FN_STDDEV: case COB_FN_SQRT: case COB_FN_LOG:
    case COB_FN_LOG10: case COB_FN_SIN: case COB_FN_COS: case COB_FN_TAN: case COB_FN_ASIN: case COB_FN_ACOS: case COB_FN_ATAN:
    case COB_FN_ANNUITY: case COB_FN_PRESENT_VALUE: case COB_FN_RANDOM: case COB_FN_EXP: case COB_FN_EXP10: case COB_FN_FACTORIAL:
    case COB_FN_PI: case COB_FN_E:
        return 1;
    }
    return 0;
}

/* an operand's class, for the functions' argument rules (X3.23a-1989 22;
 * 2023 15.3): 'N' numeric, 'A' alphabetic or alphanumeric, 'X' national,
 * 'B' boolean, 0 none of those (a figurative constant, an index, a pointer) */
static int opnd_class(const Opnd *o)
{
    if (opnd_is_boolean(o)) return 'B';
    if (opnd_is_national(o)) return 'X';
    switch (o->kind) {
    case O_NUM: case O_EXPR: return 'N';
    case O_STR: case O_ALL: return 'A';
    case O_FUNC: return opnd_fn_numeric(o) ? 'N' : 'A';
    case O_REF: {
        Sym *s = o->ref.sym;
        if (o->ref.rm || s->is_group) return 'A';
        if (s->is_index || s->usage == U_INDEX || s->usage == U_POINTER) return 0;
        return is_numeric_sym(s) ? 'N' : 'A';
    }
    }
    return 0;
}

/* argument k of FUNCTION name is of the kind the function takes: 'N'
 * numeric, 'I' an integer, 'A' alphanumeric (or national, where the
 * function allows it).  An integer is checked here when it is an item, a
 * literal or a function; an expression's value, at run time. */
static void fn_arg_check(const Opnd *x, int want, const char *name, int k, int line)
{
    int e85 = g_std < 2002, c = opnd_class(x);
    char up[40]; snprintf(up, sizeof up, "%s", name);
    for (char *q = up; *q; q++) *q = (char)toupper((unsigned char)*q);
    if (want == 'A') {
        if (c == 'N' || c == 'B' || c == 0)
            die_at(line, "FUNCTION %s: argument %d is %s; it takes an alphanumeric argument (%s)", up, k,
                   c == 'N' ? "numeric" : c == 'B' ? "boolean" : "not a character string",
                   e85 ? "X3.23a-1989 22 (3)" : "2023 15.3 rule 2");
        return;
    }
    if (c != 'N')
        die_at(line, "FUNCTION %s: argument %d is not numeric (%s)", up, k,
               e85 ? (want == 'I' ? "X3.23a-1989 22 (4)" : "X3.23a-1989 22 (1)") : (want == 'I' ? "2023 15.3 rule 6" : "2023 15.3 rule 10"));
    if (want != 'I') return;
    const char *rule = e85 ? "X3.23a-1989 22 (4)" : "2023 15.3 rule 6";
    if (x->kind == O_NUM && !numlit_is_int(&x->num))
        die_at(line, "FUNCTION %s: argument %d is an integer (%s)", up, k, rule);
    if (x->kind == O_REF && (x->ref.sym->pi.scale > 0 || x->ref.sym->usage == U_FLOAT))
        die_at(line, "FUNCTION %s: argument %d is an integer; '%s' is not an integer item (%s)", up, k, x->ref.sym->name, rule);
    if (x->kind == O_FUNC && !opnd_fn_integer(x)) {
        char xn[40]; snprintf(xn, sizeof xn, "%s", x->fname ? x->fname : "?");
        for (char *q = xn; *q; q++) *q = (char)toupper((unsigned char)*q);
        die_at(line, "FUNCTION %s: argument %d is an integer; FUNCTION %s is a numeric function, not an integer function (%s)", up, k,
               xn, e85 ? "X3.23a-1989 23 (2)" : rule);
    }
}

/* one function argument: an expression, an item, a literal -- or a
 * one-dimension table with the subscript ALL, every element an argument */
/* a table reference ahead with ALL among its subscripts: the positions
 * written ALL, as a mask (a relative subscript's + or - and its integer
 * belong to the subscript before them) */
static unsigned all_sub_ahead(void)
{
    int i = g_tp;
    if (g_tok[i].kind != T_WORD || is_word(&g_tok[i], "function")) return 0;
    i++;
    while (i + 1 < g_ntok && (is_word(&g_tok[i], "of") || is_word(&g_tok[i], "in")) && g_tok[i + 1].kind == T_WORD) i += 2;
    if (i >= g_ntok || g_tok[i].kind != T_LP || g_tok[i].after_comma) return 0;
    unsigned m = 0; int pos = 0;
    for (int j = i + 1; j < g_ntok && g_tok[j].kind != T_RP; ) {
        Tok *t = &g_tok[j];
        if (t->kind == T_EOF || t->kind == T_PERIOD || t->kind == T_LP || pos >= MAXDIM) return 0;
        if (t->kind == T_OP && (!strcmp(t->s, "+") || !strcmp(t->s, "-"))) { j += 2; continue; }
        if (is_word(t, "all")) m |= 1u << pos;
        pos++; j++;
    }
    return m;
}

/* An argument phrase of a function from a later edition: ANYCASE (NUMVAL-C,
 * TEST-NUMVAL-C, 2014) and LOCALE (locale support), refused by name */
static void fn_phrase_nyi(const char *fname)
{
    if ((at_word("anycase") || at_word("locale")) && !sym_lookup_quiet(cur()->s))
        die_at(cur()->line, "FUNCTION %s: the %s phrase is not implemented (%s)", fname, at_word("anycase") ? "ANYCASE" : "LOCALE",
               at_word("anycase") ? "COBOL 2014, 2023 15.68" : "locale support, 2023 15.1");
}

static Opnd *fn89_arg(const char *fname)
{
    fn_phrase_nyi(fname);
    Opnd *x = xmalloc(sizeof *x);
    unsigned mask = all_sub_ahead();
    if (mask) {
        /* ALL as a subscript (X3.23a-1989 2.2; 2023 15.3): the reference
         * parsed with 1 in each ALL position, every element taken at run
         * time, the rightmost ALL varying fastest */
        static char one[] = "1";
        int lp = g_tp; while (g_tok[lp].kind != T_LP) lp++;
        int rp = lp; while (g_tok[rp].kind != T_RP) rp++;
        Tok save[MAXDIM * 3]; int at[MAXDIM * 3], ns = 0;
        for (int j = lp + 1; j < rp && ns < MAXDIM * 3; j++)
            if (is_word(&g_tok[j], "all")) { at[ns] = j; save[ns++] = g_tok[j]; g_tok[j].kind = T_NUM; g_tok[j].s = one; g_tok[j].len = 1; }
        parse_operand(x);
        for (int k = 0; k < ns; k++) g_tok[at[k]] = save[k];
        if (x->kind != O_REF || x->ref.rm || x->ref.nsub != x->ref.sym->ndims)
            die_at(x->line, "FUNCTION %s: ALL subscripts a table element, every subscript written", fname);
        x->all_sub = mask;
        return x;
    }
    if (cur()->kind == T_LP) { *x = expr_opnd(); return x; }   /* SIN((3 * PI) / 2) */
    int start = g_tp;
    parse_operand(x);
    if (at_arith_op()) *x = expr_opnd_after(x, start);
    return x;
}

/* an intrinsic function's name: the 1989 table, or one parsed by name */
static int fn89_known(const char *w)
{
    static const char *named[] = { "when-compiled", "upper-case", "lower-case", "current-date", "integer-of-date",
        "date-of-integer", "day-of-integer", "integer-of-day", "length", "byte-length", "highest-algebraic",
        "lowest-algebraic", "exception-status", "exception-statement", "national-of", "display-of", "char-national",
        "exception-file", "exception-file-n", "exception-location", "exception-location-n",
        "boolean-of-integer", "integer-of-boolean", NULL };
    for (int i = 0; g_fn89[i].name; i++) if (!strcmp(w, g_fn89[i].name)) return 1;
    for (int i = 0; named[i]; i++) if (!strcmp(w, named[i])) return 1;
    return 0;
}

static int fn89_parse(Opnd *o, Tok *n)
{
    int f = -1;
    for (int i = 0; g_fn89[i].name; i++) if (!strcmp(n->s, g_fn89[i].name)) { f = i; break; }
    if (f < 0) return 0;
    if (g_fn89[f].std > g_std) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
    advance();
    o->fnid = g_fn89[f].id; o->fkind = g_fn89[f].kind; o->fscale = g_fn89[f].scale;
    o->fsize = g_fn89[f].fsize; o->fn = -1;
    o->nfargs = 0;
    if (cur()->kind == T_LP) {
        advance();
        o->fargs = xmalloc(16 * sizeof *o->fargs);
        while (cur()->kind != T_RP) {
            if (o->nfargs == 16) die_at(n->line, "FUNCTION %s: more than 16 arguments", n->s);
            if ((g_fn89[f].id == -6 || g_fn89[f].id == -9) && o->nfargs == 2 && at_word("anycase") && !sym_lookup_quiet("anycase")) {
                /* NUMVAL-C (TEST-NUMVAL-C) argument-2 ANYCASE (2023 15.68, 15.94):
                 * the currency string matched in either case */
                if (g_std < 2002) die_at(cur()->line, "FUNCTION %s: ANYCASE is COBOL 2014; compile with -std=2002", n->s);
                o->fanycase = 1; advance();
                if (cur()->kind != T_RP) die_at(cur()->line, "FUNCTION %s: ANYCASE ends the arguments", n->s);
                break;
            }
            if (at_word("omitted") && !sym_lookup_quiet("omitted")) die_at(cur()->line, "FUNCTION %s: OMITTED is for a user-defined function's argument, not an intrinsic's (2023 8.4.3.2.3 rule 7)", n->s);
            o->fargs[o->nfargs++] = fn89_arg(n->s);   /* the tokenizer drops the decorative commas */
        }
        advance();
    }
    if (o->nfargs < g_fn89[f].minargs || o->nfargs > g_fn89[f].maxargs)
        die_at(n->line, "FUNCTION %s takes %d to %d arguments", n->s, g_fn89[f].minargs, g_fn89[f].maxargs);
    {
        int id = g_fn89[f].id, kind = g_fn89[f].kind;
        int anyclass = id == COB_FN_MAX || id == COB_FN_MIN || id == COB_FN_ORD_MAX || id == COB_FN_ORD_MIN;
        for (int i = 0; i < o->nfargs; i++) {
            int want = kind == FK_ALNUM ? 'A' : kind == FK_INT ? 'I' : 'N';
            if (kind == FK_NUMS && (id == COB_FN_MOD || id == COB_FN_FACTORIAL || id == COB_FN_YEAR_TO_YYYY ||
                                    id == COB_FN_DATE_TO_YYYYMMDD || id == COB_FN_DAY_TO_YYYYDDD ||
                                    id == COB_FN_TEST_DATE_YYYYMMDD || id == COB_FN_TEST_DAY_YYYYDDD ||
                                    id == COB_FN_RANDOM || (id == COB_FN_ANNUITY && i == 1))) want = 'I';
            if (!anyclass) fn_arg_check(o->fargs[i], want, n->s, i + 1, n->line);
        }
        if (anyclass) {
            /* one class throughout, alphabetic mixing with alphanumeric;
             * not boolean (2023 15.59.3, 15.63.3, 15.71.3, 15.72.3 rules 1-2) */
            int c0 = opnd_class(o->fargs[0]);
            for (int i = 0; i < o->nfargs; i++) {
                int c = o->fargs[i]->all_sub ? (is_numeric_sym(o->fargs[i]->ref.sym) ? 'N' : 'A') : opnd_class(o->fargs[i]);
                if (i == 0) c0 = c;
                char up[40]; snprintf(up, sizeof up, "%s", n->s);
                for (char *q = up; *q; q++) *q = (char)toupper((unsigned char)*q);
                if (c == 'B' || c == 0 || c != c0)
                    die_at(n->line, "FUNCTION %s: its arguments are all numeric, all alphanumeric or all national (%s)", up,
                           g_std < 2002 ? "X3.23a-1989 MAX, MIN, ORD-MAX and ORD-MIN argument rules" : "2023 15.59.3 rule 2");
                no_zero_lit(o->fargs[i], up, "2023 15.59.3, 15.63.3 rule 3; 15.71.3, 15.72.3 rule 2");
            }
        }
        if (kind == FK_ALNUM && o->nfargs == 2 && (opnd_class(o->fargs[1]) != opnd_class(o->fargs[0])))
            die_at(n->line, "FUNCTION %s: argument 2 is of the same class as argument 1 (2023 15.68.3 rule 2)", n->s);
    }
    if (g_fn89[f].kind == FK_ALNUM) {
        Opnd *x = o->fargs[0];
        if (x->kind != O_REF && x->kind != O_STR && x->kind != O_FUNC)
            die_at(n->line, "FUNCTION %s takes an alphanumeric item or literal", n->s);
        if (o->fsize == 0) {
            /* REVERSE: the argument's width -- a reference modification of
             * computed length gives a result of run-time length, at most
             * the item's (the runtime records the actual one) */
            if (x->kind == O_REF && x->ref.rm && ref_static_len(&x->ref) <= 0) { o->fsize = (int)x->ref.sym->size; o->fvar = 1; }
            else o->fsize = x->kind == O_REF ? (x->ref.rm ? ref_static_len(&x->ref) : (int)x->ref.sym->size)
                          : x->kind == O_FUNC ? x->fsize : x->tok->len;
        }
    }
    if (g_fn89[f].kind == FK_NUMS &&
        (o->fnid == COB_FN_MAX || o->fnid == COB_FN_MIN || o->fnid == COB_FN_ORD_MAX || o->fnid == COB_FN_ORD_MIN)) {
        int alnum = 0, w = 0;
        for (int i = 0; i < o->nfargs; i++) {
            Opnd *x = o->fargs[i];
            int aw = x->kind == O_STR ? x->tok->len
                   : x->kind == O_REF && !is_numeric_sym(x->ref.sym) ? (int)x->ref.sym->size : 0;
            if (aw) { alnum = 1; if (aw > w) w = aw; }
        }
        if (alnum) {                        /* the largest ARGUMENT, as a string */
            o->fkind = FK_ALNUMS;
            if (o->fnid == COB_FN_MAX || o->fnid == COB_FN_MIN) { o->fscale = -1; o->fsize = w; o->fvar = 1; }   /* the selected argument's size (15.59.4 rule 3) */
        }
    }
    /* the exact functions: any argument of up to 31 digits, the result as
     * wide as it needs (docs/wide.md phase 3).  Six have an integer result
     * no longer than their arguments, which the 64-bit code states exactly
     * as 18 digits: those keep it when every argument is an item or a
     * literal of at most 18 digits (an expression's intermediates may pass
     * 18) -- a MOD in a loop is common, and the wide stack's round trip
     * costs it several times over. */
    int narrow_ok = o->fkind == FK_NUMS && (o->fnid == COB_FN_MOD || o->fnid == COB_FN_INTEGER || o->fnid == COB_FN_INTEGER_PART ||
                                            o->fnid == COB_FN_SIGN || o->fnid == COB_FN_ORD_MAX || o->fnid == COB_FN_ORD_MIN);
    for (int i = 0; narrow_ok && i < o->nfargs; i++)
        if ((o->fargs[i]->kind != O_REF && o->fargs[i]->kind != O_NUM) || opnds_wide(o->fargs[i], 1)) narrow_ok = 0;
    if (!narrow_ok &&
        ((o->fkind == FK_NUMS && (o->fnid == COB_FN_MAX || o->fnid == COB_FN_MIN || o->fnid == COB_FN_ORD_MAX || o->fnid == COB_FN_ORD_MIN ||
                                 o->fnid == COB_FN_SUM || o->fnid == COB_FN_RANGE || o->fnid == COB_FN_MIDRANGE || o->fnid == COB_FN_MOD ||
                                 o->fnid == COB_FN_REM || o->fnid == COB_FN_INTEGER || o->fnid == COB_FN_INTEGER_PART || o->fnid == COB_FN_ABS ||
                                 o->fnid == COB_FN_SIGN || o->fnid == COB_FN_FRACTION_PART)) ||
        (o->fkind == FK_ALNUM && (o->fnid == -5 || o->fnid == -6 || o->fnid == -7)))) {
        o->fwnum = 1; o->fsize = 39;
    }
    /* computed in double, or FACTORIAL exactly: on the wide stack too, the
     * result as wide as its value (it was held to nine integer digits) */
    if (o->fkind == FK_NUMS && (o->fnid == COB_FN_MEAN || o->fnid == COB_FN_MEDIAN || o->fnid == COB_FN_VARIANCE ||
        o->fnid == COB_FN_STDDEV || o->fnid == COB_FN_SQRT || o->fnid == COB_FN_LOG || o->fnid == COB_FN_LOG10 ||
        o->fnid == COB_FN_SIN || o->fnid == COB_FN_COS || o->fnid == COB_FN_TAN || o->fnid == COB_FN_ASIN ||
        o->fnid == COB_FN_ACOS || o->fnid == COB_FN_ATAN || o->fnid == COB_FN_ANNUITY || o->fnid == COB_FN_PRESENT_VALUE ||
        o->fnid == COB_FN_RANDOM || o->fnid == COB_FN_EXP || o->fnid == COB_FN_EXP10 || o->fnid == COB_FN_FACTORIAL ||
        o->fnid == COB_FN_PI || o->fnid == COB_FN_E)) {
        o->fwnum = 1; o->fsize = 39;
    }
    o->kind = O_FUNC;
    return 1;
}

static void parse_ufunc(Opnd *o, const char *name, int line);
static int ufn_named(const char *w);
/* a function this compiler does not have: the module or edition it needs */
static void fn_refuse(Tok *n)
{
    static const struct { const char *name, *why; } later[] = {

        { "locale-compare", "locale support" }, { "locale-date", "locale support" }, { "locale-time", "locale support" },
        { "locale-time-from-seconds", "locale support" }, { "standard-compare", "the ISO/IEC 14651 ordering" },
        { NULL, NULL } };
    static const char *y2014[] = { "combined-datetime", "formatted-current-date", "formatted-date", "formatted-datetime",
        "formatted-time", "integer-of-formatted-date", "seconds-from-formatted-time", "seconds-past-midnight",
        "test-formatted-datetime", NULL };
    static const char *y2023[] = { "baseconvert", "concat", "convert", "find-string", "module-name",
        "smallest-algebraic", "substitute", NULL };
    for (int i = 0; later[i].name; i++)
        if (!strcmp(n->s, later[i].name)) die_at(n->line, "FUNCTION %s is COBOL 2002 and needs %s, not implemented yet", n->s, later[i].why);
    for (int i = 0; y2014[i]; i++) if (!strcmp(n->s, y2014[i])) die_at(n->line, "FUNCTION %s is COBOL 2014; not implemented", n->s);
    for (int i = 0; y2023[i]; i++) if (!strcmp(n->s, y2023[i])) die_at(n->line, "FUNCTION %s is COBOL 2023; not implemented", n->s);
    die_at(n->line, "FUNCTION %s is not an intrinsic function", n->s);
}

/* HIGHEST-ALGEBRAIC / LOWEST-ALGEBRAIC (2002 15.33, 15.46): the extreme
 * values the argument can represent -- its picture's, or a native
 * binary usage's range */
static void algebraic_limit(Opnd *o, Opnd *x, int high, Tok *n)
{
    if (x->kind != O_REF || x->ref.rm) die_at(n->line, "FUNCTION %s takes a numeric or numeric-edited item", n->s);
    Sym *a = x->ref.sym;
    memset(o, 0, sizeof *o); o->kind = O_NUM; o->line = n->line; o->folded = 1; o->uc = x->uc;
    long long nat = 0; int sgn = 0;
    switch (a->usage) {
    case U_BCHAR: nat = high ? 127 : -128; sgn = 1; break;
    case U_UBCHAR: nat = high ? 255 : 0; sgn = 1; break;
    case U_SSHORT: nat = high ? 32767 : -32768; sgn = 1; break;
    case U_USHORT: nat = high ? 65535 : 0; sgn = 1; break;
    case U_SINT: nat = high ? 2147483647LL : -2147483648LL; sgn = 1; break;
    case U_UINT: nat = high ? 4294967295LL : 0; sgn = 1; break;
    case U_SDBL: case U_UDBL: {                      /* past 64 signed bits in one case: written out */
        const char *t = a->usage == U_UDBL ? (high ? "18446744073709551615" : "0")
                                           : (high ? "9223372036854775807" : "9223372036854775808");
        o->num.neg = a->usage == U_SDBL && !high;
        o->num.ndigits = (int)strlen(t); memcpy(o->num.digits, t, (size_t)o->num.ndigits); o->num.scale = 0;
        return;
    }
    default: break;
    }
    if (sgn) {
        o->num.neg = nat < 0; unsigned long long m = nat < 0 ? 0ULL - (unsigned long long)nat : (unsigned long long)nat;
        char b[24]; int k = snprintf(b, sizeof b, "%llu", m);
        memcpy(o->num.digits, b, (size_t)k); o->num.ndigits = k; o->num.scale = 0;
        return;
    }
    if (a->is_group || !a->has_pic || (a->pi.category != PIC_NUMERIC && a->pi.category != PIC_NUMERIC_EDITED))
        die_at(n->line, "FUNCTION %s takes a numeric or numeric-edited item", n->s);
    /* all nines in the picture's digit positions; P positions read as zeros */
    int digits = a->pi.digits, scale = a->pi.scale, stored = digits;
    int pz = 0;                                   /* trailing P: zeros left of the point */
    if (scale < 0) { pz = -scale; stored = digits - pz; scale = 0; }
    else if (scale > digits) stored = digits;     /* leading P after the point: V PPP99 */
    int nd = 0;
    int lead0 = scale > digits ? scale - digits : 0;
    for (int i = 0; i < lead0; i++) o->num.digits[nd++] = '0';
    for (int i = 0; i < stored; i++) o->num.digits[nd++] = '9';
    for (int i = 0; i < pz; i++) o->num.digits[nd++] = '0';
    if (nd == 0) o->num.digits[nd++] = '0';
    o->num.ndigits = nd; o->num.scale = scale;
    if (!high) { if (a->pi.is_signed) o->num.neg = 1; else { o->num.ndigits = 1; o->num.digits[0] = '0'; o->num.scale = 0; } }
}

static void parse_operand_raw_1(Opnd *o);
static int ref_has_runtime_sub(const Ref *r);
static void parse_operand_raw(Opnd *o)
{
    Tok *t = cur();
    int fn = t->kind == T_WORD && (!strcmp(t->s, "function") ||
             (!strcmp(t->s, "length") && g_tp + 1 < g_ntok && is_word(&g_tok[g_tp + 1], "of")) ||
             ((ufn_named(t->s) || (g_repo_all_intrinsic && fn89_known(t->s))) && !sym_lookup_quiet(t->s)));
    int start = g_tp;
    g_fn_depth += fn;
    parse_operand_raw_1(o);
    g_fn_depth -= fn;
    if (fn && o->kind == O_FUNC && !o->fname)
        o->fname = !strcmp(t->s, "function") && start + 1 < g_ntok ? g_tok[start + 1].s : t->s;
}

static void parse_operand_raw_1(Opnd *o)
{
    memset(o, 0, sizeof *o);
    Tok *t = cur();
    o->line = t->line;
    /* ADDRESS OF identifier (2002 8.4.2.11; 2023 8.4.3.11): the address of
     * an item, a data-pointer value.  SET, CALL and relations take it. */
    if (t->kind == T_WORD && !strcmp(t->s, "address") && is_word(peek(1), "of") && !sym_lookup_quiet("address")) {
        if (g_std < 2002) die_at(t->line, "ADDRESS OF is COBOL 2002; compile with -std=2002");
        if (!g_cond_depth && strcmp(g_cur_stmt, "SET") && strcmp(g_cur_stmt, "CALL"))
            die_at(t->line, "ADDRESS OF is a sending operand of SET or CALL, or a relation's operand; not of %s", g_cur_stmt);
        advance(); advance();
        if (at_word("function") && !sym_lookup_quiet(cur()->s))
            die_at(t->line, "ADDRESS OF FUNCTION is COBOL 2014 (2023 8.4.3.12, a function-pointer's value); not implemented (docs/plans/standard-queue.md item 26)");
        if (at_word("program") && !sym_lookup_quiet(cur()->s)) {
            /* ADDRESS OF PROGRAM {identifier | literal | prototype-name}
             * (2023 8.4.3.13): a program-pointer value, the program found
             * in the registry at run time (NULL, and EC-PROGRAM-NOT-FOUND
             * when checked, if it is not there); by a prototype-name the
             * value is restricted to that prototype (rule 3) */
            advance();
            o->kind = O_ADDR; o->paddr = 1;
            if (cur()->kind == T_STR) {
                if (cur()->len == 0) die_at(cur()->line, "ADDRESS OF PROGRAM: a literal of length zero (2023 8.4.3.13.3 rule 2)");
                char *nm = xmalloc((size_t)cur()->len + 1); memcpy(nm, cur()->s, (size_t)cur()->len); nm[cur()->len] = 0;
                for (char *k = nm; *k; k++) *k = (char)tolower((unsigned char)*k);
                o->pname = nm; advance();
            } else if (cur()->kind == T_WORD && !sym_lookup_quiet(cur()->s) && repo_pg_find(cur()->s) >= 0) {
                o->pproto = xstrdup(cur()->s);
                char *nm = xstrdup(pg_extname(cur()->s));
                for (char *k = nm; *k; k++) *k = (char)tolower((unsigned char)*k);
                o->pname = nm; advance();
            } else {
                parse_ref(&o->ref);
                const Sym *x = o->ref.sym;
                if (x->is_group || (x->pi.category != PIC_ALPHANUMERIC && x->pi.category != PIC_NATIONAL))
                    die_at(o->line, "ADDRESS OF PROGRAM '%s': an alphanumeric or national item holding the name, a literal, or a program-prototype-name (2023 8.4.3.13)", x->name);
            }
            return;
        }
        o->kind = O_ADDR;
        parse_ref(&o->ref);
        const Sym *x = o->ref.sym;
        cen_flag(x, CEN_ADDR);
        if (x->is_cond || x->is_index) die_at(o->line, "ADDRESS OF '%s': it is not a data item", x->name);
        if (x->strong == 0 && !x->is_group && sym_in_strong(x))
            die_at(o->line, "ADDRESS OF '%s': an item inside a strongly-typed group (2023 8.4.3.11 rule 2)", x->name);
        if (sym_bitlike(x) && ((x->bitoff % 8) || ref_has_runtime_sub(&o->ref) || o->ref.rm))
            die_at(o->line, "ADDRESS OF '%s': a bit item not on a byte, or located at run time (2023 8.4.3.11 rule 4)", x->name);
        return;
    }
    /* LENGTH OF item: the IBM register the corpus writes (damm), the same
     * compile-time size as FUNCTION LENGTH; a data item named LENGTH wins */
    if (t->kind == T_WORD && !strcmp(t->s, "length") && g_tp + 1 < g_ntok &&
        g_tok[g_tp + 1].kind == T_WORD && !strcmp(g_tok[g_tp + 1].s, "of") && !sym_lookup_quiet("length")) {
        advance(); advance();
        Opnd x; parse_operand(&x);
        if (x.kind != O_REF) die_at(t->line, "LENGTH OF takes a data item");
        if (x.kind == O_REF && x.ref.rm && !x.ref.rm_len) {
            /* a part of computed length: its bytes, counted at run time as FUNCTION BYTE-LENGTH counts them */
            Opnd *fx = xmalloc(sizeof *fx); *fx = x;
            memset(o, 0, sizeof *o); o->kind = O_FUNC; o->fn = FN_RMLEN; o->farg = fx; o->fsize = 9;
            o->fnid = 0; o->line = t->line;
            return;
        }
        int len = opnd_size(&x);
        if (len < 0) die_at(t->line, "internal: LENGTH OF a part of unknown length");
        o->kind = O_NUM; numlit_from_int(&o->num, len); o->folded = 1; o->uc = x.uc;   /* a user function's call is still made */
        return;
    }
    /* a user-defined function named in REPOSITORY (or this function
     * itself), invoked without the word FUNCTION (COBOL 2002 8.4.3.2) */
    if (t->kind == T_WORD && ufn_named(t->s) && !sym_lookup_quiet(t->s)) {
        advance(); parse_ufunc(o, t->s, t->line); return;
    }
    /* FUNCTION ALL INTRINSIC: an intrinsic without the word FUNCTION too */
    int bare_fn = t->kind == T_WORD && g_repo_all_intrinsic && fn89_known(t->s) && !sym_lookup_quiet(t->s);
    if (t->kind == T_WORD && (!strcmp(t->s, "function") || bare_fn)) {
        if (!bare_fn) advance();
        Tok *n = cur();
        if (n->kind != T_WORD) die_at(n->line, "expected an intrinsic function name");
        if (ufn_named(n->s)) { advance(); parse_ufunc(o, n->s, n->line); return; }
        if (!strcmp(n->s, "when-compiled")) {
            advance();
            static Tok wc; static char wcbuf[22];
            if (!wcbuf[0]) {
                /* SOURCE_DATE_EPOCH, the reproducible-builds variable:
                 * the compile time to use (tests/asm-snapshot.sh sets it) */
                const char *sde = getenv("SOURCE_DATE_EPOCH");
                time_t now = sde && *sde ? (time_t)strtoll(sde, NULL, 10) : time(0);
                struct tm *t = localtime(&now);
                int y = t->tm_year + 1900, mo = t->tm_mon + 1, da = t->tm_mday;
                int hh = t->tm_hour, mm = t->tm_min, ss = t->tm_sec;
                long off = t->tm_gmtoff; int oneg = off < 0; if (oneg) off = -off;
                int zh = (int)(off / 3600), zm = (int)((off % 3600) / 60);
                if (y < 0) y = 0;
                if (y > 9999) y = 9999;
                if (mo < 1) mo = 1;
                if (mo > 12) mo = 12;
                if (da < 1) da = 1;
                if (da > 31) da = 31;
                if (hh < 0) hh = 0;
                if (hh > 23) hh = 23;
                if (mm < 0) mm = 0;
                if (mm > 59) mm = 59;
                if (ss < 0) ss = 0;
                if (ss > 59) ss = 59;
                if (zh < 0) zh = 0;
                if (zh > 99) zh = 99;
                if (zm < 0) zm = 0;
                if (zm > 59) zm = 59;
                snprintf(wcbuf, sizeof wcbuf, "%04d%02d%02d%02d%02d%02d00%c%02d%02d",
                         y, mo, da, hh, mm, ss, oneg ? '-' : '+', zh, zm);
                wc.kind = T_STR; wc.s = wcbuf; wc.len = 21;
            }
            o->kind = O_STR; o->tok = &wc;
            return;
        }
        if (!strcmp(n->s, "upper-case")) o->fn = FN_UPPER;
        else if (!strcmp(n->s, "lower-case")) o->fn = FN_LOWER;
        else if (!strcmp(n->s, "national-of") || !strcmp(n->s, "display-of") || !strcmp(n->s, "char-national")) {
            /* COBOL 2002 15.66, 15.26, 15.16 (cobol ISSUES-64) */
            if (g_std < 2002) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
            int natof = !strcmp(n->s, "national-of"), dispof = !strcmp(n->s, "display-of");
            advance();
            if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION %s", n->s);
            advance();
            Opnd *a1 = xmalloc(sizeof *a1); parse_operand(a1);
            Opnd *a2 = NULL;
            if (cur()->kind != T_RP && (natof || dispof)) { a2 = xmalloc(sizeof *a2); parse_operand(a2); }
            if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the arguments of FUNCTION %s", n->s);
            advance();
            o->kind = O_FUNC; o->farg = a1; o->farg2 = a2; o->line = n->line;
            if (natof) {
                no_zero_lit(a1, "FUNCTION NATIONAL-OF", "2023 15.66.3 rule 3");
                if (opnd_is_national(a1) || a1->kind == O_NUM || (a1->kind == O_REF && is_numeric_sym(a1->ref.sym)))
                    die_at(n->line, "FUNCTION NATIONAL-OF takes an alphanumeric argument (15.66.3)");
                if (a2 && !(opnd_is_national(a2) && a2->kind != O_FUNC && opnd_size(a2) == 2))
                    die_at(n->line, "FUNCTION NATIONAL-OF: the substitution character is one national character (15.66.3)");
                o->fn = FN_NATOF; o->fnat = 1; o->fvar = 1;
                /* at most one national character per alphanumeric byte */
                o->fsize = 2 * (a1->kind == O_FUNC ? a1->fsize : opnd_size(a1));
            } else if (dispof) {
                if (!opnd_is_national(a1)) die_at(n->line, "FUNCTION DISPLAY-OF takes a national argument (15.26.3)");
                if (a2 && (opnd_is_national(a2) || a2->kind == O_NUM || a2->kind == O_FUNC || opnd_size(a2) != 1))
                    die_at(n->line, "FUNCTION DISPLAY-OF: the substitution character is one alphanumeric character (15.26.3)");
                o->fn = FN_DISPOF; o->fvar = 1;
                /* at most three UTF-8 bytes per national character */
                o->fsize = 3 * ((a1->kind == O_FUNC ? a1->fsize : opnd_size(a1)) / 2);
            } else {
                if (a1->kind != O_NUM && !(a1->kind == O_REF && is_int_item(a1->ref.sym)))
                    die_at(n->line, "FUNCTION CHAR-NATIONAL takes an integer (15.16.3)");
                o->fn = FN_CHARNAT; o->fnat = 1; o->fsize = 2;
            }
            if (o->fsize > 8190) die_at(n->line, "FUNCTION %s: the result could exceed 8190 bytes", n->s);
            return;
        }
        else if (!strcmp(n->s, "trim")) {
            /* TRIM (2014; 2023 15.96): argument-1 [LEADING | TRAILING]
             * [argument-2 ...], the characters to delete, a space by
             * default; the result of run-time length, zero when nothing
             * is left (returned value rule 4) */
            bp(BP_E27_TRIM, n->line);
            advance();
            if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION TRIM");
            advance();
            Opnd *a1 = xmalloc(sizeof *a1); parse_operand(a1);
            int nat = opnd_is_national(a1);
            if (a1->kind == O_NUM || (a1->kind == O_REF && (is_numeric_sym(a1->ref.sym) || sym_is_boolean(a1->ref.sym))) || opnd_is_boolean(a1) ||
                a1->kind == O_FIG || a1->kind == O_ALL || (a1->kind == O_FUNC && fn_is_numeric(a1->fn)))
                die_at(n->line, "FUNCTION TRIM takes an alphabetic, alphanumeric or national argument (2023 15.96.3 rule 1)");
            int mode = 0;
            if (accept_word("leading")) mode = 1; else if (accept_word("trailing")) mode = 2;
            unsigned char chars[64]; int nch = 0;
            while (cur()->kind != T_RP) {
                Tok *c = cur();
                if (c->kind != T_STR || c->nat != nat || c->boolv || c->len != (nat ? 2 : 1))
                    die_at(c->line, "FUNCTION TRIM: each character to delete is one %s character, a literal here (2023 15.96.3 rule 2)", nat ? "national" : "alphanumeric");
                if (nch + c->len > (int)sizeof chars) die_at(c->line, "FUNCTION TRIM: too many characters to delete");
                memcpy(chars + nch, c->s, (size_t)c->len); nch += c->len;
                advance();
            }
            advance();
            memset(o, 0, sizeof *o); o->kind = O_FUNC; o->fn = FN_TRIM; o->farg = a1; o->line = n->line;
            o->fvar = 1; o->fnat = nat;
            o->fsize = a1->kind == O_FUNC ? a1->fsize : opnd_size(a1);
            if (o->fsize < 1) o->fsize = 1;
            o->fnid = mode;
            if (nch) {
                Tok *t = xmalloc(sizeof *t); memset(t, 0, sizeof *t);
                t->kind = T_STR; t->s = xmalloc((size_t)nch + 1); memcpy(t->s, chars, (size_t)nch); t->s[nch] = 0; t->len = nch;
                o->ftrim = t;
            }
            return;
        }
        else if (!strcmp(n->s, "boolean-of-integer") || !strcmp(n->s, "integer-of-boolean")) {
            /* COBOL 2002 15.13, 15.45 (cobol ISSUES-76) */
            if (g_std < 2002) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
            int boi = !strcmp(n->s, "boolean-of-integer");
            advance();
            if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION %s", n->s);
            advance();
            Opnd *a1 = xmalloc(sizeof *a1); parse_operand(a1);
            Opnd *a2 = NULL;
            if (boi) { a2 = xmalloc(sizeof *a2); parse_operand(a2); }
            if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the arguments of FUNCTION %s", n->s);
            advance();
            memset(o, 0, sizeof *o); o->kind = O_FUNC; o->farg = a1; o->farg2 = a2; o->line = n->line;
            if (boi) {
                for (Opnd *x = a1; x; x = x == a1 ? a2 : NULL)
                    if (!((x->kind == O_NUM && numlit_is_int(&x->num) && !x->num.neg) || (x->kind == O_REF && is_int_item(x->ref.sym))))
                        die_at(n->line, "FUNCTION BOOLEAN-OF-INTEGER takes two positive integers (15.13.3)");
                o->fn = FN_BOOLOFINT; o->fbool = 1;
                if (a2->kind == O_NUM) {
                    long long len = numlit_int(&a2->num);
                    if (len < 1 || len > 8190) die_at(n->line, "FUNCTION BOOLEAN-OF-INTEGER: a length of %lld boolean positions (1 to 8190 here)", len);
                    o->fsize = (int)len;
                } else { o->fvar = 1; o->fsize = 8190; }
            } else {
                if (!opnd_is_boolean(a1) || a1->kind == O_ALL)
                    die_at(n->line, "FUNCTION INTEGER-OF-BOOLEAN takes a boolean argument (15.45.3)");
                o->fn = FN_INTOFBOOL; o->fsize = 18;
            }
            return;
        }
        else if (!strcmp(n->s, "exception-status") || !strcmp(n->s, "exception-statement")) {
            /* COBOL 2002 15.32-15.33: the last exception status */
            if (g_std < 2002) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
            int st = !strcmp(n->s, "exception-status");
            advance();
            o->kind = O_FUNC; o->fn = st ? FN_EXCSTATUS : FN_EXCSTMT; o->fsize = st ? 31 : 63;
            return;
        }
        else if (!strcmp(n->s, "exception-file") || !strcmp(n->s, "exception-file-n") ||
                 !strcmp(n->s, "exception-location") || !strcmp(n->s, "exception-location-n")) {
            /* COBOL 2002 15.23-15.26: as long as their contents (cobol ISSUES-65) */
            if (g_std < 2002) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
            int file = !strncmp(n->s, "exception-file", 14), nat = n->s[strlen(n->s) - 2] == '-';
            advance();
            o->kind = O_FUNC; o->fn = file ? FN_EXCFILE : FN_EXCLOC; o->fvar = 1; o->fnat = nat; o->fnid = nat;
            o->fsize = (file ? 2 + 64 : 255) * (nat ? 2 : 1);   /* the file-name, the location string: their bounds */
            return;
        }
        else if (!strcmp(n->s, "current-date")) {
            advance();
            o->kind = O_FUNC; o->fn = FN_CURDATE; o->fsize = 21;
            return;
        } else if (!strcmp(n->s, "integer-of-date") || !strcmp(n->s, "date-of-integer") ||
                   !strcmp(n->s, "day-of-integer") || !strcmp(n->s, "integer-of-day")) {
            int fn = !strcmp(n->s, "integer-of-date") ? FN_INTDATE : !strcmp(n->s, "date-of-integer") ? FN_DATEINT
                   : !strcmp(n->s, "day-of-integer") ? FN_DAYINT : FN_INTDAY;
            advance();
            if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION %s", n->s);
            advance();
            o->farg = fn89_arg(n->s);             /* an integer: an arithmetic expression too (15.3 rule 6) */
            fn_arg_check(o->farg, 'I', n->s, 1, n->line);
            if (cur()->kind != T_RP) { fn_phrase_nyi(n->s); die_at(cur()->line, "expected ')' after the function argument"); }
            advance();
            o->kind = O_FUNC; o->fn = fn;
            o->fsize = fn == FN_DATEINT ? 8 : fn == FN_DAYINT ? 7 : 10;   /* DISPLAYed directly: yyyymmdd, yyyyddd, or ten digits, as GnuCOBOL shows them */
            return;
        } else if (fn89_parse(o, n)) {
            return;
        } else if (!strcmp(n->s, "length")) {
            /* known at compile time, except for a variable reference modification */
            advance();
            if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION LENGTH");
            advance();
            Opnd x; parse_operand(&x);
            if (cur()->kind != T_RP) { fn_phrase_nyi(n->s); die_at(cur()->line, "expected ')' after the function argument"); }
            advance();
            if (x.kind != O_REF && x.kind != O_STR && x.kind != O_FUNC) die_at(n->line, "FUNCTION LENGTH takes an item or a literal");
            if (x.kind == O_FUNC && x.fvar) {       /* the length of a result known only at run time */
                Opnd *fx = xmalloc(sizeof *fx); *fx = x;
                memset(o, 0, sizeof *o); o->kind = O_FUNC; o->fn = FN_VARLEN; o->farg = fx; o->fsize = 9;
                o->fnid = x.fnat;                   /* characters of a national result */
                o->line = n->line;
                return;
            }
            if (x.kind == O_REF && x.ref.rm && !x.ref.rm_len) {
                /* the part's length is computed: counted at run time, in
                 * character positions (cobol ISSUES-81) */
                Opnd *fx = xmalloc(sizeof *fx); *fx = x;
                memset(o, 0, sizeof *o); o->kind = O_FUNC; o->fn = FN_RMLEN; o->farg = fx; o->fsize = 9;
                o->fnid = x.ref.rm_nat; o->line = n->line;
                return;
            }
            int len = opnd_size(&x);
            if (opnd_is_national(&x) || (x.kind == O_REF && x.ref.sym->usage == U_NATIONAL))
                len /= 2;                               /* national: character positions, two bytes each */
            if (x.kind == O_REF && !x.ref.rm && sym_bitlike(x.ref.sym))
                len = x.ref.sym->bits;                  /* bits: boolean positions */
            if (x.kind == O_REF && x.ref.rm_bit && x.ref.rm_len) len = (int)x.ref.rm_len;   /* a bit part or element */
            o->kind = O_NUM; numlit_from_int(&o->num, len); o->folded = 1; o->uc = x.uc;   /* a user function's call is still made */
            return;
        }
        else if (!strcmp(n->s, "byte-length") || !strcmp(n->s, "highest-algebraic") || !strcmp(n->s, "lowest-algebraic")) {
            /* COBOL 2002, known from the argument's description at compile time */
            if (g_std < 2002) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
            int bytes = !strcmp(n->s, "byte-length"), high = !strcmp(n->s, "highest-algebraic");
            advance();
            if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION %s", n->s);
            advance();
            Opnd x; parse_operand(&x);
            if (cur()->kind != T_RP) { fn_phrase_nyi(n->s); die_at(cur()->line, "expected ')' after the function argument"); }
            advance();
            if (bytes) {
                if (x.kind != O_REF && x.kind != O_STR && x.kind != O_FUNC) die_at(n->line, "FUNCTION BYTE-LENGTH takes an item or a literal");
                if (x.kind == O_FUNC && x.fvar) {
                    Opnd *fx = xmalloc(sizeof *fx); *fx = x;
                    memset(o, 0, sizeof *o); o->kind = O_FUNC; o->fn = FN_VARLEN; o->farg = fx; o->fsize = 9; o->fnid = 0;
                    o->line = n->line;
                    return;
                }
                if (x.kind == O_REF && x.ref.rm && !x.ref.rm_len && !x.ref.rm_bit) {
                    /* a computed part, an ANY LENGTH item's among them: its
                     * bytes, counted at run time as LENGTH counts characters */
                    Opnd *fx = xmalloc(sizeof *fx); *fx = x;
                    memset(o, 0, sizeof *o); o->kind = O_FUNC; o->fn = FN_RMLEN; o->farg = fx; o->fsize = 9;
                    o->fnid = 0; o->line = n->line;
                    return;
                }
                int len = opnd_size(&x);
                if (len < 0) die_at(n->line, "FUNCTION BYTE-LENGTH of a reference modification with a variable length is not implemented");
                o->kind = O_NUM; numlit_from_int(&o->num, len); o->folded = 1; o->uc = x.uc;   /* a user function's call is still made */
                return;
            }
            algebraic_limit(o, &x, high, n);
            return;
        }
        else fn_refuse(n);
        advance();
        if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION %s", n->s);
        advance();
        o->farg = xmalloc(sizeof *o->farg);
        parse_operand(o->farg);
        if (o->farg->kind != O_REF && o->farg->kind != O_STR && o->farg->kind != O_FUNC)
            die_at(n->line, "FUNCTION %s takes an alphanumeric item or literal", n->s);
        fn_arg_check(o->farg, 'A', n->s, 1, n->line);
        if (cur()->kind != T_RP) { fn_phrase_nyi(n->s); die_at(cur()->line, "expected ')' after the function argument"); }
        advance();
        o->kind = O_FUNC;
        Opnd *a = o->farg;
        int rmvar = a->kind == O_REF && a->ref.rm && ref_static_len(&a->ref) <= 0;   /* a part of computed length: a result of run-time length */
        o->fsize = rmvar ? (int)a->ref.sym->size
                 : a->kind == O_REF ? (a->ref.rm ? ref_static_len(&a->ref) : (int)a->ref.sym->size)
                 : a->kind == O_FUNC ? a->fsize : a->tok->len;
        /* a national argument, a national result (2002 15.78, 15.52); an
         * argument of run-time length, a result of the same length */
        o->fnat = opnd_is_national(a);
        o->fvar = (a->kind == O_FUNC && a->fvar) || rmvar;
        return;
    }
    if (t->kind == T_STR) { o->kind = O_STR; o->tok = t; advance(); return; }
    if (t->kind == T_NUM) { o->kind = O_NUM; numlit_parse(t, &o->num); advance(); return; }
    if (t->kind == T_WORD && is_figurative(t->s) && !(!strncmp(t->s, "null", 4) && sym_lookup_quiet(t->s))) { o->kind = O_FIG; o->tok = t; advance(); return; }   /* NULL is not an 85 word: a program of that era may name an item so (NIST SQL dml063) */
    if (t->kind == T_WORD && !strcmp(t->s, "all")) {
        advance();
        if (cur()->kind == T_STR) { o->kind = O_ALL; o->tok = cur(); advance(); return; }
        if (cur()->kind == T_WORD && is_figurative(cur()->s)) { o->kind = O_FIG; o->tok = cur(); o->allfig = 1; advance(); return; }
        if (cur()->kind == T_WORD && !strcmp(cur()->s, "all")) die_at(t->line, "ALL takes a literal, not a figurative constant (2023 8.3.3.6.3 rule 2)");
        die_at(t->line, "expected a literal after ALL");
    }
    o->kind = O_REF;
    parse_ref(&o->ref);
}

static int ref_needs_call(const Ref *r)
{
    for (int i = 0; i < r->nsub; i++)
        if (r->sub[i].sym && !is_hot_int(r->sub[i].sym)) return 1;
    if (r->rm && !r->rm_start) return 1;           /* the start is an expression */
    if (ec_on_name("EC-BOUND-ODO") && odo_table_for(r->sym)) return 1;   /* the check loads the DEPENDING ON item */
    return 0;
}

/* the literal length of a reference-modified item, or -1 when it is
 * only known at run time */
static int ref_static_len(const Ref *r)
{
    if (g_cen_on && !g_cen_quiet) cen_pin(r->sym, "length");    /* how many bytes it has is asked: as it is written */
    if (!r->rm) return r->sym->size;
    if (r->rm_bit) return r->rm_start ? (int)((r->sym->bitoff + r->rm_start - 1) % 8 + r->rm_len + 7) / 8   /* the bytes the bits span */
                                      : (int)(r->rm_len + 7) / 8 + 1;                                          /* at most, from a computed bit */
    return r->rm_len ? (int)r->rm_len * (r->rm_nat ? 2 : 1) : -1;
}

/* load the integer value of a hot item at address in areg into dreg */
static void emit_display_decode(int n, const char *areg, const char *dreg);
static void emit_display_encode(int n, const char *areg, const char *vreg);
static int is_display_int(Sym *s);
static int opnd_display_int(Opnd *o);

/* A big-endian (COMP) item.  SLOW-32 has no byte swap, and the only
 * scratch is r2, as emit_display_decode has it: emit_ref_addr's subscript
 * path loads with areg == dreg == r1 while emit_args may hold arguments in
 * r3 and up.  So the address is used up first and the bytes are put right
 * in registers: for a word, b0 into the top, the other three below it
 * reversed ([b0 b3 b2 b1]), then b1 and b3 exchanged by xor. */
static void emit_load_int_1(Sym *s, const char *areg, const char *dreg);
static void emit_load_int(Sym *s, const char *areg, const char *dreg)
{
    /* an item at a constant address: marked, for loopreg.h -- a binary
     * one, or an unsigned DISPLAY integer, whose value is its digits (the
     * stores of those are not marked: what they leave is the store's own
     * business, and a register is not trusted to follow it) */
    int m = mark_unit('L', (int)(s - g_sym), dreg, areg);
    cen_valued(s, areg);
    emit_load_int_1(s, areg, dreg);
    mark_end(m);
}
static void emit_load_int_1(Sym *s, const char *areg, const char *dreg)
{
    if (is_display_int(s)) { emit_display_decode(s->pi.digits, areg, dreg); return; }
    int sg = s->pi.is_signed;
    if (sym_be(s) && s->size > 1) {
        if (!strcmp(areg, "r2") || !strcmp(dreg, "r2") || (s->size != 2 && s->size != 4))
            die_at(s->line, "internal: emit_load_int of a big-endian item: registers or size");
        if (s->size == 2) {
            emit("\tldbu r2, %s+1", areg);
            emit("\tldbu %s, %s+0", dreg, areg);
            emit("\tslli %s, %s, %d", dreg, dreg, sg ? 24 : 8);
            if (sg) emit("\tsrai %s, %s, 16", dreg, dreg);
            emit("\tor %s, %s, r2", dreg, dreg);
            return;
        }
        emit("\tldw r2, %s+0", areg);                 /* b0 | b1<<8 | b2<<16 | b3<<24 */
        emit("\tslli %s, r2, 24", dreg);
        emit("\tsrli r2, r2, 8");
        emit("\tor %s, %s, r2", dreg, dreg);        /* [b0 b3 b2 b1] */
        emit("\tsrli r2, %s, 16", dreg);
        emit("\txor r2, r2, %s", dreg);
        emit("\tandi r2, r2, 255");                 /* b1 ^ b3 */
        emit("\txor %s, %s, r2", dreg, dreg);
        emit("\tslli r2, r2, 16");
        emit("\txor %s, %s, r2", dreg, dreg);        /* [b0 b1 b2 b3] */
        return;
    }
    if (s->size == 1) emit("\t%s %s, %s+0", sg ? "ldb" : "ldbu", dreg, areg);
    else if (s->size == 2) emit("\t%s %s, %s+0", sg ? "ldh" : "ldhu", dreg, areg);
    else emit("\tldw %s, %s+0", dreg, areg);
}

static void emit_store_int_1(Sym *s, const char *areg, const char *vreg);
static void emit_store_int(Sym *s, const char *areg, const char *vreg)
{
    int m = is_display_int(s) ? 0 : mark_unit('S', (int)(s - g_sym), vreg, areg);
    cen_valued(s, areg);
    emit_store_int_1(s, areg, vreg);
    mark_end(m);
}
static void emit_store_int_1(Sym *s, const char *areg, const char *vreg)
{
    if (is_display_int(s)) { emit_display_encode(s->pi.digits, areg, vreg); return; }
    if (sym_be(s) && s->size > 1) {             /* big-endian: the low byte last; vreg kept */
        emit("\tstb %s+%d, %s", areg, s->size - 1, vreg);
        for (int i = 1; i < s->size; i++) {
            emit("\tsrli r2, %s, %d", vreg, 8 * i);
            emit("\tstb %s+%d, r2", areg, s->size - 1 - i);
        }
        return;
    }
    if (s->size == 1) emit("\tstb %s+0, %s", areg, vreg);
    else if (s->size == 2) emit("\tsth %s+0, %s", areg, vreg);
    else emit("\tstw %s+0, %s", areg, vreg);
}

/* reg = address of item s plus off: WORKING-STORAGE by label, a LINKAGE
 * item through its cell, which the entry sequence filled from the
 * caller's argument register (LOCAL-STORAGE and EXTERNAL likewise) */
static void emit_item_addr(const char *reg, Sym *s, int off)
{
    Sym *rec = &g_sym[s->record];
    if (rec->ftemp_scan && !g_noemit)
        die_at(rec->line, "internal: a user function's result from a scan-ahead was used without its call (a statement keeps scanned operands)");
    if (!rec_indirect(rec)) {
        emit_la_off(reg, rec->label, off);
        /* a constant address: noted, for the marks (emit.h) -- when it is
         * the item's own and not an element's or a part's */
        g_la.sym = off == s->offset ? (int)(s - g_sym) : -1; g_la.off = off;
        snprintf(g_la.reg, sizeof g_la.reg, "%s", reg);
        cen_formed(s, reg);
        return;
    }
    emit_la(reg, rec->label);
    emit("\tldw %s, %s+0", reg, reg);
    if (rec->param_opt && strcmp(g_cur_stmt, "CALL") && ec_on_name("EC-PROGRAM-ARG-OMITTED")) {
        /* an omitted parameter referenced, not as an argument (2023 14.9.4
         * GR 12) */
        int Lok = new_label();
        emit("\tbne %s, r0, .L%d", reg, Lok);
        emit_ec_raise(ec_find("EC-PROGRAM-ARG-OMITTED", 0));
        emit_label(Lok);
    }
    if (rec->is_based && ec_on_name("EC-DATA-PTR-NULL")) {
        /* a based item referenced while its address is NULL (2002 13.16.5 GR 3) */
        int Lok = new_label();
        emit("\tbne %s, r0, .L%d", reg, Lok);
        emit_ec_raise(ec_find("EC-DATA-PTR-NULL", 0));
        emit_label(Lok);
    }
    if (off >= -2048 && off <= 2047) { if (off) emit("\taddi %s, %s, %d", reg, reg, off); }
    else { emit_li("r2", off); emit("\tadd %s, %s, r2", reg, reg); }
    cen_formed(s, reg);
}

static int ref_has_runtime_sub(const Ref *r)
{
    for (int i = 0; i < r->nsub; i++) if (r->sub[i].sym) return 1;
    if (r->rm && !r->rm_start) return 1;
    return 0;
}

/* reg = address of the reference.  Literal subscripts fold into the
 * displacement.  Runtime subscripts accumulate in r11 (callee-saved, so a
 * cob_load_int call for a DISPLAY-numeric subscript does not lose the
 * sum); r1/r2 are scratch.  A reference whose subscript needs that call
 * clobbers r3-r10, so callers stage such operands through frame slots
 * (emit_args) before loading argument registers. */
/* EC-BOUND-ODO (2023 13.18.38 general rule 7; cobol ISSUES-61): a
 * reference to an OCCURS DEPENDING ON table, to an item in it, or to a
 * group holding it, needs the DEPENDING ON value within the OCCURS
 * bounds.  Checked before the address is formed, with checking on. */
static Sym *odo_table_for(Sym *s)
{
    Sym *t = NULL;
    for (Sym *k = s; k && !t; k = k->parent >= 0 ? &g_sym[k->parent] : NULL) if (k->odo_dep_sym) t = k;
    if (!t && s->is_group) t = odo_table_below(s);
    return t && t->odo_dep_sym ? t : NULL;
}

static void emit_odo_check(Sym *s)
{
    Sym *t = odo_table_for(s);
    if (!t) return;
    Sym *d = t->odo_dep_sym;
    int Lok = new_label();
    if (is_hot_int(d)) { emit_item_addr("r1", d, d->offset); emit_load_int(d, "r1", "r1"); }
    else { emit_item_addr("r3", d, d->offset); emit_desc_addr("r4", sym_desc(d)); emit_call("cob_load_int"); }
    if (t->odo_min) emit("\taddi r1, r1, %d", -t->odo_min);
    emit_li("r2", t->occurs - t->odo_min + 1);
    emit("\tbltu r1, r2, .L%d", Lok);
    emit_ec_raise(ec_find("EC-BOUND-ODO", 0));
    emit_label(Lok);
}

/* r1 = where a bit array element's part begins, as a bit position in
 * the array from 1: (i - 1) * bits + the start within the element, the
 * subscript and the start either computed (cobol ISSUES-84, -93).  Worked
 * on the numeric stack, which leaves r11 alone.  chk: the length as
 * emit_refmod_check takes it, for a computed start under EC-BOUND-REF-MOD. */
/* a reference modification's computed position, off the numeric stack
 * into r1: with EC-BOUND-REF-MOD checked, a value that is not an integer
 * is noted for the bound check to raise (8.4.3.3.4 rule 5; cobol
 * ISSUES-94 E17) */
static void emit_pop_pos(void) { emit_call(ec_on_name("EC-BOUND-REF-MOD") ? "cob_pop_pos" : "cob_pop_int"); }
/* r1 = a position or count expression's value: on the wide stack when it
 * holds a 31-digit operand or a float (in double, the float then rounded
 * to the nearest integer, as Micro Focus has it for subscripts and
 * reference modification) -- the narrow stack would refuse either */
/* In two halves for emit_ref_addr: the value pushed, and popped later.
 * A start expression's own operands may be subscripted, and addressing
 * them uses r11, the register the outer reference's offset accumulates
 * in -- so the start is evaluated before that begins, and waits on the
 * numeric stack (a subscript's cob_load_int leaves the stack alone). */
/* A position the integer register path takes (pos_reg_kind,
 * arith_reg.h: integer items and literals, every intermediate in a word)
 * is not pushed on the numeric stack: it is computed into r1 where it is
 * taken -- or, when one of its operands is itself subscripted and so
 * needs r11, computed here, before the reference's offset begins, and
 * kept in a frame slot until it is taken.  a(i + 1:n) made two runtime
 * calls and a fetch through the stack for each of i + 1 and n. */
static int g_pos_was, g_pos_wasf;
static Expr *g_pos_reg;                 /* the position held is this expression, to compute when taken ... */
static int g_pos_slot = -1;             /* ... or its value, in this frame slot */
static int pos_reg_kind(Expr *e);
static void pos_reg_emit(Expr *e);
static int pos_reg_early(Expr *e);      /* arith_reg.h: computed first, kept in a frame slot ... */
static void pos_reg_take(int slot);     /* ... and taken */
static void emit_expr_pos_push(Expr *e)
{
    int was = g_wide, wasf = g_fstmt, kind = pos_reg_kind(e);
    if (kind == 1) { g_pos_was = was; g_pos_wasf = wasf; g_pos_reg = e; g_pos_slot = -1; return; }
    if (kind == 2) { int t = pos_reg_early(e); g_pos_was = was; g_pos_wasf = wasf; g_pos_reg = e; g_pos_slot = t; return; }
    if (e->wide || e->flt) { g_wide = 1; if (e->flt) g_fstmt = 1; }
    emit_expr(e);
    g_pos_was = was; g_pos_wasf = wasf; g_pos_reg = NULL; g_pos_slot = -1;   /* after the expression's own references have used them */
}
static void emit_expr_pos_pop(void)
{
    if (g_pos_reg) {
        Expr *e = g_pos_reg; int t = g_pos_slot;
        g_pos_reg = NULL; g_pos_slot = -1;
        if (t >= 0) pos_reg_take(t); else pos_reg_emit(e);
        return;
    }
    emit_pop_pos();
    g_wide = g_pos_was; g_fstmt = g_pos_wasf;
}
static void emit_expr_pos(Expr *e)
{
    emit_expr_pos_push(e);
    emit_expr_pos_pop();
}

/* pushed: the start expression is already on the numeric stack
 * (emit_ref_addr evaluates it before its own offset begins) */
static void emit_bitelem_start(const Ref *r, long chk, int slot, int pushed)
{
    Sym *s = r->sym;
    int k = r->bitsub - 1;
    if (r->bitu_start) emit_li("r1", r->bitu_start);
    else {
        if (pushed) emit_expr_pos_pop(); else emit_expr_pos(r->rm_sx);
        if (ec_on_name("EC-BOUND-REF-MOD")) emit_refmod_check(r, chk, slot);
    }
    emit("	add r3, r1, r0"); emit("	srai r4, r1, 31"); emit_li("r5", 0); emit_call("cob_push_lit");
    if (!r->sub[k].sym) { emit_li("r3", (r->sub[k].lit - 1) * s->bitdim_stride); emit_li("r4", 0); emit_li("r5", 0); emit_call("cob_push_lit"); }
    else if (r->sub[k].sym == &g_subx) {
        /* an arithmetic-expression subscript: its integer value (evaluated
         * again here; its function calls were made once, before) */
        emit_expr_pos(r->sub[k].x);
        emit("\tadd r3, r1, r0"); emit("\tsrai r4, r1, 31"); emit_li("r5", 0); emit_call("cob_push_lit");
        emit_li("r3", -1); emit_li("r4", -1); emit_li("r5", 0); emit_call("cob_push_lit");
        emit_call("cob_nadd");
        emit_li("r3", s->bitdim_stride); emit_li("r4", 0); emit_li("r5", 0); emit_call("cob_push_lit");
        emit_call("cob_nmul");
    }
    else {
        Sym *ss = r->sub[k].sym;
        emit_incompat_sym(ss, r->line);
        emit_item_addr("r3", ss, ss->offset); emit_desc_addr("r4", sym_desc(ss)); emit_call("cob_push");
        emit_li("r3", r->sub[k].adj - 1); emit("	srai r4, r3, 31"); emit_li("r5", 0); emit_call("cob_push_lit");
        emit_call("cob_nadd");
        emit_li("r3", s->bitdim_stride); emit_li("r4", 0); emit_li("r5", 0); emit_call("cob_push_lit");
        emit_call("cob_nmul");
    }
    emit_call("cob_nadd");
    emit_call("cob_pop_int");
}

static void emit_ref_addr(const Ref *r, const char *reg)
{
    Sym *s = r->sym;
    if (ec_on_name("EC-BOUND-ODO")) emit_odo_check(s);
    int off = s->offset;
    int runtime = ref_has_runtime_sub(r);
    for (int i = 0; i < r->nsub; i++)
        if (!r->sub[i].sym) off += (int)(r->sub[i].lit - 1) * s->dim_stride[i];
    if (r->rm && r->rm_start) off += r->rm_bit ? (s->bitoff + (int)r->rm_start - 1) / 8 : ((int)r->rm_start - 1) * (r->rm_nat ? 2 : 1);
    /* a start expression first: its operands' addressing would clobber r11 */
    int rm_pushed = r->rm && !r->rm_start && !(r->bitsub && r->bitu_start);
    if (rm_pushed) emit_expr_pos_push(r->rm_sx);
    /* expression subscripts likewise, each evaluated to an integer and
     * left on the numeric stack -- last first, so they come off in order
     * -- above the start, which comes off after them */
    int pw = g_pos_was, pwf = g_pos_wasf, pslot = g_pos_slot; Expr *pr = g_pos_reg;
    int skind[MAXDIM], sslot[MAXDIM];           /* an expression subscript: how it is computed; its slot */
    for (int i = r->nsub - 1; i >= 0; i--) {
        skind[i] = 0; sslot[i] = -1;
        if (r->sub[i].sym != &g_subx) continue;
        skind[i] = pos_reg_kind(r->sub[i].x);
        if (skind[i] == 1) continue;                /* computed where it is used, below */
        if (skind[i] == 2) { sslot[i] = pos_reg_early(r->sub[i].x); continue; }
        int w = g_wide, f = g_fstmt, chk = ec_on_name("EC-BOUND-SUBSCRIPT");
        emit_expr_pos_push(r->sub[i].x);
        emit_call(chk ? "cob_pop_pos" : "cob_pop_int");
        g_wide = w; g_fstmt = f;
        if (chk) {
            /* a value that is not an integer is out of range (2023
             * 8.4.2.3.4 rule 2, as SET's rule 2a1) */
            int Lok = new_label();
            emit("\tadd r11, r1, r0");      /* r11: callee-saved, and this reference's sum not begun */
            emit_call("cob_pos_nonint");
            emit("\tbeq r1, r0, .L%d", Lok);
            emit_ec_raise(ec_find("EC-BOUND-SUBSCRIPT", 0));
            emit_label(Lok);
            emit("\tadd r1, r11, r0");
        }
        emit("\tadd r3, r1, r0"); emit("\tsrai r4, r1, 31"); emit_li("r5", 0); emit_call("cob_push_lit");
    }
    g_pos_was = pw; g_pos_wasf = pwf; g_pos_reg = pr; g_pos_slot = pslot;
    if (runtime) emit("\tadd r11, r0, r0");
    for (int i = 0; i < r->nsub; i++) {
        if (!r->sub[i].sym) continue;
        Sym *ss = r->sub[i].sym;
        if (ss == &g_subx) {
            /* in registers it is an integer: nothing to check but its range */
            if (skind[i] == 2) pos_reg_take(sslot[i]);
            else if (skind[i] == 1) pos_reg_emit(r->sub[i].x);
            else emit_call("cob_pop_int");
        } else if (is_hot_int(ss)) {
            emit_item_addr("r1", ss, ss->offset);
            emit_load_int(ss, "r1", "r1");
        } else if (is_display_int(ss) && !g_nohx) {
            /* an unsigned DISPLAY integer: its digits, in line (r3 the
             * address: this was a call, and its callers expect no less) */
            emit_incompat_sym(ss, r->line);
            emit_item_addr("r3", ss, ss->offset);
            emit_load_int(ss, "r3", "r1");
        } else {
            emit_incompat_sym(ss, r->line);         /* item identification reads it (14.6.13.2 rule 2) */
            emit_item_addr("r3", ss, ss->offset);
            emit_desc_addr("r4", sym_desc(ss));
            emit_call("cob_load_int");
        }
        long adj = r->sub[i].adj - 1;
        if (adj) emit("\taddi r1, r1, %ld", adj);
        if (ec_on_name("EC-BOUND-SUBSCRIPT")) {
            /* the occurrence number, now less one, must be below the
             * dimension's OCCURS maximum (2023 8.4.2.3.4 rule 2) */
            int Lok = new_label();
            emit_li("r2", s->dim_count[i]);
            emit("\tbltu r1, r2, .L%d", Lok);
            emit_ec_raise(ec_find("EC-BOUND-SUBSCRIPT", 0));
            emit_label(Lok);
        }
        emit_li("r2", s->dim_stride[i]);
        emit("\tmul r1, r1, r2");
        emit("\tadd r11, r11, r1");
    }
    if (r->rm && !r->rm_start && r->bitsub) {
        /* a bit array's element at a computed subscript, or its part at a
         * computed start: the byte holding its first bit (cobol ISSUES-84) */
        emit_bitelem_start(r, r->rm_len ? (long)r->rm_len : r->rm_lx ? -3 : -1, 0, rm_pushed);
        emit("\taddi r1, r1, %d", s->bitoff - 1);
        emit("\tsrai r1, r1, 3");
        emit("\tadd r11, r11, r1");
    } else if (r->rm && !r->rm_start) {
        /* the start expression, pushed above: off the numeric stack as an int */
        emit_expr_pos_pop();
        if (ec_on_name("EC-BOUND-REF-MOD")) emit_refmod_check(r, r->rm_len ? (long)r->rm_len : r->rm_lx ? -3 : -1, 0);
        if (r->rm_bit) {
            /* bits: the byte holding bitoff + start - 1 */
            emit("\taddi r1, r1, %d", s->bitoff - 1);
            emit("\tsrai r1, r1, 3");
            emit("\tadd r11, r11, r1");
            goto addr_done;
        }
        emit("\taddi r1, r1, -1");
        if (r->rm_nat) emit("\tadd r1, r1, r1");      /* characters to bytes */
        emit("\tadd r11, r11, r1");
    }
addr_done:
    emit_item_addr(reg, s, off);
    if (runtime) { emit("\tadd %s, %s, r11", reg, reg); g_la.sym = -1; if (g_cen_on && !r->rm) cen_reformed(s, reg); }     /* an element's address: not a constant */
    if (r->rm) g_la.sym = -1;                                                   /* a part's: not the item */
}

/* a data-pointer value (2023 8.4.3.11; 14.9.39 formats 7 and 10): ADDRESS
 * OF an item, a pointer item's content, or NULL */
/* a pointer operand's category (8.5.2): 0 not a pointer, 1 data-pointer,
 * 2 program-pointer, 3 function-pointer, -1 NULL (any) */
static int opnd_ptr_cat(const Opnd *o)
{
    if (o->kind == O_FIG && o->tok && !strcmp(o->tok->s, "null")) return -1;
    if (o->kind == O_ADDR) return o->paddr ? 1 + o->paddr : 1;
    if (o->kind == O_REF && !o->ref.sym->is_group && o->ref.sym->usage == U_POINTER)
        return o->ref.sym->uvar == UV_PPTR ? 2 : o->ref.sym->uvar == UV_FPTR ? 3 : 1;
    return 0;
}
/* the prototype a program- or function-pointer operand is restricted to, or "" */
static const char *opnd_ptr_proto(const Opnd *o)
{
    if (o->kind == O_ADDR) return o->pproto ? o->pproto : "";
    if (o->kind == O_REF) return o->ref.sym->ptr_proto;
    return "";
}
static const char *ptr_cat_name(int c) { return c == 2 ? "program-pointer" : c == 3 ? "function-pointer" : "data-pointer"; }

static int opnd_is_ptr(const Opnd *o)
{
    return o->kind == O_ADDR || (o->kind == O_REF && !o->ref.sym->is_group && o->ref.sym->usage == U_POINTER) ||
           (o->kind == O_FIG && !strncmp(o->tok->s, "null", 4));
}

static void emit_ptr_value(const Opnd *o, const char *reg)
{
    if (o->kind == O_FIG) { emit("\tadd %s, r0, r0", reg); return; }
    if (o->kind == O_REF) { emit_ref_addr(&o->ref, "r3"); emit("\tldw %s, r3+0", reg); return; }
    if (o->kind == O_ADDR && o->paddr == 1) {
        /* ADDRESS OF PROGRAM: the registry's entry for the name, as CALL
         * identifier finds it (a contained program by its scope); none:
         * NULL, and EC-PROGRAM-NOT-FOUND when checked (8.4.3.13.4 rule 4) */
        if (o->pname) { emit_la("r3", lit_label((const unsigned char *)o->pname, (int)strlen(o->pname))); emit_li("r4", (long)strlen(o->pname)); }
        else { emit_ref_addr(&o->ref, "r3"); emit_li("r4", o->ref.sym->size); }
        emit_li("r5", 0);
        if (g_any_nested) { char vis[32]; snprintf(vis, sizeof vis, ".Lvis%d", g_unit); emit_la("r6", vis); emit_call("cob_resolve_v"); }
        else emit_call("cob_resolve");
        if (ec_on_name("EC-PROGRAM-NOT-FOUND")) {
            int Lok = new_label();
            emit("\tbne r1, r0, .L%d", Lok);
            emit_ec_raise(ec_find("EC-PROGRAM-NOT-FOUND", 0));
            emit_label(Lok);
        }
        if (strcmp(reg, "r1")) emit("\tadd %s, r1, r0", reg);
        return;
    }
    const Ref *r = &o->ref;
    Sym *rec = &g_sym[r->sym->record];
    if (rec == r->sym && rec_indirect(rec) && !r->nsub && !r->rm) {
        /* a based or LINKAGE record's own address: its cell, NULL or not */
        emit_la(reg, rec->label);
        emit("\tldw %s, %s+0", reg, reg);
        return;
    }
    emit_ref_addr(r, reg);
}

/* a slot's columns: a national one's character positions (cobol ISSUES-92) */
static int sfield_cols(const SField *f) { return f->pi.category == PIC_NATIONAL ? f->pi.bytes / 2 : f->pi.bytes; }
static int part_desc(const Ref *r);
/* a reference-modified screen item: a part of fixed length, alphanumeric
 * or national -- the field reads and writes the part, not the item */
static void sfield_part(SField *f, const Ref *r, int line)
{
    if (r->rm_bit) {
        /* a bit item's part, or a bit array's element: a boolean field of
         * that many positions, moved to and from the bits as MOVE moves
         * them (literal positions: part_desc; a computed subscript or
         * start is refused) */
        if (!r->rm_start) die_at(line, "a bit item's part at a computed position in a screen item is not implemented");
        f->idesc = 1 + part_desc(r);
        return;
    }
    if (r->rm_lx || !r->rm_len) {
        /* a part of computed length (or to the item's end from a computed
         * start): its descriptor is its own, in .data, and the statement
         * stores the length into it before the screen is used
         * (emit_screen_dyn_fill), as an ANY LENGTH item's is set at entry */
        static int ndyn;
        Desc d; memset(&d, 0, sizeof d);
        d.cat = sym_is_boolean(r->sym) ? COB_BOOLEAN : r->rm_nat ? COB_NATIONAL : COB_ALNUM;
        d.usage = r->rm_nat ? COB_U_NATIONAL : COB_U_DISPLAY;
        d.size = (int)r->sym->size;
        d.anylen = (1 << 30) + ndyn++;
        f->idesc = 1 + desc_add(&d);
        f->dyn = 1; f->dynpart = 1;
        return;
    }
    f->idesc = 1 + part_desc(r);
}

/* a slot's reference, resolved once the data tree is complete: LINKAGE,
 * EXTERNAL and runtime subscripts make the slot dynamic; a literal
 * subscript folds into a static offset */
static void sfield_resolve(SField *f)
{
    if (f->item || f->kind == COB_SCR_VALUE || f->kind < 0 || !f->ref_tp) return;
    int save_tp = g_tp; g_tp = f->ref_tp;
    Ref rr; parse_ref(&rr);
    if (f->kind != COB_SCR_FROM) no_constrec_recv(&rr, f->kind == COB_SCR_TO ? "a screen item's TO" : "a screen item's USING");
    g_tp = save_tp;
    f->ref = xmalloc(sizeof *f->ref); *f->ref = rr;
    if (rr.rm) sfield_part(f, &rr, f->srcline);
    /* a national field and its item move as MOVE does: national text to a
     * national receiver only (2023 14.9.25.3 rule 3) */
    if (f->has_pic && f->pi.category != PIC_NATIONAL && sym_is_national(rr.sym) && f->kind != COB_SCR_TO)
        die_at(f->srcline, "'%s' is national: it is shown through a national field (PICTURE N), not this one", rr.sym->name);
    if (f->has_pic && f->pi.category == PIC_NATIONAL && !sym_is_national(rr.sym) && f->kind != COB_SCR_FROM)
        die_at(f->srcline, "'%s' is not national: a national field (PICTURE N) takes its input into a national item", rr.sym->name);
    f->item = rr.sym;
    if (rec_indirect(&g_sym[rr.sym->record]) || ref_has_runtime_sub(&rr))
        f->dyn = 1;
    long off = rr.sym->offset;
    for (int si = 0; si < rr.nsub; si++)
        if (!rr.sub[si].sym) off += (rr.sub[si].lit - 1) * rr.sym->dim_stride[si];
    if (rr.rm && rr.rm_start) off += rr.rm_bit ? (rr.sym->bitoff + rr.rm_start - 1) / 8 : (rr.rm_start - 1) * (rr.rm_nat ? 2 : 1);   /* a part at a literal start: its byte */
    f->stat_off = off;
}

/* the screen window's dynamic slots: each reference's address, stored
 * into the slot's cell before the runtime paints or focuses the window */
static void emit_dynpart_len(SField *f);
static void emit_screen_dyn_fill(Screen *sc, int first, int count)
{
    for (int k = first; k < first + count && k < sc->nf; k++) {
        SField *f = &sc->f[k];
        sfield_resolve(f);
        if (!f->dyn) continue;
        if (f->dynpart) emit_dynpart_len(f);    /* the part's length in bytes, into its descriptor (args.h) */
        emit_ref_addr(f->ref, "r1");
        char cell[48]; snprintf(cell, sizeof cell, ".Lsdyn%d_%d_%d", g_unit, (int)(sc - g_screens), k);
        emit_la("r2", cell);
        emit("\tstw r2+0, r1");
    }
}
