/* s32-cobc: constant entries.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- Constant entries (2002 13.9; 2023 13.10) --------------------------
 * 01 constant-name CONSTANT [IS GLOBAL] AS { literal | arithmetic-expression
 * | BYTE-LENGTH OF data-name | LENGTH OF data-name }.  General rule 1: the
 * effect is as if the literal were written where the constant-name is --
 * and so it is done: the rest of the program's tokens (a contained
 * program's too, when GLOBAL) have the name replaced by the literal, and a
 * PICTURE's repetition (n) by the integer (syntax rule 2).  Every place a
 * format takes a literal then takes the constant with no change of its
 * own.  A LENGTH OF constant has no value until the DATA DIVISION is laid
 * out: its tokens share a buffer filled then (const_fill), and one used
 * before that, in the DATA DIVISION itself, is refused.
 *
 * Compile-time arithmetic (2023 7.3.6, the implementor's to define): exact,
 * over fixed-point literals of at most 18 digits, as fractions; the
 * constant is an integer (GR 4), so a result that is not one is refused. */
typedef struct {
    char name[64];
    Tok val;                /* the literal, or an integer T_NUM */
    int global, line;
    Sym *lsym; int lbytes;  /* LENGTH OF / BYTE-LENGTH OF: resolved and filled by const_fill */
    char lname[64], lq[8][64]; int nlq, lline;
    char *lbuf;
    int fill_tp;            /* the token where it was defined; uses before g_tp at fill are refused */
    int unit;
} Const;
static Const *g_const; static int g_nconst, g_cconst;
static int *g_cdefer, *g_cdefer_ci; static int g_ncdefer, g_ccdefer;

static int const_level_tok(const Tok *t)
{
    if (t->kind != T_NUM) return 0;
    for (const char *p = t->s; *p; p++) if (!isdigit((unsigned char)*p)) return 0;
    int v = atoi(t->s);
    return (v >= 1 && v <= 49) || v == 66 || v == 77 || v == 78 || v == 88;
}

/* replace the constant's name over the rest of this program (and its
 * contained programs, GLOBAL) */
static void const_subst(int ci)
{
    Const *c = &g_const[ci];
    int depth = 0, in_proc = 0;
    int is_int = 0; long long iv = 0;
    if (c->val.kind == T_NUM && !c->lbuf) {
        NumLit n; numlit_parse(&c->val, &n);
        if (numlit_is_int(&n) && !n.neg && n.ndigits <= 9) { is_int = 1; iv = numlit_int(&n); }
    }
    size_t nl = strlen(c->name);
    for (int i = g_tp; i < g_ntok; i++) {
        Tok *t = &g_tok[i];
        if (t->kind == T_WORD) {
            if ((!strcmp(t->s, "end")) && i + 1 < g_ntok && (is_word(&g_tok[i + 1], "program") || is_word(&g_tok[i + 1], "function"))) {
                if (depth == 0) return;
                depth--; i += 2; continue;
            }
            if (!strcmp(t->s, "program-id") || !strcmp(t->s, "function-id")) { depth++; continue; }
            if (!strcmp(t->s, "procedure") && i + 1 < g_ntok && is_word(&g_tok[i + 1], "division") && depth == 0) in_proc = 1;
            if (strcmp(t->s, c->name) || (depth && !c->global)) continue;
            /* the name of another constant entry, the same one again (rule 9):
             * left for parse_constant_entry to compare */
            if (i + 1 < g_ntok && is_word(&g_tok[i + 1], "constant") && i > 0 && const_level_tok(&g_tok[i - 1])) continue;
            /* a data item of that name (a level number at an entry's start) */
            if (!in_proc && i > 1 && const_level_tok(&g_tok[i - 1]) && (g_tok[i - 2].kind == T_PERIOD))
                die_at(t->line, "'%s' is a constant-name (line %d); it cannot name a data item too", c->name, c->line);
            Tok u = c->val;
            u.line = t->line; u.file = t->file; u.dbg = t->dbg; u.after_comma = t->after_comma;
            u.orig = NULL; u.strong = 0;
            *t = u;
            if (c->lbuf) {
                if (g_ncdefer == g_ccdefer) {
                    g_ccdefer = g_ccdefer ? 2 * g_ccdefer : 64;
                    g_cdefer = xrealloc(g_cdefer, (size_t)g_ccdefer * sizeof *g_cdefer);
                    g_cdefer_ci = xrealloc(g_cdefer_ci, (size_t)g_ccdefer * sizeof *g_cdefer_ci);
                }
                g_cdefer[g_ncdefer] = i; g_cdefer_ci[g_ncdefer++] = ci;
            }
        } else if (t->kind == T_PIC && (depth == 0 || c->global)) {
            /* a repetition (name): the integer in its place */
            for (char *p = strchr(t->s, '('); p; p = strchr(p + 1, '(')) {
                if (strncasecmp(p + 1, c->name, nl) || p[1 + nl] != ')') continue;
                if (c->lbuf || !is_int || iv < 1) {
                    /* refused when the picture is parsed: one error, there */
                    if (g_ncpicbad < 64) { g_cpicbad[g_ncpicbad] = i; g_cpicbad_ci[g_ncpicbad++] = ci; }
                    continue;
                }
                char *ns = xmalloc(strlen(t->s) + 24);
                snprintf(ns, strlen(t->s) + 24, "%.*s(%lld)%s", (int)(p - t->s), t->s, iv, p + 1 + nl + 1);
                p = ns + (p - t->s);
                t->s = ns; t->len = (int)strlen(ns);
            }
        }
    }
}

/* compile-time arithmetic: a fraction, n / d, d > 0 */
typedef struct { long long n, d; } CtNum;
static long long ct_gcd(long long a, long long b) { if (a < 0) a = -a; while (b) { long long t = a % b; a = b; b = t; } return a ? a : 1; }
static CtNum ct_norm(CtNum v, int line)
{
    if (v.d == 0) die_at(line, "a division by zero in a constant entry's expression (2023 7.3.6.2 rule 1c)");
    if (v.d < 0) { v.n = -v.n; v.d = -v.d; }
    long long g = ct_gcd(v.n, v.d); v.n /= g; v.d /= g;
    return v;
}
static CtNum ct_op(CtNum a, int op, CtNum b, int line)
{
    __int128 n, d;
    switch (op) {
    case '+': n = (__int128)a.n * b.d + (__int128)b.n * a.d; d = (__int128)a.d * b.d; break;
    case '-': n = (__int128)a.n * b.d - (__int128)b.n * a.d; d = (__int128)a.d * b.d; break;
    case '*': n = (__int128)a.n * b.n; d = (__int128)a.d * b.d; break;
    default:  if (b.n == 0) die_at(line, "a division by zero in a constant entry's expression (2023 7.3.6.2 rule 1c)");
              n = (__int128)a.n * b.d; d = (__int128)a.d * b.n; break;
    }
    if (d < 0) { n = -n; d = -d; }
    __int128 x = n < 0 ? -n : n, y = d; while (y) { __int128 t = x % y; x = y; y = t; }
    if (x) { n /= x; d /= x; }
    const __int128 lim = (__int128)999999999999999999LL;
    if (n > lim || n < -lim || d > lim) die_at(line, "a constant entry's expression goes past 18 digits, compile-time arithmetic's limit here (2023 7.3.6.2 rule 2)");
    CtNum r = { (long long)n, (long long)d };
    return r;
}
static CtNum ct_expr(void);
static CtNum mf_expr(void);
static CtNum ct_factor(void)
{
    Tok *t = cur();
    if (t->kind == T_OP && (!strcmp(t->s, "+") || !strcmp(t->s, "-"))) {
        int neg = t->s[0] == '-'; advance();
        CtNum v = ct_factor(); if (neg) v.n = -v.n; return v;
    }
    if (t->kind == T_LP) {
        advance(); CtNum v = ct_expr();
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' in a constant entry's expression");
        advance(); return v;
    }
    if (t->kind == T_OP && !strcmp(t->s, "**")) die_at(t->line, "exponentiation is not allowed in a compile-time arithmetic expression (2023 7.3.6.2 rule 1a)");
    if (t->kind != T_NUM) die_at(t->line, "a constant entry's expression takes fixed-point numeric literals, not %s (2023 7.3.6.2 rule 1b)", tok_desc(t));
    NumLit n; numlit_parse(t, &n);
    if (n.ndigits > 18) die_at(t->line, "a literal of more than 18 digits in a constant entry's expression (2023 7.3.6.2 rule 2)");
    long long v = 0, d = 1;
    for (int i = 0; i < n.ndigits; i++) v = v * 10 + (n.digits[i] - '0');
    for (int i = 0; i < n.scale; i++) d *= 10;
    advance();
    CtNum r = { n.neg ? -v : v, d };
    return ct_norm(r, t->line);
}
static CtNum ct_term(void)
{
    CtNum v = ct_factor();
    for (;;) {
        Tok *t = cur();
        if (t->kind == T_OP && !strcmp(t->s, "**")) die_at(t->line, "exponentiation is not allowed in a compile-time arithmetic expression (2023 7.3.6.2 rule 1a)");
        if (t->kind != T_OP || (strcmp(t->s, "*") && strcmp(t->s, "/"))) return v;
        advance(); v = ct_op(v, t->s[0], ct_factor(), t->line);
    }
}
static CtNum ct_expr(void)
{
    CtNum v = ct_term();
    for (;;) {
        Tok *t = cur();
        if (t->kind != T_OP || (strcmp(t->s, "+") && strcmp(t->s, "-"))) return v;
        advance(); v = ct_op(v, t->s[0], ct_term(), t->line);
    }
}

/* Micro Focus's constant-expression (the VALUE clause's format 3, rules
 * 13-20): integers, combined strictly left to right -- every operator of
 * one precedence, parentheses first -- in 64-bit integer arithmetic, with
 * + - * / and the bitwise AND, OR, EXCLUSIVE OR and NOT; LENGTH or SIZE OF
 * a literal is its digits (sign and point not counted) or characters, a
 * figurative constant's 1 */
static long long mf_operand(void)
{
    Tok *t = cur();
    if (accept_word("not")) return ~mf_operand();
    if (t->kind == T_LP) {
        advance(); CtNum v = mf_expr();
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' in a level 78 VALUE");
        advance(); return v.n;
    }
    if ((at_word("length") || at_word("size")) && is_word(peek(1), "of")) {
        advance(); advance();
        Tok *l = cur();
        long long n = 0;
        if (l->kind == T_WORD && is_figurative(l->s)) n = 1;
        else if (l->kind == T_STR) n = l->nat ? l->len / 2 : l->len;
        else if (l->kind == T_NUM) { NumLit nl; numlit_parse(l, &nl); n = nl.ndigits; }
        else die_at(l->line, "LENGTH OF a data item inside a level 78 expression is not implemented; give it its own entry");
        advance(); return n;
    }
    if (t->kind != T_NUM) die_at(t->line, "a level 78 expression takes integers, not %s", tok_desc(t));
    NumLit n; numlit_parse(t, &n);
    if (!numlit_is_int(&n) || n.ndigits > 18) die_at(t->line, "a level 78 expression takes integers of at most 18 digits");
    advance();
    long long v = numlit_int(&n);
    return n.neg ? -v : v;
}
static CtNum mf_expr(void)
{
    long long v = mf_operand();
    for (;;) {
        Tok *t = cur(); int op = 0;
        if (t->kind == T_OP && strlen(t->s) == 1 && strchr("+-*/", t->s[0])) op = t->s[0];
        else if (at_word("and")) op = '&';
        else if (at_word("or")) op = '|';
        else if (at_word("exclusive") && is_word(peek(1), "or")) { op = '^'; advance(); }
        if (!op) break;
        advance();
        long long w = mf_operand();
        switch (op) {
        case '+': v = (long long)((unsigned long long)v + (unsigned long long)w); break;
        case '-': v = (long long)((unsigned long long)v - (unsigned long long)w); break;
        case '*': v = (long long)((unsigned long long)v * (unsigned long long)w); break;
        case '/': if (!w) die_at(t->line, "a division by zero in a level 78 VALUE"); v /= w; break;
        case '&': v &= w; break;
        case '|': v |= w; break;
        default:  v ^= w; break;
        }
    }
    CtNum r = { v, 1 };
    return r;
}

static void parse_constant_entry(int line)
{
    Tok *nt = cur();
    if (nt->kind != T_WORD) die_at(line, "expected a constant-name, found %s", tok_desc(nt));
    user_word(nt->s, line, "a constant");
    char name[64]; snprintf(name, sizeof name, "%s", nt->s);
    advance();
    int mf = g_entry_level == 78;
    Const c; memset(&c, 0, sizeof c);
    snprintf(c.name, sizeof c.name, "%s", name); c.line = line; c.unit = g_unit;
    if (mf) {
        /* 78 name VALUE [IS] constant-expression (Micro Focus: the VALUE
         * clause's format 3) */
        if (!accept_word("value")) die_at(cur()->line, "a level 78 entry '%s' needs VALUE", name);
        accept_word("is");
    } else {
    expect_word("constant");
    if (accept_word("is")) { expect_word("global"); c.global = 1; }
    else if (accept_word("global")) c.global = 1;
    }
    if (!mf && !accept_word("as")) {
        if (at_word("from")) die_at(cur()->line, "'%s': CONSTANT ... FROM a compilation variable is COBOL 2023 (13.10); not implemented (it needs >>DEFINE)", name);
        if (cur()->kind != T_NUM && cur()->kind != T_STR) die_at(cur()->line, "expected AS in the constant entry of '%s'", name);
        bp(BP_E25_CONSTANT_NO_AS, cur()->line);
    }
    Tok *v = cur();
    char spec[256] = "";
    if (!mf && at_word("from")) die_at(v->line, "'%s': FROM compilation-variable-name needs >>DEFINE, which is not implemented", name);
    if (mf && (at_word("next") || at_word("start") || at_word("date-compiled") || at_word("true") || at_word("false") || v->boolv))
        die_at(v->line, "'%s': a level 78 VALUE of %s is not implemented", name, tok_desc(v));
    if (mf && (at_word("length") || at_word("size")) && is_word(peek(1), "of") &&
        (peek(2)->kind == T_STR || peek(2)->kind == T_NUM || (peek(2)->kind == T_WORD && is_figurative(peek(2)->s)))) {
        /* LENGTH OF a literal: an integer now, in an expression or alone */
        CtNum r = mf_expr();
        char *b = xmalloc(24); snprintf(b, 24, "%lld", r.n);
        memset(&c.val, 0, sizeof c.val); c.val.kind = T_NUM; c.val.s = b; c.val.len = (int)strlen(b);
        snprintf(spec, sizeof spec, "%d:%s", T_NUM, b);
    } else if ((at_word("length") || at_word("byte-length") || (mf && at_word("size"))) && !(mf && at_word("byte-length"))) {
        c.lbytes = at_word("byte-length") || mf;    /* Micro Focus's LENGTH is the storage's size */
        advance(); expect_word("of");
        /* data-name [OF|IN qualifier]... [(literal ...)]: by hand, as the
         * tables' dimensions are not settled until the DATA DIVISION is.
         * Every occurrence has one size, so the subscripts -- literals
         * (rule 3) -- change nothing, and the name alone is one element */
        const char *what = c.lbytes ? "BYTE-LENGTH OF" : "LENGTH OF";
        Tok *dn = cur();
        if (dn->kind != T_WORD) die_at(dn->line, "'%s': %s needs a data-name", name, what);
        advance();
        char *quals[64]; int nq = 0;
        while (accept_word("of") || accept_word("in")) {
            if (cur()->kind != T_WORD || nq == 64) die_at(cur()->line, "'%s': expected a qualifier after OF/IN", name);
            quals[nq++] = cur()->s; advance();
        }
        if (nq > 8) die_at(dn->line, "'%s': more than 8 qualifiers", name);
        snprintf(c.lname, sizeof c.lname, "%s", dn->s); c.nlq = nq; c.lline = dn->line;
        for (int i = 0; i < nq; i++) snprintf(c.lq[i], sizeof c.lq[i], "%s", quals[i]);
        if (cur()->kind == T_LP) {
            advance();
            while (cur()->kind == T_NUM) advance();
            if (cur()->kind != T_RP) die_at(cur()->line, "'%s': the subscripts of %s's data-name are integer literals (2023 13.10.3 rule 3)", name, what);
            advance();
        }
        c.lsym = NULL;
        c.lbuf = xmalloc(24); strcpy(c.lbuf, "1");
        memset(&c.val, 0, sizeof c.val); c.val.kind = T_NUM; c.val.s = c.lbuf; c.val.len = 1;
        snprintf(spec, sizeof spec, "%s %s", c.lbytes ? "byte-length" : "length", c.lname);
        for (int i = 0; i < nq; i++) { size_t l = strlen(spec); snprintf(spec + l, sizeof spec - l, " of %s", quals[i]); }
    } else if ((v->kind == T_STR || v->kind == T_NUM) && peek(1)->kind == T_PERIOD) {
        c.val = *v; advance();          /* rule 1: a single numeric literal is a literal */
        snprintf(spec, sizeof spec, "%d:%.*s", v->kind, v->len > 200 ? 200 : v->len, v->s);
    } else if (v->kind == T_WORD && is_figurative(v->s)) {
        die_at(v->line, "'%s': a figurative constant is no constant entry's literal (2023 13.10.3 rule 6)", name);
    } else if (mf) {
        CtNum r = mf_expr();
        char *b = xmalloc(24); snprintf(b, 24, "%lld", r.n);
        memset(&c.val, 0, sizeof c.val); c.val.kind = T_NUM; c.val.s = b; c.val.len = (int)strlen(b);
        snprintf(spec, sizeof spec, "%d:%s", T_NUM, b);
    } else {
        CtNum r = ct_expr();
        if (r.d != 1) die_at(v->line, "'%s': the expression's value is not an integer (%lld/%lld); a constant entry's expression gives an integer (2023 13.10.4 rule 4)", name, r.n, r.d);
        char *b = xmalloc(24); snprintf(b, 24, "%lld", r.n);
        memset(&c.val, 0, sizeof c.val); c.val.kind = T_NUM; c.val.s = b; c.val.len = (int)strlen(b);
        snprintf(spec, sizeof spec, "%d:%s", T_NUM, b);
    }
    expect_period();
    /* the same name again, in this program: the same specification (rule 9) */
    for (int i = 0; i < g_nconst; i++) {
        Const *o = &g_const[i];
        if (o->unit != g_unit || strcmp(o->name, name)) continue;
        char ospec[256];
        if (o->lbuf) {
            snprintf(ospec, sizeof ospec, "%s %s", o->lbytes ? "byte-length" : "length", o->lname);
            for (int q = 0; q < o->nlq; q++) { size_t l = strlen(ospec); snprintf(ospec + l, sizeof ospec - l, " of %s", o->lq[q]); }
        } else snprintf(ospec, sizeof ospec, "%d:%.*s", o->val.kind, o->val.len > 200 ? 200 : o->val.len, o->val.s);
        if (strcmp(ospec, spec)) die_at(line, "'%s' is a constant-name already (line %d), with another value (2023 13.10.3 rule 9)", name, o->line);
        return;
    }
    if (sym_lookup_quiet(name)) die_at(line, "'%s' names a data item already; it cannot be a constant-name too", name);
    if (g_nconst == g_cconst) { g_cconst = g_cconst ? 2 * g_cconst : 32; g_const = xrealloc(g_const, (size_t)g_cconst * sizeof *g_const); }
    c.fill_tp = g_tp;
    g_const[g_nconst] = c;
    const_subst(g_nconst++);
}

/* a PICTURE at the cursor whose repetition names a constant that cannot
 * give one */
static void const_pic_check(void)
{
    for (int k = 0; k < g_ncpicbad; k++) {
        if (g_cpicbad[k] != g_tp) continue;
        Const *c = &g_const[g_cpicbad_ci[k]];
        if (c->lbuf) die_at(cur()->line, "'%s' is a LENGTH OF constant: its value is known only after the DATA DIVISION, so it cannot be a PICTURE repetition (not implemented)", c->name);
        die_at(cur()->line, "'%s' is not a positive integer constant; it cannot be a PICTURE repetition (2023 13.10.3 rule 2)", c->name);
    }
}

/* the LENGTH OF constants of this program, now that it is laid out */
static void const_fill(void)
{
    for (int i = 0; i < g_nconst; i++) {
        Const *c = &g_const[i];
        if (c->unit != g_unit || !c->lbuf) continue;
        char *quals[8]; for (int q = 0; q < c->nlq; q++) quals[q] = c->lq[q];
        Sym *s = c->lsym = sym_lookup(c->lname, quals, c->nlq, c->lline);
        if (s->is_cond || s->usage == U_BIT || s->bitgroup)
            die_at(c->lline, "'%s': %s a condition-name or a bit data item is not implemented", c->name, c->lbytes ? "BYTE-LENGTH OF" : "LENGTH OF");
        long v = s->size;
        if (!c->lbytes && (sym_is_national(s) || (!s->is_group && s->usage == U_NATIONAL))) v /= 2;
        snprintf(c->lbuf, 24, "%ld", v);
    }
    /* a PICTURE past this DATA DIVISION -- a contained program's -- that
     * repeats by a LENGTH OF constant: its value is known now */
    for (int k = 0; k < g_ncpicbad; k++) {
        Const *c = &g_const[g_cpicbad_ci[k]];
        if (g_cpicbad[k] < g_tp || c->unit != g_unit || !c->lbuf) continue;
        Tok *t = &g_tok[g_cpicbad[k]];
        size_t nl = strlen(c->name);
        for (char *p = strchr(t->s, '('); p; p = strchr(p + 1, '(')) {
            if (strncasecmp(p + 1, c->name, nl) || p[1 + nl] != ')') continue;
            char *ns = xmalloc(strlen(t->s) + 24);
            snprintf(ns, strlen(t->s) + 24, "%.*s(%s)%s", (int)(p - t->s), t->s, c->lbuf, p + 1 + nl + 1);
            p = ns + (p - t->s);
            t->s = ns; t->len = (int)strlen(ns);
        }
        g_cpicbad[k] = -1;
    }
    for (int k = 0; k < g_ncdefer; k++) {
        Const *c = &g_const[g_cdefer_ci[k]];
        if (c->unit == g_unit && g_cdefer[k] < g_tp)
            die_at(g_tok[g_cdefer[k]].line, "'%s' is a LENGTH OF constant: its value is known only after the DATA DIVISION, so it cannot be used in it (not implemented)", c->name);
    }
}
