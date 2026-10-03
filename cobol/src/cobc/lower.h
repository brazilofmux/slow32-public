/* s32-cobc: lowering to HIR (docs/plans/hir.md; cobol ISSUES-123, step 4).
 * A part of one translation unit, included by s32-cobc.c in order; not a
 * header to include anywhere else.
 *
 * A statement over native items (native.h) that this file takes leaves
 * one line in the text, "\tisland N", and a node of its own here.  At
 * the unit's end each run of such lines is an island: one function in
 * stage08's HIR (src/hir), compiled through its pipeline, its text
 * appended after the unit's return and called where the run was.  In
 * the island the items are allocas, which SSA construction makes
 * registers.  The arithmetic is the stack's, written out (see the plan
 * and arith_reg.h, whose bounds decide the widths). */

static int g_hir_on = 1;                /* -fno-hir, S32_HIR=0 */
static int lw_trace(void) { static int t = -1; if (t < 0) t = getenv("S32_HIR_TRACE") != NULL; return t; }
/* A run of statements is made an island when a loop is among them, or
 * when there are at least this many: a statement alone pays the call, the
 * prologue and the loads and stores of its items for arithmetic the text
 * emitter does in place (csv2fw's byte loop lost 6% to 52 such islands).
 * S32_HIR_MIN=n sets it; 0 is every run. */
static int lw_min(void) { static int m = -1; if (m < 0) { const char *e = getenv("S32_HIR_MIN"); m = e ? atoi(e) : 4; } return m; }

/* ---- the statement form ---------------------------------------------- */

/* a value: op 0 an item (sym), 'k' a literal (k at scale sc), + - * / n;
 * sc its scale, bd a bound on its magnitude in units of that scale, neg
 * whether it can be below zero */
typedef struct { char op; int l, r; int sym; long long k; int sc; long double bd; int neg; } LNode;
/* a condition: cond.h's C_AND, C_OR, C_NOT, C_REL (op R_*; x, y values) */
typedef struct { int kind; int a, b; int x, y; int op; } LCond;
enum { LS_STORE, LS_ADDTO, LS_IF, LS_LOOP };
typedef struct {
    int kind, line;
    int expr;                           /* LS_STORE: the value; LS_ADDTO: the sum each receiver takes */
    int need;                           /* LS_STORE with '/' at the root: fraction digits the quotient is made to */
    int nr; int rsym[MAXOPS]; unsigned char rnd[MAXOPS];
    int rem;                            /* LS_STORE of a quotient: the REMAINDER item, or -1 */
    int subtract;                       /* LS_ADDTO: SUBTRACT */
    int cond;                           /* LS_IF; LS_LOOP: UNTIL */
    int body, nbody, els, nels;         /* ranges of g_lw_list */
    int var, from, by, test_after;      /* LS_LOOP: the VARYING item (-1: none), FROM and BY values */
    Block text;                         /* the statement's code by the text emitter, for a run that is no island */
} LStmt;

static LNode *g_lw_n; static int g_lw_nn, g_lw_ncap;
static LCond *g_lw_c; static int g_lw_nc, g_lw_ccap;
static LStmt *g_lw_s; static int g_lw_ns, g_lw_scap;
static int *g_lw_list; static int g_lw_nlist, g_lw_lcap;
static int g_lw_nisland;                /* islands made so far, for their labels */

#define LW_GROW(arr, n, cap) do { if ((n) == (cap)) { (cap) = (cap) ? 2 * (cap) : 64; (arr) = xrealloc((arr), (size_t)(cap) * sizeof *(arr)); } } while (0)

static int lw_node(char op, int l, int r, int sym, long long k, int sc, long double bd, int neg)
{
    LW_GROW(g_lw_n, g_lw_nn, g_lw_ncap);
    LNode *x = &g_lw_n[g_lw_nn]; x->op = op; x->l = l; x->r = r; x->sym = sym; x->k = k; x->sc = sc; x->bd = bd; x->neg = neg;
    return g_lw_nn++;
}
static int lw_cnode(int kind, int a, int b, int x, int y, int op)
{
    LW_GROW(g_lw_c, g_lw_nc, g_lw_ccap);
    LCond *c = &g_lw_c[g_lw_nc]; c->kind = kind; c->a = a; c->b = b; c->x = x; c->y = y; c->op = op;
    return g_lw_nc++;
}
static int lw_stmt(int kind, int line)
{
    LW_GROW(g_lw_s, g_lw_ns, g_lw_scap);
    LStmt *s = &g_lw_s[g_lw_ns]; memset(s, 0, sizeof *s); s->kind = kind; s->line = line; s->var = -1; s->expr = s->cond = s->rem = -1;
    return g_lw_ns++;
}
static void lw_list_add(int st) { LW_GROW(g_lw_list, g_lw_nlist, g_lw_lcap); g_lw_list[g_lw_nlist++] = st; }

/* the statement's line in the text */
static void lw_place(int st) { emit("\tisland %d", st); }
static int lw_is_place(const char *l, int *st)
{
    if (strncmp(l, "\tisland ", 8)) return 0;
    char *e; long v = strtol(l + 8, &e, 10);
    if (*e) return 0;
    *st = (int)v;
    return 1;
}

static void lw_refuse(int line, const char *what, const char *why)
{
    if (lw_trace()) fprintf(stderr, "hir: line %d %s: not lowered: %s\n", line, what, why);
}

/* A lowered statement leaves its placeholder AND lets the text emitter
 * write its code after it; when the statement is over (parse_statement),
 * that code is cut away and kept with the node, for a run that is no
 * island.  The lines from b0: placeholders (after a -fprofile-lines
 * label), then the text. */
static void lw_stmt_text(int b0)
{
    int p = b0, st, last = -1;
    while (p < g_nasm && !strncmp(g_asm[p], "__ln_", 5)) p++;
    while (p < g_nasm && lw_is_place(g_asm[p], &st)) { last = st; p++; }
    if (last < 0 || p == g_nasm) return;
    Block t = block_cut(p);
    g_lw_s[last].text = t;
}
/* a block of placeholders as the text emitter would have written it:
 * each statement's kept text in its place */
static Block lw_expand(const Block *b)
{
    int b0 = block_begin();
    for (int i = 0; i < b->n; i++) {
        int st;
        if (lw_is_place(b->line[i], &st)) block_put(&g_lw_s[st].text);
        else emit("%s", b->line[i]);
    }
    return block_cut(b0);
}

/* ---- what is taken ---------------------------------------------------- */

/* every hook asks this first: the lowering is off, or this is a scan, or
 * the census is being taken (its reading of the text is the emitter's) */
static int lw_off(void) { return !g_hir_on || g_noemit || g_cen_on || g_fnsig_only || g_nerrors; }

/* an item the island may hold as a value: native, whole, not subscripted */
static int lw_item_ok(const Ref *r)
{
    const Sym *s = r->sym;
    if (!s->native || r->rm || r->nsub || s->ndims || s->is_group) return 0;
    if (s->pi.category != PIC_NUMERIC || s->pi.digits > 18 || s->pi.scale < 0 || strchr(s->pi.pat, 'P')) return 0;
    if (s->size != 1 && s->size != 2 && s->size != 4 && s->size != 8) return 0;
    if (rec_indirect(&g_sym[s->record])) return 0;
    return 1;
}
/* a leaf of a register tree (hn_tree): the item, a literal, ZERO */
static int lw_leaf(const Opnd *o)
{
    if (opnd_scanned(o)) return -2;
    if (o->kind == O_FIG) return !strncmp(o->tok->s, "zero", 4) ? hn_new(0, -1, -1, o) : -2;
    if (o->kind == O_NUM) return o->num.ndigits <= 18 && o->num.scale >= 0 && o->num.scale <= 18 ? hn_new(0, -1, -1, o) : -2;
    if (o->kind != O_REF || !lw_item_ok(&o->ref)) return -2;
    return hn_new(0, -1, -1, o);
}
/* the tree at h (g_hn, bounds in g_dsc/g_dbd from dx_check) as nodes;
 * -1 for an operation not taken */
static int lw_from_hn(int h)
{
    HNode *x = &g_hn[h];
    if (!x->op) {
        const Opnd *o = &x->o;
        if (o->kind == O_FIG) return lw_node('k', -1, -1, -1, 0, 0, 0, 0);
        if (o->kind == O_NUM) { long long v = numlit_scaled(&o->num); return lw_node('k', -1, -1, -1, v, o->num.scale, v < 0 ? -(long double)v : (long double)v, v < 0); }
        const Sym *s = o->ref.sym;
        return lw_node(0, -1, -1, (int)(s - g_sym), 0, s->pi.scale, g_dbd[h], s->pi.is_signed);
    }
    if (x->op == 'n') {
        int l = lw_from_hn(x->l);
        return l < 0 ? -1 : lw_node('n', l, -1, -1, 0, g_dsc[h], g_dbd[h], 1);
    }
    if (x->op != '+' && x->op != '-' && x->op != '*' && x->op != '/') return -1;
    int l = lw_from_hn(x->l); if (l < 0) return -1;
    int r = lw_from_hn(x->r); if (r < 0) return -1;
    int neg = x->op == '-' || g_lw_n[l].neg || g_lw_n[r].neg;
    return lw_node(x->op, l, r, -1, 0, g_dsc[h], g_dbd[h], neg);     /* '/': scale and bound are the store's to work out */
}
/* an expression's value as a node, its bounds checked: -1 when not taken */
static int lw_expr(Expr *e, int top_div)
{
    g_nhn = 0;
    int root = hn_tree(e, lw_leaf);
    if (root < 0) return -1;
    int chk = g_dx_chk; g_dx_chk = 0;
    int ok = dx_check(root, top_div);
    g_dx_chk = chk;
    if (!ok) return -1;
    return lw_from_hn(root);
}
static int lw_opnd(const Opnd *o)
{
    if (o->kind == O_EXPR) return lw_expr(o->ex, 0);
    g_nhn = 0;
    int h = lw_leaf(o);
    if (h < 0) return -1;
    int chk = g_dx_chk; g_dx_chk = 0;
    int ok = dx_check(h, 0);
    g_dx_chk = chk;
    return ok ? lw_from_hn(h) : -1;
}
/* the node x op y, by the register tree's rules (dx_check), -1 when it
 * does not take it */
static int lw_binop(char op, int x, int y, int top)
{
    if (x < 0 || y < 0) return -1;
    LNode *a = &g_lw_n[x], *b = &g_lw_n[y];
    if (op == '/') return top ? lw_node('/', x, y, -1, 0, -1, 0, a->neg || b->neg) : -1;
    if (op == '*') {
        if (a->sc + b->sc > 18) return -1;
        long double bd = a->bd * b->bd;
        if (bd >= DX_LIM) return -1;
        return lw_node('*', x, y, -1, 0, a->sc + b->sc, bd, a->neg || b->neg);
    }
    int sc = a->sc > b->sc ? a->sc : b->sc;
    long double bl = a->bd * dx_p10(sc - a->sc), br = b->bd * dx_p10(sc - b->sc);
    if (bl >= DX_LIM || br >= DX_LIM || bl + br >= DX_LIM) return -1;
    return lw_node(op, x, y, -1, 0, sc, bl + br, op == '-' || a->neg || b->neg);
}
static int lw_recv_ok(const Ref *r) { return lw_item_ok(r) && !ref_pending(r); }

/* a statement's standing refusals; what the verb is, for the trace */
static const char *lw_stmt_refused(int size_err)
{
    if (size_err) return "SIZE ERROR";
    if (g_rmode) return "ROUNDED MODE";
    if (g_wide) return "wide";
    if (g_nohx) return "-fno-hot-arith";
    if (ec_on_name("EC-DATA-INCOMPATIBLE")) return "EC-DATA-INCOMPATIBLE on";
    return NULL;
}

/* a division at the root: the quotient's scale and bound by the stack's
 * rule (cob_xdivn), or 0 when the rule's answer depends on the values */
static int lw_div_shape(int n, int need, int *e_out, int *sc_out, long double *bd_out)
{
    LNode *d = &g_lw_n[n], *a = &g_lw_n[d->l], *b = &g_lw_n[d->r];
    if (a->bd >= DX_LIM || b->bd >= DX_LIM) return 0;
    int want = (a->sc > b->sc ? a->sc : b->sc) + 6;
    if (want < 9) want = 9;
    if (want > 18) want = 18;
    if (want > need) want = need;
    int scale = a->sc - b->sc, e = want > scale ? want - scale : 0;
    long double bq = a->bd * dx_p10(e);             /* the divisor is at least one unit of its scale */
    if (bq >= dx_p10(17)) return 0;                 /* the stack would stop short of want */
    *e_out = e; *sc_out = scale + e; *bd_out = bq;
    return 1;
}

/* REMAINDER r of a division into q: the dividend less the divisor times
 * the quotient truncated to q's decimals (X3.23 6.9.4; arith_reg.h
 * emit_remainder), the quotient the stack's (cob_ndiv, no receiver
 * limiting it).  Its scale is fixed when the stack's digits reach it
 * before the quotient fills 17 digits; the product and the difference
 * must stay in 64 bits.  *e2: digits the dividend is scaled by for that
 * quotient; *down: digits the quotient is then cut by; *sr: the
 * remainder's scale; *br its bound. */
static int lw_rem_shape(int n, const Sym *q, int *e2, int *down, int *sr, long double *br)
{
    LNode *d = &g_lw_n[n], *a = &g_lw_n[d->l], *b = &g_lw_n[d->r];
    int want = (a->sc > b->sc ? a->sc : b->sc) + 6;
    if (want < 9) want = 9;
    if (want > 18) want = 18;
    int qs = q->pi.scale < want ? (q->pi.scale > 0 ? q->pi.scale : 0) : want;
    int scale = a->sc - b->sc;
    *e2 = qs > scale ? qs - scale : 0;
    *down = scale > qs ? scale - qs : 0;
    long double bq = a->bd * dx_p10(*e2);
    if (bq >= dx_p10(17)) return 0;
    bq = bq / dx_p10(*down) + 1;
    long double bp = bq * b->bd;                    /* the product, at scale qs + b->sc */
    if (bp >= DX_LIM) return 0;
    int ps = qs + b->sc;
    *sr = ps > a->sc ? ps : a->sc;
    long double ba = a->bd * dx_p10(*sr - a->sc); bp *= dx_p10(*sr - ps);
    if (ba >= DX_LIM || bp >= DX_LIM || ba + bp >= DX_LIM) return 0;
    *br = ba + bp;
    return 1;
}

/* ---- the hooks: a statement as a node, or nothing ---------------------- */

static int lw_store_stmt(int expr, Ref *rs, int *rd, int nr, const Ref *rem, int line, const char *verb)
{
    for (int i = 0; i < nr; i++) if (!lw_recv_ok(&rs[i])) { lw_refuse(line, verb, "a receiver"); return 0; }
    LNode *x = &g_lw_n[expr];
    int need = 0;
    if (x->op == '/') {
        for (int i = 0; i < nr; i++) { int k = (rs[i].sym->pi.scale > 0 ? rs[i].sym->pi.scale : 0) + (rd[i] ? 1 : 0); if (k > need) need = k; }
        int e, sc; long double bd;
        if (!lw_div_shape(expr, need, &e, &sc, &bd)) { lw_refuse(line, verb, "the quotient's scale is the values'"); return 0; }
        if (rem) {
            int e2, down, sr; long double br;
            if (!lw_recv_ok(rem)) { lw_refuse(line, verb, "the REMAINDER item"); return 0; }
            if (!lw_rem_shape(expr, rs[0].sym, &e2, &down, &sr, &br)) { lw_refuse(line, verb, "the remainder's bound"); return 0; }
        }
    }
    int st = lw_stmt(LS_STORE, line);
    LStmt *s = &g_lw_s[st]; s->expr = expr; s->need = need; s->nr = nr; s->rem = rem ? (int)(rem->sym - g_sym) : -1;
    for (int i = 0; i < nr; i++) { s->rsym[i] = (int)(rs[i].sym - g_sym); s->rnd[i] = (unsigned char)(rd[i] != 0); }
    lw_place(st);
    return 1;
}

/* COMPUTE: after the expression and the phrases are read, before any code */
static int lw_compute(Ref *rs, int *rd, int nr, Expr *e, int size_err)
{
    if (lw_off()) return 0;
    const char *why = lw_stmt_refused(size_err);
    if (why) { lw_refuse(rs[0].line, "COMPUTE", why); return 0; }
    if (refs_pending(rs, nr)) { lw_refuse(rs[0].line, "COMPUTE", "a receiver's call"); return 0; }
    int x = lw_expr(e, 1);
    if (x < 0) { lw_refuse(rs[0].line, "COMPUTE", "the expression"); return 0; }
    return lw_store_stmt(x, rs, rd, nr, NULL, rs[0].line, "COMPUTE");
}

/* the operands' sum, left to right as the stack adds them */
static int lw_sum(Opnd *ops, int n)
{
    int r = lw_opnd(&ops[0]);
    for (int i = 1; i < n && r >= 0; i++) r = lw_binop('+', r, lw_opnd(&ops[i]), 0);
    return r;
}
/* ADD ... TO / SUBTRACT ... FROM: each receiver takes its value plus (less) the sum */
static int lw_addto(int sum, Ref *rs, int *rd, int nr, int subtract, int line, const char *verb)
{
    for (int i = 0; i < nr; i++) {
        if (!lw_recv_ok(&rs[i])) { lw_refuse(line, verb, "a receiver"); return 0; }
        Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = rs[i]; o.line = rs[i].line;
        int leaf = lw_opnd(&o);
        if (lw_binop(subtract ? '-' : '+', leaf, sum, 0) < 0) { lw_refuse(line, verb, "the sum's bound"); return 0; }
    }
    int st = lw_stmt(LS_ADDTO, line);
    LStmt *s = &g_lw_s[st]; s->expr = sum; s->nr = nr; s->subtract = subtract;
    for (int i = 0; i < nr; i++) { s->rsym[i] = (int)(rs[i].sym - g_sym); s->rnd[i] = (unsigned char)(rd[i] != 0); }
    lw_place(st);
    return 1;
}
/* ADD, SUBTRACT, MULTIPLY, DIVIDE as nodes (arith_reg.h): after the
 * phrases are read, before any code.  verb: 'A' 'S' 'M' 'D'. */
static int lw_arith(Arith *st, char verb)
{
    if (lw_off()) return 0;
    const char *name = verb == 'A' ? "ADD" : verb == 'S' ? "SUBTRACT" : verb == 'M' ? "MULTIPLY" : "DIVIDE";
    int line = st->rs[0].line;
    const char *why = lw_stmt_refused(st->size_err);
    if (why) { lw_refuse(line, name, why); return 0; }
    if (refs_pending(st->rs, st->nr) || (st->has_rem && ref_pending(&st->rem))) { lw_refuse(line, name, "a receiver's call"); return 0; }
    if (verb == 'A' || verb == 'S') {
        int sum = lw_sum(st->ops, st->n);
        if (sum < 0) { lw_refuse(line, name, "an operand"); return 0; }
        if (!st->giving) return lw_addto(sum, st->rs, st->rd, st->nr, verb == 'S', line, name);
        int x = verb == 'S' ? lw_binop('-', lw_opnd(&st->minuend), sum, 0) : sum;
        if (x < 0) { lw_refuse(line, name, "the bound"); return 0; }
        return lw_store_stmt(x, st->rs, st->rd, st->nr, NULL, line, name);
    }
    int a = lw_opnd(&st->a);
    if (a < 0) { lw_refuse(line, name, "an operand"); return 0; }
    if (st->giving) {
        int b = lw_opnd(&st->b);
        if (b < 0) { lw_refuse(line, name, "an operand"); return 0; }
        int x = verb == 'M' ? lw_binop('*', a, b, 0) : st->into ? lw_binop('/', b, a, 1) : lw_binop('/', a, b, 1);
        if (x < 0) { lw_refuse(line, name, "the bound"); return 0; }
        return lw_store_stmt(x, st->rs, st->rd, st->nr, st->has_rem ? &st->rem : NULL, line, name);
    }
    /* MULTIPLY a BY r ...; DIVIDE a INTO r ...: each receiver its own statement */
    int first = g_lw_ns, nplace = g_nasm;
    for (int i = 0; i < st->nr; i++) {
        if (!lw_recv_ok(&st->rs[i])) { lw_refuse(line, name, "a receiver"); goto undo; }
        Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = st->rs[i]; o.line = st->rs[i].line;
        int r = lw_opnd(&o);
        int x = verb == 'M' ? lw_binop('*', r, a, 0) : lw_binop('/', r, a, 1);
        if (x < 0) { lw_refuse(line, name, "the bound"); goto undo; }
        if (!lw_store_stmt(x, &st->rs[i], &st->rd[i], 1, NULL, line, name)) goto undo;
    }
    return 1;
undo:
    g_lw_ns = first; g_nasm = nplace;
    return 0;
}

/* MOVE of a number to numeric items: the store, truncating */
static int lw_move(Opnd *src, Ref *dst, int n)
{
    if (lw_off()) return 0;
    const char *why = lw_stmt_refused(0);
    if (why) { lw_refuse(src->line, "MOVE", why); return 0; }
    if (src->kind != O_REF && src->kind != O_NUM && src->kind != O_FIG) return 0;
    if (src->kind == O_REF && !lw_item_ok(&src->ref)) { lw_refuse(src->line, "MOVE", "the sender"); return 0; }
    for (int i = 0; i < n; i++) if (!lw_recv_ok(&dst[i])) { lw_refuse(src->line, "MOVE", "a receiver"); return 0; }
    int x = lw_opnd(src);
    if (x < 0) { lw_refuse(src->line, "MOVE", "the sender"); return 0; }
    int rd[MAXOPS] = { 0 };
    if (n > MAXOPS) return 0;
    return lw_store_stmt(x, dst, rd, n, NULL, src->line, "MOVE");
}

/* a condition as a node, -1 when not taken */
static int lw_cond(Cond *c)
{
    if (c->uc1 > c->uc0) return -1;
    if (c->kind == C_AND || c->kind == C_OR) {
        int a = lw_cond(c->a); if (a < 0) return -1;
        int b = lw_cond(c->b); if (b < 0) return -1;
        return lw_cnode(c->kind, a, b, -1, -1, 0);
    }
    if (c->kind == C_NOT) { int a = lw_cond(c->a); return a < 0 ? -1 : lw_cnode(C_NOT, a, -1, -1, -1, 0); }
    if (c->kind != C_REL || c->ptr || c->bstack) return -1;
    int x = lw_opnd(&c->x); if (x < 0) return -1;
    int y = lw_opnd(&c->y); if (y < 0) return -1;
    /* the two aligned must be within bounds, as a sum's sides are */
    if (lw_binop('-', x, y, 0) < 0) return -1;
    int op = c->op;
    if (c->neg) op = op == R_EQ ? R_NE : op == R_NE ? R_EQ : op == R_LT ? R_GE : op == R_GE ? R_LT : op == R_GT ? R_LE : R_GT;
    return lw_cnode(C_REL, -1, -1, x, y, op);
}

/* a block that is nothing but placeholders: its statements appended to
 * g_lw_list, the range in *at, *n */
static int lw_block_stmts(const Block *b, int *at, int *n)
{
    *at = g_lw_nlist; *n = 0;
    for (int i = 0; i < b->n; i++) {
        int st;
        if (!lw_is_place(b->line[i], &st)) { g_lw_nlist = *at; return 0; }
        lw_list_add(st); (*n)++;
    }
    return 1;
}

/* IF: its branches read, before its code */
static int lw_if(IfStmt *s)
{
    if (lw_off() || s->then_ns || s->else_ns) return 0;
    int line = cur()->line;
    const char *why = lw_stmt_refused(0);
    if (why) { lw_refuse(line, "IF", why); return 0; }
    int c = lw_cond(s->c);
    if (c < 0) { lw_refuse(line, "IF", "the condition"); return 0; }
    int b, nb, e = 0, ne = 0;
    if (!lw_block_stmts(&s->then_b, &b, &nb)) { lw_refuse(line, "IF", "a statement in THEN"); return 0; }
    if (s->has_else && !lw_block_stmts(&s->else_b, &e, &ne)) { lw_refuse(line, "IF", "a statement in ELSE"); g_lw_nlist = b; return 0; }
    int st = lw_stmt(LS_IF, line);
    LStmt *x = &g_lw_s[st]; x->cond = c; x->body = b; x->nbody = nb; x->els = e; x->nels = ne;
    lw_place(st);
    s->then_b = lw_expand(&s->then_b);
    if (s->has_else) s->else_b = lw_expand(&s->else_b);
    return 1;
}

/* an in-line PERFORM VARYING (one level) or UNTIL, its body read */
static int lw_perform(Vary *v, int nv, Cond *until, Body *body, int test_after)
{
    if (lw_off() || !body->inline_body) return 0;
    int line = cur()->line;
    const char *why = lw_stmt_refused(0);
    if (why) { lw_refuse(line, "PERFORM", why); return 0; }
    int var = -1, from = -1, by = -1;
    if (v) {
        if (nv != 1) { lw_refuse(line, "PERFORM", "AFTER"); return 0; }
        if (!lw_recv_ok(&v[0].var)) { lw_refuse(line, "PERFORM", "the VARYING item"); return 0; }
        var = (int)(v[0].var.sym - g_sym);
        from = lw_opnd(&v[0].from); by = lw_opnd(&v[0].by);
        if (from < 0 || by < 0) { lw_refuse(line, "PERFORM", "FROM or BY"); return 0; }
        Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = v[0].var; o.line = line;
        if (lw_binop('+', lw_opnd(&o), by, 0) < 0) { lw_refuse(line, "PERFORM", "the step's bound"); return 0; }
        until = v[0].until;
    }
    int c = lw_cond(until);
    if (c < 0) { lw_refuse(line, "PERFORM", "the condition"); return 0; }
    int b, nb;
    if (!lw_block_stmts(&body->blk, &b, &nb)) { lw_refuse(line, "PERFORM", "a statement in the body"); return 0; }
    int st = lw_stmt(LS_LOOP, line);
    LStmt *x = &g_lw_s[st]; x->cond = c; x->body = b; x->nbody = nb; x->var = var; x->from = from; x->by = by; x->test_after = test_after;
    lw_place(st);
    body->blk = lw_expand(&body->blk);
    return 1;
}

/* ---- HIR for an island -------------------------------------------------- */

typedef struct { int lo, hi; } LV;      /* a value: a word (hi < 0, its sign the value's) or a pair */

static int lw_frame;                    /* the island's frame: 8 for the saved r31, r30, then the allocas */
static int lw_blk_live;
typedef struct { int sym, a_lo, a_hi, written; } LwItem;
static LwItem g_lw_item[256]; static int g_lw_nitem;

static int lw_iconst(int v) { return hi_emit(HI_ICONST, TY_INT, -1, -1, v, NULL); }
static LV lw_lit(long long v, int wide)
{
    LV r; r.lo = lw_iconst((int)(unsigned)(unsigned long long)v); r.hi = -1;
    if (wide) r.hi = lw_iconst((int)(unsigned)((unsigned long long)v >> 32));
    return r;
}
static LV lw_widen(LV v) { if (v.hi < 0) v.hi = hi_emit(HI_SRA, TY_INT, v.lo, lw_iconst(31), 0, NULL); return v; }
static int lw_alloca(void)
{
    lw_frame += 4;
    int a = hi_emit(HI_ALLOCA, TY_INT, -1, -1, -lw_frame, NULL);
    if (hl_nalloca >= HL_MAX_ALLOCA) die_at(cur()->line, "internal: an island has too many items");
    hl_ainst[hl_nalloca] = a; hl_aoff[hl_nalloca] = -lw_frame; hl_aslot[hl_nalloca] = 0; hl_nalloca++;
    return a;
}
static void lw_begin_blk(int b) { hl_switch_block(b); lw_blk_live = 1; }
static void lw_goto(int b) { if (lw_blk_live) hi_emit(HI_BR, TY_VOID, -1, -1, b, NULL); lw_blk_live = 0; }
static void lw_brc(int c, int bt, int bf) { hi_emit(HI_BRC, TY_VOID, c, bt, bf, NULL); lw_blk_live = 0; }

/* the island's items: found before any code, so their allocas come first */
static int lw_item_slot(int sym)
{
    for (int i = 0; i < g_lw_nitem; i++) if (g_lw_item[i].sym == sym) return i;
    if (g_lw_nitem == 256) die_at(cur()->line, "internal: an island names too many items");
    LwItem *it = &g_lw_item[g_lw_nitem]; it->sym = sym; it->written = 0;
    it->a_lo = lw_alloca(); it->a_hi = g_sym[sym].size == 8 ? lw_alloca() : -1;
    return g_lw_nitem++;
}
static void lw_collect_node(int n)
{
    if (n < 0) return;
    LNode *x = &g_lw_n[n];
    if (!x->op) { lw_item_slot(x->sym); return; }
    lw_collect_node(x->l); lw_collect_node(x->r);
}
static void lw_collect_cond(int c)
{
    LCond *x = &g_lw_c[c];
    if (x->kind == C_REL) { lw_collect_node(x->x); lw_collect_node(x->y); return; }
    lw_collect_cond(x->a);
    if (x->kind != C_NOT) lw_collect_cond(x->b);
}
static void lw_collect_stmts(int at, int n)
{
    for (int i = 0; i < n; i++) {
        LStmt *s = &g_lw_s[g_lw_list[at + i]];
        if (s->expr >= 0) lw_collect_node(s->expr);
        for (int k = 0; k < s->nr; k++) lw_item_slot(s->rsym[k]);
        if (s->rem >= 0) lw_item_slot(s->rem);
        if (s->cond >= 0) lw_collect_cond(s->cond);
        if (s->var >= 0) { lw_item_slot(s->var); lw_collect_node(s->from); lw_collect_node(s->by); }
        lw_collect_stmts(s->body, s->nbody);
        lw_collect_stmts(s->els, s->nels);
    }
}

/* the item's storage */
static int lw_item_addr(const Sym *s)
{
    int a = hi_emit(HI_GADDR, TY_INT, -1, -1, 0, g_sym[s->record].label);
    return s->offset ? hi_emit(HI_ADDI, TY_INT, a, -1, s->offset, NULL) : a;
}
static int lw_item_ty(const Sym *s)
{
    int ty = s->size == 1 ? TY_CHAR : s->size == 2 ? TY_SHORT : TY_INT;
    return s->pi.is_signed || s->size == 4 ? ty : ty | TY_UNSIGNED;    /* an unsigned 9(9) is below 2^31 */
}
static void lw_entry_loads(void)
{
    for (int i = 0; i < g_lw_nitem; i++) {
        LwItem *it = &g_lw_item[i]; const Sym *s = &g_sym[it->sym];
        int a = lw_item_addr(s);
        hi_emit(HI_STORE, TY_INT, it->a_lo, hi_emit(HI_LOAD, lw_item_ty(s), a, -1, 0, NULL), 0, NULL);
        if (it->a_hi >= 0) hi_emit(HI_STORE, TY_INT, it->a_hi, hi_emit(HI_LOAD, TY_INT, hi_emit(HI_ADDI, TY_INT, a, -1, 4, NULL), -1, 0, NULL), 0, NULL);
    }
}
static void lw_exit_stores(void)
{
    for (int i = 0; i < g_lw_nitem; i++) {
        LwItem *it = &g_lw_item[i]; const Sym *s = &g_sym[it->sym];
        if (!it->written) continue;
        int a = lw_item_addr(s);
        hi_emit(HI_STORE, lw_item_ty(s), a, hi_emit(HI_LOAD, TY_INT, it->a_lo, -1, 0, NULL), 0, NULL);
        if (it->a_hi >= 0) hi_emit(HI_STORE, TY_INT, hi_emit(HI_ADDI, TY_INT, a, -1, 4, NULL), hi_emit(HI_LOAD, TY_INT, it->a_hi, -1, 0, NULL), 0, NULL);
    }
}
static LV lw_item_val(int sym)
{
    LwItem *it = &g_lw_item[lw_item_slot(sym)];
    LV v; v.lo = hi_emit(HI_LOAD, TY_INT, it->a_lo, -1, 0, NULL); v.hi = -1;
    if (it->a_hi >= 0) v.hi = hi_emit(HI_LOAD, TY_INT, it->a_hi, -1, 0, NULL);
    return v;
}
static void lw_item_set(int sym, LV v)
{
    LwItem *it = &g_lw_item[lw_item_slot(sym)];
    it->written = 1;
    hi_emit(HI_STORE, TY_INT, it->a_lo, v.lo, 0, NULL);
    if (it->a_hi >= 0) hi_emit(HI_STORE, TY_INT, it->a_hi, lw_widen(v).hi, 0, NULL);
}

/* ---- arithmetic on words and pairs ---- */

#define LW_WORD 2147483648.0L           /* a bound below this: a word holds the value */
static int lw_wide_bd(long double bd) { return bd >= LW_WORD; }

static LV lw_add64(LV a, LV b)
{
    a = lw_widen(a); b = lw_widen(b);
    LV r; r.lo = hi_emit(HI_ADD, TY_INT, a.lo, b.lo, 0, NULL);
    int c = hi_emit(HI_SLTU, TY_INT, r.lo, a.lo, 0, NULL);
    r.hi = hi_emit(HI_ADD, TY_INT, hi_emit(HI_ADD, TY_INT, a.hi, b.hi, 0, NULL), c, 0, NULL);
    return r;
}
static LV lw_sub64(LV a, LV b)
{
    a = lw_widen(a); b = lw_widen(b);
    LV r; int c = hi_emit(HI_SLTU, TY_INT, a.lo, b.lo, 0, NULL);
    r.lo = hi_emit(HI_SUB, TY_INT, a.lo, b.lo, 0, NULL);
    r.hi = hi_emit(HI_SUB, TY_INT, hi_emit(HI_SUB, TY_INT, a.hi, b.hi, 0, NULL), c, 0, NULL);
    return r;
}
static LV lw_call64(const char *fn, LV a, LV b)
{
    a = lw_widen(a); b = lw_widen(b);
    int cb = h_ncarg;
    h_carg[h_ncarg++] = a.lo; h_carg[h_ncarg++] = a.hi; h_carg[h_ncarg++] = b.lo; h_carg[h_ncarg++] = b.hi;
    LV r; r.lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, (char *)fn);
    h_cbase[r.lo] = cb;
    r.hi = hi_emit(HI_CALLHI, TY_INT, r.lo, -1, 0, NULL);
    return r;
}
static LV lw_neg(LV v, int wide)
{
    if (!wide) { LV r; r.lo = hi_emit(HI_SUB, TY_INT, lw_iconst(0), v.lo, 0, NULL); r.hi = -1; return r; }
    LV z; z.lo = lw_iconst(0); z.hi = lw_iconst(0);
    return lw_sub64(z, v);
}
/* x op y at a common width; the result wide when wide */
static LV lw_arith2(char op, LV x, LV y, int wide)
{
    if (!wide) {
        LV r; r.hi = -1;
        r.lo = hi_emit(op == '+' ? HI_ADD : op == '-' ? HI_SUB : op == '*' ? HI_MUL : op == '/' ? HI_DIV : HI_REM, TY_INT, x.lo, y.lo, 0, NULL);
        return r;
    }
    if (op == '+') return lw_add64(x, y);
    if (op == '-') return lw_sub64(x, y);
    return lw_call64(op == '*' ? "cob_mul64" : op == '/' ? "__divdi3" : "__moddi3", x, y);     /* (no __muldi3 in libs32: libcob's) */
}
static long long lw_p10(int k) { long long p = 1; while (k-- > 0) p *= 10; return p; }
/* v times 10^k, the result wide when wide */
static LV lw_scale(LV v, int k, int wide)
{
    if (k <= 0) return v;
    return lw_arith2('*', v, lw_lit(lw_p10(k), wide), wide);
}
/* |v| */
static LV lw_abs(LV v, int wide)
{
    if (!wide) {
        LV r; r.hi = -1;
        int s = hi_emit(HI_SRA, TY_INT, v.lo, lw_iconst(31), 0, NULL);
        r.lo = hi_emit(HI_SUB, TY_INT, hi_emit(HI_XOR, TY_INT, v.lo, s, 0, NULL), s, 0, NULL);
        return r;
    }
    v = lw_widen(v);
    LV s; s.lo = s.hi = hi_emit(HI_SRA, TY_INT, v.hi, lw_iconst(31), 0, NULL);
    LV x; x.lo = hi_emit(HI_XOR, TY_INT, v.lo, s.lo, 0, NULL); x.hi = hi_emit(HI_XOR, TY_INT, v.hi, s.hi, 0, NULL);
    return lw_sub64(x, s);
}
/* comparisons: a word, 0 or 1 */
static int lw_cmp(int op, LV x, LV y, int wide)
{
    if (!wide) {
        int k = op == R_EQ ? HI_SEQ : op == R_NE ? HI_SNE : op == R_LT ? HI_SLT : op == R_GT ? HI_SGT : op == R_LE ? HI_SLE : HI_SGE;
        return hi_emit(k, TY_INT, x.lo, y.lo, 0, NULL);
    }
    x = lw_widen(x); y = lw_widen(y);
    if (op == R_EQ || op == R_NE) {
        int d = hi_emit(HI_OR, TY_INT, hi_emit(HI_XOR, TY_INT, x.lo, y.lo, 0, NULL), hi_emit(HI_XOR, TY_INT, x.hi, y.hi, 0, NULL), 0, NULL);
        return hi_emit(op == R_EQ ? HI_SEQ : HI_SNE, TY_INT, d, lw_iconst(0), 0, NULL);
    }
    if (op == R_GT) { LV t = x; x = y; y = t; op = R_LT; }
    else if (op == R_LE) { LV t = x; x = y; y = t; op = R_GE; }
    /* x < y: the high words signed, the low ones unsigned when those tie */
    int lt = hi_emit(HI_OR, TY_INT, hi_emit(HI_SLT, TY_INT, x.hi, y.hi, 0, NULL),
                     hi_emit(HI_AND, TY_INT, hi_emit(HI_SEQ, TY_INT, x.hi, y.hi, 0, NULL), hi_emit(HI_SLTU, TY_INT, x.lo, y.lo, 0, NULL), 0, NULL), 0, NULL);
    return op == R_LT ? lt : hi_emit(HI_XOR, TY_INT, lt, lw_iconst(1), 0, NULL);
}

/* ---- values, stores, statements ---- */

static LV lw_val(int n);
/* the node's value, scaled up by k digits; its width that of bd.  A
 * literal is scaled where it stands, and a product's literal side takes
 * the scaling: one multiplication, not two. */
static LV lw_val_scaled(int n, int k, long double bd)
{
    LNode *x = &g_lw_n[n];
    int wide = lw_wide_bd(bd);
    if (k > 0 && x->op == 'k') return lw_lit(x->k * lw_p10(k), wide);
    if (k > 0 && x->op == '*' && (g_lw_n[x->l].op == 'k' || g_lw_n[x->r].op == 'k')) {
        int lit = g_lw_n[x->l].op == 'k' ? x->l : x->r, other = lit == x->l ? x->r : x->l;
        LV o = lw_val(other);
        if (wide) o = lw_widen(o);
        return lw_arith2('*', o, lw_lit(g_lw_n[lit].k * lw_p10(k), wide), wide);
    }
    LV v = lw_val(n);
    if (wide) v = lw_widen(v);
    return lw_scale(v, k, wide);
}
static LV lw_val(int n)
{
    LNode *x = &g_lw_n[n];
    int wide = lw_wide_bd(x->bd);
    if (x->op == 'k') return lw_lit(x->k, wide);
    if (!x->op) return lw_item_val(x->sym);
    if (x->op == 'n') return lw_neg(lw_val(x->l), wide);
    LNode *a = &g_lw_n[x->l], *b = &g_lw_n[x->r];
    if (x->op == '*') return lw_arith2('*', lw_val(x->l), lw_val(x->r), wide);
    if (x->op == '/') die_at(cur()->line, "internal: a division below the root of an island's tree");
    /* + -: aligned to the larger scale */
    LV l = lw_val_scaled(x->l, x->sc - a->sc, x->bd), r = lw_val_scaled(x->r, x->sc - b->sc, x->bd);
    return lw_arith2(x->op, l, r, wide);
}

/* v less its digits above the first n: the remainder by 10^n, taken only
 * when |v| reaches 10^n -- a value rarely does, and a 64-bit remainder is
 * a routine.  The join is a temporary the SSA pass promotes. */
static LV lw_trunc_digits(LV v, int n, int wide)
{
    LV lim = lw_lit(lw_p10(n), wide);
    int t_lo = lw_alloca(), t_hi = wide ? lw_alloca() : -1;
    if (wide) v = lw_widen(v);
    hi_emit(HI_STORE, TY_INT, t_lo, v.lo, 0, NULL);
    if (wide) hi_emit(HI_STORE, TY_INT, t_hi, v.hi, 0, NULL);
    int over = lw_cmp(R_GE, lw_abs(v, wide), lim, wide);
    int b_cut = hir_new_block(), b_join = hir_new_block();
    lw_brc(over, b_cut, b_join);
    lw_begin_blk(b_cut);
    LV r = lw_arith2('%', v, lim, wide);
    hi_emit(HI_STORE, TY_INT, t_lo, r.lo, 0, NULL);
    if (wide) hi_emit(HI_STORE, TY_INT, t_hi, r.hi, 0, NULL);
    lw_goto(b_join);
    lw_begin_blk(b_join);
    LV o; o.lo = hi_emit(HI_LOAD, TY_INT, t_lo, -1, 0, NULL); o.hi = wide ? hi_emit(HI_LOAD, TY_INT, t_hi, -1, 0, NULL) : -1;
    return o;
}

/* v (scale sc, bound bd, neg) into the item: cob_k_put_scale written out */
static void lw_store(int sym, int rounded, LV v, int sc, long double bd, int neg)
{
    const Sym *d = &g_sym[sym];
    int eff = d->pi.digits, sd = d->pi.scale;
    int wide = lw_wide_bd(bd);
    if (wide) v = lw_widen(v);
    if (sc > sd) {
        int m = sc - sd; long long P = lw_p10(m);
        LV q, r = v;
        if (bd < (long double)P) q = lw_lit(0, wide);
        else {
            q = lw_arith2('/', v, lw_lit(P, wide), wide);
            if (rounded) r = lw_arith2('-', v, lw_arith2('*', q, lw_lit(P, wide), wide), wide);   /* v - q * P: no second division */
        }
        if (rounded) {
            /* |r| at least half of P: one more, with the value's sign */
            LV ar = lw_abs(r, wide);
            int c = lw_cmp(R_GE, ar, lw_lit(P / 2, wide), wide);
            int s = hi_emit(HI_SRA, TY_INT, wide ? v.hi : v.lo, lw_iconst(31), 0, NULL);
            LV adj; adj.lo = hi_emit(HI_SUB, TY_INT, hi_emit(HI_XOR, TY_INT, c, s, 0, NULL), s, 0, NULL); adj.hi = -1;
            if (wide) adj.hi = hi_emit(HI_AND, TY_INT, s, hi_emit(HI_SUB, TY_INT, lw_iconst(0), c, 0, NULL), 0, NULL);
            q = lw_arith2('+', q, adj, wide);
        }
        v = q; bd = bd / (long double)P + (rounded ? 1 : 0); sc = sd;
    } else if (sc < sd) {
        int k = sd - sc, keep = eff - k;
        if (keep <= 0) { v = lw_lit(0, 0); bd = 0; wide = 0; }
        else {
            long double lim = dx_p10(keep);
            if (bd >= lim) { v = lw_trunc_digits(v, keep, wide); bd = lim - 1; }
            bd *= dx_p10(k);
            int w2 = lw_wide_bd(bd);
            if (w2 && !wide) { v = lw_widen(v); wide = 1; }
            v = lw_scale(v, k, wide);
        }
        sc = sd;
    }
    {
        long double lim = dx_p10(eff);
        if (bd >= lim) { v = lw_trunc_digits(v, eff, wide); bd = lim - 1; }
    }
    if (!d->pi.is_signed && neg) v = lw_abs(v, wide);
    if (d->size < 8) v.hi = -1;                     /* the value fits the word (eff <= 9) */
    lw_item_set(sym, v);
}

static void lw_gen_stmts(int at, int n);
/* a condition's value, a word 0 or 1 */
static int lw_cond_val(int c)
{
    LCond *x = &g_lw_c[c];
    if (x->kind == C_NOT) return hi_emit(HI_XOR, TY_INT, lw_cond_val(x->a), lw_iconst(1), 0, NULL);
    if (x->kind == C_AND || x->kind == C_OR)
        return hi_emit(x->kind == C_AND ? HI_AND : HI_OR, TY_INT, lw_cond_val(x->a), lw_cond_val(x->b), 0, NULL);
    LNode *a = &g_lw_n[x->x], *b = &g_lw_n[x->y];
    int sc = a->sc > b->sc ? a->sc : b->sc;
    long double bd = a->bd * dx_p10(sc - a->sc) + b->bd * dx_p10(sc - b->sc);
    LV l = lw_val_scaled(x->x, sc - a->sc, bd), r = lw_val_scaled(x->y, sc - b->sc, bd);
    return lw_cmp(x->op, l, r, lw_wide_bd(bd));
}
/* the quotient at the root of a STORE: its value, scale and bound; the
 * stores it guards go in a block of their own, skipped for a zero divisor */
static void lw_gen_store(LStmt *s)
{
    LNode *x = &g_lw_n[s->expr];
    if (x->op != '/') {
        LV v = lw_val(s->expr);
        for (int i = 0; i < s->nr; i++) lw_store(s->rsym[i], s->rnd[i], v, x->sc, x->bd, x->neg);
        return;
    }
    int e, sq; long double bq;
    if (!lw_div_shape(s->expr, s->need, &e, &sq, &bq)) die_at(s->line, "internal: an island's division changed its mind");
    LNode *a = &g_lw_n[x->l], *b = &g_lw_n[x->r];
    LV dv = lw_val(x->r);
    int bwide = lw_wide_bd(b->bd);
    int z = lw_cmp(R_NE, dv, lw_lit(0, bwide), bwide);
    int b_div = hir_new_block(), b_join = hir_new_block();
    lw_brc(z, b_div, b_join);
    lw_begin_blk(b_div);
    long double bn = a->bd * dx_p10(e);
    int wide = lw_wide_bd(bn) || bwide;
    LV nv = lw_val_scaled(x->l, e, bn);
    if (wide) { nv = lw_widen(nv); dv = lw_widen(dv); }
    /* the dividend as it was: the remainder is of the operands before the
     * quotient is stored over one of them (tests/free/divremgiving) */
    LV a0 = s->rem >= 0 ? lw_val(x->l) : nv;
    LV q = lw_arith2('/', nv, dv, wide);
    for (int i = 0; i < s->nr; i++) lw_store(s->rsym[i], s->rnd[i], q, sq, bq, x->neg);
    if (s->rem >= 0) {
        /* the dividend less the divisor times the quotient cut to the
         * quotient item's decimals (lw_rem_shape) */
        const Sym *qd = &g_sym[s->rsym[0]];
        int e2, down, sr; long double br;
        if (!lw_rem_shape(s->expr, qd, &e2, &down, &sr, &br)) die_at(s->line, "internal: an island's remainder changed its mind");
        LV qt;
        long double bq2 = a->bd * dx_p10(e2);
        int w2 = lw_wide_bd(bq2) || bwide;
        if (e2 == e && w2 == wide) qt = q;
        else {
            LV nv2 = a0, dv2 = dv;
            if (w2) { nv2 = lw_widen(nv2); dv2 = lw_widen(dv2); }
            nv2 = lw_scale(nv2, e2, w2);
            qt = lw_arith2('/', nv2, dv2, w2);
        }
        if (down) { long double bd2 = bq2; qt = lw_arith2('/', qt, lw_lit(lw_p10(down), lw_wide_bd(bd2)), lw_wide_bd(bd2)); bq2 = bq2 / dx_p10(down) + 1; }
        if (!qd->pi.is_signed && g_std < 2002) qt = lw_abs(qt, lw_wide_bd(bq2));   /* 85: the magnitude (DIVIDE rule 6) */
        int qs = sq - e + e2 - down;                 /* the cut quotient's scale */
        int ps = qs + b->sc;
        long double bp = bq2 * b->bd;
        int pw = lw_wide_bd(bp);
        LV pv = lw_arith2('*', pw ? lw_widen(qt) : qt, pw ? lw_widen(dv) : dv, pw);
        int rw = lw_wide_bd(br);
        LV av = a0;
        if (rw) { av = lw_widen(av); pv = lw_widen(pv); }
        av = lw_scale(av, sr - a->sc, rw);
        pv = lw_scale(pv, sr - ps, rw);
        lw_store(s->rem, 0, lw_arith2('-', av, pv, rw), sr, br, 1);
    }
    lw_goto(b_join);
    lw_begin_blk(b_join);
}
static void lw_gen_addto(LStmt *s)
{
    LNode *sum = &g_lw_n[s->expr];
    LV sv = lw_val(s->expr);
    for (int i = 0; i < s->nr; i++) {
        const Sym *d = &g_sym[s->rsym[i]];
        int sc = d->pi.scale > sum->sc ? d->pi.scale : sum->sc;
        long double bd = (dx_p10(d->pi.digits) - 1) * dx_p10(sc - d->pi.scale) + sum->bd * dx_p10(sc - sum->sc);
        int wide = lw_wide_bd(bd);
        LV r = lw_item_val(s->rsym[i]);
        if (wide) r = lw_widen(r);
        r = lw_scale(r, sc - d->pi.scale, wide);
        LV t = sv; if (wide) t = lw_widen(t);
        t = lw_scale(t, sc - sum->sc, wide);
        lw_store(s->rsym[i], s->rnd[i], lw_arith2(s->subtract ? '-' : '+', r, t, wide), sc, bd, d->pi.is_signed || sum->neg || s->subtract);
    }
}
static void lw_gen_loop(LStmt *s)
{
    if (s->var >= 0) { LNode *f = &g_lw_n[s->from]; lw_store(s->var, 0, lw_val(s->from), f->sc, f->bd, f->neg); }
    int b_body = hir_new_block(), b_exit = hir_new_block(), b_test = -1;
    if (s->test_after) {
        lw_goto(b_body);
        lw_begin_blk(b_body);
        lw_gen_stmts(s->body, s->nbody);
        if (lw_blk_live) {
            int b_step = hir_new_block();
            lw_brc(lw_cond_val(s->cond), b_exit, b_step);
            lw_begin_blk(b_step);
        }
    } else {
        b_test = hir_new_block();
        lw_goto(b_test);
        lw_begin_blk(b_body);
        lw_gen_stmts(s->body, s->nbody);
    }
    if (s->var >= 0 && lw_blk_live) {
        const Sym *d = &g_sym[s->var]; LNode *by = &g_lw_n[s->by];
        int sc = d->pi.scale > by->sc ? d->pi.scale : by->sc;
        long double bd = (dx_p10(d->pi.digits) - 1) * dx_p10(sc - d->pi.scale) + by->bd * dx_p10(sc - by->sc);
        int wide = lw_wide_bd(bd);
        LV r = lw_item_val(s->var); if (wide) r = lw_widen(r);
        r = lw_scale(r, sc - d->pi.scale, wide);
        LV t = lw_val_scaled(s->by, sc - by->sc, bd);
        lw_store(s->var, 0, lw_arith2('+', r, t, wide), sc, bd, d->pi.is_signed || by->neg);
    }
    if (s->test_after) lw_goto(b_body);
    else {
        /* the test, entered first and after each body */
        lw_goto(b_test);
        lw_begin_blk(b_test);
        lw_brc(lw_cond_val(s->cond), b_exit, b_body);
    }
    lw_begin_blk(b_exit);
}
static void lw_gen_stmts(int at, int n)
{
    for (int i = 0; i < n && lw_blk_live; i++) {
        LStmt *s = &g_lw_s[g_lw_list[at + i]];
        switch (s->kind) {
        case LS_STORE: lw_gen_store(s); break;
        case LS_ADDTO: lw_gen_addto(s); break;
        case LS_IF: {
            int c = lw_cond_val(s->cond);
            int b_then = hir_new_block(), b_else = hir_new_block(), b_join = hir_new_block();
            lw_brc(c, b_then, b_else);
            lw_begin_blk(b_then); lw_gen_stmts(s->body, s->nbody); lw_goto(b_join);
            lw_begin_blk(b_else); lw_gen_stmts(s->els, s->nels); lw_goto(b_join);
            lw_begin_blk(b_join);
            break;
        }
        case LS_LOOP: lw_gen_loop(s); break;
        }
    }
}

/* the island's statements in g_lw_list[at .. at+n): the lowering the
 * backend calls (hcg_func) */
static int g_lw_at, g_lw_n_stmts;
static void hl_func(Node *fn)
{
    (void)fn;
    hir_reset();
    hl_nalloca = 0; hl_temp_stack = 0; hl_nparams = 0; hl_param_nflat = 0;
    lw_frame = 8;                               /* the saved r31 and r30 (f77_contract.h tells why) */
    g_lw_nitem = 0;
    int b = hir_new_block();
    lw_begin_blk(b);
    lw_collect_stmts(g_lw_at, g_lw_n_stmts);
    lw_entry_loads();
    lw_gen_stmts(g_lw_at, g_lw_n_stmts);
    if (lw_blk_live) { lw_exit_stores(); hi_emit(HI_RET, 0, -1, -1, 0, NULL); lw_blk_live = 0; }
    fn->locals_size = lw_frame;
}

/* the backend's text, into lines of the unit's: its 4-space indent a
 * tab, so relax_branches counts each instruction as the 4 bytes it is */
static void lw_take_text(char ***out, int *no, int *cap)
{
    int i = 0;
    while (i < cg_olen) {
        int j = i; while (j < cg_olen && cg_out[j] != '\n') j++;
        int n = j - i; const char *l = cg_out + i;
        if (n > 0) {
            char *s;
            if (n >= 4 && !memcmp(l, "    ", 4)) { s = xmalloc((size_t)n - 3 + 1); s[0] = '\t'; memcpy(s + 1, l + 4, (size_t)n - 4); s[n - 3] = 0; }
            else s = xstrndup(l, (size_t)n);
            if (*no == *cap) { *cap = *cap ? 2 * *cap : 256; *out = xrealloc(*out, (size_t)*cap * sizeof **out); }
            (*out)[(*no)++] = s;
        }
        i = j + 1;
    }
    cg_olen = 0;
}

/* Does a run of statements pay as an island?  A loop among them, or
 * enough of them (lw_min), or one that is heavy: decimals, a value past a
 * word, a division or ROUNDED -- which the text emitter sends through
 * the runtime's fetch and store, where an island computes in place
 * (kedit's COMPUTE and ADD alone: -14%).  Over integers in a word the
 * text emitter is already in place, and a statement alone there costs
 * the call (kmove +0.3%). */
static int lw_heavy_node(int n)
{
    if (n < 0) return 0;
    LNode *x = &g_lw_n[n];
    if (x->sc > 0 || x->bd >= LW_WORD || x->op == '/') return 1;
    return x->op && x->op != 'k' && (lw_heavy_node(x->l) || lw_heavy_node(x->r));
}
static int lw_heavy_cond(int c)
{
    LCond *x = &g_lw_c[c];
    if (x->kind == C_REL) return lw_heavy_node(x->x) || lw_heavy_node(x->y);
    return lw_heavy_cond(x->a) || (x->kind != C_NOT && lw_heavy_cond(x->b));
}
static int lw_count(int at, int n, int *loops, int *heavy)
{
    int c = 0;
    for (int i = 0; i < n; i++) {
        LStmt *s = &g_lw_s[g_lw_list[at + i]];
        c++;
        if (s->kind == LS_LOOP) (*loops)++;
        if (s->expr >= 0 && lw_heavy_node(s->expr)) *heavy = 1;
        for (int k = 0; k < s->nr; k++) if (s->rnd[k] || g_sym[s->rsym[k]].pi.scale > 0 || g_sym[s->rsym[k]].size == 8) *heavy = 1;
        if (s->cond >= 0 && lw_heavy_cond(s->cond)) *heavy = 1;
        c += lw_count(s->body, s->nbody, loops, heavy) + lw_count(s->els, s->nels, loops, heavy);
    }
    return c;
}

/* The runs of placeholders from line `from` on are resolved: a run that
 * pays becomes "jal r31, .LislN" and its island's text waits in g_lw_pend
 * (lw_flush puts it after the unit's code); one that does not becomes
 * its statements' own text.  Called before anything reads the code as
 * code -- loopreg's regions at an outermost in-line PERFORM's end, and
 * its unit-wide pass -- since a placeholder is an instruction nobody
 * lists, and would make it forget what the registers hold (csv2fw lost
 * 6% that way before this ran first). */
typedef struct { char *name; char **line; int n; } LwPend;
static LwPend *g_lw_pend; static int g_lw_npend, g_lw_pcap;
static void lw_resolve(int from)
{
    int any = 0;
    for (int i = from; i < g_nasm && !any; i++) { int st; any = lw_is_place(g_asm[i], &st); }
    if (!any) return;
    char **out = NULL; int no = 0, cap = 0;
#define LW_OUT(l) do { if (no == cap) { cap = cap ? 2 * cap : 4096; out = xrealloc(out, (size_t)cap * sizeof *out); } out[no++] = (l); } while (0)
    for (int i = from; i < g_nasm; i++) {
        int st;
        if (!lw_is_place(g_asm[i], &st)) { LW_OUT(g_asm[i]); continue; }
        int at = g_lw_nlist, n = 0;
        for (; i < g_nasm && lw_is_place(g_asm[i], &st); i++) { lw_list_add(st); n++; }
        i--;
        int loops = 0, heavy = 0, count = lw_count(at, n, &loops, &heavy);
        if (!loops && !heavy && count < lw_min()) {
            /* not an island: the statements' own text */
            if (lw_trace()) fprintf(stderr, "hir: line %d: %d statement%s kept as text\n", g_lw_s[g_lw_list[at]].line, count, count == 1 ? "" : "s");
            for (int k = 0; k < n; k++) {
                Block *t = &g_lw_s[g_lw_list[at + k]].text;
                for (int j = 0; j < t->n; j++) LW_OUT(t->line[j]);
            }
            continue;
        }
        char name[32]; snprintf(name, sizeof name, ".Lisl%d", g_lw_nisland++);
        Node fn; memset(&fn, 0, sizeof fn);
        fn.name = xstrndup(name, strlen(name)); fn.is_static = 1;
        g_lw_at = at; g_lw_n_stmts = n;
        hl_cur_fn_dbg = fn.name;
        cg_olen = 0; cg_njt = 0; cg_njt_ent = 0; cg_nfn = 0; cg_cur_fn = -1; cg_fd = -1;
        hcg_func(&fn);
        if (cg_njt) die_at(g_lw_s[g_lw_list[at]].line, "internal: an island made a jump table");
        if (lw_trace()) fprintf(stderr, "hir: %s: %d statement%s from line %d, %d item%s, %d HIR instructions\n", name, count, count == 1 ? "" : "s",
                                g_lw_s[g_lw_list[at]].line, g_lw_nitem, g_lw_nitem == 1 ? "" : "s", h_ninst);
        LW_GROW(g_lw_pend, g_lw_npend, g_lw_pcap);
        LwPend *pd = &g_lw_pend[g_lw_npend++]; pd->name = fn.name; pd->line = NULL; pd->n = 0;
        { int pc = 0; lw_take_text(&pd->line, &pd->n, &pc); }
        char call[48]; snprintf(call, sizeof call, "\tjal r31, %s", name);
        LW_OUT(xstrndup(call, strlen(call)));
    }
#undef LW_OUT
    while (from + no > g_asmcap) { g_asmcap = g_asmcap ? 2 * g_asmcap : 4096; g_asm = xrealloc(g_asm, (size_t)g_asmcap * sizeof *g_asm); }
    memcpy(g_asm + from, out, (size_t)no * sizeof *out);
    g_nasm = from + no;
    free(out);
}

/* the unit's code is complete and returned: the islands it calls follow
 * it.  One made inside a loop that was then lowered whole is called by
 * nothing, and is left out. */
static void lw_flush(void)
{
    for (int k = 0; k < g_lw_npend; k++) {
        LwPend *pd = &g_lw_pend[k];
        char call[48]; snprintf(call, sizeof call, "\tjal r31, %s", pd->name);
        int used = 0;
        for (int i = 0; i < g_nasm && !used; i++) used = !strcmp(g_asm[i], call);
        if (!used) { if (lw_trace()) fprintf(stderr, "hir: %s: called by nothing, dropped\n", pd->name); continue; }
        for (int j = 0; j < pd->n; j++) emit("%s", pd->line[j]);
    }
    g_lw_npend = 0;
    g_lw_nn = g_lw_nc = g_lw_ns = g_lw_nlist = 0;    /* the unit's nodes are spent */
}
