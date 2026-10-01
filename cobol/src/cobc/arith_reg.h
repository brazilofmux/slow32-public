/* s32-cobc: arithmetic in registers.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- integer arithmetic in registers ----------------------------------
 * MULTIPLY, DIVIDE and COMPUTE on integer binary (and short DISPLAY)
 * items, computed in a word instead of on the decimal stack, where the
 * answer is provably the stack's:
 *  - exact: every intermediate fits a signed word, bounding each operand
 *    by its picture, or by its bytes when the usage keeps the capacity
 *    (COMP-5, the native types);
 *  - wrap: no division, every receiver a signed COMP-5 or native item,
 *    and the whole value provably below 2^63 -- the stack computes it
 *    exactly and stores its low bytes, which is what a word's wrapping
 *    arithmetic leaves.
 *  - checked: anything else on such items -- each + - * and negation
 *    tests for a word's overflow (mulh for the product) and branches to
 *    the stack's code for the whole statement, emitted after; every test
 *    comes before the first store, so the stack starts clean.  Real values
 *    rarely overflow a word, so the check is nearly always passed.
 * Division only at the top: COBOL's 7 / 2 * 2 is 7, not integer division.
 * A zero divisor leaves the receivers alone, as the stack's size error
 * does without the phrase; -1 is negation (the word's INT_MIN / -1 traps).
 * SIZE ERROR, the EC checks, ROUNDED on a quotient and anything wider
 * take the stack.  A four-byte signed item is bounded by 2^31 - 1, as
 * hot_opnd_mag bounds it: the one value past that, INT_MIN, is the one
 * place the two paths can part (INT_MIN / -1 into a truncating receiver).  The profile of majesty's date functions (jerm) put 70%
 * of its instructions in the stack for exactly these statements. */
/* a user function's result met while scanning ahead: the scan made no
 * call, and a register tree has no place to make it, so the stack takes
 * the statement (emit_expr makes the call, ucall_make) */
static int opnd_scanned(const Opnd *o) { return o->kind == O_REF && g_sym[o->ref.sym->record].ftemp_scan; }
typedef struct { char op; int l, r; Opnd o; } HNode;
static int hn_depth(int n, int per);   /* op: 0 leaf, + - * /, 'n' negate */
#define MAXHN 64
static HNode g_hn[MAXHN]; static int g_nhn;
static int hn_new(char op, int l, int r, const Opnd *o)
{
    if (l < -1 || r < -1 || g_nhn >= MAXHN) return -2;
    HNode *h = &g_hn[g_nhn]; memset(h, 0, sizeof *h);
    h->op = op; h->l = l; h->r = r; if (o) h->o = *o;
    return g_nhn++;
}
/* the frame slots evaluating node n needs, per words held at each level */
static int hn_depth(int n, int per)
{
    HNode *h = &g_hn[n];
    if (!h->op) return 0;
    int l = hn_depth(h->l, per);
    if (h->op == 'n' || h->op == 'I' || h->op == 'T' || h->op == 'A') return l;
    int r = hn_depth(h->r, per) + per;
    return l > r ? l : r;
}
/* FUNCTION MOD / REM / INTEGER / INTEGER-PART / ABS as a node of a
 * register tree: op 'M' 'R' (the divisor a literal of magnitude 2 or
 * more, so no zero and no INT_MIN / -1), 'I' 'T', 'A'; each argument an
 * item, a literal or an expression that the given path takes */
static int hn_tree(const Expr *e, int (*leaf)(const Opnd *));
static int hn_arg(Opnd *a, int (*leaf)(const Opnd *))
{
    return a->kind == O_EXPR ? hn_tree(a->ex, leaf) : leaf(a);
}
static int hn_fn(const Opnd *o, int (*leaf)(const Opnd *))
{
    if (o->kind != O_FUNC || o->fkind != FK_NUMS) return -2;
    switch (o->fnid) {
    case COB_FN_MOD: case COB_FN_REM: {
        if (o->nfargs != 2 || o->fargs[1]->kind != O_NUM || !numlit_is_int(&o->fargs[1]->num)) return -2;
        long long d = numlit_int(&o->fargs[1]->num);
        if (d > -2 && d < 2) return -2;
        return hn_new(o->fnid == COB_FN_MOD ? 'M' : 'R', hn_arg(o->fargs[0], leaf), hn_arg(o->fargs[1], leaf), NULL);
    }
    case COB_FN_INTEGER: case COB_FN_INTEGER_PART: case COB_FN_ABS:
        if (o->nfargs != 1) return -2;
        return hn_new(o->fnid == COB_FN_INTEGER ? 'I' : o->fnid == COB_FN_ABS ? 'A' : 'T', hn_arg(o->fargs[0], leaf), -1, NULL);
    default: return -2;
    }
}
/* an expression's tree as a register tree, each leaf the path's own
 * (hx_leaf, dx_leaf): -2 where the path does not take it -- a power, a
 * leaf it refuses, a leaf that is not an operand where it starts (a
 * figurative constant is one), or more than MAXHN nodes.  Nodes are
 * made in the order the expression is written, children first. */
static int hn_leaf_at(const Expr *e)
{
    int save = g_tp; g_tp = e->tp;
    int ok = at_operand() || (cur()->kind == T_WORD && is_figurative(cur()->s));
    g_tp = save;
    return ok;
}
static int hn_tree(const Expr *e, int (*leaf)(const Opnd *))
{
    if (!e->op) {
        if (!hn_leaf_at(e)) return -2;
        return e->o->kind == O_FUNC ? hn_fn(e->o, leaf) : leaf(e->o);
    }
    if (e->op == '^') return -2;
    int l = hn_tree(e->l, leaf);
    if (l < 0) return -2;
    if (e->op == 'n') return hn_new('n', l, -1, NULL);
    return hn_new(e->op, l, hn_tree(e->r, leaf), NULL);
}
static int hx_leaf(const Opnd *o);
/* an operand's magnitude bound */
static long double hx_mag(const Opnd *o)
{
    if (o->kind == O_NUM) { long long v = numlit_int(&o->num); return v < 0 ? -(long double)v : (long double)v; }
    if (o->kind != O_REF) return 0;
    Sym *s = o->ref.sym;
    if (!is_display_int(s) && (sym_notrunc(s) || s->pi.digits >= 10))
        return s->size == 1 ? (s->pi.is_signed ? 128 : 255) : s->size == 2 ? (s->pi.is_signed ? 32768 : 65535) : 2147483647.0L;
    return (long double)pow10l(s->pi.digits) - 1;
}
/* the bound of node n; *wide past a word somewhere, *inner a division
 * below the top, *neg a value that can be negative */
static long double hx_bound(int n, int *wide, int *inner, int *neg, int top)
{
    HNode *h = &g_hn[n];
    long double b;
    if (!h->op) { b = hx_mag(&h->o); if (!opnd_nonneg(&h->o)) *neg = 1; }
    else if (h->op == 'n') { b = hx_bound(h->l, wide, inner, neg, 0); *neg = 1; }
    else if (h->op == 'I' || h->op == 'T') b = hx_bound(h->l, wide, inner, neg, 0);   /* an integer's own value */
    else if (h->op == 'A') { int ng = 0; b = hx_bound(h->l, wide, inner, &ng, 0); }
    else if (h->op == 'M' || h->op == 'R') {
        int ng = 0; long long d = numlit_int(&g_hn[h->r].o.num);
        hx_bound(h->l, wide, inner, h->op == 'R' ? neg : &ng, 0);
        if (h->op == 'M' && d < 0) *neg = 1;          /* MOD takes the divisor's sign, REM the dividend's */
        b = (long double)(d < 0 ? -d : d) - 1;
    }
    else {
        long double x = hx_bound(h->l, wide, inner, neg, 0), y = hx_bound(h->r, wide, inner, neg, 0);
        if (h->op == '/') { if (!top) *inner = 1; b = x; }
        else if (h->op == '*') b = x * y;
        else { b = x + y; if (h->op == '-') *neg = 1; }
    }
    if (b > 2147483647.0L) *wide = 1;
    return b;
}
/* r1 = node n; with slow >= 0 a word's overflow branches there */
static void hx_emit(int n, int slow)
{
    HNode *h = &g_hn[n];
    if (!h->op) { emit_hot_value(&h->o); return; }
    hx_emit(h->l, slow);
    if (h->op == 'I' || h->op == 'T') return;         /* an integer is its own integer part */
    if (h->op == 'A') { emit("\tsrai r2, r1, 31"); emit("\txor r1, r1, r2"); emit("\tsub r1, r1, r2"); return; }
    if (h->op == 'M' || h->op == 'R') {               /* the divisor a literal, |d| >= 2 */
        long long d = numlit_int(&g_hn[h->r].o.num);
        emit_li("r2", (long)d);
        emit("\trem r1, r1, r2");
        if (h->op == 'M') {                            /* floor: a nonzero remainder takes the divisor's sign */
            int L = new_label();
            emit("\tbeq r1, r0, .L%d", L);
            emit("\t%s r1, r0, .L%d", d > 0 ? "bge" : "blt", L);
            emit("\tadd r1, r1, r2");
            emit_label(L);
        }
        return;
    }
    if (h->op == 'n') {
        if (slow >= 0) { emit_li("r2", -2147483647L - 1); emit("\tbeq r1, r2, .L%d", slow); }
        emit("\tsub r1, r0, r1");
        return;
    }
    if (g_slot_base >= NSLOTS) die_at(cur()->line, "internal: an arithmetic expression nests too deeply for the frame");
    int t = g_slot_base++;
    emit("\tstw sp+%d, r1", SLOT(t));
    hx_emit(h->r, slow);
    emit("\tadd r2, r1, r0");
    emit("\tldw r1, sp+%d", SLOT(t));
    g_slot_base--;
    if (slow < 0) { emit("\t%s r1, r1, r2", h->op == '+' ? "add" : h->op == '-' ? "sub" : "mul"); return; }
    if (h->op == '*') {                             /* the high word must be the low word's sign */
        emit("\tmul r3, r1, r2");
        emit("\tmulh r1, r1, r2");
        emit("\tsrai r2, r3, 31");
        emit("\tbne r1, r2, .L%d", slow);
    } else if (h->op == '+') {                      /* both operands' signs differ from the sum's */
        emit("\tadd r3, r1, r2");
        emit("\txor r1, r1, r3");
        emit("\txor r2, r2, r3");
        emit("\tand r1, r1, r2");
        emit("\tblt r1, r0, .L%d", slow);
    } else {                                        /* the operands' signs differ, and the result's from the minuend's */
        emit("\tsub r3, r1, r2");
        emit("\txor r2, r1, r2");
        emit("\txor r1, r1, r3");
        emit("\tand r1, r1, r2");
        emit("\tblt r1, r0, .L%d", slow);
    }
    emit("\tadd r1, r3, r0");
}
/* may the tree at root be stored into rs (and the remainder into rem)
 * in registers?  *bound and *nonneg for the store's truncation */
static int hx_ok(int root, Ref *rs, int *rd, int nr, Ref *rem, int size_err, long long *bound, int *nonneg)
{
    if (root < 0 || size_err || g_wide || g_nohx || g_rmode || ec_on_name("EC-DATA-INCOMPATIBLE")) return 0;
    if (g_slot_base + hn_depth(root, 1) + 2 > NSLOTS) return 0;     /* too deep for the frame: the stack */
    int wide = 0, inner = 0, neg = 0, div = g_hn[root].op == '/';
    long double b = hx_bound(root, &wide, &inner, &neg, 1);
    if (inner) return 0;
    if (div) {
        for (int i = 0; i < nr; i++) if (rd[i]) return 0;           /* ROUNDED on a quotient: the stack */
        HNode *h = &g_hn[root];
        if (!g_hn[h->r].op && g_hn[h->r].o.kind != O_REF && hx_mag(&g_hn[h->r].o) == 0) return 0;   /* a literal zero divisor */
    }
    int mode = 1;
    if (wide) {                                     /* wrap if every receiver wraps as the stack's store does, else checked */
        if (div || b >= 9.0e18L) mode = 2;
        for (int i = 0; i < nr && mode == 1; i++) {
            Sym *d = rs[i].sym;
            if (!sym_notrunc(d) || !d->pi.is_signed || !is_hot_int(d)) mode = 2;
        }
    }
    *nonneg = !neg;
    for (int i = 0; i < nr; i++) if (rs[i].rm || rs[i].sym->pi.scale != 0 || !ref_hot_store(&rs[i], 0, !neg)) return 0;
    if (rem && (rem->rm || rem->sym->pi.scale != 0 || !ref_hot_store(rem, 0, !neg))) return 0;
    /* the hardware rem is the signed quotient's remainder: under 85 an
     * unsigned quotient item's remainder is from its magnitude (DIVIDE
     * rule 6; 2002 and later take the signed one, as rem does) */
    if (rem && !rs[0].sym->pi.is_signed && g_std < 2002) return 0;
    *bound = wide ? -1 : (long long)b;
    return mode;
}
/* emit the tree and store it (and the remainder); after hx_ok said yes */
static void hx_store(int root, Ref *rs, int *rd, int nr, Ref *rem, long long bound, int nonneg, int slow)
{
    HNode *h = &g_hn[root];
    if (h->op != '/') {
        hx_emit(root, slow);
        emit("\tstw sp+%d, r1", SLOT_A);
        emit_store_receivers(rs, rd, nr, 1, 1, 0, 0, bound, nonneg);
        return;
    }
    if (g_slot_base + 2 > NSLOTS) die_at(cur()->line, "internal: an arithmetic expression nests too deeply for the frame");
    int t = g_slot_base++, tr = g_slot_base++;
    hx_emit(h->l, slow);
    emit("\tstw sp+%d, r1", SLOT(t));
    hx_emit(h->r, slow);
    emit("\tadd r2, r1, r0");
    emit("\tldw r1, sp+%d", SLOT(t));
    int Lskip = new_label(), Ldiv = new_label(), Ldone = new_label();
    emit("\tbeq r2, r0, .L%d", Lskip);                      /* a zero divisor: the receivers stay */
    emit("\taddi r3, r0, -1");
    emit("\tbne r2, r3, .L%d", Ldiv);
    if (slow >= 0) { emit_li("r3", -2147483647L - 1); emit("\tbeq r1, r3, .L%d", slow); }   /* INT_MIN / -1 */
    emit("\tsub r1, r0, r1");                                /* by -1: negation, no remainder */
    emit("\tstw sp+%d, r0", SLOT(tr));
    emit_jump(Ldone);
    emit_label(Ldiv);
    emit("\trem r3, r1, r2");
    emit("\tstw sp+%d, r3", SLOT(tr));
    emit("\tdiv r1, r1, r2");
    emit_label(Ldone);
    emit("\tstw sp+%d, r1", SLOT_A);
    emit_store_receivers(rs, rd, nr, 1, 1, 0, 0, bound, nonneg);
    if (rem) {
        int zero = 0;
        emit("\tldw r1, sp+%d", SLOT(tr));
        emit("\tstw sp+%d, r1", SLOT_A);
        emit_store_receivers(rem, &zero, 1, 1, 1, 0, 0, bound, nonneg);
    }
    emit_label(Lskip);
    g_slot_base -= 2;
}
/* a leaf for a statement's operand or receiver */
static int hx_leaf(const Opnd *o) { return opnd_hot_int((Opnd *)o) && !opnd_scanned(o) ? hn_new(0, -1, -1, o) : -2; }
static int hx_leaf_ref(const Ref *r)
{
    Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = *r; o.line = r->line;
    return hx_leaf(&o);
}

/* ---- decimal arithmetic in registers -----------------------------------
 * What the integer path above cannot take -- decimals, packed and DISPLAY
 * items of up to 18 digits, ROUNDED -- computed as 64-bit scaled integers
 * in register pairs, the scales known when compiling, instead of on the
 * decimal stack.  Only where the answer is provably the stack's: every
 * intermediate (aligned operands, sums, products) bounded below 9*10^18,
 * a product's scale at most 18, so none of the stack's shedding happens.
 * Each operand is fetched by cob_get_num and the result stored by
 * cob_put_num_x -- the stack's own fetch and store, which round, truncate
 * and edit, and which the DBT runs natively -- so what goes is the stack
 * between them: its pushes, alignment and dispatch.  A division, only at
 * the top, is the stack's own (cob_xdiv), its scale found at run time.
 * SIZE ERROR and the EC checks keep the stack. */
static int g_dsc[MAXHN]; static long double g_dbd[MAXHN];
static int dx_leaf_ok(const Opnd *o)
{
    if (o->kind == O_FIG) return !strncmp(o->tok->s, "zero", 4);
    if (o->kind == O_NUM) return o->num.ndigits <= 18 && o->num.scale >= 0 && o->num.scale <= 18;
    if (o->kind != O_REF || o->ref.rm) return 0;
    Sym *s = o->ref.sym;
    if (s->is_group || s->pi.category != PIC_NUMERIC || sym_wide(s) || s->pi.digits > 18 || s->pi.scale < 0 || strchr(s->pi.pat, 'P')) return 0;
    switch (s->usage) {
    case U_DISPLAY: case U_BINARY: case U_PACKED: case U_COMP5: case U_SINT: case U_UINT:
    case U_SSHORT: case U_USHORT: case U_BCHAR: case U_UBCHAR: return 1;
    default: return 0;
    }
}
static int dx_leaf(const Opnd *o);
static long double dx_p10(int k) { long double r = 1; while (k-- > 0) r *= 10; return r; }
#define DX_LIM 9.0e18L
/* scale and bound of node n (the bound in units of its scale); 0 when the
 * stack could answer otherwise */
static int dx_check(int n, int top)
{
    HNode *h = &g_hn[n];
    if (!h->op) {
        const Opnd *o = &h->o;
        if (o->kind == O_FIG) { g_dsc[n] = 0; g_dbd[n] = 0; return 1; }
        if (o->kind == O_NUM) { long long v = numlit_scaled(&o->num); g_dsc[n] = o->num.scale; g_dbd[n] = v < 0 ? -(long double)v : v; return 1; }
        Sym *s = o->ref.sym;
        g_dsc[n] = s->pi.scale;
        if (sym_notrunc(s)) g_dbd[n] = s->size >= 8 ? DX_LIM : dx_p10(0) * (long double)(1ULL << (8 * s->size));
        else g_dbd[n] = dx_p10(s->pi.digits) - 1;
        return g_dbd[n] < DX_LIM;
    }
    if (h->op == 'n' || h->op == 'A') { if (!dx_check(h->l, 0)) return 0; g_dsc[n] = g_dsc[h->l]; g_dbd[n] = g_dbd[h->l]; return 1; }
    if (h->op == 'I' || h->op == 'T') {                 /* to an integer: the bound shrinks by the scale, plus one for the floor */
        if (!dx_check(h->l, 0)) return 0;
        g_dsc[n] = 0; g_dbd[n] = g_dbd[h->l] / dx_p10(g_dsc[h->l]) + 1;
        return 1;
    }
    if (h->op == 'M' || h->op == 'R') {                 /* integers only; the divisor a literal */
        if (!dx_check(h->l, 0) || g_dsc[h->l] != 0) return 0;
        long long d = numlit_int(&g_hn[h->r].o.num);
        g_dsc[n] = 0; g_dbd[n] = (long double)(d < 0 ? -d : d) - 1;
        return 1;
    }
    if (!dx_check(h->l, 0) || !dx_check(h->r, 0)) return 0;
    int sl = g_dsc[h->l], sr = g_dsc[h->r];
    long double bl = g_dbd[h->l], br = g_dbd[h->r];
    if (h->op == '/') { if (!top) return 0; g_dsc[n] = -1; g_dbd[n] = 0; return 1; }
    if (h->op == '*') {
        if (sl + sr > 18) return 0;
        g_dsc[n] = sl + sr; g_dbd[n] = bl * br;
        return g_dbd[n] < DX_LIM;
    }
    int sc = sl > sr ? sl : sr;
    bl *= dx_p10(sc - sl); br *= dx_p10(sc - sr);
    g_dsc[n] = sc; g_dbd[n] = bl + br;
    return bl < DX_LIM && br < DX_LIM && g_dbd[n] < DX_LIM;
}
/* x = x * y, 64 bits, pairs of registers; r7-r9 scratch */
static void emit_mul64(const char *xl, const char *xh, const char *yl, const char *yh)
{
    emit("\tmul r7, %s, %s", xl, yl);
    emit("\tmulhu r8, %s, %s", xl, yl);
    emit("\tmul r9, %s, %s", xl, yh);
    emit("\tadd r8, r8, r9");
    emit("\tmul r9, %s, %s", xh, yl);
    emit("\tadd r8, r8, r9");
    emit("\tadd %s, r7, r0", xl);
    emit("\tadd %s, r8, r0", xh);
}
static void emit_li64(const char *lo, const char *hi, long long v)
{
    emit_li(lo, (long)(int)(unsigned)(unsigned long long)v);
    emit_li(hi, (long)(int)(unsigned)((unsigned long long)v >> 32));
}
/* the pair scaled up by 10^k */
static void emit_scale64(const char *xl, const char *xh, int k)
{
    if (k <= 0) return;
    long long p = 1; for (int i = 0; i < k; i++) p *= 10;
    emit_li64("r3", "r4", p);
    emit_mul64(xl, xh, "r3", "r4");
}
static void dx_get_edited_call(const Sym *s);
/* r1:r2 = node n at its scale */
static void dx_emit(int n)
{
    HNode *h = &g_hn[n];
    if (!h->op) {
        const Opnd *o = &h->o;
        if (o->kind == O_FIG) { emit_li("r1", 0); emit_li("r2", 0); return; }
        if (o->kind == O_NUM) { emit_li64("r1", "r2", numlit_scaled(&o->num)); return; }
        if (opnd_hot_int((Opnd *)o)) { emit_hot_value((Opnd *)o); emit("\tsrai r2, r1, 31"); return; }
        emit_ref_addr(&o->ref, "r3");
        if (o->ref.sym->pi.category == PIC_NUMERIC_EDITED) { dx_get_edited_call(o->ref.sym); return; }
        emit_desc_addr("r4", sym_desc(o->ref.sym));
        emit_call("cob_get_num");
        return;
    }
    dx_emit(h->l);
    if (h->op == 'A') {
        int L = new_label();
        emit("\tbge r2, r0, .L%d", L);
        emit("\tsltu r8, r0, r1");
        emit("\tsub r1, r0, r1"); emit("\tsub r2, r0, r2"); emit("\tsub r2, r2, r8");
        emit_label(L);
        return;
    }
    if (h->op == 'I' || h->op == 'T') {
        int sc = g_dsc[h->l];
        if (sc <= 0) return;
        long long p = 1; for (int i = 0; i < sc; i++) p *= 10;
        if (h->op == 'I') {                             /* floor: a negative value less p - 1 first, then truncate */
            int L = new_label();
            emit("\tbge r2, r0, .L%d", L);
            emit_li64("r5", "r6", -(p - 1));
            emit("\tadd r7, r1, r5"); emit("\tsltu r8, r7, r1");
            emit("\tadd r2, r2, r6"); emit("\tadd r2, r2, r8"); emit("\tadd r1, r7, r0");
            emit_label(L);
        }
        emit("\tadd r3, r1, r0"); emit("\tadd r4, r2, r0");
        emit_li64("r5", "r6", p);
        emit_call("__divdi3");
        return;
    }
    if (h->op == 'M' || h->op == 'R') {
        long long d = numlit_int(&g_hn[h->r].o.num);
        emit("\tadd r3, r1, r0"); emit("\tadd r4, r2, r0");
        emit_li64("r5", "r6", d);
        emit_call("__moddi3");                          /* the remainder, the dividend's sign */
        if (h->op == 'M') {                             /* floor: a nonzero remainder takes the divisor's sign */
            int L = new_label();
            emit("\tor r7, r1, r2");
            emit("\tbeq r7, r0, .L%d", L);
            emit("\t%s r2, r0, .L%d", d > 0 ? "bge" : "blt", L);
            emit_li64("r5", "r6", d);
            emit("\tadd r7, r1, r5"); emit("\tsltu r8, r7, r1");
            emit("\tadd r2, r2, r6"); emit("\tadd r2, r2, r8"); emit("\tadd r1, r7, r0");
            emit_label(L);
        }
        return;
    }
    if (h->op == 'n') {
        emit("\tsltu r8, r0, r1");
        emit("\tsub r1, r0, r1"); emit("\tsub r2, r0, r2"); emit("\tsub r2, r2, r8");
        return;
    }
    if (g_slot_base + 2 > NSLOTS) die_at(cur()->line, "internal: an arithmetic expression nests too deeply for the frame");
    int t = g_slot_base; g_slot_base += 2;
    emit("\tstw sp+%d, r1", SLOT(t)); emit("\tstw sp+%d, r2", SLOT(t + 1));
    dx_emit(h->r);
    emit("\tadd r5, r1, r0"); emit("\tadd r6, r2, r0");
    emit("\tldw r1, sp+%d", SLOT(t)); emit("\tldw r2, sp+%d", SLOT(t + 1));
    g_slot_base -= 2;
    int sl = g_dsc[h->l], sr = g_dsc[h->r];
    if (h->op == '*') { emit_mul64("r1", "r2", "r5", "r6"); return; }
    if (sl < sr) emit_scale64("r1", "r2", sr - sl);
    if (sr < sl) emit_scale64("r5", "r6", sl - sr);
    if (h->op == '+') {
        emit("\tadd r7, r1, r5"); emit("\tsltu r8, r7, r1");
        emit("\tadd r2, r2, r6"); emit("\tadd r2, r2, r8"); emit("\tadd r1, r7, r0");
    } else {
        emit("\tsltu r8, r1, r5");
        emit("\tsub r1, r1, r5"); emit("\tsub r2, r2, r6"); emit("\tsub r2, r2, r8");
    }
}
/* may the tree at root be stored into rs in registers? */
static int dx_ok(int root, Ref *rs, int nr, int size_err)
{
    if (root < 0 || size_err || g_wide || g_nohx || g_rmode || ec_on_name("EC-DATA-INCOMPATIBLE")) return 0;
    if (g_slot_base + hn_depth(root, 2) + 4 > NSLOTS) return 0;     /* too deep for the frame: the stack */
    if (!dx_check(root, 1)) return 0;
    for (int i = 0; i < nr; i++) {
        Sym *d = rs[i].sym;
        if (rs[i].rm || d->is_group || sym_wide(d) || d->usage == U_FLOAT || d->usage == U_NATIONAL || d->usage == U_BIT) return 0;
        if (d->pi.category != PIC_NUMERIC && d->pi.category != PIC_NUMERIC_EDITED) return 0;
    }
    return 1;
}
/* the call that stores r5:r6 (scale in r7, opts in r8) into the item at
 * r3: a numeric-edited one straight to the hooked cob_put_edited, with
 * the locale word libcob keeps (cob_put_num_x would dispatch to it) */
static int g_dx_movestore;              /* dx_move's store: move_desc, not sym_desc */
static void dx_put_call(const Sym *d)
{
    emit_desc_addr("r4", g_dx_movestore ? move_desc((Sym *)d) : sym_desc((Sym *)d));
    if (g_desc[sym_desc((Sym *)d)].cat == COB_NUM_ED) {
        emit_la("r9", "cob_locale_word"); emit("\tldw r9, r9+0");
        emit_call("cob_put_edited");
    } else emit_call("cob_put_num_x");
}
/* the fetch into r1:r2 of a numeric-edited item at r3, as cob_get_num
 * would dispatch it */
static void dx_get_edited_call(const Sym *s)
{
    emit_desc_addr("r4", sym_desc((Sym *)s));
    emit_la("r5", "cob_locale_word"); emit("\tldw r5, r5+0");
    emit_call("cob_get_edited");
}
/* emit the tree and store it into each receiver, as cob_top_store would */
static void dx_store(int root, Ref *rs, int *rd, int nr)
{
    HNode *h = &g_hn[root];
    if (g_slot_base + 3 > NSLOTS) die_at(cur()->line, "internal: an arithmetic expression nests too deeply for the frame");
    int t = g_slot_base; g_slot_base += 3;
    int Lskip = -1;
    if (h->op == '/') {
        dx_emit(h->l);
        emit("\tstw sp+%d, r1", SLOT(t)); emit("\tstw sp+%d, r2", SLOT(t + 1));
        dx_emit(h->r);
        emit("\tadd r5, r1, r0"); emit("\tadd r6, r2, r0");
        emit("\tldw r3, sp+%d", SLOT(t)); emit("\tldw r4, sp+%d", SLOT(t + 1));
        emit_li("r7", g_dsc[h->l]); emit_li("r8", g_dsc[h->r]);
        emit_call("cob_xdiv");
        emit("\tstw sp+%d, r1", SLOT(t)); emit("\tstw sp+%d, r2", SLOT(t + 1));
        emit_la("r3", "cob_xdiv_scale"); emit("\tldw r3, r3+0");
        emit("\tstw sp+%d, r3", SLOT(t + 2));
        Lskip = new_label();
        emit("\tblt r3, r0, .L%d", Lskip);                  /* a zero divisor: the receivers stay */
    } else {
        dx_emit(root);
        emit("\tstw sp+%d, r1", SLOT(t)); emit("\tstw sp+%d, r2", SLOT(t + 1));
    }
    for (int i = 0; i < nr; i++) {
        emit_ref_addr(&rs[i], "r3");
        emit("\tldw r5, sp+%d", SLOT(t)); emit("\tldw r6, sp+%d", SLOT(t + 1));
        if (h->op == '/') emit("\tldw r7, sp+%d", SLOT(t + 2)); else emit_li("r7", g_dsc[root]);
        emit_li("r8", rd[i] ? 1 : 0);
        dx_put_call(rs[i].sym);
    }
    if (Lskip >= 0) emit_label(Lskip);
    g_slot_base -= 3;
}
/* ADD ... TO / SUBTRACT ... FROM: the operands' sum once, then each
 * receiver's value plus (less) it, as cob_top_addto does -- an operand
 * that is also a receiver is read once, before any store */
static int dx_addto_ok(int sum, Ref *rs, int nr, int size_err, int *leaf)
{
    if (!dx_ok(sum, rs, nr, size_err)) return 0;
    if (g_dsc[sum] < 0) return 0;
    for (int i = 0; i < nr; i++) {
        Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = rs[i]; o.line = rs[i].line;
        if (!dx_leaf_ok(&o) || (leaf[i] = hn_new(0, -1, -1, &o)) < 0 || !dx_check(leaf[i], 0)) return 0;
        int sr = g_dsc[leaf[i]], ss = g_dsc[sum], sc = sr > ss ? sr : ss;
        long double a = g_dbd[leaf[i]] * dx_p10(sc - sr), b = g_dbd[sum] * dx_p10(sc - ss);
        if (a >= DX_LIM || b >= DX_LIM || a + b >= DX_LIM) return 0;
    }
    return 1;
}
static void dx_addto(int sum, Ref *rs, int *rd, int nr, int *leaf, int subtract)
{
    if (g_slot_base + 4 > NSLOTS) die_at(cur()->line, "internal: an arithmetic expression nests too deeply for the frame");
    int t = g_slot_base; g_slot_base += 4;
    dx_emit(sum);
    emit("\tstw sp+%d, r1", SLOT(t)); emit("\tstw sp+%d, r2", SLOT(t + 1));
    for (int i = 0; i < nr; i++) {
        int sr = g_dsc[leaf[i]], ss = g_dsc[sum];
        dx_emit(leaf[i]);
        emit("\tldw r5, sp+%d", SLOT(t)); emit("\tldw r6, sp+%d", SLOT(t + 1));
        if (sr < ss) emit_scale64("r1", "r2", ss - sr);
        if (ss < sr) emit_scale64("r5", "r6", sr - ss);
        if (!subtract) {
            emit("\tadd r7, r1, r5"); emit("\tsltu r8, r7, r1");
            emit("\tadd r2, r2, r6"); emit("\tadd r2, r2, r8"); emit("\tadd r1, r7, r0");
        } else {
            emit("\tsltu r8, r1, r5");
            emit("\tsub r1, r1, r5"); emit("\tsub r2, r2, r6"); emit("\tsub r2, r2, r8");
        }
        emit("\tstw sp+%d, r1", SLOT(t + 2)); emit("\tstw sp+%d, r2", SLOT(t + 3));
        emit_ref_addr(&rs[i], "r3");
        emit("\tldw r5, sp+%d", SLOT(t + 2)); emit("\tldw r6, sp+%d", SLOT(t + 3));
        emit_li("r7", sr > ss ? sr : ss);
        emit_li("r8", rd[i] ? 1 : 0);
        dx_put_call(rs[i].sym);
    }
    g_slot_base -= 4;
}
/* the operands' sum, left to right as the stack adds them */
static int dx_sum(Opnd *ops, int n)
{
    int r = dx_leaf_ok(&ops[0]) ? hn_new(0, -1, -1, &ops[0]) : -2;
    for (int i = 1; i < n && r >= 0; i++) r = hn_new('+', r, dx_leaf_ok(&ops[i]) ? hn_new(0, -1, -1, &ops[i]) : -2, NULL);
    return r;
}
static int dx_leaf(const Opnd *o) { return dx_leaf_ok(o) && !opnd_scanned(o) ? hn_new(0, -1, -1, o) : -2; }
/* MOVE of a numeric item, literal or MOD/INTEGER/... to a numeric or
 * numeric-edited item: cob_move does cob_put_num(cob_get_num) there, so
 * the tree's store is the same MOVE (truncating: no ROUNDED) */
static int dx_move(Opnd *src, Ref *dst)
{
    if (g_nohx || g_rmode) return 0;
    g_nhn = 0;
    int root = src->kind == O_FUNC ? hn_fn(src, dx_leaf) : (src->kind == O_REF || src->kind == O_NUM) ? dx_leaf(src) : -2;
    if (root == -2 && src->kind == O_REF && !src->ref.rm && !opnd_scanned(src)) {
        /* a numeric-edited sender (de-edited, as cob_move's cob_get_num does) */
        Sym *s = src->ref.sym;
        if (!s->is_group && s->pi.category == PIC_NUMERIC_EDITED && s->usage == U_DISPLAY && !sym_wide(s) &&
            s->pi.digits <= 18 && s->pi.scale >= 0 && !strchr(s->pi.pat, 'P'))
            root = hn_new(0, -1, -1, src);
    }
    int rd = 0;
    if (root < 0 || !dx_ok(root, dst, 1, 0)) return 0;
    g_dx_movestore = 1; dx_store(root, dst, &rd, 1); g_dx_movestore = 0;
    return 1;
}
static int dx_leaf_ref(const Ref *r)
{
    Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = *r; o.line = r->line;
    return dx_leaf(&o);
}

/* ---- the arithmetic statements as nodes (docs/plans/frontend-pass.md,
 * step 4): ADD, SUBTRACT, MULTIPLY and DIVIDE are read whole -- every
 * operand, receiver and REMAINDER, and whether a SIZE ERROR phrase
 * follows -- before any code, the operands as a scan reads them; then
 * their user function calls are made, in the order they are written,
 * and the statement's code follows.  The SIZE ERROR phrases' statements
 * are still parsed where their code goes. */
typedef struct {
    Opnd ops[MAXOPS]; int n;    /* ADD, SUBTRACT: the operands summed */
    Opnd a, b;                  /* MULTIPLY a BY b; DIVIDE a INTO b, a BY b */
    Opnd minuend;               /* SUBTRACT ... FROM minuend GIVING */
    Ref rs[MAXOPS]; int rd[MAXOPS], nr;
    int giving, into;           /* DIVIDE: INTO (else BY) */
    int has_rem; Ref rem;       /* DIVIDE ... REMAINDER rem */
    int size_err;               /* [NOT] ON SIZE ERROR written, or EC-SIZE checked */
    int comp;                   /* the composite of operands (85 rule 3) */
} Arith;
static void arith_calls(Arith *st, int ab)
{
    if (ab) { ucall_make(&st->a); ucall_make(&st->b); return; }
    for (int i = 0; i < st->n; i++) ucall_make(&st->ops[i]);
    if (st->giving) ucall_make(&st->minuend);
}

/* ADD a ... TO b ... [GIVING c ...]; ADD a ... GIVING c ... */
static void parse_add_node(Arith *st)
{
    Opnd *ops = st->ops; Ref *rs = st->rs; int *rd = st->rd;
    int n = parse_operand_list(ops, MAXOPS);
    if (!n) die_at(cur()->line, "ADD needs an operand");
    int giving = 0, nr = 0;
    if (accept_word("to")) {
        /* b is a receiver unless GIVING follows */
        int save = g_tp;
        Opnd extra[MAXOPS]; int ne = parse_operand_list(extra, MAXOPS);
        if (accept_word("giving")) {
            for (int i = 0; i < ne; i++) { if (n >= MAXOPS) die_at(cur()->line, "too many operands"); ops[n++] = extra[i]; }
            giving = 1;
            nr = parse_ref_list(rs, rd, MAXOPS, 1);
        } else { g_tp = save; nr = parse_ref_list(rs, rd, MAXOPS, 0); }
    } else if (accept_word("giving")) {
        giving = 1; nr = parse_ref_list(rs, rd, MAXOPS, 1);
    } else die_at(cur()->line, "expected TO or GIVING in ADD");
    if (!nr) die_at(cur()->line, "ADD needs a receiving item");
    st->n = n; st->nr = nr; st->giving = giving;
    st->comp = arith_composite(ops, n, rs, giving ? 0 : nr, "ADD", "X3.23-1985 ADD rule 3", rs[0].line);
    st->size_err = at_size_error_clause() || ec_size_on();
}

static void parse_add(void)
{
    if (accept_word("corresponding") || accept_word("corr")) { parse_arith_corr(1, "to", "end-add"); return; }
    Arith st; memset(&st, 0, sizeof st);
    g_noemit++; parse_add_node(&st); g_noemit--;
    arith_calls(&st, 0);
    Opnd *ops = st.ops; Ref *rs = st.rs; int *rd = st.rd;
    int n = st.n, nr = st.nr, giving = st.giving, comp = st.comp, size_err = st.size_err;
    for (int k = 0; k < n; k++) emit_incompat(&ops[k]);
    if (!giving) emit_incompat_refs(rs, nr);            /* ADD a TO b: b is summed too */
    g_wide = (g_std >= 2002 && comp > 18) || opnds_wide(ops, n) || refs_wide(rs, nr);

    int hot = !g_wide && !size_err && !any_rounded(rd, nr) && all_hot(ops, n) &&
              refs_hot(rs, nr, 0, ops_all_nonneg(ops, n)) && hot_sum_fits(ops, n);
    long k = n == 1 && ops[0].kind == O_NUM ? (long)numlit_int(&ops[0].num) : 0;
    g_addk_on = !g_nohx && hot && !giving && n == 1 && ops[0].kind == O_NUM && k > -2048 && k < 2048; g_addk = k;   /* ADD 1 TO x: an immediate */
    if (g_addk_on) { /* the store adds it as an immediate */ }
    else if (hot) emit_hot_sum(ops, n);
    else if (!g_wide && !giving && dec_add_ok(ops, n, rs, nr, size_err)) {
        emit_dec_addto(&ops[0], rs, nr, 0);
        parse_size_error_clauses(size_err, "end-add");
        return;
    }
    else {
        int leaf[MAXOPS];
        g_nhn = 0; int sum = dx_sum(ops, n);
        if (giving && dx_ok(sum, rs, nr, size_err)) { dx_store(sum, rs, rd, nr); parse_size_error_clauses(size_err, "end-add"); return; }
        if (!giving && dx_addto_ok(sum, rs, nr, size_err, leaf)) { dx_addto(sum, rs, rd, nr, leaf, 0); parse_size_error_clauses(size_err, "end-add"); return; }
        for (int i = 0; i < n; i++) { emit_push(&ops[i]); if (i) emit_call("cob_nadd"); }
    }
    emit_store_receivers(rs, rd, nr, hot, giving, 0, size_err, ops_sum_mag(ops, n), ops_all_nonneg(ops, n));
    g_addk_on = 0;
    g_wide = 0; g_fstmt = 0;
    parse_size_error_clauses(size_err, "end-add");
}

/* SUBTRACT a ... FROM b ... ; SUBTRACT a ... FROM b GIVING c ... */
static void parse_subtract_node(Arith *st)
{
    Opnd *ops = st->ops; Ref *rs = st->rs; int *rd = st->rd;
    int n = parse_operand_list(ops, MAXOPS);
    if (!n) die_at(cur()->line, "SUBTRACT needs an operand");
    expect_word("from");
    int giving = 0, nr = 0;
    int save = g_tp;
    Opnd extra[MAXOPS]; int ne = parse_operand_list(extra, MAXOPS);
    if (accept_word("giving")) {
        if (ne != 1) die_at(cur()->line, "SUBTRACT ... FROM x GIVING takes one item after FROM");
        st->minuend = extra[0]; giving = 1;
        nr = parse_ref_list(rs, rd, MAXOPS, 1);
    } else { g_tp = save; nr = parse_ref_list(rs, rd, MAXOPS, 0); }
    if (!nr) die_at(cur()->line, "SUBTRACT needs a receiving item");
    st->n = n; st->nr = nr; st->giving = giving;
    {   /* the composite: every operand, the GIVING items apart (85 rule 3) */
        Opnd all[MAXOPS + 1]; int na = 0;
        for (int k = 0; k < n; k++) all[na++] = ops[k];
        if (giving) all[na++] = st->minuend;
        st->comp = arith_composite(all, na, rs, giving ? 0 : nr, "SUBTRACT", "X3.23-1985 SUBTRACT rule 3", rs[0].line);
    }
    st->size_err = at_size_error_clause() || ec_size_on();
}

static void parse_subtract(void)
{
    if (accept_word("corresponding") || accept_word("corr")) { parse_arith_corr(2, "from", "end-subtract"); return; }
    Arith st; memset(&st, 0, sizeof st);
    g_noemit++; parse_subtract_node(&st); g_noemit--;
    arith_calls(&st, 0);
    Opnd *ops = st.ops; Ref *rs = st.rs; int *rd = st.rd; Opnd minuend = st.minuend;
    int n = st.n, nr = st.nr, giving = st.giving, size_err = st.size_err;
    {
        Opnd all[MAXOPS + 1]; int na = 0;
        for (int k = 0; k < n; k++) all[na++] = ops[k];
        if (giving) all[na++] = minuend;
        for (int k = 0; k < na; k++) emit_incompat(&all[k]);
        if (!giving) emit_incompat_refs(rs, nr);
        g_wide = (g_std >= 2002 && st.comp > 18) || opnds_wide(all, na) || refs_wide(rs, nr);
    }

    int hot = !g_wide && !size_err && !any_rounded(rd, nr) && all_hot(ops, n) &&
              refs_hot(rs, nr, 1, 0) && (!giving || opnd_hot_int(&minuend)) &&
              hot_sum_fits(ops, n);
    if (!hot && !g_wide && !giving && dec_add_ok(ops, n, rs, nr, size_err)) {
        emit_dec_addto(&ops[0], rs, nr, 1);
        parse_size_error_clauses(size_err, "end-subtract");
        return;
    }
    if (hot) {
        emit_hot_sum(ops, n);
        if (giving) {
            emit_hot_value(&minuend);
            emit("\tldw r2, sp+%d", SLOT_A);
            emit("\tsub r1, r1, r2");
            emit("\tstw sp+%d, r1", SLOT_A);
        }
    } else {
        int leaf[MAXOPS];
        g_nhn = 0; int sum = dx_sum(ops, n);
        if (giving) {
            int root = sum >= 0 ? hn_new('-', dx_leaf(&minuend), sum, NULL) : -2;
            if (dx_ok(root, rs, nr, size_err)) { dx_store(root, rs, rd, nr); parse_size_error_clauses(size_err, "end-subtract"); return; }
        } else if (dx_addto_ok(sum, rs, nr, size_err, leaf)) { dx_addto(sum, rs, rd, nr, leaf, 1); parse_size_error_clauses(size_err, "end-subtract"); return; }
        if (giving) emit_push(&minuend);
        for (int i = 0; i < n; i++) { emit_push(&ops[i]); if (i) emit_call("cob_nadd"); }
        if (giving) emit_call("cob_nsub");
    }
    emit_store_receivers(rs, rd, nr, hot, giving, !giving, size_err, -1, 0);
    g_wide = 0; g_fstmt = 0;
    parse_size_error_clauses(size_err, "end-subtract");
}

/* MULTIPLY a BY b ... ; MULTIPLY a BY b GIVING c ... */
static void parse_multiply_node(Arith *st)
{
    parse_operand(&st->a); check_numeric_opnd(&st->a);
    expect_word("by");
    int save = g_tp;
    parse_operand(&st->b); check_numeric_opnd(&st->b);
    if (accept_word("giving")) {
        st->giving = 1;
        st->nr = parse_ref_list(st->rs, st->rd, MAXOPS, 1);
        if (!st->nr) die_at(cur()->line, "MULTIPLY needs a receiving item");
    } else {
        g_tp = save; memset(&st->b, 0, sizeof st->b);
        st->nr = parse_ref_list(st->rs, st->rd, MAXOPS, 0);
        if (!st->nr) die_at(cur()->line, "MULTIPLY needs a receiving item");
    }
    st->comp = arith_composite(NULL, 0, st->rs, st->nr, "MULTIPLY", "X3.23-1985 MULTIPLY rule 3", st->rs[0].line);   /* the receiving items */
    st->size_err = at_size_error_clause() || ec_size_on();
}

static void parse_multiply(void)
{
    Arith st; memset(&st, 0, sizeof st);
    g_noemit++; parse_multiply_node(&st); g_noemit--;
    arith_calls(&st, 1);
    Opnd a = st.a, b = st.b; Ref *rs = st.rs; int *rd = st.rd;
    int nr = st.nr, comp = st.comp, size_err = st.size_err;
    if (st.giving) {
        emit_incompat(&a); emit_incompat(&b);
        { Opnd ab[2] = { a, b }; g_wide = (g_std >= 2002 && comp > 18) || opnds_wide(ab, 2) || refs_wide(rs, nr) || prod_wide(&a, &b); }
        g_nhn = 0; int root = hn_new('*', hx_leaf(&a), hx_leaf(&b), NULL); long long bd; int nn;
        int mode = hx_ok(root, rs, rd, nr, NULL, size_err, &bd, &nn), Lslow = -1, Ldone = -1;
        if (mode) { if (mode == 2) Lslow = new_label(); hx_store(root, rs, rd, nr, NULL, bd, nn, Lslow); }
        if (mode == 2) { Ldone = new_label(); emit_jump(Ldone); emit_label(Lslow); }
        int dxr = -2;
        if (!mode) { g_nhn = 0; dxr = hn_new('*', dx_leaf(&a), dx_leaf(&b), NULL); }
        if (!mode && dx_ok(dxr, rs, nr, size_err)) dx_store(dxr, rs, rd, nr);
        else if (mode != 1) {
        emit_push(&a); emit_push(&b); emit_call("cob_nmul");
        emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err, -1, 0);
        }
        if (mode == 2) emit_label(Ldone);
        g_wide = 0; g_fstmt = 0;
        parse_size_error_clauses(size_err, "end-multiply");
        return;
    }
    emit_incompat(&a); emit_incompat_refs(rs, nr);
    g_wide = (g_std >= 2002 && comp > 18) || opnds_wide(&a, 1) || refs_wide(rs, nr);
    for (int i = 0; i < nr && !g_wide; i++) { Opnd ro = ref_opnd(&rs[i]); if (prod_wide(&a, &ro)) g_wide = 1; }
    if (size_err) emit("\tstw sp+%d, r0", SLOT_B);
    for (int i = 0; i < nr; i++) {
        g_nhn = 0; int root = hn_new('*', hx_leaf_ref(&rs[i]), hx_leaf(&a), NULL); long long bd; int nn;
        int mode = hx_ok(root, &rs[i], &rd[i], 1, NULL, size_err, &bd, &nn), Lslow = -1, Ldone = -1;
        if (mode) { if (mode == 2) Lslow = new_label(); hx_store(root, &rs[i], &rd[i], 1, NULL, bd, nn, Lslow); }
        if (mode == 1) continue;
        if (!mode) {
            g_nhn = 0; int dxr = hn_new('*', dx_leaf_ref(&rs[i]), dx_leaf(&a), NULL);
            if (dx_ok(dxr, &rs[i], 1, size_err)) { dx_store(dxr, &rs[i], &rd[i], 1); continue; }
        }
        if (mode == 2) { Ldone = new_label(); emit_jump(Ldone); emit_label(Lslow); }
        Opnd r; memset(&r, 0, sizeof r); r.kind = O_REF; r.ref = rs[i]; r.line = rs[i].line;
        emit_push(&r); emit_push(&a); emit_call("cob_nmul");
        emit_top_op(&rs[i], "cob_top_store", rnd_opts(rd[i]) | (size_err ? 2 : 0)); emit_call("cob_drop");
        if (mode == 2) emit_label(Ldone);
    }
    g_wide = 0; g_fstmt = 0;
    parse_size_error_clauses(size_err, "end-multiply");
}

/* REMAINDER r: dividend - (quotient as stored, truncated) * divisor */
/* REMAINDER r: the dividend less the product of the divisor and the
 * quotient as it would be stored *before* ROUNDED -- the quotient
 * truncated to the receiver's decimals (X3.23 6.9.4), recomputed here
 * rather than read back from the receiver */
static void emit_remainder(Opnd *dividend, Ref *q, int q_rounded, Opnd *divisor, int size_err, const Ref *rem)
{
    if (!rem) return;
    {   /* Micro Focus: formats 4 and 5 take no floating-point item */
        const Sym *f = dividend->kind == O_REF && dividend->ref.sym->usage == U_FLOAT ? dividend->ref.sym
                     : divisor->kind == O_REF && divisor->ref.sym->usage == U_FLOAT ? divisor->ref.sym
                     : q->sym->usage == U_FLOAT ? q->sym : NULL;
        if (f) die_at(q->line, "DIVIDE ... REMAINDER takes no floating-point item ('%s'; Micro Focus: DIVIDE rules)", f->name);
    }
    (void)q_rounded;
    Ref r = *rem;
    if (r.sym->is_group || (r.sym->pi.category != PIC_NUMERIC && r.sym->pi.category != PIC_NUMERIC_EDITED))
        die_at(r.line, "REMAINDER '%s' is not numeric (or numeric-edited)", r.sym->name);
    int was_wide = g_wide;
    if (!r.rm && sym_wide(r.sym)) g_wide = 1;           /* computed afresh: on the wide stack when the remainder needs it */
    {   /* ...or when the product quotient x divisor does, though every item
         * is under 18 digits: 8271765550.5 / 605815.934675 into 9(8)V9(6)
         * is 13653.925354 x 605815.934675, 22 digits at scale 12, and it
         * wrapped -- the remainder came out 32.96613, not 0.18380
         * (tests/gen found it; tests/free/divremse).  The quotient's
         * integer digits are at most the dividend's plus the divisor's
         * decimals plus one; the product adds the divisor's integer digits
         * and has the quotient's scale plus the divisor's. */
        int di = 0, df = 0, vi = 0, vf = 0;
        opnd_int_frac(dividend, &di, &df); opnd_int_frac(divisor, &vi, &vf);
        int qs = q->sym->pi.scale > 0 ? q->sym->pi.scale : 0;
        if ((di + vf + 1) + vi + qs + vf > 18) g_wide = 1;
    }
    emit_push(dividend);
    emit_push(dividend); emit_push(divisor); emit_call("cob_ndiv");
    emit_li("r3", q->sym->pi.scale); emit_call("cob_ntrunc");
    /* COBOL 85: the quotient (identifier-3), or the intermediate field
     * with its presence or absence of a sign -- an unsigned quotient
     * item's is the magnitude (X3.23-1985 VI-81, DIVIDE rule 6).  2002
     * and 2023 make it a signed subsidiary quotient (2023 14.9.12,
     * general rules 6c, 7).  The user's ruling, 2026-09-30: each edition
     * as its text says; CCVS is silent, and GnuCOBOL takes the signed
     * quotient under 85 too (docs/oracles.md, free/divremu) */
    if (!q->sym->pi.is_signed && g_std < 2002) emit_call("cob_nabs");
    emit_push(divisor); emit_call("cob_nmul");
    emit_call("cob_nsub");
    /* ON SIZE ERROR: a quotient that overflowed leaves the remainder alone;
     * a remainder that overflows is the statement's size error too */
    int Lskip = new_label();
    if (size_err) { emit("\tldw r1, sp+%d", SLOT_B); emit("\tbne r1, r0, .L%d", Lskip); }
    emit_top_op(&r, "cob_top_store", size_err ? 2 : 0);
    emit_label(Lskip);
    emit_call("cob_drop");
    g_wide = was_wide; if (!was_wide) g_fstmt = 0;
}

/* DIVIDE a INTO b ... ; DIVIDE a INTO b GIVING c ... ; DIVIDE a BY b
 * GIVING c ... -- the GIVING forms with REMAINDER r after one receiver */
static void parse_divide_node(Arith *st)
{
    parse_operand(&st->a); check_numeric_opnd(&st->a);
    if (accept_word("into")) {
        int save = g_tp;
        parse_operand(&st->b); check_numeric_opnd(&st->b);
        if (!accept_word("giving")) {
            g_tp = save; memset(&st->b, 0, sizeof st->b);
            st->nr = parse_ref_list(st->rs, st->rd, MAXOPS, 0);
            if (!st->nr) die_at(cur()->line, "DIVIDE needs a receiving item");
            st->comp = arith_composite(NULL, 0, st->rs, st->nr, "DIVIDE", "X3.23-1985 DIVIDE rule 3", st->rs[0].line);
            st->size_err = at_size_error_clause() || ec_size_on();
            return;
        }
        st->into = 1;
    } else {
        expect_word("by");
        parse_operand(&st->b); check_numeric_opnd(&st->b);
        expect_word("giving");
    }
    st->giving = 1;
    st->nr = parse_ref_list(st->rs, st->rd, MAXOPS, 1);
    if (!st->nr) die_at(cur()->line, "DIVIDE needs a receiving item");
    if (at_word("remainder") && st->nr > 1) die_at(cur()->line, "DIVIDE ... REMAINDER takes one GIVING item (X3.23-1985 DIVIDE formats 4 and 5)");
    st->comp = arith_composite(NULL, 0, st->rs, st->nr, "DIVIDE", "X3.23-1985 DIVIDE rule 3", st->rs[0].line);
    if (accept_word("remainder")) { st->has_rem = 1; parse_ref(&st->rem); }
    st->size_err = at_size_error_clause() || ec_size_on();
}

/* the GIVING forms: dividend / divisor into the receivers, and the
 * remainder */
static void emit_divide_giving(Arith *st, Opnd *dividend, Opnd *divisor)
{
    Ref *rs = st->rs; int *rd = st->rd; int nr = st->nr, size_err = st->size_err;
    emit_incompat(&st->a); emit_incompat(&st->b);
    { Opnd ab[2] = { st->a, st->b }; g_wide = (g_std >= 2002 && st->comp > 18) || opnds_wide(ab, 2) || refs_wide(rs, nr) || round_wide(rs, rd, nr); }
    g_nhn = 0; int root = hn_new('/', hx_leaf(dividend), hx_leaf(divisor), NULL); long long bd; int nn;
    Ref *rr = st->has_rem ? &st->rem : NULL;
    int mode = hx_ok(root, rs, rd, nr, rr, size_err, &bd, &nn), Lslow = -1, Ldone = -1;
    if (mode) { if (mode == 2) Lslow = new_label(); hx_store(root, rs, rd, nr, rr, bd, nn, Lslow); }
    if (mode == 2) { Ldone = new_label(); emit_jump(Ldone); emit_label(Lslow); }
    int dxr = -2;
    if (!mode && !rr) { g_nhn = 0; dxr = hn_new('/', dx_leaf(dividend), dx_leaf(divisor), NULL); }
    if (!mode && !rr && dx_ok(dxr, rs, nr, size_err)) dx_store(dxr, rs, rd, nr);
    else if (mode != 1) {
        emit_push(dividend); emit_push(divisor); emit_call("cob_ndiv");
        emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err, -1, 0);
        emit_remainder(dividend, &rs[0], rd[0], divisor, size_err, rr);
    }
    if (mode == 2) emit_label(Ldone);
    g_wide = 0; g_fstmt = 0;
}

static void parse_divide(void)
{
    Arith st; memset(&st, 0, sizeof st);
    g_noemit++; parse_divide_node(&st); g_noemit--;
    arith_calls(&st, 1);
    if (st.giving) {
        if (st.into) emit_divide_giving(&st, &st.b, &st.a); else emit_divide_giving(&st, &st.a, &st.b);
        parse_size_error_clauses(st.size_err, "end-divide");
        return;
    }
    Opnd a = st.a; Ref *rs = st.rs; int *rd = st.rd;
    int nr = st.nr, size_err = st.size_err;
    emit_incompat(&a); emit_incompat_refs(rs, nr);
    g_wide = (g_std >= 2002 && st.comp > 18) || opnds_wide(&a, 1) || refs_wide(rs, nr) || round_wide(rs, rd, nr);
    if (size_err) emit("\tstw sp+%d, r0", SLOT_B);
    for (int i = 0; i < nr; i++) {
        g_nhn = 0; int root = hn_new('/', hx_leaf_ref(&rs[i]), hx_leaf(&a), NULL); long long bd; int nn;
        int mode = hx_ok(root, &rs[i], &rd[i], 1, NULL, size_err, &bd, &nn), Lslow = -1, Ldone = -1;
        if (mode) { if (mode == 2) Lslow = new_label(); hx_store(root, &rs[i], &rd[i], 1, NULL, bd, nn, Lslow); }
        if (mode == 1) continue;
        if (!mode) {
            g_nhn = 0; int dxr = hn_new('/', dx_leaf_ref(&rs[i]), dx_leaf(&a), NULL);
            if (dx_ok(dxr, &rs[i], 1, size_err)) { dx_store(dxr, &rs[i], &rd[i], 1); continue; }
        }
        if (mode == 2) { Ldone = new_label(); emit_jump(Ldone); emit_label(Lslow); }
        Opnd r; memset(&r, 0, sizeof r); r.kind = O_REF; r.ref = rs[i]; r.line = rs[i].line;
        emit_push(&r); emit_push(&a); emit_call("cob_ndiv");
        emit_top_op(&rs[i], "cob_top_store", rnd_opts(rd[i]) | (size_err ? 2 : 0)); emit_call("cob_drop");
        if (mode == 2) emit_label(Ldone);
    }
    g_wide = 0; g_fstmt = 0;
    parse_size_error_clauses(size_err, "end-divide");
}
