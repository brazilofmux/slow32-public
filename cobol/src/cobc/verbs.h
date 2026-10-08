/* s32-cobc: EVALUATE, INSPECT, INITIALIZE, SEARCH.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- EVALUATE ---------------------------------------------------------- */

typedef struct { int kind; Opnd o; Cond *c; } Subject;      /* kind: 0 value, 1 TRUE, 2 FALSE, 3 a condition */

static Cond *cond_never(void)
{
    Opnd z, one; memset(&z, 0, sizeof z); memset(&one, 0, sizeof one);
    z.kind = O_NUM; numlit_zero(&z.num); one.kind = O_NUM; numlit_from_int(&one.num, 1);
    return cond_rel(&z, R_EQ, &one, 0);
}

/* an EVALUATE operand's class for THRU (2023 14.9.13.3 rule 4): 1 numeric, 0 not */
static int eval_numeric(const Opnd *o)
{
    if (o->kind == O_NUM || o->kind == O_EXPR) return 1;
    if (o->kind == O_FIG) return !strncmp(o->tok->s, "zero", 4) ? -1 : 0;     /* ZERO goes with either */
    if (o->kind == O_REF) return o->ref.rm ? 0 : is_numeric_sym(o->ref.sym);
    if (o->kind == O_FUNC) return o->fn == -1 || fn_is_numeric(o->fn);
    return 0;
}
static int eval_literal(const Opnd *o) { return o->kind == O_NUM || o->kind == O_STR || o->kind == O_FIG || o->kind == O_ALL; }
static int at_relational_tok(const Tok *t)
{
    static const char *w[] = { "greater", "less", "equal", "equals", "numeric", "alphabetic", "alphabetic-lower", "alphabetic-upper", "boolean",
                               "positive", "negative", "farthest-from-zero", "nearest-to-zero", "in-arithmetic-range", "float-infinity",
                               "float-not-a-number", "float-not-a-number-quiet", "float-not-a-number-signaling", NULL };   /* not ZERO: WHEN ZERO is a figurative constant */
    if (t->kind == T_OP && (!strcmp(t->s, "=") || !strcmp(t->s, "<") || !strcmp(t->s, ">") || !strcmp(t->s, "<=") || !strcmp(t->s, ">=") || !strcmp(t->s, "<>"))) return 1;
    if (t->kind != T_WORD) return 0;
    for (int k = 0; w[k]; k++) if (!strcmp(t->s, w[k])) return 1;
    for (int i = 0; i < g_nclass; i++) if (!strcmp(t->s, g_class[i].name)) return 1;   /* a SPECIAL-NAMES class */
    return 0;
}
static int at_relational(void)
{
    return at_relational_tok(cur());
}

/* An EVALUATE subject that is an arithmetic expression or a numeric
 * function is assigned its value at the beginning (2023 14.9.13.4 rule
 * 3c), once: every WHEN compares against that value.  Evaluated again
 * for each WHEN -- twice for a THRU -- EVALUATE FUNCTION INTEGER
 * (FUNCTION RANDOM * 6) + 1 rolled a new die for each of WHEN 1 ... WHEN
 * 6, and a third of the time matched none.  The value is taken off the
 * numeric stack into a compiler-made record and pushed again from it
 * (cob_nsave, cob_npush_saved). */
static void evaluate_subject_once(Opnd *o)
{
    if (o->kind == O_FUNC && opnd_fn_numeric(o)) {
        /* a numeric function alone: as an expression of one operand */
        Expr *e = ex_node(0, NULL, NULL);
        e->o = ex_alloc(sizeof *e->o); *e->o = *o;
        int sw = g_saw_wide, sf = g_saw_float; g_saw_wide = g_saw_float = 0;
        g_noemit++; emit_push(e->o); g_noemit--;        /* what a scan of it notes: its width */
        e->wide = g_saw_wide; e->flt = g_saw_float;
        g_saw_wide |= sw; g_saw_float |= sf;
        Opnd x; memset(&x, 0, sizeof x);
        x.kind = O_EXPR; x.line = o->line; x.ex = e; x.wide = e->wide; x.flt = e->flt;
        *o = x;
    }
    if (o->kind == O_FUNC) {
        /* an alphanumeric, national or boolean function: its result kept,
         * bytes and run-time length, and made the result just evaluated
         * again for each comparison (cob_fn_keep, cob_fn_kept) -- a WHEN's
         * objects may evaluate functions of their own in between */
        int sz = o->fsize > 0 ? o->fsize : 1;
        FDesc kd; memset(&kd, 0, sizeof kd); kd.group = 1; kd.size = 4 + sz;
        Sym *k = ftemp_new(&kd, o->line);
        emit_fn_value(o);
        emit("\tadd r4, r1, r0");
        emit_item_addr("r3", k, k->offset);
        emit_li("r5", sz);
        emit_call("cob_fn_keep");
        o->fkept = k;
        return;
    }
    if (o->kind != O_EXPR) return;
    FDesc fd; memset(&fd, 0, sizeof fd); fd.group = 1; fd.size = 64;       /* a cob_wnum, with room */
    Sym *t = ftemp_new(&fd, o->line);
    int was = g_wide;
    g_wide = was || opnds_wide(o, 1);
    emit_push_opnd(o);
    emit_item_addr("r3", t, t->offset);
    emit_call("cob_nsave");
    g_wide = was; if (!was) g_fstmt = 0;
    o->nsave = t;
}

static int lw_evaluate(int b0, const Block *pre, const Block *body, Cond **c, const int *other, int nwh);   /* lower.h */
static void parse_evaluate(void)
{
    Subject subj[8]; int ns = 0, subj_lit[8];
    int lw_b0 = g_nasm;                     /* lower.h: code the subjects make from here keeps the statement text */
    int o85 = g_std < 2002;
    const char *r_cnt = o85 ? "X3.23-1985 EVALUATE syntax rule 5" : "2023 14.9.13.3 rule 2";
    const char *r_thru = o85 ? "X3.23-1985 EVALUATE syntax rule 4" : "2023 14.9.13.3 rule 4";
    const char *r_cmp = o85 ? "X3.23-1985 EVALUATE syntax rule 6a" : "2023 14.9.13.3, Table 15";
    const char *r_cond = o85 ? "X3.23-1985 EVALUATE syntax rule 6b" : "2023 14.9.13.3, Table 15";
    for (;;) {
        if (ns >= 8) die_at(cur()->line, "too many EVALUATE subjects");
        if (accept_word("true")) subj[ns].kind = 1;
        else if (accept_word("false")) subj[ns].kind = 2;
        else {
            int start = g_tp;
            /* written as a literal (a folded FUNCTION LENGTH("...") is an
             * identifier, CCVS-85 IF115A) */
            Tok *st = cur();
            subj_lit[ns] = st->kind == T_NUM || st->kind == T_STR || (st->kind == T_WORD && (is_figurative(st->s) || !strcmp(st->s, "all")));
            /* read as a scan: whether it is an operand or a condition's
             * beginning is known only after it, and a condition is read
             * again whole -- its user functions called once, by that */
            subj[ns].kind = 0;
            g_noemit++; subj[ns].o = parse_cond_operand(); g_noemit--;
            /* an operand followed by a class word or a relation is a condition
             * subject, matched by WHEN TRUE / WHEN FALSE */
            static const char *cw[] = { "numeric", "alphabetic", "alphabetic-lower", "alphabetic-upper", "positive", "negative",
                "is", "not", "equal", "equals", "greater", "less", "=", "<", ">", "<=", ">=", "<>", NULL };
            int is_cond = 0;
            if (cur()->kind == T_WORD || cur()->kind == T_OP) for (int k = 0; cw[k]; k++) if (!strcmp(cur()->s, cw[k])) is_cond = 1;
            if (cur()->kind == T_WORD && switch_find(cur()->s)) is_cond = 0;
            /* a condition-name alone is a condition subject too (NC225A: ALSO IT-IS-81 ... WHEN ... ALSO TRUE) */
            if (subj[ns].o.kind == O_REF && subj[ns].o.ref.sym->is_cond) is_cond = 1;
            if (is_cond) {
                g_tp = start; subj[ns].kind = 3; subj[ns].c = parse_cond();
                /* a subject is evaluated once, at the beginning (2023
                 * 14.9.13.4 rule 3): the condition's user functions are
                 * called here, and each WHEN tests it without them */
                Cond *sc = subj[ns].c;
                if (sc->uc1 > sc->uc0) { emit_ucalls(sc->uc0, sc->uc1); sc->uc0 = sc->uc1 = 0; }
            }
            else {
                ucall_make(&subj[ns].o);            /* an operand: its calls made here, at the start */
                evaluate_subject_once(&subj[ns].o);
            }
        }
        ns++;
        if (!accept_word("also")) break;
    }
    /* the WHEN phrases, read whole before their code (docs/plans/
     * frontend-pass.md, step 4): each one's objects -- with any code
     * reading them makes, a user function's call -- its test, and its
     * statements as a Block */
    typedef struct { Block pre, body; Cond *c; int other; } When;
    When *wh = NULL; int nwh = 0, whcap = 0;
    while (at_word("when")) {
        Cond *group = NULL; int other = 0;
        int pre0 = block_begin();
        CallList when_outer = calls_scope_begin();      /* its objects' calls stay here, made when this WHEN is reached */
        while (accept_word("when")) {
            if (accept_word("other")) { other = 1; break; }
            Cond *all = NULL;
            for (int i = 0; i < ns; i++) {
                if (i && !at_word("also"))
                    die_at(cur()->line, "EVALUATE has %d subjects; each WHEN has as many objects, joined by ALSO (%s)", ns, r_cnt);
                if (i) expect_word("also");
                Cond *c = NULL;
                if (accept_word("any")) c = NULL;
                else if (subj[i].kind == 3) {
                    /* a condition subject against TRUE or FALSE */
                    if (accept_word("true")) c = subj[i].c;
                    else if (accept_word("false")) { Cond *nn = cond_new(C_NOT); nn->a = subj[i].c; c = nn; }
                    else die_at(cur()->line, "WHEN for a condition subject takes TRUE, FALSE or ANY");
                }
                else if (subj[i].kind) {
                    if (at_word("true") || at_word("false")) {
                        int t = at_word("true"); advance();
                        if ((subj[i].kind == 1) != t) c = cond_never();
                    } else {
                        c = parse_cond();
                        if (subj[i].kind == 2) { Cond *nn = cond_new(C_NOT); nn->a = c; c = nn; }
                    }
                } else {
                    if (at_word("true") || at_word("false"))
                        die_at(cur()->line, "WHEN %s goes with a subject that is TRUE, FALSE or a condition (%s)", at_word("true") ? "TRUE" : "FALSE", r_cond);
                    /* a partial expression (2014; 2023 14.9.13.3 rules 5, 8): the
                     * object begins with a relational operator, or a class or sign
                     * condition without its identifier, [IS] [NOT] ahead of it;
                     * the subject goes to its left and the condition is evaluated
                     * (abbreviated combinations and all).  Not ZERO alone: WHEN
                     * ZERO is the figurative constant, as in 1985 */
                    if (at_relational() || (at_word("is") && (at_relational_tok(peek(1)) || is_word(peek(1), "zero") || (is_word(peek(1), "not") && (at_relational_tok(peek(2)) || is_word(peek(2), "zero"))))) ||
                        (at_word("not") && at_relational_tok(peek(1)))) {   /* IS ZERO, IS NOT ZERO: the sign condition, the IS saying so */
                        if (g_std < 2014) die_at(cur()->line, g_std < 2002 ? "a WHEN object that begins with a relation is COBOL 2014's partial expression; compile with -std=2014"
                                                                             : "a partial expression as a WHEN object is COBOL 2014 (2023 14.9.13); compile with -std=2014");
                        if (subj_lit[i]) die_at(cur()->line, "a partial expression goes with a subject that is an identifier or an expression, not a literal (2023 14.9.13.3 rule 6e)");
                        g_cond_left = &subj[i].o;
                        c = parse_cond();
                        if (g_cond_left) die_at(cur()->line, "internal: the partial expression did not take its subject");
                        if (c) all = all ? cond_bin(C_AND, all, c) : c;
                        continue;
                    }
                    int neg = accept_word("not");
                    Opnd x = parse_cond_operand();
                    if (at_relational())
                        die_at(cur()->line, "a condition as a WHEN object goes with a subject that is TRUE, FALSE or a condition (%s)", r_cond);
                    if (accept_word("thru") || accept_word("through")) {
                        Opnd y = parse_cond_operand();
                        int cx = eval_numeric(&x), cy = eval_numeric(&y);
                        if (cx >= 0 && cy >= 0 && cx != cy)
                            die_at(y.line, "WHEN ... THRU: the two ends are of the same class, both numeric or neither (%s)", r_thru);
                        if (ec_on_name("EC-RANGE-INVALID")) {
                            /* the starting value above the ending one: the condition,
                             * nonfatal, then an empty range (14.7.8), which the test
                             * below is of itself */
                            int Lok = new_label();
                            cond_jump_false(cond_rel(&x, R_GT, &y, 0), Lok);
                            emit_ec_raise(ec_find("EC-RANGE-INVALID", 0));
                            emit_label(Lok);
                        }
                        c = cond_bin(C_AND, cond_rel(&subj[i].o, R_GE, &x, 0), cond_rel(&subj[i].o, R_LE, &y, 0));
                    } else {
                        if (subj_lit[i] && eval_literal(&subj[i].o) && eval_literal(&x))
                            die_at(x.line, "a literal subject is not compared with a literal object (%s)", r_cmp);
                        c = cond_rel(&subj[i].o, R_EQ, &x, 0);
                    }
                    if (neg) { Cond *nn = cond_new(C_NOT); nn->a = c; c = nn; }
                }
                if (c) all = all ? cond_bin(C_AND, all, c) : c;
            }
            if (at_word("also")) die_at(cur()->line, "EVALUATE has %d subject%s; this WHEN has more objects (%s)", ns, ns == 1 ? "" : "s", r_cnt);
            if (!all) { Cond *nn = cond_new(C_NOT); nn->a = cond_never(); all = nn; }   /* every ANY: always */
            group = group ? cond_bin(C_OR, group, all) : all;
        }
        if (at_scope_end() || at_word("end-evaluate"))
            die_at(cur()->line, "each WHEN phrase of EVALUATE is followed by an imperative statement (%s format)", g_std < 2002 ? "X3.23-1985 EVALUATE" : "2023 14.9.13");
        if (nwh == whcap) { whcap = whcap ? 2 * whcap : 8; wh = xrealloc(wh, (size_t)whcap * sizeof *wh); }
        When *w = &wh[nwh++];
        calls_scope_end(when_outer);
        w->pre = block_cut(pre0); w->c = group; w->other = other;
        w->body = parse_block();
        if (other) {
            if (at_word("when")) die_at(cur()->line, "WHEN OTHER is the last phrase of EVALUATE (%s format)", g_std < 2002 ? "X3.23-1985 EVALUATE" : "2023 14.9.13");
            break;
        }
    }
    accept_word("end-evaluate");
    {   /* an island's too (lower.h): its placeholder, then the text */
        Block *pre = xmalloc((size_t)nwh * sizeof *pre), *body = xmalloc((size_t)nwh * sizeof *body);
        Cond **cc = xmalloc((size_t)nwh * sizeof *cc); int *oth = xmalloc((size_t)nwh * sizeof *oth);
        for (int i = 0; i < nwh; i++) { pre[i] = wh[i].pre; body[i] = wh[i].body; cc[i] = wh[i].c; oth[i] = wh[i].other; }
        lw_evaluate(lw_b0, pre, body, cc, oth, nwh);
        free(pre); free(body); free(cc); free(oth);
    }
    /* laid out: each test falls to the next WHEN; a body that is one jump
     * (GO TO) is its test's own branch; the last body, and one that ends
     * in a jump, need no jump to the end */
    int Lend = new_label();
    for (int i = 0; i < nwh; i++) {
        When *w = &wh[i];
        int last = i == nwh - 1;
        char tgt[96];
        block_put(&w->pre);
        if (w->other) { block_put(&w->body); continue; }
        if (block_is_jump(&w->body, tgt, sizeof tgt)) {
            int L = new_label(), b0 = g_nasm;
            cond_jump_true(w->c, L);
            if (!g_noemit) retarget(b0, L, tgt);
            continue;
        }
        int Lnext = last ? Lend : new_label();
        cond_jump_false(w->c, Lnext);
        block_put(&w->body);
        if (!last) {
            if (!block_ends_jump(&w->body)) emit_jump(Lend);
            emit_label(Lnext);
        }
    }
    emit_label(Lend);
    free(wh);
}

/* ---- INSPECT ----------------------------------------------------------- */

static int g_insp_nat;                  /* the inspected item is national (cobol ISSUES-68) */

/* a pattern operand: address and length as Args.  Beside a national item
 * every operand is national, and a figurative constant is one national
 * character (2023 14.9.22.3 rules 3 and 4) */
static void pattern_args(Opnd *o, Arg *addr, Arg *len);
/* EC-RANGE-INSPECT-SIZE at run time (14.9.22.4 rules 14, 22): the two
 * operands' lengths, one of them computed, compared; fatal when unequal */
static void insp_size_check(Opnd *a, Opnd *b)
{
    if (!ec_on_name("EC-RANGE-INSPECT-SIZE")) return;
    Arg da, db, la[2];
    pattern_args(a, &da, &la[0]); pattern_args(b, &db, &la[1]);
    emit_args(la, 2);
    int Lok = new_label();
    emit("\tbeq r3, r4, .L%d", Lok);
    emit_ec_raise(ec_find("EC-RANGE-INSPECT-SIZE", 0));
    emit_label(Lok);
}
static void pattern_args(Opnd *o, Arg *addr, Arg *len)
{
    if (o->kind == O_FIG && g_insp_nat) {
        unsigned u = nat_fig(o->tok->s); unsigned char two[2] = { (unsigned char)(u >> 8), (unsigned char)u };
        *addr = arg_label(lit_label(two, 2)); *len = arg_imm(2); return;
    }
    if (o->kind == O_FIG) { unsigned char c = (unsigned char)fig_byte(o->tok->s); *addr = arg_label(lit_label(&c, 1)); *len = arg_imm(1); return; }
    if (opnd_is_national(o) != g_insp_nat)
        die_at(o->line, g_insp_nat ? "INSPECT of a national item: every operand must be national (2023 14.9.22.3 rule 4)"
                                   : "INSPECT of an item that is not national: a national operand is not allowed (2023 14.9.22.3 rule 4)");
    Arg d; opnd_args(o, addr, &d, 0, 0);
    *len = arg_len(o);
}

static Opnd ref_opnd(const Ref *r)
{
    Opnd o; memset(&o, 0, sizeof o);
    o.kind = O_REF; o.ref = *r; o.line = r->line;
    return o;
}

/* [BEFORE|AFTER] [INITIAL] operand, either or both, after a TALLYING or
 * REPLACING phrase: the runtime is told the range for the next phrase */
/* an INSPECT operand other than the item and the tally: an identifier
 * is an elementary item of usage display or national (2023 14.9.22.3
 * rule 2; 85 INSPECT rule 3); a literal is no ALL figurative (rule 3) */
static void insp_operand(const Opnd *o)
{
    int e85 = g_std < 2002;
    if (o->kind == O_ALL) die_at(o->line, "INSPECT: an ALL figurative constant is not an INSPECT operand (%s)", e85 ? "X3.23-1985 INSPECT rule 3" : "2023 14.9.22.3 rule 3");
    if (o->kind == O_NUM) die_at(o->line, "INSPECT: a numeric literal is not an INSPECT operand; write it as \"...\" (%s)", e85 ? "X3.23-1985 INSPECT rule 3" : "2023 14.9.22.3 rule 3");
    no_zero_lit(o, "INSPECT", "2023 14.9.22.3 rule 3");
    if (o->kind != O_REF) return;
    const Sym *x = o->ref.sym;
    if (x->is_group && !o->ref.rm)
        die_at(o->line, "INSPECT: '%s' is a group; an operand is an elementary item (%s)", x->name, e85 ? "X3.23-1985 INSPECT rule 2" : "2023 14.9.22.3 rule 2");
    if (!x->is_group) cen_pin(x, "INSPECT");
    if (!x->is_group && x->usage != U_DISPLAY && x->usage != U_NATIONAL)
        die_at(o->line, "INSPECT: '%s' is USAGE %s; an operand is usage display%s (%s)", x->name, usage_name(x->usage), e85 ? "" : " or national",
               e85 ? "X3.23-1985 INSPECT rule 2" : "2023 14.9.22.3 rule 2");
}

/* INSPECT as a node (docs/plans/frontend-pass.md, step 4): the whole
 * statement is read before any code -- the runtime is told of the item,
 * then of each phrase, then makes its pass (cob_inspect_begin, _range,
 * _phrase, _run), and nothing else may run in between: a user function
 * among the operands, called where it was read, could itself INSPECT,
 * and the runtime would lose this statement's item and phrases (cobol
 * ISSUES-121).  So every operand is read as a scan, the calls are made
 * first (2023 14.6.4), and then the runtime's sequence is emitted whole. */
typedef struct { int hb, ha; Opnd before, after; } InspRange;
typedef struct { int kind; Opnd pat, rep; InspRange rg; Ref tally; int szchk; } InspPh;   /* kind: 0 CHARACTERS, 1 ALL, 2 LEADING, 3 FIRST; szchk: a length computed, compared at run time */

static void parse_inspect_range(InspRange *g)
{
    g->hb = g->ha = 0;
    for (;;) {
        if (accept_word("before")) { if (g->hb) die_at(cur()->line, "two BEFORE phrases"); accept_word("initial"); parse_operand(&g->before); insp_operand(&g->before); g->hb = 1; }
        else if (accept_word("after")) { if (g->ha) die_at(cur()->line, "two AFTER phrases"); accept_word("initial"); parse_operand(&g->after); insp_operand(&g->after); g->ha = 1; }
        else break;
    }
}
static void insp_range_calls(InspRange *g)
{
    if (g->hb) ucall_make(&g->before);
    if (g->ha) ucall_make(&g->after);
}
static void emit_inspect_range(InspRange *g)
{
    if (!g->hb && !g->ha) return;
    Arg a[4];
    if (g->hb) pattern_args(&g->before, &a[0], &a[1]); else { a[0] = arg_imm(0); a[1] = arg_imm(0); }
    if (g->ha) pattern_args(&g->after, &a[2], &a[3]); else { a[2] = arg_imm(0); a[3] = arg_imm(0); }
    emit_args(a, 4);
    emit_call("cob_inspect_range");
}

/* after a run: each TALLYING phrase's count added to its item */
static void emit_inspect_tallies(Ref *tallies, int *tally_ph, int nt)
{
    for (int t = 0; t < nt; t++) {
        Ref *tally = &tallies[t];
        emit_li("r3", tally_ph[t]);
        emit_call("cob_inspect_count");
        emit("\tstw sp+%d, r1", SLOT_C);
        if (is_hot_int(tally->sym)) {
            emit_ref_addr(tally, "r3");
            emit_load_int(tally->sym, "r3", "r1");
            emit("\tldw r2, sp+%d", SLOT_C);
            emit("\tadd r1, r1, r2");
            emit_trunc(tally->sym);
            emit_store_int(tally->sym, "r3", "r1");
        } else {
            emit("\tldw r3, sp+%d", SLOT_C);
            emit("\tsrai r4, r3, 31");
            emit_li("r5", 0);
            emit_call("cob_push_lit");
            emit_top_op(tally, "cob_top_addto", 0);
            emit_call("cob_drop");
        }
    }
}

static void parse_inspect_1(void);
static void parse_inspect(void)
{
    parse_inspect_1(); g_insp_nat = 0;
}
static void parse_inspect_1(void)
{
    Ref item; memset(&item, 0, sizeof item);
    Opnd itemo; memset(&itemo, 0, sizeof itemo);
    Opnd fo; memset(&fo, 0, sizeof fo);
    int fsubj = 0, fline = cur()->line;
    const char *fwhy = "a function-identifier is not a receiving operand (2023 8.4.3.2.3 rule 1); only TALLYING inspects one";
    static InspPh tl[32], rp[32];           /* the TALLYING and the REPLACING phrases */
    int ntl = 0, nrp = 0, converting = 0, conv_szchk = 0;
    Opnd from, to; InspRange crg; memset(&crg, 0, sizeof crg);
    memset(&from, 0, sizeof from); memset(&to, 0, sizeof to);

    int backward = 0;
    if (at_word("backward") && !sym_lookup_quiet("backward") && peek(1)->kind == T_WORD && !is_verb(peek(1)->s)) {
        /* INSPECT BACKWARD (2023 14.9.22.4 rule 3): the scan from the right,
         * BEFORE and AFTER found in that direction, the matching itself
         * leftmost-first at each position (note 2) */
        if (g_std < 2023) die_at(cur()->line, "INSPECT BACKWARD is COBOL 2023 (14.9.22); compile with -std=2023");
        backward = 1; advance();
    }
    /* ---- the statement, read: no code ---- */
    g_noemit++;
    if (at_word("function") || (cur()->kind == T_WORD && ufn_named(cur()->s))) {
        /* a function-identifier: a sending operand, so TALLYING only --
         * REPLACING and CONVERTING would change it */
        parse_operand(&fo);
        int numeric = fo.kind == O_NUM ||           /* LENGTH and the like, folded at compile time */
                      (fo.kind == O_FUNC && (fo.fn == -1 ? fo.fscale >= 0 : fn_is_numeric(fo.fn))) || fo.fwnum || fo.fbool ||
                      (fo.kind == O_REF && is_numeric_sym(fo.ref.sym));
        if (numeric) die_at(fline, "INSPECT of a numeric or boolean function's value: the subject is alphanumeric or national (2023 14.9.22.3 rule 1)");
        fsubj = 1;
        g_insp_nat = opnd_is_national(&fo);
    } else {
        parse_ref(&item);
        if (item.sym->is_cond) die_at(item.line, "INSPECT of a condition-name");
        /* a numeric USAGE NATIONAL item's characters are national too */
        { Opnd io; memset(&io, 0, sizeof io); io.kind = O_REF; io.ref = item; io.line = item.line; no_bits(&io, "INSPECT"); }
        if (item.sym->strong) die_at(item.line, "INSPECT of a strongly-typed group (2023 14.9.22.3 rule 1)");
        cen_pin(item.sym, "INSPECT");
        if (!item.sym->is_group && !item.rm && item.sym->usage != U_DISPLAY && item.sym->usage != U_NATIONAL)
            die_at(item.line, "INSPECT of '%s', USAGE %s: the item is usage display%s, or a group (%s)", item.sym->name, usage_name(item.sym->usage),
                   g_std < 2002 ? "" : " or national", g_std < 2002 ? "X3.23-1985 INSPECT rule 1" : "2023 14.9.22.3 rule 1");
        g_insp_nat = sym_is_national(item.sym) || (!item.sym->is_group && item.sym->usage == U_NATIONAL);
        itemo = ref_opnd(&item);
        operand_odo_length(&itemo);             /* a group over an ODO table is inspected at its current length */

    }
    int w = g_insp_nat ? 2 : 1;             /* a character's bytes */
    if (fsubj && at_word("converting")) die_at(fline, "INSPECT CONVERTING of a function: %s", fwhy);
    if (accept_word("converting")) {
        converting = 1;
        if (!fsubj) no_constrec_recv(&item, "INSPECT CONVERTING");
        parse_operand(&from); insp_operand(&from); expect_word("to"); parse_operand(&to);
        if (to.kind == O_REF) insp_operand(&to);
        int fl = from.kind == O_FIG ? w : opnd_size(&from), tl2 = to.kind == O_FIG ? w : opnd_size(&to);
        if (fl > 0 && tl2 > 0 && fl != tl2 && to.kind != O_FIG) die_at(to.line, "INSPECT CONVERTING: the two operands must be the same length");
        conv_szchk = (fl < 0 || tl2 < 0) && to.kind != O_FIG;
        parse_inspect_range(&crg);
    } else {
        if (accept_word("tallying")) {
            for (;;) {
                Ref tally; parse_ref(&tally); no_constrec_recv(&tally, "INSPECT TALLYING");
                if (tally.sym->is_group || tally.sym->pi.category != PIC_NUMERIC)
                    die_at(tally.line, "the INSPECT tally '%s' is an elementary numeric item (2023 14.9.22.3 rule 5)", tally.sym->name);
                expect_word("for");
                for (;;) {
                    int kind = 0;
                    if (accept_word("characters")) kind = 0;
                    else if (accept_word("all")) kind = 1;
                    else if (accept_word("leading")) kind = 2;
                    else die_at(cur()->line, "expected CHARACTERS, ALL or LEADING in INSPECT TALLYING");
                    /* CHARACTERS [range]; ALL|LEADING {operand [range]}... */
                    for (;;) {
                        if (ntl == 32) die_at(cur()->line, "INSPECT: more than 32 phrases");
                        InspPh *ph = &tl[ntl]; memset(ph, 0, sizeof *ph);
                        ph->kind = kind; ph->tally = tally;
                        if (kind) { parse_operand(&ph->pat); insp_operand(&ph->pat); }
                        parse_inspect_range(&ph->rg);
                        ntl++;
                        /* another operand under the same ALL/LEADING: not a keyword, not the next tally (an identifier followed by FOR) */
                        if (!kind || !at_operand() || at_word("characters") || at_word("all") || at_word("leading") || at_word("replacing")) break;
                        if (cur()->kind == T_WORD && is_word(peek(1), "for")) break;
                    }
                    if (!(at_word("characters") || at_word("all") || at_word("leading"))) break;
                }
                if (!at_operand() || at_word("replacing")) break;
            }
        }
        if (fsubj && at_word("replacing")) die_at(fline, "INSPECT REPLACING of a function: %s", fwhy);
        if (accept_word("replacing")) {
            if (!fsubj) no_constrec_recv(&item, "INSPECT REPLACING");
            for (;;) {
                int kind = 0;
                if (accept_word("characters")) kind = 0;
                else if (accept_word("all")) kind = 1;
                else if (accept_word("leading")) kind = 2;
                else if (accept_word("first")) kind = 3;
                else die_at(cur()->line, "expected CHARACTERS, ALL, LEADING or FIRST in INSPECT REPLACING");
                /* CHARACTERS BY rep [range]; ALL|LEADING|FIRST {pat BY rep [range]}... */
                for (;;) {
                    if (nrp == 32) die_at(cur()->line, "INSPECT: more than 32 phrases");
                    InspPh *ph = &rp[nrp]; memset(ph, 0, sizeof *ph);
                    ph->kind = kind;
                    if (kind) { parse_operand(&ph->pat); insp_operand(&ph->pat); }
                    expect_word("by"); parse_operand(&ph->rep); insp_operand(&ph->rep);
                    if (!kind) {
                        /* CHARACTERS BY: one character (rule 7) */
                        int rl = ph->rep.kind == O_FIG ? w : opnd_size(&ph->rep);
                        if (rl != w) die_at(ph->rep.line, "INSPECT REPLACING CHARACTERS BY: one character (%s)", g_std < 2002 ? "X3.23-1985 INSPECT rule 8" : "2023 14.9.22.3 rule 7");
                    }
                    if (kind) {
                        int pl = ph->pat.kind == O_FIG ? w : opnd_size(&ph->pat), rl = ph->rep.kind == O_FIG ? w : opnd_size(&ph->rep);
                        if (pl > 0 && rl > 0 && pl != rl) die_at(ph->rep.line, "INSPECT REPLACING: the two operands must be the same length");
                        ph->szchk = (pl < 0 || rl < 0) && ph->rep.kind != O_FIG && ph->pat.kind != O_FIG;
                    }
                    parse_inspect_range(&ph->rg);
                    nrp++;
                    if (!kind || !at_operand() || at_word("characters") || at_word("all") || at_word("leading") || at_word("first")) break;
                }
                if (!(at_word("characters") || at_word("all") || at_word("leading") || at_word("first"))) break;
            }
        }
        if (!ntl && !nrp) die_at(fline, "INSPECT needs TALLYING, REPLACING or CONVERTING");
    }
    g_noemit--;

    /* ---- its user functions, called: before the runtime hears of it ---- */
    if (fsubj) ucall_make(&fo); else ucall_make(&itemo);
    if (converting) { ucall_make(&from); ucall_make(&to); insp_range_calls(&crg); }
    for (int i = 0; i < ntl; i++) { ref_calls(&tl[i].tally); if (tl[i].kind) ucall_make(&tl[i].pat); insp_range_calls(&tl[i].rg); }
    for (int i = 0; i < nrp; i++) { if (rp[i].kind) ucall_make(&rp[i].pat); ucall_make(&rp[i].rep); insp_range_calls(&rp[i].rg); }

    /* ---- the code.  The phrases are registered with the runtime, which
     * makes the one pass the text describes (cob_inspect_run); then each
     * tally is added.  A statement with both TALLYING and REPLACING is two
     * statements, the tallying pass first (X3.23 general rule): two
     * begin/run rounds. ---- */
#define INSP_BEGIN() do { \
        if (fsubj) { emit_str_arg(&fo); if (g_insp_nat) emit_desc_addr("r5", nat_desc(2)); else emit_li("r5", 0); } \
        else { Arg ba[3] = { arg_ref(&itemo.ref), arg_len(&itemo), itemo.ref.rm ? (g_insp_nat ? arg_desc(nat_desc(2)) : arg_imm(0)) : arg_desc(sym_desc(item.sym)) }; emit_args(ba, 3); } \
        emit_call("cob_inspect_begin"); if (backward) emit_call("cob_inspect_backward"); } while (0)
    /* the plain forms, one call (performance.md 2026-10-08): an alphanumeric
     * item or part, no BEFORE/AFTER, not BACKWARD, single-byte characters --
     * CONVERTING literal TO literal, or one TALLYING phrase FOR CHARACTERS
     * or FOR ALL of one byte, a literal; the runtime drives the sweep kernel */
    int plain = !fsubj && !g_insp_nat && !backward && w == 1 && (itemo.ref.rm || !is_numeric_sym(item.sym));
    if (plain && converting && !crg.hb && !crg.ha && !conv_szchk && from.kind == O_STR && to.kind == O_STR && from.tok->len == to.tok->len && from.tok->len > 0) {
        Arg a[5], x;
        a[0] = arg_ref(&itemo.ref); a[1] = arg_len(&itemo);
        pattern_args(&from, &a[2], &a[3]); pattern_args(&to, &a[4], &x);
        emit_args(a, 5);
        emit_call("cob_inspect_convert_plain");
        return;
    }
    if (plain && !converting && !nrp && ntl == 1 && !tl[0].rg.hb && !tl[0].rg.ha &&
        (tl[0].kind == 0 || (tl[0].kind == 1 && tl[0].pat.kind == O_STR && tl[0].pat.tok->len == 1))) {
        Arg a[4] = { arg_ref(&itemo.ref), arg_len(&itemo), arg_imm(tl[0].kind), arg_imm(tl[0].kind ? (unsigned char)tl[0].pat.tok->s[0] : 0) };
        emit_args(a, 4);
        emit_call("cob_inspect_tally_plain");
        Ref tallies1[1]; int ph1[1] = { 0 }; tallies1[0] = tl[0].tally;
        emit_inspect_tallies(tallies1, ph1, 1);
        return;
    }
    INSP_BEGIN();
    if (converting) {
        int fl = from.kind == O_FIG ? w : opnd_size(&from);
        emit_inspect_range(&crg);
        Arg a[3], x;
        if (to.kind == O_FIG && fl > w) {
            /* CONVERTING "abc" TO SPACE: the figurative is as long as the other */
            unsigned char *f = xmalloc((size_t)fl);
            if (g_insp_nat) { unsigned u = nat_fig(to.tok->s); for (int i = 0; i + 1 < fl; i += 2) { f[i] = (unsigned char)(u >> 8); f[i + 1] = (unsigned char)u; } }
            else memset(f, fig_byte(to.tok->s), (size_t)fl);
            a[2] = arg_label(lit_label(f, fl)); free(f);
        } else pattern_args(&to, &a[2], &x);
        pattern_args(&from, &a[0], &a[1]);
        emit_args(a, 3);
        if (conv_szchk) insp_size_check(&from, &to);
        emit_call("cob_inspect_convert");
        emit_call("cob_inspect_run");
        return;
    }
    Ref tallies[32]; int tally_ph[32];
    for (int i = 0; i < ntl; i++) {
        InspPh *ph = &tl[i];
        emit_inspect_range(&ph->rg);
        Arg a[5];
        a[0] = arg_imm(1); a[1] = arg_imm(ph->kind);
        if (ph->kind) pattern_args(&ph->pat, &a[2], &a[3]); else { a[2] = arg_imm(0); a[3] = arg_imm(0); }
        a[4] = arg_imm(0);
        emit_args(a, 5);
        emit_call("cob_inspect_phrase");
        tallies[i] = ph->tally; tally_ph[i] = i;
    }
    if (nrp && ntl) {
        /* the tallying pass first, its counts added; then the replacing pass */
        emit_call("cob_inspect_run");
        emit_inspect_tallies(tallies, tally_ph, ntl);
        ntl = 0;
        INSP_BEGIN();
    }
    for (int i = 0; i < nrp; i++) {
        InspPh *ph = &rp[i];
        if (ph->szchk) insp_size_check(&ph->pat, &ph->rep);
        emit_inspect_range(&ph->rg);
        Arg a[5];
        a[0] = arg_imm(0); a[1] = arg_imm(ph->kind);
        if (ph->kind) pattern_args(&ph->pat, &a[2], &a[3]); else { a[2] = arg_imm(0); a[3] = arg_imm(1); }
        Arg rl; pattern_args(&ph->rep, &a[4], &rl);
        emit_args(a, 5);
        emit_call("cob_inspect_phrase");
    }
    emit_call("cob_inspect_run");
    emit_inspect_tallies(tallies, tally_ph, ntl);
#undef INSP_BEGIN
}

/* ---- INITIALIZE -------------------------------------------------------- */

/* INITIALIZE ... REPLACING category DATA BY value: every elementary item
 * of that category below the receiver (index items, condition-names,
 * REDEFINES items and elementary FILLERs left alone, X3.23 6.16) takes
 * the value by the MOVE rules -- every occurrence of a table, the
 * receiver's own subscripts leading, the rest unrolled at compile time */
static void init_replace_walk(Sym *s, const Ref *base, int cat, Opnd *value, long *sub, int nsub, int line, int bits_only)
{
    if (s->is_cond || s->is_index || s->redefines >= 0 || s->dyn || s->dynl) return;
    if (s->is_group) {
        for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
            Sym *k = &g_sym[c];
            if (k->dyn) continue;                   /* filled apart (init_dyn_fill) */
            if (k->occurs) {
                /* one more dimension: every occurrence (a bit array's
                 * elements too, their bits found by ref_resolve_bits) */
                if (nsub >= MAXDIM) die_at(line, "INITIALIZE REPLACING: too many dimensions");
                for (long i = 1; i <= k->occurs; i++) { sub[nsub] = i; init_replace_walk(k, base, cat, value, sub, nsub + 1, line, bits_only); }
            } else init_replace_walk(k, base, cat, value, sub, nsub, line, bits_only);
        }
        return;
    }
    if (s->is_filler || s->pi.category != cat) return;
    if (bits_only && s->usage != U_BIT) return;
    Ref r = *base; r.sym = s; r.nsub = nsub; r.rm = 0; r.user_rm = 0; r.rm_bit = 0; r.bitsub = 0;
    for (int i = 0; i < nsub; i++) { if (i < base->nsub) r.sub[i] = base->sub[i]; else { r.sub[i].sym = NULL; r.sub[i].lit = sub[i]; r.sub[i].adj = 0; } }
    if (nsub != s->ndims) die_at(line, "INITIALIZE REPLACING: '%s' needs %d subscripts", s->name, s->ndims);
    ref_resolve_bits(&r);                       /* a bit array's element: its own bits (cobol ISSUES-94 B2) */
    emit_move(value, &r);
}

/* the bytes INITIALIZE sets from the template: those of the elementary
 * items it initializes -- not FILLERs, index items, or REDEFINES items
 * and their subordinates (X3.23 6.16; the item a REDEFINES redefines is
 * initialized, cobol ISSUES-94), every occurrence.  Bit items share bytes
 * with their neighbours and are set by MOVE instead (ISSUES-94 B1). */
static void init_cover(Sym *s, int top_off, int disp, unsigned char *cover, int limit, int is_top)
{
    if (s->is_cond || s->is_index || s->dyn || s->dynl) return;   /* a dynamic table's slot stays, its elements are filled apart; a dynamic-length item's length is set to zero apart */
    if (!is_top && s->redefines >= 0) return;
    if (s->bitgroup || (!s->is_group && s->usage == U_BIT)) return;
    int reps = (!is_top && s->occurs) ? s->occurs : 1;
    for (int i = 0; i < reps; i++) {
        int d = disp + i * s->size;
        if (s->is_group) { for (int c = s->child; c >= 0; c = g_sym[c].sibling) init_cover(&g_sym[c], top_off, d, cover, limit, 0); }
        else if (!s->is_filler) { int a = s->offset - top_off + d; for (int k = a; k < a + s->size && k < limit; k++) if (k >= 0) cover[k] = 1; }
    }
}

/* COBOL 2002's INITIALIZE (2023 14.9.20): WITH FILLER, {ALL | category}
 * TO VALUE, REPLACING, TO DEFAULT.  Every elementary item below the
 * receiver, in the order of definition, every occurrence, is a possible
 * receiving operand (GR 5: not condition-names or index items, not
 * REDEFINES items below the receiver, a FILLER only WITH FILLER); it
 * takes its VALUE clause's value when the VALUE phrase names its category
 * and it has one (a pointer: NULL), else the REPLACING value for its
 * category, else its category's default (GR 6c) when TO DEFAULT is given
 * or neither VALUE nor REPLACING is -- or is left alone. */
enum { IC_DPTR = 100, IC_NATED, IC_PPTR, IC_FPTR };  /* categories beyond PIC_*: data-pointer, national-edited, program-pointer, function-pointer */
typedef struct {
    int filler, value, value_all, value_cat, deflt, nrep;
    int rep_cat[16]; Opnd rep_val[16];
} InitSpec;
static int init_cat(const Sym *s)
{
    if (s->usage == U_POINTER) return s->uvar == UV_PPTR ? IC_PPTR : s->uvar == UV_FPTR ? IC_FPTR : IC_DPTR;
    if (s->pi.category == PIC_NATIONAL && s->pi.edited) return IC_NATED;
    return s->pi.category;
}
static Opnd init_value_opnd(Sym *s)
{
    Opnd o = lit_opnd(s->value_tok);
    if (s->value_all && s->value_tok->kind == T_STR) o.kind = O_ALL;
    return o;
}
static void init_elem2k(Sym *s, Ref *r, const InitSpec *sp)
{
    static Tok tz = { T_WORD, 0, "zero", 4, NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    static Tok ts = { T_WORD, 0, "spaces", 6, NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    int cat = init_cat(s);
    Opnd v; memset(&v, 0, sizeof v); v.line = r->line;
    int ptr_null = 0, have = 0;
    if (sp->value && (sp->value_all || sp->value_cat == cat)) {
        if (cat == IC_DPTR || cat == IC_PPTR || cat == IC_FPTR) { ptr_null = 1; have = 1; }
        else if (s->value_tok) { v = init_value_opnd(s); have = 1; }
    }
    if (!have) for (int k = 0; k < sp->nrep; k++) if (sp->rep_cat[k] == cat) { v = sp->rep_val[k]; have = 1; break; }
    if (!have && (sp->deflt || (!sp->value && !sp->nrep))) {
        if (cat == IC_DPTR || cat == IC_PPTR || cat == IC_FPTR) ptr_null = 1;
        else { v.kind = O_FIG; v.tok = cat == PIC_NUMERIC || cat == PIC_NUMERIC_EDITED || cat == PIC_BOOLEAN ? &tz : &ts; }
        have = 1;
    }
    if (!have) return;
    if (cat == IC_DPTR || cat == IC_PPTR || cat == IC_FPTR) {     /* SET receiving-operand TO NULL, or TO the REPLACING pointer */
        if (cat == IC_FPTR && !ptr_null && opnd_ptr_proto(&v)[0] && !fnsig_same(s->ptr_proto, opnd_ptr_proto(&v)))
            die_at(r->line, "INITIALIZE '%s' REPLACING FUNCTION-POINTER: a function-pointer TO %s takes a value whose prototype has the same signature; %s's differs (the implicit SET, 2023 14.9.39.3 rule 20)",
                   s->name, s->ptr_proto, opnd_ptr_proto(&v));
        if (ptr_null) emit_li("r1", 0);
        else emit_ptr_value(&v, "r1");
        emit("\tstw sp+%d, r1", SLOT_A);
        emit_ref_addr(r, "r3");
        emit("\tldw r1, sp+%d", SLOT_A);
        emit("\tstw r3+0, r1");
        return;
    }
    emit_move(&v, r);
}
/* INITIALIZE of a group holding dynamic-capacity tables (14.9.20.4 rule
 * 10): each table's elements up to its capacity to their initial state --
 * the VALUE clauses' when the statement says ALL TO VALUE, the categories'
 * defaults otherwise -- the capacity unchanged.  The statement's other
 * phrases (REPLACING, a category's VALUE) do not reach the elements in
 * this stage: refused. */
/* a dynamic-length item's length set to zero (14.9.20.4 rule 7): the item named, or every one under the group, each occurrence */
static void init_dynl_zero(Sym *k, const Ref *base, long *sub, int nsub, int line)
{
    Ref r; memset(&r, 0, sizeof r); r.sym = k; r.line = line; r.nsub = nsub;
    for (int i = 0; i < nsub; i++) { if (base && i < base->nsub) r.sub[i] = base->sub[i]; else { r.sub[i].sym = NULL; r.sub[i].lit = sub[i]; r.sub[i].adj = 0; } }
    if (nsub != k->ndims) die_at(line, "INITIALIZE: '%s' needs %d subscripts", k->name, k->ndims);
    char dl[32]; snprintf(dl, sizeof dl, ".Ldynl%d_%d", g_unit, k->dynl_id);
    Arg a[2] = { arg_ref(&r), arg_label(dl) }; emit_args(a, 2); emit_li("r5", 0); emit_call("cob_dynl_size");
}
static void init_dynl_walk(Sym *s, const Ref *base, long *sub, int nsub, int line)
{
    for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
        Sym *k = &g_sym[c];
        if (k->is_cond || k->is_index || k->redefines >= 0) continue;
        if (k->dynl) {
            if (k->occurs) { if (nsub >= MAXDIM) die_at(line, "INITIALIZE: too many dimensions"); for (long i = 1; i <= k->occurs; i++) { sub[nsub] = i; init_dynl_zero(k, base, sub, nsub + 1, line); } }
            else init_dynl_zero(k, base, sub, nsub, line);
        } else if (k->is_group) {
            if (k->occurs) { if (nsub >= MAXDIM) die_at(line, "INITIALIZE: too many dimensions"); for (long i = 1; i <= k->occurs; i++) { sub[nsub] = i; init_dynl_walk(k, base, sub, nsub + 1, line); } }
            else init_dynl_walk(k, base, sub, nsub, line);
        }
    }
}
static void init_dyn_fill(Sym *g, int values, int line)
{
    for (int c = g->child; c >= 0; c = g_sym[c].sibling) {
        Sym *k = &g_sym[c];
        if (k->is_cond || k->is_index) continue;
        if (k->dyn) {
            emit_item_addr("r3", k, k->offset);
            char dl[32]; snprintf(dl, sizeof dl, ".Ldyn%d_%d", g_unit, k->dyn_id);
            emit_la("r4", dl); emit_li("r5", values);
            emit_call("cob_dyn_fill");
            (void)line;
        } else if (k->is_group && !k->occurs) init_dyn_fill(k, values, line);
        else if (k->is_group && dyn_table_below(k)) die_at(line, "INITIALIZE: the dynamic-capacity table '%s' is inside the table '%s'", dyn_table_below(k)->name, k->name);
    }
}
static void init_walk(Sym *s, const Ref *base, const InitSpec *sp, long *sub, int nsub, int line, int is_top)
{
    if (s->is_cond || s->is_index || s->usage == U_INDEX || s->dyn) return;
    if (!is_top && s->redefines >= 0) return;
    if (s->dynl) { if (is_top) { Ref r = *base; init_dynl_zero(s, &r, sub, nsub, line); } return; }   /* its length to zero (14.9.20.4 rule 7); under a group, init_dynl_walk's */
    if (s->is_group) {
        for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
            Sym *k = &g_sym[c];
            if (k->dyn) continue;                   /* filled apart (init_dyn_fill) */
            if (k->occurs) {
                if (nsub >= MAXDIM) die_at(line, "INITIALIZE: too many dimensions");
                for (long i = 1; i <= k->occurs; i++) { sub[nsub] = i; init_walk(k, base, sp, sub, nsub + 1, line, 0); }
            } else init_walk(k, base, sp, sub, nsub, line, 0);
        }
        return;
    }
    if (s->is_filler && !sp->filler && !is_top) return;
    Ref r = *base; r.sym = s; r.nsub = nsub; r.rm = 0; r.user_rm = 0; r.rm_bit = 0; r.bitsub = 0;
    for (int i = 0; i < nsub; i++) { if (i < base->nsub) r.sub[i] = base->sub[i]; else { r.sub[i].sym = NULL; r.sub[i].lit = sub[i]; r.sub[i].adj = 0; } }
    if (nsub != s->ndims) die_at(line, "INITIALIZE: '%s' needs %d subscripts", s->name, s->ndims);
    ref_resolve_bits(&r);
    init_elem2k(s, &r, sp);
}
/* a category-name of the 2002 INITIALIZE, or -1 */
static int init_cat_word(void)
{
    static const struct { const char *w; int c; } cw[] = {
        { "alphabetic", PIC_ALPHABETIC }, { "alphanumeric", PIC_ALPHANUMERIC }, { "alphanumeric-edited", PIC_ALPHANUMERIC_EDITED },
        { "numeric", PIC_NUMERIC }, { "numeric-edited", PIC_NUMERIC_EDITED }, { "national", PIC_NATIONAL },
        { "national-edited", IC_NATED }, { "boolean", PIC_BOOLEAN }, { "data-pointer", IC_DPTR }, { "program-pointer", IC_PPTR }, { "function-pointer", IC_FPTR },
    };
    for (unsigned i = 0; i < sizeof cw / sizeof cw[0]; i++) if (at_word(cw[i].w)) return cw[i].c;
    if (at_word("function-pointer") || at_word("message-tag") || at_word("object-reference"))
        die_at(cur()->line, "INITIALIZE: the category %s is not implemented (no such items exist here)", cur()->s);
    return -1;
}
static void parse_initialize_2002(Ref *rs, int n)
{
    InitSpec sp; memset(&sp, 0, sizeof sp);
    if (accept_word("with")) { expect_word("filler"); sp.filler = 1; }
    else if (accept_word("filler")) sp.filler = 1;
    if (at_word("all") || (init_cat_word() >= 0 && is_word(peek(1), "to"))) {
        if (accept_word("all")) sp.value_all = 1; else { sp.value_cat = init_cat_word(); advance(); }
        expect_word("to"); expect_word("value"); sp.value = 1;
    }
    if (at_word("then") && is_word(peek(1), "replacing")) advance();
    if (accept_word("replacing")) {
        for (;;) {
            int line = cur()->line, cat = init_cat_word();
            if (cat < 0) die_at(line, "INITIALIZE REPLACING: expected a category-name");
            advance();
            for (int k = 0; k < sp.nrep; k++)
                if (sp.rep_cat[k] == cat) die_at(line, "INITIALIZE REPLACING: a category named twice (2023 14.9.20.3 rule 6)");
            accept_word("data"); expect_word("by");
            Opnd value; parse_operand(&value);
            if (cat == IC_DPTR || cat == IC_PPTR || cat == IC_FPTR) {
                int vc = opnd_ptr_cat(&value), want = cat == IC_DPTR ? 1 : cat == IC_PPTR ? 2 : 3;
                if (!vc || (vc > 0 && vc != want))
                    die_at(line, "INITIALIZE REPLACING %s needs a %s item, ADDRESS OF%s or NULL (2023 14.9.20.3 rules 3-4)",
                           cat == IC_DPTR ? "DATA-POINTER" : cat == IC_PPTR ? "PROGRAM-POINTER" : "FUNCTION-POINTER", ptr_cat_name(want),
                           cat == IC_DPTR ? "" : cat == IC_PPTR ? " PROGRAM" : " FUNCTION");
            } else if (value.kind != O_REF && value.kind != O_STR && value.kind != O_NUM && value.kind != O_FIG)
                die_at(line, "INITIALIZE REPLACING ... BY needs an item or a literal");
            emit_incompat(&value);
            if (sp.nrep == 16) die_at(line, "INITIALIZE REPLACING: too many categories");
            sp.rep_cat[sp.nrep] = cat; sp.rep_val[sp.nrep++] = value;
            if (init_cat_word() < 0) break;
        }
    }
    if (at_word("then") && is_word(peek(1), "to")) advance();
    if (at_word("to") && is_word(peek(1), "default")) { advance(); advance(); sp.deflt = 1; }
    for (int i = 0; i < n; i++) {
        if (rs[i].user_rm) {
            /* a reference-modified item: an elementary item of its part's
             * category (alphanumeric, national, boolean) with no VALUE
             * clause -- a REPLACING of that category, else the category's
             * default (TO DEFAULT, or no VALUE and no REPLACING phrase),
             * else unchanged (14.9.20.4 rules 2-5; 8.4.3.3.4 rule 6) */
            Sym *t = rs[i].sym;
            int rcat = rs[i].rm_bit || t->pi.category == PIC_BOOLEAN ? PIC_BOOLEAN : rs[i].rm_nat ? PIC_NATIONAL : PIC_ALPHANUMERIC;
            static Tok tz = { T_WORD, 0, "zero", 4, NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
            static Tok ts = { T_WORD, 0, "spaces", 6, NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
            Opnd v; memset(&v, 0, sizeof v); v.line = rs[i].line;
            int have = 0;
            for (int k = 0; k < sp.nrep; k++) if (sp.rep_cat[k] == rcat) { v = sp.rep_val[k]; have = 1; break; }
            if (!have && (sp.deflt || (!sp.value && !sp.nrep))) { v.kind = O_FIG; v.tok = rcat == PIC_BOOLEAN ? &tz : &ts; have = 1; }
            if (have) emit_move(&v, &rs[i]);
            continue;
        }
        long sub[MAXDIM];
        init_walk(rs[i].sym, &rs[i], &sp, sub, rs[i].nsub, rs[i].line, 1);
        Sym *t = rs[i].sym;
        if (t->is_group && vlen_below(t)) { long sub2[MAXDIM]; for (int q = 0; q < rs[i].nsub; q++) sub2[q] = 0; init_dynl_walk(t, &rs[i], sub2, rs[i].nsub, rs[i].line); }
        if (t->is_group && dyn_table_below(t)) {
            if (sp.nrep || (sp.value && !sp.value_all))
                die_at(rs[i].line, "INITIALIZE '%s': the group holds the dynamic-capacity table '%s', whose elements this stage initializes only to their initial state (no REPLACING, no category VALUE)", t->name, dyn_table_below(t)->name);
            init_dyn_fill(t, sp.value ? 1 : 0, rs[i].line);
        }
    }
}

static void parse_initialize(void)
{
    Ref rs[MAXOPS]; int n = 0;
    static Tok tok_zero = { T_WORD, 0, "zero", 4, NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    static Tok tok_space = { T_WORD, 0, "spaces", 6, NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    Opnd fig_zero, fig_space; memset(&fig_zero, 0, sizeof fig_zero); memset(&fig_space, 0, sizeof fig_space);
    fig_zero.kind = O_FIG; fig_zero.tok = &tok_zero; fig_space.kind = O_FIG; fig_space.tok = &tok_space;
    while (at_operand() && !at_word("all") && !at_word("with") && !at_word("filler") && !at_word("then") && !is_word(peek(1), "to")) {
        if (n >= MAXOPS) die_at(cur()->line, "too many items in INITIALIZE");
        Ref *r = &rs[n]; parse_ref(r); no_constrec_recv(r, "INITIALIZE");
        if (r->sym->is_cond) die_at(r->line, "INITIALIZE of a condition-name");
        if (r->sym->is_rename)
            die_at(r->line, "INITIALIZE: '%s' is a RENAMES item (%s)", r->sym->name, g_std < 2002 ? "X3.23-1985 INITIALIZE syntax rule 6" : "2023 14.9.20.3 rule 5");
        if (r->sym->is_index)
            die_at(r->line, "INITIALIZE: '%s' is an index-name, not a data item; SET it", r->sym->name);
        if (g_std < 2002 && odo_table_for(r->sym)) bp(BP_E15_INIT_ODO, r->line);
        n++;
    }
    if (!n) die_at(cur()->line, "INITIALIZE needs an item");
    /* the COBOL 2002 phrases: WITH FILLER, ... TO VALUE, TO DEFAULT */
    {
        int j = g_tp, two = 0;
        if (is_word(&g_tok[j], "with") || is_word(&g_tok[j], "filler") || is_word(&g_tok[j], "all")) two = 1;
        else if (is_word(&g_tok[j], "then")) two = 1;
        else if (g_tok[j].kind == T_WORD && is_word(&g_tok[j + 1], "to") && (is_word(&g_tok[j + 2], "value") || is_word(&g_tok[j + 2], "default"))) two = 1;
        else if (is_word(&g_tok[j], "to") && is_word(&g_tok[j + 1], "default")) two = 1;
        else if (is_word(&g_tok[j], "replacing")) {
            /* REPLACING ... THEN TO DEFAULT, or a 2002 category */
            for (int k = j + 1; g_tok[k].kind == T_WORD || g_tok[k].kind == T_STR || g_tok[k].kind == T_NUM; k++) {
                if (is_word(&g_tok[k], "default") || is_word(&g_tok[k], "national-edited") || is_word(&g_tok[k], "data-pointer") ||
                    is_word(&g_tok[k], "program-pointer") || is_word(&g_tok[k], "function-pointer")) { two = 1; break; }
                if (g_tok[k].kind == T_WORD && !strcmp(g_tok[k].s, "then")) { two = 1; break; }
                if (g_tok[k].kind == T_WORD && is_verb(g_tok[k].s)) break;
            }
        }
        if (two) {
            if (g_std < 2002) die_at(cur()->line, "INITIALIZE WITH FILLER / TO VALUE / TO DEFAULT is COBOL 2002; compile with -std=2002");
            parse_initialize_2002(rs, n);
            return;
        }
    }
    if (!at_word("replacing")) {
        /* no REPLACING: every elementary item to its category's default --
         * the template image copied in runs around the bytes left alone,
         * then the edited items by MOVE (ZERO or SPACES through the edit) */
        for (int i = 0; i < n; i++) {
            Ref *r = &rs[i]; Sym *t = r->sym;
            if (r->user_rm) {
                /* a reference-modified item is an elementary item of its
                 * part's category: alphanumeric (national, boolean), set to
                 * spaces (national spaces, zeros) (X3.23 6.16; 2023 8.4.2.4;
                 * cobol ISSUES-94) */
                emit_move(r->rm_bit || t->pi.category == PIC_BOOLEAN ? &fig_zero : &fig_space, r);
                continue;
            }
            if (t->dynl) { long sub0[MAXDIM]; init_dynl_zero(t, r, sub0, r->nsub, r->line); continue; }   /* its length to zero (14.9.20.4 rule 7) */
            if (r->rm) { emit_move(&fig_zero, r); continue; }      /* a bit array's element */
            Sym tmp; memset(&tmp, 0, sizeof tmp);
            tmp.image = xmalloc(t->size); tmp.image_size = t->size;
            g_no_values = 1;
            init_one(&tmp, sym_idx(t), 0, 1);
            g_no_values = 0;
            unsigned char *cover = xmalloc((size_t)t->size + 1); memset(cover, 0, (size_t)t->size + 1);
            init_cover(t, t->offset, 0, cover, t->size, 1);
            for (int a = 0; a < t->size; ) {
                if (!cover[a]) { a++; continue; }
                int b = a; while (b < t->size && cover[b]) b++;
                Ref part = *r;
                if (a) { part.rm = 1; part.rm_start = a + 1; part.rm_len = b - a; part.rm_lx = NULL; part.rm_nat = 0; }   /* bytes */
                Arg args[3] = { arg_ref(&part), arg_label(lit_label(tmp.image + a, b - a)), arg_imm(b - a) };
                emit_args(args, 3);
                if (!t->is_group && is_numeric_sym(t)) cen_bless(t);        /* a numeric item: zero, stored */
                emit_call("memcpy");
                a = b;
            }
            free(cover); free(tmp.image);
            long sub[MAXDIM];
            if (t->is_group) {
                init_replace_walk(t, r, PIC_NUMERIC_EDITED, &fig_zero, sub, r->nsub, r->line, 0);
                init_replace_walk(t, r, PIC_ALPHANUMERIC_EDITED, &fig_space, sub, r->nsub, r->line, 0);
                init_replace_walk(t, r, PIC_BOOLEAN, &fig_zero, sub, r->nsub, r->line, 1);   /* bit items, a MOVE each */
                if (dyn_table_below(t)) init_dyn_fill(t, 0, r->line);
                if (vlen_below(t)) { long sub2[MAXDIM]; for (int q = 0; q < r->nsub; q++) sub2[q] = 0; init_dynl_walk(t, r, sub2, r->nsub, r->line); }
            } else if (t->pi.category == PIC_NUMERIC_EDITED) emit_move(&fig_zero, r);
            else if (t->pi.category == PIC_ALPHANUMERIC_EDITED) emit_move(&fig_space, r);
            else if (t->usage == U_BIT) emit_move(&fig_zero, r);
        }
    }
    if (accept_word("replacing")) {
        int seen[8] = { 0 };
        for (;;) {
            int line = cur()->line, cat = -1;
            if (accept_word("alphabetic")) cat = PIC_ALPHABETIC;
            else if (accept_word("alphanumeric")) cat = PIC_ALPHANUMERIC;
            else if (accept_word("numeric")) cat = PIC_NUMERIC;
            else if (accept_word("alphanumeric-edited")) cat = PIC_ALPHANUMERIC_EDITED;
            else if (accept_word("numeric-edited")) cat = PIC_NUMERIC_EDITED;
            else if (g_std >= 2002 && accept_word("national")) cat = PIC_NATIONAL;
            else if (g_std >= 2002 && accept_word("boolean")) cat = PIC_BOOLEAN;
            else die_at(line, "INITIALIZE REPLACING: expected ALPHABETIC, ALPHANUMERIC, NUMERIC, ALPHANUMERIC-EDITED, NUMERIC-EDITED%s",
                        g_std >= 2002 ? " or NATIONAL" : "");
            if (cat >= 0 && cat < 8 && seen[cat]++)
                die_at(line, "INITIALIZE REPLACING: a category named twice (%s)", g_std < 2002 ? "X3.23-1985 INITIALIZE syntax rule 3" : "2023 14.9.20.3 rule 6");
            accept_word("data"); expect_word("by");
            Opnd value; parse_operand(&value);
            if (value.kind != O_REF && value.kind != O_STR && value.kind != O_NUM && value.kind != O_FIG)
                die_at(line, "INITIALIZE REPLACING ... BY needs an item or a literal");
            emit_incompat(&value);
            for (int i = 0; i < n; i++) {
                Sym *t = rs[i].sym;
                if (t->is_group && dyn_table_below(t))
                    die_at(rs[i].line, "INITIALIZE '%s' REPLACING: the group holds the dynamic-capacity table '%s', whose elements this stage initializes only to their initial state (INITIALIZE without REPLACING, or ... ALL TO VALUE)", t->name, dyn_table_below(t)->name);
                long sub[MAXDIM];
                for (int k = 0; k < rs[i].nsub && k < MAXDIM; k++) sub[k] = 0;
                int rcat = rs[i].user_rm ? (rs[i].rm_bit || t->pi.category == PIC_BOOLEAN ? PIC_BOOLEAN : rs[i].rm_nat ? PIC_NATIONAL : PIC_ALPHANUMERIC)
                                         : t->pi.category;      /* a part is of its part's category */
                if (!t->is_group || rs[i].user_rm) {
                    if (!t->is_filler && rcat == cat) { Ref r = rs[i]; emit_move(&value, &r); }
                } else init_replace_walk(t, &rs[i], cat, &value, sub, rs[i].nsub, line, 0);
            }
            if (!(at_word("alphabetic") || at_word("alphanumeric") || at_word("numeric") || at_word("alphanumeric-edited") ||
                  at_word("numeric-edited") || (g_std >= 2002 && (at_word("national") || at_word("boolean"))))) break;
        }
    }
}

/* ---- SEARCH ------------------------------------------------------------ */

/* SEARCH table [VARYING id] [AT END s] {WHEN cond s}... [END-SEARCH]
 * walks the table's first index from its current value, a serial scan.
 * SEARCH ALL is a binary search over the table's KEYs when its WHEN has
 * the form the standard gives it -- key (index) = value, joined by AND,
 * the keys a leading run of the OCCURS KEY list -- and a scan from 1
 * otherwise (a scan finds the same entry when the keys are unique, and
 * any entry when they are not, which the standard allows).  The bound is
 * the OCCURS count, or the DEPENDING ON item. */

/* SEARCH ALL: is o the table's key k, subscripted by exactly the index? */
static int sa_key_of(const Opnd *o, Sym *tbl, Sym *ix)
{
    if (o->kind != O_REF || o->ref.rm || o->ref.nsub != 1 || o->ref.sub[0].sym != ix || o->ref.sub[0].adj != 0) return -1;   /* ix, not ix + n or ix - n (rule 8) */
    const Sym *x = o->ref.sym;
    int inside = 0;
    for (const Sym *a = x; a; a = a->parent >= 0 ? &g_sym[a->parent] : NULL) if (a == tbl) { inside = 1; break; }
    if (!inside) return -1;
    for (int k = 0; k < tbl->nokey; k++) if (!strcmp(tbl->okey[k], x->name)) return k;
    return -1;
}
/* does the operand depend on the index (then it is no search argument)? */
static int sa_uses_index(const Opnd *o, const Sym *ix)
{
    if (o->kind == O_REF) { for (int i = 0; i < o->ref.nsub; i++) if (o->ref.sub[i].sym == ix) return 1; return o->ref.sym == ix || o->ref.rm; }
    if (o->kind == O_EXPR) return expr_names(o->ex, sym_is, ix);
    return o->kind == O_FUNC || o->kind == O_BEXPR || o->kind == O_ADDR;
}
/* collect c's key = value relations; 0 when c has another shape */
static int sa_collect(Cond *c, Sym *tbl, Sym *ix, Cond **rel, int *n)
{
    if (c->kind == C_AND) return sa_collect(c->a, tbl, ix, rel, n) && sa_collect(c->b, tbl, ix, rel, n);
    if (c->kind != C_REL || c->op != R_EQ || c->neg || c->bstack || c->ptr || *n >= 8) return 0;
    int kx = sa_key_of(&c->x, tbl, ix), ky = sa_key_of(&c->y, tbl, ix);
    if ((kx < 0) == (ky < 0)) return 0;
    if (ky >= 0) { Opnd t = c->x; c->x = c->y; c->y = t; kx = ky; }     /* the key on the left */
    if (sa_uses_index(&c->y, ix)) return 0;
    for (int i = 0; i < *n; i++) if (sa_key_of(&rel[i]->x, tbl, ix) == kx) return 0;
    rel[(*n)++] = c;
    return 1;
}

/* SEARCH ALL's WHEN, as its format has it (2023 14.9.37.3 rules 8-11;
 * X3.23-1985 SEARCH syntax rules): data-name = value, or a condition-name
 * of one value, joined by AND; each data-name a KEY of the table
 * subscripted by exactly its first index, the value neither a key nor
 * indexed by it, the keys used a leading run of the KEY list */
static int sa_key_named(const Opnd *o, Sym *tbl)      /* the KEY o names, whatever its subscripts; -1 */
{
    if (o->kind != O_REF) return -1;
    const Sym *x = o->ref.sym;
    int inside = 0;
    for (const Sym *a = x; a; a = a->parent >= 0 ? &g_sym[a->parent] : NULL) if (a == tbl) { inside = 1; break; }
    if (!inside) return -1;
    for (int k = 0; k < tbl->nokey; k++) if (!strcmp(tbl->okey[k], x->name)) return k;
    return -1;
}
static void sa_validate(Cond *c, Sym *tbl, Sym *ix, unsigned *used, int line)
{
    int e85 = g_std < 2002;
    const char *r8 = e85 ? "X3.23-1985 SEARCH syntax rule 4" : "2023 14.9.37.3 rules 8-9", *r10 = e85 ? "X3.23-1985 SEARCH syntax rule 4" : "2023 14.9.37.3 rule 10";
    if (c->kind == C_AND) { sa_validate(c->a, tbl, ix, used, line); sa_validate(c->b, tbl, ix, used, line); return; }
    if (c->kind != C_REL || c->op != R_EQ || c->neg)
        die_at(line, "SEARCH ALL ... WHEN: a key = a value, or a condition-name of one value, joined by AND; no OR, NOT or other relation (%s)", r8);
    int kx = sa_key_named(&c->x, tbl), ky = sa_key_named(&c->y, tbl);
    if (kx < 0)
        die_at(line, ky >= 0 ? "SEARCH ALL ... WHEN: the KEY data-name is written first, key = value (%s)"
                             : "SEARCH ALL ... WHEN: each relation tests a KEY of the table (%s)", r8);
    /* the index at the table's own level; the outer levels' subscripts
     * are whatever the program says (CCVS-85 NC233A, NC237A) */
    const Ref *kr = &c->x.ref; int lv = tbl->ndims - 1;
    if (kr->rm || kr->nsub != kr->sym->ndims || lv < 0 || lv >= kr->nsub || kr->sub[lv].sym != ix || kr->sub[lv].adj != 0)
        die_at(line, "SEARCH ALL ... WHEN: the key '%s' is subscripted by the table's first index '%s' at its level, without + or - (%s)",
               c->x.ref.sym->name, ix->name, r8);
    no_zero_lit(&c->y, "SEARCH ALL ... WHEN", "2023 14.9.37.3 rule 13");
    if (ky >= 0 || sa_uses_index(&c->y, ix))
        die_at(line, "SEARCH ALL ... WHEN: the value compared with '%s' is neither a key of the table nor subscripted by '%s' (%s)",
               c->x.ref.sym->name, ix->name, r10);
    if (*used & (1u << kx)) die_at(line, "SEARCH ALL ... WHEN: the key '%s' is tested twice", c->x.ref.sym->name);
    *used |= 1u << kx;
}

static void parse_search(void)
{
    int all = accept_word("all");
    /* the table is named without subscripts */
    Tok *tt = cur();
    if (tt->kind != T_WORD) die_at(tt->line, "SEARCH needs a table name");
    Ref t; memset(&t, 0, sizeof t); t.line = tt->line;
    g_cen_ctx = CEN_PLAIN; t.sym = sym_lookup(tt->s, NULL, 0, tt->line); g_cen_ctx = 0; advance();
    if (g_cen_on) cen_pin(t.sym, "SEARCH");         /* the table as entries of its size */
    Sym *tbl = t.sym;
    if (!tbl->occurs) die_at(t.line, "SEARCH needs a table (an item with OCCURS)");
    if (tbl->dyn) {
        /* a dynamic-capacity table: searched to its capacity; its capacity
         * is not SET within the statement (14.9.39.4 rule 31) */
        if (g_nsearch_dyn == 16) die_at(t.line, "SEARCH statements nested too deep");
        g_search_dyn[g_nsearch_dyn++] = tbl;
    }
    if (cur()->kind == T_LP) die_at(t.line, "SEARCH names the table without subscripts");
    if (tbl->idx1 < 0) die_at(t.line, "SEARCH needs the table to have INDEXED BY");
    Sym *ix = &g_sym[tbl->idx1];
    Ref ixr; memset(&ixr, 0, sizeof ixr); ixr.sym = ix; ixr.line = t.line;
    Ref vary; int has_vary = 0;
    if (accept_word("varying")) { parse_ref(&vary); has_vary = 1; no_constrec_recv(&vary, "SEARCH VARYING"); if (!is_int_item(vary.sym)) die_at(vary.line, "VARYING needs an integer or index item"); }
    if (all && has_vary) die_at(t.line, "SEARCH ALL takes no VARYING");
    if (has_vary && vary.sym->is_index && vary.sym->ix_table == sym_idx(tbl)) {
        /* VARYING one of the table's own indexes: that index does the search */
        ix = vary.sym; ixr.sym = ix; has_vary = 0;
    }

    /* the phrases first: AT END's statements and each WHEN's body are
     * parsed here, once, and their code cut out (Block) to go after the
     * loop, which holds only the tests */
    int has_atend = 0; Block atend = { NULL, 0 };
    if (at_word("at") || at_word("end")) {          /* [AT] END: AT is optional (NC237A writes SEARCH ALL t END GO TO ...) */
        accept_word("at"); expect_word("end");
        has_atend = 1;
        int b0 = block_begin(); parse_statements(); atend = block_cut(b0);
    }
    Cond *wc[16]; Block wbody[16]; int wnext[16], nwhen = 0, next_sent = 0;
    while (at_word("when")) {
        if (nwhen >= 16) die_at(cur()->line, "too many WHENs in SEARCH");
        int wline = cur()->line;
        if (all && nwhen) die_at(wline, "SEARCH ALL has one WHEN (%s)", g_std < 2002 ? "X3.23-1985 SEARCH format 2" : "2023 14.9.37 format 2");
        advance();
        wc[nwhen] = parse_cond();
        if (all) {
            if (!tbl->nokey) die_at(wline, "SEARCH ALL '%s': its OCCURS clause has no KEY phrase (%s)", tbl->name,
                                   g_std < 2002 ? "X3.23-1985 SEARCH syntax rule 1" : "2023 14.9.37.3 rule 7");
            for (int k = 0; k < tbl->nokey; k++)            /* Micro Focus: a KEY data item is no floating-point item */
                for (int j = (int)(tbl - g_sym) + 1; j < g_nsym; j++) {
                    int in = 0;                          /* the key is the table entry's descendant */
                    for (int a2 = g_sym[j].parent; a2 >= 0 && !in; a2 = g_sym[a2].parent) in = &g_sym[a2] == tbl;
                    if (in && !strcmp(g_sym[j].name, tbl->okey[k]) && (g_sym[j].usage == U_FLOAT || g_sym[j].usage == U_DFLOAT))
                        die_at(wline, "SEARCH ALL: the KEY '%s' is a floating-point item (Micro Focus: SEARCH rules)", g_sym[j].name);
                }
            unsigned used = 0;
            sa_validate(wc[nwhen], tbl, ix, &used, wline);
            for (int k = 0; k < tbl->nokey; k++)
                if (!(used & (1u << k)) && (used >> k))
                    die_at(wline, "SEARCH ALL ... WHEN tests a later KEY without '%s', which comes before it (%s)", tbl->okey[k],
                           g_std < 2002 ? "X3.23-1985 SEARCH syntax rule 4" : "2023 14.9.37.3 rule 11");
        }
        wnext[nwhen] = 0; wbody[nwhen].line = NULL; wbody[nwhen].n = 0;
        if (at_word("next")) { advance(); expect_word("sentence"); next_sent = 1; wnext[nwhen] = 1; }
        else { int b0 = block_begin(); parse_statements(); wbody[nwhen] = block_cut(b0); }
        nwhen++;
    }
    if (!nwhen) die_at(t.line, "SEARCH needs at least one WHEN");

    int Lend = new_label(), Latend = new_label(), Lwhen[16];
    for (int i = 0; i < nwhen; i++) Lwhen[i] = new_label();
    Cond *rel[8]; int nrel = 0;
    int binary = all && nwhen == 1 && tbl->ndims == 1 && tbl->nokey > 0 && !(wc[0]->uc1 > wc[0]->uc0) &&
                 sa_collect(wc[0], tbl, ix, rel, &nrel) && nrel > 0 && is_hot_int(ix);
    if (binary) {
        /* the keys used must be the first ones declared (2023 14.9.37.3
         * rule 8); in declared order they steer the search */
        Cond *ord[8]; int no = 0;
        for (int k = 0; k < tbl->nokey && no < nrel; k++) {
            int f = -1;
            for (int i = 0; i < nrel; i++) if (sa_key_of(&rel[i]->x, tbl, ix) == k) f = i;
            if (f < 0) break;
            ord[no++] = rel[f];
        }
        if (no != nrel) binary = 0;
        else {
            /* lo, hi and the middle in slots of their own: the key tests
             * use SLOT_A and the staging slots above g_slot_base */
            int base = g_slot_base; g_slot_base += 3;
            if (g_slot_base > NSLOTS) die_at(t.line, "internal: too many staged operands");
            int lo = SLOT(base), hi = SLOT(base + 1), mid = SLOT(base + 2);
            int Ltop = new_label(), Lup = new_label(), Ldown = new_label();
            emit_li("r1", 1); emit("\tstw sp+%d, r1", lo);
            emit_table_count(tbl, t.line);
            emit("\tstw sp+%d, r1", hi);
            emit_label(Ltop);
            emit("\tldw r1, sp+%d", lo); emit("\tldw r2, sp+%d", hi);
            emit("\tslt r3, r2, r1");                        /* hi < lo: not there */
            emit("\tbne r3, r0, .L%d", Latend);
            emit("\tadd r1, r1, r2"); emit("\tsrli r1, r1, 1");
            emit("\tstw sp+%d, r1", mid);
            emit_ref_addr(&ixr, "r3");
            emit("\tldw r1, sp+%d", mid);
            emit_store_int(ix, "r3", "r1");                  /* the index at the middle entry */
            for (int i = 0; i < no; i++) {
                /* key below the argument: the entry sought lies after the
                 * middle for an ascending key, before it for a descending one */
                int desc = tbl->okey_desc[sa_key_of(&ord[i]->x, tbl, ix)];
                Cond *lt = cond_new(C_REL); *lt = *ord[i]; lt->op = R_LT;
                Cond *gt = cond_new(C_REL); *gt = *ord[i]; gt->op = R_GT;
                cond_jump_true(lt, desc ? Ldown : Lup);
                cond_jump_true(gt, desc ? Lup : Ldown);
            }
            emit_jump(Lwhen[0]);                             /* every key equal: found */
            emit_label(Lup);                                 /* lo = mid + 1 */
            emit("\tldw r1, sp+%d", mid); emit("\taddi r1, r1, 1"); emit("\tstw sp+%d, r1", lo);
            emit_jump(Ltop);
            emit_label(Ldown);                               /* hi = mid - 1 */
            emit("\tldw r1, sp+%d", mid); emit("\taddi r1, r1, -1"); emit("\tstw sp+%d, r1", hi);
            emit_jump(Ltop);
            g_slot_base = base;
        }
    }
    if (!binary) {
        int Ltop = new_label();
        Opnd one; memset(&one, 0, sizeof one); one.kind = O_NUM; numlit_from_int(&one.num, 1); one.line = t.line;
        if (all) emit_move(&one, &ixr);
        else if (ec_on_name("EC-RANGE-SEARCH-INDEX")) {
            /* the search index outside the table at the start: the search
             * is unsuccessful and the condition exists (2023 14.9.37.4 GR 4) */
            int Lok = new_label(), Lbad = new_label();
            Opnd ixo; memset(&ixo, 0, sizeof ixo); ixo.kind = O_REF; ixo.ref = ixr; ixo.line = t.line;
            emit_hot_value(&ixo);
            emit("\tstw sp+%d, r1", SLOT_A);
            emit("\tbge r0, r1, .L%d", Lbad);                  /* 0 or less */
            emit_table_count(tbl, t.line);
            emit("\tldw r2, sp+%d", SLOT_A);
            emit("\tslt r1, r1, r2");
            emit("\tbeq r1, r0, .L%d", Lok);
            emit_label(Lbad);
            emit_ec_raise(ec_find("EC-RANGE-SEARCH-INDEX", 0));
            emit_jump(Latend);
            emit_label(Lok);
        }
        emit_label(Ltop);
        /* at end when the index passes the bound */
        Opnd ixo; memset(&ixo, 0, sizeof ixo); ixo.kind = O_REF; ixo.ref = ixr; ixo.line = t.line;
        emit_hot_value(&ixo);
        if (!tbl->odo_dep_sym && !tbl->dyn && !g_nohx) {          /* a fixed bound: no spill */
            emit_li("r2", tbl->occurs);
            emit("\tslt r1, r2, r1");               /* bound < index */
        } else {
            emit("\tstw sp+%d, r1", SLOT_A);
            emit_table_count(tbl, t.line);
            emit("\tldw r2, sp+%d", SLOT_A);
            emit("\tslt r1, r1, r2");                /* bound < index */
        }
        emit("\tbne r1, r0, .L%d", Latend);
        for (int i = 0; i < nwhen; i++) cond_jump_true(wc[i], Lwhen[i]);
        /* no WHEN held: step and go round */
        Opnd step; memset(&step, 0, sizeof step); step.kind = O_NUM; numlit_from_int(&step.num, 1); step.line = t.line;
        emit_add_to_ref(&step, &ixr);
        if (has_vary && vary.sym != ixr.sym) emit_add_to_ref(&step, &vary);   /* VARYING the table's own index: once */
        emit_jump(Ltop);
    }

    /* AT END */
    emit_label(Latend);
    if (ec_on_name("EC-RANGE-SEARCH-NO-MATCH")) emit_ec_raise(ec_find("EC-RANGE-SEARCH-NO-MATCH", 0));   /* the search unsuccessful (2023 14.9.37.4) */
    if (has_atend) block_put(&atend);
    emit_jump(Lend);
    /* WHEN bodies */
    for (int i = 0; i < nwhen; i++) {
        emit_label(Lwhen[i]);
        if (wnext[i]) { if (g_sentence_label < 0) g_sentence_label = new_label(); emit_jump(g_sentence_label); }
        else block_put(&wbody[i]);
        emit_jump(Lend);
    }
    emit_label(Lend);
    if (tbl->dyn) g_nsearch_dyn--;
    if (at_word("end-search") && next_sent)
        die_at(cur()->line, "SEARCH with NEXT SENTENCE ends at the period, not END-SEARCH (%s)", g_std < 2002 ? "X3.23-1985 SEARCH syntax rule 5" : "2023 14.9.37.3 rule 4");
    accept_word("end-search");
}
