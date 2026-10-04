/* s32-cobc: GO TO, SET.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- GO TO, SET ------------------------------------------------------- */

/* declaratives and the rest meet only by PERFORM (2023 14.9.49.3 rules
 * 3-4; X3.23-1985 USE rules 3-4): a declarative procedure names no
 * nondeclarative one, and a declarative one is named from outside its
 * section only by PERFORM */
static void decl_ref_check(const Para *p, int is_perform, int line)
{
    if (g_in_decl && !p->in_decl)
        die_at(line, "a declarative procedure refers to '%s', which is not in the declaratives (2023 14.9.49.3 rule 3)", p->oname);
    if (!is_perform && p->in_decl) {
        int here = g_cur_sec_id, there = p->is_section ? p->id : p->section;
        if (!g_in_decl || here != there)
            die_at(line, "'%s' is in a declarative section: it is named from outside that section only by PERFORM (2023 14.9.49.3 rule 4)", p->oname);
    }
}

/* an unconditional GO TO ends its run of imperative statements (2023
 * 14.9.17.3 rule 2; X3.23-1985 GO TO syntax rule 2): nothing after it
 * could be reached */
static void goto_last_check(void)
{
    if (cur()->kind == T_WORD && is_verb(cur()->s))
        die_at(cur()->line, "a GO TO is the last statement of its sequence; '%s' after it is never reached (%s)", cur()->s,
               g_std < 2002 ? "X3.23-1985 GO TO syntax rule 2" : "2023 14.9.17.3 rule 2");
}

static int lw_go_to(int target);           /* lower.h */
static void parse_goto(void)
{
    if (g_in_finally) die_at(cur()->line, "GO TO in a FINALLY phrase: no statement there transfers control out of the PERFORM (2023 14.9.28.4 rule 16)");
    if (g_in_ecp_when) die_at(cur()->line, "GO TO in a WHEN phrase of an exception-checking PERFORM (2023 14.9.17.3 rule 3)");
    accept_word("to");
    Para *ps[64]; int n = 0;
    while (at_para_name(cur()) && !at_word("depending") && !(cur()->kind == T_WORD && (is_verb(cur()->s) || is_terminator(cur()->s))) && para_find(cur()->s)) {
        if (n >= 64) die_at(cur()->line, "too many GO TO targets");
        ps[n++] = expect_para();
        decl_ref_check(ps[n - 1], 0, cur()->line);
    }
    int altered = g_cur_para && is_altered_para(g_cur_para->name);
    if (!n && !altered && at_para_name(cur()) && !(cur()->kind == T_WORD && (is_verb(cur()->s) || is_terminator(cur()->s))) &&
        !at_word("depending"))
        die_at(cur()->line, "'%s' is not a paragraph or section", cur()->s);
    if (!n && !altered) die_at(cur()->line, "GO TO without a procedure-name: the paragraph is not named in any ALTER");
    if (altered && n <= 1 && !at_word("depending")) {
        /* through the paragraph's cell, which ALTER rewrites */
        if (g_naltcell == 64) die_at(cur()->line, "too many altered paragraphs");
        g_altcell[g_naltcell].para = g_cur_para->id; g_altcell[g_naltcell].target = n ? ps[0]->id : -1; g_naltcell++;
        if (n) pc_goto(ps[0], "goto");
        char lab[32]; snprintf(lab, sizeof lab, ".Lalt%d_%d", g_unit, g_cur_para->id);
        emit_la("r1", lab);
        emit("\tldw r1, r1+0");
        emit("\tjalr r0, r1, 0");
        goto_last_check();
        return;
    }
    if (accept_word("depending")) {
        accept_word("on");
        Opnd o; parse_operand(&o);
        if (o.kind != O_REF || !is_int_item(o.ref.sym)) die_at(o.line, "GO TO DEPENDING ON needs an integer item");
        emit_incompat(&o);
        if (is_hot_int(o.ref.sym)) emit_hot_value(&o);
        else { Arg a[2] = { arg_ref(&o.ref), arg_desc(sym_desc(o.ref.sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
        for (int i = 0; i < n; i++) {
            emit_li("r2", i + 1);
            emit("\tbeq r1, r2, .Lp%d_%d", g_unit, ps[i]->id);
            pc_goto(ps[i], "depending");
        }
        return;
    }
    if (n != 1) die_at(cur()->line, "GO TO with several procedure-names needs DEPENDING ON");
    lw_go_to(ps[0]->id);                    /* an island's too (lower.h): a branch, inside an inlined range that holds the target */
    emit("\tjal r0, .Lp%d_%d", g_unit, ps[0]->id);
    pc_goto(ps[0], "goto");
    goto_last_check();
}

/* How well a WHEN (or USE) exception-name w, for file wf (-1: none),
 * matches condition i raised on file fidx: the order of USE general rule
 * 3c-3g (14.9.49.4), which 14.9.28 rule 17 points at -- the name with its
 * file, its group with the file, the name, the group, EC-ALL.  0 best;
 * -1 no match. */
static int ec_match_rank(int w, int wf, int i, int fidx)
{
    int g = ec_group(i);
    if (wf >= 0) {
        if (wf != fidx) return -1;
        return w == i ? 0 : w == g ? 1 : -1;
    }
    return w == i ? 2 : w == g ? 3 : w == ec_find("EC-ALL", 0) ? 4 : -1;
}

/* the WHEN phrase of the innermost exception-checking PERFORM that takes
 * condition i here, or -1; *resume set to where its return goes.  A fatal
 * condition goes to a WHEN that names it or its hierarchy only, never to
 * WHEN OTHER (14.6.13.1.3 rule 4); a nonfatal one to WHEN OTHER when no
 * WHEN names it (14.9.28 rule 18). */
static int ecp_target(int i, int fidx, Ecp **ep, int *resume)
{
    for (int k = g_necp - 1; k >= 0; k--) {
        Ecp *e = g_ecp[k];
        int best = -1, rank = 5;
        for (int w = 0; w < e->nw; w++)
            for (int q = 0; q < e->w[w].n; q++) {
                int r = ec_match_rank(e->w[w].ec[q], e->w[w].file[q], i, fidx);
                if (r >= 0 && r < rank) { rank = r; best = e->w[w].label; }
            }
        *ep = e; *resume = e->resume;
        if (best >= 0) return best;
        if (e->Lother >= 0 && !ec_fatal(i)) { *resume = e->Lend; return e->Lother; }
    }
    return -1;
}

/* a raise inside imperative-statement-1 that a WHEN takes: the resume
 * point and fatality onto libcob's stack of them (a recursive activation's
 * raise pushes its own; cobol ISSUES-94 E9), then the phrase; returns 1
 * when a WHEN took it */
static int ecp_dispatch(int i)
{
    Ecp *e; int resume;
    int target = ecp_target(i, g_ec_fidx, &e, &resume);
    if (target < 0) return 0;
    char lab[32]; snprintf(lab, sizeof lab, ".L%d", resume);
    emit_li("r3", e->id);
    emit_la("r4", lab);
    emit_li("r5", ec_fatal(i));
    emit("\tadd r6, sp, r0");                   /* the activation's frame */
    emit_call("cob_ecp_push");
    emit_jump(target);
    return 1;
}

/* after exception condition i is raised: a WHEN of the exception-checking
 * PERFORM around it, else the declarative that applies -- this program's
 * USE for the name, else its group's, else EC-ALL's (2023 14.6.13.1.3-4)
 * -- then stop the run if i is fatal */
static void emit_ec_dispatch(int i)
{
    if (g_necp && ecp_dispatch(i)) return;          /* the WHEN takes it; USE does not (17) */
    int cand[3] = { i, ec_group(i), ec_find("EC-ALL", 0) }, sec = -1;
    for (int c = 0; c < 3 && sec < 0; c++)
        for (int u = unit_use_own_from(); u < g_nuse; u++)
            if (g_use[u].unit == g_unit && g_use[u].ec >= 0 && g_use[u].ec == cand[c]) { sec = g_use[u].sec; break; }
    if (sec >= 0) {
        int Lret = new_label();
        char lab[32]; snprintf(lab, sizeof lab, ".L%d", Lret);
        emit_para_cell("r3", g_unit, sec);
        emit_la("r4", lab);
        emit_call("cob_use_push");                  /* EC-FLOW-USE when it is active already (E14) */
        emit("\tjal r0, .Lp%d_%d", g_unit, sec);
        emit_label(Lret);
    }
    if (ec_fatal(i)) emit_call("cob_ec_abort");     /* abnormal run unit termination (14.6.12) */
}

/* EC-SIZE (cobol ISSUES-55): with checking on for any of the conditions a
 * statement's arithmetic can meet, the statement is compiled as if it had
 * a SIZE ERROR phrase, and the phrase's place raises the condition libcob
 * saw -- EC-SIZE-ZERO-DIVIDE, -OVERFLOW (the 18-digit intermediate), or
 * -TRUNCATION (a result too large for its receiver), 14.7.5 */
static int ec_size_on(void)
{
    if (g_std < 2002) return 0;
    static const char *n[] = { "EC-SIZE-ZERO-DIVIDE", "EC-SIZE-OVERFLOW", "EC-SIZE-TRUNCATION", "EC-SIZE-EXPONENTIATION" };
    for (int k = 0; k < 4; k++) if (g_ecs.on[ec_find(n[k], 0)]) return 1;
    return 0;
}

static void emit_ec_size(void)
{
    static const char *n[] = { "EC-SIZE-ZERO-DIVIDE", "EC-SIZE-OVERFLOW", "EC-SIZE-TRUNCATION", "EC-SIZE-EXPONENTIATION" };
    int Ldone = new_label();
    emit_call("cob_size_kind");
    emit("\tadd r13, r1, r0");
    for (int k = 0; k < 4; k++) {
        int i = ec_find(n[k], 0);
        if (!g_ecs.on[i]) continue;
        int Lnext = new_label();
        emit_li("r2", k + 1);
        emit("\tbne r13, r2, .L%d", Lnext);
        emit_ec_raise(i);
        emit_jump(Ldone);
        emit_label(Lnext);
    }
    emit_label(Ldone);
}

static int ec_on_name(const char *name) { return g_std >= 2002 && g_ecs.on[ec_find(name, 0)]; }
/* an EC-I-O condition for one file: its TURN for that file, else for all */
static int ec_on_io(const char *name, int file) { return g_std >= 2002 && ec_on_file(ec_find(name, 0), file, NULL); }

/* EXCEPTION-LOCATION's string (2002 15.25.2 rule 2b), known here: the
 * program-name; the paragraph, OF its section, or the section; the line.
 * The line is implementor-defined: its number, and the copybook's name
 * before it when the statement came from one. */
static void ec_location(char *b, size_t n)
{
    int k = snprintf(b, n, "%s; ", g_progid_orig);
    Para *p = g_cur_para;
    if (p && !p->is_section && p->section > 0)
        k += snprintf(b + k, n - (size_t)k, "%s OF %s; ", p->oname, g_para[p->section - 1].oname);
    else if (p) k += snprintf(b + k, n - (size_t)k, "%s; ", p->oname);
    else k += snprintf(b + k, n - (size_t)k, "; ");
    const Tok *t = g_stmt_tok;
    if (t && t->file && g_ntok && g_tok[0].file && strcmp(t->file, g_tok[0].file)) {
        const char *base = strrchr(t->file, '/');
        snprintf(b + k, n - (size_t)k, "%s:%d", base ? base + 1 : t->file, t->line);
    } else snprintf(b + k, n - (size_t)k, "%d", t ? t->line : 0);
}

/* raise condition i here: the last exception status, the statement's
 * name when WITH LOCATION turned it on, then the declarative and fatality */
static void emit_ec_raise(int i)
{
    char nm[64]; snprintf(nm, sizeof nm, "%s", ec_name(i));
    emit_la("r3", lit_label((const unsigned char *)nm, (int)strlen(nm) + 1));
    int loc = g_ecs.loc[i];
    if (g_ec_fidx >= 0) ec_on_file(i, g_ec_fidx, &loc);
    if (loc && g_cur_stmt[0]) emit_la("r4", lit_label((const unsigned char *)g_cur_stmt, (int)strlen(g_cur_stmt) + 1));
    else emit_li("r4", 0);
    if (loc) {
        char loc[256]; ec_location(loc, sizeof loc);
        emit_la("r5", lit_label((const unsigned char *)loc, (int)strlen(loc) + 1));
    } else emit_li("r5", 0);
    if (g_ec_file) emit_la("r6", lit_label((const unsigned char *)g_ec_file, (int)strlen(g_ec_file) + 1));
    else emit_li("r6", 0);
    emit_call("cob_ec_raise");
    emit_ec_dispatch(i);
}

/* RAISE EXCEPTION exception-name (2023 14.9.29).  Everything is known here:
 * whether checking is on at this statement, the declarative that applies
 * (the name's own USE, its group's, EC-ALL's), and whether the condition
 * is fatal.  Checking off: the statement does nothing. */
static void parse_raise(void)
{
    int line = cur()->line;
    if (!accept_word("exception")) die_at(line, "RAISE of an exception object is object orientation, not implemented");
    if (cur()->kind != T_WORD) die_at(line, "RAISE EXCEPTION needs an exception-name");
    int i = ec_find(cur()->s, line);
    if (i < 0) die_at(line, "'%s' is not an exception-name", cur()->s);
    if (ec_level(i) != 3) die_at(line, "RAISE needs a level-3 exception-name, not %s", ec_name(i));
    if (g_ecp_handler) die_at(line, "RAISE in a WHEN or FINALLY phrase of an exception-checking PERFORM (2023 14.9.29.3 rule 4)");
    advance();
    if (!g_ecs.on[i]) return;
    emit_ec_raise(i);
}

/* SET formats 1 and 2 (X3.23-1985 6.23; 2023 14.9.39): what an operand is */
enum { SK_OTHER, SK_INDEX, SK_IXD, SK_INT };
static int set_kind(Sym *s)
{
    if (s->is_index) return SK_INDEX;
    if (!s->is_group && s->usage == U_INDEX) return SK_IXD;
    if (is_numeric_sym(s) && s->usage != U_FLOAT && s->pi.scale <= 0) return SK_INT;
    return SK_OTHER;
}

/* is SET's sending operand an arithmetic expression, not an operand
 * alone?  (only where one is allowed: 2002 and later, index receivers) */
static int set_at_expr(int never)
{
    if (never) return 0;
    if (!at_operand()) return 1;                /* (, a sign */
    int save = g_tp; Opnd o;
    g_noemit++; parse_operand(&o); int more = at_arith_op(); g_noemit--;
    g_tp = save;
    return more;
}

/* SET's arithmetic-expression-1 or -2 when it is not a whole number on
 * its face: its value, once, into sp+SLOT_A; with EC-BOUND-SUBSCRIPT
 * checked, a value that is not an integer raises it and the receivers
 * are left alone (2023 14.9.39.4 rules 2 and 3) */
static void emit_set_value(Opnd *v, int Lskip)
{
    int was = g_wide, wasf = g_fstmt, chk = ec_on_name("EC-BOUND-SUBSCRIPT");
    if (opnds_wide(v, 1) || g_fstmt || (v->kind == O_FUNC && v->fwnum)) g_wide = 1;
    emit_push_opnd(v);
    emit_call(chk ? "cob_pop_pos" : "cob_pop_int");
    g_wide = was; g_fstmt = wasf;
    emit("\tstw sp+%d, r1", SLOT_A);
    if (chk) {
        int Lok = new_label();
        emit_call("cob_pos_nonint");
        emit("\tbeq r1, r0, .L%d", Lok);
        emit_ec_raise(ec_find("EC-BOUND-SUBSCRIPT", 0));
        emit_jump(Lskip);
        emit_label(Lok);
    }
}

/* SET condition-name TO TRUE or FALSE: the literal goes in by the VALUE
 * clause's rules (2023 14.9.39.4 rules 6-7), not MOVE's.  They differ
 * for an edited item given an alphanumeric literal: VALUE places the
 * characters as written (13.18.63.3 rules 4 and 7-8), MOVE would edit
 * them -- and then the condition was false after SET ... TO TRUE */
static void set_cond_move(Opnd *v, Ref *p)
{
    Sym *x = p->sym;
    int alnum = v->kind == O_STR || v->kind == O_ALL ||
                (v->kind == O_FIG && v->tok && strncmp(v->tok->s, "zero", 4));
    if (alnum && !x->is_group && !p->rm &&
        (x->pi.category == PIC_NUMERIC_EDITED || x->pi.category == PIC_ALPHANUMERIC_EDITED)) {
        p->rm = 1; p->rm_start = 1; p->rm_len = x->size;
    }
    emit_move(v, p);
}

static void env_text_args(Opnd *o, const char *what);
static void parse_set(void)
{
    Ref rs[MAXOPS]; int nr = 0;
    if (at_word("environment") && !sym_lookup_quiet("environment")) {
        /* SET ENVIRONMENT name TO value (GnuCOBOL's; BP-E31): a variable
         * the run unit's later ACCEPT ... FROM ENVIRONMENT reads */
        int line = cur()->line;
        bp(BP_E31_ENVIRONMENT, line);
        advance();
        Opnd no; parse_operand(&no);
        env_text_args(&no, "SET ENVIRONMENT");
        emit("\tstw sp+%d, r3", SLOT_A); emit("\tstw sp+%d, r4", SLOT_B);
        expect_word("to");
        Opnd vo; parse_operand(&vo);
        env_text_args(&vo, "SET ENVIRONMENT ... TO");
        emit("\tadd r5, r3, r0"); emit("\tadd r6, r4, r0");
        emit("\tldw r3, sp+%d", SLOT_A); emit("\tldw r4, sp+%d", SLOT_B);
        emit_call("cob_env_set_named");
        return;
    }
    if (g_std >= 2002 && at_word("last") && is_word(peek(1), "exception")) {
        /* SET LAST EXCEPTION TO OFF (2023 14.9.39): no exception condition exists */
        advance(); advance(); expect_word("to"); expect_word("off");
        emit_call("cob_ec_clear");
        return;
    }
    if (cur()->kind == T_WORD && switch_find(cur()->s) && switch_find(cur()->s)->on < 0) {
        /* SET {mnemonic-name ... TO ON | OFF}... (NC174A: SET SW-1 TO ON SW-2 TO OFF) */
        while (cur()->kind == T_WORD && switch_find(cur()->s) && switch_find(cur()->s)->on < 0) {
            int sws[8], ns = 0;
            while (cur()->kind == T_WORD && switch_find(cur()->s) && switch_find(cur()->s)->on < 0) {
                if (ns < 8) sws[ns++] = switch_find(cur()->s)->sw;
                advance();
            }
            expect_word("to");
            int v = 0;
            if (accept_word("on")) v = 1; else if (accept_word("off")) v = 0;
            else die_at(cur()->line, "SET switch: expected ON or OFF");
            emit_la("r3", "cob_switches"); emit_li("r1", v);
            for (int i = 0; i < ns; i++) emit("\tstw r3+%d, r1", 4 * (sws[i] - 1));
        }
        return;
    }
    int raddr[MAXOPS], nptr = 0;
    while (at_operand()) {
        if (nr >= MAXOPS) die_at(cur()->line, "too many items in SET");
        raddr[nr] = 0;
        if (at_word("address") && is_word(peek(1), "of") && !sym_lookup_quiet("address")) {
            /* SET ADDRESS OF data-name (format 7): a based entry's implicit
             * pointer (2023 14.9.39.3 rule 18); a LINKAGE record's cell
             * likewise, as IBM and GnuCOBOL allow */
            int line = cur()->line;
            if (g_std < 2002) die_at(line, "ADDRESS OF is COBOL 2002; compile with -std=2002");
            advance(); advance();
            g_noemit++; parse_ref(&rs[nr]); g_noemit--;
            Sym *x = rs[nr].sym;
            if (rs[nr].nsub || rs[nr].rm || x->parent >= 0 || !(x->is_based || x->is_linkage))
                die_at(line, "SET ADDRESS OF '%s': it is a BASED entry, or a LINKAGE record at level 01 or 77 (2023 14.9.39.3 rule 18)", x->name);
            raddr[nr] = 1;
        } else { g_noemit++; parse_ref(&rs[nr]); g_noemit--; }   /* a receiver: identified immediately before it is changed (2023 14.9.39.4), its calls then (recv_calls) */
        if (raddr[nr] || (!rs[nr].sym->is_group && rs[nr].sym->usage == U_POINTER)) nptr++;
        nr++;
    }
    if (!nr) die_at(cur()->line, "SET needs an item");
    if (nptr && nptr != nr) die_at(rs[0].line, "SET: data-pointer receivers are not mixed with others");
    if (nptr && accept_word("to")) {
        /* format 7: the value once, then each receiver in order */
        Opnd v; parse_operand(&v);
        if (!opnd_is_ptr(&v))
            die_at(v.line, "SET of a data pointer takes ADDRESS OF, a pointer item or NULL (2023 14.9.39.3 rule 17)");
        emit_ptr_value(&v, "r1");
        emit("\tstw sp+%d, r1", SLOT_A);
        for (int i = 0; i < nr; i++) {
            recv_calls(&rs[i]);
            if (raddr[i]) emit_la("r3", g_sym[rs[i].sym->record].label);
            else emit_ref_addr(&rs[i], "r3");
            emit("\tldw r1, sp+%d", SLOT_A);
            emit("\tstw r3+0, r1");
        }
        return;
    }
    if (nptr) {
        /* format 10: SET pointer UP|DOWN BY n, in bytes */
        int down = 0;
        if (accept_word("up")) down = 0; else if (accept_word("down")) down = 1;
        else die_at(cur()->line, "expected TO, UP BY or DOWN BY in SET");
        expect_word("by");
        Opnd v; parse_operand(&v); check_numeric_opnd(&v);
        emit_incompat(&v);
        for (int i = 0; i < nr; i++) {
            if (raddr[i]) die_at(rs[i].line, "SET ADDRESS OF ... UP or DOWN: set a pointer item instead (2023 14.9.39 format 10)");
            recv_calls(&rs[i]);
            emit_push(&v); emit_call("cob_pop_int");
            emit("\tstw sp+%d, r1", SLOT_A);
            emit_ref_addr(&rs[i], "r3");
            emit("\tldw r2, r3+0");
            if (ec_on_name("EC-DATA-PTR-NULL")) {
                int Lok = new_label();
                emit("\tbne r2, r0, .L%d", Lok);
                emit_ec_raise(ec_find("EC-DATA-PTR-NULL", 0));
                emit_label(Lok);
                emit_ref_addr(&rs[i], "r3");
                emit("\tldw r2, r3+0");
            }
            emit("\tldw r1, sp+%d", SLOT_A);
            emit("\t%s r2, r2, r1", down ? "sub" : "add");
            emit("\tstw r3+0, r2");
        }
        return;
    }
    if (accept_word("to")) {
        if (accept_word("true")) {
            for (int i = 0; i < nr; i++) {
                Sym *c = rs[i].sym;
                if (!c->is_cond) die_at(rs[i].line, "'%s' is not a condition-name", c->name);
                Opnd v = lit_opnd(c->cv_lo[0]);
                if (c->cv_all & 1u) v.kind = O_ALL;
                recv_calls(&rs[i]);
                Ref p = rs[i]; p.sym = &g_sym[c->parent];
                set_cond_move(&v, &p);
            }
            return;
        }
        if (accept_word("false")) {
            if (g_std < 2002) die_at(cur()->line, "SET ... TO FALSE is COBOL 2002; compile with -std=2002");
            /* the conditional variable takes the FALSE phrase's literal
             * (2023 14.9.39.4 rule 7) */
            for (int i = 0; i < nr; i++) {
                Sym *c = rs[i].sym;
                if (!c->is_cond) die_at(rs[i].line, "'%s' is not a condition-name", c->name);
                if (!c->cv_false) die_at(rs[i].line, "SET '%s' TO FALSE: its VALUE clause has no FALSE phrase (2023 14.9.39.3 rule 7)", c->name);
                Opnd v = lit_opnd(c->cv_false);
                recv_calls(&rs[i]);
                Ref p = rs[i]; p.sym = &g_sym[c->parent];
                set_cond_move(&v, &p);
            }
            return;
        }
        /* format 1: which sending operand each receiver takes (X3.23-1985
         * SET general rule 5's table; 2023 14.9.39.3 rules 1-4) */
        int e85 = g_std < 2002, allix = 1;
        for (int i = 0; i < nr; i++) {
            int k = set_kind(rs[i].sym);
            if (k == SK_OTHER)
                die_at(rs[i].line, "SET '%s' TO: the receiver is an index-name, an index data item or an integer item (%s)", rs[i].sym->name,
                       e85 ? "X3.23-1985 SET syntax rule 2" : "2023 14.9.39.3 rule 1");
            if (k != SK_INDEX) allix = 0;
        }
        Opnd v;
        if (set_at_expr(e85 || !allix)) v = expr_opnd();
        else parse_operand(&v);
        if (at_arith_op())
            die_at(v.line, "SET ... TO an arithmetic expression: %s", e85 ? "that is COBOL 2002; compile with -std=2002" :
                   "every receiver is an index-name (2023 14.9.39.3 rules 3-4)");
        int sk = v.kind == O_REF && !v.ref.rm ? set_kind(v.ref.sym) : SK_OTHER;
        int whole = sk == SK_INT || (v.kind == O_NUM && v.num.scale == 0);
        for (int i = 0; i < nr; i++) {
            int k = set_kind(rs[i].sym);
            if (k == SK_INT && sk != SK_INDEX)
                die_at(rs[i].line, "SET '%s' TO: an integer item is set only from an index-name (%s)", rs[i].sym->name,
                       e85 ? "X3.23-1985 SET general rule 3c" : "2023 14.9.39.3 rule 4");
            if (k == SK_IXD && sk != SK_INDEX && sk != SK_IXD)
                die_at(rs[i].line, "SET '%s' TO: an index data item is set only from an index-name or an index data item (%s)", rs[i].sym->name,
                       e85 ? "X3.23-1985 SET general rule 3b" : "2023 14.9.39.3 rules 2-3");
            if (k == SK_INDEX && e85 && v.kind == O_NUM && (v.num.neg || strspn(v.num.digits, "0") == (size_t)v.num.ndigits))
                die_at(v.line, "SET '%s' TO: the integer is positive (X3.23-1985 SET syntax rule 4)", rs[i].sym->name);
            if (k == SK_INDEX && sk != SK_INDEX && sk != SK_IXD && !whole) {
                if (e85)
                    die_at(v.line, "SET '%s' TO: an index-name is set from an index-name, an index data item, an integer item or an integer (X3.23-1985 SET syntax rules 2 and 4)", rs[i].sym->name);
                check_numeric_opnd(&v);         /* arithmetic-expression-1 (2023 14.9.39 format 1) */
            }
        }
        if (v.kind != O_EXPR) emit_incompat(&v);
        if (allix && sk != SK_INDEX && sk != SK_IXD && !whole) {
            /* arithmetic-expression-1: its value once, then each index
             * (2023 14.9.39.4 rule 2) */
            int Lskip = new_label();
            emit_set_value(&v, Lskip);
            for (int i = 0; i < nr; i++) {
                recv_calls(&rs[i]);
                emit_ref_addr(&rs[i], "r3");
                emit("\tldw r1, sp+%d", SLOT_A);
                emit("\tstw r3+0, r1");
            }
            emit_label(Lskip);
            return;
        }
        for (int i = 0; i < nr; i++) { recv_calls(&rs[i]); emit_move(&v, &rs[i]); }
        return;
    }
    int down = 0;
    if (accept_word("up")) down = 0; else if (accept_word("down")) down = 1;
    else die_at(cur()->line, "expected TO, UP BY or DOWN BY in SET");
    expect_word("by");
    int e85 = g_std < 2002;
    for (int i = 0; i < nr; i++)
        if (set_kind(rs[i].sym) != SK_INDEX)
            die_at(rs[i].line, "SET '%s' UP BY or DOWN BY: the receiver is an index-name (%s)", rs[i].sym->name,
                   e85 ? "X3.23-1985 SET format 2" : "2023 14.9.39 format 2");
    Opnd v;
    if (set_at_expr(e85)) v = expr_opnd();
    else parse_operand(&v);
    if (at_arith_op()) die_at(v.line, "SET ... UP BY or DOWN BY an arithmetic expression is COBOL 2002; compile with -std=2002");
    check_numeric_opnd(&v);
    int whole = (v.kind == O_REF && !v.ref.rm && set_kind(v.ref.sym) == SK_INT) || (v.kind == O_NUM && v.num.scale == 0);
    if (!whole && e85)
        die_at(v.line, "SET ... UP BY or DOWN BY: an integer item or an integer (X3.23-1985 SET syntax rule 3)");
    if (!whole) {
        /* arithmetic-expression-2: its value once, then each index
         * (2023 14.9.39.4 rules 3-4) */
        int Lskip = new_label();
        emit_set_value(&v, Lskip);
        for (int i = 0; i < nr; i++) {
            recv_calls(&rs[i]);
            emit_ref_addr(&rs[i], "r3");
            emit("\tldw r2, r3+0");
            emit("\tldw r1, sp+%d", SLOT_A);
            emit("\t%s r2, r2, r1", down ? "sub" : "add");
            emit("\tstw r3+0, r2");
        }
        emit_label(Lskip);
        return;
    }
    emit_incompat(&v); emit_incompat_refs(rs, nr);      /* the receivers are summed too */
    for (int i = 0; i < nr; i++) {
        recv_calls(&rs[i]);
        Opnd ops[1] = { v };
        int hot = opnd_hot_int(&v) && ref_hot_store(&rs[i], down, ops_all_nonneg(ops, 1));
        int rd[1] = { 0 };
        if (hot) emit_hot_sum(ops, 1); else emit_push(&v);
        emit_store_receivers(&rs[i], rd, 1, hot, 0, down, 0, ops_sum_mag(ops, 1), ops_all_nonneg(ops, 1));
    }
}
