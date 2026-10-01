/* s32-cobc: CALL, program-name scope.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- CALL -------------------------------------------------------------- */

/* a PROGRAM-ID or CALL literal as a linker symbol: the SLOW-32 C ABI's
 * name space, shared with C and Fortran (docs/lowering.md) */
/* ---- the scope of program-names (2023 8.4.6.3; X3.23-1985 X-6) --------
 * Every program unit of the source, numbered as the units are (source
 * order, contained ones included), with its container and its COMMON and
 * RECURSIVE attributes, from a scan of the tokens before anything is
 * compiled: a containing program's CALLs are parsed before the programs
 * it contains.  A contained program's entry is a local symbol, .Lcp<n>,
 * so only a CALL the scope rules let see it links to it; any other CALL of
 * that name means an outermost program of the name (rule 3), which is not
 * here -- the run unit's registry is asked, and has none. */
typedef struct { char name[64]; int parent, outer, common, recursive, func; } ProgNode;
static ProgNode g_pnode[4096]; static int g_npnode;
static int g_any_nested;            /* any contained program in this source: scope tables wanted */
static void prog_tree_scan(void)
{
    int stack[64], sp = 0;
    for (int i = 0; i < g_ntok && g_npnode < 4096; i++) {
        Tok *t = &g_tok[i];
        if (t->kind != T_WORD) continue;
        if (!strcmp(t->s, "end") && i + 1 < g_ntok && (is_word(&g_tok[i + 1], "program") || is_word(&g_tok[i + 1], "function"))) {
            if (sp) sp--;
            continue;
        }
        if (strcmp(t->s, "program-id") && strcmp(t->s, "function-id")) continue;
        ProgNode *n = &g_pnode[g_npnode];
        memset(n, 0, sizeof *n);
        n->func = t->s[0] == 'f';
        n->parent = sp ? stack[sp - 1] : -1;
        n->outer = sp ? g_pnode[stack[0]].outer : g_npnode;
        int k = i + 1;
        if (k < g_ntok && g_tok[k].kind == T_PERIOD) k++;
        if (k < g_ntok && (g_tok[k].kind == T_WORD || g_tok[k].kind == T_STR))
            snprintf(n->name, sizeof n->name, "%.*s", g_tok[k].len > 63 ? 63 : g_tok[k].len, g_tok[k].s);
        for (char *c = n->name; *c; c++) *c = (char)tolower((unsigned char)*c);
        for (k++; k < g_ntok && g_tok[k].kind != T_PERIOD; k++) {
            if (is_word(&g_tok[k], "common")) n->common = 1;
            if (is_word(&g_tok[k], "recursive")) n->recursive = 1;
        }
        /* a function is recursive; so is a program contained in a recursive one (2023 11.10.4 rule 4) */
        if (n->func || (n->parent >= 0 && g_pnode[n->parent].recursive)) n->recursive = 1;
        if (sp < 64) stack[sp++] = g_npnode;
        if (n->parent >= 0) g_any_nested = 1;
        g_npnode++;
    }
}
static int pnode_within(int c, int a)          /* c is a, or contained in a, directly or not */
{
    for (; c >= 0; c = g_pnode[c].parent) if (c == a) return 1;
    return 0;
}
/* may program c's statements reference contained program t by name
 * (2023 8.4.6.3 rules 1-2)? */
static int pnode_visible(int c, int t)
{
    int p = g_pnode[t].parent;
    if (p < 0) return 1;
    if (!g_pnode[t].common) return c == p || (c == t && g_pnode[t].recursive);
    if (!pnode_within(c, p)) return 0;
    if (pnode_within(c, t)) return g_pnode[t].recursive;
    return 1;
}
/* the contained program of that name in c's outermost program, or -1 */
static int pnode_find(int c, const char *name)
{
    if (c < 0 || c >= g_npnode) return -1;
    for (int t = 0; t < g_npnode; t++)
        if (g_pnode[t].parent >= 0 && g_pnode[t].outer == g_pnode[c].outer && !strcmp(g_pnode[t].name, name)) return t;
    return -1;
}

static const char *link_name(const char *name)
{
    static char b[128];
    int n = 0;
    for (const char *p = name; *p && n < 120; p++) b[n++] = (isalnum((unsigned char)*p) || *p == '_') ? *p : '_';
    b[n] = 0;
    return b;
}

static void parse_call(void)
{
    int line = cur()->line;
    Tok *t = cur();
    char name[128]; Ref target; int dynamic = 0;
    if (t->kind == T_STR) {
        snprintf(name, sizeof name, "%.*s", t->len > 120 ? 120 : t->len, t->s);
        for (char *k = name; *k; k++) *k = (char)tolower((unsigned char)*k);
        advance();
    } else if (t->kind == T_WORD) {
        /* CALL identifier: the item names the program; resolved at run
         * time against the registry every unit joins at start-up */
        parse_ref(&target); dynamic = 1;
        if (target.sym->is_cond) die_at(line, "CALL: a condition-name cannot name a program");
    } else die_at(line, "expected a program-name literal or an identifier after CALL");
    /* a contained program of that name: its own entry when in scope; out
     * of scope, the name is an outermost program's, found (or not) at run
     * time like an identifier's (2023 8.4.6.3) */
    int cpn = dynamic ? -1 : pnode_find(g_unit, name), cp_direct = 0;
    if (cpn >= 0) { if (pnode_visible(g_unit, cpn)) cp_direct = 1; else dynamic = 2; }
    Arg a[16]; Opnd ops[16]; int n = 0, ncontent = 0;
    if (accept_word("using")) {
        int mode = 0;               /* 0 reference, 1 content, 2 value */
        for (;;) {
            if (accept_word("by")) {
                if (accept_word("reference")) mode = 0;
                else if (accept_word("content")) mode = 1;
                else if (accept_word("value")) { mode = 2; if (g_std < 2002) bp(BP_E9_CALL_VALUE, cur()->line); }
                else die_at(cur()->line, "expected REFERENCE, CONTENT or VALUE after BY");
                continue;
            }
            if (accept_word("reference")) { mode = 0; continue; }
            if (accept_word("value")) { mode = 2; if (g_std < 2002) bp(BP_E9_CALL_VALUE, cur()->line); continue; }
            if (accept_word("content")) { mode = 1; continue; }
            if (g_std >= 2002 && at_word("omitted")) {
                /* OMITTED: no argument, a NULL address (2023 14.9.4.2) */
                if (n >= 16) die_at(cur()->line, "more than 16 CALL arguments (an implementation limit)");
                if (mode == 2) die_at(cur()->line, "OMITTED is a BY REFERENCE argument's (2023 14.9.4.2)");
                advance();
                a[n++] = arg_imm(0);
                continue;
            }
            if (!at_operand()) break;
            if (n >= 16) die_at(cur()->line, "more than 16 CALL arguments (an implementation limit)");
            parse_operand(&ops[n]);
            Opnd *o = &ops[n];
            if (o->kind == O_ADDR) {
                /* the address, a word, BY VALUE; by reference or content,
                 * the unique data item ADDRESS OF creates (2023 8.4.3.11
                 * GR 1): a compiler-made pointer record holding it */
                if (mode == 2) { a[n++] = arg_value(o); continue; }
                FDesc fd; memset(&fd, 0, sizeof fd); fd.size = 4; fd.usage = U_POINTER; snprintf(fd.pic, sizeof fd.pic, "-");
                Sym *t = ftemp_new(&fd, o->line);
                Ref tr = ftemp_ref(t, o->line);
                emit_ptr_value(o, "r1");
                emit("\tstw sp+%d, r1", SLOT_A);
                emit_ref_addr(&tr, "r3");
                emit("\tldw r1, sp+%d", SLOT_A);
                emit("\tstw r3+0, r1");
                memset(o, 0, sizeof *o); o->kind = O_REF; o->ref = tr; o->line = tr.line;
                if (mode == 1) { a[n] = arg_content(o); ncontent++; } else a[n] = arg_ref(&o->ref);
                n++;
                continue;
            }
            if (mode == 1) {
                if (o->kind == O_REF && o->ref.sym->is_cond) die_at(o->line, "a condition-name cannot be passed");
                if (o->kind == O_REF && o->ref.rm && o->ref.rm_bit) die_at(o->line, "BY CONTENT of a reference-modified bit item is not implemented (its bits would need moving to a byte)");
                if (!(o->kind == O_REF || o->kind == O_STR || o->kind == O_NUM)) die_at(o->line, "a CALL argument must be an item or a literal");
                a[n] = arg_content(o); ncontent++;
            } else if (mode == 2) {
                if (o->kind == O_REF) {
                    if (o->ref.sym->usage == U_FLOAT) die_at(o->line, "BY VALUE '%s': a floating-point item is not passed by value (Micro Focus: CALL rules)", o->ref.sym->name);
                    if (!is_int_item(o->ref.sym)) die_at(o->line, "BY VALUE '%s' must be an integer item", o->ref.sym->name);
                    if (o->ref.sym->size > 4) die_at(o->line, "BY VALUE '%s': only items up to four bytes (a word) are passed by value", o->ref.sym->name);
                    a[n] = arg_value(o);
                } else if (o->kind == O_NUM) {
                    if (!numlit_is_int(&o->num)) die_at(o->line, "BY VALUE needs an integer");
                    a[n] = arg_imm((long)numlit_int(&o->num));
                } else die_at(o->line, "BY VALUE needs an integer item or literal");
            } else {
                if (o->kind == O_REF) {
                    if (o->ref.sym->is_cond) die_at(o->line, "a condition-name cannot be passed");
                    if (sym_bitlike(o->ref.sym)) bit_arg_check(&o->ref);
                    a[n] = arg_ref(&o->ref);
                }
                else if (o->kind == O_STR) a[n] = arg_label(lit_label((unsigned char *)o->tok->s, o->tok->len));
                else if (o->kind == O_NUM) a[n] = arg_label(call_num_lit_label(&o->num));
                else die_at(o->line, "a CALL argument must be an item or a literal");
            }
            n++;
        }
    }
    Ref ret; int has_ret = 0;
    if (accept_word("returning") || accept_word("giving")) {
        if (g_std < 2002) bp(BP_E9_CALL_VALUE, cur()->line);
        parse_ref(&ret); has_ret = 1;
        if (ret.sym->is_cond) die_at(ret.line, "RETURNING '%s': a condition-name receives nothing", ret.sym->name);
        if (g_std < 2002 && !is_int_item(ret.sym)) die_at(ret.line, "RETURNING '%s' must be an integer item (the C ABI returns a word)", ret.sym->name);
    }
    /* [ON] EXCEPTION|OVERFLOW ... [NOT [ON] EXCEPTION|OVERFLOW ...]: the
     * exception is the program not being in this executable.  A literal
     * CALL with the clause goes through the registry too, so the link
     * does not demand the program; without it, the linker resolves it. */
    int has_clause = at_word("on") || at_word("exception") || at_word("overflow") ||
                     (at_word("not") && (is_word(peek(1), "on") || is_word(peek(1), "exception") || is_word(peek(1), "overflow")));
    /* EC-PROGRAM-NOT-FOUND (2023 14.9.4 general rule 3b; cobol ISSUES-59):
     * with checking on and no ON EXCEPTION phrase, the CALL resolves at run
     * time, and a missing program raises the condition (fatal) */
    int on_phrase = at_word("on") || at_word("exception") || at_word("overflow");
    int ecnf = !on_phrase && ec_on_name("EC-PROGRAM-NOT-FOUND");
    int Lcall = new_label(), Lafter = new_label();
    char vis[32]; snprintf(vis, sizeof vis, ".Lvis%d", g_unit);
    if (dynamic || has_clause || ecnf) {
        if (dynamic == 1) { emit_ref_addr(&target, "r3"); emit_li("r4", target.sym->size); }
        else { emit_la("r3", lit_label((const unsigned char *)t->s, t->len)); emit_li("r4", t->len); }
        emit_li("r5", !has_clause && !ecnf);            /* no clause: the runtime stops on a missing program */
        if (g_any_nested) { emit_la("r6", vis); emit_call("cob_resolve_v"); }   /* the contained programs in scope here */
        else emit_call("cob_resolve");
        emit("\tadd r12, r0, r1");                      /* callee-saved; the compiler uses no other of r12-r28 */
        if (has_clause || ecnf) {
            emit("\tbne r12, r0, .L%d", Lcall);
            if (ecnf) emit_ec_raise(ec_find("EC-PROGRAM-NOT-FOUND", 0));
            emit_li("r1", 1); emit("\tstw sp+%d, r1", SLOT_C);
            emit_jump(Lafter);
            emit_label(Lcall);
        }
    }
    if (ec_on_name("EC-PROGRAM-RECURSIVE-CALL")) {
        /* the called program active and not RECURSIVE (14.9.4 general rule
         * 3f): known here from its registered descriptor, before the call */
        int Lok = new_label();
        if (dynamic == 1) { emit_ref_addr(&target, "r3"); emit_li("r4", target.sym->size); }
        else { emit_la("r3", lit_label((const unsigned char *)t->s, t->len)); emit_li("r4", t->len); }
        if (g_any_nested) { emit_la("r5", vis); emit_call("cob_program_busy_v"); }
        else emit_call("cob_program_busy");
        emit("\tbeq r1, r0, .L%d", Lok);
        emit_ec_raise(ec_find("EC-PROGRAM-RECURSIVE-CALL", 0));
        emit_label(Lok);
    }
    /* arguments past the eighth go on the stack (the C ABI): each is
     * worked out into a slot first, the first eight into r3-r10, then the
     * slots are copied to an outgoing area at sp+0 for the call */
    /* the returning item's address, for a COBOL program's result */
    int rslot = -1;
    if (g_std >= 2002 && has_ret) { rslot = g_slot_base++; emit_ref_addr(&ret, "r1"); emit("\tstw sp+%d, r1", SLOT(rslot)); }
    /* each argument's length in bytes, for a parameter described ANY
     * LENGTH (2023 13.18.2.4): beside the count, before the argument
     * registers are loaded -- a computed length takes calls */
    if (g_std >= 2002) {
        for (int k = 0; k < n; k++) {
            Arg *x = &a[k]; long len = -2; const Ref *lr = NULL;
            if (x->kind == A_REF) lr = x->ref;
            else if (x->kind == A_CONTENT || x->kind == A_LABEL) {
                Opnd *o = x->kind == A_CONTENT ? x->fn : &ops[k];
                if (o->kind == O_REF) lr = &o->ref;
                else if (o->kind == O_STR) len = o->tok->len;
                else if (o->kind == O_NUM) len = o->num.ndigits;
                else len = 0;
            } else if (x->kind == A_VALUE) len = 4;
            else len = 0;                                   /* OMITTED */
            if (lr) {
                int sl = ref_static_len(lr);
                if (sl > 0) len = sl;
                else { Arg l[1] = { arg_rlen(lr) }; emit_args(l, 1); emit_la("r1", "cob_call_lens"); emit("\tstw r1+%d, r3", 4 * k); continue; }
            }
            emit_la("r1", "cob_call_lens"); emit_li("r2", len); emit("\tstw r1+%d, r2", 4 * k);
        }
        emit_la("r1", "cob_call_nlens"); emit_li("r2", n); emit("\tstw r1+0, r2");
    }
    int nx = n > 8 ? n - 8 : 0, xbase = g_slot_base, out = (nx * 4 + 7) & ~7;
    if (nx) {
        g_slot_base += nx;
        if (g_slot_base + 8 > NSLOTS) die_at(line, "internal: too many staged operands");
        for (int k = 0; k < nx; k++) { emit_args(&a[8 + k], 1); emit("\tstw sp+%d, r3", SLOT(xbase + k)); }
    }
    emit_args(a, n > 8 ? 8 : n);
    /* the count, for a called program's OPTIONAL parameters */
    if (g_std >= 2002) {
        emit_la("r1", "cob_call_nargs"); emit_li("r2", n); emit("\tstw r1+0, r2");
        emit_la("r1", "cob_call_retaddr");
        if (rslot >= 0) emit("\tldw r2, sp+%d", SLOT(rslot)); else emit("\tadd r2, r0, r0");
        emit("\tstw r1+0, r2");
        emit_la("r1", "cob_call_returned"); emit("\tstw r1+0, r0");
    }
    if (nx) {
        emit("\taddi sp, sp, -%d", out);
        for (int k = 0; k < nx; k++) { emit("\tldw r1, sp+%d", out + SLOT(xbase + k)); emit("\tstw sp+%d, r1", 4 * k); }
    }
    if (dynamic || has_clause || ecnf) emit("\tjalr r31, r12, 0");
    else if (cp_direct) emit("\tjal r31, .Lcp%d", cpn);
    else emit("\tjal r31, %s", link_name(name));
    if (nx) { emit("\taddi sp, sp, %d", out); g_slot_base = xbase; }
    if (rslot >= 0) g_slot_base = rslot;
    if (ncontent) {                 /* the BY CONTENT copies go, the result kept */
        emit("\tstw sp+%d, r1", SLOT_C);
        emit_li("r3", ncontent); emit_call("cob_content_pop");
        emit("\tldw r1, sp+%d", SLOT_C);
    }
    if (!has_ret && g_uses_rc) {
        /* RETURN-CODE: what the callee returned, a COBOL program's own
         * RETURN-CODE or a C function's result */
        emit_la("r2", "cob_return_code"); emit("\tstw r2+0, r1");
    }
    int Lcobret = -1;
    if (has_ret && g_std >= 2002) {
        /* a COBOL program put its result in place; a C function left it in r1 */
        Lcobret = new_label();
        emit_la("r2", "cob_call_returned"); emit("\tldw r2, r2+0");
        emit("\tbne r2, r0, .L%d", Lcobret);
    }
    if (has_ret && !is_int_item(ret.sym)) { /* only a COBOL program fills it */ }
    else if (has_ret) {
        if (is_hot_int(ret.sym)) {
            emit("\tstw sp+%d, r1", SLOT_C);
            emit_ref_addr(&ret, "r3");
            emit("\tldw r1, sp+%d", SLOT_C);
            emit_store_int(ret.sym, "r3", "r1");
        } else {
            emit("\tstw sp+%d, r1", SLOT_C);
            Arg b[2] = { arg_ref(&ret), arg_desc(sym_desc(ret.sym)) };
            emit_args(b, 2);
            emit("\tldw r5, sp+%d", SLOT_C);
            emit_call("cob_store_int");
        }
    }
    if (Lcobret >= 0) emit_label(Lcobret);
    if (ecnf && !has_clause) emit_label(Lafter);    /* reached only past a raise that returned, which a fatal one never does */
    if (has_clause) {
        emit("\tstw sp+%d, r0", SLOT_C);
        emit_label(Lafter);
        int Lend = new_label();
        if (at_word("on") || at_word("exception") || at_word("overflow")) {
            accept_word("on");
            if (!accept_word("exception") && !accept_word("overflow")) die_at(cur()->line, "expected EXCEPTION or OVERFLOW after ON");
            int Lnot = new_label();
            emit("\tldw r1, sp+%d", SLOT_C);
            emit("\tbeq r1, r0, .L%d", Lnot);
            parse_statements();
            emit_jump(Lend);
            emit_label(Lnot);
        }
        if (at_word("not")) {
            advance(); accept_word("on");
            if (!accept_word("exception") && !accept_word("overflow")) die_at(cur()->line, "expected EXCEPTION or OVERFLOW after NOT");
            emit("\tldw r1, sp+%d", SLOT_C);
            emit("\tbne r1, r0, .L%d", Lend);
            parse_statements();
        }
        emit_label(Lend);
    }
    accept_word("end-call");
}
