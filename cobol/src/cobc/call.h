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
typedef struct { char name[64], ext[64]; int parent, outer, common, recursive, func, proto; } ProgNode;   /* ext: its AS literal; proto: IS PROTOTYPE */
static ProgNode g_pnode[4096]; static int g_npnode;
static int unit_is_contained(int unit) { return unit < g_npnode && g_pnode[unit].parent >= 0; }
static const char *unit_outer_name(int unit) { return unit < g_npnode ? g_pnode[g_pnode[unit].outer].name : g_progid; }
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
        str_fold(n->name);
        for (k++; k < g_ntok && g_tok[k].kind != T_PERIOD; k++) {
            if (is_word(&g_tok[k], "common")) n->common = 1;
            if (is_word(&g_tok[k], "recursive")) n->recursive = 1;
            if (is_word(&g_tok[k], "prototype")) n->proto = 1;
            if (is_word(&g_tok[k], "as") && k + 1 < g_ntok && g_tok[k + 1].kind == T_STR) {
                snprintf(n->ext, sizeof n->ext, "%.*s", g_tok[k + 1].len > 63 ? 63 : g_tok[k + 1].len, g_tok[k + 1].s);
                str_fold(n->ext);
            }
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
        if (g_pnode[t].parent >= 0 && g_pnode[t].outer == g_pnode[c].outer && !g_pnode[t].proto &&
            (!strcmp(g_pnode[t].name, name) || !strcmp(g_pnode[t].ext, name))) return t;
    return -1;
}

static const char *link_name(const char *name)
{
    static char b[128];
    int n = 0;
    for (const char *p = name; *p && n < 116; p++) {
        if (isalnum((unsigned char)*p) || *p == '_') b[n++] = *p;
        else if ((unsigned char)*p & 0x80) n += snprintf(b + n, sizeof b - (size_t)n, "_%02x", (unsigned char)*p);   /* an extended letter's bytes, distinct */
        else b[n++] = '_';
    }
    b[n] = 0;
    return b;
}

static void parse_call(void)
{
    int line = cur()->line;
    Tok *t = cur();
    char name[128]; Ref target; int dynamic = 0, sig = -1, nested_as = 0;
    int is_proto_name = t->kind == T_WORD && g_std >= 2002 && !sym_lookup_quiet(t->s) && repo_pg_find(t->s) >= 0;
    if (is_proto_name) {
        /* CALL program-prototype-name (14.9.4 format 2): the program
         * externalized under the name the REPOSITORY gives it, its
         * arguments checked and converted against its signature */
        snprintf(name, sizeof name, "%s", pg_extname(t->s));
        str_fold(name);
        sig = pgsig_find(t->s);
        advance();
    } else if (t->kind == T_STR) {
        no_zero_tok(t, "CALL", "2023 14.9.4.3 rule 2");
        snprintf(name, sizeof name, "%.*s", t->len > 120 ? 120 : t->len, t->s);
        str_fold(name);
        advance();
    } else if (t->kind == T_WORD) {
        /* CALL identifier: the item names the program; resolved at run
         * time against the registry every unit joins at start-up */
        parse_ref(&target); dynamic = 1;
        if (target.sym->is_cond) die_at(line, "CALL: a condition-name cannot name a program");
        if (!target.sym->is_group && target.sym->usage == U_POINTER) {
            /* CALL program-pointer: the entry it holds; NULL raises
             * EC-PROGRAM-PTR-NULL (14.9.4.4 rule 3b); restricted to a
             * prototype, its signature checks the arguments */
            if (target.sym->uvar != UV_PPTR) die_at(line, "CALL '%s': a data-pointer does not name a program; a program-pointer does (2023 14.9.4)", target.sym->name);
            dynamic = 3;
            if (target.sym->ptr_proto[0]) sig = pgsig_find(target.sym->ptr_proto);
        }
    } else die_at(line, "expected a program-name literal or an identifier after CALL");
    if (!is_proto_name && g_std >= 2002 && accept_word("as")) {
        /* AS NESTED: the literal names a program in scope here (14.9.4.3
         * rule 15), its signature known when it was defined earlier in
         * the group; AS program-prototype-name: the signature the
         * arguments are checked against (rule 16) */
        if (accept_word("nested")) {
            if (dynamic) die_at(line, "CALL ... AS NESTED names the program with a literal (2023 14.9.4.3 rule 15)");
            if (g_is_function) die_at(line, "the NESTED phrase is a program definition's (2023 14.9.4.3 rule 13)");
            if (pnode_find(g_unit, name) < 0) die_at(line, "CALL \"%s\" AS NESTED: no program of that name is contained in, or common to, this one (2023 14.9.4.3 rule 15)", name);
            nested_as = 1;
            for (int i = 0; i < g_npgsig; i++) if (!strcmp(g_pgsig[i].name, name) || !strcmp(g_pgsig[i].ext, name)) sig = i;
        } else {
            if (cur()->kind != T_WORD || repo_pg_find(cur()->s) < 0)
                die_at(cur()->line, "CALL ... AS takes NESTED or a program-prototype-name of the REPOSITORY (2023 14.9.4.3 rule 16)");
            sig = pgsig_find(cur()->s);
            if (sig < 0) die_at(cur()->line, "no signature for the program prototype '%s'", cur()->s);
            advance();
        }
    }
    FnSig *ps = sig >= 0 ? &g_pgsig[sig] : NULL;
    int omitted[16] = { 0 };
    if (!dynamic && g_dialect_gnu && !strcmp(name, "c$justify")) {
        /* CALL "C$JUSTIFY" USING item ["L"|"R"|"C"] (BP-G7): ACUCOBOL's
         * routine, which GnuCOBOL carries -- the item's text moved to
         * the left, the right (the default) or the centre.  It needs the
         * item's length, which a plain call does not pass, so it is done
         * here; without -dialect=gnucobol it is an ordinary CALL. */
        bp(BP_G7_C_JUSTIFY, line);
        expect_word("using");
        Opnd o; parse_operand(&o);
        if (o.kind != O_REF) die_at(line, "C$JUSTIFY takes an alphanumeric item");
        int mode = 'R';
        if (cur()->kind == T_STR) { mode = toupper((unsigned char)cur()->s[0]); advance(); }
        if (mode != 'L' && mode != 'R' && mode != 'C') die_at(line, "C$JUSTIFY takes \"L\", \"R\" or \"C\"");
        accept_word("end-call");
        Arg ja[3] = { arg_ref(&o.ref), arg_len(&o), arg_imm(mode) };
        emit_args(ja, 3); emit_call("cob_c_justify");
        return;
    }
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
                omitted[n] = 1;
                a[n++] = arg_imm(0);
                continue;
            }
            if (!at_operand()) break;
            if (n >= 16) die_at(cur()->line, "more than 16 CALL arguments (an implementation limit)");
            int ostart = g_tp;
            g_call_byref = mode == 0;               /* BY REFERENCE: a variable-length group goes by its address, slot and all */
            parse_operand(&ops[n]);
            g_call_byref = 0;
            Opnd *o = &ops[n];
            if (ps && at_arith_op()) ops[n] = expr_opnd_after(o, ostart);   /* format 2: an expression, BY CONTENT (implied) or BY VALUE */
            if (ps && n < ps->nparam && mode != 2 && !ps->byval[n] && !ps->param[n].group && ps->param[n].size != -1 &&
                (mode == 1 || !(o->kind == O_REF && !o->ref.rm))) {
                /* through a signature, BY CONTENT (or a literal or
                 * expression) goes into a copy described as the parameter
                 * is, converted as COMPUTE or MOVE would (14.8.2.3.3 rule
                 * 2), and that copy's address is passed */
                if (o->kind == O_ADDR) die_at(o->line, "ADDRESS OF is passed BY REFERENCE or BY VALUE");
                Sym *c = ftemp_new(&ps->param[n], o->line);
                Ref cr = ftemp_ref(c, o->line);
                if (o->kind == O_EXPR) {
                    int rd[1] = { 0 };
                    emit_expr(o->ex);
                    emit_store_receivers(&cr, rd, 1, 0, 1, 0, 0, -1, 0);
                } else {
                    if (o->kind == O_REF && o->ref.sym->is_cond) die_at(o->line, "a condition-name cannot be passed");
                    emit_move(o, &cr);
                }
                memset(o, 0, sizeof *o); o->kind = O_REF; o->ref = cr; o->line = cr.line;
                a[n++] = arg_ref(&o->ref);
                continue;
            }
            if (ps && mode == 2 && o->kind == O_EXPR) {
                /* BY VALUE of an expression: its integer value */
                FDesc fd; memset(&fd, 0, sizeof fd); fd.size = 4; fd.usage = U_SINT; snprintf(fd.pic, sizeof fd.pic, "-");
                Sym *c = ftemp_new(&fd, o->line);
                Ref cr = ftemp_ref(c, o->line);
                int rd[1] = { 0 };
                emit_expr(o->ex);
                emit_store_receivers(&cr, rd, 1, 0, 1, 0, 0, -1, 0);
                memset(o, 0, sizeof *o); o->kind = O_REF; o->ref = cr; o->line = cr.line;
                a[n++] = arg_value(o);
                continue;
            }
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
                if (o->kind == O_REF && o->ref.rm && o->ref.rm_bit) {
                    /* a part of a bit item: its bits moved to a boolean
                     * record on a byte boundary, which is the copy passed */
                    if (!o->ref.rm_len) die_at(o->line, "BY CONTENT of a bit item's part of computed length is not implemented");
                    FDesc fd; memset(&fd, 0, sizeof fd); fd.usage = U_BIT; fd.has_pic = 1; snprintf(fd.pic, sizeof fd.pic, "1(%ld)", o->ref.rm_len);
                    fd.size = (int)((o->ref.rm_len + 7) / 8);
                    Sym *c = ftemp_new(&fd, o->line);
                    Ref cr = ftemp_ref(c, o->line);
                    emit_move(o, &cr);
                    memset(o, 0, sizeof *o); o->kind = O_REF; o->ref = cr; o->line = cr.line;
                    a[n++] = arg_ref(&o->ref);
                    continue;
                }
                if (!(o->kind == O_REF || o->kind == O_STR || o->kind == O_NUM)) die_at(o->line, "a CALL argument must be an item or a literal");
                a[n] = arg_content(o); ncontent++;
            } else if (mode == 2) {
                if (o->kind == O_REF) {
                    if (o->ref.sym->usage == U_FLOAT || o->ref.sym->usage == U_DFLOAT) die_at(o->line, "BY VALUE '%s': a floating-point item is not passed by value (Micro Focus: CALL rules)", o->ref.sym->name);
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
                    cen_flag(o->ref.sym, CEN_CALL);
                }
                else if (o->kind == O_STR) a[n] = arg_label(lit_label((unsigned char *)o->tok->s, o->tok->len));
                else if (o->kind == O_NUM) a[n] = arg_label(call_num_lit_label(&o->num));
                else die_at(o->line, "a CALL argument must be an item or a literal");
            }
            n++;
        }
    }
    if (ps) {
        /* against the signature (14.8.2): as many arguments as parameters
         * but for trailing OPTIONAL ones; OMITTED for an OPTIONAL one; BY
         * VALUE on both sides (14.9.4.3 rule 21); a BY REFERENCE argument
         * described as the parameter is (14.8.2.3.2 rule 2), a group one
         * at least as long (14.8.2.2 rule 1) */
        int need = ps->nparam;
        while (need > 0 && ps->opt[need - 1]) need--;
        if (n > ps->nparam || n < need)
            die_at(line, "the program '%s' takes %d argument%s%s, not %d", name, ps->nparam, ps->nparam == 1 ? "" : "s",
                   need < ps->nparam ? " (the last ones OPTIONAL)" : "", n);
        for (int k = 0; k < n; k++) {
            if (omitted[k]) {
                if (!ps->opt[k]) die_at(line, "argument %d of '%s' is OMITTED, but the parameter is not OPTIONAL (2023 14.9.4.3 rule 24)", k + 1, name);
                continue;
            }
            if ((a[k].kind == A_VALUE || (a[k].kind == A_IMM)) != (ps->byval[k] != 0))
                die_at(line, "argument %d of '%s' is BY %s, the parameter BY %s (2023 14.9.4.3 rules 19, 21)", k + 1, name,
                       a[k].kind == A_VALUE || a[k].kind == A_IMM ? "VALUE" : "REFERENCE or CONTENT", ps->byval[k] ? "VALUE" : "REFERENCE");
            if (a[k].kind != A_REF || ps->byval[k] || ps->param[k].size == -1) continue;
            const Ref *r = a[k].ref;
            if (r->sym->is_ftemp || r->rm) continue;
            FDesc ad; fdesc_of(&ad, r->sym);
            if (ps->param[k].group || ad.group) {
                if (!(ps->param[k].group || ps->param[k].usage == U_DISPLAY) || ps->param[k].size > ad.size)
                    die_at(line, "argument %d of '%s': the parameter is a group of %d bytes, longer than the %d of '%s' (2023 14.8.2.2 rule 1)",
                           k + 1, name, ps->param[k].size, ad.size, r->sym->name);
            } else if (!fdesc_match(&ad, &ps->param[k]))
                die_at(line, "argument %d of '%s' must be described as the parameter is (PICTURE %s, %d bytes; 2023 14.8.2.3.2 rule 2), or go BY CONTENT",
                       k + 1, name, ps->param[k].pic, ps->param[k].size);
        }
    }
    Ref ret; int has_ret = 0;
    if (accept_word("returning") || accept_word("giving")) {
        if (g_std < 2002) bp(BP_E9_CALL_VALUE, cur()->line);
        parse_ref(&ret); has_ret = 1; cen_flag(ret.sym, CEN_CALL); no_constrec_recv(&ret, "CALL RETURNING");
        if (ps && !g_is_function) {
            /* the returning item as the signature describes it (14.8.3) */
            FDesc rd; fdesc_of(&rd, ret.sym);
            if (!ps->ret.size) die_at(ret.line, "RETURNING: the program '%s' has no RETURNING item (2023 14.8.3)", name);
            if (!fdesc_match(&rd, &ps->ret)) die_at(ret.line, "RETURNING '%s' is not described as the program's returning item is (2023 14.8.3)", ret.sym->name);
        }
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
    if (at_word("overflow") || (at_word("on") && is_word(peek(1), "overflow"))) bp(BP_R2_CALL_ON_OVERFLOW, cur()->line);
    int ecnf = !on_phrase && ec_on_name(dynamic == 3 ? "EC-PROGRAM-PTR-NULL" : "EC-PROGRAM-NOT-FOUND");
    int Lcall = new_label(), Lafter = new_label();
    char vis[32]; snprintf(vis, sizeof vis, ".Lvis%d", g_unit);
    if (dynamic == 3) {
        /* the pointer's value; NULL: EC-PROGRAM-PTR-NULL when checked, the
         * ON EXCEPTION phrase when written, else the run stops */
        emit_ref_addr(&target, "r3"); emit("\tldw r12, r3+0");
        emit("\tbne r12, r0, .L%d", Lcall);
        if (ecnf) emit_ec_raise(ec_find("EC-PROGRAM-PTR-NULL", 0));
        if (has_clause) { emit_li("r1", 1); emit("\tstw sp+%d, r1", SLOT_C); emit_jump(Lafter); }
        else { emit_la("r3", lit_label((const unsigned char *)target.sym->name, (int)strlen(target.sym->name) + 1)); emit_call("cob_call_null_ptr"); }
        emit_label(Lcall);
    } else if (dynamic || has_clause || ecnf) {
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
    if (ec_on_name("EC-PROGRAM-RECURSIVE-CALL") && dynamic != 3) {
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
    (void)nested_as;
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
    if (g_std >= 2002) emit_ec_propagated();          /* a condition the program handed back (14.9.18.4 rule 1b) */
    if (ecnf && !has_clause) emit_label(Lafter);    /* reached only past a raise that returned, which a fatal one never does */
    if (has_clause) {
        emit("\tstw sp+%d, r0", SLOT_C);
        emit_label(Lafter);
        Phrases ph; memset(&ph, 0, sizeof ph);
        if (at_word("on") || at_word("exception") || at_word("overflow")) {
            accept_word("on");
            if (!accept_word("exception") && !accept_word("overflow")) die_at(cur()->line, "expected EXCEPTION or OVERFLOW after ON");
            ph.has_on = 1; ph.on = parse_block();
        }
        if (at_word("not")) {
            advance(); accept_word("on");
            if (!accept_word("exception") && !accept_word("overflow")) die_at(cur()->line, "expected EXCEPTION or OVERFLOW after NOT");
            ph.has_not = 1; ph.not_on = parse_block();
        }
        emit_phrases(&ph, SLOT_C, 0);
    }
    accept_word("end-call");
}
