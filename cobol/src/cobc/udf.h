/* s32-cobc: user-defined functions.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- user-defined functions: the caller's side (COBOL 2002; ISSUES-50) -- */

/* The external repository: name.s32fn, written by the function's own
 * compile (-fnsig, or any compile of it), found beside the output, beside
 * the source, or on -I.  One line per item: the RETURNING item, then each
 * parameter -- group size usage has_pic just bwz sign_lead sign_sep pic
 * byval opt.  Version 2 (2026-10-06): the result's address goes in
 * cob_call_retaddr, as a program's RETURNING does, and the parameters
 * take CALL's path -- eight registers, the rest on the stack; a version-1
 * file is from the compiler that passed the result after the arguments,
 * and its function is recompiled. */
static const char *g_outdir = ".";

static void fdesc_of(FDesc *d, const Sym *x)
{
    memset(d, 0, sizeof *d);
    d->group = x->is_group; d->size = x->any_len ? -1 : x->size; d->usage = x->usage | x->uvar << 8; d->has_pic = x->has_pic;   /* the variant in the second byte: COMP-X is not COMP-5 */
    d->just = x->just; d->bwz = x->blank_zero; d->sign_lead = x->sign_lead; d->sign_sep = x->sign_sep;
    snprintf(d->pic, sizeof d->pic, "%s", x->has_pic ? x->pic : "-");
}

/* kind: "fn" a function's, "pg" a program's (the same record) */
static void fnsig_path(char *out, size_t n, const char *dir, const char *name, const char *kind)
{
    snprintf(out, n, "%s/%s.s32%s", dir, link_name(name), kind);
}

static void fnsig_write(const FnSig *f, const char *kind)
{
    char path[1100]; fnsig_path(path, sizeof path, g_outdir, f->ext, kind);
    FILE *o = fopen(path, "w");
    if (!o) { fprintf(stderr, "s32-cobc: cannot write %s\n", path); fail(); }
    fprintf(o, "s32%s 2 %s %s %s %d\n", kind, f->name, f->ext, f->link, f->nparam);
    for (int k = -1; k < f->nparam; k++) {
        const FDesc *d = k < 0 ? &f->ret : &f->param[k];
        fprintf(o, "%d %d %d %d %d %d %d %d %s %d %d\n", d->group, d->size, d->usage, d->has_pic, d->just, d->bwz, d->sign_lead, d->sign_sep, d->pic,
                k < 0 ? 0 : f->byval[k], k < 0 ? 0 : f->opt[k]);
    }
    fclose(o);
}

static int fnsig_read(const char *path, FnSig *f, const char *kind)
{
    FILE *in = fopen(path, "r");
    if (!in) return 0;
    memset(f, 0, sizeof *f);
    int ver = 0; char magic[8];
    int ok = fscanf(in, "%7s %d %63s", magic, &ver, f->name) == 3 && !strcmp(magic + 3, kind);
    if (ok && ver != 2)
        die_at(0, "%s is from an older compiler (version %d): recompile the function '%s' (its source first, as compile.sh does)", path, ver, f->name);
    ok = ok && fscanf(in, "%63s %127s %d", f->ext, f->link, &f->nparam) == 3 && f->nparam >= 0 && f->nparam <= 16;
    for (int k = -1; ok && k < f->nparam; k++) {
        FDesc *d = k < 0 ? &f->ret : &f->param[k];
        int bv = 0, op = 0;
        ok = fscanf(in, "%d %d %d %d %d %d %d %d %255s %d %d", &d->group, &d->size, &d->usage, &d->has_pic, &d->just, &d->bwz,
                    &d->sign_lead, &d->sign_sep, d->pic, &bv, &op) == 11;
        if (k >= 0) { f->byval[k] = (unsigned char)bv; f->opt[k] = (unsigned char)op; }
    }
    fclose(in);
    return ok;
}

/* the externalized name a function is known by (11.5 GR 1, 12.3.8 GR 2):
 * the unit's own AS literal, the REPOSITORY's FUNCTION name AS literal,
 * else the name itself */
static const char *fn_extname(const char *name)
{
    if (g_is_function && !strcmp(name, g_progid) && g_fn_as[0]) return g_fn_as;
    for (int i = 0; i < g_nrepo_fn; i++) if (!strcmp(g_repo_fn[i], name) && g_repo_fn_as[i][0]) return g_repo_fn_as[i];
    return name;
}

/* the function's signature: defined earlier in this source, or from the
 * repository -- by its externalized name (an AS literal's), else by the
 * name itself, which a prototype or definition in this group also
 * answers to (12.3.8.3 rule 10) */
static int fnsig_find(const char *name0)
{
    const char *name = fn_extname(name0);
    for (int i = 0; i < g_nfnsig; i++) if (!strcmp(g_fnsig[i].ext, name) || !strcmp(g_fnsig[i].name, name)) return i;
    char srcdir[1024]; snprintf(srcdir, sizeof srcdir, "%s", g_file);
    char *sl = strrchr(srcdir, '/'); if (sl) *sl = 0; else strcpy(srcdir, ".");
    for (int d = -2; d < g_nincdir; d++) {
        const char *dir = d == -2 ? g_outdir : d == -1 ? srcdir : g_incdirs[d];
        char path[1100]; fnsig_path(path, sizeof path, dir, name, "fn");
        FnSig f;
        if (fnsig_read(path, &f, "fn") && (!strcmp(f.ext, name) || !strcmp(f.name, name))) {
            if (g_nfnsig == 128) die_at(0, "more than 128 user-defined functions");
            g_fnsig[g_nfnsig] = f;
            return g_nfnsig++;
        }
    }
    return -1;
}

/* a program's externalized name: its own AS literal, the REPOSITORY's
 * PROGRAM name AS literal, else the name */
static const char *pg_extname(const char *name)
{
    if (!g_is_function && !strcmp(name, g_progid) && g_prog_as[0]) return g_prog_as;
    for (int i = 0; i < g_nrepo_pg; i++) if (!strcmp(g_repo_pg[i], name) && g_repo_pg_as[i][0]) return g_repo_pg_as[i];
    return name;
}

/* a program's signature (12.3.8.3 rule 14): a prototype or definition
 * in this group, else the external repository's name.s32pg */
static int pgsig_find(const char *name0)
{
    const char *name = pg_extname(name0);
    for (int i = 0; i < g_npgsig; i++) if (!strcmp(g_pgsig[i].ext, name) || !strcmp(g_pgsig[i].name, name)) return i;
    char srcdir[1024]; snprintf(srcdir, sizeof srcdir, "%s", g_file);
    char *sl = strrchr(srcdir, '/'); if (sl) *sl = 0; else strcpy(srcdir, ".");
    for (int d = -2; d < g_nincdir; d++) {
        const char *dir = d == -2 ? g_outdir : d == -1 ? srcdir : g_incdirs[d];
        char path[1100]; fnsig_path(path, sizeof path, dir, name, "pg");
        FnSig f;
        if (fnsig_read(path, &f, "pg") && (!strcmp(f.ext, name) || !strcmp(f.name, name))) {
            if (g_npgsig == 64) die_at(0, "more than 64 program signatures");
            g_pgsig[g_npgsig] = f;
            return g_npgsig++;
        }
    }
    return -1;
}

/* BY REFERENCE conformance (14.8.2.3.2): the same PICTURE, USAGE,
 * JUSTIFIED, BLANK WHEN ZERO and SIGN -- pictures compared as analysed,
 * so S9(4) and s9999 are the same picture */
/* a two's-complement binary integer with no truncation to a digit count:
 * COMP-5 with an integer picture, or a native usage (SIGNED-INT ...).
 * 1 signed, 2 unsigned, 0 neither */
static int fdesc_native_int(const FDesc *d)
{
    if (d->group) return 0;
    switch (d->usage) {
    case U_SINT: case U_SSHORT: case U_BCHAR: case U_SDBL: return 1;
    case U_UINT: case U_USHORT: case U_UBCHAR: case U_UDBL: return 2;
    case U_COMP5: {
        PicInfo pi;
        if (!d->has_pic || pic_analyse(d->pic, &pi) < 0 || pi.scale != 0) return 0;
        return pi.is_signed ? 1 : 2;
    }
    default: return 0;
    }
}

static int fdesc_match(const FDesc *a, const FDesc *b)
{
    /* an implementor extension (docs/behavior-points.md, class E): the same
     * storage under two spellings -- PIC S9(8) COMP-5 and SIGNED-INT -- is
     * taken as conforming, as GnuCOBOL takes it; majesty's holidays passes
     * a SIGNED-INT to floor-divmod's COMP-5 parameter */
    int na = fdesc_native_int(a), nb = fdesc_native_int(b);
    if (na && na == nb && a->size == b->size) return 1;
    if (a->group || b->group) return a->group == b->group && a->size == b->size;
    if (a->size != b->size || a->usage != b->usage || a->has_pic != b->has_pic || a->just != b->just || a->bwz != b->bwz ||
        a->sign_lead != b->sign_lead || a->sign_sep != b->sign_sep) return 0;
    if (!a->has_pic) return 1;
    PicInfo pa, pb;
    if (pic_analyse(a->pic, &pa) < 0 || pic_analyse(b->pic, &pb) < 0) return 0;
    return pa.category == pb.category && pa.digits == pb.digits && pa.scale == pb.scale && pa.is_signed == pb.is_signed &&
           pa.bytes == pb.bytes && !strcmp(pa.pat, pb.pat);
}

/* two prototypes with the same signature (2023 14.9.39.3 rule 20; 14.8.2,
 * 14.8.3): as many parameters, each conforming and passed the same way,
 * OPTIONAL alike, the returning items conforming */
static int fnsig_same(const char *a, const char *b)
{
    if (!strcmp(a, b)) return 1;
    int ia = fnsig_find(a), ib = fnsig_find(b);
    if (ia < 0 || ib < 0) return 0;
    const FnSig *x = &g_fnsig[ia], *y = &g_fnsig[ib];
    if (x->nparam != y->nparam || !fdesc_match(&x->ret, &y->ret)) return 0;
    for (int k = 0; k < x->nparam; k++)
        if (x->byval[k] != y->byval[k] || x->opt[k] != y->opt[k] || !fdesc_match(&x->param[k], &y->param[k])) return 0;
    return 1;
}

/* is this word a user function this unit may invoke without FUNCTION? */
static int ufn_named(const char *w)
{
    if (g_is_function && !strcmp(w, g_progid)) return 1;
    for (int i = 0; i < g_nrepo_fn; i++) if (!strcmp(g_repo_fn[i], w)) return 1;
    return 0;
}

/* a compiler-made record described by d: a function's result, or a BY
 * CONTENT argument.  LOCAL-STORAGE in a program that can be re-entered
 * (the same call site in two activations must not share it), static
 * otherwise. */
static Sym *ftemp_new(const FDesc *d, int line)
{
    static int n;
    Sym *t = sym_new();
    int idx = sym_idx(t);
    t->level = 1; t->line = line; t->is_filler = 1; t->is_ftemp = 1; t->ftemp_scan = g_noemit > 0;
    snprintf(t->name, sizeof t->name, "filler");
    t->usage = d->group ? U_DISPLAY : d->usage & 255; t->uvar = d->group ? UV_NONE : d->usage >> 8; t->has_usage = !d->group;
    t->is_local = g_recursive;
    if (d->group || !d->has_pic) {
        if (d->group) { t->has_pic = 1; snprintf(t->pic, sizeof t->pic, "x(%d)", d->size); }
    } else {
        t->has_pic = 1; snprintf(t->pic, sizeof t->pic, "%s", d->pic);
        t->just = d->just; t->blank_zero = d->bwz; t->sign_lead = d->sign_lead; t->sign_sep = d->sign_sep;
    }
    if (t->has_pic && !bool_picture(t->pic, &t->pi, line) && pic_analyse(t->pic, &t->pi) < 0) die_at(line, "internal: function signature picture '%s': %s", t->pic, t->pi.err);
    sym_finish(t);
    int zero[1] = { 0 };
    layout(idx, 0);
    set_dims(idx, 0, zero, zero);
    t = &g_sym[idx];
    t->record = idx; t->desc_id = -1;
    snprintf(t->label, sizeof t->label, "%s%d_%d", t->is_local ? ".Llft" : "ft", g_unit, n++);
    t->image_size = t->size; t->image = xmalloc((size_t)t->size);
    init_record(t, idx, 1);
    return &g_sym[idx];
}

typedef struct UCall_ { int sig, nargs, line; Opnd arg[16]; int byref[16]; Sym *ctmp[16]; Sym *res; int via_fp; Ref fp; } UCall;   /* byref 2: OMITTED; via_fp: invoked through the function-pointer fp */
static UCall *g_ucall; static int g_ucap;
static void ucall_bind(UCall *u, const char *name);
static void ucall_emit(const UCall *u);

static Ref ftemp_ref(Sym *t, int line)
{
    Ref r; memset(&r, 0, sizeof r); r.sym = t; r.line = line; r.rm_lx = NULL;
    return r;
}

/* the call: arguments by reference or into their content copies, the
 * result's address last; the function fills the result */
static void emit_ucall(const UCall *u0)
{
    /* its own copy: an argument's expression may call a function too, and
     * recording that call grows g_ucall, which u may point into */
    UCall c = *u0, *u = &c;
    FnSig *f = &g_fnsig[u->sig];
    Ref refs[17]; Arg a[17];
    for (int k = 0; k < u->nargs; k++) {
        if (u->byref[k] == 2) { memset(&refs[k], 0, sizeof refs[k]); continue; }
        if (u->byref[k]) { refs[k] = u->arg[k].ref; cen_flag(refs[k].sym, CEN_CALL); continue; }
        refs[k] = ftemp_ref(u->ctmp[k], u->line);
        if (u->arg[k].kind == O_EXPR) {
            int rd[1] = { 0 };
            emit_expr(u->arg[k].ex);
            emit_store_receivers(&refs[k], rd, 1, 0, 1, 0, 0, -1, 0);
        } else emit_move(&u->arg[k], &refs[k]);
    }
    Ref rres = ftemp_ref(u->res, u->line);
    if (u->via_fp) {
        /* through a function-pointer (8.4.3.2.4 rule 6c): its value in r12
         * (callee-saved, as CALL program-pointer keeps it) before the
         * arguments are staged; NULL is EC-FUNCTION-PTR-NULL when checked,
         * else the run stops */
        int Lgo = new_label();
        emit_ref_addr(&u->fp, "r3"); emit("\tldw r12, r3+0");
        emit("\tbne r12, r0, .L%d", Lgo);
        if (ec_on_name("EC-FUNCTION-PTR-NULL")) emit_ec_raise(ec_find("EC-FUNCTION-PTR-NULL", 0));
        emit_la("r3", lit_label((const unsigned char *)u->fp.sym->name, (int)strlen(u->fp.sym->name) + 1)); emit_call("cob_fn_null_ptr");
        emit_label(Lgo);
    }
    int anyl = 0;
    for (int k = 0; k < u->nargs; k++) anyl |= f->param[k].size == -1;
    if (anyl) {
        /* the arguments' lengths, for the ANY LENGTH parameters (as CALL) */
        for (int k = 0; k < u->nargs; k++) {
            if (u->byref[k] == 2) { emit_la("r1", "cob_call_lens"); emit("\tstw r1+%d, r0", 4 * k); continue; }
            int sl = ref_static_len(&refs[k]);
            if (sl > 0) { emit_la("r1", "cob_call_lens"); emit_li("r2", sl); emit("\tstw r1+%d, r2", 4 * k); }
            else { Arg l[1] = { arg_rlen(&refs[k]) }; emit_args(l, 1); emit_la("r1", "cob_call_lens"); emit("\tstw r1+%d, r3", 4 * k); }
        }
        emit_la("r1", "cob_call_nlens"); emit_li("r2", u->nargs); emit("\tstw r1+0, r2");
    }
    for (int k = 0; k < u->nargs; k++) a[k] = u->byref[k] == 2 ? arg_imm(0) : arg_ref(&refs[k]);
    /* as CALL stages them: eight in registers, the rest on the stack
     * above the callee's frame; the count and the result's address in
     * the cells a program's CALL fills (the callee reads them at entry) */
    int n = u->nargs, nx = n > 8 ? n - 8 : 0, xbase = g_slot_base, out = (nx * 4 + 7) & ~7;
    emit_la("r1", "cob_call_nargs"); emit_li("r2", n); emit("\tstw r1+0, r2");
    emit_ref_addr(&rres, "r2");
    emit_la("r1", "cob_call_retaddr"); emit("\tstw r1+0, r2");
    if (nx) {
        g_slot_base += nx;
        if (g_slot_base + 8 > NSLOTS) die_at(u->line, "internal: too many staged operands");
        for (int k = 0; k < nx; k++) { emit_args(&a[8 + k], 1); emit("\tstw sp+%d, r3", SLOT(xbase + k)); }
    }
    emit_args(a, n > 8 ? 8 : n);
    if (nx) {
        emit("\taddi sp, sp, -%d", out);
        for (int k = 0; k < nx; k++) { emit("\tldw r1, sp+%d", out + SLOT(xbase + k)); emit("\tstw sp+%d, r1", 4 * k); }
    }
    if (u->via_fp) emit("\tjalr r31, r12, 0"); else emit_call(f->link);
    if (nx) { emit("\taddi sp, sp, %d", out); g_slot_base = xbase; }
    if (g_std >= 2002) emit_ec_propagated();          /* a condition the function handed back (GOBACK RAISING; 14.9.18.4 rule 1b) */
}

static void emit_ucalls(int from, int to)
{
    for (int k = from; k < to; k++) emit_ucall(&g_ucall[k]);
}

/* name(args), the cursor past the name: the operand becomes the result */
static void parse_ufunc_1(Opnd *o, const char *name, int line, const Ref *fp);
static void parse_ufunc(Opnd *o, const char *name, int line) { parse_ufunc_1(o, name, line, NULL); }
/* function-pointer-name (arguments) (2014; 2023 8.4.3.2): the function
 * the pointer holds, with the signature of the prototype the pointer is
 * restricted to; the parentheses are required (rule 5) */
static void parse_ufunc_fp(Opnd *o, const Ref *fp, int line)
{
    if (cur()->kind != T_LP) die_at(line, "'%s': a function-pointer is invoked with its arguments in parentheses, () with none (2023 8.4.3.2.3 rule 5)", fp->sym->name);
    parse_ufunc_1(o, fp->sym->ptr_proto, line, fp);
}
static void parse_ufunc_1(Opnd *o, const char *name, int line, const Ref *fp)
{
    int sig = fnsig_find(name);
    if (sig < 0)
        die_at(line, "no signature for the function '%s': define it earlier in this source, or compile its own source "
                     "first (compile.sh does), so its %s.s32fn is beside the output or on -I", name, link_name(name));
    UCall u; memset(&u, 0, sizeof u);
    u.sig = sig; u.line = line;
    if (fp) { u.via_fp = 1; u.fp = *fp; }
    if (cur()->kind == T_LP) {
        advance();
        while (cur()->kind != T_RP) {
            if (cur()->kind == T_EOF) die_at(line, "expected ')' after the arguments of '%s'", name);
            if (u.nargs == 16) die_at(line, "'%s': more than sixteen arguments (an implementation limit)", name);
            if (at_word("omitted")) {
                /* OMITTED: no argument, a NULL address, for an OPTIONAL parameter (14.8.2.1) */
                memset(&u.arg[u.nargs], 0, sizeof u.arg[0]); u.arg[u.nargs].line = cur()->line;
                u.byref[u.nargs++] = 2;
                advance();
                continue;
            }
            int start = g_tp;
            Opnd a; parse_operand(&a);
            if (at_arith_op()) a = expr_opnd_after(&a, start);
            u.arg[u.nargs++] = a;
        }
        advance();
    }
    ucall_bind(&u, name);
    memset(o, 0, sizeof *o);
    o->kind = O_REF; o->ref = ftemp_ref(u.res, line); o->line = line;
    if (g_noemit) {                             /* a scan: no call, but it is kept, for emit_expr */
        o->uc = xmalloc(sizeof *o->uc); *o->uc = u;
        return;
    }
    ucall_emit(&u);
}

/* the arguments' passing, and the result's record: BY CONTENT copies and
 * the result made new, as each parse of the call makes them */
static void ucall_bind(UCall *u, const char *name)
{
    int line = u->line;
    FnSig *f = &g_fnsig[u->sig];
    /* as many arguments as parameters, but for trailing OPTIONAL ones left out (14.8.2.1) */
    int need = f->nparam;
    while (need > 0 && f->opt[need - 1]) need--;
    if (u->nargs > f->nparam || u->nargs < need)
        die_at(line, "the function '%s' takes %d argument%s%s, not %d", name, f->nparam, f->nparam == 1 ? "" : "s",
               need < f->nparam ? " (the last ones OPTIONAL)" : "", u->nargs);
    for (int k = 0; k < u->nargs; k++) {
        Opnd *a = &u->arg[k];
        if (u->byref[k] == 2) {
            if (!f->opt[k]) die_at(a->line, "argument %d of '%s' is OMITTED, but the parameter is not OPTIONAL (2023 14.8.2.1)", k + 1, name);
            continue;
        }
        if (f->byval[k]) {
            /* BY VALUE: the argument converted to the parameter's
             * description as COMPUTE (numeric) or MOVE would (14.8.2.3.3
             * rule 2), into a copy the function takes the address of */
            if (a->kind == O_REF && a->ref.sym->is_cond) die_at(a->line, "a condition-name cannot be passed");
            u->byref[k] = 0;
            u->ctmp[k] = ftemp_new(&f->param[k], line);
            continue;
        }
        if (f->param[k].size == -1) {
            /* an ANY LENGTH parameter (2023 13.18.2): an item of its class
             * by reference, whatever its length; a literal or a function
             * result by content, into a copy of the value's own length */
            int pnat = f->param[k].pic[0] == 'n' || f->param[k].pic[0] == 'N';
            if (opnd_is_national(a) != pnat || a->kind == O_NUM || a->kind == O_EXPR ||
                (a->kind == O_REF && (is_numeric_sym(a->ref.sym) || (a->ref.sym->is_group && pnat))) || (a->kind == O_FUNC && a->fvar))
                die_at(a->line, "argument %d of '%s' is for an ANY LENGTH %s parameter", k + 1, name, pnat ? "national" : "alphanumeric");
            if (a->kind == O_REF && !a->ref.sym->is_ftemp) { u->byref[k] = 1; continue; }
            FDesc cd = f->param[k];
            int len = a->kind == O_STR ? a->tok->len : opnd_size(a);
            if (len < 1) die_at(a->line, "argument %d of '%s': a value of no length for an ANY LENGTH parameter", k + 1, name);
            cd.size = len; snprintf(cd.pic, sizeof cd.pic, "%c(%d)", pnat ? 'n' : 'x', pnat ? len / 2 : len);
            u->byref[k] = 0;
            u->ctmp[k] = ftemp_new(&cd, line);
            continue;
        }
        /* 8.4.3.2.4 rule 5: an identifier that could receive goes BY REFERENCE,
         * and must then be described as the parameter is (14.8.2.3); a
         * literal, an expression or a function result goes BY CONTENT, into
         * a copy described as the parameter is */
        if (a->kind == O_REF && !a->ref.sym->is_ftemp && !a->ref.rm) {
            FDesc ad; fdesc_of(&ad, a->ref.sym);
            FDesc *pd = &f->param[k];
            if (!fdesc_match(&ad, pd))
                die_at(a->line, "argument %d of '%s' must be described as the parameter is (PICTURE %s, %d bytes; "
                                "2023 14.8.2.3), or be a literal or expression", k + 1, name, pd->group ? "group" : pd->pic, pd->size);
            u->byref[k] = 1;
        } else {
            u->byref[k] = 0;
            u->ctmp[k] = ftemp_new(&f->param[k], line);
        }
    }
    u->res = ftemp_new(&f->ret, line);
}

/* the call recorded, and made now -- or, inside a condition, where the
 * condition is evaluated */
static void ucall_emit(const UCall *u)
{
    if (g_nucall == g_ucap) { g_ucap = g_ucap ? 2 * g_ucap : 64; g_ucall = realloc(g_ucall, (size_t)g_ucap * sizeof *g_ucall); }
    g_ucall[g_nucall] = *u;
    if (g_cond_depth > 0) { g_nucall++; return; }
    int c0 = block_begin();
    emit_ucall(&g_ucall[g_nucall]);
    stmt_call_cut(c0);                          /* to go before the statement's code */
}

/* Item identification's function evaluation (2023 14.6.4): the identifiers
 * in a statement are evaluated left to right as the first operation of
 * its execution.  So, for an operand a scan parsed, about to be emitted:
 * every user function call in it made (or, inside a condition, queued
 * with the condition), once and in the order written -- in its
 * subscripts and reference modifiers,
 * its expression's leaves, a function's arguments, and the operand's own;
 * a call's arguments' first.  In place: the operand is then free of
 * calls, and its code may be emitted any number of times.  The copies and
 * the result are the ones the scan made (flagged ftemp_scan until a call
 * fills them). */
static void ref_calls(Ref *r);
static void expr_calls(Expr *e)
{
    if (!e->op) { ucall_make(e->o); return; }
    expr_calls(e->l);
    if (e->r) expr_calls(e->r);
}
static void ref_calls(Ref *r)
{
    for (int k = 0; k < r->nsub; k++) if (r->sub[k].sym == &g_subx) expr_calls(r->sub[k].x);
    if (r->rm_sx) expr_calls(r->rm_sx);
    if (r->rm_lx) expr_calls(r->rm_lx);
}
static void bexpr_calls(BExpr *b)
{
    if (b->l) bexpr_calls(b->l);
    if (b->r) bexpr_calls(b->r);
    if (b->o) ucall_make(b->o);
}
/* is a call still waiting anywhere in it?  (ucall_make's walk) */
static int opnd_pending(const Opnd *o);
static int expr_pending(const Expr *e)
{
    if (!e->op) return opnd_pending(e->o);
    return expr_pending(e->l) || (e->r && expr_pending(e->r));
}
static int ref_pending(const Ref *r)
{
    for (int k = 0; k < r->nsub; k++) if (r->sub[k].sym == &g_subx && expr_pending(r->sub[k].x)) return 1;
    return (r->rm_sx && expr_pending(r->rm_sx)) || (r->rm_lx && expr_pending(r->rm_lx));
}
static int bexpr_pending(const BExpr *b)
{
    return (b->l && bexpr_pending(b->l)) || (b->r && bexpr_pending(b->r)) || (b->o && opnd_pending(b->o));
}
static int opnd_pending(const Opnd *o)
{
    if (o->uc) return 1;
    switch (o->kind) {
    case O_EXPR: return expr_pending(o->ex);
    case O_BEXPR: return bexpr_pending(o->bx);
    case O_REF: case O_ADDR: return ref_pending(&o->ref);
    case O_FUNC:
        if ((o->farg && opnd_pending(o->farg)) || (o->farg2 && opnd_pending(o->farg2))) return 1;
        for (int k = 0; k < o->nfargs; k++) if (opnd_pending(o->fargs[k])) return 1;
        return (o->fsx && expr_pending(o->fsx)) || (o->flx && expr_pending(o->flx));
    default: return 0;
    }
}
static int refs_pending(const Ref *rs, int n)
{
    for (int i = 0; i < n; i++) if (ref_pending(&rs[i])) return 1;
    return 0;
}
/* A receiving item's calls, made here: where the statement accesses it,
 * not at the statement's beginning -- a MOVE's receiver immediately
 * before the move (2023 14.9.25.4), an arithmetic statement's as each is
 * accessed (14.7.7 rule 4b), READ INTO's after the record is read
 * (14.9.30.4).  The receiver is read as a scan, so its calls wait for
 * this; MOVE 2 TO N T(F(N)) calls F with the new N. */
static void recv_calls(Ref *r)
{
    if (!ref_pending(r)) return;
    g_stmt_calls_hold++;
    ref_calls(r);
    g_stmt_calls_hold--;
}
static void ucall_make(Opnd *o)
{
    if (g_noemit) return;                       /* still a scan: the calls wait */
    switch (o->kind) {
    case O_EXPR: expr_calls(o->ex); break;
    case O_BEXPR: bexpr_calls(o->bx); break;
    case O_REF: case O_ADDR: if (!o->uc) ref_calls(&o->ref); break;
    case O_FUNC:
        if (o->farg) ucall_make(o->farg);
        if (o->farg2) ucall_make(o->farg2);
        for (int k = 0; k < o->nfargs; k++) ucall_make(o->fargs[k]);
        if (o->fsx) expr_calls(o->fsx);
        if (o->flx) expr_calls(o->flx);
        break;
    default: break;
    }
    if (!o->uc) return;
    UCall u = *o->uc;
    for (int k = 0; k < u.nargs; k++) ucall_make(&u.arg[k]);
    for (int k = 0; k < u.nargs; k++) if (u.ctmp[k]) u.ctmp[k]->ftemp_scan = 0;
    u.res->ftemp_scan = 0;
    o->uc = NULL;
    ucall_emit(&u);
}


/* evaluate an intrinsic into libcob's buffer; r1 holds the pointer */
static int g_stmt_convcheck;            /* this statement evaluated a checked NATIONAL-OF / DISPLAY-OF */

/* every element a table reference with ALL subscripts names (X3.23a-1989
 * 2.2; 2023 15.3): the address with 1 in each ALL position, then a loop
 * per ALL position, outermost first, so the rightmost varies fastest;
 * each runs to its OCCURS, or to the DEPENDING ON item's value for the
 * dimension that has one.  Each element is pushed (alnum 0) or handed to
 * MAX/MIN over strings (alnum 1); cslot, when >= 0, counts them. */
static Sym *odo_table_for(Sym *s);
static void emit_all_elements(Opnd *ax, int alnum, int cslot)
{
    Sym *s = ax->ref.sym, *ot = odo_table_for(s);
    int odim = ot && ot->odo_dep_sym ? ot->ndims - 1 : -1;
    int base = g_slot_base, ks[MAXDIM], nk = 0;
    for (int k = 0; k < ax->ref.nsub; k++) if (ax->all_sub & (1u << k)) ks[nk++] = k;
    int B = g_slot_base++, I[MAXDIM];
    for (int q = 0; q < nk; q++) I[q] = g_slot_base++;
    if (g_slot_base > NSLOTS) die_at(ax->line, "internal: too many staged operands");
    emit_ref_addr(&ax->ref, "r3");
    emit("\tstw sp+%d, r3", SLOT(B));
    int Ltop[MAXDIM], Lend[MAXDIM];
    for (int q = 0; q < nk; q++) {
        emit_li("r1", 0); emit("\tstw sp+%d, r1", SLOT(I[q]));
        Ltop[q] = new_label(); Lend[q] = new_label();
        emit_label(Ltop[q]);
        if (ks[q] == odim) {
            Sym *d = ot->odo_dep_sym;
            if (is_hot_int(d)) { emit_item_addr("r1", d, d->offset); emit_load_int(d, "r1", "r1"); }
            else { emit_item_addr("r3", d, d->offset); emit_desc_addr("r4", sym_desc(d)); emit_call("cob_load_int"); }
            emit("\tadd r2, r1, r0");
        } else emit_li("r2", s->dim_count[ks[q]]);
        emit("\tldw r1, sp+%d", SLOT(I[q]));
        emit("\tbge r1, r2, .L%d", Lend[q]);
    }
    emit("\tldw r3, sp+%d", SLOT(B));
    for (int q = 0; q < nk; q++) {
        emit("\tldw r1, sp+%d", SLOT(I[q]));
        emit_li("r2", s->dim_stride[ks[q]]);
        emit("\tmul r1, r1, r2");
        emit("\tadd r3, r3, r1");
    }
    if (alnum) { emit_li("r4", (long)s->size); emit_call("cob_fn_al_arg"); }
    else { emit_desc_addr("r4", sym_desc(s)); emit_call("cob_push"); }
    if (cslot >= 0) { emit("\tldw r1, sp+%d", SLOT(cslot)); emit("\taddi r1, r1, 1"); emit("\tstw sp+%d, r1", SLOT(cslot)); }
    for (int q = nk - 1; q >= 0; q--) {
        emit("\tldw r1, sp+%d", SLOT(I[q]));
        emit("\taddi r1, r1, 1");
        emit("\tstw sp+%d, r1", SLOT(I[q]));
        emit_jump(Ltop[q]);
        emit_label(Lend[q]);                      /* then the next position out steps */
    }
    g_slot_base = base;
}

static void emit_fn_value_raw(Opnd *f);
/* after cob_fn_rm or cob_fn_var_skip, r1 the part: EC-BOUND-REF-MOD when
 * the runtime noted the positions out of range, r1 kept */
static void emit_fn_rm_check(void)
{
    if (!ec_on_name("EC-BOUND-REF-MOD")) return;
    int Lok = new_label();
    emit("\tadd r12, r1, r0");
    emit_call("cob_fn_rm_bad");
    emit("\tbeq r1, r0, .L%d", Lok);
    emit_ec_raise(ec_find("EC-BOUND-REF-MOD", 0));
    emit_label(Lok);
    emit("\tadd r1, r12, r0");
}
/* a function's value in libcob's buffer, r1 its address -- evaluated at
 * its full width, the address then moved to a reference modification's part */
static void emit_fn_value(Opnd *f)
{
    if (f->fkept) {                             /* kept from its one evaluation: the result again, and its length */
        emit_item_addr("r3", f->fkept, f->fkept->offset);
        emit_call("cob_fn_kept");
        return;
    }
    if (!f->ffull) { emit_fn_value_raw(f); return; }
    int part = f->fsize;
    if (f->frm < 0) {
        /* computed positions: the start and the length first -- they may
         * call functions themselves, which would overwrite this one's
         * result and its recorded length (cobol ISSUES-94 E1) -- then the
         * function, then cob_fn_rm, which finds the part (cobol ISSUES-91).
         * No length written is -1, so a computed 0 is out of range (E2). */
        int base = g_slot_base; g_slot_base += 3;
        if (g_slot_base > NSLOTS) die_at(f->line, "internal: too many staged operands");
        emit_expr_pos(f->fsx);
        emit("\tstw sp+%d, r1", SLOT(base + 1));
        if (f->flx) { emit_expr_pos(f->flx); } else emit_li("r1", -1);
        emit("\tstw sp+%d, r1", SLOT(base + 2));
        f->fsize = f->ffull; emit_fn_value_raw(f); f->fsize = part;
        emit("\tstw sp+%d, r1", SLOT(base));
        emit("\tldw r3, sp+%d", SLOT(base));
        emit_li("r4", f->fwasvar ? -1 : f->ffull);
        emit("\tldw r5, sp+%d", SLOT(base + 1));
        emit("\tldw r6, sp+%d", SLOT(base + 2));
        emit_li("r7", f->fnat ? 2 : 1);
        emit_call("cob_fn_rm");
        g_slot_base = base;
        emit_fn_rm_check();
        return;
    }
    f->fsize = f->ffull; emit_fn_value_raw(f); f->fsize = part;
    if (f->fvar && f->frm) {
        /* to the end of a run-time-length result: the pointer on, the
         * length the runtime keeps shortened (cobol ISSUES-88); a start
         * past the end is noted, and checked (E3) */
        emit("\tadd r3, r1, r0");
        emit_li("r4", f->frm);
        emit_call("cob_fn_var_skip");
        emit_fn_rm_check();
    } else {
        if (f->fwasvar && ec_on_name("EC-BOUND-REF-MOD")) {
            /* the part must lie within the result as it came out (8.4.3.3.4
             * rule 5): its end against the length the runtime recorded */
            int Lok = new_label();
            emit("\tadd r12, r1, r0");
            emit_call("cob_fn_last_len");
            emit_li("r2", f->frm + f->fsize);
            emit("\tbge r1, r2, .L%d", Lok);
            emit_ec_raise(ec_find("EC-BOUND-REF-MOD", 0));
            emit_label(Lok);
            emit("\tadd r1, r12, r0");
        }
        if (f->frm) emit("\taddi r1, r1, %d", f->frm);
    }
}

/* r3 = a string argument's address, r4 its length in bytes -- a
 * run-time-length function's taken from libcob as it is evaluated */
static void emit_ref_addr_len(Ref *r);
static void emit_str_arg(Opnd *x)
{
    if (x->kind == O_FUNC) {
        emit_fn_value(x);
        if (x->fvar) {
            emit("\tadd r12, r1, r0");
            emit_call("cob_fn_last_len");
            emit("\tadd r4, r1, r0");
            emit("\tadd r3, r12, r0");
        } else { emit("\tadd r3, r1, r0"); emit_li("r4", x->fsize); }
        return;
    }
    if (x->kind == O_STR) { emit_la("r3", lit_label((unsigned char *)x->tok->s, x->tok->len)); emit_li("r4", x->tok->len); return; }
    if (x->kind != O_REF || (x->ref.rm && x->ref.rm_bit && ref_static_len(&x->ref) <= 0))
        die_at(x->line, "this function's argument must be an item or a literal of known length");
    if (x->ref.rm && ref_static_len(&x->ref) <= 0) { emit_ref_addr_len(&x->ref); return; }   /* its length computed: an ANY LENGTH item's too */
    emit_ref_addr(&x->ref, "r3");
    emit_li("r4", x->ref.rm ? ref_static_len(&x->ref) : (long)x->ref.sym->size);
}

/* a function's arguments are sending items: each numeric one pushed is
 * tested for EC-DATA-INCOMPATIBLE when that is checked (14.6.13.2 rule 2) */
static void emit_fn_value_raw_1(Opnd *f);
static void emit_fn_value_raw(Opnd *f)
{
    /* its arguments go to the stack the function reads: the wide one for
     * an exact function, the narrow one otherwise, whatever the statement
     * around it computes with */
    int was = g_wide;
    g_wide = f->fwnum && f->fkind == FK_NUMS;
    g_incompat_push++;
    emit_fn_value_raw_1(f);
    g_incompat_push--;
    g_wide = was; if (!was) g_fstmt = 0;
    if (ec_on_name("EC-ARGUMENT-FUNCTION")) {
        /* an argument, or the value, outside the function's rules: the
         * library noted it (2023 15.3); r1, the result, kept */
        int Lok = new_label();
        emit("\tadd r12, r1, r0");
        emit_call("cob_fn_argbad");
        emit("\tbeq r1, r0, .L%d", Lok);
        emit_ec_raise(ec_find("EC-ARGUMENT-FUNCTION", 0));
        emit_label(Lok);
        emit("\tadd r1, r12, r0");
    }
}
static void emit_fn_value_raw_1(Opnd *f)
{
    Opnd *x = f->farg;
    if (f->fn == FN_NATOF || f->fn == FN_DISPOF) {
        /* the substitution character's address first, into a frame slot */
        int slot = g_slot_base++;
        if (f->farg2) {
            Opnd *s2 = f->farg2;
            if (s2->kind == O_STR) emit_la("r1", lit_label((unsigned char *)s2->tok->s, s2->tok->len));
            else emit_ref_addr(&s2->ref, "r1");
        } else emit_li("r1", 0);
        emit("\tstw sp+%d, r1", SLOT(slot));
        emit_str_arg(x);
        emit("\tldw r5, sp+%d", SLOT(slot));
        g_slot_base--;
        /* no substitution character and checking on: libcob notes a
         * substitution (15.66.4 rule 3, 15.26.4 rule 3), and the statement
         * raises EC-DATA-CONVERSION when it completes -- not here, in the
         * middle of its operands, where a declarative that returns would
         * leave the operands already staged behind it */
        int track = !f->farg2 && ec_on_name("EC-DATA-CONVERSION");
        emit_li("r6", track);
        if (track && !g_noemit) g_stmt_convcheck = 1;
        emit_call(f->fn == FN_NATOF ? "cob_fn_national_of" : "cob_fn_display_of");
        return;
    }
    if (f->fn == FN_TRIM) {
        emit_str_arg(x);
        if (f->ftrim) { emit_la("r5", lit_label((unsigned char *)f->ftrim->s, f->ftrim->len)); emit_li("r6", f->ftrim->len / (f->fnat ? 2 : 1)); }
        else { emit_li("r5", 0); emit_li("r6", 0); }
        emit_li("r7", f->fnid);
        emit_li("r8", f->fnat);
        emit_call("cob_fn_trim");
        return;
    }
    if (f->fn == FN_BOOLOFINT) {
        emit_push_opnd(x);                      /* argument-1, on the numeric stack */
        if (f->fvar) { emit_push_opnd(f->farg2); emit_call("cob_pop_int"); emit("\tadd r3, r1, r0"); }
        else emit_li("r3", f->fsize);
        emit_call("cob_fn_boolean_of_integer");
        return;
    }
    if (f->fn == FN_INTOFBOOL) {
        Arg a[2]; opnd_args(x, &a[0], &a[1], 0, 0);
        emit_args(a, 2);
        emit_call("cob_fn_integer_of_boolean");
        return;
    }
    if (f->fn == FN_CHARNAT) {
        emit_push_opnd(x);
        emit_call("cob_pop_int");
        emit("\tadd r3, r1, r0");
        emit_call("cob_fn_char_national");
        return;
    }
    if (f->fn == FN_RMLEN) {
        Arg a[1] = { arg_rlen(&x->ref) };            /* the part's bytes */
        emit_args(a, 1);
        emit_li("r4", f->fnid);
        emit_call("cob_fn_len_digits");
        return;
    }
    if (f->fn == FN_VARLEN) {
        emit_fn_value(x);                        /* its length is libcob's now */
        emit_li("r3", f->fnid);
        emit_call("cob_fn_last_len_digits");
        return;
    }
    if (f->fn == -1) {
        if (f->fkind == FK_NUMS) {
            /* the count in a frame slot: an ALL subscript's is known at run time */
            int cslot = g_slot_base++;
            if (g_slot_base > NSLOTS) die_at(f->line, "internal: too many staged operands");
            emit_li("r1", 0); emit("\tstw sp+%d, r1", SLOT(cslot));
            for (int i = 0; i < f->nfargs; i++) {
                Opnd *ax = f->fargs[i];
                if (ax->all_sub) { emit_all_elements(ax, 0, cslot); continue; }
                emit_push_opnd(ax);
                emit("\tldw r1, sp+%d", SLOT(cslot)); emit("\taddi r1, r1, 1"); emit("\tstw sp+%d, r1", SLOT(cslot));
            }
            emit_li("r3", f->fnid);
            emit("\tldw r4, sp+%d", SLOT(cslot));
            g_slot_base--;
            if (f->fwnum) { emit_li("r5", f->fscale < 0 ? 0 : f->fscale); emit_call("cob_fn_wnum"); }
            else emit_call("cob_fn_num");
            return;
        }
        if (f->fkind == FK_ALNUMS) {                    /* MAX/MIN over strings */
            for (int i = 0; i < f->nfargs; i++) {
                Opnd *ax = f->fargs[i];
                if (ax->all_sub) { emit_all_elements(ax, 1, -1); continue; }
                if (ax->kind == O_STR) { emit_la("r3", lit_label((unsigned char *)ax->tok->s, ax->tok->len)); emit_li("r4", ax->tok->len); }
                else if (ax->kind == O_FUNC) { emit_fn_value(ax); emit("\tadd r3, r1, r0"); emit_li("r4", ax->fsize); }
                else emit_ref_addr_len(&ax->ref);
                emit_call("cob_fn_al_arg");
            }
            emit_li("r3", f->fnid);
            emit_li("r4", f->fsize);
            emit_call("cob_fn_al");
            return;
        }
        if (f->fkind == FK_INT) {                       /* CHAR: one integer, by value */
            emit_push_opnd(f->fargs[0]);
            emit_call("cob_pop_int");
            emit("\tadd r3, r1, r0");
            emit_call("cob_fn_char");
            return;
        }
        if ((f->fnid == -6 || f->fnid == -9) && f->nfargs == 2) {
            /* NUMVAL-C's currency string, argument-2: handed over first */
            Opnd *cx = f->fargs[1];
            if (cx->kind == O_STR) { emit_la("r3", lit_label((unsigned char *)cx->tok->s, cx->tok->len)); emit_li("r4", cx->tok->len); }
            else if (cx->kind == O_REF) emit_ref_addr_len(&cx->ref);
            else die_at(f->line, "FUNCTION NUMVAL-C: the currency string must be an item or a literal");
            emit_li("r5", f->fanycase);
            emit_call("cob_fn_currency_arg");
        }
        Opnd *ax = f->fargs[0];                         /* the string functions: r3 the argument, r4 its length */
        if (ax->kind == O_FUNC) { emit_fn_value(ax); emit("\tadd r3, r1, r0"); emit_li("r4", ax->fsize); }
        else if (ax->kind == O_REF) emit_ref_addr_len(&ax->ref);
        else { emit_la("r3", lit_label((unsigned char *)ax->tok->s, ax->tok->len)); emit_li("r4", ax->tok->len); }
        switch (f->fnid) {
        case -3: emit_call("cob_fn_ord"); break;
        case -4: emit_call("cob_fn_reverse"); break;
        case -7: emit_call("cob_fn_numval_f"); break;
        case -8: case -9: case -10:
            emit_li("r5", f->fnid == -8 ? 0 : f->fnid == -9 ? 1 : 2); emit_call("cob_fn_test_numval"); break;
        default: emit_li("r5", f->fnid == -6); emit_call("cob_fn_numval"); break;
        }
        return;
    }
    if (f->fn == FN_CURDATE) { emit_call("cob_fn_current_date"); return; }
    if (f->fn == FN_EXCSTATUS) { emit_call("cob_fn_exception_status"); return; }
    if (f->fn == FN_EXCSTMT) { emit_call("cob_fn_exception_statement"); return; }
    if (f->fn == FN_EXCFILE || f->fn == FN_EXCLOC) {
        emit_li("r3", f->fnid);
        emit_call(f->fn == FN_EXCFILE ? "cob_fn_exception_file" : "cob_fn_exception_location");
        return;
    }
    if (fn_is_numeric(f->fn)) {
        if (x->kind == O_REF && is_hot_int(x->ref.sym)) { emit_ref_addr(&x->ref, "r3"); emit_load_int(x->ref.sym, "r3", "r1"); }
        else if (x->kind == O_REF) { emit_incompat(x); emit_ref_addr(&x->ref, "r3"); emit_desc_addr("r4", sym_desc(x->ref.sym)); emit_call("cob_load_int"); }
        else if (x->kind == O_NUM) emit_li("r1", (long)numlit_int(&x->num));
        else { emit_push_opnd(x); emit_call("cob_pop_fnint"); }   /* an expression: a fraction is an incorrect argument */
        emit("\tadd r3, r1, r0");
        emit_call(fn_runtime_name(f->fn));
        return;
    }
    if (f->fvar) emit_str_arg(x);
    else {
        if (x->kind == O_FUNC) { emit_fn_value(x); emit("\tadd r3, r1, r0"); }
        else if (x->kind == O_REF) emit_ref_addr(&x->ref, "r3");
        else emit_la("r3", lit_label((unsigned char *)x->tok->s, x->tok->len));
        emit_li("r4", f->fsize);
    }
    emit_call(f->fn == FN_UPPER ? (f->fnat ? "cob_fn_upper_nat" : "cob_fn_upper")
                                : (f->fnat ? "cob_fn_lower_nat" : "cob_fn_lower"));
}

/* address + descriptor of an operand, as two Args.  Figuratives need the
 * other operand's size and are expanded by the caller. */
static void opnd_args(Opnd *o, Arg *addr, Arg *desc, int other_size, int other_numeric)
{
    switch (o->kind) {
    case O_REF:
        *addr = arg_ref(&o->ref);
        if (!o->ref.rm) *desc = arg_desc(sym_desc(o->ref.sym));
        else if (o->ref.rm_len && (o->ref.rm_start || !o->ref.rm_bit)) *desc = arg_desc(part_desc(&o->ref));
        else *desc = arg_rdesc(&o->ref);
        return;
    case O_FUNC:
        *addr = arg_func(o);
        if (o->fvar || o->fwnum) { *desc = arg_fdesc(o); return; }
        if (o->fnat) { *desc = arg_desc(nat_desc(o->fsize)); return; }
        if (o->fbool) { *desc = arg_desc(bool_desc(o->fsize)); return; }
        if (o->fn == -1) *desc = arg_desc(o->fscale >= 0 ? numfn_desc(o->fscale) : str_desc(o->fsize));
        else *desc = arg_desc(fn_is_numeric(o->fn) ? fn_num_desc(o) : str_desc(o->fsize));
        return;
    case O_STR:
        *addr = arg_label(lit_label((unsigned char *)o->tok->s, o->tok->len));
        *desc = arg_desc(o->tok->nat ? nat_desc(o->tok->len) : o->tok->boolv ? bool_desc(o->tok->len) : str_desc(o->tok->len)); return;
    case O_NUM: {
        int d; const char *l = num_lit_label(&o->num, &d);
        *addr = arg_label(l); *desc = arg_desc(d); return;
    }
    case O_FIG: case O_ALL: {
        /* ZERO against a numeric item is the number; otherwise a fill of
         * the other operand's length */
        if (o->kind == O_FIG && other_numeric && (!strncmp(o->tok->s, "zero", 4) || !strncmp(o->tok->s, "null", 4))) {   /* NULL: a pointer's zero */
            NumLit z; numlit_zero(&z);
            int d; const char *l = num_lit_label(&z, &d);
            *addr = arg_label(l); *desc = arg_desc(d); return;
        }
        int n = other_size > 0 ? other_size : 1;
        unsigned char *buf = xmalloc(n);
        if (o->kind == O_ALL) for (int i = 0; i < n; i++) buf[i] = (unsigned char)o->tok->s[i % o->tok->len];
        else memset(buf, fig_byte(o->tok->s), n);
        *addr = arg_label(lit_label(buf, n)); *desc = arg_desc(str_desc(n));
        free(buf);
        return;
    }
    }
}

/* a size to expand a figurative constant to: a run-time-length function
 * result's maximum, else the operand's size */
static int opnd_size_bound(Opnd *o) { return o->kind == O_FUNC && o->fvar ? o->fsize : opnd_size(o); }

static int opnd_size(Opnd *o)
{
    switch (o->kind) {
    case O_REF: return ref_static_len(&o->ref);
    case O_STR: return o->tok->len;
    case O_NUM: return o->num.ndigits;
    case O_FUNC:
        if (o->fvar) die_at(o->line, "a function whose length is known only at run time is not supported here yet");
        return o->fsize;
    default: return 0;
    }
}

static int opnd_numeric(Opnd *o)
{
    if (o->kind == O_REF) return !o->ref.rm && is_numeric_sym(o->ref.sym);
    return o->kind == O_NUM || o->kind == O_EXPR;
}

/* a function whose result is a number: ZERO beside it is the number 0,
 * not a fill of its size, and a sign condition takes it (ACAS's maps04:
 * IF FUNCTION TEST-DATE-YYYYMMDD (d) NOT = ZERO was true for every date) */
static int opnd_func_numeric(const Opnd *o)
{
    if (o->kind != O_FUNC || o->fnat || o->fbool) return 0;
    if (o->fwnum) return 1;
    return o->fn == -1 ? o->fscale >= 0 : fn_is_numeric(o->fn);
}

/* the byte length of an operand as an Arg: a literal, or for a
 * reference-modified item whose length is an expression, evaluated */
static Arg arg_len(Opnd *o)
{
    if (o->kind == O_REF && o->ref.rm && !o->ref.rm_len) return arg_rlen(&o->ref);
    if (o->kind == O_FUNC && o->fvar) return arg_flen(o);     /* staged after the function's own A_FUNC */
    return arg_imm(opnd_size(o));
}

/* an integer operand usable on the hot path: a hot-int item, or an
 * integer literal that fits a word */
static int opnd_hot_int(Opnd *o)
{
    /* An unsigned DISPLAY integer of <= 9 digits joins the hot path now that
     * emit_load_int decodes one and emit_store_int encodes one: its value is
     * below 10^9, so every partial sum hot_sum_fits admits still fits a word.
     * GitHub #29 shape (3). */
    if (opnd_display_int(o)) return 1;
    /* a four-byte unsigned item that keeps its capacity (COMP-5, the native
     * types) may use the top bit; one its picture truncates holds at most
     * 999999999 and is a signed word's value */
    if (o->kind == O_REF) return !o->ref.rm && is_hot_int(o->ref.sym) && !(o->ref.sym->size == 4 && !o->ref.sym->pi.is_signed && sym_notrunc(o->ref.sym));
    if (o->kind == O_NUM) return numlit_is_int(&o->num) && numlit_int(&o->num) <= 2147483647LL && numlit_int(&o->num) >= -2147483647LL;
    if (o->kind == O_FIG) return !strncmp(o->tok->s, "zero", 4);
    return 0;
}

/* Two operands whose descriptors are byte-identical and are unsigned
 * DISPLAY numeric with no editing and no P positions: the comparison is a
 * memcmp.  Same length, same digit count, same scale, so the decimal points
 * line up and every character of a canonical field is '0'..'9' -- byte order
 * IS numeric order.
 *
 * It is exact for every value the standard defines, and NOT a conformance
 * fix -- an earlier draft of this comment claimed it was, on GnuCOBOL's
 * evidence alone, and that is wrong.  The 1985 text has a numeric relation
 * condition compare the *algebraic value* of the operands, whatever their
 * usage; for two canonical fields of one descriptor, byte order and
 * algebraic order coincide, so the two readings cannot disagree on any
 * datum the standard admits.
 *
 * They disagree only on a numeric item holding non-digits, which the
 * standard does not define, and there nothing is authoritative -- measured
 * 2026-09-02, three compilers give three answers.  A PIC 9(4) holding
 * '  12' against one holding '0012':
 *
 *      GnuCOBOL   differs, and LESS   (a byte compare; ' ' is 0x20)
 *      gcobol     equal               (decodes, reading a space as zero)
 *      us, before equal               (the same decode)
 *      us, now    differs, and LESS   (GnuCOBOL's answer)
 *
 * So this moves us off gcobol's answer and onto GnuCOBOL's on data that is
 * already outside the language.  Note which way that goes: gcobol's decode
 * is the literal reading of the text, and it is not the implementation we
 * would follow by preference.  The byte compare is adopted because it is
 * exact on every defined value and much cheaper -- NOT because GnuCOBOL
 * does it -- and matching GnuCOBOL here is a side effect, not a warrant.
 * It is a choice on undefined input; free/cmpbytes records it so that
 * changing it later is visible rather than silent.
 *
 * Do not reason from here to the identical-descriptor MOVE of #27, or back.
 * They look alike and rest on different ground: a MOVE between identical
 * descriptors is byte movement, and all three implementations agree on it
 * for exactly the bytes that split them here.  A numeric relation is
 * defined on the algebraic VALUE, which is why implementations diverge as
 * soon as the bytes are not one.
 *
 * Signed is excluded, and that is a correctness condition rather than
 * caution: an overpunched last byte does not order like its digit, and
 * memcmp lands on the opposite side of GnuCOBOL's answer ('001B' against
 * '0012' -- GnuCOBOL says less, memcmp says greater, because 'B' is 0x42
 * and '2' is 0x32).  So are the separate-sign forms, whose sign character
 * sorts against a digit, and BLANK WHEN ZERO, whose spaces are not digits.
 * GitHub #29. */
/* The flag test.  After #29's three shapes, 99.7% of the batch's remaining
 * cob_cmp calls were one shape: a one-byte alphanumeric item against another
 * or against a one-character literal -- "PERFORM UNTIL ws-eof-flag = 'Y'",
 * act-crdb, d-lin-type -- 1.06M calls at 84 instructions each, in every
 * program.  A one-byte alphanumeric relation under the native collating
 * sequence is a byte load and one compare: no padding (both sides are one
 * byte), and byte value IS collating order.  Ordering is unsigned, as
 * cmp_bytes orders it.  Bars: a PROGRAM COLLATING SEQUENCE (the runtime
 * compares through its table; the text says a unit without one is native,
 * and that is what this emits), groups, 88s, reference modification, and a
 * numeric class on either side (that is a digits-as-characters compare with
 * its own rules).  Both sides literal is a constant, left to the runtime.
 * GitHub #29, ISSUES-26. */
/* a reference modification of constant length len, of an item whose
 * part is alphanumeric (anything but national or boolean: 2023 8.4.2.3.3
 * makes the part alphanumeric) */
static int ref_rm_alnum_len(const Ref *r, int len)
{
    Sym *s = r->sym;
    if (!r->rm || r->rm_bit || r->rm_nat || (long)r->rm_len != len || s->is_cond) return 0;
    if (s->usage == U_NATIONAL || s->usage == U_BIT || s->nat_usage || s->natgroup || s->bitgroup) return 0;
    return s->pi.category != PIC_NATIONAL && s->pi.category != PIC_BOOLEAN;
}

static int opnd_onebyte_alnum(Opnd *o)
{
    if (o->kind == O_REF) {
        Sym *s = o->ref.sym;
        if (o->ref.rm) return ref_rm_alnum_len(&o->ref, 1);
        if (s->is_group || s->is_cond) return 0;
        if (s->pi.category != PIC_ALPHANUMERIC && s->pi.category != PIC_ALPHABETIC) return 0;
        if (s->pi.edited || s->size != 1) return 0;
        return 1;
    }
    if (o->kind == O_STR || o->kind == O_ALL) return o->tok->len == 1;
    if (o->kind == O_FIG) return strncmp(o->tok->s, "null", 4) != 0;
    return 0;
}

static int cmp_is_onebyte(Opnd *x, Opnd *y)
{
    if (g_collate >= 0) return 0;
    if (x->kind != O_REF && y->kind != O_REF) return 0;
    return opnd_onebyte_alnum(x) && opnd_onebyte_alnum(y);
}

/* r1 = the byte of a one-byte operand (cmp_is_onebyte admitted it) */
static void emit_onebyte_value(Opnd *o)
{
    if (o->kind == O_REF) { emit_ref_addr(&o->ref, "r3"); emit("\tldbu r1, r3+0"); return; }
    emit_li("r1", o->kind == O_FIG ? fig_byte(o->tok->s) : (o->tok->s[0] & 255));
}

/* a constant-length reference modification against a literal of its
 * length, under the native collating sequence: the bytes, as memcmp */
static int cmp_is_rm_lit(Opnd *x, Opnd *y)
{
    if (g_collate >= 0) return 0;
    if (x->kind == O_STR && y->kind == O_REF) { Opnd *t = x; x = y; y = t; }
    if (x->kind != O_REF || y->kind != O_STR || y->tok->len < 1) return 0;
    return ref_rm_alnum_len(&x->ref, y->tok->len);
}

static int cmp_is_bytewise(Opnd *x, Opnd *y)
{
    if (x->kind != O_REF || y->kind != O_REF) return 0;
    if (x->ref.rm || y->ref.rm) return 0;
    Sym *a = x->ref.sym, *b = y->ref.sym;
    if (a->is_group || b->is_group || a->is_cond || b->is_cond) return 0;
    /* two alphanumeric or alphabetic items of one length under the native
     * collating sequence: cmp_bytes pads neither, and orders unsigned
     * bytes as memcmp does -- which slow32-dbt runs natively.  A serial
     * SEARCH over PIC X keys was a cob_cmp per entry (the ksearch kernel,
     * 31% of its instructions). */
    if (g_collate < 0 && a->size == b->size && a->size > 0 &&
        (a->pi.category == PIC_ALPHANUMERIC || a->pi.category == PIC_ALPHABETIC) && !a->pi.edited &&
        (b->pi.category == PIC_ALPHANUMERIC || b->pi.category == PIC_ALPHABETIC) && !b->pi.edited)
        return 1;
    if (sym_desc(a) != sym_desc(b)) return 0;
    int di = sym_desc(a);                        /* made first: making it may move the table */
    const Desc *d = &g_desc[di];
    if (d->cat != COB_NUM || d->usage != COB_U_DISPLAY) return 0;
    if (d->flags & (COB_F_SIGNED | COB_F_SEPLEAD | COB_F_SEPTRAIL | COB_F_LEAD | COB_F_BLANKZ)) return 0;
    if (d->picstr[0]) return 0;          /* an edited picture, or P scaling */
    return 1;
}

/* An unsigned DISPLAY integer narrow enough to decode into a word: at most
 * nine digits, so its value is below 10^9 and fits a signed 32-bit register
 * with room to spare.  No scale (a scaled operand would have to be aligned
 * against the other side before comparing), no sign in any of its forms, no
 * editing and no P, and the picture's digits must fill the item exactly so
 * that digit i really is byte i.
 *
 * This is the compare path only.  Arithmetic keeps is_hot_int: a partial sum
 * of these can still leave the word, and the encode side is a different
 * problem from the decode side.  GitHub #29 shape (2). */
static int is_display_int(Sym *s)
{
    if (s->is_group || s->pi.category != PIC_NUMERIC) return 0;
    if (s->usage != U_DISPLAY) return 0;
    if (s->pi.scale != 0 || s->pi.is_signed || s->sign_sep || s->sign_lead) return 0;
    if (s->blank_zero || s->pi.edited || strchr(s->pi.pat, 'P')) return 0;
    if (s->pi.digits < 1 || s->pi.digits > 9) return 0;
    return s->size == s->pi.digits;
}

static int opnd_display_int(Opnd *o)
{
    return o->kind == O_REF && !o->ref.rm && is_display_int(o->ref.sym);
}

/* r1 = the value of such an item, decoded in line: a load, a mask and a
 * multiply-accumulate per digit, against cob_cmp's ~28 per digit through
 * cob_get_num.  The first digit needs no multiply.  r3 holds the address and
 * r11 the constant ten; emit_ref_addr has finished with r11 by then.
 *
 * The mask is `& 15`, not `- '0'`, and that is not a micro-optimisation: it
 * is what cob_get_num does, so the inline decode agrees with the runtime on
 * bytes that are not digits as well as on those that are.  '0'..'9' mask to
 * 0..9; a space (0x20) masks to 0, which is cob_get_num's explicit
 * space-is-zero rule; anything else masks to its low nibble, which is
 * cob_get_num's fallback.  Subtracting '0' would have agreed on digits and
 * diverged on everything else, which is a divergence worth not having for
 * free.  One case is left: cob_get_num reads 'p'..'y' as a NEGATIVE
 * overpunch even in an unsigned item, where this reads the low nibble and
 * stays positive.  An unsigned item cannot hold a negative and cob_put_num
 * would never write those bytes, so that is undefined input on both sides.
 * GitHub #29. */
/* NOTHING HERE MAY TOUCH r11.  r11 is the subscript accumulator, and
 * emit_ref_addr holds a partial sum in it across the reference-modification
 * start expression -- which goes through emit_expr, emit_push and so
 * reaches this function.  The first version of this loop kept the constant
 * ten in r11 and silently miscompiled `e(i)(d - 1:2)`: the accumulator
 * became 10, so the subscript resolved to the wrong element.  It read
 * correctly in testing only because the table's element size was also 10.
 * That was CCVS NC122A's regression.  Hence the multiply by ten as
 * (x << 3) + (x << 1), which needs no register beyond r2 and the
 * accumulator: two more instructions per digit than a `mul`, against the
 * ~28 per digit this replaces, and no invariant to remember. */
static void emit_display_decode(int n, const char *areg, const char *dreg)
{
    for (int i = 0; i < n; i++) {
        if (i) {
            emit("\tslli r2, %s, 3", dreg);        /* x * 8 */
            emit("\tslli %s, %s, 1", dreg, dreg);  /* x * 2 */
            emit("\tadd %s, %s, r2", dreg, dreg);  /* x * 10 */
        }
        emit("\tldbu r2, %s+%d", areg, i);
        emit("\tandi r2, r2, 15");
        if (i == 0) emit("\tadd %s, r2, r0", dreg);
        else emit("\tadd %s, %s, r2", dreg, dreg);
    }
}

/* The other direction: vreg's value as n digit characters.  The caller has
 * already brought it inside the picture (emit_trunc_bounded) and made it
 * non-negative, which is what cob_put_num_x would have done, so this is a
 * plain radix loop and vreg may be consumed.  GitHub #29 shape (3). */
static void emit_display_encode(int n, const char *areg, const char *vreg)
{
    /* Ten has to live in a register -- there is no divide-immediate -- and by
     * the rule above it must not be r11.  r4 is an argument register: caller
     * saved, dead outside a call's setup, and this sequence contains no call.
     * The store paths that reach here (emit_store_receivers' hot branch and
     * emit_move's) have finished with emit_ref_addr before calling, so no
     * argument is live either. */
    emit_li("r4", 10);
    for (int i = n - 1; i >= 0; i--) {
        emit("\trem r2, %s, r4", vreg);
        emit("\taddi r2, r2, 48");
        emit("\tstb %s+%d, r2", areg, i);
        if (i) emit("\tdiv %s, %s, r4", vreg, vreg);
    }
}

static void emit_display_value(const Ref *r)
{
    emit_ref_addr(r, "r3");
    cen_valued(r->sym, "r3");
    emit_display_decode(r->sym->pi.digits, "r3", "r1");
}

/* Comparison is more permissive than arithmetic.  opnd_hot_int bars the
 * four-byte unsigned item because no signed SLT can order a value that uses
 * the top bit, and a partial sum of such operands overflows a word -- both
 * true, and both about arithmetic.  A comparison has neither problem: the
 * unsigned SLTU family orders the whole 32-bit range exactly, and a COBOL
 * unsigned item never holds a negative, so when every operand is
 * non-negative the unsigned compare is the right one for all of them.
 *
 * The bar cost a call: "PERFORM UNTIL ws-i > 56164" with ws-i PIC 9(9) COMP
 * built a descriptor for the literal and went through cob_cmp -- about 440
 * instructions for what is one SGTU.  GitHub #27.  Arithmetic keeps
 * opnd_hot_int; only the relation condition uses this. */
static int opnd_hot_cmp(Opnd *o)
{
    if (opnd_display_int(o)) return 1;
    if (o->kind == O_REF) return !o->ref.rm && is_hot_int(o->ref.sym);
    return opnd_hot_int(o);
}

/* r1 = the operand's value on the compare path */
static void emit_cmp_value(Opnd *o)
{
    if (opnd_display_int(o)) { emit_display_value(&o->ref); return; }
    emit_hot_value(o);
}

/* the operand cannot be negative, so an unsigned compare orders it */
static int opnd_nonneg(Opnd *o)
{
    if (o->kind == O_REF) return !o->ref.sym->pi.is_signed;
    if (o->kind == O_NUM) return numlit_int(&o->num) >= 0;
    return 1;   /* ZERO */
}

/* r1 = integer value of a hot operand; uses r3 (address) and r1/r2/r11 */
static void emit_hot_value(Opnd *o)
{
    if (o->kind == O_NUM) { emit_li("r1", (long)numlit_int(&o->num)); return; }
    if (o->kind == O_FIG) { emit_li("r1", 0); return; }
    emit_ref_addr(&o->ref, "r3");
    g_mark_v = 1;               /* r1 is all this is for: r3 is nobody's afterwards */
    emit_load_int(o->ref.sym, "r3", "r1");
}

static long pow10l(int n) { long v = 1; while (n-- > 0) v *= 10; return v; }

/* Truncate r1 to the receiver's picture when it is a COMP item (COMP-5 and
 * the C types keep the binary field's capacity).
 *
 * bound is an upper bound on |r1|, or -1 when the caller does not know one;
 * nonneg says r1 cannot be negative.  With a bound the divide usually goes:
 * a value that cannot reach the picture's limit needs no truncation at all,
 * and one that can pass it only once -- "ADD 1 TO" an item already inside
 * its picture, which is every PERFORM VARYING step -- tests with a compare
 * and divides only when it has passed it.  REM is a divide, ~30 cycles where
 * the compare is one, and it sat in the hottest loop COBOL has (GitHub
 * #27).  The branch takes REM and not a subtract: the item is inside its
 * picture only if everything that wrote it kept it there, and a group
 * MOVE or a READ INTO over it does not -- 25455 in a PIC 9(4) COMP plus 1
 * came out 15456 by one subtract, where the runtime's store (cob_put_num_x,
 * and GnuCOBOL) makes 5456; the HIR islands' differential found it. */
static void emit_trunc_bounded(Sym *s, long long bound, int nonneg)
{
    int disp = is_display_int(s);
    if (!disp && s->usage != U_BINARY) return;
    /* a binary field wider than its picture needs no truncation; a DISPLAY
     * item is exactly its digits, so it always does */
    if (!disp && s->pi.digits >= capacity_digits(s->size)) return;
    long long lim = pow10l(s->pi.digits);
    if (bound >= 0 && bound < lim) return;                  /* cannot reach it */
    emit_li("r2", lim);
    if (bound >= 0 && nonneg && bound < 2 * lim) {           /* at most one wrap */
        int L = new_label();
        emit("\tbltu r1, r2, .L%d", L);
        emit("\trem r1, r1, r2");
        emit_label(L);
        return;
    }
    emit("\trem r1, r1, r2");
}

static void emit_trunc(Sym *s) { emit_trunc_bounded(s, -1, 0); }
