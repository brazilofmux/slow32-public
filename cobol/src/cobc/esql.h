/* s32-cobc: EXEC SQL.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ====================================================================== */
/* EXEC SQL (docs/esql.md)                                                 */
/* ====================================================================== */

/* The compiler is the precompiler.  A statement's text arrives whole as
 * a T_SQL token; host references (:name, :group.name, an indicator as
 * :name :ind or :name INDICATOR :ind) become ? parameters, bound at run
 * time by one cob_sql_in / cob_sql_out call each, then the statement
 * runs through libcob/esql.c.  No SQL parser: the statement kind comes
 * from its first words and the rest is passed through. */

typedef struct { char name[64], qual[64], ind[64], indqual[64]; int line; Sym *sym, *vclen; } SqlHost;   /* sym: an item of an expanded host structure, or a VARCHAR's text; vclen: a VARCHAR's length */
typedef struct { char name[64]; char *query; SqlHost *in; int nin; int unit, id, positioned, line, dyn; } SqlCursor;   /* dyn: FOR a prepared statement's id, else -1 */
typedef struct { char name[64]; int unit, id, rowid; } SqlDyn;  /* a prepared statement's name; rowid: a positioned cursor runs it */
static SqlDyn *g_sqldyn; static int g_nsqldyn;
typedef struct { char *text; int kind, cursor, unit, id; } SqlStmt;
enum { SQLK_EXEC = 0, SQLK_SELECT_INTO = 1, SQLK_NOOP = 2, SQLK_POSITIONED = 3 };

static SqlCursor *g_sqlcur; static int g_nsqlcur;
static SqlStmt *g_sqlst; static int g_nsqlst;
static int g_sql_used;                  /* the program has SQL: compile.sh links the runtime */

static int sqlw(int c) { return isalnum(c) || c == '_' || c == '-' || c == '$' || c == '#' || c == '@'; }

/* the next SQL word from *p (outside quotes); its bytes in w, lowercased */
static int sql_word(const char **p, char *w, int wn)
{
    const char *q = *p;
    while (*q == ' ') q++;
    int n = 0;
    while (sqlw((unsigned char)*q) && n < wn - 1) w[n++] = (char)tolower((unsigned char)*q++);
    w[n] = 0;
    *p = q;
    return n;
}

/* an SQL name at *p: a regular identifier uppercased (so a and "A" are
 * one name, as SQL has it), or a delimited one ("A < a", "" a quote)
 * exactly */
static int sql_name(const char **p, char *w, int wn)
{
    const char *q = *p; int n = 0;
    while (*q == ' ') q++;
    if (*q == '"') {
        for (q++; *q; q++) {
            if (*q == '"') { if (q[1] == '"') { if (n < wn - 1) w[n++] = '"'; q++; continue; } q++; break; }
            if (n < wn - 1) w[n++] = *q;
        }
    } else while (sqlw((unsigned char)*q) && n < wn - 1) w[n++] = (char)toupper((unsigned char)*q++);
    w[n] = 0;
    *p = q;
    return n;
}

/* a host reference at p (just past the ':'): the name, qualifier, and an
 * indicator; returns the text after it */
static const char *sql_host(const char *p, SqlHost *h, int line)
{
    memset(h, 0, sizeof *h); h->line = line;
    int n = 0;
    while (sqlw((unsigned char)*p) && n < 63) h->name[n++] = (char)tolower((unsigned char)*p++);
    if (!n) die_at(line, "EXEC SQL: ':' must be followed by a host variable's name");
    if (*p == '.' && sqlw((unsigned char)p[1])) {            /* :group.name */
        memcpy(h->qual, h->name, sizeof h->qual); n = 0; p++;
        memset(h->name, 0, sizeof h->name);
        while (sqlw((unsigned char)*p) && n < 63) h->name[n++] = (char)tolower((unsigned char)*p++);
    }
    const char *q = p; while (*q == ' ') q++;
    const char *ip = NULL;
    if (*q == ':') ip = q + 1;
    else if (!strncasecmp(q, "indicator", 9) && !sqlw((unsigned char)q[9])) { q += 9; while (*q == ' ') q++; if (*q == ':') ip = q + 1; }
    if (ip) {
        n = 0; p = ip;
        while (sqlw((unsigned char)*p) && n < 63) h->ind[n++] = (char)tolower((unsigned char)*p++);
        if (*p == '.' && sqlw((unsigned char)p[1])) {
            memcpy(h->indqual, h->ind, sizeof h->indqual); n = 0; p++; memset(h->ind, 0, sizeof h->ind);
            while (sqlw((unsigned char)*p) && n < 63) h->ind[n++] = (char)tolower((unsigned char)*p++);
        }
    }
    return p;
}

/* a host reference's item, or NULL when it is not declared yet (a cursor
 * declared in the data division) or ambiguous */
static Sym *sql_sym_quiet(const char *name, const char *qual)
{
    Sym *found = NULL; int n = 0;
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_filler || strcmp(s->name, name)) continue;
        if (qual[0]) {
            int ok = 0;
            for (int p = s->parent; p >= 0; p = g_sym[p].parent) if (!strcmp(g_sym[p].name, qual)) { ok = 1; break; }
            if (!ok) continue;
        }
        found = s; n++;
    }
    return n == 1 ? found : NULL;
}

/* a host structure's elementary items, in order: DB2 takes :group as the
 * list of its items (FILLER, 88s and REDEFINES left out) */
static int sql_struct_items(Sym *g, Sym **out, int max)
{
    int n = 0, gi = (int)(g - g_sym);
    for (int i = gi + 1; i < g_nsym && n < max; i++) {
        Sym *s = &g_sym[i];
        int inside = 0;
        for (int p = s->parent; p >= 0; p = g_sym[p].parent) if (p == gi) { inside = 1; break; }
        if (!inside) break;
        if (s->is_group || s->is_cond || s->is_filler || s->redefines >= 0) continue;
        int red = 0;
        for (int p = s->parent; p >= 0 && p != gi; p = g_sym[p].parent) if (g_sym[p].redefines >= 0) red = 1;
        if (!red) out[n++] = s;
    }
    return n;
}

/* DB2's VARCHAR host variable: a group of exactly two level-49 items, an
 * integer (the length) and alphanumeric text.  One host variable, not a
 * structure: *len and *text its items. */
static int sql_varchar(Sym *g, Sym **len, Sym **text)
{
    Sym *items[3];
    if (sql_struct_items(g, items, 3) != 2) return 0;
    if (items[0]->level != 49 || items[1]->level != 49) return 0;
    if (items[0]->pi.category != PIC_NUMERIC || items[0]->pi.scale) return 0;
    if (items[1]->pi.category != PIC_ALPHANUMERIC) return 0;
    *len = items[0]; *text = items[1];
    return 1;
}

/* the text with each host reference replaced by ?, the references in
 * order; *nh counts them */
static char *sql_params(const char *text, SqlHost **hs, int *nh, int line)
{
    size_t n = strlen(text);
    char *out = xmalloc(n + 1); size_t o = 0;
    char quote = 0;
    *hs = NULL; *nh = 0; int cap = 0;
    for (const char *p = text; *p; ) {
        char c = *p;
        if (quote) { if (c == quote) quote = 0; out[o++] = *p++; continue; }
        if (c == '\'' || c == '"') { quote = c; out[o++] = *p++; continue; }
        if (c == ':' && sqlw((unsigned char)p[1])) {
            SqlHost h;
            p = sql_host(p + 1, &h, line);
            Sym *g = sql_sym_quiet(h.name, h.qual);
            Sym *items[256]; int ni = 0;
            Sym *vlen, *vtext;
            if (g && g->is_group && sql_varchar(g, &vlen, &vtext)) {
                /* a VARCHAR: one ?, its text and its length */
                if (*nh == cap) { cap = cap ? cap * 2 : 8; *hs = xrealloc(*hs, (size_t)cap * sizeof **hs); }
                h.sym = vtext; h.vclen = vlen;
                (*hs)[(*nh)++] = h;
                out[o++] = '?';
                continue;
            }
            if (g && g->is_group && !h.ind[0]) ni = sql_struct_items(g, items, 256);
            if (ni) {
                /* a host structure: one ? per item */
                out = xrealloc(out, n + 1 + o + (size_t)ni * 3 + 16);
                for (int k = 0; k < ni; k++) {
                    if (*nh == cap) { cap = cap ? cap * 2 : 8; *hs = xrealloc(*hs, (size_t)cap * sizeof **hs); }
                    SqlHost *e = &(*hs)[(*nh)++];
                    *e = h; e->sym = items[k];
                    snprintf(e->name, sizeof e->name, "%s", items[k]->name);
                    if (k) { out[o++] = ','; out[o++] = ' '; }
                    out[o++] = '?';
                }
                n += (size_t)ni * 3;
                continue;
            }
            if (*nh == cap) { cap = cap ? cap * 2 : 8; *hs = xrealloc(*hs, (size_t)cap * sizeof **hs); }
            (*hs)[(*nh)++] = h;
            out[o++] = '?';
            continue;
        }
        out[o++] = *p++;
    }
    out[o] = 0;
    return out;
}

/* the top-level (outside quotes and parentheses) word w in text, from
 * position from; its offset or -1 */
static int sql_find(const char *text, int from, const char *w)
{
    int depth = 0; char quote = 0; size_t wl = strlen(w);
    for (int i = from; text[i]; i++) {
        char c = text[i];
        if (quote) { if (c == quote) quote = 0; continue; }
        if (c == '\'' || c == '"') { quote = c; continue; }
        if (c == '(') depth++;
        else if (c == ')') depth--;
        else if (!depth && !strncasecmp(text + i, w, wl) && !sqlw((unsigned char)text[i + wl]) &&
                 (i == 0 || !sqlw((unsigned char)text[i - 1]))) return i;
    }
    return -1;
}

static Sym *sql_sym(const char *name, const char *qual, int line)
{
    char *q[1] = { (char *)qual };
    g_cen_ctx = CEN_SQL; Sym *s = sym_lookup(name, q, qual[0] ? 1 : 0, line); g_cen_ctx = 0;
    if (s->is_cond) die_at(line, "EXEC SQL: ':%s' is a condition-name, not a host variable", name);
    if (s->is_group) die_at(line, "EXEC SQL: ':%s' is a group; host structures are not implemented yet (docs/esql.md, phase 2)", name);
    if (s->ndims) die_at(line, "EXEC SQL: ':%s' is in a table; subscripted host variables are not implemented yet", name);
    return s;
}

/* one cob_sql_in or cob_sql_out call per host reference */
static void sql_emit_hosts(const SqlHost *hs, int n, const char *fn)
{
    for (int i = 0; i < n; i++) {
        Ref r, ri; memset(&r, 0, sizeof r); memset(&ri, 0, sizeof ri);
        r.sym = hs[i].sym ? hs[i].sym : sql_sym(hs[i].name, hs[i].qual, hs[i].line); r.line = hs[i].line;
        if (r.sym->ndims) die_at(hs[i].line, "EXEC SQL: ':%s' is in a table; subscripted host variables are not implemented yet", hs[i].name);
        Arg a[4] = { arg_ref(&r), arg_desc(sym_desc(r.sym)), arg_imm(0), arg_imm(0) };
        if (hs[i].ind[0]) {
            ri.sym = sql_sym(hs[i].ind, hs[i].indqual, hs[i].line); ri.line = hs[i].line;
            if (ri.sym->pi.category != PIC_NUMERIC) die_at(hs[i].line, "EXEC SQL: the indicator ':%s' must be a numeric item", hs[i].ind);
            a[2] = arg_ref(&ri); a[3] = arg_desc(sym_desc(ri.sym));
        }
        if (hs[i].vclen) {
            /* a VARCHAR: its length item too */
            Ref rl; memset(&rl, 0, sizeof rl); rl.sym = hs[i].vclen; rl.line = hs[i].line;
            Arg a6[6] = { a[0], a[1], a[2], a[3], arg_ref(&rl), arg_desc(sym_desc(rl.sym)) };
            emit_args(a6, 6);
            emit_call(!strcmp(fn, "cob_sql_in") ? "cob_sql_in_vc" : "cob_sql_out_vc");
            continue;
        }
        emit_args(a, 4);
        emit_call(fn);
    }
}

/* after a statement: the program's SQLCODE and SQLSTATE, if it has them */
static void sql_emit_status(int line)
{
    static const char *nm[2] = { "sqlcode", "sqlstate" }, *fn[2] = { "cob_sql_put_sqlcode", "cob_sql_put_sqlstate" };
    for (int k = 0; k < 2; k++) {
        Sym *s = sym_lookup_quiet(nm[k]);
        if (!s || s->is_group || s->is_cond || s->ndims) continue;
        cen_flag(s, CEN_SQL);
        Ref r; memset(&r, 0, sizeof r); r.sym = s; r.line = line;
        Arg a[2] = { arg_ref(&r), arg_desc(sym_desc(r.sym)) };
        emit_args(a, 2);
        emit_call(fn[k]);
    }
    /* the rest of an SQLCA, by name (a site's own SQLCA copybook as well
     * as the built-in one): the message, the row count in SQLERRD(3), the
     * warning flags */
    static const char *fld[] = { "sqlerrml", "sqlerrmc", "sqlerrd", "sqlwarn0", "sqlwarn1", NULL };
    for (int k = 0; fld[k]; k++) {
        Sym *s = sym_lookup_quiet(fld[k]);
        if (!s || s->is_group || s->is_cond) continue;
        cen_flag(s, CEN_SQL);
        Ref r; memset(&r, 0, sizeof r); r.sym = s; r.line = line;
        if (k == 2) { if (s->ndims != 1) continue; r.nsub = 1; r.sub[0].sym = NULL; r.sub[0].lit = 3; }
        else if (s->ndims) continue;
        Arg a[3] = { arg_ref(&r), arg_desc(sym_desc(r.sym)), arg_imm(k) };
        emit_args(a, 3);
        emit_call("cob_sql_put_field");
    }
}

static SqlCursor *sql_cursor(const char *name, int line, int must)
{
    for (int i = 0; i < g_nsqlcur; i++)
        if (g_sqlcur[i].unit == g_unit && !strcmp(g_sqlcur[i].name, name)) return &g_sqlcur[i];
    if (must) die_at(line, "EXEC SQL: the cursor '%s' is not declared (DECLARE ... CURSOR FOR must come first)", name);
    return NULL;
}

static int sql_stmt(char *text, int kind, int cursor)
{
    g_sqlst = xrealloc(g_sqlst, (size_t)(g_nsqlst + 1) * sizeof *g_sqlst);
    SqlStmt *st = &g_sqlst[g_nsqlst];
    st->text = text; st->kind = kind; st->cursor = cursor; st->unit = g_unit; st->id = g_nsqlst;
    return g_nsqlst++;
}

/* WHENEVER SQLERROR | SQLWARNING | NOT FOUND {CONTINUE | GO TO name}:
 * where it stands in the text, to the end of the unit */
static struct { int unit; char para[3][64]; } g_when = { -1, { "", "", "" } };
enum { WH_ERROR, WH_WARNING, WH_NOTFOUND };

static void sql_whenever(const char *text, int line)
{
    const char *p = text; char w[64], w2[64];
    sql_word(&p, w, sizeof w);                      /* whenever */
    sql_word(&p, w, sizeof w);
    int k;
    if (!strcmp(w, "sqlerror")) k = WH_ERROR;
    else if (!strcmp(w, "sqlwarning")) k = WH_WARNING;
    else if (!strcmp(w, "not") && sql_word(&p, w2, sizeof w2) && !strcmp(w2, "found")) k = WH_NOTFOUND;
    else die_at(line, "EXEC SQL WHENEVER: expected SQLERROR, SQLWARNING or NOT FOUND");
    if (g_when.unit != g_unit) { memset(&g_when, 0, sizeof g_when); g_when.unit = g_unit; }
    sql_word(&p, w, sizeof w);
    if (!strcmp(w, "continue")) { g_when.para[k][0] = 0; return; }
    if (!strcmp(w, "goto") || (!strcmp(w, "go") && sql_word(&p, w2, sizeof w2) && !strcmp(w2, "to"))) {
        while (*p == ' ') p++;
        if (*p == ':') p++;                             /* GO TO :label, as some precompilers write it */
        char name[64]; if (!sql_word(&p, name, sizeof name)) die_at(line, "EXEC SQL WHENEVER ... GO TO needs a procedure-name");
        if (!para_find(name)) die_at(line, "EXEC SQL WHENEVER ... GO TO: '%s' is not a paragraph or section", name);
        snprintf(g_when.para[k], sizeof g_when.para[k], "%s", name);
        return;
    }
    die_at(line, "EXEC SQL WHENEVER: expected CONTINUE or GO TO");
}

/* after an executable statement: the WHENEVER branches in force */
static void sql_emit_whenever(void)
{
    if (g_when.unit != g_unit) return;
    if (g_when.para[WH_ERROR][0] || g_when.para[WH_NOTFOUND][0]) {
        emit_call("cob_sql_code");
        if (g_when.para[WH_ERROR][0]) { emit("	blt r1, r0, .Lp%d_%d", g_unit, para_find(g_when.para[WH_ERROR])->id); pc_goto(para_find(g_when.para[WH_ERROR]), "sql"); }
        if (g_when.para[WH_NOTFOUND][0]) { emit_li("r2", 100); emit("	beq r1, r2, .Lp%d_%d", g_unit, para_find(g_when.para[WH_NOTFOUND])->id); pc_goto(para_find(g_when.para[WH_NOTFOUND]), "sql"); }
    }
    if (g_when.para[WH_WARNING][0]) {
        emit_call("cob_sql_warn");
        emit("	bne r1, r0, .Lp%d_%d", g_unit, para_find(g_when.para[WH_WARNING])->id);
        pc_goto(para_find(g_when.para[WH_WARNING]), "sql");
    }
}

/* a prepared statement name's descriptor, made on first mention */
static int sql_dyn(const char *name)
{
    for (int i = 0; i < g_nsqldyn; i++) if (g_sqldyn[i].unit == g_unit && !strcmp(g_sqldyn[i].name, name)) return g_sqldyn[i].id;
    g_sqldyn = xrealloc(g_sqldyn, (size_t)(g_nsqldyn + 1) * sizeof *g_sqldyn);
    SqlDyn *d = &g_sqldyn[g_nsqldyn];
    snprintf(d->name, sizeof d->name, "%s", name); d->unit = g_unit; d->id = g_nsqldyn; d->rowid = 0;
    return g_nsqldyn++;
}

/* a statement given as :host or 'literal' after position p: the host is
 * bound as the one input (cob_sql_in); a literal's text goes in *lit */
static void sql_stmt_source(const char *p, int line, const char *what, char **lit)
{
    *lit = NULL;
    while (*p == ' ') p++;
    if (*p == ':') {
        SqlHost h; sql_host(p + 1, &h, line);
        sql_emit_hosts(&h, 1, "cob_sql_in");
    } else if (*p == '\'') {
        size_t n = strlen(p); char *t = xmalloc(n + 1); size_t o = 0;
        for (p++; *p; p++) { if (*p == '\'') { if (p[1] == '\'') { t[o++] = '\''; p++; continue; } break; } t[o++] = *p; }
        t[o] = 0; *lit = t;
    } else die_at(line, "EXEC SQL %s needs a host variable or a literal holding the statement", what);
}

/* a descriptor's name at *p: 'literal' (its label in *lab) or :host
 * (bound as the pending input, *lab NULL); returns the text after it */
static const char *sql_desc_name(const char *p, int line, const char **lab)
{
    while (*p == ' ') p++;
    *lab = NULL;
    if (*p == ':') { SqlHost h; memset(&h, 0, sizeof h); int k = 0; p++;
        while (sqlw((unsigned char)*p) && k < 63) h.name[k++] = (char)tolower((unsigned char)*p++);
        h.line = line; sql_emit_hosts(&h, 1, "cob_sql_in"); return p; }
    if (*p == '\'') {
        char buf[256]; int o = 0;
        for (p++; *p; p++) { if (*p == '\'') { if (p[1] == '\'') { if (o < 255) buf[o++] = '\''; p++; continue; } p++; break; } if (o < 255) buf[o++] = *p; }
        buf[o] = 0;
        *lab = lit_label((const unsigned char *)buf, o + 1);
        return p;
    }
    die_at(line, "EXEC SQL: a descriptor is named by a literal or a host variable");
    return p;
}
static void sql_la_or_zero(const char *reg, const char *lab) { if (lab) emit_la(reg, lab); else emit_li(reg, 0); }

static int sql_desc_field(const char *w)
{
    static const char *f[] = { "count", "type", "length", "octet_length", "returned_length", "returned_octet_length",
        "precision", "scale", "datetime_interval_code", "datetime_interval_precision", "nullable", "indicator", "data",
        "name", "unnamed", NULL };
    for (int k = 0; f[k]; k++) if (!strcmp(f[k], w)) return k;
    return 15;                                  /* a field SQLite has nothing for */
}

/* one host reference as a Ref (a plain name: a descriptor's value, a target) */
static void sql_ref(const char **pp, Ref *r, int line)
{
    const char *p = *pp; SqlHost h; memset(&h, 0, sizeof h); int k = 0;
    while (sqlw((unsigned char)*p) && k < 63) h.name[k++] = (char)tolower((unsigned char)*p++);
    memset(r, 0, sizeof *r); r->sym = sql_sym(h.name, "", line); r->line = line;
    *pp = p;
}

/* GET / SET DESCRIPTOR name {COUNT ... | VALUE n ...} */
static void sql_getset_desc(const char *p, int get, int line)
{
    const char *lab; p = sql_desc_name(p, line, &lab);
    sql_la_or_zero("r3", lab); emit_call("cob_sql_desc_begin");
    while (*p == ' ') p++;
    char w[64]; const char *q = p; sql_word(&q, w, sizeof w);
    if (!strcmp(w, "value")) {
        p = q; while (*p == ' ') p++;
        if (*p == ':') { p++; Ref r; sql_ref(&p, &r, line); Arg a[2] = { arg_ref(&r), arg_desc(sym_desc(r.sym)) }; emit_args(a, 2); emit_call("cob_sql_desc_value"); }
        else { long n = strtol(p, (char **)&p, 10); emit_li("r3", n); emit_call("cob_sql_desc_value_n"); }
    }
    for (;;) {
        while (*p == ' ' || *p == ',') p++;
        if (!*p) break;
        if (get) {
            /* :target = field */
            if (*p != ':') die_at(line, "EXEC SQL GET DESCRIPTOR: expected :target = field");
            p++; Ref r; sql_ref(&p, &r, line);
            while (*p == ' ') p++;
            if (*p != '=') die_at(line, "EXEC SQL GET DESCRIPTOR: expected = after the target");
            p++;
            char f[64]; sql_word(&p, f, sizeof f);
            Arg a[3] = { arg_ref(&r), arg_desc(sym_desc(r.sym)), arg_imm(sql_desc_field(f)) };
            emit_args(a, 3); emit_call("cob_sql_desc_get");
        } else {
            /* field = n | :value */
            char f[64]; sql_word(&p, f, sizeof f);
            while (*p == ' ') p++;
            if (*p != '=') die_at(line, "EXEC SQL SET DESCRIPTOR: expected = after %s", f);
            p++; while (*p == ' ') p++;
            int code = sql_desc_field(f);
            if (*p == ':') { p++; Ref r; sql_ref(&p, &r, line); Arg a[3] = { arg_ref(&r), arg_desc(sym_desc(r.sym)), arg_imm(code) }; emit_args(a, 3); emit_call("cob_sql_desc_set"); }
            else if (*p == '-' || isdigit((unsigned char)*p)) { long n = strtol(p, (char **)&p, 10); emit_li("r3", code); emit_li("r4", n); emit_call("cob_sql_desc_set_n"); }
            else die_at(line, "EXEC SQL SET DESCRIPTOR %s: a literal other than an integer is not implemented", f);
        }
    }
    emit_li("r3", get); emit_call("cob_sql_desc_end");
}

/* USING SQL DESCRIPTOR d / INTO SQL DESCRIPTOR d at text+at (after the
 * keyword): 1 when it is one, and the call emitted */
static int sql_desc_clause(const char *after, int into, int line)
{
    const char *q = after; char w[64]; sql_word(&q, w, sizeof w);
    if (!strcmp(w, "sql")) sql_word(&q, w, sizeof w);
    if (strcmp(w, "descriptor")) return 0;
    const char *lab; sql_desc_name(q, line, &lab);
    sql_la_or_zero("r3", lab); emit_call(into ? "cob_sql_desc_into" : "cob_sql_desc_using");
    return 1;
}

static const char *sql_unimpl[][2] = {
    { 0, 0 } };

/* DECLARE name TABLE (...): DB2's DCLGEN documentation of a table, which
 * the precompiler checks statements against; nothing here */
static int sql_declare_table(const char *text)
{
    const char *p = text; char w[64];
    sql_word(&p, w, sizeof w); sql_word(&p, w, sizeof w);
    while (*p == ' ') p++;
    if (*p == '.') { p++; sql_word(&p, w, sizeof w); }  /* a qualified table name */
    sql_word(&p, w, sizeof w);
    return !strcmp(w, "table");
}

/* DECLARE name [...] CURSOR FOR query: recorded, nothing emitted */
static int sql_declare(const char *text, int line)
{
    const char *p = text; char w[64];
    sql_word(&p, w, sizeof w);                          /* declare */
    char name[64]; sql_name(&p, name, sizeof name);
    int f = sql_find(text, 0, "for");
    if (sql_find(text, 0, "cursor") < 0 || f < 0) return 0;
    if (sql_cursor(name, line, 0)) die_at(line, "EXEC SQL: the cursor '%s' is declared twice", name);
    g_sqlcur = xrealloc(g_sqlcur, (size_t)(g_nsqlcur + 1) * sizeof *g_sqlcur);
    SqlCursor *c = &g_sqlcur[g_nsqlcur];
    memset(c, 0, sizeof *c);
    snprintf(c->name, sizeof c->name, "%s", name);
    const char *q = text + f + 3; while (*q == ' ') q++;
    c->dyn = -1;
    {   /* FOR a prepared statement's name: a dynamic cursor, its inputs at OPEN ... USING */
        const char *r = q; char w[64]; int n = sql_word(&r, w, sizeof w);
        while (*r == ' ') r++;
        if (n && !*r && strcmp(w, "select") && strcmp(w, "values") && strcmp(w, "table")) c->dyn = sql_dyn(w);
    }
    c->query = c->dyn >= 0 ? xstrndup("", 0) : sql_params(q, &c->in, &c->nin, line);
    c->unit = g_unit; c->id = g_nsqlcur; c->line = line;
    g_nsqlcur++;
    return 1;
}

/* EXEC SQL in the data division: DECLARE SECTION markers, DECLARE CURSOR */
static void parse_exec_sql_data(void)
{
    Tok *t = cur(); advance();
    if (cur()->kind == T_PERIOD) advance();
    g_sql_used = 1;
    const char *p = t->s; char w1[64], w2[64];
    sql_word(&p, w1, sizeof w1); sql_word(&p, w2, sizeof w2);
    if ((!strcmp(w1, "begin") || !strcmp(w1, "end")) && !strcmp(w2, "declare")) { g_sql_declare = w1[0] == 'b'; return; }
    if (!strcmp(w1, "declare") && sql_declare(t->s, t->line)) return;
    if (!strcmp(w1, "declare") && sql_declare_table(t->s)) return;
    die_at(t->line, "EXEC SQL %s in the DATA DIVISION is not implemented yet (docs/esql.md)", w1);
}

/* EXEC SQL in the procedure division */
static void parse_exec_sql(void)
{
    Tok *t = cur(); advance();
    int line = t->line;
    g_sql_used = 1;
    const char *p = t->s; char w1[64], w2[64];
    sql_word(&p, w1, sizeof w1);
    const char *after1 = p;
    sql_word(&p, w2, sizeof w2);
    for (int i = 0; sql_unimpl[i][0]; i++)
        if (!strcmp(w1, sql_unimpl[i][0]))
            die_at(line, "EXEC SQL %s is not implemented yet (docs/esql.md, phase %s)", sql_unimpl[i][0], sql_unimpl[i][1]);
    if ((!strcmp(w1, "begin") || !strcmp(w1, "end")) && !strcmp(w2, "declare")) return;
    if (!strcmp(w1, "declare")) {
        if (sql_declare(t->s, line) || sql_declare_table(t->s)) return;
        die_at(line, "EXEC SQL DECLARE: only DECLARE ... CURSOR FOR and DECLARE ... TABLE are implemented");
    }
    if (!strcmp(w1, "whenever")) { sql_whenever(t->s, line); return; }
    if (!strcmp(w1, "open") || !strcmp(w1, "close")) {
        char cn[64]; const char *cp = after1; sql_name(&cp, cn, sizeof cn);
        SqlCursor *c = sql_cursor(cn, line, 1);
        if (w1[0] == 'o') {
            int u = sql_find(t->s, 0, "using");
            if (u >= 0) {
                if (c->dyn < 0) die_at(line, "EXEC SQL OPEN %s USING: the cursor is not declared FOR a prepared statement", cn);
                const char *r = t->s + u + 5;
                if (!sql_desc_clause(r, 0, line)) {
                    SqlHost *in; int nin; free(sql_params(r, &in, &nin, line));
                    sql_emit_hosts(in, nin, "cob_sql_in");
                }
            } else sql_emit_hosts(c->in, c->nin, "cob_sql_in");
        }
        char lab[32]; snprintf(lab, sizeof lab, ".Lsqc%d", c->id);
        emit_la("r3", lab);
        emit_call(w1[0] == 'o' ? "cob_sql_open" : "cob_sql_close");
        sql_emit_status(line);
        sql_emit_whenever();
        return;
    }
    if (!strcmp(w1, "fetch")) {
        /* FETCH [NEXT] [FROM] cursor INTO :a, ... */
        int in = sql_find(t->s, 0, "into");
        if (in < 0) die_at(line, "EXEC SQL FETCH needs INTO");
        char buf[256]; snprintf(buf, sizeof buf, "%.*s", in, t->s);
        const char *q = buf; char w[64], last[64] = "";
        sql_word(&q, w, sizeof w);                      /* fetch */
        while (sql_name(&q, w, sizeof w)) {
            if (!strcmp(w, "NEXT") || !strcmp(w, "FROM")) continue;
            if (!strcmp(w, "PRIOR") || !strcmp(w, "FIRST") || !strcmp(w, "LAST") || !strcmp(w, "ABSOLUTE") || !strcmp(w, "RELATIVE"))
                die_at(line, "EXEC SQL FETCH %s: scroll cursors are not implemented (SQLite cursors go forward)", w);
            snprintf(last, sizeof last, "%s", w);
        }
        SqlCursor *c = sql_cursor(last, line, 1);
        if (!sql_desc_clause(t->s + in + 4, 1, line)) {
            SqlHost *out; int nout;
            free(sql_params(t->s + in + 4, &out, &nout, line));
            sql_emit_hosts(out, nout, "cob_sql_out");
        }
        char lab[32]; snprintf(lab, sizeof lab, ".Lsqc%d", c->id);
        emit_la("r3", lab);
        emit_call("cob_sql_fetch");
        sql_emit_status(line);
        sql_emit_whenever();
        return;
    }
    if (!strcmp(w1, "execute") && !strcmp(w2, "immediate")) {
        char *lit; sql_stmt_source(p, line, "EXECUTE IMMEDIATE", &lit);
        if (lit) { emit_la("r3", lit_label((const unsigned char *)lit, (int)strlen(lit) + 1)); emit_call("cob_sql_immediate_text"); }
        else emit_call("cob_sql_immediate");
        sql_emit_status(line); sql_emit_whenever();
        return;
    }
    if (!strcmp(w1, "prepare")) {
        /* PREPARE name FROM :text | 'text' */
        int fr = sql_find(t->s, 0, "from");
        if (fr < 0) die_at(line, "EXEC SQL PREPARE needs FROM");
        int id = sql_dyn(w2);
        char *lit; sql_stmt_source(t->s + fr + 4, line, "PREPARE", &lit);
        char lab[32]; snprintf(lab, sizeof lab, ".Lsqd%d", id);
        emit_la("r3", lab);
        if (lit) { emit_la("r4", lit_label((const unsigned char *)lit, (int)strlen(lit) + 1)); emit_call("cob_sql_prepare_text"); }
        else emit_call("cob_sql_prepare");
        sql_emit_status(line); sql_emit_whenever();
        return;
    }
    if (!strcmp(w1, "execute")) {
        /* EXECUTE name [INTO :x, ...] [USING :a, ...] (either order) */
        int u = sql_find(t->s, 0, "using"), in = sql_find(t->s, 0, "into");
        SqlHost *ins = NULL, *outs = NULL; int nins = 0, nouts = 0;
        for (int k = 0; k < 2; k++) {
            int at = k ? in : u;
            if (at < 0) continue;
            int other = k ? u : in;
            const char *r = t->s + at + (k ? 4 : 5);
            if (sql_desc_clause(r, k, line)) continue;
            size_t len = other > at ? (size_t)(other - at) : strlen(t->s + at);
            char *part = xstrndup(t->s + at + (k ? 4 : 5), (int)(len - (k ? 4 : 5)));
            free(sql_params(part, k ? &outs : &ins, k ? &nouts : &nins, line)); free(part);
        }
        int id = sql_dyn(w2);
        sql_emit_hosts(ins, nins, "cob_sql_in");
        sql_emit_hosts(outs, nouts, "cob_sql_out");
        char lab[32]; snprintf(lab, sizeof lab, ".Lsqd%d", id);
        emit_la("r3", lab);
        emit_call("cob_sql_execute");
        sql_emit_status(line); sql_emit_whenever();
        return;
    }
    if (!strcmp(w1, "allocate") || (!strcmp(w1, "deallocate") && !strcmp(w2, "descriptor"))) {
        if (strcmp(w2, "descriptor")) die_at(line, "EXEC SQL %s %s is not implemented", w1, w2);
        const char *lab; const char *r = sql_desc_name(p, line, &lab);
        if (w1[0] == 'a') {
            /* WITH MAX n | :h */
            int wm = sql_find(r, 0, "max");
            const char *m = wm >= 0 ? r + wm + 3 : NULL;
            while (m && *m == ' ') m++;
            if (m && *m == ':') { m++; Ref mr; sql_ref(&m, &mr, line); Arg a[2] = { arg_ref(&mr), arg_desc(sym_desc(mr.sym)) }; emit_args(a, 2); emit_call("cob_load_int"); emit("\tadd r4, r1, r0"); }
            else emit_li("r4", m ? strtol(m, NULL, 10) : 0);
            sql_la_or_zero("r3", lab); emit_call("cob_sql_desc_alloc");
        } else { sql_la_or_zero("r3", lab); emit_call("cob_sql_desc_dealloc"); }
        sql_emit_status(line); sql_emit_whenever();
        return;
    }
    if (!strcmp(w1, "describe")) {
        /* DESCRIBE [INPUT | OUTPUT] s USING SQL DESCRIPTOR d */
        const char *r = after1; char w[64]; int input = 0;
        const char *r0 = r; sql_word(&r0, w, sizeof w);
        if (!strcmp(w, "input") || !strcmp(w, "output")) { input = w[0] == 'i'; r = r0; }
        char nm[64]; sql_word(&r, nm, sizeof nm);
        int u = sql_find(r, 0, "using");
        if (u < 0) die_at(line, "EXEC SQL DESCRIBE needs USING SQL DESCRIPTOR");
        const char *q = r + u + 5; sql_word(&q, w, sizeof w); if (!strcmp(w, "sql")) sql_word(&q, w, sizeof w);
        const char *lab; sql_desc_name(q, line, &lab);
        char dl[32]; snprintf(dl, sizeof dl, ".Lsqd%d", sql_dyn(nm));
        emit_la("r3", dl); sql_la_or_zero("r4", lab); emit_li("r5", input);
        emit_call("cob_sql_describe");
        sql_emit_status(line); sql_emit_whenever();
        return;
    }
    if (!strcmp(w1, "deallocate")) {
        if (!strcmp(w2, "prepare")) {
            char nm[64]; sql_word(&p, nm, sizeof nm);
            char lab[32]; snprintf(lab, sizeof lab, ".Lsqd%d", sql_dyn(nm));
            emit_la("r3", lab);
            emit_call("cob_sql_deallocate_prepare");
            sql_emit_status(line); sql_emit_whenever();
            return;
        }
        die_at(line, "EXEC SQL DEALLOCATE %s is not implemented yet (docs/esql.md, phase 3c)", w2);
    }
    if ((!strcmp(w1, "set") || !strcmp(w1, "get")) && !strcmp(w2, "descriptor")) {
        sql_getset_desc(p, w1[0] == 'g', line);
        sql_emit_status(line); sql_emit_whenever();
        return;
    }
    if (!strcmp(w1, "get")) {
        if (strcmp(w2, "diagnostics")) die_at(line, "EXEC SQL GET %s is not implemented yet (docs/esql.md, phase 3c)", w2);
        /* GET DIAGNOSTICS [EXCEPTION n] :target = item [, ...] */
        static const char *items[] = { "number", "more", "command_function", "dynamic_function", "row_count", NULL };
        static const char *citems[] = { "returned_sqlstate", "message_text", "message_length", "message_octet_length",
                                        "class_origin", "subclass_origin", "condition_number", NULL };
        const char *r = p; char w[64];
        const char *r0 = r; sql_word(&r0, w, sizeof w);
        int cond = 0;
        if (!strcmp(w, "exception") || !strcmp(w, "condition")) {
            cond = 1; r = r0; while (*r == ' ') r++;
            if (*r == ':') {
                /* the condition number: a plain name (the next :target is not its indicator) */
                SqlHost h; memset(&h, 0, sizeof h); int k = 0; r++;
                while (sqlw((unsigned char)*r) && k < 63) h.name[k++] = (char)tolower((unsigned char)*r++);
                Ref cr; memset(&cr, 0, sizeof cr); cr.sym = sql_sym(h.name, h.qual, line); cr.line = line;
                Arg a[2] = { arg_ref(&cr), arg_desc(sym_desc(cr.sym)) }; emit_args(a, 2); emit_call("cob_sql_diag_cond"); }
            else { long n = strtol(r, (char **)&r, 10); emit_li("r3", n); emit_call("cob_sql_diag_cond_n"); }
        }
        for (;;) {
            while (*r == ' ' || *r == ',') r++;
            if (!*r) break;
            if (*r != ':') die_at(line, "EXEC SQL GET DIAGNOSTICS: expected :target = item");
            SqlHost h; r = sql_host(r + 1, &h, line);
            while (*r == ' ') r++;
            if (*r != '=') die_at(line, "EXEC SQL GET DIAGNOSTICS: expected = after :%s", h.name);
            r++;
            char it[64]; sql_word(&r, it, sizeof it);
            int code = -1;
            const char **tab = cond ? citems : items;
            for (int k = 0; tab[k]; k++) if (!strcmp(tab[k], it)) code = k + (cond ? 10 : 0);
            if (code < 0) code = cond ? 17 : 17;             /* an item SQLite has nothing for: blank or zero */
            Ref tr; memset(&tr, 0, sizeof tr); tr.sym = sql_sym(h.name, h.qual, line); tr.line = line;
            Arg a[3] = { arg_ref(&tr), arg_desc(sym_desc(tr.sym)), arg_imm(code) };
            emit_args(a, 3);
            emit_call("cob_sql_diag");
        }
        sql_emit_status(line);
        return;
    }
    int kind = SQLK_EXEC, cursor = -1;
    char *text;
    SqlHost *in = NULL, *out = NULL; int nin = 0, nout = 0;
    if (!strcmp(w1, "grant") || !strcmp(w1, "revoke")) {
        kind = SQLK_NOOP; text = xstrndup(t->s, (int)strlen(t->s));      /* SQLite has no privileges: a behavior point */
    } else if (!strcmp(w1, "select") && sql_find(t->s, 0, "into") >= 0) {
        /* SELECT list INTO :a, ... FROM ...: the INTO list is the outputs */
        int in0 = sql_find(t->s, 0, "into"), fr = sql_find(t->s, in0, "from");
        if (fr < 0) die_at(line, "EXEC SQL SELECT INTO needs FROM");
        char *targets = xstrndup(t->s + in0 + 4, fr - in0 - 4);
        free(sql_params(targets, &out, &nout, line)); free(targets);
        size_t n = strlen(t->s);
        char *rest = xmalloc(n + 1);
        snprintf(rest, n + 1, "%.*s%s", in0, t->s, t->s + fr);
        text = sql_params(rest, &in, &nin, line); free(rest);
        kind = SQLK_SELECT_INTO;
    } else {
        /* positioned UPDATE / DELETE: WHERE CURRENT OF c becomes rowid = ?,
         * the cursor's row bound by the runtime (its query selects rowid) */
        int wc = sql_find(t->s, 0, "where");
        const char *tail = wc >= 0 ? t->s + wc + 5 : NULL;
        char cw1[64] = "", cw2[64] = "", cname[64] = "";
        if (tail) { const char *q = tail; sql_word(&q, cw1, sizeof cw1); sql_word(&q, cw2, sizeof cw2); sql_name(&q, cname, sizeof cname); }
        if (!strcmp(cw1, "current") && !strcmp(cw2, "of")) {
            SqlCursor *c = sql_cursor(cname, line, 1);
            c->positioned = 1; cursor = c->id; kind = SQLK_POSITIONED;
            size_t n = strlen(t->s);
            char *rest = xmalloc(n + 32);
            snprintf(rest, n + 32, "%.*sWHERE rowid = ?", wc, t->s);
            text = sql_params(rest, &in, &nin, line); free(rest);
        } else text = sql_params(t->s, &in, &nin, line);
        (void)after1;
    }
    sql_emit_hosts(in, nin, "cob_sql_in");
    sql_emit_hosts(out, nout, "cob_sql_out");
    char lab[32]; snprintf(lab, sizeof lab, ".Lsqs%d", sql_stmt(text, kind, cursor));
    emit_la("r3", lab);
    emit_call("cob_sql_exec");
    sql_emit_status(line);
    sql_emit_whenever();
}

/* the unit's statement and cursor descriptors (libcob/esql.c reads them) */
/* STOP RUN is the last statement of a consecutive sequence of imperative
 * statements in its sentence (X3.23-1985 STOP syntax rule 2; 2002
 * 14.8.38.2 rule 1): another statement straight after it is refused --
 * ELSE, WHEN, a scope terminator or the period end the sequence */
static int is_verb(const char *w);
static void stop_last_check(Tok *t)
{
    if (cur()->kind == T_WORD && is_verb(cur()->s)) {
        if (g_dialect_mf) { bp(BP_D3_MF_STOP_NOT_LAST, t->line); return; }    /* MF: not enforced */
        die_at(t->line, "STOP RUN is the last statement of its sequence; '%s' follows it (%s)", cur()->s,
               g_std < 2002 ? "X3.23-1985 STOP syntax rule 2" : "2002 14.8.38.2 rule 1");
    }
}

static void emit_sql_data(void)
{
    for (int i = 0; i < g_nsqlst; i++) {
        SqlStmt *st = &g_sqlst[i];
        if (st->unit != g_unit) continue;
        const char *tl = lit_label((const unsigned char *)st->text, (int)strlen(st->text) + 1);
        emit("\t.p2align 2");
        emit(".Lsqs%d:", st->id);
        emit("\t.word %s", tl);                 /* the SQL text */
        emit("\t.word 0");                       /* the prepared statement, the runtime's */
        emit("\t.word %d", st->kind);
        if (st->cursor >= 0) emit("\t.word .Lsqc%d", st->cursor); else emit("\t.word 0");
    }
    for (int i = 0; i < g_nsqlcur; i++) {
        SqlCursor *c = &g_sqlcur[i];
        if (c->unit != g_unit) continue;
        char *q = c->query;
        if (c->positioned && c->dyn >= 0) g_sqldyn[c->dyn].rowid = 1;       /* the runtime puts rowid first when it prepares */
        else if (c->positioned) {
            /* the row a positioned statement names: SELECT rowid, ... */
            int s = sql_find(q, 0, "select");
            if (s < 0) die_at(c->line, "EXEC SQL: cursor '%s' is used with WHERE CURRENT OF but its query is not a SELECT", c->name);
            size_t n = strlen(q);
            char *r = xmalloc(n + 16);
            snprintf(r, n + 16, "%.*sSELECT rowid,%s", s, q, q + s + 6);
            q = r;
        }
        const char *tl = lit_label((const unsigned char *)q, (int)strlen(q) + 1);
        const char *nl = lit_label((const unsigned char *)c->name, (int)strlen(c->name) + 1);
        emit("\t.p2align 2");
        emit(".Lsqc%d:", c->id);
        emit("\t.word %s", tl);                 /* the query */
        emit("\t.word 0");                       /* the prepared statement */
        emit("\t.word %d", c->positioned);       /* its first column is the rowid */
        emit("\t.word 0");                       /* the runtime's: open, and the current row's rowid */
        emit("\t.word 0, 0");
        emit("\t.word %s", nl);                 /* its name, for messages */
        if (c->dyn >= 0) emit("\t.word .Lsqd%d", c->dyn); else emit("\t.word 0");   /* FOR a prepared statement */
    }
    for (int i = 0; i < g_nsqldyn; i++) {
        SqlDyn *d = &g_sqldyn[i];
        if (d->unit != g_unit) continue;
        const char *nl = lit_label((const unsigned char *)d->name, (int)strlen(d->name) + 1);
        emit("\t.p2align 2");
        emit(".Lsqd%d:", d->id);
        emit("\t.word %s", nl);                 /* the statement name */
        emit("\t.word 0");                       /* the prepared statement, the runtime's */
        emit("\t.word 0");                       /* its text, the runtime's copy */
        emit("\t.word %d", d->rowid);            /* a positioned cursor runs it: SELECT rowid, ... */
    }
}

static void parse_statement_1(void)
{
    apply_dirs();                           /* a >>TURN before this statement */
    if (!g_wide) g_fstmt = g_qstmt = 0;
    Tok *t = cur();
    g_rmode = 0;
    if (t->kind == T_SQL) { snprintf(g_cur_stmt, sizeof g_cur_stmt, "EXEC SQL"); parse_exec_sql(); return; }
    if (t->kind != T_WORD) die_at(t->line, "expected a statement, found %s", tok_desc(t));
    g_xd_depth = 0;                     /* no statement is inside an expression; a recovered refusal may have left one open */
    const char *v = t->s;
    { int k = 0; for (; v[k] && k < 15; k++) g_cur_stmt[k] = (char)toupper((unsigned char)v[k]); g_cur_stmt[k] = 0; }

    if (g_std >= 2002 && !strcmp(v, "raise")) { advance(); parse_raise(); return; }
    if (g_std >= 2002 && !strcmp(v, "validate"))
        die_at(t->line, "VALIDATE is not implemented: an obsolete facility no COBOL provider has implemented (2023 D.22, F.2 item 5; docs/standards.md)");
    if (!strcmp(v, "raise")) die_at(t->line, "RAISE is COBOL 2002; compile with -std=2002");
    if (g_std >= 2002 && !strcmp(v, "resume"))
        die_at(t->line, "RESUME is not implemented (COBOL 2014 made it optional)");
    /* refused by name, as the other gaps are: they were "not a COBOL verb"
     * (docs/conformance/coverage.md found them) */
    if (g_std >= 2002 && (!strcmp(v, "commit") || !strcmp(v, "rollback")))
        die_at(t->line, "%s is COBOL 2023; not implemented", !strcmp(v, "commit") ? "COMMIT" : "ROLLBACK");
    if (g_std >= 2002 && !strcmp(v, "invoke"))
        die_at(t->line, "INVOKE is object orientation, not implemented");

    if (g_std >= 2002 && !strcmp(v, "allocate")) { advance(); parse_allocate(); return; }
    if (g_std >= 2002 && !strcmp(v, "free")) { advance(); parse_free(); return; }
    if (!strcmp(v, "display")) {
        advance(); parse_display();
        /* the scope terminator (2023 14.9.11.2, every format) */
        if (at_word("end-display")) {
            if (g_std < 2002) die_at(cur()->line, "END-DISPLAY is COBOL 2002; compile with -std=2002");
            advance();
        }
        return;
    }
    if (!strcmp(v, "move")) { advance(); parse_move(); return; }
    if (!strcmp(v, "add")) { advance(); parse_add(); return; }
    if (!strcmp(v, "subtract")) { advance(); parse_subtract(); return; }
    if (!strcmp(v, "multiply")) { advance(); parse_multiply(); return; }
    if (!strcmp(v, "divide")) { advance(); parse_divide(); return; }
    if (!strcmp(v, "compute")) { advance(); parse_compute(); return; }
    if (!strcmp(v, "open")) { advance(); parse_open(); return; }
    if (!strcmp(v, "close")) { advance(); parse_close(); return; }
    if (!strcmp(v, "read")) { advance(); parse_read(); return; }
    if (!strcmp(v, "write")) { advance(); parse_write(); return; }
    if (!strcmp(v, "rewrite")) { advance(); parse_rewrite(); return; }
    if (!strcmp(v, "delete")) { advance(); parse_delete(); return; }
    if (!strcmp(v, "start")) { advance(); parse_start(); return; }
    if (!strcmp(v, "use")) { advance(); parse_use(); return; }
    if (!strcmp(v, "sort")) { advance(); g_is_merge = 0; parse_sort(); return; }
    if (!strcmp(v, "merge")) { advance(); g_is_merge = 1; parse_sort(); g_is_merge = 0; return; }
    if (!strcmp(v, "release")) { advance(); parse_release(); return; }
    if (!strcmp(v, "return")) { advance(); parse_return(); return; }
    if (!strcmp(v, "string")) { advance(); parse_string(); return; }
    if (!strcmp(v, "unstring")) { advance(); parse_unstring(); return; }
    if (!strcmp(v, "call")) { advance(); parse_call(); return; }
    if (!strcmp(v, "suppress")) {
        advance(); accept_word("printing");
        int rep = -1;
        for (int i = 0; i < g_nrwuse; i++)
            if (g_rwuse[i].unit == g_unit && g_rwuse[i].sec == g_cur_sec_id) rep = g_rwuse[i].rep;
        if (rep < 0) die_at(t->line, "SUPPRESS belongs in a USE BEFORE REPORTING section");
        emit_report_addr("r1", &g_reports[rep]);
        emit_li("r2", 1);
        emit("\tstw r1+%d, r2", RW_OFF_SUPPRESS);
        return;
    }
    if (!strcmp(v, "initiate")) { advance(); parse_initiate(); return; }
    if (!strcmp(v, "accept")) { advance(); parse_accept(); return; }
    if (!strcmp(v, "evaluate")) { advance(); parse_evaluate(); return; }
    if (!strcmp(v, "search")) { advance(); parse_search(); return; }
    if (!strcmp(v, "inspect")) { advance(); parse_inspect(); return; }
    if (!strcmp(v, "initialize")) { advance(); parse_initialize(); return; }
    if (!strcmp(v, "generate")) { advance(); parse_generate(); return; }
    if (!strcmp(v, "terminate")) { advance(); parse_terminate(); return; }
    if (!strcmp(v, "cancel")) {
        /* nothing to release -- the program is linked in -- but its next
         * CALL finds it in its initial state: the registry's cancel routine */
        advance();
        if (cur()->kind == T_NUM) die_at(t->line, "CANCEL: literal-1 is an alphanumeric literal, a program-name (%s)", g_std < 2002 ? "X3.23-1985 CANCEL syntax rule 1" : "2023 14.9.5.3 rule 2");
        while (cur()->kind == T_STR || (cur()->kind == T_WORD && !is_verb(cur()->s) && !is_terminator(cur()->s))) {
            if (cur()->kind == T_STR) { emit_la("r3", lit_label((const unsigned char *)cur()->s, cur()->len)); emit_li("r4", cur()->len); advance(); }
            else {
                Ref r; parse_ref(&r);
                if (r.sym->is_group ? 0 : (is_numeric_sym(r.sym) || sym_is_boolean(r.sym)))
                    die_at(r.line, "CANCEL '%s': identifier-1 is an alphanumeric or national item (%s)", r.sym->name,
                           g_std < 2002 ? "X3.23-1985 CANCEL syntax rule 2" : "2023 14.9.5.3 rule 1");
                emit_ref_addr(&r, "r3"); emit_li("r4", r.sym->size);
            }
            if (g_any_nested) { char vis[32]; snprintf(vis, sizeof vis, ".Lvis%d", g_unit); emit_la("r5", vis); emit_call("cob_cancel_v"); }
            else emit_call("cob_cancel");
            if (ec_on_name("EC-PROGRAM-CANCEL-ACTIVE")) {
                int Lok = new_label();
                emit("\tbeq r1, r0, .L%d", Lok);
                emit_ec_raise(ec_find("EC-PROGRAM-CANCEL-ACTIVE", 0));
                emit_label(Lok);
            }
        }
        return;
    }
    if (!strcmp(v, "if")) { advance(); parse_if(); return; }
    if (!strcmp(v, "perform")) { advance(); parse_perform(); return; }
    if (!strcmp(v, "go")) { advance(); parse_goto(); return; }
    if (!strcmp(v, "set")) { advance(); parse_set(); return; }
    if (!strcmp(v, "unlock")) {
        /* UNLOCK file [RECORD|RECORDS|ALL RECORDS]: RM/COBOL's record
         * locking, released.  One user here: nothing was locked. */
        advance();
        if (cur()->kind != T_WORD) die_at(t->line, "UNLOCK needs a file-name");
        File *uf = file_find(cur()->s);
        if (!uf) die_at(cur()->line, "UNLOCK '%s': not a file (no SELECT)", cur()->s);
        if (uf->org == COB_ORG_SORT) die_at(cur()->line, "UNLOCK of the sort file '%s' (2023 14.9.47.3 rule 1)", uf->name);
        advance();
        accept_word("all"); accept_word("record"); accept_word("records");
        /* the file's I-O status is set (14.9.47.4 rule 3): 00 when open, 47 when not (rule 2) */
        emit_file_addr("r3", uf);
        emit_call("cob_unlock");
        return;
    }
    if (!strcmp(v, "stop")) {
        advance();
        if (at_word("run") && is_word(peek(1), "with")) {
            /* STOP RUN WITH {ERROR | NORMAL} STATUS [identifier | literal]
             * (2002 14.8.38; 2023 14.9.42): the status to the operating
             * system -- an integer its exit status, ERROR alone 1, NORMAL
             * alone 0; an alphanumeric value to standard error, then 1 or 0 */
            advance(); advance();
            if (g_std < 2002) die_at(t->line, "STOP RUN WITH ... STATUS is COBOL 2002; compile with -std=2002");
            int err = 0;
            if (accept_word("error")) err = 1; else if (!accept_word("normal")) die_at(cur()->line, "STOP RUN WITH: expected ERROR or NORMAL");
            accept_word("status");
            if (cur()->kind == T_PERIOD || cur()->kind == T_EOF || !at_operand() || is_verb(cur()->s)) { emit_li("r3", err); emit_call("cob_stop_run"); }
            else {
                Opnd n; parse_operand(&n);
                if (n.kind == O_NUM) {
                    if (!numlit_is_int(&n.num)) die_at(t->line, "STOP RUN WITH STATUS: a numeric literal is an integer (2023 14.9.42.3 rule 3)");
                    emit_li("r3", (long)numlit_int(&n.num)); emit_call("cob_stop_run");
                } else if (n.kind == O_STR) {
                    Arg a[2] = { arg_label(lit_label((unsigned char *)n.tok->s, n.tok->len)), arg_imm(n.tok->len) };
                    emit_args(a, 2); emit_li("r5", err); emit_call("cob_stop_text");
                } else if (n.kind == O_REF && !n.ref.rm && is_int_item(n.ref.sym)) {
                    Arg a[2] = { arg_ref(&n.ref), arg_desc(sym_desc(n.ref.sym)) };
                    emit_args(a, 2); emit_call("cob_load_int");
                    emit("\tadd r3, r0, r1"); emit_call("cob_stop_run");
                } else if (n.kind == O_REF && (n.ref.sym->usage == U_DISPLAY || n.ref.sym->usage == U_NATIONAL) && !is_numeric_sym(n.ref.sym)) {
                    Arg a[2] = { arg_ref(&n.ref), arg_rlen(&n.ref) };
                    emit_args(a, 2); emit_li("r5", err); emit_call("cob_stop_text");
                } else die_at(t->line, "STOP RUN WITH STATUS: an integer item, an item of usage display or national, or a literal (2023 14.9.42.3 rules 2-3)");
            }
            stop_last_check(t);
            return;
        }
        if (accept_word("run")) {
            /* STOP RUN [RETURNING] {integer | identifier}: the process exit
             * status.  Neither form is in the 1985 text (RETURNING is 2002; the
             * bare identifier is RM/COBOL, the Open Systems suite's SJCLCODE
             * copybook ends every program with STOP RUN JCL-CODE), and both are
             * one operand on the exit path that exists (GitHub #35). */
            if (at_word("returning")) bp(BP_E6_STOP_RUN_VALUE, t->line);
            accept_word("returning");
            if (cur()->kind == T_PERIOD || cur()->kind == T_EOF || !at_operand() || is_verb(cur()->s)) { emit_li("r3", 0); emit_call("cob_stop_run"); stop_last_check(t); return; }
            bp(BP_E6_STOP_RUN_VALUE, t->line);
            { Opnd n; parse_operand(&n); check_numeric_opnd(&n);
              emit_incompat(&n);
              if (opnd_hot_int(&n)) emit_hot_value(&n);
              else {
                  if (n.kind != O_REF) die_at(t->line, "STOP RUN needs an integer or a numeric identifier");
                  Arg a[2] = { arg_ref(&n.ref), arg_desc(sym_desc(n.ref.sym)) };
                  emit_args(a, 2); emit_call("cob_load_int");
              }
              emit("\tadd r3, r0, r1"); emit_call("cob_stop_run"); stop_last_check(t); return; }
        }
        /* STOP literal (obsolete): the literal to the operator, who would
         * resume the run -- displayed, and the run goes on */
        if (at_word("all")) die_at(t->line, "STOP literal: not an ALL literal (X3.23-1985 STOP syntax rule 1)");
        if (cur()->kind != T_STR && cur()->kind != T_NUM) die_at(t->line, "STOP needs RUN or a literal");
        if (cur()->kind == T_NUM) { NumLit q; numlit_parse(cur(), &q);
            if (!numlit_is_int(&q) || q.neg || cur()->s[0] == '+') die_at(t->line, "STOP literal: a numeric literal is an unsigned integer (X3.23-1985 STOP syntax rule 3)"); }
        bp(BP_O3_STOP_LITERAL, t->line);
        { Arg a[2] = { arg_label(lit_label((unsigned char *)cur()->s, cur()->len)), arg_imm(cur()->len) }; emit_args(a, 2); emit_call("cob_display"); emit_call("cob_display_nl"); }
        advance();
        return;
    }
    if (!strcmp(v, "goback")) {
        if (g_std < 2002) bp(BP_E2_GOBACK, t->line);
        if (g_in_decl && cur_use_is_global())
            die_at(t->line, "GOBACK in a declarative whose USE statement says GLOBAL (2002 14.8.17.2 rule 1; 2023 14.9.18.3 rule 1)");
        advance();
        if (at_word("raising")) parse_raising_phrase(t->line);
        if (at_word("with") && (is_word(peek(1), "error") || is_word(peek(1), "normal")))
            die_at(t->line, "GOBACK WITH ... STATUS is COBOL 2023 (14.9.18); not implemented -- STOP RUN WITH STATUS is 2002's");
        pc_rec_leave();
        emit("\tjal r0, .Lgb%d", g_unit);
        return;
    }
    if (!strcmp(v, "continue")) {
        advance();
        if (at_word("after")) {
            int k = g_tp; while (k < g_ntok && g_tok[k].kind != T_PERIOD && !is_word(&g_tok[k], "seconds")) k++;
            if (k < g_ntok && is_word(&g_tok[k], "seconds"))
                die_at(t->line, "CONTINUE AFTER ... SECONDS is COBOL 2023 (14.9.9); not implemented");
        }
        return;
    }
    if (!strcmp(v, "exit")) {
        int exit_tp = g_tp;
        advance();
        if (accept_word("program")) {
            if (at_word("raising")) parse_raising_phrase(t->line);
            if (g_is_function)
                die_at(t->line, "EXIT PROGRAM is only in a program's procedure division, not a function's (2023 14.9.14.3 rule 7)");
            if (g_in_decl && cur_use_is_global())
                die_at(t->line, "EXIT PROGRAM in a declarative procedure whose USE is GLOBAL (X3.23-1985 EXIT PROGRAM rule 2; 2023 14.9.14.3 rule 2)");
            if (g_std < 2002 && cur()->kind == T_WORD && is_verb(cur()->s))
                bp(BP_E21_EXIT_PROGRAM_NOT_LAST, t->line);   /* the NIST SQL suite's dml116s: EXIT PROGRAM then STOP RUN */
            /* a program no calling program controls continues past it
             * (X3.23-1985 EXIT PROGRAM general rule 1; 2023 14.9.14.4 rule 2) */
            emit_call("cob_called");
            pc_rec_leave();
            emit("\tbne r1, r0, .Lgb%d", g_unit);
            return;
        }
        if (g_std >= 2002 && accept_word("perform")) {
            /* 2023 14.9.14 format 3 (cobol ISSUES-90) */
            int cycle = accept_word("cycle");
            if (!g_npstk) die_at(t->line, "EXIT PERFORM is only in an inline or exception-checking PERFORM (2023 14.9.14.3 rule 8)");
            if (cycle && g_pstk[g_npstk - 1].Lcycle < 0)
                die_at(t->line, "EXIT PERFORM CYCLE is not allowed in an exception-checking PERFORM (2023 14.9.14.3 rule 8)");
            emit_jump(cycle ? g_pstk[g_npstk - 1].Lcycle : g_pstk[g_npstk - 1].Lexit);
            return;
        }
        if (g_in_finally && (at_word("paragraph") || at_word("section")))
            die_at(t->line, "EXIT %s in a FINALLY phrase: no statement there transfers control out of the PERFORM (2023 14.9.28.4 rule 16)",
                   at_word("paragraph") ? "PARAGRAPH" : "SECTION");
        if (g_std >= 2002 && accept_word("paragraph")) {
            if (!g_cur_para || g_cur_para->is_section) die_at(t->line, "EXIT PARAGRAPH is only in a paragraph (2023 14.9.14.3 rule 10)");
            if (g_exit_par_label < 0) g_exit_par_label = new_label();
            emit_jump(g_exit_par_label);
            return;
        }
        if (g_std >= 2002 && accept_word("section")) {
            if (g_cur_sec_id < 0) die_at(t->line, "EXIT SECTION is only in a section (2023 14.9.14.3 rule 9)");
            if (g_exit_sec_label < 0) g_exit_sec_label = new_label();
            emit_jump(g_exit_sec_label);
            return;
        }
        if (at_word("perform") || at_word("paragraph") || at_word("section"))
            die_at(t->line, "EXIT %s is COBOL 2002; compile with -std=2002",
                   at_word("perform") ? "PERFORM" : at_word("paragraph") ? "PARAGRAPH" : "SECTION");
        /* EXIT alone: a sentence by itself, the only one in its paragraph
         * (X3.23-1985 EXIT syntax rules 1-2; 2023 14.9.14.3 rule 1) */
        int alone = exit_tp == g_para_body_tp && cur()->kind == T_PERIOD;
        if (alone) {
            Tok *n = peek(1);
            alone = n->kind == T_EOF || is_word(n, "end") || unit_start(n) ||
                    (at_para_name(n) && (peek(2)->kind == T_PERIOD || is_word(peek(2), "section")));
        }
        if (!alone && g_dialect_mf) bp(BP_D4_MF_EXIT_NOT_ALONE, t->line);   /* MF: not enforced, a no-op */
        else if (!alone) die_at(t->line, "EXIT must be a sentence by itself, the only one in its paragraph (X3.23-1985 EXIT syntax rule 1; 2023 14.9.14.3 rule 1)");
        return;
    }
    if (!strcmp(v, "next")) die_at(t->line, "NEXT SENTENCE is only valid inside IF (or SEARCH)");
    if (!strcmp(v, "alter")) {
        /* ALTER p1 TO [PROCEED TO] p2 ...: p1's GO TO now goes to p2 */
        bp(BP_O1_ALTER, t->line);
        advance();
        for (;;) {
            Para *p1 = expect_para();
            decl_ref_check(p1, 0, cur()->line);
            if (p1->is_section) die_at(t->line, "ALTER names a paragraph, not a section");
            if (!is_altered_para(p1->name)) die_at(t->line, "internal: '%s' was not seen by the ALTER prescan", p1->name);
            expect_word("to");
            if (accept_word("proceed")) expect_word("to");
            Para *p2 = expect_para();
            decl_ref_check(p2, 0, cur()->line);
            char cell[32], tgt[32];
            snprintf(cell, sizeof cell, ".Lalt%d_%d", g_unit, p1->id);
            snprintf(tgt, sizeof tgt, ".Lp%d_%d", g_unit, p2->id);
            pc_goto_from(p1, p2, "alter");
            emit_la("r2", tgt); emit_la("r1", cell); emit("\tstw r1+0, r2");
            if (!(cur()->kind == T_WORD && !is_verb(cur()->s) && !is_terminator(cur()->s) && para_find(cur()->s))) break;
        }
        return;
    }
    if (!strcmp(v, "receive") || !strcmp(v, "send"))
        die_at(t->line, "%s: COBOL 85's Communication module is out by ruling, and COBOL 2023's asynchronous messaging (14.9.31, 14.9.38) is not implemented", v);
    if (!strcmp(v, "enter") || !strcmp(v, "disable") || !strcmp(v, "enable") || !strcmp(v, "purge"))
        die_at(t->line, "%s is not supported (the Communication module is deliberately out)", v);
    if (is_terminator(v)) die_at(t->line, "'%s' without a matching statement", v);
    if (!strcmp(v, "identification") || !strcmp(v, "id") || unit_start(t))
        die_at(t->line, "%s in the middle of a sentence (a contained program begins after a period)", unit_start(t) && !strcmp(v, "identification") ? "IDENTIFICATION DIVISION" : t->orig ? t->orig : v);
    die_at(t->line, "'%s' is not a COBOL verb", v);
}

static void emit_exit_check(int id)
{
    /* is this exit being performed?  Its cell says (emit_para_cell): zero,
     * and control falls through with no call */
    emit("#@E %d", id);                         /* a mark: the paragraph's statements end here (lower.h reads a paragraph's placeholders) */
    int Ln = new_label();
    emit_para_cell("r3", g_unit, id);
    emit("\tldw r1, r3+0");
    emit("\tbeq r1, r0, .L%d", Ln);
    emit_call("cob_perform_exit");
    emit("\tbeq r1, r0, .L%d", Ln);
    emit("\tjalr r0, r1, 0");
    emit_label(Ln);
}

static int g_saw_end_program;
static int g_initial;               /* PROGRAM-ID ... IS INITIAL: WORKING-STORAGE fresh on every CALL */
static int g_recursive;             /* PROGRAM-ID ... IS RECURSIVE, or contained in such a program (COBOL 2002) */
static int g_std = 85;              /* -std=85 (the default) or -std=2002: Stage B, docs/standards.md */

/* everything a unit keeps in globals, saved while a contained program is compiled */
struct UnitSave {
    int unit, sym_base, sym_end, file_base, file_end, para_base, para_end, use_end;
    char progid[64], progid_orig[64];
    int nreport, report_base, nscreen, screen_base, nclass, nswitch, nalphabet, nmnemonic, last_item, nsame_groups, collate, lowval, highval, cur_fd, in_linkage;
    Alphabet alphabets[16];         /* the containing unit's alphabets: the contained one inherits its collating sequence (2023 12.3.6.4 rule 1) and may declare its own over them */
    char collate_name[64];
    char crtname[64], cursorname[64];
    int nuse, in_decl, cur_sec_id, saw_end, initial, recursive, nsorttab;
    int default_rmode;              /* the OPTIONS paragraph's DEFAULT ROUNDED MODE, which a contained program inherits (11.9.4) */
    int float_bigend, float_dpd;    /* its FLOAT-BINARY and FLOAT-DECIMAL defaults, inherited likewise */
    UseEntry use[64];
    File *io_file;
    UClass cls[16]; SwitchName sw[32]; Alphabet alph[16]; Mnemonic mn[16]; int same[8][16], nsame[8];
    SortTab *sorttab;
};
static void unit_range(int level, int *from, int *to) { *from = g_ustack[level]->sym_base; *to = g_ustack[level]->sym_end; }
static void unit_file_range(int level, int *from, int *to) { *from = g_ustack[level]->file_base; *to = g_ustack[level]->file_end; }
static void unit_use_range(int level, int *from, int *to) { *from = level ? g_ustack[level - 1]->use_end : 0; *to = g_ustack[level]->use_end; }
static int unit_use_own_from(void) { return g_udepth ? g_ustack[g_udepth - 1]->use_end : 0; }

static void parse_identification_division(void);
static void parse_environment_division(void);
static void parse_data_division(void);
static void emit_unit_data(void);
static void emit_act_desc(void);
static void parse_procedure_division(void);

/* IDENTIFICATION DIVISION inside a program: a contained program.  It is
 * compiled as a unit of its own -- its own entry, WORKING-STORAGE, files,
 * paragraphs -- seeing the containing programs' GLOBAL items, files and
 * USE procedures.  The tables are shared: the contained unit's entries
 * are appended and cut back on its END PROGRAM; the USE entries of every
 * enclosing unit stay in g_use below this unit's own. */
static int g_unit_contains;         /* the outermost unit contains a program: its END PROGRAM is required (2023 10.7.3 rule 1) */
static int g_unit_defined;          /* a definition (not a prototype) has been seen: prototypes come first (10.6.2 rule 1) */
static void compile_nested_unit(void)
{
    if (g_udepth == 8) die_at(cur()->line, "programs nested more than 8 deep");
    g_unit_contains = 1;
    UnitSave *u = xmalloc(sizeof *u);
    u->unit = g_unit; u->sym_base = g_sym_base; u->sym_end = g_nsym; u->file_base = g_file_base; u->file_end = g_nfile;
    u->para_base = g_para_base; u->para_end = g_npara; u->use_end = g_nuse;
    memcpy(u->progid, g_progid, sizeof u->progid); memcpy(u->progid_orig, g_progid_orig, sizeof u->progid_orig);
    u->nreport = g_nreport; u->report_base = g_report_base; u->nscreen = g_nscreen; u->screen_base = g_screen_base; u->nclass = g_nclass; u->nswitch = g_nswitch; u->nalphabet = g_nalphabet;
    memcpy(u->alphabets, g_alphabet, sizeof u->alphabets);
    u->nmnemonic = g_nmnemonic; u->last_item = g_last_item; u->nsame_groups = g_nsame_groups; u->collate = g_collate;
    u->lowval = g_lowval; u->highval = g_highval; u->cur_fd = g_cur_fd; u->in_linkage = g_in_linkage;
    memcpy(u->collate_name, g_collate_name, sizeof u->collate_name);
    memcpy(u->crtname, g_crt_status_name, sizeof u->crtname);
    memcpy(u->cursorname, g_cursor_name, sizeof u->cursorname);
    u->nuse = g_nuse; memcpy(u->use, g_use, sizeof u->use); u->in_decl = g_in_decl; u->cur_sec_id = g_cur_sec_id;
    u->saw_end = g_saw_end_program; u->initial = g_initial; u->recursive = g_recursive; u->io_file = g_io_file; u->default_rmode = g_default_rmode; u->float_bigend = g_float_bigend; u->float_dpd = g_float_dpd;
    memcpy(u->cls, g_class, sizeof u->cls); memcpy(u->sw, g_switch, sizeof u->sw); memcpy(u->alph, g_alphabet, sizeof u->alph);
    memcpy(u->mn, g_mnemonic, sizeof u->mn); memcpy(u->same, g_same, sizeof u->same); memcpy(u->nsame, g_nsame, sizeof u->nsame);
    u->nsorttab = g_nsorttab; u->sorttab = xmalloc((size_t)(g_nsorttab + 1) * sizeof *g_sorttab);
    if (g_nsorttab) memcpy(u->sorttab, g_sorttab, (size_t)g_nsorttab * sizeof *g_sorttab);   /* (g_sorttab is NULL until a SORT: memcpy's arguments are nonnull even for no bytes) */
    g_ustack[g_udepth++] = u;

    g_unit = ++g_unit_counter;
    if (g_unit < 4096) g_unit_parent1[g_unit] = u->unit + 1;
    g_sym_base = g_nsym; g_file_base = g_nfile; g_para_base = g_npara;
    /* the contained unit's own USE entries follow every enclosing unit's */
    g_report_base = g_nreport; g_screen_base = g_nscreen; g_nclass = 0; g_nswitch = 0; g_nalphabet = 0; g_nmnemonic = 0; g_last_item = -1;
    g_nsame_groups = 0; g_npoison = 0; g_collate_name[0] = 0; g_crt_status_name[0] = 0; g_cursor_name[0] = 0; g_cur_fd = -1; g_in_linkage = 0;
    /* the PROGRAM COLLATING SEQUENCE, and HIGH-VALUE and LOW-VALUE with it,
     * are the containing unit's unless this one names its own (2023
     * 12.3.6.4 rule 1): the alphabet is carried down as the first of this
     * unit's alphabets */
    if (g_collate >= 0) { Alphabet inh = g_alphabet[g_collate]; g_alphabet[0] = inh; g_nalphabet = 1; g_collate = 0; }
    else { g_lowval = 0x00; g_highval = 0xFF; }
    g_nsorttab = 0; g_initial = 0;
    /* a program contained in a recursive program is recursive (2023 11.10.4 rule 4) */
    g_recursive = u->recursive;
    int in_proc = g_in_proc; char cur_stmt[16]; memcpy(cur_stmt, g_cur_stmt, sizeof cur_stmt);
    /* the enclosing program's frame, returning item and RETURN-CODE use:
     * its epilogue is emitted after this unit is compiled */
    int frame = g_frame, uses_rc = g_uses_rc, lr_used = g_lr_used; Sym *prog_ret = g_prog_ret;
    g_in_proc = 0;
    parse_identification_division();
    parse_environment_division();
    parse_data_division();
    native_apply();
    if (!at_word("procedure")) die_at(cur()->line, "expected PROCEDURE DIVISION, found %s", tok_desc(cur()));
    parse_procedure_division();
    g_in_proc = in_proc; memcpy(g_cur_stmt, cur_stmt, sizeof cur_stmt);
    g_frame = frame; g_uses_rc = uses_rc; g_prog_ret = prog_ret; g_lr_used = lr_used;
    emit_unit_data();
    if (!g_saw_end_program) die_at(cur()->line, "a contained program needs its END PROGRAM");

    g_udepth--;
    g_unit = u->unit; g_sym_base = u->sym_base; g_nsym = u->sym_end; g_file_base = u->file_base; g_nfile = u->file_end;
    g_para_base = u->para_base; g_npara = u->para_end;
    memcpy(g_progid, u->progid, sizeof g_progid); memcpy(g_progid_orig, u->progid_orig, sizeof g_progid_orig);
    g_nreport = u->nreport; g_report_base = u->report_base; g_nscreen = u->nscreen; g_screen_base = u->screen_base; g_nclass = u->nclass; g_nswitch = u->nswitch; g_nalphabet = u->nalphabet;
    memcpy(g_alphabet, u->alphabets, sizeof u->alphabets);
    g_nmnemonic = u->nmnemonic; g_last_item = u->last_item; g_nsame_groups = u->nsame_groups; g_collate = u->collate;
    g_lowval = u->lowval; g_highval = u->highval; g_cur_fd = u->cur_fd; g_in_linkage = u->in_linkage;
    memcpy(g_collate_name, u->collate_name, sizeof g_collate_name);
    memcpy(g_crt_status_name, u->crtname, sizeof g_crt_status_name);
    memcpy(g_cursor_name, u->cursorname, sizeof g_cursor_name);
    g_nuse = u->nuse; memcpy(g_use, u->use, sizeof g_use); g_in_decl = u->in_decl; g_cur_sec_id = u->cur_sec_id;
    g_saw_end_program = u->saw_end; g_initial = u->initial; g_recursive = u->recursive; g_io_file = u->io_file; g_default_rmode = u->default_rmode; g_float_bigend = u->float_bigend; g_float_dpd = u->float_dpd;
    memcpy(g_class, u->cls, sizeof g_class); memcpy(g_switch, u->sw, sizeof g_switch); memcpy(g_alphabet, u->alph, sizeof g_alphabet);
    memcpy(g_mnemonic, u->mn, sizeof g_mnemonic); memcpy(g_same, u->same, sizeof g_same); memcpy(g_nsame, u->nsame, sizeof g_nsame);
    g_nsorttab = u->nsorttab;
    if (g_nsorttab > g_sorttabcap) { g_sorttabcap = g_nsorttab; g_sorttab = realloc(g_sorttab, (size_t)g_sorttabcap * sizeof *g_sorttab); }
    if (g_nsorttab) memcpy(g_sorttab, u->sorttab, (size_t)g_nsorttab * sizeof *g_sorttab);
    free(u->sorttab); free(u);
}

/* Where a parse resumes after an error in a sentence: past its period,
 * unless a paragraph or section header, or the end of the program or of
 * DECLARATIVES, comes first (the period was left off). */
static void resync_sentence(int start)
{
    if (g_tp > start && g_tok[g_tp - 1].kind == T_PERIOD) return;
    if (g_tp == start) advance();
    while (cur()->kind != T_PERIOD && cur()->kind != T_EOF) {
        Tok *t = cur(), *n = peek(1);
        if (t->kind == T_WORD && g_tok[g_tp - 1].line != t->line &&
            ((n->kind == T_PERIOD && para_find(t->s)) || (is_word(n, "section") && peek(2)->kind == T_PERIOD))) return;
        if (is_word(t, "end") && (is_word(n, "program") || is_word(n, "declaratives"))) return;
        if (unit_start(t)) return;
        advance();
    }
    if (cur()->kind == T_PERIOD) advance();
}

/* -fnsig: past this unit's procedure division to its END PROGRAM or END
 * FUNCTION (a contained program's END names another), which is consumed */
static void skip_unit_body(void)
{
    const char *kind = g_is_function ? "function" : "program";
    while (cur()->kind != T_EOF) {
        if (at_word("end") && is_word(peek(1), kind) &&
            (peek(2)->kind == T_PERIOD || peek(2)->kind == T_EOF || is_word(peek(2), g_progid))) {
            advance(); advance();
            if (cur()->kind == T_WORD) advance();
            if (cur()->kind == T_PERIOD) advance();
            g_saw_end_program = 1;
            return;
        }
        advance();
    }
    g_saw_end_program = 0;
}

static void parse_procedure_division(void)
{
    expect_word("procedure"); expect_word("division");
    g_cur_stmt[0] = 0; g_in_proc = 1;
    g_lk_check = 0;                     /* until this division's USING is known */
    g_uses_rc = 0;
    g_lr_used = 0;
    for (int k = g_tp; k < g_ntok && !(g_tok[k].kind == T_WORD && !strcmp(g_tok[k].s, "end") && k + 1 < g_ntok && is_word(&g_tok[k + 1], "program")); k++)
        if (g_tok[k].kind == T_WORD && !strcmp(g_tok[k].s, "return-code")) { g_uses_rc = !sym_lookup_quiet("return-code"); break; }
    /* USING [BY REFERENCE] [OPTIONAL] data-name ... | BY VALUE data-name ...
     * (2023 14.2.1; the phrase carries over to the names after it) */
    Sym *using[32]; int nusing = 0, uval[32], fval[32], mode_val = 0;
    if (accept_word("using")) {
        while (cur()->kind == T_WORD && !at_word("returning")) {
            int opt = 0;
            if (g_std >= 2002) {
                if (accept_word("by")) {
                    if (accept_word("value")) mode_val = 1;
                    else { expect_word("reference"); mode_val = 0; }
                }
                if (at_word("optional")) {
                    if (mode_val) die_at(cur()->line, "OPTIONAL is a BY REFERENCE parameter's (2023 14.2.1)");
                    advance(); opt = 1;
                }
                if (cur()->kind != T_WORD || at_word("returning")) break;
            }
            if (nusing >= (g_is_function ? 16 : 32))
                die_at(cur()->line, g_is_function ? "more than 16 USING items in a function (an implementation limit)" :
                                                    "more than 32 USING items (an implementation limit)");
            Sym *u = sym_lookup(cur()->s, NULL, 0, cur()->line);
            if (!g_sym[u->record].is_linkage || u->parent >= 0)
                die_at(cur()->line, "USING '%s' must be a level 01 or 77 item of the LINKAGE SECTION", u->name);
            if (u->is_based || u->redefines >= 0)
                die_at(cur()->line, "USING '%s': a parameter has no BASED or REDEFINES clause (2023 14.2.2 rule 1)", u->name);
            for (int k = 0; k < nusing; k++)
                if (using[k] == u) die_at(cur()->line, "USING '%s' twice (2023 14.2.2 rule 1)", u->name);
            if (mode_val && (u->is_group || (u->pi.category != PIC_NUMERIC && u->usage != U_POINTER)))
                die_at(cur()->line, "BY VALUE '%s': a numeric or pointer item (2023 14.2.2 rule 2)", u->name);
            u->param_opt = opt;
            /* a function's BY VALUE parameter: the caller converts the
             * argument into a copy described as the parameter is
             * (14.8.2.3.3 rule 2) and passes that copy's address -- the
             * same cell as a BY REFERENCE one, the copy being the
             * caller's and discarded; fval marks it for the signature */
            fval[nusing] = mode_val && g_is_function;
            uval[nusing] = mode_val && !g_is_function;
            using[nusing++] = u;
            advance();
        }
    }
    g_lk_nusing = nusing;
    for (int k = 0; k < nusing; k++) g_lk_using[k] = using[k]->record;
    g_lk_check = g_std < 2002;
    /* BY VALUE parameters live in this activation's frame, above FRAME */
    int voff[32], ext = 0, any_opt = 0;
    for (int i = 0; i < nusing; i++) {
        voff[i] = uval[i] ? FRAME + ext : 0;
        if (uval[i]) ext += (using[i]->size + 3) & ~3;
        any_opt |= using[i]->param_opt;
    }
    g_frame = FRAME + ((ext + 7) & ~7);
    g_prog_ret = NULL;
    if (at_word("returning") && !g_is_function) {
        /* a program's returning item (2023 14.2.2 rules 4-6): the
         * caller's storage, its address taken at entry */
        if (g_std < 2002) die_at(cur()->line, "PROCEDURE DIVISION RETURNING is COBOL 2002; compile with -std=2002");
        advance();
        Sym *r = sym_lookup(cur()->s, NULL, 0, cur()->line);
        if (!r->is_linkage || r->parent >= 0 || r->level == 66 || r->is_cond || r->redefines >= 0 || r->is_based)
            die_at(cur()->line, "RETURNING '%s' must be a level 01 or 77 item of the LINKAGE SECTION, without REDEFINES or BASED (2023 14.2.2 rule 5)", r->name);
        for (int k = 0; k < nusing; k++)
            if (using[k] == r) die_at(cur()->line, "RETURNING '%s' is a USING item too (2023 14.2.2 rule 6)", r->name);
        g_prog_ret = r;
        advance();
    }
    if (!g_is_function && g_std >= 2002) {
        /* the program's signature, for a CALL through a program-specifier
         * (12.3.8; 14.8.2.3 rule 2): kept for this group, and -- an
         * outermost definition -- written to the external repository */
        FnSig sig, *f = &sig;
        memset(f, 0, sizeof *f);
        const char *ext = g_prog_as[0] ? g_prog_as : g_progid;
        snprintf(f->name, sizeof f->name, "%s", g_progid);
        snprintf(f->ext, sizeof f->ext, "%s", ext);
        snprintf(f->link, sizeof f->link, "%s", link_name(ext));
        f->nparam = nusing; f->proto = g_prototype;
        for (int k = 0; k < nusing; k++) { fdesc_of(&f->param[k], using[k]); f->byval[k] = (unsigned char)uval[k]; f->opt[k] = (unsigned char)using[k]->param_opt; }
        if (g_prog_ret) fdesc_of(&f->ret, g_prog_ret); else f->ret.size = 0;
        int prev = -1;
        for (int i = 0; i < g_npgsig; i++) if (!strcmp(g_pgsig[i].ext, f->ext)) prev = i;
        if (prev >= 0 && g_pgsig[prev].proto) {
            FnSig *p = &g_pgsig[prev];
            if (p->nparam != f->nparam) die_at(cur()->line, "the program '%s' takes %d parameter%s, its prototype %d", g_progid, f->nparam, f->nparam == 1 ? "" : "s", p->nparam);
            for (int k = 0; k < nusing; k++)
                if (!fdesc_match(&p->param[k], &f->param[k]) || p->byval[k] != f->byval[k] || p->opt[k] != f->opt[k])
                    die_at(using[k]->line, "the program '%s': parameter '%s' is not described as its prototype's is", g_progid, using[k]->name);
            if (p->ret.size != f->ret.size || (f->ret.size && !fdesc_match(&p->ret, &f->ret)))
                die_at(cur()->line, "the program '%s': RETURNING is not described as its prototype's is", g_progid);
            *p = *f;
        } else if (prev < 0 || g_udepth) {       /* a contained program's name may repeat in another outer program */
            if (g_npgsig == 64) die_at(cur()->line, "more than 64 program signatures");
            g_pgsig[g_npgsig++] = *f;
        }
        if (!g_prototype && !g_udepth) fnsig_write(f, "pg");
    }
    if (g_is_function) {
        /* the function's result: a level 01 or 77 item; the caller passes
         * the address of its temporary after the arguments */
        if (!accept_word("returning")) die_at(cur()->line, "a function needs PROCEDURE DIVISION ... RETURNING its result");
        Sym *r = sym_lookup(cur()->s, NULL, 0, cur()->line);
        if (!r->is_linkage || r->parent >= 0 || r->level == 66 || r->is_cond || r->redefines >= 0)
            die_at(cur()->line, "RETURNING '%s' must be a level 01 or 77 item of the LINKAGE SECTION, without REDEFINES (2023 14.2.2 rule 5)", r->name);
        g_returning = r;
        advance();
        FnSig sig, *f = &sig;
        memset(f, 0, sizeof *f);
        const char *ext = g_fn_as[0] ? g_fn_as : g_progid;
        snprintf(f->name, sizeof f->name, "%s", g_progid);
        snprintf(f->ext, sizeof f->ext, "%s", ext);
        snprintf(f->link, sizeof f->link, "%s", link_name(ext));
        f->nparam = nusing; f->proto = g_prototype;
        for (int k = 0; k < nusing; k++) { fdesc_of(&f->param[k], using[k]); f->byval[k] = (unsigned char)fval[k]; f->opt[k] = (unsigned char)using[k]->param_opt; }
        fdesc_of(&f->ret, r);
        int prev = -1;
        for (int i = 0; i < g_nfnsig; i++) if (!strcmp(g_fnsig[i].ext, f->ext)) prev = i;
        if (prev >= 0 && !g_fnsig[prev].proto)
            die_at(cur()->line, "the function '%s' is defined twice in this compilation group", g_progid);
        if (prev >= 0) {
            /* the definition against its prototype: the same parameters,
             * passed the same way, the same result */
            FnSig *p = &g_fnsig[prev];
            if (p->nparam != f->nparam) die_at(cur()->line, "the function '%s' takes %d parameter%s, its prototype %d", g_progid, f->nparam, f->nparam == 1 ? "" : "s", p->nparam);
            for (int k = 0; k < nusing; k++)
                if (!fdesc_match(&p->param[k], &f->param[k]) || p->byval[k] != f->byval[k] || p->opt[k] != f->opt[k])
                    die_at(using[k]->line, "the function '%s': parameter '%s' is not described as its prototype's is", g_progid, using[k]->name);
            if (!fdesc_match(&p->ret, &f->ret)) die_at(r->line, "the function '%s': RETURNING '%s' is not described as its prototype's is", g_progid, r->name);
            *p = *f;
        } else {
            if (g_nfnsig == 128) die_at(cur()->line, "more than 128 user-defined functions");
            g_fnsig[g_nfnsig++] = *f;
        }
        if (!g_prototype) fnsig_write(f, "fn");
    }
    g_nraising = 0;
    if (accept_word("raising")) {
        /* RAISING exception-name ... (2023 14.2): what this unit may hand
         * its caller; an EC-USER name raised must be here */
        if (g_std < 2002) die_at(cur()->line, "PROCEDURE DIVISION RAISING is COBOL 2002; compile with -std=2002");
        while (cur()->kind == T_WORD && cur()->kind != T_PERIOD) {
            if (strncasecmp(cur()->s, "ec-", 3)) die_at(cur()->line, "RAISING an object reference is object orientation, not implemented; an exception-name is taken");
            int i = ec_find(cur()->s, cur()->line);
            if (i < 0) die_at(cur()->line, "'%s' is not an exception-name", cur()->s);
            if (ec_level(i) != 3 || strncasecmp(ec_name(i), "EC-USER-", 8)) die_at(cur()->line, "RAISING names a level-3 EC-USER exception-name, not %s (2023 14.2.2 rule 7)", ec_name(i));
            if (g_nraising == 32) die_at(cur()->line, "more than 32 names in RAISING");
            g_raising[g_nraising++] = i;
            advance();
        }
        if (!g_nraising) die_at(cur()->line, "RAISING needs an exception-name");
    }
    expect_period();
    if (g_fnsig_only) { skip_unit_body(); return; }
    if (g_prototype) {
        /* a prototype's procedure division is its header (11.5 format 2, 11.10 format 2) */
        if (!(at_word("end") && is_word(peek(1), g_is_function ? "function" : "program")))
            die_at(cur()->line, "a %s prototype has no statements: END %s follows the PROCEDURE DIVISION header",
                   g_is_function ? "function" : "program", g_is_function ? "FUNCTION" : "PROGRAM");
        skip_unit_body();
        return;
    }
    prescan_paragraphs(g_tp);

    char entry[128];
    snprintf(entry, sizeof entry, "%s", link_name(g_fn_as[0] ? g_fn_as : g_prog_as[0] ? g_prog_as : g_progid));   /* the externalized name (AS literal); link_name's buffer is static, CALLs reuse it */
    int nested = g_unit < g_npnode && g_pnode[g_unit].parent >= 0;
    if (nested) snprintf(entry, sizeof entry, ".Lcp%d", g_unit);  /* a contained program: in scope only (8.4.6.3) */
    emit("\t.text");
    if (!nested) emit("\t.globl %s", entry);
    emit("\t.p2align 2");
    if (!nested) emit("\t.type %s,@function", entry);
    emit("%s:", entry);
    emit("\taddi sp, sp, -%d", g_frame);
    emit("\tstw sp+0, lr");
    emit("\tstw sp+4, r11");
    emit("\tstw sp+%d, r12", SLOT_R12);
    emit("\tstw sp+%d, r13", SLOT_R13);
    emit("#@P %d", g_unit);            /* where the saves of loopreg.h's registers go, once it is known which */
    /* the caller's addresses go into the LINKAGE cells, first: every call
     * below clobbers the argument registers (a USING program with DECIMAL-
     * POINT IS COMMA, CURRENCY SIGN, a COLLATING SEQUENCE or IS INITIAL
     * used to take its addresses from what those calls left there) */
    /* how many arguments the CALL passed, when a program has OPTIONAL
     * parameters: a trailing one not passed is omitted (2023 14.9.4 GR
     * 11).  -1: not a CALL from this compiler's code, all taken as given */
    if (any_opt) {
        emit_la("r1", "cob_call_nargs"); emit("\tldw r2, r1+0"); emit("\tstw sp+%d, r2", SLOT_B);
        emit_li("r2", -1); emit("\tstw r1+0, r2");
    }
    if (g_is_function) {
        /* where the result goes: the caller's temporary, its address in
         * cob_call_retaddr (as a program's RETURNING item's) */
        emit_la("r1", "cob_call_retaddr"); emit("\tldw r2, r1+0"); emit("\tstw r1+0, r0");
        emit("\tstw sp+%d, r2", SLOT_RET);
    }
    if (g_prog_ret) {
        /* the caller's returning item; none (a CALL without RETURNING, or
         * from C): a scratch item of this unit's, the result discarded */
        emit_la("r1", "cob_call_retaddr"); emit("\tldw r2, r1+0"); emit("\tstw r1+0, r0");
        int Lhave = new_label(), scr = new_label();
        emit("\tbne r2, r0, .L%d", Lhave);
        emit("\t.data"); emit("\t.p2align 3"); emit(".L%d:", scr); emit("\t.space %d", g_prog_ret->size > 0 ? g_prog_ret->size : 1); emit("\t.text");
        char sl[24]; snprintf(sl, sizeof sl, ".L%d", scr); emit_la("r2", sl);
        emit_label(Lhave);
        emit("\tstw sp+%d, r2", SLOT_RET);
    }
    /* parameter i: in its argument register, or past the eighth at the
     * caller's stack, above this frame */
    #define PARAM_AT(i, reg) do { if ((i) < 8) emit("\tldw %s, sp+%d", reg, SLOT(i)); \
                                  else emit("\tldw %s, sp+%d", reg, g_frame + 4 * ((i) - 8)); } while (0)
    int nreg = nusing < 8 ? nusing : 8;
    if (g_std >= 2002) {
        /* the arguments wait in the frame while cob_act_enter saves the
         * cells they are about to overwrite (a RECURSIVE caller's own) */
        for (int i = 0; i < nreg; i++) emit("\tstw sp+%d, %s", SLOT(i), argreg(i));
        char lab[32]; snprintf(lab, sizeof lab, ".Lact%d", g_unit);
        emit_la("r3", lab); emit_call("cob_act_enter"); emit("\tstw sp+%d, r1", SLOT_ACT);
        for (int i = 0; i < nusing; i++) {
            Sym *u = using[i];
            if (uval[i]) {
                /* BY VALUE: a copy of this activation's, the value stored
                 * into it as into the item */
                emit("\taddi r2, sp, %d", voff[i]);
                emit_la("r1", g_sym[u->record].label);
                emit("\tstw r1+0, r2");
                PARAM_AT(i, "r1");
                if (u->usage == U_POINTER || is_hot_int(u)) {
                    emit("\taddi r3, sp, %d", voff[i]);
                    emit_store_int(u, "r3", "r1");
                } else {
                    emit("\tadd r5, r1, r0");
                    emit("\taddi r3, sp, %d", voff[i]);
                    emit_desc_addr("r4", sym_desc(u));
                    emit_call("cob_store_int");
                }
                continue;
            }
            PARAM_AT(i, "r2");
            if (u->param_opt) {
                /* past the arguments passed: omitted, a NULL cell */
                int Lkeep = new_label();
                emit("\tldw r1, sp+%d", SLOT_B);
                emit("\tblt r1, r0, .L%d", Lkeep);
                emit_li("r3", i);
                emit("\tslt r1, r3, r1");
                emit("\tbne r1, r0, .L%d", Lkeep);
                emit("\tadd r2, r0, r0");
                emit_label(Lkeep);
            }
            emit_la("r1", g_sym[u->record].label);
            emit("\tstw r1+0, r2");
        }
        /* ANY LENGTH (2023 13.18.2.4): each such parameter's size is its
         * argument's, which the caller left in cob_call_lens (in bytes) --
         * into its descriptor, saved and restored with the activation's
         * words when the program recurses.  No lengths (a caller compiled
         * -std=85, or C): the run stops rather than guess */
        int nany = 0;
        for (int i = 0; i < nusing; i++) {
            nany += using[i]->any_len;
            if (using[i]->any_len && uval[i])
                die_at(using[i]->line, "'%s': an ANY LENGTH parameter is BY REFERENCE (2023 13.18.2.3 rules 3-4)", using[i]->name);
        }
        for (int k = g_sym_base; k < g_nsym; k++) {
            if (!g_sym[k].any_len) continue;
            int found = 0;
            for (int i = 0; i < nusing; i++) found |= using[i] == &g_sym[k];
            if (!found) die_at(g_sym[k].line, "'%s': an ANY LENGTH item is a parameter of the PROCEDURE DIVISION header (2023 13.18.2.3 rules 3-4)", g_sym[k].name);
        }
        if (nany) {
            int Lok = new_label();
            emit_la("r1", "cob_call_nlens"); emit("\tldw r2, r1+0");
            emit_li("r3", -1); emit("\tstw r1+0, r3");
            emit_li("r3", nusing);
            emit("\tbge r2, r3, .L%d", Lok);
            emit_la("r3", lit_label((const unsigned char *)g_progid, (int)strlen(g_progid) + 1));
            emit_call("cob_anylen_missing");
            emit_label(Lok);
            for (int i = 0; i < nusing; i++) {
                if (!using[i]->any_len) continue;
                emit_la("r1", "cob_call_lens"); emit("\tldw r2, r1+%d", 4 * i);
                emit_desc_addr("r1", sym_desc(using[i])); emit("\tstw r1+8, r2");
            }
        }
        if (g_prog_ret) {
            emit_la("r1", g_prog_ret->label);
            emit("\tldw r2, sp+%d", SLOT_RET);
            emit("\tstw r1+0, r2");
        }
        if (g_is_function) {
            /* the LINKAGE result is the caller's temporary itself */
            emit_la("r1", g_returning->label);
            emit("\tldw r2, sp+%d", SLOT_RET);
            emit("\tstw r1+0, r2");
        }
    } else
        for (int i = 0; i < nusing; i++) {
            emit_la("r1", g_sym[using[i]->record].label);
            if (i < 8) emit("\tstw r1+0, %s", argreg(i));
            else { emit("\tldw r2, sp+%d", g_frame + 4 * (i - 8)); emit("\tstw r1+0, r2"); }
        }
    #undef PARAM_AT
    emit_call("cob_perform_enter"); emit("\tstw sp+%d, r1", SLOT_PBASE);   /* this activation's PERFORM frames */
    if (g_collate >= 0) {       /* PROGRAM COLLATING SEQUENCE: this unit's table, the caller's kept */
        char lab[32]; snprintf(lab, sizeof lab, ".Lcoll%d", g_unit);
        emit_la("r3", lab); emit_call("cob_set_collating"); emit("\tstw sp+%d, r1", SLOT_COLL);
    }
    if (g_dp_comma) { emit("\taddi r3, r0, 1"); emit_call("cob_set_decimal_point"); emit("\tstw sp+%d, r1", SLOT_DP); }
    if (g_currency_len > 1) {
        emit_li("r3", g_currency); emit_la("r4", lit_label((const unsigned char *)g_currency_str, g_currency_len)); emit_li("r5", g_currency_len);
        emit_call("cob_set_currency_str"); emit("\tstw sp+%d, r1", SLOT_CUR);
    } else if (g_currency && g_currency != '$') { emit_li("r3", g_currency); emit_call("cob_set_currency"); emit("\tstw sp+%d, r1", SLOT_CUR); }
    if (g_initial) { char cl[32]; snprintf(cl, sizeof cl, ".Lcan%d", g_unit); emit_call(cl); }   /* INITIAL: as after CANCEL */
    /* a FILE STATUS item in the LINKAGE SECTION (or EXTERNAL): the image
     * takes its address now that the cell is filled (status is at 16) */
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->status_sym) continue;
        Sym *rec = &g_sym[f->status_sym->record];
        if (!rec_indirect(rec)) continue;
        emit_item_addr("r1", f->status_sym, f->status_sym->offset);
        char lab[32]; snprintf(lab, sizeof lab, ".Lf%d_%d", f->unit, i);
        emit_la("r2", lab);
        emit("\tstw r2+16, r1");
    }
    /* likewise an ASSIGN data-name there (its address is at 24) */
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->assign_sym || !rec_indirect(&g_sym[f->assign_sym->record])) continue;
        emit_item_addr("r1", f->assign_sym, f->assign_sym->offset);
        char lab[32]; snprintf(lab, sizeof lab, ".Lf%d_%d", f->unit, i);
        emit_la("r2", lab);
        emit("\tstw r2+24, r1");
    }
    /* EXTERNAL records: the block every program of this name shares (the
     * records of an EXTERNAL FD share one block under the file's name) */
    int has_ext_file = 0;
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0 || !s->is_external) continue;
        char nm[80];
        if (s->fd >= 0) snprintf(nm, sizeof nm, "file:%s", g_files[s->fd].name);
        else snprintf(nm, sizeof nm, "%s", s->ext_as[0] ? s->ext_as : s->name);   /* AS literal: its externalized name */
        emit_la("r3", lit_label((const unsigned char *)nm, (int)strlen(nm) + 1));
        emit_li("r4", s->image_size);
        emit_call("cob_external");
        emit("\tadd r2, r0, r1");
        emit_la("r1", s->label);
        emit("\tstw r1+0, r2");
    }
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->external) continue;
        has_ext_file = 1;
        char nm[80]; snprintf(nm, sizeof nm, "%s", f->name);
        emit_la("r3", lit_label((const unsigned char *)nm, (int)strlen(nm) + 1));
        char lab[32]; snprintf(lab, sizeof lab, ".Lf%d_%d", f->unit, i);
        emit_la("r4", lab);
        if (f->rec >= 0) { emit_la("r5", g_sym[g_sym[f->rec].record].label); emit("\tldw r5, r5+0"); } else emit_li("r5", 0);
        emit_call("cob_ext_file_enter");
        snprintf(lab, sizeof lab, ".Lfx%d_%d", f->unit, i);
        emit("\tadd r2, r0, r1");
        emit_la("r1", lab);
        emit("\tstw r1+0, r2");
    }

    int cur_par = -1, cur_sec = -1;
    int Ldecl_end = -1;
    g_cur_para = NULL;
    if (!g_udepth) g_nuse = 0;              /* a contained unit's USE entries follow the enclosing units' */
    g_cur_sec_id = -1; g_in_decl = 0;
    g_propagate = 0; apply_dirs();          /* a >>PROPAGATE ON over this unit arrives as a directive at its start */
    if (accept_word("declaratives")) {
        /* the declarative sections are reached only through USE; jump over them */
        expect_period();
        Ldecl_end = new_label(); emit_jump(Ldecl_end); g_in_decl = 1;
    }
    for (;;) {
        Tok *t = cur();
        if (t->kind == T_EOF) break;
        if (is_word(t, "end") && (is_word(peek(1), "program") || is_word(peek(1), "function"))) break;
        if (unit_start(t)) {
            /* a contained program: from here to END PROGRAM the text is nested
             * programs; the containing program's flow ends as at its last line */
            if (cur_par >= 0) { end_par_label(); pc_para_end(cur_par); emit_exit_check(cur_par); }
            if (cur_sec >= 0) { end_sec_label(); pc_para_end(cur_sec); emit_exit_check(cur_sec); }
            cur_par = -1; cur_sec = -1; g_cur_sec_id = -1;
            emit("\tjal r0, .Lgb%d", g_unit);
            compile_nested_unit();
            emit("\t.text");                   /* the contained unit's data left the section */
            continue;
        }
        if (is_word(t, "end") && is_word(peek(1), "declaratives")) {
            if (!g_in_decl) die_at(t->line, "END DECLARATIVES without DECLARATIVES");
            if (cur_par >= 0) { end_par_label(); pc_para_end(cur_par); emit_exit_check(cur_par); }
            if (cur_sec >= 0) { end_sec_label(); pc_para_end(cur_sec); emit_exit_check(cur_sec); }
            cur_par = -1; cur_sec = -1; g_cur_sec_id = -1;
            advance(); advance(); expect_period();
            emit_label(Ldecl_end); g_in_decl = 0;
            continue;
        }

        /* the prescan's test: a verb or a scope terminator is no paragraph-name */
        if (((t->kind == T_WORD && !is_verb(t->s) && !is_terminator(t->s)) || (t->kind != T_WORD && at_para_name(t))) && (peek(1)->kind == T_PERIOD ||
            (is_word(peek(1), "section") && peek(2)->kind == T_PERIOD))) {
            Para *p = is_word(peek(1), "section") ? para_find(t->s) : para_find_in(t->s, cur_sec >= 0 ? cur_sec : -1);
            if (!p) p = para_find(t->s);
            if (!p) die_at(t->line, "internal: paragraph '%s' not prescanned", t->s);
            if (cur_par >= 0) { end_par_label(); pc_para_end(cur_par); emit_exit_check(cur_par); }
            if (p->is_section && cur_sec >= 0) { end_sec_label(); pc_para_end(cur_sec); emit_exit_check(cur_sec); }
            emit_para_label(p);
            g_cur_para = p;
            if (p->is_section) { cur_sec = p->id; cur_par = -1; g_cur_sec_id = p->id; } else cur_par = p->id;
            advance(); if (p->is_section) advance();
            expect_period();
            g_para_body_tp = g_tp;
            if (!p->is_section && is_altered_para(p->name)) {
                /* a paragraph ALTER names holds one sentence, a GO TO without
                 * DEPENDING (X3.23-1985 ALTER syntax rule 1) */
                int j = g_tp, ok = is_word(&g_tok[j], "go");
                if (ok) { j++; if (is_word(&g_tok[j], "to")) j++; if (g_tok[j].kind == T_WORD || g_tok[j].kind == T_NUM) j++; ok = g_tok[j].kind == T_PERIOD; j++; }
                if (ok) {
                    Tok *nx = &g_tok[j];
                    ok = nx->kind == T_EOF || (is_word(nx, "end") && (is_word(&g_tok[j + 1], "program") || is_word(&g_tok[j + 1], "declaratives") || is_word(&g_tok[j + 1], "function"))) ||
                         ((nx->kind == T_WORD || nx->kind == T_NUM) && (g_tok[j + 1].kind == T_PERIOD || is_word(&g_tok[j + 1], "section")) && !(nx->kind == T_WORD && is_verb(nx->s)));
                }
                if (!ok) die_at(p->line, "ALTER names the paragraph '%s', which must hold one sentence, a GO TO without DEPENDING (X3.23-1985 ALTER syntax rule 1)", p->oname[0] ? p->oname : p->name);
            }
            continue;
        }
        if (t->kind == T_WORD && !is_verb(t->s) && peek(1)->kind == T_NUM && is_word(peek(2), "section"))
            die_at(t->line, "section segment numbers are obsolete in COBOL 85; not supported");

        /* a sentence; after an error in it, the next one (ISSUES-41) */
        g_sentence_label = -1;
        jmp_buf jb, *outer = g_recover;
        int start = g_tp, noemit = g_noemit, slot = g_slot_base, cdepth = g_cond_depth, merge = g_is_merge, fdepth = g_fn_depth;
        /* the state a statement may leave half-changed when it fails: the
         * checking (an exception-checking PERFORM's implicit TURN), the
         * PERFORMs open around it (cobol ISSUES-94 E10) */
        static EcState ecs0;
        ecs_copy(&ecs0, &g_ecs);
        int necp = g_necp, ecp_handler = g_ecp_handler, npstk = g_npstk, necu = g_necu, in_finally = g_in_finally, in_ecpw = g_in_ecp_when;
        if (setjmp(jb)) {
            g_recover = outer;
            g_noemit = noemit; g_slot_base = slot; g_cond_depth = cdepth; g_is_merge = merge; g_fn_depth = fdepth;
            ecs_copy(&g_ecs, &ecs0);
            for (int c = NEC + necu; c < NEC + g_necu; c++) { g_ecs.on[c] = (unsigned char)g_ecs.user_on; g_ecs.loc[c] = (unsigned char)g_ecs.user_loc; }
            g_necp = necp; g_ecp_handler = ecp_handler; g_npstk = npstk; g_in_finally = in_finally; g_in_ecp_when = in_ecpw; g_wide = 0; g_fstmt = g_qstmt = 0; g_saw_wide = 0;
            g_abbr_op = -1; g_sentence_label = -1;
            memset(&g_stmt_calls, 0, sizeof g_stmt_calls); g_stmt_calls_on = 0; g_stmt_calls_hold = 0; g_hn_busy = 0;
            resync_sentence(start);
            continue;
        }
        g_recover = &jb;
        for (;;) {
            parse_statement();
            if (cur()->kind == T_PERIOD) { advance(); break; }
            if (cur()->kind == T_EOF) die_at(cur()->line, "missing '.' at the end of the last sentence");
            if (at_scope_end()) die_at(cur()->line, "'%s' without a matching statement", cur()->s);
        }
        g_recover = outer;
        if (g_sentence_label >= 0) emit_label(g_sentence_label);
    }
    if (cur_par >= 0) { end_par_label(); pc_para_end(cur_par); emit_exit_check(cur_par); }
    if (cur_sec >= 0) { end_sec_label(); pc_para_end(cur_sec); emit_exit_check(cur_sec); }
    pc_unit();
    sort_proc_check();

    emit_para_cells();                          /* this program's exit cells: its ids end at g_npara */
    emit(".Lgb%d:", g_unit);
    if (has_ext_file)
        for (int i = g_file_base; i < g_nfile; i++) {
            File *f = &g_files[i];
            if (!f->external) continue;
            char nm[80]; snprintf(nm, sizeof nm, "%s", f->name);
            emit_la("r3", lit_label((const unsigned char *)nm, (int)strlen(nm) + 1));
            char lab[32]; snprintf(lab, sizeof lab, ".Lf%d_%d", f->unit, i);
            emit_la("r4", lab);
            emit_call("cob_ext_file_exit");
        }
    emit("\tldw r3, sp+%d", SLOT_PBASE); emit_call("cob_perform_leave");
    if (g_std >= 2002) {
        char lab[32]; snprintf(lab, sizeof lab, ".Lact%d", g_unit);
        emit_la("r3", lab); emit("\tldw r4, sp+%d", SLOT_ACT); emit_call("cob_act_leave");
    }
    if (g_collate >= 0) { emit("\tldw r3, sp+%d", SLOT_COLL); emit_call("cob_set_collating"); }
    if (g_dp_comma) { emit("\tldw r3, sp+%d", SLOT_DP); emit_call("cob_set_decimal_point"); }
    if (g_currency_len > 1) { emit("\tldw r3, sp+%d", SLOT_CUR); emit_call("cob_restore_currency"); }
    else if (g_currency && g_currency != '$') { emit("\tldw r3, sp+%d", SLOT_CUR); emit_call("cob_set_currency"); }
    if (g_std >= 2002 && !g_is_function) {
        /* whether a result was put in place, for the caller's RETURNING */
        emit_la("r2", "cob_call_returned");
        if (g_prog_ret) { emit_li("r1", 1); emit("\tstw r2+0, r1"); } else emit("\tstw r2+0, r0");
    }
    if (g_uses_rc) { emit_la("r1", "cob_return_code"); emit("\tldw r1, r1+0"); }   /* RETURN-CODE, to the caller */
    else emit("\taddi r1, r0, 0");
    g_lw_final = 1; lw_inline_performs(); lw_resolve(0); g_lw_final = 0;   /* the islands, before the code is read as code (lower.h) */
    lr_unit(); lr_unit_saves(); lr_unit_restores();
    census_unit();
    emit("\tldw r13, sp+%d", SLOT_R13);
    emit("\tldw r12, sp+%d", SLOT_R12);
    emit("\tldw r11, sp+4");
    emit("\tldw lr, sp+0");
    emit("\taddi sp, sp, %d", g_frame);
    emit("\tjalr r0, r31, 0");
    lw_flush();                                 /* the islands' code, after the unit's (lower.h) */

    /* the unit joins the program registry at start-up (CALL identifier);
     * a function is invoked, never CALLed, and does not */
    if (!g_is_function) {
        char nm[130]; const char *rn = g_prog_as[0] ? g_prog_as : g_progid;   /* CALL finds it by its externalized name */
        int nl = (int)strlen(rn);
        memcpy(nm, rn, (size_t)nl); nm[nl] = 0;
        for (int i = 0; i < nl; i++) nm[i] = (char)tolower((unsigned char)nm[i]);
        const char *nlab = lit_label((const unsigned char *)nm, nl + 1);
        /* CANCEL: every WORKING-STORAGE record back to its initial state */
        emit("\t.p2align 2");
        emit(".Lcan%d:", g_unit);
        emit("\taddi sp, sp, -8");
        emit("\tstw sp+0, lr");
        for (int i = g_sym_base; i < g_nsym; i++) {
            Sym *s = &g_sym[i];
            if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0 || rec_indirect(s) || s->is_rc) continue;
            emit_la("r3", s->label);
            char il[80]; snprintf(il, sizeof il, "%s_i", s->label);
            emit_la("r4", il);
            emit_li("r5", s->image_size);
            emit_call("memcpy");
        }
        /* its internal files closed (2023 14.9.5.4 rule 9), and the programs
         * it contains canceled, last first (rule 4) -- each of those closes
         * its own files and cancels its own contained programs */
        for (int i = g_file_base; i < g_nfile; i++) {
            File *f = &g_files[i];
            if (f->external || f->org == COB_ORG_SORT || f->unit != g_unit) continue;
            emit_file_addr("r3", f);
            emit_call("cob_cancel_close");
        }
        for (int u = g_unit_counter; u > g_unit; u--)
            if (u < 4096 && g_unit_parent1[u] == g_unit + 1) emit("\tjal r31, .Lcan%d", u);
        emit("\tldw lr, sp+0");
        emit("\taddi sp, sp, 8");
        emit("\tjalr r0, r31, 0");
        emit("\t.p2align 2");
        emit(".Lreg%d:", g_unit);
        emit("\taddi sp, sp, -8");
        emit("\tstw sp+0, lr");
        emit_la("r3", nlab);
        emit_la("r4", entry);
        char cl[32]; snprintf(cl, sizeof cl, ".Lcan%d", g_unit);
        emit_la("r5", cl);
        emit_call(nested ? "cob_register_nested" : "cob_register");
        if (g_std >= 2002) {                   /* and its activation descriptor, for EC-PROGRAM-RECURSIVE-CALL */
            char al[32]; snprintf(al, sizeof al, ".Lact%d", g_unit);
            emit_la("r3", nlab); emit_la("r4", al);
            emit_call("cob_register_act");
        }
        emit("\tldw lr, sp+0");
        emit("\taddi sp, sp, 8");
        emit("\tjalr r0, r31, 0");
        emit("\t.section .init_array");
        emit("\t.p2align 2");
        emit("\t.word .Lreg%d", g_unit);
        emit("\t.text");
    }
    /* the contained programs this unit's statements may name: a CALL
     * identifier, CANCEL and the registry's lookups see them, and no
     * other contained program (2023 8.4.6.3) */
    if (g_any_nested) {                        /* none to see without contained programs */
    emit("\t.section .rodata");
    emit("\t.p2align 2");
    emit(".Lvis%d:", g_unit);
    if (g_unit < g_npnode)
        for (int t = 0; t < g_npnode; t++)
            if (g_pnode[t].parent >= 0 && !g_pnode[t].func && g_pnode[t].outer == g_pnode[g_unit].outer && pnode_visible(g_unit, t))
                emit("\t.word .Lcp%d", t);
    emit("\t.word 0");
    emit("\t.text");
    }

    if (!g_is_function && !g_main_done && !g_module && !g_udepth) {
        /* the first program of an executable is the main program (functions
         * defined ahead of it, as REPOSITORY requires, are not) */
        g_main_done = 1;
        emit("\t.globl main");
        emit("\t.p2align 2");
        emit("\t.type main,@function");
        emit("main:");
        emit("\taddi sp, sp, -16");
        emit("\tstw sp+0, lr");
        emit_call("cob_set_args");          /* r3 = argc, r4 = argv, as crt0 hands them */
        emit_call("cob_init");
        emit("\tjal r31, %s", entry);
        emit_li("r3", 0);
        emit_call("cob_stop_run");          /* flushes, restores the terminal, exits */
    }

    g_saw_end_program = 0;
    if (g_is_function) {
        if (!(at_word("end") && is_word(peek(1), "function"))) die_at(cur()->line, "a function ends with END FUNCTION %s", g_progid);
        advance(); advance();
        if (cur()->kind != T_WORD || strcmp(cur()->s, g_progid))
            die_at(cur()->line, "END FUNCTION names '%s' but the function is '%s'", cur()->s, g_progid);
        advance();
        if (cur()->kind != T_EOF) expect_period();
        g_saw_end_program = 1;
    } else if (accept_word("end")) {
        expect_word("program");
        if (cur()->kind == T_PERIOD || cur()->kind == T_EOF) {
            /* a bare END PROGRAM. -- RM/COBOL; the Open Systems AP and IN
             * modules end every program that way (PA's CRPACHK without
             * even the period, as the last line of the file) */
        } else {
            if (cur()->kind != T_WORD || strcmp(cur()->s, g_progid))
                die_at(cur()->line, "END PROGRAM names '%s' but the program is '%s'", cur()->s, g_progid);
            advance();
        }
        if (cur()->kind != T_EOF) expect_period();
        g_saw_end_program = 1;
    }
}
