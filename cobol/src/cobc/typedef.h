/* s32-cobc: TYPEDEF and TYPE.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- TYPEDEF and TYPE (COBOL 2002 13.18.58, 13.18.57; cobol ISSUES-79) ----
 * The TYPE clause is "as though the data description identified by
 * type-name-1 had been coded in place of the TYPE clause", subordinate
 * level-numbers adjusted (13.18.57.4 rules 1-2): so it is expanded here,
 * over the tokens, before anything is parsed.  A TYPEDEF entry and its
 * subordinates are recorded -- its clauses without TYPEDEF, GLOBAL and the
 * name, its subordinate entries with levels relative to it -- and dropped:
 * a type declaration has no storage (13.18.58.4 rule 2).  Each TYPE [TO]
 * name is replaced by the type's clauses, and its subordinate entries
 * follow the entry.  Types are defined before use, and a type's own
 * TYPE clauses are expanded as it is recorded. */
typedef struct { char name[64]; Tok *clause; int nclause; Tok *sub; int nsub; int *sublvl; int level, strong, key; } TypeDef;
/* strong types (13.18.58, STRONG; cobol ISSUES-80): each strongly-typed
 * group -- the entry using the type, and each group inside it -- gets a
 * marker token carrying a key; the same key is the same type */
static char (*g_strong_key)[80]; static int g_nstrong_key, g_strong_cap;
static int strong_key(const char *k)
{
    for (int i = 0; i < g_nstrong_key; i++) if (!strcmp(g_strong_key[i], k)) return i;
    if (g_nstrong_key == g_strong_cap) { g_strong_cap = g_strong_cap ? g_strong_cap * 2 : 16; g_strong_key = realloc(g_strong_key, (size_t)g_strong_cap * sizeof *g_strong_key); }
    snprintf(g_strong_key[g_nstrong_key], sizeof g_strong_key[0], "%s", k);
    return g_nstrong_key++;
}

/* Concatenation expressions (2002 and 2023 8.8.3): literal & literal ...
 * is one literal of the operands' class (general rule 3), so the text is
 * joined here, once COPY and REPLACE are done, before anything parses it.
 * SPACE, ZERO and QUOTE take the other operand's class (rule 1a; both
 * figurative, alphanumeric).  HIGH-VALUE and LOW-VALUE are characters of
 * the program's collating sequence, not known yet: not implemented. */
static int concat_fig(const Tok *t)
{
    if (t->kind != T_WORD) return 0;
    if (!strcmp(t->s, "space") || !strcmp(t->s, "spaces")) return ' ';
    if (!strcmp(t->s, "zero") || !strcmp(t->s, "zeros") || !strcmp(t->s, "zeroes")) return '0';
    if (!strcmp(t->s, "quote") || !strcmp(t->s, "quotes")) return '"';
    if (!strncmp(t->s, "high-value", 10) || !strncmp(t->s, "low-value", 9))
        die_at(t->line, "%s as an operand of & is not implemented (its character is the collating sequence's)", t->orig ? t->orig : t->s);
    return 0;
}
static int concat_opnd(const Tok *t) { return t->kind == T_STR || concat_fig(t); }
static void join_concat(void)
{
    int w = 0, any = 0;
    for (int r = 0; r < g_ntok && !any; r++) if (g_tok[r].kind == T_OP && !strcmp(g_tok[r].s, "&")) any = 1;
    if (!any) return;
    int *map = xmalloc((size_t)(g_ntok + 1) * sizeof *map);      /* old position -> new, for the >>TURNs */
    for (int r = 0; r < g_ntok; r++) {
        Tok *t = &g_tok[r];
        map[r] = w;
        if (!(t->kind == T_OP && !strcmp(t->s, "&"))) { g_tok[w++] = *t; continue; }
        if (g_std < 2002) die_at(t->line, "the concatenation expression (&) is COBOL 2002; compile with -std=2002");
        if ((w > 1 && is_word(&g_tok[w - 2], "all")) || (r + 1 < g_ntok && is_word(&g_tok[r + 1], "all")))
            die_at(t->line, "a figurative constant with ALL is no operand of & (2023 8.8.3.2 rule 1)");
        if (!w || !concat_opnd(&g_tok[w - 1]) || r + 1 >= g_ntok || !concat_opnd(&g_tok[r + 1]))
            die_at(t->line, "& joins two literals (2023 8.8.3)");
        Tok a = g_tok[w - 1], b = g_tok[r + 1];
        int nat, bl;
        if (a.kind == T_STR && b.kind == T_STR) {
            if (a.nat != b.nat || a.boolv != b.boolv) die_at(t->line, "the operands of & are of one class: alphanumeric, national or boolean (2023 8.8.3.2 rule 1)");
            nat = a.nat; bl = a.boolv;
        } else if (a.kind == T_STR) { nat = a.nat; bl = a.boolv; }
        else if (b.kind == T_STR) { nat = b.nat; bl = b.boolv; }
        else nat = bl = 0;
        /* each operand's bytes in the result's class */
        unsigned char *buf = xmalloc((size_t)(a.len + b.len) * 2 + 4); int n = 0;
        const Tok *ops[2] = { &a, &b };
        for (int k = 0; k < 2; k++) {
            const Tok *o = ops[k];
            if (o->kind == T_STR) { memcpy(buf + n, o->s, (size_t)o->len); n += o->len; continue; }
            int ch = concat_fig(o);
            if (bl && ch != '0') die_at(t->line, "only ZERO stands for a boolean character in a concatenation");
            if (nat) { buf[n++] = 0; buf[n++] = (unsigned char)ch; }
            else buf[n++] = (unsigned char)ch;
        }
        buf[n] = 0;
        int pos = nat ? n / 2 : n;
        if (pos > 8191) die_at(t->line, "a concatenation of %d positions, more than 8,191 (2023 8.8.3.2 rules 2-4)", pos);
        if (pos > 160) bp(BP_E20_LONG_LITERAL, t->line);
        Tok *m = &g_tok[w - 1];
        if (m->kind != T_STR) { *m = b; m->line = a.line; m->file = a.file; m->after_comma = a.after_comma; }
        m->kind = T_STR; m->s = (char *)buf; m->len = n; m->nat = (unsigned char)nat; m->boolv = (unsigned char)bl; m->orig = NULL;
        map[r + 1] = w;
        r++;
    }
    for (int d = 0; d < g_ndir; d++) g_dir[d].pos = g_dir[d].pos < g_ntok ? map[g_dir[d].pos] : w;
    free(map);
    g_ntok = w;
}

static const char *strong_name(int key) { return g_strong_key[key]; }
static int g_type_recording_strong;     /* recording a STRONG type: its TYPE clauses may name strong types */
static TypeDef *g_types; static int g_ntypes, g_typecap;
static Tok *g_xt; static int g_nxt, g_xtcap;
static void xt_push(const Tok *t)
{
    if (g_nxt == g_xtcap) { g_xtcap = g_xtcap ? g_xtcap * 2 : 1024; g_xt = realloc(g_xt, (size_t)g_xtcap * sizeof *g_xt); }
    g_xt[g_nxt++] = *t;
}
static int tok_is(const Tok *t, const char *w) { return t->kind == T_WORD && !strcmp(t->s, w); }
static int tok_level(const Tok *t)
{
    if (t->kind != T_NUM || strlen(t->s) > 2) return -1;
    for (const char *k = t->s; *k; k++) if (!isdigit((unsigned char)*k)) return -1;
    return atoi(t->s);
}
static TypeDef *type_find(const char *name)
{
    for (int i = 0; i < g_ntypes; i++) if (!strcmp(g_types[i].name, name)) return &g_types[i];
    return NULL;
}
static Tok strong_tok(const Tok *like, int key)
{
    Tok t = *like; t.kind = T_WORD; t.s = "\001strong"; t.len = 7; t.orig = 0; t.strong = key + 1;
    return t;
}
static Tok level_tok(const Tok *like, int level)
{
    Tok t = *like; char b[8]; snprintf(b, sizeof b, "%02d", level);
    t.kind = T_NUM; t.s = xstrndup(b, (int)strlen(b)); t.len = (int)strlen(b); t.orig = 0;
    return t;
}
/* the words Report Writer's TYPE clause starts with (13.16.x TYPE) */
static int rw_type_word(const char *w)
{
    static const char *k[] = { "is", "report", "page", "control", "detail", "de", "rh", "ph", "ch", "cf", "pf", "rf", NULL };
    for (int i = 0; k[i]; i++) if (!strcmp(w, k[i])) return 1;
    return 0;
}
/* one entry's tokens [a, e] (e its period) to the output, TYPE clauses
 * expanded; the expansion's subordinate entries follow, at level + rel */
static void type_emit_entry(const Tok *tk, int a, int e, int level)
{
    /* the TYPE clause, if any: its type and its tokens [ti, tj] */
    TypeDef *used = NULL; int ti = -1, tj = -1;
    for (int i = a + 2; i < e; i++) {
        if (!tok_is(&tk[i], "type") || i + 1 >= e) continue;
        int j = i + 1;
        if (tok_is(&tk[j], "to") && j + 1 < e) j++;
        TypeDef *ty = tk[j].kind == T_WORD ? type_find(tk[j].s) : NULL;
        if (!ty && tk[j].kind == T_WORD && (j > i + 1 || !rw_type_word(tk[j].s)))
            /* TYPE TO, or a TYPE that is not Report Writer's: a type-name
             * declared before this entry -- a type does not refer to itself
             * or to one declared later (13.18.58.3 rule 2; ISSUES-94) */
            die_at(tk[j].line, "'%s' is not a type declared before this entry (TYPEDEF)", tk[j].s);
        if (!ty) continue;
        if (used) die_at(tk[i].line, "two TYPE clauses in one entry");
        used = ty; ti = i; tj = j;
    }
    if (!used) { for (int i = a; i <= e; i++) xt_push(&tk[i]); return; }
    /* the level and the name, then the type's clauses, then the entry's
     * own: where both say the same (VALUE), the entry's comes later and
     * is the one used (13.18.57.4 rule 3; cobol ISSUES-94 B9) */
    xt_push(&tk[a]); xt_push(&tk[a + 1]);
    if (used->strong) {
        /* a strong type at level 01, or inside a strong type (13.18.57.3 rule 6) */
        if (level != 1 && !g_type_recording_strong)
            die_at(tk[ti].line, "the strong type '%s' is used only at level 01 or inside a strong type (2023 13.18.57.3 rule 6)", used->name);
        Tok m = strong_tok(&tk[ti], used->key); xt_push(&m);
    }
    if (used->nsub && level != 1 && level != 77) {
        /* a group type is aligned as a level 1 item (rule 2d; B10) */
        Tok m = tk[ti]; m.kind = T_WORD; m.s = "\001lvl1"; m.len = 5; m.orig = 0; xt_push(&m);
    }
    for (int k = 0; k < used->nclause; k++) xt_push(&used->clause[k]);
    for (int i = a + 2; i <= e; i++) if (i < ti || i > tj) xt_push(&tk[i]);
    if (level == 77 && used->nsub) die_at(tk[a].line, "a level 77 item takes an elementary type (2023 13.18.57.3 rule 7)");
    for (int k = 0; k < used->nsub; k++) {
        if (used->sublvl[k] >= 0) {
            int lv = used->sublvl[k] >= 66 ? used->sublvl[k] : level + used->sublvl[k];
            if (lv > 49 && lv < 66) die_at(tk[a].line, "the type '%s' expands past level 49 here, which is not implemented", used->name);
            Tok lt = level_tok(&used->sub[k], lv); xt_push(&lt);
        } else xt_push(&used->sub[k]);
    }
}
static void expand_types(void)
{
    if (g_std < 2002) return;
    int any = 0;
    for (int i = 0; i < g_ntok; i++)
        if (tok_is(&g_tok[i], "typedef") || (tok_is(&g_tok[i], "type") && i + 1 < g_ntok && tok_is(&g_tok[i + 1], "to"))) { any = 1; break; }
    if (!any) return;
    int *map = xmalloc((size_t)(g_ntok + 1) * sizeof *map);
    g_nxt = 0; g_ntypes = 0;
    int in_data = 0;
    for (int i = 0; i < g_ntok; ) {
        Tok *t = &g_tok[i];
        if (tok_is(t, "data") && i + 1 < g_ntok && tok_is(&g_tok[i + 1], "division")) in_data = 1;
        if (tok_is(t, "procedure") && i + 1 < g_ntok && tok_is(&g_tok[i + 1], "division")) in_data = 0;
        int lv = tok_level(t);
        int at_entry = in_data && lv >= 1 && i > 0 && g_tok[i - 1].kind == T_PERIOD && i + 1 < g_ntok;
        if (!at_entry) { map[i] = g_nxt; xt_push(t); i++; continue; }
        int e = i; while (e < g_ntok && g_tok[e].kind != T_PERIOD && g_tok[e].kind != T_EOF) e++;
        int td = -1;
        for (int k = i + 1; k < e; k++) if (tok_is(&g_tok[k], "typedef")) td = k;
        if (td < 0) {
            for (int k = i; k <= e && k < g_ntok; k++) map[k] = g_nxt;
            type_emit_entry(g_tok, i, e, lv);
            i = e + 1;
            continue;
        }
        /* a type declaration: recorded, not emitted */
        int strong = td + 1 < e && tok_is(&g_tok[td + 1], "strong");
        if (g_tok[i + 1].kind != T_WORD) die_at(t->line, "a TYPEDEF entry needs a name");
        if (!(td == i + 2 || (td == i + 3 && tok_is(&g_tok[i + 2], "is"))))
            die_at(g_tok[td].line, "'%s': TYPEDEF comes immediately after the data-name (2023 13.16.3 rule 4)", g_tok[i + 1].s);
        if (lv != 1 && lv != 77) die_at(t->line, "a type declaration here is a level 01 or 77 entry");
        if (g_ntypes == g_typecap) { g_typecap = g_typecap ? g_typecap * 2 : 16; g_types = realloc(g_types, (size_t)g_typecap * sizeof *g_types); }
        TypeDef *ty = &g_types[g_ntypes]; memset(ty, 0, sizeof *ty);
        snprintf(ty->name, sizeof ty->name, "%s", g_tok[i + 1].s);
        ty->level = lv; ty->strong = strong;
        if (strong) ty->key = strong_key(ty->name);
        g_type_recording_strong = strong;
        /* its own clauses, TYPE clauses expanded, without TYPEDEF, IS
         * before it, and GLOBAL */
        int save = g_nxt;
        type_emit_entry(g_tok, i, e, lv);
        int n = g_nxt - save;
        ty->clause = xmalloc((size_t)(n + 1) * sizeof *ty->clause);
        for (int k = save + 2; k < g_nxt - 1; k++) {           /* past the level and name, before the period */
            Tok *c = &g_xt[k];
            if (tok_is(c, "typedef") || tok_is(c, "global") || (strong && tok_is(c, "strong"))) continue;
            if (tok_is(c, "is") && k + 1 < g_nxt && tok_is(&g_xt[k + 1], "typedef")) continue;
            ty->clause[ty->nclause++] = *c;
        }
        /* a TYPE clause inside a type: its subordinates follow, already emitted after the period */
        int tail = g_nxt;
        for (int k = save; k < g_nxt; k++) if (g_xt[k].kind == T_PERIOD) { tail = k + 1; break; }
        /* the subordinate entries: to the next entry at this level or above */
        int j = e + 1, subst = tail;              /* a TYPE clause's expansion is its first subordinates */
        while (j < g_ntok) {
            int sl = tok_level(&g_tok[j]);
            if (sl < 0 || g_tok[j - 1].kind != T_PERIOD) break;
            if (sl != 66 && sl != 88 && sl <= lv) break;
            if (sl == 77) break;
            int se = j; while (se < g_ntok && g_tok[se].kind != T_PERIOD && g_tok[se].kind != T_EOF) se++;
            type_emit_entry(g_tok, j, se, sl);
            j = se + 1;
        }
        int ns = g_nxt - subst;
        ty->sub = xmalloc((size_t)(ns + 1) * sizeof *ty->sub); ty->sublvl = xmalloc((size_t)(ns + 1) * sizeof *ty->sublvl);
        for (int k = 0; k < ns; k++) {
            Tok *c = &g_xt[subst + k];
            ty->sub[ty->nsub] = *c;
            /* a level-number opens each subordinate entry: made relative */
            int at = (subst + k == subst) || g_xt[subst + k - 1].kind == T_PERIOD;
            int l2 = at ? tok_level(c) : -1;
            ty->sublvl[ty->nsub] = l2 < 0 ? -1 : (l2 == 66 || l2 == 88) ? l2 : l2 - lv;
            ty->nsub++;
        }
        g_type_recording_strong = 0;
        if (strong && !ns) die_at(t->line, "TYPEDEF STRONG: '%s' is not a group (2023 13.18.58.3 rule 1)", ty->name);
        if (strong) {
            /* each subordinate group of a strong type is strong too: a marker,
             * keyed type#n, after its level and name */
            Tok *ns2 = xmalloc((size_t)(ty->nsub * 2 + 1) * sizeof *ns2); int *nl2 = xmalloc((size_t)(ty->nsub * 2 + 1) * sizeof *nl2);
            int m = 0, gn = 0;
            for (int k = 0; k < ty->nsub; k++) {
                ns2[m] = ty->sub[k]; nl2[m] = ty->sublvl[k]; m++;
                if (ty->sublvl[k] >= 0 && ty->sublvl[k] < 66 && k + 1 < ty->nsub) {
                    int e2 = k; while (e2 < ty->nsub && ty->sub[e2].kind != T_PERIOD) e2++;
                    int nextlv = -1;
                    for (int q = e2 + 1; q < ty->nsub; q++) if (ty->sublvl[q] >= 0) { nextlv = ty->sublvl[q]; break; }
                    int already = 0;
                    for (int q = k + 1; q < e2; q++) if (ty->sub[q].strong) already = 1;
                    if (nextlv > ty->sublvl[k] && nextlv < 66 && !already && k + 1 < e2) {
                        char key[80]; snprintf(key, sizeof key, "%s#%d", ty->name, ++gn);
                        ns2[m] = ty->sub[k + 1]; nl2[m] = -1; m++;         /* the name */
                        ns2[m] = strong_tok(&ty->sub[k], strong_key(key)); nl2[m] = -1; m++;
                        k++;
                    }
                }
            }
            ty->sub = ns2; ty->sublvl = nl2; ty->nsub = m;
        }
        g_nxt = save;                                   /* no storage: nothing of it stays */
        g_ntypes++;
        for (int k = i; k < j; k++) map[k] = g_nxt;
        i = j;
    }
    map[g_ntok] = g_nxt;
    for (int d = 0; d < g_ndir; d++) g_dir[d].pos = g_dir[d].pos <= g_ntok ? map[g_dir[d].pos] : g_nxt;
    free(g_tok); g_tok = g_xt; g_ntok = g_nxt; g_tcap = g_xtcap;
    g_xt = NULL; g_nxt = g_xtcap = 0;
    free(map);
}

static void tokenize(void)
{
    g_tok_file = g_file;
    strip_comment_entries(g_lines, g_nlines);
    {
        SrcLine *tl; int tn;
        text_manipulation(g_lines, g_nlines, &tl, &tn, 1);
        tokenize_lines(tl, tn);
    }
    expand_sql_includes();
    {
        int w = 0;
        /* Debugging lines (D in column 7) are compiled under WITH DEBUGGING
         * MODE and are comments otherwise (X3.23-1985 VI-10, SOURCE-COMPUTER
         * rules 4-5).  The clause is found here, before parsing, because it
         * decides which tokens exist, and is taken for the whole source file:
         * exact for one program and the programs nested in it. */
        int mode = 0, last = -1;
        for (int r = 0; r + 1 < g_ntok; r++)
            if (!g_tok[r].dbg && is_word(&g_tok[r], "debugging") && is_word(&g_tok[r + 1], "mode")) {
                mode = 1; bp(BP_O12_DEBUG_LINES, g_tok[r].line);
            }
        for (int r = 0; r < g_ntok; r++) {
            if (g_tok[r].dbg && g_tok[r].line != last) { last = g_tok[r].line; if (!mode) bp(BP_O12_DEBUG_LINES, last); }
            if (g_tok[r].kind == T_DIR) {
                /* a >>TURN: the parser applies it on reaching this point */
                if (g_ndir == g_dircap) { g_dircap = g_dircap ? 2 * g_dircap : 16; g_dir = realloc(g_dir, (size_t)g_dircap * sizeof *g_dir); }
                g_dir[g_ndir].pos = w; g_dir[g_ndir].tok = g_tok[r]; g_ndir++;
                continue;
            }
            if (mode || !g_tok[r].dbg) g_tok[w++] = g_tok[r];
        }
        g_ntok = w;
    }
    apply_decimal_point();
    join_concat();
    push_tok(T_EOF, g_nlines ? g_lines[g_nlines - 1].line : 1, "", 0);
}
