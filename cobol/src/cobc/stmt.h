/* s32-cobc: statements, paragraphs.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ====================================================================== */
/* Procedure Division: statements                                          */
/* ====================================================================== */

static int is_verb(const char *w)
{
    static const char *verbs[] = { "accept", "add", "alter", "call", "cancel", "close",
        "compute", "continue", "delete", "disable", "display", "divide", "enable",
        "enter", "evaluate", "exit", "generate", "go", "goback", "if", "initialize",
        "initiate", "inspect", "merge", "move", "multiply", "open", "perform", "purge",
        "read", "receive", "release", "return", "rewrite", "search", "send", "set",
        "sort", "start", "stop", "string", "subtract", "suppress", "terminate", "unlock",
        "unstring", "use", "write", "next", NULL };
    for (int i = 0; verbs[i]; i++) if (!strcmp(w, verbs[i])) return 1;
    if (g_std >= 2002 && (!strcmp(w, "raise") || !strcmp(w, "resume") || !strcmp(w, "allocate") || !strcmp(w, "free")))
        return 1;                                   /* COBOL 2002's verbs */
    return 0;
}

static int is_terminator(const char *w)
{
    static const char *t[] = { "else", "end-if", "end-perform", "when", "end-evaluate",
        "end-read", "end-write", "end-add", "end-subtract", "end-multiply", "end-divide",
        "end-compute", "end-call", "end-string", "end-unstring", "end-search", "end-start",
        "end-delete", "end-rewrite", "end-return", "end-accept", "end-display", NULL };
    for (int i = 0; t[i]; i++) if (!strcmp(w, t[i])) return 1;
    if (g_std >= 2002 && !strcmp(w, "finally")) return 1;     /* an exception-checking PERFORM's (cobol ISSUES-89) */
    return 0;
}

static int at_scope_end(void)
{
    Tok *t = cur();
    if (t->kind == T_PERIOD || t->kind == T_EOF) return 1;
    if (t->kind != T_WORD) return 0;
    if (!strcmp(t->s, "not") && (is_word(peek(1), "on") || is_word(peek(1), "size") ||
                                 is_word(peek(1), "at") || is_word(peek(1), "invalid") ||
                                 is_word(peek(1), "end") || is_word(peek(1), "overflow") ||
                                 is_word(peek(1), "exception") || is_word(peek(1), "end-of-page") ||
                                 is_word(peek(1), "eop"))) return 1;
    return is_terminator(t->s);
}

/* the operand list of a statement continues while the next token can
 * start an operand and is not a verb or a clause word */
static int at_operand(void)
{
    Tok *t = cur();
    if (t->kind == T_STR || t->kind == T_NUM) return 1;
    if (t->kind != T_WORD) return 0;
    if (is_verb(t->s) || is_terminator(t->s)) return 0;
    static const char *clause[] = { "to", "from", "by", "into", "giving", "rounded", "on",
        "size", "upon", "with", "thru", "through", "until", "varying", "times", "after",
        "before", "remainder", "depending", "corresponding", "corr", "then", "and", "or",
        "is", "not", "up", "down", "delimited", "pointer", "overflow", "at", "next", "record",
        "key", "invalid", "advancing", "lines", "line", "page", "input", "output", "i-o",
        "extend", "lock", "rewind", "end-string", "returning", "reference", "content",
        "exception", "end-call", "also", "when", "other", "tallying", "replacing", "converting",
        "characters", "leading", "first", "initial", "true", "false", "any", "end-search", NULL };
    /* OTHER, TRUE, FALSE, ANY became reserved with COBOL-85; RM/COBOL 2
     * programs declare data items by those names (PAACEMP: MOVE ... TO OTHER (QY)).
     * A declared item wins over the clause word. */
    if ((!strcmp(t->s, "other") || !strcmp(t->s, "true") || !strcmp(t->s, "false") || !strcmp(t->s, "any"))
        && sym_lookup_quiet(t->s)) return 1;
    for (int i = 0; clause[i]; i++) if (!strcmp(t->s, clause[i])) return 0;
    return 1;
}

static void parse_statement(void);
static void parse_statements(void)
{
    while (!at_scope_end()) parse_statement();
}

static int g_sentence_label = -1;   /* NEXT SENTENCE target, made on demand */

/* ---- paragraphs ------------------------------------------------------- */

typedef struct { char name[64], oname[64]; int id, is_section, line, section, unit, in_decl; } Para;   /* oname: as written */   /* section: id of the enclosing section, 0 none; unit: where it is */
static Para *g_para; static int g_npara, g_pcap;

static int g_cur_sec_id;            /* the section being parsed (or prescanned), -1 outside one */

/* a paragraph name may be repeated in different sections; an unqualified
 * reference means the one in the current section, else the only one */
static Para *para_find_in(const char *name, int section)
{
    for (int i = g_para_base; i < g_npara; i++)
        if (!strcmp(g_para[i].name, name) && (g_para[i].is_section || g_para[i].section == section)) return &g_para[i];
    return NULL;
}

static int g_para_ambiguous;        /* para_find: the name is in several sections, none the current one (2023 8.4.2.2 rule 6) */
static Para *para_find(const char *name)
{
    Para *found = NULL; int n = 0;
    g_para_ambiguous = 0;
    for (int i = g_para_base; i < g_npara; i++) {
        if (strcmp(g_para[i].name, name)) continue;
        if (g_para[i].is_section || g_para[i].section == g_cur_sec_id) return &g_para[i];
        if (!found) found = &g_para[i];
        n++;
    }
    if (n > 1) g_para_ambiguous = 1;
    return found;
}

static int g_prescan_decl;          /* the prescan is between DECLARATIVES and END DECLARATIVES */
static Para *para_add(const char *name, const char *oname, int is_section, int line)
{
    user_word(name, line, is_section ? "a section" : "a paragraph");
    if (is_section) { for (int i = g_para_base; i < g_npara; i++) if (!strcmp(g_para[i].name, name)) die_at(line, "the procedure-name '%s' is declared twice", name); }
    else if (para_find_in(name, g_cur_sec_id)) die_at(line, "the paragraph '%s' is declared twice in the same section", name);
    if (g_npara == g_pcap) { g_pcap = g_pcap ? g_pcap * 2 : 64; g_para = realloc(g_para, g_pcap * sizeof *g_para); }
    Para *p = &g_para[g_npara];
    snprintf(p->name, sizeof p->name, "%s", name);
    snprintf(p->oname, sizeof p->oname, "%s", oname);
    p->id = g_npara + 1; p->is_section = is_section; p->line = line; p->unit = g_unit; p->in_decl = g_prescan_decl;
    p->section = is_section ? 0 : (g_cur_sec_id >= 0 ? g_cur_sec_id : 0);
    if (is_section) g_cur_sec_id = p->id;
    g_npara++;
    return p;
}

/* reg = the address of a paragraph's or section's exit cell: a word each,
 * an array for each program (.Lpx<unit>, written where its procedure
 * division ends), which the runtime keeps -- the place of the PERFORM
 * frame waiting on that exit, or zero (libcob, cob_perform_push).  Ids
 * are a program's own: they begin again at the next program in the file,
 * and a contained program's are given to the one after it, so the unit
 * is part of the name -- a containing program's USE procedure is
 * performed from the contained one by its own unit's cell. */
static int *g_px_max, *g_px_size, g_px_cap;     /* by unit: the largest id asked for; the cells written (0: not yet) */
static void px_room(int unit)
{
    if (unit < g_px_cap) return;
    int n = unit + 16;
    g_px_max = realloc(g_px_max, n * sizeof *g_px_max); g_px_size = realloc(g_px_size, n * sizeof *g_px_size);
    if (!g_px_max || !g_px_size) die_at(cur()->line, "out of memory");
    for (int i = g_px_cap; i < n; i++) g_px_max[i] = g_px_size[i] = 0;
    g_px_cap = n;
}
static void emit_para_cell(const char *reg, int unit, int id)
{
    px_room(unit);
    if (id < 1 || (g_px_size[unit] && id >= g_px_size[unit])) die_at(cur()->line, "internal: a paragraph's exit cell outside its program's");
    if (id > g_px_max[unit]) g_px_max[unit] = id;
    char l[32]; snprintf(l, sizeof l, ".Lpx%d", unit);
    emit_la_off(reg, l, 4 * id);
}
/* the cells of the program whose procedure division ends here */
static void emit_para_cells(void)
{
    px_room(g_unit);
    if (g_px_max[g_unit] > g_npara) die_at(cur()->line, "internal: a paragraph's exit cell outside its program's");
    g_px_size[g_unit] = g_npara + 1;
    emit("\t.data"); emit("\t.p2align 2"); emit(".Lpx%d:", g_unit); emit("\t.space %d", 4 * (g_npara + 1)); emit("\t.text");
}
static void emit_para_label(Para *p) { emit(".Lp%d_%d:\t# %s%s", g_unit, p->id, p->name, p->is_section ? " section" : ""); }

/* prescan the Procedure Division for paragraph and section headers */
/* ALTER (obsolete in the 1985 text; NC302M, NC303M and NC401M use it): a
 * paragraph named in an ALTER statement holds one GO TO, which jumps
 * through a cell the ALTER rewrites.  The names are gathered before the
 * procedure division is compiled; the cells are laid out with the unit's
 * data, each initialised to the GO TO's own target (or 0 for a bare GO TO). */
static char g_altname[64][64]; static int g_naltname;
static struct { int para, target; } g_altcell[64]; static int g_naltcell;
static Para *g_cur_para;
static int is_altered_para(const char *name)
{
    for (int i = 0; i < g_naltname; i++) if (!strcmp(g_altname[i], name)) return 1;
    return 0;
}

static void prescan_paragraphs(int from)
{
    int sentence_start = 1;
    g_prescan_decl = 0;
    g_cur_sec_id = -1;
    for (int i = from; i < g_ntok; i++) {
        Tok *t = &g_tok[i];
        if (t->kind == T_EOF) break;
        if (t->kind == T_WORD && !strcmp(t->s, "alter")) {
            /* ALTER p1 TO [PROCEED TO] p2 [p3 TO [PROCEED TO] p4]... -- anywhere
             * in a sentence: GENSRT19's sit inside IF/ELSE (GitHub #36).  ALTER
             * is a reserved word, so the match cannot be a data-name. */
            for (int j = i + 1; j + 2 < g_ntok && g_tok[j].kind == T_WORD && is_word(&g_tok[j + 1], "to"); ) {
                if (g_naltname < 64) snprintf(g_altname[g_naltname++], 64, "%s", g_tok[j].s);
                j += 2;
                if (is_word(&g_tok[j], "proceed") && is_word(&g_tok[j + 1], "to")) j += 2;
                if (g_tok[j].kind != T_WORD) break;
                j++;
            }
        }
        if (sentence_start && t->kind == T_NUM && !strchr(t->s, '.') && !strchr(t->s, '+') && !strchr(t->s, '-')) {
            /* a procedure-name of digits only (NC107A's paragraphs 3, 4, 5) */
            if (g_tok[i + 1].kind == T_PERIOD) para_add(t->s, tok_orig(t), 0, t->line);
            else if (is_word(&g_tok[i + 1], "section") && g_tok[i + 2].kind == T_PERIOD) para_add(t->s, tok_orig(t), 1, t->line);
        }
        if (sentence_start && t->kind == T_WORD && !is_verb(t->s) && !is_terminator(t->s)) {
            if (!strcmp(t->s, "declaratives")) { g_prescan_decl = 1; }
            else if (!strcmp(t->s, "end") && (is_word(&g_tok[i + 1], "declaratives") || is_word(&g_tok[i + 1], "program"))) {
                if (is_word(&g_tok[i + 1], "program")) break;
                g_prescan_decl = 0;
            }
            else if (unit_start(t)) break;   /* a contained program's */
            else if (g_tok[i + 1].kind == T_PERIOD) { para_add(t->s, tok_orig(t), 0, t->line); }
            else if (is_word(&g_tok[i + 1], "section") && g_tok[i + 2].kind == T_PERIOD) para_add(t->s, tok_orig(t), 1, t->line);
        }
        sentence_start = (t->kind == T_PERIOD);
    }
    g_cur_sec_id = -1;
}

/* procedure-name [OF|IN section-name] */
/* a token that may name a procedure: a word, or a number of digits only */
static int at_para_name(Tok *t)
{
    if (t->kind == T_WORD) return 1;
    return t->kind == T_NUM && !strchr(t->s, '.') && !strchr(t->s, '+') && !strchr(t->s, '-');
}

static Para *expect_para(void)
{
    Tok *t = cur();
    if (!at_para_name(t)) die_at(t->line, "expected a procedure-name, found %s", tok_desc(t));
    Para *p;
    if (is_word(peek(1), "of") || is_word(peek(1), "in")) {
        Tok *q = peek(2);
        if (q->kind != T_WORD) die_at(t->line, "expected a section-name after OF/IN");
        Para *sec = NULL;
        for (int i = g_para_base; i < g_npara; i++) if (g_para[i].is_section && !strcmp(g_para[i].name, q->s)) sec = &g_para[i];
        if (!sec) die_at(q->line, "'%s' is not a section", q->s);
        p = para_find_in(t->s, sec->id);
        if (!p || p->is_section) die_at(t->line, "'%s' is not a paragraph of section '%s'", t->s, q->s);
        advance(); advance();
    } else {
        p = para_find(t->s);
        if (!p) die_at(t->line, "'%s' is not a paragraph or section", t->s);
        if (g_para_ambiguous) die_at(t->line, "'%s' is a paragraph of several sections, and not of this one: qualify it with OF/IN section-name (2023 8.4.2.2 rule 6)", t->s);
    }
    advance();
    return p;
}
