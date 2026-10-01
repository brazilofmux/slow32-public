/* s32-cobc: the token cursor.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- cursor ---------------------------------------------------------- */

static int g_tp;

static Tok *cur(void)  { return &g_tok[g_tp]; }

static const char *diag_file(int line)
{
    if (g_tok && g_tp < g_ntok && g_tok[g_tp].file && g_tok[g_tp].line == line) return g_tok[g_tp].file;
    if (g_tok && g_tp > 0 && g_tp <= g_ntok && g_tok[g_tp - 1].file && g_tok[g_tp - 1].line == line) return g_tok[g_tp - 1].file;
    return g_tok_file ? g_tok_file : g_file;
}
static Tok *peek(int k){ int i = g_tp + k; if (i >= g_ntok) i = g_ntok - 1; return &g_tok[i]; }
static void advance(void) { if (g_tp < g_ntok - 1) g_tp++; }

static int is_word(Tok *t, const char *w) { return t->kind == T_WORD && !strcmp(t->s, w); }
/* does a program unit begin at t: IDENTIFICATION DIVISION, or -- the
 * header being optional from 2002 on (11.1.1) -- PROGRAM-ID. or
 * FUNCTION-ID. itself */
static int unit_start(Tok *t)
{
    if ((is_word(t, "identification") || is_word(t, "id")) && is_word(t + 1, "division")) return 1;
    return (is_word(t, "program-id") || is_word(t, "function-id")) && t[1].kind == T_PERIOD;
}
static int at_word(const char *w) { return is_word(cur(), w); }
static int accept_word(const char *w) { if (at_word(w)) { advance(); return 1; } return 0; }
static int at_op(const char *o) { return cur()->kind == T_OP && !strcmp(cur()->s, o); }

static const char *tok_desc(Tok *t)
{
    static char b[96];
    switch (t->kind) {
    case T_EOF:    return "end of file";
    case T_PERIOD: return "'.'";
    case T_STR:    snprintf(b, sizeof b, "literal '%.*s'", t->len > 40 ? 40 : t->len, t->s); return b;
    case T_PIC:    snprintf(b, sizeof b, "picture '%s'", t->s); return b;
    default:       snprintf(b, sizeof b, "'%s'", t->s); return b;
    }
}

static void expect_word(const char *w)
{
    if (!accept_word(w)) die_at(cur()->line, "expected '%s', found %s", w, tok_desc(cur()));
}

static void expect_period(void)
{
    if (cur()->kind != T_PERIOD) die_at(cur()->line, "expected '.', found %s", tok_desc(cur()));
    advance();
}

/* SPECIAL-NAMES SYMBOLIC CHARACTERS: figurative constants of the program's
 * own, each a character named by its ordinal position (1-based) in the
 * native character set */
static struct { char name[64]; int byte; } g_symch[32]; static int g_nsymch;
static int symch_find(const char *w)
{
    for (int i = 0; i < g_nsymch; i++) if (!strcmp(g_symch[i].name, w)) return g_symch[i].byte;
    return -1;
}

static int is_figurative(const char *w)
{
    static const char *figs[] = { "space", "spaces", "zero", "zeros", "zeroes",
        "low-value", "low-values", "high-value", "high-values", "quote", "quotes",
        "null", "nulls", NULL };
    for (int i = 0; figs[i]; i++) if (!strcmp(w, figs[i])) return 1;
    return symch_find(w) >= 0;
}

static int g_lowval, g_highval;
static int fig_byte(const char *w)
{
    int sc = symch_find(w);
    if (sc >= 0) return sc;
    if (!strncmp(w, "space", 5)) return ' ';
    if (!strncmp(w, "zero", 4)) return '0';
    if (!strncmp(w, "high", 4)) return g_highval;            /* X'FF', or the program collating sequence's last */
    if (!strncmp(w, "quote", 5)) return '"';
    if (!strncmp(w, "low", 3)) return g_lowval;              /* X'00', or the sequence's first */
    return 0;                                                 /* NULL */
}
