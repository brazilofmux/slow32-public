/* s32-cobc: IF, DECLARATIVES, exception conditions, PERFORM.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- IF ---------------------------------------------------------------- */

/* an IF branch: NEXT SENTENCE, or one or more statements -- not none
 * (statement-1 and statement-2 are required: 85 IF syntax rule 1; 2023
 * 14.9.19.3 rule 1); returns whether it was NEXT SENTENCE */
static int parse_branch_body(void)
{
    if (at_word("next")) {
        advance(); expect_word("sentence");
        if (g_sentence_label < 0) g_sentence_label = new_label();
        emit_jump(g_sentence_label);
        return 1;
    }
    if (at_scope_end() || at_word("else") || at_word("end-if"))
        die_at(cur()->line, "IF: the condition, and ELSE, are each followed by a statement or NEXT SENTENCE (%s)",
               g_std < 2002 ? "X3.23-1985 IF syntax rule 1" : "2023 14.9.19.3 rule 1");
    parse_statements();
    return 0;
}

static void parse_if(void)
{
    Cond *c = parse_cond();
    accept_word("then");
    int Lelse = new_label();
    cond_jump_false(c, Lelse);
    int ns = parse_branch_body();
    if (accept_word("else")) {
        int Lend = new_label();
        emit_jump(Lend);
        emit_label(Lelse);
        ns |= parse_branch_body();
        emit_label(Lend);
    } else emit_label(Lelse);
    if (ns && at_word("end-if"))
        die_at(cur()->line, "IF with NEXT SENTENCE ends at the period, not END-IF (%s)", g_std < 2002 ? "X3.23-1985 IF syntax rule 3" : "2023 14.9.19 format 2");
    accept_word("end-if");
}

/* ---- DECLARATIVES: USE AFTER ERROR PROCEDURE --------------------------- */

static File *expect_file(void);
static void emit_file_addr(const char *reg, File *f);
static File *g_io_file;             /* the file the statement being parsed acts on, for the USE dispatch */

/* ---- exception conditions (COBOL 2002 14.6.13; cobol ISSUES-53) ------- */

/* ISO/IEC 1989:2023 Table 13: every exception-name, its level (1 EC-ALL,
 * 2 a group, 3 a condition) and its fatality, 'F' fatal, 'N' nonfatal,
 * 'I' implementor-defined (taken here as nonfatal).  EC-USER-suffix names
 * are the user's, level 3 and nonfatal, and are added as they are met. */
static const struct { const char *name; char level; char fatal; } g_ec[] = {
    { "EC-ALL", 1, 0 },
    { "EC-ARGUMENT", 2, 0 },
    { "EC-ARGUMENT-FUNCTION", 3, 'F' },
    { "EC-ARGUMENT-IMP", 3, 'I' },
    { "EC-BOUND", 2, 0 },
    { "EC-BOUND-FUNC-RET-VALUE", 3, 'N' },
    { "EC-BOUND-IMP", 3, 'I' },
    { "EC-BOUND-ODO", 3, 'F' },
    { "EC-BOUND-OVERFLOW", 3, 'N' },
    { "EC-BOUND-PTR", 3, 'F' },
    { "EC-BOUND-REF-MOD", 3, 'F' },
    { "EC-BOUND-SET", 3, 'N' },
    { "EC-BOUND-SUBSCRIPT", 3, 'F' },
    { "EC-BOUND-TABLE-LIMIT", 3, 'F' },
    { "EC-CONTINUE", 2, 0 },
    { "EC-CONTINUE-IMP", 3, 'I' },
    { "EC-CONTINUE-LESS-THAN-ZERO", 3, 'N' },
    { "EC-DATA", 2, 0 },
    { "EC-DATA-CONVERSION", 3, 'N' },
    { "EC-DATA-IMP", 3, 'I' },
    { "EC-DATA-INCOMPATIBLE", 3, 'F' },
    { "EC-DATA-NOT-FINITE", 3, 'F' },
    { "EC-DATA-OVERFLOW", 3, 'F' },
    { "EC-DATA-PTR-NULL", 3, 'F' },
    { "EC-EXTERNAL", 2, 0 },
    { "EC-EXTERNAL-DATA-MISMATCH", 3, 'F' },
    { "EC-EXTERNAL-FILE-MISMATCH", 3, 'F' },
    { "EC-EXTERNAL-FORMAT-CONFLICT", 3, 'F' },
    { "EC-EXTERNAL-IMP", 3, 'I' },
    { "EC-FLOW", 2, 0 },
    { "EC-FLOW-APPLY-COMMIT", 3, 'F' },
    { "EC-FLOW-COMMIT", 3, 'F' },
    { "EC-FLOW-GLOBAL-EXIT", 3, 'F' },
    { "EC-FLOW-GLOBAL-GOBACK", 3, 'F' },
    { "EC-FLOW-IMP", 3, 'I' },
    { "EC-FLOW-RELEASE", 3, 'F' },
    { "EC-FLOW-REPORT", 3, 'F' },
    { "EC-FLOW-RETURN", 3, 'F' },
    { "EC-FLOW-ROLLBACK", 3, 'F' },
    { "EC-FLOW-SEARCH", 3, 'F' },
    { "EC-FLOW-USE", 3, 'F' },
    { "EC-FUNCTION", 2, 0 },
    { "EC-FUNCTION-ARG-OMITTED", 3, 'F' },
    { "EC-FUNCTION-IMP", 3, 'I' },
    { "EC-FUNCTION-NOT-FOUND", 3, 'F' },
    { "EC-FUNCTION-PTR-INVALID", 3, 'F' },
    { "EC-FUNCTION-PTR-NULL", 3, 'F' },
    { "EC-I-O", 2, 0 },
    { "EC-I-O-AT-END", 3, 'N' },
    { "EC-I-O-EOP", 3, 'N' },
    { "EC-I-O-EOP-OVERFLOW", 3, 'N' },
    { "EC-I-O-FILE-SHARING", 3, 'N' },
    { "EC-I-O-IMP", 3, 'I' },
    { "EC-I-O-INVALID-KEY", 3, 'N' },
    { "EC-I-O-LINAGE", 3, 'F' },
    { "EC-I-O-LOGIC-ERROR", 3, 'F' },
    { "EC-I-O-PERMANENT-ERROR", 3, 'F' },
    { "EC-I-O-RECORD-CONTENT", 3, 'F' },
    { "EC-I-O-RECORD-OPERATION", 3, 'N' },
    { "EC-I-O-WARNING", 3, 'N' },
    { "EC-IMP", 2, 0 },
    /* { "EC-IMP-suffix", 3, 'I' },  pattern entry (implementor/user supplies suffix), not a literal name */
    { "EC-LOCALE", 2, 0 },
    { "EC-LOCALE-IMP", 3, 'I' },
    { "EC-LOCALE-INCOMPATIBLE", 3, 'F' },
    { "EC-LOCALE-INVALID", 3, 'F' },
    { "EC-LOCALE-INVALID-PTR", 3, 'F' },
    { "EC-LOCALE-MISSING", 3, 'F' },
    { "EC-LOCALE-SIZE", 3, 'F' },
    { "EC-MCS", 2, 0 },
    { "EC-MCS-ABNORMAL-TERMINATION", 3, 'N' },
    { "EC-MCS-IMP", 3, 'I' },
    { "EC-MCS-INVALID-TAG", 3, 'N' },
    { "EC-MCS-MESSAGE-LENGTH", 3, 'N' },
    { "EC-MCS-NO-REQUESTER", 3, 'N' },
    { "EC-MCS-NO-SERVER", 3, 'N' },
    { "EC-MCS-NORMAL-TERMINATION", 3, 'N' },
    { "EC-MCS-REQUESTOR-FAILED", 3, 'N' },
    { "EC-OO", 2, 0 },
    { "EC-OO-ARG-OMITTED", 3, 'F' },
    { "EC-OO-CONFORMANCE", 3, 'F' },
    { "EC-OO-EXCEPTION", 3, 'F' },
    { "EC-OO-IMP", 3, 'I' },
    { "EC-OO-METHOD", 3, 'F' },
    { "EC-OO-NULL", 3, 'F' },
    { "EC-OO-RESOURCE", 3, 'F' },
    { "EC-OO-UNIVERSAL", 3, 'F' },
    { "EC-ORDER", 2, 0 },
    { "EC-ORDER-IMP", 3, 'I' },
    { "EC-ORDER-NOT-SUPPORTED", 3, 'F' },
    { "EC-OVERFLOW", 2, 0 },
    { "EC-OVERFLOW-IMP", 3, 'I' },
    { "EC-OVERFLOW-STRING", 3, 'N' },
    { "EC-OVERFLOW-UNSTRING", 3, 'N' },
    { "EC-PROGRAM", 2, 0 },
    { "EC-PROGRAM-ARG-MISMATCH", 3, 'F' },
    { "EC-PROGRAM-ARG-OMITTED", 3, 'F' },
    { "EC-PROGRAM-CANCEL-ACTIVE", 3, 'F' },
    { "EC-PROGRAM-IMP", 3, 'I' },
    { "EC-PROGRAM-NOT-FOUND", 3, 'F' },
    { "EC-PROGRAM-PTR-NULL", 3, 'F' },
    { "EC-PROGRAM-RECURSIVE-CALL", 3, 'F' },
    { "EC-PROGRAM-RESOURCES", 3, 'F' },
    { "EC-RAISING", 2, 0 },
    { "EC-RAISING-IMP", 3, 'I' },
    { "EC-RAISING-NOT-SPECIFIED", 3, 'F' },
    { "EC-RANGE", 2, 0 },
    { "EC-RANGE-IMP", 3, 'I' },
    { "EC-RANGE-INDEX", 3, 'F' },
    { "EC-RANGE-INSPECT-SIZE", 3, 'F' },
    { "EC-RANGE-INVALID", 3, 'N' },
    { "EC-RANGE-PERFORM-VARYING", 3, 'F' },
    { "EC-RANGE-PTR", 3, 'F' },
    { "EC-RANGE-SEARCH-INDEX", 3, 'N' },
    { "EC-RANGE-SEARCH-NO-MATCH", 3, 'N' },
    { "EC-REPORT", 2, 0 },
    { "EC-REPORT-ACTIVE", 3, 'F' },
    { "EC-REPORT-COLUMN-OVERLAP", 3, 'N' },
    { "EC-REPORT-FILE-MODE", 3, 'F' },
    { "EC-REPORT-IMP", 3, 'I' },
    { "EC-REPORT-INACTIVE", 3, 'F' },
    { "EC-REPORT-LINE-OVERLAP", 3, 'N' },
    { "EC-REPORT-NOT-TERMINATED", 3, 'N' },
    { "EC-REPORT-PAGE-LIMIT", 3, 'N' },
    { "EC-REPORT-PAGE-WIDTH", 3, 'N' },
    { "EC-REPORT-SUM-SIZE", 3, 'F' },
    { "EC-REPORT-VARYING", 3, 'F' },
    { "EC-SCREEN", 2, 0 },
    { "EC-SCREEN-FIELD-OVERLAP", 3, 'N' },
    { "EC-SCREEN-IMP", 3, 'I' },
    { "EC-SCREEN-ITEM-TRUNCATED", 3, 'N' },
    { "EC-SCREEN-LINE-NUMBER", 3, 'N' },
    { "EC-SCREEN-STARTING-COLUMN", 3, 'N' },
    { "EC-SIZE", 2, 0 },
    { "EC-SIZE-ADDRESS", 3, 'F' },
    { "EC-SIZE-EXPONENTIATION", 3, 'F' },
    { "EC-SIZE-IMP", 3, 'I' },
    { "EC-SIZE-OVERFLOW", 3, 'F' },
    { "EC-SIZE-TRUNCATION", 3, 'F' },
    { "EC-SIZE-UNDERFLOW", 3, 'F' },
    { "EC-SIZE-ZERO-DIVIDE", 3, 'F' },
    { "EC-SORT-MERGE", 2, 0 },
    { "EC-SORT-MERGE-ACTIVE", 3, 'F' },
    { "EC-SORT-MERGE-FILE-OPEN", 3, 'F' },
    { "EC-SORT-MERGE-IMP", 3, 'I' },
    { "EC-SORT-MERGE-RELEASE", 3, 'F' },
    { "EC-SORT-MERGE-RETURN", 3, 'F' },
    { "EC-SORT-MERGE-SEQUENCE", 3, 'F' },
    { "EC-STORAGE", 2, 0 },
    { "EC-STORAGE-IMP", 3, 'I' },
    { "EC-STORAGE-NOT-ALLOC", 3, 'N' },
    { "EC-STORAGE-NOT-AVAIL", 3, 'N' },
    { "EC-USER", 2, 0 },
    /* { "EC-USER-suffix", 3, 'N' },  pattern entry (implementor/user supplies suffix), not a literal name */
    { "EC-VALIDATE", 2, 0 },
    { "EC-VALIDATE-CONTENT", 3, 'N' },
    { "EC-VALIDATE-FORMAT", 3, 'N' },
    { "EC-VALIDATE-IMP", 3, 'I' },
    { "EC-VALIDATE-RELATION", 3, 'N' },
    { "EC-VALIDATE-VARYING", 3, 'F' },
    { NULL, 0, 0 }
};
#define NEC (int)(sizeof g_ec / sizeof g_ec[0] - 1)
static char g_ecu[64][64]; static int g_necu;             /* EC-USER-suffix names met so far */
/* TURN for one file (7.3.25 rules 4, 6, 8; cobol ISSUES-87): an override
 * of checking for one EC-I-O condition and one file, over the setting
 * for all files; a TURN without a file clears a condition's overrides */
typedef struct { int ec, file; unsigned char on, loc; } EcFile;
/* The exception checking in force at a point in the source (cobol
 * ISSUES-53, -87, -94): each level-3 condition's checking and WITH
 * LOCATION, the per-file overrides, and the setting EC-USER-names not
 * yet met will take.  One state, saved and restored whole. */
typedef struct {
    unsigned char on[NEC + 64], loc[NEC + 64];
    EcFile *f; int nf, fcap;
    int user_on, user_loc;
} EcState;
static EcState g_ecs;
static void ecs_copy(EcState *d, const EcState *s)
{
    EcFile *f = d->f; int cap = d->fcap;
    if (cap < s->nf) { cap = s->nf + 16; f = xrealloc(f, (size_t)cap * sizeof *f); }
    *d = *s; d->f = f; d->fcap = cap;
    if (s->nf) memcpy(d->f, s->f, (size_t)s->nf * sizeof *s->f);
}

static const char *ec_name(int i) { return i < NEC ? g_ec[i].name : g_ecu[i - NEC]; }
static int ec_level(int i) { return i < NEC ? g_ec[i].level : 3; }
static int ec_fatal(int i) { return i < NEC && g_ec[i].fatal == 'F'; }

/* the index of an exception-name, or -1; a new EC-USER-suffix is added */
static int ec_find(const char *w, int line)
{
    for (int i = 0; i < NEC; i++) if (!strcasecmp(w, g_ec[i].name)) return i;
    if (!strncasecmp(w, "ec-user-", 8) && w[8]) {
        size_t n = strlen(w);
        for (size_t k = 8; k < n; k++) if (!isalnum((unsigned char)w[k]) && w[k] != '-' && w[k] != '_') return -1;
        if (w[n - 1] == '-' || w[n - 1] == '_') return -1;
        for (int i = 0; i < g_necu; i++) if (!strcasecmp(w, g_ecu[i])) return NEC + i;
        if (g_necu == 64) die_at(line, "more than 64 EC-USER exception-names");
        snprintf(g_ecu[g_necu], sizeof g_ecu[0], "%s", w);
        for (char *c = g_ecu[g_necu]; *c; c++) *c = (char)toupper((unsigned char)*c);
        g_ecs.on[NEC + g_necu] = (unsigned char)g_ecs.user_on; g_ecs.loc[NEC + g_necu] = (unsigned char)g_ecs.user_loc;
        return NEC + g_necu++;
    }
    return -1;
}

/* a level-3 name's level-2 group */
static int ec_group(int i)
{
    if (i >= NEC) return ec_find("EC-USER", 0);
    int best = -1; size_t bl = 0;
    for (int k = 0; k < NEC; k++) {
        if (g_ec[k].level != 2) continue;
        size_t l = strlen(g_ec[k].name);
        if (l > bl && !strncmp(g_ec[i].name, g_ec[k].name, l) && g_ec[i].name[l] == '-') { best = k; bl = l; }
    }
    return best;
}

static int ec_on_io(const char *name, int file);
/* an exception-checking PERFORM (2023 14.9.28 format 3; cobol ISSUES-89)
 * whose imperative-statement-1 is being compiled: its WHEN phrases, the
 * labels of their handlers, and the data words a raise leaves for the
 * handler's return -- where to resume, and whether the condition was
 * fatal (general rule 20) */
typedef struct { int *ec, *file, n, cap, label; } EcpWhen;
typedef struct { EcpWhen *w; int nw, wcap, Lother, Lcommon, Lend, id, resume; } Ecp;
static Ecp **g_ecp; static int g_necp, g_ecp_cap, g_ecp_handler;

static void ecf_set(int c, int file, int on, int loc)
{
    EcState *st = &g_ecs;
    for (int k = 0; k < st->nf; k++) if (st->f[k].ec == c && st->f[k].file == file) { st->f[k].on = (unsigned char)on; st->f[k].loc = (unsigned char)loc; return; }
    if (st->nf == st->fcap) {
        st->fcap = st->fcap ? 2 * st->fcap : 16;
        st->f = xrealloc(st->f, (size_t)st->fcap * sizeof *st->f);
    }
    st->f[st->nf].ec = c; st->f[st->nf].file = file; st->f[st->nf].on = (unsigned char)on; st->f[st->nf].loc = (unsigned char)loc; st->nf++;
}
static void ecf_clear(int c)
{
    EcState *st = &g_ecs;
    int m = 0;
    for (int k = 0; k < st->nf; k++) if (st->f[k].ec != c) st->f[m++] = st->f[k];
    st->nf = m;
}
/* checking for condition c on file index file (-1: none), and WITH LOCATION */
static int ec_on_file(int c, int file, int *loc)
{
    for (int k = 0; file >= 0 && k < g_ecs.nf; k++)
        if (g_ecs.f[k].ec == c && g_ecs.f[k].file == file) { if (loc) *loc = g_ecs.f[k].loc; return g_ecs.f[k].on; }
    if (loc) *loc = g_ecs.loc[c];
    return g_ecs.on[c];
}
/* checking for condition c everywhere: on, and no file's override off */
static int ec_on_all(int c)
{
    if (!g_ecs.on[c]) return 0;
    for (int k = 0; k < g_ecs.nf; k++) if (g_ecs.f[k].ec == c && !g_ecs.f[k].on) return 0;
    return 1;
}
/* does exception-name i cover level-3 condition c: itself, its group's,
 * or EC-ALL's -- EC-I-O-WARNING only by its own name (14.6.13.1.2) */
static int ec_covers(int i, int c)
{
    if (ec_level(c) != 3) return 0;
    if (c == ec_find("EC-I-O-WARNING", 0) && c != i) return 0;
    int lv = ec_level(i);
    return c == i || lv == 1 || (lv == 2 && ec_group(c) == i);
}
/* does name i (at level 1, or EC-USER) also decide the EC-USER-names not yet met */
static int ec_covers_later_users(int i) { return ec_level(i) == 1 || (i < NEC && !strcmp(g_ec[i].name, "EC-USER")); }
/* one condition's checking set, for all files (their overrides cleared)
 * or for one */
static void ec_turn_c(int c, int file, int on, int loc)
{
    if (file >= 0) { ecf_set(c, file, on, on && loc); return; }
    g_ecs.on[c] = (unsigned char)on; g_ecs.loc[c] = (unsigned char)(on && loc);
    ecf_clear(c);
}

/* >>TURN name [file-name] ... CHECKING {ON [WITH LOCATION] | OFF} (2023 7.3.25) */
static void apply_turn(Tok *d)
{
    char buf[512]; snprintf(buf, sizeof buf, "%s", d->s);
    char *w[64]; int nw = 0;
    for (char *t = strtok(buf, " \t"); t && nw < 64; t = strtok(NULL, " \t")) w[nw++] = t;
    int k = 1, names[64], files[64], nn = 0;       /* w[0] is TURN */
    while (k < nw && strcasecmp(w[k], "checking")) {
        if (strncasecmp(w[k], "ec-", 3)) {
            /* a file-name after an exception-name (rule 1: a word not EC-) */
            if (!nn) die_at(d->line, ">>TURN: '%s' is not an exception-name", w[k]);
            char lw[64]; int q = 0; for (; w[k][q] && q < 63; q++) lw[q] = (char)tolower((unsigned char)w[k][q]); lw[q] = 0;
            File *f = file_find(lw);
            if (!f) die_at(d->line, ">>TURN: '%s' is not a file-name", w[k]);
            const char *en = ec_name(names[nn - 1]);
            if (strncmp(en, "EC-I-O", 6)) die_at(d->line, ">>TURN: a file-name follows only an EC-I-O exception-name (2023 7.3.25.3 rule 4)");
            if (files[nn - 1] >= 0) { if (nn == 64) die_at(d->line, ">>TURN: too many names"); names[nn] = names[nn - 1]; nn++; }
            files[nn - 1] = (int)(f - g_files); k++;
            continue;
        }
        int i = ec_find(w[k], d->line);
        if (i < 0) die_at(d->line, ">>TURN: '%s' is not an exception-name", w[k]);
        if (nn == 64) die_at(d->line, ">>TURN: too many names");
        names[nn] = i; files[nn] = -1; nn++; k++;
    }
    if (!nn || k >= nw) die_at(d->line, ">>TURN needs exception-names and CHECKING ON or OFF");
    for (int a = 0; a < nn; a++)
        for (int b = a + 1; b < nn; b++)
            if (names[a] == names[b] && files[a] == files[b])
                die_at(d->line, ">>TURN names %s%s%s twice (2023 7.3.25.3 rule 3)", ec_name(names[a]),
                       files[a] >= 0 ? " for " : "", files[a] >= 0 ? g_files[files[a]].name : "");
    k++;
    int on = 1, loc = 0;
    if (k < nw && !strcasecmp(w[k], "off")) { on = 0; k++; }
    else {
        if (k < nw && !strcasecmp(w[k], "on")) k++;
        if (k < nw && !strcasecmp(w[k], "with")) k++;
        if (k < nw && !strcasecmp(w[k], "location")) { loc = 1; k++; }
    }
    if (k < nw) die_at(d->line, ">>TURN: unexpected '%s'", w[k]);
    for (int j = 0; j < nn; j++) {
        int i = names[j];
        for (int c = 0; c < NEC + g_necu; c++) if (ec_covers(i, c)) ec_turn_c(c, files[j], on, loc);
        if (files[j] < 0 && ec_covers_later_users(i)) { g_ecs.user_on = on; g_ecs.user_loc = on && loc; }
    }
}

static int unit_use_own_from(void);

/* the directives the parser has reached */
static void apply_dirs(void)
{
    while (g_ndir_done < g_ndir && g_dir[g_ndir_done].pos <= g_tp) apply_turn(&g_dir[g_ndir_done++].tok);
}

/* the unit's declarative sections: each USE names files or open modes.
 * After an I/O statement the compiler emits the choice: this unit's USE
 * for the file, then this unit's for the open mode, then outward through
 * the containing programs' GLOBAL ones (X3.23-1985 USE general rules). */
typedef struct { int sec, unit, global, mode; File *file; int ec; } UseEntry;   /* ec: an exception-name's index (USE AFTER EXCEPTION CONDITION), else -1 */
static UseEntry g_use[64]; static int g_nuse;
static struct { int unit, sec; int rep; } g_rwuse[16]; static int g_nrwuse;   /* USE BEFORE REPORTING sections: their report, for SUPPRESS */
static int g_in_decl;

/* USE [GLOBAL] AFTER [STANDARD] {ERROR|EXCEPTION} PROCEDURE [ON] {file... | INPUT | OUTPUT | I-O | EXTEND} */
static void parse_use(void)
{
    int line = cur()->line;
    if (!g_in_decl) die_at(line, "USE belongs in a DECLARATIVES section");
    /* immediately after the section header, a sentence by itself
     * (X3.23-1985 USE rule 1; 2023 14.9.49.3 rule 1) */
    if (g_cur_sec_id < 0 || g_tp < 4 || g_tok[g_tp - 2].kind != T_PERIOD || !is_word(&g_tok[g_tp - 3], "section"))   /* USE itself is g_tp - 1 */
        die_at(line, "USE immediately follows its section header (2023 14.9.49.3 rule 1)");
    int global = accept_word("global");
    if (accept_word("before")) {
        expect_word("reporting");
        if (cur()->kind != T_WORD) die_at(line, "USE BEFORE REPORTING needs a report group name");
        RGroup *g = NULL; Report *r = NULL;
        for (int i = g_report_base; i < g_nreport && !g; i++)
            for (int k = 0; k < g_reports[i].ng; k++)
                if (g_reports[i].g[k].name[0] && !strcmp(g_reports[i].g[k].name, cur()->s)) { r = &g_reports[i]; g = &g_reports[i].g[k]; break; }
        if (!g) die_at(line, "'%s' is not a report group", cur()->s);
        if (g->use_sec >= 0) die_at(line, "two USE BEFORE REPORTING procedures for '%s'", cur()->s);
        advance();
        g->use_sec = g_cur_sec_id;
        if (g_nrwuse == 16) die_at(line, "too many USE BEFORE REPORTING sections");
        g_rwuse[g_nrwuse].unit = g_unit; g_rwuse[g_nrwuse].sec = g_cur_sec_id; g_rwuse[g_nrwuse].rep = (int)(r - g_reports);
        g_nrwuse++;
        (void)global;
        if (cur()->kind != T_PERIOD) die_at(cur()->line, "the USE statement is a sentence by itself (2023 14.9.49.3 rule 1)");
        return;
    }
    if (at_word("for") && is_word(cur() + 1, "debugging")) {
        /* the section's uses of the module's special register stay quiet */
        static const char *dbg[] = { "debug-item", "debug-line", "debug-name", "debug-sub-1", "debug-sub-2",
                                     "debug-sub-3", "debug-contents", NULL };
        for (int i = 0; dbg[i] && g_npoison < 64; i++) snprintf(g_poison[g_npoison++], sizeof g_poison[0], "%s", dbg[i]);
        die_at(line, "USE FOR DEBUGGING is the Debug module, obsolete in COBOL 85 (item 18) and not implemented here");
    }
    expect_word("after");
    if ((at_word("exception") && is_word(cur() + 1, "condition")) || at_word("ec")) {
        /* USE AFTER EXCEPTION CONDITION exception-name ... (2023 14.9.49 format 3) */
        if (g_std < 2002) die_at(line, "USE AFTER EXCEPTION CONDITION is COBOL 2002; compile with -std=2002");
        if (global) die_at(line, "USE GLOBAL is not allowed with EXCEPTION CONDITION (2023 14.9.49.2, format 3 has no GLOBAL)");
        if (!accept_word("ec")) { advance(); advance(); }
        int any = 0;
        while (cur()->kind == T_WORD && !strncmp(cur()->s, "ec-", 3)) {
            int i = ec_find(cur()->s, cur()->line);
            if (i < 0) die_at(cur()->line, "'%s' is not an exception-name", cur()->s);
            advance();
            if (at_word("file")) die_at(cur()->line, "USE AFTER EXCEPTION CONDITION ... FILE is not implemented yet");
            /* the same name in two USE statements is allowed: the first
             * in the source is the one selected (14.9.49.4 rule 3) */
            if (g_nuse == 64) die_at(line, "too many USE procedures");
            g_use[g_nuse].sec = g_cur_sec_id; g_use[g_nuse].unit = g_unit; g_use[g_nuse].global = 0;
            g_use[g_nuse].mode = 0; g_use[g_nuse].file = NULL; g_use[g_nuse].ec = i;
            g_nuse++; any = 1;
        }
        if (!any) die_at(line, "USE AFTER EXCEPTION CONDITION needs an exception-name");
        if (cur()->kind != T_PERIOD) die_at(cur()->line, "the USE statement is a sentence by itself (2023 14.9.49.3 rule 1)");
        return;
    }
    if (at_word("exception") && is_word(cur() + 1, "object")) die_at(line, "USE AFTER EXCEPTION OBJECT is object orientation, not implemented");
    accept_word("standard");
    if (!accept_word("error") && !accept_word("exception")) die_at(line, "USE AFTER ... : expected ERROR or EXCEPTION PROCEDURE (the other USE forms are not implemented)");
    expect_word("procedure"); accept_word("on");
    int sec = g_cur_sec_id, any = 0;
    for (;;) {
        int mode = 0;
        if (accept_word("input")) mode = COB_OPEN_INPUT;
        else if (accept_word("output")) mode = COB_OPEN_OUTPUT;
        else if (accept_word("i-o")) mode = COB_OPEN_IO;
        else if (accept_word("extend")) mode = COB_OPEN_EXTEND;
        File *f = NULL;
        if (!mode) {
            if (!(cur()->kind == T_WORD && file_find(cur()->s))) break;
            f = expect_file();
            if (f->org == COB_ORG_SORT)
                die_at(line, "'%s' is a sort or merge file and takes no USE procedure (2023 14.9.49.3 rule 2)", f->name);
        }
        for (int i = 0; i < g_nuse; i++)
            if (g_use[i].unit == g_unit && g_use[i].mode == mode && g_use[i].file == f)
                die_at(line, mode ? "two USE procedures for the same open mode (2023 14.9.49.3 rule 7)" : "two USE procedures for file '%s' (2023 14.9.49.3 rule 8)", f ? f->name : "");
        if (g_nuse == 64) die_at(line, "too many USE procedures");
        g_use[g_nuse].sec = sec; g_use[g_nuse].unit = g_unit; g_use[g_nuse].global = global; g_use[g_nuse].mode = mode; g_use[g_nuse].file = f; g_use[g_nuse].ec = -1;
        g_nuse++; any = 1;
    }
    if (!any) die_at(line, "USE AFTER ERROR PROCEDURE needs a file-name or INPUT/OUTPUT/I-O/EXTEND");
    if (cur()->kind != T_PERIOD) die_at(cur()->line, "the USE statement is a sentence by itself (2023 14.9.49.3 rule 1)");
}

/* after an I/O statement, with its result in SLOT_C: if the condition is
 * not handled by the statement's own clause and a USE procedure applies,
 * perform that section (the runtime picks it: the file's, else the open
 * mode's), then continue with the next statement */
static void unit_use_range(int level, int *from, int *to);
static int unit_use_own_from(void);                     /* where this unit's own USE entries begin */
static int ecp_target(int i, int fidx, Ecp **ep, int *resume);

/* is there a USE AFTER ERROR procedure the file could reach (its own, or
 * one for an open mode, here or GLOBAL in a containing program)? */
static int file_has_use(File *f)
{
    for (int level = g_udepth; level >= 0; level--) {
        int from, to;
        if (level == g_udepth) { from = unit_use_own_from(); to = g_nuse; } else unit_use_range(level, &from, &to);
        for (int i = from; i < to; i++) {
            UseEntry *u = &g_use[i];
            if (level < g_udepth && !u->global) continue;
            if (u->ec >= 0) continue;
            if (u->file == f || u->mode) return 1;
        }
    }
    return 0;
}
/* AT END or INVALID KEY with no USE procedure for the file: the 1985 text
 * requires the phrase (READ rule 2, and the keyed statements' likewise);
 * here the FILE STATUS or the run's stop takes the condition */
static void io_phrase_required(File *f, const char *w, int line)
{
    if (!at_word(w) && !(at_word("not") && is_word(peek(1), w)) && !file_has_use(f)) bp(BP_E18_NO_ATEND, line);
}

static void emit_use_dispatch(File *f, int has_clause)
{
    /* the candidates, in the order the text gives them: this unit's USE
     * for the file, its USE for the open mode, then each containing
     * program's GLOBAL ones the same way */
    UseEntry *c[64]; int nc = 0, any_mode = 0;
    for (int level = g_udepth; level >= 0; level--) {
        int from, to;
        if (level == g_udepth) { from = unit_use_own_from(); to = g_nuse; } else unit_use_range(level, &from, &to);
        for (int pass = 0; pass < 2; pass++)
            for (int i = from; i < to; i++) {
                UseEntry *u = &g_use[i];
                if (level < g_udepth && !u->global) continue;
                if (u->ec >= 0) continue;                 /* an exception-name's, not a file's */
                if (pass == 0 ? u->file != f : !u->mode) continue;
                if (u->mode) any_mode = 1;
                c[nc++] = u;
            }
    }
    /* SLOT_C after the statement: 0 fine, 1 the statement's own condition,
     * 2 an error with a FILE STATUS to record it, 3 an error nothing but a
     * USE procedure can take -- the run stops if none does */
    int Ldone = new_label();
    /* EC-I-O (cobol ISSUES-58): with checking on, the condition the I-O
     * status names (2023 9.1.13) -- after the statement's own phrase and
     * the file's and the open mode's USE AFTER ERROR procedures, before
     * the run stops for want of one (USE general rule 3) */
    int fidx = (int)(f - g_files);
    int warn = ec_on_io("EC-I-O-WARNING", fidx), Lwarn = warn ? new_label() : 0;
    emit("\tldw r13, sp+%d", SLOT_C);
    emit("\tbeq r13, r0, .L%d", warn ? Lwarn : Ldone);
    if (has_clause) { emit_li("r2", 1); emit("\tbeq r13, r2, .L%d", Ldone); }
    static const struct { const char *name; int digit; } ecio[] = {
        { "EC-I-O-AT-END", 1 }, { "EC-I-O-INVALID-KEY", 2 }, { "EC-I-O-PERMANENT-ERROR", 3 },
        { "EC-I-O-LOGIC-ERROR", 4 }, { "EC-I-O-RECORD-OPERATION", 5 }, { "EC-I-O-FILE-SHARING", 6 },
        { "EC-I-O-RECORD-CONTENT", 7 }, { "EC-I-O-IMP", 9 }, { NULL, 0 } };
    /* inside imperative-statement-1 of an exception-checking PERFORM, a
     * condition a WHEN phrase takes goes there, and a USE procedure that
     * would match is ignored (14.9.28 rules 17, 18; cobol ISSUES-94 E8) */
    int taken[16] = { 0 }, any_taken = 0;
    for (int k = 0; g_necp && ecio[k].name; k++) {
        Ecp *e; int r, i = ec_find(ecio[k].name, 0);
        if (ec_on_io(ecio[k].name, fidx) && ecp_target(i, fidx, &e, &r) >= 0) { taken[k] = 1; any_taken = 1; }
    }
    if (any_taken) {
        emit_call("cob_io_class");                  /* the status's first digit; r13 survives */
        for (int k = 0; ecio[k].name; k++) {
            if (!taken[k]) continue;
            int Lnext = new_label();
            emit_li("r2", ecio[k].digit);
            emit("\tbne r1, r2, .L%d", Lnext);
            g_ec_file = f->oname; g_ec_fidx = fidx;
            emit_ec_raise(ec_find(ecio[k].name, 0));
            g_ec_file = NULL; g_ec_fidx = -1;
            emit_jump(Ldone);
            emit_label(Lnext);
        }
    }
    if (any_mode) { emit_file_addr("r3", f); emit_call("cob_open_mode"); emit("\tadd r12, r0, r1"); }
    for (int i = 0; i < nc; i++) {
        int Lnext = new_label(), Lret = new_label();
        char lab[32]; snprintf(lab, sizeof lab, ".L%d", Lret);
        if (c[i]->mode) { emit_li("r2", c[i]->mode); emit("\tbne r12, r2, .L%d", Lnext); }
        emit_li("r3", c[i]->sec);
        emit_la("r4", lab);
        emit_call("cob_perform_push");
        emit("\tjal r0, .Lp%d_%d", c[i]->unit, c[i]->sec);
        emit_label(Lret);
        emit_jump(Ldone);
        emit_label(Lnext);
    }
    int any = 0;
    for (int k = 0; ecio[k].name; k++) if (ec_on_io(ecio[k].name, fidx) && !taken[k]) any = 1;
    if (any) {
        emit_call("cob_io_class");                  /* the status's first digit; r13 survives */
        for (int k = 0; ecio[k].name; k++) {
            if (!ec_on_io(ecio[k].name, fidx) || taken[k]) continue;
            int Lnext = new_label();
            emit_li("r2", ecio[k].digit);
            emit("\tbne r1, r2, .L%d", Lnext);
            g_ec_file = f->oname; g_ec_fidx = fidx;
            emit_ec_raise(ec_find(ecio[k].name, 0));      /* a fatal one ends the run here */
            g_ec_file = NULL; g_ec_fidx = -1;
            emit_jump(Ldone);
            emit_label(Lnext);
        }
    }
    emit_li("r2", 3);
    emit("\tbne r13, r2, .L%d", Ldone);
    emit_file_addr("r3", f);
    emit_call("cob_io_unhandled");
    if (warn) {
        /* a successful statement whose status is not 00: EC-I-O-WARNING,
         * only when turned on by its own name (7.3.25 rule 4) */
        emit_jump(Ldone);
        emit_label(Lwarn);
        emit_call("cob_io_class");
        emit("\tbne r1, r0, .L%d", Ldone);
        g_ec_file = f->oname; g_ec_fidx = fidx;
        emit_ec_raise(ec_find("EC-I-O-WARNING", 0));
        g_ec_file = NULL; g_ec_fidx = -1;
    }
    emit_label(Ldone);
}

/* ---- PERFORM ---------------------------------------------------------- */

static int g_ncnt;      /* TIMES counters */
static int *g_cnt_unit; static int g_cnt_cap;   /* the unit each counter belongs to */

typedef struct { Para *from, *thru; int inline_body, Lexit; } Body;   /* Lexit: after END-PERFORM, for EXIT PERFORM */

/* the inline PERFORMs being compiled, innermost last: where EXIT PERFORM
 * goes, and EXIT PERFORM CYCLE (-1: not allowed) (2023 14.9.14.4 rules
 * 4-5; cobol ISSUES-90) */
static struct { int Lexit, Lcycle; } g_pstk[64]; static int g_npstk;
static void pstk_push(int lexit, int lcycle)
{
    if (g_npstk == 64) die_at(cur()->line, "inline PERFORMs nested more than 64 deep");
    g_pstk[g_npstk].Lexit = lexit; g_pstk[g_npstk].Lcycle = lcycle; g_npstk++;
}
/* EXIT PARAGRAPH and EXIT SECTION: the current paragraph's and section's
 * end, made when an EXIT asks for it (rules 6-7) */
static int g_exit_par_label = -1, g_exit_sec_label = -1;
static void end_par_label(void) { if (g_exit_par_label >= 0) { emit_label(g_exit_par_label); g_exit_par_label = -1; } }
static void end_sec_label(void) { if (g_exit_sec_label >= 0) { emit_label(g_exit_sec_label); g_exit_sec_label = -1; } }

static void emit_body(Body *b);
