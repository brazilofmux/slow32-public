/* s32-cobc: lowering to HIR (docs/plans/hir.md; cobol ISSUES-123, step 4).
 * A part of one translation unit, included by s32-cobc.c in order; not a
 * header to include anywhere else.
 *
 * A statement over native items (native.h) that this file takes leaves
 * one line in the text, "\tisland N", and a node of its own here.  At
 * the unit's end each run of such lines is an island: one function in
 * stage08's HIR (src/hir), compiled through its pipeline, its text
 * appended after the unit's return and called where the run was.  In
 * the island the items are allocas, which SSA construction makes
 * registers.  The arithmetic is the stack's, written out (see the plan
 * and arith_reg.h, whose bounds decide the widths). */

static int g_hir_on = 1;                /* -fno-hir, S32_HIR=0 */
static int lw_trace(void) { static int t = -1; if (t < 0) t = getenv("S32_HIR_TRACE") != NULL; return t; }
/* A run of statements is made an island when a loop is among them, or
 * when there are at least this many: a statement alone pays the call, the
 * prologue and the loads and stores of its items for arithmetic the text
 * emitter does in place (csv2fw's byte loop lost 6% to 52 such islands).
 * S32_HIR_MIN=n sets it; 0 is every run. */
static int lw_min(void) { static int m = -1; if (m < 0) { const char *e = getenv("S32_HIR_MIN"); m = e ? atoi(e) : 4; } return m; }
/* S32_HIR_ONLY=a[-b]: of the runs that would be islands, only those whose
 * first statement is on source lines a to b are; for finding the one
 * that is wrong */
static int lw_only(int *b)
{
    static int lo = -2, hi = -2;
    if (lo < -1) { const char *e = getenv("S32_HIR_ONLY"); lo = hi = e ? atoi(e) : -1; if (e && strchr(e, '-')) hi = atoi(strchr(e, '-') + 1); }
    *b = hi; return lo;
}


/* ---- the statement form ---------------------------------------------- */

/* a value: op 0 an item (sym; native, or in storage and fetched by the
 * runtime), 'k' a literal (k at scale sc), + - * / n; the functions the
 * register trees take (arith_reg.h hn_fn): 'M' MOD and 'R' REM by the
 * literal k, 'I' INTEGER, 'T' INTEGER-PART, 'A' ABS, 'G' MAX, 'L' MIN;
 * sc its scale, bd a bound on its magnitude in units of that scale, neg
 * whether it can be below zero */
typedef struct { char op; int l, r; int sym; int ref; long long k; int sc; long double bd; int neg; } LNode;   /* ref: a subscripted item's reference in g_lw_o, else -1 */
/* a condition: cond.h's C_AND, C_OR, C_NOT, C_REL (op R_*; x, y values;
 * or, alnum, ax and ay operands in g_lw_o compared as bytes) */
typedef struct { int kind; int a, b; int x, y; int op; int alnum, ax, ay; } LCond;
enum { LS_STORE, LS_ADDTO, LS_IF, LS_LOOP, LS_AMOVE, LS_DISPLAY, LS_TEXT, LS_PHRASE, LS_GOTO };
typedef struct {
    int kind, line, para;               /* para: the paragraph it is in (id), -1 outside any */
    int expr;                           /* LS_STORE: the value; LS_ADDTO: the sum each receiver takes */
    int need;                           /* LS_STORE with '/' at the root: fraction digits the quotient is made to */
    int nr; int rsym[MAXOPS]; unsigned char rnd[MAXOPS];
    int rref[MAXOPS];                   /* a subscripted receiver's reference in g_lw_o, else -1 */
    int rem;                            /* LS_STORE of a quotient: the REMAINDER item, or -1 */
    int subtract;                       /* LS_ADDTO: SUBTRACT */
    int cond;                           /* LS_IF; LS_LOOP: UNTIL */
    int body, nbody, els, nels;         /* ranges of g_lw_list */
    int nv, var[8], from[8], by[8], vcond[8], test_after;   /* LS_LOOP: the VARYING levels (0: UNTIL alone, cond), each its item, FROM, BY and UNTIL */
    int asrc, adst[MAXOPS];             /* LS_AMOVE: operands in g_lw_o -- the sender, the receivers (nr); LS_DISPLAY: its operands (nr), asrc = NO ADVANCING */
    Block text;                         /* the statement's code by the text emitter, for a run that is no island */
    int cut;                            /* ... taken out of the stream (lw_stmt_text) */
    int nperf, perf0;                   /* LS_TEXT: the paragraph ranges it PERFORMs, in g_lw_perf from perf0 */
    int inl;                            /* LS_TEXT, a plain PERFORM of a range whose statements are all nodes: body is those statements, emitted in its place */
    int is_perform;                     /* LS_TEXT: the statement is a plain out-of-line PERFORM itself (not a statement with one inside) */
    int inl_lo, inl_hi, *poff;          /* inl: the range, and where each paragraph's statements begin in body (poff[hi - lo + 1] = nbody) */
    int gto;                            /* LS_GOTO: the target paragraph (id) */
    int phrase;                         /* LS_TEXT: its ON/NOT ON phrases (an LS_PHRASE), or -1: the call's value is the status word */
    int slot, on_one;                   /* LS_PHRASE: the status word's slot, and whether ON means 1 (else nonzero) */
    Block ptext;                        /* LS_TEXT with a phrase: the lines of the call alone */
} LStmt;

static LNode *g_lw_n; static int g_lw_nn, g_lw_ncap;
static LCond *g_lw_c; static int g_lw_nc, g_lw_ccap;
static LStmt *g_lw_s; static int g_lw_ns, g_lw_scap;
static int *g_lw_list; static int g_lw_nlist, g_lw_lcap;
static Opnd *g_lw_o; static int g_lw_no, g_lw_ocap;       /* operands of bytes, kept whole */
static int g_lw_nisland;                /* islands made so far, for their labels */

#define LW_GROW(arr, n, cap) do { if ((n) == (cap)) { (cap) = (cap) ? 2 * (cap) : 64; (arr) = xrealloc((arr), (size_t)(cap) * sizeof *(arr)); } } while (0)

static int lw_node(char op, int l, int r, int sym, long long k, int sc, long double bd, int neg)
{
    LW_GROW(g_lw_n, g_lw_nn, g_lw_ncap);
    LNode *x = &g_lw_n[g_lw_nn]; x->op = op; x->l = l; x->r = r; x->sym = sym; x->ref = -1; x->k = k; x->sc = sc; x->bd = bd; x->neg = neg;
    return g_lw_nn++;
}
static int lw_cnode(int kind, int a, int b, int x, int y, int op)
{
    LW_GROW(g_lw_c, g_lw_nc, g_lw_ccap);
    LCond *c = &g_lw_c[g_lw_nc]; c->kind = kind; c->a = a; c->b = b; c->x = x; c->y = y; c->op = op; c->alnum = 0; c->ax = c->ay = -1;
    return g_lw_nc++;
}
static int lw_stmt(int kind, int line)
{
    LW_GROW(g_lw_s, g_lw_ns, g_lw_scap);
    LStmt *s = &g_lw_s[g_lw_ns]; memset(s, 0, sizeof *s); s->kind = kind; s->line = line; s->para = g_cur_para ? g_cur_para->id : -1; s->expr = s->cond = s->rem = s->phrase = -1;
    return g_lw_ns++;
}
static void lw_list_add(int st) { LW_GROW(g_lw_list, g_lw_nlist, g_lw_lcap); g_lw_list[g_lw_nlist++] = st; }
/* the ranges the statement being read PERFORMs out of line (emit_body notes them) */
static struct { int lo, hi, once; } *g_lw_perf; static int g_lw_nperf, g_lw_pcap2;   /* once: a plain PERFORM of the range (sort.h's g_lw_pf_once) */
static void lw_note_perform(int lo, int hi) { LW_GROW(g_lw_perf, g_lw_nperf, g_lw_pcap2); g_lw_perf[g_lw_nperf].lo = lo; g_lw_perf[g_lw_nperf].hi = hi; g_lw_perf[g_lw_nperf].once = g_lw_pf_once; g_lw_nperf++; }
static int lw_opnd_keep(const Opnd *o) { LW_GROW(g_lw_o, g_lw_no, g_lw_ocap); g_lw_o[g_lw_no] = *o; return g_lw_no++; }
static int lw_ref_keep(const Ref *r) { Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = *r; o.line = r->line; return lw_opnd_keep(&o); }

/* the statement's line in the text */
static void lw_place(int st) { if (lw_trace()) fprintf(stderr, "hir: line %d: statement %d kind %d\n", g_lw_s[st].line, st, g_lw_s[st].kind); emit("\tisland %d", st); }
/* ... at line at of the stream, before code a statement wrote as it read
 * its operands (DISPLAY): the placeholder must come first */
static void lw_place_at(int at, int st)
{
    char b[32]; snprintf(b, sizeof b, "\tisland %d", st);
    if (lw_trace()) fprintf(stderr, "hir: line %d: statement %d (at %d of %d)\n", g_lw_s[st].line, st, at, g_nasm);
    emit("%s", b);
    if (at < g_nasm - 1) { char *l = g_asm[g_nasm - 1]; memmove(g_asm + at + 1, g_asm + at, (size_t)(g_nasm - 1 - at) * sizeof *g_asm); g_asm[at] = l; }
}
static int lw_is_place(const char *l, int *st)
{
    if (strncmp(l, "\tisland ", 8)) return 0;
    char *e; long v = strtol(l + 8, &e, 10);
    if (*e) return 0;
    *st = (int)v;
    return 1;
}

static void lw_refuse(int line, const char *what, const char *why)
{
    if (lw_trace()) fprintf(stderr, "hir: line %d %s: not lowered: %s\n", line, what, why);
}

/* A lowered statement leaves its placeholder AND lets the text emitter
 * write its code after it; when the statement is over (parse_statement),
 * that code is cut away and kept with the node, for a run that is no
 * island.  The lines from b0: placeholders (after a -fprofile-lines
 * label), then the text. */
static int lw_stmt_text(int b0)
{
    int p = b0, st, last = -1;
    while (p < g_nasm && (!strncmp(g_asm[p], "__ln_", 5) || !strncmp(g_asm[p], "\t.globl __ln_", 13))) p++;
    /* the statement's own placeholders are the uncut ones; a statement
     * whose code begins with an inner statement's -- an exception-checking
     * PERFORM, whose body comes first -- is not lowered, and that one was
     * cut when its own statement ended (2002/exitperform) */
    while (p < g_nasm && lw_is_place(g_asm[p], &st) && !g_lw_s[st].cut) { last = st; g_lw_s[st].cut = 1; p++; }
    if (last < 0) {
        /* a statement not lowered: placeholders inside it are its inner
         * statements', cut already.  One of its own anywhere but first
         * would run twice, as text and as island. */
        if (lw_trace() && p < g_nasm && g_hir_on) {
            /* the verbs no hook takes, for the tally of what keeps a loop in the text */
            static const char *hooked[] = { "COMPUTE", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "MOVE", "IF", NULL };
            int k; for (k = 0; hooked[k] && strcmp(hooked[k], g_cur_stmt); k++) ;
            if (!hooked[k]) fprintf(stderr, "hir: line %d %s: no hook%s\n", g_stmt_tok ? g_stmt_tok->line : 0, g_cur_stmt, g_inline_depth ? " (in a loop)" : "");
        }
        for (int i = p; i < g_nasm; i++)
            if (lw_is_place(g_asm[i], &st) && !g_lw_s[st].cut)
                die_at(g_lw_s[st].line, "internal: a lowered statement's placeholder is not first in its code");
        return 0;
    }
    if (p == g_nasm) return 1;
    Block t = block_cut(p);
    g_lw_s[last].text = t;
    return 1;
}
/* every hook asks this first: the lowering is off, or this is a scan, or
 * the census is being taken (its reading of the text is the emitter's) */
static int lw_off(void) { return !g_hir_on || g_noemit || g_cen_on || g_fnsig_only || g_nerrors || g_stmt_calls.n; }
/* (g_stmt_calls.n: a user function's call was made for this statement;
 * its code goes before the statement's -- before the placeholder -- and
 * the island would compute with the result as the text does, twice over
 * (2002/userfnarith: the text was not cut, and both ran).  A statement
 * with a call keeps to the text.) */

/* a block of placeholders as the text emitter would have written it:
 * each statement's kept text in its place */
static void lw_expand_into(const Block *b)
{
    for (int i = 0; i < b->n; i++) {
        int st;
        if (lw_is_place(b->line[i], &st)) lw_expand_into(&g_lw_s[st].text);     /* a statement's text may hold its inner statements' placeholders */
        else emit("%s", b->line[i]);
    }
}
static Block lw_expand(const Block *b)
{
    int b0 = block_begin();
    lw_expand_into(b);
    return block_cut(b0);
}

/* a block that is nothing but placeholders: its statements appended to
 * g_lw_list, the range in *at, *n */
static int lw_block_stmts(const Block *b, int *at, int *n)
{
    *at = g_lw_nlist; *n = 0;
    for (int i = 0; i < b->n; i++) {
        int st; const char *l = b->line[i];
        if (!l[0] || (l[0] == '#' && l[1] == '@') || !strncmp(l, "__ln_", 5) || !strncmp(l, "\t.globl __ln_", 13)) continue;   /* marks, -fprofile-lines' labels */
        if (!lw_is_place(l, &st)) { g_lw_nlist = *at; return 0; }
        lw_list_add(st); (*n)++;
    }
    return 1;
}

/* ---- text statements (docs/plans/hir.md, milestone 2) ------------------
 * A statement no hook takes may still be an island's, as the lines the
 * text emitter wrote for it: emitted in place as an opaque call, with
 * the island's native items stored before it and loaded again after.
 * Admitted when its code is self-contained: every jump or branch to a
 * label of its own, no slot of the unit's (sp+8..sp+84 are a statement's
 * scratch), no indirect jump -- except the PERFORM of a paragraph range,
 * which jumps out and returns to the line after, and is admitted when
 * the range comes back (lw_range_returns, decided when the unit is read). */

/* does the range of paragraphs lo..hi (ids), PERFORMed, always come back
 * to its PERFORM?  No GO TO out of it, no GOBACK or EXIT PROGRAM in it,
 * and the same of every range it PERFORMs, transitively */
static int lw_range_hi(int lo, int thru)
{
    int hi = thru >= 0 ? thru : lo;
    const Para *p = &g_para[hi - 1];
    if (p->is_section) {                        /* a section: through its last paragraph (the unit's own: contained programs' paragraphs lie between) */
        for (int k = 0; k < g_npara; k++) if (g_para[k].unit == p->unit && g_para[k].section == p->id && g_para[k].id > hi) hi = g_para[k].id;
    }
    return hi;
}
static int lw_range_returns_1(int lo, int hi, int *seen, int nseen)
{
    if (lo < 1 || hi < lo || hi > g_npara) return 0;
    for (int k = 0; k < nseen; k += 2) if (seen[k] == lo && seen[k + 1] == hi) return 1;   /* a recursion: already being judged */
    if (nseen + 2 > 64) return 0;
    seen[nseen] = lo; seen[nseen + 1] = hi; nseen += 2;
    for (int k = 0; k < g_npcl; k++) if (g_pcl[k] >= lo && g_pcl[k] <= hi) return 0;
    for (int k = 0; k < g_npcg; k++) if (g_pcg[k].from >= lo && g_pcg[k].from <= hi && (g_pcg[k].target < lo || g_pcg[k].target > hi)) return 0;
    for (int k = 0; k < g_npcf; k++)
        if (g_pcf[k].from >= lo && g_pcf[k].from <= hi && !lw_range_returns_1(g_pcf[k].lo, lw_range_hi(g_pcf[k].lo, g_pcf[k].thru), seen, nseen)) return 0;
    return 1;
}
static int lw_range_returns(int lo, int hi)
{
    int seen[64], r = lw_range_returns_1(lo, hi, seen, 0);
    if (lw_trace()) fprintf(stderr, "hir: range %d..%d (%s..%s) %s (%d performs, %d gotos, %d leaves known)\n", lo, hi, g_para[lo - 1].name, g_para[hi - 1].name, r ? "returns" : "may not return", g_npcf, g_npcg, g_npcl);
    return r;
}

/* is paragraph p's code run more than once per entry to the unit, by
 * the PERFORMs alone: a PERFORM reaching it that iterates (UNTIL,
 * VARYING, TIMES) or is issued inside an in-line loop, or one issued from
 * a paragraph that is itself looped -- transitively, a recursion counted
 * as a loop.  GO TO loops are not seen (conservative: fewer islands). */
static int lw_para_looped_1(int p, int *seen, int nseen)
{
    for (int k = 0; k < nseen; k++) if (seen[k] == p) return 1;
    if (nseen >= 64) return 0;
    seen[nseen++] = p;
    for (int k = 0; k < g_npcf; k++) {
        int lo = g_pcf[k].lo, hi = lw_range_hi(lo, g_pcf[k].thru);
        if (p < lo || p > hi) continue;
        if (g_pcf[k].iter) return 1;
        if (g_pcf[k].from >= 1 && lw_para_looped_1(g_pcf[k].from, seen, nseen)) return 1;
    }
    return 0;
}
static int lw_para_looped(int p) { int seen[64]; return p >= 1 && lw_para_looped_1(p, seen, 0); }

/* the label a line defines, the target a jump or branch names */
static int lw_line_def(const char *l, char *name, int cap)
{
    if (l[0] == '\t' || l[0] == ' ' || l[0] == '#' || !l[0]) return 0;
    const char *c = strchr(l, ':');
    if (!c || c - l >= cap) return 0;
    memcpy(name, l, (size_t)(c - l)); name[c - l] = 0;
    return 1;
}
static int lw_line_target(const char *l, char *t, int cap)
{
    char op[8], ops[64];
    if (!strncmp(l, "\tjal r0, ", 9)) { snprintf(t, (size_t)cap, "%s", l + 9); for (char *e = t; *e; e++) if (*e == ' ' || *e == '\t' || *e == '#') { *e = 0; break; } return 1; }
    return line_branch(l, op, ops, t) ? 1 : 0;
}
/* is the statement's code self-contained?  *nperf: its PERFORMs of a range */
static int lw_text_ok(const Block *b, int line, const char *verb)
{
    char defs[512][64]; int nd = 0;
    for (int i = 0; i < b->n; i++) { char nm[64]; if (nd < 512 && lw_line_def(b->line[i], nm, sizeof nm)) snprintf(defs[nd++], 64, "%s", nm); }
    for (int i = 0; i < b->n; i++) {
        const char *l = b->line[i];
        if (l[0] != '\t' || l[1] == '.' || l[1] == '#') continue;
        if (!strncmp(l, "\tjalr ", 6)) { lw_refuse(line, verb, "an indirect jump"); return 0; }
        if (!strncmp(l, "\tjal r31, .L", 12)) { lw_refuse(line, verb, "a call to a label"); return 0; }
        const char *sp = strstr(l, "sp+");
        if (sp) { int n = atoi(sp + 3); if (n < 8 || n >= 84) { lw_refuse(line, verb, "a slot of the unit's"); return 0; } }
        if (strstr(l, " sp,") || !strncmp(l, "\taddi sp", 8)) { lw_refuse(line, verb, "the stack pointer"); return 0; }
        /* r14-r28 are the island's callee-saved registers, where its values
         * live across the node; the text emitter never names them -- except
         * loopreg, which rewrote an in-line loop's lines before the
         * statement's text was cut (gen-native 4401: w2's high word) */
        for (const char *p = l; (p = strchr(p, 'r')) != NULL; p++) {
            if (p > l && (isalnum((unsigned char)p[-1]) || p[-1] == '_' || p[-1] == '.')) continue;
            if (p[1] < '0' || p[1] > '9') continue;
            int r = atoi(p + 1); const char *e = p + 1; while (*e >= '0' && *e <= '9') e++;
            if (!isalnum((unsigned char)*e) && *e != '_' && r >= 14 && r <= 28) { lw_refuse(line, verb, "a callee-saved register"); return 0; }
        }
        char t[64];
        if (!lw_line_target(l, t, sizeof t)) continue;
        int k; for (k = 0; k < nd && strcmp(defs[k], t); k++) ;
        if (k < nd) continue;
        /* PERFORM of a range: its jump to the paragraph, after the push */
        if (!strncmp(l, "\tjal r0, .Lp", 12) && i > 0 && !strcmp(b->line[i - 1], "\tjal r31, cob_perform_push")) continue;
        lw_refuse(line, verb, "a jump out"); return 0;
    }
    return 1;
}
/* A statement's ON / NOT ON phrases (AT END, SIZE ERROR, ON EXCEPTION,
 * OVERFLOW: emit_phrases) whose blocks are all placeholders: the
 * statement's own code leaves its status word in a slot of the frame,
 * and the branches on it are an island's -- an LS_PHRASE records the
 * blocks, and one marker line stands where the branches would be.  The
 * statement then becomes a text statement whose call returns the word
 * (the lines end in a load of the slot into r1) and whose phrases are the
 * island's blocks (lw_text_stmt).  Not for the STRING/UNSTRING OVERFLOW
 * form, which has its value in r1 already (slot -1): the marker's load
 * would be of nothing; that one keeps to the text. */
static int lw_phrases(const Phrases *p, int slot, int on_one)
{
    if (lw_off() || slot < 0) return 0;
    int b = 0, nb = 0, e = 0, ne = 0, n0 = g_lw_nlist;
    if (p->has_on && !lw_block_stmts(&p->on, &b, &nb)) return 0;
    if (p->has_not && !lw_block_stmts(&p->not_on, &e, &ne)) { g_lw_nlist = n0; return 0; }     /* (= b reset the list to 0 without an ON block: every earlier IF's branches pointed elsewhere) */
    int st = lw_stmt(LS_PHRASE, cur()->line);
    LStmt *s = &g_lw_s[st]; s->slot = slot; s->on_one = on_one;
    s->body = b; s->nbody = p->has_on ? nb : 0; s->els = e; s->nels = p->has_not ? ne : 0;
    s->text = p->has_on ? p->on : p->not_on;           /* (the blocks themselves, for the text laid out again: lw_phrases_text) */
    s->ptext = p->has_not ? p->not_on : p->on;
    s->asrc = p->has_on; s->rem = p->has_not;
    emit("\tisland-phrase %d", st);
    return 1;
}
/* the phrases as the text emitter would have laid them out, over the blocks' text */
static Block lw_phrases_text(const LStmt *ph)
{
    Phrases p; memset(&p, 0, sizeof p);
    p.has_on = ph->asrc; p.has_not = ph->rem;
    if (p.has_on) p.on = ph->text;              /* the blocks as they are, placeholders inside: resolved when this text is laid out */
    if (p.has_not) p.not_on = ph->ptext;
    int b0 = block_begin();
    int save = g_hir_on; g_hir_on = 0;          /* (the hook stays out of its own layout) */
    emit_phrases(&p, ph->slot, ph->on_one);
    g_hir_on = save;
    return block_cut(b0);
}
static int lw_is_phrase_marker(const char *l, int *st)
{
    if (strncmp(l, "\tisland-phrase ", 15)) return 0;
    char *e; long v = strtol(l + 15, &e, 10);
    if (*e) return 0;
    *st = (int)v;
    return 1;
}
/* a statement not lowered, its code the lines from b0: a text node when
 * admitted; its PERFORMs are g_lw_perf[perf0..] */
static int lw_text_stmt(int b0, int perf0)
{
    if (g_nasm <= b0) return 0;
    int line = g_stmt_tok ? g_stmt_tok->line : 0;
    /* a phrase marker, which must be the statement's last line; the text
     * laid out whole, for the text's own fallback */
    int ph = -1, pst;
    for (int i = b0; i < g_nasm; i++) if (lw_is_phrase_marker(g_asm[i], &pst)) { if (ph >= 0 || i != g_nasm - 1) ph = -2; else ph = pst; }
    if (ph == -2) die_at(line, "internal: a statement's phrase marker is not its last line");
    if (lw_off() || !strcmp(g_cur_stmt, "STOP") || !strcmp(g_cur_stmt, "GOBACK") || !strcmp(g_cur_stmt, "EXIT") || !strcmp(g_cur_stmt, "GO") ||
        !strcmp(g_cur_stmt, "ALTER") || !strcmp(g_cur_stmt, "RAISE") || !strcmp(g_cur_stmt, "RESUME") || !strcmp(g_cur_stmt, "CONTINUE")) {
        if (ph >= 0) { g_nasm--; Block t = lw_phrases_text(&g_lw_s[ph]); block_put(&t); free(t.line); }
        return 0;
    }
    if (ph >= 0) g_nasm--;                      /* the marker */
    Block raw = block_cut(b0);
    Block t = lw_expand(&raw);                  /* the call's lines: inner statements' placeholders are their text */
    if (!lw_text_ok(&t, line, g_cur_stmt)) {
        block_put(&raw); free(raw.line); free(t.line);
        if (ph >= 0) { Block pt = lw_phrases_text(&g_lw_s[ph]); block_put(&pt); free(pt.line); }
        return 0;
    }
    /* kept: the raw lines, inner placeholders and all -- laid out again as
     * text, the inner statements are resolved on their own (an island
     * among them is not lost to the statement around it); the call's
     * lines are the expansion, and with phrases end in the status word */
    int st = lw_stmt(LS_TEXT, line);
    LStmt *s = &g_lw_s[st]; s->cut = 1; s->perf0 = perf0; s->nperf = g_lw_nperf - perf0; s->phrase = ph;
    s->is_perform = !strcmp(g_cur_stmt, "PERFORM") && g_lw_pf_once;    /* its own parse set the flag last: an in-line PERFORM around one resets it */
    s->ptext = t;
    if (ph < 0) s->text = raw;
    else {
        char ld[32]; snprintf(ld, sizeof ld, "\tldw r1, sp+%d", g_lw_s[ph].slot);
        int b1 = block_begin();
        block_put(&t); emit("%s", ld);
        s->ptext = block_cut(b1); free(t.line);
        Block pt = lw_phrases_text(&g_lw_s[ph]);
        int b2 = block_begin();
        block_put(&raw); block_put(&pt);
        s->text = block_cut(b2);
        free(pt.line); free(raw.line);
    }
    if (lw_trace()) fprintf(stderr, "hir: line %d %s: a text statement (%d lines%s%s)\n", line, g_cur_stmt, t.n, s->nperf ? ", performs" : "", ph >= 0 ? ", phrases" : "");
    lw_place(st);
    return 1;
}


/* ---- what is taken ---------------------------------------------------- */


/* an item the island may hold as a value: native, whole, not subscripted */
static int lw_item_ok(const Ref *r)
{
    const Sym *s = r->sym;
    if (!s->native || r->rm || r->nsub || s->ndims || s->is_group) return 0;
    if (s->pi.category != PIC_NUMERIC || s->pi.digits > 18 || s->pi.scale < 0 || strchr(s->pi.pat, 'P')) return 0;
    if (s->size != 1 && s->size != 2 && s->size != 4 && s->size != 8) return 0;
    if (rec_indirect(&g_sym[s->record])) return 0;
    return 1;
}
/* a numeric item in storage the runtime fetches and stores for the
 * island (cob_get_num, cob_put_num_x; the edited forms): whole, not
 * subscripted, at a label's known offset, of a usage the register trees
 * take (dx_leaf_ok) -- and not COMP-X, whose MOVE has a descriptor of its
 * own (move_desc) */
static int lw_sub_item_ok(Sym *s);
static int lw_mem_ok(const Ref *r)
{
    const Sym *s = r->sym;
    if (s->is_index) {                          /* INDEXED BY: a word holding the occurrence number, read as one (a subscript, a value; never a receiver here) */
        const Sym *rec = &g_sym[s->record];
        return !r->rm && !r->nsub && s->size == 4 && s->record >= 0 && rec->label[0] && !rec_indirect(rec);
    }
    if ((s->native && !s->ndims) || r->rm || s->is_group || s->is_rc || s->lin_file >= 0 || s->rep_ctr >= 0 || s->dynl) return 0;
    if (r->nsub != s->ndims || (r->nsub && (ec_on_name("EC-BOUND-SUBSCRIPT") || (odo_table_for((Sym *)s) || dyn_table_for((Sym *)s))))) return 0;   /* an element: its address formed as lw_ref_addr forms it */
    for (int k = 0; k < r->nsub; k++)
        if (r->sub[k].sym == &g_subx || (r->sub[k].sym && !lw_sub_item_ok(r->sub[k].sym))) return 0;
    if (s->native) return 1;                    /* a native table's element: a word in storage, loaded and stored as one */
    if (s->pi.category != PIC_NUMERIC && !(s->pi.category == PIC_NUMERIC_EDITED && s->usage == U_DISPLAY)) return 0;
    if (sym_wide(s) || s->pi.digits > 18 || s->pi.scale < 0 || strchr(s->pi.pat, 'P') || s->uvar != UV_NONE) return 0;
    switch (s->usage) {
    case U_DISPLAY: case U_BINARY: case U_PACKED: case U_COMP5: case U_SINT: case U_UINT:
    case U_SSHORT: case U_USHORT: case U_BCHAR: case U_UBCHAR: break;
    default: return 0;
    }
    const Sym *rec = &g_sym[s->record];
    if (rec_indirect(rec) || !rec->label[0] || rec->ftemp_scan) return 0;
    return 1;
}
static int lw_opnd_item_ok(const Ref *r) { return lw_item_ok(r) || lw_mem_ok(r); }

/* ---- bytes: alphanumeric moves and compares of lengths the compiler
 * knows (8.4.2.4.3: a reference-modified operand is alphanumeric
 * whatever its item) ---- */

/* an integer item a subscript may be: native or in storage, no decimals */
static int lw_sub_item_ok(Sym *s)
{
    Ref r; memset(&r, 0, sizeof r); r.sym = s; r.line = s->line;
    if (s->is_index) return lw_mem_ok(&r);
    return s->pi.scale == 0 && !s->ndims && lw_opnd_item_ok(&r);
}
/* a reference whose address and length the island can form: its record
 * by label, each subscript a literal or an integer item, a reference
 * modification with literal positions, no EC-BOUND check on; *len the
 * bytes.  A whole item is alphanumeric or alphabetic, or a group of fixed
 * length; a part is any item's bytes. */
static int g_lw_bytes_any;           /* a whole numeric item's bytes too (a bytewise compare) */
/* the bytes a reference has (lw_bytes_ref_ok admitted it): asked again
 * when the island is made, with nothing else -- the >>TURN state of the
 * compile has moved on by then (2002/ecbound turns EC-BOUND on after a
 * statement an island took) */
static long lw_bytes_len(const Ref *r) { return r->rm ? r->rm_len : r->sym->size; }
static int lw_expr(Expr *e, int top_div);
static int lw_bytes_ref_ok(const Ref *r, long *len)
{
    Sym *s = r->sym;
    if (s->is_cond || s->any_len || s->dynl || sym_bitlike(s) || s->natgroup || s->nat_usage || s->usage == U_NATIONAL || s->usage == U_BIT || s->is_index) return 0;
    if (s->pi.category == PIC_NATIONAL || s->pi.category == PIC_BOOLEAN || s->is_rc || s->lin_file >= 0 || s->rep_ctr >= 0) return 0;
    const Sym *rec = &g_sym[s->record];
    if (rec_indirect(rec) || !rec->label[0] || rec->ftemp_scan || odo_table_for(s) || dyn_table_for(s)) return 0;
    if (r->nsub != s->ndims) return 0;
    if (r->nsub && ec_on_name("EC-BOUND-SUBSCRIPT")) return 0;
    for (int k = 0; k < r->nsub; k++)
        if (r->sub[k].sym == &g_subx || (r->sub[k].sym && !lw_sub_item_ok(r->sub[k].sym))) return 0;
    if (r->rm) {
        if (r->rm_nat || r->rm_bit || r->rm_odo || r->bitsub || r->rm_zero) return 0;
        if (r->rm_sx) {
            /* a computed start: an integer expression the island can form, a
             * word (an item, one +/- a literal, or more: `fpos + fw - len`) */
            int x = lw_expr(r->rm_sx, 0);
            if (x < 0 || g_lw_n[x].sc != 0 || g_lw_n[x].bd >= 2147483648.0L) return 0;
            ((Ref *)r)->lw_startx = x + 1;
        }
        else if (r->rm_start <= 0) return 0;
        if (ec_on_name("EC-BOUND-REF-MOD")) return 0;
        if (r->rm_lx) {
            /* a computed length: an integer expression the island can form,
             * a word; its value is checked where the bytes are moved
             * (lw_dyn_len).  *len = -1 says so; only a MOVE takes one. */
            int x = lw_expr(r->rm_lx, 0);
            if (x < 0 || g_lw_n[x].sc != 0 || g_lw_n[x].bd >= 2147483648.0L) return 0;
            ((Ref *)r)->lw_lenx = x + 1;
            *len = -1;
            return 1;
        }
        if (r->rm_len <= 0) return 0;
        *len = r->rm_len;
        return 1;
    }
    if (s->is_group) { if (has_odo(s) || s->bitgroup || s->strong) return 0; }
    else if ((s->pi.category != PIC_ALPHANUMERIC && s->pi.category != PIC_ALPHABETIC && !(g_lw_bytes_any && s->pi.category == PIC_NUMERIC)) || s->pi.edited) return 0;
    *len = s->size;
    return 1;
}
/* a sending operand of bytes: a reference as above, a nonnumeric or
 * integer literal, SPACE or ZERO (filling); *len its bytes (a figurative's
 * is the receiver's) */
static int lw_bytes_src_ok(const Opnd *o, long *len)
{
    if (opnd_scanned(o) || o->all_sub) return 0;
    if (o->kind == O_REF) return lw_bytes_ref_ok(&o->ref, len);
    if (o->kind == O_STR) { *len = o->tok->len; return *len > 0; }
    if (o->kind == O_NUM) { *len = o->num.ndigits; return numlit_is_int(&o->num) && !o->folded && *len > 0; }
    if (o->kind == O_FIG) { *len = 0; return !strncmp(o->tok->s, "space", 5) || !strncmp(o->tok->s, "zero", 4); }
    return 0;
}
/* a receiving operand of bytes: a reference as above that is not a
 * numeric or edited item taken whole, and not JUSTIFIED */
static int lw_bytes_dst_ok(const Ref *r, long *len)
{
    Sym *d = r->sym;
    if (!lw_bytes_ref_ok(r, len) || ref_pending(r)) return 0;
    if (d->just) return 0;
    if (!r->rm && !d->is_group && (is_numeric_sym(d) || d->pi.category == PIC_NUMERIC_EDITED || d->pi.category == PIC_ALPHANUMERIC_EDITED)) return 0;
    return 1;
}
/* a leaf of a register tree (hn_tree): the item, a literal, ZERO */
static int lw_leaf(const Opnd *o)
{
    if (opnd_scanned(o)) return -2;
    if (o->kind == O_FIG) return !strncmp(o->tok->s, "zero", 4) ? hn_new(0, -1, -1, o) : -2;
    if (o->kind == O_NUM) return o->num.ndigits <= 18 && o->num.scale >= 0 && o->num.scale <= 18 ? hn_new(0, -1, -1, o) : -2;
    if (o->kind != O_REF || !lw_opnd_item_ok(&o->ref)) return -2;
    return hn_new(0, -1, -1, o);
}
/* the tree at h (g_hn, bounds in g_dsc/g_dbd from dx_check) as nodes;
 * -1 for an operation not taken */
static int lw_from_hn(int h)
{
    HNode *x = &g_hn[h];
    if (!x->op) {
        const Opnd *o = &x->o;
        if (o->kind == O_FIG) return lw_node('k', -1, -1, -1, 0, 0, 0, 0);
        if (o->kind == O_NUM) { long long v = numlit_scaled(&o->num); return lw_node('k', -1, -1, -1, v, o->num.scale, v < 0 ? -(long double)v : (long double)v, v < 0); }
        const Sym *s = o->ref.sym;
        int n = lw_node(0, -1, -1, (int)(s - g_sym), 0, s->pi.scale, g_dbd[h], s->pi.is_signed);
        if (o->ref.nsub) g_lw_n[n].ref = lw_opnd_keep(o);
        return n;
    }
    if (x->op == 'n' || x->op == 'A' || x->op == 'I' || x->op == 'T') {
        int l = lw_from_hn(x->l);
        if (l < 0) return -1;
        int neg = x->op == 'n' ? 1 : x->op == 'A' ? 0 : g_lw_n[l].neg;
        return lw_node(x->op, l, -1, -1, 0, g_dsc[h], g_dbd[h], neg);
    }
    if (x->op == 'M' || x->op == 'R') {
        int l = lw_from_hn(x->l);
        if (l < 0) return -1;
        long long d = numlit_int(&g_hn[x->r].o.num);
        return lw_node(x->op, l, -1, -1, d, 0, g_dbd[h], x->op == 'M' ? d < 0 : g_lw_n[l].neg);
    }
    if (x->op == 'G' || x->op == 'L') {
        int l = lw_from_hn(x->l); if (l < 0) return -1;
        int r = lw_from_hn(x->r); if (r < 0) return -1;
        return lw_node(x->op, l, r, -1, 0, g_dsc[h], g_dbd[h], g_lw_n[l].neg || g_lw_n[r].neg);
    }
    if (x->op != '+' && x->op != '-' && x->op != '*' && x->op != '/') return -1;
    int l = lw_from_hn(x->l); if (l < 0) return -1;
    int r = lw_from_hn(x->r); if (r < 0) return -1;
    int neg = x->op == '-' || g_lw_n[l].neg || g_lw_n[r].neg;
    return lw_node(x->op, l, r, -1, 0, g_dsc[h], g_dbd[h], neg);     /* '/': scale and bound are the store's to work out */
}
/* an expression's value as a node, its bounds checked: -1 when not taken */
static int lw_expr(Expr *e, int top_div)
{
    g_nhn = 0;
    int root = hn_tree(e, lw_leaf);
    if (root < 0) return -1;
    int chk = g_dx_chk; g_dx_chk = 0;
    int ok = dx_check(root, top_div);
    g_dx_chk = chk;
    if (!ok) return -1;
    return lw_from_hn(root);
}
static int lw_opnd(const Opnd *o)
{
    if (o->kind == O_EXPR) return lw_expr(o->ex, 0);
    g_nhn = 0;
    int h = lw_leaf(o);
    if (h < 0) return -1;
    int chk = g_dx_chk; g_dx_chk = 0;
    int ok = dx_check(h, 0);
    g_dx_chk = chk;
    return ok ? lw_from_hn(h) : -1;
}
/* the node x op y, by the register tree's rules (dx_check), -1 when it
 * does not take it */
static int lw_binop(char op, int x, int y, int top)
{
    if (x < 0 || y < 0) return -1;
    LNode *a = &g_lw_n[x], *b = &g_lw_n[y];
    if (op == '/') return top ? lw_node('/', x, y, -1, 0, -1, 0, a->neg || b->neg) : -1;
    if (op == '*') {
        if (a->sc + b->sc > 18) return -1;
        long double bd = a->bd * b->bd;
        if (bd >= DX_LIM) return -1;
        return lw_node('*', x, y, -1, 0, a->sc + b->sc, bd, a->neg || b->neg);
    }
    int sc = a->sc > b->sc ? a->sc : b->sc;
    long double bl = a->bd * dx_p10(sc - a->sc), br = b->bd * dx_p10(sc - b->sc);
    if (bl >= DX_LIM || br >= DX_LIM || bl + br >= DX_LIM) return -1;
    return lw_node(op, x, y, -1, 0, sc, bl + br, op == '-' || a->neg || b->neg);
}
static int lw_recv_ok(const Ref *r) { return !r->sym->is_index && (lw_item_ok(r) || lw_mem_ok(r)) && !ref_pending(r); }

/* a statement's standing refusals; what the verb is, for the trace */
static const char *lw_stmt_refused(int size_err)
{
    if (size_err) return "SIZE ERROR";
    if (g_rmode) return "ROUNDED MODE";
    if (g_wide) return "wide";
    if (g_nohx) return "-fno-hot-arith";
    if (ec_on_name("EC-DATA-INCOMPATIBLE")) return "EC-DATA-INCOMPATIBLE on";
    return NULL;
}

/* a division at the root: the quotient's scale and bound by the stack's
 * rule (cob_xdivn), or 0 when the rule's answer depends on the values */
static int lw_div_shape(int n, int need, int *e_out, int *sc_out, long double *bd_out)
{
    LNode *d = &g_lw_n[n], *a = &g_lw_n[d->l], *b = &g_lw_n[d->r];
    if (a->bd >= DX_LIM || b->bd >= DX_LIM) return 0;
    int want = (a->sc > b->sc ? a->sc : b->sc) + 6;
    if (want < 9) want = 9;
    if (want > 18) want = 18;
    if (want > need) want = need;
    int scale = a->sc - b->sc, e = want > scale ? want - scale : 0;
    long double bq = a->bd * dx_p10(e);             /* the divisor is at least one unit of its scale */
    if (bq >= dx_p10(17)) return 0;                 /* the stack would stop short of want */
    *e_out = e; *sc_out = scale + e; *bd_out = bq;
    return 1;
}

/* REMAINDER r of a division into q: the dividend less the divisor times
 * the quotient truncated to q's decimals (X3.23 6.9.4; arith_reg.h
 * emit_remainder), the quotient the stack's (cob_ndiv, no receiver
 * limiting it).  Its scale is fixed when the stack's digits reach it
 * before the quotient fills 17 digits; the product and the difference
 * must stay in 64 bits.  *e2: digits the dividend is scaled by for that
 * quotient; *down: digits the quotient is then cut by; *sr: the
 * remainder's scale; *br its bound. */
static int lw_rem_shape(int n, const Sym *q, int *e2, int *down, int *sr, long double *br)
{
    LNode *d = &g_lw_n[n], *a = &g_lw_n[d->l], *b = &g_lw_n[d->r];
    int want = (a->sc > b->sc ? a->sc : b->sc) + 6;
    if (want < 9) want = 9;
    if (want > 18) want = 18;
    int qs = q->pi.scale < want ? (q->pi.scale > 0 ? q->pi.scale : 0) : want;
    int scale = a->sc - b->sc;
    *e2 = qs > scale ? qs - scale : 0;
    *down = scale > qs ? scale - qs : 0;
    long double bq = a->bd * dx_p10(*e2);
    if (bq >= dx_p10(17)) return 0;
    bq = bq / dx_p10(*down) + 1;
    long double bp = bq * b->bd;                    /* the product, at scale qs + b->sc */
    if (bp >= DX_LIM) return 0;
    int ps = qs + b->sc;
    *sr = ps > a->sc ? ps : a->sc;
    long double ba = a->bd * dx_p10(*sr - a->sc); bp *= dx_p10(*sr - ps);
    if (ba >= DX_LIM || bp >= DX_LIM || ba + bp >= DX_LIM) return 0;
    *br = ba + bp;
    return 1;
}

/* ---- the hooks: a statement as a node, or nothing ---------------------- */

static int lw_store_stmt(int expr, Ref *rs, int *rd, int nr, const Ref *rem, int line, const char *verb)
{
    for (int i = 0; i < nr; i++) if (!lw_recv_ok(&rs[i])) { lw_refuse(line, verb, "a receiver"); return 0; }
    LNode *x = &g_lw_n[expr];
    int need = 0;
    if (x->op == '/') {
        for (int i = 0; i < nr; i++) { int k = (rs[i].sym->pi.scale > 0 ? rs[i].sym->pi.scale : 0) + (rd[i] ? 1 : 0); if (k > need) need = k; }
        int e, sc; long double bd;
        if (!lw_div_shape(expr, need, &e, &sc, &bd)) { lw_refuse(line, verb, "the quotient's scale is the values'"); return 0; }
        if (rem) {
            int e2, down, sr; long double br;
            if (!lw_recv_ok(rem) || rem->nsub) { lw_refuse(line, verb, "the REMAINDER item"); return 0; }
            if (!lw_rem_shape(expr, rs[0].sym, &e2, &down, &sr, &br)) { lw_refuse(line, verb, "the remainder's bound"); return 0; }
        }
    }
    int st = lw_stmt(LS_STORE, line);
    LStmt *s = &g_lw_s[st]; s->expr = expr; s->need = need; s->nr = nr; s->rem = rem ? (int)(rem->sym - g_sym) : -1;
    for (int i = 0; i < nr; i++) { s->rsym[i] = (int)(rs[i].sym - g_sym); s->rnd[i] = (unsigned char)(rd[i] != 0); s->rref[i] = rs[i].nsub ? lw_ref_keep(&rs[i]) : -1; }
    lw_place(st);
    return 1;
}

/* COMPUTE: after the expression and the phrases are read, before any code */
static int lw_compute(Ref *rs, int *rd, int nr, Expr *e, int size_err)
{
    if (lw_off()) return 0;
    const char *why = lw_stmt_refused(size_err);
    if (why) { lw_refuse(rs[0].line, "COMPUTE", why); return 0; }
    if (refs_pending(rs, nr)) { lw_refuse(rs[0].line, "COMPUTE", "a receiver's call"); return 0; }
    int x = lw_expr(e, 1);
    if (x < 0) { lw_refuse(rs[0].line, "COMPUTE", "the expression"); return 0; }
    return lw_store_stmt(x, rs, rd, nr, NULL, rs[0].line, "COMPUTE");
}

/* the operands' sum, left to right as the stack adds them */
static int lw_sum(Opnd *ops, int n)
{
    int r = lw_opnd(&ops[0]);
    for (int i = 1; i < n && r >= 0; i++) r = lw_binop('+', r, lw_opnd(&ops[i]), 0);
    return r;
}
/* ADD ... TO / SUBTRACT ... FROM: each receiver takes its value plus (less) the sum */
static int lw_addto(int sum, Ref *rs, int *rd, int nr, int subtract, int line, const char *verb)
{
    for (int i = 0; i < nr; i++) {
        if (!lw_recv_ok(&rs[i])) { lw_refuse(line, verb, "a receiver"); return 0; }
        Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = rs[i]; o.line = rs[i].line;
        int leaf = lw_opnd(&o);
        if (lw_binop(subtract ? '-' : '+', leaf, sum, 0) < 0) { lw_refuse(line, verb, "the sum's bound"); return 0; }
    }
    int st = lw_stmt(LS_ADDTO, line);
    LStmt *s = &g_lw_s[st]; s->expr = sum; s->nr = nr; s->subtract = subtract;
    for (int i = 0; i < nr; i++) { s->rsym[i] = (int)(rs[i].sym - g_sym); s->rnd[i] = (unsigned char)(rd[i] != 0); s->rref[i] = rs[i].nsub ? lw_ref_keep(&rs[i]) : -1; }
    lw_place(st);
    return 1;
}
/* ADD, SUBTRACT, MULTIPLY, DIVIDE as nodes (arith_reg.h): after the
 * phrases are read, before any code.  verb: 'A' 'S' 'M' 'D'. */
static int lw_arith(Arith *st, char verb)
{
    if (lw_off()) return 0;
    const char *name = verb == 'A' ? "ADD" : verb == 'S' ? "SUBTRACT" : verb == 'M' ? "MULTIPLY" : "DIVIDE";
    int line = st->rs[0].line;
    const char *why = lw_stmt_refused(st->size_err);
    if (why) { lw_refuse(line, name, why); return 0; }
    if (refs_pending(st->rs, st->nr) || (st->has_rem && ref_pending(&st->rem))) { lw_refuse(line, name, "a receiver's call"); return 0; }
    if (verb == 'A' || verb == 'S') {
        int sum = lw_sum(st->ops, st->n);
        if (sum < 0) { lw_refuse(line, name, "an operand"); return 0; }
        if (!st->giving) return lw_addto(sum, st->rs, st->rd, st->nr, verb == 'S', line, name);
        int x = verb == 'S' ? lw_binop('-', lw_opnd(&st->minuend), sum, 0) : sum;
        if (x < 0) { lw_refuse(line, name, "the bound"); return 0; }
        return lw_store_stmt(x, st->rs, st->rd, st->nr, NULL, line, name);
    }
    int a = lw_opnd(&st->a);
    if (a < 0) { lw_refuse(line, name, "an operand"); return 0; }
    if (st->giving) {
        int b = lw_opnd(&st->b);
        if (b < 0) { lw_refuse(line, name, "an operand"); return 0; }
        int x = verb == 'M' ? lw_binop('*', a, b, 0) : st->into ? lw_binop('/', b, a, 1) : lw_binop('/', a, b, 1);
        if (x < 0) { lw_refuse(line, name, "the bound"); return 0; }
        return lw_store_stmt(x, st->rs, st->rd, st->nr, st->has_rem ? &st->rem : NULL, line, name);
    }
    /* MULTIPLY a BY r ...; DIVIDE a INTO r ...: each receiver its own statement */
    int first = g_lw_ns, nplace = g_nasm;
    for (int i = 0; i < st->nr; i++) {
        if (!lw_recv_ok(&st->rs[i])) { lw_refuse(line, name, "a receiver"); goto undo; }
        Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = st->rs[i]; o.line = st->rs[i].line;
        int r = lw_opnd(&o);
        int x = verb == 'M' ? lw_binop('*', r, a, 0) : lw_binop('/', r, a, 1);
        if (x < 0) { lw_refuse(line, name, "the bound"); goto undo; }
        if (!lw_store_stmt(x, &st->rs[i], &st->rd[i], 1, NULL, line, name)) goto undo;
    }
    return 1;
undo:
    g_lw_ns = first; g_nasm = nplace;
    return 0;
}

/* MOVE of a number to numeric items: the store, truncating */
static int lw_move(Opnd *src, Ref *dst, int n)
{
    if (lw_off()) return 0;
    const char *why = lw_stmt_refused(0);
    if (why) { lw_refuse(src->line, "MOVE", why); return 0; }
    if (n > MAXOPS) return 0;
    {   /* bytes to bytes, every length known */
        long sl, dl; int bytes = lw_bytes_src_ok(src, &sl);
        for (int i = 0; i < n && bytes; i++) bytes = lw_bytes_dst_ok(&dst[i], &dl);
        if (bytes) {
            int st = lw_stmt(LS_AMOVE, src->line);
            LStmt *s = &g_lw_s[st]; s->asrc = lw_opnd_keep(src); s->nr = n;
            for (int i = 0; i < n; i++) s->adst[i] = lw_ref_keep(&dst[i]);
            lw_place(st);
            return 1;
        }
    }
    if (src->kind != O_REF && src->kind != O_NUM && src->kind != O_FIG) return 0;
    if (src->kind == O_REF && !lw_opnd_item_ok(&src->ref)) { lw_refuse(src->line, "MOVE", "the sender"); return 0; }
    for (int i = 0; i < n; i++) if (!lw_recv_ok(&dst[i])) { lw_refuse(src->line, "MOVE", "a receiver"); return 0; }
    int x = lw_opnd(src);
    if (x < 0) { lw_refuse(src->line, "MOVE", "the sender"); return 0; }
    int rd[MAXOPS] = { 0 };
    return lw_store_stmt(x, dst, rd, n, NULL, src->line, "MOVE");
}

/* DISPLAY of literals and items, to the console, ADVANCING or not: its
 * operands read (parse_display), its code written from line a0 on.  An
 * item is displayed from its storage, so a native one is stored first. */
static int lw_disp_ref_ok(const Ref *r)
{
    Sym *s = r->sym;
    long len;
    if (r->rm) return lw_bytes_ref_ok(r, &len) && len >= 0;
    if (s->is_cond || s->any_len || s->dynl || sym_bitlike(s) || s->natgroup || s->nat_usage || s->usage == U_NATIONAL || s->usage == U_BIT || s->is_index) return 0;
    if (s->pi.category == PIC_NATIONAL || s->pi.category == PIC_BOOLEAN || s->is_rc || s->lin_file >= 0 || s->rep_ctr >= 0 || s->usage == U_FLOAT || s->usage == U_DFLOAT) return 0;
    if (s->is_group && (has_odo(s) || s->bitgroup || s->strong)) return 0;
    const Sym *rec = &g_sym[s->record];
    if (rec_indirect(rec) || !rec->label[0] || rec->ftemp_scan || odo_table_for(s) || dyn_table_for(s)) return 0;
    if (r->nsub != s->ndims || (r->nsub && ec_on_name("EC-BOUND-SUBSCRIPT"))) return 0;
    for (int k = 0; k < r->nsub; k++)
        if (r->sub[k].sym == &g_subx || (r->sub[k].sym && !lw_sub_item_ok(r->sub[k].sym))) return 0;
    return 1;
}
static int lw_display(Opnd *ops, int n, int no_adv, int a0)
{
    if (lw_off()) return 0;
    const char *why = lw_stmt_refused(0);
    if (why) { lw_refuse(ops[0].line, "DISPLAY", why); return 0; }
    if (n > MAXOPS) return 0;
    for (int i = 0; i < n; i++) {
        Opnd *o = &ops[i];
        int ok = !opnd_scanned(o) && !o->all_sub &&
                 (o->kind == O_STR ? !o->tok->nat : o->kind == O_NUM ? !o->folded : o->kind == O_FIG || o->kind == O_ALL ? 1 :
                  o->kind == O_REF ? lw_disp_ref_ok(&o->ref) : 0);
        if (!ok) { lw_refuse(o->line, "DISPLAY", "an operand"); return 0; }
    }
    int st = lw_stmt(LS_DISPLAY, ops[0].line);
    LStmt *s = &g_lw_s[st]; s->nr = n; s->asrc = no_adv;
    for (int i = 0; i < n; i++) s->adst[i] = lw_opnd_keep(&ops[i]);
    lw_place_at(a0, st);
    return 1;
}

/* a condition as a node, -1 when not taken */
static int lw_cond(Cond *c)
{
    if (c->uc1 > c->uc0) return -1;
    if (c->kind == C_AND || c->kind == C_OR) {
        int a = lw_cond(c->a); if (a < 0) return -1;
        int b = lw_cond(c->b); if (b < 0) return -1;
        return lw_cnode(c->kind, a, b, -1, -1, 0);
    }
    if (c->kind == C_NOT) { int a = lw_cond(c->a); return a < 0 ? -1 : lw_cnode(C_NOT, a, -1, -1, -1, 0); }
    if (c->kind != C_REL || c->ptr || c->bstack) return -1;
    {   /* two operands of bytes under the native collating sequence: equal
         * or not, a literal shorter or longer than the item padded with
         * spaces here, as the comparison pads (8.8.4.1.2); and any relation
         * of two items the text emitter compares bytewise (cmp_is_bytewise:
         * one length, or one descriptor -- two unsigned DISPLAY numbers
         * among them, whose bytes order as their values do when both are
         * numbers, and which the text compares as bytes whatever they hold) */
        long lx, ly;
        int bx = c->x.kind != O_FIG && lw_bytes_src_ok(&c->x, &lx), by = c->y.kind != O_FIG && lw_bytes_src_ok(&c->y, &ly);
        int fx = c->x.kind == O_FIG && lw_bytes_src_ok(&c->x, &lx), fy = c->y.kind == O_FIG && lw_bytes_src_ok(&c->y, &ly);
        int bytewise = 0;
        if (c->x.kind == O_REF && c->y.kind == O_REF && !c->x.ref.rm && !c->y.ref.rm && cmp_is_bytewise(&c->x, &c->y)) {
            g_lw_bytes_any = 1;
            bytewise = lw_bytes_ref_ok(&c->x.ref, &lx) && lw_bytes_ref_ok(&c->y.ref, &ly) && lx >= 0 && ly >= 0;
            g_lw_bytes_any = 0;
        }
        int op = c->op;
        if (c->neg) op = op == R_EQ ? R_NE : op == R_NE ? R_EQ : op == R_LT ? R_GE : op == R_GE ? R_LT : op == R_GT ? R_LE : R_GT;
        if (bytewise || ((bx || fx) && (by || fy) && (bx || by) && g_collate < 0 && (c->op == R_EQ || c->op == R_NE))) {
            /* a whole numeric item is a number, compared as one, unless both are and of one description */
            int numx = c->x.kind == O_REF && !c->x.ref.rm && is_numeric_sym(c->x.ref.sym);
            int numy = c->y.kind == O_REF && !c->y.ref.rm && is_numeric_sym(c->y.ref.sym);
            int lit = (c->x.kind != O_REF) + (c->y.kind != O_REF);
            if (bytewise || (!numx && !numy && lit < 2 && (lit || lx == ly))) {
                int n = lw_cnode(C_REL, -1, -1, -1, -1, op);
                g_lw_c[n].alnum = 1; g_lw_c[n].ax = lw_opnd_keep(&c->x); g_lw_c[n].ay = lw_opnd_keep(&c->y);
                return n;
            }
        }
    }
    /* a numeric-edited item is not numeric in a relation (8.8.4.1: the
     * comparison is alphanumeric -- a condition-name over one, free/setcond) */
    if (c->x.kind == O_REF && c->x.ref.sym->pi.category == PIC_NUMERIC_EDITED) return -1;
    if (c->y.kind == O_REF && c->y.ref.sym->pi.category == PIC_NUMERIC_EDITED) return -1;
    int x = lw_opnd(&c->x); if (x < 0) return -1;
    int y = lw_opnd(&c->y); if (y < 0) return -1;
    /* the two aligned must be within bounds, as a sum's sides are */
    if (lw_binop('-', x, y, 0) < 0) return -1;
    int op = c->op;
    if (c->neg) op = op == R_EQ ? R_NE : op == R_NE ? R_EQ : op == R_LT ? R_GE : op == R_GE ? R_LT : op == R_GT ? R_LE : R_GT;
    return lw_cnode(C_REL, -1, -1, x, y, op);
}


/* IF: its branches read, before its code */
static int lw_if(IfStmt *s)
{
    if (lw_off() || s->then_ns || s->else_ns) return 0;
    int line = cur()->line;
    const char *why = lw_stmt_refused(0);
    if (why) { lw_refuse(line, "IF", why); return 0; }
    int c = lw_cond(s->c);
    if (c < 0) { lw_refuse(line, "IF", "the condition"); return 0; }
    int b, nb, e = 0, ne = 0;
    if (!lw_block_stmts(&s->then_b, &b, &nb)) { lw_refuse(line, "IF", "a statement in THEN"); return 0; }
    if (s->has_else && !lw_block_stmts(&s->else_b, &e, &ne)) { lw_refuse(line, "IF", "a statement in ELSE"); g_lw_nlist = b; return 0; }
    int st = lw_stmt(LS_IF, line);
    LStmt *x = &g_lw_s[st]; x->cond = c; x->body = b; x->nbody = nb; x->els = e; x->nels = ne;
    lw_place(st);
    return 1;
}

/* GO TO a paragraph (goto_set.h, the plain form): a node whose code is a
 * branch to the paragraph's block -- which exists only inside an inlined
 * PERFORM of a range holding the target (lw_gen_inlined); a run with a
 * GO TO anywhere else stays text */
static int lw_go_to(int target)
{
    if (lw_off()) return 0;
    int st = lw_stmt(LS_GOTO, cur()->line);
    g_lw_s[st].gto = target;
    lw_place(st);
    return 1;
}
/* are the GO TOs among the statements (not inside inlined PERFORMs, which
 * answered for their own) all to paragraphs lo..hi? */
static int lw_gotos_ok(int at, int n, int lo, int hi)
{
    for (int i = 0; i < n; i++) {
        const LStmt *s = &g_lw_s[g_lw_list[at + i]];
        if (s->kind == LS_GOTO) { if (s->gto < lo || s->gto > hi) return 0; continue; }
        if (s->kind == LS_TEXT && s->inl) continue;
        if (!lw_gotos_ok(s->body, s->nbody, lo, hi) || !lw_gotos_ok(s->els, s->nels, lo, hi)) return 0;
        if (s->kind == LS_TEXT && s->phrase >= 0) { const LStmt *ph = &g_lw_s[s->phrase]; if (!lw_gotos_ok(ph->body, ph->nbody, lo, hi) || !lw_gotos_ok(ph->els, ph->nels, lo, hi)) return 0; }
    }
    return 1;
}

/* EVALUATE: its WHENs read (verbs.h, each a condition over the subjects
 * and a body), before its code -- a chain of IF nodes, each WHEN's
 * condition and body, the next WHEN its ELSE, WHEN OTHER the last ELSE.
 * The tests run in order before any body, so a subject is read once as
 * the standard has it; a WHEN whose objects made code of their own (a
 * user function's call, pre) is not taken. */
static int lw_evaluate(int b0, const Block *pre, const Block *body, Cond **c, const int *other, int nwh)
{
    if (lw_off() || nwh == 0) return 0;
    int line = cur()->line;
    const char *why = lw_stmt_refused(0);
    if (why) { lw_refuse(line, "EVALUATE", why); return 0; }
    /* a subject computed first (an expression, a function) is code before
     * the node: the statement stays text */
    for (int i = b0; i < g_nasm; i++)
        if (g_asm[i][0] == '\t' && g_asm[i][1] != '.' && g_asm[i][1] != '#') { lw_refuse(line, "EVALUATE", "a subject's code"); return 0; }
    int nl0 = g_lw_nlist;
    int cn[64], bat[64], bn[64];
    if (nwh > 64) return 0;
    for (int i = 0; i < nwh; i++) {
        int code = 0; for (int j = 0; j < pre[i].n; j++) if (pre[i].line[j][0] == '\t' && pre[i].line[j][1] != '.') code = 1;
        if (code) { lw_refuse(line, "EVALUATE", "a WHEN object's code"); g_lw_nlist = nl0; return 0; }
        cn[i] = other[i] ? -1 : lw_cond(c[i]);
        if (!other[i] && cn[i] < 0) { lw_refuse(line, "EVALUATE", "a WHEN's condition"); g_lw_nlist = nl0; return 0; }
        if (!lw_block_stmts(&body[i], &bat[i], &bn[i])) { lw_refuse(line, "EVALUATE", "a statement in a WHEN"); g_lw_nlist = nl0; return 0; }
    }
    /* from the last WHEN back: each IF's ELSE is the chain after it */
    int e = 0, ne = 0;
    for (int i = nwh - 1; i >= 0; i--) {
        if (other[i]) { e = bat[i]; ne = bn[i]; continue; }
        int st = lw_stmt(LS_IF, line);
        LStmt *x = &g_lw_s[st]; x->cond = cn[i]; x->body = bat[i]; x->nbody = bn[i]; x->els = e; x->nels = ne;
        e = g_lw_nlist; lw_list_add(st); ne = 1;
    }
    if (ne != 1 || g_lw_s[g_lw_list[e]].kind != LS_IF) { g_lw_nlist = nl0; return 0; }   /* WHEN OTHER alone */
    lw_place(g_lw_list[e]);
    return 1;
}

/* an in-line PERFORM VARYING (one level) or UNTIL, its body read */
static int lw_perform(Vary *v, int nv, Cond *until, Body *body, int test_after)
{
    if (!body->inline_body) return 0;
    if (lw_off()) { if (lw_trace() && g_hir_on && !g_cen_on) fprintf(stderr, "hir: line %d PERFORM: not lowered: the hook is off (noemit %d, calls %d)\n", cur()->line, g_noemit, g_stmt_calls.n); return 0; }
    int line = cur()->line;
    const char *why = lw_stmt_refused(0);
    if (why) { lw_refuse(line, "PERFORM", why); return 0; }
    int var[8], from[8], by[8], vcond[8], c = -1;
    if (v) {
        if (nv > 1 && test_after) { lw_refuse(line, "PERFORM", "AFTER with TEST AFTER"); return 0; }
        for (int k = 0; k < nv; k++) {
            if (!(lw_recv_ok(&v[k].var) || (v[k].var.sym->is_index && lw_mem_ok(&v[k].var) && !ec_on_name("EC-RANGE-PERFORM-VARYING"))) || v[k].var.nsub) { lw_refuse(line, "PERFORM", "the VARYING item"); return 0; }   /* an index-name FROM a value that is not positive is an EC with checking on (2002/perfvary) */
            var[k] = (int)(v[k].var.sym - g_sym);
            from[k] = lw_opnd(&v[k].from); by[k] = lw_opnd(&v[k].by);
            if (from[k] < 0 || by[k] < 0) { lw_refuse(line, "PERFORM", "FROM or BY"); return 0; }
            Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = v[k].var; o.line = line;
            if (lw_binop('+', lw_opnd(&o), by[k], 0) < 0) { lw_refuse(line, "PERFORM", "the step's bound"); return 0; }
            vcond[k] = lw_cond(v[k].until);
            if (vcond[k] < 0) { lw_refuse(line, "PERFORM", "the condition"); return 0; }
        }
    } else {
        nv = 0;
        c = lw_cond(until);
        if (c < 0) { lw_refuse(line, "PERFORM", "the condition"); return 0; }
    }
    int b, nb;
    if (!lw_block_stmts(&body->blk, &b, &nb)) { lw_refuse(line, "PERFORM", "a statement in the body"); return 0; }
    int st = lw_stmt(LS_LOOP, line);
    LStmt *x = &g_lw_s[st]; x->cond = c; x->body = b; x->nbody = nb; x->nv = nv; x->test_after = test_after;
    for (int k = 0; k < nv; k++) { x->var[k] = var[k]; x->from[k] = from[k]; x->by[k] = by[k]; x->vcond[k] = vcond[k]; }
    lw_place(st);
    return 1;
}

/* ---- HIR for an island -------------------------------------------------- */

typedef struct { int lo, hi; } LV;      /* a value: a word (hi < 0, its sign the value's) or a pair */

static int lw_frame;                    /* the island's frame: 8 for the saved r31, r30, then the allocas */
static int lw_blk_live;
typedef struct { int sym, a_lo, a_hi, written; } LwItem;
static LwItem g_lw_item[256]; static int g_lw_nitem;

static int lw_iconst(int v) { return hi_emit(HI_ICONST, TY_INT, -1, -1, v, NULL); }
/* a literal: a word when it fits one, whatever width is asked -- the
 * pair operations widen what they are handed, and a product of two words
 * is one MULH where a pair's is three multiplications */
static LV lw_lit(long long v, int wide)
{
    LV r; r.lo = lw_iconst((int)(unsigned)(unsigned long long)v); r.hi = -1;
    if (wide && (v < -2147483647LL - 1 || v > 2147483647LL)) r.hi = lw_iconst((int)(unsigned)((unsigned long long)v >> 32));
    return r;
}
static LV lw_widen(LV v) { if (v.hi < 0) v.hi = hi_emit(HI_SRA, TY_INT, v.lo, lw_iconst(31), 0, NULL); return v; }
static int lw_alloca(void)
{
    lw_frame += 4;
    int a = hi_emit(HI_ALLOCA, TY_INT, -1, -1, -lw_frame, NULL);
    if (hl_nalloca >= HL_MAX_ALLOCA) die_at(cur()->line, "internal: an island has too many items");
    hl_ainst[hl_nalloca] = a; hl_aoff[hl_nalloca] = -lw_frame; hl_aslot[hl_nalloca] = 0; hl_nalloca++;
    return a;
}
static void lw_begin_blk(int b) { hl_switch_block(b); lw_blk_live = 1; }
static void lw_goto(int b) { if (lw_blk_live) hi_emit(HI_BR, TY_VOID, -1, -1, b, NULL); lw_blk_live = 0; }
static void lw_brc(int c, int bt, int bf) { hi_emit(HI_BRC, TY_VOID, c, bt, bf, NULL); lw_blk_live = 0; }

/* the island's items: found before any code, so their allocas come first */
static int g_lw_items_closed;                   /* the entry loads are emitted: no item may join now */
static int lw_item_slot(int sym)
{
    for (int i = 0; i < g_lw_nitem; i++) if (g_lw_item[i].sym == sym) return i;
    if (g_lw_items_closed) die_at(cur()->line, "internal: island item %s named by code the collector did not see", g_sym[sym].name);
    if (g_lw_nitem == 256) die_at(cur()->line, "internal: an island names too many items");
    LwItem *it = &g_lw_item[g_lw_nitem]; it->sym = sym; it->written = 0;
    it->a_lo = lw_alloca(); it->a_hi = g_sym[sym].size == 8 ? lw_alloca() : -1;
    return g_lw_nitem++;
}
static void lw_collect_opnd(int oi);
static void lw_collect_node(int n)
{
    if (n < 0) return;
    LNode *x = &g_lw_n[n];
    if (!x->op) { if (g_sym[x->sym].native && x->ref < 0) lw_item_slot(x->sym); lw_collect_opnd(x->ref); return; }   /* a native table's element is a word in storage, not an alloca */
    if (x->op == 'k') return;
    lw_collect_node(x->l); lw_collect_node(x->r);
}
static void lw_collect_opnd(int oi)
{
    if (oi < 0) return;
    const Opnd *o = &g_lw_o[oi];
    if (o->kind != O_REF) return;
    for (int k = 0; k < o->ref.nsub; k++) if (o->ref.sub[k].sym && o->ref.sub[k].sym->native) lw_item_slot((int)(o->ref.sub[k].sym - g_sym));
    if (o->ref.rm && o->ref.lw_startx) lw_collect_node(o->ref.lw_startx - 1);
    if (o->ref.rm && o->ref.lw_lenx) lw_collect_node(o->ref.lw_lenx - 1);
}
static void lw_collect_cond(int c)
{
    LCond *x = &g_lw_c[c];
    if (x->kind == C_REL && x->alnum) { lw_collect_opnd(x->ax); lw_collect_opnd(x->ay); return; }
    if (x->kind == C_REL) { lw_collect_node(x->x); lw_collect_node(x->y); return; }
    lw_collect_cond(x->a);
    if (x->kind != C_NOT) lw_collect_cond(x->b);
}
/* an item the island writes somewhere: known before any code, so a text
 * statement in the middle stores it before and loads it after (the flag
 * set as the stores were made left a READ loop's totals unstored before
 * its READ, and the reload after undid every ADD) */
static void lw_item_written(int sym) { g_lw_item[lw_item_slot(sym)].written = 1; }
static void lw_collect_stmts(int at, int n)
{
    for (int i = 0; i < n; i++) {
        LStmt *s = &g_lw_s[g_lw_list[at + i]];
        if (s->expr >= 0) lw_collect_node(s->expr);
        if (s->kind == LS_STORE || s->kind == LS_ADDTO) for (int k = 0; k < s->nr; k++) { if (g_sym[s->rsym[k]].native && s->rref[k] < 0) lw_item_written(s->rsym[k]); lw_collect_opnd(s->rref[k]); }
        if (s->rem >= 0 && g_sym[s->rem].native) lw_item_written(s->rem);
        if (s->kind == LS_AMOVE) { lw_collect_opnd(s->asrc); for (int k = 0; k < s->nr; k++) lw_collect_opnd(s->adst[k]); }
        if (s->kind == LS_DISPLAY)
            for (int k = 0; k < s->nr; k++) {
                lw_collect_opnd(s->adst[k]);
                const Opnd *o = &g_lw_o[s->adst[k]];
                if (o->kind == O_REF && o->ref.sym->native && !o->ref.nsub) lw_item_slot((int)(o->ref.sym - g_sym));
            }
        if (s->cond >= 0) lw_collect_cond(s->cond);
        for (int k = 0; k < s->nv; k++) {
            if (g_sym[s->var[k]].native) lw_item_written(s->var[k]);
            lw_collect_node(s->from[k]); lw_collect_node(s->by[k]); lw_collect_cond(s->vcond[k]);
        }
        lw_collect_stmts(s->body, s->nbody);
        lw_collect_stmts(s->els, s->nels);
        if (s->kind == LS_TEXT && s->phrase >= 0) { LStmt *ph = &g_lw_s[s->phrase]; lw_collect_stmts(ph->body, ph->nbody); lw_collect_stmts(ph->els, ph->nels); }
    }
}

/* the item's storage */
static int lw_item_addr(const Sym *s)
{
    int a = hi_emit(HI_GADDR, TY_INT, -1, -1, 0, g_sym[s->record].label);
    return s->offset ? hi_emit(HI_ADDI, TY_INT, a, -1, s->offset, NULL) : a;
}
static int lw_item_ty(const Sym *s)
{
    int ty = s->size == 1 ? TY_CHAR : s->size == 2 ? TY_SHORT : TY_INT;
    return s->pi.is_signed || s->size == 4 ? ty : ty | TY_UNSIGNED;    /* an unsigned 9(9) is below 2^31 */
}
static const unsigned char *g_lw_touch;         /* which items the text node being emitted may touch (lw_gen_text), or NULL: all */
static void lw_entry_loads(void)
{
    for (int i = 0; i < g_lw_nitem; i++) {
        if (g_lw_touch && !g_lw_touch[i]) continue;
        LwItem *it = &g_lw_item[i]; const Sym *s = &g_sym[it->sym];
        int a = lw_item_addr(s);
        hi_emit(HI_STORE, TY_INT, it->a_lo, hi_emit(HI_LOAD, lw_item_ty(s), a, -1, 0, NULL), 0, NULL);
        if (it->a_hi >= 0) hi_emit(HI_STORE, TY_INT, it->a_hi, hi_emit(HI_LOAD, TY_INT, hi_emit(HI_ADDI, TY_INT, a, -1, 4, NULL), -1, 0, NULL), 0, NULL);
    }
}
static void lw_exit_stores(void)
{
    for (int i = 0; i < g_lw_nitem; i++) {
        if (g_lw_touch && !g_lw_touch[i]) continue;
        LwItem *it = &g_lw_item[i]; const Sym *s = &g_sym[it->sym];
        if (!it->written) continue;
        int a = lw_item_addr(s);
        hi_emit(HI_STORE, lw_item_ty(s), a, hi_emit(HI_LOAD, TY_INT, it->a_lo, -1, 0, NULL), 0, NULL);
        if (it->a_hi >= 0) hi_emit(HI_STORE, TY_INT, hi_emit(HI_ADDI, TY_INT, a, -1, 4, NULL), hi_emit(HI_LOAD, TY_INT, it->a_hi, -1, 0, NULL), 0, NULL);
    }
}
static int lw_desc_addr(Sym *s)
{
    char b[32]; snprintf(b, sizeof b, ".Ld%d", sym_desc(s));
    return hi_emit(HI_GADDR, TY_INT, -1, -1, 0, xstrndup(b, strlen(b)));
}
static int lw_locale_word(void)
{
    return hi_emit(HI_LOAD, TY_INT, hi_emit(HI_GADDR, TY_INT, -1, -1, 0, "cob_locale_word"), -1, 0, NULL);
}
static LV lw_call(const char *fn, int *args, int n)
{
    int cb = h_ncarg;
    for (int i = 0; i < n; i++) h_carg[h_ncarg++] = args[i];
    LV r; r.lo = hi_emit(HI_CALL, TY_INT, -1, -1, n, (char *)fn);
    h_cbase[r.lo] = cb;
    r.hi = hi_emit(HI_CALLHI, TY_INT, r.lo, -1, 0, NULL);
    return r;
}
/* an item's value: a native one from its alloca, one in storage fetched
 * by the runtime, as the register trees fetch it (dx_emit) */
/* a DISPLAY or packed integer of at most nine digits in storage, read
 * digit by digit as the text's emit_dec_load reads it (any byte's low
 * nibble a digit; a trailing overpunch 'p'..'y' or a D nibble the
 * sign) -- the text's word path does this in line, and an island that
 * called cob_get_num for it instead cost csv2fw 5% on its two-digit
 * state item */
static int lw_dec_inline_ok(Sym *s) { return sym_dec_ok(s) && s->pi.digits <= 9; }
static LV lw_dec_load(Sym *s, int a)
{
    int D = s->pi.digits, v = -1, sg;
    if (s->usage == U_DISPLAY) {
        for (int d = 0; d < D; d++) {
            int b = hi_emit(HI_LOAD, TY_CHAR | TY_UNSIGNED, d ? hi_emit(HI_ADDI, TY_INT, a, -1, d, NULL) : a, -1, 0, NULL);
            b = hi_emit(HI_AND, TY_INT, b, lw_iconst(15), 0, NULL);
            v = v < 0 ? b : hi_emit(HI_ADD, TY_INT, hi_emit(HI_MUL, TY_INT, v, lw_iconst(10), 0, NULL), b, 0, NULL);
        }
        if (!s->pi.is_signed) { LV r; r.lo = v; r.hi = -1; return r; }
        int last = hi_emit(HI_LOAD, TY_CHAR | TY_UNSIGNED, D > 1 ? hi_emit(HI_ADDI, TY_INT, a, -1, D - 1, NULL) : a, -1, 0, NULL);
        sg = hi_emit(HI_SLTU, TY_INT, lw_iconst(111), last, 0, NULL);     /* 'p' (112) and above: negative */
    } else {
        int k0 = 2 * (int)s->size - 1 - D, cur = -1, byte = -1;
        for (int d = 0; d < D; d++) {
            int k = k0 + d, bi = k / 2;
            if (bi != cur) { byte = hi_emit(HI_LOAD, TY_CHAR | TY_UNSIGNED, bi ? hi_emit(HI_ADDI, TY_INT, a, -1, bi, NULL) : a, -1, 0, NULL); cur = bi; }
            int b = k % 2 == 0 ? hi_emit(HI_SRL, TY_INT, byte, lw_iconst(4), 0, NULL) : hi_emit(HI_AND, TY_INT, byte, lw_iconst(15), 0, NULL);
            v = v < 0 ? b : hi_emit(HI_ADD, TY_INT, hi_emit(HI_MUL, TY_INT, v, lw_iconst(10), 0, NULL), b, 0, NULL);
        }
        if (!s->pi.is_signed) { LV r; r.lo = v; r.hi = -1; return r; }
        if (cur != (int)s->size - 1) byte = hi_emit(HI_LOAD, TY_CHAR | TY_UNSIGNED, hi_emit(HI_ADDI, TY_INT, a, -1, (int)s->size - 1, NULL), -1, 0, NULL);
        sg = hi_emit(HI_SEQ, TY_INT, hi_emit(HI_AND, TY_INT, byte, lw_iconst(15), 0, NULL), lw_iconst(13), 0, NULL);   /* the D nibble: negative */
    }
    /* v = sg ? -v : v, without a branch */
    int m = hi_emit(HI_SUB, TY_INT, lw_iconst(0), sg, 0, NULL);
    LV r; r.lo = hi_emit(HI_SUB, TY_INT, hi_emit(HI_XOR, TY_INT, v, m, 0, NULL), m, 0, NULL); r.hi = -1;
    return r;
}
/* the address of a receiver or value: its reference's when subscripted */
static int lw_ref_addr(const Ref *r);
static int lw_sym_addr(int sym, int ref) { return ref >= 0 ? lw_ref_addr(&g_lw_o[ref].ref) : lw_item_addr(&g_sym[sym]); }
static LV lw_item_val_ref(int sym, int ref)
{
    Sym *s = &g_sym[sym];
    if (s->is_index) { LV v; v.lo = hi_emit(HI_LOAD, TY_INT, lw_sym_addr(sym, ref), -1, 0, NULL); v.hi = -1; return v; }
    if (s->native && ref >= 0) {               /* a native table's element */
        int a = lw_sym_addr(sym, ref);
        LV v; v.lo = hi_emit(HI_LOAD, lw_item_ty(s), a, -1, 0, NULL); v.hi = -1;
        if (s->size == 8) v.hi = hi_emit(HI_LOAD, TY_INT, hi_emit(HI_ADDI, TY_INT, a, -1, 4, NULL), -1, 0, NULL);
        return v;
    }
    if (!s->native) {
        int addr = lw_sym_addr(sym, ref);
        if (lw_dec_inline_ok(s)) return lw_dec_load(s, addr);
        int a[3] = { addr, lw_desc_addr(s), 0 };
        if (s->pi.category == PIC_NUMERIC_EDITED) { a[2] = lw_locale_word(); return lw_call("cob_get_edited", a, 3); }
        return lw_call("cob_get_num", a, 2);
    }
    LwItem *it = &g_lw_item[lw_item_slot(sym)];
    LV v; v.lo = hi_emit(HI_LOAD, TY_INT, it->a_lo, -1, 0, NULL); v.hi = -1;
    if (it->a_hi >= 0) v.hi = hi_emit(HI_LOAD, TY_INT, it->a_hi, -1, 0, NULL);
    return v;
}
static LV lw_item_val(int sym) { return lw_item_val_ref(sym, -1); }
static void lw_item_set(int sym, LV v)
{
    LwItem *it = &g_lw_item[lw_item_slot(sym)];
    it->written = 1;
    hi_emit(HI_STORE, TY_INT, it->a_lo, v.lo, 0, NULL);
    if (it->a_hi >= 0) hi_emit(HI_STORE, TY_INT, it->a_hi, lw_widen(v).hi, 0, NULL);
}
/* v at scale sc into an item in storage: the runtime's store, which
 * aligns, rounds, truncates and edits (cob_put_num_x; cob_put_edited) */
static void lw_mem_store(int sym, int ref, int rounded, LV v, int sc)
{
    Sym *s = &g_sym[sym];
    v = lw_widen(v);
    int a[7] = { lw_sym_addr(sym, ref), lw_desc_addr(s), v.lo, v.hi, lw_iconst(sc), lw_iconst(rounded ? 1 : 0), 0 };
    if (s->pi.category == PIC_NUMERIC_EDITED) { a[6] = lw_locale_word(); lw_call("cob_put_edited", a, 7); }
    else lw_call("cob_put_num_x", a, 6);
}

/* an item's bound, as dx_check has it: its picture's, or the binary
 * field's capacity when the usage keeps that (COMP-5) */
static long double lw_sym_bd(const Sym *s) { return sym_content_bound(s); }

/* ---- arithmetic on words and pairs ---- */

#define LW_WORD 2147483648.0L           /* a bound below this: a word holds the value */
static int lw_wide_bd(long double bd) { return bd >= LW_WORD; }

static LV lw_add64(LV a, LV b)
{
    a = lw_widen(a); b = lw_widen(b);
    LV r; r.lo = hi_emit(HI_ADD, TY_INT, a.lo, b.lo, 0, NULL);
    int c = hi_emit(HI_SLTU, TY_INT, r.lo, a.lo, 0, NULL);
    r.hi = hi_emit(HI_ADD, TY_INT, hi_emit(HI_ADD, TY_INT, a.hi, b.hi, 0, NULL), c, 0, NULL);
    return r;
}
static LV lw_sub64(LV a, LV b)
{
    a = lw_widen(a); b = lw_widen(b);
    LV r; int c = hi_emit(HI_SLTU, TY_INT, a.lo, b.lo, 0, NULL);
    r.lo = hi_emit(HI_SUB, TY_INT, a.lo, b.lo, 0, NULL);
    r.hi = hi_emit(HI_SUB, TY_INT, hi_emit(HI_SUB, TY_INT, a.hi, b.hi, 0, NULL), c, 0, NULL);
    return r;
}
static LV lw_call64(const char *fn, LV a, LV b)
{
    a = lw_widen(a); b = lw_widen(b);
    int cb = h_ncarg;
    h_carg[h_ncarg++] = a.lo; h_carg[h_ncarg++] = a.hi; h_carg[h_ncarg++] = b.lo; h_carg[h_ncarg++] = b.hi;
    LV r; r.lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, (char *)fn);
    h_cbase[r.lo] = cb;
    r.hi = hi_emit(HI_CALLHI, TY_INT, r.lo, -1, 0, NULL);
    return r;
}
/* a 64-bit product: of two words (each the sign-extension of its lo),
 * MUL and MULH; of pairs, the low 64 bits by MULHU and two MULs */
static LV lw_mul64(LV a, LV b)
{
    LV r;
    if (a.hi < 0 && b.hi < 0) {
        r.lo = hi_emit(HI_MUL, TY_INT, a.lo, b.lo, 0, NULL);
        r.hi = hi_emit(HI_MULH, TY_INT, a.lo, b.lo, 0, NULL);
        return r;
    }
    a = lw_widen(a); b = lw_widen(b);
    r.lo = hi_emit(HI_MUL, TY_INT, a.lo, b.lo, 0, NULL);
    int h = hi_emit(HI_MULHU, TY_INT, a.lo, b.lo, 0, NULL);
    h = hi_emit(HI_ADD, TY_INT, h, hi_emit(HI_MUL, TY_INT, a.lo, b.hi, 0, NULL), 0, NULL);
    r.hi = hi_emit(HI_ADD, TY_INT, h, hi_emit(HI_MUL, TY_INT, a.hi, b.lo, 0, NULL), 0, NULL);
    return r;
}
static LV lw_neg(LV v, int wide)
{
    if (!wide) { LV r; r.lo = hi_emit(HI_SUB, TY_INT, lw_iconst(0), v.lo, 0, NULL); r.hi = -1; return r; }
    LV z; z.lo = lw_iconst(0); z.hi = lw_iconst(0);
    return lw_sub64(z, v);
}
/* x op y at a common width; the result wide when wide */
static LV lw_arith2(char op, LV x, LV y, int wide)
{
    if (!wide) {
        LV r; r.hi = -1;
        r.lo = hi_emit(op == '+' ? HI_ADD : op == '-' ? HI_SUB : op == '*' ? HI_MUL : op == '/' ? HI_DIV : HI_REM, TY_INT, x.lo, y.lo, 0, NULL);
        return r;
    }
    if (op == '+') return lw_add64(x, y);
    if (op == '-') return lw_sub64(x, y);
    if (op == '*') return lw_mul64(x, y);
    return lw_call64(op == '/' ? "__divdi3" : "__moddi3", x, y);
}
static long long lw_p10(int k) { long long p = 1; while (k-- > 0) p *= 10; return p; }
/* v times 10^k, the result wide when wide */
static LV lw_scale(LV v, int k, int wide)
{
    if (k <= 0) return v;
    return lw_arith2('*', v, lw_lit(lw_p10(k), wide), wide);
}
/* |v| */
static LV lw_abs(LV v, int wide)
{
    if (!wide) {
        LV r; r.hi = -1;
        int s = hi_emit(HI_SRA, TY_INT, v.lo, lw_iconst(31), 0, NULL);
        r.lo = hi_emit(HI_SUB, TY_INT, hi_emit(HI_XOR, TY_INT, v.lo, s, 0, NULL), s, 0, NULL);
        return r;
    }
    v = lw_widen(v);
    LV s; s.lo = s.hi = hi_emit(HI_SRA, TY_INT, v.hi, lw_iconst(31), 0, NULL);
    LV x; x.lo = hi_emit(HI_XOR, TY_INT, v.lo, s.lo, 0, NULL); x.hi = hi_emit(HI_XOR, TY_INT, v.hi, s.hi, 0, NULL);
    return lw_sub64(x, s);
}
/* comparisons: a word, 0 or 1 */
static int lw_cmp(int op, LV x, LV y, int wide)
{
    if (!wide) {
        int k = op == R_EQ ? HI_SEQ : op == R_NE ? HI_SNE : op == R_LT ? HI_SLT : op == R_GT ? HI_SGT : op == R_LE ? HI_SLE : HI_SGE;
        return hi_emit(k, TY_INT, x.lo, y.lo, 0, NULL);
    }
    x = lw_widen(x); y = lw_widen(y);
    if (op == R_EQ || op == R_NE) {
        int d = hi_emit(HI_OR, TY_INT, hi_emit(HI_XOR, TY_INT, x.lo, y.lo, 0, NULL), hi_emit(HI_XOR, TY_INT, x.hi, y.hi, 0, NULL), 0, NULL);
        return hi_emit(op == R_EQ ? HI_SEQ : HI_SNE, TY_INT, d, lw_iconst(0), 0, NULL);
    }
    if (op == R_GT) { LV t = x; x = y; y = t; op = R_LT; }
    else if (op == R_LE) { LV t = x; x = y; y = t; op = R_GE; }
    /* x < y: the high words signed, the low ones unsigned when those tie */
    int lt = hi_emit(HI_OR, TY_INT, hi_emit(HI_SLT, TY_INT, x.hi, y.hi, 0, NULL),
                     hi_emit(HI_AND, TY_INT, hi_emit(HI_SEQ, TY_INT, x.hi, y.hi, 0, NULL), hi_emit(HI_SLTU, TY_INT, x.lo, y.lo, 0, NULL), 0, NULL), 0, NULL);
    return op == R_LT ? lt : hi_emit(HI_XOR, TY_INT, lt, lw_iconst(1), 0, NULL);
}

/* ---- values, stores, statements ---- */

static LV lw_val(int n);
/* the node's value, scaled up by k digits; its width that of bd.  A
 * literal is scaled where it stands, and a product's literal side takes
 * the scaling: one multiplication, not two. */
static LV lw_val_scaled(int n, int k, long double bd)
{
    LNode *x = &g_lw_n[n];
    int wide = lw_wide_bd(bd);
    if (k > 0 && x->op == 'k') return lw_lit(x->k * lw_p10(k), wide);
    if (k > 0 && x->op == '*' && (g_lw_n[x->l].op == 'k' || g_lw_n[x->r].op == 'k')) {
        int lit = g_lw_n[x->l].op == 'k' ? x->l : x->r, other = lit == x->l ? x->r : x->l;
        return lw_arith2('*', lw_val(other), lw_lit(g_lw_n[lit].k * lw_p10(k), wide), wide);
    }
    LV v = lw_scale(lw_val(n), k, wide);
    return wide ? lw_widen(v) : v;
}
static LV lw_val(int n)
{
    LNode *x = &g_lw_n[n];
    int wide = lw_wide_bd(x->bd);
    if (x->op == 'k') return lw_lit(x->k, wide);
    if (!x->op) { LV v = lw_item_val_ref(x->sym, x->ref); if (!wide) v.hi = -1; return v; }
    if (x->op == 'n') return lw_neg(lw_val(x->l), wide);
    LNode *a = &g_lw_n[x->l];
    if (x->op == 'A') return lw_abs(lw_val(x->l), lw_wide_bd(a->bd));
    if (x->op == 'R' || x->op == 'M') {
        /* the remainder, the dividend's sign; MOD: a nonzero one of the
         * other sign than the divisor's takes the divisor */
        int w = lw_wide_bd(a->bd);
        LV v = lw_val(x->l), d = lw_widen(lw_lit(x->k, w));
        if (!w) d.hi = -1;
        LV r = lw_arith2('%', v, d, w);
        if (x->op == 'R') return r;
        int mask;
        if (x->k > 0) mask = hi_emit(HI_SRA, TY_INT, w ? r.hi : r.lo, lw_iconst(31), 0, NULL);          /* r < 0 */
        else {
            int gt;                                                                                 /* r > 0 */
            if (!w) gt = hi_emit(HI_SGT, TY_INT, r.lo, lw_iconst(0), 0, NULL);
            else gt = lw_cmp(R_GT, r, lw_lit(0, 1), 1);
            mask = hi_emit(HI_SUB, TY_INT, lw_iconst(0), gt, 0, NULL);
        }
        LV adj; adj.lo = hi_emit(HI_AND, TY_INT, d.lo, mask, 0, NULL); adj.hi = w ? hi_emit(HI_AND, TY_INT, d.hi, mask, 0, NULL) : -1;
        return lw_arith2('+', r, adj, w);
    }
    if (x->op == 'I' || x->op == 'T') {
        /* to an integer: truncated; INTEGER is the floor, so a negative
         * value less P - 1 first */
        int w = lw_wide_bd(a->bd);
        LV v = lw_val(x->l);
        if (a->sc <= 0) return v;
        long long P = lw_p10(a->sc);
        if (x->op == 'I') {
            int mask = hi_emit(HI_SRA, TY_INT, w ? lw_widen(v).hi : v.lo, lw_iconst(31), 0, NULL);
            LV pm = lw_widen(lw_lit(P - 1, w));
            if (!w) pm.hi = -1;
            LV adj; adj.lo = hi_emit(HI_AND, TY_INT, pm.lo, mask, 0, NULL); adj.hi = w ? hi_emit(HI_AND, TY_INT, pm.hi, mask, 0, NULL) : -1;
            v = lw_arith2('-', v, adj, w);
        }
        return lw_arith2('/', v, lw_lit(P, w), w);
    }
    LNode *b = &g_lw_n[x->r];
    if (x->op == 'G' || x->op == 'L') {
        /* the greater (the less) of the two, aligned: taken by a mask */
        long double bd = a->bd * dx_p10(x->sc - a->sc) + b->bd * dx_p10(x->sc - b->sc);
        int w = lw_wide_bd(bd);
        LV l = lw_val_scaled(x->l, x->sc - a->sc, bd), r = lw_val_scaled(x->r, x->sc - b->sc, bd);
        if (w) { l = lw_widen(l); r = lw_widen(r); }
        int c = lw_cmp(x->op == 'G' ? R_GT : R_LT, r, l, w);
        int mask = hi_emit(HI_SUB, TY_INT, lw_iconst(0), c, 0, NULL);
        LV o; o.lo = hi_emit(HI_XOR, TY_INT, l.lo, hi_emit(HI_AND, TY_INT, hi_emit(HI_XOR, TY_INT, l.lo, r.lo, 0, NULL), mask, 0, NULL), 0, NULL);
        o.hi = w ? hi_emit(HI_XOR, TY_INT, l.hi, hi_emit(HI_AND, TY_INT, hi_emit(HI_XOR, TY_INT, l.hi, r.hi, 0, NULL), mask, 0, NULL), 0, NULL) : -1;
        if (!wide) o.hi = -1;
        return o;
    }
    if (x->op == '*') return lw_arith2('*', lw_val(x->l), lw_val(x->r), wide);
    if (x->op == '/') die_at(cur()->line, "internal: a division below the root of an island's tree");
    /* + -: aligned to the larger scale */
    LV l = lw_val_scaled(x->l, x->sc - a->sc, x->bd), r = lw_val_scaled(x->r, x->sc - b->sc, x->bd);
    return lw_arith2(x->op, l, r, wide);
}

/* v less its digits above the first n: the remainder by 10^n, taken only
 * when |v| reaches 10^n -- a value rarely does, and a 64-bit remainder is
 * a routine.  The join is a temporary the SSA pass promotes. */
static LV lw_trunc_digits(LV v, int n, int wide, int neg)
{
    LV lim = lw_lit(lw_p10(n), wide);
    int t_lo = lw_alloca(), t_hi = wide ? lw_alloca() : -1;
    if (wide) v = lw_widen(v);
    hi_emit(HI_STORE, TY_INT, t_lo, v.lo, 0, NULL);
    if (wide) hi_emit(HI_STORE, TY_INT, t_hi, v.hi, 0, NULL);
    int over;
    if (!neg) over = lw_cmp(R_GE, v, lim, wide);                       /* never below zero: the value itself */
    else if (!wide) {                                                   /* |v| >= 10^n, n <= 9: v + (10^n - 1) past 2*10^n - 2 unsigned (a negative v wraps high) */
        int t = hi_emit(HI_ADD, TY_INT, v.lo, lw_iconst((int)(lw_p10(n) - 1)), 0, NULL);
        over = hi_emit(HI_SGEU, TY_INT, t, lw_iconst((int)(2 * lw_p10(n) - 1)), 0, NULL);
    } else over = lw_cmp(R_GE, lw_abs(v, wide), lim, wide);
    int b_cut = hir_new_block(), b_join = hir_new_block();
    lw_brc(over, b_cut, b_join);
    lw_begin_blk(b_cut);
    LV r = lw_arith2('%', v, lim, wide);
    hi_emit(HI_STORE, TY_INT, t_lo, r.lo, 0, NULL);
    if (wide) hi_emit(HI_STORE, TY_INT, t_hi, r.hi, 0, NULL);
    lw_goto(b_join);
    lw_begin_blk(b_join);
    LV o; o.lo = hi_emit(HI_LOAD, TY_INT, t_lo, -1, 0, NULL); o.hi = wide ? hi_emit(HI_LOAD, TY_INT, t_hi, -1, 0, NULL) : -1;
    return o;
}

/* v (scale sc, bound bd, neg) into the item: cob_k_put_scale written out */
static void lw_store_ref(int sym, int ref, int rounded, LV v, int sc, long double bd, int neg);
static void lw_store(int sym, int rounded, LV v, int sc, long double bd, int neg) { lw_store_ref(sym, -1, rounded, v, sc, bd, neg); }
/* |v| (a word, below 10^D) into a DISPLAY or packed item of D <= 9 digits
 * at a: the digits by division, as the text's emit_dec_store writes them;
 * the sign an overpunch on the last digit ('p'..'y'), or the C/D nibble */
static int lw_div10(int m, int *r);
static void lw_dec_store(const Sym *d, int a, int mag, int neg)
{
    int D = d->pi.digits;
    if (d->usage == U_DISPLAY) {
        for (int k = D - 1; k >= 0; k--) {
            int dig, q = k ? lw_div10(mag, &dig) : -1;             /* the last division's quotient is the top digit */
            if (!k) dig = mag;
            int ch = hi_emit(HI_ADD, TY_INT, dig, lw_iconst(48), 0, NULL);
            if (k == D - 1 && d->pi.is_signed) ch = hi_emit(HI_ADD, TY_INT, ch, hi_emit(HI_SLL, TY_INT, neg, lw_iconst(6), 0, NULL), 0, NULL);   /* + 64 when negative */
            hi_emit(HI_STORE, TY_CHAR, k ? hi_emit(HI_ADDI, TY_INT, a, -1, k, NULL) : a, ch, 0, NULL);
            if (k) mag = q;
        }
        return;
    }
    /* packed: the sign nibble last, then digit pairs from the right; a
     * zero pad nibble at the top when the digits do not fill the bytes */
    int bytes = (int)d->size;
    int sgn = d->pi.is_signed ? hi_emit(HI_ADD, TY_INT, lw_iconst(12), neg, 0, NULL) : lw_iconst(15);     /* C, D; F unsigned */
    int lo; mag = lw_div10(mag, &lo);
    hi_emit(HI_STORE, TY_CHAR, hi_emit(HI_ADDI, TY_INT, a, -1, bytes - 1, NULL), hi_emit(HI_OR, TY_INT, hi_emit(HI_SLL, TY_INT, lo, lw_iconst(4), 0, NULL), sgn, 0, NULL), 0, NULL);
    int left = D - 1;
    for (int b = bytes - 2; b >= 0; b--) {
        int low = lw_iconst(0), high = lw_iconst(0);
        if (left > 0) mag = lw_div10(mag, &low);
        left--;
        if (left > 0) mag = lw_div10(mag, &high);
        left--;
        hi_emit(HI_STORE, TY_CHAR, b ? hi_emit(HI_ADDI, TY_INT, a, -1, b, NULL) : a, hi_emit(HI_OR, TY_INT, hi_emit(HI_SLL, TY_INT, high, lw_iconst(4), 0, NULL), low, 0, NULL), 0, NULL);
    }
}
/* ---- numeric editing in line (kern.h cob_edit_apply, per picture) ----
 * A numeric-edited receiver whose picture holds 9 Z , . + - $ CR DB and
 * V S (no * B 0 / P, no BLANK WHEN ZERO, no DECIMAL-POINT IS COMMA or
 * CURRENCY SIGN) and at most nine digit positions is edited by code of
 * its own: the digits by division, each position's byte from the
 * picture -- the suppression state static where the picture decides it
 * (a 9, the point), a value where the digits do -- and the floating
 * symbol placed last.  The rules are cob_edit_apply's, position for
 * position; kedit spent 0.33 of its 0.34 s in the runtime's walk. */
static void lw_fill_n(int dst, long n, int c);
static char lw_edit_fl(const char *pat)
{
    int cp = 0, cm = 0, cd = 0;
    for (const char *p = pat; *p; p++) { if (*p == '+') cp++; else if (*p == '-') cm++; else if (*p == '$') cd++; }
    return cp > 1 ? '+' : cm > 1 ? '-' : cd > 1 ? '$' : 0;
}
static int lw_edit_ok(const Sym *d)
{
    if (d->pi.category != PIC_NUMERIC_EDITED || d->usage != U_DISPLAY || d->blank_zero || g_dp_comma || g_currency || d->pi.fpexp) return 0;   /* a floating-point edited item: the runtime's */
    if (d->pi.digits < 1 || d->pi.digits > 9 || d->sign_sep || d->sign_lead) return 0;
    const char *pat = d->pi.pat; char fl = lw_edit_fl(pat); int npos = 0;
    for (const char *p = pat; *p; p++) {
        if (!strchr("9Z,.+-$CDVS", *p)) return 0;
        if (*p == '9' || *p == 'Z' || (fl && *p == fl)) npos++;
    }
    if (fl) npos--;
    return npos == d->pi.digits;
}
/* q = m / 10 and r = m % 10 for an unsigned word, by the reciprocal:
 * mulhu by 0xCCCCCCCD then >> 3 (exact for every 32-bit m), where divu
 * and remu are two hardware divisions each -- the digits of an edit cost
 * kedit as much that way as the runtime's walk had */
static int lw_div10(int m, int *r)
{
    static int way = -1; if (way < 0) { const char *e = getenv("S32_HIR_DIV10"); way = e ? atoi(e) : 1; }   /* =0: divu/remu, for the measurement */
    if (!way) { int q = hi_emit(HI_DIV, TY_INT | TY_UNSIGNED, m, lw_iconst(10), 0, NULL); if (r) *r = hi_emit(HI_REM, TY_INT | TY_UNSIGNED, m, lw_iconst(10), 0, NULL); return q; }
    int q = hi_emit(HI_SRL, TY_INT, hi_emit(HI_MULHU, TY_INT, m, lw_iconst((int)0xCCCCCCCD), 0, NULL), lw_iconst(3), 0, NULL);
    if (r) *r = hi_emit(HI_SUB, TY_INT, m, hi_emit(HI_MUL, TY_INT, q, lw_iconst(10), 0, NULL), 0, NULL);
    return q;
}
/* byte b to addr + o */
static void lw_edit_put(int addr, int o, int b) { hi_emit(HI_STORE, TY_CHAR, o ? hi_emit(HI_ADDI, TY_INT, addr, -1, o, NULL) : addr, b, 0, NULL); }
/* mag: the magnitude, a word of at most the picture's digits; neg: 1 when
 * the value was negative (a value), or -1 when it never is */
static void lw_edit_store(const Sym *d, int addr, int mag, int neg)
{
    const char *pat = d->pi.pat; char fl = lw_edit_fl(pat);
    int npos = d->pi.digits, has9 = strchr(pat, '9') != NULL;
    int dg[9], m = mag;
    for (int j = npos - 1; j >= 0; j--) { if (j) m = lw_div10(m, &dg[j]); else dg[j] = m; }
    int z = hi_emit(HI_SEQ, TY_INT, mag, lw_iconst(0), 0, NULL);
    int negv = neg < 0 ? lw_iconst(0) : hi_emit(HI_AND, TY_INT, neg, hi_emit(HI_XOR, TY_INT, z, lw_iconst(1), 0, NULL), 0, NULL);   /* zero is positive */
    /* the walk: sig the suppression state (-2 false, -1 true, else the
     * value), fs the first significant position (-2 none yet, static >= 0,
     * or fsv a value that is -1 for none) */
    int sig = -2, fs = -2, fsv = -1, flpos = -1, instr = 0, fl_seen = 0, o = 0, di = 0;
    for (const char *p = pat; *p; p++) {
        char c = *p;
        if ((fl && c == fl) || c == 'Z') {
            if (fl && c == fl && !fl_seen) { fl_seen = 1; flpos = o; instr = 1; lw_edit_put(addr, o++, lw_iconst(' ')); continue; }
            instr = 1;
            int dv = dg[di++];
            if (sig == -1) { lw_edit_put(addr, o++, hi_emit(HI_ADD, TY_INT, dv, lw_iconst('0'), 0, NULL)); continue; }
            int nz = hi_emit(HI_SNE, TY_INT, dv, lw_iconst(0), 0, NULL);
            int sv = sig == -2 ? nz : hi_emit(HI_OR, TY_INT, sig, nz, 0, NULL);
            /* ' ' or the digit: 32 + sv * (16 + d) */
            lw_edit_put(addr, o, hi_emit(HI_ADD, TY_INT, lw_iconst(' '), hi_emit(HI_MUL, TY_INT, sv, hi_emit(HI_ADD, TY_INT, dv, lw_iconst(16), 0, NULL), 0, NULL), 0, NULL));
            /* first significant: o when it is this one, else as it was (-1 while none) */
            int here = hi_emit(HI_SUB, TY_INT, hi_emit(HI_MUL, TY_INT, nz, lw_iconst(o + 1), 0, NULL), lw_iconst(1), 0, NULL);   /* nz ? o : -1 */
            if (sig == -2) fsv = here;
            else fsv = hi_emit(HI_ADD, TY_INT, fsv, hi_emit(HI_MUL, TY_INT, hi_emit(HI_XOR, TY_INT, sig, lw_iconst(1), 0, NULL), hi_emit(HI_ADD, TY_INT, here, lw_iconst(1), 0, NULL), 0, NULL), 0, NULL);   /* sig ? fsv : here (fsv is -1 when !sig) */
            sig = sv; fs = -3;                      /* dynamic: fsv */
            o++;
            continue;
        }
        switch (c) {
        case '9': {
            int dv = dg[di++];
            lw_edit_put(addr, o, hi_emit(HI_ADD, TY_INT, dv, lw_iconst('0'), 0, NULL));
            if (sig == -2) fs = o;
            else if (sig != -1) { fsv = hi_emit(HI_ADD, TY_INT, fsv, hi_emit(HI_MUL, TY_INT, hi_emit(HI_XOR, TY_INT, sig, lw_iconst(1), 0, NULL), lw_iconst(o + 1), 0, NULL), 0, NULL); fs = -3; }
            sig = -1; o++;
            break;
        }
        case '.':
            lw_edit_put(addr, o, lw_iconst('.'));
            if (sig == -2) fs = o;
            else if (sig != -1) { fsv = hi_emit(HI_ADD, TY_INT, fsv, hi_emit(HI_MUL, TY_INT, hi_emit(HI_XOR, TY_INT, sig, lw_iconst(1), 0, NULL), lw_iconst(o + 1), 0, NULL), 0, NULL); fs = -3; }
            sig = -1; o++;
            break;
        case ',':
            if (sig == -1 || !instr) lw_edit_put(addr, o++, lw_iconst(','));
            else if (sig == -2) lw_edit_put(addr, o++, lw_iconst(' '));
            else lw_edit_put(addr, o++, hi_emit(HI_ADD, TY_INT, lw_iconst(' '), hi_emit(HI_MUL, TY_INT, sig, lw_iconst(12), 0, NULL), 0, NULL));
            break;
        case '+': lw_edit_put(addr, o++, hi_emit(HI_ADD, TY_INT, lw_iconst('+'), hi_emit(HI_MUL, TY_INT, negv, lw_iconst(2), 0, NULL), 0, NULL)); break;
        case '-': lw_edit_put(addr, o++, hi_emit(HI_ADD, TY_INT, lw_iconst(' '), hi_emit(HI_MUL, TY_INT, negv, lw_iconst(13), 0, NULL), 0, NULL)); break;
        case '$': lw_edit_put(addr, o++, lw_iconst('$')); break;
        case 'C':
            lw_edit_put(addr, o++, hi_emit(HI_ADD, TY_INT, lw_iconst(' '), hi_emit(HI_MUL, TY_INT, negv, lw_iconst('C' - ' '), 0, NULL), 0, NULL));
            lw_edit_put(addr, o++, hi_emit(HI_ADD, TY_INT, lw_iconst(' '), hi_emit(HI_MUL, TY_INT, negv, lw_iconst('R' - ' '), 0, NULL), 0, NULL));
            break;
        case 'D':
            lw_edit_put(addr, o++, hi_emit(HI_ADD, TY_INT, lw_iconst(' '), hi_emit(HI_MUL, TY_INT, negv, lw_iconst('D' - ' '), 0, NULL), 0, NULL));
            lw_edit_put(addr, o++, hi_emit(HI_ADD, TY_INT, lw_iconst(' '), hi_emit(HI_MUL, TY_INT, negv, lw_iconst('B' - ' '), 0, NULL), 0, NULL));
            break;
        default: break;                             /* V S */
        }
    }
    int width = o;
    if (fl) {
        /* the symbol immediately left of the first significant position,
         * no further left than its own first; every position floating and
         * the value zero: spaces throughout */
        int symch = fl == '$' ? lw_iconst('$') : fl == '+' ? hi_emit(HI_ADD, TY_INT, lw_iconst('+'), hi_emit(HI_MUL, TY_INT, negv, lw_iconst(2), 0, NULL), 0, NULL)
                                                           : hi_emit(HI_ADD, TY_INT, lw_iconst(' '), hi_emit(HI_MUL, TY_INT, negv, lw_iconst(13), 0, NULL), 0, NULL);
        int pos;
        if (fs >= 0) pos = lw_iconst(fs - 1 > flpos ? fs - 1 : flpos);
        else {
            int fm1 = hi_emit(HI_ADDI, TY_INT, fsv, -1, -1, NULL);
            int lt = hi_emit(HI_SLT, TY_INT, fm1, lw_iconst(flpos), 0, NULL);      /* fm1 < flpos: flpos */
            pos = hi_emit(HI_ADD, TY_INT, fm1, hi_emit(HI_MUL, TY_INT, lt, hi_emit(HI_SUB, TY_INT, lw_iconst(flpos), fm1, 0, NULL), 0, NULL), 0, NULL);
        }
        hi_emit(HI_STORE, TY_CHAR, hi_emit(HI_ADD, TY_INT, addr, pos, 0, NULL), symch, 0, NULL);
        if (!has9) {
            int b_sp = hir_new_block(), b_join = hir_new_block();
            lw_brc(z, b_sp, b_join);                 /* (fsv < 0 only when every digit is zero: z says it) */
            lw_begin_blk(b_sp); lw_fill_n(addr, width, ' '); lw_goto(b_join);
            lw_begin_blk(b_join);
        }
        return;
    }
    if (!has9) {                                    /* all Z and the value zero: spaces throughout, the point too */
        int b_sp = hir_new_block(), b_join = hir_new_block();
        lw_brc(z, b_sp, b_join);
        lw_begin_blk(b_sp); lw_fill_n(addr, width, ' '); lw_goto(b_join);
        lw_begin_blk(b_join);
    }
}
static void lw_store_ref(int sym, int ref, int rounded, LV v, int sc, long double bd, int neg)
{
    const Sym *d = &g_sym[sym];
    if (d->is_index) {                              /* VARYING an index: the occurrence number, a word */
        if (sc != 0) die_at(cur()->line, "internal: an island stores a scaled value into an index");
        hi_emit(HI_STORE, TY_INT, lw_sym_addr(sym, ref), v.lo, 0, NULL);
        return;
    }
    int ed = !d->native && lw_edit_ok(d);          /* a numeric-edited item of a picture edited in line (above) */
    int dec = ed || (!d->native && lw_dec_inline_ok((Sym *)d));     /* a short DISPLAY or packed item: aligned and truncated here, written digit by digit */
    if (!d->native && !dec) { lw_mem_store(sym, ref, rounded, v, sc); return; }
    int eff = d->pi.digits, sd = d->pi.scale;
    int wide = lw_wide_bd(bd);
    if (wide) v = lw_widen(v);
    int sg0 = -1;                                   /* dec: the sign, read where cob_k_put_scale reads it (below) */
    if (sc > sd) {
        int m = sc - sd; long long P = lw_p10(m);
        LV q, r = v;
        if (bd < (long double)P) q = lw_lit(0, wide);
        else {
            q = lw_arith2('/', v, lw_lit(P, wide), wide);
            if (rounded) r = lw_arith2('-', v, lw_arith2('*', q, lw_lit(P, wide), wide), wide);   /* v - q * P: no second division */
        }
        if (rounded) {
            /* |r| at least half of P: one more, with the value's sign */
            LV ar = lw_abs(r, wide);
            int c = lw_cmp(R_GE, ar, lw_lit(P / 2, wide), wide);
            int s = hi_emit(HI_SRA, TY_INT, wide ? v.hi : v.lo, lw_iconst(31), 0, NULL);
            LV adj; adj.lo = hi_emit(HI_SUB, TY_INT, hi_emit(HI_XOR, TY_INT, c, s, 0, NULL), s, 0, NULL); adj.hi = -1;
            if (wide) adj.hi = hi_emit(HI_AND, TY_INT, s, hi_emit(HI_SUB, TY_INT, lw_iconst(0), c, 0, NULL), 0, NULL);
            q = lw_arith2('+', q, adj, wide);
        }
        v = q; bd = bd / (long double)P + (rounded ? 1 : 0); sc = sd;
    } else if (sc < sd) {
        int k = sd - sc, keep = eff - k;
        if (keep <= 0) { v = lw_lit(0, 0); bd = 0; wide = 0; }
        else {
            long double lim = dx_p10(keep);
            if (bd >= lim) { v = lw_trunc_digits(v, keep, wide, neg); bd = lim - 1; }
            bd *= dx_p10(k);
            int w2 = lw_wide_bd(bd);
            v = lw_scale(v, k, w2);
            if (w2) { v = lw_widen(v); wide = 1; }
        }
        sc = sd;
    }
    /* the sign is the value's after the scale is aligned and before the
     * high digits are cut, as the runtime reads it (cob_k_put_scale): a
     * value scaled down to nothing is +0 (gen-native 5024), one whose
     * digits are all cut keeps its sign, a negative zero (gen-lit 4506) */
    if (dec && neg) sg0 = hi_emit(HI_SLT, TY_INT, v.hi >= 0 ? v.hi : v.lo, lw_iconst(0), 0, NULL);   /* (a pair narrowed by the scaling has hi < 0 while wide still says pair) */
    {
        long double lim = dx_p10(eff);
        if (bd >= lim) { v = lw_trunc_digits(v, eff, wide, neg); bd = lim - 1; }
    }
    if (dec) {
        /* the magnitude, and the sign the value arrived with */
        LV m = neg ? lw_abs(v, 0) : v;
        if (ed) lw_edit_store(d, lw_sym_addr(sym, ref), m.lo, sg0);
        else lw_dec_store(d, lw_sym_addr(sym, ref), m.lo, sg0 >= 0 ? sg0 : lw_iconst(0));
        return;
    }
    if (!d->pi.is_signed && neg) v = lw_abs(v, wide);
    if (d->size < 8) v.hi = -1;                     /* the value fits the word (eff <= 9) */
    if (ref >= 0) {                                 /* a native table's element: the word at its address */
        int a = lw_sym_addr(sym, ref);
        hi_emit(HI_STORE, lw_item_ty(d), a, v.lo, 0, NULL);
        if (d->size == 8) hi_emit(HI_STORE, TY_INT, hi_emit(HI_ADDI, TY_INT, a, -1, 4, NULL), lw_widen(v).hi, 0, NULL);
        return;
    }
    lw_item_set(sym, v);
}

/* the address of a reference (lw_bytes_ref_ok): the record's label, the
 * item's offset, each subscript less one times its stride, the part's
 * start less one */
static int lw_ref_addr(const Ref *r)
{
    Sym *s = r->sym;
    long off = s->offset;
    int a = hi_emit(HI_GADDR, TY_INT, -1, -1, 0, g_sym[s->record].label);
    for (int k = 0; k < r->nsub; k++) {
        if (!r->sub[k].sym) { off += (r->sub[k].lit - 1) * s->dim_stride[k]; continue; }
        LV v = lw_item_val((int)(r->sub[k].sym - g_sym)); v.hi = -1;
        int i = v.lo;
        if (r->sub[k].adj - 1) i = hi_emit(HI_ADDI, TY_INT, i, -1, (int)r->sub[k].adj - 1, NULL);
        if (s->dim_stride[k] != 1) i = hi_emit(HI_MUL, TY_INT, i, lw_iconst(s->dim_stride[k]), 0, NULL);
        a = hi_emit(HI_ADD, TY_INT, a, i, 0, NULL);
    }
    if (r->rm && r->rm_sx) {
        /* a computed start: checked as the text has cob_refmod_len_chk
         * check it -- 1 <= start and start - 1 + len <= the item's size --
         * by one unsigned compare, the runtime's own check (and its
         * message) on the branch that fails */
        if (!r->lw_startx) die_at(r->line, "internal: a computed start the island did not admit");
        LV sv = lw_val(r->lw_startx - 1); sv.hi = -1;
        int st = sv.lo;
        int st0 = hi_emit(HI_ADDI, TY_INT, st, -1, -1, NULL);
        int ok = hi_emit(HI_SLTU, TY_INT, st0, lw_iconst((int)(s->size - r->rm_len + 1)), 0, NULL);
        int b_bad = hir_new_block(), b_ok = hir_new_block();
        lw_brc(ok, b_ok, b_bad);
        lw_begin_blk(b_bad);
        { int args[3] = { lw_desc_addr(s), st, lw_iconst((int)r->rm_len) }; lw_call("cob_refmod_len_chk", args, 3); }
        lw_goto(b_ok);
        lw_begin_blk(b_ok);
        if (off) a = hi_emit(HI_ADDI, TY_INT, a, -1, (int)off, NULL);
        return hi_emit(HI_ADD, TY_INT, a, st0, 0, NULL);
    }
    if (r->rm) off += r->rm_start - 1;
    return off ? hi_emit(HI_ADDI, TY_INT, a, -1, (int)off, NULL) : a;
}
/* the address of a sending operand's bytes and their count; a figurative
 * constant has no address: *fill is its byte, and n is 0 */
static int lw_bytes_src(const Opnd *o, long *n, int *fill)
{
    *fill = -1;
    if (o->kind == O_REF) { *n = lw_bytes_len(&o->ref); return lw_ref_addr(&o->ref); }
    if (o->kind == O_FIG) { *n = 0; *fill = fig_byte(o->tok->s); return -1; }
    const unsigned char *b = (const unsigned char *)(o->kind == O_NUM ? o->num.digits : o->tok->s);
    *n = o->kind == O_NUM ? o->num.ndigits : o->tok->len;
    const char *l = lit_label(b, (int)*n);
    return hi_emit(HI_GADDR, TY_INT, -1, -1, 0, (char *)l);
}
#define LW_INLINE_BYTES 32
static int lw_chunk_ty(int w) { return w == 4 ? TY_INT : w == 2 ? TY_SHORT | TY_UNSIGNED : TY_CHAR | TY_UNSIGNED; }
static int lw_at(int a, long o) { return o ? hi_emit(HI_ADDI, TY_INT, a, -1, (int)o, NULL) : a; }
/* n bytes from src to dst: loaded all, then stored, so an overlap is a
 * memmove's; long ones by memcpy, as the text emitter copies them */
static void lw_copy_n(int dst, int src, long n)
{
    if (n <= 0) return;
    if (n > LW_INLINE_BYTES) { int a[3] = { dst, src, lw_iconst((int)n) }; lw_call("memcpy", a, 3); return; }
    int vals[LW_INLINE_BYTES], offs[LW_INLINE_BYTES], ws[LW_INLINE_BYTES], k = 0;
    for (long o = 0; o < n; ) {
        int w = n - o >= 4 ? 4 : n - o >= 2 ? 2 : 1;
        vals[k] = hi_emit(HI_LOAD, lw_chunk_ty(w), lw_at(src, o), -1, 0, NULL); offs[k] = (int)o; ws[k] = w; k++;
        o += w;
    }
    for (int i = 0; i < k; i++) hi_emit(HI_STORE, lw_chunk_ty(ws[i]), lw_at(dst, offs[i]), vals[i], 0, NULL);
}
/* n bytes of a literal stored at dst as immediates: the literal's label
 * need not be loaded (a one-byte MOVE "n" TO X was gaddr, load, store) */
static void lw_store_lit_n(int dst, const unsigned char *b, long n)
{
    for (long o = 0; o < n; ) {
        int w = n - o >= 4 ? 4 : n - o >= 2 ? 2 : 1;
        unsigned k = 0;
        for (int j = w - 1; j >= 0; j--) k = (k << 8) | b[o + j];       /* little-endian, as the store writes it */
        hi_emit(HI_STORE, lw_chunk_ty(w), lw_at(dst, o), lw_iconst((int)k), 0, NULL);
        o += w;
    }
}
/* n bytes at dst set to c */
static void lw_fill_n(int dst, long n, int c)
{
    if (n <= 0) return;
    if (n > LW_INLINE_BYTES) { int a[3] = { dst, lw_iconst((int)n), lw_iconst(c) }; lw_call("cob_fill", a, 3); return; }
    int w4 = -1, w2 = -1, w1 = -1;
    for (long o = 0; o < n; ) {
        int w = n - o >= 4 ? 4 : n - o >= 2 ? 2 : 1;
        int *v = w == 4 ? &w4 : w == 2 ? &w2 : &w1;
        if (*v < 0) *v = lw_iconst(w == 4 ? c * 0x01010101 : w == 2 ? c * 0x0101 : c);
        hi_emit(HI_STORE, lw_chunk_ty(w), lw_at(dst, o), *v, 0, NULL);
        o += w;
    }
}
/* MOVE of bytes: the receiver takes the sender's first bytes, the rest
 * spaces (cob_move_alnum, left-justified); a figurative fills it */
/* a computed length as a value: checked as cob_refmod_len_chk checks it
 * -- 1 <= len and start - 1 + len <= the item's size, the start already
 * known valid -- by one unsigned compare, the runtime's check (and its
 * message) on the branch that fails */
static int lw_dyn_len(const Ref *r)
{
    const Sym *s = r->sym;
    LV v = lw_val(r->lw_lenx - 1); v.hi = -1;
    int start = -1;
    if (r->rm_sx) { LV sv = lw_val(r->lw_startx - 1); start = sv.lo; }
    else start = lw_iconst((int)r->rm_start);
    int room = hi_emit(HI_SUB, TY_INT, lw_iconst((int)s->size + 1), start, 0, NULL);      /* size - (start - 1) */
    int ok = hi_emit(HI_SLTU, TY_INT, hi_emit(HI_ADDI, TY_INT, v.lo, -1, -1, NULL), room, 0, NULL);
    int b_bad = hir_new_block(), b_ok = hir_new_block();
    lw_brc(ok, b_ok, b_bad);
    lw_begin_blk(b_bad);
    { int args[3] = { lw_desc_addr((Sym *)s), start, v.lo }; lw_call("cob_refmod_len_chk", args, 3); }
    lw_goto(b_ok);
    lw_begin_blk(b_ok);
    return v.lo;
}
/* the smaller of two words, by a branch into a temporary */
static int lw_min_val(int a, int b)
{
    int t = lw_alloca();
    hi_emit(HI_STORE, TY_INT, t, a, 0, NULL);
    int b_lt = hir_new_block(), b_join = hir_new_block();
    lw_brc(hi_emit(HI_SLT, TY_INT, b, a, 0, NULL), b_lt, b_join);
    lw_begin_blk(b_lt); hi_emit(HI_STORE, TY_INT, t, b, 0, NULL); lw_goto(b_join);
    lw_begin_blk(b_join);
    return hi_emit(HI_LOAD, TY_INT, t, -1, 0, NULL);
}
static void lw_gen_amove(LStmt *s)
{
    const Opnd *src = &g_lw_o[s->asrc];
    long sn; int fill; int sa = lw_bytes_src(src, &sn, &fill);
    const unsigned char *lb = src->kind == O_STR ? (const unsigned char *)src->tok->s : src->kind == O_NUM ? (const unsigned char *)src->num.digits : NULL;
    int snv = -1;                               /* the sender's length as a value, when computed */
    if (src->kind == O_REF && src->ref.rm && src->ref.lw_lenx) { snv = lw_dyn_len(&src->ref); sn = -1; }
    for (int i = 0; i < s->nr; i++) {
        const Ref *d = &g_lw_o[s->adst[i]].ref;
        if (g_lw_o[s->adst[i]].kind != O_REF || !d->sym) die_at(s->line, "internal: a MOVE of bytes lost its receiver (statement %d, operand %d of %d, kind %d)", (int)(s - g_lw_s), s->adst[i], g_lw_no, g_lw_o[s->adst[i]].kind);
        long dn = lw_bytes_len(d);
        int dnv = -1;
        if (d->rm && d->lw_lenx) { dnv = lw_dyn_len(d); dn = -1; }
        int da = lw_ref_addr(d);
        if (sn >= 0 && dn >= 0) {               /* both lengths known: in line */
            if (fill >= 0) { lw_fill_n(da, dn, fill); continue; }
            long n = sn < dn ? sn : dn;
            if (lb && n <= LW_INLINE_BYTES) lw_store_lit_n(da, lb, n);
            else lw_copy_n(da, sa, n);
            lw_fill_n(lw_at(da, n), dn - n, ' ');
            continue;
        }
        /* a length computed at run time: the shorter one's bytes by
         * memcpy, the receiver's rest filled -- the runtime's own
         * cob_move_alnum, in two calls (cob_fill of nothing is nothing) */
        if (dnv < 0) dnv = lw_iconst((int)dn);
        if (fill >= 0) { int a[3] = { da, dnv, lw_iconst(fill) }; lw_call("cob_fill", a, 3); continue; }
        if (snv < 0) snv = lw_iconst((int)sn);
        int k = lw_min_val(snv, dnv);
        { int a[3] = { da, sa, k }; lw_call("memcpy", a, 3); }
        { int a[3] = { hi_emit(HI_ADD, TY_INT, da, k, 0, NULL), hi_emit(HI_SUB, TY_INT, dnv, k, 0, NULL), lw_iconst(' ') }; lw_call("cob_fill", a, 3); }
    }
}
/* bytes equal or not: chunks xor-ed and or-ed together for a short
 * operand, memcmp for a long one; a literal is padded with spaces to the
 * item's length, or the item's bytes past the literal are spaces */
static int lw_acmp_val(const LCond *c)
{
    const Opnd *x = &g_lw_o[c->ax], *y = &g_lw_o[c->ay];
    if (x->kind != O_REF) { const Opnd *t = x; x = y; y = t; }         /* the item first */
    long nx, ny; int fx, fy;
    int ax = lw_bytes_src(x, &nx, &fx);
    int ay = lw_bytes_src(y, &ny, &fy);
    unsigned char *lit = NULL;                  /* the literal's bytes, compared as constants where small */
    if (y->kind != O_REF) {
        /* the literal, or the figurative, as nx bytes */
        unsigned char *b = xmalloc((size_t)nx);
        memset(b, fy >= 0 ? fy : ' ', (size_t)nx);
        if (fy < 0) {
            const unsigned char *lb = (const unsigned char *)(y->kind == O_NUM ? y->num.digits : y->tok->s);
            memcpy(b, lb, (size_t)(ny < nx ? ny : nx));
            for (long k = nx; k < ny; k++) if (lb[k] != ' ') { free(b); return lw_iconst(c->op == R_NE); }   /* longer than the item, and not spaces: never equal */
        }
        ay = hi_emit(HI_GADDR, TY_INT, -1, -1, 0, (char *)lit_label(b, (int)nx));
        lit = b; ny = nx;
    }
    long n = nx;
    int d;
    if (c->op != R_EQ && c->op != R_NE) {
        /* ordered, as the text does: memcmp's sign */
        int a[3] = { ax, ay, lw_iconst((int)n) }; d = lw_call("memcmp", a, 3).lo;
        int k = c->op == R_LT ? HI_SLT : c->op == R_GT ? HI_SGT : c->op == R_LE ? HI_SLE : HI_SGE;
        return hi_emit(k, TY_INT, d, lw_iconst(0), 0, NULL);
    }
    if (n > 16) { int a[3] = { ax, ay, lw_iconst((int)n) }; d = lw_call("memcmp", a, 3).lo; }
    else {
        d = -1;
        for (long o = 0; o < n; ) {
            int w = n - o >= 4 ? 4 : n - o >= 2 ? 2 : 1, yv;
            unsigned k = 0;
            if (lit) for (int j = w - 1; j >= 0; j--) k = (k << 8) | lit[o + j];       /* little-endian, as the load would read it */
            if (lit && k < 4096) yv = lw_iconst((int)k);                               /* an xori immediate (the backend takes 12 bits unsigned) */
            else yv = hi_emit(HI_LOAD, lw_chunk_ty(w), lw_at(ay, o), -1, 0, NULL);
            int t = hi_emit(HI_XOR, TY_INT, hi_emit(HI_LOAD, lw_chunk_ty(w), lw_at(ax, o), -1, 0, NULL), yv, 0, NULL);
            d = d < 0 ? t : hi_emit(HI_OR, TY_INT, d, t, 0, NULL);
            o += w;
        }
    }
    free(lit);
    return hi_emit(c->op == R_EQ ? HI_SEQ : HI_SNE, TY_INT, d, lw_iconst(0), 0, NULL);
}

/* a native item's storage brought up to date, for a routine that reads it */
static void lw_item_sync(int sym)
{
    LwItem *it = &g_lw_item[lw_item_slot(sym)]; const Sym *s = &g_sym[sym];
    if (!it->written) return;
    int a = lw_item_addr(s);
    hi_emit(HI_STORE, lw_item_ty(s), a, hi_emit(HI_LOAD, TY_INT, it->a_lo, -1, 0, NULL), 0, NULL);
    if (it->a_hi >= 0) hi_emit(HI_STORE, TY_INT, hi_emit(HI_ADDI, TY_INT, a, -1, 4, NULL), hi_emit(HI_LOAD, TY_INT, it->a_hi, -1, 0, NULL), 0, NULL);
}
/* DISPLAY: each operand to the console as parse_display writes it, then the line's end */
static void lw_gen_display(LStmt *s)
{
    for (int i = 0; i < s->nr; i++) {
        Opnd *o = &g_lw_o[s->adst[i]];
        if (o->kind == O_REF) {
            if (o->ref.sym->native && !o->ref.nsub) lw_item_sync((int)(o->ref.sym - g_sym));   /* (a native table's element lives in storage) */
            int d = o->ref.rm ? part_desc(&o->ref) : sym_desc(o->ref.sym);
            char b[32]; snprintf(b, sizeof b, ".Ld%d", d);
            int a[2] = { lw_ref_addr(&o->ref), hi_emit(HI_GADDR, TY_INT, -1, -1, 0, xstrndup(b, strlen(b))) };
            lw_call("cob_display_field", a, 2);
            continue;
        }
        unsigned char txt[64]; int k = 0; const unsigned char *b; 
        if (o->kind == O_NUM) {
            if (o->num.neg) txt[k++] = '-';
            for (int j = 0; j < o->num.ndigits; j++) {
                if (o->num.scale && j == o->num.ndigits - o->num.scale) txt[k++] = g_dp_comma ? ',' : '.';
                txt[k++] = (unsigned char)o->num.digits[j];
            }
            b = txt;
        } else if (o->kind == O_FIG) { txt[0] = (unsigned char)fig_byte(o->tok->s); k = 1; b = txt; }
        else { b = (const unsigned char *)o->tok->s; k = o->tok->len; }      /* O_STR, O_ALL */
        int a[2] = { hi_emit(HI_GADDR, TY_INT, -1, -1, 0, (char *)lit_label(b, k)), lw_iconst(k) };
        lw_call("cob_display", a, 2);
    }
    if (!s->asrc) lw_call("cob_display_nl", NULL, 0);
}

/* a text statement: the island's items stored to their storage, the
 * lines in place (an HI_CALL the emitter writes as them), the items
 * loaded again -- the text may have read or written any of them */
static int g_lw_has_text;                       /* this island has text statements: the frame and the registers it needs */
static void lw_gen_stmts(int at, int n);
/* does the text name the label -- as %hi(label), %lo(label+off): the
 * only way code reaches a native item (census.h: no address of one is
 * ever taken, none lies in a record anything else names) */
static int lw_text_names(const Block *t, const char *label)
{
    size_t n = strlen(label);
    for (int i = 0; i < t->n; i++)
        for (const char *p = strstr(t->line[i], label); p; p = strstr(p + 1, label)) {
            char c = p[n];
            if (!(c == '_' || (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))) return 1;
        }
    return 0;
}
typedef struct { char *name; char **line; int n; } LwPend;   /* an island's text, waiting for lw_flush */
static LwPend *g_lw_pend; static int g_lw_npend, g_lw_pcap;
/* does the code of paragraphs lo..hi name the label -- their own lines in
 * the unit's text, the islands they call (whose lines wait in g_lw_pend),
 * and the ranges they PERFORM, transitively?  The same argument as
 * lw_text_names: a native item is reached by its label or not at all. */
static int lw_line_names(const char *l, const char *label)
{
    Block t; t.line = (char **)&l; t.n = 1;
    return lw_text_names(&t, label);
}
static int lw_opnd_names(int oi, int sym);
static int lw_range_names_1(int lo, int hi, const char *label, int *seen, int nseen);
/* does the node name item sym -- as a value, a receiver, a subscript, a
 * displayed operand, in a text node's lines or the ranges it performs */
static int lw_node_names(int n, int sym)
{
    if (n < 0) return 0;
    const LNode *x = &g_lw_n[n];
    if (!x->op) return x->sym == sym || (x->ref >= 0 && lw_opnd_names(x->ref, sym));
    return lw_node_names(x->l, sym) || lw_node_names(x->r, sym);
}
static int lw_opnd_names(int oi, int sym)
{
    if (oi < 0) return 0;
    const Opnd *o = &g_lw_o[oi];
    if (o->kind != O_REF) return 0;
    if (o->ref.sym == &g_sym[sym]) return 1;
    for (int k = 0; k < o->ref.nsub; k++) if (o->ref.sub[k].sym == &g_sym[sym]) return 1;
    if (o->ref.rm && o->ref.lw_startx && lw_node_names(o->ref.lw_startx - 1, sym)) return 1;
    if (o->ref.rm && o->ref.lw_lenx && lw_node_names(o->ref.lw_lenx - 1, sym)) return 1;
    return 0;
}
static int lw_cond_names(int c, int sym)
{
    if (c < 0) return 0;
    const LCond *x = &g_lw_c[c];
    if (x->kind == C_REL) return x->alnum ? lw_opnd_names(x->ax, sym) || lw_opnd_names(x->ay, sym) : lw_node_names(x->x, sym) || lw_node_names(x->y, sym);
    return lw_cond_names(x->a, sym) || (x->kind != C_NOT && lw_cond_names(x->b, sym));
}
static int lw_stmts_name(int at, int n, int sym, int *seen, int nseen);
static int lw_stmt_names(int st, int sym, int *seen, int nseen)
{
    const LStmt *s = &g_lw_s[st];
    if (lw_node_names(s->expr, sym) || lw_cond_names(s->cond, sym)) return 1;
    if (s->kind == LS_STORE || s->kind == LS_ADDTO) for (int k = 0; k < s->nr; k++) if (s->rsym[k] == sym || lw_opnd_names(s->rref[k], sym)) return 1;
    if (s->rem == sym) return 1;
    if (s->kind == LS_AMOVE || s->kind == LS_DISPLAY) { if (lw_opnd_names(s->asrc, sym)) return 1; for (int k = 0; k < s->nr; k++) if (lw_opnd_names(s->adst[k], sym)) return 1; }
    for (int k = 0; k < s->nv; k++) if (s->var[k] == sym || lw_node_names(s->from[k], sym) || lw_node_names(s->by[k], sym) || lw_cond_names(s->vcond[k], sym)) return 1;
    if (s->kind == LS_TEXT && !s->inl) {
        const char *label = g_sym[g_sym[sym].record].label;
        if (lw_text_names(&s->ptext, label)) return 1;
        for (int k = 0; k < s->nperf; k++)
            if (lw_range_names_1(g_lw_perf[s->perf0 + k].lo, lw_range_hi(g_lw_perf[s->perf0 + k].lo, g_lw_perf[s->perf0 + k].hi), label, seen, nseen)) return 1;
        if (s->phrase >= 0) { const LStmt *ph = &g_lw_s[s->phrase]; if (lw_stmts_name(ph->body, ph->nbody, sym, seen, nseen) || lw_stmts_name(ph->els, ph->nels, sym, seen, nseen)) return 1; }
        return 0;
    }
    return lw_stmts_name(s->body, s->nbody, sym, seen, nseen) || lw_stmts_name(s->els, s->nels, sym, seen, nseen);
}
static int lw_stmts_name(int at, int n, int sym, int *seen, int nseen)
{
    for (int i = 0; i < n; i++) if (lw_stmt_names(g_lw_list[at + i], sym, seen, nseen)) return 1;
    return 0;
}
/* the item whose record is labelled so, or -1: the range scan meets a
 * placeholder and asks its node instead of giving up */
static int lw_item_of_label(const char *label)
{
    for (int i = 0; i < g_lw_nitem; i++) if (!strcmp(g_sym[g_sym[g_lw_item[i].sym].record].label, label)) return g_lw_item[i].sym;
    return -1;
}
static int lw_range_names_1(int lo, int hi, const char *label, int *seen, int nseen)
{
    if (lo < 1 || hi < lo || hi > g_npara) return 1;
    for (int k = 0; k < nseen; k += 2) if (seen[k] == lo && seen[k + 1] == hi) return 0;
    if (nseen + 2 > 64) return 1;
    seen[nseen] = lo; seen[nseen + 1] = hi; nseen += 2;
    for (int p = lo; p <= hi; p++) {
        char lab[32]; snprintf(lab, sizeof lab, ".Lp%d_%d:", g_para[p - 1].unit, p);
        char mark[16]; snprintf(mark, sizeof mark, "#@E %d", p);
        int i = 0;
        for (; i < g_nasm && strncmp(g_asm[i], lab, strlen(lab)); i++) ;
        if (i == g_nasm) return 1;              /* not in the text (yet): anything */
        for (i++; i < g_nasm && strcmp(g_asm[i], mark); i++) {
            const char *l = g_asm[i];
            if (lw_line_names(l, label)) return 1;
            if (!strncmp(l, "\tjal r31, .Lisl", 15)) {
                int k; for (k = 0; k < g_lw_npend && strcmp(g_lw_pend[k].name, l + 11); k++) ;
                if (k == g_lw_npend) return 1;
                Block t; t.line = g_lw_pend[k].line; t.n = g_lw_pend[k].n;
                if (lw_text_names(&t, label)) return 1;
            }
            int st; if (lw_is_place(l, &st)) {              /* a placeholder not yet resolved: its node knows what it names */
                int sym = lw_item_of_label(label);
                if (sym < 0 || lw_stmt_names(st, sym, seen, nseen)) return 1;
            }
        }
        if (i == g_nasm) return 1;
    }
    for (int k = 0; k < g_npcf; k++)
        if (g_pcf[k].from >= lo && g_pcf[k].from <= hi && lw_range_names_1(g_pcf[k].lo, lw_range_hi(g_pcf[k].lo, g_pcf[k].thru), label, seen, nseen)) return 1;
    return 0;
}
static int lw_range_names(int lo, int hi, const char *label) { int seen[64]; return lw_range_names_1(lo, hi, label, seen, 0); }
/* is PERFORM record pf one of a node in the lists -- an ON/NOT ON block's
 * own statement, generated as a branch, not the verb's own code */
static int lw_perf_in_lists(int at, int n, int pf)
{
    for (int i = 0; i < n; i++) {
        const LStmt *t = &g_lw_s[g_lw_list[at + i]];
        if (t->kind == LS_TEXT && pf >= t->perf0 && pf < t->perf0 + t->nperf) return 1;
        if (lw_perf_in_lists(t->body, t->nbody, pf) || lw_perf_in_lists(t->els, t->nels, pf)) return 1;
    }
    return 0;
}
static void lw_gen_text(int st)
{
    LStmt *s = &g_lw_s[st];
    /* the items the node may touch: those its lines name, and those the
     * ranges its own code PERFORMs name (a phrase block's PERFORM is that
     * block's node's); the rest stay in registers across it */
    unsigned char touch[256];
    int own[64], nown = 0;
    for (int k = 0; k < s->nperf && nown < 64; k++) {
        int pf = s->perf0 + k;
        if (s->phrase >= 0) { const LStmt *ph = &g_lw_s[s->phrase]; if (lw_perf_in_lists(ph->body, ph->nbody, pf) || lw_perf_in_lists(ph->els, ph->nels, pf)) continue; }
        own[nown++] = pf;
    }
    /* a runtime call may run a USE declarative (an I/O error, an EC): in a
     * unit that has them, their sections' footprint is every such node's
     * (free/faultbyte: the declarative displays the loop's own item) */
    int decl = 0;
    for (int u = 0; u < g_nuse && !decl; u++) if (g_use[u].unit == g_unit) decl = 1;
    if (decl) { decl = 0; for (int i = 0; i < s->ptext.n && !decl; i++) decl = strncmp(s->ptext.line[i], "\tjal r31, cob_", 14) == 0; }
    for (int i = 0; i < g_lw_nitem; i++) {
        const char *label = g_sym[g_sym[g_lw_item[i].sym].record].label;
        touch[i] = (unsigned char)lw_text_names(&s->ptext, label);
        for (int k = 0; k < nown && !touch[i]; k++)
            touch[i] = (unsigned char)lw_range_names(g_lw_perf[own[k]].lo, lw_range_hi(g_lw_perf[own[k]].lo, g_lw_perf[own[k]].hi), label);
        for (int u = 0; decl && u < g_nuse && !touch[i]; u++)
            if (g_use[u].unit == g_unit) touch[i] = (unsigned char)lw_range_names(g_use[u].sec, lw_range_hi(g_use[u].sec, -1), label);
    }
    g_lw_touch = touch;
    if (lw_trace()) { int k = 0; for (int i = 0; i < g_lw_nitem; i++) k += touch[i]; fprintf(stderr, "hir: line %d text node: syncs %d of %d items%s\n", s->line, k, g_lw_nitem, nown ? " (performs)" : ""); }
    lw_exit_stores();
    char b[32]; snprintf(b, sizeof b, ".Ltext%d", st);
    LV v = lw_call(xstrndup(b, strlen(b)), NULL, 0);
    lw_entry_loads();
    g_lw_touch = NULL;
    if (s->phrase < 0) return;
    /* the phrases: the status word the call returned, ON when it is 1
     * (on_one) or not 0, and the blocks */
    LStmt *ph = &g_lw_s[s->phrase];
    int c = ph->on_one ? hi_emit(HI_SEQ, TY_INT, v.lo, lw_iconst(1), 0, NULL) : hi_emit(HI_SNE, TY_INT, v.lo, lw_iconst(0), 0, NULL);
    int b_on = hir_new_block(), b_not = hir_new_block(), b_join = hir_new_block();
    lw_brc(c, b_on, b_not);
    lw_begin_blk(b_on); lw_gen_stmts(ph->body, ph->nbody); lw_goto(b_join);
    lw_begin_blk(b_not); lw_gen_stmts(ph->els, ph->nels); lw_goto(b_join);
    lw_begin_blk(b_join);
}
/* the emitter's side (hcg_text_call): the lines, marks left out */
static int lw_text_is(char *name) { return !strncmp(name, ".Ltext", 6); }
/* the lines with every label they define renamed to a fresh one: a text
 * node is emitted once per place it stands, and an inlined paragraph's
 * nodes stand in two or more */
static void lw_relabel(const Block *t, int (*emit_line)(const char *))
{
    int old[256], neu[256], n = 0;
    for (int i = 0; i < t->n && n < 256; i++) {
        const char *l = t->line[i];
        if (l[0] == '.' && l[1] == 'L' && l[2] >= '0' && l[2] <= '9') {
            char *e; long v = strtol(l + 2, &e, 10);
            if (*e == ':') { old[n] = (int)v; neu[n] = new_label(); n++; }
        }
    }
    char buf[4096];
    for (int i = 0; i < t->n; i++) {
        const char *l = t->line[i];
        if (l[0] == '#' && l[1] == '@') continue;
        /* -fprofile-lines' global label: a fresh sequence number each time
         * the lines are emitted (prof.py reads the line number before it) */
        if (!strncmp(l, "__ln_", 5) || !strncmp(l, "\t.globl __ln_", 13)) {
            const char *p = strstr(l, "__ln_") + 5; char *e; long ln = strtol(p, &e, 10);
            static int seq = 900000;
            snprintf(buf, sizeof buf, l[0] == '\t' ? "\t.globl __ln_%ld_%d" : "__ln_%ld_%d:", ln, l[0] == '\t' ? seq + 1 : ++seq);
            emit_line(buf); continue;
        }
        if (!n) { emit_line(l); continue; }
        int k = 0;
        for (const char *p = l; *p && k < (int)sizeof buf - 16; ) {
            if (p[0] == '.' && p[1] == 'L' && p[2] >= '0' && p[2] <= '9') {
                char *e; long v = strtol(p + 2, &e, 10);
                int j; for (j = 0; j < n && old[j] != (int)v; j++) ;
                if (j < n) { k += snprintf(buf + k, sizeof buf - (size_t)k, ".L%d", neu[j]); p = e; continue; }
            }
            buf[k++] = *p++;
        }
        buf[k] = 0;
        emit_line(buf);
    }
}
static int lw_cg_line(const char *l) { cg_s((char *)l); cg_c(10); return 0; }
static int lw_text_call(char *name)
{
    if (!lw_text_is(name)) return 0;
    const LStmt *s = &g_lw_s[atoi(name + 6)];
    lw_relabel(&s->ptext, lw_cg_line);
    return 1;
}

/* a condition's value, a word 0 or 1 */
static int lw_cond_val(int c)
{
    LCond *x = &g_lw_c[c];
    if (x->kind == C_REL && x->alnum) return lw_acmp_val(x);
    if (x->kind == C_NOT) return hi_emit(HI_XOR, TY_INT, lw_cond_val(x->a), lw_iconst(1), 0, NULL);
    if (x->kind == C_AND || x->kind == C_OR)
        return hi_emit(x->kind == C_AND ? HI_AND : HI_OR, TY_INT, lw_cond_val(x->a), lw_cond_val(x->b), 0, NULL);
    LNode *a = &g_lw_n[x->x], *b = &g_lw_n[x->y];
    int sc = a->sc > b->sc ? a->sc : b->sc;
    long double bd = a->bd * dx_p10(sc - a->sc) + b->bd * dx_p10(sc - b->sc);
    LV l = lw_val_scaled(x->x, sc - a->sc, bd), r = lw_val_scaled(x->y, sc - b->sc, bd);
    return lw_cmp(x->op, l, r, lw_wide_bd(bd));
}
/* a branch on a condition: AND and OR short-circuit, as the text's
 * emit_cond does -- the second operand is reached only when the first
 * has not decided; NOT swaps the targets */
static void lw_cond_br(int c, int bt, int bf)
{
    LCond *x = &g_lw_c[c];
    if (x->kind == C_NOT) { lw_cond_br(x->a, bf, bt); return; }
    if (x->kind == C_AND || x->kind == C_OR) {
        int mid = hir_new_block();
        if (x->kind == C_AND) lw_cond_br(x->a, mid, bf); else lw_cond_br(x->a, bt, mid);
        lw_begin_blk(mid);
        lw_cond_br(x->b, bt, bf);
        return;
    }
    lw_brc(lw_cond_val(c), bt, bf);
}
/* the quotient at the root of a STORE: its value, scale and bound; the
 * stores it guards go in a block of their own, skipped for a zero divisor */
/* two items in storage with one descriptor: a MOVE between them is a copy
 * of the bytes, as the text emitter makes it (move.h, GitHub #27), not a
 * fetch and a store */
static int lw_same_desc(Sym *a, Sym *b) { return !a->native && !b->native && sym_desc(a) == sym_desc(b) && a->size <= 4 * 64; }   /* (64: LW_MAXCHUNK, below) */
/* the item's bytes at fa, loaded in chunks (values the receivers share:
 * the sender is identified and read once, before the first receiver --
 * MOVE te(b) TO b, ce(b); free/moveonce) */
#define LW_MAXCHUNK 64
static int lw_load_chunks(const Sym *from, int fa, int *val, int *ty)
{
    int n = from->size, k = 0;
    for (int o = 0; o < n && k < LW_MAXCHUNK; ) {
        int w = n - o >= 4 ? 4 : n - o >= 2 ? 2 : 1;
        ty[k] = w == 4 ? TY_INT : w == 2 ? TY_SHORT | TY_UNSIGNED : TY_CHAR | TY_UNSIGNED;
        val[k] = hi_emit(HI_LOAD, ty[k], o ? hi_emit(HI_ADDI, TY_INT, fa, -1, o, NULL) : fa, -1, 0, NULL);
        o += w; k++;
    }
    return k;
}
static void lw_store_chunks(int ta, const int *val, const int *ty, int k)
{
    for (int i = 0, o = 0; i < k; i++) {
        hi_emit(HI_STORE, ty[i], o ? hi_emit(HI_ADDI, TY_INT, ta, -1, o, NULL) : ta, val[i], 0, NULL);
        o += ty_size(ty[i]);
    }
}
static void lw_gen_store(LStmt *s)
{
    LNode *x = &g_lw_n[s->expr];
    if (x->op != '/') {
        /* the sender once, before any receiver: its value, and its bytes
         * for a receiver of its own descriptor */
        LV v; int have = 0, copy = 0, val[LW_MAXCHUNK], ty[LW_MAXCHUNK], nch = 0;
        for (int i = 0; i < s->nr; i++) {
            if (!x->op && !s->rnd[i] && lw_same_desc(&g_sym[x->sym], &g_sym[s->rsym[i]])) copy = 1; else have = 1;
        }
        if (copy) nch = lw_load_chunks(&g_sym[x->sym], lw_sym_addr(x->sym, x->ref), val, ty);
        if (have) v = lw_val(s->expr);
        for (int i = 0; i < s->nr; i++) {
            if (!x->op && !s->rnd[i] && lw_same_desc(&g_sym[x->sym], &g_sym[s->rsym[i]])) { lw_store_chunks(lw_sym_addr(s->rsym[i], s->rref[i]), val, ty, nch); continue; }
            lw_store_ref(s->rsym[i], s->rref[i], s->rnd[i], v, x->sc, x->bd, x->neg);
        }
        return;
    }
    int e, sq; long double bq;
    if (!lw_div_shape(s->expr, s->need, &e, &sq, &bq)) die_at(s->line, "internal: an island's division changed its mind");
    LNode *a = &g_lw_n[x->l], *b = &g_lw_n[x->r];
    LV dv = lw_val(x->r);
    int bwide = lw_wide_bd(b->bd);
    int z = lw_cmp(R_NE, dv, lw_lit(0, bwide), bwide);
    int b_div = hir_new_block(), b_join = hir_new_block();
    lw_brc(z, b_div, b_join);
    lw_begin_blk(b_div);
    long double bn = a->bd * dx_p10(e);
    int wide = lw_wide_bd(bn) || bwide;
    LV nv = lw_val_scaled(x->l, e, bn);
    if (wide) { nv = lw_widen(nv); dv = lw_widen(dv); }
    /* the dividend as it was: the remainder is of the operands before the
     * quotient is stored over one of them (tests/free/divremgiving) */
    LV a0 = s->rem >= 0 ? lw_val(x->l) : nv;
    LV q = lw_arith2('/', nv, dv, wide);
    for (int i = 0; i < s->nr; i++) lw_store_ref(s->rsym[i], s->rref[i], s->rnd[i], q, sq, bq, x->neg);
    if (s->rem >= 0) {
        /* the dividend less the divisor times the quotient cut to the
         * quotient item's decimals (lw_rem_shape) */
        const Sym *qd = &g_sym[s->rsym[0]];
        int e2, down, sr; long double br;
        if (!lw_rem_shape(s->expr, qd, &e2, &down, &sr, &br)) die_at(s->line, "internal: an island's remainder changed its mind");
        LV qt;
        long double bq2 = a->bd * dx_p10(e2);
        int w2 = lw_wide_bd(bq2) || bwide;
        if (e2 == e && w2 == wide) qt = q;
        else {
            LV nv2 = a0, dv2 = dv;
            if (w2) { nv2 = lw_widen(nv2); dv2 = lw_widen(dv2); }
            nv2 = lw_scale(nv2, e2, w2);
            qt = lw_arith2('/', nv2, dv2, w2);
        }
        if (down) { long double bd2 = bq2; qt = lw_arith2('/', qt, lw_lit(lw_p10(down), lw_wide_bd(bd2)), lw_wide_bd(bd2)); bq2 = bq2 / dx_p10(down) + 1; }
        if (!qd->pi.is_signed && g_std < 2002) qt = lw_abs(qt, lw_wide_bd(bq2));   /* 85: the magnitude (DIVIDE rule 6) */
        int qs = sq - e + e2 - down;                 /* the cut quotient's scale */
        int ps = qs + b->sc;
        long double bp = bq2 * b->bd;
        int pw = lw_wide_bd(bp);
        LV pv = lw_arith2('*', qt, dv, pw);
        int rw = lw_wide_bd(br);
        LV av = a0;
        if (rw) { av = lw_widen(av); pv = lw_widen(pv); }
        av = lw_scale(av, sr - a->sc, rw);
        pv = lw_scale(pv, sr - ps, rw);
        lw_store(s->rem, 0, lw_arith2('-', av, pv, rw), sr, br, 1);
    }
    lw_goto(b_join);
    lw_begin_blk(b_join);
}
static void lw_gen_addto(LStmt *s)
{
    LNode *sum = &g_lw_n[s->expr];
    LV sv = lw_val(s->expr);
    for (int i = 0; i < s->nr; i++) {
        const Sym *d = &g_sym[s->rsym[i]];
        int sc = d->pi.scale > sum->sc ? d->pi.scale : sum->sc;
        long double bd = lw_sym_bd(d) * dx_p10(sc - d->pi.scale) + sum->bd * dx_p10(sc - sum->sc);
        int wide = lw_wide_bd(bd);
        LV r = lw_item_val_ref(s->rsym[i], s->rref[i]);
        if (!wide) r.hi = -1;
        r = lw_scale(r, sc - d->pi.scale, wide);
        LV t = lw_scale(sv, sc - sum->sc, wide);
        lw_store_ref(s->rsym[i], s->rref[i], s->rnd[i], lw_arith2(s->subtract ? '-' : '+', r, t, wide), sc, bd, d->pi.is_signed || sum->neg || s->subtract);
    }
}
/* the VARYING item at level k set to its FROM; augmented by its BY */
static void lw_vary_init(LStmt *s, int k) { LNode *f = &g_lw_n[s->from[k]]; lw_store(s->var[k], 0, lw_val(s->from[k]), f->sc, f->bd, f->neg); }
static void lw_vary_step(LStmt *s, int k)
{
    const Sym *d = &g_sym[s->var[k]]; LNode *by = &g_lw_n[s->by[k]];
    int sc = d->pi.scale > by->sc ? d->pi.scale : by->sc;
    long double bd = lw_sym_bd(d) * dx_p10(sc - d->pi.scale) + by->bd * dx_p10(sc - by->sc);
    int wide = lw_wide_bd(bd);
    LV r = lw_item_val(s->var[k]); if (!wide) r.hi = -1;
    r = lw_scale(r, sc - d->pi.scale, wide);
    LV t = lw_val_scaled(s->by[k], sc - by->sc, bd);
    lw_store(s->var[k], 0, lw_arith2('+', r, t, wide), sc, bd, d->pi.is_signed || by->neg);
}
/* a loop laid out as emit_varying does: one jump in to the test, then
 * each iteration the body and the test's branch back; TEST AFTER the
 * body first.  Level k of a VARYING ... AFTER: its item set, the levels
 * inside it run as the body, and when its condition holds an inner item
 * goes back to FROM (6.20.4) */
static void lw_gen_loop_level(LStmt *s, int k)
{
    if (s->nv) lw_vary_init(s, k);
    int cond = s->nv ? s->vcond[k] : s->cond;
    int b_body = hir_new_block(), b_exit = hir_new_block(), b_test = -1;
    if (s->test_after) {
        lw_goto(b_body);
        lw_begin_blk(b_body);
        lw_gen_stmts(s->body, s->nbody);
        if (lw_blk_live) {
            int b_step = hir_new_block();
            lw_cond_br(cond, b_exit, b_step);
            lw_begin_blk(b_step);
        }
    } else {
        b_test = hir_new_block();
        lw_goto(b_test);
        lw_begin_blk(b_body);
        if (k + 1 < s->nv) lw_gen_loop_level(s, k + 1); else lw_gen_stmts(s->body, s->nbody);
    }
    if (s->nv && lw_blk_live) lw_vary_step(s, k);
    if (s->test_after) lw_goto(b_body);
    else {
        lw_goto(b_test);
        lw_begin_blk(b_test);
        lw_cond_br(cond, b_exit, b_body);
    }
    lw_begin_blk(b_exit);
    if (k > 0) lw_vary_init(s, k);
}
static void lw_gen_loop(LStmt *s) { lw_gen_loop_level(s, 0); }
/* an inlined PERFORM: each paragraph of the range its own block, entered
 * by falling through or by a GO TO node (the context the GO TOs look up) */
static struct LwGctx { int lo, hi; int *blk; } *g_lw_gctx; static int g_lw_ngctx, g_lw_gcap;
static void lw_gen_inlined(LStmt *s)
{
    int np = s->inl_hi - s->inl_lo + 1;
    int *blk = xmalloc((size_t)np * sizeof *blk);
    for (int k = 0; k < np; k++) blk[k] = hir_new_block();
    LW_GROW(g_lw_gctx, g_lw_ngctx, g_lw_gcap);        /* (CCVS NC102A nests them past sixteen) */
    g_lw_gctx[g_lw_ngctx].lo = s->inl_lo; g_lw_gctx[g_lw_ngctx].hi = s->inl_hi; g_lw_gctx[g_lw_ngctx].blk = blk; g_lw_ngctx++;
    for (int k = 0; k < np; k++) {
        lw_goto(blk[k]);
        lw_begin_blk(blk[k]);
        lw_gen_stmts(s->body + s->poff[k], s->poff[k + 1] - s->poff[k]);
    }
    g_lw_ngctx--;
    free(blk);
}
static void lw_gen_stmts(int at, int n)
{
    for (int i = 0; i < n && lw_blk_live; i++) {
        LStmt *s = &g_lw_s[g_lw_list[at + i]];
        switch (s->kind) {
        case LS_STORE: lw_gen_store(s); break;
        case LS_ADDTO: lw_gen_addto(s); break;
        case LS_AMOVE: lw_gen_amove(s); break;
        case LS_DISPLAY: lw_gen_display(s); break;
        case LS_TEXT: if (s->inl) lw_gen_inlined(s); else lw_gen_text(g_lw_list[at + i]); break;
        case LS_GOTO: {
            int k; for (k = g_lw_ngctx - 1; k >= 0 && (s->gto < g_lw_gctx[k].lo || s->gto > g_lw_gctx[k].hi); k--) ;
            if (k < 0) die_at(s->line, "internal: a GO TO outside any inlined range holding its target reached an island");
            lw_goto(g_lw_gctx[k].blk[s->gto - g_lw_gctx[k].lo]);
            break;
        }
        case LS_IF: {
            int b_then = hir_new_block(), b_else = hir_new_block(), b_join = hir_new_block();
            lw_cond_br(s->cond, b_then, b_else);
            lw_begin_blk(b_then); lw_gen_stmts(s->body, s->nbody); lw_goto(b_join);
            lw_begin_blk(b_else); lw_gen_stmts(s->els, s->nels); lw_goto(b_join);
            lw_begin_blk(b_join);
            break;
        }
        case LS_LOOP: lw_gen_loop(s); break;
        }
    }
}

/* the island's statements in g_lw_list[at .. at+n): the lowering the
 * backend calls (hcg_func) */
static int g_lw_at, g_lw_n_stmts;
static void hl_func(Node *fn)
{
    (void)fn;
    hir_reset();
    hl_nalloca = 0; hl_temp_stack = 0; hl_nparams = 0; hl_param_nflat = 0;
    lw_frame = 8;                               /* the saved r31 and r30 (f77_contract.h tells why) */
    g_lw_nitem = 0; g_lw_items_closed = 0;
    int b = hir_new_block();
    lw_begin_blk(b);
    lw_collect_stmts(g_lw_at, g_lw_n_stmts);
    g_lw_items_closed = 1;
    lw_entry_loads();
    lw_gen_stmts(g_lw_at, g_lw_n_stmts);
    if (lw_blk_live) { lw_exit_stores(); hi_emit(HI_RET, 0, -1, -1, 0, NULL); lw_blk_live = 0; }
    fn->locals_size = lw_frame;
    hir_dump("HIR0");                           /* S32_HIR_DUMP: as lowered, before the optimizer */
}

/* the backend's text, into lines of the unit's: its 4-space indent a
 * tab, so relax_branches counts each instruction as the 4 bytes it is */
static void lw_take_text(char ***out, int *no, int *cap)
{
    int i = 0;
    while (i < cg_olen) {
        int j = i; while (j < cg_olen && cg_out[j] != '\n') j++;
        int n = j - i; const char *l = cg_out + i;
        if (n > 0) {
            char *s;
            if (n >= 4 && !memcmp(l, "    ", 4)) { s = xmalloc((size_t)n - 3 + 1); s[0] = '\t'; memcpy(s + 1, l + 4, (size_t)n - 4); s[n - 3] = 0; }
            else s = xstrndup(l, (size_t)n);
            if (*no == *cap) { *cap = *cap ? 2 * *cap : 256; *out = xrealloc(*out, (size_t)*cap * sizeof **out); }
            (*out)[(*no)++] = s;
        }
        i = j + 1;
    }
    cg_olen = 0;
}

/* Does a run of statements pay as an island?  A loop among them, or
 * enough of them (lw_min), or one that is heavy: decimals, an eight-byte
 * item, ROUNDED -- what the text emitter's word path (arith_reg.h hx_*)
 * does not take and sends through the runtime's fetch and store, where
 * an island computes in place (kedit's COMPUTE and ADD alone: -14%).
 * Over integers the text is already in place -- a product past a word
 * included, which it computes in a word with an overflow test and takes
 * the slow path only when it overflows, where the island computes the
 * pair and divides it by a routine (ksearch's MOD alone: +5%; with the
 * product two instructions, still +4.6%: the remainder is the cost) --
 * and a statement alone there costs the call (kmove +0.3%).  The
 * island's own checked word path is a lever not pulled yet. */
static int lw_heavy_node(int n)
{
    if (n < 0) return 0;
    LNode *x = &g_lw_n[n];
    if (!x->op) return g_sym[x->sym].native && x->ref < 0 && (x->sc > 0 || g_sym[x->sym].size == 8);   /* one in storage is the runtime's fetch either way */
    if (x->op == 'k') return x->sc > 0;
    if (x->sc > 0) return 1;
    return lw_heavy_node(x->l) || lw_heavy_node(x->r);
}
static int lw_heavy_cond(int c)
{
    LCond *x = &g_lw_c[c];
    if (x->kind == C_REL) return lw_heavy_node(x->x) || lw_heavy_node(x->y);
    return lw_heavy_cond(x->a) || (x->kind != C_NOT && lw_heavy_cond(x->b));
}
static int g_lw_ntext, g_lw_nperform;           /* of the run being counted: text statements, and those that PERFORM */
static int lw_count(int at, int n, int *loops, int *heavy)
{
    int c = 0;
    for (int i = 0; i < n; i++) {
        LStmt *s = &g_lw_s[g_lw_list[at + i]];
        c++;
        if (s->kind == LS_TEXT && s->inl) c--;                                  /* an inlined PERFORM is its body, counted below */
        else if (s->kind == LS_TEXT) { g_lw_ntext++; if (s->nperf) g_lw_nperform++; if (s->phrase >= 0) { LStmt *ph = &g_lw_s[s->phrase]; c += lw_count(ph->body, ph->nbody, loops, heavy) + lw_count(ph->els, ph->nels, loops, heavy); } }
        if (s->kind == LS_LOOP) (*loops)++;
        if (s->expr >= 0 && lw_heavy_node(s->expr)) *heavy = 1;
        int numeric = s->kind == LS_STORE || s->kind == LS_ADDTO;         /* rsym is theirs; a MOVE of bytes or a DISPLAY has operands instead */
        if (numeric) for (int k = 0; k < s->nr; k++) if (g_sym[s->rsym[k]].native && s->rref[k] < 0 && (s->rnd[k] || g_sym[s->rsym[k]].pi.scale > 0 || g_sym[s->rsym[k]].size == 8)) *heavy = 1;
        /* a quotient with decimals, or rounded: the word path leaves it to the stack's division */
        if (s->expr >= 0 && g_lw_n[s->expr].op == '/')
            for (int k = 0; k < s->nr; k++) if (s->rnd[k] || g_sym[s->rsym[k]].pi.scale > 0) *heavy = 1;
        if (s->cond >= 0 && lw_heavy_cond(s->cond)) *heavy = 1;
        c += lw_count(s->body, s->nbody, loops, heavy) + lw_count(s->els, s->nels, loops, heavy);
    }
    return c;
}

/* The runs of placeholders from line `from` on are resolved: a run that
 * pays becomes "jal r31, .LislN" and its island's text waits in g_lw_pend
 * (lw_flush puts it after the unit's code); one that does not becomes
 * its statements' own text.  Called before anything reads the code as
 * code -- loopreg's regions at an outermost in-line PERFORM's end, and
 * its unit-wide pass -- since a placeholder is an instruction nobody
 * lists, and would make it forget what the registers hold (csv2fw lost
 * 6% that way before this ran first). */
/* does the run name a native item at all?  an island of none has nothing
 * to hold in registers */
static int lw_node_native(int n)
{
    if (n < 0) return 0;
    LNode *x = &g_lw_n[n];
    if (!x->op) return g_sym[x->sym].native && x->ref < 0;
    if (x->op == 'k') return 0;
    return lw_node_native(x->l) || lw_node_native(x->r);
}
static int lw_cond_native(int c)
{
    LCond *x = &g_lw_c[c];
    if (x->kind == C_REL) return x->alnum ? 0 : lw_node_native(x->x) || lw_node_native(x->y);
    return lw_cond_native(x->a) || (x->kind != C_NOT && lw_cond_native(x->b));
}
static int lw_native_items(int at, int n)
{
    for (int i = 0; i < n; i++) {
        LStmt *s = &g_lw_s[g_lw_list[at + i]];
        if (s->expr >= 0 && lw_node_native(s->expr)) return 1;
        if (s->kind == LS_STORE || s->kind == LS_ADDTO) for (int k = 0; k < s->nr; k++) if (g_sym[s->rsym[k]].native && s->rref[k] < 0) return 1;
        if (s->cond >= 0 && lw_cond_native(s->cond)) return 1;
        for (int k = 0; k < s->nv; k++) if (g_sym[s->var[k]].native || lw_node_native(s->from[k]) || lw_node_native(s->by[k]) || lw_cond_native(s->vcond[k])) return 1;
        if (lw_native_items(s->body, s->nbody) || lw_native_items(s->els, s->nels)) return 1;
        if (s->kind == LS_TEXT && s->phrase >= 0) { LStmt *ph = &g_lw_s[s->phrase]; if (lw_native_items(ph->body, ph->nbody) || lw_native_items(ph->els, ph->nels)) return 1; }
    }
    return 0;
}

/* do the text statements of the run come back?  each PERFORMed range, by
 * the census of the whole unit */
static int lw_run_returns(int at, int n)
{
    for (int i = 0; i < n; i++) {
        LStmt *s = &g_lw_s[g_lw_list[at + i]];
        if (s->kind == LS_TEXT)
            for (int k = 0; k < s->nperf; k++) {
                int lo = g_lw_perf[s->perf0 + k].lo, hi = lw_range_hi(lo, g_lw_perf[s->perf0 + k].hi);
                if (!lw_range_returns(lo, hi)) { if (lw_trace()) fprintf(stderr, "hir: line %d: PERFORM of a range that may not come back\n", s->line); return 0; }
            }
        if (!lw_run_returns(s->body, s->nbody) || !lw_run_returns(s->els, s->nels)) return 0;
        if (s->kind == LS_TEXT && s->phrase >= 0) { LStmt *ph = &g_lw_s[s->phrase]; if (!lw_run_returns(ph->body, ph->nbody) || !lw_run_returns(ph->els, ph->nels)) return 0; }
    }
    return 1;
}
/* ---- a PERFORM of a paragraph as the paragraph's own nodes ----------
 * A plain PERFORM of a range (once, out of line) whose paragraphs hold
 * nothing but nodes, and which comes back, is emitted as those nodes in
 * its place -- no perform stack, and the items stay in registers across
 * it.  The paragraph's own code stays for its other callers, and a text
 * node among the shared statements is relabelled wherever it is emitted
 * (lw_relabel).  Decided when the unit is read whole, before its runs are
 * resolved: the paragraphs' placeholders are read off the text between
 * the paragraph's label and its end mark (#@E, emit_exit_check). */
static int lw_para_nodes(int id)
{
    char lab[32]; snprintf(lab, sizeof lab, ".Lp%d_%d:", g_unit, id);
    char mark[16]; snprintf(mark, sizeof mark, "#@E %d", id);
    int i = 0, st;
    for (; i < g_nasm && strncmp(g_asm[i], lab, strlen(lab)); i++) ;
    if (i == g_nasm) return 0;
    for (i++; i < g_nasm && strcmp(g_asm[i], mark); i++) {
        const char *l = g_asm[i];
        if (lw_is_place(l, &st)) { lw_list_add(st); continue; }
        if (!l[0] || (l[0] == '#' && l[1] == '@') || !strncmp(l, "__ln_", 5) || !strncmp(l, "\t.globl __ln_", 13)) continue;
        return 0;                               /* code no node accounts for */
    }
    return i < g_nasm;
}
/* does the list reach statement target through the inlined PERFORMs' bodies? */
static int lw_reaches(int at, int n, int target)
{
    for (int i = 0; i < n; i++) {
        int k = g_lw_list[at + i]; LStmt *s = &g_lw_s[k];
        if (k == target) return 1;
        if (lw_reaches(s->body, s->nbody, target) || lw_reaches(s->els, s->nels, target)) return 1;
        if (s->kind == LS_TEXT && s->phrase >= 0) { LStmt *ph = &g_lw_s[s->phrase]; if (lw_reaches(ph->body, ph->nbody, target) || lw_reaches(ph->els, ph->nels, target)) return 1; }
    }
    return 0;
}
static void lw_inline_performs(void)
{
    int only = -1;
    { const char *e = getenv("S32_HIR_INLINE"); if (e) { only = atoi(e); if (!only) return; } }    /* S32_HIR_INLINE=0: no PERFORM is inlined; =line: that line's alone (bisection) */
    for (int k = 0; k < g_lw_ns; k++) {
        LStmt *s = &g_lw_s[k];
        if (only > 0 && s->line != only) continue;
        if (s->kind != LS_TEXT || !s->is_perform || s->inl || s->nperf != 1 || !g_lw_perf[s->perf0].once || s->phrase >= 0) continue;
        int lo = g_lw_perf[s->perf0].lo, hi = lw_range_hi(lo, g_lw_perf[s->perf0].hi);
        if (lo < 1 || hi > g_npara || g_para[lo - 1].unit != g_unit) continue;
        if (!lw_range_returns(lo, hi)) continue;
        int at = g_lw_nlist, ok = 1;
        int *poff = xmalloc((size_t)(hi - lo + 2) * sizeof *poff);
        for (int p = lo; p <= hi && ok; p++) { poff[p - lo] = g_lw_nlist - at; ok = lw_para_nodes(p); }
        poff[hi - lo + 1] = g_lw_nlist - at;
        if (!ok) { g_lw_nlist = at; free(poff); if (lw_trace()) fprintf(stderr, "hir: line %d PERFORM: not inlined: a paragraph of %s..%s holds code no node accounts for\n", s->line, g_para[lo - 1].name, g_para[hi - 1].name); continue; }
        int n = g_lw_nlist - at;
        if (!lw_gotos_ok(at, n, lo, hi)) { g_lw_nlist = at; free(poff); if (lw_trace()) fprintf(stderr, "hir: line %d PERFORM: not inlined: a GO TO out of %s..%s\n", s->line, g_para[lo - 1].name, g_para[hi - 1].name); continue; }
        if (lw_reaches(at, n, k)) { g_lw_nlist = at; free(poff); if (lw_trace()) fprintf(stderr, "hir: line %d PERFORM: not inlined: recursive\n", s->line); continue; }
        s->body = at; s->nbody = n; s->inl = 1; s->inl_lo = lo; s->inl_hi = hi; s->poff = poff;
    }
    /* the size, with every PERFORM inside inlined too: the innermost one
     * past the cap goes back to a text node, and the ones around it
     * shrink -- until nothing changes.  Without this the whole program folded into one
     * island of 1,500 statements and 22 items (csv2fw, +16%; 64, 128 and 256 are 3.28, 3.26 and 3.18 G there, 0.17 s all three). */
    int cap = 128; { const char *e = getenv("S32_HIR_INL"); if (e && atoi(e) > 0) cap = atoi(e); }
    for (int again = 1; again; ) {
        again = 0;
        int worst = -1, wcount = 0;
        for (int k = 0; k < g_lw_ns; k++) {
            LStmt *s = &g_lw_s[k];
            if (s->kind != LS_TEXT || !s->inl) continue;
            int loops = 0, heavy = 0, nt0 = g_lw_ntext, np0 = g_lw_nperform, count = lw_count(s->body, s->nbody, &loops, &heavy);
            g_lw_ntext = nt0; g_lw_nperform = np0;
            if (count > cap && (worst < 0 || count < wcount)) { worst = k; wcount = count; }   /* the smallest past the cap: innermost, so the ones around it shrink */
        }
        if (worst >= 0) {
            LStmt *s = &g_lw_s[worst]; s->inl = 0; s->nbody = 0; again = 1;
            if (lw_trace()) fprintf(stderr, "hir: line %d PERFORM: not inlined: %d statements (S32_HIR_INL=%d)\n", s->line, wcount, cap);
        }
    }
    if (lw_trace())
        for (int k = 0; k < g_lw_ns; k++) {
            LStmt *s = &g_lw_s[k];
            if (s->kind != LS_TEXT || !s->inl) continue;
            int loops = 0, heavy = 0, nt0 = g_lw_ntext, np0 = g_lw_nperform, count = lw_count(s->body, s->nbody, &loops, &heavy);
            g_lw_ntext = nt0; g_lw_nperform = np0;
            fprintf(stderr, "hir: line %d PERFORM %s%s%s: inlined, %d statement%s\n", s->line, g_para[s->inl_lo - 1].name, s->inl_hi > s->inl_lo ? " THRU " : "", s->inl_hi > s->inl_lo ? g_para[s->inl_hi - 1].name : "", count, count == 1 ? "" : "s");
        }
}
/* the in-line PERFORM whose code begins at lay0 folded into one node: the
 * line before is its placeholder (sort.h asks before resolving its text) */
static int lw_loop_folded_at(int lay0) { int st; return lay0 > 0 && lw_is_place(g_asm[lay0 - 1], &st) && g_lw_s[st].kind == LS_LOOP; }

static int g_lw_final;                          /* the unit is read whole: every PERFORM is known */
static int g_lw_again;                          /* a pass restored text that holds placeholders: another pass */
static void lw_resolve_1(int from);
static void lw_resolve(int from)
{
    do { g_lw_again = 0; lw_resolve_1(from); } while (g_lw_again);
}
/* the island for the statements g_lw_list[at..at+n): its text waits in
 * g_lw_pend; the line that calls it is returned */
static char *lw_make_island(int at, int n, int count, int ntext)
{
    char name[32]; snprintf(name, sizeof name, ".Lisl%d", g_lw_nisland++);
    Node fn; memset(&fn, 0, sizeof fn);
    fn.name = xstrndup(name, strlen(name)); fn.is_static = 1;
    g_lw_at = at; g_lw_n_stmts = n;
    hl_cur_fn_dbg = fn.name;
    /* text statements inside: the bottom of the frame is theirs, and
     * r11-r13, r30 (hir_contract.h: the backend's knobs) */
    g_lw_has_text = ntext > 0;
    hcg_frame_reserve = g_lw_has_text ? FRAME : 0;
    ra_callee_skip = g_lw_has_text ? 3 : 0;
    hcg_r30_keep = g_lw_has_text;
    hcg_text_call = lw_text_call; hcg_text_is = lw_text_is;
    hd_fn = getenv("S32_HIR_DUMP");            /* =.LislN: that island's HIR after the optimizer, to stderr (the backend's -dhir) */
    cg_olen = 0; cg_njt = 0; cg_njt_ent = 0; cg_nfn = 0; cg_cur_fn = -1; cg_fd = -1;
    hcg_func(&fn);
    if (cg_njt) die_at(g_lw_s[g_lw_list[at]].line, "internal: an island made a jump table");
    if (lw_trace()) fprintf(stderr, "hir: %s: %d statement%s (%d text) from line %d, %d item%s, %d HIR instructions\n", name, count, count == 1 ? "" : "s",
                            ntext, g_lw_s[g_lw_list[at]].line, g_lw_nitem, g_lw_nitem == 1 ? "" : "s", h_ninst);
    LW_GROW(g_lw_pend, g_lw_npend, g_lw_pcap);
    LwPend *pd = &g_lw_pend[g_lw_npend++]; pd->name = fn.name; pd->line = NULL; pd->n = 0;
    { int pc = 0; lw_take_text(&pd->line, &pd->n, &pc); }
    char call[48]; snprintf(call, sizeof call, "\tjal r31, %s", name);
    return xstrndup(call, strlen(call));
}

/* the lines a run of statements becomes, appended to out: an island's
 * call when the run pays as one; else the run split -- each loop alone,
 * each stretch free of text statements alone, a text statement as its
 * text -- and each piece judged again; a statement alone that does not
 * pay is its text, whose inner placeholders (if any) are resolved by the
 * next pass (g_lw_again) */
static char **g_lw_out; static int g_lw_nout, g_lw_ocap2;     /* (not g_lw_no: that is the operands' count, and one tentative definition joined them) */
static void lw_out(char *l) { LW_GROW(g_lw_out, g_lw_nout, g_lw_ocap2); g_lw_out[g_lw_nout++] = l; }
static void lw_out_text(int st)
{
    Block *t = &g_lw_s[st].text;
    for (int j = 0; j < t->n; j++) { int st2; if (lw_is_place(t->line[j], &st2)) g_lw_again = 1; lw_out(t->line[j]); }
}
static void lw_resolve_run(int at, int n)
{
    g_lw_ntext = g_lw_nperform = 0;
    int loops = 0, heavy = 0, count = lw_count(at, n, &loops, &heavy), ntext = g_lw_ntext;
    int ob, oa = lw_only(&ob), run = g_lw_s[g_lw_list[at]].line;
    int skip = oa >= 0 && (run < oa || run > ob);
    if (!lw_gotos_ok(at, n, 1, 0)) { skip = 1; if (lw_trace() && n > 1) fprintf(stderr, "hir: line %d: %d statements: a GO TO outside an inlined range\n", run, count); }
    /* text statements pay only inside a loop whose own statements
     * outnumber them three to one and whose items are in registers
     * (kreport's READ loop +4%, ksort's RETURN loop +3% as islands) */
    if (ntext && !skip) {
        /* a run in a paragraph that a PERFORM iterates is in a loop too, but
         * not one of its own: it loads its items at entry, syncs them round
         * each text node and stores them at exit every time, and the text
         * does none of that -- measured at par at best (csv2fw +0.2% with
         * its 23 such islands, after the inline DISPLAY load, short-circuit
         * conditions and immediate compares were added for them) */
        const char *why = !loops ? (g_lw_final && lw_para_looped(g_lw_s[g_lw_list[at]].para) ? "no loop of its own (its paragraph is performed in one)" : "no loop") : ntext * 3 > count ? "text statements more than a third" : !lw_native_items(at, n) ? "no native item" : !lw_run_returns(at, n) ? "a PERFORM may not come back" : NULL;
        if (why) { skip = 1; if (lw_trace()) fprintf(stderr, "hir: line %d: %d statements, %d text: %s\n", run, count, ntext, why); }
    }
    if (!skip && (loops || heavy || count >= lw_min())) { lw_out(lw_make_island(at, n, count, ntext)); return; }
    if (n == 1) {
        if (lw_trace()) fprintf(stderr, "hir: line %d: %d statement%s kept as text\n", g_lw_s[g_lw_list[at]].line, count, count == 1 ? "" : "s");
        lw_out_text(g_lw_list[at]);
        return;
    }
    /* the pieces */
    int i = 0;
    while (i < n) {
        int k = g_lw_list[at + i];
        if (g_lw_s[k].kind == LS_TEXT || !lw_gotos_ok(at + i, 1, 1, 0)) { lw_out_text(k); i++; continue; }
        if (g_lw_s[k].kind == LS_LOOP) { lw_resolve_run(at + i, 1); i++; continue; }
        int j = i;
        while (j < n && g_lw_s[g_lw_list[at + j]].kind != LS_TEXT && g_lw_s[g_lw_list[at + j]].kind != LS_LOOP && lw_gotos_ok(at + j, 1, 1, 0)) j++;
        if (j - i == n) {                       /* no piece smaller than the whole: its text */
            for (int q = 0; q < n; q++) lw_out_text(g_lw_list[at + q]);
            if (lw_trace()) fprintf(stderr, "hir: line %d: %d statement%s kept as text\n", g_lw_s[g_lw_list[at]].line, count, count == 1 ? "" : "s");
            return;
        }
        lw_resolve_run(at + i, j - i);
        i = j;
    }
}
static void lw_resolve_1(int from)
{
    int any = 0;
    for (int i = from; i < g_nasm && !any; i++) { int st; any = lw_is_place(g_asm[i], &st); }
    if (!any) return;
    g_lw_nout = 0;
    for (int i = from; i < g_nasm; i++) {
        int st;
        if (!lw_is_place(g_asm[i], &st)) { lw_out(g_asm[i]); continue; }
        int at = g_lw_nlist, n = 0;
        for (; i < g_nasm && lw_is_place(g_asm[i], &st); i++) { lw_list_add(st); n++; }
        i--;
        g_lw_ntext = g_lw_nperform = 0;
        int loops = 0, heavy = 0; lw_count(at, n, &loops, &heavy);
        if (g_lw_nperform && !g_lw_final) {
            /* a PERFORM of a range: whether it comes back is known when the
             * unit is read whole -- the placeholders wait */
            g_lw_nlist = at;
            for (int k = 0; k < n; k++) lw_out(g_asm[i - n + 1 + k]);
            continue;
        }
        lw_resolve_run(at, n);
    }
    while (from + g_lw_nout > g_asmcap) { g_asmcap = g_asmcap ? 2 * g_asmcap : 4096; g_asm = xrealloc(g_asm, (size_t)g_asmcap * sizeof *g_asm); }
    memcpy(g_asm + from, g_lw_out, (size_t)g_lw_nout * sizeof *g_lw_out);
    g_nasm = from + g_lw_nout;
}

/* the unit's code is complete and returned: the islands it calls follow
 * it.  One made inside a loop that was then lowered whole is called by
 * nothing, and is left out. */
static void lw_flush(void)
{
    for (int k = 0; k < g_lw_npend; k++) {
        LwPend *pd = &g_lw_pend[k];
        char call[48]; snprintf(call, sizeof call, "\tjal r31, %s", pd->name);
        int used = 0;
        for (int i = 0; i < g_nasm && !used; i++) used = !strcmp(g_asm[i], call);
        if (!used) { if (lw_trace()) fprintf(stderr, "hir: %s: called by nothing, dropped\n", pd->name); continue; }
        for (int j = 0; j < pd->n; j++) {
            emit("%s", pd->line[j]);
            /* -fprofile-lines: a global alias on the island's label, so that
             * bench/prof.py can tell the islands apart (a .L label is not in
             * the symbol table) */
            if (g_proflines && pd->line[j][0] == '.' && !strncmp(pd->line[j], pd->name, strlen(pd->name)) && pd->line[j][strlen(pd->name)] == ':') {
                emit("\t.globl __isl_%s", pd->name + 5);
                emit("__isl_%s:", pd->name + 5);
            }
        }
    }
    g_lw_npend = 0;
    /* (the nodes, operands and census records are kept: a contained
     * program is compiled before its container's end, and the container's
     * placeholders still name theirs; paragraph ids are unique across the
     * units, so the census needs no fence either) */
}
