/* s32-cobc: integer items kept in registers across an in-line loop.  A
 * part of one translation unit, included by s32-cobc.c in order; not a
 * header to include anywhere else. */

/* A binary item's every use is a load from storage -- for a COMP item
 * six instructions, the address and a byte swap -- and a loop's own item
 * is loaded to be tested, loaded to be stepped and loaded wherever the
 * body uses it (docs/performance.md; docs/plans/performance.md, stage 3).
 * Inside an in-line PERFORM's loop such an item is kept in a callee-saved
 * register as well as in storage: a load is a copy, a store is the store
 * and a copy.  Storage stays current, so whatever reads the item some
 * other way reads it right; what has to be ruled out is anything that
 * could change the item some other way, behind the register.
 *
 * That is decided from the loop's code, once it is all there, and not
 * from the statements: the code is what runs.  Every line of the region
 * is read (lr_scan), with what each register and frame slot holds
 * followed as far as "an address in this record" -- and:
 *
 *   - a store through an address is a store into that address's record
 *     (two records never share storage: layout.h gives a redefinition its
 *     subject's label; an item reached through a cell -- LINKAGE, LOCAL-
 *     STORAGE, EXTERNAL, BASED -- has no label and is an address nobody
 *     knows);
 *   - a call is what lr_call says its routine may write, and a routine
 *     it does not list may write anything;
 *   - a jump or a branch to anything but a label of the compiler's own, a
 *     jump through a register, an instruction not listed: anything may
 *     have happened.  An out-of-line PERFORM and a GO TO are such jumps; a
 *     declarative is reached by one.
 *
 * An item that none of it can touch, other than by its own marked stores
 * (emit.h, "Marks"), is given a register: loaded once where the loop is
 * entered, and its marked loads and stores rewritten.  A mark is only a
 * claim -- the lines under it are checked to be what it says, a load from
 * or a store to exactly that address -- so a mark that is wrong costs the
 * rewrite and nothing else.
 *
 * Regions nest as loops do.  The outermost is done first, so that an
 * item used in an inner loop is loaded before the outer one; what it
 * could not take (the inner loop's own item, set where the inner loop
 * begins) the inner region is asked for next, with the registers still
 * free.
 *
 * The same reading does a second thing, with no loop (lr_unit, at a
 * unit's end, over all its code): it follows which item each of the four
 * registers holds as the code is read -- an item loaded or stored is held
 * from there until something may store into it, a label nothing is known
 * at is passed, or a loop that owns the register begins -- and a load of
 * an item that is held, by every way to that load, is a copy.  Nothing is
 * refused here: what would have refused an item in a loop only ends its
 * being held.  A load or a store puts its value in a register only when
 * some later load takes it from there.
 *
 * -fno-loop-reg leaves all of it out, and -fno-hot-arith does;
 * -fno-avail-reg leaves the second out.  The harness compiles every
 * program with and without and compares what they print. */

#define LR_NREG 4
static const char *const lr_regname[LR_NREG] = { "r14", "r15", "r16", "r17" };
#define SLOT_LR(i)  (116 + 4 * (i))     /* their places in the frame (emit.h, FRAME) */
static int g_lr_used;                   /* which of them this unit's code uses: its entry saves those */
static int g_noavailreg;                /* -fno-avail-reg: only the loops' items */
static int g_inline_depth;              /* in-line PERFORM bodies being read (sort.h) */

typedef struct { unsigned char k; char label[48]; long off; } LrVal;
enum { LV_UNK, LV_HI, LV_AT, LV_IN };   /* nothing known; %hi(label+off) so far; that address; somewhere in label's record */

typedef struct {
    int sym; long off; int size; char label[48];
    int nl, ns;                         /* its marked loads and stores that are what they say */
    int conflict;                       /* something else may store into it */
    int reg;                            /* the register it is given, or -1 */
} LrItem;
#define LR_MAXITEM 24

typedef struct { char op[12]; char a[3][96]; int n; } LrIns;

static int lr_is_mark(const char *l) { return l[0] == '#' && l[1] == '@'; }

/* an instruction line into its mnemonic and operands; 0 for anything else */
static int lr_ins(const char *l, LrIns *x)
{
    if (l[0] != '\t' || l[1] == '.' || l[1] == '#' || !l[1]) return 0;
    const char *p = l + 1; int n = 0;
    while (*p && *p != ' ' && *p != '\t' && n < 11) x->op[n++] = *p++;
    x->op[n] = 0; x->n = 0;
    if (*p && *p != ' ' && *p != '\t') return 0;
    while (*p) {
        while (*p == ' ' || *p == '\t') p++;
        if (!*p || *p == '#') break;
        if (x->n == 3) return 0;
        int k = 0, depth = 0;
        while (*p && (depth || (*p != ',' && *p != '\t' && *p != '#')) && k < 95) {
            if (*p == '(') depth++; else if (*p == ')') depth--;
            x->a[x->n][k++] = *p++;
        }
        while (k > 0 && x->a[x->n][k - 1] == ' ') k--;
        x->a[x->n][k] = 0; x->n++;
        if (*p == ',') p++;
    }
    return 1;
}

static int lr_reg(const char *s)
{
    if (!strcmp(s, "sp")) return 29;
    if (!strcmp(s, "fp")) return 30;
    if (!strcmp(s, "lr")) return 31;
    if (s[0] != 'r' || !isdigit((unsigned char)s[1])) return -1;
    char *e; long v = strtol(s + 1, &e, 10);
    return (*e || v < 0 || v > 31) ? -1 : (int)v;
}

/* "r3+1", "sp+12": the base register and the displacement */
static int lr_mem(const char *s, int *base, long *disp)
{
    const char *plus = strchr(s, '+');
    if (!plus || plus - s > 7) return 0;
    char r[8]; memcpy(r, s, (size_t)(plus - s)); r[plus - s] = 0;
    *base = lr_reg(r);
    if (*base < 0) return 0;
    char *e; *disp = strtol(plus + 1, &e, 10);
    return !*e && e != plus + 1;
}

/* "%hi(ws0_5+12)" or "%lo(...)": the label and the offset */
static int lr_sym(const char *s, const char *which, char *label, int cap, long *off)
{
    size_t w = strlen(which);
    if (strncmp(s, which, w) || s[w] != '(') return 0;
    const char *p = s + w + 1; int n = 0;
    while (*p && *p != '+' && *p != ')' && n < cap - 1) label[n++] = *p++;
    label[n] = 0; *off = 0;
    if (*p == '+') { char *e; *off = strtol(p + 1, &e, 10); p = e; }
    return *p == ')' && p[1] == 0 && n > 0;
}

/* a label of the compiler's own making: .L and digits.  Paragraphs
 * (.Lp), programs, and names are not */
static int lr_local(const char *t)
{
    if (t[0] != '.' || t[1] != 'L' || !isdigit((unsigned char)t[2])) return 0;
    for (const char *p = t + 2; *p; p++) if (!isdigit((unsigned char)*p)) return 0;
    return 1;
}

/* a store of [lo, hi) into label's record -- hi < 0: from lo on, how far
 * is not known; label NULL: nobody knows where.  own: the item whose
 * marked store this is, which it does not count against. */
static const char *g_lr_why;            /* S32_LR_TRACE: the line being read, for "refused because of" */
static void lr_refuse(LrItem *x)
{
    if (!x->conflict && g_lr_why) fprintf(stderr, "loopreg: %s+%ld refused:%s\n", x->label, x->off, g_lr_why);
    x->conflict = 1;
}
static void lr_store(LrItem *it, int nit, const char *label, long lo, long hi, int own)
{
    for (int k = 0; k < nit; k++) {
        if (k == own) continue;
        if (!label) { lr_refuse(&it[k]); continue; }
        if (strcmp(label, it[k].label)) continue;
        if (lo < it[k].off + it[k].size && (hi < 0 || hi > it[k].off)) lr_refuse(&it[k]);
    }
}
static void lr_store_val(LrItem *it, int nit, const LrVal *v)
{
    if (v->k == LV_AT) lr_store(it, nit, v->label, v->off, -1, -1);
    else if (v->k == LV_IN) lr_store(it, nit, v->label, LONG_MIN, -1, -1);
    else lr_store(it, nit, NULL, 0, 0, -1);
}

/* an item of a file's -- its FILE STATUS, its DEPENDING ON -- as a store */
static void lr_store_sym(LrItem *it, int nit, const Sym *s)
{
    if (!s) return;
    const Sym *rec = &g_sym[s->record];
    if (rec_indirect(rec)) lr_store(it, nit, NULL, 0, 0, -1);
    else lr_store(it, nit, rec->label, s->offset, s->offset + s->size, -1);
}

/* What a call may store into the program's data.  The routines the
 * compiler's own loops call most, by what each does (libcob.c); any
 * other may store anywhere.  v: the registers as they stand at the call. */
static void lr_call(const char *fn, const LrVal *v, LrItem *it, int nit)
{
    static const char *const none[] = {         /* read, compare, display, check; or never come back */
        "cob_cmp", "memcmp", "cob_get_num", "cob_load_int", "cob_display", "cob_display_nl", "cob_display_field",
        "cob_refmod_len", "cob_refmod_len_chk", "cob_refmod_desc", "cob_bound_refmod", "cob_class_bytes", "cob_class",
        "cob_refmod_len_z", "cob_refmod_len_chk_z", "cob_refmod_desc_z", "cob_bound_refmod_z",
        "cob_push", "cob_push_lit", "cob_pop_int", "cob_pop_pos", "cob_io_unhandled", "cob_stop_run", "cob_get_edited",
        /* the arithmetic stacks' own work, and the compiler's 64-bit helpers */
        "cob_nadd", "cob_nsub", "cob_nmul", "cob_ndiv", "cob_nneg", "cob_ncmp", "cob_drop", "cob_xdivn",
        "cob_wpush", "cob_wpush_lit", "cob_wadd", "cob_wsub", "cob_wmul", "cob_wdiv", "cob_wcmp", "cob_wdrop",
        "__muldi3", "__divdi3", "__udivdi3", "__moddi3", "__umoddi3",
        /* a numeric function: from the stack, to a buffer of the runtime's */
        "cob_fn_num", "cob_fn_wnum", NULL };
    static const char *const first[] = {        /* store through their first argument */
        "memcpy", "cob_fill", "cob_fill_all", "cob_put_num_x", "cob_put_edited", "cob_top_store", "cob_top_addto",
        "cob_top_subfrom", "cob_wtop_store", "cob_wtop_addto", "cob_wtop_subfrom", NULL };
    static const char *const third[] = { "cob_move", "cob_move_alnum", NULL };
    for (int k = 0; none[k]; k++) if (!strcmp(fn, none[k])) return;
    for (int k = 0; first[k]; k++) if (!strcmp(fn, first[k])) { lr_store_val(it, nit, &v[3]); return; }
    for (int k = 0; third[k]; k++) if (!strcmp(fn, third[k])) { lr_store_val(it, nit, &v[5]); return; }
    if (!strcmp(fn, "cob_write") || !strcmp(fn, "cob_read")) {
        /* the file's block, its status item; a READ its record area and
         * the item its record's length is left in */
        int u, i; char tail;
        if (v[3].k == LV_AT && v[3].off == 0 && sscanf(v[3].label, ".Lf%d_%d%c", &u, &i, &tail) == 2 && i >= 0 && i < g_nfile &&
            g_files[i].unit == u && !g_files[i].external) {            /* (an EXTERNAL file's status item may be another program's) */
            const File *f = &g_files[i];
            lr_store(it, nit, v[3].label, LONG_MIN, -1, -1);
            lr_store_sym(it, nit, f->status_sym);
            lr_store_sym(it, nit, f->relkey_sym);                      /* a relative file read or written in sequence */
            if (!strcmp(fn, "cob_read")) {
                lr_store_sym(it, nit, f->dep_sym);
                if (f->rec < 0) lr_store(it, nit, NULL, 0, 0, -1);
                else lr_store(it, nit, g_sym[g_sym[f->rec].record].label, LONG_MIN, -1, -1);
            }
            return;
        }
    }
    lr_store(it, nit, NULL, 0, 0, -1);
}

static int lr_find(const LrItem *it, int nit, int sym, long off)
{
    for (int k = 0; k < nit; k++) if (it[k].label[0] && it[k].sym == sym && it[k].off == off) return k;
    return -1;
}

/* the item a mark names, if it is one this file keeps: a binary integer,
 * or an unsigned DISPLAY one, at a constant address */
static int lr_item(LrItem *x, int sym, long off)
{
    if (sym < 0 || sym >= g_nsym) return 0;
    Sym *s = &g_sym[sym]; const Sym *rec = &g_sym[s->record];
    if (!(is_hot_int(s) || is_display_int(s)) || rec_indirect(rec) || !rec->label[0] || strlen(rec->label) >= sizeof x->label) return 0;
    memset(x, 0, sizeof *x);
    x->sym = sym; x->off = off; x->size = s->size; x->reg = -1;
    snprintf(x->label, sizeof x->label, "%s", rec->label);
    return 1;
}

/* a mark's fields: "#@L sym off dreg areg [v]" */
static int lr_mark(const char *l, char *kind, int *sym, long *off, char *vreg, char *areg, int *v)
{
    char flag[4] = "";
    if (!lr_is_mark(l) || (l[2] != 'L' && l[2] != 'S')) return 0;
    *kind = l[2];
    int n = sscanf(l + 3, "%d %ld %7s %7s %3s", sym, off, vreg, areg, flag);
    *v = n == 5 && !strcmp(flag, "v");
    return n >= 4;
}

static int lr_alu(const char *op)
{
    static const char *const ops[] = { "add", "sub", "mul", "mulh", "mulhu", "div", "rem", "and", "or", "xor", "sll", "srl", "sra",
        "slt", "sltu", "sgt", "sgtu", "sge", "sgeu", "sle", "sleu", "seq", "sne",
        "addi", "andi", "ori", "xori", "slli", "srli", "srai", "slti", "sltiu", "lui", NULL };
    for (int k = 0; ops[k]; k++) if (!strcmp(op, ops[k])) return 1;
    return 0;
}
static int lr_load(const char *op) { return !strcmp(op, "ldb") || !strcmp(op, "ldbu") || !strcmp(op, "ldh") || !strcmp(op, "ldhu") || !strcmp(op, "ldw"); }
static int lr_storeop(const char *op) { return !strcmp(op, "stb") ? 1 : !strcmp(op, "sth") ? 2 : !strcmp(op, "stw") ? 4 : 0; }
static int lr_branch(const char *op) { return !strcmp(op, "beq") || !strcmp(op, "bne") || !strcmp(op, "blt") || !strcmp(op, "bge") || !strcmp(op, "bltu") || !strcmp(op, "bgeu"); }

/* What is known where the code is: each register and each word of the
 * frame.  At a label that every jump to it comes from above, it is what
 * those jumps and the line before agree on (lr_merge); at any other --
 * the top of a loop, a label whose address is taken, one nothing here
 * jumps to -- nothing. */
#define LR_MAXDEF 4
typedef struct {
    LrVal reg[32], slot[64];
    /* lr_unit's: the item each of the registers holds (label empty: none),
     * the marked loads and stores that put it there (their lines), and
     * when it was last wanted */
    LrItem h[LR_NREG];
    struct { int n, line[LR_MAXDEF]; unsigned stamp; } hd[LR_NREG];
} LrState;
/* lr_unit's notes, a line each: the register a load takes its item from
 * (use), the one a load or a store leaves its item in (def), and whether
 * anything took it from there (useful); the registers loops own where the
 * reading is (resv), and a clock for "last wanted" */
typedef struct { unsigned char *use, *def, *useful; int resv, rstk[16], nrstk; unsigned clock; } LrAvail;
typedef struct { char name[24]; int def, nfwd, back; LrState *in; } LrLabel;

static void lr_merge_val(LrVal *d, const LrVal *s)
{
    if (d->k == s->k && (d->k == LV_UNK || (!strcmp(d->label, s->label) && (d->k == LV_IN || d->off == s->off)))) return;
    if (d->k >= LV_AT && s->k >= LV_AT && !strcmp(d->label, s->label)) { d->k = LV_IN; return; }
    d->k = LV_UNK;
}
static void lr_merge(LrState *d, const LrState *s)
{
    for (int k = 0; k < 32; k++) lr_merge_val(&d->reg[k], &s->reg[k]);
    for (int k = 0; k < 64; k++) lr_merge_val(&d->slot[k], &s->slot[k]);
    /* a register holds an item here when it holds it by both ways in --
     * put there by any of the loads and stores of either */
    for (int r = 0; r < LR_NREG; r++) {
        if (!d->h[r].label[0]) continue;
        if (!s->h[r].label[0] || d->h[r].sym != s->h[r].sym || d->h[r].off != s->h[r].off) { d->h[r].label[0] = 0; continue; }
        for (int k = 0; k < s->hd[r].n && d->h[r].label[0]; k++) {
            int j; for (j = 0; j < d->hd[r].n && d->hd[r].line[j] != s->hd[r].line[k]; j++) ;
            if (j < d->hd[r].n) continue;
            if (d->hd[r].n == LR_MAXDEF) d->h[r].label[0] = 0;          /* too many to keep track of: not held */
            else d->hd[r].line[d->hd[r].n++] = s->hd[r].line[k];
        }
        if (s->hd[r].stamp > d->hd[r].stamp) d->hd[r].stamp = s->hd[r].stamp;
    }
}
static LrLabel *lr_label(LrLabel *lb, int nlb, const char *name)
{
    for (int k = 0; k < nlb; k++) if (!strcmp(lb[k].name, name)) return &lb[k];
    return NULL;
}
/* the state goes along a jump or a branch to label t */
static void lr_flow(LrLabel *lb, int nlb, const char *t, int at, const LrState *st)
{
    LrLabel *L = lr_label(lb, nlb, t);
    if (!L || L->def < at) return;              /* out of the region; or backward, where nothing is assumed */
    if (!L->in) { L->in = xmalloc(sizeof *L->in); *L->in = *st; }
    else lr_merge(L->in, st);
}

/* a register for an item about to be held: one that holds nothing, or
 * the one whose item was wanted longest ago; -1 when loops own them all */
static int lr_pick(const LrState *st, const LrAvail *av)
{
    int best = -1;
    for (int r = 0; r < LR_NREG; r++) {
        if (av->resv & (1 << r)) continue;
        if (!st->h[r].label[0]) return r;
        if (best < 0 || st->hd[r].stamp < st->hd[best].stamp) best = r;
    }
    return best;
}

/* after each line of a unit's reading: an item something may have stored
 * into is not held any more */
static void lr_sweep(const LrAvail *av, LrState *st)
{
    if (!av) return;
    for (int r = 0; r < LR_NREG; r++) if (st->h[r].conflict) { st->h[r].label[0] = 0; st->h[r].conflict = 0; }
}

/* Read the lines [a, b).  For a loop's region (av NULL): which of the
 * items it[] something else may store into, and which marked loads and
 * stores are what their marks say (ok[i - a] for the mark at line i).
 * For a unit (av): what the registers hold as the reading goes -- it[] is
 * then the four held items themselves, an item stored into simply not
 * held any more -- and av's notes of which loads take an item from a
 * register and which loads and stores leave one there. */
static void lr_scan(int a, int b, LrItem *it, int nit, unsigned char *ok, LrAvail *av)
{
    LrState st; LrVal *reg = st.reg, *slot = st.slot;
    memset(&st, 0, sizeof st);
    if (av) { it = st.h; nit = LR_NREG; }
    LrItem gi; memset(&gi, 0, sizeof gi);          /* the item of the mark being read under */
    int cur = -1, curline = -1, good = 0, dead = 0; char ckind = 0, careg[8] = "";
    char name[128];
    /* the labels defined here, and how each is reached */
    int nlb = 0, lcap = 0; LrLabel *lb = NULL;
    for (int i = a; i < b; i++) {
        if (!line_label(g_asm[i], name, sizeof name) || strlen(name) >= sizeof lb[0].name) continue;
        if (nlb == lcap) { lcap = lcap ? 2 * lcap : 16; lb = xrealloc(lb, (size_t)lcap * sizeof *lb); }
        memset(&lb[nlb], 0, sizeof lb[nlb]);
        memcpy(lb[nlb].name, name, strlen(name) + 1);
        lb[nlb].def = i; nlb++;
    }
    for (int i = a; i < b && nlb; i++) {
        LrIns x; const char *l = g_asm[i];
        if (lr_is_mark(l) || !lr_ins(l, &x)) continue;
        int jump = (lr_branch(x.op) && x.n == 3) ? 2 : (!strcmp(x.op, "jal") && x.n == 2 && lr_reg(x.a[0]) == 0) ? 1 : -1;
        for (int k = 0; k < x.n; k++) {
            LrLabel *L = k == jump ? lr_label(lb, nlb, x.a[k]) : NULL;
            if (L) { if (i < L->def) L->nfwd++; else L->back = 1; continue; }
            /* named any other way -- its address taken: reached from who knows where */
            const char *q = x.a[k];
            while ((q = strstr(q, ".L")) != NULL) {
                char nm[24]; int n = 0;
                while (q[n] && (isalnum((unsigned char)q[n]) || q[n] == '.' || q[n] == '_') && n < 23) { nm[n] = q[n]; n++; }
                nm[n] = 0;
                if ((L = lr_label(lb, nlb, nm)) != NULL) L->back = 1;
                q += n ? n : 2;
            }
        }
    }
    int trace = !av && getenv("S32_LR_TRACE") != NULL;
    for (int i = a; i < b; lr_sweep(av, &st), i++) {
        const char *l = g_asm[i];
        if (trace) g_lr_why = l;
        if (lr_is_mark(l)) {
            char kind, vreg[8], areg[8]; int sym, v; long off;
            if (l[2] == '.') {
                if (av && curline >= 0 && good) {
                    if (ckind == 'L' && cur >= 0) {
                        /* held: the load is a copy, and whatever put the item
                         * there has earned its place */
                        av->use[curline - a] = (unsigned char)(cur + 1);
                        for (int k = 0; k < st.hd[cur].n; k++) av->useful[st.hd[cur].line[k] - a] = 1;
                        st.hd[cur].stamp = ++av->clock;
                    } else {
                        /* loaded, or stored: held from here, in the register
                         * it was in or one picked for it */
                        int r = cur >= 0 ? cur : lr_pick(&st, av);
                        if (r >= 0) {
                            st.h[r] = gi; st.hd[r].n = 1; st.hd[r].line[0] = curline; st.hd[r].stamp = ++av->clock;
                            av->def[curline - a] = (unsigned char)(r + 1);
                        }
                    }
                }
                else if (!av && curline >= 0 && good && cur >= 0) { ok[curline - a] = 1; if (ckind == 'L') it[cur].nl++; else it[cur].ns++; }
                /* a store's mark that did not hold: its lines stay as they
                 * are, a store the register would not see */
                else if (curline >= 0 && cur >= 0 && ckind == 'S') lr_refuse(&it[cur]);
                cur = -1; curline = -1;
            } else if (lr_mark(l, &kind, &sym, &off, vreg, areg, &v)) {
                cur = lr_find(it, nit, sym, off); curline = i; ckind = kind; good = 1;
                snprintf(careg, sizeof careg, "%s", areg);
                /* the claim -- areg is this item's address -- is checked at
                 * each load and store under the mark, below */
                if (av) { if (!lr_item(&gi, sym, off)) good = 0; }
                else if (cur < 0) good = 0;
                else gi = it[cur];
            } else if (av && !strncmp(l, "#@K<", 4)) {
                /* a loop that keeps items of its own in these registers:
                 * they are its until it ends */
                int m = atoi(l + 4);
                if (av->nrstk == 16) m = (1 << LR_NREG) - 1; else av->rstk[av->nrstk++] = av->resv;
                av->resv |= m;
                for (int r = 0; r < LR_NREG; r++) if (av->resv & (1 << r)) st.h[r].label[0] = 0;
            } else if (av && !strcmp(l, "#@K>")) {
                if (av->nrstk) av->resv = av->rstk[--av->nrstk];       /* (its registers hold nothing: lr_pick gave it none) */
            }
            continue;
        }
        if (line_label(l, name, sizeof name)) {
            if (!strncmp(name, "__ln_", 5)) continue;   /* -fprofile-lines' label: a name for a place, nothing jumps to it */
            LrLabel *L = lr_label(lb, nlb, name);
            if (L && !L->back && L->nfwd && L->in) { if (dead) st = *L->in; else lr_merge(&st, L->in); }
            else memset(&st, 0, sizeof st);
            dead = 0; good = 0;
            continue;
        }
        LrIns x;
        if (!lr_ins(l, &x)) { if (l[0] == '\t' && l[1] != '.' && l[1] != '#') goto unknown; continue; }
        int d = x.n > 0 ? lr_reg(x.a[0]) : -1;
        if (lr_storeop(x.op)) {
            int base; long disp; int w = lr_storeop(x.op), src = x.n == 2 ? lr_reg(x.a[1]) : -1;
            if (x.n != 2 || src < 0 || !lr_mem(x.a[0], &base, &disp)) goto unknown;
            if (base == 29) {                           /* the frame */
                if (w == 4 && disp >= 0 && disp < 256 && !(disp & 3)) slot[disp / 4] = reg[src];
                else memset(st.slot, 0, sizeof st.slot);
                if (curline >= 0) good = 0;
                continue;
            }
            const LrVal *bv = &reg[base];
            if (bv->k == LV_AT) {
                int own = -1;
                if (curline >= 0 && ckind == 'S' && good && base == lr_reg(careg) && !strcmp(bv->label, gi.label) &&
                    bv->off == gi.off && disp >= 0 && disp + w <= gi.size) own = cur;      /* (cur -1: not one of it[], and nothing to spare) */
                else if (curline >= 0) good = 0;
                lr_store(it, nit, bv->label, bv->off + disp, bv->off + disp + w, own);
            } else {
                if (curline >= 0) good = 0;
                if (bv->k == LV_IN) lr_store(it, nit, bv->label, LONG_MIN, -1, -1);
                else lr_store(it, nit, NULL, 0, 0, -1);
            }
            continue;
        }
        if (lr_load(x.op)) {
            int base; long disp;
            if (x.n != 2 || d < 0 || !lr_mem(x.a[1], &base, &disp)) goto unknown;
            if (curline >= 0) {
                /* under a load's mark: from the item, and nowhere else */
                const LrVal *bv = &reg[base];
                if (!(ckind == 'L' && good && base == lr_reg(careg) && bv->k == LV_AT && !strcmp(bv->label, gi.label) &&
                      bv->off == gi.off && disp >= 0 && disp < gi.size)) good = 0;
            }
            LrVal nv; memset(&nv, 0, sizeof nv);
            if (base == 29 && !strcmp(x.op, "ldw") && disp >= 0 && disp < 256 && !(disp & 3)) nv = slot[disp / 4];
            if (d == 29) goto unknown;
            if (d) reg[d] = nv;
            continue;
        }
        if (lr_branch(x.op)) {
            if (x.n != 3 || !lr_local(x.a[2])) goto unknown;
            if (curline >= 0) good = 0;
            lr_flow(lb, nlb, x.a[2], i, &st);
            continue;
        }
        if (!strcmp(x.op, "jal")) {
            if (x.n != 2) goto unknown;
            int link = lr_reg(x.a[0]);
            if (curline >= 0) good = 0;
            if (link == 0) {
                if (!lr_local(x.a[1])) goto unknown;
                lr_flow(lb, nlb, x.a[1], i, &st);
                dead = 1;                       /* nothing falls out of a jump */
                continue;
            }
            if (link != 31) goto unknown;
            lr_call(x.a[1], reg, it, nit);
            for (int r = 1; r <= 10; r++) reg[r].k = LV_UNK;
            reg[31].k = LV_UNK;
            continue;
        }
        if (lr_alu(x.op)) {
            if (d < 0 || d == 29) goto unknown;
            LrVal nv; memset(&nv, 0, sizeof nv);
            int s1 = x.n > 1 ? lr_reg(x.a[1]) : -1, s2 = x.n > 2 ? lr_reg(x.a[2]) : -1;
            char lab[48]; long off;
            if (!strcmp(x.op, "lui")) {
                if (x.n == 2 && lr_sym(x.a[1], "%hi", lab, sizeof lab, &off)) { nv.k = LV_HI; snprintf(nv.label, sizeof nv.label, "%s", lab); nv.off = off; }
            } else if (!strcmp(x.op, "addi") && x.n == 3 && s1 >= 0) {
                if (lr_sym(x.a[2], "%lo", lab, sizeof lab, &off)) {
                    if (reg[s1].k == LV_HI && !strcmp(reg[s1].label, lab) && reg[s1].off == off) { nv = reg[s1]; nv.k = LV_AT; }
                } else {
                    char *e; long imm = strtol(x.a[2], &e, 10);
                    if (!*e && e != x.a[2]) {
                        if (reg[s1].k == LV_AT) { nv = reg[s1]; nv.off += imm; }
                        else if (reg[s1].k == LV_IN) nv = reg[s1];
                    }
                }
            } else if (!strcmp(x.op, "add") && x.n == 3 && s1 >= 0 && s2 >= 0) {
                if (s2 == 0) nv = reg[s1];                              /* a copy */
                else if (s1 == 0) nv = reg[s2];
                else if (reg[s1].k >= LV_AT && reg[s2].k == LV_UNK) { nv = reg[s1]; nv.k = LV_IN; }   /* an element of it */
                else if (reg[s2].k >= LV_AT && reg[s1].k == LV_UNK) { nv = reg[s2]; nv.k = LV_IN; }
            } else if (!strcmp(x.op, "sub") && x.n == 3 && s1 >= 0 && s2 >= 0) {
                if (reg[s1].k >= LV_AT && reg[s2].k == LV_UNK) { nv = reg[s1]; nv.k = LV_IN; }
            }
            if (curline >= 0 && nv.k != LV_UNK) good = 0;              /* a unit makes no addresses */
            if (d) reg[d] = nv;
            continue;
        }
    unknown:
        /* a jump through a register, a jump to a paragraph, the frame
         * moved, a line not understood: anything may have happened */
        lr_store(it, nit, NULL, 0, 0, -1);
        memset(&st, 0, sizeof st);
        good = 0;
    }
    g_lr_why = NULL;
    for (int k = 0; k < nlb; k++) free(lb[k].in);
    free(lb);
}

/* what becomes of register r from line i on (to b), as far as the next
 * lines say outright: 1 written before it is read, 0 read, 2 a label, a
 * branch, a jump or a call comes first */
static int lr_after(int i, int b, int r)
{
    char name[128];
    for (; i < b; i++) {
        const char *l = g_asm[i];
        if (lr_is_mark(l)) continue;
        if (line_label(l, name, sizeof name)) { if (!strncmp(name, "__ln_", 5)) continue; return 2; }
        LrIns x;
        if (!lr_ins(l, &x)) { if (l[0] == '\t' && l[1] == '.') continue; return 0; }
        if (lr_branch(x.op)) { if (lr_reg(x.a[0]) == r || lr_reg(x.a[1]) == r) return 0; return 2; }
        if (!strcmp(x.op, "jal") || !strcmp(x.op, "jalr")) return 2;
        int base; long disp;
        if (lr_storeop(x.op)) {
            if (x.n != 2 || !lr_mem(x.a[0], &base, &disp) || base == r || lr_reg(x.a[1]) == r) return 0;
            continue;
        }
        if (lr_load(x.op)) {
            if (x.n != 2 || !lr_mem(x.a[1], &base, &disp) || base == r) return 0;
            if (lr_reg(x.a[0]) == r) return 1;
            continue;
        }
        if (!lr_alu(x.op)) return 0;
        for (int k = 1; k < x.n; k++) if (lr_reg(x.a[k]) == r) return 0;
        if (lr_reg(x.a[0]) == r) return 1;
    }
    return 2;
}

static char *lr_line(const char *fmt, ...)
{
    char buf[256]; va_list ap;
    va_start(ap, fmt); vsnprintf(buf, sizeof buf, fmt, ap); va_end(ap);
    return xstrndup(buf, strlen(buf));
}

/* A marked load of x (its lines end at e, the region or unit at b) as a
 * copy from the register rc.  The address before it -- out's last two
 * lines, when they are that -- goes too when nothing else wants it: the
 * mark's v is the emitter's word that it is wanted no further, taken
 * where the lines cannot say. */
static int lr_put_load(char **out, int no, const LrItem *x, const char *vreg, const char *areg, int v, int e, int b, const char *rc)
{
    char l1[160], l2[160];
    if (x->off) { snprintf(l1, sizeof l1, "\tlui %s, %%hi(%s+%ld)", areg, x->label, x->off); snprintf(l2, sizeof l2, "\taddi %s, %s, %%lo(%s+%ld)", areg, areg, x->label, x->off); }
    else { snprintf(l1, sizeof l1, "\tlui %s, %%hi(%s)", areg, x->label); snprintf(l2, sizeof l2, "\taddi %s, %s, %%lo(%s)", areg, areg, x->label); }
    if (no >= 2 && !strcmp(out[no - 2], l1) && !strcmp(out[no - 1], l2)) {
        int after = !strcmp(vreg, areg) ? 1 : lr_after(e + 1, b, lr_reg(areg));
        if (after == 1 || (after == 2 && v)) no -= 2;
    }
    out[no++] = lr_line("\tadd %s, r0, %s", vreg, rc);
    return no;
}
/* before a marked store of vreg into item s: rc made what a load of the
 * item will give once it is stored -- the bytes stored, extended */
static int lr_put_copy(char **out, int no, const Sym *s, const char *rc, const char *vreg)
{
    int sg = s->pi.is_signed;
    if (s->size == 4) out[no++] = lr_line("\tadd %s, r0, %s", rc, vreg);
    else if (s->size == 1 && !sg) out[no++] = lr_line("\tandi %s, %s, 255", rc, vreg);
    else {
        out[no++] = lr_line("\tslli %s, %s, %d", rc, vreg, 32 - 8 * s->size);
        out[no++] = lr_line("\t%s %s, %s, %d", sg ? "srai" : "srli", rc, rc, 32 - 8 * s->size);
    }
    return no;
}

/* the region's lines [a, b) -- g_asm[a] its "#@R<", g_asm[b - 1] its
 * "#@R>" -- read and rewritten in place; returns how many lines longer
 * it is (shorter: negative).  used: the registers enclosing regions hold. */
static int lr_region(int a, int b, int used)
{
    LrItem it[LR_MAXITEM]; int nit = 0;
    for (int i = a; i < b; i++) {
        char kind, vreg[8], areg[8]; int sym, v; long off;
        if (!lr_mark(g_asm[i], &kind, &sym, &off, vreg, areg, &v)) continue;
        if (lr_find(it, nit, sym, off) >= 0 || nit == LR_MAXITEM || !lr_item(&it[nit], sym, off)) continue;
        nit++;
    }
    int n0 = b - a, delta = 0, mine = 0;
    if (nit) {
        unsigned char *ok = calloc((size_t)n0, 1);
        lr_scan(a, b, it, nit, ok, NULL);
        /* the items worth a register, most loads first: a load saved is
         * three instructions and more, a store costs one or two */
        for (;;) {
            int best = -1;
            for (int k = 0; k < nit; k++)
                if (it[k].reg < 0 && !it[k].conflict && it[k].nl > 0 && 3 * it[k].nl > 2 * it[k].ns && (best < 0 || it[k].nl > it[best].nl)) best = k;
            if (best < 0) break;
            int r; for (r = 0; r < LR_NREG && ((used | mine) & (1 << r)); r++) ;
            if (r == LR_NREG) break;
            it[best].reg = r; mine |= 1 << r;
        }
        if (getenv("S32_LR_TRACE"))
            for (int k = 0; k < nit; k++)
                fprintf(stderr, "loopreg: %s+%ld: %d loads, %d stores%s%s\n", it[k].label, it[k].off, it[k].nl, it[k].ns,
                        it[k].conflict ? ", refused" : "", it[k].reg >= 0 ? ", kept in a register" : "");
        if (mine) {
            char **out = xmalloc((size_t)(n0 + 12 * LR_NREG + 4 * n0) * sizeof *out); int no = 0;
            out[no++] = g_asm[a];
            /* where the loop is entered: each item into its register */
            for (int k = 0; k < nit; k++) {
                if (it[k].reg < 0) continue;
                int at = g_nasm;
                g_mark_off++;
                emit_la_off("r3", it[k].label, (int)it[k].off);
                emit_load_int_1(&g_sym[it[k].sym], "r3", lr_regname[it[k].reg]);
                g_mark_off--;
                for (int j = at; j < g_nasm; j++) out[no++] = g_asm[j];
                g_nasm = at;
            }
            for (int i = a + 1; i < b; i++) {
                char kind, vreg[8], areg[8]; int sym, v; long off;
                const char *l = g_asm[i];
                int k = lr_mark(l, &kind, &sym, &off, vreg, areg, &v) ? lr_find(it, nit, sym, off) : -1;
                if (k < 0 || it[k].reg < 0 || !ok[i - a]) { out[no++] = g_asm[i]; continue; }
                const char *rc = lr_regname[it[k].reg];
                Sym *s = &g_sym[sym];
                int e = i + 1; while (e < b && strcmp(g_asm[e], "#@.")) e++;       /* the unit's end */
                if (kind == 'L') no = lr_put_load(out, no, &it[k], vreg, areg, v, e, b, rc);
                else {
                    no = lr_put_copy(out, no, s, rc, vreg);
                    for (int j = i + 1; j < e; j++) out[no++] = g_asm[j];
                }
                i = e;
            }
            /* the region is done: its marks become "a loop that owns these
             * registers", for lr_unit */
            out[0] = lr_line("#@K< %d", mine);
            out[no - 1] = lr_line("#@K>");
            delta = no - n0;
            int tail = g_nasm - b;
            while (g_nasm + delta + 1 > g_asmcap) { g_asmcap = g_asmcap ? g_asmcap * 2 : 4096; g_asm = realloc(g_asm, (size_t)g_asmcap * sizeof *g_asm); }
            memmove(g_asm + a + no, g_asm + b, (size_t)tail * sizeof *g_asm);
            memcpy(g_asm + a, out, (size_t)no * sizeof *out);
            g_nasm += delta;
            free(out);
            g_lr_used |= mine;
        }
        free(ok);
    }
    /* the regions inside it, with the registers that are left */
    int end = b + delta, depth = 0, start = -1;
    for (int i = a + 1; i < end - 1; i++) {
        if (!strcmp(g_asm[i], "#@R<")) { if (depth++ == 0) start = i; }
        else if (!strcmp(g_asm[i], "#@R>") && depth > 0 && --depth == 0) {
            int dd = lr_region(start, i + 1, used | mine);
            delta += dd; end += dd; i += dd;
        }
    }
    return delta;
}

/* the code from line `from` on is a whole in-line PERFORM, outermost: its
 * loops' regions, each with those inside it */
static void lr_run(int from)
{
    if (g_noemit || g_nohx || g_noloopreg) return;
    int depth = 0, start = -1;
    for (int i = from; i < g_nasm; i++) {
        if (!strcmp(g_asm[i], "#@R<")) { if (depth++ == 0) start = i; }
        else if (!strcmp(g_asm[i], "#@R>") && depth > 0 && --depth == 0) i += lr_region(start, i + 1, 0);
    }
}

/* A unit's code is all there (from its "#@P" on): what its registers hold
 * as the code is read, and the loads that can be copies.  Then the marks
 * go -- a unit that holds this one would otherwise read them as its own. */
static void lr_unit(void)
{
    char mark[24]; snprintf(mark, sizeof mark, "#@P %d", g_unit);
    int a = -1, b = g_nasm;
    for (int i = g_nasm - 1; i >= 0; i--) if (!strcmp(g_asm[i], mark)) { a = i + 1; break; }
    if (a < 0) return;
    int n0 = b - a, any = 0;
    for (int i = a; i < b && !any; i++) any = lr_is_mark(g_asm[i]);
    if (!any) return;
    char **out = xmalloc((size_t)(3 * n0 + 8) * sizeof *out); int no = 0, used = 0;
    if (!g_noavailreg && !g_noemit) {
        LrAvail av; memset(&av, 0, sizeof av);
        av.use = calloc((size_t)n0, 1); av.def = calloc((size_t)n0, 1); av.useful = calloc((size_t)n0, 1);
        lr_scan(a, b, NULL, 0, NULL, &av);
        for (int i = a; i < b; i++) {
            char kind, vreg[8], areg[8]; int sym, v; long off;
            LrItem x;
            int u = av.use[i - a], d = av.useful[i - a] ? av.def[i - a] : 0;
            if ((!u && !d) || !lr_mark(g_asm[i], &kind, &sym, &off, vreg, areg, &v) || !lr_item(&x, sym, off)) { out[no++] = g_asm[i]; continue; }
            int e = i + 1; while (e < b && strcmp(g_asm[e], "#@.")) e++;
            if (u) { no = lr_put_load(out, no, &x, vreg, areg, v, e, b, lr_regname[u - 1]); used |= 1 << (u - 1); }
            else {
                const char *rc = lr_regname[d - 1];
                used |= 1 << (d - 1);
                if (kind == 'S') no = lr_put_copy(out, no, &g_sym[sym], rc, vreg);
                for (int j = i + 1; j < e; j++) out[no++] = g_asm[j];
                if (kind == 'L') out[no++] = lr_line("\tadd %s, r0, %s", rc, vreg);       /* the value just loaded */
            }
            i = e;
        }
        free(av.use); free(av.def); free(av.useful);
    } else for (int i = a; i < b; i++) out[no++] = g_asm[i];
    /* the marks, read for the last time */
    int w = 0;
    for (int j = 0; j < no; j++) if (!lr_is_mark(out[j])) out[w++] = out[j];
    while (a + w + 1 > g_asmcap) { g_asmcap = g_asmcap ? g_asmcap * 2 : 4096; g_asm = realloc(g_asm, (size_t)g_asmcap * sizeof *g_asm); }
    memcpy(g_asm + a, out, (size_t)w * sizeof *out);
    g_nasm = a + w;
    free(out);
    g_lr_used |= used;
}

/* a loop's code begins (after its item is set) and ends */
static void lr_begin(void) { if (!g_nohx && !g_noloopreg) emit("#@R<"); }
static void lr_end(void) { if (!g_nohx && !g_noloopreg) emit("#@R>"); }

/* the unit's entry saves the registers its loops used: the lines go in
 * at the place the entry left for them, now that it is known which */
static void lr_unit_saves(void)
{
    int at = -1;
    char mark[24]; snprintf(mark, sizeof mark, "#@P %d", g_unit);
    for (int i = g_nasm - 1; i >= 0; i--) if (!strcmp(g_asm[i], mark)) { at = i; break; }
    if (at < 0 || !g_lr_used) return;
    int n = 0; char *ln[LR_NREG];
    for (int r = 0; r < LR_NREG; r++) if (g_lr_used & (1 << r)) ln[n++] = lr_line("\tstw sp+%d, %s", SLOT_LR(r), lr_regname[r]);
    while (g_nasm + n > g_asmcap) { g_asmcap = g_asmcap ? g_asmcap * 2 : 4096; g_asm = realloc(g_asm, (size_t)g_asmcap * sizeof *g_asm); }
    memmove(g_asm + at + n, g_asm + at + 1, (size_t)(g_nasm - at - 1) * sizeof *g_asm);
    memcpy(g_asm + at, ln, (size_t)n * sizeof *ln);
    g_nasm += n - 1;
}
static void lr_unit_restores(void)
{
    for (int r = LR_NREG - 1; r >= 0; r--) if (g_lr_used & (1 << r)) emit("\tldw %s, sp+%d", lr_regname[r], SLOT_LR(r));
}
