/* s32-cobc: emitter.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ====================================================================== */
/* Emitter                                                                 */
/* ====================================================================== */

static int g_nlabel;

/* label is heap-allocated, NOT an array in this struct.  lit_label hands
 * its pointer to callers who hold it while building an Arg list, and
 * g_lit is realloc'd -- an inline array would move out from under them.
 * See the comment on lit_label. */
typedef struct { char *label; unsigned char *bytes; int len; } Lit;
static Lit *g_lit; static int g_nlit, g_lcap;

/* descriptors: emitted into .rodata at the end */
typedef struct { unsigned char cat, usage, digits; signed char scale; unsigned char flags, flags2; int size; char picstr[PIC_MAXPAT]; int anylen; } Desc;
static Desc *g_desc; static int g_ndesc, g_dcap;

static int g_noemit;        /* >0 while a lookahead parse runs: no code */

/* The assembly is kept in memory until the end so that conditional
 * branches can be relaxed: a bcond reaches +/-4096 bytes and a big
 * program's IF or PERFORM body can be longer than that (gl008 was the
 * first).  Every instruction line the compiler writes is one 4-byte
 * instruction -- li and la are already spelled out -- so positions in
 * .text are exact, and a branch that cannot reach becomes its inverse
 * over a jal (+/-1 MB), iterated to a fixed point. */
static char **g_asm; static int g_nasm, g_asmcap;
static int new_label(void);

static void emit(const char *fmt, ...)
{
    if (g_noemit) return;
    char buf[4096];
    va_list ap;
    va_start(ap, fmt); vsnprintf(buf, sizeof buf, fmt, ap); va_end(ap);
    if (g_nasm == g_asmcap) { g_asmcap = g_asmcap ? g_asmcap * 2 : 4096; g_asm = realloc(g_asm, g_asmcap * sizeof *g_asm); }
    g_asm[g_nasm++] = xstrndup(buf, strlen(buf));
}

/* a label definition line: ".L12:", ".Lp0_3:\t# name", "ws0_1:\t# ..." */
static int line_label(const char *l, char *name, int cap)
{
    if (l[0] == '\t' || l[0] == ' ' || l[0] == '#' || !l[0]) return 0;
    const char *c = strchr(l, ':');
    if (!c || c - l >= cap) return 0;
    memcpy(name, l, (size_t)(c - l)); name[c - l] = 0;
    return 1;
}

/* a conditional branch line: "\tbeq r1, r0, .L12" -> op, operands, target */
static int line_branch(const char *l, char *op, char *ops, char *target)
{
    static const char *bops[] = { "beq", "bne", "blt", "bge", "bltu", "bgeu", NULL };
    if (l[0] != '\t' || l[1] != 'b') return 0;
    const char *sp = strchr(l, ' ');
    if (!sp || sp - l - 1 > 7) return 0;
    memcpy(op, l + 1, (size_t)(sp - l - 1)); op[sp - l - 1] = 0;
    int k; for (k = 0; bops[k] && strcmp(bops[k], op); k++) ;
    if (!bops[k]) return 0;
    const char *last = strrchr(sp, ',');
    if (!last) return 0;
    memcpy(ops, sp + 1, (size_t)(last - sp - 1)); ops[last - sp - 1] = 0;   /* "r1, r0" */
    while (*++last == ' ') ;
    snprintf(target, 64, "%s", last);
    for (char *e = target; *e; e++)                 /* a trailing comment or blank is not the label's name */
        if (*e == ' ' || *e == '\t' || *e == '#') { *e = 0; break; }
    return 1;
}

static const char *branch_inverse(const char *op)
{
    if (!strcmp(op, "beq")) return "bne";
    if (!strcmp(op, "bne")) return "beq";
    if (!strcmp(op, "blt")) return "bge";
    if (!strcmp(op, "bge")) return "blt";
    if (!strcmp(op, "bltu")) return "bgeu";
    return "bltu";
}

typedef struct { char *name; long pos; } LabelPos;

static int labelpos_cmp(const void *a, const void *b) { return strcmp(((const LabelPos *)a)->name, ((const LabelPos *)b)->name); }

static void relax_branches(void)
{
    unsigned char *islong = calloc((size_t)g_nasm, 1);
    long *pos = xmalloc((size_t)g_nasm * sizeof *pos);
    LabelPos *labels = xmalloc((size_t)g_nasm * sizeof *labels);
    char name[128], op[8], ops[64], target[64];
    /* islong only ever grows, so this terminates in at most one pass per
     * branch; a fixed cap left a long chain half-relaxed (GitHub #22) */
    for (;;) {
        /* positions: .text only; a label's position is the next instruction's */
        int in_text = 0, nl = 0; long at = 0;
        for (int i = 0; i < g_nasm; i++) {
            const char *l = g_asm[i];
            pos[i] = at;
            if (!strcmp(l, "\t.text")) { in_text = 1; continue; }
            if (!strcmp(l, "\t.data") || !strcmp(l, "\t.rodata") || !strncmp(l, "\t.section", 9)) { in_text = 0; continue; }
            if (!in_text) continue;
            if (line_label(l, name, sizeof name)) { labels[nl].name = xstrndup(name, strlen(name)); labels[nl].pos = at; nl++; continue; }
            if (l[0] != '\t') continue;
            if (l[1] == '.') { if (!strncmp(l, "\t.p2align", 9)) at += 12; continue; }   /* padding, over-estimated */
            at += islong[i] ? 8 : 4;
        }
        qsort(labels, (size_t)nl, sizeof *labels, labelpos_cmp);
        int changed = 0;
        for (int i = 0; i < g_nasm; i++) {
            if (islong[i] || !line_branch(g_asm[i], op, ops, target)) continue;
            LabelPos key = { target, 0 };
            LabelPos *lp = bsearch(&key, labels, (size_t)nl, sizeof *labels, labelpos_cmp);
            if (!lp) continue;                         /* a symbol elsewhere: leave it */
            long d = lp->pos - pos[i];
            if (d > 4000 || d < -4000) { islong[i] = 1; changed = 1; }
        }
        for (int k = 0; k < nl; k++) free(labels[k].name);
        if (!changed) break;
    }

    for (int i = 0; i < g_nasm; i++) {
        if (islong[i] && line_branch(g_asm[i], op, ops, target)) {
            int L = new_label();
            fprintf(g_out, "\t%s %s, .L%d\n\tjal r0, %s\n.L%d:\n", branch_inverse(op), ops, L, target, L);
        } else fprintf(g_out, "%s\n", g_asm[i]);
    }
    free(islong); free(pos); free(labels);
}

static int new_label(void) { return g_nlabel++; }

/* The returned pointer MUST outlive further calls to this function.
 *
 * Callers hold it: opnd_args stores it in an Arg, and a statement builds
 * several Args before emit_args consumes them -- parse_inspect_range does
 * exactly that, one pattern_args per BEFORE/AFTER phrase.  While the label
 * lived in an array inside g_lit, the second call could realloc the table
 * and leave the first caller's pointer dangling; the Arg then emitted
 * `%hi()` with no symbol, which the assembler resolves to address 0.
 *
 * That is CCVS NC122A: `REPLACING ALL "A" BY "E"` searched for whatever
 * byte sits at address 0 instead of "A", so nothing was replaced.  It
 * needs the table to cross a power of two between the two calls, which is
 * why it took a program with ~80 literals to show and why every small
 * reproduction of the statement looked fine.  #29 shape (2) did not create
 * it -- it changed how many literals a comparison emits, which moved the
 * boundary onto this pair.  It was latent for as long as Args have been
 * built before being emitted.
 *
 * So the label is allocated separately and never moves. */
static const char *lit_label(const unsigned char *bytes, int len)
{
    for (int i = 0; i < g_nlit; i++)
        if (g_lit[i].len == len && !memcmp(g_lit[i].bytes, bytes, len)) return g_lit[i].label;
    if (g_nlit == g_lcap) { g_lcap = g_lcap ? g_lcap * 2 : 32; g_lit = realloc(g_lit, g_lcap * sizeof *g_lit); }
    Lit *l = &g_lit[g_nlit++];
    char buf[32];
    snprintf(buf, sizeof buf, ".Lstr%d", g_nlit - 1);
    size_t n = strlen(buf) + 1;
    l->label = xmalloc(n); memcpy(l->label, buf, n);
    l->bytes = xmalloc(len); memcpy(l->bytes, bytes, len); l->len = len;
    return l->label;
}

static int desc_add(const Desc *d)
{
    for (int i = 0; i < g_ndesc; i++)
        if (!memcmp(&g_desc[i], d, sizeof *d)) return i;
    if (g_ndesc == g_dcap) { g_dcap = g_dcap ? g_dcap * 2 : 64; g_desc = realloc(g_desc, g_dcap * sizeof *g_desc); }
    g_desc[g_ndesc] = *d;
    return g_ndesc++;
}

static int sym_desc(Sym *s)
{
    if (s->desc_id >= 0) return s->desc_id;
    Desc d; memset(&d, 0, sizeof d);
    if (s->any_len) d.anylen = sym_idx(s) + 1;     /* its own, in .data: the size is the argument's, set at entry */
    if (s->natgroup) { d.cat = COB_NATIONAL; d.usage = COB_U_DISPLAY; }   /* treated as PIC N(m) (13.18.29.4 rule 2b) */
    else if (sym_bitlike(s)) {           /* not a group with a USAGE BIT clause: that one is alphanumeric (13.18.60; B5) */
        /* bits: size the boolean positions, scale the first bit's place */
        d.cat = COB_BOOLEAN; d.usage = COB_U_BIT; d.size = s->bits; d.scale = (signed char)s->bitoff;
        s->desc_id = desc_add(&d);
        return s->desc_id;
    }
    else if (s->is_group) { d.cat = COB_GROUP; d.usage = COB_U_DISPLAY; }
    else {
        switch (s->pi.category) {
        case PIC_ALPHABETIC: d.cat = COB_ALPHA; break;
        case PIC_ALPHANUMERIC: d.cat = COB_ALNUM; break;
        case PIC_ALPHANUMERIC_EDITED: d.cat = COB_ALNUM_ED; break;
        case PIC_NUMERIC: d.cat = COB_NUM; break;
        case PIC_NATIONAL: d.cat = COB_NATIONAL; break;
        case PIC_BOOLEAN: d.cat = COB_BOOLEAN; break;
        default: d.cat = COB_NUM_ED; break;
        }
        switch (s->usage) {
        case U_DISPLAY: d.usage = COB_U_DISPLAY; break;
        case U_PACKED: d.usage = COB_U_PACKED; break;
        case U_NATIONAL: d.usage = COB_U_NATIONAL; break;
        case U_FLOAT: d.usage = COB_U_FLOAT; break;
        default: d.usage = COB_U_BINARY; break;
        }
        d.digits = (unsigned char)s->pi.digits; d.scale = (signed char)s->pi.scale;
        if (s->pi.is_signed) d.flags |= COB_F_SIGNED;
        if (s->usage == U_COMP5 || usage_is_native(s->usage)) d.flags |= COB_F_NOTRUNC;
        if (sym_be(s)) d.flags2 |= COB_F2_BIGEND;
        if (s->uvar == UV_NOSIGN) d.flags2 |= COB_F2_NOSIGN;
        if (s->uvar == UV_COMPX && !s->compx_x) d.flags2 |= COB_F2_SIZEDIG;   /* MF: the 9s decide a size error */
        if (s->just) d.flags |= COB_F_JUST;
        if (s->blank_zero) d.flags |= COB_F_BLANKZ;
        if (s->sign_sep) d.flags |= s->sign_lead ? COB_F_SEPLEAD : COB_F_SEPTRAIL;
        else if (s->sign_lead) d.flags |= COB_F_LEAD;
        if (s->pi.edited || strchr(s->pi.pat, 'P')) snprintf(d.picstr, sizeof d.picstr, "%s", s->pi.pat);   /* P: the runtime counts the stored digits */
    }
    d.size = s->size;
    s->desc_id = desc_add(&d);
    return s->desc_id;
}

/* the descriptor a MOVE stores through: a COMP-X item takes a negative
 * value in two's complement there (MF: "as if the item had been signed"),
 * where an arithmetic statement's unsigned receiver takes the magnitude */
static int move_desc(Sym *s)
{
    int id = sym_desc(s);
    if (s->uvar != UV_COMPX) return id;
    Desc d = g_desc[id]; d.flags2 |= COB_F2_TWOSC;
    return desc_add(&d);
}

/* a nonnumeric literal's descriptor */
static int str_desc(int len)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_ALNUM; d.usage = COB_U_DISPLAY; d.size = len;
    return desc_add(&d);
}

/* a national literal's descriptor: len bytes, len / 2 characters */
static int nat_desc(int len)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_NATIONAL; d.usage = COB_U_DISPLAY; d.size = len;
    return desc_add(&d);
}

/* the columns national text (len bytes of UTF-16BE) takes on a screen or
 * a report line: a grapheme cluster at a time, each its display width, a
 * mark with nothing to sit on one -- the runtime's nat_clusters, on the
 * shared model of common/s32utf.h (cobol ISSUES-92, -94) */
static int nat_lit_cols(const unsigned char *p, int len)
{
    s32u_clu st; memset(&st, 0, sizeof st);
    int w = 0, pend = 0, n = len / 2;
    for (int i = 0; i < n; ) {
        uint32_t cp;
        i += (int)s32u_u16_get(p, (size_t)n, (size_t)i, &cp);
        if (s32u_clu_step(&st, cp)) w += pend;
        pend = s32u_clu_lone(&st) ? 1 : s32u_clu_width(&st);
    }
    return w + pend;
}

/* a boolean literal's or part's descriptor: len boolean positions, DISPLAY */
static int bool_desc(int len)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_BOOLEAN; d.usage = COB_U_DISPLAY; d.size = len;
    return desc_add(&d);
}

/* national bytes (UTF-16BE) back to UTF-8, for DISPLAY of a literal */
static int utf16be_to_utf8(const unsigned char *p, int nbytes, char *out)
{
    int k = 0, n = nbytes / 2;
    for (int i = 0; i < n; ) {
        uint32_t cp;
        i += (int)s32u_u16_get(p, (size_t)n, (size_t)i, &cp);
        k += s32u_encode(cp, (unsigned char *)out + k);
    }
    return k;
}

/* an unsigned integer of n DISPLAY digits (a calendar function's result) */
static int num_desc(int digits)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_NUM; d.usage = COB_U_DISPLAY; d.digits = (unsigned char)digits; d.scale = 0; d.size = digits;
    return desc_add(&d);
}

/* an intrinsic's result: a sign and 18 DISPLAY digits at the given scale */
static int numfn_desc(int scale)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_NUM; d.usage = COB_U_DISPLAY; d.digits = 18; d.scale = (signed char)scale;
    d.flags = COB_F_SIGNED | COB_F_SEPLEAD; d.size = 19;
    return desc_add(&d);
}

/* a numeric literal: DISPLAY digits with a separate leading sign */
static const char *num_lit_label(const NumLit *n, int *desc)
{
    char img[40];
    img[0] = n->neg ? '-' : '+';
    memcpy(img + 1, n->digits, n->ndigits);
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_NUM; d.usage = COB_U_DISPLAY; d.digits = (unsigned char)n->ndigits;
    d.scale = (signed char)n->scale; d.flags = COB_F_SIGNED | COB_F_SEPLEAD; d.size = n->ndigits + 1;
    *desc = desc_add(&d);
    return lit_label((unsigned char *)img, n->ndigits + 1);
}

/* a numeric literal as a CALL argument: the callee reads it through its
 * own picture, so the bytes are the plain digits (a negative one zoned
 * in its last digit), as GnuCOBOL stores literals -- not the pool's
 * sign-led image the runtime's descriptors describe */
static const char *call_num_lit_label(const NumLit *n)
{
    char img[40];
    memcpy(img, n->digits, n->ndigits);
    if (n->neg && n->ndigits) img[n->ndigits - 1] = (char)('p' + (img[n->ndigits - 1] - '0'));
    return lit_label((unsigned char *)img, n->ndigits);
}

/* rd = address of sym+off */
static void emit_la_off(const char *rd, const char *sym, int off)
{
    if (off) { emit("\tlui %s, %%hi(%s+%d)", rd, sym, off); emit("\taddi %s, %s, %%lo(%s+%d)", rd, rd, sym, off); }
    else { emit("\tlui %s, %%hi(%s)", rd, sym); emit("\taddi %s, %s, %%lo(%s)", rd, rd, sym); }
}

static void emit_la(const char *rd, const char *sym) { emit_la_off(rd, sym, 0); }

static void emit_desc_addr(const char *rd, int desc)
{
    char b[32]; snprintf(b, sizeof b, ".Ld%d", desc);
    emit_la(rd, b);
}

/* rd = 32-bit constant */
static void emit_li(const char *rd, long v)
{
    if (v >= -2048 && v <= 2047) { emit("\taddi %s, r0, %ld", rd, v); return; }
    unsigned long u = (unsigned long)v;
    unsigned long hi = ((u + 0x800) >> 12) & 0xFFFFF;
    long lo = (long)(u & 0xFFF); if (lo >= 2048) lo -= 4096;
    emit("\tlui %s, %lu", rd, hi);
    if (lo) emit("\taddi %s, %s, %ld", rd, rd, lo);
}

/* the statement being emitted computes with more than 18 digits
 * (docs/wide.md phase 2): its stack operations go to the wide stack */
static int g_wide;
static int g_saw_wide;              /* an operand past 18 digits was met (set even under g_noemit) */
static int g_saw_float;             /* a floating-point item was pushed (likewise) */
static int g_nohx;                  /* -fno-hot-arith: the register paths and their peepholes off -- the code as before them (a differential's other side) */
static int g_fstmt;                 /* the wide statement computes in double: a float among its operands or
                                     * receivers, so every operand goes on the stack as a double (docs/usage.md) */
static const char *wide_fn(const char *fn)
{
    static const char *map[][2] = {
        { "cob_push", "cob_wpush" }, { "cob_push_lit", "cob_wpush_lit" }, { "cob_nadd", "cob_wadd" },
        { "cob_nsub", "cob_wsub" }, { "cob_nmul", "cob_wmul" }, { "cob_ndiv", "cob_wdiv" }, { "cob_nneg", "cob_wneg" }, { "cob_nabs", "cob_wabs" },
        { "cob_ntrunc", "cob_wtrunc" }, { "cob_npow", "cob_wpow" }, { "cob_ncmp", "cob_wcmp" },
        { "cob_top_store", "cob_wtop_store" }, { "cob_top_addto", "cob_wtop_addto" }, { "cob_top_subfrom", "cob_wtop_subfrom" },
        { "cob_drop", "cob_wdrop" }, { "cob_pop_int", "cob_wpop_int" }, { "cob_pop_pos", "cob_wpop_pos" }, { NULL, NULL } };
    for (int i = 0; map[i][0]; i++) if (!strcmp(fn, map[i][0])) {
        if (g_fstmt && !strcmp(map[i][1], "cob_wpush")) return "cob_fpush";
        if (g_fstmt && !strcmp(map[i][1], "cob_wpush_lit")) return "cob_fpush_lit";
        return map[i][1];
    }
    return fn;
}
static void emit_call(const char *fn) { if (g_wide) fn = wide_fn(fn); emit("\tjal r31, %s", fn); }
static void emit_jump(int label) { emit("\tjal r0, .L%d", label); }
static void emit_label(int label) { emit(".L%d:", label); }

/* A stretch of code taken out of the stream and put back elsewhere: a
 * statement's nested statements are parsed once, where they are written,
 * their code made there and cut out, and placed where the statement
 * wants them (docs/plans/frontend-pass.md, step 4).  Nothing reads the
 * stream back but the branch relaxation at the end, so a stretch moves
 * whole.  Under g_noemit a block is empty, as the code would have been. */
typedef struct { char **line; int n; } Block;
static int block_begin(void) { return g_nasm; }
static Block block_cut(int from)
{
    Block b; b.n = g_nasm - from;
    b.line = b.n ? xmalloc((size_t)b.n * sizeof *b.line) : NULL;
    if (b.n) memcpy(b.line, g_asm + from, (size_t)b.n * sizeof *b.line);
    g_nasm = from;
    return b;
}
/* is the block one unconditional jump to a label?  its target in t */
static int block_is_jump(const Block *b, char *t, int cap)
{
    if (b->n != 1 || strncmp(b->line[0], "\tjal r0, ", 9)) return 0;
    const char *x = b->line[0] + 9;
    if (!*x || strpbrk(x, " ,\t#") || (int)strlen(x) >= cap) return 0;
    snprintf(t, (size_t)cap, "%s", x);
    return 1;
}
/* the lines emitted since from that branch or jump to .L<L>: to target
 * instead (L is never defined) */
static void retarget(int from, int L, const char *target)
{
    char suf[24]; int ns = snprintf(suf, sizeof suf, " .L%d", L);
    for (int i = from; i < g_nasm; i++) {
        size_t n = strlen(g_asm[i]);
        if (n < (size_t)ns || strcmp(g_asm[i] + n - ns, suf)) continue;
        size_t keep = n - (size_t)ns + 1;           /* through the space */
        char *r = xmalloc(keep + strlen(target) + 1);
        memcpy(r, g_asm[i], keep); strcpy(r + keep, target);
        g_asm[i] = r;
    }
}
static void block_put(const Block *b)
{
    if (g_noemit) return;
    for (int i = 0; i < b->n; i++) {
        if (g_nasm == g_asmcap) { g_asmcap = g_asmcap ? g_asmcap * 2 : 4096; g_asm = realloc(g_asm, g_asmcap * sizeof *g_asm); }
        g_asm[g_nasm++] = b->line[i];
    }
}

static void emit_bytes(const unsigned char *b, int n)
{
    for (int i = 0; i < n; i += 16) {
        char line[128]; int k = snprintf(line, sizeof line, "\t.byte ");
        for (int j = i; j < n && j < i + 16; j++)
            k += snprintf(line + k, sizeof line - (size_t)k, "%s%d", j == i ? "" : ",", b[j]);
        emit("%s", line);
    }
}

/* frame: sp+0 lr, sp+4 r11, sp+8.. operand slots, three scratch words, the slots named below; r12/r13 at SLOT_R12/SLOT_R13 */
#define FRAME       120
static int g_frame = FRAME;
static Sym *g_prog_ret;
static int g_uses_rc;               /* this unit names RETURN-CODE: its CALLs set it, its exit returns it */             /* a program's PROCEDURE DIVISION RETURNING item (-std=2002) */         /* this unit's frame: FRAME and its BY VALUE parameters' storage */
#define SLOT_R12    92          /* the caller's r12 and r13: callee-saved in the C ABI, and */
#define SLOT_R13    112         /* the generated code uses both as scratch (cobol ISSUES-57) */
#define SLOT_COLL   96          /* the caller's collating table, when this unit sets its own */
#define SLOT_DP     100         /* the caller's decimal point, under DECIMAL-POINT IS COMMA */
#define SLOT_CUR    104         /* the caller's currency sign, under CURRENCY SIGN */
#define SLOT_PBASE  108         /* the caller's PERFORM frame base (cob_perform_enter) */
#define SLOT_ACT    84          /* this activation's saved words and LOCAL-STORAGE (cob_act_enter, -std=2002) */
#define SLOT_RET    88          /* a function's result: the caller's temporary (-std=2002) */
#define SLOT(i)     (8 + 4 * (i))
#define NSLOTS      16
#define SLOT_A      (8 + 4 * NSLOTS)
#define SLOT_B      (SLOT_A + 4)
#define SLOT_C      (SLOT_A + 8)
