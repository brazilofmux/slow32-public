/* s32-cobc: arithmetic.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- arithmetic ------------------------------------------------------- */

/* [NOT] [ON] SIZE ERROR follows the receivers; whether it is there
 * decides the store options, so look before emitting the stores */
static int at_size_error_clause(void)
{
    /* ON EXCEPTION after an arithmetic statement inside a CALL's clause
     * belongs to the CALL: only ON SIZE / SIZE is ours */
    if (at_word("size")) return 1;
    if (at_word("on")) return is_word(peek(1), "size");
    return at_word("not") && (is_word(peek(1), "size") || (is_word(peek(1), "on") && is_word(peek(2), "size")));
}

static void accept_size_error_words(void)
{
    accept_word("on"); expect_word("size"); expect_word("error");
}

/* [NOT] ON SIZE ERROR as a node (docs/plans/frontend-pass.md, step 4):
 * each phrase's statements parsed once into a Block, read with the rest
 * of the statement before its code, and laid out after the stores */
typedef struct { int size_err, has_on, has_not; Block on, not_on; } SizePh;
static void parse_size_phrases(SizePh *p, int size_err, const char *end_word)
{
    memset(p, 0, sizeof *p);
    p->size_err = size_err;
    if (size_err) {
        if (at_word("size") || (at_word("on") && is_word(peek(1), "size"))) {
            accept_size_error_words(); p->has_on = 1;
            int b0 = block_begin(); parse_statements(); p->on = block_cut(b0);
        }
        if (at_size_error_clause() && accept_word("not")) {
            accept_size_error_words(); p->has_not = 1;
            int b0 = block_begin(); parse_statements(); p->not_on = block_cut(b0);
        }
    }
    accept_word(end_word);
}
/* after the stores: branch on the accumulated status in SLOT_B */
static void emit_size_phrases(const SizePh *p)
{
    if (!p->size_err) return;
    Phrases ph; memset(&ph, 0, sizeof ph);
    ph.has_not = p->has_not; ph.not_on = p->not_on;
    if (p->has_on) { ph.has_on = 1; ph.on = p->on; }
    else if (ec_size_on()) {                    /* no ON SIZE ERROR: EC-SIZE, if checking is on (2023 14.7.5) */
        int b0 = block_begin(); emit_ec_size(); ph.on = block_cut(b0); ph.has_on = 1;
    }
    emit_phrases(&ph, SLOT_B, 0);
}
/* the phrases read and laid out at once, for a statement that is not a
 * node yet (CORRESPONDING) */
static void parse_size_error_clauses(int size_err, const char *end_word)
{
    SizePh p; parse_size_phrases(&p, size_err, end_word);
    emit_size_phrases(&p);
}

static void check_numeric_opnd(Opnd *o)
{
    if (o->kind == O_STR || o->kind == O_ALL) die_at(o->line, "an arithmetic operand must be numeric");
    if (o->kind == O_FIG && strncmp(o->tok->s, "zero", 4)) die_at(o->line, "an arithmetic operand must be numeric");
    if (o->kind == O_REF && o->ref.rm) die_at(o->line, "a reference-modified item is not numeric");
    if (o->kind == O_REF && !is_numeric_sym(o->ref.sym)) die_at(o->line, "'%s' is not numeric", o->ref.sym->name);
    if (o->kind == O_FUNC && !opnd_fn_numeric(o)) {
        char up[40]; snprintf(up, sizeof up, "%s", o->fname ? o->fname : "?");
        for (char *q = up; *q; q++) *q = (char)toupper((unsigned char)*q);
        die_at(o->line, "FUNCTION %s is %s function, not numeric; an arithmetic operand is numeric (%s)", up,
               o->fnat ? "a national" : o->fbool ? "a boolean" : "an alphanumeric", g_std < 2002 ? "X3.23a-1989 23" : "2023 15.2");
    }
}

/* push an operand onto the numeric stack */
/* 31 digits in arithmetic: phase 2 of docs/wide.md; refused until then,
 * never computed in 64 bits and truncated */
static int sym_wide(const Sym *s) { return !s->is_group && (s->pi.digits > 18 || (s->usage == U_BINARY && s->size > 8) || s->usage == U_FLOAT); }   /* a float computes on the wide stack, in double */
static void wide_arith_refuse(int line, const char *what)
{
    die_at(line, "arithmetic on %s of more than 18 digits is not implemented yet (COBOL 2002's 31 digits: docs/wide.md, phase 2)", what);
}
static const char *num_lit_label(const NumLit *n, int *desc);
static void emit_push(Opnd *o)
{
    int w = (o->kind == O_NUM && numlit_wide(&o->num)) || (o->kind == O_REF && !o->ref.rm && sym_wide(o->ref.sym));
    if (w || fn_inexact(o)) g_saw_wide = 1;
    if (o->kind == O_REF && o->ref.sym->usage == U_FLOAT) g_saw_float = 1;
    if (w && !g_wide && !g_noemit) wide_arith_refuse(o->line, o->kind == O_NUM ? "a literal" : "an item");
    if (g_wide && o->kind == O_NUM && numlit_wide(&o->num)) {
        int d; const char *l = num_lit_label(&o->num, &d);
        Arg a[2] = { arg_label(l), arg_desc(d) };
        emit_args(a, 2);
        emit_call("cob_push");                      /* cob_wpush, g_wide being set */
        return;
    }
    if (g_incompat_push) emit_incompat(o);
    if (o->kind == O_EXPR) die_at(o->line, "internal: expression pushed as an operand");
    if (o->kind == O_FUNC) {
        emit_fn_value(o);
        if (o->fwnum) {                             /* its descriptor from the run time */
            int slot = g_slot_base++;
            emit("\tstw sp+%d, r1", SLOT(slot));
            emit_li("r3", 3);
            emit_call("cob_fn_var_desc");
            emit("\tadd r4, r1, r0");
            emit("\tldw r3, sp+%d", SLOT(slot));
            g_slot_base--;
            emit_call("cob_push");
            return;
        }
        emit("\tadd r3, r1, r0");
        emit_desc_addr("r4", o->fn == -1 ? (o->fscale >= 0 ? numfn_desc(o->fscale) : str_desc(o->fsize))
                            : fn_is_numeric(o->fn) ? fn_num_desc(o) : str_desc(o->fsize));
        emit_call("cob_push");
        return;
    }
    if (o->kind == O_NUM || o->kind == O_FIG) {
        long long v = o->kind == O_NUM ? numlit_scaled(&o->num) : 0;
        int scale = o->kind == O_NUM ? o->num.scale : 0;
        emit_li("r3", (long)(int)(v & 0xFFFFFFFF));
        emit_li("r4", (long)(int)(v >> 32));
        emit_li("r5", scale);
        emit_call("cob_push_lit");
        return;
    }
    if (!g_wide && opnd_display_int(o)) {
        /* GitHub #29 shape (3): an unsigned DISPLAY integer reaches the
         * numeric stack through the same inline decode the compare path
         * uses, instead of cob_push -> cob_get_num's digit loop.  The value
         * is below 10^9 so the high word is zero and the scale is zero;
         * everything above this -- the 64-bit arithmetic, the receiver's
         * truncation, ROUNDED, ON SIZE ERROR -- is untouched, which is what
         * keeps this a decode change rather than an arithmetic one. */
        emit_display_value(&o->ref);
        emit("\tadd r3, r1, r0");
        emit_li("r4", 0);
        emit_li("r5", 0);
        emit_call("cob_push_lit");
        return;
    }
    Arg a[2] = { arg_ref(&o->ref), arg_desc(sym_desc(o->ref.sym)) };
    emit_args(a, 2);
    emit_call("cob_push");
}

/* store from the stack top; opts 1 = ROUNDED, 2 = size-error check.
 * With the check on, the status accumulates in SLOT_B. */
static void emit_top_op(Ref *r, const char *fn, int opts)
{
    if (!g_wide && !r->rm && sym_wide(r->sym)) wide_arith_refuse(r->line, "a receiver");
    Arg a[3] = { arg_ref(r), arg_desc(sym_desc(r->sym)), arg_imm(opts) };
    emit_args(a, 3);
    emit_call(fn);
    if (opts & 2) {
        emit("\tldw r2, sp+%d", SLOT_B);
        emit("\tor r2, r2, r1");
        emit("\tstw sp+%d, r2", SLOT_B);
    }
}

static int all_hot(Opnd *ops, int n)
{
    for (int i = 0; i < n; i++) if (!opnd_hot_int(&ops[i])) return 0;
    return 1;
}

/* May this receiver take the hot path's word?  A four-byte unsigned NOTRUNC
 * item uses its top bit for value, so the sign fixup in emit_store_receivers
 * cannot tell 4000000000 from -294967296 -- it negated the former, and
 * "ADD 2000000000 TO" a PIC 9(9) COMP-5 holding 2000000000 stored 294967296
 * (GitHub #28).  It stays hot only where the result cannot be negative, an
 * ADD of non-negative operands; a SUBTRACT, or an operand that may be
 * negative, takes the generic path, where cob_put_num_x has all 64 bits and
 * the 85 rule (an unsigned receiver takes the magnitude) is decidable.  The
 * narrower COMP-5 items keep their sign in a word and need none of this. */
static int ref_hot_store(Ref *r, int subtract, int nonneg)
{
    Sym *s = r->sym;
    if (!is_hot_int(s) && !is_display_int(s)) return 0;
    if (!s->pi.is_signed && s->size == 4 && sym_notrunc(s) && (subtract || !nonneg)) return 0;
    return 1;
}

static int refs_hot(Ref *rs, int n, int subtract, int nonneg)
{
    if (refs_pending(rs, n)) return 0;          /* a receiver's call is made as it is stored (recv_calls) */
    for (int i = 0; i < n; i++) if (!ref_hot_store(&rs[i], subtract, nonneg)) return 0;
    return 1;
}

/* 32-bit hot-path sum stays correct only if every partial sum fits a
 * signed word. S9(9) COMP with three max operands does not (GitHub #18). */
static long long hot_opnd_mag(Opnd *o)
{
    if (o->kind == O_NUM) {
        long long v = numlit_int(&o->num);
        return v < 0 ? -v : v;
    }
    if (o->kind == O_FIG) return 0;
    if (o->kind == O_REF) {
        int d = o->ref.sym->pi.digits;
        if (d > 0 && d < 10) return pow10l(d) - 1;
        if (o->ref.sym->size == 1) return o->ref.sym->pi.is_signed ? 127 : 255;
        if (o->ref.sym->size == 2) return o->ref.sym->pi.is_signed ? 32767 : 65535;
        return 2147483647;
    }
    return 2147483647;
}

/* The staged sum's magnitude bound, or -1 when there is not a sound one.
 *
 * hot_opnd_mag bounds an item by its PICTURE, which a COMP-5 or C-ABI item
 * does not obey -- it keeps the binary field's whole capacity.  hot_sum_fits
 * has always taken that bound at face value; this does not, because the
 * truncation it feeds would then wrap by a compare and a subtract where the
 * value needs a REM.  One NOTRUNC operand and the bound is unknown. */
static long long ops_sum_mag(Opnd *ops, int n)
{
    long long bound = 0;
    for (int i = 0; i < n; i++) {
        if (ops[i].kind == O_REF && sym_notrunc(ops[i].ref.sym)) return -1;
        bound += hot_opnd_mag(&ops[i]);
        if (bound > 2147483647LL) return -1;
    }
    return bound;
}

static int ops_all_nonneg(Opnd *ops, int n)
{
    for (int i = 0; i < n; i++) if (!opnd_nonneg(&ops[i])) return 0;
    return 1;
}

static int hot_sum_fits(Opnd *ops, int n)
{
    long long bound = 0;
    int i;
    for (i = 0; i < n; i++) {
        bound += hot_opnd_mag(&ops[i]);
        if (bound > 2147483647LL) return 0;
    }
    return 1;
}

/* SLOT_A = sum of the operands (hot path) */
static void emit_hot_sum(Opnd *ops, int n)
{
    for (int i = 0; i < n; i++) {
        emit_hot_value(&ops[i]);
        if (i) { emit("\tldw r2, sp+%d", SLOT_A); emit("\tadd r1, r1, r2"); }
        emit("\tstw sp+%d, r1", SLOT_A);
    }
}

#define MAXOPS 64                  /* NC106A/NC176A add and subtract 21 operands */

static int parse_operand_list(Opnd *ops, int max)
{
    int n = 0;
    while (at_operand() || (cur()->kind == T_WORD && is_figurative(cur()->s))) {
        if (n >= max) die_at(cur()->line, "too many operands");
        parse_operand(&ops[n]); check_numeric_opnd(&ops[n]); n++;
    }
    return n;
}

/* receivers, each with an optional ROUNDED; GIVING and COMPUTE receivers
 * may be numeric-edited */
static int g_rmode;                     /* this statement has a ROUNDED MODE other than the default: the stack path */
/* after ROUNDED: [MODE IS mode] (2014; 2023 14.7.4) -- 1 for plain
 * ROUNDED and NEAREST-AWAY-FROM-ZERO (the DEFAULT ROUNDED clause's
 * default, 11.9.6), 0 for TRUNCATION (as no ROUNDED, general rule 2), and
 * any other mode in bits 4-7 */
static int parse_rounded_mode(void)
{
    if (!at_word("mode")) return 1;
    int line = cur()->line;
    advance(); accept_word("is");
    static const char *modes[] = { "away-from-zero", "nearest-away-from-zero", "nearest-even", "nearest-toward-zero",
                                   "prohibited", "toward-greater", "toward-lesser", "truncation", NULL };
    int m = 0;
    for (int k = 0; modes[k]; k++) if (at_word(modes[k])) m = k + 1;
    if (!m) die_at(cur()->line, "expected a rounding mode after ROUNDED MODE IS, found %s (2023 14.7.4.2)", tok_desc(cur()));
    advance();
    bp(BP_E29_ROUNDED_MODE, line);
    if (m == 2) return 1;
    if (m == 8) return 0;
    g_rmode = 1;
    return m << 4;
}

static int parse_ref_list(Ref *rs, int *rounded, int max, int edited_ok)
{
    int n = 0;
    while (at_operand()) {
        if (n >= max) die_at(cur()->line, "too many receiving items");
        parse_ref(&rs[n]);
        Sym *d = rs[n].sym;
        if (rs[n].rm) die_at(rs[n].line, "a reference-modified item cannot be an arithmetic receiver");
        if (d->is_group || (d->pi.category != PIC_NUMERIC && !(edited_ok && d->pi.category == PIC_NUMERIC_EDITED) &&
                            !(edited_ok == 2 && sym_is_boolean(d))))    /* 2: COMPUTE, whose format 2 stores a boolean */
            die_at(rs[n].line, "'%s' is not numeric", d->name);
        rounded[n] = 0;
        if (accept_word("rounded")) rounded[n] = parse_rounded_mode();
        n++;
    }
    return n;
}

static int any_rounded(const int *r, int n) { for (int i = 0; i < n; i++) if (r[i]) return 1; return 0; }
/* a receiver's rounding as the runtime's opts: 1 plain ROUNDED (also
 * NEAREST-AWAY-FROM-ZERO), a mode in bits 4-7 (libcob rmode_round) */
static int rnd_opts(int r) { return r == 1 ? 1 : (r & 0xF0); }

/* store the sum on the stack top (general) or in SLOT_A (hot) to receivers */
/* sum_mag bounds |the staged sum| (-1: unknown) and sum_nonneg says it cannot
 * be negative; together with the receiver's own picture they bound the value
 * being stored, which is what lets the truncation and the sign fixup go. */
/* the hot sum is a literal that fits an addi: the store adds it as an
 * immediate, no SLOT_A (every PERFORM VARYING and SEARCH step is ADD 1) */
static int g_addk_on; static long g_addk;
static void recv_access(Ref *r, int reads);
static void emit_store_receivers(Ref *rs, int *rounded, int nr, int hot, int giving, int subtract, int size_err,
                                 long long sum_mag, int sum_nonneg)
{
    for (int i = 0; i < nr; i++) if (!g_wide && !rs[i].rm && sym_wide(rs[i].sym)) wide_arith_refuse(rs[i].line, "a receiver");
    if (size_err) emit("\tstw sp+%d, r0", SLOT_B);
    for (int i = 0; i < nr; i++) {
        int opts = rnd_opts(rounded[i]) | (size_err ? 2 : 0);
        recv_access(&rs[i], !giving);           /* identified as it is accessed */
        if (hot) {
            Sym *d = rs[i].sym;
            emit_ref_addr(&rs[i], "r3");
            if (giving) emit("\tldw r1, sp+%d", SLOT_A);
            else if (g_addk_on) {
                emit_load_int(d, "r3", "r1");
                emit("\taddi r1, r1, %ld", subtract ? -g_addk : g_addk);
            } else {
                emit_load_int(d, "r3", "r1");
                emit("\tldw r2, sp+%d", SLOT_A);
                emit(subtract ? "\tsub r1, r1, r2" : "\tadd r1, r1, r2");
            }
            /* An unsigned COMP receiver holds 0 .. 10^digits-1: every path
             * that stores one truncates, so adding a bounded non-negative
             * sum to it lands below twice the limit and cannot go negative. */
            long long bound = -1; int nonneg = 0;
            if (!subtract && !d->pi.is_signed && d->size == 4 && sym_notrunc(d)) {
                /* the whole word is value (ref_hot_store admitted it only
                 * with non-negative operands): no picture, no sign fixup */
                nonneg = sum_nonneg;
            } else if (sum_mag >= 0 && !subtract) {
                if (giving) { bound = sum_mag; nonneg = sum_nonneg; }
                else if (!d->pi.is_signed && (d->usage == U_BINARY || is_display_int(d)) &&
                         d->pi.digits > 0 && d->pi.digits < 19) {
                    bound = pow10l(d->pi.digits) - 1 + sum_mag; nonneg = sum_nonneg;
                }
            }
            emit_trunc_bounded(d, bound, nonneg);
            if (!d->pi.is_signed && !nonneg) {
                /* unsigned takes the magnitude, matching cob_put_num_x */
                int Lpos = new_label();
                emit("\tbge r1, r0, .L%d", Lpos);
                emit("\tsub r1, r0, r1");
                emit_label(Lpos);
            }
            emit_store_int(d, "r3", "r1");
        } else {
            emit_top_op(&rs[i], giving ? "cob_top_store" : subtract ? "cob_top_subfrom" : "cob_top_addto", opts);
        }
    }
    if (!hot) emit_call("cob_drop");
}

/* ---- the scaled ADD, in line ----------------------------------------------
 *
 * After #27, #29's three shapes and #30, the batch's largest remaining
 * runtime line item was cob_top_addto: 208,889 calls, every one of them a
 * COMP-3 receiver of eleven digits at scale 2 taking a same-scale operand,
 * DISPLAY 9(9)V99 or the same COMP-3 picture (ws-debits, ws-total-debits,
 * yt-debits(i)), at ~900 instructions each -- two cob_get_num, a 64-bit
 * alignment, cob_put_num_x with its digit loop.  ~12% of the batch.
 *
 * With the scales equal there is nothing to align: the two digit strings
 * add column-wise.  Each item is read into two limbs in base 10^9 -- hi for
 * the digits above the low nine, lo for the low nine -- and a sign; the
 * limbs add or subtract as sign-magnitude with one carry or borrow between
 * them; the result is brought inside the receiver's picture by one REM on
 * the limb the picture ends in; and it is written back as digits or
 * nibbles.  No call, no descriptor, no 64-bit arithmetic: everything fits a
 * word because a limb is below 10^9 and a sum of two is below 2^31.
 * Eighteen digits is the ceiling, two limbs.
 *
 * What it takes: ADD x TO r and SUBTRACT x FROM r, one operand, one or more
 * receivers, both DISPLAY (digits exactly the bytes, a trailing overpunch
 * sign or none) or COMP-3, the operand's scale the receiver's.  ROUNDED is
 * admitted because with one scale it has nothing to do.  SIZE ERROR is
 * not: that needs the overflow detected, and the generic path has it.
 * Literals and GIVING stay generic too -- the batch had 12k stores against
 * 209k adds, and a literal is a different decode.  The 85 rule for an
 * unsigned receiver, the magnitude, is kept; a zero result is positive.
 *
 * Registers: the values live in r5-r10 across the sequence, which holds no
 * call -- so a subscript that needs cob_load_int (r3-r10 clobbered) bars
 * the path.  r1/r2 scratch, r4 a constant, r3 the address, r11 untouched
 * (the subscript accumulator: see emit_display_decode).  GitHub #29. */

/* an item the inline decimal add can read and write */
static int sym_dec_ok(Sym *s)
{
    if (s->is_group || s->is_cond || s->pi.category != PIC_NUMERIC) return 0;
    if (s->sign_sep || s->sign_lead || s->blank_zero || s->pi.edited || strchr(s->pi.pat, 'P')) return 0;
    if (s->pi.digits < 1 || s->pi.digits > 18) return 0;
    if (s->usage == U_DISPLAY) return (int)s->size == s->pi.digits;
    if (s->usage == U_PACKED) return s->uvar == UV_NONE && (int)s->size == (s->pi.digits + 2) / 2;
    return 0;
}

/* a reference whose address the sequence can form without a call */
static int ref_dec_addr_ok(const Ref *r)
{
    if (r->rm) return 0;
    for (int i = 0; i < r->nsub; i++) if (r->sub[i].sym && !is_hot_int(r->sub[i].sym)) return 0;
    return 1;
}

/* acc = acc * 10 with r2 scratch and no constant register */
static void emit_mul10(const char *acc)
{
    emit("\tslli r2, %s, 3", acc);
    emit("\tslli %s, %s, 1", acc, acc);
    emit("\tadd %s, %s, r2", acc, acc);
}

/* item s at areg -> limbs hi (digits above the low nine) and lo (the low
 * nine), sg = 1 if negative.  Digit d of D (0 the most significant) goes
 * to hi while d < D - 9. */
static void emit_dec_load(Sym *s, const char *areg, const char *hi, const char *lo, const char *sg)
{
    int D = s->pi.digits, split = D > 9 ? D - 9 : 0;
    emit("\tadd %s, r0, r0", hi);
    emit("\tadd %s, r0, r0", lo);
    if (s->usage == U_DISPLAY) {
        for (int d = 0; d < D; d++) {
            const char *acc = d < split ? hi : lo;
            if (d != 0 && d != split) emit_mul10(acc);
            emit("\tldbu r2, %s+%d", areg, d);
            emit("\tandi r2, r2, 15");            /* '0'..'9' and the overpunch 'p'..'y' alike */
            emit("\tadd %s, %s, r2", acc, acc);
        }
        if (s->pi.is_signed) {
            emit("\tldbu r2, %s+%d", areg, D - 1);
            emit("\tsltiu %s, r2, 112", sg);      /* below 'p': positive */
            emit("\txori %s, %s, 1", sg, sg);
        } else emit("\tadd %s, r0, r0", sg);
    } else {
        /* 2*size nibbles: a zero pad first when D is even, the D digits,
         * the sign last */
        int k0 = 2 * (int)s->size - 1 - D, curbyte = -1;
        for (int d = 0; d < D; d++) {
            int k = k0 + d, b = k / 2;
            const char *acc = d < split ? hi : lo;
            if (b != curbyte) { emit("\tldbu r1, %s+%d", areg, b); curbyte = b; }
            if (d != 0 && d != split) emit_mul10(acc);
            if (k % 2 == 0) emit("\tsrli r2, r1, 4"); else emit("\tandi r2, r1, 15");
            emit("\tadd %s, %s, r2", acc, acc);
        }
        if (s->pi.is_signed) {
            if (curbyte != (int)s->size - 1) emit("\tldbu r1, %s+%d", areg, (int)s->size - 1);
            emit("\tandi r2, r1, 15");
            emit("\txori r2, r2, 13");            /* 0xD: negative; C, F or anything else: not */
            emit("\tseq %s, r2, r0", sg);
        } else emit("\tadd %s, r0, r0", sg);
    }
}

/* (hi,lo,sg) += (oh,ol,os), sign-magnitude in limbs of base 10^9 */
static void emit_dec_add(const char *hi, const char *lo, const char *sg, const char *oh, const char *ol, const char *os)
{
    int Lsame = new_label(), Lless = new_label(), Lsub = new_label(), Ldone = new_label(), Lnz = new_label();
    emit_li("r4", 1000000000);
    emit("\tbeq %s, %s, .L%d", sg, os, Lsame);
    emit("\tbltu %s, %s, .L%d", hi, oh, Lless);
    emit("\tbne %s, %s, .L%d", hi, oh, Lsub);
    emit("\tbltu %s, %s, .L%d", lo, ol, Lless);
    emit_label(Lsub);                              /* |x| >= |y|: x - y, x's sign */
    emit("\tsltu r2, %s, %s", lo, ol);
    emit("\tsub %s, %s, %s", lo, lo, ol);
    emit("\tsub %s, %s, %s", hi, hi, oh);
    emit("\tsub %s, %s, r2", hi, hi);
    emit("\tbeq r2, r0, .L%d", Ldone);
    emit("\tadd %s, %s, r4", lo, lo);
    emit("\tjal r0, .L%d", Ldone);
    emit_label(Lless);                             /* |y| > |x|: y - x, y's sign */
    emit("\tsltu r2, %s, %s", ol, lo);
    emit("\tsub %s, %s, %s", lo, ol, lo);
    emit("\tsub %s, %s, %s", hi, oh, hi);
    emit("\tsub %s, %s, r2", hi, hi);
    emit("\tadd %s, %s, r0", sg, os);
    emit("\tbeq r2, r0, .L%d", Ldone);
    emit("\tadd %s, %s, r4", lo, lo);
    emit("\tjal r0, .L%d", Ldone);
    emit_label(Lsame);                             /* one sign: x + y */
    emit("\tadd %s, %s, %s", lo, lo, ol);
    emit("\tadd %s, %s, %s", hi, hi, oh);
    emit("\tbltu %s, r4, .L%d", lo, Ldone);
    emit("\tsub %s, %s, r4", lo, lo);
    emit("\taddi %s, %s, 1", hi, hi);
    emit_label(Ldone);
    emit("\tadd r2, %s, %s", hi, lo);              /* zero is positive */
    emit("\tbne r2, r0, .L%d", Lnz);
    emit("\tadd %s, r0, r0", sg);
    emit_label(Lnz);
}

/* bring (hi,lo) inside s's picture: the high-order digits past it go */
static void emit_dec_trunc(Sym *s, const char *hi, const char *lo)
{
    int D = s->pi.digits;
    if (D > 9) { emit_li("r2", pow10l(D - 9)); emit("\trem %s, %s, r2", hi, hi); }
    else { emit_li("r2", pow10l(D)); emit("\trem %s, %s, r2", lo, lo); emit("\tadd %s, r0, r0", hi); }
}

/* (hi,lo,sg), already inside the picture -> item s at areg */
static void emit_dec_store(Sym *s, const char *areg, const char *hi, const char *lo, const char *sg)
{
    int D = s->pi.digits, split = D > 9 ? D - 9 : 0, d = D - 1;
    emit_li("r4", 10);
    /* the next digit, least significant first, into reg; the limb is
     * divided down unless this was its last digit */
#define DEC_DIGIT(reg) do { \
        const char *src_ = d < split ? hi : lo; \
        emit("\trem %s, %s, r4", reg, src_); \
        if (d != split && d != 0) emit("\tdiv %s, %s, r4", src_, src_); \
        d--; \
    } while (0)
    if (s->usage == U_DISPLAY) {
        while (d >= 0) {
            int at = d;
            DEC_DIGIT("r2");
            emit("\taddi r2, r2, 48");
            emit("\tstb %s+%d, r2", areg, at);
        }
        if (s->pi.is_signed) {
            int L = new_label();
            emit("\tbeq %s, r0, .L%d", sg, L);
            emit("\tldbu r2, %s+%d", areg, D - 1);
            emit("\taddi r2, r2, 64");             /* '0'..'9' -> 'p'..'y' */
            emit("\tstb %s+%d, r2", areg, D - 1);
            emit_label(L);
        }
    } else {
        for (int b = (int)s->size - 1; b >= 0; b--) {
            if (b == (int)s->size - 1) {
                if (s->pi.is_signed) emit("\taddi r2, %s, 12", sg);   /* C, or D when negative */
                else emit("\taddi r2, r0, 15");
            } else DEC_DIGIT("r2");
            if (d >= 0) { DEC_DIGIT("r1"); emit("\tslli r1, r1, 4"); emit("\tadd r2, r2, r1"); }
            emit("\tstb %s+%d, r2", areg, b);
        }
    }
#undef DEC_DIGIT
}

static int dec_add_ok(Opnd *ops, int n, Ref *rs, int nr, int size_err)
{
    if (size_err || n != 1 || ops[0].kind != O_REF || ops[0].all_sub) return 0;
    if (refs_pending(rs, nr)) return 0;
    if (!sym_dec_ok(ops[0].ref.sym) || !ref_dec_addr_ok(&ops[0].ref)) return 0;
    for (int i = 0; i < nr; i++) {
        if (!sym_dec_ok(rs[i].sym) || !ref_dec_addr_ok(&rs[i])) return 0;
        if (rs[i].sym->pi.scale != ops[0].ref.sym->pi.scale) return 0;
    }
    return 1;
}

static void emit_dec_addto(Opnd *op, Ref *rs, int nr, int subtract)
{
    for (int i = 0; i < nr; i++) {
        Sym *d = rs[i].sym;
        emit_ref_addr(&rs[i], "r3");
        emit("\tstw sp+%d, r3", SLOT_A);
        emit_ref_addr(&op->ref, "r3");
        emit_dec_load(op->ref.sym, "r3", "r8", "r9", "r10");
        if (subtract) emit("\txori r10, r10, 1");
        emit("\tldw r3, sp+%d", SLOT_A);
        emit_dec_load(d, "r3", "r5", "r6", "r7");
        emit_dec_add("r5", "r6", "r7", "r8", "r9", "r10");
        emit_dec_trunc(d, "r5", "r6");
        if (!d->pi.is_signed) emit("\tadd r7, r0, r0");   /* an unsigned receiver takes the magnitude */
        emit_dec_store(d, "r3", "r5", "r6", "r7");
    }
}

/* EC-DATA-INCOMPATIBLE (2023 14.6.13.2 rules 1-2): a numeric sending item
 * whose content would fail a NUMERIC class test, referenced while the
 * condition is checked.  Binary items are always valid; DISPLAY, packed
 * and national numeric ones are tested before the statement uses them.
 * Nothing is emitted unless the checking is on. */
static void emit_incompat(const Opnd *o)
{
    if (o->kind != O_REF || o->ref.rm || !ec_on_name("EC-DATA-INCOMPATIBLE")) return;
    Sym *x = o->ref.sym;
    /* rule 1: a boolean item of usage display or national whose content
     * fails the BOOLEAN class test (a USAGE BIT item is always valid) */
    int boolean = x->pi.category == PIC_BOOLEAN;
    if (x->is_group || (x->pi.category != PIC_NUMERIC && !boolean)) return;
    if (x->usage != U_DISPLAY && x->usage != U_NATIONAL && (boolean || x->usage != U_PACKED)) return;
    Arg a[3] = { arg_ref(&o->ref), arg_desc(sym_desc(x)), arg_imm(boolean ? 4 : 0) };
    emit_args(a, 3);
    emit_call("cob_class");
    int Lok = new_label();
    emit("\tbne r1, r0, .L%d", Lok);
    emit_ec_raise(ec_find("EC-DATA-INCOMPATIBLE", 0));
    emit_label(Lok);
}
static void emit_incompat_sym(Sym *s, int line)
{
    Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref.sym = s; o.ref.line = line; o.line = line; emit_incompat(&o);
}
static void emit_incompat_ref(const Ref *r)
{
    Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = *r; o.line = r->line; emit_incompat(&o);
}
/* the receivers that are summed too, checked before the arithmetic -- but
 * one whose subscript still has a call to make is identified only as it
 * is accessed (recv_calls), so its check waits for that (recv_access) */
static void emit_incompat_refs(const Ref *rs, int nr)
{
    for (int k = 0; k < nr; k++) if (!ref_pending(&rs[k])) emit_incompat_ref(&rs[k]);
}
/* a receiver about to be accessed: its calls made, and, when its content
 * is read too (ADD a TO b), the incompatible-data check that waited */
static void recv_access(Ref *r, int reads)
{
    if (!ref_pending(r)) return;
    recv_calls(r);
    if (reads) emit_incompat_ref(r);
}

/* the composite of operands (X3.23-1985 6.4.4 rule 2; 2023 14.7.7 rule
 * 2): the operands superimposed on their decimal points -- the widest
 * integer part and the widest fraction -- at most 18 digits in 1985, 31
 * in 2002, and 18 is what this compiler's arithmetic holds.  Which
 * operands count is each statement's rule; COMPUTE has none. */
static void opnd_int_frac(const Opnd *o, int *in, int *fr)
{
    int digits = -1, scale = 0;
    if (o->kind == O_REF && o->ref.sym->usage == U_FLOAT) return;       /* no digits: a float is computed in double */
    if (o->kind == O_REF && !o->ref.sym->is_group && (o->ref.sym->pi.category == PIC_NUMERIC || o->ref.sym->pi.category == PIC_NUMERIC_EDITED))
        { digits = o->ref.sym->pi.digits; scale = o->ref.sym->pi.scale; }
    else if (o->kind == O_NUM) { digits = o->num.ndigits; scale = o->num.scale; }
    if (digits < 0) return;
    int f = scale > 0 ? scale : 0, i = digits - f;
    if (i < 0) i = 0;
    if (i > *in) *in = i;
    if (f > *fr) *fr = f;
}
/* can a MULTIPLY's product of a and b pass 18 digits?  Then it goes to
 * the wide stack: the narrow cob_nmul sheds fraction digits to fit 64
 * bits, which an expression's intermediate may do (its precision is the
 * implementor's) but a MULTIPLY's result may not -- SV9(5) x 9(12)V9(4)
 * gave 20074490570.06 for 20092543169.49 (tests/gen found it;
 * tests/free/mulwide) */
static int prod_wide(const Opnd *a, const Opnd *b)
{
    int ai = 0, af = 0, bi = 0, bf = 0;
    opnd_int_frac(a, &ai, &af); opnd_int_frac(b, &bi, &bf);
    return ai + af + bi + bf > 18;
}
/* does a DIVIDE need the wide stack to round?  The narrow division
 * develops 18 significant digits, all an 18-digit receiver keeps, but
 * ROUNDED needs the digit after them: 73844192123.9531 / 0.561 into
 * 9(12)V9(6) ROUNDED is ...978.526025, and gave ...978.526024 (tests/gen
 * found it; tests/free/divround18) */
static int round_wide(const Ref *rs, const int *rd, int nr)
{
    for (int i = 0; i < nr; i++) if (rd[i] && !rs[i].sym->is_group && rs[i].sym->pi.digits >= 18) return 1;
    return 0;
}
static int arith_composite(const Opnd *ops, int n, const Ref *rs, int nr, const char *stmt, const char *rule85, int line)
{
    int in = 0, fr = 0;
    for (int k = 0; k < n; k++) opnd_int_frac(&ops[k], &in, &fr);
    for (int k = 0; k < nr; k++) { Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = rs[k]; opnd_int_frac(&o, &in, &fr); }
    int c = in + fr;
    if (c <= 18) return c;
    /* past 31: no edition allows it.  19-31: 2002's, the 31-digit gap
     * here; under -std=85 forbidden, yet majesty's dist01 has a 19-digit
     * SUBTRACT whose values fit, so it is taken, and a strict build is
     * told (BP-E14, -warn-extensions) */
    if (c > 31)
        die_at(line, "%s: the composite of operands is %d digits, more than %s allows (%s)", stmt, c,
               g_std < 2002 ? "COBOL 85's 18, or any edition's 31," : "31", g_std < 2002 ? rule85 : "2023 14.7.7 rule 2");
    if (g_std < 2002) bp(BP_E14_COMPOSITE, line);
    return c;
}
/* does an arithmetic statement take the wide path?  An operand, literal
 * or receiver past 18 digits, or -- 2002's 31 -- a composite past 18 */
static int opnds_wide(const Opnd *ops, int n)
{
    for (int k = 0; k < n; k++)
        if ((ops[k].kind == O_REF && ops[k].ref.sym->usage == U_FLOAT) || (ops[k].kind == O_EXPR && ops[k].flt)) g_fstmt = 1;
    for (int k = 0; k < n; k++)
        if ((ops[k].kind == O_REF && !ops[k].ref.rm && sym_wide(ops[k].ref.sym)) || (ops[k].kind == O_NUM && numlit_wide(&ops[k].num)) ||
            (ops[k].kind == O_EXPR && ops[k].wide) || fn_inexact(&ops[k])) return 1;
    return 0;
}
static int refs_wide(const Ref *rs, int nr)
{
    for (int k = 0; k < nr; k++) if (rs[k].sym->usage == U_FLOAT) g_fstmt = 1;
    for (int k = 0; k < nr; k++) if (!rs[k].rm && sym_wide(rs[k].sym)) return 1;
    return 0;
}
