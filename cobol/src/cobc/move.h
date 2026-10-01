/* s32-cobc: MOVE.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- MOVE ------------------------------------------------------------- */

/* A copy whose length the compiler knows.  memcpy is a call that saves five
 * registers, and then -- whenever the two addresses are not congruent mod 4,
 * which is the ordinary case for fields packed into a record -- copies a byte
 * at a time: about 90 instructions to move the ten bytes of a PIC 9(10).
 * Below the threshold the same copy is 2n inline instructions and needs no
 * alignment analysis at all, which is what makes it unconditionally safe;
 * above it, the loop earns its prologue back and memcpy is still the answer.
 * a[0] is the destination and a[1] the source, already staged as Args so the
 * subscripted and reference-modified forms marshal the way they always do.
 * GitHub #27. */
/* The threshold, 2026-09-01.  The engines disagree, because slow32-dbt
 * recognises the memcpy entry point by name and substitutes a native stub,
 * while the interpreters execute every instruction the call runs.  Both hosts
 * put the DBT's cliff between 8 and 16, so one constant is right -- but do
 * NOT settle it on bench/b3big, which is a MOVE-only loop and therefore
 * nothing but the thing being measured: at the 0/8 boundary it says 8 on
 * x86-64 and 0 on arm64.  The corpus decides, and says 8 on both.
 *
 *      COPY_INLINE_MAX      0            8           16           40
 *      corpus insns         2099450533   2046857172  1983245824   1963697060
 *      corpus batch.sh (s)  0.40         0.40        0.42         0.43
 *
 * Note the inversion: past 8, guest instructions go down while wall time goes
 * up.  The inline copy is fewer instructions and still slower than the DBT's
 * stub, so instruction count is the wrong metric for this one constant.
 * cobol/ISSUES.md section 24 carries both hosts' tables and the reasoning;
 * bench/sweep.sh re-runs the microbenchmark, but decide on the corpus. */
#ifndef COPY_INLINE_MAX
#define COPY_INLINE_MAX 8
#endif

static void emit_copy_fixed(const Arg *a, int n)
{
    if (n <= 0) return;
    if (n > COPY_INLINE_MAX) {
        Arg b[3] = { a[0], a[1], arg_imm(n) };
        emit_args(b, 3); emit_call("memcpy");
        return;
    }
    emit_args(a, 2);            /* r3 = destination, r4 = source */
    for (int i = 0; i < n; i++) {
        emit("\tldbu r1, r4+%d", i);
        emit("\tstb r3+%d, r1", i);
    }
}

/* does a group's length depend on an OCCURS DEPENDING ON below it?  One
 * occurrence of the table itself (always subscripted) is fixed-length. */
static int has_odo(Sym *s)
{
    for (int c = s->child; c >= 0; c = g_sym[c].sibling)
        if (g_sym[c].odo_dep[0] || has_odo(&g_sym[c])) return 1;
    return 0;
}

/* the OCCURS DEPENDING ON table below a group, at any depth (85 allows one) */
static Sym *odo_table_below(Sym *s)
{
    for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
        if (g_sym[c].odo_dep[0]) return &g_sym[c];
        Sym *t = odo_table_below(&g_sym[c]);
        if (t) return t;
    }
    return NULL;
}

/* MOVE ALL literal to a numeric or numeric-edited item.  The literal is
 * repeated to the receiver's character positions and then moved as any
 * alphanumeric literal is: an unsigned integer, aligned on the decimal
 * point (X3.23-1985 IV-11).  The text's own example (XVII-82, from X3J4
 * interpretation B-23): MOVE ALL "123" to a PIC 99V99 item gives 31.00,
 * the digits of "1231" that fit, not the 12.31 a fill would leave. */
static void emit_move(Opnd *src, Ref *dst);
static void emit_move_all_numeric(Opnd *src, Ref *dst, int n)
{
    if (src->tok->len > 1) bp(BP_O9_ALL_NUMERIC, src->line);
    Tok *t = xmalloc(sizeof *t); *t = *src->tok;
    t->s = xmalloc((size_t)n + 1);
    for (int i = 0; i < n; i++) t->s[i] = src->tok->s[i % src->tok->len];
    t->s[n] = 0; t->len = n;
    Opnd lit = *src; lit.kind = O_STR; lit.tok = t;
    emit_move(&lit, dst);
}

/* national data in a MOVE (COBOL 2002 14.9.25; cobol ISSUES-62).  A
 * national receiver takes anything alphanumeric, numeric or national,
 * converted by libcob (UTF-8 to UTF-16BE, national-space padding); a
 * national sender reaches only a national receiver or a group (moved as
 * bytes, general rule 4).  Returns 0 when neither side is national. */
static int opnd_is_national(const Opnd *o)
{
    if (o->kind == O_FUNC) return o->fnat;
    if (o->kind == O_STR || o->kind == O_ALL) return o->tok && o->tok->nat;
    /* a USAGE NATIONAL numeric item reference-modified: a national part (8.4.3.3.4 rule 6c) */
    return o->kind == O_REF && (sym_is_national(o->ref.sym) ||
           (o->ref.rm && !o->ref.sym->is_group && o->ref.sym->usage == U_NATIONAL && !sym_is_boolean(o->ref.sym)));
}

static int ref_is_national(const Ref *r) { return sym_is_national(r->sym); }

/* INSPECT, STRING and UNSTRING take items of usage display or national
 * only (2023 14.9.22.3 rules 1-2, 14.9.43.3 rule 1, 14.9.48.3 rules 2 and
 * 4): not bits (cobol ISSUES-86) */
static void no_bits(const Opnd *o, const char *stmt)
{
    static const char *rule[] = { "INSPECT", "14.9.22.3 rules 1 and 2", "STRING", "14.9.43.3 rule 1", "UNSTRING", "14.9.48.3 rules 2 and 4", NULL };
    const char *r = "";
    for (int i = 0; rule[i]; i += 2) if (!strcmp(stmt, rule[i])) r = rule[i + 1];
    if (o->kind == O_REF && sym_bitlike(o->ref.sym))
        die_at(o->line, "%s takes items of usage display or national, not the USAGE BIT item '%s' (2023 %s)", stmt, o->ref.sym->name, r);
}

/* STRING, UNSTRING: when one operand is national all are (2023 14.9.43.3
 * rule 1, 14.9.48.3 rule 3); a figurative constant takes the class */
static void nat_class_check(const Opnd *o, int nat, const char *stmt, const char *rule)
{
    if (o->kind == O_FIG) return;
    if (opnd_is_national(o) != nat)
        die_at(o->line, "%s: %s operand beside %s ones (2023 %s)", stmt, nat ? "a non-national" : "a national",
               nat ? "national" : "non-national", rule);
}

/* a one-character figurative constant as an address and length: one
 * byte, or one national character */
static void fig_char_args(const Opnd *o, int nat, Arg *addr, Arg *len)
{
    if (nat) {
        unsigned u = nat_fig(o->tok->s); unsigned char two[2] = { (unsigned char)(u >> 8), (unsigned char)u };
        *addr = arg_label(lit_label(two, 2)); *len = arg_imm(2);
    } else {
        unsigned char c = (unsigned char)fig_byte(o->tok->s);
        *addr = arg_label(lit_label(&c, 1)); *len = arg_imm(1);
    }
}

/* a figurative constant or ALL literal, as the national literal of nbytes
 * it stands for beside a national operand */
static void nat_fig_opnd(Opnd *o, int nbytes)
{
    if (o->kind != O_FIG && o->kind != O_ALL) return;
    if (nbytes < 2) nbytes = 2;
    unsigned char *b = xmalloc((size_t)nbytes);
    if (o->kind == O_FIG) {
        unsigned u = nat_fig(o->tok->s);
        for (int i = 0; i + 1 < nbytes; i += 2) { b[i] = (unsigned char)(u >> 8); b[i + 1] = (unsigned char)u; }
    } else {
        const unsigned char *lit = (const unsigned char *)o->tok->s; int len = o->tok->len;
        unsigned char *conv = NULL;
        if (!o->tok->nat) {
            conv = xmalloc((size_t)len * 4 + 2); len = utf8_to_utf16be(lit, len, conv); lit = conv;
            if (len < 0) die_at(o->line, "an ALL literal compared with a national item must be UTF-8 text");
        }
        for (int i = 0; i < nbytes; i++) b[i] = lit[i % len];
        free(conv);
    }
    Tok *t = xmalloc(sizeof *t); *t = *o->tok;
    t->kind = T_STR; t->s = (char *)b; t->len = nbytes & ~1; t->nat = 1;
    o->kind = O_STR; o->tok = t;
}

static int emit_move_national(Opnd *src, Ref *dst)
{
    Sym *d = dst->sym;
    int dn = sym_is_national(d) || (dst->rm && !d->is_group && d->usage == U_NATIONAL && !sym_is_boolean(d)),   /* a national part, 8.4.3.3.4 rule 6c */
        sn = opnd_is_national(src);
    if (!dn && !sn) return 0;
    if (!dn) {
        if (d->is_group) return 0;                  /* a group receives the bytes (14.9.25 general rule 4) */
        if (is_numeric_sym(d) || d->pi.category == PIC_NUMERIC_EDITED) {
            /* national to numeric or numeric-edited: valid (the 14.9.25
             * table), the characters taken as for an alphanumeric sender */
            if (dst->rm) die_at(dst->line, "a reference-modified numeric receiver of national data is not implemented");
            Arg a[4];
            opnd_args(src, &a[0], &a[1], d->size, 1);
            a[2] = arg_ref(dst); a[3] = arg_desc(sym_desc(d));
            emit_args(a, 4); emit_call("cob_move");
            return 1;
        }
        die_at(dst->line, "a national item cannot be moved to the alphanumeric item '%s' (2023 14.9.25): use FUNCTION DISPLAY-OF", d->name);
    }
    /* a numeric sender that is not an integer has no national form (the
     * 14.9.25 table: numeric noninteger to national, no) */
    if ((src->kind == O_NUM && src->num.scale > 0) ||
        (src->kind == O_REF && !src->ref.rm && is_numeric_sym(src->ref.sym) && src->ref.sym->pi.scale > 0))
        die_at(dst->line, "a numeric item that is not an integer cannot be moved to the national item '%s' (2023 14.9.25)", d->name);
    int n = d->size / 2;
    /* a reference-modified receiver: its bytes, known here or at run time */
    Arg dlen = !dst->rm ? arg_imm(d->size) : dst->rm_len ? arg_imm(2 * dst->rm_len) : arg_rlen(dst);
    Arg ddesc = !dst->rm ? arg_desc(sym_desc(d)) : dst->rm_len ? arg_desc(nat_desc(2 * (int)dst->rm_len)) : arg_rdesc(dst);
    if ((src->kind == O_FIG || src->kind == O_ALL) && d->pi.edited && !dst->rm) {
        /* national-edited: the figurative as a national literal of the
         * item's characters, which the move then edits (cobol ISSUES-73) */
        Opnd lit = *src;
        if (lit.kind == O_ALL && !lit.tok->nat) {
            unsigned char *conv = xmalloc((size_t)lit.tok->len * 4 + 2); int len = utf8_to_utf16be((const unsigned char *)lit.tok->s, lit.tok->len, conv);
            if (len < 0) die_at(src->line, "an ALL literal moved to a national item must be UTF-8 text");
            Tok *t = xmalloc(sizeof *t); *t = *lit.tok; t->s = (char *)conv; t->len = len; t->nat = 1; lit.tok = t;
        }
        nat_fig_opnd(&lit, d->size);
        Arg a[4];
        opnd_args(&lit, &a[0], &a[1], d->size, 0);
        a[2] = arg_ref(dst); a[3] = arg_desc(sym_desc(d));
        emit_args(a, 4); emit_call("cob_move");
        return 1;
    }
    if (src->kind == O_FIG && dst->rm) {
        unsigned u = nat_fig(src->tok->s); unsigned char two[2] = { (unsigned char)(u >> 8), (unsigned char)u };
        Arg a[4] = { arg_ref(dst), dlen, arg_label(lit_label(two, 2)), arg_imm(2) };
        emit_args(a, 4); emit_call("cob_fill_all");
        return 1;
    }
    if (src->kind == O_FIG) {
        Arg a[3] = { arg_ref(dst), arg_imm(n), arg_imm((long)nat_fig(src->tok->s)) };
        emit_args(a, 3); emit_call("cob_fill_nat");
        return 1;
    }
    if (src->kind == O_ALL) {
        /* ALL literal: its national characters repeated */
        const unsigned char *lit = (const unsigned char *)src->tok->s; int len = src->tok->len;
        unsigned char *conv = NULL;
        if (!src->tok->nat) {
            conv = xmalloc((size_t)len * 4 + 2); len = utf8_to_utf16be(lit, len, conv); lit = conv;
            if (len < 0) die_at(src->line, "an ALL literal moved to a national item must be UTF-8 text");
        }
        Arg a[4] = { arg_ref(dst), dlen, arg_label(lit_label(lit, len)), arg_imm(len) };
        emit_args(a, 4); emit_call("cob_fill_all");
        free(conv);
        return 1;
    }
    Arg a[4];
    opnd_args(src, &a[0], &a[1], ref_static_len(dst) > 0 ? ref_static_len(dst) : d->size, 0);
    a[2] = arg_ref(dst); a[3] = ddesc;
    emit_args(a, 4); emit_call("cob_move");
    if (!sn && ec_on_name("EC-DATA-CONVERSION")) {
        /* a byte that is not UTF-8 became U+FFFD (14.9.25 general rule 6) */
        int Lok = new_label();
        emit_call("cob_nat_conv_bad");
        emit("\tbeq r1, r0, .L%d", Lok);
        emit_ec_raise(ec_find("EC-DATA-CONVERSION", 0));
        emit_label(Lok);
    }
    return 1;
}

/* boolean positions of a boolean item */
static int bool_positions(const Sym *s) { return sym_bitlike(s) ? s->bits : s->usage == U_NATIONAL ? s->size / 2 : s->size; }

/* a figurative constant or ALL literal beside a boolean operand of n
 * positions: ZERO is boolean zeros, ALL B"..." its value repeated; any
 * other figurative is no boolean value (2023 14.9.25 rule 7) */
static void bool_fig_opnd(Opnd *o, int n)
{
    if (o->kind != O_FIG && o->kind != O_ALL) return;
    if (n < 1) n = 1;
    char *b = xmalloc((size_t)n + 1);
    if (o->kind == O_FIG) {
        if (strncmp(o->tok->s, "zero", 4)) die_at(o->line, "%s is not a boolean value (2023 14.9.25 rule 7)", o->tok->s);
        memset(b, '0', (size_t)n);
    } else {
        /* ALL "1" is as good as ALL B"1": rule 7 bars only characters that
         * are no boolean character (cobol ISSUES-94 B15) */
        Tok *t = o->tok;
        int w = t->nat ? 2 : 1, len = t->len / w;
        char *v = xmalloc((size_t)len + 1);
        for (int i = 0; i < len; i++) {
            unsigned ch = t->nat ? ((unsigned char)t->s[2 * i] << 8 | (unsigned char)t->s[2 * i + 1]) : (unsigned char)t->s[i];
            if (!t->boolv && ch != '0' && ch != '1') die_at(o->line, "ALL %s: a character that is not 0 or 1 is no boolean value (2023 14.9.25.3 rule 7)", tok_desc(t));
            v[i] = (char)ch;
        }
        for (int i = 0; i < n; i++) b[i] = v[i % (len ? len : 1)];
        free(v);
    }
    Tok *t = xmalloc(sizeof *t); *t = *o->tok;
    t->kind = T_STR; t->s = b; t->len = n; t->boolv = 1; t->nat = 0;
    o->kind = O_STR; o->tok = t;
}

static int opnd_is_boolean(const Opnd *o)
{
    if (o->kind == O_STR || o->kind == O_ALL) return o->tok && o->tok->boolv;
    if (o->kind == O_FUNC) return o->fbool;
    if (o->kind == O_BEXPR) return 1;
    return o->kind == O_REF && sym_is_boolean(o->ref.sym);
}

/* MOVE with a boolean side (2023 14.9.25 table): a boolean receiver takes
 * a boolean, alphanumeric or national sender, aligned left, zero-filled
 * or truncated on the right (14.6.8.6); a boolean sender goes to an
 * alphanumeric, national or group receiver as its characters 0 and 1.
 * Numeric and edited categories are no boolean's partners either way. */
static int emit_move_boolean(Opnd *src, Ref *dst)
{
    Sym *d = dst->sym;
    int db = sym_is_boolean(d), sb = opnd_is_boolean(src);
    if (!db && !sb) return 0;
    /* A move with a group on either side -- a group that is not a bit
     * group, which is treated as elementary (13.18.29.4 rule 1b) -- is no
     * elementary move: its bytes are copied without conversion (14.9.25.4
     * rule 4; cobol ISSUES-94 B14).  The group MOVE does that. */
    if (d->is_group && !d->bitgroup && !dst->rm) return 0;
    if (src->kind == O_REF && src->ref.sym->is_group && !src->ref.sym->bitgroup && !src->ref.rm) return 0;
    if (!db) {
        int c = d->pi.category;
        if (c == PIC_ALPHANUMERIC || c == PIC_ALPHANUMERIC_EDITED || c == PIC_NATIONAL) return 0;   /* as its characters */
        die_at(dst->line, "a boolean item cannot be moved to the %s item '%s' (2023 14.9.25)",
               c == PIC_ALPHABETIC ? "alphabetic" : "numeric", d->name);
    }
    int n = dst->rm ? (dst->rm_len ? (int)dst->rm_len : 1) : bool_positions(d);
    Opnd lit;
    if (src->kind == O_ALL && dst->rm && !dst->rm_len) {
        /* ALL to positions known only at run time: repeated to them there
         * (cobol ISSUES-94 B4) */
        lit = *src;
        if (!lit.tok->boolv) { bool_fig_opnd(&lit, lit.tok->len / (lit.tok->nat ? 2 : 1)); lit.kind = O_ALL; }
        bool_emit_operand(&lit);
        Arg a[2] = { arg_ref(dst), arg_rdesc(dst) };
        emit_args(a, 2);
        emit_call("cob_bstore");
        emit_call("cob_bdrop");
        return 1;
    }
    if (src->kind == O_FIG || src->kind == O_ALL) { lit = *src; bool_fig_opnd(&lit, n); src = &lit; }
    else if (!sb) {
        int ok = src->kind == O_STR ||
                 (src->kind == O_REF && (src->ref.sym->is_group || src->ref.sym->pi.category == PIC_ALPHANUMERIC ||
                                         (sym_is_national(src->ref.sym) && !src->ref.sym->pi.edited))) ||
                 (src->kind == O_FUNC && !fn_is_numeric(src->fn));
        if (!ok) die_at(src->line, "only a boolean, alphanumeric or national item can be moved to the boolean item '%s' (2023 14.9.25)", d->name);
    }
    Arg a[4];
    opnd_args(src, &a[0], &a[1], n, 0);
    a[2] = arg_ref(dst);
    a[3] = !dst->rm ? arg_desc(sym_desc(d)) : dst->rm_len && (dst->rm_start || !dst->rm_bit) ? arg_desc(part_desc(dst)) : arg_rdesc(dst);
    emit_args(a, 4); emit_call("cob_move");
    return 1;
}

static int dx_move(Opnd *src, Ref *dst);
/* is s g itself or one of its subordinates */
static int sym_within(const Sym *s, const Sym *g)
{
    for (;;) {
        if (s == g) return 1;
        if (s->parent < 0) return 0;
        s = &g_sym[s->parent];
    }
}
static void emit_move(Opnd *src, Ref *dst)
{
    Sym *d = dst->sym;
    /* A receiving group over an OCCURS DEPENDING ON table (X3.23-1985
     * VI-27, OCCURS general rule 3): with the DEPENDING ON item outside
     * the group, only the part its value gives at the start of the
     * operation is used, receiving as sending (3a) -- the bytes past it
     * are left alone; with the item inside the group, a receiving group
     * has its maximum length (3b), as it is laid out.  Every receiving
     * group took the maximum, so a MOVE into a record built at a shorter
     * count overwrote the rest (tests/gen found it; tests/free/odorecv). */
    if (d->is_group && !dst->rm && !dst->nsub && has_odo(d)) {
        Sym *tbl = odo_table_below(d);
        if (tbl && tbl->odo_dep_sym && !sym_within(tbl->odo_dep_sym, d)) {
            for (Sym *k = tbl; k != d; k = &g_sym[k->parent])
                if (k->sibling >= 0)
                    die_at(dst->line, "'%s': items follow its OCCURS DEPENDING ON table (variable-location items are not implemented)", d->name);
            dst->rm = 1; dst->rm_start = 1; dst->rm_len = 0; dst->rm_lx = NULL;
            dst->rm_odo = 1; dst->odo_dep = tbl->odo_dep_sym;
            dst->odo_base = d->size - tbl->occurs * tbl->size; dst->odo_elem = tbl->size;
        } else {
            bp(BP_M2_ODO_RECEIVE, dst->line);   /* 3b: the maximum; COBOL 74 used the current length */
        }
    }
    if (src->kind == O_REF && src->ref.sym->is_group && has_odo(src->ref.sym) && !src->ref.rm_odo && !src->ref.rm && !src->ref.nsub) {
        /* a sending group's length is its current one.  The group is laid
         * out with the table at its maximum, so however deep the table
         * sits, as long as nothing follows it: length = size - (max - d) * elem */
        Sym *g = src->ref.sym, *tbl = odo_table_below(g);
        if (!tbl || !tbl->odo_dep_sym)
            die_at(src->line, "MOVE of the group '%s': its OCCURS DEPENDING ON table's DEPENDING ON item is not resolved", g->name);
        /* the table must be the last thing in the group: items after it
         * would sit at variable locations, which this layout (the maximum)
         * does not give them */
        for (Sym *k = tbl; k != g; k = &g_sym[k->parent])
            if (k->sibling >= 0)
                die_at(src->line, "MOVE of the group '%s': items follow its OCCURS DEPENDING ON table (variable-location items are not implemented)", g->name);
        Opnd dep; memset(&dep, 0, sizeof dep); dep.kind = O_REF; dep.ref.sym = tbl->odo_dep_sym; dep.ref.line = src->line;
        Arg a[6] = { arg_ref(&src->ref), arg_ref(dst), arg_value(&dep), arg_imm(d->size),
                     arg_imm(g->size - tbl->occurs * tbl->size), arg_imm(tbl->size) };
        emit_args(a, 6);
        emit_call("cob_move_odo");
        return;
    }
    if (d->is_cond) die_at(dst->line, "'%s' is a condition-name and cannot receive a MOVE", d->name);
    {   /* a strongly-typed group receives only a group of its own type
         * (14.9.25.3 rule 2); as a sender it goes anywhere a group does
         * (Table 16; cobol ISSUES-94 B13) */
        int ss = src->kind == O_REF ? src->ref.sym->strong : 0, ds = d->strong;
        if (ds && ss != ds)
            die_at(dst->line, "MOVE: the strongly-typed group '%s' (%s) receives only a group of the same type, the sender %s (2023 14.9.25.3 rule 2)",
                   d->name, strong_name(ds - 1), ss ? strong_name(ss - 1) : "is not strongly typed");
    }
    if (emit_move_boolean(src, dst)) return;
    if (emit_move_national(src, dst)) return;
    /* Sending and receiving items with byte-identical descriptors -- same
     * category, usage, size, digit count, scale, flags and PICTURE -- so the
     * move is a byte copy.  Descriptors are deduplicated by a whole-struct
     * memcmp, which is why identity is one integer compare here.
     *
     * This is a conformance fix that happens to be fast.  Measured against
     * the oracle 2026-09-01: GnuCOBOL passes the bytes through unchanged,
     * including bytes cob_put_num would never write -- spaces in a numeric
     * field nothing has filled in, an 0xF sign nibble on a COMP-3 record
     * from a foreign system, a COMP holding more than its picture's digits.
     * The generic path decoded and re-encoded all three, so ' 12 45abc '
     * arrived as '0120451230' where GnuCOBOL delivered it verbatim.  The
     * cost went with it: a PIC 9(10) to PIC 9(10) MOVE ran a digit loop out
     * through cob_get_num and a divide loop back through cob_put_num, 646
     * instructions to copy ten bytes.  GitHub #27; tests/free/identmove. */
    if (src->kind == O_REF && !src->ref.rm && !dst->rm && !src->ref.sym->is_cond &&
        sym_desc(src->ref.sym) == sym_desc(d)) {
        Arg a[2] = { arg_ref(dst), arg_ref(&src->ref) };
        emit_copy_fixed(a, d->size);
        return;
    }
    if (src->kind == O_REF && src->ref.sym->is_group && !src->ref.rm && !dst->rm && !src->ref.sym->is_cond) {
        /* a group sending item: an alphanumeric-to-alphanumeric move whatever
         * the receiver -- no conversion, no editing (X3.23 6.18.2; NC105A
         * moves a group to numeric and to edited items and reads the bytes) */
        Sym *s = src->ref.sym;
        Arg a[4] = { arg_ref(&src->ref), arg_imm(s->size), arg_ref(dst), arg_imm(d->size) };
        emit_args(a, 4); emit_li("r7", d->just); emit_call("cob_move_alnum");
        return;
    }
    /* a reference-modified receiver is alphanumeric, whatever its item's category (8.4.2.4.3) */
    if (!d->is_group && !dst->rm && (d->pi.category == PIC_NUMERIC_EDITED || d->pi.category == PIC_ALPHANUMERIC_EDITED)) {
        int ned = d->pi.category == PIC_NUMERIC_EDITED;
        if (src->kind == O_FIG && !ned) {
            /* MOVE SPACES to an alphanumeric-edited item: a literal of spaces
             * through the edit, the insertion characters appearing */
            unsigned char *f = xmalloc((size_t)d->size); memset(f, fig_byte(src->tok->s), (size_t)d->size);
            Arg a[4] = { arg_label(lit_label(f, d->size)), arg_desc(str_desc(d->size)), arg_ref(dst), arg_desc(sym_desc(d)) };
            free(f);
            emit_args(a, 4); emit_call("cob_move");
            return;
        }
        if (src->kind == O_FIG && !(ned && !strncmp(src->tok->s, "zero", 4))) {
            Arg a[3] = { arg_ref(dst), arg_imm(d->size), arg_imm(fig_byte(src->tok->s)) };
            emit_args(a, 3); emit_call("cob_fill");
            return;
        }
        if (src->kind == O_ALL && ned) { emit_move_all_numeric(src, dst, d->size); return; }
        if (src->kind == O_ALL) {
            Arg a[4] = { arg_ref(dst), arg_imm(d->size), arg_label(lit_label((unsigned char *)src->tok->s, src->tok->len)), arg_imm(src->tok->len) };
            emit_args(a, 4); emit_call("cob_fill_all");
            return;
        }
        if (src->kind == O_REF && src->ref.sym->is_cond) die_at(src->line, "'%s' is a condition-name and cannot be moved", src->ref.sym->name);
        if (ned && !dst->rm && dx_move(src, dst)) return;   /* a numeric sender: fetch and store, the store editing */
        Arg a[4];
        opnd_args(src, &a[0], &a[1], d->size, ned);
        a[2] = arg_ref(dst); a[3] = arg_desc(sym_desc(d));
        emit_args(a, 4); emit_call("cob_move");
        return;
    }
    int dnum = is_numeric_sym(d);

    if (dst->rm || (src->kind == O_REF && src->ref.rm)) {
        /* a reference-modified side is an alphanumeric of runtime extent */
        if (src->kind == O_FIG || src->kind == O_ALL) {
            Arg len = dst->rm_len ? arg_imm((long)dst->rm_len) : arg_rlen(dst);
            if (src->kind == O_ALL && src->tok->len > 1) {
                Arg b[4] = { arg_ref(dst), len, arg_label(lit_label((unsigned char *)src->tok->s, src->tok->len)), arg_imm(src->tok->len) };
                emit_args(b, 4); emit_call("cob_fill_all"); return;
            }
            Arg a[3] = { arg_ref(dst), len, arg_imm(src->kind == O_ALL ? (unsigned char)src->tok->s[0] : fig_byte(src->tok->s)) };
            emit_args(a, 3); emit_call("cob_fill"); return;
        }
        Arg a[4];
        opnd_args(src, &a[0], &a[1], ref_static_len(dst) > 0 ? ref_static_len(dst) : 1, dnum && !dst->rm);
        a[2] = arg_ref(dst);
        a[3] = dst->rm ? (dst->rm_len ? arg_desc(str_desc((int)dst->rm_len)) : arg_rdesc(dst)) : arg_desc(sym_desc(d));
        emit_args(a, 4); emit_call("cob_move");
        return;
    }

    if (!dnum) {
        switch (src->kind) {
        case O_STR: case O_NUM: {
            if (src->kind == O_NUM && !numlit_is_int(&src->num))
                die_at(src->line, "MOVE of a non-integer numeric literal to the alphanumeric item '%s' is not valid COBOL", d->name);
            const char *txt = src->tok ? src->tok->s : NULL;
            int len = src->tok ? src->tok->len : 0;
            char dig[40];
            if (src->kind == O_NUM) { memcpy(dig, src->num.digits, src->num.ndigits); txt = dig; len = src->num.ndigits; }
            const char *l = lit_label((unsigned char *)txt, len);
            if (len == d->size && !d->just) {
                Arg a[2] = { arg_ref(dst), arg_label(l) };
                emit_copy_fixed(a, len);
            } else {
                Arg a[5] = { arg_label(l), arg_imm(len), arg_ref(dst), arg_imm(d->size), arg_imm(d->just) };
                emit_args(a, 5); emit_call("cob_move_alnum");
            }
            return;
        }
        case O_FIG: {
            Arg a[3] = { arg_ref(dst), arg_imm(d->size), arg_imm(fig_byte(src->tok->s)) };
            emit_args(a, 3); emit_call("cob_fill");
            return;
        }
        case O_ALL: {
            Arg a[4] = { arg_ref(dst), arg_imm(d->size), arg_label(lit_label((unsigned char *)src->tok->s, src->tok->len)), arg_imm(src->tok->len) };
            emit_args(a, 4); emit_call("cob_fill_all");
            return;
        }
        case O_FUNC: {
            Arg a[4];
            opnd_args(src, &a[0], &a[1], d->size, 0);
            a[2] = arg_ref(dst); a[3] = arg_desc(sym_desc(d));
            emit_args(a, 4); emit_call("cob_move");
            return;
        }
        default: {
            Sym *s = src->ref.sym;
            if (s->is_cond) die_at(src->line, "'%s' is a condition-name and cannot be moved", s->name);
            /* a non-integer numeric item to an alphanumeric one: the 85 text
             * forbids it, the NIST cases (NC105A, NC114M, NC124A) want it --
             * the digits as stored, the sign and the point unrepresented; the
             * cases win (the user's ruling, 2026-08-31) */
            if (!is_numeric_sym(s) && s->size == d->size && !d->just) {
                Arg a[2] = { arg_ref(dst), arg_ref(&src->ref) };
                emit_copy_fixed(a, d->size);
                return;
            }
            if (d->is_group) {
                /* a group receiving item: an alphanumeric-to-alphanumeric move
                 * (a group sending item was taken above) */
                Arg a[4] = { arg_ref(&src->ref), arg_imm(s->size), arg_ref(dst), arg_imm(d->size) };
                emit_args(a, 4); emit_li("r7", d->just); emit_call("cob_move_alnum");
                return;
            }
            Arg a[4] = { arg_ref(&src->ref), arg_desc(sym_desc(s)), arg_ref(dst), arg_desc(sym_desc(d)) };
            emit_args(a, 4); emit_call("cob_move");
            return;
        }
        }
    }

    /* numeric receiver; NULL is a pointer's zero address (SET ... TO NULL) */
    if (src->kind == O_FIG && (!strncmp(src->tok->s, "zero", 4) || (!strncmp(src->tok->s, "null", 4) && d->usage == U_POINTER))) {
        Opnd z; memset(&z, 0, sizeof z); z.kind = O_NUM; numlit_zero(&z.num); z.line = src->line;
        emit_move(&z, dst);
        return;
    }
    if (src->kind == O_FIG || src->kind == O_ALL) {
        if (d->usage != U_DISPLAY) die_at(src->line, "%s cannot be moved to the %s item '%s'", src->tok->s, usage_name(d->usage), d->name);
        if (src->kind == O_ALL) { emit_move_all_numeric(src, dst, d->size); return; }
        Arg a[3] = { arg_ref(dst), arg_imm(d->size), arg_imm(fig_byte(src->tok->s)) };
        emit_args(a, 3); emit_call("cob_fill");
        return;
    }
    if (src->kind == O_NUM && !numlit_wide(&src->num) && is_hot_int(d)) {
        long long v = numlit_int(&src->num);
        if (d->usage == U_BINARY) v %= pow10l(d->pi.digits);
        if (!d->pi.is_signed && v < 0 && d->uvar != UV_COMPX) v = -v;     /* COMP-X: two's complement (move_desc) */
        emit_ref_addr(dst, "r3");
        emit_li("r1", (long)v);
        emit_store_int(d, "r3", "r1");
        return;
    }
    if (src->kind == O_REF && is_hot_int(d) && is_hot_int(src->ref.sym) &&
        (d->pi.is_signed || !src->ref.sym->pi.is_signed)) {
        Sym *s = src->ref.sym;
        emit_ref_addr(&src->ref, "r3");
        emit_load_int(s, "r3", "r1");
        if (d->usage == U_BINARY && !(s->usage == U_BINARY && s->pi.digits <= d->pi.digits)) emit_trunc(d);
        emit("\tstw sp+%d, r1", SLOT_A);
        emit_ref_addr(dst, "r3");
        emit("\tldw r1, sp+%d", SLOT_A);
        emit_store_int(d, "r3", "r1");
        return;
    }
    if (dx_move(src, dst)) return;              /* numeric to numeric: fetch and store, without cob_move's dispatch */
    Arg a[4]; Arg da = arg_ref(dst), dd = arg_desc(move_desc(d));
    opnd_args(src, &a[0], &a[1], d->size, 1);
    a[2] = da; a[3] = dd;
    emit_args(a, 4);
    emit_call("cob_move");
}

/* CORRESPONDING (X3.23 6.4.2): items of the two groups with the same
 * name and the same qualifiers below them, neither FILLER, neither with
 * REDEFINES or OCCURS (nor subordinate to one: such a child is skipped
 * with its subtree), no condition-names.  Two groups that correspond
 * are searched further; MOVE moves a pair when at least one is
 * elementary, ADD/SUBTRACT act on a pair of elementary numeric items.
 * The operands' own subscripts and qualification carry to every pair. */
static void emit_store_receivers(Ref *rs, int *rounded, int nr, int hot, int giving, int subtract, int size_err,
                                 long long sum_mag, int sum_nonneg);
static void emit_push(Opnd *o);
static Opnd ref_opnd(const Ref *r);
static int at_size_error_clause(void);
static void parse_size_error_clauses(int size_err, const char *end_word);

static int corr_eligible(Sym *c)
{
    return !c->is_filler && !c->is_cond && c->level != 66 && c->redefines < 0 && !c->occurs && !c->odo_dep[0] &&
           (c->is_group || (c->usage != U_INDEX && c->usage != U_POINTER));    /* 14.7.6 rule 4 */
}

/* The validity of a MOVE by category (2023 14.9.25.3 syntax rules 5, 6,
 * 8 and 10 with Table 16; 85 VI-104 general rule 3a-c).  Boolean moves
 * and strongly-typed groups are checked where they are emitted; a group
 * on either side is an alphanumeric move and always valid, and a
 * reference modification is alphanumeric (or national). */
enum { MC_NONE, MC_ALPHA, MC_ALNUM, MC_ALNUMED, MC_NAT, MC_NATED, MC_INT, MC_NONINT, MC_NUMED };
static const char *mc_name[] = { "", "alphabetic", "alphanumeric", "alphanumeric-edited", "national", "national-edited",
                                 "numeric integer", "numeric noninteger", "numeric-edited" };

static int move_cat_sym(const Sym *s, int rm)
{
    if (s->is_group || sym_is_boolean(s) || sym_bitlike(s)) return MC_NONE;
    if (rm) return sym_is_national(s) || s->usage == U_NATIONAL ? MC_NAT : MC_ALNUM;
    switch (s->pi.category) {
    case PIC_ALPHABETIC: return MC_ALPHA;
    case PIC_ALPHANUMERIC: return MC_ALNUM;
    case PIC_ALPHANUMERIC_EDITED: return MC_ALNUMED;
    case PIC_NATIONAL: return s->pi.edited ? MC_NATED : MC_NAT;
    case PIC_NUMERIC: return s->pi.scale > 0 ? MC_NONINT : MC_INT;
    case PIC_NUMERIC_EDITED: return MC_NUMED;
    }
    return MC_NONE;
}

/* why the move is invalid, into msg, or NULL: MOVE refuses it, and a
 * CORRESPONDING pair it names does not correspond (2023 14.7.6 rule 2,
 * 85 VI-68 6.4.3 rule 2) */
#define MV_BAD(...) do { snprintf(msg, MV_MSG, __VA_ARGS__); return msg; } while (0)
enum { MV_MSG = 256 };
static const char *move_invalid(const Opnd *src, const Ref *dst, char *msg)
{
    const Sym *d = dst->sym;
    const Sym *sy = src->kind == O_REF ? src->ref.sym : NULL;
    /* index and pointer items are set, not moved (rule 1; 85 syntax rule 4) */
    for (int k = 0; k < 2; k++) {
        const Sym *x = k ? d : sy;
        if (x && !x->is_group && (x->usage == U_INDEX || x->usage == U_POINTER))
            MV_BAD("MOVE: the %s item '%s' is not an operand of MOVE; use SET (%s)", x->usage == U_INDEX ? "index" : "pointer", x->name,
                   g_std < 2002 ? "85 VI-103 syntax rule 4" : "2023 14.9.25.3 rule 1");
    }
    int r = move_cat_sym(d, dst->rm);
    int rnum = r == MC_INT || r == MC_NONINT || r == MC_NUMED;
    /* binary-char, -short, -long go only to numeric items (rule 8) */
    if (sy && !src->ref.rm && !sy->is_group && (sy->usage == U_BCHAR || sy->usage == U_UBCHAR || sy->usage == U_SSHORT || sy->usage == U_USHORT ||
                                                   sy->usage == U_SINT || sy->usage == U_UINT || sy->usage == U_SDBL || sy->usage == U_UDBL) && !rnum)
        MV_BAD("MOVE: the %s item '%s' goes only to a numeric or numeric-edited item, not '%s' (2023 14.9.25.3 rule 8)",
               sy->usage == U_SSHORT || sy->usage == U_USHORT ? "binary-short" : sy->usage == U_SINT || sy->usage == U_UINT ? "binary-long" :
               sy->usage == U_SDBL || sy->usage == U_UDBL ? "binary-double" : "binary-char", sy->name, d->name);
    if (r == MC_NONE) return NULL;
    if (src->kind == O_FIG || src->kind == O_ALL) {
        const char *w = src->tok->s;
        if (src->kind == O_FIG && !strncmp(w, "null", 4)) return NULL;
        int zero = src->kind == O_FIG && !strncmp(w, "zero", 4);
        char up[64]; int n = 0;
        for (; w[n] && n < 63; n++) up[n] = (char)toupper((unsigned char)w[n]);
        up[n] = 0;
        if (zero && r == MC_ALPHA)
            MV_BAD("MOVE: ZERO cannot be moved to the alphabetic item '%s' (%s)", d->name, g_std < 2002 ? "85 VI-104 general rule 3b" : "2023 14.9.25.3 rule 6");
        if (zero || !rnum) return NULL;
        const char *rn = r == MC_NUMED ? "numeric-edited" : "numeric";
        if (g_std < 2002) {
            if (src->kind == O_FIG && !strncmp(w, "space", 5))
                MV_BAD("MOVE: SPACE cannot be moved to the %s item '%s' (85 VI-104 general rule 3a)", rn, d->name);
            return NULL;
        }
        /* an ALL literal of digits (or a symbolic character that is a
         * digit) may go to an integer: an obsolete feature */
        int digits = 1;
        if (src->kind == O_ALL && !src->tok->nat) for (int i = 0; i < src->tok->len; i++) digits &= isdigit((unsigned char)w[i]) != 0;
        else if (src->kind == O_ALL) digits = 0;
        else digits = symch_find(w) >= 0 && isdigit(fig_byte(w));
        if (digits && r == MC_INT) return NULL;
        MV_BAD("MOVE: the figurative constant %s%s cannot be moved to the %s item '%s' (2023 14.9.25.3 rule 5)",
               src->kind == O_ALL ? "ALL " : "", src->kind == O_ALL ? tok_desc(src->tok) : up, rn, d->name);
    }
    int s = MC_NONE;
    if (src->kind == O_NUM) s = numlit_is_int(&src->num) ? MC_INT : MC_NONINT;
    else if (src->kind == O_STR) s = src->tok->boolv ? MC_NONE : src->tok->nat ? MC_NAT : MC_ALNUM;
    else if (sy && !sy->is_cond) s = move_cat_sym(sy, src->ref.rm);
    if (s == MC_NONE) return NULL;
    int ok = 1; const char *r85 = NULL;
    switch (s) {
    case MC_ALPHA: case MC_ALNUMED: ok = !rnum; r85 = "3a"; break;
    case MC_NAT: ok = r != MC_ALPHA && r != MC_ALNUM && r != MC_ALNUMED; break;
    case MC_NATED: ok = r == MC_NAT || r == MC_NATED; break;
    case MC_INT: case MC_NUMED: ok = r != MC_ALPHA; r85 = "3b"; break;
    case MC_NONINT:
        if (r == MC_ALPHA) { ok = 0; r85 = "3b"; break; }
        if (r == MC_ALNUM || r == MC_ALNUMED) {
            /* the 85 text forbids it (3c) but NIST NC105A, NC114M and NC124A
             * move a noninteger item to an alphanumeric one, and the cases
             * win under -std=85 (the ruling of 2026-08-31; a literal was
             * always refused) */
            ok = g_std < 2002 && src->kind == O_REF; r85 = "3c"; break;
        }
        ok = r != MC_NAT && r != MC_NATED;
        break;
    }
    if (ok) return NULL;
    if (s == MC_NAT)
        MV_BAD("a national item cannot be moved to the %s item '%s' (2023 14.9.25.3 rule 10, Table 16): use FUNCTION DISPLAY-OF", mc_name[r], d->name);   /* r is alphanumeric or alphabetic here */
    char why[48];
    if (g_std < 2002 && r85) snprintf(why, sizeof why, "85 VI-104 general rule %s", r85);
    else snprintf(why, sizeof why, "2023 14.9.25.3 rule 10, Table 16");
    MV_BAD("MOVE: %s %s %s cannot be moved to the %s item '%s' (%s)", s == MC_ALPHA || s == MC_ALNUM || s == MC_ALNUMED ? "an" : "a",
           mc_name[s], src->kind == O_REF ? "item" : "literal", r == MC_INT || r == MC_NONINT ? "numeric" : mc_name[r], d->name, why);
}
#undef MV_BAD

static void move_valid(const Opnd *src, const Ref *dst)
{
    char msg[MV_MSG];
    const char *why = move_invalid(src, dst, msg);
    if (why) die_at(src->kind == O_FIG || src->kind == O_ALL ? src->line : dst->line, "%s", why);
}

static int arith_composite(const Opnd *ops, int n, const Ref *rs, int nr, const char *stmt, const char *rule85, int line);
static int corr_walk(Ref *a, Ref *b, int mode, int rounded, int size_err)
{
    int n = 0;
    for (int i = a->sym->child; i >= 0; i = g_sym[i].sibling) {
        Sym *c1 = &g_sym[i];
        if (!corr_eligible(c1)) continue;
        Sym *c2 = NULL;
        for (int j = b->sym->child; j >= 0; j = g_sym[j].sibling)
            if (corr_eligible(&g_sym[j]) && !strcmp(g_sym[j].name, c1->name)) { c2 = &g_sym[j]; break; }
        if (!c2) continue;
        Ref r1 = *a, r2 = *b; r1.sym = c1; r2.sym = c2;
        if (c1->is_group && c2->is_group) { n += corr_walk(&r1, &r2, mode, rounded, size_err); continue; }
        if (mode == 0) {
            Opnd o = ref_opnd(&r1);
            char msg[MV_MSG];
            if (move_invalid(&o, &r2, msg)) continue;       /* not a corresponding pair (14.7.6 rule 2) */
            emit_move(&o, &r2); n++;
        } else {
            if (c1->is_group || c2->is_group || c1->pi.category != PIC_NUMERIC || c2->pi.category != PIC_NUMERIC) continue;
            Opnd o = ref_opnd(&r1);
            arith_composite(&o, 1, &r2, 1, mode == 2 ? "SUBTRACT CORRESPONDING" : "ADD CORRESPONDING",
                            mode == 2 ? "X3.23-1985 SUBTRACT rule 3c" : "X3.23-1985 ADD rule 3c", r2.line);
            emit_push(&o);
            int rd = rounded;
            emit_store_receivers(&r2, &rd, 1, 0, 0, mode == 2, size_err, -1, 0);
            if (size_err) {         /* the size error of any pair is the statement's */
                emit("\tldw r1, sp+%d", SLOT_B); emit("\tldw r2, sp+%d", SLOT_A);
                emit("\tor r1, r1, r2"); emit("\tstw sp+%d, r1", SLOT_A);
            }
            n++;
        }
    }
    return n;
}

/* the two group operands of a CORRESPONDING statement */
static void parse_corr_operands(Ref *a, Ref *b, const char *between)
{
    parse_ref(a);
    if (!a->sym->is_group) die_at(a->line, "CORRESPONDING: '%s' is not a group", a->sym->name);
    if (a->rm) die_at(a->line, "CORRESPONDING: no reference modification on a group");
    expect_word(between);
    parse_ref(b);
    if (!b->sym->is_group) die_at(b->line, "CORRESPONDING: '%s' is not a group", b->sym->name);
    if (b->rm) die_at(b->line, "CORRESPONDING: no reference modification on a group");
}

static int parse_rounded_mode(void);
static void parse_arith_corr(int mode, const char *between, const char *end_word)
{
    Ref a, b; parse_corr_operands(&a, &b, between);
    int rounded = accept_word("rounded") ? parse_rounded_mode() : 0;
    int size_err = at_size_error_clause() || ec_size_on();
    if (size_err) emit("\tstw sp+%d, r0", SLOT_A);
    corr_walk(&a, &b, mode, rounded, size_err);
    if (size_err) { emit("\tldw r1, sp+%d", SLOT_A); emit("\tstw sp+%d, r1", SLOT_B); }
    parse_size_error_clauses(size_err, end_word);
}

/* storage in common: the two items' records, or one redefining the other's */
static int rec_base(const Sym *s)
{
    int r = s->record;
    while (r >= 0 && g_sym[r].redefines >= 0) r = g_sym[r].redefines;
    return r;
}

/* 0: the sender is safe to identify again for each receiver; 1: copy it
 * to a compiler-made record first; 2: an OCCURS DEPENDING ON group,
 * whose DEPENDING ON item is copied instead (its length is a run-time
 * one, and the bytes stay where they are) */
/* a reference modifier's length naming an item a receiver ahead of the
 * last shares storage with (move_needs_temp) */
typedef struct { const Ref *dst; int n; int line; } MvShare;
static int mv_shares(const Sym *s, const void *cx)
{
    const MvShare *m = cx;
    for (int i = 0; i < m->n - 1; i++)
        if (rec_base(s) == rec_base(m->dst[i].sym))
            die_at(m->line, "MOVE: the sender's reference modification uses '%s', which a receiver before the last changes; "
                   "identifying the sender once (general rule 1) with a computed length is not implemented", s->name);
    return 0;
}
static int move_needs_temp(const Opnd *src, const Ref *dst, int n)
{
    if (n < 2 || src->kind != O_REF) return 0;
    const Ref *r = &src->ref;
    for (int i = 0; i < n - 1; i++) {
        int rb = rec_base(dst[i].sym);
        if (r->rm_odo) { if (r->odo_dep && rec_base(r->odo_dep) == rb) return 2; continue; }
        for (int k = 0; k < r->nsub; k++)
            if (r->sub[k].sym == &g_subx || (r->sub[k].sym && rec_base(r->sub[k].sym) == rb)) return 1;
    }
    if (r->rm_odo) return 0;
    /* a reference modifier's start that is an expression: any receiver
     * may be in it */
    if (r->rm && r->rm_len && !r->rm_bit && !r->rm_start) return 1;
    /* a length that is one would need a snapshot of run-time length: not
     * done, so refused when a receiver ahead of the last shares storage
     * with an item the expressions name (docs/conformance/move.md) */
    if (r->rm && !r->rm_len && r->rm_lx) {
        MvShare m = { dst, n, src->line };
        if (r->rm_sx) expr_names(r->rm_sx, mv_shares, &m);
        expr_names(r->rm_lx, mv_shares, &m);
    }
    return 0;
}

static void parse_move(void)
{
    if (accept_word("corresponding") || accept_word("corr")) {
        Ref a, b; parse_corr_operands(&a, &b, "to");
        corr_walk(&a, &b, 0, 0, 0);
        return;
    }
    Opnd src; parse_operand(&src);
    expect_word("to");
    int n = 0, cap = 0;
    Ref *dst = NULL;
    while (at_operand()) {
        if (n == cap) { cap = cap ? 2 * cap : 8; dst = xrealloc(dst, (size_t)cap * sizeof *dst); }
        parse_ref(&dst[n]);
        move_valid(&src, &dst[n]);
        n++;
    }
    if (!n) die_at(cur()->line, "MOVE needs a receiving item");
    emit_incompat(&src);                /* a numeric sender's content (14.6.13.2 rule 2; MOVE GR 6d1) */
    /* the sender is identified once, before the first move (general rule
     * 1: MOVE a (b) TO b, c (b) moves a (b) to a temporary first).  When
     * a receiver ahead of the last shares storage with a subscript, the
     * DEPENDING ON item or a reference modifier's operands, the sender
     * is copied to a compiler-made record first */
    if (src.kind == O_FUNC && n > 1) {
        /* a function-identifier likewise: evaluated once, its result
         * kept for every receiver (RANDOM, CURRENT-DATE, or an argument
         * that a receiver changes) */
        int l = new_label(), sz = src.fsize > 0 ? src.fsize : 1;
        emit("\t.data"); emit("\t.p2align 3"); emit(".L%d:", l); emit("\t.space %d", sz); emit("\t.text");
        emit_fn_value(&src);
        emit("\tadd r4, r1, r0");
        char lb[24]; snprintf(lb, sizeof lb, ".L%d", l); emit_la("r3", lb);
        emit_li("r5", sz);
        emit_call("memcpy");
        src.fsaved = l + 1;
    }
    int snap = move_needs_temp(&src, dst, n);
    if (snap == 2) {
        FDesc fd; fdesc_of(&fd, src.ref.odo_dep);
        Sym *t = ftemp_new(&fd, src.line);
        Ref tr = ftemp_ref(t, src.line), dr; memset(&dr, 0, sizeof dr);
        dr.sym = src.ref.odo_dep; dr.line = src.line; dr.rm_lx = NULL;
        Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = dr; o.line = src.line;
        emit_move(&o, &tr);
        src.ref.odo_dep = t;
    } else if (snap == 1) {
        FDesc fd; fdesc_of(&fd, src.ref.sym);
        if (src.ref.rm) { fd.group = 1; fd.size = (int)(src.ref.rm_nat ? 2 * src.ref.rm_len : src.ref.rm_len); }
        Sym *t = ftemp_new(&fd, src.line);
        Ref tr = ftemp_ref(t, src.line);
        emit_move(&src, &tr);
        Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = tr; o.line = src.line;
        src = o;
    }
    for (int i = 0; i < n; i++) emit_move(&src, &dst[i]);
    free(dst);
}
