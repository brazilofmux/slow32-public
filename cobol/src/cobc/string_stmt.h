/* s32-cobc: STRING, UNSTRING.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- STRING ------------------------------------------------------------ */

/* a STRING sending operand or delimiter: no ALL figurative (X3.23-1985
 * STRING rule 1; 2023 14.9.43.3 rule 2); a literal is nonnumeric, an
 * identifier is usage display or national (85 rule 2; 2023 rule 1); an
 * elementary numeric item is an integer without P (85 rule 6; 2023 rule 8) */
static void str_operand(const Opnd *o)
{
    int e85 = g_std < 2002;
    if (o->kind == O_ALL) die_at(o->line, "STRING: an ALL figurative constant is not a STRING operand (%s)", e85 ? "X3.23-1985 STRING rule 1" : "2023 14.9.43.3 rule 2");
    if (o->kind == O_NUM) die_at(o->line, "STRING: a numeric literal is not a STRING operand; write it as \"...\" (%s)", e85 ? "X3.23-1985 STRING rule 2" : "2023 14.9.43.3 rule 1");
    if (o->kind != O_REF || o->ref.sym->is_group) return;
    Sym *x = o->ref.sym;
    no_bits(o, "STRING");                           /* the USAGE BIT wording */
    if (x->usage != U_DISPLAY && x->usage != U_NATIONAL)
        die_at(o->line, "STRING: '%s' is USAGE %s; an operand is usage display%s (%s)", x->name, usage_name(x->usage), e85 ? "" : " or national",
               e85 ? "X3.23-1985 STRING rule 2" : "2023 14.9.43.3 rule 1");
    if (is_numeric_sym(x)) cen_pin(x, "STRING");    /* its characters */
    if (!o->ref.rm && is_numeric_sym(x) && !is_int_item(x))
        die_at(o->line, "STRING: '%s' is numeric but not an integer without P (%s)", x->name, e85 ? "X3.23-1985 STRING rule 6" : "2023 14.9.43.3 rule 8");
}

/* [ON OVERFLOW statements] [NOT ON OVERFLOW statements], each a Block;
 * ON alone begins one only before OVERFLOW (an enclosing CALL's ON
 * EXCEPTION is not ours) */
static int parse_overflow_phrases(Phrases *ph)
{
    memset(ph, 0, sizeof *ph);
    if (at_word("overflow") || (at_word("on") && is_word(peek(1), "overflow"))) {
        accept_word("on"); expect_word("overflow");
        ph->has_on = 1; ph->on = parse_block();
    }
    if (at_word("not") && (is_word(peek(1), "overflow") || (is_word(peek(1), "on") && is_word(peek(2), "overflow")))) {
        advance(); accept_word("on"); expect_word("overflow");
        ph->has_not = 1; ph->not_on = parse_block();
    }
    return ph->has_on || ph->has_not;
}

static void parse_string_1(void);
static void parse_string(void)
{
    parse_string_1();
}
static void parse_string_1(void)
{
    Opnd srcs[MAXOPS]; Opnd delims[MAXOPS]; int has_delim[MAXOPS];
    int n = 0, pending = 0;
    for (;;) {
        while (at_operand() || at_word("function")) {
            if (n >= MAXOPS) die_at(cur()->line, "too many STRING sources");
            parse_operand(&srcs[n]);
            if (srcs[n].kind == O_EXPR) die_at(srcs[n].line, "a STRING source must be an item, a literal or a figurative constant");
            str_operand(&srcs[n]);
            has_delim[n] = 0; n++; pending++;
        }
        if (accept_word("delimited")) {
            accept_word("by");
            Opnd d; memset(&d, 0, sizeof d);
            if (accept_word("size")) d.kind = O_ALL;      /* stands for SIZE here */
            else { parse_operand(&d); str_operand(&d); if (d.kind != O_STR && d.kind != O_REF && d.kind != O_FIG) die_at(d.line, "DELIMITED BY needs SIZE, a literal or an item"); }
            no_zero_lit(&d, "STRING ... DELIMITED BY", "2023 14.9.43.3 rule 3");
            for (int i = n - pending; i < n; i++) { delims[i] = d; has_delim[i] = 1; }
            pending = 0;
            continue;
        }
        break;
    }
    if (!n) die_at(cur()->line, "STRING needs a source");
    /* DELIMITED BY is mandatory in the 1985 text; GnuCOBOL lets it be
     * omitted and takes SIZE, and taskdt does exactly that (dialect.md) */
    for (int i = 0; i < n; i++) if (!has_delim[i]) { memset(&delims[i], 0, sizeof delims[i]); delims[i].kind = O_ALL; has_delim[i] = 1; }
    expect_word("into");
    Ref dst; parse_ref(&dst); no_constrec_recv(&dst, "STRING INTO");
    if (dst.sym->dynl) die_at(dst.line, "STRING INTO '%s': a dynamic-length item as the STRING receiver is not implemented in this stage (MOVE and SET SIZE OF set its length)", dst.sym->name);
    /* the receiver: not edited, not JUSTIFIED (X3.23 6.24.2); a group is alphanumeric */
    if (!dst.sym->is_group && (dst.sym->pi.category == PIC_NUMERIC || dst.sym->pi.edited || dst.sym->just))
        die_at(dst.line, "the STRING receiver must be an alphanumeric item, not edited or JUSTIFIED");
    if (dst.user_rm) die_at(dst.line, "the STRING receiver shall not be reference-modified (2023 14.9.43.3 rule 4; X3.23-1985 STRING syntax rule 3)");
    if (dst.sym->strong) die_at(dst.line, "a strongly-typed group is not a STRING receiver (2023 14.9.43.3 rule 6)");
    /* national operands (cobol ISSUES-69): characters of two bytes throughout */
    int nat = ref_is_national(&dst);
    static const char *srule = "14.9.43.3 rule 1";
    for (int i = 0; i < n; i++) { no_bits(&srcs[i], "STRING"); no_bits(&delims[i], "STRING"); }
    { Opnd dq; memset(&dq, 0, sizeof dq); dq.kind = O_REF; dq.ref = dst; dq.line = dst.line; no_bits(&dq, "STRING"); }
    for (int i = 0; i < n; i++) {
        nat_class_check(&srcs[i], nat, "STRING", srule);
        if (delims[i].kind != O_ALL) nat_class_check(&delims[i], nat, "STRING", srule);   /* O_ALL: SIZE */
    }
    Ref ptr; int has_ptr = 0;
    if (accept_word("with")) { expect_word("pointer"); parse_ref(&ptr); has_ptr = 1; }
    else if (accept_word("pointer")) { parse_ref(&ptr); has_ptr = 1; }
    if (has_ptr) no_constrec_recv(&ptr, "STRING POINTER");
    /* the POINTER: an elementary numeric integer without P, able to hold
     * one more than the receiver's length (85 rule 5; 2023 rule 7) */
    if (has_ptr && (ptr.sym->is_group || !is_int_item(ptr.sym))) die_at(ptr.line, "the POINTER must be an integer item");
    if (has_ptr && !sym_notrunc(ptr.sym)) {
        int need = 1; for (long v = (dst.rm ? 0 : dst.sym->size / (nat ? 2 : 1)) + 1; v >= 10; v /= 10) need++;
        if (!dst.rm && ptr.sym->pi.digits < need)
            die_at(ptr.line, "the POINTER '%s' has %d digit%s; the receiver needs %d (%s)", ptr.sym->name, ptr.sym->pi.digits, ptr.sym->pi.digits == 1 ? "" : "s", need,
                   g_std < 2002 ? "X3.23-1985 STRING rule 5" : "2023 14.9.43.3 rule 7");
    }

    /* begin: receiver, its length, the pointer's value */
    if (has_ptr) {
        if (is_hot_int(ptr.sym)) { Opnd po; memset(&po, 0, sizeof po); po.kind = O_REF; po.ref = ptr; emit_hot_value(&po); }
        else { Arg a[2] = { arg_ref(&ptr), arg_desc(sym_desc(ptr.sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
        emit("\tstw sp+%d, r1", SLOT_C);
    }
    Arg b[2] = { arg_ref(&dst), arg_imm(dst.sym->size) };
    emit_args(b, 2);
    if (has_ptr) emit("\tldw r5, sp+%d", SLOT_C); else emit_li("r5", 1);
    emit_call(nat ? "cob_str_begin_nat" : "cob_str_begin");

    for (int i = 0; i < n; i++) {
        Arg a[4]; Arg dd;
        if (srcs[i].kind == O_FIG) fig_char_args(&srcs[i], nat, &a[0], &a[1]);   /* SPACE, ZERO, ...: one character */
        else if (srcs[i].kind == O_ALL) {
            a[0] = arg_label(lit_label((unsigned char *)srcs[i].tok->s, srcs[i].tok->len)); a[1] = arg_imm(srcs[i].tok->len);
        } else {
            emit_incompat(&srcs[i]);
            opnd_args(&srcs[i], &a[0], &dd, 0, 0);
            a[1] = arg_len(&srcs[i]);
        }
        Opnd *d = &delims[i];
        if (d->kind == O_ALL) { a[2] = arg_imm(0); a[3] = arg_imm(0); }
        else if (d->kind == O_FIG) fig_char_args(d, nat, &a[2], &a[3]);
        else { Arg x; opnd_args(d, &a[2], &x, 0, 0); a[3] = arg_len(d); }
        emit_args(a, 4);
        emit_call("cob_str_src");
    }
    if (has_ptr) {
        emit_call("cob_str_pointer");
        emit("\tstw sp+%d, r1", SLOT_C);
        Arg a[2] = { arg_ref(&ptr), arg_desc(sym_desc(ptr.sym)) };
        emit_args(a, 2);
        emit("\tldw r5, sp+%d", SLOT_C);
        emit_call("cob_store_int");
    }
    Phrases ph;
    emit_ec_query("EC-OVERFLOW-STRING", "cob_str_overflow", 1);       /* 2023 14.9.43.4 rule 8b; nonfatal: the phrase follows */
    if (parse_overflow_phrases(&ph)) { emit_call("cob_str_overflow"); emit_phrases(&ph, -1, 0); }
    accept_word("end-string");
}

/* UNSTRING src [DELIMITED BY [ALL] d [OR [ALL] d]...] INTO {r [DELIMITER IN
 * r] [COUNT IN r]}... [WITH POINTER p] [TALLYING IN t] [[NOT] ON OVERFLOW]
 * [END-UNSTRING]; the runtime does the scanning (cob_unstr_*) */
/* UNSTRING's sending item, delimiters and DELIMITER IN items are of
 * category alphanumeric or national (X3.23-1985 UNSTRING rule 2; 2023
 * 14.9.48.3 rule 2): a group or a reference-modified item qualifies */
static void unstr_alnum(const Ref *r, const char *role)
{
    Sym *x = r->sym;
    if (r->rm || x->is_group || sym_bitlike(x) || sym_is_national(x)) return;   /* no_bits, nat_class_check */
    int c = x->pi.category;
    if (x->usage == U_DISPLAY && c == PIC_ALPHANUMERIC) return;
    if (c == PIC_NATIONAL || (x->usage == U_NATIONAL && c != PIC_NUMERIC && c != PIC_NUMERIC_EDITED)) return;
    die_at(r->line, "UNSTRING: %s '%s' is %s; an alphanumeric%s item is required (%s)", role, x->name,
           x->usage != U_DISPLAY && x->usage != U_NATIONAL ? "not usage display" : pic_category_name(c), g_std < 2002 ? "" : " or national",
           g_std < 2002 ? "X3.23-1985 UNSTRING rule 2" : "2023 14.9.48.3 rule 2");
}

/* an UNSTRING receiver: usage display and alphabetic, alphanumeric or
 * numeric, or usage national and national or numeric; numeric without P
 * (85 rule 3; 2023 rule 4) */
static void unstr_receiver(const Ref *r)
{
    Sym *x = r->sym;
    if (r->rm || x->is_group || sym_bitlike(x) || sym_is_national(x)) return;   /* checked with the national rules */
    int c = x->pi.category, e85 = g_std < 2002;
    const char *rule = e85 ? "X3.23-1985 UNSTRING rule 3" : "2023 14.9.48.3 rule 4";
    int ok = x->usage == U_DISPLAY ? (c == PIC_ALPHABETIC || c == PIC_ALPHANUMERIC || c == PIC_NUMERIC)
           : x->usage == U_NATIONAL ? (c == PIC_NATIONAL || c == PIC_NUMERIC) : 0;
    if (ok && c == PIC_NUMERIC) cen_pin(x, "UNSTRING");     /* a receiver is usage display */
    if (!ok)
        die_at(r->line, "UNSTRING: the receiver '%s' is %s%s; a receiver is alphabetic, alphanumeric or numeric%s (%s)", x->name,
               x->usage != U_DISPLAY && x->usage != U_NATIONAL ? "USAGE " : "", x->usage != U_DISPLAY && x->usage != U_NATIONAL ? usage_name(x->usage) : pic_category_name(c),
               e85 ? ", usage display" : ", usage display or national", rule);
    if (c == PIC_NUMERIC && memchr(x->pi.pat, 'P', x->pi.patlen))
        die_at(r->line, "UNSTRING: the receiver '%s' has P in its picture (%s)", x->name, rule);
}

static void parse_unstring_1(void);
static void parse_unstring(void)
{
    parse_unstring_1();
}
static void parse_unstring_1(void)
{
    int line = cur()->line;
    Opnd src; parse_operand(&src);
    if (src.kind != O_REF) die_at(src.line, "UNSTRING needs a data item to take apart");
    if (g_std < 2002 && src.ref.user_rm)                /* 2023 dropped the rule */
        die_at(src.line, "the UNSTRING sending item shall not be reference-modified in COBOL 85 (X3.23-1985 UNSTRING syntax rule 7)");
    unstr_alnum(&src.ref, "the sending item");
    Opnd delims[16]; int dall[16]; int nd = 0;
    if (accept_word("delimited")) {
        accept_word("by");
        for (;;) {
            if (nd == 16) die_at(cur()->line, "UNSTRING: more than 16 delimiters");
            dall[nd] = accept_word("all");
            parse_operand(&delims[nd]);
            if (delims[nd].kind != O_STR && delims[nd].kind != O_REF && delims[nd].kind != O_FIG)
                die_at(delims[nd].line, "DELIMITED BY needs a nonnumeric literal or an item (%s)", g_std < 2002 ? "X3.23-1985 UNSTRING rule 1" : "2023 14.9.48.3 rule 1");
            if (delims[nd].kind == O_REF) unstr_alnum(&delims[nd].ref, "the delimiter");
            no_zero_lit(&delims[nd], "UNSTRING ... DELIMITED BY", "2023 14.9.48.3 rule 1");
            nd++;
            if (!accept_word("or")) break;
        }
    }
    expect_word("into");
    Ref rcv[MAXOPS], dlm[MAXOPS], cnt[MAXOPS]; int has_d[MAXOPS], has_c[MAXOPS], n = 0;
    while (at_operand() && cur()->kind == T_WORD && !at_word("with") && !at_word("pointer") && !at_word("tallying") && !at_word("on") && !at_word("overflow") && !at_word("not") && !at_word("end-unstring")) {
        if (n >= MAXOPS) die_at(cur()->line, "too many UNSTRING receivers");
        parse_ref(&rcv[n]); no_constrec_recv(&rcv[n], "UNSTRING INTO");
        if (rcv[n].sym->dynl) die_at(rcv[n].line, "UNSTRING INTO '%s': a dynamic-length item as an UNSTRING receiver is not implemented in this stage", rcv[n].sym->name);
        if (rcv[n].sym->is_cond) die_at(rcv[n].line, "'%s' is a condition-name", rcv[n].sym->name);
        if (rcv[n].sym->strong)                   /* its category is its type (8.5.2.1) */
            die_at(rcv[n].line, "the strongly-typed group '%s' is not an UNSTRING receiver (2023 14.9.48.3 rule 4)", rcv[n].sym->name);
        unstr_receiver(&rcv[n]);
        has_d[n] = has_c[n] = 0;
        for (;;) {
            if (accept_word("delimiter")) { accept_word("in"); parse_ref(&dlm[n]); has_d[n] = 1; no_constrec_recv(&dlm[n], "UNSTRING DELIMITER IN"); unstr_alnum(&dlm[n], "the DELIMITER IN item"); continue; }
            if (accept_word("count")) { accept_word("in"); parse_ref(&cnt[n]); has_c[n] = 1; no_constrec_recv(&cnt[n], "UNSTRING COUNT IN"); if (!is_int_item(cnt[n].sym)) die_at(cnt[n].line, "COUNT IN needs an integer item"); continue; }
            break;
        }
        if (has_d[n] && !nd) die_at(rcv[n].line, "DELIMITER IN without DELIMITED BY");
        if (has_c[n] && !nd) die_at(rcv[n].line, "COUNT IN without DELIMITED BY");
        n++;
    }
    if (!n) die_at(line, "UNSTRING needs a receiver after INTO");
    Ref ptr; int has_ptr = 0;
    if (accept_word("with")) { expect_word("pointer"); parse_ref(&ptr); has_ptr = 1; }
    else if (accept_word("pointer")) { parse_ref(&ptr); has_ptr = 1; }
    if (has_ptr) no_constrec_recv(&ptr, "UNSTRING POINTER");
    if (has_ptr && !is_int_item(ptr.sym)) die_at(ptr.line, "the POINTER must be an integer item");
    /* wide enough for one more than the sending item's length (85 rule 5; 2023 rule 6) */
    if (has_ptr && !sym_notrunc(ptr.sym) && !src.ref.rm) {
        int need = 1; for (long v = src.ref.sym->size / (opnd_is_national(&src) ? 2 : 1) + 1; v >= 10; v /= 10) need++;
        if (ptr.sym->pi.digits < need)
            die_at(ptr.line, "the POINTER '%s' has %d digit%s; the sending item needs %d (%s)", ptr.sym->name, ptr.sym->pi.digits, ptr.sym->pi.digits == 1 ? "" : "s", need,
                   g_std < 2002 ? "X3.23-1985 UNSTRING rule 5" : "2023 14.9.48.3 rule 6");
    }
    Ref tly; int has_tly = 0;
    if (accept_word("tallying")) { accept_word("in"); parse_ref(&tly); has_tly = 1; no_constrec_recv(&tly, "UNSTRING TALLYING"); if (!is_int_item(tly.sym)) die_at(tly.line, "TALLYING IN needs an integer item"); }
    /* national operands (cobol ISSUES-69): the source, the delimiters, the
     * receivers and DELIMITER IN items all national, or none */
    int nat = opnd_is_national(&src);
    static const char *urule = "14.9.48.3 rule 3";
    no_bits(&src, "UNSTRING");
    for (int i = 0; i < n; i++) { Opnd rq; memset(&rq, 0, sizeof rq); rq.kind = O_REF; rq.ref = rcv[i]; rq.line = rcv[i].line; no_bits(&rq, "UNSTRING"); }
    for (int i = 0; i < nd; i++) nat_class_check(&delims[i], nat, "UNSTRING", urule);
    for (int i = 0; i < n; i++) {
        Opnd ro; memset(&ro, 0, sizeof ro); ro.kind = O_REF; ro.ref = rcv[i]; ro.line = rcv[i].line;
        if (nat && is_numeric_sym(rcv[i].sym)) {
            if (rcv[i].sym->usage != U_NATIONAL)
                die_at(rcv[i].line, "UNSTRING: a numeric receiver of national data must be USAGE NATIONAL (2023 14.9.48.3 rule 4)");
        } else nat_class_check(&ro, nat, "UNSTRING", urule);
        if (nat && rcv[i].sym->pi.edited)
            die_at(rcv[i].line, "UNSTRING: a national-edited receiver is not allowed (2023 14.9.48.3 rule 4)");
        if (has_d[i]) { ro.ref = dlm[i]; ro.line = dlm[i].line; nat_class_check(&ro, nat, "UNSTRING", urule); }
    }

    /* begin: the source, its length, the pointer */
    if (has_ptr) {
        Arg a[2] = { arg_ref(&ptr), arg_desc(sym_desc(ptr.sym)) }; emit_args(a, 2); emit_call("cob_load_int");
        emit("\tstw sp+%d, r1", SLOT_C);
    }
    { Arg a[2], dd; opnd_args(&src, &a[0], &dd, 0, 0); a[1] = arg_len(&src); emit_args(a, 2); }
    if (has_ptr) emit("\tldw r5, sp+%d", SLOT_C); else emit_li("r5", 1);
    emit_call(nat ? "cob_unstr_begin_nat" : "cob_unstr_begin");
    for (int i = 0; i < nd; i++) {
        Arg a[3];
        if (delims[i].kind == O_FIG) fig_char_args(&delims[i], nat, &a[0], &a[1]);
        else { Arg x; opnd_args(&delims[i], &a[0], &x, 0, 0); a[1] = arg_len(&delims[i]); }
        a[2] = arg_imm(dall[i]);
        emit_args(a, 3);
        emit_call("cob_unstr_delim");
    }
    for (int i = 0; i < n; i++) {
        Arg a[6];
        a[0] = arg_ref(&rcv[i]); a[1] = arg_desc(sym_desc(rcv[i].sym));
        if (has_d[i]) { a[2] = arg_ref(&dlm[i]); a[3] = arg_desc(sym_desc(dlm[i].sym)); } else { a[2] = arg_imm(0); a[3] = arg_imm(0); }
        if (has_c[i]) { a[4] = arg_ref(&cnt[i]); a[5] = arg_desc(sym_desc(cnt[i].sym)); } else { a[4] = arg_imm(0); a[5] = arg_imm(0); }
        emit_args(a, 6);
        emit_call("cob_unstr_into");
    }
    if (has_ptr) {
        emit_call("cob_unstr_pointer");
        emit("\tstw sp+%d, r1", SLOT_C);
        Arg a[2] = { arg_ref(&ptr), arg_desc(sym_desc(ptr.sym)) };
        emit_args(a, 2);
        emit("\tldw r5, sp+%d", SLOT_C);
        emit_call("cob_store_int");
    }
    if (has_tly) {
        /* TALLYING IN is incremented by the receivers acted on */
        Arg a[2] = { arg_ref(&tly), arg_desc(sym_desc(tly.sym)) };
        emit_args(a, 2); emit_call("cob_load_int");
        emit("\tstw sp+%d, r1", SLOT_C);
        emit_call("cob_unstr_tally");
        emit("\tldw r2, sp+%d", SLOT_C);
        emit("\tadd r1, r1, r2");
        emit("\tstw sp+%d, r1", SLOT_C);
        emit_args(a, 2);
        emit("\tldw r5, sp+%d", SLOT_C);
        emit_call("cob_store_int");
    }
    Phrases ph;
    emit_ec_query("EC-OVERFLOW-UNSTRING", "cob_unstr_overflow", 1);   /* 2023 14.9.48.4 */
    if (parse_overflow_phrases(&ph)) { emit_call("cob_unstr_overflow"); emit_phrases(&ph, -1, 0); }
    accept_word("end-unstring");
}
