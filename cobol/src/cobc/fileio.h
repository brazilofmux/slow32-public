/* s32-cobc: OPEN, CLOSE, READ, WRITE.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- files: OPEN, CLOSE, READ, WRITE ---------------------------------- */

static void emit_file_addr(const char *reg, File *f)
{
    char lab[32]; snprintf(lab, sizeof lab, ".Lf%s%d_%d", f->external ? "x" : "", f->unit, (int)(f - g_files));
    emit_la(reg, lab);
    if (f->external) emit("\tldw %s, %s+0", reg, reg);      /* the shared connector, from cob_ext_file_enter */
}

static File *expect_file(void)
{
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a file-name, found %s", tok_desc(t));
    File *f = file_find(t->s);
    if (!f) die_at(t->line, "'%s' is not a file (no SELECT)", t->s);
    advance();
    return f;
}

static void parse_open(void)
{
    int n = 0;
    for (;;) {
        int mode;
        if (cur()->kind == T_WORD && file_find(cur()->s) && file_find(cur()->s)->org == COB_ORG_SORT)
            die_at(cur()->line, "'%s' is a sort file (SD); SORT opens it", cur()->s);
        if (accept_word("input")) mode = COB_OPEN_INPUT;
        else if (accept_word("output")) mode = COB_OPEN_OUTPUT;
        else if (accept_word("i-o")) mode = COB_OPEN_IO;
        else if (accept_word("extend")) mode = COB_OPEN_EXTEND;
        else break;
        while (cur()->kind == T_WORD && !at_word("input") && !at_word("output") && !at_word("i-o") &&
               !at_word("extend") && !is_verb(cur()->s) && !is_terminator(cur()->s)) {
            int fline = cur()->line;
            File *f = expect_file();
            int reversed = 0, e85 = g_std < 2002, seq = f->org == COB_ORG_SEQ || f->org == COB_ORG_LINESEQ;
            if (f->report_name[0] && (mode == COB_OPEN_INPUT || mode == COB_OPEN_IO))
                die_at(fline, "OPEN %s '%s': a report file is opened OUTPUT or EXTEND (%s)", mode == COB_OPEN_INPUT ? "INPUT" : "I-O", f->name,
                       e85 ? "X3.23-1985 Report Writer OPEN format" : "2023 14.9.27.3 rule 1");
            if (mode == COB_OPEN_EXTEND && (f->linage || f->access))
                die_at(fline, "OPEN EXTEND '%s': EXTEND is for a file in sequential access mode without LINAGE (%s)", f->name,
                       !e85 ? "2023 14.9.27.3 rule 2" : f->linage ? "X3.23-1985 sequential OPEN syntax rule 3" : "X3.23-1985 relative and indexed OPEN syntax rule 1");
            /* [WITH] NO REWIND, [WITH] LOCK: WITH is optional (the
             * X3.23-1985 and 2023 OPEN formats); OPEN OUTPUT f NO REWIND
             * was refused as "'no' is not a file" */
            if (accept_word("with") || (at_word("no") && is_word(peek(1), "rewind")) || at_word("lock")) {
                if (accept_word("no")) {
                    accept_word("rewind");
                    if (!seq || (mode != COB_OPEN_INPUT && mode != COB_OPEN_OUTPUT))
                        die_at(fline, "OPEN ... NO REWIND '%s': for a sequential file opened INPUT or OUTPUT (%s)", f->name,
                               e85 ? "the X3.23-1985 OPEN formats" : "2023 14.9.27.3 rules 5-6");
                }
                accept_word("lock");
            }
            if (at_word("reversed")) bp(BP_O4_REVERSED, cur()->line);
            if (accept_word("reversed")) {          /* obsolete: read from the last record back (SQ303M, SQ401M) */
                if (mode != COB_OPEN_INPUT) die_at(cur()->line, "REVERSED goes with OPEN INPUT");
                reversed = 8;
            }
            emit_file_addr("r3", f); emit_li("r4", mode | reversed); emit_call("cob_open");
            emit("\tstw sp+%d, r1", SLOT_C); emit_use_dispatch(f, 0);
            n++;
        }
    }
    if (!n) die_at(cur()->line, "OPEN needs INPUT, OUTPUT, I-O or EXTEND and a file-name");
}

static void parse_close(void)
{
    int n = 0;
    while (cur()->kind == T_WORD && !is_verb(cur()->s) && !is_terminator(cur()->s)) {
        File *f = expect_file();
        int lock = 0, seq = f->org == COB_ORG_SEQ || f->org == COB_ORG_LINESEQ;
        const char *crule = g_std < 2002 ? "the X3.23-1985 relative and indexed CLOSE format" : "2023 14.9.6.3 rule 1";
        accept_word("with");
        if (accept_word("no")) { accept_word("rewind"); if (!seq) die_at(cur()->line, "CLOSE ... NO REWIND '%s': for a sequential file (%s)", f->name, crule); }
        if (!seq && (at_word("reel") || at_word("unit"))) die_at(cur()->line, "CLOSE %s '%s': for a sequential file (%s)", cur()->s, f->name, crule);
        if (accept_word("lock")) lock = 1;
        if (lock) { emit_file_addr("r3", f); emit_call("cob_close_lock"); emit("\tstw sp+%d, r1", SLOT_C); emit_use_dispatch(f, 0); n++; continue; }
        if (accept_word("reel") || accept_word("unit")) {
            /* closes a reel, not the file; a disk file has one reel, so the
             * runtime only reports 07 (successful, no reel) */
            if (accept_word("for")) accept_word("removal");
            if (accept_word("with")) { accept_word("no"); accept_word("rewind"); }
            emit_file_addr("r3", f); emit_call("cob_close_reel");
            emit("\tstw sp+%d, r1", SLOT_C); emit_use_dispatch(f, 0);
            n++; continue;
        }
        emit_file_addr("r3", f); emit_call("cob_close");
        emit("\tstw sp+%d, r1", SLOT_C); emit_use_dispatch(f, 0);
        n++;
    }
    if (!n) die_at(cur()->line, "CLOSE needs a file-name");
}

/* [NOT] INVALID KEY / [NOT] AT END after a keyed verb, on the result in
 * SLOT_C: 0 done, 1 the condition, 2 an error already reported */
static void emit_use_dispatch(File *f, int has_clause);
static void io_phrase_required(File *f, const char *w, int line);

static void parse_condition_clauses(const char *w1, const char *w2, const char *end_word)
{
    int Lend = new_label();
    int has_clause = at_word(w1) || at_word(w2);
    if (g_io_file) emit_use_dispatch(g_io_file, has_clause);
    if (at_word(w1) || at_word(w2)) {
        /* AT END / INVALID KEY: AT and KEY may be omitted */
        if (accept_word(w1)) accept_word(w2); else advance();
        int Lnot = new_label();
        emit("\tldw r1, sp+%d", SLOT_C);
        emit_li("r2", 1);
        emit("\tbne r1, r2, .L%d", Lnot);
        parse_statements();
        emit_jump(Lend);
        emit_label(Lnot);
    }
    if (at_word("not") && (is_word(peek(1), w1) || is_word(peek(1), w2))) {
        advance();
        if (accept_word(w1)) accept_word(w2); else advance();
        emit("\tldw r1, sp+%d", SLOT_C);
        emit("\tbne r1, r0, .L%d", Lend);
        parse_statements();
    }
    emit_label(Lend);
    accept_word(end_word);
}

/* which key of an indexed file a data item names: 0 the RECORD KEY, i the
 * i-th ALTERNATE, -1 none.  An item that begins where a key begins and is
 * no longer is a leading part of it (START on a partial key): *len is
 * then the item's size. */
static int file_key_index(File *f, Sym *s, int *len)
{
    *len = 0;
    if (s == f->key_sym) return 0;
    for (int a = 0; a < f->nalt; a++) if (s == f->alt[a].sym) return a + 1;
    if (f->rec < 0 || s->record != g_sym[f->rec].record || s->ndims) return -1;
    if (f->key_sym && s->offset == f->key_sym->offset && s->size <= f->key_sym->size) { *len = s->size; return 0; }
    for (int a = 0; a < f->nalt; a++)
        if (s->offset == f->alt[a].sym->offset && s->size <= f->alt[a].sym->size) { *len = s->size; return a + 1; }
    return -1;
}

static void parse_read(void)
{
    File *f = expect_file();
    if (f->org == COB_ORG_SORT) die_at(cur()->line, "READ of the sort file '%s': use RETURN inside the OUTPUT PROCEDURE", f->name);
    int has_prev = 0;
    if (at_word("previous")) {
        /* READ PREVIOUS (COBOL 2002): a sequential read backwards, an
         * indexed or relative file in sequential or dynamic access
         * (2023 14.9.30.3 rules 6-7) */
        if (g_std < 2002) die_at(cur()->line, "READ PREVIOUS is COBOL 2002; compile with -std=2002");
        if (f->org == COB_ORG_LINESEQ) die_at(cur()->line, "READ PREVIOUS of the LINE SEQUENTIAL file '%s' (2023 14.9.30.3 rule 7)", f->name);
        if (f->access == 1) die_at(cur()->line, "READ PREVIOUS of '%s', whose access mode is RANDOM (2023 14.9.30.3 rule 6)", f->name);
        if (f->org != COB_ORG_INDEXED && f->org != COB_ORG_RELATIVE)
            die_at(cur()->line, "READ PREVIOUS of the sequential file '%s' is not implemented", f->name);
        advance(); has_prev = 1;
    }
    int has_next = !has_prev && accept_word("next"); accept_word("record");
    Ref into; int has_into = 0;
    if (accept_word("into")) {
        parse_ref(&into); has_into = 1;
        if (f->rec >= 0 && into.sym->record == g_sym[f->rec].record)
            die_at(into.line, "READ ... INTO '%s': the item is the file's own record area (X3.23-1985 READ syntax rule 1)", into.sym->name);
        /* several record descriptions: INTO and all of them alphanumeric (2023 rule 1) */
        int nrec = 0, alnum = 1;
        for (int j = 0; j < g_nsym; j++)
            if (g_sym[j].level == 1 && f->rec >= 0 && g_sym[j].fd >= 0 && g_sym[j].fd == g_sym[f->rec].fd) {
                nrec++;
                Sym *q = &g_sym[j];
                if (!q->is_group && q->pi.category != PIC_ALPHANUMERIC && q->pi.category != PIC_NATIONAL) alnum = 0;
            }
        Sym *q = into.sym;
        if (!into.rm && !q->is_group && q->pi.category != PIC_ALPHANUMERIC && q->pi.category != PIC_NATIONAL) alnum = 0;
        if (g_std >= 2002 && nrec > 1 && !alnum)
            die_at(into.line, "READ %s INTO '%s': with several record descriptions, the INTO item and every record are alphanumeric (2023 14.9.30.3 rule 1)", f->name, into.sym->name);
    }
    int keyed = 0, ki = 0;
    if (accept_word("key")) {
        accept_word("is");
        Ref k; parse_ref(&k);
        if (f->org == COB_ORG_RELATIVE) die_at(k.line, "READ ... KEY IS is for INDEXED files; a RELATIVE file reads the record its RELATIVE KEY names");
        if (f->org != COB_ORG_INDEXED) die_at(k.line, "READ ... KEY needs an INDEXED file");
        int klen; ki = file_key_index(f, k.sym, &klen);
        if (ki < 0 || klen) die_at(k.line, "READ ... KEY IS '%s': not the RECORD KEY or an ALTERNATE RECORD KEY of '%s'", k.sym->name, f->name);
        keyed = 1;
    }
    if (has_prev && keyed) die_at(cur()->line, "READ PREVIOUS names no KEY (2023 14.9.30 format 1)");
    has_next |= has_prev;                       /* a sequential read, backwards */
    if (f->org == COB_ORG_INDEXED) {
        if (has_next && keyed) die_at(cur()->line, "READ NEXT cannot name a KEY");
        if (!has_next && !keyed && f->access != 0) keyed = 1;         /* ACCESS RANDOM or DYNAMIC: a READ without NEXT is by the prime key */
        if (has_next && f->access == 1) die_at(cur()->line, "READ NEXT needs ACCESS SEQUENTIAL or DYNAMIC");
        if (keyed && f->access == 0) die_at(cur()->line, "READ ... KEY needs ACCESS RANDOM or DYNAMIC");
    } else if (f->org == COB_ORG_RELATIVE) {
        /* random or dynamic access: a READ without NEXT is by the RELATIVE KEY */
        if (has_next && f->access == 1) die_at(cur()->line, "READ NEXT needs ACCESS SEQUENTIAL or DYNAMIC");
        if (!has_next && f->access != 0) keyed = 1;
    } else if (keyed) die_at(cur()->line, "READ ... KEY needs an INDEXED file");

    g_io_file = f;
    emit_file_addr("r3", f); emit_li("r4", ki);
    emit_call(keyed ? "cob_read_key" : has_prev ? "cob_read_prev" : "cob_read");
    emit("\tstw sp+%d, r1", SLOT_C);
    if (has_into) {
        int Lskip = new_label();
        emit("\tbne r1, r0, .L%d", Lskip);
        Opnd src; memset(&src, 0, sizeof src); src.kind = O_REF; src.line = into.line;
        src.ref.sym = &g_sym[f->rec]; src.ref.line = into.line;
        emit_move(&src, &into);
        emit_label(Lskip);
    }
    if (keyed) {
        if (at_word("at")) die_at(cur()->line, "a READ by key takes INVALID KEY, not AT END");
        io_phrase_required(f, "invalid", cur()->line);
        parse_condition_clauses("invalid", "key", "end-read");
    } else {
        if (at_word("invalid")) die_at(cur()->line, "a sequential READ takes AT END, not INVALID KEY");
        if (!at_word("end")) io_phrase_required(f, "at", cur()->line);
        parse_condition_clauses("at", "end", "end-read");
    }
}

static void parse_write(void)
{
    Ref rec; parse_ref(&rec);
    File *f = file_of_record(rec.sym, rec.line);
    if (f->org == COB_ORG_SORT) die_at(rec.line, "WRITE to the sort file '%s': use RELEASE inside the INPUT PROCEDURE", f->name);
    if (accept_word("from")) {
        Opnd src; parse_operand(&src);
        emit_move(&src, &rec);
    }
    int before = 0, after = 0, after_kw = 0; Opnd n; int dyn = 0, adv = 0;
    if (at_word("before") || at_word("after")) {
        adv = 1;
        after_kw = accept_word("after"); if (!after_kw) accept_word("before");
        accept_word("advancing");
        if (accept_word("page") || (cur()->kind == T_WORD && mnemonic_kind(cur()->s) == 3 && (advance(), 1))) {
            /* a form feed before (AFTER PAGE) or after (BEFORE PAGE) the record */
            if (after_kw) before = -1; else after = -1;
            accept_word("line"); accept_word("lines");
            goto advancing_done;
        }
        parse_operand(&n);
        if (n.kind == O_FIG && !strncmp(n.tok->s, "zero", 4)) { n.kind = O_NUM; numlit_zero(&n.num); }   /* ADVANCING ZERO (SQ101M) */
        if (n.kind == O_NUM) {
            long v = (long)numlit_int(&n.num);
            /* the runtime's counts are the newlines beyond the record's own; PAGE is -1, ZERO lines -2 (no advance at all) */
            if (f->linage) { if (after_kw) before = (int)v; else after = (int)v; }
            else if (after_kw) before = v ? (int)v - 1 : -2; else after = v ? (int)v - 1 : -2;
        }
        else if (n.kind == O_REF && is_int_item(n.ref.sym)) dyn = 1;
        else die_at(n.line, "ADVANCING needs an integer");
        accept_word("line"); accept_word("lines");
    }
advancing_done:;
    /* a BEFORE phrase on a print file (not LINAGE, which counts its own):
     * before = -3 marks it, so BEFORE 1 is not taken for AFTER 1 -- the
     * runtime's printer needs to know which side of the record the move
     * falls on (libcob.c; cobol ISSUES-46) */
    if (adv && !after_kw && !f->linage) before = -3;
    /* a file written WITH ADVANCING and no ORGANIZATION clause is a print
     * file: its records are lines (GnuCOBOL's "line advancing" file).  The
     * phrase decides, not its count: AFTER 1 is zero newlines beyond the
     * record's own, so testing the counts left a file written only AFTER 1
     * a plain sequential file with no line breaks at all (CCVS-85 NC113M;
     * cobol ISSUES-44) */
    if (adv && f->org == COB_ORG_SEQ && !f->org_given && !f->varying) f->org = COB_ORG_LINESEQ;
    /* (a LINAGE file took the line counts themselves above, not n-1: AFTER n
     * in r4, BEFORE n in r5, -1 for PAGE, 0/0 for no ADVANCING) */
    int keyed_org = f->org == COB_ORG_INDEXED || f->org == COB_ORG_RELATIVE;
    if (keyed_org && adv) die_at(rec.line, "ADVANCING is not valid on an %s file", f->org == COB_ORG_INDEXED ? "INDEXED" : "RELATIVE");
    if (!keyed_org && at_word("invalid")) die_at(cur()->line, "INVALID KEY needs an INDEXED or RELATIVE file");
    if (dyn) {
        if (is_hot_int(n.ref.sym)) emit_hot_value(&n);
        else { Arg a[2] = { arg_ref(&n.ref), arg_desc(sym_desc(n.ref.sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
        /* the runtime's counts are n-1, and zero lines is -2: a zero in the
         * item must not become -1, which is PAGE (SQ101M's LONG-ZERO) */
        if (!f->linage) { emit("\tseq r2, r1, r0"); emit("\taddi r1, r1, -1"); emit("\tsub r1, r1, r2"); }
        emit("\tstw sp+%d, r1", SLOT_C);
        emit_file_addr("r3", f);
        if (after_kw) { emit("\tldw r4, sp+%d", SLOT_C); emit_li("r5", 0); }
        else { emit_li("r4", f->linage ? 0 : -3); emit("\tldw r5, sp+%d", SLOT_C); }
    } else {
        emit_file_addr("r3", f); emit_li("r4", before); emit_li("r5", after);
    }
    emit_li("r6", rec.sym->size);          /* the 01 named: a mode-V record's length */
    emit_call("cob_write");
    emit("\tstw sp+%d, r1", SLOT_C);
    g_io_file = f;
    if (!f->linage && (at_word("eop") || at_word("end-of-page") || (at_word("at") && (is_word(peek(1), "eop") || is_word(peek(1), "end-of-page"))) ||
                       (at_word("not") && (is_word(peek(1), "eop") || is_word(peek(1), "end-of-page") || is_word(peek(1), "at")))))
        die_at(cur()->line, "END-OF-PAGE on '%s', whose FD has no LINAGE clause (%s)", f->name, g_std < 2002 ? "X3.23-1985 sequential WRITE syntax rule 8" : "2023 14.9.51.3 rule 19");
    if (f->linage && (before == -1 || after == -1) && (at_word("eop") || at_word("end-of-page") || (at_word("at") && !is_word(peek(1), "end")) || (at_word("not") && !is_word(peek(1), "invalid"))))
        die_at(cur()->line, "ADVANCING PAGE and END-OF-PAGE in one WRITE (%s)", g_std < 2002 ? "X3.23-1985 sequential WRITE syntax rule 7" : "2023 14.9.51.3 rule 18");
    if (keyed_org) { io_phrase_required(f, "invalid", cur()->line); parse_condition_clauses("invalid", "key", "end-write"); }
    else if (f->linage) {
        emit_use_dispatch(f, 0);
        /* [NOT] [AT] END-OF-PAGE (EOP): the runtime's verdict on this WRITE */
        for (int j = g_tp; j < g_ntok && g_tok[j].kind != T_PERIOD && !is_word(&g_tok[j], "end-write"); j++)
            if (is_word(&g_tok[j], "eop")) { free(g_tok[j].s); g_tok[j].s = xstrndup("end-of-page", 11); }
        if (at_word("at") || at_word("end-of-page") || (at_word("not") && (is_word(peek(1), "at") || is_word(peek(1), "end-of-page")))) {
            emit_file_addr("r3", f);
            emit("\tldw r1, r3+%d", COB_FILE_LIN_COUNTER_OFF + 4);    /* lin_eop */
            emit("\tstw sp+%d, r1", SLOT_C);
            g_io_file = NULL;
            parse_condition_clauses("at", "end-of-page", "end-write");
        } else accept_word("end-write");
    }
    else { emit_use_dispatch(f, 0); accept_word("end-write"); }
}

/* REWRITE record [FROM x] [INVALID KEY ...] */
static void parse_rewrite(void)
{
    Ref rec; parse_ref(&rec);
    File *f = file_of_record(rec.sym, rec.line);
    if (f->org == COB_ORG_LINESEQ) die_at(rec.line, "REWRITE is not valid on a LINE SEQUENTIAL file");
    if (accept_word("from")) { Opnd src; parse_operand(&src); emit_move(&src, &rec); }
    emit_file_addr("r3", f); emit_li("r4", rec.sym->size);
    emit_call("cob_rewrite");
    emit("\tstw sp+%d, r1", SLOT_C);
    g_io_file = f;
    if (f->org == COB_ORG_RELATIVE && f->access == 0 && (at_word("invalid") || (at_word("not") && is_word(peek(1), "invalid"))))
        die_at(cur()->line, "REWRITE of the relative file '%s' in sequential access takes no INVALID KEY (%s)", f->name,
               g_std < 2002 ? "X3.23-1985 relative REWRITE syntax rule 3" : "2023 14.9.35.3 rule 2");
    if (f->org == COB_ORG_INDEXED || (f->org == COB_ORG_RELATIVE && f->access)) io_phrase_required(f, "invalid", cur()->line);
    if (f->org == COB_ORG_INDEXED || f->org == COB_ORG_RELATIVE) parse_condition_clauses("invalid", "key", "end-rewrite");
    else { if (at_word("invalid")) die_at(cur()->line, "INVALID KEY needs an INDEXED or RELATIVE file"); emit_use_dispatch(f, 0); accept_word("end-rewrite"); }
}

/* DELETE file [RECORD] [INVALID KEY ...] */
static void parse_delete(void)
{
    File *f = expect_file();
    accept_word("record");
    if (f->org != COB_ORG_INDEXED && f->org != COB_ORG_RELATIVE) die_at(cur()->line, "DELETE needs an INDEXED or RELATIVE file");
    if (f->access == 0 && (at_word("invalid") || (at_word("not") && is_word(peek(1), "invalid"))))
        die_at(cur()->line, "DELETE '%s' in sequential access takes no INVALID KEY (%s)", f->name,
               g_std < 2002 ? "X3.23-1985 DELETE syntax rule 1" : "2023 14.9.10.3 rule 2");
    if (f->access) io_phrase_required(f, "invalid", cur()->line);
    emit_file_addr("r3", f);
    emit_call("cob_delete");
    emit("\tstw sp+%d, r1", SLOT_C);
    g_io_file = f;
    parse_condition_clauses("invalid", "key", "end-delete");
}

/* START file [KEY IS relation key] [INVALID KEY ...] */
static void parse_start(void)
{
    File *f = expect_file();
    if (f->org != COB_ORG_INDEXED && f->org != COB_ORG_RELATIVE) die_at(cur()->line, "START needs an INDEXED or RELATIVE file");
    if (f->access == 1) die_at(cur()->line, "START needs ACCESS SEQUENTIAL or DYNAMIC");
    int op = 0;                     /* = */
    int ki = 0, klen = 0;           /* the key: prime, or an alternate; a leading part's length */
    if (accept_word("key")) {
        accept_word("is");
        int neg = 0;
        if (accept_word("not")) neg = 1;
        if (at_op("=") || at_word("equal") || at_word("equals")) { advance(); accept_word("to"); op = 0; }
        else if (at_op(">") || at_word("greater")) { advance(); accept_word("than"); op = 1; if (accept_word("or")) { expect_word("equal"); accept_word("to"); op = 2; } }
        else if (at_op(">=")) { advance(); op = 2; }
        else if (at_op("<") || at_word("less")) { advance(); accept_word("than"); op = 3; if (accept_word("or")) { expect_word("equal"); accept_word("to"); op = 4; } }
        else if (at_op("<=")) { advance(); op = 4; }
        else die_at(cur()->line, "expected a relation in START ... KEY IS");
        if (neg) { if (op == 3) op = 2; else if (op == 1) op = 4; else die_at(cur()->line, "START KEY IS NOT takes LESS or GREATER"); }
        Ref k; parse_ref(&k);
        if (f->org == COB_ORG_RELATIVE) { if (k.sym != f->relkey_sym) die_at(k.line, "START ... KEY IS '%s': a RELATIVE file starts on its RELATIVE KEY", k.sym->name); }
        else {
            ki = file_key_index(f, k.sym, &klen);
            if (ki < 0) die_at(k.line, "START ... KEY IS '%s': not a key of '%s', nor an item that begins where one begins", k.sym->name, f->name);
        }
    }
    emit_file_addr("r3", f);
    emit_li("r4", op); emit_li("r5", ki); emit_li("r6", klen);
    emit_call("cob_start");
    emit("\tstw sp+%d, r1", SLOT_C);
    g_io_file = f;
    parse_condition_clauses("invalid", "key", "end-start");
}
