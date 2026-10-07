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
    /* a sort or merge file is SORT's and MERGE's, RELEASE's and RETURN's;
     * no input-output statement names it (2023 13.4.6.3 rule 3) */
    if (f->org == COB_ORG_SORT && strcmp(g_cur_stmt, "SORT") && strcmp(g_cur_stmt, "MERGE") && strcmp(g_cur_stmt, "RELEASE") && strcmp(g_cur_stmt, "RETURN") && strcmp(g_cur_stmt, "USE"))
        die_at(t->line, "%s of the sort file '%s': a sort or merge file is named only by SORT, MERGE, RELEASE and RETURN (2023 13.4.6.3 rule 3)", g_cur_stmt, f->name);
    advance();
    return f;
}

/* The record-locking and retry phrases of the I-O statements (2023
 * 14.7.9 RETRY, 9.1.16 record locking; optional since 2014): refused by
 * name where they would stand, not met as "not a COBOL verb". */
static void io_nyi(const char *stmt)
{
    if ((at_word("with") && (is_word(peek(1), "lock") || is_word(peek(1), "no"))) || at_word("lock") ||
        (at_word("advancing") && is_word(peek(1), "on")) || (at_word("ignoring") && is_word(peek(1), "lock")))
        die_at(cur()->line, "%s with a record-locking phrase is not implemented (file sharing and record locking, 2023 9.1.15-16)", stmt);
    if (at_word("retry"))
        die_at(cur()->line, "%s ... RETRY is not implemented (2023 14.7.9)", stmt);
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
        if (at_word("sharing") || at_word("retry"))
            die_at(cur()->line, "OPEN ... %s is not implemented (file sharing, 2023 9.1.15; RETRY, 14.7.9)", at_word("sharing") ? "SHARING" : "RETRY");
        while (cur()->kind == T_WORD && !at_word("input") && !at_word("output") && !at_word("i-o") &&
               !at_word("extend") && !is_verb(cur()->s) && !is_terminator(cur()->s)) {
            int fline = cur()->line;
            File *f = expect_file();
            if (at_word("sharing") || at_word("retry"))
                die_at(cur()->line, "OPEN ... %s is not implemented (file sharing, 2023 9.1.15; RETRY, 14.7.9)", at_word("sharing") ? "SHARING" : "RETRY");
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
        int norewind = 0;
        if (accept_word("no")) { accept_word("rewind"); norewind = 1; if (!seq) die_at(cur()->line, "CLOSE ... NO REWIND '%s': for a sequential file (%s)", f->name, crule); }
        if (!seq && (at_word("reel") || at_word("unit"))) die_at(cur()->line, "CLOSE %s '%s': for a sequential file (%s)", cur()->s, f->name, crule);
        if (at_word("lock")) bp(BP_R3_CLOSE_WITH_LOCK, cur()->line);
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
        if (ec_on_name("EC-REPORT-NOT-TERMINATED"))
            for (int ri = g_report_base; ri < g_nreport; ri++)
                if (g_reports[ri].file == (int)(f - g_files)) {
                    emit_report_addr("r3", &g_reports[ri]);
                    emit_ec_query("EC-REPORT-NOT-TERMINATED", "cob_rw_active", 1);   /* the file closed with its report active (2023 14.9.6.4) */
                }
        emit_file_addr("r3", f); emit_call(norewind && g_std >= 2014 ? "cob_close_norewind" : "cob_close");
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
    int has_clause = at_word(w1) || at_word(w2);
    if (g_f14[F14_IODECL] && !has_clause && !(at_word("not") && (is_word(peek(1), w1) || is_word(peek(1), w2)))) {
        /* FLAG-14 I-O-DECLARATIVE (7.3.15.4 rule 4d): the phrase left out while
         * a declarative for an open mode could take the condition -- INPUT
         * or I-O for AT END, any for INVALID KEY */
        int at_end = !strcmp(w1, "at");
        for (int i = 0; i < g_nuse; i++)
            if (g_use[i].ec < 0 && g_use[i].mode && (!at_end || g_use[i].mode == COB_OPEN_INPUT || g_use[i].mode == COB_OPEN_IO)) {
                f14(F14_IODECL, cur()->line, "%s without %s while a USE procedure for an open mode is declared: 2023 runs the procedure for the condition (E.2 item 8)",
                    at_end ? "READ" : "the statement", at_end ? "AT END" : "INVALID KEY");
                break;
            }
    }
    if (g_io_file) emit_use_dispatch(g_io_file, has_clause);
    Phrases ph; memset(&ph, 0, sizeof ph);
    if (at_word(w1) || at_word(w2)) {
        /* AT END / INVALID KEY: AT and KEY may be omitted */
        if (accept_word(w1)) accept_word(w2); else advance();
        ph.has_on = 1; ph.on = parse_block();
    }
    if (at_word("not") && (is_word(peek(1), w1) || is_word(peek(1), w2))) {
        advance();
        if (accept_word(w1)) accept_word(w2); else advance();
        ph.has_not = 1; ph.not_on = parse_block();
    }
    emit_phrases(&ph, SLOT_C, 1);
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
        f14(F14_READPREV, cur()->line, "READ PREVIOUS: 2023 changed its positioning after a START (E.2 item 16)");
        if (f->org == COB_ORG_LINESEQ) die_at(cur()->line, "READ PREVIOUS of the LINE SEQUENTIAL file '%s' (2023 14.9.30.3 rule 7)", f->name);
        if (f->access == 1) die_at(cur()->line, "READ PREVIOUS of '%s', whose access mode is RANDOM (2023 14.9.30.3 rule 6)", f->name);
        if (f->org == COB_ORG_SEQ && f->varying)
            die_at(cur()->line, "READ PREVIOUS of the sequential file '%s', whose records are of variable length: its records have no fixed place to step back to (a ruling; 2023 14.9.30)", f->name);
        advance(); has_prev = 1;
    }
    int has_next = !has_prev && accept_word("next"); accept_word("record");
    Ref into; int has_into = 0;
    if (f->implicit_rec && !at_word("into"))
        die_at(cur()->line, "READ %s: the file has no record description entry, so READ takes an INTO phrase (2023 13.4.5.3 rule 3c)", f->name);
    if (accept_word("into")) {
        g_noemit++; parse_ref(&into); g_noemit--;     /* identified after the record is read (2023 14.9.30.4) */
        has_into = 1;
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
    io_nyi("READ");
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
    io_nyi("READ");
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
        recv_calls(&into);
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

/* WRITE FILE file-name FROM ... (14.9.51 rules 1, 7), REWRITE FILE
 * likewise (14.9.35): the file's record area, which an FD with no record
 * description has as a FILLER record (13.4.5.3 rule 3b) */
static File *parse_file_phrase(Ref *rec, const char *verb)
{
    int line = cur()->line;
    advance();
    if (cur()->kind != T_WORD) die_at(line, "%s FILE needs a file-name", verb);
    File *f = file_find(cur()->s);
    if (!f) die_at(line, "%s FILE: '%s' is not a file", verb, cur()->s);
    advance();
    if (f->rec < 0) die_at(line, "%s FILE %s: the file has no record area", verb, f->name);
    memset(rec, 0, sizeof *rec); rec->sym = &g_sym[f->rec]; rec->line = line; rec->rm_lx = NULL;
    if (!at_word("from")) die_at(cur()->line, "%s FILE %s takes a FROM phrase (2023 14.9.51.3 rule 7)", verb, f->name);
    return f;
}
static void parse_write(void)
{
    Ref rec; File *f;
    if (at_word("file") && !sym_lookup_quiet("file")) f = parse_file_phrase(&rec, "WRITE");
    else {
        parse_ref(&rec);
        f = file_of_record(rec.sym, rec.line);
    }
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
    if (at_word("before") || at_word("after")) {
        /* the other phrase too (2023 14.9.51 format; rule 17: not with
         * PAGE): AFTER n moves before the record, BEFORE m after it.  A
         * LINAGE file's write takes both counts; a print file's gets the
         * BEFORE count by cob_write_also_before */
        if (g_std < 2023) die_at(cur()->line, "WRITE with both BEFORE and AFTER ADVANCING is COBOL 2023 (14.9.51); compile with -std=2023");
        int second_after = accept_word("after"); if (!second_after) accept_word("before");
        if (second_after == after_kw) die_at(cur()->line, "WRITE: the %s phrase twice", second_after ? "AFTER" : "BEFORE");
        if (before == -1 || after == -1 || at_word("page")) die_at(cur()->line, "WRITE: BEFORE and AFTER together, not with PAGE (2023 14.9.51.3 rule 17)");
        accept_word("advancing");
        if (dyn) die_at(cur()->line, "WRITE with both BEFORE and AFTER ADVANCING: the counts are integer literals here");
        Opnd m; parse_operand(&m);
        if (m.kind == O_FIG && !strncmp(m.tok->s, "zero", 4)) { m.kind = O_NUM; numlit_zero(&m.num); }
        if (m.kind != O_NUM) die_at(m.line, "WRITE with both BEFORE and AFTER ADVANCING: the counts are integer literals here");
        long v = (long)numlit_int(&m.num);
        accept_word("line"); accept_word("lines");
        if (f->linage) { if (second_after) before = (int)v; else after = (int)v; }
        else {
            /* the print file: the AFTER phrase's count by the usual encoding, the BEFORE's aside */
            int aft = second_after ? (int)v : (after >= 0 ? after + 1 : 0), bef = second_after ? (after >= 0 ? after + 1 : 0) : (int)v;
            if (after_kw) { aft = before >= 0 ? before + 1 : 0; bef = (int)v; }
            before = aft ? aft - 1 : -2; after = 0; after_kw = 1;
            emit_li("r3", bef); emit_call("cob_write_also_before");
        }
    }
    io_nyi("WRITE");
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
        if (!(at_word("at") || at_word("end-of-page") || (at_word("not") && (is_word(peek(1), "at") || is_word(peek(1), "end-of-page")))))
            f14(F14_WRITE_EOP, cur()->line, "WRITE to a LINAGE file without END-OF-PAGE: 2023 raises EC-I-O-EOP where the phrase could stand (E.2 item 20)");
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
    Ref rec; File *f;
    if (at_word("file") && !sym_lookup_quiet("file")) f = parse_file_phrase(&rec, "REWRITE");
    else {
        parse_ref(&rec);
        f = file_of_record(rec.sym, rec.line);
    }
    if (f->org == COB_ORG_LINESEQ) die_at(rec.line, "REWRITE is not valid on a LINE SEQUENTIAL file");
    if (accept_word("from")) { Opnd src; parse_operand(&src); emit_move(&src, &rec); }
    io_nyi("REWRITE");
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
    if (at_word("file") && !file_find(cur()->s)) {
        /* DELETE FILE [OVERRIDE] file-name ... [ON EXCEPTION] (2023 14.9.10
         * format 2): each file removed from storage in turn (GR 12), the
         * connector closed (41 if open), 05 when it is not there, 37 when it
         * cannot be removed; OVERRIDE skips the fixed-attribute check, of
         * which this runtime makes none (GR 19) */
        if (g_std < 2023) die_at(cur()->line, "DELETE FILE is COBOL 2023 (14.9.10 format 2); compile with -std=2023");
        advance();
        int override = accept_word("override");
        File *fs[16]; int n = 0;
        while (cur()->kind == T_WORD && file_find(cur()->s)) {
            if (n == 16) die_at(cur()->line, "DELETE FILE: more than 16 files");
            fs[n] = expect_file();
            if (fs[n]->org == COB_ORG_SORT) die_at(cur()->line, "DELETE FILE '%s': a sort-merge file is not deleted (2023 14.9.10.3 rule 3)", fs[n]->name);
            n++;
        }
        if (!n) die_at(cur()->line, "DELETE FILE needs a file-name");
        if (n > 1 && g_npstk && g_pstk[g_npstk - 1].Lcycle < 0) die_at(cur()->line, "DELETE FILE of several files is not in an exception-checking PERFORM (2023 14.9.10.3 rule 4)");
        io_nyi("DELETE");
        /* the result in SLOT_C: the last file's, an exception from any of them standing */
        emit_li("r1", 0); emit("\tstw sp+%d, r1", SLOT_C);
        for (int i = 0; i < n; i++) {
            emit_file_addr("r3", fs[i]); emit_li("r4", override);
            emit_call("cob_delete_file");
            emit("\tldw r2, sp+%d", SLOT_C); emit("\tor r1, r1, r2"); emit("\tstw sp+%d, r1", SLOT_C);
        }
        g_io_file = fs[n - 1];
        parse_condition_clauses("on", "exception", "end-delete");
        return;
    }
    File *f = expect_file();
    accept_word("record");
    io_nyi("DELETE");
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
    int op = 0;                     /* = ; 5 FIRST, 6 LAST */
    int ki = 0, klen = 0;           /* the key: prime, or an alternate; a leading part's length */
    int firstlast = at_word("first") || at_word("last");
    if (firstlast) {
        /* FIRST, LAST (2023 14.9.41): the first or last record -- of a
         * sequential file by position, of a relative file by number, of an
         * indexed file by the prime key, which becomes the key of reference */
        if (g_std < 2002) die_at(cur()->line, "START ... %s is COBOL 2002 (14.9.41); compile with -std=2002", at_word("first") ? "FIRST" : "LAST");
        op = at_word("first") ? 5 : 6; advance();
    }
    if (f->org == COB_ORG_LINESEQ) die_at(cur()->line, "START of the LINE SEQUENTIAL file '%s': its records have no fixed place (2023 14.9.41 is for sequential, relative and indexed files)", f->name);
    if (f->org != COB_ORG_INDEXED && f->org != COB_ORG_RELATIVE && !firstlast)
        die_at(cur()->line, g_std < 2002 ? "START needs an INDEXED or RELATIVE file (X3.23-1985)"
                                         : "START of the sequential file '%s' takes FIRST or LAST (2023 14.9.41.3 rule 2)", f->name);
    if (f->access == 1) die_at(cur()->line, "START needs ACCESS SEQUENTIAL or DYNAMIC (2023 14.9.41.3 rule 1)");
    if (!firstlast && accept_word("key")) {
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
    Opnd wl; int haslen = 0;
    if (!firstlast && (at_word("with") || at_word("length"))) {
        /* WITH LENGTH arithmetic-expression (2023 14.9.41, GR 13-14): the
         * characters of the key compared, an indexed file's (rule 8);
         * outside 1 to the key's length, 23 at run time */
        if (g_std < 2002) die_at(cur()->line, "START ... WITH LENGTH is COBOL 2002 (14.9.41); compile with -std=2002");
        accept_word("with"); expect_word("length");
        if (f->org != COB_ORG_INDEXED) die_at(cur()->line, "START ... WITH LENGTH is for an indexed file (2023 14.9.41.3 rule 8)");
        int start = g_tp;
        parse_operand(&wl);
        if (at_arith_op()) wl = expr_opnd_after(&wl, start);
        check_numeric_opnd(&wl);
        haslen = 1;
    }
    io_nyi("START");
    if (haslen) {
        /* the length to a slot first: the expression's code uses the
         * argument registers */
        Sym *ks = ki ? f->alt[ki - 1].sym : f->key_sym;
        if (klen) die_at(wl.line, "START ... WITH LENGTH names the key's own length; the leading part of the key is already the length (2023 14.9.41.3 rule 8)");
        emit_push_opnd(&wl); emit_call("cob_pop_int");
        if (ks && sym_is_national(ks)) emit("\tadd r1, r1, r1");   /* a national key: the length counts character positions, two bytes each (GR 13) */
        emit("\tstw sp+%d, r1", SLOT_A);
    }
    emit_file_addr("r3", f);
    emit_li("r4", op | (haslen ? 0x100 : 0)); emit_li("r5", ki);
    if (haslen) emit("\tldw r6, sp+%d", SLOT_A); else emit_li("r6", klen);
    emit_call("cob_start");
    emit("\tstw sp+%d, r1", SLOT_C);
    g_io_file = f;
    parse_condition_clauses("invalid", "key", "end-start");
}
