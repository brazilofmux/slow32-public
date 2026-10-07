/* s32-cobc: SORT, RELEASE, RETURN.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- SORT / RELEASE / RETURN ------------------------------------------ */

static File *expect_file(void);
static void emit_file_addr(const char *reg, File *f);
static void parse_condition_clauses(const char *w1, const char *w2, const char *end_word);

/* SORT sd {ON ASCENDING|DESCENDING KEY item...}... [WITH DUPLICATES IN ORDER]
 *   {USING file... | INPUT PROCEDURE IS para [THRU para]}
 *   {GIVING file... | OUTPUT PROCEDURE IS para [THRU para]}
 * The records live in memory for the statement's duration; the sort is
 * stable whether or not DUPLICATES IN ORDER is written. */
static int g_is_merge;    /* parse_sort is parsing MERGE: USING of two or more files, no INPUT PROCEDURE;
                             each is already in key order and joins the merge as presorted runs (cob_merge_using) */

/* SORT and MERGE are not in an input or output procedure, nor in a
 * declarative (2023 14.9.40.3 rule 3, 14.9.24.3 rule 1; X3.23-1985 SORT
 * rule 2, MERGE rule 2): each unit's procedures and statements are noted
 * and checked when its procedure division is done, the paragraphs being
 * known by then */
static struct { Para *from, *thru; int unit, merge; } g_sproc[128]; static int g_nsproc;
static struct { Para *in; int line, unit, merge; } g_sstmt[256]; static int g_nsstmt;
static int para_in_range(const Para *q, const Para *from, const Para *thru)
{
    const Para *last = thru ? thru : from;
    if (!q || q->line < from->line) return 0;
    if (last->is_section) return q == last || q->section == last->id || q->line <= last->line;
    return q->line <= last->line;
}
static void sort_proc_check(void)
{
    for (int i = 0; i < g_nsstmt; i++) {
        if (g_sstmt[i].unit != g_unit) continue;
        for (int j = 0; j < g_nsproc; j++) {
            if (g_sproc[j].unit != g_unit) continue;
            /* a MERGE may be in a SORT's procedures only if ... it may not
             * (rule 1), nor a SORT in a MERGE's output procedure */
            if (para_in_range(g_sstmt[i].in, g_sproc[j].from, g_sproc[j].thru))
                die_at(g_sstmt[i].line, "%s inside the %s procedure of a %s statement (%s)", g_sstmt[i].merge ? "MERGE" : "SORT",
                       "input or output", g_sproc[j].merge ? "MERGE" : "SORT",
                       g_std < 2002 ? (g_sstmt[i].merge ? "X3.23-1985 MERGE syntax rule 1" : "X3.23-1985 SORT syntax rule 1")
                                    : (g_sstmt[i].merge ? "2023 14.9.24.3 rule 1" : "2023 14.9.40.3 rule 3"));
        }
    }
    int k = 0; for (int i = 0; i < g_nsstmt; i++) if (g_sstmt[i].unit != g_unit) g_sstmt[k++] = g_sstmt[i]; g_nsstmt = k;
    k = 0; for (int j = 0; j < g_nsproc; j++) if (g_sproc[j].unit != g_unit) g_sproc[k++] = g_sproc[j]; g_nsproc = k;
}

/* [COLLATING] SEQUENCE [IS] alphabet-name: -1 native, else the alphabet */
static int sort_collating(const char *verb)
{
    int coll = -1;
    if (accept_word("collating") || at_word("sequence")) {
        expect_word("sequence"); accept_word("is");
        if (cur()->kind != T_WORD) die_at(cur()->line, "%s COLLATING SEQUENCE needs an alphabet-name", verb);
        for (int i = 0; i < g_nalphabet; i++) if (!strcmp(g_alphabet[i].name, cur()->s)) coll = i;
        if (coll < 0) die_at(cur()->line, "%s COLLATING SEQUENCE: '%s' is not an alphabet-name", verb, cur()->s);
        if (g_alphabet[coll].native) coll = -1;
        else g_alphabet[coll].used = 1;
        advance();
    }
    return coll;
}
/* a key's class (both formats' rule c): not boolean, not a pointer */
static void sort_key_class(const Sym *k, int line, const char *rule)
{
    if (!k->is_group && (k->usage == U_BIT || k->pi.category == PIC_BOOLEAN || k->usage == U_POINTER || k->usage == U_INDEX))
        die_at(line, "SORT key '%s' is %s (%s)", k->name, k->usage == U_POINTER ? "a pointer" : k->usage == U_INDEX ? "an index" : "boolean", rule);
}

/* SORT table (COBOL 2002; 2023 14.9.40 format 2): SORT data-name-2 [ON
 * {ASCENDING | DESCENDING} KEY data-name-1 ...]... [WITH DUPLICATES [IN
 * ORDER]] [COLLATING SEQUENCE alphabet-name]: the table's occurrences
 * (the DEPENDING ON count, or all) put in order in place */
static void parse_sort_table(int line)
{
    /* data-name-2, written without subscripts: resolved by name and
     * qualifiers, not by parse_ref, which asks for the subscripts */
    Ref tr; memset(&tr, 0, sizeof tr); tr.line = cur()->line;
    char nm[64], qb[8][64]; char *qv[8]; int nq = 0;
    snprintf(nm, sizeof nm, "%s", cur()->s); advance();
    while ((at_word("of") || at_word("in")) && peek(1)->kind == T_WORD && nq < 8) { advance(); snprintf(qb[nq], 64, "%s", cur()->s); qv[nq] = qb[nq]; nq++; advance(); }
    g_cen_ctx = CEN_PLAIN; tr.sym = sym_lookup(nm, qv, nq, tr.line); g_cen_ctx = 0;
    if (g_cen_on) cen_pin(tr.sym, "SORT");          /* the table's entries, moved whole */
    if (at_op("(")) die_at(tr.line, "SORT '%s': a table SORT of a table inside another table is not implemented", nm);
    Sym *e = tr.sym;
    if (!e->occurs) die_at(tr.line, "SORT '%s': a table SORT names an entry with an OCCURS clause (2023 14.9.40.3 rule 13)", e->name);
    if (e->ndims != 1 || tr.nsub || tr.rm) die_at(tr.line, "SORT '%s': a table SORT of a table inside another table is not implemented", e->name);
    if (g_nsorttab == g_sorttabcap) { g_sorttabcap = g_sorttabcap ? g_sorttabcap * 2 : 4; g_sorttab = realloc(g_sorttab, g_sorttabcap * sizeof *g_sorttab); }
    SortTab *t = &g_sorttab[g_nsorttab++];
    memset(t, 0, sizeof *t); t->id = new_label();
    int ei = sym_idx(e);
    while (at_word("on") || at_word("ascending") || at_word("descending")) {
        accept_word("on");
        int descending = 0;
        if (accept_word("descending")) descending = 1;
        else if (!accept_word("ascending")) die_at(cur()->line, "expected ASCENDING or DESCENDING in SORT");
        accept_word("key");
        int any = 0;
        while (cur()->kind == T_WORD && !at_word("on") && !at_word("ascending") && !at_word("descending") &&
               !at_word("with") && !at_word("collating") && !at_word("sequence") && !is_verb(cur()->s) && !is_terminator(cur()->s)) {
            Sym *q = sym_lookup_quiet(cur()->s);
            if (q && (q->ndims > 1 || (q->occurs && q != e)))
                die_at(cur()->line, "SORT key '%s' has an OCCURS clause or is in a table inside '%s' (2023 14.9.40.3 rule 14e)", q->name, e->name);
            int save = g_noemit; g_noemit++;
            Ref k; memset(&k, 0, sizeof k);
            g_cen_ctx = CEN_PLAIN; k.sym = sym_lookup(cur()->s, NULL, 0, cur()->line); g_cen_ctx = 0; k.line = cur()->line; advance();
            while (accept_word("of") || accept_word("in")) advance();
            g_noemit = save;
            if (at_op("(")) die_at(k.line, "SORT key '%s' is written without subscripts (2023 14.9.40.3 rule 14b)", k.sym->name);
            if (!sym_under(sym_idx(k.sym), ei))
                die_at(k.line, "SORT key '%s' is not '%s' or an item inside it (2023 14.9.40.3 rule 14a)", k.sym->name, e->name);
            for (int a = k.sym->parent; a >= 0 && a != ei; a = g_sym[a].parent)
                if (g_sym[a].occurs) die_at(k.line, "SORT key '%s' is inside '%s', which has an OCCURS clause (2023 14.9.40.3 rule 14e)", k.sym->name, g_sym[a].name);
            sort_key_class(k.sym, k.line, "2023 14.9.40.3 rule 14c");
            if (t->nk == 16) die_at(k.line, "too many SORT keys (16)");
            t->k[t->nk].offset = k.sym->offset - e->offset; t->k[t->nk].desc = sym_desc(k.sym); t->k[t->nk].descending = descending; t->nk++;
            any = 1;
        }
        if (!any) die_at(cur()->line, "expected a key data-name after KEY");
    }
    if (!t->nk) {
        /* no KEY phrase: the table's own (rule 15) */
        if (!e->nokey) die_at(line, "SORT '%s' without a KEY phrase: its OCCURS clause has no KEY either (2023 14.9.40.3 rule 15)", e->name);
        for (int i = 0; i < e->nokey && t->nk < 16; i++) {
            Sym *k = NULL;
            for (int j = ei; j < g_nsym && !k; j++) if (!g_sym[j].is_cond && !g_sym[j].is_index && !strcmp(g_sym[j].name, e->okey[i]) && sym_under(j, ei)) k = &g_sym[j];
            if (!k) die_at(line, "SORT '%s': its KEY '%s' is not found", e->name, e->okey[i]);
            t->k[t->nk].offset = k->offset - e->offset; t->k[t->nk].desc = sym_desc(k); t->k[t->nk].descending = e->okey_desc[i]; t->nk++;
        }
    }
    if (accept_word("with")) { expect_word("duplicates"); accept_word("in"); accept_word("order"); }
    int coll = sort_collating("SORT");
    /* the first occurrence's address, the count, the stride */
    Ref first = tr; first.nsub = 1; first.sub[0].sym = NULL; first.sub[0].lit = 1; first.sub[0].adj = 0;
    if (e->odo_dep_sym) {
        Opnd d; memset(&d, 0, sizeof d); d.kind = O_REF; d.ref.sym = e->odo_dep_sym; d.ref.line = line; d.line = line;
        if (is_hot_int(e->odo_dep_sym)) emit_hot_value(&d);
        else { Arg a[2] = { arg_ref(&d.ref), arg_desc(sym_desc(e->odo_dep_sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
    } else emit_li("r1", e->occurs);
    emit("\tstw sp+%d, r1", SLOT_A);
    char tab[32]; snprintf(tab, sizeof tab, ".Lsk%d_%d", g_unit, t->id);
    emit_ref_addr(&first, "r3");
    emit("\tldw r4, sp+%d", SLOT_A);
    emit_li("r5", e->size);
    emit_la("r6", tab); emit_li("r7", t->nk);
    if (coll >= 0) { char al[32]; snprintf(al, sizeof al, ".Lalph%d_%d", g_unit, coll); emit_la("r8", al); } else emit_li("r8", 0);
    emit_call("cob_sort_table");
}

static void parse_sort(void)
{
    int line = cur()->line;
    const char *verb = g_is_merge ? "MERGE" : "SORT";
    if (!g_is_merge && cur()->kind == T_WORD && !file_find(cur()->s) && sym_lookup_quiet(cur()->s)) {
        if (g_std < 2002) die_at(line, "SORT '%s': a table SORT is COBOL 2002; compile with -std=2002", cur()->s);
        parse_sort_table(line);
        return;
    }
    if (g_in_decl) die_at(line, "%s in a declarative procedure (%s)", verb, g_std < 2002 ? (g_is_merge ? "X3.23-1985 MERGE syntax rule 1" : "X3.23-1985 SORT syntax rule 1") : g_is_merge ? "2023 14.9.24.3 rule 1" : "2023 14.9.40.3 rule 3");
    if (g_nsstmt < 256) { g_sstmt[g_nsstmt].in = g_cur_para; g_sstmt[g_nsstmt].line = line; g_sstmt[g_nsstmt].unit = g_unit; g_sstmt[g_nsstmt].merge = g_is_merge; g_nsstmt++; }
    File *sd = expect_file();
    if (sd->org != COB_ORG_SORT) die_at(line, "%s '%s': the file must be described by an SD (%s)", verb, sd->name,
                                        g_std < 2002 ? "X3.23-1985 SORT syntax rule 2" : "2023 14.9.40.3 rule 4");
    if (sd->rec < 0) die_at(line, "SD %s has no record description", sd->name);
    if (g_nsorttab == g_sorttabcap) { g_sorttabcap = g_sorttabcap ? g_sorttabcap * 2 : 4; g_sorttab = realloc(g_sorttab, g_sorttabcap * sizeof *g_sorttab); }
    SortTab *t = &g_sorttab[g_nsorttab++];
    memset(t, 0, sizeof *t); t->id = new_label();
    while (at_word("on") || at_word("ascending") || at_word("descending")) {
        accept_word("on");
        int descending = 0;
        if (accept_word("descending")) descending = 1;
        else if (!accept_word("ascending")) die_at(cur()->line, "expected ASCENDING or DESCENDING in SORT");
        accept_word("key");
        int any = 0;
        while (cur()->kind == T_WORD && !at_word("on") && !at_word("ascending") && !at_word("descending") &&
               !at_word("with") && !at_word("collating") && !at_word("sequence") && !at_word("using") && !at_word("input") &&
               !at_word("giving") && !at_word("output")) {
            Sym *q = sym_lookup_quiet(cur()->s);
            if (q && q->ndims)
                die_at(cur()->line, "%s key '%s' has an OCCURS clause or is in a table (%s)", verb, q->name,
                       g_std < 2002 ? "X3.23-1985 SORT syntax rule 4" : "2023 14.9.40.3 rule 6b");
            Ref k; parse_ref(&k);
            if (k.sym->record != g_sym[sd->rec].record) die_at(k.line, "SORT key '%s' is not an item of the SD %s", k.sym->name, sd->name);
            sort_key_class(k.sym, k.line, "2023 14.9.40.3 rule 6c");
            if (k.nsub || k.rm) die_at(k.line, "a SORT key is a plain data item of the SD record");
            if (t->nk == 16) die_at(k.line, "too many SORT keys (16)");
            t->k[t->nk].offset = k.sym->offset; t->k[t->nk].desc = sym_desc(k.sym); t->k[t->nk].descending = descending; t->k[t->nk].size = k.sym->size; t->nk++;
            any = 1;
        }
        if (!any) die_at(cur()->line, "expected a key data-name after KEY");
    }
    if (!t->nk) die_at(line, "SORT needs at least one KEY");
    int dups = 0;
    if (accept_word("with")) { expect_word("duplicates"); accept_word("in"); accept_word("order"); dups = 1; }
    int coll = sort_collating(verb);         /* the keys compare by its ranks */
    File *named[32]; int nnamed = 0;
    const char *e85r = g_std < 2002 ? "X3.23-1985" : "2023";
    char tab[32]; snprintf(tab, sizeof tab, ".Lsk%d_%d", g_unit, t->id);
    emit_ec_query("EC-SORT-MERGE-ACTIVE", "cob_sort_any_active", 1);  /* a SORT or MERGE under way already (2023 14.9.40.4 rule 2, 14.9.24.4) */
    emit_file_addr("r3", sd); emit_la("r4", tab); emit_li("r5", t->nk); emit_li("r6", dups);
    if (coll >= 0) { char al[32]; snprintf(al, sizeof al, ".Lalph%d_%d", g_unit, coll); emit_la("r7", al); } else emit_li("r7", 0);
    emit_call("cob_sort_begin");
    if (accept_word("using")) {
        int n = 0;
        while (cur()->kind == T_WORD && !at_word("giving") && !at_word("output")) {
            File *in = expect_file();
            if (in->org == COB_ORG_SORT) die_at(line, "%s USING names a sort file", verb);
            for (int q = 0; q < nnamed; q++) if (named[q] == in) die_at(line, "%s names the file '%s' twice (%s)", verb, in->name, g_std < 2002 ? "X3.23-1985 MERGE syntax rule 7" : "2023 14.9.24.3 rule 7");
            if (nnamed < 32) named[nnamed++] = in;
            if (in->recsize > sd->recsize)
                die_at(line, "%s USING '%s': its record (%d) is longer than the sort file's (%d) (%s %s)", verb, in->name, in->recsize, sd->recsize, e85r,
                       g_std < 2002 ? (g_is_merge ? "MERGE syntax rule 3" : "SORT syntax rule 3") : (g_is_merge ? "14.9.24.3 rule 3" : "14.9.40.3 rule 5"));
            if ((in->org == COB_ORG_RELATIVE || in->org == COB_ORG_INDEXED) && in->access == 1)
                die_at(line, "%s USING '%s': a relative or indexed file here is in sequential or dynamic access (%s)", verb, in->name,
                       g_is_merge ? "2023 14.9.24.3 rule 13" : "2023 14.9.40.3 rule 12");
            emit_file_addr("r3", in);
            emit_ec_query("EC-SORT-MERGE-FILE-OPEN", "cob_open_mode", 1);   /* a USING file open when the sort begins (14.9.40.4, 14.9.24.4) */
            emit_file_addr("r3", sd); emit_file_addr("r4", in); emit_call(g_is_merge ? "cob_merge_using" : "cob_sort_using"); n++;
            if (g_is_merge) emit_ec_query("EC-SORT-MERGE-SEQUENCE", "cob_merge_sequence_error", 1);   /* a USING file out of order (14.9.24.4 rule 6) */
        }
        if (!n) die_at(cur()->line, "expected a file-name after USING");
        if (g_is_merge && n < 2) die_at(line, "MERGE USING needs at least two files");
    } else if (g_is_merge) die_at(cur()->line, "MERGE needs USING");
    else if (accept_word("input")) {
        expect_word("procedure"); accept_word("is");
        Body b; memset(&b, 0, sizeof b);
        b.from = expect_para();
        if (accept_word("thru") || accept_word("through")) b.thru = expect_para();
        if (g_nsproc < 128) { g_sproc[g_nsproc].from = b.from; g_sproc[g_nsproc].thru = b.thru; g_sproc[g_nsproc].unit = g_unit; g_sproc[g_nsproc].merge = g_is_merge; g_nsproc++; }
        emit_body(&b);
    } else die_at(cur()->line, "SORT needs USING or INPUT PROCEDURE");
    emit_file_addr("r3", sd); emit_call("cob_sort_perform");
    if (accept_word("giving")) {
        int n = 0;
        while (cur()->kind == T_WORD && file_find(cur()->s)) {
            File *out = expect_file();
            if (out->org == COB_ORG_SORT) die_at(line, "SORT GIVING names a sort file");
            if (g_is_merge) for (int q = 0; q < nnamed; q++) if (named[q] == out) die_at(line, "MERGE names the file '%s' twice (%s)", out->name, g_std < 2002 ? "X3.23-1985 MERGE syntax rule 7" : "2023 14.9.24.3 rule 7");
            if (sd->recsize > out->recsize && !out->varying && out->org != COB_ORG_LINESEQ)
                die_at(line, "%s GIVING '%s': the sort file's record (%d) is longer than its record (%d) (%s)", verb, out->name, sd->recsize, out->recsize,
                       g_std < 2002 ? (g_is_merge ? "X3.23-1985 MERGE syntax rule 11" : "X3.23-1985 SORT syntax rule 10") : g_is_merge ? "2023 14.9.24.3 rule 12" : "2023 14.9.40.3 rule 11");
            if (out->org == COB_ORG_INDEXED && out->key_sym &&
                (t->k[0].descending || t->k[0].offset != out->key_sym->offset - g_sym[out->rec].offset || t->k[0].size != out->key_sym->size))
                die_at(line, "%s GIVING the indexed file '%s': the first key is ASCENDING and in the place of its RECORD KEY (%s)", verb, out->name,
                       g_std < 2002 ? (g_is_merge ? "X3.23-1985 MERGE syntax rule 10" : "X3.23-1985 SORT syntax rule 8") : g_is_merge ? "2023 14.9.24.3 rule 10" : "2023 14.9.40.3 rule 9");
            emit_file_addr("r3", out);
            emit_ec_query("EC-SORT-MERGE-FILE-OPEN", "cob_open_mode", 1);   /* a GIVING file open */
            emit_file_addr("r3", sd); emit_file_addr("r4", out); emit_call("cob_sort_giving"); n++;
        }
        if (!n) die_at(cur()->line, "expected a file-name after GIVING");
    } else if (accept_word("output")) {
        expect_word("procedure"); accept_word("is");
        Body b; memset(&b, 0, sizeof b);
        b.from = expect_para();
        if (accept_word("thru") || accept_word("through")) b.thru = expect_para();
        if (g_nsproc < 128) { g_sproc[g_nsproc].from = b.from; g_sproc[g_nsproc].thru = b.thru; g_sproc[g_nsproc].unit = g_unit; g_sproc[g_nsproc].merge = g_is_merge; g_nsproc++; }
        emit_body(&b);
    } else die_at(cur()->line, "SORT needs GIVING or OUTPUT PROCEDURE");
    emit_file_addr("r3", sd); emit_call("cob_sort_end");
}

/* RELEASE record [FROM x] */
static void parse_release(void)
{
    Ref rec; parse_ref(&rec);
    File *f = file_of_record(rec.sym, rec.line);
    if (f->org != COB_ORG_SORT) die_at(rec.line, "RELEASE '%s': the record must belong to an SD", rec.sym->name);
    if (accept_word("from")) { Opnd src; parse_operand(&src); emit_move(&src, &rec); }
    emit_file_addr("r3", f);
    emit_ec_query("EC-FLOW-RELEASE", "cob_sort_under_way", 0);        /* RELEASE outside its SORT (2023 14.9.32.4 rule 1) */
    emit_file_addr("r3", f);
    emit_call("cob_release");
}

/* RETURN sd [RECORD] [INTO x] AT END ... [NOT AT END ...] [END-RETURN] */
static void parse_return(void)
{
    File *f = expect_file();
    if (f->org != COB_ORG_SORT) die_at(cur()->line, "RETURN '%s': the file must be an SD", f->name);
    accept_word("record");
    Ref into; int has_into = 0;
    if (accept_word("into")) { g_noemit++; parse_ref(&into); g_noemit--; has_into = 1; }   /* identified after the record is read */
    emit_file_addr("r3", f);
    emit_ec_query("EC-FLOW-RETURN", "cob_sort_under_way", 0);         /* RETURN outside its SORT or MERGE (2023 14.9.34.4 rule 1) */
    emit_file_addr("r3", f);
    emit_ec_query("EC-SORT-MERGE-RETURN", "cob_sort_at_end", 1);      /* a RETURN after the at end condition (14.9.34.4 rule 3) */
    emit_file_addr("r3", f);
    emit_call("cob_return");
    emit("\tstw sp+%d, r1", SLOT_C);
    g_io_file = NULL;                   /* an SD has no USE procedure */
    if (has_into) {
        int Lskip = new_label();
        emit("\tbne r1, r0, .L%d", Lskip);
        Opnd src; memset(&src, 0, sizeof src); src.kind = O_REF; src.line = into.line;
        src.ref.sym = &g_sym[f->rec]; src.ref.line = into.line;
        recv_calls(&into);
        emit_move(&src, &into);
        emit_label(Lskip);
    }
    parse_condition_clauses("at", "end", "end-return");
}

static void lw_note_perform(int lo, int thru);  /* lower.h */
static int g_lw_pf_once;                        /* lower.h: the PERFORM being emitted is a plain one of a range */
static int lw_loop_folded_at(int lay0);         /* lower.h */
static void emit_body(Body *b)
{
    if (!b->inline_body) {
        int Lret = new_label();
        char lab[32]; snprintf(lab, sizeof lab, ".L%d", Lret);
        lw_note_perform(b->from->id, b->thru ? b->thru->id : -1);     /* lower.h: the statement performs this range */
        emit_para_cell("r3", g_unit, b->thru ? b->thru->id : b->from->id);
        emit_la("r4", lab);
        emit_call("cob_perform_push");
        emit("\tjal r0, .Lp%d_%d", g_unit, b->from->id);
        emit_label(Lret);
    } else {
        block_put(&b->blk);
        if (b->Lcycle >= 0) emit_label(b->Lcycle);
    }
}

/* an inline body's statements, up to END-PERFORM: EXIT PERFORM CYCLE
 * comes to its end, EXIT PERFORM past END-PERFORM */
static void parse_inline_body(Body *b)
{
    b->Lcycle = -1;
    g_inline_depth++;                   /* loops in it wait for this one's code (loopreg.h) */
    if (b->Lexit < 0) b->blk = parse_block();
    else {
        b->Lcycle = new_label();
        pstk_push(b->Lexit, b->Lcycle);
        b->blk = parse_block();
        g_npstk--;
    }
    g_inline_depth--;
    expect_word("end-perform");
}

static void emit_add_to_ref(Opnd *by, Ref *var)
{
    Opnd ops[1] = { *by }; Ref rs[1] = { *var };
    emit_incompat(by); emit_incompat_refs(rs, 1);       /* both are summed */
    int was = g_wide;
    g_wide = was || opnds_wide(ops, 1) || refs_wide(rs, 1);
    int hot = !g_wide && opnd_hot_int(by) && ref_hot_store(var, 0, ops_all_nonneg(ops, 1));
    int rd[1] = { 0 };
    long k = by->kind == O_NUM ? (long)numlit_int(&by->num) : 0;
    g_addk_on = !g_nohx && hot && by->kind == O_NUM && k > -2048 && k < 2048;
    g_addk = k;
    if (!g_addk_on) { if (hot) emit_hot_sum(ops, 1); else emit_push(by); }
    emit_store_receivers(rs, rd, 1, hot, 0, 0, 0, ops_sum_mag(ops, 1), ops_all_nonneg(ops, 1));
    g_addk_on = 0;
    g_wide = was; if (!was) g_fstmt = 0;
}

/* ucv/ucf/ucb: the user function calls in the item's subscripts, FROM
 * and BY, recorded as a condition's are and made where each is
 * evaluated -- the item's at every set and augmentation, FROM at every
 * set, BY at every augmentation (2023 14.9.28.4 rules 7, 9, 12) */
typedef struct { Ref var; Opnd from, by; Cond *until; int ucv0, ucv1, ucf0, ucf1, ucb0, ucb1; } Vary;
static void vary_augment(Vary *x)
{
    emit_ucalls(x->ucv0, x->ucv1); emit_ucalls(x->ucb0, x->ucb1);
    emit_add_to_ref(&x->by, &x->var);
}

/* an induction variable to its FROM value; an index-name set from an
 * identifier that is not positive is EC-RANGE-PERFORM-VARYING (2023
 * 14.9.28.4 rule 3) */
static void emit_vary_init(Vary *x)
{
    emit_ucalls(x->ucv0, x->ucv1); emit_ucalls(x->ucf0, x->ucf1);
    if (x->var.sym->is_index && x->from.kind == O_REF && ec_on_name("EC-RANGE-PERFORM-VARYING")) {
        int Lok = new_label();
        Arg a[2] = { arg_ref(&x->from.ref), arg_desc(sym_desc(x->from.ref.sym)) };
        emit_args(a, 2); emit_call("cob_load_int");
        emit("\tblt r0, r1, .L%d", Lok);
        emit_ec_raise(ec_find("EC-RANGE-PERFORM-VARYING", 0));
        emit_label(Lok);
    }
    emit_incompat(&x->from);
    emit_move(&x->from, &x->var);
}

/* A loop is laid out with its test at the bottom: one jump in, to the
 * test, and then each iteration is the body and the test's own branch
 * back -- not a test, the body and a jump back to the test. */
static void emit_varying(Vary *v, int nv, int level, Body *body, int test_after)
{
    Vary *x = &v[level];
    emit_vary_init(x);
    if (body->inline_body) lr_begin();          /* the loop, its item set: a region (loopreg.h) */
    if (test_after) {
        int Ltop = new_label(), Lend = new_label();
        emit_label(Ltop);
        if (level + 1 < nv) emit_varying(v, nv, level + 1, body, test_after);
        else emit_body(body);
        cond_jump_true(x->until, Lend);
        emit_add_to_ref(&x->by, &x->var);
        emit_jump(Ltop);
        emit_label(Lend);
    } else {
        int Lbody = new_label(), Ltest = new_label();
        emit_jump(Ltest);
        emit_label(Lbody);
        if (level + 1 < nv) emit_varying(v, nv, level + 1, body, test_after);
        else emit_body(body);
        vary_augment(x);
        emit_label(Ltest);
        cond_jump_false(x->until, Lbody);
    }
    if (body->inline_body) lr_end();
    /* an inner item goes back to its FROM when its condition is true and
     * the outer one is augmented (6.20.4), so it reads FROM at the end */
    if (level > 0) emit_vary_init(x);
}

/* VARYING ... AFTER ... WITH TEST AFTER (X3.23 6.20.4, the figure for
 * two identifiers): every item takes its FROM once; after each execution
 * of the body the innermost condition is tested -- false: its item is
 * augmented and the body runs again; true: the next outer condition is
 * tested -- false: every inner item goes back to its FROM, the outer is
 * augmented and the body runs again; true: outward again, the first
 * condition's truth ending the statement.  The items keep the values at
 * which their conditions came true. */
static void emit_varying_test_after(Vary *v, int nv, Body *body)
{
    for (int k = 0; k < nv; k++) emit_vary_init(&v[k]);
    if (body->inline_body) lr_begin();
    int Ltop = new_label();
    emit_label(Ltop);
    emit_body(body);
    for (int k = nv - 1; k >= 0; k--) {
        int Ldone = new_label();
        cond_jump_true(v[k].until, Ldone);
        for (int j = k + 1; j < nv; j++) emit_vary_init(&v[j]);
        vary_augment(&v[k]);
        emit_jump(Ltop);
        emit_label(Ldone);
    }
    if (body->inline_body) lr_end();
}

/* is the operand at the cursor followed by TIMES?  (a data-name may carry
 * OF/IN qualifiers and a subscript) */
static int times_follows(void)
{
    int j = g_tp;
    if (g_tok[j].kind == T_NUM) return is_word(&g_tok[j + 1], "times");
    if (g_tok[j].kind != T_WORD) return 0;
    j++;
    while (is_word(&g_tok[j], "of") || is_word(&g_tok[j], "in")) j += 2;
    if (g_tok[j].kind == T_LP) {
        int depth = 0;
        do { if (g_tok[j].kind == T_LP) depth++; else if (g_tok[j].kind == T_RP) depth--; else if (g_tok[j].kind == T_EOF) return 0; j++; } while (depth > 0);
    }
    return is_word(&g_tok[j], "times");
}

/* The WHEN phrases of the inline PERFORM at the cursor, read ahead from
 * the tokens -- their names are turned on before imperative-statement-1
 * is compiled (14.9.28 rule 14).  Returns 1 when the PERFORM ends in WHEN
 * ... EXCEPTION or FINALLY at its own level, an exception-checking
 * PERFORM; with e NULL it only answers that. */
static int g_in_finally;            /* inside a FINALLY phrase: no transfer out of the PERFORM (14.9.28.4 rule 16) */
static int g_in_ecp_when;           /* inside a WHEN phrase of an exception-checking PERFORM: no GO TO (14.9.17.3 rule 3) */
static int ecp_scan(Ecp *e)
{
    int depth = 0, found = 0;
    for (int k = g_tp; k < g_ntok; k++) {
        Tok *t = &g_tok[k];
        if (t->kind == T_PERIOD || t->kind == T_EOF) break;
        if (t->kind != T_WORD) continue;
        if (!strcmp(t->s, "perform") && !(k > 0 && is_word(&g_tok[k - 1], "exit"))) {   /* EXIT PERFORM opens nothing */
            Tok *n = &g_tok[k + 1];
            if (!(at_para_name(n) && para_find(n->s))) depth++;       /* inline: closed by END-PERFORM */
            continue;
        }
        if (!strcmp(t->s, "end-perform")) { if (depth-- == 0) break; continue; }
        if (depth) continue;
        if (!strcmp(t->s, "finally")) { if (!e) return 1; found = 1; continue; }
        if (strcmp(t->s, "when")) continue;
        Tok *a = &g_tok[k + 1], *b = &g_tok[k + 2];
        int other = is_word(a, "other") && is_word(b, "exception"), common = is_word(a, "common") && is_word(b, "exception");
        if (!is_word(a, "exception") && !other && !common) continue;
        if (!e) return 1;
        found = 1;
        if (other) { e->Lother = new_label(); continue; }
        if (common) { e->Lcommon = new_label(); continue; }
        if (e->nw == e->wcap) { e->wcap = e->wcap ? 2 * e->wcap : 8; e->w = xrealloc(e->w, (size_t)e->wcap * sizeof *e->w); }
        EcpWhen *w = &e->w[e->nw++]; memset(w, 0, sizeof *w); w->label = new_label();
        /* WHEN EXCEPTION file-name-1 ... or an open mode (14.9.28 format 3): a
         * file's I-O errors, chosen as a USE AFTER EXCEPTION PROCEDURE ON
         * the file or the mode chooses them (14.9.49.4 rules 3a-b, 6).  An
         * entry with no exception-name: ec -1, file the file's index, or
         * -10 - the open mode */
        {
            int q = k + 2, mode = 0;
            Tok *x = &g_tok[q];
            if (is_word(x, "input")) mode = COB_OPEN_INPUT;
            else if (is_word(x, "output")) mode = COB_OPEN_OUTPUT;
            else if (is_word(x, "i-o") || is_word(x, "io")) mode = COB_OPEN_IO;
            else if (is_word(x, "extend")) mode = COB_OPEN_EXTEND;
            if (mode || (x->kind == T_WORD && strncmp(x->s, "ec-", 3) && file_find(x->s))) {
                for (; q < g_ntok && g_tok[q].kind == T_WORD && !is_verb(g_tok[q].s); q++) {
                    x = &g_tok[q];
                    int fk;
                    if (mode) { if (q > k + 2) break; fk = -10 - mode; }
                    else {
                        File *f = file_find(x->s);
                        if (!f) break;
                        if (f->org == COB_ORG_SORT) die_at(x->line, "'%s' is a sort or merge file (2023 14.9.49.3 rule 2)", f->name);
                        fk = (int)(f - g_files);
                    }
                    for (int v = 0; v < e->nw; v++) for (int u = 0; u < e->w[v].n; u++)
                        if (e->w[v].ec[u] < 0 && e->w[v].file[u] == fk)
                            die_at(x->line, mode ? "the open mode %s appears twice in the WHEN phrases (2023 14.9.28.3 rule 14)"
                                                 : "the file '%s' appears twice in the WHEN phrases (2023 14.9.28.3 rule 14)", x->s);
                    if (w->n == w->cap) { w->cap = w->cap ? 2 * w->cap : 8; w->ec = xrealloc(w->ec, (size_t)w->cap * sizeof *w->ec); w->file = xrealloc(w->file, (size_t)w->cap * sizeof *w->file); }
                    w->ec[w->n] = -1; w->file[w->n] = fk; w->n++;
                }
                continue;
            }
        }
        for (int q = k + 2; q < g_ntok && g_tok[q].kind == T_WORD && !is_verb(g_tok[q].s); q++) {
            Tok *x = &g_tok[q];
            if (strncmp(x->s, "ec-", 3))
                die_at(x->line, "WHEN EXCEPTION names exception-names, file-names or an open mode, not '%s' (2023 14.9.28.2)", x->s);
            int i = ec_find(x->s, x->line);
            if (i < 0) die_at(x->line, "'%s' is not an exception-name", x->s);
            int file = -1;
            if (is_word(&g_tok[q + 1], "file")) {
                File *f = file_find(g_tok[q + 2].s);
                if (!f) die_at(x->line, "WHEN %s FILE: '%s' is not a file-name", ec_name(i), g_tok[q + 2].s);
                if (strncmp(ec_name(i), "EC-I-O", 6)) die_at(x->line, "FILE follows only an EC-I-O exception-name (2023 14.9.28.3 rule 16)");
                file = (int)(f - g_files); q += 2;
            }
            for (int v = 0; v < e->nw; v++) for (int u = 0; u < e->w[v].n; u++)
                if (e->w[v].ec[u] == i && e->w[v].file[u] == file)
                    die_at(x->line, "%s appears twice in the WHEN phrases (2023 14.9.28.3 rule 15)", ec_name(i));
            if (w->n == w->cap) { w->cap = w->cap ? 2 * w->cap : 8; w->ec = xrealloc(w->ec, (size_t)w->cap * sizeof *w->ec); w->file = xrealloc(w->file, (size_t)w->cap * sizeof *w->file); }
            w->ec[w->n] = i; w->file[w->n] = file; w->n++;
        }
        if (!w->n) die_at(t->line, "WHEN EXCEPTION needs an exception-name");
    }
    return found;
}
static int perform_is_ecp(void) { return ecp_scan(NULL); }

/* the implicit TURN before imperative-statement-1 (rule 14): each
 * condition a WHEN name covers whose checking is not already enabled --
 * for all files, or for the WHEN's file -- turned on, with LOCATION when
 * the PERFORM has it; a condition already enabled keeps its setting */
static void ecp_implicit_turn(int i, int file, int loc)
{
    for (int c = 0; c < NEC + g_necu; c++) {
        if (!ec_covers(i, c)) continue;
        if (file >= 0 ? ec_on_file(c, file, NULL) : ec_on_all(c)) continue;
        ec_turn_c(c, file, 1, loc);
    }
    if (file < 0 && ec_covers_later_users(i) && !g_ecs.user_on) { g_ecs.user_on = 1; g_ecs.user_loc = loc; }
}

/* PERFORM [WITH LOCATION] imperative-statement-1 {WHEN EXCEPTION ...}...
 * [WHEN OTHER EXCEPTION ...] [WHEN COMMON EXCEPTION ...] [FINALLY ...]
 * END-PERFORM (2023 14.9.28 format 3) */
static void parse_perform_ecp(void)
{
    static int ecp_ids;
    int line = cur()->line, loc = 0, start = g_tp;
    if (at_word("with") && is_word(peek(1), "location")) { advance(); advance(); loc = 1; }
    Ecp *e = xmalloc(sizeof *e); memset(e, 0, sizeof *e);
    e->id = ecp_ids++; e->Lother = e->Lcommon = -1; e->Lend = new_label();
    ecp_scan(e);
    /* the checking before the PERFORM: after END-PERFORM it is back as it
     * was, for no TURN can be inside it (7.3.25.3 rule 5), and whatever
     * the implicit TURN enabled is not enabled any more (rule 22) */
    EcState pre; memset(&pre, 0, sizeof pre); ecs_copy(&pre, &g_ecs);
    int necu0 = g_necu;
    for (int w = 0; w < e->nw; w++) for (int q = 0; q < e->w[w].n; q++) if (e->w[w].ec[q] >= 0) ecp_implicit_turn(e->w[w].ec[q], e->w[w].file[q], loc);
    /* imperative-statement-1, a statement at a time: a raise resumes after
     * the statement it occurred in (rule 20) */
    int Lafter = new_label();
    pstk_push(e->Lend, -1);                       /* EXIT PERFORM: to FINALLY or END-PERFORM, no CYCLE (rules 4, 8) */
    if (g_necp == g_ecp_cap) { g_ecp_cap = g_ecp_cap ? 2 * g_ecp_cap : 8; g_ecp = xrealloc(g_ecp, (size_t)g_ecp_cap * sizeof *g_ecp); }
    g_ecp[g_necp++] = e;
    while (!at_word("when") && !at_word("finally") && !at_word("end-perform") && !at_scope_end()) {
        e->resume = new_label();
        parse_statement();
        emit_label(e->resume);
    }
    g_necp--;
    emit_jump(e->Lend);
    /* the phrases: checking off inside them (the implicit PUSH ALL and
     * TURN OFF ALL, rule 14), no WHEN of this PERFORM for their raises (21) */
    memset(g_ecs.on, 0, sizeof g_ecs.on); memset(g_ecs.loc, 0, sizeof g_ecs.loc);
    g_ecs.nf = 0; g_ecs.user_on = g_ecs.user_loc = 0;
    g_ecp_handler++;
    int wi = 0;
    for (;;) {
        int is_common = 0;
        if (at_word("when") && is_word(peek(1), "exception")) {
            advance(); advance();
            while (cur()->kind == T_WORD && !is_verb(cur()->s)) advance();     /* the names, read already */
            emit_label(e->w[wi++].label);
        } else if (at_word("when") && is_word(peek(1), "other")) { advance(); advance(); expect_word("exception"); emit_label(e->Lother); }
        else if (at_word("when") && is_word(peek(1), "common")) { advance(); advance(); expect_word("exception"); emit_label(e->Lcommon); is_common = 1; }
        else break;
        g_in_ecp_when++; parse_statements(); g_in_ecp_when--;
        /* a WHEN phrase goes on to WHEN COMMON (17-19); the last of them
         * returns where the raise left its resume point -- after the
         * statement for a nonfatal condition; a fatal one ends the run
         * there (20; 14.6.13.1.3 rule 4) */
        if (!is_common && e->Lcommon >= 0) { emit_jump(e->Lcommon); continue; }
        emit_li("r3", e->id);
        emit("\tadd r4, sp, r0");
        emit_call("cob_ecp_pop");
        emit("\tjalr r0, r1, 0");
    }
    emit_label(e->Lend);
    /* what a phrase left by EXIT PERFORM: dropped, and a fatal condition
     * ends the run all the same */
    emit_li("r3", e->id);
    emit("\tadd r4, sp, r0");
    emit_call("cob_ecp_drop");
    g_npstk--;
    if (accept_word("finally")) {                   /* in FINALLY: EXIT PERFORM goes past END-PERFORM (16) */
        pstk_push(Lafter, -1); g_in_finally++; parse_statements(); g_in_finally--; g_npstk--;
    }
    g_ecp_handler--;
    if (!accept_word("end-perform")) die_at(cur()->line, "expected END-PERFORM to end the exception-checking PERFORM, found %s", tok_desc(cur()));
    emit_label(Lafter);
    for (int k = 0; k < g_ndir; k++)
        if (g_dir[k].pos >= start && g_dir[k].pos < g_tp)
            die_at(g_dir[k].tok.line, "a TURN directive inside an exception-checking PERFORM (2023 7.3.25.3 rule 5)");
    ecs_copy(&g_ecs, &pre);
    for (int c = NEC + necu0; c < NEC + g_necu; c++) { g_ecs.on[c] = (unsigned char)pre.user_on; g_ecs.loc[c] = (unsigned char)pre.user_loc; }
    free(pre.f);
    (void)line;
}

static void decl_ref_check(const Para *p, int is_perform, int line);
/* the declarative section a paragraph or section is in, or -1 */
static int para_decl_sec(const Para *p)
{
    int sec = p->is_section ? p->id : p->section;    /* ids from 1; 0: in no section */
    if (!sec) return -1;
    for (int u = 0; u < g_nuse; u++) if (g_use[u].unit == g_unit && g_use[u].sec == sec) return sec;
    for (int u = 0; u < g_nrwuse; u++) if (g_rwuse[u].unit == g_unit && g_rwuse[u].sec == sec) return sec;
    return -1;
}

/* VARYING or AFTER with an index-name (2023 14.9.28.3 rules 4-6; X3.23-1985
 * PERFORM rules 3-4): the other operands integers, FROM a positive and BY
 * a nonzero literal; BY is never zero */
static void varying_rules(const Ref *var, const Opnd *from, const Opnd *by)
{
    if (by->kind == O_NUM && numlit_is_zero(&by->num))
        die_at(by->line, "the BY literal of PERFORM VARYING shall not be zero (2023 14.9.28.3 rule 6)");
    if (var->sym->is_index) {
        if (from->kind == O_REF && !is_int_item(from->ref.sym))
            die_at(from->line, "VARYING an index-name: FROM '%s' must be an integer item (2023 14.9.28.3 rule 4a)", from->ref.sym->name);
        if (from->kind == O_NUM && (!numlit_is_int(&from->num) || from->num.neg || numlit_is_zero(&from->num)))
            die_at(from->line, "VARYING an index-name: the FROM literal must be a positive integer (2023 14.9.28.3 rule 4b)");
        if (by->kind == O_REF && !is_int_item(by->ref.sym))
            die_at(by->line, "VARYING an index-name: BY '%s' must be an integer item (2023 14.9.28.3 rule 4a)", by->ref.sym->name);
        if (by->kind == O_NUM && !numlit_is_int(&by->num))
            die_at(by->line, "VARYING an index-name: the BY literal must be a nonzero integer (2023 14.9.28.3 rule 4c)");
    }
    if (from->kind == O_REF && from->ref.sym->is_index) {
        if (!is_int_item(var->sym))
            die_at(var->line, "FROM an index-name: the VARYING item '%s' must be an integer (2023 14.9.28.3 rule 5a)", var->sym->name);
        if (by->kind == O_REF && !is_int_item(by->ref.sym))
            die_at(by->line, "FROM an index-name: BY '%s' must be an integer item (2023 14.9.28.3 rule 5b)", by->ref.sym->name);
        if (by->kind == O_NUM && !numlit_is_int(&by->num))
            die_at(by->line, "FROM an index-name: the BY literal must be an integer (2023 14.9.28.3 rule 5c)");
    }
}

static int lw_perform(Vary *v, int nv, Cond *until, Body *body, int test_after);   /* lower.h */
static void lw_resolve(int from);
static void parse_perform(void)
{
    Body body; memset(&body, 0, sizeof body);
    if (g_std >= 2002 && ((at_word("with") && is_word(peek(1), "location")) ||
        (!(at_para_name(cur()) && para_find(cur()->s)) && perform_is_ecp()))) { parse_perform_ecp(); return; }
    if (at_para_name(cur()) && para_find(cur()->s)) {
        body.from = expect_para();
        decl_ref_check(body.from, 1, cur()->line);
        if (accept_word("thru") || accept_word("through")) {
            body.thru = expect_para();
            decl_ref_check(body.thru, 1, cur()->line);
            /* a range into or out of the declaratives stays in one
             * declarative section (X3.23-1985 PERFORM rule 5; 2023
             * 14.9.28.3 rule 11) */
            int d1 = para_decl_sec(body.from), d2 = para_decl_sec(body.thru);
            if ((d1 >= 0 || d2 >= 0) && d1 != d2)
                die_at(cur()->line, "PERFORM %s THRU %s: a range that names a declarative procedure stays in one declarative section (2023 14.9.28.3 rule 11)",
                       body.from->oname, body.thru->oname);
        }
    } else {
        /* a name that is no statement, loop phrase or TIMES count can only
         * have meant a paragraph */
        Tok *t = cur(), *n = peek(1);
        if (t->kind == T_WORD && !is_verb(t->s) && !is_terminator(t->s) && !is_word(t, "with") && !is_word(t, "test") &&
            !is_word(t, "until") && !is_word(t, "varying") && !is_word(n, "times") && !is_word(n, "of") && !is_word(n, "in") &&
            n->kind != T_LP)
            die_at(t->line, "'%s' is not a paragraph or section", t->s);
        body.inline_body = 1;
        body.Lexit = g_std >= 2002 ? new_label() : -1;     /* EXIT PERFORM is 2002: -std=85 output keeps its labels */
    }

    int test_after = 0, test_given = 0;
    if (accept_word("with")) { expect_word("test"); test_given = 1; if (accept_word("after")) test_after = 1; else expect_word("before"); }
    else if (accept_word("test")) { test_given = 1; if (accept_word("after")) test_after = 1; else expect_word("before"); }

    /* the phrases, then an inline body's statements, all read before the
     * loop's code */
    enum { PF_ONCE, PF_UNTIL_EXIT, PF_UNTIL, PF_VARYING, PF_TIMES } kind = PF_ONCE;
    Cond *c = NULL;
    Vary v[8]; int nv = 0;                     /* the text sets no limit; NC233A/NC243A nest four */
    Opnd n; memset(&n, 0, sizeof n);
    if (accept_word("until") && g_std >= 2002 && accept_word("exit")) {
        if (test_given) die_at(g_tok[g_tp - 1].line, "UNTIL EXIT takes no WITH TEST phrase (2023 14.9.28.3 rule 8)");
        kind = PF_UNTIL_EXIT;
    } else if (g_tok[g_tp - 1].kind == T_WORD && !strcmp(g_tok[g_tp - 1].s, "until")) {
        kind = PF_UNTIL;
        c = parse_cond();
    } else if (accept_word("varying")) {
        kind = PF_VARYING;
        for (;;) {
            if (nv >= 8) die_at(cur()->line, "more than eight VARYING/AFTER levels");
            /* the item's subscripting is evaluated each time it is set or
             * augmented (X3.23-1985 XVII-64, substantive change 27; 2023
             * 14.9.28.4 rule 12), an AFTER's FROM at every reset and BY at
             * every step: a user function in any of them is recorded as a
             * condition's are (g_cond_depth) and made at each of those
             * points (vary_augment, emit_vary_init), not where it is parsed */
            g_cond_depth++;
            v[nv].ucv0 = g_nucall;
            parse_ref(&v[nv].var);
            v[nv].ucv1 = g_nucall;
            g_cond_depth--;
            if (!is_numeric_sym(v[nv].var.sym)) die_at(v[nv].var.line, "the VARYING item must be numeric");
            expect_word("from");
            g_cond_depth++;
            v[nv].ucf0 = g_nucall;
            parse_operand(&v[nv].from); check_numeric_opnd(&v[nv].from);
            v[nv].ucf1 = g_nucall;
            g_cond_depth--;
            if (nv > 0 && v[nv].from.kind == O_REF)          /* BP-M1: the 74/85 reset order shows here */
                for (int k = 0; k < nv; k++)
                    if (v[k].var.sym == v[nv].from.ref.sym) { bp(BP_M1_VARYING_AFTER, v[nv].from.line); break; }
            expect_word("by");
            g_cond_depth++;
            v[nv].ucb0 = g_nucall;
            parse_operand(&v[nv].by); check_numeric_opnd(&v[nv].by);
            v[nv].ucb1 = g_nucall;
            g_cond_depth--;
            varying_rules(&v[nv].var, &v[nv].from, &v[nv].by);
            expect_word("until");
            if (at_word("exit"))
                die_at(cur()->line, "UNTIL EXIT is not a VARYING or AFTER phrase's condition (2023 14.9.28.3 rule 8)");
            v[nv].until = parse_cond();
            nv++;
            if (!accept_word("after")) break;
        }
        if (g_std < 2002 && body.inline_body && nv > 1)
            die_at(v[1].var.line, "an in-line PERFORM VARYING takes no AFTER phrase in COBOL 85 (X3.23-1985 PERFORM syntax rule 2)");
    } else if (at_operand() && times_follows()) {
        kind = PF_TIMES;
        parse_operand(&n); check_numeric_opnd(&n);
        if ((n.kind == O_REF && !is_int_item(n.ref.sym)) || (n.kind == O_NUM && !numlit_is_int(&n.num)))
            die_at(n.line, "PERFORM ... TIMES takes an integer (2023 14.9.28.3 rule 2)");
        expect_word("times");
    }
    if (body.inline_body) parse_inline_body(&body);
    g_lw_pf_once = kind == PF_ONCE && !body.inline_body;     /* lower.h: a plain PERFORM of a range, inlinable */
    if (!body.inline_body) pc_perform(body.from, body.thru, kind == PF_ONCE ? "once" : kind == PF_UNTIL ? "until" : kind == PF_VARYING ? "varying" : kind == PF_TIMES ? "times" : "exit");

    if (kind == PF_VARYING || kind == PF_UNTIL) lw_perform(kind == PF_VARYING ? v : NULL, nv, c, &body, test_after);   /* an island's too (lower.h): its placeholder, then the text */
    int lay0 = g_nasm;                  /* the statement's code from here: its loops' regions (loopreg.h) */
    switch (kind) {
    case PF_UNTIL_EXIT: {
        /* UNTIL EXIT: a condition that never holds (14.9.28.4 rule 11); an
         * EXIT PERFORM, a GOBACK or a STOP leaves it (cobol ISSUES-90) */
        int Ltop = new_label();
        emit_label(Ltop);
        emit_body(&body);
        emit_jump(Ltop);
        break;
    }
    case PF_UNTIL: {
        int Lbody = new_label(), Ltest = new_label();
        if (body.inline_body) lr_begin();
        if (!test_after) emit_jump(Ltest);      /* WITH TEST AFTER falls into the body */
        emit_label(Lbody);
        emit_body(&body);
        emit_label(Ltest);
        cond_jump_false(c, Lbody);
        if (body.inline_body) lr_end();
        break;
    }
    case PF_VARYING:
        if (test_after && nv > 1) emit_varying_test_after(v, nv, &body);
        else emit_varying(v, nv, 0, &body, test_after);
        break;
    case PF_TIMES: {
        if (g_ncnt == g_cnt_cap) { g_cnt_cap = g_cnt_cap ? 2 * g_cnt_cap : 64; g_cnt_unit = realloc(g_cnt_unit, (size_t)g_cnt_cap * sizeof *g_cnt_unit); }
        g_cnt_unit[g_ncnt] = g_unit;
        char cnt[32]; snprintf(cnt, sizeof cnt, ".Lcnt%d", g_ncnt++);
        emit_incompat(&n);
        if (opnd_hot_int(&n)) emit_hot_value(&n);
        else {
            if (n.kind != O_REF) die_at(n.line, "TIMES needs an integer");
            if (n.ref.sym->usage == U_FLOAT) die_at(n.line, "TIMES needs an integer, not the floating-point item '%s' (Micro Focus: PERFORM rules)", n.ref.sym->name);
            Arg a[2] = { arg_ref(&n.ref), arg_desc(sym_desc(n.ref.sym)) };
            emit_args(a, 2); emit_call("cob_load_int");
        }
        emit_la("r2", cnt);
        emit("\tstw r2+0, r1");
        /* the count left to do, in r1 at the test: the whole count coming
         * in, one less after each execution of the body */
        int Lbody = new_label(), Ltest = new_label();
        if (body.inline_body) lr_begin();       /* (r1, the count, is live here: what a region loads its items with leaves it alone) */
        emit_jump(Ltest);
        emit_label(Lbody);
        emit_body(&body);
        emit_la("r2", cnt);
        emit("\tldw r1, r2+0");
        emit("\taddi r1, r1, -1");
        emit("\tstw r2+0, r1");
        emit_label(Ltest);
        emit("\tblt r0, r1, .L%d", Lbody);
        if (body.inline_body) lr_end();
        break;
    }
    default:
        emit_body(&body);
        break;
    }
    if (body.inline_body && body.Lexit >= 0) emit_label(body.Lexit);
    if (body.inline_body && !g_inline_depth) {
        /* the outermost in-line PERFORM: its islands, then its loops and those
         * in them -- unless the whole loop is one node already: its text is
         * then cut away with the node, and resolved only if it comes back */
        if (!lw_loop_folded_at(lay0)) lw_resolve(lay0);
        lr_run(lay0);
    }
    /* an out-of-line PERFORM has no END-PERFORM: the next one belongs to
     * whatever inline PERFORM encloses this statement */
}
