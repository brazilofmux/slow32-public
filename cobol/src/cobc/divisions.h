/* s32-cobc: the other divisions.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ====================================================================== */
/* The other divisions                                                     */
/* ====================================================================== */

static int at_division(void)
{
    return cur()->kind == T_WORD && is_word(peek(1), "division") &&
           (at_word("environment") || at_word("data") || at_word("procedure"));
}

static void parse_identification_division(void)
{
    if (accept_word("identification") || accept_word("id")) { expect_word("division"); expect_period(); }
    else if (!unit_start(cur()))
        die_at(cur()->line, "expected IDENTIFICATION DIVISION, found %s", tok_desc(cur()));
    else if (g_std < 2002)
        die_at(cur()->line, "a program without its IDENTIFICATION DIVISION header is COBOL 2002 (11.1.1); compile with -std=2002 -- X3.23-1985 requires the header");
    g_is_function = 0; g_returning = NULL; g_nrepo_fn = 0; g_repo_all_intrinsic = 0;
    if (at_word("function-id")) {
        /* COBOL 2002 11.5: a user-defined function, always recursive */
        if (g_std < 2002) die_at(cur()->line, "FUNCTION-ID is COBOL 2002; compile with -std=2002 (docs/standards.md, Stage B)");
        if (g_udepth) die_at(cur()->line, "a function definition cannot be contained in a program");
        g_is_function = 1; g_recursive = 1;
    } else expect_word("program-id");
    if (g_is_function) advance();
    expect_period();
    if (cur()->kind != T_WORD) die_at(cur()->line, "expected a program-name");
    snprintf(g_progid, sizeof g_progid, "%s", cur()->s);
    snprintf(g_progid_orig, sizeof g_progid_orig, "%s", tok_orig(cur()));
    advance();
    if (g_is_function) {
        if (accept_word("as")) die_at(cur()->line, "FUNCTION-ID ... AS literal is not implemented yet");
        if (accept_word("is") || at_word("prototype")) {
            if (at_word("prototype")) die_at(cur()->line, "function prototypes (IS PROTOTYPE) are not implemented yet; the caller finds the definition's signature file");
            die_at(cur()->line, "expected '.' after the function name, found %s", tok_desc(cur()));
        }
    }
    accept_word("is");
    for (;;) {
        int line = cur()->line;
        if (accept_word("initial")) {
            g_initial = 1;                                       /* fresh WORKING-STORAGE on every CALL */
            if (g_recursive)
                die_at(line, "INITIAL: a program that is, or is contained in, a RECURSIVE program cannot be INITIAL (2023 11.10.3 rule 5)");
        }
        else if (accept_word("common")) { }                      /* callable by the siblings too: every program here is */
        else if (accept_word("recursive")) {
            if (g_std < 2002) die_at(line, "RECURSIVE is COBOL 2002; compile with -std=2002 (docs/standards.md, Stage B)");
            for (int k = 0; k < g_udepth; k++)
                if (g_ustack[k]->initial) die_at(line, "RECURSIVE: a program contained in an INITIAL program cannot be RECURSIVE (2023 11.10.3 rule 6)");
            g_recursive = 1;
        }
        else break;
    }
    if (g_initial && g_recursive) die_at(cur()->line, "a program cannot be both INITIAL and RECURSIVE");
    accept_word("program");
    expect_period();

    static const char *paras[] = { "author", "installation", "date-written",
        "date-compiled", "security", "remarks", NULL };
    while (!at_division() && cur()->kind != T_EOF) {
        Tok *t = cur();
        int known = 0;
        for (int i = 0; paras[i]; i++) if (is_word(t, paras[i])) known = 1;
        if (!known) die_at(t->line, "unexpected %s in the IDENTIFICATION DIVISION", tok_desc(t));
        /* deleted by 2002, and taken there as an extension: the
         * paragraphs are comments in any edition that had them */
        bp(g_std >= 2002 ? BP_E24_COMMENT_ENTRY_2002 : BP_O2_COMMENT_ENTRY, t->line);
        advance(); expect_period();
        while (!at_division() && cur()->kind != T_EOF) {
            int hdr = 0;
            for (int i = 0; paras[i]; i++) if (at_word(paras[i]) && peek(1)->kind == T_PERIOD) hdr = 1;
            if (hdr) break;
            advance();
        }
    }
}

static void skip_to_period(void) __attribute__((unused));
static void skip_to_period(void)
{
    while (cur()->kind != T_PERIOD && cur()->kind != T_EOF) advance();
    expect_period();
}

/* SELECT [OPTIONAL] file ASSIGN TO ... [ORGANIZATION ...] [ACCESS ...]
 * [RECORD KEY ...] [FILE STATUS ...] [SHARING ...]. */
/* after a key's name, SOURCE IS data-name ... (2002 12.3.4.12, a
 * record-key-name: the parts' concatenation) or "= data-name ..." (Micro
 * Focus's spelling, BP-D2): the parts, to the next clause or the period */
static int parse_split_parts(char (**out)[64], int line, int *mf)
{
    *mf = 0;
    if (at_word("source")) {
        if (g_std < 2002) die_at(line, "a record key SOURCE IS ... (a split key) is COBOL 2002; compile with -std=2002");
        advance(); accept_word("is");
    } else if (at_op("=")) {
        bp(BP_D2_MF_SPLIT_KEY, line);
        advance(); *mf = 1;
    } else return 0;
    static const char *stop[] = { "alternate", "access", "assign", "organization", "organisation", "record", "with",
        "duplicates", "file", "lock", "reserve", "password", "suppress", "sharing", "relative", "collating", "status", NULL };
    char (*p)[64] = xmalloc(16 * sizeof *p); int n = 0;
    while (cur()->kind == T_WORD) {
        int halt = 0;
        for (int k = 0; stop[k]; k++) if (at_word(stop[k])) halt = 1;
        if (halt) break;
        if (n == 16) die_at(line, "a split key of more than 16 parts");
        snprintf(p[n++], 64, "%s", cur()->s); advance();
    }
    if (!n) die_at(line, "a split key needs its data-names after '='");
    *out = p;
    return n;
}

static void parse_select(void)
{
    int line = cur()->line;
    if (g_nfile == g_fcap) { g_fcap = g_fcap ? g_fcap * 2 : 16; g_files = realloc(g_files, g_fcap * sizeof *g_files); }
    File *f = &g_files[g_nfile++];
    memset(f, 0, sizeof *f);
    f->line = line; f->rec = -1; f->org = COB_ORG_SEQ; f->unit = g_unit;
    if (accept_word("optional")) f->optional = 1;
    if (cur()->kind != T_WORD) die_at(line, "expected a file-name after SELECT");
    if (file_find(cur()->s)) die_at(line, "file '%s' is SELECTed twice", cur()->s);
    user_word(cur()->s, line, "a file");
    snprintf(f->name, sizeof f->name, "%s", cur()->s);
    snprintf(f->oname, sizeof f->oname, "%s", tok_orig(cur()));
    advance();
    int has_assign = 0;
    while (cur()->kind != T_PERIOD) {
        Tok *t = cur();
        if (t->kind != T_WORD) die_at(t->line, "unexpected %s in SELECT %s", tok_desc(t), f->name);
        if (accept_word("assign")) {
            accept_word("to");
            if (cur()->kind == T_STR) { f->assign_lit = cur(); advance(); }
            else if (cur()->kind == T_PERIOD || at_word("file") || at_word("organization") || at_word("organisation") || at_word("access") || at_word("record") || at_word("status"))
                has_assign = -1;                        /* nothing named: allowed for an EXTERNAL file */
            else if (cur()->kind == T_WORD) {
                /* RM/COBOL's device word before the name -- ASSIGN TO RANDOM
                 * "GLMAST.GLDATA", ASSIGN TO PRINT "PRINTER" -- says nothing on
                 * this machine: accepted and ignored, as SHARING is (GitHub #34).
                 * A device word with nothing after it is still refused. */
                static const char *devs[] = { "random", "print", "printer", "disk", "input", "output", "input-output",
                                              "display", "keyboard", "tape", "cassette", NULL };
                static const char *clauses[] = { "organization", "organisation", "access", "record", "status", "file",
                                                 "sequential", "indexed", "relative", "line", "lock", "sharing", "key",
                                                 "alternate", "reserve", "padding", "data", "block", NULL };
                int dev = 0, clause_next = peek(1)->kind != T_STR && peek(1)->kind != T_WORD;
                for (int i = 0; devs[i]; i++) if (at_word(devs[i])) dev = 1;
                for (int i = 0; clauses[i]; i++) if (is_word(peek(1), clauses[i])) clause_next = 1;
                if (dev && !clause_next) advance();
                else if (dev)
                    die_at(t->line, "ASSIGN TO %s (a device) is not supported; name a file", cur()->s);
                if (cur()->kind == T_STR) { f->assign_lit = cur(); advance(); }
                else if (cur()->kind == T_WORD) { snprintf(f->assign_name, sizeof f->assign_name, "%s", cur()->s); advance(); }
                else die_at(t->line, "expected a literal or data-name after ASSIGN TO");
            } else die_at(t->line, "expected a literal or data-name after ASSIGN TO");
            has_assign = 1;
            continue;
        }
        if (at_word("sequential") || at_word("indexed") || (at_word("line") && is_word(peek(1), "sequential"))) {
            /* ORGANIZATION IS may be omitted */
            f->org_given = 1;
            if (accept_word("line")) { bp(BP_E12_LINE_SEQUENTIAL, cur()->line); expect_word("sequential"); f->org = COB_ORG_LINESEQ; }
            else if (accept_word("sequential")) f->org = COB_ORG_SEQ;
            else { advance(); f->org = COB_ORG_INDEXED; }
            continue;
        }
        if (accept_word("organization") || accept_word("organisation")) {
            accept_word("is"); f->org_given = 1;
            if (accept_word("line")) { bp(BP_E12_LINE_SEQUENTIAL, cur()->line); expect_word("sequential"); f->org = COB_ORG_LINESEQ; }
            else if (accept_word("sequential")) f->org = COB_ORG_SEQ;
            else if (accept_word("indexed")) f->org = COB_ORG_INDEXED;
            else if (accept_word("relative")) f->org = COB_ORG_RELATIVE;
            else die_at(t->line, "unknown ORGANIZATION %s", cur()->s);
            continue;
        }
        if (accept_word("access")) {
            accept_word("mode"); accept_word("is");
            if (accept_word("sequential")) f->access = 0;
            else if (accept_word("random")) f->access = 1;
            else if (accept_word("dynamic")) f->access = 2;
            else die_at(t->line, "unknown ACCESS MODE %s", cur()->s);
            continue;
        }
        if (accept_word("record")) {
            if (accept_word("delimiter")) { accept_word("is"); if (cur()->kind == T_WORD) advance(); continue; }   /* RECORD DELIMITER IS STANDARD-1 */
            accept_word("key"); accept_word("is");
            if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after RECORD KEY");
            snprintf(f->key_name, sizeof f->key_name, "%s", cur()->s); advance();
            if ((at_word("in") || at_word("of")) && peek(1)->kind == T_WORD) { advance(); snprintf(f->key_qual, sizeof f->key_qual, "%s", cur()->s); advance(); }
            f->nksplit = parse_split_parts(&f->ksplit, t->line, &f->ksplit_mf);
            continue;
        }
        if (accept_word("alternate")) {
            /* ALTERNATE [RECORD] [KEY] [IS] data-name [WITH DUPLICATES] */
            accept_word("record"); accept_word("key"); accept_word("is");
            if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after ALTERNATE RECORD KEY");
            if (f->nalt == 16) die_at(t->line, "too many ALTERNATE RECORD KEYs (16)");
            snprintf(f->alt[f->nalt].name, sizeof f->alt[f->nalt].name, "%s", cur()->s); advance();
            if ((at_word("in") || at_word("of")) && peek(1)->kind == T_WORD) { advance(); snprintf(f->alt[f->nalt].qual, sizeof f->alt[f->nalt].qual, "%s", cur()->s); advance(); }
            f->alt[f->nalt].nsplit = parse_split_parts(&f->alt[f->nalt].split, t->line, &f->alt[f->nalt].split_mf);
            if (accept_word("with")) { expect_word("duplicates"); f->alt[f->nalt].dups = 1; }
            else if (accept_word("duplicates")) f->alt[f->nalt].dups = 1;
            f->nalt++;
            continue;
        }
        if (accept_word("relative")) {
            /* RELATIVE [KEY IS] data-name -- or ORGANIZATION IS omitted before
             * a bare RELATIVE, told apart by what follows */
            static const char *clause_words[] = { "access", "assign", "organization", "organisation", "record",
                "alternate", "file", "status", "sharing", "lock", "reserve", "padding", "sequential", "indexed",
                "relative", "line", "select", NULL };
            int has_key = accept_word("key");
            if (has_key) accept_word("is");
            int is_clause = 0;
            if (cur()->kind == T_WORD) for (int k = 0; clause_words[k]; k++) if (!strcmp(cur()->s, clause_words[k])) is_clause = 1;
            if (has_key || (cur()->kind == T_WORD && !is_clause)) {
                if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after RELATIVE KEY");
                snprintf(f->relkey_name, sizeof f->relkey_name, "%s", cur()->s); advance();
            } else { f->org_given = 1; f->org = COB_ORG_RELATIVE; }
            continue;
        }
        if (at_word("file") || at_word("status")) {
            accept_word("file"); expect_word("status"); accept_word("is");
            if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after FILE STATUS");
            snprintf(f->status_name, sizeof f->status_name, "%s", cur()->s); advance();
            if (accept_word("of") || accept_word("in")) {           /* status-name OF group */
                if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after OF/IN");
                snprintf(f->status_qual, sizeof f->status_qual, "%s", cur()->s); advance();
            }
            continue;
        }
        if (accept_word("padding")) {           /* PADDING CHARACTER: block padding, no blocks here */
            accept_word("character"); accept_word("is");
            if (cur()->kind == T_STR || cur()->kind == T_WORD) advance();
            continue;
        }
        if (accept_word("reserve")) {           /* RESERVE n AREAS: buffering is the host's */
            f->reserve_given = 1;
            if (cur()->kind == T_NUM || at_word("no")) advance();
            accept_word("area"); accept_word("areas");
            continue;
        }
        if (accept_word("sharing")) {
            /* SHARING WITH ALL OTHER: accepted and ignored on this machine */
            accept_word("with");
            if (accept_word("all")) accept_word("other");
            else if (accept_word("no")) accept_word("other");
            else if (accept_word("read")) accept_word("only");
            continue;
        }
        if (accept_word("lock")) {
            accept_word("mode"); accept_word("is");
            while (cur()->kind == T_WORD && !at_word("assign") && !at_word("organization") &&
                   !at_word("access") && !at_word("file") && !at_word("record") && !at_word("sharing")) advance();
            continue;
        }
        if (accept_word("reserve")) { while (cur()->kind != T_PERIOD && !at_word("organization") && !at_word("access") && !at_word("file")) advance(); continue; }
        die_at(t->line, "unexpected %s in SELECT %s", tok_desc(t), f->name);
    }
    expect_period();
    if (!has_assign) die_at(line, "SELECT %s has no ASSIGN clause", f->name);
    if (has_assign < 0) f->assign_name[0] = 0, f->assign_lit = NULL;   /* checked against EXTERNAL once the FD is in */
    if (f->org == COB_ORG_INDEXED && !f->key_name[0]) die_at(line, "an INDEXED file needs RECORD KEY");
}

/* REPOSITORY (COBOL 2002 12.3.8): FUNCTION name ... makes user functions
 * invocable without the word FUNCTION; FUNCTION ALL INTRINSIC and
 * FUNCTION name ... INTRINSIC do the same for intrinsics. */
static void parse_repository(void)
{
    advance(); expect_period();
    while (at_word("function")) {
        int line = cur()->line;
        advance();
        if (accept_word("all")) { expect_word("intrinsic"); g_repo_all_intrinsic = 1; continue; }
        int first = g_nrepo_fn;
        while (cur()->kind == T_WORD && !at_word("function") && !at_word("intrinsic") && !at_division() &&
               !at_word("input-output") && !at_word("special-names") && !at_word("select")) {
            if (at_word("as")) die_at(cur()->line, "REPOSITORY FUNCTION ... AS literal is not implemented yet");
            if (g_nrepo_fn == 32) die_at(cur()->line, "more than 32 functions in REPOSITORY");
            snprintf(g_repo_fn[g_nrepo_fn++], sizeof g_repo_fn[0], "%s", cur()->s);
            advance();
        }
        if (g_nrepo_fn == first) die_at(line, "REPOSITORY FUNCTION needs a function name, or ALL INTRINSIC");
        if (accept_word("intrinsic")) {
            /* intrinsics named individually: invocable without FUNCTION, like ALL INTRINSIC does for all */
            for (int k = first; k < g_nrepo_fn; k++) if (!fn89_known(g_repo_fn[k]))
                die_at(line, "'%s' is not an intrinsic function", g_repo_fn[k]);
            g_repo_all_intrinsic = 1;         /* narrower in the text; the names are checked, the rest is harmless */
            g_nrepo_fn = first;
        }
    }
    for (;;) {
        if (at_word("class") || at_word("interface") || at_word("program") || at_word("property"))
            die_at(cur()->line, "REPOSITORY %s is object orientation or a program prototype, not implemented", cur()->s);
        break;
    }
    if (cur()->kind == T_PERIOD) advance();
}

static void parse_environment_division(void)
{
    if (!accept_word("environment")) return;
    expect_word("division"); expect_period();
    if (accept_word("configuration")) {
        expect_word("section"); expect_period();
        for (;;) {
            if (accept_word("source-computer") || accept_word("object-computer")) {
                expect_period();
                while ((cur()->kind == T_WORD || cur()->kind == T_NUM) && !at_word("special-names") && !at_word("input-output") && !at_word("repository") && !at_word("select") &&
                       !at_word("source-computer") && !at_word("object-computer") && !at_division()) {   /* MEMORY SIZE 64000 CHARACTERS: obsolete, no effect */
                    if (at_word("memory")) bp(BP_O5_MEMORY_SIZE, cur()->line);
                    if (accept_word("collating")) {         /* [PROGRAM] COLLATING SEQUENCE IS alphabet-name */
                        accept_word("sequence"); accept_word("is");
                        if (cur()->kind != T_WORD) die_at(cur()->line, "expected an alphabet-name after COLLATING SEQUENCE");
                        snprintf(g_collate_name, sizeof g_collate_name, "%s", cur()->s);
                    }
                    advance();
                }
                if (cur()->kind == T_PERIOD) advance();
                continue;
            }
            if (at_word("special-names")) {
                advance(); expect_period();
                for (;;) {
                    if (cur()->kind == T_PERIOD) { advance(); continue; }
                    if (accept_word("class")) {
                        if (cur()->kind != T_WORD) die_at(cur()->line, "expected a class-name after CLASS");
                        if (g_nclass == (int)(sizeof g_class / sizeof g_class[0])) die_at(cur()->line, "too many CLASS clauses");
                        user_word(cur()->s, cur()->line, "a class");
                        UClass *uc = &g_class[g_nclass++];
                        memset(uc, 0, sizeof *uc);
                        snprintf(uc->name, sizeof uc->name, "%s", cur()->s); advance();
                        accept_word("is");
                        int any = 0;
                        while (cur()->kind == T_STR) {
                            Tok *lo = cur(); advance();
                            if (at_word("through") || at_word("thru")) {
                                /* a range: one character to one character */
                                advance();
                                if (lo->len != 1) die_at(lo->line, "CLASS %s: THROUGH takes one-character literals", uc->name);
                                if (cur()->kind != T_STR || cur()->len != 1) die_at(cur()->line, "CLASS %s: THROUGH needs a one-character literal", uc->name);
                                unsigned a = (unsigned char)lo->s[0], b = (unsigned char)cur()->s[0]; advance();
                                if (b < a) { unsigned t = a; a = b; b = t; }
                                for (unsigned c = a; c <= b; c++) uc->tab[c] = 1;
                            } else {
                                /* every character of the literal is in the class ("ABCD") */
                                if (lo->len < 1) die_at(lo->line, "CLASS %s: an empty literal", uc->name);
                                for (int k = 0; k < lo->len; k++) uc->tab[(unsigned char)lo->s[k]] = 1;
                            }
                            any = 1;
                        }
                        if (!any) die_at(cur()->line, "CLASS %s: expected a one-character literal", uc->name);
                        continue;
                    }
                    if (cur()->kind == T_WORD && !strncmp(cur()->s, "switch-", 7) && isdigit((unsigned char)cur()->s[7])) {
                        int sw = atoi(cur()->s + 7); advance();
                        if (sw < 1 || sw > 8) die_at(cur()->line, "SWITCH-%d: switches are 1 to 8", sw);
                        if (accept_word("is")) {
                            if (cur()->kind != T_WORD) die_at(cur()->line, "expected a mnemonic-name after SWITCH-%d IS", sw);
                            if (g_nswitch == 32) die_at(cur()->line, "too many switch names");
                            SwitchName *m = &g_switch[g_nswitch++];
                            snprintf(m->name, sizeof m->name, "%s", cur()->s); m->sw = sw; m->on = -1; advance();
                        }
                        while (at_word("on") || at_word("off")) {
                            int on = accept_word("on"); if (!on) accept_word("off");
                            accept_word("status"); accept_word("is");
                            if (cur()->kind != T_WORD) die_at(cur()->line, "expected a condition-name after ON/OFF STATUS");
                            if (g_nswitch == 32) die_at(cur()->line, "too many switch names");
                            SwitchName *m = &g_switch[g_nswitch++];
                            snprintf(m->name, sizeof m->name, "%s", cur()->s); m->sw = sw; m->on = on; advance();
                        }
                        continue;
                    }
                    if (accept_word("symbolic")) {
                        /* SYMBOLIC [CHARACTERS] {name... {IS|ARE} integer...}... [IN alphabet-name] */
                        accept_word("characters");
                        for (;;) {
                            char names[32][64]; int nn = 0;
                            while (cur()->kind == T_WORD && !at_word("is") && !at_word("are") && !at_word("in")) {
                                if (nn == 32) die_at(cur()->line, "SYMBOLIC CHARACTERS: too many names in one list");
                                user_word(cur()->s, cur()->line, "a symbolic character");
                                snprintf(names[nn++], 64, "%s", cur()->s); advance();
                            }
                            if (!nn) die_at(cur()->line, "SYMBOLIC CHARACTERS: expected a name");
                            if (!accept_word("is")) accept_word("are");
                            int ni = 0;
                            while (cur()->kind == T_NUM) {
                                if (ni >= nn) die_at(cur()->line, "SYMBOLIC CHARACTERS: more integers than names");
                                int ord = atoi(cur()->s);
                                if (ord < 1 || ord > 256) die_at(cur()->line, "SYMBOLIC CHARACTERS: the ordinal position is 1 through 256");
                                if (g_nsymch == 32) die_at(cur()->line, "too many SYMBOLIC CHARACTERS");
                                if (symch_find(names[ni]) >= 0) die_at(cur()->line, "SYMBOLIC CHARACTERS: '%s' is named twice", names[ni]);
                                memcpy(g_symch[g_nsymch].name, names[ni], sizeof g_symch[0].name);
                                g_symch[g_nsymch].byte = ord - 1; g_nsymch++;
                                ni++; advance();
                            }
                            if (ni != nn) die_at(cur()->line, "SYMBOLIC CHARACTERS: %d names but %d integers", nn, ni);
                            if (accept_word("in")) {
                                /* IN alphabet-name: the native sequence is the one there is */
                                if (cur()->kind != T_WORD) die_at(cur()->line, "SYMBOLIC CHARACTERS IN needs an alphabet-name");
                                advance(); break;
                            }
                            if (cur()->kind != T_WORD || at_word("class") || at_word("currency") || at_word("decimal-point") || at_word("alphabet") || at_word("symbolic") || switch_find(cur()->s)) break;
                            if (mnemonic_kind(cur()->s) >= 0 || !strncmp(cur()->s, "switch-", 7) || at_word("sysin") || at_word("sysout") || at_word("console") || at_word("syserr") || at_word("formfeed")) break;
                        }
                        continue;
                    }
                    if (at_word("crt") && is_word(peek(1), "status")) {
                        advance(); advance(); accept_word("is");
                        if (cur()->kind != T_WORD) die_at(cur()->line, "CRT STATUS IS needs a data-name");
                        snprintf(g_crt_status_name, sizeof g_crt_status_name, "%s", cur()->s);
                        advance(); continue;
                    }
                    if (accept_word("currency")) {            /* already applied to the pictures; see apply_decimal_point */
                        accept_word("sign"); accept_word("is");
                        if (cur()->kind != T_STR) die_at(cur()->line, "CURRENCY SIGN needs a literal");
                        advance(); continue;
                    }
                    if (accept_word("decimal-point")) {       /* already applied to the text; see apply_decimal_point */
                        accept_word("is");
                        if (!accept_word("comma")) die_at(cur()->line, "DECIMAL-POINT IS COMMA is the only form");
                        continue;
                    }
                    if (accept_word("alphabet")) {
                        if (cur()->kind != T_WORD) die_at(cur()->line, "expected an alphabet-name after ALPHABET");
                        if (g_nalphabet == 16) die_at(cur()->line, "too many ALPHABET clauses");
                        user_word(cur()->s, cur()->line, "an alphabet");
                        Alphabet *a = &g_alphabet[g_nalphabet++];
                        snprintf(a->name, sizeof a->name, "%s", cur()->s); advance();
                        accept_word("is");
                        if (accept_word("native") || accept_word("standard-1") || accept_word("standard-2")) a->native = 1;
                        else if (accept_word("ebcdic")) {
                            /* EBCDIC (the user's ruling of 2026-09-28): a collating sequence
                             * and a CODE-SET, each character at its CP037 code.  The data
                             * stays in the machine's code; only order and file bytes change */
                            a->native = 0; a->ebcdic = 1;
                            memcpy(a->rank, g_cp037, 256);
                        }
                        else {
                            /* literal phrases: lit [THROUGH lit | ALSO lit ...] ... -- the
                             * characters named take the first collating positions in that
                             * order (ALSO: the same position), the rest follow in native order */
                            a->native = 0;
                            int seen[256] = { 0 }, rank = 0, any = 0;
                            #define ALPHA_CH(tok, out) do { \
                                Tok *_t = (tok); \
                                if (_t->kind == T_STR) { if (_t->len != 1) die_at(_t->line, "ALPHABET %s: a literal of one character (or THROUGH a range)", a->name); *(out) = (unsigned char)_t->s[0]; } \
                                else if (_t->kind == T_NUM) { int _v = atoi(_t->s); if (_v < 1 || _v > 256) die_at(_t->line, "ALPHABET %s: an ordinal position is 1 to 256", a->name); *(out) = (unsigned char)(_v - 1); } \
                                else if (_t->kind == T_WORD && is_figurative(_t->s)) *(out) = (unsigned char)fig_byte(_t->s); \
                                else die_at(_t->line, "ALPHABET %s: expected a literal", a->name); } while (0)
                            for (;;) {
                                Tok *lo = cur();
                                if (!(lo->kind == T_STR || lo->kind == T_NUM || (lo->kind == T_WORD && is_figurative(lo->s)))) break;
                                if (lo->kind == T_STR && lo->len > 1) {
                                    /* a longer literal: each character in turn */
                                    for (int c = 0; c < lo->len; c++) { unsigned char ch = (unsigned char)lo->s[c]; if (!seen[ch]) { seen[ch] = 1; a->rank[ch] = (unsigned char)rank++; } }
                                    advance(); any = 1; continue;
                                }
                                unsigned char c1; ALPHA_CH(lo, &c1); advance();
                                if (accept_word("through") || accept_word("thru")) {
                                    unsigned char c2 = 0; ALPHA_CH(cur(), &c2); advance();
                                    int step = c2 >= c1 ? 1 : -1;
                                    for (int c = c1; ; c += step) { if (!seen[c]) { seen[c] = 1; a->rank[c] = (unsigned char)rank++; } if (c == c2) break; }
                                } else {
                                    if (!seen[c1]) { seen[c1] = 1; a->rank[c1] = (unsigned char)rank; }
                                    while (accept_word("also")) { unsigned char c3 = 0; ALPHA_CH(cur(), &c3); advance(); if (!seen[c3]) { seen[c3] = 1; a->rank[c3] = (unsigned char)rank; } }
                                    rank++;
                                }
                                any = 1;
                            }
                            #undef ALPHA_CH
                            if (!any) die_at(cur()->line, "ALPHABET %s: expected NATIVE, STANDARD-1 or literals", a->name);
                            for (int c = 0; c < 256; c++) if (!seen[c]) a->rank[c] = (unsigned char)(rank < 255 ? rank++ : 255);
                        }
                        continue;
                    }
                    if (cur()->kind == T_WORD) {
                        int mk = 0;
                        if (at_word("sysin") || at_word("stdin") || at_word("sysipt")) mk = 1;
                        else if (at_word("sysout") || at_word("stdout") || at_word("console") || at_word("syslst") || at_word("sysprint")) mk = 2;
                        else if (at_word("syserr") || at_word("stderr")) mk = 4;
                        else if (at_word("formfeed") || at_word("c01") || at_word("csp")) mk = 3;
                        if (mk) {
                            advance(); accept_word("is");
                            if (cur()->kind != T_WORD) die_at(cur()->line, "expected a mnemonic-name after the device name");
                            if (g_nmnemonic == 16) die_at(cur()->line, "too many mnemonic-names");
                            user_word(cur()->s, cur()->line, "a mnemonic");
                            Mnemonic *m = &g_mnemonic[g_nmnemonic++];
                            snprintf(m->name, sizeof m->name, "%s", cur()->s); m->kind = mk; advance();
                            continue;
                        }
                    }
                    if (at_division() || at_word("input-output") || at_word("repository") || at_word("select")) break;
                    die_at(cur()->line, "SPECIAL-NAMES clause '%s' is not implemented yet (CLASS, SWITCH-n, ALPHABET and the device names are)", cur()->s);
                }
                continue;
            }
            if (at_word("repository")) {
                if (g_std < 2002) die_at(cur()->line, "REPOSITORY is COBOL 2002; compile with -std=2002, or rewrite user-defined functions as CALL (docs/functions.md)");
                parse_repository();
                continue;
            }
            break;
        }
    }
    if (g_collate_name[0]) {
        int found = -1;
        for (int i = 0; i < g_nalphabet; i++) if (!strcmp(g_alphabet[i].name, g_collate_name)) found = i;
        if (found < 0) die_at(cur()->line, "PROGRAM COLLATING SEQUENCE '%s' is not an ALPHABET of SPECIAL-NAMES", g_collate_name);
        if (!g_alphabet[found].native) {
            g_collate = found;
            /* LOW-VALUE and HIGH-VALUE are the sequence's first and last characters */
            int lo = 0, hi = 0;
            for (int c = 0; c < 256; c++) { if (g_alphabet[found].rank[c] < g_alphabet[found].rank[lo]) lo = c; if (g_alphabet[found].rank[c] >= g_alphabet[found].rank[hi]) hi = c; }
            g_lowval = lo; g_highval = hi;
        }
    }
    if (at_word("select")) {
        /* neither header: Micro Focus's practice (BP-D1) */
        bp(BP_D1_MF_NO_FILE_CONTROL, cur()->line);
        while (accept_word("select")) parse_select();
    }
    if (accept_word("input-output")) {
        expect_word("section"); expect_period();
        if (accept_word("file-control")) {
            expect_period();
            while (accept_word("select")) parse_select();
        } else if (at_word("select")) {
            bp(BP_D1_MF_NO_FILE_CONTROL, cur()->line);   /* the paragraph header left out (Micro Focus) */
            while (accept_word("select")) parse_select();
        }
        if (accept_word("i-o-control")) {
            /* SAME RECORD AREA means what it says; SAME AREA / SORT AREA,
             * RERUN and MULTIPLE FILE TAPE are hints for machines with tapes
             * and scarce memory, and are read past */
            expect_period();
            while (!at_division() && cur()->kind != T_EOF) {
                if (at_word("rerun")) bp(BP_O10_RERUN, cur()->line);
                if (at_word("multiple") && is_word(cur() + 1, "file")) bp(BP_O11_MULTIPLE_FILE, cur()->line);
                if (accept_word("same")) {
                    int is_record = accept_word("record");
                    if (!is_record) { accept_word("sort"); accept_word("sort-merge"); }
                    accept_word("area"); accept_word("for");
                    int g = -1;
                    if (is_record) {
                        if (g_nsame_groups == 8) die_at(cur()->line, "too many SAME RECORD AREA clauses");
                        g = g_nsame_groups++; g_nsame[g] = 0;
                    }
                    while (cur()->kind == T_WORD && file_find(cur()->s)) {
                        if (g >= 0 && g_nsame[g] < 16) g_same[g][g_nsame[g]++] = (int)(file_find(cur()->s) - g_files);
                        advance();
                    }
                    continue;
                }
                advance();
            }
        }
    }
    if (!at_division()) die_at(cur()->line, "unexpected %s in the ENVIRONMENT DIVISION", tok_desc(cur()));
}

/* FD file-name [clauses]. followed by its 01s */
static void parse_fd(void)
{
    int line = cur()->line;
    int is_sd = accept_word("sd");
    if (!is_sd) expect_word("fd");
    if (cur()->kind != T_WORD) die_at(line, "expected a file-name after %s", is_sd ? "SD" : "FD");
    File *f = file_find(cur()->s);
    if (!f) die_at(line, "%s %s has no SELECT", is_sd ? "SD" : "FD", cur()->s);
    if (f->fd_line) die_at(line, "%s %s: the file already has an FD at line %d", is_sd ? "SD" : "FD", f->name, f->fd_line);
    f->fd_line = line;
    f->lin_counter_sym = -1;
    if (is_sd) f->org = COB_ORG_SORT;         /* a sort file: SORT opens it, RELEASE/RETURN use it */
    advance();
    while (cur()->kind != T_PERIOD) {
        Tok *t = cur();
        if (t->kind != T_WORD) die_at(t->line, "unexpected %s in FD %s", tok_desc(t), f->name);
        if (accept_word("block")) {
            /* BLOCK CONTAINS: a blocking hint with no meaning on a byte stream */
            accept_word("contains"); f->block_given = 1;
            if (cur()->kind == T_NUM) advance();
            if (accept_word("to")) { if (cur()->kind == T_NUM) advance(); }
            accept_word("records"); accept_word("characters");
            continue;
        }
        if (accept_word("record")) {
            if (accept_word("is") || accept_word("are")) { }
            if (accept_word("varying")) {
                accept_word("in"); accept_word("size");
                accept_word("from");
                if (cur()->kind == T_NUM) { f->minlen = atoi(cur()->s); f->rc_varying_from = 1; advance(); }
                if (accept_word("to")) { if (cur()->kind != T_NUM) die_at(t->line, "expected a number after TO"); f->maxlen = atoi(cur()->s); advance(); }
                if (f->rc_varying_from && f->maxlen && f->maxlen <= f->minlen)
                    die_at(t->line, "FD %s: RECORD VARYING FROM %d TO %d: the maximum must be greater than the minimum (2023 13.18.43.3 rule 9)", f->name, f->minlen, f->maxlen);
                accept_word("characters");
                if (accept_word("depending")) {
                    accept_word("on");
                    if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after DEPENDING ON");
                    snprintf(f->dep_name, sizeof f->dep_name, "%s", cur()->s); advance();
                }
                f->varying = 1;
                continue;
            }
            accept_word("contains"); f->rc_given = 1;
            if (cur()->kind != T_NUM) die_at(t->line, "expected a number after RECORD CONTAINS");
            f->minlen = atoi(cur()->s); advance();
            if (accept_word("to")) {
                if (cur()->kind != T_NUM) die_at(t->line, "expected a number after TO");
                f->maxlen = atoi(cur()->s); advance();
                if (f->maxlen <= f->minlen)
                    die_at(t->line, "FD %s: RECORD CONTAINS %d TO %d: the maximum must be greater than the minimum (%s)", f->name, f->minlen, f->maxlen,
                           g_std < 2002 ? "X3.23-1985 RECORD syntax rule 3" : "2023 13.18.43.3 rule 5");
                f->varying = 1;                     /* m TO n: variable, as cobc370 infers */
            } else { f->maxlen = f->minlen; }
            accept_word("characters");
            continue;
        }
        if (at_word("label")) bp(BP_O6_LABEL_RECORDS, cur()->line);
        if (accept_word("label")) { accept_word("record"); accept_word("records"); accept_word("is"); accept_word("are"); accept_word("standard"); accept_word("omitted"); continue; }
        if (at_word("data")) bp(BP_O8_DATA_RECORDS, cur()->line);
        if (accept_word("data")) { accept_word("record"); accept_word("records"); accept_word("is"); accept_word("are"); while (cur()->kind == T_WORD && !at_word("block") && !at_word("record") && !at_word("label") && !at_word("report") && !at_word("value")) { if (f->ndata_rec < 8) snprintf(f->data_rec[f->ndata_rec++], 64, "%s", cur()->s); advance(); } continue; }
        if (accept_word("report") || accept_word("reports")) {
            accept_word("is"); accept_word("are");
            if (cur()->kind != T_WORD) die_at(t->line, "expected a report-name");
            snprintf(f->report_name, sizeof f->report_name, "%s", cur()->s); advance();
            /* REPORTS ARE r1 r2 ...: several reports to one file (X3.23-1985
             * XIII 2.2; each told apart by its CODE, cobol ISSUES-94) */
            while (cur()->kind == T_WORD && !is_verb(cur()->s) && !at_word("label") && !at_word("block") && !at_word("record") &&
                   !at_word("records") && !at_word("data") && !at_word("value") && !at_word("recording") && !at_word("code-set") &&
                   !at_word("linage") && !at_word("external") && !at_word("global") && !at_word("is")) {
                f->report_more = xrealloc(f->report_more, (size_t)(f->nreport_more + 1) * sizeof *f->report_more);
                snprintf(f->report_more[f->nreport_more++], 64, "%s", cur()->s); advance();
            }
            continue;
        }
        if (accept_word("recording")) {
            accept_word("mode"); accept_word("is");
            if (accept_word("f")) { f->varying = 0; continue; }
            if (accept_word("v")) { f->varying = 1; continue; }
            die_at(t->line, "RECORDING MODE %s is refused (U and S are tapemgr's business; docs/framing.md)", cur()->s);
        }
        if (at_word("value")) bp(BP_O7_VALUE_OF, cur()->line);
        if (accept_word("value")) { expect_word("of"); while (cur()->kind != T_PERIOD && !at_word("block") && !at_word("record") && !at_word("data")) advance(); continue; }
        if (accept_word("is")) continue;
        if (accept_word("global")) { f->global = 1; continue; }
        if (accept_word("external")) { f->external = 1; continue; }
        if (accept_word("linage")) {
            /* LINAGE [IS] n [LINES] [WITH FOOTING [AT] f] [LINES AT TOP t] [LINES AT BOTTOM b] */
            accept_word("is");
            f->linage = 1;
            int which = 0;
            for (;;) {
                if (cur()->kind == T_NUM) { f->lin_lit[which] = atol(cur()->s); advance(); }
                else if (cur()->kind == T_WORD && !at_word("lines") && !at_word("with") && !at_word("footing") && !at_word("at") && !at_word("top") && !at_word("bottom")) { snprintf(f->lin_name[which], sizeof f->lin_name[which], "%s", cur()->s); advance(); }
                else die_at(t->line, "LINAGE: expected an integer or a data-name");
                if (which == 0) accept_word("lines");
                if (accept_word("with")) { expect_word("footing"); accept_word("at"); which = 1; continue; }
                if (accept_word("footing")) { accept_word("at"); which = 1; continue; }
                if (accept_word("lines")) { accept_word("at"); if (accept_word("top")) which = 2; else if (accept_word("bottom")) which = 3; else die_at(t->line, "LINAGE: LINES AT TOP or BOTTOM"); continue; }
                if (accept_word("at")) { if (accept_word("top")) which = 2; else if (accept_word("bottom")) which = 3; else die_at(t->line, "LINAGE: AT TOP or BOTTOM"); continue; }
                if (accept_word("top")) { which = 2; continue; }
                if (accept_word("bottom")) { which = 3; continue; }
                break;
            }
            /* the file's LINAGE-COUNTER: a four-byte unsigned cell in its cob_file */
            Sym *lc = sym_new();
            snprintf(lc->name, sizeof lc->name, "linage-counter");
            lc->line = t->line; lc->level = 77; lc->usage = U_COMP5; lc->has_usage = 1;
            lc->has_pic = 1; snprintf(lc->pic, sizeof lc->pic, "9(9)"); pic_analyse(lc->pic, &lc->pi);
            lc->size = 4; lc->lin_file = (int)(f - g_files);
            f = &g_files[lc->lin_file];              /* sym_new may have moved nothing of files; keep f */
            f->lin_counter_sym = sym_idx(lc);
            continue;
        }
        if (accept_word("code-set")) {
            accept_word("is");
            if (cur()->kind != T_WORD) die_at(t->line, "CODE-SET needs an alphabet-name");
            int found = -1;
            for (int i = 0; i < g_nalphabet; i++) if (!strcmp(g_alphabet[i].name, cur()->s)) found = i;
            if (found < 0) die_at(t->line, "CODE-SET: '%s' is not an alphabet-name", cur()->s);
            if (!g_alphabet[found].native && !g_alphabet[found].ebcdic)
                die_at(t->line, "CODE-SET %s: an alphabet given by literals is no code set (X3.23-1985 CODE-SET rule 2)", cur()->s);
            if (g_alphabet[found].ebcdic) {
                /* the records are EBCDIC on the medium: converted at READ and
                 * WRITE (85 CODE-SET general rule 1b), for a record sequential file */
                if (f->org != COB_ORG_SEQ)
                    die_at(t->line, "CODE-SET %s on a file that is not ORGANIZATION SEQUENTIAL is not implemented", cur()->s);
                f->codeset = found + 1; f->codeset_line = t->line;
            }
            advance(); continue;
        }
        die_at(t->line, "unexpected %s in FD %s", tok_desc(t), f->name);
    }
    expect_period();
    g_cur_fd = (int)(f - g_files);
    while (cur()->kind == T_NUM) parse_data_item();
    g_cur_fd = -1;
    /* 2023 12.4.5.2 rule 12 and 13.4.5.3 rule 4 say LINE SEQUENTIAL takes
     * no RESERVE, BLOCK CONTAINS or RECORD CONTAINS; majesty's jerm writes
     * RECORD CONTAINS on one, as GnuCOBOL allows: taken, BP-E19 */
    if (g_std >= 2002 && f->org == COB_ORG_LINESEQ && (f->reserve_given || f->block_given || f->rc_given))
        bp(BP_E19_LINESEQ_CLAUSES, line);
    if (f->varying && f->org == COB_ORG_LINESEQ)
        die_at(line, "FD %s: variable records need ORGANIZATION SEQUENTIAL (LINE SEQUENTIAL names its own framing; docs/framing.md)", f->name);
}

/* RD report-name [PAGE [LIMIT IS] n [LINE(S)]] [HEADING n] [FIRST DETAIL n]
 * [LAST DETAIL n] [FOOTING n]. then the group descriptions */
/* the report's layout against X3.23-1985 XIII's syntax rules, once every
 * group is read: TYPE (3.20.3 rules 2-4, 7), LINE (3.15.3 rules 3-9), NEXT
 * GROUP (3.16.3 rules 3-5), the page regions (3.8.3 rules 8-9), and COLUMN
 * (3.11.3 rule 2) */
static const char *rg_type_name(int t)
{
    static const char *n[] = { "PAGE HEADING", "DETAIL", "PAGE FOOTING", "REPORT HEADING", "REPORT FOOTING", "CONTROL HEADING", "CONTROL FOOTING" };
    return t >= 0 && t < 7 ? n[t] : "?";
}
static void rw_layout_check(Report *r, int paged)
{
    int body = 0;
    for (int i = 0; i < r->ng; i++) {
        RGroup *g = &r->g[i];
        int isbody = g->type == RG_DETAIL || g->type == RG_CONTROL_HEADING || g->type == RG_CONTROL_FOOTING;
        body |= isbody;
        /* once each: RH, PH, CH FINAL, CF FINAL, PF, RF; one CH and one CF a control */
        for (int j = 0; j < i; j++) {
            RGroup *h = &r->g[j];
            if (h->type != g->type || g->type == RG_DETAIL) continue;
            int same = 1;
            if (g->type == RG_CONTROL_HEADING || g->type == RG_CONTROL_FOOTING)
                same = (g->ctl_tp == 0 && h->ctl_tp == 0) ||
                       (g->ctl_tp && h->ctl_tp && !strcmp(g_tok[g->ctl_tp].s, g_tok[h->ctl_tp].s));
            if (same) die_at(g->line, "RD %s: a second %s%s%s group (X3.23-1985 XIII 3.20.3 rules 2 and 4)", r->name, rg_type_name(g->type),
                             g->type == RG_CONTROL_HEADING || g->type == RG_CONTROL_FOOTING ? " " : "",
                             g->type == RG_CONTROL_HEADING || g->type == RG_CONTROL_FOOTING ? (g->ctl_tp ? g_tok[g->ctl_tp].s : "FINAL") : "");
        }
        if (!paged && (g->type == RG_PAGE_HEADING || g->type == RG_PAGE_FOOTING))
            die_at(g->line, "RD %s: a %s group needs a PAGE clause (X3.23-1985 XIII 3.20.3 rule 3)", r->name, rg_type_name(g->type));
        /* NEXT GROUP */
        if (g->next_kind && (g->type == RG_REPORT_FOOTING || g->type == RG_PAGE_HEADING))
            die_at(g->line, "NEXT GROUP is not for a %s group (X3.23-1985 XIII 3.16.3 rule 5)", rg_type_name(g->type));
        if (g->next_kind == 3 && g->type == RG_PAGE_FOOTING)
            die_at(g->line, "NEXT GROUP NEXT PAGE is not for a PAGE FOOTING group (X3.23-1985 XIII 3.16.3 rule 4)");
        if (!paged && (g->next_kind == 1 || g->next_kind == 3))
            die_at(g->line, "RD %s has no PAGE clause: only NEXT GROUP PLUS (X3.23-1985 XIII 3.16.3 rule 3)", r->name);
        /* LINE */
        int seen_rel = 0, last_abs = 0;
        for (int k = 0; k < g->nl; k++) {
            RLine *ln = &g->l[k];
            if (ln->np && k > 0) die_at(ln->line, "NEXT PAGE is in the first LINE clause of a group, once (X3.23-1985 XIII 3.15.3 rule 6)");
            if (ln->np && !isbody && g->type != RG_REPORT_FOOTING)
                die_at(ln->line, "LINE ... NEXT PAGE is for body groups and the REPORT FOOTING, not a %s group (X3.23-1985 XIII 3.15.3 rule 7)", rg_type_name(g->type));
            if (ln->abs) {
                if (!paged) die_at(ln->line, "RD %s has no PAGE clause: only relative LINE clauses (X3.23-1985 XIII 3.15.3 rule 5)", r->name);
                if (seen_rel) die_at(ln->line, "an absolute LINE after a relative one in the group (X3.23-1985 XIII 3.15.3 rule 3)");
                if (ln->abs <= last_abs) die_at(ln->line, "absolute LINE %d after LINE %d: they ascend (X3.23-1985 XIII 3.15.3 rule 4)", ln->abs, last_abs);
                last_abs = ln->abs;
            } else if (!ln->np) seen_rel = 1;
            if (k == 0 && g->type == RG_PAGE_FOOTING && !ln->abs)
                die_at(ln->line, "a PAGE FOOTING group's first LINE is absolute (X3.23-1985 XIII 3.15.3 rule 9)");
            /* COLUMN: ascending, no overlap, among the items presented */
            int endcol = 0;
            for (int f = 0; f < ln->nf; f++) {
                RField *fd = &ln->f[f];
                if (!fd->column) continue;
                if (fd->column <= endcol)
                    die_at(fd->line, "COLUMN %d: the printable items of a line ascend and do not overlap (the one before ends at %d; X3.23-1985 XIII 3.11.3 rule 2)", fd->column, endcol);
                endcol = fd->column + rfield_cols(fd) - 1;
            }
        }
        /* the page region the group's absolute lines fall in (3.8.3 rule 8) */
        if (paged) {
            int lo, hi, own_page = g->next_kind == 3 || (g->nl && g->l[0].np);
            switch (g->type) {
            case RG_REPORT_HEADING: lo = r->heading; hi = own_page ? r->page_limit : r->first_detail - 1; break;
            case RG_PAGE_HEADING:   lo = r->heading; hi = r->first_detail - 1; break;
            case RG_CONTROL_FOOTING: lo = r->first_detail; hi = r->footing; break;
            case RG_PAGE_FOOTING:   lo = r->footing + 1; hi = r->page_limit; break;
            case RG_REPORT_FOOTING: lo = own_page ? r->heading : r->footing + 1; hi = r->page_limit; break;
            default:                lo = r->first_detail; hi = r->last_detail; break;
            }
            int pos = 0;
            for (int k = 0; k < g->nl; k++) {
                RLine *ln = &g->l[k];
                if (ln->abs) pos = ln->abs; else if (pos) pos += ln->plus; else continue;
                if (pos < lo || pos > hi)
                    die_at(ln->line, "line %d of the %s group is outside its region of the page, lines %d to %d (X3.23-1985 XIII 3.8.3 rule 8)", pos, rg_type_name(g->type), lo, hi);
            }
            int height = 1;
            for (int k = 1; k < g->nl; k++) height += g->l[k].abs ? g->l[k].abs - (g->l[k - 1].abs ? g->l[k - 1].abs : 0) : g->l[k].plus;
            if (height > hi - lo + 1 && hi >= lo)
                die_at(g->line, "the %s group takes %d lines, more than its region of the page holds (%d; X3.23-1985 XIII 3.8.3 rules 8-9)", rg_type_name(g->type), height, hi - lo + 1);
        }
    }
    if (!body) die_at(r->line, "RD %s has no body group (a DETAIL, CONTROL HEADING or CONTROL FOOTING; X3.23-1985 XIII 3.20.3 rule 7)", r->name);
}

static void parse_rd(void)
{
    int line = cur()->line;
    expect_word("rd");
    if (cur()->kind != T_WORD) die_at(line, "expected a report-name after RD");
    if (g_nreport == g_rcap) { g_rcap = g_rcap ? g_rcap * 2 : 4; g_reports = realloc(g_reports, g_rcap * sizeof *g_reports); }
    Report *r = &g_reports[g_nreport++];
    memset(r, 0, sizeof *r);
    r->line = line; r->file = -1;
    snprintf(r->name, sizeof r->name, "%s", cur()->s);
    advance();
    for (int i = g_file_base; i < g_nfile; i++) {
        if (!strcmp(g_files[i].report_name, r->name)) r->file = i;
        for (int k = 0; k < g_files[i].nreport_more; k++) if (!strcmp(g_files[i].report_more[k], r->name)) r->file = i;
    }
    if (r->file < 0) die_at(line, "no FD says REPORT IS %s", r->name);
    /* a print file SELECTed without ORGANIZATION is line sequential: that
     * is what GnuCOBOL made of gl036's, and its .prn is the oracle */
    if (!g_files[r->file].org_given && g_files[r->file].org == COB_ORG_SEQ) g_files[r->file].org = COB_ORG_LINESEQ;
    /* (a print file of another organization takes each line as a record) */
    int pg_h = -1, pg_f = -1, pg_l = -1, pg_ft = -1, pg_given = 0;   /* the PAGE integers written (-1 not) */
    while (cur()->kind != T_PERIOD) {
        Tok *t = cur();
        if (accept_word("page")) {
            accept_word("limit"); accept_word("limits"); accept_word("is"); accept_word("are");
            if (cur()->kind != T_NUM) die_at(t->line, "expected a number after PAGE LIMIT");
            r->page_limit = atoi(cur()->s); pg_given = 1;
            if (r->page_limit > 999) die_at(t->line, "PAGE LIMIT %d: integer-1 has at most three significant digits (X3.23-1985 XIII 3.8.3 rule 2)", r->page_limit);
            advance();
            accept_word("line"); accept_word("lines");
            continue;
        }
        if (accept_word("heading")) { if (cur()->kind != T_NUM) die_at(t->line, "expected a number after HEADING"); r->heading = pg_h = atoi(cur()->s); advance(); continue; }
        if (accept_word("first")) { expect_word("detail"); if (cur()->kind != T_NUM) die_at(t->line, "expected a number"); r->first_detail = pg_f = atoi(cur()->s); advance(); continue; }
        if (accept_word("last")) { expect_word("detail"); if (cur()->kind != T_NUM) die_at(t->line, "expected a number"); r->last_detail = pg_l = atoi(cur()->s); advance(); continue; }
        if (accept_word("footing")) { if (cur()->kind != T_NUM) die_at(t->line, "expected a number after FOOTING"); r->footing = pg_ft = atoi(cur()->s); advance(); continue; }
        if (accept_word("control") || accept_word("controls")) {
            accept_word("is"); accept_word("are");
            if (accept_word("final")) r->ctl_final = 1;
            while (cur()->kind == T_WORD && !at_word("page") && !at_word("heading") && !at_word("first") && !at_word("last") && !at_word("footing") && !at_word("code")) {
                if (r->nctl == 8) die_at(t->line, "more than 8 control levels");
                Sym *c = sym_lookup(cur()->s, NULL, 0, t->line); advance();
                cen_flag(c, CEN_PTR);
                for (int q = 0; q < r->nctl; q++)
                    if (r->ctl_sym[q] == sym_idx(c)) die_at(t->line, "CONTROL names '%s' twice; each data-name a different item (X3.23-1985 XIII 3.7.3 rule 2)", c->name);
                r->ctl_sym[r->nctl] = sym_idx(c);
                /* the prior values a CONTROL FOOTING prints: a hidden clone
                 * of the item, sensed against and refreshed at each break */
                Sym *cl = sym_new();
                snprintf(cl->name, sizeof cl->name, "*prior-%.50s-%d", c->name, r->nctl);
                cl->line = t->line; cl->level = 77;
                cl->has_pic = c->has_pic; memcpy(cl->pic, c->pic, sizeof cl->pic); cl->pi = c->pi;
                cl->usage = c->usage; cl->uvar = c->uvar; cl->has_usage = c->has_usage;
                r->ctl_clone[r->nctl] = sym_idx(cl);
                Sym *hd = sym_new();
                snprintf(hd->name, sizeof hd->name, "*held-%.51s-%d", c->name, r->nctl);
                hd->line = t->line; hd->level = 77;
                hd->has_pic = c->has_pic; memcpy(hd->pic, c->pic, sizeof hd->pic); hd->pi = c->pi;
                hd->usage = c->usage; hd->uvar = c->uvar; hd->has_usage = c->has_usage;
                r->ctl_held[r->nctl] = sym_idx(hd);
                r = &g_reports[g_nreport - 1];                       /* sym_new may move nothing, but be safe */
                r->nctl++;
            }
            if (!r->nctl && !r->ctl_final) die_at(t->line, "CONTROL needs FINAL or data-names");
            continue;
        }
        if (accept_word("code")) {
            /* CODE (X3.23-1985 XIII 3.6; 2023 13.18.12): the characters
             * each record of this report begins with, outside the lines'
             * columns.  85: a two-character literal; 2023 also an
             * identifier, evaluated at the start of each body group. */
            accept_word("is");
            if (cur()->kind == T_STR) {
                if (g_std < 2002 && cur()->len != 2) die_at(t->line, "CODE takes a two-character literal (X3.23-1985 XIII 3.6.3 rule 1)");
                r->code_lit = cur(); advance();
            } else if (g_std >= 2002 && cur()->kind == T_WORD) {
                r->code_tp = g_tp; advance();
                while ((at_word("of") || at_word("in")) && peek(1)->kind == T_WORD) { advance(); advance(); }
            } else die_at(t->line, "CODE takes %s", g_std >= 2002 ? "an alphanumeric literal or identifier" : "a two-character literal");
            continue;
        }
        die_at(t->line, "unexpected %s in RD %s", tok_desc(t), r->name);
    }
    expect_period();
    /* the PAGE integers in order (X3.23-1985 XIII 3.8.3 rules 3-7): 1 <= HEADING
     * <= FIRST DETAIL <= LAST DETAIL <= FOOTING <= PAGE LIMIT, among those written */
    {
        int v[5] = { 1, pg_h, pg_f, pg_l, pg_ft }; const char *nm[5] = { "1", "HEADING", "FIRST DETAIL", "LAST DETAIL", "FOOTING" };
        if (pg_h == 0) die_at(line, "RD %s: HEADING is at least 1 (X3.23-1985 XIII 3.8.3 rule 3)", r->name);
        for (int a = 1; a < 5; a++) {
            if (v[a] < 0) continue;
            for (int b = a - 1; b >= 1; b--) if (v[b] >= 0) {
                if (v[a] < v[b]) die_at(line, "RD %s: %s %d is less than %s %d (X3.23-1985 XIII 3.8.3 rules 4-6)", r->name, nm[a], v[a], nm[b], v[b]);
                break;
            }
            if (pg_given && v[a] > r->page_limit) die_at(line, "RD %s: %s %d is past PAGE LIMIT %d (X3.23-1985 XIII 3.8.3 rule 7)", r->name, nm[a], v[a], r->page_limit);
        }
    }
    /* no PAGE clause: no page control -- one endless page (the runtime
     * pads nothing and never ends it) */
    if (!r->page_limit) { r->heading = 1; r->first_detail = 1; r->last_detail = 1 << 30; r->footing = 1 << 30; }
    if (!r->heading) r->heading = 1;
    if (!r->first_detail) r->first_detail = r->heading;
    if (!r->last_detail) r->last_detail = r->footing ? r->footing : r->page_limit;
    if (!r->footing) r->footing = r->page_limit;
    /* LINE-COUNTER and PAGE-COUNTER: four-byte unsigned cells of the report block */
    for (int which = 0; which < 2; which++) {
        Sym *c = sym_new();
        snprintf(c->name, sizeof c->name, which ? "page-counter" : "line-counter");
        c->line = line; c->level = 77; c->usage = U_COMP5; c->has_usage = 1;
        c->has_pic = 1; snprintf(c->pic, sizeof c->pic, "9(9)"); pic_analyse(c->pic, &c->pi);
        c->size = 4; c->offset = which ? 24 : 20; c->rep_ctr = (int)(r - g_reports);
        r = &g_reports[c->rep_ctr];
        if (which) r->pc_sym = sym_idx(c); else r->lc_sym = sym_idx(c);
    }

    /* groups: 01 [name] with TYPE, and every entry's clauses in any order
     * (X3.23 VIII-7): LINE begins a line of the group (on the 01 too),
     * COLUMN / PICTURE / SOURCE / VALUE make the entry a printable field
     * of the current line (an entry may carry both -- the elementary
     * report group RW101A and RW301M write) */
    while (cur()->kind == T_NUM && !strcmp(cur()->s, "01")) {
        advance();
        if (r->ng == r->gcap) { r->gcap = r->gcap ? r->gcap * 2 : 8; r->g = realloc(r->g, r->gcap * sizeof *r->g); }
        RGroup *g = &r->g[r->ng++];
        memset(g, 0, sizeof *g);
        g->use_sec = -1; g->ctl_level = -1;
        g->line = cur()->line;
        int has_type = 0, first = 1;
        int lstk[50], nlstk = 0;            /* the levels of the open entries that hold a LINE */
        /* every word that starts one of the entry's clauses: anything else
         * first is the entry's name ("05 COL 1" is a COLUMN clause, not an
         * entry named COL -- ACAS, cobol ISSUES-124) */
        static const char *clause_words[] = { "type", "line", "next", "column", "columns", "col", "cols", "pic", "picture", "source", "value",
            "just", "justified", "blank", "sum", "group", "usage", "display", "present", "sign", NULL };
        for (;;) {
            int eline = cur()->line, lvl = 1;
            if (!first) {
                if (cur()->kind != T_NUM || !strcmp(cur()->s, "01")) break;
                lvl = parse_level(); advance();
                if (lvl < 2 || lvl > 49) die_at(eline, "bad level %d in report group '%s'", lvl, g->name);
            }
            char entry_name[64] = "";
            if (cur()->kind == T_WORD) {
                int is_clause = 0;
                for (int k = 0; clause_words[k]; k++) if (at_word(clause_words[k])) is_clause = 1;
                if (!is_clause) {
                    if (first) snprintf(g->name, sizeof g->name, "%s", cur()->s);
                    else snprintf(entry_name, sizeof entry_name, "%s", cur()->s);
                    advance();
                }   /* a name */
            }
            while (nlstk && lstk[nlstk - 1] >= lvl) nlstk--;       /* entries this one is not under */
            /* the entry's clauses */
            int has_line = 0, labs = 0, lplus = 0, is_field = 0, lnp = 0;
            RField fd; memset(&fd, 0, sizeof fd); fd.line = eline;
            int usage_disp = 0;         /* DISPLAY written: a PICTURE of N refuses it (13.18.60.3 rule 20) */
            snprintf(fd.ename, sizeof fd.ename, "%s", entry_name);
            while (cur()->kind != T_PERIOD) {
                Tok *t = cur();
                if (accept_word("type")) {
                    if (!first) die_at(t->line, "TYPE belongs on the 01 of report group '%s'", g->name);
                    accept_word("is");
                    if (accept_word("page")) { if (accept_word("heading")) g->type = RG_PAGE_HEADING; else if (accept_word("footing")) g->type = RG_PAGE_FOOTING; else die_at(t->line, "TYPE PAGE: HEADING or FOOTING"); }
                    else if (accept_word("ph")) g->type = RG_PAGE_HEADING;
                    else if (accept_word("pf")) g->type = RG_PAGE_FOOTING;
                    else if (accept_word("detail") || accept_word("de")) g->type = RG_DETAIL;
                    else if (accept_word("report")) { if (accept_word("heading")) g->type = RG_REPORT_HEADING; else if (accept_word("footing")) g->type = RG_REPORT_FOOTING; else die_at(t->line, "TYPE REPORT: HEADING or FOOTING"); }
                    else if (accept_word("rh")) g->type = RG_REPORT_HEADING;
                    else if (accept_word("rf")) g->type = RG_REPORT_FOOTING;
                    else if (at_word("control") || at_word("ch") || at_word("cf")) {
                        int foot = at_word("cf");
                        if (accept_word("control")) { if (accept_word("footing")) foot = 1; else if (!accept_word("heading")) die_at(t->line, "TYPE CONTROL: HEADING or FOOTING"); }
                        else advance();
                        g->type = foot ? RG_CONTROL_FOOTING : RG_CONTROL_HEADING;
                        if (accept_word("final")) g->ctl_tp = 0;
                        else if (cur()->kind == T_WORD) { g->ctl_tp = g_tp; advance(); while (at_word("of") || at_word("in")) { advance(); if (cur()->kind == T_WORD) advance(); } }
                        else die_at(t->line, "TYPE CONTROL %s needs a control data-name or FINAL", foot ? "FOOTING" : "HEADING");
                    }
                    else die_at(t->line, "unknown report group TYPE %s", cur()->s);
                    has_type = 1;
                    continue;
                }
                if (accept_word("line")) {
                    accept_word("number"); accept_word("is");
                    if (accept_word("plus")) { if (cur()->kind != T_NUM) die_at(t->line, "expected a number after LINE PLUS"); lplus = atoi(cur()->s); advance(); }
                    else if (at_op("+")) { advance(); if (cur()->kind != T_NUM) die_at(t->line, "expected a number after LINE +"); lplus = atoi(cur()->s); advance(); }
                    else if (cur()->kind == T_NUM) {
                        if (cur()->s[0] == '+') lplus = atoi(cur()->s + 1);      /* "+1" read as a signed literal */
                        else if (cur()->s[0] == '-') die_at(t->line, "LINE cannot be negative");
                        else labs = atoi(cur()->s);
                        advance();
                    } else if (accept_word("next")) { expect_word("page"); lnp = 1; }
                    else die_at(t->line, "expected a line number after LINE");
                    if (accept_word("on")) { expect_word("next"); expect_word("page"); lnp = 1; }
                    else if (labs && at_word("next") && is_word(peek(1), "page")) { advance(); advance(); lnp = 1; }
                    if (!lnp && !labs && !lplus) die_at(t->line, "LINE needs a number");
                    if (labs > 999 || lplus > 999) die_at(t->line, "LINE: at most three significant digits (X3.23-1985 XIII 3.15.3 rule 1)");
                    if (r->page_limit && labs > r->page_limit) die_at(t->line, "LINE %d is past PAGE LIMIT %d", labs, r->page_limit);
                    if (nlstk) die_at(t->line, "a LINE clause in an entry under another with LINE (X3.23-1985 XIII 3.9.3 rule 9, 3.15.3 rule 2)");
                    has_line = 1;
                    continue;
                }
                if (accept_word("next")) {
                    expect_word("group"); accept_word("is");
                    if (!first) die_at(t->line, "NEXT GROUP belongs on the 01 of report group '%s'", g->name);
                    if (accept_word("plus")) { if (cur()->kind != T_NUM) die_at(t->line, "expected a number after NEXT GROUP PLUS"); g->next_kind = 2; g->next_n = atoi(cur()->s); advance(); }
                    else if (accept_word("next")) { expect_word("page"); g->next_kind = 3; }
                    else if (cur()->kind == T_NUM) {
                        if (cur()->s[0] == '+') { g->next_kind = 2; g->next_n = atoi(cur()->s + 1); }
                        else { g->next_kind = 1; g->next_n = atoi(cur()->s); }
                        advance();
                    }
                    else die_at(t->line, "NEXT GROUP takes an integer, PLUS integer, or NEXT PAGE");
                    if (g->next_n > 999) die_at(t->line, "NEXT GROUP: at most three significant digits (X3.23-1985 XIII 3.16.3 rule 2)");
                    continue;
                }
                if (accept_word("column") || accept_word("col") || accept_word("columns") || accept_word("cols")) {
                    accept_word("number"); accept_word("numbers"); accept_word("is"); accept_word("are");
                    if (at_word("plus") || at_op("+") || at_word("left") || at_word("right") || at_word("center") || at_word("centered") ||
                        (cur()->kind == T_NUM && peek(1)->kind == T_NUM))
                        die_at(t->line, "COLUMN PLUS, LEFT, RIGHT, CENTER and several column numbers are COBOL 2002's Report Writer; not implemented (the 1985 module is)");
                    if (cur()->kind != T_NUM) die_at(t->line, "expected a number after COLUMN");
                    fd.column = atoi(cur()->s); advance(); is_field = 1;
                    continue;
                }
                if (accept_word("pic") || accept_word("picture")) {
                    accept_word("is");
                    if (cur()->kind != T_PIC) die_at(t->line, "expected a PICTURE character-string");
            if (g_ncpicbad) const_pic_check();
                    fd.has_pic = 1;
                    snprintf(fd.pic, sizeof fd.pic, "%s", cur()->s); pic_len_check(fd.pic, t->line);
                    if (nat_picture(fd.pic, &fd.pi, t->line)) { advance(); is_field = 1; continue; }
                    if (pic_analyse(fd.pic, &fd.pi) < 0) die_at(t->line, "report field: %s", fd.pi.err);
                    advance(); is_field = 1;
                    continue;
                }
                if (accept_word("source")) {
                    accept_word("is");
                    if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after SOURCE");
                    /* keep the reference's position: parse_ref reads it at GENERATE, when every item is declared */
                    fd.has_source = 1; fd.source_tp = g_tp; advance();
                    while (at_word("of") || at_word("in")) { advance(); if (cur()->kind == T_WORD) advance(); }
                    while (cur()->kind == T_LP) {
                        int depth = 0;
                        do {
                            if (cur()->kind == T_LP) depth++;
                            else if (cur()->kind == T_RP) depth--;
                            else if (cur()->kind == T_PERIOD || cur()->kind == T_EOF) die_at(t->line, "unbalanced parentheses in SOURCE");
                            advance();
                        } while (depth > 0);
                    }
                    is_field = 1;
                    continue;
                }
                if (accept_word("value")) {
                    accept_word("is");
                    if (cur()->kind != T_STR && cur()->kind != T_NUM) die_at(t->line, "VALUE in a report field needs a literal");
                    fd.value = cur(); advance(); is_field = 1;
                    continue;
                }
                if (accept_word("just") || accept_word("justified")) { accept_word("right"); fd.just = 1; is_field = 1; continue; }
                if (accept_word("blank")) { accept_word("when"); accept_word("zero"); accept_word("zeros"); fd.blank_zero = 1; is_field = 1; continue; }
                if (accept_word("usage")) {
                    accept_word("is");
                    if (accept_word("national")) { if (g_std < 2002) die_at(t->line, "USAGE NATIONAL is COBOL 2002; compile with -std=2002"); fd.usage_nat = 1; }
                    else if (accept_word("display")) usage_disp = 1;
                    else die_at(cur()->line, "a report group item takes only USAGE DISPLAY or NATIONAL, not %s (2023 13.18.60.3 rule 7)", tok_desc(cur()));
                    continue;
                }
                if (accept_word("display")) { usage_disp = 1; continue; }
                if (g_std >= 2002 && accept_word("national")) { fd.usage_nat = 1; continue; }
                if (accept_word("sum")) {
                    fd.has_sum = 1; is_field = 1;
                    for (;;) {
                        accept_word("of");
                        if (cur()->kind != T_WORD) die_at(t->line, "SUM needs data-names");
                        if (fd.nsum == 8) die_at(t->line, "more than 8 SUM operands");
                        fd.sum_tp[fd.nsum++] = g_tp; advance();
                        while (at_word("of") || at_word("in")) { advance(); if (cur()->kind == T_WORD) advance(); }
                        if (at_word("upon") || at_word("reset") || cur()->kind == T_PERIOD || at_word("sum")) {
                            if (accept_word("sum")) continue;
                            break;
                        }
                        {   /* the next clause of the entry: the operands end (clauses come in any order) */
                            int cw = 0;
                            for (int k = 0; clause_words[k]; k++) if (at_word(clause_words[k])) cw = 1;
                            if (cw || at_word("sign") || at_word("indicate") || cur()->kind != T_WORD) break;
                        }
                    }
                    if (accept_word("upon")) {
                        while (cur()->kind == T_WORD && !at_word("reset")) {
                            if (fd.nupon == 4) die_at(t->line, "more than 4 UPON details");
                            fd.upon_tp[fd.nupon++] = g_tp; advance();
                        }
                        if (!fd.nupon) die_at(t->line, "UPON needs a DETAIL group name");
                    }
                    if (accept_word("reset")) {
                        accept_word("on");
                        if (accept_word("final")) fd.reset_final = 1;
                        else if (cur()->kind == T_WORD) { fd.reset_tp = g_tp; advance(); }
                        else die_at(t->line, "RESET ON needs a control data-name or FINAL");
                    }
                    continue;
                }
                if (accept_word("group")) { accept_word("indicate"); fd.gi = 1; is_field = 1; continue; }
                if (accept_word("sign")) {
                    /* SIGN [IS] {LEADING | TRAILING} SEPARATE [CHARACTER]: in a
                     * report group SEPARATE is required (X3.23-1985 XIII 3.17.3 rule 3) */
                    accept_word("is");
                    if (accept_word("leading")) fd.sign_lead = 1; else if (!accept_word("trailing")) die_at(t->line, "SIGN: LEADING or TRAILING");
                    if (!accept_word("separate")) die_at(t->line, "SIGN in a report group is SEPARATE (X3.23-1985 XIII 3.17.3 rule 3)");
                    accept_word("character");
                    fd.sign_sep = 1;
                    continue;
                }
                if (at_word("present") || at_word("varying") || at_word("occurs"))
                    die_at(t->line, "%s in a report group is COBOL 2002's Report Writer; not implemented (the 1985 module is)",
                           at_word("present") ? "PRESENT WHEN" : at_word("varying") ? "VARYING" : "OCCURS");
                die_at(t->line, "unexpected %s in report group '%s'", tok_desc(t), g->name);
            }
            expect_period();
            if (first && !has_type) die_at(g->line, "report group '%s' needs a TYPE", g->name);
            if (has_line && nlstk < 50) lstk[nlstk++] = lvl;
            if (fd.gi && g->type != RG_DETAIL)
                die_at(eline, "GROUP INDICATE belongs in a DETAIL report group (X3.23-1985 XIII 3.9.3 rule 10a, 3.13.3 rule 1)");
            if (fd.value && !fd.column)
                die_at(eline, "an entry with VALUE also has COLUMN (X3.23-1985 XIII 3.9.3 rule 10e)");
            if (fd.sign_sep && !(fd.has_pic && fd.pi.category == PIC_NUMERIC && fd.pi.is_signed))
                die_at(eline, "SIGN: a numeric entry whose PICTURE has S (X3.23-1985 XIII 3.17.3 rule 1)");
            if (fd.has_sum && fd.has_pic && fd.pi.category == PIC_ALPHABETIC)
                die_at(eline, "a SUM entry is not alphabetic (X3.23-1985 XIII 3.19.3 rule 1)");
            if (has_line) {
                if (g->nl == g->lcap) { g->lcap = g->lcap ? g->lcap * 2 : 4; g->l = realloc(g->l, g->lcap * sizeof *g->l); }
                RLine *ln = &g->l[g->nl++];
                memset(ln, 0, sizeof *ln);
                ln->line = eline; ln->abs = labs; ln->plus = lplus; ln->np = lnp;
            }
            if (is_field) {
                if (!g->nl) die_at(eline, "a printable entry of report group '%s' before any LINE", g->name);
                RLine *ln = &g->l[g->nl - 1];
                if (fd.has_sum) {
                    /* the sum counter: a signed item sized by the entry's
                     * PICTURE, named by the entry's data-name when it has
                     * one (X3.23 VIII 2.20), zeroed by INITIATE */
                    if (g->type != RG_CONTROL_FOOTING) die_at(eline, "SUM belongs in a CONTROL FOOTING group");
                    if (!fd.has_pic) die_at(eline, "a SUM entry needs a PICTURE");
                    Sym *ctr = sym_new();
                    if (fd.ename[0]) snprintf(ctr->name, sizeof ctr->name, "%s", fd.ename);
                    else snprintf(ctr->name, sizeof ctr->name, "*sum-%d-%d", (int)(r - g_reports), r->ng * 100 + g->nl);
                    ctr->line = eline; ctr->level = 77;
                    ctr->has_pic = 1;
                    int idig = fd.pi.digits - fd.pi.scale;
                    if (fd.pi.scale > 0 && idig > 0) snprintf(ctr->pic, sizeof ctr->pic, "s9(%d)v9(%d)", idig, fd.pi.scale);
                    else if (fd.pi.scale > 0) snprintf(ctr->pic, sizeof ctr->pic, "sv9(%d)", fd.pi.scale);
                    else snprintf(ctr->pic, sizeof ctr->pic, "s9(%d)", fd.pi.digits > 0 ? fd.pi.digits : 1);
                    pic_analyse(ctr->pic, &ctr->pi);
                    fd.ctr_sym = sym_idx(ctr);
                    r = &g_reports[g_nreport - 1]; g = &r->g[r->ng - 1]; ln = &g->l[g->nl - 1];
                }
                if (!fd.has_pic && fd.value && fd.value->kind == T_STR) {
                    /* VALUE without PICTURE: an alphanumeric of the literal's width */
                    fd.has_pic = 1;
                    if (fd.value->nat) {
                        /* a national literal: national, as many positions as it
                         * has characters or takes columns, so all of it shows */
                        int nu = fd.value->len / 2, nc = nat_lit_cols((const unsigned char *)fd.value->s, fd.value->len);
                        snprintf(fd.pic, sizeof fd.pic, "n(%d)", nc > nu ? nc : nu > 0 ? nu : 1);
                        nat_picture(fd.pic, &fd.pi, eline);
                    } else {
                        snprintf(fd.pic, sizeof fd.pic, "x(%d)", fd.value->len > 0 ? fd.value->len : 1);
                        if (pic_analyse(fd.pic, &fd.pi) < 0) die_at(eline, "report field: %s", fd.pi.err);
                    }
                }
                if (fd.blank_zero) bwz_check("the report field", &fd.pi, 0, eline);
                if (usage_disp && fd.pi.category == PIC_NATIONAL)
                    die_at(eline, "a report group item with a PICTURE of N takes only USAGE NATIONAL (2023 13.18.60.3 rule 20)");
                if (fd.usage_nat) {
                    if (fd.pi.category == PIC_NATIONAL) fd.usage_nat = 0;     /* PICTURE N is national usage already */
                    else if (fd.pi.category != PIC_NUMERIC && fd.pi.category != PIC_NUMERIC_EDITED)
                        die_at(eline, "USAGE NATIONAL takes a PICTURE N, or a numeric or numeric-edited one (2023 13.18.60.3 rule 12)");
                }
                if (fd.value && fd.value->kind == T_STR && fd.value->nat && !rfield_is_nat(&fd))
                    die_at(eline, "a national VALUE goes to a national field (PICTURE N)");
                if (!fd.has_pic) die_at(eline, "a report field needs a PICTURE");
                if (fd.has_source + !!fd.value + fd.has_sum != 1) die_at(eline, "a report field needs exactly one of SOURCE, VALUE and SUM");
                if (fd.value && fd.value->kind == T_STR && !fd.value->nat && fd.pi.category != PIC_NATIONAL && fd.value->len > fd.pi.bytes)
                    die_at(eline, "VALUE: the literal has %d characters, the PICTURE %d (X3.23-1985 XIII 3.22.3 rule 2)", fd.value->len, fd.pi.bytes);
                /* no COLUMN: the item is not presented (3.11.4 rule 1) -- a SUM
                 * counter so defined still counts; column 0 marks it */
                if (ln->nf == ln->fcap) { ln->fcap = ln->fcap ? ln->fcap * 2 : 8; ln->f = realloc(ln->f, ln->fcap * sizeof *ln->f); }
                ln->f[ln->nf++] = fd;
            }
            first = 0;
        }
        if (!g->nl) die_at(g->line, "report group '%s' has no LINE", g->name);
    }
    rw_layout_check(r, pg_given);
}

/* ERASE {EOL | EOS | END OF LINE | END OF SCREEN} (2023 13.18.21): the
 * erase flag for the entry, the word ERASE already read */
static int parse_erase_clause(int line)
{
    if (accept_word("eol")) return COB_SX_ERASE_EOL;
    if (accept_word("eos")) return COB_SX_ERASE_EOS;
    if (accept_word("end")) {
        accept_word("of");
        if (accept_word("line")) return COB_SX_ERASE_EOL;
        if (accept_word("screen")) return COB_SX_ERASE_EOS;
    }
    die_at(line, "ERASE takes EOL, EOS, END OF LINE or END OF SCREEN (2023 13.18.21.2)");
    return 0;
}

/* 01 screen-name. then slot entries at deeper levels, each with LINE /
 * COLUMN / VALUE / PIC FROM|TO|USING / attributes */
static void parse_screen_section(void)
{
    while (cur()->kind == T_NUM && !strcmp(cur()->s, "01")) {
        int line = cur()->line; advance();
        if (cur()->kind != T_WORD) die_at(line, "expected a screen-name after 01");
        if (g_nscreen == g_scrcap) { g_scrcap = g_scrcap ? g_scrcap * 2 : 4; g_screens = realloc(g_screens, g_scrcap * sizeof *g_screens); }
        Screen *sc = &g_screens[g_nscreen++];
        memset(sc, 0, sizeof *sc);
        sc->line = line;
        snprintf(sc->name, sizeof sc->name, "%s", cur()->s); advance();
        if (sym_lookup_quiet(sc->name)) die_at(line, "'%s' is both a data item and a screen", sc->name);
        sc->fg = sc->bg = 255;
        /* an ERASE on a group clears from the group's position, which is
         * its first field's: it waits for that field (13.18.21.4 rule 1) */
        int pend_erase = 0;
        while (cur()->kind != T_PERIOD) {
            if (accept_word("blank")) { expect_word("screen"); sc->blank_screen = 1; continue; }
            if (accept_word("erase")) { pend_erase |= parse_erase_clause(cur()->line); continue; }
            if (at_word("foreground-color") || at_word("foreground-colour") || at_word("background-color") || at_word("background-colour")) {
                /* the 01 is a group like any other (2023 13.18.4.4 rule 3, 13.18.23.4 rule 3):
                 * its colours are inherited by every entry below it */
                Tok *t = cur(); advance();
                int bg = t->s[0] == 'b' || t->s[0] == 'B';
                accept_word("is");
                if (cur()->kind != T_NUM) die_at(t->line, "expected a colour number 0-7 after %s", t->s);
                int c = atoi(cur()->s); advance();
                if (c < 0 || c > 7) die_at(t->line, "a screen colour is 0-7 (black, blue, green, cyan, red, magenta, yellow, white)");
                if (bg) sc->bg = c; else sc->fg = c;
                continue;
            }
            die_at(cur()->line, "unexpected %s on screen '%s' (v1 takes BLANK SCREEN on the 01, fields below it)", tok_desc(cur()), sc->name);
        }
        expect_period();
        /* nested groups: a stack of the enclosing entries.  Each carries
         * the composed look (flags, colours) its children inherit, and
         * the group's LINE/COLUMN, which anchor its first child. */
        struct { int level, flags, fg, bg, line, col, subidx, usage; } gstk[16];
        int gdepth = 0;
        while (cur()->kind == T_NUM && strcmp(cur()->s, "01")) {
            int fl = parse_level(); int fline = cur()->line; advance();
            if (fl <= 1 || fl > 49) die_at(fline, "bad level %d in a screen", fl);
            while (gdepth && gstk[gdepth - 1].level >= fl) {
                int si = gstk[--gdepth].subidx;
                if (si >= 0) sc->sub[si].count = sc->nf - sc->sub[si].first;
            }
            char ename[64] = ""; int ename_tp = -1;   /* the name's token: an implicit USING refers to it (BP-G4) */
            int susage = 0;             /* USAGE: 1 DISPLAY, 2 NATIONAL; a group's reaches its children */
            if (cur()->kind == T_WORD && !at_word("blank") && !at_word("usage") && !at_word("line") && !at_word("column") && !at_word("col") &&
                !at_word("value") && !at_word("pic") && !at_word("picture") && !at_word("highlight") && !at_word("underline") &&
                !at_word("auto") && !at_word("auto-skip") && !at_word("reverse-video") && !at_word("from") && !at_word("to") && !at_word("using") &&
                !at_word("secure") && !at_word("required") && !at_word("full") && !at_word("lowlight") && !at_word("blink") && !at_word("bell") &&
                !at_word("beep") && !at_word("erase") && !at_word("foreground-color") && !at_word("background-color") &&
                !at_word("occurs")) {
                snprintf(ename, sizeof ename, "%s", cur()->s);
                ename_tp = g_tp;
                advance();                                       /* a name on the entry */
            }
            if (sc->nf == sc->fcap) { sc->fcap = sc->fcap ? sc->fcap * 2 : 16; sc->f = realloc(sc->f, sc->fcap * sizeof *sc->f); }
            SField *f = &sc->f[sc->nf];
            SField *prev = sc->nf ? &sc->f[sc->nf - 1] : NULL;
            memset(f, 0, sizeof *f);
            f->srcline = fline; f->kind = -1; f->fg = 255; f->bg = 255;
            int blank_screen_entry = 0;
            /* OCCURS n: n occurrences, each placed as though it had the same
             * LINE and COLUMN clauses (2023 13.18.38.4 rule 6) -- so a LINE
             * PLUS or COLUMN PLUS steps from the occurrence before */
            int occ = 0, line_plus = -1, col_plus = -1;
            while (cur()->kind != T_PERIOD) {
                Tok *t = cur();
                if (accept_word("occurs")) {
                    if (cur()->kind != T_NUM) die_at(t->line, "OCCURS in the SCREEN SECTION takes an integer (2023 13.18.38.3 rule 11)");
                    occ = atoi(cur()->s); advance(); accept_word("times");
                    if (occ < 1) die_at(t->line, "OCCURS needs at least one occurrence");
                    continue;
                }
                if (accept_word("blank")) {
                    if (accept_word("screen")) { blank_screen_entry = 1; sc->blank_screen = 1; continue; }
                    if (accept_word("line")) die_at(t->line, "BLANK LINE is not implemented");
                    accept_word("when"); if (!(accept_word("zero") || accept_word("zeros") || accept_word("zeroes"))) die_at(t->line, "expected ZERO after BLANK WHEN");
                    f->blank_zero = 1; continue;
                }
                if (accept_word("line")) {
                    accept_word("number"); accept_word("is");
                    if (accept_word("plus") || accept_word("+")) {       /* relative to the previous slot's line */
                        int n = 1;
                        if (cur()->kind == T_NUM) { n = atoi(cur()->s); advance(); }
                        f->line = (prev ? prev->line : 0) + n; line_plus = n; continue;
                    }
                    if (cur()->kind != T_NUM) die_at(t->line, "expected a number after LINE");
                    f->line = atoi(cur()->s); advance(); continue;
                }
                if (accept_word("column") || accept_word("col")) {
                    accept_word("number"); accept_word("is");
                    if (accept_word("plus") || accept_word("+")) {       /* from the position after the previous slot, as GnuCOBOL counts */
                        int n = 1;
                        if (cur()->kind == T_NUM) { n = atoi(cur()->s); advance(); }
                        f->col = (prev && (!f->line || f->line == prev->line) ? prev->col + prev->width : 0) + n; col_plus = n; continue;
                    }
                    if (cur()->kind != T_NUM) die_at(t->line, "expected a number after COLUMN");
                    f->col = atoi(cur()->s); advance(); continue;
                }
                if (accept_word("value")) {
                    accept_word("is");
                    if (cur()->kind != T_STR) die_at(t->line, "a screen VALUE needs a nonnumeric literal");
                    f->value = cur(); f->natlit = cur()->nat; advance(); f->kind = COB_SCR_VALUE; continue;
                }
                if (accept_word("pic") || accept_word("picture")) {
                    if (cur()->kind != T_PIC) die_at(t->line, "expected a PICTURE character-string");
            if (g_ncpicbad) const_pic_check();
                    f->has_pic = 1;
                    snprintf(f->pic, sizeof f->pic, "%s", cur()->s);
                    pic_len_check(f->pic, t->line);
                    if (nat_picture(f->pic, &f->pi, t->line)) { advance(); continue; }
                    if (pic_analyse(f->pic, &f->pi) < 0) die_at(t->line, "screen field: %s", f->pi.err);
                    advance(); continue;
                }
                if (at_word("from") || at_word("to") || at_word("using")) {
                    int kind = at_word("from") ? COB_SCR_FROM : at_word("to") ? COB_SCR_TO : COB_SCR_USING;
                    advance();
                    if (kind == COB_SCR_FROM && (cur()->kind == T_STR || cur()->kind == T_NUM)) {
                        /* FROM literal-1 (2002 13.15.1): the literal through
                         * the entry's PICTURE -- a VALUE with a PICTURE, as
                         * the slot is finished below */
                        if (cur()->kind == T_NUM) die_at(t->line, "a screen FROM with a numeric literal is not implemented (2002 13.15.1)");
                        f->kind = COB_SCR_VALUE; f->value = cur(); f->natlit = cur()->nat; f->from_lit = 1;
                        advance(); continue;
                    }
                    /* the reference's tokens are recorded and skipped, as
                     * Report Writer records SOURCE: the table dimensions do
                     * not exist yet, so it is parsed at first use
                     * (sfield_resolve) and kept; its address is computed
                     * at every ACCEPT/DISPLAY when it is not static */
                    f->ref_tp = g_tp;
                    if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name");
                    advance();
                    while ((at_word("of") || at_word("in")) && peek(1)->kind == T_WORD) { advance(); advance(); }
                    if (cur()->kind == T_LP) {
                        int d = 0;
                        do { if (cur()->kind == T_LP) d++; else if (cur()->kind == T_RP) d--; advance(); }
                        while (d && cur()->kind != T_PERIOD);
                    }
                    if (cur()->kind == T_LP) {       /* (start:length): resolved with the reference */
                        int d = 0;
                        do { if (cur()->kind == T_LP) d++; else if (cur()->kind == T_RP) d--; advance(); }
                        while (d && cur()->kind != T_PERIOD);
                    }
                    f->kind = kind; continue;
                }
                if (accept_word("highlight")) { f->flags |= COB_SF_HIGHLIGHT; continue; }
                if (accept_word("underline")) { f->flags |= COB_SF_UNDERLINE; continue; }
                if (accept_word("auto") || accept_word("auto-skip")) { f->flags |= COB_SF_AUTO; continue; }
                if (accept_word("reverse-video")) { f->flags |= COB_SF_REVERSE; continue; }
                if (accept_word("bell") || accept_word("beep") || accept_word("blink")) continue;   /* no bell, no blink: painted plain */
                if (accept_word("erase")) { f->ext |= parse_erase_clause(t->line); continue; }
                if (accept_word("foreground-color") || accept_word("foreground-colour") || accept_word("background-color") || accept_word("background-colour")) {
                    int bg = t->s[0] == 'b';
                    accept_word("is");
                    if (cur()->kind != T_NUM) die_at(t->line, "expected a colour number 0-7 after %s", t->s);
                    int c = atoi(cur()->s); advance();
                    if (c < 0 || c > 7) die_at(t->line, "a screen colour is 0-7 (black, blue, green, cyan, red, magenta, yellow, white)");
                    if (bg) f->bg = c; else f->fg = c;
                    continue;
                }
                if (accept_word("secure")) { f->flags |= COB_SF_SECURE; continue; }
                if (accept_word("required")) { f->flags |= COB_SF_REQUIRED; continue; }
                if (accept_word("full")) { f->flags |= COB_SF_FULL; continue; }
                if (accept_word("lowlight")) { f->flags |= COB_SF_LOWLIGHT; continue; }
                if (accept_word("usage")) {
                    accept_word("is");
                    if (accept_word("national")) susage = 2;
                    else if (accept_word("display")) susage = 1;
                    else die_at(cur()->line, "a screen item takes only USAGE DISPLAY or NATIONAL, not %s (2023 13.18.60.3 rule 17)", tok_desc(cur()));
                    continue;
                }
                die_at(t->line, "unexpected %s in screen '%s'", tok_desc(t), sc->name);
            }
            expect_period();
            if (!susage && gdepth) susage = gstk[gdepth - 1].usage;
            if (f->has_pic && f->blank_zero) bwz_check("the screen field", &f->pi, 0, fline);
            if (f->has_pic && susage == 1 && f->pi.category == PIC_NATIONAL)
                die_at(fline, "a screen item with a PICTURE of N takes only USAGE NATIONAL (2023 13.18.60.3 rule 20)");
            if (f->has_pic && susage == 2 && f->pi.category != PIC_NATIONAL)
                die_at(fline, "USAGE NATIONAL on a screen item whose PICTURE is not N is not implemented");
            if (f->kind < 0 && !f->has_pic && (f->ext & (COB_SX_ERASE_EOL | COB_SX_ERASE_EOS))) {
                pend_erase |= f->ext & (COB_SX_ERASE_EOL | COB_SX_ERASE_EOS);   /* a group's ERASE: its first field's */
                f->ext &= ~(COB_SX_ERASE_EOL | COB_SX_ERASE_EOS);
            }
            if (blank_screen_entry && f->kind < 0 && !f->has_pic) continue;   /* just BLANK SCREEN */
            if (occ > 1 && f->kind < 0 && !f->has_pic) die_at(fline, "OCCURS on a screen group is not implemented");
            if (f->kind < 0 && !f->has_pic) {
                /* a group: its look composes over the enclosing one and its
                 * children inherit it; its position anchors the first child */
                if (gdepth == 16) die_at(fline, "screen groups nested more than 16 deep");
                int pf = gdepth ? gstk[gdepth - 1].flags : 0;
                int pfg = gdepth ? gstk[gdepth - 1].fg : sc->fg, pbg = gdepth ? gstk[gdepth - 1].bg : sc->bg;
                gstk[gdepth].level = fl;
                gstk[gdepth].flags = pf | f->flags;
                gstk[gdepth].fg = f->fg != 255 ? f->fg : pfg;
                gstk[gdepth].bg = f->bg != 255 ? f->bg : pbg;
                gstk[gdepth].line = f->line; gstk[gdepth].col = f->col;
                gstk[gdepth].subidx = -1;
                gstk[gdepth].usage = susage;
                if (ename[0] && strcmp(ename, "filler")) {
                    if (sym_lookup_quiet(ename)) die_at(fline, "'%s' is both a data item and a screen group", ename);
                    char dummy[40]; int d1, d2;
                    if (screen_ref(ename, dummy, sizeof dummy, &d1, &d2)) die_at(fline, "screen group '%s' is already a screen or group name", ename);
                    if (sc->nsub == sc->subcap) { sc->subcap = sc->subcap ? sc->subcap * 2 : 4; sc->sub = realloc(sc->sub, sc->subcap * sizeof *sc->sub); }
                    SGroup *g = &sc->sub[sc->nsub];
                    snprintf(g->name, sizeof g->name, "%s", ename);
                    g->first = sc->nf; g->count = 0;
                    gstk[gdepth].subidx = sc->nsub++;
                }
                gdepth++;
                continue;
            }
            if (f->kind < 0 && f->has_pic && ename[0] && strcmp(ename, "filler") && ename_tp >= 0) {
                /* a named item with a PICTURE and no FROM, TO or USING
                 * (BP-G4, -dialect=gnucobol only): its own storage, as
                 * GnuCOBOL gives every screen item -- an item of its name
                 * and picture, the slot USING it (ACAS's sys002 MOVEs to
                 * one).  Micro Focus's reference requires FROM, TO or
                 * USING with a PICTURE (its screen PICTURE clause, rule 2). */
                bp(BP_G4_SCREEN_ITEM_STORAGE, fline);
                if (sym_lookup_quiet(ename)) die_at(fline, "'%s' is both a data item and a screen item", ename);
                Sym *si = sym_new();
                snprintf(si->name, sizeof si->name, "%s", ename);
                si->level = 1; si->line = fline; si->has_pic = 1;
                snprintf(si->pic, sizeof si->pic, "%s", f->pic);
                si->pi = f->pi;
                si->usage = f->pi.category == PIC_NATIONAL ? U_NATIONAL : U_DISPLAY;
                f->kind = COB_SCR_USING; f->ref_tp = ename_tp;
            }
            if (f->kind < 0) die_at(fline, "a screen slot needs VALUE, or PIC with FROM, TO or USING");
            if (gdepth) {
                /* inherit the enclosing look; the input-only clauses reach
                 * only the fields that take input */
                int gf = gstk[gdepth - 1].flags;
                if (f->kind != COB_SCR_TO && f->kind != COB_SCR_USING)
                    gf &= ~(COB_SF_AUTO | COB_SF_SECURE | COB_SF_REQUIRED | COB_SF_FULL);
                f->flags |= gf;
                if (f->fg == 255) f->fg = gstk[gdepth - 1].fg;
                if (f->bg == 255) f->bg = gstk[gdepth - 1].bg;
                if (!f->line && gstk[gdepth - 1].line) f->line = gstk[gdepth - 1].line;
                if (!f->col && gstk[gdepth - 1].col) f->col = gstk[gdepth - 1].col;
                gstk[gdepth - 1].line = 0; gstk[gdepth - 1].col = 0;    /* the anchor is the first child's */
            } else {
                /* straight under the 01: its colours */
                if (f->fg == 255) f->fg = sc->fg;
                if (f->bg == 255) f->bg = sc->bg;
            }
            if (f->from_lit && !f->has_pic) die_at(fline, "a FROM/TO/USING slot needs a PICTURE");   /* rule 7: PICTURE with FROM */
            if (f->kind == COB_SCR_VALUE && f->has_pic) {
                /* PICTURE with VALUE (2002 13.15.2 rule 7, GR 3: the picture
                 * "may be omitted" for an alphanumeric literal, so it may be
                 * written): the literal shown in a field of the picture's
                 * size, as a MOVE puts it -- padded with spaces, or cut on
                 * the right, which is warned */
                int pnat = f->pi.category == PIC_NATIONAL;
                const char *what = f->from_lit ? "FROM" : "VALUE";
                if (f->pi.category != PIC_ALPHANUMERIC && f->pi.category != PIC_ALPHABETIC && !pnat)
                    die_at(fline, "a screen %s literal with a numeric or edited PICTURE is not implemented (2002 13.15.2 rule 7)", what);
                if (pnat != !!f->natlit)
                    die_at(fline, "a screen %s literal and its PICTURE are of one class: alphanumeric, or national", what);
                int u = pnat ? 2 : 1, cols = f->pi.bytes / u, have = f->value->len / u;
                if (have > cols)
                    warn_at(fline, "the screen %s literal (%d characters) is cut to its PICTURE's %d", what, have, cols);
                Tok *v = xmalloc(sizeof *v); *v = *f->value;
                char *b = xmalloc((size_t)cols * u + 1);
                for (int k = 0; k < cols; k++) {
                    if (k < have) memcpy(b + k * u, f->value->s + k * u, (size_t)u);
                    else if (pnat) { b[k * 2] = 0; b[k * 2 + 1] = ' '; }
                    else b[k] = ' ';
                }
                b[cols * u] = 0;
                v->s = b; v->len = cols * u;
                f->value = v; f->has_pic = 0;
            }
            if (f->kind == COB_SCR_VALUE) {
                if (f->has_pic) die_at(fline, "a VALUE slot takes no PICTURE");
                f->width = f->natlit ? nat_lit_cols((const unsigned char *)f->value->s, f->value->len) : f->value->len;
            }
            else {
                if (!f->has_pic && f->ref_tp && !f->from_lit) {
                    /* FROM, TO or USING with no PICTURE (BP-G5,
                     * -dialect=gnucobol only): the field takes its item's
                     * picture, as GnuCOBOL does (ACAS's sys002).  2023
                     * 13.17.3 rule 7 wants the PICTURE written. */
                    bp(BP_G5_SCREEN_SLOT_NO_PIC, fline);
                    int save_tp = g_tp; g_tp = f->ref_tp;
                    char *quals[8]; int nq = 0;
                    const char *nm = cur()->s; advance();
                    while ((at_word("of") || at_word("in")) && peek(1)->kind == T_WORD && nq < 8) { advance(); quals[nq++] = cur()->s; advance(); }
                    if (cur()->kind == T_LP)
                        for (int d = 0; cur()->kind != T_PERIOD; advance()) {
                            if (cur()->kind == T_LP) d++;
                            else if (cur()->kind == T_RP && !--d) break;
                            else if (cur()->kind == T_COLON) die_at(fline, "a screen slot with no PICTURE takes a whole item's, not a part's");
                        }
                    Sym *it = sym_lookup(nm, quals, nq, fline);
                    g_tp = save_tp;
                    if (!it->has_pic) die_at(fline, "'%s' has no PICTURE for the screen slot to take; write one", it->name);
                    snprintf(f->pic, sizeof f->pic, "%s", it->pic);
                    f->pi = it->pi; f->has_pic = 1;
                }
                if (!f->has_pic) die_at(fline, "a FROM/TO/USING slot needs a PICTURE");
                f->width = sfield_cols(f);
            }
            if (!f->line) f->line = prev ? prev->line : 1;        /* no LINE: the previous slot's line */
            if (!f->col) f->col = prev && prev->line == f->line ? prev->col + prev->width : 1;   /* no COLUMN: right after it */
            if ((f->flags & (COB_SF_SECURE | COB_SF_REQUIRED | COB_SF_FULL)) && f->kind != COB_SCR_TO && f->kind != COB_SCR_USING)
                die_at(fline, "SECURE, REQUIRED and FULL belong to an input field (TO or USING)");
            if (occ > 1 && f->kind != COB_SCR_VALUE)
                die_at(fline, "OCCURS on a FROM, TO or USING screen item (a table's elements, 2023 13.18.38.3 rule 13) is not implemented");
            if (pend_erase) { f->ext |= pend_erase; pend_erase = 0; }
            sc->nf++;
            for (int k = 1; k < occ; k++) {
                /* the next occurrence: the same entry, placed by the same
                 * clauses against the one before it */
                if (sc->nf == sc->fcap) { sc->fcap = sc->fcap ? sc->fcap * 2 : 16; sc->f = realloc(sc->f, sc->fcap * sizeof *sc->f); }
                SField *last = &sc->f[sc->nf - 1], *o = &sc->f[sc->nf];
                *o = *last;
                if (line_plus >= 0) o->line = last->line + line_plus;
                if (col_plus >= 0) o->col = (line_plus < 0 ? last->col + last->width : o->col) + col_plus;
                sc->nf++;
            }
        }
        while (gdepth) {
            int si = gstk[--gdepth].subidx;
            if (si >= 0) sc->sub[si].count = sc->nf - sc->sub[si].first;
        }
    }
}

/* the sections in their order (X3.23-1985 IV-34, which "defines the order
 * of their presentation"; 2023 13.2.1): each at most once, in this order */
static void section_order(int *last, int rank, const char *name)
{
    static const char *names[] = { "", "FILE", "WORKING-STORAGE", "LOCAL-STORAGE", "LINKAGE", "COMMUNICATION", "REPORT", "SCREEN" };
    if (rank == *last)
        die_at(cur()->line, "a second %s SECTION (%s)", name,
               g_std < 2002 ? "X3.23-1985 IV-34, the DATA DIVISION's sections" : "2023 13.2.1");
    if (rank < *last)
        die_at(cur()->line, "the %s SECTION follows the %s SECTION here, but comes before it (%s)", name, names[*last],
               g_std < 2002 ? "X3.23-1985 IV-34, the order of the DATA DIVISION's sections" : "2023 13.2.1");
    *last = rank;
}

static void parse_data_division(void)
{
    if (!accept_word("data")) { finish_data_division(); const_fill(); return; }
    expect_word("division"); expect_period();
    int last = 0;
    for (;;) {
        if (at_word("file") && is_word(peek(1), "section")) {
            section_order(&last, 1, "FILE");
            advance(); advance(); expect_period();
            while (at_word("fd") || at_word("sd")) parse_fd();
            g_cur_fd = -1;
            continue;
        }
        if ((at_word("fd") || at_word("sd")) && last == 0) {
            /* the FILE SECTION header left out (BP-D5) */
            bp(BP_D5_MF_NO_FILE_SECTION, cur()->line);
            section_order(&last, 1, "FILE");
            while (at_word("fd") || at_word("sd")) parse_fd();
            g_cur_fd = -1;
            continue;
        }
        if (at_word("working-storage")) {
            section_order(&last, 2, "WORKING-STORAGE");
            advance(); expect_word("section"); expect_period();
            while (cur()->kind == T_NUM || cur()->kind == T_SQL) { if (cur()->kind == T_SQL) parse_exec_sql_data(); else parse_data_item(); }
            continue;
        }
        if (at_word("local-storage") && is_word(peek(1), "section")) {
            /* COBOL 2002: automatic data, a fresh copy for every activation
             * (2023 8.6.4), reached through a cell like a LINKAGE record */
            if (g_std < 2002) die_at(cur()->line, "the LOCAL-STORAGE SECTION is COBOL 2002; compile with -std=2002 (docs/standards.md, Stage B)");
            section_order(&last, 3, "LOCAL-STORAGE");
            advance(); advance(); expect_period();
            g_in_local = 1;
            while (cur()->kind == T_NUM || cur()->kind == T_SQL) { if (cur()->kind == T_SQL) parse_exec_sql_data(); else parse_data_item(); }
            g_in_local = 0;
            continue;
        }
        if (at_word("linkage") && is_word(peek(1), "section")) {
            section_order(&last, 4, "LINKAGE");
            advance(); advance(); expect_period();
            g_in_linkage = 1;
            while (cur()->kind == T_NUM || cur()->kind == T_SQL) { if (cur()->kind == T_SQL) parse_exec_sql_data(); else parse_data_item(); }
            g_in_linkage = 0;
            continue;
        }
        if (at_word("report") && is_word(peek(1), "section")) {
            section_order(&last, 6, "REPORT");
            advance(); advance(); expect_period();
            while (at_word("rd")) parse_rd();
            continue;
        }
        if (at_word("screen") && is_word(peek(1), "section")) {
            if (g_std < 2002) bp(BP_E10_SCREEN_SECTION, cur()->line);
            section_order(&last, 7, "SCREEN");

            advance(); advance(); expect_period();
            parse_screen_section();
            continue;
        }
        if (at_word("communication") && is_word(peek(1), "section"))
            die_at(cur()->line, "the COMMUNICATION SECTION is deliberately out");
        break;
    }
    if (!at_division() && cur()->kind != T_EOF) die_at(cur()->line, "unexpected %s in the DATA DIVISION", tok_desc(cur()));
    finish_data_division();
    const_fill();
}
