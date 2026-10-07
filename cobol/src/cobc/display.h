/* s32-cobc: DISPLAY, positioned DISPLAY/ACCEPT.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- DISPLAY ---------------------------------------------------------- */

/* ---- RM/COBOL positioned DISPLAY / ACCEPT (GitHub #32, #33) ------------
 *   DISPLAY x LINE n [,] POSITION m [,] ERASE [EOS|EOL] [,] HIGH|LOW|REVERSE
 *           [,] SIZE n ...        DISPLAY x AT rrcc [WITH ERASE EOS|EOL]
 *   ACCEPT  x LINE n POSITION m PROMPT [UPDATE] [NO BEEP] [ECHO] [TAB] ...
 *           ACCEPT x AT rrcc [WITH PROMPT]
 * The Open Systems suite (~/open) paints every screen this way: RM never
 * had a SCREEN SECTION.  Each statement becomes a screen of its own, one
 * slot per operand, on the SCREEN SECTION runtime.  LINE/POSITION are the
 * slot's line/col -- 0 when absent, which the runtime reads as "the line
 * after the last positioned statement" / column 1 -- and an identifier's
 * value is stored into the slot before the call (AT rrcc likewise, split by
 * cob_scr_at).  SIZE is the width; ERASE, PROMPT and NO BEEP are the slot's
 * ext bits; UPDATE makes the ACCEPT slot USING; HIGH/LOW/REVERSE are the
 * flags slots already carry.  ECHO, OFF, TAB, CONVERT, BLINK, BEEP, UNIT
 * and CONTROL are accepted and ignored. */
#define SCRF_SIZE 32               /* sizeof(cob_scr_field) on the guest */

static int pos_word_at(int i)      /* token i begins a positioning clause */
{
    Tok *t = &g_tok[i];
    if (t->kind != T_WORD) return 0;
    if (i > 0 && is_word(&g_tok[i - 1], "function")) return 0;   /* FUNCTION REVERSE is the function, not reverse video */
    static const char *strong[] = { "line", "position", "erase", "prompt", "size", "high", "low", "reverse", "update", "at", NULL };
    for (int k = 0; strong[k]; k++) if (!strcmp(t->s, strong[k])) return 1;
    if (!strcmp(t->s, "no") && is_word(&g_tok[i + 1], "beep")) return 1;
    if (!strcmp(t->s, "with") && (is_word(&g_tok[i + 1], "erase") || is_word(&g_tok[i + 1], "prompt") ||
                                  (is_word(&g_tok[i + 1], "no") && is_word(&g_tok[i + 2], "beep")))) return 1;
    return 0;
}

/* does the DISPLAY/ACCEPT at g_tp carry a positioning clause?  A look ahead
 * to the end of the sentence: a period, the next verb, or a terminator. */
static int stmt_positioned(void)
{
    for (int i = g_tp; i < g_ntok; i++) {
        Tok *t = &g_tok[i];
        if (t->kind == T_PERIOD || t->kind == T_EOF) return 0;
        if (t->kind == T_WORD && i > g_tp && (is_verb(t->s) || is_terminator(t->s))) return 0;
        if (t->kind == T_WORD) {
            /* the phrases of an enclosing statement end the DISPLAY/ACCEPT:
             * READ ... AT END DISPLAY x NOT AT END ..., COMPUTE ... ON SIZE
             * ERROR DISPLAY x NOT ON SIZE ERROR ... (the suite's compute and
             * lineseq tests), IF ... ELSE, EVALUATE ... WHEN */
            static const char *stop[] = { "not", "on", "else", "when", "invalid", "exception", "overflow", "end-of-page", "eop", "also", NULL };
            for (int k = 0; stop[k]; k++) if (!strcmp(t->s, stop[k])) return 0;
            if (!strcmp(t->s, "at") && (is_word(&g_tok[i + 1], "end") || is_word(&g_tok[i + 1], "end-of-page") || is_word(&g_tok[i + 1], "eop"))) return 0;
            if (!strcmp(t->s, "size") && is_word(&g_tok[i + 1], "error")) return 0;
        }
        if (pos_word_at(i)) return 1;
    }
    return 0;
}

/* LINE n / POSITION n: an integer literal, or an identifier whose value the
 * statement stores into the slot at run time (tp: where it sits) */
static SField *g_pos_field;    /* the slot pos_int is filling: an explicit 0 marks it CONT */
static void pos_int(int *val, Ref **rp, const char *what)
{
    accept_word("is"); accept_word("number");
    if (cur()->kind == T_NUM) {
        *val = atoi(cur()->s); advance();
        /* RM: LINE 0 / POSITION 0 is "where the cursor is", the position after
         * the last thing painted (APPKJRNL builds a title from three DISPLAYs);
         * an omitted clause is the next line / column 1.  The runtime's
         * continue-after-the-last-slot rule covers the explicit zero. */
        if (*val == 0 && g_pos_field) g_pos_field->ext |= COB_SX_CONT;
        return;
    }
    if (cur()->kind != T_WORD || !rp) die_at(cur()->line, "%s needs an integer%s", what, rp ? " or a numeric identifier" : "");
    Ref r; parse_ref(&r);
    if (!is_numeric_sym(r.sym)) die_at(r.line, "%s needs a numeric identifier", what);
    *rp = xmalloc(sizeof **rp); **rp = r;
}

static void parse_pos_clauses(SField *f, int is_accept)
{
    g_pos_field = f;
    for (;;) {
        if (accept_word("with")) continue;
        if (at_word("line") || at_word("position") || at_word("column") || at_word("col") || at_word("at"))
            bp(BP_E7_POSITIONED_IO, cur()->line);
        if (accept_word("line")) { pos_int(&f->line, &f->line_r, "LINE"); continue; }
        if (accept_word("position") || accept_word("column") || accept_word("col")) { pos_int(&f->col, &f->col_r, "POSITION"); continue; }
        if (accept_word("at")) {
            if (accept_word("line")) {
                pos_int(&f->line, &f->line_r, "AT LINE");
                if (accept_word("position") || accept_word("column") || accept_word("col")) pos_int(&f->col, &f->col_r, "COLUMN");
                continue;
            }
            if (cur()->kind == T_NUM) { int v = atoi(cur()->s); advance(); f->line = v / 100; f->col = v % 100; continue; }
            if (cur()->kind != T_WORD) die_at(cur()->line, "AT needs rrcc or a numeric identifier");
            { Ref r; parse_ref(&r); if (!is_numeric_sym(r.sym)) die_at(r.line, "AT needs a numeric identifier");
              f->at_r = xmalloc(sizeof *f->at_r); *f->at_r = r; }
            continue;
        }
        if (accept_word("erase")) {
            if (accept_word("eos")) f->ext |= COB_SX_ERASE_EOS;
            else if (accept_word("eol")) f->ext |= COB_SX_ERASE_EOL;
            else { accept_word("screen"); f->ext |= COB_SX_ERASE_ALL; }   /* ERASE [SCREEN]: the whole screen */
            continue;
        }
        if (accept_word("prompt")) { f->ext |= COB_SX_PROMPT; if (cur()->kind == T_STR) { f->prompt = (unsigned char)cur()->s[0]; advance(); } continue; }
        if (accept_word("size")) { pos_int(&f->width, NULL, "SIZE"); continue; }
        if (accept_word("high")) { f->flags |= COB_SF_HIGHLIGHT; continue; }
        if (accept_word("low")) { f->flags |= COB_SF_LOWLIGHT; continue; }
        if (accept_word("reverse") || accept_word("reverse-video")) { f->flags |= COB_SF_REVERSE; continue; }
        if (accept_word("update")) { if (is_accept) f->kind = COB_SCR_USING; continue; }
        /* the screen entry's own clauses, as Micro Focus and RM write them on
         * the statement (BP-E7): AUTO[-SKIP] ends the field when it is full,
         * the rest as in the SCREEN SECTION; the input ones only on ACCEPT */
        if (accept_word("auto") || accept_word("auto-skip")) { if (is_accept) f->flags |= COB_SF_AUTO; continue; }
        if (accept_word("secure")) { if (is_accept) f->flags |= COB_SF_SECURE; continue; }
        if (accept_word("required") || accept_word("empty-check")) { if (is_accept) f->flags |= COB_SF_REQUIRED; continue; }
        if (accept_word("full") || accept_word("length-check")) { if (is_accept) f->flags |= COB_SF_FULL; continue; }
        if (accept_word("underline")) { f->flags |= COB_SF_UNDERLINE; continue; }
        if (at_word("foreground-color") || at_word("foreground-colour") || at_word("background-color") || at_word("background-colour")) {
            /* a colour 0-7, as in the SCREEN SECTION (a level 78 or
             * constant name arrives here as its number); positioned I/O
             * is BP-E7's, and its colours with it */
            Tok *t = cur(); advance();
            int bg = t->s[0] == 'b' || t->s[0] == 'B';
            accept_word("is");
            if (cur()->kind != T_NUM) die_at(t->line, "%s takes a colour number 0-7 here; an identifier is not implemented", t->s);
            int c = atoi(cur()->s); advance();
            if (c < 0 || c > 7) die_at(t->line, "a screen colour is 0-7 (black, blue, green, cyan, red, magenta, yellow, white)");
            if (bg) f->bg = c; else f->fg = c;
            continue;
        }
        if (accept_word("highlight")) { f->flags |= COB_SF_HIGHLIGHT; continue; }
        if (accept_word("lowlight")) { f->flags |= COB_SF_LOWLIGHT; continue; }
        if (accept_word("no")) { expect_word("beep"); f->ext |= COB_SX_NOBEEP; continue; }
        if (accept_word("blink") || accept_word("echo") || accept_word("off") || accept_word("tab") || accept_word("convert") || accept_word("beep")) continue;
        if (accept_word("unit") || accept_word("control")) { Opnd o; parse_operand(&o); continue; }
        break;
    }
}

static Screen *screen_synth(void)
{
    if (g_nscreen == g_scrcap) { g_scrcap = g_scrcap ? g_scrcap * 2 : 4; g_screens = realloc(g_screens, g_scrcap * sizeof *g_screens); }
    Screen *sc = &g_screens[g_nscreen++];
    memset(sc, 0, sizeof *sc);
    snprintf(sc->name, sizeof sc->name, "(positioned %d)", g_nscreen);   /* not a word: screen_ref never matches it */
    return sc;
}

static SField *screen_synth_field(Screen *sc)
{
    if (sc->nf == sc->fcap) { sc->fcap = sc->fcap ? sc->fcap * 2 : 4; sc->f = realloc(sc->f, sc->fcap * sizeof *sc->f); }
    SField *f = &sc->f[sc->nf++];
    memset(f, 0, sizeof *f);
    f->fg = f->bg = 255; f->ext = COB_SX_POS; f->srcline = cur()->line;
    return f;
}

/* a literal of width bytes: len bytes of text padded with fill, or, fill
 * < 0, the text repeated (a figurative constant, ALL) */
static Tok *pos_literal(const char *bytes, int len, int width, int fill)
{
    Tok *t = xmalloc(sizeof *t); memset(t, 0, sizeof *t);
    t->kind = T_STR; t->s = xmalloc(width + 1); t->len = width;
    for (int i = 0; i < width; i++) t->s[i] = i < len ? bytes[i] : fill < 0 ? bytes[i % len] : (char)fill;
    t->s[width] = 0;
    return t;
}

static void emit_pos_int(const Ref *r)   /* r1 = the integer value of the identifier */
{
    Opnd n; memset(&n, 0, sizeof n); n.kind = O_REF; n.ref = *r; n.line = r->line;
    emit_incompat(&n);
    if (opnd_hot_int(&n)) emit_hot_value(&n);
    else { Arg a[2] = { arg_ref(&n.ref), arg_desc(sym_desc(n.ref.sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
}

/* the statement: item addresses and run-time positions into the slots, then the runtime */
static void emit_pos_stmt(int si, const char *fn)
{
    Screen *sc = &g_screens[si];
    emit_screen_dyn_fill(sc, 0, sc->nf);
    char rec[48]; snprintf(rec, sizeof rec, ".Lscrf%d_%d", g_unit, si);
    for (int k = 0; k < sc->nf; k++) {
        SField *f = &sc->f[k];
        if (f->line_r) { emit_pos_int(f->line_r); emit_la_off("r2", rec, k * SCRF_SIZE + 2); emit("\tsth r2+0, r1"); }
        if (f->col_r)  { emit_pos_int(f->col_r);  emit_la_off("r2", rec, k * SCRF_SIZE + 4); emit("\tsth r2+0, r1"); }
        if (f->at_r)    { emit_pos_int(f->at_r); emit("\tadd r4, r0, r1"); emit_la_off("r3", rec, k * SCRF_SIZE); emit_call("cob_scr_at"); }
        if (f->dynlen) {
            /* the part's length -- in columns, a national character two
             * bytes -- into the width, or under SIZE into the value word */
            Arg a[1] = { arg_rlen(f->ref) };
            emit_args(a, 1);
            if (f->ref->rm_nat) emit("\tsrli r3, r3, 1");
            emit_la_off("r2", rec, k * SCRF_SIZE + (f->width ? 12 : 8)); emit("\tstw r2+0, r3");
        }
    }
    char lab[48]; snprintf(lab, sizeof lab, ".Lscr%d_%d", g_unit, si);
    emit_la("r3", lab); emit_call(fn);
}

/* the columns a plain DISPLAY of a binary or packed numeric item takes
 * (libcob's cob_display_field: sign, digits, point), or 0 when the item
 * is not one -- a DISPLAY-usage item is shown as it is stored */
static int pos_display_width(Sym *s)
{
    static const int cap[9] = { 0, 3, 5, 8, 10, 13, 15, 17, 19 };
    if (s->is_group || s->pi.category != PIC_NUMERIC) return 0;
    if (s->usage == U_DISPLAY || s->usage == U_NATIONAL || s->usage == U_FLOAT || s->usage == U_POINTER) return 0;
    if (s->pi.scale < 0 || strchr(s->pi.pat, 'P')) return 0;
    int digits = sym_notrunc(s) ? (s->size >= 1 && s->size <= 8 ? cap[s->size] : 19) : s->pi.digits;
    if (digits <= 0 || digits > 18) return 0;                /* wide items keep the old path */
    return digits + (s->pi.is_signed ? 1 : 0) + (s->pi.scale > 0 ? 1 : 0);
}

static void parse_display_positioned(void)
{
    int si = (int)(screen_synth() - g_screens);
    int first = 1;
    for (;;) {
        Tok *t = cur();
        if (t->kind == T_PERIOD || t->kind == T_EOF) break;
        if (!at_operand() && !(t->kind == T_WORD && (is_figurative(t->s) || !strcmp(t->s, "all")))) break;
        Opnd o; parse_operand(&o);
        SField *f = screen_synth_field(&g_screens[si]);
        if (!first) f->ext |= COB_SX_CONT;
        first = 0;
        parse_pos_clauses(f, 0);
        switch (o.kind) {
        case O_REF:
            f->kind = COB_SCR_FROM; f->item = o.ref.sym; f->dyn = 1;
            f->ref = xmalloc(sizeof *f->ref); *f->ref = o.ref;
            f->has_pic = 1;
            if (o.ref.rm && (o.ref.rm_lx || !o.ref.rm_len) && !o.ref.rm_bit) {
                /* a part of computed length (ACAS's pl015: line-7-19
                 * (Screen-Start:Screen-End)), or to the item's end from a
                 * computed start: its characters, as many as the length
                 * says when the statement runs, stored into the slot's
                 * width (emit_pos_stmt); with SIZE, the width is SIZE's
                 * and the part fills it from the left */
                f->dynlen = 1;
                if (o.ref.rm_nat) { f->pi.category = PIC_NATIONAL; f->pi.bytes = o.ref.sym->size; }
                else { f->pi.category = PIC_ALPHANUMERIC; f->pi.bytes = o.ref.sym->size; }
                break;
            }
            if (o.ref.rm) {
                /* a part: shown as its own characters (as ACCEPT's) */
                sfield_part(f, &o.ref, o.line);
                int chars = (int)o.ref.rm_len;
                if (o.ref.rm_nat) { f->pi.category = PIC_NATIONAL; f->pi.bytes = 2 * chars; }
                else { f->pi.category = PIC_ALPHANUMERIC; f->pi.bytes = chars; }
                if (!f->width) f->width = chars;
                break;
            }
            if (sym_is_national(o.ref.sym)) {
                /* national text in columns (cobol ISSUES-92): a column a character position, SIZE counting columns */
                int n = o.ref.sym->size / 2;
                f->pi.category = PIC_NATIONAL; f->pi.bytes = 2 * n;
                if (!f->width) f->width = n;
                break;
            }
            {
                /* a binary or packed item: its storage is not its text.
                 * Shown as a plain DISPLAY shows it -- a sign when it is
                 * signed, its digits, a point when it has a fraction --
                 * in a field of that many columns (it was cut to the
                 * item's bytes: 1234 in PIC 9(4) COMP showed "12") */
                Sym *ns = o.ref.sym;
                int nw = pos_display_width(ns);
                if (nw > 0) {
                    f->dispval = 1;
                    if (!f->width) f->width = nw;
                    f->pi.category = PIC_ALPHANUMERIC; f->pi.bytes = f->width;
                    break;
                }
            }
            if (sym_bitlike(o.ref.sym)) {
                /* a bit item or bit group: its boolean positions, 0 and 1 characters */
                if (!f->width) f->width = o.ref.sym->bits;
                f->pi.category = PIC_BOOLEAN; f->pi.bytes = f->width;
                break;
            }
            if (!f->width) f->width = !o.ref.sym->is_group && o.ref.sym->usage == U_NATIONAL ? o.ref.sym->size / 2 : o.ref.sym->size;
            f->pi.category = PIC_ALPHANUMERIC; f->pi.bytes = f->width;
            break;
        case O_STR:
            f->kind = COB_SCR_VALUE;
            if (o.tok->nat) {
                f->value = o.tok; f->natlit = 1;
                if (!f->width) f->width = nat_lit_cols((const unsigned char *)o.tok->s, o.tok->len);
                break;
            }
            if (!f->width || f->width == o.tok->len) { f->value = o.tok; f->width = o.tok->len; }
            else f->value = pos_literal(o.tok->s, o.tok->len, f->width, ' ');
            break;
        case O_NUM: {
            char txt[48]; int k = 0;
            if (o.num.neg) txt[k++] = '-';
            for (int i = 0; i < o.num.ndigits; i++) {
                if (o.num.scale && i == o.num.ndigits - o.num.scale) txt[k++] = g_dp_comma ? ',' : '.';
                txt[k++] = o.num.digits[i];
            }
            f->kind = COB_SCR_VALUE; if (!f->width) f->width = k;
            f->value = pos_literal(txt, k, f->width, ' ');
            break; }
        case O_FIG: case O_ALL: {
            int len = o.kind == O_ALL ? o.tok->len : 1;
            char one = o.kind == O_ALL ? 0 : (char)fig_byte(o.tok->s);
            f->kind = COB_SCR_VALUE; if (!f->width) f->width = len;
            f->value = pos_literal(o.kind == O_ALL ? o.tok->s : &one, len, f->width, -1);
            break; }
        default: die_at(o.line, "a positioned DISPLAY takes identifiers and literals");
        }
    }
    emit_pos_stmt(si, "cob_screen_display");
}

static void parse_env_exception(void);
static void env_text_args(Opnd *o, const char *what);

/* the items SPECIAL-NAMES gives the terminal: the CRT STATUS the ACCEPT's
 * ending goes to, the CURSOR item its cursor comes from and goes back to */
static void emit_crt_item(const char *name, const char *what, const char *fn, int line)
{
    g_cen_ctx = CEN_PTR; Sym *cs = sym_lookup(name, NULL, 0, line); g_cen_ctx = 0;
    if (rec_indirect(&g_sym[cs->record])) die_at(line, "a %s item cannot be the %s yet", indirect_kind(&g_sym[cs->record]), what);
    char b[80]; snprintf(b, sizeof b, "%s+%d", g_sym[cs->record].label, cs->offset);
    emit_la("r3", b);
    snprintf(b, sizeof b, ".Ld%d", sym_desc(cs));
    emit_la("r4", b);
    emit_call(fn);
}
static void emit_crt_items(int line)
{
    if (g_crt_status_name[0]) emit_crt_item(g_crt_status_name, "CRT STATUS", "cob_crt_status", line);
    if (g_cursor_name[0]) {
        g_cen_ctx = CEN_PTR; Sym *cs = sym_lookup(g_cursor_name, NULL, 0, line); g_cen_ctx = 0;
        if (cs->size != 6 || (!cs->is_group && (cs->usage != U_DISPLAY || cs->pi.category != PIC_NUMERIC || cs->pi.is_signed || cs->pi.scale)))
            die_at(line, "the CURSOR item '%s' is six digits: an unsigned 9(6), or a group of two 9(3) (2023 12.3.7 rule 29)", cs->name);
        emit_crt_item(g_cursor_name, "CURSOR", "cob_crt_cursor", line);
    }
}
static void parse_accept_positioned(Ref *r)
{
    int si = (int)(screen_synth() - g_screens);
    SField *f = screen_synth_field(&g_screens[si]);
    f->kind = COB_SCR_TO; f->item = r->sym; f->dyn = 1;
    f->ref = xmalloc(sizeof *f->ref); *f->ref = *r;
    parse_pos_clauses(f, 1);
    f->has_pic = 1;
    if (r->rm && (r->rm_lx || !r->rm_len) && !r->rm_bit) {
        /* a part of computed length: the field as wide as the part is
         * when the statement runs (emit_pos_stmt), keyed into the part
         * through its writable descriptor (sfield_part) */
        sfield_part(f, r, r->line);
        f->dynlen = 1;
        if (r->rm_nat) { f->pi.category = PIC_NATIONAL; f->pi.bytes = r->sym->size; }
        else { f->pi.category = PIC_ALPHANUMERIC; f->pi.bytes = r->sym->size; }
    } else
    if (r->rm) {
        /* a part (abrignoli_COBSOFT keys a CPF number into f-cpf(07:03)
         * and its neighbours): a field of the part's characters */
        sfield_part(f, r, r->line);
        int chars = (int)r->rm_len;
        if (r->rm_nat) { f->pi.category = PIC_NATIONAL; f->pi.bytes = 2 * chars; }
        else { f->pi.category = PIC_ALPHANUMERIC; f->pi.bytes = chars; }
        if (!f->width) f->width = chars;
    } else
    if (sym_is_national(r->sym)) {
        /* national input (cobol ISSUES-92): the field a column a character position */
        f->pi.category = PIC_NATIONAL; f->pi.bytes = r->sym->size;
        if (!f->width) f->width = r->sym->size / 2;
    } else {
        /* the field is as wide as the item's picture: its storage size
         * for a DISPLAY item, its picture's positions for a binary or
         * packed one (PIC 9(4) COMP is four columns, not two) */
        if (!f->width) f->width = !r->sym->is_group && r->sym->pi.bytes &&
                                  r->sym->usage != U_DISPLAY ? r->sym->pi.bytes : r->sym->size;
        if (r->sym->is_group || !r->sym->pi.bytes) { f->pi.category = PIC_ALPHANUMERIC; f->pi.bytes = f->width; }
        else {
            f->pi = r->sym->pi; snprintf(f->pic, sizeof f->pic, "%s", r->sym->pic);
            if (r->sym->usage == U_DISPLAY) { f->sign_lead = r->sym->sign_lead; f->sign_sep = r->sym->sign_sep; }   /* the item's SIGN is the field's */
        }
    }
    emit_crt_items(r->line);
    emit_pos_stmt(si, "cob_screen_accept");
    parse_env_exception();                      /* a function key, or no field to accept into (rule 25) */
    accept_word("end-accept");
}

static void parse_accept_1(void);
/* ACCEPT into a national item (cobol ISSUES-70): the text arrives as
 * UTF-8 and is moved, so a byte that begins no UTF-8 character becomes
 * U+FFFD and, checked, EC-DATA-CONVERSION (as a MOVE, 14.9.25 rule 6) */
static int g_accept_nat_check;
static void parse_accept(void)
{
    g_accept_nat_check = 0;
    parse_accept_1();
    if (g_accept_nat_check) {
        int Lok = new_label();
        emit_call("cob_nat_conv_bad");
        emit("\tbeq r1, r0, .L%d", Lok);
        emit_ec_raise(ec_find("EC-DATA-CONVERSION", 0));
        emit_label(Lok);
    }
}
/* ACCEPT or DISPLAY screen-name AT ... (2002's screen formats take an
 * AT phrase): the screen placed with its origin there.  At line 1,
 * column 1 -- AT 0101 or AT LINE 1 COLUMN 1, the only placement ACAS
 * uses (cobol ISSUES-124) -- the screen is where its own clauses put it;
 * any other origin is not implemented. */
static void screen_at_origin(int line)
{
    if (!accept_word("at")) return;
    int l = -1, c = -1;
    if (cur()->kind == T_NUM && strlen(cur()->s) == 4) {
        int v = atoi(cur()->s); l = v / 100; c = v % 100; advance();
    } else {
        if (accept_word("line")) { accept_word("number"); if (cur()->kind == T_NUM) { l = atoi(cur()->s); advance(); } }
        if (accept_word("column") || accept_word("col") || accept_word("position")) {
            accept_word("number"); if (cur()->kind == T_NUM) { c = atoi(cur()->s); advance(); }
        }
        if (l < 0) l = 1;
        if (c < 0) c = 1;
    }
    if (l != 1 || c != 1)
        die_at(line, "a screen placed AT a position other than line 1, column 1 is not implemented");
}

/* ACCEPT or DISPLAY screen-name ... WITH attributes: GnuCOBOL takes the
 * phrase and drops it (its cob_screen_display and cob_screen_accept get
 * none of it); the standard's and Micro Focus's screen formats have no
 * WITH (MF's WITH is its format 3, of an item).  Under -dialect=gnucobol
 * it is read and ignored (BP-G6) -- but for ACCEPT's UPDATE, which is
 * BP-G3 and returned as 1. */
static int screen_with_phrase(int line, int is_accept)
{
    if (!at_word("with")) return 0;
    int from = g_tp, upd = 0, other = 0;
    SField dummy; memset(&dummy, 0, sizeof dummy); dummy.fg = dummy.bg = 255; dummy.kind = -1;
    SField *save = g_pos_field;
    parse_pos_clauses(&dummy, is_accept);
    g_pos_field = save;
    for (int k = from; k < g_tp; k++) {
        if (is_word(&g_tok[k], "with")) continue;
        if (is_accept && is_word(&g_tok[k], "update")) upd = 1;
        else other = 1;
    }
    if (upd) bp(BP_G3_ACCEPT_SCREEN_UPDATE, line);
    if (other) bp(BP_G6_SCREEN_WITH_IGNORED, line);
    return upd;
}

static void parse_accept_1(void)
{
    Tok *t = cur();
    if (t->kind == T_WORD) {
        char scrlab[40]; int sfirst, scount;
        Screen *scp = screen_ref(t->s, scrlab, sizeof scrlab, &sfirst, &scount);
        if (scp) {
            advance();
            /* a screen with output items and none for input is DISPLAYed,
             * not ACCEPTed (2023 14.9.1.3 rule 4) */
            int nin = 0, nout = 0;
            for (int k = sfirst; k < sfirst + scount; k++) {
                if (scp->f[k].kind == COB_SCR_TO || scp->f[k].kind == COB_SCR_USING) nin++;
                else nout++;
            }
            if (nout && !nin) die_at(t->line, "ACCEPT of '%s', which has FROM or VALUE items and no TO or USING item (2023 14.9.1.3 rule 4)", t->s);
            screen_at_origin(t->line);
            int upd = screen_with_phrase(t->line, 1);    /* WITH UPDATE (BP-G3): the TO fields start from their items */
            emit_screen_dyn_fill(scp, sfirst, scount);
            emit_crt_items(t->line);
            if (upd) emit_call("cob_scr_update_next");
            emit_la("r3", scrlab); emit_call("cob_screen_accept");
            parse_env_exception();              /* a function key, or no input field (2023 14.9.1.4 rules 24-25) */
            accept_word("end-accept");
            return;
        }
    }
    Ref r; parse_ref(&r);
    if (r.sym->strong) die_at(r.line, "ACCEPT into the strongly-typed group '%s' (2023 14.9.1.3 rule 1)", r.sym->name);
    int nat = ref_is_national(&r);
    /* the positioning words are looked for in this statement only: the
     * item is read, so a verb or scope terminator here already begins
     * what follows (ACCEPT A ACCEPT B LINE 3 made the first positioned) */
    if (!(cur()->kind == T_WORD && (is_verb(cur()->s) || is_terminator(cur()->s))) && stmt_positioned()) {
        parse_accept_positioned(&r); return;
    }
    if (nat && ec_on_name("EC-DATA-CONVERSION")) {
        emit_call("cob_nat_conv_bad");              /* clear what an earlier MOVE left: at end of file nothing is moved */
        g_accept_nat_check = 1;
    }
    if (accept_word("from")) {
        if (at_word("environment-value") || at_word("environment")) {
            /* FROM ENVIRONMENT-VALUE, the variable DISPLAY ... UPON
             * ENVIRONMENT-NAME chose; FROM ENVIRONMENT name, one named
             * here (BP-E31; MF ACCEPT rules 8 and 56).  No such variable:
             * the exception, the item left as it was */
            bp(BP_E31_ENVIRONMENT, r.line);
            int named = at_word("environment"); advance();
            if (named) {
                Opnd no; parse_operand(&no);
                env_text_args(&no, "ACCEPT ... FROM ENVIRONMENT");
                emit("\tstw sp+%d, r3", SLOT_A); emit("\tstw sp+%d, r4", SLOT_B);
                Arg a[2] = { arg_ref(&r), arg_desc(sym_desc(r.sym)) }; emit_args(a, 2);
                emit("\tadd r5, r3, r0"); emit("\tadd r6, r4, r0");
                emit("\tldw r3, sp+%d", SLOT_A); emit("\tldw r4, sp+%d", SLOT_B);
                emit_call("cob_env_accept_named");
            } else {
                Arg a[2] = { arg_ref(&r), arg_desc(sym_desc(r.sym)) }; emit_args(a, 2);
                emit_call("cob_env_accept");
            }
            parse_env_exception();
            accept_word("end-accept");
            return;
        }
        if (at_word("argument-number") || at_word("argument-value") || at_word("command-line")) {
            const char *fn = at_word("argument-number") ? "cob_accept_argnum"
                           : at_word("argument-value") ? "cob_accept_argval" : "cob_accept_cmdline";
            if (at_word("argument-number") && !is_numeric_sym(r.sym)) die_at(r.line, "ACCEPT ... FROM ARGUMENT-NUMBER needs a numeric item");
            advance();
            Arg a[2] = { arg_ref(&r), arg_desc(sym_desc(r.sym)) };
            emit_args(a, 2);
            emit_call(fn);
            if (at_word("on") || at_word("exception") || at_word("not")) die_at(cur()->line, "ACCEPT ... ON EXCEPTION is not implemented");
            accept_word("end-accept");
            return;
        }
        if ((at_word("lines") || at_word("columns")) && !mnemonic_kind(cur()->s)) {
            /* the terminal's size (X/Open; BP-E32) */
            if (!is_numeric_sym(r.sym)) die_at(r.line, "ACCEPT ... FROM %s needs a numeric item", at_word("lines") ? "LINES" : "COLUMNS");
            bp(BP_E32_SCREEN_DIMS, r.line);
            int cols = at_word("columns"); advance();
            Arg a[3] = { arg_imm(cols), arg_ref(&r), arg_desc(sym_desc(r.sym)) };
            emit_args(a, 3);
            emit_call("cob_accept_scr_dim");
            accept_word("end-accept");
            return;
        }
        if (at_word("date") || at_word("day") || at_word("time") || at_word("day-of-week")) {
            /* the unsigned integer of the text -- YYMMDD, YYDDD, HHMMSShh, 1 (Monday) to 7 -- by the MOVE rules */
            int which = at_word("date") ? 0 : at_word("day") ? 1 : at_word("time") ? 2 : 3;
            advance();
            /* DATE YYYYMMDD and DAY YYYYDDD: the four-digit year (COBOL 2002) */
            if ((which == 0 && at_word("yyyymmdd")) || (which == 1 && at_word("yyyyddd"))) {
                if (g_std < 2002) die_at(cur()->line, "ACCEPT ... FROM %s %s is COBOL 2002; compile with -std=2002", which ? "DAY" : "DATE", which ? "YYYYDDD" : "YYYYMMDD");
                advance(); which += 4;
            }
            if (!r.rm && !r.sym->is_group && (r.sym->pi.category == PIC_ALPHABETIC || r.sym->pi.category == PIC_BOOLEAN || r.sym->usage == U_BIT))
                die_at(r.line, "ACCEPT '%s' FROM DATE, DAY, TIME or DAY-OF-WEEK: an alphabetic or boolean item does not take the digits (%s)", r.sym->name,
                       g_std < 2002 ? "X3.23-1985 ACCEPT general rule 6: the MOVE rules" : "2023 14.9.1.3 rule 3");
            Arg a[3] = { arg_imm(which), arg_ref(&r), arg_desc(sym_desc(r.sym)) };
            emit_args(a, 3);
            emit_call("cob_accept_datetime");
            accept_word("end-accept");
            return;
        }
        if (cur()->kind == T_WORD && mnemonic_kind(cur()->s) == 1) {
            advance();
            Arg a[2] = { arg_ref(&r), arg_desc(sym_desc(r.sym)) };
            emit_args(a, 2);
            emit_call("cob_accept_console");
            accept_word("end-accept");
            return;
        }
        if (cur()->kind == T_WORD && !mnemonic_kind(cur()->s))
            die_at(cur()->line, "ACCEPT FROM '%s': not a mnemonic-name of SPECIAL-NAMES, nor DATE, DAY, TIME, DAY-OF-WEEK (%s)", cur()->s,
                   g_std < 2002 ? "X3.23-1985 ACCEPT syntax rule 2" : "2023 14.9.1.3 rule 2");
        die_at(cur()->line, "ACCEPT FROM %s is not implemented", tok_desc(cur()));
    }
    /* ACCEPT identifier: a line from standard input */
    {
        Arg a[2] = { arg_ref(&r), arg_desc(sym_desc(r.sym)) };
        emit_args(a, 2);
        emit_call("cob_accept_console");
        accept_word("end-accept");
    }
}

/* [ON] EXCEPTION ... [NOT [ON] EXCEPTION ...] after a statement that left
 * 1 in r1 for its exception condition (the environment's) */
static void parse_env_exception(void)
{
    if (!(at_word("on") || at_word("exception") || (at_word("not") && (is_word(peek(1), "on") || is_word(peek(1), "exception"))))) return;
    emit("\tstw sp+%d, r1", SLOT_C);
    Phrases ph; memset(&ph, 0, sizeof ph);
    if (at_word("on") || at_word("exception")) {
        accept_word("on"); expect_word("exception");
        ph.has_on = 1; ph.on = parse_block();
    }
    if (at_word("not")) {
        advance(); accept_word("on"); expect_word("exception");
        ph.has_not = 1; ph.not_on = parse_block();
    }
    emit_phrases(&ph, SLOT_C, 0);
}

/* r3, r4: an alphanumeric literal's or item's bytes, for the environment */
static void env_text_args(Opnd *o, const char *what)
{
    if (o->kind == O_STR && !o->tok->nat) { Arg a[2] = { arg_label(lit_label((unsigned char *)o->tok->s, o->tok->len)), arg_imm(o->tok->len) }; emit_args(a, 2); return; }
    if (o->kind == O_REF && (o->ref.sym->is_group || o->ref.sym->pi.category == PIC_ALPHANUMERIC || o->ref.sym->pi.category == PIC_ALPHABETIC) && !sym_is_national(o->ref.sym)) {
        Arg a[2] = { arg_ref(&o->ref), arg_len(o) }; emit_args(a, 2); return;
    }
    die_at(o->line, "%s takes an alphanumeric literal or item (Micro Focus DISPLAY rule 6)", what);
}

static int lw_display(Opnd *ops, int n, int no_adv, int a0);     /* lower.h */
static void parse_display(void)
{
    int line = cur()->line;
    int n = 0, no_adv = 0, a0 = g_nasm;
    Opnd lw_ops[16]; int lw_n = 0;     /* (MAXOPS, defined later) */
    if (cur()->kind == T_WORD) {
        char scrlab[40]; int sfirst, scount;
        Screen *scp = screen_ref(cur()->s, scrlab, sizeof scrlab, &sfirst, &scount);
        if (scp) { int sl = cur()->line; advance(); screen_at_origin(sl); screen_with_phrase(sl, 0); emit_screen_dyn_fill(scp, sfirst, scount); emit_la("r3", scrlab); emit_call("cob_screen_display"); return; }
    }
    /* DISPLAY n UPON ARGUMENT-NUMBER: the next ARGUMENT-VALUE will be n */
    if (stmt_positioned()) { parse_display_positioned(); return; }
    if (is_word(peek(1), "upon") && (is_word(peek(2), "environment-name") || is_word(peek(2), "environment-value"))) {
        /* DISPLAY x UPON ENVIRONMENT-NAME | ENVIRONMENT-VALUE (BP-E31):
         * one operand, the variable's name, or its value set */
        bp(BP_E31_ENVIRONMENT, line);
        Opnd o; parse_operand(&o);
        advance();
        int val = at_word("environment-value"); advance();
        env_text_args(&o, val ? "DISPLAY UPON ENVIRONMENT-VALUE" : "DISPLAY UPON ENVIRONMENT-NAME");
        if (val) emit_call("cob_env_set_value");
        else { emit_call("cob_env_set_name"); emit_li("r1", 0); }   /* ON EXCEPTION ignored (MF DISPLAY rule 6) */
        parse_env_exception();
        return;
    }
    if (is_word(peek(1), "upon") && is_word(peek(2), "argument-number")) {
        Opnd o; parse_operand(&o);
        emit_incompat(&o);
        if (!opnd_hot_int(&o)) {
            if (o.kind != O_REF || !is_int_item(o.ref.sym)) die_at(o.line, "DISPLAY ... UPON ARGUMENT-NUMBER needs an integer");
            Arg a[2] = { arg_ref(&o.ref), arg_desc(sym_desc(o.ref.sym)) }; emit_args(a, 2); emit_call("cob_load_int");
        } else emit_hot_value(&o);
        emit("\tadd r3, r1, r0");
        emit_call("cob_display_upon_argnum");
        advance(); advance();
        return;
    }
    if (is_word(peek(1), "upon") && (is_word(peek(2), "sysout") || is_word(peek(2), "console") || is_word(peek(2), "syserr") || is_word(peek(2), "stderr"))) {
        /* the console: an ordinary DISPLAY */
    }
    /* UPON SYSERR (or STDERR, or a mnemonic-name for either): the line goes
     * to the error stream.  UPON follows the operands, so look ahead to it
     * -- to the end of the statement: a period, the next verb, or a word
     * that ends a scope -- and switch before any operand is written. */
    int to_err = 0;
    for (int k = g_tp, depth = 0; k < g_ntok; k++) {
        Tok *t = &g_tok[k];
        if (t->kind == T_PERIOD || t->kind == T_EOF) break;
        if (t->kind == T_LP) { depth++; continue; }
        if (t->kind == T_RP) { depth--; continue; }
        if (depth || t->kind != T_WORD) continue;
        if (!strcmp(t->s, "upon")) {
            const Tok *d = k + 1 < g_ntok ? &g_tok[k + 1] : NULL;
            to_err = d && d->kind == T_WORD && (!strcmp(d->s, "syserr") || !strcmp(d->s, "stderr") || mnemonic_kind(d->s) == 4);
            break;
        }
        if (k > g_tp && (is_verb(t->s) || !strncmp(t->s, "end-", 4) || !strcmp(t->s, "else") || !strcmp(t->s, "when"))) break;
    }
    if (to_err) { emit("\taddi r3, r0, 1"); emit_call("cob_display_err"); }
    for (;;) {
        Tok *t = cur();
        if (t->kind == T_WORD && !strcmp(t->s, "upon")) {
            advance();
            if (accept_word("sysout") || accept_word("console") || accept_word("syserr") || accept_word("stderr")) continue;
            if (cur()->kind == T_WORD && (mnemonic_kind(cur()->s) == 2 || mnemonic_kind(cur()->s) == 4)) { advance(); continue; }
            if (cur()->kind == T_WORD && !mnemonic_kind(cur()->s) && !at_word("argument-number") && !at_word("environment-name") && !at_word("environment-value"))
                die_at(t->line, "DISPLAY UPON '%s': not a mnemonic-name of SPECIAL-NAMES (%s)", cur()->s,
                       g_std < 2002 ? "X3.23-1985 DISPLAY syntax rule 2" : "2023 14.9.11.3 rule 2");
            die_at(t->line, "DISPLAY UPON %s is not implemented (ARGUMENT-NUMBER takes one operand)", cur()->s);
        }
        if (t->kind == T_WORD && (!strcmp(t->s, "with") || !strcmp(t->s, "no"))) {
            accept_word("with"); expect_word("no"); expect_word("advancing");
            no_adv = 1; break;
        }
        if (!at_operand() && !(t->kind == T_WORD && (is_figurative(t->s) || !strcmp(t->s, "all")))) break;
        Opnd o; parse_operand(&o);
        n++;
        if (lw_n < 16) lw_ops[lw_n++] = o;
        switch (o.kind) {
        case O_STR: {
            if (o.tok->nat) {                       /* a national literal: written as UTF-8 */
                char *u = xmalloc((size_t)o.tok->len * 2 + 1);
                int un = utf16be_to_utf8((const unsigned char *)o.tok->s, o.tok->len, u);
                Arg a[2] = { arg_label(lit_label((unsigned char *)u, un)), arg_imm(un) };
                emit_args(a, 2); emit_call("cob_display"); free(u); break;
            }
            Arg a[2] = { arg_label(lit_label((unsigned char *)o.tok->s, o.tok->len)), arg_imm(o.tok->len) };
            emit_args(a, 2); emit_call("cob_display"); break;
        }
        case O_NUM: {  /* a numeric literal displays as written */
            char txt[48]; int k = 0;
            if (o.num.neg) txt[k++] = '-';
            for (int i = 0; i < o.num.ndigits; i++) {
                if (o.num.scale && i == o.num.ndigits - o.num.scale) txt[k++] = g_dp_comma ? ',' : '.';
                txt[k++] = o.num.digits[i];
            }
            Arg a[2] = { arg_label(lit_label((unsigned char *)txt, k)), arg_imm(k) };
            emit_args(a, 2); emit_call("cob_display"); break;
        }
        case O_FIG: case O_ALL: {
            int len = o.kind == O_ALL ? o.tok->len : 1;
            unsigned char *b = xmalloc(len);
            if (o.kind == O_ALL) memcpy(b, o.tok->s, len); else b[0] = (unsigned char)fig_byte(o.tok->s);
            Arg a[2] = { arg_label(lit_label(b, len)), arg_imm(len) };
            free(b);
            emit_args(a, 2); emit_call("cob_display"); break;
        }
        default: {
            Arg a[2];
            emit_incompat(&o);
            opnd_args(&o, &a[0], &a[1], 0, 0);
            emit_args(a, 2); emit_call("cob_display_field"); break;
        }
        }
    }
    if (!n) die_at(line, "DISPLAY needs at least one operand");
    if (!no_adv) emit_call("cob_display_nl");
    if (to_err) { emit("\taddi r3, r0, 0"); emit_call("cob_display_err"); }
    if (!to_err && lw_n == n) lw_display(lw_ops, n, no_adv, a0);     /* an island's too (lower.h): its placeholder first, then this text */
}
