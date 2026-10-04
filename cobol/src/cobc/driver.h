/* s32-cobc: driver.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ====================================================================== */
/* Driver                                                                  */
/* ====================================================================== */

/* The activation descriptor (-std=2002; cobol ISSUES-49).  cob_act_enter
 * reads it at every entry: the active count, whether the program is
 * RECURSIVE, its name for the EC-PROGRAM-RECURSIVE-CALL message, the words
 * an activation owns but which live in static cells -- LINKAGE and
 * LOCAL-STORAGE cells, FILE STATUS pointers into them, TIMES counters --
 * saved on entry and restored on return, and each LOCAL-STORAGE record's
 * cell, initial image and size, a fresh copy per activation (2023 8.6.4).
 * Only a RECURSIVE program can be re-entered, so only its words are saved. */
static void emit_act_desc(void)
{
    char words[256][48]; int nw = 0;
    if (g_recursive) {
        for (int i = g_sym_base; i < g_nsym; i++) {
            Sym *s = &g_sym[i];
            if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0) continue;
            if ((s->is_linkage || s->is_local) && nw < 256) snprintf(words[nw++], sizeof words[0], "%s", s->label);
            if (s->any_len && nw < 256) snprintf(words[nw++], sizeof words[0], ".Ld%d+8", sym_desc(s));   /* its size, the argument's */
        }
        for (int i = g_file_base; i < g_nfile; i++) {
            File *f = &g_files[i];
            if (f->status_sym && (g_sym[f->status_sym->record].is_linkage || g_sym[f->status_sym->record].is_local) && nw < 256)
                snprintf(words[nw++], sizeof words[0], ".Lf%d_%d+16", f->unit, i);
            if (f->assign_sym && (g_sym[f->assign_sym->record].is_linkage || g_sym[f->assign_sym->record].is_local) && nw < 256)
                snprintf(words[nw++], sizeof words[0], ".Lf%d_%d+24", f->unit, i);
        }
        for (int k = 0; k < g_ncnt; k++)
            if (g_cnt_unit[k] == g_unit && nw < 256) snprintf(words[nw++], sizeof words[0], ".Lcnt%d", k);
        if (nw == 256) die_at(0, "internal: more than 256 per-activation words in '%s'", g_progid);
    }
    int nl = 0;
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (!(s->is_cond || s->parent >= 0 || s->redefines >= 0) && s->is_local) nl++;
    }
    char nm[80]; snprintf(nm, sizeof nm, "%s", g_progid);
    const char *nlab = lit_label((const unsigned char *)nm, (int)strlen(nm) + 1);
    emit("\t.p2align 2");
    emit(".Lact%d:\t# activation descriptor", g_unit);
    emit("\t.word 0");                              /* active instances */
    emit("\t.word %d", g_recursive);
    emit("\t.word %s", nlab);
    emit("\t.word 0");                              /* the outermost activation's block, kept (cob_act_enter) */
    emit("\t.word %d", nw);
    for (int k = 0; k < nw; k++) emit("\t.word %s", words[k]);
    emit("\t.word %d", nl);
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || !s->is_local) continue;
        /* a function's result temporary is written by its call before it is
         * read: storage of its own per activation, no initial image to copy */
        emit("\t.word %s", s->label);
        if (s->is_ftemp) emit("\t.word 0"); else emit("\t.word %s_i", s->label);
        emit("\t.word %d", s->image_size);
    }
}

static void emit_sql_data(void);
static void emit_unit_data(void)
{
    emit("");
    emit("\t.data");
    emit_sql_data();
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0 || s->is_rc) continue;
        if (s->is_local) {
            /* a cell for the activation's copy, and the copy's initial state
             * (cob_act_enter makes a fresh one on every entry) */
            emit("\t.p2align 2");
            emit("%s:\t# local-storage %02d %s (%d bytes, the activation's)", s->label, s->level, s->name, s->image_size);
            emit("\t.word 0");
            emit("\t.section .rodata");
            emit("\t.p2align 3");
            emit("%s_i:", s->label);
            emit_bytes(s->image, s->image_size);
            emit("\t.data");
            continue;
        }
        if (s->is_based && !s->is_linkage) {
            emit("\t.p2align 2");
            emit("%s:\t# based %02d %s (%d bytes, wherever SET ADDRESS OF puts it)", s->label, s->level, s->name, s->image_size);
            emit("\t.word 0");
            continue;
        }
        if (s->is_linkage || s->is_external) {
            emit("\t.p2align 2");
            emit("%s:\t# %s %02d %s (%d bytes %s)", s->label, s->is_linkage ? "linkage" : "external", s->level, s->name, s->image_size,
                 s->is_linkage ? "at the caller's" : "shared by name");
            emit("\t.word 0");
            continue;
        }
        emit("\t.p2align 3");
        emit("%s:\t# %02d %s (%d bytes)", s->label, s->level, s->name, s->image_size);
        emit_bytes(s->image, s->image_size);
        /* the record's initial state, for CANCEL */
        emit("\t.section .rodata");
        emit("\t.p2align 3");
        emit("%s_i:", s->label);
        emit_bytes(s->image, s->image_size);
        emit("\t.data");
    }
    if (g_std >= 2002) emit_act_desc();
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        /* a line sequential file of national records holds UTF-8 text:
         * the runtime converts, told by varying = 2 (cobol ISSUES-74) */
        int varying = f->varying;
        if (f->org == COB_ORG_LINESEQ) {
            int nrec = 0, arec = 0;
            for (int k = g_sym_base; k < g_nsym; k++)
                if (g_sym[k].level == 1 && g_sym[k].fd == i) { if (sym_is_national(&g_sym[k])) nrec = k + 1; else arec = k + 1; }
            if (nrec && arec)
                die_at(f->line, "the line sequential file '%s' has national and alphanumeric records; they are all one or the other here", f->name);
            if (nrec) varying = 2;
            if (g_std >= 2002) varying |= 4;    /* 2023 14.9.30 rule 15: 06, the rest for the next READ (cobol ISSUES-94 N8) */
        }
        emit("\t.p2align 2");
        emit(".Lf%d_%d:\t# %s", f->unit, i, f->name);
        emit("\t.byte %d,%d,%d,0", f->org, f->access, f->optional);
        emit("\t.word 0");
        if (f->rec >= 0 && !f->external) emit("\t.word %s", g_sym[g_sym[f->rec].record].label); else emit("\t.word 0");   /* an EXTERNAL file's record area is set at entry */
        emit("\t.word %d", f->recsize);
        if (f->status_sym && !rec_indirect(&g_sym[f->status_sym->record]))
            emit("\t.word %s+%d", g_sym[f->status_sym->record].label, f->status_sym->offset);
        else emit("\t.word 0");                            /* a LINKAGE or EXTERNAL status item: its address is stored at entry */
        if (f->assign_lit) {
            unsigned char *z = xmalloc(f->assign_lit->len + 1);
            memcpy(z, f->assign_lit->s, f->assign_lit->len);
            emit("\t.word %s", lit_label(z, f->assign_lit->len + 1));
            free(z);
        } else emit("\t.word 0");
        if (f->assign_sym && !rec_indirect(&g_sym[f->assign_sym->record])) { emit("\t.word %s+%d", g_sym[f->assign_sym->record].label, f->assign_sym->offset); emit("\t.word %d", f->assign_sym->size); }
        else if (f->assign_sym) { emit("\t.word 0"); emit("\t.word %d", f->assign_sym->size); }   /* a LINKAGE or EXTERNAL name: its address is stored at entry */
        else { emit("\t.word 0"); emit("\t.word 0"); }
        emit("\t.word 0");
        emit("\t.word 0");
        if (f->key_sym) { emit("\t.word %d", f->key_sym->offset); emit("\t.word %d", f->key_sym->size); }
        else { emit("\t.word 0"); emit("\t.word 0"); }
        emit("\t.word 0");
        emit("\t.word %d", varying);
        emit("\t.word %d", f->minlen);
        if (f->dep_sym) { emit("\t.word %s+%d", g_sym[f->dep_sym->record].label, f->dep_sym->offset); emit("\t.word .Ld%d", sym_desc(f->dep_sym)); }
        else { emit("\t.word 0"); emit("\t.word 0"); }
        if (f->relkey_sym) { emit("\t.word %s+%d", g_sym[f->relkey_sym->record].label, f->relkey_sym->offset); emit("\t.word .Ld%d", sym_desc(f->relkey_sym)); }
        else { emit("\t.word 0"); emit("\t.word 0"); }
        emit("\t.word 0");                  /* rel_pos, rel_last: the runtime's */
        emit("\t.word 0");
        emit("\t.word 0");                 /* (use_para, use_modes: the compiler now emits the USE choice itself) */
        emit("\t.word .Luse%d", g_unit);
        emit("\t.word 0");
        emit("\t.word 0");                  /* locked (CLOSE WITH LOCK) */
        emit("\t.word 0");                  /* eof_seen */
        emit("\t.word 0");                  /* fpos */
        if (f->nalt) emit("\t.word .Lak%d_%d", g_unit, i); else emit("\t.word 0");   /* ALTERNATE RECORD KEYs */
        emit("\t.word %d", f->nalt);
        if (f->linage) emit("\t.word .Llin%d_%d", g_unit, i); else emit("\t.word 0");   /* LINAGE: lines/footing/top/bottom */
        for (int w = 0; w < 7; w++) emit("\t.word 0");    /* lin_lines lin_foot lin_top lin_bot lin_counter lin_eop lin_needs_top */
        emit("\t.word 0");                                 /* saved_status (EXTERNAL) */
        emit("\t.word 0");                                 /* reversed (OPEN INPUT ... REVERSED) */
        emit("\t.word 0");                                 /* pr_state (the print file's cursor) */
        emit("\t.word 0");                                 /* rbuf, rpos, rlen: the runtime's line-sequential read buffer */
        emit("\t.word 0");
        emit("\t.word 0");
        if (f->codeset) {                                  /* code_out, code_in: CODE-SET's two tables */
            emit("\t.word .Lcso%d_%d", f->unit, i); emit("\t.word .Lcsi%d_%d", f->unit, i);
        } else { emit("\t.word 0"); emit("\t.word 0"); }
        if (f->nsplitw) emit("\t.word .Lspk%d_%d", f->unit, i); else emit("\t.word 0\t# no split keys");   /* split: the split keys' table */
        emit("\t.word 0");                                 /* fast_r1, fast_r, fast_w1, fast_w: the runtime's (READ and WRITE's short entries) */
        if (f->external) { emit(".Lfx%d_%d:\t# the shared connector of EXTERNAL %s", f->unit, i, f->name); emit("\t.word 0"); }
    }
    /* CODE-SET: every elementary item of the file's records DISPLAY, a
     * signed number SIGN SEPARATE (X3.23-1985 CODE-SET rule 1; 2023
     * 13.18.13.3 rule 3a) -- the conversion is of characters -- and the
     * tables: native to the medium's code, and back */
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->codeset) continue;
        for (int k = g_sym_base; k < g_nsym; k++) {
            Sym *x = &g_sym[k];
            if (x->is_cond || x->is_group || x->is_index || x->level == 66) continue;
            int r = k; while (g_sym[r].parent >= 0) r = g_sym[r].parent;
            if (g_sym[r].fd != i) continue;
            if (x->usage != U_DISPLAY || x->pi.category == PIC_NATIONAL || x->pi.category == PIC_BOOLEAN)
                die_at(x->line, "'%s' in the CODE-SET file %s must be USAGE DISPLAY (X3.23-1985 CODE-SET rule 1; 2023 13.18.13.3 rule 3a)", x->name, f->name);
            if (x->pi.category == PIC_NUMERIC && x->pi.is_signed && !x->sign_sep)
                die_at(x->line, "'%s' in the CODE-SET file %s: a signed number needs SIGN SEPARATE (X3.23-1985 CODE-SET rule 1)", x->name, f->name);
        }
        const unsigned char *out = g_alphabet[f->codeset - 1].rank;
        unsigned char in[256];
        for (int c = 0; c < 256; c++) in[out[c]] = (unsigned char)c;
        emit("\t.section .rodata");
        emit(".Lcso%d_%d:\t# CODE-SET %s: native to the medium", f->unit, i, g_alphabet[f->codeset - 1].name);
        emit_bytes(out, 256);
        emit(".Lcsi%d_%d:\t# and back", f->unit, i);
        emit_bytes(in, 256);
        emit("\t.data");
    }
    emit("\t.p2align 2");
    emit(".Luse%d:\t# USE sections by open mode", g_unit);
    for (int m = 0; m < 5; m++) emit("\t.word 0");
    if (g_collate >= 0) {
        emit(".Lcoll%d:\t# PROGRAM COLLATING SEQUENCE %s: rank of each character", g_unit, g_alphabet[g_collate].name);
        emit_bytes(g_alphabet[g_collate].rank, 256);
    }
    for (int i = 0; i < g_nalphabet; i++)
        if (g_alphabet[i].used) {
            emit(".Lalph%d_%d:\t# ALPHABET %s: rank of each character, for SORT", g_unit, i, g_alphabet[i].name);
            emit_bytes(g_alphabet[i].rank, 256);
        }
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->linage) continue;
        emit("\t.p2align 2");
        emit(".Llin%d_%d:\t# LINAGE of %s: lines, footing, top, bottom -- literal, item, descriptor", g_unit, i, f->name);
        for (int w = 0; w < 4; w++) {
            emit("\t.word %ld", f->lin_lit[w]);
            if (f->lin_sym[w]) { emit("\t.word %s+%d", g_sym[f->lin_sym[w]->record].label, f->lin_sym[w]->offset); emit("\t.word .Ld%d", sym_desc(f->lin_sym[w])); }
            else { emit("\t.word 0"); emit("\t.word 0"); }
        }
    }
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->nalt) continue;
        emit("\t.p2align 2");
        emit(".Lak%d_%d:\t# ALTERNATE RECORD KEYs of %s", g_unit, i, f->name);
        for (int a = 0; a < f->nalt; a++) { emit("\t.word %d", f->alt[a].sym->offset); emit("\t.word %d", f->alt[a].sym->size); emit("\t.word %d", f->alt[a].dups); }
    }
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->nsplitw) continue;
        emit("\t.p2align 2");
        emit(".Lspk%d_%d:\t# split keys of %s: count, then slot, parts, (offset, length)...", f->unit, i, f->name);
        for (int k = 0; k < f->nsplitw; k++) emit("\t.word %d", f->splitw[k]);
    }
    for (int i = 0; i < g_nsorttab; i++) {
        SortTab *t = &g_sorttab[i];
        emit("\t.p2align 2");
        emit(".Lsk%d_%d:\t# SORT keys", g_unit, t->id);
        for (int k = 0; k < t->nk; k++) { emit("\t.word %d", t->k[k].offset); emit("\t.word .Ld%d", t->k[k].desc); emit("\t.word %d", t->k[k].descending); }
    }
    g_nsorttab = 0;
    for (int i = g_screen_base; i < g_nscreen; i++) {
        Screen *sc = &g_screens[i];
        emit("\t.p2align 2");
        emit(".Lscrf%d_%d:\t# screen %s slots", g_unit, i, sc->name);
        for (int k = 0; k < sc->nf; k++) {
            SField *f = &sc->f[k];
            sfield_resolve(f);
            emit("\t.byte %d,%d", f->kind | (f->dyn ? 0x80 : 0), f->flags);
            emit("\t.short %d", f->line); emit("\t.short %d", f->col);
            emit("\t.byte %d,%d", f->fg, f->bg);   /* FOREGROUND-COLOR, BACKGROUND-COLOR (255: not given) */
            emit("\t.word %d", f->width);
            if (f->kind == COB_SCR_VALUE) emit("\t.word %s", lit_label((unsigned char *)f->value->s, f->value->len)); else emit("\t.word 0");
            if (f->kind == COB_SCR_VALUE && f->natlit) emit("\t.word .Ld%d", nat_desc(f->value->len));   /* painted as national text */
            else if (f->has_pic) {
                Desc d; memset(&d, 0, sizeof d);
                switch (f->pi.category) {
                case PIC_NATIONAL: d.cat = COB_NATIONAL; break;
                case PIC_ALPHABETIC: d.cat = COB_ALPHA; break;
                case PIC_ALPHANUMERIC: d.cat = COB_ALNUM; break;
                case PIC_ALPHANUMERIC_EDITED: d.cat = COB_ALNUM_ED; break;
                case PIC_NUMERIC: d.cat = COB_NUM; break;
                default: d.cat = COB_NUM_ED; break;
                }
                d.usage = COB_U_DISPLAY; d.digits = (unsigned char)f->pi.digits; d.scale = (signed char)f->pi.scale;
                if (f->pi.is_signed) d.flags |= COB_F_SIGNED;
                if (f->blank_zero) d.flags |= COB_F_BLANKZ;
                if (f->pi.edited) snprintf(d.picstr, sizeof d.picstr, "%s", f->pi.pat);
                d.size = f->pi.bytes;
                emit("\t.word .Ld%d", desc_add(&d));
            } else emit("\t.word 0");
            if (f->item && f->dyn) { emit("\t.word .Lsdyn%d_%d_%d", g_unit, i, k); emit("\t.word .Ld%d", f->idesc ? f->idesc - 1 : sym_desc(f->item)); }
            else if (f->item) { emit("\t.word %s+%ld", g_sym[f->item->record].label, f->stat_off); emit("\t.word .Ld%d", f->idesc ? f->idesc - 1 : sym_desc(f->item)); }
            else { emit("\t.word 0"); emit("\t.word 0"); }
            emit("\t.byte %d,%d", f->ext, f->prompt ? f->prompt : '_'); emit("\t.short 0");   /* ext, prompt, rsv */
        }
        emit(".Lscr%d_%d:\t# screen %s", g_unit, i, sc->name);
        emit("\t.word %d", sc->nf);
        emit("\t.word %d", sc->blank_screen);
        emit("\t.word .Lscrf%d_%d", g_unit, i);
        for (int j = 0; j < sc->nsub; j++) {                  /* a named group: a window into the same slots */
            emit(".Lscrg%d_%d_%d:\t# screen %s group %s", g_unit, i, j, sc->name, sc->sub[j].name);
            emit("\t.word %d", sc->sub[j].count);
            emit("\t.word 0");
            emit("\t.word .Lscrf%d_%d+%d", g_unit, i, sc->sub[j].first * SCRF_SIZE);
        }
        for (int k = 0; k < sc->nf; k++)
            if (sc->f[k].dyn) { emit(".Lsdyn%d_%d_%d:\t# %s: the address, computed at ACCEPT/DISPLAY", g_unit, i, k, sc->f[k].item->name); emit("\t.word 0"); }
    }
    for (int i = 0; i < g_naltcell; i++) {
        emit("\t.p2align 2");
        emit(".Lalt%d_%d:\t# ALTERed paragraph's GO TO target", g_unit, g_altcell[i].para);
        if (g_altcell[i].target >= 0) emit("\t.word .Lp%d_%d", g_unit, g_altcell[i].target); else emit("\t.word 0");
    }
    g_naltcell = 0; g_naltname = 0;
    for (int i = g_report_base; i < g_nreport; i++) {
        Report *r = &g_reports[i];
        emit("\t.p2align 2");
        emit(".Lrpt%d_%d:\t# report %s", g_unit, i, r->name);
        emit("\t.word .Lf%d_%d", g_files[r->file].unit, r->file);
        emit("\t.word %d", r->page_limit); emit("\t.word %d", r->heading);
        emit("\t.word %d", r->first_detail); emit("\t.word %d", r->last_detail);
        emit("\t.word 0"); emit("\t.word 0"); emit("\t.word 0");    /* line_counter (20), page_counter (24), body_seen */
        emit("\t.word %d", r->footing); emit("\t.word 0");           /* footing, page_started */
        for (int w = 0; w < 6; w++) emit("\t.word 0");               /* first_gen brk next_line next_page suppress gi_pending */
    }
}

static void emit_rodata(void)
{
    emit("");
    emit("\t.data");
    for (int i = 0; i < g_ncnt; i++) { emit("\t.p2align 2"); emit(".Lcnt%d:", i); emit("\t.word 0"); }
    emit("");
    emit("\t.section .rodata");
    for (int i = 0; i < g_nlit; i++) {
        emit("%s:", g_lit[i].label);
        emit_bytes(g_lit[i].bytes, g_lit[i].len);
    }
    for (int i = 0; i < g_ndesc; i++) {
        Desc *d = &g_desc[i];
        if (d->picstr[0]) {
            emit(".Lpic%d:", i);
            emit_bytes((unsigned char *)d->picstr, (int)strlen(d->picstr) + 1);
        }
    }
    int nany = 0;
    for (int i = 0; i < g_ndesc; i++) nany += g_desc[i].anylen != 0;
    for (int pass = 0; pass < 1 + (nany > 0); pass++) {
        /* an ANY LENGTH item's descriptor is written at entry: .data */
        if (pass) emit("\t.data");
        for (int i = 0; i < g_ndesc; i++) {
            Desc *d = &g_desc[i];
            if (!d->anylen != !pass) continue;
            emit("\t.p2align 2");
            emit(".Ld%d:", i);
            emit("\t.byte %d,%d,%d,%d,%d,%d,0,0", d->cat, d->usage, d->digits, (unsigned char)d->scale, d->flags, d->flags2);
            emit("\t.word %d", d->size);
            if (d->picstr[0]) emit("\t.word .Lpic%d", i); else emit("\t.word 0");
        }
    }
}

static void usage(void)
{
    fprintf(stderr, "s32-cobc %s -- COBOL 85 for SLOW-32\n"
        "usage: s32-cobc [-free|-fixed] [-o out.s] source.cbl\n"
        "  -fixed   reference format (columns 7/8-72); the default\n"
        "  -free    free format (GnuCOBOL -free; majesty)\n"
        "  -m       module: no main entry, every unit a subprogram\n"
        "  -I dir   where COPY looks for copybooks (repeatable)\n"
        "  -std=85  X3.23-1985 and the 1989 intrinsics; the default\n"
        "  -std=2002 add the COBOL 2002 modules landed so far (docs/standards.md, Stage B)\n"
        "  -fnsig   only write the user functions' .s32fn signature files (docs/functions.md)\n"
        "  -fixed-columns=bytes  count reference-format columns in bytes, not characters (UTF-8 source)\n"
        "  -fbinary-byteorder=native  COMP/BINARY in SLOW-32's little-endian order, not big-endian (docs/usage.md)\n"
        "  -fcomp1=binary|float  COMP-1 as RM's binary or MF's float, whatever its PICTURE (default: binary with one)\n"
        "  -warn-74 warn where a COBOL 74 program needs updating (docs/behavior-points.md)\n"
        "  -warn-extensions warn where a program uses an extension to the standard it is compiled for\n", VERSION);
    exit(2);
}

int main(int argc, char **argv)
{
    const char *in = NULL, *out = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-free")) g_free = 1;
        else if (!strcmp(argv[i], "-fixed")) g_free = 0;
        else if (!strcmp(argv[i], "-m")) g_module = 1;
        else if (!strcmp(argv[i], "-I") && i + 1 < argc) { if (g_nincdir < 16) g_incdirs[g_nincdir++] = argv[++i]; }
        else if (!strncmp(argv[i], "-I", 2) && argv[i][2]) { if (g_nincdir < 16) g_incdirs[g_nincdir++] = argv[i] + 2; }
        else if (!strcmp(argv[i], "-o") && i + 1 < argc) out = argv[++i];
        else if (!strcmp(argv[i], "--version")) { printf("s32-cobc %s\n", VERSION); return 0; }
        else if (!strcmp(argv[i], "-warn-74")) g_warn74 = 1;
        else if (!strcmp(argv[i], "-warn-extensions")) g_warn_ext = 1;
        else if (!strcmp(argv[i], "-fnsig")) g_fnsig_only = 1;
        else if (!strcmp(argv[i], "-fixed-columns=bytes")) g_col_bytes = 1;
        else if (!strcmp(argv[i], "-fixed-columns=chars")) g_col_bytes = 0;
        else if (!strcmp(argv[i], "-fbinary-byteorder=native")) g_bin_native = 1;
        else if (!strcmp(argv[i], "-fbinary-byteorder=big-endian")) g_bin_native = 0;
        else if (!strcmp(argv[i], "-fcomp1=binary")) g_comp1 = 1;
        else if (!strcmp(argv[i], "-fcomp1=float")) g_comp1 = 0;
        else if (!strcmp(argv[i], "-fno-hot-arith")) g_nohx = 1;
        else if (!strcmp(argv[i], "-fno-loop-reg")) g_noloopreg = 1;
        else if (!strcmp(argv[i], "-fno-avail-reg")) g_noavailreg = 1;
        else if (!strcmp(argv[i], "-fno-native-items")) g_native_on = 0;
        else if (!strcmp(argv[i], "-fnative-items")) g_native_on = 1;
        else if (!strcmp(argv[i], "-fno-hir")) g_hir_on = 0;
        else if (!strcmp(argv[i], "-fhir")) g_hir_on = 1;
        else if (!strcmp(argv[i], "-fprofile-lines")) g_proflines = 1;
        else if (!strcmp(argv[i], "-dialect=mf")) g_dialect_mf = 1;
        else if (!strcmp(argv[i], "-dialect=gnucobol")) g_dialect_gnu = 1;
        else if (!strncmp(argv[i], "-dialect=", 9)) { fprintf(stderr, "s32-cobc: %s: the dialects are mf and gnucobol (docs/behavior-points.md)\n", argv[i]); return 2; }
        else if (!strcmp(argv[i], "-std=85") || !strcmp(argv[i], "-std=cobol85")) g_std = 85;
        else if (!strcmp(argv[i], "-std=2002") || !strcmp(argv[i], "-std=cobol2002")) { g_std = 2002; pic_max_digits = 31; }
        else if (!strcmp(argv[i], "-std=74") || !strcmp(argv[i], "-std=cobol74")) {
            fprintf(stderr, "s32-cobc: there is no -std=74: 74 programs compile as 85, and -warn-74 flags where their "
                            "meaning changed; full COBOL 74 is cobc370's job (docs/standards.md)\n");
            return 2;
        }
        else if (!strncmp(argv[i], "-std=", 5)) {
            fprintf(stderr, "s32-cobc: %s is not implemented; -std=85 (the default) and -std=2002 "
                            "(COBOL 2002, Stage B of docs/standards.md, as its modules land)\n", argv[i]);
            return 2;
        }
        else if (argv[i][0] == '-') usage();
        else if (in) usage();
        else in = argv[i];
    }
    if (!in) usage();
    g_file = in;
    g_cen_dir = getenv("S32_CENSUS_DIR");
    if (g_cen_dir && !g_cen_dir[0]) g_cen_dir = NULL;
    if (g_cen_dir) g_cen_on = 1;
    {   /* S32_NATIVE_ITEMS=0: -fno-native-items for every compile of a build that passes no flags through */
        const char *e = getenv("S32_NATIVE_ITEMS");
        if (e && !strcmp(e, "0")) g_native_on = 0;
        e = getenv("S32_HIR");                  /* S32_HIR=0: -fno-hir likewise */
        if (e && !strcmp(e, "0")) g_hir_on = 0;
    }

    char outbuf[1024];
    if (!out) {
        const char *base = strrchr(in, '/'); base = base ? base + 1 : in;
        snprintf(outbuf, sizeof outbuf, "%s", base);
        char *dot = strrchr(outbuf, '.'); if (dot) *dot = 0;
        strcat(outbuf, ".s");
        out = outbuf;
    }

    {   /* the external repository: signature files go beside the output */
        static char od[1024]; snprintf(od, sizeof od, "%s", out);
        char *sl = strrchr(od, '/'); if (sl) { *sl = 0; g_outdir = od; } else g_outdir = ".";
    }
    read_source(in);
    tokenize();
    if (g_free && g_std < 2002 && g_ntok) bp(BP_E11_FREE_FORMAT, g_tok[0].line);
    expand_types();
    prog_tree_scan();
    native_prepass();

    if (g_fnsig_only) g_noemit = 1;         /* signatures only: no code, no output file */
    else if (!g_native_child) {
        g_out = fopen(out, "w");
        if (!g_out) { fprintf(stderr, "s32-cobc: cannot write %s\n", out); return 1; }
        g_out_path = out;
    }
    emit("\t.file\t\"%s\"", in);
    emit("# s32-cobc %s", VERSION);

    for (;;) {
        /* one program unit; a source file may hold several, each closed
         * by END PROGRAM */
        g_nsym = 0; g_nfile = 0; g_npara = 0; g_nreport = 0; g_report_base = 0; g_nscreen = 0; g_screen_base = 0; g_nclass = 0; g_nswitch = 0; g_nalphabet = 0; g_nmnemonic = 0; g_last_item = -1;
        g_nsame_groups = 0; g_collate = -1; g_collate_name[0] = 0; g_lowval = 0x00; g_highval = 0xFF; g_cur_fd = -1; g_in_linkage = 0;
        g_sym_base = g_file_base = g_para_base = 0; g_udepth = 0; g_nuse = 0; g_initial = 0; g_recursive = 0; g_nsymch = 0;
        g_in_proc = 0;
        parse_identification_division();
        parse_environment_division();
        parse_data_division();
        native_apply();
        if (!at_word("procedure")) die_at(cur()->line, "expected PROCEDURE DIVISION, found %s", tok_desc(cur()));
        parse_procedure_division();
        if (!g_nerrors && !g_fnsig_only) emit_unit_data();   /* nothing is generated once anything has failed */
        if (cur()->kind == T_EOF) break;
        if (!g_saw_end_program) die_at(cur()->line, "unexpected %s after the program (a further program needs END PROGRAM before it)", tok_desc(cur()));
        g_unit = ++g_unit_counter;
    }
    if (g_native_child) _exit(0);           /* the census is taken: the parent has it */
    if (g_nerrors) fail();
    if (g_fnsig_only) return 0;
    emit_rodata();
    relax_branches();
    fclose(g_out);
    return 0;
}
