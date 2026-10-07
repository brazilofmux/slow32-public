/* s32-cobc: argument staging.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- argument staging ------------------------------------------------- */

enum { A_REF, A_LABEL, A_DESC, A_IMM, A_FUNC, A_VALUE, A_RDESC, A_RLEN, A_CONTENT, A_FDESC, A_FLEN, A_RLENC };
typedef struct { int kind; const Ref *ref; const char *label; int desc; long imm; Opnd *fn; } Arg;
static Arg arg_func(Opnd *o)       { Arg a = { A_FUNC, 0, 0, 0, 0, o }; return a; }
static Arg arg_value(Opnd *o)      { Arg a = { A_VALUE, 0, 0, 0, 0, o }; return a; }
static Arg arg_content(Opnd *o)    { Arg a = { A_CONTENT, 0, 0, 0, 0, o }; return a; }   /* BY CONTENT: a copy's address */
static Arg arg_fdesc(Opnd *o)      { Arg a = { A_FDESC, 0, 0, 0, 0, o }; return a; }   /* the descriptor of the function result just evaluated */
static Arg arg_flen(Opnd *o)       { Arg a = { A_FLEN, 0, 0, 0, 0, o }; return a; }    /* ... and its length in bytes */
static Arg arg_rdesc(const Ref *r) { Arg a = { A_RDESC, r, 0, 0, 0, 0 }; return a; }
static Arg arg_rlen(const Ref *r)  { Arg a = { A_RLEN, r, 0, 0, 0, 0 }; return a; }
static Arg arg_rlenc(const Ref *r) { Arg a = { A_RLENC, r, 0, 0, 0, 0 }; return a; }   /* the part's length, the part checked (cob_refmod_len_chk) */
static void opnd_args(Opnd *o, Arg *addr, Arg *desc, int other_size, int other_numeric);
static int g_slot_base;             /* staged operands of nested evaluations use higher slots */

static void emit_fn_value(Opnd *f);
static void emit_push_opnd(Opnd *o);

static Arg arg_ref(const Ref *r)   { Arg a = { A_REF, r, 0, 0, 0, 0 }; return a; }
static Arg arg_label(const char *l){ Arg a = { A_LABEL, 0, l, 0, 0, 0 }; return a; }
static Arg arg_desc(int d)         { Arg a = { A_DESC, 0, 0, d, 0, 0 }; return a; }
static Arg arg_imm(long v)         { Arg a = { A_IMM, 0, 0, 0, v, 0 }; return a; }

static const char *argreg(int i)
{
    static const char *r[] = { "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10" };
    return r[i];
}

/* load r3.. with the arguments; operands whose address needs a runtime
 * call are computed first and parked in frame slots */
/* r1 = the reference modification's start; its length in SLOT(slot).
 * The expressions may stage operands of their own, above this call's
 * slots. */
static void emit_args(const Arg *a, int n);
static void emit_hot_value(Opnd *o);

/* EC-BOUND-REF-MOD (2023 8.4.2.4): r1 holds the leftmost position; the
 * part must lie within the item.  len: the literal length, -1 when
 * omitted, -2 when computed (then in the frame slot emit_rm_start_len
 * filled).  r1 survives. */
static void emit_refmod_check(const Ref *r, long len, int slot)
{
    int Lok = new_label();
    emit("\tadd r12, r1, r0");
    emit("\tadd r3, r1, r0");
    if (len == -2) emit("\tldw r4, sp+%d", SLOT(slot));
    else emit_li("r4", len == -3 ? -1 : len);    /* -3: computed, and checked with the length */
    if (r->sym->any_len) {
        /* the size the argument gave it, in its descriptor */
        emit_desc_addr("r5", sym_desc(r->sym)); emit("\tldw r5, r5+8");
        if (r->rm_nat) emit("\tsrai r5, r5, 1");
    } else
    emit_li("r5", r->rm_bit ? r->sym->bits : r->rm_nat ? r->sym->size / 2 : r->sym->size);   /* in character positions, or bits */
    emit_call("cob_bound_refmod");
    emit("\tbeq r1, r0, .L%d", Lok);
    emit_ec_raise(ec_find("EC-BOUND-REF-MOD", 0));
    emit_label(Lok);
    emit("\tadd r1, r12, r0");
}

static void emit_rm_start_len(const Ref *r, int slot)
{
    if (r->rm_odo) {
        /* the group's current length: base + DEPENDING ON x element */
        Opnd po; memset(&po, 0, sizeof po); po.kind = O_REF; po.ref.sym = r->odo_dep; po.ref.line = r->line;
        if (is_hot_int(r->odo_dep)) emit_hot_value(&po);
        else { Arg a[2] = { arg_ref(&po.ref), arg_desc(sym_desc(r->odo_dep)) }; emit_args(a, 2); emit_call("cob_load_int"); }
        emit("\tadd r3, r0, r1"); emit_li("r4", r->odo_base); emit_li("r5", r->odo_elem);
        if (r->odo_bits) { emit_li("r6", r->odo_bits); emit_call("cob_odo_length_bits"); }
        else emit_call("cob_odo_length");
        emit("\tstw sp+%d, r1", SLOT(slot));
        emit_li("r1", 1);
        return;
    }
    if (r->rm_len) emit_li("r1", r->rm_len);
    else if (r->rm_lx) { emit_expr_pos(r->rm_lx); }
    else if (r->bitsub) {
        /* a bit array element's part to the element's end: its bits past the start */
        if (r->bitu_start) emit_li("r1", r->sym->bits - r->bitu_start + 1);
        else { emit_expr_pos(r->rm_sx); emit_li("r2", r->sym->bits + 1); emit("\tsub r1, r2, r1"); }
    }
    else emit_li("r1", 0);
    emit("\tstw sp+%d, r1", SLOT(slot));
    if (r->bitsub) {
        /* a bit array's element: the array's bit (i - 1) * bits + start */
        if (!r->rm_start) { emit_bitelem_start(r, r->rm_lx ? -2 : r->rm_len ? (long)r->rm_len : -1, slot, 0); return; }
        emit_li("r1", r->rm_start);
        if (r->rm_lx && ec_on_name("EC-BOUND-REF-MOD")) {
            emit_li("r1", r->bitu_start); emit_refmod_check(r, -2, slot); emit_li("r1", r->rm_start);
        }
        return;
    }
    if (r->rm_start) emit_li("r1", r->rm_start);
    else { emit_expr_pos(r->rm_sx); }
    if (r->rm_lx && ec_on_name("EC-BOUND-REF-MOD")) emit_refmod_check(r, -2, slot);   /* a computed length */
}

static void emit_args(const Arg *a, int n)
{
    int slotted[8] = { 0 };
    int base = g_slot_base;
    if (base + n > NSLOTS) die_at(cur()->line, "internal: too many staged operands");
    g_slot_base += n;
    for (int i = 0; i < n; i++) {
        if (a[i].kind == A_REF && ref_needs_call(a[i].ref)) {
            emit_ref_addr(a[i].ref, "r1");
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_RDESC || a[i].kind == A_RLEN || a[i].kind == A_RLENC) {
            const Ref *r = a[i].ref;
            emit_rm_start_len(r, base + i);
            emit("\tadd r4, r1, r0");
            emit("\tldw r5, sp+%d", SLOT(base + i));
            emit_desc_addr("r3", r->bitsub ? bitarray_desc(r->sym) : sym_desc(r->sym));
            emit_call(a[i].kind == A_RDESC ? "cob_refmod_desc" : a[i].kind == A_RLENC ? "cob_refmod_len_chk" : "cob_refmod_len");
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_CONTENT) {
            /* BY CONTENT: the callee gets a copy, from the runtime's arena,
             * released after the CALL (cob_content_pop) */
            Opnd *o = a[i].fn;
            if (o->kind == O_REF && o->ref.rm) {
                /* a reference-modified part: its length in bytes, worked
                 * out as its descriptor's is (X3.23-1985 and 2023 put no
                 * restriction on it; cobol ISSUES-94) */
                const Ref *r = &o->ref;
                emit_rm_start_len(r, base + i);
                emit("\tadd r4, r1, r0");
                emit("\tldw r5, sp+%d", SLOT(base + i));
                emit_desc_addr("r3", sym_desc(r->sym));
                emit_call("cob_refmod_len");
                emit("\tstw sp+%d, r1", SLOT(base + i));
                emit_ref_addr(r, "r3");
                emit("\tldw r4, sp+%d", SLOT(base + i));
            }
            else if (o->kind == O_REF) { emit_ref_addr(&o->ref, "r3"); emit_li("r4", o->ref.sym->size); }
            else if (o->kind == O_STR) { emit_la("r3", lit_label((unsigned char *)o->tok->s, o->tok->len)); emit_li("r4", o->tok->len); }
            else { emit_la("r3", call_num_lit_label(&o->num)); emit_li("r4", o->num.ndigits); }
            emit_call("cob_content_push");
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_VALUE) {
            /* BY VALUE: the item's integer value, widened to a word */
            Opnd *o = a[i].fn;
            if (o->kind == O_ADDR) emit_ptr_value(o, "r1");
            else if (o->kind == O_REF && is_hot_int(o->ref.sym)) { emit_ref_addr(&o->ref, "r3"); emit_load_int(o->ref.sym, "r3", "r1"); }
            else if (o->kind == O_REF) { emit_incompat(o); emit_ref_addr(&o->ref, "r3"); emit_desc_addr("r4", sym_desc(o->ref.sym)); emit_call("cob_load_int"); }
            else emit_li("r1", (long)numlit_int(&o->num));
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_FUNC) {
            if (a[i].fn->fsaved) { char l[24]; snprintf(l, sizeof l, ".L%d", a[i].fn->fsaved - 1); emit_la("r1", l); }
            else emit_fn_value(a[i].fn);   /* r1 = the result buffer */
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_FDESC) {
            /* a result whose length is known only now: its descriptor, taken
             * while it is still the last function evaluated (the A_FUNC
             * before this one) */
            emit_li("r3", a[i].fn->fwnum ? 3 : a[i].fn->fnat ? 1 : a[i].fn->fbool ? 2 : 0);
            emit_call("cob_fn_var_desc");
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_FLEN) {
            /* the same, its length: the A_FUNC before this one */
            emit_call("cob_fn_last_len");
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        }
    }
    for (int i = 0; i < n; i++) {
        const char *reg = argreg(i);
        if (slotted[i]) {
            emit("\tldw %s, sp+%d", reg, SLOT(base + i));
            if (g_cen_on && a[i].kind == A_REF) cen_moved(a[i].ref->sym, reg);      /* the address formed above, back from its slot */
            continue;
        }
        switch (a[i].kind) {
        case A_REF:   emit_ref_addr(a[i].ref, reg); break;
        case A_LABEL: emit_la(reg, a[i].label); break;
        case A_DESC:  emit_desc_addr(reg, a[i].desc); break;
        case A_IMM:   emit_li(reg, a[i].imm); break;
        default: die_at(cur()->line, "internal: unstaged argument kind");
        }
    }
    g_slot_base = base;
}

/* r3 = an argument's address, r4 = its length in bytes: a reference
 * modification's own length, not the item's size (FUNCTION NUMVAL(t(p:1))
 * read t from p to its end until 2026-09-30) */
/* a screen item's part of computed length: its bytes, stored into the
 * part's writable descriptor before the screen is used (sfield_part) */
static void emit_dynpart_len(SField *f)
{
    Arg a[1] = { arg_rlen(f->ref) };
    emit_args(a, 1);
    char dl[32]; snprintf(dl, sizeof dl, ".Ld%d+8", f->idesc - 1);
    emit_la("r2", dl);
    emit("\tstw r2+0, r3");
}

static void emit_ref_addr_len(Ref *r)
{
    if (r->rm) { Arg a[2] = { arg_ref(r), arg_rlen(r) }; emit_args(a, 2); }
    else { emit_ref_addr(r, "r3"); emit_li("r4", (long)r->sym->size); }
}

static void emit_move(Opnd *src, Ref *dst);
static Opnd expr_opnd(void);
static int at_arith_op(void);
static void init_record(Sym *rec, int si, int defaults);
static void emit_store_receivers(Ref *rs, int *rounded, int nr, int hot, int giving, int subtract, int size_err,
                                 long long sum_mag, int sum_nonneg);
static void sym_finish(Sym *s);
static int layout(int si, int base);
static void set_dims(int si, int ndims, const int *counts, const int *strides);
static const char *link_name(const char *name);
static void emit_args(const Arg *a, int n);
static Arg arg_ref(const Ref *r);
