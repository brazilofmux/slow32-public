/* f77_parse.h -- recursive-descent Fortran 77 parser, lowering straight
 * to HIR as it goes.
 *
 * One pass, syntax-directed.  F77 requires declarations to precede
 * executable statements, so nothing needs a second look, and the copied
 * backend's SSA/optimizer does the work an AST would otherwise be built
 * to enable.  Control flow is handled with an explicit stack, and
 * forward GOTO targets work because HIR blocks can be created before
 * they are filled.
 *
 * STATEMENT CLASSIFICATION is the interesting part, and it cannot be
 * done by the lexer: F77 has no reserved words, so `IF` and `DO` are
 * legal variable names.  The rules implemented in f77_classify():
 *
 *   IF( ... )   -- match the balanced parens, then look at what follows:
 *                  THEN -> block IF, `=` -> assignment to the array
 *                  element IF(...), digits -> arithmetic IF, anything
 *                  else -> logical IF.
 *   DO          -- a DO statement needs a top-level `=` AND a top-level
 *                  `,` after it.  That is the whole difference between
 *                  `DO 20 I = 1, 10` (a loop) and `DO20I = 1.10` (an
 *                  assignment to the variable DO20I).
 *   otherwise   -- a top-level `=` means assignment; if there is none,
 *                  dispatch on a keyword prefix.
 */
#ifndef F77_PARSE_H
#define F77_PARSE_H

/* SLOW-32 is 32-bit, so addresses are plain ints. */
#define HL_ADDR_TY TY_INT

/* --- symbols -------------------------------------------------------- */

#define F77_MAX_SYM 1024
#define F77_MAX_RANK 7                 /* the F77 limit */
static char f77_sname[F77_MAX_SYM][F77_MAX_NAME];
static int  f77_sty[F77_MAX_SYM];      /* TY_INT / TY_FLOAT / TY_DOUBLE */
static int  f77_sval[F77_MAX_SYM];     /* HIR alloca */
static int  f77_srank[F77_MAX_SYM];    /* 0 = scalar */
static int  f77_slo[F77_MAX_SYM][F77_MAX_RANK];   /* lower bound per dim */
static int  f77_sext[F77_MAX_SYM][F77_MAX_RANK];  /* constant extent */
/* Adjustable dimensions: DOUBLE PRECISION A(LDA,1) inside a subprogram,
 * where LDA is itself a dummy argument.  LINPACK is built on this.  When
 * f77_sextsym[s][k] >= 0 the extent is that symbol's runtime value and
 * f77_sext[s][k] is meaningless. */
static int  f77_sextsym[F77_MAX_SYM][F77_MAX_RANK];
static int  f77_nsym;

/* Lookups start here, not at 0.  Inlining a subprogram pushes a fresh
 * scope so the callee's locals cannot resolve to identically-named
 * caller locals -- without this, a callee's `I` would silently become
 * the caller's `I`. */
static int  f77_scope_base;
static int  f77_label_base;

/* Is `s` a dummy argument?  Dummy arguments are passed BY REFERENCE in
 * Fortran, so the symbol's value is already an address and there is no
 * alloca behind it -- everything downstream works off an address, so
 * loads, stores and subscripting need no special case. */
static int  f77_sarg[F77_MAX_SYM];

/* Hi-word slot for a scalar DOUBLE local, or -1.
 *
 * A double is an 8-byte alloca whose hi word is normally reached with
 * `ADDI base,4` at each use.  That ADDI is a use of the alloca which is
 * neither a LOAD nor a STORE, and every mem2reg scan reads such a use
 * as address-taken -- so NO double was ever promoted to a register, and
 * every one lived in memory.  A routine with one double and one integer
 * emitted twelve frame accesses.
 *
 * Giving the hi word its OWN alloca, emitted beside the lo one, means
 * neither slot ever has its address taken: each has only direct word
 * LOAD/STOREs, which is exactly what the promoter accepts.  The hi slot
 * keeps the frame offset the ADDI would have addressed, so the layout
 * is unchanged. */
static int  f77_shi[F77_MAX_SYM];
static int  f77_split_count;
static int  f77_split_on = -1;

/* Static storage flags: member of a COMMON block / named in SAVE. */
static int  f77_scom[F77_MAX_SYM];
static int  f77_ssaved[F77_MAX_SYM];

/* Read-only dummy-load tracking, for LICM's h_ld_ro assertion.  F77
 * 15.9.3.6: while a subprogram executes, its scalar dummies cannot
 * legally be modified through any other name -- so a load of one is
 * loop-invariant even across stores and calls, UNLESS this unit
 * itself stores to it (assignment, DO variable, READ target, or its
 * address passed onward).  One-pass compilation means the store may
 * be seen after the load, so loads are recorded and the flag is
 * RETRACTED at unit end for every symbol that got stored. */
static int  f77_sstored[F77_MAX_SYM];
#define F77_MAX_LDRO 4096
static int  f77_ldro_inst[F77_MAX_LDRO];
static int  f77_ldro_sym[F77_MAX_LDRO];
static int  f77_ldro_n;

static void f77_ldro_note(int inst, int s) {
    if (inst < 0 || f77_ldro_n >= F77_MAX_LDRO) return;
    h_ld_ro[inst] = 1;
    f77_ldro_inst[f77_ldro_n] = inst;
    f77_ldro_sym[f77_ldro_n] = s;
    f77_ldro_n = f77_ldro_n + 1;
}

static void f77_ldro_finalize(void) {
    int i;
    for (i = 0; i < f77_ldro_n; i++) {
        if (f77_sstored[f77_ldro_sym[i]])
            h_ld_ro[f77_ldro_inst[i]] = 0;
    }
}
static int  f77_sgidx[F77_MAX_SYM];    /* ps_g* index once bound static */

/* PARAMETER named constants: no storage, value folded at parse time.
 * The value is kept in both domains and converted per the symbol's
 * type at each use. */
static int    f77_sparam[F77_MAX_SYM];
static int    f77_spival[F77_MAX_SYM];
static double f77_spdval[F77_MAX_SYM];

/* Explicitly typed by a type declaration (or a typed FUNCTION
 * header): what IMPLICIT NONE checks, and what an IMPLICIT statement
 * must not retype. */
static int    f77_styped[F77_MAX_SYM];

/* Per-unit implicit typing map, letter -> type.  bind_unit resets it
 * to the default rule (I-N INTEGER, the rest REAL) and the IMPLICIT
 * statement rewrites ranges; before the first bind (the phase-1 unit
 * scan) the default rule applies directly. */
static int f77_imp_map[26];
static int f77_imp_ready;
static int f77_imp_none;

/* Defined (tentatively) again beside their own logic further down. */
static int f77_decls_open;

static int f77_implicit_ty(char *nm) {
    int c;
    c = nm[0];
    if (f77_imp_ready && c >= 'A' && c <= 'Z') return f77_imp_map[c - 'A'];
    if (c >= 'I' && c <= 'N') return TY_INT;
    return TY_FLOAT;
}

static int f77_frame;   /* bytes of locals allocated so far */

static int f77_sym(char *nm) {
    int i;
    i = f77_scope_base;
    while (i < f77_nsym) {
        if (strcmp(f77_sname[i], nm) == 0) return i;
        i = i + 1;
    }
    if (f77_nsym >= F77_MAX_SYM) { f77_error("too many symbols"); return 0; }
    if (f77_imp_none && !f77_decls_open) {
        /* A name first seen in executable code has no declaration. */
        static char msg[F77_MAX_NAME + 40];
        strcpy(msg, nm);
        strcat(msg, " is not declared (IMPLICIT NONE)");
        f77_error(msg);
    }
    strcpy(f77_sname[f77_nsym], nm);
    f77_sty[f77_nsym] = f77_implicit_ty(nm);
    f77_srank[f77_nsym] = 0;
    f77_sarg[f77_nsym] = 0;
    f77_shi[f77_nsym] = -1;
    f77_scom[f77_nsym] = 0;
    f77_ssaved[f77_nsym] = 0;
    f77_sgidx[f77_nsym] = -1;
    f77_sparam[f77_nsym] = 0;
    f77_styped[f77_nsym] = 0;
    f77_sstored[f77_nsym] = 0;
    { int d; d = 0; while (d < F77_MAX_RANK) { f77_sextsym[f77_nsym][d] = -1; d = d + 1; } }
    f77_frame = f77_frame + ty_size(f77_sty[f77_nsym]);
    f77_sval[f77_nsym] = hi_emit(HI_ALLOCA, f77_sty[f77_nsym], -1, -1,
                                 0 - f77_frame, NULL);
    hl_ainst[hl_nalloca] = f77_sval[f77_nsym];
    hl_aoff[hl_nalloca] = 0 - f77_frame;
    hl_nalloca = hl_nalloca + 1;
    f77_nsym = f77_nsym + 1;
    return f77_nsym - 1;
}

/* --- program units --------------------------------------------------- */

#define F77_UNIT_PROGRAM 0
#define F77_UNIT_SUBR    1
#define F77_UNIT_FUNC    2
#define F77_MAX_UNIT     256

static char f77_uname[F77_MAX_UNIT][F77_MAX_NAME];
static int  f77_ukind[F77_MAX_UNIT];
static int  f77_upos[F77_MAX_UNIT];    /* source offset of the header */
static int  f77_uline[F77_MAX_UNIT];
static int  f77_urty[F77_MAX_UNIT];    /* FUNCTION result type */
static int  f77_nunit;

/* Create a symbol whose storage is a by-reference dummy argument: its
 * value is the ADDRESS of the actual, delivered in an argument
 * register.  Deliberately NOT registered in hl_ainst -- it is not an
 * alloca, and listing it there would offer it to mem2reg, which would
 * then rewrite loads through it into the address itself. */
static int f77_sym_param(char *nm, int index) {
    int s;
    if (f77_nsym >= F77_MAX_SYM) { f77_error("too many symbols"); return 0; }
    s = f77_nsym;
    strcpy(f77_sname[s], nm);
    f77_sty[s] = f77_implicit_ty(nm);
    f77_srank[s] = 0;
    f77_sarg[s] = 1;
    f77_shi[s] = -1;
    f77_scom[s] = 0;
    f77_ssaved[s] = 0;
    f77_sgidx[s] = -1;
    f77_sparam[s] = 0;
    f77_styped[s] = 0;
    f77_sstored[s] = 0;
    { int d; d = 0; while (d < F77_MAX_RANK) { f77_sextsym[s][d] = -1; d = d + 1; } }
    f77_sval[s] = hi_emit(HI_PARAM, HL_ADDR_TY, -1, -1, index, NULL);
    f77_nsym = f77_nsym + 1;
    return s;
}

/* Total element count of a shaped symbol. */
static int f77_sym_nelem(int s) {
    int n;
    int k;
    n = 1;
    k = 0;
    while (k < f77_srank[s]) { n = n * f77_sext[s][k]; k = k + 1; }
    return n;
}

/* (Re)allocate a symbol's frame slot once its type and shape are known.
 * Declarations arrive after the implicit-typing allocation, so the slot
 * is simply re-issued at the end of the frame; the earlier bytes are
 * abandoned, which costs a few words in exchange for not needing a
 * second pass over the declarations. */
static void f77_realloc_sym(int s) {
    int bytes;
    f77_shi[s] = -1;
    if (f77_sarg[s]) {
        /* By-reference dummy: storage belongs to the caller.  Only the
         * type and shape are recorded here. */
        h_ty[f77_sval[s]] = HL_ADDR_TY;
        return;
    }
    bytes = ty_size(f77_sty[s]) * f77_sym_nelem(s);
    f77_frame = f77_frame + bytes;
    h_val[f77_sval[s]] = 0 - f77_frame;
    h_ty[f77_sval[s]] = f77_sty[s];
    hl_aoff[hl_nalloca - 1] = 0 - f77_frame;

    /* Scalar double: give the hi word its own promotable slot. */
    if (f77_split_on < 0) f77_split_on = getenv("F77_NO_SPLIT") ? 0 : 1;
    if (f77_split_on && f77_srank[s] == 0 && ty_size(f77_sty[s]) == 8) {
        h_ty[f77_sval[s]] = TY_INT;                 /* lo word only */
        f77_shi[s] = hi_emit(HI_ALLOCA, TY_INT, -1, -1,
                             0 - f77_frame + 4, NULL);
        if (hl_nalloca < HL_MAX_ALLOCA) {
            hl_ainst[hl_nalloca] = f77_shi[s];
            hl_aoff[hl_nalloca] = 0 - f77_frame + 4;
            hl_nalloca = hl_nalloca + 1;
        }
        f77_split_count = f77_split_count + 1;
    }
    {
        /* Keep the alloca registry entry pointing at this symbol's
         * instruction, wherever it landed. */
        int i;
        i = 0;
        while (i < hl_nalloca) {
            if (hl_ainst[i] == f77_sval[s]) hl_aoff[i] = 0 - f77_frame;
            i = i + 1;
        }
    }
}

/* --- statement labels ----------------------------------------------- */

#define F77_MAX_LABEL 512
static int f77_lnum[F77_MAX_LABEL];
static int f77_lblk[F77_MAX_LABEL];
static int f77_nlabel;

/* Block for a label, created on first mention so forward GOTOs work. */
static int f77_label_blk(int n) {
    int i;
    i = f77_label_base;
    while (i < f77_nlabel) {
        if (f77_lnum[i] == n) return f77_lblk[i];
        i = i + 1;
    }
    if (f77_nlabel >= F77_MAX_LABEL) { f77_error("too many labels"); return 0; }
    f77_lnum[f77_nlabel] = n;
    f77_lblk[f77_nlabel] = hir_new_block();
    f77_nlabel = f77_nlabel + 1;
    return f77_lblk[f77_nlabel - 1];
}

/* --- FORMAT statements ----------------------------------------------- */

/* A FORMAT may appear anywhere in the unit, including after the WRITE
 * that names it, so phase 1 collects them all before any code is
 * emitted.  The format text is interned verbatim (parens included) and
 * handed to the runtime, which interprets it -- see runtime/libf77.c
 * for why this cannot be expanded inline. */
#define F77_MAX_FORMAT 256
static int f77_flabel[F77_MAX_FORMAT];
static int f77_funit[F77_MAX_FORMAT];   /* owning program unit */
static int f77_fstr[F77_MAX_FORMAT];    /* string-pool index */
static int f77_nformat;
static int f77_cur_unit;               /* set while compiling a unit */

static int f77_find_format(int label) {
    int i;
    i = 0;
    while (i < f77_nformat) {
        if (f77_flabel[i] == label && f77_funit[i] == f77_cur_unit)
            return f77_fstr[i];
        i = i + 1;
    }
    return -1;
}

/* --- control stack (DO loops and block IFs) ------------------------- */

#define F77_CTL_DO 1
#define F77_CTL_IF 2
#define F77_MAX_CTL 64

static int ctl_kind[F77_MAX_CTL];
static int ctl_label[F77_MAX_CTL];   /* DO: terminal statement label */
static int ctl_var[F77_MAX_CTL];     /* DO: symbol index */
static int ctl_step[F77_MAX_CTL];    /* DO: HIR value of the step */
static int ctl_stephi[F77_MAX_CTL];  /* DO: hi word of a DOUBLE step */
static int ctl_trip[F77_MAX_CTL];    /* DO: alloca holding the trip count */
static int ctl_test[F77_MAX_CTL];    /* DO: test block */
static int ctl_body[F77_MAX_CTL];    /* DO: body head, the rotated back edge */

static int ctl_exit[F77_MAX_CTL];    /* DO/IF: block after the construct */
static int ctl_else[F77_MAX_CTL];    /* IF: pending else block */
static int ctl_n;

/* Floor for f77_close_do.  A spliced callee raises it so that a label
 * in the callee can never close a DO belonging to the caller: DGEFA has
 * a statement labelled 70 and is inlined inside main's `DO 70`, and
 * without this floor parsing the callee's label popped the CALLER's
 * loop off the control stack. */
static int f77_ctl_base;

static void f77_ctl_reset(void) { ctl_n = 0; f77_ctl_base = 0; }

/* --- expression lowering -------------------------------------------- */

static int ex_ty;     /* type of the value most recently produced */
static int ex_hi;     /* its hi word, when ex_ty is TY_DOUBLE */

static int f77_expr(void);
static int f77_power(void);
static int f77_find_func(char *nm);
static int f77_actual_addr(void);
static int f77_urty_of(int u);
static int f77_load_at(int addr, int ty);
static int f77_load_sym(int s);
static void f77_store_sym_val(int s, int v, int vty, int vhi);
static void f77_store_at(int addr, int ty, int v, int vty, int vhi);
static int f77_can_inline(int u);
static int f77_inline_unit_body(int u, int *addrs, int nargs);
static int f77_find_unit(char *nm);
static int f77_stmt_is_declaration(int cls);
static void f77_emit_copyin(void);
static void f77_emit_copyout(void);

/* DOUBLE PRECISION on SLOW-32 is a PAIR of 32-bit values: the lo word is
 * the expression's value and the hi word travels beside it.  Operations
 * are emitted as calls to the __fp64_* helpers, every one of which the
 * backend recognises and replaces with inline hardware FP instructions
 * (hcg_fp64_kind / hcg_fp64_emit) -- so despite looking like libcalls
 * here, no call survives to the assembly.
 *
 * The hi word must be captured immediately after each subexpression:
 * the next emission overwrites the side channel. */

/* Is this lo/hi value pair the fp64 constant +2.0 (0x4000000000000000)? */
static int f77_fp64_is_two(int lo, int hi) {
    return lo >= 0 && hi >= 0 &&
           h_kind[lo] == HI_ICONST && h_val[lo] == 0 &&
           h_kind[hi] == HI_ICONST && h_val[hi] == 1073741824;
}

static int f77_fp64_call2(char *name, int alo, int ahi, int blo, int bhi, int *rhi) {
    int cb;
    int r;
    /* 2.0*X and X*2.0 are exactly X+X in IEEE (scaling by a power of
     * two never rounds), and the rewrite deletes the constant: in a
     * loop that is two fewer remat instructions every iteration. */
    if (strcmp(name, "__fp64_mul") == 0) {
        if (f77_fp64_is_two(alo, ahi)) {
            alo = blo; ahi = bhi; name = "__fp64_add";
        } else if (f77_fp64_is_two(blo, bhi)) {
            blo = alo; bhi = ahi; name = "__fp64_add";
        }
    }
    cb = h_ncarg;
    h_carg[h_ncarg] = alo; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = ahi; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = blo; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = bhi; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, name);
    h_cbase[r] = cb;
    if (rhi) *rhi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
    return r;
}

/* Unary operation on a double PAIR: exactly two argument words. */
static int f77_fp64_pair1(char *name, int alo, int ahi, int *rhi) {
    int cb;
    int r;
    cb = h_ncarg;
    h_carg[h_ncarg] = alo; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = ahi; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, name);
    h_cbase[r] = cb;
    if (rhi) *rhi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
    return r;
}

static int f77_fp64_call1(char *name, int alo, int *rhi) {
    int cb;
    int r;
    cb = h_ncarg;
    h_carg[h_ncarg] = alo; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, name);
    h_cbase[r] = cb;
    if (rhi) *rhi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
    return r;
}

/* --- libm calls by their C names ---------------------------------------
 *
 * The transcendentals are REAL calls, and the name IS the contract:
 * slow32 and slow32-fast execute the Newton-series SLOW-32 code that
 * libs32.s32a links in, while slow32-dbt and qemu-tcg find these names
 * in the .s32x symbol table and substitute the host's native routines
 * at translation time.  A wrapper name would starve that machinery, so
 * these emit `sin`, `atan2`, `pow`, ... verbatim -- and, unlike the
 * __fp64_* pseudo-calls the backend inlines, they carry real ABI tags
 * (aligned pairs for doubles). */

static int f77_libm_d1(char *name, int a, int ahi, int *rhi) {
    int cb;
    int r;
    cb = h_ncarg;
    h_carg[h_ncarg] = a;   h_carg_tag[h_ncarg] = 1; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = ahi; h_carg_tag[h_ncarg] = 2; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, name);
    h_cbase[r] = cb;
    *rhi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
    return r;
}

static int f77_libm_d2(char *name, int a, int ahi, int b, int bhi, int *rhi) {
    int cb;
    int r;
    cb = h_ncarg;
    h_carg[h_ncarg] = a;   h_carg_tag[h_ncarg] = 1; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = ahi; h_carg_tag[h_ncarg] = 2; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = b;   h_carg_tag[h_ncarg] = 1; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = bhi; h_carg_tag[h_ncarg] = 2; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, name);
    h_cbase[r] = cb;
    *rhi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
    return r;
}

static int f77_libm_f1(char *name, int a) {
    int cb;
    int r;
    cb = h_ncarg;
    h_carg[h_ncarg] = a; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_FLOAT, -1, -1, 1, name);
    h_cbase[r] = cb;
    return r;
}

static int f77_libm_f2(char *name, int a, int b) {
    int cb;
    int r;
    cb = h_ncarg;
    h_carg[h_ncarg] = a; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = b; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_FLOAT, -1, -1, 2, name);
    h_cbase[r] = cb;
    return r;
}

/* A DOUBLE literal is just its two words as constants. */
static int f77_dconst(double d, int *hi) {
    int w[2];
    memcpy(w, &d, 8);
    *hi = hi_emit(HI_ICONST, TY_INT, -1, -1, w[1], NULL);
    return hi_emit(HI_ICONST, TY_INT, -1, -1, w[0], NULL);
}

/* Convert `v` (with hi word *hi when it is a double) from one type to
 * another, updating *hi in place. */
static int f77_cvt(int v, int *hi, int from, int to) {
    if (from == to) return v;
    if (to == TY_DOUBLE) {
        if (from == TY_INT)   return f77_fp64_call1("__fp64_cvt_itoD", v, hi);
        if (from == TY_FLOAT) return f77_fp64_call1("__fp64_cvt_ftoD", v, hi);
        return v;
    }
    if (from == TY_DOUBLE) {
        int cb;
        int r;
        cb = h_ncarg;
        h_carg[h_ncarg] = v;   h_ncarg = h_ncarg + 1;
        h_carg[h_ncarg] = *hi; h_ncarg = h_ncarg + 1;
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 2,
                    to == TY_INT ? "__fp64_cvt_DtoI" : "__fp64_cvt_DtoF");
        h_cbase[r] = cb;
        h_ty[r] = to;
        return r;
    }
    if (from == TY_INT && to == TY_FLOAT)
        return hi_emit(HI_FCVT_ItoF, TY_FLOAT, v, -1, 0, NULL);
    if (from == TY_FLOAT && to == TY_INT)
        return hi_emit(HI_FCVT_FtoI, TY_INT, v, -1, 0, NULL);
    return v;
}

/* Back-compatible single-word convert (no doubles involved). */
static int f77_convert(int v, int from, int to) {
    int hi;
    hi = -1;
    return f77_cvt(v, &hi, from, to);
}

/* F77 mixed mode: DOUBLE beats REAL beats INTEGER. */
static int f77_balance(int *a, int *ahi, int aty, int *b, int *bhi, int bty) {
    if (aty == bty) return aty;
    if (aty == TY_DOUBLE || bty == TY_DOUBLE) {
        *a = f77_cvt(*a, ahi, aty, TY_DOUBLE);
        *b = f77_cvt(*b, bhi, bty, TY_DOUBLE);
        return TY_DOUBLE;
    }
    if (aty == TY_FLOAT || bty == TY_FLOAT) {
        *a = f77_cvt(*a, ahi, aty, TY_FLOAT);
        *b = f77_cvt(*b, bhi, bty, TY_FLOAT);
        return TY_FLOAT;
    }
    return TY_INT;
}

/* Integer constants are cached PER BASIC BLOCK.  CSE hashes on operand
 * instruction indices, so two identical expressions built from two
 * separately-emitted ICONSTs of the same value never match -- which is
 * why `(i-1)*8` was being recomputed once per array reference instead
 * of shared.  The cache is per-block, not per-function, so a reused
 * constant always dominates its uses. */
#define F77_KCACHE 64
static int f77_kval[F77_KCACHE];
static int f77_kinst[F77_KCACHE];
static int f77_nk;
static int f77_kblk = -1;

static int f77_iconst(int v) {
    int i;
    if (hl_cur_blk != f77_kblk) { f77_kblk = hl_cur_blk; f77_nk = 0; }
    i = 0;
    while (i < f77_nk) {
        if (f77_kval[i] == v) return f77_kinst[i];
        i = i + 1;
    }
    i = hi_emit(HI_ICONST, TY_INT, -1, -1, v, NULL);
    if (f77_nk < F77_KCACHE) {
        f77_kval[f77_nk] = v;
        f77_kinst[f77_nk] = i;
        f77_nk = f77_nk + 1;
    }
    return i;
}

/* REAL constant: f32 bits carried in an ICONST typed TY_FLOAT, which is
 * how the C compiler represents them too. */
static int f77_rconst(double d) {
    float f;
    int bits;
    f = (float)d;
    memcpy(&bits, &f, 4);
    return hi_emit(HI_ICONST, TY_FLOAT, -1, -1, bits, NULL);
}

/* Address of A(s1,s2,...).  Fortran is COLUMN-MAJOR and 1-based by
 * default, so the element offset is
 *
 *     (s1-lo1) + (s2-lo2)*n1 + (s3-lo3)*n1*n2 + ...
 *
 * i.e. the FIRST subscript varies fastest -- the opposite of C.  Any
 * lower bound is allowed (A(0:9), A(-5:5)); it is folded into the
 * subtraction, and constant subscripts fold away entirely downstream. */
static int f77_subscript_addr(int s) {
    int off;
    int cstride;      /* constant part of the running stride */
    int vstride;      /* runtime stride value, or -1 while still constant */
    int k;
    int idx;
    int t;
    int byte;
    int e;
    int coff;         /* compile-time part: -sum(lo_k * stride_k) */
    int addr;

    /* A constant lower bound over a CONSTANT stride contributes a
     * compile-time byte offset, accumulated in coff and emitted as one
     * trailing ADDI on the final address -- which BURG folds into each
     * load/store's 12-bit displacement, so Fortran's 1-based (I-1)
     * subtraction costs nothing at run time.  A lower bound over a
     * RUNTIME stride (adjustable dimensions) keeps the subtraction on
     * the index, as before. */
    off = -1;
    coff = 0;
    cstride = 1;
    vstride = -1;
    k = 0;
    for (;;) {
        idx = f77_expr();
        idx = f77_cvt(idx, &ex_hi, ex_ty, TY_INT);
        if (k < f77_srank[s]) {
            if (f77_slo[s][k] != 0) {
                if (vstride >= 0)
                    idx = hi_emit(HI_ADDI, TY_INT, idx, -1, 0 - f77_slo[s][k], NULL);
                else
                    coff = coff - f77_slo[s][k] * cstride;
            }
            if (vstride >= 0)
                idx = hi_emit(HI_MUL, TY_INT, idx, vstride, 0, NULL);
            else if (cstride != 1)
                idx = hi_emit(HI_MUL, TY_INT, idx, f77_iconst(cstride), 0, NULL);
            off = (off < 0) ? idx : hi_emit(HI_ADD, TY_INT, off, idx, 0, NULL);

            /* Advance the stride by this dimension's extent.  Once any
             * extent is a run-time value the stride becomes one too;
             * the last dimension's extent is never needed, which is why
             * A(LDA,1) and A(LDA,*) both work. */
            if (f77_sextsym[s][k] >= 0) {
                e = f77_load_at(f77_sval[f77_sextsym[s][k]],
                                f77_sty[f77_sextsym[s][k]]);
                if (f77_sarg[f77_sextsym[s][k]])
                    f77_ldro_note(e, f77_sextsym[s][k]);
                if (vstride >= 0) {
                    vstride = hi_emit(HI_MUL, TY_INT, vstride, e, 0, NULL);
                } else if (cstride == 1) {
                    vstride = e;
                } else {
                    vstride = hi_emit(HI_MUL, TY_INT, e, f77_iconst(cstride), 0, NULL);
                }
            } else if (vstride >= 0) {
                vstride = hi_emit(HI_MUL, TY_INT, vstride,
                                  f77_iconst(f77_sext[s][k]), 0, NULL);
            } else {
                cstride = cstride * f77_sext[s][k];
            }
        }
        k = k + 1;
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) { f77_error("expected ) after subscripts"); }
    else f77_tok();
    if (k != f77_srank[s]) f77_error("wrong number of subscripts");

    if (off < 0) off = f77_iconst(0);
    t = ty_size(f77_sty[s]);
    if (t == 1) byte = off;
    else if (t == 4) byte = hi_emit(HI_SLL, TY_INT, off, f77_iconst(2), 0, NULL);
    else if (t == 8) byte = hi_emit(HI_SLL, TY_INT, off, f77_iconst(3), 0, NULL);
    else byte = hi_emit(HI_MUL, TY_INT, off, f77_iconst(t), 0, NULL);
    addr = hi_emit(HI_ADD, HL_ADDR_TY, f77_sval[s], byte, 0, NULL);
    if (coff != 0) {
        int cb;
        cb = coff * t;
        if (cb >= -2048 && cb <= 2047)
            addr = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, cb, NULL);
        else
            addr = hi_emit(HI_ADD, HL_ADDR_TY, addr, f77_iconst(cb), 0, NULL);
    }
    return addr;
}

/* Load a scalar symbol.  When it has a paired hi slot, both halves are
 * direct loads of promotable allocas -- no address arithmetic, so
 * mem2reg can keep the whole double in registers. */
static int f77_load_sym(int s) {
    int v;
    ex_ty = f77_sty[s];
    if (f77_shi[s] >= 0) {
        v = hi_emit(HI_LOAD, TY_INT, f77_sval[s], -1, 0, NULL);
        ex_hi = hi_emit(HI_LOAD, TY_INT, f77_shi[s], -1, 0, NULL);
        return v;
    }
    v = f77_load_at(f77_sval[s], f77_sty[s]);
    if (f77_sarg[s] && f77_srank[s] == 0) {
        f77_ldro_note(v, s);
        if (f77_sty[s] == TY_DOUBLE && ex_hi >= 0) f77_ldro_note(ex_hi, s);
    }
    return v;
}

static void f77_store_sym_val(int s, int v, int vty, int vhi) {
    f77_sstored[s] = 1;
    if (f77_shi[s] >= 0) {
        v = f77_cvt(v, &vhi, vty, f77_sty[s]);
        hi_emit(HI_STORE, TY_INT, f77_sval[s], v, 0, NULL);
        hi_emit(HI_STORE, TY_INT, f77_shi[s], vhi, 0, NULL);
        return;
    }
    f77_store_at(f77_sval[s], f77_sty[s], v, vty, vhi);
}

/* Load a value of type `ty` from `addr`, setting ex_ty/ex_hi. */
static int f77_load_at(int addr, int ty) {
    int v;
    ex_ty = ty;
    if (ty == TY_DOUBLE) {
        int a4;
        v = hi_emit(HI_LOAD, TY_INT, addr, -1, 0, NULL);
        a4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
        ex_hi = hi_emit(HI_LOAD, TY_INT, a4, -1, 0, NULL);
        return v;
    }
    return hi_emit(HI_LOAD, ty, addr, -1, 0, NULL);
}

/* Store `v` (hi word `vhi`) of type `vty` to `addr` as type `ty`. */
static void f77_store_at(int addr, int ty, int v, int vty, int vhi) {
    v = f77_cvt(v, &vhi, vty, ty);
    if (ty == TY_DOUBLE) {
        int a4;
        hi_emit(HI_STORE, TY_INT, addr, v, 0, NULL);
        a4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
        hi_emit(HI_STORE, TY_INT, a4, vhi, 0, NULL);
        return;
    }
    hi_emit(HI_STORE, ty, addr, v, 0, NULL);
}

/* --- intrinsic functions --------------------------------------------- */

/* Branchless select: cond ? a : b, via  b ^ ((a^b) & -cond).
 * SLOW-32 has no conditional move, and a branch inside an expression
 * would mean splitting the current block mid-expression, so the mask
 * form is both simpler here and better code. */
static int f77_select(int cond, int a, int b) {
    int m;
    int x;
    m = hi_emit(HI_NEG, TY_INT, cond, -1, 0, NULL);
    x = hi_emit(HI_XOR, TY_INT, a, b, 0, NULL);
    x = hi_emit(HI_AND, TY_INT, x, m, 0, NULL);
    return hi_emit(HI_XOR, TY_INT, b, x, 0, NULL);
}

#define IN_NONE   0
#define IN_ABS    1
#define IN_MAX    2
#define IN_MIN    3
#define IN_MOD    4
#define IN_INT    5
#define IN_REAL   6
#define IN_DBLE   7
#define IN_SQRT   8
#define IN_SIGN   9
#define IN_MAX1   10   /* real args, integer result */
#define IN_MIN1   11
#define IN_AMAX0  12   /* integer args, real result */
#define IN_AMIN0  13
/* Transcendentals: contiguous ids, range-tested in the lowering. */
#define IN_SIN    14
#define IN_COS    15
#define IN_TAN    16
#define IN_ASIN   17
#define IN_ACOS   18
#define IN_ATAN   19
#define IN_ATAN2  20
#define IN_EXP    21
#define IN_LOG    22
#define IN_LOG10  23
#define IN_SINH   24
#define IN_COSH   25
#define IN_TANH   26

/* F77 intrinsic names are type-decorated (ABS/IABS/DABS, MAX0/AMAX1/
 * DMAX1); the operation is the same and the operand types decide the
 * lowering, so the decorations all fold onto one id here. */
static int f77_intrinsic(char *nm) {
    if (strcmp(nm, "ABS") == 0 || strcmp(nm, "IABS") == 0 ||
        strcmp(nm, "DABS") == 0) return IN_ABS;
    if (strcmp(nm, "MAX") == 0 || strcmp(nm, "MAX0") == 0 ||
        strcmp(nm, "AMAX1") == 0 || strcmp(nm, "DMAX1") == 0) return IN_MAX;
    if (strcmp(nm, "MAX1") == 0) return IN_MAX1;
    if (strcmp(nm, "AMAX0") == 0) return IN_AMAX0;
    if (strcmp(nm, "MIN") == 0 || strcmp(nm, "MIN0") == 0 ||
        strcmp(nm, "AMIN1") == 0 || strcmp(nm, "DMIN1") == 0) return IN_MIN;
    if (strcmp(nm, "MIN1") == 0) return IN_MIN1;
    if (strcmp(nm, "AMIN0") == 0) return IN_AMIN0;
    if (strcmp(nm, "MOD") == 0 || strcmp(nm, "AMOD") == 0 ||
        strcmp(nm, "DMOD") == 0) return IN_MOD;
    if (strcmp(nm, "INT") == 0 || strcmp(nm, "IDINT") == 0 ||
        strcmp(nm, "IFIX") == 0) return IN_INT;
    if (strcmp(nm, "REAL") == 0 || strcmp(nm, "FLOAT") == 0 ||
        strcmp(nm, "SNGL") == 0) return IN_REAL;
    if (strcmp(nm, "DBLE") == 0 || strcmp(nm, "DFLOAT") == 0) return IN_DBLE;
    if (strcmp(nm, "SQRT") == 0 || strcmp(nm, "DSQRT") == 0) return IN_SQRT;
    if (strcmp(nm, "SIGN") == 0 || strcmp(nm, "ISIGN") == 0 ||
        strcmp(nm, "DSIGN") == 0) return IN_SIGN;
    if (strcmp(nm, "SIN") == 0 || strcmp(nm, "DSIN") == 0) return IN_SIN;
    if (strcmp(nm, "COS") == 0 || strcmp(nm, "DCOS") == 0) return IN_COS;
    if (strcmp(nm, "TAN") == 0 || strcmp(nm, "DTAN") == 0) return IN_TAN;
    if (strcmp(nm, "ASIN") == 0 || strcmp(nm, "DASIN") == 0) return IN_ASIN;
    if (strcmp(nm, "ACOS") == 0 || strcmp(nm, "DACOS") == 0) return IN_ACOS;
    if (strcmp(nm, "ATAN") == 0 || strcmp(nm, "DATAN") == 0) return IN_ATAN;
    if (strcmp(nm, "ATAN2") == 0 || strcmp(nm, "DATAN2") == 0) return IN_ATAN2;
    if (strcmp(nm, "EXP") == 0 || strcmp(nm, "DEXP") == 0) return IN_EXP;
    if (strcmp(nm, "LOG") == 0 || strcmp(nm, "ALOG") == 0 ||
        strcmp(nm, "DLOG") == 0) return IN_LOG;
    if (strcmp(nm, "LOG10") == 0 || strcmp(nm, "ALOG10") == 0 ||
        strcmp(nm, "DLOG10") == 0) return IN_LOG10;
    if (strcmp(nm, "SINH") == 0 || strcmp(nm, "DSINH") == 0) return IN_SINH;
    if (strcmp(nm, "COSH") == 0 || strcmp(nm, "DCOSH") == 0) return IN_COSH;
    if (strcmp(nm, "TANH") == 0 || strcmp(nm, "DTANH") == 0) return IN_TANH;
    return IN_NONE;
}

/* The libm name for a transcendental, by result width. */
static char *f77_tr_name(int in, int dbl) {
    if (in == IN_SIN)   return dbl ? "sin"   : "sinf";
    if (in == IN_COS)   return dbl ? "cos"   : "cosf";
    if (in == IN_TAN)   return dbl ? "tan"   : "tanf";
    if (in == IN_ASIN)  return dbl ? "asin"  : "asinf";
    if (in == IN_ACOS)  return dbl ? "acos"  : "acosf";
    if (in == IN_ATAN)  return dbl ? "atan"  : "atanf";
    if (in == IN_ATAN2) return dbl ? "atan2" : "atan2f";
    if (in == IN_EXP)   return dbl ? "exp"   : "expf";
    if (in == IN_LOG)   return dbl ? "log"   : "logf";
    if (in == IN_LOG10) return dbl ? "log10" : "log10f";
    if (in == IN_SINH)  return dbl ? "sinh"  : "sinhf";
    if (in == IN_COSH)  return dbl ? "cosh"  : "coshf";
    return dbl ? "tanh" : "tanhf";
}

/* |x|: for FP just clear the sign bit; for an integer use the standard
 * branchless (x ^ (x>>31)) - (x>>31). */
static int f77_abs(int v, int ty, int *hi) {
    int m;
    if (ty == TY_DOUBLE) {
        *hi = hi_emit(HI_AND, TY_INT, *hi, f77_iconst(0x7fffffff), 0, NULL);
        return v;
    }
    if (ty == TY_FLOAT)
        return hi_emit(HI_AND, TY_FLOAT, v, f77_iconst(0x7fffffff), 0, NULL);
    m = hi_emit(HI_SRA, TY_INT, v, f77_iconst(31), 0, NULL);
    v = hi_emit(HI_XOR, TY_INT, v, m, 0, NULL);
    return hi_emit(HI_SUB, TY_INT, v, m, 0, NULL);
}

/* max/min of two already-balanced operands. */
static int f77_minmax(int want_max, int a, int ahi, int b, int bhi,
                      int ty, int *rhi) {
    int c;
    if (ty == TY_DOUBLE) {
        c = want_max ? f77_fp64_call2("__fp64_lt", b, bhi, a, ahi, NULL)
                     : f77_fp64_call2("__fp64_lt", a, ahi, b, bhi, NULL);
        *rhi = f77_select(c, ahi, bhi);
        return f77_select(c, a, b);
    }
    if (ty == TY_FLOAT)
        c = want_max ? hi_emit(HI_FLT, TY_INT, b, a, 0, NULL)
                     : hi_emit(HI_FLT, TY_INT, a, b, 0, NULL);
    else
        c = want_max ? hi_emit(HI_SGT, TY_INT, a, b, 0, NULL)
                     : hi_emit(HI_SLT, TY_INT, a, b, 0, NULL);
    return f77_select(c, a, b);
}

static int f77_primary(void) {
    int v;
    int s;
    int save;

    if (lx_t == T_ICON) { v = f77_iconst(lex_ival); ex_ty = TY_INT; f77_tok(); return v; }
    if (lx_t == T_RCON) { v = f77_rconst(lex_dval); ex_ty = TY_FLOAT; f77_tok(); return v; }
    if (lx_t == T_DCON) {
        v = f77_dconst(lex_dval, &ex_hi); ex_ty = TY_DOUBLE; f77_tok(); return v;
    }
    if (lx_t == T_TRUE)  { v = f77_iconst(1); ex_ty = TY_INT; f77_tok(); return v; }
    if (lx_t == T_FALSE) { v = f77_iconst(0); ex_ty = TY_INT; f77_tok(); return v; }
    if (lx_t == T_LP) {
        f77_tok();
        v = f77_expr();
        if (lx_t != T_RP) f77_error("expected )");
        f77_tok();
        return v;
    }
    if (lx_t == T_MINUS) {
        int vhi;
        f77_tok();
        /* Negation binds LOOSER than ** in F77: -X**2 is -(X**2), so
         * the operand is a power, not a primary.  For every other
         * operator the difference is invisible ((-a)*b == -(a*b)),
         * which is how binding at primary level survived until **
         * landed. */
        v = f77_power();
        save = ex_ty;
        vhi = ex_hi;
        if (save == TY_DOUBLE) {
            ex_ty = TY_DOUBLE;
            return f77_fp64_pair1("__fp64_neg", v, vhi, &ex_hi);
        }
        if (save == TY_FLOAT) return hi_emit(HI_FNEG, TY_FLOAT, v, -1, 0, NULL);
        return hi_emit(HI_NEG, TY_INT, v, -1, 0, NULL);
    }
    if (lx_t == T_PLUS) { f77_tok(); return f77_power(); }
    if (lx_t == T_NAME) {
        char nm[F77_MAX_NAME];
        int u;
        int in;
        strcpy(nm, lex_name);
        u = f77_find_func(nm);

        /* Intrinsics, unless a unit in this file defines the name --
         * a user FUNCTION shadows the intrinsic of the same name. */
        in = (u < 0) ? f77_intrinsic(nm) : IN_NONE;
        if (in != IN_NONE) {
            int a; int ahi; int aty;
            int b; int bhi; int bty;
            int rty;
            int qzero_hi;
            qzero_hi = -1;
            f77_tok();
            if (lx_t != T_LP) { f77_error("intrinsic needs arguments"); return f77_iconst(0); }
            f77_tok();
            a = f77_expr(); aty = ex_ty; ahi = ex_hi;

            if (in == IN_ABS) {
                a = f77_abs(a, aty, &ahi);
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                ex_ty = aty; ex_hi = ahi;
                return a;
            }
            if (in == IN_INT || in == IN_REAL || in == IN_DBLE) {
                rty = (in == IN_INT) ? TY_INT : (in == IN_REAL ? TY_FLOAT : TY_DOUBLE);
                a = f77_cvt(a, &ahi, aty, rty);
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                ex_ty = rty; ex_hi = ahi;
                return a;
            }
            if (in == IN_SQRT) {
                /* SLOW-32 has FSQRT, but the HI_FSQRT path is only wired
                 * for single-value doubles; the pair form goes through
                 * libm, which the DBT intercepts to a native call. */
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                if (aty == TY_DOUBLE) {
                    /* Inlined by the backend to a single FSQRT.D; the
                     * symbol never reaches the linker. */
                    ex_ty = TY_DOUBLE;
                    return f77_fp64_pair1("__fp64_sqrt", a, ahi, &ex_hi);
                }
                {
                    a = f77_cvt(a, &ahi, aty, TY_FLOAT);
                    ex_ty = TY_FLOAT;
                    return f77_fp64_call1("__fp32_sqrt", a, NULL);
                }
            }

            if (in >= IN_SIN && in <= IN_TANH) {
                /* A D-prefixed specific name (DSIN, DATAN2, ...) forces
                 * the double path even for a non-double argument; the
                 * generic names dispatch on the argument's type. */
                int dbl;
                dbl = (aty == TY_DOUBLE) || (nm[0] == 'D');
                if (in == IN_ATAN2) {
                    if (lx_t != T_COMMA) {
                        f77_error("ATAN2 needs two arguments");
                        return f77_iconst(0);
                    }
                    f77_tok();
                    b = f77_expr(); bty = ex_ty; bhi = ex_hi;
                    if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                    if (dbl || bty == TY_DOUBLE) {
                        a = f77_cvt(a, &ahi, aty, TY_DOUBLE);
                        b = f77_cvt(b, &bhi, bty, TY_DOUBLE);
                        ex_ty = TY_DOUBLE;
                        return f77_libm_d2("atan2", a, ahi, b, bhi, &ex_hi);
                    }
                    a = f77_cvt(a, &ahi, aty, TY_FLOAT);
                    b = f77_cvt(b, &bhi, bty, TY_FLOAT);
                    ex_ty = TY_FLOAT;
                    return f77_libm_f2("atan2f", a, b);
                }
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                if (dbl) {
                    a = f77_cvt(a, &ahi, aty, TY_DOUBLE);
                    ex_ty = TY_DOUBLE;
                    return f77_libm_d1(f77_tr_name(in, 1), a, ahi, &ex_hi);
                }
                a = f77_cvt(a, &ahi, aty, TY_FLOAT);
                ex_ty = TY_FLOAT;
                return f77_libm_f1(f77_tr_name(in, 0), a);
            }

            /* Binary and n-ary forms: MAX/MIN fold pairwise. */
            for (;;) {
                if (lx_t != T_COMMA) break;
                f77_tok();
                b = f77_expr(); bty = ex_ty; bhi = ex_hi;
                rty = f77_balance(&a, &ahi, aty, &b, &bhi, bty);
                if (in == IN_MAX || in == IN_MAX1 || in == IN_AMAX0 ||
                    in == IN_MIN || in == IN_MIN1 || in == IN_AMIN0) {
                    a = f77_minmax(in == IN_MAX || in == IN_MAX1 || in == IN_AMAX0,
                                   a, ahi, b, bhi, rty, &ahi);
                } else if (in == IN_MOD) {
                    if (rty == TY_INT) {
                        a = hi_emit(HI_REM, TY_INT, a, b, 0, NULL);
                    } else {
                        /* a - INT(a/b)*b, in the operands' own type */
                        int q; int qhi; int t;
                        qhi = -1;
                        if (rty == TY_DOUBLE) {
                            q = f77_fp64_call2("__fp64_div", a, ahi, b, bhi, &qhi);
                            t = f77_cvt(q, &qhi, TY_DOUBLE, TY_INT);
                            q = f77_cvt(t, &qhi, TY_INT, TY_DOUBLE);
                            q = f77_fp64_call2("__fp64_mul", q, qhi, b, bhi, &qhi);
                            a = f77_fp64_call2("__fp64_sub", a, ahi, q, qhi, &ahi);
                        } else {
                            q = hi_emit(HI_FDIV, TY_FLOAT, a, b, 0, NULL);
                            t = hi_emit(HI_FCVT_FtoI, TY_INT, q, -1, 0, NULL);
                            q = hi_emit(HI_FCVT_ItoF, TY_FLOAT, t, -1, 0, NULL);
                            q = hi_emit(HI_FMUL, TY_FLOAT, q, b, 0, NULL);
                            a = hi_emit(HI_FSUB, TY_FLOAT, a, q, 0, NULL);
                        }
                    }
                } else if (in == IN_SIGN) {
                    /* |a| with the sign of b */
                    int neg;
                    a = f77_abs(a, rty, &ahi);
                    if (rty == TY_DOUBLE) {
                        neg = f77_fp64_call2("__fp64_lt", b, bhi,
                                             f77_dconst(0.0, &qzero_hi), qzero_hi, NULL);
                        ahi = f77_select(neg,
                                hi_emit(HI_OR, TY_INT, ahi, f77_iconst(0x80000000), 0, NULL),
                                ahi);
                    } else if (rty == TY_FLOAT) {
                        neg = hi_emit(HI_FLT, TY_INT, b, f77_rconst(0.0), 0, NULL);
                        a = f77_select(neg,
                                hi_emit(HI_OR, TY_INT, a, f77_iconst(0x80000000), 0, NULL), a);
                    } else {
                        neg = hi_emit(HI_SLT, TY_INT, b, f77_iconst(0), 0, NULL);
                        a = f77_select(neg, hi_emit(HI_NEG, TY_INT, a, -1, 0, NULL), a);
                    }
                }
                aty = rty;
            }
            if (lx_t == T_RP) f77_tok(); else f77_error("expected ) after intrinsic");
            if (in == IN_MAX1 || in == IN_MIN1) {
                a = f77_cvt(a, &ahi, aty, TY_INT);
                aty = TY_INT;
            } else if (in == IN_AMAX0 || in == IN_AMIN0) {
                a = f77_cvt(a, &ahi, aty, TY_FLOAT);
                aty = TY_FLOAT;
            }
            ex_ty = aty; ex_hi = ahi;
            return a;
        }

        if (u >= 0 && u != f77_cur_unit) {
            /* FUNCTION reference: by-reference actuals, result in the
             * usual return register (plus its hi word for a double). */
            int cb;
            int nargs;
            int r;
            int rty;
            int addrs[64];
            int ai;
            f77_tok();
            nargs = 0;
            if (lx_t == T_LP) {
                f77_tok();
                if (lx_t != T_RP) {
                    for (;;) {
                        /* Evaluate every actual BEFORE reserving any
                         * h_carg slots: an actual may itself contain a
                         * call, which would otherwise interleave its
                         * arguments with ours. */
                        if (nargs < 64) addrs[nargs] = f77_actual_addr();
                        else f77_actual_addr();
                        nargs = nargs + 1;
                        if (lx_t != T_COMMA) break;
                        f77_tok();
                    }
                }
                if (lx_t != T_RP) f77_error("expected ) after arguments");
                else f77_tok();
            }
            if (f77_can_inline(u)) {
                /* Splice the body, then read its result variable. */
                int rs;
                rs = f77_inline_unit_body(u, addrs, nargs);
                if (rs >= 0) return f77_load_sym(rs);
                ex_ty = TY_INT;
                return f77_iconst(0);
            }
            cb = h_ncarg;
            ai = 0;
            while (ai < nargs) {
                h_carg[h_ncarg] = addrs[ai];
                h_carg_tag[h_ncarg] = 0;
                h_ncarg = h_ncarg + 1;
                ai = ai + 1;
            }
            rty = f77_urty_of(u);
            r = hi_emit(HI_CALL, rty == TY_DOUBLE ? TY_INT : rty, -1, -1,
                        nargs, f77_uname[u]);
            h_cbase[r] = cb;
            ex_ty = rty;
            if (rty == TY_DOUBLE)
                ex_hi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
            return r;
        }
        s = f77_sym(nm);
        f77_tok();
        if (f77_sparam[s]) {            /* named constant: fold in place */
            ex_ty = f77_sty[s];
            if (ex_ty == TY_DOUBLE) return f77_dconst(f77_spdval[s], &ex_hi);
            if (ex_ty == TY_FLOAT) return f77_rconst(f77_spdval[s]);
            return f77_iconst(f77_spival[s]);
        }
        if (lx_t == T_LP && f77_srank[s] > 0) {
            int addr;
            f77_tok();
            addr = f77_subscript_addr(s);
            return f77_load_at(addr, f77_sty[s]);
        }
        if (lx_t == T_LP) {
            /* Not an array and not a FUNCTION in this file: an
             * external function reference. Leaving the ( unconsumed
             * used to drop the rest of the expression (GitHub #20). */
            int cb;
            int nargs;
            int r;
            int addrs[64];
            int ai;
            int rty;
            nargs = 0;
            f77_tok();
            if (lx_t != T_RP) {
                for (;;) {
                    if (nargs < 64) addrs[nargs] = f77_actual_addr();
                    else f77_actual_addr();
                    nargs = nargs + 1;
                    if (lx_t != T_COMMA) break;
                    f77_tok();
                }
            }
            if (lx_t != T_RP) f77_error("expected ) after arguments");
            else f77_tok();
            cb = h_ncarg;
            ai = 0;
            while (ai < nargs) {
                h_carg[h_ncarg] = addrs[ai];
                h_carg_tag[h_ncarg] = 0;
                h_ncarg = h_ncarg + 1;
                ai = ai + 1;
            }
            rty = f77_implicit_ty(nm);
            /* nm is this frame's buffer and hi_emit keeps the pointer:
             * the callee's name must outlive the parse (GitHub #20 --
             * the call came out as "EN", whatever the buffer held by
             * codegen time). */
            r = hi_emit(HI_CALL, rty == TY_DOUBLE ? TY_INT : rty, -1, -1,
                        nargs, strdup(nm));
            h_cbase[r] = cb;
            ex_ty = rty;
            ex_hi = -1;
            if (rty == TY_DOUBLE) ex_hi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
            return r;
        }
        return f77_load_sym(s);
    }
    f77_error("bad expression");
    ex_ty = TY_INT;
    return f77_iconst(0);
}

/* ** is right-associative in F77.  An INTEGER exponent goes to the
 * runtime's binary-exponentiation helpers (f77_ipow / f77_rpow_i /
 * f77_dpow_i -- real calls, unlike the __fp64_* names the backend
 * inlines, so the arguments carry real ABI tags).  A real exponent
 * is pow / powf, by the libm names the DBT intercepts. */
static int f77_power(void) {
    int a;
    int b;
    int aty;
    int bty;
    int ahi;
    int cb;
    int r;
    a = f77_primary();
    aty = ex_ty;
    ahi = ex_hi;
    if (lx_t == T_POWER) {
        int bhi;
        f77_tok();
        b = f77_power();
        bty = ex_ty;
        bhi = ex_hi;
        if (bty != TY_INT) {
            /* Real exponent: pow / powf, by the libm names the DBT and
             * qemu intercept.  An INTEGER base promotes to the
             * exponent's type (F77: the result is real). */
            if (aty == TY_DOUBLE || bty == TY_DOUBLE) {
                a = f77_cvt(a, &ahi, aty, TY_DOUBLE);
                b = f77_cvt(b, &bhi, bty, TY_DOUBLE);
                ex_ty = TY_DOUBLE;
                return f77_libm_d2("pow", a, ahi, b, bhi, &ex_hi);
            }
            a = f77_cvt(a, &ahi, aty, TY_FLOAT);
            b = f77_cvt(b, &bhi, bty, TY_FLOAT);
            ex_ty = TY_FLOAT;
            return f77_libm_f2("powf", a, b);
        }
        cb = h_ncarg;
        if (aty == TY_DOUBLE) {
            h_carg[h_ncarg] = a;   h_carg_tag[h_ncarg] = 1; h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = ahi; h_carg_tag[h_ncarg] = 2; h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = b;   h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
            r = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "f77_dpow_i");
            h_cbase[r] = cb;
            ex_hi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
            ex_ty = TY_DOUBLE;
            return r;
        }
        h_carg[h_ncarg] = a; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
        h_carg[h_ncarg] = b; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
        r = hi_emit(HI_CALL, aty == TY_FLOAT ? TY_FLOAT : TY_INT, -1, -1, 2,
                    aty == TY_FLOAT ? "f77_rpow_i" : "f77_ipow");
        h_cbase[r] = cb;
        ex_ty = aty;
        return r;
    }
    ex_ty = aty;
    ex_hi = ahi;
    return a;
}

static int f77_term(void) {
    int a; int b; int aty; int bty; int rty; int ahi; int bhi;
    a = f77_power();
    aty = ex_ty; ahi = ex_hi;
    while (lx_t == T_STAR || lx_t == T_SLASH) {
        int op;
        op = lx_t;
        f77_tok();
        b = f77_power();
        bty = ex_ty; bhi = ex_hi;
        rty = f77_balance(&a, &ahi, aty, &b, &bhi, bty);
        if (rty == TY_DOUBLE)
            a = f77_fp64_call2(op == T_STAR ? "__fp64_mul" : "__fp64_div",
                               a, ahi, b, bhi, &ahi);
        else if (rty == TY_FLOAT)
            a = hi_emit(op == T_STAR ? HI_FMUL : HI_FDIV, TY_FLOAT, a, b, 0, NULL);
        else
            a = hi_emit(op == T_STAR ? HI_MUL : HI_DIV, TY_INT, a, b, 0, NULL);
        aty = rty;
    }
    ex_ty = aty; ex_hi = ahi;
    return a;
}

static int f77_arith(void) {
    int a; int b; int aty; int bty; int rty; int ahi; int bhi;
    a = f77_term();
    aty = ex_ty; ahi = ex_hi;
    while (lx_t == T_PLUS || lx_t == T_MINUS) {
        int op;
        op = lx_t;
        f77_tok();
        b = f77_term();
        bty = ex_ty; bhi = ex_hi;
        rty = f77_balance(&a, &ahi, aty, &b, &bhi, bty);
        if (rty == TY_DOUBLE)
            a = f77_fp64_call2(op == T_PLUS ? "__fp64_add" : "__fp64_sub",
                               a, ahi, b, bhi, &ahi);
        else if (rty == TY_FLOAT)
            a = hi_emit(op == T_PLUS ? HI_FADD : HI_FSUB, TY_FLOAT, a, b, 0, NULL);
        else
            a = hi_emit(op == T_PLUS ? HI_ADD : HI_SUB, TY_INT, a, b, 0, NULL);
        aty = rty;
    }
    ex_ty = aty; ex_hi = ahi;
    return a;
}

/* Relationals produce an integer 0/1.  The FP comparison set is only
 * FEQ/FLT/FLE, so >, >= and /= are built by swapping operands or
 * inverting the result. */
static int f77_relat(void) {
    int a; int b; int aty; int bty; int rty; int op; int r; int ahi; int bhi;
    a = f77_arith();
    aty = ex_ty; ahi = ex_hi;
    if (lx_t == T_EQ || lx_t == T_NE || lx_t == T_LT ||
        lx_t == T_LE || lx_t == T_GT || lx_t == T_GE) {
        op = lx_t;
        f77_tok();
        b = f77_arith();
        bty = ex_ty; bhi = ex_hi;
        rty = f77_balance(&a, &ahi, aty, &b, &bhi, bty);
        ex_ty = TY_INT;
        if (rty == TY_DOUBLE) {
            /* Only eq/lt/le exist; the rest come from swapping the
             * operands or inverting the result. */
            if (op == T_EQ) return f77_fp64_call2("__fp64_eq", a, ahi, b, bhi, NULL);
            if (op == T_NE) {
                r = f77_fp64_call2("__fp64_eq", a, ahi, b, bhi, NULL);
                return hi_emit(HI_NOT, TY_INT, r, -1, 0, NULL);
            }
            if (op == T_LT) return f77_fp64_call2("__fp64_lt", a, ahi, b, bhi, NULL);
            if (op == T_LE) return f77_fp64_call2("__fp64_le", a, ahi, b, bhi, NULL);
            if (op == T_GT) return f77_fp64_call2("__fp64_lt", b, bhi, a, ahi, NULL);
            return f77_fp64_call2("__fp64_le", b, bhi, a, ahi, NULL);
        }
        if (rty == TY_FLOAT) {
            if (op == T_EQ) return hi_emit(HI_FEQ, TY_INT, a, b, 0, NULL);
            if (op == T_NE) {
                r = hi_emit(HI_FEQ, TY_INT, a, b, 0, NULL);
                return hi_emit(HI_NOT, TY_INT, r, -1, 0, NULL);
            }
            if (op == T_LT) return hi_emit(HI_FLT, TY_INT, a, b, 0, NULL);
            if (op == T_LE) return hi_emit(HI_FLE, TY_INT, a, b, 0, NULL);
            if (op == T_GT) return hi_emit(HI_FLT, TY_INT, b, a, 0, NULL);
            return hi_emit(HI_FLE, TY_INT, b, a, 0, NULL);
        }
        if (op == T_EQ) return hi_emit(HI_SEQ, TY_INT, a, b, 0, NULL);
        if (op == T_NE) return hi_emit(HI_SNE, TY_INT, a, b, 0, NULL);
        if (op == T_LT) return hi_emit(HI_SLT, TY_INT, a, b, 0, NULL);
        if (op == T_LE) return hi_emit(HI_SLE, TY_INT, a, b, 0, NULL);
        if (op == T_GT) return hi_emit(HI_SGT, TY_INT, a, b, 0, NULL);
        return hi_emit(HI_SGE, TY_INT, a, b, 0, NULL);
    }
    ex_ty = aty; ex_hi = ahi;
    return a;
}

static int f77_not(void) {
    int v;
    if (lx_t == T_NOT) {
        f77_tok();
        v = f77_not();
        ex_ty = TY_INT;
        return hi_emit(HI_NOT, TY_INT, v, -1, 0, NULL);
    }
    return f77_relat();
}

static int f77_and(void) {
    int a;
    int b;
    a = f77_not();
    while (lx_t == T_AND) {
        f77_tok();
        b = f77_not();
        a = hi_emit(HI_AND, TY_INT, a, b, 0, NULL);
        ex_ty = TY_INT;
    }
    return a;
}

static int f77_or(void) {
    int a;
    int b;
    a = f77_and();
    while (lx_t == T_OR) {
        f77_tok();
        b = f77_and();
        a = hi_emit(HI_OR, TY_INT, a, b, 0, NULL);
        ex_ty = TY_INT;
    }
    return a;
}

static int f77_expr(void) {
    int a;
    int b;
    a = f77_or();
    while (lx_t == T_EQV || lx_t == T_NEQV) {
        int op;
        op = lx_t;
        f77_tok();
        b = f77_or();
        a = hi_emit(HI_XOR, TY_INT, a, b, 0, NULL);
        if (op == T_EQV) a = hi_emit(HI_NOT, TY_INT, a, -1, 0, NULL);
        ex_ty = TY_INT;
    }
    return a;
}

/* --- statement classification --------------------------------------- */

#define S_ASSIGN   1
#define S_IF_BLOCK 2
#define S_IF_LOGIC 3
#define S_DO       4
#define S_KEYWORD  5

/* Does the statement text start with `kw`?  Returns the length matched,
 * or 0.  Blanks are already squeezed, so this is a plain prefix test. */
static int f77_starts(char *kw) {
    int n;
    n = (int)strlen(kw);
    if (lx_stmt_len < n) return 0;
    if (strncmp(lx_stmt, kw, n) != 0) return 0;
    return n;
}

/* The unit terminator is the statement END, exactly. A prefix match
 * swallows ENDIF/ENDFILE and was the slice2 vacuous-pass bug; the
 * inliner still had the prefix form. */
static int f77_is_unit_end(void) {
    return f77_starts("END") && lx_stmt_len == 3;
}

/* Offset of the first `c` at paren depth zero, or -1.  Character
 * constants are skipped so a quoted '=' or ',' cannot fool the
 * classifier. */
static int f77_toplevel(int c, int from) {
    int i;
    int depth;
    i = from;
    depth = 0;
    while (i < lx_stmt_len) {
        if (lx_stmt[i] == '\'') {
            i = i + 1;
            while (i < lx_stmt_len && lx_stmt[i] != '\'') i = i + 1;
            i = i + 1;
            continue;
        }
        if (lx_stmt[i] == '(') depth = depth + 1;
        else if (lx_stmt[i] == ')') depth = depth - 1;
        else if (depth == 0 && lx_stmt[i] == c) return i;
        i = i + 1;
    }
    return -1;
}

/* Offset just past the parenthesised group starting at `open`. */
static int f77_match_paren(int open) {
    int i;
    int depth;
    i = open;
    depth = 0;
    while (i < lx_stmt_len) {
        if (lx_stmt[i] == '\'') {
            i = i + 1;
            while (i < lx_stmt_len && lx_stmt[i] != '\'') i = i + 1;
        } else if (lx_stmt[i] == '(') depth = depth + 1;
        else if (lx_stmt[i] == ')') {
            depth = depth - 1;
            if (depth == 0) return i + 1;
        }
        i = i + 1;
    }
    return lx_stmt_len;
}

static int f77_if_tail;   /* offset of what follows IF(...) */
static int f77_do_body;   /* offset of the DO control text */

static int f77_classify(void) {
    int eq;
    int after;

    if (f77_starts("IF(")) {
        after = f77_match_paren(2);
        f77_if_tail = after;
        if (after >= lx_stmt_len) return S_KEYWORD;      /* malformed */
        if (strncmp(lx_stmt + after, "THEN", 4) == 0 && after + 4 == lx_stmt_len)
            return S_IF_BLOCK;
        if (lx_stmt[after] == '=') return S_ASSIGN;      /* array element IF(..) */
        return S_IF_LOGIC;
    }

    if (f77_starts("DO")) {
        eq = f77_toplevel('=', 2);
        if (eq >= 0 && f77_toplevel(',', eq + 1) >= 0) {
            f77_do_body = 2;
            return S_DO;
        }
    }

    eq = f77_toplevel('=', 0);
    if (eq >= 0) return S_ASSIGN;
    return S_KEYWORD;
}

/* --- statements ------------------------------------------------------ */

static int f77_stop_code;    /* value for the trailing RET */
static int f77_cur_blk_live; /* 0 once the current block has terminated */

/* Re-point the token scanner at an offset inside the statement. */
static void f77_scan_from(int off) {
    lx_rp = lx_stmt + off;
    lx_rpe = lx_stmt + lx_stmt_len;
    lx_rcs = f77_lexer_start;
    lx_ract = 0;
    lx_rts = 0;
    lx_rte = 0;
    f77_tok();
}

static void f77_store_sym(int s, int v, int vty, int vhi) {
    f77_sstored[s] = 1;
    v = f77_cvt(v, &vhi, vty, f77_sty[s]);
    if (f77_sty[s] == TY_DOUBLE) {
        int addr4;
        hi_emit(HI_STORE, TY_INT, f77_sval[s], v, 0, NULL);
        addr4 = hi_emit(HI_ADDI, HL_ADDR_TY, f77_sval[s], -1, 4, NULL);
        hi_emit(HI_STORE, TY_INT, addr4, vhi, 0, NULL);
        return;
    }
    hi_emit(HI_STORE, f77_sty[s], f77_sval[s], v, 0, NULL);
}

/* Start a new block and make it current. */
static void f77_begin_blk(int b) {
    hl_switch_block(b);
    f77_cur_blk_live = 1;
    f77_nk = 0;
    f77_kblk = b;
}

static void f77_goto_blk(int b) {
    if (f77_cur_blk_live) hi_emit(HI_BR, TY_VOID, -1, -1, b, NULL);
    f77_cur_blk_live = 0;
}

static void f77_stmt_assign(void) {
    int s;
    int v;
    int vty;
    int addr;
    f77_scan_from(0);
    if (lx_t != T_NAME) { f77_error("expected variable on the left of ="); return; }
    s = f77_sym(lex_name);
    if (f77_sparam[s]) { f77_error("cannot assign to a PARAMETER"); return; }
    f77_tok();
    addr = f77_sval[s];
    if (lx_t == T_LP && f77_srank[s] > 0) {
        f77_tok();
        addr = f77_subscript_addr(s);
    }
    if (lx_t != T_ASSIGN) { f77_error("expected ="); return; }
    f77_tok();
    v = f77_expr();
    vty = ex_ty;
    if (addr == f77_sval[s] && f77_srank[s] == 0)
        f77_store_sym_val(s, v, vty, ex_hi);
    else
        f77_store_at(addr, f77_sty[s], v, vty, ex_hi);
}

/* A dimension bound: a signed integer constant.  F77 allows constant
 * expressions here; only literals and negation are accepted so far. */
static int f77_dim_bound(void) {
    int neg;
    int v;
    neg = 0;
    if (lx_t == T_MINUS) { neg = 1; f77_tok(); }
    else if (lx_t == T_PLUS) f77_tok();
    if (lx_t != T_ICON) { f77_error("dimension bound must be an integer constant"); return 1; }
    v = lex_ival;
    f77_tok();
    return neg ? 0 - v : v;
}

/* --- constant expressions (PARAMETER, DATA, dimension bounds) --------
 *
 * Evaluated at compile time on the host -- f77 is a cross-compiler in
 * the ordinary universe, so host doubles are the sanctioned tool.  The
 * value is carried as (type, ival, dval): integer arithmetic stays
 * exact in ival, anything real is done in dval, and mixed operands
 * promote the usual way. */
static int    f77_cx_ty;
static int    f77_cx_i;
static double f77_cx_d;

static void f77_cexpr(void);

static void f77_cprimary(void) {
    if (lx_t == T_ICON) { f77_cx_ty = TY_INT; f77_cx_i = lex_ival; f77_cx_d = (double)lex_ival; f77_tok(); return; }
    if (lx_t == T_RCON) { f77_cx_ty = TY_FLOAT; f77_cx_d = lex_dval; f77_cx_i = (int)lex_dval; f77_tok(); return; }
    if (lx_t == T_DCON) { f77_cx_ty = TY_DOUBLE; f77_cx_d = lex_dval; f77_cx_i = (int)lex_dval; f77_tok(); return; }
    if (lx_t == T_TRUE)  { f77_cx_ty = TY_INT; f77_cx_i = 1; f77_cx_d = 1.0; f77_tok(); return; }
    if (lx_t == T_FALSE) { f77_cx_ty = TY_INT; f77_cx_i = 0; f77_cx_d = 0.0; f77_tok(); return; }
    if (lx_t == T_LP) {
        f77_tok();
        f77_cexpr();
        if (lx_t != T_RP) { f77_error("expected ) in constant expression"); return; }
        f77_tok();
        return;
    }
    if (lx_t == T_MINUS) {
        f77_tok();
        f77_cprimary();
        f77_cx_i = 0 - f77_cx_i;
        f77_cx_d = 0.0 - f77_cx_d;
        return;
    }
    if (lx_t == T_PLUS) { f77_tok(); f77_cprimary(); return; }
    if (lx_t == T_NAME) {
        int s;
        s = f77_sym(lex_name);
        if (!f77_sparam[s]) {
            f77_error("only PARAMETER constants may appear in a constant expression");
            f77_cx_ty = TY_INT; f77_cx_i = 1; f77_cx_d = 1.0;
            f77_tok();
            return;
        }
        f77_cx_ty = f77_sty[s];
        f77_cx_i = f77_spival[s];
        f77_cx_d = f77_spdval[s];
        f77_tok();
        return;
    }
    f77_error("bad constant expression");
    f77_cx_ty = TY_INT; f77_cx_i = 1; f77_cx_d = 1.0;
}

static int f77_cx_promote(int a, int b) {
    if (a == TY_DOUBLE || b == TY_DOUBLE) return TY_DOUBLE;
    if (a == TY_FLOAT || b == TY_FLOAT) return TY_FLOAT;
    return TY_INT;
}

static void f77_cterm(void) {
    int ty; int iv; double dv; int mul;
    f77_cprimary();
    for (;;) {
        if (lx_t == T_STAR) mul = 1;
        else if (lx_t == T_SLASH) mul = 0;
        else return;
        ty = f77_cx_ty; iv = f77_cx_i; dv = f77_cx_d;
        f77_tok();
        f77_cprimary();
        f77_cx_ty = f77_cx_promote(ty, f77_cx_ty);
        if (mul) {
            f77_cx_i = iv * f77_cx_i;
            f77_cx_d = dv * f77_cx_d;
        } else if (f77_cx_ty == TY_INT) {
            if (f77_cx_i == 0) { f77_error("division by zero in constant expression"); f77_cx_i = 1; }
            f77_cx_i = iv / f77_cx_i;
        } else {
            if (f77_cx_d == 0.0) { f77_error("division by zero in constant expression"); f77_cx_d = 1.0; }
            f77_cx_d = dv / f77_cx_d;
        }
        if (f77_cx_ty == TY_INT) f77_cx_d = (double)f77_cx_i;
        else f77_cx_i = (int)f77_cx_d;
    }
}

static void f77_cexpr(void) {
    int ty; int iv; double dv; int add;
    f77_cterm();
    for (;;) {
        if (lx_t == T_PLUS) add = 1;
        else if (lx_t == T_MINUS) add = 0;
        else return;
        ty = f77_cx_ty; iv = f77_cx_i; dv = f77_cx_d;
        f77_tok();
        f77_cterm();
        f77_cx_ty = f77_cx_promote(ty, f77_cx_ty);
        if (add) { f77_cx_i = iv + f77_cx_i; f77_cx_d = dv + f77_cx_d; }
        else     { f77_cx_i = iv - f77_cx_i; f77_cx_d = dv - f77_cx_d; }
        if (f77_cx_ty == TY_INT) f77_cx_d = (double)f77_cx_i;
        else f77_cx_i = (int)f77_cx_d;
    }
}

/* One dimension bound: `*` (assumed size), a non-PARAMETER name (an
 * adjustable dimension), or a constant expression -- which is where a
 * PARAMETER bound like A(N) or A(2*N+1) resolves. */
static int f77_bound(int *sym) {
    *sym = -1;
    if (lx_t == T_STAR) { f77_tok(); return 1; }   /* assumed size */
    if (lx_t == T_NAME) {
        int s;
        s = f77_sym(lex_name);
        if (!f77_sparam[s]) {          /* adjustable dimension */
            *sym = s;
            f77_tok();
            return 1;
        }
    }
    f77_cexpr();
    if (f77_cx_ty != TY_INT) { f77_error("dimension bound must be an integer constant"); return 1; }
    return f77_cx_i;
}

/* Parse `(d1[,d2...])` after a name, each dimension `[lo:]hi`. */
static void f77_parse_dims(int s) {
    int rank;
    int lo;
    int hi;
    int losym;
    int hisym;
    rank = 0;
    f77_tok();                       /* past ( */
    for (;;) {
        lo = 1;
        losym = -1;
        hi = f77_bound(&hisym);
        if (lx_t == T_COLON) {
            f77_tok();
            lo = hi;
            losym = hisym;
            hi = f77_bound(&hisym);
        }
        if (rank < F77_MAX_RANK) {
            f77_slo[s][rank] = (losym >= 0) ? 1 : lo;
            f77_sextsym[s][rank] = hisym;
            if (hisym >= 0) {
                f77_sext[s][rank] = 1;      /* size unknown until run time */
            } else {
                f77_sext[s][rank] = hi - lo + 1;
                if (f77_sext[s][rank] < 0) f77_sext[s][rank] = 0;
            }
        }
        rank = rank + 1;
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) f77_error("expected ) after dimensions");
    else f77_tok();
    if (rank > F77_MAX_RANK) { f77_error("too many dimensions"); rank = F77_MAX_RANK; }
    f77_srank[s] = rank;
}

/* Type declarations, and DIMENSION (which sets shape without a type). */
static void f77_stmt_decl(int ty, int skip) {
    int s;
    f77_scan_from(skip);
    /* Type-level length: REAL*8 X. Per-name REAL X*8 is below. */
    if (lx_t == T_STAR) {
        f77_tok();
        if (lx_t == T_ICON) {
            if (lex_ival == 8 && ty == TY_FLOAT) ty = TY_DOUBLE;
            f77_tok();
        }
    }
    for (;;) {
        if (lx_t != T_NAME) break;
        s = f77_sym(lex_name);
        if (ty >= 0) {                  /* declaration overrides implicit typing */
            f77_sty[s] = ty;
            f77_styped[s] = 1;
        }
        f77_tok();
        if (lx_t == T_STAR) {           /* REAL*8 X -- length specifier */
            f77_tok();
            if (lx_t == T_ICON) {
                if (lex_ival == 8 && f77_sty[s] == TY_FLOAT) f77_sty[s] = TY_DOUBLE;
                f77_tok();
            }
        }
        if (lx_t == T_LP) f77_parse_dims(s);
        f77_realloc_sym(s);
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
}

/* --- COMMON and SAVE --------------------------------------------------
 *
 * COMMON blocks are a compiler problem, not a linker problem (see the
 * plan): each block is emitted as ONE named .bss object through the
 * ps_g* tables gen_data() walks, and every unit that declares the
 * block lays its own members over that same object.  The block table
 * is therefore global to the compilation; sizes take the max across
 * units.
 *
 * Layout cannot happen at the COMMON statement itself: F77 lets the
 * type and DIMENSION statements that decide each member's size come
 * before OR after it.  So the COMMON statement only records
 * membership, and f77_layout_static() -- run when the declaration part
 * closes, alongside the dummy copy-in -- assigns the offsets, rebinds
 * each member's f77_sval to GADDR(block)+offset, and drops any split
 * hi slot so doubles are stored contiguously.  The member's original
 * alloca is simply abandoned, the same trade f77_realloc_sym already
 * makes.  SAVE rides the same mechanism: a saved local becomes its own
 * one-symbol .bss object. */

#define F77_MAX_CBLK 64
#define F77_MAX_CMEM 256
#define F77_GPOOL_MAX 16384

/* Defined (tentatively) again further down, next to their own logic;
 * repeated here because COMMON/SAVE parsing needs them earlier. */
static int f77_decls_open;
static int f77_result_sym;

static char f77_cblk_name[F77_MAX_CBLK][F77_MAX_NAME + 8];  /* mangled */
static int  f77_cblk_gidx[F77_MAX_CBLK];   /* index into ps_g* */
static int  f77_ncblk;

/* Per-unit COMMON membership, in declaration order. */
static int f77_cm_sym[F77_MAX_CMEM];
static int f77_cm_blk[F77_MAX_CMEM];
static int f77_cm_n;

/* Per-unit SAVE state. */
static int f77_save_all;
static int f77_sv_sym[F77_MAX_CMEM];
static int f77_sv_n;

/* Names handed to HI_GADDR are kept by pointer, so they need storage
 * that outlives the statement buffer. */
static char f77_gpool[F77_GPOOL_MAX];
static int  f77_gpool_n;

static char *f77_gpool_name(char *a, char *b, char *c, char *d) {
    char *p;
    int need;
    need = (int)(strlen(a) + strlen(b) + strlen(c) + strlen(d)) + 1;
    if (f77_gpool_n + need > F77_GPOOL_MAX) {
        f77_error("too many static names");
        return f77_gpool;   /* garbage but safe */
    }
    p = f77_gpool + f77_gpool_n;
    strcpy(p, a);
    strcat(p, b);
    strcat(p, c);
    strcat(p, d);
    f77_gpool_n = f77_gpool_n + need;
    return p;
}

/* One zero-initialized .bss object in the tables gen_data() walks.
 * Every field is set explicitly: the parser owns these entries, and a
 * default-zero ps_ginit_start would read as an initializer list. */
static int f77_g_add(char *name, int size) {
    int i;
    i = ps_nglobals;
    if (i >= P_MAX_GLOBALS) { f77_error("too many globals"); return 0; }
    ps_gname[i] = name;
    ps_gtype[i] = TY_INT;
    ps_gsize[i] = size;
    ps_ginit[i] = 0;
    ps_ginit_hi[i] = 0;
    ps_gstr[i] = -1;
    ps_glocal[i] = 1;
    ps_gextern[i] = 0;
    ps_ginit_start[i] = -1;
    ps_ginit_count[i] = 0;
    ps_girel_start[i] = 0;
    ps_girel_count[i] = 0;
    ps_nglobals = i + 1;
    return i;
}

/* Find or create a COMMON block.  `nm` is the Fortran name; "" is
 * blank COMMON.  The mangled name is what the .bss label carries. */
static int f77_cblk_find(char *nm) {
    int i;
    char mangled[F77_MAX_NAME + 8];
    strcpy(mangled, "f77c_");
    strcat(mangled, nm);
    i = 0;
    while (i < f77_ncblk) {
        if (strcmp(f77_cblk_name[i], mangled) == 0) return i;
        i = i + 1;
    }
    if (f77_ncblk >= F77_MAX_CBLK) { f77_error("too many COMMON blocks"); return 0; }
    strcpy(f77_cblk_name[f77_ncblk], mangled);
    f77_cblk_gidx[f77_ncblk] = f77_g_add(f77_cblk_name[f77_ncblk], 0);
    f77_ncblk = f77_ncblk + 1;
    return f77_ncblk - 1;
}

/* COMMON [/name/] a, b /name2/ c ...  -- also blank COMMON via // or a
 * bare list.  Members may carry their dimensions here. */
static void f77_stmt_common(int skip) {
    int blk;
    int s;
    char bname[F77_MAX_NAME];

    if (!f77_decls_open) {
        f77_error("COMMON after the first executable statement");
        return;
    }
    f77_scan_from(skip);
    blk = -1;                           /* bare list = blank COMMON */
    for (;;) {
        if (lx_t == T_EOF) break;
        if (lx_t == T_CONCAT) {         /* // -- blank COMMON */
            blk = -1;
            f77_tok();
        } else if (lx_t == T_SLASH) {
            f77_tok();
            if (lx_t == T_SLASH) {      /* / / -- blank COMMON */
                blk = -1;
                f77_tok();
            } else if (lx_t == T_NAME) {
                strcpy(bname, lex_name);
                f77_tok();
                if (lx_t != T_SLASH) { f77_error("expected / after COMMON block name"); return; }
                f77_tok();
                blk = f77_cblk_find(bname);
            } else {
                f77_error("bad COMMON block name");
                return;
            }
        }
        if (lx_t != T_NAME) { f77_error("expected a variable in COMMON"); return; }
        if (blk < 0) blk = f77_cblk_find("");
        s = f77_sym(lex_name);
        if (f77_sarg[s]) { f77_error("a dummy argument cannot be in COMMON"); return; }
        if (f77_sparam[s]) { f77_error("a PARAMETER cannot be in COMMON"); return; }
        if (f77_scom[s]) { f77_error("variable appears in COMMON twice"); return; }
        f77_tok();
        if (lx_t == T_LP) {             /* dimensions declared here */
            f77_parse_dims(s);
            f77_realloc_sym(s);
        }
        f77_scom[s] = 1;
        if (f77_cm_n >= F77_MAX_CMEM) { f77_error("too many COMMON members"); return; }
        f77_cm_sym[f77_cm_n] = s;
        f77_cm_blk[f77_cm_n] = blk;
        f77_cm_n = f77_cm_n + 1;
        if (lx_t == T_COMMA) f77_tok();
    }
}

/* SAVE           -- every local becomes static
 * SAVE a, /b/, c -- listed variables (a named block is already static,
 *                   so /b/ is recognised and skipped) */
static void f77_stmt_save(int skip) {
    if (!f77_decls_open) {
        f77_error("SAVE after the first executable statement");
        return;
    }
    f77_scan_from(skip);
    if (lx_t == T_EOF) { f77_save_all = 1; return; }
    for (;;) {
        if (lx_t == T_SLASH) {          /* /block/ -- nothing to do */
            f77_tok();
            if (lx_t == T_NAME) f77_tok();
            if (lx_t != T_SLASH) { f77_error("expected / after SAVE block name"); return; }
            f77_tok();
        } else if (lx_t == T_NAME) {
            if (f77_sv_n >= F77_MAX_CMEM) { f77_error("too many SAVE names"); return; }
            f77_sv_sym[f77_sv_n] = f77_sym(lex_name);
            f77_sv_n = f77_sv_n + 1;
            f77_tok();
        } else {
            f77_error("bad SAVE list");
            return;
        }
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
}

/* IMPLICIT NONE, or IMPLICIT type (letter-ranges), ... -- rewrite the
 * per-unit letter map.  Symbols that already exist untyped (dummies,
 * created at bind time under the default rule) are retyped to the new
 * map, with one guarded exception: the FUNCTION result's type was
 * fixed in phase 1 for every caller to see, so an IMPLICIT that would
 * change it is refused rather than silently splitting the ABI. */
static void f77_stmt_implicit(int skip) {
    int ty;
    int s;
    int c;
    int lo;
    int hi;

    if (!f77_decls_open) {
        f77_error("IMPLICIT after the first executable statement");
        return;
    }
    f77_scan_from(skip);
    if (lx_t == T_NAME && strcmp(lex_name, "NONE") == 0) {
        f77_imp_none = 1;
        f77_tok();
        return;
    }
    for (;;) {
        if (lx_t != T_NAME) { f77_error("IMPLICIT needs a type"); return; }
        if (strcmp(lex_name, "INTEGER") == 0) ty = TY_INT;
        else if (strcmp(lex_name, "REAL") == 0) ty = TY_FLOAT;
        else if (strcmp(lex_name, "DOUBLEPRECISION") == 0) ty = TY_DOUBLE;
        else if (strcmp(lex_name, "LOGICAL") == 0) ty = TY_INT;
        else { f77_error("IMPLICIT type not supported"); return; }
        f77_tok();
        if (lx_t != T_LP) { f77_error("IMPLICIT needs (letter-ranges)"); return; }
        f77_tok();
        for (;;) {
            if (lx_t != T_NAME || lex_name[1] != 0) {
                f77_error("IMPLICIT range needs single letters");
                return;
            }
            lo = lex_name[0];
            hi = lo;
            f77_tok();
            if (lx_t == T_MINUS) {
                f77_tok();
                if (lx_t != T_NAME || lex_name[1] != 0) {
                    f77_error("IMPLICIT range needs single letters");
                    return;
                }
                hi = lex_name[0];
                f77_tok();
            }
            if (lo < 'A' || hi > 'Z' || lo > hi) {
                f77_error("bad IMPLICIT letter range");
                return;
            }
            for (c = lo; c <= hi; c++) f77_imp_map[c - 'A'] = ty;
            if (lx_t != T_COMMA) break;
            f77_tok();
        }
        if (lx_t != T_RP) { f77_error("expected ) in IMPLICIT"); return; }
        f77_tok();
        if (lx_t != T_COMMA) break;
        f77_tok();
    }

    /* Retype symbols the default rule already created. */
    for (s = f77_scope_base; s < f77_nsym; s++) {
        if (f77_styped[s] || f77_sparam[s]) continue;
        ty = f77_imp_map[f77_sname[s][0] - 'A'];
        if (ty == f77_sty[s]) continue;
        if (s == f77_result_sym) {
            f77_error("IMPLICIT would change the FUNCTION result type; declare it in the header");
            return;
        }
        f77_sty[s] = ty;
        if (!f77_sarg[s]) f77_realloc_sym(s);
    }
}

/* PARAMETER (name = cexpr, ...) -- named constants, no storage.  The
 * symbol's type (declared or implicit, fixed by this point per F77's
 * ordering rules) decides which domain the value is served from. */
static void f77_stmt_parameter(int skip) {
    int s;
    if (!f77_decls_open) {
        f77_error("PARAMETER after the first executable statement");
        return;
    }
    f77_scan_from(skip);
    if (lx_t != T_LP) { f77_error("PARAMETER needs a parenthesised list"); return; }
    f77_tok();
    for (;;) {
        if (lx_t != T_NAME) { f77_error("PARAMETER needs a name"); return; }
        s = f77_sym(lex_name);
        if (f77_sarg[s] || f77_scom[s] || f77_srank[s] > 0) {
            f77_error("this name cannot be a PARAMETER");
            return;
        }
        f77_tok();
        if (lx_t != T_ASSIGN) { f77_error("expected = in PARAMETER"); return; }
        f77_tok();
        f77_cexpr();
        f77_sparam[s] = 1;
        if (f77_sty[s] == TY_INT) {
            f77_spival[s] = (f77_cx_ty == TY_INT) ? f77_cx_i : (int)f77_cx_d;
            f77_spdval[s] = (double)f77_spival[s];
        } else {
            f77_spdval[s] = (f77_cx_ty == TY_INT) ? (double)f77_cx_i : f77_cx_d;
            if (f77_sty[s] == TY_FLOAT)
                f77_spdval[s] = (double)(float)f77_spdval[s];
            f77_spival[s] = (int)f77_spdval[s];
        }
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) f77_error("expected ) after PARAMETER list");
}

/* --- DATA -------------------------------------------------------------
 *
 * DATA gives a variable an initial value, which in F77 means STATIC
 * storage initialized at load -- so a DATA'd variable takes the SAVE
 * path, and its values are poured into the ps_ginit pool when the
 * declaration part closes and the object exists.  Parsing flattens
 * each nlist/clist pair immediately (F77 requires the shape to be
 * declared before the DATA statement, so element counts are known):
 * one (symbol, element, value) triple per initialized element. */
#define F77_MAX_DINIT 8192
static int    f77_di_sym[F77_MAX_DINIT];
static int    f77_di_elem[F77_MAX_DINIT];
static int    f77_di_ty[F77_MAX_DINIT];
static int    f77_di_i[F77_MAX_DINIT];
static double f77_di_d[F77_MAX_DINIT];
static int    f77_di_n;

/* One DATA constant: sign, literal, .TRUE./.FALSE., or a PARAMETER
 * name.  NOT f77_cexpr: `/` is the value-list delimiter here, and the
 * evaluator would read it as division. */
static void f77_data_const(void) {
    int neg;
    neg = 0;
    if (lx_t == T_MINUS) { neg = 1; f77_tok(); }
    else if (lx_t == T_PLUS) f77_tok();
    if (lx_t == T_ICON) { f77_cx_ty = TY_INT; f77_cx_i = lex_ival; f77_cx_d = (double)lex_ival; f77_tok(); }
    else if (lx_t == T_RCON) { f77_cx_ty = TY_FLOAT; f77_cx_d = lex_dval; f77_cx_i = (int)lex_dval; f77_tok(); }
    else if (lx_t == T_DCON) { f77_cx_ty = TY_DOUBLE; f77_cx_d = lex_dval; f77_cx_i = (int)lex_dval; f77_tok(); }
    else if (lx_t == T_TRUE)  { f77_cx_ty = TY_INT; f77_cx_i = 1; f77_cx_d = 1.0; f77_tok(); }
    else if (lx_t == T_FALSE) { f77_cx_ty = TY_INT; f77_cx_i = 0; f77_cx_d = 0.0; f77_tok(); }
    else if (lx_t == T_NAME) {
        int s;
        s = f77_sym(lex_name);
        if (!f77_sparam[s]) { f77_error("DATA value must be a constant"); f77_cx_ty = TY_INT; f77_cx_i = 0; f77_cx_d = 0.0; return; }
        f77_cx_ty = f77_sty[s];
        f77_cx_i = f77_spival[s];
        f77_cx_d = f77_spdval[s];
        f77_tok();
    } else {
        f77_error("DATA value must be a constant");
        f77_cx_ty = TY_INT; f77_cx_i = 0; f77_cx_d = 0.0;
        return;
    }
    if (neg) { f77_cx_i = 0 - f77_cx_i; f77_cx_d = 0.0 - f77_cx_d; }
}

static void f77_stmt_data(int skip) {
    int tsym[64];
    int tstart[64];
    int tcount[64];
    int ntgt;
    int s;
    int k;

    if (!f77_decls_open) {
        f77_error("DATA after the first executable statement");
        return;
    }
    f77_scan_from(skip);
    for (;;) {
        /* --- the variable list, up to the opening / --- */
        ntgt = 0;
        for (;;) {
            if (lx_t != T_NAME) {
                if (lx_t == T_LP) f77_error("implied-DO in DATA is not supported");
                else f77_error("expected a variable in DATA");
                return;
            }
            s = f77_sym(lex_name);
            if (f77_sarg[s]) { f77_error("a dummy argument cannot appear in DATA"); return; }
            if (f77_sparam[s]) { f77_error("a PARAMETER cannot appear in DATA"); return; }
            if (f77_scom[s]) {
                f77_error("DATA for a COMMON member needs BLOCK DATA, which is not supported");
                return;
            }
            f77_tok();
            if (ntgt >= 64) { f77_error("too many DATA items"); return; }
            tsym[ntgt] = s;
            if (lx_t == T_LP) {
                /* Constant subscripts -> one element. */
                int elem;
                int stride;
                int d;
                if (f77_srank[s] == 0) { f77_error("subscript on a scalar in DATA"); return; }
                elem = 0;
                stride = 1;
                d = 0;
                f77_tok();
                for (;;) {
                    for (k = 0; k < f77_srank[s]; k++) {
                        if (f77_sextsym[s][k] >= 0) { f77_error("adjustable array in DATA"); return; }
                    }
                    f77_cexpr();
                    if (f77_cx_ty != TY_INT) { f77_error("DATA subscript must be an integer constant"); return; }
                    if (d >= f77_srank[s]) { f77_error("too many DATA subscripts"); return; }
                    if (f77_cx_i < f77_slo[s][d] ||
                        f77_cx_i >= f77_slo[s][d] + f77_sext[s][d]) {
                        f77_error("DATA subscript out of bounds");
                        return;
                    }
                    elem = elem + (f77_cx_i - f77_slo[s][d]) * stride;
                    stride = stride * f77_sext[s][d];
                    d = d + 1;
                    if (lx_t != T_COMMA) break;
                    f77_tok();
                }
                if (lx_t != T_RP) { f77_error("expected ) after DATA subscripts"); return; }
                if (d != f77_srank[s]) { f77_error("wrong number of DATA subscripts"); return; }
                f77_tok();
                tstart[ntgt] = elem;
                tcount[ntgt] = 1;
            } else if (f77_srank[s] > 0) {
                for (k = 0; k < f77_srank[s]; k++) {
                    if (f77_sextsym[s][k] >= 0) { f77_error("adjustable array in DATA"); return; }
                }
                tstart[ntgt] = 0;
                tcount[ntgt] = f77_sym_nelem(s);
            } else {
                tstart[ntgt] = 0;
                tcount[ntgt] = 1;
            }
            f77_ssaved[s] = 1;          /* DATA implies static storage */
            ntgt = ntgt + 1;
            if (lx_t != T_COMMA) break;
            f77_tok();
        }
        if (lx_t != T_SLASH) { f77_error("expected / before DATA values"); return; }
        f77_tok();

        /* --- the value list, poured across the targets in order --- */
        {
            int ti;       /* target index */
            int te;       /* elements consumed within target ti */
            int rep;
            ti = 0;
            te = 0;
            for (;;) {
                if (lx_t == T_SLASH) break;
                f77_data_const();
                rep = 1;
                if (lx_t == T_STAR) {   /* r*c: what we parsed was the count */
                    if (f77_cx_ty != TY_INT || f77_cx_i <= 0) {
                        f77_error("DATA repeat count must be a positive integer");
                        return;
                    }
                    rep = f77_cx_i;
                    f77_tok();
                    f77_data_const();
                }
                while (rep > 0) {
                    while (ti < ntgt && te >= tcount[ti]) { ti = ti + 1; te = 0; }
                    if (ti >= ntgt) { f77_error("more DATA values than variables"); return; }
                    if (f77_di_n >= F77_MAX_DINIT) { f77_error("too many DATA initializers"); return; }
                    f77_di_sym[f77_di_n] = tsym[ti];
                    f77_di_elem[f77_di_n] = tstart[ti] + te;
                    f77_di_ty[f77_di_n] = f77_cx_ty;
                    f77_di_i[f77_di_n] = f77_cx_i;
                    f77_di_d[f77_di_n] = f77_cx_d;
                    f77_di_n = f77_di_n + 1;
                    te = te + 1;
                    rep = rep - 1;
                }
                if (lx_t == T_COMMA) { f77_tok(); continue; }
                break;
            }
            while (ti < ntgt && te >= tcount[ti]) { ti = ti + 1; te = 0; }
            if (ti < ntgt) { f77_error("fewer DATA values than variables"); return; }
        }
        if (lx_t != T_SLASH) { f77_error("expected / after DATA values"); return; }
        f77_tok();
        if (lx_t == T_COMMA) f77_tok();
        if (lx_t == T_EOF) break;
    }
}

/* Rebind symbol `s` to static storage at `name`+`off`.  The GADDR (and
 * ADDI) land in whatever block is current -- the declaration part, so
 * the entry block, which dominates every use. */
static void f77_bind_static(int s, char *name, int off) {
    int ga;
    ga = hi_emit(HI_GADDR, HL_ADDR_TY, -1, -1, 0, name);
    if (off != 0) ga = hi_emit(HI_ADDI, HL_ADDR_TY, ga, -1, off, NULL);
    f77_sval[s] = ga;
    f77_shi[s] = -1;    /* doubles live contiguously in static storage */
}

/* Run once per unit when the declaration part closes: lay out this
 * unit's COMMON members and convert SAVEd locals to statics. */
static void f77_layout_static(void) {
    int i;
    int k;
    int s;
    int blk;
    int sz;
    int boff[F77_MAX_CBLK];

    /* IMPLICIT NONE: every symbol created during the declaration part
     * (dummies included) must have been explicitly typed by now.
     * Names first seen in executable code are caught in f77_sym. */
    if (f77_imp_none) {
        for (s = f77_scope_base; s < f77_nsym; s++) {
            if (f77_styped[s] || f77_sparam[s]) continue;
            {
                static char msg[F77_MAX_NAME + 40];
                strcpy(msg, f77_sname[s]);
                strcat(msg, " is not declared (IMPLICIT NONE)");
                f77_error(msg);
            }
            return;
        }
    }

    for (i = 0; i < f77_ncblk; i++) boff[i] = 0;

    for (i = 0; i < f77_cm_n; i++) {
        s = f77_cm_sym[i];
        blk = f77_cm_blk[i];
        for (k = 0; k < f77_srank[s]; k++) {
            if (f77_sextsym[s][k] >= 0) {
                f77_error("adjustable dimensions cannot appear in COMMON");
                return;
            }
        }
        f77_bind_static(s, f77_cblk_name[blk], boff[blk]);
        sz = ty_size(f77_sty[s]) * f77_sym_nelem(s);
        boff[blk] = boff[blk] + sz;
    }
    for (i = 0; i < f77_ncblk; i++) {
        if (boff[i] > ps_gsize[f77_cblk_gidx[i]])
            ps_gsize[f77_cblk_gidx[i]] = boff[i];
    }

    if (f77_save_all) {
        for (s = f77_scope_base; s < f77_nsym; s++) f77_ssaved[s] = 1;
    }
    for (i = 0; i < f77_sv_n; i++) f77_ssaved[f77_sv_sym[i]] = 1;

    for (s = f77_scope_base; s < f77_nsym; s++) {
        if (!f77_ssaved[s]) continue;
        if (f77_sarg[s] || f77_scom[s] || s == f77_result_sym) continue;
        for (k = 0; k < f77_srank[s]; k++) {
            if (f77_sextsym[s][k] >= 0) {
                f77_error("adjustable dimensions cannot be SAVEd");
                return;
            }
        }
        {
            char *nm;
            nm = f77_gpool_name("f77s_", f77_uname[f77_cur_unit], "_",
                                f77_sname[s]);
            f77_sgidx[s] = f77_g_add(nm, ty_size(f77_sty[s]) * f77_sym_nelem(s));
            f77_bind_static(s, nm, 0);
        }
    }

    /* Pour DATA values into the init pool, now that every DATA'd
     * variable is a static object of known size.  A touched object
     * moves from .bss to .data whole: its image is reserved and
     * zeroed first, then each triple lands at element*size. */
    for (i = 0; i < f77_di_n; i++) {
        int g;
        int esz;
        int off;
        s = f77_di_sym[i];
        g = f77_sgidx[s];
        if (g < 0) continue;            /* parse already errored */
        if (ps_ginit_start[g] < 0) {
            if (ps_ginit_pool_len + ps_gsize[g] > PS_MAX_INIT_POOL) {
                f77_error("DATA initializer pool overflow");
                return;
            }
            ps_ginit_start[g] = ps_ginit_pool_len;
            ps_ginit_count[g] = ps_gsize[g];
            for (k = 0; k < ps_gsize[g]; k++)
                ps_ginit_pool[ps_ginit_pool_len + k] = 0;
            ps_ginit_pool_len = ps_ginit_pool_len + ps_gsize[g];
        }
        esz = ty_size(f77_sty[s]);
        off = ps_ginit_start[g] + f77_di_elem[i] * esz;
        if (f77_sty[s] == TY_DOUBLE) {
            union { double d; unsigned char b[8]; } u;
            u.d = (f77_di_ty[i] == TY_INT) ? (double)f77_di_i[i] : f77_di_d[i];
            for (k = 0; k < 8; k++) ps_ginit_pool[off + k] = u.b[k];
        } else if (f77_sty[s] == TY_FLOAT) {
            union { float f; unsigned char b[4]; } u;
            u.f = (f77_di_ty[i] == TY_INT) ? (float)f77_di_i[i] : (float)f77_di_d[i];
            for (k = 0; k < 4; k++) ps_ginit_pool[off + k] = u.b[k];
        } else {
            int v;
            v = (f77_di_ty[i] == TY_INT) ? f77_di_i[i] : (int)f77_di_d[i];
            for (k = 0; k < 4; k++) ps_ginit_pool[off + k] = (unsigned char)((v >> (k * 8)) & 255);
        }
    }
}

static void f77_open_do(void) {
    int lab;
    int s;
    int m1, m2, m3;
    int t1, t2, t3;
    int trip_alloca;
    int b_test, b_body, b_exit;
    int c;
    int step_hi;
    step_hi = 0;

    f77_scan_from(f77_do_body);
    if (lx_t != T_ICON) { f77_error("DO needs a terminal statement label"); return; }
    lab = lex_ival;
    f77_tok();
    if (lx_t == T_COMMA) f77_tok();          /* DO 20, I = ... is accepted */
    if (lx_t != T_NAME) { f77_error("DO needs a loop variable"); return; }
    s = f77_sym(lex_name);
    f77_tok();
    if (lx_t != T_ASSIGN) { f77_error("expected = in DO"); return; }
    f77_tok();
    {
        int vty;
        int m1hi;
        int m2hi;
        int m3hi;
        m3hi = 0;
        vty = f77_sty[s];
        m1 = f77_expr(); m1hi = ex_hi; m1 = f77_cvt(m1, &m1hi, ex_ty, vty);
        if (lx_t != T_COMMA) { f77_error("expected , in DO"); return; }
        f77_tok();
        m2 = f77_expr(); m2hi = ex_hi; m2 = f77_cvt(m2, &m2hi, ex_ty, vty);
        if (lx_t == T_COMMA) {
            f77_tok();
            m3 = f77_expr(); m3hi = ex_hi; m3 = f77_cvt(m3, &m3hi, ex_ty, vty);
        } else {
            m3 = (vty == TY_FLOAT) ? f77_rconst(1.0) :
                 (vty == TY_DOUBLE) ? f77_dconst(1.0, &m3hi) : f77_iconst(1);
        }

        /* var = m1 in the DO variable's type */
        f77_sstored[s] = 1;
        if (vty == TY_DOUBLE) {
            int addr4;
            hi_emit(HI_STORE, TY_INT, f77_sval[s], m1, 0, NULL);
            addr4 = hi_emit(HI_ADDI, HL_ADDR_TY, f77_sval[s], -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, addr4, m1hi, 0, NULL);
        } else {
            hi_emit(HI_STORE, vty, f77_sval[s], m1, 0, NULL);
        }

        /* Trip count MAX(0, INT((m2-m1+m3)/m3)), then counted as INT. */
        if (vty == TY_FLOAT) {
            t1 = hi_emit(HI_FSUB, TY_FLOAT, m2, m1, 0, NULL);
            t2 = hi_emit(HI_FADD, TY_FLOAT, t1, m3, 0, NULL);
            t3 = hi_emit(HI_FDIV, TY_FLOAT, t2, m3, 0, NULL);
            t3 = hi_emit(HI_FCVT_FtoI, TY_INT, t3, -1, 0, NULL);
        } else if (vty == TY_DOUBLE) {
            {
                int dhi;
                int thi;
                t1 = f77_fp64_call2("__fp64_sub", m2, m2hi, m1, m1hi, &dhi);
                t2 = f77_fp64_call2("__fp64_add", t1, dhi, m3, m3hi, &thi);
                t3 = f77_fp64_call2("__fp64_div", t2, thi, m3, m3hi, &dhi);
                t3 = f77_cvt(t3, &dhi, TY_DOUBLE, TY_INT);
            }
        } else {
            t1 = hi_emit(HI_SUB, TY_INT, m2, m1, 0, NULL);
            t2 = hi_emit(HI_ADD, TY_INT, t1, m3, 0, NULL);
            t3 = hi_emit(HI_DIV, TY_INT, t2, m3, 0, NULL);
        }
        step_hi = m3hi;
    }

    f77_frame = f77_frame + 4;
    trip_alloca = hi_emit(HI_ALLOCA, TY_INT, -1, -1, 0 - f77_frame, NULL);
    hl_ainst[hl_nalloca] = trip_alloca;
    hl_aoff[hl_nalloca] = 0 - f77_frame;
    hl_nalloca = hl_nalloca + 1;
    hi_emit(HI_STORE, TY_INT, trip_alloca, t3, 0, NULL);

    b_test = hir_new_block();
    b_body = hir_new_block();
    b_exit = hir_new_block();

    f77_goto_blk(b_test);
    f77_begin_blk(b_test);
    c = hi_emit(HI_LOAD, TY_INT, trip_alloca, -1, 0, NULL);
    c = hi_emit(HI_SGT, TY_INT, c, f77_iconst(0), 0, NULL);
    hi_emit(HI_BRC, TY_VOID, c, b_body, b_exit, NULL);
    f77_cur_blk_live = 0;

    f77_begin_blk(b_body);

    if (ctl_n >= F77_MAX_CTL) { f77_error("control nesting too deep"); return; }
    ctl_kind[ctl_n] = F77_CTL_DO;
    ctl_label[ctl_n] = lab;
    ctl_var[ctl_n] = s;
    ctl_step[ctl_n] = m3;
    ctl_stephi[ctl_n] = step_hi;
    ctl_trip[ctl_n] = trip_alloca;
    ctl_test[ctl_n] = b_test;
    ctl_body[ctl_n] = b_body;
    ctl_exit[ctl_n] = b_exit;
    ctl_n = ctl_n + 1;
}

/* Close every DO whose terminal label is the one just executed. */
static void f77_close_do(int lab) {
    int s;
    int v;
    int t;
    while (ctl_n > f77_ctl_base && ctl_kind[ctl_n - 1] == F77_CTL_DO &&
           ctl_label[ctl_n - 1] == lab) {
        ctl_n = ctl_n - 1;
        s = ctl_var[ctl_n];
        if (f77_cur_blk_live) {
            int vty;
            vty = f77_sty[s];
            if (vty == TY_FLOAT) {
                v = hi_emit(HI_LOAD, TY_FLOAT, f77_sval[s], -1, 0, NULL);
                v = hi_emit(HI_FADD, TY_FLOAT, v, ctl_step[ctl_n], 0, NULL);
                hi_emit(HI_STORE, TY_FLOAT, f77_sval[s], v, 0, NULL);
            } else if (vty == TY_DOUBLE) {
                int vlo;
                int vhi;
                int shi;
                int addr4;
                vlo = hi_emit(HI_LOAD, TY_INT, f77_sval[s], -1, 0, NULL);
                addr4 = hi_emit(HI_ADDI, HL_ADDR_TY, f77_sval[s], -1, 4, NULL);
                vhi = hi_emit(HI_LOAD, TY_INT, addr4, -1, 0, NULL);
                shi = ctl_stephi[ctl_n];
                vlo = f77_fp64_call2("__fp64_add", vlo, vhi,
                                     ctl_step[ctl_n], shi, &vhi);
                hi_emit(HI_STORE, TY_INT, f77_sval[s], vlo, 0, NULL);
                hi_emit(HI_STORE, TY_INT, addr4, vhi, 0, NULL);
            } else {
                v = hi_emit(HI_LOAD, TY_INT, f77_sval[s], -1, 0, NULL);
                v = hi_emit(HI_ADD, TY_INT, v, ctl_step[ctl_n], 0, NULL);
                hi_emit(HI_STORE, TY_INT, f77_sval[s], v, 0, NULL);
            }
            t = hi_emit(HI_LOAD, TY_INT, ctl_trip[ctl_n], -1, 0, NULL);
            t = hi_emit(HI_ADDI, TY_INT, t, -1, -1, NULL);
            hi_emit(HI_STORE, TY_INT, ctl_trip[ctl_n], t, 0, NULL);
            /* Rotated back edge: test the freshly decremented trip
             * here and branch straight to the body; the header test
             * still guards the zero-trip entry.  The scalar-double
             * gate that once protected fp64-phi loops is gone:
             * live-range splitting (licm_split) frees the registers
             * those loops needed, and rotated double loops now win
             * too (mandel 35.2M -> 28.4M with ZERO spills). */
            v = hi_emit(HI_SGT, TY_INT, t, f77_iconst(0), 0, NULL);
            hi_emit(HI_BRC, TY_VOID, v, ctl_body[ctl_n], ctl_exit[ctl_n], NULL);
        }
        f77_cur_blk_live = 0;
        f77_begin_blk(ctl_exit[ctl_n]);
    }
}

/* --- subprograms ------------------------------------------------------ */

#define F77_MAX_INLINE_DEPTH 3
/* Default 12 = inlining ON.  For most of this compiler's life the
 * default was 0: inlining measured 2.3x WORSE on LINPACK at every
 * threshold, and the frame note below blamed the by-reference
 * convention.  The convention was never the whole story.  The loss
 * was three register-allocator defects stacked: linear-interval
 * liveness inventing cross-loop interference, coloring failures
 * landing on the hottest nodes because nothing chose spill victims,
 * and pair pins leaking across gc_build rounds once iteration
 * existed.  With per-block liveness + iterated spilling (hir_regalloc)
 * those are gone and threshold 12 measures LINPACK 563M -> 526M
 * (1.048x clang).  F77_INLINE_MAX=<n> still overrides; 0 disables. */
#define F77_INLINE_MAX_STMTS 12

static int f77_inline_depth;
static int f77_inline_unit[F77_MAX_INLINE_DEPTH];
static int f77_inline_ret_blk;       /* where RETURN branches to */
static int f77_inline_result;        /* FUNCTION result symbol, or -1 */
static int f77_ustmts[F77_MAX_UNIT]; /* statement count per unit */
static int f77_unosplice[F77_MAX_UNIT]; /* unit holds scope-stateful
    declarations (COMMON/IMPLICIT/DATA/...) that cannot be re-parsed in
    an executable context: never inline it.  A false positive (DATAX=5
    prefix-matches DATA) only costs a missed inline. */
static int f77_inline_count;         /* diagnostics */



static int f77_result_sym;    /* FUNCTION: the symbol named after the unit */

/* If this statement is a `[type] FUNCTION name(...)` header, return the
 * result type; otherwise -1.  Checked before the type-declaration
 * keywords, because `REAL FUNCTION F(X)` squeezes to REALFUNCTIONF(X)
 * and would otherwise parse as a REAL declaration. */
static int f77_unit_header_ty(void) {
    if (f77_starts("FUNCTION"))                return TY_INT;   /* implicit */
    if (f77_starts("INTEGERFUNCTION"))         return TY_INT;
    if (f77_starts("LOGICALFUNCTION"))         return TY_INT;
    if (f77_starts("REALFUNCTION"))            return TY_FLOAT;
    if (f77_starts("DOUBLEPRECISIONFUNCTION")) return TY_DOUBLE;
    return -1;
}

/* Emit the unit's return.  A FUNCTION returns the variable named after
 * it, which F77 code assigns to; a SUBROUTINE and the main PROGRAM
 * return an int (the exit status, for the PROGRAM). */
static void f77_emit_return(void) {
    int v;
    if (!f77_cur_blk_live) return;
    f77_emit_copyout();
    if (f77_inline_depth > 0) {
        /* Inside a spliced body a RETURN is a branch to the
         * continuation, not a machine return.  The FUNCTION result is
         * already in its variable, which the caller reads. */
        f77_goto_blk(f77_inline_ret_blk);
        return;
    }
    if (f77_ukind[f77_cur_unit] == F77_UNIT_FUNC && f77_result_sym >= 0) {
        v = f77_load_sym(f77_result_sym);
        if (ex_ty == TY_DOUBLE) {
            /* Wide result: lo in src1, hi in src2, and ty stays 0 --
             * the convention the C compiler uses for llong/double
             * returns on SLOW-32. */
            hi_emit(HI_RET, 0, v, ex_hi, 0, NULL);
            f77_cur_blk_live = 0;
            return;
        }
        hi_emit(HI_RET, 0, v, -1, 0, NULL);
        f77_cur_blk_live = 0;
        return;
    }
    hi_emit(HI_RET, TY_INT, f77_iconst(0), -1, 0, NULL);
    f77_cur_blk_live = 0;
}

/* Is the actual starting at the current token just a variable or a
 * single array element -- i.e. does a NAME, optionally followed by a
 * balanced subscript list, run to the very end of this argument?
 *
 * This is a text-level lookahead over the assembled statement rather
 * than a parse-and-rewind, because rewinding would mean un-emitting the
 * HIR the subscript expressions had already produced. */
static int f77_actual_is_simple(void) {
    int i;
    int depth;
    i = (int)(lx_rts - lx_stmt);       /* start of the current token */
    if (i < 0 || i >= lx_stmt_len) return 0;
    if (!lx_isalpha(lx_stmt[i])) return 0;
    while (i < lx_stmt_len &&
           (lx_isalpha(lx_stmt[i]) || lx_isdigit(lx_stmt[i]))) i = i + 1;
    if (i < lx_stmt_len && lx_stmt[i] == '(') {
        depth = 0;
        while (i < lx_stmt_len) {
            if (lx_stmt[i] == '(') depth = depth + 1;
            else if (lx_stmt[i] == ')') {
                depth = depth - 1;
                if (depth == 0) { i = i + 1; break; }
            }
            i = i + 1;
        }
    }
    if (i >= lx_stmt_len) return 0;
    return lx_stmt[i] == ',' || lx_stmt[i] == ')';
}

/* Address of an actual argument.  Fortran is call-by-reference, so a
 * variable or array element is passed as its address; anything else is
 * evaluated into a temporary whose address is passed instead. */
static int f77_actual_addr(void) {
    int s;
    int v;
    int vty;
    int vhi;
    int tmp;

    if (lx_t == T_NAME && f77_find_func(lex_name) < 0 && f77_actual_is_simple()) {
        /* A bare name, or a subscripted element, is passed by address;
         * a whole array passes its base.  A FUNCTION reference is not a
         * variable, so it falls through and is evaluated below.
         *
         * f77_actual_is_simple() is what keeps `N-K+1` out of this
         * path: without it the leading NAME would be taken as the whole
         * actual, N's address passed, and `-K+1` left unconsumed. */
        s = f77_sym(lex_name);
        /* A PARAMETER is a named constant, not a variable.  Passing
         * its leftover alloca would hand the callee garbage; fall
         * through and evaluate into a temp, like N+0 already does. */
        if (!f77_sparam[s]) {
            f77_tok();
            if (lx_t == T_LP && f77_srank[s] > 0) {
                f77_tok();
                return f77_subscript_addr(s);
            }
            /* Taking the address of a split double escapes BOTH halves.
             * The promoter sees the lo alloca in h_carg and rejects it, but
             * the hi alloca appears nowhere -- it would stay promoted while
             * the callee read stale memory at offset+4.  A dead ADDI on the
             * hi slot is an address-taking use, which the promotion scan
             * rejects; DCE removes it afterwards, so it costs nothing in
             * the emitted code. */
            if (f77_srank[s] == 0) f77_sstored[s] = 1;   /* address escapes */
            if (f77_shi[s] >= 0)
                hi_emit(HI_ADDI, HL_ADDR_TY, f77_shi[s], -1, 0, NULL);
            return f77_sval[s];
        }
    }

    v = f77_expr();
    vty = ex_ty;
    vhi = ex_hi;
    f77_frame = f77_frame + ty_size(vty);
    tmp = hi_emit(HI_ALLOCA, vty, -1, -1, 0 - f77_frame, NULL);
    hl_ainst[hl_nalloca] = tmp;
    hl_aoff[hl_nalloca] = 0 - f77_frame;
    hl_nalloca = hl_nalloca + 1;
    f77_store_at(tmp, vty, v, vty, vhi);
    return tmp;
}

/* --- inlining (ON by default since the allocator rework) --------------
 *
 * f77 is one-pass with no AST, so a callee's body is not sitting in a
 * data structure waiting to be spliced.  What IS available is its
 * source offset (phase 1 recorded it), so inlining here means
 * RE-LEXING the callee's body at the call site with its dummy
 * arguments bound to the actuals.
 *
 * Fortran makes the binding unusually clean: arguments are by
 * reference, so a dummy's "value" is just an address.  Inlining
 * therefore binds each dummy symbol to the actual's address value
 * directly -- no PARAM, no marshalling, no call.  That is the entire
 * saving, and on LINPACK it is DAXPY vanishing into DGEFA's inner loop.
 *
 * Everything the callee touches must be scoped: symbols and labels get
 * fresh bases, the control stack is checkpointed (f77_ctl_base -- a
 * callee label must not close a CALLER's DO loop), the caller's
 * assembled statement is saved, and so is the token scanner's own state,
 * or a nested splice resumes the outer call mid-nowhere.
 *
 * HISTORY: this shipped OFF for most of the compiler's life, measured
 * 2.30x-off / 2.79x-at-12 / 6.09x-at-40 vs clang on LINPACK, and a
 * long note here blamed the by-reference convention (a dummy stays an
 * address when spliced, so the splice "only buys call/return").  That
 * explanation is RETRACTED: the loss was the register allocator, not
 * the convention -- linear-interval liveness invented interference
 * between loops laid end to end, coloring failures landed on the
 * hottest nodes, and pair pins leaked across allocation rounds.  With
 * per-block liveness and iterated spilling (see hir_regalloc.h) the
 * same splice at the same threshold 12 measures LINPACK at 1.048x
 * clang against 1.123x with inlining off.  h_ld_ro (F77 15.9.3.6)
 * already hoists the loads the old note said needed scalar
 * replacement.
 *
 * F77_INLINE_MAX=<n> overrides the threshold; 0 disables.
 */

/* Is `u` worth and safe to inline here?  Recursion is excluded by
 * checking the active inline stack, which also covers mutual
 * recursion. */
static int f77_inline_disabled = -1;   /* -1 = not yet probed */
static int f77_inline_max;

static int f77_can_inline(int u) {
    int i;
    if (f77_inline_disabled < 0) {
        char *e;
        f77_inline_disabled = getenv("F77_NO_INLINE") ? 1 : 0;
        e = getenv("F77_INLINE_MAX");
        f77_inline_max = e ? atoi(e) : F77_INLINE_MAX_STMTS;
    }
    if (f77_inline_disabled) return 0;
    if (u < 0) return 0;
    if (f77_ukind[u] == F77_UNIT_PROGRAM) return 0;
    if (f77_inline_depth >= F77_MAX_INLINE_DEPTH) return 0;
    if (f77_ustmts[u] > f77_inline_max) return 0;
    if (f77_unosplice[u]) return 0;
    if (u == f77_cur_unit) return 0;
    i = 0;
    while (i < f77_inline_depth) {
        if (f77_inline_unit[i] == u) return 0;
        i = i + 1;
    }
    return 1;
}

/* Bind one dummy argument to an actual's address, as a symbol whose
 * "storage" IS that address.  This is what a by-reference call would
 * have achieved via a PARAM, minus the call. */
static int f77_bind_actual(char *nm, int addr) {
    int s;
    if (f77_nsym >= F77_MAX_SYM) { f77_error("too many symbols"); return 0; }
    s = f77_nsym;
    strcpy(f77_sname[s], nm);
    f77_sty[s] = f77_implicit_ty(nm);
    f77_srank[s] = 0;
    f77_sarg[s] = 1;
    f77_shi[s] = -1;                 /* by-reference: value is an address */
    { int d; d = 0; while (d < F77_MAX_RANK) { f77_sextsym[s][d] = -1; d = d + 1; } }
    f77_sval[s] = addr;
    f77_nsym = f77_nsym + 1;
    return s;
}

static void f77_statement(void);

/* Splice unit `u`'s body in at the current point, with `addrs[0..n)`
 * bound to its dummy arguments.  Returns the FUNCTION result symbol,
 * or -1 for a SUBROUTINE. */
static int f77_inline_unit_body(int u, int *addrs, int nargs) {
    char save_stmt[F77_MAX_STMT];
    int save_len;
    int save_label;
    int save_line;
    int save_pos;
    /* The token scanner runs over lx_stmt, so splicing a callee moves
     * the cursor into the callee's statements.  Restoring the buffer is
     * not enough -- the scanner state and the current token have to come
     * back too, or the caller resumes mid-nowhere.  This is what broke
     * ISQ(ISQ(2)): the outer call resumed with a stale cursor. */
    char *save_rp;
    char *save_rpe;
    char *save_rts;
    char *save_rte;
    int save_rcs;
    int save_ract;
    int save_t;
    char save_name[F77_MAX_NAME];
    int save_namelen;
    int save_ival;
    double save_dval;
    int save_sidx;
    int save_slen;
    int save_scope;
    int save_lblbase;
    int save_nlabel;
    int save_ctl;
    int save_ctl_base;
    int save_unit;
    int save_ret;
    int save_result;
    int b_cont;
    int skip;
    int i;
    int rty;
    int result;

    /* Checkpoint everything the callee will disturb. */
    memcpy(save_stmt, lx_stmt, lx_stmt_len + 1);
    save_len = lx_stmt_len;
    save_label = lx_stmt_label;
    save_line = lx_line;
    save_pos = lx_pos;
    save_rp = lx_rp; save_rpe = lx_rpe; save_rts = lx_rts; save_rte = lx_rte;
    save_rcs = lx_rcs; save_ract = lx_ract;
    save_t = lx_t;
    memcpy(save_name, lex_name, F77_MAX_NAME);
    save_namelen = lex_namelen;
    save_ival = lex_ival; save_dval = lex_dval;
    save_sidx = lex_sidx; save_slen = lex_slen;
    save_scope = f77_scope_base;
    save_lblbase = f77_label_base;
    save_nlabel = f77_nlabel;
    save_ctl = ctl_n;
    save_ctl_base = f77_ctl_base;
    save_unit = f77_cur_unit;
    save_ret = f77_inline_ret_blk;
    save_result = f77_inline_result;

    b_cont = hir_new_block();

    f77_scope_base = f77_nsym;
    f77_label_base = f77_nlabel;
    f77_ctl_base = ctl_n;
    f77_inline_ret_blk = b_cont;
    f77_cur_unit = u;
    f77_inline_unit[f77_inline_depth] = u;
    f77_inline_depth = f77_inline_depth + 1;
    f77_inline_count = f77_inline_count + 1;

    /* Read the callee's header and bind its dummies to the actuals. */
    lx_pos = f77_upos[u];
    lx_line = f77_uline[u];
    if (!f77_next_stmt()) { f77_error("inline: empty unit"); }
    if (f77_ukind[u] == F77_UNIT_SUBR) skip = 10;
    else if (f77_starts("DOUBLEPRECISIONFUNCTION")) skip = 23;
    else if (f77_starts("INTEGERFUNCTION"))    skip = 15;
    else if (f77_starts("LOGICALFUNCTION"))    skip = 15;
    else if (f77_starts("REALFUNCTION"))       skip = 12;
    else                                       skip = 8;
    f77_scan_from(skip);
    if (lx_t == T_NAME) f77_tok();
    i = 0;
    if (lx_t == T_LP) {
        f77_tok();
        while (lx_t == T_NAME) {
            if (i < nargs) f77_bind_actual(lex_name, addrs[i]);
            else           f77_sym(lex_name);   /* missing actual: local */
            i = i + 1;
            f77_tok();
            if (lx_t != T_COMMA) break;
            f77_tok();
        }
        if (lx_t == T_RP) f77_tok();
    }

    result = -1;
    if (f77_ukind[u] == F77_UNIT_FUNC) {
        rty = f77_urty[u];
        result = f77_sym(f77_uname[u]);
        f77_sty[result] = rty;
        f77_sarg[result] = 0;
        f77_realloc_sym(result);
    }
    f77_inline_result = result;

    /* Body. */
    for (;;) {
        if (!f77_next_stmt()) break;
        if (f77_is_unit_end()) break;
        if (f77_unit_header_ty() >= 0 || f77_starts("SUBROUTINE")) break;
        f77_statement();
    }

    f77_goto_blk(b_cont);
    f77_begin_blk(b_cont);

    /* Restore. */
    f77_inline_depth = f77_inline_depth - 1;
    f77_cur_unit = save_unit;
    f77_inline_ret_blk = save_ret;
    f77_inline_result = save_result;
    f77_scope_base = save_scope;
    f77_label_base = save_lblbase;
    /* f77_nsym is deliberately NOT restored: the FUNCTION result
     * symbol must outlive the splice so the caller can load it, and
     * the scope base already hides the callee's names. */
    f77_nlabel = save_nlabel;
    ctl_n = save_ctl;
    f77_ctl_base = save_ctl_base;
    lx_pos = save_pos;
    lx_line = save_line;
    memcpy(lx_stmt, save_stmt, save_len + 1);
    lx_stmt_len = save_len;
    lx_stmt_label = save_label;
    lx_rp = save_rp; lx_rpe = save_rpe; lx_rts = save_rts; lx_rte = save_rte;
    lx_rcs = save_rcs; lx_ract = save_ract;
    lx_t = save_t;
    memcpy(lex_name, save_name, F77_MAX_NAME);
    lex_namelen = save_namelen;
    lex_ival = save_ival; lex_dval = save_dval;
    lex_sidx = save_sidx; lex_slen = save_slen;

    return result;
}

/* CALL name(a1, a2, ...) */
static void f77_stmt_call(int skip) {
    char nm[F77_MAX_NAME];
    int addrs[64];
    int cb;
    int nargs;
    int r;

    f77_scan_from(skip);
    if (lx_t != T_NAME) { f77_error("CALL needs a subroutine name"); return; }
    strcpy(nm, lex_name);
    f77_tok();

    nargs = 0;
    if (lx_t == T_LP) {
        f77_tok();
        if (lx_t != T_RP) {
            for (;;) {
                /* Evaluated before any h_carg slot is reserved -- see
                 * the note in the FUNCTION path. */
                if (nargs < 64) addrs[nargs] = f77_actual_addr();
                else f77_actual_addr();
                nargs = nargs + 1;
                if (lx_t != T_COMMA) break;
                f77_tok();
            }
        }
        if (lx_t != T_RP) f77_error("expected ) after arguments");
        else f77_tok();
    }
    {
        int u;
        u = f77_find_unit(nm);
        if (u >= 0 && f77_can_inline(u)) {
            f77_inline_unit_body(u, addrs, nargs);
            return;
        }
    }
    cb = h_ncarg;
    {
        int ai;
        ai = 0;
        while (ai < nargs) {
            h_carg[h_ncarg] = addrs[ai];
            h_carg_tag[h_ncarg] = 0;
            h_ncarg = h_ncarg + 1;
            ai = ai + 1;
        }
    }
    r = hi_emit(HI_CALL, TY_INT, -1, -1, nargs, strdup(nm));
    h_cbase[r] = cb;
}

static int f77_unit_nparams;

/* --- scalar dummy copy-in / copy-out ---------------------------------
 *
 * A by-reference scalar dummy is re-loaded through its address at every
 * use, and LICM cannot hoist those loads out of a loop that stores
 * through any other pointer, because it has no alias analysis.  In
 * DAXPY that costs two loads of DA on every iteration.
 *
 * Fortran, unlike C, says those loads ARE invariant: if a dummy
 * argument is assigned, no other name may be associated with the same
 * storage (F77 15.9.3.6).  DY is assigned, so DA cannot alias it.  This
 * is the rule that historically made Fortran faster than C on numeric
 * code, and it is available here for free.
 *
 * So each scalar dummy is copied into a local on entry and copied back
 * at every RETURN.  Copy-out keeps routines like SWAP correct; arrays
 * are left alone, since copying them would cost more than it saves.
 *
 * MEASURED A LOSS, so it is OFF by default (F77_COPYIN=1 to enable):
 * 2.14x clang without it, 3.09x with, and DAXPY's hot loop went from 17
 * instructions to 30.  The reason is not the idea but a deeper gap:
 * DOUBLE PRECISION locals are NEVER register-promoted in this compiler,
 * so the "local copy" is still a memory access -- no load is saved --
 * while the extra live values push the allocator into spilling.  A
 * program with one double and one integer emits twelve frame accesses.
 *
 * mem2reg refuses a double alloca because reaching its hi word takes
 * the alloca's address (ADDI base,4), which every promotion scan treats
 * as address-taken.  Promoting doubles needs PAIR-AWARE promotion --
 * one alloca becoming two SSA values -- and until that exists, copy-in
 * has nothing to win.
 *
 * The copies are emitted at the first EXECUTABLE statement rather than
 * at the header, because in a one-pass compiler the dummy's type is not
 * known until its declaration has been read -- and F77 requires all
 * declarations to precede executable statements, so that point is
 * exactly when every type is known. */
#define F77_MAX_COPYIN 32
static int f77_ci_sym[F77_MAX_COPYIN];   /* the dummy's symbol */
static int f77_ci_addr[F77_MAX_COPYIN];  /* its original address value */
static int f77_ci_n;
static int f77_decls_open;               /* still in the declaration part */

static int f77_copyin_on = -1;

static void f77_emit_copyin(void) {
    int i;
    /* The declaration part is closing: every member's type and shape
     * is final, so COMMON blocks and SAVEd locals can be laid out. */
    f77_layout_static();
    if (f77_copyin_on < 0) f77_copyin_on = getenv("F77_COPYIN") ? 1 : 0;
    if (!f77_copyin_on) { f77_decls_open = 0; f77_ci_n = 0; return; }
    int s;
    int a;
    int v;
    int hi;
    f77_decls_open = 0;
    i = 0;
    while (i < f77_ci_n) {
        s = f77_ci_sym[i];
        a = f77_ci_addr[i];
        if (f77_srank[s] == 0) {
            /* Give the symbol real local storage and seed it --
             * through f77_realloc_sym, so a double copy gets the
             * SPLIT lo/hi slots every declared local gets.  The
             * 2026-08-27 measurement that sank copy-in predates the
             * split: the copy was one 8-byte alloca, reached by
             * ADDI base,4, address-taken, never promoted -- so no
             * load was saved while the extra live values spilled. */
            hi = -1;
            v = f77_load_at(a, f77_sty[s]);
            hi = ex_hi;
            f77_sarg[s] = 0;
            f77_frame = f77_frame + 4;
            f77_sval[s] = hi_emit(HI_ALLOCA, TY_INT, -1, -1,
                                  0 - f77_frame, NULL);
            hl_ainst[hl_nalloca] = f77_sval[s];
            hl_aoff[hl_nalloca] = 0 - f77_frame;
            hl_nalloca = hl_nalloca + 1;
            f77_realloc_sym(s);
            f77_store_sym_val(s, v, f77_sty[s], hi);
            i = i + 1;
        } else {
            /* An array dummy keeps its by-reference binding. */
            f77_ci_sym[i] = f77_ci_sym[f77_ci_n - 1];
            f77_ci_addr[i] = f77_ci_addr[f77_ci_n - 1];
            f77_ci_n = f77_ci_n - 1;
        }
    }
}

/* Write the local copies back through their original addresses. */
static void f77_emit_copyout(void) {
    int i;
    int s;
    int v;
    i = 0;
    while (i < f77_ci_n) {
        s = f77_ci_sym[i];
        if (!f77_sarg[s]) {
            v = f77_load_sym(s);
            f77_store_at(f77_ci_addr[i], f77_sty[s], v, f77_sty[s], ex_hi);
        }
        i = i + 1;
    }
}

static int f77_urty_of(int u) { return f77_urty[u]; }

/* Index of the FUNCTION unit called `nm`, or -1.  Phase 1 records every
 * unit in the file before any of them is compiled, so a call can be
 * resolved even when the callee appears later in the source. */
/* Any program unit with this name -- SUBROUTINE or FUNCTION. */
static int f77_find_unit(char *nm) {
    int i;
    i = 0;
    while (i < f77_nunit) {
        if (f77_ukind[i] != F77_UNIT_PROGRAM &&
            strcmp(f77_uname[i], nm) == 0) return i;
        i = i + 1;
    }
    return -1;
}

static int f77_find_func(char *nm) {
    int i;
    i = 0;
    while (i < f77_nunit) {
        if (f77_ukind[i] == F77_UNIT_FUNC && strcmp(f77_uname[i], nm) == 0)
            return i;
        i = i + 1;
    }
    return -1;
}

/* Record the unit's name from its header statement. */
static void f77_unit_name(int u) {
    int skip;
    if (f77_starts("SUBROUTINE")) skip = 10;
    else if (f77_starts("DOUBLEPRECISIONFUNCTION")) skip = 23;
    else if (f77_starts("INTEGERFUNCTION")) skip = 15;
    else if (f77_starts("LOGICALFUNCTION")) skip = 15;
    else if (f77_starts("REALFUNCTION")) skip = 12;
    else skip = 8;
    f77_scan_from(skip);
    if (lx_t == T_NAME) strcpy(f77_uname[u], lex_name);
    else { f77_error("subprogram needs a name"); strcpy(f77_uname[u], "unnamed"); }
}

/* Bind this unit's header: record its dummy arguments as by-reference
 * symbols backed by HIR PARAMs, and, for a FUNCTION, create the result
 * variable that shares the unit's name. */
static void f77_bind_unit(int u) {
    int skip;
    int s;
    int nparam;
    int rty;

    f77_cur_unit = u;
    f77_result_sym = -1;
    nparam = 0;
    f77_unit_nparams = 0;
    f77_ci_n = 0;
    f77_decls_open = 1;
    f77_cm_n = 0;
    f77_save_all = 0;
    f77_sv_n = 0;
    f77_di_n = 0;
    f77_ldro_n = 0;

    /* Reset the implicit-typing map to the default rule. */
    {
        int c;
        for (c = 0; c < 26; c++)
            f77_imp_map[c] = (c >= 'I' - 'A' && c <= 'N' - 'A') ? TY_INT : TY_FLOAT;
    }
    f77_imp_ready = 1;
    f77_imp_none = 0;

    if (f77_ukind[u] == F77_UNIT_PROGRAM) {
        hl_param_nflat = 0;
        hl_nparams = 0;
        return;
    }

    /* Skip past the keyword and the unit name to the argument list. */
    skip = 0;
    if (f77_ukind[u] == F77_UNIT_SUBR) skip = 10;              /* SUBROUTINE */
    else {
        rty = f77_unit_header_ty();
        if (f77_starts("DOUBLEPRECISIONFUNCTION")) skip = 23;
        else if (f77_starts("INTEGERFUNCTION"))    skip = 15;
        else if (f77_starts("LOGICALFUNCTION"))    skip = 15;
        else if (f77_starts("REALFUNCTION"))       skip = 12;
        else                                       skip = 8;   /* FUNCTION */
        (void)rty;
    }
    f77_scan_from(skip);
    if (lx_t != T_NAME) { f77_error("subprogram needs a name"); return; }
    f77_tok();

    if (lx_t == T_LP) {
        f77_tok();
        if (lx_t != T_RP) {
            for (;;) {
                if (lx_t != T_NAME) { f77_error("bad dummy argument"); break; }
                s = f77_sym_param(lex_name, nparam);
                if (f77_ci_n < F77_MAX_COPYIN) {
                    f77_ci_sym[f77_ci_n] = s;
                    f77_ci_addr[f77_ci_n] = f77_sval[s];
                    f77_ci_n = f77_ci_n + 1;
                }
                nparam = nparam + 1;
                f77_tok();
                if (lx_t != T_COMMA) break;
                f77_tok();
            }
        }
        if (lx_t == T_RP) f77_tok();
    }

    f77_unit_nparams = nparam;

    /* Resolve where the incoming arguments actually arrive.  Without
     * this the codegen's entry sequence is skipped entirely (it gates
     * on h_val < hl_param_nflat) and the argument registers are never
     * moved into the registers the allocator chose -- which silently
     * "works" only when the allocator happens to pick the ABI register.
     * Every Fortran dummy is one 32-bit address, so every tag is 0. */
    {
        int i;
        int ord;
        i = 0;
        while (i < 64) { hl_param_tags[i] = 0; i = i + 1; }
        hl_param_nflat = nparam;
        hl_nparams = nparam;
        hi_abi_assign(hl_param_tags, hl_param_nflat, hl_param_map);
        ord = 0;
        i = 0;
        while (i < hl_param_nflat) {
            if (hl_param_map[i] < 0) { hl_param_stkord[i] = ord; ord = ord + 4; }
            else hl_param_stkord[i] = -1;
            i = i + 1;
        }
    }

    if (f77_ukind[u] == F77_UNIT_FUNC) {
        f77_result_sym = f77_sym(f77_uname[u]);
        f77_sty[f77_result_sym] = f77_urty[u];
        /* A typed header (INTEGER FUNCTION F...) is an explicit
         * declaration; a bare FUNCTION F relies on implicit typing,
         * which a later IMPLICIT may not change (phase 1 already told
         * every caller the result type). */
        f77_styped[f77_result_sym] = (skip != 8) ? 1 : 0;
        f77_realloc_sym(f77_result_sym);
    }
}

/* --- formatted I/O ---------------------------------------------------- */

/* WRITE and READ share the list walkers (f77_wr_one and friends):
 * implied-DOs, whole arrays, and the comma structure are identical in
 * both directions.  f77_io_rd flips the leaves -- a WRITE leaf loads a
 * value and hands it to f77_wr_*, a READ leaf takes the item's ADDRESS
 * and hands it to f77_rd_*, which stores through it. */
static int f77_io_rd;

/* Emit the call that hands one item to the runtime, choosing the entry
 * point by the item's type.  Doubles travel as an aligned register pair
 * (tags 1/2), which is the ABI a C callee taking a double expects. */
static void f77_wr_item(int v, int vty, int vhi) {
    int cb;
    int r;
    cb = h_ncarg;
    if (vty == TY_DOUBLE) {
        h_carg[h_ncarg] = v;   h_carg_tag[h_ncarg] = 1; h_ncarg = h_ncarg + 1;
        h_carg[h_ncarg] = vhi; h_carg_tag[h_ncarg] = 2; h_ncarg = h_ncarg + 1;
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "f77_wr_d");
    } else if (vty == TY_FLOAT) {
        h_carg[h_ncarg] = v; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "f77_wr_r");
    } else {
        h_carg[h_ncarg] = v; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "f77_wr_i");
    }
    h_cbase[r] = cb;
}

/* READ leaf: hand the runtime the item's address to store through.
 * INTEGER and LOGICAL both travel as TY_INT; the runtime tells I and L
 * apart by the format descriptor, not the type. */
static void f77_rd_item(int addr, int ty) {
    int cb;
    int r;
    cb = h_ncarg;
    h_carg[h_ncarg] = addr; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    if (ty == TY_DOUBLE)
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "f77_rd_d");
    else if (ty == TY_FLOAT)
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "f77_rd_r");
    else
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "f77_rd_i");
    h_cbase[r] = cb;
}

/* A READ list item must be a variable or an array element -- somewhere
 * to store.  Same address recipe as f77_actual_addr's simple path,
 * including the address-taking escape on a split double's hi slot: the
 * runtime stores 8 contiguous bytes through the lo address, so both
 * halves must stay in memory. */
static int f77_rd_target(int *tyo) {
    int s;
    if (lx_t != T_NAME) {
        f77_error("READ list item must be a variable or array element");
        *tyo = TY_INT;
        return f77_iconst(0);
    }
    s = f77_sym(lex_name);
    if (f77_sparam[s]) {
        f77_error("cannot READ into a PARAMETER");
        *tyo = TY_INT;
        return f77_iconst(0);
    }
    f77_sstored[s] = 1;
    f77_tok();
    *tyo = f77_sty[s];
    if (lx_t == T_LP && f77_srank[s] > 0) {
        f77_tok();
        return f77_subscript_addr(s);
    }
    if (f77_shi[s] >= 0)
        hi_emit(HI_ADDI, HL_ADDR_TY, f77_shi[s], -1, 0, NULL);
    return f77_sval[s];
}

/* Does the parenthesised group starting at `open` look like an
 * implied-DO?  It does when the group contains a top-level `=` -- an
 * ordinary parenthesised expression never has one.  Reports the offset
 * just past the closing paren, the `=`, where the items start, and
 * where the control variable begins. */
static int f77_implied_do_spans(int open, int *close_off, int *eq_off,
                                int *items_off, int *ctl_off) {
    int i;
    int depth;
    int eq;
    int comma;

    if (open >= lx_stmt_len || lx_stmt[open] != '(') return 0;
    depth = 0;
    eq = -1;
    comma = -1;
    i = open;
    while (i < lx_stmt_len) {
        if (lx_stmt[i] == '\'') {
            i = i + 1;
            while (i < lx_stmt_len && lx_stmt[i] != '\'') i = i + 1;
        } else if (lx_stmt[i] == '(') depth = depth + 1;
        else if (lx_stmt[i] == ')') {
            depth = depth - 1;
            if (depth == 0) break;
        } else if (depth == 1) {
            if (lx_stmt[i] == '=' && eq < 0) eq = i;
            else if (lx_stmt[i] == ',' && eq < 0) comma = i;
        }
        i = i + 1;
    }
    if (i >= lx_stmt_len || eq < 0 || comma < 0) return 0;
    *close_off = i + 1;
    *eq_off = eq;
    *items_off = open + 1;
    *ctl_off = comma + 1;
    return 1;
}

/* Column-major expansion of an unsubscripted array in an I/O list. */
static void f77_wr_whole_array(int s) {
    int rank;
    int k;
    int iv[F77_MAX_RANK];
    int trip[F77_MAX_RANK];
    int b_test[F77_MAX_RANK];
    int b_body[F77_MAX_RANK];
    int b_exit[F77_MAX_RANK];
    int c;
    int t;
    int idx;
    int off;
    int byte;
    int addr;
    int v;
    int vhi;
    int cstride;

    rank = f77_srank[s];
    for (k = 0; k < rank; k++) {
        if (f77_sextsym[s][k] >= 0) {
            f77_error("whole-array I/O needs constant extents");
            return;
        }
    }
    for (k = rank - 1; k >= 0; k--) {
        f77_frame = f77_frame + 4;
        iv[k] = hi_emit(HI_ALLOCA, TY_INT, -1, -1, 0 - f77_frame, NULL);
        hl_ainst[hl_nalloca] = iv[k];
        hl_aoff[hl_nalloca] = 0 - f77_frame;
        hl_nalloca = hl_nalloca + 1;
        f77_frame = f77_frame + 4;
        trip[k] = hi_emit(HI_ALLOCA, TY_INT, -1, -1, 0 - f77_frame, NULL);
        hl_ainst[hl_nalloca] = trip[k];
        hl_aoff[hl_nalloca] = 0 - f77_frame;
        hl_nalloca = hl_nalloca + 1;
        hi_emit(HI_STORE, TY_INT, iv[k], f77_iconst(f77_slo[s][k]), 0, NULL);
        hi_emit(HI_STORE, TY_INT, trip[k], f77_iconst(f77_sext[s][k]), 0, NULL);
        b_test[k] = hir_new_block();
        b_body[k] = hir_new_block();
        b_exit[k] = hir_new_block();
        f77_goto_blk(b_test[k]);
        f77_begin_blk(b_test[k]);
        c = hi_emit(HI_LOAD, TY_INT, trip[k], -1, 0, NULL);
        c = hi_emit(HI_SGT, TY_INT, c, f77_iconst(0), 0, NULL);
        hi_emit(HI_BRC, TY_VOID, c, b_body[k], b_exit[k], NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_body[k]);
    }
    off = f77_iconst(0);
    cstride = 1;
    for (k = 0; k < rank; k++) {
        idx = hi_emit(HI_LOAD, TY_INT, iv[k], -1, 0, NULL);
        if (f77_slo[s][k] != 0)
            idx = hi_emit(HI_ADDI, TY_INT, idx, -1, 0 - f77_slo[s][k], NULL);
        if (cstride != 1)
            idx = hi_emit(HI_MUL, TY_INT, idx, f77_iconst(cstride), 0, NULL);
        off = hi_emit(HI_ADD, TY_INT, off, idx, 0, NULL);
        cstride = cstride * f77_sext[s][k];
    }
    t = ty_size(f77_sty[s]);
    if (t == 4) byte = hi_emit(HI_SLL, TY_INT, off, f77_iconst(2), 0, NULL);
    else if (t == 8) byte = hi_emit(HI_SLL, TY_INT, off, f77_iconst(3), 0, NULL);
    else byte = hi_emit(HI_MUL, TY_INT, off, f77_iconst(t), 0, NULL);
    addr = hi_emit(HI_ADD, HL_ADDR_TY, f77_sval[s], byte, 0, NULL);
    if (f77_io_rd) {
        f77_rd_item(addr, f77_sty[s]);
    } else {
        v = f77_load_at(addr, f77_sty[s]);
        vhi = ex_hi;
        f77_wr_item(v, f77_sty[s], vhi);
    }
    for (k = 0; k < rank; k++) {
        idx = hi_emit(HI_LOAD, TY_INT, iv[k], -1, 0, NULL);
        idx = hi_emit(HI_ADDI, TY_INT, idx, -1, 1, NULL);
        hi_emit(HI_STORE, TY_INT, iv[k], idx, 0, NULL);
        t = hi_emit(HI_LOAD, TY_INT, trip[k], -1, 0, NULL);
        t = hi_emit(HI_ADDI, TY_INT, t, -1, -1, NULL);
        hi_emit(HI_STORE, TY_INT, trip[k], t, 0, NULL);
        hi_emit(HI_BR, TY_VOID, -1, -1, b_test[k], NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_exit[k]);
    }
}

/* One element of an output list: an expression, a character constant,
 * or an implied-DO `(items, VAR = e1, e2 [, e3])`, which becomes a real
 * loop around the item calls. */
static void f77_wr_list(void);

static void f77_wr_one(void) {
    int v;
    int vty;
    int vhi;
    int cb;
    int r;

    if (lx_t == T_SCON) {
        int sa;
        if (f77_io_rd) {
            f77_error("a character constant cannot be a READ list item");
            return;
        }
        sa = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, lex_sidx, NULL);
        cb = h_ncarg;
        h_carg[h_ncarg] = sa; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
        h_carg[h_ncarg] = f77_iconst(lex_slen); h_carg_tag[h_ncarg] = 0;
        h_ncarg = h_ncarg + 1;
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "f77_wr_a");
        h_cbase[r] = cb;
        f77_tok();
        return;
    }
    if (lx_t == T_NAME) {
        int s;
        int after;
        s = f77_sym(lex_name);
        after = (int)(lx_rte - lx_stmt);
        if (f77_srank[s] > 0 &&
            (after >= lx_stmt_len || lx_stmt[after] != '(')) {
            f77_tok();
            f77_wr_whole_array(s);
            return;
        }
    }

    /* Implied-DO: `(items, VAR = e1, e2 [, e3])`.  The loop control sits
     * AFTER the items it governs, so the control spec is parsed first
     * from its own offset, the loop opened, and the scanner then rewound
     * to the items -- which are emitted straight into the body block.
     * Rewinding is safe here precisely because nothing has been emitted
     * for the items yet. */
    {
        int open_off;
        int close_off;
        int eq_off;
        int items_off;
        int ctl_off;
        open_off = (int)(lx_rts - lx_stmt);
        if (lx_t == T_LP &&
            f77_implied_do_spans(open_off, &close_off, &eq_off, &items_off, &ctl_off)) {
            int sv;
            int m1, m2, m3, t1, t2, t3;
            int trip;
            int b_test, b_body, b_exit, c;

            f77_scan_from(ctl_off);
            if (lx_t != T_NAME) { f77_error("implied-DO needs a control variable"); return; }
            sv = f77_sym(lex_name);
            f77_tok();
            if (lx_t != T_ASSIGN) { f77_error("expected = in implied-DO"); return; }
            f77_tok();
            m1 = f77_expr(); m1 = f77_cvt(m1, &ex_hi, ex_ty, TY_INT);
            if (lx_t != T_COMMA) { f77_error("expected , in implied-DO"); return; }
            f77_tok();
            m2 = f77_expr(); m2 = f77_cvt(m2, &ex_hi, ex_ty, TY_INT);
            if (lx_t == T_COMMA) {
                f77_tok();
                m3 = f77_expr(); m3 = f77_cvt(m3, &ex_hi, ex_ty, TY_INT);
            } else {
                m3 = f77_iconst(1);
            }

            f77_sstored[sv] = 1;
            hi_emit(HI_STORE, TY_INT, f77_sval[sv], m1, 0, NULL);
            t1 = hi_emit(HI_SUB, TY_INT, m2, m1, 0, NULL);
            t2 = hi_emit(HI_ADD, TY_INT, t1, m3, 0, NULL);
            t3 = hi_emit(HI_DIV, TY_INT, t2, m3, 0, NULL);
            f77_frame = f77_frame + 4;
            trip = hi_emit(HI_ALLOCA, TY_INT, -1, -1, 0 - f77_frame, NULL);
            hl_ainst[hl_nalloca] = trip;
            hl_aoff[hl_nalloca] = 0 - f77_frame;
            hl_nalloca = hl_nalloca + 1;
            hi_emit(HI_STORE, TY_INT, trip, t3, 0, NULL);

            b_test = hir_new_block();
            b_body = hir_new_block();
            b_exit = hir_new_block();
            f77_goto_blk(b_test);
            f77_begin_blk(b_test);
            c = hi_emit(HI_LOAD, TY_INT, trip, -1, 0, NULL);
            c = hi_emit(HI_SGT, TY_INT, c, f77_iconst(0), 0, NULL);
            hi_emit(HI_BRC, TY_VOID, c, b_body, b_exit, NULL);
            f77_cur_blk_live = 0;
            f77_begin_blk(b_body);

            /* Rewind to the items and emit them inside the body. */
            f77_scan_from(items_off);
            for (;;) {
                f77_wr_one();
                if (lx_t != T_COMMA) break;
                if ((int)(lx_rts - lx_stmt) >= ctl_off - 1) break;
                f77_tok();
                if ((int)(lx_rts - lx_stmt) >= ctl_off) break;
            }

            {
                int iv;
                iv = hi_emit(HI_LOAD, TY_INT, f77_sval[sv], -1, 0, NULL);
                iv = hi_emit(HI_ADD, TY_INT, iv, m3, 0, NULL);
                hi_emit(HI_STORE, TY_INT, f77_sval[sv], iv, 0, NULL);
                iv = hi_emit(HI_LOAD, TY_INT, trip, -1, 0, NULL);
                iv = hi_emit(HI_ADDI, TY_INT, iv, -1, -1, NULL);
                hi_emit(HI_STORE, TY_INT, trip, iv, 0, NULL);
                hi_emit(HI_BR, TY_VOID, -1, -1, b_test, NULL);
            }
            f77_cur_blk_live = 0;
            f77_begin_blk(b_exit);

            /* Continue after the implied-DO's closing paren. */
            f77_scan_from(close_off);
            return;
        }
    }

    if (f77_io_rd) {
        int a;
        a = f77_rd_target(&vty);
        f77_rd_item(a, vty);
        return;
    }

    v = f77_expr();
    vty = ex_ty;
    vhi = ex_hi;
    f77_wr_item(v, vty, vhi);
}

static void f77_wr_list(void) {
    if (lx_t == T_EOF) return;
    for (;;) {
        f77_wr_one();
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
}

/* Emit the f77_wr_begin call: unit number, plus the format string or a
 * null pointer for list-directed (FMT=*) output. */
static void f77_wr_begin(int unit_val, int fmt_label) {
    int cb;
    int r;
    int fs;
    int fa;
    cb = h_ncarg;
    h_carg[h_ncarg] = unit_val; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    if (fmt_label >= 0) {
        fs = f77_find_format(fmt_label);
        if (fs < 0) { f77_error("no FORMAT statement with that label"); fs = 0; }
        fa = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, fs, NULL);
    } else {
        fa = f77_iconst(0);
    }
    h_carg[h_ncarg] = fa; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "f77_wr_begin");
    h_cbase[r] = cb;
}

static void f77_wr_finish(void) {
    int r;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 0, "f77_wr_end");
    h_cbase[r] = h_ncarg;
}

/* WRITE (unit, fmt) list  --  unit and fmt may each be `*`. */
static void f77_stmt_write(int skip) {
    int unit_val;
    int fmt_label;
    f77_scan_from(skip);
    if (lx_t != T_LP) { f77_error("WRITE needs (unit, format)"); return; }
    f77_tok();
    if (lx_t == T_STAR) { unit_val = f77_iconst(6); f77_tok(); }
    else { unit_val = f77_expr(); unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT); }
    fmt_label = -1;
    if (lx_t == T_COMMA) {
        f77_tok();
        if (lx_t == T_STAR) f77_tok();
        else if (lx_t == T_ICON) { fmt_label = lex_ival; f77_tok(); }
        else { f77_error("only a FORMAT label or * is supported"); return; }
    }
    if (lx_t != T_RP) { f77_error("expected ) after WRITE control list"); return; }
    f77_tok();
    f77_wr_begin(unit_val, fmt_label);
    f77_wr_list();
    f77_wr_finish();
}

/* PRINT fmt, list  --  always unit 6. */
static void f77_stmt_print(int skip) {
    int fmt_label;
    f77_scan_from(skip);
    fmt_label = -1;
    if (lx_t == T_STAR) f77_tok();
    else if (lx_t == T_ICON) { fmt_label = lex_ival; f77_tok(); }
    else { f77_error("PRINT needs a FORMAT label or *"); return; }
    if (lx_t == T_COMMA) f77_tok();
    f77_wr_begin(f77_iconst(6), fmt_label);
    f77_wr_list();
    f77_wr_finish();
}

/* READ (unit, fmt [, END=label]) list  --  or  READ fmt, list.
 *
 * The unit may be `*` or 5 (stdin); fmt is a FORMAT label or `*` for
 * list-directed.  END= names the statement to branch to at end of
 * file, checked when the READ begins -- which is what the standard
 * read-until-EOF loop exercises.  ERR= and IOSTAT= are refused
 * honestly, as is unformatted `READ (u)`. */
static void f77_stmt_read(int skip) {
    int unit_val;
    int fmt_label;
    int end_label;
    int cb;
    int r;
    int fs;
    int fa;
    int c;

    f77_scan_from(skip);
    fmt_label = -1;
    end_label = -1;

    if (lx_t == T_LP) {
        f77_tok();
        if (lx_t == T_STAR) { unit_val = f77_iconst(5); f77_tok(); }
        else { unit_val = f77_expr(); unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT); }
        if (lx_t != T_COMMA) {
            f77_error("unformatted READ is not supported -- needs (unit, format)");
            return;
        }
        f77_tok();
        if (lx_t == T_STAR) f77_tok();
        else if (lx_t == T_ICON) { fmt_label = lex_ival; f77_tok(); }
        else { f77_error("only a FORMAT label or * is supported"); return; }
        while (lx_t == T_COMMA) {
            f77_tok();
            if (lx_t == T_NAME && strcmp(lex_name, "END") == 0) {
                f77_tok();
                if (lx_t != T_ASSIGN) { f77_error("END needs =label"); return; }
                f77_tok();
                if (lx_t != T_ICON) { f77_error("END= needs a statement label"); return; }
                end_label = lex_ival;
                f77_tok();
            } else {
                f77_error("only END= is supported in the READ control list");
                return;
            }
        }
        if (lx_t != T_RP) { f77_error("expected ) after READ control list"); return; }
        f77_tok();
    } else {
        /* READ fmt, list  --  always unit 5. */
        unit_val = f77_iconst(5);
        if (lx_t == T_STAR) f77_tok();
        else if (lx_t == T_ICON) { fmt_label = lex_ival; f77_tok(); }
        else { f77_error("READ needs a FORMAT label or *"); return; }
        if (lx_t == T_COMMA) f77_tok();
    }

    cb = h_ncarg;
    h_carg[h_ncarg] = unit_val; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    if (fmt_label >= 0) {
        fs = f77_find_format(fmt_label);
        if (fs < 0) { f77_error("no FORMAT statement with that label"); fs = 0; }
        fa = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, fs, NULL);
    } else {
        fa = f77_iconst(0);
    }
    h_carg[h_ncarg] = fa; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = f77_iconst(end_label >= 0 ? 1 : 0);
    h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "f77_rd_begin");
    h_cbase[r] = cb;

    if (end_label >= 0) {
        int b_cont;
        b_cont = hir_new_block();
        c = hi_emit(HI_SNE, TY_INT, r, f77_iconst(0), 0, NULL);
        hi_emit(HI_BRC, TY_VOID, c, f77_label_blk(end_label), b_cont, NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_cont);
    }

    f77_io_rd = 1;
    f77_wr_list();
    f77_io_rd = 0;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 0, "f77_rd_end");
    h_cbase[r] = h_ncarg;
}

/* --- OPEN / CLOSE / REWIND -------------------------------------------- */

/* The unit in an OPEN/CLOSE control list: an integer constant, a
 * scalar variable, a PARAMETER, or UNIT=expr.  A leading NAME must be
 * peeked past to tell a keyword item from a positional unit, so a
 * name-led unit is restricted to a bare scalar -- OPEN (NU+1, ...) is
 * not worth the rewind machinery it would need. */
static int f77_io_unit_after_name(char *nm) {
    int s;
    s = f77_sym(nm);
    if (f77_sparam[s]) return f77_iconst(f77_spival[s]);
    return f77_load_sym(s);
}

/* OPEN (unit, FILE='name' [, STATUS='...']) -- FILE must be a
 * character constant until CHARACTER variables exist.  Everything
 * else in the standard's control list is refused honestly. */
static void f77_stmt_open(int skip) {
    int unit_val;
    int have_unit;
    int file_sidx; int file_slen;
    int stat_sidx; int stat_slen;
    char kw[F77_MAX_NAME];
    int cb;
    int r;
    int a;

    f77_scan_from(skip);
    have_unit = 0;
    unit_val = -1;
    file_sidx = -1; file_slen = 0;
    stat_sidx = -1; stat_slen = 0;
    if (lx_t != T_LP) { f77_error("OPEN needs a control list"); return; }
    f77_tok();
    for (;;) {
        if (lx_t == T_NAME) {
            strcpy(kw, lex_name);
            f77_tok();
            if (lx_t == T_ASSIGN) {
                f77_tok();
                if (strcmp(kw, "UNIT") == 0) {
                    unit_val = f77_expr();
                    unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
                    have_unit = 1;
                } else if (strcmp(kw, "FILE") == 0) {
                    if (lx_t != T_SCON) {
                        f77_error("FILE= needs a character constant (CHARACTER variables are not supported)");
                        return;
                    }
                    file_sidx = lex_sidx;
                    file_slen = lex_slen;
                    f77_tok();
                } else if (strcmp(kw, "STATUS") == 0) {
                    if (lx_t != T_SCON) {
                        f77_error("STATUS= needs a character constant");
                        return;
                    }
                    stat_sidx = lex_sidx;
                    stat_slen = lex_slen;
                    f77_tok();
                } else {
                    f77_error("only UNIT=, FILE= and STATUS= are supported in OPEN");
                    return;
                }
            } else if (!have_unit) {
                unit_val = f77_io_unit_after_name(kw);
                unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
                have_unit = 1;
            } else {
                f77_error("bad OPEN control list");
                return;
            }
        } else if (!have_unit) {
            unit_val = f77_expr();
            unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
            have_unit = 1;
        } else {
            f77_error("bad OPEN control list");
            return;
        }
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) { f77_error("expected ) after OPEN control list"); return; }
    f77_tok();
    if (!have_unit) { f77_error("OPEN needs a unit"); return; }
    if (file_sidx < 0) { f77_error("OPEN needs FILE="); return; }

    cb = h_ncarg;
    h_carg[h_ncarg] = unit_val; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    a = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, file_sidx, NULL);
    h_carg[h_ncarg] = a; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = f77_iconst(file_slen); h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    if (stat_sidx >= 0) a = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, stat_sidx, NULL);
    else a = f77_iconst(0);
    h_carg[h_ncarg] = a; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = f77_iconst(stat_slen); h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 5, "f77_open");
    h_cbase[r] = cb;
}

/* CLOSE (unit [, STATUS='KEEP'|'DELETE']) */
static void f77_stmt_close(int skip) {
    int unit_val;
    int have_unit;
    int stat_sidx; int stat_slen;
    char kw[F77_MAX_NAME];
    int cb;
    int r;
    int a;

    f77_scan_from(skip);
    have_unit = 0;
    unit_val = -1;
    stat_sidx = -1; stat_slen = 0;
    if (lx_t != T_LP) { f77_error("CLOSE needs a control list"); return; }
    f77_tok();
    for (;;) {
        if (lx_t == T_NAME) {
            strcpy(kw, lex_name);
            f77_tok();
            if (lx_t == T_ASSIGN) {
                f77_tok();
                if (strcmp(kw, "UNIT") == 0) {
                    unit_val = f77_expr();
                    unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
                    have_unit = 1;
                } else if (strcmp(kw, "STATUS") == 0) {
                    if (lx_t != T_SCON) {
                        f77_error("STATUS= needs a character constant");
                        return;
                    }
                    stat_sidx = lex_sidx;
                    stat_slen = lex_slen;
                    f77_tok();
                } else {
                    f77_error("only UNIT= and STATUS= are supported in CLOSE");
                    return;
                }
            } else if (!have_unit) {
                unit_val = f77_io_unit_after_name(kw);
                unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
                have_unit = 1;
            } else {
                f77_error("bad CLOSE control list");
                return;
            }
        } else if (!have_unit) {
            unit_val = f77_expr();
            unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
            have_unit = 1;
        } else {
            f77_error("bad CLOSE control list");
            return;
        }
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) { f77_error("expected ) after CLOSE control list"); return; }
    f77_tok();
    if (!have_unit) { f77_error("CLOSE needs a unit"); return; }

    cb = h_ncarg;
    h_carg[h_ncarg] = unit_val; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    if (stat_sidx >= 0) a = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, stat_sidx, NULL);
    else a = f77_iconst(0);
    h_carg[h_ncarg] = a; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = f77_iconst(stat_slen); h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "f77_close");
    h_cbase[r] = cb;
}

/* REWIND u  --  or REWIND (u). */
static void f77_stmt_rewind(int skip) {
    int unit_val;
    int paren;
    int cb;
    int r;

    f77_scan_from(skip);
    paren = 0;
    if (lx_t == T_LP) { paren = 1; f77_tok(); }
    unit_val = f77_expr();
    unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
    if (paren) {
        if (lx_t != T_RP) { f77_error("expected ) after REWIND unit"); return; }
        f77_tok();
    }
    cb = h_ncarg;
    h_carg[h_ncarg] = unit_val; h_carg_tag[h_ncarg] = 0; h_ncarg = h_ncarg + 1;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "f77_rewind");
    h_cbase[r] = cb;
}

/* Is this statement part of the declaration part (so the scalar dummy
 * copies must not be emitted yet)? */
static int f77_stmt_is_declaration(int cls) {
    if (cls != S_KEYWORD) return 0;
    if (f77_starts("INTEGER")) return 1;
    if (f77_starts("REAL")) return 1;
    if (f77_starts("DOUBLEPRECISION")) return 1;
    if (f77_starts("LOGICAL")) return 1;
    if (f77_starts("CHARACTER")) return 1;
    if (f77_starts("COMPLEX")) return 1;
    if (f77_starts("DIMENSION")) return 1;
    if (f77_starts("COMMON")) return 1;
    if (f77_starts("EQUIVALENCE")) return 1;
    if (f77_starts("BLOCKDATA")) return 1;
    if (f77_starts("EXTERNAL")) return 1;
    if (f77_starts("INTRINSIC")) return 1;
    if (f77_starts("SAVE")) return 1;
    if (f77_starts("DATA")) return 1;
    if (f77_starts("PARAMETER")) return 1;
    if (f77_starts("IMPLICIT")) return 1;
    if (f77_starts("FORMAT")) return 1;
    if (f77_starts("PROGRAM")) return 1;
    if (f77_starts("SUBROUTINE")) return 1;
    if (f77_unit_header_ty() >= 0) return 1;
    return 0;
}

/* Dispatch one statement.  Called once per assembled statement. */
static void f77_statement(void) {
    int cls;
    int n;
    int c;
    int b_then;
    int b_else;
    int b_end;
    int lab;

    /* A labelled statement starts (or continues into) that label's
     * block, so GOTOs and DO terminals both land correctly. */
    if (lx_stmt_label >= 0) {
        int b;
        b = f77_label_blk(lx_stmt_label);
        f77_goto_blk(b);
        f77_begin_blk(b);
    }

    cls = f77_classify();

    /* Declarations must all precede executable statements, so the first
     * executable one is where every dummy's type is finally known --
     * and therefore where the scalar copies can be emitted. */
    if (f77_decls_open && !f77_stmt_is_declaration(cls)) f77_emit_copyin();

    if (cls == S_ASSIGN)   { f77_stmt_assign(); goto done; }
    if (cls == S_DO)       { f77_open_do();     goto done; }

    if (cls == S_IF_LOGIC &&
        f77_if_tail < lx_stmt_len &&
        lx_stmt[f77_if_tail] >= '0' && lx_stmt[f77_if_tail] <= '9') {
        /* Arithmetic IF: IF (e) l1, l2, l3 -- branch on the sign of e.
         * No statement can begin with a digit, so a digit after the
         * closing paren can only be the first label.  Both compares
         * are emitted up front in the current block (which dominates
         * the middle one), then two conditional branches chain. */
        int v;
        int vty;
        int vhi;
        int c_neg;
        int c_eq;
        int l1, l2, l3;
        int b_mid;
        f77_scan_from(2);
        v = f77_expr();          /* parses the (e) as a parenthesised primary */
        vty = ex_ty;
        vhi = ex_hi;
        if (lx_t != T_ICON) { f77_error("arithmetic IF needs three labels"); goto done; }
        l1 = lex_ival; f77_tok();
        if (lx_t != T_COMMA) { f77_error("arithmetic IF needs three labels"); goto done; }
        f77_tok();
        if (lx_t != T_ICON) { f77_error("arithmetic IF needs three labels"); goto done; }
        l2 = lex_ival; f77_tok();
        if (lx_t != T_COMMA) { f77_error("arithmetic IF needs three labels"); goto done; }
        f77_tok();
        if (lx_t != T_ICON) { f77_error("arithmetic IF needs three labels"); goto done; }
        l3 = lex_ival; f77_tok();

        if (vty == TY_DOUBLE) {
            int z;
            int zhi;
            z = f77_dconst(0.0, &zhi);
            c_neg = f77_fp64_call2("__fp64_lt", v, vhi, z, zhi, NULL);
            c_eq = f77_fp64_call2("__fp64_eq", v, vhi, z, zhi, NULL);
        } else if (vty == TY_FLOAT) {
            int z;
            z = f77_rconst(0.0);
            c_neg = hi_emit(HI_FLT, TY_INT, v, z, 0, NULL);
            c_eq = hi_emit(HI_FEQ, TY_INT, v, z, 0, NULL);
        } else {
            c_neg = hi_emit(HI_SLT, TY_INT, v, f77_iconst(0), 0, NULL);
            c_eq = hi_emit(HI_SEQ, TY_INT, v, f77_iconst(0), 0, NULL);
        }
        b_mid = hir_new_block();
        hi_emit(HI_BRC, TY_VOID, c_neg, f77_label_blk(l1), b_mid, NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_mid);
        hi_emit(HI_BRC, TY_VOID, c_eq, f77_label_blk(l2), f77_label_blk(l3), NULL);
        f77_cur_blk_live = 0;
        goto done;
    }

    if (cls == S_IF_LOGIC) {
        /* IF (e) stmt -- execute stmt only when e is true. */
        f77_scan_from(2);
        c = f77_expr();
        b_then = hir_new_block();
        b_end = hir_new_block();
        hi_emit(HI_BRC, TY_VOID, c, b_then, b_end, NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_then);
        {
            /* Re-classify the tail as a statement in its own right by
             * shifting it to the front of the buffer. */
            int i;
            int tail;
            int save_label;
            tail = f77_if_tail;
            save_label = lx_stmt_label;
            i = 0;
            while (tail + i < lx_stmt_len) { lx_stmt[i] = lx_stmt[tail + i]; i = i + 1; }
            lx_stmt_len = i;
            lx_stmt[i] = 0;
            lx_stmt_label = -1;
            f77_statement();
            lx_stmt_label = save_label;
        }
        f77_goto_blk(b_end);
        f77_begin_blk(b_end);
        goto done;
    }

    if (cls == S_IF_BLOCK) {
        f77_scan_from(2);
        c = f77_expr();
        b_then = hir_new_block();
        b_else = hir_new_block();
        b_end = hir_new_block();
        hi_emit(HI_BRC, TY_VOID, c, b_then, b_else, NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_then);
        if (ctl_n >= F77_MAX_CTL) { f77_error("control nesting too deep"); goto done; }
        ctl_kind[ctl_n] = F77_CTL_IF;
        ctl_else[ctl_n] = b_else;
        ctl_exit[ctl_n] = b_end;
        ctl_n = ctl_n + 1;
        goto done;
    }

    /* --- keyword statements --- */

    if (f77_starts("PROGRAM")) goto done;      /* name is documentation */

    if (f77_starts("FORMAT")) goto done;       /* collected in phase 1 */

    if ((n = f77_starts("WRITE")) != 0) { f77_stmt_write(n); goto done; }
    if ((n = f77_starts("PRINT")) != 0) { f77_stmt_print(n); goto done; }
    if ((n = f77_starts("READ")) != 0)  { f77_stmt_read(n);  goto done; }
    if ((n = f77_starts("OPEN")) != 0)  { f77_stmt_open(n);  goto done; }
    if ((n = f77_starts("CLOSE")) != 0) { f77_stmt_close(n); goto done; }
    if ((n = f77_starts("REWIND")) != 0) { f77_stmt_rewind(n); goto done; }
    if (f77_starts("BACKSPACE")) { f77_error("BACKSPACE is not supported"); goto done; }
    if (f77_starts("EQUIVALENCE")) { f77_error("EQUIVALENCE is not supported"); goto done; }
    if (f77_starts("CHARACTER")) { f77_error("CHARACTER is not supported"); goto done; }
    if (f77_starts("COMPLEX")) { f77_error("COMPLEX is not supported"); goto done; }
    if (f77_starts("BLOCKDATA")) { f77_error("BLOCK DATA is not supported"); goto done; }
    if (f77_starts("EXTERNAL")) { f77_error("EXTERNAL is not supported"); goto done; }
    if (f77_starts("INTRINSIC")) { f77_error("INTRINSIC is not supported"); goto done; }
    if (f77_starts("ASSIGN")) { f77_error("ASSIGN / assigned GOTO is not supported"); goto done; }
    if (f77_starts("PAUSE")) { f77_error("PAUSE is not supported"); goto done; }
    if (f77_starts("ENTRY")) { f77_error("ENTRY is not supported"); goto done; }


    if (f77_starts("SUBROUTINE") || f77_unit_header_ty() >= 0) {
        /* The header was consumed by f77_bind_unit() before the body
         * was parsed; seeing it here means it is this unit's own
         * header, which needs no code. */
        goto done;
    }

    if (f77_starts("RETURN")) {
        f77_emit_return();
        goto done;
    }

    if ((n = f77_starts("CALL")) != 0) {
        f77_stmt_call(n);
        goto done;
    }

    if (f77_starts("CONTINUE")) goto done;
    if (f77_starts("INTEGER"))  { f77_stmt_decl(TY_INT,   7); goto done; }
    if (f77_starts("LOGICAL"))  { f77_stmt_decl(TY_INT,   7); goto done; }
    if (f77_starts("DOUBLEPRECISION")) { f77_stmt_decl(TY_DOUBLE, 15); goto done; }
    if (f77_starts("DIMENSION")) { f77_stmt_decl(-1, 9); goto done; }
    if (f77_starts("REAL"))     { f77_stmt_decl(TY_FLOAT, 4); goto done; }
    if ((n = f77_starts("COMMON")) != 0) { f77_stmt_common(n); goto done; }
    if ((n = f77_starts("SAVE")) != 0)   { f77_stmt_save(n);   goto done; }
    if ((n = f77_starts("PARAMETER")) != 0) { f77_stmt_parameter(n); goto done; }
    if ((n = f77_starts("DATA")) != 0)   { f77_stmt_data(n);   goto done; }
    if ((n = f77_starts("IMPLICIT")) != 0) { f77_stmt_implicit(n); goto done; }

    if (f77_starts("ELSEIF")) {
        /* ELSE IF (e) THEN -- close the current arm, open a new test in
         * the pending else block. */
        if (ctl_n == 0 || ctl_kind[ctl_n - 1] != F77_CTL_IF) {
            f77_error("ELSE IF without IF");
            goto done;
        }
        f77_goto_blk(ctl_exit[ctl_n - 1]);
        f77_begin_blk(ctl_else[ctl_n - 1]);
        f77_scan_from(8);                       /* past "ELSEIF(" minus the ( */
        {
            int save;
            save = f77_match_paren(6);
            (void)save;
        }
        f77_scan_from(7);
        c = f77_expr();
        b_then = hir_new_block();
        b_else = hir_new_block();
        hi_emit(HI_BRC, TY_VOID, c, b_then, b_else, NULL);
        f77_cur_blk_live = 0;
        ctl_else[ctl_n - 1] = b_else;
        f77_begin_blk(b_then);
        goto done;
    }

    if (f77_starts("ELSE")) {
        if (ctl_n == 0 || ctl_kind[ctl_n - 1] != F77_CTL_IF) {
            f77_error("ELSE without IF");
            goto done;
        }
        f77_goto_blk(ctl_exit[ctl_n - 1]);
        f77_begin_blk(ctl_else[ctl_n - 1]);
        ctl_else[ctl_n - 1] = -1;
        goto done;
    }

    if (f77_starts("ENDIF")) {
        if (ctl_n == 0 || ctl_kind[ctl_n - 1] != F77_CTL_IF) {
            f77_error("ENDIF without IF");
            goto done;
        }
        ctl_n = ctl_n - 1;
        f77_goto_blk(ctl_exit[ctl_n]);
        if (ctl_else[ctl_n] >= 0) {
            /* No ELSE arm: the false edge falls straight through. */
            f77_begin_blk(ctl_else[ctl_n]);
            f77_goto_blk(ctl_exit[ctl_n]);
        }
        f77_begin_blk(ctl_exit[ctl_n]);
        goto done;
    }

    if ((n = f77_starts("GOTO")) != 0) {
        f77_scan_from(n);
        if (lx_t == T_LP) {
            /* Computed GOTO: GOTO (l1, l2, ...) [,] e -- jump to the
             * e'th label, fall through when e is outside [1, n].  One
             * unsigned compare covers both ends (e-1 underflows), and
             * the dispatch itself is a jump table, the machinery
             * switch statements already proved. */
            int targets[64];
            int nt;
            int v;
            int b_tab;
            int b_next;
            nt = 0;
            f77_tok();
            for (;;) {
                if (lx_t != T_ICON) { f77_error("computed GOTO needs statement labels"); goto done; }
                if (nt >= 64) { f77_error("too many labels in computed GOTO"); goto done; }
                targets[nt] = f77_label_blk(lex_ival);
                nt = nt + 1;
                f77_tok();
                if (lx_t != T_COMMA) break;
                f77_tok();
            }
            if (lx_t != T_RP) { f77_error("expected ) in computed GOTO"); goto done; }
            f77_tok();
            if (lx_t == T_COMMA) f77_tok();
            v = f77_expr();
            v = f77_cvt(v, &ex_hi, ex_ty, TY_INT);
            v = hi_emit(HI_ADDI, TY_INT, v, -1, -1, NULL);
            b_tab = hir_new_block();
            b_next = hir_new_block();
            c = hi_emit(HI_SGTU, TY_INT, f77_iconst(nt), v, 0, NULL);
            hi_emit(HI_BRC, TY_VOID, c, b_tab, b_next, NULL);
            f77_cur_blk_live = 0;
            f77_begin_blk(b_tab);
            hi_emit_jmptab(v, b_next, targets, nt);
            f77_cur_blk_live = 0;
            f77_begin_blk(b_next);
            goto done;
        }
        if (lx_t != T_ICON) { f77_error("GOTO needs a label (assigned GOTO is not supported)"); goto done; }
        f77_goto_blk(f77_label_blk(lex_ival));
        goto done;
    }

    if ((n = f77_starts("STOP")) != 0) {
        int v;
        int cb;
        int r;
        f77_scan_from(n);
        v = (lx_t == T_ICON) ? f77_iconst(lex_ival) : f77_iconst(0);
        if (f77_cur_blk_live) {
            /* Process exit, not a function return -- STOP in a
             * SUBROUTINE must terminate the program (GitHub #20). */
            cb = h_ncarg;
            h_carg[h_ncarg] = v;
            h_carg_tag[h_ncarg] = 0;
            h_ncarg = h_ncarg + 1;
            r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "exit");
            h_cbase[r] = cb;
            hi_emit(HI_RET, TY_INT, v, -1, 0, NULL);
        }
        f77_cur_blk_live = 0;
        goto done;
    }

    if (f77_starts("ENDFILE")) {        /* must precede the END match */
        f77_error("ENDFILE is not supported");
        goto done;
    }

    if (f77_is_unit_end()) {
        if (f77_cur_blk_live) hi_emit(HI_RET, TY_INT, f77_iconst(0), -1, 0, NULL);
        f77_cur_blk_live = 0;
        goto done;
    }

    f77_error("unrecognised statement");

done:
    /* If this statement carried a label that closes one or more DO
     * loops, close them now -- the terminal statement is part of the
     * loop body and has just been emitted. */
    if (lx_stmt_label >= 0) {
        lab = lx_stmt_label;
        f77_close_do(lab);
    }
}

#endif
