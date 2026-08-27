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
static char f77_sname[F77_MAX_SYM][F77_MAX_NAME];
static int  f77_sty[F77_MAX_SYM];      /* TY_INT / TY_FLOAT */
static int  f77_sval[F77_MAX_SYM];     /* HIR alloca */
static int  f77_nsym;

/* F77 implicit typing: I-N are INTEGER, everything else REAL. */
static int f77_implicit_ty(char *nm) {
    int c;
    c = nm[0];
    if (c >= 'I' && c <= 'N') return TY_INT;
    return TY_FLOAT;
}

static int f77_frame;   /* bytes of locals allocated so far */

static int f77_sym(char *nm) {
    int i;
    i = 0;
    while (i < f77_nsym) {
        if (strcmp(f77_sname[i], nm) == 0) return i;
        i = i + 1;
    }
    if (f77_nsym >= F77_MAX_SYM) { f77_error("too many symbols"); return 0; }
    strcpy(f77_sname[f77_nsym], nm);
    f77_sty[f77_nsym] = f77_implicit_ty(nm);
    f77_frame = f77_frame + ty_size(f77_sty[f77_nsym]);
    f77_sval[f77_nsym] = hi_emit(HI_ALLOCA, f77_sty[f77_nsym], -1, -1,
                                 0 - f77_frame, NULL);
    hl_ainst[hl_nalloca] = f77_sval[f77_nsym];
    hl_aoff[hl_nalloca] = 0 - f77_frame;
    hl_nalloca = hl_nalloca + 1;
    f77_nsym = f77_nsym + 1;
    return f77_nsym - 1;
}

/* --- statement labels ----------------------------------------------- */

#define F77_MAX_LABEL 512
static int f77_lnum[F77_MAX_LABEL];
static int f77_lblk[F77_MAX_LABEL];
static int f77_nlabel;

/* Block for a label, created on first mention so forward GOTOs work. */
static int f77_label_blk(int n) {
    int i;
    i = 0;
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

/* --- control stack (DO loops and block IFs) ------------------------- */

#define F77_CTL_DO 1
#define F77_CTL_IF 2
#define F77_MAX_CTL 64

static int ctl_kind[F77_MAX_CTL];
static int ctl_label[F77_MAX_CTL];   /* DO: terminal statement label */
static int ctl_var[F77_MAX_CTL];     /* DO: symbol index */
static int ctl_step[F77_MAX_CTL];    /* DO: HIR value of the step */
static int ctl_trip[F77_MAX_CTL];    /* DO: alloca holding the trip count */
static int ctl_test[F77_MAX_CTL];    /* DO: test block */
static int ctl_exit[F77_MAX_CTL];    /* DO/IF: block after the construct */
static int ctl_else[F77_MAX_CTL];    /* IF: pending else block */
static int ctl_n;

static void f77_ctl_reset(void) { ctl_n = 0; }

/* --- expression lowering -------------------------------------------- */

static int ex_ty;     /* type of the value most recently produced */
static int ex_hi;     /* its hi word, when ex_ty is TY_DOUBLE */

static int f77_expr(void);

/* DOUBLE PRECISION on SLOW-32 is a PAIR of 32-bit values: the lo word is
 * the expression's value and the hi word travels beside it.  Operations
 * are emitted as calls to the __fp64_* helpers, every one of which the
 * backend recognises and replaces with inline hardware FP instructions
 * (hcg_fp64_kind / hcg_fp64_emit) -- so despite looking like libcalls
 * here, no call survives to the assembly.
 *
 * The hi word must be captured immediately after each subexpression:
 * the next emission overwrites the side channel. */

static int f77_fp64_call2(char *name, int alo, int ahi, int blo, int bhi, int *rhi) {
    int cb;
    int r;
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

static int f77_iconst(int v) {
    return hi_emit(HI_ICONST, TY_INT, -1, -1, v, NULL);
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
        v = f77_primary();
        save = ex_ty;
        vhi = ex_hi;
        if (save == TY_DOUBLE) {
            int cb;
            int r;
            cb = h_ncarg;
            h_carg[h_ncarg] = v;   h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = vhi; h_ncarg = h_ncarg + 1;
            r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "__fp64_neg");
            h_cbase[r] = cb;
            ex_hi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
            ex_ty = TY_DOUBLE;
            return r;
        }
        if (save == TY_FLOAT) return hi_emit(HI_FNEG, TY_FLOAT, v, -1, 0, NULL);
        return hi_emit(HI_NEG, TY_INT, v, -1, 0, NULL);
    }
    if (lx_t == T_PLUS) { f77_tok(); return f77_primary(); }
    if (lx_t == T_NAME) {
        s = f77_sym(lex_name);
        f77_tok();
        ex_ty = f77_sty[s];
        if (ex_ty == TY_DOUBLE) {
            int addr4;
            v = hi_emit(HI_LOAD, TY_INT, f77_sval[s], -1, 0, NULL);
            addr4 = hi_emit(HI_ADDI, HL_ADDR_TY, f77_sval[s], -1, 4, NULL);
            ex_hi = hi_emit(HI_LOAD, TY_INT, addr4, -1, 0, NULL);
            return v;
        }
        return hi_emit(HI_LOAD, f77_sty[s], f77_sval[s], -1, 0, NULL);
    }
    f77_error("bad expression");
    ex_ty = TY_INT;
    return f77_iconst(0);
}

/* ** is right-associative in F77. */
static int f77_power(void) {
    int a;
    int b;
    int aty;
    int bty;
    a = f77_primary();
    aty = ex_ty;
    if (lx_t == T_POWER) {
        f77_tok();
        b = f77_power();
        bty = ex_ty;
        /* Integer**integer stays integer; anything else is REAL and
         * goes through powf.  Only the integer case is lowered inline
         * for the slice. */
        if (aty == TY_INT && bty == TY_INT) {
            f77_error("integer ** not implemented in the slice");
            ex_ty = TY_INT;
            return a;
        }
        f77_error("real ** not implemented in the slice");
        ex_ty = aty;
        return a;
    }
    ex_ty = aty;
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
}

static void f77_goto_blk(int b) {
    if (f77_cur_blk_live) hi_emit(HI_BR, TY_VOID, -1, -1, b, NULL);
    f77_cur_blk_live = 0;
}

static void f77_stmt_assign(void) {
    int s;
    int v;
    int vty;
    f77_scan_from(0);
    if (lx_t != T_NAME) { f77_error("expected variable on the left of ="); return; }
    s = f77_sym(lex_name);
    f77_tok();
    if (lx_t != T_ASSIGN) { f77_error("expected ="); return; }
    f77_tok();
    v = f77_expr();
    vty = ex_ty;
    f77_store_sym(s, v, vty, ex_hi);
}

static void f77_stmt_decl(int ty, int skip) {
    int s;
    int was;
    f77_scan_from(skip);
    for (;;) {
        if (lx_t != T_NAME) break;
        s = f77_sym(lex_name);
        was = f77_sty[s];
        f77_sty[s] = ty;          /* declaration overrides implicit typing */
        h_ty[f77_sval[s]] = ty;
        if (ty_size(ty) > ty_size(was)) {
            /* The slot was sized by implicit typing; a DOUBLE PRECISION
             * declaration needs 8 bytes, so widen it before anything
             * else is allocated below it. */
            f77_frame = f77_frame + (ty_size(ty) - ty_size(was));
            h_val[f77_sval[s]] = 0 - f77_frame;
            hl_aoff[hl_nalloca - 1] = 0 - f77_frame;
        }
        f77_tok();
        if (lx_t != T_COMMA) break;
        f77_tok();
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
    m1 = f77_expr(); m1 = f77_cvt(m1, &ex_hi, ex_ty, TY_INT);
    if (lx_t != T_COMMA) { f77_error("expected , in DO"); return; }
    f77_tok();
    m2 = f77_expr(); m2 = f77_cvt(m2, &ex_hi, ex_ty, TY_INT);
    if (lx_t == T_COMMA) {
        f77_tok();
        m3 = f77_expr(); m3 = f77_cvt(m3, &ex_hi, ex_ty, TY_INT);
    } else {
        m3 = f77_iconst(1);
    }

    /* var = m1 */
    hi_emit(HI_STORE, TY_INT, f77_sval[s], m1, 0, NULL);

    /* F77 trip count, computed once: MAX(0, (m2 - m1 + m3) / m3).
     * Doing it this way rather than testing the variable each time is
     * what makes a negative or variable step work without knowing its
     * sign at compile time, and it is what the standard specifies. */
    t1 = hi_emit(HI_SUB, TY_INT, m2, m1, 0, NULL);
    t2 = hi_emit(HI_ADD, TY_INT, t1, m3, 0, NULL);
    t3 = hi_emit(HI_DIV, TY_INT, t2, m3, 0, NULL);

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
    ctl_trip[ctl_n] = trip_alloca;
    ctl_test[ctl_n] = b_test;
    ctl_exit[ctl_n] = b_exit;
    ctl_n = ctl_n + 1;
}

/* Close every DO whose terminal label is the one just executed. */
static void f77_close_do(int lab) {
    int s;
    int v;
    int t;
    while (ctl_n > 0 && ctl_kind[ctl_n - 1] == F77_CTL_DO &&
           ctl_label[ctl_n - 1] == lab) {
        ctl_n = ctl_n - 1;
        s = ctl_var[ctl_n];
        if (f77_cur_blk_live) {
            v = hi_emit(HI_LOAD, TY_INT, f77_sval[s], -1, 0, NULL);
            v = hi_emit(HI_ADD, TY_INT, v, ctl_step[ctl_n], 0, NULL);
            hi_emit(HI_STORE, TY_INT, f77_sval[s], v, 0, NULL);
            t = hi_emit(HI_LOAD, TY_INT, ctl_trip[ctl_n], -1, 0, NULL);
            t = hi_emit(HI_ADDI, TY_INT, t, -1, -1, NULL);
            hi_emit(HI_STORE, TY_INT, ctl_trip[ctl_n], t, 0, NULL);
            hi_emit(HI_BR, TY_VOID, -1, -1, ctl_test[ctl_n], NULL);
        }
        f77_cur_blk_live = 0;
        f77_begin_blk(ctl_exit[ctl_n]);
    }
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

    if (cls == S_ASSIGN)   { f77_stmt_assign(); goto done; }
    if (cls == S_DO)       { f77_open_do();     goto done; }

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
    if (f77_starts("CONTINUE")) goto done;
    if (f77_starts("INTEGER"))  { f77_stmt_decl(TY_INT,   7); goto done; }
    if (f77_starts("LOGICAL"))  { f77_stmt_decl(TY_INT,   7); goto done; }
    if (f77_starts("DOUBLEPRECISION")) { f77_stmt_decl(TY_DOUBLE, 15); goto done; }
    if (f77_starts("REAL"))     { f77_stmt_decl(TY_FLOAT, 4); goto done; }

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
        if (lx_t != T_ICON) { f77_error("GOTO needs a label"); goto done; }
        f77_goto_blk(f77_label_blk(lex_ival));
        goto done;
    }

    if ((n = f77_starts("STOP")) != 0) {
        int v;
        f77_scan_from(n);
        v = (lx_t == T_ICON) ? f77_iconst(lex_ival) : f77_iconst(0);
        if (f77_cur_blk_live) hi_emit(HI_RET, TY_INT, v, -1, 0, NULL);
        f77_cur_blk_live = 0;
        goto done;
    }

    if (f77_starts("END")) {
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
