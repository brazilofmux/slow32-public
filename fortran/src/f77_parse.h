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

/* Append a call argument (tag: hir.h h_carg_tag; 0 a plain word).  The
 * pool grows (tables.h, TAB_HIR_CARG), so every writer comes through
 * here -- as hir_lower.h's hl_carg_push for C. */
static void f77_carg(int v, int tag) {
    hir_carg_room(1);
    h_carg[h_ncarg] = v;
    h_carg_tag[h_ncarg] = tag;
    h_ncarg = h_ncarg + 1;
}

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
 * f77_sextv[s][k] >= 0 the extent is that HIR value (computed in the
 * declaration part) and f77_sext[s][k] is meaningless. */
static int  f77_sextsym[F77_MAX_SYM][F77_MAX_RANK];   /* now: HIR value of the extent, or -1 */
static int  f77_slov[F77_MAX_SYM][F77_MAX_RANK];      /* HIR value of the lower bound, or -1 */
#define f77_sextv f77_sextsym
/* An assumed-size dummy, A(*) or A(N,*): its size is not known anywhere
 * (the * is recorded as an extent of 1). */
static int  f77_sassz[F77_MAX_SYM];
/* An automatic array (Fortran 90): a local whose bounds are not
 * constant, REAL W(MAX(M,N)) -- storage allocated when the declarations
 * close (f77_layout_static), freed at RETURN.  It had been taken for an
 * adjustable dummy's shape on a slot of one element, and LAPACK's
 * DTRSYL3 wrote its norms over the frame. */
static int  f77_sauto[F77_MAX_SYM];
static int f77_adjustable(int s) {
    int k;
    for (k = 0; k < f77_srank[s]; k++)
        if (f77_sextv[s][k] >= 0 || f77_slov[s][k] >= 0) return 1;
    return 0;
}
static int  f77_nsym;
/* fort370: NAMELIST groups of the unit (fort370 issue 4): a group's
 * name and its items' symbols, f77_nml_isym[first..first+count).  Reset
 * with the symbols at each unit (f77.c). */
#define F77_MAX_NML 64
#define F77_MAX_NMLITEM 512
static char f77_nml_gname[F77_MAX_NML][32];
static int  f77_nml_gfirst[F77_MAX_NML];
static int  f77_nml_gcount[F77_MAX_NML];
static int  f77_nml_isym[F77_MAX_NMLITEM];
static int  f77_nnml, f77_nnmlitem;
static int  f77_io_nml;        /* the group of the READ or WRITE in hand */
static int f77_nml_find(char *nm) {
    int g;
    for (g = 0; g < f77_nnml; g++) if (strcmp(f77_nml_gname[g], nm) == 0) return g;
    return -1;
}

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

static int f77_unit_entries;    /* the unit being compiled is or has an ENTRY (set by hl_func) */
static int f77_ldro_local_ok(int s);
static void f77_ldro_finalize(void) {
    int i;
    for (i = 0; i < f77_ldro_n; i++) {
        if (f77_sstored[f77_ldro_sym[i]] ||
            (!f77_sarg[f77_ldro_sym[i]] && !f77_ldro_local_ok(f77_ldro_sym[i])))
            h_ld_ro[f77_ldro_inst[i]] = 0;
    }
}
static int  f77_sgidx[F77_MAX_SYM];    /* ps_g* index once bound static */
static int  f77_sgoff[F77_MAX_SYM];    /* byte offset inside that object */

/* --- CHARACTER ---------------------------------------------------------
 *
 * A CHARACTER*n entity is n bytes of storage and nothing else: no
 * descriptor, no terminator.  A character-valued EXPRESSION is an
 * ADDRESS (the HIR value f77_primary returns) plus a LENGTH, carried
 * beside ex_ty in ex_len (an HIR int value) and ex_clen (the constant
 * when it is known at compile time, else -1).  Lengths are constant
 * everywhere except an assumed-length dummy, CHARACTER*(*), whose
 * length arrives as a hidden INTEGER argument appended after the
 * regular ones -- one per CHARACTER actual, in order, the gfortran
 * convention -- and a CHARACTER FUNCTION, which takes its result
 * buffer and length as two hidden leading arguments. */
static int  f77_slen[F77_MAX_SYM];     /* length in bytes, -1 = assumed (*) */
static int  f77_slenv[F77_MAX_SYM];    /* HIR value of the length when -1 */
static int  f77_spstr[F77_MAX_SYM];    /* CHARACTER PARAMETER: string index */
static int  f77_sexternal[F77_MAX_SYM]; /* named in EXTERNAL */
static char *f77_slink[F77_MAX_SYM];     /* a USEd module procedure's link name, or NULL */
static int  f77_sme[F77_MAX_SYM];        /* ... its module entity (a procedure, a generic), or -1 */
static int  f77_sintrinsic[F77_MAX_SYM]; /* named in INTRINSIC */
/* ALLOCATABLE (Fortran 90): the symbol's slot is a descriptor, 4 + 8*rank
 * bytes -- the storage's address, then per dimension its lower bound and
 * extent -- and every element address and bound is loaded from it.  It is
 * reached only by an ADDI of the slot (f77_desc_at), which the guard holds
 * it to. */
static int  f77_salloc[F77_MAX_SYM];
/* LOGICAL shares TY_INT with INTEGER in the backend; what tells them
 * apart for list-directed output (T/F vs a number) is this flag on the
 * symbol, carried through expressions in ex_log. */
static int  f77_slog[F77_MAX_SYM];
/* fort370: the storage of an INTEGER*2 (2) or LOGICAL*1 (1) item --
 * its value is TY_INT, as FORTRAN H computes in fullword registers, but
 * it is loaded and stored at this width (fort370 issue 10); 0 for a
 * fullword or any other type. */
static signed char f77_snar[F77_MAX_SYM];
static int  f77_imp_log[26];
static int  f77_imp_nar[26];          /* IMPLICIT INTEGER*2, LOGICAL*1 per letter */
static int  ex_log;
static int  f77_decl_log;              /* the type statement being parsed is LOGICAL */
static int  f77_imp_len[26];           /* IMPLICIT CHARACTER*n per letter */
static int  ex_len;
static int  ex_clen;
static int  f77_act_len;               /* length of the last f77_actual_addr, or -1 */
static int  f77_act_ty;                /* ... its type, +16 for a whole array, -1 a procedure (generic resolution) */

/* The unit's dummies in order (also the copy-in list), the count of
 * its incoming words, and the length of the last CHARACTER READ
 * target; declared here because the static layout reads them. */
#define F77_MAX_COPYIN 32
static int f77_ci_sym[F77_MAX_COPYIN];   /* the dummy's symbol */
static int f77_ci_addr[F77_MAX_COPYIN];  /* its original address value */
static int f77_ci_n;
static int f77_unit_nparams;
static int f77_rd_len;
static int f77_inline_depth;
static int f77_reduce(int in);
static int f77_arr_inquiry(int in);
/* The array-expression context (Fortran 90): while the right side of an
 * array assignment is parsed, inside the loops over its shape, f77_aex_k
 * is the shape's rank and f77_aex_cnt[j] the j-th loop's iteration count
 * (from 0); an array operand there -- a section or a whole array -- is
 * the element those counts reach (f77_aex_elem).  0 elsewhere. */
static int f77_aex_k;
/* WHERE (Fortran 90): the mask, evaluated once into a heap temporary of
 * its shape (its elements 0 or 1, as INTEGERs, in array element order),
 * and whether the assignments are ELSEWHERE's.  Active for the array
 * assignments of the text the WHERE is in, not a spliced callee's
 * (f77_where_depth).  -1: none. */
static int f77_where_tmp = -1;
static int f77_where_neg;
static int f77_where_depth;
static int f77_where_nt;
static int f77_where_ext[8];
static int f77_aex_cnt[8];
static int f77_aex_elem(int s, int whole);
static int f77_result_sym;          /* FUNCTION: its result variable (f77_bind_unit) */
static void f77_assign_params(int n);

/* COMPLEX: a parser-level type id (never the type of an HIR
 * instruction).  A COMPLEX value is two REAL words, re in the value and
 * im in ex_hi -- the channel DOUBLE PRECISION already uses for its hi
 * word -- so every pair site (split frame slots, load/store, return in
 * r1/r2, CALLHI) carries it with a type test added. */
#define TY_CPLX 9
/* COMPLEX*16 (DOUBLE COMPLEX): two DOUBLE PRECISION parts, real then
 * imaginary, 16 bytes.  A double is already two words here, so a
 * COMPLEX*16 value does not ride the value/ex_hi pair as COMPLEX does: in
 * an expression it is the ADDRESS of its 16 bytes (an HL_ADDR_TY value).
 * A variable or element reference yields its own address; every
 * operation reads its operands' parts and writes a fresh frame temporary
 * (f77_dcx_make), so nothing writes the bytes a pending value points at,
 * and an assignment copies.  docs/plans/fortran77.md "COMPLEX*16".  Never
 * the type of an HIR instruction: f77_dcx_guard stops a unit in which one
 * is, a value having reached code that does not handle the type. */
#define TY_DCPLX 10
/* A hexadecimal or octal constant of DATA (MIL-STD-1753 Z'1F', O'17'),
 * only ever the type of a DATA value: its bits are in f77_cx_u. */
#define TY_HEXC 11

/* fort370: the storage width of a length on INTEGER or LOGICAL --
 * INTEGER*2 a halfword, LOGICAL*1 a byte, as FORTRAN H has them (issue
 * 10); 0 for a fullword, INTEGER*4 and LOGICAL*4 among them. */
static int f77_lennar(int ty, int n, int lg) {
    if (ty != TY_INT) return 0;
    if (lg) return n == 1 ? 1 : 0;
    return n == 2 ? 2 : 0;
}

/* Element size: a CHARACTER element is its length. */
static int f77_esize(int s) {
    if (f77_snar[s]) return f77_snar[s];
    if (f77_sty[s] == TY_CHAR) return f77_slen[s] > 0 ? f77_slen[s] : 1;
    if (f77_sty[s] == TY_CPLX) return 8;
    if (f77_sty[s] == TY_DCPLX) return 16;
    return ty_size(f77_sty[s]);
}
static int f77_tysize(int ty) { return ty == TY_CPLX ? 8 : ty == TY_DCPLX ? 16 : ty_size(ty); }
/* The HIR type of a variable's storage slot: a COMPLEX is two REAL words,
 * a COMPLEX*16 four words reached by address (never promoted). */
static int f77_aty(int ty) { return ty == TY_CPLX ? TY_FLOAT : ty == TY_DCPLX ? TY_INT : ty; }

/* A length after a type, *n (an extension, as fort370 reads it): REAL*8
 * is DOUBLE PRECISION and REAL*4 takes it back, COMPLEX*16 is DOUBLE
 * COMPLEX and COMPLEX*8 takes it back; any other is the type itself
 * (INTEGER*2 and LOGICAL*1 get a fullword still).  `say` is unused since
 * COMPLEX*16 stopped being refused. */
static int f77_lenty(int ty, int n, int say) {
    (void)say;
    if ((ty == TY_FLOAT || ty == TY_DOUBLE) && (n == 4 || n == 8)) return n == 8 ? TY_DOUBLE : TY_FLOAT;
    if ((ty == TY_CPLX || ty == TY_DCPLX) && (n == 8 || n == 16)) return n == 16 ? TY_DCPLX : TY_CPLX;
    return ty;
}
static int f77_ispair(int ty) { return ty == TY_DOUBLE || ty == TY_CPLX; }

/* --- EQUIVALENCE -------------------------------------------------------
 *
 * Each statement's groups are flattened into pairs: addr(a)+oa ==
 * addr(b)+ob, with the offsets in bytes from constant subscripts (and,
 * later, substrings).  Layout resolves them at the close of the
 * declaration part with a union-find that carries offsets, then either
 * hangs the group on the COMMON block one of its members belongs to or
 * gives it a static object of its own -- storage association means
 * static storage, the same trade SAVE makes. */
#define F77_MAX_EQ 256
static int f77_eq_sa[F77_MAX_EQ];
static int f77_eq_oa[F77_MAX_EQ];
static int f77_eq_sb[F77_MAX_EQ];
static int f77_eq_ob[F77_MAX_EQ];
static int f77_eq_n;
static int f77_eq_parent[F77_MAX_SYM];   /* union-find; -1 = not involved */
static int f77_eq_delta[F77_MAX_SYM];    /* addr(s) = addr(parent) + delta */

/* PARAMETER named constants: no storage, value folded at parse time.
 * The value is kept in both domains and converted per the symbol's
 * type at each use. */
static int    f77_sparam[F77_MAX_SYM];
static int    f77_spival[F77_MAX_SYM];
static double f77_spdval[F77_MAX_SYM];
static double f77_spdval2[F77_MAX_SYM];  /* COMPLEX PARAMETER: imaginary part */

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

/* --- statement functions ---------------------------------------------
 *
 * NAME(d1, d2, ...) = expr in the declaration part.  The definition is
 * kept as TEXT (with its blank-boundary flags), and every reference
 * re-lexes it: the actuals are evaluated into fresh temporaries, each
 * dummy name is bound to its temporary for the duration (f77_sym
 * consults the bindings first), and the expression is parsed in place
 * with the scanner pointed at the saved text -- the same checkpoint and
 * restore the inliner uses, minus the scope push, because every other
 * name in the expression must still resolve to the unit's variables. */
#define F77_MAX_STF      64
#define F77_MAX_STF_ARGS 16
#define F77_STF_POOL     16384
static int  f77_stf_sym[F77_MAX_STF];
static int  f77_stf_nargs[F77_MAX_STF];
static char f77_stf_arg[F77_MAX_STF][F77_MAX_STF_ARGS][F77_MAX_NAME];
static int  f77_stf_argty[F77_MAX_STF][F77_MAX_STF_ARGS];
static int  f77_stf_arglen[F77_MAX_STF][F77_MAX_STF_ARGS];   /* a CHARACTER dummy's length */
static int  f77_stf_text[F77_MAX_STF];
static int  f77_stf_len[F77_MAX_STF];
static int  f77_stf_n;
static char f77_stf_pool[F77_STF_POOL];
static unsigned char f77_stf_pbrk[F77_STF_POOL];
static int  f77_stf_pool_n;
#define F77_MAX_STFB 64
static char f77_stfb_name[F77_MAX_STFB][F77_MAX_NAME];
static int  f77_stfb_sym[F77_MAX_STFB];
static int  f77_stfb_n;

/* Names whose ADDRESS leaves the unit -- READ list items and actual
 * arguments -- collected per unit in phase 1 (over-approximated from
 * the text: any identifier standing alone inside the parentheses that
 * follow an identifier).  A DOUBLE PRECISION or COMPLEX scalar on this
 * list keeps its two words in ONE frame slot: the split lo/hi slots
 * are promotable only while nothing writes them behind the compiler's
 * back, and the dead-ADDI marker that was meant to say "address taken"
 * is removed by DCE before the next promotion pass runs -- READ into a
 * split double returned the value from before the READ. */
#define F77_MAX_ESC 8192
static int  f77_cur_unit;              /* defined with the FORMAT tables */
static char f77_esc_name[F77_MAX_ESC][F77_MAX_NAME];
static int  f77_esc_unit[F77_MAX_ESC];
static int  f77_nesc;

static void f77_esc_add(int unit, char *nm, int len) {
    int i;
    if (len <= 0 || len >= F77_MAX_NAME || f77_nesc >= F77_MAX_ESC) return;
    for (i = 0; i < f77_nesc; i++)
        if (f77_esc_unit[i] == unit && strncmp(f77_esc_name[i], nm, len) == 0 &&
            f77_esc_name[i][len] == 0) return;
    memcpy(f77_esc_name[f77_nesc], nm, len);
    f77_esc_name[f77_nesc][len] = 0;
    f77_esc_unit[f77_nesc] = unit;
    f77_nesc = f77_nesc + 1;
}

static int f77_name_escapes(int unit, char *nm) {
    int i;
    for (i = 0; i < f77_nesc; i++)
        if (f77_esc_unit[i] == unit && strcmp(f77_esc_name[i], nm) == 0) return 1;
    return 0;
}

/* Phase-1 scan of one assembled statement for escaping names. */
static void f77_esc_scan(int unit) {
    int i;
    int j;
    int k;
    int depth;
    /* READ list items: after the control list, or after `fmt,` */
    if (strncmp(lx_stmt, "READ", 4) == 0) {
        i = 4;
        if (i < lx_stmt_len && lx_stmt[i] == '(') {
            depth = 0;
            while (i < lx_stmt_len) {
                if (lx_stmt[i] == '(') depth = depth + 1;
                else if (lx_stmt[i] == ')') { depth = depth - 1; if (depth == 0) { i = i + 1; break; } }
                i = i + 1;
            }
        } else {
            while (i < lx_stmt_len && lx_stmt[i] != ',') i = i + 1;
            if (i < lx_stmt_len) i = i + 1;
        }
        /* items at depth 0, separated by commas */
        while (i < lx_stmt_len) {
            j = i;
            if (lx_isalpha(lx_stmt[j])) {
                while (j < lx_stmt_len && (lx_isalpha(lx_stmt[j]) || lx_isdigit(lx_stmt[j]))) j = j + 1;
                if (j >= lx_stmt_len || lx_stmt[j] == ',') f77_esc_add(unit, lx_stmt + i, j - i);
            }
            /* skip to the next top-level comma */
            depth = 0;
            while (i < lx_stmt_len) {
                if (lx_stmt[i] == '\'') { i = i + 1; while (i < lx_stmt_len && lx_stmt[i] != '\'') i = i + 1; }
                else if (lx_stmt[i] == '(') depth = depth + 1;
                else if (lx_stmt[i] == ')') depth = depth - 1;
                else if (lx_stmt[i] == ',' && depth == 0) break;
                i = i + 1;
            }
            if (i < lx_stmt_len) i = i + 1;
        }
    }
    /* NAMELIST /G/ item, item: the runtime stores through the items'
     * addresses (fort370 issue 4); the group names between slashes are
     * not variables */
    if (strncmp(lx_stmt, "NAMELIST", 8) == 0) {
        int slashes;
        slashes = 0;
        i = 8;
        while (i < lx_stmt_len) {
            if (lx_stmt[i] == '/') { slashes = slashes + 1; i = i + 1; continue; }
            if (lx_isalpha(lx_stmt[i])) {
                j = i;
                while (j < lx_stmt_len && (lx_isalpha(lx_stmt[j]) || lx_isdigit(lx_stmt[j]))) j = j + 1;
                if (slashes % 2 == 0) f77_esc_add(unit, lx_stmt + i, j - i);
                i = j;
                continue;
            }
            i = i + 1;
        }
        return;
    }
    /* IDENT( item, item ) anywhere: a call or a subscript */
    i = 0;
    while (i < lx_stmt_len) {
        if (lx_stmt[i] == '\'') { i = i + 1; while (i < lx_stmt_len && lx_stmt[i] != '\'') i = i + 1; i = i + 1; continue; }
        if (lx_stmt[i] == '(' && i > 0 && (lx_isalpha(lx_stmt[i - 1]) || lx_isdigit(lx_stmt[i - 1]))) {
            j = i + 1;
            for (;;) {
                k = j;
                if (k < lx_stmt_len && lx_isalpha(lx_stmt[k])) {
                    while (k < lx_stmt_len && (lx_isalpha(lx_stmt[k]) || lx_isdigit(lx_stmt[k]))) k = k + 1;
                    if (k < lx_stmt_len && (lx_stmt[k] == ',' || lx_stmt[k] == ')'))
                        f77_esc_add(unit, lx_stmt + j, k - j);
                }
                /* advance to the next top-level comma or the close */
                depth = 0;
                while (j < lx_stmt_len) {
                    if (lx_stmt[j] == '\'') { j = j + 1; while (j < lx_stmt_len && lx_stmt[j] != '\'') j = j + 1; }
                    else if (lx_stmt[j] == '(') depth = depth + 1;
                    else if (lx_stmt[j] == ')') { if (depth == 0) break; depth = depth - 1; }
                    else if (lx_stmt[j] == ',' && depth == 0) break;
                    j = j + 1;
                }
                if (j >= lx_stmt_len || lx_stmt[j] == ')') break;
                j = j + 1;
            }
        }
        i = i + 1;
    }
}

/* ASSIGN labels, collected per unit in phase 1 so an assigned GOTO
 * without a label list knows its candidates. */
#define F77_MAX_ASG 256
static int f77_asg_lab[F77_MAX_ASG];
static int f77_asg_unit[F77_MAX_ASG];
static int f77_nasg;

/* A name's symbol in scope from `base`, or -1: looked up, not made. */
static int f77_sym_at(char *nm, int base) {
    int i;
    i = base;
    while (i < f77_nsym) {
        if (strcmp(f77_sname[i], nm) == 0) return i;
        i = i + 1;
    }
    return -1;
}

/* a type's name, for messages */
static char *f77_tyname(int ty, int lg) {
    if (ty == TY_INT) return lg ? "LOGICAL" : "INTEGER";
    if (ty == TY_FLOAT) return "REAL";
    if (ty == TY_DOUBLE) return "DOUBLE PRECISION";
    if (ty == TY_CPLX) return "COMPLEX";
    if (ty == TY_DCPLX) return "COMPLEX*16";
    if (ty == TY_CHAR) return "CHARACTER";
    return "untyped";
}

/* Under IMPLICIT NONE a function reference needs its name typed in the
 * referencing unit -- an external, a dummy procedure or a FUNCTION of this
 * file alike (F77 needs it of every function name; gfortran: "has no
 * IMPLICIT type").  s < 0: the name was never declared here. */
static void f77_need_type(int s, char *nm) {
    if (f77_imp_none && (s < 0 || !f77_styped[s])) {
        static char msg[F77_MAX_NAME + 40];
        strcpy(msg, nm);
        strcat(msg, " is not declared (IMPLICIT NONE)");
        f77_error(msg);
    }
}

static int f77_is_cname(char *nm);
static int f77_sym(char *nm) {
    int i;
    i = f77_stfb_n;
    while (i > 0) {
        i = i - 1;
        if (strcmp(f77_stfb_name[i], nm) == 0) return f77_stfb_sym[i];
    }
    i = f77_scope_base;
    while (i < f77_nsym) {
        if (strcmp(f77_sname[i], nm) == 0) return i;
        i = i + 1;
    }
    if (f77_nsym >= F77_MAX_SYM) { f77_error("too many symbols"); return 0; }
    if (f77_inline_depth == 0 && f77_is_cname(nm)) {
        static char cmsg[F77_MAX_NAME + 40];
        strcpy(cmsg, nm);
        strcat(cmsg, " is a construct name, not a variable");
        f77_error(cmsg);
    }
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
    f77_sgoff[f77_nsym] = 0;
    f77_slen[f77_nsym] = 1;
    f77_slenv[f77_nsym] = -1;
    f77_spstr[f77_nsym] = -1;
    f77_sexternal[f77_nsym] = 0;
    f77_sauto[f77_nsym] = 0;
    f77_slink[f77_nsym] = NULL;
    f77_sme[f77_nsym] = -1;
    f77_salloc[f77_nsym] = 0;
    f77_sintrinsic[f77_nsym] = 0;
    f77_slog[f77_nsym] = 0;
    f77_snar[f77_nsym] = 0;
    if (f77_imp_ready && nm[0] >= 'A' && nm[0] <= 'Z') {
        if (f77_sty[f77_nsym] == TY_CHAR) f77_slen[f77_nsym] = f77_imp_len[nm[0] - 'A'];
        f77_slog[f77_nsym] = f77_imp_log[nm[0] - 'A'];
        f77_snar[f77_nsym] = f77_imp_nar[nm[0] - 'A'];
    }
    f77_sparam[f77_nsym] = 0;
    f77_styped[f77_nsym] = 0;
    f77_sstored[f77_nsym] = 0;
    { int d; d = 0; while (d < F77_MAX_RANK) { f77_sextsym[f77_nsym][d] = -1; f77_slov[f77_nsym][d] = -1; d = d + 1; } }
    /* An implicitly typed CHARACTER (IMPLICIT CHARACTER*14 (C)) has its
     * length now and gets all of it; a declaration re-issues the slot
     * anyway.  One byte here left CVCOMP*14 overlapping its
     * neighbours (FCVS FM301, test 18). */
    if (f77_sty[f77_nsym] == TY_CHAR && f77_slen[f77_nsym] > 1)
        f77_frame = f77_frame + ((f77_slen[f77_nsym] + 3) & ~3);
    else
        f77_frame = f77_frame + f77_tysize(f77_sty[f77_nsym]);
    f77_sval[f77_nsym] = hi_emit(HI_ALLOCA, f77_aty(f77_sty[f77_nsym]), -1, -1,
                                 0 - f77_frame, NULL);
    f77_alloca_add(f77_sval[f77_nsym], 0 - f77_frame);
    f77_nsym = f77_nsym + 1;
    return f77_nsym - 1;
}

/* A fresh anonymous local of type `ty`: a statement-function actual.
 * Never looked up by name, so no scope or IMPLICIT NONE concerns. */
static void f77_realloc_sym(int s);
static int f77_sym_temp(int ty) {
    int s;
    if (f77_nsym >= F77_MAX_SYM) { f77_error("too many symbols"); return 0; }
    s = f77_nsym;
    strcpy(f77_sname[s], "%T");
    f77_sty[s] = ty;
    f77_srank[s] = 0;
    f77_sarg[s] = 0;
    f77_shi[s] = -1;
    f77_scom[s] = 0;
    f77_ssaved[s] = 0;
    f77_sgidx[s] = -1;
    f77_sgoff[s] = 0;
    f77_sparam[s] = 0;
    f77_styped[s] = 1;
    f77_sstored[s] = 0;
    f77_slen[s] = 1;
    f77_slenv[s] = -1;
    f77_spstr[s] = -1;
    f77_sexternal[s] = 0;
    f77_sauto[s] = 0;
    f77_slink[s] = NULL;
    f77_sme[s] = -1;
    f77_salloc[s] = 0;
    f77_sintrinsic[s] = 0;
    f77_slog[s] = 0;
    f77_snar[s] = 0;
    { int d; d = 0; while (d < F77_MAX_RANK) { f77_sextsym[s][d] = -1; f77_slov[s][d] = -1; d = d + 1; } }
    f77_frame = f77_frame + f77_tysize(ty);
    f77_sval[s] = hi_emit(HI_ALLOCA, f77_aty(ty), -1, -1, 0 - f77_frame, NULL);
    f77_alloca_add(f77_sval[s], 0 - f77_frame);
    f77_nsym = f77_nsym + 1;
    f77_realloc_sym(s);
    return s;
}

/* --- program units --------------------------------------------------- */

#define F77_UNIT_PROGRAM 0
#define F77_UNIT_SUBR    1
#define F77_UNIT_FUNC    2
#define F77_UNIT_BDATA   3     /* BLOCK DATA: declarations only */
#define F77_UNIT_MODULE  4     /* MODULE (Fortran 90): its specification part, an empty function */
#define F77_MAX_UNIT     256
#define F77_MAX_LINK     (3 * F77_MAX_NAME)

static char f77_uname[F77_MAX_UNIT][F77_MAX_NAME];
/* Fortran 90 hosts.  A module procedure's host is its MODULE, an internal
 * procedure's the unit whose CONTAINS it follows (-1: none).  Such a unit
 * is linked under a name no Fortran name can be, MOD.PROC or HOST.PROC --
 * reference LAPACK has an external SISNAN beside LA_XISNAN's, and an
 * external SLAISNAN beside SISNAN's internal one -- and is seen by name
 * only from its host and the host's other procedures (and, for a module
 * procedure, where a USE brings it). */
static int  f77_uhost[F77_MAX_UNIT];
static char f77_ulink[F77_MAX_UNIT][F77_MAX_LINK];

/* modules (Fortran 90): see f77_mod_snapshot */
#define F77_MAX_MOD    64
#define F77_MAX_MODENT 4096
#define F77_ME_PARAM   1
#define F77_ME_PROC    2
#define F77_ME_GENERIC 3
static int    f77_nmod;
static char   f77_mod_name[F77_MAX_MOD][F77_MAX_NAME];
static int    f77_mod_unit[F77_MAX_MOD];         /* its unit in this file, -1 if read */
static int    f77_nme;
static int    f77_me_mod[F77_MAX_MODENT];
static int    f77_me_home[F77_MAX_MODENT];       /* the module whose procedures a generic names */
static char   f77_me_name[F77_MAX_MODENT][F77_MAX_NAME];
static int    f77_me_kind[F77_MAX_MODENT];
static int    f77_me_ty[F77_MAX_MODENT];         /* a PARAMETER's type, a FUNCTION's result's */
static int    f77_me_log[F77_MAX_MODENT];
static int    f77_me_ival[F77_MAX_MODENT];       /* PARAMETER: value; CHARACTER: length */
static double f77_me_d[F77_MAX_MODENT];
static double f77_me_d2[F77_MAX_MODENT];
static char  *f77_me_str[F77_MAX_MODENT];        /* CHARACTER value; link name; a generic's specifics */
static int    f77_me_pkind[F77_MAX_MODENT];      /* F77_UNIT_SUBR or F77_UNIT_FUNC */
static int    f77_me_nargs[F77_MAX_MODENT];
static int    f77_me_argty[F77_MAX_MODENT][16];  /* a dummy's type, +16 for an array */
/* the generics a unit's INTERFACE blocks name, until its snapshot */
static char   f77_gi_name[32][F77_MAX_NAME];
static char   f77_gi_spec[32][512];
static int    f77_gi_n;
/* per unit: its implicit typing at the end, for the units it hosts */
static int    f77_uimp_map[F77_MAX_UNIT][26];
static int    f77_uimp_len[F77_MAX_UNIT][26];
static int    f77_uimp_log[F77_MAX_UNIT][26];
static int    f77_uimp_nar[F77_MAX_UNIT][26];
static int    f77_uimp_none[F77_MAX_UNIT];

static int f77_generic_pick(int g, int *tys, int n);
static int  f77_ukind[F77_MAX_UNIT];
static int  f77_upos[F77_MAX_UNIT];    /* source offset of the header */
static int  f77_uline[F77_MAX_UNIT];
static int  f77_urty[F77_MAX_UNIT];    /* FUNCTION result type */
static int  f77_urpend[F77_MAX_UNIT];  /* ... its kind not told by the scan (REAL(K), K not found) */
static int  f77_urlen[F77_MAX_UNIT];   /* CHARACTER FUNCTION result length, -1 = (*) */
static int  f77_urlog[F77_MAX_UNIT];   /* LOGICAL FUNCTION */
static int  f77_urnar[F77_MAX_UNIT];   /* INTEGER*2 or LOGICAL*1 FUNCTION: f77_snar of the result */
/* ENTRY: a secondary entry into a subprogram is compiled as a unit of
 * its own -- the parent's declarations, then the statements from the
 * ENTRY to END -- with f77_uentry_parent naming the parent (-1 for an
 * ordinary unit).  In a FUNCTION the entry names and the function
 * name all denote the one result, by aliasing. */
static int  f77_uentry_parent[F77_MAX_UNIT];

/* The unit whose text is being compiled: an ENTRY's is its parent's,
 * and what phase 1 collected from the text (FORMATs, ASSIGNed labels,
 * escaping names) is filed under the parent. */
static int f77_text_unit(void) {
    return f77_uentry_parent[f77_cur_unit] >= 0 ? f77_uentry_parent[f77_cur_unit] : f77_cur_unit;
}
/* The dummies of every unit of an ENTRY family, collected in phase 1:
 * the shared statements name them all, so when one unit of the family
 * is compiled the dummies of the OTHER units are created as phantoms
 * -- by-reference symbols whose address is a frame slot -- so the
 * text compiles (to code that is never reached with them undefined)
 * instead of taking F(X) for a call to an external function F. */
#define F77_MAX_FAMD 512
static char f77_famd_name[F77_MAX_FAMD][F77_MAX_NAME];
static int  f77_famd_unit[F77_MAX_FAMD];
static int  f77_nfamd;
static int  f77_hdr_clen;              /* length seen by f77_function_kw_end; -2 = at f77_hdr_clen_off */
static int  f77_hdr_clen_off;
static int  f77_hdr_log;
static int  f77_hdr_len;              /* REAL*8 FUNCTION: the *n seen by f77_function_kw_end, else 0 */
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
    f77_sgoff[s] = 0;
    f77_sparam[s] = 0;
    f77_styped[s] = 0;
    f77_sstored[s] = 0;
    f77_slen[s] = 1;
    f77_slenv[s] = -1;
    f77_spstr[s] = -1;
    f77_sexternal[s] = 0;
    f77_sauto[s] = 0;
    f77_slink[s] = NULL;
    f77_sme[s] = -1;
    f77_salloc[s] = 0;
    f77_sintrinsic[s] = 0;
    f77_slog[s] = 0;
    f77_snar[s] = 0;
    if (f77_imp_ready && nm[0] >= 'A' && nm[0] <= 'Z') {
        if (f77_sty[s] == TY_CHAR) f77_slen[s] = f77_imp_len[nm[0] - 'A'];
        f77_slog[s] = f77_imp_log[nm[0] - 'A'];
        f77_snar[s] = f77_imp_nar[nm[0] - 'A'];
    }
    { int d; d = 0; while (d < F77_MAX_RANK) { f77_sextsym[s][d] = -1; f77_slov[s][d] = -1; d = d + 1; } }
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
/* A symbol's storage: its elements, or an ALLOCATABLE's descriptor. */
static int f77_sym_nelem(int s);
static int f77_storage_bytes(int s) {
    if (f77_salloc[s]) return 4 + 8 * f77_srank[s];
    return f77_esize(s) * f77_sym_nelem(s);
}

static void f77_realloc_sym(int s) {
    int bytes;
    if (f77_sauto[s] && !f77_decls_open) return;     /* its storage is allocated */
    f77_shi[s] = -1;
    if (f77_sarg[s]) {
        /* By-reference dummy: storage belongs to the caller.  Only the
         * type and shape are recorded here.  A dummy the inliner bound
         * to an actual (f77_bind_actual) shares the caller's own
         * value -- often a local's alloca -- whose type must stay the
         * local's: retyped to an address, the SSA promoter saw its
         * double loads as type puns and left it in memory. */
        if (h_kind[f77_sval[s]] == HI_PARAM) h_ty[f77_sval[s]] = HL_ADDR_TY;
        return;
    }
    bytes = f77_storage_bytes(s);
    bytes = (bytes + 3) & ~3;             /* keep the frame word-aligned */
    /* A main program's array is static (f77_layout_static): its slot
     * is only a placeholder until then. */
    if (f77_srank[s] > 0 && !f77_salloc[s] && f77_ukind[f77_cur_unit] == F77_UNIT_PROGRAM) bytes = 4;
    f77_frame = f77_frame + bytes;
    h_val[f77_sval[s]] = 0 - f77_frame;
    h_ty[f77_sval[s]] = f77_salloc[s] ? TY_INT : f77_aty(f77_sty[s]);
    hl_aoff[hl_nalloca - 1] = 0 - f77_frame;

    /* Scalar double or complex: give the hi word its own promotable slot. */
    if (f77_split_on < 0) f77_split_on = getenv("F77_NO_SPLIT") ? 0 : 1;
    if (f77_split_on && f77_srank[s] == 0 && f77_ispair(f77_sty[s]) &&
        !f77_name_escapes(f77_text_unit(), f77_sname[s])) {
        int wty;
        wty = f77_sty[s] == TY_CPLX ? TY_FLOAT : TY_INT;
        h_ty[f77_sval[s]] = wty;                    /* lo word only */
        f77_shi[s] = hi_emit(HI_ALLOCA, wty, -1, -1,
                             0 - f77_frame + 4, NULL);
        f77_alloca_add(f77_shi[s], 0 - f77_frame + 4);
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
static signed char f77_ldef[F77_MAX_LABEL];   /* a statement carries it */
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
    f77_ldef[f77_nlabel] = 0;
    f77_nlabel = f77_nlabel + 1;
    return f77_lblk[f77_nlabel - 1];
}

/* A label the unit refers to (GO TO, DO, IF, END=) but no statement
 * carries is an error: its block was made on first mention and, left
 * empty, fell through to whatever came next.  (Found by fort370.) */
static void f77_label_check(void) {
    int i;
    char msg[64];
    i = f77_label_base;
    while (i < f77_nlabel) {
        if (!f77_ldef[i]) {
            sprintf(msg, "statement label %d is referred to but never defined", f77_lnum[i]);
            f77_error(msg);
        }
        i = i + 1;
    }
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
        if (f77_flabel[i] == label && f77_funit[i] == f77_text_unit())
            return f77_fstr[i];
        i = i + 1;
    }
    return -1;
}

/* --- control stack (DO loops and block IFs) ------------------------- */

#define F77_CTL_DO 1
#define F77_CTL_IF 2
#define F77_CTL_WHERE 3   /* a WHERE construct: its mask is f77_where_* */
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
static int ctl_cont[F77_MAX_CTL];    /* DO: the step, made by the first CYCLE (-1: none) */
static int ctl_else[F77_MAX_CTL];    /* IF: pending else block */
static char ctl_name[F77_MAX_CTL][64]; /* DO/IF: its construct name, or "" */
static int ctl_n;

/* The unit's construct names (Fortran 90): each names one construct and
 * nothing else in the unit -- no other construct, no variable (gfortran
 * refuses both).  Not kept for a spliced callee's text, which was
 * checked when its own unit was compiled, and whose names are not the
 * caller's. */
#define F77_MAX_CNAME 256
static char f77_cnames[F77_MAX_CNAME][64];
static int f77_ncname;
static int f77_is_cname(char *nm) {
    int i;
    for (i = 0; i < f77_ncname; i++) if (strcmp(f77_cnames[i], nm) == 0) return 1;
    return 0;
}

/* Floor for f77_close_do.  A spliced callee raises it so that a label
 * in the callee can never close a DO belonging to the caller: DGEFA has
 * a statement labelled 70 and is inlined inside main's `DO 70`, and
 * without this floor parsing the callee's label popped the CALLER's
 * loop off the control stack. */
static int f77_ctl_base;

static void f77_ctl_reset(void) { ctl_n = 0; f77_ctl_base = 0; f77_ncname = 0; f77_where_tmp = -1; }

/* --- expression lowering -------------------------------------------- */

static int ex_ty;     /* type of the value most recently produced */
static int ex_hi;     /* its hi word, when ex_ty is TY_DOUBLE */

static int f77_expr(void);
static int f77_power(void);
static int f77_find_func(char *nm);
static int f77_actual_addr(void);
static int f77_tfree_mark(void);
static int f77_implied_do(void (*items)(int, int));
static int f77_is_inquiry(char *nm);
static int f77_inquiry(char *nm, int t, int *iv, double *dv);
static int f77_const_int(int *v);
static int f77_kind_ty(int ty, int k);
static void f77_tfree_to(int mark);
static int f77_urty_of(int u);
static int f77_load_at(int addr, int ty);
static int f77_load_sym(int s);
static int f77_mty(int s);
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
    f77_carg(alo, 0);
    f77_carg(ahi, 0);
    f77_carg(blo, 0);
    f77_carg(bhi, 0);
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
    f77_carg(alo, 0);
    f77_carg(ahi, 0);
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, name);
    h_cbase[r] = cb;
    if (rhi) *rhi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
    return r;
}

static int f77_fp64_call1(char *name, int alo, int *rhi) {
    int cb;
    int r;
    cb = h_ncarg;
    f77_carg(alo, 0);
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
    f77_carg(a, 1);
    f77_carg(ahi, 2);
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, name);
    h_cbase[r] = cb;
    *rhi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
    return r;
}

static int f77_libm_d2(char *name, int a, int ahi, int b, int bhi, int *rhi) {
    int cb;
    int r;
    cb = h_ncarg;
    f77_carg(a, 1);
    f77_carg(ahi, 2);
    f77_carg(b, 1);
    f77_carg(bhi, 2);
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, name);
    h_cbase[r] = cb;
    *rhi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
    return r;
}

static int f77_libm_f1(char *name, int a) {
    int cb;
    int r;
    cb = h_ncarg;
    f77_carg(a, 0);
    r = hi_emit(HI_CALL, TY_FLOAT, -1, -1, 1, name);
    h_cbase[r] = cb;
    return r;
}

static int f77_libm_f2(char *name, int a, int b) {
    int cb;
    int r;
    cb = h_ncarg;
    f77_carg(a, 0);
    f77_carg(b, 0);
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

/* --- COMPLEX*16 values (TY_DCPLX: the address of 16 bytes) --------- */

static int f77_dcx_mark(int v);
static int f77_ndcxv;

/* A fresh 16-byte frame temporary. */
static int f77_dcx_temp(void) {
    int t;
    f77_frame = f77_frame + 16;
    t = hi_emit(HI_ALLOCA, TY_INT, -1, -1, 0 - f77_frame, NULL);
    f77_alloca_add(t, 0 - f77_frame);
    return t;
}

/* The address `off` bytes into a COMPLEX*16. */
static int f77_dcx_at(int addr, int off) {
    if (off == 0) return addr;
    return hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, off, NULL);
}

/* One part as a DOUBLE PRECISION value: part 0 the real, 1 the imaginary.
 * Returns the lo word, the hi word in *hi. */
static int f77_dcx_part(int addr, int part, int *hi) {
    int lo;
    lo = hi_emit(HI_LOAD, TY_INT, f77_dcx_at(addr, part * 8), -1, 0, NULL);
    *hi = hi_emit(HI_LOAD, TY_INT, f77_dcx_at(addr, part * 8 + 4), -1, 0, NULL);
    return lo;
}

/* A COMPLEX*16 value from two doubles: a fresh temporary's address. */
static int f77_dcx_make(int re, int rehi, int im, int imhi) {
    int t;
    t = f77_dcx_temp();
    hi_emit(HI_STORE, TY_INT, t, re, 0, NULL);
    hi_emit(HI_STORE, TY_INT, f77_dcx_at(t, 4), rehi, 0, NULL);
    hi_emit(HI_STORE, TY_INT, f77_dcx_at(t, 8), im, 0, NULL);
    hi_emit(HI_STORE, TY_INT, f77_dcx_at(t, 12), imhi, 0, NULL);
    return f77_dcx_mark(t);
}

/* Copy a COMPLEX*16 value's 16 bytes to `dst`.  All four words are read
 * before any is written, so dst may overlap src. */
static void f77_dcx_copy(int dst, int src) {
    int w[4];
    int k;
    if (dst == src) return;
    for (k = 0; k < 4; k++) w[k] = hi_emit(HI_LOAD, TY_INT, f77_dcx_at(src, 4 * k), -1, 0, NULL);
    for (k = 0; k < 4; k++) hi_emit(HI_STORE, TY_INT, f77_dcx_at(dst, 4 * k), w[k], 0, NULL);
}

/* The guard.  A COMPLEX*16 value is an address; one that reached code
 * written for the other types would be computed with as a word -- the
 * arithmetic dispatch falls through to INTEGER instructions -- and give
 * wrong code without a word.  So every instruction that IS such a value is
 * recorded (f77_dcx_mark: f77_load_at and f77_dcx_make make them all), and
 * when the unit's HIR is complete (f77.c's hl_func, before any pass) each
 * use of one must be as an address: the base of an ADDI, the address of a
 * LOAD or a STORE, or a call argument.  Any other use -- an operand of
 * arithmetic or a comparison, the value a STORE writes -- stops the unit. */
static int *f77_dcxv;
static int f77_dcxv_cap;

static int f77_dcx_mark(int v) {
    if (f77_ndcxv >= f77_dcxv_cap) {
        f77_dcxv_cap = f77_dcxv_cap ? f77_dcxv_cap * 2 : 256;
        f77_dcxv = (int *)realloc(f77_dcxv, (size_t)f77_dcxv_cap * sizeof(int));
        if (!f77_dcxv) { fdputs("f77: out of memory\n", 2); exit(1); }
    }
    f77_dcxv[f77_ndcxv++] = v;
    return v;
}

static void f77_desc_guard(void);
static void f77_dcx_guard(void) {
    char *is;
    int i;
    int k;
    f77_desc_guard();
    if (f77_ndcxv == 0) return;
    is = (char *)calloc((size_t)h_ninst + 1, 1);
    if (!is) { fdputs("f77: out of memory\n", 2); exit(1); }
    for (k = 0; k < f77_ndcxv; k++)
        if (f77_dcxv[k] >= 0 && f77_dcxv[k] < h_ninst) is[f77_dcxv[k]] = 1;
    f77_ndcxv = 0;
    for (i = 0; i < h_ninst; i++) {
        int bad;
        bad = 0;
        /* A branch keeps block numbers there: BR in src1, BRC (src1 its
         * condition) and JMPTAB in src2. */
        if (h_kind[i] != HI_BR && h_kind[i] != HI_BRC && h_kind[i] != HI_JMPTAB &&
            h_src2[i] >= 0 && h_src2[i] < h_ninst && is[h_src2[i]]) bad = 1;
        if (h_kind[i] != HI_BR &&
            h_src1[i] >= 0 && h_src1[i] < h_ninst && is[h_src1[i]] &&
            h_kind[i] != HI_ADDI && h_kind[i] != HI_LOAD && h_kind[i] != HI_STORE) bad = 1;
        if (bad) {
            if (getenv("F77_DCX_DEBUG")) fprintf(stderr, "dcx guard: inst %d kind %d src1 %d src2 %d\n", i, h_kind[i], h_src1[i], h_src2[i]);
            f77_error("COMPLEX*16 is not supported in this context yet");
            free(is);
            return;
        }
    }
    /* A call argument: the runtime's COMPLEX*16 helpers take addresses,
     * and pass them as f77_dcx_arg's alias, never the value itself. */
    for (k = 0; k < h_ncarg; k++) {
        if (h_carg[k] >= 0 && h_carg[k] < h_ninst && is[h_carg[k]]) {
            if (getenv("F77_DCX_DEBUG")) fprintf(stderr, "dcx guard: call argument %d is inst %d\n", k, h_carg[k]);
            f77_error("COMPLEX*16 is not supported in this context yet");
            break;
        }
    }
    free(is);
}

/* Convert `v` (with hi word *hi when it is a double) from one type to
 * another, updating *hi in place. */
static int f77_rconst(double d);
static int f77_cvt(int v, int *hi, int from, int to) {
    if (from == to) return v;
    if (from == TY_CHAR || to == TY_CHAR) {
        f77_error("CHARACTER value where a numeric one is needed (or vice versa)");
        return v;
    }
    if (to == TY_DCPLX) {
        /* (x, 0D0), or each part of a COMPLEX widened */
        int re;
        int rehi;
        int im;
        int imhi;
        if (from == TY_CPLX) {
            rehi = -1;
            re = f77_cvt(v, &rehi, TY_FLOAT, TY_DOUBLE);
            imhi = -1;
            im = f77_cvt(*hi, &imhi, TY_FLOAT, TY_DOUBLE);
        } else {
            rehi = *hi;
            re = f77_cvt(v, &rehi, from, TY_DOUBLE);
            im = f77_dconst(0.0, &imhi);
        }
        *hi = -1;
        return f77_dcx_make(re, rehi, im, imhi);
    }
    if (from == TY_DCPLX) {
        /* the real part (F77 4.6), or each part narrowed to a COMPLEX */
        int re;
        int rehi;
        re = f77_dcx_part(v, 0, &rehi);
        if (to == TY_CPLX) {
            int im;
            int imhi;
            im = f77_dcx_part(v, 1, &imhi);
            re = f77_cvt(re, &rehi, TY_DOUBLE, TY_FLOAT);
            *hi = f77_cvt(im, &imhi, TY_DOUBLE, TY_FLOAT);
            return re;
        }
        *hi = rehi;
        return f77_cvt(re, hi, TY_DOUBLE, to);
    }
    if (to == TY_CPLX) {
        /* (x, 0.0) */
        v = f77_cvt(v, hi, from, TY_FLOAT);
        *hi = f77_rconst(0.0);
        return v;
    }
    if (from == TY_CPLX) {
        /* the real part, F77 4.6 */
        return f77_cvt(v, hi, TY_FLOAT, to);
    }
    if (to == TY_DOUBLE) {
        if (from == TY_INT)   return f77_fp64_call1("__fp64_cvt_itoD", v, hi);
        if (from == TY_FLOAT) return f77_fp64_call1("__fp64_cvt_ftoD", v, hi);
        return v;
    }
    if (from == TY_DOUBLE) {
        int cb;
        int r;
        cb = h_ncarg;
        f77_carg(v, 0);
        f77_carg(*hi, 0);
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
    /* COMPLEX*16 beats everything, and COMPLEX with DOUBLE PRECISION is
     * COMPLEX*16 (F77 forbids the mix; gfortran, IBM's FORTRAN and
     * fort370 give it this type). */
    if (aty == TY_DCPLX || bty == TY_DCPLX ||
        (aty == TY_CPLX && bty == TY_DOUBLE) || (aty == TY_DOUBLE && bty == TY_CPLX)) {
        *a = f77_cvt(*a, ahi, aty, TY_DCPLX);
        *b = f77_cvt(*b, bhi, bty, TY_DCPLX);
        return TY_DCPLX;
    }
    if (aty == TY_CPLX || bty == TY_CPLX) {
        *a = f77_cvt(*a, ahi, aty, TY_CPLX);
        *b = f77_cvt(*b, bhi, bty, TY_CPLX);
        return TY_CPLX;
    }
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

/* --- COMPLEX*16 arithmetic ------------------------------------------- */

/* A COMPLEX*16 value passed to one of the runtime's helpers: an alias of
 * the address (the guard allows the value itself only as an address, and
 * not as a call argument, which is how an unhandled use would look). */
static int f77_dcx_arg(int v) { return hi_emit(HI_ADDI, HL_ADDR_TY, v, -1, 0, NULL); }

static int f77_call_i(char *name, int nargs, int *args);

/* a op b, both COMPLEX*16, op one of + - * /.  + - * inline on the parts
 * with the double-precision primitives -- the product the textbook
 * (ar br - ai bi, ar bi + ai br), as gfortran computes it for finite
 * values; / in the runtime (f77_zdiv, Smith's method). */
static int f77_dcx_binop(int op, int a, int b) {
    int ar; int arh; int ai; int aih;
    int br; int brh; int bi; int bih;
    int re; int reh; int im; int imh;
    if (op == T_SLASH) {
        int args[3];
        int out;
        out = f77_dcx_temp();
        args[0] = f77_dcx_arg(out); args[1] = f77_dcx_arg(a); args[2] = f77_dcx_arg(b);
        f77_call_i("f77_zdiv", 3, args);
        return f77_dcx_mark(out);
    }
    ar = f77_dcx_part(a, 0, &arh); ai = f77_dcx_part(a, 1, &aih);
    br = f77_dcx_part(b, 0, &brh); bi = f77_dcx_part(b, 1, &bih);
    if (op == T_PLUS || op == T_MINUS) {
        char *f;
        f = op == T_PLUS ? "__fp64_add" : "__fp64_sub";
        re = f77_fp64_call2(f, ar, arh, br, brh, &reh);
        im = f77_fp64_call2(f, ai, aih, bi, bih, &imh);
    } else {
        int t1; int t1h; int t2; int t2h;
        t1 = f77_fp64_call2("__fp64_mul", ar, arh, br, brh, &t1h);
        t2 = f77_fp64_call2("__fp64_mul", ai, aih, bi, bih, &t2h);
        re = f77_fp64_call2("__fp64_sub", t1, t1h, t2, t2h, &reh);
        t1 = f77_fp64_call2("__fp64_mul", ar, arh, bi, bih, &t1h);
        t2 = f77_fp64_call2("__fp64_mul", ai, aih, br, brh, &t2h);
        im = f77_fp64_call2("__fp64_add", t1, t1h, t2, t2h, &imh);
    }
    return f77_dcx_make(re, reh, im, imh);
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
/* Byte offset of element `off` (zero-based, linear) of symbol s. */
static int f77_elem_bytes(int s, int off) {
    int t;
    if (f77_sty[s] == TY_CHAR && f77_slen[s] < 0)
        return hi_emit(HI_MUL, TY_INT, off, f77_slenv[s], 0, NULL);
    t = f77_esize(s);
    if (t == 1) return off;
    if (t == 4) return hi_emit(HI_SLL, TY_INT, off, f77_iconst(2), 0, NULL);
    if (t == 8) return hi_emit(HI_SLL, TY_INT, off, f77_iconst(3), 0, NULL);
    return hi_emit(HI_MUL, TY_INT, off, f77_iconst(t), 0, NULL);
}

/* An ALLOCATABLE's descriptor, `off` bytes in: always an ADDI of the slot
 * (offset 0 too), the only use of it the guard allows. */
static int f77_desc_at(int s, int off) { return hi_emit(HI_ADDI, HL_ADDR_TY, f77_sval[s], -1, off, NULL); }
static int f77_desc_load(int s, int off) { return hi_emit(HI_LOAD, TY_INT, f77_desc_at(s, off), -1, 0, NULL); }
static int f77_desc_base(int s) { return f77_desc_load(s, 0); }

/* The address of A(idx[0], ..., idx[n-1]), the subscripts INTEGER values
 * already computed: the address arithmetic of f77_subscript_addr, which
 * parses the subscripts and comes here, and of array sections, whose
 * subscripts are the indices of loops the front end generates. */
static int f77_elem_bytes(int s, int off);
static int f77_elem_addr(int s, int *idxs, int n) {
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
    if (f77_salloc[s]) {
        /* bounds and storage from the descriptor (ALLOCATABLE) */
        int stride;
        int d;
        if (n != f77_srank[s]) f77_error("wrong number of subscripts");
        off = -1;
        stride = -1;
        for (k = 0; k < n && k < f77_srank[s]; k++) {
            d = hi_emit(HI_SUB, TY_INT, idxs[k], f77_desc_load(s, 4 + 8 * k), 0, NULL);
            if (stride >= 0) d = hi_emit(HI_MUL, TY_INT, d, stride, 0, NULL);
            off = off < 0 ? d : hi_emit(HI_ADD, TY_INT, off, d, 0, NULL);
            e = f77_desc_load(s, 8 + 8 * k);
            stride = stride < 0 ? e : hi_emit(HI_MUL, TY_INT, stride, e, 0, NULL);
        }
        if (off < 0) off = f77_iconst(0);
        return hi_emit(HI_ADD, HL_ADDR_TY, f77_desc_base(s), f77_elem_bytes(s, off), 0, NULL);
    }
    off = -1;
    coff = 0;
    cstride = 1;
    vstride = -1;
    for (k = 0; k < n; k++) {
        idx = idxs[k];
        if (k < f77_srank[s]) {
            if (f77_slov[s][k] >= 0)
                idx = hi_emit(HI_SUB, TY_INT, idx, f77_slov[s][k], 0, NULL);
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
            if (f77_sextv[s][k] >= 0) {
                e = f77_sextv[s][k];
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
    }
    if (n != f77_srank[s]) f77_error("wrong number of subscripts");

    if (off < 0) off = f77_iconst(0);
    byte = f77_elem_bytes(s, off);
    addr = hi_emit(HI_ADD, HL_ADDR_TY, f77_sval[s], byte, 0, NULL);
    if (coff != 0 && f77_sty[s] == TY_CHAR && f77_slen[s] < 0) {
        /* assumed-length elements: the offset is a run-time product */
        t = hi_emit(HI_MUL, TY_INT, f77_iconst(coff), f77_slenv[s], 0, NULL);
        addr = hi_emit(HI_ADD, HL_ADDR_TY, addr, t, 0, NULL);
    } else if (coff != 0) {
        int cb;
        cb = coff * f77_esize(s);
        if (cb >= -2048 && cb <= 2047)
            addr = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, cb, NULL);
        else
            addr = hi_emit(HI_ADD, HL_ADDR_TY, addr, f77_iconst(cb), 0, NULL);
    }
    return addr;
}

static int f77_subscript_addr(int s) {
    int idxs[F77_MAX_RANK + 1];
    int n;
    int idx;
    n = 0;
    for (;;) {
        idx = f77_expr();
        idx = f77_cvt(idx, &ex_hi, ex_ty, TY_INT);
        if (n <= F77_MAX_RANK) idxs[n] = idx;
        n = n + 1;
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) { f77_error("expected ) after subscripts"); }
    else f77_tok();
    if (n > F77_MAX_RANK) { f77_error("wrong number of subscripts"); n = F77_MAX_RANK; }
    return f77_elem_addr(s, idxs, n);
}

/* A static local's loads may hoist only if nothing but this unit's own
 * stores could change it: not in an EQUIVALENCE (it could change under
 * the other name), and not in a unit with ENTRY points (each compiles
 * part of the parent's text against the same storage). */
static int f77_ldro_local_ok(int s) {
    int q;
    if (f77_scom[s]) return 0;
    for (q = 0; q < f77_eq_n; q++)
        if (f77_eq_sa[q] == s || f77_eq_sb[q] == s) return 0;
    if (f77_unit_entries) return 0;
    return 1;
}

/* Load a scalar symbol.  When it has a paired hi slot, both halves are
 * direct loads of promotable allocas -- no address arithmetic, so
 * mem2reg can keep the whole double in registers. */
static int f77_load_sym(int s) {
    int v;
    ex_ty = f77_sty[s];
    ex_log = f77_slog[s];
    if (f77_shi[s] >= 0) {
        int wty;
        wty = f77_sty[s] == TY_CPLX ? TY_FLOAT : TY_INT;
        v = hi_emit(HI_LOAD, wty, f77_sval[s], -1, 0, NULL);
        ex_hi = hi_emit(HI_LOAD, wty, f77_shi[s], -1, 0, NULL);
        return v;
    }
    v = f77_load_at(f77_sval[s], f77_mty(s));
    /* A scalar dummy's load is read-only (F77 15.9.3.6), and so is a
     * local scalar's in static storage (SAVE, or DATA's initial value)
     * outside COMMON: only this unit can reach it, so if
     * the unit never stores it its value cannot change -- LEN in
     * INTEGER LEN/30000/, reloaded on every pass of a loop.  Retracted
     * at unit end (f77_ldro_finalize) for a stored, equivalenced or
     * ENTRY-shared one. */
    if (f77_srank[s] == 0 && f77_sty[s] != TY_CHAR &&
        (f77_sarg[s] || (f77_ssaved[s] && !f77_scom[s]))) {
        f77_ldro_note(v, s);
        if (f77_sty[s] == TY_DOUBLE && ex_hi >= 0) f77_ldro_note(ex_hi, s);
    }
    return v;
}

static void f77_store_sym_val(int s, int v, int vty, int vhi) {
    f77_sstored[s] = 1;
    if (f77_shi[s] >= 0) {
        int wty;
        wty = f77_sty[s] == TY_CPLX ? TY_FLOAT : TY_INT;
        v = f77_cvt(v, &vhi, vty, f77_sty[s]);
        hi_emit(HI_STORE, wty, f77_sval[s], v, 0, NULL);
        hi_emit(HI_STORE, wty, f77_shi[s], vhi, 0, NULL);
        return;
    }
    f77_store_at(f77_sval[s], f77_mty(s), v, vty, vhi);
}

/* fort370: a value stored to an INTEGER*2 is cut to 16 bits and sign
 * extended, as STH and then LH make it, so that one kept in a register
 * reads back as memory would (FORTRAN H: K = 40000 is -25536).  A
 * LOGICAL*1 value is 0 or 1 already. */
static int f77_narrow(int v, int ty) {
    if (ty != TY_SHORT) return v;
    if (h_kind[v] == HI_ICONST)
        return f77_iconst(((h_val[v] & 0xFFFF) ^ 0x8000) - 0x8000);
    v = hi_emit(HI_SLL, TY_INT, v, f77_iconst(16), 0, NULL);
    return hi_emit(HI_SRA, TY_INT, v, f77_iconst(16), 0, NULL);
}

/* fort370: the type a symbol is loaded and stored as -- its own, or
 * for INTEGER*2 a halfword and for LOGICAL*1 an unsigned byte (issue
 * 10).  f77_load_at and f77_store_at take these and give and take
 * TY_INT values. */
#define F77_TY_BYTE (TY_CHAR | TY_UNSIGNED)
static int f77_mty(int s) {
    if (f77_snar[s] == 2) return TY_SHORT;
    if (f77_snar[s] == 1) return F77_TY_BYTE;
    return f77_sty[s];
}

/* The last INTEGER*2 or LOGICAL*1 load and its memory type: an
 * unformatted WRITE whose item is that very value writes 2 or 1 bytes. */
static int f77_nar_ld = -1;
static int f77_nar_ty;

/* Load a value of type `ty` from `addr`, setting ex_ty/ex_hi. */
static int f77_load_at(int addr, int ty) {
    int v;
    if (ty == TY_SHORT || ty == F77_TY_BYTE) {
        ex_ty = TY_INT;
        f77_nar_ld = hi_emit(HI_LOAD, ty, addr, -1, 0, NULL);
        f77_nar_ty = ty;
        return f77_nar_ld;
    }
    ex_ty = ty;
    if (ty == TY_DCPLX) {
        /* A COMPLEX*16 value is its address -- as an alias, which the
         * guard marks: the variable's own address instruction is also
         * what a by-reference actual passes, legitimately. */
        ex_hi = -1;
        return f77_dcx_mark(hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 0, NULL));
    }
    if (f77_ispair(ty)) {
        int a4;
        int wty;
        wty = ty == TY_CPLX ? TY_FLOAT : TY_INT;
        v = hi_emit(HI_LOAD, wty, addr, -1, 0, NULL);
        a4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
        ex_hi = hi_emit(HI_LOAD, wty, a4, -1, 0, NULL);
        return v;
    }
    return hi_emit(HI_LOAD, ty, addr, -1, 0, NULL);
}

/* Store `v` (hi word `vhi`) of type `vty` to `addr` as type `ty`. */
static void f77_store_at(int addr, int ty, int v, int vty, int vhi) {
    if (ty == TY_SHORT || ty == F77_TY_BYTE) {
        v = f77_cvt(v, &vhi, vty, TY_INT);
        v = f77_narrow(v, ty);
        hi_emit(HI_STORE, ty, addr, v, 0, NULL);
        return;
    }
    v = f77_cvt(v, &vhi, vty, ty);
    if (ty == TY_DCPLX) {
        f77_dcx_copy(addr, v);
        return;
    }
    if (f77_ispair(ty)) {
        int a4;
        int wty;
        wty = ty == TY_CPLX ? TY_FLOAT : TY_INT;
        hi_emit(HI_STORE, wty, addr, v, 0, NULL);
        a4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
        hi_emit(HI_STORE, wty, a4, vhi, 0, NULL);
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
#define IN_AINT   27   /* truncation: AINT/DINT */
#define IN_ANINT  28   /* nearest whole number: ANINT/DNINT */
#define IN_NINT   29   /* nearest integer: NINT/IDNINT */
#define IN_DIM    30   /* positive difference: DIM/IDIM/DDIM */
#define IN_DPROD  31   /* double product of two reals */
#define IN_LEN    32   /* CHARACTER intrinsics */
#define IN_ICHAR  33
#define IN_CHAR   34
#define IN_INDEX  35
#define IN_LGE    36
#define IN_LGT    37
#define IN_LLE    38
#define IN_LLT    39
#define IN_AIMAG  40   /* COMPLEX intrinsics */
#define IN_CMPLX  41
#define IN_CONJG  42
#define IN_DCMPLX 54   /* COMPLEX*16 constructor (DCMPLX) */
#define IN_CEILING 55  /* Fortran 90: CEILING, FLOOR, EXPONENT */
#define IN_FLOOR   56
#define IN_EXPONENT 57
#define IN_MAXLOC  58  /* Fortran 90 reductions of a rank-1 array */
#define IN_MINLOC  59
#define IN_MAXVAL  60
#define IN_MINVAL  61
#define IN_ALLOCATED 62
#define IN_TRANSFER 63    /* an actual argument only (f77_transfer_actual) */
#define IN_TRIM     64    /* Fortran 90 CHARACTER: TRIM, LEN_TRIM */
#define IN_LEN_TRIM 65
#define IN_INQUIRY  66    /* HUGE TINY EPSILON KIND DIGITS ...: f77_inquiry */
#define IN_SUM      67    /* Fortran 90 reductions of an array expression (f77_reduce) */
#define IN_PRODUCT  68
#define IN_COUNT    69
#define IN_ANY      70
#define IN_ALL      71
#define IN_DOTP     72    /* DOT_PRODUCT */
#define IN_SIZE     73    /* Fortran 90 array inquiry (f77_arr_inquiry) */
#define IN_LBOUND   74
#define IN_UBOUND   75
/* MIL-STD-1753's bit functions, in line (its 2.6 hopes so) */
#define IN_IAND   44
#define IN_IOR    45
#define IN_IEOR   46
#define IN_NOT    47
#define IN_ISHFT  48
#define IN_ISHFTC 49
#define IN_IBITS  50
#define IN_BTEST  51
#define IN_IBSET  52
#define IN_IBCLR  53

/* The rightmost len bits, 0 <= len <= 32: SRL(-1, 32-len), with no
 * shift of 32 (which the hardware takes as one of 0). */
static int f77_bitmask(int len) {
    int t;
    int c;
    int m;
    c = f77_iconst(32);
    t = hi_emit(HI_SUB, TY_INT, c, len, 0, NULL);
    c = f77_iconst(-1);
    m = hi_emit(HI_SRL, TY_INT, c, t, 0, NULL);
    c = f77_iconst(0);
    t = hi_emit(HI_SEQ, TY_INT, len, c, 0, NULL);
    return f77_select(t, c, m);
}

/* The bit functions of MIL-STD-1753 2.6 on their arguments' values v:
 * bit 0 is the rightmost.  A shift count, bit position or length out
 * of the standard's range gives whatever the shifts give.  One emit a
 * statement: C leaves the order of a call's arguments open, and the
 * instructions' order must not depend on the host's compiler. */
static int f77_bits(int in, int *v) {
    int m;
    int t;
    int k;
    int c;
    if (in == IN_IAND) return hi_emit(HI_AND, TY_INT, v[0], v[1], 0, NULL);
    if (in == IN_IOR) return hi_emit(HI_OR, TY_INT, v[0], v[1], 0, NULL);
    if (in == IN_IEOR) return hi_emit(HI_XOR, TY_INT, v[0], v[1], 0, NULL);
    if (in == IN_NOT) return hi_emit(HI_BNOT, TY_INT, v[0], -1, 0, NULL);
    if (in == IN_ISHFT) {
        k = v[1];
        if (h_kind[k] == HI_ICONST) {
            if (h_val[k] >= 32 || h_val[k] <= -32) return f77_iconst(0);
            if (h_val[k] >= 0) return hi_emit(HI_SLL, TY_INT, v[0], k, 0, NULL);
            c = f77_iconst(-h_val[k]);
            return hi_emit(HI_SRL, TY_INT, v[0], c, 0, NULL);
        }
        m = hi_emit(HI_SLL, TY_INT, v[0], k, 0, NULL);
        c = hi_emit(HI_NEG, TY_INT, k, -1, 0, NULL);
        t = hi_emit(HI_SRL, TY_INT, v[0], c, 0, NULL);
        c = f77_iconst(0);
        c = hi_emit(HI_SGE, TY_INT, k, c, 0, NULL);
        m = f77_select(c, m, t);
        /* a shift of 32 either way is all zeros (|SHIFT| <= 32) */
        c = f77_iconst(32);
        t = hi_emit(HI_SGE, TY_INT, k, c, 0, NULL);
        c = f77_iconst(0);
        m = f77_select(t, c, m);
        c = f77_iconst(-32);
        t = hi_emit(HI_SLE, TY_INT, k, c, 0, NULL);
        c = f77_iconst(0);
        return f77_select(t, c, m);
    }
    if (in == IN_ISHFTC) {
        /* the rightmost ic bits rotated left k places, k taken into
         * 0..ic-1; the rest of the word as it was */
        int ic;
        int w;
        int r;
        ic = v[2];
        m = f77_bitmask(ic);
        c = f77_iconst(0);
        c = hi_emit(HI_SLT, TY_INT, v[1], c, 0, NULL);
        t = hi_emit(HI_ADD, TY_INT, v[1], ic, 0, NULL);
        k = f77_select(c, t, v[1]);
        c = hi_emit(HI_SEQ, TY_INT, k, ic, 0, NULL);
        t = f77_iconst(0);
        k = f77_select(c, t, k);
        w = hi_emit(HI_AND, TY_INT, v[0], m, 0, NULL);
        r = hi_emit(HI_SLL, TY_INT, w, k, 0, NULL);
        c = hi_emit(HI_SUB, TY_INT, ic, k, 0, NULL);
        t = hi_emit(HI_SRL, TY_INT, w, c, 0, NULL);
        r = hi_emit(HI_OR, TY_INT, r, t, 0, NULL);
        c = f77_iconst(0);
        c = hi_emit(HI_SEQ, TY_INT, k, c, 0, NULL);
        r = f77_select(c, w, r);
        r = hi_emit(HI_AND, TY_INT, r, m, 0, NULL);
        c = hi_emit(HI_BNOT, TY_INT, m, -1, 0, NULL);
        t = hi_emit(HI_AND, TY_INT, v[0], c, 0, NULL);
        return hi_emit(HI_OR, TY_INT, t, r, 0, NULL);
    }
    if (in == IN_IBITS) {
        t = hi_emit(HI_SRL, TY_INT, v[0], v[1], 0, NULL);
        m = f77_bitmask(v[2]);
        return hi_emit(HI_AND, TY_INT, t, m, 0, NULL);
    }
    if (in == IN_BTEST) {
        ex_log = 1;
        t = hi_emit(HI_SRL, TY_INT, v[0], v[1], 0, NULL);
        c = f77_iconst(1);
        return hi_emit(HI_AND, TY_INT, t, c, 0, NULL);
    }
    c = f77_iconst(1);
    t = hi_emit(HI_SLL, TY_INT, c, v[1], 0, NULL);
    if (in == IN_IBSET) return hi_emit(HI_OR, TY_INT, v[0], t, 0, NULL);
    t = hi_emit(HI_BNOT, TY_INT, t, -1, 0, NULL);
    return hi_emit(HI_AND, TY_INT, v[0], t, 0, NULL);
}

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
    if (strcmp(nm, "AINT") == 0 || strcmp(nm, "DINT") == 0) return IN_AINT;
    if (strcmp(nm, "ANINT") == 0 || strcmp(nm, "DNINT") == 0) return IN_ANINT;
    if (strcmp(nm, "NINT") == 0 || strcmp(nm, "IDNINT") == 0) return IN_NINT;
    if (strcmp(nm, "CEILING") == 0) return IN_CEILING;
    if (strcmp(nm, "FLOOR") == 0) return IN_FLOOR;
    if (strcmp(nm, "EXPONENT") == 0) return IN_EXPONENT;
    if (strcmp(nm, "MAXLOC") == 0) return IN_MAXLOC;
    if (strcmp(nm, "MINLOC") == 0) return IN_MINLOC;
    if (strcmp(nm, "MAXVAL") == 0) return IN_MAXVAL;
    if (strcmp(nm, "MINVAL") == 0) return IN_MINVAL;
    if (strcmp(nm, "SUM") == 0) return IN_SUM;
    if (strcmp(nm, "PRODUCT") == 0) return IN_PRODUCT;
    if (strcmp(nm, "COUNT") == 0) return IN_COUNT;
    if (strcmp(nm, "ANY") == 0) return IN_ANY;
    if (strcmp(nm, "ALL") == 0) return IN_ALL;
    if (strcmp(nm, "DOT_PRODUCT") == 0) return IN_DOTP;
    if (strcmp(nm, "SIZE") == 0) return IN_SIZE;
    if (strcmp(nm, "LBOUND") == 0) return IN_LBOUND;
    if (strcmp(nm, "UBOUND") == 0) return IN_UBOUND;
    if (strcmp(nm, "ALLOCATED") == 0) return IN_ALLOCATED;
    if (strcmp(nm, "TRANSFER") == 0) return IN_TRANSFER;
    if (strcmp(nm, "TRIM") == 0) return IN_TRIM;
    if (f77_is_inquiry(nm)) return IN_INQUIRY;
    if (strcmp(nm, "LEN_TRIM") == 0) return IN_LEN_TRIM;
    if (strcmp(nm, "DIM") == 0 || strcmp(nm, "IDIM") == 0 ||
        strcmp(nm, "DDIM") == 0) return IN_DIM;
    if (strcmp(nm, "DPROD") == 0) return IN_DPROD;
    if (strcmp(nm, "LEN") == 0) return IN_LEN;
    if (strcmp(nm, "ICHAR") == 0) return IN_ICHAR;
    if (strcmp(nm, "CHAR") == 0) return IN_CHAR;
    if (strcmp(nm, "INDEX") == 0) return IN_INDEX;
    if (strcmp(nm, "LGE") == 0) return IN_LGE;
    if (strcmp(nm, "LGT") == 0) return IN_LGT;
    if (strcmp(nm, "LLE") == 0) return IN_LLE;
    if (strcmp(nm, "LLT") == 0) return IN_LLT;
    if (strcmp(nm, "AIMAG") == 0) return IN_AIMAG;
    if (strcmp(nm, "CMPLX") == 0) return IN_CMPLX;
    if (strcmp(nm, "CONJG") == 0) return IN_CONJG;
    /* COMPLEX*16, by IBM's and gfortran's names: DREAL is DBLE of one */
    if (strcmp(nm, "DCMPLX") == 0) return IN_DCMPLX;
    if (strcmp(nm, "DIMAG") == 0) return IN_AIMAG;
    if (strcmp(nm, "DCONJG") == 0) return IN_CONJG;
    if (strcmp(nm, "DREAL") == 0) return IN_DBLE;
    if (strcmp(nm, "CDABS") == 0 || strcmp(nm, "ZABS") == 0) return IN_ABS;
    if (strcmp(nm, "CABS") == 0) return IN_ABS;
    if (strcmp(nm, "CSQRT") == 0) return IN_SQRT;
    if (strcmp(nm, "CEXP") == 0) return IN_EXP;
    if (strcmp(nm, "CLOG") == 0) return IN_LOG;
    if (strcmp(nm, "CSIN") == 0) return IN_SIN;
    if (strcmp(nm, "CCOS") == 0) return IN_COS;
    /* COMPLEX*16, by IBM's (CD) and gfortran's (Z) names */
    if (strcmp(nm, "CDSQRT") == 0 || strcmp(nm, "ZSQRT") == 0) return IN_SQRT;
    if (strcmp(nm, "CDEXP") == 0 || strcmp(nm, "ZEXP") == 0) return IN_EXP;
    if (strcmp(nm, "CDLOG") == 0 || strcmp(nm, "ZLOG") == 0) return IN_LOG;
    if (strcmp(nm, "CDSIN") == 0 || strcmp(nm, "ZSIN") == 0) return IN_SIN;
    if (strcmp(nm, "CDCOS") == 0 || strcmp(nm, "ZCOS") == 0) return IN_COS;
    if (strcmp(nm, "IAND") == 0) return IN_IAND;
    if (strcmp(nm, "IOR") == 0) return IN_IOR;
    if (strcmp(nm, "IEOR") == 0) return IN_IEOR;
    if (strcmp(nm, "NOT") == 0) return IN_NOT;
    if (strcmp(nm, "ISHFT") == 0) return IN_ISHFT;
    if (strcmp(nm, "ISHFTC") == 0) return IN_ISHFTC;
    if (strcmp(nm, "IBITS") == 0) return IN_IBITS;
    if (strcmp(nm, "BTEST") == 0) return IN_BTEST;
    if (strcmp(nm, "IBSET") == 0) return IN_IBSET;
    if (strcmp(nm, "IBCLR") == 0) return IN_IBCLR;
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

static int f77_stf_find(char *nm);
static int f77_stf_expand(int si);
static int f77_sym_find(char *nm);

/* A call to a runtime routine taking `n` word arguments, result in r1. */
static int f77_call_i(char *name, int n, int *args) {
    int cb;
    int r;
    int i;
    cb = h_ncarg;
    for (i = 0; i < n; i++) {
        f77_carg(args[i], 0);
    }
    r = hi_emit(HI_CALL, TY_INT, -1, -1, n, name);
    h_cbase[r] = cb;
    return r;
}

/* A frame temporary of `len` bytes for a CHARACTER value. */
static int f77_char_temp(int len) {
    int tmp;
    f77_frame = f77_frame + ((len + 3) & ~3);
    tmp = hi_emit(HI_ALLOCA, TY_CHAR, -1, -1, 0 - f77_frame, NULL);
    f77_alloca_add(tmp, 0 - f77_frame);
    return tmp;
}

/* dst(1:dlen) = src(1:slen), blank-padded or truncated. */
static void f77_char_assign(int dst, int dlen, int src, int slen) {
    int args[4];
    args[0] = dst; args[1] = dlen; args[2] = src; args[3] = slen;
    f77_call_i("f77_strcpy", 4, args);
}

/* Substring (e1:e2) of a value at `base` with length `lenv` (constant
 * `clen`, or -1).  The scanner is on the opening parenthesis.  Sets
 * ex_len/ex_clen. */
static int f77_substring(int base, int lenv, int clen) {
    int e1; int e2; int c1; int c2; int k1; int k2; int t;
    f77_tok();
    e1 = -1; e2 = -1; c1 = 1; c2 = clen; k1 = 1; k2 = clen >= 0;
    if (lx_t != T_COLON) {
        e1 = f77_expr(); e1 = f77_cvt(e1, &ex_hi, ex_ty, TY_INT);
        k1 = h_kind[e1] == HI_ICONST;
        c1 = k1 ? h_val[e1] : -2;
    }
    if (lx_t != T_COLON) { f77_error("expected : in substring"); return base; }
    f77_tok();
    if (lx_t != T_RP) {
        e2 = f77_expr(); e2 = f77_cvt(e2, &ex_hi, ex_ty, TY_INT);
        k2 = h_kind[e2] == HI_ICONST;
        c2 = k2 ? h_val[e2] : -2;
    }
    if (lx_t != T_RP) { f77_error("expected ) after substring"); return base; }
    f77_tok();
    /* (e1:e2) needs 1 <= e1 <= e2 <= the length (F77 5.7.1); bounds
     * that are constants are checked here, the rest are not checked. */
    if ((k1 && (c1 < 1 || (clen >= 0 && c1 > clen)))
        || (k2 && (c2 < 1 || (clen >= 0 && c2 > clen)))
        || (k1 && k2 && c1 > c2)) {
        f77_error("substring outside its string");
        c1 = 1; c2 = 1;
    }
    if (e1 < 0) e1 = f77_iconst(1);
    if (e2 < 0) e2 = lenv;
    t = hi_emit(HI_ADDI, TY_INT, e1, -1, -1, NULL);
    base = hi_emit(HI_ADD, HL_ADDR_TY, base, t, 0, NULL);
    t = hi_emit(HI_SUB, TY_INT, e2, e1, 0, NULL);
    ex_len = hi_emit(HI_ADDI, TY_INT, t, -1, 1, NULL);
    ex_clen = (c1 >= 0 && c2 >= 0) ? c2 - c1 + 1 : -1;
    if (ex_clen >= 0) ex_len = f77_iconst(ex_clen);
    ex_ty = TY_CHAR;
    ex_hi = -1;
    return base;
}

/* Does the parenthesised group the scanner is on contain a top-level
 * colon?  That is what tells C(1:3) (a substring) from F(1) (a call to
 * an external CHARACTER function). */
static int f77_paren_has_colon(void) {
    int i;
    int depth;
    i = (int)(lx_rts - lx_stmt);
    if (i < 0 || i >= lx_stmt_len || lx_stmt[i] != '(') return 0;
    depth = 0;
    while (i < lx_stmt_len) {
        if (lx_stmt[i] == '\'') { i = i + 1; while (i < lx_stmt_len && lx_stmt[i] != '\'') i = i + 1; }
        else if (lx_stmt[i] == '(') depth = depth + 1;
        else if (lx_stmt[i] == ')') { depth = depth - 1; if (depth == 0) return 0; }
        else if (lx_stmt[i] == ':' && depth == 1) return 1;
        i = i + 1;
    }
    return 0;
}

/* Reference to CHARACTER symbol s (name already consumed): the whole
 * entity, an array element, or a substring of either.  Returns the
 * address; sets ex_ty/ex_len/ex_clen. */
static int f77_char_ref(int s) {
    int addr;
    int lenv;
    int clen;
    clen = f77_slen[s];
    if (clen >= 0) lenv = f77_iconst(clen);
    else if (f77_slenv[s] >= 0) lenv = f77_slenv[s];
    else { f77_error("CHARACTER length is not known here"); lenv = f77_iconst(1); clen = 1; }
    if (f77_sparam[s]) addr = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, f77_spstr[s], NULL);
    else addr = f77_sval[s];
    if (lx_t == T_LP && f77_srank[s] > 0) {
        f77_tok();
        addr = f77_subscript_addr(s);
    }
    if (lx_t == T_LP) return f77_substring(addr, lenv, clen);
    ex_ty = TY_CHAR;
    ex_hi = -1;
    ex_len = lenv;
    ex_clen = clen;
    return addr;
}

/* Call a CHARACTER-valued function: hidden result buffer and length
 * first, then the actuals, then their hidden lengths.  `rlen` is the
 * result length the caller knows. */
/* A call to a COMPLEX*16 FUNCTION: the result is four words, past the
 * two return registers, so -- as for a CHARACTER function -- the caller
 * passes a 16-byte temporary as hidden argument 0, the dummies follow, and
 * the callee writes its result there (f77_bind_unit binds the function's
 * name to that address).  fptr >= 0: a dummy procedure, called through it. */
static int f77_dcx_fcall(char *name, int fptr, int *addrs, int *lens, int nargs) {
    int out;
    int cb;
    int nc;
    int ai;
    int r;
    out = f77_dcx_temp();
    cb = h_ncarg;
    f77_carg(f77_dcx_arg(out), 0);
    nc = 1;
    for (ai = 0; ai < nargs; ai++) { f77_carg(addrs[ai], 0); nc = nc + 1; }
    for (ai = 0; ai < nargs; ai++) {
        if (lens[ai] < 0) continue;
        f77_carg(lens[ai], 0);
        nc = nc + 1;
    }
    if (fptr >= 0) r = hi_emit(HI_CALLP, TY_INT, fptr, -1, nc, NULL);
    else r = hi_emit(HI_CALL, TY_INT, -1, -1, nc, name);
    h_cbase[r] = cb;
    ex_ty = TY_DCPLX; ex_hi = -1; ex_log = 0;
    return f77_dcx_mark(out);
}

static int f77_char_fcall(char *name, int rlen, int *addrs, int *lens, int nargs) {
    int tmp;
    int cb;
    int ai;
    int r;
    int nc;
    if (rlen < 0) {
        f77_error("the length of this CHARACTER function is not known at the call");
        rlen = 1;
    }
    tmp = f77_char_temp(rlen);
    cb = h_ncarg;
    f77_carg(tmp, 0);
    f77_carg(f77_iconst(rlen), 0);
    nc = 2;
    for (ai = 0; ai < nargs; ai++) {
        f77_carg(addrs[ai], 0);
        nc = nc + 1;
    }
    for (ai = 0; ai < nargs; ai++) {
        if (lens[ai] < 0) continue;
        f77_carg(lens[ai], 0);
        nc = nc + 1;
    }
    r = hi_emit(HI_CALL, TY_INT, -1, -1, nc, name);
    h_cbase[r] = cb;
    ex_ty = TY_CHAR;
    ex_hi = -1;
    ex_clen = rlen;
    ex_len = f77_iconst(rlen);
    return tmp;
}

static int f77_primary(void) {
    int v;
    int s;
    int save;

    ex_log = 0;
    if (lx_t == T_ICON) { v = f77_iconst(lex_ival); ex_ty = TY_INT; f77_tok(); return v; }
    if (lx_t == T_RCON) { v = f77_rconst(lex_dval); ex_ty = TY_FLOAT; f77_tok(); return v; }
    if (lx_t == T_DCON) {
        v = f77_dconst(lex_dval, &ex_hi); ex_ty = TY_DOUBLE; f77_tok(); return v;
    }
    if (lx_t == T_TRUE)  { v = f77_iconst(1); ex_ty = TY_INT; ex_log = 1; f77_tok(); return v; }
    if (lx_t == T_FALSE) { v = f77_iconst(0); ex_ty = TY_INT; ex_log = 1; f77_tok(); return v; }
    if (lx_t == T_SCON) {
        v = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, lex_sidx, NULL);
        ex_ty = TY_CHAR;
        ex_hi = -1;
        ex_clen = lex_slen;
        ex_len = f77_iconst(lex_slen);
        f77_tok();
        return v;
    }
    if (lx_t == T_LP && lx_rp < lx_rpe && *lx_rp == '/') {
        /* (/ ... /): f77_acon_assign's, and TRANSFER's MOLD */
        f77_error("an array constructor is supported only as the right side of an array assignment");
        f77_tok();
        ex_ty = TY_INT;
        return f77_iconst(0);
    }
    if (lx_t == T_LP) {
        f77_tok();
        v = f77_expr();
        if (lx_t == T_COMMA) {
            /* (re, im): a COMPLEX constant (F77 takes constants; any
             * real expressions are accepted) */
            /* COMPLEX*16 when a part is DOUBLE PRECISION, as gfortran
             * has it; each part converted from its own type */
            int re; int im; int rty;
            int vty; int vhi; int ihi;
            vty = ex_ty; vhi = ex_hi;
            f77_tok();
            im = f77_expr();
            rty = ex_ty; ihi = ex_hi;
            if (lx_t != T_RP) f77_error("expected ) after complex constant");
            f77_tok();
            ex_log = 0;
            if (vty == TY_DOUBLE || rty == TY_DOUBLE) {
                re = f77_cvt(v, &vhi, vty, TY_DOUBLE);
                im = f77_cvt(im, &ihi, rty, TY_DOUBLE);
                ex_ty = TY_DCPLX; ex_hi = -1;
                return f77_dcx_make(re, vhi, im, ihi);
            }
            re = f77_cvt(v, &vhi, vty, TY_FLOAT);
            im = f77_cvt(im, &ihi, rty, TY_FLOAT);
            ex_ty = TY_CPLX; ex_hi = im;
            return re;
        }
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
        if (save == TY_DCPLX) {
            int re; int reh; int im; int imh;
            re = f77_dcx_part(v, 0, &reh);
            im = f77_dcx_part(v, 1, &imh);
            re = f77_fp64_pair1("__fp64_neg", re, reh, &reh);
            im = f77_fp64_pair1("__fp64_neg", im, imh, &imh);
            ex_ty = TY_DCPLX; ex_hi = -1;
            return f77_dcx_make(re, reh, im, imh);
        }
        if (save == TY_CPLX) {
            ex_ty = TY_CPLX;
            ex_hi = hi_emit(HI_FNEG, TY_FLOAT, vhi, -1, 0, NULL);
            return hi_emit(HI_FNEG, TY_FLOAT, v, -1, 0, NULL);
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

        /* Inside a FUNCTION its name -- and an ENTRY's -- is the result
         * variable (F77 15.5.1; F90 12.5.2.2 without RESULT), so F(N-1)
         * there subscripts a scalar: F77 has no recursion, and F90's
         * direct recursion needs RESULT (milestone 4).  It had compiled
         * as a recursive call. */
        if (u >= 0 && f77_result_sym >= 0 && f77_inline_depth == 0 &&
            lx_rp < lx_rpe && *lx_rp == '(') {
            int k;
            int is_res;
            is_res = strcmp(nm, f77_sname[f77_result_sym]) == 0;
            for (k = 0; k < f77_stfb_n; k++)
                if (f77_stfb_sym[k] == f77_result_sym && strcmp(f77_stfb_name[k], nm) == 0) is_res = 1;
            if (is_res) {
                f77_error("a FUNCTION's name is its result here: a recursive reference needs RESULT, which is not supported yet");
                ex_ty = TY_INT;
                return f77_iconst(0);
            }
        }

        /* A statement function of this unit shadows everything. */
        {
            int si;
            si = f77_stf_find(nm);
            if (si >= 0) return f77_stf_expand(si);
        }

        /* Intrinsics, unless a unit in this file defines the name --
         * a user FUNCTION shadows the intrinsic of the same name -- or
         * a dummy argument carries it (FF324(NINT, N) in the FCVS). */
        in = (u < 0) ? f77_intrinsic(nm) : IN_NONE;
        {
            int ds;
            ds = f77_sym_find(nm);
            if (ds >= 0 && (f77_sarg[ds] || f77_sexternal[ds])) in = IN_NONE;
            /* An intrinsic's name is the intrinsic only where it is
             * referenced as a function; not followed by '(' it is a
             * local variable (INTEGER LEN/30000/), and an array of that
             * name is the array.  (Found by fort370.) */
            if (in != IN_NONE && !(lx_rp < lx_rpe && *lx_rp == '(')) in = IN_NONE;
            if (in != IN_NONE && ds >= 0 && f77_srank[ds] > 0) in = IN_NONE;
        }
        if (in != IN_NONE) {
            int a; int ahi; int aty;
            int b; int bhi; int bty;
            int rty;
            int qzero_hi;
            qzero_hi = -1;
            f77_tok();
            if (lx_t != T_LP) { f77_error("intrinsic needs arguments"); return f77_iconst(0); }
            f77_tok();
            if ((in >= IN_MAXLOC && in <= IN_MINVAL) || (in >= IN_SUM && in <= IN_DOTP)) return f77_reduce(in);
            if (in >= IN_SIZE && in <= IN_UBOUND) return f77_arr_inquiry(in);
            if (in == IN_TRANSFER) {
                f77_error("TRANSFER is supported only as an actual argument");
                ex_ty = TY_INT;
                return f77_iconst(0);
            }
            if (in == IN_INQUIRY) {
                /* a constant of the argument's type: an array names its
                 * elements' type, and an expression's value is unused */
                int as;
                int t;
                int iv;
                double dv;
                if (lx_t == T_NAME && (as = f77_sym_find(lex_name)) >= 0 && f77_srank[as] > 0 &&
                    lx_rp < lx_rpe && (*lx_rp == ')' || *lx_rp == ',')) {
                    t = f77_sty[as];
                    f77_tok();
                } else {
                    f77_expr();
                    t = ex_ty;
                }
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                t = f77_inquiry(nm, t, &iv, &dv);
                ex_hi = -1; ex_log = 0;
                if (t == TY_DOUBLE) { ex_ty = TY_DOUBLE; return f77_dconst(dv, &ex_hi); }
                if (t == TY_FLOAT) { ex_ty = TY_FLOAT; return f77_rconst(dv); }
                ex_ty = TY_INT;
                return f77_iconst(t == TY_INT ? iv : 0);
            }
            if (in == IN_ALLOCATED) {
                int as;
                if (lx_t != T_NAME || (as = f77_sym_find(lex_name)) < 0 || !f77_salloc[as]) {
                    f77_error("ALLOCATED needs an ALLOCATABLE array");
                    return f77_iconst(0);
                }
                f77_tok();
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                ex_ty = TY_INT; ex_hi = -1; ex_log = 1;
                return hi_emit(HI_SNE, TY_INT, f77_desc_base(as), f77_iconst(0), 0, NULL);
            }
            a = f77_expr(); aty = ex_ty; ahi = ex_hi;

            if (in >= IN_IAND && in <= IN_IBCLR) {
                int v[3];
                int n;
                int want;
                v[0] = a;
                n = 1;
                if (aty != TY_INT) f77_error("this intrinsic needs INTEGER arguments");
                while (lx_t == T_COMMA && n < 3) {
                    f77_tok();
                    v[n] = f77_expr();
                    if (ex_ty != TY_INT) f77_error("this intrinsic needs INTEGER arguments");
                    n = n + 1;
                }
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                want = in == IN_NOT ? 1 : (in == IN_ISHFTC || in == IN_IBITS) ? 3 : 2;
                if (n != want) { f77_error("wrong number of arguments to a bit intrinsic"); return f77_iconst(0); }
                ex_ty = TY_INT; ex_hi = -1; ex_log = 0;
                return f77_bits(in, v);
            }

            if (in == IN_DCMPLX ||
                (aty == TY_DCPLX && (in == IN_AIMAG || in == IN_CONJG || in == IN_REAL || in == IN_DBLE))) {
                /* COMPLEX*16: DCMPLX(x [,y]); and of a COMPLEX*16, as
                 * gfortran has them, REAL, DBLE and DREAL the real part
                 * and AIMAG and DIMAG the imaginary, each DOUBLE
                 * PRECISION, CONJG and DCONJG a COMPLEX*16 */
                int re;
                int rehi;
                int im;
                int imhi;
                if (in == IN_DCMPLX) {
                    if (lx_t == T_COMMA) {
                        rehi = ahi;
                        re = f77_cvt(a, &rehi, aty, TY_DOUBLE);
                        f77_tok();
                        im = f77_expr();
                        imhi = ex_hi;
                        if (ex_ty == TY_CPLX || ex_ty == TY_DCPLX) f77_error("DCMPLX(x, y) needs x and y not COMPLEX");
                        im = f77_cvt(im, &imhi, ex_ty, TY_DOUBLE);
                        if (aty == TY_CPLX || aty == TY_DCPLX) f77_error("DCMPLX(x, y) needs x and y not COMPLEX");
                        if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                        ex_ty = TY_DCPLX; ex_hi = -1; ex_log = 0;
                        return f77_dcx_make(re, rehi, im, imhi);
                    }
                    if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                    a = f77_cvt(a, &ahi, aty, TY_DCPLX);
                    ex_ty = TY_DCPLX; ex_hi = -1; ex_log = 0;
                    return a;
                }
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                ex_log = 0;
                if (in == IN_REAL || in == IN_DBLE || in == IN_AIMAG) {
                    re = f77_dcx_part(a, in == IN_AIMAG ? 1 : 0, &rehi);
                    ex_ty = TY_DOUBLE; ex_hi = rehi;
                    return re;
                }
                /* CONJG: the imaginary part's sign bit flipped */
                re = f77_dcx_part(a, 0, &rehi);
                im = f77_dcx_part(a, 1, &imhi);
                imhi = hi_emit(HI_XOR, TY_INT, imhi, f77_iconst((int)0x80000000u), 0, NULL);
                ex_ty = TY_DCPLX; ex_hi = -1;
                return f77_dcx_make(re, rehi, im, imhi);
            }
            if (in == IN_AIMAG || in == IN_CONJG || in == IN_CMPLX) {
                if (in == IN_CMPLX) {
                    int im;
                    int a0;
                    int a0hi;
                    a0 = a; a0hi = ahi;
                    a = f77_cvt(a, &ahi, aty, TY_FLOAT);
                    if (lx_t == T_COMMA) {
                        f77_tok();
                        im = f77_expr();
                        im = f77_cvt(im, &ex_hi, ex_ty, TY_FLOAT);
                    } else if (aty == TY_CPLX) {
                        im = ahi;
                    } else if (aty == TY_DCPLX) {   /* CMPLX of a COMPLEX*16: each part narrowed */
                        im = f77_cvt(a0, &a0hi, TY_DCPLX, TY_CPLX);
                        im = a0hi;
                    } else {
                        im = f77_rconst(0.0);
                    }
                    if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                    ex_ty = TY_CPLX; ex_hi = im; ex_log = 0;
                    return a;
                }
                if (aty != TY_CPLX) { a = f77_cvt(a, &ahi, aty, TY_CPLX); }
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                if (in == IN_AIMAG) { ex_ty = TY_FLOAT; ex_hi = -1; return ahi; }
                ex_ty = TY_CPLX;
                ex_hi = hi_emit(HI_FNEG, TY_FLOAT, ahi, -1, 0, NULL);
                return a;
            }
            if (aty == TY_DCPLX && (in == IN_SQRT || in == IN_EXP || in == IN_LOG ||
                                    in == IN_SIN || in == IN_COS)) {
                /* the COMPLEX*16 transcendentals: in the runtime, into a
                 * fresh temporary */
                int args[2];
                int out;
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                out = f77_dcx_temp();
                args[0] = f77_dcx_arg(out); args[1] = f77_dcx_arg(a);
                f77_call_i(in == IN_SQRT ? "f77_zsqrt" : in == IN_EXP ? "f77_zexp" :
                           in == IN_LOG ? "f77_zlog" : in == IN_SIN ? "f77_zsin" : "f77_zcos", 2, args);
                ex_ty = TY_DCPLX; ex_hi = -1; ex_log = 0;
                return f77_dcx_mark(out);
            }
            if (aty != TY_DCPLX &&
                (strcmp(nm, "CDSQRT") == 0 || strcmp(nm, "ZSQRT") == 0 || strcmp(nm, "CDEXP") == 0 ||
                 strcmp(nm, "ZEXP") == 0 || strcmp(nm, "CDLOG") == 0 || strcmp(nm, "ZLOG") == 0 ||
                 strcmp(nm, "CDSIN") == 0 || strcmp(nm, "ZSIN") == 0 || strcmp(nm, "CDCOS") == 0 ||
                 strcmp(nm, "ZCOS") == 0 || strcmp(nm, "CDABS") == 0 || strcmp(nm, "ZABS") == 0)) {
                f77_error("this intrinsic needs a COMPLEX*16 argument");
                return f77_iconst(0);
            }
            if (aty == TY_DCPLX && in == IN_ABS) {
                /* |z| of a COMPLEX*16: hypot in the runtime, a double back */
                int args[1];
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                args[0] = f77_dcx_arg(a);
                a = f77_call_i("f77_zabs", 1, args);
                ex_hi = hi_emit(HI_CALLHI, TY_INT, a, -1, 0, NULL);
                ex_ty = TY_DOUBLE; ex_log = 0;
                return a;
            }
            if (aty == TY_CPLX && (in == IN_ABS || in == IN_SQRT || in == IN_EXP ||
                                   in == IN_LOG || in == IN_SIN || in == IN_COS)) {
                /* CABS and the complex transcendentals: in the runtime */
                int args[4];
                int out;
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                if (in == IN_ABS) {
                    args[0] = a; args[1] = ahi;
                    a = f77_call_i("f77_cabs", 2, args);
                    h_ty[a] = TY_FLOAT;
                    ex_ty = TY_FLOAT; ex_hi = -1;
                    return a;
                }
                out = f77_char_temp(8);
                args[0] = out; args[1] = a; args[2] = ahi;
                f77_call_i(in == IN_SQRT ? "f77_csqrt" : in == IN_EXP ? "f77_cexp" :
                           in == IN_LOG ? "f77_clog" : in == IN_SIN ? "f77_csin" : "f77_ccos",
                           3, args);
                return f77_load_at(out, TY_CPLX);
            }
            if (aty != TY_CPLX &&
                (strcmp(nm, "CSQRT") == 0 || strcmp(nm, "CEXP") == 0 || strcmp(nm, "CLOG") == 0 ||
                 strcmp(nm, "CSIN") == 0 || strcmp(nm, "CCOS") == 0 || strcmp(nm, "CABS") == 0)) {
                f77_error("this intrinsic needs a COMPLEX argument");
                return f77_iconst(0);
            }

            if (in == IN_TRIM || in == IN_LEN_TRIM) {
                /* the string less its trailing blanks: its own storage,
                 * a shorter length -- a substring (1:LEN_TRIM) */
                int args[2];
                int l;
                if (aty != TY_CHAR) { f77_error("this intrinsic needs a CHARACTER argument"); return f77_iconst(0); }
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                args[0] = a; args[1] = ex_len;
                l = f77_call_i("f77_len_trim", 2, args);
                ex_hi = -1;
                if (in == IN_LEN_TRIM) { ex_ty = TY_INT; return l; }
                ex_ty = TY_CHAR; ex_clen = -1; ex_len = l;
                return a;
            }
            if (in >= IN_LEN && in <= IN_LLT) {
                int alen;
                int args[4];
                alen = ex_len;
                if (in == IN_CHAR) {
                    int tmp;
                    a = f77_cvt(a, &ahi, aty, TY_INT);
                    if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                    tmp = f77_char_temp(1);
                    hi_emit(HI_STORE, TY_CHAR, tmp, a, 0, NULL);
                    ex_ty = TY_CHAR; ex_hi = -1; ex_clen = 1; ex_len = f77_iconst(1);
                    return tmp;
                }
                if (aty != TY_CHAR) { f77_error("this intrinsic needs a CHARACTER argument"); return f77_iconst(0); }
                if (in == IN_LEN) {
                    if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                    ex_ty = TY_INT; ex_hi = -1;
                    return alen;
                }
                if (in == IN_ICHAR) {
                    if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                    ex_ty = TY_INT; ex_hi = -1;
                    return hi_emit(HI_LOAD, TY_CHAR | TY_UNSIGNED, a, -1, 0, NULL);
                }
                /* INDEX and the lexical comparisons: two CHARACTER args */
                if (lx_t != T_COMMA) { f77_error("this intrinsic needs two arguments"); return f77_iconst(0); }
                f77_tok();
                b = f77_expr(); bty = ex_ty;
                if (bty != TY_CHAR) { f77_error("this intrinsic needs CHARACTER arguments"); return f77_iconst(0); }
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                args[0] = a; args[1] = alen; args[2] = b; args[3] = ex_len;
                ex_ty = TY_INT; ex_hi = -1;
                if (in == IN_INDEX) return f77_call_i("f77_index", 4, args);
                a = f77_call_i("f77_strcmp", 4, args);
                ex_log = 1;
                if (in == IN_LGE) return hi_emit(HI_SGE, TY_INT, a, f77_iconst(0), 0, NULL);
                if (in == IN_LGT) return hi_emit(HI_SGT, TY_INT, a, f77_iconst(0), 0, NULL);
                if (in == IN_LLE) return hi_emit(HI_SLE, TY_INT, a, f77_iconst(0), 0, NULL);
                return hi_emit(HI_SLT, TY_INT, a, f77_iconst(0), 0, NULL);
            }

            if (in == IN_ABS) {
                a = f77_abs(a, aty, &ahi);
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                ex_ty = aty; ex_hi = ahi;
                return a;
            }
            if (in == IN_CEILING || in == IN_FLOOR) {
                /* the INTEGER next above (below) x: truncate, and step
                 * by one when the truncation went the other way */
                int t; int b; int bhi2; int c;
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                if (aty != TY_FLOAT && aty != TY_DOUBLE) { f77_error("CEILING and FLOOR need a REAL or DOUBLE PRECISION argument"); return f77_iconst(0); }
                bhi2 = ahi;
                t = f77_cvt(a, &bhi2, aty, TY_INT);
                bhi2 = -1;
                b = f77_cvt(t, &bhi2, TY_INT, aty);
                if (aty == TY_DOUBLE) {
                    c = in == IN_CEILING ? f77_fp64_call2("__fp64_lt", b, bhi2, a, ahi, NULL)
                                         : f77_fp64_call2("__fp64_lt", a, ahi, b, bhi2, NULL);
                } else {
                    c = in == IN_CEILING ? hi_emit(HI_FLT, TY_INT, b, a, 0, NULL)
                                         : hi_emit(HI_FLT, TY_INT, a, b, 0, NULL);
                }
                ex_ty = TY_INT; ex_hi = -1; ex_log = 0;
                return hi_emit(in == IN_CEILING ? HI_ADD : HI_SUB, TY_INT, t, c, 0, NULL);
            }
            if (in == IN_EXPONENT) {
                /* frexp's exponent, in the runtime; 0 for 0 */
                int cb; int r;
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                if (aty != TY_FLOAT && aty != TY_DOUBLE) { f77_error("EXPONENT needs a REAL or DOUBLE PRECISION argument"); return f77_iconst(0); }
                a = f77_cvt(a, &ahi, aty, TY_DOUBLE);
                cb = h_ncarg;
                f77_carg(a, 1);
                f77_carg(ahi, 2);
                r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "f77_exponent");
                h_cbase[r] = cb;
                ex_ty = TY_INT; ex_hi = -1; ex_log = 0;
                return r;
            }
            if (in == IN_INT || in == IN_REAL || in == IN_DBLE) {
                rty = (in == IN_INT) ? TY_INT : (in == IN_REAL ? TY_FLOAT : TY_DOUBLE);
                if (lx_t == T_COMMA && in != IN_DBLE) {
                    /* REAL(X, KIND), INT(X, KIND) (Fortran 90) */
                    f77_tok();
                    if (lx_t == T_NAME && strcmp(lex_name, "KIND") == 0 && lx_rp < lx_rpe && *lx_rp == '=') { f77_tok(); f77_tok(); }
                    {
                        int k;
                        rty = f77_const_int(&k) ? f77_kind_ty(rty, k) : -1;
                    }
                    if (rty < 0) { f77_error("this kind is not supported"); return f77_iconst(0); }
                }
                a = f77_cvt(a, &ahi, aty, rty);
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                ex_ty = rty; ex_hi = ahi;
                if (rty == TY_FLOAT) ex_hi = -1;
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

            if (in == IN_AINT || in == IN_ANINT || in == IN_NINT) {
                /* trunc / round by the libm names the DBT intercepts
                 * (math_soft.c has them for the interpreters).  The
                 * D-specific names force the double path; NINT is
                 * ANINT followed by the integer conversion. */
                int dbl;
                char *fn;
                if (lx_t == T_RP) f77_tok(); else f77_error("expected )");
                dbl = (aty == TY_DOUBLE) || (in != IN_NINT && nm[0] == 'D') ||
                      strcmp(nm, "IDNINT") == 0;
                if (dbl) {
                    a = f77_cvt(a, &ahi, aty, TY_DOUBLE);
                    fn = (in == IN_AINT) ? "trunc" : "round";
                    a = f77_libm_d1(fn, a, ahi, &ahi);
                    aty = TY_DOUBLE;
                } else {
                    a = f77_cvt(a, &ahi, aty, TY_FLOAT);
                    fn = (in == IN_AINT) ? "truncf" : "roundf";
                    a = f77_libm_f1(fn, a);
                    aty = TY_FLOAT;
                }
                if (in == IN_NINT) {
                    a = f77_cvt(a, &ahi, aty, TY_INT);
                    aty = TY_INT;
                }
                ex_ty = aty; ex_hi = ahi;
                return a;
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
                } else if (in == IN_DIM) {
                    /* max(a - b, 0) in the operands' type */
                    int zero;
                    int zhi;
                    zhi = -1;
                    if (rty == TY_DOUBLE) {
                        a = f77_fp64_call2("__fp64_sub", a, ahi, b, bhi, &ahi);
                        zero = f77_dconst(0.0, &zhi);
                    } else if (rty == TY_FLOAT) {
                        a = hi_emit(HI_FSUB, TY_FLOAT, a, b, 0, NULL);
                        zero = f77_rconst(0.0);
                    } else {
                        a = hi_emit(HI_SUB, TY_INT, a, b, 0, NULL);
                        zero = f77_iconst(0);
                    }
                    a = f77_minmax(1, a, ahi, zero, zhi, rty, &ahi);
                } else if (in == IN_DPROD) {
                    a = f77_cvt(a, &ahi, rty, TY_DOUBLE);
                    b = f77_cvt(b, &bhi, rty, TY_DOUBLE);
                    a = f77_fp64_call2("__fp64_mul", a, ahi, b, bhi, &ahi);
                    rty = TY_DOUBLE;
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
            int nc;
            int r;
            int rty;
            int addrs[64];
            int lens[64];
            int ai;
            int mk;
            mk = f77_tfree_mark();
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
                        if (nargs < 64) { addrs[nargs] = f77_actual_addr(); lens[nargs] = f77_act_len; }
                        else f77_actual_addr();
                        nargs = nargs + 1;
                        if (lx_t != T_COMMA) break;
                        f77_tok();
                    }
                }
                if (lx_t != T_RP) f77_error("expected ) after arguments");
                else f77_tok();
            }
            rty = f77_urty_of(u);
            {
                /* fort370: the caller's type for the name, declared or
                 * implicit, is the definition's (fort370 issue 21).  The
                 * call takes the definition's, so the value was right
                 * here, but a compiler that compiles units apart reads
                 * it by the caller's, and gfortran refuses it: the
                 * program does not mean what it says. */
                int s;
                int ulog;
                int cty;
                int clog;
                /* looked up, not made: a symbol made here moved the frame */
                s = f77_sym_at(nm, f77_scope_base);
                /* an internal or module procedure has an explicit
                 * interface (Fortran 90): its definition types the
                 * reference, and the caller declares nothing */
                if (f77_uhost[u] >= 0) goto typed;
                f77_need_type(s, nm);
                if (s >= 0) { cty = f77_sty[s]; clog = f77_slog[s]; }
                else {
                    cty = f77_implicit_ty(nm);
                    clog = (f77_imp_ready && nm[0] >= 'A' && nm[0] <= 'Z') ? f77_imp_log[nm[0] - 'A'] : 0;
                }
                ulog = f77_uentry_parent[u] >= 0 ? f77_urlog[f77_uentry_parent[u]] : f77_urlog[u];
                if (cty >= 0 && (cty != rty || (rty == TY_INT && clog != ulog))) {
                    char m[256];
                    sprintf(m, "%s is %s here but %s where it is defined (%s)", nm,
                            f77_tyname(cty, clog), f77_tyname(rty, ulog), lx_where_text(f77_uline[u]));
                    f77_error(m);
                }
            }
        typed:
            if (rty == TY_CHAR) {
                int rl;
                rl = f77_urlen[u];
                if (rl < 0) rl = f77_slen[f77_sym(nm)];
                r = f77_char_fcall(f77_ulink[u], rl, addrs, lens, nargs);
                f77_tfree_to(mk);
                return r;
            }
            if (rty == TY_DCPLX) {
                r = f77_dcx_fcall(f77_ulink[u], -1, addrs, lens, nargs);
                f77_tfree_to(mk);
                return r;
            }
            if (f77_can_inline(u)) {
                /* Splice the body, then read its result variable. */
                int rs;
                rs = f77_inline_unit_body(u, addrs, nargs);
                f77_tfree_to(mk);
                if (rs >= 0) return f77_load_sym(rs);
                ex_ty = TY_INT;
                return f77_iconst(0);
            }
            cb = h_ncarg;
            nc = 0;
            ai = 0;
            while (ai < nargs) {
                f77_carg(addrs[ai], 0);
                nc = nc + 1;
                ai = ai + 1;
            }
            for (ai = 0; ai < nargs; ai++) {
                if (lens[ai] < 0) continue;
                f77_carg(lens[ai], 0);
                nc = nc + 1;
            }
            r = hi_emit(HI_CALL, rty == TY_DOUBLE ? TY_INT : rty == TY_CPLX ? TY_FLOAT : rty, -1, -1,
                        nc, f77_ulink[u]);
            h_cbase[r] = cb;
            ex_ty = rty;
            ex_log = f77_urlog[u];
            if (rty == TY_DOUBLE)
                ex_hi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
            else if (rty == TY_CPLX)
                ex_hi = hi_emit(HI_CALLHI, TY_FLOAT, r, -1, 0, NULL);
            f77_tfree_to(mk);
            return r;
        }
        s = f77_sym(nm);
        f77_tok();
        if (f77_sty[s] == TY_CHAR &&
            !(lx_t == T_LP && f77_srank[s] == 0 && !f77_paren_has_colon()))
            return f77_char_ref(s);
        if (f77_sparam[s]) {            /* named constant: fold in place */
            ex_ty = f77_sty[s];
            ex_log = f77_slog[s];
            if (ex_ty == TY_CPLX) { ex_hi = f77_rconst(f77_spdval2[s]); return f77_rconst(f77_spdval[s]); }
            if (ex_ty == TY_DCPLX) {
                int re; int rehi; int im; int imhi;
                re = f77_dconst(f77_spdval[s], &rehi);
                im = f77_dconst(f77_spdval2[s], &imhi);
                ex_hi = -1;
                return f77_dcx_make(re, rehi, im, imhi);
            }
            if (ex_ty == TY_DOUBLE) return f77_dconst(f77_spdval[s], &ex_hi);
            if (ex_ty == TY_FLOAT) return f77_rconst(f77_spdval[s]);
            return f77_iconst(f77_spival[s]);
        }
        if (f77_aex_k > 0 && lx_t == T_LP && f77_srank[s] > 0 && f77_sty[s] != TY_CHAR && f77_paren_has_colon())
            return f77_aex_elem(s, 0);           /* a section's element */
        if (lx_t == T_LP && f77_srank[s] > 0) {
            int addr;
            f77_tok();
            addr = f77_subscript_addr(s);
            v = f77_load_at(addr, f77_mty(s));
            ex_log = f77_slog[s];
            return v;
        }
        if (lx_t == T_LP) {
            /* Not an array and not a FUNCTION in this file: an
             * external function reference -- or a dummy procedure,
             * called through the address the caller passed. Leaving
             * the ( unconsumed used to drop the rest of the expression
             * (GitHub #20). */
            int cb;
            int nargs;
            int nc;
            int r;
            int addrs[64];
            int lens[64];
            int ai;
            int rty;
            int fptr;
            int mk;
            int tys[64];
            char *cname;
            int rlog;
            mk = f77_tfree_mark();
            fptr = (f77_sarg[s] && f77_srank[s] == 0) ? f77_sval[s] : -1;
            f77_need_type(s, nm);
            nargs = 0;
            f77_tok();
            if (lx_t != T_RP) {
                for (;;) {
                    if (nargs < 64) { addrs[nargs] = f77_actual_addr(); lens[nargs] = f77_act_len; tys[nargs] = f77_act_ty; }
                    else f77_actual_addr();
                    nargs = nargs + 1;
                    if (lx_t != T_COMMA) break;
                    f77_tok();
                }
            }
            if (lx_t != T_RP) f77_error("expected ) after arguments");
            else f77_tok();
            /* The type is the caller's declaration of the name (or the
             * implicit rule): INTEGER IRAND / CHARACTER*8 CDATE -- but a
             * USEd module procedure's is its own, and a generic's the
             * specific's its actuals pick. */
            rty = f77_sty[s];
            rlog = f77_slog[s];
            cname = strdup(nm);
            if (f77_sme[s] >= 0) {
                int e;
                e = f77_sme[s];
                if (f77_me_kind[e] == F77_ME_GENERIC) e = f77_generic_pick(e, tys, nargs > 64 ? 64 : nargs);
                if (e >= 0 && f77_me_pkind[e] != F77_UNIT_FUNC) { f77_error("a SUBROUTINE referenced as a FUNCTION"); e = -1; }
                if (e >= 0) { rty = f77_me_ty[e]; rlog = f77_me_log[e]; cname = f77_me_str[e]; }
            } else if (f77_slink[s]) cname = f77_slink[s];
            if (rty == TY_CHAR || rty == TY_DCPLX) {
                r = rty == TY_CHAR ? f77_char_fcall(cname, f77_slen[s], addrs, lens, nargs)
                                   : f77_dcx_fcall(cname, fptr, addrs, lens, nargs);
                f77_tfree_to(mk);
                return r;
            }
            cb = h_ncarg;
            nc = 0;
            ai = 0;
            while (ai < nargs) {
                f77_carg(addrs[ai], 0);
                nc = nc + 1;
                ai = ai + 1;
            }
            for (ai = 0; ai < nargs; ai++) {
                if (lens[ai] < 0) continue;
                f77_carg(lens[ai], 0);
                nc = nc + 1;
            }
            /* nm is this frame's buffer and hi_emit keeps the pointer:
             * the callee's name must outlive the parse (GitHub #20 --
             * the call came out as "EN", whatever the buffer held by
             * codegen time). */
            if (fptr >= 0)
                r = hi_emit(HI_CALLP, rty == TY_DOUBLE ? TY_INT : rty == TY_CPLX ? TY_FLOAT : rty, fptr, -1,
                            nc, NULL);
            else
                r = hi_emit(HI_CALL, rty == TY_DOUBLE ? TY_INT : rty == TY_CPLX ? TY_FLOAT : rty, -1, -1,
                            nc, cname);
            h_cbase[r] = cb;
            ex_ty = rty;
            ex_log = rlog;
            ex_hi = -1;
            if (rty == TY_DOUBLE) ex_hi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
            else if (rty == TY_CPLX) ex_hi = hi_emit(HI_CALLHI, TY_FLOAT, r, -1, 0, NULL);
            f77_tfree_to(mk);
            return r;
        }
        if (f77_srank[s] > 0 && f77_aex_k > 0 && f77_sty[s] != TY_CHAR)
            return f77_aex_elem(s, 1);           /* a whole array's element */
        if (f77_srank[s] > 0) {
            /* A whole array as an operand: F77 has none, and F90's array
             * expressions are milestone 3.  It had compiled, X = L giving
             * X a word of L's storage.  (An actual argument, an I/O list
             * item and a DATA name reach their own code, not this.) */
            f77_error("a whole array in an expression is not supported yet");
            ex_ty = f77_sty[s]; ex_hi = -1;
            return f77_iconst(0);
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
        if (aty == TY_DCPLX || bty == TY_DCPLX ||
            (aty == TY_CPLX && bty == TY_DOUBLE) || (aty == TY_DOUBLE && bty == TY_CPLX)) {
            /* COMPLEX*16: z**n by binary exponentiation in libgfortran's
             * order, z**w as exp(w log z), both in the runtime */
            int out;
            int args[3];
            out = f77_dcx_temp();
            a = f77_cvt(a, &ahi, aty, TY_DCPLX);
            args[0] = f77_dcx_arg(out); args[1] = f77_dcx_arg(a);
            if (bty == TY_INT) {
                args[2] = b;
                f77_call_i("f77_zpow_i", 3, args);
            } else {
                b = f77_cvt(b, &bhi, bty, TY_DCPLX);
                args[2] = f77_dcx_arg(b);
                f77_call_i("f77_zpow_z", 3, args);
            }
            ex_ty = TY_DCPLX; ex_hi = -1;
            return f77_dcx_mark(out);
        }
        if (aty == TY_CPLX || bty == TY_CPLX) {
            /* z**n by repeated squaring, z**w as exp(w log z): both in
             * the runtime, which writes the pair to a frame temporary. */
            int out;
            int args[6];
            out = f77_char_temp(8);
            if (bty == TY_INT) {
                args[0] = out; args[1] = a; args[2] = ahi; args[3] = b;
                f77_call_i("f77_cpow_i", 4, args);
            } else {
                b = f77_cvt(b, &bhi, bty, TY_CPLX);
                args[0] = out; args[1] = a; args[2] = ahi; args[3] = b; args[4] = bhi;
                f77_call_i("f77_cpow_c", 5, args);
            }
            return f77_load_at(out, TY_CPLX);
        }
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
            f77_carg(a, 1);
            f77_carg(ahi, 2);
            f77_carg(b, 0);
            r = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "f77_dpow_i");
            h_cbase[r] = cb;
            ex_hi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
            ex_ty = TY_DOUBLE;
            return r;
        }
        f77_carg(a, 0);
        f77_carg(b, 0);
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

/* a op b, op T_PLUS T_MINUS T_STAR or T_SLASH, the operands balanced to
 * one type first: the result, its type in ex_ty and hi word in ex_hi.
 * What f77_term and f77_arith do for each operator, and the reductions
 * (SUM, PRODUCT, DOT_PRODUCT) for each element. */
static int f77_binop(int op, int a, int aty, int ahi, int b, int bty, int bhi) {
    int rty;
    rty = f77_balance(&a, &ahi, aty, &b, &bhi, bty);
    ex_log = 0;
    if (rty == TY_DCPLX) {
        ex_ty = rty; ex_hi = -1;
        return f77_dcx_binop(op, a, b);
    }
    if (rty == TY_CPLX) {
        int t1; int t2; int re; int im;
        if (op == T_PLUS || op == T_MINUS) {
            re = hi_emit(op == T_PLUS ? HI_FADD : HI_FSUB, TY_FLOAT, a, b, 0, NULL);
            im = hi_emit(op == T_PLUS ? HI_FADD : HI_FSUB, TY_FLOAT, ahi, bhi, 0, NULL);
        } else if (op == T_STAR) {
            /* (ar*br - ai*bi, ar*bi + ai*br) */
            t1 = hi_emit(HI_FMUL, TY_FLOAT, a, b, 0, NULL);
            t2 = hi_emit(HI_FMUL, TY_FLOAT, ahi, bhi, 0, NULL);
            re = hi_emit(HI_FSUB, TY_FLOAT, t1, t2, 0, NULL);
            t1 = hi_emit(HI_FMUL, TY_FLOAT, a, bhi, 0, NULL);
            t2 = hi_emit(HI_FMUL, TY_FLOAT, ahi, b, 0, NULL);
            im = hi_emit(HI_FADD, TY_FLOAT, t1, t2, 0, NULL);
        } else {
            /* in the runtime, in double (f77_cdiv): in float the
             * divisor's square overflowed past about 1.8E19 */
            int args[5];
            int out;
            out = f77_char_temp(8);
            args[0] = out; args[1] = a; args[2] = ahi; args[3] = b; args[4] = bhi;
            f77_call_i("f77_cdiv", 5, args);
            re = f77_load_at(out, TY_CPLX);
            im = ex_hi;
        }
        ex_ty = rty; ex_hi = im; ex_log = 0;
        return re;
    }
    if (rty == TY_DOUBLE) {
        int r; int rhi;
        r = f77_fp64_call2(op == T_PLUS ? "__fp64_add" : op == T_MINUS ? "__fp64_sub" :
                           op == T_STAR ? "__fp64_mul" : "__fp64_div", a, ahi, b, bhi, &rhi);
        ex_ty = rty; ex_hi = rhi; ex_log = 0;
        return r;
    }
    ex_ty = rty; ex_hi = -1;
    if (rty == TY_FLOAT)
        return hi_emit(op == T_PLUS ? HI_FADD : op == T_MINUS ? HI_FSUB : op == T_STAR ? HI_FMUL : HI_FDIV,
                       TY_FLOAT, a, b, 0, NULL);
    return hi_emit(op == T_PLUS ? HI_ADD : op == T_MINUS ? HI_SUB : op == T_STAR ? HI_MUL : HI_DIV,
                   TY_INT, a, b, 0, NULL);
}

static int f77_term(void) {
    int a; int b; int aty; int bty; int ahi; int bhi;
    a = f77_power();
    aty = ex_ty; ahi = ex_hi;
    /* a '/' before ')' closes an array constructor (/ ... /), no division */
    while (lx_t == T_STAR || (lx_t == T_SLASH && !(lx_rp < lx_rpe && *lx_rp == ')'))) {
        int op;
        op = lx_t;
        f77_tok();
        b = f77_power();
        bty = ex_ty; bhi = ex_hi;
        a = f77_binop(op, a, aty, ahi, b, bty, bhi);
        aty = ex_ty; ahi = ex_hi;
    }
    ex_ty = aty; ex_hi = ahi;
    return a;
}

static int f77_arith(void) {
    int a; int b; int aty; int bty; int ahi; int bhi;
    a = f77_term();
    aty = ex_ty; ahi = ex_hi;
    while (lx_t == T_PLUS || lx_t == T_MINUS) {
        int op;
        op = lx_t;
        f77_tok();
        b = f77_term();
        bty = ex_ty; bhi = ex_hi;
        a = f77_binop(op, a, aty, ahi, b, bty, bhi);
        aty = ex_ty; ahi = ex_hi;
    }
    ex_ty = aty; ex_hi = ahi;
    return a;
}

/* Relationals produce an integer 0/1.  The FP comparison set is only
 * FEQ/FLT/FLE, so >, >= and /= are built by swapping operands or
 * inverting the result. */
/* a // b: CHARACTER concatenation into a frame temporary.  Both
 * lengths must be known here (F77 allows an assumed-length operand
 * only on the right of an assignment, which this does not special-case). */
static int f77_concat(void) {
    int a; int b; int aty; int alen; int aclen; int bclen; int tmp; int t;
    a = f77_arith();
    aty = ex_ty; alen = ex_len; aclen = ex_clen;
    while (lx_t == T_CONCAT) {
        f77_tok();
        b = f77_arith();
        if (aty != TY_CHAR || ex_ty != TY_CHAR) {
            f77_error("// needs CHARACTER operands");
            return a;
        }
        bclen = ex_clen;
        if (aclen < 0 || bclen < 0) {
            /* An assumed-length operand: the runtime builds the result
             * in its scratch ring and the length is a run-time sum. */
            int args[4];
            args[0] = a; args[1] = alen; args[2] = b; args[3] = ex_len;
            a = f77_call_i("f77_cat", 4, args);
            alen = hi_emit(HI_ADD, TY_INT, alen, ex_len, 0, NULL);
            aclen = -1;
            continue;
        }
        tmp = f77_char_temp(aclen + bclen);
        f77_char_assign(tmp, f77_iconst(aclen), a, alen);
        t = hi_emit(HI_ADDI, HL_ADDR_TY, tmp, -1, aclen, NULL);
        f77_char_assign(t, f77_iconst(bclen), b, ex_len);
        a = tmp;
        aclen = aclen + bclen;
        alen = f77_iconst(aclen);
    }
    ex_ty = aty; ex_len = alen; ex_clen = aclen;
    if (aty == TY_CHAR) ex_hi = -1;
    return a;
}

static int f77_relat(void) {
    int a; int b; int aty; int bty; int rty; int op; int r; int ahi; int bhi;
    int alen;
    a = f77_concat();
    aty = ex_ty; ahi = ex_hi; alen = ex_len;
    if (lx_t == T_EQ || lx_t == T_NE || lx_t == T_LT ||
        lx_t == T_LE || lx_t == T_GT || lx_t == T_GE) {
        op = lx_t;
        f77_tok();
        b = f77_concat();
        bty = ex_ty; bhi = ex_hi;
        ex_log = 1;
        if (aty == TY_CHAR || bty == TY_CHAR) {
            int args[4];
            if (aty != bty) { f77_error("CHARACTER compared with a numeric"); ex_ty = TY_INT; return f77_iconst(0); }
            args[0] = a; args[1] = alen; args[2] = b; args[3] = ex_len;
            r = f77_call_i("f77_strcmp", 4, args);
            ex_ty = TY_INT; ex_hi = -1;
            if (op == T_EQ) return hi_emit(HI_SEQ, TY_INT, r, f77_iconst(0), 0, NULL);
            if (op == T_NE) return hi_emit(HI_SNE, TY_INT, r, f77_iconst(0), 0, NULL);
            if (op == T_LT) return hi_emit(HI_SLT, TY_INT, r, f77_iconst(0), 0, NULL);
            if (op == T_LE) return hi_emit(HI_SLE, TY_INT, r, f77_iconst(0), 0, NULL);
            if (op == T_GT) return hi_emit(HI_SGT, TY_INT, r, f77_iconst(0), 0, NULL);
            return hi_emit(HI_SGE, TY_INT, r, f77_iconst(0), 0, NULL);
        }
        rty = f77_balance(&a, &ahi, aty, &b, &bhi, bty);
        ex_ty = TY_INT;
        ex_log = 1;
        if (rty == TY_DCPLX) {
            int ar; int arh; int ai; int aih; int br; int brh; int bi; int bih;
            int e1; int e2;
            if (op != T_EQ && op != T_NE) { f77_error("COMPLEX operands allow only .EQ. and .NE."); return f77_iconst(0); }
            ar = f77_dcx_part(a, 0, &arh); ai = f77_dcx_part(a, 1, &aih);
            br = f77_dcx_part(b, 0, &brh); bi = f77_dcx_part(b, 1, &bih);
            e1 = f77_fp64_call2("__fp64_eq", ar, arh, br, brh, NULL);
            e2 = f77_fp64_call2("__fp64_eq", ai, aih, bi, bih, NULL);
            r = hi_emit(HI_AND, TY_INT, e1, e2, 0, NULL);
            if (op == T_NE) r = hi_emit(HI_NOT, TY_INT, r, -1, 0, NULL);
            return r;
        }
        if (rty == TY_CPLX) {
            int e1; int e2;
            if (op != T_EQ && op != T_NE) { f77_error("COMPLEX operands allow only .EQ. and .NE."); return f77_iconst(0); }
            e1 = hi_emit(HI_FEQ, TY_INT, a, b, 0, NULL);
            e2 = hi_emit(HI_FEQ, TY_INT, ahi, bhi, 0, NULL);
            r = hi_emit(HI_AND, TY_INT, e1, e2, 0, NULL);
            if (op == T_NE) r = hi_emit(HI_NOT, TY_INT, r, -1, 0, NULL);
            return r;
        }
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
        ex_log = 1;
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
        ex_log = 1;
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
        ex_log = 1;
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
        ex_log = 1;
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
/* END, or Fortran 90's END SUBROUTINE [name], END FUNCTION [name], END
 * PROGRAM [name], END BLOCK DATA [name].  Not ENDDO, ENDIF or ENDFILE: the
 * word after END must be a unit's, and only a name may follow it. */
/* CONTAINS (Fortran 90): the end of its unit's body; procedures follow. */
static int f77_is_contains(void) { return lx_stmt_len == 8 && strncmp(lx_stmt, "CONTAINS", 8) == 0; }

static int f77_is_unit_end(void) {
    static const char *kinds[] = { "SUBROUTINE", "FUNCTION", "PROGRAM", "BLOCKDATA", "MODULE", 0 };
    int k;
    int i;
    if (!f77_starts("END")) return 0;
    if (lx_stmt_len == 3) return 1;
    for (k = 0; kinds[k]; k++) {
        int n;
        n = (int)strlen(kinds[k]);
        if (strncmp(lx_stmt + 3, kinds[k], n) != 0) continue;
        for (i = 3 + n; i < lx_stmt_len; i++)
            if (!lx_isalpha(lx_stmt[i]) && !lx_isdigit(lx_stmt[i]) && lx_stmt[i] != '_') return 0;
        return 1;
    }
    return 0;
}

/* Offset of the first `c` at paren depth zero, or -1.  Character
 * constants are skipped so a quoted '=' or ',' cannot fool the
 * classifier. */
/* Is the '=' at i an assignment's (or a keyword's, or DO's) -- not part of
 * Fortran 90's == /= <= >=?  The text scanners that find a statement's
 * shape by its '=' ask this. */
static int f77_eq_is_assign(int i) {
    char b;
    if (i < 0 || i >= lx_stmt_len || lx_stmt[i] != '=') return 0;
    if (i + 1 < lx_stmt_len && lx_stmt[i + 1] == '=') return 0;
    if (i + 1 < lx_stmt_len && lx_stmt[i + 1] == '>') return 0;     /* => (a USE rename) */
    b = i > 0 ? lx_stmt[i - 1] : 0;
    return !(b == '=' || b == '/' || b == '<' || b == '>');
}

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
        else if (depth == 0 && lx_stmt[i] == c && (c != '=' || f77_eq_is_assign(i))) return i;
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
        if (f77_eq_is_assign(after)) return S_ASSIGN;    /* array element IF(..) */
        return S_IF_LOGIC;
    }

    /* WHERE (mask) [assignment]: the mask's ')' followed by the end of
     * the statement or by a name -- WHERE(I) = 3 is an array named WHERE */
    if (f77_starts("WHERE(")) {
        int ws;
        ws = f77_sym_find("WHERE");
        after = f77_match_paren(5);
        if (!(ws >= 0 && f77_srank[ws] > 0) &&
            (after == lx_stmt_len || (after < lx_stmt_len && lx_isalpha(lx_stmt[after])))) {
            f77_if_tail = after;
            return S_KEYWORD;
        }
    }

    if (f77_starts("DO")) {
        eq = f77_toplevel('=', 2);
        if (eq >= 0 && f77_toplevel(',', eq + 1) >= 0) {
            f77_do_body = 2;
            return S_DO;
        }
    }

    /* A top-level :: is a Fortran 90 declaration, whatever follows it --
     * INTEGER :: K = 3 has an '=' but is no assignment. */
    {
        int c;
        c = f77_toplevel(':', 0);
        if (c >= 0 && c + 1 < lx_stmt_len && lx_stmt[c + 1] == ':') return S_KEYWORD;
    }
    /* an assignment's '=', not a USE rename's => (nor == or /=) */
    eq = f77_toplevel('=', 0);
    while (eq >= 0 && !f77_eq_is_assign(eq)) eq = f77_toplevel('=', eq + 1);
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
    if (f77_snar[s]) { f77_store_at(f77_sval[s], f77_mty(s), v, vty, vhi); return; }
    v = f77_cvt(v, &vhi, vty, f77_sty[s]);
    if (f77_sty[s] == TY_DOUBLE || f77_sty[s] == TY_CPLX) {
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

/* --- array sections (Fortran 90) ------------------------------------
 *
 * A section's subscripts: per dimension a scalar or a triplet
 * [lo]:[hi][:stride], the bounds defaulting to the declared ones.  Its
 * elements are reached by loops the front end generates, one per triplet,
 * through f77_elem_addr -- no new HIR.  Milestone 1 has the section as the
 * target of a scalar and as the argument of a rank-1 reduction (MAXLOC,
 * MINLOC, MAXVAL, MINVAL); array expressions are milestone 3's. */

/* A frame INTEGER temporary (a loop's index or trip count: promoted). */
static int f77_int_temp(void) {
    int t;
    f77_frame = f77_frame + 4;
    t = hi_emit(HI_ALLOCA, TY_INT, -1, -1, 0 - f77_frame, NULL);
    f77_alloca_add(t, 0 - f77_frame);
    return t;
}

/* A counted loop: idx from lo by st, MAX((hi-lo+st)/st, 0) times, as a
 * DO loop.  f77_loop_begin leaves the body's block current; the body
 * reads the index with hi_emit(HI_LOAD, TY_INT, l->idx, ...). */
typedef struct { int idx; int trip; int st; int body; int exit; } F77Loop;

static void f77_loop_begin(F77Loop *l, int lo, int hi, int st) {
    int t;
    int c;
    l->idx = f77_int_temp();
    l->trip = f77_int_temp();
    l->st = st;
    l->body = hir_new_block();
    l->exit = hir_new_block();
    hi_emit(HI_STORE, TY_INT, l->idx, lo, 0, NULL);
    t = hi_emit(HI_SUB, TY_INT, hi, lo, 0, NULL);
    t = hi_emit(HI_ADD, TY_INT, t, st, 0, NULL);
    t = hi_emit(HI_DIV, TY_INT, t, st, 0, NULL);
    hi_emit(HI_STORE, TY_INT, l->trip, t, 0, NULL);
    c = hi_emit(HI_SGT, TY_INT, t, f77_iconst(0), 0, NULL);
    hi_emit(HI_BRC, TY_VOID, c, l->body, l->exit, NULL);
    f77_cur_blk_live = 0;
    f77_begin_blk(l->body);
}

static void f77_loop_end(F77Loop *l) {
    int v;
    int c;
    if (f77_cur_blk_live) {
        v = hi_emit(HI_LOAD, TY_INT, l->idx, -1, 0, NULL);
        v = hi_emit(HI_ADD, TY_INT, v, l->st, 0, NULL);
        hi_emit(HI_STORE, TY_INT, l->idx, v, 0, NULL);
        v = hi_emit(HI_LOAD, TY_INT, l->trip, -1, 0, NULL);
        v = hi_emit(HI_ADDI, TY_INT, v, -1, -1, NULL);
        hi_emit(HI_STORE, TY_INT, l->trip, v, 0, NULL);
        c = hi_emit(HI_SGT, TY_INT, v, f77_iconst(0), 0, NULL);
        hi_emit(HI_BRC, TY_VOID, c, l->body, l->exit, NULL);
    }
    f77_cur_blk_live = 0;
    f77_begin_blk(l->exit);
}

/* A dimension's declared lower and upper bounds as INTEGER values; -1 for
 * the upper of an assumed-size (*) dimension. */
static void f77_dim_bounds(int s, int k, int *lo, int *hi) {
    int ext;
    if (f77_salloc[s]) {
        *lo = f77_desc_load(s, 4 + 8 * k);
        *hi = hi_emit(HI_ADD, TY_INT, *lo, f77_desc_load(s, 8 + 8 * k), 0, NULL);
        *hi = hi_emit(HI_ADDI, TY_INT, *hi, -1, -1, NULL);
        return;
    }
    *lo = f77_slov[s][k] >= 0 ? f77_slov[s][k] : f77_iconst(f77_slo[s][k]);
    if (k == f77_srank[s] - 1 && f77_sassz[s]) { *hi = -1; return; }
    ext = f77_sextv[s][k] >= 0 ? f77_sextv[s][k] : f77_iconst(f77_sext[s][k]);
    *hi = hi_emit(HI_ADD, TY_INT, *lo, ext, 0, NULL);
    *hi = hi_emit(HI_ADDI, TY_INT, *hi, -1, -1, NULL);
}

/* The subscripts of a section, lx_t just past its '(' and the ')' eaten:
 * per dimension a scalar (trip[k] 0, idx[k] its value) or a triplet
 * (trip[k] 1, lo/hi/st[k]).  Returns the number of triplets, -1 on error. */
static int f77_section_subs(int s, int *idx, int *trip, int *lo, int *hi, int *st) {
    int k;
    int nt;
    nt = 0;
    for (k = 0; ; k++) {
        if (k >= f77_srank[s]) { f77_error("wrong number of subscripts"); return -1; }
        trip[k] = 0;
        idx[k] = -1;
        if (lx_t != T_COLON) {
            idx[k] = f77_expr();
            idx[k] = f77_cvt(idx[k], &ex_hi, ex_ty, TY_INT);
        }
        if (lx_t == T_COLON) {
            int dlo;
            int dhi;
            trip[k] = 1;
            nt = nt + 1;
            f77_dim_bounds(s, k, &dlo, &dhi);
            lo[k] = idx[k] >= 0 ? idx[k] : dlo;   /* [lo]: */
            f77_tok();                          /* : */
            if (lx_t != T_COLON && lx_t != T_COMMA && lx_t != T_RP) {
                hi[k] = f77_expr();
                hi[k] = f77_cvt(hi[k], &ex_hi, ex_ty, TY_INT);
            } else {
                if (dhi < 0) { f77_error("the last dimension of an assumed-size array needs an upper bound in a section"); return -1; }
                hi[k] = dhi;
            }
            st[k] = f77_iconst(1);
            if (lx_t == T_COLON) {
                f77_tok();
                st[k] = f77_expr();
                st[k] = f77_cvt(st[k], &ex_hi, ex_ty, TY_INT);
            }
        }
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) { f77_error("expected ) after section subscripts"); return -1; }
    f77_tok();
    if (k + 1 != f77_srank[s]) { f77_error("wrong number of subscripts"); return -1; }
    return nt;
}

/* Is nm, referenced as a function, an intrinsic that is not elemental:
 * a reduction, an inquiry, an array-valued one?  Its value is a scalar
 * (or refused), and its arguments are its own, not operands of the array
 * expression around it. */
static int f77_nonelem_intrinsic(char *nm) {
    static const char *ne[] = { "SUM", "PRODUCT", "MAXVAL", "MINVAL", "MAXLOC", "MINLOC", "COUNT",
                                "ANY", "ALL", "DOT_PRODUCT", "SIZE", "LBOUND", "UBOUND", "SHAPE",
                                "MATMUL", "TRANSPOSE", "RESHAPE", "SPREAD", "PACK", "UNPACK",
                                "CSHIFT", "EOSHIFT", "TRANSFER", "ALLOCATED", "PRESENT", "LEN", 0 };
    int i;
    for (i = 0; ne[i]; i++) if (strcmp(ne[i], nm) == 0) return 1;
    return f77_is_inquiry(nm);
}

/* The first operand of an array expression in the text [from, to): an
 * array named whole, or a section (a non-CHARACTER array subscripted with
 * a ':' at its own level) -- in an element's subscripts too, a vector
 * subscript, whose shape is the element's.  Not in the arguments of a
 * function other than an elemental intrinsic: those
 * are the function's, and its value is one scalar.  The array's symbol,
 * and *at the offset of its name; -1 when there is none.  Text-level, as
 * the classifier is. */
static int f77_aex_first(int from, int to, int *at) {
    int i;
    i = from;
    while (i < to) {
        if (lx_stmt[i] == '\'') {
            i = i + 1;
            while (i < to && lx_stmt[i] != '\'') i = i + 1;
            i = i + 1;
            continue;
        }
        if (lx_isalpha(lx_stmt[i]) && (i == from || !(lx_isalpha(lx_stmt[i - 1]) || lx_isdigit(lx_stmt[i - 1]) ||
                                                      lx_stmt[i - 1] == '_' || lx_stmt[i - 1] == '.'))) {
            char nm[F77_MAX_NAME];
            int n;
            int b;
            int sy;
            int paren;
            b = i;
            n = 0;
            while (i < to && (lx_isalpha(lx_stmt[i]) || lx_isdigit(lx_stmt[i]) || lx_stmt[i] == '_')) {
                if (n < F77_MAX_NAME - 1) nm[n++] = lx_stmt[i];
                i = i + 1;
            }
            nm[n] = 0;
            paren = i < to && lx_stmt[i] == '(';
            sy = f77_sym_find(nm);
            if (sy >= 0 && f77_srank[sy] > 0 && !f77_sparam[sy]) {
                if (!paren) { *at = b; return sy; }            /* a whole array */
                if (f77_sty[sy] != TY_CHAR) {
                    int d;
                    int j;
                    d = 0;
                    for (j = i; j < to; j++) {
                        if (lx_stmt[j] == '\'') { j = j + 1; while (j < to && lx_stmt[j] != '\'') j = j + 1; continue; }
                        if (lx_stmt[j] == '(') d = d + 1;
                        else if (lx_stmt[j] == ')') { d = d - 1; if (d == 0) break; }
                        else if (lx_stmt[j] == ':' && d == 1) { *at = b; return sy; }   /* a section */
                    }
                }
                continue;   /* an element: a whole array in its subscripts is a vector subscript's */
            }
            if (paren) {
                int in;
                in = IN_NONE;
                if (!(sy >= 0 && (f77_sty[sy] == TY_CHAR || f77_sarg[sy] || f77_sexternal[sy])) &&
                    f77_find_func(nm) < 0 && f77_stf_find(nm) < 0)
                    in = f77_intrinsic(nm);
                if (in == IN_NONE || f77_nonelem_intrinsic(nm)) i = f77_match_paren(i);
            }
            continue;
        }
        i = i + 1;
    }
    return -1;
}

/* Does the statement text from `from` hold an array expression's operand? */
static int f77_text_has_array_expr(int from) {
    int at;
    return f77_aex_first(from, lx_stmt_len, &at) >= 0;
}

/* Does the statement text from `from` name `nm` as a whole word? */
static int f77_text_names(int from, char *nm) {
    int i;
    int n;
    n = (int)strlen(nm);
    i = from;
    while (i < lx_stmt_len) {
        if (lx_stmt[i] == '\'') {
            i = i + 1;
            while (i < lx_stmt_len && lx_stmt[i] != '\'') i = i + 1;
            i = i + 1;
            continue;
        }
        if (lx_isalpha(lx_stmt[i])) {
            int b;
            b = i;
            while (i < lx_stmt_len && (lx_isalpha(lx_stmt[i]) || lx_isdigit(lx_stmt[i]) || lx_stmt[i] == '_')) i = i + 1;
            if (i - b == n && strncmp(lx_stmt + b, nm, n) == 0 && (b == 0 || lx_stmt[b - 1] != '.')) return 1;
            continue;
        }
        i = i + 1;
    }
    return 0;
}

/* An array operand of an array expression, at the loops' current counts
 * (f77_aex_cnt): a section (lx_t at its '('), its t-th triplet at lo +
 * count[t]*stride, or a whole array (whole), dimension d at its lower bound
 * + count[d].  Its rank must be the expression's. */
static int f77_aex_elem(int s, int whole) {
    int idx[F77_MAX_RANK]; int trip[F77_MAX_RANK];
    int lo[F77_MAX_RANK]; int hi[F77_MAX_RANK]; int st[F77_MAX_RANK];
    int cur[F77_MAX_RANK];
    int k;
    int t;
    int nt;
    int v;
    if (whole) {
        if (f77_srank[s] != f77_aex_k) { f77_error("the shapes in an array expression do not conform"); return f77_iconst(0); }
        for (k = 0; k < f77_srank[s]; k++) {
            int dlo;
            int dhi;
            f77_dim_bounds(s, k, &dlo, &dhi);
            cur[k] = hi_emit(HI_ADD, TY_INT, dlo, f77_aex_cnt[k], 0, NULL);
        }
    } else {
        f77_tok();                                   /* ( */
        nt = f77_section_subs(s, idx, trip, lo, hi, st);
        if (nt < 0) return f77_iconst(0);
        if (nt != f77_aex_k) { f77_error("the shapes in an array expression do not conform"); return f77_iconst(0); }
        t = 0;
        for (k = 0; k < f77_srank[s]; k++) {
            if (trip[k]) {
                int c;
                c = hi_emit(HI_MUL, TY_INT, f77_aex_cnt[t], st[k], 0, NULL);
                cur[k] = hi_emit(HI_ADD, TY_INT, lo[k], c, 0, NULL);
                t = t + 1;
            } else {
                cur[k] = idx[k];
            }
        }
    }
    v = f77_load_at(f77_elem_addr(s, cur, f77_srank[s]), f77_mty(s));
    ex_log = f77_slog[s];
    return v;
}

/* An array constructor (/ v, v, ... /) as the whole right side of an
 * array assignment (Fortran 90), the target of rank 1 (one triplet).
 * Each value is stored as it is parsed, at a running position, to the
 * target's next element; a value may be an implied-DO (v, ..., I = m1,
 * m2 [, m3]), nested as deep as I/O's.  The values agree in type (F90
 * 4.5).  When the target array is named among the values -- X = (/ X(2),
 * X(1) /) -- they go to a temporary of the target's extent first, as
 * F90's evaluate-then-assign has it.  An array as a value (flattened by
 * F90) is refused where it is parsed, as an expression. */
static int f77_acon_s;
static int f77_acon_rank;
static int f77_acon_idx[F77_MAX_RANK];
static int f77_acon_tdim;
static int f77_acon_lo;
static int f77_acon_st;
static int f77_acon_pos;
static int f77_acon_tmp;
static int f77_acon_ty0;
static int f77_acon_n;        /* values parsed; -1 once an implied-DO makes it dynamic */

static void f77_acon_item(void);

/* An INTEGER value's constant, when HIR built it from constants alone
 * (bounds are ADD/ADDI chains over ICONSTs): 1 and *out, else 0. */
static int f77_hconst(int v, int *out) {
    int a; int b;
    if (v < 0) return 0;
    if (h_kind[v] == HI_ICONST) { *out = h_val[v]; return 1; }
    if (h_kind[v] == HI_ADDI) {
        if (!f77_hconst(h_src1[v], &a)) return 0;
        *out = a + h_val[v];
        return 1;
    }
    if (h_kind[v] == HI_ADD || h_kind[v] == HI_SUB || h_kind[v] == HI_MUL) {
        if (!f77_hconst(h_src1[v], &a) || !f77_hconst(h_src2[v], &b)) return 0;
        *out = h_kind[v] == HI_ADD ? a + b : h_kind[v] == HI_SUB ? a - b : a * b;
        return 1;
    }
    return 0;
}

static void f77_acon_items(int items_off, int ctl_off) {
    f77_scan_from(items_off);
    for (;;) {
        f77_acon_item();
        if (lx_t != T_COMMA) break;
        if ((int)(lx_rts - lx_stmt) >= ctl_off - 1) break;
        f77_tok();
        if ((int)(lx_rts - lx_stmt) >= ctl_off) break;
    }
}

static void f77_acon_item(void) {
    int v; int vty; int vhi;
    int p;
    int s;
    int addr;
    if (lx_t == T_LP && f77_implied_do(f77_acon_items)) { f77_acon_n = -1; return; }
    if (f77_acon_n >= 0) f77_acon_n = f77_acon_n + 1;
    s = f77_acon_s;
    v = f77_expr();
    vty = ex_ty; vhi = ex_hi;
    if (vty == TY_CHAR) { f77_error("a CHARACTER value for a numeric array"); return; }
    if (f77_acon_ty0 < 0) f77_acon_ty0 = vty;
    else if (vty != f77_acon_ty0) { f77_error("the values of an array constructor differ in type"); return; }
    p = hi_emit(HI_LOAD, TY_INT, f77_acon_pos, -1, 0, NULL);
    if (f77_acon_tmp >= 0) {
        addr = hi_emit(HI_ADD, HL_ADDR_TY, f77_acon_tmp, hi_emit(HI_MUL, TY_INT, p, f77_iconst(f77_esize(s)), 0, NULL), 0, NULL);
    } else {
        int cur[F77_MAX_RANK];
        int k;
        for (k = 0; k < f77_acon_rank; k++) cur[k] = f77_acon_idx[k];
        cur[f77_acon_tdim] = hi_emit(HI_ADD, TY_INT, f77_acon_lo, hi_emit(HI_MUL, TY_INT, p, f77_acon_st, 0, NULL), 0, NULL);
        addr = f77_elem_addr(s, cur, f77_acon_rank);
    }
    f77_store_at(addr, f77_mty(s), v, vty, vhi);
    hi_emit(HI_STORE, TY_INT, f77_acon_pos, hi_emit(HI_ADDI, TY_INT, p, -1, 1, NULL), 0, NULL);
}

static void f77_acon_assign(int s, int *idx, int *trip, int *lo, int *hi, int *st, int nt) {
    int k;
    int ext;
    int esz;
    int mty;
    if (nt != 1) { f77_error("an array constructor is rank 1; the target is not"); return; }
    f77_acon_s = s;
    f77_acon_rank = f77_srank[s];
    for (k = 0; k < f77_acon_rank; k++) {
        f77_acon_idx[k] = idx[k];
        if (trip[k]) f77_acon_tdim = k;
    }
    k = f77_acon_tdim;
    f77_acon_lo = lo[k];
    f77_acon_st = st[k];
    f77_acon_ty0 = -1;
    f77_acon_n = 0;
    esz = f77_esize(s);
    mty = f77_mty(s);
    ext = hi_emit(HI_SUB, TY_INT, hi[k], lo[k], 0, NULL);
    ext = hi_emit(HI_ADD, TY_INT, ext, st[k], 0, NULL);
    ext = hi_emit(HI_DIV, TY_INT, ext, st[k], 0, NULL);
    ext = f77_select(hi_emit(HI_SGT, TY_INT, ext, f77_iconst(0), 0, NULL), ext, f77_iconst(0));
    f77_acon_tmp = -1;
    if (f77_text_names((int)(lx_rts - lx_stmt), f77_sname[s])) {
        int args[3];
        args[0] = f77_iconst(0);
        args[1] = hi_emit(HI_MUL, TY_INT, ext, f77_iconst(esz), 0, NULL);
        args[2] = f77_iconst(0);
        f77_acon_tmp = f77_call_i("f77_allocate", 3, args);
    }
    f77_acon_pos = f77_int_temp();
    hi_emit(HI_STORE, TY_INT, f77_acon_pos, f77_iconst(0), 0, NULL);
    f77_tok();                                   /* ( */
    f77_tok();                                   /* / */
    for (;;) {
        f77_acon_item();
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_SLASH) { f77_error("expected /) after an array constructor"); return; }
    f77_tok();
    if (lx_t != T_RP) { f77_error("expected /) after an array constructor"); return; }
    f77_tok();
    /* both counts known: they must agree (gfortran's compile-time check) */
    {
        int e; int cl; int ch; int cs;
        if (f77_acon_n >= 0 && f77_hconst(lo[k], &cl) && f77_hconst(hi[k], &ch) && f77_hconst(st[k], &cs) && cs != 0)
            e = (ch - cl + cs) / cs;
        else
            e = f77_acon_n;
        if (e < 0) e = 0;
        if (f77_acon_n >= 0 && e != f77_acon_n) {
            char m[128];
            sprintf(m, "the array constructor has %d values for %d elements", f77_acon_n, e);
            f77_error(m);
            return;
        }
    }
    if (f77_acon_tmp >= 0) {
        /* the copy, and the temporary freed */
        F77Loop cl;
        int c;
        int cur[F77_MAX_RANK];
        int v;
        int args[1];
        int tmp;
        tmp = f77_acon_tmp;
        f77_loop_begin(&cl, f77_iconst(0), hi_emit(HI_ADDI, TY_INT, ext, -1, -1, NULL), f77_iconst(1));
        c = hi_emit(HI_LOAD, TY_INT, cl.idx, -1, 0, NULL);
        v = f77_load_at(hi_emit(HI_ADD, HL_ADDR_TY, tmp, hi_emit(HI_MUL, TY_INT, c, f77_iconst(esz), 0, NULL), 0, NULL), mty);
        for (k = 0; k < f77_acon_rank; k++) cur[k] = idx[k];
        cur[f77_acon_tdim] = hi_emit(HI_ADD, TY_INT, lo[f77_acon_tdim], hi_emit(HI_MUL, TY_INT, c, st[f77_acon_tdim], 0, NULL), 0, NULL);
        f77_store_at(f77_elem_addr(s, cur, f77_acon_rank), mty, v,
                     mty == TY_SHORT || mty == F77_TY_BYTE ? TY_INT : f77_sty[s], ex_hi);
        f77_loop_end(&cl);
        args[0] = tmp;
        f77_call_i("f77_dealloc_auto", 1, args);
    }
}

/* Is a WHERE's mask in force for the assignment being compiled? */
static int f77_where_on(void) {
    return f77_where_tmp >= 0 && f77_where_depth == f77_inline_depth;
}

/* In an array assignment's loops (cnt the counts, first fastest): branch
 * past the element unless the mask holds for it (ELSEWHERE: unless it
 * does not), leaving the element's block current.  The block to go on
 * in, which the caller enters after the element. */
static int f77_where_guard(int *cnt, int nt) {
    int off; int c; int j; int b_do; int b_skip;
    if (nt != f77_where_nt) { f77_error("the shapes of a WHERE's mask and its assignment do not conform"); return -1; }
    off = cnt[nt - 1];
    for (j = nt - 2; j >= 0; j--) {
        off = hi_emit(HI_MUL, TY_INT, off, f77_where_ext[j], 0, NULL);
        off = hi_emit(HI_ADD, TY_INT, off, cnt[j], 0, NULL);
    }
    off = hi_emit(HI_MUL, TY_INT, off, f77_iconst(4), 0, NULL);
    c = hi_emit(HI_LOAD, TY_INT, hi_emit(HI_ADD, HL_ADDR_TY, f77_where_tmp, off, 0, NULL), -1, 0, NULL);
    c = hi_emit(f77_where_neg ? HI_SEQ : HI_SNE, TY_INT, c, f77_iconst(0), 0, NULL);
    b_do = hir_new_block();
    b_skip = hir_new_block();
    hi_emit(HI_BRC, TY_VOID, c, b_do, b_skip, NULL);
    f77_cur_blk_live = 0;
    f77_begin_blk(b_do);
    return b_skip;
}

/* An array assignment (Fortran 90): A(section) = e, or A = e for the whole
 * array (whole; lx_t past the name, else past the section's '(').
 * - e a scalar: it and the scalar subscripts are evaluated once, before
 *   the loops, and stored to every element.
 * - e an array expression: loops over the left side's shape, the first
 *   triplet innermost, and e parsed once inside them, its array operands
 *   the elements the loops' counts reach (f77_aex_elem).  F90 evaluates e
 *   whole before any element is stored, so when the left side's array is
 *   named in e -- A(I:J,K) = A(I,K)*C(1:N), whose first store changes
 *   A(I,K) -- the values go to a temporary first and are copied after. */
static void f77_array_assign(int s, int whole) {
    int idx[F77_MAX_RANK]; int trip[F77_MAX_RANK];
    int lo[F77_MAX_RANK]; int hi[F77_MAX_RANK]; int st[F77_MAX_RANK];
    int ext[F77_MAX_RANK]; int tdim[F77_MAX_RANK];
    F77Loop lp[F77_MAX_RANK];
    int v; int vty; int vhi;
    int k;
    int nt;
    int rank;
    int ty;
    int mty;
    rank = f77_srank[s];
    ty = f77_sty[s];
    mty = f77_mty(s);
    for (k = 0; k < F77_MAX_RANK; k++) idx[k] = -1;
    if (whole) {
        for (k = 0; k < rank; k++) {
            trip[k] = 1;
            f77_dim_bounds(s, k, &lo[k], &hi[k]);
            if (hi[k] < 0) { f77_error("an assumed-size array cannot be assigned whole"); return; }
            st[k] = f77_iconst(1);
        }
        nt = rank;
    } else {
        nt = f77_section_subs(s, idx, trip, lo, hi, st);
        if (nt < 0) return;
    }
    if (lx_t != T_ASSIGN) { f77_error("expected ="); return; }
    f77_tok();
    f77_sstored[s] = 1;
    if (lx_t == T_LP && lx_rp < lx_rpe && *lx_rp == '/' && f77_match_paren((int)(lx_rts - lx_stmt)) >= lx_stmt_len) {
        if (f77_where_on()) { f77_error("an array constructor in a WHERE is not supported yet"); return; }
        f77_acon_assign(s, idx, trip, lo, hi, st, nt);
        return;
    }
    /* under a WHERE a scalar too goes through the loops below, which
     * test the mask */
    if (!f77_text_has_array_expr((int)(lx_rts - lx_stmt)) && !f77_where_on()) {
        /* a scalar to every element */
        v = f77_expr();
        vty = ex_ty; vhi = ex_hi;
        if (vty == TY_CHAR) { f77_error("a CHARACTER value for a numeric array"); return; }
        v = f77_cvt(v, &vhi, vty, ty);
        for (k = rank - 1; k >= 0; k--)
            if (trip[k]) f77_loop_begin(&lp[k], lo[k], hi[k], st[k]);
        {
            int cur[F77_MAX_RANK];
            for (k = 0; k < rank; k++)
                cur[k] = trip[k] ? hi_emit(HI_LOAD, TY_INT, lp[k].idx, -1, 0, NULL) : idx[k];
            f77_store_at(f77_elem_addr(s, cur, rank), mty, v, ty, vhi);
        }
        for (k = 0; k < rank; k++)
            if (trip[k]) f77_loop_end(&lp[k]);
        return;
    }
    /* an array expression */
    {
        int j;
        int overlap;
        int tmp;
        int esz;
        int saved_k;
        int saved_cnt[F77_MAX_RANK];
        int b_wskip;
        F77Loop cl[F77_MAX_RANK];
        int cnt[F77_MAX_RANK];
        if (nt > 8) { f77_error("too many dimensions in an array expression"); return; }
        j = 0;
        for (k = 0; k < rank; k++) {
            if (!trip[k]) continue;
            /* the extent, MAX((hi - lo + st)/st, 0) */
            {
                int e;
                e = hi_emit(HI_SUB, TY_INT, hi[k], lo[k], 0, NULL);
                e = hi_emit(HI_ADD, TY_INT, e, st[k], 0, NULL);
                e = hi_emit(HI_DIV, TY_INT, e, st[k], 0, NULL);
                ext[j] = f77_select(hi_emit(HI_SGT, TY_INT, e, f77_iconst(0), 0, NULL), e, f77_iconst(0));
            }
            tdim[j] = k;
            j = j + 1;
        }
        esz = f77_esize(s);
        overlap = f77_text_names((int)(lx_rts - lx_stmt), f77_sname[s]);
        tmp = -1;
        if (overlap) {
            int args[3];
            int n;
            n = ext[0];
            for (j = 1; j < nt; j++) n = hi_emit(HI_MUL, TY_INT, n, ext[j], 0, NULL);
            args[0] = f77_iconst(0);
            args[1] = hi_emit(HI_MUL, TY_INT, n, f77_iconst(esz), 0, NULL);
            args[2] = f77_iconst(0);
            tmp = f77_call_i("f77_allocate", 3, args);
        }
        for (j = nt - 1; j >= 0; j--)
            f77_loop_begin(&cl[j], f77_iconst(0), hi_emit(HI_ADDI, TY_INT, ext[j], -1, -1, NULL), f77_iconst(1));
        for (j = 0; j < nt; j++) cnt[j] = hi_emit(HI_LOAD, TY_INT, cl[j].idx, -1, 0, NULL);
        b_wskip = f77_where_on() ? f77_where_guard(cnt, nt) : -1;
        saved_k = f77_aex_k;
        for (j = 0; j < 8 && j < F77_MAX_RANK; j++) saved_cnt[j] = f77_aex_cnt[j];
        f77_aex_k = nt;
        for (j = 0; j < nt; j++) f77_aex_cnt[j] = cnt[j];
        v = f77_expr();
        vty = ex_ty; vhi = ex_hi;
        f77_aex_k = saved_k;
        for (j = 0; j < 8 && j < F77_MAX_RANK; j++) f77_aex_cnt[j] = saved_cnt[j];
        if (vty == TY_CHAR) { f77_error("a CHARACTER value for a numeric array"); return; }
        if (overlap) {
            /* the temporary, in the loops' order: count[0] fastest */
            int off;
            off = cnt[nt - 1];
            for (j = nt - 2; j >= 0; j--) {
                off = hi_emit(HI_MUL, TY_INT, off, ext[j], 0, NULL);
                off = hi_emit(HI_ADD, TY_INT, off, cnt[j], 0, NULL);
            }
            off = hi_emit(HI_MUL, TY_INT, off, f77_iconst(esz), 0, NULL);
            f77_store_at(hi_emit(HI_ADD, HL_ADDR_TY, tmp, off, 0, NULL), mty, v, vty, vhi);
        } else {
            int cur[F77_MAX_RANK];
            for (k = 0; k < rank; k++) cur[k] = idx[k];
            for (j = 0; j < nt; j++)
                cur[tdim[j]] = hi_emit(HI_ADD, TY_INT, lo[tdim[j]],
                                       hi_emit(HI_MUL, TY_INT, cnt[j], st[tdim[j]], 0, NULL), 0, NULL);
            f77_store_at(f77_elem_addr(s, cur, rank), mty, v, vty, vhi);
        }
        if (b_wskip >= 0) { f77_goto_blk(b_wskip); f77_begin_blk(b_wskip); }
        for (j = 0; j < nt; j++) f77_loop_end(&cl[j]);
        if (overlap) {
            /* then the copy, in the same order, and the temporary freed */
            int args[1];
            for (j = nt - 1; j >= 0; j--)
                f77_loop_begin(&cl[j], f77_iconst(0), hi_emit(HI_ADDI, TY_INT, ext[j], -1, -1, NULL), f77_iconst(1));
            {
                int cur[F77_MAX_RANK];
                int off;
                for (j = 0; j < nt; j++) cnt[j] = hi_emit(HI_LOAD, TY_INT, cl[j].idx, -1, 0, NULL);
                b_wskip = f77_where_on() ? f77_where_guard(cnt, nt) : -1;
                off = cnt[nt - 1];
                for (j = nt - 2; j >= 0; j--) {
                    off = hi_emit(HI_MUL, TY_INT, off, ext[j], 0, NULL);
                    off = hi_emit(HI_ADD, TY_INT, off, cnt[j], 0, NULL);
                }
                off = hi_emit(HI_MUL, TY_INT, off, f77_iconst(esz), 0, NULL);
                v = f77_load_at(hi_emit(HI_ADD, HL_ADDR_TY, tmp, off, 0, NULL), mty);
                vhi = ex_hi;
                for (k = 0; k < rank; k++) cur[k] = idx[k];
                for (j = 0; j < nt; j++)
                    cur[tdim[j]] = hi_emit(HI_ADD, TY_INT, lo[tdim[j]],
                                           hi_emit(HI_MUL, TY_INT, cnt[j], st[tdim[j]], 0, NULL), 0, NULL);
                f77_store_at(f77_elem_addr(s, cur, rank), mty, v, mty == TY_SHORT || mty == F77_TY_BYTE ? TY_INT : ty, vhi);
                if (b_wskip >= 0) { f77_goto_blk(b_wskip); f77_begin_blk(b_wskip); }
            }
            for (j = 0; j < nt; j++) f77_loop_end(&cl[j]);
            args[0] = tmp;
            f77_call_i("f77_dealloc_auto", 1, args);
        }
    }
}

/* An intrinsic reference's arguments, read from the text: lx_t is the
 * first token after its '('.  Each argument's text [from, to) -- to at the
 * ',' or ')' after it -- and its keyword ("" when positional); end is past
 * the ')'.  Text-level, so that each argument can be parsed where the
 * caller wants it: a reduction's ARRAY and MASK inside its loops. */
#define F77_MAX_IARGS 8
typedef struct {
    int n;
    int from[F77_MAX_IARGS];
    int to[F77_MAX_IARGS];
    char kw[F77_MAX_IARGS][16];
    int end;
} F77Args;

static int f77_intr_args(F77Args *a) {
    int a0;
    int i;
    int b;
    int depth;
    a0 = (int)(lx_rts - lx_stmt);
    a->end = f77_match_paren(a0 - 1);
    a->n = 0;
    depth = 0;
    b = a0;
    for (i = a0; i < a->end; i++) {
        int c;
        c = lx_stmt[i];
        if (c == '\'') { i = i + 1; while (i < a->end && lx_stmt[i] != '\'') i = i + 1; continue; }
        if (c == '(') { depth = depth + 1; continue; }
        if (c == ')' && depth > 0) { depth = depth - 1; continue; }
        if ((c == ',' && depth == 0) || (c == ')' && depth == 0)) {
            int j;
            int k;
            if (a->n >= F77_MAX_IARGS) { f77_error("too many arguments to an intrinsic"); return 0; }
            k = a->n;
            a->kw[k][0] = 0;
            /* KEYWORD=, not == */
            j = b;
            while (j < i && (lx_isalpha(lx_stmt[j]) || lx_stmt[j] == '_')) j = j + 1;
            if (j > b && j < i && lx_stmt[j] == '=' && lx_stmt[j + 1] != '=' && j - b < 16) {
                memcpy(a->kw[k], lx_stmt + b, (size_t)(j - b));
                a->kw[k][j - b] = 0;
                b = j + 1;
            }
            a->from[k] = b;
            a->to[k] = i;
            a->n = k + 1;
            b = i + 1;
            if (c == ')') break;
        }
    }
    if (a->n == 1 && a->from[0] == a->to[0]) a->n = 0;     /* F() */
    return 1;
}

/* Parse the expression that is the text [from, to): its value, ex_ty and
 * ex_hi as f77_expr leaves them.  It must use all of the text. */
static int f77_expr_at(int from, int to) {
    int v;
    f77_scan_from(from);
    v = f77_expr();
    if ((int)(lx_rts - lx_stmt) != to && f77_nerr == 0) f77_error("unexpected text in an intrinsic's argument");
    return v;
}

/* MAX((hi - lo + st)/st, 0): the number of elements a triplet reaches. */
static int f77_extent(int lo, int hi, int st) {
    int e;
    e = hi_emit(HI_SUB, TY_INT, hi, lo, 0, NULL);
    e = hi_emit(HI_ADD, TY_INT, e, st, 0, NULL);
    e = hi_emit(HI_DIV, TY_INT, e, st, 0, NULL);
    return f77_select(hi_emit(HI_SGT, TY_INT, e, f77_iconst(0), 0, NULL), e, f77_iconst(0));
}

/* The shape of the operand f77_aex_first found (array sy, its name at
 * text offset at): its rank, ext[] the extents.  A section's bounds are
 * evaluated here, before the loops (and again for each element: loop-
 * invariant).  -1 on error. */
static int f77_aex_shape(int sy, int at, int *ext) {
    int idx[F77_MAX_RANK]; int trip[F77_MAX_RANK];
    int lo[F77_MAX_RANK]; int hi[F77_MAX_RANK]; int st[F77_MAX_RANK];
    int k;
    int nt;
    f77_scan_from(at);
    f77_tok();                                       /* the name */
    if (lx_t == T_LP) {
        f77_tok();
        nt = f77_section_subs(sy, idx, trip, lo, hi, st);
        if (nt < 0) return -1;
        nt = 0;
        for (k = 0; k < f77_srank[sy]; k++)
            if (trip[k]) { ext[nt] = f77_extent(lo[k], hi[k], st[k]); nt = nt + 1; }
        return nt;
    }
    for (k = 0; k < f77_srank[sy]; k++) {
        f77_dim_bounds(sy, k, &lo[k], &hi[k]);
        if (hi[k] < 0) { f77_error("an assumed-size array has no shape to reduce over"); return -1; }
        ext[k] = f77_extent(lo[k], hi[k], f77_iconst(1));
    }
    return f77_srank[sy];
}

/* Fortran 90's reductions of an array expression (lx_t just past the '('):
 * SUM PRODUCT MAXVAL MINVAL ([ARRAY=]e [,MASK=m]), COUNT ANY ALL (mask),
 * MAXLOC MINLOC (e, [DIM=]1 [,MASK=m]), DOT_PRODUCT (a, b).  Loops over the
 * shape of e's first operand (f77_aex_first), first dimension fastest,
 * with m and e parsed inside them as an array assignment's right side is
 * (f77_aex_k).  DIM= is 1 on a rank-1 argument; DIM elsewhere gives an
 * array, refused.  A second positional argument holding an array operand
 * is MASK (F2003 allows that form, and gfortran takes it), else DIM.
 *
 * SUM and PRODUCT: the first element taken is the accumulator, the next
 * ones added (multiplied) to it -- the type is the element expression's,
 * known only once it is parsed -- and 0 (1) of that type when none is.
 * MAXVAL MINVAL MAXLOC MINLOC: libgfortran's order, so that NaNs go as
 * gfortran has them: the location is that of the first element taken
 * (with MASK, the first for which it holds), the first element that is
 * not a NaN sets the extreme, and only a strictly greater (less) one moves
 * it; MAXVAL of no elements is -HUGE (MINVAL +HUGE), of NaNs alone a NaN.
 * DOT_PRODUCT: SUM(CONJG(a)*b) (numeric), ANY(a .AND. b) (LOGICAL).  A
 * loop with branches in the front end, no new HIR. */
static int f77_reduce(int in) {
    F77Args A;
    int arr; int arr2; int mask; int dim;
    int sy; int at;
    int ext[F77_MAX_RANK]; int ext2[F77_MAX_RANK];
    int nt;
    int k;
    int ismax; int wantloc; int isminmax; int islog;
    int acc; int found; int pos; int loc;
    int ty;
    int rlog;
    int saved_k; int saved_cnt[8];
    F77Loop lp[F77_MAX_RANK];
    char *fname;
    static char m[160];
    ismax = in == IN_MAXLOC || in == IN_MAXVAL;
    wantloc = in == IN_MAXLOC || in == IN_MINLOC;
    isminmax = in >= IN_MAXLOC && in <= IN_MINVAL;
    islog = in == IN_COUNT || in == IN_ANY || in == IN_ALL;
    fname = in == IN_MAXLOC ? "MAXLOC" : in == IN_MINLOC ? "MINLOC" : in == IN_MAXVAL ? "MAXVAL" :
            in == IN_MINVAL ? "MINVAL" : in == IN_SUM ? "SUM" : in == IN_PRODUCT ? "PRODUCT" :
            in == IN_COUNT ? "COUNT" : in == IN_ANY ? "ANY" : in == IN_ALL ? "ALL" : "DOT_PRODUCT";
    ex_ty = TY_INT; ex_hi = -1; ex_log = 0;
    if (!f77_intr_args(&A)) return f77_iconst(0);
    arr = -1; arr2 = -1; mask = -1; dim = -1;
    for (k = 0; k < A.n; k++) {
        char *kw;
        kw = A.kw[k];
        if (in == IN_DOTP) {
            if ((kw[0] == 0 && k == 0) || strcmp(kw, "VECTOR_A") == 0) { if (arr >= 0) goto badarg; arr = k; }
            else if ((kw[0] == 0 && k == 1) || strcmp(kw, "VECTOR_B") == 0) { if (arr2 >= 0) goto badarg; arr2 = k; }
            else goto badarg;
            continue;
        }
        if ((kw[0] == 0 && k == 0) || strcmp(kw, islog ? "MASK" : "ARRAY") == 0) {
            if (arr >= 0) goto badarg;
            arr = k;
        } else if (strcmp(kw, "DIM") == 0) {
            if (dim >= 0) goto badarg;
            dim = k;
        } else if (!islog && strcmp(kw, "MASK") == 0) {
            if (mask >= 0) goto badarg;
            mask = k;
        } else if (kw[0] == 0 && k == 1) {
            if (!islog && f77_aex_first(A.from[k], A.to[k], &at) >= 0) mask = k;
            else dim = k;
        } else if (kw[0] == 0 && k == 2 && !islog) {
            if (mask >= 0) goto badarg;
            mask = k;
        } else goto badarg;
    }
    if (arr < 0 || (in == IN_DOTP && arr2 < 0)) goto badarg;
    sy = f77_aex_first(A.from[arr], A.to[arr], &at);
    if (sy < 0) { sprintf(m, "%s needs an array", fname); f77_error(m); return f77_iconst(0); }
    nt = f77_aex_shape(sy, at, ext);
    if (nt <= 0) return f77_iconst(0);
    if (nt > 8) { f77_error("too many dimensions in an array expression"); return f77_iconst(0); }
    if (in == IN_DOTP) {
        int sy2;
        sy2 = f77_aex_first(A.from[arr2], A.to[arr2], &at);
        if (sy2 < 0) { f77_error("DOT_PRODUCT needs two arrays"); return f77_iconst(0); }
        if (nt != 1 || f77_aex_shape(sy2, at, ext2) != 1) { f77_error("DOT_PRODUCT needs arrays of rank 1"); return f77_iconst(0); }
    }
    if (dim >= 0) {
        int d;
        d = f77_expr_at(A.from[dim], A.to[dim]);
        if (ex_ty != TY_INT || ex_log || h_kind[d] != HI_ICONST) {
            sprintf(m, "DIM= of %s is supported only as a constant", fname);
            f77_error(m);
            return f77_iconst(0);
        }
        if (nt != 1) {
            sprintf(m, "%s with DIM= of an array of rank above 1 gives an array, which is not supported yet", fname);
            f77_error(m);
            return f77_iconst(0);
        }
        if (h_val[d] != 1) { f77_error("a rank-1 array's DIM is the constant 1"); return f77_iconst(0); }
    } else if (wantloc) {
        sprintf(m, "%s without DIM= gives an array, which is not supported yet", fname);
        f77_error(m);
        return f77_iconst(0);
    }

    acc = f77_char_temp(16);
    found = f77_int_temp(); pos = f77_int_temp(); loc = f77_int_temp();
    hi_emit(HI_STORE, TY_INT, found, f77_iconst(0), 0, NULL);
    hi_emit(HI_STORE, TY_INT, pos, f77_iconst(0), 0, NULL);
    hi_emit(HI_STORE, TY_INT, loc, f77_iconst(0), 0, NULL);
    if (islog) hi_emit(HI_STORE, TY_INT, acc, f77_iconst(in == IN_ALL ? 1 : 0), 0, NULL);
    ty = -1;
    rlog = 0;
    for (k = nt - 1; k >= 0; k--)
        f77_loop_begin(&lp[k], f77_iconst(0), hi_emit(HI_ADDI, TY_INT, ext[k], -1, -1, NULL), f77_iconst(1));
    saved_k = f77_aex_k;
    for (k = 0; k < 8; k++) saved_cnt[k] = f77_aex_cnt[k];
    f77_aex_k = nt;
    for (k = 0; k < nt; k++) f77_aex_cnt[k] = hi_emit(HI_LOAD, TY_INT, lp[k].idx, -1, 0, NULL);
    {
        int p;
        int b_next;
        int x; int xhi; int xty;
        b_next = hir_new_block();
        p = hi_emit(HI_LOAD, TY_INT, pos, -1, 0, NULL);
        p = hi_emit(HI_ADDI, TY_INT, p, -1, 1, NULL);
        hi_emit(HI_STORE, TY_INT, pos, p, 0, NULL);
        if (mask >= 0) {
            int c;
            int b_do;
            c = f77_expr_at(A.from[mask], A.to[mask]);
            if (ex_ty != TY_INT || !ex_log) { sprintf(m, "MASK= of %s needs a LOGICAL array", fname); f77_error(m); }
            b_do = hir_new_block();
            hi_emit(HI_BRC, TY_VOID, c, b_do, b_next, NULL);
            f77_cur_blk_live = 0;
            f77_begin_blk(b_do);
        }
        x = f77_expr_at(A.from[arr], A.to[arr]);
        xty = ex_ty; xhi = ex_hi;
        if (in == IN_DOTP) {
            int y; int yhi; int yty; int ylog; int xlog;
            xlog = ex_log;
            y = f77_expr_at(A.from[arr2], A.to[arr2]);
            yty = ex_ty; yhi = ex_hi; ylog = ex_log;
            if (xlog != ylog) { f77_error("DOT_PRODUCT of a LOGICAL and a numeric array"); }
            else if (xlog) {
                x = hi_emit(HI_AND, TY_INT, x, y, 0, NULL);
                xty = TY_INT; ex_log = 1;
            } else {
                if (xty == TY_CHAR || yty == TY_CHAR) f77_error("DOT_PRODUCT needs numeric or LOGICAL arrays");
                if (xty == TY_CPLX) xhi = hi_emit(HI_FNEG, TY_FLOAT, xhi, -1, 0, NULL);
                if (xty == TY_DCPLX) {
                    int re; int reh; int im; int imh;
                    re = f77_dcx_part(x, 0, &reh);
                    im = f77_dcx_part(x, 1, &imh);
                    im = f77_fp64_pair1("__fp64_neg", im, imh, &imh);
                    x = f77_dcx_make(re, reh, im, imh);
                }
                x = f77_binop(T_STAR, x, xty, xhi, y, yty, yhi);
                xty = ex_ty; xhi = ex_hi; ex_log = 0;
            }
        }
        if (xty == TY_CHAR) { sprintf(m, "%s of a CHARACTER array", fname); f77_error(m); xty = TY_INT; }
        ty = xty;
        rlog = ex_log;
        if (islog || (in == IN_DOTP && rlog)) {
            int a0;
            if (!rlog) { sprintf(m, "%s needs a LOGICAL array", fname); f77_error(m); }
            a0 = hi_emit(HI_LOAD, TY_INT, acc, -1, 0, NULL);
            if (in == IN_COUNT) a0 = hi_emit(HI_ADD, TY_INT, a0, hi_emit(HI_SNE, TY_INT, x, f77_iconst(0), 0, NULL), 0, NULL);
            else if (in == IN_ALL) a0 = hi_emit(HI_AND, TY_INT, a0, x, 0, NULL);
            else a0 = hi_emit(HI_OR, TY_INT, a0, x, 0, NULL);
            hi_emit(HI_STORE, TY_INT, acc, a0, 0, NULL);
        } else if (!isminmax) {
            /* SUM, PRODUCT, numeric DOT_PRODUCT: the first element taken
             * is the accumulator */
            int b_first; int b_more; int fb;
            if (rlog) { sprintf(m, "%s of a LOGICAL array", fname); f77_error(m); }
            b_first = hir_new_block(); b_more = hir_new_block();
            fb = hi_emit(HI_LOAD, TY_INT, found, -1, 0, NULL);
            hi_emit(HI_BRC, TY_VOID, fb, b_more, b_first, NULL);
            f77_cur_blk_live = 0;
            f77_begin_blk(b_first);
            f77_store_at(acc, ty, x, ty, xhi);
            hi_emit(HI_STORE, TY_INT, found, f77_iconst(1), 0, NULL);
            f77_goto_blk(b_next);
            f77_begin_blk(b_more);
            {
                int a0; int a0hi;
                a0 = f77_load_at(acc, ty);
                a0hi = ex_hi;
                a0 = f77_binop(in == IN_PRODUCT ? T_STAR : T_PLUS, a0, ty, a0hi, x, ty, xhi);
                f77_store_at(acc, ty, a0, ty, ex_hi);
            }
        } else {
            int fb; int b_first; int b_more; int b_take; int c;
            if (rlog || (ty != TY_INT && ty != TY_FLOAT && ty != TY_DOUBLE)) {
                sprintf(m, "%s needs an INTEGER, REAL or DOUBLE PRECISION array", fname);
                f77_error(m);
                ty = TY_INT;
            }
            /* the location is the first element's taken, for a non-empty array */
            {
                int b_one; int b_go; int l0;
                b_one = hir_new_block(); b_go = hir_new_block();
                l0 = hi_emit(HI_LOAD, TY_INT, loc, -1, 0, NULL);
                hi_emit(HI_BRC, TY_VOID, hi_emit(HI_SEQ, TY_INT, l0, f77_iconst(0), 0, NULL), b_one, b_go, NULL);
                f77_cur_blk_live = 0;
                f77_begin_blk(b_one);
                hi_emit(HI_STORE, TY_INT, loc, p, 0, NULL);
                f77_goto_blk(b_go);
                f77_begin_blk(b_go);
            }
            b_first = hir_new_block(); b_more = hir_new_block(); b_take = hir_new_block();
            fb = hi_emit(HI_LOAD, TY_INT, found, -1, 0, NULL);
            hi_emit(HI_BRC, TY_VOID, fb, b_more, b_first, NULL);
            f77_cur_blk_live = 0;
            /* not found yet: take x unless it is a NaN */
            f77_begin_blk(b_first);
            if (ty == TY_DOUBLE) c = f77_fp64_call2("__fp64_eq", x, xhi, x, xhi, NULL);
            else if (ty == TY_FLOAT) c = hi_emit(HI_FEQ, TY_INT, x, x, 0, NULL);
            else c = f77_iconst(1);
            hi_emit(HI_BRC, TY_VOID, c, b_take, b_next, NULL);
            f77_cur_blk_live = 0;
            /* found: take x only past the extreme */
            f77_begin_blk(b_more);
            {
                int mx; int mh;
                mx = f77_load_at(acc, ty);
                mh = ex_hi;
                if (ty == TY_DOUBLE)
                    c = ismax ? f77_fp64_call2("__fp64_lt", mx, mh, x, xhi, NULL) : f77_fp64_call2("__fp64_lt", x, xhi, mx, mh, NULL);
                else if (ty == TY_FLOAT)
                    c = ismax ? hi_emit(HI_FLT, TY_INT, mx, x, 0, NULL) : hi_emit(HI_FLT, TY_INT, x, mx, 0, NULL);
                else
                    c = ismax ? hi_emit(HI_SLT, TY_INT, mx, x, 0, NULL) : hi_emit(HI_SLT, TY_INT, x, mx, 0, NULL);
            }
            hi_emit(HI_BRC, TY_VOID, c, b_take, b_next, NULL);
            f77_cur_blk_live = 0;
            f77_begin_blk(b_take);
            f77_store_at(acc, ty, x, ty, xhi);
            hi_emit(HI_STORE, TY_INT, loc, p, 0, NULL);
            hi_emit(HI_STORE, TY_INT, found, f77_iconst(1), 0, NULL);
        }
        f77_goto_blk(b_next);
        f77_begin_blk(b_next);
    }
    f77_aex_k = saved_k;
    for (k = 0; k < 8; k++) f77_aex_cnt[k] = saved_cnt[k];
    for (k = 0; k < nt; k++) f77_loop_end(&lp[k]);
    f77_scan_from(A.end);                    /* the scanner past the ')' */
    ex_log = 0;
    if (islog || (in == IN_DOTP && rlog)) {
        ex_ty = TY_INT; ex_hi = -1; ex_log = in != IN_COUNT;
        return hi_emit(HI_LOAD, TY_INT, acc, -1, 0, NULL);
    }
    if (wantloc) {
        ex_ty = TY_INT; ex_hi = -1;
        return hi_emit(HI_LOAD, TY_INT, loc, -1, 0, NULL);
    }
    if (!isminmax) {
        /* none taken: 0 (1) of the type */
        int b_none; int b_done; int fb; int z; int zhi;
        b_none = hir_new_block(); b_done = hir_new_block();
        fb = hi_emit(HI_LOAD, TY_INT, found, -1, 0, NULL);
        hi_emit(HI_BRC, TY_VOID, fb, b_done, b_none, NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_none);
        zhi = -1;
        z = f77_cvt(f77_iconst(in == IN_PRODUCT ? 1 : 0), &zhi, TY_INT, ty);
        f77_store_at(acc, ty, z, ty, zhi);
        f77_goto_blk(b_done);
        f77_begin_blk(b_done);
        return f77_load_at(acc, ty);
    }
    /* MAXVAL/MINVAL: nothing found -- no elements: -HUGE (+HUGE); NaNs
     * alone: a NaN */
    {
        int b_none; int b_done; int fb; int p; int e;
        b_none = hir_new_block(); b_done = hir_new_block();
        fb = hi_emit(HI_LOAD, TY_INT, found, -1, 0, NULL);
        hi_emit(HI_BRC, TY_VOID, fb, b_done, b_none, NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_none);
        /* no element taken (none, or none the mask holds for) has no
         * location: -HUGE; else they were all NaNs */
        p = hi_emit(HI_LOAD, TY_INT, loc, -1, 0, NULL);
        e = hi_emit(HI_SEQ, TY_INT, p, f77_iconst(0), 0, NULL);
        if (ty == TY_INT) {
            /* F90 13.13.63: "the negative number of the largest magnitude",
             * -HUGE-1 in two's complement (gfortran's); -HUGE was wrong */
            hi_emit(HI_STORE, TY_INT, acc, f77_iconst(ismax ? (int)(-2147483647 - 1) : 2147483647), 0, NULL);
        } else if (ty == TY_FLOAT) {
            int big; int nan;
            union { float f; int i; } u;
            u.f = ismax ? -3.4028234663852886e38f : 3.4028234663852886e38f;
            big = f77_iconst(u.i);
            nan = f77_iconst(0x7FC00000);
            hi_emit(HI_STORE, TY_INT, acc, f77_select(e, big, nan), 0, NULL);
        } else {
            int hlo; int hhi; int nlo; int nhi; int a4;
            hlo = f77_dconst(ismax ? -1.7976931348623157e308 : 1.7976931348623157e308, &hhi);
            nlo = f77_iconst(0);
            nhi = f77_iconst(0x7FF80000);
            hi_emit(HI_STORE, TY_INT, acc, f77_select(e, hlo, nlo), 0, NULL);
            a4 = hi_emit(HI_ADDI, HL_ADDR_TY, acc, -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, a4, f77_select(e, hhi, nhi), 0, NULL);
        }
        f77_goto_blk(b_done);
        f77_begin_blk(b_done);
    }
    return f77_load_at(acc, ty);
badarg:
    sprintf(m, "the arguments of %s are not ones it takes", fname);
    f77_error(m);
    ex_ty = TY_INT; ex_hi = -1; ex_log = 0;
    return f77_iconst(0);
}

/* SIZE, LBOUND, UBOUND (lx_t just past the '('): ([ARRAY=]a [, [DIM=]d]),
 * a an array or a section.  A section's lower bounds are 1 and its upper
 * bounds its extents; an array's are its declared ones, but 1 and 0 for a
 * dimension of no elements (F90 13.13.52).  SIZE without DIM is the
 * product of the extents; LBOUND and UBOUND without it give an array,
 * refused.  d is any INTEGER: a select among the dimensions when it is
 * not a constant. */
static int f77_arr_inquiry(int in) {
    F77Args A;
    int arr; int dim;
    int sy; int at;
    int lo[F77_MAX_RANK]; int hi[F77_MAX_RANK]; int ext[F77_MAX_RANK]; int v[F77_MAX_RANK];
    int idx[F77_MAX_RANK]; int trip[F77_MAX_RANK]; int st[F77_MAX_RANK];
    int k; int nt; int r; int section;
    char *fname;
    static char m[160];
    fname = in == IN_SIZE ? "SIZE" : in == IN_LBOUND ? "LBOUND" : "UBOUND";
    ex_ty = TY_INT; ex_hi = -1; ex_log = 0;
    if (!f77_intr_args(&A)) return f77_iconst(0);
    arr = -1; dim = -1;
    for (k = 0; k < A.n; k++) {
        if ((A.kw[k][0] == 0 && k == 0) || strcmp(A.kw[k], "ARRAY") == 0) { if (arr >= 0) goto badarg; arr = k; }
        else if ((A.kw[k][0] == 0 && k == 1) || strcmp(A.kw[k], "DIM") == 0) { if (dim >= 0) goto badarg; dim = k; }
        else goto badarg;
    }
    if (arr < 0) goto badarg;
    sy = f77_aex_first(A.from[arr], A.to[arr], &at);
    if (sy < 0 || at != A.from[arr]) { sprintf(m, "%s needs an array", fname); f77_error(m); return f77_iconst(0); }
    f77_scan_from(at);
    f77_tok();
    section = lx_t == T_LP;
    if (section) {
        f77_tok();
        if (f77_section_subs(sy, idx, trip, lo, hi, st) < 0) return f77_iconst(0);
        nt = 0;
        for (k = 0; k < f77_srank[sy]; k++)
            if (trip[k]) { ext[nt] = f77_extent(lo[k], hi[k], st[k]); nt = nt + 1; }
    } else {
        nt = f77_srank[sy];
        for (k = 0; k < nt; k++) {
            f77_dim_bounds(sy, k, &lo[k], &hi[k]);
            if (hi[k] >= 0) ext[k] = f77_extent(lo[k], hi[k], f77_iconst(1));
            else ext[k] = -1;
        }
    }
    if ((int)(lx_rts - lx_stmt) != A.to[arr]) { sprintf(m, "%s needs an array", fname); f77_error(m); return f77_iconst(0); }
    for (k = 0; k < nt; k++) {
        if (in == IN_SIZE) v[k] = ext[k];
        else if (section) v[k] = in == IN_LBOUND ? f77_iconst(1) : ext[k];
        else if (ext[k] < 0) v[k] = in == IN_LBOUND ? lo[k] : -1;
        else {
            int ne;
            ne = hi_emit(HI_SGT, TY_INT, ext[k], f77_iconst(0), 0, NULL);
            v[k] = f77_select(ne, in == IN_LBOUND ? lo[k] : hi[k], f77_iconst(in == IN_LBOUND ? 1 : 0));
        }
    }
    if (dim < 0) {
        if (in != IN_SIZE) {
            sprintf(m, "%s without DIM= gives an array, which is not supported yet", fname);
            f77_error(m);
            return f77_iconst(0);
        }
        r = f77_iconst(1);
        for (k = 0; k < nt; k++) {
            if (v[k] < 0) { f77_error("the size of an assumed-size array is not known"); return f77_iconst(0); }
            r = hi_emit(HI_MUL, TY_INT, r, v[k], 0, NULL);
        }
    } else {
        int d;
        d = f77_expr_at(A.from[dim], A.to[dim]);
        if (ex_ty != TY_INT || ex_log) { sprintf(m, "DIM= of %s needs an INTEGER", fname); f77_error(m); return f77_iconst(0); }
        if (h_kind[d] == HI_ICONST) {
            if (h_val[d] < 1 || h_val[d] > nt) { sprintf(m, "DIM= of %s is outside the array's rank", fname); f77_error(m); return f77_iconst(0); }
            r = v[h_val[d] - 1];
            if (r < 0) { f77_error("the last dimension of an assumed-size array has no upper bound"); return f77_iconst(0); }
        } else {
            r = f77_iconst(0);
            for (k = 0; k < nt; k++) {
                if (v[k] < 0) { f77_error("the last dimension of an assumed-size array has no upper bound"); return f77_iconst(0); }
                r = f77_select(hi_emit(HI_SEQ, TY_INT, d, f77_iconst(k + 1), 0, NULL), v[k], r);
            }
        }
    }
    f77_scan_from(A.end);
    ex_ty = TY_INT; ex_hi = -1; ex_log = 0;
    return r;
badarg:
    sprintf(m, "the arguments of %s are not ones it takes", fname);
    f77_error(m);
    return f77_iconst(0);
}

/* WHERE (Fortran 90).  The mask, the text [from, to), is evaluated once,
 * before any assignment (F90 7.5.3.2), into a heap temporary of its shape
 * (f77_where_tmp); each assignment then loops over its own shape, which
 * must be the mask's rank, and stores only the elements the mask holds
 * for -- ELSEWHERE's those it does not.  The right side is evaluated for
 * those elements only.  F90's WHERE: no WHERE inside one, no masked
 * ELSEWHERE (Fortran 95's), assignments alone in its body. */
static void f77_where_begin(int from, int to) {
    int sy; int at; int nt; int k; int n; int tmp; int pos;
    int ext[F77_MAX_RANK];
    int saved_k; int saved_cnt[8];
    int args[3];
    F77Loop lp[F77_MAX_RANK];
    sy = f77_aex_first(from, to, &at);
    if (sy < 0) { f77_error("WHERE needs a LOGICAL array for its mask"); return; }
    nt = f77_aex_shape(sy, at, ext);
    if (nt <= 0) return;
    if (nt > 8) { f77_error("too many dimensions in an array expression"); return; }
    n = ext[0];
    for (k = 1; k < nt; k++) n = hi_emit(HI_MUL, TY_INT, n, ext[k], 0, NULL);
    args[0] = f77_iconst(0);
    args[1] = hi_emit(HI_MUL, TY_INT, n, f77_iconst(4), 0, NULL);
    args[2] = f77_iconst(0);
    tmp = f77_call_i("f77_allocate", 3, args);
    pos = f77_int_temp();
    hi_emit(HI_STORE, TY_INT, pos, f77_iconst(0), 0, NULL);
    for (k = nt - 1; k >= 0; k--)
        f77_loop_begin(&lp[k], f77_iconst(0), hi_emit(HI_ADDI, TY_INT, ext[k], -1, -1, NULL), f77_iconst(1));
    saved_k = f77_aex_k;
    for (k = 0; k < 8; k++) saved_cnt[k] = f77_aex_cnt[k];
    f77_aex_k = nt;
    for (k = 0; k < nt; k++) f77_aex_cnt[k] = hi_emit(HI_LOAD, TY_INT, lp[k].idx, -1, 0, NULL);
    {
        int v; int p;
        v = f77_expr_at(from, to);
        if (ex_ty != TY_INT || !ex_log) f77_error("WHERE needs a LOGICAL array for its mask");
        p = hi_emit(HI_LOAD, TY_INT, pos, -1, 0, NULL);
        hi_emit(HI_STORE, TY_INT, hi_emit(HI_ADD, HL_ADDR_TY, tmp, hi_emit(HI_MUL, TY_INT, p, f77_iconst(4), 0, NULL), 0, NULL),
                hi_emit(HI_SNE, TY_INT, v, f77_iconst(0), 0, NULL), 0, NULL);
        hi_emit(HI_STORE, TY_INT, pos, hi_emit(HI_ADDI, TY_INT, p, -1, 1, NULL), 0, NULL);
    }
    f77_aex_k = saved_k;
    for (k = 0; k < 8; k++) f77_aex_cnt[k] = saved_cnt[k];
    for (k = 0; k < nt; k++) f77_loop_end(&lp[k]);
    f77_where_tmp = tmp;
    f77_where_neg = 0;
    f77_where_depth = f77_inline_depth;
    f77_where_nt = nt;
    for (k = 0; k < nt; k++) f77_where_ext[k] = ext[k];
}

static void f77_where_end(void) {
    int args[1];
    if (f77_where_tmp < 0) return;
    args[0] = f77_where_tmp;
    f77_call_i("f77_dealloc_auto", 1, args);
    f77_where_tmp = -1;
}

static void f77_stmt_assign(void);

/* WHERE (mask) assignment, or WHERE (mask) opening the construct its
 * ELSEWHERE and END WHERE close. */
static void f77_stmt_where(void) {
    int after;
    after = f77_match_paren(5);
    if (f77_where_on()) { f77_error("a WHERE inside a WHERE is Fortran 95's, not supported"); return; }
    f77_where_begin(6, after - 1);
    if (f77_where_tmp < 0) return;
    if (after == lx_stmt_len) {
        if (ctl_n >= F77_MAX_CTL) { f77_error("control nesting too deep"); return; }
        ctl_kind[ctl_n] = F77_CTL_WHERE;
        ctl_else[ctl_n] = 0;
        ctl_name[ctl_n][0] = 0;
        ctl_n = ctl_n + 1;
        lx_t = T_EOF;
        return;
    }
    {
        /* the assignment, moved to the front of the buffer, as a logical
         * IF's statement is */
        int i;
        i = 0;
        while (after + i < lx_stmt_len) { lx_stmt[i] = lx_stmt[after + i]; i = i + 1; }
        lx_stmt_len = i;
        lx_stmt[i] = 0;
        if (f77_classify() != S_ASSIGN) f77_error("a WHERE statement's statement is an array assignment");
        else f77_stmt_assign();
    }
    f77_where_end();
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
    if (f77_sty[s] == TY_CHAR) {
        int dlen;
        if (f77_where_on()) { f77_error("a CHARACTER assignment in a WHERE is not supported"); return; }
        addr = f77_char_ref(s);
        dlen = ex_len;
        if (lx_t != T_ASSIGN) { f77_error("expected ="); return; }
        f77_tok();
        v = f77_expr();
        if (ex_ty != TY_CHAR) { f77_error("a CHARACTER variable needs a CHARACTER value"); return; }
        f77_sstored[s] = 1;
        f77_char_assign(addr, dlen, v, ex_len);
        return;
    }
    addr = f77_sval[s];
    if (lx_t == T_LP && f77_srank[s] > 0 && f77_paren_has_colon()) {
        f77_tok();
        f77_array_assign(s, 0);
        return;
    }
    if (lx_t != T_LP && f77_srank[s] > 0) {     /* A = e, the whole array (F90) */
        f77_array_assign(s, 1);
        return;
    }
    if (f77_where_on()) { f77_error("a WHERE assigns to arrays, not to a scalar or an element"); return; }
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
        f77_store_at(addr, f77_mty(s), v, vty, ex_hi);
}

static int f77_stf_find(char *nm) {
    int i;
    for (i = 0; i < f77_stf_n; i++)
        if (strcmp(f77_sname[f77_stf_sym[i]], nm) == 0) return i;
    return -1;
}

/* Is this S_ASSIGN statement a statement-function definition?  Shape:
 * an identifier, a parenthesised list ending exactly at the =, and a
 * name that is not an array (an array element assignment has the same
 * shape).  Records it and returns 1; returns 0 to let the assignment
 * path have it. */
static int f77_stf_define(void) {
    int i;
    int eq;
    int s;
    int n;
    int len;
    char nm[F77_MAX_NAME];

    i = 0;
    while (i < lx_stmt_len && (lx_isalpha(lx_stmt[i]) || lx_isdigit(lx_stmt[i]))) i = i + 1;
    if (i == 0 || i >= F77_MAX_NAME || i >= lx_stmt_len || lx_stmt[i] != '(') return 0;
    eq = f77_toplevel('=', 0);
    if (eq < 0 || f77_match_paren(i) != eq) return 0;
    memcpy(nm, lx_stmt, i);
    nm[i] = 0;
    s = f77_sym(nm);
    if (f77_srank[s] > 0 || f77_sarg[s] || f77_sparam[s]) return 0;
    if (f77_sty[s] == TY_CHAR) {
        /* C(1:3) = ... is a substring assignment, not a definition. */
        int k;
        int depth;
        depth = 0;
        for (k = i; k < eq; k++) {
            if (lx_stmt[k] == '(') depth = depth + 1;
            else if (lx_stmt[k] == ')') depth = depth - 1;
            else if (lx_stmt[k] == ':' && depth == 1) return 0;
        }
    }
    if (!f77_decls_open && f77_inline_depth == 0) return 0;
    if (f77_stf_n >= F77_MAX_STF) { f77_error("too many statement functions"); return 1; }

    f77_scan_from(i + 1);
    n = 0;
    while (lx_t == T_NAME) {
        if (n < F77_MAX_STF_ARGS) {
            strcpy(f77_stf_arg[f77_stf_n][n], lex_name);
            f77_stf_argty[f77_stf_n][n] = f77_sty[f77_sym(lex_name)];
            f77_stf_arglen[f77_stf_n][n] = f77_slen[f77_sym(lex_name)];
        }
        n = n + 1;
        f77_tok();
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) { f77_error("bad statement function dummy list"); return 1; }
    if (n > F77_MAX_STF_ARGS) { f77_error("too many statement function dummies"); return 1; }

    len = lx_stmt_len - (eq + 1);
    if (f77_stf_pool_n + len + 1 > F77_STF_POOL) { f77_error("statement function pool overflow"); return 1; }
    memcpy(f77_stf_pool + f77_stf_pool_n, lx_stmt + eq + 1, len);
    memcpy(f77_stf_pbrk + f77_stf_pool_n, lx_brk + eq + 1, len);
    f77_stf_sym[f77_stf_n] = s;
    f77_stf_nargs[f77_stf_n] = n;
    f77_stf_text[f77_stf_n] = f77_stf_pool_n;
    f77_stf_len[f77_stf_n] = len;
    f77_stf_pool_n = f77_stf_pool_n + len + 1;
    f77_stf_n = f77_stf_n + 1;
    lx_t = T_EOF;            /* the body is kept as text, parsed where it is used */
    return 1;
}

/* Reference: NAME(a1, ...) with the scanner on NAME. */
/* A CHARACTER actual of a statement function: its value is an address,
 * copied into a temporary of the dummy's length, blank-padded or cut
 * (F77 15.4.2), and the dummy bound to that.  Stored as a value it was
 * the address's low byte. */
static int f77_stf_char_arg(int dlen, int v, int vlen) {
    int s;
    s = f77_sym_temp(TY_CHAR);
    f77_sstored[s] = 1;
    if (dlen < 0) {              /* CHARACTER*(*): the actual itself */
        f77_slen[s] = -1;
        f77_slenv[s] = vlen;
        f77_sval[s] = v;
        return s;
    }
    f77_slen[s] = dlen;
    f77_sval[s] = f77_char_temp(dlen);
    f77_char_assign(f77_sval[s], f77_iconst(dlen), v, vlen);
    return s;
}

static int f77_stf_expand(int si) {
    int n;
    int k;
    int v;
    int vty;
    int vhi;
    int fs;
    int targs[F77_MAX_STF_ARGS];
    char save_stmt[F77_MAX_STMT];
    unsigned char save_brk[F77_MAX_STMT];
    int save_len;
    char *save_rp; char *save_rpe; char *save_rts; char *save_rte;
    int save_rcs; int save_ract;
    int save_t;
    char save_name[F77_MAX_NAME];
    int save_namelen;
    int save_ival;
    double save_dval;
    int save_sidx;
    int save_slen;

    f77_tok();
    if (lx_t != T_LP) { f77_error("statement function needs arguments"); return f77_iconst(0); }
    f77_tok();
    n = 0;
    if (lx_t != T_RP) {
        for (;;) {
            v = f77_expr(); vty = ex_ty; vhi = ex_hi;
            if (n < f77_stf_nargs[si] && f77_stf_argty[si][n] == TY_CHAR && vty == TY_CHAR) {
                targs[n] = f77_stf_char_arg(f77_stf_arglen[si][n], v, ex_len);
            } else if (n < f77_stf_nargs[si]) {
                targs[n] = f77_sym_temp(f77_stf_argty[si][n]);
                f77_store_sym_val(targs[n], v, vty, vhi);
            }
            n = n + 1;
            if (lx_t != T_COMMA) break;
            f77_tok();
        }
    }
    if (lx_t != T_RP) f77_error("expected ) after statement function arguments");
    else f77_tok();
    if (n != f77_stf_nargs[si]) {
        f77_error("wrong number of statement function arguments");
        return f77_iconst(0);
    }
    if (f77_stfb_n + n > F77_MAX_STFB) { f77_error("statement functions nested too deep"); return f77_iconst(0); }

    /* Checkpoint the statement and scanner, swap the definition in. */
    memcpy(save_stmt, lx_stmt, lx_stmt_len + 1);
    memcpy(save_brk, lx_brk, lx_stmt_len);
    save_len = lx_stmt_len;
    save_rp = lx_rp; save_rpe = lx_rpe; save_rts = lx_rts; save_rte = lx_rte;
    save_rcs = lx_rcs; save_ract = lx_ract;
    save_t = lx_t;
    memcpy(save_name, lex_name, F77_MAX_NAME);
    save_namelen = lex_namelen;
    save_ival = lex_ival; save_dval = lex_dval;
    save_sidx = lex_sidx; save_slen = lex_slen;

    memcpy(lx_stmt, f77_stf_pool + f77_stf_text[si], f77_stf_len[si]);
    memcpy(lx_brk, f77_stf_pbrk + f77_stf_text[si], f77_stf_len[si]);
    lx_stmt_len = f77_stf_len[si];
    lx_stmt[lx_stmt_len] = 0;
    for (k = 0; k < n; k++) {
        strcpy(f77_stfb_name[f77_stfb_n], f77_stf_arg[si][k]);
        f77_stfb_sym[f77_stfb_n] = targs[k];
        f77_stfb_n = f77_stfb_n + 1;
    }
    f77_scan_from(0);
    v = f77_expr(); vty = ex_ty; vhi = ex_hi;
    if (lx_t != T_EOF) f77_error("junk after statement function expression");
    f77_stfb_n = f77_stfb_n - n;

    memcpy(lx_stmt, save_stmt, save_len + 1);
    memcpy(lx_brk, save_brk, save_len);
    lx_stmt_len = save_len;
    lx_rp = save_rp; lx_rpe = save_rpe; lx_rts = save_rts; lx_rte = save_rte;
    lx_rcs = save_rcs; lx_ract = save_ract;
    lx_t = save_t;
    memcpy(lex_name, save_name, F77_MAX_NAME);
    lex_namelen = save_namelen;
    lex_ival = save_ival; lex_dval = save_dval;
    lex_sidx = save_sidx; lex_slen = save_slen;

    fs = f77_stf_sym[si];
    v = f77_cvt(v, &vhi, vty, f77_sty[fs]);
    ex_ty = f77_sty[fs];
    ex_log = f77_slog[fs];
    ex_hi = vhi;
    return v;
}

/* ASSIGN label TO var -- the label number is the variable's value. */
static void f77_stmt_assign_label(int skip) {
    int lab;
    int s;
    f77_scan_from(skip);
    if (lx_t != T_ICON) { f77_error("ASSIGN needs a statement label"); return; }
    lab = lex_ival;
    f77_tok();
    /* Blanks are gone: "TO M" arrives as the one name TOM. */
    if (lx_t != T_NAME || lex_name[0] != 'T' || lex_name[1] != 'O') {
        f77_error("ASSIGN needs TO");
        return;
    }
    if (lex_name[2] == 0) {
        f77_tok();
        if (lx_t != T_NAME) { f77_error("ASSIGN needs a variable"); return; }
        s = f77_sym(lex_name);
    } else {
        s = f77_sym(lex_name + 2);
    }
    f77_tok();
    f77_store_sym_val(s, f77_iconst(lab), TY_INT, -1);
}

/* A FORMAT named by an ASSIGNed variable: pick the format text at run
 * time by comparing the value against every FORMAT label of the unit
 * (a branchless select chain -- the unit has a handful). */
static int f77_fmt_dyn;
static int f77_fmt_dyn_len;    /* HIR length of a CHARACTER format (-3) */

/* The unit of a WRITE/READ control list.  A CHARACTER entity makes it
 * an internal file: f77_io_int is set with the record length and count
 * (an unsubscripted CHARACTER array is one record per element). */
static int f77_io_int;
static int f77_io_int_len;
static int f77_io_int_nrec;
static int f77_io_unf;         /* unformatted statement in progress */
static int f77_io_unit_expr(void) {
    int v;
    int nrec;
    nrec = 1;
    f77_io_int = 0;
    if (lx_t == T_NAME) {
        int us;
        int after;
        us = f77_sym(lex_name);
        after = (int)(lx_rte - lx_stmt);
        if (f77_sty[us] == TY_CHAR && f77_srank[us] > 0 &&
            (after >= lx_stmt_len || lx_stmt[after] != '('))
            nrec = f77_sym_nelem(us);
    }
    v = f77_expr();
    if (ex_ty == TY_CHAR) {
        f77_io_int = 1;
        f77_io_int_len = ex_len;
        f77_io_int_nrec = f77_iconst(nrec);
        return v;
    }
    return f77_cvt(v, &ex_hi, ex_ty, TY_INT);
}

static int f77_fmt_dyn_addr(int v) {
    int i;
    int acc;
    int c;
    int fa;
    acc = f77_iconst(0);
    for (i = 0; i < f77_nformat; i++) {
        if (f77_funit[i] != f77_text_unit()) continue;
        fa = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, f77_fstr[i], NULL);
        c = hi_emit(HI_SEQ, TY_INT, v, f77_iconst(f77_flabel[i]), 0, NULL);
        acc = f77_select(c, fa, acc);
    }
    return acc;
}

/* A dimension bound: a signed integer constant.  F77 allows constant
 * expressions here; only literals and negation are accepted so far. */
static void f77_cexpr(void);
static int    f77_cx_ty;
static int    f77_cx_i;
static int f77_dim_bound(void) {
    f77_cexpr();
    if (f77_cx_ty != TY_INT) { f77_error("dimension bound must be an integer constant"); return 1; }
    return f77_cx_i;
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
static int    f77_cx_sidx;   /* TY_CHAR: string index; f77_cx_i is the length */
static double f77_cx_d2;     /* TY_CPLX: the imaginary part (f77_cx_d is the real) */
static unsigned long long f77_cx_u;   /* TY_HEXC: the constant's bits */
#define F77_MAX_CXB 8
static char f77_cxb_name[F77_MAX_CXB][F77_MAX_NAME];
static int  f77_cxb_val[F77_MAX_CXB];
static int  f77_cxb_n;

static void f77_cexpr(void);

/* A complex type: COMPLEX or COMPLEX*16.  The imaginary part is f77_cx_d2. */
static int f77_iscx(int ty) { return ty == TY_CPLX || ty == TY_DCPLX; }

/* A complex constant's part, as its own type makes it: a REAL part is
 * rounded to single precision, an INTEGER or DOUBLE PRECISION one kept
 * (the evaluator holds every value as a double).  The constant is
 * COMPLEX*16 when a part is DOUBLE PRECISION, as gfortran has it --
 * (0.1D0, 0.2) is the double 0.1 and the single 0.2 widened. */
static double f77_cx_part(double d, int ty) { return ty == TY_FLOAT ? (double)(float)d : d; }

/* The evaluator holds every value as a double, but a REAL or COMPLEX one is
 * single precision: rounded as each literal is read and after each
 * operation, as the arithmetic at run time rounds it.  Without this,
 * PARAMETER (D = 1.0/3.0) gave a DOUBLE PRECISION D the double third,
 * not the REAL third widened, and DATA D /0.1/ the double 0.1, where F77
 * converts the REAL constant (gfortran: 0.10000000149011612). */
static void f77_cx_round(void) {
    if (f77_cx_ty == TY_FLOAT) f77_cx_d = (double)(float)f77_cx_d;
    else if (f77_cx_ty == TY_CPLX) {
        f77_cx_d = (double)(float)f77_cx_d;
        f77_cx_d2 = (double)(float)f77_cx_d2;
    }
}
static void f77_cpower(void);
static int f77_cx_promote(int a, int b);

/* The unit scan's constant expressions (f77_scan_mode): a kind before the
 * unit is compiled -- REAL(WP) :: F types FUNCTION F's result, which its
 * callers and its own binding need -- with names from the scan's table of
 * the unit's INTEGER named constants, and errors not reported but
 * remembered (f77_scan_bad): what the scan cannot tell, the compilation
 * will, or refuse. */
static char f77_sk_name[64][F77_MAX_NAME];
static int f77_sk_val[64];
static int f77_sk_n;
static int f77_sk_unit = -1;
static int f77_scan_const(char *nm, int *v) {
    int i;
    *v = 0;
    for (i = 0; i < f77_sk_n; i++)
        if (strcmp(f77_sk_name[i], nm) == 0) { *v = f77_sk_val[i]; return 1; }
    return 0;
}
/* The table is the unit's: a new unit starts it empty. */
static void f77_scan_begin(int unit) {
    if (f77_sk_unit != unit) { f77_sk_n = 0; f77_sk_unit = unit; }
}
static void f77_scan_const_add(int unit, char *nm, int v) {
    f77_scan_begin(unit);
    if (f77_sk_n >= 64) return;
    strcpy(f77_sk_name[f77_sk_n], nm);
    f77_sk_val[f77_sk_n] = v;
    f77_sk_n = f77_sk_n + 1;
}

/* The type a kind type parameter (Fortran 90) gives a base type, as
 * gfortran numbers them: REAL 4 and 8, COMPLEX 4 and 8 (its parts'),
 * INTEGER and LOGICAL 4.  -1 for one this compiler has not. */
static int f77_kind_ty(int ty, int k) {
    if (ty == TY_FLOAT || ty == TY_DOUBLE) return k == 4 ? TY_FLOAT : k == 8 ? TY_DOUBLE : -1;
    if (ty == TY_CPLX || ty == TY_DCPLX) return k == 4 ? TY_CPLX : k == 8 ? TY_DCPLX : -1;
    if (ty == TY_INT) return k == 4 ? TY_INT : -1;
    if (ty == TY_CHAR) return k == 1 ? TY_CHAR : -1;
    return -1;
}
/* KIND of a value of type ty. */
static int f77_ty_kind(int ty) {
    return ty == TY_DOUBLE || ty == TY_DCPLX ? 8 : ty == TY_CHAR ? 1 : 4;
}

/* A kind selector after a type keyword, lx_t at its '(': (n) or
 * (KIND=n), n a constant expression.  The type it makes, -1 after an
 * error.  DOUBLE PRECISION has none. */
static void f77_cexpr(void);
static int f77_kind_sel(int ty) {
    int r;
    char m[80];
    f77_tok();                                   /* ( */
    if (lx_t == T_NAME && strcmp(lex_name, "KIND") == 0 && lx_rp < lx_rpe && *lx_rp == '=') { f77_tok(); f77_tok(); }
    f77_cexpr();
    if (f77_cx_ty != TY_INT) { f77_error("a kind is an INTEGER constant"); return -1; }
    if (lx_t != T_RP) { f77_error("expected ) after a kind"); return -1; }
    f77_tok();
    if (ty == TY_DOUBLE) { f77_error("DOUBLE PRECISION takes no kind"); return -1; }
    r = f77_kind_ty(ty, f77_cx_i);
    if (r < 0) {
        sprintf(m, "kind %d of this type is not supported", f77_cx_i);
        f77_error(m);
    }
    return r;
}

/* An INTEGER constant expression: 1 and its value, else 0. */
static int f77_const_int(int *v) {
    f77_cexpr();
    *v = f77_cx_i;
    return f77_cx_ty == TY_INT;
}

/* f77_kind_sel in the unit scan: the type, or -2 when the scan cannot
 * tell (a name not in its table, or a kind not supported -- which the
 * compilation reports). */
static int f77_scan_kind_sel(int ty) {
    int r;
    f77_scan_mode = 1;
    f77_scan_bad = 0;
    r = f77_kind_sel(ty);
    f77_scan_mode = 0;
    return f77_scan_bad || r < 0 ? -2 : r;
}

/* The inquiry intrinsics (Fortran 90), by the argument's type alone:
 * HUGE TINY EPSILON (of the argument's type), and KIND RADIX DIGITS
 * MINEXPONENT MAXEXPONENT PRECISION RANGE (INTEGER) -- gfortran's numbers;
 * RANGE and PRECISION of a COMPLEX are its parts'.  The result's type
 * with its value in *iv and *dv; -1 when nm is none of them, -2 after an
 * error.  The constant evaluator and expressions share it. */
static int f77_is_inquiry(char *nm) {
    static const char *names[] = { "HUGE", "TINY", "EPSILON", "KIND", "RADIX", "DIGITS", "MINEXPONENT",
                                   "MAXEXPONENT", "PRECISION", "RANGE", 0 };
    int k;
    for (k = 0; names[k]; k++) if (strcmp(nm, names[k]) == 0) return 1;
    return 0;
}
static int f77_inquiry(char *nm, int t, int *iv, double *dv) {
    int dbl;
    *iv = 0;
    *dv = 0.0;
    if (!f77_is_inquiry(nm)) return -1;
    dbl = t == TY_DOUBLE || t == TY_DCPLX;
    if (nm[0] == 'H' || nm[0] == 'T' || nm[0] == 'E') {
        int big;
        big = nm[0] == 'H';
        if (t == TY_INT && big) { *iv = 2147483647; *dv = 2147483647.0; return TY_INT; }
        if (t == TY_FLOAT) *dv = big ? 3.4028234663852886e38 : nm[0] == 'T' ? 1.1754943508222875e-38 : 1.1920928955078125e-07;
        else if (t == TY_DOUBLE) *dv = big ? 1.7976931348623157e308 : nm[0] == 'T' ? 2.2250738585072014e-308 : 2.220446049250313e-16;
        else { f77_error("HUGE, TINY and EPSILON need a REAL or DOUBLE PRECISION argument (HUGE an INTEGER too)"); return -2; }
        return t;
    }
    if (strcmp(nm, "KIND") == 0) *iv = f77_ty_kind(t);
    else if (t == TY_CHAR) { f77_error("this inquiry needs a numeric argument"); return -2; }
    else if (strcmp(nm, "RADIX") == 0) *iv = 2;
    else if (t == TY_INT) {
        if (strcmp(nm, "DIGITS") == 0) *iv = 31;
        else if (strcmp(nm, "RANGE") == 0) *iv = 9;
        else { f77_error("this inquiry needs a REAL or COMPLEX argument"); return -2; }
    } else if (strcmp(nm, "DIGITS") == 0) *iv = dbl ? 53 : 24;
    else if (strcmp(nm, "MINEXPONENT") == 0) *iv = dbl ? -1021 : -125;
    else if (strcmp(nm, "MAXEXPONENT") == 0) *iv = dbl ? 1024 : 128;
    else if (strcmp(nm, "PRECISION") == 0) *iv = dbl ? 15 : 6;
    else *iv = dbl ? 307 : 37;
    *dv = (double)*iv;
    return TY_INT;
}

/* A number with a kind suffix, just scanned (the lexer's lx_kind_hook):
 * the suffix is digits or an INTEGER named constant, and a REAL constant
 * of kind 8 becomes DOUBLE PRECISION -- its value was read in double
 * precision already. */
static void f77_kind_lit(void) {
    char nm[F77_MAX_NAME];
    char m[96];
    int k;
    strcpy(nm, lex_kind);
    lex_kind[0] = 0;
    if (lx_isdigit(nm[0])) k = atoi(nm);
    else if (f77_scan_mode) {
        if (!f77_scan_const(nm, &k)) { f77_scan_bad = 1; return; }
    } else {
        int s;
        s = f77_sym_find(nm);
        if (s < 0 || !f77_sparam[s] || f77_sty[s] != TY_INT) {
            sprintf(m, "the kind of a constant must be digits or an INTEGER named constant, not %s", nm);
            f77_error(m);
            return;
        }
        k = f77_spival[s];
    }
    if (lx_t == T_DCON) { f77_error("a constant has a D exponent and a kind both"); return; }
    if (f77_kind_ty(lx_t == T_ICON ? TY_INT : TY_FLOAT, k) < 0) {
        sprintf(m, "%s kind %d is not supported", lx_t == T_ICON ? "INTEGER" : "REAL", k);
        f77_error(m);
        return;
    }
    if (lx_t == T_RCON && k == 8) lx_t = T_DCON;
}

/* An intrinsic in a constant expression (Fortran 90 initialization
 * expressions: elemental intrinsics of constants, and the inquiries):
 * HUGE TINY EPSILON, DCMPLX CMPLX, DBLE REAL INT NINT ABS, MAX MIN MOD.
 * lx_t is the name, '(' next.  0 when the name is none of these (or is an
 * array of the unit): the caller goes on as before. */
static int f77_cintrinsic(void) {
    static const char *names[] = { "HUGE", "TINY", "EPSILON", "DCMPLX", "CMPLX", "DBLE", "REAL",
                                   "INT", "NINT", "ABS", "MAX", "MIN", "MOD", "KIND", "RADIX", "DIGITS",
                                   "MINEXPONENT", "MAXEXPONENT", "PRECISION", "RANGE", "SELECTED_REAL_KIND",
                                   "SELECTED_INT_KIND", "SQRT", "CEILING", "FLOOR", 0 };
    char nm[F77_MAX_NAME];
    int ty[8]; int iv[8]; double dv[8]; double d2[8];
    int n;
    int k;
    int s;
    int rty;
    for (k = 0; names[k]; k++) if (strcmp(lex_name, names[k]) == 0) break;
    if (!names[k]) return 0;
    s = f77_sym_find(lex_name);
    if (s >= 0 && (f77_srank[s] > 0 || f77_sparam[s])) return 0;
    strcpy(nm, lex_name);
    f77_tok();                                   /* the name */
    f77_tok();                                   /* ( */
    n = 0;
    for (;;) {
        f77_cexpr();
        if (n < 8) {
            ty[n] = f77_cx_ty; iv[n] = f77_cx_i; dv[n] = f77_cx_d;
            d2[n] = f77_iscx(f77_cx_ty) ? f77_cx_d2 : 0.0;
            if (ty[n] == TY_INT) dv[n] = (double)iv[n];
        }
        n = n + 1;
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) { f77_error("expected ) after an intrinsic's arguments"); return 1; }
    f77_tok();
    if (n > 8) { f77_error("too many arguments to an intrinsic in a constant expression"); return 1; }
    f77_cx_d2 = 0.0;
    if (f77_is_inquiry(nm)) {
        int r;
        r = f77_inquiry(nm, ty[0], &f77_cx_i, &f77_cx_d);
        if (r < 0) return 1;
        f77_cx_ty = r;
        if (r != TY_INT) f77_cx_i = 0;
        return 1;
    }
    if (strcmp(nm, "SELECTED_REAL_KIND") == 0 || strcmp(nm, "SELECTED_INT_KIND") == 0) {
        /* the kinds this compiler has: -1 when none will do (for REAL, -1
         * past the precision, -2 past the range, -3 past both) */
        int k;
        for (k = 0; k < n; k++)
            if (ty[k] != TY_INT) { f77_error("SELECTED_REAL_KIND and SELECTED_INT_KIND take INTEGER arguments"); return 1; }
        f77_cx_ty = TY_INT;
        if (nm[9] == 'I') f77_cx_i = iv[0] <= 9 ? 4 : -1;
        else {
            int p; int r;
            p = n > 0 ? iv[0] : 0;
            r = n > 1 ? iv[1] : 0;
            if (p <= 6 && r <= 37) f77_cx_i = 4;
            else if (p <= 15 && r <= 307) f77_cx_i = 8;
            else f77_cx_i = (p > 15 ? -1 : 0) + (r > 307 ? -2 : 0);
        }
        f77_cx_d = (double)f77_cx_i;
        return 1;
    }
    if (strcmp(nm, "SQRT") == 0) {
        /* correctly rounded by IEEE 754 on every host, so the host's
         * sqrt folds it as the target computes it; a REAL in double then
         * rounded to single is correctly rounded too (53 >= 2*24 + 2) */
        if (ty[0] != TY_FLOAT && ty[0] != TY_DOUBLE) { f77_error("SQRT in a constant expression needs a REAL or DOUBLE PRECISION argument"); return 1; }
        if (dv[0] < 0) { f77_error("SQRT of a negative constant"); return 1; }
        f77_cx_ty = ty[0];
        f77_cx_d = sqrt(dv[0]);
        f77_cx_round();
        f77_cx_i = (int)f77_cx_d;
        return 1;
    }
    if (strcmp(nm, "CEILING") == 0 || strcmp(nm, "FLOOR") == 0) {
        double t;
        if (ty[0] != TY_FLOAT && ty[0] != TY_DOUBLE) { f77_error("CEILING and FLOOR need a REAL argument"); return 1; }
        t = nm[0] == 'C' ? ceil(dv[0]) : floor(dv[0]);
        if (t > 2147483647.0 || t < -2147483648.0) { f77_error("CEILING or FLOOR out of INTEGER range"); return 1; }
        f77_cx_ty = TY_INT;
        f77_cx_i = (int)t;
        f77_cx_d = t;
        return 1;
    }
    if (strcmp(nm, "DCMPLX") == 0 || strcmp(nm, "CMPLX") == 0) {
        f77_cx_ty = nm[0] == 'D' ? TY_DCPLX : TY_CPLX;
        if (nm[0] == 'C' && n == 3) {               /* CMPLX(X, Y, KIND) */
            f77_cx_ty = f77_kind_ty(TY_CPLX, iv[2]);
            if (ty[2] != TY_INT || f77_cx_ty < 0) { f77_error("CMPLX's KIND is 4 or 8"); return 1; }
        }
        f77_cx_d = dv[0];
        f77_cx_d2 = n > 1 ? dv[1] : d2[0];
        f77_cx_round();
        f77_cx_i = (int)f77_cx_d;
        return 1;
    }
    if (strcmp(nm, "DBLE") == 0 || strcmp(nm, "REAL") == 0 || strcmp(nm, "INT") == 0 || strcmp(nm, "NINT") == 0) {
        /* of a COMPLEX the real part; REAL of a COMPLEX*16 is DOUBLE
         * PRECISION, as gfortran has it */
        rty = nm[0] == 'D' ? TY_DOUBLE : nm[0] == 'R' ? (ty[0] == TY_DCPLX ? TY_DOUBLE : TY_FLOAT) : TY_INT;
        if (n == 2 && nm[0] != 'D') {               /* REAL(X, KIND), INT(X, KIND), NINT(X, KIND) */
            rty = f77_kind_ty(rty, iv[1]);
            if (ty[1] != TY_INT || rty < 0) { f77_error("this kind is not supported"); return 1; }
        }
        f77_cx_ty = rty;
        if (rty == TY_INT) {
            double t;
            t = dv[0];
            if (nm[0] == 'N') t = t < 0 ? t - 0.5 : t + 0.5;
            f77_cx_i = (int)t;
            f77_cx_d = (double)f77_cx_i;
            return 1;
        }
        f77_cx_d = dv[0];
        f77_cx_round();
        f77_cx_i = (int)f77_cx_d;
        return 1;
    }
    if (strcmp(nm, "ABS") == 0) {
        f77_cx_ty = ty[0];
        if (f77_iscx(ty[0])) {
            /* |z| is a hypot, which no libm rounds the same everywhere */
            f77_error("ABS of a COMPLEX in a constant expression is not supported");
            return 1;
        } else {
            f77_cx_d = dv[0] < 0 ? -dv[0] : dv[0];
            f77_cx_i = iv[0] < 0 ? -iv[0] : iv[0];
            if (ty[0] == TY_INT) return 1;
        }
        f77_cx_round();
        f77_cx_i = (int)f77_cx_d;
        return 1;
    }
    /* MAX MIN MOD: one type for all the arguments */
    rty = ty[0];
    for (k = 1; k < n; k++) rty = f77_cx_promote(rty, ty[k]);
    if (f77_iscx(rty)) { f77_error("MAX, MIN and MOD do not take COMPLEX"); return 1; }
    f77_cx_ty = rty;
    if (strcmp(nm, "MOD") == 0) {
        if (n != 2) { f77_error("MOD takes two arguments"); return 1; }
        if (rty == TY_INT) {
            if (iv[1] == 0) { f77_error("MOD by zero in a constant expression"); return 1; }
            f77_cx_i = iv[0] % iv[1];
            f77_cx_d = (double)f77_cx_i;
            return 1;
        }
        /* fmod is exact, so the host's is the target's */
        if (dv[1] == 0) { f77_error("MOD by zero in a constant expression"); return 1; }
        f77_cx_d = fmod(dv[0], dv[1]);
        f77_cx_round();
        f77_cx_i = (int)f77_cx_d;
        return 1;
    } else {
        double r;
        int ri;
        r = dv[0]; ri = iv[0];
        for (k = 1; k < n; k++) {
            if (nm[1] == 'A' ? dv[k] > r : dv[k] < r) { r = dv[k]; ri = iv[k]; }
        }
        if (rty == TY_INT) { f77_cx_i = ri; f77_cx_d = (double)ri; return 1; }
        f77_cx_d = r;
    }
    f77_cx_round();
    f77_cx_i = (int)f77_cx_d;
    return 1;
}

static void f77_cprimary(void) {
    if (lx_t == T_SCON) {
        f77_cx_ty = TY_CHAR; f77_cx_sidx = lex_sidx; f77_cx_i = lex_slen; f77_cx_d = 0.0;
        f77_tok();
        return;
    }
    if (lx_t == T_ICON) { f77_cx_ty = TY_INT; f77_cx_i = lex_ival; f77_cx_d = (double)lex_ival; f77_tok(); return; }
    if (lx_t == T_RCON) { f77_cx_ty = TY_FLOAT; f77_cx_d = (double)(float)lex_dval; f77_cx_i = (int)f77_cx_d; f77_tok(); return; }
    if (lx_t == T_DCON) { f77_cx_ty = TY_DOUBLE; f77_cx_d = lex_dval; f77_cx_i = (int)lex_dval; f77_tok(); return; }
    if (lx_t == T_TRUE)  { f77_cx_ty = TY_INT; f77_cx_i = 1; f77_cx_d = 1.0; f77_tok(); return; }
    if (lx_t == T_FALSE) { f77_cx_ty = TY_INT; f77_cx_i = 0; f77_cx_d = 0.0; f77_tok(); return; }
    if (lx_t == T_LP) {
        f77_tok();
        f77_cexpr();
        if (lx_t == T_COMMA) {
            double re;
            int rty;
            rty = f77_cx_ty;
            re = f77_cx_part(f77_cx_d, rty);
            f77_tok();
            f77_cexpr();
            if (lx_t != T_RP) { f77_error("expected ) after complex constant"); return; }
            f77_tok();
            f77_cx_d2 = f77_cx_part(f77_cx_d, f77_cx_ty);
            f77_cx_ty = (rty == TY_DOUBLE || f77_cx_ty == TY_DOUBLE) ? TY_DCPLX : TY_CPLX;
            f77_cx_d = re;
            f77_cx_i = (int)re;
            return;
        }
        if (lx_t != T_RP) { f77_error("expected ) in constant expression"); return; }
        f77_tok();
        return;
    }
    if (lx_t == T_MINUS) {
        f77_tok();
        f77_cpower();                    /* -2**2 is -(2**2) */
        f77_cx_i = 0 - f77_cx_i;
        f77_cx_d = 0.0 - f77_cx_d;
        /* and a complex value's imaginary part: -(1.0,2.0) was (-1,2) */
        if (f77_iscx(f77_cx_ty)) f77_cx_d2 = 0.0 - f77_cx_d2;
        return;
    }
    if (lx_t == T_PLUS) { f77_tok(); f77_cpower(); return; }
    if (lx_t == T_NAME && lx_rp < lx_rpe && *lx_rp == '(' && f77_cintrinsic()) return;
    if (lx_t == T_NAME) {
        int s;
        int b;
        for (b = f77_cxb_n - 1; b >= 0; b--) {
            if (strcmp(f77_cxb_name[b], lex_name) == 0) {
                f77_cx_ty = TY_INT; f77_cx_i = f77_cxb_val[b]; f77_cx_d = (double)f77_cx_i;
                f77_tok();
                return;
            }
        }
        if (f77_scan_mode) {
            /* the unit scan: its table of INTEGER named constants */
            int v;
            if (!f77_scan_const(lex_name, &v)) f77_scan_bad = 1;
            f77_cx_ty = TY_INT; f77_cx_i = v; f77_cx_d = (double)v;
            f77_tok();
            return;
        }
        s = f77_sym(lex_name);
        if (f77_sparam[s] && f77_sty[s] == TY_CHAR) {
            f77_cx_ty = TY_CHAR; f77_cx_sidx = f77_spstr[s]; f77_cx_i = f77_slen[s]; f77_cx_d = 0.0;
            f77_tok();
            return;
        }
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

/* fort370: ** in a constant expression, right-associative; F77 (6.1.2,
 * 7.1.2) lets a constant expression raise to an INTEGER power only, so
 * it is repeated multiplication.  It was refused: PARAMETER (N =
 * 2**10) said "expected ) after PARAMETER list". */
static void f77_cpower(void) {
    int bty;
    int bi;
    double bd;
    double bd2;
    int n;
    int k;
    f77_cprimary();
    if (lx_t != T_POWER) return;
    bty = f77_cx_ty; bi = f77_cx_i; bd = f77_cx_d; bd2 = f77_iscx(bty) ? f77_cx_d2 : 0.0;
    f77_tok();
    f77_cpower();
    if (f77_cx_ty != TY_INT) {
        f77_error("a constant expression raises to an INTEGER power only");
        f77_cx_ty = bty; f77_cx_i = bi; f77_cx_d = bd; f77_cx_d2 = bd2;
        return;
    }
    n = f77_cx_i;
    f77_cx_ty = bty;
    if (bty == TY_INT) {
        int r;
        if (n < 0) {
            if (bi == 0) { f77_error("zero to a negative power in constant expression"); r = 0; }
            else r = (bi == 1) ? 1 : (bi == -1) ? ((-n) % 2 ? -1 : 1) : 0;
        } else {
            r = 1;
            for (k = 0; k < n; k++) r = r * bi;
        }
        f77_cx_i = r;
        f77_cx_d = (double)r;
    } else if (f77_iscx(bty)) {
        double rr; double ri; double t;
        rr = 1.0; ri = 0.0;
        for (k = 0; k < (n < 0 ? -n : n); k++) { t = rr * bd - ri * bd2; ri = rr * bd2 + ri * bd; rr = t; }
        if (n < 0) {
            double den;
            den = rr * rr + ri * ri;
            if (den == 0.0) { f77_error("zero to a negative power in constant expression"); den = 1.0; }
            rr = rr / den; ri = 0.0 - ri / den;
        }
        f77_cx_d = rr; f77_cx_d2 = ri; f77_cx_i = (int)rr;
        f77_cx_round();
    } else {
        double r;
        r = 1.0;
        for (k = 0; k < (n < 0 ? -n : n); k++) r = r * bd;
        if (n < 0) {
            if (r == 0.0) { f77_error("zero to a negative power in constant expression"); r = 1.0; }
            r = 1.0 / r;
        }
        f77_cx_d = r;
        f77_cx_round();
        f77_cx_i = (int)f77_cx_d;
    }
}

static int f77_cx_promote(int a, int b) {
    if (a == TY_DCPLX || b == TY_DCPLX ||
        (a == TY_CPLX && b == TY_DOUBLE) || (a == TY_DOUBLE && b == TY_CPLX)) return TY_DCPLX;
    if (a == TY_CPLX || b == TY_CPLX) return TY_CPLX;
    if (a == TY_DOUBLE || b == TY_DOUBLE) return TY_DOUBLE;
    if (a == TY_FLOAT || b == TY_FLOAT) return TY_FLOAT;
    return TY_INT;
}

static void f77_cterm(void) {
    int ty; int iv; double dv; int mul; double d2v;
    f77_cpower();
    for (;;) {
        if (lx_t == T_STAR) mul = 1;
        else if (lx_t == T_SLASH) mul = 0;
        else return;
        ty = f77_cx_ty; iv = f77_cx_i; dv = f77_cx_d;
        d2v = f77_iscx(ty) ? f77_cx_d2 : 0.0;
        f77_tok();
        f77_cpower();
        if (f77_iscx(ty) || f77_iscx(f77_cx_ty)) {
            /* fort370: both parts (fort370 issue 11); the evaluator
             * took the real parts alone, and (1,2)*(3,4) came out (3,4) */
            double br; double bi; double re; double im;
            br = f77_cx_d;
            bi = f77_iscx(f77_cx_ty) ? f77_cx_d2 : 0.0;
            f77_cx_ty = f77_cx_promote(ty, f77_cx_ty);
            if (mul) {
                re = dv * br - d2v * bi;
                im = dv * bi + d2v * br;
            } else {
                double den;
                den = br * br + bi * bi;
                if (den == 0.0) { f77_error("division by zero in constant expression"); den = 1.0; }
                re = (dv * br + d2v * bi) / den;
                im = (d2v * br - dv * bi) / den;
            }
            f77_cx_d = re;
            f77_cx_d2 = im;
            f77_cx_round();
            f77_cx_i = (int)f77_cx_d;
            continue;
        }
        f77_cx_ty = f77_cx_promote(ty, f77_cx_ty);
        if (mul) {
            f77_cx_i = iv * f77_cx_i;
            f77_cx_d = dv * f77_cx_d;
        } else if (f77_cx_ty == TY_INT) {
            if (f77_cx_i == 0) { f77_error("division by zero in constant expression"); f77_cx_i = 1; }
            /* -2**31 / -1 overflows a C int; fold it as DIV does (SPEC 4.1) */
            f77_cx_i = f77_cx_i == -1 ? (int)(0u - (unsigned int)iv) : iv / f77_cx_i;
        } else {
            if (f77_cx_d == 0.0) { f77_error("division by zero in constant expression"); f77_cx_d = 1.0; }
            f77_cx_d = dv / f77_cx_d;
        }
        f77_cx_round();
        if (f77_cx_ty == TY_INT) f77_cx_d = (double)f77_cx_i;
        else f77_cx_i = (int)f77_cx_d;
    }
}

static void f77_cexpr(void) {
    int ty; int iv; double dv; int add; double d2v;
    f77_cterm();
    for (;;) {
        if (lx_t == T_PLUS) add = 1;
        else if (lx_t == T_MINUS) add = 0;
        else return;
        ty = f77_cx_ty; iv = f77_cx_i; dv = f77_cx_d;
        d2v = f77_iscx(ty) ? f77_cx_d2 : 0.0;
        f77_tok();
        f77_cterm();
        if (f77_iscx(ty) || f77_iscx(f77_cx_ty)) {
            double bi;
            bi = f77_iscx(f77_cx_ty) ? f77_cx_d2 : 0.0;
            f77_cx_ty = f77_cx_promote(ty, f77_cx_ty);
            f77_cx_d = add ? dv + f77_cx_d : dv - f77_cx_d;
            f77_cx_d2 = add ? d2v + bi : d2v - bi;
            f77_cx_round();
            f77_cx_i = (int)f77_cx_d;
            continue;
        }
        f77_cx_ty = f77_cx_promote(ty, f77_cx_ty);
        if (add) { f77_cx_i = iv + f77_cx_i; f77_cx_d = dv + f77_cx_d; }
        else     { f77_cx_i = iv - f77_cx_i; f77_cx_d = dv - f77_cx_d; }
        f77_cx_round();
        if (f77_cx_ty == TY_INT) f77_cx_d = (double)f77_cx_i;
        else f77_cx_i = (int)f77_cx_d;
    }
}

/* One dimension bound: `*` (assumed size), a non-PARAMETER name (an
 * adjustable dimension), or a constant expression -- which is where a
 * PARAMETER bound like A(N) or A(2*N+1) resolves. */
/* One dimension bound.  A constant expression gives its value; `*` is
 * an assumed size; an expression naming anything that is not a
 * PARAMETER (a dummy, say: A(LDA, 2*N+1)) is an adjustable bound,
 * evaluated here in the declaration part -- the entry block, which
 * dominates every subscript -- and returned through *val. */
static int f77_bound_star;      /* set by f77_bound on a `*` */
static int f77_bound(int *val) {
    int i;
    int depth;
    int runtime;
    *val = -1;
    if (lx_t == T_STAR) { f77_tok(); f77_bound_star = 1; return 1; }   /* assumed size */
    runtime = 0;
    i = (int)(lx_rts - lx_stmt);
    depth = 0;
    while (i < lx_stmt_len) {
        int c;
        c = lx_stmt[i];
        if (c == '(') depth = depth + 1;
        else if (c == ')') { if (depth == 0) break; depth = depth - 1; }
        else if ((c == ',' || c == ':') && depth == 0) break;
        if (lx_isalpha(c)) {
            char nm[F77_MAX_NAME];
            int j;
            int sf;
            j = i;
            while (j < lx_stmt_len && (lx_isalpha(lx_stmt[j]) || lx_isdigit(lx_stmt[j]))) j = j + 1;
            if (j - i < F77_MAX_NAME) {
                memcpy(nm, lx_stmt + i, j - i);
                nm[j - i] = 0;
                sf = f77_sym_find(nm);
                if (sf < 0 || !f77_sparam[sf]) runtime = 1;
            }
            i = j;
            continue;
        }
        i = i + 1;
    }
    if (runtime) {
        int v;
        v = f77_expr();
        *val = f77_cvt(v, &ex_hi, ex_ty, TY_INT);
        return 1;
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
    f77_bound_star = 0;
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
            f77_slo[s][rank] = (losym >= 0) ? 0 : lo;
            f77_slov[s][rank] = losym;
            if (hisym >= 0 || losym >= 0) {
                /* extent = hi - lo + 1 at run time */
                int hv; int lv; int e;
                hv = (hisym >= 0) ? hisym : f77_iconst(hi);
                lv = (losym >= 0) ? losym : f77_iconst(lo);
                e = hi_emit(HI_SUB, TY_INT, hv, lv, 0, NULL);
                f77_sextv[s][rank] = hi_emit(HI_ADDI, TY_INT, e, -1, 1, NULL);
                f77_sext[s][rank] = 1;      /* size unknown until run time */
            } else {
                f77_sextv[s][rank] = -1;
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
    f77_sassz[s] = f77_bound_star;
    f77_sauto[s] = !f77_sarg[s] && !f77_salloc[s] && f77_adjustable(s);
}

/* A CHARACTER length after the `*`: n, (cexpr), or (*) for an
 * assumed-length dummy (returned as -1). */
static void f77_cexpr(void);
/* fort370: a numeric type's length after the `*` (the current token),
 * read as digits from the text and the scan resumed after them -- the
 * scanner would take REAL*8E1 as the constant 8E1.  Returns 1 with *n,
 * or 0 (no digits; the scan moved past the star). */
static int f77_star_len(int *n) {
    int off;
    int k;
    int v;
    off = (int)(lx_rte - lx_stmt);
    k = 0;
    v = 0;
    while (off + k < lx_stmt_len && lx_isdigit(lx_stmt[off + k])) { v = v * 10 + (lx_stmt[off + k] - '0'); k = k + 1; }
    if (k == 0) { f77_tok(); return 0; }
    *n = v;
    f77_scan_from(off + k);
    return 1;
}

static int f77_char_len(void) {
    int n;
    if (lx_t == T_ICON) { n = lex_ival; f77_tok(); return n; }
    if (lx_t == T_RCON || lx_t == T_DCON) {
        /* CHARACTER*2D2XVK: the scanner read 2D2 as a constant.  The
         * digits are the length; the scanner restarts after them. */
        int off;
        off = (int)(lx_rts - lx_stmt);
        n = 0;
        while (off < lx_stmt_len && lx_isdigit(lx_stmt[off])) {
            n = n * 10 + (lx_stmt[off] - '0');
            off = off + 1;
        }
        f77_scan_from(off);
        return n;
    }
    if (lx_t != T_LP) { f77_error("bad CHARACTER length"); return 1; }
    f77_tok();
    if (lx_t == T_STAR) {
        f77_tok();
        if (lx_t != T_RP) { f77_error("expected ) after CHARACTER*(*"); return 1; }
        f77_tok();
        return -1;
    }
    f77_cexpr();
    if (lx_t != T_RP) { f77_error("expected ) after CHARACTER length"); return 1; }
    f77_tok();
    if (f77_cx_ty != TY_INT || f77_cx_i < 0) { f77_error("CHARACTER length must be an integer constant"); return 1; }
    return f77_cx_i;
}

/* A deferred shape, (:) or (:,:,...), lx_t at the '(': the rank of an
 * ALLOCATABLE array, its bounds left to ALLOCATE.  1, or 0 on error. */
static int f77_parse_deferred(int s) {
    int rank;
    int k;
    rank = 0;
    f77_tok();
    for (;;) {
        if (lx_t != T_COLON) { f77_error("an ALLOCATABLE array's shape is (:), (:,:) ..., its bounds given by ALLOCATE"); return 0; }
        f77_tok();
        if (rank >= F77_MAX_RANK) { f77_error("too many dimensions"); return 0; }
        rank = rank + 1;
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) { f77_error("expected ) after a deferred shape"); return 0; }
    f77_tok();
    f77_srank[s] = rank;
    for (k = 0; k < rank; k++) {
        f77_slo[s][k] = 1;
        f77_sext[s][k] = 1;
        f77_slov[s][k] = -1;
        f77_sextv[s][k] = -1;
        f77_sextsym[s][k] = -1;
    }
    f77_sassz[s] = 0;
    f77_salloc[s] = 1;
    return 1;
}

/* Type declarations, and DIMENSION (which sets shape without a type).
 * Fortran 90's form too: type [, attribute]... :: entity [= value], ...
 * with CHARACTER(LEN=n) or CHARACTER(n) for the length. */
static int f77_decl_init(int s);
static int f77_decl_init_eq(int s);
static int f77_param_def(int s);
static int f77_save_note(int s);
static void f77_stmt_decl(int ty, int skip) {
    int s;
    int clen;
    int tnar;
    int dcolon;
    int has_dc;
    int a_param;
    int a_save;
    int a_ext;
    int a_intr;
    int a_alloc;
    int dim_off;
    clen = 1;
    a_alloc = 0;
    tnar = 0;
    dcolon = 0;
    a_param = 0; a_save = 0; a_ext = 0; a_intr = 0;
    dim_off = -1;
    {
        int c;
        c = f77_toplevel(':', 0);
        has_dc = c >= 0 && c + 1 < lx_stmt_len && lx_stmt[c + 1] == ':';
    }
    f77_scan_from(skip);
    /* Type-level length: REAL*8 X, CHARACTER*10 S.  Per-name forms
     * (REAL X*8, CHARACTER S*10) are below. */
    if (lx_t == T_STAR) {
        int n;
        if (ty == TY_CHAR) {
            f77_tok();
            clen = f77_char_len();
        } else if (f77_star_len(&n)) {
            ty = f77_lenty(ty, n, 1);
            tnar = f77_lennar(ty, n, f77_decl_log);
        }
        if (lx_t == T_COMMA && !has_dc) f77_tok();       /* CHARACTER*10, A, B */
    } else if (lx_t == T_LP && ty == TY_CHAR) {
        /* CHARACTER(LEN=n), CHARACTER(n), CHARACTER(LEN=*) */
        if (lx_rpe - lx_rp >= 4 && strncmp(lx_rp, "LEN=", 4) == 0 && lx_rp[4] != '=') lx_rp = lx_rp + 4;
        clen = f77_char_len();
    } else if (lx_t == T_LP && ty >= 0) {
        /* a kind: REAL(WP), REAL(KIND=8), COMPLEX(DP) (Fortran 90) */
        ty = f77_kind_sel(ty);
        if (ty < 0) return;
    }
    if (has_dc) {
        /* [, attribute]... :: */
        while (lx_t == T_COMMA) {
            char at[F77_MAX_NAME];
            f77_tok();
            if (lx_t != T_NAME) { f77_error("expected an attribute"); return; }
            strcpy(at, lex_name);
            f77_tok();
            if (strcmp(at, "PARAMETER") == 0) a_param = 1;
            else if (strcmp(at, "SAVE") == 0) a_save = 1;
            else if (strcmp(at, "EXTERNAL") == 0) a_ext = 1;
            else if (strcmp(at, "INTRINSIC") == 0) a_intr = 1;
            else if (strcmp(at, "DIMENSION") == 0) {
                int end;
                if (lx_t != T_LP) { f77_error("DIMENSION needs (bounds)"); return; }
                dim_off = (int)(lx_rts - lx_stmt);
                end = f77_match_paren(dim_off);
                if (end < 0) { f77_error("expected ) after DIMENSION bounds"); return; }
                f77_scan_from(end);
            } else if (strcmp(at, "INTENT") == 0) {
                /* recorded nowhere yet: checking it is the caller's
                 * business, and F90 does not require a processor to */
                if (lx_t != T_LP) { f77_error("INTENT needs (IN), (OUT) or (INOUT)"); return; }
                f77_tok();
                if (lx_t != T_NAME || (strcmp(lex_name, "IN") != 0 && strcmp(lex_name, "OUT") != 0 &&
                                       strcmp(lex_name, "INOUT") != 0)) {
                    f77_error("INTENT needs (IN), (OUT) or (INOUT)");
                    return;
                }
                f77_tok();
                if (lx_t != T_RP) { f77_error("expected ) after INTENT"); return; }
                f77_tok();
            } else if (strcmp(at, "ALLOCATABLE") == 0) {
                a_alloc = 1;
            } else if (strcmp(at, "POINTER") == 0 ||
                       strcmp(at, "TARGET") == 0 || strcmp(at, "OPTIONAL") == 0 ||
                       strcmp(at, "PUBLIC") == 0 || strcmp(at, "PRIVATE") == 0) {
                char m[96];
                sprintf(m, "the %s attribute is not supported yet", at);
                f77_error(m);
                return;
            } else {
                char m[96];
                sprintf(m, "%s is not an attribute", at);
                f77_error(m);
                return;
            }
        }
        if (lx_t != T_COLON) { f77_error("expected :: in a declaration"); return; }
        f77_tok();
        if (lx_t != T_COLON) { f77_error("expected :: in a declaration"); return; }
        f77_tok();
        dcolon = 1;
    }
    for (;;) {
        if (lx_t != T_NAME) break;
        s = f77_sym(lex_name);
        if (ty >= 0) {                  /* declaration overrides implicit typing */
            f77_sty[s] = ty;
            f77_styped[s] = 1;
            f77_slog[s] = f77_decl_log;
            f77_snar[s] = tnar;
            if (ty == TY_CHAR) f77_slen[s] = clen;
        }
        f77_tok();
        /* name*len(dims) and name(dims)*len are both F77. */
        if (lx_t == T_LP && a_alloc) { if (!f77_parse_deferred(s)) return; }
        else if (lx_t == T_LP) f77_parse_dims(s);
        if (lx_t == T_STAR) {           /* REAL*8 X -- length specifier */
            int n;
            if (f77_sty[s] == TY_CHAR) {
                f77_tok();
                f77_slen[s] = f77_char_len();
            } else if (f77_star_len(&n)) {
                f77_sty[s] = f77_lenty(f77_sty[s], n, 1);
                f77_snar[s] = f77_lennar(f77_sty[s], n, f77_slog[s]);
            }
            if (lx_t == T_LP) f77_parse_dims(s);
        }
        if (dcolon) {
            if (f77_srank[s] == 0 && dim_off >= 0) {
                /* the DIMENSION attribute: the shape of every entity
                 * that does not give its own */
                int pos;
                pos = (int)(lx_rts - lx_stmt);
                if (lx_t == T_EOF) pos = lx_stmt_len;
                f77_scan_from(dim_off);
                if (a_alloc) { if (!f77_parse_deferred(s)) return; }
                else f77_parse_dims(s);
                f77_scan_from(pos);
            }
            if (a_alloc) {
                if (!f77_salloc[s]) { f77_error("an ALLOCATABLE array needs its rank, as (:) or (:,:)"); return; }
                if (f77_sarg[s]) { f77_error("an ALLOCATABLE dummy argument is not supported"); return; }
                if (a_param || lx_t == T_ASSIGN) { f77_error("an ALLOCATABLE array cannot have a value in its declaration"); return; }
            }
            if (a_ext) f77_sexternal[s] = 1;
            if (a_intr) f77_sintrinsic[s] = 1;
            if (a_save && !f77_save_note(s)) return;
        }
        f77_realloc_sym(s);
        if (ty >= 0 && f77_ukind[f77_cur_unit] == F77_UNIT_FUNC && f77_urpend[f77_cur_unit] &&
            strcmp(f77_sname[s], f77_uname[f77_cur_unit]) == 0) {
            /* the result's kind, which the scan could not tell: known now,
             * before any reference in the unit (the scan refuses callers
             * that came first) */
            if (f77_sty[s] == TY_CHAR || f77_sty[s] == TY_DCPLX)   /* bound with hidden arguments, or without */
                f77_error("this FUNCTION's result kind must be known from its unit's INTEGER, PARAMETER statements");
            f77_urty[f77_cur_unit] = f77_sty[s];
            f77_urpend[f77_cur_unit] = 0;
        }
        if (lx_t == T_SLASH && ty >= 0 && !dcolon && !f77_decl_init(s)) return;
        if (dcolon && a_param) {
            if (lx_t != T_ASSIGN) { f77_error("a PARAMETER needs = value"); return; }
            if (!f77_param_def(s)) return;
        } else if (dcolon && lx_t == T_ASSIGN) {
            if (!f77_decl_init_eq(s)) return;
        }
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (dcolon && lx_t != T_EOF) f77_error("unexpected text after the declaration");
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
    ps_glocal[f77_cblk_gidx[f77_ncblk]] = 0;    /* one block for every file naming it */
    f77_ncblk = f77_ncblk + 1;
    return f77_ncblk - 1;
}

/* A COMMON block is one object for the whole program, whatever files
 * name it.  A block DATA initializes (BLOCK DATA, or DATA in a unit) is
 * defined here, global; any other is a common symbol (.comm), which the
 * linker makes one block of, the largest size, a definition elsewhere
 * winning -- emitted by f77_cblk_comm after the backend's data, the
 * backend told it is extern.  Each block was a local label: two files
 * naming /INFOC/ had two blocks, and LAPACK's testing XERBLA read the
 * INFOT and SRNAMT its callers had never set. */
static void cg_s(char *s);
static void cg_n(int v);
static void f77_cblk_finish(void) {
    int i;
    for (i = 0; i < f77_ncblk; i++) {
        int g;
        g = f77_cblk_gidx[i];
        if (ps_ginit_start[g] < 0) ps_gextern[g] = 1;
    }
}
static void f77_cblk_comm(void) {
    int i;
    for (i = 0; i < f77_ncblk; i++) {
        int g;
        g = f77_cblk_gidx[i];
        if (!ps_gextern[g]) continue;
        cg_s(".comm ");
        cg_s(ps_gname[g]);
        cg_s(", ");
        cg_n(ps_gsize[g] > 0 ? ps_gsize[g] : 4);
        cg_s(", 8\n");
    }
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
/* One name SAVEd, as by the SAVE statement or the SAVE attribute. */
static int f77_save_note(int s) {
    if (f77_sv_n >= F77_MAX_CMEM) { f77_error("too many SAVE names"); return 0; }
    f77_sv_sym[f77_sv_n] = s;
    f77_sv_n = f77_sv_n + 1;
    return 1;
}

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
        int clen;
        int lg;
        int nar;
        clen = 1;
        lg = 0;
        nar = 0;
        if (lx_t != T_NAME) { f77_error("IMPLICIT needs a type"); return; }
        if (strcmp(lex_name, "INTEGER") == 0) ty = TY_INT;
        else if (strcmp(lex_name, "REAL") == 0) ty = TY_FLOAT;
        else if (strcmp(lex_name, "DOUBLEPRECISION") == 0) ty = TY_DOUBLE;
        else if (strcmp(lex_name, "DOUBLECOMPLEX") == 0) ty = TY_DCPLX;
        else if (strcmp(lex_name, "LOGICAL") == 0) { ty = TY_INT; lg = 1; }
        else if (strcmp(lex_name, "CHARACTER") == 0) ty = TY_CHAR;
        else if (strcmp(lex_name, "COMPLEX") == 0) ty = TY_CPLX;
        else { f77_error("IMPLICIT type not supported"); return; }
        f77_tok();
        if (lx_t == T_STAR) {
            int n;
            if (ty == TY_CHAR) { f77_tok(); clen = f77_char_len(); }
            else if (f77_star_len(&n)) { ty = f77_lenty(ty, n, 1); nar = f77_lennar(ty, n, lg); }
        }
        if (clen < 0) { f77_error("IMPLICIT CHARACTER*(*) is not allowed"); return; }
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
            for (c = lo; c <= hi; c++) { f77_imp_map[c - 'A'] = ty; f77_imp_len[c - 'A'] = clen; f77_imp_log[c - 'A'] = lg; f77_imp_nar[c - 'A'] = nar; }
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
        f77_slog[s] = f77_imp_log[f77_sname[s][0] - 'A'];
        f77_snar[s] = f77_imp_nar[f77_sname[s][0] - 'A'];
        if (ty == f77_sty[s] &&
            (ty != TY_CHAR || f77_slen[s] == f77_imp_len[f77_sname[s][0] - 'A'])) continue;
        if (s == f77_result_sym) {
            f77_error("IMPLICIT would change the FUNCTION result type; declare it in the header");
            return;
        }
        f77_sty[s] = ty;
        if (ty == TY_CHAR) f77_slen[s] = f77_imp_len[f77_sname[s][0] - 'A'];
        if (!f77_sarg[s]) f77_realloc_sym(s);
    }
}

/* PARAMETER (name = cexpr, ...) -- named constants, no storage.  The
 * symbol's type (declared or implicit, fixed by this point per F77's
 * ordering rules) decides which domain the value is served from. */
/* A PARAMETER's value: lx_t at the '=' after its name.  The PARAMETER
 * statement and the PARAMETER attribute (INTEGER, PARAMETER :: N = 3)
 * both come here.  0 on error. */
static int f77_param_def(int s) {
        if (lx_t != T_ASSIGN) { f77_error("expected = in PARAMETER"); return 0; }
        f77_tok();
        f77_cexpr();
        f77_sparam[s] = 1;
        if (f77_sty[s] == TY_CHAR) {
            if (f77_cx_ty != TY_CHAR) { f77_error("a CHARACTER PARAMETER needs a character constant"); return 0; }
            if (f77_slen[s] < 0) f77_slen[s] = f77_cx_i;       /* CHARACTER*(*) takes the constant's length */
            if (f77_slen[s] == f77_cx_i) {
                f77_spstr[s] = f77_cx_sidx;
            } else {
                /* Pad or truncate to the declared length. */
                char buf[F77_MAX_STMT];
                int k;
                for (k = 0; k < f77_slen[s] && k < F77_MAX_STMT - 1; k++)
                    buf[k] = (k < f77_cx_i) ? lex_strpool[lex_str_off[f77_cx_sidx] + k] : ' ';
                f77_spstr[s] = f77_intern_str(buf, k);
            }
        } else if (f77_cx_ty == TY_CHAR) {
            f77_error("a character constant cannot initialise a numeric PARAMETER");
            return 0;
        } else if (f77_sty[s] == TY_CPLX) {
            f77_spdval[s] = (f77_cx_ty == TY_INT) ? (double)f77_cx_i : f77_cx_d;
            f77_spdval2[s] = f77_iscx(f77_cx_ty) ? f77_cx_d2 : 0.0;
            f77_spival[s] = (int)f77_spdval[s];
        } else if (f77_sty[s] == TY_DCPLX) {
            /* each part as its constant's type makes it: a REAL one
             * rounded to single precision, then widened */
            f77_spdval[s] = (f77_cx_ty == TY_INT) ? (double)f77_cx_i : f77_cx_part(f77_cx_d, f77_cx_ty);
            f77_spdval2[s] = f77_iscx(f77_cx_ty) ? f77_cx_d2 : 0.0;
            f77_spival[s] = (int)f77_spdval[s];
        } else if (f77_sty[s] == TY_INT) {
            f77_spival[s] = (f77_cx_ty == TY_INT) ? f77_cx_i : (int)f77_cx_d;
            f77_spdval[s] = (double)f77_spival[s];
        } else {
            f77_spdval[s] = (f77_cx_ty == TY_INT) ? (double)f77_cx_i : f77_cx_d;
            if (f77_sty[s] == TY_FLOAT)
                f77_spdval[s] = (double)(float)f77_spdval[s];
            f77_spival[s] = (int)f77_spdval[s];
        }
    return 1;
}

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
        if (!f77_param_def(s)) return;
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) f77_error("expected ) after PARAMETER list");
    else f77_tok();
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
static int    f77_di_sidx[F77_MAX_DINIT];
static double f77_di_d2[F77_MAX_DINIT];
static unsigned long long f77_di_u[F77_MAX_DINIT];   /* TY_HEXC */
static int    f77_di_sub[F77_MAX_DINIT];   /* substring target: byte length, elem is a byte offset */
static int    f77_di_n;

/* Z'1F' or O'17' (MIL-STD-1753): digits of base 16 or 8 into
 * f77_cx_u.  Digits past 64 bits push the leftmost out, as the item
 * would.  Returns 0 if one of the digits is not. */
static int f77_bitconst(const char *d, int n, int base) {
    unsigned long long v;
    int k;
    int x;
    char c;
    if (n == 0) return 0;
    v = 0;
    for (k = 0; k < n; k++) {
        c = d[k];
        if (c >= '0' && c <= '9') x = c - '0';
        else if (c >= 'A' && c <= 'F') x = c - 'A' + 10;
        else if (c >= 'a' && c <= 'f') x = c - 'a' + 10;
        else return 0;
        if (x >= base) return 0;
        v = v * (unsigned long long)base + (unsigned long long)x;
    }
    f77_cx_ty = TY_HEXC;
    f77_cx_u = v;
    f77_cx_i = (int)(unsigned int)v;
    f77_cx_d = (double)f77_cx_i;
    return 1;
}

/* One DATA constant: sign, literal, .TRUE./.FALSE., or a PARAMETER
 * name.  NOT f77_cexpr: `/` is the value-list delimiter here, and the
 * evaluator would read it as division. */
static void f77_data_const(void) {
    int neg;
    neg = 0;
    if (lx_t == T_MINUS) { neg = 1; f77_tok(); }
    else if (lx_t == T_PLUS) f77_tok();
    if (lx_t == T_ICON) { f77_cx_ty = TY_INT; f77_cx_i = lex_ival; f77_cx_d = (double)lex_ival; f77_tok(); }
    else if (lx_t == T_RCON) { f77_cx_ty = TY_FLOAT; f77_cx_d = (double)(float)lex_dval; f77_cx_i = (int)f77_cx_d; f77_tok(); }
    else if (lx_t == T_DCON) { f77_cx_ty = TY_DOUBLE; f77_cx_d = lex_dval; f77_cx_i = (int)lex_dval; f77_tok(); }
    else if (lx_t == T_TRUE)  { f77_cx_ty = TY_INT; f77_cx_i = 1; f77_cx_d = 1.0; f77_tok(); }
    else if (lx_t == T_FALSE) { f77_cx_ty = TY_INT; f77_cx_i = 0; f77_cx_d = 0.0; f77_tok(); }
    else if (lx_t == T_SCON) { f77_cx_ty = TY_CHAR; f77_cx_sidx = lex_sidx; f77_cx_i = lex_slen; f77_cx_d = 0.0; f77_tok(); }
    else if (lx_t == T_LP) {
        /* (re, im) */
        double re;
        int rty;
        f77_tok();
        f77_data_const();
        rty = f77_cx_ty;
        re = f77_cx_part(f77_cx_d, rty);
        if (lx_t != T_COMMA) { f77_error("expected , in complex constant"); return; }
        f77_tok();
        f77_data_const();
        if (lx_t != T_RP) { f77_error("expected ) after complex constant"); return; }
        f77_tok();
        f77_cx_d2 = f77_cx_part(f77_cx_d, f77_cx_ty);
        f77_cx_ty = (rty == TY_DOUBLE || f77_cx_ty == TY_DOUBLE) ? TY_DCPLX : TY_CPLX;
        f77_cx_d = re;
        f77_cx_i = (int)re;
    }
    else if (lx_t == T_NAME) {
        int s;
        char nm[F77_MAX_NAME];
        strcpy(nm, lex_name);
        f77_tok();
        if (nm[1] == 0 && (nm[0] == 'Z' || nm[0] == 'O') && lx_t == T_SCON) {
            if (neg) { f77_error("a sign cannot precede a hexadecimal or octal constant"); neg = 0; }
            if (!f77_bitconst(lex_strpool + lex_str_off[lex_sidx], lex_slen, nm[0] == 'Z' ? 16 : 8))
                f77_error(nm[0] == 'Z' ? "bad hexadecimal constant" : "bad octal constant");
            f77_tok();
            return;
        }
        s = f77_sym(nm);
        if (!f77_sparam[s]) { f77_error("DATA value must be a constant"); f77_cx_ty = TY_INT; f77_cx_i = 0; f77_cx_d = 0.0; return; }
        f77_cx_ty = f77_sty[s];
        f77_cx_i = f77_spival[s];
        f77_cx_d = f77_spdval[s];
        f77_cx_d2 = f77_spdval2[s];
        if (f77_cx_ty == TY_CHAR) { f77_cx_sidx = f77_spstr[s]; f77_cx_i = f77_slen[s]; }
    } else {
        f77_error("DATA value must be a constant");
        f77_cx_ty = TY_INT; f77_cx_i = 0; f77_cx_d = 0.0;
        return;
    }
    if (neg) { f77_cx_i = 0 - f77_cx_i; f77_cx_d = 0.0 - f77_cx_d; }
}

#define F77_MAX_DTGT 1024
static int f77_dt_sym[F77_MAX_DTGT];
static int f77_dt_start[F77_MAX_DTGT];   /* element index, or byte offset for a substring */
static int f77_dt_count[F77_MAX_DTGT];   /* elements, or 1 */
static int f77_dt_sublen[F77_MAX_DTGT];  /* substring length, 0 = not a substring */
static int f77_dt_n;

/* Constant bindings of implied-DO variables while a DATA target list
 * is expanded; f77_cprimary consults them before PARAMETERs. */
static int f77_implied_do_spans(int open, int *close_off, int *eq_off, int *items_off, int *ctl_off);

/* One DATA target: name, name(subscripts), name(e1:e2), or an
 * implied-DO (items, var = c1, c2 [, c3]) expanded here at compile time.
 * `stop` is the statement offset where this list ends (the comma before
 * an enclosing implied-DO's control, or the statement end). Returns 0
 * on error. */
static int f77_data_target(int stop) {
    int s;
    int k;
    if (lx_t == T_LP) {
        int open_off; int close_off; int eq_off; int items_off; int ctl_off;
        int c1; int c2; int c3; int v;
        int bi;
        open_off = (int)(lx_rts - lx_stmt);
        if (!f77_implied_do_spans(open_off, &close_off, &eq_off, &items_off, &ctl_off)) {
            f77_error("bad parenthesised DATA item");
            return 0;
        }
        f77_scan_from(ctl_off);
        if (lx_t != T_NAME) { f77_error("implied-DO in DATA needs a variable"); return 0; }
        if (f77_cxb_n >= F77_MAX_CXB) { f77_error("implied-DO in DATA nested too deep"); return 0; }
        bi = f77_cxb_n;
        strcpy(f77_cxb_name[bi], lex_name);
        f77_tok();
        if (lx_t != T_ASSIGN) { f77_error("expected = in implied-DO"); return 0; }
        f77_tok();
        f77_cexpr(); c1 = f77_cx_i;
        if (lx_t != T_COMMA) { f77_error("expected , in implied-DO"); return 0; }
        f77_tok();
        f77_cexpr(); c2 = f77_cx_i;
        c3 = 1;
        if (lx_t == T_COMMA) { f77_tok(); f77_cexpr(); c3 = f77_cx_i; }
        if (c3 == 0) { f77_error("zero step in implied-DO"); return 0; }
        f77_cxb_n = bi + 1;
        for (v = c1; c3 > 0 ? v <= c2 : v >= c2; v = v + c3) {
            f77_cxb_val[bi] = v;
            f77_scan_from(items_off);
            for (;;) {
                if (!f77_data_target(ctl_off - 1)) { f77_cxb_n = bi; return 0; }
                if (lx_t != T_COMMA || (int)(lx_rts - lx_stmt) >= ctl_off - 1) break;
                f77_tok();
                if ((int)(lx_rts - lx_stmt) >= ctl_off) break;
            }
        }
        f77_cxb_n = bi;
        f77_scan_from(close_off);
        return 1;
    }
    if (lx_t != T_NAME) { f77_error("expected a variable in DATA"); return 0; }
    s = f77_sym(lex_name);
    if (f77_sarg[s]) { f77_error("a dummy argument cannot appear in DATA"); return 0; }
    if (f77_sparam[s]) { f77_error("a PARAMETER cannot appear in DATA"); return 0; }
    f77_tok();
    if (f77_dt_n >= F77_MAX_DTGT) { f77_error("too many DATA items"); return 0; }
    f77_dt_sym[f77_dt_n] = s;
    f77_dt_sublen[f77_dt_n] = 0;
    if (lx_t == T_LP && f77_srank[s] > 0) {
        /* Constant subscripts -> one element. */
        int elem;
        int stride;
        int d;
        elem = 0;
        stride = 1;
        d = 0;
        f77_tok();
        for (;;) {
            if (f77_adjustable(s)) { f77_error("adjustable array in DATA"); return 0; }
            f77_cexpr();
            if (f77_cx_ty != TY_INT) { f77_error("DATA subscript must be an integer constant"); return 0; }
            if (d >= f77_srank[s]) { f77_error("too many DATA subscripts"); return 0; }
            if (f77_cx_i < f77_slo[s][d] ||
                f77_cx_i >= f77_slo[s][d] + f77_sext[s][d]) {
                f77_error("DATA subscript out of bounds");
                return 0;
            }
            elem = elem + (f77_cx_i - f77_slo[s][d]) * stride;
            stride = stride * f77_sext[s][d];
            d = d + 1;
            if (lx_t != T_COMMA) break;
            f77_tok();
        }
        if (lx_t != T_RP) { f77_error("expected ) after DATA subscripts"); return 0; }
        if (d != f77_srank[s]) { f77_error("wrong number of DATA subscripts"); return 0; }
        f77_tok();
        f77_dt_start[f77_dt_n] = elem;
        f77_dt_count[f77_dt_n] = 1;
    } else if (f77_srank[s] > 0) {
        if (f77_adjustable(s)) { f77_error("adjustable array in DATA"); return 0; }
        f77_dt_start[f77_dt_n] = 0;
        f77_dt_count[f77_dt_n] = f77_sym_nelem(s);
    } else {
        f77_dt_start[f77_dt_n] = 0;
        f77_dt_count[f77_dt_n] = 1;
    }
    if (lx_t == T_LP && f77_sty[s] == TY_CHAR) {
        /* (e1:e2) substring of the element or scalar: a byte range */
        int e1; int e2;
        f77_tok();
        e1 = 1; e2 = f77_slen[s];
        if (lx_t != T_COLON) { f77_cexpr(); e1 = f77_cx_i; }
        if (lx_t != T_COLON) { f77_error("expected : in DATA substring"); return 0; }
        f77_tok();
        if (lx_t != T_RP) { f77_cexpr(); e2 = f77_cx_i; }
        if (lx_t != T_RP) { f77_error("expected ) after DATA substring"); return 0; }
        f77_tok();
        f77_dt_start[f77_dt_n] = f77_dt_start[f77_dt_n] * f77_esize(s) + (e1 - 1);
        f77_dt_count[f77_dt_n] = 1;
        f77_dt_sublen[f77_dt_n] = e2 - e1 + 1;
    }
    if (!f77_scom[s]) f77_ssaved[s] = 1;   /* DATA implies static storage */
    f77_dt_n = f77_dt_n + 1;
    (void)stop;
    return 1;
}

/* The value list of DATA (or of a type statement's /c/), from the first
 * value up to the closing /, poured across f77_dt_* in order.  Returns 0
 * on error. */
static int f77_data_values(void) {
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
                return 0;
            }
            rep = f77_cx_i;
            f77_tok();
            f77_data_const();
        }
        while (rep > 0) {
            while (ti < f77_dt_n && te >= f77_dt_count[ti]) { ti = ti + 1; te = 0; }
            if (ti >= f77_dt_n) { f77_error("more DATA values than variables"); return 0; }
            if (f77_di_n >= F77_MAX_DINIT) { f77_error("too many DATA initializers"); return 0; }
            f77_di_sym[f77_di_n] = f77_dt_sym[ti];
            f77_di_elem[f77_di_n] = f77_dt_sublen[ti] ? f77_dt_start[ti] : f77_dt_start[ti] + te;
            f77_di_ty[f77_di_n] = f77_cx_ty;
            f77_di_i[f77_di_n] = f77_cx_i;
            f77_di_d[f77_di_n] = f77_cx_d;
            f77_di_sidx[f77_di_n] = f77_cx_sidx;
            f77_di_d2[f77_di_n] = f77_cx_d2;
            f77_di_u[f77_di_n] = f77_cx_u;
            f77_di_sub[f77_di_n] = f77_dt_sublen[ti];
            f77_di_n = f77_di_n + 1;
            te = te + 1;
            rep = rep - 1;
        }
        if (lx_t == T_COMMA) { f77_tok(); continue; }
        break;
    }
    while (ti < f77_dt_n && te >= f77_dt_count[ti]) { ti = ti + 1; te = 0; }
    if (ti < f77_dt_n) { f77_error("fewer DATA values than variables"); return 0; }
    return 1;
}

/* INTEGER K/3/, A(2)/1,2/ -- initial values in a type statement
 * (IBM's FORTRAN IV and VS FORTRAN; found by fort370), the same as DATA for the whole
 * entity; lx_t is at the opening /.  They had been skipped, and with
 * them everything after in the statement.  Returns 0 on error. */
/* INTEGER :: K = 3 (Fortran 90): an initial value, as DATA gives one -- static
 * storage, and for an array the value in every element (a scalar
 * initializer broadcasts).  lx_t at the '='.  An array constructor is
 * milestone 3's.  Returns 0 on error. */
static int f77_decl_init_eq(int s) {
    int k;
    int n;
    if (f77_sarg[s]) { f77_error("a dummy argument cannot be given an initial value"); return 0; }
    if (f77_srank[s] > 0 && f77_adjustable(s)) { f77_error("adjustable array given an initial value"); return 0; }
    f77_tok();
    if (lx_t == T_LP && lx_rp < lx_rpe && *lx_rp == '/') {
        f77_error("an array constructor (/ ... /) is not supported yet");
        return 0;
    }
    f77_cxb_n = 0;
    f77_cexpr();
    if (!f77_scom[s]) f77_ssaved[s] = 1;   /* an initial value implies SAVE (F90 5.1) */
    n = f77_srank[s] > 0 ? f77_sym_nelem(s) : 1;
    for (k = 0; k < n; k++) {
        if (f77_di_n >= F77_MAX_DINIT) { f77_error("too many DATA initializers"); return 0; }
        f77_di_sym[f77_di_n] = s;
        f77_di_elem[f77_di_n] = k;
        f77_di_ty[f77_di_n] = f77_cx_ty;
        f77_di_i[f77_di_n] = f77_cx_i;
        f77_di_d[f77_di_n] = f77_cx_d;
        f77_di_sidx[f77_di_n] = f77_cx_sidx;
        f77_di_d2[f77_di_n] = f77_cx_d2;
        f77_di_u[f77_di_n] = f77_cx_u;
        f77_di_sub[f77_di_n] = 0;
        f77_di_n = f77_di_n + 1;
    }
    return 1;
}

static int f77_decl_init(int s) {
    if (f77_sarg[s]) { f77_error("a dummy argument cannot be given an initial value"); return 0; }
    if (f77_srank[s] > 0 && f77_adjustable(s)) { f77_error("adjustable array given an initial value"); return 0; }
    f77_dt_n = 1;
    f77_cxb_n = 0;
    f77_dt_sym[0] = s;
    f77_dt_start[0] = 0;
    f77_dt_count[0] = f77_srank[s] > 0 ? f77_sym_nelem(s) : 1;
    f77_dt_sublen[0] = 0;
    if (!f77_scom[s]) f77_ssaved[s] = 1;   /* DATA implies static storage */
    f77_tok();
    if (!f77_data_values()) return 0;
    if (lx_t != T_SLASH) { f77_error("expected / after initial values"); return 0; }
    f77_tok();
    return 1;
}


static void f77_stmt_data(int skip) {
    int s;
    int k;

    if (!f77_decls_open) {
        f77_error("DATA after the first executable statement");
        return;
    }
    f77_scan_from(skip);
    for (;;) {
        /* --- the variable list, up to the opening / --- */
        f77_dt_n = 0;
        f77_cxb_n = 0;
        for (;;) {
            if (!f77_data_target(lx_stmt_len)) return;
            if (lx_t != T_COMMA) break;
            f77_tok();
        }
        if (lx_t != T_SLASH) { f77_error("expected / before DATA values"); return; }
        f77_tok();

        if (!f77_data_values()) return;
        if (lx_t != T_SLASH) { f77_error("expected / after DATA values"); return; }
        f77_tok();
        if (lx_t == T_COMMA) f77_tok();
        if (lx_t == T_EOF) break;
    }
}

/* EQUIVALENCE (a, b(2), c(1,2)), (d, e) ... -- record the pairs.  An
 * array element with constant subscripts (or a single subscript into a
 * multi-dimensional array, which F77 allows here) gives a byte offset;
 * a bare array name means its first element. */
static int f77_eq_offset(int s) {
    int elem;
    int stride;
    int d;
    int n;
    int subs[F77_MAX_RANK];
    if (lx_t != T_LP) return 0;
    if (f77_srank[s] == 0 && f77_sty[s] == TY_CHAR) {
        /* (e1:e2) on a scalar: the offset is e1-1 */
        int e1;
        f77_tok();
        e1 = 1;
        if (lx_t != T_COLON) { f77_cexpr(); e1 = f77_cx_i; }
        if (lx_t != T_COLON) { f77_error("expected : in EQUIVALENCE substring"); return 0; }
        f77_tok();
        if (lx_t != T_RP) f77_cexpr();
        if (lx_t != T_RP) { f77_error("expected ) in EQUIVALENCE"); return 0; }
        f77_tok();
        return e1 - 1;
    }
    if (f77_srank[s] == 0) { f77_error("subscript on a scalar in EQUIVALENCE"); return 0; }
    f77_tok();
    n = 0;
    for (;;) {
        f77_cexpr();
        if (f77_cx_ty != TY_INT) { f77_error("EQUIVALENCE subscript must be an integer constant"); return 0; }
        if (n < F77_MAX_RANK) subs[n] = f77_cx_i;
        n = n + 1;
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) { f77_error("expected ) in EQUIVALENCE"); return 0; }
    f77_tok();
    if (f77_adjustable(s)) { f77_error("adjustable array in EQUIVALENCE"); return 0; }
    if (n == 1 && f77_srank[s] > 1) {
        elem = subs[0] - 1;                 /* linear element number */
    } else {
        if (n != f77_srank[s]) { f77_error("wrong number of EQUIVALENCE subscripts"); return 0; }
        elem = 0;
        stride = 1;
        for (d = 0; d < n; d++) {
            elem = elem + (subs[d] - f77_slo[s][d]) * stride;
            stride = stride * f77_sext[s][d];
        }
    }
    return elem * f77_esize(s);
}

static void f77_stmt_equivalence(int skip) {
    int sa;
    int oa;
    int sb;
    int ob;
    if (!f77_decls_open) {
        f77_error("EQUIVALENCE after the first executable statement");
        return;
    }
    f77_scan_from(skip);
    for (;;) {
        if (lx_t != T_LP) { f77_error("EQUIVALENCE needs a parenthesised list"); return; }
        f77_tok();
        if (lx_t != T_NAME) { f77_error("expected a variable in EQUIVALENCE"); return; }
        sa = f77_sym(lex_name);
        f77_tok();
        oa = f77_eq_offset(sa);
        if (lx_t != T_COMMA) { f77_error("EQUIVALENCE needs at least two items"); return; }
        while (lx_t == T_COMMA) {
            f77_tok();
            if (lx_t != T_NAME) { f77_error("expected a variable in EQUIVALENCE"); return; }
            sb = f77_sym(lex_name);
            f77_tok();
            ob = f77_eq_offset(sb);
            if (f77_sarg[sa] || f77_sarg[sb]) { f77_error("a dummy argument cannot be in EQUIVALENCE"); return; }
            if (f77_sparam[sa] || f77_sparam[sb]) { f77_error("a PARAMETER cannot be in EQUIVALENCE"); return; }
            if (f77_eq_n >= F77_MAX_EQ) { f77_error("too many EQUIVALENCE pairs"); return; }
            f77_eq_sa[f77_eq_n] = sa; f77_eq_oa[f77_eq_n] = oa;
            f77_eq_sb[f77_eq_n] = sb; f77_eq_ob[f77_eq_n] = ob;
            f77_eq_n = f77_eq_n + 1;
        }
        if (lx_t != T_RP) { f77_error("expected ) in EQUIVALENCE"); return; }
        f77_tok();
        if (lx_t == T_COMMA) f77_tok();
        if (lx_t == T_EOF) break;
    }
}

/* Union-find with offsets: addr(s) = addr(root) + *off. */
static int f77_eq_find(int s, int *off) {
    int o;
    o = 0;
    while (f77_eq_parent[s] != s) {
        o = o + f77_eq_delta[s];
        s = f77_eq_parent[s];
    }
    *off = o;
    return s;
}

/* The static object `prefix`<unit>_<name> of the unit being compiled,
 * made on first use.  A subprogram and its ENTRY points are one
 * subprogram with one set of statics, so they are named after the
 * parent, and an ENTRY compiled after it finds the object already
 * there (each had had its own: a SAVEd K set through one entry was
 * not the K the other read). */
static int f77_static_obj(char *prefix, char *name, int size, char **out) {
    int fam;
    int g;
    char *nm;
    fam = f77_cur_unit;
    if (fam >= 0 && f77_uentry_parent[fam] >= 0) fam = f77_uentry_parent[fam];
    nm = f77_gpool_name(prefix, f77_uname[fam], "_", name);
    for (g = 0; g < ps_nglobals; g++) {
        if (strcmp(ps_gname[g], nm) == 0) {
            if (size > ps_gsize[g]) ps_gsize[g] = size;
            *out = ps_gname[g];
            return g;
        }
    }
    *out = nm;
    return f77_g_add(nm, size);
}

/* Rebind symbol `s` to static storage at `name`+`off`.  The GADDR (and
 * ADDI) land in whatever block is current -- the declaration part, so
 * the entry block, which dominates every use. */
static void f77_bind_static(int s, char *name, int off) {
    int ga;
    int old;
    int j;
    old = f77_sval[s];
    ga = hi_emit(HI_GADDR, HL_ADDR_TY, -1, -1, 0, name);
    if (off != 0) ga = hi_emit(HI_ADDI, HL_ADDR_TY, ga, -1, off, NULL);
    f77_sval[s] = ga;
    f77_shi[s] = -1;    /* doubles live contiguously in static storage */
    /* A reference made before the binding -- a COMMON member in a
     * dimension bound of the declaration part, INTEGER A(N:M) with M
     * in COMMON -- read the symbol's provisional local slot.  It reads
     * the static now.  (SN713 of FCVS FM711: the extent came from an
     * unset local, which SSA then made nothing at all.) */
    if (old >= 0 && h_kind[old] == HI_ALLOCA) {
        for (j = 0; j < h_ninst; j++) {
            int k;
            k = h_kind[j];
            if (k == HI_NOP || j == ga) continue;
            if (h_src1[j] == old) h_src1[j] = ga;
            if (h_src2[j] == old && ho_src2_is_ref(k)) h_src2[j] = ga;
            if ((k == HI_CALL || k == HI_CALLP) && h_cbase[j] >= 0) {
                int q;
                for (q = 0; q < h_val[j]; q++) if (h_carg[h_cbase[j] + q] == old) h_carg[h_cbase[j] + q] = ga;
            }
        }
    }
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
            /* EXTERNAL and INTRINSIC name procedures: a subroutine has no
             * type, and an intrinsic has its own.  An EXTERNAL used as a
             * function needs one, and is checked where it is referenced
             * (f77_need_type).  Refusing them here refused LAPACK's
             * `EXTERNAL XERBLA` and `INTRINSIC ABS` under IMPLICIT NONE. */
            if (f77_sexternal[s] || f77_sintrinsic[s]) continue;
            {
                static char msg[F77_MAX_NAME + 40];
                strcpy(msg, f77_sname[s]);
                strcat(msg, " is not declared (IMPLICIT NONE)");
                f77_error(msg);
            }
            return;
        }
    }

    for (s = f77_scope_base; s < f77_nsym; s++) {
        if (f77_sty[s] == TY_CHAR && f77_slen[s] < 0 && !f77_sarg[s] && !f77_sparam[s]) {
            static char msg[F77_MAX_NAME + 48];
            strcpy(msg, f77_sname[s]);
            strcat(msg, ": CHARACTER*(*) is only for dummy arguments and PARAMETERs");
            f77_error(msg);
            return;
        }
    }

    /* Hidden lengths: one trailing INTEGER per CHARACTER dummy, in
     * dummy order; an assumed-length dummy reads its length from it. */
    {
        int nh;
        nh = 0;
        for (i = 0; i < f77_ci_n; i++) {
            s = f77_ci_sym[i];
            if (f77_sty[s] != TY_CHAR) continue;
            if (f77_slen[s] < 0)
                f77_slenv[s] = hi_emit(HI_PARAM, TY_INT, -1, -1, f77_unit_nparams + nh, NULL);
            nh = nh + 1;
        }
        if (nh > 0) f77_assign_params(f77_unit_nparams + nh);
    }

    for (i = 0; i < f77_ncblk; i++) boff[i] = 0;

    for (i = 0; i < f77_cm_n; i++) {
        s = f77_cm_sym[i];
        blk = f77_cm_blk[i];
        if (f77_adjustable(s)) {
            f77_error("adjustable dimensions cannot appear in COMMON");
            return;
        }
        f77_bind_static(s, f77_cblk_name[blk], boff[blk]);
        f77_sgidx[s] = f77_cblk_gidx[blk];
        f77_sgoff[s] = boff[blk];
        if (f77_sty[s] == TY_CHAR && f77_slen[s] < 0) {
            f77_error("an assumed-length CHARACTER cannot be in COMMON");
            return;
        }
        sz = f77_esize(s) * f77_sym_nelem(s);
        boff[blk] = boff[blk] + sz;
    }

    /* EQUIVALENCE groups.  Resolve the pairs, then place each group:
     * on the COMMON block of a member that has one (the block grows
     * to cover the group, and may not be extended backwards), else on
     * a static object of the group's own. */
    if (f77_eq_n > 0) {
        int a; int b; int ra; int rb; int oa; int ob;
        for (s = f77_scope_base; s < f77_nsym; s++) { f77_eq_parent[s] = s; f77_eq_delta[s] = 0; }
        for (i = 0; i < f77_eq_n; i++) {
            a = f77_eq_sa[i]; b = f77_eq_sb[i];
            ra = f77_eq_find(a, &oa);
            rb = f77_eq_find(b, &ob);
            /* addr(b) = addr(a) + oa_i - ob_i */
            if (ra == rb) {
                if (oa + f77_eq_oa[i] - f77_eq_ob[i] != ob) {
                    f77_error("inconsistent EQUIVALENCE");
                    return;
                }
                continue;
            }
            f77_eq_parent[rb] = ra;
            f77_eq_delta[rb] = oa + f77_eq_oa[i] - f77_eq_ob[i] - ob;
        }
        for (i = f77_scope_base; i < f77_nsym; i++) {
            int root; int o; int minoff; int maxend; int csym; int coff; int g; int nm_s;
            char *nm;
            if (f77_eq_parent[i] != i) continue;          /* not a root */
            /* Gather the group's extent and any COMMON member. */
            minoff = 0; maxend = 0; csym = -1; coff = 0; nm_s = 0;
            for (s = f77_scope_base; s < f77_nsym; s++) {
                root = f77_eq_find(s, &o);
                if (root != i) continue;
                nm_s = nm_s + 1;
                if (o < minoff) minoff = o;
                sz = f77_esize(s) * f77_sym_nelem(s);
                if (o + sz > maxend) maxend = o + sz;
                if (f77_scom[s]) {
                    if (csym >= 0 && f77_sgidx[csym] != f77_sgidx[s]) {
                        f77_error("EQUIVALENCE joins two COMMON blocks");
                        return;
                    }
                    if (csym < 0 || f77_sgoff[s] - o < coff) { csym = s; coff = f77_sgoff[s] - o; }
                }
            }
            if (nm_s < 2) continue;                        /* a lone symbol */
            if (csym >= 0) {
                /* coff: block offset of the group origin (offset 0). */
                g = f77_sgidx[csym];
                if (coff + minoff < 0) {
                    f77_error("EQUIVALENCE extends a COMMON block backwards");
                    return;
                }
                for (s = f77_scope_base; s < f77_nsym; s++) {
                    root = f77_eq_find(s, &o);
                    if (root != i) continue;
                    if (f77_scom[s]) {
                        if (f77_sgoff[s] != coff + o) { f77_error("inconsistent EQUIVALENCE in COMMON"); return; }
                        continue;
                    }
                    f77_bind_static(s, ps_gname[g], coff + o);
                    f77_sgidx[s] = g;
                    f77_sgoff[s] = coff + o;
                }
                for (k = 0; k < f77_ncblk; k++) {
                    if (f77_cblk_gidx[k] == g && coff + maxend > boff[k]) boff[k] = coff + maxend;
                }
            } else {
                g = f77_static_obj("f77e_", f77_sname[i], maxend - minoff, &nm);
                for (s = f77_scope_base; s < f77_nsym; s++) {
                    root = f77_eq_find(s, &o);
                    if (root != i) continue;
                    f77_bind_static(s, nm, o - minoff);
                    f77_sgidx[s] = g;
                    f77_sgoff[s] = o - minoff;
                }
            }
        }
    }

    for (i = 0; i < f77_ncblk; i++) {
        if (boff[i] > ps_gsize[f77_cblk_gidx[i]])
            ps_gsize[f77_cblk_gidx[i]] = boff[i];
    }

    if (f77_save_all) {
        for (s = f77_scope_base; s < f77_nsym; s++) f77_ssaved[s] = 1;
    }
    for (i = 0; i < f77_sv_n; i++) f77_ssaved[f77_sv_sym[i]] = 1;
    /* A main program's arrays are static, SAVEd or not, as FORTRAN H
     * kept them: the unit runs once and is never inlined, so static
     * storage is indistinguishable from a frame.  On the stack,
     * INTEGER A(30000) (120 KB) ran past the linker's 64 KB default
     * and faulted on its first store; f77_realloc_sym gives it only a
     * placeholder word.  (A subprogram's arrays stay on the stack: one with a
     * static could not be spliced in, f77_unosplice.) */
    if (f77_ukind[f77_cur_unit] == F77_UNIT_PROGRAM) {
        for (s = f77_scope_base; s < f77_nsym; s++) {
            if (f77_srank[s] > 0 && !f77_sarg[s] && !f77_scom[s])
                f77_ssaved[s] = 1;
        }
    }

    for (s = f77_scope_base; s < f77_nsym; s++) {
        if (!f77_ssaved[s]) continue;
        if (f77_sarg[s] || f77_scom[s] || s == f77_result_sym) continue;
        if (f77_sgidx[s] >= 0) continue;          /* placed by EQUIVALENCE */
        if (f77_adjustable(s)) {
            f77_error("adjustable dimensions cannot be SAVEd");
            return;
        }
        {
            char *nm;
            f77_sgidx[s] = f77_static_obj("f77s_", f77_sname[s],
                                          f77_storage_bytes(s), &nm);
            f77_bind_static(s, nm, 0);
        }
    }

    /* An ALLOCATABLE is in no EQUIVALENCE and has no DATA (F90 5.5.1,
     * 5.2.10): its slot is a descriptor, which either would read as the
     * array.  It starts unallocated: a static one is zero already, a frame
     * one is zeroed here, in the entry block. */
    for (i = 0; i < f77_eq_n; i++) {
        if (f77_salloc[f77_eq_sa[i]] || f77_salloc[f77_eq_sb[i]]) {
            f77_error("an ALLOCATABLE array cannot be in an EQUIVALENCE");
            return;
        }
    }
    for (i = 0; i < f77_di_n; i++) {
        if (f77_di_sym[i] >= f77_scope_base && f77_salloc[f77_di_sym[i]]) {
            f77_error("an ALLOCATABLE array cannot be given DATA");
            return;
        }
    }
    for (s = f77_scope_base; s < f77_nsym; s++) {
        if (!f77_salloc[s] || f77_ssaved[s] || f77_sarg[s]) continue;
        if (f77_scom[s]) { f77_error("an ALLOCATABLE array cannot be in COMMON"); return; }
        hi_emit(HI_STORE, TY_INT, f77_desc_at(s, 0), f77_iconst(0), 0, NULL);
    }
    /* automatic arrays: their storage, by the extents just computed */
    for (s = f77_scope_base; s < f77_nsym; s++) {
        int n;
        int k;
        int args[3];
        if (!f77_sauto[s]) continue;
        if (f77_sarg[s] || f77_salloc[s]) { f77_sauto[s] = 0; continue; }
        if (f77_scom[s] || f77_ssaved[s] || f77_sgidx[s] >= 0) {
            f77_error("an automatic array cannot be in COMMON, SAVEd, or in an EQUIVALENCE");
            return;
        }
        if (f77_inline_depth > 0) { f77_error("an automatic array in a spliced unit (an f77 defect)"); return; }
        n = f77_iconst(f77_esize(s));
        for (k = 0; k < f77_srank[s]; k++) {
            int e;
            e = f77_sextv[s][k] >= 0 ? f77_sextv[s][k] : f77_iconst(f77_sext[s][k]);
            e = f77_select(hi_emit(HI_SGT, TY_INT, e, f77_iconst(0), 0, NULL), e, f77_iconst(0));
            n = hi_emit(HI_MUL, TY_INT, n, e, 0, NULL);
        }
        args[0] = f77_iconst(0); args[1] = n; args[2] = f77_iconst(0);
        f77_sval[s] = f77_call_i("f77_allocate", 3, args);
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
        esz = f77_esize(s);
        off = ps_ginit_start[g] + f77_sgoff[s] + f77_di_elem[i] * esz;
        if (f77_di_sub[i] > 0) {
            off = ps_ginit_start[g] + f77_sgoff[s] + f77_di_elem[i];
            esz = f77_di_sub[i];
        }
        if (f77_di_ty[i] == TY_CHAR) {
            /* A character constant: into a CHARACTER item padded or
             * truncated to its length; into a numeric item (Hollerith
             * data) left-justified, blank-filled. */
            int n;
            n = f77_di_i[i];
            for (k = 0; k < esz; k++)
                ps_ginit_pool[off + k] = (k < n) ?
                    (unsigned char)lex_strpool[lex_str_off[f77_di_sidx[i]] + k] : ' ';
        } else if (f77_sty[s] == TY_CHAR) {
            f77_error("a CHARACTER item needs a character constant in DATA");
            return;
        } else if (f77_di_ty[i] == TY_HEXC) {
            /* the constant's bits, right-justified in the item: zeros
             * on the left of one too short, the leftmost bits of one
             * too long lost (MIL-STD-1753 2.8) */
            if (f77_sty[s] == TY_CPLX || f77_sty[s] == TY_DCPLX) {
                f77_error("a hexadecimal or octal constant cannot initialize a COMPLEX item");
                return;
            }
            for (k = 0; k < esz; k++)
                ps_ginit_pool[off + k] = k < 8 ? (unsigned char)((f77_di_u[i] >> (k * 8)) & 255) : 0;
        } else if (f77_sty[s] == TY_CPLX) {
            union { float f; unsigned char b[4]; } u;
            u.f = (f77_di_ty[i] == TY_INT) ? (float)f77_di_i[i] : (float)f77_di_d[i];
            for (k = 0; k < 4; k++) ps_ginit_pool[off + k] = u.b[k];
            u.f = f77_iscx(f77_di_ty[i]) ? (float)f77_di_d2[i] : 0.0f;
            for (k = 0; k < 4; k++) ps_ginit_pool[off + 4 + k] = u.b[k];
        } else if (f77_sty[s] == TY_DCPLX) {
            /* each part as its constant's type makes it (f77_cx_part) */
            union { double d; unsigned char b[8]; } u;
            u.d = (f77_di_ty[i] == TY_INT) ? (double)f77_di_i[i] : f77_cx_part(f77_di_d[i], f77_di_ty[i]);
            for (k = 0; k < 8; k++) ps_ginit_pool[off + k] = u.b[k];
            u.d = f77_iscx(f77_di_ty[i]) ? f77_di_d2[i] : 0.0;
            for (k = 0; k < 8; k++) ps_ginit_pool[off + 8 + k] = u.b[k];
        } else if (f77_sty[s] == TY_DOUBLE) {
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
            /* an INTEGER*2 or LOGICAL*1 image is its width: the low bytes,
             * which come first (fort370 issue 10) */
            for (k = 0; k < (f77_snar[s] ? f77_snar[s] : 4); k++)
                ps_ginit_pool[off + k] = (unsigned char)((v >> (k * 8)) & 255);
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
    /* fort370: without a label the loop ends at END DO (MIL-STD-1753,
     * 1978), its label -1, which no statement carries */
    lab = -1;
    if (lx_t == T_ICON) {
        lab = lex_ival;
        f77_tok();
        if (lx_t == T_COMMA) f77_tok();      /* DO 20, I = ... is accepted */
    }
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
        /* Through the symbol's own store: a DOUBLE DO variable lives
         * in split lo/hi slots, and a hand-rolled ADDI+4 store left
         * the promoted hi slot stale (FM719 test 14 summed garbage). */
        f77_store_sym_val(s, m1, vty, m1hi);

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
    f77_alloca_add(trip_alloca, 0 - f77_frame);
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
    ctl_cont[ctl_n] = -1;
    ctl_n = ctl_n + 1;
}

/* Close the innermost DO: the step and the trip count, or for a DO
 * WHILE (ctl_var -1) back to its test. */
static void f77_close_top_do(void) {
    int s;
    int v;
    int t;
    {
        ctl_n = ctl_n - 1;
        s = ctl_var[ctl_n];
        if (s < 0) {
            if (f77_cur_blk_live) hi_emit(HI_BR, TY_VOID, -1, -1, ctl_test[ctl_n], NULL);
            f77_cur_blk_live = 0;
            f77_begin_blk(ctl_exit[ctl_n]);
            return;
        }
        if (ctl_cont[ctl_n] >= 0) {         /* a CYCLE came here: the step is its block */
            f77_goto_blk(ctl_cont[ctl_n]);
            f77_begin_blk(ctl_cont[ctl_n]);
        }
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
                vlo = f77_load_sym(s);
                vhi = ex_hi;
                shi = ctl_stephi[ctl_n];
                vlo = f77_fp64_call2("__fp64_add", vlo, vhi,
                                     ctl_step[ctl_n], shi, &vhi);
                f77_store_sym_val(s, vlo, TY_DOUBLE, vhi);
            } else if (f77_snar[s]) {         /* INTEGER*2 I: DO 10 I = ... */
                v = f77_load_at(f77_sval[s], f77_mty(s));
                v = hi_emit(HI_ADD, TY_INT, v, ctl_step[ctl_n], 0, NULL);
                f77_store_at(f77_sval[s], f77_mty(s), v, TY_INT, -1);
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

/* Close every DO whose terminal label is the one just executed. */
static char *f77_name_tail(int off);
static void f77_cname_match(int k, char *nm, char *what);
static void f77_close_do(int lab) {
    while (ctl_n > f77_ctl_base && ctl_kind[ctl_n - 1] == F77_CTL_DO &&
           ctl_label[ctl_n - 1] == lab) {
        /* A named DO ends in END DO and its name, labelled or not
         * (gfortran: "requires matching ENDDO name"). */
        if (ctl_name[ctl_n - 1][0]) {
            char *nm;
            nm = f77_starts("ENDDO") ? f77_name_tail(5) : 0;
            f77_cname_match(ctl_n - 1, nm ? nm : "", "the end of a named DO");
        }
        f77_close_top_do();
    }
}

/* fort370: DO [label [,]] WHILE (e), MIL-STD-1753's: the test block
 * evaluates e and enters the body or leaves; the loop's end, its label
 * or END DO, goes back to the test.  forever: Fortran 90's DO [label]
 * with no control, whose test block goes straight to the body (b_exit is
 * reached by EXIT alone). */
static void f77_open_dowhile(int forever) {
    int lab;
    int b_test;
    int b_body;
    int b_exit;
    int c;
    f77_scan_from(2);
    lab = -1;
    if (lx_t == T_ICON) {
        lab = lex_ival;
        f77_tok();
        if (lx_t == T_COMMA) f77_tok();
    }
    if (!forever) f77_tok();                 /* WHILE */
    b_test = hir_new_block();
    b_body = hir_new_block();
    b_exit = hir_new_block();
    f77_goto_blk(b_test);
    f77_begin_blk(b_test);
    if (forever) hi_emit(HI_BR, TY_VOID, -1, -1, b_body, NULL);
    else {
        c = f77_expr();
        if (ex_ty != TY_INT || !ex_log) f77_error("DO WHILE needs a logical expression");
        hi_emit(HI_BRC, TY_VOID, c, b_body, b_exit, NULL);
    }
    f77_cur_blk_live = 0;
    f77_begin_blk(b_body);
    if (ctl_n >= F77_MAX_CTL) { f77_error("control nesting too deep"); return; }
    ctl_kind[ctl_n] = F77_CTL_DO;
    ctl_label[ctl_n] = lab;
    ctl_var[ctl_n] = -1;
    ctl_test[ctl_n] = b_test;
    ctl_body[ctl_n] = b_body;
    ctl_exit[ctl_n] = b_exit;
    ctl_cont[ctl_n] = -1;
    ctl_n = ctl_n + 1;
}

/* STAT= of ALLOCATE or DEALLOCATE: an INTEGER variable or element; lx_t
 * past the '='.  Its address, or -1. */
static int f77_stat_target(void) {
    int s;
    if (lx_t != T_NAME) { f77_error("STAT= needs an INTEGER variable"); return -1; }
    s = f77_sym(lex_name);
    if (f77_sty[s] != TY_INT || f77_slog[s] || f77_sparam[s]) { f77_error("STAT= needs an INTEGER variable"); return -1; }
    f77_tok();
    f77_sstored[s] = 1;
    if (lx_t == T_LP && f77_srank[s] > 0) { f77_tok(); return f77_subscript_addr(s); }
    if (f77_srank[s] > 0) { f77_error("STAT= needs a scalar"); return -1; }
    return f77_sval[s];
}

/* ALLOCATE (A(lo:hi, n), ... [, STAT=v]) and DEALLOCATE (A, ... [, STAT=v])
 * (Fortran 90).  Every bound is evaluated, in order, before any storage is
 * taken (STAT= may come last); a lower bound left out is 1.  ALLOCATE
 * writes the new bounds only where the storage was taken: one already
 * allocated, or out of memory, under STAT= keeps its old descriptor. */
#define F77_MAX_ALLOBJ 32
static void f77_stmt_allocate(int skip, int alloc) {
    int obj[F77_MAX_ALLOBJ];
    int lo[F77_MAX_ALLOBJ][F77_MAX_RANK];
    int ext[F77_MAX_ALLOBJ][F77_MAX_RANK];
    int nobj;
    int statp;
    int i;
    int k;
    f77_scan_from(skip);
    if (lx_t != T_LP) { f77_error(alloc ? "ALLOCATE needs (objects)" : "DEALLOCATE needs (objects)"); return; }
    f77_tok();
    nobj = 0;
    statp = -1;
    for (;;) {
        int s;
        if (lx_t != T_NAME) { f77_error("expected an ALLOCATABLE array"); return; }
        if (strcmp(lex_name, "STAT") == 0 && lx_rp < lx_rpe && *lx_rp == '=') {
            f77_tok();
            f77_tok();
            statp = f77_stat_target();
            if (statp < 0) return;
        } else {
            s = f77_sym_find(lex_name);
            if (s < 0 || !f77_salloc[s]) { f77_error("ALLOCATE and DEALLOCATE need ALLOCATABLE arrays"); return; }
            if (nobj >= F77_MAX_ALLOBJ) { f77_error("too many objects in one ALLOCATE"); return; }
            obj[nobj] = s;
            f77_tok();
            if (alloc) {
                if (lx_t != T_LP) { f77_error("ALLOCATE needs the array's bounds"); return; }
                f77_tok();
                for (k = 0; ; k++) {
                    int a;
                    int b;
                    if (k >= f77_srank[s]) { f77_error("wrong number of bounds in ALLOCATE"); return; }
                    a = f77_expr();
                    a = f77_cvt(a, &ex_hi, ex_ty, TY_INT);
                    if (lx_t == T_COLON) {
                        f77_tok();
                        b = f77_expr();
                        b = f77_cvt(b, &ex_hi, ex_ty, TY_INT);
                    } else {
                        b = a;
                        a = f77_iconst(1);
                    }
                    lo[nobj][k] = a;
                    /* the extent, MAX(hi - lo + 1, 0) */
                    b = hi_emit(HI_SUB, TY_INT, b, a, 0, NULL);
                    b = hi_emit(HI_ADDI, TY_INT, b, -1, 1, NULL);
                    ext[nobj][k] = f77_select(hi_emit(HI_SGT, TY_INT, b, f77_iconst(0), 0, NULL), b, f77_iconst(0));
                    if (lx_t != T_COMMA) break;
                    f77_tok();
                }
                if (k + 1 != f77_srank[s]) { f77_error("wrong number of bounds in ALLOCATE"); return; }
                if (lx_t != T_RP) { f77_error("expected ) after the bounds"); return; }
                f77_tok();
            }
            nobj = nobj + 1;
        }
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
    if (lx_t != T_RP) { f77_error("expected ) after the ALLOCATE list"); return; }
    f77_tok();
    if (lx_t != T_EOF) { f77_error("unexpected text after the statement"); return; }
    for (i = 0; i < nobj; i++) {
        int s;
        int args[3];
        int cur;
        int r;
        s = obj[i];
        f77_sstored[s] = 1;
        cur = f77_desc_base(s);
        args[0] = cur;
        if (alloc) {
            int n;
            int changed;
            n = ext[i][0];
            for (k = 1; k < f77_srank[s]; k++) n = hi_emit(HI_MUL, TY_INT, n, ext[i][k], 0, NULL);
            args[1] = hi_emit(HI_MUL, TY_INT, n, f77_iconst(f77_esize(s)), 0, NULL);
            args[2] = statp >= 0 ? statp : f77_iconst(0);
            r = f77_call_i("f77_allocate", 3, args);
            changed = hi_emit(HI_SNE, TY_INT, r, cur, 0, NULL);
            for (k = 0; k < f77_srank[s]; k++) {
                int ol;
                int oe;
                ol = f77_desc_load(s, 4 + 8 * k);
                oe = f77_desc_load(s, 8 + 8 * k);
                hi_emit(HI_STORE, TY_INT, f77_desc_at(s, 4 + 8 * k), f77_select(changed, lo[i][k], ol), 0, NULL);
                hi_emit(HI_STORE, TY_INT, f77_desc_at(s, 8 + 8 * k), f77_select(changed, ext[i][k], oe), 0, NULL);
            }
        } else {
            args[1] = statp >= 0 ? statp : f77_iconst(0);
            r = f77_call_i("f77_deallocate", 2, args);
        }
        hi_emit(HI_STORE, TY_INT, f77_desc_at(s, 0), r, 0, NULL);
    }
}

/* A RETURN frees the subprogram's allocated, unSAVEd ALLOCATABLE arrays
 * (Fortran 95's rule).  Its units are never spliced (f77_unosplice). */
static void f77_alloc_free_all(void) {
    int s;
    for (s = f77_scope_base; s < f77_nsym; s++) {
        int args[1];
        if (f77_sauto[s] && !f77_decls_open) {
            args[0] = f77_sval[s];
            f77_call_i("f77_dealloc_auto", 1, args);
            continue;
        }
        if (!f77_salloc[s] || f77_ssaved[s] || f77_sarg[s]) continue;
        args[0] = f77_desc_base(s);
        f77_call_i("f77_dealloc_auto", 1, args);
    }
}

/* The ALLOCATABLE guard: an ALLOCATABLE's slot is its descriptor, and code
 * that took it for the array's storage -- an element address, an actual
 * argument, a whole-array I/O item -- would address the descriptor.  Every
 * use of the slot must be f77_desc_at's ADDI; any other stops the unit. */
static void f77_desc_guard(void) {
    int s;
    int i;
    for (s = f77_scope_base; s < f77_nsym; s++) {
        int d;
        if (!f77_salloc[s]) continue;
        d = f77_sval[s];
        for (i = 0; i < h_ninst; i++) {
            int bad;
            if (h_kind[i] == HI_NOP) continue;
            bad = 0;
            if (h_kind[i] != HI_BR && h_kind[i] != HI_BRC && h_kind[i] != HI_JMPTAB && h_src2[i] == d) bad = 1;
            if (h_kind[i] != HI_BR && h_src1[i] == d && h_kind[i] != HI_ADDI) bad = 1;
            if ((h_kind[i] == HI_CALL || h_kind[i] == HI_CALLP) && h_cbase[i] >= 0) {
                int q;
                for (q = 0; q < h_val[i]; q++) if (h_carg[h_cbase[i] + q] == d) bad = 1;
            }
            if (bad) {
                char m[F77_MAX_NAME + 80];
                sprintf(m, "the ALLOCATABLE array %s is not supported in this context yet", f77_sname[s]);
                f77_error(m);
                return;
            }
        }
    }
}

/* EXIT and CYCLE (Fortran 90), of the innermost DO or of the DO the
 * construct name names: EXIT to the block after it, CYCLE to its step (a
 * counted DO; made here the first time, f77_close_top_do runs the step in
 * it) or its test (DO WHILE, DO forever).  EXIT from a named IF is
 * Fortran 2008's. */
static void f77_stmt_exit_cycle(int cycle) {
    int k;
    char *nm;
    static char m[120];
    nm = f77_name_tail(cycle ? 5 : 4);
    if (!nm) { f77_error(cycle ? "unexpected text after CYCLE" : "unexpected text after EXIT"); return; }
    k = ctl_n - 1;
    if (nm[0]) {
        while (k >= f77_ctl_base && strcmp(ctl_name[k], nm) != 0) k = k - 1;
        if (k < f77_ctl_base) {
            sprintf(m, "%s names %s, which is no construct around it", cycle ? "CYCLE" : "EXIT", nm);
            f77_error(m);
            return;
        }
        if (ctl_kind[k] != F77_CTL_DO) {
            sprintf(m, "%s names %s, an IF construct, not a DO%s", cycle ? "CYCLE" : "EXIT", nm,
                    cycle ? "" : " (EXIT from an IF is Fortran 2008)");
            f77_error(m);
            return;
        }
    } else {
        while (k >= f77_ctl_base && ctl_kind[k] != F77_CTL_DO) k = k - 1;
        if (k < f77_ctl_base) { f77_error(cycle ? "CYCLE outside a DO loop" : "EXIT outside a DO loop"); return; }
    }
    if (!cycle) { f77_goto_blk(ctl_exit[k]); return; }
    if (ctl_var[k] < 0) { f77_goto_blk(ctl_test[k]); return; }
    if (ctl_cont[k] < 0) ctl_cont[k] = hir_new_block();
    f77_goto_blk(ctl_cont[k]);
}

/* Is the statement DO [label [,]] WHILE (e)? */
static int f77_is_dowhile(void) {
    int i;
    if (!f77_starts("DO")) return 0;
    i = 2;
    while (i < lx_stmt_len && lx_isdigit(lx_stmt[i])) i = i + 1;
    if (i > 2 && i < lx_stmt_len && lx_stmt[i] == ',') i = i + 1;
    if (i + 6 > lx_stmt_len || strncmp(lx_stmt + i, "WHILE(", 6) != 0) return 0;
    return f77_match_paren(i + 5) == lx_stmt_len;
}

/* Is the statement DO [label], with no loop control (Fortran 90): a loop
 * left only by EXIT, GO TO, RETURN or STOP? */
static int f77_is_doforever(void) {
    int i;
    if (!f77_starts("DO")) return 0;
    i = 2;
    while (i < lx_stmt_len && lx_isdigit(lx_stmt[i])) i = i + 1;
    return i == lx_stmt_len;
}

/* The text from off to the statement's end, when it is empty or one name
 * (a construct name after END DO, END IF, ELSE, EXIT, CYCLE); else 0. */
static char *f77_name_tail(int off) {
    int i;
    if (off >= lx_stmt_len) return lx_stmt + lx_stmt_len;
    if (!lx_isalpha(lx_stmt[off])) return 0;
    for (i = off + 1; i < lx_stmt_len; i++)
        if (!lx_isalpha(lx_stmt[i]) && !lx_isdigit(lx_stmt[i]) && lx_stmt[i] != '_') return 0;
    return lx_stmt + off;
}

/* The name (nm, possibly "") that what closes or continues construct k
 * (what: "END DO", "END IF", "ELSE") carries against the construct's own:
 * the same name, or none on both. */
static void f77_cname_match(int k, char *nm, char *what) {
    static char m[200];
    if (strcmp(ctl_name[k], nm) == 0) return;
    if (!ctl_name[k][0]) sprintf(m, "%s names %s; the construct has no name", what, nm);
    else if (!nm[0]) sprintf(m, "%s needs the construct name %s", what, ctl_name[k]);
    else sprintf(m, "%s names %s; the construct is %s", what, nm, ctl_name[k]);
    f77_error(m);
}

/* A construct name on the statement now starting a construct: one per
 * construct in the unit, and no variable's. */
static void f77_cname_define(char *nm) {
    static char m[120];
    int i;
    if (f77_inline_depth > 0) return;
    if (f77_is_cname(nm)) { sprintf(m, "the construct name %s is used twice", nm); f77_error(m); return; }
    for (i = f77_scope_base; i < f77_nsym; i++)
        if (strcmp(f77_sname[i], nm) == 0) { sprintf(m, "%s is a variable; it cannot name a construct", nm); f77_error(m); return; }
    if (f77_ncname >= F77_MAX_CNAME) { f77_error("too many construct names"); return; }
    strcpy(f77_cnames[f77_ncname], nm);
    f77_ncname = f77_ncname + 1;
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

static int f77_ident_cont(int c) {
    return lx_isalpha(c) || (c >= '0' && c <= '9');
}

/* Length of the identifier at `off`, or 0. */
static int f77_name_at(int off) {
    int n;
    if (off >= lx_stmt_len || !lx_isalpha(lx_stmt[off])) return 0;
    n = 1;
    while (off + n < lx_stmt_len && f77_ident_cont(lx_stmt[off + n]))
        n = n + 1;
    return n;
}

/* True if a blank was squeezed out somewhere in [lo, hi).  Index 0 is
 * the start of the statement, not a squeezed blank. */
static int f77_has_brk(int lo, int hi) {
    int i;
    i = lo;
    if (i < 1) i = 1;
    while (i < hi && i < lx_stmt_len) {
        if (lx_brk[i]) return 1;
        i = i + 1;
    }
    return 0;
}

/* Match `kw` at `off` as a complete token: the letters match, and the
 * next character is not glued onto a longer identifier.  A squeezed
 * blank before that next character (lx_brk) counts as a delimiter, so
 * FUNCTION is complete in `INTEGER FUNCTION F` but not in
 * `INTEGER FUNCTIONAL`.  Returns the offset just past `kw`, or -1. */
static int f77_kw_at(int off, char *kw) {
    int i;
    int k;
    k = 0;
    i = off;
    while (kw[k] != 0) {
        if (i >= lx_stmt_len) return -1;
        if (lx_stmt[i] != kw[k]) return -1;
        k = k + 1;
        i = i + 1;
    }
    if (i < lx_stmt_len && f77_ident_cont(lx_stmt[i]) && lx_brk[i] == 0)
        return -1;
    return i;
}

/* If the statement starts with `kw` as a complete token, or glued
 * specifically to FUNCTION (the INTEGERFUNCTIONF spelling), return
 * strlen(kw); else 0. */
static int f77_type_before_function(char *kw) {
    int n;
    n = (int)strlen(kw);
    if (lx_stmt_len < n) return 0;
    if (strncmp(lx_stmt, kw, n) != 0) return 0;
    if (f77_kw_at(0, kw) >= 0) return n;
    if (lx_stmt_len >= n + 8 && strncmp(lx_stmt + n, "FUNCTION", 8) == 0)
        return n;
    return 0;
}

/* Offset just past `[type] FUNCTION` if this is a function header
 * (`name (` follows); else -1.  Sets *ty_out to the result type.
 *
 * Two spellings, one rule: FUNCTION must be a complete token after
 * the type-spec (gfortran tokenizes on remaining spaces, so
 * FUNCTIONAL stays one name), except the fully squeezed no-space
 * form INTEGERFUNCTIONF(X) which the blank-squeeze was chosen for.
 * A FUNCTION statement also requires the dummy-list parenthesis
 * (F77 8.5); INTEGER FUNCTION G without parens is not a header. */
static int f77_function_kw_end(int *ty_out) {
    int off;
    int fn;
    int n;
    int ty;

    ty = TY_INT;
    off = 0;
    f77_hdr_clen = 1;
    f77_hdr_log = 0;
    f77_hdr_len = 0;
    n = f77_type_before_function("DOUBLEPRECISION");
    if (n > 0) { off = n; ty = TY_DOUBLE; }
    else if ((n = f77_type_before_function("DOUBLECOMPLEX")) > 0) { off = n; ty = TY_DCPLX; }
    else if ((n = f77_type_before_function("CHARACTER")) > 0) {
        off = n;
        ty = TY_CHAR;
        if (off < lx_stmt_len && lx_stmt[off] == '*') {
            /* *n or *(*) by text; *(cexpr) through the scanner. */
            off = off + 1;
            if (off + 2 < lx_stmt_len && lx_stmt[off] == '(' && lx_stmt[off + 1] == '*' &&
                lx_stmt[off + 2] == ')') {
                f77_hdr_clen = -1;
                off = off + 3;
            } else if (off < lx_stmt_len && lx_isdigit(lx_stmt[off])) {
                f77_hdr_clen = 0;
                while (off < lx_stmt_len && lx_isdigit(lx_stmt[off])) {
                    f77_hdr_clen = f77_hdr_clen * 10 + (lx_stmt[off] - '0');
                    off = off + 1;
                }
            } else if (off < lx_stmt_len && lx_stmt[off] == '(') {
                /* Not evaluated here: this is also the test for "is it
                 * a header at all", run in phase 1 where a PARAMETER in
                 * the length is not yet known.  The caller evaluates
                 * once the statement is known to be a header. */
                int end;
                end = f77_match_paren(off);
                if (end < 0) return -1;
                f77_hdr_clen = -2;
                f77_hdr_clen_off = off;
                off = end;
            }
        }
    }
    else {
        n = f77_type_before_function("INTEGER");
        if (n > 0) { off = n; ty = TY_INT; }
        else {
            n = f77_type_before_function("LOGICAL");
            if (n > 0) { off = n; ty = TY_INT; f77_hdr_log = 1; }
            else if ((n = f77_type_before_function("COMPLEX")) > 0) { off = n; ty = TY_CPLX; }
            else {
                n = f77_type_before_function("REAL");
                if (n > 0) { off = n; ty = TY_FLOAT; }
            }
        }
    }

    /* A length after the type: REAL*8 FUNCTION F(X) */
    if (ty != TY_CHAR && off > 0 && off + 1 < lx_stmt_len && lx_stmt[off] == '*' && lx_isdigit(lx_stmt[off + 1])) {
        int ln;
        ln = 0;
        off = off + 1;
        while (off < lx_stmt_len && lx_isdigit(lx_stmt[off])) { ln = ln * 10 + (lx_stmt[off] - '0'); off = off + 1; }
        ty = f77_lenty(ty, ln, 0);
        f77_hdr_len = ln;
    }
    fn = f77_kw_at(off, "FUNCTION");
    if (fn < 0) {
        /* No-space spelling: only when no blank was squeezed in the
         * prefix through the name, so INTEGER FUNCTIONAL(N) (a blank
         * after INTEGER, FUNCTIONAL one word) is not INTEGER
         * FUNCTION AL(N). */
        if (off + 8 > lx_stmt_len) return -1;
        if (strncmp(lx_stmt + off, "FUNCTION", 8) != 0) return -1;
        n = f77_name_at(off + 8);
        if (n == 0) return -1;
        fn = off + 8;
        if (f77_has_brk(0, fn + n)) return -1;
    } else {
        n = f77_name_at(fn);
        if (n == 0) return -1;
    }
    /* fort370: the length after the name, CHARACTER FUNCTION F*10(X)
     * (F77 8.5) and IBM's INTEGER FUNCTION F*2(I) */
    if (fn + n + 1 < lx_stmt_len && lx_stmt[fn + n] == '*' && lx_isdigit(lx_stmt[fn + n + 1]) &&
        (ty != TY_CHAR || f77_hdr_clen == 1)) {
        int k;
        int ln;
        k = fn + n + 1;
        ln = 0;
        while (k < lx_stmt_len && lx_isdigit(lx_stmt[k])) { ln = ln * 10 + (lx_stmt[k] - '0'); k = k + 1; }
        if (k >= lx_stmt_len || lx_stmt[k] != '(' || f77_match_paren(k) != lx_stmt_len) return -1;
        if (ty == TY_CHAR) f77_hdr_clen = ln;
        else if (f77_hdr_len == 0) { ty = f77_lenty(ty, ln, 0); f77_hdr_len = ln; }
        *ty_out = ty;
        return fn;
    }
    /* F77 8.5: the dummy list in parens is required, and nothing
     * follows it.  FUNCTIONAL(1)=10 is an array assignment, not
     * FUNCTION AL with dummy 1. */
    if (fn + n >= lx_stmt_len || lx_stmt[fn + n] != '(') return -1;
    if (f77_match_paren(fn + n) != lx_stmt_len) return -1;
    *ty_out = ty;
    return fn;
}

/* After a FUNCTION header's name token: its length, F*2, if any. */
static void f77_hdr_skip_len(void) {
    if (lx_t != T_STAR) return;
    f77_tok();
    if (lx_t == T_ICON) f77_tok();
}

/* If this statement is a `[type] FUNCTION name(...)` header, return the
 * result type; otherwise -1.  Checked before the type-declaration
 * keywords, because `REAL FUNCTION F(X)` squeezes to REALFUNCTIONF(X)
 * and would otherwise parse as a REAL declaration. */
static int f77_unit_header_ty(void) {
    int ty;
    if (f77_function_kw_end(&ty) < 0) return -1;
    return ty;
}

/* Characters to skip to reach the function name on a header we have
 * already classified.  8 is the bare-FUNCTION fallback. */
static int f77_function_skip(void) {
    int ty;
    int n;
    n = f77_function_kw_end(&ty);
    if (n < 0) return 8;
    return n;
}

/* Emit the unit's return.  A FUNCTION returns the variable named after
 * it, which F77 code assigns to; a SUBROUTINE and the main PROGRAM
 * return an int (the exit status, for the PROGRAM). */
static int f77_ret_code = -1;    /* RETURN e: the alternate-return index */
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
    f77_alloc_free_all();
    if (f77_ukind[f77_cur_unit] == F77_UNIT_FUNC && f77_result_sym >= 0 &&
        f77_sty[f77_result_sym] == TY_DCPLX) {
        /* the result is in the caller's temporary already (hidden arg 0) */
        hi_emit(HI_RET, TY_INT, f77_iconst(0), -1, 0, NULL);
        f77_cur_blk_live = 0;
        return;
    }
    if (f77_ukind[f77_cur_unit] == F77_UNIT_FUNC && f77_result_sym >= 0) {
        v = f77_load_sym(f77_result_sym);
        if (ex_ty == TY_DOUBLE || ex_ty == TY_CPLX) {
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
    hi_emit(HI_RET, TY_INT, f77_ret_code >= 0 ? f77_ret_code : f77_iconst(0), -1, 0, NULL);
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
/* The runtime's by-reference version of a specific intrinsic, for
 * passing it as an actual argument (F77 15.9.3: a subset of Table 5). */
static char *f77_intrinsic_proc(char *nm) {
    static char *tab[] = {
        "IABS", "ABS", "DABS", "SQRT", "DSQRT", "EXP", "DEXP", "ALOG", "DLOG",
        "ALOG10", "DLOG10", "SIN", "DSIN", "COS", "DCOS", "TAN", "DTAN",
        "ASIN", "DASIN", "ACOS", "DACOS", "ATAN", "DATAN", "ATAN2", "DATAN2",
        "SINH", "DSINH", "COSH", "DCOSH", "TANH", "DTANH", "AINT", "DINT",
        "ANINT", "DNINT", "NINT", "IDNINT", "MOD", "AMOD", "DMOD", "SIGN",
        "ISIGN", "DSIGN", "DIM", "IDIM", "DDIM", "DPROD", "LEN", "INDEX",
        "AIMAG", "CABS", "CSQRT", "CEXP", "CLOG", "CSIN", "CCOS", "CONJG", 0 };
    static char buf[F77_MAX_NAME + 8];
    int i;
    for (i = 0; tab[i]; i++) {
        if (strcmp(tab[i], nm) == 0) {
            char *q;
            strcpy(buf, "f77i_");
            q = buf + 5;
            while (*nm) { *q++ = (char)(*nm >= 'A' && *nm <= 'Z' ? *nm - 'A' + 'a' : *nm); nm++; }
            *q = 0;
            return strdup(buf);
        }
    }
    return NULL;
}

/* Index of an existing symbol, or -1 (no creation). */
static int f77_sym_find(char *nm) {
    int i;
    for (i = f77_scope_base; i < f77_nsym; i++)
        if (strcmp(f77_sname[i], nm) == 0) return i;
    return -1;
}

/* NAME(...) as an actual argument is a value, not a variable, unless NAME
 * is an array (an element) or a CHARACTER variable (a substring): it is a
 * function reference -- a statement function, an intrinsic, an EXTERNAL or
 * implicitly external function, or a dummy procedure.  This used to answer
 * yes for statement functions and intrinsics only, so F(G(1)) with G
 * external passed G's address as a variable and left (1) unparsed
 * ("expected ) after arguments"): LAPACK's DLAPY2(DNRM2(...), DNRM2(...))
 * in dgeev and its kin.  A FUNCTION of this file is filtered out before
 * this is asked (f77_find_func). */
static int f77_actual_is_call(char *nm) {
    int s;
    int after;
    after = (int)(lx_rte - lx_stmt);
    if (after >= lx_stmt_len || lx_stmt[after] != '(') return 0;   /* a bare name */
    if (f77_stf_find(nm) >= 0) return 1;
    s = f77_sym_find(nm);
    if (s < 0) return 1;                  /* undeclared and not an array */
    if (f77_srank[s] > 0) return 0;       /* an array element */
    if (f77_sty[s] == TY_CHAR) return f77_sexternal[s];   /* a substring, or a CHARACTER function */
    return 1;
}

/* Heap temporaries an actual argument made (TRANSFER), freed after the
 * call that received them: f77_tfree_mark() before the actuals,
 * f77_tfree_to(mark) after the call. */
static int f77_tfree_list[64];
static int f77_ntfree;
static int f77_tfree_mark(void) { return f77_ntfree; }
static void f77_tfree_to(int mark) {
    int args[1];
    while (f77_ntfree > mark) {
        f77_ntfree = f77_ntfree - 1;
        args[0] = f77_tfree_list[f77_ntfree];
        f77_call_i("f77_dealloc_auto", 1, args);
    }
}

/* esz bytes from src to dst: words, or memcpy below a word. */
static void f77_copy_bytes(int dst, int src, int esz) {
    int o;
    if (esz % 4 != 0) {
        int args[3];
        args[0] = dst; args[1] = src; args[2] = f77_iconst(esz);
        f77_call_i("memcpy", 3, args);
        return;
    }
    for (o = 0; o < esz; o = o + 4) {
        int w;
        w = hi_emit(HI_LOAD, TY_INT, o ? hi_emit(HI_ADDI, HL_ADDR_TY, src, -1, o, NULL) : src, -1, 0, NULL);
        hi_emit(HI_STORE, TY_INT, o ? hi_emit(HI_ADDI, HL_ADDR_TY, dst, -1, o, NULL) : dst, w, 0, NULL);
    }
}

/* TRANSFER(SOURCE, MOLD [, SIZE]) as an actual argument (Fortran 90),
 * lx_t at the name: the bits of SOURCE as MOLD's type, a scalar when MOLD
 * is one and SIZE is absent, else an array of SIZE elements (or as many
 * as SOURCE fills).  The value is a heap temporary -- the callee's
 * changes to it reach nothing, as gfortran has it: LAPACK's xyyRFSX pass
 * TRANSFER(RWORK(1:2*N), (/ (ZERO,ZERO) /), N) beside RWORK itself -- and
 * the call frees it.  SOURCE is a scalar, a whole array or a section;
 * MOLD a scalar, an array or an array constructor, only its type read.
 * Bytes past SOURCE's are left undefined, as the standard leaves them. */
static int f77_transfer_actual(void) {
    int srcb;          /* SOURCE's bytes */
    int resb;          /* the result's */
    int msz;
    int marr;
    int tmp;
    int args[3];
    int n;
    f77_tok();                                   /* TRANSFER */
    f77_tok();                                   /* ( */
    /* SOURCE: copied below, once the temporary exists; parsed first, so
     * its value or its section's bounds are evaluated here. */
    {
        int s;
        int idx[F77_MAX_RANK]; int trip[F77_MAX_RANK];
        int lo[F77_MAX_RANK]; int hi[F77_MAX_RANK]; int st[F77_MAX_RANK];
        int ext[F77_MAX_RANK];
        int rank;
        int nt;
        int esz;
        int k;
        int sval;        /* a scalar SOURCE's address, or -1 */
        int after;
        s = -1;
        sval = -1;
        rank = 0;
        esz = 0;
        nt = 0;
        after = (int)(lx_rte - lx_stmt);
        if (lx_t == T_NAME && (s = f77_sym_find(lex_name)) >= 0 && f77_srank[s] > 0 && !f77_sparam[s] &&
            (after >= lx_stmt_len || lx_stmt[after] != '(' || f77_sty[s] != TY_CHAR)) {
            f77_tok();
            rank = f77_srank[s];
            if (f77_sty[s] == TY_CHAR) { f77_error("TRANSFER of CHARACTER is not supported yet"); return f77_iconst(0); }
            for (k = 0; k < F77_MAX_RANK; k++) idx[k] = -1;
            if (lx_t == T_LP && f77_paren_has_colon()) {
                f77_tok();
                nt = f77_section_subs(s, idx, trip, lo, hi, st);
                if (nt < 0) return f77_iconst(0);
            } else if (lx_t == T_LP) {
                f77_tok();
                sval = f77_subscript_addr(s);     /* one element */
                esz = f77_esize(s);
                rank = 0;
            } else {
                for (k = 0; k < rank; k++) {
                    trip[k] = 1;
                    f77_dim_bounds(s, k, &lo[k], &hi[k]);
                    if (hi[k] < 0) { f77_error("TRANSFER of an assumed-size array"); return f77_iconst(0); }
                    st[k] = f77_iconst(1);
                }
                nt = rank;
            }
            if (sval < 0) {
                esz = f77_esize(s);
                srcb = f77_iconst(esz);
                for (k = 0; k < rank; k++) {
                    int e;
                    if (!trip[k]) { ext[k] = f77_iconst(1); continue; }
                    e = hi_emit(HI_SUB, TY_INT, hi[k], lo[k], 0, NULL);
                    e = hi_emit(HI_ADD, TY_INT, e, st[k], 0, NULL);
                    e = hi_emit(HI_DIV, TY_INT, e, st[k], 0, NULL);
                    ext[k] = f77_select(hi_emit(HI_SGT, TY_INT, e, f77_iconst(0), 0, NULL), e, f77_iconst(0));
                    srcb = hi_emit(HI_MUL, TY_INT, srcb, ext[k], 0, NULL);
                }
            } else {
                srcb = f77_iconst(esz);
            }
        } else {
            int v; int vty; int vhi;
            v = f77_expr();
            vty = ex_ty; vhi = ex_hi;
            if (vty == TY_CHAR) { f77_error("TRANSFER of CHARACTER is not supported yet"); return f77_iconst(0); }
            esz = f77_tysize(vty);
            if (vty == TY_DCPLX) sval = f77_dcx_arg(v);
            else {
                f77_frame = f77_frame + esz;
                sval = hi_emit(HI_ALLOCA, f77_aty(vty), -1, -1, 0 - f77_frame, NULL);
                f77_alloca_add(sval, 0 - f77_frame);
                f77_store_at(sval, vty, v, vty, vhi);
            }
            srcb = f77_iconst(esz);
            s = -1;
        }
        if (lx_t != T_COMMA) { f77_error("TRANSFER needs a MOLD"); return f77_iconst(0); }
        f77_tok();
        /* MOLD: its type, and whether it is an array */
        marr = 0;
        after = (int)(lx_rte - lx_stmt);
        if (lx_t == T_LP && lx_rp < lx_rpe && *lx_rp == '/') {
            int ty0;
            f77_tok();                           /* ( */
            f77_tok();                           /* / */
            ty0 = -1;
            for (;;) {
                f77_expr();
                if (ty0 < 0) ty0 = ex_ty;
                else if (ex_ty != ty0) { f77_error("the values of an array constructor differ in type"); return f77_iconst(0); }
                if (lx_t != T_COMMA) break;
                f77_tok();
            }
            if (lx_t != T_SLASH) { f77_error("expected /) after an array constructor"); return f77_iconst(0); }
            f77_tok();
            if (lx_t != T_RP) { f77_error("expected /) after an array constructor"); return f77_iconst(0); }
            f77_tok();
            if (ty0 == TY_CHAR) { f77_error("TRANSFER to CHARACTER is not supported yet"); return f77_iconst(0); }
            msz = f77_tysize(ty0);
            marr = 1;
        } else {
            int ms;
            ms = lx_t == T_NAME ? f77_sym_find(lex_name) : -1;
            if (ms >= 0 && f77_srank[ms] > 0 && (after >= lx_stmt_len || lx_stmt[after] != '(')) {
                f77_tok();
                if (f77_sty[ms] == TY_CHAR) { f77_error("TRANSFER to CHARACTER is not supported yet"); return f77_iconst(0); }
                msz = f77_esize(ms);
                marr = 1;
            } else {
                f77_expr();
                if (ex_ty == TY_CHAR) { f77_error("TRANSFER to CHARACTER is not supported yet"); return f77_iconst(0); }
                msz = f77_tysize(ex_ty);
            }
        }
        if (lx_t == T_COMMA) {
            int v;
            f77_tok();
            if (lx_t == T_NAME && strcmp(lex_name, "SIZE") == 0 && lx_rp < lx_rpe && *lx_rp == '=') { f77_tok(); f77_tok(); }
            v = f77_expr();
            v = f77_cvt(v, &ex_hi, ex_ty, TY_INT);
            resb = hi_emit(HI_MUL, TY_INT, v, f77_iconst(msz), 0, NULL);
        } else if (marr) {
            /* as many elements as SOURCE fills, the last one partly */
            resb = hi_emit(HI_ADDI, TY_INT, srcb, -1, msz - 1, NULL);
            resb = hi_emit(HI_DIV, TY_INT, resb, f77_iconst(msz), 0, NULL);
            resb = hi_emit(HI_MUL, TY_INT, resb, f77_iconst(msz), 0, NULL);
        } else {
            resb = f77_iconst(msz);
        }
        if (lx_t != T_RP) { f77_error("expected ) after TRANSFER's arguments"); return f77_iconst(0); }
        f77_tok();
        /* the temporary holds the larger of the two, so the copy fits */
        n = f77_select(hi_emit(HI_SGT, TY_INT, srcb, resb, 0, NULL), srcb, resb);
        args[0] = f77_iconst(0); args[1] = n; args[2] = f77_iconst(0);
        tmp = f77_call_i("f77_allocate", 3, args);
        if (sval >= 0) {
            f77_copy_bytes(tmp, sval, esz);
        } else {
            /* SOURCE's elements in array element order, the first
             * subscript fastest */
            F77Loop lp[F77_MAX_RANK];
            int pos;
            int cur[F77_MAX_RANK];
            int p;
            pos = f77_int_temp();
            hi_emit(HI_STORE, TY_INT, pos, f77_iconst(0), 0, NULL);
            for (k = rank - 1; k >= 0; k--)
                if (trip[k]) f77_loop_begin(&lp[k], lo[k], hi[k], st[k]);
            for (k = 0; k < rank; k++)
                cur[k] = trip[k] ? hi_emit(HI_LOAD, TY_INT, lp[k].idx, -1, 0, NULL) : idx[k];
            p = hi_emit(HI_LOAD, TY_INT, pos, -1, 0, NULL);
            f77_copy_bytes(hi_emit(HI_ADD, HL_ADDR_TY, tmp, p, 0, NULL), f77_elem_addr(s, cur, rank), esz);
            hi_emit(HI_STORE, TY_INT, pos, hi_emit(HI_ADDI, TY_INT, p, -1, esz, NULL), 0, NULL);
            for (k = 0; k < rank; k++)
                if (trip[k]) f77_loop_end(&lp[k]);
        }
    }
    if (f77_ntfree >= 64) { f77_error("too many TRANSFER arguments in one statement"); return tmp; }
    f77_tfree_list[f77_ntfree] = tmp;
    f77_ntfree = f77_ntfree + 1;
    f77_act_len = -1;
    return tmp;
}

static int f77_actual_addr(void) {
    int s;
    int v;
    int vty;
    int vhi;
    int tmp;

    f77_act_ty = -1;

    if (lx_t == T_NAME && strcmp(lex_name, "TRANSFER") == 0 && lx_rp < lx_rpe && *lx_rp == '(' &&
        f77_find_unit(lex_name) < 0) {
        int ts;
        ts = f77_sym_find(lex_name);
        if (ts < 0 || !(f77_sarg[ts] || f77_sexternal[ts] || f77_srank[ts] > 0)) return f77_transfer_actual();
    }

    if (lx_t == T_NAME && f77_actual_is_simple()) {
        /* A bare name that is a procedure -- EXTERNAL, a unit of this
         * file, or INTRINSIC -- passes the procedure's address.  A name
         * the unit has typed, stored, shaped or received as a dummy is
         * its variable; so is a bare intrinsic name no INTRINSIC
         * statement covers (F77 8.8, 15.10): CALL SETOUT(IFIX) passes
         * the implicitly typed variable IFIX, which the callee may store
         * through.  Found by fort370 in NPSOL's LSADDS. */
        int after;
        int sf;
        after = (int)(lx_rte - lx_stmt);
        if (after >= lx_stmt_len || lx_stmt[after] != '(') {
            sf = f77_sym_find(lex_name);
            if (sf < 0 || (!f77_sstored[sf] && f77_srank[sf] == 0 &&
                           !f77_sarg[sf] && !f77_scom[sf] && !f77_sparam[sf])) {
                if ((sf >= 0 && f77_sexternal[sf]) || f77_find_unit(lex_name) >= 0) {
                    char *pn;
                    int pu;
                    /* by the name it links by: a USEd or hosted procedure's */
                    pu = f77_find_unit(lex_name);
                    if (sf >= 0 && f77_slink[sf]) pn = f77_slink[sf];
                    else if (pu >= 0) pn = f77_ulink[pu];
                    else pn = strdup(lex_name);
                    if (sf >= 0 && f77_sme[sf] >= 0 && f77_me_kind[f77_sme[sf]] == F77_ME_GENERIC) {
                        f77_error("a generic name cannot be an actual argument");
                    }
                    f77_tok();
                    f77_act_len = -1;
                    return hi_emit(HI_FADDR, HL_ADDR_TY, -1, -1, 0, pn);
                }
                if (sf >= 0 && f77_sintrinsic[sf]) {
                    char *rn;
                    rn = f77_intrinsic_proc(lex_name);
                    f77_tok();
                    f77_act_len = -1;
                    if (rn == NULL) { f77_error("this intrinsic cannot be passed as an argument"); return f77_iconst(0); }
                    return hi_emit(HI_FADDR, HL_ADDR_TY, -1, -1, 0, rn);
                }
            }
        }
    }

    if (lx_t == T_NAME && f77_find_func(lex_name) < 0 && f77_actual_is_simple() &&
        !f77_actual_is_call(lex_name)) {
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

        if (f77_sty[s] == TY_CHAR) {
            f77_tok();
            v = f77_char_ref(s);
            f77_act_len = ex_len;
            f77_act_ty = TY_CHAR;
            if (f77_srank[s] == 0) f77_sstored[s] = 1;
            return v;
        }
        f77_act_len = -1;
        if (!f77_sparam[s]) {
            f77_tok();
            if (lx_t == T_LP && f77_srank[s] > 0) {
                f77_tok();
                f77_act_ty = f77_sty[s];
                return f77_subscript_addr(s);
            }
            f77_act_ty = f77_sty[s] + (f77_srank[s] > 0 ? 16 : 0);
            if (f77_salloc[s]) {            /* an ALLOCATABLE: its storage */
                f77_sstored[s] = 1;
                return f77_desc_base(s);
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
    f77_act_ty = vty;
    if (vty == TY_CHAR) {
        /* A character value is already an address. */
        f77_act_len = ex_len;
        return v;
    }
    f77_act_len = -1;
    f77_frame = f77_frame + f77_tysize(vty);
    tmp = hi_emit(HI_ALLOCA, f77_aty(vty), -1, -1, 0 - f77_frame, NULL);
    f77_alloca_add(tmp, 0 - f77_frame);
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
    if (f77_urty[u] == TY_CHAR) return 0;
    if (f77_urty[u] == TY_DCPLX) return 0;     /* its result is the caller's temporary */
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
    /* And the rest as f77_sym_param has them: they were left as whatever
     * symbol last had this index, often one of an earlier unit's -- a
     * spliced dummy at a PARAMETER's old index could not be assigned. */
    f77_scom[s] = 0;
    f77_ssaved[s] = 0;
    f77_sgidx[s] = -1;
    f77_sgoff[s] = 0;
    f77_sparam[s] = 0;
    f77_styped[s] = 0;
    f77_slen[s] = 1;
    f77_slenv[s] = -1;
    f77_spstr[s] = -1;
    f77_sexternal[s] = 0;
    f77_sauto[s] = 0;
    f77_slink[s] = NULL;
    f77_sme[s] = -1;
    f77_salloc[s] = 0;
    f77_sintrinsic[s] = 0;
    f77_slog[s] = 0;
    f77_snar[s] = 0;
    if (f77_imp_ready && nm[0] >= 'A' && nm[0] <= 'Z') {
        if (f77_sty[s] == TY_CHAR) f77_slen[s] = f77_imp_len[nm[0] - 'A'];
        f77_slog[s] = f77_imp_log[nm[0] - 'A'];
        f77_snar[s] = f77_imp_nar[nm[0] - 'A'];
    }
    /* And taken as stored.  A dummy's loads are asserted loop-invariant
     * (f77_ldro_note) because nothing else may change it while its own
     * unit runs; inlined, the callee runs as part of the caller, whose
     * loop may be changing the actual -- the DO variable in
     * DO 10 K = ... CALL RAMP(K, V), where RAMP's V = FLOAT(3*K+1) was
     * hoisted out of the loop and every V was the first trip's. */
    f77_sstored[s] = 1;
    { int d; d = 0; while (d < F77_MAX_RANK) { f77_sextsym[s][d] = -1; f77_slov[s][d] = -1; d = d + 1; } }
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
    unsigned char save_brk[F77_MAX_STMT];
    int save_len;
    int save_label;
    int save_line;
    int save_stmt_line;
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
    int save_stf_n;
    int save_stf_pool;
    int save_imp_map[26];            /* the caller's IMPLICIT, which the */
    int save_imp_len[26];            /* callee does not share */
    int save_imp_log[26];
    int save_imp_nar[26];
    int save_imp_ready;
    int save_imp_none;
    int b_cont;
    int skip;
    int i;
    int rty;
    int result;

    /* Checkpoint everything the callee will disturb. */
    memcpy(save_stmt, lx_stmt, lx_stmt_len + 1);
    memcpy(save_brk, lx_brk, lx_stmt_len);
    save_len = lx_stmt_len;
    save_label = lx_stmt_label;
    save_line = lx_line;
    save_stmt_line = lx_stmt_line;
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
    save_stf_n = f77_stf_n;
    save_stf_pool = f77_stf_pool_n;

    /* The callee is typed by its own rules: a unit with an IMPLICIT is
     * never spliced (f77_unosplice), so its rule is the default one,
     * whatever the caller's IMPLICIT or IMPLICIT NONE says.  Under the
     * caller's, IMPLICIT INTEGER (A-Z) made the callee's Y = 2.5 store
     * 2, and IMPLICIT NONE refused its undeclared names.  From fort370. */
    memcpy(save_imp_map, f77_imp_map, sizeof save_imp_map);
    memcpy(save_imp_len, f77_imp_len, sizeof save_imp_len);
    memcpy(save_imp_log, f77_imp_log, sizeof save_imp_log);
    memcpy(save_imp_nar, f77_imp_nar, sizeof save_imp_nar);
    save_imp_ready = f77_imp_ready;
    save_imp_none = f77_imp_none;
    for (i = 0; i < 26; i++) {
        f77_imp_map[i] = (i >= 'I' - 'A' && i <= 'N' - 'A') ? TY_INT : TY_FLOAT;
        f77_imp_len[i] = 1;
        f77_imp_log[i] = 0;
        f77_imp_nar[i] = 0;
    }
    f77_imp_ready = 1;
    f77_imp_none = 0;

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
    else skip = f77_function_skip();
    f77_scan_from(skip);
    if (lx_t == T_NAME) f77_tok();
    f77_hdr_skip_len();
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
        f77_snar[result] = f77_urnar[u];
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

    /* Restore.  The callee's statement functions go with it: a caller
     * variable of the same name must not resolve to them. */
    f77_stf_n = save_stf_n;
    f77_stf_pool_n = save_stf_pool;
    memcpy(f77_imp_map, save_imp_map, sizeof save_imp_map);
    memcpy(f77_imp_len, save_imp_len, sizeof save_imp_len);
    memcpy(f77_imp_log, save_imp_log, sizeof save_imp_log);
    memcpy(f77_imp_nar, save_imp_nar, sizeof save_imp_nar);
    f77_imp_ready = save_imp_ready;
    f77_imp_none = save_imp_none;
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
    lx_stmt_line = save_stmt_line;   /* or the caller's next diagnostic names the callee's last line */
    memcpy(lx_stmt, save_stmt, save_len + 1);
    memcpy(lx_brk, save_brk, save_len);
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
static int f77_call_mk;

static void f77_stmt_call1(int skip) {
    char nm[F77_MAX_NAME];
    int addrs[64];
    int lens[64];
    int tys[64];
    char *cname;
    int alts[16];
    int nalt;
    int cb;
    int nargs;
    int nc;
    int r;
    int fptr;

    f77_scan_from(skip);
    if (lx_t != T_NAME) { f77_error("CALL needs a subroutine name"); return; }
    strcpy(nm, lex_name);
    f77_tok();

    nargs = 0;
    nalt = 0;
    if (lx_t == T_LP) {
        f77_tok();
        if (lx_t != T_RP) {
            for (;;) {
                if (lx_t == T_STAR) {
                    /* *label: an alternate return */
                    f77_tok();
                    if (lx_t != T_ICON) { f77_error("* needs a statement label"); return; }
                    if (nalt < 16) { alts[nalt] = lex_ival; nalt = nalt + 1; }
                    f77_tok();
                    if (lx_t != T_COMMA) break;
                    f77_tok();
                    continue;
                }
                /* Evaluated before any h_carg slot is reserved -- see
                 * the note in the FUNCTION path. */
                if (nargs < 64) { addrs[nargs] = f77_actual_addr(); lens[nargs] = f77_act_len; tys[nargs] = f77_act_ty; }
                else f77_actual_addr();
                nargs = nargs + 1;
                if (lx_t != T_COMMA) break;
                f77_tok();
            }
        }
        if (lx_t != T_RP) f77_error("expected ) after arguments");
        else f77_tok();
    }
    fptr = -1;
    {
        int ds;
        ds = f77_sym_find(nm);
        if (ds >= 0 && f77_sarg[ds] && f77_srank[ds] == 0) fptr = f77_sval[ds];   /* dummy procedure */
        /* MIL-STD-1753's MVBITS(M, I, LEN, N, J), in line unless a unit
         * of the file or EXTERNAL makes it the program's: LEN bits of M
         * from bit I into N from bit J.  The items are taken as
         * fullword INTEGERs. */
        /* Fortran 95's CPU_TIME(T): processor seconds, REAL or DOUBLE
         * PRECISION, unless the program has its own */
        if (strcmp(nm, "CPU_TIME") == 0 && fptr < 0 && nalt == 0 && f77_find_unit(nm) < 0 &&
            !(ds >= 0 && (f77_sexternal[ds] || f77_slink[ds]))) {
            int args[1];
            if (nargs != 1 || (tys[0] != TY_FLOAT && tys[0] != TY_DOUBLE)) {
                f77_error("CPU_TIME needs one REAL or DOUBLE PRECISION argument");
                return;
            }
            args[0] = addrs[0];
            f77_call_i(tys[0] == TY_FLOAT ? "f77_cpu_time_r" : "f77_cpu_time_d", 1, args);
            return;
        }
        if (strcmp(nm, "MVBITS") == 0 && fptr < 0 && nalt == 0 && f77_find_unit(nm) < 0 &&
            !(ds >= 0 && f77_sexternal[ds])) {
            int v[5];
            int k;
            int m;
            if (nargs != 5) { f77_error("MVBITS needs five arguments"); return; }
            for (k = 0; k < 5; k++) v[k] = hi_emit(HI_LOAD, TY_INT, addrs[k], -1, 0, NULL);
            m = f77_bitmask(v[2]);
            k = hi_emit(HI_SRL, TY_INT, v[0], v[1], 0, NULL);
            k = hi_emit(HI_AND, TY_INT, k, m, 0, NULL);
            k = hi_emit(HI_SLL, TY_INT, k, v[4], 0, NULL);
            m = hi_emit(HI_SLL, TY_INT, m, v[4], 0, NULL);
            m = hi_emit(HI_BNOT, TY_INT, m, -1, 0, NULL);
            m = hi_emit(HI_AND, TY_INT, v[3], m, 0, NULL);
            m = hi_emit(HI_OR, TY_INT, m, k, 0, NULL);
            hi_emit(HI_STORE, TY_INT, addrs[3], m, 0, NULL);
            return;
        }
    }
    if (fptr < 0 && nalt == 0) {
        int u;
        u = f77_find_unit(nm);
        if (u >= 0 && f77_can_inline(u)) {
            f77_inline_unit_body(u, addrs, nargs);
            return;
        }
    }
    cb = h_ncarg;
    nc = 0;
    {
        int ai;
        ai = 0;
        while (ai < nargs) {
            f77_carg(addrs[ai], 0);
            nc = nc + 1;
            ai = ai + 1;
        }
        for (ai = 0; ai < nargs; ai++) {
            if (lens[ai] < 0) continue;
            f77_carg(lens[ai], 0);
            nc = nc + 1;
        }
    }
    if (fptr >= 0) r = hi_emit(HI_CALLP, TY_INT, fptr, -1, nc, NULL);
    else {
        /* the name it links by: a hosted unit's, a USEd procedure's, the
         * specific a generic's actuals pick */
        int cu;
        int ds;
        cname = strdup(nm);
        cu = f77_find_unit(nm);
        ds = f77_sym_find(nm);
        if (cu >= 0) cname = f77_ulink[cu];
        else if (ds >= 0 && f77_sme[ds] >= 0) {
            int e;
            e = f77_sme[ds];
            if (f77_me_kind[e] == F77_ME_GENERIC) e = f77_generic_pick(e, tys, nargs > 64 ? 64 : nargs);
            if (e >= 0 && f77_me_pkind[e] != F77_UNIT_SUBR) { f77_error("a FUNCTION called as a SUBROUTINE"); e = -1; }
            if (e >= 0) cname = f77_me_str[e];
        } else if (ds >= 0 && f77_slink[ds]) cname = f77_slink[ds];
        r = hi_emit(HI_CALL, TY_INT, -1, -1, nc, cname);
    }
    h_cbase[r] = cb;
    f77_tfree_to(f77_call_mk);                /* before the alternate returns branch */
    if (nalt > 0) {
        /* RETURN k in the callee comes back as k: branch to the k-th
         * label, fall through on 0 or out of range. */
        int i;
        for (i = 0; i < nalt; i++) {
            int c; int b_next;
            c = hi_emit(HI_SEQ, TY_INT, r, f77_iconst(i + 1), 0, NULL);
            b_next = hir_new_block();
            hi_emit(HI_BRC, TY_VOID, c, f77_label_blk(alts[i]), b_next, NULL);
            f77_cur_blk_live = 0;
            f77_begin_blk(b_next);
        }
    }
}


/* CALL: the TRANSFER temporaries its actuals made are freed after it. */
static void f77_stmt_call(int skip) {
    int saved;
    saved = f77_call_mk;
    f77_call_mk = f77_tfree_mark();
    f77_stmt_call1(skip);
    f77_tfree_to(f77_call_mk);
    f77_call_mk = saved;
}

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
static int f77_decls_open;               /* still in the declaration part */

/* Compiling an ENTRY: the parent's whole text is compiled, and control
 * goes from the declarations straight to the ENTRY statement on
 * f77_entry_line.  A GO TO from after the ENTRY back into the text
 * before it is legal, and that text has to be there to go to.
 * 0 = not an ENTRY, 1 = branch not yet emitted, 2 = branched, the
 * parent's text before the ENTRY is being compiled, 3 = past it. */
static int f77_entry_state;
static int f77_entry_line;
static int f77_entry_blk;

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
            v = f77_load_at(a, f77_mty(s));
            hi = ex_hi;
            f77_sarg[s] = 0;
            f77_frame = f77_frame + 4;
            f77_sval[s] = hi_emit(HI_ALLOCA, TY_INT, -1, -1,
                                  0 - f77_frame, NULL);
            f77_alloca_add(f77_sval[s], 0 - f77_frame);
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
            f77_store_at(f77_ci_addr[i], f77_mty(s), v, f77_sty[s], ex_hi);
        }
        i = i + 1;
    }
}

static int f77_urty_of(int u) {
    if (f77_urpend[u]) {
        char m[160];
        sprintf(m, "the result kind of %s is not known where it is referenced", f77_uname[u]);
        f77_error(m);
    }
    return f77_urty[u];
}

/* Index of the FUNCTION unit called `nm`, or -1.  Phase 1 records every
 * unit in the file before any of them is compiled, so a call can be
 * resolved even when the callee appears later in the source. */
/* Any program unit with this name -- SUBROUTINE or FUNCTION. */
/* Does unit `from` see hosted unit u by name: is u's host `from` or one
 * of `from`'s hosts?  (A procedure sees its own host's procedures, and an
 * internal procedure its host's siblings.) */
static int f77_unit_sees(int from, int u) {
    int h;
    if (f77_uhost[u] < 0) return 1;
    for (h = from; h >= 0; h = f77_uhost[h])
        if (h == f77_uhost[u]) return 1;
    return 0;
}

static int f77_find_unit(char *nm) {
    int i;
    int found;
    found = -1;
    i = 0;
    while (i < f77_nunit) {
        if (f77_ukind[i] != F77_UNIT_PROGRAM && f77_ukind[i] != F77_UNIT_MODULE &&
            strcmp(f77_uname[i], nm) == 0 && f77_unit_sees(f77_cur_unit, i)) {
            /* the innermost: an internal procedure hides an external of its name */
            if (found < 0 || f77_uhost[i] >= 0) found = i;
        }
        i = i + 1;
    }
    return found;
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

/* --- modules (Fortran 90) ---------------------------------------------
 *
 * A module is a table of entities: its PARAMETERs, its procedures (link
 * name, result type, the dummies' types) and its generic interfaces.  A
 * module of this file fills it as it is compiled -- the specification
 * part at its CONTAINS or END (f77_mod_snapshot), each procedure at the
 * end of its own unit (f77_mod_proc) -- and f77_mod_write leaves it in
 * NAME.mod, in text, for USE in later compilations (f77_mod_read).
 * Module procedures see the table by host association, a USE by name.
 * What a module may not hold yet -- variables, interface bodies -- is
 * refused there. */
static int f77_mod_find(char *nm) {
    int i;
    for (i = 0; i < f77_nmod; i++) if (strcmp(f77_mod_name[i], nm) == 0) return i;
    return -1;
}
static int f77_mod_new(char *nm, int unit) {
    if (f77_nmod >= F77_MAX_MOD) { f77_error("too many modules"); return -1; }
    strcpy(f77_mod_name[f77_nmod], nm);
    f77_mod_unit[f77_nmod] = unit;
    f77_nmod = f77_nmod + 1;
    return f77_nmod - 1;
}
static int f77_me_new(int m, char *nm, int kind) {
    int e;
    if (f77_nme >= F77_MAX_MODENT) { f77_error("too many module entities"); return -1; }
    e = f77_nme;
    f77_nme = f77_nme + 1;
    f77_me_mod[e] = m;
    f77_me_home[e] = m;
    strcpy(f77_me_name[e], nm);
    f77_me_kind[e] = kind;
    f77_me_ty[e] = TY_INT; f77_me_log[e] = 0; f77_me_ival[e] = 0;
    f77_me_d[e] = 0.0; f77_me_d2[e] = 0.0; f77_me_str[e] = NULL;
    f77_me_pkind[e] = 0; f77_me_nargs[e] = 0;
    return e;
}
static int f77_me_find(int m, char *nm) {
    int e;
    for (e = 0; e < f77_nme; e++)
        if (f77_me_mod[e] == m && strcmp(f77_me_name[e], nm) == 0) return e;
    return -1;
}

/* The specification part of the module compiled as unit u, at its
 * CONTAINS or END: its PARAMETERs, and the generics its INTERFACE blocks
 * named. */
static void f77_mod_snapshot(int u) {
    int m;
    int s;
    int g;
    m = f77_mod_find(f77_uname[u]);
    if (m < 0) m = f77_mod_new(f77_uname[u], u);
    if (m < 0) return;
    for (s = f77_scope_base; s < f77_nsym; s++) {
        int e;
        if (!f77_sparam[s]) {
            if (f77_sexternal[s] || f77_sintrinsic[s]) continue;
            {
                char msg[160];
                sprintf(msg, "%s: a module variable is not supported yet", f77_sname[s]);
                f77_error(msg);
            }
            continue;
        }
        e = f77_me_new(m, f77_sname[s], F77_ME_PARAM);
        if (e < 0) return;
        f77_me_ty[e] = f77_sty[s];
        f77_me_log[e] = f77_slog[s];
        f77_me_ival[e] = f77_sty[s] == TY_CHAR ? f77_slen[s] : f77_spival[s];
        f77_me_d[e] = f77_spdval[s];
        f77_me_d2[e] = f77_spdval2[s];
        if (f77_sty[s] == TY_CHAR) {
            int k;
            k = f77_spstr[s];
            f77_me_str[e] = (char *)malloc((size_t)lex_str_len[k] + 1);
            memcpy(f77_me_str[e], lex_strpool + lex_str_off[k], (size_t)lex_str_len[k]);
            f77_me_str[e][lex_str_len[k]] = 0;
        }
    }
    for (g = 0; g < f77_gi_n; g++) {
        int e;
        e = f77_me_new(m, f77_gi_name[g], F77_ME_GENERIC);
        if (e < 0) return;
        f77_me_str[e] = strdup(f77_gi_spec[g]);
    }
    f77_gi_n = 0;
}

/* A module procedure, unit u, compiled: its entry in its module's
 * table.  The dummies are read from the header again, now that the
 * unit's declarations have typed them. */
static void f77_mod_proc(int u) {
    int m;
    int e;
    int n;
    m = f77_mod_find(f77_uname[f77_uhost[u]]);
    if (m < 0) return;
    e = f77_me_new(m, f77_uname[u], F77_ME_PROC);
    if (e < 0) return;
    f77_me_str[e] = strdup(f77_ulink[u]);
    f77_me_pkind[e] = f77_ukind[u];
    f77_me_ty[e] = f77_ukind[u] == F77_UNIT_FUNC ? f77_urty[u] : TY_INT;
    f77_me_log[e] = f77_urlog[u];
    if (f77_ukind[u] == F77_UNIT_FUNC && (f77_urty[u] == TY_CHAR || f77_urty[u] == TY_DCPLX)) {
        f77_error("a CHARACTER or COMPLEX*16 module FUNCTION is not supported yet");
        return;
    }
    lx_pos = f77_upos[u];
    lx_line = f77_uline[u];
    n = 0;
    if (f77_next_stmt()) {
        f77_scan_from(f77_ukind[u] == F77_UNIT_SUBR ? 10 : f77_function_skip());
        if (lx_t == T_NAME) f77_tok();
        if (lx_t == T_LP) {
            f77_tok();
            while (lx_t == T_NAME || lx_t == T_STAR) {
                int s;
                int t;
                t = -1;
                if (lx_t == T_NAME) {
                    s = f77_sym_find(lex_name);
                    if (s >= 0) t = f77_sty[s] + (f77_srank[s] > 0 ? 16 : 0);
                }
                if (n < 16) f77_me_argty[e][n] = t;
                n = n + 1;
                f77_tok();
                if (lx_t != T_COMMA) break;
                f77_tok();
            }
        }
    }
    f77_me_nargs[e] = n;
}

/* Entity e of a module under the name `local` in the current unit. */
static void f77_me_bind(int e, char *local) {
    int s;
    if (f77_me_kind[e] == F77_ME_PARAM) {
        s = f77_sym(local);
        f77_sparam[s] = 1;
        f77_sty[s] = f77_me_ty[e];
        f77_styped[s] = 1;
        f77_slog[s] = f77_me_log[e];
        f77_spival[s] = f77_me_ty[e] == TY_CHAR ? 0 : f77_me_ival[e];
        f77_spdval[s] = f77_me_d[e];
        f77_spdval2[s] = f77_me_d2[e];
        if (f77_me_ty[e] == TY_CHAR) {
            f77_slen[s] = f77_me_ival[e];
            f77_spstr[s] = f77_intern_str(f77_me_str[e], f77_me_ival[e]);
        }
        return;
    }
    if (f77_me_kind[e] == F77_ME_PROC) {
        s = f77_sym(local);
        f77_sexternal[s] = 1;
        f77_styped[s] = 1;
        f77_sty[s] = f77_me_ty[e];
        f77_slog[s] = f77_me_log[e];
        f77_slink[s] = f77_me_str[e];
        f77_sme[s] = e;
        return;
    }
    /* a generic: resolved where it is referenced */
    s = f77_sym(local);
    f77_sexternal[s] = 1;
    f77_styped[s] = 1;
    f77_sme[s] = e;
}

/* The whole of module m, as host association or a USE without ONLY
 * gives it. */
static void f77_mod_bind_all(int m) {
    int e;
    for (e = 0; e < f77_nme; e++)
        if (f77_me_mod[e] == m) f77_me_bind(e, f77_me_name[e]);
}

/* A host of internal procedures keeps, after it is compiled, its
 * PARAMETERs as a table of its own (named #u, a name no module has) and
 * the names of its other entities.  Its internal procedures get the
 * PARAMETERs; a name of the others they take implicitly would be host
 * association with a variable, which is refused, not made a local. */
static char  f77_host_keybuf[16];
static char *f77_host_key(int u) { sprintf(f77_host_keybuf, "#%d", u); return f77_host_keybuf; }
static char  f77_hostnames[F77_MAX_SYM][F77_MAX_NAME];
static int   f77_hostnames_n;
static char  f77_uhn_pool[F77_MAX_UNIT][4096];   /* a host's other names, space-separated */
static void f77_host_save(int u) {
    int m;
    int s;
    int k;
    m = f77_mod_find(f77_host_key(u));
    if (m < 0) m = f77_mod_new(f77_host_key(u), u);
    if (m < 0) return;
    k = 0;
    f77_uhn_pool[u][0] = 0;
    for (s = f77_scope_base; s < f77_nsym; s++) {
        int e;
        if (f77_sme[s] >= 0) {
            /* a procedure or generic the host has by USE: its internal
             * procedures have it too */
            int o;
            o = f77_sme[s];
            e = f77_me_new(m, f77_sname[s], f77_me_kind[o]);
            if (e < 0) return;
            f77_me_home[e] = f77_me_home[o];
            f77_me_ty[e] = f77_me_ty[o]; f77_me_log[e] = f77_me_log[o];
            f77_me_str[e] = f77_me_str[o]; f77_me_pkind[e] = f77_me_pkind[o];
            f77_me_nargs[e] = f77_me_nargs[o];
            memcpy(f77_me_argty[e], f77_me_argty[o], sizeof(f77_me_argty[o]));
            continue;
        }
        if (!f77_sparam[s]) {
            int n;
            n = (int)strlen(f77_sname[s]);
            if (k + n + 2 < 4096) { memcpy(f77_uhn_pool[u] + k, f77_sname[s], (size_t)n); k = k + n; f77_uhn_pool[u][k++] = ' '; f77_uhn_pool[u][k] = 0; }
            continue;
        }
        e = f77_me_new(m, f77_sname[s], F77_ME_PARAM);
        if (e < 0) return;
        f77_me_ty[e] = f77_sty[s];
        f77_me_log[e] = f77_slog[s];
        f77_me_ival[e] = f77_sty[s] == TY_CHAR ? f77_slen[s] : f77_spival[s];
        f77_me_d[e] = f77_spdval[s];
        f77_me_d2[e] = f77_spdval2[s];
        if (f77_sty[s] == TY_CHAR) {
            int q;
            q = f77_spstr[s];
            f77_me_str[e] = (char *)malloc((size_t)lex_str_len[q] + 1);
            memcpy(f77_me_str[e], lex_strpool + lex_str_off[q], (size_t)lex_str_len[q]);
            f77_me_str[e][lex_str_len[q]] = 0;
        }
    }
}
static void f77_hostnames_load(int h) {
    char *p;
    f77_hostnames_n = 0;
    p = f77_uhn_pool[h];
    while (*p) {
        int n;
        n = 0;
        while (p[n] && p[n] != ' ') n = n + 1;
        if (n > 0 && n < F77_MAX_NAME && f77_hostnames_n < F77_MAX_SYM) {
            memcpy(f77_hostnames[f77_hostnames_n], p, (size_t)n);
            f77_hostnames[f77_hostnames_n][n] = 0;
            f77_hostnames_n = f77_hostnames_n + 1;
        }
        p = p + n;
        while (*p == ' ') p = p + 1;
    }
}
/* At an internal procedure's end: an implicitly typed local named as a
 * host variable is host association, not supported yet. */
static void f77_hostnames_check(void) {
    int s;
    int k;
    for (s = f77_scope_base; s < f77_nsym; s++) {
        if (f77_styped[s] || f77_sarg[s] || f77_sparam[s] || f77_sexternal[s]) continue;
        if (s == f77_result_sym) continue;
        for (k = 0; k < f77_hostnames_n; k++) {
            if (strcmp(f77_hostnames[k], f77_sname[s]) == 0) {
                char m[160];
                sprintf(m, "%s: host association with a variable is not supported yet", f77_sname[s]);
                f77_error(m);
                break;
            }
        }
    }
}

/* --- USE and INTERFACE (Fortran 90) ------------------------------------ */

static char f77_mod_dir_buf[1024];

/* NAME.mod (lower case, as gfortran names it) from the current
 * directory: the table a compilation of the module left. */
static int f77_mod_read(char *nm) {
    char path[F77_MAX_NAME + 8];
    char *buf;
    char *p;
    int fd;
    int n;
    int cap;
    int m;
    int k;
    for (k = 0; nm[k]; k++) path[k] = (char)(nm[k] >= 'A' && nm[k] <= 'Z' ? nm[k] - 'A' + 'a' : nm[k]);
    strcpy(path + k, ".mod");
    fd = open(path, 0);
    if (fd < 0) return -1;
    cap = 1 << 20;
    buf = (char *)malloc((size_t)cap + 1);
    n = (int)read(fd, buf, (size_t)cap);
    close(fd);
    if (n < 0) n = 0;
    buf[n] = 0;
    if (strncmp(buf, "F77MOD 1 ", 9) != 0) { free(buf); f77_error("this .mod file is not f77's"); return -2; }
    m = f77_mod_new(nm, -1);
    if (m < 0) { free(buf); return -2; }
    p = strchr(buf, '\n');
    while (p && *p) {
        char kind;
        char name[F77_MAX_NAME];
        int e;
        p = p + 1;
        if (!*p) break;
        kind = *p;
        if (sscanf(p + 2, "%63s", name) != 1) break;
        if (kind == 'P') {
            int ty; int lg; int iv; int len;
            char d1[64]; char d2[64];
            char *q;
            e = f77_me_new(m, name, F77_ME_PARAM);
            if (e < 0) break;
            sscanf(p + 2, "%*s %d %d %d %63s %63s %d", &ty, &lg, &iv, d1, d2, &len);
            f77_me_ty[e] = ty; f77_me_log[e] = lg; f77_me_ival[e] = iv;
            f77_me_d[e] = strtod(d1, NULL); f77_me_d2[e] = strtod(d2, NULL);
            if (ty == TY_CHAR) {
                q = strchr(p, '|');
                f77_me_str[e] = (char *)malloc((size_t)len + 1);
                if (q) memcpy(f77_me_str[e], q + 1, (size_t)len);
                f77_me_str[e][len] = 0;
                p = q ? q + 1 + len : p;
            }
        } else if (kind == 'R') {
            int pk; int ty; int lg; int na; int a;
            char link[F77_MAX_LINK];
            char *q;
            e = f77_me_new(m, name, F77_ME_PROC);
            if (e < 0) break;
            sscanf(p + 2, "%*s %255s %d %d %d %d", link, &pk, &ty, &lg, &na);
            f77_me_str[e] = strdup(link);
            f77_me_pkind[e] = pk; f77_me_ty[e] = ty; f77_me_log[e] = lg; f77_me_nargs[e] = na;
            q = p + 2;
            for (a = 0; a < 6; a++) { while (*q && *q != ' ') q++; while (*q == ' ') q++; }
            for (a = 0; a < na && a < 16; a++) {
                f77_me_argty[e][a] = (int)strtol(q, &q, 10);
            }
        } else if (kind == 'G') {
            char *q;
            char *eol;
            e = f77_me_new(m, name, F77_ME_GENERIC);
            if (e < 0) break;
            q = p + 2 + strlen(name);
            while (*q == ' ') q++;
            eol = strchr(q, '\n');
            if (!eol) eol = q + strlen(q);
            f77_me_str[e] = (char *)malloc((size_t)(eol - q) + 1);
            memcpy(f77_me_str[e], q, (size_t)(eol - q));
            f77_me_str[e][eol - q] = 0;
        }
        p = strchr(p, '\n');
    }
    free(buf);
    return m;
}

/* Every module of this file into its NAME.mod, after a clean compilation. */
static void f77_mod_write(void) {
    int m;
    for (m = 0; m < f77_nmod; m++) {
        char path[F77_MAX_NAME + 8];
        FILE *f;
        int k;
        int e;
        if (f77_mod_unit[m] < 0 || f77_mod_name[m][0] == '#') continue;
        for (k = 0; f77_mod_name[m][k]; k++) {
            char c;
            c = f77_mod_name[m][k];
            path[k] = (char)(c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c);
        }
        strcpy(path + k, ".mod");
        f = fopen(path, "w");
        if (!f) { f77_error("cannot write a .mod file"); return; }
        fprintf(f, "F77MOD 1 %s\n", f77_mod_name[m]);
        for (e = 0; e < f77_nme; e++) {
            if (f77_me_mod[e] != m) continue;
            if (f77_me_kind[e] == F77_ME_PARAM) {
                fprintf(f, "P %s %d %d %d %a %a %d", f77_me_name[e], f77_me_ty[e], f77_me_log[e], f77_me_ival[e],
                        f77_me_d[e], f77_me_d2[e], f77_me_ty[e] == TY_CHAR ? f77_me_ival[e] : 0);
                if (f77_me_ty[e] == TY_CHAR) { fputc('|', f); fwrite(f77_me_str[e], 1, (size_t)f77_me_ival[e], f); }
                fputc('\n', f);
            } else if (f77_me_kind[e] == F77_ME_PROC) {
                int a;
                fprintf(f, "R %s %s %d %d %d %d", f77_me_name[e], f77_me_str[e], f77_me_pkind[e], f77_me_ty[e],
                        f77_me_log[e], f77_me_nargs[e]);
                for (a = 0; a < f77_me_nargs[e] && a < 16; a++) fprintf(f, " %d", f77_me_argty[e][a]);
                fputc('\n', f);
            } else {
                fprintf(f, "G %s %s\n", f77_me_name[e], f77_me_str[e]);
            }
        }
        fclose(f);
    }
}

/* USE name [, rename-list] | USE name, ONLY: [only-list] (Fortran 90),
 * each item local => name, or name.  A module of this file compiled
 * before the USE, else NAME.mod. */
static void f77_stmt_use(int skip) {
    char mname[F77_MAX_NAME];
    int m;
    int only;
    int e;
    char loc[64][F77_MAX_NAME];
    char rem[64][F77_MAX_NAME];
    int nr;
    int k;
    if (!f77_decls_open) { f77_error("USE after the first executable statement"); return; }
    f77_scan_from(skip);
    if (lx_t == T_COMMA || lx_t == T_COLON) {
        f77_error("USE, INTRINSIC and USE :: are Fortran 2003, not supported");
        return;
    }
    if (lx_t != T_NAME) { f77_error("USE needs a module name"); return; }
    strcpy(mname, lex_name);
    f77_tok();
    m = f77_mod_find(mname);
    if (m < 0) m = f77_mod_read(mname);
    if (m == -2) return;
    if (m < 0) {
        char msg[160];
        sprintf(msg, "no module %s: neither in this file before the USE, nor a .mod file of it here", mname);
        f77_error(msg);
        return;
    }
    only = 0;
    nr = 0;
    if (lx_t == T_COMMA) {
        f77_tok();
        if (lx_t == T_NAME && strcmp(lex_name, "ONLY") == 0 && lx_rp < lx_rpe && *lx_rp == ':') {
            only = 1;
            f77_tok();
            f77_tok();                           /* : */
        }
        while (lx_t == T_NAME) {
            if (nr >= 64) { f77_error("too many names in a USE"); return; }
            strcpy(loc[nr], lex_name);
            strcpy(rem[nr], lex_name);
            f77_tok();
            if (lx_t == T_EQ || lx_t == T_ASSIGN) {
                /* => : the lexer's = then > */
                if (lx_t == T_ASSIGN) f77_tok();
                if (lx_t != T_GT && lx_t != T_GE) { f77_error("expected => in a USE"); return; }
                f77_tok();
                if (lx_t != T_NAME) { f77_error("expected a name after =>"); return; }
                strcpy(rem[nr], lex_name);
                f77_tok();
            } else if (!only) { f77_error("expected => in a USE rename"); return; }
            nr = nr + 1;
            if (lx_t != T_COMMA) break;
            f77_tok();
        }
    }
    if (lx_t != T_EOF) { f77_error("unexpected text in a USE"); return; }
    for (k = 0; k < nr; k++) {
        e = f77_me_find(m, rem[k]);
        if (e < 0) {
            char msg[200];
            sprintf(msg, "module %s has no %s", mname, rem[k]);
            f77_error(msg);
            return;
        }
        f77_me_bind(e, loc[k]);
    }
    if (!only) {
        for (e = 0; e < f77_nme; e++) {
            int renamed;
            if (f77_me_mod[e] != m) continue;
            renamed = 0;
            for (k = 0; k < nr; k++) if (strcmp(rem[k], f77_me_name[e]) == 0) renamed = 1;
            if (!renamed) f77_me_bind(e, f77_me_name[e]);
        }
    }
}

/* INTERFACE: a generic name for module procedures,
 *   INTERFACE name / MODULE PROCEDURE a, b / END INTERFACE,
 * in a module.  Interface bodies, operators and assignment are refused.
 * f77_iface_open is the generic being read, -1 when none is. */
static int f77_iface_open = -1;
static void f77_stmt_interface(void) {
    f77_scan_from(9);
    if (f77_ukind[f77_cur_unit] != F77_UNIT_MODULE) { f77_error("an INTERFACE outside a module's specification part is not supported yet"); return; }
    if (lx_t != T_NAME) { f77_error("an INTERFACE block of interface bodies is not supported yet"); return; }
    if ((strcmp(lex_name, "OPERATOR") == 0 || strcmp(lex_name, "ASSIGNMENT") == 0) && lx_rp < lx_rpe && *lx_rp == '(') {
        f77_error("INTERFACE OPERATOR and ASSIGNMENT are not supported yet");
        return;
    }
    if (f77_gi_n >= 32) { f77_error("too many generic interfaces"); return; }
    strcpy(f77_gi_name[f77_gi_n], lex_name);
    f77_gi_spec[f77_gi_n][0] = 0;
    f77_iface_open = f77_gi_n;
    f77_gi_n = f77_gi_n + 1;
    f77_tok();
}
/* A statement inside an INTERFACE block: 1 when it was one. */
static int f77_iface_stmt(void) {
    int n;
    if (f77_iface_open < 0) return 0;
    if (lx_stmt_len >= 12 && strncmp(lx_stmt, "ENDINTERFACE", 12) == 0) {
        f77_scan_from(12);
        if (lx_t == T_NAME) {
            if (strcmp(lex_name, f77_gi_name[f77_iface_open]) != 0) f77_error("END INTERFACE names another interface");
            f77_tok();
        }
        if (f77_gi_spec[f77_iface_open][0] == 0) f77_error("a generic interface with no MODULE PROCEDURE");
        f77_iface_open = -1;
        return 1;
    }
    if ((n = f77_starts("MODULEPROCEDURE")) != 0) {
        f77_scan_from(n);
        while (lx_t == T_NAME) {
            char *sp;
            sp = f77_gi_spec[f77_iface_open];
            if (strlen(sp) + strlen(lex_name) + 2 >= 512) { f77_error("too many procedures in a generic"); return 1; }
            if (sp[0]) strcat(sp, " ");
            strcat(sp, lex_name);
            f77_tok();
            if (lx_t != T_COMMA) break;
            f77_tok();
        }
        return 1;
    }
    f77_error("an interface body is not supported yet");
    lx_t = T_EOF;
    return 1;
}

/* A reference to the generic of module entity g with these actuals'
 * types (f77_act_tys): the specific whose dummies match, by the entity
 * of its module; -1, reported, when none does. */
static int f77_generic_pick(int g, int *tys, int n) {
    char *p;
    int m;
    m = f77_me_home[g];
    p = f77_me_str[g];
    while (*p) {
        char nm[F77_MAX_NAME];
        int k;
        int e;
        k = 0;
        while (*p && *p != ' ' && k < F77_MAX_NAME - 1) nm[k++] = *p++;
        nm[k] = 0;
        while (*p == ' ') p++;
        e = f77_me_find(m, nm);
        if (e >= 0 && f77_me_kind[e] == F77_ME_PROC && f77_me_nargs[e] == n) {
            int a;
            for (a = 0; a < n && a < 16; a++) if (f77_me_argty[e][a] != tys[a]) break;
            if (a == n || a == 16) return e;
        }
    }
    {
        char msg[160];
        sprintf(msg, "no procedure of the generic %s takes these arguments", f77_me_name[g]);
        f77_error(msg);
    }
    return -1;
}

/* Record the unit's name from its header statement. */
static void f77_unit_name(int u) {
    int skip;
    if (f77_starts("SUBROUTINE")) skip = 10;
    else skip = f77_function_skip();
    f77_scan_from(skip);
    if (lx_t == T_NAME) strcpy(f77_uname[u], lex_name);
    else { f77_error("subprogram needs a name"); strcpy(f77_uname[u], "unnamed"); }
}

/* Resolve where `n` incoming arguments arrive.  Without this the
 * codegen's entry sequence is skipped entirely (it gates on h_val <
 * hl_param_nflat) and the argument registers are never moved into the
 * registers the allocator chose -- which silently "works" only when
 * the allocator happens to pick the ABI register.  Every Fortran
 * argument is one 32-bit word (an address, or a hidden CHARACTER
 * length), so every tag is 0.  Called again from f77_layout_static
 * when the declarations turn out to add hidden lengths: a later
 * argument never moves an earlier one, so re-running is safe. */
static void f77_assign_params(int n) {
    int i;
    int ord;
    hl_param_room(n);    /* the tables grow (hir.h); their tags all 0 */
    hl_param_nflat = n;
    hl_nparams = n;
    hi_abi_assign(hl_param_tags, hl_param_nflat, hl_param_map);
    ord = 0;
    i = 0;
    while (i < hl_param_nflat) {
        if (hl_param_map[i] < 0) { hl_param_stkord[i] = ord; ord = ord + 4; }
        else hl_param_stkord[i] = -1;
        i = i + 1;
    }
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
    f77_stf_n = 0;
    f77_stf_pool_n = 0;
    f77_stfb_n = 0;
    f77_eq_n = 0;

    /* Reset the implicit-typing map to the default rule. */
    {
        int c;
        for (c = 0; c < 26; c++) {
            f77_imp_map[c] = (c >= 'I' - 'A' && c <= 'N' - 'A') ? TY_INT : TY_FLOAT;
            f77_imp_len[c] = 1;
            f77_imp_log[c] = 0;
            f77_imp_nar[c] = 0;
        }
    }
    f77_imp_ready = 1;
    f77_imp_none = 0;
    f77_hostnames_n = 0;
    if (f77_uhost[u] >= 0) {
        /* a hosted procedure (Fortran 90): its host's implicit typing,
         * and what host association gives it -- a module's entities, an
         * internal procedure its host's PARAMETERs (and the names of the
         * host's variables, which it may not use yet) */
        int h;
        int m;
        h = f77_uhost[u];
        memcpy(f77_imp_map, f77_uimp_map[h], sizeof(f77_imp_map));
        memcpy(f77_imp_len, f77_uimp_len[h], sizeof(f77_imp_len));
        memcpy(f77_imp_log, f77_uimp_log[h], sizeof(f77_imp_log));
        memcpy(f77_imp_nar, f77_uimp_nar[h], sizeof(f77_imp_nar));
        f77_imp_none = f77_uimp_none[h];
        m = f77_mod_find(f77_ukind[h] == F77_UNIT_MODULE ? f77_uname[h] : f77_host_key(h));
        if (m >= 0) f77_mod_bind_all(m);
        if (f77_ukind[h] != F77_UNIT_MODULE) f77_hostnames_load(h);
    }

    if (f77_ukind[u] == F77_UNIT_PROGRAM || f77_ukind[u] == F77_UNIT_BDATA || f77_ukind[u] == F77_UNIT_MODULE) {
        hl_param_nflat = 0;
        hl_nparams = 0;
        return;
    }

    /* Skip past the keyword and the unit name to the argument list. */
    skip = 0;
    if (f77_uentry_parent[u] >= 0) skip = 5;                   /* ENTRY */
    else if (f77_ukind[u] == F77_UNIT_SUBR) skip = 10;         /* SUBROUTINE */
    else {
        rty = f77_unit_header_ty();
        skip = f77_function_skip();
        (void)rty;
    }
    f77_scan_from(skip);
    if (lx_t != T_NAME) { f77_error("subprogram needs a name"); return; }
    f77_tok();
    f77_hdr_skip_len();

    /* A CHARACTER FUNCTION takes its result buffer and length as two
     * hidden leading arguments; the dummies follow. */
    if (f77_ukind[u] == F77_UNIT_FUNC && f77_urty[u] == TY_CHAR) nparam = 2;
    if (f77_ukind[u] == F77_UNIT_FUNC && f77_urty[u] == TY_DCPLX) nparam = 1;

    if (lx_t == T_LP) {
        f77_tok();
        if (lx_t != T_RP) {
            for (;;) {
                if (lx_t == T_STAR) {
                    /* alternate return: no argument travels for it */
                    f77_tok();
                    if (lx_t != T_COMMA) break;
                    f77_tok();
                    continue;
                }
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
    f77_assign_params(nparam);

    if (f77_ukind[u] == F77_UNIT_FUNC && f77_urty[u] == TY_CHAR) {
        /* The result is the caller's buffer: a by-reference CHARACTER
         * dummy in all but name, with its length in hidden param 1. */
        f77_result_sym = f77_sym_param(f77_uname[u], 0);
        f77_sty[f77_result_sym] = TY_CHAR;
        f77_styped[f77_result_sym] = 1;
        f77_slen[f77_result_sym] = f77_urlen[u];
        f77_slenv[f77_result_sym] = hi_emit(HI_PARAM, TY_INT, -1, -1, 1, NULL);
    } else if (f77_ukind[u] == F77_UNIT_FUNC && f77_urty[u] == TY_DCPLX) {
        /* The result is the caller's 16-byte temporary, hidden param 0
         * (f77_dcx_fcall): a by-reference COMPLEX*16 dummy in all but name. */
        f77_result_sym = f77_sym_param(f77_uname[u], 0);
        f77_sty[f77_result_sym] = TY_DCPLX;
        f77_styped[f77_result_sym] = 1;
    } else if (f77_ukind[u] == F77_UNIT_FUNC) {
        f77_result_sym = f77_sym(f77_uname[u]);
        f77_sty[f77_result_sym] = f77_urty[u];
        f77_slog[f77_result_sym] = f77_urlog[u];
        f77_snar[f77_result_sym] = f77_urnar[u];
        /* A typed header (INTEGER FUNCTION F...) is an explicit
         * declaration; a bare FUNCTION F relies on implicit typing,
         * which a later IMPLICIT may not change (phase 1 already told
         * every caller the result type). */
        f77_styped[f77_result_sym] = (skip != 8 && skip != 5) ? 1 : 0;
        f77_realloc_sym(f77_result_sym);
    }

    /* Phantoms for the family's other dummies (see f77_famd_name). */
    if (f77_ukind[u] != F77_UNIT_PROGRAM && f77_ukind[u] != F77_UNIT_BDATA) {
        int fam;
        int q;
        fam = f77_uentry_parent[u] >= 0 ? f77_uentry_parent[u] : u;
        for (q = 0; q < f77_nfamd; q++) {
            int du;
            int ps;
            du = f77_famd_unit[q];
            if (du == u) continue;
            if (du != fam && f77_uentry_parent[du] != fam) continue;
            ps = f77_sym_find(f77_famd_name[q]);
            if (ps >= 0) continue;                    /* a real dummy of this unit, or seen */
            ps = f77_sym(f77_famd_name[q]);           /* a frame slot ... */
            f77_sarg[ps] = 1;                         /* ... treated as the address passed */
        }
    }

    /* Entry names and the function name denote one result (F77 15.7.3). */
    if (f77_ukind[u] == F77_UNIT_FUNC && f77_result_sym >= 0) {
        int fam;
        int e;
        fam = f77_uentry_parent[u] >= 0 ? f77_uentry_parent[u] : u;
        for (e = 0; e < f77_nunit; e++) {
            if (e == u) continue;
            if (e != fam && f77_uentry_parent[e] != fam) continue;
            /* A COMPLEX*16 result lives in the caller's temporary, any
             * other in this frame: one family cannot have both. */
            if ((f77_urty[e] == TY_DCPLX) != (f77_urty[u] == TY_DCPLX)) {
                f77_error("an ENTRY of a different type than its COMPLEX*16 FUNCTION (or the reverse) is not supported");
                return;
            }
            if (f77_stfb_n < F77_MAX_STFB) {
                strcpy(f77_stfb_name[f77_stfb_n], f77_uname[e]);
                f77_stfb_sym[f77_stfb_n] = f77_result_sym;
                f77_stfb_n = f77_stfb_n + 1;
            }
        }
    }
}

/* Is the assembled statement a specification statement for the purpose
 * of compiling an ENTRY: a declaration, or a statement function? */
static int f77_is_spec_stmt(void) {
    int cls;
    int i;
    int eq;
    cls = f77_classify();
    if (f77_stmt_is_declaration(cls)) return 1;
    if (cls != S_ASSIGN) return 0;
    i = 0;
    while (i < lx_stmt_len && (lx_isalpha(lx_stmt[i]) || lx_isdigit(lx_stmt[i]))) i = i + 1;
    if (i == 0 || i >= lx_stmt_len || lx_stmt[i] != '(') return 0;
    eq = f77_toplevel('=', 0);
    if (eq < 0 || f77_match_paren(i) != eq) return 0;
    {
        char nm[F77_MAX_NAME];
        int sf;
        if (i >= F77_MAX_NAME) return 0;
        memcpy(nm, lx_stmt, i); nm[i] = 0;
        sf = f77_sym_find(nm);
        if (sf >= 0 && (f77_srank[sf] > 0 || f77_sty[sf] == TY_CHAR)) return 0;
    }
    return 1;
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
    if (vty == TY_DCPLX) {
        f77_carg(f77_dcx_arg(v), 0);
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "f77_wr_z");
        h_cbase[r] = cb;
        return;
    }
    if (vty == TY_CPLX) {
        f77_carg(v, 0);
        f77_carg(vhi, 0);
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "f77_wr_c");
        h_cbase[r] = cb;
        return;
    }
    if (vty == TY_DOUBLE) {
        f77_carg(v, 1);
        f77_carg(vhi, 2);
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "f77_wr_d");
    } else if (vty == TY_FLOAT) {
        f77_carg(v, 0);
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "f77_wr_r");
    } else if (v == f77_nar_ld && vty == TY_INT) {
        /* fort370: an INTEGER*2 or LOGICAL*1 item, its width with it --
         * A editing writes its bytes */
        f77_carg(v, 0);
        f77_carg(f77_iconst(f77_tysize(f77_nar_ty)), 0);
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "f77_wr_in");
    } else {
        f77_carg(v, 0);
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
    if (f77_io_unf) {
        int args[2];
        args[0] = addr;
        args[1] = (ty == TY_CHAR) ? f77_rd_len : f77_iconst(f77_tysize(ty));
        f77_call_i("f77_ru", 2, args);
        return;
    }
    if (ty == TY_CHAR) {
        int args[2];
        args[0] = addr; args[1] = f77_rd_len;
        f77_call_i("f77_rd_s", 2, args);
        return;
    }
    if (ty == TY_SHORT || ty == F77_TY_BYTE) {
        /* fort370: an INTEGER*2 or LOGICAL*1, its width with it: the
         * runtime keeps the low bytes (H keeps 4464 of 70000), and A
         * editing reads its bytes */
        int args[2];
        args[0] = addr; args[1] = f77_iconst(f77_tysize(ty));
        f77_call_i("f77_rd_in", 2, args);
        return;
    }
    cb = h_ncarg;
    f77_carg(addr, 0);
    if (ty == TY_DCPLX)
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "f77_rd_z");
    else if (ty == TY_CPLX)
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "f77_rd_c");
    else if (ty == TY_DOUBLE)
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
    *tyo = f77_mty(s);
    if (f77_sty[s] == TY_CHAR) {
        int a;
        a = f77_char_ref(s);
        f77_rd_len = ex_len;
        return a;
    }
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
            if (f77_eq_is_assign(i) && eq < 0) eq = i;
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

/* One element of array s in an I/O list, at addr: read or written by its
 * type and the statement's kind. */
static void f77_io_elem(int s, int addr) {
    int v;
    int vhi;
    if (f77_io_unf) {
        int args[2];
        args[0] = addr;
        args[1] = (f77_sty[s] == TY_CHAR && f77_slen[s] < 0) ? f77_slenv[s] : f77_iconst(f77_esize(s));
        f77_call_i(f77_io_rd ? "f77_ru" : "f77_wu", 2, args);
    } else if (f77_sty[s] == TY_CHAR) {
        int args[2];
        args[0] = addr;
        args[1] = (f77_slen[s] >= 0) ? f77_iconst(f77_slen[s]) : f77_slenv[s];
        f77_call_i(f77_io_rd ? "f77_rd_s" : "f77_wr_a", 2, args);
    } else if (f77_io_rd) {
        f77_rd_item(addr, f77_mty(s));
    } else {
        v = f77_load_at(addr, f77_mty(s));
        vhi = ex_hi;
        f77_wr_item(v, f77_sty[s], vhi);
    }
}

/* One element of a whole array in an I/O list, `off` its element number. */
static void f77_whole_item(int s, int off) {
    int byte;
    byte = f77_elem_bytes(s, off);
    f77_io_elem(s, hi_emit(HI_ADD, HL_ADDR_TY, f77_salloc[s] ? f77_desc_base(s) : f77_sval[s], byte, 0, NULL));
}

/* A section in an I/O list (Fortran 90), lx_t past its '(': its elements in
 * array element order, the first triplet fastest. */
static void f77_io_section(int s) {
    int idx[F77_MAX_RANK]; int trip[F77_MAX_RANK];
    int lo[F77_MAX_RANK]; int hi[F77_MAX_RANK]; int st[F77_MAX_RANK];
    F77Loop lp[F77_MAX_RANK];
    int cur[F77_MAX_RANK];
    int k;
    if (f77_section_subs(s, idx, trip, lo, hi, st) < 0) return;
    for (k = f77_srank[s] - 1; k >= 0; k--)
        if (trip[k]) f77_loop_begin(&lp[k], lo[k], hi[k], st[k]);
    for (k = 0; k < f77_srank[s]; k++)
        cur[k] = trip[k] ? hi_emit(HI_LOAD, TY_INT, lp[k].idx, -1, 0, NULL) : idx[k];
    f77_io_elem(s, f77_elem_addr(s, cur, f77_srank[s]));
    for (k = 0; k < f77_srank[s]; k++)
        if (trip[k]) f77_loop_end(&lp[k]);
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
    int cstride;

    rank = f77_srank[s];
    if (f77_adjustable(s) || f77_salloc[s]) {
        /* fort370: an adjustable array's extents are run-time values;
         * its elements, in storage order, are one run of the product of
         * them -- one loop over the element number */
        int n;
        n = f77_iconst(1);
        for (k = 0; k < rank; k++)
            n = hi_emit(HI_MUL, TY_INT, n, f77_salloc[s] ? f77_desc_load(s, 8 + 8 * k) :
                        f77_sextv[s][k] >= 0 ? f77_sextv[s][k] : f77_iconst(f77_sext[s][k]), 0, NULL);
        f77_frame = f77_frame + 4;
        iv[0] = hi_emit(HI_ALLOCA, TY_INT, -1, -1, 0 - f77_frame, NULL);
        f77_alloca_add(iv[0], 0 - f77_frame);
        hi_emit(HI_STORE, TY_INT, iv[0], f77_iconst(0), 0, NULL);
        b_test[0] = hir_new_block();
        b_body[0] = hir_new_block();
        b_exit[0] = hir_new_block();
        f77_goto_blk(b_test[0]);
        f77_begin_blk(b_test[0]);
        c = hi_emit(HI_LOAD, TY_INT, iv[0], -1, 0, NULL);
        c = hi_emit(HI_SLT, TY_INT, c, n, 0, NULL);
        hi_emit(HI_BRC, TY_VOID, c, b_body[0], b_exit[0], NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_body[0]);
        off = hi_emit(HI_LOAD, TY_INT, iv[0], -1, 0, NULL);
        f77_whole_item(s, off);
        idx = hi_emit(HI_LOAD, TY_INT, iv[0], -1, 0, NULL);
        idx = hi_emit(HI_ADDI, TY_INT, idx, -1, 1, NULL);
        hi_emit(HI_STORE, TY_INT, iv[0], idx, 0, NULL);
        hi_emit(HI_BR, TY_VOID, -1, -1, b_test[0], NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_exit[0]);
        return;
    }
    for (k = rank - 1; k >= 0; k--) {
        f77_frame = f77_frame + 4;
        iv[k] = hi_emit(HI_ALLOCA, TY_INT, -1, -1, 0 - f77_frame, NULL);
        f77_alloca_add(iv[k], 0 - f77_frame);
        f77_frame = f77_frame + 4;
        trip[k] = hi_emit(HI_ALLOCA, TY_INT, -1, -1, 0 - f77_frame, NULL);
        f77_alloca_add(trip[k], 0 - f77_frame);
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
    f77_whole_item(s, off);
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

static void f77_wr_one(void);

/* An implied-DO `(items, VAR = e1, e2 [, e3])`, lx_t at its '(': 1 when
 * it is one, its loop emitted around items(items_off, ctl_off) -- which
 * parses the items from items_off, stopping before the comma at
 * ctl_off - 1 -- and the scanner left past its ')'; 0, nothing consumed,
 * when it is not.  The loop control sits AFTER the items it governs, so
 * the control spec is parsed first from its own offset, the loop opened,
 * and the scanner then rewound to the items -- which are emitted
 * straight into the body block.  Rewinding is safe here precisely
 * because nothing has been emitted for the items yet.  I/O lists and
 * array constructors share it. */
static int f77_implied_do(void (*items)(int, int)) {
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
            if (lx_t != T_NAME) { f77_error("implied-DO needs a control variable"); return 1; }
            sv = f77_sym(lex_name);
            f77_tok();
            if (lx_t != T_ASSIGN) { f77_error("expected = in implied-DO"); return 1; }
            f77_tok();
            m1 = f77_expr(); m1 = f77_cvt(m1, &ex_hi, ex_ty, TY_INT);
            if (lx_t != T_COMMA) { f77_error("expected , in implied-DO"); return 1; }
            f77_tok();
            m2 = f77_expr(); m2 = f77_cvt(m2, &ex_hi, ex_ty, TY_INT);
            if (lx_t == T_COMMA) {
                f77_tok();
                m3 = f77_expr(); m3 = f77_cvt(m3, &ex_hi, ex_ty, TY_INT);
            } else {
                m3 = f77_iconst(1);
            }

            f77_sstored[sv] = 1;
            if (f77_snar[sv]) f77_store_at(f77_sval[sv], f77_mty(sv), m1, TY_INT, -1);
            else hi_emit(HI_STORE, TY_INT, f77_sval[sv], m1, 0, NULL);
            t1 = hi_emit(HI_SUB, TY_INT, m2, m1, 0, NULL);
            t2 = hi_emit(HI_ADD, TY_INT, t1, m3, 0, NULL);
            t3 = hi_emit(HI_DIV, TY_INT, t2, m3, 0, NULL);
            f77_frame = f77_frame + 4;
            trip = hi_emit(HI_ALLOCA, TY_INT, -1, -1, 0 - f77_frame, NULL);
            f77_alloca_add(trip, 0 - f77_frame);
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
            items(items_off, ctl_off);

            {
                int iv;
                if (f77_snar[sv]) {
                    iv = f77_load_at(f77_sval[sv], f77_mty(sv));
                    iv = hi_emit(HI_ADD, TY_INT, iv, m3, 0, NULL);
                    f77_store_at(f77_sval[sv], f77_mty(sv), iv, TY_INT, -1);
                } else {
                    iv = hi_emit(HI_LOAD, TY_INT, f77_sval[sv], -1, 0, NULL);
                    iv = hi_emit(HI_ADD, TY_INT, iv, m3, 0, NULL);
                    hi_emit(HI_STORE, TY_INT, f77_sval[sv], iv, 0, NULL);
                }
                iv = hi_emit(HI_LOAD, TY_INT, trip, -1, 0, NULL);
                iv = hi_emit(HI_ADDI, TY_INT, iv, -1, -1, NULL);
                hi_emit(HI_STORE, TY_INT, trip, iv, 0, NULL);
                hi_emit(HI_BR, TY_VOID, -1, -1, b_test, NULL);
            }
            f77_cur_blk_live = 0;
            f77_begin_blk(b_exit);

            /* Continue after the implied-DO's closing paren. */
            f77_scan_from(close_off);
            return 1;
        }
    }
    return 0;
}

/* An I/O list's implied-DO items. */
static void f77_io_ido_items(int items_off, int ctl_off) {
    f77_scan_from(items_off);
    for (;;) {
        f77_wr_one();
        if (lx_t != T_COMMA) break;
        if ((int)(lx_rts - lx_stmt) >= ctl_off - 1) break;
        f77_tok();
        if ((int)(lx_rts - lx_stmt) >= ctl_off) break;
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

    if (lx_t == T_SCON && f77_io_rd) {
        f77_error("a character constant cannot be a READ list item");
        return;
    }
    if (lx_t == T_NAME && f77_sym_find(lex_name) >= 0) {
        /* an array is declared: a name not yet a symbol is a scalar or
         * a function, which the expression below makes of it (making
         * it here had IMPLICIT NONE refuse TRIM) */
        int s;
        int after;
        s = f77_sym_find(lex_name);
        after = (int)(lx_rte - lx_stmt);
        if (f77_srank[s] > 0 &&
            (after >= lx_stmt_len || lx_stmt[after] != '(')) {
            f77_tok();
            f77_wr_whole_array(s);
            return;
        }
        if (f77_srank[s] > 0 && f77_sty[s] != TY_CHAR) {
            int off;
            off = (int)(lx_rts - lx_stmt);
            f77_tok();
            if (lx_t == T_LP && f77_paren_has_colon()) {
                /* a section: unless more follows it (A(1:3)+1 is an
                 * array expression, milestone 3) */
                int end;
                end = f77_match_paren((int)(lx_rts - lx_stmt));
                if (end >= lx_stmt_len || lx_stmt[end] == ',' || lx_stmt[end] == ')') {
                    f77_tok();
                    f77_io_section(s);
                    return;
                }
            }
            f77_scan_from(off);
        }
    }

    /* Implied-DO: `(items, VAR = e1, e2 [, e3])` */
    if (lx_t == T_LP && f77_implied_do(f77_io_ido_items)) return;

    if (f77_io_rd) {
        int a;
        a = f77_rd_target(&vty);
        f77_rd_item(a, vty);
        return;
    }
    v = f77_expr();
    if (f77_io_unf) {
        /* raw bytes: a character value is an address already; anything
         * else goes through a temporary */
        int args[2];
        if (ex_ty == TY_CHAR) { args[0] = v; args[1] = ex_len; }
        else if (v == f77_nar_ld && ex_ty == TY_INT) {
            /* fort370: an INTEGER*2 or LOGICAL*1 item, at its width */
            int tmp;
            tmp = f77_char_temp(4);
            f77_store_at(tmp, f77_nar_ty, v, TY_INT, -1);
            args[0] = tmp; args[1] = f77_iconst(f77_tysize(f77_nar_ty));
        } else {
            int tmp;
            tmp = f77_char_temp(f77_tysize(ex_ty));
            f77_store_at(tmp, ex_ty, v, ex_ty, ex_hi);
            args[0] = tmp; args[1] = f77_iconst(f77_tysize(ex_ty));
        }
        f77_call_i("f77_wu", 2, args);
        return;
    }
    if (ex_ty == TY_CHAR) {
        int args[2];
        args[0] = v; args[1] = ex_len;
        f77_call_i("f77_wr_a", 2, args);
        return;
    }
    if (ex_ty == TY_INT && ex_log) {
        /* T or F (list-directed), or by the L descriptor */
        f77_call_i("f77_wr_l", 1, &v);
        return;
    }
    f77_wr_item(v, ex_ty, ex_hi);
}

static void f77_wr_list(void) {
    if (lx_t == T_EOF) return;
    for (;;) {
        f77_wr_one();
        if (lx_t != T_COMMA) break;
        f77_tok();
    }
}

/* Length of the format the runtime will walk: a CHARACTER's ex_len,
 * a FORMAT statement's interned length, -1 for an ASSIGNed label
 * (those strings are NUL-terminated), 0 when there is no format. */
static int f77_fmt_len_arg(int fmt_label) {
    int fs;
    if (fmt_label == -3) return f77_fmt_dyn_len;
    if (fmt_label == -2) return f77_iconst(-1);
    if (fmt_label >= 0) {
        fs = f77_find_format(fmt_label);
        if (fs < 0) return f77_iconst(-1);
        return f77_iconst(lex_str_len[fs]);
    }
    return f77_iconst(0);
}

/* Emit the f77_wr_begin call: unit number, plus the format string or a
 * null pointer for list-directed (FMT=*) output, plus the format length. */
static void f77_wr_begin(int unit_val, int fmt_label) {
    int cb;
    int r;
    int fs;
    int fa;
    cb = h_ncarg;
    f77_carg(unit_val, 0);
    if (f77_io_int) {
        f77_carg(f77_io_int_len, 0);
        f77_carg(f77_io_int_nrec, 0);
    }
    if (fmt_label == -3) {
        fa = f77_fmt_dyn;
    } else if (fmt_label == -2) {
        fa = f77_fmt_dyn_addr(f77_fmt_dyn);
    } else if (fmt_label >= 0) {
        fs = f77_find_format(fmt_label);
        if (fs < 0) { f77_error("no FORMAT statement with that label"); fs = 0; }
        fa = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, fs, NULL);
    } else {
        fa = f77_iconst(0);
    }
    f77_carg(fa, 0);
    f77_carg(f77_fmt_len_arg(fmt_label), 0);
    if (f77_io_int) r = hi_emit(HI_CALL, TY_INT, -1, -1, 5, "f77_wr_begin_int");
    else r = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "f77_wr_begin");
    h_cbase[r] = cb;
    f77_io_int = 0;
}

static void f77_wr_finish(void) {
    int r;
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 0, "f77_wr_end");
    h_cbase[r] = h_ncarg;
}

/* fort370: a format the compiler can see -- a FORMAT statement, or a
 * format that is one character constant -- is checked here for what is
 * certainly wrong, which the runtime would otherwise read some other
 * way (fort370 issue 24): an nH that runs past the end of the format,
 * a count with no descriptor after it (5H1234567 is 5H12345 and then
 * 67), a letter that is no edit descriptor, I F E D G or L without its
 * width, and parentheses that do not match; what follows the last )
 * is ignored, as F77 says.  What IBM's compilers took,
 * a comma left out between two items among them, is taken here too.
 * In a FORMAT statement the lexer has made each nH a quoted string
 * already, so both forms are read. */
static int f77_fmt_digits(const char *s, int n, int *i) {
    int v;
    v = 0;
    while (*i < n && s[*i] == ' ') *i = *i + 1;
    if (*i >= n || s[*i] < '0' || s[*i] > '9') return -1;
    while (*i < n && ((s[*i] >= '0' && s[*i] <= '9') || s[*i] == ' ')) {
        if (s[*i] != ' ') v = v * 10 + (s[*i] - '0');
        *i = *i + 1;
    }
    return v;
}
/* the next character that is not a blank, or 0 at the end; blanks in a
 * format are not significant outside its strings (fort370 issue 25) */
static int f77_fmt_peek(const char *s, int n, int *i) {
    while (*i < n && s[*i] == ' ') *i = *i + 1;
    return *i < n ? s[*i] : 0;
}
static void f77_fmt_bad(const char *what) {
    char m[96];
    strcpy(m, "FORMAT: ");
    strcat(m, what);
    f77_error(m);
}
static void f77_check_format(const char *s, int n) {
    int i, depth, c, k, cnt;
    while (n > 0 && s[n - 1] == ' ') n = n - 1;
    i = 0;
    while (i < n && s[i] == ' ') i = i + 1;
    if (i >= n || s[i] != '(') { f77_fmt_bad("a format begins with ("); return; }
    i = i + 1;
    depth = 1;
    while (i < n) {
        c = s[i];
        if (c == ' ' || c == ',' || c == '/' || c == ':') { i = i + 1; continue; }
        if (c == ')') {
            depth = depth - 1;
            i = i + 1;
            if (depth == 0) return;       /* what follows is ignored (F77 13.1.1) */
            continue;
        }
        if (c == '(') { depth = depth + 1; i = i + 1; continue; }
        if (c == '\'' || c == '"') {
            int q;
            q = c;
            i = i + 1;
            for (;;) {
                if (i >= n) { f77_fmt_bad("a quoted string runs past the end of the format"); return; }
                if (s[i] == q) {
                    if (i + 1 < n && s[i + 1] == q) { i = i + 2; continue; }
                    i = i + 1;
                    break;
                }
                i = i + 1;
            }
            continue;
        }
        cnt = -1;
        if (c == '+' || c == '-') {          /* a scale factor's sign: -2P */
            i = i + 1;
            cnt = f77_fmt_digits(s, n, &i);
            k = f77_fmt_peek(s, n, &i);
            if (cnt < 0 || (k != 'P' && k != 'p')) { f77_fmt_bad("a sign belongs to a scale factor, nP"); return; }
        } else if (c >= '0' && c <= '9') {
            cnt = f77_fmt_digits(s, n, &i);
        }
        if (f77_fmt_peek(s, n, &i) == 0) { f77_fmt_bad("the format ends without its )"); return; }
        c = s[i];
        if (c >= 'a' && c <= 'z') c = c - 'a' + 'A';
        if (cnt < 0 && c == 'H') { f77_fmt_bad("H needs its count, nH"); return; }
        if (cnt >= 0 && c == 'H') {
            if (cnt == 0) { f77_fmt_bad("0H is no Hollerith string"); return; }
            if (i + 1 + cnt > n) {
                char m[80];
                sprintf(m, "%dH wants %d characters, and the format has %d left", cnt, cnt, n - i - 1);
                f77_fmt_bad(m);
                return;
            }
            i = i + 1 + cnt;
            continue;
        }
        if (cnt >= 0 && c == '(') { depth = depth + 1; i = i + 1; continue; }   /* a group repeated */
        if (cnt >= 0 && (c == 'X' || c == 'P')) { i = i + 1; continue; }
        if (c == 'X') { i = i + 1; continue; }        /* X alone: IBM's 1X */
        if (c == 'T') {
            i = i + 1;
            k = f77_fmt_peek(s, n, &i);
            if (k == 'L' || k == 'R' || k == 'l' || k == 'r') i = i + 1;
            if (f77_fmt_digits(s, n, &i) < 0) { f77_fmt_bad("T, TL and TR take a column"); return; }
            continue;
        }
        if (c == 'S') {
            i = i + 1;
            k = f77_fmt_peek(s, n, &i);
            if (k == 'P' || k == 'S' || k == 'p' || k == 's') i = i + 1;
            continue;
        }
        if (c == 'B') {
            i = i + 1;
            k = f77_fmt_peek(s, n, &i);
            if (k == 'N' || k == 'Z' || k == 'n' || k == 'z') { i = i + 1; continue; }
            f77_fmt_bad("B is BN or BZ");
            return;
        }
        if (c == 'A' || c == 'Z' || c == 'O') {    /* the width may be left out */
            i = i + 1;
            (void)f77_fmt_digits(s, n, &i);
            continue;
        }
        if (c == 'I' || c == 'F' || c == 'E' || c == 'D' || c == 'G' || c == 'L') {
            int w;
            int d;
            i = i + 1;
            if (c == 'E') {                          /* ES, EN (Fortran 90) */
                k = f77_fmt_peek(s, n, &i);
                if (k == 'S' || k == 'N' || k == 's' || k == 'n') i = i + 1;
            }
            w = f77_fmt_digits(s, n, &i);
            if (w < 0) {
                char m[48];
                sprintf(m, "%c needs a width", c);
                f77_fmt_bad(m);
                return;
            }
            if (f77_fmt_peek(s, n, &i) == '.') {
                i = i + 1;
                d = f77_fmt_digits(s, n, &i);
                if (d < 0) { f77_fmt_bad("a . needs digits after it"); return; }
                /* Iw.m: m digits at least, in a field of w (13.5.9.1);
                 * I10.20 printed asterisks (fort370 issue 38, from MV410) */
                if ((c == 'I' || c == 'i') && d > w) { f77_fmt_bad("Iw.m needs m no more than w"); return; }
            }
            k = f77_fmt_peek(s, n, &i);
            if ((c == 'E' || c == 'G' || c == 'D') && (k == 'E' || k == 'e')) {
                i = i + 1;
                if (f77_fmt_digits(s, n, &i) < 0) { f77_fmt_bad("Ee needs its digits"); return; }
            }
            continue;
        }
        if (cnt >= 0) {
            f77_fmt_bad(c == ')' || c == ',' ? "a count with no descriptor after it"
                                             : "a count before something that is no edit descriptor");
            return;
        }
        k = c;
        {
            char m[64];
            sprintf(m, "%c is no edit descriptor", k);
            f77_fmt_bad(m);
        }
        return;
    }
    f77_fmt_bad("the format ends without its )");
}

/* WRITE (unit, fmt) list  --  unit and fmt may each be `*`. */
/* The control list of a READ or WRITE: ( [UNIT=]u [, [FMT=]f] [, END=l]
 * [, ERR=l] [, IOSTAT=v] [, REC=r] ) in any order once keyworded.
 * fmt_label: >= 0 a label, -1 list-directed (*), -2 ASSIGNed variable,
 * -3 a CHARACTER expression (both in f77_fmt_dyn), -4 none at all
 * (unformatted). */
static int f77_io_end_label;
static int f77_io_err_label;
static int f77_io_iostat;      /* address of the IOSTAT variable, or -1 */
static int f77_io_rec;         /* REC= value, or -1 */

static int f77_io_fmt_spec(void) {
    if (lx_t == T_STAR) { f77_tok(); return -1; }
    if (lx_t == T_NAME && f77_nml_find(lex_name) >= 0) {
        /* a NAMELIST group: -5, the group in f77_io_nml */
        f77_io_nml = f77_nml_find(lex_name);
        f77_tok();
        return -5;
    }
    if (lx_t == T_ICON) { int l; l = lex_ival; f77_tok(); return l; }
    if (lx_t == T_SCON || (lx_t == T_NAME && f77_sty[f77_sym(lex_name)] == TY_CHAR)) {
        /* fort370: a format that is one character constant is checked
         * as a FORMAT statement is (fort370 issue 24) */
        if (lx_t == T_SCON && f77_inline_depth == 0) {
            int sidx, slen;
            sidx = lex_sidx; slen = lex_slen;
            f77_fmt_dyn = f77_expr();
            /* the constant itself, not one end of a // */
            if (f77_fmt_dyn >= 0 && h_kind[f77_fmt_dyn] == HI_SADDR && h_val[f77_fmt_dyn] == sidx)
                f77_check_format(lex_strpool + lex_str_off[sidx], slen);
            f77_fmt_dyn_len = ex_len;
            return -3;
        }
        {
            /* an unsubscripted CHARACTER array is the concatenation of
             * its elements (F77 13.1.1); a single element is one item. */
            /* an assumed-size one has no known size: walked as before,
             * to its ); an adjustable one's count is the run-time
             * product of its extents */
            int nelem = 1, whole = -1;
            if (lx_t == T_NAME) {
                int s = f77_sym(lex_name);
                int after = (int)(lx_rte - lx_stmt);
                if (f77_srank[s] > 0 && (after >= lx_stmt_len || lx_stmt[after] != '(')) {
                    whole = s;
                    nelem = f77_sym_nelem(s);
                }
            }
            f77_fmt_dyn = f77_expr();
            f77_fmt_dyn_len = ex_len;
            if (whole >= 0 && f77_sassz[whole])
                f77_fmt_dyn_len = f77_iconst(-1);
            else if (whole >= 0 && f77_adjustable(whole)) {
                int k, n = f77_iconst(1);
                for (k = 0; k < f77_srank[whole]; k++)
                    n = hi_emit(HI_MUL, TY_INT, n, f77_sextv[whole][k] >= 0 ? f77_sextv[whole][k] : f77_iconst(f77_sext[whole][k]), 0, NULL);
                f77_fmt_dyn_len = hi_emit(HI_MUL, TY_INT, ex_len, n, 0, NULL);
            } else if (nelem > 1)
                f77_fmt_dyn_len = hi_emit(HI_MUL, TY_INT, ex_len, f77_iconst(nelem), 0, NULL);
            return -3;
        }
    }
    if (lx_t == T_NAME) {
        f77_fmt_dyn = f77_load_sym(f77_sym(lex_name));
        f77_fmt_dyn = f77_cvt(f77_fmt_dyn, &ex_hi, ex_ty, TY_INT);
        f77_tok();
        return -2;
    }
    f77_error("bad FORMAT specifier");
    return -1;
}

static int f77_io_control(int is_read, int *unit_val, int *fmt_label) {
    int pos;
    int have_unit;
    char kw[F77_MAX_NAME];
    pos = 0;
    have_unit = 0;
    *fmt_label = -4;
    f77_io_end_label = -1;
    f77_io_err_label = -1;
    f77_io_iostat = -1;
    f77_io_rec = -1;
    f77_io_int = 0;
    if (lx_t != T_LP) { f77_error("I/O statement needs a control list"); return 0; }
    f77_tok();
    for (;;) {
        int iskw;
        iskw = 0;
        if (lx_t == T_NAME) {
            /* NAME= is a keyword; otherwise the name starts a value */
            int after;
            after = (int)(lx_rte - lx_stmt);
            if (after < lx_stmt_len && lx_stmt[after] == '=' &&
                !(after + 1 < lx_stmt_len && lx_stmt[after + 1] == '=')) {
                strcpy(kw, lex_name);
                f77_tok();
                f77_tok();
                iskw = 1;
            }
        }
        if (!iskw) {
            if (pos == 0) {
                if (lx_t == T_STAR) { *unit_val = f77_iconst(is_read ? 5 : 6); f77_tok(); }
                else *unit_val = f77_io_unit_expr();
                have_unit = 1;
            } else if (pos == 1) {
                *fmt_label = f77_io_fmt_spec();
            } else {
                f77_error("too many positional items in the control list");
                return 0;
            }
            pos = pos + 1;
        } else if (strcmp(kw, "UNIT") == 0) {
            if (lx_t == T_STAR) { *unit_val = f77_iconst(is_read ? 5 : 6); f77_tok(); }
            else *unit_val = f77_io_unit_expr();
            have_unit = 1;
            pos = pos < 1 ? 1 : pos;
        } else if (strcmp(kw, "FMT") == 0 || strcmp(kw, "NML") == 0) {
            *fmt_label = f77_io_fmt_spec();
            if (kw[0] == 'N' && *fmt_label != -5) { f77_error("NML= names a NAMELIST group"); return 0; }
            pos = 2;
        } else if (strcmp(kw, "END") == 0 || strcmp(kw, "ERR") == 0) {
            if (lx_t != T_ICON) { f77_error("END=/ERR= needs a statement label"); return 0; }
            if (kw[1] == 'N') f77_io_end_label = lex_ival; else f77_io_err_label = lex_ival;
            f77_tok();
        } else if (strcmp(kw, "IOSTAT") == 0) {
            int ss;
            if (lx_t != T_NAME) { f77_error("IOSTAT= needs a variable"); return 0; }
            ss = f77_sym(lex_name);
            f77_sstored[ss] = 1;
            f77_tok();
            if (lx_t == T_LP && f77_srank[ss] > 0) { f77_tok(); f77_io_iostat = f77_subscript_addr(ss); }
            else {
                if (f77_shi[ss] >= 0) hi_emit(HI_ADDI, HL_ADDR_TY, f77_shi[ss], -1, 0, NULL);
                f77_io_iostat = f77_sval[ss];
            }
        } else if (strcmp(kw, "REC") == 0) {
            f77_io_rec = f77_expr();
            f77_io_rec = f77_cvt(f77_io_rec, &ex_hi, ex_ty, TY_INT);
        } else {
            f77_error("unknown keyword in the control list");
            return 0;
        }
        if (lx_t == T_COMMA) { f77_tok(); continue; }
        break;
    }
    if (lx_t != T_RP) { f77_error("expected ) after the control list"); return 0; }
    f77_tok();
    if (!have_unit) { f77_error("the control list needs a unit"); return 0; }
    /* A CHARACTER unit is an internal file, and an internal file is
     * read and written only with a format (F77 12.2.5): READ('(I5)') N
     * is a format written where the unit goes. */
    if (f77_io_int && (*fmt_label == -4 || *fmt_label == -5)) {
        f77_error("a CHARACTER unit is an internal file and needs a format");
        return 0;
    }
    return 1;
}

/* Common tail of READ/WRITE begin: REC= is announced to the runtime
 * before the statement opens. */
static void f77_io_announce(void) {
    if (f77_io_rec >= 0) f77_call_i("f77_set_rec", 1, &f77_io_rec);
}

/* After f77_rd_begin: branch on its code -- 1 end of file, 2 error --
 * to END=/ERR=, and store IOSTAT (-1 eof, 1 error, 0 ok). */
static void f77_io_after_read(int r) {
    if (f77_io_iostat >= 0) {
        int c; int v;
        /* iostat = (r == 1) ? -1 : (r == 2) ? 1 : 0 */
        c = hi_emit(HI_SEQ, TY_INT, r, f77_iconst(1), 0, NULL);
        v = f77_select(c, f77_iconst(-1), r);      /* 0 -> 0, 2 -> 2 -> then map 2 -> 1 */
        c = hi_emit(HI_SEQ, TY_INT, r, f77_iconst(2), 0, NULL);
        v = f77_select(c, f77_iconst(1), v);
        hi_emit(HI_STORE, TY_INT, f77_io_iostat, v, 0, NULL);
    }
    if (f77_io_end_label >= 0 || f77_io_err_label >= 0) {
        int b_cont; int c;
        if (f77_io_end_label >= 0) {
            b_cont = hir_new_block();
            c = hi_emit(HI_SEQ, TY_INT, r, f77_iconst(1), 0, NULL);
            hi_emit(HI_BRC, TY_VOID, c, f77_label_blk(f77_io_end_label), b_cont, NULL);
            f77_cur_blk_live = 0;
            f77_begin_blk(b_cont);
        }
        if (f77_io_err_label >= 0) {
            b_cont = hir_new_block();
            c = hi_emit(HI_SEQ, TY_INT, r, f77_iconst(2), 0, NULL);
            hi_emit(HI_BRC, TY_VOID, c, f77_label_blk(f77_io_err_label), b_cont, NULL);
            f77_cur_blk_live = 0;
            f77_begin_blk(b_cont);
        }
    }
}

/* READ or WRITE of a NAMELIST group: the runtime is told the group,
 * then each item's name, kind, address and size (an array's bounds
 * first), and reads or writes the lot at f77_nml_end (runtime/libf77.c,
 * NAMELIST).  A READ's END=, ERR= and IOSTAT= take f77_nml_end's code.
 * (fort370 issue 4.  This front end has no DOUBLE COMPLEX, the runtime's code 5.) */
static int f77_nml_name(char *nm) {
    int sidx;
    sidx = f77_intern_str(nm, (int)strlen(nm));
    return hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, sidx, NULL);
}
static void f77_nml_io(int unit_val, int reading) {
    int g, k, args[6], r;
    g = f77_io_nml;
    args[0] = unit_val;
    args[1] = f77_nml_name(f77_nml_gname[g]);
    args[2] = f77_iconst((int)strlen(f77_nml_gname[g]));
    args[3] = f77_iconst(reading);
    args[4] = f77_iconst((f77_io_end_label >= 0 ? 1 : 0) |
                         ((f77_io_err_label >= 0 || f77_io_iostat >= 0) ? 2 : 0));
    f77_call_i("f77_nml_begin", 5, args);
    for (k = 0; k < f77_nml_gcount[g]; k++) {
        int s, code, ty, d, n;
        s = f77_nml_isym[f77_nml_gfirst[g] + k];
        ty = f77_sty[s];
        if (ty == TY_CHAR) code = 7;
        else if (ty == TY_CPLX) code = 4;
        else if (ty == TY_DCPLX) code = 5;
        else if (ty == TY_DOUBLE) code = 3;
        else if (ty == TY_FLOAT) code = 2;
        else code = f77_slog[s] ? 6 : 1;
        n = f77_iconst(1);
        for (d = 0; d < f77_srank[s]; d++) {
            int lo, ext;
            lo = f77_slov[s][d] >= 0 ? f77_slov[s][d] : f77_iconst(f77_slo[s][d]);
            ext = f77_sextv[s][d] >= 0 ? f77_sextv[s][d] : f77_iconst(f77_sext[s][d]);
            args[0] = lo;
            args[1] = ext;
            f77_call_i("f77_nml_dim", 2, args);
            n = f77_srank[s] == 1 && d == 0 ? ext : hi_emit(HI_MUL, TY_INT, n, ext, 0, NULL);
        }
        args[0] = f77_nml_name(f77_sname[s]);
        args[1] = f77_iconst((int)strlen(f77_sname[s]));
        args[2] = f77_iconst(code);
        args[3] = f77_sval[s];
        if (ty == TY_CHAR) args[4] = f77_slen[s] >= 0 ? f77_iconst(f77_slen[s]) : f77_slenv[s];
        else args[4] = f77_iconst(f77_esize(s));
        args[5] = n;
        f77_call_i("f77_nml_item", 6, args);
    }
    r = f77_call_i("f77_nml_end", 0, args);
    if (reading) f77_io_after_read(r);
}

/* NAMELIST /GROUP/ item, item... [/GROUP/ ...]: FORTRAN H's, its items
 * variables and arrays of the unit. */
static void f77_stmt_namelist(int skip) {
    f77_scan_from(skip);
    while (lx_t == T_SLASH) {
        int g;
        f77_tok();
        if (lx_t != T_NAME) { f77_error("NAMELIST needs a group name between slashes"); return; }
        if (f77_nml_find(lex_name) >= 0) { f77_error("a NAMELIST group of that name is already defined"); return; }
        if (f77_nnml >= F77_MAX_NML) { f77_error("too many NAMELIST groups in one unit"); return; }
        g = f77_nnml;
        f77_nnml = f77_nnml + 1;
        strncpy(f77_nml_gname[g], lex_name, 31);
        f77_nml_gname[g][31] = 0;
        f77_nml_gfirst[g] = f77_nnmlitem;
        f77_nml_gcount[g] = 0;
        f77_tok();
        if (lx_t != T_SLASH) { f77_error("expected / after the NAMELIST group name"); return; }
        f77_tok();
        while (lx_t == T_NAME) {
            int s;
            s = f77_sym(lex_name);
            if (f77_sexternal[s] || f77_sparam[s]) { f77_error("a NAMELIST item is a variable or an array"); return; }
            if (f77_nnmlitem >= F77_MAX_NMLITEM) { f77_error("too many NAMELIST items in one unit"); return; }
            f77_sstored[s] = 1;
            f77_nml_isym[f77_nnmlitem] = s;
            f77_nnmlitem = f77_nnmlitem + 1;
            f77_nml_gcount[g] = f77_nml_gcount[g] + 1;
            f77_tok();
            if (lx_t == T_COMMA) { f77_tok(); continue; }
            break;
        }
        if (f77_nml_gcount[g] == 0) { f77_error("a NAMELIST group needs at least one item"); return; }
    }
    if (lx_t != T_EOF) f77_error("NAMELIST is /GROUP/ followed by names");
}

static void f77_stmt_write(int skip) {
    int unit_val;
    int fmt_label;
    f77_scan_from(skip);
    if (!f77_io_control(0, &unit_val, &fmt_label)) return;
    f77_io_announce();
    if (fmt_label == -5) {
        if (lx_t != T_EOF) { f77_error("a NAMELIST WRITE has no list"); return; }
        f77_nml_io(unit_val, 0);
        return;
    }
    if (fmt_label == -4) {
        /* Unformatted: raw bytes, one runtime call per item. */
        int r;
        f77_io_unf = 1;
        f77_call_i("f77_wu_begin", 1, &unit_val);
        f77_wr_list();
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 0, "f77_wu_end");
        h_cbase[r] = h_ncarg;
        f77_io_unf = 0;
        return;
    }
    f77_wr_begin(unit_val, fmt_label);
    f77_wr_list();
    f77_wr_finish();
}

/* PRINT fmt, list  --  always unit 6. */
static void f77_stmt_print(int skip) {
    int fmt_label;
    f77_scan_from(skip);
    fmt_label = f77_io_fmt_spec();
    /* blanks do not count: PRINT * 1234 is PRINT*1234, no list -- a
     * comma is required before one (fort370 issue 38) */
    if (lx_t == T_COMMA) f77_tok();
    else if (lx_t != T_EOF) {
        f77_error("a comma after the format, before the list");
        return;
    }
    f77_io_int = 0;
    f77_io_rec = -1;
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
    int cb;
    int r;
    int fs;
    int fa;

    f77_scan_from(skip);
    f77_io_int = 0;
    f77_io_end_label = -1;
    f77_io_err_label = -1;
    f77_io_iostat = -1;
    f77_io_rec = -1;

    if (lx_t == T_LP) {
        if (!f77_io_control(1, &unit_val, &fmt_label)) return;
    } else {
        /* READ fmt, list  --  always unit 5. */
        unit_val = f77_iconst(5);
        fmt_label = f77_io_fmt_spec();
        if (lx_t == T_COMMA) f77_tok();         /* READ f, list: the comma required */
        else if (lx_t != T_EOF) {
            f77_error("a comma after the format, before the list");
            return;
        }
    }
    f77_io_announce();

    if (fmt_label == -5) {
        if (lx_t != T_EOF) { f77_error("a NAMELIST READ has no list"); return; }
        f77_nml_io(unit_val, 1);
        return;
    }
    if (fmt_label == -4) {
        int args[2];
        args[0] = unit_val;
        args[1] = f77_iconst((f77_io_end_label >= 0 ? 1 : 0) |
                             ((f77_io_err_label >= 0 || f77_io_iostat >= 0) ? 2 : 0));
        r = f77_call_i("f77_ru_begin", 2, args);
        f77_io_after_read(r);
        f77_io_unf = 1;
        f77_io_rd = 1;
        f77_wr_list();
        f77_io_rd = 0;
        f77_io_unf = 0;
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 0, "f77_ru_end");
        h_cbase[r] = h_ncarg;
        return;
    }

    cb = h_ncarg;
    f77_carg(unit_val, 0);
    if (f77_io_int) {
        f77_carg(f77_io_int_len, 0);
        f77_carg(f77_io_int_nrec, 0);
    }
    if (fmt_label == -3) {
        fa = f77_fmt_dyn;
    } else if (fmt_label == -2) {
        fa = f77_fmt_dyn_addr(f77_fmt_dyn);
    } else if (fmt_label >= 0) {
        fs = f77_find_format(fmt_label);
        if (fs < 0) { f77_error("no FORMAT statement with that label"); fs = 0; }
        fa = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, fs, NULL);
    } else {
        fa = f77_iconst(0);
    }
    f77_carg(fa, 0);
    f77_carg(f77_fmt_len_arg(fmt_label), 0);
    f77_carg(f77_iconst((f77_io_end_label >= 0 ? 1 : 0) |
                                 ((f77_io_err_label >= 0 || f77_io_iostat >= 0) ? 2 : 0)), 0);
    if (f77_io_int) r = hi_emit(HI_CALL, TY_INT, -1, -1, 6, "f77_rd_begin_int");
    else r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "f77_rd_begin");
    h_cbase[r] = cb;
    f77_io_int = 0;

    f77_io_after_read(r);

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
    if (f77_sparam[s]) {
        /* the type with it: every caller converts by ex_ty, which still
         * held the last expression's -- after READ (5, FMT='(A72)') the
         * format's CHARACTER, and CLOSE (NIN) was refused */
        ex_ty = TY_INT; ex_hi = -1; ex_log = 0;
        return f77_iconst(f77_spival[s]);
    }
    return f77_load_sym(s);
}

/* OPEN (unit, FILE='name' [, STATUS='...']) -- FILE must be a
 * character constant until CHARACTER variables exist.  Everything
 * else in the standard's control list is refused honestly. */
static void f77_stmt_open(int skip) {
    int unit_val;
    int have_unit;
    int file_addr; int file_lenv;
    int stat_addr; int stat_lenv;
    int acc_addr; int acc_lenv;
    int form_addr; int form_lenv;
    int blank_addr; int blank_lenv;
    int recl_val;
    int err_label;
    int iostat_addr;
    char kw[F77_MAX_NAME];
    int cb;
    int r;

    f77_scan_from(skip);
    have_unit = 0;
    unit_val = -1;
    file_addr = -1; file_lenv = -1;
    stat_addr = -1; stat_lenv = -1;
    acc_addr = -1; acc_lenv = -1;
    form_addr = -1; form_lenv = -1;
    blank_addr = -1; blank_lenv = -1;
    recl_val = -1;
    err_label = -1;
    iostat_addr = -1;
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
                    file_addr = f77_expr();
                    if (ex_ty != TY_CHAR) { f77_error("FILE= needs a CHARACTER value"); return; }
                    file_lenv = ex_len;
                } else if (strcmp(kw, "STATUS") == 0) {
                    stat_addr = f77_expr();
                    if (ex_ty != TY_CHAR) { f77_error("STATUS= needs a CHARACTER value"); return; }
                    stat_lenv = ex_len;
                } else if (strcmp(kw, "ACCESS") == 0) {
                    int a;
                    a = f77_expr();
                    if (ex_ty != TY_CHAR) { f77_error("ACCESS= needs a CHARACTER value"); return; }
                    acc_addr = a; acc_lenv = ex_len;
                } else if (strcmp(kw, "FORM") == 0) {
                    int a;
                    a = f77_expr();
                    if (ex_ty != TY_CHAR) { f77_error("FORM= needs a CHARACTER value"); return; }
                    form_addr = a; form_lenv = ex_len;
                } else if (strcmp(kw, "BLANK") == 0) {
                    int a;
                    a = f77_expr();
                    if (ex_ty != TY_CHAR) { f77_error("BLANK= needs a CHARACTER value"); return; }
                    blank_addr = a; blank_lenv = ex_len;
                } else if (strcmp(kw, "RECL") == 0) {
                    recl_val = f77_expr();
                    recl_val = f77_cvt(recl_val, &ex_hi, ex_ty, TY_INT);
                } else if (strcmp(kw, "ERR") == 0) {
                    if (lx_t != T_ICON) { f77_error("ERR= needs a statement label"); return; }
                    err_label = lex_ival;
                    f77_tok();
                } else if (strcmp(kw, "IOSTAT") == 0) {
                    int ss;
                    if (lx_t != T_NAME) { f77_error("IOSTAT= needs a variable"); return; }
                    ss = f77_sym(lex_name);
                    f77_sstored[ss] = 1;
                    f77_tok();
                    if (f77_shi[ss] >= 0) hi_emit(HI_ADDI, HL_ADDR_TY, f77_shi[ss], -1, 0, NULL);
                    iostat_addr = f77_sval[ss];
                } else {
                    f77_error("unknown keyword in OPEN");
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

    /* f77_open(unit, file, flen, status, slen, access, alen, form, fmlen,
     *          blank, blen, recl, recoverable) -> 0 ok, 2 error */
    cb = h_ncarg;
    f77_carg(unit_val, 0);
    if (file_addr < 0) { file_addr = f77_iconst(0); file_lenv = f77_iconst(0); }
    f77_carg(file_addr, 0);
    f77_carg(file_lenv, 0);
    if (stat_addr < 0) { stat_addr = f77_iconst(0); stat_lenv = f77_iconst(0); }
    f77_carg(stat_addr, 0);
    f77_carg(stat_lenv, 0);
    if (acc_addr < 0) { acc_addr = f77_iconst(0); acc_lenv = f77_iconst(0); }
    f77_carg(acc_addr, 0);
    f77_carg(acc_lenv, 0);
    if (form_addr < 0) { form_addr = f77_iconst(0); form_lenv = f77_iconst(0); }
    f77_carg(form_addr, 0);
    f77_carg(form_lenv, 0);
    if (blank_addr < 0) { blank_addr = f77_iconst(0); blank_lenv = f77_iconst(0); }
    f77_carg(blank_addr, 0);
    f77_carg(blank_lenv, 0);
    if (recl_val < 0) recl_val = f77_iconst(0);
    f77_carg(recl_val, 0);
    f77_carg(f77_iconst((err_label >= 0 || iostat_addr >= 0) ? 1 : 0), 0);
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 13, "f77_open");
    h_cbase[r] = cb;
    if (iostat_addr >= 0) hi_emit(HI_STORE, TY_INT, iostat_addr, r, 0, NULL);
    if (err_label >= 0) {
        int b_cont; int c;
        b_cont = hir_new_block();
        c = hi_emit(HI_SNE, TY_INT, r, f77_iconst(0), 0, NULL);
        hi_emit(HI_BRC, TY_VOID, c, f77_label_blk(err_label), b_cont, NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_cont);
    }
}

/* INQUIRE (UNIT=u | FILE=f, specifier=var, ...).  The runtime looks the
 * unit or file up once (f77_inquire) and then fills each specifier
 * variable by code (f77_inq); the codes are the runtime's. */
static void f77_stmt_inquire(int skip) {
    int unit_val;
    int file_addr; int file_lenv;
    int err_label;
    int iostat_addr;
    int nspec;
    int spec_code[24]; int spec_addr[24]; int spec_len[24]; int spec_log[24];
    char kw[F77_MAX_NAME];
    int r;
    int i;
    int args[4];

    f77_scan_from(skip);
    unit_val = -1;
    file_addr = -1; file_lenv = -1;
    err_label = -1;
    iostat_addr = -1;
    nspec = 0;
    if (lx_t != T_LP) { f77_error("INQUIRE needs a control list"); return; }
    f77_tok();
    for (;;) {
        int code;
        if (lx_t != T_NAME) {
            /* the first item may be the unit without UNIT= (12.10.3) */
            if (nspec == 0 && unit_val < 0 && file_addr < 0 && err_label < 0 && iostat_addr < 0) {
                unit_val = f77_expr();
                unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
                if (lx_t == T_COMMA) { f77_tok(); continue; }
                break;
            }
            f77_error("INQUIRE needs keyword=value items");
            return;
        }
        strcpy(kw, lex_name);
        f77_tok();
        if (lx_t != T_ASSIGN) {
            if (nspec == 0 && unit_val < 0 && file_addr < 0 && err_label < 0 && iostat_addr < 0) {
                /* a positional unit that is a name, as OPEN takes it */
                unit_val = f77_io_unit_after_name(kw);
                unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
                if (lx_t == T_COMMA) { f77_tok(); continue; }
                break;
            }
            f77_error("INQUIRE needs keyword=value items");
            return;
        }
        f77_tok();
        code = 0;
        if (strcmp(kw, "UNIT") == 0) {
            unit_val = f77_expr();
            unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
        } else if (strcmp(kw, "FILE") == 0) {
            file_addr = f77_expr();
            if (ex_ty != TY_CHAR) { f77_error("FILE= needs a CHARACTER value"); return; }
            file_lenv = ex_len;
        } else if (strcmp(kw, "ERR") == 0) {
            if (lx_t != T_ICON) { f77_error("ERR= needs a statement label"); return; }
            err_label = lex_ival;
            f77_tok();
        } else if (strcmp(kw, "IOSTAT") == 0) {
            int ty;
            iostat_addr = f77_rd_target(&ty);
        }
        else if (strcmp(kw, "EXIST") == 0) code = 1;
        else if (strcmp(kw, "OPENED") == 0) code = 2;
        else if (strcmp(kw, "NUMBER") == 0) code = 3;
        else if (strcmp(kw, "NAMED") == 0) code = 4;
        else if (strcmp(kw, "NAME") == 0) code = 5;
        else if (strcmp(kw, "ACCESS") == 0) code = 6;
        else if (strcmp(kw, "SEQUENTIAL") == 0) code = 7;
        else if (strcmp(kw, "DIRECT") == 0) code = 8;
        else if (strcmp(kw, "FORM") == 0) code = 9;
        else if (strcmp(kw, "FORMATTED") == 0) code = 10;
        else if (strcmp(kw, "UNFORMATTED") == 0) code = 11;
        else if (strcmp(kw, "RECL") == 0) code = 12;
        else if (strcmp(kw, "NEXTREC") == 0) code = 13;
        else if (strcmp(kw, "BLANK") == 0) code = 14;
        else { f77_error("unknown INQUIRE specifier"); return; }
        if (code > 0) {
            int ty;
            if (nspec >= 24) { f77_error("too many INQUIRE specifiers"); return; }
            spec_addr[nspec] = f77_rd_target(&ty);
            spec_len[nspec] = (ty == TY_CHAR) ? f77_rd_len : f77_iconst(0);
            spec_code[nspec] = code;
            spec_log[nspec] = (ty != TY_CHAR);
            nspec = nspec + 1;
        }
        if (lx_t == T_COMMA) { f77_tok(); continue; }
        break;
    }
    if (lx_t != T_RP) { f77_error("expected ) after INQUIRE list"); return; }
    f77_tok();
    if (unit_val < 0 && file_addr < 0) { f77_error("INQUIRE needs UNIT= or FILE="); return; }
    args[0] = (unit_val >= 0) ? unit_val : f77_iconst(-1);
    args[1] = (file_addr >= 0) ? file_addr : f77_iconst(0);
    args[2] = (file_addr >= 0) ? file_lenv : f77_iconst(0);
    r = f77_call_i("f77_inquire", 3, args);
    if (iostat_addr >= 0) hi_emit(HI_STORE, TY_INT, iostat_addr, r, 0, NULL);
    for (i = 0; i < nspec; i++) {
        args[0] = f77_iconst(spec_code[i]);
        args[1] = spec_addr[i];
        args[2] = spec_len[i];
        f77_call_i("f77_inq", 3, args);
    }
    if (err_label >= 0) {
        int b_cont; int c;
        b_cont = hir_new_block();
        c = hi_emit(HI_SNE, TY_INT, r, f77_iconst(0), 0, NULL);
        hi_emit(HI_BRC, TY_VOID, c, f77_label_blk(err_label), b_cont, NULL);
        f77_cur_blk_live = 0;
        f77_begin_blk(b_cont);
    }
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
    f77_carg(unit_val, 0);
    if (stat_sidx >= 0) a = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, stat_sidx, NULL);
    else a = f77_iconst(0);
    f77_carg(a, 0);
    f77_carg(f77_iconst(stat_slen), 0);
    r = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "f77_close");
    h_cbase[r] = cb;
}

/* REWIND u  --  or REWIND (u). */
static int f77_rd_target(int *tyo);
static void f77_stmt_rewind(int skip) {
    int unit_val;
    f77_scan_from(skip);
    if (lx_t == T_LP) {
        /* (u) or (UNIT=u [, ERR=l] [, IOSTAT=v]) */
        unit_val = -1;
        f77_tok();
        for (;;) {
            if (lx_t == T_NAME && (int)(lx_rte - lx_stmt) < lx_stmt_len &&
                f77_eq_is_assign((int)(lx_rte - lx_stmt))) {
                char kw[F77_MAX_NAME];
                strcpy(kw, lex_name);
                f77_tok();
                f77_tok();
                if (strcmp(kw, "UNIT") == 0) {
                    unit_val = f77_expr(); unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
                } else if (strcmp(kw, "ERR") == 0) {
                    f77_tok();                      /* a REWIND does not fail here */
                } else if (strcmp(kw, "IOSTAT") == 0) {
                    int ty; int a;
                    a = f77_rd_target(&ty);
                    hi_emit(HI_STORE, TY_INT, a, f77_iconst(0), 0, NULL);
                } else { f77_error("unknown keyword in REWIND"); return; }
            } else {
                unit_val = f77_expr(); unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
            }
            if (lx_t == T_COMMA) { f77_tok(); continue; }
            break;
        }
        if (lx_t != T_RP) { f77_error("expected ) after REWIND unit"); return; }
        f77_tok();
        if (unit_val < 0) { f77_error("REWIND needs a unit"); return; }
    } else {
        unit_val = f77_expr();
        unit_val = f77_cvt(unit_val, &ex_hi, ex_ty, TY_INT);
    }
    f77_call_i("f77_rewind", 1, &unit_val);
}

/* Is this statement part of the declaration part (so the scalar dummy
 * copies must not be emitted yet)? */
static int f77_stmt_is_declaration(int cls) {
    if (cls != S_KEYWORD) return 0;
    if (f77_starts("INTEGER")) return 1;
    if (f77_starts("REAL")) return 1;
    if (f77_starts("DOUBLEPRECISION")) return 1;
    if (f77_starts("DOUBLECOMPLEX")) return 1;
    if (f77_starts("LOGICAL")) return 1;
    if (f77_starts("CHARACTER")) return 1;
    if (f77_starts("COMPLEX")) return 1;
    if (f77_starts("DIMENSION")) return 1;
    if (f77_starts("COMMON")) return 1;
    if (f77_starts("EQUIVALENCE")) return 1;
    if (f77_starts("BLOCKDATA")) return 1;
    if (f77_starts("EXTERNAL")) return 1;
    if (f77_starts("INTRINSIC")) return 1;
    if (f77_starts("NAMELIST")) return 1;
    if (f77_starts("SAVE")) return 1;
    if (f77_starts("DATA")) return 1;
    if (f77_starts("PARAMETER")) return 1;
    if (f77_starts("IMPLICIT")) return 1;
    if (f77_starts("FORMAT")) return 1;
    if (f77_starts("PROGRAM")) return 1;
    if (f77_starts("SUBROUTINE")) return 1;
    if (f77_unit_header_ty() >= 0) return 1;
    /* Fortran 90's specification statements */
    if (f77_starts("USE")) return 1;
    if (f77_starts("INTERFACE")) return 1;
    if (f77_starts("ENDINTERFACE")) return 1;
    if (f77_starts("MODULEPROCEDURE")) return 1;
    if (f77_starts("MODULE")) return 1;
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
    int nerr0;
    int ctl_n0;
    char cname[64];

    /* A statement no handler scans leaves no tokens; one that
     * is scanned must be used up (see done:) */
    lx_t = T_EOF;
    nerr0 = f77_nerr;
    cls = f77_classify();

    /* A construct name (the card reader took it off the text) is on a
     * DO or a block IF; the construct pushed is given it at done: */
    strcpy(cname, lx_stmt_cname);
    lx_stmt_cname[0] = 0;
    ctl_n0 = ctl_n;
    if (cname[0]) {
        if (cls == S_IF_LOGIC) { f77_error("a construct name is on a block IF, not a logical IF"); goto done; }
        if (!(cls == S_DO || cls == S_IF_BLOCK || f77_is_dowhile() || f77_is_doforever())) {
            f77_error("a construct name is on DO or IF (...) THEN only");
            goto done;
        }
        f77_cname_define(cname);
    }

    /* Fortran 90: the statements of an INTERFACE block, and USE */
    if (f77_iface_stmt()) goto done;
    if (cls == S_KEYWORD && (n = f77_starts("USE")) != 0) {
        f77_stmt_use(n);
        goto done;
    }
    if (cls == S_KEYWORD && f77_starts("INTERFACE")) {
        f77_stmt_interface();
        goto done;
    }
    if (cls == S_KEYWORD && f77_starts("MODULE") && f77_ukind[f77_cur_unit] == F77_UNIT_MODULE &&
        !f77_starts("MODULEPROCEDURE")) goto done;               /* the module's own header */

    if (cls == S_ASSIGN && f77_stf_define()) goto done;

    /* Declarations must all precede executable statements, so the first
     * executable one is where every dummy's type is finally known --
     * and therefore where the static layout and the scalar copies are
     * emitted.  That happens BEFORE the label's block is opened: the
     * layout emits hidden-argument PARAMs and must stay in the entry
     * block, and it runs once, which a labelled first statement (a
     * loop target) would not guarantee. */
    if (f77_decls_open && !f77_stmt_is_declaration(cls)) f77_emit_copyin();
    if (f77_entry_state == 1 && !f77_decls_open) {
        /* The text before the ENTRY: reached only by a GO TO */
        f77_goto_blk(f77_entry_blk);
        f77_entry_state = 2;
    }

    /* A labelled statement starts (or continues into) that label's
     * block, so GOTOs and DO terminals both land correctly. */
    if (lx_stmt_label >= 0) {
        int b;
        b = f77_label_blk(lx_stmt_label);
        { int q; for (q = f77_label_base; q < f77_nlabel; q++)
            if (f77_lnum[q] == lx_stmt_label) f77_ldef[q] = 1; }
        f77_goto_blk(b);
        f77_begin_blk(b);
    }

    /* An unlabelled statement after RETURN, GOTO or STOP is unreachable,
     * but it is still compiled -- and it must not land in the block the
     * terminator just closed.  The backend's contract is that a
     * terminator is the LAST instruction of its block (the codegen drops
     * the jump of a RET or BR to the block laid out next, so anything
     * after it would run): the body of the next ENTRY, parsed here as
     * dead code, called through a phantom dummy's address in FCVS FM509
     * once the re-synced backend took that liberty.  So: a fresh block,
     * which has no predecessor and is removed as dead. */
    if (!f77_cur_blk_live && !f77_stmt_is_declaration(cls))
        f77_begin_blk(hir_new_block());

    if (ctl_n > f77_ctl_base && ctl_kind[ctl_n - 1] == F77_CTL_WHERE && cls != S_ASSIGN &&
        !f77_starts("ELSEWHERE") && !f77_starts("ENDWHERE") && !f77_starts("WHERE(")) {
        f77_error("a WHERE construct holds array assignments only");
        goto done;
    }
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
        {
            /* one at a time: f77_label_blk makes a label's block on first
             * sight, and C leaves the order of a call's arguments open,
             * so as arguments the two blocks came out numbered one way
             * by gcc on x86_64 and the other by gcc on arm64 and clang */
            int b2;
            int b3;
            b2 = f77_label_blk(l2);
            b3 = f77_label_blk(l3);
            hi_emit(HI_BRC, TY_VOID, c_eq, b2, b3, NULL);
        }
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
        if (lx_t == T_NAME && strcmp(lex_name, "THEN") == 0) f77_tok();
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

    if (f77_starts("FORMAT")) {                /* collected in phase 1 */
        /* fort370: checked here, once, where its line is known */
        if (f77_inline_depth == 0 && lx_stmt_len > 6 && lx_stmt[6] == '(')
            f77_check_format(lx_stmt + 6, lx_stmt_len - 6);
        goto done;
    }

    if ((n = f77_starts("WRITE")) != 0) { f77_stmt_write(n); goto done; }
    if ((n = f77_starts("PRINT")) != 0) { f77_stmt_print(n); goto done; }
    if ((n = f77_starts("READ")) != 0)  { f77_stmt_read(n);  goto done; }
    if ((n = f77_starts("OPEN")) != 0)  { f77_stmt_open(n);  goto done; }
    if ((n = f77_starts("CLOSE")) != 0) { f77_stmt_close(n); goto done; }
    if (f77_starts("EXIT")) { f77_stmt_exit_cycle(0); goto done; }
    if ((n = f77_starts("ALLOCATE")) != 0) { f77_stmt_allocate(n, 1); goto done; }
    if ((n = f77_starts("DEALLOCATE")) != 0) { f77_stmt_allocate(n, 0); goto done; }
    if (f77_starts("CYCLE")) { f77_stmt_exit_cycle(1); goto done; }
    if ((n = f77_starts("INQUIRE")) != 0) { f77_stmt_inquire(n); goto done; }
    if ((n = f77_starts("REWIND")) != 0) { f77_stmt_rewind(n); goto done; }
    if ((n = f77_starts("BACKSPACE")) != 0) {
        int u;
        f77_scan_from(n);
        if (lx_t == T_LP) {
            f77_tok();
            if (lx_t == T_NAME && strcmp(lex_name, "UNIT") == 0) { f77_tok(); if (lx_t == T_ASSIGN) f77_tok(); }
            u = f77_expr(); u = f77_cvt(u, &ex_hi, ex_ty, TY_INT);
            if (lx_t != T_RP) f77_error("expected ) in BACKSPACE");
            else f77_tok();
        } else {
            u = f77_expr(); u = f77_cvt(u, &ex_hi, ex_ty, TY_INT);
        }
        f77_call_i("f77_backspace", 1, &u);
        goto done;
    }
    if ((n = f77_starts("EQUIVALENCE")) != 0) { f77_stmt_equivalence(n); goto done; }
    if (f77_starts("CHARACTER")) { f77_stmt_decl(TY_CHAR, 9); goto done; }
    if (f77_starts("COMPLEX")) { f77_stmt_decl(TY_CPLX, 7); goto done; }
    if (f77_starts("BLOCKDATA")) goto done;    /* this unit's own header */
    if ((n = f77_starts("EXTERNAL")) != 0) {
        /* Names of external procedures.  Recorded so that passing one
         * as an actual argument is refused instead of passing the
         * address of a variable of that name. */
        if (strncmp(lx_stmt + n, "::", 2) == 0) n = n + 2;    /* Fortran 90 */
        f77_scan_from(n);
        while (lx_t == T_NAME) {
            f77_sexternal[f77_sym(lex_name)] = 1;
            f77_tok();
            if (lx_t != T_COMMA) break;
            f77_tok();
        }
        goto done;
    }
    if ((n = f77_starts("INTRINSIC")) != 0) {
        if (strncmp(lx_stmt + n, "::", 2) == 0) n = n + 2;    /* Fortran 90 */
        f77_scan_from(n);
        while (lx_t == T_NAME) {
            f77_sintrinsic[f77_sym(lex_name)] = 1;
            f77_tok();
            if (lx_t != T_COMMA) break;
            f77_tok();
        }
        goto done;
    }
    if ((n = f77_starts("NAMELIST")) != 0) { f77_stmt_namelist(n); goto done; }
    if ((n = f77_starts("ASSIGN")) != 0) { f77_stmt_assign_label(n); goto done; }
    if ((n = f77_starts("PAUSE")) != 0) {
        int args[2];
        f77_scan_from(n);
        if (lx_t == T_SCON) {
            args[0] = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, lex_sidx, NULL);
            args[1] = f77_iconst(lex_slen);
        } else if (lx_t == T_ICON) {
            int si;
            si = f77_intern_str(lx_stmt + n, lx_stmt_len - n);
            args[0] = hi_emit(HI_SADDR, HL_ADDR_TY, -1, -1, si, NULL);
            args[1] = f77_iconst(lx_stmt_len - n);
        } else {
            args[0] = f77_iconst(0);
            args[1] = f77_iconst(0);
        }
        if (lx_t == T_SCON || lx_t == T_ICON) f77_tok();
        f77_call_i("f77_pause", 2, args);
        goto done;
    }
    if (f77_starts("ENTRY")) {
        /* A no-op in the flow, except where the unit being compiled is
         * this ENTRY: its code begins here (falling into an ENTRY
         * statement goes on past it, as for any other). */
        if (f77_entry_state == 2 && lx_stmt_line == f77_entry_line) {
            f77_goto_blk(f77_entry_blk);
            f77_begin_blk(f77_entry_blk);
            f77_entry_state = 3;
        }
        goto done;
    }


    if (f77_starts("SUBROUTINE") || f77_unit_header_ty() >= 0) {
        /* The header was consumed by f77_bind_unit() before the body
         * was parsed; seeing it here means it is this unit's own
         * header, which needs no code. */
        goto done;
    }

    if ((n = f77_starts("RETURN")) != 0) {
        /* RETURN [e]: e selects an alternate return in the caller. */
        f77_scan_from(n);
        f77_ret_code = -1;
        if (lx_t != T_EOF) {
            f77_ret_code = f77_expr();
            f77_ret_code = f77_cvt(f77_ret_code, &ex_hi, ex_ty, TY_INT);
        }
        f77_emit_return();
        f77_ret_code = -1;
        goto done;
    }

    if ((n = f77_starts("CALL")) != 0) {
        f77_stmt_call(n);
        goto done;
    }

    if (f77_starts("CONTINUE")) goto done;
    if (f77_starts("INTEGER"))  { f77_stmt_decl(TY_INT,   7); goto done; }
    if (f77_starts("LOGICAL"))  { f77_decl_log = 1; f77_stmt_decl(TY_INT, 7); f77_decl_log = 0; goto done; }
    if (f77_starts("DOUBLEPRECISION")) { f77_stmt_decl(TY_DOUBLE, 15); goto done; }
    if (f77_starts("DOUBLECOMPLEX")) { f77_stmt_decl(TY_DCPLX, 13); goto done; }
    if (f77_starts("DIMENSION")) { f77_stmt_decl(-1, 9); goto done; }
    if (f77_starts("REAL"))     { f77_stmt_decl(TY_FLOAT, 4); goto done; }
    if ((n = f77_starts("COMMON")) != 0) { f77_stmt_common(n); goto done; }
    if ((n = f77_starts("SAVE")) != 0)   {
        if (strncmp(lx_stmt + n, "::", 2) == 0) n = n + 2;    /* Fortran 90 */
        f77_stmt_save(n);
        goto done;
    }
    if ((n = f77_starts("PARAMETER")) != 0) { f77_stmt_parameter(n); goto done; }
    if ((n = f77_starts("DATA")) != 0)   { f77_stmt_data(n);   goto done; }
    if ((n = f77_starts("IMPLICIT")) != 0) { f77_stmt_implicit(n); goto done; }

    if (f77_starts("WHERE(") && cls == S_KEYWORD) { f77_stmt_where(); goto done; }
    if (f77_starts("ELSEWHERE") && lx_stmt_len == 9 && ctl_n > f77_ctl_base && ctl_kind[ctl_n - 1] == F77_CTL_WHERE) {
        if (ctl_else[ctl_n - 1]) { f77_error("a second ELSEWHERE in one WHERE construct"); goto done; }
        ctl_else[ctl_n - 1] = 1;
        f77_where_neg = 1;
        goto done;
    }
    if (f77_starts("ELSEWHERE(") && ctl_n > f77_ctl_base && ctl_kind[ctl_n - 1] == F77_CTL_WHERE) {
        f77_error("ELSEWHERE (mask) is Fortran 95's, not supported");
        goto done;
    }
    if (f77_starts("ENDWHERE") && lx_stmt_len == 8) {
        if (!(ctl_n > f77_ctl_base && ctl_kind[ctl_n - 1] == F77_CTL_WHERE)) { f77_error("END WHERE without WHERE"); goto done; }
        ctl_n = ctl_n - 1;
        f77_where_end();
        goto done;
    }
    if (f77_starts("ELSEIF(")) {
        /* ELSE IF (e) THEN [name] -- close the current arm, open a new
         * test in the pending else block.  (ELSE IFX, the construct
         * named IFX, is the ELSE below.) */
        if (ctl_n == 0 || ctl_kind[ctl_n - 1] != F77_CTL_IF) {
            f77_error("ELSE IF without IF");
            goto done;
        }
        if (ctl_else[ctl_n - 1] < 0) { f77_error("ELSE IF after ELSE"); goto done; }
        {
            /* THEN and the name are one token once the blanks are
             * gone (THENOUTER): read from the text */
            int after;
            char *nm;
            after = f77_match_paren(6);
            nm = 0;
            if (after + 4 <= lx_stmt_len && strncmp(lx_stmt + after, "THEN", 4) == 0)
                nm = f77_name_tail(after + 4);
            if (!nm) { f77_error("ELSE IF (e) needs THEN, and nothing after it but a construct name"); goto done; }
            f77_cname_match(ctl_n - 1, nm, "ELSE IF");
        }
        f77_goto_blk(ctl_exit[ctl_n - 1]);
        f77_begin_blk(ctl_else[ctl_n - 1]);
        f77_scan_from(7);
        c = f77_expr();
        if (lx_t != T_RP) { f77_error("expected ) in ELSE IF"); goto done; }
        lx_t = T_EOF;                           /* THEN [name]: checked above */
        b_then = hir_new_block();
        b_else = hir_new_block();
        hi_emit(HI_BRC, TY_VOID, c, b_then, b_else, NULL);
        f77_cur_blk_live = 0;
        ctl_else[ctl_n - 1] = b_else;
        f77_begin_blk(b_then);
        goto done;
    }

    if (f77_starts("ELSE")) {
        char *nm;
        if (ctl_n == 0 || ctl_kind[ctl_n - 1] != F77_CTL_IF) {
            f77_error("ELSE without IF");
            goto done;
        }
        if ((nm = f77_name_tail(4)) == 0) { f77_error("unexpected text after ELSE"); goto done; }
        if (ctl_else[ctl_n - 1] < 0) { f77_error("a second ELSE in one IF construct"); goto done; }
        f77_cname_match(ctl_n - 1, nm, "ELSE");
        f77_goto_blk(ctl_exit[ctl_n - 1]);
        f77_begin_blk(ctl_else[ctl_n - 1]);
        ctl_else[ctl_n - 1] = -1;
        goto done;
    }

    if (f77_starts("ENDDO") && f77_name_tail(5)) {
        /* fort370: END DO [name] closes the innermost unlabelled DO;
         * with the label a labelled DO names, it is that DO's terminal
         * statement (and closed with the label, below, where a named
         * DO's name is checked) */
        char *nm;
        nm = f77_name_tail(5);
        if (ctl_n > f77_ctl_base && ctl_kind[ctl_n - 1] == F77_CTL_DO && ctl_label[ctl_n - 1] == -1) {
            f77_cname_match(ctl_n - 1, nm, "END DO");
            f77_close_top_do();
        } else if (ctl_n > f77_ctl_base && ctl_kind[ctl_n - 1] == F77_CTL_DO &&
                   lx_stmt_label >= 0 && ctl_label[ctl_n - 1] == lx_stmt_label) {
            if (!ctl_name[ctl_n - 1][0] && nm[0]) f77_cname_match(ctl_n - 1, nm, "END DO");
        } else
            f77_error("END DO without DO");
        goto done;
    }
    if (f77_is_dowhile()) { f77_open_dowhile(0); goto done; }
    if (f77_is_doforever()) { f77_open_dowhile(1); goto done; }

    if (f77_starts("ENDIF")) {
        char *nm;
        if (ctl_n == 0 || ctl_kind[ctl_n - 1] != F77_CTL_IF) {
            f77_error("ENDIF without IF");
            goto done;
        }
        if ((nm = f77_name_tail(5)) == 0) { f77_error("unexpected text after END IF"); goto done; }
        f77_cname_match(ctl_n - 1, nm, "END IF");
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
        if (lx_t == T_NAME) {
            /* Assigned GOTO: GO TO v [, (l1, l2, ...)].  A compare
             * chain over the listed labels, or over every label this
             * unit ASSIGNs when there is no list; the last candidate
             * is taken unconditionally (the standard leaves a value
             * outside the list undefined). */
            int labs[64];
            int nl;
            int i;
            int v;
            int s;
            s = f77_sym(lex_name);
            v = f77_load_sym(s);
            v = f77_cvt(v, &ex_hi, ex_ty, TY_INT);
            f77_tok();
            if (lx_t == T_COMMA) f77_tok();
            nl = 0;
            if (lx_t == T_LP) {
                f77_tok();
                for (;;) {
                    if (lx_t != T_ICON) { f77_error("assigned GOTO needs statement labels"); goto done; }
                    if (nl < 64) { labs[nl] = lex_ival; nl = nl + 1; }
                    f77_tok();
                    if (lx_t != T_COMMA) break;
                    f77_tok();
                }
                if (lx_t != T_RP) { f77_error("expected ) in assigned GOTO"); goto done; }
                f77_tok();
            } else {
                for (i = 0; i < f77_nasg; i++) {
                    if (f77_asg_unit[i] != f77_text_unit()) continue;
                    if (f77_find_format(f77_asg_lab[i]) >= 0) continue;
                    if (nl < 64) { labs[nl] = f77_asg_lab[i]; nl = nl + 1; }
                }
            }
            if (nl == 0) { f77_error("assigned GOTO has no candidate labels"); goto done; }
            for (i = 0; i + 1 < nl; i++) {
                int b_next;
                c = hi_emit(HI_SEQ, TY_INT, v, f77_iconst(labs[i]), 0, NULL);
                b_next = hir_new_block();
                hi_emit(HI_BRC, TY_VOID, c, f77_label_blk(labs[i]), b_next, NULL);
                f77_cur_blk_live = 0;
                f77_begin_blk(b_next);
            }
            f77_goto_blk(f77_label_blk(labs[nl - 1]));
            goto done;
        }
        if (lx_t != T_ICON) { f77_error("GOTO needs a label"); goto done; }
        f77_goto_blk(f77_label_blk(lex_ival));
        f77_tok();
        goto done;
    }

    if ((n = f77_starts("STOP")) != 0) {
        int v;
        int cb;
        int r;
        f77_scan_from(n);
        v = (lx_t == T_ICON) ? f77_iconst(lex_ival) : f77_iconst(0);
        if (lx_t == T_ICON || lx_t == T_SCON) f77_tok();   /* STOP n, STOP 'text' */
        if (f77_cur_blk_live) {
            /* Process exit, not a function return -- STOP in a
             * SUBROUTINE must terminate the program (GitHub #20). */
            cb = h_ncarg;
            f77_carg(v, 0);
            r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "exit");
            h_cbase[r] = cb;
            hi_emit(HI_RET, TY_INT, v, -1, 0, NULL);
        }
        f77_cur_blk_live = 0;
        goto done;
    }

    if ((n = f77_starts("ENDFILE")) != 0) {   /* must precede the END match */
        int u;
        f77_scan_from(n);
        if (lx_t == T_LP) {
            f77_tok();
            if (lx_t == T_NAME && strcmp(lex_name, "UNIT") == 0) {
                f77_tok();
                if (lx_t == T_ASSIGN) f77_tok();
            }
            u = f77_expr();
            u = f77_cvt(u, &ex_hi, ex_ty, TY_INT);
            if (lx_t != T_RP) f77_error("expected ) in ENDFILE");
            else f77_tok();
        } else {
            u = f77_expr();
            u = f77_cvt(u, &ex_hi, ex_ty, TY_INT);
        }
        f77_call_i("f77_endfile", 1, &u);
        goto done;
    }

    if (f77_is_unit_end() || f77_is_contains()) {
        /* Falling off END (or CONTAINS, which ends the body) returns like RETURN does -- a FUNCTION hands
         * back its result variable.  This emitted a bare `return 0`
         * for years: every direct caller was spliced inline and read
         * the variable itself, so only a FUNCTION reached through a
         * dummy procedure ever executed the out-of-line body. */
        f77_emit_return();
        f77_cur_blk_live = 0;
        goto done;
    }

    f77_error("unrecognised statement");

done:
    /* Text left over after a statement's handler is text it
     * did not understand.  Ignored, it was ignored without a word:
     * INTEGER N/9000/, LEN/30000/ lost both values and every name after
     * them.  A statement that already reported an error is left be. */
    if (lx_t != T_EOF && f77_nerr == nerr0) f77_error("unexpected text after the statement");
    if (ctl_n == ctl_n0 + 1) strcpy(ctl_name[ctl_n - 1], cname);
    /* If this statement carried a label that closes one or more DO
     * loops, close them now -- the terminal statement is part of the
     * loop body and has just been emitted. */
    if (lx_stmt_label >= 0) {
        lab = lx_stmt_label;
        f77_close_do(lab);
    }
}

#endif
